# source.R -------------------------------------------------------------------
# Data helpers and one wrapper per nowcasting method.
#
# Conventions shared by every method (so results are comparable):
#   * Weekly = MMWR epiweeks (Sunday start). Dates are the Sunday of the week.
#   * Cutoff: only reports with DT_DIGITA <= cutoff are used. For weekly, the
#     last incomplete report week is also dropped, exactly like
#     nowcaster::nowcasting_inla(use.epiweek = TRUE): reports are kept up to
#     the last Saturday before the week of max(DT_DIGITA).
#   * "now" = last reference step that can be nowcast (weekly: Sunday that
#     starts the last complete week; daily: max(DT_DIGITA) after the cutoff).
#   * Delays > max_delay (in steps) are ignored; negative delays are dropped.
#   * Output horizon = the last `max_delay` steps up to "now" (what nowcaster
#     returns). Every wrapper returns a tibble with
#     dt_event, median, lower (2.5%), upper (97.5%), method.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

# ---- Data --------------------------------------------------------------------

read_linelist <- function(path, uf = 35, years = 2025:2026) {
  arrow::read_parquet(path, col_select = c("SG_UF", "DT_SIN_PRI", "DT_DIGITA")) |>
    mutate(SG_UF = as.numeric(SG_UF)) |>
    filter(SG_UF %in% uf) |>
    transmute(DT_SIN_PRI = as.Date(DT_SIN_PRI),
              DT_DIGITA  = as.Date(DT_DIGITA)) |>
    filter(epiyear(DT_SIN_PRI) %in% years)
}

# Sunday that starts the MMWR week (same as MMWRweek2Date(epiyear, epiweek))
week_start <- function(x) x - as.integer(format(x, "%w"))

# Data as available on `cutoff`
as_of <- function(data, cutoff, scale = c("weekly", "daily")) {
  scale <- match.arg(scale)
  raw <- data |>
    filter(!is.na(DT_SIN_PRI), !is.na(DT_DIGITA), DT_DIGITA <= cutoff)
  dmax <- max(raw$DT_DIGITA)

  if (scale == "weekly") {
    last_sat <- dmax - (as.integer(format(dmax, "%w")) + 1)  # nowcaster rule
    now      <- last_sat - 6
    used     <- filter(raw, DT_DIGITA <= last_sat)
  } else {
    last_sat <- NA
    now      <- dmax
    used     <- raw
  }

  list(raw = raw,          # cut at the user cutoff only (nowcaster applies its own rule)
       data = used,        # what the other methods use
       now = now,
       last_report = if (scale == "weekly") last_sat else dmax,
       scale = scale,
       step = if (scale == "weekly") 7 else 1)
}

# Reference/report dates binned to the time step, with delay in steps
binned <- function(ao) {
  d <- if (ao$scale == "weekly") {
    transmute(ao$data, ref = week_start(DT_SIN_PRI), rep = week_start(DT_DIGITA))
  } else {
    transmute(ao$data, ref = DT_SIN_PRI, rep = DT_DIGITA)
  }
  d |>
    mutate(delay = as.integer(rep - ref) %/% ao$step) |>
    filter(delay >= 0)
}

# Counts by reference step as observed on the cutoff
observed_counts <- function(ao) {
  binned(ao) |> count(dt_event = ref, name = "Cases")
}

# "Truth" from a later file: counts with delay <= max_delay steps
truth_counts <- function(data_later, scale = c("weekly", "daily"), max_delay) {
  scale <- match.arg(scale)
  step  <- if (scale == "weekly") 7 else 1
  d <- data_later |> filter(!is.na(DT_SIN_PRI), !is.na(DT_DIGITA))
  d <- if (scale == "weekly") {
    transmute(d, ref = week_start(DT_SIN_PRI), rep = week_start(DT_DIGITA))
  } else {
    transmute(d, ref = DT_SIN_PRI, rep = DT_DIGITA)
  }
  d |>
    mutate(delay = as.integer(rep - ref) %/% step) |>
    filter(delay >= 0, delay <= max_delay) |>
    count(dt_event = ref, name = "truth")
}

horizon_start <- function(ao, max_delay) ao$now - (max_delay - 1) * ao$step

summarise_draws <- function(df, date_col, value_col, method) {
  df |>
    as_tibble() |>
    group_by(dt_event = as.Date(.data[[date_col]])) |>
    summarise(median = median(.data[[value_col]]),
              lower  = quantile(.data[[value_col]], 0.025),
              upper  = quantile(.data[[value_col]], 0.975),
              .groups = "drop") |>
    mutate(method = method)
}

# ---- nowcaster (weekly only) -------------------------------------------------

do_nowcaster <- function(ao, engine = c("inla", "mgcv"), max_delay = 15, window = 30) {
  engine <- match.arg(engine)
  stopifnot(ao$scale == "weekly")
  fun <- if (engine == "inla") nowcaster::nowcasting_inla else nowcaster::nowcasting_mgcv
  args <- list(dataset = ao$raw, date_onset = "DT_SIN_PRI", date_report = "DT_DIGITA",
               data.by.week = TRUE, trim.data = 0, Dmax = max_delay, wdw = window,
               silent = TRUE)
  args <- args[names(args) %in% names(formals(fun))]   # mgcv may not take all of them
  res <- do.call(fun, args)
  res$total |>
    as_tibble() |>
    transmute(dt_event = as.Date(dt_event), median = Median, lower = LI, upper = LS,
              method = paste0("nowcaster (", toupper(engine), ")"))
}

# ---- baselinenowcast ---------------------------------------------------------
# Uses the whole history (as in test.R); window = NULL keeps it that way.

do_baselinenowcast <- function(ao, max_delay = 15, window = NULL, draws = 1000) {
  b <- binned(ao) |> filter(delay <= max_delay)
  if (!is.null(window)) b <- filter(b, ref > ao$now - window * ao$step)

  counts <- count(b, ref, rep, name = "cases")

  rep_tri <- counts |>
    baselinenowcast::as_reporting_triangle(
      delays_unit = if (ao$scale == "weekly") "weeks" else "days",
      reference_date = "ref", report_date = "rep", count = "cases") |>
    baselinenowcast::truncate_to_delay(max_delay = max_delay)

  baselinenowcast::baselinenowcast(rep_tri, draws = draws) |>
    summarise_draws("reference_date", "pred_count", "baselinenowcast")
}

# ---- epinowcast --------------------------------------------------------------

# Cumulative counts by reference x report date, as epinowcast expects
enw_obs_from_ao <- function(ao, max_delay, window) {
  refs <- seq(ao$now - (window - 1) * ao$step, ao$now, by = ao$step)
  counts <- binned(ao) |>
    filter(delay <= max_delay, ref >= min(refs)) |>
    count(ref, rep, name = "new_confirm")

  tidyr::crossing(reference_date = refs, d = 0:max_delay) |>
    mutate(report_date = reference_date + d * ao$step) |>
    filter(report_date <= ao$now) |>
    left_join(counts, by = c("reference_date" = "ref", "report_date" = "rep")) |>
    mutate(new_confirm = coalesce(new_confirm, 0L)) |>
    arrange(reference_date, report_date) |>
    group_by(reference_date) |>
    mutate(confirm = cumsum(new_confirm)) |>
    ungroup() |>
    select(reference_date, report_date, confirm)
}

do_epinowcast <- function(ao, max_delay = 15, window = 30, model = NULL,
                          chains = 2, iter = 500) {
  obs  <- enw_obs_from_ao(ao, max_delay, window)
  weekly <- ao$scale == "weekly"

  # epinowcast counts delays including 0, so max_delay + 1 covers 0..max_delay
  pobs <- epinowcast::enw_preprocess_data(obs, max_delay = max_delay + 1,
                                          timestep = if (weekly) "week" else "day")

  expectation <- if (weekly) {
    epinowcast::enw_expectation(~ rw(week), data = pobs)
  } else {
    epinowcast::enw_expectation(~ rw(week) + (1 | day_of_week), data = pobs)
  }
  report <- if (weekly) {
    epinowcast::enw_report(data = pobs)
  } else {
    epinowcast::enw_report(~ (1 | day_of_week), data = pobs)
  }

  args <- list(
    data = pobs,
    expectation = expectation,
    report = report,
    obs = epinowcast::enw_obs(family = "negbin", data = pobs),
    fit = epinowcast::enw_fit_opts(save_warmup = FALSE, pp = FALSE,
                                   chains = chains, parallel_chains = chains,
                                   iter_warmup = iter, iter_sampling = iter,
                                   show_messages = FALSE)
  )
  if (!is.null(model)) args$model <- model
  fit <- do.call(epinowcast::epinowcast, args)

  summary(fit, type = "nowcast_samples") |>
    summarise_draws("reference_date", "sample", "epinowcast")
}

# ---- surveillance::nowcast ---------------------------------------------------
# Its weekly aggregation uses ISO (Monday) weeks, so for weekly data each
# Sunday-start week is mapped to one "pseudo day" and the model runs with
# aggregate.by = "1 day". Results are mapped back to the Sunday dates.

do_surveillance <- function(ao, max_delay = 15, window = 30,
                            method = "bayes.trunc", nSamples = 1000) {
  b <- binned(ao) |> filter(delay <= max_delay)

  origin <- min(b$ref)
  p0 <- as.Date("2000-01-02")
  to_p   <- function(x) p0 + as.integer(x - origin) %/% ao$step
  from_p <- function(p) origin + ao$step * as.integer(p - p0)

  ll    <- data.frame(dEvent = to_p(b$ref), dReport = to_p(b$rep))
  now_p <- to_p(ao$now)
  when_p <- seq(now_p - (max_delay - 1), now_p, by = "1 day")

  # Poisson-gamma prior for N(t, Inf) centred on recent complete counts
  per_step <- count(b, ref)
  recent <- per_step |>
    filter(ref <  ao$now - (max_delay - 1) * ao$step,
           ref >= ao$now - (window - 1) * ao$step) |>
    pull(n)
  if (length(recent) < 2) recent <- per_step$n
  mu <- max(mean(recent), 1)
  v  <- max(var(recent), 2 * mu, na.rm = TRUE)
  prior <- structure("poisgamma", mean.lambda = mu, var.lambda = v)

  N_max <- max(ceiling(3 * max(per_step$n[per_step$ref >= ao$now - (window - 1) * ao$step])), 100)

  # dRange set explicitly so "now" is included even with no onsets on that day
  utils::capture.output(
    nc <- surveillance::nowcast(
      now = now_p, when = when_p, data = ll,
      dEventCol = "dEvent", dReportCol = "dReport",
      method = method, aggregate.by = "1 day",
      D = max_delay, m = max(window, max_delay),
      control = list(dRange = c(min(ll$dEvent), now_p),
                     N.tInf.prior = prior, N.tInf.max = N_max,
                     nSamples = nSamples, alpha = 0.05)))

  pi <- nc@pi
  lower <- if (length(dim(pi)) == 3) pi[, 1, 1] else pi[, 1]
  upper <- if (length(dim(pi)) == 3) pi[, 1, 2] else pi[, 2]

  tibble(p = as.Date(surveillance::epoch(nc)),
         median = surveillance::upperbound(nc)[, 1],
         lower = lower, upper = upper) |>
    filter(p %in% when_p) |>
    transmute(dt_event = from_p(p), median, lower, upper,
              method = "surveillance")
}

# ---- NobBS -------------------------------------------------------------------

do_nobbs <- function(ao, max_delay = 15, window = 30, dist = "NB", nSamp = 5000) {
  b <- binned(ao) |> filter(delay <= max_delay)
  # NobBS needs a data.frame sorted by onset (it takes the first unique onset)
  df <- data.frame(onset = b$ref, report = b$rep)
  df <- df[order(df$onset), ]

  res <- NobBS::NobBS(df, now = ao$now,
                      units = if (ao$scale == "weekly") "1 week" else "1 day",
                      onset_date = "onset", report_date = "report",
                      moving_window = max(window, max_delay + 1),
                      max_D = max_delay, cutoff_D = TRUE,
                      specs = list(dist = dist, nSamp = nSamp))

  res$estimates |>
    as_tibble() |>
    transmute(dt_event = as.Date(onset_date), median = estimate, lower, upper,
              method = "NobBS")
}

# ---- Run several methods -----------------------------------------------------

run_methods <- function(ao, methods, max_delay, window, enw_model = NULL,
                        progress = function(msg) NULL) {
  out <- list(); errors <- list(); times <- list()
  for (m in methods) {
    progress(m)
    t0 <- Sys.time()
    res <- tryCatch(
      switch(m,
        "nowcaster (INLA)" = do_nowcaster(ao, "inla", max_delay, window),
        "nowcaster (MGCV)" = do_nowcaster(ao, "mgcv", max_delay, window),
        "baselinenowcast"  = do_baselinenowcast(ao, max_delay),
        "epinowcast"       = do_epinowcast(ao, max_delay, window, model = enw_model),
        "surveillance"     = do_surveillance(ao, max_delay, window),
        "NobBS"            = do_nobbs(ao, max_delay, window),
        stop("Unknown method")),
      error = function(e) { errors[[m]] <<- conditionMessage(e); NULL })
    times[[m]] <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
    if (!is.null(res)) out[[m]] <- res
  }

  nowcasts <- bind_rows(out)
  if (nrow(nowcasts) > 0) {
    nowcasts <- filter(nowcasts, dt_event >= horizon_start(ao, max_delay),
                       dt_event <= ao$now)
  }
  list(nowcasts = nowcasts,
       errors = tibble(method = names(errors), error = unlist(errors)),
       times = tibble(method = names(times), seconds = unlist(times)))
}

# ---- Scores ------------------------------------------------------------------

score_nowcasts <- function(nowcasts, truth, alpha = 0.05) {
  nowcasts |>
    inner_join(truth, by = "dt_event") |>
    group_by(method) |>
    summarise(
      steps = n(),
      MAE = mean(abs(median - truth)),
      `Rel. error (%)` = 100 * mean((median - truth) / pmax(truth, 1)),
      `Coverage 95%` = mean(truth >= lower & truth <= upper),
      `Interval score 95%` = mean((upper - lower) +
                                    2 / alpha * (lower - truth) * (truth < lower) +
                                    2 / alpha * (truth - upper) * (truth > upper)),
      .groups = "drop") |>
    arrange(`Interval score 95%`)
}

epiweek_lab <- function(x) sprintf("%02d/%02d", epiweek(x), epiyear(x) %% 100)
