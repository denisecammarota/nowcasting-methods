# app.R ----------------------------------------------------------------------
# Interactive comparison of nowcasting methods with a chosen DT_DIGITA cutoff.
# Run with: shiny::runApp("path/to/this/folder")

library(shiny)
library(ggplot2)
source("source.R")

# ---- Configuration -----------------------------------------------------------
# Line-list files (parquet). The first is the default for nowcasting, the last
# the default "truth" (a later download used to score the nowcasts).
FILES <- c("July"     = "dengue-sp-july.parquet",
           "November" = "dengue-sp-nov.parquet")
UF    <- 35
YEARS <- 2025:2026

WEEKLY_METHODS <- c("nowcaster (INLA)", "nowcaster (MGCV)", "baselinenowcast",
                    "epinowcast", "surveillance", "NobBS")
DAILY_METHODS  <- c("baselinenowcast", "epinowcast", "surveillance", "NobBS")
DEFAULTS <- list(weekly = list(max_delay = 15, window = 30, zoom = 20),
                 daily  = list(max_delay = 30, window = 90, zoom = 60))

COLOURS <- c("nowcaster (INLA)" = "#009E73", "nowcaster (MGCV)" = "#56B4E9",
             "baselinenowcast"  = "#E69F00", "epinowcast"       = "#CC79A7",
             "surveillance"     = "#D55E00", "NobBS"            = "#0072B2",
             "Observed at cutoff" = "grey55", "Truth" = "black")

# ---- Data (read once) --------------------------------------------------------
FILES   <- FILES[file.exists(FILES)]
if (length(FILES) == 0) stop("None of the files in FILES were found.")
DATASETS <- lapply(FILES, read_linelist, uf = UF, years = YEARS)

# epinowcast model: compiled once, on first use
.enw_model <- NULL
get_enw_model <- function() {
  if (is.null(.enw_model)) .enw_model <<- epinowcast::enw_model()
  .enw_model
}

# ---- UI ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Nowcasting comparison"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("data_file", "Data to nowcast", choices = names(FILES)),
      selectInput("truth_file", "Truth (later file)",
                  choices = c("None", names(FILES)), selected = tail(names(FILES), 1)),
      dateInput("cutoff", "Cutoff: use reports with DT_DIGITA up to",
                value = max(DATASETS[[1]]$DT_DIGITA, na.rm = TRUE),
                max = max(DATASETS[[1]]$DT_DIGITA, na.rm = TRUE),
                format = "dd/mm/yyyy"),
      radioButtons("scale", "Time scale", c("weekly", "daily"), inline = TRUE),
      checkboxGroupInput("methods", "Methods", choices = WEEKLY_METHODS,
                         selected = c("nowcaster (INLA)", "baselinenowcast")),
      numericInput("max_delay", "Maximum delay (weeks/days)", 15, min = 1),
      numericInput("window", "History window (weeks/days)", 30, min = 2),
      numericInput("zoom", "Steps shown before the nowcast date", 20, min = 5),
      actionButton("run", "Run nowcasts", class = "btn-primary"),
      br(), br(),
      helpText("Weekly = epiweeks starting on Sunday. The last incomplete report",
               "week is dropped, as in nowcaster. The history window is used by",
               "nowcaster, epinowcast, surveillance and NobBS; baselinenowcast",
               "uses the whole history. nowcaster runs only on weekly data.")
    ),
    mainPanel(
      width = 9,
      uiOutput("status"),
      plotOutput("plot", height = "500px"),
      h4("Scores against the truth file"),
      tableOutput("scores"),
      h4("Run time and errors"),
      tableOutput("times"),
      tableOutput("errors"),
      downloadButton("download", "Download nowcasts (CSV)")
    )
  )
)

# ---- Server ------------------------------------------------------------------
server <- function(input, output, session) {

  # Methods and defaults depend on the time scale
  observeEvent(input$scale, {
    ch  <- if (input$scale == "weekly") WEEKLY_METHODS else DAILY_METHODS
    sel <- intersect(input$methods, ch)
    if (length(sel) == 0) sel <- "baselinenowcast"
    updateCheckboxGroupInput(session, "methods", choices = ch, selected = sel)
    d <- DEFAULTS[[input$scale]]
    updateNumericInput(session, "max_delay", value = d$max_delay)
    updateNumericInput(session, "window", value = d$window)
    updateNumericInput(session, "zoom", value = d$zoom)
  }, ignoreInit = TRUE)

  # Cutoff range follows the chosen data file
  observeEvent(input$data_file, {
    rng <- range(DATASETS[[input$data_file]]$DT_DIGITA, na.rm = TRUE)
    updateDateInput(session, "cutoff", min = rng[1], max = rng[2], value = rng[2])
  })

  results <- eventReactive(input$run, {
    validate(need(length(input$methods) > 0, "Choose at least one method."),
             need(input$window > input$max_delay,
                  "The history window must be longer than the maximum delay."))

    ao <- as_of(DATASETS[[input$data_file]], input$cutoff, input$scale)
    methods <- input$methods
    needs_enw <- "epinowcast" %in% methods

    withProgress(message = "Running nowcasts", value = 0, {
      enw_model <- NULL
      if (needs_enw) {
        incProgress(0, detail = "compiling epinowcast model (first time only)")
        enw_model <- tryCatch(get_enw_model(), error = function(e) NULL)
      }
      n <- length(methods); i <- 0
      res <- run_methods(ao, methods, input$max_delay, input$window,
                         enw_model = enw_model,
                         progress = function(m) { i <<- i + 1; incProgress(1 / n, detail = m) })
    })

    truth <- NULL; truth_ok <- NA
    if (input$truth_file != "None") {
      later <- DATASETS[[input$truth_file]]
      truth <- truth_counts(later, input$scale, input$max_delay)
      # truth is complete only if the later file has max_delay steps of reports after "now"
      truth_ok <- max(later$DT_DIGITA, na.rm = TRUE) >=
        ao$now + input$max_delay * ao$step + (ao$step - 1)
    }

    c(res, list(ao = ao, observed = observed_counts(ao), truth = truth,
                truth_ok = truth_ok, max_delay = input$max_delay, zoom = input$zoom))
  })

  output$status <- renderUI({
    r <- results()
    msg <- sprintf("Nowcast date (%s): %s. Reports used up to %s.",
                   r$ao$scale,
                   format(r$ao$now, "%d/%m/%Y"),
                   format(r$ao$last_report, "%d/%m/%Y"))
    if (r$ao$scale == "weekly") msg <- paste0(msg, " Epiweek ", epiweek_lab(r$ao$now), ".")
    if (isFALSE(r$truth_ok)) {
      msg <- paste(msg, "<b>Warning:</b> the truth file does not yet have",
                   r$max_delay, "steps of reports after the nowcast date, so the",
                   "truth for the latest steps is still incomplete.")
    }
    HTML(msg)
  })

  output$plot <- renderPlot({
    r <- results()
    ao <- r$ao
    x_from <- ao$now - r$zoom * ao$step
    obs   <- filter(r$observed, dt_event >= x_from, dt_event <= ao$now)
    truth <- if (!is.null(r$truth)) filter(r$truth, dt_event >= x_from, dt_event <= ao$now)
    nc    <- r$nowcasts

    p <- ggplot(mapping = aes(x = dt_event))
    if (nrow(nc) > 0) {
      p <- p +
        geom_ribbon(data = nc, aes(ymin = lower, ymax = upper, fill = method), alpha = 0.15) +
        geom_line(data = nc, aes(y = median, colour = method), linewidth = 0.8)
    }
    p <- p +
      geom_line(data = obs, aes(y = Cases, colour = "Observed at cutoff"), linetype = "dashed") +
      geom_point(data = obs, aes(y = Cases, colour = "Observed at cutoff"), size = 1)
    if (!is.null(truth)) {
      p <- p +
        geom_line(data = truth, aes(y = truth, colour = "Truth")) +
        geom_point(data = truth, aes(y = truth, colour = "Truth"), size = 1)
    }

    x_scale <- if (ao$scale == "weekly") {
      scale_x_date(breaks = seq(ao$now - 2 * 7 * (r$zoom %/% 2), ao$now, by = "2 weeks"),
                   labels = epiweek_lab, name = "Epiweek")
    } else {
      scale_x_date(date_breaks = "1 week", date_labels = "%d/%m", name = "Date of onset")
    }

    p + x_scale +
      scale_colour_manual(values = COLOURS, name = NULL) +
      scale_fill_manual(values = COLOURS, guide = "none") +
      coord_cartesian(xlim = c(x_from, ao$now)) +
      expand_limits(y = 0) +
      labs(y = "Nº Cases") +
      theme_bw(base_size = 13) +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 90, vjust = 0.5))
  })

  output$scores <- renderTable({
    r <- results()
    if (is.null(r$truth) || nrow(r$nowcasts) == 0) return(NULL)
    score_nowcasts(r$nowcasts, r$truth)
  }, digits = 2)

  output$times  <- renderTable(results()$times)
  output$errors <- renderTable({
    e <- results()$errors
    if (nrow(e) == 0) NULL else e
  })

  output$download <- downloadHandler(
    filename = function() sprintf("nowcasts_%s_%s.csv", input$scale, input$cutoff),
    content = function(file) {
      r <- results()
      out <- r$nowcasts
      if (!is.null(r$truth)) out <- left_join(out, r$truth, by = "dt_event")
      write.csv(out, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
