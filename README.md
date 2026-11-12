# Nowcasting comparison app

Compares nowcaster (INLA and mgcv), baselinenowcast, epinowcast, surveillance::nowcast and NobBS on the same line list. You choose the `DT_DIGITA` cutoff and the time scale (weekly or daily). On daily data, nowcaster is not offered.

## Files

- `source.R`: data helpers and one wrapper per method (`do_nowcaster`, `do_baselinenowcast`, `do_epinowcast`, `do_surveillance`, `do_nobbs`), plus `run_methods()` and `score_nowcasts()`.
- `app.R`: the Shiny app. Set `FILES`, `UF` and `YEARS` at the top of the file.

Put both files in the same folder as the parquet files and run:

```r
shiny::runApp(".")
```

## Shared conventions

- Weekly data uses MMWR epiweeks, which start on Sunday. `dt_event` is the Sunday of each week.
- Only reports with `DT_DIGITA <= cutoff` are used. For weekly data, the last incomplete report week is also dropped, using the same rule as nowcaster.
- Delays longer than the maximum delay are ignored, and negative delays are dropped.
- Every method returns the last `max_delay` steps, as nowcaster does, with the median and the 2.5% and 97.5% quantiles.
- The truth comes from a later file. For each onset week or day, it counts only reports with a delay of at most `max_delay`. The scores are MAE, relative error, 95% coverage and the 95% interval score.

## Installation

```r
install.packages(c("shiny", "ggplot2", "dplyr", "tidyr", "lubridate", "arrow",
                   "surveillance", "NobBS", "baselinenowcast"))

# NobBS needs JAGS installed on the system (https://mcmc-jags.sourceforge.io/)

# epinowcast needs cmdstanr and CmdStan
install.packages("epinowcast",
                 repos = c("https://epinowcast.r-universe.dev", getOption("repos")))
install.packages("cmdstanr",
                 repos = c("https://stan-dev.r-universe.dev", getOption("repos")))
cmdstanr::install_cmdstan()

# nowcaster needs INLA
install.packages("INLA",
                 repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"),
                 dep = TRUE)
remotes::install_github("covid19br/nowcaster")
```

## Notes per method

- **surveillance**: its `"1 week"` aggregation uses ISO weeks, which start on Monday. To keep Sunday weeks, each epiweek is run as one "pseudo day" and the results are mapped back to the Sunday. The wrapper uses `bayes.trunc` with a Poisson-gamma prior centred on recent counts. `m` is the history window.
- **epinowcast**: weekly runs use `rw(week)`. Daily runs add day-of-week effects to both the expectation and the reporting delay. The Stan model compiles on first use, so the first run is slow.
- **NobBS**: uses the negative binomial model, with the history window as `moving_window`.
- **baselinenowcast**: uses the whole history, as in `test.R`.
- **Deployment**: INLA, CmdStan and JAGS are hard to install on shinyapps.io. Running the app locally, or on a server where you control the system libraries, is the simplest option.
