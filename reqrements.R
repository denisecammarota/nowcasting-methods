#install.packages(c("shiny", "ggplot2", "dplyr", "tidyr", "lubridate", "arrow",
#                   "surveillance", "NobBS", "baselinenowcast")) # these ones i already have installed

# NobBS needs JAGS installed on the system (https://mcmc-jags.sourceforge.io/)

# epinowcast needs cmdstanr and CmdStan - needed to install again
install.packages("epinowcast",
                 repos = c("https://epinowcast.r-universe.dev", getOption("repos")))
install.packages("cmdstanr",
                 repos = c("https://stan-dev.r-universe.dev", getOption("repos")))
cmdstanr::install_cmdstan()

# nowcaster and INLA - already had installed 
#install.packages("INLA",
#                 repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"),
#                 dep = TRUE)
#remotes::install_github("covid19br/nowcaster")