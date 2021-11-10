pkg_lst <- c(
  "knitr",
  "here",
  "readODS",
  "dplyr",
  "broom",
  "flextable",
  "DT",
  "officer",
  "officedown",
  "readtext",
  "aod",
  "drc",
  "gtsummary",
  "ggplot2",
  "ggExtra",
  "ggforce",
  "plotly",
  "VCA",
  "mcr",
  "chemCal",
  "knitr",
  "Rmisc",
  "Hmisc",
  "EnvStats",
  "mcr",
  "languageserver",
  "httpgd",
  "flexdashboard"
)

new_pkg <-
  pkg_lst[!(pkg_lst %in% installed.packages()[, "Package"])]

if (length(new_pkg)) install.packages(new_pkg)
