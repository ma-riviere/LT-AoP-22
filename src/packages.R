#========================#
#### Project packages ####
#========================#

project_repos <- c(
  # EasyStats = "https://easystats.r-universe.dev",
  CRAN = "https://cloud.r-project.org/"
)

base_pkgs <- c("renv", "here", "config", "rlang", "fs", "knitr", "rmarkdown", "crayon", "usethis")

project_pkgs <- c(
  ### Data wrangling
  "tibble",
  "janitor",
  "readxl",
  "stringr",
  "purrr",
  "tidyr",
  "dplyr",

  ### Model fitting
  "car",
  "afex",
  "glmmTMB",
  "forecast",
  "optimx",
  
  ### Model analysis
  "broom",
  "easystats/insight",
  "easystats/datawizard",
  "performance",
  "correlation",
  "easystats/parameters",
  "DHARMa",
  "emmeans",
  
  ### Visualizations
  "ggplot2",
  "ggtext",
  "patchwork",
  "see",
  "ggdist",
  "bayesplot",
  "GGally",

  ### Reporting
  
  ### Misc
  "glue", 
  "styler", 
  "miniUI", 
  "gtools",
  "magrittr"
)