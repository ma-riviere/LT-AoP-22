#========================#
#### Project packages ####
#========================#

project_pkgs <- c(
  ### Base packages
  "renv", 
  "here", 
  "config", 
  "rlang", 
  "fs", 
  "knitr", 
  "rmarkdown", 
  "crayon", 
  "usethis",
  
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
  "qqplotr", # Required by performance
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

  ### Reporting
  "gt",
  "quarto",
  "downlit", # For code linking
  "xml2", # For code linking
  
  ### Misc
  "glue", 
  "styler", 
  "miniUI", 
  "gtools",
  "magrittr"
)