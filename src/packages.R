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
  "insight",
  "datawizard",
  "performance",
  "correlation",
  "psych",
  "parameters",
  "DHARMa",
  "emmeans",
  
  ### Visualizations
  "wilkelab/gridtext",
  "ggplot2",
  "ggtext",
  "patchwork",
  "see",
  "ggdist",
  "bayesplot",
  "GGally",

  ### Reporting
  "gt",
  
  ### Misc
  "glue", 
  "styler", 
  "miniUI", 
  "gtools",
  "magrittr"
)