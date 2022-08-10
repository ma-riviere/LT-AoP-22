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
  "qqplotr",           # Required by performance
  "correlation",
  "psych",             # For categorical correlations
  "parameters",
  "DHARMa",
  "emmeans",
  
  ### Visualizations
  "wilkelab/gridtext", # Fixing Markdown display issue with R4.2+
  "ggplot2",
  "ggtext",
  "patchwork",
  "see",
  "ggdist",
  "bayesplot",

  ### Reporting
  "gt",
  "gtExtras",
  "knitr",
  "rmarkdown",
  "quarto",
  "downlit",           # For code linking
  "xml2",              # For code linking
  "sessioninfo",
  
  ### Misc
  "glue", 
  "styler", 
  "miniUI", 
  "gtools",
  "magrittr"
)
