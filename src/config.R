#======================#
#### Project Config ####
#======================#

log.main("[CONFIG] Configuring project ...")

config <- config::get()

options(
  scipen = 999L, 
  digits = 4L,
  mc.cores = max(1L, parallel::detectCores(logical = TRUE)),
  glmmTMB.cores = max(1L, parallel::detectCores(logical = FALSE)),
  na.action = "na.omit",
  contrasts = c("contr.sum", "contr.poly"),
  seed = 256
)

set.seed(getOption("seed"))

#-------------------------#
#### Knitr & RMarkdown ####
#-------------------------#

knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  fig.align = "center",
  # fig.retina = 2,
  dpi = 250,
  dev = 'svg',
  dev.args = list(bg = "transparent")
)

dpi_save_png <- knitr::opts_chunk$get("dpi")

#------------------------#
#### Packages options ####
#------------------------#

afex::afex_options(
  type = 3,
  method_mixed = "KR",
  include_aov = TRUE,
  factorize = FALSE,
  check_contrasts = FALSE,
  es_aov = "pes",
  correction_aov = "HF"
)

emmeans::emm_options(
  lmer.df = "kenward-roger",
  opt.digits = 4,
  back.bias.adj = FALSE 
)

#---------------#
#### Masking ####
#---------------#

get <- base::get

#-----------------#
#### Constants ####
#-----------------#

alpha <- 0.05
threshold.reg <- 1