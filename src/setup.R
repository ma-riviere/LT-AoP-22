#=====================#
#### Project Setup ####
#=====================#

is_installed <- \(pkg) suppressPackageStartupMessages({require(pkg, quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)})

if (!is_installed("here")) {install.packages("here"); require(here, quietly = TRUE)}
here::i_am("src/setup.R")

source(here::here("src", "logger.R"), echo = F)

log.title("\n[SETUP] Setting up the project ...\n")

if (!is_installed("renv")) {install.packages("renv"); require(renv, quietly = TRUE)}
if(is.null(renv::project())) renv::init(project = here::here(), bare = TRUE, restart = FALSE)

if (!startsWith(.libPaths()[1], here::here())) {
  v <- paste0("R-", version$major, ".", strsplit(version$minor, ".", fixed = TRUE)[[1]][1])
  dir <- ifelse(Sys.info()[["sysname"]] == "Windows", "x86_64-w64-mingw32", "x86_64-pc-linux-gnu")
  path <- here::here("renv", "library", v, dir)
  if(!dir.exists(path)) dir.create(path, recursive = TRUE)
  .libPaths(path)
}

project_base_scripts <- c("packages.R", "utils.R", "authors.R", "packman.R")

tmp <- sapply(project_base_scripts, \(f) source(here::here("src", f), echo = FALSE))

init_project_packages()

log.title("[SETUP] Loading additional src scripts ...")

project_scripts <- fs::dir_ls(path = here::here("src"), type = "file", glob = "*.R") |> fs::path_file()

tmp <- sapply(project_scripts[which(project_scripts %ni% c("init.R", "setup.R", project_base_scripts))], \(f) source(here::here("src", f), echo = FALSE))