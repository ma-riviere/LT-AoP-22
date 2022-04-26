#====================#
#### Project Init ####
#====================#

cat("\n[SETUP] Setting up Project ...\n")

is_installed <- \(pkg) suppressPackageStartupMessages({require(pkg, quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)})

if (!is_installed("renv")) {install.packages("renv"); require(renv, quietly = TRUE)}
if (!is_installed("here")) {install.packages("here"); require(here, quietly = TRUE)}

here::i_am("src/init.R")

if(is.null(renv::project())) renv::init(project = here::here(), bare = TRUE, restart = FALSE)

## Temporary fix for renv library path issue
if (!startsWith(.libPaths()[1], here::here())) {
  v <- paste0("R-", version$major, ".", strsplit(version$minor, ".", fixed = TRUE)[[1]][1])
  renv::use(library = here::here("renv", "library", v, "x86_64-w64-mingw32"))
}

project_base_scripts <- c("logger.R", "packages.R", "utils.R", "authors.R", "packman.R")

tmp <- sapply(project_base_scripts, \(f) source(here::here("src", f), echo = F))

init_project_packages()

log.title("[SETUP] Loading additional src scripts ...")

project_scripts <- fs::dir_ls(path = here::here("src"), type = "file", glob = "*.R") |> fs::path_file()

tmp <- sapply(project_scripts[which(project_scripts %ni% c("init.R", project_base_scripts))], \(f) source(here::here("src", f), echo = F))