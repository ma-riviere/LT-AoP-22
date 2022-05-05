#====================#
#### Project Init ####
#====================#


is_installed <- \(pkg) suppressPackageStartupMessages({require(pkg, quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)})

# here::i_am("src/init.R")

if (!startsWith(.libPaths()[1], here::here())) {
  v <- paste0("R-", version$major, ".", strsplit(version$minor, ".", fixed = TRUE)[[1]][1])
  renv::use(library = here::here("renv", "library", v, "x86_64-w64-mingw32"))
}

project_base_scripts <- c("logger.R", "packages.R", "utils.R", "packman.R")

tmp <- sapply(project_base_scripts, \(f) source(here::here("src", f), echo = F))

load_packages(project_pkgs)

project_scripts <- fs::dir_ls(path = here::here("src"), type = "file", glob = "*.R") |> fs::path_file()

tmp <- sapply(project_scripts[which(project_scripts %ni% c("init.R", "setup.R", project_base_scripts))], \(f) source(here::here("src", f), echo = F))