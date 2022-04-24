#========================#
#### Packages Manager ####
#========================#

options(
  repos = project_repos,
  pkgType = ifelse(Sys.info()[["sysname"]] == "Windows", "both", "source"),
  Ncpus = max(1, parallel::detectCores(logical = TRUE) - 1),
  verbose = FALSE
)

Sys.setenv(MAKEFLAGS = paste0("-j", getOption("Ncpus")))

#---------------------#
#### Main function ####
#---------------------#

init_project_packages <- function() {
  log.title("[PACKAGES] Restoring project packages ...")
  
  renv::restore(prompt = FALSE, repos = project_repos)
  load_packages(c(base_pkgs, project_pkgs))
}

#------------------------#
#### Helper functions ####
#------------------------#

get_pkg_name <- function(pkg) {
  pkg_name <- pkg
  if (grepl("/", pkg_name, fixed = TRUE)) {
    pkg_path <- strsplit(pkg_name, "/", fixed = TRUE)[[1]]
    pkg_name <- pkg_path[length(pkg_path)]
  }
  if (grepl("@", pkg_name, fixed = TRUE)) {
    pkg_path <- strsplit(pkg_name, "@", fixed = TRUE)[[1]]
    pkg_name <- pkg_path[1]
  }
  return(pkg_name)
}

get_pkg_version <- function(pkg) {
  if (grepl("@", pkg, fixed = TRUE)) {
    pkg_path <- strsplit(pkg, split = "@", fixed = TRUE)[[1]]
    return(pkg_path[length(pkg_path)])
  }
  return(NA_character_)
}

load_packages <- function(pkgs) {
  suppressPackageStartupMessages({
    for (pkg in pkgs) {
      pkg_name <- get_pkg_name(pkg)
      if (is_installed(pkg_name)) require(pkg_name, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
    }
  })
}