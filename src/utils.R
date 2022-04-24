#===============================#
#### Miscellaneous functions ####
#===============================#

log.main("[UTILS] Loading Utils ...")

"%ni%" <- Negate("%in%")

"%s+%" <- function(lhs, rhs) paste0(lhs, rhs)


### From: https://michaelbarrowman.co.uk/post/getting-a-variable-name-in-a-pipeline/
get_var_name <- function(x) {
  lhs <- get_lhs()
  if(is.null(lhs)) lhs <- rlang::ensym(x)
  return(rlang::as_name(lhs))
}


get_lhs <- function() {
  calls <- sys.calls()
  
  #pull out the function or operator (e.g. the `%>%`)
  call_firsts <- lapply(calls, `[[`, 1) 
  
  #check which ones are equal to the pipe
  pipe_calls <- vapply(call_firsts,identical, logical(1), quote(`%>%`))
  
  #if we have no pipes, then get_lhs() was called incorrectly
  if(all(!pipe_calls)){
    NULL
  } else {
    #Get the most recent pipe, lowest on the 
    pipe_calls <- which(pipe_calls)
    pipe_calls <- pipe_calls[length(pipe_calls)]
    
    #Get the second element of the pipe call
    this_call <- calls[[c(pipe_calls, 2)]]
    
    #We need to dig down into the call to find the original
    while(is.call(this_call) && identical(this_call[[1]], quote(`%>%`))){
      this_call <- this_call[[2]]
    }
    this_call
    
  }
}


get_current_file_name <- function() {
  rstudioapi::getActiveDocumentContext()$path |> str_split(pattern = "/") |> first() |> last() |> str_split("[.]") |> first() |> first()
}


save_png <- function(plot, filename = NULL, subfolder = "", dpi = 600, width = 8, height = 8, display = TRUE) {
  if(is.null(filename)) filename <- as.list(match.call()[-1])$plot
  
  file_path <- here("fig", paste0(filename, ".png"))
  if(subfolder != "") {
    if(!dir.exists(here::here("fig", subfolder))) dir.create(here::here("fig", subfolder))
    file_path <- here("fig", subfolder, paste0(filename, ".png"))
  }
  
  ggsave(filename = file_path, plot = plot, device = "png", scale = 1, dpi = dpi, width = width, height = height)
  if(display) return(plot)
}


## Get element by name from list:
rmatch <- function(x, name) {
  pos <- match(name, names(x))
  if (!is.na(pos)) return(x[[pos]])
  for (el in x) {
    if (class(el) == "list") {
      out <- Recall(el, name)
      if (!is.null(out)) return(out)
    }
  }
}


## Convert matrix to math latex notation
matrix2latex <- function(mat) {
  printmrow <- \(x) cat(paste0(x, collapse = " & "), "\\\\\n")
  cat("$$\n", "\\begin{bmatrix}", "\n", sep = "")
  body <- apply(mat, 1, printmrow)
  cat("\\end{bmatrix}", "\n$$", sep = "")
}


#-------------------#
#### Stats Utils ####
#-------------------#

get_stars <- function(expr, p.val) {
  return(ifelse(expr == regulation_type$NOT_REG, "", gtools::stars.pval(p.val)))
}


poly_encoding <- function(fctr) {
  contrasts(fctr) <- contr.poly
  return(
    car::Recode(fctr, glue::glue_collapse(glue::glue("'{levels(fctr)}' = {as.vector(contrasts(fctr)[,1])}"), sep = "; ") |> as_string()) |> 
      as.character() |> 
      as.numeric()
  )
}


label_encoding <- function(var) {
  vals <- unique(var)
  car::Recode(var, glue::glue_collapse(glue::glue("'{vals}' = {as.vector(seq.int(1, length(vals)))}"), sep = "; ") |> as_string()) |> 
    as.character() |> 
    as.numeric()
}