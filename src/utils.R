#===============================#
#### Miscellaneous functions ####
#===============================#

log.main("[UTILS] Loading Utils ...")

"%ni%" <- Negate("%in%")

"%s+%" <- \(lhs, rhs) paste0(lhs, rhs)

"%ne%" <- \(lhs, rhs) if(is.null(lhs) || rlang::is_empty(lhs) || lhs == "") return(rhs) else return(lhs)

label_pval <- \(p) ifelse(p <= alpha, str_c(scales::label_pvalue()(p), gtools::stars.pval(p) |> str_replace(fixed("."), ""), sep = " "), scales::label_pvalue()(p))

get_response_name <- function(var) {
  if(exists(paste0(var, "_name"))) return(eval(parse(text = get_var_name(!!paste0(var, "_name")))))
  else return(var)
}

describe_variables <- function(dat) {
  return(
    data.frame(Variable = colnames(dat))
    |> mutate(Description = Vectorize(get_response_name)(Variable) |> str_remove_all("\n")) # |> str_remove_all(fixed("*"))
    |> filter(Variable != Description)
    |> gt()
    |> fmt_markdown(columns = Description)
  )
}

get_model_family <- function(mod) {
  family <- insight::get_family(mod)$family |> stringr::str_to_sentence()
  link <- insight::get_family(mod)$link
  
  model_tag <- glue::glue("{family} ('{link}')")
  
  cov_struct <- stringr::str_match(insight::get_call(mod)$formula |> toString(), "\\s(\\w{2,3})\\(.*\\)")[[2]]
  if (!is.null(cov_struct) && !is.na(cov_struct) && cov_struct != "") model_tag <- glue::glue("{model_tag} + {toupper(cov_struct)}")
  
  return(model_tag)
}


get_model_tag <- function(mod) {
  resp <- insight::find_response(mod)
  return(glue::glue("{resp} - {get_model_family(mod)}"))
}


print_model_call <- function(mod) {
  cat("```{{r}}\n")
  print(insight::get_call(mod))
  cat("```\n")
}


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


save_png <- function(plot, filename = NULL, subfolder = "", bg = "white", dpi = 600, width = 8, height = 8, display = TRUE) {
  if(is.null(filename)) filename <- as.list(match.call()[-1])$plot
  
  file_path <- here("fig", paste0(filename, ".png"))
  if(!is.null(subfolder) && subfolder != "") {
    if(!fs::dir_exists(here::here("fig", subfolder))) fs::dir_create(here::here("fig", subfolder))
    file_path <- here("fig", subfolder, paste0(filename, ".png"))
  }
  
  ggsave(filename = file_path, plot = plot, device = "png", scale = 1, dpi = dpi, width = width, height = height, bg = bg)
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