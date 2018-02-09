#' Import library into environment
#'
#' Super simple import function inspired by klmr/modules
#' Simplicity makes it far more brittle + opinionated
#'
#' @param module_name The module filepath
#'
#' @examples
#' pkg = import("lib/pkg")
#'
#' @export
import = function(module_name) {
  
  file_path_candidate = function(module_name, ext) file.path(module_name, ext, fsep = ".")
  
  do_import = function(module_name) {
    # Set up a new environment
    env = structure(
      new.env(parent = globalenv()),
      class = c('namespace', 'environment')
    )
    
    # Try the two types of R file extension, r or R
    filepath = file_path_candidate(module_name, "r")
    if (file.exists(filepath))  eval(parse(filepath, encoding = 'UTF-8'), envir = env)
    
    filepath = file_path_candidate(module_name, "R")
    if (file.exists(filepath))  eval(parse(filepath, encoding = 'UTF-8'), envir = env)
    
    if (dir.exists(module_name)) {
      module_name = normalizePath(module_name)
      submodules = tools::file_path_sans_ext(dir(module_name, full.names = TRUE))
      subenvs = lapply(submodules, import)
      
      for (i in seq_along(submodules)) {
        env[[basename(submodules[i])]] = subenvs[[i]]
      }
      
    }
    
    env
    
  }
  
  do_import(module_name)
  
}

