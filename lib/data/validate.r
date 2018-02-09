#' Validate
#'
#' Validate will  
#'  - Ensure the column names between the model and dataset are equivalent
#'  - Cast any columns to be formatted as a Date or POSIXct
#'  - Extract any values defined as a regex
#'    - Guard against completely mapping a column to NA
#'    - WARNING: Will not guard against partial NA mappings
#'  - Attempt to cast any simple cast types (e.g. char -> int)  
#'
#' @param dt The R model to cast and validate
#' @param model The yml defined model we are validating against
#'
#' @return
#' @export
validate = function(dt, model) {
  
  # Check column names as expected; Just report set differences
  names_model = names(model)
  names_dt = names(dt)
  names_difference = c(setdiff(names_model, names_dt), setdiff(names_dt, names_model))
  
  # Log out if differences exist
  if (length(names_difference) != 0) {
    extra_model = paste0(setdiff(names_model, names_dt), collapse = ", ")
    extra_dt    = paste0(setdiff(names_dt, names_model), collapse = ", ")
    
    if(nchar(extra_model) > 0) cat("Columns: ", extra_model, " missing from data table\n")
    if(nchar(extra_dt) > 0) stop("Columns: ", extra_dt, " missing from model")
  }
  
  # Cast any columns we know we need to cast
  # -- Things we need to strftime or regex extract
  # -- Columns are guarded against whole column NA casts, but not part column NA casts
  
  # format casts
  to_format = find_to_cast(model, "format")
  dt = cast_columns(dt, model, to_format, cast_format)
  
  # regex casts
  to_reg_extract = find_to_cast(model, "regex")
  dt = cast_columns(dt, model, to_reg_extract, cast_regex)
  
  # Only simple casts remaining, check what is still out of sync
  classes = get_classes(dt, model)
  classes_model = classes$model
  classes_dt = classes$dt
  
  # simple casts
  bool_classes   = classes_model == classes_dt
  to_simple_cast = classes_model[!bool_classes]
  dt = cast_columns(dt, model, to_simple_cast, cast_simple)

  # Everything should be synced now
  classes = get_classes(dt, model)
  classes_model = classes$model
  classes_dt = classes$dt
  
  dt
}

#' Find to Cast
#'
#' Finds columns to cast in the model if defined
#' by an easily handled property. Currently implemented:
#'   - format: Datetime Format
#'   - regex:  Regex
#'
#' @param model The yml defined model we are validating against
#' @param cast_type An implemented cast method
#'
#' @return
find_to_cast = function(model, cast_type) {
  unlist(lapply(model, purrr::pluck, cast_type))
}

#' Get classes
#' 
#' Gets the classes defined by the model and the
#' highest level class of the R type for vector comparison
#' 
#' @param dt The R model to cast and validate
#' @param model The yml defined model we are validating against
#'
#' @return
get_classes = function(dt, model) {
  # Check column types as expected
  classes_model = purrr::map_chr(model, purrr::pluck, "r_type")
  # Take highest level class type
  classes_dt = purrr::map_chr(dt[, names(model), with = FALSE], ~ class(.x)[1] )
  
  list(model = classes_model, dt = classes_dt)
}

#' Cast Columns
#'
#' The columns are (inefficiently due to NSE) cast to their intended type
#' by dispatching a cast function against each column in a loop
#' 
#' An error will occur if the column previously had values and is mapped to an NA col
#'
#' @param dt The dataset to manipulate
#' @param model The model that is being cast to 
#' @param column_list The columns to be cast
#' @param cast_function The implementation of the cast type
#'
#' @return
cast_columns = function(dt, model, column_list, cast_function) {
  
  # Helper function in scope of dt
  all_na = function(col) all(is.na(dt[[col]]))  # Check if whole column is na
  
  for (i in seq_along(column_list)) {
    
    # To avoid dropping name
    col = column_list[i]
    col_name = names(col)
    
    # Check if all entries in the column are NA
    # If they are, we don't really care what happens
    if (all_na(col_name)) { no_entries = TRUE } else { no_entries = FALSE }
    
    # Cast the column as per the model
    dt[[col_name]] = cast_function(dt, col)
    
    # If there were no entries, we can skip the NA cast check
    if (no_entries) next()
    # Die if cast goes wrong
    if (all_na(col_name)) stop("Type casting produced NAs in ", col_name)
  }
  
  # Return dt with a casted subset of columns
  dt
}

#' Cast implementations
#'
#' @param dt The dataset with the column to cast
#' @param col The column name and information we need to perform the cast form the model
#'
#' @return
cast_simple = function(dt, col) {
  model_type = unname(col)
  
  # as.Date method is slightly different
  if (model_type == "Date") {
    return(as.Date(dt[[names(col)]]))
  }
  
  as(dt[[names(col)]], model_type)
}

# Implements Date/POSIXct casting
cast_format = function(dt, col) {
  as.POSIXct(dt[[names(col)]], format = unname(col))
}

# Implements regex extraction and casting
cast_regex = function(dt, col) {
  stringr::str_extract(dt[[names(col)]], pattern = unname(col))
}
