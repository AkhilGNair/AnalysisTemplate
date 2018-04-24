# To enable import
source(file.path(root, "lib/import.r")) 
init_data = import("lib/data/init_data_helpers")
validate = import("lib/data/validate")$validate

#' load_datasets
#'
#' Loads all the datasets specified into the global environment
#'
#' @param config The dataset configurations
#'
#' @return
#' @export
load_datasets = function(config) {
  
  # Get the names of the datasets
  datasets = names(config$data_specification)
  
  # If an order exists, enforce it, otherwise move on
  datasets = order_datasets(datasets, config)
  
  # Walk over each dataset to process it
  purrr::iwalk(datasets, ~ load_dataset(.x, config = config$data_specification[[.y]]))
  
}


#' Load an individual dataset from config file 
#'
#' Control flow specifies whether to load from the cache, or initiailise
#' and cache the result for next time
#'
#' @param dataset The dataset to intialise
#' @param config The config file with the data initialisation specification
#'
#' @return
#' @export
load_dataset = function(dataset, config) {
  
  cache         = 'cache/'
  initialise    = config$initialise
  assign_to     = config$assign
  init_function = config$init_function
  filepath      = config$save_as
  model         = config$model
  
  # To initialise, change initialise flag in "conf/conf.yml" to TRUE
  if (file.exists(filepath)) {
    
    # If the file exists in the cache load it globally and return
    assign(assign_to, fst::read.fst(filepath, as.data.table = TRUE), envir = globalenv())
    
    cat(paste0("FOUND: ", dataset, " loaded as ", assign_to, "\n"))
    return()
    
  } else if (initialise) {
    
    # Initialisation will
    #  - Read in the initial data via the helper function
    #  - Validate the munged data against the target model
    #  - Cache the validated model via fst in ./cache
    
    # Use the helper function to initialise the data from the config
    # Assign the resulting dataset to the global environment
    assign(assign_to, init_data[[init_function]](config, dataset), envir = globalenv())
    
    # Modify all columns in place
    colnames = purrr::map_chr(model, purrr::pluck, "read_name")
    lapply(list_meta, setnames, unname(colnames), names(colnames))

    # Validate the initialised dataset against the expected model 
    # -- If some columns are cast in the model validation, we need to reassign
    assign(assign_to, validate(get(assign_to), model), envir = globalenv())
    cat(paste0("VALIDATED: ", dataset, "\n"))
    
    # Write to cache for easy loading next time
    fst::write.fst(get(assign_to), path = filepath)
    
    cat(paste0("CACHED: ", dataset, " available as ", assign_to, "\n"))
    
  } else {
    
    cat(paste0("MISSED: Initialise ", dataset," in '", cache, "' by changing relevant initialise flag in 'conf/conf.yml' to TRUE\n"))
    
  }
  
}

#' Define the order in which we process the specified datasets 
#'
#' Returns the dataset handles in the correct order, if an order exists
#'
#' @param datasets The datasets to intialise
#' @param config The config file with the data initialisation specification
#'
#' @return
#' @export
order_datasets = function(datasets, config) {
  order_datasets = purrr::modify_depth(config, 2, "order") %>% unlist()
  if (!is.null(order_datasets)) {
    order_datasets = order_datasets %>% order()
    datasets = datasets[order_datasets]
  }
  datasets
}
