s3 = import("lib/loaders/s3")$loader_s3
xlsx = import("lib/loaders/xlsx")$loader_xlsx

loader_generic = function(data_specification, ...) {
  
  # Grabs the extension from the data specification config
  load_type = data_specification$ext
  # Uses the extension to pick a generic data loader
  load_function = subset(tbl_loaders, ext == load_type)$loader[[1]]
  
  # Dispatch the load function, passing along paramaters from 
  #  - The expected paramters defined by the file extention
  #    - These should be provided in the config
  #  - The user defined function that builds on the loader
  #    - These should be provided in the in the init_data_helper
  load_function(c(data_specification$extra, list(...)))

}

# A switch table to pick the right generic loader for an extension
tbl_loaders = tibble::tribble(
  ~ext, ~loader,
  "s3", s3,
  "xlsx", xlsx
)
