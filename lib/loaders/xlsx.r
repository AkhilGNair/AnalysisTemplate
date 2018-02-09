# A method to handle opening excel files and extracting the table on a sheet
# This function is used if the excel file has a simple format, else a custom
# function is expected to be written
# - An excel file
# - An excel sheet

expected_params = c("file", "sheet")

loader_xlsx = function(...) {
  
  params = as.list(...)
  
  # Check the correct parameters have been provided in the config > extra section
  if(!all(expected_params %in% names(params)))
    stop("Please pass", paste0(expected_params, collapse = ", "), "to xlsx_loader")
  
  file = params$file
  sheet = params$sheet
  
  dt = data.table::setDT(readxl::read_excel(path = file, sheet = sheet))
  
  dt[]
  
}