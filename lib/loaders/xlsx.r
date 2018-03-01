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
  
  # Construct parameter list to call read_excel with
  excel_params = list(path = params$file, sheet = params$sheet)
  
  # If the data range is passed, add it to the called params
  if ("range" %in% names(params)) excel_params = c(list(range = params$range), excel_params)
  
  # Read the excel file with specified paramaters and set as a data.table
  dt = do.call(readxl::read_excel, excel_params)
  data.table::setDT(dt)
  
  dt[]
  
}