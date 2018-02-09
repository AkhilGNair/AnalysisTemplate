#### -- Packrat Autoloader (version 0.4.8-36) -- ####
source("packrat/init.R")
#### -- End Packrat Autoloader -- ####

# Don't drop into debugger on error
options(error = NULL)
options("download.file.method" = "libcurl")

# Always print data.tables instead of data.frames
# Might be annoying in some cases, but avoids crashing R
print.data.frame = function(df) {
  data.table::setDT(df)
  print(df)
}

# For file.path linking
root = rprojroot::is_rstudio_project$make_fix_file()()
