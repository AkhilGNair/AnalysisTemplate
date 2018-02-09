#!/usr/bin/r

# Run from command line if littler is installed

if (Sys.getenv("RSTUDIO") == "") source(".Rprofile")

. = lapply(dir("munge", full.names = TRUE), source); rm(.)
