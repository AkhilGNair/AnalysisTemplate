# Helper functions
source("lib/import.r")
source("lib/infix.r")

# Normal R libraries
library(data.table)
library(magrittr)

# Data specification config
config = suppressWarnings(config::get(file = "conf/conf.yml"))
load_datasets = import("lib/data/load_datasets")$load_datasets
