library(tidyverse)

timestamped.filename <- function(name) {
  return(paste(format(Sys.time(), "%Y%m%d-%H%M%S"), name, sep='-'))
}

generate.sample <- function(size, seed = 0, file.name = "./base-data.rds"){
  set.seed(seed)
  
  load(file.name)
  
  sample_n(data, size = size, replace = F)
  
}

