library(bnlearn)
library(Rgraphviz)
library(chron)
library(rbin)
library(Ckmeans.1d.dp)
library(tidyverse)
library(causaleffect)
library(igraph)

timestamped.filename <- function(prefix, name) {
  return(paste(prefix, format(Sys.time(), "%Y%m%d-%H%M%S"), name, sep='-'))
}

generate.sample <- function(size, seed = 0, dev = TRUE){
  
  set.seed(seed)
  
  if(dev){
    load("./base-data.rds")
  } else {
    print("Currently only development option possible!")
    #link = url("https://drive.google.com/file/d/1VHmAELtWWn9PMvMFMKts_wPmxN_qt4Cd/view?usp=sharing")#
    #load(link)
  }
  
  sample_n(data, size = size, replace = F)
  
}

