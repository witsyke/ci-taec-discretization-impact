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
    print("This option is not recommend, as it will take ages to donwload the data!")
    link = url("https://onedrive.live.com/download?cid=D09637ADEC795D08&resid=D09637ADEC795D08%21214&authkey=AHHe9Alke-E72J4")
    source(link)
  }
  
  sample <- sample_n(data, size = size, replace = F)
  rm(data)
  
  sample
  
  }

