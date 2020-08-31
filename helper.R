library(bnlearn)
library(Rgraphviz)
library(chron)
library(rbin)
library(Ckmeans.1d.dp)
library(tidyverse)
library(causaleffect)
library(igraph)

filename <- function(prefix, name) {
  return(paste(prefix,  name, sep = ''))
}

generate.sample <- function(size, seed = 0) {
  set.seed(seed)
  load("./base-data.RDS")
  sample <- sample_n(data, size = size, replace = F)
  rm(data)
  sample
}
