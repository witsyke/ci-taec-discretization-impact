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
    load("./base-data-datetime.RDS")
  } else {
    print("This option is not recommend, as it will take ages to donwload the data!")
    link = url("https://onedrive.live.com/download?cid=D09637ADEC795D08&resid=D09637ADEC795D08%21214&authkey=AHHe9Alke-E72J4")
    source(link)
  }
  
  sample <- sample_n(data.datetime, size = size, replace = F)
  rm(data)
  
  sample
  
}

# applies final presentation theme to plots
apply_theme <- function(plot){
  plot + 
    theme(panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.box.background = element_rect(fill = "transparent", color = NA),
          legend.position = "none",
          axis.text.x = element_text(colour="white", size = 16),
          #axis.text.x=element_blank(),
          #axis.ticks.x=element_blank(),
          #line = element_line(colour = "#10e7d9"),
          axis.text.y = element_text(colour="white", size = 18),
          text = element_text(colour="white", size = 16),
    ) +
    scale_fill_manual(values = c("#10e7d9", "#24d8a0", "#1d4c7a", "#a7f5b8", "#1781a1"))
  
}

# support function to allow saving with transparent background
save_with_theme <- function(plot, file = "newplot.png"){
  ggsave(plot, filename = file,  bg = "transparent", width=6) #, width=5, height=3)
}

