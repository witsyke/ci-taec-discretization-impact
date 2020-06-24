timestamped.filename <- function(name) {
  return(paste(format(Sys.time(), "%Y%m%d-%H%M%S"), name, sep='-'))
}