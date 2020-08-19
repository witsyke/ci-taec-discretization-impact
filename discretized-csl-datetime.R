source("./helper2.R")

load("./blacklist-datetime.rds")
sample.sizes <- c(15000000, 12500000)
bins <- c(10, 7, 5, 4, 3, 2)

for (sample.size in sample.sizes) {
  sample <- generate.sample(sample.size)
  for (bin.count in bins) {
    
    #discretize data with bnlearn built-in
    discrete.sample.builtin <- discretize(sample, breaks = bin.count)
    
    # pc stable with discrete values (built-in)
    cat(paste("discrete built-in,", bin.count, "bins,", sample.size, "samples:"))
    
    cat(system.time(computed.net1 <-
                      pc.stable(
                        discrete.sample.builtin,
                        test = "mi-sh",
                        blacklist = base::as.data.frame(bl)
                      )))
    
    cat("\n")
    
    save(computed.net1, file = timestamped.filename(
      prefix = "./nets/",
      name = paste(
        sample.size,
        "datetime-built-in-discrete",
        bin.count,
        "bin.rds",
        sep = '-'
      )
    ))
    
    
    # discretize data with k-means
    clustered.data <- sample %>%
      base::sapply(function(x)
        if (!is.factor(x)) {
          meds <- Ckmedian.1d.dp(x, k = c(bin.count, bin.count))
          trans <- meds$cluster %>%
            base::sapply(function(y)
              meds$centers[[y]])
          factor(trans)
        } else
          x) %>%
      base::as.data.frame()
    
    cat(paste("discrete k-median,", bin.count, "clusters,", sample.size, "samples:"))
    
    cat(system.time(computed.net2 <-
                      pc.stable(clustered.data,
                                test = "mi-sh",
                                blacklist = base::as.data.frame(bl))))
    
    cat("\n")
    
    save(computed.net2, file = timestamped.filename(
      prefix = "./nets/",
      name = paste(sample.size, "datetime-k-median", bin.count, "clusters.rds", sep = '-')
    ))
  }
}
