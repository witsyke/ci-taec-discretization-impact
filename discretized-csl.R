source("./helper.R")

load("./blacklist.rds")
sample.sizes <-
  c(100, 1000, 10000, 100000, 1000000, 5000000, 10000000)
bins <- c(2, 3, 4, 5, 7, 10)

for (sample.size in sample.sizes) {
  sample <- generate.sample(sample.size)
  for (bin.count in bins) {
    #discretize data with bnlearn built-in
    discrete.sample.builtin <-
      discretize(sample, breaks = bin.count)
    
    # pc stable with discrete values (built-in)
    cat(paste("discrete built-in,",
              bin.count,
              "bins,",
              sample.size,
              "samples: "))
    
    cat(system.time(computed.net1 <- pc.stable(discrete.sample.builtin,
                                               test = "mi-sh",
                                               blacklist = base::as.data.frame(bl))))
    
    cat("\n")
    
    save(computed.net1, file = filename(prefix = "./nets/",
                                        name = paste(sample.size,
                                                     "built-in-discrete",
                                                     bin.count,
                                                     "bin.rds",
                                                     sep = '-')))
    
    
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
    
    cat(paste("discrete k-median,",
              bin.count,
              "clusters,",
              sample.size,
              "samples: "))
    
    cat(system.time(computed.net2 <- pc.stable(clustered.data,
                                               test = "mi-sh",
                                               blacklist = base::as.data.frame(bl))))
    
    cat("\n")
    
    save(computed.net2, file = filename(prefix = "./nets/",
                                        name = paste(sample.size,
                                                     "k-median",
                                                     bin.count,
                                                     "clusters.rds",
                                                     sep = '-')))
  }
}
