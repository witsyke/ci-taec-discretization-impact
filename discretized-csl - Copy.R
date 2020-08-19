source("./helper.R")

load("./blacklist.rds")
sample.size <- 12500000
sample <- generate.sample(sample.size)


# pc stable with discrete values (built-in)

#discretize data with bnlearn built-in
discrete.sample.builtin <- discretize(sample)

computed.net1 <- pc.stable(discrete.sample.builtin, test="mi-sh", blacklist = base::as.data.frame(bl))
# graphviz.plot(computed.net2)

save(computed.net1,file=timestamped.filename(prefix = "./nets/", name = paste(sample.size, "built-in.discrete.rds", sep='-')))

# discretize data with k-means
# already discrete variables: Region, Zone, Type, Season

clustered.data <- sample %>%
  base::sapply(function(x) if(!is.factor(x)){factor(Ckmedian.1d.dp(x, k=c(3,5))$cluster)} else x) %>%
  base::as.data.frame()


computed.net2 <- pc.stable(clustered.data, test="mi-sh", blacklist = base::as.data.frame(bl))
# graphviz.plot(computed.net2)


save(computed.net2,file=timestamped.filename(prefix = "./nets/", name = paste(sample.size, "k-median-discrete.rds", sep='-')))
