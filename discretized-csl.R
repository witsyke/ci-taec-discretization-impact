source("./helper.R")

load("./blacklist.rds")
sample.size <- 25000000
sample <- generate.sample(sample.size)


# pc stable with discrete values (built-in)

#discretize data with bnlearn built-in
discrete.sample.builtin <- discretize(sample)

discrete.sample.builtin1 <- discretize(sample, breaks=2)


#ggplot(data.frame(discrete.sample.builtin2$co), aes(discrete.sample.builtin2$co)) + geom_bar()

computed.net1 <- pc.stable(discrete.sample.builtin, test="mi-sh", blacklist = base::as.data.frame(bl))
# graphviz.plot(computed.net2)

save(computed.net1,file=timestamped.filename(prefix = "./nets/", name = paste(sample.size, "built-in.discrete-3-bin.rds", sep='-')))

computed.net1 <- pc.stable(discrete.sample.builtin1, test="mi-sh", blacklist = base::as.data.frame(bl))
# graphviz.plot(computed.net2)

save(computed.net1,file=timestamped.filename(prefix = "./nets/", name = paste(sample.size, "built-in.discrete-2-bin.rds", sep='-')))

# discretize data with k-means
# already discrete variables: Region, Zone, Type, Season
#for (ki in 2:5) {
#clustered.data <- sample %>%
#  base::sapply(function(x) if(!is.factor(x)){
#    meds <- Ckmedian.1d.dp(x, k=c(3,3))
#    trans <- meds$cluster %>%
#      base::sapply(function(y) meds$centers[[y]])
#    factor(trans)
#    } else x) %>%
#  base::as.data.frame()


#ggplot(data.frame(clustered.data$co), aes(clustered.data$co)) + geom_bar()

#ggplot(data.frame(sample$co), aes(sample$co)) + geom_density() #(aes(y = ..count..))

#for (col in names(clustered.data)) {
#ggplot(data=clustered.data, count(sample$Type)) + geom_histogram()
#ggplot(data.frame(clustered.data[[col]]), aes(clustered.data[[col]])) + geom_bar()

#ggplot(data.frame(sample$ssr), aes(sample$ssr)) + geom_density(aes(y = ..count.., xmin=0))
#}

#}


#computed.net2 <- pc.stable(clustered.data, test="mi-sh", blacklist = base::as.data.frame(bl))
# graphviz.plot(computed.net2)


#save(computed.net2,file=timestamped.filename(prefix = "./nets/", name = paste(sample.size, "k-median-discrete.rds", sep='-')))
