source("./helper.R")


#discretize data with bnlearn built-in
discrete.sim.data <- discretize(sim.data)

# discretize data with k-means
# discrete variables: Region, Zone, Type, Season
# does not work well for now
discrete.variables <- c("Region","Zone", "Type", "Season")
contin.data <- sim.data[,!(names(sim.data) %in% discrete.variables)]
clustered.data <- sim.data[,(names(sim.data) %in% discrete.variables)]

for(column in names(contin.data)) {
  values <- contin.data[[column]]
  k.medians <- Ckmedian.1d.dp(values, k=c(3,5))
  clustered.values <- factor(k.medians$cluster)
  clustered.data[[column]] <- clustered.values
}


# pc stable with discrete values
computed.net2 <- pc.stable(discrete.sim.data, test="mi-sh", blacklist=bl)
graphviz.plot(computed.net2)
computed.net3 <- pc.stable(clustered.data, test="mi-sh", blacklist=bl)
graphviz.plot(computed.net3)




save(bn,file=timestamped.filename("original.rda"))
save(computed.net, file=timestamped.filename("mixed.rda"))
save(computed.net2,file=timestamped.filename("built-in.discrete.rda"))
save(computed.net2,file=timestamped.filename("k-median-discrete.rda"))