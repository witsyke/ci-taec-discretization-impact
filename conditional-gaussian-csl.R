source("./helper.R")

load("./blacklist.rds")

sample.size <- 10000000
sample <- generate.sample(sample.size)
# pc stable with mixed test
computed.net <- pc.stable(sample, test="mi-cg", blacklist = base::as.data.frame(bl))

save(computed.net,file=timestamped.filename(prefix = "./nets/", name = paste(sample.size, "mixed.rds", sep='-')))

graphviz.plot(bn)
graphviz.plot(computed.net)
