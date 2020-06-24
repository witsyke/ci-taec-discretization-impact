source("./helper.R")

load("./blacklist.rds")
sample <- generate.sample(100000)
# pc stable with mixed test
computed.net <- pc.stable(sample, test="mi-cg", blacklist = base::as.data.frame(bl))

save(computed.net,file=timestamped.filename(prefix = "./nets/", name = "mixed.rds"))

graphviz.plot(bn)
graphviz.plot(computed.net)
