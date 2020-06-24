source("./helper.R")

load("./blacklist.rds")
sample <- generate.sample(10000)
# pc stable with mixed test
computed.net <- pc.stable(sample, test="mi-cg", blacklist = base::as.data.frame(bl))

graphviz.plot(bn)
graphviz.plot(computed.net)


library(causaleffect)

causal.effect("o3", "pm2.5", z = NULL, computed.net, expr = TRUE)
