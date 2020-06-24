library(bnlearn)
library(Rgraphviz)
library(chron)
library(rbin)
library(Ckmeans.1d.dp)
library(tidyverse)

load("./nets/mehra-complete.rda")
sample.size <- 10000

# gather observations from original net
sim.data <- rbn(bn, sample.size)

# convert timestamps to continuous
# maybe we should just sample new values here

clean.sim.data <- sim.data %>%
  mutate(DateTime = chron(dates = paste(Year, Month, Day, sep="-"), times = paste(Hour, "0", "0", sep=":"), format = c('y-m-d','h:m:s'))) %>%
  filter(!is.na(DateTime)) %>%
  select(-Year, -Month, -Day, -Hour)



# blacklist arcs to DateTime, Region, Zone, Long, Lat, Alt
# and from CVD60
source.nodes <- c("DateTime", "Region", "Zone", "Type", "Latitude", "Altitude", "Longitude")

bl.source <- names(clean.sim.data) %>%
  crossing(source.nodes) %>%
  rename("from" = ".", "to" = "source.nodes")

bl.sink <- c("CVD60") %>%
  crossing(names(clean.sim.data)) %>%
  rename("from" = ".", "to" = "names(clean.sim.data)")

bl <- dplyr::union(bl.source, bl.sink)


# pc stable with mixed test
computed.net <- pc.stable(sim.data, test="mi-cg", blacklist=bl)

graphviz.plot(bn)
graphviz.plot(computed.net)

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


timestamped.filename <- function(name) {
  return(paste(format(Sys.time(), "%Y%m%d-%H%M%S"), name, sep='-'))
}

save(bn,file=timestamped.filename("original.rda"))
save(computed.net, file=timestamped.filename("mixed.rda"))
save(computed.net2,file=timestamped.filename("built-in.discrete.rda"))
save(computed.net2,file=timestamped.filename("k-median-discrete.rda"))