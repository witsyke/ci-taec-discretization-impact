library(bnlearn)
library(Rgraphviz)
library(chron)
library(rbin)
library(Ckmeans.1d.dp)

load("./nets/mehra-complete.rda")
sample.size <- 10000

# gather observations from original net
sim.data <- rbn(bn, sample.size)

# convert timestamps to continuous
# this is very basic to not have NAs
# maybe we should just sample new values here
datetimes <-  vector(length=sample.size)
for(i in 1:sample.size) {
  date <- paste(sim.data$Year[i], sim.data$Month[i], sim.data$Day[i], sep="-")
  time <- paste(sim.data$Hour[i], "0", "0", sep=":")
  datetime <- chron(dates=date, times=time, format=c('y-m-d','h:m:s'))
  day <- strtoi(sim.data$Day[i])
  while(is.na(datetime)) {
    day <- day - 1
    date <- paste(sim.data$Year[i], sim.data$Month[i], day, sep="-")
    datetime <- chron(dates=date, times=time, format=c('y-m-d','h:m:s'))
  }
  datetimes[i] <- as.numeric(datetime)
}

# remove old times
drop <- c("Year","Month", "Day", "Hour")
sim.data <- sim.data[,!(names(sim.data) %in% drop)]

# add new time
sim.data$DateTime = datetimes

# blacklist arcs to DateTime, Region, Zone, Long, Lat, Alt
# and from CVD60
source.nodes <- c("DateTime", "Region", "Zone", "Type", "Latitude", "Altitude")
bl.source.from <- vector(length=length(source.nodes) * length(names(sim.data)))
bl.source.to <- vector(length=length(bl.source.from))
for (i in 1:length(source.nodes)) {
  for(j in 1:length(names(sim.data))) {
    bl.source.to[(i - 1) * length(names(sim.data)) + j] <- source.nodes[i]
    bl.source.from[(i - 1) * length(names(sim.data)) + j] <- names(sim.data)[j]
  }
}

bl.sink.from <- vector(length=length(names(sim.data)))
bl.sink.to <- vector(length=length(bl.sink.from))

for(i in 1:length(names(sim.data))) {
  bl.sink.from[i] <- "CVD60"
  bl.sink.to[i] <- names(sim.data)[i]
}

bl <- data.frame("from"=c(bl.source.from, bl.sink.from), "to"=c(bl.source.to, bl.sink.to))

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
  k.means <- Ckmedian.1d.dp(values, k=c(10,100))
  clustered.values <- factor(k.means$cluster)
  clustered.data[[column]] <- clustered.values
}

# pc stable with discrete values
computed.net2 <- pc.stable(discrete.sim.data, test="mi-sh", blacklist=bl)
graphviz.plot(computed.net2)
computed.net3 <- pc.stable(clustered.data, test="mi-sh", blacklist=bl)
graphviz.plot(computed.net3)

