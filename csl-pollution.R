library(bnlearn)
library(Rgraphviz)
library(chron)

load("./nets/mehra-original.rda")

# gather 10'000 observations from original net
sim.data <- rbn(bn, 10000)


# convert timestamps to continuous
# this is very basic, so NaNs can occur (e.g. 2000-02-30)
datetimes <-  vector(length = 10000)
for(i in 1:10000) {
  date <- paste(sim.data$Year[i], sim.data$Month[i], sim.data$Day[i], sep="-")
  time <- paste(sim.data$Hour[i], "0", "0", sep=":")
  datetime <- chron(dates=date, times=time, format=c('y-m-d','h:m:s'))
  datetimes[i] <- as.numeric(datetime)
}

# remove old times
drop <- c("Year","Month", "Day", "Hour")
sim.data <- sim.data[,!(names(sim.data) %in% drop)]

# blacklist arcs to DateTime
bl.from <- names(sim.data)
bl.to <- vector(length=length(bl.from))
for(i in 1:length(bl.from)) {
  bl.to[i] <- "DateTime"
}
bl <- data.frame("from"=bl.from, "to"=bl.to)

# add new time
sim.data$DateTime = datetimes

# pc stable with mixed test
computed.net <- pc.stable(sim.data, test="mi-cg", blacklist=bl)


graphviz.plot(bn)
graphviz.plot(computed.net)