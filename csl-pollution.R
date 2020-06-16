library(bnlearn)
library(Rgraphviz)
library(chron)
library(rbin)
library(Ckmeans.1d.dp)

load("./nets/mehra-complete.rda")
sample.size <- 10000 #50000

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
  k.means <- Ckmedian.1d.dp(values, k=c(5,200))
  clustered.values <- factor(k.means$cluster)
  clustered.data[[column]] <- clustered.values
}

# pc stable with discrete values
computed.net2 <- pc.stable(discrete.sim.data, test="mi-sh", blacklist=bl)
graphviz.plot(computed.net2)
computed.net3 <- pc.stable(clustered.data, test="mi-sh", blacklist=bl)
graphviz.plot(computed.net3)
