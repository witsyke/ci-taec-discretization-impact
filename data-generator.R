library(bnlearn)
library(chron)
library(tidyverse)
source("./helper.R")

load("./nets/mehra-complete.rda")

# define sample size
sample.size <- 10000000

# gather observations from original net
sim.data <- rbn(bn, sample.size)

# convert timestamps to continuous
clean.sim.data <- sim.data %>%
  mutate(DateTime = chron(dates = paste(Year, Month, Day, sep="-"), times = paste(Hour, "0", "0", sep=":"), format = c('y-m-d','h:m:s'))) %>%
  filter(!is.na(DateTime)) %>%
  select(-Year, -Month, -Day, -Hour)

# save base data set for further use
save(clean.sim.data,file=timestamped.filename("base-data.rds"))

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

# save complete blacklist
save(clean.sim.data,file=timestamped.filename("blacklist.rds"))
