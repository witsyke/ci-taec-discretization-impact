source("./helper.R")

load("./nets/mehra-complete.rda")

# define sample size
sample.size <- 20000000

# gather observations from original net
sim.data <- rbn(bn, sample.size)

# convert timestamps to continuous
data <- sim.data %>%
  mutate(DateTime = as.numeric(chron(
    dates = paste(Year, Month, Day, sep = "-"),
    times = paste(Hour, "0", "0", sep = ":"),
    format = c('y-m-d', 'h:m:s')
  ))) %>%
  filter(!is.na(DateTime))

data <- data %>%
  select(-Year,-Month,-Day,-Hour)


# save base data set for further use
save(data, file = "base-data.RDS")

# blacklist arcs to DateTime, Region, Zone, Long, Lat, Alt
# and from CVD60
source.nodes <-
  c("DateTime",
    "Region",
    "Zone",
    "Type",
    "Latitude",
    "Altitude",
    "Longitude")

bl.source <- names(data) %>%
  tidyr::crossing(source.nodes) %>%
  rename("from" = ".", "to" = "source.nodes")

bl.sink <- c("CVD60") %>%
  tidyr::crossing(names(data)) %>%
  rename("from" = ".", "to" = "names(data)")

bl.other <- data.frame(from = c("t2m"), to = c("wd"))

bl <- dplyr::union(bl.source, bl.sink)

bl <- dplyr::union(bl, bl.other)

# save complete blacklist
save(bl, file = "blacklist.rds")
