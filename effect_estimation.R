library(bnlearn)
library(Rgraphviz)
library(tidyverse)
library(causaleffect)
library(igraph)
library(Ckmeans.1d.dp)

load("./nets/-20200716-172045-2.5e+07-built-in.discrete-2-bin.rds")
load("./nets/-20200728-113947-1e+06-built-in.discrete-2-bin.rds")
source("./helper.R")
sample <- generate.sample(100000)
discrete.sample.builtin <- discretize(sample, breaks = 2)
bn.disc.fit <- bn.fit(x, data = discrete.sample.builtin, iss = 3, method = "bayes")
graphviz.plot(x)
load("./nets/mehra-complete.rds")
x <- set.arc(computed.net1, from="co", to="so2") #, check.cycles = FALSE)
x <- set.arc(x, from="co", to="pm10")
x <- set.arc(x, from="ssr", to="tp", check.cycles = FALSE)
x <- set.arc(x, from="ws", to="blh", check.cycles = FALSE)
x <- set.arc(x, from="ws", to="tp", check.cycles = FALSE)
#set.arc(x, from="co", to="pm2.5", check.cycles = FALSE)
x <- drop.arc(x, from = "pm10", to="so2")
x <- drop.arc(x, from = "tp", to="t2m")
x <- drop.arc(x, from = "tp", to="blh")
x <- drop.arc(x, from = "o3", to="ws")
#?set.arc
directed(x)
acyclic(x)

library(DiagrammeRsvg)
library(DiagrammeR)
load("./nets/-20200728-112020-100-built-in.discrete-2-bin.rds")
bn.2.bin.100 <- computed.net1
graphviz.plot(bn.2.bin.100)

load("./nets/-20200728-204320-5e+06-built-in.discrete-2-bin.rds")
bn.2.bin.5m <- computed.net1
plot <- graphviz.plot(bn.2.bin.5m)

load("./nets/-20200729-013003-5e+06-built-in.discrete-3-bin.rds")
bn.3.bin.5m <- computed.net1
plot <- graphviz.plot(bn.3.bin.5m)

load("./nets/-20200728-112440-1e+05-built-in.discrete-3-bin.rds")
bn.3.bin.100k <- computed.net1
plot <- graphviz.plot(bn.3.bin.100k)

load("./nets/-20200729-084940-5e+06-built-in.discrete-10-bin.rds")
bn.10.bin.5m <- computed.net1
plot <- graphviz.plot(bn.10.bin.5m)

load("./nets/-20200729-063308-5e+06-built-in.discrete-5-bin.rds")
bn.5.bin.5m <- computed.net1
plot <- graphviz.plot(bn.5.bin.5m)

load("./nets/-20200729-042341-5e+06-built-in.discrete-4-bin.rds")
bn.4.bin.5m <- computed.net1
plot <- graphviz.plot(bn.4.bin.5m)

graphviz.compare(ground.truth, bn.4.bin.5m)
hamming(ground.truth, bn.2.bin.5m)
hamming(ground.truth, bn.3.bin.5m)
hamming(ground.truth, bn.4.bin.5m)
hamming(ground.truth, bn.5.bin.5m)
hamming(ground.truth, bn.10.bin.5m)

shd(ground.truth, bn.2.bin.5m)
shd(ground.truth, bn.3.bin.5m)
shd(ground.truth, bn.4.bin.5m)
shd(ground.truth, bn.5.bin.5m)
shd(ground.truth, bn.10.bin.5m)

load("./nets/mehra-complete.rda")
graphviz.plot(bn)
bn.sample <- rbn(bn, 10000)
t <-  theme_bw() + theme(#panel.background = element_rect(fill = "transparent"),
  #plot.background = element_rect(fill = "transparent", color = NA),
  #legend.background = element_rect(fill = "transparent", color = NA),
  #legend.box.background = element_rect(fill = "transparent", color = NA),
  legend.position = "none",
  axis.text.x = element_text(size = 24),
  axis.text.y = element_text(size = 24),
  text = element_text(size = 28),
  #axis.text.x=element_blank(),
  #axis.ticks.x=element_blank(),
  #line = element_line(colour = "#10e7d9"),
  #axis.text.y = element_text(colour="white", size = 18),
  #text = element_text(colour="white", size = 16),
  )

plot1 <- ggplot(data.frame(bn.sample$so2), aes(bn.sample$so2)) + geom_density() + xlab(expression("Concentration of SO"[2])) + ylim(0,85) + t
plot2 <- ggplot(data.frame(bn.sample$so2), aes(bn.sample$so2)) + geom_density() + xlab(expression("Concentration of SO"[2])) + xlim(13.76,13.86)+ ylim(0,85)  + t

  
disc.sample.quantile <- discretize(bn.sample, breaks = 3)
disc.sample.kM <- bn.sample %>%
  base::sapply(function(x)
    if (!is.factor(x)) {
      meds <- Ckmedian.1d.dp(x, k = c(3, 3))
      trans <- meds$cluster %>%
        base::sapply(function(y)
          meds$centers[[y]])
      factor(trans)
    } else
      x) %>%
  base::as.data.frame()

b <- disc.sample.quantile %>%
  count(so2)

(plot1 <- disc.sample.quantile %>%
           count(so2) %>%
ggplot(aes(x=so2, y=n, fill=so2)) + geom_col() + ylab("count") +  xlab(expression("Concentration of SO"[2])) + ylim(0,10000)  + scale_x_discrete(labels = function(labels) {
  sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 == 1, '', '\n'), labels[i]))
}) + t + scale_fill_manual(values = grey_pal()(3)))

(plot2 <- (disc.sample.kM %>%
           ggplot(aes(x=so2, fill=so2)) + geom_bar() + ylab("count") +  xlab(expression("Concentration of SO"[2])) + scale_x_discrete(labels = function(labels) {
             sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 == 1, '', '\n'), sprintf("%.2f", as.numeric(labels[i]))))})  + ylim(0,10000) + t + scale_fill_manual(values = grey_pal()(3))))


ggsave(plot2, filename = "disc-km.pdf", device = "pdf", width=7, height=5)
ggsave(plot1, filename = "disc-q.pdf", device = "pdf", width=7, height=5)



graphviz.compare(bn.3.bin.5m, bn.2.bin.5m)
hamming(bn.3.bin.5m, bn.2.bin.5m)

graphviz.compare(x, bn.net(bn))
compare(bn.net(bn), x)
hamming(bn.net(bn), x)

new.model.string <- "[Region][Zone][Type][DateTime][Season][Latitude][Longitude][Altitude][wd|Region:DateTime][CVD60|Region:DateTime:Season][co|Zone:Type:DateTime][pm10|Zone:Type:DateTime][t2m|DateTime:Latitude:wd][so2|Zone:Type:DateTime:co][pm2.5|Region:Type:DateTime:pm10][ws|DateTime:Latitude:Longitude:t2m:wd][blh|Region:DateTime:Longitude:t2m:ws:wd][o3|Zone:Type:DateTime:blh][no2|Zone:Type:DateTime:Altitude:blh:o3:co:pm10][ssr|DateTime:Latitude:Longitude:Altitude:t2m:ws:wd:blh:no2:o3:so2:co:pm10][tp|DateTime:ws:ssr]"
ground.truth <- model2network(new.model.string)
graphviz.plot(ground.truth)
graphviz.compare(ground.truth, bn.2.bin.5m)
graphviz.compare(ground.truth, bn.3.bin.5m)
graphviz.compare(ground.truth, bn.3.bin.100k)

bnlearn::compare(ground.truth, bn.3.bin.5m)


model.string <- modelstring(bn)
model.string

load("./nets/-20200728-113947-1e+06-built-in.discrete-2-bin.rds")
source("./helper.R")
sample <- generate.sample(1000)
discrete.sample.builtin <- discretize(sample, breaks = 2)
bn.disc.fit <- bn.fit(x, data = discrete.sample.builtin, iss = 3, method = "bayes")
graphviz.plot(x)
#load("../nets/mehra-original.rds")
x <- set.arc(computed.net1, from="co", to="so2") #, check.cycles = FALSE)
x <- set.arc(x, from="co", to="pm10")
x <- set.arc(x, from="ssr", to="tp", check.cycles = FALSE)
x <- set.arc(x, from="ws", to="blh", check.cycles = FALSE)
x <- set.arc(x, from="ws", to="tp", check.cycles = FALSE)
#set.arc(x, from="co", to="pm2.5", check.cycles = FALSE)
x <- drop.arc(x, from = "pm10", to="so2")
x <- drop.arc(x, from = "tp", to="t2m")
x <- drop.arc(x, from = "tp", to="blh")
x <- drop.arc(x, from = "o3", to="ws")
#?set.arc
directed(x)
acyclic(x)


blub <- "[-2.07,1.44]"
cpquery(bn.disc.fit, event = (so2=="2"), evidence = ((co == "1") & (Zone == "Greater London Urban Area") & (Type == "Background Urban") & (Year == "1982")))
cpquery(bn.disc.fit, event = (Zone == "Greater London Urban Area"), evidence = TRUE)
cpquery(bn.disc.fit, event = (so2=="[-4.62e+05,13.8]"), evidence = (co == "[-2.07,1.44]"))
cpquery(bn.disc.fit, event = (so2=="[-4.62e+05,13.8]"), evidence = (co == blub))
cpquery(bn.disc.fit, event = (so2=="[-4.62e+05,13.8]"), evidence = ((co != "[-2.07,1.44]")))
cpquery(bn.disc.fit, event = (so2!="[-4.62e+05,13.8]"), evidence = ((co == "[-2.07,1.44]")))
cpquery(bn.disc.fit, event = (so2!="[-4.62e+05,13.8]"), evidence = ((co != "[-2.07,1.44]")))
cpquery(bn.disc.fit, event = (so2=="[-4.62e+05,13.8]"), evidence = TRUE)
sum(discrete.sample.builtin$Zone == "Greater London Urban Area")/nrow(discrete.sample.builtin) 

adj.matrix.from.bn <- amat(bn)
igraph.from.bnmatrix<- graph_from_adjacency_matrix(adj.matrix.from.bn)

adj.matrix.from.learned <- amat(x)
igraph.from.learned<- graph_from_adjacency_matrix(adj.matrix.from.learned)

y <- difference(igraph.from.bnmatrix, igraph.from.learned)

tkplot(y, vertex.size=35, vertex.label.family="sans", vertex.color="white")

sample.sizes <- c("100", "1000", "10000", "1e+05", "1e+06", "5e+06", "1e+07")
bins <- c(2,3,4,5,7,10)
disc.methods <- c("built-in-discrete", "k-median")


loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

calculate.hd <- function(row, gt) {
  net <- loadRData(row["filename"])
  hamming(cpdag(net), cpdag(ground.truth))
}

calculate.shd  <- function(row, gt) {
  net <- loadRData(row["filename"])
  shd(cpdag(net), ground.truth)
}

calculate.f1  <- function(row, gt) {
  net <- loadRData(row["filename"])
  conf.matrix <- bnlearn::compare(ground.truth, net)
  conf.matrix$tp / (conf.matrix$tp + 0.5 * (conf.matrix$fp + conf.matrix$fn))
}

calculate.fm <- function(row, gt) {
  net <- loadRData(row["filename"])
  conf.matrix <- bnlearn::compare(ground.truth, net)
  tp <- conf.matrix$tp
  fp <- conf.matrix$fp
  fn <- conf.matrix$fn
  sqrt((tp/(tp+fp))*(tp/(tp+fn)))
}

calculate.mcc <- function(row, gt) {
  net <- loadRData(row["filename"])
  conf.matrix <- bnlearn::compare(ground.truth, net)
  tp <- conf.matrix$tp
  fp <- conf.matrix$fp
  fn <- conf.matrix$fn
  edge.sum <- sum(seq(1, length(net$nodes) - 1))
  tn <- edge.sum - (tp + fp + fn)
  (tp*tn - fp*fn) / sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))
}

calculate.mcc2 <- function(row, gt) {
  net <- loadRData(row["filename"])
  conf.matrix <- bnlearn::compare(ground.truth, net)
  tp <- conf.matrix$tp
  fp <- conf.matrix$fp
  fn <- conf.matrix$fn
  edge.sum <- 2 * sum(seq(1, length(net$nodes) - 1))
  tn <- edge.sum - (tp + fp + fn)
  (tp*tn - fp*fn) / sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))
}

calculate.mcc3 <- function(row, gt) {
  net <- loadRData(row["filename"])
  conf.matrix <- bnlearn::compare(ground.truth, net)
  tp <- conf.matrix$tp
  fp <- conf.matrix$fp
  fn <- conf.matrix$fn
  edge.sum <- length(net$nodes)*length(net$nodes)-length(net$nodes)
  tn <- edge.sum - (tp + fp + fn)
  (tp*tn - fp*fn) / sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))
}

has.so2_co <- function(row) {
  net <- loadRData(row["filename"])
  "so2" %in% bnlearn::children(net, "co")
}


filenames <- sample.sizes %>%
  tidyr::crossing(bins) %>%
  tidyr::crossing(disc.methods) %>%
  rename(n = ".") %>%
  mutate(filename=paste(paste("./nets/paper/", n, sep=""),"datetime",disc.methods, bins, "bin.rds", sep = "-"))


filenames$hamming <- apply(filenames, 1, calculate.hd, gt=ground.truth)
filenames$shd <- apply(filenames, 1, calculate.shd, gt=ground.truth)
filenames$f1 <- apply(filenames, 1, calculate.f1, gt=ground.truth)
filenames$fm <- apply(filenames, 1, calculate.fm, gt=ground.truth)
filenames$mcc <- apply(filenames, 1, calculate.mcc2, gt=ground.truth)
filenames$mc2 <- apply(filenames, 1, calculate.mcc3, gt=ground.truth)
#filenames$has.so2_co <- apply(filenames, 1, has.so2_co)
#filenames$mcc2 <- apply(filenames, 1, calculate.mcc2, gt=ground.truth)

csl.aggregated <- filenames %>%
  dplyr::group_by(disc.methods, n) %>%
  summarise(mean_hd = mean(hamming)) #%>%
  #summarise(mean_shd = mean(shd)) %>%
  #summarise(mean_f1 = mean(f1)) %>%
  #summarise(mean_mcc = mean(mcc))


csl.metrics <- filenames %>% 
  select(-filename)

csl.2 <- filenames %>%
  dplyr::filter(bins == 2)




load("./nets/foo/5e+06-built-in.discrete-3-bin.rds")
bn.3.bin.5m <- computed.net1
graphviz.compare(ground.truth, bn.3.bin.5m)

load("./nets/foo/5e+06-built-in.discrete-2-bin.rds")
bn.2.bin.5m <- computed.net1
graphviz.compare(ground.truth, bn.2.bin.5m)



load("./nets/foo3/12500000-legacy-built-in-discrete-2-bin.rds")
bn.2.cluster.12.5m <- computed.net1
graphviz.compare(bn.net(bn), bn.2.cluster.12.5m)

load("./nets/foo3/12500000-legacy-k-median-4-clusters.rds")
bn.4.cluster.12.5m <- computed.net2
graphviz.compare(bn.net(bn), bn.4.cluster.12.5m)

directed(bn.4.cluster.5m)
acyclic(bn.4.cluster.5m)

load("./nets/foo3/bin1e+07-datetime-built-in-discrete-3-bin.rds")
bn.2.bin.105m <- computed.net1
graphviz.compare(ground.truth, bn.2.bin.105m)

bnlearn::compare(cpdag(bn.net(bn)), cpdag(bn.4.cluster.12.5m))
bnlearn::compare(bn.net(bn), bn.4.cluster.12.5m)
graphviz.compare(ground.truth, cpdag(ground.truth))
bnlearn::compare(ground.truth, ground.truth)

graphviz.plot(ground.truth)
bl <- set.arc(ground.truth, from = "CVD60", to = "Season")
graphviz.plot(bl)
graphviz.compare(ground.truth, bl)
bnlearn::compare(ground.truth, bl)

load("./nets/paper/1e+05-datetime-built-in-discrete-2-bin.rds")
bn.2.bin.100k <- computed.net1
graphviz.compare(ground.truth, bn.2.bin.100k)

load("./nets/foo3/1e+07-datetime-k-median-4-clusters.rds")
bn.4.cluster.10m <- computed.net2
graphviz.compare(ground.truth, bn.4.cluster.10m)

load("./nets/paper/1e+07-datetime-built-in-discrete-3-bin.rds")
bn.3.bin.10m <- computed.net1
graphviz.compare(ground.truth, bn.3.bin.10m)
undirected.arcs(bn.3.bin.10m)

bn.3.bin.10m.directed <- drop.arc(bn.3.bin.10m, from="t2m", to="ws")
bn.3.bin.10m.directed <- set.arc(bn.3.bin.10m.directed, from="co", to="so2")
bn.3.bin.10m.directed <- set.arc(bn.3.bin.10m.directed, from="so2", to="pm10")
bn.3.bin.10m.directed <- set.arc(bn.3.bin.10m.directed, from="co", to="pm10")
graphviz.compare(ground.truth, bn.3.bin.10m.directed)
undirected.arcs(bn.3.bin.10m.directed)
directed.arcs()

igraph.3.bin.10m<- graph_from_adjacency_matrix(amat(bn.3.bin.10m.directed))
causal.effect(y = "so2", x = "co", z = NULL, G = igraph.3.bin.10m, expr = T)

source("./helper.R")
sample.10m <- generate.sample(10000000)
sample.10m.3.bin <- discretize(sample.10m, breaks = 3)

causal.effect.probabilites.so2.co <- function(data) {
  
  # select relevant columns
  data <- data %>%
    select(so2, co, Type, Zone, DateTime) 
  
  # calculate probabilities for realisations of Zone 
  data <- data %>%
    add_count(Zone) %>%
    mutate(prob_zone = n / n()) %>%
    select(-n) 
  
  # calculate probabilities for realisations of Type 
  data <- data %>%
    add_count(Type) %>%
    mutate(prob_type = n / n()) %>%
    select(-n) 
  
  # calculate probabilities for realisations of Year 
  data <- data %>%
    add_count(DateTime) %>%
    mutate(prob_datetime = n / n()) %>%
    select(-n) 
  
  # calculate conditional probability for so2 by Zone, Type, Year, co
  data <- data %>%
    group_by(Zone, Type, DateTime, co) %>%
    add_count(so2) %>%
    group_by(Zone, Type, DateTime, co) %>%
    mutate(so2_cond_prob = n / n()) %>%
    select(-n) 
  
  # calculate causal effect and add probability for co to table
  data <- data %>%
    ungroup() %>%
    distinct() %>%
    mutate(total_prob = prob_zone * prob_type * prob_datetime * so2_cond_prob) %>%
    group_by(so2, co) %>%
    summarise("Pso2doco" = sum(total_prob)) %>%
    inner_join(data %>%
                 group_by(co) %>%
                 count(co) %>%
                 mutate("P(co)" = n / nrow(data)) %>%
                 select(-n))
  
}

causal.effect.co.so2 <- causal.effect.probabilites.so2.co(sample.10m.3.bin)

causal.effect.co.so2 %>%
  group_by(co) %>%
  summarise(s = sum(Pso2doco))



#cdag <- drop.arc(bn.2.bin.100k, from="co", to="pm10")
igraph.cdag<- graph_from_adjacency_matrix(amat(bn.2.bin.100k))

causal.effect(y = "pm10", x = "pm2.5", z = NULL, G = igraph.cdag, expr = T)

causal.effect.probabilites.pm10.pm2.5 <- function(data) {
  
  # select relevant columns
  data <- data %>%
    select(pm10, Zone, Type, DateTime, pm2.5) 
  
  # calculate probabilities for realisations of Zone 
  data <- data %>%
    add_count(Zone) %>%
    mutate(prob_zone = n / n()) %>%
    select(-n) 
  
  # calculate probabilities for realisations of Type 
  data <- data %>%
    add_count(Type) %>%
    mutate(prob_type = n / n()) %>%
    select(-n) 
  
  # calculate probabilities for realisations of Year 
  data <- data %>%
    add_count(DateTime) %>%
    mutate(prob_datetime = n / n()) %>%
    select(-n) 
  
  # calculate conditional probability for so2 by Zone, Type, Year, co
  data <- data %>%
    group_by(Zone, Type, DateTime, pm2.5) %>%
    add_count(pm10) %>%
    group_by(Zone, Type, DateTime, pm2.5) %>%
    mutate(pm10_cond_prob = n / n()) %>%
    select(-n) 
  
  # calculate causal effect and add probability for co to table
  data <- data %>%
    ungroup() %>%
    distinct() %>%
    mutate(total_prob = prob_zone * prob_type * prob_datetime * pm10_cond_prob) %>%
    group_by(pm10, pm2.5) %>%
    summarise(Ppm10 = sum(total_prob)) %>%
    inner_join(data %>%
                 group_by(pm2.5) %>%
                 count(pm2.5) %>%
                 mutate("P(pm2.5)" = n / nrow(data)) %>%
                 select(-n))
}

causal.effect.probabilites.pm10.pm2.5(bn.sample)

source("./helper.R")
sample.100k <- generate.sample(100000)
sample.100k.2.cin <- discretize(sample.100k, breaks = 2)

causal.effects <- causal.effect.probabilites.pm10.pm2.5(sample.100k.2.cin)

causal.effects %>%
  group_by(pm2.5) %>%
  summarise(s = sum(Ppm10))

igraph.cdag<- graph_from_adjacency_matrix(amat(bn.2.bin.100k))

causal.effect(y = "pm10", x = "pm2.5", z = NULL, G = igraph.cdag, expr = T)

graphviz.compare(ground.truth, bn.4.cluster.5m)

causal.effect.probabilites.pm10.pm2.5 <- function(data) {
  
  # select relevant columns
  data <- data %>%
    select(pm10, Zone, Type, DateTime, pm2.5) 
  
  # calculate probabilities for realisations of Zone 
  data <- data %>%
    add_count(Zone) %>%
    mutate(prob_zone = n / n()) %>%
    select(-n) 
  
  # calculate probabilities for realisations of Type 
  data <- data %>%
    add_count(Type) %>%
    mutate(prob_type = n / n()) %>%
    select(-n) 
  
  # calculate probabilities for realisations of Year 
  data <- data %>%
    add_count(DateTime) %>%
    mutate(prob_datetime = n / n()) %>%
    select(-n) 
  
  # calculate conditional probability for so2 by Zone, Type, Year, co
  data <- data %>%
    group_by(Zone, Type, DateTime, pm2.5) %>%
    add_count(pm10) %>%
    group_by(Zone, Type, DateTime, pm2.5) %>%
    mutate(pm10_cond_prob = n / n()) %>%
    select(-n) 
  
  # calculate causal effect and add probability for co to table
  data <- data %>%
    ungroup() %>%
    distinct() %>%
    mutate(total_prob = prob_zone * prob_type * prob_datetime * pm10_cond_prob) %>%
    group_by(pm10, pm2.5) %>%
    summarise(Ppm10 = sum(total_prob)) %>%
    inner_join(data %>%
                 group_by(pm2.5) %>%
                 count(pm2.5) %>%
                 mutate("P(pm2.5)" = n / nrow(data)) %>%
                 select(-n))
}

causal.effect.probabilites.pm10.pm2.5(bn.sample)

sample.100k <- generate.sample(100000)
sample.100k.2.cin <- discretize(sample.100k, breaks = 2)

norm.MI.pm25.pm10 <- function(data){
  1- MI.pm25_pm10(data)/JointEntropy.pm25_pm10(data)
}

norm.MI.pm25.pm10(sample.100k.2.cin)
MI.pm25_pm10(sample.100k.2.cin)

MI.pm25_pm10 <- function(data){
  result <- 0
  for (pm10 in unique(data$pm10)){
    for (pm25_1 in unique(data$pm2.5)){
      joint <- sum(data$pm10 == pm10 & data$pm2.5 == pm25_1)/nrow(data)
      log_counter <-  sum(data$pm10 == pm10 & data$pm2.5 == pm25_1)/sum(data$pm2.5 == pm25_1)
      log_denom <- 0
      for(pm25_2 in unique(data$pm2.5)){
        temp_inner <- sum(data$pm10 == pm10 & data$pm2.5 == pm25_2)/nrow(data)
        log_denom <- log_denom + temp_inner
      }
      #print(log_denom)
      temp <- joint*log2(log_counter/log_denom)
      result <- result + if_else(is.nan(temp), 0, temp)
    }
  }
  result
}

JointEntropy.pm25_pm10 <- function(data){
  result <- 0
  for (pm10 in unique(data$pm10)){
    for (pm25_1 in unique(data$pm2.5)){
      joint <- sum(data$pm10 == pm10 & data$pm2.5 == pm25_1)/nrow(data)
      temp <- joint*log2(joint)
      result <- result - if_else(is.nan(temp), 0, temp)
    }
  }
  result
}

source("./helper.R")
sample.100k <- generate.sample(100000)
sample.100k.2.cin <- discretize(sample.100k, breaks = 2)

causal.effects <- causal.effect.probabilites.pm10.pm2.5(sample.100k.2.cin)

causal.effects %>%
  group_by(pm2.5) %>%
  summarise(s = sum(Ppm10))

bn.2.bin.fitted <- bn.fit(bn.2.bin.100k, sample.100k.2.cin)


cpquery(bn.2.bin.fitted, event = (Zone == "Greater London Urban Area"), evidence = TRUE)

do.pm25.1 <- causal.effects %>%
  filter(pm2.5 == "(18.6,32.5]") 

do.pm25.2 <- causal.effects %>%
  filter(pm2.5 == "[-4.75,18.6]")

(plot1<-ggplot(do.pm25.1, aes(x=pm10, y = Ppm10, fill=pm10)) + geom_bar(stat='identity', position = position_dodge(), show.legend = F)  + xlab(expression("PM"[10])) + ylab("P") + ylim(0,0.6) + t + scale_fill_manual(values = grey_pal()(2)))

(plot2<-ggplot(do.pm25.2, aes(x=pm10, y = Ppm10, fill=pm10)) + geom_bar(stat='identity', position = position_dodge()) + xlab(expression("PM"[10])) + ylab("P") + ylim(0,0.6) + t + scale_fill_manual(values = grey_pal()(2)))

library(scales)

ggsave(plot2, filename = "do-pm25.pdf", device = "pdf", width=7, height=5)
ggsave(plot1, filename = "do-pm25-1.pdf", device = "pdf", width=7, height=5)




easy_string = "[Zone][Type][DateTime][Region][pm10|pm2.5:Zone:Type:DateTime][pm2.5|Type:DateTime:Region]"
wasy.bn <- model2network(easy_string)
graphviz.plot(wasy.bn)

easy_string2 = "[Zone][Type][DateTime][pm10|pm2.5:Zone:Type:DateTime][pm2.5]"
wasy.bn2 <- model2network(easy_string2)
graphviz.plot(wasy.bn2)

load("./base-data-datetime.RDS")


load("./nets/paper/5e+06-datetime-k-median-4-bin.rds")
bn.4.cluster.5m <- computed.net2
graphviz.compare(ground.truth, bn.4.cluster.5m)

bn.4.cluster <- drop.arc(bn.4.cluster.5m, from="blh", to="ws")
bn.4.cluster <- drop.arc(bn.4.cluster, from="ssr", to="tp")
graphviz.compare(ground.truth, bn.4.cluster)


sample.5m <- generate.sample(5000000)

igraph.cdag.4<- graph_from_adjacency_matrix(amat(bn.4.cluster))
causal.effect(y = "t2m", x = "wd", z = NULL, G = igraph.cdag.4, expr = T)

sample.5m.4.cluster <- sample.5m  %>%
  base::sapply(function(x)
    if (!is.factor(x)) {
      meds <- Ckmedian.1d.dp(x, k = c(4, 4))
      trans <- meds$cluster %>%
        base::sapply(function(y)
          meds$centers[[y]])
      factor(trans)
    } else
      x) %>%
  base::as.data.frame()

causal.effect.probabilites.t2m.wd <- function(data) {
  
  # select relevant columns
  data <- data %>%
    select(wd, t2m, Latitude, DateTime) 
  
  # calculate probabilities for realisations of Zone 
  data <- data %>%
    add_count(Latitude) %>%
    mutate(prob_lat = n / n()) %>%
    select(-n) 

  # calculate probabilities for realisations of Year 
  data <- data %>%
    add_count(DateTime) %>%
    mutate(prob_datetime = n / n()) %>%
    select(-n) 
  
  # calculate conditional probability for so2 by Zone, Type, Year, co
  data <- data %>%
    group_by(wd, Latitude, DateTime) %>%
    add_count(t2m) %>%
    group_by(wd, Latitude, DateTime) %>%
    mutate(t2m_cond_prob = n / n()) %>%
    select(-n) 
  
  # calculate causal effect and add probability for co to table
  data <- data %>%
    ungroup() %>%
    distinct() %>%
    mutate(total_prob = prob_lat * prob_datetime * t2m_cond_prob) %>%
    group_by(wd, t2m) %>%
    summarise(Pt2m = sum(total_prob)) %>%
    inner_join(data %>%
                 group_by(wd) %>%
                 count(wd) %>%
                 mutate("P(wd)" = n / nrow(data)) %>%
                 select(-n))
}

causal.effect.prob.t2m.wd <- causal.effect.probabilites.t2m.wd(sample.5m.4.cluster)

causal.effect.prob.t2m.wd %>%
  group_by(wd) %>%
  summarise(s = sum(Pt2m))


norm.MI.wd.t2m <- function(data){
  1- MI.wd_t2m(data)/JointEntropy.wd_t2m(data)
}

norm.MI.wd.t2m(sample.5m.4.cluster)
MI.wd_t2m(sample.5m.4.cluster)

MI.wd_t2m <- function(data){
  result <- 0
  for (t2m in unique(data$t2m)){
    for (wd_1 in unique(data$wd)){
      joint <- sum(data$t2m == t2m & data$wd == wd_1)/nrow(data)
      log_counter <-  sum(data$t2m == t2m & data$wd == wd_1)/sum(data$wd == wd_1)
      log_denom <- 0
      for(wd_2 in unique(data$wd)){
        temp_inner <- sum(data$t2m == t2m & data$wd == wd_2)/nrow(data)
        log_denom <- log_denom + temp_inner
      }
      #print(log_denom)
      temp <- joint*log2(log_counter/log_denom)
      result <- result + if_else(is.nan(temp), 0, temp)
    }
  }
  result
}

JointEntropy.wd_t2m <- function(data){
  result <- 0
  for (t2m in unique(data$t2m)){
    for (wd_1 in unique(data$wd)){
      joint <- sum(data$t2m == t2m & data$wd == wd_1)/nrow(data)
      temp <- joint*log2(joint)
      result <- result - if_else(is.nan(temp), 0, temp)
    }
  }
  result
}

causal.effect.prob.t2m.wd %>%
  select(t2m) %>%
  distinct()

plot <- (sample.5m.4.cluster %>%
           ggplot(aes(x=t2m)) + geom_bar() + ylab("count") +  xlab("T2M") + scale_x_discrete(labels = function(x) sprintf("%.2f", as.numeric(x)))) #+ ylim(0,10000) + t)

wd = c(65, 65, 65, 65, 167, 167, 167, 167, 235, 235, 235, 235, 296, 296, 296, 296)
t2m = as.factor(c(275.9, 280.6, 285.2, 290.0, 275.9, 280.6, 285.2, 290.0, 275.9, 280.6, 285.2, 290.0, 275.9, 280.6, 285.2, 290.0))
p = c(0.268, 0.234, 0.259, 0.239, 0.228, 0.295, 0.277, 0.200, 0.201, 0.310, 0.287, 0.202, 0.204, 0.297, 0.291, 0.208)


probs.wd.t2m <- data.frame(wd = wd, t2m = t2m, p = p)

t2m.do.wd65 <- probs.wd.t2m %>% filter(wd == 65)
t2m.do.wd167 <- probs.wd.t2m %>% filter(wd == 167)
t2m.do.wd235 <- probs.wd.t2m %>% filter(wd == 235)
t2m.do.wd296 <- probs.wd.t2m %>% filter(wd == 296)

(plot1 <- (t2m.do.wd65 %>%
           ggplot(aes(x=t2m, y=p, fill=t2m)) + geom_bar(stat = 'identity') + ylab("P") +  xlab("T2M [K]") + ylim(0,0.35) + t + scale_fill_manual(values = grey_pal()(4))))
(plot2 <- (t2m.do.wd167 %>%
            ggplot(aes(x=t2m, y=p, fill=t2m)) + geom_bar(stat = 'identity') + ylab("P") +  xlab("T2M [K]") + ylim(0,0.35) + t + scale_fill_manual(values = grey_pal()(4))))
(plot3 <- (t2m.do.wd235 %>%
            ggplot(aes(x=t2m, y=p, fill=t2m)) + geom_bar(stat = 'identity') + ylab("P") +  xlab("T2M [K]") + ylim(0,0.35) + t + scale_fill_manual(values = grey_pal()(4))))
(plot4 <- (t2m.do.wd296 %>%
            ggplot(aes(x=t2m, y=p, fill=t2m)) + geom_bar(stat = 'identity') + ylab("P") +  xlab("T2M [K]") + ylim(0,0.35) + t + scale_fill_manual(values = grey_pal()(4))))


ggsave(plot1, filename = "t2m-do-wd-1.pdf", device = "pdf", width=7, height=5)
ggsave(plot2, filename = "t2m-do-wd-2.pdf", device = "pdf", width=7, height=5)
ggsave(plot3, filename = "t2m-do-wd-3.pdf", device = "pdf", width=7, height=5)
ggsave(plot4, filename = "t2m-do-wd-4.pdf", device = "pdf", width=7, height=5)


