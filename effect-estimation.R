library(bnlearn)
library(Rgraphviz)
library(tidyverse)
library(causaleffect)
library(igraph)
library(Ckmeans.1d.dp)
library(scales)

source("./helper.R")
load("./nets/mehra-complete.rda")

PLOT_WIDTH <- 7
PLOT_HEIGHT <- 5

t <-  theme_bw() + 
      theme(legend.position = "none",
            axis.text.x = element_text(size = 24),
            axis.text.y = element_text(size = 24),
            text = element_text(size = 28))


# Generate density plots of SO2 concentration
dp.sample <- rbn(bn, 10000)
dp.plot <- ggplot(data.frame(dp.sample$so2), aes(dp.sample$so2)) + 
            geom_density() + 
            xlab(expression("Concentration of SO"[2])) + 
            ylim(0, 85) + 
            t
dp.plot.crop <- ggplot(data.frame(dp.sample$so2), aes(dp.sample$so2)) + 
                  geom_density() + 
                  xlab(expression("Concentration of SO"[2])) + 
                  xlim(13.76, 13.86) + 
                  ylim(0, 85) + 
                  t

ggsave(dp.plot,
       filename = "density_so2.pdf",
       device = "pdf",
       width = PLOT_WIDTH,
       height = PLOT_HEIGHT)
ggsave(dp.plot.crop,
       filename = "density_so2_crop.pdf",
       device = "pdf",
       width = PLOT_WIDTH,
       height = PLOT_HEIGHT)


# Disrectize and generate histograms of SO2 concentration
disc.sample.quantile <- discretize(dp.sample, breaks = 3)
disc.sample.kM <- dp.sample %>%
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


histogram.so2.quantile <- disc.sample.quantile %>%
  count(so2) %>%
  ggplot(aes(x = so2, y = n, fill = so2)) + 
    geom_col() + ylab("count") +  
    xlab(expression("Concentration of SO"[2])) + 
    ylim(0, 10000)  + 
    scale_x_discrete(
      labels = function(labels) {
        sapply(seq_along(labels), function(i)
          paste0(ifelse(i %% 2 == 1, '', '\n'), labels[i]))}) + 
    t + 
    scale_fill_manual(values = grey_pal()(3))

histogram.so2.kMedian <- (
  disc.sample.kM %>%
    ggplot(aes(x = so2, fill = so2)) + 
      geom_bar() + 
      ylab("count") +  
      xlab(expression("Concentration of SO"[2])) + 
      scale_x_discrete(
      labels = function(labels) {
        sapply(seq_along(labels), function(i)
          paste0(ifelse(i %% 2 == 1, '', '\n'), sprintf("%.2f", as.numeric(labels[i]))))}) + 
      ylim(0, 10000) + 
      t + 
      scale_fill_manual(values = grey_pal()(3)))


ggsave(histogram.so2.kMedian,
       filename = "disc-so2-km.pdf",
       device = "pdf",
       width = PLOT_WIDTH,
       height = PLOT_HEIGHT)
ggsave(histogram.so2.quantile,
       filename = "disc-so2-q.pdf",
       device = "pdf",
       width = PLOT_WIDTH,
       height = PLOT_HEIGHT)


# Textual representation of the original network
model.string <- modelstring(bn)
model.string

# Manually defining the ground truth as the original network and replace Year, Month, Day and Hour with DateTime
new.model.string <- "[Region][Zone][Type][DateTime][Season][Latitude][Longitude][Altitude][wd|Region:DateTime][CVD60|Region:DateTime:Season][co|Zone:Type:DateTime][pm10|Zone:Type:DateTime][t2m|DateTime:Latitude:wd][so2|Zone:Type:DateTime:co][pm2.5|Region:Type:DateTime:pm10][ws|DateTime:Latitude:Longitude:t2m:wd][blh|Region:DateTime:Longitude:t2m:ws:wd][o3|Zone:Type:DateTime:blh][no2|Zone:Type:DateTime:Altitude:blh:o3:co:pm10][ssr|DateTime:Latitude:Longitude:Altitude:t2m:ws:wd:blh:no2:o3:so2:co:pm10][tp|DateTime:ws:ssr]"
ground.truth <- model2network(new.model.string)


sample.sizes <- c("100", "1000", "10000", "1e+05", "1e+06", "5e+06", "1e+07")
bins <- c(2, 3, 4, 5, 7, 10)
disc.methods <- c("built-in-discrete", "k-median")


# Load RData file
loadRData <- function(fileName) {
  load(fileName)
  get(ls()[ls() != "fileName"])
}


# Define quality measures for learned Bayesian networks
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

calculate.mcc <- function(row, gt) {
  net <- loadRData(row["filename"])
  conf.matrix <- bnlearn::compare(ground.truth, net)
  tp <- conf.matrix$tp
  fp <- conf.matrix$fp
  fn <- conf.matrix$fn
  edge.sum <- length(net$nodes) ^ 2 - length(net$nodes)
  tn <- edge.sum - (tp + fp + fn)
  (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
}

# Calculate quality measures for all learned graphs
quality.scores <- sample.sizes %>%
  tidyr::crossing(bins) %>%
  tidyr::crossing(disc.methods) %>%
  rename(n = ".") %>%
  mutate(filename = filename("./nets", paste(n, sep = ""), disc.methods, bins, "bin.rds", sep = "-"))


quality.scores$hamming <- apply(filenames, 1, calculate.hd, gt = ground.truth)
quality.scores$shd <- apply(filenames, 1, calculate.shd, gt = ground.truth)
quality.scores$f1 <- apply(filenames, 1, calculate.f1, gt = ground.truth)
quality.scores$mcc <- apply(filenames, 1, calculate.mcc, gt = ground.truth)



# Perform effect estimation for graph learned on k-Median sample, n = 5'000'000, k = 4
# Estimate WD -> T2M
load("./nets/5e+06-k-median-4-bin.rds")
cpdag1 <- computed.net2
graphviz.compare(ground.truth, cpdag1)

# drop undirected unineteresting edges, obtain forumla for do-operator
cpdag1 <- drop.arc(cpdag1, from = "blh", to = "ws")
cpdag1 <- drop.arc(cpdag1, from = "ssr", to = "tp")
sample.5m <- generate.sample(5000000)
igraph.cpdag1 <- graph_from_adjacency_matrix(amat(bn.4.cluster))

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

causal.effect(y = "t2m",
              x = "wd",
              z = NULL,
              G = igraph.cpdag1,
              expr = T)


# calculate do-operator for WD -> T2M
causal.effect.probabilites.t2m.wd <- function(data) {
  # select relevant columns
  data <- data %>%
    select(wd, t2m, Latitude, DateTime)
  
  # calculate probabilities for realizations of Latitude
  data <- data %>%
    add_count(Latitude) %>%
    mutate(prob_lat = n / n()) %>%
    select(-n)
  
  # calculate probabilities for realizations of DateTime
  data <- data %>%
    add_count(DateTime) %>%
    mutate(prob_datetime = n / n()) %>%
    select(-n)
  
  # calculate conditional probability for t2m by Latitude, DateTime, wd
  data <- data %>%
    group_by(wd, Latitude, DateTime) %>%
    add_count(t2m) %>%
    group_by(wd, Latitude, DateTime) %>%
    mutate(t2m_cond_prob = n / n()) %>%
    select(-n)
  
  # calculate causal effect and add probability for wd to table
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


# calculate information flow and normalized information flow for WD -> T2M
IF.wd_t2m <- function(data) {
  result <- 0
  for (t2m in unique(data$t2m)) {
    for (wd_1 in unique(data$wd)) {
      joint <- sum(data$t2m == t2m & data$wd == wd_1) / nrow(data)
      log_counter <-
        sum(data$t2m == t2m & data$wd == wd_1) / sum(data$wd == wd_1)
      log_denom <- 0
      for (wd_2 in unique(data$wd)) {
        temp_inner <- sum(data$t2m == t2m & data$wd == wd_2) / nrow(data)
        log_denom <- log_denom + temp_inner
      }
      temp <- joint * log2(log_counter / log_denom)
      result <- result + if_else(is.nan(temp), 0, temp)
    }
  }
  result
}

JointEntropy.wd_t2m <- function(data) {
  result <- 0
  for (t2m in unique(data$t2m)) {
    for (wd_1 in unique(data$wd)) {
      joint <- sum(data$t2m == t2m & data$wd == wd_1) / nrow(data)
      temp <- joint * log2(joint)
      result <- result - if_else(is.nan(temp), 0, temp)
    }
  }
  result
}

norm.IF.wd.t2m <- function(data) {
  1 - IF.wd_t2m(data) / JointEntropy.wd_t2m(data)
}

IF.wd.t2m(sample.5m.4.cluster)
norm.IF.wd.t2m(sample.5m.4.cluster)

# Generate plots for probability distributions P(T2M | do(WD))
t2m.do.wd65 <- causal.effect.prob.t2m.wd %>% filter(wd == 65)
t2m.do.wd167 <- causal.effect.prob.t2m.wd %>% filter(wd == 167)
t2m.do.wd235 <- causal.effect.prob.t2m.wd %>% filter(wd == 235)
t2m.do.wd296 <- causal.effect.prob.t2m.wd %>% filter(wd == 296)

plot.t2m.do.wd65 <- (t2m.do.wd65 %>%
                       ggplot(aes(x = t2m, y = p, fill = t2m)) + 
                        geom_bar(stat = 'identity') + 
                        ylab("P") +  
                        xlab("T2M") + 
                        ylim(0, 0.35) + 
                        t + 
                        scale_fill_manual(values = grey_pal()(4)))
plot.t2m.do.wd167 <- (t2m.do.wd167 %>%
                        ggplot(aes(x = t2m, y = p, fill = t2m)) + 
                          geom_bar(stat = 'identity') + 
                          ylab("P") +  
                          xlab("T2M") + 
                          ylim(0, 0.35) + 
                          t + 
                          scale_fill_manual(values = grey_pal()(4)))
plott2m.do.wd235 <- (t2m.do.wd235 %>%
                       ggplot(aes(x = t2m, y = p, fill = t2m)) + 
                        geom_bar(stat = 'identity') + 
                        ylab("P") +  
                        xlab("T2M") + 
                        ylim(0, 0.35) + 
                        t + 
                        scale_fill_manual(values = grey_pal()(4)))
plott2m.do.wd296 <- (t2m.do.wd296 %>%
                       ggplot(aes(x = t2m, y = p, fill = t2m)) + 
                        geom_bar(stat = 'identity') + 
                        ylab("P") +  
                        xlab("T2M") + 
                        ylim(0, 0.35) + 
                        t + 
                        scale_fill_manual(values = grey_pal()(4)))


ggsave(plot.t2m.do.wd65,
       filename = "t2m-do-wd-1.pdf",
       device = "pdf",
       width = PLOT_WIDTH,
       height = PLOT_HEIGHT)
ggsave(plot.t2m.do.wd167,
       filename = "t2m-do-wd-2.pdf",
       device = "pdf",
       width = PLOT_WIDTH,
       height = PLOT_HEIGHT)
ggsave(plot.t2m.do.wd135,
       filename = "t2m-do-wd-3.pdf",
       device = "pdf",
       width = PLOT_WIDTH,
       height = PLOT_HEIGHT
)
ggsave(plot.t2m.do.wd196,
       filename = "t2m-do-wd-4.pdf",
       device = "pdf",
       width = PLOT_WIDTH,
       height = PLOT_HEIGHT)


# Perform effect estimation for graph learned on Quantile sample, n = 100'000, k = 2
# Estimate PM2.5 -> PM10
load("./nets/1e+05-quantile-2-bin.rds")
cpdag2 <- computed.net1
graphviz.compare(ground.truth, cpdag2)
bn.2.bin.100k <- generate.sample(100000)
igraph.cdag <- graph_from_adjacency_matrix(amat(bn.2.bin.100k))

causal.effect(y = "pm10",
              x = "pm2.5",
              z = NULL,
              G = igraph.cdag,
              expr = T)

causal.effect.probabilites.pm10.pm2.5 <- function(data) {
  # select relevant columns
  data <- data %>%
    select(pm10, Zone, Type, DateTime, pm2.5)
  
  # calculate probabilities for realizations of Zone
  data <- data %>%
    add_count(Zone) %>%
    mutate(prob_zone = n / n()) %>%
    select(-n)
  
  # calculate probabilities for realizations of Type
  data <- data %>%
    add_count(Type) %>%
    mutate(prob_type = n / n()) %>%
    select(-n)
  
  # calculate probabilities for realizations of DateTime
  data <- data %>%
    add_count(DateTime) %>%
    mutate(prob_datetime = n / n()) %>%
    select(-n)
  
  # calculate conditional probability for pm10 by Zone, Type, DateTime, pm2.5
  data <- data %>%
    group_by(Zone, Type, DateTime, pm2.5) %>%
    add_count(pm10) %>%
    group_by(Zone, Type, DateTime, pm2.5) %>%
    mutate(pm10_cond_prob = n / n()) %>%
    select(-n)
  
  # calculate causal effect and add probability for pm2.5 to table
  data <- data %>%
    ungroup() %>%
    distinct() %>%
    mutate(total_prob = prob_zone * prob_type * prob_datetime * pm10_cond_prob) %>%
    group_by(pm10, pm2.5) %>%
    summarise(Ppm10 = sum(total_prob)) %>%
    inner_join(
      data %>%
        group_by(pm2.5) %>%
        count(pm2.5) %>%
        mutate("P(pm2.5)" = n / nrow(data)) %>%
        select(-n))
}

causal.effect.probabilites.pm10.pm2.5(bn.sample)

do.pm25.1 <- causal.effects %>%
  filter(pm2.5 == "(18.6,32.5]")

do.pm25.2 <- causal.effects %>%
  filter(pm2.5 == "[-4.75,18.6]")


plot.pm10.do.pm25.1 <- ggplot(do.pm25.1, aes(x = pm10, y = Ppm10, fill = pm10)) + 
                        geom_bar(stat = 'identity',
                                 position = position_dodge(),
                                 show.legend = F) + 
                        xlab(expression("PM"[10])) + 
                        ylab("P") + 
                        ylim(0, 0.6) + 
                        t + 
                        scale_fill_manual(values = grey_pal()(2))

plot.pm10.do.pm25.2 <- ggplot(do.pm25.2, aes(x = pm10, y = Ppm10, fill = pm10)) + 
                        geom_bar(stat = 'identity', 
                                 position = position_dodge()) + 
                        xlab(expression("PM"[10])) + 
                        ylab("P") + 
                        ylim(0, 0.6) + 
                        t + 
                        scale_fill_manual(values = grey_pal()(2))

ggsave(plot.pm10.do.pm25.1,
       filename = "do-pm25-1.pdf",
       device = "pdf",
       width = PLOT_WIDTH,
       height = PLOT_HEIGHT)
ggsave(plot.pm10.do.pm25.2,
       filename = "do-pm25-2.pdf",
       device = "pdf",
       width = PLOT_WIDTH,
       height = PLOT_HEIGHT)

# Generate networks to illustrate section of CPDAG2 before and after do-operator
learned.model.string = "[Zone][Type][DateTime][Region][pm10|pm2.5:Zone:Type:DateTime][pm2.5|Type:DateTime:Region]"
learned.graph.section <- model2network(learned_model_string)
graphviz.plot(learned_model_string)

do.model.string = "[Zone][Type][DateTime][pm10|pm2.5:Zone:Type:DateTime][pm2.5]"
do.graph.section <- model2network(do.model.string)
graphviz.plot(do.graph.section)


# Perform effect estimation for graph learned on Quantile sample, n = 10'000'000, k = 3
# Estimate CO -> SO2
sample.10m <- generate.sample(10000000)
sample.10m.3.bin <- discretize(sample.10m, breaks = 3)

causal.effect.probabilites.so2.co <- function(data) {
  # select relevant columns
  data <- data %>%
    select(so2, co, Type, Zone, DateTime)
  
  # calculate probabilities for realizations of Zone
  data <- data %>%
    add_count(Zone) %>%
    mutate(prob_zone = n / n()) %>%
    select(-n)
  
  # calculate probabilities for realizations of Type
  data <- data %>%
    add_count(Type) %>%
    mutate(prob_type = n / n()) %>%
    select(-n)
  
  # calculate probabilities for realizations of DateTime
  data <- data %>%
    add_count(DateTime) %>%
    mutate(prob_datetime = n / n()) %>%
    select(-n)
  
  # calculate conditional probability for so2 by Zone, Type, DateTime, co
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
