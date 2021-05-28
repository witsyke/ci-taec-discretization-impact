source("./helper.R")

load("./blacklist.rds")
sample.sizes <-
  c(1000000, 5000000, 10000000)

for (sample.size in sample.sizes) {
  sample <- generate.sample(sample.size)

  cat(paste(
    "continuous,",
    sample.size,
    "samples: "
  ))

  cat(system.time(computed.net1 <- pc.stable(sample,
    test = "mi-cg",
    blacklist = base::as.data.frame(bl)
  )))

  cat("\n")

  save(computed.net1, file = filename(
    prefix = "./nets/",
    name = paste(sample.size,
      "continuous.rds",
      sep = "-"
    )
  ))
}