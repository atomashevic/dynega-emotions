# Analysis for the paper revision

source("src/helpers.R")

# Load GPS and URL data

urls <- read.csv("data/videos-faces-selection.csv")
gps <- read.csv("data/gps.csv")

# Aggregate CSV files

ds <- read.csv("data/revision/video-1-300.csv")[, -1]
ds["video"] <- 1
ds["populism"] <- as.integer(populism(gps, 1))

pops <- c(as.integer(populism(gps, 1)))

k <- get_max_id("data/revision")

for (i in 2:k) {
  filename <- paste("data/revision/video-", i, "-300.csv", sep = "")
  # check if file exists
  if (file.exists(filename)) {
    tdf <- read.csv(filename)[, -1]
    tdf["video"] <- i
    tdf["populism"] <- as.integer(populism(gps, i))
    if (get_empty_rows(tdf) < 0.1) {
      ds <- rbind(ds, tdf)
      pops <- c(pops, as.integer(populism(gps, i)))
    }
  }
}

# Dataset information

print(paste("Dataset has", length(unique(ds$video)), "videos"))
print(paste("Dataset has:", nrow(ds), "rows"))
print(paste("Average number of frames per video:",
            nrow(ds) / length(unique(ds$video))))
print(paste("Dataset has", ncol(ds), "columns"))
print("Column names:")
print(colnames(ds))
print("===============================================")

print("Model 1: population, zero order")

network_ds <- ds[, c(1:6, 8)]

nembeds <- c(3, 5, 10, 15, 20)


tefis <- c()

for (n in nembeds) {
  model_1 <- dynEGA(
    data = network_ds,
    n.embed = n,
    level = "population",
    id = 7,
    use.derivative = 0,
    ncores = 10,
    algorithm = "louvain",
    progress = TRUE
  )
  print(paste("Model 1 with", n, "embeddings"))
  print(model_1$dynEGA$population$TEFI)
  tefis <- c(tefis, model_1$dynEGA$population$TEFI)
}

ne <- nembeds[which.min(tefis)]
print(paste("Lowest TEFI of",
            round(min(tefis), 3),
            "was found for",
            ne, "embedding dimensions"))

model_1 <- dynEGA(
  data = network_ds,
  n.embed = ne,
  level = "population",
  id = 7,
  use.derivative = 0,
  ncores = 10,
  algorithm = "louvain",
  progress = TRUE
)

print("Model 1 with optimal number of embeddings: ")

print(paste("Model has", model_1$dynEGA$population$n.dim, "communities"))

dimnames(model_1$dynEGA$population$network)[[1]] <-
  c("excitement", "happiness", "pride", "anger", "fear", "sadness")

dimnames(model_1$dynEGA$population$network)[[2]] <-
  c("excitement", "happiness", "pride", "anger", "fear", "sadness")


png("figures/revision-model1.png", width = 2400, height = 2400, res = 300)
plot(model_1, node.size = 20)
dev.off()

print(model_1$dynEGA$population)

model_1$dynEGA$population$network

print("Network Loadings for Model 1")

net.loads(model_1$dynEGA$population$network, model_1$dynEGA$population$wc)

print("===============================================")
print("Model 2: population, first order")

tefis <- c()

for (n in nembeds) {
  model_2 <- dynEGA(
    data = network_ds,
    n.embed = n,
    level = "population",
    id = 7,
    use.derivative = 1,
    ncores = 10,
    algorithm = "louvain",
    progress = TRUE
  )
  print(paste("Model 2 with", n, "embeddings"))
  print(model_2$dynEGA$population$TEFI)
  tefis <- c(tefis, model_2$dynEGA$population$TEFI)
}

ne <- nembeds[which.min(tefis)]

print(paste("Lowest TEFI of",
            round(min(tefis), 3),
            "was found for",
            ne, "embedding dimensions"))

model_2 <- dynEGA(
  data = network_ds,
  n.embed = ne,
  level = "population",
  id = 7,
  user.derivative = 1,
  ncores = 10,
  algorithm = "louvain"
)

dimnames(model_2$dynEGA$population$network)[[1]] <-
  c("excitement", "happiness", "pride", "anger", "fear", "sadness")

dimnames(model_2$dynEGA$population$network)[[2]] <-
  c("excitement", "happiness", "pride", "anger", "fear", "sadness")

png("figures/revision-model2.png", width = 2400, height = 2400, res = 300)

plot(model_2, node.size = 20)

dev.off()


## anger is negative with everything, so it's in the wrong community

print("Model 2 with optimal number of embeddings: ")

print(paste("Model has", model_2$dynEGA$population$n.dim, "communities"))

print(model_2$dynEGA$population)


print("Network Loadings for Model 2")

net.loads(model_2$dynEGA$population$network, model_2$dynEGA$population$wc)

print("===============================================")


print("Model 3: group, zero order")


model_3 <- dynEGA(
  data = network.g.ds,
  n.embed = 5,
  group = 8,
  level = "group",
  id = 7,
  use.derivative = 0,
  ncores = 10,
  algorithm = "louvain"
)

dimnames(model_3$dynEGA[[1]][[1]]$network)[[1]] <-
  c("excitement", "happiness", "pride", "anger", "fear", "sadness")
dimnames(model_3$dynEGA[[1]][[1]]$network)[[2]] <-
  c("excitement", "happiness", "pride", "anger", "fear", "sadness")

dimnames(model_3$dynEGA[[1]][[2]]$network)[[1]] <-
  c("excitement", "happiness", "pride", "anger", "fear", "sadness")
dimnames(model_3$dynEGA[[1]][[2]]$network)[[2]] <-
  c("excitement", "happiness", "pride", "anger", "fear", "sadness")

dimnames(model_3$dynEGA[[1]][[3]]$network)[[1]] <-
  c("excitement", "happiness", "pride", "anger", "fear", "sadness")
dimnames(model_3$dynEGA[[1]][[3]]$network)[[2]] <-
  c("excitement", "happiness", "pride", "anger", "fear", "sadness")

dimnames(model_3$dynEGA[[1]][[4]]$network)[[1]] <-
  c("excitement", "happiness", "pride", "anger", "fear", "sadness")

dimnames(model_3$dynEGA[[1]][[4]]$network)[[2]] <-
  c("excitement", "happiness", "pride", "anger", "fear", "sadness")


png("figures/revision-model3.png", width = 3600, height = 3600, res = 300)

plot(model_3)

dev.off()

print("Model 3 details: ")

print(model_3)


library(ggplot2)


nl1 <-  net.loads(model_3$dynEGA[[1]][[1]]$network, model_3$dynEGA[[1]][[1]]$wc)

nl2 <-  net.loads(model_3$dynEGA[[1]][[2]]$network, model_3$dynEGA[[1]][[2]]$wc)

nl3 <-  net.loads(model_3$dynEGA[[1]][[3]]$network, model_3$dynEGA[[1]][[3]]$wc)

nl4 <-  net.loads(model_3$dynEGA[[1]][[4]]$network, model_3$dynEGA[[1]][[4]]$wc)


### create data frame of network scores for each emotion and group

df <- data.frame(
  emotion = c("excitement", "happiness", "pride", "anger", "fear", "sadness"),
  group2 = c(0.294, 0.186, 0.359, 0.312, 0.27, -0.253),
  group3 = c(0.250, 0.233, 0.278, 0.294, 0.51, -0.198),
  group4 = c(0.277, 0.466, 0.253, 0.174, 0.3, -0.263)
)

library(tidyr)
df <- df %>% gather(key = "group", value = "loading", -emotion)

# line plot, lines are emotions, x-axis is group, y-axis is loading


png("figures/revision-model3-loadings.png",
    width = 2400, height = 2400, res = 300)
ggplot(df, aes(x = factor(group), y = loading,
               group = emotion, color = emotion)) +
  geom_line(size = 1.5) +
  geom_point(size = 2) +
  theme_minimal() +
  scale_x_discrete(labels = c("Moderate pluralist",
                              "Moderate populist",
                              "Strong populist")) +
  xlab("Populism") + ylab("Loading") +
  ggtitle("Network Loadings Across Groups for Model 5")
dev.off()

print("===============================================")


### Ergodicity Information Index



print("Model 1i: population + individual, zero order")

model_1i <- dynEGA(
  data = network_ds,
  n.embed = 5,
  level = c("individual", "population"),
  id = 7,
  user.derivative = 0,
  ncores = 10,
  algorithm = "louvain"
)


ic <- infoCluster(model_1i)

print("Model 1i details: ")

print(model_1i)

print("===============================================")

set.seed(42)

boot_ei <- boot.ergoInfo(model_1i, iter = 100, ncores = 10)

boot_ei$empirical.ergoInfo

print("Ergodicity information for model 1 (individual, zero order): ")

print(boot_ei)

png("figures/revision-model1-ei.png", width = 2400, height = 2400, res = 300)

plot(boot_ei)

dev.off()
print("===============================================")
