# Analysis for the revision of the paper

source("src/helpers.R")

# first dataset

ds1 <- read.csv("data/revision/video-1-300.csv")[, -1]

ds["video"] <- 1

# loop for other videos

for (i in 2:20) {
    filename <- paste("data/revision/video-", i, "-300.csv", sep = "")
    tdf <- read.csv(filename)[, -1]
}
