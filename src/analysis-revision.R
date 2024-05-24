# Analysis for the revision of the paper

source("src/helpers.R")

# i want to log entire output to a new text file


# Load GPS data

urls <- read.csv('data/videos-faces-selection.csv')
gps <- read.csv('data/gps.csv')

# Aggregate CSV files

ds <- read.csv("data/revision/video-1-300.csv")[, -1]
ds["video"] <- 1
ds["populism"] <- as.integer(populism(gps, 1))

pops <- c(as.integer(populism(gps, 1)))

k = get_max_id("data/revision")

for (i in 2:k) { 

    filename <- paste("data/revision/video-", i, "-300.csv", sep = "")
    #check if file exists
    if (file.exists(filename)) {
        tdf <- read.csv(filename)[, -1]
        tdf["video"] <- i
        tdf["populism"] <- as.integer(populism(gps, i))
        if(get_empty_rows(tdf) < 0.1){
            ds <- rbind(ds, tdf)
            pops <- c(pops, as.integer(populism(gps, i)))
        }
    }
}

# Dataset information

print(paste("Dataset has", length(unique(ds$video)), "videos"))
print(paste("Dataset has:", nrow(ds), "rows"))
print(paste("Average number of frames per video:", nrow(ds)/length(unique(ds$video))))
print(paste("Dataset has", ncol(ds), "columns"))
print("Column names:")
print(colnames(ds))


print('===============================================')

print("Model 1: population, zero order")

network.ds <- ds[,c(1:6, 8)]

nembeds = c(3, 5, 10, 15, 20)


tefis <- c()

for (n in nembeds) {
    model.1 <- dynEGA(
        data = network.ds,
        n.embed = n,
        level = 'population',
        id = 7,
        use.derivative = 0,
        ncores = 10,
        algorithm = 'louvain',
        progress = TRUE
    )
    print(paste("Model 1 with", n, "embeddings"))
    print(model.1$dynEGA$population$TEFI)
    tefis <- c(tefis, model.1$dynEGA$population$TEFI)
}

ne <- nembeds[which.min(tefis)]
print(paste("Lowest TEFI of", round(min(tefis),3),'was found for'  , ne, "embedding dimensions"))

model.1 <- dynEGA(
    data = network.ds,
    n.embed = ne,
    level = 'population',
    id = 7,
    use.derivative = 0,
    ncores = 10,
    algorithm = 'louvain',
    progress = TRUE
)
print('Model 1 with optimal number of embeddings: ')

print(paste('Model has', model.1$dynEGA$population$n.dim, 'communities'))

dimnames(model.1$dynEGA$population$network)[[1]] <- c('excitement', 'happiness', 'pride', 'anger', 'fear', 'sadness')
dimnames(model.1$dynEGA$population$network)[[2]] <- c('excitement', 'happiness', 'pride', 'anger', 'fear', 'sadness')


png('figures/revision-model1.png', width = 2400, height = 2400, res = 300)

plot(model.1, node.size = 20)

dev.off()

print(model.1$dynEGA$population)




print('===============================================')
print("Model 2: population, first order")

tefis <- c()

for (n in nembeds) {
    model.2 <- dynEGA(
        data = network.ds,
        n.embed = n,
        level = 'population',
        id = 7,
        use.derivative = 1,
        ncores = 10,
        algorithm = 'louvain',
        progress = TRUE
    )
    print(paste("Model 2 with", n, "embeddings"))
    print(model.2$dynEGA$population$TEFI)
    tefis <- c(tefis, model.2$dynEGA$population$TEFI)
}

ne <- nembeds[which.min(tefis)]

print(paste("Lowest TEFI of", round(min(tefis),3),'was found for'  , ne, "embedding dimensions"))

model.2 <- dynEGA(
    data = network.ds,
    n.embed = ne,
    level = 'population',
    id = 7,
    user.derivative = 1,
    ncores = 10,
    algorithm = 'louvain'
)

dimnames(model.2$dynEGA$population$network)[[1]] <- c('excitement', 'happiness', 'pride', 'anger', 'fear', 'sadness')

dimnames(model.2$dynEGA$population$network)[[2]] <- c('excitement', 'happiness', 'pride', 'anger', 'fear', 'sadness')

png('figures/revision-model2.png', width = 2400, height = 2400, res = 300)

plot(model.2, node.size = 20)

dev.off()


## anger is negative with everything, so it's in the wrong community

print('Model 2 with optimal number of embeddings: ')

print(paste('Model has', model.2$dynEGA$population$n.dim, 'communities'))

print(model.2$dynEGA$population)

print('===============================================')


print("Model 3: individual, zero order")

model.3 <- dynEGA(
    data = network.ds,
    n.embed = 5,
    level = c('individual','population'),
    id = 7,
    user.derivative = 0,
    ncores = 10,
    algorithm = 'louvain'
)


ic <- infoCluster(model.3)

print('Model 3 details: ')

print(model.3)

print('===============================================')

print("Model 4: individual, first order")

model.4 <- dynEGA(
    data = network.ds,
    n.embed = 15,
    level = c('individual','population'),
    id = 7,
    user.derivative = 1,
    ncores = 10,
    algorithm = 'louvain'
)

print('Model 4 details: ')

print(model.4)

print('===============================================')

### Model 5: group, zero order

network.g.ds <- ds[,c(1:6, 8, 9)]

network.g.ds$group <- as.factor(network.g.ds$populism)

network.g.ds <- network.g.ds[,-8]



print("Model 5: group, zero order")



model.5 <- dynEGA(
    data = network.g.ds,
    n.embed = 5,
    group = 8,
    level = 'group',
    id = 7,
    use.derivative = 0,
    ncores = 10,
    algorithm = 'louvain'
)

dimnames(model.5$dynEGA[[1]][[1]]$network)[[1]] <- c('excitement', 'happiness', 'pride', 'anger', 'fear', 'sadness')
dimnames(model.5$dynEGA[[1]][[1]]$network)[[2]] <- c('excitement', 'happiness', 'pride', 'anger', 'fear', 'sadness')

dimnames(model.5$dynEGA[[1]][[2]]$network)[[1]] <- c('excitement', 'happiness', 'pride', 'anger', 'fear', 'sadness')
dimnames(model.5$dynEGA[[1]][[2]]$network)[[2]] <- c('excitement', 'happiness', 'pride', 'anger', 'fear', 'sadness')

dimnames(model.5$dynEGA[[1]][[3]]$network)[[1]] <- c('excitement', 'happiness', 'pride', 'anger', 'fear', 'sadness')
dimnames(model.5$dynEGA[[1]][[3]]$network)[[2]] <- c('excitement', 'happiness', 'pride', 'anger', 'fear', 'sadness')

dimnames(model.5$dynEGA[[1]][[4]]$network)[[1]] <- c('excitement', 'happiness', 'pride', 'anger', 'fear', 'sadness')

dimnames(model.5$dynEGA[[1]][[4]]$network)[[2]] <- c('excitement', 'happiness', 'pride', 'anger', 'fear', 'sadness')


png('figures/revision-model5.png', width = 3600, height = 3600, res = 300)

plot(model.5)

dev.off()

print('Model 5 details: ')

print(model.5)

print('===============================================')

### Model 6: group, first order

print("Model 6: group, first order")

model.6 <- dynEGA(
    data = network.g.ds,
    n.embed = 20,
    group = 8,
    level = 'group',
    id = 7,
    use.derivative = 1,
    ncores = 10,
    algorithm = 'louvain'
)

dimnames(model.6$dynEGA[[1]][[1]]$network)[[1]] <- c('excitement', 'happiness', 'pride', 'anger', 'fear', 'sadness')
dimnames(model.6$dynEGA[[1]][[1]]$network)[[2]] <- c('excitement', 'happiness', 'pride', 'anger', 'fear', 'sadness')

dimnames(model.6$dynEGA[[1]][[2]]$network)[[1]] <- c('excitement', 'happiness', 'pride', 'anger', 'fear', 'sadness')
dimnames(model.6$dynEGA[[1]][[2]]$network)[[2]] <- c('excitement', 'happiness', 'pride', 'anger', 'fear', 'sadness')

dimnames(model.6$dynEGA[[1]][[3]]$network)[[1]] <- c('excitement', 'happiness', 'pride', 'anger', 'fear', 'sadness')
dimnames(model.6$dynEGA[[1]][[3]]$network)[[2]] <- c('excitement', 'happiness', 'pride', 'anger', 'fear', 'sadness')

dimnames(model.6$dynEGA[[1]][[4]]$network)[[1]] <- c('excitement', 'happiness', 'pride', 'anger', 'fear', 'sadness')

dimnames(model.6$dynEGA[[1]][[4]]$network)[[2]] <- c('excitement', 'happiness', 'pride', 'anger', 'fear', 'sadness')

png('figures/revision-model6.png', width = 3600, height = 3600, res = 300)

plot(model.6)

dev.off()

print('Model 6 details: ')

print(model.6)

print('===============================================')


## Ergodicity Information Index

set.seed(42)

boot.ei.3 <- boot.ergoInfo(model.3, iter = 100, ncores = 10)

print('Ergodicity information for model 3 (individual, zero order): ')

print(boot.ei.3)

print('===============================================')


boot.ei.4 <- boot.ergoInfo(model.4, iter = 100, ncores = 10)

print('Ergodicity information for model 4 (individual, first order): ')

print(boot.ei.4)