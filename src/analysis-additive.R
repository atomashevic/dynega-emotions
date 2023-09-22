source("helpers.R")

# Main analysis

## Data without transformation

urls <- read.csv('../data/videos-faces-selection.csv')
print('Face selection file loaded!')

gps <- read.csv('../data/gps.csv')
print('GPS data loaded!')

## Load and combine CSV files
data <- clean.csv(read.csv('../data/0-t300.csv'), face = 0, tmax = 240, k = 1)

for (i in 2:nrow(urls)){
    f <- urls$Face[i]
    if (f != 999){
        csv <- paste('../data/',i-1,'-t300.csv',sep='')
        # print(paste('Debug, video#:', i))
        tdf <- clean.csv(read.csv(csv), face = f, tmax = 240, k = i)
        data <- rbind(data, tdf)
    }
}

## Add populism data from GPS
data$populism <- as.integer(data$populism)
data <- data[!is.na(data$populism),]

data <- data[complete.cases(data),]
## remove video 152, no variance in disgust
data <- data[data$id != 152,]

print('Raw emotion time series data loaded!')

print(paste('Dataset has',length(unique(data$id)),'videos'))
print(paste('Dataset has',nrow(data),'rows'))
print(paste('Average number of frames per video:',nrow(data)/length(unique(data$id))))
print(paste('Dataset has',ncol(data),'columns'))
print('Names of columns:')
print(colnames(data))

a <- aggregate(data[,1:7], list(data$id), mean, na.rm = T)

min_ang <- min(a$anger, na.rm = T)
max_ang <- max(a$anger, na.rm = T)
avg_ang <- round(mean(a$anger, na.rm = T),3)

min_dis <- min(a$disgust, na.rm = T)
max_dis <- max(a$disgust, na.rm = T)
avg_dis <- round(mean(a$disgust, na.rm = T),3)

min_fear <- min(a$fear, na.rm = T)
max_fear <- max(a$fear, na.rm = T)
avg_fear <- round(mean(a$fear, na.rm = T),3)

min_hap <- min(a$happiness, na.rm = T)
max_hap <- max(a$happiness, na.rm = T)
avg_hap <- round(mean(a$happiness, na.rm = T),3)

min_sad <- min(a$sadness, na.rm = T)
max_sad <- max(a$sadness, na.rm = T)
avg_sad <- round(mean(a$sadness, na.rm = T),3)

min_sur <- min(a$surprise, na.rm = T)
max_sur <- max(a$surprise, na.rm = T)
avg_sur <- round(mean(a$surprise, na.rm = T),3)

min_neu <- min(a$neutral, na.rm = T)
max_neu <- max(a$neutral, na.rm = T)
avg_neu <- round(mean(a$neutral, na.rm = T),3)

min_pop <- min(data$populism, na.rm = T)
max_pop <- max(data$populism, na.rm = T)
avg_pop <- round(mean(data$populism, na.rm = T),3)

desc_table <- data.frame(
  min = round(c(min_ang, min_dis, min_fear, min_hap, min_sad, min_sur, min_neu, min_pop),3),
  max = round(c(max_ang, max_dis, max_fear, max_hap, max_sad, max_sur, max_neu, max_pop),3),
  avg = round(c(avg_ang, avg_dis, avg_fear, avg_hap, avg_sad, avg_sur, avg_neu,avg_pop),3),
   row.names = c('anger', 'disgust', 'fear', 'happiness', 'sadness', 'surprise', 'neutral', 'populism'))

print("Raw dataset statistis:")

desc_table

## Example, video # 90

k = 90
example <- data[data$id==k,]
app.entropies.raw <- c()
sam.entropies.raw <- c()

for (i in 1:7){
    app.entropies.raw <- c(app.entropies.raw,ApEn(example[,i]))
    sam.entropies.raw <- c(sam.entropies.raw,SampEn(example[,i]))
}

example <- rollmean(example[,1:7], 10, align = 'right')

png(filename = '../figures/vid90-raw.png', width = 800, height = 800 )
par(mfrow=c(3,3)) 
for (i in 1:7) {
    plot(example[,i], type='l', xlab='Time', ylab='Raw Score', main=colnames(example)[i], ylim=c(0,0.6))
    abline(h = mean(example[, i]), lty = 2, col = "red")
}
dev.off()

print("Figure 1 - Example 90 - Raw data DONE!")

## Data with additive transformation


if (!file.exists("../data/dyn-add.csv"))
{
data.add <- clean.csv(read.csv('../data/0-t300.csv'), face = 0, tmax = 240, k = 1, transformation = 'additive')

for (i in 2:nrow(urls)){
    f <- urls$Face[i]
    if (f != 999){
        csv <- paste('../data/',i-1,'-t300.csv',sep='')
        # print(paste('Debug, video#:', i))
        tdf <- clean.csv(read.csv(csv), face = f, tmax = 240, k = i, transformation = 'additive')
        data.add <- rbind(data.add, tdf)
    }
}

data.add$populism <- as.integer(data.add$populism)
data.add <- data.add[!is.na(data.add$populism),]
write.csv(as.data.frame(data.add),"../data/dyn-add.csv", row.names = FALSE)
print("Additive transformation dataset saved!")
} else { data.add <- as.data.frame(read.csv("../data/dyn-add.csv"))
	print('Additive transformation  emotion time series data loaded!')
}
data.add <- data.add[complete.cases(data.add),]
## remove video 152, no variance in disgust
data.add <- data.add[data.add$id != 152,]

print(paste('Dataset has',length(unique(data.add$id)),'videos'))
print(paste('Dataset has',nrow(data.add),'rows'))
print(paste('Average number of frames per video:',nrow(data.add)/length(unique(data.add$id))))
print(paste('Dataset has',ncol(data.add),'columns'))
print('Names of columns:')
print(colnames(data.add))

grouped_data <- aggregate(data.add[,1:8], list(data.add$id), mean, na.rm = T)

min_ang <- min(grouped_data$anger, na.rm = T)
max_ang <- max(grouped_data$anger, na.rm = T)
avg_ang <- round(mean(grouped_data$anger, na.rm = T),3)

min_dis <- min(grouped_data$disgust, na.rm = T)
max_dis <- max(grouped_data$disgust, na.rm = T)
avg_dis <- round(mean(grouped_data$disgust, na.rm = T),3)

min_fear <- min(grouped_data$fear, na.rm = T)
max_fear <- max(grouped_data$fear, na.rm = T)
avg_fear <- round(mean(grouped_data$fear, na.rm = T),3)

min_hap <- min(grouped_data$happiness, na.rm = T)
max_hap <- max(grouped_data$happiness, na.rm = T)
avg_hap <- round(mean(grouped_data$happiness, na.rm = T),3)

min_sad <- min(grouped_data$sadness, na.rm = T)
max_sad <- max(grouped_data$sadness, na.rm = T)
avg_sad <- round(mean(grouped_data$sadness, na.rm = T),3)

min_sur <- min(grouped_data$surprise, na.rm = T)
max_sur <- max(grouped_data$surprise, na.rm = T)
avg_sur <- round(mean(grouped_data$surprise, na.rm = T),3)

min_pop <- min(data.add$populism, na.rm = T)
max_pop <- max(data.add$populism, na.rm = T)
avg_pop <- round(mean(data.add$populism, na.rm = T),3)

desc.table.add <- data.frame(
  min = round(c(min_ang, min_dis, min_fear, min_hap, min_sad, min_sur, min_pop),3),
  max = round(c(max_ang, max_dis, max_fear, max_hap, max_sad, max_sur, max_pop),3),
  avg = round(c(avg_ang, avg_dis, avg_fear, avg_hap, avg_sad, avg_sur,avg_pop),3),
   row.names = c('anger', 'disgust', 'fear', 'happiness', 'sadness', 'surprise', 'populism'))


print("Additive transformation dataset statistics:")

desc.table.add

## Example, video # 90

k = 90
example <- data.add[data.add$id==k,]
app.entropies.add <- c()
sam.entropies.add <- c()

for (i in 1:6){
    app.entropies.add <- c(app.entropies.add,ApEn(example[,i]))
    sam.entropies.add <- c(sam.entropies.add,SampEn(example[,i]))
}

example <- rollmean(example[,1:7], 10, align = 'center')

png(filename = "../figures/vid90-add.png", height = 800, width = 800)
par(mfrow=c(3,3)) 
for (i in 1:6) {
  plot(example[,i], type='l', xlab='Time', ylab='Transformed Score', main=colnames(example)[i], ylim=c(-5,2))
  abline(h = mean(example[, i]), lty = 2, col = "red")
}
dev.off()

print("Figure 2 - Example 90 - Additive trasnformation data DONE!")

## Example frames:

print('First example:')
example.frame.1.raw <- c(0.06, 0.00, 0.24, 0.07, 0.01, 0.29, 0.34)

print('Second example:')
example.frame.2.raw <- c(0.10, 0.00, 0.34, 0.00, 0.02, 0.41, 0.12)

example.frame.1.add <- addlogratio(t(as.data.frame((example.frame.1.raw))))
example.frame.2.add <- addlogratio(t(as.data.frame((example.frame.2.raw))))

print('First example transformed:')
example.frame.1.add
print('Second example transformed:')
example.frame.2.add

## Divide the additive dataset into different levels of populism

# data.add select only complete cases

data.add <- data.add[complete.cases(data.add),]
data <- data[complete.cases(data),]
## remove video 152, no variance in disgust

data.add <- data.add[data.add$id != 152,]
data <- data[data$id != 152,]

data.add.plu <- data.add[data.add$populism <3, ]
data.add.pop <- data.add[data.add$populism >=3, ]
data.add.p1 <- data.add[data.add$populism == 1, ]
data.add.p2 <- data.add[data.add$populism == 2, ]
data.add.p3 <- data.add[data.add$populism == 3, ]
data.add.p4 <- data.add[data.add$populism == 4, ]

## Distribution of entropies
ids <- unique(data.add$id)
n <-  length(ids)

app.entropies.raw <-  matrix(0, nrow = n, ncol = 6)
sam.entropies.raw <-  matrix(0, nrow = n, ncol = 6)

app.entropies <-  matrix(0, nrow = n, ncol = 6)
sam.entropies <-  matrix(0, nrow = n, ncol = 6)

for (j in 1:n){
    example <- data.add[data.add$id==ids[j],]
    example.rw <- data[data$id == ids[j],]
    app.entropies.add <- c()
    sam.entropies.add <- c()
    app.entropies.rw <- c()
    sam.entropies.rw <- c()
    for (i in 1:6){
        app.entropies.add <- c(app.entropies.add,ApEn(example[,i]))
        app.entropies.rw <- c(app.entropies.rw,ApEn(example.rw[,i]))
        sam.entropies.add <- c(sam.entropies.add,SampEn(example[,i]))
        sam.entropies.rw <- c(sam.entropies.rw,SampEn(example.rw[,i]))
}
    app.entropies[j,] <- app.entropies.add
    sam.entropies[j,] <- sam.entropies.add
    app.entropies.raw[j,] <- app.entropies.rw
    sam.entropies.raw[j,] <- sam.entropies.rw
}

# Average change in Approximate Entropy
print('Average change in Approximate Entropy')
mean(app.entropies - app.entropies.raw, na.rm = T)
print('Average change in Approximate Entropy without disgust')
mean(app.entropies[,-2] - app.entropies.raw[,-2], na.rm = T)
print('Change in maximum Approximate Entropy')
max(app.entropies) - max(app.entropies.raw, na.rm = T)

# Average change in Sample Entropy
print('Average change in Sample Entropy')
mean(sam.entropies - sam.entropies.raw, na.rm = T)

print('Average change in Sample Entropy without disgust')
mean(sam.entropies[,-2] - sam.entropies.raw[,-2], na.rm = T)

print('Change in maximum Sample Entropy')
max(sam.entropies) - max(sam.entropies.raw, na.rm = T)

tdf <- data.frame(
  variable = rep(c("sam.entropies", "sam.entropies.raw"), each = length(sam.entropies)),
  value = c(sam.entropies, sam.entropies.raw)
)

densities <- lapply(list(sam.entropies, sam.entropies.raw), density)
max_density <- max(sapply(densities, function(x) max(x$y)))

png(filename = '../figures/entropy-densities.png', width = 800, height = 800 )

plot(density(tdf$value[tdf$variable == "sam.entropies"]), col = "red", lwd = 2, main = "Sample Entropy Comparison", xlab = "Sample Entropy", ylab = "Density")

lines(density(tdf$value[tdf$variable == "sam.entropies.raw"]), col = "blue", lwd = 2)

plot(densities[[1]], type = "l", col = "red", lwd = 2, ylim = c(0, max_density), xlab = "Sample Entropy", ylab = "Density",main = "Sample Entropy Comparison")

lines(densities[[2]], col = "blue", lwd = 2)

legend("topright", legend = c("Transformed data", "Raw data"), col = c("red", "blue"), lwd = 2)

dev.off()

## DynEGA - Mean correlations

print('Running DynEGA models ...')

print('One model to rule them all')

dega.add.all.0 <- dynEGA(
    data = data.add[,-8],
    n.embed = 10,
    tau = 1,
    delta = 1,
    level = 'population',
    id = 7,
    use.derivatives = 0,
    ncores = 10,
    progress = TRUE
)

dega.add.all.0.indpop <- dynEGA.ind.pop(
    data = data.add[,-8],
    n.embed = 10,
    tau = 1,
    delta = 1,
    id = 7,
    use.derivatives = 0,
    progress = FALSE
)

print("Pluralist model")


dega.add.plu.0 <- dynEGA(
    data = data.add.plu[,-8],
    n.embed = 10,
    tau = 1,
    delta = 1,
    level = 'population',
    id = 7,
    use.derivatives = 0,
    ncores = 10,
    progress = TRUE
)

dega.add.plu.0.indpop <- dynEGA.ind.pop(
    data = data.add.plu[,-8],
    n.embed = 10,
    tau = 1,
    delta = 1,
    id = 7,
    use.derivatives = 0,
    progress = TRUE
)

print("Populist model")

dega.add.pop.0 <- dynEGA(
    data = data.add.pop[,-8],
    n.embed = 10,
    tau = 1,
    delta = 1,
    level = 'population',
    id = 7,
    use.derivatives = 0,
    ncores = 10,
    progress = TRUE
)

dega.add.pop.0.indpop <- dynEGA.ind.pop(
    data = data.add.pop[,-8],
    n.embed = 10,
    tau = 1,
    delta = 1,
    id = 7,
    use.derivatives = 0,
    progress = TRUE
)

print("Populism = 1 model")

dega.add.p1.0 <- dynEGA(
    data = data.add.p1[,-8],
    n.embed = 10,
    tau = 1,
    delta = 1,
    level = 'population',
    id = 7,
    use.derivatives = 0,
    ncores = 10,
    progress = TRUE
)

print("Populism = 2 model")

dega.add.p2.0 <- dynEGA(
    data = data.add.p2[,-8],
    n.embed = 10,
    tau = 1,
    delta = 1,
    level = 'population',
    id = 7,
    use.derivatives = 0,
    ncores = 10,
    progress = TRUE
)

print("Populism = 3 model")

dega.add.p3.0 <- dynEGA(
    data = data.add.p3[,-8],
    n.embed = 10,
    tau = 1,
    delta = 1,
    level = 'population',
    id = 7,
    use.derivatives = 0,
    ncores = 10,
    progress = TRUE
)

print("Populism = 4 model")

dega.add.p4.0 <- dynEGA(
    data = data.add.p4[,-8],
    n.embed = 10,
    tau = 1,
    delta = 1,
    level = 'population',
    id = 7,
    use.derivatives = 0,
    ncores = 10,
    progress = TRUE
)

### Plot networks

print('DynEGA All derivatives=0')
net.add.all.0 <- dega.add.all.0$dynEGA$network
write.csv(as.data.frame(net.add.all.0), file = "../results/net.add.all.0.csv", row.names = FALSE)

png(filename = '../figures/dega.add.all.0.png', width = 800, height = 800)
plot(dega.add.all.0)
dev.off()


print('DynEGA P1 derivatives=0')
net.add.p1.0 <- dega.add.p1.0$dynEGA$network
write.csv(as.data.frame(net.add.p1.0), file = "../results/net.add.p1.0.csv", row.names = FALSE)

png(filename = '../figures/dega.add.p1.0.png', width = 800, height = 800)
plot(dega.add.p1.0)
dev.off()

print('Done!')

print('DynEGA P2 derivatives=0')
net.add.p2.0 <- dega.add.p2.0$dynEGA$network
write.csv(as.data.frame(net.add.p2.0), file = "../results/net.add.p2.0.csv", row.names = FALSE)

png(filename = '../figures/dega.add.p2.0.png', width = 800, height = 800)
plot(dega.add.p2.0)
dev.off()

print('Done!')

print('DynEGA P3 derivatives=0')
net.add.p3.0 <- dega.add.p3.0$dynEGA$network
write.csv(as.data.frame(net.add.p3.0), file = "../results/net.add.p3.0.csv", row.names = FALSE)

png(filename = '../figures/dega.add.p3.0.png', width = 800, height = 800)
plot(dega.add.p3.0)
dev.off()

print('Done!')

print('DynEGA P4 derivatives=0')
net.add.p4.0 <- dega.add.p4.0$dynEGA$network
write.csv(as.data.frame(net.add.p4.0), file = "../results/net.add.p4.0.csv", row.names = FALSE)

png(filename = '../figures/dega.add.p4.0.png', width = 800, height = 800)
plot(dega.add.p4.0)
dev.off()

print('Done!')


print('DynEGA Populism derivatives=0')
net.add.pop.0 <- dega.add.pop.0$dynEGA$network
write.csv(as.data.frame(net.add.pop.0), file = "../results/net.add.pop.0.csv", row.names = FALSE)

png(filename = '../figures/dega.add.pop.0.png', width = 800, height = 800)
plot(dega.add.pop.0)
dev.off()

print('Done!')

print('DynEGA Pluralism derivatives=0')
net.add.plu.0 <- dega.add.plu.0$dynEGA$network
write.csv(as.data.frame(net.add.plu.0), file = "../results/net.add.plu.0.csv", row.names = FALSE)

png(filename = '../figures/dega.add.plu.0.png', width = 800, height = 800)
plot(dega.add.plu.0)
dev.off()

print('Done!')


## DynEGA - Rate of change correlations

print('Running DynEGA models ...')

print('One model to rule them all')

dega.add.all.1 <- dynEGA(
    data = data.add[,-8],
    n.embed = 10,
    tau = 1,
    delta = 1,
    level = 'population',
    id = 7,
    use.derivatives = 1,
    ncores = 10,
    progress = TRUE
)

dega.add.all.1.indpop <- dynEGA.ind.pop(
    data = data.add[,-8],
    n.embed = 10,
    tau = 1,
    delta = 1,
    id = 7,
    use.derivatives = 1,
    progress = TRUE
)

print("Pluralist model")

dega.add.plu.1 <- dynEGA(
    data = data.add.plu[,-8],
    n.embed = 10,
    tau = 1,
    delta = 1,
    level = 'population',
    id = 7,
    use.derivatives = 1,
    ncores = 10,
    progress = TRUE
)

dega.add.plu.1.indpop <- dynEGA.ind.pop(
    data = data.add.plu[,-8],
    n.embed = 10,
    tau = 1,
    delta = 1,
    id = 7,
    use.derivatives = 1,
    progress = TRUE
)

print("Populist model")

dega.add.pop.1 <- dynEGA(
    data = data.add.pop[,-8],
    n.embed = 10,
    tau = 1,
    delta = 1,
    level = 'population',
    id = 7,
    use.derivatives = 1,
    ncores = 10,
    progress = TRUE
)

dega.add.pop.1.indpop <- dynEGA.ind.pop(
    data = data.add.pop[,-8],
    n.embed = 10,
    tau = 1,
    delta = 1,
    id = 7,
    use.derivatives = 1,
    progress = TRUE
)

print("Populism = 1 model")

dega.add.p1.1 <- dynEGA(
    data = data.add.p1[,-8],
    n.embed = 10,
    tau = 1,
    delta = 1,
    level = 'population',
    id = 7,
    use.derivatives = 1,
    ncores = 10,
    progress = TRUE
)

print("Populism = 2 model")

dega.add.p2.1 <- dynEGA(
    data = data.add.p2[,-8],
    n.embed = 10,
    tau = 1,
    delta = 1,
    level = 'population',
    id = 7,
    use.derivatives = 1,
    ncores = 10,
    progress = TRUE
)

print("Populism = 3 model")

dega.add.p3.1 <- dynEGA(
    data = data.add.p3[,-8],
    n.embed = 10,
    tau = 1,
    delta = 1,
    level = 'population',
    id = 7,
    use.derivatives = 1,
    ncores = 10,
    progress = TRUE
)

print("Populism = 4 model")

dega.add.p4.1 <- dynEGA(
    data = data.add.p4[,-8],
    n.embed = 10,
    tau = 1,
    delta = 1,
    level = 'population',
    id = 7,
    use.derivatives = 1,
    ncores = 10,
    progress = TRUE
)

### Plot networks

print('DynEGA All derivatives=1')

net.add.all.1 <- dega.add.all.1$dynEGA$network
write.csv(as.data.frame(net.add.all.1), file = "../results/net.add.all.1.csv", row.names = FALSE)

png(filename = '../figures/dega.add.all.1.png', width = 800, height = 800)
plot(dega.add.all.1)
dev.off()


print('DynEGA P1 derivatives=1')

net.add.p1.1 <- dega.add.p1.1$dynEGA$network
write.csv(as.data.frame(net.add.p1.1), file = "../results/net.add.p1.1.csv", row.names = FALSE)

png(filename = '../figures/dega.add.p1.1.png', width = 800, height = 800)
plot(dega.add.p1.1)
dev.off()

print('Done!')

print('DynEGA P2 derivatives=1')

net.add.p2.1 <- dega.add.p2.1$dynEGA$network
write.csv(as.data.frame(net.add.p2.1), file = "../results/net.add.p2.1.csv", row.names = FALSE)

png(filename = '../figures/dega.add.p2.1.png', width = 800, height = 800)
plot(dega.add.p2.1)
dev.off()

print('Done!')

print('DynEGA P3 derivatives=1')

net.add.p3.1 <- dega.add.p3.1$dynEGA$network
write.csv(as.data.frame(net.add.p3.1), file = "../results/net.add.p3.1.csv", row.names = FALSE)

png(filename = '../figures/dega.add.p3.1.png', width = 800, height = 800)
plot(dega.add.p3.1)
dev.off()

print('Done!')

print('DynEGA P4 derivatives=1')

net.add.p4.1 <- dega.add.p4.1$dynEGA$network
write.csv(as.data.frame(net.add.p4.1), file = "../results/net.add.p4.1.csv", row.names = FALSE)

png(filename = '../figures/dega.add.p4.1.png', width = 800, height = 800)
plot(dega.add.p4.1)
dev.off()

print('Done!')

print('DynEGA Populism derivatives=1')

net.add.pop.1 <- dega.add.pop.1$dynEGA$network
write.csv(as.data.frame(net.add.pop.1), file = "../results/net.add.pop.1.csv", row.names = FALSE)

png(filename = '../figures/dega.add.pop.1.png', width = 800, height = 800)
plot(dega.add.pop.1)
dev.off()

print('Done!')

print('DynEGA Pluralism derivatives=1')

net.add.plu.1 <- dega.add.plu.1$dynEGA$network
write.csv(as.data.frame(net.add.plu.1), file = "../results/net.add.plu.1.csv", row.names = FALSE)

png(filename = '../figures/dega.add.plu.1.png', width = 800, height = 800)
plot(dega.add.plu.1)
dev.off()

print('Done!')

## Ergodicity Information Index

set.seed(42)

boot.ei.all.0 <-  boot.ergoInfo(dega.add.all.0.indpop, iter = 100)
save(boot.ei.all.0, file = "../results/boot.ei.all.0.RData")

# plot density of ei.all.0
ei.all.0 <- boot.ei.all.0$boot.ergoInfo
densities <- lapply(list(ei.all.0), density)
max_density <- max(sapply(densities, function(x) max(x$y)))
ergo.info <- ergoInfo(dega.add.all.0.indpop)


png(filename = '../figures/ei.all.0.png', width = 800, height = 800 )
plot(density(ei.all.0), col = "red", lwd = 2, main = "Ergodicity Information Index", xlab = "Ergodicity Information Index", ylab = "Density")
# add ego.info$EII as vertical dashed blue line
abline(v = ergo.info$EII, lty = 2, col = "blue")

dev.off()

boot.ei.plu.0 <-  boot.ergoInfo(dega.add.plu.0.indpop, iter = 100)
save(boot.ei.plu.0, file = "../results/boot.ei.plu.0.RData")

boot.ei.pop.0 <-  boot.ergoInfo(dega.add.pop.0.indpop, iter = 100)
save(boot.ei.pop.0, file = "../results/boot.ei.pop.0.RData")

# boot.ei.all.1 <- boot.ergoInfo(dega.add.all.1.indpop, iter = 100)

# boot.ei.plu.1 <- boot.ergoInfo(dega.add.plu.1.indpop, iter = 100)

# boot.ei.pop.1 <- boot.ergoInfo(dega.add.pop.1.indpop, iter = 100)

# Distribution of neutral scores

n <- length(unique(data$id))
neutrals <- c()
for (i in 1:n){
    video <- data[data$id==i,]
    neutrals <- c(neutrals, mean(video$neutral, na.rm =TRUE))
}

# plot density of neutrals

neutrals <- neutrals[!is.na(neutrals)]

densities <- lapply(list(neutrals), density)
max_density <- max(sapply(densities, function(x) max(x$y)))

png(filename = '../figures/neutral-densities.png', width = 800, height = 800 )

plot(density(neutrals), col = "red", lwd = 2, main = "Neutral score Density", xlab = "Average Neutral score", ylab = "Density")
dev.off()

# Entropy of neutral scores

n <- length(unique(data$id))
neutral.entropy <- c()
for (i in 1:n){
    video <- data[data$id==i,]
    video <- video[!is.na(video$neutral),]
    if (nrow(video) > 1) neutral.entropy <- c(neutral.entropy, SampEn(video$neutral))
}

png(filename = '../figures/neutral-entropy.png', width = 800, height = 800)
plot(density(neutral.entropy), col = "red", lwd = 2, main = "Neutral score Sample Entropy", xlab = "Sample Entropy", ylab = "Density")
dev.off()