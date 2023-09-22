source("helpers.R")

urls <- read.csv('../data/videos-faces-selection.csv')
print('Face selection file loaded!')

gps <- read.csv('../data/gps.csv')
print('GPS data loaded!')

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

if (!file.exists("../data/dyn-cent.csv"))
{

data.cent<- clean.csv(read.csv('../data/0-t300.csv'), face = 0, tmax = 240, k = 1, transformation = 'center')

for (i in 2:nrow(urls)){
    f <- urls$Face[i]
    if (f != 999){
        csv <- paste('../data/',i-1,'-t300.csv',sep='')
        # print(paste('Debug, video#:', i))
        tdf <- clean.csv(read.csv(csv), face = f, tmax = 240, k = i, transformation = 'center')
        data.cent <- rbind(data.cent, tdf)
    }
}
data.cent$populism <- as.integer(data.cent$populism)
data.cent <- data.cent[!is.na(data.cent$populism),]
write.csv(as.data.frame(data.cent),"../data/dyn-cent.csv", row.names = FALSE)
print("Center transformation dataset saved!")
} else { data.cent <- as.data.frame(read.csv("../data/dyn-cent.csv"))
	print('Center transformation  emotion time series data loaded!')
}

data.cent$populism <- as.integer(data.cent$populism)
data.cent <- data.cent[!is.na(data.cent$populism),]

print(paste('Dataset has',length(unique(data.cent$id)),'videos'))
print(paste('Dataset has',nrow(data.cent),'rows'))
print(paste('Average number of frames per video:',nrow(data.cent)/length(unique(data.cent$id))))
print(paste('Dataset has',ncol(data.cent),'columns'))
print('Names of columns:')
print(colnames(data.cent))

## Example, video # 90

k = 90
example <- data.cent[data.cent$id==k,]
app.entropies.cent <- c()
sam.entropies.cent <- c()

for (i in 1:7) {
  app.entropies.cent <- c(app.entropies.cent, ApEn(example[,i]))
  sam.entropies.cent <- c(sam.entropies.cent, SampEn(example[,i]))
}

grouped_data <- aggregate(data.cent[,1:8], list(data.cent$id), mean, na.rm = T)

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

min_pop <- min(data.cent$populism, na.rm = T)
max_pop <- max(data.cent$populism, na.rm = T)
avg_pop <- round(mean(data.cent$populism, na.rm = T),3)

min_neu <- min(grouped_data$neutral, na.rm = T)
max_neu <- max(grouped_data$neutral, na.rm = T)
avg_neu <- round(mean(grouped_data$neutral, na.rm = T),3)

# combine all of these into a table

desc.table.cent <- data.frame(
  min = round(c(min_ang, min_dis, min_fear, min_hap, min_sad, min_sur, min_neu, min_pop),3),
  max = round(c(max_ang, max_dis, max_fear, max_hap, max_sad, max_sur, max_neu,max_pop),3),
  avg = round(c(avg_ang, avg_dis, avg_fear, avg_hap, avg_sad, avg_sur,avg_neu, avg_pop),3),
   row.names = c('anger', 'disgust', 'fear', 'happiness', 'sadness', 'surprise','neutral', 'populism'))

desc.table.cent

k = 90
example <- data.cent[data.cent$id==k,]
example <- rollmean(example[,1:7], 10)

png(filename = "../figures/vid90-cent.png", height = 800, width = 800)
par(mfrow=c(3,3)) 
for (i in 1:7) {
  plot(example[,i], type='l', xlab='Time', ylab='Transformed Score', main=colnames(example)[i], ylim=c(-4,3))
}
dev.off()

print('First example:')
example.frame.1.raw <- c(0.06, 0.00, 0.24, 0.07, 0.01, 0.29, 0.34)

print('Second example:')
example.frame.2.raw <- c(0.10, 0.00, 0.34, 0.00, 0.02, 0.41, 0.12)

example.frame.1.cent <- centlogratio(t(as.data.frame((example.frame.1.raw))))
example.frame.2.cent <- centlogratio(t(as.data.frame((example.frame.2.raw))))

print('First example transformed:')
example.frame.1.cent
print('Second example transformed:')
example.frame.2.cent

# data.cent select only complete cases

data.cent <- data.cent[complete.cases(data.cent),]

## remove video 152, no variance in disgust

data.cent <- data.cent[data.cent$id != 152,]

data.cent.plu <- data.cent[data.cent$populism <3, ]
data.cent.pop <- data.cent[data.cent$populism >=3, ]
data.cent.p1 <- data.cent[data.cent$populism == 1, ]
data.cent.p2 <- data.cent[data.cent$populism == 2, ]
data.cent.p3 <- data.cent[data.cent$populism == 3, ]
data.cent.p4 <- data.cent[data.cent$populism == 4, ]

## DynEGA - Mean correlations

print('Running DynEGA models ...')

print('One model to rule them all')

dega.cent.all.0 <- dynEGA(
  data = data.cent[,c(1:6,8)],
  n.embed = 10,
  tau = 1,
  delta = 1,
  level = 'population',
  id = 7,
  use.derivatives = 0,
  ncores = 10,
  progress = TRUE
)

print("Pluralist model")

dega.cent.plu.0 <- dynEGA(
  data = data.cent.plu[,c(1:6,8)],
  n.embed = 10,
  tau = 1,
  delta = 1,
  level = 'population',
  id = 7,
  use.derivatives = 0,
  ncores = 10,
  progress = TRUE
)

dega.cent.pop.0 <- dynEGA(
  data = data.cent.pop[,c(1:6,8)],
  n.embed = 10,
  tau = 1,
  delta = 1,
  level = 'population',
  id = 7,
  use.derivatives = 0,
  ncores = 10,
  progress = TRUE
)

dega.cent.p1.0 <- dynEGA(
  data = data.cent.p1[,c(1:6,8)],
  n.embed = 10,
  tau = 1,
  delta = 1,
  level = 'population',
  id = 7,
  use.derivatives = 0,
  ncores = 10,
  progress = TRUE
)

dega.cent.p2.0 <- dynEGA(
  data = data.cent.p2[,c(1:6,8)],
  n.embed = 10,
  tau = 1,
  delta = 1,
  level = 'population',
  id = 7,
  use.derivatives = 0,
  ncores = 10,
  progress = TRUE
)

dega.cent.p3.0 <- dynEGA(
  data = data.cent.p3[,c(1:6,8)],
  n.embed = 10,
  tau = 1,
  delta = 1,
  level = 'population',
  id = 7,
  use.derivatives = 0,
  ncores = 10,
  progress = TRUE
)

dega.cent.p4.0 <- dynEGA(
  data = data.cent.p4[,c(1:6,8)],
  n.embed = 10,
  tau = 1,
  delta = 1,
  level = 'population',
  id = 7,
  use.derivatives = 0,
  ncores = 10,
  progress = TRUE
)

print('DynEGA All derivatives=0')
net.cent.all.0 <- dega.cent.all.0$dynEGA$network
write.csv(as.data.frame(net.cent.all.0), file = "../results/net.cent.all.0.csv", row.names = FALSE)

png(filename = '../figures/dega.cent.all.0.png', width = 800, height = 800)
plot(dega.cent.all.0)
dev.off()


print('DynEGA P1 derivatives=0')
net.cent.p1.0 <- dega.cent.p1.0$dynEGA$network
write.csv(as.data.frame(net.cent.p1.0), file = "../results/net.cent.p1.0.csv", row.names = FALSE)

png(filename = '../figures/dega.cent.p1.0.png', width = 800, height = 800)
plot(dega.cent.p1.0)
dev.off()

print('Done!')

print('DynEGA P2 derivatives=0')

net.cent.p2.0 <- dega.cent.p2.0$dynEGA$network
write.csv(as.data.frame(net.cent.p2.0), file = "../results/net.cent.p2.0.csv", row.names = FALSE)

png(filename = '../figures/dega.cent.p2.0.png', width = 800, height = 800)
plot(dega.cent.p2.0)
dev.off()

print('Done!')

print('DynEGA P3 derivatives=0')

net.cent.p3.0 <- dega.cent.p3.0$dynEGA$network
write.csv(as.data.frame(net.cent.p3.0), file = "../results/net.cent.p3.0.csv", row.names = FALSE)

png(filename = '../figures/dega.cent.p3.0.png', width = 800, height = 800)
plot(dega.cent.p3.0)
dev.off()

print('Done!')

print('DynEGA P4 derivatives=0')

net.cent.p4.0 <- dega.cent.p4.0$dynEGA$network
write.csv(as.data.frame(net.cent.p4.0), file = "../results/net.cent.p4.0.csv", row.names = FALSE)

png(filename = '../figures/dega.cent.p4.0.png', width = 800, height = 800)
plot(dega.cent.p4.0)
dev.off()

print('Done!')

print('DynEGA Pluralist derivatives=0')

net.cent.plu.0 <- dega.cent.plu.0$dynEGA$network
write.csv(as.data.frame(net.cent.plu.0), file = "../results/net.cent.plu.0.csv", row.names = FALSE)

png(filename = '../figures/dega.cent.plu.0.png', width = 800, height = 800)
plot(dega.cent.plu.0)
dev.off()

print('Done!')

print('DynEGA Populist derivatives=0')

net.cent.pop.0 <- dega.cent.pop.0$dynEGA$network
write.csv(as.data.frame(net.cent.pop.0), file = "../results/net.cent.pop.0.csv", row.names = FALSE)

png(filename = '../figures/dega.cent.pop.0.png', width = 800, height = 800)
plot(dega.cent.pop.0)
dev.off()

print('Done!')

## DynEGA - Rate of change correlations

print('Running DynEGA models ...')

print('One model to rule them all')

dega.cent.all.1 <- dynEGA(
  data = data.cent[,c(1:6,8)],
  n.embed = 10,
  tau = 1,
  delta = 1,
  level = 'population',
  id = 7,
  use.derivatives = 1,
  ncores = 10,
  progress = TRUE
)

print("Pluralist model")

dega.cent.plu.1 <- dynEGA(
  data = data.cent.plu[,c(1:6,8)],
  n.embed = 10,
  tau = 1,
  delta = 1,
  level = 'population',
  id = 7,
  use.derivatives = 1,
  ncores = 10,
  progress = TRUE
)

dega.cent.pop.1 <- dynEGA(
  data = data.cent.pop[,c(1:6,8)],
  n.embed = 10,
  tau = 1,
  delta = 1,
  level = 'population',
  id = 7,
  use.derivatives = 1,
  ncores = 10,
  progress = TRUE
)

dega.cent.p1.1 <- dynEGA(
  data = data.cent.p1[,c(1:6,8)],
  n.embed = 10,
  tau = 1,
  delta = 1,
  level = 'population',
  id = 7,
  use.derivatives = 1,
  ncores = 10,
  progress = TRUE
)

dega.cent.p2.1 <- dynEGA(
  data = data.cent.p2[,c(1:6,8)],
  n.embed = 10,
  tau = 1,
  delta = 1,
  level = 'population',
  id = 7,
  use.derivatives = 1,
  ncores = 10,
  progress = TRUE
)

dega.cent.p3.1 <- dynEGA(
  data = data.cent.p3[,c(1:6,8)],
  n.embed = 10,
  tau = 1,
  delta = 1,
  level = 'population',
  id = 7,
  use.derivatives = 1,
  ncores = 10,
  progress = TRUE
)

dega.cent.p4.1 <- dynEGA(
  data = data.cent.p4[,c(1:6,8)],
  n.embed = 10,
  tau = 1,
  delta = 1,
  level = 'population',
  id = 7,
  use.derivatives = 1,
  ncores = 10,
  progress = TRUE
)

print('DynEGA All derivatives=1')
net.cent.all.1 <- dega.cent.all.1$dynEGA$network
write.csv(as.data.frame(net.cent.all.1), file = "../results/net.cent.all.1.csv", row.names = FALSE)

png(filename = '../figures/dega.cent.all.1.png', width = 800, height = 800)
plot(dega.cent.all.1)
dev.off()


print('DynEGA P1 derivatives=1')
net.cent.p1.1 <- dega.cent.p1.1$dynEGA$network
write.csv(as.data.frame(net.cent.p1.1), file = "../results/net.cent.p1.1.csv", row.names = FALSE)

png(filename = '../figures/dega.cent.p1.1.png', width = 800, height = 800)
plot(dega.cent.p1.1)
dev.off()

print('Done!')

print('DynEGA P2 derivatives=1')

net.cent.p2.1 <- dega.cent.p2.1$dynEGA$network
write.csv(as.data.frame(net.cent.p2.1), file = "../results/net.cent.p2.1.csv", row.names = FALSE)

png(filename = '../figures/dega.cent.p2.1.png', width = 800, height = 800)
plot(dega.cent.p2.1)
dev.off()

print('Done!')

print('DynEGA P3 derivatives=1')

net.cent.p3.1 <- dega.cent.p3.1$dynEGA$network
write.csv(as.data.frame(net.cent.p3.1), file = "../results/net.cent.p3.1.csv", row.names = FALSE)

png(filename = '../figures/dega.cent.p3.1.png', width = 800, height = 800)
plot(dega.cent.p3.1)
dev.off()

print('Done!')

print('DynEGA P4 derivatives=1')

net.cent.p4.1 <- dega.cent.p4.1$dynEGA$network
write.csv(as.data.frame(net.cent.p4.1), file = "../results/net.cent.p4.1.csv", row.names = FALSE)

png(filename = '../figures/dega.cent.p4.1.png', width = 800, height = 800)
plot(dega.cent.p4.1)
dev.off()

print('Done!')

print('DynEGA Pluralist derivatives=1')

net.cent.plu.1 <- dega.cent.plu.1$dynEGA$network
write.csv(as.data.frame(net.cent.plu.1), file = "../results/net.cent.plu.1.csv", row.names = FALSE)

png(filename = '../figures/dega.cent.plu.1.png', width = 800, height = 800)
plot(dega.cent.plu.1)
dev.off()

print('Done!')

print('DynEGA Populist derivatives=1')

net.cent.pop.1 <- dega.cent.pop.1$dynEGA$network
write.csv(as.data.frame(net.cent.pop.1), file = "../results/net.cent.pop.1.csv", row.names = FALSE)

png(filename = '../figures/dega.cent.pop.1.png', width = 800, height = 800)
plot(dega.cent.pop.1)
dev.off()

print('Done!')

#### Bootstrap Ergodicity


