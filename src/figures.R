library(qgraph)

net.all.0.am <- read.csv("../results/net.add.all.0.csv")
net.pop.0.am <- read.csv("../results/net.add.pop.0.csv")
net.plu.0.am <- read.csv("../results/net.add.plu.0.csv")
net.all.1.am <- read.csv("../results/net.add.all.1.csv")
net.all.0.cent.am <- read.csv("../results/net.cent.all.0.csv")
net.all.1.cent.am <- read.csv("../results/net.cent.all.1.csv")

load("../results/layout.favorite.RData")

emo.names <- c("Anger", "Disgust", "Fear","Happiness","Sadness","Surprise")

png("../figures/net-1.png", width = 3300, height = 3300, res = 300)
net.all.0 <- qgraph(net.all.0.am, layout = layout.favorite, labels = emo.names, 
                    label.cex = 1.2, label.scale = FALSE, 
                    label.color = "black", 
                    label.font = 2, colFactor = 1.5, theme = 'colorblind', minimum = 0.05)
dev.off()

png("../figures/net-2-a.png", width = 3300, height = 3300, res = 300)
net.pop.0 <- qgraph(net.pop.0.am, layout = layout.favorite, labels = emo.names, 
                    label.cex = 1.2, label.scale = FALSE, 
                    label.color = "black", 
                    label.font = 2, colFactor = 1.5, min = 0.05, theme = 'colorblind', title = "Populist leaders", title.cex = 3, title.font = 2)
dev.off()

png("../figures/net-2-b.png", width = 3300, height = 3300, res = 300)
net.plu.0 <- qgraph(net.plu.0.am, layout = layout.favorite, labels = emo.names, 
                    label.cex = 1.2, label.scale = FALSE, 
                    label.color = "black", 
                    label.font = 2, colFactor = 1.5, min = 0.05, theme = 'colorblind', title = "Pluralist leaders", title.cex = 3, title.font = 2)
dev.off()

png("../figures/net-3.png", width = 3300, height = 3300, res = 300)
net.all.1 <- qgraph(net.all.1.am, layout = layout.favorite, labels = emo.names, 
                    label.cex = 1.2, label.scale = FALSE, 
                    label.color = "black", 
                    label.font = 2, colFactor = 1.5, min = 0.05, theme = 'colorblind')
dev.off()

centrality(graph = net.all.0)

strengths <- as.data.frame(matrix(0, nrow = 6, ncol = 4))
colnames(strengths) <- c("all.0", "pop.0", "plu.0", "all.1")
rownames(strengths) <- emo.names

strengths$all.0 <- centrality(graph = net.all.0)$InDegree
strengths$pop.0 <- centrality(graph = net.pop.0)$InDegree
strengths$plu.0 <- centrality(graph = net.plu.0)$InDegree
strengths$all.1 <- centrality(graph = net.all.1)$InDegree

png("../figures/strengths.png", width = 4000, height = 3000,res=300)
plot(NULL, xlim = c(0, max(strengths)+0.2), ylim = c(1, 6), xlab = "", ylab = "", axes = FALSE)
axis(1, las = 1)
axis(2, at = c(6:1), labels = emo.names)
points(x = strengths[1,], y = rep(6,4), col = c("black","#009E73","#D55E00","#CC79A7"), pch = 21, bg = c("black","#009E73","#D55E00","#CC79A7"), cex = 3)
points(x = strengths[2,], y = rep(5,4), col = c("black","#009E73","#D55E00","#CC79A7"), pch = 21, bg = c("black","#009E73","#D55E00","#CC79A7"), cex = 3)
points(x = strengths[3,], y = rep(4,4), col = c("black","#009E73","#D55E00","#CC79A7"), pch = 21, bg = c("black","#009E73","#D55E00","#CC79A7"), cex = 3)
points(x = strengths[4,], y = rep(3,4), col = c("black","#009E73","#D55E00","#CC79A7"), pch = 21, bg = c("black","#009E73","#D55E00","#CC79A7"), cex = 3)
points(x = strengths[5,], y = rep(2,4), col = c("black","#009E73","#D55E00","#CC79A7"), pch = 21, bg = c("black","#009E73","#D55E00","#CC79A7"), cex = 3)
points(x = strengths[6,], y = rep(1,4), col = c("black","#009E73","#D55E00","#CC79A7"), pch = 21, bg = c("black","#009E73","#D55E00","#CC79A7"), cex = 3)

legend("topright", legend = c("Sample, scores", "Populists, scores", "Pluralists, scores", "Sample, rate"), col = c("black","#009E73","#D55E00","#CC79A7"), pch = 21, pt.bg = c("black","#009E73","#D55E00","#CC79A7"), cex = 1.2)

dev.off()


png("../figures/net-1.png", width = 3300, height = 3300, res = 300)
net.all.0 <- qgraph(net.all.0.am, layout = layout.favorite, labels = emo.names, 
                    label.cex = 1.2, label.scale = FALSE, 
                    label.color = "black", 
                    label.font = 2, colFactor = 1.5, theme = 'colorblind', minimum = 0.05)
dev.off()

png("../figures/net-cent-0.png", width = 3300, height = 3300, res = 300)
net.all.0 <- qgraph(net.all.0.cent.am, layout = layout.favorite, labels = emo.names, 
                    label.cex = 1.2, label.scale = FALSE, 
                    label.color = "black", 
                    label.font = 2, colFactor = 1.5, theme = 'colorblind', minimum = 0.05)
dev.off()

png("../figures/net-cent-1.png", width = 3300, height = 3300, res = 300)
net.all.1 <- qgraph(net.all.1.cent.am, layout = layout.favorite, labels = emo.names, 
                    label.cex = 1.2, label.scale = FALSE, 
                    label.color = "black", 
                    label.font = 2, colFactor = 1.5, theme = 'colorblind', minimum = 0.05)
dev.off()
