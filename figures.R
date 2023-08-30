set.seed(123)

setwd("/home/philko/Documents/Uni/SoSe23/SparseAndNetwork")

source("./MB_DGF.R")

library(huge)

png("./figures/hub_fig.png")
data <- huge.generator(n = 100, p = 500, graph = 'hub')
plot(data)
dev.off()

png("./figures/cluster_fig.png")
data <- huge.generator(n = 100, g = 5, prob = 0.7, graph = 'cluster', verbose = T)
plot(data)
dev.off()

png("./figures/band_fig.png")
data <- huge.generator(n = 100, p = 500, graph = 'band')
plot(data)
dev.off()

png("./figures/scale_free_fig.png")
data <- huge.generator(n = 100, p = 500, graph = 'scale-free')
plot(data)
dev.off()

png("./figures/m_and_b_fig.png")
data <- mb_gen(n = 100, d = 500)
plot(data)
dev.off()

png("./figures/random_fig.png")
data <- huge.generator(n = 100, p = 500, graph = 'random')
plot(data)
dev.off()

data <- huge.generator(n = 100, p = 500, graph = 'scale-free')
huge.plot(
  data$theta,
  epsflag = TRUE,
  graph.name = "./figures/ex_fig",
  location = getwd())