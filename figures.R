# Script for generating figures of the seminar-paper.
#
# Seminar: Network Learning and Sparse Estimation
# Institute for Statistics
# LMU Munich
#
# Philipp Koch, 2023

setwd("/home/philko/Documents/Uni/SoSe23/SparseAndNetwork")

library(huge)

png("hub_fig.png")
data <- huge.generator(n = 100, p = 500, graph = 'hub')
plot(data)
dev.off()

png("cluster_fig.png")
data <- huge.generator(n = 100, g = 5, prob = 0.7, graph = 'cluster', verbose = T)
plot(data)
dev.off()

png("band_fig.png")
data <- huge.generator(n = 100, p = 500, graph = 'band')
plot(data)
dev.off()

png("scale_free_fig.png")
data <- huge.generator(n = 100, p = 500, graph = 'scale-free')
plot(data)
dev.off()
