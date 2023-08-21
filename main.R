# Script for the report
# 
# Neighborhood Selection as Covariance Selection - Revisiting Meinshausen and
# Bühlmann's Approach
#
# For the seminar "Network Learning and Sparse Estimation"
#
# Philipp Koch, 2023

setwd("/home/philko/Documents/Uni/SoSe23/SparseAndNetwork")

library(dplyr)
library(logger)
library(huge)
library(peakRAM)
library(tidyr)

source("./utils.R")

# Set Parameter

DEBUG <- T

if(DEBUG){
  n <- 1000
  dimensions <- seq(10, 20, by = 10)
  methods <- c('mb')
  selectors <- c('stars', 'ebic')
  seeds <- c(123)
  graphs <- c("random")
} else {
  n <- 1000
  dimensions <- seq(100, 1000, by = 100)
  methods < c('mb', 'glasso') 
  selectors <- c('stars', 'ric', 'ebic')
  seeds <- c(123, 42, 198, 984, 214)
  graphs <- c("hub", "cluster", "band", "scale-free", "MB")
}


# Set-up Tracking DF
df_len <- length(dimensions) * length(methods) * length(selectors) * length(seeds)
tracking <- data.frame(
  seed = rep(NA, df_len),
  graph = rep(NA, df_len),
  dim = rep(NA, df_len),
  method = rep(NA, df_len),
  selector = rep(NA, df_len),
  elapsed_time_model = rep(NA, df_len),
  total_ram_model = rep(NA, df_len),
  peak_ram_model = rep(NA, df_len),
  elapsed_time_best = rep(NA, df_len),
  total_ram_best = rep(NA, df_len),
  peak_ram_best = rep(NA, df_len),
  f1 = rep(NA, df_len),
  precision = rep(NA, df_len),
  recall = rep(NA, df_len),
  hamming_dist = rep(NA, df_len),
  best = rep(NA, df_len)
)
counter <- 1

# Loop to Produce different Data Scenarios
graph <- 'random'
for(seed in seeds){
  set.seed(seed)
  for(graph in graphs){
    for(d in dimensions) {
      for(method in methods) {
        for(selector in selectors){
          log_info('Current: Seed: {seed}, Dimension: {d}, Method: {method}, Selector: {selector}.')
          if(method == 'mb' & selector == 'ebic') {
            log_info("Incompatible combination. Proceeding.")
            counter <- counter + 1
            next
          }
          if(graph !=  "MB") {
            gen_data <- huge.generator(
              n = n,
              d = d,
              graph = graph,
            )
          } else {
            stop("TODO")
            gen_data <- NA  # TODO
          }
          
          # TODO: Track time for one estimation?
          
          res_model <- peakRAM({model <- huge(
            x = gen_data$data,
            lambda = rev(seq(0.01, 0.03, by = 0.01)),
            method = method,
            sym = 'and'
          )})
          res_best <- peakRAM({best <- huge.select(model, criterion = selector)})
          
          metrics <- get_metrics(
            as(best$refit, "dgCMatrix"),
            as(gen_data$theta, "dsCMatrix"))
          
          # Track
          tracking[counter, ]$seed <- seed
          tracking[counter, ]$graph <- graph
          tracking[counter, ]$method <- method
          tracking[counter, ]$dim <- d
          tracking[counter, ]$selector <- selector
          tracking[counter, ]$elapsed_time_model <- res_model$Elapsed_Time_sec
          tracking[counter, ]$total_ram_model <- res_model$Total_RAM_Used_MiB
          tracking[counter, ]$peak_ram_model <- res_model$Peak_RAM_Used_MiB
          tracking[counter, ]$elapsed_time_best <- res_best$Elapsed_Time_sec
          tracking[counter, ]$total_ram_best <- res_best$Total_RAM_Used_MiB
          tracking[counter, ]$peak_ram_best <- res_best$Peak_RAM_Used_MiB
          tracking[counter, ]$f1 <- metrics$f1
          tracking[counter, ]$precision <- metrics$precision
          tracking[counter, ]$recall <- metrics$recall
          tracking[counter, ]$hamming_dist <- metrics$hamming_dist
          tracking[counter, ]$best <- 0
          
          
          counter <- counter + 1
        }
      }
    }
  }
}
tracking <- drop_na(tracking)


