# install.packages('Metrics')
# install.packages('testthat')
# install.packages('checkmate')
# install.packages('logger')
# install.packages('huge')
# install.packages('peakRAM')
# install.packages('MASS')


setwd("/home/philko/Documents/Uni/SoSe23/SparseAndNetwork")

library(dplyr)
library(logger)
library(huge)
library(peakRAM)
library(tidyr)

source("./utils.R")
source("./MB_DGF.R")

# Set Parameter

DEBUG <- T

if(DEBUG){
  hyperparams <- list(
    n = 100,
    dimensions = c(750, 100, 150, 200, 250, 300),
    methods = c('glasso'),
    selectors = c('stars'),
    seeds = c(123),
    graphs = c("cluster")
  )
} else {
  n <- 100
  dimensions <- c(100, 250, 500, 750, 1000) #, 2000)#, 3000)
  methods <- c('glasso', 'mb') 
  selectors <- c('stars', 'ebic')
  # seeds <- c(123, 42, 198, 984, 214)
  seeds <- c(42, 198, 984) #, 214)
  graphs <-c("hub", "cluster", "band", "scale-free", "MB")
}


# Set-up Tracking DF
df_len <- length(hyperparams$dimensions) * length(hyperparams$methods) * length(hyperparams$selectors) * length(hyperparams$seeds) * length(hyperparams$graphs)
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
for(seed in hyperparams$seeds){
  set.seed(seed)
  for(graph in hyperparams$graphs){
    for(d in hyperparams$dimensions) {
      for(method in hyperparams$methods) {
        for(selector in hyperparams$selectors){
          log_info('\'-------> {counter}/{df_len}')
          log_info('Current: Seed: {seed}, Dimension: {d}, Method: {method}, Selector: {selector}, Graph: {graph}.')
          if(method == 'mb' & selector == 'ebic') {
            log_info("Incompatible combination. Proceeding.")
            counter <- counter + 1
            next
          }
          if(graph !=  "MB") {
            gen_data <- huge.generator(
              n = hyperparams$n,
              d = d,
              g = ceiling(d / 20),
              graph = graph,
            )
          } else {
            gen_data <- mb_gen(
              n = hyperparams$n,
              d = d
            )
          }
          
          # TODO: Track time for one estimation?
          
          # Model Computation
          start_time <- Sys.time()
          model <- huge(
            x = gen_data$data,
            method = method,
            sym = 'or')
          end_time <- Sys.time()
          model_time <- end_time - start_time
          gc(reset = TRUE, full = TRUE)
          
          # res_model <- peakRAM({})
          # res_best <- peakRAM({})
          
          # Model Selection
          start_time <- Sys.time()
          best <- huge.select(model, criterion = selector)
          end_time <- Sys.time()
          model_sel_time <- end_time - start_time
          gc(reset = TRUE, full = TRUE)
          
          metrics <- get_metrics(
            as(best$refit, "dgCMatrix"),
            as(gen_data$theta, "dsCMatrix"))
          gc(reset = TRUE, full = TRUE)
          
          # Track
          tracking[counter, ]$seed <- seed
          tracking[counter, ]$graph <- graph
          tracking[counter, ]$method <- method
          tracking[counter, ]$dim <- d
          tracking[counter, ]$selector <- selector
          tracking[counter, ]$elapsed_time_model <- model_time # res_model$Elapsed_Time_sec
          # tracking[counter, ]$total_ram_model <- res_model$Total_RAM_Used_MiB
          # tracking[counter, ]$peak_ram_model <- res_model$Peak_RAM_Used_MiB
          tracking[counter, ]$elapsed_time_best <- model_sel_time # res_best$Elapsed_Time_sec
          # tracking[counter, ]$total_ram_best <- res_best$Total_RAM_Used_MiB
          # tracking[counter, ]$peak_ram_best <- res_best$Peak_RAM_Used_MiB
          tracking[counter, ]$f1 <- metrics$f1
          tracking[counter, ]$precision <- metrics$precision
          tracking[counter, ]$recall <- metrics$recall
          tracking[counter, ]$hamming_dist <- metrics$hamming_dist
          tracking[counter, ]$best <- 0
          
          if (file.exists("tmp.csv")) {
            file.remove("tmp.csv")
          }
          write.csv(tracking, "tmp.csv", )
          
          counter <- counter + 1
          rm(best, model, metrics)
          gc(reset = TRUE, full = TRUE)
          # rm(gen_data, model, best, metrics)
          # env_list <- ls()
          # rm(list = env_list[!(env_list %in% c(
          #   'hyperparams', 'counter',
          #   'tracking', 'df_len',
          #   'seed', 'graph',
          #   'd', 'method',
          #   'selector'))])
          # gc(reset = TRUE)
        }
      }
    }
  }
}
# tracking <- drop_na(tracking)
write.csv(tracking, paste("tracking", Sys.time(), ".csv"))
unlink("tmp.csv")


