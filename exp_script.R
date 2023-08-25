library(huge)
library(logger)
library(peakRAM)

source("./utils.R")
source("./MB_DGF.R")

options(echo=TRUE)
args <- commandArgs(trailingOnly = TRUE)

# 
n <- as.numeric(args[1])
dims <- as.numeric(unlist(strsplit(args[2], ',')))
graph <- args[3]
method <- args[4]
selector <- args[5]
tracking_file <- args[6]
seed <- args[7]
set.seed(seed)


if(method != 'mb' | selector != 'ebic') {
  df_len <- length(dims)
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
  print(dims)

  counter <- 1

  for (d in dims){
    if(graph !=  "MB") {
      gen_data <- huge.generator(
        n = n,
        d = d,
        graph = graph,
      )
    } else {
      gen_data <- mb_gen(
        n = n,
        d = d
      )
    }
    
    # TODO: Track time for one estimation?
    
    # Model Computation
    res_model <- peakRAM({
      model <- huge(
        x = gen_data$data,
        method = method,
        sym = 'or')
      end_time <- Sys.time()
    })
    
    # Model Selection
    res_best <- peakRAM({
      best <- huge.select(model, criterion = selector)
    })
    
    metrics <- get_metrics(
      as(best$refit, "dgCMatrix"),
      as(gen_data$theta, "dsCMatrix"))
    
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

  # tracker <- read.csv(tracking_file)
  if (!file.exists(tracking_file)) {
    write.csv(tracking, tracking_file, row.names = FALSE)
  } else {
    curr <- read.csv(tracking_file)
    curr <- rbind(curr, tracking)
    write.csv(curr, tracking_file, row.names = FALSE) 
  }

}