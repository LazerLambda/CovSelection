# Script for Analysis of the obtained data.
# 
# Colors for labels taken from https://latexcolor.com/.
#
# Neighborhood Selection as Covariance Selection - Revisiting Meinshausen and
# Bühlmann's Approach.
#
# For the seminar "Network Learning and Sparse Estimation"
#
# Philipp Koch, 2023

library(data.table)
library(gridExtra)
library(dplyr)
library(ggplot2)


# PREPARE DATA

setwd("/home/philko/Documents/Uni/SoSe23/SparseAndNetwork")
data <- read.csv('./data/data.csv')
data$name <- apply( data[ , c('method', 'selector')] , 1 , paste , collapse = "-" )
data <- subset(data, select = -c(method, selector))

# PREPARE OUTPUT
folder_path <- "figures"
if (!file.exists(folder_path)) {
  dir.create(folder_path, recursive = TRUE)
  cat("Folder created:", folder_path, "\n")
} else {
  cat("Folder already exists:", folder_path, "\n")
}


# HELPER FUNCTIONS

data_grouped <- data %>%
  group_by(graph, dim, name) %>% 
  summarise_at(
    vars(
      "f1",
      "precision",
      "recall",
      "elapsed_time_model",
      "elapsed_time_best",
      "peak_ram_model",
      "peak_ram_best"),
    list(
      mean = mean,
      std = ~sqrt(var(.x))))
               
add_ci <- function(df, col) {
  new_col_max <- paste("ci", col, "max", sep = "_")
  new_col_min <- paste("ci", col, "min", sep = "_")
  
  mean_nm <- paste(col, "mean", sep = "_")
  std_nm <- paste(col, "std", sep = "_")
  
  df[new_col_max] <- data_grouped[mean_nm] + 1.96 * data_grouped[std_nm]
  df[new_col_min] <- data_grouped[mean_nm] - 1.96 * data_grouped[std_nm]
  return(df)
}

get_title <- function(graph_name) {
  if (graph_name == "hub") return("Hub-Graph")
  if (graph_name == "scale-free") return("Scale-Free-Graph")
  if (graph_name == "cluster") return("Cluster-Graph")
  if (graph_name == "band") return("Band-Graph")
  if (graph_name == "MB") return("MB-DGF-Graph")
}

plot_models <- function(df, col_name) {
  plot <- ggplot(data=df, aes_string(x="dim", y=paste(col_name, "mean", sep="_"), group="name")) +
    geom_line(aes(col = name)) +
    scale_color_manual(
      values = c(
        "glasso-stars" = "#5D8AA8", # air force blue
        "glasso-ebic" = "#E52B50", # amararanth
        "mb-stars" = "#ED872D")) + # cadmium orange
    geom_point() +
    ylab("") +
    xlab("") +
    geom_ribbon(
      data = data_tmp,
      aes_string(
        x = "dim",
        ymin = paste("ci", col_name, "min", sep="_"),
        ymax = paste("ci", col_name, "max", sep="_")),
      alpha = 0.3) +
    theme(legend.position = "none") +
    { if (col_name %in% c("f1", "precision", "recall")) {
      coord_cartesian(ylim = c(0, 1))
    } else {
      xlab("")}
    }
  return(plot)
}



# PLOT PERFORMANCE GRAPHS

data_grouped <- add_ci(data_grouped, "f1")
data_grouped <- add_ci(data_grouped, "precision")
data_grouped <- add_ci(data_grouped, "recall")

graphs <- unique(data$graph)
plots_perf <- list()
i <- 1
for (g in graphs) {
  data_tmp <- data_grouped %>%
    filter(graph == g)
  data_tmp <- na.omit(data_tmp)
  plot1 <- plot_models(data_tmp, "f1") +
    ggtitle("F1")

  plot2 <- plot_models(data_tmp, "precision") +
    ggtitle("Precision")

  plot3 <- plot_models(data_tmp, "recall") +
    ggtitle("Recall")

  plt <- grid.arrange(
    plot1, plot2, plot3,
    ncol = 3,
    top = get_title(g),
    bottom = "Dimensions (p)")
  ggsave(
    plt,
    file=paste("./figures/", g, "_perf", ".pdf", sep = ""),
    width = 22,
    height = 10,
    units = "cm")
  plots_perf[[i]] <- plt
  i <- i + 1
}



# PLOT ELAPSED TIME FOR MODEL AND MODEL-SELECTION

data_grouped <- add_ci(data_grouped, "elapsed_time_model")
data_grouped <- add_ci(data_grouped, "elapsed_time_best")

plots_time <- list()
i <- 1
for (g in graphs) {
  data_tmp <- data_grouped %>%
    filter(graph == g)
  data_tmp <- na.omit(data_tmp)
  plot1 <- plot_models(data_tmp, "elapsed_time_model") +
    ylab("Time (in Seconds)") +
    ggtitle("Model Estimation")
  plot2 <- plot_models(data_tmp, "elapsed_time_best") +
    ggtitle("Model Selection")

  plt <- grid.arrange(
    plot1,
    plot2,
    ncol = 2,
    top = get_title(g),
    bottom = "Dimensions (p)")
  ggsave(
    plt,
    file=paste("./figures/", g, "_time", ".pdf", sep = ""),
    width = 22,
    height = 10,
    units = "cm")
  plots_time[[i]] <- plt
  i <- i + 1
}



# PLOT RAM USAGE

data_grouped <- add_ci(data_grouped, "peak_ram_model")
data_grouped <- add_ci(data_grouped, "peak_ram_best")

plots_ram <- list()
i <- 1
for (g in graphs) {
  data_tmp <- data_grouped %>%
    filter(graph == g)
  data_tmp <- na.omit(data_tmp)
  plot1 <- plot_models(data_tmp, "peak_ram_model") +
    ylab("Memory (in MiB)") +
    ggtitle("Model Estimation")
  plot2 <- plot_models(data_tmp, "peak_ram_best") +
    ggtitle("Model Selection")


  plt <- grid.arrange(
    plot1,
    plot2,
    ncol = 2,
    top = get_title(g),
    bottom = "Dimensions (p)")
  ggsave(
    plt,
    file=paste("./figures/", g, "_memory", ".pdf", sep = ""),
    width = 22,
    height = 10,
    units = "cm")
  plots_ram[[i]] <- plt
  i <- i + 1
}



# GET TABLES FOR EACH GRAPH
# data_grouped[data_grouped$graph == "MB", ][c("name", "dim", "f1_mean", "precision_mean", "recall_mean")]
print_table <- function(df, graph, cols){
  with_mean <- unname(sapply(cols, function(e) paste(e, "mean", sep = "_")))
  cols <- c("name", "dim", with_mean)
  lapply(
    c("mb-stars", "glasso-stars", "glasso-ebic"),
    function(n) {
      data_grouped[
        data_grouped$graph == graph & data_grouped$name == n, ][cols]})
}