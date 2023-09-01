library(data.table)
library(gridExtra)
library(dplyr)
library(ggplot2)


# PREPARE DATA

# install.packages('Metrics')
# install.packages('testthat')
# install.packages('checkmate')
# install.packages('logger')
# install.packages('huge')
# install.packages('peakRAM')
# install.packages('MASS')

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
      "peak_ram_best",
      "hamming_dist"),
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

get_title <- function(graph_name, short = F) {
  if (graph_name == "hub" & !short) return("Hub-Graph")
  if (graph_name == "scale-free" & !short) return("Scale-Free-Graph")
  if (graph_name == "cluster" & !short) return("Cluster-Graph")
  if (graph_name == "band" & !short) return("Band-Graph")
  if (graph_name == "MB" & !short) return("MB-DGP-Graph")
  
  if (graph_name == "hub" & short) return("Hub")
  if (graph_name == "scale-free" & short) return("Scale-Free")
  if (graph_name == "cluster" & short) return("Cluster")
  if (graph_name == "band" & short) return("Band")
  if (graph_name == "MB" & short) return("MB-DGP")
}

plot_models <- function(df, col_name, df_unsorted = NULL) {
  plot <- ggplot(data=df, aes_string(x="dim", y=paste(col_name, "mean", sep="_"), group="name", fill = "name") ) +
    geom_line(aes(col = name)) +
    scale_color_manual(
      values = c(
        "glasso-stars" = "#5D8AA8", # air force blue
        "glasso-ebic" = "#E52B50", # amararanth
        "mb-stars" = "#ED872D")) + # cadmium orange
    scale_fill_manual(
      values = c(
        "glasso-stars" = "#5D8AA8", # air force blue
        "glasso-ebic" = "#E52B50", # amararanth
        "mb-stars" = "#ED872D")) + # cadmium orange
    # {if(is.null(df_unsorted)) geom_point() + }
    ylab("") +
    xlab("") +
    geom_ribbon(
      data = data_tmp,
      aes_string(
        x = "dim",
        ymin = paste("ci", col_name, "min", sep="_"),
        ymax = paste("ci", col_name, "max", sep="_")),
      linetype = 3,
      alpha = 0.3) +
    theme(legend.position = "none") +
    xlim(0, 1000) +
    { if (col_name %in% c("f1", "precision", "recall")) {
      coord_cartesian(ylim = c(0, 1))
    } else {
      xlab("")}
    }
  if (!is.null(df_unsorted)) {
    plot <- plot +
      geom_point(
        data=df_unsorted,
        aes_string(
          x="dim",
          y=col_name,
          group="name",
          col="name"))
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
  data_uns <- data %>%
    filter(graph == g)
  data_uns <- na.omit(data_uns)
  plot1 <- plot_models(data_tmp, "f1", data_uns) +
    ggtitle("F1")

  plot2 <- plot_models(data_tmp, "precision", data_uns) +
    ggtitle("Precision")

  plot3 <- plot_models(data_tmp, "recall", data_uns) +
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
    height = 8,
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
  data_uns <- data %>%
    filter(graph == g)
  data_uns <- na.omit(data_uns)
  plot1 <- plot_models(data_tmp, "elapsed_time_model", data_uns) +
    ylab("Time (in Seconds)") +
    ggtitle("Model Estimation")
  plot2 <- plot_models(data_tmp[!(data_tmp$name == "glasso-ebic"), ], "elapsed_time_best", data_uns) +
    ggtitle("Model Selection")

  plt <- grid.arrange(
    plot1,
    plot2,
    ncol = 2,
    top = get_title(g),
    bottom = "Dimension (p)")
  ggsave(
    plt,
    file=paste("./figures/", g, "_time", ".pdf", sep = ""),
    width = 22,
    height = 10,
    units = "cm")
  plots_time[[i]] <- plt
  i <- i + 1
}


plots_time_mod <- list()
plots_time_bst <- list()
i <- 1
for (g in graphs) {
  data_tmp <- data_grouped %>%
    filter(graph == g)
  data_tmp <- na.omit(data_tmp)
  data_uns <- data %>%
    filter(graph == g)
  data_uns <- na.omit(data_uns)
  plot1 <- plot_models(data_tmp, "elapsed_time_model", data_uns) + ggtitle(get_title(g, T)) + coord_cartesian(ylim = c(0, 80))
  plot2 <- plot_models(data_tmp[!(data_tmp$name == "glasso-ebic"), ], "elapsed_time_best", data_uns) + coord_cartesian(ylim = c(0, 1500)) + ggtitle("")
  
  plots_time_mod[[i]] <- plot1
  plots_time_bst[[i]] <- plot2
  i <- i + 1
}

plt_time <- do.call("grid.arrange", c(plots_time_mod, plots_time_bst, nrow=2, ncol=5, top="Elapsed Time for Model Estimation and Selection", bottom="Dimension (p)", left="Elapsed Time (in Seconds)"))
ggsave(
  plt_time,
  file=paste("./figures/", "elapsed_time.pdf", sep = ""),
  width = 25,
  height = 12.5,
  units = "cm")



# PLOT RAM USAGE

data_grouped <- add_ci(data_grouped, "peak_ram_model")
data_grouped <- add_ci(data_grouped, "peak_ram_best")

plots_ram <- list()
i <- 1
for (g in graphs) {
  data_tmp <- data_grouped %>%
    filter(graph == g)
  data_tmp <- na.omit(data_tmp)
  data_uns <- data %>%
    filter(graph == g)
  data_uns <- na.omit(data_uns)
  plot1 <- plot_models(data_tmp, "peak_ram_model", data_uns) +
    ylab("Memory (in MiB)") +
    ggtitle("Model Estimation")
  plot2 <- plot_models(
    data_tmp[!(data_tmp$name == "glasso-ebic"), ],
    "peak_ram_best",
    data_uns[!(data_uns$name == "glasso-ebic"), ]) +
    ggtitle("Model Selection")


  plt <- grid.arrange(
    plot1,
    plot2,
    ncol = 2,
    top = get_title(g),
    bottom = "Dimension (p)")
  ggsave(
    plt,
    file=paste("./figures/", g, "_memory", ".pdf", sep = ""),
    width = 22,
    height = 10,
    units = "cm")
  plots_ram[[i]] <- plt
  i <- i + 1
}

plots_mem_mod <- list()
plots_mem_bst <- list()
i <- 1
for (g in graphs) {
  data_tmp <- data_grouped %>%
    filter(graph == g)
  data_tmp <- na.omit(data_tmp)
  data_uns <- data %>%
    filter(graph == g)
  data_uns <- na.omit(data_uns)
  plot1 <- plot_models(data_tmp, "peak_ram_model", data_uns) + ggtitle(get_title(g, T))
  plot2 <- plot_models(
    data_tmp[!(data_tmp$name == "glasso-ebic"), ],
    "peak_ram_best",
    data_uns[!(data_uns$name == "glasso-ebic"), ]) +
    ggtitle("")
  
  plots_mem_mod[[i]] <- plot1
  plots_mem_bst[[i]] <- plot2
  i <- i + 1
}

plt_mem <- do.call("grid.arrange", c(plots_mem_mod, plots_mem_bst, nrow=2, ncol=5, top="Memory Consumption for Model Estimation and Selection", bottom="Dimension (p)", left="Memory (in MiB)"))
ggsave(
  plt_mem,
  file=paste("./figures/", "memory.pdf", sep = ""),
  width = 22,
  height = 10,
  units = "cm")


# PLOT HAMMING DIST


data_grouped <- add_ci(data_grouped, "hamming_dist")


g <- "hub"
data_tmp <- data_grouped %>%
  filter(graph == g)
data_tmp <- na.omit(data_tmp)
data_uns <- data %>%
  filter(graph == g)
data_uns <- na.omit(data_uns)
plot1 <- plot_models(data_tmp, "hamming_dist", data_uns) +
  # ylab("Hamming Distance") +
  coord_cartesian(ylim = c(0, 17500)) +
ggtitle(get_title(g))

g <- "scale-free"
data_tmp <- data_grouped %>%
  filter(graph == g)
data_tmp <- na.omit(data_tmp)
data_uns <- data %>%
  filter(graph == g)
data_uns <- na.omit(data_uns)
plot2 <- plot_models(data_tmp, "hamming_dist", data_uns) +
  # ylab("Hamming Distance") +
  coord_cartesian(ylim = c(0, 110000)) +
  ggtitle(get_title(g))

plt <- grid.arrange(
  plot1,
  plot2,
  ncol = 2,
  left = "Hamming Distance",
  top = "Hamming Distance",
  bottom = "Dimension (p)")

ggsave(
  plt,
  file=paste("./figures/","hub+scale-free_ham", ".pdf", sep = ""),
  width = 22,
  height = 10,
  units = "cm")



g <- "band"
data_tmp <- data_grouped %>%
  filter(graph == g)
data_tmp <- na.omit(data_tmp)
data_uns <- data %>%
  filter(graph == g)
data_uns <- na.omit(data_uns)
plot1 <- plot_models(data_tmp, "hamming_dist", data_uns) +
  # ylab("Hamming Distance") +
  coord_cartesian(ylim = c(0, 20000)) +
  ggtitle(get_title(g))

g <- "cluster"
data_tmp <- data_grouped %>%
  filter(graph == g)
data_tmp <- na.omit(data_tmp)
data_uns <- data %>%
  filter(graph == g)
data_uns <- na.omit(data_uns)
plot2 <- plot_models(data_tmp, "hamming_dist", data_uns) +
  # ylab("Hamming Distance") +
  coord_cartesian(ylim = c(0, 100000)) +
  ggtitle(get_title(g))

plt <- grid.arrange(
  plot1,
  plot2,
  ncol = 2,
  left = "Hamming Distance",
  top = "Hamming Distance",
  bottom = "Dimension (p)")

ggsave(
  plt,
  file=paste("./figures/","band+cluster_ham", ".pdf", sep = ""),
  width = 22,
  height = 10,
  units = "cm")

g <- "MB"
data_tmp <- data_grouped %>%
  filter(graph == g)
data_tmp <- na.omit(data_tmp)
data_uns <- data %>%
  filter(graph == g)
data_uns <- na.omit(data_uns)
plot1 <- plot_models(data_tmp, "hamming_dist", data_uns) +
  coord_cartesian(ylim = c(0, 20000)) +
  ggtitle(get_title(g))


ggsave(
  plot1,
  file=paste("./figures/","MB_ham", ".pdf", sep = ""),
  width = 9,
  height = 10,
  units = "cm")



# GET TABLES FOR EACH GRAPH

print_table <- function(df, graph, cols){
  with_mean <- unname(sapply(cols, function(e) paste(e, "mean", sep = "_")))
  cols <- c("name", "dim", with_mean)
  lapply(
    c("mb-stars", "glasso-stars", "glasso-ebic"),
    function(n) {
      data_grouped[
        data_grouped$graph == graph & data_grouped$name == n, ][cols]})
}

get_metric_name <- function(metric_string) {
  if (metric_string == "f1") return("\\textbf{F1}")
  if (metric_string == "precision") return("\\textbf{Precision}")
  if (metric_string == "recall") return("\\textbf{Recall}")
  if (metric_string == "hamming_dist") return("\\textbf{Hamming Distance}")
}

to_table_row <- function(df, col) {
  names_df <- sapply(df, function(e) unique(e$name))
  dims <- c(get_metric_name(col), t(df[[1]]$dim))
  x <- t(sapply(df, function(e) e[[paste0(col, "_mean")]]))
  X <- rbind(dims, unname(cbind(names_df, round(x, 4))))
  written <-apply(X, 1, paste, collapse = " & ")
  cat(paste(written, collapse = "\\\\ \\hline "))
}

wrap_to_table <- function(df, col, graph) {
  to_table_row(
    print_table(df, graph, c(col)),
    col)}

wrap_to_table(data_grouped, "hamming_dist", "hub")
wrap_to_table(data_grouped, "hamming_dist", "scale-free")
wrap_to_table(data_grouped, "hamming_dist", "band")
wrap_to_table(data_grouped, "hamming_dist", "cluster")
wrap_to_table(data_grouped, "hamming_dist", "MB")