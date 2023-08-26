# Script for Analysis of the obtained data.
# 
# Neighborhood Selection as Covariance Selection - Revisiting Meinshausen and
# Bühlmann's Approach
#
# For the seminar "Network Learning and Sparse Estimation"
#
# Philipp Koch, 2023

library(data.table)
library(gridExtra)
library(dplyr)
library(ggplot2)

setwd("/home/philko/Documents/Uni/SoSe23/SparseAndNetwork")
data <- read.csv('./data/data.csv')
data$name <- apply( data[ , c('method', 'selector')] , 1 , paste , collapse = "-" )
data <- subset(data, select = -c(method, selector))

data_grouped <- data %>%
  group_by(graph, dim, name) %>% 
  summarise_at(vars("f1", "precision", "recall"), list(mean = mean, std = ~sqrt(var(.x))))
               
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

data_grouped <- add_ci(data_grouped, "f1")
data_grouped <- add_ci(data_grouped, "precision")
data_grouped <- add_ci(data_grouped, "recall")

graphs <- unique(data$graph)
plots <- list()
i <- 1
for (g in graphs){
  data_tmp <- data_grouped %>%
    filter(graph == g)
  data_tmp <- na.omit(data_tmp)
  plot1 <- ggplot(data=data_tmp, aes(x=dim, y=f1_mean, group=name)) +
    geom_line(aes(col = name)) +
    scale_color_manual(
      values = c(
        "glasso-stars" = "#5D8AA8",
        "glasso-ebic" = "#E52B50",
        "mb-stars" = "#ED872D")) +
    geom_point() +
    ylab("F1") +
    xlab("Dimensions (p)") +
    geom_ribbon(
      data = data_tmp,
      aes(x = dim, ymin = ci_f1_min, ymax = ci_f1_max, group = name),
      alpha = 0.3) + 
    ggtitle("") +
    theme(legend.position = "none")
  plot2 <- ggplot(data=data_tmp, aes(x=dim, y=precision_mean, group=name)) +
    geom_line(aes(col = name)) +
    scale_color_manual(
      values = c(
        "glasso-stars" = "#5D8AA8",
        "glasso-ebic" = "#E52B50",
        "mb-stars" = "#ED872D")) +
    geom_ribbon(
      data = data_tmp,
      aes(x = dim, ymin = ci_precision_min, ymax = ci_precision_max, group = name),
      alpha = 0.3) + 
    geom_point() +
    ylab("Precision") +
    xlab("Dimensions (p)") +
    ggtitle(get_title(g)) +
    theme(legend.position = "none")
  plot3 <- ggplot(data=data_tmp, aes(x=dim, y=recall_mean, group=name)) +
    geom_line(aes(col = name)) +
    scale_color_manual(
      values = c(
        "glasso-stars" = "#5D8AA8", # air force blue
        "glasso-ebic" = "#E52B50", # amararanth
        "mb-stars" = "#ED872D")) + # cadmium orange
    geom_ribbon(
      data = data_tmp,
      aes(x = dim, ymin = ci_recall_min, ymax = ci_recall_max, group = name),
      alpha = 0.3) + 
    geom_point() +
    ylab("Recall") +
    xlab("Dimensions (p)") +
    ggtitle("") +
    labs(color = "Estimator (+ Selector)") +
    theme(legend.position = c(1.75, 0.2))
    
    
    
  # grid.arrange(plot1, plot2, plot3, ncol = 3)
  plots[[i]] <- grid.arrange(plot1, plot2, plot3, ncol = 3)
  i <- i + 1
  #  Adapat from here https://latexcolor.com/
}

# grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], nrow = 5)
# 
# 
# plot3 <- ggplot(data=data_tmp, aes(x=dim, y=recall_mean, group=name)) +
#   geom_line(aes(col = name)) + 
#   geom_ribbon(
#     data = data_tmp,
#     aes(x = dim, ymin = ci_recall_min, ymax = ci_recall_max, group = name),
#     alpha = 0.3) + 
#   geom_point() +
#   ylab("Recall") +
#   xlab("Dimensions (p)") +
#   ggtitle("") +
#   labs(color = "Estimator (+ Selector)")
# plots[[i]] <- grid.arrange(plot1, plot2, plot3, ncol = 3)
