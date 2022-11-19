# packages
library(tidyverse)
library(scales)

# print boxplot of a variable for each genotype
bplot = function(x, var) {
  x %>% ggplot(aes(x = Genotype, y = get(var))) +
    geom_jitter(aes(colour = Genotype, shape = Trial), size = 2) +
    geom_boxplot(aes(fill = Genotype), alpha = 0.25, outlier.shape = NA) + # do not show outliers, they are already plotted by geom_point
    scale_fill_viridis_d(end = 0.75) +
    scale_colour_viridis_d(end = 0.75) +
    scale_y_continuous(labels = comma, name = "var") +
    ggtitle(var) #+
    #geom_text(aes(label = fish_id), hjust=0, vjust=0)
}

bplot2 = function(x, var) {
  x %>% group_by(fish_id) %>% # groups the time bins for each larva
    mutate(totalvar = sum(get(var))) %>% # then calculates total dist from the groups
    dplyr::select(fish_id, Trial, Genotype, totalvar) %>%
    unique() %>%  # each total dist is duplicated 6x because of the bins
    ggplot(aes(x = Genotype, y = totalvar)) +
    geom_point(aes(colour = Genotype,
                   shape = Trial),
               size = 4) +
    geom_boxplot(aes(fill = Genotype),
                 alpha = 0.25,
                 outlier.shape = NA) + # do not show outliers, they are already plotted by geom_point
    scale_fill_viridis_d(end = 0.75) +
    scale_colour_viridis_d(end = 0.75) +
    scale_y_continuous(labels = comma, name = "Distance (cm)") +
    ggtitle("Total distance moved by larvae in one hour (cm)")
}

kt = function(x) {
  df <- data.frame(Variable = colnames(overall)[5:13])
  pval <- c()
  for (i in df$Variable) {
    calc <- kruskal.test(unlist(overall[, i]) ~ overall$Genotype)[3]
    pval <- c(pval, calc) %>% print()
  }
  return(df$pval <-  pval)
}

violin <- function(data, i) {
  ggbetweenstats(
    data = data,
    x = Genotype,
    y = sym(i),
    type = "nonparametric",
    pairwise.display = "all",
    p.adjust.method = "none"
  )
}

get_outliers <- function(x) {
  outliers <- which(
    x > quantile(x)[4] + 1.5 * IQR(x) | x < quantile(x)[4] - 1.5 * IQR(x)
  )
  return(outliers)
}

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
