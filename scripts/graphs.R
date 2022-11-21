panels <- data.frame(start = c(0.5, 5.5, 10.5),
                     end = c(5.5, 10.5, 15.5),
                     colours = factor(c("#FFE961", "#000000", "#FFE961")))

plot_raw <- function(data, col) {
  ggplot() +
    # geom_jitter(data = data, aes_string(x = "Bin", y = col, colour = "Genotype"), alpha = 0.25) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    # Line showing the mean for each genotype in each time bin
    # geom_line(
    #   data = data %>%
    #     group_by(Bin, Genotype) %>%
    #     summarise(g_mean = mean(get(col)), .groups = "drop"),
    #   aes(x = Bin, y = g_mean, group = Genotype, colour = Genotype), size = 1) +
    geom_boxplot(data = data, aes_string(x = "Bin", y = col, colour = "Genotype")) +
    # Panels showing light/dark periods
    geom_rect(
      data = panels,
      aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
      fill = panels$colours, alpha = 0.3) +
    ggtitle(deparse(substitute(data)))
}

boxplot_data <- function(data, var) {
  ggplot(data, aes_string(x = "Genotype", y = var, label = "fish_id")) +
    geom_dotplot(aes(fill = Genotype), 
                  alpha = 0.25, binaxis = "y",
                  dotsize = 0.6, stackdir = "center") +
    # geom_jitter(aes(colour = get(points)), size = 2, alpha = 0.5) +
    # geom_text_repel() +
    scale_fill_viridis_d() +
    geom_boxplot(aes(fill = Genotype), alpha = 0.25, outlier.shape = NA) +
    ggtitle(deparse(substitute(data))) +
    ggtitle(labels[labels$var == var, "Feature"]) +
    ylab(paste0(labels[labels$var == var, "Feature"],
               " (", labels[labels$var == var, "stat"], " ",
               labels[labels$var == var, "units"], "/minute)"))
}

plot_cdot <- function(dat, var) {
  x <- dat %>%
    group_by(Bin, Genotype) %>%
    summarise(mean = mean(get(var)) %>% signif(2), max = max(get(var))) %>%
    ungroup()
  
  p <- tibble(Bin = 1:15, wt.het = 0, wt.hom = 0, het.hom = 0)
  for (i in 1:15) {
    y <- aov(get(var) ~ Genotype, data = dat[dat$Bin == i,]) %>% TukeyHSD()
    p$wt.het[i] <- y[[1]][1,4]
    p$wt.hom[i] <- y[[1]][2,4]
    p$het.hom[i] <- y[[1]][3,4]
  }
  p$diff <- rowSums(p[,-1] < 0.05) > 0
  diff <- filter(x, Bin %in% subset(p, diff == T)$Bin)
  
  ggplot(x, aes(mean, Bin)) +
    geom_line(aes(group = Bin), alpha = 0.3) +
    geom_point(aes(color = Genotype), size = 1.5, alpha = 0.3) +
    geom_line(data = diff, aes(group = Bin)) +
    geom_point(data = diff, aes(color = Genotype), size = 2) +
    geom_text_repel(data = diff, aes(color = Genotype, label = round(mean, 0)), size = 3) +
    ggtitle(labels[labels$var == var, "Feature"])
}

# boxplot_data <- function(data, x, y, p = NULL, title = "Title") {
#   ggplot(data, aes_string(x = x, y = y)) +
#     # geom_jitter(aes(colour = Genotype), size = 2, alpha = 0.5) +
#     geom_dotplot(aes(fill = Genotype), 
#                  alpha = 0.25, binaxis = "y", 
#                  dotsize = 1, stackdir = "center",
#                  binwidth = 0.3) +
#     geom_boxplot(aes(fill = Genotype), alpha = 0.25, outlier.shape = NA) +
#     scale_fill_viridis_d() +
#     stat_pvalue_manual(p, step.increase = 0.1) +
#     annotate("text", x = "wt", y = 20, label = paste0("Mean: ", summary_11$wt[y, "Mean"])) +
#     annotate("text", x = "het", y = 20, label = paste0("Mean: ", summary_11$het[y, "Mean"])) +
#     annotate("text", x = "hom", y = 20, label = paste0("Mean: ", summary_11$hom[y, "Mean"])) +
#     ggtitle(title)
# }

hist_data <- function(data, col) {
  ggplot(data, aes_string(x = col)) +
    geom_histogram()
}

plot_data2 <- function(data, col) {
  ggplot(data, aes_string(x = "Genotype", y = col)) +
    geom_jitter(aes(colour = Genotype, shape = ), size = 2, alpha = 0.5) +
    geom_boxplot(aes(fill = Genotype), alpha = 0.25, outlier.shape = NA) +
    facet_wrap(~ Trial) +
    ggtitle(deparse(substitute(data)))
}