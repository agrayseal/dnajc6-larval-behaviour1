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

boxplot_data <- function(data, y) {
  ggplot(data, aes_string(x = "Genotype", y = y, label = "fish_id")) +
    geom_dotplot(aes(fill = Genotype), 
                  alpha = 0.25, binaxis = "y",
                  dotsize = 1, stackdir = "center") +
    # geom_jitter(aes(colour = get(points)), size = 2, alpha = 0.5) +
    # geom_text_repel() +
    geom_boxplot(aes(fill = Genotype), alpha = 0.25, outlier.shape = NA) +
    ggtitle(deparse(substitute(data))) +
    facet_wrap(~Family)
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