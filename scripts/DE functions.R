plot_histbox <- function(data, var, labels, wrap = NULL) {
  a <- ggplot(data, aes(x = "", y = get(var))) +
    geom_boxplot(fill = "lightblue", color = "black") + 
    coord_flip() +
    theme_classic() + # removes grid behind plot
    ylab(paste0(labels[labels$var == var, "Feature"], " (",
            labels[labels$var == var, "units"], ")")) +
    xlab("") +
    # ylim(min(data[,var]),max(data[,var])) +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    stat_summary(fun = mean, colour = "black", 
                 geom = "point", shape = 18, size = 4) +
    facet_wrap(wrap)
  
  b <- ggplot(data, aes(x = get(var), after_stat(density))) +
    geom_histogram(fill = "lightblue", color = "black", bins = 50, stat ="bin") +
    xlab("") +
    ggtitle(labels[labels$var == var, "Feature"]) +
    facet_wrap(wrap)
  # xlim(min(data[,var]),max(data[,var]))
  
  cowplot::plot_grid(b, a, 
                     ncol = 1, rel_heights = c(2, 1),
                     align = 'v', axis = 'lr')
}

plot_hist<- function(data, var, wrap = NULL) {
  ggplot(data, aes(x = "", y = get(var))) +
    geom_boxplot(fill = "lightblue", color = "black") + 
    coord_flip() +
    theme_classic() + # removes grid behind plot
    xlab(var) +
    # ylim(min(data[,var]),max(data[,var])) +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    stat_summary(fun = mean, colour = "black", 
                 geom = "point", shape = 18, size = 4) +
    ggtitle(deparse(substitute(data))) +
    facet_wrap(wrap)
}

plot_pie <- function(x) {
  a <- table(x$Genotype) %>% proportions() %>%
    as.data.frame() %>% `colnames<-`(c("Genotype", "Freq")) %>%
    ggplot(aes(x = "", y = Freq, fill = Genotype)) +
      geom_bar(stat = "identity", position = "fill") +
    # facet_grid(cols = vars(sex)) +
      coord_polar("y", start = 0) +
      theme_void()
    
    # pie(labels = paste0(round(.*100), "%"))
}

get_lambda <- function(x, feats) {
  do.call(cbind, lapply(
    x[,c(feats)], 
    transformTukey, plotit = F, returnLambda = T, quiet = T))
}