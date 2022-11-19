# importing data for each family
# data1 <- read_dataold("25-Aug-22")
# data2 <- read_dataold("30-Aug-22")
# data3 <- read_data("7-Sep-22")

# data3bin <- read_bindata("7-Sep-22")

# ommitting larva with tracking problems
#data2 <- data2[!data2$fish_id == 17,] # just an outlier, need to check video
# data3 <- data3[!data3$fish_id == 28,]
#print(data, n = Inf)

# combining all datasets
# alldata <- bind_rows(data1, dplyr::select(data3, fish_id:Mobility), .id = "Family")
# alldata$Family <- as.factor(alldata$Family)
# removing crazy 9 &39
# alldata <- alldata[!alldata$fish_id %in% c(9, 28, 39),]

# # omit larvae with tracking problems
# data <- data[!(data$Trial == 3 & data$Position == "B4"),] # no fish in well
# data <- data[!(data$Trial == 1 & data$Position == "A2"),] # not tracked for majority
# data <- data[!(data$Trial == 2 & data$Position == "C4"),] # not tracked right

# source("functions.R")
# 
# # omitting larvae 9 and 39
# alldatab <- alldata[!alldata$fish_id %in% c(9, 28, 39),]
# 
# # print plots
# plots2 <- list()
# count <- 1
# for (i in 5:length(colnames(alldatab))) {
#   p <- bplot(alldatab, colnames(alldatab)[i])
#   plots2[[count]] <- p
#   count <- count + 1
# }
# ggarrange(plotlist = plots2, common.legend = T)

# The het and hom fish seemed to move less, and the wt fish more. There is one outlier for the hets and the wt larvae had very high variation, with 2 potential outliers. I rewatched the video and there were no tracking problems for the two wt outliers (they actually did move that much).

# source("functions.R")
# 
# # dependent variables to analyse
# bplot(data3, "Distance_travelled")
# bplot(data3, "Mean_velocity")
# bplot(data3, "Time_in_middle")
# bplot(data3, "Mobility")
# 
# # scaling dependent variables & omitting wildtype outliers
# data3s <- data3 %>% mutate_at(c(5:13), scale) %>% .[!.$fish_id %in% c(9, 39),]
# 
# # now performing one-way MANOVA
# # getting the dependent variables
# dep_vars <- cbind(data3s$Distance_travelled, data3s$Time_in_middle, data3s$Mobility, data3s$Max_acceleration)
# 
# # model
# fit_gen <- manova(dep_vars ~ Genotype, data = data3s)
# summary(fit_gen) # significant
# 
# fit_trl <- manova(dep_vars ~ Trial, data = data3s)
# summary(fit_trl)
# 
# lda <- lda(data3s$Genotype ~ dep_vars, CV = F)
# plot_lda <- data.frame(
#   data3s[, c("Trial", "Genotype")], 
#   lda = predict(lda)$x)
# 
# ggplot(plot_lda) + geom_point(aes(
#   x = lda.LD1, y = lda.LD2, 
#   colour = Genotype, 
#   shape = Trial), size = 4)
# 
# # density plots
# ggplot(data3s, aes(x = get(colnames(data3s)[5]), fill = Genotype)) + 
#   geom_density(alpha = 0.5) #+
# # xlab(colnames(alldata)[i])
# 
# # pca
# pca = prcomp(data3s[,c(-1:-4)]) %>%
#   autoplot(data = data3s, colour = "Genotype", size = 4)
# pca + 
#   geom_text(aes(label = fish_id))
# The two wildtype fish (9 and 39) may be skewing the data. Here are the plots without these larvae.

read_dataold <- function(fam) {
  data <- read_excel(sprintf("./%s/%s", fam, grep("(overall)", dir(fam), value = T))) %>% # read the overall data
    # renaming columns - throws error when column doesn't exist
    dplyr::select(
      Trial = `...2`,
      Position = `...3`,
      Distance_travelled = "Distance moved center-point Total mm",
      Mean_velocity = "Velocity center-point Mean mm/s",
      Time_moving = "Movement Moving / center-point Cumulative Duration s",
      Time_in_middle = "In zone middle / center-point Cumulative Duration s",
      Mobility = "Mobility Body fill Mean %") %>%
    #Max_acceleration = matches("Acceleration center-point Maximum",.),
    #Min_acceleration = matches("Acceleration center-point Minimum",.),
    # Time_mobile = "Mobility state Mobile Cumulative Duration s",
    # Time_immobile = "Mobility state Immobile Cumulative Duration s") %>%
    mutate(Trial = str_remove(Trial, pattern = "Trial     ") %>% as.factor())
  # read in metadata and merge the dataframes
  data <- read_metaold(fam) %>% left_join(data, by = c("Trial", "Position")) %>% na.omit
  return(data)
}

read_metaold <- function(fam) {
  meta <- read_excel(sprintf("./%s/%s", fam, grep("genotypes", dir(fam), value = T))) %>%
    mutate(Genotype = factor(Genotype, levels = c("wt", "het")), 
           Trial = as.factor(Trial),
           Position = as.factor(Position)
    )
  return(meta)
}

old_read_data <- function(fam) {
  print(paste("Retrieving", fam, "data"))
  data <- read_excel(sprintf("./data/%s/%s", fam, grep("(overall)", dir(paste0("./data/", fam)), value = T))) %>% # read the overall data
    # renaming columns - throws error when column doesn't exist
    dplyr::select(
      Trial = `...2`,
      Position = `...3`,
      Distance_travelled = "Distance moved center-point Total mm",
      Velocity_mean = "Velocity center-point Mean mm/s",
      # Velocity_var = "",
      Time_moving = "Movement Moving / center-point Cumulative Duration s",
      Acceleration_max = matches("Acceleration center-point Maximum",.),
      Acceleration_min = matches("Acceleration center-point Minimum",.),
      # Acceleration_mean = matches("Acceleration ",.),
      # Acceleration_var = matches("Acceleration ",.),
      Freq_in_middle = "In zone middle / center-point Frequency",
      Time_in_middle = "In zone middle / center-point Cumulative Duration s",
      Dist_to_middle_total = "Distance to zone middle / center-point Total mm",
      Dist_to_middle_mean = "Distance to zone middle / center-point Mean mm",
      Dist_to_middle_var = matches("Distance to zone middle / center-point Variance",.),
      Mobility_mean = "Mobility Body fill Mean %",
      Mobility_var = matches("Mobility Body fill Variance",.)) %>%
    mutate(Trial = str_remove(Trial, pattern = "Trial     ") %>% as.factor())
  # read in metadata and merge the dataframes
  # data <- read_meta(fam) %>% left_join(data, by = c("Trial", "Position")) %>% na.omit
  return(data)
}

<!-- ### Dark data -->
  
  ```{r anova dd, include = F}
# for (i in colnames(dark_data[,-c(1:7)])) {
#   print(i)
#   print(summary(aov(get(i) ~ Genotype + Trial + Genotype:Trial, data = dark_data)))
# }
```

<!-- ### Light data -->
  
  ```{r anova ld, include = F}
# for (i in colnames(light_data[,-c(1:7)])) {
#   print(i)
#   print(summary(aov(get(i) ~ Genotype + Trial + Genotype:Trial, data = light_data)))
# }
```

<!-- The interaction between genotype and trial here is significant. The data doesn't really meet the right assumptions though, so I'm currently unsure how to interpret this. -->
  
  <!-- ### Mean dark data -->
  
  ```{r anova mdd, include = F}
# for (i in colnames(mean_dark[,-c(1:3)])) {
#   print(i)
#   print(summary(aov(get(i) ~ Genotype + Trial + Genotype:Trial, data = mean_dark)))
# }
```

<!-- ### Mean light data -->
  
  ```{r anova mld, include = F}
# for (i in colnames(mean_light[,-c(1:3)])) {
#   print(i)
#   print(summary(aov(get(i) ~ Genotype + Trial + Genotype:Trial, data = mean_light)))
# }
```

## Boxplots

### Genotype

11/10 Family
```{r boxplots a11}
# source("scripts/graphs.R")

# features <- colnames(raw_all)[-c(1:7)]

# plots <- lapply(features, boxplot_data, data = raw_11, x = "Genotype")
# ggarrange(plotlist = plots, common.legend = T, ncol = 2)
```
12/10 Family
```{r boxplots a12}
# plots <- lapply(features, boxplot_data, data = raw_12, x = "Genotype")
# ggarrange(plotlist = plots, common.legend = T, ncol = 2)
```


### Trial

11/10 Family
```{r boxplots b11}
# plots <- lapply(features, boxplot_data, data = raw_11, x = "Trial")
# ggarrange(plotlist = plots, common.legend = T, ncol = 2)
```
12/10 Family
```{r boxplots b12}
# plots <- lapply(features, boxplot_data, data = raw_12, x = "Trial")
# ggarrange(plotlist = plots, common.legend = T, ncol = 2)
```

### Tray

11/10 Family
```{r boxplots c11}
# plots <- lapply(features, boxplot_data, data = raw_11, x = "Tray")
# ggarrange(plotlist = plots, common.legend = T, ncol = 2)
```
12/10 Family
```{r boxplots c12}
# plots <- lapply(features, boxplot_data, data = raw_12, x = "Tray")
# ggarrange(plotlist = plots, common.legend = T, ncol = 2)
```


### Histograms

```{r histograms}
# plots <- lapply(features, hist_data, data = raw_11)
# 
# ggarrange(plotlist = plots, common.legend = T, ncol = 2)
```


<!-- I spot 2 extreme outliers in this graph (in bin 2 and 5), so will try and identify them. -->
  
  <!-- ```{r omit} -->
  <!-- raw_11 <- raw_11[!((raw_11$fish_id == "2.2-6.10") & (raw_11$Bin == 2) & (raw_11$Trial == 7)),] -->
  
  <!-- raw_11 <- raw_11[!((raw_11$fish_id == "5.2-6.10") & (raw_11$Bin == 5) & (raw_11$Trial == 7)),] -->
  <!-- ``` -->