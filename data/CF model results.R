library(readxl)
library(ggplot2)

x <- read_csv("CF results.csv")

ggplot(x, aes(x = Perceptrons), ylab = "Accuracy") +
  geom_line(aes(y = Train), colour = "blue", size = 1) +
  geom_line(aes(y = Test), colour = "red", size = 1)
