library(gdata)                   # load gdata package
library(ggplot2)
library(dplyr)

df <- read.csv("./benchmark.csv")
df <- df %>% mutate(time = timeNs / 1e9)

plotSubset <- function (vec, y_axis_scale = 1) {
  ggplot(df %>% filter(type %in% vec)) + scale_x_discrete() + geom_point(aes(x = input, y = time, col = type)) +
    scale_y_continuous(trans = "log10", labels = scales::comma) +
    labs(y = "Time in seconds", x = "Input ID")
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  
}


plotSubset(c("naive", "qbf", "muser"))
plotSubset(c("naive", "muser"))
plotSubset(c("naive", "qbf"))
plotSubset(c("muser", "qbf"))
