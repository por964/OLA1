library("readxl")
library(dplyr)
library("ggpubr")
library(ggplot2)
library(reshape)

df <- read.csv('EDC_renset_data.csv', sep=";", header = TRUE)

df <- select(df,-1,-4,-11,-15,-16,-19,-20,-21,-22,-23,-24)

sort()

df <- na.omit(df)

str(df)

df$calc_px <- with(df, Vgtet_areal * Pris_m_)

my_sum <- as.data.frame(summary(df))

which(df$X_id == 61104218)
my_row <- df[df$x_id == 61104218,]
