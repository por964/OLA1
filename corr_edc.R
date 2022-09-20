library("readxl")
library(dplyr)
library("ggpubr")
library(ggplot2)
library(reshape)
library(Hmisc)
library(corrplot)
library(RColorBrewer)
library(purrr)
library("tidyr")
library(reshape2)
library(stringr)
library(stringi)
library(data.table)

df <- read.csv('edc_for_corr.csv', sep=",", header = TRUE)
df <- df[-1]
str(df)

corr <- cor(df)
round(corr, 2)
res2 <- rcorr(as.matrix(df))
res2

plot(df$Ejerudgifter_pr__md_, df$Pris_m_, pch = 16, cex = 1.3, col = "blue",
     main = "Ejerudgifter per måned plottet mod kvadratmeter pris
     med regressionslinje (grøn)",
     xlab = "Ejerudgifter per måned.", ylab = "Kvadratmeterpris")

abline(lm(Pris_m_ ~ Ejerudgifter_pr__md_, data = df), col='green')

cor(df$Boligareal,df$pris)

r <- round(cor(df$pris, df$Boligareal), 2)
p <- cor.test(df$pris, df$Boligareal)$p.value

ggplot(df, aes(y=pris, x=Boligareal)) + 
  geom_point() + 
  labs(title = "Korrelation mellem areal og pris",
       subtitle = "(Data scraped fra EDC's hjemmeside)",
       x = "Kvadratmeter",
       y = "Udbudspris") +
  geom_smooth(method="lm", col="black") + 
  annotate("text", x=200, y=6000000, label=paste0("r = ", r), hjust=0) +
  annotate("text", x=200, y=5600000, label=paste0("p = ", round(p, 3)),
           hjust=0) + theme_classic() 

