library("readxl")
library(dplyr)
library("ggpubr")
library(ggplot2)
library(reshape)
library(Hmisc)
library(corrplot)
library(RColorBrewer)

df <- read.csv('edc_reg.csv')
df <- df[-1]
colnames(df)

#Correlation matrix
res <- cor(df)
res <- round(res, 2)
res

res2 <- rcorr(as.matrix(df))
res2

res2$r
res2$P

symnum(res, abbr.colnames = FALSE)

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)

corrplot(res, method="color")
corrplot(res, method="number")
corrplot(res, type="upper")



col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(res, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = res2$P, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=TRUE 
)


areal <- lm(Pris_m_ ~ Boligareal, data = df)
grund <- lm(Pris_m_ ~ Grundareal, data = df)
ejerudg <- lm(Pris_m_ ~ Ejerudgifter_pr__md_, data = df)
pris <- lm(Pris_m_ ~ pris, data = df)
netto_ejerudg <- lm(Pris_m_ ~ Netto_ekskl__ejerudgifter, data = df)

summary(areal)
summary(grund)
summary(ejerudg)
summary(pris)
summary(netto_ejerudg)

plot(Pris_m_ ~ Boligareal, data = df)
abline(lm(Pris_m_ ~ Boligareal, data = df))
