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

#options(scipen = 999)

df <- read.csv('edc_clean.csv', sep=",", header = TRUE)
df <- df[-1]

df[c('rum', 'vaer')] <- "x"
df$nedslag <- 0

df <- df %>% relocate(c('rum', 'vaer'), .before = Antal_plan)

df$Rum_vrelser <- as.character(df$Rum_vrelser)

setDT(df)[nchar(Rum_vrelser) > 3, vaer := paste0(str_sub(Rum_vrelser,-2,-1))]
setDT(df)[nchar(Rum_vrelser) <= 3, vaer := paste0(str_sub(Rum_vrelser,-1))]

setDT(df)[nchar(Rum_vrelser) >= 3, rum := paste0(str_sub(Rum_vrelser,1,2))]
setDT(df)[nchar(Rum_vrelser) == 2 , rum := paste0(str_sub(Rum_vrelser,1,1))]

df <- df %>% mutate_at(c('rum', 'vaer'), as.numeric)
str(df)

setDT(df)[Prisudvikling > 0.0 , nedslag := 
            paste0(round(((pris / (100.0 - Prisudvikling))*100)
                         - pris, digits = 0))]

min(df$Ombygget[df$Ombygget != min(df$Ombygget)])

sapply(df[c(Boligareal, Grundareal, pris)], sd)
sd(df$Grundareal)
str(df)
sapply(df, sd)
describe(df)
sum(df$Grundareal <= 4000, na.rm=TRUE)
sum(df$Boligareal <= 300, na.rm=TRUE)
sum(df$pris <= 5000000, na.rm=TRUE)

df2 <- df[which(Grundareal < 3000 & Boligareal < 300 & pris < 5000000),]

plot(df2$Grundareal)

df3 <- subset(df2, select = -c(Rum_vrelser) )

str(df3)
write.csv(df3, "edc_for_corr.csv",row.names = TRUE)

#prepare correlation plot
corr <- cor(df3)
round(corr, 2)
res2 <- rcorr(as.matrix(df3))
res2

corrplot(corr, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD",
                          "#4477AA"))
corrplot(corr, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = res2$P, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=TRUE 
)

#prepare to drop duplicates and correlations of 1     
corr[lower.tri(corr,diag=TRUE)] <- NA 
#drop perfect correlations
corr[corr == 1] <- NA 
#turn into a 3-column table
corr <- as.data.frame(as.table(corr))
#remove the NA values from above 
corr <- na.omit(corr) 
#select significant values  
sig <- 0.1
corr <- subset(corr, abs(Freq) > sig) 
#sort by highest correlation
corr <- corr[order(-abs(corr$Freq)),] 
#print table
print(corr)
#turn corr back into matrix in order to plot with corrplot
mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")

#plot correlations visually
corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corr, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = res2$P, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=TRUE 
)

plot(df3$Liggetid, df3$Pris_m_, pch = 16, cex = 1.3, col = "blue",
     main = "Pris nedsættelse plottet mod kvadratmeter pris
     med regressionslinje (grøn)",
     xlab = "Prisnedsættelse i kr", ylab = "Kvadratmeterpris")

abline(lm(Pris_m_ ~ Liggetid, data = df3), col='green')

edc.lm <- lm(formula = Pris_m_ ~  Netto_ekskl__ejerudgifter +
               Boligareal + Grundareal + Ejerudgifter_pr__md_,
             data = edc_reg)

