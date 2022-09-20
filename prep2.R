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

df <- read.csv('EDC_renset_data.csv', sep=";", header = TRUE)

colSums(is.na(df))

df <- select(df,-Energimrke,-Tidligere_salgspris,-Boligydelse,
             -Forbrugsafh__forhold,-Teknisk_pris)

colSums(is.na(df))

df <- df %>% drop_na(Pris_m_, pris )
df$Udbetaling <- as.numeric(as.character(df$Udbetaling))

df <- mutate_all(df, function(x) as.double(as.numeric(x)))

colSums(is.na(df))
str(df)

df[ , c(1, 4)] <- list(NULL)

nas1 <- apply(df, 2, function(x) any(is.na(x)))
sapply(nas1, print)
nas <- df[rowSums(is.na(df)) > 0, ]
map(df, ~sum(is.na(.)))


colnames(df)
df["Klder"][is.na(df["Klder"])] <- 0

df[c("Prisudvikling","Udbetaling",
     "Ombygget", "Liggetid")][is.na(df[c("Prisudvikling", "Udbetaling",
                                         "Ombygget", "Liggetid")])] <- 0

subset(df,is.na(Antal_plan))

map(df, ~sum(is.na(.)))

df <- df %>% 
  drop_na(c("Brutto_ekskl__ejerudgifter", "Netto_ekskl__ejerudgifter",
            "Bygger", "Grundareal", "Antal_plan"))

df <- df[-1]
map(df, ~sum(is.na(.)))

#Correlation matrix
res <- cor(df)
res <- round(res, 2)

symnum(res, abbr.colnames = FALSE)

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

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


