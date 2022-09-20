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

#install.packages("reshape2")

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

df %>% summarise_if(is.numeric, max)
map(df, ~sum(is.na(.)))

write.csv(df, "edc_clean.csv",row.names = TRUE)





corr <- cor(df)
#prepare to drop duplicates and correlations of 1     
corr[lower.tri(corr,diag=TRUE)] <- NA 
#drop perfect correlations
corr[corr == 1] <- NA 
#turn into a 3-column table
corr <- as.data.frame(as.table(corr))
#remove the NA values from above 
corr <- na.omit(corr) 
#select significant values  
sig <- 0.3
corr <- subset(corr, abs(Freq) > sig) 
#sort by highest correlation
corr <- corr[order(-abs(corr$Freq)),] 
#print table
print(corr)
#turn corr back into matrix in order to plot with corrplot
mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")

#plot correlations visually
corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ")
