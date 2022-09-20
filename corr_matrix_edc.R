library("ggpubr")
library(ggplot2)
library(corrplot)
library(RColorBrewer)

#læs data og fjern X column
df <- read.csv('edc_second_clean.csv', sep=",", header = TRUE)
df <- df[-1]
df <- df %>% select(pris, Brutto_ekskl__ejerudgifter, Udbetaling,
              Ejerudgifter_pr__md_, zip)

#check for NA
map(df, ~sum(is.na(.)))

#forbered correlation matrix
res <- cor(df)
round(res, 2)
res2 <- rcorr(as.matrix(df))
res2

#plot matrix med farver og værdier (gem som foto for bedre kvalitet)
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





