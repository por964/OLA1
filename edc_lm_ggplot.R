library(ggplot2)
library(jtools)

#indlæs data fra second clean
df <- read.csv('edc_second_clean.csv', sep=",", header = TRUE)
#fjern col X
df <- df[-1]

str(df)

#alternativt plot med regr værdier
fit1 <- lm(Pris_m_ ~ zip, data = df)

ggplot(df, aes(x = zip, y = Pris_m_)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
 
} + theme_classic()

#nyt plot med værdier
ggplotRegression(fit1)

