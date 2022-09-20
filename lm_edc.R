library(jtools)
#installer package jtools

#install.packages("jtools")

#indlæs data fra second clean
df <- read.csv('edc_second_clean.csv', sep=",", header = TRUE)
#fjern col X
df <- df[-1]

#plot: dependent = Pris_m_ , independent = de 5 valgte features, her pris
plot(df$pris, df$Pris_m_, pch = 16, cex = 1.3, col = "blue",
     main = "Pris nedsættelse plottet mod kvadratmeter pris
     med regressionslinje (grøn)",
     xlab = "Prisnedsættelse i kr", ylab = "Kvadratmeterpris")
#tilføj reg linje til plot
abline(lm(Pris_m_ ~ pris, data = df), col='green')

#gem lineær reg model som variabel
edc.lm <- lm(formula = Pris_m_ ~  pris +
               Brutto_ekskl__ejerudgifter + Udbetaling + 
               Ejerudgifter_pr__md_ + zip,
             data = df)

#vis model output
summ(edc.lm)




