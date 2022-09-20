library("readxl")
library(dplyr)
library("ggpubr")
library(ggplot2)
library(reshape)
library(tidyr)



EDC <- read.csv('EDC_renset_data.csv', sep=";", header = TRUE)
EDC <- na.omit(EDC)
my_row <- EDC[EDC$X_id == 61104218,]
str(EDC)

EDC1 <- select(EDC,-1,-4,-6,-11,-15,-16,-19,-20,-21,-22,-23,-24)

EDC1 <- na.omit(EDC1)

two_props <- EDC1[EDC1$'X_id' %in% c(28801085, 61103979),]

min(EDC1$Rum_vrelser)
max(EDC1$Rum_vrelser)

#descriptive statistics
str(EDC1)
range(EDC1$pris)
mean(EDC1$pris)
median(EDC1$pris)

sort(table(EDC1$zip))

lapply(EDC1[, 3:4], range)

sort(EDC1$Bygger)

summary(EDC1)

res <- cor(EDC1[,3:13])
res

#simple regression cols 3,4,8,12,13 against co 9
edc_reg <- EDC1 %>% select(3,4,8,9,12,13)

write.csv(edc_reg, "edc_reg.csv",row.names = TRUE)

colnames(edc_reg)
sapply(colnames(edc_reg), print)

plot(edc_reg)

plot(edc_reg$Pris_m_, edc_reg$Netto_ekskl__ejerudgifter,
     xlab="Pris per kvm", ylab="Netto ejerudg")

lm(edc_reg$Netto_ekskl__ejerudgifter ~ edc_reg$Pris_m_) %>% abline()


edc.lm <- lm(formula = Pris_m_ ~  Netto_ekskl__ejerudgifter +
               Boligareal + Grundareal + Ejerudgifter_pr__md_,
             data = edc_reg)

edc.lm2 <- lm(formula = Pris_m_ ~  Netto_ekskl__ejerudgifter,
             data = edc_reg)

summary(edc.lm2)

edc_reg$pris.lm <- edc.lm2$fitted.values

plot(x = edc_reg$Pris_m_,                          # True values on x-axis
     y = edc.lm2$fitted.values,               # fitted values on y-axis
     xlab = "True Values",
     ylab = "Model Fitted Values",
     main = "Regression fits of property values")











