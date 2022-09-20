library("readxl")
library(dplyr)
library("ggpubr")

#install.packages("ggpubr")

df <- read_excel('EDC_renset_data2.xlsx')

str(df)

glimpse(df)

df <- df %>% mutate_at(c(4:9, 11:19, 24, 25), as.numeric)

str(df)

glimpse(df)

names(df)[sapply(df, function(x) sum(is.na(x)) == length(x))]

df <- subset(df, select = -c(Tidligere_salgspris) )

two_props <- df[df$'_id' %in% c(22005137, 61103979),]
two_props

properties <- c(28801085, 61103979)
df[df$'_id' %in% properties, ]

df[df$'_id' %in% c(47502896, 61103979),]

sum(is.na(df$'_id'))
length(unique(df$'_id'))

sum(is.na(df$pris))

#correlation

sum(is.na(df$lder))

ggqqplot(df$Boligareal, ylab = "kvm")

ggqqplot(df$pris, ylab = "Pris")

pris_kvm <- subset(df, select = c(pris, Boligareal) )

ggqqplot(pris_kvm, x = "Boligareal",
         color = "pris", palette = c("#00AFBB", "#E7B800"))

ggscatter(df, x = "pris", y = "Boligareal", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Pris", ylab = "Areal")

