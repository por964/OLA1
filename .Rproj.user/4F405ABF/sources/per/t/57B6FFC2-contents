library(data.table)

#indlæs data fra first clean
df <- read.csv('edc_first_clean.csv', sep=",", header = TRUE)
#fjern col X
df <- df[-1]

#lav to nye cols til rum og vaerelser
df[c('rum', 'vaer')] <- "x"

#flyt de to nye cols
df <- df %>% relocate(c('rum', 'vaer'), .before = Antal_plan)

#skift datatype på nye cols for at kunne manipulere dem
df$Rum_vrelser <- as.character(df$Rum_vrelser)

#henter vaerelser fra rum_vaer
setDT(df)[nchar(Rum_vrelser) > 3, vaer := paste0(str_sub(Rum_vrelser,-2,-1))]
setDT(df)[nchar(Rum_vrelser) <= 3, vaer := paste0(str_sub(Rum_vrelser,-1))]

#henter rum fra rum_vaer
setDT(df)[nchar(Rum_vrelser) >= 3, rum := paste0(str_sub(Rum_vrelser,1,2))]
setDT(df)[nchar(Rum_vrelser) == 2 , rum := paste0(str_sub(Rum_vrelser,1,1))]

#skift datatype tilbage til numeric
df <- df %>% mutate_at(c('rum', 'vaer'), as.numeric)

#ny column nedslag med værdi nul
df$nedslag <- 0

#beregn nedslag i kr
setDT(df)[Prisudvikling > 0.0 , nedslag := 
            paste0(round(((pris / (100.0 - Prisudvikling))*100) - pris, digits = 0))]

#check at alle datatyper er num
str(df)

#se på data for at fjerne outliers
describe(df)

#valgte max værdier til at fjerne outliers
sum(df$Grundareal <= 4000, na.rm=TRUE)
sum(df$Boligareal <= 300, na.rm=TRUE)
sum(df$pris <= 5000000, na.rm=TRUE)

#gem data med fjernede outliers som ny df
df2 <- df[which(Grundareal < 3000 & Boligareal < 300 & pris < 5000000),]

#plot for at se data efter remove outliers
plot(df2$Grundareal)
plot(df2$Boligareal)
plot(df2$pris)

my_summary <- as.data.frame(do.call(cbind, lapply(df2, summary)))

#fjern den gamle rum/vaer column
df3 <- subset(df2, select = -c(Rum_vrelser) )

#gem data som csv
write.csv(df3, "edc_second_clean.csv",row.names = TRUE)
