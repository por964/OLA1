#læs renset data
df <- read.csv('edc_second_clean.csv', sep=",", header = TRUE)

str(df)

#se gns pris for at sætte prisintervaller
mean(df[,"pris"])

px1 <- sum(df$pris <= 500000)
px2 <- sum(df$pris > 500000 & df$pris <= 1000000)
px3 <- sum(df$pris > 1000000 & df$pris <= 2000000)
px4 <- sum(df$pris > 2000000 & df$pris <= 3000000)
px5 <- sum(df$pris > 3000000 & df$pris <= 5000000)
px6 <- sum(df$pris > 5000000 & df$pris <= 10000000)
px7 <- sum(df$pris > 10000000)

#værdier for x og y akser
v1 <- c(px1, px2, px3, px4, px5, px6, px7)
v2 <- c('0-0.5', '0.5-1', '1-2', '2-3', '3-5', '5-10', 'over 10')

#plot med text
barplot( v1 ~ v2, main="Boliger til salg per pris segment",
         xlab="Pris i mio",
         ylab="Antal boliger til salg",col = "blue")

text(6, 500, 'Antal boliger til salg: 1688')
text(6, 450, 'Gennemsnitspris: 2.013.055')

barplot( df$Boligareal ~ df$pris, main="Boliger til salg per pris segment",
         xlab="Pris i mio",
         ylab="Antal boliger til salg",col = "blue")





