#læs renset data
df <- read.csv('edc_second_clean.csv', sep=",", header = TRUE)

#intervaller for postnumre
zip1 <- nrow(df[df$zip < 1999,])
zip2 <- nrow(df[df$zip > 1999 & df$zip < 2999,])
zip3 <- nrow(df[df$zip > 3000 & df$zip < 3999,])
zip4 <- nrow(df[df$zip > 4000 & df$zip < 4999,])
zip5 <- nrow(df[df$zip > 5000 & df$zip < 5999,])
zip6 <- nrow(df[df$zip > 6000 & df$zip < 6999,])
zip7 <- nrow(df[df$zip > 7000 & df$zip < 7999,])
zip8 <- nrow(df[df$zip > 8000 & df$zip < 8999,])
zip9 <- nrow(df[df$zip > 9000 & df$zip < 9999,])

#værdier for x og y akser
v1 <- c(zip1, zip2, zip3, zip4, zip5, zip6, zip7, zip8, zip9)
v2 <- c('0-1-', '2-', '3-', '4-', '5-',
        '6-', '7-', '8-', '9-')

#plot med text
barplot( v1 ~ v2, main="Boliger til salg per postnummer",
         xlab="Postnummer",
         ylab="Antal boliger til salg",col = "blue")

text(3, 350, 'Antal boliger til salg: 1688')

