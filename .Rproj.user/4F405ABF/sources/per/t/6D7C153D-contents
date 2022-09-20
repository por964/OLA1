#indlæs rå data
df <- read.csv('EDC_renset_data.csv', sep=";", header = TRUE)

new_DF <- df[is.na(df$Antal_plan),]

#check hvor mange NA i hver column
colSums(is.na(df))

#fjern columns med for mange NA og dem vi ikke skal bruge
df <- select(df,-X, -X_id, -scrapedate,-Energimrke,-Tidligere_salgspris,
             -Boligydelse,
             -Forbrugsafh__forhold,-Teknisk_pris)

#check NA igen
colSums(is.na(df))

#drop rows hvor kvm pris eller pris er NA
df <- df %>% drop_na(Pris_m_, pris )

#lav udbetaling til numeric
df$Udbetaling <- as.numeric(as.character(df$Udbetaling))

#lav hele datasættet til double
df <- mutate_all(df, function(x) as.double(as.numeric(x)))

#check antal NA per column igen og datatype for hver column
colSums(is.na(df))
str(df)

#check hvilke rækker og columns der stadig har NA værdier
nas1 <- apply(df, 2, function(x) any(is.na(x)))
sapply(nas1, print)
nas <- df[rowSums(is.na(df)) > 0, ]
map(df, ~sum(is.na(.)))

#sætter kælder til nul hvis den er NA
df["Klder"][is.na(df["Klder"])] <- 0

#efter at have checket datasættet med EDC.dk sætter jeg NA til nul i disse cols
df[c("Prisudvikling","Udbetaling",
     "Ombygget", "Liggetid")][is.na(df[c("Prisudvikling", "Udbetaling",
                                         "Ombygget", "Liggetid")])] <- 0

#check hvilke rows der har NA i antal plan
subset(df,is.na(Antal_plan))

#antal rows med NA for hver column
map(df, ~sum(is.na(.)))

#drop rows med NA i følgende column values
df <- df %>% 
  drop_na(c("Brutto_ekskl__ejerudgifter", "Netto_ekskl__ejerudgifter",
            "Bygger", "Grundareal", "Antal_plan"))

#check at alle NA værdier er fjernet/ændret
df %>% summarise_if(is.numeric, max)
map(df, ~sum(is.na(.)))

#skriver den rensede dataframe til fil
write.csv(df, "edc_first_clean.csv",row.names = TRUE)
