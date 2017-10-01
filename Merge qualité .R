rm(list=ls())

library(readxl)
library(dplyr)
library(ggplot2)
library(rlist)
library(xlsx)
library(gdata)
library(readr)


getwd() # Pour regarder où est-ce qu'il bosse
setwd("/Users/Gatien/GoogleDrive/Math/Thèse/R/Reunions")   # Pour définir où est-ce qu'il bosse

data.files = list.files(pattern = "*.xlsx")
print(data.files)

# Pour faire les tests, la boucle de la fin fera tout en même temps
for(i in data.files) {
  x <- as.data.frame(read_excel(i))
  assign(sub('\\.xlsx$', '', i),x) # Pour lui donner son joli nom, sans le .xlsx
}  #

Reunion1

structure <- read_excel("Structure CR.xlsx")
structure <- as.data.frame(structure)

Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}

structure$`Nom Structure` <- Unaccent(structure$`Nom Structure`)
structure$`Nom Structure`

names(Reunion1)[2] <- structure$`Date sur le Doc`[match("Reunion1",structure$`Nom Structure`)]
Reunion1[2]



test <- Reunion1[1]
test
data.files
for(f in data.files){
  x <- as.data.frame(read_excel(f))
  # Là j'ai besoin de l'extension .xlsx !!! # assign(sub('\\.xlsx$', '', f),x)  # Pour lui donner son joli nom, sans le .xlsx
  assign(f,x)  # Pour lui donner son joli nom, sans le .xlsx
  
  names(x)[[2]] <- structure$`Date sur le Doc`[match(sub('\\.xlsx$', '', f),structure$`Nom Structure`)]
  
  
  test <- merge(test, x[,c(1,2)], by.x="Noms", by.y="Noms", all=TRUE)
}

# Ca dit qu'il y a une erreur mais ça marche bien
test

Qualite.dates <- tibble::column_to_rownames(test, var = "Noms")
Qualite.dates
class(Qualite.dates)



write.table(Qualite.dates, "Qualité date", sep="\t")









# En dessous c'est la recherche de la solution








test <- Reunion1[1]
test
data.files  # ATTENTION !!!! C'est pas dans le bon ordre dans data.files
for(f in data.files){
  x <- as.data.frame(read_excel(f))
  assign(sub('\\.xlsx$', '', f),x)  # Pour lui donner son joli nom, sans le .xlsx
  test <- merge(test, x[,c("Noms", "Qualité")], by.x="Noms", by.y="Noms", all=TRUE)
}

test
Qualite.sansdate <- test
Qualite.sansdate


# Là je veux changer qualité par la date du doc, il restera plus qu'à faire la boucle ci-dessus en mettant x[,1 et 2]

structure <- read_excel("Structure CR.xlsx")
structure <- as.data.frame(structure)

Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}

structure$`Nom Structure` <- Unaccent(structure$`Nom Structure`)
structure$`Nom Structure`

names(Reunion1)[2] <- structure$`Date sur le Doc`[match("Reunion1",structure$`Nom Structure`)]
Reunion1[2]



test <- Reunion1[1]
test
data.files
for(f in data.files){
  x <- as.data.frame(read_excel(f))
  # Là j'ai besoin de l'extension .xlsx !!! # assign(sub('\\.xlsx$', '', f),x)  # Pour lui donner son joli nom, sans le .xlsx
  assign(f,x)  # Pour lui donner son joli nom, sans le .xlsx
  
  names(x)[[2]] <- structure$`Date sur le Doc`[match(sub('\\.xlsx$', '', f),structure$`Nom Structure`)]
  
  
  test <- merge(test, x[,c(1,2)], by.x="Noms", by.y="Noms", all=TRUE)
}


Qualite.dates <- tibble::column_to_rownames(test, var = "Noms")
Qualite.dates
class(Qualite.dates)



write.table(Qualite.dates, "Qualité date", sep="\t")





test <- merge(Reunion1[,c("Noms", "Qualité")], Reunion2[,c("Noms", "Qualité")], by.x="Noms", by.y="Noms", all=TRUE)
test






FileNames <- list.files(path=".../tempDataFolder/", full.names=TRUE)
dataMerge <- data.frame()
for(f in FileNames){ 
  ReadInMerge <- read.csv(file=f, header=T, na.strings="NULL")
  dataMerge <- merge(dataMerge, ReadInMerge, 
                     by=c("COUNTRYNAME", "COUNTRYCODE", "Year"), all=T)
}