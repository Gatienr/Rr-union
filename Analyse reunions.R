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






data <- lapply(setNames(data.files, data.files), function(x) read_excel(x))
data  # Donc là on a bien data qui est une liste de mes documents .xslx contenu dans le dossier source
data$`Structure CR.xlsx` <- NULL # J'enlève structure puisque je le prend à part
data

for(i in data.files) {
  x <- tibble::column_to_rownames(as.data.frame(read_excel(i)), var = "Noms")
  assign(sub('\\.xlsx$', '', i),x) # Pour lui donner son joli nom, sans le .xlsx
}  # Importe tous les documents mentionnés dans data.files (read.excel), les transforme en data.frame
# et défini le noms des lignes comme les valeurs de la colonne "Noms"

row.names(Reunion1.xlsx) #  Pour vérifier que ça marche bien

allData <- tibble::column_to_rownames(merge(Reunion1.xlsx, Reunion2.xlsx, by="row.names", all = TRUE), var = "Row.names") # Pour deux ça marche
# Pour deux ça marche, je veux maintenant faire une boucle


# Là j'arrive à obtenir la liste avec les mentions : deux façons différentes !

qual <- lapply(data, function(x) x[, c(1,2)]) # Garder uniquement les colonnes que je veux. Ici la qualité et les noms
qual$`Structure CR`<- NULL
names(qual) <- sub('\\.xlsx$', '', names(qual)) # Là j'enlève .xlsx dans le noms des documents
qual
resqual <- Reduce(function(x, y) merge(x, y, all=TRUE), qual)
resqual

uniq <- unique(resqual[,1])
uniq # Pour avoir juste les personnes mentionnées

# Juste pour les mentions de Cartan
cartan <- Reduce(function(x,y) rbind(x[which(x$Noms == "Cartan"),],y[which(y$Noms == "Cartan"),]), qual )
cartan

cartan <- Reduce(function(x,y) rbindlist(x[which(x$Noms == "Cartan"),],y[which(y$Noms == "Cartan"),], use.names = TRUE, fill = TRUE), qual )

# Maintenant si je veux ajouter la date il faut que je change le nom de la colonne Qualité par la date

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

qualdate <- lapply(qual, function(x) {names(x)[[2]] <- structure$`Date sur le Doc`[match("Reunion1",structure$`Nom Structure`)]; x})
qualdate



# cbind pour chaque personne avec NA si pas mentionné, puis rbind des personnes

cartan <- Reduce(function(x,y) cbind(x[which(x$Noms == "Cartan"),2],y[which(y$Noms == "Cartan"),2]), qualdate )
# Ca marche pas parce qu'il n'y a pas Cartan dans Reunion2
qual$Reunion1[2,]

qualdate$Reunion1[,2]
qualdate$Reunion2[which(qualdate$Reunion2$Noms == "Cartan"),2]
qualdate

# Du coup je vais commencer par rajouter des lignes avec NA si la personne n'est pas mentionnée

resqualdate <- Reduce(function(x, y) merge(x, y, all=TRUE), qualdate)
uniq <- unique(resqualdate[,1])
uniq # Pour avoir la liste des personnes mentionnés dans base sur laquelle je travail
dates <- unique(structure[,4])

# Trouvé sur Stack : créer un truc de ref avec tous les noms et pas de valeurs, puis remplir avec ce qui va bien

# Le data frame vide
Noms.vide <- data.frame(Noms = uniq, check.names = FALSE)
class(dates)
list.dates <- as.data.frame(dates)
list.dates
length(list.dates)
df <- Noms.vide
for(i in 1:nrow(list.dates)){df[i+1]<- NA}
for(i in 1:nrow(list.dates)){colnames(df)[i+1] <- list.dates[i,1]}
pres.vide <- df[,-1]
rownames(pres.vide) <- df[,1]
pres.vide
Noms.vide

# Le remplissage
# Ici j'essaye de faire un cbind à partir de Noms.vide

Noms.vide[2] <- NA
Noms.vide <- as_tibble(Noms.vide)
# Noms.vide[1] <- as.character(Noms.vide[1])
Noms.vide

test <- merge(qualdate[2], qualdate[1], by.x=qualdate[[1]][1], all = TRUE)
test <- merge(Reunion1, Reunion2, all=TRUE)
test 
Reunion1
qualdate[[1]][1]
Noms.vide



# LA C'EST TOOOOOP !!!!

# Pas compatible avec la première importations des documents car je veux garder une colonne Noms
for(i in data.files) {
  x <- as.data.frame(read_excel(i))
  assign(sub('\\.xlsx$', '', i),x) # Pour lui donner son joli nom, sans le .xlsx
}  # 

test <- merge(Reunion1[,c("Noms", "Qualité")], Reunion2[,c("Noms", "Qualité")], by.x="Noms", by.y="Noms", all=TRUE) # LA CA FAIT UN TRUC BIEN !!!
# Ca marche bien mais il faut encore, changer le noms des colonnes, faire une boucle pour tous les docs 





# AVAAAAAAAANT !!!







lol <- lapply(qual, function(x) filter(x, Noms=="Mandelbrot"))
lol
lol2 <- lapply(lol, function(x) filter(x, Noms=="Artin"))
lol2 <- lapply(lol, function(x) filter(x, !is.na("Artin")))
lol2













list.dates[6,1]
df




dates
names(df)[,2] <- NA
colnames(df) <- list(dates)
df

nodata <- as.data.frame(setNames(replicate(5,numeric(0), simplify = F), letters[1:5]))

setNames( 1:3, c("foo", "bar", "baz") )


df2 <- as.data.frame(setNames(df, list(dates)))
df2
# create the dataset with some data (this will be the 'original' dataset)
df2 <- data.frame(date = c(199807:199809, 199811:199812, 199901:199903),
                  `1` = rnorm(8), `2` = rnorm(8), `3` = rnorm(8), check.names = FALSE)

# write data from original dataset to placeholder dataset
df[df$date %in% df2$date,] <- df2













# Autre méthode : pour finir je regroupe tout à la suite avec les noms : mais ça ne marche pas trop ça

resqual <- Reduce(function(x, y) merge(x, y, all=TRUE), qualdate)
resqual


require(plyr)
resqual <- Reduce(function(x, y) rbind.fill(x, y), qualdate)
resqual

t(qualdate[1])






# https://stackoverflow.com/questions/6988184/combining-two-data-frames-of-different-lengths

cbind.na<-function(df1, df2){
  
  #Collect all unique rownames
  total.rownames<-union(x = rownames(x = df1),y = rownames(x=df2))
  
  #Create a new dataframe with rownames
  df<-data.frame(row.names = total.rownames)
  
  #Get absent rownames for both of the dataframe
  absent.names.1<-setdiff(x = rownames(df1),y = rownames(df))
  absent.names.2<-setdiff(x = rownames(df2),y = rownames(df))
  
  #Fill absents with NAs
  df1.fixed<-data.frame(row.names = absent.names.1,matrix(data = NA,nrow = length(absent.names.1),ncol=ncol(df1)))
  colnames(df1.fixed)<-colnames(df1)
  df1<-rbind(df1,df1.fixed)
  
  df2.fixed<-data.frame(row.names = absent.names.2,matrix(data = NA,nrow = length(absent.names.2),ncol=ncol(df2)))
  colnames(df2.fixed)<-colnames(df2)
  df2<-rbind(df2,df2.fixed)
  
  #Finally cbind into new dataframe
  df<-cbind(df,df1[rownames(df),],df2[rownames(df),])
  return(df)
  
}


test <- cbind(qualdate[8], qualdate[[9]][2])
test

qualdate[[8]][2]

resqual <- Reduce(cbind.na(x, y) rbind.fill(x, y), qualdate)










#









for (i in names(qual) ) {
  names(qual[[match(i,names(qual))]][1]) <- structure$`Date sur le Doc`[match(i,structure$`Nom Structure`)] 
}

match(names(qual[1]),structure$`Nom Structure`)

names(qual[1])

class(as.POSIXct(as.character(structure$'Date sur le Doc'[1])))



Reduce(function(x,y) merge(x,y,by="Golum1"), xy)




# En dessous c'est du brouillon

Reduce(merge, lapply(data.files, function(x) data.frame(x, rn = row.names(x))), all = TRUE)



MyMerge       <- function(x, y){
  df            <- merge(x, y, by= "row.names", all.x= F, all.y= F)
  rownames(df)  <- df$Row.names
  df$Row.names  <- NULL
  return(df)
}
dat           <- Reduce(MyMerge, data.files)





for(i in data.files){
  allData <- merge(allData, i, by="row.names", all = TRUE)
}



require(data.table)
for(i in 1:length(data.files))
{
  data <- fread(data.files[i], header=T)
  
  # Merge
  mydata <- merge(mydata, data, all =TRUE)
  
  rm(data)
}












