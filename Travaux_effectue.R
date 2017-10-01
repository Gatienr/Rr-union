rm(list=ls())

library(readxl)
library(dplyr)
library(ggplot2)

getwd() # Pour regarder où est-ce qu'il bosse
setwd("/Users/Gatien/GoogleDrive/Math/Thèse/R/Reunions")   # Pour définir où est-ce qu'il bosse

structure <- read_excel("Structure CR.xlsx")
structure <- as.data.frame(structure)
structure
structure$`Travaux effectués`

# Séparer les éléments pour chaque date

list.Travaux <- structure$`Travaux effectués`

for(i in 1:length(list.Travaux)){list.Travaux[i] <- strsplit(as.character(list.Travaux[i]), split = ", ")}
list.Travaux

# Faire une liste des travaux effectués par date : creer un df avec le nom de tous les sujets puis marquer oui dans la colonne
# date si mentionné

# Mettre tous les noms unique dans une liste 


# L'idée
class(list.Travaux[[19]])

uniq <- list.Travaux[[19]]
uniq
class(uniq)
# uniq <- paste0(uniq,list.Travaux[[18]], collapse= ", ")
uniq
df <- data.frame(uniq)
df
uniq <- list.Travaux[[20]]
df2 <- data.frame(uniq)
df2
df <- rbind(df, df2)
df


# La boucle

uniq <- list.Travaux[[1]]
df <- data.frame(uniq)
df
 for(i in 2:length(list.Travaux)){uniq <-list.Travaux[[i]]
   df1 <- data.frame(uniq)
  df <- rbind(df,df1)
}
df
Sujets <- unique(df)
Sujets   # Unique mais ça a juste fait sauté les lignes qui était redondantes


Sujets <- data.frame(Sujets[order(Sujets),])
names(Sujets)[1]<- "Mentions"
Sujets$Mentions <- as.character(Sujets$Mentions) # Parce que c'est des factor avant.. BIZARRE !!!

# Ajout nouvel colonne avec la date

for(i in 1:length(list.Travaux)){Sujets[, i+1] <- NA # Ajout nouvelles colonnes
  names(Sujets)[i+1] <- structure$`Date sur le Doc`[i] # Ajout des dates
}
Sujets

# Boucle pour trouver les mentions d'un sujet à toutes les dates


Sujets[, which(sapply(list.Travaux, FUN=function(X) "Géométrie" %in% X))]
Sujets[which(Sujets=="Géométrie"), which(sapply(list.Travaux, FUN=function(X) "Géométrie" %in% X))]
for(i in which(sapply(list.Travaux, FUN=function(X) "Géométrie" %in% X))){
   Sujets[which(Sujets=="Géométrie"), which(sapply(list.Travaux, FUN=function(X) "Géométrie" %in% X))]<- "Oui"
   }
colnames(Sujets)

# Boucle sur toutes les mentions

call <- "iiii"
for(f in Sujets$Mentions){for(i in which(sapply(list.Travaux, FUN=function(X) f %in% X))){
  Sujets[which(Sujets==f), which(sapply(list.Travaux, FUN=function(X) f %in% X))]<- "Oui"
}
}
Sujets

# BINGO !!!!
# Maintenant je veux ploter truc par truc

# Avec des outils de texte mining
library(NLP)
library(tm)

df$uniq
myCorpus <- Corpus(VectorSource(df$uniq))
myCorpus[[1]]



dtm <- TermDocumentMatrix(myCorpus)
dtm
m <- as.matrix(dtm)
m
v <- sort(rowSums(m),decreasing=TRUE)
v
d <- data.frame(word = names(v),freq=v)
head(d, 10)

term.freq <- rowSums(as.matrix(dtm))
df <- data.frame(term = names(term.freq), freq = term.freq)

library(ggplot2)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") +coord_flip()


# Je peux transformer Sujets pour qu'il y ait un compteur pour chaque Mentions

ggplot(data = Sujets[18,])
Sujets[18,]

Sujets$Mentions <- Unaccent(Sujets$Mentions)

# Compteur
Sujets[is.na(Sujets)] <- "Non" # Pour remplacer NA par Non
for(k in 1:nrow(Sujets)){
  j <- 0
  for(i in 2:ncol(Sujets)){if (Sujets[k,i]=="Oui"){j <- j+1}
    Sujets[k,i] <- j}}
Sujets

Sujets[43,] <- colnames(Sujets)
Sujets[43,]

samp <- Sujets[,-1]
rownames(samp) <- Sujets[,1]

samp
samp[18,]
library(ggplot2)


ggplot(samp,aes(x=samp$`-1106438400`,y=samp$`-1102204800`)) + geom_bar(stat='identity')

samp$`-1106438400`
samp$`-1102204800`








## count frequency of "miners"
GeomTrav <- tm_map(myCorpus, grep, pattern = "\\<Géométrie")
GeomTrav
sum(unlist(GeomTrav))






ggplot(data=mpg,aes(x=cty, y=hwy))


ggplot(Sujets,aes(x=person,y=age,fill=factor(drunk))) + geom_bar(stat='identity')

op <- par(mfrow = c(2, 2))
hist(islands)
utils::str(hist(islands, col = "gray", labels = TRUE))
islands
hist(sqrt(islands), breaks = 12, col = "lightblue", border = "pink")





Sujets[, structure$`Date sur le Doc`[1]] <- NA 
Sujets

example <- data.frame(col1 = rnorm(10, 0, 1), col2 = rnorm(10, 2, 3))
namevector <- c(structure$`Date sur le Doc`[1])
namevector
Sujets[ , namevector] <- NA
Sujets



replace(structure$`Date sur le Doc`, structure$`Date sur le Doc`==NA, ???)

structure$`Date sur le Doc`[is.na(structure$`Date sur le Doc`)] <- A trouver

paste('a','b','c', sep= " ")

paste('this string is concatenated', 'to this string')

for(i in 1:length(list.Travaux)){}











# Boucle pour chercher ce qui est pas un caractére
df <- data.frame(c(1:length(list.Travaux)))
df
df[1,1] <- "caca"
df
for (i in 1:length(list.Travaux)){df[1,i] <- class(list.Travaux[i])}
df







list.Travaux[2] <- strsplit(as.character(list.Travaux[12]), split = ", ")
cut.to.pieces

list.Travaux





