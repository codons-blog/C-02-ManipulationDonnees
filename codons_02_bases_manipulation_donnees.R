# Ateliers codon(s)!
# 02 - Les bases de la manipulation de donnees
# Lundi 30/05/2022

# Charger les packages ----

library(dplyr)

# Definir le repertoire de travail ----

setwd("D:/codons/Codons-01-PremiersPasAvecR/")

# Importer les donnees ----

oiseaux <- read.csv("oiseaux.csv")

# Explorer les donnees ----

head(oiseaux)  # affiche les premieres lignes
tail(oiseaux)  # affiche les dernieres lignes
str(oiseaux)  # affiche la structure des donnees

head(oiseaux$categorie)  # affiche les premiers elements de la variable
class(oiseaux$categorie)  # indique le type de variable
oiseaux$categorie <- as.factor(oiseaux$categorie)  # transforme la variable en facteur
class(oiseaux$categorie)  # verification

dim(oiseaux)  # affiche les dimensions de l'objet (x lignes par y colonnes)
summary(oiseaux)  # synthese de l'objet
summary(oiseaux$categorie)  # synthese de la variable

# Compter le nombre d'especes par niveau de preoccupation ----

en_danger_critique <- filter(oiseaux, categorie == "En danger critique")
en_danger <- filter(oiseaux, categorie == "En danger")
vulnerable <- filter(oiseaux, categorie == "Vulnerable")
quasi_menacee <- filter(oiseaux, categorie == "Quasi-menacee")
preoccupation_mineure <- filter(oiseaux, categorie == "Preoccupation mineure")

a <- length(unique(en_danger_critique$nom_francais))
b <- length(unique(en_danger$nom_francais))
c <- length(unique(vulnerable$nom_francais))
d <- length(unique(quasi_menacee$nom_francais))
e <- length(unique(preoccupation_mineure$nom_francais))

comptage_especes <- c(a, b, c, d, e)
names(comptage_especes) <- c("En danger critique",
                             "En danger",
                             "Vulnerable",
                             "Quasi-menacee",
                             "Preoccupation mineure")

# Representer les donn?es ----

barplot(comptage_especes)

help(barplot)  # affiche l'aide de la fonction
?barplot

# Sauvegarder le graphique ----

png("barplot.png",
    width = 1600, height = 600)

barplot(comptage_especes,
        xlab = "Niveau de menace", ylab = "Nombre d'esp?ces",
        ylim = c(0, 70),
        cex.names = 1.5, cex.axis = 1.5, cex.lab = 1.5)

dev.off()

# Creer un dataframe ----

categories <- c("En danger critique",
                "En danger",
                "Vulnerable",
                "Quasi-menacee",
                "Preoccupation mineure")

categories_f <- factor(categories)

comptage <- c(a, b, c, d, e)

especes_menacees <- data.frame(categories_f, comptage)  # cr?er le data frame

write.csv(especes_menacees, "especes_menacees.csv")

# Representation graphique ----

png("barplot2.png",
    width = 1600, height = 600)

barplot(especes_menacees$comptage,
        names.arg = c("En danger critique",
                      "En danger",
                      "Vulnerable",
                      "Quasi-menacee",
                      "Preoccupation mineure"),
        xlab = "Niveau   de menace", ylab = "Nombre d'especes",
        ylim = c(0, 70),
        cex.names = 1.5, cex.axis = 1.5, cex.lab = 1.5)

# Defi ----

# Extraire les envergures pour chaque espece
env_aigle <- c(195, 201, 185)
env_chouette <- c(85, 102, 91)
env_colibri <- c(8, 9, 9)
env_moineau <- c(24, 21, 22)

# Calculer l'envergure moyenne
env_moy_aigle <- mean(env_aigle)
env_moy_chouette <- mean(env_chouette)
env_moy_colibri <- mean(env_colibri)
env_moy_moineau <- mean(env_moineau)

# Rassembler ces valeurs moyennes dans un vecteur
env_moyenne <- c(env_moy_aigle, env_moy_chouette, env_moy_colibri, env_moy_moineau)

# Creer un vecteur avec les noms des especes (attention ? l'ordre !)
especes <- c("Aigle", "Chouette", "Colibri", "Moineau")

# Transformer ce vecteur en un facteur
especes_f <- as.factor(especes)
class(especes_f)

# Combiner les deux vecteurs en un data frame
envergure <- data.frame(especes_f, env_moyenne)

# Sauvegarder le barplot dans un fichier
png("barplot_envergure.png",
    width = 1600, height = 600)

barplot(envergure$env_moyenne,
        names.arg = envergure$especes_f,
        xlab = "Esp?ces", ylab = "Envergure moyenne",
        ylim = c(0, 200),
        cex.names = 1.5, cex.axis = 1.5, cex.lab = 1.5,
        col = "gold")  # modifier la couleur des barres

dev.off()