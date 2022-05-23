# Ateliers codon(s)!
# 02 - Manipulation de donnees
# Lundi 30/05/2022

# Charger le tidyverse ----

library(tidyverse)

# Definir le repertoire de travail ----

setwd("C-02-ManipulationDonnees/")

# Importer les donnees ----

croissance <- read_csv("https://raw.githubusercontent.com/codons-blog/C-02-ManipulationDonnees/main/croissance.csv")

# Explorer les donnees ----

head(croissance)  # affiche les premieres lignes
str(croissance)  # affiche les types des variables
croissance$Indiv  # affiche tous les elements de la variable Indiv
length(unique(croissance$Indiv))  # nombre d'arbrisseaux dans le jeu de donnees

# Selectionner des donnees ----

croissance[2, 5]  # 2e ligne et 5e colonne
croissance[6, ]  # 6e ligne
croissance[6, ]$Indiv  # contenu de la colonne Indiv pour la 6e ligne
croissance[croissance$Indiv == 603, ]  # toutes les colonnes pour l'individu n°603

croissance[croissance$Zone < 4, ]  # zones 2 et 3
croissance[croissance$Zone <= 4, ]  # zones 2, 3 et 4
croissance[!croissance$Zone >= 5, ]  # meme resultat

croissance[croissance$Zone == 2 | croissance$Zone == 7, ]  # zones 2 et 7
croissance[croissance$Zone == 2 & croissance$Indiv %in% c(300:400), ]  # zones 2 et individus 300 a 400

# Creer et ecraser des objets ----

croissance2 <- croissance  # cree une copie de travail
names(croissance2)  # affiche les noms de colonnes
names(croissance2)[1] <- "zone"  # modifie le nom de la 1e colonne : "Zone" devient "zone"
names(croissance2)[2] <- "ID"  # modifie le nom de la 2e colonne : "Indiv" devient "ID

croissance2[1, 4] <- 5.7  # remplace la valeur 5.1 par 5.7 dans la 1e ligne - 4e colonne
croissance2[croissance2$ID == 373, ]$X2008 <- 5.7  # meme resultat

croissance2$zone <- as.factor(croissance2$zone)  # transformer la variable zone en facteur
str(croissance2)
levels(croissance2$zone) <- c("A", "B", "C", "D", "E", "F")  # modifier les niveaux du facteur

# Donnees tidy ----

croissance_long <- pivot_longer(  # format large -> format long
  data = croissance,  # objet a transformer
  cols = c(X2007, X2008, X2009, X2010, X2011, X2012),  # colonnes a regrouper
  names_to = "Annee",  # les annees sont regroupees dans cette colonne
  values_to = "Croissance")  # les valeurs mesurees sont regroupees dans cette colonne

croissance_large <- pivot_wider(  # format long -> format large
  data = croissance_long,  # objet a transformer
  id_cols = c(Zone, Indiv),  # colonnes qui contiennent les identifiants
  names_from = Annee,  # cette colonne sert a creer les noms de colonnes
  values_from = Croissance)  # cette colonne sert a remplir les colonnes

croissance_long2 <- pivot_longer(
  data = croissance,
  cols = c(3:8),  # numeros de colonnes
  names_to = "Annee",
  values_to = "Croissance")

# Boxplot de la croissance annuelle ----

boxplot(Croissance ~ Annee,  # 1 boxplot par annee
        data = croissance_long,
        main = "Croissance annuelle de Empetrum hermaphroditum",
        ylab = "Croissance (cm)")

# rename() ----

croissance_long <- rename(croissance_long,  # objet
                          zone = Zone,  # renomme "Zone" en "zone"
                          indiv = Indiv,  # renomme "Indiv" en "indiv"
                          annee = Annee,  # renomme "Annee" en "annee"
                          croissance = Croissance)  # renomme "Croissance" en "croissance"

# filter() ----

croissance_selection <- filter(croissance_long,  # objet
                               zone %in% c(2, 3),  # garder les zones 2 et 3
                               annee %in% c("X2009", "X2010", "X2011"))  # garder les annees 2009 a 2011

# select() ----

croissance_sans_zone <- select(croissance_long,  # objet
                               indiv, annee, croissance)  # selectionner ces trois colonnes

croissance_sans_zone <- select(croissance_long,  # objet
                               -zone)  # supprimer cette colonne

croissance_sans_zone <- select(croissance_long,  # objet
                               Annee = annee,  # selectionne et renomme la colonne
                               ID.arbrisseau = indiv,
                               Croissance = croissance)

# mutate() ----

croissance_totale <- mutate(croissance,  # objet
                            croissance.totale = X2007 + X2008 + X2009 + X2010 + X2011 + X2012)  # somme des colonnes

# group_by() ----

croissance_gp <- group_by(croissance_long, indiv)  # regrouper les donnees par individu

# summarise() ----

synthese1 <- summarise(croissance_long,  # objet
                       croissance.totale = sum(croissance))  # ajouter une colonne avec la somme des croissances

synthese2 <- summarise(croissance_gp,  # objet
                       croissance.totale = sum(croissance))  # somme des croissances par individu

synthese3 <- summarise(croissance_gp,
                       total.croissance = sum(croissance),
                       moyenne.croissance = mean(croissance),
                       ecart.type.croissance = sd(croissance))

# join() ----

traitements <- read.csv2("traitements.csv")  # importer le fichier
head(traitements)  # afficher les 6 premieres lignes

expe <- left_join(croissance_long, traitements,  # tableau d'origine + tableau a joindre
                  by = c("indiv" = "Indiv", "zone" = "Zone"))  # colonnes communes aux 2 tableaux

expe2 <- merge(croissance_long, traitements,  # meme resultat en R basique
               by.x = c("zone", "indiv"),
               by.y = c("Zone", "Indiv"))

boxplot(croissance ~ Treatment,
        data = expe)
