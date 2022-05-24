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

# rename() ----

croissance2 <- rename(croissance,
                      zone = Zone,
                      indiv = Indiv)

croissance2 <- croissance %>% 
  rename(zone = Zone,
         indiv = Indiv)

# select() ----

# Conserver des colonnes

croissance2_selection <- croissance2 %>% 
  select(indiv, `2007`:`2012`)

# Supprimer une colonne

croissance2_selection <- croissance2 %>% 
  select(-zone)

# Renommer et modifier l'ordre de colonnes

croissance2_selection <- croissance %>% 
  select(zone = Zone, indiv = Indiv, `2007`:`2012`)

# Meme chose avec la fonction everything

croissance2_selection <- croissance %>% 
  select(zone = Zone, indiv = Indiv, everything())

# filter() ----

# Conserver les observations pour les zones 2 et 3 et les individus dont les identifiants sont compris entre 350 et 450

croissance2_filtre <- croissance2 %>% 
  filter(zone %in% c(2, 3),
         indiv %in% 350:450)

# Autre possibilité avec la fonction between()

croissance2_filtre <- croissance2 %>% 
  filter(zone %in% c(2, 3),
         between(indiv, 350, 450))

# Selectionner l'individu n°603

croissance2 %>% filter(indiv == 603)

# Selectionner les zones 2, 3 et 4
croissance2 %>% filter(zone <= 4)
croissance2 %>% filter(!zone >= 5)

# Selectionner les zones 2 et 7
croissance2 %>% filter(zone == 2 | zone == 7)
croissance2 %>% filter(zone %in% c(2, 7))

# Selectionner les individus dans la zone 2 avec des identifiants entre 300 et 400
croissance2 %>% filter(zone == 2, indiv %in% 300:400)
croissance2 %>% filter(zone == 2, between(indiv, 300, 400))



croissance2[1, 4] <- 5.7  # remplace la valeur 5.1 par 5.7 dans la 1e ligne - 4e colonne
croissance2[croissance2$ID == 373, ]$X2008 <- 5.7  # meme resultat

croissance2$zone <- as.factor(croissance2$zone)  # transformer la variable zone en facteur
str(croissance2)
levels(croissance2$zone) <- c("A", "B", "C", "D", "E", "F")  # modifier les niveaux du facteur

# arrange() ----

# Trier par ordre croissant sur l'annee 2007

croissance2 %>% arrange(`2007`)

# Trier par ordre decroissant sur l'annee 2008

croissance2 %>% arrange(desc(`2008`))

# mutate() ----

# Calculer la croissance totale pour chaque individu entre 2007 et 2012

croissance2_totale <- croissance2 %>% 
  mutate(croissance.totale = `2007` + `2008` + `2009` + `2010` + `2011` + `2012`)

# group_by() ----

croissance2_groupes <- croissance2 %>% 
  group_by(zone)

head(croissance2)
head(croissance2_groupes)

# summarise() ----

synthese1 <- croissance2 %>% 
  summarise(croissance.totale.2007 = sum(`2007`))

synthese2 <- croissance2_groupes %>% 
  summarise(croissance.totale.2007 = sum(`2007`))

synthese3 <- croissance2_groupes %>% 
  summarise(croissance.totale.2007 = sum(`2007`),
            croissance.moyenne.2007 = mean(`2007`),
            croissance.ecart.type.2007 = sd(`2007`)) 

# join() ----

# Importer le fichier traitements.csv

traitements <- read_csv2("https://raw.githubusercontent.com/codons-blog/C-02-ManipulationDonnees/main/traitements.csv")  # importer le fichier

# Joindre les deux jeux de données

expe <- croissance2 %>% 
  left_join(traitements,
            by = c("zone" = "Zone",
                   "indiv" = "Indiv"))

# pivot_longer() ----

croissance_long <- croissance %>% 
  pivot_longer(cols = `2007`:`2012`,
               names_to = "Annee",
               values_to = "Croissance")

# pivot_wider() ----

croissance_large <- croissance_long %>% 
  pivot_wider(id_cols = Zone:Indiv,
              names_from = Annee,
              values_from = Croissance)

# pipe ----

croissance <- read_csv("https://raw.githubusercontent.com/codons-blog/C-02-ManipulationDonnees/main/croissance.csv")
traitements <- read_csv2("https://raw.githubusercontent.com/codons-blog/C-02-ManipulationDonnees/main/traitements.csv")  # importer le fichier

expe <- croissance %>% 
  pivot_longer(cols = `2007`:`2012`,
               names_to = "annee",
               values_to = "croissance") %>% 
  left_join(traitements) %>% 
  rename(zone = Zone,
         indiv = Indiv,
         traitement = Traitement) %>% 
  mutate(zone = as.factor(zone),
         indiv = as.factor(indiv),
         annee = as.numeric(annee),
         traitement = as.factor(traitement))

boxplot(croissance ~ traitement, data = expe)

  
expe


# Boxplot de la croissance annuelle ----

png("boxplot.png",
    width = 1600, height = 600)

boxplot(croissance ~ traitement, 
        data = expe,
        main = "Effet de la température (T) et de la fertilisation (F) sur la croissance d'Empetrum",
        xlab = "Traitement",
        ylab = "Croissance des tiges (cm)")

dev.off()






