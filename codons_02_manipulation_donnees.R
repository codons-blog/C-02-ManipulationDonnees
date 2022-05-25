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

croissance <- croissance %>% 
  rename(zone = Zone,
         indiv = Indiv)

# select() ----

# Conserver des colonnes

croissance_selection <- croissance %>% 
  select(indiv, `2007`:`2012`)

# Supprimer une colonne

croissance_selection <- croissance %>% 
  select(-zone)

# Renommer et modifier l'ordre de colonnes

croissance_selection <- croissance %>% 
  select(zone, id = indiv, `2007`:`2012`)

# Meme chose avec la fonction everything

croissance_selection <- croissance %>% 
  select(zone, id = indiv, everything())

# filter() ----

# individu n°603

croissance %>% filter(indiv == 603)

# zones 2, 3 et 4

croissance %>% filter(zone <= 4)
croissance %>% filter(zone %in% c(2, 3, 4))
croissance %>% filter(zone %in% 2:4)
croissance %>% filter(!zone >= 5)

# zones 2 et 7

croissance %>% filter(zone == 2 | zone == 7)
croissance %>% filter(zone %in% c(2, 7))

# zone 2 + individus entre 300 et 400

croissance %>% filter(zone == 2 & indiv %in% 300:400)
croissance %>% filter(zone == 2 & between(indiv, 300, 400))

# arrange() ----

# Trier par ordre croissant sur l'annee 2007

croissance %>% arrange(`2007`)

# Trier par ordre decroissant sur l'annee 2008

croissance %>% arrange(desc(`2008`))

# mutate() ----

# Calculer la croissance totale pour chaque individu entre 2007 et 2012

croissance_totale <- croissance %>% 
  mutate(croissance.totale = `2007` + `2008` + `2009` + `2010` + `2011` + `2012`)

croissance_totale <- croissance %>% 
  mutate(zone = as.factor(zone),  # definir les 2 premieres colonnes comme etant des facteurs
         indiv = as.factor(indiv)) %>% 
  mutate(croissance.totale = rowSums(across(where(is.numeric))))  # calculer la somme des lignes pour les variables numeriques

# group_by() ----

croissance_groupes <- croissance %>% 
  group_by(zone)

head(croissance)
head(croissance_groupes)

# summarise() ----

# Croissance totale pour l'ensemble des individus pour l'annee 2007

synthese1 <- croissance %>% 
  summarise(croissance.totale.2007 = sum(`2007`))

# Croissance totale pour l'ensemble des individus groupes par zone pour l'annee 2007

synthese2 <- croissance_groupes %>% 
  summarise(croissance.totale.2007 = sum(`2007`))

# Calcul de plusieurs parametres

synthese3 <- croissance_groupes %>% 
  summarise(croissance.totale.2007 = sum(`2007`),
            croissance.moyenne.2007 = mean(`2007`),
            croissance.ecart.type.2007 = sd(`2007`)) 

# join() ----

# Importer le fichier traitements.csv

traitements <- read_csv2("https://raw.githubusercontent.com/codons-blog/C-02-ManipulationDonnees/main/traitements.csv")

# Joindre les deux jeux de données

expe <- croissance %>% 
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

# Boxplot de la croissance annuelle ----


boxplot(croissance ~ traitement, 
        data = expe,
        main = "Effet de la température (T) et de la fertilisation (F) sur la croissance d'Empetrum",
        xlab = "Traitement",
        ylab = "Croissance des tiges (cm)")

# Defi ----

# Importer les donnees

dragons <- read_csv("https://raw.githubusercontent.com/codons-blog/C-02-ManipulationDonnees/main/dragons.csv")

d1 <- dragons %>% 
  rename(curcuma = paprika) %>% 
  pivot_longer(cols = tabasco:curcuma,
               names_to = "epice",
               values_to = "flamme_cm") %>% 
  mutate(flamme_cm = case_when(espece == "magyar_a_pointes" & epice == "tabasco" ~ flamme_cm - 30,
                               TRUE ~ flamme_cm)) %>% 
  mutate(flamme_m = flamme_cm / 100)

magyar_a_pointes <- d1 %>% filter(espece == "magyar_a_pointes")
suedois_a_museau_court <- d1 %>% filter(espece == "suedois_a_museau_court")
vert_gallois <- d1 %>% filter(espece == "vert_gallois")

par(mfrow = c(1, 3))

boxplot(flamme_m ~ epice,
        data = magyar_a_pointes,
        xlab = "Epice",
        ylab = "Longueur flamme (m)",
        main = "Magyar à pointes")

boxplot(flamme_m ~ epice,
        data = suedois_a_museau_court,
        xlab = "Epice",
        ylab = "Longueur flamme (m)",
        main = "Suédois à museau court")

boxplot(flamme_m ~ epice,
        data = vert_gallois,
        xlab = "Epice",
        ylab = "Longueur flamme (m)",
        main = "Vert gallois")