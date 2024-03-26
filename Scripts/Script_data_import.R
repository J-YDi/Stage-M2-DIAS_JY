# Script V. Pochic modifie JY Dias
# 16/01/2024

library(dplyr)
library(tidyverse)
library(stringr)
library(FactoMineR)
library(factoextra)
library(vegan)
library(paletteer)
library(mgcv)
library(gratia)
library(scatterpie)

#### Import data ####

# Open with the correct file encoding allows to preserve accents
DataREPHY_MA <- read.csv2('data/REPHY_Manche_Atlantique_1987-2022.csv', fileEncoding = "ISO-8859-1")
DataREPHY_Med <- read.csv2('data/REPHY_Med_1987-2022.csv', fileEncoding = "ISO-8859-1")

# Creating a vector to import all columns as characters
classes_char <- rep('character', 56)
DataREPHY_2023 <- read.csv2('data/Extraction SEANOE_REPHY_phyto-Hydro_Manche_Atl-Med 2023 Semestre1 validé 16012024.csv', 
                            fileEncoding = "ISO-8859-1", colClasses = classes_char)

# Merge the first 2 datasets
DataREPHY <- bind_rows(DataREPHY_MA, DataREPHY_Med) %>%
  # This line allows to remove the problematic row that separates the hydro and phyto datasets
  filter(Passage...Mois != 'Passage : Mois')

# Binding the 2 datasets
DataREPHY_8723 <- bind_rows(DataREPHY, DataREPHY_2023)


### Load tables used to enrich the dataset
# Load table "Zones_marines"
ZM <- read.csv('data/Zones_marines.csv', sep = ';', header = TRUE)

# Load table "Liste_phylum.classe"
PhyClasse <- read.csv('data/Liste_phylum.classe_REPHY.csv', sep =';', header = TRUE, fileEncoding = 'ISO-8859-1')

#### Formatting the dataframe with better column names ####

# Extracting the numeric code for the ZM
Table1 <- DataREPHY_8723 %>%
  mutate(ZM_Quadrige_Numero = as.numeric(str_extract(Lieu.de.surveillance...Entité.de.classement...Libellé, '[:alnum:]+')))

# Change column names (to match ZM)
colnames(Table1)[which(names(Table1) == "Lieu.de.surveillance...Mnémonique")] <- "Code_point_Mnemonique"
colnames(Table1)[which(names(Table1) == "Lieu.de.surveillance...Libellé")] <- "Code_point_Libelle"
colnames(Table1)[which(names(Table1) == "Passage...Date")] <- "Date"
colnames(Table1)[which(names(Table1) == "Coordonnées.passage...Coordonnées.minx")] <- "lon"
colnames(Table1)[which(names(Table1) == "Coordonnées.passage...Coordonnées.miny")] <- "lat"
colnames(Table1)[which(names(Table1) == "Résultat...Libellé.unité.de.mesure.associé.au.quintuplet")] <- "Mesure_Unite"
colnames(Table1)[which(names(Table1) == "Résultat...Symbole.unité.de.mesure.associé.au.quintuplet")] <- "Mesure_Symbole"
colnames(Table1)[which(names(Table1) == "Résultat...Nom.du.taxon.référent")] <- "Taxon"
colnames(Table1)[which(names(Table1) == "Résultat...Libellé.du.groupe.de.taxon")] <- "Groupe_Taxon"
colnames(Table1)[which(names(Table1) == "Résultat...Valeur.de.la.mesure")] <- "Valeur_mesure"
colnames(Table1)[which(names(Table1) == "Prélèvement...Immersion")] <- "Profondeur.metre"
colnames(Table1)[which(names(Table1) == "Prélèvement...Niveau")] <- "Prelevement.niveau"
colnames(Table1)[which(names(Table1) == "Résultat...Code.paramètre")] <- "Code.parametre"
colnames(Table1)[which(names(Table1) == "Résultat...Libellé.paramètre")] <- "Parametre"
colnames(Table1)[which(names(Table1) == "Résultat...Niveau.de.qualité")] <- "Qualite.resultat"
colnames(Table1)[which(names(Table1) == "Prélèvement...Niveau.de.qualité")] <- "Qualite.prelevement"
colnames(Table1)[which(names(Table1) == "Passage...Service.saisisseur...Libellé")] <- "Service.saisie"
colnames(Table1)[which(names(Table1) == "Passage...Heure")] <- "Heure"
colnames(Table1)[which(names(Table1) == "Résultat...Service.analyste...Code")] <- "Service.analyse"
colnames(Table1)[which(names(Table1) == "Prélèvement...Service.préleveur...Code")] <- "Service.prelevement"
colnames(Table1)[which(names(Table1) == "Prélèvement...Identifiant.interne")] <- "ID.interne.prelevement"
colnames(Table1)[which(names(Table1) == "Passage...Identifiant.interne")] <- "ID.interne.passage"

#### Curate table to keep only desired variables ####
Table1 <- Table1 %>%
  dplyr::select(c('ZM_Quadrige_Numero', 'Code_point_Mnemonique', 'Code_point_Libelle', 'Date', 
                  'Heure', 'lon', 'lat', 'Mesure_Unite', 'Mesure_Symbole', 'Taxon', 'Valeur_mesure', 
                  'Prelevement.niveau', 'Profondeur.metre', 'Code.parametre', 'Parametre', 
                  'Qualite.prelevement', 'Qualite.resultat', 'ID.interne.prelevement', 'ID.interne.passage'))

# Modifying date format so that it gives the year and month, and getting rid of rows with no Year value
Table1 <- Table1 %>%
  mutate(Date = dmy(Date)) %>%
  # modifies the date format
  mutate(Day = day(Date)) %>%
  mutate(Month = month(Date, label = F)) %>%
  mutate(Year = year(Date)) %>%
  filter(!is.na(Year))

# Transform the measured values into numbers
# Caution! The decimal separator is a comma, 
# we need first to transform it to a full stop to avoid fuckery
Table1$Valeur_mesure <-  str_replace_all(Table1$Valeur_mesure, ',', '.')
Table1 <- Table1 %>%
  mutate(Valeur_mesure = as.numeric(Valeur_mesure))

#### Tidying table structure ####

## Associate a region with each ZM code
Table1 <- left_join(Table1, ZM, by='ZM_Quadrige_Numero', suffix=c('',''))

# Basically, we want the hydrological measurements as columns and the phytoplankton taxa as rows

# Separate the table into 2 : 1 for hydrology and the other for phytoplankton
Table1_hydro <- Table1 %>%
  filter(Taxon == "")

Table1_phyto <- Table1 %>%
  filter(Taxon != "")

### Manipulating the phyto table ###
# Associate the Phylum/Class to the taxon
Table1_phyto <- left_join(Table1_phyto, PhyClasse, by='Taxon', suffix=c('',''))

# Compute genus from Taxon
# We find the genus by selecting only the first word in the string 'Taxon'
Table1_phyto <- Table1_phyto %>%
  mutate(Genus = str_extract(Taxon, 'Pseudo-nitzschia|[:alnum:]+'))

# Applying quality control and selecting only FLORTOT
Table1_phyto_select <- Table1_phyto %>%
  #filter out Code.Region = 0 (only a few mysterious events in 2011-2013)
  filter(Code.Region != 0) %>%
  #filter(Qualite.prelevement == 'Bon') %>%
  filter(Code.parametre == 'FLORTOT' | Code.parametre == 'FLORIND' | Code.parametre == 'FLORPAR') %>%
  # Keeping only surface (or near surface) sampling
  filter(Prelevement.niveau == 'Surface (0-1m)' | Prelevement.niveau == '2 mètres' |
           Prelevement.niveau == 'de 3 à 5 mètres')
# We're cutting the pipe here so we can create 3 tables: 1 grouped by genus, 1 by class and 1 by taxon

Table1_phyto_genus <- Table1_phyto_select %>%
  group_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage, Prelevement.niveau,Code.parametre, Genus) %>%
  summarise(Comptage = sum(Valeur_mesure), .groups = 'keep') %>%
  pivot_wider(names_from = Genus, values_from = Comptage)

Table1_phyto_class <- left_join(Table1_phyto_select, PhyClasse, by = 'Taxon', suffix = c('','')) %>%
  group_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage, Prelevement.niveau,Code.parametre, Phylum.Classe) %>%
  summarise(Comptage = sum(Valeur_mesure), .groups = 'keep')

# Des classes non attribues
unique(Table1_phyto_class$Phylum.Classe) # Effectivement
Table1_phyto_class <- Table1_phyto_class[Table1_phyto_class$Phylum.Classe != "", ] #On les supprime ces lignes
Table1_phyto_class <- pivot_wider(Table1_phyto_class ,names_from="Phylum.Classe", values_from = "Comptage",values_fn = mean) #On pivote


Table1_phyto_taxon <- Table1_phyto_select %>%
  group_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage, Prelevement.niveau,Code.parametre, Taxon) %>%
  summarise(Comptage = sum(Valeur_mesure), .groups = 'keep') %>%
  pivot_wider(names_from = Taxon, values_from = Comptage)

# write the tables to free some memory space
write.csv2(Table1_phyto_genus, 'Table1_phyto_genus_JY.csv', row.names = FALSE)
write.csv2(Table1_phyto_class, 'Table1_phyto_class_JY.csv', row.names = FALSE)
write.csv2(Table1_phyto_taxon, 'Table1_phyto_taxon_JY.csv', row.names = FALSE)


### Manipulating the hydro table ###
# Spread the hydrology measurements

Table1_hydro_select <- Table1_hydro %>%
  select(c('ID.interne.passage', 'Qualite.resultat', 'Code.parametre', 'Valeur_mesure', 'Prelevement.niveau',
           'Code.Region', 'Region', 'Date', 'Day', 'Month', 'Year', 'Code_point_Libelle', 
           'Code_point_Mnemonique', 'lon', 'lat')) %>%
  #filter(Code_point_Libelle == 'Ouest Loscolo')
  #filter out Code.Region = 0 (only a few mysterious events in 2011-2013)
  filter(Code.Region != 0) %>%
  #filter(Qualite.resultat == 'Bon') %>%
  filter(Prelevement.niveau == 'Surface (0-1m)') %>%
  group_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage, Prelevement.niveau, Code.parametre) %>%
  # There is probably something shifty here, regarding multiple CHLOROA measurements at certain stations,
  # made with different methods. For now we average everything but this is quite bad.
  summarise(Valeur_mesure = mean(Valeur_mesure), .groups = 'keep') %>%
  pivot_wider(names_from = Code.parametre, values_from = Valeur_mesure)

write.csv2(Table1_hydro_select, 'Table1_hydro_JY.csv', row.names = FALSE)

##### Essai de faire la selection de la profondeur ########

#detection des doublons
doublons <- Table1_hydro_select[duplicated(Table1_hydro_select$ID.interne.passage) | duplicated(Table1_hydro_select$ID.interne.passage, fromLast = TRUE), ]

doublons_phyto <- Table1_phyto_taxon[duplicated(Table1_phyto_taxon$ID.interne.passage) | duplicated(Table1_phyto_taxon$ID.interne.passage, fromLast = TRUE), ]

# Donnees d'essai
donnees2 <- data.frame(Identifiant = c("25", "25", "25", "2",  "2",  "3",  "3",  "2","2"), 
           Prelevement.niveau = c("Surface (0-1m)",  "Surface (0-1m)",  "de 3 à 5 mètres", "de 3 à 5 mètres", "2 mètres", "2 mètres",       
  "de 3 à 5 mètres", "de 3 à 5 mètres","Surface (0-1m)")
)

# MARCHE PRESQUE MAIS NON :
resultat_filtre <- doublons_phyto %>%
  filter(Prelevement.niveau %in% c("Surface (0-1m)", "2 mètres", "de 3 à 5 mètres")) %>%
  group_by(ID.interne.passage) %>%
  mutate(Ordre = match(Prelevement.niveau, c("Surface (0-1m)", "2 mètres", "de 3 à 5 mètres"))) %>%
  slice_min(order_by = Ordre) %>%
  filter(!duplicated(ID.interne.passage)) %>%
  select(-Ordre)

# On fais un tableau phyto et hydro 
data_hydro <- read_delim("data_modif/Table1_hydro_JY.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
# Donnees phyto
data_phyto <- read_delim("data_modif/Table1_phyto_taxon_JY.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

# On ne garde que FLORTOT
data_phyto_FLORTOT <- filter(data_phyto,Code.parametre == "FLORTOT")
# On separe selon les 3 niveaux presents
data_phyto_FLORTOT_S01 <- filter(data_phyto_FLORTOT, Prelevement.niveau == "Surface (0-1m)")
data_phyto_FLORTOT_2M <- filter(data_phyto_FLORTOT, Prelevement.niveau == "2 mètres")
data_phyto_FLORTOT_35M <- filter(data_phyto_FLORTOT, Prelevement.niveau == "de 3 à 5 mètres")

# On ne garde que FLORIND
data_phyto_FLORIND <- filter(data_phyto, Code.parametre == "FLORIND")
data_phyto_FLORIND_S01 <- filter(data_phyto_FLORIND, Prelevement.niveau == "Surface (0-1m)")
data_phyto_FLORIND_2M <- filter(data_phyto_FLORIND, Prelevement.niveau == "2 mètres")
data_phyto_FLORIND_35M <- filter(data_phyto_FLORIND, Prelevement.niveau == "de 3 à 5 mètres")

# On ne garde que FLORPAR
data_phyto_FLORPAR <- filter(data_phyto, Code.parametre == "FLORPAR")
data_phyto_FLORPAR_S01 <- filter(data_phyto_FLORPAR, Prelevement.niveau == "Surface (0-1m)")
data_phyto_FLORPAR_2M <- filter(data_phyto_FLORPAR, Prelevement.niveau == "2 mètres")
data_phyto_FLORPAR_35M <- filter(data_phyto_FLORPAR, Prelevement.niveau == "de 3 à 5 mètres")

# On fusionne avec les donnees phyto et hydro et
# Tentative qui marche presque de selection de la profondeur 
# FLORTOT
data_FLORTOT_S01 <- right_join(data_hydro,data_phyto_FLORTOT_S01) 
data_FLORTOT_2M <- right_join(data_hydro,data_phyto_FLORTOT_2M)
names <- colnames(data_FLORTOT_S01) # On prend les noms des colonnes pour plus tard
# On obtient un tableau qui a des colonnes en plus parce que ces colonnes ont Code.Region jusqu'a ID.interne.passage nouveaux
data_FLORTOTS01vs2M <- right_join(data_FLORTOT_S01,data_FLORTOT_2M,by = join_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage))
# On recupere seulement ces nouvelles colonnes
data_FLORTOTS01vs2M <- select(data_FLORTOTS01vs2M,Code.Region:ID.interne.passage,Prelevement.niveau.y:`Oxytoxum caudatum.y`)
# On vire le .y inutile
colnames(data_FLORTOTS01vs2M)<- names
# On combine les deux
data_FLORTOTS012M <- rbind(data_FLORTOT_S01,data_FLORTOTS01vs2M)
# En theorie aucun doublon

data_FLORTOT_35M <- right_join(data_hydro,data_phyto_FLORTOT_35M)
data_FLORTOTS012Mvs35M <- right_join(data_FLORTOTS012M,data_FLORTOT_35M,by = join_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage))
data_FLORTOTS012Mvs35M <- select(data_FLORTOTS012Mvs35M,Code.Region:ID.interne.passage,Prelevement.niveau.y:`Oxytoxum caudatum.y`)
colnames(data_FLORTOTS012Mvs35M)<- names
data_FLORTOTS012M35M <- rbind(data_FLORTOTS012M,data_FLORTOTS012Mvs35M)

doublons_FLORTOT <- data_FLORTOTS012M35M[duplicated(data_FLORTOTS012M35M$ID.interne.passage) | duplicated(data_FLORTOTS012M35M$ID.interne.passage, fromLast = TRUE), ]
# Sauf qu'il y en a... mais on a ajouté 100 lignes et on se retrouve qu'avec 4 en double...



# FLORIND
data_FLORIND_S01 <- right_join(data_hydro,data_phyto_FLORIND_S01) 
data_FLORIND_2M <- right_join(data_hydro,data_phyto_FLORIND_2M)
names <- colnames(data_FLORIND_S01) # On prend les noms des colonnes pour plus tard
# On obtient un tableau qui a des colonnes en plus parce que ces colonnes ont Code.Region jusqu'a ID.interne.passage nouveaux
data_FLORINDS01vs2M <- right_join(data_FLORIND_S01,data_FLORIND_2M,by = join_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage))
# On recupere seulement ces nouvelles colonnes
data_FLORINDS01vs2M <- select(data_FLORINDS01vs2M,Code.Region:ID.interne.passage,Prelevement.niveau.y:`Oxytoxum caudatum.y`)
# On vire le .y inutile
colnames(data_FLORINDS01vs2M)<- names
# On combine les deux
data_FLORINDS012M <- rbind(data_FLORIND_S01,data_FLORINDS01vs2M)
# En theorie aucun doublon

data_FLORIND_35M <- right_join(data_hydro,data_phyto_FLORIND_35M)
data_FLORINDS012Mvs35M <- right_join(data_FLORINDS012M,data_FLORIND_35M,by = join_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage))
data_FLORINDS012Mvs35M <- select(data_FLORINDS012Mvs35M,Code.Region:ID.interne.passage,Prelevement.niveau.y:`Oxytoxum caudatum.y`)
colnames(data_FLORINDS012Mvs35M)<- names
data_FLORINDS012M35M <- rbind(data_FLORINDS012M,data_FLORINDS012Mvs35M)

doublons_FLORIND <- data_FLORINDS012M35M[duplicated(data_FLORINDS012M35M$ID.interne.passage) | duplicated(data_FLORINDS012M35M$ID.interne.passage, fromLast = TRUE), ]
# Sauf qu'il y en a... mais on a ajouté 94 lignes et on se retrouve qu'avec 38 en double...



# FLORPAR
data_FLORPAR_S01 <- right_join(data_hydro,data_phyto_FLORPAR_S01) 
data_FLORPAR_2M <- right_join(data_hydro,data_phyto_FLORPAR_2M)
names <- colnames(data_FLORPAR_S01) # On prend les noms des colonnes pour plus tard
# On obtient un tableau qui a des colonnes en plus parce que ces colonnes ont Code.Region jusqu'a ID.interne.passage nouveaux
data_FLORPARS01vs2M <- right_join(data_FLORPAR_S01,data_FLORPAR_2M,by = join_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage))
# On recupere seulement ces nouvelles colonnes
data_FLORPARS01vs2M <- select(data_FLORPARS01vs2M,Code.Region:ID.interne.passage,Prelevement.niveau.y:`Oxytoxum caudatum.y`)
# On vire le .y inutile
colnames(data_FLORPARS01vs2M)<- names
# On combine les deux
data_FLORPARS012M <- rbind(data_FLORPAR_S01,data_FLORPARS01vs2M)
# En theorie aucun doublon

data_FLORPAR_35M <- right_join(data_hydro,data_phyto_FLORPAR_35M)
data_FLORPARS012Mvs35M <- right_join(data_FLORPARS012M,data_FLORPAR_35M,by = join_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage))
data_FLORPARS012Mvs35M <- select(data_FLORPARS012Mvs35M,Code.Region:ID.interne.passage,Prelevement.niveau.y:`Oxytoxum caudatum.y`)
colnames(data_FLORPARS012Mvs35M)<- names
data_FLORPARS012M35M <- rbind(data_FLORPARS012M,data_FLORPARS012Mvs35M)

doublons_FLORPAR <- data_FLORPARS012M35M[duplicated(data_FLORPARS012M35M$ID.interne.passage) | duplicated(data_FLORPARS012M35M$ID.interne.passage, fromLast = TRUE), ]
# Sauf qu'il y en a... mais on a ajouté 1083 lignes et on se retrouve qu'avec 567 en double...


write.csv2(data_phyto_FLORTOT_S01,file="data_modif/Table2_FLORTOT_S.csv", row.names = FALSE)
write.csv2(data_phyto_FLORTOT_2M,file="data_modif/Table2_FLORTOT_2M.csv", row.names = FALSE)
write.csv2(data_phyto_FLORTOT_35M,file="data_modif/Table2_FLORTOT_35M.csv", row.names = FALSE)

######### UNIQUEMENT LES STATIONS SELECTIONNEES #####
### CRITERE 1 LONGUE SERIE TEMPORELLE ####
Table <- read_delim("data_modif/Table2_FLORTOT_S.csv", 
                                        delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                            grouping_mark = ""), trim_ws = TRUE)
###### SELECTION DES STATIONS #####
Table_St_select <- filter(Table,
                          Code_point_Libelle == "Point 1 Dunkerque" | 
                          Code_point_Libelle == "Point 1 Boulogne" |
                          Code_point_Libelle == "At so" |
                          Code_point_Libelle == "Antoine" | 
                          Code_point_Libelle == "Anse de Carteau 2" |
                          Code_point_Libelle == "Barcares" |
                          Code_point_Libelle == "Bouzigues (a)" |
                          Code_point_Libelle == "Sète mer" |
                          Code_point_Libelle == "Parc Leucate 2"|
                          Code_point_Libelle == "Marseillan (a)"|
                          Code_point_Libelle == "Villefranche"|
                          Code_point_Libelle == "Sud Bastia"|
                          Code_point_Libelle == "Lazaret (a)"|
                          Code_point_Libelle == "Diana centre"|
                          Code_point_Libelle == "Calvi"|
                          Code_point_Libelle == "22B - Toulon gde rade"|
                          Code_point_Libelle == "Ouest Loscolo"|
                          Code_point_Libelle == "Nord Saumonards"|
                          Code_point_Libelle == "Le Croisic (a)"|
                          Code_point_Libelle == "Le Cornard"|
                          Code_point_Libelle == "La Palmyre"|
                          Code_point_Libelle == "L'Eperon (terre)"|
                          Code_point_Libelle == "Filière w"|
                          Code_point_Libelle == "Boyard"|
                          Code_point_Libelle == "Bois de la Chaise large"|
                          Code_point_Libelle == "Bois de la Chaise (a)"|
                          Code_point_Libelle == "Auger"|
                          Code_point_Libelle == "Géfosse"|
                          Code_point_Libelle == "Cabourg"|
                          Code_point_Libelle == "Antifer ponton pétrolier"|
                          Code_point_Libelle == "Teychan bis"|
                          Code_point_Libelle == "Arcachon - Bouée 7"|
                          Code_point_Libelle == "Men er Roue"|
                          Code_point_Libelle == "Men Du"|
                          Code_point_Libelle == "Le Passage (a)"|
                          Code_point_Libelle == "Lanvéoc large"|
                          Code_point_Libelle == "Lanvéoc"|
                          Code_point_Libelle == "Kervel large"|
                          Code_point_Libelle == "Kervel"|
                          Code_point_Libelle == "Concarneau large"|
                          Code_point_Libelle == "St Pol large"|
                          Code_point_Libelle == "St Cast"|
                          Code_point_Libelle == "Pen al Lann"|
                          Code_point_Libelle == "Loguivy"|
                          Code_point_Libelle == "les Hébihens"|
                          Code_point_Libelle == "Bréhat"|
                          Code_point_Libelle == "Donville")

write.csv2(Table_St_select,file="data_modif/Table_FLORTOT_S_select.csv", row.names = FALSE,dec = ".")


### CRITERE 2 SERIE DE 5 ANS MIN####
Table <- read_delim("data_modif/Table2_FLORTOT_S.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                        grouping_mark = ""), trim_ws = TRUE)
###### SELECTION DES STATIONS #####
Table_St_select_5 <- filter(Table,
                          Code_point_Libelle == "Point 1 Dunkerque" | 
                            Code_point_Libelle == "Point 1 Boulogne" |
                            Code_point_Libelle == "At so" |
                            Code_point_Libelle == "Antoine" | 
                            Code_point_Libelle == "Anse de Carteau 2" |
                            Code_point_Libelle == "Barcares" |
                            Code_point_Libelle == "Bouzigues (a)" |
                            Code_point_Libelle == "Sète mer" |
                            Code_point_Libelle == "Parc Leucate 2"|
                            Code_point_Libelle == "Marseillan (a)"|
                            Code_point_Libelle == "Villefranche"|
                            Code_point_Libelle == "Sud Bastia"|
                            Code_point_Libelle == "Lazaret (a)"|
                            Code_point_Libelle == "Diana centre"|
                            Code_point_Libelle == "Calvi"|
                            Code_point_Libelle == "22B - Toulon gde rade"|
                            Code_point_Libelle == "Ouest Loscolo"|
                            Code_point_Libelle == "Nord Saumonards"|
                            Code_point_Libelle == "Le Croisic (a)"|
                            Code_point_Libelle == "Le Cornard"|
                            Code_point_Libelle == "La Palmyre"|
                            Code_point_Libelle == "L'Eperon (terre)"|
                            Code_point_Libelle == "Filière w"|
                            Code_point_Libelle == "Boyard"|
                            Code_point_Libelle == "Bois de la Chaise large"|
                            Code_point_Libelle == "Bois de la Chaise (a)"|
                            Code_point_Libelle == "Auger"|
                            Code_point_Libelle == "Géfosse"|
                            Code_point_Libelle == "Cabourg"|
                            Code_point_Libelle == "Antifer ponton pétrolier"|
                            Code_point_Libelle == "Teychan bis"|
                            Code_point_Libelle == "Arcachon - Bouée 7"|
                            Code_point_Libelle == "Men er Roue"|
                            Code_point_Libelle == "Men Du"|
                            Code_point_Libelle == "Le Passage (a)"|
                            Code_point_Libelle == "Lanvéoc large"|
                            Code_point_Libelle == "Lanvéoc"|
                            Code_point_Libelle == "Kervel large"|
                            Code_point_Libelle == "Kervel"|
                            Code_point_Libelle == "Concarneau large"|
                            Code_point_Libelle == "St Pol large"|
                            Code_point_Libelle == "St Cast"|
                            Code_point_Libelle == "Pen al Lann"|
                            Code_point_Libelle == "Loguivy"|
                            Code_point_Libelle == "les Hébihens"|
                            Code_point_Libelle == "Bréhat"|
                            Code_point_Libelle == "Donville"|
                            Code_point_Libelle == "Bif"|
                            Code_point_Libelle == "Jai"|
                            Code_point_Libelle == "Grand Rhône"|
                            Code_point_Libelle == "Etang d'Urbino - Centre"|
                            Code_point_Libelle == "Vert Bois 2"|
                            Code_point_Libelle == "Pointe Pen Bé"|
                            Code_point_Libelle == "La Carrelère"|
                            Code_point_Libelle == "Beauvoir - le Gois"|
                            Code_point_Libelle == "Basse Michaud" |
                            Code_point_Libelle == "Ouessant - Youc'h korz"|
                            Code_point_Libelle == "Les Glénan"|
                            Code_point_Libelle == "Mont St Michel"|
                            Code_point_Libelle == "Jospinet")

write.csv2(Table_St_select_5,file="data_modif/Table_FLORTOT_S_select_5A.csv", row.names = FALSE,dec = ".")

Table_St_select_5 
# Critere serie temporelle de plus de 20 ans
Table_St_select_20 <- filter(Table,
                             Code_point_Libelle == "Point 1 Dunkerque" | 
                               Code_point_Libelle == "Point 1 Boulogne" |
                               Code_point_Libelle == "At so" |
                               Code_point_Libelle == "Anse de Carteau 2" |
                               Code_point_Libelle == "Villefranche"|
                               Code_point_Libelle == "Lazaret (a)"|
                               Code_point_Libelle == "Le Croisic (a)"|
                               Code_point_Libelle == "Le Cornard"|
                               Code_point_Libelle == "L'Eperon (terre)"|
                               Code_point_Libelle == "Boyard"|
                               Code_point_Libelle == "Cabourg"|
                               Code_point_Libelle == "Antifer ponton pétrolier"|
                               Code_point_Libelle == "Arcachon - Bouée 7"|
                               Code_point_Libelle == "Loguivy"|
                               Code_point_Libelle == "les Hébihens")



# Creation de tableau par facade pour les stations selectionnees
# Channel
Table.Manche_select <- filter(Table_Station_select, Code.Region %in% c(11,12,13))
# Atlantic
Table.Atlantic_select <- filter(Table_Station_select, Code.Region %in% c(21,22,23))
# Mediterranean
Table.Med_select <- filter(Table_Station_select, Code.Region %in% c(31,32))


###################### TRAVAIL SUR LES DEUX METHODES DE CHLOROA ############

# Open with the correct file encoding allows to preserve accents
DataREPHY_MA <- read.csv2('data/REPHY_Manche_Atlantique_1987-2022.csv', fileEncoding = "ISO-8859-1")
DataREPHY_Med <- read.csv2('data/REPHY_Med_1987-2022.csv', fileEncoding = "ISO-8859-1")

# Creating a vector to import all columns as characters
classes_char <- rep('character', 56)
DataREPHY_2023 <- read.csv2('data/Extraction SEANOE_REPHY_phyto-Hydro_Manche_Atl-Med 2023 Semestre1 validé 16012024.csv', 
                            fileEncoding = "ISO-8859-1", colClasses = classes_char)

# Merge the first 2 datasets
DataREPHY <- bind_rows(DataREPHY_MA, DataREPHY_Med) %>%
  # This line allows to remove the problematic row that separates the hydro and phyto datasets
  filter(Passage...Mois != 'Passage : Mois')

# Binding the 2 datasets
DataREPHY_8723 <- bind_rows(DataREPHY, DataREPHY_2023)


### Load tables used to enrich the dataset
# Load table "Zones_marines"
ZM <- read.csv('data/Zones_marines.csv', sep = ';', header = TRUE)

# Load table "Liste_phylum.classe"
PhyClasse <- read.csv('data/Liste_phylum.classe_REPHY.csv', sep =';', header = TRUE, fileEncoding = 'ISO-8859-1')

#### Formatting the dataframe with better column names ####

# Extracting the numeric code for the ZM
Table1 <- DataREPHY_8723 %>%
  mutate(ZM_Quadrige_Numero = as.numeric(str_extract(Lieu.de.surveillance...Entité.de.classement...Libellé, '[:alnum:]+')))

# Change column names (to match ZM)
colnames(Table1)[which(names(Table1) == "Lieu.de.surveillance...Mnémonique")] <- "Code_point_Mnemonique"
colnames(Table1)[which(names(Table1) == "Lieu.de.surveillance...Libellé")] <- "Code_point_Libelle"
colnames(Table1)[which(names(Table1) == "Passage...Date")] <- "Date"
colnames(Table1)[which(names(Table1) == "Coordonnées.passage...Coordonnées.minx")] <- "lon"
colnames(Table1)[which(names(Table1) == "Coordonnées.passage...Coordonnées.miny")] <- "lat"
colnames(Table1)[which(names(Table1) == "Résultat...Libellé.unité.de.mesure.associé.au.quintuplet")] <- "Mesure_Unite"
colnames(Table1)[which(names(Table1) == "Résultat...Symbole.unité.de.mesure.associé.au.quintuplet")] <- "Mesure_Symbole"
colnames(Table1)[which(names(Table1) == "Résultat...Nom.du.taxon.référent")] <- "Taxon"
colnames(Table1)[which(names(Table1) == "Résultat...Libellé.du.groupe.de.taxon")] <- "Groupe_Taxon"
colnames(Table1)[which(names(Table1) == "Résultat...Valeur.de.la.mesure")] <- "Valeur_mesure"
colnames(Table1)[which(names(Table1) == "Prélèvement...Immersion")] <- "Profondeur.metre"
colnames(Table1)[which(names(Table1) == "Prélèvement...Niveau")] <- "Prelevement.niveau"
colnames(Table1)[which(names(Table1) == "Résultat...Code.paramètre")] <- "Code.parametre"
colnames(Table1)[which(names(Table1) == "Résultat...Libellé.paramètre")] <- "Parametre"
colnames(Table1)[which(names(Table1) == "Résultat...Niveau.de.qualité")] <- "Qualite.resultat"
colnames(Table1)[which(names(Table1) == "Prélèvement...Niveau.de.qualité")] <- "Qualite.prelevement"
colnames(Table1)[which(names(Table1) == "Passage...Service.saisisseur...Libellé")] <- "Service.saisie"
colnames(Table1)[which(names(Table1) == "Passage...Heure")] <- "Heure"
colnames(Table1)[which(names(Table1) == "Résultat...Service.analyste...Code")] <- "Service.analyse"
colnames(Table1)[which(names(Table1) == "Prélèvement...Service.préleveur...Code")] <- "Service.prelevement"
colnames(Table1)[which(names(Table1) == "Prélèvement...Identifiant.interne")] <- "ID.interne.prelevement"
colnames(Table1)[which(names(Table1) == "Passage...Identifiant.interne")] <- "ID.interne.passage"
# METHODE 
colnames(Table1)[which(names(Table1) == "Résultat...Libellé.méthode")] <- "Code.methode"


#### Curate table to keep only desired variables ####
Table1 <- Table1 %>%
  dplyr::select(c('ZM_Quadrige_Numero', 'Code_point_Mnemonique', 'Code_point_Libelle', 'Date', 
                  'Heure', 'lon', 'lat', 'Mesure_Unite', 'Mesure_Symbole', 'Taxon', 'Valeur_mesure', 
                  'Prelevement.niveau', 'Profondeur.metre', 'Code.parametre', 'Parametre','Code.methode', 
                  'Qualite.prelevement', 'Qualite.resultat', 'ID.interne.prelevement', 'ID.interne.passage'))

# Modifying date format so that it gives the year and month, and getting rid of rows with no Year value
Table1 <- Table1 %>%
  mutate(Date = dmy(Date)) %>%
  # modifies the date format
  mutate(Day = day(Date)) %>%
  mutate(Month = month(Date, label = F)) %>%
  mutate(Year = year(Date)) %>%
  filter(!is.na(Year))

# Transform the measured values into numbers
# Caution! The decimal separator is a comma, 
# we need first to transform it to a full stop to avoid fuckery
Table1$Valeur_mesure <-  str_replace_all(Table1$Valeur_mesure, ',', '.')
Table1 <- Table1 %>%
  mutate(Valeur_mesure = as.numeric(Valeur_mesure))

#### Tidying table structure ####

## Associate a region with each ZM code
Table1 <- left_join(Table1, ZM, by='ZM_Quadrige_Numero', suffix=c('',''))

# Separate the table into 2 : 1 for hydrology and the other for phytoplankton
Table1_hydro <- Table1 %>%
  filter(Taxon == "")

# Dans un premier temps selectionner que la CHLOROA
Table1_CHLOROA <- filter(Table1_hydro, Code.parametre == "CHLOROA")
levels(as.factor(Table1_CHLOROA$Code.methode))

#### REPRENDRE DIFFEREMMENT SELON ANALYSE TOUTES LES METHODES OU QUE CELLES SELECTIONNEES

# QUE CELLES RECOMMANDEES PAR LE REPHY
Table1_CHLOROA <- filter(Table1_CHLOROA, Code.methode == "Chromatographie liquide - pigments phytoplanctoniques (Van Heukelem et Thomas 2001)"|
 Code.methode == "Fluorimétrie (Aminot A. Kérouel R. 2004 - Chlorophylle)"|
 Code.methode == "Spectrophotométrie monochromatique (Aminot A. Kérouel R. 2004 - Chlorophylle)")

# sinon ne pas lancer ce qui est avant 
Table1_CHLOROA$Code.parametre <- ifelse(Table1_CHLOROA$Code.methode == "Chromatographie liquide - pigments phytoplanctoniques (Van Heukelem et Thomas 2001)", "CHLOROA_CHRL01", 
                                 ifelse(Table1_CHLOROA$Code.methode == "Chromatographie liquide - pigments phytoplanctoniques (Zapata et al. 2000)", "CHLOROA_CHRL00",
                                 ifelse(Table1_CHLOROA$Code.methode == "Fluorimétrie (Aminot A. Kérouel R. 2004 - Chlorophylle)", "CHLOROA_FLUO04",
                                 ifelse(Table1_CHLOROA$Code.methode == "Fluorimétrie (Neveux J. et Panouse M. 1987  - Chlorophylle)", "CHLOROA_FLUO87",
                                 ifelse(Table1_CHLOROA$Code.methode == "Méthode non définie (uniquement pour la reprise)", "CHLOROA_NA",
                                 ifelse(Table1_CHLOROA$Code.methode == "Spectrométrie d'absorption moléculaire (NF T90-117 1999 - Chlorophylle)", "CHLOROA_SPECAM",
                                 ifelse(Table1_CHLOROA$Code.methode == "Spectrophotométrie monochromatique (Aminot A. Kérouel R. 2004 - Chlorophylle)", "CHLOROA_SPECMO04",
                                 ifelse(Table1_CHLOROA$Code.methode == "Spectrophotométrie monochromatique (Aminot et Chaussepied 1983 - Chlorophylle)", "CHLOROA_SPECMO83",
                                 ifelse(Table1_CHLOROA$Code.methode == "Spectrophotométrie trichromatique (Aminot A. Kérouel R. 2004 - Chlorophylle)", "CHLOROA_SPECTRI04",
                                 ifelse(Table1_CHLOROA$Code.methode == "Spectrophotométrie trichromatique (UNESCO 1997 - Chlorophylle)", "CHLOROA_SPECTRI97",
                                        Table1_CHLOROA$Code.parametre))))))))))
levels(as.factor(Table1_CHLOROA$Code.parametre))

# Pour tout

CHRL00 <- filter(Table1_CHLOROA, Code.parametre == levels(as.factor(Table1_CHLOROA$Code.parametre))[1] )
CHRL01 <- filter(Table1_CHLOROA, Code.parametre == levels(as.factor(Table1_CHLOROA$Code.parametre))[2] )
CHLOROA_FLUO04 <- filter(Table1_CHLOROA, Code.parametre == levels(as.factor(Table1_CHLOROA$Code.parametre))[3] )
CHLOROA_FLUO87 <- filter(Table1_CHLOROA, Code.parametre == levels(as.factor(Table1_CHLOROA$Code.parametre))[4] )
CHLOROA_NA <- filter(Table1_CHLOROA, Code.parametre == levels(as.factor(Table1_CHLOROA$Code.parametre))[5] )
CHLOROA_SPECAM <- filter(Table1_CHLOROA, Code.parametre == levels(as.factor(Table1_CHLOROA$Code.parametre))[6] )
CHLOROA_SPECMO04 <- filter(Table1_CHLOROA, Code.parametre == levels(as.factor(Table1_CHLOROA$Code.parametre))[7] )
CHLOROA_SPECMO83 <- filter(Table1_CHLOROA, Code.parametre == levels(as.factor(Table1_CHLOROA$Code.parametre))[8] )
CHLOROA_SPECTRI04 <- filter(Table1_CHLOROA, Code.parametre == levels(as.factor(Table1_CHLOROA$Code.parametre))[9] )
CHLOROA_SPECTRI97 <- filter(Table1_CHLOROA, Code.parametre == levels(as.factor(Table1_CHLOROA$Code.parametre))[10] )

CHRL00w <- CHRL00 |> 
  pivot_wider(names_from = Code.parametre, values_from = Valeur_mesure)
CHRL01d <- CHRL01 |> 
  pivot_wider(names_from = Code.parametre, values_from = Valeur_mesure) # MESURE EN DOUBLE ?

CHRL01_double <- filter(CHRL01, ID.interne.passage == 60741042)

CHRL01w <- CHRL01 |> 
  pivot_wider(names_from = Code.parametre, values_from = Valeur_mesure, values_fn = mean)

CHLOROA_FLUO04w <- CHLOROA_FLUO04 |> 
  pivot_wider(names_from = Code.parametre, values_from = Valeur_mesure)
CHLOROA_FLUO87w <- CHLOROA_FLUO87 |> 
  pivot_wider(names_from = Code.parametre, values_from = Valeur_mesure)
CHLOROA_NAw <- CHLOROA_NA |> 
  pivot_wider(names_from = Code.parametre, values_from = Valeur_mesure)
CHLOROA_SPECAMw<- CHLOROA_SPECAM |> 
  pivot_wider(names_from = Code.parametre, values_from = Valeur_mesure)
CHLOROA_SPECMO04w <- CHLOROA_SPECMO04 |> 
  pivot_wider(names_from = Code.parametre, values_from = Valeur_mesure)
CHLOROA_SPECMO83w <- CHLOROA_SPECMO83 |> 
  pivot_wider(names_from = Code.parametre, values_from = Valeur_mesure,values_fn = mean)
CHLOROA_SPECTRI04w <- CHLOROA_SPECTRI04 |> 
  pivot_wider(names_from = Code.parametre, values_from = Valeur_mesure)
CHLOROA_SPECTRI97w <- CHLOROA_SPECTRI97 |> 
  pivot_wider(names_from = Code.parametre, values_from = Valeur_mesure)

# Pour tout
list_of_dataframes <- list(CHRL00w, CHRL01w, CHLOROA_FLUO04w,CHLOROA_FLUO87w,CHLOROA_NAw,CHLOROA_SPECAMw,CHLOROA_SPECMO04w,
                           CHLOROA_SPECMO83w,CHLOROA_SPECTRI04w,CHLOROA_SPECTRI97w)
# Pour que celles recommandés
LIQ <- filter(Table1_CHLOROA, Code.parametre == levels(as.factor(Table1_CHLOROA$Code.parametre))[1] )
FLUO <- filter(Table1_CHLOROA, Code.parametre == levels(as.factor(Table1_CHLOROA$Code.parametre))[2] )
SPECTRO <- filter(Table1_CHLOROA, Code.parametre == levels(as.factor(Table1_CHLOROA$Code.parametre))[3] )

LIQw <- LIQ |> 
  pivot_wider(names_from = Code.parametre, values_from = Valeur_mesure,values_fn = mean)
FLUOw <- FLUO |> 
  pivot_wider(names_from = Code.parametre, values_from = Valeur_mesure)
SPECTROw <- SPECTRO |> 
  pivot_wider(names_from = Code.parametre, values_from = Valeur_mesure)

list_of_dataframes <- list(LIQw,FLUOw,SPECTROw)

Table2_CHLOROA <- Reduce(function(x, y) full_join(x, y), list_of_dataframes)
write.csv2(Table2_CHLOROA,file="data_modif/Table_CHLOROA_SELECT.csv", row.names = FALSE)
Table2_CHLOROA_S <- filter(Table2_CHLOROA, Prelevement.niveau == "Surface (0-1m)")
write.csv2(Table2_CHLOROA_S,file="data_modif/Table_CHLOROA_S_SELECT.csv", row.names = FALSE)


############ CREATION TABLEAU PHYTO + HYDRO STATION SELECTIONNEES #########
data_hydro <- read_delim("data_modif/Table1_hydro_JY.csv", 
                                            delim = ";", escape_double = FALSE, locale = locale(grouping_mark = "."), 
                                            trim_ws = TRUE)
data_hydro <- select(data_hydro,-c(lon,lat))
data_phyto <- read_delim("data_modif/Table_FLORTOT_S_select_5A.csv", 
                                                      delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                                          grouping_mark = ""), trim_ws = TRUE)
data_hp <- left_join(data_phyto,data_hydro,by = join_by(Code.Region, Code_point_Libelle, Year, Month, Date, ID.interne.passage,
                                                        Prelevement.niveau))
data_hp <- select(data_hp, Code.Region:Code.parametre,SALI:PCYAN,`Asterionellopsis glacialis`:`Oxytoxum caudatum`)

write.csv2(data_hp,file="data_modif/Table_FLORTOT_S_HYDRO_PHYTO.csv", row.names = FALSE,dec = ".")



data_hp <- read_delim("data_modif/Table_FLORTOT_S_HYDRO_PHYTO.csv", 
                                          delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                              grouping_mark = ""), trim_ws = TRUE)

data_hp <- select(data_hp, Code.Region:`NO3+NO2`,`TURB-FNU`,`Asterionellopsis glacialis`:`Oxytoxum caudatum`)

data_hpT <- data_hp |>
mutate(across(-c(Code.Region:'TURB-FNU'), coalesce, 0)) %>%
  # Et on additionne les comptages par genre
  mutate(Alexandrium_g = `Alexandrium` + `Alexandrium andersonii` + `Alexandrium affine` + 
           `Alexandrium minutum` + `Alexandrium catenella` + `Alexandrium leei` +
           `Alexandrium margalefii` + `Alexandrium ostenfeldii` + `Alexandrium pseudogonyaulax` +
           `Alexandrium tamarense` + `Alexandrium tamutum` + `Alexandrium tamarense + catenella + tamutum`
         + `Alexandrium insuetum` + `Alexandrium hiranoi`) %>%
  mutate(Dinophysis_g = `Dinophysis` + `Dinophysis acuminata` + `Dinophysis acuta` + `Dinophysis caudata` +
           `Dinophysis hastata + odiosa` + `Dinophysis fortii` + `Dinophysis sacculus` + `Dinophysis tripos` +
           `Dinophysis odiosa` + `Dinophysis norvegica`) %>%
  mutate(Lepidodinium_g = `Lepidodinium chlorophorum` + `Lepidodinium`)%>%
  mutate(Lingulodinium_g = `Lingulodinium` + `Lingulodinium polyedra`) %>%
  mutate(Mesodinium_g = `Mesodinium` + `Mesodinium rubrum`) %>%
  mutate(Noctiluca_g = `Noctiluca` + `Noctiluca scintillans`)

# On remet des NA quand on a 0 uniquement pour les taxons
data_hpT[,c(21:662)][data_hpT[,c(21:662)] == 0] <- NA

##### NOUVEAU JEU DE DONNEES POUR ANALYSER LA CHLOROPHYLLE ####
# Open with the correct file encoding allows to preserve accents
DataREPHY_MA <- read.csv2('data/REPHY_Manche_Atlantique_1987-2022.csv', fileEncoding = "ISO-8859-1")
DataREPHY_Med <- read.csv2('data/REPHY_Med_1987-2022.csv', fileEncoding = "ISO-8859-1")

# Creating a vector to import all columns as characters
classes_char <- rep('character', 56)
DataREPHY_2023 <- read.csv2('data/Extraction SEANOE_REPHY_phyto-Hydro_Manche_Atl-Med 2023 Semestre1 validé 16012024.csv', 
                            fileEncoding = "ISO-8859-1", colClasses = classes_char)

# Merge the first 2 datasets
DataREPHY <- bind_rows(DataREPHY_MA, DataREPHY_Med) %>%
  # This line allows to remove the problematic row that separates the hydro and phyto datasets
  filter(Passage...Mois != 'Passage : Mois')

# Binding the 2 datasets
DataREPHY_8723 <- bind_rows(DataREPHY, DataREPHY_2023)


### Load tables used to enrich the dataset
# Load table "Zones_marines"
ZM <- read.csv('data/Zones_marines.csv', sep = ';', header = TRUE)

# Load table "Liste_phylum.classe"
PhyClasse <- read.csv('data/Liste_phylum.classe_REPHY.csv', sep =';', header = TRUE, fileEncoding = 'ISO-8859-1')

#### Formatting the dataframe with better column names 

# Extracting the numeric code for the ZM
Table1 <- DataREPHY_8723 %>%
  mutate(ZM_Quadrige_Numero = as.numeric(str_extract(Lieu.de.surveillance...Entité.de.classement...Libellé, '[:alnum:]+')))

# Change column names (to match ZM)
colnames(Table1)[which(names(Table1) == "Lieu.de.surveillance...Mnémonique")] <- "Code_point_Mnemonique"
colnames(Table1)[which(names(Table1) == "Lieu.de.surveillance...Libellé")] <- "Code_point_Libelle"
colnames(Table1)[which(names(Table1) == "Passage...Date")] <- "Date"
colnames(Table1)[which(names(Table1) == "Coordonnées.passage...Coordonnées.minx")] <- "lon"
colnames(Table1)[which(names(Table1) == "Coordonnées.passage...Coordonnées.miny")] <- "lat"
colnames(Table1)[which(names(Table1) == "Résultat...Libellé.unité.de.mesure.associé.au.quintuplet")] <- "Mesure_Unite"
colnames(Table1)[which(names(Table1) == "Résultat...Symbole.unité.de.mesure.associé.au.quintuplet")] <- "Mesure_Symbole"
colnames(Table1)[which(names(Table1) == "Résultat...Nom.du.taxon.référent")] <- "Taxon"
colnames(Table1)[which(names(Table1) == "Résultat...Libellé.du.groupe.de.taxon")] <- "Groupe_Taxon"
colnames(Table1)[which(names(Table1) == "Résultat...Valeur.de.la.mesure")] <- "Valeur_mesure"
colnames(Table1)[which(names(Table1) == "Prélèvement...Immersion")] <- "Profondeur.metre"
colnames(Table1)[which(names(Table1) == "Prélèvement...Niveau")] <- "Prelevement.niveau"
colnames(Table1)[which(names(Table1) == "Résultat...Code.paramètre")] <- "Code.parametre"
colnames(Table1)[which(names(Table1) == "Résultat...Libellé.paramètre")] <- "Parametre"
colnames(Table1)[which(names(Table1) == "Résultat...Niveau.de.qualité")] <- "Qualite.resultat"
colnames(Table1)[which(names(Table1) == "Prélèvement...Niveau.de.qualité")] <- "Qualite.prelevement"
colnames(Table1)[which(names(Table1) == "Passage...Service.saisisseur...Libellé")] <- "Service.saisie"
colnames(Table1)[which(names(Table1) == "Passage...Heure")] <- "Heure"
colnames(Table1)[which(names(Table1) == "Résultat...Service.analyste...Code")] <- "Service.analyse"
colnames(Table1)[which(names(Table1) == "Prélèvement...Service.préleveur...Code")] <- "Service.prelevement"
colnames(Table1)[which(names(Table1) == "Prélèvement...Identifiant.interne")] <- "ID.interne.prelevement"
colnames(Table1)[which(names(Table1) == "Passage...Identifiant.interne")] <- "ID.interne.passage"
# METHODE 
colnames(Table1)[which(names(Table1) == "Résultat...Libellé.méthode")] <- "Code.methode"


#### Curate table to keep only desired variables ####
Table1 <- Table1 %>%
  dplyr::select(c('ZM_Quadrige_Numero', 'Code_point_Mnemonique', 'Code_point_Libelle', 'Date', 
                  'Heure', 'lon', 'lat', 'Mesure_Unite', 'Mesure_Symbole', 'Taxon', 'Valeur_mesure', 
                  'Prelevement.niveau', 'Profondeur.metre', 'Code.parametre', 'Parametre','Code.methode', 
                  'Qualite.prelevement', 'Qualite.resultat', 'ID.interne.prelevement', 'ID.interne.passage'))

# Modifying date format so that it gives the year and month, and getting rid of rows with no Year value
Table1 <- Table1 %>%
  mutate(Date = dmy(Date)) %>%
  # modifies the date format
  mutate(Day = day(Date)) %>%
  mutate(Month = month(Date, label = F)) %>%
  mutate(Year = year(Date)) %>%
  filter(!is.na(Year))

# Transform the measured values into numbers
# Caution! The decimal separator is a comma, 
# we need first to transform it to a full stop to avoid fuckery
Table1$Valeur_mesure <-  str_replace_all(Table1$Valeur_mesure, ',', '.')
Table1 <- Table1 %>%
  mutate(Valeur_mesure = as.numeric(Valeur_mesure))

#### Tidying table structure ####

## Associate a region with each ZM code
Table1 <- left_join(Table1, ZM, by='ZM_Quadrige_Numero', suffix=c('',''))

# Separate the table into 2 : 1 for hydrology and the other for phytoplankton
Table1_hydro <- Table1 %>%
  filter(Taxon == "")

Table1_hydro$Code.methode <- ifelse(Table1_hydro$Code.methode == "Chromatographie liquide - pigments phytoplanctoniques (Van Heukelem et Thomas 2001)", "CHLOROA_CHRL01", 
                                        ifelse(Table1_hydro$Code.methode == "Fluorimétrie (Aminot A. Kérouel R. 2004 - Chlorophylle)", "CHLOROA_FLUO04",
                                        ifelse(Table1_hydro$Code.methode == "Spectrophotométrie monochromatique (Aminot A. Kérouel R. 2004 - Chlorophylle)", "CHLOROA_SPECMO04",
                                                                                  "Non recommande")))
Table1_hydro <- filter(Table1_hydro,
                            Code_point_Libelle == "Point 1 Dunkerque" | 
                              Code_point_Libelle == "Point 1 Boulogne" |
                              Code_point_Libelle == "At so" |
                              Code_point_Libelle == "Antoine" | 
                              Code_point_Libelle == "Anse de Carteau 2" |
                              Code_point_Libelle == "Barcares" |
                              Code_point_Libelle == "Bouzigues (a)" |
                              Code_point_Libelle == "Sète mer" |
                              Code_point_Libelle == "Parc Leucate 2"|
                              Code_point_Libelle == "Marseillan (a)"|
                              Code_point_Libelle == "Villefranche"|
                              Code_point_Libelle == "Sud Bastia"|
                              Code_point_Libelle == "Lazaret (a)"|
                              Code_point_Libelle == "Diana centre"|
                              Code_point_Libelle == "Calvi"|
                              Code_point_Libelle == "22B - Toulon gde rade"|
                              Code_point_Libelle == "Ouest Loscolo"|
                              Code_point_Libelle == "Nord Saumonards"|
                              Code_point_Libelle == "Le Croisic (a)"|
                              Code_point_Libelle == "Le Cornard"|
                              Code_point_Libelle == "La Palmyre"|
                              Code_point_Libelle == "L'Eperon (terre)"|
                              Code_point_Libelle == "Filière w"|
                              Code_point_Libelle == "Boyard"|
                              Code_point_Libelle == "Bois de la Chaise large"|
                              Code_point_Libelle == "Bois de la Chaise (a)"|
                              Code_point_Libelle == "Auger"|
                              Code_point_Libelle == "Géfosse"|
                              Code_point_Libelle == "Cabourg"|
                              Code_point_Libelle == "Antifer ponton pétrolier"|
                              Code_point_Libelle == "Teychan bis"|
                              Code_point_Libelle == "Arcachon - Bouée 7"|
                              Code_point_Libelle == "Men er Roue"|
                              Code_point_Libelle == "Men Du"|
                              Code_point_Libelle == "Le Passage (a)"|
                              Code_point_Libelle == "Lanvéoc large"|
                              Code_point_Libelle == "Lanvéoc"|
                              Code_point_Libelle == "Kervel large"|
                              Code_point_Libelle == "Kervel"|
                              Code_point_Libelle == "Concarneau large"|
                              Code_point_Libelle == "St Pol large"|
                              Code_point_Libelle == "St Cast"|
                              Code_point_Libelle == "Pen al Lann"|
                              Code_point_Libelle == "Loguivy"|
                              Code_point_Libelle == "les Hébihens"|
                              Code_point_Libelle == "Bréhat"|
                              Code_point_Libelle == "Donville"|
                              Code_point_Libelle == "Bif"|
                              Code_point_Libelle == "Jai"|
                              Code_point_Libelle == "Grand Rhône"|
                              Code_point_Libelle == "Etang d'Urbino - Centre"|
                              Code_point_Libelle == "Vert Bois 2"|
                              Code_point_Libelle == "Pointe Pen Bé"|
                              Code_point_Libelle == "La Carrelère"|
                              Code_point_Libelle == "Beauvoir - le Gois"|
                              Code_point_Libelle == "Basse Michaud" |
                              Code_point_Libelle == "Ouessant - Youc'h korz"|
                              Code_point_Libelle == "Les Glénan"|
                              Code_point_Libelle == "Mont St Michel"|
                              Code_point_Libelle == "Jospinet")

Table1_hydro_select <- Table1_hydro %>%
  select(c('ID.interne.passage', 'Qualite.resultat', 'Code.parametre', 'Valeur_mesure', 'Prelevement.niveau',
           'Code.Region', 'Region', 'Date', 'Day', 'Month', 'Year', 'Code_point_Libelle', 'Code.methode',
           'Code_point_Mnemonique', 'lon', 'lat')) %>%
  filter(Prelevement.niveau == 'Surface (0-1m)') %>%
  group_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage, Prelevement.niveau, Code.parametre) %>%
  summarise(Valeur_mesure = mean(Valeur_mesure), .groups = 'keep') %>%
  pivot_wider(names_from = Code.parametre, values_from = Valeur_mesure)


Table1_hydro_join <- Table1_hydro %>%
  select(c('ID.interne.passage', 'Prelevement.niveau',
           'Code.Region', 'Code_point_Libelle', 'Code.methode','Code.parametre','Valeur_mesure',
           'lon', 'lat','Date','Year','Month')) |>
  filter(Prelevement.niveau == 'Surface (0-1m)') |>
  filter(Code.parametre == "CHLOROA") |>
  group_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage, Prelevement.niveau, Code.parametre,Code.methode) %>%
  # There is probably something shifty here, regarding multiple CHLOROA measurements at certain stations,
  # made with different methods. For now we average everything but this is quite bad.
  summarise(Valeur_mesure = mean(Valeur_mesure), .groups = 'keep') %>%
  pivot_wider(names_from = Code.parametre, values_from = Valeur_mesure) 

Table1_hydro_select <- select(Table1_hydro_select,-'CHLOROA')

Table1_hydro_chloro <- right_join(Table1_hydro_join,Table1_hydro_select)
colnames(Table1_hydro_chloro)[10] <- "Methode.chloro"

write.csv2(Table1_hydro_chloro,file="data_modif/Table_S_select5A_chloro.csv", row.names = FALSE,dec = ".")
 

##### NOUVEAU JEU DE DONNEES POUR ANALYSER LA CHLOROPHYLLE sans selection station mais profondeur oui ####
# Open with the correct file encoding allows to preserve accents
DataREPHY_MA <- read.csv2('data/REPHY_Manche_Atlantique_1987-2022.csv', fileEncoding = "ISO-8859-1")
DataREPHY_Med <- read.csv2('data/REPHY_Med_1987-2022.csv', fileEncoding = "ISO-8859-1")

# Creating a vector to import all columns as characters
classes_char <- rep('character', 56)
DataREPHY_2023 <- read.csv2('data/Extraction SEANOE_REPHY_phyto-Hydro_Manche_Atl-Med 2023 Semestre1 validé 16012024.csv', 
                            fileEncoding = "ISO-8859-1", colClasses = classes_char)

# Merge the first 2 datasets
DataREPHY <- bind_rows(DataREPHY_MA, DataREPHY_Med) %>%
  # This line allows to remove the problematic row that separates the hydro and phyto datasets
  filter(Passage...Mois != 'Passage : Mois')

# Binding the 2 datasets
DataREPHY_8723 <- bind_rows(DataREPHY, DataREPHY_2023)


### Load tables used to enrich the dataset
# Load table "Zones_marines"
ZM <- read.csv('data/Zones_marines.csv', sep = ';', header = TRUE)

# Load table "Liste_phylum.classe"
PhyClasse <- read.csv('data/Liste_phylum.classe_REPHY.csv', sep =';', header = TRUE, fileEncoding = 'ISO-8859-1')

#### Formatting the dataframe with better column names

# Extracting the numeric code for the ZM
Table1 <- DataREPHY_8723 %>%
  mutate(ZM_Quadrige_Numero = as.numeric(str_extract(Lieu.de.surveillance...Entité.de.classement...Libellé, '[:alnum:]+')))

# Change column names (to match ZM)
colnames(Table1)[which(names(Table1) == "Lieu.de.surveillance...Mnémonique")] <- "Code_point_Mnemonique"
colnames(Table1)[which(names(Table1) == "Lieu.de.surveillance...Libellé")] <- "Code_point_Libelle"
colnames(Table1)[which(names(Table1) == "Passage...Date")] <- "Date"
colnames(Table1)[which(names(Table1) == "Coordonnées.passage...Coordonnées.minx")] <- "lon"
colnames(Table1)[which(names(Table1) == "Coordonnées.passage...Coordonnées.miny")] <- "lat"
colnames(Table1)[which(names(Table1) == "Résultat...Libellé.unité.de.mesure.associé.au.quintuplet")] <- "Mesure_Unite"
colnames(Table1)[which(names(Table1) == "Résultat...Symbole.unité.de.mesure.associé.au.quintuplet")] <- "Mesure_Symbole"
colnames(Table1)[which(names(Table1) == "Résultat...Nom.du.taxon.référent")] <- "Taxon"
colnames(Table1)[which(names(Table1) == "Résultat...Libellé.du.groupe.de.taxon")] <- "Groupe_Taxon"
colnames(Table1)[which(names(Table1) == "Résultat...Valeur.de.la.mesure")] <- "Valeur_mesure"
colnames(Table1)[which(names(Table1) == "Prélèvement...Immersion")] <- "Profondeur.metre"
colnames(Table1)[which(names(Table1) == "Prélèvement...Niveau")] <- "Prelevement.niveau"
colnames(Table1)[which(names(Table1) == "Résultat...Code.paramètre")] <- "Code.parametre"
colnames(Table1)[which(names(Table1) == "Résultat...Libellé.paramètre")] <- "Parametre"
colnames(Table1)[which(names(Table1) == "Résultat...Niveau.de.qualité")] <- "Qualite.resultat"
colnames(Table1)[which(names(Table1) == "Prélèvement...Niveau.de.qualité")] <- "Qualite.prelevement"
colnames(Table1)[which(names(Table1) == "Passage...Service.saisisseur...Libellé")] <- "Service.saisie"
colnames(Table1)[which(names(Table1) == "Passage...Heure")] <- "Heure"
colnames(Table1)[which(names(Table1) == "Résultat...Service.analyste...Code")] <- "Service.analyse"
colnames(Table1)[which(names(Table1) == "Prélèvement...Service.préleveur...Code")] <- "Service.prelevement"
colnames(Table1)[which(names(Table1) == "Prélèvement...Identifiant.interne")] <- "ID.interne.prelevement"
colnames(Table1)[which(names(Table1) == "Passage...Identifiant.interne")] <- "ID.interne.passage"
# METHODE 
colnames(Table1)[which(names(Table1) == "Résultat...Libellé.méthode")] <- "Code.methode"


#### Curate table to keep only desired variables ####
Table1 <- Table1 %>%
  dplyr::select(c('ZM_Quadrige_Numero', 'Code_point_Mnemonique', 'Code_point_Libelle', 'Date', 
                  'Heure', 'lon', 'lat', 'Mesure_Unite', 'Mesure_Symbole', 'Taxon', 'Valeur_mesure', 
                  'Prelevement.niveau', 'Profondeur.metre', 'Code.parametre', 'Parametre','Code.methode', 
                  'Qualite.prelevement', 'Qualite.resultat', 'ID.interne.prelevement', 'ID.interne.passage'))

# Modifying date format so that it gives the year and month, and getting rid of rows with no Year value
Table1 <- Table1 %>%
  mutate(Date = dmy(Date)) %>%
  # modifies the date format
  mutate(Day = day(Date)) %>%
  mutate(Month = month(Date, label = F)) %>%
  mutate(Year = year(Date)) %>%
  filter(!is.na(Year))

# Transform the measured values into numbers
# Caution! The decimal separator is a comma, 
# we need first to transform it to a full stop to avoid fuckery
Table1$Valeur_mesure <-  str_replace_all(Table1$Valeur_mesure, ',', '.')
Table1 <- Table1 %>%
  mutate(Valeur_mesure = as.numeric(Valeur_mesure))

#### Tidying table structure ####

## Associate a region with each ZM code
Table1 <- left_join(Table1, ZM, by='ZM_Quadrige_Numero', suffix=c('',''))

# Separate the table into 2 : 1 for hydrology and the other for phytoplankton
Table1_hydro <- Table1 %>%
  filter(Taxon == "")

Table1_hydro$Code.methode <- ifelse(Table1_hydro$Code.methode == "Chromatographie liquide - pigments phytoplanctoniques (Van Heukelem et Thomas 2001)", "CHLOROA_CHRL01",
                                    ifelse(Table1_hydro$Code.methode == "Chromatographie liquide - pigments phytoplanctoniques (Zapata et al. 2000)", "CHLOROA_CHRL00",
                                    ifelse(Table1_hydro$Code.methode == "Fluorimétrie (Aminot A. Kérouel R. 2004 - Chlorophylle)", "CHLOROA_FLUO04",
                                           ifelse(Table1_hydro$Code.methode == "Fluorimétrie (Neveux J. et Panouse M. 1987  - Chlorophylle)", "CHLOROA_FLUO87",
                                           ifelse(Table1_hydro$Code.methode == "Spectrophotométrie monochromatique (Aminot A. Kérouel R. 2004 - Chlorophylle)", "CHLOROA_SPECMO04",
                                                  ifelse(Table1_hydro$Code.methode == "Spectrophotométrie monochromatique (Aminot et Chaussepied 1983 - Chlorophylle)", "CHLOROA_SPECMO83",
                                                  "Non recommande"))))))


Table1_hydro_select <- Table1_hydro %>%
  select(c('ID.interne.passage', 'Qualite.resultat', 'Code.parametre', 'Valeur_mesure', 'Prelevement.niveau',
           'Code.Region', 'Region', 'Date', 'Day', 'Month', 'Year', 'Code_point_Libelle', 'Code.methode',
           'Code_point_Mnemonique', 'lon', 'lat')) %>%
  filter(Prelevement.niveau == 'Surface (0-1m)') %>%
  group_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage, Prelevement.niveau, Code.parametre) %>%
  summarise(Valeur_mesure = mean(Valeur_mesure), .groups = 'keep') %>%
  pivot_wider(names_from = Code.parametre, values_from = Valeur_mesure)


Table1_hydro_join <- Table1_hydro %>%
  select(c('ID.interne.passage', 'Prelevement.niveau',
           'Code.Region', 'Code_point_Libelle', 'Code.methode','Code.parametre','Valeur_mesure',
           'lon', 'lat','Date','Year','Month')) |>
  filter(Prelevement.niveau == 'Surface (0-1m)') |>
  filter(Code.parametre == "CHLOROA") |>
  group_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage, Prelevement.niveau, Code.parametre,Code.methode) %>%
  # There is probably something shifty here, regarding multiple CHLOROA measurements at certain stations,
  # made with different methods. For now we average everything but this is quite bad.
  summarise(Valeur_mesure = mean(Valeur_mesure), .groups = 'keep') %>%
  pivot_wider(names_from = Code.parametre, values_from = Valeur_mesure) 

Table1_hydro_select <- select(Table1_hydro_select,-'CHLOROA')

Table1_hydro_chloro <- right_join(Table1_hydro_join,Table1_hydro_select)
colnames(Table1_hydro_chloro)[10] <- "Methode.chloro"

write.csv2(Table1_hydro_chloro,file="data_modif/Table_S_chloro_oldnew.csv", row.names = FALSE,dec = ".")






##### CE QUI EST AVANT PEUT ETRE VIRER ######
############ NOUVEAUX JEUX DE DONNEES POUR SELECTION FINALE ############
#### Import data 

# Open with the correct file encoding allows to preserve accents
DataREPHY_MA <- read.csv2('data/REPHY_Manche_Atlantique_1987-2022.csv', fileEncoding = "ISO-8859-1")
DataREPHY_Med <- read.csv2('data/REPHY_Med_1987-2022.csv', fileEncoding = "ISO-8859-1")

# Creating a vector to import all columns as characters
classes_char <- rep('character', 56)
DataREPHY_2023 <- read.csv2('data/Extraction SEANOE_REPHY_phyto-Hydro_Manche_Atl-Med 2023 Semestre1 validé 16012024.csv', 
                            fileEncoding = "ISO-8859-1", colClasses = classes_char)

# Merge the first 2 datasets
DataREPHY <- bind_rows(DataREPHY_MA, DataREPHY_Med) %>%
  # This line allows to remove the problematic row that separates the hydro and phyto datasets
  filter(Passage...Mois != 'Passage : Mois')

# Binding the 2 datasets
DataREPHY_8723 <- bind_rows(DataREPHY, DataREPHY_2023)


### Load tables used to enrich the dataset
# Load table "Zones_marines"
ZM <- read.csv('data/Zones_marines.csv', sep = ';', header = TRUE)

# Load table "Liste_phylum.classe"
PhyClasse <- read.csv('data/Liste_phylum.classe_REPHY.csv', sep =';', header = TRUE, fileEncoding = 'ISO-8859-1')

#### Formatting the dataframe with better column names

# Extracting the numeric code for the ZM
Table1 <- DataREPHY_8723 %>%
  mutate(ZM_Quadrige_Numero = as.numeric(str_extract(Lieu.de.surveillance...Entité.de.classement...Libellé, '[:alnum:]+')))

# Change column names (to match ZM)
colnames(Table1)[which(names(Table1) == "Lieu.de.surveillance...Mnémonique")] <- "Code_point_Mnemonique"
colnames(Table1)[which(names(Table1) == "Lieu.de.surveillance...Libellé")] <- "Code_point_Libelle"
colnames(Table1)[which(names(Table1) == "Passage...Date")] <- "Date"
colnames(Table1)[which(names(Table1) == "Coordonnées.passage...Coordonnées.minx")] <- "lon"
colnames(Table1)[which(names(Table1) == "Coordonnées.passage...Coordonnées.miny")] <- "lat"
colnames(Table1)[which(names(Table1) == "Résultat...Libellé.unité.de.mesure.associé.au.quintuplet")] <- "Mesure_Unite"
colnames(Table1)[which(names(Table1) == "Résultat...Symbole.unité.de.mesure.associé.au.quintuplet")] <- "Mesure_Symbole"
colnames(Table1)[which(names(Table1) == "Résultat...Nom.du.taxon.référent")] <- "Taxon"
colnames(Table1)[which(names(Table1) == "Résultat...Libellé.du.groupe.de.taxon")] <- "Groupe_Taxon"
colnames(Table1)[which(names(Table1) == "Résultat...Valeur.de.la.mesure")] <- "Valeur_mesure"
colnames(Table1)[which(names(Table1) == "Prélèvement...Immersion")] <- "Profondeur.metre"
colnames(Table1)[which(names(Table1) == "Prélèvement...Niveau")] <- "Prelevement.niveau"
colnames(Table1)[which(names(Table1) == "Résultat...Code.paramètre")] <- "Code.parametre"
colnames(Table1)[which(names(Table1) == "Résultat...Libellé.paramètre")] <- "Parametre"
colnames(Table1)[which(names(Table1) == "Résultat...Niveau.de.qualité")] <- "Qualite.resultat"
colnames(Table1)[which(names(Table1) == "Prélèvement...Niveau.de.qualité")] <- "Qualite.prelevement"
colnames(Table1)[which(names(Table1) == "Passage...Service.saisisseur...Libellé")] <- "Service.saisie"
colnames(Table1)[which(names(Table1) == "Passage...Heure")] <- "Heure"
colnames(Table1)[which(names(Table1) == "Résultat...Service.analyste...Code")] <- "Service.analyse"
colnames(Table1)[which(names(Table1) == "Prélèvement...Service.préleveur...Code")] <- "Service.prelevement"
colnames(Table1)[which(names(Table1) == "Prélèvement...Identifiant.interne")] <- "ID.interne.prelevement"
colnames(Table1)[which(names(Table1) == "Passage...Identifiant.interne")] <- "ID.interne.passage"

# Convertir les ml/l en mg/l pour l'oxygene
Table1$Valeur_mesure <-  str_replace_all(Table1$Valeur_mesure, ',', '.')
Table1 <- Table1 %>%
  mutate(Valeur_mesure = as.numeric(Valeur_mesure))

Table1$Valeur_mesure <- ifelse(Table1$Code.parametre == "OXYGENE" & Table1$Mesure_Symbole == "ml.l-1", Table1$Valeur_mesure * 1.429 , Table1$Valeur_mesure)
# On indique le changement 
Table1$Mesure_Symbole <- ifelse(Table1$Code.parametre == "OXYGENE" & Table1$Mesure_Symbole == "ml.l-1", "mg.l-1 converted" , Table1$Mesure_Symbole)


#### Curate table to keep only desired variables
Table1 <- Table1 %>%
  dplyr::select(c('ZM_Quadrige_Numero', 'Code_point_Mnemonique', 'Code_point_Libelle', 'Date', 
                  'Heure', 'lon', 'lat', 'Mesure_Unite', 'Mesure_Symbole', 'Taxon', 'Valeur_mesure', 'Résultat...Libellé.méthode',
                  'Prelevement.niveau', 'Profondeur.metre', 'Code.parametre', 'Parametre', 
                  'Qualite.prelevement', 'Qualite.resultat', 'ID.interne.prelevement', 'ID.interne.passage'))

# Modifying date format so that it gives the year and month, and getting rid of rows with no Year value
Table1 <- Table1 %>%
  mutate(Date = dmy(Date)) %>%
  # modifies the date format
  mutate(Day = day(Date)) %>%
  mutate(Month = month(Date, label = F)) %>%
  mutate(Year = year(Date)) %>%
  filter(!is.na(Year))

#### Tidying table structure 
## Associate a region with each ZM code
Table1 <- left_join(Table1, ZM, by='ZM_Quadrige_Numero', suffix=c('',''))

# Separate the table for hydrology and phytoplankton
Table1_hydro <- Table1 %>%
  filter(Taxon == "")

Table1_phyto <- Table1 %>%
  filter(Taxon != "")

# HYDRO SEULEMENT
# Selection de la profondeur entre 0 et 5m
Table1_hydro$Profondeur.metre <- as.numeric(Table1_hydro$Profondeur.metre)
Table1_hydro <- Table1_hydro |> filter(Profondeur.metre <= 5 | is.na(Profondeur.metre)) |> #Profondeur pas toujours indiquee
  filter(Year >= 1995) 
Table1_hydro_b <- Table1_hydro
# Garder la chlorophylle qu'on veut
Table1_hydro <- filter(Table1_hydro, Code.parametre != "CHLOROA")
Table1_hydro_chloro <- Table1_hydro_b |>
  filter(Code.parametre == "CHLOROA") |>
  filter(Résultat...Libellé.méthode == "Chromatographie liquide - pigments phytoplanctoniques (Van Heukelem et Thomas 2001)" |
         Résultat...Libellé.méthode == "Chromatographie liquide - pigments phytoplanctoniques (Zapata et al. 2000)" |
         Résultat...Libellé.méthode == "Fluorimétrie (Aminot A. Kérouel R. 2004 - Chlorophylle)" |
         Résultat...Libellé.méthode == "Fluorimétrie (Neveux J. et Panouse M. 1987  - Chlorophylle)" |
         Résultat...Libellé.méthode == "Spectrophotométrie monochromatique (Aminot A. Kérouel R. 2004 - Chlorophylle)" |
         Résultat...Libellé.méthode == "Spectrophotométrie monochromatique (Aminot et Chaussepied 1983 - Chlorophylle)" )

Table1_hydro_chloro <- dplyr::select(Table1_hydro_chloro, Date, ID.interne.passage,
                              Prelevement.niveau,Valeur_mesure,Résultat...Libellé.méthode)

colnames(Table1_hydro_chloro)[4:5] <- c("CHLOROA","Methode_CHLOROA")

# Regler le probleme des doublons a cause de double mesure de chloro avec des methodes differentes
doublons_chloro <- Table1_hydro_chloro[duplicated(Table1_hydro_chloro$ID.interne.passage) |
                                 duplicated(Table1_hydro_chloro$ID.interne.passage, fromLast = TRUE), ]
# Filtre des doublons hydro :
resultat_filtre_chloro <- doublons_chloro %>%
  filter(Methode_CHLOROA %in% levels(as.factor(doublons_chloro$Methode_CHLOROA))) %>%
  group_by(ID.interne.passage) %>%
  mutate(Ordre = match(Methode_CHLOROA, c("Spectrophotométrie monochromatique (Aminot A. Kérouel R. 2004 - Chlorophylle)",
                                          "Fluorimétrie (Aminot A. Kérouel R. 2004 - Chlorophylle)",
                                          "Chromatographie liquide - pigments phytoplanctoniques (Van Heukelem et Thomas 2001)",
                                          "Spectrophotométrie monochromatique (Aminot et Chaussepied 1983 - Chlorophylle)",
                                          "Fluorimétrie (Neveux J. et Panouse M. 1987  - Chlorophylle)",
                                          "Chromatographie liquide - pigments phytoplanctoniques (Zapata et al. 2000)"))) %>%
  arrange(desc(Ordre)) %>%
  filter(duplicated(ID.interne.passage) | n()==1)

# On supprime les lignes en doublon dans le jeu de données initial
Table1_hydro_chloro_unique <- subset(Table1_hydro_chloro, !(ID.interne.passage %in% unique(doublons_chloro$ID.interne.passage)))
# On les remets ces doublons filtres
Table1_hydro_chloro <- bind_rows(Table1_hydro_chloro_unique,resultat_filtre_chloro)
  

Table1_hydro <- Table1_hydro |>
  filter(Code.Region != 0) %>%
  filter(Prelevement.niveau == "Surface (0-1m)" |Prelevement.niveau == "2 mètres" |Prelevement.niveau == "de 3 à 5 mètres" |Prelevement.niveau == "Mi-profondeur" ) |>
  #filter(Qualite.resultat == 'Bon') %>%
  group_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage, Prelevement.niveau, Profondeur.metre, Code.parametre) %>%
  # There is probably something shifty here, regarding multiple CHLOROA measurements at certain stations,
  # made with different methods. For now we average everything but this is quite bad.
  summarise(Valeur_mesure = mean(Valeur_mesure), .groups = 'keep') %>%
  pivot_wider(names_from = Code.parametre, values_from = Valeur_mesure)

# Choix niveaux par ordre
#detection des doublons
doublons_hydro <- Table1_hydro[duplicated(Table1_hydro$ID.interne.passage) |
                        duplicated(Table1_hydro$ID.interne.passage, fromLast = TRUE), ]

# Filtre des doublons hydro :
resultat_filtre <- doublons_hydro %>%
  filter(Prelevement.niveau %in% c("Surface (0-1m)", "2 mètres", "de 3 à 5 mètres","Mi-profondeur")) %>%
  group_by(ID.interne.passage) %>%
  mutate(Ordre = match(Prelevement.niveau, c("Surface (0-1m)", "2 mètres", "de 3 à 5 mètres","Mi-profondeur"))) %>%
  arrange(desc(Ordre)) %>%
  filter(duplicated(ID.interne.passage) | n()==1)

# On supprime les lignes en doublon dans le jeu de données initial
Table1_unique <- subset(Table1_hydro, !(ID.interne.passage %in% unique(doublons_hydro$ID.interne.passage)))
# On les remets ces doublons filtres
Table1_hydro <- bind_rows(Table1_unique,resultat_filtre)
# On remet au propre
Table1_hydro <- Table1_hydro |>
  group_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage, Prelevement.niveau,Profondeur.metre)


# S'occuper du jeu de donnees phyto
# Selection de la profondeur entre 0 et 5m
Table1_phyto$Profondeur.metre <- as.numeric(Table1_phyto$Profondeur.metre)
Table1_phyto <- Table1_phyto |> filter(Profondeur.metre <= 5 | is.na(Profondeur.metre)) |> #Profondeur pas toujours indiquee
  filter(Year >= 1995) |>
  filter(Code.parametre == "FLORTOT")

Table1_phyto <- Table1_phyto |>
  filter(Code.Region != 0) %>%
  filter(Prelevement.niveau == "Surface (0-1m)" |Prelevement.niveau == "2 mètres" |Prelevement.niveau == "de 3 à 5 mètres" |Prelevement.niveau == "Mi-profondeur" ) |>
  #filter(Qualite.resultat == 'Bon') %>%
  group_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage, Prelevement.niveau,Profondeur.metre, Code.parametre)

### Manipulating the phyto table ###
# Associate the Phylum/Class to the taxon
Table1_phyto <- left_join(Table1_phyto, PhyClasse, by='Taxon', suffix=c('',''))

# Compute genus from Taxon
# We find the genus by selecting only the first word in the string 'Taxon'
Table1_phyto <- Table1_phyto %>%
  mutate(Genus = str_extract(Taxon, 'Pseudo-nitzschia|[:alnum:]+'))

# On ne preserve que les Bacillariophyceae et Dinophyceae (et Mesodinium, cilie)
Table1_phyto_select <- Table1_phyto |>
  filter(Phylum.Classe == "Bacillariophyceae" | Phylum.Classe == "Dinophyceae" |
           Genus == "Mesodinium")

Table1_phyto_genus <- Table1_phyto_select %>%
  group_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage, Prelevement.niveau, Profondeur.metre,Code.parametre, Genus) %>%
  summarise(Comptage = sum(Valeur_mesure), .groups = 'keep') %>%
  pivot_wider(names_from = Genus, values_from = Comptage)

Table1_phyto_class <- Table1_phyto[Table1_phyto$Phylum.Classe != "", ]

Table1_phyto_classe <- Table1_phyto_class %>%
  group_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage, Prelevement.niveau, Profondeur.metre,Code.parametre, Phylum.Classe) %>%
  summarise(Comptage = sum(Valeur_mesure), .groups = 'keep') %>%
  pivot_wider(names_from = Phylum.Classe, values_from = Comptage)

# Choix niveaux par ordre
# detection doublons phyto
doublons_phyto_genus <- Table1_phyto_genus[duplicated(Table1_phyto_genus$ID.interne.passage) |
                       duplicated(Table1_phyto_genus$ID.interne.passage, fromLast = TRUE), ]

# Filtre des doublons phyto :
resultat_filtre_genus <- doublons_phyto_genus %>%
  filter(Prelevement.niveau %in% c("Surface (0-1m)", "2 mètres", "de 3 à 5 mètres","Mi-profondeur")) %>%
  group_by(ID.interne.passage) %>%
  mutate(Ordre = match(Prelevement.niveau, c("Surface (0-1m)", "2 mètres", "de 3 à 5 mètres","Mi-profondeur"))) %>%
  arrange(desc(Ordre)) %>%
  filter(duplicated(ID.interne.passage) | n()==1)

# On supprime les lignes en doublon dans le jeu de données initial
Table1_phyto_unique <- subset(Table1_phyto_genus, !(ID.interne.passage %in% unique(doublons_phyto_genus$ID.interne.passage)))
# On les remets ces doublons filtres
Table1_phyto_genus <- bind_rows(Table1_phyto_unique,resultat_filtre_genus)
# On remet au propre
Table1_phyto_genus <- Table1_phyto_genus |>
  group_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage, Prelevement.niveau)

Table1_hydro <- dplyr::select(ungroup(Table1_hydro),Date:Prelevement.niveau,SALI:PCYAN )

data_hp <- left_join(Table1_phyto_genus,Table1_hydro,by = join_by(Date, ID.interne.passage,
                                                        Prelevement.niveau))

Table1_phyto_classe <- select(ungroup(Table1_phyto_classe),Date, ID.interne.passage, Prelevement.niveau,Bacillariophyceae:Xanthophyceae)

data_hpc <- left_join(data_hp,Table1_phyto_classe,by = join_by(Date, ID.interne.passage,
                                                                  Prelevement.niveau))

data_hpc <- dplyr::select(data_hpc, Code.Region:Code.parametre, SALI:`NO3+NO2`,`TURB-FNU`, `Actinoptychus`:`Coscinodiscophycidae`,Bacillariophyceae.y:Xanthophyceae)

# Avec filtrage de la chlorophylle
data_hpc2 <- left_join(data_hpc, Table1_hydro_chloro)

data_hpc2 <- dplyr::select(data_hpc2, Code.Region:Code.parametre,Methode_CHLOROA,CHLOROA, SALI:`NO3+NO2`, `TURB-FNU`, `Actinoptychus`:`Coscinodiscophycidae`,Bacillariophyceae.y:Xanthophyceae)

# Les profondeurs manquantes correspondent uniquement a de la "Surface (0-1m)" donc on dis que correspond 
# a une profondeur de 0.5 les Profondeur en NA
data_hpc2$Profondeur.metre <- ifelse(is.na(data_hpc2$Profondeur.metre),0.5,data_hpc2$Profondeur.metre) 

data_hpc2 <- data_hpc2 |>
  group_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage, Prelevement.niveau)


## On remet au propre
#data_hpc_ok <- data_hpc_ok |>
#  group_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage, Prelevement.niveau,Profondeur.metre)

#data_hpc_ok <- select(data_hpc_ok, -Ordre)
#colnames(data_hpc_ok)[246:247] <- c("Bacillariophyceae","Dinophyceae")


colnames(data_hpc2)[247:248] <- c("Bacillariophyceae","Dinophyceae")
# Il reste des doublons 
# detection doublons phyto
doublons_final <- data_hpc2[duplicated(data_hpc2$ID.interne.passage) |
                                             duplicated(data_hpc2$ID.interne.passage, fromLast = TRUE), ]
# Filtre des doublons phyto :
resultat_filtre_final <- doublons_final %>%
  filter(Prelevement.niveau %in% c("Surface (0-1m)", "2 mètres", "de 3 à 5 mètres","Mi-profondeur")) %>%
  group_by(ID.interne.passage) %>%
  mutate(Ordre = match(Prelevement.niveau, c("Surface (0-1m)", "2 mètres", "de 3 à 5 mètres","Mi-profondeur"))) %>%
  arrange(desc(Ordre)) %>%
  filter(duplicated(ID.interne.passage) | n()==1)

# On supprime les lignes en doublon dans le jeu de données initial
data_hpc2_unique <- subset(data_hpc2, !(ID.interne.passage %in% unique(doublons_final$ID.interne.passage)))
# On les remets ces doublons filtres
data_hpc3 <- bind_rows(data_hpc2_unique,resultat_filtre_final)
# On remet au propre
data_hpc3 <- data_hpc3 |>
  group_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage, Prelevement.niveau)

data_hpc3 <- select(data_hpc3, -Ordre)

write.csv2(data_hpc3,file="data_modif/Table_FLORTOT_Surf_9523_hydro_phyto_chloro_phylum.csv", row.names = FALSE,dec = ".")

########## SELECTION STATION APRES ANALYSE ECHANTILLONNAGE ########
Table <- read_delim("data_modif/Table_FLORTOT_Surf_9523_hydro_phyto_chloro_phylum.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                        grouping_mark = ""), trim_ws = TRUE)

Table_select <- filter(Table, Code_point_Libelle == "Point 1 Dunkerque" |
                         Code_point_Libelle == "Point 1 Boulogne" |
                         Code_point_Libelle == "Bif" |
                         Code_point_Libelle == "At so"|
                         Code_point_Libelle == "Sète mer"|
                         Code_point_Libelle == "Parc Leucate 2"|
                         Code_point_Libelle == "Grand Rhône"|
                         Code_point_Libelle == "Bouzigues (a)"|
                         Code_point_Libelle == "Barcares"|
                         Code_point_Libelle == "Antoine"|
                         Code_point_Libelle == "Anse de Carteau 2"|
                         Code_point_Libelle == "Géfosse"|
                         Code_point_Libelle == "Cabourg"|
                         Code_point_Libelle == "Antifer ponton pétrolier"|
                         Code_point_Libelle == "Villefranche"|
                         Code_point_Libelle == "Sud Bastia"|
                         Code_point_Libelle == "Lazaret (a)"|
                         Code_point_Libelle == "Endoume"|
                         Code_point_Libelle == "Diana centre"|
                         Code_point_Libelle == "Calvi"|
                         Code_point_Libelle == "22B - Toulon gde rade"|
                         Code_point_Libelle == "Ouest Loscolo"|
                         Code_point_Libelle == "Nord Saumonards"|
                         Code_point_Libelle == "Le Croisic (a)"|
                         Code_point_Libelle == "Le Cornard"|
                         Code_point_Libelle == "La Palmyre"|
                         Code_point_Libelle == "Filière w"|
                         Code_point_Libelle == "Boyard"|
                         Code_point_Libelle == "Bois de la Chaise large"|
                         Code_point_Libelle == "Basse Michaud"|
                         Code_point_Libelle == "Auger" |
                         Code_point_Libelle == "Teychan bis"|
                         Code_point_Libelle == "Arcachon - Bouée 7"|
                         Code_point_Libelle == "Ouessant - Youc'h korz"|
                         Code_point_Libelle == "Men er Roue"|
                         Code_point_Libelle == "Men Du"|
                         Code_point_Libelle == "Le Passage (a)"|
                         Code_point_Libelle == "Lanvéoc large"|
                         Code_point_Libelle == "Kervel large"|
                         Code_point_Libelle == "Concarneau large"|
                         Code_point_Libelle == "St Pol large"|
                         Code_point_Libelle == "St Cast"|
                         Code_point_Libelle == "Pen al Lann"|
                         Code_point_Libelle == "Loguivy"|
                         Code_point_Libelle == "les Hébihens"|
                         Code_point_Libelle == "Donville"|
                         Code_point_Libelle == "Bréhat" |
                         Code_point_Libelle == "Marseillan (a)"
)

# Creation de tableau par facade pour les stations selectionnees
# Channel
Table.Manche_select <- filter(Table_select, Code.Region %in% c(11,12,13))
# Atlantic
Table.Atlantic_select <- filter(Table_select, Code.Region %in% c(21,22,23))
# Mediterranean
Table.Med_select <- filter(Table_select, Code.Region %in% c(31,32))

# Selection avec periode par station interet 
Table_select <- filter(Table, Code_point_Libelle == "Point 1 Dunkerque" & (Date >= as.Date("2002-04-01") & Date <= as.Date("2012-11-30"))|
                         Code_point_Libelle == "Point 1 Boulogne" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Bif" & (Date >= as.Date("2009-08-01") & Date <= as.Date("2017-06-30"))|
                         Code_point_Libelle == "At so" & (Date >= as.Date("2003-03-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Sète mer" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2023-01-31"))|
                         Code_point_Libelle == "Parc Leucate 2" & (Date >= as.Date("1995-02-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Grand Rhône" & (Date >= as.Date("2009-03-01") & Date <= as.Date("2015-08-31"))|
                         Code_point_Libelle == "Bouzigues (a)" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2023-02-28"))|
                         Code_point_Libelle == "Barcares" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Antoine" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2003-01-31"))|
                         Code_point_Libelle == "Anse de Carteau 2" & (Date >= as.Date("2003-02-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Géfosse" & (Date >= as.Date("2004-01-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Cabourg" & (Date >= as.Date("2002-03-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Antifer ponton pétrolier" & (Date >= as.Date("2002-04-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Villefranche" & (Date >= as.Date("1998-10-01") & Date <= as.Date("2014-04-30"))|
                         Code_point_Libelle == "Sud Bastia" & (Date >= as.Date("2007-04-01") & Date <= as.Date("2020-02-29"))|
                         Code_point_Libelle == "Lazaret (a)" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2015-09-30"))|
                         Code_point_Libelle == "Endoume" & (Date >= as.Date("2006-03-01") & Date <= as.Date("2016-02-29"))|
                         Code_point_Libelle == "Diana centre" & (Date >= as.Date("2001-01-01") & Date <= as.Date("2022-11-30"))|
                         Code_point_Libelle == "Calvi" & (Date >= as.Date("2006-02-01") & Date <= as.Date("2022-10-31"))|
                         Code_point_Libelle == "22B - Toulon gde rade" & (Date >= as.Date("2006-02-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Ouest Loscolo" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Nord Saumonards" & (Date >= as.Date("2009-03-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Le Croisic (a)" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2015-12-31"))|
                         Code_point_Libelle == "Le Cornard" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "La Palmyre" & (Date >= as.Date("2011-01-01") & Date <= as.Date("2020-06-30"))|
                         Code_point_Libelle == "Filière w" & (Date >= as.Date("2012-03-01") & Date <= as.Date("2023-05-31"))|
                         Code_point_Libelle == "Boyard" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2015-07-30"))|
                         Code_point_Libelle == "Bois de la Chaise large" & (Date >= as.Date("2007-01-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Basse Michaud" & (Date >= as.Date("2016-01-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Auger" & (Date >= as.Date("1995-04-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Teychan bis" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Arcachon - Bouée 7" & (Date >= as.Date("2003-02-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Ouessant - Youc'h korz" & (Date >= as.Date("2015-03-01") & Date <= as.Date("2022-01-31"))|
                         Code_point_Libelle == "Men er Roue" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Men Du" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2003-12-31"))|
                         Code_point_Libelle == "Le Passage (a)" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2000-12-31"))|
                         Code_point_Libelle == "Lanvéoc large" & (Date >= as.Date("2010-01-01") & Date <= as.Date("2020-08-31"))|
                         Code_point_Libelle == "Kervel large" & (Date >= as.Date("2013-11-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Concarneau large" & (Date >= as.Date("2010-01-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "St Pol large" & (Date >= as.Date("2013-01-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "St Cast" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2004-03-31"))|
                         Code_point_Libelle == "Pen al Lann" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2006-02-28"))|
                         Code_point_Libelle == "Loguivy" & (Date >= as.Date("2007-03-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "les Hébihens" & (Date >= as.Date("2007-03-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Donville" & (Date >= as.Date("2002-04-01") & Date <= as.Date("2014-08-31"))|
                         Code_point_Libelle == "Bréhat" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2003-06-30"))|
                         Code_point_Libelle == "Marseillan (a)" & (Date >= as.Date("2009-03-01") & Date <= as.Date("2023-02-28"))
)


write.csv2(Table_select,file="data_modif/Table_FLORTOT_Surf_9523_Stselect_hydro_phyto_chloro_phylum_period.csv", row.names = FALSE,dec = ".")


# Creation de tableau par facade pour les stations selectionnees
# Channel
Table.Manche_select <- filter(Table_select, Code.Region %in% c(11,12,13))
# Atlantic
Table.Atlantic_select <- filter(Table_select, Code.Region %in% c(21,22,23))
# Mediterranean
Table.Med_select <- filter(Table_select, Code.Region %in% c(31,32))



Table <- read_delim("data_modif/Table_FLORTOT_Surf_9523_hydro_phyto_chloro_phylum.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                        grouping_mark = ""), trim_ws = TRUE)


# Selection avec periode par station interet sur le critere de continuite de la chlorophylle CRITERE 5 ANS
Table_select <- filter(Table, Code_point_Libelle == "Point 1 Dunkerque" & (Date >= as.Date("2002-04-01") & Date <= as.Date("2012-11-30"))|
                         Code_point_Libelle == "Point 1 Boulogne" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Bif" & (Date >= as.Date("2009-08-01") & Date <= as.Date("2017-06-30"))|
                         Code_point_Libelle == "At so" & (Date >= as.Date("2003-03-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Sète mer" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2023-01-31"))|
                         Code_point_Libelle == "Parc Leucate 2" & (Date >= as.Date("1995-02-01") & Date <= as.Date("2022-08-31"))|
                         Code_point_Libelle == "Grand Rhône" & (Date >= as.Date("2008-01-01") & Date <= as.Date("2015-08-31"))|
                         Code_point_Libelle == "Bouzigues (a)" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2023-02-28"))|
                         Code_point_Libelle == "Barcares" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2022-10-31"))|
                         Code_point_Libelle == "Anse de Carteau 2" & (Date >= as.Date("2003-02-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Géfosse" & (Date >= as.Date("2004-01-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Cabourg" & (Date >= as.Date("2002-03-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Antifer ponton pétrolier" & (Date >= as.Date("2002-04-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Villefranche" & (Date >= as.Date("1998-10-01") & Date <= as.Date("2014-04-30"))| # A VOIR
                         Code_point_Libelle == "Sud Bastia" & (Date >= as.Date("2007-04-01") & Date <= as.Date("2020-02-29"))|
                         Code_point_Libelle == "Lazaret (a)" & (Date >= as.Date("1999-01-01") & Date <= as.Date("2015-09-30"))|
                         Code_point_Libelle == "Diana centre" & (Date >= as.Date("2001-02-01") & Date <= as.Date("2022-11-30"))|
                         Code_point_Libelle == "Calvi" & (Date >= as.Date("2006-02-01") & Date <= as.Date("2022-08-31"))|
                         Code_point_Libelle == "22B - Toulon gde rade" & (Date >= as.Date("2006-02-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Ouest Loscolo" & (Date >= as.Date("1996-01-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Nord Saumonards" & (Date >= as.Date("2009-03-01") & Date <= as.Date("2023-04-30"))|
                         Code_point_Libelle == "Le Croisic (a)" & (Date >= as.Date("2000-02-01") & Date <= as.Date("2015-12-31"))|
                         Code_point_Libelle == "Le Cornard" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2023-05-31"))|
                         Code_point_Libelle == "La Palmyre" & (Date >= as.Date("2011-01-01") & Date <= as.Date("2020-03-31"))|
                         Code_point_Libelle == "Filière w" & (Date >= as.Date("2012-03-01") & Date <= as.Date("2023-05-31"))|
                         Code_point_Libelle == "Boyard" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2015-07-30"))|
                         Code_point_Libelle == "Bois de la Chaise large" & (Date >= as.Date("2007-03-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Basse Michaud" & (Date >= as.Date("2016-01-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Auger" & (Date >= as.Date("1995-04-01") & Date <= as.Date("2023-03-31"))|
                         Code_point_Libelle == "Teychan bis" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Arcachon - Bouée 7" & (Date >= as.Date("2014-10-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Ouessant - Youc'h korz" & (Date >= as.Date("2016-01-01") & Date <= as.Date("2022-01-31"))|
                         Code_point_Libelle == "Men er Roue" & (Date >= as.Date("1996-01-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Men Du" & (Date >= as.Date("1995-02-01") & Date <= as.Date("2003-12-31"))|
                         Code_point_Libelle == "Le Passage (a)" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2000-12-31"))|
                         Code_point_Libelle == "Lanvéoc large" & (Date >= as.Date("2010-01-01") & Date <= as.Date("2020-08-31"))|
                         Code_point_Libelle == "Kervel large" & (Date >= as.Date("2013-11-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Concarneau large" & (Date >= as.Date("2010-01-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "St Pol large" & (Date >= as.Date("2013-01-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "St Cast" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2004-03-31"))|
                         Code_point_Libelle == "Pen al Lann" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2006-02-28"))|
                         Code_point_Libelle == "Loguivy" & (Date >= as.Date("2007-03-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "les Hébihens" & (Date >= as.Date("2007-03-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Donville" & (Date >= as.Date("2002-04-01") & Date <= as.Date("2014-08-31"))|
                         Code_point_Libelle == "Bréhat" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2003-06-30"))|
                         Code_point_Libelle == "Marseillan (a)" & (Date >= as.Date("2009-03-01") & Date <= as.Date("2023-02-28"))
)


write.csv2(Table_select,file="data_modif/Table_FLORTOT_Surf_9523_Stselect_hydro_phyto_chloro_phylum_period5_chlafilter.csv", row.names = FALSE,dec = ".")



Table <- read_delim("data_modif/Table_FLORTOT_Surf_9523_hydro_phyto_chloro_phylum.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                        grouping_mark = ""), trim_ws = TRUE)


# Selection avec periode par station interet sur le critere de continuite de la chlorophylle CRITERE 5 ANS
Table_select <- filter(Table, Code_point_Libelle == "Point 1 Boulogne" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "At so" & (Date >= as.Date("2003-03-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Sète mer" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2023-01-31"))|
                         Code_point_Libelle == "Parc Leucate 2" & (Date >= as.Date("1995-02-01") & Date <= as.Date("2022-08-31"))|
                         Code_point_Libelle == "Bouzigues (a)" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2023-02-28"))|
                         Code_point_Libelle == "Barcares" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2022-10-31"))|
                         Code_point_Libelle == "Anse de Carteau 2" & (Date >= as.Date("2003-02-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Géfosse" & (Date >= as.Date("2004-01-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Cabourg" & (Date >= as.Date("2002-03-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Antifer ponton pétrolier" & (Date >= as.Date("2002-04-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Villefranche" & (Date >= as.Date("1998-10-01") & Date <= as.Date("2014-04-30"))| # A VOIR
                         Code_point_Libelle == "Lazaret (a)" & (Date >= as.Date("1999-01-01") & Date <= as.Date("2015-09-30"))|
                         Code_point_Libelle == "Diana centre" & (Date >= as.Date("2001-02-01") & Date <= as.Date("2022-11-30"))|
                         Code_point_Libelle == "Calvi" & (Date >= as.Date("2006-02-01") & Date <= as.Date("2022-08-31"))|
                         Code_point_Libelle == "22B - Toulon gde rade" & (Date >= as.Date("2006-02-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Ouest Loscolo" & (Date >= as.Date("1996-01-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Le Croisic (a)" & (Date >= as.Date("2000-02-01") & Date <= as.Date("2015-12-31"))|
                         Code_point_Libelle == "Le Cornard" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2023-05-31"))|
                         Code_point_Libelle == "Boyard" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2015-07-30"))|
                         Code_point_Libelle == "Bois de la Chaise large" & (Date >= as.Date("2007-03-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Auger" & (Date >= as.Date("1995-04-01") & Date <= as.Date("2023-03-31"))|
                         Code_point_Libelle == "Teychan bis" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Men er Roue" & (Date >= as.Date("1996-01-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Loguivy" & (Date >= as.Date("2007-03-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "les Hébihens" & (Date >= as.Date("2007-03-01") & Date <= as.Date("2023-06-30"))
)


write.csv2(Table_select,file="data_modif/Table_FLORTOT_Surf_9523_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter.csv", row.names = FALSE,dec = ".")

############## JEU DE DONNEES AVEC PERIODE COMMUNE POUR TENDANCE TEMPORELLE #######################
data <- read_delim("data_modif/Table_FLORTOT_Surf_9523_Stselect_hydro_phyto_chloro_phylum_period5_chlafilter.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

Table_select <- filter(data, Code_point_Libelle == "Teychan bis" |
                         Code_point_Libelle == "Ouest Loscolo" |
                         Code_point_Libelle == "Men er Roue" |
                         Code_point_Libelle == "Le Cornard" |
                         Code_point_Libelle == "Bois de la Chaise large" |
                         Code_point_Libelle == "Auger" |
                         Code_point_Libelle == "Point 1 Boulogne" |
                         Code_point_Libelle == "Loguivy" |
                         Code_point_Libelle == "les Hébihens" |
                         Code_point_Libelle == "Géfosse" |
                         Code_point_Libelle == "Cabourg" |
                         Code_point_Libelle == "At so" |
                         Code_point_Libelle == "Antifer ponton pétrolier" |
                         Code_point_Libelle == "Sète mer" |
                         Code_point_Libelle == "Parc Leucate 2" |
                         Code_point_Libelle == "Diana centre" |
                         Code_point_Libelle == "Calvi" |
                         Code_point_Libelle == "Bouzigues (a)" |
                         Code_point_Libelle == "Barcares" |
                         Code_point_Libelle == "Anse de Carteau 2"|
                         Code_point_Libelle == "22B - Toulon gde rade"
                         
)
Table_select <- filter(Table_select, Date >= as.Date("2007-03-01") & Date <= as.Date("2022-08-31"))

write.csv2(Table_select,file="data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter.csv", row.names = FALSE,dec = ".")


########## Associer l'information du clustering sur les donnees ######
#### 5 ans ####
data <- read_delim("data_modif/Table_FLORTOT_Surf_9523_Stselect_hydro_phyto_chloro_phylum_period5_chlafilter.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

data_clust <- read_delim("data_modif/clusters_EM_01_k5.csv", 
                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                             grouping_mark = ""), trim_ws = TRUE)
# Pour s'affranchir des changements de position des stations
data_clust <- dplyr::select(data_clust, Code_point_Libelle, cluster)

# Association
data_clusterised <- left_join(data,data_clust)
# Rearrangement de l'ordre

data_ok <- dplyr::select(data_clusterised,Code.Region:Code_point_Libelle,cluster,lon:Xanthophyceae)

write.csv2(data_ok,file="data_modif/Table_FLORTOT_Surf_9523_Stselect_hydro_phyto_chloro_phylum_period5_chlafilter_cluster.csv", row.names = FALSE,dec = ".")

#### 15 ans mais avec les clusters de 5 ans ####

data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)


data_clust <- read_delim("data_modif/clusters_EM_01_k5.csv", 
                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                             grouping_mark = ""), trim_ws = TRUE)
# Pour s'affranchir des changements de position des stations
data_clust <- dplyr::select(data_clust, Code_point_Libelle, cluster)

# Association
data_clusterised <- left_join(data,data_clust)
# Rearrangement de l'ordre

data_ok <- dplyr::select(data_clusterised,Code.Region:Code_point_Libelle,cluster,lon:Xanthophyceae)

write.csv2(data_ok,file="data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5.csv", row.names = FALSE,dec = ".")


########## Rajouter les indices de diversite dans les données 
data <- read_delim("data_modif/Table_FLORTOT_Surf_9523_Stselect_hydro_phyto_chloro_phylum_period5_chlafilter_cluster.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

TableIDiv <- data
TableIDiv <- dplyr::select(TableIDiv, ID.interne.passage, Asterionellopsis:Coscinodiscophycidae)

Data_results <- as.data.frame(c("",""))
Data_results[1,1] <- 0
Data_results[1,2] <- 0

library(OTUtable)

# Sur toutes les annees UNIQUEMENT sur les genres
for (i in 1:17110){
  Table2 <- TableIDiv[i,]
  abondance_sums <- apply(Table2[,2:224],2,sum,na.rm=T)
  print(i/17110 * 100)
  Data_results[i,1] <- diversity(abondance_sums,index = "shannon")
  Data_results[i,2] <- diversity(abondance_sums,index = "simpson")
  Data_results[i,3] <- max(abondance_sums) / sum(abondance_sums)
  Data_results[i,4] <- pielou(sample = abondance_sums)
  Data_results[i,5] <- TableIDiv[i,1]
}
colnames(Data_results) <- c("Shannon","Simpson","BergerParker","Pielou","ID.interne.passage")

Data_results$Shannon <- as.numeric(Data_results$Shannon)

data_ok <- left_join(data,Data_results)

write.csv2(data_ok,file="data_modif/Table_FLORTOT_Surf_9523_Stselect_hydro_phyto_chloro_phylum_period5_chlafilter_cluster_div.csv", row.names = FALSE,dec = ".")



########## Rajouter les indices de diversite dans les données 
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

TableIDiv <- data
TableIDiv <- dplyr::select(TableIDiv, ID.interne.passage, Asterionellopsis:Coscinodiscophycidae)

Data_results <- as.data.frame(c("",""))
Data_results[1,1] <- 0
Data_results[1,2] <- 0

library(OTUtable)

# Sur toutes les annees UNIQUEMENT sur les genres
for (i in 1:7582){
  Table2 <- TableIDiv[i,]
  abondance_sums <- apply(Table2[,2:224],2,sum,na.rm=T)
  print(i/7582 * 100)
  Data_results[i,1] <- diversity(abondance_sums,index = "shannon")
  Data_results[i,2] <- diversity(abondance_sums,index = "simpson")
  Data_results[i,3] <- max(abondance_sums) / sum(abondance_sums)
  Data_results[i,4] <- pielou(sample = abondance_sums)
  Data_results[i,5] <- TableIDiv[i,1]
}
colnames(Data_results) <- c("Shannon","Simpson","BergerParker","Pielou","ID.interne.passage")

Data_results$Shannon <- as.numeric(Data_results$Shannon)

data_ok <- left_join(data,Data_results)

write.csv2(data_ok,file="data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div.csv", row.names = FALSE,dec = ".")








##### POUR TABLEAU CROISE DYNAMIQUE ######
############ NOUVEAUX JEUX DE DONNEES POUR SELECTION FINALE ############
#### Import data 

# Open with the correct file encoding allows to preserve accents
DataREPHY_MA <- read.csv2('data/REPHY_Manche_Atlantique_1987-2022.csv', fileEncoding = "ISO-8859-1")
DataREPHY_Med <- read.csv2('data/REPHY_Med_1987-2022.csv', fileEncoding = "ISO-8859-1")

# Creating a vector to import all columns as characters
classes_char <- rep('character', 56)
DataREPHY_2023 <- read.csv2('data/Extraction SEANOE_REPHY_phyto-Hydro_Manche_Atl-Med 2023 Semestre1 validé 16012024.csv', 
                            fileEncoding = "ISO-8859-1", colClasses = classes_char)

# Merge the first 2 datasets
DataREPHY <- bind_rows(DataREPHY_MA, DataREPHY_Med) %>%
  # This line allows to remove the problematic row that separates the hydro and phyto datasets
  filter(Passage...Mois != 'Passage : Mois')

# Binding the 2 datasets
DataREPHY_8723 <- bind_rows(DataREPHY, DataREPHY_2023)


### Load tables used to enrich the dataset
# Load table "Zones_marines"
ZM <- read.csv('data/Zones_marines.csv', sep = ';', header = TRUE)

# Load table "Liste_phylum.classe"
PhyClasse <- read.csv('data/Liste_phylum.classe_REPHY.csv', sep =';', header = TRUE, fileEncoding = 'ISO-8859-1')

#### Formatting the dataframe with better column names

# Extracting the numeric code for the ZM
Table1 <- DataREPHY_8723 %>%
  mutate(ZM_Quadrige_Numero = as.numeric(str_extract(Lieu.de.surveillance...Entité.de.classement...Libellé, '[:alnum:]+')))

# Change column names (to match ZM)
colnames(Table1)[which(names(Table1) == "Lieu.de.surveillance...Mnémonique")] <- "Code_point_Mnemonique"
colnames(Table1)[which(names(Table1) == "Lieu.de.surveillance...Libellé")] <- "Code_point_Libelle"
colnames(Table1)[which(names(Table1) == "Passage...Date")] <- "Date"
colnames(Table1)[which(names(Table1) == "Coordonnées.passage...Coordonnées.minx")] <- "lon"
colnames(Table1)[which(names(Table1) == "Coordonnées.passage...Coordonnées.miny")] <- "lat"
colnames(Table1)[which(names(Table1) == "Résultat...Libellé.unité.de.mesure.associé.au.quintuplet")] <- "Mesure_Unite"
colnames(Table1)[which(names(Table1) == "Résultat...Symbole.unité.de.mesure.associé.au.quintuplet")] <- "Mesure_Symbole"
colnames(Table1)[which(names(Table1) == "Résultat...Nom.du.taxon.référent")] <- "Taxon"
colnames(Table1)[which(names(Table1) == "Résultat...Libellé.du.groupe.de.taxon")] <- "Groupe_Taxon"
colnames(Table1)[which(names(Table1) == "Résultat...Valeur.de.la.mesure")] <- "Valeur_mesure"
colnames(Table1)[which(names(Table1) == "Prélèvement...Immersion")] <- "Profondeur.metre"
colnames(Table1)[which(names(Table1) == "Prélèvement...Niveau")] <- "Prelevement.niveau"
colnames(Table1)[which(names(Table1) == "Résultat...Code.paramètre")] <- "Code.parametre"
colnames(Table1)[which(names(Table1) == "Résultat...Libellé.paramètre")] <- "Parametre"
colnames(Table1)[which(names(Table1) == "Résultat...Niveau.de.qualité")] <- "Qualite.resultat"
colnames(Table1)[which(names(Table1) == "Prélèvement...Niveau.de.qualité")] <- "Qualite.prelevement"
colnames(Table1)[which(names(Table1) == "Passage...Service.saisisseur...Libellé")] <- "Service.saisie"
colnames(Table1)[which(names(Table1) == "Passage...Heure")] <- "Heure"
colnames(Table1)[which(names(Table1) == "Résultat...Service.analyste...Code")] <- "Service.analyse"
colnames(Table1)[which(names(Table1) == "Prélèvement...Service.préleveur...Code")] <- "Service.prelevement"
colnames(Table1)[which(names(Table1) == "Prélèvement...Identifiant.interne")] <- "ID.interne.prelevement"
colnames(Table1)[which(names(Table1) == "Passage...Identifiant.interne")] <- "ID.interne.passage"

# Convertir les ml/l en mg/l pour l'oxygene
Table1$Valeur_mesure <-  str_replace_all(Table1$Valeur_mesure, ',', '.')
Table1 <- Table1 %>%
  mutate(Valeur_mesure = as.numeric(Valeur_mesure))

Table1$Valeur_mesure <- ifelse(Table1$Code.parametre == "OXYGENE" & Table1$Mesure_Symbole == "ml.l-1", Table1$Valeur_mesure * 1.429 , Table1$Valeur_mesure)
# On indique le changement 
Table1$Mesure_Symbole <- ifelse(Table1$Code.parametre == "OXYGENE" & Table1$Mesure_Symbole == "ml.l-1", "mg.l-1 converted" , Table1$Mesure_Symbole)


#### Curate table to keep only desired variables
Table1 <- Table1 %>%
  dplyr::select(c('ZM_Quadrige_Numero', 'Code_point_Mnemonique', 'Code_point_Libelle', 'Date', 
                  'Heure', 'lon', 'lat', 'Mesure_Unite', 'Mesure_Symbole', 'Taxon', 'Valeur_mesure', 'Résultat...Libellé.méthode',
                  'Prelevement.niveau', 'Profondeur.metre', 'Code.parametre', 'Parametre', 
                  'Qualite.prelevement', 'Qualite.resultat', 'ID.interne.prelevement', 'ID.interne.passage'))

# Modifying date format so that it gives the year and month, and getting rid of rows with no Year value
Table1 <- Table1 %>%
  mutate(Date = dmy(Date)) %>%
  # modifies the date format
  mutate(Day = day(Date)) %>%
  mutate(Month = month(Date, label = F)) %>%
  mutate(Year = year(Date)) %>%
  filter(!is.na(Year))

#### Tidying table structure 
## Associate a region with each ZM code
Table1 <- left_join(Table1, ZM, by='ZM_Quadrige_Numero', suffix=c('',''))

# Separate the table for hydrology and phytoplankton
Table1_hydro <- Table1 %>%
  filter(Taxon == "")

Table1_phyto <- Table1 %>%
  filter(Taxon != "")

# HYDRO SEULEMENT
# Selection de la profondeur entre 0 et 5m
Table1_hydro$Profondeur.metre <- as.numeric(Table1_hydro$Profondeur.metre)
Table1_hydro <- Table1_hydro |> filter(Profondeur.metre <= 5 | is.na(Profondeur.metre)) |> #Profondeur pas toujours indiquee
  filter(Year >= 1995) 
Table1_hydro_b <- Table1_hydro
# Garder la chlorophylle qu'on veut
Table1_hydro <- filter(Table1_hydro, Code.parametre != "CHLOROA")
Table1_hydro_chloro <- Table1_hydro_b |>
  filter(Code.parametre == "CHLOROA") |>
  filter(Résultat...Libellé.méthode == "Chromatographie liquide - pigments phytoplanctoniques (Van Heukelem et Thomas 2001)" |
           Résultat...Libellé.méthode == "Chromatographie liquide - pigments phytoplanctoniques (Zapata et al. 2000)" |
           Résultat...Libellé.méthode == "Fluorimétrie (Aminot A. Kérouel R. 2004 - Chlorophylle)" |
           Résultat...Libellé.méthode == "Fluorimétrie (Neveux J. et Panouse M. 1987  - Chlorophylle)" |
           Résultat...Libellé.méthode == "Spectrophotométrie monochromatique (Aminot A. Kérouel R. 2004 - Chlorophylle)" |
           Résultat...Libellé.méthode == "Spectrophotométrie monochromatique (Aminot et Chaussepied 1983 - Chlorophylle)" )

Table1_hydro_chloro <- dplyr::select(Table1_hydro_chloro, Date, ID.interne.passage,
                                     Prelevement.niveau,Valeur_mesure,Résultat...Libellé.méthode)

colnames(Table1_hydro_chloro)[4:5] <- c("CHLOROA","Methode_CHLOROA")

# Regler le probleme des doublons a cause de double mesure de chloro avec des methodes differentes
doublons_chloro <- Table1_hydro_chloro[duplicated(Table1_hydro_chloro$ID.interne.passage) |
                                         duplicated(Table1_hydro_chloro$ID.interne.passage, fromLast = TRUE), ]
# Filtre des doublons hydro :
resultat_filtre_chloro <- doublons_chloro %>%
  filter(Methode_CHLOROA %in% levels(as.factor(doublons_chloro$Methode_CHLOROA))) %>%
  group_by(ID.interne.passage) %>%
  mutate(Ordre = match(Methode_CHLOROA, c("Spectrophotométrie monochromatique (Aminot A. Kérouel R. 2004 - Chlorophylle)",
                                          "Fluorimétrie (Aminot A. Kérouel R. 2004 - Chlorophylle)",
                                          "Chromatographie liquide - pigments phytoplanctoniques (Van Heukelem et Thomas 2001)",
                                          "Spectrophotométrie monochromatique (Aminot et Chaussepied 1983 - Chlorophylle)",
                                          "Fluorimétrie (Neveux J. et Panouse M. 1987  - Chlorophylle)",
                                          "Chromatographie liquide - pigments phytoplanctoniques (Zapata et al. 2000)"))) %>%
  arrange(desc(Ordre)) %>%
  filter(duplicated(ID.interne.passage) | n()==1)

# On supprime les lignes en doublon dans le jeu de données initial
Table1_hydro_chloro_unique <- subset(Table1_hydro_chloro, !(ID.interne.passage %in% unique(doublons_chloro$ID.interne.passage)))
# On les remets ces doublons filtres
Table1_hydro_chloro <- bind_rows(Table1_hydro_chloro_unique,resultat_filtre_chloro)


Table1_hydro <- Table1_hydro |>
  filter(Code.Region != 0) %>%
  filter(Prelevement.niveau == "Surface (0-1m)" |Prelevement.niveau == "2 mètres" |Prelevement.niveau == "de 3 à 5 mètres" |Prelevement.niveau == "Mi-profondeur" ) |>
  #filter(Qualite.resultat == 'Bon') %>%
  group_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage, Prelevement.niveau, Profondeur.metre, Code.parametre) %>%
  # There is probably something shifty here, regarding multiple CHLOROA measurements at certain stations,
  # made with different methods. For now we average everything but this is quite bad.
  summarise(Valeur_mesure = mean(Valeur_mesure), .groups = 'keep') %>%
  pivot_wider(names_from = Code.parametre, values_from = Valeur_mesure)

# Choix niveaux par ordre
#detection des doublons
doublons_hydro <- Table1_hydro[duplicated(Table1_hydro$ID.interne.passage) |
                                 duplicated(Table1_hydro$ID.interne.passage, fromLast = TRUE), ]

# Filtre des doublons hydro :
resultat_filtre <- doublons_hydro %>%
  filter(Prelevement.niveau %in% c("Surface (0-1m)", "2 mètres", "de 3 à 5 mètres","Mi-profondeur")) %>%
  group_by(ID.interne.passage) %>%
  mutate(Ordre = match(Prelevement.niveau, c("Surface (0-1m)", "2 mètres", "de 3 à 5 mètres","Mi-profondeur"))) %>%
  arrange(desc(Ordre)) %>%
  filter(duplicated(ID.interne.passage) | n()==1)

# On supprime les lignes en doublon dans le jeu de données initial
Table1_unique <- subset(Table1_hydro, !(ID.interne.passage %in% unique(doublons_hydro$ID.interne.passage)))
# On les remets ces doublons filtres
Table1_hydro <- bind_rows(Table1_unique,resultat_filtre)
# On remet au propre
Table1_hydro <- Table1_hydro |>
  group_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage, Prelevement.niveau,Profondeur.metre)


# S'occuper du jeu de donnees phyto
# Selection de la profondeur entre 0 et 5m
Table1_phyto$Profondeur.metre <- as.numeric(Table1_phyto$Profondeur.metre)
Table1_phyto <- Table1_phyto |> filter(Profondeur.metre <= 5 | is.na(Profondeur.metre)) |> #Profondeur pas toujours indiquee
  filter(Year >= 1995) |>
  filter(Code.parametre == "FLORTOT")

Table1_phyto <- Table1_phyto |>
  filter(Code.Region != 0) %>%
  filter(Prelevement.niveau == "Surface (0-1m)" |Prelevement.niveau == "2 mètres" |Prelevement.niveau == "de 3 à 5 mètres" |Prelevement.niveau == "Mi-profondeur" ) |>
  #filter(Qualite.resultat == 'Bon') %>%
  group_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage, Prelevement.niveau,Profondeur.metre, Code.parametre)

### Manipulating the phyto table ###
# Associate the Phylum/Class to the taxon
Table1_phyto <- left_join(Table1_phyto, PhyClasse, by='Taxon', suffix=c('',''))

# Compute genus from Taxon
# We find the genus by selecting only the first word in the string 'Taxon'
Table1_phyto <- Table1_phyto %>%
  mutate(Genus = str_extract(Taxon, 'Pseudo-nitzschia|[:alnum:]+'))

Table1_phyto_class <- Table1_phyto[Table1_phyto$Phylum.Classe != "", ]

Table1_hydro <- dplyr::select(ungroup(Table1_hydro),Date:Prelevement.niveau,SALI:PCYAN )

data_hp <- left_join(Table1_phyto_class,Table1_hydro,by = join_by(Date, ID.interne.passage,
                                                                  Prelevement.niveau))


data_hpc <- dplyr::select(data_hp, Code.Region,Code_point_Libelle,lon,lat,Prelevement.niveau,Profondeur.metre,
                          ID.interne.passage,Date,Day,Month,Year,SALI:`NO3+NO2`,`TURB-FNU`,Phylum.Classe,Genus,Taxon,Valeur_mesure)

# Les profondeurs manquantes correspondent uniquement a de la "Surface (0-1m)" donc on dis que correspond 
# a une profondeur de 0.5 les Profondeur en NA
data_hpc$Profondeur.metre <- ifelse(is.na(data_hpc$Profondeur.metre),0.5,data_hpc$Profondeur.metre) 

data_hpc <- data_hpc |>
  group_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage, Prelevement.niveau)


data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)
data <- dplyr::select(data,Code.Region:CHLOROA,Shannon:Outlier)
data$ID.interne.passage <- as.character(data$ID.interne.passage)


data_new <- left_join(data,data_hpc)
data_outlier_v2 <- filter(data_new,Outlier == "OUI") 
data_outlier_v2 <- filter(data_outlier_v2, CHLOROA >= 0)
write.csv2(data_outlier_v2,file="data_modif/Table_outliers_Phylumclasse_Genus_taxon_V2.csv", row.names = FALSE,dec = ".")



### Associer les especes bloomantes à l'info de bloom ###
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

Table_bloom_R <- read_delim("data_modif/Table_bloom_R.csv", 
                            delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), 
                            trim_ws = TRUE)

Table_bloom_R <- Table_bloom_R[complete.cases(Table_bloom_R$Code_point_Libelle),] 
Table_bloom_R <- select(Table_bloom_R, -CHLOROA, - BergerParker, -cluster)


data_ok <- left_join(data,Table_bloom_R, join_by(Code_point_Libelle, Date))

write.csv2(data_ok,file="data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid.csv", row.names = FALSE,dec = ".")
