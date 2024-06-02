# Script JY.Dias - Stage M2 #

# Load packages
library(ggplot2)
library(ggthemes)
library(readr)
library(dplyr)
library(tidyr)
library(FactoMineR)
library(factoextra)
library(cowplot)

# Load data
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)
# Adding season info
data <- data |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Hiver",
                            Month %in% c(03, 04, 05) ~ "Printemps",
                            Month %in% c(06, 07, 08) ~ "Ete",
                            Month %in% c(09, 10, 11) ~ "Automne", TRUE ~ NA_character_))

data$cluster[data$cluster == "1"] <- "1-Méditerranée"
data$cluster[data$cluster == "2"] <- "2-Manche orientale - Mer du Nord"
data$cluster[data$cluster == "3"] <- "3-Atlantique - Manche occidentale"
data$cluster[data$cluster == "4"] <- "4-Mer des Pertuis"
data$cluster <- as.factor(data$cluster)

# Color for each cluster
cluster_col <- c("1-Méditerranée" = "#F8766D","2-Manche orientale - Mer du Nord" = "#CD9600", 
                 "3-Atlantique - Manche occidentale" = "#00BE67",  "4-Mer des Pertuis" = "#00A9FF")

data <- filter(data, cluster != "4-Mer des Pertuis")

### Graph to show phytoplankton composition by season and cluster #####
# Prepare data for graph
data$Autres <- rowSums(select(data,Raphidophyceae:`Autres protistes`,Dinoflagellata,Xanthophyceae),na.rm = T)
datam <- summarise(group_by(data,cluster,season,Month,Year), Bacillariophyceae=mean(Bacillariophyceae,na.rm=T),
                   Dinophyceae=mean(Dinophyceae,na.rm=T),Cryptophyceae=mean(Cryptophyceae,na.rm=T),
                   Haptophyta=mean(Haptophyta,na.rm=T),Ciliophora=mean(Ciliophora,na.rm=T),Autres=mean(Autres,na.rm=T))

# Compute total abundances to have relative composition
datam$Abdtot <- rowSums(datam[,c(5:10)],na.rm=T)
datam[,c(5:10)] <- datam[,c(5:10)]/datam$Abdtot

date_string <- paste(datam$Year, datam$Month, "01", sep = "-")

# Convert to date format
datam$Date <- as.Date(date_string,format = "%Y-%m-%d")

# Make it as the graph needs
datag <- pivot_longer(data = datam,cols = Bacillariophyceae:Autres,names_to = "Phylum")
datag$Abdtot <- NULL

datag$MonthYear <- format(datag$Date, "%Y-%m")
datag$Phylum <- as.factor(datag$Phylum)

datag$Phylum <-
  factor(datag$Phylum,
         levels = c("Bacillariophyceae","Dinophyceae","Ciliophora","Cryptophyceae","Haptophyta","Autres"))

ggplot(datag) +
  geom_col(aes(x = MonthYear, y = value * 100, fill = Phylum), position = "stack", na.rm = FALSE, width = 1) +
  facet_wrap(~cluster, scales = "free", ncol = 1) +
  scale_x_discrete(labels = function(x) format(as.Date(paste(x, "01", sep = "-")), "%Y-%m")) +
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 0.5, size = 5))+
  labs(x="Date",y="Abondance relative (%)",fill="Taxon")+
  scale_fill_manual(values = c("#56B4E9", "#009E73" ,"#F0E442", "#CC79A7","#996136","grey60"))
#ggsave('Annexe2.png', path = "output/graphs/Final",dpi = 500, width = 360, height =200, units = 'mm')


# Make it by Genus
# Compute the mean of all genus by season and cluster
data_graph <- data[,c(3,354,24:328)] %>%
  group_by(season, cluster) %>%
  summarise_all(~mean(., na.rm = TRUE))

# Make it relative 
data_graph$Abdtot <- rowSums(data_graph[,c(3:307)],na.rm=T)
data_graph[,c(3:307)] <- data_graph[,c(3:307)]/data_graph$Abdtot

datag <- pivot_longer(data = data_graph,cols = Actinoptychus:Coscinodiscophycidae,names_to = "Phylum")
datag$Abdtot <- NULL

# Group by season and cluster, and select the 5 most abundant phyla for each cluster
data_graph <- datag %>%
  group_by(season, cluster, Phylum) %>%
  summarise(abondance = sum(value,na.rm=T)) %>%
  ungroup() %>%
  group_by(season, cluster) %>%
  mutate(rank = rank(desc(abondance)),
         Phylum = ifelse(rank <= 4, as.character(Phylum), "Autres taxons")) %>%
  group_by(season, cluster, Phylum) %>%
  summarise(abondance = sum(abondance,na.rm = T)) %>%
  ungroup()

selection <- unique(data_graph$Phylum)

data_graph <- data[,c(3,6,7,24:328)] %>%
  group_by(Month,Year, cluster) %>%
  summarise_all(~mean(., na.rm = TRUE))

# Make it relative 
data_graph$Abdtot <- rowSums(data_graph[,c(4:308)],na.rm=T)
data_graph[,c(4:308)] <- data_graph[,c(4:308)]/data_graph$Abdtot

datag <- pivot_longer(data = data_graph,cols = Actinoptychus:Coscinodiscophycidae,names_to = "Phylum")
datag$Abdtot <- NULL

data_graph <- datag |>
  mutate(Phylum = ifelse(Phylum %in% selection,Phylum,"Autres taxons"))

# Convert to date format
date_string <- paste(data_graph$Year, data_graph$Month, "01", sep = "-")
data_graph$Date <- as.Date(date_string,format = "%Y-%m-%d")
data_graph$MonthYear <- format(data_graph$Date, "%Y-%m")
data_graph$Phylum <- as.factor(data_graph$Phylum)

data_graph$Phylum <-
  factor(data_graph$Phylum,
         levels = c("Autres taxons","Chaetoceros","Chaetocerotaceae","Skeletonema","Asterionellopsis","Phaeocystis","Leptocylindrus","Nitzschia",
                    "Akashiwo","Protoctista","Cryptophyceae.x","Cryptomonadales","Pseudo-nitzschia","Cylindrotheca","Choanofila","Azadinium","Phytoflagellés",
                    "Chrysochromulina"))

ggplot(data_graph) +
  geom_col(aes(x = MonthYear, y = value * 100, fill = Phylum), position = "stack", na.rm = FALSE, width = 1) +
  facet_wrap(~cluster, scales = "free", ncol = 1) +
  scale_x_discrete(labels = function(x) format(as.Date(paste(x, "01", sep = "-")), "%Y-%m")) +
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 0.5, size = 5))+
  labs(x="Date",y="Abondance relative (%)",fill="Taxon")+
  scale_fill_manual(values = c("grey60","#FB9A99", "#FBA793","#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C",
                               "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928"
                               , "#FFBCFF" , "#BC00BC", "#510051","#191DF7","#005100"))
#ggsave('Figure3.png', path = "output/graphs/Final",dpi = 500, width = 360, height =200, units = 'mm')


### CA ANALYSIS TO HAVE COMPOSITION TRENDS #######
##### Cluster 1 ####
# Select cluster 1
data1 <- filter(data,cluster == "1-Méditerranée")

# Keep only phytoplankton
phyto <- data1[,c(24:328)]

# Convert NA's to 0 to make it uniform
phyto[is.na(phyto)] <- 0

# Delete the lines where there are no counts
phyto_ok <- phyto[rowSums(phyto != 0) > 0,]

# Make the CA
CA_phyto <- CA(phyto_ok)

# Explore the result of the CA
summary(CA_phyto)

contrib11 <- fviz_contrib(CA_phyto, choice = "col",axes = 1, top = 10, fill = "#F8766D",col="#F8766D", title = "Axe 1" )
contrib12 <- fviz_contrib(CA_phyto, choice = "col",axes = 2, top = 10, fill = "#F8766D",col="#F8766D", title = "Axe 2" )
contrib13 <- fviz_contrib(CA_phyto, choice = "col",axes = 3, top = 10, fill = "#F8766D",col="#F8766D", title = "Axe 3")

fviz_eig(CA_phyto)

# Save the coordinates of the genus on the CA axis
phyto_coord <- as.data.frame(CA_phyto[["col"]][["coord"]])
phyto_coord$name <- rownames(phyto_coord)

# We have too much genus to make a graph so we keep only the 10 most abundant genus
# So we need few lines to select them
datag <- pivot_longer(data = data1,cols = Actinoptychus:Coscinodiscophycidae,names_to = "Phylum")
important_genus <- datag %>%
  group_by(Phylum) %>%
  summarise(abondance = sum(value,na.rm=T)) %>%
  mutate(rank = rank(desc(abondance)),
         Phylum = ifelse(rank <= 11, as.character(Phylum), "Autre phylum")) %>%
  group_by(Phylum) %>%
  summarise(abondance = sum(abondance,na.rm = T)) %>%
  ungroup()
# Delete the "Other genus" counts to keep 10 genus
important_genus <- filter(important_genus, Phylum != "Autre phylum")

# Keep the coordinates of the selected genus + those who contributes the most to the different axes
phyto_coord <- filter(phyto_coord,name %in% important_genus$Phylum | 
                        name %in% c("Prorocentrum","Skeletonema","Chaetoceros","Pseudo-nitzschia", # Axe 1
                                    "Dictyocha","Cryptophyceae.x",                                 # Axe 2
                                    "Cylindrotheca","Skeletonema","Nitzschia","Pseudo-nitzschia") )# Axe 3

write.csv2(phyto_coord,file="data_modif/phytocoord_Cluster1_final.csv", row.names = FALSE,dec = ".")

# Store metadata
info <- data1[, c("Code_point_Libelle","Date","cluster","season","Month","Year")]
info <- info[rowSums(phyto != 0) > 0,]

# Store the coordinates of the different dates
obs_coord <- as.data.frame(CA_phyto[["row"]][["coord"]])
# Adding the metadata informations
obs_coord$cluster <- as.character(info$cluster)
obs_coord$season <- info$season
obs_coord$Month <- info$Month
obs_coord$Year <- info$Year
obs_coord$Code_point_Libelle <- info$Code_point_Libelle
obs_coord$Date <- info$Date

write.csv2(obs_coord,file="data_modif/CA_Cluster1_final.csv", row.names = FALSE,dec = ".")


# Make the mean of the date coordinates by season
obs_coord_mean <- summarise(group_by(obs_coord, cluster,season), CA1=mean(`Dim 1`,na.rm=T),
                            CA2=mean(`Dim 2`,na.rm=T),CA3=mean(`Dim 3`,na.rm=T))

# Make some graphs
ggplot(obs_coord_mean, aes(x = CA1, y = CA2)) +
  geom_text(aes(x = `Dim 1`, y = `Dim 2`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA1 (5.41%)", y = "CA2 (5.06%)",colour="Saison",shape = "Saison") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 1`, yend = `Dim 2`), color = "black", size = 0.5,data=phyto_coord)+
  geom_point(aes(colour=season,shape=season),size=2) +
  theme_minimal()+
  scale_shape_manual(values=c(15,19,17,18))

ggplot(obs_coord_mean, aes(x = CA2, y = CA3)) +
  geom_text(aes(x = `Dim 2`, y = `Dim 3`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA2 (5.06%)", y = "CA3 (4.71%)",colour="Saison",shape="Saison") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 2`, yend = `Dim 3`), color = "black", size = 0.5,data=phyto_coord)+
  geom_point(aes(colour=season,shape=season),size=2) +
  theme_minimal()+
  scale_shape_manual(values=c(15,19,17,18))

# Make a time series of the mean axes's coordinates by Month and Year
ca_ts <- summarise(group_by(obs_coord, cluster,Month,Year), CA1=mean(`Dim 1`,na.rm=T),
                   CA2=mean(`Dim 2`,na.rm=T),CA3=mean(`Dim 3`,na.rm=T))

ca_ts$Date <- as.Date(paste(ca_ts$Year, ca_ts$Month, "01", sep = "-"), format = "%Y-%m-%d")
a <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA1),size=2,col="#F8766D")+
  geom_point(aes(x=Date,y=CA1),size=2.5,col="#F8766D")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_blank())+
  labs(y= "CA1 (5.41%)",x=NULL)+
  facet_wrap(~cluster)

b <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA2),size=2,col="#F8766D")+
  geom_point(aes(x=Date,y=CA2),size=2.5,col="#F8766D")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_blank())+
  labs(y= "CA2 (5.06%)",x=NULL)
c <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA3),size=2,col="#F8766D")+
  geom_point(aes(x=Date,y=CA3),size=2.5,col="#F8766D")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5,size = 5))+
  labs(y= "CA3 (4.71%)")

Cluster1 <- plot_grid(a,b,c,ncol = 1)
Cluster1

Cluster1contrib <- plot_grid(contrib11,contrib12,contrib13,ncol = 1)

#### Cluster 2 ######
# Select cluster 2
data2 <- filter(data,cluster == "2-Manche orientale - Mer du Nord")

# Keep only phytoplankton
phyto <- data2[,c(24:328)]

# Convert NA's to 0 to make it uniform
phyto[is.na(phyto)] <- 0

# Delete the lines where there are no counts
phyto_ok <- phyto[rowSums(phyto != 0) > 0,]

# Make the CA
CA_phyto <- CA(phyto_ok)

# Explore the result of the CA
summary(CA_phyto)

contrib21 <- fviz_contrib(CA_phyto, choice = "col",axes = 1, top = 10, fill = "#CD9600",col="#CD9600", title = "Axe 1" )
contrib22 <- fviz_contrib(CA_phyto, choice = "col",axes = 2, top = 10, fill = "#CD9600",col="#CD9600", title = "Axe 2" )
contrib23 <- fviz_contrib(CA_phyto, choice = "col",axes = 3, top = 10, fill = "#CD9600",col="#CD9600", title = "Axe 3")

fviz_eig(CA_phyto)

# Save the coordinates of the genus on the CA axis
phyto_coord <- as.data.frame(CA_phyto[["col"]][["coord"]])
phyto_coord$name <- rownames(phyto_coord)

# We have too much genus to make a graph so we keep only the 10 most abundant genus
# So we need few lines to select them
datag <- pivot_longer(data = data2,cols = Actinoptychus:Coscinodiscophycidae,names_to = "Phylum")
important_genus <- datag %>%
  group_by(Phylum) %>%
  summarise(abondance = sum(value,na.rm=T)) %>%
  mutate(rank = rank(desc(abondance)),
         Phylum = ifelse(rank <= 10, as.character(Phylum), "Autre phylum")) %>%
  group_by(Phylum) %>%
  summarise(abondance = sum(abondance,na.rm = T)) %>%
  ungroup()


# Keep the coordinates of the selected genus + those who contributes the most to the different axes
phyto_coord <- filter(phyto_coord,name %in% important_genus$Phylum | 
                        name %in% c("Phaeocystis","Chaetoceros","Leptocylindrus","Skeletonema","Cryptophyceae.x","Asterionnellopsis","Protoctista","Thalassiosira","Pseudo-nitzschia", # Axe 1
                      "Protoctista","Chaetoceros", # Axe 2
                      "Leptocylindrus","Chaetoceros","Skeletonema","Asterionellopsis","Dactyliosolen","Cerataulina","Eucampia"))

write.csv2(phyto_coord,file="data_modif/phytocoord_Cluster2_final.csv", row.names = FALSE,dec = ".")
# Store metadata
info <- data2[, c("Code_point_Libelle","Date","cluster","season","Month","Year")]
info <- info[rowSums(phyto != 0) > 0,]

# Store the coordinates of the different dates
obs_coord <- as.data.frame(CA_phyto[["row"]][["coord"]])
# Adding the metadata informations
obs_coord$cluster <- as.character(info$cluster)
obs_coord$season <- info$season
obs_coord$Month <- info$Month
obs_coord$Year <- info$Year
obs_coord$Code_point_Libelle <- info$Code_point_Libelle
obs_coord$Date <- info$Date

write.csv2(obs_coord,file="data_modif/CA_Cluster2_final.csv", row.names = FALSE,dec = ".")

# Make the mean of the date coordinates by season
obs_coord_mean <- summarise(group_by(obs_coord, cluster,season), CA1=mean(`Dim 1`,na.rm=T),
                            CA2=mean(`Dim 2`,na.rm=T),CA3=mean(`Dim 3`,na.rm=T))

# Make some graphs
ggplot(obs_coord_mean, aes(x = CA1, y = CA2)) +
  geom_text(aes(x = `Dim 1`, y = `Dim 2`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA1 (7.04%)", y = "CA2 (6.86%)",colour="Saison",shape = "Saison") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 1`, yend = `Dim 2`), color = "black", size = 0.5,data=phyto_coord)+
  geom_point(aes(colour=season,shape=season),size=2) +
  theme_minimal()+
  scale_shape_manual(values=c(15,19,17,18))

ggplot(obs_coord_mean, aes(x = CA2, y = CA3)) +
  geom_text(aes(x = `Dim 2`, y = `Dim 3`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA2 (6.86%)", y = "CA3 (5.22%)",colour="Saison",shape="Saison") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 2`, yend = `Dim 3`), color = "black", size = 0.5,data=phyto_coord)+
  geom_point(aes(colour=season,shape=season),size=2) +
  theme_minimal()+
  scale_shape_manual(values=c(15,19,17,18))

# Make a time series of the mean axes's coordinates by Month and Year
ca_ts <- summarise(group_by(obs_coord, cluster,Month,Year), CA1=mean(`Dim 1`,na.rm=T),
                   CA2=mean(`Dim 2`,na.rm=T),CA3=mean(`Dim 3`,na.rm=T))

ca_ts$Date <- as.Date(paste(ca_ts$Year, ca_ts$Month, "01", sep = "-"), format = "%Y-%m-%d")
a <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA1),size=2,col="#CD9600")+
  geom_point(aes(x=Date,y=CA1),size=2.5,col="#CD9600")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_blank())+
  labs(y= "CA1 (7.04%)",x=NULL)+
  facet_wrap(~cluster)

b <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA2),size=2,col="#CD9600")+
  geom_point(aes(x=Date,y=CA2),size=2.5,col="#CD9600")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_blank())+
  labs(y= "CA2 (6.86%)",x=NULL)
c <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA3),size=2,col="#CD9600")+
  geom_point(aes(x=Date,y=CA3),size=2.5,col="#CD9600")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5,size = 5))+
  labs(y= "CA3 (5.22%)")

Cluster2 <- plot_grid(a,b,c,ncol = 1)
Cluster2contrib <- plot_grid(contrib21,contrib22,contrib23,ncol = 1)

#### Cluster 3 ######
# Select cluster 3
data3 <- filter(data,cluster == "3-Atlantique - Manche occidentale")

# Keep only phytoplankton
phyto <- data3[,c(24:328)]

# Convert NA's to 0 to make it uniform
phyto[is.na(phyto)] <- 0

# Delete the lines where there are no counts
phyto_ok <- phyto[rowSums(phyto != 0) > 0,]

# Some genus are not counted so we need to delete them
phyto_ok <- select(phyto_ok,-c(Spatulodinium,Kryptoperidinium,Tiarina,Metaphalacroma,Lyrella,Ceratocorys,Centrodinium))

# Make the CA
CA_phyto <- CA(phyto_ok)

# Explore the result of the CA
summary(CA_phyto)

contrib31 <- fviz_contrib(CA_phyto, choice = "col",axes = 1, top = 10, fill = "#00BE67",col="#00BE67", title = "Axe 1" )
contrib32 <- fviz_contrib(CA_phyto, choice = "col",axes = 2, top = 10, fill = "#00BE67",col="#00BE67", title = "Axe 2" )
contrib33 <- fviz_contrib(CA_phyto, choice = "col",axes = 3, top = 10, fill = "#00BE67",col="#00BE67", title = "Axe 3" )
fviz_eig(CA_phyto)

# Save the coordinates of the genus on the CA axis
phyto_coord <- as.data.frame(CA_phyto[["col"]][["coord"]])
phyto_coord$name <- rownames(phyto_coord)

# We have too much genus to make a graph so we keep only the 10 most abundant genus
# So we need few lines to select them
datag <- pivot_longer(data = data3,cols = Actinoptychus:Coscinodiscophycidae,names_to = "Phylum")
important_genus <- datag %>%
  group_by(Phylum) %>%
  summarise(abondance = sum(value,na.rm=T)) %>%
  mutate(rank = rank(desc(abondance)),
         Phylum = ifelse(rank <= 10, as.character(Phylum), "Autre phylum")) %>%
  group_by(Phylum) %>%
  summarise(abondance = sum(abondance,na.rm = T)) %>%
  ungroup()


# Keep the coordinates of the selected genus + those who contributes the most to the different axes
phyto_coord <- filter(phyto_coord,name %in% important_genus$Phylum | 
                        name %in% c("Phaocystis","Phytoflagellés","Lepidodinium","Skeletonema","Guinardia", # Axe 1
                        "Phytoflagellés","Skeletonema","Lepidodinium","Thalassiosira","Plagiogramma","Leptocylindrus","Chaetocerotaceae","Phaocystis","Pseudo-nitzschia","Asterionellopsis","Brockmanniella", #Axe 2
                        "Plagiogramma","Phytoflagellés","Skeletonema","Skeletonema","Leptocylindrus","Phaeocystis","Thalassiosira","Cerataulina","Chaetoceros"
                        ))
write.csv2(phyto_coord,file="data_modif/phytocoord_Cluster3_final.csv", row.names = FALSE,dec = ".")
# Store metadata
info <- data3[, c("Code_point_Libelle","Date","cluster","season","Month","Year")]
info <- info[rowSums(phyto != 0) > 0,]

# Store the coordinates of the different dates
obs_coord <- as.data.frame(CA_phyto[["row"]][["coord"]])
# Adding the metadata informations
obs_coord$cluster <- as.character(info$cluster)
obs_coord$season <- info$season
obs_coord$Month <- info$Month
obs_coord$Year <- info$Year
obs_coord$Code_point_Libelle <- info$Code_point_Libelle
obs_coord$Date <- info$Date

write.csv2(obs_coord,file="data_modif/CA_Cluster3_final.csv", row.names = FALSE,dec = ".")

# Make the mean of the date coordinates by season
obs_coord_mean <- summarise(group_by(obs_coord, cluster,season), CA1=mean(`Dim 1`,na.rm=T),
                            CA2=mean(`Dim 2`,na.rm=T),CA3=mean(`Dim 3`,na.rm=T))

# Make some graphs
ggplot(obs_coord_mean, aes(x = CA1, y = CA2)) +
  geom_text(aes(x = `Dim 1`, y = `Dim 2`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA1 (4.50%)", y = "CA2 (4.48%)",colour="Saison",shape = "Saison") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 1`, yend = `Dim 2`), color = "black", size = 0.5,data=phyto_coord)+
  geom_point(aes(colour=season,shape=season),size=2) +
  theme_minimal()+
  scale_shape_manual(values=c(15,19,17,18))

ggplot(obs_coord_mean, aes(x = CA2, y = CA3)) +
  geom_text(aes(x = `Dim 2`, y = `Dim 3`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA2 (4.48%)", y = "CA3 (4.38%)",colour="Saison",shape="Saison") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 2`, yend = `Dim 3`), color = "black", size = 0.5,data=phyto_coord)+
  geom_point(aes(colour=season,shape=season),size=2) +
  theme_minimal()+
  scale_shape_manual(values=c(15,19,17,18))

# Make a time series of the mean axes's coordinates by Month and Year
ca_ts <- summarise(group_by(obs_coord, cluster,Month,Year), CA1=mean(`Dim 1`,na.rm=T),
                   CA2=mean(`Dim 2`,na.rm=T),CA3=mean(`Dim 3`,na.rm=T))

ca_ts$Date <- as.Date(paste(ca_ts$Year, ca_ts$Month, "01", sep = "-"), format = "%Y-%m-%d")
a <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA1),size=2,col="#00BE67")+
  geom_point(aes(x=Date,y=CA1),size=2.5,col="#00BE67")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_blank())+
  labs(y= "CA1 (4.50%)",x=NULL)+
  facet_wrap(~cluster)
b <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA2),size=2,col="#00BE67")+
  geom_point(aes(x=Date,y=CA2),size=2.5,col="#00BE67")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_blank())+
  labs(y= "CA2 (4.48%)",x=NULL)
c <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA3),size=2,col="#00BE67")+
  geom_point(aes(x=Date,y=CA3),size=2.5,col="#00BE67")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5,size = 5))+
  labs(y= "CA3 (4.38%)")

Cluster3 <- plot_grid(a,b,c,ncol = 1)
Cluster3contrib <- plot_grid(contrib31,contrib32,contrib33,ncol = 1)


plot_grid(Cluster1,Cluster2,Cluster3,ncol = 3)
plot_grid(Cluster1contrib,Cluster2contrib,Cluster3contrib,ncol = 3,labels = "AUTO")
