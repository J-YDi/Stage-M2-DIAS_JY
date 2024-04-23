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

### Graph to show phytoplankton composition by season and cluster #####
# Prepare data for graph
datam <- summarise(group_by(data,cluster,season), Bacillariophyceae=mean(Bacillariophyceae,na.rm=T),
                   Dinophyceae=mean(Dinophyceae,na.rm=T),Cryptophyceae=mean(Cryptophyceae,na.rm=T),
                   Haptophyta=mean(Haptophyta,na.rm=T),Ciliophora=mean(Ciliophora,na.rm=T))

# Compute total abundances to have relative composition
datam$Abdtot <- rowSums(datam[,c(3:7)],na.rm=T)
datam[,c(3:7)] <- datam[,c(3:7)]/datam$Abdtot

# Make it as the graph needs
datag <- pivot_longer(data = datam,cols = Bacillariophyceae:Ciliophora,names_to = "Phylum")
datag$Abdtot <- NULL

# Graph by phylum
ggplot()+
  geom_col(aes(x ="", y = value*100 , fill = Phylum ), width = 0.7, datag)+
  coord_flip()+
  labs(title="Composition moyenne de la communité par cluster",
       x="Saison",y="Abondance relative",fill="Phylum")+
  facet_grid(season~cluster)+
scale_fill_manual(values=c("steelblue3","darkorchid2","tomato1","lightgreen","gold1"))+
  theme_test()

# Make it without season consideration

datam <- summarise(group_by(data,cluster), Bacillariophyceae=mean(Bacillariophyceae,na.rm=T),
                   Dinophyceae=mean(Dinophyceae,na.rm=T),Cryptophyceae=mean(Cryptophyceae,na.rm=T),
                   Haptophyta=mean(Haptophyta,na.rm=T),Ciliophora=mean(Ciliophora,na.rm=T))
datam$Abdtot <- rowSums(datam[,c(2:6)],na.rm=T)
datam[,c(2:6)] <- datam[,c(2:6)]/datam$Abdtot

datag <- pivot_longer(data = datam,cols = Bacillariophyceae:Ciliophora,names_to = "Phylum")
datag$Abdtot <- NULL
ggplot()+
  geom_col(aes(x ="", y = value*100 , fill = Phylum ), width = 0.7, datag)+
  coord_flip()+
  labs(title="Composition moyenne de la communité par cluster",
       x="",y="Abondance relative",fill="Phylum")+
  facet_wrap(~cluster,ncol = 1)+
  scale_fill_manual(values=c("steelblue3","darkorchid2","tomato1","lightgreen","gold1"))+
  theme_test()

# Make it by Genus

data_graph <- data[,c(3,354,24:328)] %>%
  # Regrouper par saison et cluster
  group_by(season, cluster) %>%
  # Calculer la moyenne pour chaque groupe
  summarise_all(~median(., na.rm = TRUE))

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
         Phylum = ifelse(rank <= 5, as.character(Phylum), "Autre phylum")) %>%
  group_by(season, cluster, Phylum) %>%
  summarise(abondance = sum(abondance,na.rm = T)) %>%
  ungroup()

ggplot()+
  geom_col(aes(x ="", y = abondance*100 , fill = Phylum ), width = 0.7, data_graph)+
  coord_flip()+
  labs(title="Composition mediane de la communité par cluster",
       x="Saison",y="Abondance relative",fill="Genre")+
  facet_grid(season~cluster)+
  scale_fill_manual(values=c("cornsilk","coral","chartreuse4","gray46","burlywood","brown1",
                             "blueviolet","dodgerblue","deepskyblue4","deeppink2","darkviolet",
                             "darkorange3","greenyellow","magenta2","turquoise1","violet","sienna",
                             "yellow","red3","palegreen3","royalblue","tomato","thistle1","brown3","orchid1",
                             "honeydew3","indianred1","turquoise","blue"
                             ))+
  theme_test()
  

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
         Phylum = ifelse(rank <= 5, as.character(Phylum), "Autre phylum")) %>%
  group_by(season, cluster, Phylum) %>%
  summarise(abondance = sum(abondance,na.rm = T)) %>%
  ungroup()

ggplot()+
  geom_col(aes(x ="", y = abondance*100 , fill = Phylum ), width = 0.7, data_graph)+
  coord_flip()+
  labs(title="Composition moyenne de la communité par cluster",
       x="Saison",y="Abondance relative",fill="Genre")+
  facet_grid(season~cluster)+
  scale_fill_manual(values=c("cornsilk","coral","chartreuse4","gray46","burlywood",
                             "blueviolet","dodgerblue","deepskyblue4","deeppink2","darkviolet",
                             "darkorange3","greenyellow","magenta2","turquoise1","violet","sienna","yellow","red3","palegreen3","royalblue","tomato"
                             ,"thistle1","brown3","orchid1",
                             "honeydew3","indianred1","turquoise","blue"))+
  theme_test()

# Make it without season consideration

data_graph <- data[,c(3,24:328)] %>%
  group_by(cluster) %>%
  summarise_all(~mean(., na.rm = TRUE))

# Make it relative
data_graph$Abdtot <- rowSums(data_graph[,c(2:306)],na.rm=T)
data_graph[,c(2:306)] <- data_graph[,c(2:306)]/data_graph$Abdtot

datag <- pivot_longer(data = data_graph,cols = Actinoptychus:Coscinodiscophycidae,names_to = "Phylum")
datag$Abdtot <- NULL

# Group by season and cluster, and select the 5 most abundant phyla for each cluster
data_graph <- datag %>%
  group_by(cluster, Phylum) %>%
  summarise(abondance = sum(value,na.rm=T)) %>%
  ungroup() %>%
  group_by(cluster) %>%
  mutate(rank = rank(desc(abondance)),
         Phylum = ifelse(rank <= 5, as.character(Phylum), "Autre phylum")) %>%
  group_by(cluster, Phylum) %>%
  summarise(abondance = sum(abondance,na.rm = T)) %>%
  ungroup()

ggplot()+
  geom_col(aes(x ="", y = abondance*100 , fill = Phylum ), width = 0.7, data_graph)+
  coord_flip()+
  labs(title="Composition moyenne de la communité par cluster",
       x="",y="Abondance relative",fill="Genre")+
  facet_wrap(~cluster,ncol = 1)+
  scale_fill_manual(values=c("gray46","cornsilk","chartreuse4","burlywood","brown1",
                             "blueviolet","dodgerblue","deepskyblue4","deeppink2",
                             "darkorange3","greenyellow","magenta2","turquoise1","violet","sienna","yellow","red3","palegreen3","royalblue","tomato"))+
  theme_test()


### CA ANALYSIS TO HAVE COMPOSITION TRENDS #######
##### Cluster 1 ####
# Select cluster 1
data1 <- filter(data,cluster == 1)

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

fviz_contrib(CA_phyto, choice = "col",axes = 1, top = 15)
fviz_contrib(CA_phyto, choice = "col",axes = 2, top = 15)
fviz_contrib(CA_phyto, choice = "col",axes = 3, top = 15)
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
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA1 (5.41%)",title = "Composition de la communauté au cours du temps")
b <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA2),size=2,col="#F8766D")+
  geom_point(aes(x=Date,y=CA2),size=2.5,col="#F8766D")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA2 (5.06%)")
c <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA3),size=2,col="#F8766D")+
  geom_point(aes(x=Date,y=CA3),size=2.5,col="#F8766D")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA3 (4.71%)")

Cluster1 <- plot_grid(a,b,c,ncol = 1)

#### Cluster 2 ######
# Select cluster 2
data2 <- filter(data,cluster == 2)

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

fviz_contrib(CA_phyto, choice = "col",axes = 1, top = 15)
fviz_contrib(CA_phyto, choice = "col",axes = 2, top = 15)
fviz_contrib(CA_phyto, choice = "col",axes = 3, top = 15)
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
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA1 (7.04%)",title = "Composition de la communauté au cours du temps")
b <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA2),size=2,col="#CD9600")+
  geom_point(aes(x=Date,y=CA2),size=2.5,col="#CD9600")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA2 (6.86%)")
c <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA3),size=2,col="#CD9600")+
  geom_point(aes(x=Date,y=CA3),size=2.5,col="#CD9600")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA3 (5.22%)")

Cluster2 <- plot_grid(a,b,c,ncol = 1)

#### Cluster 3 ######
# Select cluster 3
data3 <- filter(data,cluster == 3)

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

fviz_contrib(CA_phyto, choice = "col",axes = 1,top = 15)
fviz_contrib(CA_phyto, choice = "col",axes = 2,top = 15)
fviz_contrib(CA_phyto, choice = "col",axes = 3,top = 15)
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
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA1 (4.50%)",title = "Composition de la communauté au cours du temps")
b <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA2),size=2,col="#00BE67")+
  geom_point(aes(x=Date,y=CA2),size=2.5,col="#00BE67")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA2 (4.48%)")
c <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA3),size=2,col="#00BE67")+
  geom_point(aes(x=Date,y=CA3),size=2.5,col="#00BE67")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA3 (4.38%)")

Cluster3 <- plot_grid(a,b,c,ncol = 1)


#### Cluster 4 ######
# Select cluster 4
data4 <- filter(data,cluster == 4)

# Keep only phytoplankton
phyto <- data4[,c(24:328)]

# Convert NA's to 0 to make it uniform
phyto[is.na(phyto)] <- 0

# Delete the lines where there are no counts
phyto_ok <- phyto[rowSums(phyto != 0) > 0,]

# Make the CA
CA_phyto <- CA(phyto_ok)

# Explore the result of the CA
summary(CA_phyto)

fviz_contrib(CA_phyto, choice = "col",axes = 1,top = 15)
fviz_contrib(CA_phyto, choice = "col",axes = 2,top = 15)
fviz_contrib(CA_phyto, choice = "col",axes = 3,top = 15)
fviz_eig(CA_phyto)

# Save the coordinates of the genus on the CA axis
phyto_coord <- as.data.frame(CA_phyto[["col"]][["coord"]])
phyto_coord$name <- rownames(phyto_coord)

# We have too much genus to make a graph so we keep only the 10 most abundant genus
# So we need few lines to select them
datag <- pivot_longer(data = data4,cols = Actinoptychus:Coscinodiscophycidae,names_to = "Phylum")
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
                        name %in% c("Dactyliosolen","Skeletonema","Lepidodinium", # Axe 1
                        "Lepidodinium","Skeletonema","Leptocylindrus","Prorocentrum","Asterionellopsis","Scrippsiella","Gymnodiniaceae","Chaetoceros", # Axe 2
                        "Lepidodinium","Skeletonema","Leptocylindrus","Gymnodiniaceae","Chaetoceros","Amphora","Gymnodinium","Pseudo-nitzschia","Guinardia"
                        ))

# Store metadata
info <- data4[, c("Code_point_Libelle","Date","cluster","season","Month","Year")]
info <- info[rowSums(phyto != 0) > 0,]

# Store the coordinates of the different dates
obs_coord <- as.data.frame(CA_phyto[["row"]][["coord"]])
# Adding the metadata informations
obs_coord$cluster <- as.character(info$cluster)
obs_coord$season <- info$season
obs_coord$Month <- info$Month
obs_coord$Year <- info$Year

# Make the mean of the date coordinates by season
obs_coord_mean <- summarise(group_by(obs_coord, cluster,season), CA1=mean(`Dim 1`,na.rm=T),
                            CA2=mean(`Dim 2`,na.rm=T),CA3=mean(`Dim 3`,na.rm=T))

# Make some graphs
ggplot(obs_coord_mean, aes(x = CA1, y = CA2)) +
  geom_text(aes(x = `Dim 1`, y = `Dim 2`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA1 (8.37%)", y = "CA2 (8.00%)",colour="Saison",shape = "Saison") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 1`, yend = `Dim 2`), color = "black", size = 0.5,data=phyto_coord)+
  geom_point(aes(colour=season,shape=season),size=2) +
  theme_minimal()+
  scale_shape_manual(values=c(15,19,17,18))

ggplot(obs_coord_mean, aes(x = CA2, y = CA3)) +
  geom_text(aes(x = `Dim 2`, y = `Dim 3`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA2 (8.00%)", y = "CA3 (7.23%)",colour="Saison",shape="Saison") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 2`, yend = `Dim 3`), color = "black", size = 0.5,data=phyto_coord)+
  geom_point(aes(colour=season,shape=season),size=2) +
  theme_minimal()+
  scale_shape_manual(values=c(15,19,17,18))

# Make a time series of the mean axes's coordinates by Month and Year
ca_ts <- summarise(group_by(obs_coord, cluster,Month,Year), CA1=mean(`Dim 1`,na.rm=T),
                   CA2=mean(`Dim 2`,na.rm=T),CA3=mean(`Dim 3`,na.rm=T))

ca_ts$Date <- as.Date(paste(ca_ts$Year, ca_ts$Month, "01", sep = "-"), format = "%Y-%m-%d")
a <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA1),size=2,col="#00A9FF")+
  geom_point(aes(x=Date,y=CA1),size=2.5,col="#00A9FF")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA1 (4.50%)",title = "Composition de la communauté au cours du temps")
b <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA2),size=2,col="#00A9FF")+
  geom_point(aes(x=Date,y=CA2),size=2.5,col="#00A9FF")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA2 (4.48%)")
c <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA3),size=2,col="#00A9FF")+
  geom_point(aes(x=Date,y=CA3),size=2.5,col="#00A9FF")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA3 (4.38%)")

Cluster4 <- plot_grid(a,b,c,ncol = 1)


plot_grid(Cluster1,Cluster2,Cluster3,Cluster4,ncol = 4)



