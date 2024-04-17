# Composition de la communauté #####
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)
data <- data |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Hiver",
                            Month %in% c(03, 04, 05) ~ "Printemps",
                            Month %in% c(06, 07, 08) ~ "Ete",
                            Month %in% c(09, 10, 11) ~ "Automne", TRUE ~ NA_character_))
datam <- summarise(group_by(data,cluster,season), Bacillariophyceae=mean(Bacillariophyceae,na.rm=T),
                   Dinophyceae=mean(Dinophyceae,na.rm=T),Cryptophyceae=mean(Cryptophyceae,na.rm=T),
                   Haptophyta=mean(Haptophyta,na.rm=T),Ciliophora=mean(Ciliophora,na.rm=T))
datam$Abdtot <- rowSums(datam[,c(3:7)],na.rm=T)
datam[,c(3:7)] <- datam[,c(3:7)]/datam$Abdtot

datag <- pivot_longer(data = datam,cols = Bacillariophyceae:Ciliophora,names_to = "Phylum")
datag$Abdtot <- NULL

ggplot()+
  geom_col(aes(x ="", y = value*100 , fill = Phylum ), width = 0.7, datag)+
  coord_flip()+
  labs(title="Composition moyenne de la communité par cluster",
       x="Saison",y="Abondance relative",fill="Phylum")+
  facet_grid(season~cluster)+
scale_fill_manual(values=c("steelblue3","darkorchid2","tomato1","lightgreen","gold1"))+
  theme_test()

result <- data[,c(3,277,24:247)] %>%
  # Regrouper par saison et cluster
  group_by(season, cluster) %>%
  # Calculer la moyenne pour chaque groupe
  summarise_all(~median(., na.rm = TRUE))

result$Abdtot <- rowSums(result[,c(3:226)],na.rm=T)
result[,c(3:226)] <- result[,c(3:226)]/result$Abdtot

datag <- pivot_longer(data = result,cols = Actinoptychus:Coscinodiscophycidae,names_to = "Phylum")
datag$Abdtot <- NULL

result <- datag %>%
  group_by(season, cluster, Phylum) %>%
  summarise(abondance = sum(value,na.rm=T)) %>%
  ungroup() %>%
  # Regrouper par saison et cluster, et sélectionner les 10 phylums les plus abondants
  group_by(season, cluster) %>%
  mutate(rank = rank(desc(abondance)),
         Phylum = ifelse(rank <= 5, as.character(Phylum), "Autre phylum")) %>%
  group_by(season, cluster, Phylum) %>%
  summarise(abondance = sum(abondance,na.rm = T)) %>%
  ungroup()

ggplot()+
  geom_col(aes(x ="", y = abondance*100 , fill = Phylum ), width = 0.7, result)+
  coord_flip()+
  labs(title="Composition mediane de la communité par cluster",
       x="Saison",y="Abondance relative",fill="Genre")+
  facet_grid(season~cluster)+
  scale_fill_manual(values=c("cornsilk","coral","chartreuse4","gray46","burlywood","brown1",
                             "blueviolet","dodgerblue","deepskyblue4","deeppink2","darkviolet",
                             "darkorange3","greenyellow","magenta2","turquoise1","violet","sienna","yellow","red3","palegreen3","royalblue","tomato"))+
  theme_test()
  


result <- data[,c(3,277,24:247)] %>%
  # Regrouper par saison et cluster
  group_by(season, cluster) %>%
  # Calculer la moyenne pour chaque groupe
  summarise_all(~mean(., na.rm = TRUE))

result$Abdtot <- rowSums(result[,c(3:226)],na.rm=T)
result[,c(3:226)] <- result[,c(3:226)]/result$Abdtot

datag <- pivot_longer(data = result,cols = Actinoptychus:Coscinodiscophycidae,names_to = "Phylum")
datag$Abdtot <- NULL

result <- datag %>%
  group_by(season, cluster, Phylum) %>%
  summarise(abondance = sum(value,na.rm=T)) %>%
  ungroup() %>%
  # Regrouper par saison et cluster, et sélectionner les 10 phylums les plus abondants
  group_by(season, cluster) %>%
  mutate(rank = rank(desc(abondance)),
         Phylum = ifelse(rank <= 5, as.character(Phylum), "Autre phylum")) %>%
  group_by(season, cluster, Phylum) %>%
  summarise(abondance = sum(abondance,na.rm = T)) %>%
  ungroup()

ggplot()+
  geom_col(aes(x ="", y = abondance*100 , fill = Phylum ), width = 0.7, result)+
  coord_flip()+
  labs(title="Composition moyenne de la communité par cluster",
       x="Saison",y="Abondance relative",fill="Genre")+
  facet_grid(season~cluster)+
  scale_fill_manual(values=c("cornsilk","coral","chartreuse4","gray46","burlywood","brown1",
                             "blueviolet","dodgerblue","deepskyblue4","deeppink2","darkviolet",
                             "darkorange3","greenyellow","magenta2","turquoise1","violet","sienna","yellow","red3","palegreen3","royalblue","tomato"))+
  theme_test()



result <- data[,c(3,24:247)] %>%
  # Regrouper par saison et cluster
  group_by(cluster) %>%
  # Calculer la moyenne pour chaque groupe
  summarise_all(~mean(., na.rm = TRUE))

result$Abdtot <- rowSums(result[,c(2:225)],na.rm=T)
result[,c(2:225)] <- result[,c(2:225)]/result$Abdtot

datag <- pivot_longer(data = result,cols = Actinoptychus:Coscinodiscophycidae,names_to = "Phylum")
datag$Abdtot <- NULL

result <- datag %>%
  group_by(cluster, Phylum) %>%
  summarise(abondance = sum(value,na.rm=T)) %>%
  ungroup() %>%
  # Regrouper par saison et cluster, et sélectionner les 10 phylums les plus abondants
  group_by(cluster) %>%
  mutate(rank = rank(desc(abondance)),
         Phylum = ifelse(rank <= 5, as.character(Phylum), "Autre phylum")) %>%
  group_by(cluster, Phylum) %>%
  summarise(abondance = sum(abondance,na.rm = T)) %>%
  ungroup()

ggplot()+
  geom_col(aes(x ="", y = abondance*100 , fill = Phylum ), width = 0.7, result)+
  coord_flip()+
  labs(title="Composition moyenne de la communité par cluster",
       x="",y="Abondance relative",fill="Genre")+
  facet_wrap(~cluster,ncol = 1)+
  scale_fill_manual(values=c("cornsilk","gray46","chartreuse4","burlywood","brown1",
                             "blueviolet","dodgerblue","deepskyblue4","deeppink2",
                             "darkorange3","greenyellow","magenta2","turquoise1","violet","sienna","yellow","red3","palegreen3","royalblue","tomato"))+
  theme_test()

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

## CCA #####
phyto <- data[,c(24:247)]
phyto[is.na(phyto)] <- 0
phyto <- phyto[rowSums(phyto != 0) > 0,]

Table_PCA <- data[, c("SALI","TEMP","TURB","NH4","PO4","SIOH","OXYGENE","NO3+NO2","TURB-FNU")]
Table_PCA <- Table_PCA[rowSums(phyto != 0) > 0,]

info <- data[, c("Code_point_Libelle","Date","cluster","season","Month","Year")]
info <- info[rowSums(phyto != 0) > 0,]

# Imputation des valeurs hydro manquantes
ncomp <- estim_ncpPCA(Table_PCA)
res.imp <- imputePCA(Table_PCA, ncp = ncomp$ncp,method = "EM")
envi <- as.data.frame(res.imp$completeObs)


phyto_cca <- cca(phyto ~ ., data=envi)
summary(phyto_cca)

plot(phyto_cca, scaling="species",   display=c("wa", "sp", "bp"))


observations_coords <- scores(phyto_cca, display = "sites")

# Créez un dataframe pour stocker les coordonnées des observations
observations_df <- as.data.frame(observations_coords)
observations_df$cluster <- as.character(info$cluster)
observations_df$season <- info$season
observations_df$Month <- info$Month
observations_df$Year <- info$Year

cluster_pos <- summarise(group_by(observations_df, cluster,season), CCA1=mean(CCA1,na.rm=T),
                         CCA2=mean(CCA2,na.rm=T))

cluster_pos$cluster <- as.character(cluster_pos$cluster)

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF")
species_coords <- scores(phyto_cca, display = "species")

# Créez un dataframe pour stocker les coordonnées des espèces
species_df <- as.data.frame(species_coords)
species_df$name <- rownames(species_df)

species_df <- filter(species_df,name %in% c("Akashiwo"  ,       "Amphora"   ,       "Asterionellopsis" ,"Autre phylum",     "Azadinium",       
                     "Chaetoceros",      "Chaetocerotaceae", "Cylindrotheca",    "Cymatosiraceae",   "Dactyliosolen",   
                     "Delphineis",       "Gymnodiniaceae",   "Heterocapsa",      "Lepidodinium",     "Leptocylindrus",  
                     "Nitzschia",        "Paralia",          "Prorocentrum",     "Pseudo-nitzschia", "Skeletonema",     
                     "Thalassiosira"))

envcoord <- as.data.frame(phyto_cca[["CCA"]][["biplot"]])
envcoord$Var <- rownames(envcoord)


# Créer le graphique avec ggplot2
ggplot(cluster_pos, aes(x = CCA1, y = CCA2)) +
  geom_point(aes(colour=cluster,shape=season),size=5) +
  geom_text(aes(x=CCA1,y=CCA2,label=name),size=3,data=species_df)+
  labs(x = "Dimension 1 (31.97%)", y = "Dimension 2 (21.97%)",colour="Cluster") +
  geom_segment(aes(x = 0, y = 0, xend = CCA1, yend = CCA2), color = "black", size = 0.5,data=species_df,linetype="dashed")+
  geom_segment(aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               arrow = arrow(length = unit(0.08, "inches")), color = "red", size = 0.4,data=envcoord)+
  geom_text(aes(x=CCA1,y=CCA2,label=Var),size=3,data=envcoord,color = "red")+
  labs(x="CCA1 (31.97%)",y="CCA2 (21.97%)",colour="Cluster",shape = "Saison")+
  scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))+
  theme_minimal()

ggplot(observations_df, aes(x = CCA1, y = CCA2)) +
  #geom_point(aes(colour=as.character(cluster))) +
  geom_text(aes(x=CCA1,y=CCA2,label=name),size=3,data=species_df)+
  labs(x = "Dimension 1 (31.97%)", y = "Dimension 2 (21.97%)",colour="Cluster") +
  geom_segment(aes(x = 0, y = 0, xend = CCA1, yend = CCA2), color = "black", size = 0.5,data=species_df)+
  geom_segment(aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               arrow = arrow(length = unit(0.08, "inches")), color = "blue", size = 0.4,data=envcoord)+
  geom_text(aes(x=CCA1,y=CCA2,label=Var),size=3,data=envcoord,color = "blue")+
  #stat_ellipse(aes(x= CCA1,y=CCA2,color = as.character(cluster)), level = 0.95)+
  scale_colour_manual(values=cluster_col)+
  theme_minimal()+
  scale_x_continuous(limits = c(-2,2))+
  scale_y_continuous(limits = c(-2,2))


cca_ts <- summarise(group_by(observations_df, cluster,Month,Year), CCA1=mean(CCA1,na.rm=T),
                         CCA2=mean(CCA2,na.rm=T))

cca_ts$Date <- as.Date(paste(cca_ts$Year, cca_ts$Month, "01", sep = "-"), format = "%Y-%m-%d")



ggplot(cca_ts)+
  geom_line(aes(x=Date,y=CCA1,colour=cluster),size=2)+
  geom_point(aes(x=Date,y=CCA1,colour=cluster),size=2.5)+
  scale_colour_manual(values=cluster_col,guide = "none")+
  facet_wrap(~cluster,ncol=1)+
  scale_x_date(breaks = seq( min(cca_ts$Date), max(cca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(cca_ts$Date), max(cca_ts$Date)))+
  scale_y_continuous(breaks = seq(-9,9, by = 1),limits = c(-2.5,9))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(y= "CCA1 (31.97%)",title = "Composition de la communauté au cours du temps")+

ggplot(cca_ts)+
  geom_line(aes(x=Date,y=CCA2,colour=cluster),size=2)+
  geom_point(aes(x=Date,y=CCA2,colour=cluster),size=2.5)+
  scale_colour_manual(values=cluster_col,guide = "none")+
  facet_wrap(~cluster,ncol=1)+
  scale_x_date(breaks = seq( min(cca_ts$Date), max(cca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(cca_ts$Date), max(cca_ts$Date)))+
  scale_y_continuous(breaks = seq(-3,15, by = 3),limits = c(-3,15))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(y= "CCA2 (21.97%)",title = "Composition de la communauté au cours du temps")
summary(phyto_cca)

### CA ####
phyto <- data[,c(24:247)]
phyto[is.na(phyto)] <- 0
phyto <- phyto[rowSums(phyto != 0) > 0,]

CA_phyto <- CA(phyto)

phyto_coord <- as.data.frame(CA_phyto[["col"]][["coord"]])
phyto_coord$name <- rownames(phyto_coord)

phyto_coord <- filter(phyto_coord,name %in% c("Akashiwo"  ,       "Amphora"   ,       "Asterionellopsis" ,"Autre phylum",     "Azadinium",       
                                            "Chaetoceros",      "Chaetocerotaceae", "Cylindrotheca",    "Cymatosiraceae",   "Dactyliosolen",   
                                            "Delphineis",       "Gymnodiniaceae",   "Heterocapsa",      "Lepidodinium",     "Leptocylindrus",  
                                            "Nitzschia",        "Paralia",          "Prorocentrum",     "Pseudo-nitzschia", "Skeletonema",     
                                            "Thalassiosira"))


obs_coord <- as.data.frame(CA_phyto[["row"]][["coord"]])
obs_coord$cluster <- as.character(info$cluster)
obs_coord$season <- info$season
obs_coord$Month <- info$Month
obs_coord$Year <- info$Year

obs_coord_mean <- summarise(group_by(obs_coord, cluster,season), CA1=mean(`Dim 1`,na.rm=T),
                   CA2=mean(`Dim 2`,na.rm=T),CA3=mean(`Dim 3`,na.rm=T))

ggplot(obs_coord, aes(x = `Dim 1`, y = `Dim 2`)) +
  geom_point(aes(colour=cluster,shape=season),size=2) +
  geom_text(aes(x = `Dim 1`, y = `Dim 2`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA1 (3.71%)", y = "CA2 (3.54%)",colour="Cluster") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 1`, yend = `Dim 2`), color = "black", size = 0.5,data=phyto_coord)+
  #stat_ellipse(aes(x= CCA1,y=CCA2,color = as.character(cluster)), level = 0.95)+
  scale_colour_manual(values=cluster_col)+
  theme_minimal()+
  scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))

ggplot(obs_coord, aes(x = `Dim 2`, y = `Dim 3`)) +
  geom_point(aes(colour=cluster,shape=season),size=2) +
  geom_text(aes(x = `Dim 2`, y = `Dim 3`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA2 (3.54%)", y = "CA3 (3.34%)",colour="Cluster") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 2`, yend = `Dim 3`), color = "black", size = 0.5,data=phyto_coord)+
  #stat_ellipse(aes(x= CCA1,y=CCA2,color = as.character(cluster)), level = 0.95)+
  scale_colour_manual(values=cluster_col)+
  theme_minimal()+
  scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))+
  scale_x_continuous(limits = c(-2,2))+
  scale_y_continuous(limits = c(-2,2))

ggplot(obs_coord_mean, aes(x = CA1, y = CA2)) +
  geom_text(aes(x = `Dim 1`, y = `Dim 2`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA1 (3.71%)", y = "CA2 (3.54%)",colour="Cluster") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 1`, yend = `Dim 2`), color = "black", size = 0.5,data=phyto_coord)+
  #stat_ellipse(aes(x= CCA1,y=CCA2,color = as.character(cluster)), level = 0.95)+
  geom_point(aes(colour=cluster,shape=season),size=2) +
  scale_colour_manual(values=cluster_col)+
  theme_minimal()+
  scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))

ggplot(obs_coord_mean, aes(x = CA2, y = CA3)) +
  geom_text(aes(x = `Dim 2`, y = `Dim 3`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA2 (3.54%)", y = "CA3 (3.34%)",colour="Cluster") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 2`, yend = `Dim 3`), color = "black", size = 0.5,data=phyto_coord)+
  #stat_ellipse(aes(x= CCA1,y=CCA2,color = as.character(cluster)), level = 0.95)+
  scale_colour_manual(values=cluster_col)+
  geom_point(aes(colour=cluster,shape=season),size=2) +
  theme_minimal()+
  scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))+
  scale_x_continuous(limits = c(-2,2))+
  scale_y_continuous(limits = c(-2,2))


ca_ts <- summarise(group_by(obs_coord, cluster,Month,Year), CA1=mean(`Dim 1`,na.rm=T),
                    CA2=mean(`Dim 2`,na.rm=T),CA3=mean(`Dim 3`,na.rm=T))

ca_ts$Date <- as.Date(paste(ca_ts$Year, ca_ts$Month, "01", sep = "-"), format = "%Y-%m-%d")
ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA1,colour=cluster),size=2)+
  geom_point(aes(x=Date,y=CA1,colour=cluster),size=2.5)+
  scale_colour_manual(values=cluster_col,guide = "none")+
  facet_wrap(~cluster,ncol=1)+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  scale_y_continuous(breaks = seq(-1,6, by = 1),limits = c(-1,6))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA1 (3.71%)",title = "Composition de la communauté au cours du temps")+
ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA2,colour=cluster),size=2)+
  geom_point(aes(x=Date,y=CA2,colour=cluster),size=2.5)+
  scale_colour_manual(values=cluster_col,guide = "none")+
  facet_wrap(~cluster,ncol=1)+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  scale_y_continuous(breaks = seq(-1,2, by = 0.5),limits = c(-1,2))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA2 (3.54%)",title = "Composition de la communauté au cours du temps")+
ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA3,colour=cluster),size=2)+
  geom_point(aes(x=Date,y=CA3,colour=cluster),size=2.5)+
  scale_colour_manual(values=cluster_col,guide = "none")+
  facet_wrap(~cluster,ncol=1)+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  scale_y_continuous(breaks = seq(-0.5,6, by = 1),limits = c(-0.5,6))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA3 (3.34%)",title = "Composition de la communauté au cours du temps")







## CCA cluster 1 #####
# 10 Especes majoritaires par saison dans le cluster 1 
data1 <- filter(data, cluster == 1)
result <- data1[,c(3,277,24:247)] %>%
  # Regrouper par saison et cluster
  group_by(season) %>%
  # Calculer la moyenne pour chaque groupe
  summarise_all(~median(., na.rm = TRUE))

result$Abdtot <- rowSums(result[,c(3:226)],na.rm=T)
result[,c(3:226)] <- result[,c(3:226)]/result$Abdtot

datag <- pivot_longer(data = result,cols = Actinoptychus:Coscinodiscophycidae,names_to = "Phylum")
datag$Abdtot <- NULL

result <- datag %>%
  group_by(season, Phylum) %>%
  summarise(abondance = sum(value,na.rm=T)) %>%
  ungroup() %>%
  # Regrouper par saison et cluster, et sélectionner les 10 phylums les plus abondants
  group_by(season) %>%
  mutate(rank = rank(desc(abondance)),
         Phylum = ifelse(rank <= 10, as.character(Phylum), "Autre phylum")) %>%
  group_by(season, Phylum) %>%
  summarise(abondance = sum(abondance,na.rm = T)) %>%
  ungroup()
filter(result, season == "Automne")$Phylum
filter(result, season == "Ete")$Phylum
filter(result, season == "Printemps")$Phylum
filter(result, season == "Hiver")$Phylum



data1 <- filter(data, cluster == 1)
phyto <- data1[,c(24:247)]
phyto[is.na(phyto)] <- 0
phyto_ok <- phyto[rowSums(phyto != 0) > 0,]

Table_PCA <- data1[, c("SALI","TEMP","NH4","PO4","SIOH","OXYGENE","NO3+NO2","TURB-FNU")]
Table_PCA <- Table_PCA[rowSums(phyto != 0) > 0,]

info <- data1[, c("Code_point_Libelle","Date","cluster","season","Month","Year")]
info <- info[rowSums(phyto != 0) > 0,]

# Imputation des valeurs hydro manquantes
ncomp <- estim_ncpPCA(Table_PCA)
res.imp <- imputePCA(Table_PCA, ncp = ncomp$ncp,method = "EM")
envi <- as.data.frame(res.imp$completeObs)


phyto_cca <- cca(phyto_ok ~ ., data=envi)
summary(phyto_cca)
anova(phyto_cca)

plot(phyto_cca, scaling="species",   display=c("wa", "sp", "bp"))


observations_coords <- scores(phyto_cca, display = "sites",choices = c(1:3))

# Créez un dataframe pour stocker les coordonnées des observations
observations_df <- as.data.frame(observations_coords)
observations_df$cluster <- as.character(info$cluster)
observations_df$season <- info$season
observations_df$Month <- info$Month
observations_df$Year <- info$Year

species_coords <- scores(phyto_cca, display = "species",choices = c(1:3))

# Créez un dataframe pour stocker les coordonnées des espèces
species_df <- as.data.frame(species_coords)
species_df$name <- rownames(species_df)

species_df <- filter(species_df,name %in% c(# Automne
  "Asterionellopsis",     "Chaetoceros"  ,    "Cymatosiraceae",   "Dinophyceae.x"  ,  "Gymnodiniales",   
  "Leptocylindrus" ,  "Pseudo-nitzschia" ,"Skeletonema" ,     "Synedra" ,
  # Ete
  "Asterionellopsis" ,    "Chaetoceros"    ,  "Cylindrotheca" ,   "Dinophyceae.x"  ,  "Gymnodiniaceae" , 
  "Gymnodiniales" ,   "Leptocylindrus" ,  "Paralia"     ,     "Pseudo-nitzschia" ,"Skeletonema" ,
  # Printemps
  "Asterionellopsis",    "Bacteriastrum" ,   "Brockmanniella" ,  "Chaetoceros"  ,    "Gymnodiniales"   ,
  "Lauderia"    ,     "Leptocylindrus" ,  "Paralia"     ,     "Pseudo-nitzschia", "Skeletonema",
  # Hiver
  "Asterionellopsis" ,   "Chaetoceros"      ,"Gymnodiniales",    "Lauderia"  ,       "Leptocylindrus" , 
  "Melosira" ,        "Oblea"   ,         "Paralia"   ,       "Pseudo-nitzschia" ,"Skeletonema"))

envcoord <- as.data.frame(phyto_cca[["CCA"]][["biplot"]])
envcoord$Var <- rownames(envcoord)

cluster_pos <- summarise(group_by(observations_df, season), CCA1=mean(CCA1,na.rm=T),
                         CCA2=mean(CCA2,na.rm=T),CCA3=mean(CCA3,na.rm=T))

# Créer le graphique avec ggplot2
ggplot(cluster_pos, aes(x = CCA1, y = CCA2)) +
  geom_segment(aes(x = 0, y = -1, xend = 0, yend = 1),col="grey",size=1)+
  geom_segment(aes(x = -1, y = 0, xend = 1, yend = 0),col="grey",size=1)+
  geom_point(aes(colour=as.factor(season),shape=as.factor(season)),size=5) +
  geom_text(aes(x=CCA1,y=CCA2,label=name),size=3,data=species_df)+
  labs(x = "Dimension 1 (31.97%)", y = "Dimension 2 (21.97%)",colour="Cluster") +
  geom_segment(aes(x = 0, y = 0, xend = CCA1, yend = CCA2), color = "black", size = 0.5,data=species_df,linetype="dashed")+
  geom_segment(aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               arrow = arrow(length = unit(0.08, "inches")), color = "blue", size = 0.4,data=envcoord)+
  geom_text(aes(x=CCA1,y=CCA2,label=Var),size=3,data=envcoord,color = "blue")+
  
  labs(x="CCA1 (64.06%)",y="CCA2 (20.79%)",colour="Saison",shape = "Saison",
       title = "CCA Cluster 1 - 10 taxons maj par saison")+
  #scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))+
  theme_minimal()

species_df <- as.data.frame(species_coords)
species_df$name <- rownames(species_df)

ggplot(observations_df, aes(x = CCA1, y = CCA2)) +
  geom_point(aes(colour=as.factor(season),shape=as.factor(season)),size=1) +
  geom_text(aes(x=CCA1,y=CCA2,label=name),size=3,data=species_df)+
  labs(x = "Dimension 1 (31.97%)", y = "Dimension 2 (21.97%)",colour="Cluster") +
  geom_segment(aes(x = 0, y = 0, xend = CCA1, yend = CCA2), color = "black", size = 0.5,data=species_df,linetype="dashed")+
  geom_segment(aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               arrow = arrow(length = unit(0.08, "inches")), color = "red", size = 0.4,data=envcoord)+
  geom_text(aes(x=CCA1,y=CCA2,label=Var),size=3,data=envcoord,color = "red")+
  scale_x_continuous(limits = c(-1,1))+
  scale_y_continuous(limits = c(-2,2))+
  labs(x="CCA1 (64.06%)",y="CCA2 (20.79%)",colour="Cluster",shape = "Saison")+
  #scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))+
  theme_minimal()


ggplot(cluster_pos, aes(x = CCA2, y = CCA3)) +
  geom_segment(aes(x = 0, y = -1, xend = 0, yend = 1),col="grey",size=1)+
  geom_segment(aes(x = -1, y = 0, xend = 1, yend = 0),col="grey",size=1)+
  geom_point(aes(colour=as.factor(season),shape=as.factor(season)),size=5) +
  geom_text(aes(x=CCA2,y=CCA3,label=name),size=3,data=species_df)+
  labs(x = "Dimension 1 (31.97%)", y = "Dimension 2 (21.97%)",colour="Cluster") +
  geom_segment(aes(x = 0, y = 0, xend = CCA2, yend = CCA3), color = "black", size = 0.5,data=species_df,linetype="dashed")+
  geom_segment(aes(x = 0, y = 0, xend = CCA2, yend = CCA3),
               arrow = arrow(length = unit(0.08, "inches")), color = "blue", size = 0.4,data=envcoord)+
  geom_text(aes(x=CCA2,y=CCA3,label=Var),size=3,data=envcoord,color = "blue")+
  
  labs(x="CCA2 (20.79%)",y="CCA3 (6.58%)",colour="Saison",shape = "Saison",
       title = "CCA Cluster 1 - 10 taxons maj par saison")+
  #scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))+
  theme_minimal()

species_df <- as.data.frame(species_coords)
species_df$name <- rownames(species_df)

ggplot(observations_df, aes(x = CCA2, y = CCA3)) +
  geom_point(aes(colour=as.factor(season),shape=as.factor(season)),size=1) +
  geom_text(aes(x=CCA2,y=CCA3,label=name),size=3,data=species_df)+
  labs(x = "Dimension 2 (31.97%)", y = "Dimension 3 (21.97%)",colour="Cluster") +
  geom_segment(aes(x = 0, y = 0, xend = CCA2, yend = CCA3), color = "black", size = 0.5,data=species_df,linetype="dashed")+
  geom_segment(aes(x = 0, y = 0, xend = CCA2, yend = CCA3),
               arrow = arrow(length = unit(0.08, "inches")), color = "red", size = 0.4,data=envcoord)+
  geom_text(aes(x=CCA2,y=CCA3,label=Var),size=3,data=envcoord,color = "red")+
  scale_x_continuous(limits = c(-1,1))+
  scale_y_continuous(limits = c(-2,2))+
  labs(x="CCA2 (20.79%)",y="CCA3 (6.58%)",colour="Cluster",shape = "Saison")+
  #scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))+
  theme_minimal()


cca_ts <- summarise(group_by(observations_df, Month,Year), CCA1=mean(CCA1,na.rm=T),
                    CCA2=mean(CCA2,na.rm=T),CCA3=mean(CCA3,na.rm=T))

cca_ts$Date <- as.Date(paste(cca_ts$Year, cca_ts$Month, "01", sep = "-"), format = "%Y-%m-%d")



a <- ggplot(cca_ts)+
  geom_line(aes(x=Date,y=CCA1),size=2,col="#F8766D")+
  geom_point(aes(x=Date,y=CCA1),size=2.5,col="#F8766D")+
  #scale_colour_manual(values=cluster_col,guide = "none")+
  #facet_wrap(~cluster,ncol=1)+
  scale_x_date(breaks = seq( min(cca_ts$Date), max(cca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(cca_ts$Date), max(cca_ts$Date)))+
  #scale_y_continuous(breaks = seq(-2.5,1.5, by = 0.5),limits = c(-2.5,1.5))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(y= "CCA1 (64.06%)",title = "Composition de la communauté au cours du temps")
  
b<-  ggplot(cca_ts)+
  geom_line(aes(x=Date,y=CCA2),size=2,col="#F8766D")+
  geom_point(aes(x=Date,y=CCA2),size=2.5,col="#F8766D")+
  #scale_colour_manual(values=cluster_col,guide = "none")+
  #facet_wrap(~cluster,ncol=1)+
  scale_x_date(breaks = seq( min(cca_ts$Date), max(cca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(cca_ts$Date), max(cca_ts$Date)))+
  #scale_y_continuous(breaks = seq(-2.5,1.5, by = 0.5),limits = c(-2.5,1.5))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(y= "CCA2 (20.79%)",title = "Composition de la communauté au cours du temps")

c <- ggplot(cca_ts)+
  geom_line(aes(x=Date,y=CCA3),size=2,col="#F8766D")+
  geom_point(aes(x=Date,y=CCA3),size=2.5,col="#F8766D")+
  #scale_colour_manual(values=cluster_col,guide = "none")+
  #facet_wrap(~cluster,ncol=1)+
  scale_x_date(breaks = seq( min(cca_ts$Date), max(cca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(cca_ts$Date), max(cca_ts$Date)))+
  #scale_y_continuous(breaks = seq(-2.5,1.5, by = 0.5),limits = c(-2.5,1.5))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(y= "CCA3 (6.58%)",title = "Composition de la communauté au cours du temps")
summary(phyto_cca)
a/b/c



## CCA cluster 2 #####
# 10 Especes majoritaires par saison dans le cluster 2 
data2 <- filter(data, cluster == 2)
result <- data2[,c(3,277,24:247)] %>%
  # Regrouper par saison et cluster
  group_by(season) %>%
  # Calculer la moyenne pour chaque groupe
  summarise_all(~median(., na.rm = TRUE))

result$Abdtot <- rowSums(result[,c(3:226)],na.rm=T)
result[,c(3:226)] <- result[,c(3:226)]/result$Abdtot

datag <- pivot_longer(data = result,cols = Actinoptychus:Coscinodiscophycidae,names_to = "Phylum")
datag$Abdtot <- NULL

result <- datag %>%
  group_by(season, Phylum) %>%
  summarise(abondance = sum(value,na.rm=T)) %>%
  ungroup() %>%
  # Regrouper par saison et cluster, et sélectionner les 10 phylums les plus abondants
  group_by(season) %>%
  mutate(rank = rank(desc(abondance)),
         Phylum = ifelse(rank <= 10, as.character(Phylum), "Autre phylum")) %>%
  group_by(season, Phylum) %>%
  summarise(abondance = sum(abondance,na.rm = T)) %>%
  ungroup()
filter(result, season == "Automne")$Phylum
filter(result, season == "Ete")$Phylum
filter(result, season == "Printemps")$Phylum
filter(result, season == "Hiver")$Phylum



data2 <- filter(data, cluster == 2)
phyto <- data2[,c(24:247)]
phyto[is.na(phyto)] <- 0
phyto_ok <- phyto[rowSums(phyto != 0) > 0,]

Table_PCA <- data2[, c("SALI","TEMP","NH4","PO4","SIOH","OXYGENE","NO3+NO2","TURB-FNU")]
Table_PCA <- Table_PCA[rowSums(phyto != 0) > 0,]

info <- data2[, c("Code_point_Libelle","Date","cluster","season","Month","Year")]
info <- info[rowSums(phyto != 0) > 0,]

# Imputation des valeurs hydro manquantes
ncomp <- estim_ncpPCA(Table_PCA)
res.imp <- imputePCA(Table_PCA, ncp = ncomp$ncp,method = "EM")
envi <- as.data.frame(res.imp$completeObs)


phyto_cca <- cca(phyto_ok ~ ., data=envi)
summary(phyto_cca)
anova(phyto_cca)

plot(phyto_cca, scaling="species",   display=c("wa", "sp", "bp"))


observations_coords <- scores(phyto_cca, display = "sites",choices = c(1:3))

# Créez un dataframe pour stocker les coordonnées des observations
observations_df <- as.data.frame(observations_coords)
observations_df$cluster <- as.character(info$cluster)
observations_df$season <- info$season
observations_df$Month <- info$Month
observations_df$Year <- info$Year

species_coords <- scores(phyto_cca, display = "species",choices = c(1:3))

# Créez un dataframe pour stocker les coordonnées des espèces
species_df <- as.data.frame(species_coords)
species_df$name <- rownames(species_df)

species_df <- filter(species_df,name %in% c(# Automne
  "Akashiwo"   ,      "Asterionellopsis"   , "Azadinium"       , "Brockmanniella",   "Chaetoceros"  ,   
  "Delphineis"    ,   "Lepidodinium"  ,   "Leptocylindrus"  , "Paralia"      ,    "Skeletonema" ,
  # Ete
  "Asterionellopsis" ,    "Brockmanniella",   "Chaetoceros"  ,    "Delphineis"   ,    "Guinardia"    ,   "Lepidodinium"  ,   "Leptocylindrus"  , "Pseudo-nitzschia" ,"Rhizosolenia"  ,   "Skeletonema"  ,
  # Printemps
  "Asterionellopsis" ,"Chaetoceros" ,     "Fragilariaceae"  , "Lauderia"      ,   "Leptocylindrus" , "Melosiraceae",     "Pseudo-nitzschia" ,"Rhizosolenia"     ,"Skeletonema"  ,    "Thalassiosira" ,
  # Hiver
  "Asterionellopsis" ,"Attheya"    ,   "Brockmanniella"  , "Delphineis"    ,   "Paralia" ,        
  "Plagiogramma"   ,  "Proboscia"   ,     "Skeletonema"  ,    "Thalassiosira"  ,  "Triceratium") )

envcoord <- as.data.frame(phyto_cca[["CCA"]][["biplot"]])
envcoord$Var <- rownames(envcoord)

cluster_pos <- summarise(group_by(observations_df, season), CCA1=mean(CCA1,na.rm=T),
                         CCA2=mean(CCA2,na.rm=T),CCA3 = mean(CCA3, na.rm=T))

# Créer le graphique avec ggplot2
ggplot(cluster_pos, aes(x = CCA1, y = CCA2)) +
  geom_segment(aes(x = 0, y = -3.5, xend = 0, yend = 1),col="grey",size=1)+
  geom_segment(aes(x = -1, y = 0, xend = 2.5, yend = 0),col="grey",size=1)+
  geom_point(aes(colour=as.factor(season),shape=as.factor(season)),size=5) +
  geom_text(aes(x=CCA1,y=CCA2,label=name),size=3,data=species_df)+
  labs(x = "Dimension 1 (31.97%)", y = "Dimension 2 (21.97%)",colour="Cluster") +
  geom_segment(aes(x = 0, y = 0, xend = CCA1, yend = CCA2), color = "black", size = 0.5,data=species_df,linetype="dashed")+
  geom_segment(aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               arrow = arrow(length = unit(0.08, "inches")), color = "blue", size = 0.4,data=envcoord)+
  geom_text(aes(x=CCA1,y=CCA2,label=Var),size=3,data=envcoord,color = "blue")+
  
  labs(x="CCA1 (40.10%)",y="CCA2 (19.92%)",colour="Saison",shape = "Saison",
       title = "CCA Cluster 2 - 10 taxons maj par saison")+
  #scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))+
  theme_minimal()

species_df <- as.data.frame(species_coords)
species_df$name <- rownames(species_df)

ggplot(observations_df, aes(x = CCA1, y = CCA2)) +
  geom_point(aes(colour=as.factor(season),shape=as.factor(season)),size=1) +
  geom_text(aes(x=CCA1,y=CCA2,label=name),size=3,data=species_df)+
  labs(x = "Dimension 1 (31.97%)", y = "Dimension 2 (21.97%)",colour="Cluster") +
  geom_segment(aes(x = 0, y = 0, xend = CCA1, yend = CCA2), color = "black", size = 0.5,data=species_df,linetype="dashed")+
  geom_segment(aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               arrow = arrow(length = unit(0.08, "inches")), color = "red", size = 0.4,data=envcoord)+
  geom_text(aes(x=CCA1,y=CCA2,label=Var),size=3,data=envcoord,color = "red")+
  #scale_x_continuous(limits = c(-1,1))+
  #scale_y_continuous(limits = c(-2,2))+
  labs(x="CCA1 (40.10%)",y="CCA2 (19.92%)",colour="Cluster",shape = "Saison")+
  #scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))+
  theme_minimal()

ggplot(cluster_pos, aes(x = CCA2, y = CCA3)) +
  geom_segment(aes(x = 0, y = -1, xend = 0, yend = 1),col="grey",size=1)+
  geom_segment(aes(x = -1, y = 0, xend = 1, yend = 0),col="grey",size=1)+
  geom_point(aes(colour=as.factor(season),shape=as.factor(season)),size=5) +
  geom_text(aes(x=CCA2,y=CCA3,label=name),size=3,data=species_df)+
  geom_segment(aes(x = 0, y = 0, xend = CCA2, yend = CCA3), color = "black", size = 0.5,data=species_df,linetype="dashed")+
  geom_segment(aes(x = 0, y = 0, xend = CCA2, yend = CCA3),
               arrow = arrow(length = unit(0.08, "inches")), color = "blue", size = 0.4,data=envcoord)+
  geom_text(aes(x=CCA2,y=CCA3,label=Var),size=3,data=envcoord,color = "blue")+
  
  labs(x="CCA2 (19.92%)",y="CCA3 (18.17%)",colour="Saison",shape = "Saison",
       title = "CCA Cluster 2 - 10 taxons maj par saison")+
  #scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))+
  theme_minimal()

species_df <- as.data.frame(species_coords)
species_df$name <- rownames(species_df)

ggplot(observations_df, aes(x = CCA2, y = CCA3)) +
  geom_point(aes(colour=as.factor(season),shape=as.factor(season)),size=1) +
  geom_text(aes(x=CCA2,y=CCA3,label=name),size=3,data=species_df)+
  labs(x = "Dimension 2 (31.97%)", y = "Dimension 3 (21.97%)",colour="Cluster") +
  geom_segment(aes(x = 0, y = 0, xend = CCA2, yend = CCA3), color = "black", size = 0.5,data=species_df,linetype="dashed")+
  geom_segment(aes(x = 0, y = 0, xend = CCA2, yend = CCA3),
               arrow = arrow(length = unit(0.08, "inches")), color = "red", size = 0.4,data=envcoord)+
  geom_text(aes(x=CCA2,y=CCA3,label=Var),size=3,data=envcoord,color = "red")+
  scale_x_continuous(limits = c(-1,1))+
  scale_y_continuous(limits = c(-2,2))+
  labs(x="CCA2 (19.92%)",y="CCA3 (18.17%)",colour="Cluster",shape = "Saison")+
  #scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))+
  theme_minimal()


cca_ts <- summarise(group_by(observations_df, Month,Year), CCA1=mean(CCA1,na.rm=T),
                    CCA2=mean(CCA2,na.rm=T),CCA3=mean(CCA3,na.rm=T))


cca_ts$Date <- as.Date(paste(cca_ts$Year, cca_ts$Month, "01", sep = "-"), format = "%Y-%m-%d")



a <- ggplot(cca_ts)+
  geom_line(aes(x=Date,y=CCA1),size=2,col="#CD9600")+
  geom_point(aes(x=Date,y=CCA1),size=2.5,col="#CD9600")+
  #scale_colour_manual(values=cluster_col,guide = "none")+
  #facet_wrap(~cluster,ncol=1)+
  scale_x_date(breaks = seq( min(cca_ts$Date), max(cca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(cca_ts$Date), max(cca_ts$Date)))+
  #scale_y_continuous(breaks = seq(-2.5,1.5, by = 0.5),limits = c(-2.5,1.5))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(y= "CCA1 (40.10%)",title = "Composition de la communauté au cours du temps")

b<-  ggplot(cca_ts)+
  geom_line(aes(x=Date,y=CCA2),size=2,col="#CD9600")+
  geom_point(aes(x=Date,y=CCA2),size=2.5,col="#CD9600")+
  #scale_colour_manual(values=cluster_col,guide = "none")+
  #facet_wrap(~cluster,ncol=1)+
  scale_x_date(breaks = seq( min(cca_ts$Date), max(cca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(cca_ts$Date), max(cca_ts$Date)))+
  #scale_y_continuous(breaks = seq(-2.5,1.5, by = 0.5),limits = c(-2.5,1.5))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(y= "CCA2 (19.92%)",title = "Composition de la communauté au cours du temps")

c <-  ggplot(cca_ts)+
  geom_line(aes(x=Date,y=CCA3),size=2,col="#CD9600")+
  geom_point(aes(x=Date,y=CCA3),size=2.5,col="#CD9600")+
  #scale_colour_manual(values=cluster_col,guide = "none")+
  #facet_wrap(~cluster,ncol=1)+
  scale_x_date(breaks = seq( min(cca_ts$Date), max(cca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(cca_ts$Date), max(cca_ts$Date)))+
  #scale_y_continuous(breaks = seq(-2.5,1.5, by = 0.5),limits = c(-2.5,1.5))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(y= "CCA3 (18.17%)",title = "Composition de la communauté au cours du temps")
a/b/c


## CCA cluster 3 #####
# 10 Especes majoritaires par saison dans le cluster 3
data3 <- filter(data, cluster == 3)
result <- data3[,c(3,277,24:247)] %>%
  # Regrouper par saison et cluster
  group_by(season) %>%
  # Calculer la moyenne pour chaque groupe
  summarise_all(~median(., na.rm = TRUE))

result$Abdtot <- rowSums(result[,c(3:226)],na.rm=T)
result[,c(3:226)] <- result[,c(3:226)]/result$Abdtot

datag <- pivot_longer(data = result,cols = Actinoptychus:Coscinodiscophycidae,names_to = "Phylum")
datag$Abdtot <- NULL

result <- datag %>%
  group_by(season, Phylum) %>%
  summarise(abondance = sum(value,na.rm=T)) %>%
  ungroup() %>%
  # Regrouper par saison et cluster, et sélectionner les 10 phylums les plus abondants
  group_by(season) %>%
  mutate(rank = rank(desc(abondance)),
         Phylum = ifelse(rank <= 10, as.character(Phylum), "Autre phylum")) %>%
  group_by(season, Phylum) %>%
  summarise(abondance = sum(abondance,na.rm = T)) %>%
  ungroup()
filter(result, season == "Automne")$Phylum
filter(result, season == "Ete")$Phylum
filter(result, season == "Printemps")$Phylum
filter(result, season == "Hiver")$Phylum



data3 <- filter(data, cluster == 3)
phyto <- data3[,c(24:247)]
phyto[is.na(phyto)] <- 0
phyto_ok <- phyto[rowSums(phyto != 0) > 0,]

Table_PCA <- data3[, c("SALI","TEMP","NH4","PO4","SIOH","OXYGENE","NO3+NO2","TURB-FNU")]
Table_PCA <- Table_PCA[rowSums(phyto != 0) > 0,]

info <- data3[, c("Code_point_Libelle","Date","cluster","season","Month","Year")]
info <- info[rowSums(phyto != 0) > 0,]

# Imputation des valeurs hydro manquantes
ncomp <- estim_ncpPCA(Table_PCA)
res.imp <- imputePCA(Table_PCA, ncp = ncomp$ncp,method = "EM")
envi <- as.data.frame(res.imp$completeObs)


phyto_cca <- cca(phyto_ok ~ ., data=envi)
summary(phyto_cca)
anova(phyto_cca)

plot(phyto_cca, scaling="species",   display=c("wa", "sp", "bp"))


observations_coords <- scores(phyto_cca, display = "sites",choices = c(1:3))

# Créez un dataframe pour stocker les coordonnées des observations
observations_df <- as.data.frame(observations_coords)
observations_df$cluster <- as.character(info$cluster)
observations_df$season <- info$season
observations_df$Month <- info$Month
observations_df$Year <- info$Year

species_coords <- scores(phyto_cca, display = "species",choices = c(1:3))

# Créez un dataframe pour stocker les coordonnées des espèces
species_df <- as.data.frame(species_coords)
species_df$name <- rownames(species_df)

species_df <- filter(species_df,name %in% c(# Automne
  "Asterionellopsis", "Asteroplanus" , "Chaetoceros"     , "Chaetocerotaceae" ,"Cylindrotheca"  , "Guinardia"       , "Leptocylindrus",   "Lithodesmium"  ,   "Paralia"   ,       "Skeletonema" ,    
  # Ete
  "Asterionellopsis" ,  "Chaetoceros"  ,    "Chaetocerotaceae", "Dactyliosolen" ,   "Guinardia"   ,    "Lepidodinium",     "Leptocylindrus"  , "Melosira"  ,       "Pseudo-nitzschia" ,"Skeletonema" ,    
  # Printemps
  "Asterionellopsis", "Brockmanniella"  , "Chaetoceros" ,     "Chaetocerotaceae" ,"Dactyliosolen",   
  "Leptocylindrus" ,  "Peridiniaceae"   , "Plagiogramma" ,    "Skeletonema"  ,    "Thalassiosira" ,
  "Asterionellopsis",  "Azadinium"      ,  "Brockmanniella" ,  "Chaetoceros"   ,   "Delphineis"  ,    "Paralia" ,         "Peridiniaceae" ,   "Plagiogramma"   ,  "Skeletonema"    ,  "Thalassiosira" )  )

envcoord <- as.data.frame(phyto_cca[["CCA"]][["biplot"]])
envcoord$Var <- rownames(envcoord)

cluster_pos <- summarise(group_by(observations_df, season), CCA1=mean(CCA1,na.rm=T),
                         CCA2=mean(CCA2,na.rm=T),CCA3=mean(CCA3,na.rm=T))

# Créer le graphique avec ggplot2
ggplot(cluster_pos, aes(x = CCA1, y = CCA2)) +
  geom_segment(aes(x = 0, y = -8, xend = 0, yend = 1),col="grey",size=1)+
  geom_segment(aes(x = -4.5, y = 0, xend = 2, yend = 0),col="grey",size=1)+
  geom_point(aes(colour=as.factor(season),shape=as.factor(season)),size=5) +
  geom_text(aes(x=CCA1,y=CCA2,label=name),size=3,data=species_df)+
  labs(x = "Dimension 1 (31.97%)", y = "Dimension 2 (21.97%)",colour="Cluster") +
  geom_segment(aes(x = 0, y = 0, xend = CCA1, yend = CCA2), color = "black", size = 0.5,data=species_df,linetype="dashed")+
  geom_segment(aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               arrow = arrow(length = unit(0.08, "inches")), color = "blue", size = 0.4,data=envcoord)+
  geom_text(aes(x=CCA1,y=CCA2,label=Var),size=3,data=envcoord,color = "blue")+
  
  labs(x="CCA1 (45.66%)",y="CCA2 (16.80%)",colour="Saison",shape = "Saison",
       title = "CCA Cluster 3 - 10 taxons maj par saison")+
  #scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))+
  theme_minimal()

species_df <- as.data.frame(species_coords)
species_df$name <- rownames(species_df)

ggplot(observations_df, aes(x = CCA1, y = CCA2)) +
  geom_point(aes(colour=as.factor(season),shape=as.factor(season)),size=1) +
  geom_text(aes(x=CCA1,y=CCA2,label=name),size=3,data=species_df)+
  labs(x = "Dimension 1 (31.97%)", y = "Dimension 2 (21.97%)",colour="Cluster") +
  geom_segment(aes(x = 0, y = 0, xend = CCA1, yend = CCA2), color = "black", size = 0.5,data=species_df,linetype="dashed")+
  geom_segment(aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               arrow = arrow(length = unit(0.08, "inches")), color = "red", size = 0.4,data=envcoord)+
  geom_text(aes(x=CCA1,y=CCA2,label=Var),size=3,data=envcoord,color = "red")+
  scale_x_continuous(limits = c(-1,1))+
  scale_y_continuous(limits = c(-2,2))+
  labs(x="CCA1 (45.66%)",y="CCA2 (16.80%)",colour="Saison",shape = "Saison")+
  #scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))+
  theme_minimal()



ggplot(cluster_pos, aes(x = CCA2, y = CCA3)) +
  geom_segment(aes(x = 0, y = -1, xend = 0, yend = 1),col="grey",size=1)+
  geom_segment(aes(x = -1, y = 0, xend = 1, yend = 0),col="grey",size=1)+
  geom_point(aes(colour=as.factor(season),shape=as.factor(season)),size=5) +
  geom_text(aes(x=CCA2,y=CCA3,label=name),size=3,data=species_df)+
  geom_segment(aes(x = 0, y = 0, xend = CCA2, yend = CCA3), color = "black", size = 0.5,data=species_df,linetype="dashed")+
  geom_segment(aes(x = 0, y = 0, xend = CCA2, yend = CCA3),
               arrow = arrow(length = unit(0.08, "inches")), color = "blue", size = 0.4,data=envcoord)+
  geom_text(aes(x=CCA2,y=CCA3,label=Var),size=3,data=envcoord,color = "blue")+
  
  labs(x="CCA2 (16.80%)",y="CCA3 (12.79%)",colour="Saison",shape = "Saison",
       title = "CCA Cluster 3 - 10 taxons maj par saison")+
  #scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))+
  theme_minimal()

species_df <- as.data.frame(species_coords)
species_df$name <- rownames(species_df)

ggplot(observations_df, aes(x = CCA2, y = CCA3)) +
  geom_point(aes(colour=as.factor(season),shape=as.factor(season)),size=1) +
  geom_text(aes(x=CCA2,y=CCA3,label=name),size=3,data=species_df)+
  labs(x = "Dimension 2 (16.80%)", y = "Dimension 3 (12.79%)",colour="Cluster") +
  geom_segment(aes(x = 0, y = 0, xend = CCA2, yend = CCA3), color = "black", size = 0.5,data=species_df,linetype="dashed")+
  geom_segment(aes(x = 0, y = 0, xend = CCA2, yend = CCA3),
               arrow = arrow(length = unit(0.08, "inches")), color = "red", size = 0.4,data=envcoord)+
  geom_text(aes(x=CCA2,y=CCA3,label=Var),size=3,data=envcoord,color = "red")+
  scale_x_continuous(limits = c(-1,1))+
  scale_y_continuous(limits = c(-2,2))+
  labs(x="CCA2 (16.80%)",y="CCA3 (12.79%)",colour="Cluster",shape = "Saison")+
  #scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))+
  theme_minimal()


cca_ts <- summarise(group_by(observations_df, Month,Year), CCA1=mean(CCA1,na.rm=T),
                    CCA2=mean(CCA2,na.rm=T),CCA3=mean(CCA3,na.rm=T))


cca_ts$Date <- as.Date(paste(cca_ts$Year, cca_ts$Month, "01", sep = "-"), format = "%Y-%m-%d")



a <- ggplot(cca_ts)+
  geom_line(aes(x=Date,y=CCA1),size=2,col="#00BE67")+
  geom_point(aes(x=Date,y=CCA1),size=2.5,col="#00BE67")+
  #scale_colour_manual(values=cluster_col,guide = "none")+
  #facet_wrap(~cluster,ncol=1)+
  scale_x_date(breaks = seq( min(cca_ts$Date), max(cca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(cca_ts$Date), max(cca_ts$Date)))+
  #scale_y_continuous(breaks = seq(-2.5,1.5, by = 0.5),limits = c(-2.5,1.5))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(y= "CCA1 (45.66%)",title = "Composition de la communauté au cours du temps")

b<-  ggplot(cca_ts)+
  geom_line(aes(x=Date,y=CCA2),size=2,col="#00BE67")+
  geom_point(aes(x=Date,y=CCA2),size=2.5,col="#00BE67")+
  #scale_colour_manual(values=cluster_col,guide = "none")+
  #facet_wrap(~cluster,ncol=1)+
  scale_x_date(breaks = seq( min(cca_ts$Date), max(cca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(cca_ts$Date), max(cca_ts$Date)))+
  #scale_y_continuous(breaks = seq(-2.5,1.5, by = 0.5),limits = c(-2.5,1.5))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(y= "CCA2 (16.80%)",title = "Composition de la communauté au cours du temps")

c<-  ggplot(cca_ts)+
  geom_line(aes(x=Date,y=CCA3),size=2,col="#00BE67")+
  geom_point(aes(x=Date,y=CCA3),size=2.5,col="#00BE67")+
  #scale_colour_manual(values=cluster_col,guide = "none")+
  #facet_wrap(~cluster,ncol=1)+
  scale_x_date(breaks = seq( min(cca_ts$Date), max(cca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(cca_ts$Date), max(cca_ts$Date)))+
  #scale_y_continuous(breaks = seq(-2.5,1.5, by = 0.5),limits = c(-2.5,1.5))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(y= "CCA3 (12.79%)",title = "Composition de la communauté au cours du temps")
a/b/c


## CCA cluster 4 #####
# 10 Especes majoritaires par saison dans le cluster 4
data4 <- filter(data, cluster == 4)
result <- data4[,c(3,277,24:247)] %>%
  # Regrouper par saison et cluster
  group_by(season) %>%
  # Calculer la moyenne pour chaque groupe
  summarise_all(~median(., na.rm = TRUE))

result$Abdtot <- rowSums(result[,c(3:226)],na.rm=T)
result[,c(3:226)] <- result[,c(3:226)]/result$Abdtot

datag <- pivot_longer(data = result,cols = Actinoptychus:Coscinodiscophycidae,names_to = "Phylum")
datag$Abdtot <- NULL

result <- datag %>%
  group_by(season, Phylum) %>%
  summarise(abondance = sum(value,na.rm=T)) %>%
  ungroup() %>%
  # Regrouper par saison et cluster, et sélectionner les 10 phylums les plus abondants
  group_by(season) %>%
  mutate(rank = rank(desc(abondance)),
         Phylum = ifelse(rank <= 10, as.character(Phylum), "Autre phylum")) %>%
  group_by(season, Phylum) %>%
  summarise(abondance = sum(abondance,na.rm = T)) %>%
  ungroup()
filter(result, season == "Automne")$Phylum
filter(result, season == "Ete")$Phylum
filter(result, season == "Printemps")$Phylum
filter(result, season == "Hiver")$Phylum



data4 <- filter(data, cluster == 4)
phyto <- data4[,c(24:247)]
phyto[is.na(phyto)] <- 0
phyto_ok <- phyto[rowSums(phyto != 0) > 0,]

Table_PCA <- data4[, c("SALI","TEMP","NH4","PO4","SIOH","OXYGENE","NO3+NO2","TURB-FNU")]
Table_PCA <- Table_PCA[rowSums(phyto != 0) > 0,]

info <- data4[, c("Code_point_Libelle","Date","cluster","season","Month","Year")]
info <- info[rowSums(phyto != 0) > 0,]

# Imputation des valeurs hydro manquantes
ncomp <- estim_ncpPCA(Table_PCA)
res.imp <- imputePCA(Table_PCA, ncp = ncomp$ncp,method = "EM")
envi <- as.data.frame(res.imp$completeObs)


phyto_cca <- cca(phyto_ok ~ ., data=envi)
summary(phyto_cca)
anova(phyto_cca)

plot(phyto_cca, scaling="species",   display=c("wa", "sp", "bp"))


observations_coords <- scores(phyto_cca, display = "sites",choices = c(1:3))

# Créez un dataframe pour stocker les coordonnées des observations
observations_df <- as.data.frame(observations_coords)
observations_df$cluster <- as.character(info$cluster)
observations_df$season <- info$season
observations_df$Month <- info$Month
observations_df$Year <- info$Year

species_coords <- scores(phyto_cca, display = "species", choices = c(1:3))

# Créez un dataframe pour stocker les coordonnées des espèces
species_df <- as.data.frame(species_coords)
species_df$name <- rownames(species_df)

species_df <- filter(species_df,name %in% c(# Automne
  "Bacteriastrum" , "Chaetoceros"  ,  "Cymatosiraceae" ,"Fragilariaceae", "Guinardia",      "Leptocylindrus",
  "Paralia" ,       "Skeletonema" ,   "Thalassiosira" ,
  # Ete
  "Amphora" , "Chaetoceros" ,   "Cylindrotheca" , "Gymnodiniaceae", "Gymnodinium"  ,  "Lepidodinium",  
  "Leptocylindrus", "Prorocentrum" ,  "Scrippsiella"  , "Thalassiosira", 
  # Printemps
  "Asterionella" ,       "Asterionellopsis" , "Chaetoceros"   ,      "Cylindrotheca" ,     
  "Cymatosiraceae"   ,   "Leptocylindrus"  ,    "Paralia"    ,         "Skeletonema"     ,    "Thalassionemataceae",
  "Thalassiosira" ,     
  # Hiver
  "Asterionellopsis" , "Bacillaria"      , "Cymatosiraceae" ,  "Fragilariaceae" ,  "Melosiraceae"  ,  
  "Paralia"    ,      "Pennées"   ,       "Plagiogrammopsis" ,"Skeletonema"     , "Thalassiosira")  )

envcoord <- as.data.frame(phyto_cca[["CCA"]][["biplot"]])
envcoord$Var <- rownames(envcoord)

cluster_pos <- summarise(group_by(observations_df, season), CCA1=mean(CCA1,na.rm=T),
                         CCA2=mean(CCA2,na.rm=T),CCA3=mean(CCA3,na.rm=T))

# Créer le graphique avec ggplot2
ggplot(cluster_pos, aes(x = CCA1, y = CCA2)) +
  geom_segment(aes(x = 0, y = -2.5, xend = 0, yend = .5),col="grey",size=1)+
  geom_segment(aes(x = -2, y = 0, xend = 2, yend = 0),col="grey",size=1)+
  geom_point(aes(colour=as.factor(season),shape=as.factor(season)),size=5) +
  geom_text(aes(x=CCA1,y=CCA2,label=name),size=3,data=species_df)+
  labs(x = "Dimension 1 (31.97%)", y = "Dimension 2 (21.97%)",colour="Cluster") +
  geom_segment(aes(x = 0, y = 0, xend = CCA1, yend = CCA2), color = "black", size = 0.5,data=species_df,linetype="dashed")+
  geom_segment(aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               arrow = arrow(length = unit(0.08, "inches")), color = "blue", size = 0.4,data=envcoord)+
  geom_text(aes(x=CCA1,y=CCA2,label=Var),size=3,data=envcoord,color = "blue")+
  
  labs(x="CCA1 (37.33%)",y="CCA2 (27.82%)",colour="Saison",shape = "Saison",
       title = "CCA Cluster 4 - 10 taxons maj par saison")+
  #scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))+
  theme_minimal()

species_df <- as.data.frame(species_coords)
species_df$name <- rownames(species_df)

ggplot(observations_df, aes(x = CCA1, y = CCA2)) +
  geom_point(aes(colour=as.factor(season),shape=as.factor(season)),size=1) +
  geom_text(aes(x=CCA1,y=CCA2,label=name),size=3,data=species_df)+
  labs(x = "Dimension 1 (31.97%)", y = "Dimension 2 (21.97%)",colour="Cluster") +
  geom_segment(aes(x = 0, y = 0, xend = CCA1, yend = CCA2), color = "black", size = 0.5,data=species_df,linetype="dashed")+
  geom_segment(aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               arrow = arrow(length = unit(0.08, "inches")), color = "red", size = 0.4,data=envcoord)+
  geom_text(aes(x=CCA1,y=CCA2,label=Var),size=3,data=envcoord,color = "red")+
  scale_x_continuous(limits = c(-1,1))+
  scale_y_continuous(limits = c(-2,2))+
  labs(x="CCA1 (37.33%)",y="CCA2 (27.82%)",colour="Saison",shape = "Saison")+
  #scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))+
  theme_minimal()

ggplot(cluster_pos, aes(x = CCA2, y = CCA3)) +
  geom_segment(aes(x = 0, y = -1, xend = 0, yend = 1),col="grey",size=1)+
  geom_segment(aes(x = -1, y = 0, xend = 1, yend = 0),col="grey",size=1)+
  geom_point(aes(colour=as.factor(season),shape=as.factor(season)),size=5) +
  geom_text(aes(x=CCA2,y=CCA3,label=name),size=3,data=species_df)+
  geom_segment(aes(x = 0, y = 0, xend = CCA2, yend = CCA3), color = "black", size = 0.5,data=species_df,linetype="dashed")+
  geom_segment(aes(x = 0, y = 0, xend = CCA2, yend = CCA3),
               arrow = arrow(length = unit(0.08, "inches")), color = "blue", size = 0.4,data=envcoord)+
  geom_text(aes(x=CCA2,y=CCA3,label=Var),size=3,data=envcoord,color = "blue")+
  
  labs(x="CCA2 (27.82%)",y="CCA3 (23.88%)",colour="Saison",shape = "Saison",
       title = "CCA Cluster 4 - 10 taxons maj par saison")+
  #scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))+
  theme_minimal()

species_df <- as.data.frame(species_coords)
species_df$name <- rownames(species_df)

ggplot(observations_df, aes(x = CCA2, y = CCA3)) +
  geom_point(aes(colour=as.factor(season),shape=as.factor(season)),size=1) +
  geom_text(aes(x=CCA2,y=CCA3,label=name),size=3,data=species_df)+
  #labs(x = "Dimension 2 (16.80%)", y = "Dimension 3 (12.79%)",colour="Cluster") +
  geom_segment(aes(x = 0, y = 0, xend = CCA2, yend = CCA3), color = "black", size = 0.5,data=species_df,linetype="dashed")+
  geom_segment(aes(x = 0, y = 0, xend = CCA2, yend = CCA3),
               arrow = arrow(length = unit(0.08, "inches")), color = "red", size = 0.4,data=envcoord)+
  geom_text(aes(x=CCA2,y=CCA3,label=Var),size=3,data=envcoord,color = "red")+
  scale_x_continuous(limits = c(-1,1))+
  scale_y_continuous(limits = c(-2,2))+
  labs(x="CCA2 (27.82%)",y="CCA3 (23.88%)",colour="Cluster",shape = "Saison")+
  #scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))+
  theme_minimal()


cca_ts <- summarise(group_by(observations_df, Month,Year), CCA1=mean(CCA1,na.rm=T),
                    CCA2=mean(CCA2,na.rm=T),CCA3=mean(CCA3,na.rm=T))


cca_ts$Date <- as.Date(paste(cca_ts$Year, cca_ts$Month, "01", sep = "-"), format = "%Y-%m-%d")



a <- ggplot(cca_ts)+
  geom_line(aes(x=Date,y=CCA1),size=2,col="#00A9FF")+
  geom_point(aes(x=Date,y=CCA1),size=2.5,col="#00A9FF")+
  #scale_colour_manual(values=cluster_col,guide = "none")+
  #facet_wrap(~cluster,ncol=1)+
  scale_x_date(breaks = seq( min(cca_ts$Date), max(cca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(cca_ts$Date), max(cca_ts$Date)))+
  #scale_y_continuous(breaks = seq(-2.5,1.5, by = 0.5),limits = c(-2.5,1.5))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(y= "CCA1 (37.33)",title = "Composition de la communauté au cours du temps")

b<-  ggplot(cca_ts)+
  geom_line(aes(x=Date,y=CCA2),size=2,col="#00A9FF")+
  geom_point(aes(x=Date,y=CCA2),size=2.5,col="#00A9FF")+
  #scale_colour_manual(values=cluster_col,guide = "none")+
  #facet_wrap(~cluster,ncol=1)+
  scale_x_date(breaks = seq( min(cca_ts$Date), max(cca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(cca_ts$Date), max(cca_ts$Date)))+
  #scale_y_continuous(breaks = seq(-2.5,1.5, by = 0.5),limits = c(-2.5,1.5))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(y= "CCA2 (27.82%)",title = "Composition de la communauté au cours du temps")

c<-  ggplot(cca_ts)+
  geom_line(aes(x=Date,y=CCA3),size=2,col="#00A9FF")+
  geom_point(aes(x=Date,y=CCA3),size=2.5,col="#00A9FF")+
  #scale_colour_manual(values=cluster_col,guide = "none")+
  #facet_wrap(~cluster,ncol=1)+
  scale_x_date(breaks = seq( min(cca_ts$Date), max(cca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(cca_ts$Date), max(cca_ts$Date)))+
  #scale_y_continuous(breaks = seq(-2.5,1.5, by = 0.5),limits = c(-2.5,1.5))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(y= "CCA3 (23.88%)",title = "Composition de la communauté au cours du temps")
a/b/c



## CA 1 ####
data1 <- filter(data,cluster == 1)
phyto <- data1[,c(24:247)]
phyto[is.na(phyto)] <- 0
phyto_ok <- phyto[rowSums(phyto != 0) > 0,]

CA_phyto <- CA(phyto)

phyto_coord <- as.data.frame(CA_phyto[["col"]][["coord"]])
phyto_coord$name <- rownames(phyto_coord)

phyto_coord <- filter(phyto_coord,name %in% c(# Automne
  "Asterionellopsis",     "Chaetoceros"  ,    "Cymatosiraceae",   "Dinophyceae.x"  ,  "Gymnodiniales",   
  "Leptocylindrus" ,  "Pseudo-nitzschia" ,"Skeletonema" ,     "Synedra" ,
  # Ete
  "Asterionellopsis" ,    "Chaetoceros"    ,  "Cylindrotheca" ,   "Dinophyceae.x"  ,  "Gymnodiniaceae" , 
  "Gymnodiniales" ,   "Leptocylindrus" ,  "Paralia"     ,     "Pseudo-nitzschia" ,"Skeletonema" ,
  # Printemps
  "Asterionellopsis",    "Bacteriastrum" ,   "Brockmanniella" ,  "Chaetoceros"  ,    "Gymnodiniales"   ,
  "Lauderia"    ,     "Leptocylindrus" ,  "Paralia"     ,     "Pseudo-nitzschia", "Skeletonema",
  # Hiver
  "Asterionellopsis" ,   "Chaetoceros"      ,"Gymnodiniales",    "Lauderia"  ,       "Leptocylindrus" , 
  "Melosira" ,        "Oblea"   ,         "Paralia"   ,       "Pseudo-nitzschia" ,"Skeletonema"))

info <- data1[, c("Code_point_Libelle","Date","cluster","season","Month","Year")]
info <- info[rowSums(phyto != 0) > 0,]

obs_coord <- as.data.frame(CA_phyto[["row"]][["coord"]])
obs_coord$cluster <- as.character(info$cluster)
obs_coord$season <- info$season
obs_coord$Month <- info$Month
obs_coord$Year <- info$Year

obs_coord_mean <- summarise(group_by(obs_coord, cluster,season), CA1=mean(`Dim 1`,na.rm=T),
                            CA2=mean(`Dim 2`,na.rm=T),CA3=mean(`Dim 3`,na.rm=T))

ggplot(obs_coord, aes(x = `Dim 1`, y = `Dim 2`)) +
  geom_point(aes(colour=season,shape=season),size=2) +
  geom_text(aes(x = `Dim 1`, y = `Dim 2`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA1 (6.24%)", y = "CA2 (5.46%)",colour="season",title="Cluster 1 - 10 les plus abondants par saison") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 1`, yend = `Dim 2`), color = "black", size = 0.5,data=phyto_coord)+
  #stat_ellipse(aes(x= CCA1,y=CCA2,color = as.character(cluster)), level = 0.95)+
  theme_minimal()+
  #scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))

ggplot(obs_coord, aes(x = `Dim 2`, y = `Dim 3`)) +
  geom_point(aes(colour=season,shape=season),size=2) +
  geom_text(aes(x = `Dim 2`, y = `Dim 3`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA2 (6.24%)", y = "CA3 (5.33%)",colour="season") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 2`, yend = `Dim 3`), color = "black", size = 0.5,data=phyto_coord)+
  #stat_ellipse(aes(x= CCA1,y=CCA2,color = as.character(cluster)), level = 0.95)+
  #scale_colour_manual(values=cluster_col)+
  theme_minimal()+
  scale_shape_manual(values=c(15,19,17,18))

ggplot(obs_coord_mean, aes(x = CA1, y = CA2)) +
  geom_text(aes(x = `Dim 1`, y = `Dim 2`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA1 (6.24%)", y = "CA2 (5.46%)",colour="Cluster") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 1`, yend = `Dim 2`), color = "black", size = 0.5,data=phyto_coord)+
  #stat_ellipse(aes(x= CCA1,y=CCA2,color = as.character(cluster)), level = 0.95)+
  geom_point(aes(colour=season,shape=season),size=2) +
  theme_minimal()+
  scale_shape_manual(values=c(15,19,17,18))

ggplot(obs_coord_mean, aes(x = CA2, y = CA3)) +
  geom_text(aes(x = `Dim 2`, y = `Dim 3`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA2 (5.46%)", y = "CA3 (5.33%)",colour="season") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 2`, yend = `Dim 3`), color = "black", size = 0.5,data=phyto_coord)+
  #stat_ellipse(aes(x= CCA1,y=CCA2,color = as.character(cluster)), level = 0.95)+
  geom_point(aes(colour=season,shape=season),size=2) +
  theme_minimal()+
  scale_shape_manual(values=c(15,19,17,18))


ca_ts <- summarise(group_by(obs_coord, cluster,Month,Year), CA1=mean(`Dim 1`,na.rm=T),
                   CA2=mean(`Dim 2`,na.rm=T),CA3=mean(`Dim 3`,na.rm=T))

ca_ts$Date <- as.Date(paste(ca_ts$Year, ca_ts$Month, "01", sep = "-"), format = "%Y-%m-%d")
a <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA1),size=2,col="#F8766D")+
  geom_point(aes(x=Date,y=CA1),size=2.5,col="#F8766D")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA1 (6.24%)",title = "Composition de la communauté au cours du temps")
b <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA2),size=2,col="#F8766D")+
  geom_point(aes(x=Date,y=CA2),size=2.5,col="#F8766D")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA2 (5.46%)",title = "Composition de la communauté au cours du temps")
c <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA3),size=2,col="#F8766D")+
  geom_point(aes(x=Date,y=CA3),size=2.5,col="#F8766D")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA3 (5.33%)",title = "Composition de la communauté au cours du temps")
a/b/c

## CA 2 ####
data2 <- filter(data,cluster == 2)
phyto <- data2[,c(24:247)]
phyto[is.na(phyto)] <- 0
phyto_ok <- phyto[rowSums(phyto != 0) > 0,]

CA_phyto <- CA(phyto)

phyto_coord <- as.data.frame(CA_phyto[["col"]][["coord"]])
phyto_coord$name <- rownames(phyto_coord)

phyto_coord <- filter(phyto_coord,name %in% c(# Automne
  "Akashiwo"   ,      "Asterionellopsis"   , "Azadinium"       , "Brockmanniella",   "Chaetoceros"  ,   
  "Delphineis"    ,   "Lepidodinium"  ,   "Leptocylindrus"  , "Paralia"      ,    "Skeletonema" ,
  # Ete
  "Asterionellopsis" ,    "Brockmanniella",   "Chaetoceros"  ,    "Delphineis"   ,    "Guinardia"    ,   "Lepidodinium"  ,   "Leptocylindrus"  , "Pseudo-nitzschia" ,"Rhizosolenia"  ,   "Skeletonema"  ,
  # Printemps
  "Asterionellopsis" ,"Chaetoceros" ,     "Fragilariaceae"  , "Lauderia"      ,   "Leptocylindrus" , "Melosiraceae",     "Pseudo-nitzschia" ,"Rhizosolenia"     ,"Skeletonema"  ,    "Thalassiosira" ,
  # HiveR
  "Asterionellopsis" ,"Attheya"        ,   "Brockmanniella"  , "Delphineis"    ,   "Paralia" ,        
  "Plagiogramma"   ,  "Proboscia"   ,     "Skeletonema"  ,    "Thalassiosira"  ,  "Triceratium") )

info <- data2[, c("Code_point_Libelle","Date","cluster","season","Month","Year")]
info <- info[rowSums(phyto != 0) > 0,]

obs_coord <- as.data.frame(CA_phyto[["row"]][["coord"]])
obs_coord$cluster <- as.character(info$cluster)
obs_coord$season <- info$season
obs_coord$Month <- info$Month
obs_coord$Year <- info$Year

obs_coord_mean <- summarise(group_by(obs_coord, cluster,season), CA1=mean(`Dim 1`,na.rm=T),
                            CA2=mean(`Dim 2`,na.rm=T),CA3=mean(`Dim 3`,na.rm=T))

ggplot(obs_coord, aes(x = `Dim 1`, y = `Dim 2`)) +
  geom_point(aes(colour=season,shape=season),size=2) +
  geom_text(aes(x = `Dim 1`, y = `Dim 2`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA1 (6.32%)", y = "CA2 (6.22%)",colour="season",title="Cluster 2 - 10 les plus abondants par saison") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 1`, yend = `Dim 2`), color = "black", size = 0.5,data=phyto_coord)+
  #stat_ellipse(aes(x= CCA1,y=CCA2,color = as.character(cluster)), level = 0.95)+
  theme_minimal()+
  #scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))

ggplot(obs_coord, aes(x = `Dim 2`, y = `Dim 3`)) +
  geom_point(aes(colour=season,shape=season),size=2) +
  geom_text(aes(x = `Dim 2`, y = `Dim 3`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA2 (6.22%)", y = "CA3 (5.78%)",colour="season") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 2`, yend = `Dim 3`), color = "black", size = 0.5,data=phyto_coord)+
  #stat_ellipse(aes(x= CCA1,y=CCA2,color = as.character(cluster)), level = 0.95)+
  #scale_colour_manual(values=cluster_col)+
  theme_minimal()+
  scale_shape_manual(values=c(15,19,17,18))

ggplot(obs_coord_mean, aes(x = CA1, y = CA2)) +
  geom_text(aes(x = `Dim 1`, y = `Dim 2`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA1 (6.32%)", y = "CA2 (6.22%)",colour="season") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 1`, yend = `Dim 2`), color = "black", size = 0.5,data=phyto_coord)+
  #stat_ellipse(aes(x= CCA1,y=CCA2,color = as.character(cluster)), level = 0.95)+
  geom_point(aes(colour=season,shape=season),size=2) +
  theme_minimal()+
  scale_shape_manual(values=c(15,19,17,18))

ggplot(obs_coord_mean, aes(x = CA2, y = CA3)) +
  geom_text(aes(x = `Dim 2`, y = `Dim 3`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA2 (6.22%)", y = "CA3 (5.78%)",colour="season") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 2`, yend = `Dim 3`), color = "black", size = 0.5,data=phyto_coord)+
  #stat_ellipse(aes(x= CCA1,y=CCA2,color = as.character(cluster)), level = 0.95)+
  geom_point(aes(colour=season,shape=season),size=2) +
  theme_minimal()+
  scale_shape_manual(values=c(15,19,17,18))


ca_ts <- summarise(group_by(obs_coord, cluster,Month,Year), CA1=mean(`Dim 1`,na.rm=T),
                   CA2=mean(`Dim 2`,na.rm=T),CA3=mean(`Dim 3`,na.rm=T))

ca_ts$Date <- as.Date(paste(ca_ts$Year, ca_ts$Month, "01", sep = "-"), format = "%Y-%m-%d")
a <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA1),size=2,col="#CD9600")+
  geom_point(aes(x=Date,y=CA1),size=2.5,col="#CD9600")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA1 (6.32%)",title = "Composition de la communauté au cours du temps")
b <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA2),size=2,col="#CD9600")+
  geom_point(aes(x=Date,y=CA2),size=2.5,col="#CD9600")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA2 (6.22%)",title = "Composition de la communauté au cours du temps")
c <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA3),size=2,col="#CD9600")+
  geom_point(aes(x=Date,y=CA3),size=2.5,col="#CD9600")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA3 (5.78%)",title = "Composition de la communauté au cours du temps")
a/b/c

## CA 3 ####
data3 <- filter(data,cluster == 3)
phyto <- data3[,c(24:247)]
phyto[is.na(phyto)] <- 0
phyto_ok <- phyto[rowSums(phyto != 0) > 0,]

CA_phyto <- CA(phyto)

phyto_coord <- as.data.frame(CA_phyto[["col"]][["coord"]])
phyto_coord$name <- rownames(phyto_coord)

phyto_coord <- filter(phyto_coord,name %in% c(# Automne
  "Asterionellopsis", "Asteroplanus", "Chaetoceros"     , "Chaetocerotaceae" ,"Cylindrotheca"  , "Guinardia"       , "Leptocylindrus",   "Lithodesmium"  ,   "Paralia"   ,       "Skeletonema" ,    
  # Ete
  "Asterionellopsis" ,  "Chaetoceros"  ,    "Chaetocerotaceae", "Dactyliosolen" ,   "Guinardia"   ,    "Lepidodinium",     "Leptocylindrus"  , "Melosira"  ,       "Pseudo-nitzschia" ,"Skeletonema" ,    
  # Printemps
  "Asterionellopsis", "Brockmanniella"  , "Chaetoceros" ,     "Chaetocerotaceae" ,"Dactyliosolen",   
  "Leptocylindrus" ,  "Peridiniaceae"   , "Plagiogramma" ,    "Skeletonema"  ,    "Thalassiosira" ,  
#Hiver
  "Asterionellopsis",  "Azadinium"      ,  "Brockmanniella" ,  "Chaetoceros"   ,   "Delphineis"  ,    "Paralia" ,         "Peridiniaceae" ,   "Plagiogramma"   ,  "Skeletonema"    ,  "Thalassiosira" ) )

info <- data3[, c("Code_point_Libelle","Date","cluster","season","Month","Year")]
info <- info[rowSums(phyto != 0) > 0,]

obs_coord <- as.data.frame(CA_phyto[["row"]][["coord"]])
obs_coord$cluster <- as.character(info$cluster)
obs_coord$season <- info$season
obs_coord$Month <- info$Month
obs_coord$Year <- info$Year

obs_coord_mean <- summarise(group_by(obs_coord, cluster,season), CA1=mean(`Dim 1`,na.rm=T),
                            CA2=mean(`Dim 2`,na.rm=T),CA3=mean(`Dim 3`,na.rm=T))

ggplot(obs_coord, aes(x = `Dim 1`, y = `Dim 2`)) +
  geom_point(aes(colour=season,shape=season),size=2) +
  geom_text(aes(x = `Dim 1`, y = `Dim 2`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA1 (5.81%)", y = "CA2 (5.34%)",colour="season",title="Cluster 3 - 10 les plus abondants par saison") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 1`, yend = `Dim 2`), color = "black", size = 0.5,data=phyto_coord)+
  #stat_ellipse(aes(x= CCA1,y=CCA2,color = as.character(cluster)), level = 0.95)+
  theme_minimal()+
  #scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))

ggplot(obs_coord, aes(x = `Dim 2`, y = `Dim 3`)) +
  geom_point(aes(colour=season,shape=season),size=2) +
  geom_text(aes(x = `Dim 2`, y = `Dim 3`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA2 (5.34%)", y = "CA3 (5.10%)",colour="season") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 2`, yend = `Dim 3`), color = "black", size = 0.5,data=phyto_coord)+
  #stat_ellipse(aes(x= CCA1,y=CCA2,color = as.character(cluster)), level = 0.95)+
  #scale_colour_manual(values=cluster_col)+
  theme_minimal()+
  scale_shape_manual(values=c(15,19,17,18))


ggplot(obs_coord, aes(x = `Dim 1`, y = `Dim 2`)) +
  geom_point(aes(colour=season,shape=season),size=2) +
  geom_text(aes(x = `Dim 1`, y = `Dim 2`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA1 (5.81%)", y = "CA2 (5.34%)",colour="season",title="Cluster 3 - 10 les plus abondants par saison") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 1`, yend = `Dim 2`), color = "black", size = 0.5,data=phyto_coord)+
  #stat_ellipse(aes(x= CCA1,y=CCA2,color = as.character(cluster)), level = 0.95)+
  theme_minimal()+
  scale_x_continuous(limits = c(-0.1,0.1))
  #scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))

ggplot(obs_coord, aes(x = `Dim 2`, y = `Dim 3`)) +
  geom_point(aes(colour=season,shape=season),size=2) +
  geom_text(aes(x = `Dim 2`, y = `Dim 3`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA2 (5.34%)", y = "CA3 (5.10%)",colour="season") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 2`, yend = `Dim 3`), color = "black", size = 0.5,data=phyto_coord)+
  #stat_ellipse(aes(x= CCA1,y=CCA2,color = as.character(cluster)), level = 0.95)+
  #scale_colour_manual(values=cluster_col)+
  theme_minimal()+
  scale_y_continuous(limits = c(-0.5,0.5))
  scale_shape_manual(values=c(15,19,17,18))



ggplot(obs_coord_mean, aes(x = CA1, y = CA2)) +
  geom_text(aes(x = `Dim 1`, y = `Dim 2`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA1 (5.816%)", y = "CA2 (5.338%)",colour="season") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 1`, yend = `Dim 2`), color = "black", size = 0.5,data=phyto_coord)+
  #stat_ellipse(aes(x= CCA1,y=CCA2,color = as.character(cluster)), level = 0.95)+
  geom_point(aes(colour=season,shape=season),size=2) +
  theme_minimal()+
  scale_x_continuous(limits = c(-0.1,0.1))+
  scale_shape_manual(values=c(15,19,17,18))

ggplot(obs_coord_mean, aes(x = CA2, y = CA3)) +
  geom_text(aes(x = `Dim 2`, y = `Dim 3`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA2 (5.34%)", y = "CA3 (5.10%)",colour="season") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 2`, yend = `Dim 3`), color = "black", size = 0.5,data=phyto_coord)+
  #stat_ellipse(aes(x= CCA1,y=CCA2,color = as.character(cluster)), level = 0.95)+
  geom_point(aes(colour=season,shape=season),size=2) +
  scale_y_continuous(limits = c(-0.5,0.5))+
  theme_minimal()+
  scale_shape_manual(values=c(15,19,17,18))


ca_ts <- summarise(group_by(obs_coord, cluster,Month,Year), CA1=mean(`Dim 1`,na.rm=T),
                   CA2=mean(`Dim 2`,na.rm=T),CA3=mean(`Dim 3`,na.rm=T))

ca_ts$Date <- as.Date(paste(ca_ts$Year, ca_ts$Month, "01", sep = "-"), format = "%Y-%m-%d")
a <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA1),size=2,col="#00BE67")+
  geom_point(aes(x=Date,y=CA1),size=2.5,col="#00BE67")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA1 (5.81%)",title = "Composition de la communauté au cours du temps")
b <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA2),size=2,col="#00BE67")+
  geom_point(aes(x=Date,y=CA2),size=2.5,col="#00BE67")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA2 (5.34%)",title = "Composition de la communauté au cours du temps")
c <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA3),size=2,col="#00BE67")+
  geom_point(aes(x=Date,y=CA3),size=2.5,col="#00BE67")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA3 (5.10%)",title = "Composition de la communauté au cours du temps")
a/b/c


## CA 4 ####
data4 <- filter(data,cluster == 4)
phyto <- data4[,c(24:247)]
phyto[is.na(phyto)] <- 0
phyto_ok <- phyto[rowSums(phyto != 0) > 0,]

CA_phyto <- CA(phyto)

phyto_coord <- as.data.frame(CA_phyto[["col"]][["coord"]])
phyto_coord$name <- rownames(phyto_coord)

phyto_coord <- filter(phyto_coord,name %in% c(# Automne
  "Bacteriastrum" , "Chaetoceros"  ,  "Cymatosiraceae" ,"Fragilariaceae", "Guinardia",      "Leptocylindrus",
  "Paralia" ,       "Skeletonema" ,   "Thalassiosira" ,
  # Ete
  "Amphora" , "Chaetoceros" ,   "Cylindrotheca" , "Gymnodiniaceae", "Gymnodinium"  ,  "Lepidodinium",  
  "Leptocylindrus", "Prorocentrum" ,  "Scrippsiella"  , "Thalassiosira", 
  # Printemps
  "Asterionella" ,       "Asterionellopsis" , "Chaetoceros"   ,      "Cylindrotheca" ,     
  "Cymatosiraceae"   ,   "Leptocylindrus"  ,    "Paralia"    ,         "Skeletonema"     ,    "Thalassionemataceae",
  "Thalassiosira" ,     
  # Hiver
  "Asterionellopsis" , "Bacillaria"      , "Cymatosiraceae" ,  "Fragilariaceae" ,  "Melosiraceae"  ,  
  "Paralia"    ,      "Pennées"   ,       "Plagiogrammopsis" ,"Skeletonema"     , "Thalassiosira") )

info <- data4[, c("Code_point_Libelle","Date","cluster","season","Month","Year")]
info <- info[rowSums(phyto != 0) > 0,]

obs_coord <- as.data.frame(CA_phyto[["row"]][["coord"]])
obs_coord$cluster <- as.character(info$cluster)
obs_coord$season <- info$season
obs_coord$Month <- info$Month
obs_coord$Year <- info$Year

obs_coord_mean <- summarise(group_by(obs_coord, cluster,season), CA1=mean(`Dim 1`,na.rm=T),
                            CA2=mean(`Dim 2`,na.rm=T),CA3=mean(`Dim 3`,na.rm=T))

ggplot(obs_coord, aes(x = `Dim 1`, y = `Dim 2`)) +
  geom_point(aes(colour=season,shape=season),size=2) +
  geom_text(aes(x = `Dim 1`, y = `Dim 2`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA1 (9.04%)", y = "CA2 (8.66%)",colour="season",title="Cluster 4 - 10 les plus abondants par saison") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 1`, yend = `Dim 2`), color = "black", size = 0.5,data=phyto_coord)+
  #stat_ellipse(aes(x= CCA1,y=CCA2,color = as.character(cluster)), level = 0.95)+
  theme_minimal()+
  #scale_colour_manual(values=cluster_col)+
  scale_shape_manual(values=c(15,19,17,18))

ggplot(obs_coord, aes(x = `Dim 2`, y = `Dim 3`)) +
  geom_point(aes(colour=season,shape=season),size=2) +
  geom_text(aes(x = `Dim 2`, y = `Dim 3`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA2 (8.66%)", y = "CA3 (8.04%)",colour="season") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 2`, yend = `Dim 3`), color = "black", size = 0.5,data=phyto_coord)+
  #stat_ellipse(aes(x= CCA1,y=CCA2,color = as.character(cluster)), level = 0.95)+
  #scale_colour_manual(values=cluster_col)+
  theme_minimal()+
  scale_shape_manual(values=c(15,19,17,18))


ggplot(obs_coord_mean, aes(x = CA1, y = CA2)) +
  geom_text(aes(x = `Dim 1`, y = `Dim 2`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA1 (9.04%)", y = "CA2 (8.66%)",colour="season") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 1`, yend = `Dim 2`), color = "black", size = 0.5,data=phyto_coord)+
  #stat_ellipse(aes(x= CCA1,y=CCA2,color = as.character(cluster)), level = 0.95)+
  geom_point(aes(colour=season,shape=season),size=2) +
  theme_minimal()+
  scale_shape_manual(values=c(15,19,17,18))

ggplot(obs_coord_mean, aes(x = CA2, y = CA3)) +
  geom_text(aes(x = `Dim 2`, y = `Dim 3`,label=name),size=3,data=phyto_coord)+
  labs(x = "CA2 (8.66%)", y = "CA3 (8.04%)",colour="season") +
  geom_segment(aes(x = 0, y = 0, xend = `Dim 2`, yend = `Dim 3`), color = "black", size = 0.5,data=phyto_coord)+
  #stat_ellipse(aes(x= CCA1,y=CCA2,color = as.character(cluster)), level = 0.95)+
  geom_point(aes(colour=season,shape=season),size=2) +
  theme_minimal()+
  scale_shape_manual(values=c(15,19,17,18))


ca_ts <- summarise(group_by(obs_coord, cluster,Month,Year), CA1=mean(`Dim 1`,na.rm=T),
                   CA2=mean(`Dim 2`,na.rm=T),CA3=mean(`Dim 3`,na.rm=T))

ca_ts$Date <- as.Date(paste(ca_ts$Year, ca_ts$Month, "01", sep = "-"), format = "%Y-%m-%d")
a <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA1),size=2,col="#00A9FF")+
  geom_point(aes(x=Date,y=CA1),size=2.5,col="#00A9FF")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA1 (9.04%)",title = "Composition de la communauté au cours du temps")
b <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA2),size=2,col="#00A9FF")+
  geom_point(aes(x=Date,y=CA2),size=2.5,col="#00A9FF")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA2 (8.66%)",title = "Composition de la communauté au cours du temps")
c <- ggplot(ca_ts)+
  geom_line(aes(x=Date,y=CA3),size=2,col="#00A9FF")+
  geom_point(aes(x=Date,y=CA3),size=2.5,col="#00A9FF")+
  scale_x_date(breaks = seq( min(ca_ts$Date), max(ca_ts$Date),by=100),date_labels = "%Y-%m",limits = c(min(ca_ts$Date), max(ca_ts$Date)))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 5))+
  labs(y= "CA3 (8.04%)",title = "Composition de la communauté au cours du temps")
a/b/c













