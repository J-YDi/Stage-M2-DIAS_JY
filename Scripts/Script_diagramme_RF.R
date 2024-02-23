data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = ""), trim_ws = TRUE)


data <- data %>%
  arrange(Code_point_Libelle, Date)
# Diagramme rang frequence pendant un bloom 
# Tout cluster

bloomeur  <- levels(as.factor(data$Bloom))[1]
# Tout
data_inbloom <- filter(data, Bloom == bloomeur)
data_inbloom <- data_inbloom |> 
  select(Code_point_Libelle,Date,Actinoptychus:Auricula) |>
  pivot_longer(Actinoptychus:Auricula,names_to = "Genre",values_to = "Comptage")

data_rf <- summarise(group_by(data_inbloom, Genre), Comptage=sum(Comptage,na.rm=T))

data_rf <- data_rf |>
  arrange(desc(Comptage))

#ordre_genre <- data_rf$Genre
#
## Convertissez la colonne "Station" en facteur avec l'ordre spécifié
#data_rf$Genre <- factor(data_rf$Genre, levels = ordre_genre)

data_rf$Rang <- seq(1:nrow(data_rf))
data_rf$Frequence <- data_rf$Comptage/sum(data_rf$Comptage)

ggplot(data_rf)+
  geom_point(aes(x=Rang,y=Frequence),size=3,col="grey")+
  geom_line(aes(x=Rang,y=Frequence),size=1)+
  scale_x_log10()+
  scale_y_log10()+
  geom_rug(aes(x = Rang, y = Frequence),outside = F, length = unit(0.02,"npc"))+
  labs(title = paste("Diagramme rang-frequence pendant bloom de",bloomeur),
       subtitle = "Tout",
    x = "Rang", y = "Frequence")
nom <- paste0("Bloom_",bloomeur,"_tout.png")
ggsave(nom, path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Rang_frequence",dpi = 600, width = 200, height = 180, units = 'mm')

data_inbloom <- filter(data, Bloom == bloomeur)

data_inbloom <- data_inbloom |> 
  filter(Monospecifique == "O") |>
  select(Code_point_Libelle,Date,Actinoptychus:Auricula) |>
  pivot_longer(Actinoptychus:Auricula,names_to = "Genre",values_to = "Comptage")

data_rf <- summarise(group_by(data_inbloom, Genre), Comptage=sum(Comptage,na.rm=T))

data_rf <- data_rf |>
  arrange(desc(Comptage))

#ordre_genre <- data_rf$Genre
#
## Convertissez la colonne "Station" en facteur avec l'ordre spécifié
#data_rf$Genre <- factor(data_rf$Genre, levels = ordre_genre)

data_rf$Rang <- seq(1:nrow(data_rf))
data_rf$Frequence <- data_rf$Comptage/sum(data_rf$Comptage)

ggplot(data_rf)+
  geom_point(aes(x=Rang,y=Frequence),size=3,col="grey")+
  geom_line(aes(x=Rang,y=Frequence),size=1)+
  scale_x_log10()+
  scale_y_log10()+
  geom_rug(aes(x = Rang, y = Frequence),outside = F, length = unit(0.02,"npc"))+
  labs(title = paste("Diagramme rang-frequence pendant bloom de",bloomeur),
       subtitle = "Monospecifique",
       x = "Rang", y = "Frequence")
nom <- paste0("Bloom_",bloomeur,"_mono.png")
ggsave(nom, path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Rang_frequence",dpi = 600, width = 200, height = 180, units = 'mm')


data_inbloom <- filter(data, Bloom == bloomeur)
data_inbloom <- data_inbloom |> 
  filter(Monospecifique == "N") |>
  select(Code_point_Libelle,Date,Actinoptychus:Auricula) |>
  pivot_longer(Actinoptychus:Auricula,names_to = "Genre",values_to = "Comptage")

data_rf <- summarise(group_by(data_inbloom, Genre), Comptage=sum(Comptage,na.rm=T))

data_rf <- data_rf |>
  arrange(desc(Comptage))

#ordre_genre <- data_rf$Genre
#
## Convertissez la colonne "Station" en facteur avec l'ordre spécifié
#data_rf$Genre <- factor(data_rf$Genre, levels = ordre_genre)

data_rf$Rang <- seq(1:nrow(data_rf))
data_rf$Frequence <- data_rf$Comptage/sum(data_rf$Comptage)

ggplot(data_rf)+
  geom_point(aes(x=Rang,y=Frequence),size=3,col="grey")+
  geom_line(aes(x=Rang,y=Frequence),size=1)+
  scale_x_log10()+
  scale_y_log10()+
  geom_rug(aes(x = Rang, y = Frequence),outside = F, length = unit(0.02,"npc"))+
  labs(title = paste("Diagramme rang-frequence pendant bloom de",bloomeur),
       subtitle = "Non-monospecifique",
       x = "Rang", y = "Frequence")
nom <- paste0("Bloom_",bloomeur,"_nonmono.png")
ggsave(nom, path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Rang_frequence",dpi = 600, width = 200, height = 180, units = 'mm')

# Cluster 1 

data <- filter(data, cluster == 1)

bloomeur  <- levels(as.factor(data$Bloom))[16]

# Tout
data_inbloom <- filter(data, Bloom == bloomeur)
data_inbloom <- data_inbloom |> 
  select(Code_point_Libelle,Date,Actinoptychus:Auricula) |>
  pivot_longer(Actinoptychus:Auricula,names_to = "Genre",values_to = "Comptage")

data_rf <- summarise(group_by(data_inbloom, Genre), Comptage=sum(Comptage,na.rm=T))

data_rf <- data_rf |>
  arrange(desc(Comptage))

#ordre_genre <- data_rf$Genre
#
## Convertissez la colonne "Station" en facteur avec l'ordre spécifié
#data_rf$Genre <- factor(data_rf$Genre, levels = ordre_genre)

data_rf$Rang <- seq(1:nrow(data_rf))
data_rf$Frequence <- data_rf$Comptage/sum(data_rf$Comptage)

ggplot(data_rf)+
  geom_point(aes(x=Rang,y=Frequence),size=3,col="grey")+
  geom_line(aes(x=Rang,y=Frequence),size=1)+
  scale_x_log10()+
  scale_y_log10()+
  geom_rug(aes(x = Rang, y = Frequence),outside = F, length = unit(0.02,"npc"))+
  labs(title = paste("Diagramme rang-frequence pendant bloom de",bloomeur),
       subtitle = "Tout - cluster 1",
       x = "Rang", y = "Frequence")
nom <- paste0("Bloom_",bloomeur,"_tout_cluster1.png")
ggsave(nom, path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Rang_frequence",dpi = 600, width = 200, height = 180, units = 'mm')

data_inbloom <- filter(data, Bloom == bloomeur)

data_inbloom <- data_inbloom |> 
  filter(Monospecifique == "O") |>
  select(Code_point_Libelle,Date,Actinoptychus:Auricula) |>
  pivot_longer(Actinoptychus:Auricula,names_to = "Genre",values_to = "Comptage")

data_rf <- summarise(group_by(data_inbloom, Genre), Comptage=sum(Comptage,na.rm=T))

data_rf <- data_rf |>
  arrange(desc(Comptage))

#ordre_genre <- data_rf$Genre
#
## Convertissez la colonne "Station" en facteur avec l'ordre spécifié
#data_rf$Genre <- factor(data_rf$Genre, levels = ordre_genre)

data_rf$Rang <- seq(1:nrow(data_rf))
data_rf$Frequence <- data_rf$Comptage/sum(data_rf$Comptage)

ggplot(data_rf)+
  geom_point(aes(x=Rang,y=Frequence),size=3,col="grey")+
  geom_line(aes(x=Rang,y=Frequence),size=1)+
  scale_x_log10()+
  scale_y_log10()+
  geom_rug(aes(x = Rang, y = Frequence),outside = F, length = unit(0.02,"npc"))+
  labs(title = paste("Diagramme rang-frequence pendant bloom de",bloomeur),
       subtitle = "Monospecifique - cluster 1",
       x = "Rang", y = "Frequence")
nom <- paste0("Bloom_",bloomeur,"_mono_cluster1.png")
ggsave(nom, path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Rang_frequence",dpi = 600, width = 200, height = 180, units = 'mm')


data_inbloom <- filter(data, Bloom == bloomeur)
data_inbloom <- data_inbloom |> 
  filter(Monospecifique == "N") |>
  select(Code_point_Libelle,Date,Actinoptychus:Auricula) |>
  pivot_longer(Actinoptychus:Auricula,names_to = "Genre",values_to = "Comptage")

data_rf <- summarise(group_by(data_inbloom, Genre), Comptage=sum(Comptage,na.rm=T))

data_rf <- data_rf |>
  arrange(desc(Comptage))

#ordre_genre <- data_rf$Genre
#
## Convertissez la colonne "Station" en facteur avec l'ordre spécifié
#data_rf$Genre <- factor(data_rf$Genre, levels = ordre_genre)

data_rf$Rang <- seq(1:nrow(data_rf))
data_rf$Frequence <- data_rf$Comptage/sum(data_rf$Comptage)

ggplot(data_rf)+
  geom_point(aes(x=Rang,y=Frequence),size=3,col="grey")+
  geom_line(aes(x=Rang,y=Frequence),size=1)+
  scale_x_log10()+
  scale_y_log10()+
  geom_rug(aes(x = Rang, y = Frequence),outside = F, length = unit(0.02,"npc"))+
  labs(title = paste("Diagramme rang-frequence pendant bloom de",bloomeur),
       subtitle = "Non-monospecifique - cluster 1",
       x = "Rang", y = "Frequence")
nom <- paste0("Bloom_",bloomeur,"_nonmono_cluster1.png")
ggsave(nom, path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Rang_frequence",dpi = 600, width = 200, height = 180, units = 'mm')



# Cluster 2
data <- filter(data, cluster == 2)

bloomeur  <- levels(as.factor(data$Bloom))[11]

  # Tout
  data_inbloom <- filter(data, Bloom == bloomeur)
  data_inbloom <- data_inbloom |> 
    select(Code_point_Libelle,Date,Actinoptychus:Auricula) |>
    pivot_longer(Actinoptychus:Auricula,names_to = "Genre",values_to = "Comptage")
  
  data_rf <- summarise(group_by(data_inbloom, Genre), Comptage=sum(Comptage,na.rm=T))
  
  data_rf <- data_rf |>
    arrange(desc(Comptage))
  
  #ordre_genre <- data_rf$Genre
  #
  ## Convertissez la colonne "Station" en facteur avec l'ordre spécifié
  #data_rf$Genre <- factor(data_rf$Genre, levels = ordre_genre)
  
  data_rf$Rang <- seq(1:nrow(data_rf))
  data_rf$Frequence <- data_rf$Comptage/sum(data_rf$Comptage)
  
  ggplot(data_rf)+
    geom_point(aes(x=Rang,y=Frequence),size=3,col="grey")+
    geom_line(aes(x=Rang,y=Frequence),size=1)+
    scale_x_log10()+
    scale_y_log10()+
    geom_rug(aes(x = Rang, y = Frequence),outside = F, length = unit(0.02,"npc"))+
    labs(title = paste("Diagramme rang-frequence pendant bloom de",bloomeur),
         subtitle = "Tout - cluster 2",
         x = "Rang", y = "Frequence")
  nom <- paste0("Bloom_",bloomeur,"_tout_cluster2.png")
  ggsave(nom, path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Rang_frequence",dpi = 600, width = 200, height = 180, units = 'mm')
  
  data_inbloom <- filter(data, Bloom == bloomeur)
  
  data_inbloom <- data_inbloom |> 
    filter(Monospecifique == "O") |>
    select(Code_point_Libelle,Date,Actinoptychus:Auricula) |>
    pivot_longer(Actinoptychus:Auricula,names_to = "Genre",values_to = "Comptage")
  
  data_rf <- summarise(group_by(data_inbloom, Genre), Comptage=sum(Comptage,na.rm=T))
  
  data_rf <- data_rf |>
    arrange(desc(Comptage))
  
  #ordre_genre <- data_rf$Genre
  #
  ## Convertissez la colonne "Station" en facteur avec l'ordre spécifié
  #data_rf$Genre <- factor(data_rf$Genre, levels = ordre_genre)
  
  data_rf$Rang <- seq(1:nrow(data_rf))
  data_rf$Frequence <- data_rf$Comptage/sum(data_rf$Comptage)
  
  ggplot(data_rf)+
    geom_point(aes(x=Rang,y=Frequence),size=3,col="grey")+
    geom_line(aes(x=Rang,y=Frequence),size=1)+
    scale_x_log10()+
    scale_y_log10()+
    geom_rug(aes(x = Rang, y = Frequence),outside = F, length = unit(0.02,"npc"))+
    labs(title = paste("Diagramme rang-frequence pendant bloom de",bloomeur),
         subtitle = "Monospecifique - cluster 2",
         x = "Rang", y = "Frequence")
  nom <- paste0("Bloom_",bloomeur,"_mono_cluster2.png")
  ggsave(nom, path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Rang_frequence",dpi = 600, width = 200, height = 180, units = 'mm')
  
  
  data_inbloom <- filter(data, Bloom == bloomeur)
  data_inbloom <- data_inbloom |> 
    filter(Monospecifique == "N") |>
    select(Code_point_Libelle,Date,Actinoptychus:Auricula) |>
    pivot_longer(Actinoptychus:Auricula,names_to = "Genre",values_to = "Comptage")
  
  data_rf <- summarise(group_by(data_inbloom, Genre), Comptage=sum(Comptage,na.rm=T))
  
  data_rf <- data_rf |>
    arrange(desc(Comptage))
  
  #ordre_genre <- data_rf$Genre
  #
  ## Convertissez la colonne "Station" en facteur avec l'ordre spécifié
  #data_rf$Genre <- factor(data_rf$Genre, levels = ordre_genre)
  
  data_rf$Rang <- seq(1:nrow(data_rf))
  data_rf$Frequence <- data_rf$Comptage/sum(data_rf$Comptage)
  
  ggplot(data_rf)+
    geom_point(aes(x=Rang,y=Frequence),size=3,col="grey")+
    geom_line(aes(x=Rang,y=Frequence),size=1)+
    scale_x_log10()+
    scale_y_log10()+
    geom_rug(aes(x = Rang, y = Frequence),outside = F, length = unit(0.02,"npc"))+
    labs(title = paste("Diagramme rang-frequence pendant bloom de",bloomeur),
         subtitle = "Non-monospecifique - cluster 2",
         x = "Rang", y = "Frequence")
  nom <- paste0("Bloom_",bloomeur,"_nonmono_cluster2.png")
  ggsave(nom, path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Rang_frequence",dpi = 600, width = 200, height = 180, units = 'mm')



  # Cluster 3
  data <- filter(data, cluster == 3)
  
  bloomeur  <- levels(as.factor(data$Bloom))[19]
  
  # Tout
  data_inbloom <- filter(data, Bloom == bloomeur)
  data_inbloom <- data_inbloom |> 
    select(Code_point_Libelle,Date,Actinoptychus:Auricula) |>
    pivot_longer(Actinoptychus:Auricula,names_to = "Genre",values_to = "Comptage")
  
  data_rf <- summarise(group_by(data_inbloom, Genre), Comptage=sum(Comptage,na.rm=T))
  
  data_rf <- data_rf |>
    arrange(desc(Comptage))
  
  #ordre_genre <- data_rf$Genre
  #
  ## Convertissez la colonne "Station" en facteur avec l'ordre spécifié
  #data_rf$Genre <- factor(data_rf$Genre, levels = ordre_genre)
  
  data_rf$Rang <- seq(1:nrow(data_rf))
  data_rf$Frequence <- data_rf$Comptage/sum(data_rf$Comptage)
  
  ggplot(data_rf)+
    geom_point(aes(x=Rang,y=Frequence),size=3,col="grey")+
    geom_line(aes(x=Rang,y=Frequence),size=1)+
    scale_x_log10()+
    scale_y_log10()+
    geom_rug(aes(x = Rang, y = Frequence),outside = F, length = unit(0.02,"npc"))+
    labs(title = paste("Diagramme rang-frequence pendant bloom de",bloomeur),
         subtitle = "Tout - cluster 3",
         x = "Rang", y = "Frequence")
  nom <- paste0("Bloom_",bloomeur,"_tout_cluster3.png")
  ggsave(nom, path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Rang_frequence",dpi = 600, width = 200, height = 180, units = 'mm')
  
  data_inbloom <- filter(data, Bloom == bloomeur)
  
  data_inbloom <- data_inbloom |> 
    filter(Monospecifique == "O") |>
    select(Code_point_Libelle,Date,Actinoptychus:Auricula) |>
    pivot_longer(Actinoptychus:Auricula,names_to = "Genre",values_to = "Comptage")
  
  data_rf <- summarise(group_by(data_inbloom, Genre), Comptage=sum(Comptage,na.rm=T))
  
  data_rf <- data_rf |>
    arrange(desc(Comptage))
  
  #ordre_genre <- data_rf$Genre
  #
  ## Convertissez la colonne "Station" en facteur avec l'ordre spécifié
  #data_rf$Genre <- factor(data_rf$Genre, levels = ordre_genre)
  
  data_rf$Rang <- seq(1:nrow(data_rf))
  data_rf$Frequence <- data_rf$Comptage/sum(data_rf$Comptage)
  
  ggplot(data_rf)+
    geom_point(aes(x=Rang,y=Frequence),size=3,col="grey")+
    geom_line(aes(x=Rang,y=Frequence),size=1)+
    scale_x_log10()+
    scale_y_log10()+
    geom_rug(aes(x = Rang, y = Frequence),outside = F, length = unit(0.02,"npc"))+
    labs(title = paste("Diagramme rang-frequence pendant bloom de",bloomeur),
         subtitle = "Monospecifique - cluster 3",
         x = "Rang", y = "Frequence")
  nom <- paste0("Bloom_",bloomeur,"_mono_cluster3.png")
  ggsave(nom, path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Rang_frequence",dpi = 600, width = 200, height = 180, units = 'mm')
  
  
  data_inbloom <- filter(data, Bloom == bloomeur)
  data_inbloom <- data_inbloom |> 
    filter(Monospecifique == "N") |>
    select(Code_point_Libelle,Date,Actinoptychus:Auricula) |>
    pivot_longer(Actinoptychus:Auricula,names_to = "Genre",values_to = "Comptage")
  
  data_rf <- summarise(group_by(data_inbloom, Genre), Comptage=sum(Comptage,na.rm=T))
  
  data_rf <- data_rf |>
    arrange(desc(Comptage))
  
  #ordre_genre <- data_rf$Genre
  #
  ## Convertissez la colonne "Station" en facteur avec l'ordre spécifié
  #data_rf$Genre <- factor(data_rf$Genre, levels = ordre_genre)
  
  data_rf$Rang <- seq(1:nrow(data_rf))
  data_rf$Frequence <- data_rf$Comptage/sum(data_rf$Comptage)
  
  ggplot(data_rf)+
    geom_point(aes(x=Rang,y=Frequence),size=3,col="grey")+
    geom_line(aes(x=Rang,y=Frequence),size=1)+
    scale_x_log10()+
    scale_y_log10()+
    geom_rug(aes(x = Rang, y = Frequence),outside = F, length = unit(0.02,"npc"))+
    labs(title = paste("Diagramme rang-frequence pendant bloom de",bloomeur),
         subtitle = "Non-monospecifique - cluster 3",
         x = "Rang", y = "Frequence")
  nom <- paste0("Bloom_",bloomeur,"_nonmono_cluster3.png")
  ggsave(nom, path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Rang_frequence",dpi = 600, width = 200, height = 180, units = 'mm')
  
  
  
  
  
  
  
  # Cluster 4
  data <- filter(data, cluster == 4)
  
  bloomeur  <- levels(as.factor(data$Bloom))[9]
  
  # Tout
  data_inbloom <- filter(data, Bloom == bloomeur)
  data_inbloom <- data_inbloom |> 
    select(Code_point_Libelle,Date,Actinoptychus:Auricula) |>
    pivot_longer(Actinoptychus:Auricula,names_to = "Genre",values_to = "Comptage")
  
  data_rf <- summarise(group_by(data_inbloom, Genre), Comptage=sum(Comptage,na.rm=T))
  
  data_rf <- data_rf |>
    arrange(desc(Comptage))
  
  #ordre_genre <- data_rf$Genre
  #
  ## Convertissez la colonne "Station" en facteur avec l'ordre spécifié
  #data_rf$Genre <- factor(data_rf$Genre, levels = ordre_genre)
  
  data_rf$Rang <- seq(1:nrow(data_rf))
  data_rf$Frequence <- data_rf$Comptage/sum(data_rf$Comptage)
  
  ggplot(data_rf)+
    geom_point(aes(x=Rang,y=Frequence),size=3,col="grey")+
    geom_line(aes(x=Rang,y=Frequence),size=1)+
    scale_x_log10()+
    scale_y_log10()+
    geom_rug(aes(x = Rang, y = Frequence),outside = F, length = unit(0.02,"npc"))+
    labs(title = paste("Diagramme rang-frequence pendant bloom de",bloomeur),
         subtitle = "Tout - cluster 4",
         x = "Rang", y = "Frequence")
  nom <- paste0("Bloom_",bloomeur,"_tout_cluster4.png")
  ggsave(nom, path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Rang_frequence",dpi = 600, width = 200, height = 180, units = 'mm')
  
  data_inbloom <- filter(data, Bloom == bloomeur)
  
  data_inbloom <- data_inbloom |> 
    filter(Monospecifique == "O") |>
    select(Code_point_Libelle,Date,Actinoptychus:Auricula) |>
    pivot_longer(Actinoptychus:Auricula,names_to = "Genre",values_to = "Comptage")
  
  data_rf <- summarise(group_by(data_inbloom, Genre), Comptage=sum(Comptage,na.rm=T))
  
  data_rf <- data_rf |>
    arrange(desc(Comptage))
  
  #ordre_genre <- data_rf$Genre
  #
  ## Convertissez la colonne "Station" en facteur avec l'ordre spécifié
  #data_rf$Genre <- factor(data_rf$Genre, levels = ordre_genre)
  
  data_rf$Rang <- seq(1:nrow(data_rf))
  data_rf$Frequence <- data_rf$Comptage/sum(data_rf$Comptage)
  
  ggplot(data_rf)+
    geom_point(aes(x=Rang,y=Frequence),size=3,col="grey")+
    geom_line(aes(x=Rang,y=Frequence),size=1)+
    scale_x_log10()+
    scale_y_log10()+
    geom_rug(aes(x = Rang, y = Frequence),outside = F, length = unit(0.02,"npc"))+
    labs(title = paste("Diagramme rang-frequence pendant bloom de",bloomeur),
         subtitle = "Monospecifique - cluster 4",
         x = "Rang", y = "Frequence")
  nom <- paste0("Bloom_",bloomeur,"_mono_cluster4.png")
  ggsave(nom, path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Rang_frequence",dpi = 600, width = 200, height = 180, units = 'mm')
  
  
  data_inbloom <- filter(data, Bloom == bloomeur)
  data_inbloom <- data_inbloom |> 
    filter(Monospecifique == "N") |>
    select(Code_point_Libelle,Date,Actinoptychus:Auricula) |>
    pivot_longer(Actinoptychus:Auricula,names_to = "Genre",values_to = "Comptage")
  
  data_rf <- summarise(group_by(data_inbloom, Genre), Comptage=sum(Comptage,na.rm=T))
  
  data_rf <- data_rf |>
    arrange(desc(Comptage))
  
  #ordre_genre <- data_rf$Genre
  #
  ## Convertissez la colonne "Station" en facteur avec l'ordre spécifié
  #data_rf$Genre <- factor(data_rf$Genre, levels = ordre_genre)
  
  data_rf$Rang <- seq(1:nrow(data_rf))
  data_rf$Frequence <- data_rf$Comptage/sum(data_rf$Comptage)
  
  ggplot(data_rf)+
    geom_point(aes(x=Rang,y=Frequence),size=3,col="grey")+
    geom_line(aes(x=Rang,y=Frequence),size=1)+
    scale_x_log10()+
    scale_y_log10()+
    geom_rug(aes(x = Rang, y = Frequence),outside = F, length = unit(0.02,"npc"))+
    labs(title = paste("Diagramme rang-frequence pendant bloom de",bloomeur),
         subtitle = "Non-monospecifique - cluster 4",
         x = "Rang", y = "Frequence")
  nom <- paste0("Bloom_",bloomeur,"_nonmono_cluster4.png")
  ggsave(nom, path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Rang_frequence",dpi = 600, width = 200, height = 180, units = 'mm')
  
  
  
  
  
# Avant bloom   
  data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid.csv", 
                     delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = ""), trim_ws = TRUE)
  
  
  data <- data %>%
    arrange(Code_point_Libelle, Date)
  # Diagramme rang frequence pendant un bloom 
  # Tout cluster
  i = 1
  bloomeur <- levels(as.factor(data$Bloom))[i]
  data_before  <- data[grep(levels(as.factor(data$Bloom))[i],data$Bloom)-1,]
  
  # Tout
  data_before <- filter(data_before, is.na(Bloom))
  data_before <- data_before |> 
    select(Code_point_Libelle,Date,Actinoptychus:Auricula) |>
    pivot_longer(Actinoptychus:Auricula,names_to = "Genre",values_to = "Comptage")
  
  data_rf <- summarise(group_by(data_before, Genre), Comptage=sum(Comptage,na.rm=T))
  
  data_rf <- data_rf |>
    arrange(desc(Comptage))
  
  #ordre_genre <- data_rf$Genre
  #
  ## Convertissez la colonne "Station" en facteur avec l'ordre spécifié
  #data_rf$Genre <- factor(data_rf$Genre, levels = ordre_genre)
  
  data_rf$Rang <- seq(1:nrow(data_rf))
  data_rf$Frequence <- data_rf$Comptage/sum(data_rf$Comptage)
  
  ggplot(data_rf)+
    geom_point(aes(x=Rang,y=Frequence),size=3,col="grey")+
    geom_line(aes(x=Rang,y=Frequence),size=1)+
    scale_x_log10()+
    scale_y_log10()+
    geom_rug(aes(x = Rang, y = Frequence),outside = F, length = unit(0.02,"npc"))+
    labs(title = paste("Diagramme rang-frequence avant bloom de",bloomeur),
         subtitle = "Tout",
         x = "Rang", y = "Frequence")
  nom <- paste0("Avant_Bloom_",bloomeur,"_tout.png")
  ggsave(nom, path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Rang_frequence",dpi = 600, width = 200, height = 180, units = 'mm')
  
  
  # Diagramme rang frequence apres un bloom 
  # Tout cluster
  i = 35
  
{  bloomeur <- levels(as.factor(data$Bloom))[i]
  data_after  <- data[grep(levels(as.factor(data$Bloom))[i],data$Bloom)+1,]
  
  # Tout
  data_after <- filter(data_after, is.na(Bloom))
  data_after <- data_after |> 
    select(Code_point_Libelle,Date,Actinoptychus:Auricula) |>
    pivot_longer(Actinoptychus:Auricula,names_to = "Genre",values_to = "Comptage")
  
  data_rf <- summarise(group_by(data_after, Genre), Comptage=sum(Comptage,na.rm=T))
  
  data_rf <- data_rf |>
    arrange(desc(Comptage))
  
  #ordre_genre <- data_rf$Genre
  #
  ## Convertissez la colonne "Station" en facteur avec l'ordre spécifié
  #data_rf$Genre <- factor(data_rf$Genre, levels = ordre_genre)
  
  data_rf$Rang <- seq(1:nrow(data_rf))
  data_rf$Frequence <- data_rf$Comptage/sum(data_rf$Comptage)
  
  ggplot(data_rf)+
    geom_point(aes(x=Rang,y=Frequence),size=3,col="grey")+
    geom_line(aes(x=Rang,y=Frequence),size=1)+
    scale_x_log10()+
    scale_y_log10()+
    geom_rug(aes(x = Rang, y = Frequence),outside = F, length = unit(0.02,"npc"))+
    labs(title = paste("Diagramme rang-frequence apres bloom de",bloomeur),
         subtitle = "Tout",
         x = "Rang", y = "Frequence")
  nom <- paste0("Apres_Bloom_",bloomeur,"_tout.png")
  ggsave(nom, path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Rang_frequence",dpi = 600, width = 200, height = 180, units = 'mm')
}  
  
data2 <- data
data2$ligne <- c(1:7582)

i = 1
bloomeur <- levels(as.factor(data$Bloom))[i]

data_before <- filter(data2, Bloom == bloomeur)

data_before <- data_before |> 
  filter(Monospecifique == "O") 

ligne <- data_before$ligne

ligne -1 
data_before <- data[ligne-1,]
data_before <- filter(data_before, is.na(Bloom))

data_before <- data_before |>
  select(Code_point_Libelle,Date,Actinoptychus:Auricula) |>
  pivot_longer(Actinoptychus:Auricula,names_to = "Genre",values_to = "Comptage")

data_rf <- summarise(group_by(data_before, Genre), Comptage=sum(Comptage,na.rm=T))

data_rf <- data_rf |>
  arrange(desc(Comptage))

#ordre_genre <- data_rf$Genre
#
## Convertissez la colonne "Station" en facteur avec l'ordre spécifié
#data_rf$Genre <- factor(data_rf$Genre, levels = ordre_genre)

data_rf$Rang <- seq(1:nrow(data_rf))
data_rf$Frequence <- data_rf$Comptage/sum(data_rf$Comptage)

ggplot(data_rf)+
  geom_point(aes(x=Rang,y=Frequence),size=3,col="grey")+
  geom_line(aes(x=Rang,y=Frequence),size=1)+
  scale_x_log10()+
  scale_y_log10()+
  geom_rug(aes(x = Rang, y = Frequence),outside = F, length = unit(0.02,"npc"))+
  labs(title = paste("Diagramme rang-frequence avant bloom de",bloomeur),
       subtitle = "Monospecifique",
       x = "Rang", y = "Frequence")
nom <- paste0("Avant_Bloom_",bloomeur,"_mono.png")
ggsave(nom, path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Rang_frequence",dpi = 600, width = 200, height = 180, units = 'mm')


bloomeur <- levels(as.factor(data$Bloom))[i]

data_after <- filter(data2, Bloom == bloomeur)

data_after <- data_after |> 
  filter(Monospecifique == "O") 

ligne <- data_after$ligne

ligne +1 
data_after <- data[ligne+1,]
data_after <- filter(data_after, is.na(Bloom))
data_after <- data_after |>
  select(Code_point_Libelle,Date,Actinoptychus:Auricula) |>
  pivot_longer(Actinoptychus:Auricula,names_to = "Genre",values_to = "Comptage")

data_rf <- summarise(group_by(data_after, Genre), Comptage=sum(Comptage,na.rm=T))

data_rf <- data_rf |>
  arrange(desc(Comptage))

#ordre_genre <- data_rf$Genre
#
## Convertissez la colonne "Station" en facteur avec l'ordre spécifié
#data_rf$Genre <- factor(data_rf$Genre, levels = ordre_genre)

data_rf$Rang <- seq(1:nrow(data_rf))
data_rf$Frequence <- data_rf$Comptage/sum(data_rf$Comptage)

ggplot(data_rf)+
  geom_point(aes(x=Rang,y=Frequence),size=3,col="grey")+
  geom_line(aes(x=Rang,y=Frequence),size=1)+
  scale_x_log10()+
  scale_y_log10()+
  geom_rug(aes(x = Rang, y = Frequence),outside = F, length = unit(0.02,"npc"))+
  labs(title = paste("Diagramme rang-frequence apres bloom de",bloomeur),
       subtitle = "Monospecifique",
       x = "Rang", y = "Frequence")
nom <- paste0("Apres_Bloom_",bloomeur,"_mono.png")
ggsave(nom, path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Rang_frequence",dpi = 600, width = 200, height = 180, units = 'mm')








i = 34
bloomeur <- levels(as.factor(data$Bloom))[i]

data_before <- filter(data2, Bloom == bloomeur)

data_before <- data_before |> 
  filter(Monospecifique == "N") 

ligne <- data_before$ligne

ligne -1 
data_before <- data[ligne-1,]
data_before <- filter(data_before, is.na(Bloom))

data_before <- data_before |>
  select(Code_point_Libelle,Date,Actinoptychus:Auricula) |>
  pivot_longer(Actinoptychus:Auricula,names_to = "Genre",values_to = "Comptage")

data_rf <- summarise(group_by(data_before, Genre), Comptage=sum(Comptage,na.rm=T))

data_rf <- data_rf |>
  arrange(desc(Comptage))

#ordre_genre <- data_rf$Genre
#
## Convertissez la colonne "Station" en facteur avec l'ordre spécifié
#data_rf$Genre <- factor(data_rf$Genre, levels = ordre_genre)

data_rf$Rang <- seq(1:nrow(data_rf))
data_rf$Frequence <- data_rf$Comptage/sum(data_rf$Comptage)

ggplot(data_rf)+
  geom_point(aes(x=Rang,y=Frequence),size=3,col="grey")+
  geom_line(aes(x=Rang,y=Frequence),size=1)+
  scale_x_log10()+
  scale_y_log10()+
  geom_rug(aes(x = Rang, y = Frequence),outside = F, length = unit(0.02,"npc"))+
  labs(title = paste("Diagramme rang-frequence avant bloom de",bloomeur),
       subtitle = "Non-Monospecifique",
       x = "Rang", y = "Frequence")
nom <- paste0("Avant_Bloom_",bloomeur,"_nonmono.png")
ggsave(nom, path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Rang_frequence",dpi = 600, width = 200, height = 180, units = 'mm')


bloomeur <- levels(as.factor(data$Bloom))[i]

data_after <- filter(data2, Bloom == bloomeur)

data_after <- data_after |> 
  filter(Monospecifique == "N") 

ligne <- data_after$ligne

ligne +1 
data_after <- data[ligne+1,]
data_after <- filter(data_after, is.na(Bloom))
data_after <- data_after |>
  select(Code_point_Libelle,Date,Actinoptychus:Auricula) |>
  pivot_longer(Actinoptychus:Auricula,names_to = "Genre",values_to = "Comptage")

data_rf <- summarise(group_by(data_after, Genre), Comptage=sum(Comptage,na.rm=T))

data_rf <- data_rf |>
  arrange(desc(Comptage))

#ordre_genre <- data_rf$Genre
#
## Convertissez la colonne "Station" en facteur avec l'ordre spécifié
#data_rf$Genre <- factor(data_rf$Genre, levels = ordre_genre)

data_rf$Rang <- seq(1:nrow(data_rf))
data_rf$Frequence <- data_rf$Comptage/sum(data_rf$Comptage)

ggplot(data_rf)+
  geom_point(aes(x=Rang,y=Frequence),size=3,col="grey")+
  geom_line(aes(x=Rang,y=Frequence),size=1)+
  scale_x_log10()+
  scale_y_log10()+
  geom_rug(aes(x = Rang, y = Frequence),outside = F, length = unit(0.02,"npc"))+
  labs(title = paste("Diagramme rang-frequence apres bloom de",bloomeur),
       subtitle = "Non-Monospecifique",
       x = "Rang", y = "Frequence")
nom <- paste0("Apres_Bloom_",bloomeur,"_nonmono.png")
ggsave(nom, path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Rang_frequence",dpi = 600, width = 200, height = 180, units = 'mm')


