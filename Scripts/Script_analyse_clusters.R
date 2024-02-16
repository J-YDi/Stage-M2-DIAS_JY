##### Stations plus de 5 ans ########
# Import data
data <- read_delim("data_modif/Table_FLORTOT_Surf_9523_Stselect_hydro_phyto_chloro_phylum_period5_chlafilter_cluster_div.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

data$cluster <- as.factor(data$cluster)
cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

# Carte des stations
Worldmap <- map_data('worldHires')

data$lon <- as.numeric(data$lon)
data$lat <- as.numeric(data$lat)

ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)')+
  theme_gdocs()+
  geom_point(data = data, aes(x = lon, y = lat,colour=cluster), size =8)+
  scale_colour_manual(values = cluster_col,name="Cluster")+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('maps_station_ALLCluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description", dpi = 600, width = 400, height = 400, units = 'mm')



# Au niveau des clusters

ggplot(data)+
  geom_boxplot(aes(x=Month,y=TEMP,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution des temperatures en fonction des mois par cluster",
       x = "Mois", y = "Temperature")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('TEMP_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Month,y=SALI,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la salinite en fonction des mois par cluster",
       x = "Mois", y = "Salinite")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('SALI_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=NH4,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la concentration en NH4 en fonction des mois par cluster",
       x = "Mois", y = "Concentration en NH4")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,30, by = 5),limits = c(0,15))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('NH4_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=SIOH,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la concentration en SIOH en fonction des mois par cluster",
       x = "Mois", y = "Concentration en SIOH")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('SIOH_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=PO4,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la concentration en PO4 en fonction des mois par cluster",
       x = "Mois", y = "Concentration en PO4")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('PO4_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=OXYGENE,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la concentration en oxygene en fonction des mois par cluster",
       x = "Mois", y = "Concentration en oxygene")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('OXYGENE_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=TURB,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la turbidite (NTU) en fonction des mois par cluster",
       x = "Mois", y = "Turbidite (NTU)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,150, by = 15))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('TURB_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=`TURB-FNU`,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la turbidite (FNU) en fonction des mois par cluster",
       x = "Mois", y = "Turbidite (FNU)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,220, by = 15))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('TURBFNU_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=`NO3+NO2`,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la concentration en NO3+NO2 en fonction des mois par cluster",
       x = "Mois", y = "Concentration en NO3+NO2")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,400, by = 20))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('NO3NO2_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=CHLOROA,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la concentration en chlorophylle en fonction des mois par cluster",
       x = "Mois", y = "Concentration en chl-a")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,220, by = 15))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('CHLOROA_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=log(Bacillariophyceae+1),group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Bacillariophyceae en fonction des mois par cluster",
       x = "Mois", y = "log(Abondance Bacillariphyceae+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('BACILLARIO_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=log(Dinophyceae+1),group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Dinophyceae en fonction des mois par cluster",
       x = "Mois", y = "log(Abondance Dinophyceae+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('DINOPHYCEAE_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=log(Ciliophora+1),group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Ciliophora en fonction des mois par cluster",
       x = "Mois", y = "log(Abondance Ciliophora+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('CILIOPHORA_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=log(Alexandrium+1),group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Alexandrium en fonction des mois par cluster",
       x = "Mois", y = "log(Abondance Alexandrium+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('Alex_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Month,y=log(Lepidodinium+1),group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Lepidodinium en fonction des mois par cluster",
       x = "Mois", y = "log(Abondance Lepidodinium+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('Lepido_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Month,y=log(Lingulodinium+1),group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Lingulodinium en fonction des mois par cluster",
       x = "Mois", y = "log(Abondance Lingulodinium+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('Lingu_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Month,y=log(Mesodinium+1),group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Mesodinium en fonction des mois par cluster",
       x = "Mois", y = "log(Abondance Mesodinium+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('Mesodinium_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Month,y=log(Noctiluca+1),group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Noctiluca en fonction des mois par cluster",
       x = "Mois", y = "log(Abondance Noctiluca+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('Noctiluca_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=Shannon,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'indice de Shannon en fonction des mois par cluster",
       x = "Mois", y = "Shannon index")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('Shannon_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Month,y=Pielou,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'indice de Pielou en fonction des mois par cluster",
       x = "Mois", y = "Pielou index")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('Pielou_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=BergerParker,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'indice de Berger-Parker en fonction des mois par cluster",
       x = "Mois", y = "Berger-Parker index")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('Bergerparker_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')



ggplot(data)+
  geom_boxplot(aes(x=Year,y=TEMP,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution des temperatures en fonction des Annees par cluster",
       x = "Annee", y = "Temperature")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = seq(1995,2023,by=1))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 2)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('TEMP_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Year,y=SALI,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la salinite en fonction des Annees par cluster",
       x = "Annee", y = "Salinite")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1995:2023))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('SALI_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=NH4,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la concentration en NH4 en fonction des Annees par cluster",
       x = "Annee", y = "Concentration en NH4")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1995:2023))+
  scale_y_continuous(breaks = seq(0,30, by = 5),limits = c(0,15))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('NH4_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=SIOH,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la concentration en SIOH en fonction des Annees par cluster",
       x = "Annee", y = "Concentration en SIOH")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1995:2023))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('SIOH_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=PO4,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la concentration en PO4 en fonction des Annees par cluster",
       x = "Annee", y = "Concentration en PO4")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1995:2023))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('PO4_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=OXYGENE,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la concentration en oxygene en fonction des Annees par cluster",
       x = "Annee", y = "Concentration en oxygene")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1995:2023))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('OXYGENE_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=TURB,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la turbidite (NTU) en fonction des Annees par cluster",
       x = "Annee", y = "Turbidite (NTU)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1995:2023))+
  scale_y_continuous(breaks = seq(0,150, by = 15))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('TURB_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=`TURB-FNU`,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la turbidite (FNU) en fonction des Annees par cluster",
       x = "Annee", y = "Turbidite (FNU)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1995:2023))+
  scale_y_continuous(breaks = seq(0,220, by = 15))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('TURBFNU_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=`NO3+NO2`,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la concentration en NO3+NO2 en fonction des Annees par cluster",
       x = "Annee", y = "Concentration en NO3+NO2")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1995:2023))+
  scale_y_continuous(breaks = seq(0,400, by = 20))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('NO3NO2_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=CHLOROA,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la concentration en chlorophylle en fonction des Annees par cluster",
       x = "Annee", y = "Concentration en chl-a")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1995:2023))+
  scale_y_continuous(breaks = seq(0,220, by = 15))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('CHLOROA_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=log(Bacillariophyceae+1),group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Bacillariophyceae en fonction des Annee par cluster",
       x = "Annee", y = "log(Abondance Bacillariphyceae+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1995:2023))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('BACILLARIO_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=log(Dinophyceae+1),group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Dinophyceae en fonction des Annees par cluster",
       x = "Annee", y = "log(Abondance Dinophyceae+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1995:2023))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('DINOPHYCEAE_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=log(Ciliophora+1),group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Ciliophora en fonction des Annee par cluster",
       x = "Annee", y = "log(Abondance Ciliophora+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1995:2023))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('CILIOPHORA_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=log(Alexandrium+1),group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Alexandrium en fonction des Annees par cluster",
       x = "Annee", y = "log(Abondance Alexandrium+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1995:2023))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Alex_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Year,y=log(Lepidodinium+1),group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Lepidodinium en fonction des Annee par cluster",
       x = "Annee", y = "log(Abondance Lepidodinium+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1995:2023))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Lepido_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Year,y=log(Lingulodinium+1),group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Lingulodinium en fonction des Annee par cluster",
       x = "Annee", y = "log(Abondance Lingulodinium+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1995:2023))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Lingu_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Year,y=log(Mesodinium+1),group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Mesodinium en fonction des Annee par cluster",
       x = "Annee", y = "log(Abondance Mesodinium+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1995:2023))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Mesodinium_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Year,y=log(Noctiluca+1),group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Noctiluca en fonction des Annees par cluster",
       x = "Annee", y = "log(Abondance Noctiluca+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1995:2023))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Noctiluca_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=Shannon,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'indice de Shannon en fonction des Annees par cluster",
       x = "Annee", y = "Shannon index")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1995:2023))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Shannon_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Year,y=Pielou,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'indice de Pielou en fonction des Annees par cluster",
       x = "Annee", y = "Pielou index")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1995:2023))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Pielou_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=BergerParker,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'indice de Berger-Parker en fonction des Annees par cluster",
       x = "Annee", y = "Berger-Parker index")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1995:2023))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Bergerparker_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')




ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=TEMP,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Temperatures en fonction des stations des clusters",
       x = "Station", y = "Temperature",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,35, by = 5),limits = c(0,35))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('TEMP_Station_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=SALI,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Salinites en fonction des stations des clusters",
       x = "Station", y = "Salinite",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,35, by = 5),limits = c(0,35))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('SALI_Station_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=NH4,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Concentration en NH4 en fonction des stations des clusters",
       x = "Station", y = "NH4",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('NH4_Station_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=SIOH,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Concentration en SIOH en fonction des stations des clusters",
       x = "Station", y = "SIOH",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('SIOH_Station_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=PO4,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Concentration en PO4 en fonction des stations des clusters",
       x = "Station", y = "PO4",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('PO4_Station_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=OXYGENE,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Concentration en OXYGENE en fonction des stations des clusters",
       x = "Station", y = "OXYGENE",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('OXYGENE_Station_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=CHLOROA,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Concentration en CHLOROA en fonction des stations des clusters",
       x = "Station", y = "CHLOROA",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('CHLOROA_Station_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=`NO3+NO2`,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Concentration en NO3+NO2 en fonction des stations des clusters",
       x = "Station", y = "NO3+NO2",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('NO3NO2_Station_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=`TURB-FNU`,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Turbidite (FNU) en fonction des stations des clusters",
       x = "Station", y = "Turbidite (FNU)",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('TURBFNU_Station_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=log(Bacillariophyceae+1),group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Abondance en Bacillariophyceae en fonction des stations des clusters",
       x = "Station", y = "log(Bacillariophyceae+1)",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('BACILLARIO_Station_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=log(Dinophyceae+1),group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Abondance en Dinophyceae en fonction des stations des clusters",
       x = "Station", y = "log(Dinophyceae+1)",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('DINOPHYCEAE_Station_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=log(Ciliophora+1),group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Abondance en Ciliophora en fonction des stations des clusters",
       x = "Station", y = "log(Ciliophora+1)",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('CILIOPHORA_Station_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=log(Dinophysis+1),group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Abondance en Dinophysis en fonction des stations des clusters",
       x = "Station", y = "log(Dinophysis+1)",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Dino_Station_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=log(Noctiluca+1),group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Abondance en Noctiluca en fonction des stations des clusters",
       x = "Station", y = "log(Noctiluca+1)",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Noctiluca_Station_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=log(Mesodinium+1),group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Abondance en Mesodinium en fonction des stations des clusters",
       x = "Station", y = "log(Mesodinium+1)",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Mesodinium_Station_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=log(Lingulodinium+1),group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Abondance en Lingulodinium en fonction des stations des clusters",
       x = "Station", y = "log(Lingulodinium+1)",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Lingu_Station_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=log(Lepidodinium+1),group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Abondance en Lepidodinium en fonction des stations des clusters",
       x = "Station", y = "log(Lepidodinium+1)",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Lepidodinium_Station_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=log(Alexandrium+1),group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Abondance en Alexandrium en fonction des stations des clusters",
       x = "Station", y = "log(Alexandrium+1)",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Alexandrium_Station_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=Shannon,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Indice de Shannon en fonction des stations des clusters",
       x = "Station", y = "Shannon index",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Shannon_Station_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=Pielou,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Indice de Pielou en fonction des stations des clusters",
       x = "Station", y = "Pielou index",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Pielou_Station_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=BergerParker,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Indice de Berger-Parker en fonction des stations des clusters",
       x = "Station", y = "Berger-Parker index",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('BergerParker_Station_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


dataforbox_hydro <- pivot_longer(data, names_to = "Variable",cols = c(CHLOROA:`TURB-FNU`))
dataforbox_hydro <- dplyr::select(dataforbox_hydro,Code.Region:Code.parametre,Variable,value)

dataforbox_phyto <- pivot_longer(data, names_to = "Variable",cols = c(Bacillariophyceae,Dinophyceae,Ciliophora,Dinophysis,Noctiluca,Mesodinium,Lingulodinium,Lepidodinium,Alexandrium))
dataforbox_phyto <- dplyr::select(dataforbox_phyto,Code.Region:Code.parametre,Variable,value)

dataforbox_div <- pivot_longer(data, names_to = "Variable",cols = c(Shannon,Pielou,BergerParker))
dataforbox_div <- dplyr::select(dataforbox_div,Code.Region:Code.parametre,Variable,value)

ggplot(dataforbox_hydro)+
  geom_boxplot(aes(y=value,x=Month,group = Month,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(title = "Parametres hydrologiques",
       x = "Mois", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,100, by = 5),limits = c(0,100))+
  scale_x_continuous(breaks = c(1:12))+
  facet_wrap(cluster ~ Variable, nrow = 5, scales="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 7))
ggsave('Hydro_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(dataforbox_hydro)+
  geom_boxplot(aes(y=value,x=Year,group = Year,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(title = "Parametres hydrologiques",
       x = "Year", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,100, by = 5),limits = c(0,100))+
  scale_x_continuous(n.breaks = 25)+
  facet_wrap(cluster ~ Variable, nrow = 5, scales="free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 2))
ggsave('Hydro_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(dataforbox_phyto)+
  geom_boxplot(aes(y=log(value+1),x=Month,group = Month,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(title = "Phytoplancton",
       x = "Mois", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,20, by = 5),limits = c(0,20))+
  scale_x_continuous(breaks = c(1:12))+
  facet_wrap(cluster ~ Variable, nrow = 5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 7))
ggsave('Phyto_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(dataforbox_phyto)+
  geom_boxplot(aes(y=log(value+1),x=Year,group = Year,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(title = "Phytoplancton",
       x = "Year", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,20, by = 5),limits = c(0,20))+
  scale_x_continuous(n.breaks = 25)+
  facet_wrap(cluster ~ Variable, nrow = 5, scales="free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 2))
ggsave('Phyto_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(dataforbox_div)+
  geom_boxplot(aes(y=log(value+1),x=Month,group = Month,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(title = "Indices de diversite",
       x = "Mois", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,20, by = 5),limits = c(0,20))+
  scale_x_continuous(breaks = c(1:12))+
  facet_wrap(cluster ~ Variable, nrow = 5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 12))
ggsave('Div_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(dataforbox_div)+
  geom_boxplot(aes(y=log(value+1),x=Year,group = Year,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(title = "Indice de diversite",
       x = "Year", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1.5, by = 0.3),limits = c(0,1.5))+
  scale_x_continuous(breaks = seq(1995,2023, by = 1),limits = c(1995,2023))+
  facet_wrap(cluster ~ Variable, nrow = 5, scales="free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 9))
ggsave('Div_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

## Travailler sur les moyennes directement
# Mois 
data_clus_month <- group_by(data, cluster,Month)
data_clus_month_hydro <- summarise(data_clus_month, TEMP=mean(TEMP,na.rm=T),SALI=mean(SALI,na.rm=T),TURB=mean(TURB,na.rm=T),NH4=mean(NH4,na.rm=T),
                                   SIOH=mean(SIOH,na.rm=T),PO4=mean(PO4,na.rm=T),OXYGENE=mean(OXYGENE,na.rm=T),NO3NO2=mean(`NO3+NO2`,na.rm=T),
                                   TURBFNU=mean(`TURB-FNU`,na.rm=T),CHLOROA=mean(CHLOROA,na.rm=T))

dataforline_hydro <- pivot_longer(data_clus_month_hydro, names_to = "Variable",cols = c(TEMP:CHLOROA))
dataforline_hydro <- dplyr::select(dataforline_hydro,cluster,Month,Variable,value)


data_clus_month_phyto <- summarise(data_clus_month, Bacillariophyceae=mean(Bacillariophyceae,na.rm=T),Dinophyceae=mean(Dinophyceae,na.rm=T),Ciliophora=mean(Ciliophora,na.rm=T),Dinophysis=mean(Dinophysis,na.rm=T),
                                   Noctiluca=mean(Noctiluca,na.rm=T),Mesodinium=mean(Mesodinium,na.rm=T),Lingulodinium=mean(Lingulodinium,na.rm=T),Lepidodinium=mean(Lepidodinium,na.rm=T),
                                   Alexandrium=mean(Alexandrium,na.rm=T))

dataforline_phyto <- pivot_longer(data_clus_month_phyto, names_to = "Variable",cols = c(Bacillariophyceae:Alexandrium))
dataforline_phyto <- dplyr::select(dataforline_phyto,cluster,Month,Variable,value)

data_clus_month_div <- summarise(data_clus_month, Shannon=mean(Shannon,na.rm=T),Pielou=mean(Pielou,na.rm=T),BergerParker=mean(BergerParker,na.rm=T))

dataforline_div <- pivot_longer(data_clus_month_div, names_to = "Variable",cols = c(Shannon:BergerParker))
dataforline_div <- dplyr::select(dataforline_div,cluster,Month,Variable,value)


ggplot(dataforline_hydro)+
  geom_line(aes(y=value,x=Month,colour= cluster),linewidth = 2)+
  labs(title = "Hydrologie moyenne",
       x = "Mois", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,20, by = 5),limits = c(0,20))+
  scale_x_continuous(n.breaks = 12)+
  facet_wrap(cluster ~ Variable, nrow = 5, scales="free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 7))
ggsave('Hydro_Moy_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(dataforline_phyto)+
  geom_line(aes(y=log(value+1),x=Month,colour= cluster),linewidth = 2)+
  labs(title = "Abondance en phytoplancton moyenne",
       x = "Mois", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,20, by = 5),limits = c(0,20))+
  scale_x_continuous(n.breaks = 12)+
  facet_wrap(cluster ~ Variable, nrow = 5, scales="free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 7))
ggsave('Phyto_Moy_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(dataforline_div)+
  geom_line(aes(y=value,x=Month,colour= cluster),linewidth = 2)+
  labs(title = "Valeur indice de diversité moyen",
       x = "Mois", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,20, by = 5),limits = c(0,20))+
  scale_x_continuous(n.breaks = 12)+
  facet_wrap(cluster ~ Variable, nrow = 5, scales="free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 12))
ggsave('Div_Moy_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

# Annee
data_clus_Year <- group_by(data, cluster,Year)
data_clus_Year_hydro <- summarise(data_clus_Year, TEMP=mean(TEMP,na.rm=T),SALI=mean(SALI,na.rm=T),TURB=mean(TURB,na.rm=T),NH4=mean(NH4,na.rm=T),
                                  SIOH=mean(SIOH,na.rm=T),PO4=mean(PO4,na.rm=T),OXYGENE=mean(OXYGENE,na.rm=T),NO3NO2=mean(`NO3+NO2`,na.rm=T),
                                  TURBFNU=mean(`TURB-FNU`,na.rm=T),CHLOROA=mean(CHLOROA,na.rm=T))

dataforline_hydro <- pivot_longer(data_clus_Year_hydro, names_to = "Variable",cols = c(TEMP:CHLOROA))
dataforline_hydro <- dplyr::select(dataforline_hydro,cluster,Year,Variable,value)


data_clus_Year_phyto <- summarise(data_clus_Year, Bacillariophyceae=mean(Bacillariophyceae,na.rm=T),Dinophyceae=mean(Dinophyceae,na.rm=T),Ciliophora=mean(Ciliophora,na.rm=T),Dinophysis=mean(Dinophysis,na.rm=T),
                                  Noctiluca=mean(Noctiluca,na.rm=T),Mesodinium=mean(Mesodinium,na.rm=T),Lingulodinium=mean(Lingulodinium,na.rm=T),Lepidodinium=mean(Lepidodinium,na.rm=T),
                                  Alexandrium=mean(Alexandrium,na.rm=T))

dataforline_phyto <- pivot_longer(data_clus_Year_phyto, names_to = "Variable",cols = c(Bacillariophyceae:Alexandrium))
dataforline_phyto <- dplyr::select(dataforline_phyto,cluster,Year,Variable,value)

data_clus_Year_div <- summarise(data_clus_Year, Shannon=mean(Shannon,na.rm=T),Pielou=mean(Pielou,na.rm=T),BergerParker=mean(BergerParker,na.rm=T))

dataforline_div <- pivot_longer(data_clus_Year_div, names_to = "Variable",cols = c(Shannon:BergerParker))
dataforline_div <- dplyr::select(dataforline_div,cluster,Year,Variable,value)


ggplot(dataforline_hydro)+
  geom_line(aes(y=value,x=Year,colour= cluster),linewidth = 2)+
  labs(title = "Hydrologie moyenne",
       x = "Annee", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_colour_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,20, by = 5),limits = c(0,20))+
  scale_x_continuous(breaks = seq(1995,2023,by=1),limits=c(1995,2023))+
  facet_wrap(cluster ~ Variable, nrow = 5, scales="free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 4))
ggsave('Hydro_Moy_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(dataforline_phyto)+
  geom_line(aes(y=log(value+1),x=Year,colour= cluster),linewidth = 2)+
  labs(title = "Abondance en phytoplancton moyenne",
       x = "Annee", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_colour_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = seq(1995,2023,by=1),limits=c(1995,2023))+
  facet_wrap(cluster ~ Variable, nrow = 5, scales="free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 4))
ggsave('Phyto_Moy_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(dataforline_div)+
  geom_line(aes(y=value,x=Year,colour= cluster),linewidth = 2)+
  labs(title = "Valeur indice de diversité moyen",
       x = "Annee", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_colour_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = seq(1995,2023,by=1),limits=c(1995,2023))+
  facet_wrap(cluster ~ Variable, nrow = 5, scales="free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 4))
ggsave('Div_Moy_Annee_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

# Test stats difference entre les clusters
kruskal.test(data$TEMP~data$cluster)
DunnTest(data$TEMP~data$cluster,method="BH") 

kruskal.test(data$SALI~data$cluster)
DunnTest(data$SALI~data$cluster,method="BH") 

kruskal.test(data$TURB~data$cluster)
DunnTest(data$TURB~data$cluster,method="BH") 

kruskal.test(data$NH4~data$cluster)
DunnTest(data$NH4~data$cluster,method="BH") 

kruskal.test(data$PO4~data$cluster)
DunnTest(data$PO4~data$cluster,method="BH") 

kruskal.test(data$SIOH~data$cluster)
DunnTest(data$SIOH~data$cluster,method="BH") 

kruskal.test(data$OXYGENE~data$cluster)
DunnTest(data$OXYGENE~data$cluster,method="BH") 

kruskal.test(data$CHLOROA~data$cluster)
DunnTest(data$CHLOROA~data$cluster,method="BH") 

kruskal.test(data$`NO3+NO2`~data$cluster)
DunnTest(data$`NO3+NO2`~data$cluster,method="BH") 

kruskal.test(data$`TURB-FNU`~data$cluster)
DunnTest(data$`TURB-FNU`~data$cluster,method="BH") 

kruskal.test(data$Bacillariophyceae~data$cluster)
DunnTest(data$Bacillariophyceae~data$cluster,method="BH") 

kruskal.test(data$Dinophyceae~data$cluster)
DunnTest(data$Dinophyceae~data$cluster,method="BH") 

kruskal.test(data$Ciliophora~data$cluster)
DunnTest(data$Ciliophora~data$cluster,method="BH") 

kruskal.test(data$Dinophysis~data$cluster)
DunnTest(data$Dinophysis~data$cluster,method="BH") 

kruskal.test(data$Alexandrium~data$cluster)
DunnTest(data$Alexandrium~data$cluster,method="BH") 

kruskal.test(data$Noctiluca~data$cluster)
DunnTest(data$Noctiluca~data$cluster,method="BH") 

kruskal.test(data$Mesodinium~data$cluster)
DunnTest(data$Mesodinium~data$cluster,method="BH") 

kruskal.test(data$Lingulodinium~data$cluster)
DunnTest(data$Lingulodinium~data$cluster,method="BH") 

kruskal.test(data$Lepidodinium~data$cluster)
DunnTest(data$Lepidodinium~data$cluster,method="BH") 

kruskal.test(data$Shannon~data$cluster)
DunnTest(data$Shannon~data$cluster,method="BH") 

kruskal.test(data$Pielou~data$cluster)
DunnTest(data$Pielou~data$cluster,method="BH") 

kruskal.test(data$BergerParker~data$cluster)
DunnTest(data$BergerParker~data$cluster,method="BH") 

# Difference de saisonnalite
Table.dino <- dplyr::select(data,cluster,Code_point_Libelle,Month,Dinophysis)
Table.dino[Table.dino== 0] = NA
Table.dino <- Table.dino[!is.na(Table.dino$Dinophysis),]

ggplot(Table.dino, aes(x = Month, y = cluster, fill = cluster))+
  geom_density_ridges(stat = 'binline', bins = 12, scale = 0.9,
                      #rel_min_height = .01,
                      draw_baseline = F, 
                      show.legend = TRUE)+
  scale_fill_manual(name = 'Cluster', values = cluster_col)+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_y_discrete(breaks = 10)+
  labs(x = 'Mois', y = 'Presence de Dinophysis par mois')+
  theme(axis.title.x = element_text(size=10), axis.text = element_text(size=10, color = 'black'), axis.title.y =element_text(size=10),
        legend.text = , legend.title = element_text(size = 10, face = 'bold'), 
        legend.box.margin = margin(0, 0, 0, 0, 'cm'), legend.background = element_rect(color = 'gray10'),
        legend.position = 'bottom',
        panel.grid.major = element_line(color = 'white', size = .5))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('Phenologie_Dino_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

Table.nocti <- dplyr::select(data,cluster,Code_point_Libelle,Month,Noctiluca)
Table.nocti[Table.nocti== 0] = NA
Table.nocti <- Table.nocti[!is.na(Table.nocti$Noctiluca),]

ggplot(Table.nocti, aes(x = Month, y = cluster, fill = cluster))+
  geom_density_ridges(stat = 'binline', bins = 12, scale = 0.9,
                      #rel_min_height = .01,
                      draw_baseline = F, 
                      show.legend = TRUE)+
  scale_fill_manual(name = 'Cluster', values = cluster_col)+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_y_discrete(breaks = 10)+
  labs(x = 'Mois', y = 'Presence de Noctiluca par mois')+
  theme(axis.title.x = element_text(size=10), axis.text = element_text(size=10, color = 'black'), axis.title.y =element_text(size=10),
        legend.text = , legend.title = element_text(size = 10, face = 'bold'), 
        legend.box.margin = margin(0, 0, 0, 0, 'cm'), legend.background = element_rect(color = 'gray10'),
        legend.position = 'bottom',
        panel.grid.major = element_line(color = 'white', size = .5))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('Phenologie_Nocti_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')



Table.meso <- dplyr::select(data,cluster,Code_point_Libelle,Month,Mesodinium)
Table.meso[Table.meso== 0] = NA
Table.meso <- Table.meso[!is.na(Table.meso$Mesodinium),]

ggplot(Table.meso, aes(x = Month, y = cluster, fill = cluster))+
  geom_density_ridges(stat = 'binline', bins = 12, scale = 0.9,
                      #rel_min_height = .01,
                      draw_baseline = F, 
                      show.legend = TRUE)+
  scale_fill_manual(name = 'Cluster', values = cluster_col)+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_y_discrete(breaks = 10)+
  labs(x = 'Mois', y = 'Presence de Mesodinium par mois')+
  theme(axis.title.x = element_text(size=10), axis.text = element_text(size=10, color = 'black'), axis.title.y =element_text(size=10),
        legend.text = , legend.title = element_text(size = 10, face = 'bold'), 
        legend.box.margin = margin(0, 0, 0, 0, 'cm'), legend.background = element_rect(color = 'gray10'),
        legend.position = 'bottom',
        panel.grid.major = element_line(color = 'white', size = .5))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('Phenologie_Meso_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

Table.lingu <- dplyr::select(data,cluster,Code_point_Libelle,Month,Lingulodinium)
Table.lingu[Table.lingu== 0] = NA
Table.lingu <- Table.lingu[!is.na(Table.lingu$Lingulodinium),]

ggplot(Table.lingu, aes(x = Month, y = cluster, fill = cluster))+
  geom_density_ridges(stat = 'binline', bins = 12, scale = 0.9,
                      #rel_min_height = .01,
                      draw_baseline = F, 
                      show.legend = TRUE)+
  scale_fill_manual(name = 'Cluster', values = cluster_col)+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_y_discrete(breaks = 10)+
  labs(x = 'Mois', y = 'Presence de Lingulodinium par mois')+
  theme(axis.title.x = element_text(size=10), axis.text = element_text(size=10, color = 'black'), axis.title.y =element_text(size=10),
        legend.text = , legend.title = element_text(size = 10, face = 'bold'), 
        legend.box.margin = margin(0, 0, 0, 0, 'cm'), legend.background = element_rect(color = 'gray10'),
        legend.position = 'bottom',
        panel.grid.major = element_line(color = 'white', size = .5))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('Phenologie_Lingu_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

Table.lepido <- dplyr::select(data,cluster,Code_point_Libelle,Month,Lepidodinium)
Table.lepido[Table.lepido== 0] = NA
Table.lepido <- Table.lepido[!is.na(Table.lepido$Lepidodinium),]

ggplot(Table.lepido, aes(x = Month, y = cluster, fill = cluster))+
  geom_density_ridges(stat = 'binline', bins = 12, scale = 0.9,
                      #rel_min_height = .01,
                      draw_baseline = F, 
                      show.legend = TRUE)+
  scale_fill_manual(name = 'Cluster', values = cluster_col)+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_y_discrete(breaks = 10)+
  labs(x = 'Mois', y = 'Presence de Lepidodinium par mois')+
  theme(axis.title.x = element_text(size=10), axis.text = element_text(size=10, color = 'black'), axis.title.y =element_text(size=10),
        legend.text = , legend.title = element_text(size = 10, face = 'bold'), 
        legend.box.margin = margin(0, 0, 0, 0, 'cm'), legend.background = element_rect(color = 'gray10'),
        legend.position = 'bottom',
        panel.grid.major = element_line(color = 'white', size = .5))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('Phenologie_Lepido_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


Table.alex <- dplyr::select(data,cluster,Code_point_Libelle,Month,Alexandrium)
Table.alex[Table.alex== 0] = NA
Table.alex <- Table.alex[!is.na(Table.alex$Alexandrium),]

ggplot(Table.alex, aes(x = Month, y = cluster, fill = cluster))+
  geom_density_ridges(stat = 'binline', bins = 12, scale = 0.9,
                      #rel_min_height = .01,
                      draw_baseline = F, 
                      show.legend = TRUE)+
  scale_fill_manual(name = 'Cluster', values = cluster_col)+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_y_discrete(breaks = 10)+
  labs(x = 'Mois', y = 'Presence de Alexandrium par mois')+
  theme(axis.title.x = element_text(size=10), axis.text = element_text(size=10, color = 'black'), axis.title.y =element_text(size=10),
        legend.text = , legend.title = element_text(size = 10, face = 'bold'), 
        legend.box.margin = margin(0, 0, 0, 0, 'cm'), legend.background = element_rect(color = 'gray10'),
        legend.position = 'bottom',
        panel.grid.major = element_line(color = 'white', size = .5))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('Phenologie_Alex_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')





###### Data a long terme #####
# Import data
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

data$cluster <- as.factor(data$cluster)
cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF")

# Carte des stations
Worldmap <- map_data('worldHires')

data$lon <- as.numeric(data$lon)
data$lat <- as.numeric(data$lat)

ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)')+
  theme_gdocs()+
  geom_point(data = data, aes(x = lon, y = lat,colour=cluster), size =8)+
  scale_colour_manual(values = cluster_col,name="Cluster")+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('maps_station_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description", dpi = 600, width = 400, height = 400, units = 'mm')


# Au niveau des clusters
ggplot(data)+
  geom_boxplot(aes(x=Month,y=TEMP,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution des temperatures en fonction des mois par cluster",
       x = "Mois", y = "Temperature")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('TEMP_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Month,y=SALI,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la salinite en fonction des mois par cluster",
       x = "Mois", y = "Salinite")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('SALI_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=NH4,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la concentration en NH4 en fonction des mois par cluster",
       x = "Mois", y = "Concentration en NH4")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,30, by = 5),limits = c(0,15))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('NH4_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=SIOH,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la concentration en SIOH en fonction des mois par cluster",
       x = "Mois", y = "Concentration en SIOH")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('SIOH_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=PO4,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la concentration en PO4 en fonction des mois par cluster",
       x = "Mois", y = "Concentration en PO4")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('PO4_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=OXYGENE,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la concentration en oxygene en fonction des mois par cluster",
       x = "Mois", y = "Concentration en oxygene")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('OXYGENE_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=TURB,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la turbidite (NTU) en fonction des mois par cluster",
       x = "Mois", y = "Turbidite (NTU)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,150, by = 15))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('TURB_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=`TURB-FNU`,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la turbidite (FNU) en fonction des mois par cluster",
       x = "Mois", y = "Turbidite (FNU)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,220, by = 15))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('TURBFNU_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=`NO3+NO2`,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la concentration en NO3+NO2 en fonction des mois par cluster",
       x = "Mois", y = "Concentration en NO3+NO2")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,400, by = 20))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('NO3NO2_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=CHLOROA,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la concentration en chlorophylle en fonction des mois par cluster",
       x = "Mois", y = "Concentration en chl-a")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,220, by = 15))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('CHLOROA_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=log(Bacillariophyceae+1),group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Bacillariophyceae en fonction des mois par cluster",
       x = "Mois", y = "log(Abondance Bacillariphyceae+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('BACILLARIO_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=log(Dinophyceae+1),group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Dinophyceae en fonction des mois par cluster",
       x = "Mois", y = "log(Abondance Dinophyceae+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('DINOPHYCEAE_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=log(Ciliophora+1),group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Ciliophora en fonction des mois par cluster",
       x = "Mois", y = "log(Abondance Ciliophora+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('CILIOPHORA_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=log(Alexandrium+1),group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Alexandrium en fonction des mois par cluster",
       x = "Mois", y = "log(Abondance Alexandrium+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('Alex_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Month,y=log(Lepidodinium+1),group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Lepidodinium en fonction des mois par cluster",
       x = "Mois", y = "log(Abondance Lepidodinium+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('Lepido_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Month,y=log(Lingulodinium+1),group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Lingulodinium en fonction des mois par cluster",
       x = "Mois", y = "log(Abondance Lingulodinium+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('Lingu_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Month,y=log(Mesodinium+1),group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Mesodinium en fonction des mois par cluster",
       x = "Mois", y = "log(Abondance Mesodinium+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('Mesodinium_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Month,y=log(Noctiluca+1),group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Noctiluca en fonction des mois par cluster",
       x = "Mois", y = "log(Abondance Noctiluca+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('Noctiluca_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=Shannon,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'indice de Shannon en fonction des mois par cluster",
       x = "Mois", y = "Shannon index")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  #scale_y_continuous(breaks = seq(0,1, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('Shannon_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Month,y=Pielou,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'indice de Pielou en fonction des mois par cluster",
       x = "Mois", y = "Pielou index")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  #scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('Pielou_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=BergerParker,group = Month,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'indice de Berger-Parker en fonction des mois par cluster",
       x = "Mois", y = "Berger-Parker index")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(1:12))+
  #scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('Bergerparker_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')



ggplot(data)+
  geom_boxplot(aes(x=Year,y=TEMP,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution des temperatures en fonction des Annees par cluster",
       x = "Annee", y = "Temperature")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = seq(2007,2023,by=1))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('TEMP_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Year,y=SALI,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la salinite en fonction des Annees par cluster",
       x = "Annee", y = "Salinite")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(2007:2023))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('SALI_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=NH4,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la concentration en NH4 en fonction des Annees par cluster",
       x = "Annee", y = "Concentration en NH4")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(2007:2023))+
  scale_y_continuous(breaks = seq(0,30, by = 5),limits = c(0,15))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('NH4_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=SIOH,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la concentration en SIOH en fonction des Annees par cluster",
       x = "Annee", y = "Concentration en SIOH")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(2007:2023))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('SIOH_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=PO4,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la concentration en PO4 en fonction des Annees par cluster",
       x = "Annee", y = "Concentration en PO4")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(2007:2023))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('PO4_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=OXYGENE,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la concentration en oxygene en fonction des Annees par cluster",
       x = "Annee", y = "Concentration en oxygene")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(2007:2023))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('OXYGENE_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=TURB,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la turbidite (NTU) en fonction des Annees par cluster",
       x = "Annee", y = "Turbidite (NTU)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(2007:2023))+
  scale_y_continuous(breaks = seq(0,150, by = 15))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('TURB_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=`TURB-FNU`,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la turbidite (FNU) en fonction des Annees par cluster",
       x = "Annee", y = "Turbidite (FNU)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(2007:2023))+
  scale_y_continuous(breaks = seq(0,220, by = 15))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('TURBFNU_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=`NO3+NO2`,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la concentration en NO3+NO2 en fonction des Annees par cluster",
       x = "Annee", y = "Concentration en NO3+NO2")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(2007:2023))+
  scale_y_continuous(breaks = seq(0,400, by = 20))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('NO3NO2_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=CHLOROA,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de la concentration en chlorophylle en fonction des Annees par cluster",
       x = "Annee", y = "Concentration en chl-a")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(2007:2023))+
  scale_y_continuous(breaks = seq(0,220, by = 15))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('CHLOROA_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=log(Bacillariophyceae+1),group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Bacillariophyceae en fonction des Annee par cluster",
       x = "Annee", y = "log(Abondance Bacillariphyceae+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(2007:2023))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('BACILLARIO_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=log(Dinophyceae+1),group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Dinophyceae en fonction des Annees par cluster",
       x = "Annee", y = "log(Abondance Dinophyceae+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(2007:2023))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('DINOPHYCEAE_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=log(Ciliophora+1),group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Ciliophora en fonction des Annee par cluster",
       x = "Annee", y = "log(Abondance Ciliophora+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(2007:2023))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('CILIOPHORA_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=log(Alexandrium+1),group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Alexandrium en fonction des Annees par cluster",
       x = "Annee", y = "log(Abondance Alexandrium+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(2007:2023))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Alex_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Year,y=log(Lepidodinium+1),group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Lepidodinium en fonction des Annee par cluster",
       x = "Annee", y = "log(Abondance Lepidodinium+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(2007:2023))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Lepido_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Year,y=log(Lingulodinium+1),group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Lingulodinium en fonction des Annee par cluster",
       x = "Annee", y = "log(Abondance Lingulodinium+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(2007:2023))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Lingu_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Year,y=log(Mesodinium+1),group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Mesodinium en fonction des Annee par cluster",
       x = "Annee", y = "log(Abondance Mesodinium+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(2007:2023))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Mesodinium_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Year,y=log(Noctiluca+1),group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Noctiluca en fonction des Annees par cluster",
       x = "Annee", y = "log(Abondance Noctiluca+1)")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(2007:2023))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Noctiluca_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=Shannon,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'indice de Shannon en fonction des Annees par cluster",
       x = "Annee", y = "Shannon index")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(2007:2023))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Shannon_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Year,y=Pielou,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'indice de Pielou en fonction des Annees par cluster",
       x = "Annee", y = "Pielou index")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(2007:2023))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Pielou_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Year,y=BergerParker,group = Year,fill=cluster),linewidth = 1)+
  labs(title = "Evolution de l'indice de Berger-Parker en fonction des Annees par cluster",
       x = "Annee", y = "Berger-Parker index")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = c(2007:2023))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Bergerparker_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')




ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=TEMP,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Temperatures en fonction des stations des clusters",
       x = "Station", y = "Temperature",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,35, by = 5),limits = c(0,35))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('TEMP_Station_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=SALI,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Salinites en fonction des stations des clusters",
       x = "Station", y = "Salinite",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,35, by = 5),limits = c(0,35))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('SALI_Station_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=NH4,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Concentration en NH4 en fonction des stations des clusters",
       x = "Station", y = "NH4",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('NH4_Station_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=SIOH,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Concentration en SIOH en fonction des stations des clusters",
       x = "Station", y = "SIOH",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('SIOH_Station_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=PO4,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Concentration en PO4 en fonction des stations des clusters",
       x = "Station", y = "PO4",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('PO4_Station_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=OXYGENE,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Concentration en OXYGENE en fonction des stations des clusters",
       x = "Station", y = "OXYGENE",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('OXYGENE_Station_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=CHLOROA,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Concentration en CHLOROA en fonction des stations des clusters",
       x = "Station", y = "CHLOROA",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('CHLOROA_Station_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=`NO3+NO2`,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Concentration en NO3+NO2 en fonction des stations des clusters",
       x = "Station", y = "NO3+NO2",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('NO3NO2_Station_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=`TURB-FNU`,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Turbidite (FNU) en fonction des stations des clusters",
       x = "Station", y = "Turbidite (FNU)",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('TURBFNU_Station_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=TURB,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Turbidite (NTU) en fonction des stations des clusters",
       x = "Station", y = "Turbidite (NTU)",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('TURB_Station_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=log(Bacillariophyceae+1),group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Abondance en Bacillariophyceae en fonction des stations des clusters",
       x = "Station", y = "log(Bacillariophyceae+1)",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('BACILLARIO_Station_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=log(Dinophyceae+1),group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Abondance en Dinophyceae en fonction des stations des clusters",
       x = "Station", y = "log(Dinophyceae+1)",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('DINOPHYCEAE_Station_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=log(Ciliophora+1),group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Abondance en Ciliophora en fonction des stations des clusters",
       x = "Station", y = "log(Ciliophora+1)",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('CILIOPHORA_Station_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=log(Dinophysis+1),group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Abondance en Dinophysis en fonction des stations des clusters",
       x = "Station", y = "log(Dinophysis+1)",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Dino_Station_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=log(Noctiluca+1),group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Abondance en Noctiluca en fonction des stations des clusters",
       x = "Station", y = "log(Noctiluca+1)",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Noctiluca_Station_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=log(Mesodinium+1),group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Abondance en Mesodinium en fonction des stations des clusters",
       x = "Station", y = "log(Mesodinium+1)",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Mesodinium_Station_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=log(Lingulodinium+1),group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Abondance en Lingulodinium en fonction des stations des clusters",
       x = "Station", y = "log(Lingulodinium+1)",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Lingu_Station_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=log(Lepidodinium+1),group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Abondance en Lepidodinium en fonction des stations des clusters",
       x = "Station", y = "log(Lepidodinium+1)",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Lepidodinium_Station_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=log(Alexandrium+1),group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Abondance en Alexandrium en fonction des stations des clusters",
       x = "Station", y = "log(Alexandrium+1)",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Alexandrium_Station_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=Shannon,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Indice de Shannon en fonction des stations des clusters",
       x = "Station", y = "Shannon index",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Shannon_Station_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=Pielou,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Indice de Pielou en fonction des stations des clusters",
       x = "Station", y = "Pielou index",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('Pielou_Station_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Code_point_Libelle,y=BergerParker,group = Code_point_Libelle,fill= cluster,fill=cluster),linewidth = 1)+
  labs(title = "Indice de Berger-Parker en fonction des stations des clusters",
       x = "Station", y = "Berger-Parker index",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,25, by = 5),limits = c(0,25))+
  facet_wrap(~ cluster, nrow = 1, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 10))
ggsave('BergerParker_Station_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


dataforbox_hydro <- pivot_longer(data, names_to = "Variable",cols = c(CHLOROA:`TURB-FNU`))
dataforbox_hydro <- dplyr::select(dataforbox_hydro,Code.Region:Code.parametre,Variable,value)

dataforbox_phyto <- pivot_longer(data, names_to = "Variable",cols = c(Bacillariophyceae,Dinophyceae,Ciliophora,Dinophysis,Noctiluca,Mesodinium,Lingulodinium,Lepidodinium,Alexandrium))
dataforbox_phyto <- dplyr::select(dataforbox_phyto,Code.Region:Code.parametre,Variable,value)

dataforbox_div <- pivot_longer(data, names_to = "Variable",cols = c(Shannon,Pielou,BergerParker))
dataforbox_div <- dplyr::select(dataforbox_div,Code.Region:Code.parametre,Variable,value)

ggplot(dataforbox_hydro)+
  geom_boxplot(aes(y=value,x=Month,group = Month,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(title = "Parametres hydrologiques",
       x = "Mois", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,100, by = 5),limits = c(0,100))+
  scale_x_continuous(breaks = c(1:12))+
  facet_wrap(cluster ~ Variable, nrow = 4, scales="free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 7))
ggsave('Hydro_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(dataforbox_hydro)+
  geom_boxplot(aes(y=value,x=Year,group = Year,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(title = "Parametres hydrologiques",
       x = "Year", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,100, by = 5),limits = c(0,100))+
  scale_x_continuous(n.breaks = 25)+
  facet_wrap(cluster ~ Variable, nrow = 4, scales="free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 2))
ggsave('Hydro_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(dataforbox_phyto)+
  geom_boxplot(aes(y=log(value+1),x=Month,group = Month,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(title = "Phytoplancton",
       x = "Mois", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,20, by = 5),limits = c(0,20))+
  scale_x_continuous(breaks = c(1:12))+
  facet_wrap(cluster ~ Variable, nrow = 4)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 7))
ggsave('Phyto_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(dataforbox_phyto)+
  geom_boxplot(aes(y=log(value+1),x=Year,group = Year,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(title = "Phytoplancton",
       x = "Year", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,20, by = 5),limits = c(0,20))+
  scale_x_continuous(n.breaks = 25)+
  facet_wrap(cluster ~ Variable, nrow = 4, scales="free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 2))
ggsave('Phyto_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(dataforbox_div)+
  geom_boxplot(aes(y=log(value+1),x=Month,group = Month,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(title = "Indices de diversite",
       x = "Mois", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,20, by = 5),limits = c(0,20))+
  scale_x_continuous(breaks = c(1:12))+
  facet_wrap(cluster ~ Variable, nrow = 4)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 12))
ggsave('Div_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(dataforbox_div)+
  geom_boxplot(aes(y=log(value+1),x=Year,group = Year,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(title = "Indice de diversite",
       x = "Year", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1.5, by = 0.3),limits = c(0,1.5))+
  scale_x_continuous(breaks = seq(2007,2023, by = 1),limits = c(2007,2023))+
  facet_wrap(cluster ~ Variable, nrow = 5, scales="free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 9))
ggsave('Div_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

## Travailler sur les moyennes directement
# Mois 
data_clus_month <- group_by(data, cluster,Month)
data_clus_month_hydro <- summarise(data_clus_month, TEMP=mean(TEMP,na.rm=T),SALI=mean(SALI,na.rm=T),TURB=mean(TURB,na.rm=T),NH4=mean(NH4,na.rm=T),
               SIOH=mean(SIOH,na.rm=T),PO4=mean(PO4,na.rm=T),OXYGENE=mean(OXYGENE,na.rm=T),NO3NO2=mean(`NO3+NO2`,na.rm=T),
               TURBFNU=mean(`TURB-FNU`,na.rm=T),CHLOROA=mean(CHLOROA,na.rm=T))

dataforline_hydro <- pivot_longer(data_clus_month_hydro, names_to = "Variable",cols = c(TEMP:CHLOROA))
dataforline_hydro <- dplyr::select(dataforline_hydro,cluster,Month,Variable,value)


data_clus_month_phyto <- summarise(data_clus_month, Bacillariophyceae=mean(Bacillariophyceae,na.rm=T),Dinophyceae=mean(Dinophyceae,na.rm=T),Ciliophora=mean(Ciliophora,na.rm=T),Dinophysis=mean(Dinophysis,na.rm=T),
                                   Noctiluca=mean(Noctiluca,na.rm=T),Mesodinium=mean(Mesodinium,na.rm=T),Lingulodinium=mean(Lingulodinium,na.rm=T),Lepidodinium=mean(Lepidodinium,na.rm=T),
                                   Alexandrium=mean(Alexandrium,na.rm=T))

dataforline_phyto <- pivot_longer(data_clus_month_phyto, names_to = "Variable",cols = c(Bacillariophyceae:Alexandrium))
dataforline_phyto <- dplyr::select(dataforline_phyto,cluster,Month,Variable,value)

data_clus_month_div <- summarise(data_clus_month, Shannon=mean(Shannon,na.rm=T),Pielou=mean(Pielou,na.rm=T),BergerParker=mean(BergerParker,na.rm=T))

dataforline_div <- pivot_longer(data_clus_month_div, names_to = "Variable",cols = c(Shannon:BergerParker))
dataforline_div <- dplyr::select(dataforline_div,cluster,Month,Variable,value)


ggplot(dataforline_hydro)+
  geom_line(aes(y=value,x=Month,colour= cluster),linewidth = 2)+
  labs(title = "Hydrologie moyenne",
       x = "Mois", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,20, by = 5),limits = c(0,20))+
  scale_x_continuous(n.breaks = 12)+
  facet_wrap(cluster ~ Variable, nrow = 4, scales="free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 7))
ggsave('Hydro_Moy_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(dataforline_phyto)+
  geom_line(aes(y=log(value+1),x=Month,colour= cluster),linewidth = 2)+
  labs(title = "Abondance en phytoplancton moyenne",
       x = "Mois", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,20, by = 5),limits = c(0,20))+
  scale_x_continuous(n.breaks = 12)+
  facet_wrap(cluster ~ Variable, nrow = 4, scales="free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 7))
ggsave('Phyto_Moy_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(dataforline_div)+
  geom_line(aes(y=value,x=Month,colour= cluster),linewidth = 2)+
  labs(title = "Valeur indice de diversité moyen",
       x = "Mois", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,20, by = 5),limits = c(0,20))+
  scale_x_continuous(n.breaks = 12)+
  facet_wrap(cluster ~ Variable, nrow = 4, scales="free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 12))
ggsave('Div_Moy_Mois_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

# Annee
data_clus_Year <- group_by(data, cluster,Year)
data_clus_Year_hydro <- summarise(data_clus_Year, TEMP=mean(TEMP,na.rm=T),SALI=mean(SALI,na.rm=T),TURB=mean(TURB,na.rm=T),NH4=mean(NH4,na.rm=T),
                                   SIOH=mean(SIOH,na.rm=T),PO4=mean(PO4,na.rm=T),OXYGENE=mean(OXYGENE,na.rm=T),NO3NO2=mean(`NO3+NO2`,na.rm=T),
                                   TURBFNU=mean(`TURB-FNU`,na.rm=T),CHLOROA=mean(CHLOROA,na.rm=T))

dataforline_hydro <- pivot_longer(data_clus_Year_hydro, names_to = "Variable",cols = c(TEMP:CHLOROA))
dataforline_hydro <- dplyr::select(dataforline_hydro,cluster,Year,Variable,value)


data_clus_Year_phyto <- summarise(data_clus_Year, Bacillariophyceae=mean(Bacillariophyceae,na.rm=T),Dinophyceae=mean(Dinophyceae,na.rm=T),Ciliophora=mean(Ciliophora,na.rm=T),Dinophysis=mean(Dinophysis,na.rm=T),
                                   Noctiluca=mean(Noctiluca,na.rm=T),Mesodinium=mean(Mesodinium,na.rm=T),Lingulodinium=mean(Lingulodinium,na.rm=T),Lepidodinium=mean(Lepidodinium,na.rm=T),
                                   Alexandrium=mean(Alexandrium,na.rm=T))

dataforline_phyto <- pivot_longer(data_clus_Year_phyto, names_to = "Variable",cols = c(Bacillariophyceae:Alexandrium))
dataforline_phyto <- dplyr::select(dataforline_phyto,cluster,Year,Variable,value)

data_clus_Year_div <- summarise(data_clus_Year, Shannon=mean(Shannon,na.rm=T),Pielou=mean(Pielou,na.rm=T),BergerParker=mean(BergerParker,na.rm=T))

dataforline_div <- pivot_longer(data_clus_Year_div, names_to = "Variable",cols = c(Shannon:BergerParker))
dataforline_div <- dplyr::select(dataforline_div,cluster,Year,Variable,value)


ggplot(dataforline_hydro)+
  geom_line(aes(y=value,x=Year,colour= cluster),linewidth = 2)+
  labs(title = "Hydrologie moyenne",
       x = "Annee", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_colour_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,20, by = 5),limits = c(0,20))+
  scale_x_continuous(breaks = seq(2007,2023,by=1),limits=c(2007,2023))+
  facet_wrap(cluster ~ Variable, nrow = 4, scales="free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 4))
ggsave('Hydro_Moy_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(dataforline_phyto)+
  geom_line(aes(y=log(value+1),x=Year,colour= cluster),linewidth = 2)+
  labs(title = "Abondance en phytoplancton moyenne",
       x = "Annee", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_colour_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = seq(2007,2023,by=1),limits=c(2007,2023))+
  facet_wrap(cluster ~ Variable, nrow = 4, scales="free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 4))
ggsave('Phyto_Moy_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(dataforline_div)+
  geom_line(aes(y=value,x=Year,colour= cluster),linewidth = 2)+
  labs(title = "Valeur indice de diversité moyen",
       x = "Annee", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_colour_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = seq(2007,2023,by=1),limits=c(2007,2023))+
  facet_wrap(cluster ~ Variable, nrow = 4, scales="free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 4))
ggsave('Div_Moy_Annee_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


dataforline_hydro <- pivot_longer(data, names_to = "Variable",cols = c(CHLOROA:`TURB-FNU`))
dataforline_hydro <- dplyr::select(dataforline_hydro,cluster,Year,Variable,value)

dataforline_phyto <- pivot_longer(data, names_to = "Variable",cols = c(Bacillariophyceae,Dinophyceae,Ciliophora,Dinophysis,Noctiluca,Mesodinium,Lingulodinium,Lepidodinium,Alexandrium))
dataforline_phyto <- dplyr::select(dataforline_phyto,cluster,Year,Variable,value)

dataforline_div <- pivot_longer(data, names_to = "Variable",cols = c(Shannon:BergerParker))
dataforline_div <- dplyr::select(dataforline_div,cluster,Year,Variable,value)

ggplot(dataforline_hydro)+
  geom_boxplot(aes(x=cluster,y=value,group = cluster,fill=cluster),linewidth = 1)+
  labs(title = "",
       x = "Cluster", y = "Temperature")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_x_continuous(breaks = c(1:12))+
  #scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ Variable, nrow = 2,scales = "free")+
  theme_bw()
ggsave('Hydro_Clus_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(dataforline_phyto)+
  geom_boxplot(aes(x=cluster,y=log(value+1),group = cluster,fill=cluster),linewidth = 1)+
  labs(title = "",
       x = "Cluster", y = "Temperature")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_x_continuous(breaks = c(1:12))+
  #scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ Variable, nrow = 2,scales = "free")+
  theme_bw()
ggsave('Phyto_Clus_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(dataforline_div)+
  geom_boxplot(aes(x=cluster,y=log(value+1),group = cluster,fill=cluster),linewidth = 1)+
  labs(title = "",
       x = "Cluster", y = "Temperature")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_x_continuous(breaks = c(1:12))+
  #scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ Variable, nrow = 1,scales = "free")+
  theme_bw()
ggsave('Div_Clus_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')





# Test stats difference entre les clusters
kruskal.test(data$TEMP~data$cluster)
DunnTest(data$TEMP~data$cluster,method="BH") 

kruskal.test(data$SALI~data$cluster)
DunnTest(data$SALI~data$cluster,method="BH") 

kruskal.test(data$TURB~data$cluster)
DunnTest(data$TURB~data$cluster,method="BH") 

kruskal.test(data$NH4~data$cluster)
DunnTest(data$NH4~data$cluster,method="BH") 

kruskal.test(data$PO4~data$cluster)
DunnTest(data$PO4~data$cluster,method="BH") 

kruskal.test(data$SIOH~data$cluster)
DunnTest(data$SIOH~data$cluster,method="BH") 

kruskal.test(data$OXYGENE~data$cluster)
DunnTest(data$OXYGENE~data$cluster,method="BH") 

kruskal.test(data$CHLOROA~data$cluster)
DunnTest(data$CHLOROA~data$cluster,method="BH") 

kruskal.test(data$`NO3+NO2`~data$cluster)
DunnTest(data$`NO3+NO2`~data$cluster,method="BH") 

kruskal.test(data$`TURB-FNU`~data$cluster)
DunnTest(data$`TURB-FNU`~data$cluster,method="BH") 

kruskal.test(data$Bacillariophyceae~data$cluster)
DunnTest(data$Bacillariophyceae~data$cluster,method="BH") 

kruskal.test(data$Dinophyceae~data$cluster)
DunnTest(data$Dinophyceae~data$cluster,method="BH") 

kruskal.test(data$Ciliophora~data$cluster)
DunnTest(data$Ciliophora~data$cluster,method="BH") 

kruskal.test(data$Dinophysis~data$cluster)
DunnTest(data$Dinophysis~data$cluster,method="BH") 

kruskal.test(data$Alexandrium~data$cluster)
DunnTest(data$Alexandrium~data$cluster,method="BH") 

kruskal.test(data$Noctiluca~data$cluster)
DunnTest(data$Noctiluca~data$cluster,method="BH") 

kruskal.test(data$Mesodinium~data$cluster)
DunnTest(data$Mesodinium~data$cluster,method="BH") 

kruskal.test(data$Lingulodinium~data$cluster)
DunnTest(data$Lingulodinium~data$cluster,method="BH") 

kruskal.test(data$Lepidodinium~data$cluster)
DunnTest(data$Lepidodinium~data$cluster,method="BH") 

kruskal.test(data$Shannon~data$cluster)
DunnTest(data$Shannon~data$cluster,method="BH") 

kruskal.test(data$Pielou~data$cluster)
DunnTest(data$Pielou~data$cluster,method="BH") 

kruskal.test(data$BergerParker~data$cluster)
DunnTest(data$BergerParker~data$cluster,method="BH") 


# Difference de saisonnalite
Table.dino <- dplyr::select(data,cluster,Code_point_Libelle,Month,Dinophysis)
Table.dino[Table.dino== 0] = NA
Table.dino <- Table.dino[!is.na(Table.dino$Dinophysis),]

ggplot(Table.dino, aes(x = Month, y = cluster, fill = cluster))+
  geom_density_ridges(stat = 'binline', bins = 12, scale = 0.9,
                      #rel_min_height = .01,
                      draw_baseline = F, 
                      show.legend = TRUE)+
  scale_fill_manual(name = 'Cluster', values = cluster_col)+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_y_discrete(breaks = 10)+
  labs(x = 'Mois', y = 'Presence de Dinophysis par mois')+
  theme(axis.title.x = element_text(size=10), axis.text = element_text(size=10, color = 'black'), axis.title.y =element_text(size=10),
        legend.text = , legend.title = element_text(size = 10, face = 'bold'), 
        legend.box.margin = margin(0, 0, 0, 0, 'cm'), legend.background = element_rect(color = 'gray10'),
        legend.position = 'bottom',
        panel.grid.major = element_line(color = 'white', size = .5))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('Phenologie_Dino_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

Table.nocti <- dplyr::select(data,cluster,Code_point_Libelle,Month,Noctiluca)
Table.nocti[Table.nocti== 0] = NA
Table.nocti <- Table.nocti[!is.na(Table.nocti$Noctiluca),]

ggplot(Table.nocti, aes(x = Month, y = cluster, fill = cluster))+
  geom_density_ridges(stat = 'binline', bins = 12, scale = 0.9,
                      #rel_min_height = .01,
                      draw_baseline = F, 
                      show.legend = TRUE)+
  scale_fill_manual(name = 'Cluster', values = cluster_col)+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_y_discrete(breaks = 10)+
  labs(x = 'Mois', y = 'Presence de Noctiluca par mois')+
  theme(axis.title.x = element_text(size=10), axis.text = element_text(size=10, color = 'black'), axis.title.y =element_text(size=10),
        legend.text = , legend.title = element_text(size = 10, face = 'bold'), 
        legend.box.margin = margin(0, 0, 0, 0, 'cm'), legend.background = element_rect(color = 'gray10'),
        legend.position = 'bottom',
        panel.grid.major = element_line(color = 'white', size = .5))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('Phenologie_Nocti_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')



Table.meso <- dplyr::select(data,cluster,Code_point_Libelle,Month,Mesodinium)
Table.meso[Table.meso== 0] = NA
Table.meso <- Table.meso[!is.na(Table.meso$Mesodinium),]

ggplot(Table.meso, aes(x = Month, y = cluster, fill = cluster))+
  geom_density_ridges(stat = 'binline', bins = 12, scale = 0.9,
                      #rel_min_height = .01,
                      draw_baseline = F, 
                      show.legend = TRUE)+
  scale_fill_manual(name = 'Cluster', values = cluster_col)+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_y_discrete(breaks = 10)+
  labs(x = 'Mois', y = 'Presence de Mesodinium par mois')+
  theme(axis.title.x = element_text(size=10), axis.text = element_text(size=10, color = 'black'), axis.title.y =element_text(size=10),
        legend.text = , legend.title = element_text(size = 10, face = 'bold'), 
        legend.box.margin = margin(0, 0, 0, 0, 'cm'), legend.background = element_rect(color = 'gray10'),
        legend.position = 'bottom',
        panel.grid.major = element_line(color = 'white', size = .5))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('Phenologie_Meso_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

Table.lingu <- dplyr::select(data,cluster,Code_point_Libelle,Month,Lingulodinium)
Table.lingu[Table.lingu== 0] = NA
Table.lingu <- Table.lingu[!is.na(Table.lingu$Lingulodinium),]

ggplot(Table.lingu, aes(x = Month, y = cluster, fill = cluster))+
  geom_density_ridges(stat = 'binline', bins = 12, scale = 0.9,
                      #rel_min_height = .01,
                      draw_baseline = F, 
                      show.legend = TRUE)+
  scale_fill_manual(name = 'Cluster', values = cluster_col)+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_y_discrete(breaks = 10)+
  labs(x = 'Mois', y = 'Presence de Lingulodinium par mois')+
  theme(axis.title.x = element_text(size=10), axis.text = element_text(size=10, color = 'black'), axis.title.y =element_text(size=10),
        legend.text = , legend.title = element_text(size = 10, face = 'bold'), 
        legend.box.margin = margin(0, 0, 0, 0, 'cm'), legend.background = element_rect(color = 'gray10'),
        legend.position = 'bottom',
        panel.grid.major = element_line(color = 'white', size = .5))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('Phenologie_Lingu_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')

Table.lepido <- dplyr::select(data,cluster,Code_point_Libelle,Month,Lepidodinium)
Table.lepido[Table.lepido== 0] = NA
Table.lepido <- Table.lepido[!is.na(Table.lepido$Lepidodinium),]

ggplot(Table.lepido, aes(x = Month, y = cluster, fill = cluster))+
  geom_density_ridges(stat = 'binline', bins = 12, scale = 0.9,
                      #rel_min_height = .01,
                      draw_baseline = F, 
                      show.legend = TRUE)+
  scale_fill_manual(name = 'Cluster', values = cluster_col)+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_y_discrete(breaks = 10)+
  labs(x = 'Mois', y = 'Presence de Lepidodinium par mois')+
  theme(axis.title.x = element_text(size=10), axis.text = element_text(size=10, color = 'black'), axis.title.y =element_text(size=10),
        legend.text = , legend.title = element_text(size = 10, face = 'bold'), 
        legend.box.margin = margin(0, 0, 0, 0, 'cm'), legend.background = element_rect(color = 'gray10'),
        legend.position = 'bottom',
        panel.grid.major = element_line(color = 'white', size = .5))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('Phenologie_Lepido_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


Table.alex <- dplyr::select(data,cluster,Code_point_Libelle,Month,Alexandrium)
Table.alex[Table.alex== 0] = NA
Table.alex <- Table.alex[!is.na(Table.alex$Alexandrium),]

ggplot(Table.alex, aes(x = Month, y = cluster, fill = cluster))+
  geom_density_ridges(stat = 'binline', bins = 12, scale = 0.9,
                      #rel_min_height = .01,
                      draw_baseline = F, 
                      show.legend = TRUE)+
  scale_fill_manual(name = 'Cluster', values = cluster_col)+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_y_discrete(breaks = 10)+
  labs(x = 'Mois', y = 'Presence de Alexandrium par mois')+
  theme(axis.title.x = element_text(size=10), axis.text = element_text(size=10, color = 'black'), axis.title.y =element_text(size=10),
        legend.text = , legend.title = element_text(size = 10, face = 'bold'), 
        legend.box.margin = margin(0, 0, 0, 0, 'cm'), legend.background = element_rect(color = 'gray10'),
        legend.position = 'bottom',
        panel.grid.major = element_line(color = 'white', size = .5))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('Phenologie_Alex_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')



