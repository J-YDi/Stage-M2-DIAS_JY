# Import data
data <- read_delim("data_modif/Table_FLORTOT_Surf_9523_Stselect_hydro_phyto_chloro_phylum_period5_chlafilter_cluster.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)


# Au niveau des clusters
ggplot(data)+
  geom_boxplot(aes(x=Month,y=TEMP,group = Month,colour= Month),linewidth = 1)+
  labs(title = "Evolution des temperatures en fonction des mois par cluster",
       x = "Mois", y = "Temperature")+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,12, by = 1), limits = c(0,12), guide= "none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('TEMP_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 200, height = 180, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Month,y=SALI,group = Month,colour= Month),linewidth = 1)+
  labs(title = "Evolution de la salinite en fonction des mois par cluster",
       x = "Mois", y = "Salinite")+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,12, by = 1), limits = c(0,12), guide= "none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('SALI_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 200, height = 180, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=NH4,group = Month,colour= Month),linewidth = 1)+
  labs(title = "Evolution de la concentration en NH4 en fonction des mois par cluster",
       x = "Mois", y = "Concentration en NH4")+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,12, by = 1), limits = c(0,12), guide= "none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,30, by = 5),limits = c(0,15))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('NH4_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 200, height = 180, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=SIOH,group = Month,colour= Month),linewidth = 1)+
  labs(title = "Evolution de la concentration en SIOH en fonction des mois par cluster",
       x = "Mois", y = "Concentration en SIOH")+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,12, by = 1), limits = c(0,12), guide= "none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('SIOH_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 200, height = 180, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=PO4,group = Month,colour= Month),linewidth = 1)+
  labs(title = "Evolution de la concentration en PO4 en fonction des mois par cluster",
       x = "Mois", y = "Concentration en PO4")+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,12, by = 1), limits = c(0,12), guide= "none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('PO4_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 200, height = 180, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=OXYGENE,group = Month,colour= Month),linewidth = 1)+
  labs(title = "Evolution de la concentration en oxygene en fonction des mois par cluster",
       x = "Mois", y = "Concentration en oxygene")+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,12, by = 1), limits = c(0,12), guide= "none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,30, by = 5))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('OXYGENE_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 200, height = 180, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=TURB,group = Month,colour= Month),linewidth = 1)+
  labs(title = "Evolution de la turbidite (NTU) en fonction des mois par cluster",
       x = "Mois", y = "Turbidite (NTU)")+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,12, by = 1), limits = c(0,12), guide= "none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,150, by = 15))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('TURB_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 200, height = 180, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=`TURB-FNU`,group = Month,colour= Month),linewidth = 1)+
  labs(title = "Evolution de la turbidite (FNU) en fonction des mois par cluster",
       x = "Mois", y = "Turbidite (FNU)")+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,12, by = 1), limits = c(0,12), guide= "none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,220, by = 15))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('TURBFNU_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 200, height = 180, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=`NO3+NO2`,group = Month,colour= Month),linewidth = 1)+
  labs(title = "Evolution de la concentration en NO3+NO2 en fonction des mois par cluster",
       x = "Mois", y = "Concentration en NO3+NO2")+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,12, by = 1), limits = c(0,12), guide= "none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,400, by = 20))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('NO3NO2_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 200, height = 180, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=CHLOROA,group = Month,colour= Month),linewidth = 1)+
  labs(title = "Evolution de la concentration en chlorophylle en fonction des mois par cluster",
       x = "Mois", y = "Concentration en chl-a")+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,12, by = 1), limits = c(0,12), guide= "none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,220, by = 15))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('CHLOROA_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 200, height = 180, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=log(Bacillariophyceae+1),group = Month,colour= Month),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Bacillariophyceae en fonction des mois par cluster",
       x = "Mois", y = "log(Abondance Bacillariphyceae+1)")+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,12, by = 1), limits = c(0,12), guide= "none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('BACILLARIO_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 200, height = 180, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=log(Dinophyceae+1),group = Month,colour= Month),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Dinophyceae en fonction des mois par cluster",
       x = "Mois", y = "log(Abondance Dinophyceae+1)")+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,12, by = 1), limits = c(0,12), guide= "none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('DINOPHYCEAE_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 200, height = 180, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=log(Ciliophora+1),group = Month,colour= Month),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Ciliophora en fonction des mois par cluster",
       x = "Mois", y = "log(Abondance Ciliophora+1)")+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,12, by = 1), limits = c(0,12), guide= "none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('CILIOPHORA_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 200, height = 180, units = 'mm')

ggplot(data)+
  geom_boxplot(aes(x=Month,y=log(Alexandrium+1),group = Month,colour= Month),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Alexandrium en fonction des mois par cluster",
       x = "Mois", y = "log(Abondance Alexandrium+1)")+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,12, by = 1), limits = c(0,12), guide= "none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('Alex_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 200, height = 180, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Month,y=log(Lepidodinium+1),group = Month,colour= Month),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Lepidodinium en fonction des mois par cluster",
       x = "Mois", y = "log(Abondance Lepidodinium+1)")+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,12, by = 1), limits = c(0,12), guide= "none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('Lepido_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 200, height = 180, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Month,y=log(Lingulodinium+1),group = Month,colour= Month),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Lingulodinium en fonction des mois par cluster",
       x = "Mois", y = "log(Abondance Lingulodinium+1)")+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,12, by = 1), limits = c(0,12), guide= "none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('Lingu_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 200, height = 180, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Month,y=log(Mesodinium+1),group = Month,colour= Month),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Mesodinium en fonction des mois par cluster",
       x = "Mois", y = "log(Abondance Mesodinium+1)")+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,12, by = 1), limits = c(0,12), guide= "none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('Mesodinium_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 200, height = 180, units = 'mm')


ggplot(data)+
  geom_boxplot(aes(x=Month,y=log(Noctiluca+1),group = Month,colour= Month),linewidth = 1)+
  labs(title = "Evolution de l'abondance en Noctiluca en fonction des mois par cluster",
       x = "Mois", y = "log(Abondance Noctiluca+1)")+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,12, by = 1), limits = c(0,12), guide= "none")+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,20, by = 4))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()
ggsave('Noctiluca_Mois_ALLcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 200, height = 180, units = 'mm')



ggplot(dplyr::filter(data,cluster == 1))+
  geom_boxplot(aes(x=Code_point_Libelle,y=TEMP,group = Code_point_Libelle,colour= Code_point_Libelle),linewidth = 1)+
  labs(title = "Temperatures en fonction des stations du cluster 1",
       x = "Station", y = "Temperature",colour="Station")+
  scale_colour_discrete(guide= "none")+
  #scale_x_continuous(breaks = c(1:12))+
  scale_y_continuous(breaks = seq(0,35, by = 5),limits = c(0,35))+
  facet_wrap(~ cluster, nrow = 1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 15))
