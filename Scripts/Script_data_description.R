# Script description du jeu de donnees 
# DIAS JY

# Chargement package
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(viridis)
library(ggmap)
library(mapdata)
library(maps)
library(ggplot2)
library(ggthemes)
library(raster)
library(rasterVis)

# Chargement donnees
# Donnees hydro
Table <- read_delim("data_modif/Table_FLORTOT_Surf_9523_Stselect_hydro_phyto_chloro_phylum_period.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                        grouping_mark = ""), trim_ws = TRUE)



# Passage en presence/absence 
data_phyto_binaire <- ifelse(select(Table,Actinoptychus:Coscinodiscophycidae) > 0, 1, 0)
data_phyto_binaire <- as.data.frame(data_phyto_binaire)
data_phyto_binaire[is.na(data_phyto_binaire)] <- 0
data_phyto_binaire <- cbind(select(Table,Code.Region:Code.parametre),data_phyto_binaire)

##### Nombre de taxons detecte par prelevement ####

# Detecte
data_phyto_sum <- select(data_phyto_binaire,-(Code.Region:Code.parametre)) |>
  rowwise() |>
  mutate(Somme = sum(c_across(everything())))

data_phyto_binaire$N_detect <- data_phyto_sum$Somme
# Non detecte

data_phyto_sum_NA <- select(data_phyto_binaire,-(Code.Region:Code.parametre)) |>
  rowwise() |>
  mutate(N_nodetect = sum(c_across(everything()) == 0))

data_phyto_binaire$N_nodetect <- data_phyto_sum_NA$N_nodetect

# Verif par somme (inutile)
data_phyto_binaire_sumTax <- select(data_phyto_binaire,N_detect:N_nodetect) |>
  rowwise() |>
  mutate(N_tax = sum(c_across(everything())))

data_phyto_binaire$N_Tax <- data_phyto_binaire_sumTax$N_tax
# C'est ok

data_descript_data <- select(data_phyto_binaire,c(Code.Region:Code.parametre,N_detect:N_nodetect))

# Enregistrement
write.csv2(data_descript_data, file="data_modif/data_descript_data.csv")

# Representation graphique du nombre de taxon par station par region par date par mois
 
ggplot(filter(data_descript_data,data_descript_data$Code.Region == "11"))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = N_detect),size=5)+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,55, by = 10), limits = c(0,55))+
  facet_wrap(~Year)+
  labs(title = "Taxon detecte region 11", x= "Mois", y="Station",colour="Nombre",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('N_tax_11.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data,data_descript_data$Code.Region == "12"))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = N_detect),size=3)+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,55, by = 10), limits = c(0,55))+
  facet_wrap(~Year)+
  labs(title = "Taxon detecte region 12", x= "Mois", y="Station",colour="Nombre",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text(size=3))
ggsave('N_tax_12.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data,data_descript_data$Code.Region == "13"))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = N_detect),size=3)+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,55, by = 10), limits = c(0,55))+
  facet_wrap(~Year)+
  labs(title = "Taxon detecte region 13", x= "Mois", y="Station",colour="Nombre",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text(size=3))
ggsave('N_tax_13.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 600, height = 500, units = 'mm')

ggplot(filter(data_descript_data,data_descript_data$Code.Region == "21"))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = N_detect),size=3)+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,55, by = 10), limits = c(0,55))+
  facet_wrap(~Year)+
  labs(title = "Taxon detecte region 21", x= "Mois", y="Station",colour="Nombre",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text(size=3))
ggsave('N_tax_21.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 600, height = 500, units = 'mm')

ggplot(filter(data_descript_data,data_descript_data$Code.Region == "22"))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = N_detect),size=2)+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,55, by = 10), limits = c(0,55))+
  facet_wrap(~Year)+
  labs(title = "Taxon detecte region 22", x= "Mois", y="Station",colour="Nombre",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text(size=4))
ggsave('N_tax_22.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 600, height = 500, units = 'mm')

ggplot(filter(data_descript_data,data_descript_data$Code.Region == "23"))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = N_detect),size=3)+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,55, by = 10), limits = c(0,55))+
  facet_wrap(~Year)+
  labs(title = "Taxon detecte region 23", x= "Mois", y="Station",colour="Nombre",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
        #axis.text.y = element_text(size=6))
ggsave('N_tax_23.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data,data_descript_data$Code.Region == "31"))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = N_detect),size=3)+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,55, by = 10), limits = c(0,55))+
  facet_wrap(~Year)+
  labs(title = "Taxon detecte region 31", x= "Mois", y="Station",colour="Nombre",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text(size=6))
ggsave('N_tax_31.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 600, height = 500, units = 'mm')

ggplot(filter(data_descript_data,data_descript_data$Code.Region == "32"))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = N_detect),size=3)+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,55, by = 10), limits = c(0,55))+
  facet_wrap(~Year)+
  labs(title = "Taxon detecte region 32", x= "Mois", y="Station",colour="Nombre",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('N_tax_32.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 600, height = 500, units = 'mm')

# Moyenne et ecart-type par station par region

data_descript_data_station <- summarise(group_by(data_descript_data, Code_point_Libelle), mean_Ndetect=mean(N_detect,na.rm=T), sd_N_detect=sd(N_detect,na.rm=T))
data_descript_data_station <- left_join(data_descript_data_station,data_descript_data)

ggplot(filter(data_descript_data_station,data_descript_data$Code.Region == "32"))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,10, by = 5), limits = c(0,10))+
  scale_x_continuous(breaks = seq(0,30, by = 5), limits = c(0,30))+
  labs(title = "Moyenne Taxon detecte region 32", x= "Moyenne", y="Station",colour = "Nombre",size=0.5,
       subtitle = mean(filter(data_descript_data_station,data_descript_data$Code.Region == "32")$mean_Ndetect))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanN_tax_32.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 300, height = 200, units = 'mm')

ggplot(filter(data_descript_data_station,data_descript_data$Code.Region == "31"))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,10, by = 5), limits = c(0,10))+
  scale_x_continuous(breaks = seq(0,30, by = 5), limits = c(0,30))+
  labs(title = "Moyenne Taxon detecte region 31", x= "Moyenne", y="Station",colour = "Nombre",size=0.5,
       subtitle = mean(filter(data_descript_data_station,data_descript_data$Code.Region == "31")$mean_Ndetect))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanN_tax_31.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station,data_descript_data$Code.Region == "23"))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,10, by = 5), limits = c(0,10))+
  scale_x_continuous(breaks = seq(0,30, by = 5), limits = c(0,30))+
  labs(title = "Moyenne Taxon detecte region 23", x= "Moyenne", y="Station",colour = "Nombre",size=0.5,
       subtitle = mean(filter(data_descript_data_station,data_descript_data$Code.Region == "23")$mean_Ndetect))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanN_tax_23.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station,data_descript_data$Code.Region == "22"))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,10, by = 5), limits = c(0,10))+
  scale_x_continuous(breaks = seq(0,30, by = 5), limits = c(0,35))+
  labs(title = "Moyenne Taxon detecte region 22", x= "Moyenne", y="Station",colour = "Nombre",size=0.5,
       subtitle = mean(filter(data_descript_data_station,data_descript_data$Code.Region == "22")$mean_Ndetect))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanN_tax_22.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station,data_descript_data$Code.Region == "21"))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,10, by = 5), limits = c(0,10))+
  scale_x_continuous(breaks = seq(0,30, by = 5), limits = c(0,30))+
  labs(title = "Moyenne Taxon detecte region 21", x= "Moyenne", y="Station",colour = "Nombre",size=0.5,
       subtitle = mean(filter(data_descript_data_station,data_descript_data$Code.Region == "21")$mean_Ndetect))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanN_tax_21.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station,data_descript_data$Code.Region == "13"))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,10, by = 5), limits = c(0,10))+
  scale_x_continuous(breaks = seq(0,30, by = 5), limits = c(0,30))+
  labs(title = "Moyenne Taxon detecte region 13", x= "Moyenne", y="Station",colour = "Nombre",size=0.5,
       subtitle = mean(filter(data_descript_data_station,data_descript_data$Code.Region == "13")$mean_Ndetect))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanN_tax_13.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station,data_descript_data$Code.Region == "12"))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,10, by = 5), limits = c(0,10))+
  scale_x_continuous(breaks = seq(0,30, by = 5), limits = c(0,30))+
  labs(title = "Moyenne Taxon detecte region 12", x= "Moyenne", y="Station",colour = "Nombre",size=0.5,
       subtitle = mean(filter(data_descript_data_station,data_descript_data$Code.Region == "12")$mean_Ndetect))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanN_tax_12.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station,data_descript_data$Code.Region == "11"))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,10, by = 5), limits = c(0,10))+
  scale_x_continuous(breaks = seq(0,30, by = 5), limits = c(0,30))+
  labs(title = "Moyenne Taxon detecte region 11", x= "Moyenne", y="Station",colour = "Nombre",size=0.5,
       subtitle = mean(filter(data_descript_data_station,data_descript_data$Code.Region == "11")$mean_Ndetect))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanN_tax_11.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

# Nombre de station
length(levels(as.factor(data_descript_data$Code_point_Libelle)))
# Par region
length(levels(as.factor(filter(data_descript_data,data_descript_data$Code.Region == "11")$Code_point_Libelle)))
length(levels(as.factor(filter(data_descript_data,data_descript_data$Code.Region == "12")$Code_point_Libelle)))
length(levels(as.factor(filter(data_descript_data,data_descript_data$Code.Region == "13")$Code_point_Libelle)))
length(levels(as.factor(filter(data_descript_data,data_descript_data$Code.Region == "21")$Code_point_Libelle)))
length(levels(as.factor(filter(data_descript_data,data_descript_data$Code.Region == "22")$Code_point_Libelle)))
length(levels(as.factor(filter(data_descript_data,data_descript_data$Code.Region == "23")$Code_point_Libelle)))
length(levels(as.factor(filter(data_descript_data,data_descript_data$Code.Region == "31")$Code_point_Libelle)))
length(levels(as.factor(filter(data_descript_data,data_descript_data$Code.Region == "32")$Code_point_Libelle)))


##### Carte des stations ####
Table <- read_delim("data_modif/Table_FLORTOT_Surf_9523_Stselect_hydro_phyto_chloro_phylum_period.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                        grouping_mark = ""), trim_ws = TRUE)


Worldmap <- map_data('worldHires')

Table$lon <- as.numeric(Table$lon)
Table$lat <- as.numeric(Table$lat)

ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)')+
  theme_gdocs()+
  geom_point(data = Table, aes(x = lon, y = lat,colour=as.character(Code.Region)), size =4)+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))+
  scale_colour_discrete(name = "Code Region")
ggsave('maps_station_select.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description", dpi = 600, width = 200, height = 200, units = 'mm')



##### On regarde les coordonnees des stations pour trouver les stations mouvantes ####
# Autant le faire en carte
Table_R11 <- filter(Table, Code.Region == "11")
ggplot() + 
  geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(min(Table_R11$lon)-1,max(Table_R11$lon)+1), ylim=c(min(Table_R11$lat)-0.5,max(Table_R11$lat)+0.5), ratio=1.4)+
  geom_point(data=Table_R11 ,aes(y=lat,x=lon,colour=Code_point_Libelle),size=5)
ggsave('maps_R11_station.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description", dpi = 600, width = 200, height = 200, units = 'mm')

Table_R12 <- filter(Table, Code.Region == "12")
ggplot() + 
  geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(min(Table_R12$lon)-1,max(Table_R12$lon)+1), ylim=c(min(Table_R12$lat)-0.5,max(Table_R12$lat)+0.5), ratio=1.4)+
  geom_point(data=Table_R12 ,aes(y=lat,x=lon,colour=Code_point_Libelle),size=5)
ggsave('maps_R12_station.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description", dpi = 600, width = 200, height = 200, units = 'mm')


Table_R13 <- filter(Table, Code.Region == "13")
ggplot() + 
  geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(min(Table_R13$lon)-1,max(Table_R13$lon)+1), ylim=c(min(Table_R13$lat)-0.5,max(Table_R13$lat)+0.5), ratio=1.4)+
  geom_point(data=Table_R13 ,aes(y=lat,x=lon,colour=Code_point_Libelle),size=5)
ggsave('maps_R13_station.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description", dpi = 600, width = 200, height = 200, units = 'mm')


Table_R21 <- filter(Table, Code.Region == "21")
ggplot() + 
  geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(min(Table_R21$lon)-1,max(Table_R21$lon)+1), ylim=c(min(Table_R21$lat)-0.5,max(Table_R21$lat)+0.5), ratio=1.4)+
  geom_point(data=Table_R21 ,aes(y=lat,x=lon,colour=Code_point_Libelle),size=5)
ggsave('maps_R21_station.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description", dpi = 600, width = 200, height = 200, units = 'mm')
# OK

Table_R22 <- filter(Table, Code.Region == "22")
ggplot() + 
  geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(min(Table_R22$lon)-1,max(Table_R22$lon)+1), ylim=c(min(Table_R22$lat)-0.5,max(Table_R22$lat)+0.5), ratio=1.4)+
  geom_point(data=Table_R22 ,aes(y=lat,x=lon,colour=Code_point_Libelle),size=5)
ggsave('maps_R22_station.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description", dpi = 600, width = 200, height = 200, units = 'mm')
# OK

Table_R23 <- filter(Table, Code.Region == "23")
ggplot() + 
  geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(min(Table_R23$lon)-1,max(Table_R23$lon)+1), ylim=c(min(Table_R23$lat)-0.5,max(Table_R23$lat)+0.5), ratio=1.4)+
  geom_point(data=Table_R23 ,aes(y=lat,x=lon,colour=Code_point_Libelle),size=5)
ggsave('maps_R23_station.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description", dpi = 600, width = 200, height = 200, units = 'mm')

Table_R31 <- filter(Table, Code.Region == "31")
ggplot() + 
  geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(min(Table_R31$lon)-1,max(Table_R31$lon)+1), ylim=c(min(Table_R31$lat)-0.5,max(Table_R31$lat)+0.5), ratio=1.4)+
  geom_point(data=Table_R31 ,aes(y=lat,x=lon,colour=Code_point_Libelle),size=5)
ggsave('maps_R31_station.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description", dpi = 600, width = 200, height = 200, units = 'mm')
# OK 

Table_R32 <- filter(Table, Code.Region == "32")
ggplot() + 
  geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(min(Table_R32$lon)-1,max(Table_R32$lon)+1), ylim=c(min(Table_R32$lat)-0.5,max(Table_R32$lat)+0.5), ratio=1.4)+
  geom_point(data=Table_R32 ,aes(y=lat,x=lon,colour=Code_point_Libelle),size=5)
ggsave('maps_R32_station.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description", dpi = 600, width = 900, height = 800, units = 'mm')



# Carte des stations selectionnees selon les differents criteres ####
Table_Station_select <- read_delim("data_modif/Table_FLORTOT_S_select_5A.csv", 
                                     delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                                     trim_ws = TRUE)

Worldmap <- map_data('worldHires')

ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41.5,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)')+
  theme_gdocs()+
  geom_point(data = Table_Station_select, aes(x = lon, y = lat,colour=as.character(Code.Region)), size =5)+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))+
  scale_colour_discrete(name = "Code Region")
ggsave('maps_station_select5M.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description", dpi = 600, width = 200, height = 200, units = 'mm')

ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41.5,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)')+
  theme_gdocs()+
  geom_point(data = Table_St_select_20, aes(x = lon, y = lat,colour=as.character(Code.Region)), size =5)+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))+
  scale_colour_discrete(name = "Code Region")
ggsave('maps_station_select20A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description", dpi = 600, width = 200, height = 200, units = 'mm')


Table_Station_select <- read_delim("data_modif/Table_FLORTOT_S_select.csv", 
                                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                                   trim_ws = TRUE)

Worldmap <- map_data('worldHires')

ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41.5,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)')+
  theme_gdocs()+
  geom_point(data = Table_Station_select, aes(x = lon, y = lat,colour=as.character(Code.Region)), size =5)+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))+
  scale_colour_discrete(name = "Code Region")
ggsave('maps_station_select.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description", dpi = 600, width = 200, height = 200, units = 'mm')


#### Carte des stations selectionnes longues series par facade #####
# Manche
Table.Atlantic_select$lon <- as.numeric(Table.Atlantic_select$lon)
Table.Atlantic_select$lat <- as.numeric(Table.Atlantic_select$lat)
Table.Manche_select$lon <- as.numeric(Table.Manche_select$lon)
Table.Manche_select$lat <- as.numeric(Table.Manche_select$lat)
Table.Med_select$lon <- as.numeric(Table.Med_select$lon)
Table.Med_select$lat <- as.numeric(Table.Med_select$lat)

ggplot() + 
  geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(min(Table.Manche_select$lon)-1,max(Table.Manche_select$lon)+1), ylim=c(min(Table.Manche_select$lat)-0.5,max(Table.Manche_select$lat)+0.5), ratio=1.4)+
  geom_point(data=Table.Manche_select ,aes(y=lat,x=lon,colour=Code_point_Libelle,shape=Code_point_Libelle),size=5)+
  scale_shape_manual(values = rep(c(15, 16, 17),10))
ggsave('maps_Manche_station_select_5.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description", dpi = 600, width = 300, height = 200, units = 'mm')

ggplot() + 
  geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(min(Table.Atlantic_select$lon)-1,max(Table.Atlantic_select$lon)+1), ylim=c(min(Table.Atlantic_select$lat)-0.5,max(Table.Atlantic_select$lat)+0.5), ratio=1.4)+
  geom_point(data=Table.Atlantic_select ,aes(y=lat,x=lon,colour=Code_point_Libelle, shape = Code_point_Libelle),size=5)+
  scale_shape_manual(values = rep(c(15, 16, 17),10))
ggsave('maps_Atlantic_station_select_5.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description", dpi = 600, width = 300, height = 200, units = 'mm')

ggplot() + 
  geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(min(Table.Med_select$lon)-1,max(Table.Med_select$lon)+1), ylim=c(min(Table.Med_select$lat)-0.5,max(Table.Med_select$lat)+0.5), ratio=1.4)+
  geom_point(data=Table.Med_select ,aes(y=lat,x=lon,colour=Code_point_Libelle, shape =Code_point_Libelle),size=5)+
  scale_shape_manual(values = rep(c(15, 16, 17),10))
ggsave('maps_Med_station_select_5.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description", dpi = 600, width = 300, height = 200, units = 'mm')


# Nombre de taxons detectes uniquement pour les stations selectionnees 5A>
Table <- read_delim("data_modif/Table_FLORTOT_Surf_9523_Stselect_hydro_phyto_chloro_phylum_period.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                        grouping_mark = ""), trim_ws = TRUE)




# Passage en presence/absence 
data_phyto_binaire <- ifelse(select(Table,Actinoptychus:Coscinodiscophycidae) > 0, 1, 0)
data_phyto_binaire <- as.data.frame(data_phyto_binaire)
data_phyto_binaire[is.na(data_phyto_binaire)] <- 0
data_phyto_binaire <- cbind(select(Table,Code.Region:Code.parametre),data_phyto_binaire)

##### Nombre de taxons detecte par prelevement ####

# Detecte
data_phyto_sum <- select(data_phyto_binaire,-(Code.Region:Code.parametre)) |>
  rowwise() |>
  mutate(Somme = sum(c_across(everything())))

data_phyto_binaire$N_detect <- data_phyto_sum$Somme
# Non detecte

data_phyto_sum_NA <- select(data_phyto_binaire,-(Code.Region:Code.parametre)) |>
  rowwise() |>
  mutate(N_nodetect = sum(c_across(everything()) == 0))

data_phyto_binaire$N_nodetect <- data_phyto_sum_NA$N_nodetect

# Verif par somme (inutile)
data_phyto_binaire_sumTax <- select(data_phyto_binaire,N_detect:N_nodetect) |>
  rowwise() |>
  mutate(N_tax = sum(c_across(everything())))

data_phyto_binaire$N_Tax <- data_phyto_binaire_sumTax$N_tax
# C'est ok

data_descript_data <- select(data_phyto_binaire,c(Code.Region:Code.parametre,N_detect:N_nodetect))

# Enregistrement
write.csv2(data_descript_data, file="data_modif/data_descript_data_station_select5A.csv")

# Representation graphique du nombre de taxon par station par region par date par mois

# Moyenne et ecart-type par station par region
data_descript_data_select <- data_descript_data
data_descript_data_station_select <- summarise(group_by(data_descript_data_select, Code_point_Libelle), mean_Ndetect=mean(N_detect,na.rm=T), sd_N_detect=sd(N_detect,na.rm=T),min_N_detect=min(N_detect,na.rm=T),max_N_detect=max(N_detect,na.rm=T))
data_descript_data_station_select <- left_join(data_descript_data_station_select,data_descript_data_select)

ggplot(filter(data_descript_data_select,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = N_detect),size=5)+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,55, by = 10), limits = c(0,55))+
  facet_wrap(~Year)+
  labs(title = "Taxon detecte region Manche", x= "Mois", y="Station",colour="Nombre",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('N_tax_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_select,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = N_detect),size=5)+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,55, by = 10), limits = c(0,55))+
  facet_wrap(~Year)+
  labs(title = "Taxon detecte region Atlantique", x= "Mois", y="Station",colour="Nombre",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('N_tax_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_descript_data_select,Code.Region %in% c(31,32)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = N_detect),size=3)+
  scale_colour_viridis(name = "N_detect",breaks = seq(0,55, by = 10), limits = c(0,55))+
  facet_wrap(~Year)+
  labs(title = "Taxon detecte region Mediterranee", x= "Mois", y="Station",colour="Nombre",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('N_tax_Mediterranee_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')




ggplot(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,10, by = 5), limits = c(0,10))+
  scale_x_continuous(breaks = seq(0,55, by = 5), limits = c(0,55))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Taxon detecte region Manche", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanN_tax_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')



ggplot(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,10, by = 5), limits = c(0,10))+
  scale_x_continuous(breaks = seq(0,55, by = 5), limits = c(0,55))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Taxon detecte region Atlantique", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanN_tax_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(31,32)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,10, by = 5), limits = c(0,10))+
  scale_x_continuous(breaks = seq(0,55, by = 5), limits = c(0,55))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Taxon detecte region Mediterranee", x= "valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanN_tax_Med_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')



################# SALINITE ####### 
Table <- read_delim("data_modif/Table_FLORTOT_Surf_9523_Stselect_hydro_phyto_chloro_phylum_period.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                        grouping_mark = ""), trim_ws = TRUE)



data_sali <- select(Table, Code.Region:Code.parametre, SALI)

ggplot(filter(data_sali,Code.Region %in% c(31,32)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = SALI),size=3)+
  scale_colour_viridis(name = "Salinite",breaks = seq(0,50, by = 10), limits = c(0,50))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "Salinite region Mediterranee", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('SALI_Mediterranee_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_sali,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = SALI),size=3)+
  scale_colour_viridis(name = "Salinite",breaks = seq(0,50, by = 10), limits = c(0,50))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "Salinite region Atlantique", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('SALI_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_sali,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = SALI),size=3)+
  scale_colour_viridis(name = "Salinite",breaks = seq(0,50, by = 10), limits = c(0,50))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "Salinite region Manche", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('SALI_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')


Table <- select(data_sali, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(31,32))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:17]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1995,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1995,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(15.5,29),
           label = c(seq(1995,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_SALI_Med1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(15.5,29),
           label = c(seq(1995,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_SALI_Med2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')



Table <- dplyr::select(data_sali, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(21,22,23))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:21]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1995,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1995,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(19.5,29),
           label = c(seq(1995,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_SALI_Atlantic1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(19.5,29),
           label = c(seq(1995,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_SALI_Atlantic2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


Table <- dplyr::select(data_sali, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(11,12,13))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:16]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1995,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1995,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(14.5,29),
           label = c(seq(1995,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_SALI_Manche1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(14.5,29),
           label = c(seq(1995,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_SALI_Manche2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')




data_descript_data_station_select <- summarise(group_by(data_sali, Code_point_Libelle), mean_Ndetect=mean(SALI,na.rm=T), sd_N_detect=sd(SALI,na.rm=T),min_N_detect=min(SALI,na.rm=T),max_N_detect=max(SALI,na.rm=T))
data_descript_data_station_select <- left_join(data_descript_data_station_select,data_sali)

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,6, by = 1), limits = c(0,6))+
  scale_x_continuous(breaks = seq(0,40, by = 5), limits = c(0,40))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Salinite region Manche", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanSALI_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,6, by = 1), limits = c(0,6))+
  scale_x_continuous(breaks = seq(0,40, by = 5), limits = c(0,40))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Salinite region Atlantique", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanSALI_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(31,32)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,6, by = 1), limits = c(0,6))+
  scale_x_continuous(breaks = seq(0,40, by = 5), limits = c(0,40))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Salinite region Mediterranee", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanSALI_Med_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')


################# TURBIDITE ######
Table <- read_delim("data_modif/Table_FLORTOT_Surf_9523_Stselect_hydro_phyto_chloro_phylum_period.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                        grouping_mark = ""), trim_ws = TRUE)

data_turb <- dplyr::select(Table, Code.Region:Code.parametre,TURB)

ggplot(filter(data_turb,Code.Region %in% c(31,32)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = TURB),size=3)+
  scale_colour_viridis(name = "Turbidite",breaks = seq(0,139, by = 10), limits = c(0,139))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "Turbidite region Mediterranee", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('TURB_Mediterranee_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_turb,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = TURB),size=3)+
  scale_colour_viridis(name = "Turbidite",breaks = seq(0,139, by = 10), limits = c(0,139))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "Turbidite region Atlantique", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('TURB_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_turb,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = TURB),size=3)+
  scale_colour_viridis(name = "Turbidite",breaks = seq(0,139, by = 10), limits = c(0,139))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "Turbidite region Manche", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('TURB_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')


Table <- dplyr::select(data_turb, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(31,32))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:15]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1995,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1995,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(13.5,29),
           label = c(seq(1995,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_TURB_Med1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(13.5,29),
           label = c(seq(1995,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_TURB_Med2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')



Table <- dplyr::select(data_turb, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(21,22,23))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:15]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1995,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1995,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(13.5,29),
           label = c(seq(1995,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_TURB_Atlantic1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(13.5,29),
           label = c(seq(1995,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_TURB_Atlantic2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


Table <- dplyr::select(data_turb, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(11,12,13))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:15]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1995,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1995,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(13.5,29),
           label = c(seq(1995,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_TURB_Manche1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(13.5,29),
           label = c(seq(1995,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_TURB_Manche2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


Table <- dplyr::select(data_turb, -c(Year,Month))
data_turb <- Table[complete.cases(Table),]

data_descript_data_station_select <- summarise(group_by(data_turb, Code_point_Libelle), mean_Ndetect=mean(TURB,na.rm=T), sd_N_detect=sd(TURB,na.rm=T),min_N_detect=min(TURB,na.rm=T),max_N_detect=max(TURB,na.rm=T))
data_descript_data_station_select <- left_join(data_descript_data_station_select,data_turb)

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,25, by = 5), limits = c(0,25))+
  scale_x_continuous(breaks = seq(0,140, by = 5), limits = c(0,140))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Turbidite region Manche", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanTURB_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,20, by = 5), limits = c(0,20))+
  scale_x_continuous(breaks = seq(0,65, by = 5), limits = c(0,65))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne TURB region Atlantique", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanTURB_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(31,32)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,20, by = 5), limits = c(0,20))+
  scale_x_continuous(breaks = seq(0,65, by = 5), limits = c(0,65))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne TURB region Mediterranee", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanTURB_Med_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')


################# NH4 #######
data_NH4 <- select(data_hp, Code.Region:Code.parametre,NH4)

ggplot(filter(data_NH4,Code.Region %in% c(31,32)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = NH4),size=3)+
  scale_colour_viridis(name = "NH4",breaks = seq(0,22, by = 5), limits = c(0,22))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "NH4 region Mediterranee", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('NH4_Mediterranee_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_NH4,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = NH4),size=3)+
  scale_colour_viridis(name = "NH4",breaks = seq(0,10, by = 5), limits = c(0,10))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "NH4 region Atlantique", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('NH4_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_NH4,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = NH4),size=3)+
  scale_colour_viridis(name = "NH4",breaks = seq(0,30, by = 10), limits = c(0,30))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "NH4 region Manche", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('NH4_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')


Table <- select(data_NH4, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(31,32))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:13]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1987,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1987,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(11.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_NH4_Med1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(11.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_NH4_Med2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')



Table <- select(data_NH4, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(21,22,23))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:16]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1987,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1987,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(14.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_NH4_Atlantic1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(14.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_NH4_Atlantic2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


Table <- select(data_NH4, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(11,12,13))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:13]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1987,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1987,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(11.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_NH4_Manche1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(11.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_NH4_Manche2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


Table <- select(data_NH4, -c(Year,Month))
data_NH4 <- Table[complete.cases(Table),]

data_descript_data_station_select <- summarise(group_by(data_NH4, Code_point_Libelle), mean_Ndetect=mean(NH4,na.rm=T), sd_N_detect=sd(NH4,na.rm=T),min_N_detect=min(NH4,na.rm=T),max_N_detect=max(NH4,na.rm=T))
data_descript_data_station_select <- left_join(data_descript_data_station_select,data_NH4)

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,20, by = 5), limits = c(0,20))+
  scale_x_continuous(breaks = seq(0,20, by = 5), limits = c(0,20))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne NH4 region Manche", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanNH4_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,20, by = 5), limits = c(0,20))+
  scale_x_continuous(breaks = seq(0,20, by = 5), limits = c(0,20))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne NH4 region Atlantique", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanNH4_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(31,32)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,20, by = 5), limits = c(0,20))+
  scale_x_continuous(breaks = seq(0,20, by = 5), limits = c(0,20))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne NH4 region Mediterranee", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanNH4_Med_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')


################# PO4 #######
data_PO4 <- select(data_hp, Code.Region:Code.parametre,PO4)

ggplot(filter(data_PO4,Code.Region %in% c(31,32)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = PO4),size=3)+
  scale_colour_viridis(name = "PO4",breaks = seq(0,5, by = 0.5), limits = c(0,5))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "PO4 region Mediterranee", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('PO4_Mediterranee_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_PO4,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = PO4),size=3)+
  scale_colour_viridis(name = "PO4",breaks = seq(0,5, by = 0.5), limits = c(0,5))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "PO4 region Atlantique", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('PO4_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_PO4,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = PO4),size=3)+
  scale_colour_viridis(name = "PO4",breaks = seq(0,5, by = 0.5), limits = c(0,5))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "PO4 region Manche", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('PO4_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')


Table <- select(data_PO4, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(31,32))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:13]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1987,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1987,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(11.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_PO4_Med1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(11.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_PO4_Med2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')



Table <- select(data_PO4, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(21,22,23))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:16]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1987,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1987,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(14.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_PO4_Atlantic1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(14.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_PO4_Atlantic2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


Table <- select(data_PO4, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(11,12,13))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:13]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1987,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1987,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(11.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_PO4_Manche1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(11.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_PO4_Manche2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


Table <- select(data_PO4, -c(Year,Month))
data_PO4 <- Table[complete.cases(Table),]

data_descript_data_station_select <- summarise(group_by(data_PO4, Code_point_Libelle), mean_Ndetect=mean(PO4,na.rm=T), sd_N_detect=sd(PO4,na.rm=T),min_N_detect=min(PO4,na.rm=T),max_N_detect=max(PO4,na.rm=T))
data_descript_data_station_select <- left_join(data_descript_data_station_select,data_PO4)

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,1, by = 0.5), limits = c(0,1))+
  scale_x_continuous(breaks = seq(0,5, by = 0.5), limits = c(0,5))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne PO4 region Manche", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanPO4_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,1, by = 0.5), limits = c(0,1))+
  scale_x_continuous(breaks = seq(0,5, by = 0.5), limits = c(0,5))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne PO4 region Atlantique", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanPO4_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(31,32)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,1, by = 0.5), limits = c(0,1))+
  scale_x_continuous(breaks = seq(0,5, by = 0.5), limits = c(0,5))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne PO4 region Mediterranee", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanPO4_Med_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

################# SIOH #######
data_SIOH <- select(data_hp, Code.Region:Code.parametre,SIOH)

ggplot(filter(data_SIOH,Code.Region %in% c(31,32)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = SIOH),size=3)+
  scale_colour_viridis(name = "SIOH",breaks = seq(0,146, by = 10), limits = c(0,146))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "SIOH region Mediterranee", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('SIOH_Mediterranee_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_SIOH,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = SIOH),size=3)+
  scale_colour_viridis(name = "SIOH",breaks = seq(0,146, by = 10), limits = c(0,136))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "SIOH region Atlantique", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('SIOH_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_SIOH,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = SIOH),size=3)+
  scale_colour_viridis(name = "SIOH",breaks = seq(0,146, by = 10), limits = c(0,146))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "SIOH region Manche", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('SIOH_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')


Table <- select(data_SIOH, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(31,32))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:13]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1987,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1987,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(11.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_SIOH_Med1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(11.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_SIOH_Med2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')



Table <- select(data_SIOH, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(21,22,23))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:16]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1987,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1987,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(14.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_SIOH_Atlantic1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(14.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_SIOH_Atlantic2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


Table <- select(data_SIOH, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(11,12,13))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:13]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1987,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1987,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(11.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_SIOH_Manche1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(11.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_SIOH_Manche2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


Table <- select(data_SIOH, -c(Year,Month))
data_SIOH <- Table[complete.cases(Table),]

data_descript_data_station_select <- summarise(group_by(data_SIOH, Code_point_Libelle), mean_Ndetect=mean(SIOH,na.rm=T), sd_N_detect=sd(SIOH,na.rm=T),min_N_detect=min(SIOH,na.rm=T),max_N_detect=max(SIOH,na.rm=T))
data_descript_data_station_select <- left_join(data_descript_data_station_select,data_SIOH)

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,20, by = 5), limits = c(0,20))+
  scale_x_continuous(breaks = seq(0,138, by = 10), limits = c(0,138))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne SIOH region Manche", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanSIOH_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,20, by = 5), limits = c(0,20))+
  scale_x_continuous(breaks = seq(0,138, by = 10), limits = c(0,138))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne SIOH region Atlantique", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanSIOH_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(31,32)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,20, by = 5), limits = c(0,20))+
  scale_x_continuous(breaks = seq(0,138, by = 10), limits = c(0,138))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne SIOH region Mediterranee", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanSIOH_Med_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

################# OXYGENE #######
data_OXYGENE <- select(data_hp, Code.Region:Code.parametre,OXYGENE)

ggplot(filter(data_OXYGENE,Code.Region %in% c(31,32)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = OXYGENE),size=3)+
  scale_colour_viridis(name = "OXYGENE",breaks = seq(0,24, by = 5), limits = c(0,24))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "OXYGENE region Mediterranee", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('OXYGENE_Mediterranee_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_OXYGENE,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = OXYGENE),size=3)+
  scale_colour_viridis(name = "OXYGENE",breaks = seq(0,24, by = 5), limits = c(0,24))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "OXYGENE region Atlantique", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('OXYGENE_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_OXYGENE,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = OXYGENE),size=3)+
  scale_colour_viridis(name = "OXYGENE",breaks = seq(0,24, by = 5), limits = c(0,24))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "OXYGENE region Manche", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('OXYGENE_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')


Table <- select(data_OXYGENE, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(31,32))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:18]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1987,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1987,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(16.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_OXYGENE_Med1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(11.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_OXYGENE_Med2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')



Table <- select(data_OXYGENE, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(21,22,23))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:21]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1987,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1987,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(19.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_OXYGENE_Atlantic1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(14.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_OXYGENE_Atlantic2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


Table <- select(data_OXYGENE, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(11,12,13))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:16]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1987,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1987,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(14.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_OXYGENE_Manche1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(14.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_OXYGENE_Manche2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


Table <- select(data_OXYGENE, -c(Year,Month))
data_OXYGENE <- Table[complete.cases(Table),]

data_descript_data_station_select <- summarise(group_by(data_OXYGENE, Code_point_Libelle), mean_Ndetect=mean(OXYGENE,na.rm=T), sd_N_detect=sd(OXYGENE,na.rm=T),min_N_detect=min(OXYGENE,na.rm=T),max_N_detect=max(OXYGENE,na.rm=T))
data_descript_data_station_select <- left_join(data_descript_data_station_select,data_OXYGENE)

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,3, by = 1), limits = c(0,3))+
  scale_x_continuous(breaks = seq(0,24, by = 5), limits = c(0,24))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne OXYGENE region Manche", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanOXYGENE_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,3, by = 1), limits = c(0,3))+
  scale_x_continuous(breaks = seq(0,24, by = 5), limits = c(0,24))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne OXYGENE region Atlantique", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanOXYGENE_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(31,32)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,3, by = 1), limits = c(0,3))+
  scale_x_continuous(breaks = seq(0,24, by = 5), limits = c(0,24))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne OXYGENE region Mediterranee", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanOXYGENE_Med_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

################# TURB-FNU #######
data_TURBFNU <- select(data_hp, Code.Region:Code.parametre,`TURB-FNU`)

colnames(data_TURBFNU)[11] <- "TURBFNU"

ggplot(filter(data_TURBFNU,Code.Region %in% c(31,32)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = TURBFNU),size=3)+
  scale_colour_viridis(name = "TURBFNU",breaks = seq(-2,90, by = 15), limits = c(-2,90))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "TURBFNU region Mediterranee", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('TURBFNU_Mediterranee_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_TURBFNU,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = TURBFNU),size=3)+
  scale_colour_viridis(name = "TURBFNU",breaks = seq(-2,90, by = 15), limits = c(-2,90))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "TURBFNU region Atlantique", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('TURBFNU_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_TURBFNU,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = TURBFNU),size=3)+
  scale_colour_viridis(name = "TURBFNU",breaks = seq(-2,90, by = 15), limits = c(-2,90))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "TURBFNU region Manche", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('TURBFNU_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')


Table <- select(data_TURBFNU, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(31,32))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:16]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1987,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1987,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(14.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_TURBFNU_Med1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(14.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_TURBFNU_Med2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')



Table <- select(data_TURBFNU, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(21,22,23))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:23]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1987,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1987,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(21.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_TURBFNU_Atlantic1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(21.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_TURBFNU_Atlantic2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


Table <- select(data_TURBFNU, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(11,12,13))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:16]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1987,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1987,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(14.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_TURBFNU_Manche1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(14.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_TURBFNU_Manche2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


Table <- select(data_TURBFNU, -c(Year,Month))
data_TURBFNU <- Table[complete.cases(Table),]

data_descript_data_station_select <- summarise(group_by(data_TURBFNU, Code_point_Libelle), mean_Ndetect=mean(TURBFNU,na.rm=T), sd_N_detect=sd(TURBFNU,na.rm=T),min_N_detect=min(TURBFNU,na.rm=T),max_N_detect=max(TURBFNU,na.rm=T))
data_descript_data_station_select <- left_join(data_descript_data_station_select,data_TURBFNU)

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,35, by = 1), limits = c(0,35))+
  scale_x_continuous(breaks = seq(0,65, by = 5), limits = c(0,65))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne TURBFNU region Manche", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanTURBFNU_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,35, by = 1), limits = c(0,35))+
  scale_x_continuous(breaks = seq(0,85, by = 5), limits = c(0,85))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne TURBFNU region Atlantique", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanTURBFNU_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(31,32)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,35, by = 1), limits = c(0,35))+
  scale_x_continuous(breaks = seq(0,190, by = 5), limits = c(0,190))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne TURBFNU region Mediterranee", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanTURBFNU_Med_select5A_1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(31,32)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,35, by = 1), limits = c(0,35))+
  scale_x_continuous(breaks = seq(0,55, by = 5), limits = c(0,55))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne TURBFNU region Mediterranee", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanTURBFNU_Med_select5A_2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')


################# NO3NO2 #######
data_NO3NO2 <- select(data_hp, Code.Region:Code.parametre,`NO3+NO2`)

colnames(data_NO3NO2)[11] <- "NO3NO2"

ggplot(filter(data_NO3NO2,Code.Region %in% c(31,32)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = NO3NO2),size=3)+
  scale_colour_viridis(name = "NO3NO2",breaks = seq(0,245, by = 15), limits = c(0,245))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "NO3NO2 region Mediterranee", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('NO3NO2_Mediterranee_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_NO3NO2,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = NO3NO2),size=3)+
  scale_colour_viridis(name = "NO3NO2",breaks = seq(0,245, by = 15), limits = c(0,245))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "NO3NO2 region Atlantique", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('NO3NO2_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_NO3NO2,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = NO3NO2),size=3)+
  scale_colour_viridis(name = "NO3NO2",breaks = seq(0,245, by = 15), limits = c(0,245))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "NO3NO2 region Manche", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('NO3NO2_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')


Table <- select(data_NO3NO2, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(31,32))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:13]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1987,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1987,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(11.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_NO3NO2_Med1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(11.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_NO3NO2_Med2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')



Table <- select(data_NO3NO2, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(21,22,23))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:16]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1987,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1987,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(14.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_NO3NO2_Atlantic1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(14.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_NO3NO2_Atlantic2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


Table <- select(data_NO3NO2, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(11,12,13))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:13]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1987,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1987,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(11.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_NO3NO2_Manche1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(11.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_NO3NO2_Manche2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


Table <- select(data_NO3NO2, -c(Year,Month))
data_NO3NO2 <- Table[complete.cases(Table),]

data_descript_data_station_select <- summarise(group_by(data_NO3NO2, Code_point_Libelle), mean_Ndetect=mean(NO3NO2,na.rm=T), sd_N_detect=sd(NO3NO2,na.rm=T),min_N_detect=min(NO3NO2,na.rm=T),max_N_detect=max(NO3NO2,na.rm=T))
data_descript_data_station_select <- left_join(data_descript_data_station_select,data_NO3NO2)

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,45, by = 5), limits = c(0,45))+
  scale_x_continuous(breaks = seq(0,245, by = 5), limits = c(0,245))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne NO3NO2 region Manche", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanNO3NO2_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,45, by = 5), limits = c(0,45))+
  scale_x_continuous(breaks = seq(0,245, by = 5), limits = c(0,245))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne NO3NO2 region Atlantique", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanNO3NO2_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(31,32)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,45, by = 5), limits = c(0,45))+
  scale_x_continuous(breaks = seq(0,245, by = 5), limits = c(0,245))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne NO3NO2 region Mediterranee", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanNO3NO2_Med_select5A_1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(31,32)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,45, by = 5), limits = c(0,45))+
  scale_x_continuous(breaks = seq(0,40, by = 5), limits = c(0,40))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne NO3NO2 region Mediterranee", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanNO3NO2_Med_select5A_2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')




################# TEMP #######
data_TEMP <- select(data_hp, Code.Region:Code.parametre, TEMP)



ggplot(filter(data_TEMP,Code.Region %in% c(31,32)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = TEMP),size=3)+
  scale_colour_viridis(name = "TEMP",breaks = seq(-1,50, by = 15), limits = c(-1,50))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "TEMP region Mediterranee", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('TEMP_Mediterranee_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_TEMP,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = TEMP),size=3)+
  scale_colour_viridis(name = "TEMP",breaks = seq(-1,50, by = 15), limits = c(-1,50))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "TEMP region Atlantique", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('TEMP_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_TEMP,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = TEMP),size=3)+
  scale_colour_viridis(name = "TEMP",breaks = seq(-1,50, by = 15), limits = c(-1,50))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "TEMP region Manche", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('TEMP_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')


Table <- select(data_TEMP, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(31,32))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:18]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1987,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1987,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(16.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_TEMP_Med1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(16.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_TEMP_Med2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')



Table <- select(data_TEMP, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(21,22,23))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:30]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1987,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1987,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(28.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_TEMP_Atlantic1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(28.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_TEMP_Atlantic2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


Table <- select(data_TEMP, -c(Year,Month))
Table <- Table[complete.cases(Table),]
Table <- filter(Table,Code.Region %in% c(11,12,13))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)



Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:18]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1987,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1987,2023,1)), '-F', sep = '') ################ IDEM

## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(16.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_TEMP_Manche1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(16.5,37),
           label = c(seq(1987,2023,1)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_TEMP_Manche2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


Table <- select(data_TEMP, -c(Year,Month))
data_TEMP <- Table[complete.cases(Table),]

data_descript_data_station_select <- summarise(group_by(data_TEMP, Code_point_Libelle), mean_Ndetect=mean(TEMP,na.rm=T), sd_N_detect=sd(TEMP,na.rm=T),min_N_detect=min(TEMP,na.rm=T),max_N_detect=max(TEMP,na.rm=T))
data_descript_data_station_select <- left_join(data_descript_data_station_select,data_TEMP)

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,7, by = 2), limits = c(0,7))+
  scale_x_continuous(breaks = seq(0,31, by = 5), limits = c(0,31))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne TEMP region Manche", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanTEMP_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,7, by = 2), limits = c(0,7))+
  scale_x_continuous(breaks = seq(0,31, by = 5), limits = c(0,31))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne TEMP region Atlantique", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanTEMP_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(31,32)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,7, by = 2), limits = c(0,7))+
  scale_x_continuous(breaks = seq(0,31, by = 5), limits = c(0,31))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne TEMP region Mediterranee", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanTEMP_Med_select5A_1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

# Pigments
summary(select(data_hp,Allo:PCYAN))

# Taxons cibles 
################# Taxons cibles #######
data_Alex <- select(data_hpT, Code.Region:Code.parametre, Alexandrium_g)



ggplot(filter(data_Alex,Code.Region %in% c(31,32)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = log(Alexandrium_g+1)),size=4)+
  scale_colour_viridis(name = "log(Alexandrium_g +1)",breaks = seq(0,10, by = 5), limits = c(0,10))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "Alexandrium region Mediterranee", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('Alex_Mediterranee_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_Alex,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = log(Alexandrium_g+1)),size=4)+
  scale_colour_viridis(name = "log(Alexandrium_g +1)",breaks = seq(0,10, by = 5), limits = c(0,10))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "Alexandrium region Atlantique", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('Alex_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_Alex,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = log(Alexandrium_g+1)),size=4)+
  scale_colour_viridis(name = "log(Alexandrium_g +1)",breaks = seq(0,10, by = 5), limits = c(0,10))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "Alexandrium region Manche", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('Alex_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

Table <- select(data_Alex, -c(Year,Month))
data_Alex <- Table[complete.cases(Table),]
data_Alex$Alexandrium_g <- log(data_Alex$Alexandrium_g +1)

data_descript_data_station_select <- summarise(group_by(data_Alex, Code_point_Libelle), mean_Ndetect=mean(Alexandrium_g,na.rm=T), sd_N_detect=sd(Alexandrium_g,na.rm=T),min_N_detect=min(Alexandrium_g,na.rm=T),max_N_detect=max(Alexandrium_g,na.rm=T))
data_descript_data_station_select <- left_join(data_descript_data_station_select,data_Alex)

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,2, by = 0.2), limits = c(0,2))+
  scale_x_continuous(breaks = seq(0,13, by = 3), limits = c(0,13))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Alexandrium region Manche", x= " log +1 Valeur", y="Station",size=0.5,colour="SD",
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanAlex_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')


ggplot(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,2, by = 0.2), limits = c(0,2))+
  scale_x_continuous(breaks = seq(0,13, by = 3), limits = c(0,13))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Alexandrium region Atlantique", x= " log +1 Valeur", y="Station",size=0.5,colour="SD",
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanAlex_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(31,32)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,2, by = 0.2), limits = c(0,2))+
  scale_x_continuous(breaks = seq(0,13, by = 3), limits = c(0,13))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Alexandrium region Mediterranee", x= " log +1 Valeur", y="Station",size=0.5,colour="SD",
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanAlex_Med_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')


data_Dino <- select(data_hpT, Code.Region:Code.parametre, Dinophysis_g)



ggplot(filter(data_Dino,Code.Region %in% c(31,32)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = log(Dinophysis_g+1)),size=4)+
  scale_colour_viridis(name = "log(Dinophysis_g +1)",breaks = seq(0,10, by = 5), limits = c(0,10))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "Dinophysis region Mediterranee", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('Dino_Mediterranee_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_Dino,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = log(Dinophysis_g+1)),size=4)+
  scale_colour_viridis(name = "log(Dinophysis_g +1)",breaks = seq(0,10, by = 5), limits = c(0,10))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "Dinophysis region Atlantique", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('Dino_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_Dino,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = log(Dinophysis_g+1)),size=4)+
  scale_colour_viridis(name = "log(Dinophysis_g +1)",breaks = seq(0,10, by = 5), limits = c(0,10))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "Dinophysis region Manche", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('Dino_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

Table <- select(data_Dino, -c(Year,Month))
data_Dino <- Table[complete.cases(Table),]
data_Dino$Dinophysis_g <- log(data_Dino$Dinophysis_g +1)

data_descript_data_station_select <- summarise(group_by(data_Dino, Code_point_Libelle), mean_Ndetect=mean(Dinophysis_g,na.rm=T), sd_N_detect=sd(Dinophysis_g,na.rm=T),min_N_detect=min(Dinophysis_g,na.rm=T),max_N_detect=max(Dinophysis_g,na.rm=T))
data_descript_data_station_select <- left_join(data_descript_data_station_select,data_Dino)

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,2, by = 0.2), limits = c(0,2))+
  scale_x_continuous(breaks = seq(0,13, by = 3), limits = c(0,13))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Dinophysis region Manche", x= " log +1 Valeur", y="Station",size=0.5,colour="SD",
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanDino_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')


ggplot(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,2, by = 0.2), limits = c(0,2))+
  scale_x_continuous(breaks = seq(0,13, by = 3), limits = c(0,13))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Dinophysis region Atlantique", x= " log +1 Valeur", y="Station",size=0.5,colour="SD",
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanDino_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(31,32)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,2, by = 0.2), limits = c(0,2))+
  scale_x_continuous(breaks = seq(0,13, by = 3), limits = c(0,13))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Dinophysis region Mediterranee", x= " log +1 Valeur", y="Station",size=0.5,colour="SD",
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanDino_Med_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')


data_Lepido <- select(data_hpT, Code.Region:Code.parametre, Lepidodinium_g)



ggplot(filter(data_Lepido,Code.Region %in% c(31,32)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = log(Lepidodinium_g+1)),size=4)+
  scale_colour_viridis(name = "log(Lepidodinium_g +1)",breaks = seq(0,16, by = 5), limits = c(0,16))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "Lepidodinium region Mediterranee", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('Lepido_Mediterranee_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_Lepido,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = log(Lepidodinium_g+1)),size=4)+
  scale_colour_viridis(name = "log(Lepidodinium_g +1)",breaks = seq(0,16, by = 5), limits = c(0,16))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "Lepidodinium region Atlantique", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('Lepido_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_Lepido,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = log(Lepidodinium_g+1)),size=4)+
  scale_colour_viridis(name = "log(Lepidodinium_g +1)",breaks = seq(0,16, by = 5), limits = c(0,16))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "Lepidodinium region Manche", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('Lepido_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

Table <- select(data_Lepido, -c(Year,Month))
data_Lepido <- Table[complete.cases(Table),]
data_Lepido$Lepidodinium_g <- log(data_Lepido$Lepidodinium_g +1)

data_descript_data_station_select <- summarise(group_by(data_Lepido, Code_point_Libelle), mean_Ndetect=mean(Lepidodinium_g,na.rm=T), sd_N_detect=sd(Lepidodinium_g,na.rm=T),min_N_detect=min(Lepidodinium_g,na.rm=T),max_N_detect=max(Lepidodinium_g,na.rm=T))
data_descript_data_station_select <- left_join(data_descript_data_station_select,data_Lepido)

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,4, by = 0.5), limits = c(0,4))+
  scale_x_continuous(breaks = seq(0,16, by = 2), limits = c(0,16))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Lepidodinium region Manche", x= " log +1 Valeur", y="Station",size=0.5,colour="SD",
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanLepido_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')


ggplot(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,4, by = 0.5), limits = c(0,4))+
  scale_x_continuous(breaks = seq(0,16, by = 2), limits = c(0,16))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Lepidodinium region Atlantique", x= " log +1 Valeur", y="Station",size=0.5,colour="SD",
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanLepido_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(31,32)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,4, by = 0.5), limits = c(0,4))+
  scale_x_continuous(breaks = seq(0,16, by = 2), limits = c(0,16))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Lepidodinium region Mediterranee", x= " log +1 Valeur", y="Station",size=0.5,colour="SD",
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanLepido_Med_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

data_Lingulo <- select(data_hpT, Code.Region:Code.parametre, Lingulodinium_g)



ggplot(filter(data_Lingulo,Code.Region %in% c(31,32)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = log(Lingulodinium_g+1)),size=4)+
  scale_colour_viridis(name = "log(Lingulodinium_g +1)",breaks = seq(0,10, by = 5), limits = c(0,10))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "Lingulodinium region Mediterranee", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('Lingulo_Mediterranee_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_Lingulo,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = log(Lingulodinium_g+1)),size=4)+
  scale_colour_viridis(name = "log(Lingulodinium_g +1)",breaks = seq(0,10, by = 5), limits = c(0,10))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "Lingulodinium region Atlantique", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('Lingulo_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_Lingulo,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = log(Lingulodinium_g+1)),size=4)+
  scale_colour_viridis(name = "log(Lingulodinium_g +1)",breaks = seq(0,16, by = 5), limits = c(0,16))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "Lingulodinium region Manche", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('Lingulo_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

Table <- select(data_Lingulo, -c(Year,Month))
data_Lingulo <- Table[complete.cases(Table),]
data_Lingulo$Lingulodinium_g <- log(data_Lingulo$Lingulodinium_g +1)

data_descript_data_station_select <- summarise(group_by(data_Lingulo, Code_point_Libelle), mean_Ndetect=mean(Lingulodinium_g,na.rm=T), sd_N_detect=sd(Lingulodinium_g,na.rm=T),min_N_detect=min(Lingulodinium_g,na.rm=T),max_N_detect=max(Lingulodinium_g,na.rm=T))
data_descript_data_station_select <- left_join(data_descript_data_station_select,data_Lingulo)

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,4, by = 0.5), limits = c(0,4))+
  scale_x_continuous(breaks = seq(0,16, by = 2), limits = c(0,16))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Lingulodinium region Manche", x= " log +1 Valeur", y="Station",size=0.5,colour="SD",
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanLingulo_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')


ggplot(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,4, by = 0.5), limits = c(0,4))+
  scale_x_continuous(breaks = seq(0,16, by = 2), limits = c(0,16))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Lingulodinium region Atlantique", x= " log +1 Valeur", y="Station",size=0.5,colour="SD",
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanLingulo_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(31,32)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,4, by = 0.5), limits = c(0,4))+
  scale_x_continuous(breaks = seq(0,16, by = 2), limits = c(0,16))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Lingulodinium region Mediterranee", x= " log +1 Valeur", y="Station",size=0.5,colour="SD",
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanLingulo_Med_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')





data_Nocti <- select(data_hpT, Code.Region:Code.parametre, Noctiluca_g)



ggplot(filter(data_Nocti,Code.Region %in% c(31,32)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = log(Noctiluca_g+1)),size=4)+
  scale_colour_viridis(name = "log(Noctiluca_g +1)",breaks = seq(0,10, by = 5), limits = c(0,10))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "Noctiluca region Mediterranee", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('Nocti_Mediterranee_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_Nocti,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = log(Noctiluca_g+1)),size=4)+
  scale_colour_viridis(name = "log(Noctiluca_g +1)",breaks = seq(0,10, by = 5), limits = c(0,10))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "Noctiluca region Atlantique", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('Nocti_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_Nocti,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = log(Noctiluca_g+1)),size=4)+
  scale_colour_viridis(name = "log(Noctiluca_g +1)",breaks = seq(0,16, by = 5), limits = c(0,16))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "Noctiluca region Manche", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('Nocti_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

Table <- select(data_Nocti, -c(Year,Month))
data_Nocti <- Table[complete.cases(Table),]
data_Nocti$Noctiluca_g <- log(data_Nocti$Noctiluca_g +1)

data_descript_data_station_select <- summarise(group_by(data_Nocti, Code_point_Libelle), mean_Ndetect=mean(Noctiluca_g,na.rm=T), sd_N_detect=sd(Noctiluca_g,na.rm=T),min_N_detect=min(Noctiluca_g,na.rm=T),max_N_detect=max(Noctiluca_g,na.rm=T))
data_descript_data_station_select <- left_join(data_descript_data_station_select,data_Nocti)

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,4, by = 0.5), limits = c(0,4))+
  scale_x_continuous(breaks = seq(0,16, by = 2), limits = c(0,16))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Noctiluca region Manche", x= " log +1 Valeur", y="Station",size=0.5,colour="SD",
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanNocti_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')


ggplot(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,4, by = 0.5), limits = c(0,4))+
  scale_x_continuous(breaks = seq(0,16, by = 2), limits = c(0,16))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Noctiluca region Atlantique", x= " log +1 Valeur", y="Station",size=0.5,colour="SD",
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanNocti_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(31,32)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,4, by = 0.5), limits = c(0,4))+
  scale_x_continuous(breaks = seq(0,16, by = 2), limits = c(0,16))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Noctiluca region Mediterranee", x= " log +1 Valeur", y="Station",size=0.5,colour="SD",
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanNocti_Med_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

data_Meso <- select(data_hpT, Code.Region:Code.parametre, Mesodinium_g)



ggplot(filter(data_Meso,Code.Region %in% c(31,32)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = log(Mesodinium_g+1)),size=4)+
  scale_colour_viridis(name = "log(Mesodinium_g +1)",breaks = seq(0,13, by = 5), limits = c(0,13))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "Mesodinium region Mediterranee", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('Meso_Mediterranee_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_Meso,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = log(Mesodinium_g+1)),size=4)+
  scale_colour_viridis(name = "log(Mesodinium_g +1)",breaks = seq(0,13, by = 5), limits = c(0,13))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "Mesodinium region Atlantique", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('Meso_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

ggplot(filter(data_Meso,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = log(Mesodinium_g+1)),size=4)+
  scale_colour_viridis(name = "log(Mesodinium_g +1)",breaks = seq(0,13, by = 5), limits = c(0,13))+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = "Mesodinium region Manche", x= "Mois", y="Station",colour="Salinite",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('Meso_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 420, units = 'mm')

Table <- select(data_Meso, -c(Year,Month))
data_Meso <- Table[complete.cases(Table),]
data_Meso$Mesodinium_g <- log(data_Meso$Mesodinium_g +1)

data_descript_data_station_select <- summarise(group_by(data_Meso, Code_point_Libelle), mean_Ndetect=mean(Mesodinium_g,na.rm=T), sd_N_detect=sd(Mesodinium_g,na.rm=T),min_N_detect=min(Mesodinium_g,na.rm=T),max_N_detect=max(Mesodinium_g,na.rm=T))
data_descript_data_station_select <- left_join(data_descript_data_station_select,data_Meso)

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,4, by = 0.5), limits = c(0,4))+
  scale_x_continuous(breaks = seq(0,16, by = 2), limits = c(0,16))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Mesodinium region Manche", x= " log +1 Valeur", y="Station",size=0.5,colour="SD",
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanMeso_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')


ggplot(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,4, by = 0.5), limits = c(0,4))+
  scale_x_continuous(breaks = seq(0,16, by = 2), limits = c(0,16))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Mesodinium region Atlantique", x= " log +1 Valeur", y="Station",size=0.5,colour="SD",
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanMeso_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(31,32)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,4, by = 0.5), limits = c(0,4))+
  scale_x_continuous(breaks = seq(0,16, by = 2), limits = c(0,16))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne Mesodinium region Mediterranee", x= " log +1 Valeur", y="Station",size=0.5,colour="SD",
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanMeso_Med_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

