# Import data 
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

data$cluster <- as.factor(data$cluster)
data$lon <- as.numeric(data$lon)
data$lat <- as.numeric(data$lat)
# On defini la couleur des clusters pour coherence partout
cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF")

# Carte des stations
Worldmap <- map_data('worldHires')


ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = "gray", color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)')+
  theme_gdocs()+
  geom_point(data = data, aes(x = lon, y = lat,colour=cluster), size =8)+
  geom_text(data = filter(data,Code_point_Libelle == "Teychan bis") ,aes(x = lon + 1.17, y = lat,label = "Teychan bis" ), stat = 'unique', size = 3,color="black",fontface = "bold") +
  geom_text(data = filter(data,Code_point_Libelle == "Ouest Loscolo") ,aes(x = lon + 1.3, y = lat,label = "Ouest Loscolo" ), stat = 'unique', size = 3,color="black",fontface = "bold") +
  geom_text(data = filter(data,Code_point_Libelle == "Men er Roue") ,aes(x = lon - 1.3, y = lat,label = "Men er Roue" ), stat = 'unique', size = 3,color="black",fontface = "bold") +
  geom_text(data = filter(data,Code_point_Libelle == "Le Cornard") ,aes(x = lon + 1.1, y = lat,label = "Le Cornard" ), stat = 'unique', size = 3,color="black",fontface = "bold") +
  geom_text(data = filter(data,Code_point_Libelle == "Bois de la Chaise large") ,aes(x = lon + 2, y = lat,label = "Bois de la Chaise large" ), stat = 'unique', size = 3,color="black",fontface = "bold") +
  geom_text(data = filter(data,Code_point_Libelle == "Auger") ,aes(x = lon + .8, y = lat,label = "Auger" ), stat = 'unique', size = 3,color="black",fontface = "bold") +
  geom_text(data = filter(data,Code_point_Libelle == "Point 1 Boulogne") ,aes(x = lon + 1.7, y = lat,label = "Point 1 Boulogne" ), stat = 'unique', size = 3,color="black",fontface = "bold") +
  geom_text(data = filter(data,Code_point_Libelle == "Loguivy") ,aes(x = lon - .9, y = lat+0.2,label = "Loguivy" ), stat = 'unique', size = 3,color="black",fontface = "bold") +
  
  geom_text(data = filter(data,Code_point_Libelle == "les Hébihens") ,aes(x = lon + 1.5, y = lat,label = "les Hébihens" ), stat = 'unique', size = 3,color="black",fontface = "bold") +
  geom_text(data = filter(data,Code_point_Libelle == "Géfosse") ,aes(x = lon - .9, y = lat,label = "Géfosse" ), stat = 'unique', size = 3,color="black",fontface = "bold") +
  geom_text(data = filter(data,Code_point_Libelle == "Cabourg") ,aes(x = lon + 1, y = lat,label = "Cabourg" ), stat = 'unique', size = 3,color="black",fontface = "bold") +
  geom_text(data = filter(data,Code_point_Libelle == "Antifer ponton pétrolier") ,aes(x = lon + 2, y = lat,label = "Antifer ponton pétrolier" ), stat = 'unique', size = 3,color="black",fontface = "bold") +
  geom_text(data = filter(data,Code_point_Libelle == "Sète mer") ,aes(x = lon - 1.1, y = lat-0.05,label = "Sète mer" ), stat = 'unique', size = 3,color="black",fontface = "bold") +
  geom_text(data = filter(data,Code_point_Libelle == "Parc Leucate 2")[1,] ,aes(x = lon - 1.5, y = lat-0.11,label = "Parc Leucate 2" ), stat = 'unique', size = 3,color="black",fontface = "bold") +
  geom_text(data = filter(data,Code_point_Libelle == "Diana centre")[1,] ,aes(x = lon - 1.4, y = lat,label = "Diana centre" ), stat = 'unique', size = 3,color="black",fontface = "bold") +
  
  geom_text(data = filter(data,Code_point_Libelle == "Calvi") ,aes(x = lon + .7, y = lat,label = "Calvi" ), stat = 'unique', size = 3,color="black",fontface = "bold") +
  geom_text(data = filter(data,Code_point_Libelle == "Bouzigues (a)")[1,] ,aes(x = lon - 1.5, y = lat+0.15,label = "Bouzigues (a)" ), stat = 'unique', size = 3,color="black",fontface = "bold") +
  geom_text(data = filter(data,Code_point_Libelle == "Barcares") ,aes(x = lon - 1, y = lat + 0.2,label = "Barcares" ), stat = 'unique', size = 3,color="black",fontface = "bold") +
  geom_text(data = filter(data,Code_point_Libelle == "Anse de Carteau 2") ,aes(x = lon + 1.8, y = lat,label = "Anse de Carteau 2" ), stat = 'unique', size = 3,color="black",fontface = "bold") +
  geom_text(data = filter(data,Code_point_Libelle == "22B - Toulon gde rade") ,aes(x = lon +2, y = lat,label = "22B - Toulon gde rade" ), stat = 'unique', size = 3,color="black",fontface = "bold") +
  geom_text(data = filter(data,Code_point_Libelle == "At so") ,aes(x = lon + .7, y = lat,label = "At so" ), stat = 'unique', size = 3,color="black",fontface = "bold") +
  geom_text(data = filter(data,Code_point_Libelle == "At so") ,aes(x = 3.5, y = 48,label = "FRANCE" ), stat = 'unique', size = 7,color="burlywood4",fontface = "bold") +
  
  geom_text(aes(x = -3.5, y = 46, label = 'Océan Atlantique'), stat = 'unique', size = 4,color = "lightblue4",fontface="italic",angle=-45) +
  geom_text(aes(x = -3.2, y = 50, label = 'Manche'), stat = 'unique', size = 4,color = "lightblue4",fontface="italic",angle=28) +
  geom_text(aes(x = 5, y = 42, label = 'Mer Méditerranée'), stat = 'unique', size = 4,color = "lightblue4",fontface="italic",angle=8) +
  
  scale_colour_manual(values = cluster_col,name="Cluster")+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue1'))+
  guides(color = guide_legend(override.aes = list(size = 10)))

ggsave('maps_station_lgtermcluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/final", dpi = 600, width = 200, height = 200, units = 'mm')
