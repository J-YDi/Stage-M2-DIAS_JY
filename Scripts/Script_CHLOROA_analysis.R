# Script analyse des methodes de chloro A pour sélection

Table <- read_delim("data_modif/Table_CHLOROA_S_SELECT.csv", 
                                     delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                     grouping_mark = ""), trim_ws = TRUE)

# Selection des stations ou pas
Table <- filter(Table,
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


library(GGally)
library(DescTools)
ggcorr(select(Table, CHLOROA_CHRL01:CHLOROA_SPECMO04))



t.test(Table$CHLOROA_CHRL01,Table$CHLOROA_FLUO04)
t.test(Table$CHLOROA_CHRL01,Table$CHLOROA_SPECM04)
t.test(Table$CHLOROA_FLUO04,Table$CHLOROA_SPECMO04)

Table2 <- pivot_longer(Table,CHLOROA_CHRL01:CHLOROA_SPECMO04, names_to = "methode",values_to = "n")

kruskal.test(Table2$n~Table2$methode)
DunnTest(Table2$n~Table2$methode,method="BH") 

summary(aov(n ~ methode * Date * Code_point_Libelle, data = Table2))

data_LIQ <- select(Table, ZM_Quadrige_Numero:Mesure_Symbole, Prelevement.niveau:CHLOROA_CHRL01)

data_LIQ <- data_LIQ[complete.cases(data_LIQ),]

data_descript_data_station_select <- summarise(group_by(data_LIQ, Code_point_Libelle), mean_Ndetect=mean(CHLOROA_CHRL01,na.rm=T), sd_N_detect=sd(CHLOROA_CHRL01,na.rm=T),min_N_detect=min(CHLOROA_CHRL01,na.rm=T),max_N_detect=max(CHLOROA_CHRL01,na.rm=T))
data_descript_data_station_select <- left_join(data_descript_data_station_select,data_LIQ)

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,8, by = 2), limits = c(0,8))+
  scale_x_continuous(breaks = seq(0,40, by = 5), limits = c(0,40))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne CHLOROA_CHRL01 region Manche", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanCHLOROA_CHRL01_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,20, by = 5), limits = c(0,20))+
  scale_x_continuous(breaks = seq(0,20, by = 5), limits = c(0,20))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne CHLOROA_CHRL01 region Atlantique", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanCHLOROA_CHRL01_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(31,32)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,20, by = 5), limits = c(0,20))+
  scale_x_continuous(breaks = seq(0,20, by = 5), limits = c(0,20))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne CHLOROA_CHRL01 region Mediterranee", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanCHLOROA_CHRL01_Med_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')


data_LIQ <- select(Table, ZM_Quadrige_Numero:Mesure_Symbole, Prelevement.niveau:Code.Region,CHLOROA_FLUO04)

data_LIQ <- data_LIQ[complete.cases(data_LIQ),]

data_descript_data_station_select <- summarise(group_by(data_LIQ, Code_point_Libelle), mean_Ndetect=mean(CHLOROA_FLUO04,na.rm=T), sd_N_detect=sd(CHLOROA_FLUO04,na.rm=T),min_N_detect=min(CHLOROA_FLUO04,na.rm=T),max_N_detect=max(CHLOROA_FLUO04,na.rm=T))
data_descript_data_station_select <- left_join(data_descript_data_station_select,data_LIQ)

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,8, by = 2), limits = c(0,8))+
  scale_x_continuous(breaks = seq(0,55, by = 5), limits = c(0,55))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne CHLOROA_FLUO04 region Manche", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanCHLOROA_FLUO04_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,8, by = 2), limits = c(0,8))+
  scale_x_continuous(breaks = seq(0,55, by = 5), limits = c(0,55))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne CHLOROA_FLUO04 region Atlantique", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanCHLOROA_FLUO04_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(31,32)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,8, by = 2), limits = c(0,8))+
  scale_x_continuous(breaks = seq(0,55, by = 5), limits = c(0,55))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne CHLOROA_FLUO04 region Mediterranee", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(31,32))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanCHLOROA_FLUO04_Med_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')


data_LIQ <- select(Table, ZM_Quadrige_Numero:Mesure_Symbole, Prelevement.niveau:Code.Region,CHLOROA_SPECMO04)

data_LIQ <- data_LIQ[complete.cases(data_LIQ),]

data_descript_data_station_select <- summarise(group_by(data_LIQ, Code_point_Libelle), mean_Ndetect=mean(CHLOROA_SPECMO04,na.rm=T), sd_N_detect=sd(CHLOROA_SPECMO04,na.rm=T),min_N_detect=min(CHLOROA_SPECMO04,na.rm=T),max_N_detect=max(CHLOROA_SPECMO04,na.rm=T))
data_descript_data_station_select <- left_join(data_descript_data_station_select,data_LIQ)

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,22, by = 2), limits = c(0,22))+
  scale_x_continuous(breaks = seq(0,190, by = 5), limits = c(0,190))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne CHLOROA_SPECMO04 region Manche", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(11,12,13))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanCHLOROA_SPECMO04_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,22, by = 2), limits = c(0,22))+
  scale_x_continuous(breaks = seq(0,190, by = 5), limits = c(0,190))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne CHLOROA_SPECMO04 region Atlantique", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanCHLOROA_SPECMO04_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')

ggplot(filter(data_descript_data_station_select,Code.Region %in% c(31,32)))+
  geom_point(aes(x=mean_Ndetect,y=Code_point_Libelle,colour = sd_N_detect),size=5)+
  geom_point(aes(x=max_N_detect,y=Code_point_Libelle),size=2,col="red")+
  geom_point(aes(x=min_N_detect,y=Code_point_Libelle),size=2,col="violet")+
  scale_colour_viridis(name = "Ecart-type",breaks = seq(0,22, by = 2), limits = c(0,22))+
  scale_x_continuous(breaks = seq(0,190, by = 5), limits = c(0,190))+
  geom_errorbar(aes(xmin = mean_Ndetect - sd_N_detect, xmax =mean_Ndetect + sd_N_detect, y=Code_point_Libelle,colour = sd_N_detect))+
  labs(title = "Moyenne CHLOROA_SPECMO04 region Mediterranee", x= "Valeur", y="Station",colour = "Nombre",size=0.5,
       subtitle = paste("Moy:",mean(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Min:",min(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect),"Max:",max(filter(data_descript_data_station_select,Code.Region %in% c(21,22,23))$mean_Ndetect)))+
  theme(legend.position = "left",legend.box = "horizontal",
        axis.text.y = element_text())
ggsave('meanCHLOROA_SPECMO04_Med_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 400, height = 300, units = 'mm')


# CA NE MARCHE PAS EN DESSOUS

ggplot(filter(data_LIQ,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=Month,y=Code_point_Libelle),col = "grey")+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = CHLOROA_CHRL01,size=CHLOROA_CHRL01))+
  #scale_colour_viridis(name = "NH4",breaks = seq(0,50, by = 5), limits = c(0,50))+
  #scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = " CHLOROA_CHRL01 region Mediterranee", x= "Mois", y="Station",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 400, units = 'mm')

t <- filter(data_LIQ,Code.Region %in% c(11,12,13))


# Avec nouveau jeu de donnees
Table <- read_delim("data_modif/Table_S_select5A_chloro.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                        grouping_mark = ""), trim_ws = TRUE)
ggplot(filter(Table,Code.Region %in% c(11,12,13)))+
  geom_point(aes(x=Month,y=Code_point_Libelle),col = "grey")+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = CHLOROA,shape=Methode.chloro))+
  scale_colour_viridis(name = "Chl-A")+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = " Chl-A region Manche", x= "Mois", y="Station",size=0.5,shape="Methode")+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('Chloro_methode_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 400, units = 'mm')

ggplot(filter(Table,Code.Region %in% c(21,22,23)))+
  geom_point(aes(x=Month,y=Code_point_Libelle),col = "grey")+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = CHLOROA,shape=Methode.chloro))+
  scale_colour_viridis(name = "Chl-A")+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = " Chl-A region Atlantique", x= "Mois", y="Station",size=0.5,shape="Methode")+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('Chloro_methode_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 400, units = 'mm')

ggplot(filter(Table,Code.Region %in% c(31,32)))+
  geom_point(aes(x=Month,y=Code_point_Libelle),col = "grey")+
  geom_point(aes(x=Month,y=Code_point_Libelle,colour = CHLOROA,shape=Methode.chloro))+
  scale_colour_viridis(name = "Chl-A")+
  scale_x_discrete(breaks = waiver(), labels = waiver(), limits = c(1:12))+
  facet_wrap(~Year)+
  labs(title = " Chl-A region Mediterranee", x= "Mois", y="Station",size=0.5,shape="Methode")+
  theme(legend.position = "left",legend.box = "horizontal")+
  scale_shape_manual(values = c(17, 15, 3))
ggsave('Chloro_methode_Med_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 400, units = 'mm')


# A certaines stations, il y a plusieurs méthodes utilisé pour le même mois à la meme annee 
# peut etre de quoi comparer ? on regarde

Table_compar_incompar <- 
  filter(Table, Code_point_Libelle == "Men er Roue" & 
           Month == 1 & Year == 2018 )

Table_compar_incompar <- 
  filter(Table, Code_point_Libelle == "Men er Roue" & 
           Month == 9 & Year == 2017)

Table_compar_incompar <- 
  filter(Table, Code_point_Libelle == "Men er Roue" & 
           Month == 10 & Year == 2017)

Table_compar_incompar <- 
  filter(Table, Code_point_Libelle == "Men er Roue" & 
           Month == 11 & Year == 2017)

Table_compar_incompar <- 
  filter(Table, Code_point_Libelle == "Men er Roue" & 
           Month == 12 & Year == 2017)

Table_compar_incompar <- 
  filter(Table, Code_point_Libelle == "Men er Roue" & 
           Month == 10 & Year == 2016)

Table_compar_incompar <- 
  filter(Table, Code_point_Libelle == "Men er Roue" & 
           Month == 11 & Year == 2016)

### Men er Roue
Table_compar_incompar <- 
  filter(Table, (Code_point_Libelle == "Men er Roue" & 
           Month == 1 & Year == 2018) |
        (Code_point_Libelle == "Men er Roue" & 
           Month == 9 & Year == 2017) |
        (Code_point_Libelle == "Men er Roue" & 
           Month == 10 & Year == 2017) |
        (Code_point_Libelle == "Men er Roue" & 
           Month == 11 & Year == 2017) | 
        (Code_point_Libelle == "Men er Roue" & 
           Month == 12 & Year == 2017) |
        (Code_point_Libelle == "Men er Roue" & 
           Month == 10 & Year == 2016) |
        (Code_point_Libelle == "Men er Roue" & 
           Month == 11 & Year == 2016))
Table_compar_incompar <- 
  filter(Table_compar_incompar, ID.interne.passage != 60562853)

write.csv2(Table_compar_incompar,file="output/data/Table_Chloro_compar.csv", row.names = FALSE,dec = ".")

Table <- read_delim("output/data/Table_Chloro_compar.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                        grouping_mark = ""), trim_ws = TRUE)
Table <- Table |>
  pivot_wider(names_from = "Methode.chloro",values_from = "CHLOROA") |>
  select(Code.Region:Prelevement.niveau,CHLOROA_CHRL01:CHLOROA_SPECMO04)

mean(Table$CHLOROA_CHRL01)
mean(Table$CHLOROA_FLUO04,na.rm=T)
mean(Table$CHLOROA_SPECMO04,na.rm=T)

sd(Table$CHLOROA_CHRL01)
sd(Table$CHLOROA_FLUO04,na.rm=T)
sd(Table$CHLOROA_SPECMO04,na.rm=T)

Table <- select(Table,CHLOROA_CHRL01,CHLOROA_FLUO04)
Table <- Table[complete.cases(Table),]

shapiro.test(Table$CHLOROA_CHRL01)
shapiro.test(Table$CHLOROA_FLUO04)
t.test(Table$CHLOROA_CHRL01,Table$CHLOROA_FLUO04,paired = T)

wilcox.test(Table$CHLOROA_CHRL01,Table$CHLOROA_FLUO04,paired = T)

# Verifie si pas lie a la qualite du prelevement
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

Table_compar_incompar2 <- 
  filter(Table1_hydro, Code.parametre == "CHLOROA" & ((Code_point_Libelle == "Men er Roue" & 
                   Month == 1 & Year == 2018) |
           (Code_point_Libelle == "Men er Roue" & 
              Month == 9 & Year == 2017) |
           (Code_point_Libelle == "Men er Roue" & 
              Month == 10 & Year == 2017) |
           (Code_point_Libelle == "Men er Roue" & 
              Month == 11 & Year == 2017) | 
           (Code_point_Libelle == "Men er Roue" & 
              Month == 12 & Year == 2017) |
           (Code_point_Libelle == "Men er Roue" & 
              Month == 10 & Year == 2016) |
           (Code_point_Libelle == "Men er Roue" & 
              Month == 11 & Year == 2016)))


# On regarde avec toutes les donnees 
Table <- read_delim("data_modif/Table_S_chloro.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                        grouping_mark = ""), trim_ws = TRUE)
Table <- filter(Table, Code.Region != "0")

doublons <- Table[duplicated(Table$ID.interne.passage) | duplicated(Table$ID.interne.passage, fromLast = TRUE), ]


Table <- doublons |>
  pivot_wider(names_from = "Methode.chloro",values_from = "CHLOROA") |>
  select(Code.Region:Prelevement.niveau,CHLOROA_CHRL01:CHLOROA_FLUO04)

# On prend une des lignes ci-dessous
Table <- select(Table,Code.Region:Prelevement.niveau,CHLOROA_CHRL01,CHLOROA_FLUO04)
Table <- select(Table,Code.Region:Prelevement.niveau,CHLOROA_CHRL01,CHLOROA_SPECMO04)
Table <- select(Table,Code.Region:Prelevement.niveau,CHLOROA_SPECMO04,CHLOROA_FLUO04)

# Analyse difference

Table <- Table[complete.cases(Table),]


mean(Table$CHLOROA_CHRL01)
mean(Table$CHLOROA_FLUO04,na.rm=T)
mean(Table$CHLOROA_SPECMO04,na.rm=T)

sd(Table$CHLOROA_CHRL01)
sd(Table$CHLOROA_FLUO04,na.rm=T)
sd(Table$CHLOROA_SPECMO04,na.rm=T)

shapiro.test(Table$CHLOROA_CHRL01)
shapiro.test(Table$CHLOROA_SPECMO04)
shapiro.test(Table$CHLOROA_FLUO04)
t.test(Table$CHLOROA_CHRL01,Table$CHLOROA_FLUO04,paired = T)
t.test(Table$CHLOROA_CHRL01,Table$CHLOROA_SPECMO04,paired = T)


wilcox.test(Table$CHLOROA_CHRL01,Table$CHLOROA_FLUO04,paired = T)
wilcox.test(Table$CHLOROA_CHRL01,Table$CHLOROA_SPECMO04,paired = T)

Table <- select(doublons, Code.Region:CHLOROA)

kruskal.test(Table$CHLOROA~Table$Methode.chloro)
DunnTest(Table$CHLOROA~Table$Methode.chloro,method="BH") 

summary(aov(CHLOROA ~ Methode.chloro, data = Table))

Table <- filter(Table, Methode.chloro == "CHLOROA_FLUO04" |Methode.chloro == "CHLOROA_SPECMO04" 
                | Methode.chloro == "CHLOROA_CHRL01" )
kruskal.test(Table$CHLOROA~Table$Methode.chloro)
DunnTest(Table$CHLOROA~Table$Methode.chloro,method="BH") 

summary(aov(CHLOROA ~ Methode.chloro, data = Table))

ggplot(doublons)+
  geom_boxplot(aes(Methode.chloro,CHLOROA))+
  labs(title = "Methode chloro toutes stations", x= "Methode", y="Concentration chl-a",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('Meth_chloro_allst_allmeth.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 400, units = 'mm')

ggplot(doublons)+
  geom_boxplot(aes(Methode.chloro,CHLOROA))+
  labs(title = "Methode chloro stations select", x= "Methode", y="Concentration chl-a",size=0.5)+
  theme(legend.position = "left",legend.box = "horizontal")
ggsave('Meth_chloro_select5A_allmeth.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/data_description",dpi = 600, width = 500, height = 400, units = 'mm')

ggcorr(select(Table, CHLOROA_CHRL01:CHLOROA_FLUO04))

