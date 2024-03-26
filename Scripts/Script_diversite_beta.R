# Chargement des donnees
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

# Garde que les comptages
data_count <- data[,c(2,8,24:247)]
# Remplace les NA par 0
data_count[is.na(data_count)] <- 0
# Transformation d'Hellinger pour ne s'interesser qu'a la composition
data_hel <- decostand(data_count[,-c(1,2)], method = "hellinger")
# Remet la date et la station
data_hel <- bind_cols(data_count[,c(1,2)],data_hel)

# Creation d'un df pour stocker les resultats
data_results_beta <- c("","")
data_results_beta <- as.data.frame(data_results_beta)



### ENTRE LA DATE N ET N-1 ####

i = 1
  data_station <- filter(data_hel, Code_point_Libelle == levels(as.factor(data_hel$Code_point_Libelle))[i])
  for (j in 2:nrow(data_station)){
  Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle

  Date1 <- data_station[j,"Date"]$Date
  Date2 <- data_station[j-1,"Date"]$Date
  Com1 <- data_station[j,c(3:226)]
  rownames(Com1) <- Date1
  Com2 <- data_station[j-1,c(3:226)]
  rownames(Com2) <- Date2
  BC <- vegdist(rbind(Com2, Com1), method = "bray")
  Delta <- paste0(j-1,"-",j)
  if (Date1-Date2 <= 21) {
  data_results_beta[j-1,1] <- Station
  data_results_beta[j-1,2] <- Delta 
  data_results_beta[j-1,3] <- as.numeric(BC)
  data_results_beta[j-1,4] <- Date1
   } else {
    data_results_beta[j-1,1] <- Station
    data_results_beta[j-1,2] <- Delta
    data_results_beta[j-1,3] <- NA
    data_results_beta[j-1,4] <- Date1 }
  }
for (i in 2:length(levels(as.factor(data_hel$Code_point_Libelle)))){
      data_station <- filter(data_hel, Code_point_Libelle == levels(as.factor(data_hel$Code_point_Libelle))[i])
      data_results_beta2 <- c("","")
      data_results_beta2 <- as.data.frame(data_results_beta2)
      for (j in 2:nrow(data_station)){
        Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
        
        Date1 <- data_station[j,"Date"]$Date
        Date2 <- data_station[j-1,"Date"]$Date
        Com1 <- data_station[j,c(3:226)]
        rownames(Com1) <- Date1
        Com2 <- data_station[j-1,c(3:226)]
        rownames(Com2) <- Date2
        BC <- vegdist(rbind(Com2, Com1), method = "bray")
        Delta <- paste0(j-1,"-",j)
        if (Date1-Date2 <= 21) {
          data_results_beta2[j-1,1] <- Station
          data_results_beta2[j-1,2] <- Delta 
          data_results_beta2[j-1,3] <- as.numeric(BC)
          data_results_beta2[j-1,4] <- Date1
        } else {
          data_results_beta2[j-1,1] <- Station
          data_results_beta2[j-1,2] <- Delta
          data_results_beta2[j-1,3] <- NA
          data_results_beta2[j-1,4] <- Date1 }
      }
      colnames(data_results_beta) <- c("Code_point_Libelle","Lag","Bray-Curtis","Date1")
      colnames(data_results_beta2) <- c("Code_point_Libelle","Lag","Bray-Curtis","Date1")
      data_results_beta <- rbind(data_results_beta,data_results_beta2)
    }
  
data_results_beta <- filter(data_results_beta, Code_point_Libelle != "")
# Enregistrement des resultats
write.csv2(data_results_beta,file="data_modif/data_div_beta_N-1.csv", row.names = FALSE,dec = ".")

beta <- read_delim("data_modif/data_div_beta_N-1.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

colnames(beta)[4] <- "Date"

data <- dplyr::select(data, Code_point_Libelle,ID.interne.passage, cluster,TEMP,Bloom,Date)

data_join <- left_join(data,beta)

doublons <- data_join[duplicated(data_join$ID.interne.passage) |
                   duplicated(data_join$ID.interne.passage, fromLast = TRUE), ]

# Filtre des doublons hydro :
resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

# On supprime les lignes en doublon dans le jeu de données initial
data_unique <- subset(data_join, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
# On les remets ces doublons filtres
data_join <- bind_rows(data_unique,resultat_filtre)
# On remet au propre
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

data_beta <- dplyr::select(data_join, - TEMP)
data_beta$EpBloom <- "OUI"
data_beta[is.na(data_beta$Bloom),8] <- "NON"

write.csv2(data_beta,file="data_modif/data_div_beta_ok_N-1.csv", row.names = FALSE,dec = ".")
# On a comme ca les indices de bray-curtis associé à chaque date avec les informations de bloom s'il y a
beta <- read_delim("data_modif/data_div_beta_ok_N-1.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)
#### SERIE TEMPORELLE DES INDICES DE BRAY-CURTIS ####

{ 
beta <- beta %>% 
  mutate(Week = week(Date)) %>%
  mutate(Month = month(Date)) %>%
  # 'ceiling' takes the upper integer of a decimal number
  mutate(Fortnight = ceiling(Week/2)) %>%
  arrange(Fortnight)

beta_g <-  filter(beta, Code_point_Libelle == "Bois de la Chaise large")
ggplot(beta_g)+
  geom_line(aes(x=Date,y=`Bray-Curtis`),size=1)+
  geom_point(aes(x=Date,y=`Bray-Curtis`, colour = EpBloom),size=2)+
  geom_hline(yintercept = mean(filter(beta, Code_point_Libelle == "Bois de la Chaise large" & EpBloom == "OUI")$`Bray-Curtis`),col="grey",size=1)+
  scale_x_date(breaks = seq( min(beta_g$Date), max(beta_g$Date),by=100))+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Bois de la Chaise large",x="DateN")
ggsave('Boisdelachaiselarge.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Bois de la Chaise large",x="Quinzaine")
ggsave('Boisdelachaiselarge_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Bois de la Chaise large",x="Mois")
ggsave('Boisdelachaiselarge_Mois.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')

#

beta_g <-  filter(beta, Code_point_Libelle == "Auger")
ggplot(beta_g)+
  geom_line(aes(x=Date,y=`Bray-Curtis`),size=1)+
  geom_point(aes(x=Date,y=`Bray-Curtis`, colour = EpBloom),size=2)+
  geom_hline(yintercept = mean(filter(beta, Code_point_Libelle == "Auger" & EpBloom == "OUI")$`Bray-Curtis`),col="grey",size=1)+
  scale_x_date(breaks = seq( min(beta_g$Date), max(beta_g$Date),by=100))+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Auger",x="DateN")
ggsave('Auger.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Auger",x="Quinzaine")
ggsave('Auger_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Auger",x="Mois")
ggsave('Auger_Mois.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')

#

beta_g <-  filter(beta, Code_point_Libelle == "Le Cornard")
ggplot(beta_g)+
  geom_line(aes(x=Date,y=`Bray-Curtis`),size=1)+
  geom_point(aes(x=Date,y=`Bray-Curtis`, colour = EpBloom),size=2)+
  geom_hline(yintercept = mean(filter(beta, Code_point_Libelle == "Le Cornard" & EpBloom == "OUI")$`Bray-Curtis`),col="grey",size=1)+
  scale_x_date(breaks = seq( min(beta_g$Date), max(beta_g$Date),by=100))+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Le Cornard",x="DateN")
ggsave('LeCornard.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Le Cornard",x="Quinzaine")
ggsave('LeCornard_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Le Cornard",x="Mois")
ggsave('LeCornard_Mois.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')

#

beta_g <-  filter(beta, Code_point_Libelle == "Teychan bis")
ggplot(beta_g)+
  geom_line(aes(x=Date,y=`Bray-Curtis`),size=1)+
  geom_point(aes(x=Date,y=`Bray-Curtis`, colour = EpBloom),size=2)+
  geom_hline(yintercept = mean(filter(beta, Code_point_Libelle == "Teychan bis" & EpBloom == "OUI")$`Bray-Curtis`),col="grey",size=1)+
  scale_x_date(breaks = seq( min(beta_g$Date), max(beta_g$Date),by=100))+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Teychan bis",x="DateN")
ggsave('Teychanbis.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Teychan bis",x="Quinzaine")
ggsave('Teychanbis_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Teychan bis",x="Mois")
ggsave('Teychanbis_Mois.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


#

beta_g <-  filter(beta, Code_point_Libelle == "Ouest Loscolo")
ggplot(beta_g)+
  geom_line(aes(x=Date,y=`Bray-Curtis`),size=1)+
  geom_point(aes(x=Date,y=`Bray-Curtis`, colour = EpBloom),size=2)+
  geom_hline(yintercept = mean(filter(beta, Code_point_Libelle == "Ouest Loscolo" & EpBloom == "OUI")$`Bray-Curtis`),col="grey",size=1)+
  scale_x_date(breaks = seq( min(beta_g$Date), max(beta_g$Date),by=100))+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Ouest Loscolo",x="DateN")
ggsave('Ouest Loscolo.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Ouest Loscolo",x="Quinzaine")
ggsave('OuestLoscolo_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Ouest Loscolo",x="Mois")
ggsave('OuestLoscolo_Mois.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')

#

beta_g <-  filter(beta, Code_point_Libelle == "Men er Roue")
ggplot(beta_g)+
  geom_line(aes(x=Date,y=`Bray-Curtis`),size=1)+
  geom_point(aes(x=Date,y=`Bray-Curtis`, colour = EpBloom),size=2)+
  geom_hline(yintercept = mean(filter(beta, Code_point_Libelle == "Men er Roue" & EpBloom == "OUI")$`Bray-Curtis`),col="grey",size=1)+
  scale_x_date(breaks = seq( min(beta_g$Date), max(beta_g$Date),by=100))+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Men er Roue",x="DateN")
ggsave('MenerRoue.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Men er Roue",x="Quinzaine")
ggsave('MenerRoue_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Men er Roue",x="Mois")
ggsave('MenerRoue_Mois.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


#

beta_g <-  filter(beta, Code_point_Libelle == "Loguivy")
ggplot(beta_g)+
  geom_line(aes(x=Date,y=`Bray-Curtis`),size=1)+
  geom_point(aes(x=Date,y=`Bray-Curtis`, colour = EpBloom),size=2)+
  geom_hline(yintercept = mean(filter(beta, Code_point_Libelle == "Loguivy" & EpBloom == "OUI")$`Bray-Curtis`),col="grey",size=1)+
  scale_x_date(breaks = seq( min(beta_g$Date), max(beta_g$Date),by=100))+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Loguivy",x="DateN")
ggsave('Loguivy.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Loguivy",x="Quinzaine")
ggsave('Loguivy_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Loguivy",x="Mois")
ggsave('Loguivy_Mois.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


#

beta_g <-  filter(beta, Code_point_Libelle == "les Hébihens")
ggplot(beta_g)+
  geom_line(aes(x=Date,y=`Bray-Curtis`),size=1)+
  geom_point(aes(x=Date,y=`Bray-Curtis`, colour = EpBloom),size=2)+
  geom_hline(yintercept = mean(filter(beta, Code_point_Libelle == "les Hébihens" & EpBloom == "OUI")$`Bray-Curtis`),col="grey",size=1)+
  scale_x_date(breaks = seq( min(beta_g$Date), max(beta_g$Date),by=100))+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "les Hébihens",x="DateN")
ggsave('lesHebihens.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "les Hébihens",x="Quinzaine")
ggsave('lesHebihens_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "les Hébihens",x="Mois")
ggsave('lesHebihens_Mois.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')

#

beta_g <-  filter(beta, Code_point_Libelle == "Géfosse")
ggplot(beta_g)+
  geom_line(aes(x=Date,y=`Bray-Curtis`),size=1)+
  geom_point(aes(x=Date,y=`Bray-Curtis`, colour = EpBloom),size=2)+
  geom_hline(yintercept = mean(filter(beta, Code_point_Libelle == "Géfosse" & EpBloom == "OUI")$`Bray-Curtis`),col="grey",size=1)+
  scale_x_date(breaks = seq( min(beta_g$Date), max(beta_g$Date),by=100))+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Géfosse",x="DateN")
ggsave('Géfosse.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Géfosse",x="Quinzaine")
ggsave('Géfosse_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Géfosse",x="Mois")
ggsave('Géfosse_Mois.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')

#

beta_g <-  filter(beta, Code_point_Libelle == "Cabourg")
ggplot(beta_g)+
  geom_line(aes(x=Date,y=`Bray-Curtis`),size=1)+
  geom_point(aes(x=Date,y=`Bray-Curtis`, colour = EpBloom),size=2)+
  geom_hline(yintercept = mean(filter(beta, Code_point_Libelle == "Cabourg" & EpBloom == "OUI")$`Bray-Curtis`),col="grey",size=1)+
  scale_x_date(breaks = seq( min(beta_g$Date), max(beta_g$Date),by=100))+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Cabourg",x="DateN")
ggsave('Cabourg.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Cabourg",x="Quinzaine")
ggsave('Cabourg_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Cabourg",x="Mois")
ggsave('Cabourg_Mois.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')

#

beta_g <-  filter(beta, Code_point_Libelle == "Antifer ponton pétrolier")
ggplot(beta_g)+
  geom_line(aes(x=Date,y=`Bray-Curtis`),size=1)+
  geom_point(aes(x=Date,y=`Bray-Curtis`, colour = EpBloom),size=2)+
  geom_hline(yintercept = mean(filter(beta, Code_point_Libelle == "Antifer ponton pétrolier" & EpBloom == "OUI")$`Bray-Curtis`),col="grey",size=1)+
  scale_x_date(breaks = seq( min(beta_g$Date), max(beta_g$Date),by=100))+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Antifer ponton pétrolier",x="DateN")
ggsave('Antifer ponton pétrolier.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Antifer ponton pétrolier",x="Quinzaine")
ggsave('Antifer ponton pétrolier_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Antifer ponton pétrolier",x="Mois")
ggsave('Antifer ponton pétrolier_Mois.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')

#

beta_g <-  filter(beta, Code_point_Libelle == "At so")
ggplot(beta_g)+
  geom_line(aes(x=Date,y=`Bray-Curtis`),size=1)+
  geom_point(aes(x=Date,y=`Bray-Curtis`, colour = EpBloom),size=2)+
  geom_hline(yintercept = mean(filter(beta, Code_point_Libelle == "At so" & EpBloom == "OUI")$`Bray-Curtis`),col="grey",size=1)+
  scale_x_date(breaks = seq( min(beta_g$Date), max(beta_g$Date),by=100))+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "At so",x="DateN")
ggsave('Atso.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "At so",x="Quinzaine")
ggsave('Atso_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "At so",x="Mois")
ggsave('Atso_Mois.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')

#

beta_g <-  filter(beta, Code_point_Libelle == "Point 1 Boulogne")
ggplot(beta_g)+
  geom_line(aes(x=Date,y=`Bray-Curtis`),size=1)+
  geom_point(aes(x=Date,y=`Bray-Curtis`, colour = EpBloom),size=2)+
  geom_hline(yintercept = mean(filter(beta, Code_point_Libelle == "Point 1 Boulogne" & EpBloom == "OUI")$`Bray-Curtis`),col="grey",size=1)+
  scale_x_date(breaks = seq( min(beta_g$Date), max(beta_g$Date),by=100))+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Point 1 Boulogne",x="DateN")
ggsave('Boulogne.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Point 1 Boulogne",x="Quinzaine")
ggsave('Boulogne_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Point 1 Boulogne",x="Mois")
ggsave('Boulogne_Mois.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')

#

beta_g <-  filter(beta, Code_point_Libelle == "Parc Leucate 2")
ggplot(beta_g)+
  geom_line(aes(x=Date,y=`Bray-Curtis`),size=1)+
  geom_point(aes(x=Date,y=`Bray-Curtis`, colour = EpBloom),size=2)+
  geom_hline(yintercept = mean(filter(beta, Code_point_Libelle == "Parc Leucate 2" & EpBloom == "OUI")$`Bray-Curtis`),col="grey",size=1)+
  scale_x_date(breaks = seq( min(beta_g$Date), max(beta_g$Date),by=100))+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Parc Leucate 2",x="DateN")
ggsave('ParcLeucate.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Parc Leucate 2",x="Quinzaine")
ggsave('ParcLeucate_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Parc Leucate 2",x="Mois")
ggsave('ParcLeucate_Mois.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')

#

beta_g <-  filter(beta, Code_point_Libelle == "Barcares")
ggplot(beta_g)+
  geom_line(aes(x=Date,y=`Bray-Curtis`),size=1)+
  geom_point(aes(x=Date,y=`Bray-Curtis`, colour = EpBloom),size=2)+
  geom_hline(yintercept = mean(filter(beta, Code_point_Libelle == "Barcares" & EpBloom == "OUI")$`Bray-Curtis`),col="grey",size=1)+
  scale_x_date(breaks = seq( min(beta_g$Date), max(beta_g$Date),by=100))+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Barcares",x="DateN")
ggsave("Barcares.png",path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Barcares",x="Quinzaine")
ggsave('Barcares_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Barcares",x="Mois")
ggsave('Barcares_Mois.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')

#

beta_g <-  filter(beta, Code_point_Libelle == "Sète mer")
ggplot(beta_g)+
  geom_line(aes(x=Date,y=`Bray-Curtis`),size=1)+
  geom_point(aes(x=Date,y=`Bray-Curtis`, colour = EpBloom),size=2)+
  geom_hline(yintercept = mean(filter(beta, Code_point_Libelle == "Sète mer" & EpBloom == "OUI")$`Bray-Curtis`),col="grey",size=1)+
  scale_x_date(breaks = seq( min(beta_g$Date), max(beta_g$Date),by=100))+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Sète mer",x="DateN")
ggsave('Sètemer.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Sète mer",x="Quinzaine")
ggsave('Sètemer_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Sète mer",x="Mois")
ggsave('Sètemer_Mois.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')

#

beta_g <-  filter(beta, Code_point_Libelle == "Bouzigues (a)")
ggplot(beta_g)+
  geom_line(aes(x=Date,y=`Bray-Curtis`),size=1)+
  geom_point(aes(x=Date,y=`Bray-Curtis`, colour = EpBloom),size=2)+
  geom_hline(yintercept = mean(filter(beta, Code_point_Libelle == "Bouzigues (a)" & EpBloom == "OUI")$`Bray-Curtis`),col="grey",size=1)+
  scale_x_date(breaks = seq( min(beta_g$Date), max(beta_g$Date),by=100))+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Bouzigues (a)",x="DateN")
ggsave('Bouzigues.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Bouzigues (a)",x="Quinzaine")
ggsave('Bouzigues_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Bouzigues (a)",x="Mois")
ggsave('Bouzigues_Mois.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


#

beta_g <-  filter(beta, Code_point_Libelle == "Anse de Carteau 2")
ggplot(beta_g)+
  geom_line(aes(x=Date,y=`Bray-Curtis`),size=1)+
  geom_point(aes(x=Date,y=`Bray-Curtis`, colour = EpBloom),size=2)+
  geom_hline(yintercept = mean(filter(beta, Code_point_Libelle == "Anse de Carteau 2" & EpBloom == "OUI")$`Bray-Curtis`),col="grey",size=1)+
  scale_x_date(breaks = seq( min(beta_g$Date), max(beta_g$Date),by=100))+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Anse de Carteau 2",x="DateN")
ggsave('Ansedecarteau.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Anse de Carteau 2",x="Quinzaine")
ggsave('Ansedecarteau_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Anse de Carteau 2",x="Mois")
ggsave('Ansedecarteau_Mois.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')

#

beta_g <-  filter(beta, Code_point_Libelle == "22B - Toulon gde rade")
ggplot(beta_g)+
  geom_line(aes(x=Date,y=`Bray-Curtis`),size=1)+
  geom_point(aes(x=Date,y=`Bray-Curtis`, colour = EpBloom),size=2)+
  geom_hline(yintercept = mean(filter(beta, Code_point_Libelle == "22B - Toulon gde rade" & EpBloom == "OUI")$`Bray-Curtis`),col="grey",size=1)+
  scale_x_date(breaks = seq( min(beta_g$Date), max(beta_g$Date),by=100))+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "22B - Toulon gde rade",x="DateN")
ggsave('Toulon.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "22B - Toulon gde rade",x="Quinzaine")
ggsave('Toulon_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "22B - Toulon gde rade",x="Mois")
ggsave('Toulon_Mois.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')

#

beta_g <-  filter(beta, Code_point_Libelle == "Calvi")
ggplot(beta_g)+
  geom_line(aes(x=Date,y=`Bray-Curtis`),size=1)+
  geom_point(aes(x=Date,y=`Bray-Curtis`, colour = EpBloom),size=2)+
  geom_hline(yintercept = mean(filter(beta, Code_point_Libelle == "Calvi" & EpBloom == "OUI")$`Bray-Curtis`),col="grey",size=1)+
  scale_x_date(breaks = seq( min(beta_g$Date), max(beta_g$Date),by=100))+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Calvi",x="DateN")
ggsave('Calvi.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Calvi",x="Quinzaine")
ggsave('Calvi_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Calvi",x="Mois")
ggsave('Calvi_Mois.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


#

beta_g <-  filter(beta, Code_point_Libelle == "Diana centre")
ggplot(beta_g)+
  geom_line(aes(x=Date,y=`Bray-Curtis`),size=1)+
  geom_point(aes(x=Date,y=`Bray-Curtis`, colour = EpBloom),size=2)+
  geom_hline(yintercept = mean(filter(beta, Code_point_Libelle == "Diana centre" & EpBloom == "OUI")$`Bray-Curtis`),col="grey",size=1)+
  scale_x_date(breaks = seq( min(beta_g$Date), max(beta_g$Date),by=100))+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Diana centre",x="DateN")
ggsave('Dianacentre.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Diana centre",x="Quinzaine")
ggsave('Dianacentre_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",
       subtitle = "Diana centre",x="Mois")
ggsave('Dianacentre_Mois.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
}

#### Comparaison N et N-1 ####
### !!! Modification exterme pour dire quel bloom a prendre sur data_div_beta_ok_N-1.csv
beta <- read_delim("data_modif/data_div_beta_ok_N-1.csv", 
                                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), 
                                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

data_bloom_beta <- left_join(beta,data_bloom)


wilcox.test(filter(data_bloom_beta, EpBloom == "OUI")$`Bray-Curtis`,
                   filter(data_bloom_beta, EpBloom == "NON")$`Bray-Curtis`)


cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_bloom_beta, EpBloom == "OUI" | EpBloom == "NON"))+
  geom_boxplot(aes(x=EpBloom,group=EpBloom,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(title="Indice de Bray-Curtis entre le jour N et le jour N-1",x="Episode de bloom au jour N",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_bloom_beta, EpBloom == "OUI" | EpBloom == "NON"))+
  geom_boxplot(aes(x=EpBloom,group=EpBloom,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Episode de bloom au jour N",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_bloom_beta, EpBloom == "OUI" | EpBloom == "NON"))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~EpBloom, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('N_N-1_B_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

wilcox.test(filter(data_bloom_beta, EpBloom == "OUI" & cluster == 1)$`Bray-Curtis`,
            filter(data_bloom_beta, EpBloom == "NON" & cluster == 1)$`Bray-Curtis`)

wilcox.test(filter(data_bloom_beta, EpBloom == "OUI" & cluster == 2)$`Bray-Curtis`,
            filter(data_bloom_beta, EpBloom == "NON" & cluster == 2)$`Bray-Curtis`)

wilcox.test(filter(data_bloom_beta, EpBloom == "OUI" & cluster == 3)$`Bray-Curtis`,
            filter(data_bloom_beta, EpBloom == "NON" & cluster == 3)$`Bray-Curtis`)

wilcox.test(filter(data_bloom_beta, EpBloom == "OUI" & cluster == 4)$`Bray-Curtis`,
            filter(data_bloom_beta, EpBloom == "NON" & cluster == 4)$`Bray-Curtis`)

data_kw <- filter(data_bloom_beta, EpBloom == "OUI")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta, EpBloom == "NON")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH") 

# Mtn on ne s'interresse toujours qu'au premier evenement de bloom
data_bloom_beta <- filter(data_bloom_beta, EpBloom == "OUI" | EpBloom == "NON")

data_bloom_beta$dino <- ifelse(data_bloom_beta$Bloom_Phylum == "Dino", "Dinophyceae","Autre nature")
data_bloom_beta[is.na(data_bloom_beta$dino),]$dino <- "Non bloom"

data_kw <- filter(data_bloom_beta)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$dino)
DunnTest(data_kw$`Bray-Curtis`~data_kw$dino,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=dino,group=dino,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis",subtitle ="entre le jour N et le jour N-1",x="Episode de bloom au jour N",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=dino,group=dino,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Episode de bloom au jour N",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~dino, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('N_N-1_Dino_NDino_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_bloom_beta, cluster == 1)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$dino)
DunnTest(data_kw$`Bray-Curtis`~data_kw$dino,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 2)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$dino)
DunnTest(data_kw$`Bray-Curtis`~data_kw$dino,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 3)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$dino)
DunnTest(data_kw$`Bray-Curtis`~data_kw$dino,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 4)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$dino)
DunnTest(data_kw$`Bray-Curtis`~data_kw$dino,method="BH")


data_kw <- filter(data_bloom_beta, dino == "Dinophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta, dino == "Autre nature")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta, dino == "Non bloom")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

#Bloom de Diatomees 

data_bloom_beta$bac <- ifelse(data_bloom_beta$Bloom_Phylum == "Bac", "Bacillariophyceae","Autre nature")
data_bloom_beta[is.na(data_bloom_beta$bac),]$bac <- "Non bloom"

data_kw <- filter(data_bloom_beta)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$bac)
DunnTest(data_kw$`Bray-Curtis`~data_kw$bac,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=bac,group=bac,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis",subtitle ="entre le jour N et le jour N-1",x="Episode de bloom au jour N",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=bac,group=bac,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Episode de bloom au jour N",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~bac, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('N_N-1_Bac_NBac_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_bloom_beta, cluster == 1)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$bac)
DunnTest(data_kw$`Bray-Curtis`~data_kw$bac,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 2)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$bac)
DunnTest(data_kw$`Bray-Curtis`~data_kw$dino,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 3)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$bac)
DunnTest(data_kw$`Bray-Curtis`~data_kw$dino,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 4)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$bac)
DunnTest(data_kw$`Bray-Curtis`~data_kw$dino,method="BH")


data_kw <- filter(data_bloom_beta, bac == "Bacillariophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta, bac == "Autre nature")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta, bac == "Non bloom")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

# Bloom par saison
data_bloom_beta <- data_bloom_beta %>%
  mutate(Month = month(Date, label = F))

data_bloom_beta <- data_bloom_beta |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))


data_kw <- filter(data_bloom_beta, EpBloom == "OUI")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$season)
DunnTest(data_kw$`Bray-Curtis`~data_kw$season,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_bloom_beta, EpBloom == "OUI"))+
  geom_boxplot(aes(x=season,group=season,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis",subtitle ="entre le bloom au jour N et le jour N-1",x="Saison",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_bloom_beta,EpBloom == "OUI"))+
  geom_boxplot(aes(x=season,group=season,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Saison",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_bloom_beta, EpBloom=="OUI"))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~season, ncol = 2)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Saison")

(a+c)/b
ggsave('N_N-1_Saison.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_bloom_beta, cluster == 1 & EpBloom == "OUI")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$season)
DunnTest(data_kw$`Bray-Curtis`~data_kw$season,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 2 & EpBloom == "OUI")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$season)
DunnTest(data_kw$`Bray-Curtis`~data_kw$season,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 3 & EpBloom == "OUI")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$season)
DunnTest(data_kw$`Bray-Curtis`~data_kw$season,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 4 & EpBloom == "OUI")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$season)
DunnTest(data_kw$`Bray-Curtis`~data_kw$season,method="BH")


data_kw <- filter(data_bloom_beta,EpBloom == "OUI" & season == "Summer")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta,EpBloom == "OUI" & season == "Fall")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta,EpBloom == "OUI" & season == "Spring")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta,EpBloom == "OUI" & season == "Winter")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)



###
data_bloom_beta$md <- ifelse(data_bloom_beta$P_dominance >= 63.11, "Monospecifique","Mixte")
data_bloom_beta[is.na(data_bloom_beta$md),]$md <- "Non bloom"

data_kw <- filter(data_bloom_beta)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$md)
DunnTest(data_kw$`Bray-Curtis`~data_kw$md,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=md,group=md,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis",subtitle ="entre le jour N et le jour N-1",x="Episode de bloom au jour N",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=md,group=md,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Episode de bloom au jour N",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~md, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('N_N-1_MD_NMD_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_bloom_beta, cluster == 1)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$md)
DunnTest(data_kw$`Bray-Curtis`~data_kw$md,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 2)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$md)
DunnTest(data_kw$`Bray-Curtis`~data_kw$md,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 3)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$md)
DunnTest(data_kw$`Bray-Curtis`~data_kw$md,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 4)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$md)
DunnTest(data_kw$`Bray-Curtis`~data_kw$md,method="BH")


data_kw <- filter(data_bloom_beta, md == "Mixte")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta, md == "Monospecifique")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta, md == "Non bloom")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")


# Bloom Dino,Bac, Autres
data_bloom_beta$comp <- ifelse(data_bloom_beta$Bloom_Phylum == "Bac", "Bacillariophyceae","Autre nature")
data_bloom_beta[grep("Dino",x = data_bloom_beta$Bloom_Phylum),]$comp <- "Dinophyceae"
data_bloom_beta[is.na(data_bloom_beta$comp),]$comp <- "Non bloom"

data_kw <- filter(data_bloom_beta)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$comp)
DunnTest(data_kw$`Bray-Curtis`~data_kw$comp,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=comp,group=comp,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis",subtitle ="entre le jour N et le jour N-1",x="Episode de bloom au jour N",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=comp,group=comp,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Episode de bloom au jour N",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~comp, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('N_N-1_Comp_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_bloom_beta, cluster == 1)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$comp)
DunnTest(data_kw$`Bray-Curtis`~data_kw$comp,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 2)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$comp)
DunnTest(data_kw$`Bray-Curtis`~data_kw$comp,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 3)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$comp)
DunnTest(data_kw$`Bray-Curtis`~data_kw$comp,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 4)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$comp)
DunnTest(data_kw$`Bray-Curtis`~data_kw$md,method="BH")


data_kw <- filter(data_bloom_beta, comp == "Bacillariophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta, comp == "Dinophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta, comp == "Autre nature")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")



#### ENTRE LA DATE N ET N+1 ####
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

data_count <- data[,c(2,8,24:247)]
data_count[is.na(data_count)] <- 0
data_hel <- decostand(data_count[,-c(1,2)], method = "hellinger")
data_hel <- bind_cols(data_count[,c(1,2)],data_hel)

data_results_beta <- c("","")
data_results_beta <- as.data.frame(data_results_beta)

i = 1
data_station <- filter(data_hel, Code_point_Libelle == levels(as.factor(data_hel$Code_point_Libelle))[i])
for (j in 1:(nrow(data_station)-1)){
  Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
  
  Date1 <- data_station[j,"Date"]$Date
  Date2 <- data_station[j+1,"Date"]$Date
  Com1 <- data_station[j,c(3:226)]
  rownames(Com1) <- Date1
  Com2 <- data_station[j+1,c(3:226)]
  rownames(Com2) <- Date2
  BC <- vegdist(rbind(Com2, Com1), method = "bray")
  Delta <- paste0(j,"-",j+1)
  if (Date2-Date1 <= 21){
  data_results_beta[j,1] <- Station
  data_results_beta[j,2] <- Delta
  data_results_beta[j,3] <- as.numeric(BC)
  data_results_beta[j,4] <- Date1}
  else { data_results_beta[j,1] <- Station
  data_results_beta[j,2] <- Delta
  data_results_beta[j,3] <- NA
  data_results_beta[j,4] <- Date1
    }
} 
for (i in 2:length(levels(as.factor(data_hel$Code_point_Libelle)))){
  data_station <- filter(data_hel, Code_point_Libelle == levels(as.factor(data_hel$Code_point_Libelle))[i])
  data_results_beta2 <- c("","")
  data_results_beta2 <- as.data.frame(data_results_beta2)
  for (j in 1:(nrow(data_station)-1)){
    Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
    
    Date1 <- data_station[j,"Date"]$Date
    Date2 <- data_station[j+1,"Date"]$Date
    Com1 <- data_station[j,c(3:226)]
    rownames(Com1) <- Date1
    Com2 <- data_station[j+1,c(3:226)]
    rownames(Com2) <- Date2
    BC <- vegdist(rbind(Com2, Com1), method = "bray")
    Delta <- paste0(j,"-",j+1)
    if (Date2-Date1 <= 21){
      data_results_beta2[j,1] <- Station
      data_results_beta2[j,2] <- Delta
      data_results_beta2[j,3] <- as.numeric(BC)
      data_results_beta2[j,4] <- Date1}
    else { 
    data_results_beta2[j,1] <- Station
    data_results_beta2[j,2] <- Delta
    data_results_beta2[j,3] <- NA
    data_results_beta2[j,4] <- Date1
    }
  }
  colnames(data_results_beta) <- c("Code_point_Libelle","Lag","Bray-Curtis","Date1")
  colnames(data_results_beta2) <- c("Code_point_Libelle","Lag","Bray-Curtis","Date1")
  data_results_beta <- rbind(data_results_beta,data_results_beta2)
}

data_results_beta <- filter(data_results_beta, Code_point_Libelle != "")

write.csv2(data_results_beta,file="data_modif/data_div_betaN+1.csv", row.names = FALSE,dec = ".")

beta <- read_delim("data_modif/data_div_betaN+1.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

colnames(beta)[4] <- "Date"

data <- dplyr::select(data, Code_point_Libelle,ID.interne.passage, cluster,TEMP,Bloom,Date)

data_join <- left_join(data,beta)

doublons <- data_join[duplicated(data_join$ID.interne.passage) |
                        duplicated(data_join$ID.interne.passage, fromLast = TRUE), ]

# Filtre des doublons hydro :
resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

# On supprime les lignes en doublon dans le jeu de données initial
data_unique <- subset(data_join, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
# On les remets ces doublons filtres
data_join <- bind_rows(data_unique,resultat_filtre)
# On remet au propre
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

data_beta <- dplyr::select(data_join, - TEMP)
data_beta$EpBloom <- "OUI"
data_beta[is.na(data_beta$Bloom),8] <- "NON"

write.csv2(data_beta,file="data_modif/data_div_beta_ok_N1.csv", row.names = FALSE,dec = ".")
# On a comme ca les indices de bray-curtis associé à chaque date avec les informations de bloom s'il y a

#### Comparaison N et N+1 ####

### !!! Modification exterme pour dire quel bloom a prendre sur data_div_beta_ok_N1.csv

beta <- read_delim("data_modif/data_div_beta_ok_N1.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

data_bloom_beta <- left_join(beta,data_bloom)

doublons <- data_bloom_beta[duplicated(data_bloom_beta$ID.interne.passage) |
                        duplicated(data_bloom_beta$ID.interne.passage, fromLast = TRUE), ]

# Filtre des doublons hydro :
resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

# On supprime les lignes en doublon dans le jeu de données initial
data_unique <- subset(data_bloom_beta, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
# On les remets ces doublons filtres
data_bloom_beta <- bind_rows(data_unique,resultat_filtre)
# On remet au propre
data_bloom_beta <- data_bloom_beta |>
  arrange(Code_point_Libelle, Date)


wilcox.test(filter(data_bloom_beta, EpBloom == "OUI")$`Bray-Curtis`,
            filter(data_bloom_beta, EpBloom == "NON")$`Bray-Curtis`)


cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_bloom_beta, EpBloom == "OUI" | EpBloom == "NON"))+
  geom_boxplot(aes(x=EpBloom,group=EpBloom,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(title="Indice de Bray-Curtis 
entre le jour N et le jour N+1",x="Episode de bloom au jour N",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_bloom_beta, EpBloom == "OUI" | EpBloom == "NON"))+
  geom_boxplot(aes(x=EpBloom,group=EpBloom,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Episode de bloom au jour N",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_bloom_beta, EpBloom == "OUI" | EpBloom == "NON"))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~EpBloom, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('N_N+1_B_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

wilcox.test(filter(data_bloom_beta, EpBloom == "OUI" & cluster == 1)$`Bray-Curtis`,
            filter(data_bloom_beta, EpBloom == "NON" & cluster == 1)$`Bray-Curtis`)

wilcox.test(filter(data_bloom_beta, EpBloom == "OUI" & cluster == 2)$`Bray-Curtis`,
            filter(data_bloom_beta, EpBloom == "NON" & cluster == 2)$`Bray-Curtis`)

wilcox.test(filter(data_bloom_beta, EpBloom == "OUI" & cluster == 3)$`Bray-Curtis`,
            filter(data_bloom_beta, EpBloom == "NON" & cluster == 3)$`Bray-Curtis`)

wilcox.test(filter(data_bloom_beta, EpBloom == "OUI" & cluster == 4)$`Bray-Curtis`,
            filter(data_bloom_beta, EpBloom == "NON" & cluster == 4)$`Bray-Curtis`)

data_kw <- filter(data_bloom_beta, EpBloom == "OUI")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta, EpBloom == "NON")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH") 

# Mtn on ne s'interresse toujours qu'au dernier evenement de bloom
data_bloom_beta <- filter(data_bloom_beta, EpBloom == "OUI" | EpBloom == "NON")

data_bloom_beta$dino <- ifelse(data_bloom_beta$Bloom_Phylum == "Dino", "Dinophyceae","Autre nature")
data_bloom_beta[is.na(data_bloom_beta$dino),]$dino <- "Non bloom"

data_kw <- filter(data_bloom_beta)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$dino)
DunnTest(data_kw$`Bray-Curtis`~data_kw$dino,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=dino,group=dino,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis
entre le jour N et le jour N+1",x="Episode de bloom au jour N",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=dino,group=dino,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Episode de bloom au jour N",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~dino, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('N_N+1_Dino_NDino_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_bloom_beta, cluster == 1)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$dino)
DunnTest(data_kw$`Bray-Curtis`~data_kw$dino,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 2)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$dino)
DunnTest(data_kw$`Bray-Curtis`~data_kw$dino,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 3)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$dino)
DunnTest(data_kw$`Bray-Curtis`~data_kw$dino,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 4)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$dino)
DunnTest(data_kw$`Bray-Curtis`~data_kw$dino,method="BH")


data_kw <- filter(data_bloom_beta, dino == "Dinophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta, dino == "Autre nature")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta, dino == "Non bloom")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

#Bloom de Diatomees 

data_bloom_beta$bac <- ifelse(data_bloom_beta$Bloom_Phylum == "Bac", "Bacillariophyceae","Autre nature")
data_bloom_beta[is.na(data_bloom_beta$bac),]$bac <- "Non bloom"

data_kw <- filter(data_bloom_beta)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$bac)
DunnTest(data_kw$`Bray-Curtis`~data_kw$bac,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=bac,group=bac,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis
entre le jour N et le jour N+1",x="Episode de bloom au jour N",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=bac,group=bac,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Episode de bloom au jour N",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~bac, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('N_N+1_Bac_NBac_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_bloom_beta, cluster == 1)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$bac)
DunnTest(data_kw$`Bray-Curtis`~data_kw$bac,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 2)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$bac)
DunnTest(data_kw$`Bray-Curtis`~data_kw$dino,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 3)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$bac)
DunnTest(data_kw$`Bray-Curtis`~data_kw$dino,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 4)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$bac)
DunnTest(data_kw$`Bray-Curtis`~data_kw$dino,method="BH")


data_kw <- filter(data_bloom_beta, bac == "Bacillariophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta, bac == "Autre nature")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta, bac == "Non bloom")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

# Bloom par saison
data_bloom_beta <- data_bloom_beta %>%
  mutate(Month = month(Date, label = F))

data_bloom_beta <- data_bloom_beta |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))


data_kw <- filter(data_bloom_beta, EpBloom == "OUI")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$season)
DunnTest(data_kw$`Bray-Curtis`~data_kw$season,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_bloom_beta, EpBloom == "OUI"))+
  geom_boxplot(aes(x=season,group=season,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis
entre le bloom au jour N et le jour N+1",x="Saison",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_bloom_beta,EpBloom == "OUI"))+
  geom_boxplot(aes(x=season,group=season,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Saison",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_bloom_beta, EpBloom=="OUI"))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~season, ncol = 2)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Saison")

(a+c)/b
ggsave('N_N+1_Saison.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_bloom_beta, cluster == 1 & EpBloom == "OUI")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$season)
DunnTest(data_kw$`Bray-Curtis`~data_kw$season,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 2 & EpBloom == "OUI")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$season)
DunnTest(data_kw$`Bray-Curtis`~data_kw$season,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 3 & EpBloom == "OUI")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$season)
DunnTest(data_kw$`Bray-Curtis`~data_kw$season,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 4 & EpBloom == "OUI")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$season)
DunnTest(data_kw$`Bray-Curtis`~data_kw$season,method="BH")


data_kw <- filter(data_bloom_beta,EpBloom == "OUI" & season == "Summer")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta,EpBloom == "OUI" & season == "Fall")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta,EpBloom == "OUI" & season == "Spring")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta,EpBloom == "OUI" & season == "Winter")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)



###
data_bloom_beta$md <- ifelse(data_bloom_beta$P_dominance >= 63.11, "Monospecifique","Mixte")
data_bloom_beta[is.na(data_bloom_beta$md),]$md <- "Non bloom"

data_kw <- filter(data_bloom_beta)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$md)
DunnTest(data_kw$`Bray-Curtis`~data_kw$md,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=md,group=md,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis
entre le jour N et le jour N+1",x="Episode de bloom au jour N",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=md,group=md,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Episode de bloom au jour N",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~md, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('N_N+1_MD_NMD_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_bloom_beta, cluster == 1)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$md)
DunnTest(data_kw$`Bray-Curtis`~data_kw$md,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 2)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$md)
DunnTest(data_kw$`Bray-Curtis`~data_kw$md,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 3)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$md)
DunnTest(data_kw$`Bray-Curtis`~data_kw$md,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 4)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$md)
DunnTest(data_kw$`Bray-Curtis`~data_kw$md,method="BH")


data_kw <- filter(data_bloom_beta, md == "Mixte")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta, md == "Monospecifique")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta, md == "Non bloom")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")


# Bloom Dino,Bac, Autres
data_bloom_beta$comp <- ifelse(data_bloom_beta$Bloom_Phylum == "Bac", "Bacillariophyceae","Autre nature")
data_bloom_beta[grep("Dino",x = data_bloom_beta$Bloom_Phylum),]$comp <- "Dinophyceae"
data_bloom_beta[is.na(data_bloom_beta$comp),]$comp <- "Non bloom"

data_kw <- filter(data_bloom_beta)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$comp)
DunnTest(data_kw$`Bray-Curtis`~data_kw$comp,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=comp,group=comp,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis
entre le jour N et le jour N+1",x="Episode de bloom au jour N",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=comp,group=comp,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Episode de bloom au jour N",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~comp, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('N_N+1_Comp_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_bloom_beta, cluster == 1)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$comp)
DunnTest(data_kw$`Bray-Curtis`~data_kw$comp,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 2)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$comp)
DunnTest(data_kw$`Bray-Curtis`~data_kw$comp,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 3)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$comp)
DunnTest(data_kw$`Bray-Curtis`~data_kw$comp,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 4)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$comp)
DunnTest(data_kw$`Bray-Curtis`~data_kw$md,method="BH")


data_kw <- filter(data_bloom_beta, comp == "Bacillariophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta, comp == "Dinophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta, comp == "Autre nature")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")





#### ENTRE LA DATE N-1 ET N+1 ####
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

data_count <- data[,c(2,8,24:247)]
data_count[is.na(data_count)] <- 0
data_hel <- decostand(data_count[,-c(1,2)], method = "hellinger")
data_hel <- bind_cols(data_count[,c(1,2)],data_hel)

data_results_beta <- c("","")
data_results_beta <- as.data.frame(data_results_beta)

i = 1
data_station <- filter(data_hel, Code_point_Libelle == levels(as.factor(data_hel$Code_point_Libelle))[i])
for (j in 2:(nrow(data_station)-1)){
  Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
  
  Date1 <- data_station[j-1,"Date"]$Date
  Date2 <- data_station[j+1,"Date"]$Date
  Com1 <- data_station[j-1,c(3:226)]
  rownames(Com1) <- Date1
  Com2 <- data_station[j+1,c(3:226)]
  rownames(Com2) <- Date2
  BC <- vegdist(rbind(Com2, Com1), method = "bray")
  Delta <- paste0(j-1,"-",j+1)
  if (Date2-Date1 <= 42){
    data_results_beta[j-1,1] <- Station
    data_results_beta[j-1,2] <- Delta
    data_results_beta[j-1,3] <- as.numeric(BC)
    data_results_beta[j-1,4] <- data_station[j,"Date"]$Date}
  else { 
  data_results_beta[j-1,1] <- Station
  data_results_beta[j-1,2] <- Delta
  data_results_beta[j-1,3] <- NA
  data_results_beta[j-1,4] <- data_station[j,"Date"]$Date
  }
} 
for (i in 2:length(levels(as.factor(data_hel$Code_point_Libelle)))){
  data_station <- filter(data_hel, Code_point_Libelle == levels(as.factor(data_hel$Code_point_Libelle))[i])
  data_results_beta2 <- c("","")
  data_results_beta2 <- as.data.frame(data_results_beta2)
  for (j in 2:(nrow(data_station)-1)){
    Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
    
    Date1 <- data_station[j-1,"Date"]$Date
    Date2 <- data_station[j+1,"Date"]$Date
    Com1 <- data_station[j,c(3:226)]
    rownames(Com1) <- Date1
    Com2 <- data_station[j+1,c(3:226)]
    rownames(Com2) <- Date2
    BC <- vegdist(rbind(Com2, Com1), method = "bray")
    Delta <- paste0(j-1,"-",j+1)
    if (Date2-Date1 <= 42){
      data_results_beta2[j-1,1] <- Station
      data_results_beta2[j-1,2] <- Delta
      data_results_beta2[j-1,3] <- as.numeric(BC)
      data_results_beta2[j-1,4] <- data_station[j,"Date"]$Date}
    else { 
      data_results_beta2[j-1,1] <- Station
      data_results_beta2[j-1,2] <- Delta
      data_results_beta2[j-1,3] <- NA
      data_results_beta2[j-1,4] <- data_station[j,"Date"]$Date
    }
  }
  colnames(data_results_beta) <- c("Code_point_Libelle","Lag","Bray-Curtis","Date1")
  colnames(data_results_beta2) <- c("Code_point_Libelle","Lag","Bray-Curtis","Date1")
  data_results_beta <- rbind(data_results_beta,data_results_beta2)
}

data_results_beta <- filter(data_results_beta, Code_point_Libelle != "")

write.csv2(data_results_beta,file="data_modif/data_div_betaN-1_N+1.csv", row.names = FALSE,dec = ".")

beta <- read_delim("data_modif/data_div_betaN-1_N+1.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

colnames(beta)[4] <- "Date"

data <- dplyr::select(data, Code_point_Libelle,ID.interne.passage, cluster,TEMP,Bloom,Date)

data_join <- left_join(data,beta)

doublons <- data_join[duplicated(data_join$ID.interne.passage) |
                        duplicated(data_join$ID.interne.passage, fromLast = TRUE), ]

# Filtre des doublons hydro :
resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

# On supprime les lignes en doublon dans le jeu de données initial
data_unique <- subset(data_join, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
# On les remets ces doublons filtres
data_join <- bind_rows(data_unique,resultat_filtre)
# On remet au propre
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

data_beta <- dplyr::select(data_join, - TEMP)
data_beta$EpBloom <- "OUI"
data_beta[is.na(data_beta$Bloom),8] <- "NON"

write.csv2(data_beta,file="data_modif/data_div_beta_ok_N-1_N+1.csv", row.names = FALSE,dec = ".")
# On a comme ca les indices de bray-curtis associé à chaque date avec les informations de bloom s'il y a

### !!! Modification exterme pour dire quel bloom a prendre sur data_div_beta_ok_N-1_N+1.csv

#### Comparaison N-1 et N+1 ####
beta <- read_delim("data_modif/data_div_beta_ok_N-1_N+1.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

data_bloom_beta <- left_join(beta,data_bloom)

doublons <- data_bloom_beta[duplicated(data_bloom_beta$ID.interne.passage) |
                              duplicated(data_bloom_beta$ID.interne.passage, fromLast = TRUE), ]

# Filtre des doublons hydro :
resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

# On supprime les lignes en doublon dans le jeu de données initial
data_unique <- subset(data_bloom_beta, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
# On les remets ces doublons filtres
data_bloom_beta <- bind_rows(data_unique,resultat_filtre)
# On remet au propre
data_bloom_beta <- data_bloom_beta |>
  arrange(Code_point_Libelle, Date)


wilcox.test(filter(data_bloom_beta, EpBloom == "OUI")$`Bray-Curtis`,
            filter(data_bloom_beta, EpBloom == "NON")$`Bray-Curtis`)


cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_bloom_beta, EpBloom == "OUI" | EpBloom == "NON"))+
  geom_boxplot(aes(x=EpBloom,group=EpBloom,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(title="Indice de Bray-Curtis 
entre le jour N-1 et le jour N+1",x="Episode de bloom au jour N",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_bloom_beta, EpBloom == "OUI" | EpBloom == "NON"))+
  geom_boxplot(aes(x=EpBloom,group=EpBloom,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Episode de bloom au jour N",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_bloom_beta, EpBloom == "OUI" | EpBloom == "NON"))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~EpBloom, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('N-1_N+1_B_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

wilcox.test(filter(data_bloom_beta, EpBloom == "OUI" & cluster == 1)$`Bray-Curtis`,
            filter(data_bloom_beta, EpBloom == "NON" & cluster == 1)$`Bray-Curtis`)

wilcox.test(filter(data_bloom_beta, EpBloom == "OUI" & cluster == 2)$`Bray-Curtis`,
            filter(data_bloom_beta, EpBloom == "NON" & cluster == 2)$`Bray-Curtis`)

wilcox.test(filter(data_bloom_beta, EpBloom == "OUI" & cluster == 3)$`Bray-Curtis`,
            filter(data_bloom_beta, EpBloom == "NON" & cluster == 3)$`Bray-Curtis`)

wilcox.test(filter(data_bloom_beta, EpBloom == "OUI" & cluster == 4)$`Bray-Curtis`,
            filter(data_bloom_beta, EpBloom == "NON" & cluster == 4)$`Bray-Curtis`)

data_kw <- filter(data_bloom_beta, EpBloom == "OUI")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta, EpBloom == "NON")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH") 

# Mtn on ne s'interresse toujours qu'aux episodes de bloom sans succession
data_bloom_beta <- filter(data_bloom_beta, EpBloom == "OUI" | EpBloom == "NON")

data_bloom_beta$dino <- ifelse(data_bloom_beta$Bloom_Phylum == "Dino", "Dinophyceae","Autre nature")
data_bloom_beta[is.na(data_bloom_beta$dino),]$dino <- "Non bloom"

data_kw <- filter(data_bloom_beta)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$dino)
DunnTest(data_kw$`Bray-Curtis`~data_kw$dino,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=dino,group=dino,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis
entre le jour N-1 et le jour N+1",x="Episode de bloom au jour N",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=dino,group=dino,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Episode de bloom au jour N",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~dino, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('N-1_N+1_Dino_NDino_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_bloom_beta, cluster == 1)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$dino)
DunnTest(data_kw$`Bray-Curtis`~data_kw$dino,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 2)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$dino)
DunnTest(data_kw$`Bray-Curtis`~data_kw$dino,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 3)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$dino)
DunnTest(data_kw$`Bray-Curtis`~data_kw$dino,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 4)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$dino)
DunnTest(data_kw$`Bray-Curtis`~data_kw$dino,method="BH")


data_kw <- filter(data_bloom_beta, dino == "Dinophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta, dino == "Autre nature")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta, dino == "Non bloom")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

#Bloom de Diatomees 

data_bloom_beta$bac <- ifelse(data_bloom_beta$Bloom_Phylum == "Bac", "Bacillariophyceae","Autre nature")
data_bloom_beta[is.na(data_bloom_beta$bac),]$bac <- "Non bloom"

data_kw <- filter(data_bloom_beta)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$bac)
DunnTest(data_kw$`Bray-Curtis`~data_kw$bac,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=bac,group=bac,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis
entre le jour N-1 et le jour N+1",x="Episode de bloom au jour N",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=bac,group=bac,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Episode de bloom au jour N",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~bac, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('N-1_N+1_Bac_NBac_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_bloom_beta, cluster == 1)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$bac)
DunnTest(data_kw$`Bray-Curtis`~data_kw$bac,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 2)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$bac)
DunnTest(data_kw$`Bray-Curtis`~data_kw$dino,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 3)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$bac)
DunnTest(data_kw$`Bray-Curtis`~data_kw$dino,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 4)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$bac)
DunnTest(data_kw$`Bray-Curtis`~data_kw$dino,method="BH")


data_kw <- filter(data_bloom_beta, bac == "Bacillariophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta, bac == "Autre nature")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta, bac == "Non bloom")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

# Bloom par saison
data_bloom_beta <- data_bloom_beta %>%
  mutate(Month = month(Date, label = F))

data_bloom_beta <- data_bloom_beta |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))


data_kw <- filter(data_bloom_beta, EpBloom == "OUI")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$season)
DunnTest(data_kw$`Bray-Curtis`~data_kw$season,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_bloom_beta, EpBloom == "OUI"))+
  geom_boxplot(aes(x=season,group=season,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis
entre le bloom au jour N-1 et le jour N+1",x="Saison",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_bloom_beta,EpBloom == "OUI"))+
  geom_boxplot(aes(x=season,group=season,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Saison",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_bloom_beta, EpBloom=="OUI"))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~season, ncol = 2)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Saison")

(a+c)/b
ggsave('N-1_N+1_Saison.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_bloom_beta, cluster == 1 & EpBloom == "OUI")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$season)
DunnTest(data_kw$`Bray-Curtis`~data_kw$season,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 2 & EpBloom == "OUI")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$season)
DunnTest(data_kw$`Bray-Curtis`~data_kw$season,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 3 & EpBloom == "OUI")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$season)
DunnTest(data_kw$`Bray-Curtis`~data_kw$season,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 4 & EpBloom == "OUI")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$season)
DunnTest(data_kw$`Bray-Curtis`~data_kw$season,method="BH")


data_kw <- filter(data_bloom_beta,EpBloom == "OUI" & season == "Summer")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta,EpBloom == "OUI" & season == "Fall")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta,EpBloom == "OUI" & season == "Spring")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta,EpBloom == "OUI" & season == "Winter")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)



###
data_bloom_beta$md <- ifelse(data_bloom_beta$P_dominance >= 63.11, "Monospecifique","Mixte")
data_bloom_beta[is.na(data_bloom_beta$md),]$md <- "Non bloom"

data_kw <- filter(data_bloom_beta)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$md)
DunnTest(data_kw$`Bray-Curtis`~data_kw$md,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=md,group=md,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis
entre le jour N-1 et le jour N+1",x="Episode de bloom au jour N",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=md,group=md,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Episode de bloom au jour N",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~md, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.25))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('N-1_N+1_MD_NMD_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_bloom_beta, cluster == 1)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$md)
DunnTest(data_kw$`Bray-Curtis`~data_kw$md,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 2)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$md)
DunnTest(data_kw$`Bray-Curtis`~data_kw$md,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 3)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$md)
DunnTest(data_kw$`Bray-Curtis`~data_kw$md,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 4)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$md)
DunnTest(data_kw$`Bray-Curtis`~data_kw$md,method="BH")


data_kw <- filter(data_bloom_beta, md == "Mixte")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta, md == "Monospecifique")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta, md == "Non bloom")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")


# Bloom Dino,Bac, Autres
data_bloom_beta$comp <- ifelse(data_bloom_beta$Bloom_Phylum == "Bac", "Bacillariophyceae","Autre nature")
data_bloom_beta[grep("Dino",x = data_bloom_beta$Bloom_Phylum),]$comp <- "Dinophyceae"
data_bloom_beta[is.na(data_bloom_beta$comp),]$comp <- "Non bloom"

data_kw <- filter(data_bloom_beta)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$comp)
DunnTest(data_kw$`Bray-Curtis`~data_kw$comp,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=comp,group=comp,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis
entre le jour N-1 et le jour N+1",x="Episode de bloom au jour N",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=comp,group=comp,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Episode de bloom au jour N",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_bloom_beta))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~comp, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('N-1_N+1_Comp_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_bloom_beta, cluster == 1)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$comp)
DunnTest(data_kw$`Bray-Curtis`~data_kw$comp,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 2)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$comp)
DunnTest(data_kw$`Bray-Curtis`~data_kw$comp,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 3)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$comp)
DunnTest(data_kw$`Bray-Curtis`~data_kw$comp,method="BH")

data_kw <- filter(data_bloom_beta, cluster == 4)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$comp)
DunnTest(data_kw$`Bray-Curtis`~data_kw$md,method="BH")


data_kw <- filter(data_bloom_beta, comp == "Bacillariophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta, comp == "Dinophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta, comp == "Autre nature")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

### Comparaison avant/apres/entreN-1_N+1 ####
beta <- read_delim("data_modif/data_div_beta_ok_N-1.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

avant <- left_join(beta,data_bloom)

beta <- read_delim("data_modif/data_div_beta_ok_N1.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

apres <- left_join(beta,data_bloom)

beta <- read_delim("data_modif/data_div_beta_ok_N-1_N+1.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

NN1 <- left_join(beta,data_bloom)

avant <- filter(avant, EpBloom == "OUI") #268
apres <- filter(apres, EpBloom == "OUI") #270
NN1 <- filter(NN1, EpBloom == "OUI") #209

avant$cat <- "N vs N-1"
apres$cat <- "N vs N+1"
NN1$cat <- "N-1 vs N+1"

data_comp <- rbind(avant,apres,NN1)

data_kw <- filter(data_comp,Bloom_Phylum == "Bac")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_comp, Bloom_Phylum == "Bac"))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis
bloom de Bacillariophyceae au jour N",x="Comparaison",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_comp, Bloom_Phylum == "Bac"))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Comparaison",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_comp,Bloom_Phylum == "Bac"))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~cat, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('Comp_Bac_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_comp, cluster == 1 & Bloom_Phylum == "Bac")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 2 & Bloom_Phylum == "Bac")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 3 & Bloom_Phylum == "Bac")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 4 & Bloom_Phylum == "Bac")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, Bloom_Phylum == "Bac" & cat == "N vs N-1")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp, Bloom_Phylum == "Bac" & cat == "N vs N+1")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp, Bloom_Phylum == "Bac" & cat == "N-1 vs N+1")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")



data_kw <- filter(data_comp,Bloom_Phylum == "Dino")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_comp, Bloom_Phylum == "Dino"))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis
bloom de Dinophyceae au jour N",x="Comparaison",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_comp, Bloom_Phylum == "Dino"))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Comparaison",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_comp, Bloom_Phylum == "Dino"))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~cat, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('Comp_Dino_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_comp, cluster == 1 & Bloom_Phylum == "Dino")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 2 & Bloom_Phylum == "Dino")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 3 & Bloom_Phylum == "Dino")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 4 & Bloom_Phylum == "Dino")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, Bloom_Phylum == "Dino" & cat == "N vs N-1")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp, Bloom_Phylum == "Dino" & cat == "N vs N+1")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp, Bloom_Phylum == "Dino" & cat == "N-1 vs N+1")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")


data_kw <- filter(data_comp,Bloom_Phylum != "Dino" & Bloom_Phylum != "Bac")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_comp, Bloom_Phylum != "Dino" & Bloom_Phylum != "Bac"))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis bloom non
Dino- ou Bacillario- phyceae au jour N",x="Comparaison",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_comp, Bloom_Phylum != "Dino" & Bloom_Phylum != "Bac"))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Comparaison",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_comp, Bloom_Phylum != "Dino" & Bloom_Phylum != "Bac"))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~cat, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('Comp_AN_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_comp, cluster == 1 & Bloom_Phylum != "Dino" & Bloom_Phylum != "Bac")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 2 & Bloom_Phylum != "Dino" & Bloom_Phylum != "Bac")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 3 & Bloom_Phylum != "Dino" & Bloom_Phylum != "Bac")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 4 & Bloom_Phylum != "Dino" & Bloom_Phylum != "Bac")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp,Bloom_Phylum != "Dino" & Bloom_Phylum != "Bac" & cat == "N vs N-1")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp, Bloom_Phylum != "Dino" & Bloom_Phylum != "Bac" & cat == "N vs N+1")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp, Bloom_Phylum != "Dino" & Bloom_Phylum != "Bac" & cat == "N-1 vs N+1")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")


data_kw <- filter(data_comp)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_comp))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis
bloom au jour N",x="Comparaison",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_comp))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Comparaison",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_comp))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~cat, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('Comp_Bloom_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_comp, cluster == 1)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 2 )
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 3 )
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 4)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cat == "N vs N-1")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp, cat == "N vs N+1")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp, cat == "N-1 vs N+1")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")


data_kw <- filter(data_comp, P_dominance >= 63.11)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_comp , P_dominance >= 63.11))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis
bloom monospecifique au jour N",x="Comparaison",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_comp, P_dominance >= 63.11))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Comparaison",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_comp, P_dominance >= 63.11))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~cat, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('Comp_MD_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_comp, cluster == 1, P_dominance >= 63.11)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 2 , P_dominance >= 63.11 )
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 3, P_dominance >= 63.11 )
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 4 , P_dominance >= 63.11)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cat == "N vs N-1" , P_dominance >= 63.11)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp, cat == "N vs N+1", P_dominance >= 63.11)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp, cat == "N-1 vs N+1", P_dominance >= 63.11)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")


data_kw <- filter(data_comp, P_dominance <= 63.11)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_comp , P_dominance <= 63.11))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis
bloom mixte au jour N",x="Comparaison",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_comp, P_dominance <= 63.11))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Comparaison",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_comp, P_dominance <= 63.11))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~cat, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('Comp_NMD_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_comp, cluster == 1, P_dominance <= 63.11)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 2 , P_dominance <= 63.11 )
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 3, P_dominance <= 63.11 )
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 4 , P_dominance <= 63.11)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cat == "N vs N-1" , P_dominance <= 63.11)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp, cat == "N vs N+1", P_dominance <= 63.11)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp, cat == "N-1 vs N+1", P_dominance <= 63.11)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp, season == "Winter")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_comp , season == "Winter"))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis
bloom d'hiver au jour N",x="Comparaison",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_comp, season == "Winter"))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Comparaison",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_comp, season == "Winter"))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~cat, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('Comp_Hiver_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_comp, cluster == 1, season == "Winter")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 2 , season == "Winter" )
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 3, season == "Winter" )
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 4 , season == "Winter")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cat == "N vs N-1" , season == "Winter")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp, cat == "N vs N+1", season == "Winter")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp, cat == "N-1 vs N+1", season == "Winter")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")


data_kw <- filter(data_comp, season == "Fall")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_comp , season == "Fall"))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis
bloom d'automne au jour N",x="Comparaison",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_comp, season == "Fall"))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Comparaison",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_comp, season == "Fall"))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~cat, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('Comp_Automne_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_comp, cluster == 1, season == "Fall")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 2 , season == "Fall" )
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 3, season == "Fall" )
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 4 , season == "Fall")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cat == "N vs N-1" , season == "Fall")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp, cat == "N vs N+1", season == "Fall")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp, cat == "N-1 vs N+1", season == "Fall")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")



data_kw <- filter(data_comp, season == "Spring")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_comp , season == "Spring"))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis
bloom de printemps au jour N",x="Comparaison",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_comp, season == "Spring"))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Comparaison",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_comp, season == "Spring"))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~cat, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('Comp_Printemps_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_comp, cluster == 1, season == "Spring")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 2 , season == "Spring" )
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 3, season == "Spring" )
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 4 , season == "Spring")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cat == "N vs N-1" , season == "Spring")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp, cat == "N vs N+1", season == "Spring")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp, cat == "N-1 vs N+1", season == "Spring")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")


data_kw <- filter(data_comp, season == "Summer")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_comp , season == "Summer"))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis
bloom d'été au jour N",x="Comparaison",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_comp, season == "Summer"))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Comparaison",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_comp, season == "Summer"))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~cat, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('Comp_Ete_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_comp, cluster == 1, season == "Summer")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 2 , season == "Summer" )
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 3, season == "Summer" )
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 4 , season == "Summer")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cat == "N vs N-1" , season == "Summer")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp, cat == "N vs N+1", season == "Summer")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp, cat == "N-1 vs N+1", season == "Summer")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")


# Non bloom 
beta <- read_delim("data_modif/data_div_beta_ok_N-1.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

avant <- left_join(beta,data_bloom)

beta <- read_delim("data_modif/data_div_beta_ok_N1.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

apres <- left_join(beta,data_bloom)

beta <- read_delim("data_modif/data_div_beta_ok_N-1_N+1.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

NN1 <- left_join(beta,data_bloom)

avant <- filter(avant, EpBloom == "NON") #7232
apres <- filter(apres, EpBloom == "NON") #7232
NN1 <- filter(NN1, EpBloom == "NON") #7232

avant$cat <- "N vs N-1"
apres$cat <- "N vs N+1"
NN1$cat <- "N-1 vs N+1"

data_comp <- rbind(avant,apres,NN1)
data_kw <- filter(data_comp)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_comp))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis
non bloom au jour N",x="Comparaison",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_comp))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Comparaison",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_comp))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~cat, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('Comp_NonBloom_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_comp, cluster == 1)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 2 )
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 3 )
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cluster == 4)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp, cat == "N vs N-1")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp, cat == "N vs N+1")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp, cat == "N-1 vs N+1")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

ggplot(filter(data_comp))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Cluster",y="Indice de Bray-Curtis",
       title = "Indice de Bray-Curtis")
ggsave('Cluster.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 200, units = 'mm')

