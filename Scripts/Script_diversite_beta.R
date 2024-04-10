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

for (i in 2:nrow(data_beta)){
  if (data_beta$EpBloom[i] == "OUI" & (data_beta$EpBloom[i-1] == "OUI" | data_beta$EpBloom[i-1] == "Sucession") ){
    data_beta$EpBloom[i] <- "Sucession"
  }
}

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
  mutate(Year = year(Date)) |>
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

datam <- summarise(group_by(beta,Month,cluster,Year), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(datam)+
  geom_boxplot(aes(x=Month,y=BC,group=Month),size = 1)+
  #geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  #scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  facet_wrap(~cluster)+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-1",x="Mois")
ggsave('Cluster_Mois.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


#### Comparaison N et N-1 ####
beta <- read_delim("data_modif/data_div_beta_ok_N-1.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
                                                                        `Bray-Curtis` = col_number()), locale = locale(decimal_mark = ",", 
                                                                                                                       grouping_mark = "."), trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

data_bloom_beta <- left_join(beta,data_bloom)

data_bloom_beta <- data_bloom_beta %>% 
  mutate(Week = week(Date)) %>%
  mutate(Month = month(Date)) %>%
  mutate(Year = year(Date)) |>
  # 'ceiling' takes the upper integer of a decimal number
  mutate(Fortnight = ceiling(Week/2)) %>%
  arrange(Fortnight)

data_bloom_beta <- data_bloom_beta |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))


# Bloom Dino,Bac, Non bloom
data_bloom_beta$comp <- ifelse(data_bloom_beta$Bloom_Phylum == "Bac", "Bacillariophyceae","Autre nature")
data_bloom_beta[grep("Dino",x = data_bloom_beta$Bloom_Phylum),]$comp <- "Dinophyceae"
data_bloom_beta[is.na(data_bloom_beta$comp),]$comp <- "Non bloom"
data_bloom_beta <- filter(data_bloom_beta,comp == "Non bloom" | comp == "Dinophyceae" | comp == "Bacillariophyceae")

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
entre le jour N et le jour N-1",x="Episode de bloom au jour N",y="Indice de Bray-Curtis")
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

data_kw <- filter(data_bloom_beta, comp == "Non bloom")
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

for (i in 1:nrow(data_beta)){
  if (data_beta$EpBloom[i] == "OUI" & (data_beta$EpBloom[i+1] == "OUI" | data_beta$EpBloom[i+1] == "Sucession") ){
    data_beta$EpBloom[i] <- "Sucession"
  }
}

write.csv2(data_beta,file="data_modif/data_div_beta_ok_N1.csv", row.names = FALSE,dec = ".")
# On a comme ca les indices de bray-curtis associé à chaque date avec les informations de bloom s'il y a

#### Comparaison N et N+1 ####

beta <- read_delim("data_modif/data_div_beta_ok_N1.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
                                                                        `Bray-Curtis` = col_number()), locale = locale(decimal_mark = ",", 
                                                                                                                       grouping_mark = "."), trim_ws = TRUE)
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



cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

data_bloom_beta <- data_bloom_beta %>% 
  mutate(Week = week(Date)) %>%
  mutate(Month = month(Date)) %>%
  mutate(Year = year(Date)) |>
  # 'ceiling' takes the upper integer of a decimal number
  mutate(Fortnight = ceiling(Week/2)) %>%
  arrange(Fortnight)

data_bloom_beta <- data_bloom_beta |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))


# Bloom Dino,Bac, Non bloom
data_bloom_beta$comp <- ifelse(data_bloom_beta$Bloom_Phylum == "Bac", "Bacillariophyceae","Autre nature")
data_bloom_beta[grep("Dino",x = data_bloom_beta$Bloom_Phylum),]$comp <- "Dinophyceae"
data_bloom_beta[is.na(data_bloom_beta$comp),]$comp <- "Non bloom"
data_bloom_beta <- filter(data_bloom_beta,comp == "Non bloom" | comp == "Dinophyceae" | comp == "Bacillariophyceae")

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
DunnTest(data_kw$`Bray-Curtis`~data_kw$comp,method="BH")


data_kw <- filter(data_bloom_beta, comp == "Bacillariophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta, comp == "Dinophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta, comp == "Non bloom")
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
    Com1 <- data_station[j-1,c(3:226)]
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

for (i in 1:nrow(data_beta)){
  if (data_beta$EpBloom[i] == "OUI" & (data_beta$EpBloom[i+1] == "OUI" | data_beta$EpBloom[i+1] == "Sucession") ){
    data_beta$EpBloom[i] <- "Sucession"
  }
}

for (i in 2:nrow(data_beta)){
  if (data_beta$EpBloom[i] == "OUI" & (data_beta$EpBloom[i-1] == "OUI" | data_beta$EpBloom[i-1] == "Sucession") ){
    data_beta$EpBloom[i] <- "Sucession"
  }
}

write.csv2(data_beta,file="data_modif/data_div_beta_ok_N-1_N+1.csv", row.names = FALSE,dec = ".")
# On a comme ca les indices de bray-curtis associé à chaque date avec les informations de bloom s'il y a

#### Comparaison N-1 et N+1 ####
beta <- read_delim("data_modif/data_div_beta_ok_N-1_N+1.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
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


cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

data_bloom_beta <- data_bloom_beta %>% 
  mutate(Week = week(Date)) %>%
  mutate(Month = month(Date)) %>%
  mutate(Year = year(Date)) |>
  # 'ceiling' takes the upper integer of a decimal number
  mutate(Fortnight = ceiling(Week/2)) %>%
  arrange(Fortnight)

data_bloom_beta <- data_bloom_beta |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))


# Bloom Dino,Bac, Non bloom
data_bloom_beta$comp <- ifelse(data_bloom_beta$Bloom_Phylum == "Bac", "Bacillariophyceae","Autre nature")
data_bloom_beta[grep("Dino",x = data_bloom_beta$Bloom_Phylum),]$comp <- "Dinophyceae"
data_bloom_beta[is.na(data_bloom_beta$comp),]$comp <- "Non bloom"
data_bloom_beta <- filter(data_bloom_beta,comp == "Non bloom" | comp == "Dinophyceae" | comp == "Bacillariophyceae")

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
DunnTest(data_kw$`Bray-Curtis`~data_kw$comp,method="BH")


data_kw <- filter(data_bloom_beta, comp == "Bacillariophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta, comp == "Dinophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta, comp == "Non bloom")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")


### Comparaison avant/apres/entreN-1_N+1 ####
beta <- read_delim("data_modif/data_div_beta_ok_N-1.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

avant <- left_join(beta,data_bloom)

beta <- read_delim("data_modif/data_div_beta_ok_N1.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

apres <- left_join(beta,data_bloom)

beta <- read_delim("data_modif/data_div_beta_ok_N-1_N+1.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
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

data_comp <- data_comp %>% 
  mutate(Week = week(Date)) %>%
  mutate(Month = month(Date)) %>%
  mutate(Year = year(Date)) |>
  # 'ceiling' takes the upper integer of a decimal number
  mutate(Fortnight = ceiling(Week/2)) %>%
  arrange(Fortnight)

data_comp <- data_comp |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))


# Bloom Dino,Bac, Non bloom
data_comp$comp <- ifelse(data_comp$Bloom_Phylum == "Bac", "Bacillariophyceae","Autre nature")
data_comp[grep("Dino",x = data_comp$Bloom_Phylum),]$comp <- "Dinophyceae"
data_comp[is.na(data_comp$comp),]$comp <- "Non bloom"
data_comp <- filter(data_comp,comp == "Non bloom" | comp == "Dinophyceae" | comp == "Bacillariophyceae")

data_comp_bac <- filter(data_comp,comp=="Bacillariophyceae")
data_kw <- filter(data_comp_bac)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_comp_bac))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis bloom
Bacillariophyceae au jour N",x="Comparaison",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_comp_bac))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Episode de bloom au jour N",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_comp_bac))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~cat, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Comparaison",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('Comp_bac.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_comp_bac, cluster == 1)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp_bac, cluster == 2)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp_bac, cluster == 3)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp_bac, cluster == 4)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")


data_kw <- filter(data_comp_bac, cat == "N vs N-1")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp_bac, cat == "N vs N+1")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_comp_bac, cat == "N-1 vs N+1")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_comp_dino <- filter(data_comp,comp=="Dinophyceae")
data_kw <- filter(data_comp_dino)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_comp_dino))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis bloom
Dinophyceae au jour N",x="Comparaison",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_comp_dino))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Episode de bloom au jour N",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_comp_dino))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~cat, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Comparaison",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('Comp_dino.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_comp_dino, cluster == 1)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp_dino, cluster == 2)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp_dino, cluster == 3)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp_dino, cluster == 4)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")


data_kw <- filter(data_comp_dino, cat == "N vs N-1")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp_dino, cat == "N vs N+1")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_comp_dino, cat == "N-1 vs N+1")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")



#### test de Mantel ####
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)
data$Rspe <- rowSums(data[,c(24:247)] != 0,na.rm = T)
# Communauté 
data_count <- data[,c(2,8,24:247)]
# Remplace les NA par 0
data_count[is.na(data_count)] <- 0
# Transformation d'Hellinger pour ne s'interesser qu'a la composition
data_hel <- decostand(data_count[,-c(1,2)], method = "hellinger")

# Parametre abiotique
env <- data$TEMP
# Distance
geo <- data.frame(data$lon,data$lat)

#abundance data frame - bray curtis dissimilarity
dist.abund = vegdist(data_hel, method = "bray")

#environmental vector - euclidean distance
dist.temp = dist(env, method = "euclidean")


firstPoints <- SpatialPoints(coords = cbind(as.numeric(data$lat),as.numeric(data$lon)))
dist.geo <- sp::spDists(firstPoints,longlat = FALSE)

#abundance vs environmental 
abund_temp = mantel(dist.abund, dist.temp, method = "spearman", permutations = 100, na.rm = TRUE)
abund_temp

env <- data$SALI
dist.sali = dist(env, method = "euclidean")
abund_sali = mantel(dist.abund, dist.sali, method = "spearman", permutations = 100, na.rm = TRUE)
abund_sali

env <- data$CHLOROA
dist.chloro = dist(env, method = "euclidean")
abund_chloro = mantel(dist.abund, dist.chloro, method = "spearman", permutations = 100, na.rm = TRUE)
abund_chloro

env <- data$NH4
dist.NH4 = dist(env, method = "euclidean")
abund_NH4 = mantel(dist.abund, dist.NH4, method = "spearman", permutations = 100, na.rm = TRUE)
abund_NH4

env <- data$`NO3+NO2`
dist.NO3NO2 = dist(env, method = "euclidean")
abund_NO3NO2 = mantel(dist.abund, dist.NO3NO2, method = "spearman", permutations = 100, na.rm = TRUE)
abund_NO3NO2

env <- data$OXYGENE
dist.oxy = dist(env, method = "euclidean")
abund_oxy = mantel(dist.abund, dist.oxy, method = "spearman", permutations = 100, na.rm = TRUE)
abund_oxy

env <- data$PO4
dist.PO4 = dist(env, method = "euclidean")
abund_PO4 = mantel(dist.abund, dist.PO4, method = "spearman", permutations = 100, na.rm = TRUE)
abund_PO4

env <- data$TURB
dist.TURB = dist(env, method = "euclidean")
abund_TURB = mantel(dist.abund, dist.TURB, method = "spearman", permutations = 100, na.rm = TRUE)
abund_TURB

env <- data$`TURB-FNU`
dist.TURBFNU = dist(env, method = "euclidean")
abund_TURBFNU = mantel(dist.abund, dist.TURBFNU, method = "spearman", permutations = 100, na.rm = TRUE)
abund_TURBFNU

env <- data$SIOH
dist.SIOH = dist(env, method = "euclidean")
abund_SIOH = mantel(dist.abund, dist.SIOH, method = "spearman", permutations = 100, na.rm = TRUE)
abund_SIOH

abund_geo = mantel(dist.abund, dist.geo, method = "spearman", permutations = 100, na.rm = TRUE)
abund_geo


#### PERMANOVA ####
adonis2(formula = dist(data$Shannon,method = "euclidean") ~ Month : Year : cluster, permutations = 100, method = "euclidean",data=data)
adonis2(formula = dist(data$Shannon,method = "euclidean") ~ Month : Year , permutations = 100, method = "euclidean",data=data)
adonis2(formula = dist(data$Shannon,method = "euclidean") ~ Month , permutations = 100, method = "euclidean",data=data)
adonis2(formula = dist(data$Shannon,method = "euclidean") ~ Year, permutations = 100, method = "euclidean",data=data)
adonis2(formula = dist(data$Shannon,method = "euclidean") ~ cluster, permutations = 100, method = "euclidean",data=data)
adonis2(formula = dist(data$Shannon,method = "euclidean") ~ Month:cluster, permutations = 100, method = "euclidean",data=data)

adonis2(formula = dist(data$Rspe,method = "euclidean") ~ Month : Year : cluster, permutations = 100, method = "euclidean",data=data)
adonis2(formula = dist(data$Rspe,method = "euclidean") ~ Month : Year , permutations = 100, method = "euclidean",data=data)
adonis2(formula = dist(data$Rspe,method = "euclidean") ~ Month , permutations = 100, method = "euclidean",data=data)
adonis2(formula = dist(data$Rspe,method = "euclidean") ~ Year, permutations = 100, method = "euclidean",data=data)
adonis2(formula = dist(data$Rspe,method = "euclidean") ~ cluster, permutations = 100, method = "euclidean",data=data)
adonis2(formula = dist(data$Rspe,method = "euclidean") ~ Month:cluster, permutations = 100, method = "euclidean",data=data)



adonis2(formula = dist(data[complete.cases(data$Pielou),]$Pielou,method = "euclidean") ~ Month : Year : cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$Pielou),])
adonis2(formula = dist(data[complete.cases(data$Pielou),]$Pielou,method = "euclidean") ~ Month : Year , permutations = 100, method = "euclidean",data=data[complete.cases(data$Pielou),])
adonis2(formula = dist(data[complete.cases(data$Pielou),]$Pielou,method = "euclidean") ~ Month , permutations = 100, method = "euclidean",data=data[complete.cases(data$Pielou),])
adonis2(formula = dist(data[complete.cases(data$Pielou),]$Pielou,method = "euclidean") ~ Year, permutations = 100, method = "euclidean",data=data[complete.cases(data$Pielou),])
adonis2(formula = dist(data[complete.cases(data$Pielou),]$Pielou,method = "euclidean") ~ cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$Pielou),])
adonis2(formula = dist(data[complete.cases(data$Pielou),]$Pielou,method = "euclidean") ~ Month:cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$Pielou),])

adonis2(formula = dist(data[complete.cases(data$BergerParker),]$BergerParker,method = "euclidean") ~ Month : Year : cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$BergerParker),])
adonis2(formula = dist(data[complete.cases(data$BergerParker),]$BergerParker,method = "euclidean") ~ Month : Year , permutations = 100, method = "euclidean",data=data[complete.cases(data$BergerParker),])
adonis2(formula = dist(data[complete.cases(data$BergerParker),]$BergerParker,method = "euclidean") ~ Month , permutations = 100, method = "euclidean",data=data[complete.cases(data$BergerParker),])
adonis2(formula = dist(data[complete.cases(data$BergerParker),]$BergerParker,method = "euclidean") ~ Year, permutations = 100, method = "euclidean",data=data[complete.cases(data$BergerParker),])
adonis2(formula = dist(data[complete.cases(data$BergerParker),]$BergerParker,method = "euclidean") ~ cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$BergerParker),])
adonis2(formula = dist(data[complete.cases(data$BergerParker),]$BergerParker,method = "euclidean") ~ Month:cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$BergerParker),])

adonis2(formula = dist(data[complete.cases(data$TEMP),]$TEMP,method = "euclidean") ~ Month : Year : cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$TEMP),])
adonis2(formula = dist(data[complete.cases(data$TEMP),]$TEMP,method = "euclidean") ~ Month : Year , permutations = 100, method = "euclidean",data=data[complete.cases(data$TEMP),])
adonis2(formula = dist(data[complete.cases(data$TEMP),]$TEMP,method = "euclidean") ~ Month , permutations = 100, method = "euclidean",data=data[complete.cases(data$TEMP),])
adonis2(formula = dist(data[complete.cases(data$TEMP),]$TEMP,method = "euclidean") ~ Year, permutations = 100, method = "euclidean",data=data[complete.cases(data$TEMP),])
adonis2(formula = dist(data[complete.cases(data$TEMP),]$TEMP,method = "euclidean") ~ cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$TEMP),])
adonis2(formula = dist(data[complete.cases(data$TEMP),]$TEMP,method = "euclidean") ~ Month:cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$TEMP),])


adonis2(formula = dist(data[complete.cases(data$SALI),]$SALI,method = "euclidean") ~ Month : Year : cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$SALI),])
adonis2(formula = dist(data[complete.cases(data$SALI),]$SALI,method = "euclidean") ~ Month : Year , permutations = 100, method = "euclidean",data=data[complete.cases(data$SALI),])
adonis2(formula = dist(data[complete.cases(data$SALI),]$SALI,method = "euclidean") ~ Month , permutations = 100, method = "euclidean",data=data[complete.cases(data$SALI),])
adonis2(formula = dist(data[complete.cases(data$SALI),]$SALI,method = "euclidean") ~ Year, permutations = 100, method = "euclidean",data=data[complete.cases(data$SALI),])
adonis2(formula = dist(data[complete.cases(data$SALI),]$SALI,method = "euclidean") ~ cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$SALI),])
adonis2(formula = dist(data[complete.cases(data$SALI),]$SALI,method = "euclidean") ~ Month:cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$SALI),])


adonis2(formula = dist(data[complete.cases(data$NH4),]$NH4,method = "euclidean") ~ Month : Year : cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$NH4),])
adonis2(formula = dist(data[complete.cases(data$NH4),]$NH4,method = "euclidean") ~ Month : Year , permutations = 100, method = "euclidean",data=data[complete.cases(data$NH4),])
adonis2(formula = dist(data[complete.cases(data$NH4),]$NH4,method = "euclidean") ~ Month , permutations = 100, method = "euclidean",data=data[complete.cases(data$NH4),])
adonis2(formula = dist(data[complete.cases(data$NH4),]$NH4,method = "euclidean") ~ Year, permutations = 100, method = "euclidean",data=data[complete.cases(data$NH4),])
adonis2(formula = dist(data[complete.cases(data$NH4),]$NH4,method = "euclidean") ~ cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$NH4),])
adonis2(formula = dist(data[complete.cases(data$NH4),]$NH4,method = "euclidean") ~ Month:cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$NH4),])

adonis2(formula = dist(data[complete.cases(data$`NO3+NO2`),]$`NO3+NO2`,method = "euclidean") ~ Month : Year : cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$`NO3+NO2`),])
adonis2(formula = dist(data[complete.cases(data$`NO3+NO2`),]$`NO3+NO2`,method = "euclidean") ~ Month : Year , permutations = 100, method = "euclidean",data=data[complete.cases(data$`NO3+NO2`),])
adonis2(formula = dist(data[complete.cases(data$`NO3+NO2`),]$`NO3+NO2`,method = "euclidean") ~ Month , permutations = 100, method = "euclidean",data=data[complete.cases(data$`NO3+NO2`),])
adonis2(formula = dist(data[complete.cases(data$`NO3+NO2`),]$`NO3+NO2`,method = "euclidean") ~ Year, permutations = 100, method = "euclidean",data=data[complete.cases(data$`NO3+NO2`),])
adonis2(formula = dist(data[complete.cases(data$`NO3+NO2`),]$`NO3+NO2`,method = "euclidean") ~ cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$`NO3+NO2`),])
adonis2(formula = dist(data[complete.cases(data$`NO3+NO2`),]$`NO3+NO2`,method = "euclidean") ~ Month:cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$`NO3+NO2`),])

adonis2(formula = dist(data[complete.cases(data$OXYGENE),]$OXYGENE,method = "euclidean") ~ Month : Year : cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$OXYGENE),])
adonis2(formula = dist(data[complete.cases(data$OXYGENE),]$OXYGENE,method = "euclidean") ~ Month : Year , permutations = 100, method = "euclidean",data=data[complete.cases(data$OXYGENE),])
adonis2(formula = dist(data[complete.cases(data$OXYGENE),]$OXYGENE,method = "euclidean") ~ Month , permutations = 100, method = "euclidean",data=data[complete.cases(data$OXYGENE),])
adonis2(formula = dist(data[complete.cases(data$OXYGENE),]$OXYGENE,method = "euclidean") ~ Year, permutations = 100, method = "euclidean",data=data[complete.cases(data$OXYGENE),])
adonis2(formula = dist(data[complete.cases(data$OXYGENE),]$OXYGENE,method = "euclidean") ~ cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$OXYGENE),])
adonis2(formula = dist(data[complete.cases(data$OXYGENE),]$OXYGENE,method = "euclidean") ~ Month : cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$OXYGENE),])


adonis2(formula = dist(data[complete.cases(data$PO4),]$PO4,method = "euclidean") ~ Month : Year : cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$PO4),])
adonis2(formula = dist(data[complete.cases(data$PO4),]$PO4,method = "euclidean") ~ Month : Year , permutations = 100, method = "euclidean",data=data[complete.cases(data$PO4),])
adonis2(formula = dist(data[complete.cases(data$PO4),]$PO4,method = "euclidean") ~ Month , permutations = 100, method = "euclidean",data=data[complete.cases(data$PO4),])
adonis2(formula = dist(data[complete.cases(data$PO4),]$PO4,method = "euclidean") ~ Year, permutations = 100, method = "euclidean",data=data[complete.cases(data$PO4),])
adonis2(formula = dist(data[complete.cases(data$PO4),]$PO4,method = "euclidean") ~ cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$PO4),])
adonis2(formula = dist(data[complete.cases(data$PO4),]$PO4,method = "euclidean") ~ Month:cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$PO4),])

adonis2(formula = dist(data[complete.cases(data$TURB),]$TURB,method = "euclidean") ~ Month : Year : cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$TURB),])
adonis2(formula = dist(data[complete.cases(data$TURB),]$TURB,method = "euclidean") ~ Month : Year , permutations = 100, method = "euclidean",data=data[complete.cases(data$TURB),])
adonis2(formula = dist(data[complete.cases(data$TURB),]$TURB,method = "euclidean") ~ Month , permutations = 100, method = "euclidean",data=data[complete.cases(data$TURB),])
adonis2(formula = dist(data[complete.cases(data$TURB),]$TURB,method = "euclidean") ~ Year, permutations = 100, method = "euclidean",data=data[complete.cases(data$TURB),])
adonis2(formula = dist(data[complete.cases(data$TURB),]$TURB,method = "euclidean") ~ cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$TURB),])
adonis2(formula = dist(data[complete.cases(data$TURB),]$TURB,method = "euclidean") ~ Month:cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$TURB),])

adonis2(formula = dist(data[complete.cases(data$`TURB-FNU`),]$`TURB-FNU`,method = "euclidean") ~ Month : Year : cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$`TURB-FNU`),])
adonis2(formula = dist(data[complete.cases(data$`TURB-FNU`),]$`TURB-FNU`,method = "euclidean") ~ Month : Year , permutations = 100, method = "euclidean",data=data[complete.cases(data$`TURB-FNU`),])
adonis2(formula = dist(data[complete.cases(data$`TURB-FNU`),]$`TURB-FNU`,method = "euclidean") ~ Month , permutations = 100, method = "euclidean",data=data[complete.cases(data$`TURB-FNU`),])
adonis2(formula = dist(data[complete.cases(data$`TURB-FNU`),]$`TURB-FNU`,method = "euclidean") ~ Year, permutations = 100, method = "euclidean",data=data[complete.cases(data$`TURB-FNU`),])
adonis2(formula = dist(data[complete.cases(data$`TURB-FNU`),]$`TURB-FNU`,method = "euclidean") ~ cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$`TURB-FNU`),])
adonis2(formula = dist(data[complete.cases(data$`TURB-FNU`),]$`TURB-FNU`,method = "euclidean") ~ Month : cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$`TURB-FNU`),])


adonis2(formula = dist(data[complete.cases(data$SIOH),]$SIOH,method = "euclidean") ~ Month : Year : cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$SIOH),])
adonis2(formula = dist(data[complete.cases(data$SIOH),]$SIOH,method = "euclidean") ~ Month : Year , permutations = 100, method = "euclidean",data=data[complete.cases(data$SIOH),])
adonis2(formula = dist(data[complete.cases(data$SIOH),]$SIOH,method = "euclidean") ~ Month , permutations = 100, method = "euclidean",data=data[complete.cases(data$SIOH),])
adonis2(formula = dist(data[complete.cases(data$SIOH),]$SIOH,method = "euclidean") ~ Year, permutations = 100, method = "euclidean",data=data[complete.cases(data$SIOH),])
adonis2(formula = dist(data[complete.cases(data$SIOH),]$SIOH,method = "euclidean") ~ cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$SIOH),])
adonis2(formula = dist(data[complete.cases(data$SIOH),]$SIOH,method = "euclidean") ~ Month : cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$SIOH),])


adonis2(formula = dist(data[complete.cases(data$CHLOROA),]$CHLOROA,method = "euclidean") ~ Month : Year : cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$CHLOROA),])
adonis2(formula = dist(data[complete.cases(data$CHLOROA),]$CHLOROA,method = "euclidean") ~ Month : Year , permutations = 100, method = "euclidean",data=data[complete.cases(data$CHLOROA),])
adonis2(formula = dist(data[complete.cases(data$CHLOROA),]$CHLOROA,method = "euclidean") ~ Month , permutations = 100, method = "euclidean",data=data[complete.cases(data$CHLOROA),])
adonis2(formula = dist(data[complete.cases(data$CHLOROA),]$CHLOROA,method = "euclidean") ~ Year, permutations = 100, method = "euclidean",data=data[complete.cases(data$CHLOROA),])
adonis2(formula = dist(data[complete.cases(data$CHLOROA),]$CHLOROA,method = "euclidean") ~ cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$CHLOROA),])
adonis2(formula = dist(data[complete.cases(data$CHLOROA),]$CHLOROA,method = "euclidean") ~ Month : cluster, permutations = 100, method = "euclidean",data=data[complete.cases(data$CHLOROA),])


data_count <- data[,c(2,8,24:247)]
# Remplace les NA par 0
data_count[is.na(data_count)] <- 0
# Transformation d'Hellinger pour ne s'interesser qu'a la composition
data_count <- data_count[rowSums(data_count[,-c(1,2)]) > 0,]
data_hel <- decostand(data_count[,-c(1,2)], method = "hellinger")

data_count <- data[,c(2,8,24:247)]
# Remplace les NA par 0
data_count[is.na(data_count)] <- 0

adonis2(formula = vegdist(data_hel[complete.cases(data$CHLOROA),],method = "euclidean") ~ CHLOROA, permutations = 100, method = "euclidean",data=data[complete.cases(data$CHLOROA),])

data_count <- data_count[rowSums(data_count != 0) > 0,]

adonis2(formula = vegdist(data_hel,method = "bray") ~ Month : Year : cluster, permutations = 100, method = "bray",data=data[rowSums(data_count[,-c(1,2)]) > 0,])
adonis2(formula = vegdist(data_hel,method = "bray") ~ Month : Year , permutations = 100, method = "bray",data=data[rowSums(data_count[,-c(1,2)]) > 0,])
adonis2(formula = vegdist(data_hel,method = "bray") ~ Month , permutations = 100, method = "bray",data=data[rowSums(data_count[,-c(1,2)]) > 0,])
adonis2(formula = vegdist(data_hel,method = "bray") ~ Year, permutations = 100, method = "bray",data=data[rowSums(data_count[,-c(1,2)]) > 0,])
adonis2(formula = vegdist(data_hel,method = "bray") ~ cluster, permutations = 100, method = "bray",data=data[rowSums(data_count[,-c(1,2)]) > 0,])
adonis2(formula = vegdist(data_hel,method = "bray") ~ Month : cluster, permutations = 100, method = "bray",data=data[rowSums(data_count[,-c(1,2)]) > 0,])

data2 <- data[rowSums(data_count[,-c(1,2)]) > 0,]
adonis2(formula = vegdist(data_hel[complete.cases(data2$CHLOROA),],method = "euclidean") ~ CHLOROA, permutations = 100, method = "euclidean",data=data2[complete.cases(data2$CHLOROA),])
adonis2(formula = vegdist(data_hel[complete.cases(data2$TEMP),],method = "euclidean") ~ TEMP, permutations = 100, method = "euclidean",data=data2[complete.cases(data2$TEMP),])
adonis2(formula = vegdist(data_hel[complete.cases(data2$SALI),],method = "euclidean") ~ SALI, permutations = 100, method = "euclidean",data=data2[complete.cases(data2$SALI),])
adonis2(formula = vegdist(data_hel[complete.cases(data2$NH4),],method = "euclidean") ~ NH4, permutations = 100, method = "euclidean",data=data2[complete.cases(data2$NH4),])
adonis2(formula = vegdist(data_hel[complete.cases(data2$`NO3+NO2`),],method = "euclidean") ~ `NO3+NO2`, permutations = 100, method = "euclidean",data=data2[complete.cases(data2$`NO3+NO2`),])
adonis2(formula = vegdist(data_hel[complete.cases(data2$TURB),],method = "euclidean") ~ TURB, permutations = 100, method = "euclidean",data=data2[complete.cases(data2$TURB),])
adonis2(formula = vegdist(data_hel[complete.cases(data2$`TURB-FNU`),],method = "euclidean") ~ `TURB-FNU`, permutations = 100, method = "euclidean",data=data2[complete.cases(data2$`TURB-FNU`),])
adonis2(formula = vegdist(data_hel[complete.cases(data2$PO4),],method = "euclidean") ~ PO4, permutations = 100, method = "euclidean",data=data2[complete.cases(data2$PO4),])
adonis2(formula = vegdist(data_hel[complete.cases(data2$SIOH),],method = "euclidean") ~ SIOH, permutations = 100, method = "euclidean",data=data2[complete.cases(data2$SIOH),])
adonis2(formula = vegdist(data_hel[complete.cases(data2$OXYGENE),],method = "euclidean") ~ OXYGENE, permutations = 100, method = "euclidean",data=data2[complete.cases(data2$OXYGENE),])



### ENTRE LA DATE N ET N-2 ####

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

i = 1
  data_station <- filter(data_hel, Code_point_Libelle == levels(as.factor(data_hel$Code_point_Libelle))[i])
  for (j in 3:nrow(data_station)){
  Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle

  Date1 <- data_station[j,"Date"]$Date
  Date2 <- data_station[j-2,"Date"]$Date
  Com1 <- data_station[j,c(3:226)]
  rownames(Com1) <- Date1
  Com2 <- data_station[j-2,c(3:226)]
  rownames(Com2) <- Date2
  BC <- vegdist(rbind(Com2, Com1), method = "bray")
  Delta <- paste0(j-2,"-",j)
  if (Date1-Date2 <= 42) {
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
      for (j in 3:nrow(data_station)){
        Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
        
        Date1 <- data_station[j,"Date"]$Date
        Date2 <- data_station[j-2,"Date"]$Date
        Com1 <- data_station[j,c(3:226)]
        rownames(Com1) <- Date1
        Com2 <- data_station[j-2,c(3:226)]
        rownames(Com2) <- Date2
        BC <- vegdist(rbind(Com2, Com1), method = "bray")
        Delta <- paste0(j-2,"-",j)
        if (Date1-Date2 <= 42) {
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
write.csv2(data_results_beta,file="data_modif/data_div_beta_N-2.csv", row.names = FALSE,dec = ".")

beta <- read_delim("data_modif/data_div_beta_N-2.csv", 
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

for (i in 3:nrow(data_beta)){
  if (data_beta$EpBloom[i] == "OUI" & (data_beta$EpBloom[i-2] == "OUI" | data_beta$EpBloom[i-2] == "Sucession" | data_beta$EpBloom[i-1] == "OUI" | data_beta$EpBloom[i-1] == "Sucession") ){
    data_beta$EpBloom[i] <- "Sucession"
  }
}

write.csv2(data_beta,file="data_modif/data_div_beta_ok_N-2.csv", row.names = FALSE,dec = ".")
# On a comme ca les indices de bray-curtis associé à chaque date avec les informations de bloom s'il y a

beta <- read_delim("data_modif/data_div_beta_ok_N-3.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)
#### SERIE TEMPORELLE DES INDICES DE BRAY-CURTIS ####

{
beta <- beta %>% 
  mutate(Week = week(Date)) %>%
  mutate(Month = month(Date)) %>%
  mutate(Year = year(Date)) |>
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
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Bois de la Chaise large",x="DateN")
ggsave('Boisdelachaiselarge.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Bois de la Chaise large",x="Quinzaine")
ggsave('Boisdelachaiselarge_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Auger",x="DateN")
ggsave('Auger.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Auger",x="Quinzaine")
ggsave('Auger_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Le Cornard",x="DateN")
ggsave('LeCornard.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Le Cornard",x="Quinzaine")
ggsave('LeCornard_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Teychan bis",x="DateN")
ggsave('Teychanbis.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Teychan bis",x="Quinzaine")
ggsave('Teychanbis_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Ouest Loscolo",x="DateN")
ggsave('Ouest Loscolo.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Ouest Loscolo",x="Quinzaine")
ggsave('OuestLoscolo_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Men er Roue",x="DateN")
ggsave('MenerRoue.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Men er Roue",x="Quinzaine")
ggsave('MenerRoue_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Loguivy",x="DateN")
ggsave('Loguivy.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Loguivy",x="Quinzaine")
ggsave('Loguivy_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "les Hébihens",x="DateN")
ggsave('lesHebihens.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "les Hébihens",x="Quinzaine")
ggsave('lesHebihens_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Géfosse",x="DateN")
ggsave('Géfosse.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Géfosse",x="Quinzaine")
ggsave('Géfosse_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Cabourg",x="DateN")
ggsave('Cabourg.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Cabourg",x="Quinzaine")
ggsave('Cabourg_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Antifer ponton pétrolier",x="DateN")
ggsave('Antifer ponton pétrolier.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Antifer ponton pétrolier",x="Quinzaine")
ggsave('Antifer ponton pétrolier_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "At so",x="DateN")
ggsave('Atso.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "At so",x="Quinzaine")
ggsave('Atso_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Point 1 Boulogne",x="DateN")
ggsave('Boulogne.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Point 1 Boulogne",x="Quinzaine")
ggsave('Boulogne_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Parc Leucate 2",x="DateN")
ggsave('ParcLeucate.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Parc Leucate 2",x="Quinzaine")
ggsave('ParcLeucate_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Barcares",x="DateN")
ggsave("Barcares.png",path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Barcares",x="Quinzaine")
ggsave('Barcares_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Sète mer",x="DateN")
ggsave('Sètemer.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Sète mer",x="Quinzaine")
ggsave('Sètemer_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Bouzigues (a)",x="DateN")
ggsave('Bouzigues.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Bouzigues (a)",x="Quinzaine")
ggsave('Bouzigues_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Anse de Carteau 2",x="DateN")
ggsave('Ansedecarteau.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Anse de Carteau 2",x="Quinzaine")
ggsave('Ansedecarteau_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "22B - Toulon gde rade",x="DateN")
ggsave('Toulon.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "22B - Toulon gde rade",x="Quinzaine")
ggsave('Toulon_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Calvi",x="DateN")
ggsave('Calvi.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Calvi",x="Quinzaine")
ggsave('Calvi_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Diana centre",x="DateN")
ggsave('Dianacentre.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))

ggplot(beta_g)+
  geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
  geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Diana centre",x="Quinzaine")
ggsave('Dianacentre_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(beta_g)+
  geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
  geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
       subtitle = "Diana centre",x="Mois")
ggsave('Dianacentre_Mois.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
}

datam <- summarise(group_by(beta,Month,cluster,Year), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(datam)+
  geom_boxplot(aes(x=Month,y=BC,group=Month),size = 1)+
  #geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  #scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  facet_wrap(~cluster)+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",x="Mois")
ggsave('Cluster_Mois_N-2.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


#### Comparaison N et N-2 ####
beta <- read_delim("data_modif/data_div_beta_ok_N-2.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
                                                                        `Bray-Curtis` = col_number()), locale = locale(decimal_mark = ",", 
                                                                                                                       grouping_mark = "."), trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

data_bloom_beta <- left_join(beta,data_bloom)

data_bloom_beta <- data_bloom_beta %>% 
  mutate(Week = week(Date)) %>%
  mutate(Month = month(Date)) %>%
  mutate(Year = year(Date)) |>
  # 'ceiling' takes the upper integer of a decimal number
  mutate(Fortnight = ceiling(Week/2)) %>%
  arrange(Fortnight)

data_bloom_beta <- data_bloom_beta |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))


# Bloom Dino,Bac, Non bloom
data_bloom_beta$comp <- ifelse(data_bloom_beta$Bloom_Phylum == "Bac", "Bacillariophyceae","Autre nature")
data_bloom_beta[grep("Dino",x = data_bloom_beta$Bloom_Phylum),]$comp <- "Dinophyceae"
data_bloom_beta[is.na(data_bloom_beta$comp),]$comp <- "Non bloom"
data_bloom_beta <- filter(data_bloom_beta,comp == "Non bloom" | comp == "Dinophyceae" | comp == "Bacillariophyceae")

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
entre le jour N et le jour N-2",x="Episode de bloom au jour N",y="Indice de Bray-Curtis")
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
ggsave('N_N-2_Comp_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

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
DunnTest(data_kw$`Bray-Curtis`~data_kw$comp,method="BH")


data_kw <- filter(data_bloom_beta, comp == "Bacillariophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta, comp == "Dinophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta, comp == "Non bloom")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")



#### ENTRE LA DATE N ET N+2 ####
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
for (j in 1:(nrow(data_station)-2)){
  Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
  
  Date1 <- data_station[j,"Date"]$Date
  Date2 <- data_station[j+2,"Date"]$Date
  Com1 <- data_station[j,c(3:226)]
  rownames(Com1) <- Date1
  Com2 <- data_station[j+2,c(3:226)]
  rownames(Com2) <- Date2
  BC <- vegdist(rbind(Com2, Com1), method = "bray")
  Delta <- paste0(j,"-",j+2)
  if (Date2-Date1 <= 42){
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
  for (j in 1:(nrow(data_station)-2)){
    Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
    
    Date1 <- data_station[j,"Date"]$Date
    Date2 <- data_station[j+2,"Date"]$Date
    Com1 <- data_station[j,c(3:226)]
    rownames(Com1) <- Date1
    Com2 <- data_station[j+2,c(3:226)]
    rownames(Com2) <- Date2
    BC <- vegdist(rbind(Com2, Com1), method = "bray")
    Delta <- paste0(j,"-",j+2)
    if (Date2-Date1 <= 42){
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

write.csv2(data_results_beta,file="data_modif/data_div_betaN+2.csv", row.names = FALSE,dec = ".")

beta <- read_delim("data_modif/data_div_betaN+2.csv", 
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

for (i in 1:nrow(data_beta)){
  if (data_beta$EpBloom[i] == "OUI" & (data_beta$EpBloom[i+2] == "OUI" | data_beta$EpBloom[i+2] == "Sucession" | data_beta$EpBloom[i+1] == "OUI" | data_beta$EpBloom[i+1] == "Sucession") ){
    data_beta$EpBloom[i] <- "Sucession"
  }
}

write.csv2(data_beta,file="data_modif/data_div_beta_ok_N2.csv", row.names = FALSE,dec = ".")
# On a comme ca les indices de bray-curtis associé à chaque date avec les informations de bloom s'il y a

#### Comparaison N et N+2 ####

beta <- read_delim("data_modif/data_div_beta_ok_N2.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
                                                                        `Bray-Curtis` = col_number()), locale = locale(decimal_mark = ",", 
                                                                                                                       grouping_mark = "."), trim_ws = TRUE)
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



cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

data_bloom_beta <- data_bloom_beta %>% 
  mutate(Week = week(Date)) %>%
  mutate(Month = month(Date)) %>%
  mutate(Year = year(Date)) |>
  # 'ceiling' takes the upper integer of a decimal number
  mutate(Fortnight = ceiling(Week/2)) %>%
  arrange(Fortnight)

data_bloom_beta <- data_bloom_beta |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))


# Bloom Dino,Bac, Non bloom
data_bloom_beta$comp <- ifelse(data_bloom_beta$Bloom_Phylum == "Bac", "Bacillariophyceae","Autre nature")
data_bloom_beta[grep("Dino",x = data_bloom_beta$Bloom_Phylum),]$comp <- "Dinophyceae"
data_bloom_beta[is.na(data_bloom_beta$comp),]$comp <- "Non bloom"
data_bloom_beta <- filter(data_bloom_beta,comp == "Non bloom" | comp == "Dinophyceae" | comp == "Bacillariophyceae")

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
entre le jour N et le jour N+2",x="Episode de bloom au jour N",y="Indice de Bray-Curtis")
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
ggsave('N_N+2_Comp_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

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
DunnTest(data_kw$`Bray-Curtis`~data_kw$comp,method="BH")


data_kw <- filter(data_bloom_beta, comp == "Bacillariophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta, comp == "Dinophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta, comp == "Non bloom")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")


#### ENTRE LA DATE N-2 ET N+2 ####
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
for (j in 3:(nrow(data_station)-2)){
  Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
  
  Date1 <- data_station[j-2,"Date"]$Date
  Date2 <- data_station[j+2,"Date"]$Date
  Com1 <- data_station[j-2,c(3:226)]
  rownames(Com1) <- Date1
  Com2 <- data_station[j+2,c(3:226)]
  rownames(Com2) <- Date2
  BC <- vegdist(rbind(Com2, Com1), method = "bray")
  Delta <- paste0(j-2,"-",j+2)
  if (Date2-Date1 <= 84){
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
  for (j in 3:(nrow(data_station)-2)){
    Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
    
    Date1 <- data_station[j-2,"Date"]$Date
    Date2 <- data_station[j+2,"Date"]$Date
    Com1 <- data_station[j-2,c(3:226)]
    rownames(Com1) <- Date1
    Com2 <- data_station[j+2,c(3:226)]
    rownames(Com2) <- Date2
    BC <- vegdist(rbind(Com2, Com1), method = "bray")
    Delta <- paste0(j-2,"-",j+2)
    if (Date2-Date1 <= 84){
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

write.csv2(data_results_beta,file="data_modif/data_div_betaN-2_N+2.csv", row.names = FALSE,dec = ".")

beta <- read_delim("data_modif/data_div_betaN-2_N+2.csv", 
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

for (i in 1:nrow(data_beta)){
  if (data_beta$EpBloom[i] == "OUI" & (data_beta$EpBloom[i+1] == "OUI" | data_beta$EpBloom[i+1] == "Sucession" | data_beta$EpBloom[i+2] == "OUI" | data_beta$EpBloom[i+2] == "Sucession") ){
    data_beta$EpBloom[i] <- "Sucession"
  }
}

for (i in 3:nrow(data_beta)){
  if (data_beta$EpBloom[i] == "OUI" & (data_beta$EpBloom[i-1] == "OUI" | data_beta$EpBloom[i-1] == "Sucession" | data_beta$EpBloom[i-2] == "OUI" | data_beta$EpBloom[i-2] == "Sucession") ){
    data_beta$EpBloom[i] <- "Sucession"
  }
}

write.csv2(data_beta,file="data_modif/data_div_beta_ok_N-2_N+2.csv", row.names = FALSE,dec = ".")
# On a comme ca les indices de bray-curtis associé à chaque date avec les informations de bloom s'il y a

#### Comparaison N-2 et N+2 ####
beta <- read_delim("data_modif/data_div_beta_ok_N-2_N+2.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
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


cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

data_bloom_beta <- data_bloom_beta %>% 
  mutate(Week = week(Date)) %>%
  mutate(Month = month(Date)) %>%
  mutate(Year = year(Date)) |>
  # 'ceiling' takes the upper integer of a decimal number
  mutate(Fortnight = ceiling(Week/2)) %>%
  arrange(Fortnight)

data_bloom_beta <- data_bloom_beta |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))


# Bloom Dino,Bac, Non bloom
data_bloom_beta$comp <- ifelse(data_bloom_beta$Bloom_Phylum == "Bac", "Bacillariophyceae","Autre nature")
data_bloom_beta[grep("Dino",x = data_bloom_beta$Bloom_Phylum),]$comp <- "Dinophyceae"
data_bloom_beta[is.na(data_bloom_beta$comp),]$comp <- "Non bloom"
data_bloom_beta <- filter(data_bloom_beta,comp == "Non bloom" | comp == "Dinophyceae" | comp == "Bacillariophyceae")

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
entre le jour N-2 et le jour N+2",x="Episode de bloom au jour N",y="Indice de Bray-Curtis")
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
ggsave('N-2_N+2_Comp_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

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
DunnTest(data_kw$`Bray-Curtis`~data_kw$comp,method="BH")


data_kw <- filter(data_bloom_beta, comp == "Bacillariophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta, comp == "Dinophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta, comp == "Non bloom")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")


### Comparaison avant/apres/entreN-2_N+2 ####
beta <- read_delim("data_modif/data_div_beta_ok_N-2.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

avant <- left_join(beta,data_bloom)

beta <- read_delim("data_modif/data_div_beta_ok_N2.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

apres <- left_join(beta,data_bloom)

beta <- read_delim("data_modif/data_div_beta_ok_N-2_N+2.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
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

avant$cat <- "N vs N-2"
apres$cat <- "N vs N+2"
NN1$cat <- "N-2 vs N+2"

data_comp <- rbind(avant,apres,NN1)

data_kw <- filter(data_comp,Bloom_Phylum == "Bac")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

data_comp <- data_comp %>% 
  mutate(Week = week(Date)) %>%
  mutate(Month = month(Date)) %>%
  mutate(Year = year(Date)) |>
  # 'ceiling' takes the upper integer of a decimal number
  mutate(Fortnight = ceiling(Week/2)) %>%
  arrange(Fortnight)

data_comp <- data_comp |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))


# Bloom Dino,Bac, Non bloom
data_comp$comp <- ifelse(data_comp$Bloom_Phylum == "Bac", "Bacillariophyceae","Autre nature")
data_comp[grep("Dino",x = data_comp$Bloom_Phylum),]$comp <- "Dinophyceae"
data_comp[is.na(data_comp$comp),]$comp <- "Non bloom"
data_comp <- filter(data_comp,comp == "Non bloom" | comp == "Dinophyceae" | comp == "Bacillariophyceae")

data_comp_bac <- filter(data_comp,comp=="Bacillariophyceae")
data_kw <- filter(data_comp_bac)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_comp_bac))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis bloom
Bacillariophyceae au jour N",x="Comparaison",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_comp_bac))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Episode de bloom au jour N",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_comp_bac))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~cat, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Comparaison",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('Comp_bac_N2.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_comp_bac, cluster == 1)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp_bac, cluster == 2)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp_bac, cluster == 3)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp_bac, cluster == 4)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")


data_kw <- filter(data_comp_bac, cat == "N vs N-2")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp_bac, cat == "N vs N+2")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_comp_bac, cat == "N-2 vs N+2")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_comp_dino <- filter(data_comp,comp=="Dinophyceae")
data_kw <- filter(data_comp_dino)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_comp_dino))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis bloom
Dinophyceae au jour N",x="Comparaison",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_comp_dino))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Episode de bloom au jour N",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_comp_dino))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~cat, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Comparaison",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('Comp_dinoN2.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_comp_dino, cluster == 1)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp_dino, cluster == 2)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp_dino, cluster == 3)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp_dino, cluster == 4)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")


data_kw <- filter(data_comp_dino, cat == "N vs N-2")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp_dino, cat == "N vs N+2")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_comp_dino, cat == "N-2 vs N+2")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")



### ENTRE LA DATE N ET N-3 ####

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

i = 1
data_station <- filter(data_hel, Code_point_Libelle == levels(as.factor(data_hel$Code_point_Libelle))[i])
for (j in 4:nrow(data_station)){
  Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
  
  Date1 <- data_station[j,"Date"]$Date
  Date2 <- data_station[j-3,"Date"]$Date
  Com1 <- data_station[j,c(3:226)]
  rownames(Com1) <- Date1
  Com2 <- data_station[j-3,c(3:226)]
  rownames(Com2) <- Date2
  BC <- vegdist(rbind(Com2, Com1), method = "bray")
  Delta <- paste0(j-3,"-",j)
  if (Date1-Date2 <= 63) {
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
  for (j in 4:nrow(data_station)){
    Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
    
    Date1 <- data_station[j,"Date"]$Date
    Date2 <- data_station[j-3,"Date"]$Date
    Com1 <- data_station[j,c(3:226)]
    rownames(Com1) <- Date1
    Com2 <- data_station[j-3,c(3:226)]
    rownames(Com2) <- Date2
    BC <- vegdist(rbind(Com2, Com1), method = "bray")
    Delta <- paste0(j-3,"-",j)
    if (Date1-Date2 <= 63) {
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
write.csv2(data_results_beta,file="data_modif/data_div_beta_N-3.csv", row.names = FALSE,dec = ".")

beta <- read_delim("data_modif/data_div_beta_N-3.csv", 
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

for (i in 4:nrow(data_beta)){
  if (data_beta$EpBloom[i] == "OUI" & (data_beta$EpBloom[i-2] == "OUI" | data_beta$EpBloom[i-2] == "Sucession" | data_beta$EpBloom[i-1] == "OUI" | data_beta$EpBloom[i-1] == "Sucession" | data_beta$EpBloom[i-3] == "OUI" | data_beta$EpBloom[i-3] == "Sucession") ){
    data_beta$EpBloom[i] <- "Sucession"
  }
}

write.csv2(data_beta,file="data_modif/data_div_beta_ok_N-3.csv", row.names = FALSE,dec = ".")
# On a comme ca les indices de bray-curtis associé à chaque date avec les informations de bloom s'il y a

beta <- read_delim("data_modif/data_div_beta_ok_N-3.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)
#### SERIE TEMPORELLE DES INDICES DE BRAY-CURTIS ####

{
  beta <- beta %>% 
    mutate(Week = week(Date)) %>%
    mutate(Month = month(Date)) %>%
    mutate(Year = year(Date)) |>
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
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Bois de la Chaise large",x="DateN")
  ggsave('Boisdelachaiselarge.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))
  
  ggplot(beta_g)+
    geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
    geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Bois de la Chaise large",x="Quinzaine")
  ggsave('Boisdelachaiselarge_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
  ggplot(beta_g)+
    geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
    geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Auger",x="DateN")
  ggsave('Auger.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))
  
  ggplot(beta_g)+
    geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
    geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Auger",x="Quinzaine")
  ggsave('Auger_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
  ggplot(beta_g)+
    geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
    geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Le Cornard",x="DateN")
  ggsave('LeCornard.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))
  
  ggplot(beta_g)+
    geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
    geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Le Cornard",x="Quinzaine")
  ggsave('LeCornard_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
  ggplot(beta_g)+
    geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
    geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Teychan bis",x="DateN")
  ggsave('Teychanbis.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))
  
  ggplot(beta_g)+
    geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
    geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Teychan bis",x="Quinzaine")
  ggsave('Teychanbis_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
  ggplot(beta_g)+
    geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
    geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Ouest Loscolo",x="DateN")
  ggsave('Ouest Loscolo.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))
  
  ggplot(beta_g)+
    geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
    geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Ouest Loscolo",x="Quinzaine")
  ggsave('OuestLoscolo_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
  ggplot(beta_g)+
    geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
    geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Men er Roue",x="DateN")
  ggsave('MenerRoue.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))
  
  ggplot(beta_g)+
    geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
    geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Men er Roue",x="Quinzaine")
  ggsave('MenerRoue_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
  ggplot(beta_g)+
    geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
    geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Loguivy",x="DateN")
  ggsave('Loguivy.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))
  
  ggplot(beta_g)+
    geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
    geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Loguivy",x="Quinzaine")
  ggsave('Loguivy_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
  ggplot(beta_g)+
    geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
    geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "les Hébihens",x="DateN")
  ggsave('lesHebihens.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))
  
  ggplot(beta_g)+
    geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
    geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "les Hébihens",x="Quinzaine")
  ggsave('lesHebihens_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
  ggplot(beta_g)+
    geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
    geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Géfosse",x="DateN")
  ggsave('Géfosse.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))
  
  ggplot(beta_g)+
    geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
    geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Géfosse",x="Quinzaine")
  ggsave('Géfosse_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
  ggplot(beta_g)+
    geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
    geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Cabourg",x="DateN")
  ggsave('Cabourg.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))
  
  ggplot(beta_g)+
    geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
    geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Cabourg",x="Quinzaine")
  ggsave('Cabourg_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
  ggplot(beta_g)+
    geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
    geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Antifer ponton pétrolier",x="DateN")
  ggsave('Antifer ponton pétrolier.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))
  
  ggplot(beta_g)+
    geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
    geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Antifer ponton pétrolier",x="Quinzaine")
  ggsave('Antifer ponton pétrolier_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
  ggplot(beta_g)+
    geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
    geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "At so",x="DateN")
  ggsave('Atso.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))
  
  ggplot(beta_g)+
    geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
    geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "At so",x="Quinzaine")
  ggsave('Atso_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
  ggplot(beta_g)+
    geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
    geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Point 1 Boulogne",x="DateN")
  ggsave('Boulogne.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))
  
  ggplot(beta_g)+
    geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
    geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Point 1 Boulogne",x="Quinzaine")
  ggsave('Boulogne_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
  ggplot(beta_g)+
    geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
    geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Parc Leucate 2",x="DateN")
  ggsave('ParcLeucate.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))
  
  ggplot(beta_g)+
    geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
    geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Parc Leucate 2",x="Quinzaine")
  ggsave('ParcLeucate_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
  ggplot(beta_g)+
    geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
    geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Barcares",x="DateN")
  ggsave("Barcares.png",path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))
  
  ggplot(beta_g)+
    geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
    geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Barcares",x="Quinzaine")
  ggsave('Barcares_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
  ggplot(beta_g)+
    geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
    geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Sète mer",x="DateN")
  ggsave('Sètemer.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))
  
  ggplot(beta_g)+
    geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
    geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Sète mer",x="Quinzaine")
  ggsave('Sètemer_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
  ggplot(beta_g)+
    geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
    geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Bouzigues (a)",x="DateN")
  ggsave('Bouzigues.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))
  
  ggplot(beta_g)+
    geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
    geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Bouzigues (a)",x="Quinzaine")
  ggsave('Bouzigues_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
  ggplot(beta_g)+
    geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
    geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Anse de Carteau 2",x="DateN")
  ggsave('Ansedecarteau.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))
  
  ggplot(beta_g)+
    geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
    geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Anse de Carteau 2",x="Quinzaine")
  ggsave('Ansedecarteau_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
  ggplot(beta_g)+
    geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
    geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "22B - Toulon gde rade",x="DateN")
  ggsave('Toulon.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))
  
  ggplot(beta_g)+
    geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
    geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "22B - Toulon gde rade",x="Quinzaine")
  ggsave('Toulon_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
  ggplot(beta_g)+
    geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
    geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Calvi",x="DateN")
  ggsave('Calvi.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))
  
  ggplot(beta_g)+
    geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
    geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Calvi",x="Quinzaine")
  ggsave('Calvi_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
  ggplot(beta_g)+
    geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
    geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
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
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Diana centre",x="DateN")
  ggsave('Dianacentre.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Fortnight), BC=mean(`Bray-Curtis`,na.rm=T))
  
  ggplot(beta_g)+
    geom_boxplot(aes(x=Fortnight,y=`Bray-Curtis`,group=Fortnight),size = 1)+
    geom_point(aes(x=Fortnight,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,26, by = 1),limits = c(0.5,26.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Diana centre",x="Quinzaine")
  ggsave('Dianacentre_15.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
  
  
  datam <- summarise(group_by(beta_g,Month), BC=mean(`Bray-Curtis`,na.rm=T))
  ggplot(beta_g)+
    geom_boxplot(aes(x=Month,y=`Bray-Curtis`,group=Month),size = 1)+
    geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
    scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
    scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
    labs(title = "Indice de Bray-Curtis DateN comparée à DateN-2",
         subtitle = "Diana centre",x="Mois")
  ggsave('Dianacentre_Mois.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')
}

datam <- summarise(group_by(beta,Month,cluster,Year), BC=mean(`Bray-Curtis`,na.rm=T))
ggplot(datam)+
  geom_boxplot(aes(x=Month,y=BC,group=Month),size = 1)+
  #geom_line(aes(x=Month,y=BC),size = 1,data = datam,col="red")+
  #scale_y_continuous(breaks = seq(0,1, by = 0.1),limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,12, by = 1),limits = c(0.5,12.5))+
  facet_wrap(~cluster)+
  labs(title = "Indice de Bray-Curtis DateN comparée à DateN-3",x="Mois")
ggsave('Cluster_Mois_N-3.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 400, height = 280, units = 'mm')


#### Comparaison N et N-3 ####
beta <- read_delim("data_modif/data_div_beta_ok_N-3.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
                                                                        `Bray-Curtis` = col_number()), locale = locale(decimal_mark = ",", 
                                                                                                                       grouping_mark = "."), trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

data_bloom_beta <- left_join(beta,data_bloom)

data_bloom_beta <- data_bloom_beta %>% 
  mutate(Week = week(Date)) %>%
  mutate(Month = month(Date)) %>%
  mutate(Year = year(Date)) |>
  # 'ceiling' takes the upper integer of a decimal number
  mutate(Fortnight = ceiling(Week/2)) %>%
  arrange(Fortnight)

data_bloom_beta <- data_bloom_beta |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))


# Bloom Dino,Bac, Non bloom
data_bloom_beta$comp <- ifelse(data_bloom_beta$Bloom_Phylum == "Bac", "Bacillariophyceae","Autre nature")
data_bloom_beta[grep("Dino",x = data_bloom_beta$Bloom_Phylum),]$comp <- "Dinophyceae"
data_bloom_beta[is.na(data_bloom_beta$comp),]$comp <- "Non bloom"
data_bloom_beta <- filter(data_bloom_beta,comp == "Non bloom" | comp == "Dinophyceae" | comp == "Bacillariophyceae")

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
entre le jour N et le jour N-3",x="Episode de bloom au jour N",y="Indice de Bray-Curtis")
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
ggsave('N_N-3_Comp_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

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
DunnTest(data_kw$`Bray-Curtis`~data_kw$comp,method="BH")


data_kw <- filter(data_bloom_beta, comp == "Bacillariophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta, comp == "Dinophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta, comp == "Non bloom")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")



#### ENTRE LA DATE N ET N+3 ####
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
for (j in 1:(nrow(data_station)-3)){
  Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
  
  Date1 <- data_station[j,"Date"]$Date
  Date2 <- data_station[j+3,"Date"]$Date
  Com1 <- data_station[j,c(3:226)]
  rownames(Com1) <- Date1
  Com2 <- data_station[j+3,c(3:226)]
  rownames(Com2) <- Date2
  BC <- vegdist(rbind(Com2, Com1), method = "bray")
  Delta <- paste0(j,"-",j+3)
  if (Date2-Date1 <= 63){
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
  for (j in 1:(nrow(data_station)-3)){
    Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
    
    Date1 <- data_station[j,"Date"]$Date
    Date2 <- data_station[j+3,"Date"]$Date
    Com1 <- data_station[j,c(3:226)]
    rownames(Com1) <- Date1
    Com2 <- data_station[j+3,c(3:226)]
    rownames(Com2) <- Date2
    BC <- vegdist(rbind(Com2, Com1), method = "bray")
    Delta <- paste0(j,"-",j+3)
    if (Date2-Date1 <= 63){
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

write.csv2(data_results_beta,file="data_modif/data_div_betaN+3.csv", row.names = FALSE,dec = ".")

beta <- read_delim("data_modif/data_div_betaN+3.csv", 
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

for (i in 1:nrow(data_beta)){
  if (data_beta$EpBloom[i] == "OUI" & (data_beta$EpBloom[i+2] == "OUI" | data_beta$EpBloom[i+2] == "Sucession" | data_beta$EpBloom[i+1] == "OUI" | data_beta$EpBloom[i+1] == "Sucession"
                                       | data_beta$EpBloom[i+3] == "OUI" | data_beta$EpBloom[i+3] == "Sucession") ){
    data_beta$EpBloom[i] <- "Sucession"
  }
}

write.csv2(data_beta,file="data_modif/data_div_beta_ok_N3.csv", row.names = FALSE,dec = ".")
# On a comme ca les indices de bray-curtis associé à chaque date avec les informations de bloom s'il y a

#### Comparaison N et N+3 ####

beta <- read_delim("data_modif/data_div_beta_ok_N3.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
                                                                        `Bray-Curtis` = col_number()), locale = locale(decimal_mark = ",", 
                                                                                                                       grouping_mark = "."), trim_ws = TRUE)
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



cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

data_bloom_beta <- data_bloom_beta %>% 
  mutate(Week = week(Date)) %>%
  mutate(Month = month(Date)) %>%
  mutate(Year = year(Date)) |>
  # 'ceiling' takes the upper integer of a decimal number
  mutate(Fortnight = ceiling(Week/2)) %>%
  arrange(Fortnight)

data_bloom_beta <- data_bloom_beta |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))


# Bloom Dino,Bac, Non bloom
data_bloom_beta$comp <- ifelse(data_bloom_beta$Bloom_Phylum == "Bac", "Bacillariophyceae","Autre nature")
data_bloom_beta[grep("Dino",x = data_bloom_beta$Bloom_Phylum),]$comp <- "Dinophyceae"
data_bloom_beta[is.na(data_bloom_beta$comp),]$comp <- "Non bloom"
data_bloom_beta <- filter(data_bloom_beta,comp == "Non bloom" | comp == "Dinophyceae" | comp == "Bacillariophyceae")

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
entre le jour N et le jour N+3",x="Episode de bloom au jour N",y="Indice de Bray-Curtis")
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
ggsave('N_N+3_Comp_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

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
DunnTest(data_kw$`Bray-Curtis`~data_kw$comp,method="BH")


data_kw <- filter(data_bloom_beta, comp == "Bacillariophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta, comp == "Dinophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta, comp == "Non bloom")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")


#### ENTRE LA DATE N-3 ET N+3 ####
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
for (j in 4:(nrow(data_station)-3)){
  Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
  
  Date1 <- data_station[j-3,"Date"]$Date
  Date2 <- data_station[j+3,"Date"]$Date
  Com1 <- data_station[j-3,c(3:226)]
  rownames(Com1) <- Date1
  Com2 <- data_station[j+3,c(3:226)]
  rownames(Com2) <- Date2
  BC <- vegdist(rbind(Com2, Com1), method = "bray")
  Delta <- paste0(j-3,"-",j+3)
  if (Date2-Date1 <= 126){
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
  for (j in 4:(nrow(data_station)-3)){
    Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
    
    Date1 <- data_station[j-3,"Date"]$Date
    Date2 <- data_station[j+3,"Date"]$Date
    Com1 <- data_station[j-3,c(3:226)]
    rownames(Com1) <- Date1
    Com2 <- data_station[j+3,c(3:226)]
    rownames(Com2) <- Date2
    BC <- vegdist(rbind(Com2, Com1), method = "bray")
    Delta <- paste0(j-3,"-",j+3)
    if (Date2-Date1 <= 126){
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

write.csv2(data_results_beta,file="data_modif/data_div_betaN-3_N+3.csv", row.names = FALSE,dec = ".")

beta <- read_delim("data_modif/data_div_betaN-3_N+3.csv", 
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

for (i in 1:nrow(data_beta)){
  if (data_beta$EpBloom[i] == "OUI" & (data_beta$EpBloom[i+1] == "OUI" | data_beta$EpBloom[i+1] == "Sucession" | data_beta$EpBloom[i+2] == "OUI" | data_beta$EpBloom[i+2] == "Sucession"
                                       | data_beta$EpBloom[i+3] == "OUI" | data_beta$EpBloom[i+3] == "Sucession") ){
    data_beta$EpBloom[i] <- "Sucession"
  }
}

for (i in 4:nrow(data_beta)){
  if (data_beta$EpBloom[i] == "OUI" & (data_beta$EpBloom[i-1] == "OUI" | data_beta$EpBloom[i-1] == "Sucession" | data_beta$EpBloom[i-2] == "OUI" | data_beta$EpBloom[i-2] == "Sucession"
                                       | data_beta$EpBloom[i-3] == "OUI" | data_beta$EpBloom[i-3] == "Sucession") ){
    data_beta$EpBloom[i] <- "Sucession"
  }
}

write.csv2(data_beta,file="data_modif/data_div_beta_ok_N-3_N+3.csv", row.names = FALSE,dec = ".")
# On a comme ca les indices de bray-curtis associé à chaque date avec les informations de bloom s'il y a

#### Comparaison N-3 et N+3 ####
beta <- read_delim("data_modif/data_div_beta_ok_N-3_N+3.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
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


cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

data_bloom_beta <- data_bloom_beta %>% 
  mutate(Week = week(Date)) %>%
  mutate(Month = month(Date)) %>%
  mutate(Year = year(Date)) |>
  # 'ceiling' takes the upper integer of a decimal number
  mutate(Fortnight = ceiling(Week/2)) %>%
  arrange(Fortnight)

data_bloom_beta <- data_bloom_beta |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))


# Bloom Dino,Bac, Non bloom
data_bloom_beta$comp <- ifelse(data_bloom_beta$Bloom_Phylum == "Bac", "Bacillariophyceae","Autre nature")
data_bloom_beta[grep("Dino",x = data_bloom_beta$Bloom_Phylum),]$comp <- "Dinophyceae"
data_bloom_beta[is.na(data_bloom_beta$comp),]$comp <- "Non bloom"
data_bloom_beta <- filter(data_bloom_beta,comp == "Non bloom" | comp == "Dinophyceae" | comp == "Bacillariophyceae")

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
entre le jour N-3 et le jour N+3",x="Episode de bloom au jour N",y="Indice de Bray-Curtis")
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
ggsave('N-3_N+3_Comp_NB.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

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
DunnTest(data_kw$`Bray-Curtis`~data_kw$comp,method="BH")


data_kw <- filter(data_bloom_beta, comp == "Bacillariophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_bloom_beta, comp == "Dinophyceae")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_bloom_beta, comp == "Non bloom")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")


### Comparaison avant/apres/entreN-3_N+3 ####
beta <- read_delim("data_modif/data_div_beta_ok_N-3.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

avant <- left_join(beta,data_bloom)

beta <- read_delim("data_modif/data_div_beta_ok_N3.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

apres <- left_join(beta,data_bloom)

beta <- read_delim("data_modif/data_div_beta_ok_N-3_N+3.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
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

avant$cat <- "N vs N-3"
apres$cat <- "N vs N+3"
NN1$cat <- "N-3 vs N+3"

data_comp <- rbind(avant,apres,NN1)

data_kw <- filter(data_comp,Bloom_Phylum == "Bac")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

data_comp <- data_comp %>% 
  mutate(Week = week(Date)) %>%
  mutate(Month = month(Date)) %>%
  mutate(Year = year(Date)) |>
  # 'ceiling' takes the upper integer of a decimal number
  mutate(Fortnight = ceiling(Week/2)) %>%
  arrange(Fortnight)

data_comp <- data_comp |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))


# Bloom Dino,Bac, Non bloom
data_comp$comp <- ifelse(data_comp$Bloom_Phylum == "Bac", "Bacillariophyceae","Autre nature")
data_comp[grep("Dino",x = data_comp$Bloom_Phylum),]$comp <- "Dinophyceae"
data_comp[is.na(data_comp$comp),]$comp <- "Non bloom"
data_comp <- filter(data_comp,comp == "Non bloom" | comp == "Dinophyceae" | comp == "Bacillariophyceae")

data_comp_bac <- filter(data_comp,comp=="Bacillariophyceae")
data_kw <- filter(data_comp_bac)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_comp_bac))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis bloom
Bacillariophyceae au jour N",x="Comparaison",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_comp_bac))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Episode de bloom au jour N",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_comp_bac))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~cat, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Comparaison",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('Comp_bac_N3.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_comp_bac, cluster == 1)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp_bac, cluster == 2)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp_bac, cluster == 3)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp_bac, cluster == 4)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")


data_kw <- filter(data_comp_bac, cat == "N vs N-3")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp_bac, cat == "N vs N+3")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_comp_bac, cat == "N-3 vs N+3")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_comp_dino <- filter(data_comp,comp=="Dinophyceae")
data_kw <- filter(data_comp_dino)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

a <- ggplot(filter(data_comp_dino))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`,fill=as.character(cluster)))+
  facet_wrap(~cluster)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7))+
  labs(title="Indice de Bray-Curtis bloom
Dinophyceae au jour N",x="Comparaison",y="Indice de Bray-Curtis")
b <- ggplot(filter(data_comp_dino))+
  geom_boxplot(aes(x=cat,group=cat,y=`Bray-Curtis`),fill="grey")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Episode de bloom au jour N",y="Indice de Bray-Curtis",
       subtitle = "Tout cluster confondu")

c <- ggplot(filter(data_comp_dino))+
  geom_boxplot(aes(x=cluster,group=cluster,y=`Bray-Curtis`,fill = as.character(cluster)))+
  facet_wrap(~cat, ncol = 1)+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))+
  labs(x="Comparaison",y="Indice de Bray-Curtis",
       subtitle = "Episode de bloom au jour N")

(a+c)/b
ggsave('Comp_dinoN3.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 200, height = 280, units = 'mm')

data_kw <- filter(data_comp_dino, cluster == 1)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp_dino, cluster == 2)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp_dino, cluster == 3)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")

data_kw <- filter(data_comp_dino, cluster == 4)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cat)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cat,method="BH")


data_kw <- filter(data_comp_dino, cat == "N vs N-3")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")

data_kw <- filter(data_comp_dino, cat == "N vs N+3")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)

data_kw <- filter(data_comp_dino, cat == "N-3 vs N+3")
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")


###  DIVERSITE BETA ENTRE LES MOIS DE CHAQUE ANNEE #####
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

# Garde que les comptages
data_count <- data[,c(2,3,6,7,24:247)]
# Remplace les NA par 0
data_count[is.na(data_count)] <- 0

data_count <- data_count %>%
  group_by(Code_point_Libelle, Year,Month) %>%
  summarise(across(everything(), mean, na.rm = TRUE))


# Transformation d'Hellinger pour ne s'interesser qu'a la composition
data_hel <- decostand(data_count[,-c(1:4)], method = "hellinger")
# Remet la date et la station
data_hel <- bind_cols(data_count[,c(1:4)],data_hel)

# Creation d'un df pour stocker les resultats
data_results_beta <- c("","")
data_results_beta <- as.data.frame(data_results_beta)

i = 1
data_station <- filter(data_hel, Code_point_Libelle == levels(as.factor(data_hel$Code_point_Libelle))[i])
for (j in 1:(nrow(data_station)-1)){
  Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
  
  Date1 <- data_station[j,"Month"]$Month
  Date2 <- data_station[j+1,"Month"]$Month
  Com1 <- data_station[j,c(5:228)]
  rownames(Com1) <- Date1
  Com2 <- data_station[j+1,c(5:228)]
  rownames(Com2) <- Date2
  BC <- vegdist(rbind(Com2, Com1), method = "bray")
  Delta <- paste0(Date1,"-",Date2)
  if (Date2-Date1 == 1){
    data_results_beta[j-1,1] <- Station
    data_results_beta[j-1,2] <- Delta
    data_results_beta[j-1,3] <- as.numeric(BC)
    data_results_beta[j-1,4] <- data_station[j,"Year"]$Year
    data_results_beta[j-1,5] <- data_station[j,"cluster"]$cluster
} else { 
    data_results_beta[j-1,1] <- Station
    data_results_beta[j-1,2] <- Delta
    data_results_beta[j-1,3] <- NA
    data_results_beta[j-1,4] <- data_station[j,"Year"]$Year
    data_results_beta[j-1,5] <- data_station[j,"cluster"]$cluster
  }
} 
for (i in 2:length(levels(as.factor(data_hel$Code_point_Libelle)))){
  data_station <- filter(data_hel, Code_point_Libelle == levels(as.factor(data_hel$Code_point_Libelle))[i])
  data_results_beta2 <- c("","")
  data_results_beta2 <- as.data.frame(data_results_beta2)
  for (j in 1:(nrow(data_station)-2)){
    Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
    
    Date1 <- data_station[j,"Month"]$Month
    Date2 <- data_station[j+1,"Month"]$Month
    Com1 <- data_station[j,c(5:228)]
    rownames(Com1) <- Date1
    Com2 <- data_station[j+1,c(5:228)]
    rownames(Com2) <- Date2
    BC <- vegdist(rbind(Com2, Com1), method = "bray")
    Delta <- paste0(Date1,"-",Date2)
    if (Date2-Date1 == 1){
      data_results_beta2[j-1,1] <- Station
      data_results_beta2[j-1,2] <- Delta
      data_results_beta2[j-1,3] <- as.numeric(BC)
      data_results_beta2[j-1,4] <- data_station[j,"Year"]$Year
      data_results_beta2[j-1,5] <- data_station[j,"cluster"]$cluster}
    else { 
      data_results_beta2[j-1,1] <- Station
      data_results_beta2[j-1,2] <- Delta
      data_results_beta2[j-1,3] <- NA
      data_results_beta2[j-1,4] <- data_station[j,"Year"]$Year
      data_results_beta2[j-1,5] <- data_station[j,"cluster"]$cluster
    }
  }
  colnames(data_results_beta) <- c("Code_point_Libelle","Lag","Bray-Curtis","Year","cluster")
  colnames(data_results_beta2) <- c("Code_point_Libelle","Lag","Bray-Curtis","Year","cluster")
  data_results_beta <- rbind(data_results_beta,data_results_beta2)
}

data_results_beta <- filter(data_results_beta, Code_point_Libelle != "")

data_results_beta <- data_results_beta[complete.cases(data_results_beta),]

write.csv2(data_results_beta,file="data_modif/data_div_beta_ok_Mois.csv", row.names = FALSE,dec = ".")

data_results_beta$Lag <- as.factor(data_results_beta$Lag)
levels(data_results_beta$Lag) <- c("1-2"  ,"2-3"  , "3-4" ,  "4-5" ,  "5-6" ,  "6-7" ,  "7-8"  , "8-9"  , "9-10","10-11" ,"11-12")

ggplot(data_results_beta)+
  geom_boxplot(aes(x=Lag,y=`Bray-Curtis`,group=Lag,fill=as.character(cluster)))+
  scale_fill_manual(values = cluster_col,guide="none")+
  facet_wrap(~cluster,ncol=4)+
  theme(axis.text.x = element_text(angle = 90, vjust =0.5, hjust = 1, size = 10))
ggsave('Mois_lag_cluster.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 600, height = 480, units = 'mm')


ggplot(data_results_beta)+
  geom_boxplot(aes(x=Lag,y=`Bray-Curtis`,group=Lag,fill=as.character(cluster)))+
  scale_fill_manual(values = cluster_col,guide="none")+
  facet_grid(cluster~Year)+
  theme(axis.text.x = element_text(angle = 90, vjust =0.5, hjust = 1, size = 7))
ggsave('Annee_mois_lag_cluster.png', path = "output/graphs/Bloom_description/Div_beta",dpi = 600, width = 600, height = 480, units = 'mm')

ggplot(data_results_beta)+
  geom_boxplot(aes(x=Year,y=`Bray-Curtis`,group=Year,fill=as.character(cluster)))+
  scale_fill_manual(values = cluster_col,guide="none")+
  facet_wrap(~cluster)+
  theme(axis.text.x = element_text(angle = 90, vjust =0.5, hjust = 1, size = 7))

data_kw <- filter(data_results_beta)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluste,method="BH")

ggplot(data_results_beta)+
  geom_boxplot(aes(x=cluster,y=`Bray-Curtis`,group=cluster,fill=as.character(cluster)))+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))
  #facet_wrap(~Lag)+
  #theme(axis.text.x = element_text(angle = 90, vjust =0.5, hjust = 1, size = 7))

data_kw <- filter(data_results_beta)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluste,method="BH")
