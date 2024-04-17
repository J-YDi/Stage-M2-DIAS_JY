data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)


i=1
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA",station)
nom_fichier <- paste0(nom_fichier)

data_Toulon <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=2
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA",station)
nom_fichier <- paste0(nom_fichier)

data_Ansecarteau <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                          delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=3
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA",station)
nom_fichier <- paste0(nom_fichier)

data_Antifer <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                               delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=4
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA",station)
nom_fichier <- paste0(nom_fichier)

data_Atso <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=5
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA",station)
nom_fichier <- paste0(nom_fichier)

data_Auger <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=6
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA",station)
nom_fichier <- paste0(nom_fichier)

data_Barcares <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=7
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA",station)
nom_fichier <- paste0(nom_fichier)

data_Boisdelachaise <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=8
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA",station)
nom_fichier <- paste0(nom_fichier)

data_Bouzigues <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=9
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA",station)
nom_fichier <- paste0(nom_fichier)

data_Cabourg <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=10
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA",station)
nom_fichier <- paste0(nom_fichier)

data_Calvi <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=11
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA",station)
nom_fichier <- paste0(nom_fichier)

data_Dianacentre <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)
i=12
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA",station)
nom_fichier <- paste0(nom_fichier)

data_Géfosse <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=13
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA",station)
nom_fichier <- paste0(nom_fichier)

data_Cornard <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=14
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA",station)
nom_fichier <- paste0(nom_fichier)

data_Hebihens <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=15
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA",station)
nom_fichier <- paste0(nom_fichier)

data_Loguivy <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=16
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA",station)
nom_fichier <- paste0(nom_fichier)

data_MenerRoue <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=17
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA",station)
nom_fichier <- paste0(nom_fichier)

data_OuestLoscolo <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=18
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA",station)
nom_fichier <- paste0(nom_fichier)

data_ParcLeucate <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=19
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA",station)
nom_fichier <- paste0(nom_fichier)

data_Boulogne <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=20
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA",station)
nom_fichier <- paste0(nom_fichier)

data_Setemer <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=21
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA",station)
nom_fichier <- paste0(nom_fichier)

data_Teychan <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

data_outliers_change <- rbind( data_Ansecarteau  ,    data_Antifer        ,  data_Atso       ,      data_Auger     ,     
                               data_Barcares     ,    data_Boisdelachaise ,  data_Boulogne   ,      data_Cabourg   ,       data_Calvi,          
                                data_Cornard     ,     data_Dianacentre   ,   data_Géfosse   ,       data_Hebihens ,        data_Loguivy  ,      
                                data_MenerRoue   ,     data_OuestLoscolo  ,   data_Setemer   ,       data_Teychan  ,data_Toulon, data_ParcLeucate, data_Bouzigues)



datagraph <- data_Calvi
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Calvi"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Calvi"))$CHLOROA),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier),col="red",size=1)+
  geom_point(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier,size=Outlier))+
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  geom_hline(yintercept = min(filter(datagraph,Outlier=="OUI" & CHLOROA_noseason >=0)$CHLOROA_noseason),col="orange",size=1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.95,na.rm = T), col = "violet", size = 1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.99,na.rm = T), col = "maroon3", size =1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.90,na.rm = T), col = "red3", size =1)+
  
    scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
    labs(title = "Serie temporelle de Chlorophylle Calvi",
         subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"                              Seuil:",round(min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),digits = 3),"
         pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en chlorophylle",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original
       Rose : quantile 95%, rouge foncé 90 et Violet : 99% ")
ggsave('Calvi_CHLORO.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')


datagraph <- data_Ansecarteau
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Anse de Carteau 2"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Anse de Carteau 2"))$CHLOROA),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier),col="red",size=1)+
  geom_point(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier,size=Outlier))+
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  geom_hline(yintercept = min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),col="orange",size=1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.95,na.rm = T), col = "violet", size = 1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.99,na.rm = T), col = "maroon3", size =1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.90,na.rm = T), col = "red3", size =1)+
  
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de Chlorophylle Anse de Carteau",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),                              "Seuil:",round(min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),digits = 3),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en chlorophylle",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original,
              Rose : quantile 95%, rouge foncé 90 et Violet : 99% ")
ggsave('Ansecarteau_CHLORO.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')


datagraph <- data_Antifer
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Antifer ponton pétrolier"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Antifer ponton pétrolier"))$CHLOROA),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier),col="red",size=1)+
  geom_point(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier,size=Outlier))+
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  geom_hline(yintercept = min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),col="orange",size=1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.95,na.rm = T), col = "violet", size = 1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.99,na.rm = T), col = "maroon3", size =1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.90,na.rm = T), col = "red3", size =1)+
  
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de Chlorophylle Antifer ponton pétrolier",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),                              "Seuil:",round(min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),digits = 3),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en chlorophylle",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original,
              Rose : quantile 95%, rouge foncé 90 et Violet : 99% ")
ggsave('Antifer_CHLORO.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Atso
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "At so"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "At so"))$CHLOROA),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier),col="red",size=1)+
  geom_point(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier,size=Outlier))+
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  geom_hline(yintercept = min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),col="orange",size=1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.95,na.rm = T), col = "violet", size = 1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.99,na.rm = T), col = "maroon3", size =1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.90,na.rm = T), col = "red3", size =1)+
  
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de Chlorophylle At so",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),                              "Seuil:",round(min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),digits = 3),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en chlorophylle",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original,
              Rose : quantile 95%, rouge foncé 90 et Violet : 99% ")
ggsave('Atso_CHLORO.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Auger
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Auger"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Auger"))$CHLOROA),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier),col="red",size=1)+
  geom_point(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier,size=Outlier))+
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  geom_hline(yintercept = min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),col="orange",size=1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.95,na.rm = T), col = "violet", size = 1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.99,na.rm = T), col = "maroon3", size =1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.90,na.rm = T), col = "red3", size =1)+
  
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de Chlorophylle Auger",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),                              "Seuil:",round(min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),digits = 3),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en chlorophylle",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original,
              Rose : quantile 95%, rouge foncé 90 et Violet : 99% ")
ggsave('Auger_CHLORO.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Barcares
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Barcares"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Barcares"))$CHLOROA),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier),col="red",size=1)+
  geom_point(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier,size=Outlier))+
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  geom_hline(yintercept = min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),col="orange",size=1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.95,na.rm = T), col = "violet", size = 1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.99,na.rm = T), col = "maroon3", size =1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.90,na.rm = T), col = "red3", size =1)+
  
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de Chlorophylle Barcares",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),                              "Seuil:",round(min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),digits = 3),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en chlorophylle",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original,
              Rose : quantile 95%, rouge foncé 90 et Violet : 99% ")
ggsave('Barcares_CHLORO.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Boisdelachaise
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Bois de la Chaise large"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Bois de la Chaise large"))$CHLOROA),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier),col="red",size=1)+
  geom_point(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier,size=Outlier))+
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  geom_hline(yintercept = min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),col="orange",size=1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.95,na.rm = T), col = "violet", size = 1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.99,na.rm = T), col = "maroon3", size =1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.90,na.rm = T), col = "red3", size =1)+
  
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de Chlorophylle Bois de la Chaise large",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),                              "Seuil:",round(min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),digits = 3),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en chlorophylle",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original,
              Rose : quantile 95%, rouge foncé 90 et Violet : 99% ")
ggsave('Boisdelachaise_CHLORO.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Boulogne
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Point 1 Boulogne"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Point 1 Boulogne"))$CHLOROA),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier),col="red",size=1)+
  geom_point(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier,size=Outlier))+
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  geom_hline(yintercept = min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),col="orange",size=1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.95,na.rm = T), col = "violet", size = 1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.99,na.rm = T), col = "maroon3", size =1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.90,na.rm = T), col = "red3", size =1)+
  
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de Chlorophylle Point 1 Boulogne",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),                              "Seuil:",round(min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),digits = 3),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en chlorophylle",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original,
              Rose : quantile 95%, rouge foncé 90 et Violet : 99% ")
ggsave('Boulogne_CHLORO.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Cabourg
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Cabourg"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Cabourg"))$CHLOROA),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier),col="red",size=1)+
  geom_point(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier,size=Outlier))+
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  geom_hline(yintercept = min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),col="orange",size=1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.95,na.rm = T), col = "violet", size = 1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.99,na.rm = T), col = "maroon3", size =1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.90,na.rm = T), col = "red3", size =1)+
  
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de Chlorophylle Cabourg",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),                              "Seuil:",round(min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),digits = 3),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en chlorophylle",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original,
              Rose : quantile 95%, rouge foncé 90 et Violet : 99% ")
ggsave('Cabourg_CHLORO.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Cornard
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Le Cornard"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Le Cornard"))$CHLOROA),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier),col="red",size=1)+
  geom_point(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier,size=Outlier))+
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  geom_hline(yintercept = min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),col="orange",size=1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.95,na.rm = T), col = "violet", size = 1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.99,na.rm = T), col = "maroon3", size =1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.90,na.rm = T), col = "red3", size =1)+
  
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de Chlorophylle Le Cornard",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),                              "Seuil:",round(min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),digits = 3),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en chlorophylle",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original,
              Rose : quantile 95%, rouge foncé 90 et Violet : 99% ")
ggsave('Cornard_CHLORO.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Dianacentre
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Diana centre"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Diana centre"))$CHLOROA),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier),col="red",size=1)+
  geom_point(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier,size=Outlier))+
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  geom_hline(yintercept = min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),col="orange",size=1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.95,na.rm = T), col = "violet", size = 1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.99,na.rm = T), col = "maroon3", size =1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.90,na.rm = T), col = "red3", size =1)+
  
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de Chlorophylle Diana centre",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),                              "Seuil:",round(min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),digits = 3),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en chlorophylle",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original,
              Rose : quantile 95%, rouge foncé 90 et Violet : 99% ")
ggsave('Dianacentre_CHLORO.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Géfosse
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Géfosse"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Géfosse"))$CHLOROA),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier),col="red",size=1)+
  geom_point(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier,size=Outlier))+
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  geom_hline(yintercept = min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),col="orange",size=1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.95,na.rm = T), col = "violet", size = 1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.99,na.rm = T), col = "maroon3", size =1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.90,na.rm = T), col = "red3", size =1)+
  
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de Chlorophylle Géfosse",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),                              "Seuil:",round(min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),digits = 3),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en chlorophylle",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original,
              Rose : quantile 95%, rouge foncé 90 et Violet : 99% ")
ggsave('Gefosse_CHLORO.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Hebihens
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "les Hébihens"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "les Hébihens"))$CHLOROA),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier),col="red",size=1)+
  geom_point(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier,size=Outlier))+
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  geom_hline(yintercept = min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),col="orange",size=1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.95,na.rm = T), col = "violet", size = 1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.99,na.rm = T), col = "maroon3", size =1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.90,na.rm = T), col = "red3", size =1)+
  
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de Chlorophylle les Hébihens",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),                              "Seuil:",round(min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),digits = 3),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en chlorophylle",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original,
              Rose : quantile 95%, rouge foncé 90 et Violet : 99% ")
ggsave('Hebihens_CHLORO.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Loguivy
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Loguivy"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Loguivy"))$CHLOROA),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier),col="red",size=1)+
  geom_point(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier,size=Outlier))+
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  geom_hline(yintercept = min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),col="orange",size=1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.95,na.rm = T), col = "violet", size = 1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.99,na.rm = T), col = "maroon3", size =1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.90,na.rm = T), col = "red3", size =1)+
  
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de Chlorophylle Loguivy",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),                              "Seuil:",round(min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),digits = 3),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en chlorophylle",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original,
              Rose : quantile 95%, rouge foncé 90 et Violet : 99% ")
ggsave('Loguivy_CHLORO.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_MenerRoue
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Men er Roue"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Men er Roue"))$CHLOROA),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier),col="red",size=1)+
  geom_point(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier,size=Outlier))+
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  geom_hline(yintercept = min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),col="orange",size=1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.95,na.rm = T), col = "violet", size = 1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.99,na.rm = T), col = "maroon3", size =1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.90,na.rm = T), col = "red3", size =1)+
  
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de Chlorophylle Men er Roue",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),                              "Seuil:",round(min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),digits = 3),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en chlorophylle",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original,
              Rose : quantile 95%, rouge foncé 90 et Violet : 99% ")
ggsave('MenerRoue_CHLORO.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_OuestLoscolo
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Ouest Loscolo"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Ouest Loscolo"))$CHLOROA),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier),col="red",size=1)+
  geom_point(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier,size=Outlier))+
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  geom_hline(yintercept = min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),col="orange",size=1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.95,na.rm = T), col = "violet", size = 1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.99,na.rm = T), col = "maroon3", size =1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.90,na.rm = T), col = "red3", size =1)+
  
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de Chlorophylle Ouest Loscolo",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),                              "Seuil:",round(min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),digits = 3),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en chlorophylle",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original,
              Rose : quantile 95%, rouge foncé 90 et Violet : 99% ")
ggsave('OuestLoscolo_CHLORO.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')


datagraph <- data_Setemer
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Sète mer"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Sète mer"))$CHLOROA),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier),col="red",size=1)+
  geom_point(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier,size=Outlier))+
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  geom_hline(yintercept = min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),col="orange",size=1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.95,na.rm = T), col = "violet", size = 1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.99,na.rm = T), col = "maroon3", size =1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.90,na.rm = T), col = "red3", size =1)+
  
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de Chlorophylle Sète mer",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),                              "Seuil:",round(min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),digits = 3),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en chlorophylle",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original,
              Rose : quantile 95%, rouge foncé 90 et Violet : 99% ")
ggsave('Setemer_CHLORO.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Teychan
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Teychan bis"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Teychan bis"))$CHLOROA),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier),col="red",size=1)+
  geom_point(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier,size=Outlier))+
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  geom_hline(yintercept = min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),col="orange",size=1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.95,na.rm = T), col = "violet", size = 1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.99,na.rm = T), col = "maroon3", size =1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.90,na.rm = T), col = "red3", size =1)+
  
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de Chlorophylle Teychan bis",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),                              "Seuil:",round(min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),digits = 3),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en chlorophylle",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original,
              Rose : quantile 95%, rouge foncé 90 et Violet : 99% ")
ggsave('Teychan_CHLORO.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Toulon
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "22B - Toulon gde rade"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "22B - Toulon gde rade"))$CHLOROA),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier),col="red",size=1)+
  geom_point(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier,size=Outlier))+
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  geom_hline(yintercept = min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),col="orange",size=1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.95,na.rm = T), col = "violet", size = 1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.99,na.rm = T), col = "maroon3", size =1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.90,na.rm = T), col = "red3", size =1)+
               
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de Chlorophylle 22B - Toulon gde rade",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),                              "Seuil:",round(min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),digits = 3),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en chlorophylle",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original,
              Rose : quantile 95%, rouge foncé 90 et Violet : 99% ")
ggsave('Toulon_CHLORO.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_ParcLeucate
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Parc Leucate 2"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Parc Leucate 2"))$CHLOROA),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier),col="red",size=1)+
  geom_point(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier,size=Outlier))+
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  geom_hline(yintercept = min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),col="orange",size=1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.95,na.rm = T), col = "violet", size = 1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.99,na.rm = T), col = "maroon3", size =1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.90,na.rm = T), col = "red3", size =1)+
    scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de Chlorophylle Parc Leucate",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),                              "Seuil:",round(min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),digits = 3),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en chlorophylle",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original,
              Rose : quantile 95%, rouge foncé 90 et Violet : 99% ")
ggsave('ParcLeucate_CHLORO.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Bouzigues
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Bouzigues (a)"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Bouzigues (a)"))$CHLOROA),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier),col="red",size=1)+
  geom_point(data =  datagraph,aes(x=Date,y=CHLOROA_noseason,colour=Outlier,size=Outlier))+
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  geom_hline(yintercept = min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),col="orange",size=1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.95,na.rm = T), col = "violet", size = 1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.99,na.rm = T), col = "maroon3", size =1)+
  geom_hline(yintercept = quantile(datagraph$CHLOROA_noseason, 0.90,na.rm = T), col = "red3", size =1)+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de Chlorophylle Bouzigues (a)",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),                              "Seuil:",round(min(filter(datagraph,Outlier=="OUI"& CHLOROA_noseason >=0)$CHLOROA_noseason),digits = 3),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en chlorophylle",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original,
              Rose : quantile 95%, rouge foncé 90 et Violet : 99% ")
ggsave('Bouzigues_CHLORO.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')



for (i in (1:21)){
  data <- data_outliers_change
  station <- levels(as.factor(data$Code_point_Libelle))[i]
  Table <- filter(data, Code_point_Libelle == station)
  
  m_cop3 <- lm(CHLOROA_noseason ~ Date, data=filter(data_outliers_change,Code_point_Libelle == station))
  regline_cop3 <- predict(m_cop3)
  # plot the linear trend
  plot(filter(data_outliers_change,Code_point_Libelle == station)$CHLOROA_noseason,type="o",
       main = paste0("Tendance pour ",station))
  lines(regline_cop3, col="red",lwd=3)
  
  test_lm <- summary(m_cop3)
  test_res <- trend.test(residuals(m_cop3))
  print(paste("pente:",test_lm$coefficients[2,1],"pval:",test_lm$coefficients[2,4],"residuals :",test_res$p.value,station))
}


# Trouver la date qui correspond aux outliers : c'est la date la plus proche du point logiquement 
data_outliers <- filter(data_outliers_change, Outlier =="OUI")

for (k in 1:21){
  data_Date_find <- c("","")
  data_Date_find <- as.data.frame(data_Date_find)
  
  data_Date_tosearch <- c("","")
  data_Date_tosearch <- as.data.frame(data_Date_tosearch)
  j=3
  station <- levels(as.factor(data_outliers$Code_point_Libelle))[k]
  Tabletosearch <- filter(data_outliers, Code_point_Libelle == station)
  Tabletofind <- filter(data, Code_point_Libelle == station)
  for (j in 1:nrow(Tabletosearch)){
    datetosearch <- as.Date(levels(as.factor(Tabletosearch$Date))[j])
    datefind <- Tabletofind$Date[which.min(abs(Tabletofind$Date - datetosearch))]
    
    data_Date_tosearch[j,1] <- as.numeric(datetosearch)
    data_Date_find[j,1] <- as.numeric(datefind)
    data_Date_tosearch[j,2] <- station
    data_Date_find[j,2] <- station
    
  }
  data_Date_ok <- cbind(data_Date_tosearch,data_Date_find)
  colnames(data_Date_ok) <- c("Datetosearch","Code_point_Libelle","Datefind","Station")
  data_Date_ok$Datetosearch <- as.Date(as.numeric(data_Date_ok$Datetosearch))
  data_Date_ok$Datefind <- as.Date(as.numeric(data_Date_ok$Datefind))
  
  nom_fichier <- paste0("Outliers_matchingdate",station)
  nom_fichier <- paste0(nom_fichier)
  write.csv2(data_Date_ok,file=paste0("data_outliers/Regularisé&Desaisonnalisé/Matchingdate_outliers/",nom_fichier,".csv"), row.names = FALSE,dec = ".")
  
}


# Version manuelle pour les changements abruptes :
# Entrer la date de référence
datagraph <- data_Toulon
date_reference <- datagraph$Date[complete.cases( datagraph$Changepoint)]

# Trouver la date la plus proche
data_recherche <- filter(data, Code_point_Libelle == "22B - Toulon gde rade")
date_proche <- data_recherche$Date[which.min(abs(data_recherche$Date - date_reference))]

# Afficher la date la plus proche
print(date_reference)
print(date_proche)

# Indiquer sur les donnees initiales où sont les outliers
# Importer les donnees

files <- list.files("data_outliers/Regularisé&Desaisonnalisé/Matchingdate_outliers/", full.names=TRUE)
# count how many we have
length(files)
realdate <- tibble()
# for each element of the `ctd_files` vector
for ( i in 1:length(files) ) {
  message("I am reading file number ", i)
  # get the file path
  file <- files[i]
  # read the file
  temp <- read_delim(file, 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
  # combine it with the previously read data
  realdate <- bind_rows(realdate, temp)
}
realdate$Outlier <- "OUI"
realdate_forfusion <- dplyr::select(realdate,Code_point_Libelle,Datefind,Outlier)
colnames(realdate_forfusion) <- c("Code_point_Libelle","Date","Outlier")

data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)
data_withoutliers <- left_join(data,realdate_forfusion)
# Le fait d'avoir fait la correspondance entre des "fausses dates" et des vrais fait que plusieurs fausses
# peuvent correspondre à 1 seule "vraie date" qui du coup est dupliqué
# On vas supprimer les doublons

doublons_final <- data_withoutliers[duplicated(data_withoutliers$ID.interne.passage) |
                              duplicated(data_withoutliers$ID.interne.passage, fromLast = TRUE), ]
# Filtre des doublons phyto :
resultat_filtre_final <- doublons_final %>%
  filter(duplicated(ID.interne.passage) | n()==1)

# On supprime les lignes en doublon dans le jeu de données initial
data_withoutliers_unique <- subset(data_withoutliers, !(ID.interne.passage %in% unique(doublons_final$ID.interne.passage)))
# On les remets ces doublons filtres
data_withoutliers_ok <- bind_rows(data_withoutliers_unique,resultat_filtre_final)

# Il en reste 2 en doublons 

doublons_final <- data_withoutliers_ok[duplicated(data_withoutliers_ok$ID.interne.passage) |
                                      duplicated(data_withoutliers_ok$ID.interne.passage, fromLast = TRUE), ]

resultat_filtre_final <- doublons_final %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_withoutliers_unique <- subset(data_withoutliers_ok, !(ID.interne.passage %in% unique(doublons_final$ID.interne.passage)))
# On les remets ces doublons filtres
data_withoutliers_ok <- bind_rows(data_withoutliers_unique,resultat_filtre_final)
# la c'est ok
data_withoutliers_ok <- data_withoutliers_ok |>
  group_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage, Prelevement.niveau)

doublons_final <- data_withoutliers_ok[duplicated(data_withoutliers_ok$ID.interne.passage) |
                                         duplicated(data_withoutliers_ok$ID.interne.passage, fromLast = TRUE), ]

resultat_filtre_final <- doublons_final %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_withoutliers_unique <- subset(data_withoutliers_ok, !(ID.interne.passage %in% unique(doublons_final$ID.interne.passage)))
# On les remets ces doublons filtres
data_withoutliers_ok <- bind_rows(data_withoutliers_unique,resultat_filtre_final)
# la c'est ok
data_withoutliers_ok <- data_withoutliers_ok |>
  group_by(Code.Region, Code_point_Libelle, lon, lat, Year, Month, Date, ID.interne.passage, Prelevement.niveau)

write.csv2(data_withoutliers_ok,file="data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers.csv", row.names = FALSE,dec = ".")



########### Analyse des outliers #######

data <- read_delim("data_modif/Table_outliers.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                        grouping_mark = ""), trim_ws = TRUE)

data$cluster <- as.factor(data$cluster)
cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF")

ggplot(data)+
  geom_point(aes(x=CHLOROA,y=Pielou,colour=cluster),size=3)+
  facet_wrap(~Code_point_Libelle,scale="free_x",nrow = 3)+
  scale_colour_manual(values = cluster_col,guide="none")







summary(lm(CHLOROA~BergerParker+Shannon+Pielou+TEMP+SALI+NH4 ,data))




# En retirant les outliers pour avoir la tendance : 

i=1
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA_sansoutliers",station)
nom_fichier <- paste0(nom_fichier)

data_Toulon <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé_sansoutliers/",nom_fichier,".csv"), 
                          delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=2
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA_sansoutliers",station)
nom_fichier <- paste0(nom_fichier)

data_Ansecarteau <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé_sansoutliers/",nom_fichier,".csv"), 
                               delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=3
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA_sansoutliers",station)
nom_fichier <- paste0(nom_fichier)

data_Antifer <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé_sansoutliers/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=4
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA_sansoutliers",station)
nom_fichier <- paste0(nom_fichier)

data_Atso <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé_sansoutliers/",nom_fichier,".csv"), 
                        delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=5
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA_sansoutliers",station)
nom_fichier <- paste0(nom_fichier)

data_Auger <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé_sansoutliers/",nom_fichier,".csv"), 
                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=6
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA_sansoutliers",station)
nom_fichier <- paste0(nom_fichier)

data_Barcares <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé_sansoutliers/",nom_fichier,".csv"), 
                            delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=7
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA_sansoutliers",station)
nom_fichier <- paste0(nom_fichier)

data_Boisdelachaise <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé_sansoutliers/",nom_fichier,".csv"), 
                                  delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=8
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA_sansoutliers",station)
nom_fichier <- paste0(nom_fichier)

data_Bouzigues <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé_sansoutliers/",nom_fichier,".csv"), 
                             delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=9
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA_sansoutliers",station)
nom_fichier <- paste0(nom_fichier)

data_Cabourg <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé_sansoutliers/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=10
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA_sansoutliers",station)
nom_fichier <- paste0(nom_fichier)

data_Calvi <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé_sansoutliers/",nom_fichier,".csv"), 
                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=11
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA_sansoutliers",station)
nom_fichier <- paste0(nom_fichier)

data_Dianacentre <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé_sansoutliers/",nom_fichier,".csv"), 
                               delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)
i=12
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA_sansoutliers",station)
nom_fichier <- paste0(nom_fichier)

data_Géfosse <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé_sansoutliers/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=13
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA_sansoutliers",station)
nom_fichier <- paste0(nom_fichier)

data_Cornard <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé_sansoutliers/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=14
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA_sansoutliers",station)
nom_fichier <- paste0(nom_fichier)

data_Hebihens <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé_sansoutliers/",nom_fichier,".csv"), 
                            delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=15
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA_sansoutliers",station)
nom_fichier <- paste0(nom_fichier)

data_Loguivy <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé_sansoutliers/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=16
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA_sansoutliers",station)
nom_fichier <- paste0(nom_fichier)

data_MenerRoue <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé_sansoutliers/",nom_fichier,".csv"), 
                             delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=17
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA_sansoutliers",station)
nom_fichier <- paste0(nom_fichier)

data_OuestLoscolo <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé_sansoutliers/",nom_fichier,".csv"), 
                                delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=18
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA_sansoutliers",station)
nom_fichier <- paste0(nom_fichier)

data_ParcLeucate <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé_sansoutliers/",nom_fichier,".csv"), 
                               delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=19
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA_sansoutliers",station)
nom_fichier <- paste0(nom_fichier)

data_Boulogne <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé_sansoutliers/",nom_fichier,".csv"), 
                            delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=20
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA_sansoutliers",station)
nom_fichier <- paste0(nom_fichier)

data_Setemer <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé_sansoutliers/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=21
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA_sansoutliers",station)
nom_fichier <- paste0(nom_fichier)

data_Teychan <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé_sansoutliers/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

data_outliers_change <- rbind( data_Ansecarteau  ,    data_Antifer        ,  data_Atso       ,      data_Auger     ,     
                               data_Barcares     ,    data_Boisdelachaise ,  data_Boulogne   ,      data_Cabourg   ,       data_Calvi,          
                               data_Cornard     ,     data_Dianacentre   ,   data_Géfosse   ,       data_Hebihens ,        data_Loguivy  ,      
                               data_MenerRoue   ,     data_OuestLoscolo  ,   data_Setemer   ,       data_Teychan  ,data_Toulon, data_ParcLeucate, data_Bouzigues)


###### REPRESENTATION DES PENTES SUR LES DONNEES INITIALES ########

for (i in (1:21)){
station <- levels(as.factor(data$Code_point_Libelle))[i]
Table <- filter(data, Code_point_Libelle == station)

m_cop3 <- lm(CHLOROA_noseason ~ Date, data=filter(data_outliers_change,Code_point_Libelle == station))
regline_cop3 <- predict(m_cop3)
# plot the linear trend
plot(filter(data_outliers_change,Code_point_Libelle == station)$CHLOROA_noseason,type="o",
     main = paste0("Tendance pour ",station))
lines(regline_cop3, col="red",lwd=3)

test_lm <- summary(m_cop3)
test_res <- trend.test(residuals(m_cop3))
print(paste("pente:",test_lm$coefficients[2,1],"pval:",test_lm$coefficients[2,4],"residuals :",test_res$p.value,station))
}

ggplot(filter(data_outliers_change, Code_point_Libelle == "Ouest Loscolo"))+
  geom_line(aes(x=Date,y=SALI_noseason))+
  geom_abline(slope = filter(data_outliers_change, Code_point_Libelle == "Ouest Loscolo")$slopesens[1], 
              intercept = median(filter(data_outliers_change, Code_point_Libelle == "Ouest Loscolo")$SALI_noseason,na.rm = T) - filter(data_outliers_change, Code_point_Libelle == "Ouest Loscolo")$slopesens[1] * as.numeric(median(as.numeric(filter(data_outliers_change, Code_point_Libelle == "Ouest Loscolo")$Date))) )



### Comparaison méthodes de détection ###
Q95 <- quantile(data_Calvi$CHLOROA_noseason, 0.95,na.rm = T)
Q90 <- quantile(data_Calvi$CHLOROA_noseason, 0.90,na.rm = T)
Q99 <- quantile(data_Calvi$CHLOROA_noseason, 0.99,na.rm = T)
MAD <- min(filter(data_Calvi,Outlier == "OUI" & CHLOROA_noseason >= 0)$CHLOROA_noseason)
nrow(filter(data_Calvi,CHLOROA_noseason >= Q90))
nrow(filter(data_Calvi,CHLOROA_noseason >= Q95))
nrow(filter(data_Calvi,CHLOROA_noseason >= Q99))
nrow(filter(data_Calvi,CHLOROA_noseason >= MAD))
nrow(data_Calvi)

Q95 <- quantile(data_Ansecarteau$CHLOROA_noseason, 0.95,na.rm = T)
Q90 <- quantile(data_Ansecarteau$CHLOROA_noseason, 0.90,na.rm = T)
Q99 <- quantile(data_Ansecarteau$CHLOROA_noseason, 0.99,na.rm = T)
MAD <- min(filter(data_Ansecarteau,Outlier == "OUI" & CHLOROA_noseason >= 0)$CHLOROA_noseason)
nrow(filter(data_Ansecarteau,CHLOROA_noseason >= Q90))
nrow(filter(data_Ansecarteau,CHLOROA_noseason >= Q95))
nrow(filter(data_Ansecarteau,CHLOROA_noseason >= Q99))
nrow(filter(data_Ansecarteau,CHLOROA_noseason >= MAD))
nrow(data_Ansecarteau)

Q95 <- quantile(data_Antifer$CHLOROA_noseason, 0.95,na.rm = T)
Q90 <- quantile(data_Antifer$CHLOROA_noseason, 0.90,na.rm = T)
Q99 <- quantile(data_Antifer$CHLOROA_noseason, 0.99,na.rm = T)
MAD <- min(filter(data_Antifer,Outlier == "OUI" & CHLOROA_noseason >= 0)$CHLOROA_noseason)
nrow(filter(data_Antifer,CHLOROA_noseason >= Q90))
nrow(filter(data_Antifer,CHLOROA_noseason >= Q95))
nrow(filter(data_Antifer,CHLOROA_noseason >= Q99))
nrow(filter(data_Antifer,CHLOROA_noseason >= MAD))
nrow(data_Antifer)

Q95 <- quantile(data_Atso$CHLOROA_noseason, 0.95,na.rm = T)
Q90 <- quantile(data_Atso$CHLOROA_noseason, 0.90,na.rm = T)
Q99 <- quantile(data_Atso$CHLOROA_noseason, 0.99,na.rm = T)
MAD <- min(filter(data_Atso,Outlier == "OUI" & CHLOROA_noseason >= 0)$CHLOROA_noseason)
nrow(filter(data_Atso,CHLOROA_noseason >= Q90))
nrow(filter(data_Atso,CHLOROA_noseason >= Q95))
nrow(filter(data_Atso,CHLOROA_noseason >= Q99))
nrow(filter(data_Atso,CHLOROA_noseason >= MAD))
nrow(data_Atso)

Q95 <- quantile(data_Auger$CHLOROA_noseason, 0.95,na.rm = T)
Q90 <- quantile(data_Auger$CHLOROA_noseason, 0.90,na.rm = T)
Q99 <- quantile(data_Auger$CHLOROA_noseason, 0.99,na.rm = T)
MAD <- min(filter(data_Auger,Outlier == "OUI" & CHLOROA_noseason >= 0)$CHLOROA_noseason)
nrow(filter(data_Auger,CHLOROA_noseason >= Q90))
nrow(filter(data_Auger,CHLOROA_noseason >= Q95))
nrow(filter(data_Auger,CHLOROA_noseason >= Q99))
nrow(filter(data_Auger,CHLOROA_noseason >= MAD))
nrow(data_Auger)

Q95 <- quantile(data_Cabourg$CHLOROA_noseason, 0.95,na.rm = T)
Q90 <- quantile(data_Cabourg$CHLOROA_noseason, 0.90,na.rm = T)
Q99 <- quantile(data_Cabourg$CHLOROA_noseason, 0.99,na.rm = T)
MAD <- min(filter(data_Cabourg,Outlier == "OUI" & CHLOROA_noseason >= 0)$CHLOROA_noseason)
nrow(filter(data_Cabourg,CHLOROA_noseason >= Q90))
nrow(filter(data_Cabourg,CHLOROA_noseason >= Q95))
nrow(filter(data_Cabourg,CHLOROA_noseason >= Q99))
nrow(filter(data_Cabourg,CHLOROA_noseason >= MAD))
nrow(data_Cabourg)

Q95 <- quantile(data_Géfosse$CHLOROA_noseason, 0.95,na.rm = T)
Q90 <- quantile(data_Géfosse$CHLOROA_noseason, 0.90,na.rm = T)
Q99 <- quantile(data_Géfosse$CHLOROA_noseason, 0.99,na.rm = T)
MAD <- min(filter(data_Géfosse,Outlier == "OUI" & CHLOROA_noseason >= 0)$CHLOROA_noseason)
nrow(filter(data_Géfosse,CHLOROA_noseason >= Q90))
nrow(filter(data_Géfosse,CHLOROA_noseason >= Q95))
nrow(filter(data_Géfosse,CHLOROA_noseason >= Q99))
nrow(filter(data_Géfosse,CHLOROA_noseason >= MAD))
nrow(data_Géfosse)

Q95 <- quantile(data_Hebihens$CHLOROA_noseason, 0.95,na.rm = T)
Q90 <- quantile(data_Hebihens$CHLOROA_noseason, 0.90,na.rm = T)
Q99 <- quantile(data_Hebihens$CHLOROA_noseason, 0.99,na.rm = T)
MAD <- min(filter(data_Hebihens,Outlier == "OUI" & CHLOROA_noseason >= 0)$CHLOROA_noseason)
nrow(filter(data_Hebihens,CHLOROA_noseason >= Q90))
nrow(filter(data_Hebihens,CHLOROA_noseason >= Q95))
nrow(filter(data_Hebihens,CHLOROA_noseason >= Q99))
nrow(filter(data_Hebihens,CHLOROA_noseason >= MAD))
nrow(data_Hebihens)

Q95 <- quantile(data_Loguivy$CHLOROA_noseason, 0.95,na.rm = T)
Q90 <- quantile(data_Loguivy$CHLOROA_noseason, 0.90,na.rm = T)
Q99 <- quantile(data_Loguivy$CHLOROA_noseason, 0.99,na.rm = T)
MAD <- min(filter(data_Loguivy,Outlier == "OUI" & CHLOROA_noseason >= 0)$CHLOROA_noseason)
nrow(filter(data_Loguivy,CHLOROA_noseason >= Q90))
nrow(filter(data_Loguivy,CHLOROA_noseason >= Q95))
nrow(filter(data_Loguivy,CHLOROA_noseason >= Q99))
nrow(filter(data_Loguivy,CHLOROA_noseason >= MAD))
nrow(data_Loguivy)

Q95 <- quantile(data_MenerRoue$CHLOROA_noseason, 0.95,na.rm = T)
Q90 <- quantile(data_MenerRoue$CHLOROA_noseason, 0.90,na.rm = T)
Q99 <- quantile(data_MenerRoue$CHLOROA_noseason, 0.99,na.rm = T)
MAD <- min(filter(data_MenerRoue,Outlier == "OUI" & CHLOROA_noseason >= 0)$CHLOROA_noseason)
nrow(filter(data_MenerRoue,CHLOROA_noseason >= Q90))
nrow(filter(data_MenerRoue,CHLOROA_noseason >= Q95))
nrow(filter(data_MenerRoue,CHLOROA_noseason >= Q99))
nrow(filter(data_MenerRoue,CHLOROA_noseason >= MAD))
nrow(data_MenerRoue)

Q95 <- quantile(data_OuestLoscolo$CHLOROA_noseason, 0.95,na.rm = T)
Q90 <- quantile(data_OuestLoscolo$CHLOROA_noseason, 0.90,na.rm = T)
Q99 <- quantile(data_OuestLoscolo$CHLOROA_noseason, 0.99,na.rm = T)
MAD <- min(filter(data_OuestLoscolo,Outlier == "OUI" & CHLOROA_noseason >= 0)$CHLOROA_noseason)
nrow(filter(data_OuestLoscolo,CHLOROA_noseason >= Q90))
nrow(filter(data_OuestLoscolo,CHLOROA_noseason >= Q95))
nrow(filter(data_OuestLoscolo,CHLOROA_noseason >= Q99))
nrow(filter(data_OuestLoscolo,CHLOROA_noseason >= MAD))
nrow(data_OuestLoscolo)

Q95 <- quantile(data_Boisdelachaise$CHLOROA_noseason, 0.95,na.rm = T)
Q90 <- quantile(data_Boisdelachaise$CHLOROA_noseason, 0.90,na.rm = T)
Q99 <- quantile(data_Boisdelachaise$CHLOROA_noseason, 0.99,na.rm = T)
MAD <- min(filter(data_Boisdelachaise,Outlier == "OUI" & CHLOROA_noseason >= 0)$CHLOROA_noseason)
nrow(filter(data_Boisdelachaise,CHLOROA_noseason >= Q90))
nrow(filter(data_Boisdelachaise,CHLOROA_noseason >= Q95))
nrow(filter(data_Boisdelachaise,CHLOROA_noseason >= Q99))
nrow(filter(data_Boisdelachaise,CHLOROA_noseason >= MAD))
nrow(data_Boisdelachaise)

Q95 <- quantile(data_Cornard$CHLOROA_noseason, 0.95,na.rm = T)
Q90 <- quantile(data_Cornard$CHLOROA_noseason, 0.90,na.rm = T)
Q99 <- quantile(data_Cornard$CHLOROA_noseason, 0.99,na.rm = T)
MAD <- min(filter(data_Cornard,Outlier == "OUI" & CHLOROA_noseason >= 0)$CHLOROA_noseason)
nrow(filter(data_Cornard,CHLOROA_noseason >= Q90))
nrow(filter(data_Cornard,CHLOROA_noseason >= Q95))
nrow(filter(data_Cornard,CHLOROA_noseason >= Q99))
nrow(filter(data_Cornard,CHLOROA_noseason >= MAD))
nrow(data_Cornard)

Q95 <- quantile(data_Auger$CHLOROA_noseason, 0.95,na.rm = T)
Q90 <- quantile(data_Auger$CHLOROA_noseason, 0.90,na.rm = T)
Q99 <- quantile(data_Auger$CHLOROA_noseason, 0.99,na.rm = T)
MAD <- min(filter(data_Auger,Outlier == "OUI" & CHLOROA_noseason >= 0)$CHLOROA_noseason)
nrow(filter(data_Auger,CHLOROA_noseason >= Q90))
nrow(filter(data_Auger,CHLOROA_noseason >= Q95))
nrow(filter(data_Auger,CHLOROA_noseason >= Q99))
nrow(filter(data_Auger,CHLOROA_noseason >= MAD))
nrow(data_Auger)

Q95 <- quantile(data_Teychan$CHLOROA_noseason, 0.95,na.rm = T)
Q90 <- quantile(data_Teychan$CHLOROA_noseason, 0.90,na.rm = T)
Q99 <- quantile(data_Teychan$CHLOROA_noseason, 0.99,na.rm = T)
MAD <- min(filter(data_Teychan,Outlier == "OUI" & CHLOROA_noseason >= 0)$CHLOROA_noseason)
nrow(filter(data_Teychan,CHLOROA_noseason >= Q90))
nrow(filter(data_Teychan,CHLOROA_noseason >= Q95))
nrow(filter(data_Teychan,CHLOROA_noseason >= Q99))
nrow(filter(data_Teychan,CHLOROA_noseason >= MAD))
nrow(data_Teychan)

Q95 <- quantile(data_ParcLeucate$CHLOROA_noseason, 0.95,na.rm = T)
Q90 <- quantile(data_ParcLeucate$CHLOROA_noseason, 0.90,na.rm = T)
Q99 <- quantile(data_ParcLeucate$CHLOROA_noseason, 0.99,na.rm = T)
MAD <- min(filter(data_ParcLeucate,Outlier == "OUI" & CHLOROA_noseason >= 0)$CHLOROA_noseason)
nrow(filter(data_ParcLeucate,CHLOROA_noseason >= Q90))
nrow(filter(data_ParcLeucate,CHLOROA_noseason >= Q95))
nrow(filter(data_ParcLeucate,CHLOROA_noseason >= Q99))
nrow(filter(data_ParcLeucate,CHLOROA_noseason >= MAD))
nrow(data_ParcLeucate)

Q95 <- quantile(data_Barcares$CHLOROA_noseason, 0.95,na.rm = T)
Q90 <- quantile(data_Barcares$CHLOROA_noseason, 0.90,na.rm = T)
Q99 <- quantile(data_Barcares$CHLOROA_noseason, 0.99,na.rm = T)
MAD <- min(filter(data_Barcares,Outlier == "OUI" & CHLOROA_noseason >= 0)$CHLOROA_noseason)
nrow(filter(data_Barcares,CHLOROA_noseason >= Q90))
nrow(filter(data_Barcares,CHLOROA_noseason >= Q95))
nrow(filter(data_Barcares,CHLOROA_noseason >= Q99))
nrow(filter(data_Barcares,CHLOROA_noseason >= MAD))
nrow(data_Barcares)

Q95 <- quantile(data_Setemer$CHLOROA_noseason, 0.95,na.rm = T)
Q90 <- quantile(data_Setemer$CHLOROA_noseason, 0.90,na.rm = T)
Q99 <- quantile(data_Setemer$CHLOROA_noseason, 0.99,na.rm = T)
MAD <- min(filter(data_Setemer,Outlier == "OUI" & CHLOROA_noseason >= 0)$CHLOROA_noseason)
nrow(filter(data_Setemer,CHLOROA_noseason >= Q90))
nrow(filter(data_Setemer,CHLOROA_noseason >= Q95))
nrow(filter(data_Setemer,CHLOROA_noseason >= Q99))
nrow(filter(data_Setemer,CHLOROA_noseason >= MAD))
nrow(data_Setemer)

Q95 <- quantile(data_Bouzigues$CHLOROA_noseason, 0.95,na.rm = T)
Q90 <- quantile(data_Bouzigues$CHLOROA_noseason, 0.90,na.rm = T)
Q99 <- quantile(data_Bouzigues$CHLOROA_noseason, 0.99,na.rm = T)
MAD <- min(filter(data_Bouzigues,Outlier == "OUI" & CHLOROA_noseason >= 0)$CHLOROA_noseason)
nrow(filter(data_Bouzigues,CHLOROA_noseason >= Q90))
nrow(filter(data_Bouzigues,CHLOROA_noseason >= Q95))
nrow(filter(data_Bouzigues,CHLOROA_noseason >= Q99))
nrow(filter(data_Bouzigues,CHLOROA_noseason >= MAD))
nrow(data_Bouzigues)

Q95 <- quantile(data_Toulon$CHLOROA_noseason, 0.95,na.rm = T)
Q90 <- quantile(data_Toulon$CHLOROA_noseason, 0.90,na.rm = T)
Q99 <- quantile(data_Toulon$CHLOROA_noseason, 0.99,na.rm = T)
MAD <- min(filter(data_Toulon,Outlier == "OUI" & CHLOROA_noseason >= 0)$CHLOROA_noseason)
nrow(filter(data_Toulon,CHLOROA_noseason >= Q90))
nrow(filter(data_Toulon,CHLOROA_noseason >= Q95))
nrow(filter(data_Toulon,CHLOROA_noseason >= Q99))
nrow(filter(data_Toulon,CHLOROA_noseason >= MAD))
nrow(data_Toulon)

Q95 <- quantile(data_Dianacentre$CHLOROA_noseason, 0.95,na.rm = T)
Q90 <- quantile(data_Dianacentre$CHLOROA_noseason, 0.90,na.rm = T)
Q99 <- quantile(data_Dianacentre$CHLOROA_noseason, 0.99,na.rm = T)
MAD <- min(filter(data_Dianacentre,Outlier == "OUI" & CHLOROA_noseason >= 0)$CHLOROA_noseason)
nrow(filter(data_Dianacentre,CHLOROA_noseason >= Q90))
nrow(filter(data_Dianacentre,CHLOROA_noseason >= Q95))
nrow(filter(data_Dianacentre,CHLOROA_noseason >= Q99))
nrow(filter(data_Dianacentre,CHLOROA_noseason >= MAD))
nrow(data_Dianacentre)

#### Representation heatmap pour les tendances #####
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

dataT <- data %>%
  dplyr::select(Year, Code_point_Libelle, TEMP,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), TEMP=mean(TEMP,na.rm=T))
colnames(dataS) <- c("Site","Year","TEMP")
dataC <- summarise(group_by(dataT, cluster,Year), TEMP=mean(TEMP,na.rm=T))
colnames(dataC) <- c("Site","Year","TEMP")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), TEMP=mean(TEMP,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,TEMP)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = TEMP)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(10, 21),
          title = "Temperature")


dataT <- data %>%
  dplyr::select(Year, Code_point_Libelle, CHLOROA,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), CHLOROA=mean(CHLOROA,na.rm=T))
colnames(dataS) <- c("Site","Year","CHLOROA")
dataC <- summarise(group_by(dataT, cluster,Year), CHLOROA=mean(CHLOROA,na.rm=T))
colnames(dataC) <- c("Site","Year","CHLOROA")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), CHLOROA=mean(CHLOROA,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,CHLOROA)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = CHLOROA)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(0,17),
          title = "Concentration en chlorophylle")


dataT <- data %>%
  dplyr::select(Year, Code_point_Libelle, SIOH,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), SIOH=mean(SIOH,na.rm=T))
colnames(dataS) <- c("Site","Year","SIOH")
dataC <- summarise(group_by(dataT, cluster,Year), SIOH=mean(SIOH,na.rm=T))
colnames(dataC) <- c("Site","Year","SIOH")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), SIOH=mean(SIOH,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,SIOH)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = SIOH)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(0,40),
          title = "Concentration en SIOH")

dataT <- data %>%
  dplyr::select(Year, Code_point_Libelle, PO4,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), PO4=mean(PO4,na.rm=T))
colnames(dataS) <- c("Site","Year","PO4")
dataC <- summarise(group_by(dataT, cluster,Year), PO4=mean(PO4,na.rm=T))
colnames(dataC) <- c("Site","Year","PO4")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), PO4=mean(PO4,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,PO4)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = PO4)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(0,1),
          title = "Concentration en PO4")

dataT <- data %>%
  dplyr::select(Year, Code_point_Libelle, SALI,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), SALI=mean(SALI,na.rm=T))
colnames(dataS) <- c("Site","Year","SALI")
dataC <- summarise(group_by(dataT, cluster,Year), SALI=mean(SALI,na.rm=T))
colnames(dataC) <- c("Site","Year","SALI")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), SALI=mean(SALI,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,SALI)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = SALI)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(28,40),
          title = "Salinite")


dataT <- data %>%
  dplyr::select(Year, Code_point_Libelle, NH4,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), NH4=mean(NH4,na.rm=T))
colnames(dataS) <- c("Site","Year","NH4")
dataC <- summarise(group_by(dataT, cluster,Year), NH4=mean(NH4,na.rm=T))
colnames(dataC) <- c("Site","Year","NH4")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), NH4=mean(NH4,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,NH4)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = NH4)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(0,4),
          title = "Concentration en NH4")


dataT <- data %>%
  dplyr::select(Year, Code_point_Libelle, `NO3+NO2`,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), NO3NO2=mean(`NO3+NO2`,na.rm=T))
colnames(dataS) <- c("Site","Year","NO3NO2")
dataC <- summarise(group_by(dataT, cluster,Year), NO3NO2=mean(`NO3+NO2`,na.rm=T))
colnames(dataC) <- c("Site","Year","NO3NO2")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), NO3NO2=mean(`NO3+NO2`,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,NO3NO2)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = NO3NO2)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(0,53),
          title = "Concentration en NO3+NO2")


dataT <- data %>%
  dplyr::select(Year, Code_point_Libelle, TURB,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), TURB=mean(TURB,na.rm=T))
colnames(dataS) <- c("Site","Year","TURB")
dataC <- summarise(group_by(dataT, cluster,Year), TURB=mean(TURB,na.rm=T))
colnames(dataC) <- c("Site","Year","TURB")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), TURB=mean(TURB,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,TURB)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = TURB)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(0,22),
          title = "Turbidite")


dataT <- data %>%
  dplyr::select(Year, Code_point_Libelle, `TURB-FNU`,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), TURB=mean(`TURB-FNU`,na.rm=T))
colnames(dataS) <- c("Site","Year","TURB")
dataC <- summarise(group_by(dataT, cluster,Year), TURB=mean(`TURB-FNU`,na.rm=T))
colnames(dataC) <- c("Site","Year","TURB")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), TURB=mean(`TURB-FNU`,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,TURB)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = TURB)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(0,32),
          title = "Turbidite FNU")


dataT <- data %>%
  dplyr::select(Year, Code_point_Libelle, OXYGENE,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), OXYGENE=mean(OXYGENE,na.rm=T))
colnames(dataS) <- c("Site","Year","OXYGENE")
dataC <- summarise(group_by(dataT, cluster,Year), OXYGENE=mean(OXYGENE,na.rm=T))
colnames(dataC) <- c("Site","Year","OXYGENE")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), OXYGENE=mean(OXYGENE,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,OXYGENE)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = OXYGENE)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(0,17),
          title = "Concentration en oxygène")

dataT <- data %>%
  dplyr::select(Year, Code_point_Libelle, Pielou,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), Pielou=mean(Pielou,na.rm=T))
colnames(dataS) <- c("Site","Year","Pielou")
dataC <- summarise(group_by(dataT, cluster,Year), Pielou=mean(Pielou,na.rm=T))
colnames(dataC) <- c("Site","Year","Pielou")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), Pielou=mean(Pielou,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,Pielou)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = Pielou)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(0,1),
          title = "Indice de Pielou")

dataT <- data %>%
  dplyr::select(Year, Code_point_Libelle, Shannon,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), Shannon=mean(Shannon,na.rm=T))
colnames(dataS) <- c("Site","Year","Shannon")
dataC <- summarise(group_by(dataT, cluster,Year), Shannon=mean(Shannon,na.rm=T))
colnames(dataC) <- c("Site","Year","Shannon")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), Shannon=mean(Shannon,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,Shannon)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = Shannon)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(0,2),
          title = "Indice de Shannon")

dataT <- data %>%
  dplyr::select(Year, Code_point_Libelle, BergerParker,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), BergerParker=mean(BergerParker,na.rm=T))
colnames(dataS) <- c("Site","Year","BergerParker")
dataC <- summarise(group_by(dataT, cluster,Year), BergerParker=mean(BergerParker,na.rm=T))
colnames(dataC) <- c("Site","Year","BergerParker")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), BergerParker=mean(BergerParker,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,BergerParker)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = BergerParker)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(0,1),
          title = "Indice de Berger-Parker")

dataT <- data %>%
  dplyr::select(Year, Code_point_Libelle, Bacillariophyceae,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), Bacillariophyceae=mean(Bacillariophyceae,na.rm=T))
colnames(dataS) <- c("Site","Year","Bacillariophyceae")
dataC <- summarise(group_by(dataT, cluster,Year), Bacillariophyceae=mean(Bacillariophyceae,na.rm=T))
colnames(dataC) <- c("Site","Year","Bacillariophyceae")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), Bacillariophyceae=mean(Bacillariophyceae,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,Bacillariophyceae)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = Bacillariophyceae)

# save country as row names
plotdata <- as.data.frame(plotdata)
plotdata[,c(2:17)] <- log(plotdata[,c(2:17)]+1) 
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL


# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(1, 15),
          title = "Abondance en Bacillariophyceae")

dataT <- data %>%
  dplyr::select(Year, Code_point_Libelle, Dinophyceae,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), Dinophyceae=mean(Dinophyceae,na.rm=T))
colnames(dataS) <- c("Site","Year","Dinophyceae")
dataC <- summarise(group_by(dataT, cluster,Year), Dinophyceae=mean(Dinophyceae,na.rm=T))
colnames(dataC) <- c("Site","Year","Dinophyceae")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), Dinophyceae=mean(Dinophyceae,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,Dinophyceae)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = Dinophyceae)

# save country as row names
plotdata <- as.data.frame(plotdata)
plotdata[,c(2:17)] <- log(plotdata[,c(2:17)]+1) 
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(1, 15),
          title = "Abondance en Dinophyceae")

dataT <- data %>%
  dplyr::select(Year, Code_point_Libelle, Cryptophyceae,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), Cryptophyceae=mean(Cryptophyceae,na.rm=T))
colnames(dataS) <- c("Site","Year","Cryptophyceae")
dataC <- summarise(group_by(dataT, cluster,Year), Cryptophyceae=mean(Cryptophyceae,na.rm=T))
colnames(dataC) <- c("Site","Year","Cryptophyceae")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), Cryptophyceae=mean(Cryptophyceae,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,Cryptophyceae)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = Cryptophyceae)

# save country as row names
plotdata <- as.data.frame(plotdata)
plotdata[,c(2:17)] <- log(plotdata[,c(2:17)]+1) 
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(1, 15),
          title = "Abondance en Cryptophyceae")

dataT <- data %>%
  dplyr::select(Year, Code_point_Libelle, Ciliophora,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), Ciliophora=mean(Ciliophora,na.rm=T))
colnames(dataS) <- c("Site","Year","Ciliophora")
dataC <- summarise(group_by(dataT, cluster,Year), Ciliophora=mean(Ciliophora,na.rm=T))
colnames(dataC) <- c("Site","Year","Ciliophora")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), Ciliophora=mean(Ciliophora,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,Ciliophora)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = Ciliophora)

# save country as row names
plotdata <- as.data.frame(plotdata)
plotdata[,c(2:17)] <- log(plotdata[,c(2:17)]+1) 
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(1, 10),
          title = "Abondance en Ciliophora")

dataT <- data %>%
  dplyr::select(Year, Code_point_Libelle, Haptophyta,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), Haptophyta=mean(Haptophyta,na.rm=T))
colnames(dataS) <- c("Site","Year","Haptophyta")
dataC <- summarise(group_by(dataT, cluster,Year), Haptophyta=mean(Haptophyta,na.rm=T))
colnames(dataC) <- c("Site","Year","Haptophyta")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), Haptophyta=mean(Haptophyta,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,Haptophyta)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = Haptophyta)

# save country as row names
plotdata <- as.data.frame(plotdata)
plotdata[,c(2:17)] <- log(plotdata[,c(2:17)]+1) 
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(1, 16.5),
          title = "Abondance en Haptophyta")

data$Rspe <- rowSums(data[,c(24:247)] != 0,na.rm = T)
dataT <- data %>%
  dplyr::select(Year, Code_point_Libelle, Rspe,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), Rspe=mean(Rspe,na.rm=T))
colnames(dataS) <- c("Site","Year","Rspe")
dataC <- summarise(group_by(dataT, cluster,Year), Rspe=mean(Rspe,na.rm=T))
colnames(dataC) <- c("Site","Year","Rspe")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), Rspe=mean(Rspe,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,Rspe)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = Rspe)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(3, 36),
          title = "Richesse specifique")


# Heatmap metriques ######
metrics <- read_delim("data_modif/metrics.csv", 
                      delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                      locale = locale(decimal_mark = ",", grouping_mark = "."), 
                      trim_ws = TRUE)

metrics <- metrics %>% 
  mutate(Week = week(Date)) %>%
  mutate(Month = month(Date)) |>
  mutate(Year = year(Date))

dataT <- metrics %>%
  dplyr::select(Year, Code_point_Libelle, N_liens,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), N_liens=mean(N_liens,na.rm=T))
colnames(dataS) <- c("Site","Year","N_liens")
dataC <- summarise(group_by(dataT, cluster,Year), N_liens=mean(N_liens,na.rm=T))
colnames(dataC) <- c("Site","Year","N_liens")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), N_liens=mean(N_liens,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,N_liens)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = N_liens)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(3, 340),
          title = "Nombre de liens")



dataT <- metrics %>%
  dplyr::select(Year, Code_point_Libelle, N_noeuds,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), N_noeuds=mean(N_noeuds,na.rm=T))
colnames(dataS) <- c("Site","Year","N_noeuds")
dataC <- summarise(group_by(dataT, cluster,Year), N_noeuds=mean(N_noeuds,na.rm=T))
colnames(dataC) <- c("Site","Year","N_noeuds")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), N_noeuds=mean(N_noeuds,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,N_noeuds)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = N_noeuds)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(min(plotdata), max(plotdata)),
          title = "Nombre de noeuds")


dataT <- metrics %>%
  dplyr::select(Year, Code_point_Libelle, D_liens,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), D_liens=mean(D_liens,na.rm=T))
colnames(dataS) <- c("Site","Year","D_liens")
dataC <- summarise(group_by(dataT, cluster,Year), D_liens=mean(D_liens,na.rm=T))
colnames(dataC) <- c("Site","Year","D_liens")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), D_liens=mean(D_liens,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,D_liens)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = D_liens)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(min(plotdata), max(plotdata)),
          title = "Densite")


dataT <- metrics %>%
  dplyr::select(Year, Code_point_Libelle, C_tance,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), C_tance=mean(C_tance,na.rm=T))
colnames(dataS) <- c("Site","Year","C_tance")
dataC <- summarise(group_by(dataT, cluster,Year), C_tance=mean(C_tance,na.rm=T))
colnames(dataC) <- c("Site","Year","C_tance")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), C_tance=mean(C_tance,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,C_tance)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = C_tance)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(min(plotdata), max(plotdata)),
          title = "Connectance")

metrics[metrics == Inf] <- NA
dataT <- metrics %>%
  dplyr::select(Year, Code_point_Libelle, Avg_p_length,cluster)


dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), Avg_p_length=mean(Avg_p_length,na.rm=T))
colnames(dataS) <- c("Site","Year","Avg_p_length")
dataC <- summarise(group_by(dataT, cluster,Year), Avg_p_length=mean(Avg_p_length,na.rm=T))
colnames(dataC) <- c("Site","Year","Avg_p_length")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), Avg_p_length=mean(Avg_p_length,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,Avg_p_length)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = Avg_p_length)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(min(plotdata), max(plotdata)),
          title = "Longueur moyenne des liens")

dataT <- metrics %>%
  dplyr::select(Year, Code_point_Libelle, Adhes,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), Adhes=mean(Adhes,na.rm=T))
colnames(dataS) <- c("Site","Year","Adhes")
dataC <- summarise(group_by(dataT, cluster,Year), Adhes=mean(Adhes,na.rm=T))
colnames(dataC) <- c("Site","Year","Adhes")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), Adhes=mean(Adhes,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,Adhes)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = Adhes)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(min(plotdata), max(plotdata)),
          title = "Adhesion")

dataT <- metrics %>%
  dplyr::select(Year, Code_point_Libelle, Mod,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), Mod=mean(Mod,na.rm=T))
colnames(dataS) <- c("Site","Year","Mod")
dataC <- summarise(group_by(dataT, cluster,Year), Mod=mean(Mod,na.rm=T))
colnames(dataC) <- c("Site","Year","Mod")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), Mod=mean(Mod,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,Mod)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = Mod)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(min(plotdata), max(plotdata)),
          title = "Modularite")


dataT <- metrics %>%
  dplyr::select(Year, Code_point_Libelle, meanN_liens,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), meanN_liens=mean(meanN_liens,na.rm=T))
colnames(dataS) <- c("Site","Year","meanN_liens")
dataC <- summarise(group_by(dataT, cluster,Year), meanN_liens=mean(meanN_liens,na.rm=T))
colnames(dataC) <- c("Site","Year","meanN_liens")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), meanN_liens=mean(meanN_liens,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,meanN_liens)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = meanN_liens)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(min(plotdata), max(plotdata)),
          title = "Nombre moyen de liens par noeuds")


dataT <- metrics %>%
  dplyr::select(Year, Code_point_Libelle, Assort,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), Assort=mean(Assort,na.rm=T))
colnames(dataS) <- c("Site","Year","Assort")
dataC <- summarise(group_by(dataT, cluster,Year), Assort=mean(Assort,na.rm=T))
colnames(dataC) <- c("Site","Year","Assort")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), Assort=mean(Assort,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,Assort)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = Assort)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(min(plotdata), max(plotdata)),
          title = "Assortativite")


dataT <- metrics %>%
  dplyr::select(Year, Code_point_Libelle, Diss,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), Diss=mean(Diss,na.rm=T))
colnames(dataS) <- c("Site","Year","Diss")
dataC <- summarise(group_by(dataT, cluster,Year), Diss=mean(Diss,na.rm=T))
colnames(dataC) <- c("Site","Year","Diss")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), Diss=mean(Diss,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,Diss)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = Diss)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(min(plotdata), max(plotdata)),
          title = "Dissimilarite")

dataT <- metrics %>%
  dplyr::select(Year, Code_point_Libelle, Trans,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), Trans=mean(Trans,na.rm=T))
colnames(dataS) <- c("Site","Year","Trans")
dataC <- summarise(group_by(dataT, cluster,Year), Trans=mean(Trans,na.rm=T))
colnames(dataC) <- c("Site","Year","Trans")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), Trans=mean(Trans,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,Trans)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = Trans)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(min(plotdata), max(plotdata)),
          title = "Transitivite")


dataT <- metrics %>%
  dplyr::select(Year, Code_point_Libelle, meanN_voisins,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), meanN_voisins=mean(meanN_voisins,na.rm=T))
colnames(dataS) <- c("Site","Year","meanN_voisins")
dataC <- summarise(group_by(dataT, cluster,Year), meanN_voisins=mean(meanN_voisins,na.rm=T))
colnames(dataC) <- c("Site","Year","meanN_voisins")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), meanN_voisins=mean(meanN_voisins,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,meanN_voisins)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = meanN_voisins)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(min(plotdata), max(plotdata)),
          title = "Nombre moyens de voisins")


dataT <- metrics %>%
  dplyr::select(Year, Code_point_Libelle, Nat_connect,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), Nat_connect=mean(Nat_connect,na.rm=T))
colnames(dataS) <- c("Site","Year","Nat_connect")
dataC <- summarise(group_by(dataT, cluster,Year), Nat_connect=mean(Nat_connect,na.rm=T))
colnames(dataC) <- c("Site","Year","Nat_connect")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), Nat_connect=mean(Nat_connect,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,Nat_connect)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = Nat_connect)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(min(plotdata), max(plotdata)),
          title = "Connectivite naturelle")

dataT <- metrics %>%
  dplyr::select(Year, Code_point_Libelle, N_clust,cluster)

dataS <- summarise(group_by(dataT, Code_point_Libelle,Year), N_clust=mean(N_clust,na.rm=T))
colnames(dataS) <- c("Site","Year","N_clust")
dataC <- summarise(group_by(dataT, cluster,Year), N_clust=mean(N_clust,na.rm=T))
colnames(dataC) <- c("Site","Year","N_clust")
dataC$Site <- as.character(dataC$Site)
dataA <- summarise(group_by(dataT,Year), N_clust=mean(N_clust,na.rm=T))
dataA$Site <- "France"
dataA <- dplyr::select(dataA,Site,Year,N_clust)

datag <- bind_rows(dataS,dataC)
datag <- bind_rows(datag,dataA)

plotdata <- pivot_wider(datag, names_from = Year, 
                        values_from = N_clust)

# save country as row names
plotdata <- as.data.frame(plotdata)
row.names(plotdata) <- plotdata$Site
plotdata$Site <- NULL

# row order
sort.order <- as.integer(order(c(19,18,3,2,12,15,10,17,4,20,21,6,11,7,8,9,14,1,16,13,5,22,23,24,25,26),decreasing = T))

# create the heat map
superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(min(plotdata), max(plotdata)),
          title = "Nombre de clusters")

plotdata <- read_delim("output/graphs/Trends/Tab_for_plotdata_N_blooms.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
nom <- plotdata$Station
plotdata$Station <- NULL
rownames(plotdata) <- nom

sort.order <- as.integer(25:1)

superheat(plotdata,
          scale = FALSE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          #heat.pal = colors,
          order.rows = sort.order,
          heat.lim = c(0, 41),
          title = "Detection de bloom")

