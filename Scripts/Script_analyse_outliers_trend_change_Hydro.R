data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_CHLOROA_phylum_period15_chlafilter_cluster5_div.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)


i=1
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_TEMP",station)
nom_fichier <- paste0(nom_fichier)

data_Toulon <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                          delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=2
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_TEMP",station)
nom_fichier <- paste0(nom_fichier)

data_Ansecarteau <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                               delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=3
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_TEMP",station)
nom_fichier <- paste0(nom_fichier)

data_Antifer <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=4
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_TEMP",station)
nom_fichier <- paste0(nom_fichier)

data_Atso <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                        delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=5
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_TEMP",station)
nom_fichier <- paste0(nom_fichier)

data_Auger <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=6
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_TEMP",station)
nom_fichier <- paste0(nom_fichier)

data_Barcares <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                            delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=7
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_TEMP",station)
nom_fichier <- paste0(nom_fichier)

data_Boisdelachaise <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                                  delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=8
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_TEMP",station)
nom_fichier <- paste0(nom_fichier)

data_Bouzigues <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                             delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=9
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_TEMP",station)
nom_fichier <- paste0(nom_fichier)

data_Cabourg <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=10
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_TEMP",station)
nom_fichier <- paste0(nom_fichier)

data_Calvi <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=11
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_TEMP",station)
nom_fichier <- paste0(nom_fichier)

data_Dianacentre <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                               delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)
i=12
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_TEMP",station)
nom_fichier <- paste0(nom_fichier)

data_Géfosse <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=13
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_TEMP",station)
nom_fichier <- paste0(nom_fichier)

data_Cornard <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=14
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_TEMP",station)
nom_fichier <- paste0(nom_fichier)

data_Hebihens <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                            delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=15
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_TEMP",station)
nom_fichier <- paste0(nom_fichier)

data_Loguivy <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=16
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_TEMP",station)
nom_fichier <- paste0(nom_fichier)

data_MenerRoue <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                             delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=17
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_TEMP",station)
nom_fichier <- paste0(nom_fichier)

data_OuestLoscolo <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                                delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=18
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_TEMP",station)
nom_fichier <- paste0(nom_fichier)

data_ParcLeucate <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                               delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=19
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_TEMP",station)
nom_fichier <- paste0(nom_fichier)

data_Boulogne <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                            delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=20
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_TEMP",station)
nom_fichier <- paste0(nom_fichier)

data_Setemer <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=21
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_TEMP",station)
nom_fichier <- paste0(nom_fichier)

data_Teychan <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

data_outliers_change <- rbind( data_Ansecarteau  ,    data_Antifer        ,  data_Atso       ,      data_Auger     ,     
                               data_Barcares     ,    data_Boisdelachaise ,  data_Boulogne   ,      data_Cabourg   ,       data_Calvi,          
                               data_Cornard     ,     data_Dianacentre   ,   data_Géfosse   ,       data_Hebihens ,        data_Loguivy  ,      
                               data_MenerRoue   ,     data_OuestLoscolo  ,   data_Setemer   ,       data_Teychan  ,data_Toulon, data_ParcLeucate, data_Bouzigues)



datagraph <- data_Calvi
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Calvi"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Calvi"))$TEMP),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP_noseason),col="red",size=1)+
  #
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  #
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de temperature Calvi",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
         pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en temperature",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Calvi_TEMP.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')


datagraph <- data_Ansecarteau
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Anse de Carteau 2"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Anse de Carteau 2"))$TEMP),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de temperature Anse de Carteau",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en temperature",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Ansecarteau_TEMP.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')


datagraph <- data_Antifer
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Antifer ponton pétrolier"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Antifer ponton pétrolier"))$TEMP),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de temperature Antifer ponton pétrolier",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en temperature",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Antifer_TEMP.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Atso
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "At so"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "At so"))$TEMP),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de temperature Atso",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en temperature",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Atso_TEMP.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Auger
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Auger"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Auger"))$TEMP),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de temperature Auger",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en temperature",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Auger_TEMP.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Barcares
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Barcares"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Barcares"))$TEMP),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de temperature Barcares",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en temperature",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Barcares_TEMP.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Boisdelachaise
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Bois de la Chaise large"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Bois de la Chaise large"))$TEMP),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de temperature Bois de la chaise large",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en temperature",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Boisdelachaise_TEMP.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Boulogne
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Point 1 Boulogne"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Point 1 Boulogne"))$TEMP),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de temperature Point 1 Boulogne",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en temperature",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Boulogne_TEMP.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Cabourg
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Cabourg"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Cabourg"))$TEMP),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de temperature Cabourg",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en temperature",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Cabourg_TEMP.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Cornard
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Le Cornard"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Le Cornard"))$TEMP),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de temperature Le Cornard",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en temperature",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Cornard_TEMP.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Dianacentre
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Diana centre"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Diana centre"))$TEMP),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de temperature Diana centre",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en temperature",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Dianacentre_TEMP.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Géfosse
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Géfosse"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Géfosse"))$TEMP),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de temperature Géfosse",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en temperature",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Gefosse_TEMP.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Hebihens
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "les Hébihens"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "les Hébihens"))$TEMP),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de temperature les Hébihens",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en temperature",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Hebihens_TEMP.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Loguivy
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Loguivy"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Loguivy"))$TEMP),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de temperature Loguivy",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en temperature",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Loguivy_TEMP.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_MenerRoue
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Men er Roue"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Men er Roue"))$TEMP),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de temperature Men er Roue",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en temperature",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('MenerRoue_TEMP.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_OuestLoscolo
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Ouest Loscolo"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Ouest Loscolo"))$TEMP),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de temperature Ouest Loscolo",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en temperature",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('OuestLoscolo_TEMP.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')


datagraph <- data_Setemer
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Sète mer"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Sète mer"))$TEMP),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de temperature Sète mer",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en temperature",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Setemer_TEMP.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Teychan
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Teychan bis"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Teychan bis"))$TEMP),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de temperature Teychan bis",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en temperature",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Teychan_TEMP.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Toulon
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "22B - Toulon gde rade"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "22B - Toulon gde rade"))$TEMP),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de temperature 22B - Toulon gde rade",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en temperature",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Toulon_TEMP.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_ParcLeucate
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Parc Leucate 2"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Parc Leucate 2"))$TEMP),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de temperature Parc Leucate 2",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en temperature",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('ParcLeucate_TEMP.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Bouzigues
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Bouzigues (a)"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Bouzigues (a)"))$TEMP),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=TEMP_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de temperature Bouzigues (a)",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en temperature",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Bouzigues_TEMP.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')



for (i in (1:21)){
  data <- data_outliers_change
  station <- levels(as.factor(data$Code_point_Libelle))[i]
  Table <- filter(data, Code_point_Libelle == station)
  
  m_cop3 <- lm(TEMP_noseason ~ Date, data=filter(data_outliers_change,Code_point_Libelle == station))
  regline_cop3 <- predict(m_cop3)
  # plot the linear trend
  plot(filter(data_outliers_change,Code_point_Libelle == station)$TEMP_noseason,type="o",
       main = paste0("Tendance pour ",station))
  lines(regline_cop3, col="red",lwd=3)
  
  test_lm <- summary(m_cop3)
  test_res <- trend.test(residuals(m_cop3))
  print(paste("pente:",test_lm$coefficients[2,1],"pval:",test_lm$coefficients[2,4],"residuals :",test_res$p.value,station))
}



####### SALINITE ######
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)


i=1
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_SALI",station)
nom_fichier <- paste0(nom_fichier)

data_Toulon <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                          delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=2
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_SALI",station)
nom_fichier <- paste0(nom_fichier)

data_Ansecarteau <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                               delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=3
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_SALI",station)
nom_fichier <- paste0(nom_fichier)

data_Antifer <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=4
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_SALI",station)
nom_fichier <- paste0(nom_fichier)

data_Atso <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                        delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=5
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_SALI",station)
nom_fichier <- paste0(nom_fichier)

data_Auger <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=6
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_SALI",station)
nom_fichier <- paste0(nom_fichier)

data_Barcares <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                            delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=7
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_SALI",station)
nom_fichier <- paste0(nom_fichier)

data_Boisdelachaise <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                                  delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=8
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_SALI",station)
nom_fichier <- paste0(nom_fichier)

data_Bouzigues <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                             delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=9
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_SALI",station)
nom_fichier <- paste0(nom_fichier)

data_Cabourg <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=10
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_SALI",station)
nom_fichier <- paste0(nom_fichier)

data_Calvi <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=11
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_SALI",station)
nom_fichier <- paste0(nom_fichier)

data_Dianacentre <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                               delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)
i=12
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_SALI",station)
nom_fichier <- paste0(nom_fichier)

data_Géfosse <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=13
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_SALI",station)
nom_fichier <- paste0(nom_fichier)

data_Cornard <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=14
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_SALI",station)
nom_fichier <- paste0(nom_fichier)

data_Hebihens <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                            delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=15
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_SALI",station)
nom_fichier <- paste0(nom_fichier)

data_Loguivy <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=16
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_SALI",station)
nom_fichier <- paste0(nom_fichier)

data_MenerRoue <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                             delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=17
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_SALI",station)
nom_fichier <- paste0(nom_fichier)

data_OuestLoscolo <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                                delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=18
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_SALI",station)
nom_fichier <- paste0(nom_fichier)

data_ParcLeucate <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                               delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=19
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_SALI",station)
nom_fichier <- paste0(nom_fichier)

data_Boulogne <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                            delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=20
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_SALI",station)
nom_fichier <- paste0(nom_fichier)

data_Setemer <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

i=21
station <- levels(as.factor(data$Code_point_Libelle))[i]
print(station)
nom_fichier <- paste0("Outliers_regularise_desaisonalise_SALI",station)
nom_fichier <- paste0(nom_fichier)

data_Teychan <- read_delim(paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",grouping_mark = ""), trim_ws = TRUE)

data_outliers_change <- rbind( data_Ansecarteau  ,    data_Antifer        ,  data_Atso       ,      data_Auger     ,     
                               data_Barcares     ,    data_Boisdelachaise ,  data_Boulogne   ,      data_Cabourg   ,       data_Calvi,          
                               data_Cornard     ,     data_Dianacentre   ,   data_Géfosse   ,       data_Hebihens ,        data_Loguivy  ,      
                               data_MenerRoue   ,     data_OuestLoscolo  ,   data_Setemer   ,       data_Teychan  ,data_Toulon, data_ParcLeucate, data_Bouzigues)



datagraph <- data_Calvi
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Calvi"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Calvi"))$SALI),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI_noseason),col="red",size=1)+
  #
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  #
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de salinite Calvi",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
         pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en salinite",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Calvi_SALI.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')


datagraph <- data_Ansecarteau
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Anse de Carteau 2"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Anse de Carteau 2"))$SALI),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de salinite Anse de Carteau",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en salinite",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Ansecarteau_SALI.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')


datagraph <- data_Antifer
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Antifer ponton pétrolier"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Antifer ponton pétrolier"))$SALI),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de salinite Antifer ponton pétrolier",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en salinite",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Antifer_SALI.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Atso
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "At so"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "At so"))$SALI),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de salinite Atso",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en salinite",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Atso_SALI.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Auger
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Auger"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Auger"))$SALI),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de salinite Auger",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en salinite",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Auger_SALI.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Barcares
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Barcares"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Barcares"))$SALI),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de salinite Barcares",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en salinite",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Barcares_SALI.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Boisdelachaise
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Bois de la Chaise large"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Bois de la Chaise large"))$SALI),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de salinite Bois de la chaise large",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en salinite",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Boisdelachaise_SALI.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Boulogne
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Point 1 Boulogne"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Point 1 Boulogne"))$SALI),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de salinite Point 1 Boulogne",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en salinite",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Boulogne_SALI.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Cabourg
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Cabourg"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Cabourg"))$SALI),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de salinite Cabourg",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en salinite",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Cabourg_SALI.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Cornard
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Le Cornard"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Le Cornard"))$SALI),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de salinite Le Cornard",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en salinite",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Cornard_SALI.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Dianacentre
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Diana centre"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Diana centre"))$SALI),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de salinite Diana centre",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en salinite",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Dianacentre_SALI.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Géfosse
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Géfosse"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Géfosse"))$SALI),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de salinite Géfosse",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en salinite",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Gefosse_SALI.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Hebihens
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "les Hébihens"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "les Hébihens"))$SALI),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de salinite les Hébihens",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en salinite",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Hebihens_SALI.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Loguivy
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Loguivy"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Loguivy"))$SALI),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de salinite Loguivy",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en salinite",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Loguivy_SALI.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_MenerRoue
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Men er Roue"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Men er Roue"))$SALI),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de salinite Men er Roue",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en salinite",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('MenerRoue_SALI.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_OuestLoscolo
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Ouest Loscolo"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Ouest Loscolo"))$SALI),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de salinite Ouest Loscolo",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en salinite",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('OuestLoscolo_SALI.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')


datagraph <- data_Setemer
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Sète mer"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Sète mer"))$SALI),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de salinite Sète mer",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en salinite",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Setemer_SALI.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Teychan
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Teychan bis"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Teychan bis"))$SALI),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de salinite Teychan bis",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en salinite",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Teychan_SALI.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Toulon
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "22B - Toulon gde rade"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "22B - Toulon gde rade"))$SALI),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de salinite 22B - Toulon gde rade",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en salinite",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Toulon_SALI.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_ParcLeucate
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Parc Leucate 2"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Parc Leucate 2"))$SALI),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de salinite Parc Leucate 2",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en salinite",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('ParcLeucate_SALI.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')

datagraph <- data_Bouzigues
ggplot()+
  geom_line(aes(x=as.data.frame(filter(data,Code_point_Libelle == "Bouzigues (a)"))$Date,y=as.data.frame(filter(data,Code_point_Libelle == "Bouzigues (a)"))$SALI),col="green",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI),col="blue",size=1)+
  geom_line(data =  datagraph,aes(x=Date,y=SALI_noseason,colour=Outlier),col="red",size=1)+
  
  geom_vline(xintercept =  datagraph$Date[complete.cases( datagraph$Changepoint)],size=2)+
  scale_x_date(breaks = seq( datagraph$Date[1], datagraph$Date[nrow( datagraph)],by=100))+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))+
  scale_colour_manual(values = c("red","orange"))+
  scale_size_manual(values = c(1,5))+
  geom_text(aes(x= datagraph$Date[complete.cases( datagraph$Changepoint)]-180,y=4,label= datagraph$Date[complete.cases( datagraph$Changepoint)]))+
  labs(title = "Serie temporelle de salinite Bouzigues (a)",
       subtitle = paste("Trend :", round(datagraph$Trend[1],digits = 2),"pval:",round(datagraph$pvaltrend[1],digits = 4),"
       pval changement",round(datagraph$pvalchange[1],digits = 4), paste("Sen's slope:",round(datagraph$slopesens[1],digits=5), "pval:",round(datagraph$psens[1],digits=4))),
       x = "Date", y = "Concentration en salinite",colour="Outlier",
       caption = "Bleu: serie regularise, Rouge: regularise + desaisonnalise, Vert:original")
ggsave('Bouzigues_SALI.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Outliers_change_trend/",dpi = 600, width = 400, height = 280, units = 'mm')



for (i in (1:21)){
  data <- data_outliers_change
  station <- levels(as.factor(data$Code_point_Libelle))[i]
  Table <- filter(data, Code_point_Libelle == station)
  
  m_cop3 <- lm(SALI_noseason ~ Date, data=filter(data_outliers_change,Code_point_Libelle == station))
  regline_cop3 <- predict(m_cop3)
  # plot the linear trend
  plot(filter(data_outliers_change,Code_point_Libelle == station)$SALI_noseason,type="o",
       main = paste0("Tendance pour ",station))
  lines(regline_cop3, col="red",lwd=3)
  
  test_lm <- summary(m_cop3)
  test_res <- trend.test(residuals(m_cop3))
  print(paste("pente:",test_lm$coefficients[2,1],"pval:",test_lm$coefficients[2,4],"residuals :",test_res$p.value,station))
}


Table_regul_test <- Table_regul
Table_regul_test$Mois <- month(Table_regul_test$Date)
test <- summarise(group_by(Table_regul_test,year,Mois), mean_sali=mean(SALI_noseason))
MannKendall(test$mean_sali)
mannKen(test$mean_sali)
