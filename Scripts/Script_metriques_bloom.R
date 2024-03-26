### Associer les métriques pour le suivi avant/pendant/apres bloom
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

data$Abdtot <- apply(data[,247],1,sum,na.rm=T)

data_results_DRF <- c("","")
data_results_DRF <- as.data.frame(data_results_DRF)
for (i in 1:nrow(data)){ 
data_DRF <- data[i,] |> 
  dplyr::select(Actinoptychus:Auricula)

data_DRF[is.na(data_DRF)] <- 0
if (apply(data_DRF,1,sum,na.rm=T) >0){
ZM <- rad.zipfbrot(data_DRF)
as.numeric(ZM$coefficients[2])
jpeg(paste(data[i,"Code_point_Libelle"]$Code_point_Libelle,data[i,"Date"]$Date,".png")
  , quality = 75)
plot(ZM)
dev.off()

data_results_DRF[i,1] <- data[i,"Code_point_Libelle"]$Code_point_Libelle
data_results_DRF[i,2] <- data[i,"Date"]$Date
data_results_DRF[i,3] <- length(ZM$fitted.values) # Richesse spe
data_results_DRF[i,4] <- as.numeric(ZM$coefficients[2]) #gamma
data_results_DRF[i,5] <- as.numeric(ZM$coefficients[3]) #beta
} else { data_results_DRF[i,1] <- data[i,"Code_point_Libelle"]$Code_point_Libelle
data_results_DRF[i,2] <- data[i,"Date"]$Date
data_results_DRF[i,3] <- 0 # Richesse spe
data_results_DRF[i,4] <- NA #gamma
data_results_DRF[i,5] <- NA }

colnames(data_results_DRF) <- c("Code_point_Libelle","Date","Rspe","gamma","beta") 
print(i/nrow(data))


}

data_resume <- bind_cols(data_results_DRF,data$Abdtot)
colnames(data_resume) <- c("Code_point_Libelle","Date","Rspe","gamma","beta","Abdtot") 


data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)

data_bloom_all_bind <- dplyr::select(data_bloom,Code_point_Libelle,Date,P_dominance:Bloom_Genre)

data_bloom_all_bind[grep("Antifer",x = data_bloom_all_bind$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

data_plusbloom <- left_join(data, data_bloom_all_bind, by = join_by(Code_point_Libelle,Date))

data_plusbloom$Abdtot <- apply(data_plusbloom[,248:265],1,sum,na.rm=T)

write.csv2(data_plusbloom,file="data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", row.names = FALSE,dec = ".")

data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)
doublons <- data[duplicated(data$ID.interne.passage) |
                                 duplicated(data$ID.interne.passage, fromLast = TRUE), ]

# Filtre des doublons hydro :
resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

# On supprime les lignes en doublon dans le jeu de données initial
data_unique <- subset(data, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
# On les remets ces doublons filtres
data <- bind_rows(data_unique,resultat_filtre)
# On remet au propre
data <- data |>
write.csv2(data,file="data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", row.names = FALSE,dec = ".")


# Essayer une autre methode pour la communaute à la place de rang frequence :
# Chargement des données
abondances_communaute1 <- data_bloom[25,24:247]
t <- as.data.frame(t(abondances_communaute1))
t <- t |>
  arrange(desc(V1))
t <- t[complete.cases(t),]

abondances_communaute1<- t(t)
abondances_communaute1 <- as.numeric(abondances_communaute1)


# Fonction pour calculer les proportions cumulatives
calculate_cumulative_proportions <- function(abundances) {
  proportions <- abundances / sum(abundances)
  cumulative_proportions <- cumsum(proportions)
  return(cumulative_proportions)
}
cumulative_proportions_communaute1 <- calculate_cumulative_proportions(abondances_communaute1)
plot(cumulative_proportions_communaute1)


propcomm <- cumulative_proportions_communaute1
rang <- seq(1,length(propcomm)) 

test <- as.data.frame(cbind(propcomm,rang))
test[10,c(1,2)] <- 0
test <- test |>
  arrange(desc(propcomm))


plot(test$rang,test$propcomm,"l")
abline(h=0.66)

# Representation changement au cours du temps 
data_bloom <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                             grouping_mark = ""), trim_ws = TRUE)

data_bloomlong <- data_bloom |> 
  dplyr::select(Code_point_Libelle,Date,c(CHLOROA,Abdtot,Shannon, Pielou,Simpson, BergerParker,P_dominance,Rspe,gamma,beta)) |>
  mutate(CHLOROA = log(CHLOROA+1),Abdtot = log(Abdtot+1),gamma = gamma, beta = log(beta+1)) |>
  pivot_longer(CHLOROA:beta,names_to = "Param",values_to = "Valeur")


data_bloom_date <- dplyr::select(filter(data_bloom, P_dominance >0),Code_point_Libelle, Date)  
data_bloom_date$Bloom <- "OUI"

data_bloomlong <- left_join(data_bloomlong,data_bloom_date,by = join_by(Code_point_Libelle,Date))

ggplot(data = filter(data_bloomlong,Code_point_Libelle == "Ouest Loscolo"))+
  geom_line(aes(x=Date,y=Valeur))+
  geom_point(aes(x=Date,y=Valeur,colour = Bloom))+
  facet_grid(Param~Code_point_Libelle,scales = "free")

# En utilisant la courbe d'abondance cumulée
calculate_cumulative_proportions <- function(abundances) {
  proportions <- abundances / sum(abundances)
  cumulative_proportions <- cumsum(proportions)
  return(cumulative_proportions)
}

data_results_CAC <- c("","")
data_results_CAC <- as.data.frame(data_results_CAC)


data_bloom[data_bloom == 0] <- NA
for (i in c(1:nrow(data_bloom))){ 
  if (i %in% c(2535, 3702, 3837, 3862, 3877, 3886, 4004, 4063, 5511,5528,5774,5927,5942,5945,5946,6742, 6992)){
    abondances_communaute1 <- data_bloom[i,24:247]
    abd_com_mat <- as.data.frame(t(abondances_communaute1))
    abd_com_mat <- abd_com_mat |>
      arrange(desc(V1))
    abd_com_mat <- abd_com_mat[complete.cases(abd_com_mat),]
    
    if (length(abd_com_mat)>0){
      abondances_communaute1<- t(abd_com_mat)
      abondances_communaute1 <- as.numeric(abondances_communaute1)
      
      cumulative_proportions_communaute1 <- calculate_cumulative_proportions(abondances_communaute1)
      
      propcomm <- cumulative_proportions_communaute1
      rang <- seq(1,length(propcomm)) 
      
      data_cac <- as.data.frame(cbind(propcomm,rang))
      data_cac[length(propcomm)+1,c(1,2)] <- 0
      data_cac <- data_cac |>
        arrange(desc(propcomm))
      
      plot(data_cac$rang,data_cac$propcomm,"l")
      
      if (length(propcomm) >= 1){
        
        m1 <- lm(propcomm~rang, data = data_cac[c(nrow(data_cac),nrow(data_cac)-1),])
        summary(m1)
        
        mml <- data.frame(rang = seq(0, 200, length.out = 40000))
        mml$v <- predict(m1, newdata = mml)
        
        N_66 <- min(filter(mml,v >= 0.66)$rang)
        Prop_max <- 1
        N_50 <- min(filter(mml,v >= 0.50)$rang)
        N_25 <- min(filter(mml,v >= 0.25)$rang)
        N_75 <- min(filter(mml,v >= 0.75)$rang)
        N_90 <- min(filter(mml,v >= 0.90)$rang)
        R2 <- cor(m1$model[,1],m1$fitted.values)^2
        P_1 <- filter(data_cac, rang == 1)$propcomm
        P_2 <- filter(data_cac, rang == 2)$propcomm
        P_3 <- filter(data_cac, rang == 3)$propcomm
        
        pente <- filter(data_cac, rang == 1)$propcomm/1
        
        data_results_CAC[i,1] <- data_bloom[i,"Code_point_Libelle"]$Code_point_Libelle
        data_results_CAC[i,2] <- data_bloom[i,"Date"]$Date
        data_results_CAC[i,3] <- Prop_max
        data_results_CAC[i,4] <- N_25
        data_results_CAC[i,5] <- N_50
        data_results_CAC[i,6] <- N_66
        data_results_CAC[i,7] <- N_75
        data_results_CAC[i,8] <- N_90
        data_results_CAC[i,9] <- R2
        data_results_CAC[i,10] <- P_1
        data_results_CAC[i,11] <- P_2
        data_results_CAC[i,12] <- P_3
        
        jpeg(paste0("output/graphs/Bloom_description/CAC/",data_bloom[i,"Code_point_Libelle"]$Code_point_Libelle,data_bloom[i,"Date"]$Date,".png")
             , quality = 75)
        plot(data_cac$rang,data_cac$propcomm,"l", main = paste(data_bloom[i,"Code_point_Libelle"]$Code_point_Libelle,data_bloom[i,"Date"]$Date))
        abline(a=0, b= pente, col = "blue") #215
        dev.off()
      } else {
        data_results_CAC[i,1] <- data_bloom[i,"Code_point_Libelle"]$Code_point_Libelle
        data_results_CAC[i,2] <- data_bloom[i,"Date"]$Date
        data_results_CAC[i,3] <- NA
        data_results_CAC[i,4] <- NA
        data_results_CAC[i,5] <- NA
        data_results_CAC[i,6] <- NA
        data_results_CAC[i,7] <- NA
        data_results_CAC[i,8] <- NA
        data_results_CAC[i,9] <- NA
        data_results_CAC[i,10] <- NA
        data_results_CAC[i,11] <- NA
        data_results_CAC[i,12] <- NA
        
      }
    } else {
      data_results_CAC[i,1] <- data_bloom[i,"Code_point_Libelle"]$Code_point_Libelle
      data_results_CAC[i,2] <- data_bloom[i,"Date"]$Date
      data_results_CAC[i,3] <- NA
      data_results_CAC[i,4] <- NA
      data_results_CAC[i,5] <- NA
      data_results_CAC[i,6] <- NA
      data_results_CAC[i,7] <- NA
      data_results_CAC[i,8] <- NA
      data_results_CAC[i,9] <- NA
      data_results_CAC[i,10] <- NA
      data_results_CAC[i,11] <- NA
      data_results_CAC[i,12] <- NA
      
    }
  } else {
    if (i %in% c(2,   22,   38,   48,   95,  107,  735,  783, 1732, 1914, 1930, 2190, 2428, 2444, 2445, 2670, 2673, 2679, 2681, 3048, 3201, 3202, 3203, 3205,
                3207, 3209, 3215, 3216, 3217, 3218, 3219, 3223, 3228, 3232, 3233, 3241, 3242, 3252, 3256, 3257, 3262, 3285, 3322, 3356, 3361, 3398, 3410, 3414,
                3437, 3460, 3474, 3516, 3517, 3519, 3672, 3673, 3693, 3703, 3859, 5856, 6041, 6145)){
      abondances_communaute1 <- data_bloom[i,24:247]
      abd_com_mat <- as.data.frame(t(abondances_communaute1))
      abd_com_mat <- abd_com_mat |>
        arrange(desc(V1))
      abd_com_mat <- abd_com_mat[complete.cases(abd_com_mat),]
      
      if (length(abd_com_mat)>0){
        abondances_communaute1<- t(abd_com_mat)
        abondances_communaute1 <- as.numeric(abondances_communaute1)
        
        cumulative_proportions_communaute1 <- calculate_cumulative_proportions(abondances_communaute1)
        
        propcomm <- cumulative_proportions_communaute1
        rang <- seq(1,length(propcomm)) 
        
        data_cac <- as.data.frame(cbind(propcomm,rang))
        data_cac[length(propcomm)+1,c(1,2)] <- 0
        data_cac <- data_cac |>
          arrange(desc(propcomm))
        
        plot(data_cac$rang,data_cac$propcomm,"l")
        m1 <- lm(propcomm~rang, data = data_cac[c(nrow(data_cac),(nrow(data_cac)-(nrow(data_cac)-1))),])
        summary(m1)
        
        mml <- data.frame(rang = seq(0, 200, length.out = 40000))
        mml$v <- predict(m1, newdata = mml)
        
        N_66 <- min(filter(mml,v >= 0.66)$rang)
        Prop_max <- 1
        N_50 <- min(filter(mml,v >= 0.50)$rang)
        N_25 <- min(filter(mml,v >= 0.25)$rang)
        N_75 <- min(filter(mml,v >= 0.75)$rang)
        N_90 <- min(filter(mml,v >= 0.90)$rang)
        R2 <- cor(m1$model[,1],m1$fitted.values)^2
        P_1 <- filter(data_cac, rang == 1)$propcomm
        P_2 <- ifelse(nrow(data_cac)>=3,filter(data_cac, rang == 2)$propcomm,NA)
        P_3 <- ifelse(nrow(data_cac)>=4,filter(data_cac, rang == 3)$propcomm,NA)
        
        pente <- m1$coefficients[2]
        
        data_results_CAC[i,1] <- data_bloom[i,"Code_point_Libelle"]$Code_point_Libelle
        data_results_CAC[i,2] <- data_bloom[i,"Date"]$Date
        data_results_CAC[i,3] <- Prop_max
        data_results_CAC[i,4] <- N_25
        data_results_CAC[i,5] <- N_50
        data_results_CAC[i,6] <- N_66
        data_results_CAC[i,7] <- N_75
        data_results_CAC[i,8] <- N_90
        data_results_CAC[i,9] <- R2
        data_results_CAC[i,10] <- P_1
        data_results_CAC[i,11] <- P_2
        data_results_CAC[i,12] <- P_3
        
        jpeg(paste0("output/graphs/Bloom_description/CAC/",data_bloom[i,"Code_point_Libelle"]$Code_point_Libelle,data_bloom[i,"Date"]$Date,".png")
             , quality = 75)
        plot(data_cac$rang,data_cac$propcomm,"l", main = paste(data_bloom[i,"Code_point_Libelle"]$Code_point_Libelle,data_bloom[i,"Date"]$Date))
        abline(a=0, b= pente, col = "blue") #307
        dev.off()
      }}
      
    else {
abondances_communaute1 <- data_bloom[i,24:247]
abd_com_mat <- as.data.frame(t(abondances_communaute1))
abd_com_mat <- abd_com_mat |>
  arrange(desc(V1))
abd_com_mat <- abd_com_mat[complete.cases(abd_com_mat),]

if (length(abd_com_mat)>0){
abondances_communaute1<- t(abd_com_mat)
abondances_communaute1 <- as.numeric(abondances_communaute1)

cumulative_proportions_communaute1 <- calculate_cumulative_proportions(abondances_communaute1)

propcomm <- cumulative_proportions_communaute1
rang <- seq(1,length(propcomm)) 

data_cac <- as.data.frame(cbind(propcomm,rang))
data_cac[length(propcomm)+1,c(1,2)] <- 0
data_cac <- data_cac |>
  arrange(desc(propcomm))

plot(data_cac$rang,data_cac$propcomm)

if (length(propcomm) > 2){

m1 <- drm(propcomm~rang, data = data_cac, fct = MM.3())
summary(m1)

plot(m1, log = '', pch = 17, main = "Fitted MM curve")

mml <- data.frame(rang = seq(0, 200, length.out = 40000))
mml$v <- predict(m1, newdata = mml)

N_63 <- min(filter(mml,v >= 0.63)$rang)
Prop_max <- as.numeric(m1$coefficients[2])
N_50 <- min(filter(mml,v >= 0.50)$rang)
N_25 <- min(filter(mml,v >= 0.25)$rang)
N_75 <- min(filter(mml,v >= 0.75)$rang)
N_90 <- min(filter(mml,v >= 0.90)$rang)
R2 <- cor(m1$predres[,1],m1$data$propcomm)^2
P_1 <- filter(data_cac, rang == 1)$propcomm
P_2 <- filter(data_cac, rang == 2)$propcomm
P_3 <- filter(data_cac, rang == 3)$propcomm

pente <- filter(data_cac, rang == 1)$propcomm/1

data_results_CAC[i,1] <- data_bloom[i,"Code_point_Libelle"]$Code_point_Libelle
data_results_CAC[i,2] <- data_bloom[i,"Date"]$Date
data_results_CAC[i,3] <- Prop_max
data_results_CAC[i,4] <- N_25
data_results_CAC[i,5] <- N_50
data_results_CAC[i,6] <- N_66
data_results_CAC[i,7] <- N_75
data_results_CAC[i,8] <- N_90
data_results_CAC[i,9] <- R2
data_results_CAC[i,10] <- P_1
data_results_CAC[i,11] <- P_2
data_results_CAC[i,12] <- P_3

jpeg(paste0("output/graphs/Bloom_description/CAC/",data_bloom[i,"Code_point_Libelle"]$Code_point_Libelle,data_bloom[i,"Date"]$Date,".png")
     , quality = 75)
plot(m1, log = '', pch = 17, main = paste(data_bloom[i,"Code_point_Libelle"]$Code_point_Libelle,data_bloom[i,"Date"]$Date))
abline(a=0, b= pente, col = "blue") #373
dev.off()
} else {
  data_results_CAC[i,1] <- data_bloom[i,"Code_point_Libelle"]$Code_point_Libelle
  data_results_CAC[i,2] <- data_bloom[i,"Date"]$Date
  data_results_CAC[i,3] <- NA
  data_results_CAC[i,4] <- NA
  data_results_CAC[i,5] <- NA
  data_results_CAC[i,6] <- NA
  data_results_CAC[i,7] <- NA
  data_results_CAC[i,8] <- NA
  data_results_CAC[i,9] <- NA
  data_results_CAC[i,10] <- NA
  data_results_CAC[i,11] <- NA
  data_results_CAC[i,12] <- NA

}
} else {
  data_results_CAC[i,1] <- data_bloom[i,"Code_point_Libelle"]$Code_point_Libelle
  data_results_CAC[i,2] <- data_bloom[i,"Date"]$Date
  data_results_CAC[i,3] <- NA
  data_results_CAC[i,4] <- NA
  data_results_CAC[i,5] <- NA
  data_results_CAC[i,6] <- NA
  data_results_CAC[i,7] <- NA
  data_results_CAC[i,8] <- NA
  data_results_CAC[i,9] <- NA
  data_results_CAC[i,10] <- NA
  data_results_CAC[i,11] <- NA
  data_results_CAC[i,12] <- NA

}
}
print(i/nrow(data_bloom)*100)
  }
}

for (i in c(2,   22,   38,   48,   95,  107,  735,  783, 1732, 1914, 1930, 2190, 2428, 2444, 2445, 2670, 2673, 2679, 2681, 3048, 3201, 3202, 3203, 3205,
                3207, 3209, 3215, 3216, 3217, 3218, 3219, 3223, 3228, 3232, 3233, 3241, 3242, 3252, 3256, 3257, 3262, 3285, 3322, 3356, 3361, 3398, 3410, 3414,
                3437, 3460, 3474, 3516, 3517, 3519, 3672, 3673, 3693, 3703, 3859, 5856, 6041, 6145)){
  abondances_communaute1 <- data_bloom[i,24:247]
  abd_com_mat <- as.data.frame(t(abondances_communaute1))
  abd_com_mat <- abd_com_mat |>
    arrange(desc(V1))
  abd_com_mat <- abd_com_mat[complete.cases(abd_com_mat),]
  
  if (length(abd_com_mat)>0){
    abondances_communaute1<- t(abd_com_mat)
    abondances_communaute1 <- as.numeric(abondances_communaute1)
    
    cumulative_proportions_communaute1 <- calculate_cumulative_proportions(abondances_communaute1)
    
    propcomm <- cumulative_proportions_communaute1
    rang <- seq(1,length(propcomm)) 
    
    data_cac <- as.data.frame(cbind(propcomm,rang))
    data_cac[length(propcomm)+1,c(1,2)] <- 0
    data_cac <- data_cac |>
      arrange(desc(propcomm))
    
    plot(data_cac$rang,data_cac$propcomm,"l")
      m1 <- lm(propcomm~rang, data = data_cac[c(nrow(data_cac),(nrow(data_cac)-(nrow(data_cac)-1))),])
      summary(m1)
      
      mml <- data.frame(rang = seq(0, 200, length.out = 40000))
      mml$v <- predict(m1, newdata = mml)
      
      N_66 <- min(filter(mml,v >= 0.66)$rang)
      Prop_max <- 1
      N_50 <- min(filter(mml,v >= 0.50)$rang)
      N_25 <- min(filter(mml,v >= 0.25)$rang)
      N_75 <- min(filter(mml,v >= 0.75)$rang)
      N_90 <- min(filter(mml,v >= 0.90)$rang)
      R2 <- cor(m1$model[,1],m1$fitted.values)^2
      P_1 <- filter(data_cac, rang == 1)$propcomm
      P_2 <- ifelse(nrow(data_cac)>=3,filter(data_cac, rang == 2)$propcomm,NA)
      P_3 <- ifelse(nrow(data_cac)>=4,filter(data_cac, rang == 3)$propcomm,NA)
      
      pente <- m1$coefficients[2]
        
      data_results_CAC[i,1] <- data_bloom[i,"Code_point_Libelle"]$Code_point_Libelle
      data_results_CAC[i,2] <- data_bloom[i,"Date"]$Date
      data_results_CAC[i,3] <- Prop_max
      data_results_CAC[i,4] <- N_25
      data_results_CAC[i,5] <- N_50
      data_results_CAC[i,6] <- N_66
      data_results_CAC[i,7] <- N_75
      data_results_CAC[i,8] <- N_90
      data_results_CAC[i,9] <- R2
      data_results_CAC[i,10] <- P_1
      data_results_CAC[i,11] <- P_2
      data_results_CAC[i,12] <- P_3
      
      jpeg(paste0("output/graphs/Bloom_description/CAC/",data_bloom[i,"Code_point_Libelle"]$Code_point_Libelle,data_bloom[i,"Date"]$Date,".png")
           , quality = 75)
      plot(data_cac$rang,data_cac$propcomm,"l", main = paste(data_bloom[i,"Code_point_Libelle"]$Code_point_Libelle,data_bloom[i,"Date"]$Date))
      abline(a=0, b= pente, col = "blue")
      dev.off()
    }}

}
