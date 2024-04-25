# Load packages
library(readr)
library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)
library(lubridate)
library(DescTools)
library(gratia)
# Import data
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

# Keep only the counts, station and date
data_count <- data[,c(2,8,24:328)]
# Replace NA's by 0
data_count[is.na(data_count)] <- 0
# Doing the Hellinger transformation 
data_hel <- decostand(data_count[,-c(1,2)], method = "hellinger")
# Put the station and date again
data_hel <- bind_cols(data_count[,c(1,2)],data_hel)

# Creating a df to store the results
data_results_beta <- c("","")
data_results_beta <- as.data.frame(data_results_beta)



### BETWEEN DATE N & N-1 ####

i = 1
  data_station <- filter(data_hel, Code_point_Libelle == levels(as.factor(data_hel$Code_point_Libelle))[i])
  for (j in 2:nrow(data_station)){ 
    
  Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
  Date1 <- data_station[j,"Date"]$Date
  Date2 <- data_station[j-1,"Date"]$Date
  Com1 <- data_station[j,c(3:307)]
  rownames(Com1) <- Date1
  Com2 <- data_station[j-1,c(3:307)]
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
        Com1 <- data_station[j,c(3:307)]
        rownames(Com1) <- Date1
        Com2 <- data_station[j-1,c(3:307)]
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
# Store the results
write.csv2(data_results_beta,file="data_modif/data_div_beta_N-1_final.csv", row.names = FALSE,dec = ".")

# Merge the results to the original data
# Import the results
beta <- read_delim("data_modif/data_div_beta_N-1_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

# Import data
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)


colnames(beta)[4] <- "Date"

data <- dplyr::select(data, Code_point_Libelle,ID.interne.passage, cluster,Bloom,Date)

data_join <- left_join(data,beta)

# There are duplicates to delete

doublons <- data_join[duplicated(data_join$ID.interne.passage) |
                   duplicated(data_join$ID.interne.passage, fromLast = TRUE), ]


resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_join, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

doublons <- data_join[duplicated(data_join$ID.interne.passage) |
                        duplicated(data_join$ID.interne.passage, fromLast = TRUE), ]

resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_join, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

doublons <- data_join[duplicated(data_join$ID.interne.passage) |
                        duplicated(data_join$ID.interne.passage, fromLast = TRUE), ]

resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_join, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

data_beta <- data_join

# Adding info about there is a bloom or not
data_beta$EpBloom <- "OUI"
data_beta[is.na(data_beta$Bloom),8] <- "NON"

# Loop for imputing an info about if there are a serie of blooms and "OUI" mean's it is the first date of the bloom
for (i in 2:nrow(data_beta)){
  if (data_beta$EpBloom[i] == "OUI" & (data_beta$EpBloom[i-1] == "OUI" | data_beta$EpBloom[i-1] == "Sucession") ){
    data_beta$EpBloom[i] <- "Sucession"
  }
}
# Save the data
write.csv2(data_beta,file="data_modif/data_div_beta_ok_N-1_final.csv", row.names = FALSE,dec = ".")


#### Compare date N and N-1 ####
# Import data
beta <- read_delim("data_modif/data_div_beta_ok_N-1_final.csv", 
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
DunnTest(data_kw$`Bray-Curtis`~data_kw$comp,method="BH")

# BETWEEN  DATE N & N+1 ####
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

# Keep only the count
data_count <- data[,c(2,8,24:328)]
# Replace NA's by 0
data_count[is.na(data_count)] <- 0
data_hel <- decostand(data_count[,-c(1,2)], method = "hellinger")
data_hel <- bind_cols(data_count[,c(1,2)],data_hel)

# Create a df to store the results
data_results_beta <- c("","")
data_results_beta <- as.data.frame(data_results_beta)

i = 1
data_station <- filter(data_hel, Code_point_Libelle == levels(as.factor(data_hel$Code_point_Libelle))[i])
for (j in 1:(nrow(data_station)-1)){
  Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
  
  Date1 <- data_station[j,"Date"]$Date
  Date2 <- data_station[j+1,"Date"]$Date
  Com1 <- data_station[j,c(3:307)]
  rownames(Com1) <- Date1
  Com2 <- data_station[j+1,c(3:307)]
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
    Com1 <- data_station[j,c(3:307)]
    rownames(Com1) <- Date1
    Com2 <- data_station[j+1,c(3:307)]
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

write.csv2(data_results_beta,file="data_modif/data_div_betaN+1_final.csv", row.names = FALSE,dec = ".")

# Merge the results to the original data
# Import the results
beta <- read_delim("data_modif/data_div_betaN+1_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

# Import data
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)


colnames(beta)[4] <- "Date"

data <- dplyr::select(data, Code_point_Libelle,ID.interne.passage, cluster,Bloom,Date)

data_join <- left_join(data,beta)

# There are duplicates to delete

doublons <- data_join[duplicated(data_join$ID.interne.passage) |
                        duplicated(data_join$ID.interne.passage, fromLast = TRUE), ]


resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_join, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

doublons <- data_join[duplicated(data_join$ID.interne.passage) |
                        duplicated(data_join$ID.interne.passage, fromLast = TRUE), ]

resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_join, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

doublons <- data_join[duplicated(data_join$ID.interne.passage) |
                        duplicated(data_join$ID.interne.passage, fromLast = TRUE), ]

resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_join, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

data_beta <- data_join

# Compute an info about there is a bloom or not
data_beta$EpBloom <- "OUI"
data_beta[is.na(data_beta$Bloom),8] <- "NON"

# Distinguish the end the bloom
for (i in 1:nrow(data_beta)){
  if (data_beta$EpBloom[i] == "OUI" & (data_beta$EpBloom[i+1] == "OUI" | data_beta$EpBloom[i+1] == "Sucession") ){
    data_beta$EpBloom[i] <- "Sucession"
  }
}

write.csv2(data_beta,file="data_modif/data_div_beta_ok_N1_final.csv", row.names = FALSE,dec = ".")
# On a comme ca les indices de bray-curtis associé à chaque date avec les informations de bloom s'il y a

#### Compare date N & N+1 ####

beta <- read_delim("data_modif/data_div_beta_ok_N1_final.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
                                                                        `Bray-Curtis` = col_number()), locale = locale(decimal_mark = ",", 
                                                                                                                       grouping_mark = "."), trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

data_bloom_beta <- left_join(beta,data_bloom)

# There are duplicates to delete
doublons <- data_bloom_beta[duplicated(data_bloom_beta$ID.interne.passage) |
                        duplicated(data_bloom_beta$ID.interne.passage, fromLast = TRUE), ]

resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_bloom_beta, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_bloom_beta <- bind_rows(data_unique,resultat_filtre)

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


# BETWEEN DATE N-1 & N+1 ####
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

# Keep only the counts
data_count <- data[,c(2,8,24:328)]
# Replace NA's to 0
data_count[is.na(data_count)] <- 0
data_hel <- decostand(data_count[,-c(1,2)], method = "hellinger")
data_hel <- bind_cols(data_count[,c(1,2)],data_hel)

# Create a df to store the results
data_results_beta <- c("","")
data_results_beta <- as.data.frame(data_results_beta)

i = 1
data_station <- filter(data_hel, Code_point_Libelle == levels(as.factor(data_hel$Code_point_Libelle))[i])
for (j in 2:(nrow(data_station)-1)){
  Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
  
  Date1 <- data_station[j-1,"Date"]$Date
  Date2 <- data_station[j+1,"Date"]$Date
  Com1 <- data_station[j-1,c(3:307)]
  rownames(Com1) <- Date1
  Com2 <- data_station[j+1,c(3:307)]
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
    Com1 <- data_station[j-1,c(3:307)]
    rownames(Com1) <- Date1
    Com2 <- data_station[j+1,c(3:307)]
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

write.csv2(data_results_beta,file="data_modif/data_div_betaN-1_N+1_final.csv", row.names = FALSE,dec = ".")

# Merge the results to the original data
# Import the results
beta <- read_delim("data_modif/data_div_betaN-1_N+1_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

# Import data
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)


colnames(beta)[4] <- "Date"

data <- dplyr::select(data, Code_point_Libelle,ID.interne.passage, cluster,Bloom,Date)

data_join <- left_join(data,beta)

# There are duplicates to delete

doublons <- data_join[duplicated(data_join$ID.interne.passage) |
                        duplicated(data_join$ID.interne.passage, fromLast = TRUE), ]


resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_join, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

doublons <- data_join[duplicated(data_join$ID.interne.passage) |
                        duplicated(data_join$ID.interne.passage, fromLast = TRUE), ]

resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_join, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

data_beta <- data_join

# Compute an info about there is a bloom or not
data_beta$EpBloom <- "OUI"
data_beta[is.na(data_beta$Bloom),8] <- "NON"

# Create a column to have only a bloom during one date
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

write.csv2(data_beta,file="data_modif/data_div_beta_ok_N-1_N+1_final.csv", row.names = FALSE,dec = ".")


#### Compare N-1 & N+1 ####
# Import data
beta <- read_delim("data_modif/data_div_beta_ok_N-1_N+1_final.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

data_bloom_beta <- left_join(beta,data_bloom)

# there are duplicates to delete
doublons <- data_bloom_beta[duplicated(data_bloom_beta$ID.interne.passage) |
                              duplicated(data_bloom_beta$ID.interne.passage, fromLast = TRUE), ]

resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_bloom_beta, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_bloom_beta <- bind_rows(data_unique,resultat_filtre)
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

# Compare before/during/after the bloom with a lag of 1 ####
# Import data
beta <- read_delim("data_modif/data_div_beta_ok_N-1_final.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

avant <- left_join(beta,data_bloom)

beta <- read_delim("data_modif/data_div_beta_ok_N1_final.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

apres <- left_join(beta,data_bloom)

beta <- read_delim("data_modif/data_div_beta_ok_N-1_N+1_final.csv", 
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
NN1 <- filter(NN1, EpBloom == "OUI") #176

avant$cat <- "N vs N-1"
apres$cat <- "N vs N+1"
NN1$cat <- "N-1 vs N+1"

data_comp <- rbind(avant,apres,NN1)

# Doing it for the Bacillariophyceae blooms

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

# Doing it for the Dinophyceae blooms

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

# BETWEEN DATE N ET N-2 ####

# Import data
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

# Keep the counts
data_count <- data[,c(2,8,24:328)]
# Replace NA's by 0
data_count[is.na(data_count)] <- 0
# Compute the hellinger transformation
data_hel <- decostand(data_count[,-c(1,2)], method = "hellinger")
data_hel <- bind_cols(data_count[,c(1,2)],data_hel)

# Create a df to store the result
data_results_beta <- c("","")
data_results_beta <- as.data.frame(data_results_beta)

i = 1
  data_station <- filter(data_hel, Code_point_Libelle == levels(as.factor(data_hel$Code_point_Libelle))[i])
  for (j in 3:nrow(data_station)){
  Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle

  Date1 <- data_station[j,"Date"]$Date
  Date2 <- data_station[j-2,"Date"]$Date
  Com1 <- data_station[j,c(3:307)]
  rownames(Com1) <- Date1
  Com2 <- data_station[j-2,c(3:307)]
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
        Com1 <- data_station[j,c(3:307)]
        rownames(Com1) <- Date1
        Com2 <- data_station[j-2,c(3:307)]
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
# Store the result
write.csv2(data_results_beta,file="data_modif/data_div_beta_N-2_final.csv", row.names = FALSE,dec = ".")

# Merge the result to the original data
# Import the result
beta <- read_delim("data_modif/data_div_beta_N-2_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

# Import data
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)


colnames(beta)[4] <- "Date"

data <- dplyr::select(data, Code_point_Libelle,ID.interne.passage, cluster,Bloom,Date)

data_join <- left_join(data,beta)

# There are duplicates to delete

doublons <- data_join[duplicated(data_join$ID.interne.passage) |
                        duplicated(data_join$ID.interne.passage, fromLast = TRUE), ]


resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_join, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

doublons <- data_join[duplicated(data_join$ID.interne.passage) |
                        duplicated(data_join$ID.interne.passage, fromLast = TRUE), ]

resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_join, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

doublons <- data_join[duplicated(data_join$ID.interne.passage) |
                        duplicated(data_join$ID.interne.passage, fromLast = TRUE), ]

resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_join, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

data_beta <- data_join

data_beta$EpBloom <- "OUI"
data_beta[is.na(data_beta$Bloom),8] <- "NON"

# Distinguish only the first date of the bloom
for (i in 3:nrow(data_beta)){
  if (data_beta$EpBloom[i] == "OUI" & (data_beta$EpBloom[i-2] == "OUI" | data_beta$EpBloom[i-2] == "Sucession" | data_beta$EpBloom[i-1] == "OUI" | data_beta$EpBloom[i-1] == "Sucession") ){
    data_beta$EpBloom[i] <- "Sucession"
  }
}

write.csv2(data_beta,file="data_modif/data_div_beta_ok_N-2_final.csv", row.names = FALSE,dec = ".")

#### Compare date N & N-2 ####
beta <- read_delim("data_modif/data_div_beta_ok_N-2_final.csv", 
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


# BETWEEN DATE N & N+2 ####
# Import data
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

# Keep only the counts
data_count <- data[,c(2,8,24:328)]
# Replace NA's by 0
data_count[is.na(data_count)] <- 0
# Compute the Hellinger transformation
data_hel <- decostand(data_count[,-c(1,2)], method = "hellinger")
data_hel <- bind_cols(data_count[,c(1,2)],data_hel)

# Create a df to store the result
data_results_beta <- c("","")
data_results_beta <- as.data.frame(data_results_beta)

i = 1
data_station <- filter(data_hel, Code_point_Libelle == levels(as.factor(data_hel$Code_point_Libelle))[i])
for (j in 1:(nrow(data_station)-2)){
  Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
  
  Date1 <- data_station[j,"Date"]$Date
  Date2 <- data_station[j+2,"Date"]$Date
  Com1 <- data_station[j,c(3:307)]
  rownames(Com1) <- Date1
  Com2 <- data_station[j+2,c(3:307)]
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
    Com1 <- data_station[j,c(3:307)]
    rownames(Com1) <- Date1
    Com2 <- data_station[j+2,c(3:307)]
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

write.csv2(data_results_beta,file="data_modif/data_div_betaN+2_final.csv", row.names = FALSE,dec = ".")

# Merge the result to the original data
# Import the result
beta <- read_delim("data_modif/data_div_betaN+2_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

# Import data
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)


colnames(beta)[4] <- "Date"

data <- dplyr::select(data, Code_point_Libelle,ID.interne.passage, cluster,Bloom,Date)

data_join <- left_join(data,beta)

# There are duplicates to delete

doublons <- data_join[duplicated(data_join$ID.interne.passage) |
                        duplicated(data_join$ID.interne.passage, fromLast = TRUE), ]


resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_join, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

doublons <- data_join[duplicated(data_join$ID.interne.passage) |
                        duplicated(data_join$ID.interne.passage, fromLast = TRUE), ]

resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_join, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

data_beta <- data_join

# Compute an info about there is a Bloom or not
data_beta$EpBloom <- "OUI"
data_beta[is.na(data_beta$Bloom),8] <- "NON"

# Dinstinguish the first date of the bloom
for (i in 1:nrow(data_beta)){
  if (data_beta$EpBloom[i] == "OUI" & (data_beta$EpBloom[i+2] == "OUI" | data_beta$EpBloom[i+2] == "Sucession" | data_beta$EpBloom[i+1] == "OUI" | data_beta$EpBloom[i+1] == "Sucession") ){
    data_beta$EpBloom[i] <- "Sucession"
  }
}

write.csv2(data_beta,file="data_modif/data_div_beta_ok_N2_final.csv", row.names = FALSE,dec = ".")

#### Compare N & N+2 ####

beta <- read_delim("data_modif/data_div_beta_ok_N2_final.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
                                                                        `Bray-Curtis` = col_number()), locale = locale(decimal_mark = ",", 
                                                                                                                       grouping_mark = "."), trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

data_bloom_beta <- left_join(beta,data_bloom)

# There are duplicates to delete
doublons <- data_bloom_beta[duplicated(data_bloom_beta$ID.interne.passage) |
                        duplicated(data_bloom_beta$ID.interne.passage, fromLast = TRUE), ]

resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_bloom_beta, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_bloom_beta <- bind_rows(data_unique,resultat_filtre)
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

# BETWEEN DATE N-2 & N+2 ####
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)
# keep only the counts
data_count <- data[,c(2,8,24:328)]
# Replace NA's by 0
data_count[is.na(data_count)] <- 0
# Compute the Hellinger transformation
data_hel <- decostand(data_count[,-c(1,2)], method = "hellinger")
data_hel <- bind_cols(data_count[,c(1,2)],data_hel)

# Create a df to store the results
data_results_beta <- c("","")
data_results_beta <- as.data.frame(data_results_beta)

i = 1
data_station <- filter(data_hel, Code_point_Libelle == levels(as.factor(data_hel$Code_point_Libelle))[i])
for (j in 3:(nrow(data_station)-2)){
  Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
  
  Date1 <- data_station[j-2,"Date"]$Date
  Date2 <- data_station[j+2,"Date"]$Date
  Com1 <- data_station[j-2,c(3:307)]
  rownames(Com1) <- Date1
  Com2 <- data_station[j+2,c(3:307)]
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
    Com1 <- data_station[j-2,c(3:307)]
    rownames(Com1) <- Date1
    Com2 <- data_station[j+2,c(3:307)]
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
# Store the result
write.csv2(data_results_beta,file="data_modif/data_div_betaN-2_N+2_final.csv", row.names = FALSE,dec = ".")

# Merge the result to the original data
# Import the result
beta <- read_delim("data_modif/data_div_betaN-2_N+2_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

# Import data
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)


colnames(beta)[4] <- "Date"

data <- dplyr::select(data, Code_point_Libelle,ID.interne.passage, cluster,Bloom,Date)

data_join <- left_join(data,beta)

# There are duplicates to delete

doublons <- data_join[duplicated(data_join$ID.interne.passage) |
                        duplicated(data_join$ID.interne.passage, fromLast = TRUE), ]


resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_join, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

doublons <- data_join[duplicated(data_join$ID.interne.passage) |
                        duplicated(data_join$ID.interne.passage, fromLast = TRUE), ]

resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_join, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

data_beta <- data_join

# Compute an info about there is a Bloom or not
data_beta$EpBloom <- "OUI"
data_beta[is.na(data_beta$Bloom),8] <- "NON"

# Dinstinguish the bloom of 1 date
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

write.csv2(data_beta,file="data_modif/data_div_beta_ok_N-2_N+2_final.csv", row.names = FALSE,dec = ".")


#### Compare N-2 & N+2 ####
beta <- read_delim("data_modif/data_div_beta_ok_N-2_N+2_final.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

data_bloom_beta <- left_join(beta,data_bloom)

# There are duplicates to delete
doublons <- data_bloom_beta[duplicated(data_bloom_beta$ID.interne.passage) |
                              duplicated(data_bloom_beta$ID.interne.passage, fromLast = TRUE), ]

resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_bloom_beta, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_bloom_beta <- bind_rows(data_unique,resultat_filtre)
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

# Compare before/during/after the bloom with a lag of 2 ####
# Import data
beta <- read_delim("data_modif/data_div_beta_ok_N-2_final.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

avant <- left_join(beta,data_bloom)

beta <- read_delim("data_modif/data_div_beta_ok_N2_final.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

apres <- left_join(beta,data_bloom)

beta <- read_delim("data_modif/data_div_beta_ok_N-2_N+2_final.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

NN1 <- left_join(beta,data_bloom)

avant <- filter(avant, EpBloom == "OUI") #205
apres <- filter(apres, EpBloom == "OUI") #205
NN1 <- filter(NN1, EpBloom == "OUI") #150

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

# Doing it for the Bacillariophyceae blooms

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

# Doing it for the dinophyceae blooms
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


# BETWEEN DATE N & N-3 ####
# Load data
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)
# Keep only the counts
data_count <- data[,c(2,8,24:328)]
# Replace NA's by 0
data_count[is.na(data_count)] <- 0
# Compute the Hellinger transformation
data_hel <- decostand(data_count[,-c(1,2)], method = "hellinger")
data_hel <- bind_cols(data_count[,c(1,2)],data_hel)

# Create a df to store the result
data_results_beta <- c("","")
data_results_beta <- as.data.frame(data_results_beta)

i = 1
data_station <- filter(data_hel, Code_point_Libelle == levels(as.factor(data_hel$Code_point_Libelle))[i])
for (j in 4:nrow(data_station)){
  Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
  
  Date1 <- data_station[j,"Date"]$Date
  Date2 <- data_station[j-3,"Date"]$Date
  Com1 <- data_station[j,c(3:307)]
  rownames(Com1) <- Date1
  Com2 <- data_station[j-3,c(3:307)]
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
    Com1 <- data_station[j,c(3:307)]
    rownames(Com1) <- Date1
    Com2 <- data_station[j-3,c(3:307)]
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
write.csv2(data_results_beta,file="data_modif/data_div_beta_N-3_final.csv", row.names = FALSE,dec = ".")

# Merge the result to the original data
# Import the result
beta <- read_delim("data_modif/data_div_beta_N-3_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

# Import data
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)


colnames(beta)[4] <- "Date"

data <- dplyr::select(data, Code_point_Libelle,ID.interne.passage, cluster,Bloom,Date)

data_join <- left_join(data,beta)

# There are duplicates to delete

doublons <- data_join[duplicated(data_join$ID.interne.passage) |
                        duplicated(data_join$ID.interne.passage, fromLast = TRUE), ]


resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_join, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

doublons <- data_join[duplicated(data_join$ID.interne.passage) |
                        duplicated(data_join$ID.interne.passage, fromLast = TRUE), ]

resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_join, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

doublons <- data_join[duplicated(data_join$ID.interne.passage) |
                        duplicated(data_join$ID.interne.passage, fromLast = TRUE), ]


resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_join, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)


data_beta <- data_join

# Compute an info about there is a Bloom or not
data_beta$EpBloom <- "OUI"
data_beta[is.na(data_beta$Bloom),8] <- "NON"

# Distinguish the first date of the bloom
for (i in 4:nrow(data_beta)){
  if (data_beta$EpBloom[i] == "OUI" & (data_beta$EpBloom[i-2] == "OUI" | data_beta$EpBloom[i-2] == "Sucession" | data_beta$EpBloom[i-1] == "OUI" | data_beta$EpBloom[i-1] == "Sucession" | data_beta$EpBloom[i-3] == "OUI" | data_beta$EpBloom[i-3] == "Sucession") ){
    data_beta$EpBloom[i] <- "Sucession"
  }
}

write.csv2(data_beta,file="data_modif/data_div_beta_ok_N-3_final.csv", row.names = FALSE,dec = ".")

#### Compare N et N-3 ####
# Import data
beta <- read_delim("data_modif/data_div_beta_ok_N-3_final.csv", 
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


# BETWEEN DATE N ET N+3 ####
# Load data
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)
# Keep only the counts
data_count <- data[,c(2,8,24:328)]
# Replace NA's by 0
data_count[is.na(data_count)] <- 0
# Compute the Hellinger transformation
data_hel <- decostand(data_count[,-c(1,2)], method = "hellinger")
data_hel <- bind_cols(data_count[,c(1,2)],data_hel)

# Create a df to store the result
data_results_beta <- c("","")
data_results_beta <- as.data.frame(data_results_beta)


i = 1
data_station <- filter(data_hel, Code_point_Libelle == levels(as.factor(data_hel$Code_point_Libelle))[i])
for (j in 1:(nrow(data_station)-3)){
  Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
  
  Date1 <- data_station[j,"Date"]$Date
  Date2 <- data_station[j+3,"Date"]$Date
  Com1 <- data_station[j,c(3:307)]
  rownames(Com1) <- Date1
  Com2 <- data_station[j+3,c(3:307)]
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
    Com1 <- data_station[j,c(3:307)]
    rownames(Com1) <- Date1
    Com2 <- data_station[j+3,c(3:307)]
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

write.csv2(data_results_beta,file="data_modif/data_div_betaN+3_final.csv", row.names = FALSE,dec = ".")

# Merge the result to the original data
# Import the result
beta <- read_delim("data_modif/data_div_betaN+3_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

# Import data
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)


colnames(beta)[4] <- "Date"

data <- dplyr::select(data, Code_point_Libelle,ID.interne.passage, cluster,Bloom,Date)

data_join <- left_join(data,beta)

# There are duplicates to delete

doublons <- data_join[duplicated(data_join$ID.interne.passage) |
                        duplicated(data_join$ID.interne.passage, fromLast = TRUE), ]


resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_join, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

doublons <- data_join[duplicated(data_join$ID.interne.passage) |
                        duplicated(data_join$ID.interne.passage, fromLast = TRUE), ]

resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_join, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

data_beta <- data_join

# Compute an info about there is a Bloom or not
data_beta$EpBloom <- "OUI"
data_beta[is.na(data_beta$Bloom),8] <- "NON"

# Distinguish the end of the bloom
for (i in 1:nrow(data_beta)){
  if (data_beta$EpBloom[i] == "OUI" & (data_beta$EpBloom[i+2] == "OUI" | data_beta$EpBloom[i+2] == "Sucession" | data_beta$EpBloom[i+1] == "OUI" | data_beta$EpBloom[i+1] == "Sucession"
                                       | data_beta$EpBloom[i+3] == "OUI" | data_beta$EpBloom[i+3] == "Sucession") ){
    data_beta$EpBloom[i] <- "Sucession"
  }
}

# Save the result
write.csv2(data_beta,file="data_modif/data_div_beta_ok_N3_final.csv", row.names = FALSE,dec = ".")

#### Compare N & N+3 ####
# Load data
beta <- read_delim("data_modif/data_div_beta_ok_N3_final.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
                                                                        `Bray-Curtis` = col_number()), locale = locale(decimal_mark = ",", 
                                                                                                                       grouping_mark = "."), trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

data_bloom_beta <- left_join(beta,data_bloom)

# There are duplicates to delete

doublons <- data_bloom_beta[duplicated(data_bloom_beta$ID.interne.passage) |
                              duplicated(data_bloom_beta$ID.interne.passage, fromLast = TRUE), ]

resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_bloom_beta, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_bloom_beta <- bind_rows(data_unique,resultat_filtre)
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

#BETWEEN DATE N-3 & N+3 ####
# Load data
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)
# Keep only the counts
data_count <- data[,c(2,8,24:328)]
# Replace NA's by 0
data_count[is.na(data_count)] <- 0
# Compute the Hellinger transformation
data_hel <- decostand(data_count[,-c(1,2)], method = "hellinger")
data_hel <- bind_cols(data_count[,c(1,2)],data_hel)

# Create a df to store the result
data_results_beta <- c("","")
data_results_beta <- as.data.frame(data_results_beta)
i = 1
data_station <- filter(data_hel, Code_point_Libelle == levels(as.factor(data_hel$Code_point_Libelle))[i])
for (j in 4:(nrow(data_station)-3)){
  Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
  
  Date1 <- data_station[j-3,"Date"]$Date
  Date2 <- data_station[j+3,"Date"]$Date
  Com1 <- data_station[j-3,c(3:307)]
  rownames(Com1) <- Date1
  Com2 <- data_station[j+3,c(3:307)]
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
    Com1 <- data_station[j-3,c(3:307)]
    rownames(Com1) <- Date1
    Com2 <- data_station[j+3,c(3:307)]
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

write.csv2(data_results_beta,file="data_modif/data_div_betaN-3_N+3_final.csv", row.names = FALSE,dec = ".")

# Merge the result to the original data
# Import the result
beta <- read_delim("data_modif/data_div_betaN+3_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

# Import data
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)


colnames(beta)[4] <- "Date"

data <- dplyr::select(data, Code_point_Libelle,ID.interne.passage, cluster,Bloom,Date)

data_join <- left_join(data,beta)

# There are duplicates to delete

doublons <- data_join[duplicated(data_join$ID.interne.passage) |
                        duplicated(data_join$ID.interne.passage, fromLast = TRUE), ]


resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_join, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

data_beta <- data_join

# Compute an info about there is a Bloom or not
data_beta$EpBloom <- "OUI"
data_beta[is.na(data_beta$Bloom),8] <- "NON"

# Dinstiguish only bloom of 1 date
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

write.csv2(data_beta,file="data_modif/data_div_beta_ok_N-3_N+3_final.csv", row.names = FALSE,dec = ".")


#### Compare N-3 & N+3 ####
# Import data
beta <- read_delim("data_modif/data_div_beta_ok_N-3_N+3_final.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

data_bloom_beta <- left_join(beta,data_bloom)

# There are duplicates to delete
doublons <- data_bloom_beta[duplicated(data_bloom_beta$ID.interne.passage) |
                              duplicated(data_bloom_beta$ID.interne.passage, fromLast = TRUE), ]

resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_bloom_beta, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_bloom_beta <- bind_rows(data_unique,resultat_filtre)
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

# Compare before/during/after the bloom with a lag of 3 ####
beta <- read_delim("data_modif/data_div_beta_ok_N-3_final.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

avant <- left_join(beta,data_bloom)

beta <- read_delim("data_modif/data_div_beta_ok_N3_final.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

apres <- left_join(beta,data_bloom)

beta <- read_delim("data_modif/data_div_beta_ok_N-3_N+3_final.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

NN1 <- left_join(beta,data_bloom)

avant <- filter(avant, EpBloom == "OUI") #189
apres <- filter(apres, EpBloom == "OUI") #189
NN1 <- filter(NN1, EpBloom == "OUI") #132

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

# Doing it for the Bacillariophyceae blooms
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

# Doing it for the Dinophyceae blooms
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


#  BETA DIVERSITY BETWEEN MOUNTH OF EACH YEAR #####
# Load data
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

# Keep only counts
data_count <- data[,c(2,3,6,7,24:328)]
# Replace NA's by 0
data_count[is.na(data_count)] <- 0

# Compute the mean of the abundance by year and mounth
data_count <- data_count %>%
  group_by(Code_point_Libelle, Year,Month) %>%
  summarise(across(everything(), mean, na.rm = TRUE))


# Compute the Hellinger transformation
data_hel <- decostand(data_count[,-c(1:4)], method = "hellinger")
data_hel <- bind_cols(data_count[,c(1:4)],data_hel)

# Create a df to store the result
data_results_beta <- c("","")
data_results_beta <- as.data.frame(data_results_beta)

i = 1
data_station <- filter(data_hel, Code_point_Libelle == levels(as.factor(data_hel$Code_point_Libelle))[i])
for (j in 1:(nrow(data_station)-1)){
  Station <- data_station[j,"Code_point_Libelle"]$Code_point_Libelle
  
  Date1 <- data_station[j,"Month"]$Month
  Date2 <- data_station[j+1,"Month"]$Month
  Com1 <- data_station[j,c(5:309)]
  rownames(Com1) <- Date1
  Com2 <- data_station[j+1,c(5:309)]
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
    Com1 <- data_station[j,c(5:309)]
    rownames(Com1) <- Date1
    Com2 <- data_station[j+1,c(5:309)]
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
# Save the result
write.csv2(data_results_beta,file="data_modif/data_div_beta_ok_Mois_final.csv", row.names = FALSE,dec = ".")

# Make a graph to see the seasonnality
# Import data
data_results_beta <- read_delim("data_modif/data_div_beta_ok_Mois_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)
data_results_beta$Lag <- as.factor(data_results_beta$Lag)
levels(data_results_beta$Lag) <- c("1-2"  ,"2-3"  , "3-4" ,  "4-5" ,  "5-6" ,  "6-7" ,  "7-8"  , "8-9"  , "9-10","10-11" ,"11-12")
cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

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
  facet_wrap(~cluster,ncol=4)+
  theme(axis.text.x = element_text(angle = 90, vjust =0.5, hjust = 1, size = 7))

ggplot(data_results_beta)+
  geom_boxplot(aes(x=cluster,y=`Bray-Curtis`,group=cluster,fill=as.character(cluster)))+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits =c(0,1.3))
  #facet_wrap(~Lag)+
  #theme(axis.text.x = element_text(angle = 90, vjust =0.5, hjust = 1, size = 7))

data_kw <- filter(data_results_beta)
kruskal.test(data_kw$`Bray-Curtis`~data_kw$cluster)
DunnTest(data_kw$`Bray-Curtis`~data_kw$cluster,method="BH")
