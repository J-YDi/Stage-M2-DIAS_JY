# Script JY.Dias - Stage M2 #

# Load packages
library(readr)
library(dplyr)
library(ggplot2)
library(trend)
library(lubridate)
library(cowplot)

##########################################
# Table_bloom_R_v3c.csv was made by hand #
##########################################
# Load data
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                                  delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                                  trim_ws = TRUE)


data_bloom$cluster <- as.factor(data_bloom$cluster)
cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

# Distinguish monospecific blooms from others ####
data_bloom$Monodominance <- "NON"
data_bloom$Monodominance <- ifelse(data_bloom$P_dominance >= median(data_bloom$P_dominance), "OUI",data_bloom$Monodominance) 

write.csv2(data_bloom,file="data_modif/Table_bloom_VF_final.csv", row.names = FALSE,dec = ".")

# Difference on percentage of dominance during between bloom and no bloom ####

data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

data <- dplyr::select(data, Code_point_Libelle,cluster, Date,Month, BergerParker)

data_bloom <- dplyr::select(data_bloom, -cluster,-BergerParker,-Month)

data_combo <- left_join(data,data_bloom, by = join_by(Code_point_Libelle, Date))

data_combo <- data_combo |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))
set.seed(123)
nbhiverclus1 <- sample_n(filter(data_combo,season == "Winter" & cluster == 1 & is.na(Bloom_Phylum)),21)
nbhiverclus2 <- sample_n(filter(data_combo,season == "Winter" & cluster == 2 & is.na(Bloom_Phylum)),3)
nbhiverclus3 <- sample_n(filter(data_combo,season == "Winter" & cluster == 3 & is.na(Bloom_Phylum)),3)

nbautomneclus1 <- sample_n(filter(data_combo,season == "Fall" & cluster == 1 & is.na(Bloom_Phylum)),28)
nbautomneclus2 <- sample_n(filter(data_combo,season == "Fall" & cluster == 2 & is.na(Bloom_Phylum)),9)
nbautomneclus3 <- sample_n(filter(data_combo,season == "Fall" & cluster == 3 & is.na(Bloom_Phylum)),11)

nbspringclus1 <- sample_n(filter(data_combo,season == "Spring" & cluster == 1 & is.na(Bloom_Phylum)),29)
nbspringclus2 <- sample_n(filter(data_combo,season == "Spring" & cluster == 2 & is.na(Bloom_Phylum)),49)
nbspringclus3 <- sample_n(filter(data_combo,season == "Spring" & cluster == 3 & is.na(Bloom_Phylum)),61)

nbeteclus1 <- sample_n(filter(data_combo,season == "Summer" & cluster == 1 & is.na(Bloom_Phylum)),22)
nbeteclus2 <- sample_n(filter(data_combo,season == "Summer" & cluster == 2 & is.na(Bloom_Phylum)),39)
nbeteclus3 <- sample_n(filter(data_combo,season == "Summer" & cluster == 3 & is.na(Bloom_Phylum)),25)

data_hasard <- bind_rows(nbhiverclus1,nbhiverclus2,nbhiverclus3,nbautomneclus1,nbautomneclus2,nbautomneclus3,nbspringclus1,nbspringclus2,nbspringclus3,
                         nbeteclus1,nbeteclus2,nbeteclus3)

data_bloom <- filter(data_combo, !is.na(Bloom_Phylum))

data_combo <- bind_rows(data_bloom,data_hasard)

data_combo$TBloom <- ifelse(is.na(data_combo$Bloom),"Non-bloom","Bloom")

data_combo$cluster[data_combo$cluster == "1"] <- "1-Méditerranée"
data_combo$cluster[data_combo$cluster == "2"] <- "2-Manche orientale - Mer du Nord"
data_combo$cluster[data_combo$cluster == "3"] <- "3-Atlantique - Manche occidentale"
data_combo$cluster[data_combo$cluster == "4"] <- "4-Mer des Pertuis"
data_combo$cluster <- as.factor(data_combo$cluster)

# Color for each cluster
cluster_col <- c("1-Méditerranée" = "#F8766D","2-Manche orientale - Mer du Nord" = "#CD9600", 
                 "3-Atlantique - Manche occidentale" = "#00BE67",  "4-Mer des Pertuis" = "#00A9FF")

BKglobal <- ggplot(filter(data_combo, cluster != "4-Mer des Pertuis"))+
  geom_boxplot(aes(x=TBloom,y=BergerParker,group=TBloom))+
  labs(x="Evènement",y="Indice de Berger-Parker")

BKregion <- ggplot(filter(data_combo, cluster !="4-Mer des Pertuis"))+
  geom_boxplot(aes(x=TBloom,y=BergerParker,group=TBloom,fill=as.character(cluster)))+
  labs(x="Evènement",y="Indice de Berger-Parker")+
  scale_fill_manual(values=cluster_col,guide = "none")+
  facet_wrap(~cluster)

data_combo$Bloom_Phylum[is.na(data_combo$Bloom_Phylum)] <- "Non-bloom"


BKdetail <- ggplot(filter(data_combo, cluster != "4-Mer des Pertuis", Bloom_Phylum == "Bac" | Bloom_Phylum == "Dino" | Bloom_Phylum == "Non-bloom"))+
  geom_boxplot(aes(x=Bloom_Phylum,y=BergerParker,group=Bloom_Phylum,fill=as.character(cluster)))+
  labs(x="Evènement",y="Indice de Berger-Parker")+
  scale_fill_manual(values=cluster_col,guide = "none")+
  facet_wrap(~cluster,scales = "free_x")

plot_grid(BKregion,BKdetail,ncol=1,labels = "AUTO")

wilcox.test(filter(data_combo,TBloom == "Bloom")$BergerParker,filter(data_combo,TBloom != "Bloom")$BergerParker)
wilcox.test(filter(data_combo,Bloom_Phylum == "Dino" & cluster == "1-Méditerranée")$BergerParker,filter(data_combo,TBloom != "Bloom" & cluster == "1-Méditerranée")$BergerParker)
wilcox.test(filter(data_combo,Bloom_Phylum == "Dino" & cluster == "2-Manche orientale - Mer du Nord")$BergerParker,filter(data_combo,TBloom != "Bloom" & cluster == "2-Manche orientale - Mer du Nord")$BergerParker)
wilcox.test(filter(data_combo,Bloom_Phylum == "Dino" & cluster == "3-Atlantique - Manche occidentale")$BergerParker,filter(data_combo,TBloom != "Bloom" & cluster == "3-Atlantique - Manche occidentale")$BergerParker)

