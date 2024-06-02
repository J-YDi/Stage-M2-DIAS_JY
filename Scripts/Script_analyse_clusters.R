# Script JY.Dias - Stage M2 #

# Load packages 
library(ggplot2)
library(readr)
library(dplyr)
library(ggthemes)
library(tidyr)
library(cowplot)
library(DescTools)

# Import data
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)


data$cluster[data$cluster == "1"] <- "1-Méditerranée"
data$cluster[data$cluster == "2"] <- "2-Manche orientale - Mer du Nord"
data$cluster[data$cluster == "3"] <- "3-Atlantique - Manche occidentale"
data$cluster[data$cluster == "4"] <- "4-Mer des Pertuis"
data$cluster <- as.factor(data$cluster)

# Color for each cluster
cluster_col <- c("1-Méditerranée" = "#F8766D","2-Manche orientale - Mer du Nord" = "#CD9600", 
                 "3-Atlantique - Manche occidentale" = "#00BE67",  "4-Mer des Pertuis" = "#00A9FF")

data <- filter(data, cluster != "4-Mer des Pertuis")
#### Graph to show saisonality ####

# Prepare data for graphs
data$`log(CHLOROA+1)` <- log(data$CHLOROA +1)
data$`Dino/Bac` <- data$Dinophyceae/data$Bacillariophyceae

dataforbox_hydro <- pivot_longer(data, names_to = "Variable",cols = c(SALI:`TURB-FNU`,`log(CHLOROA+1)`))
dataforbox_hydro <- dplyr::select(dataforbox_hydro,Code.Region:Code.parametre,Variable,value)
dataforbox_hydro <- filter(dataforbox_hydro,Variable != "TURB")

dataforbox_phyto <- pivot_longer(data, names_to = "Variable",cols = c(Bacillariophyceae,Dinophyceae,Shannon,Pielou,BergerParker,Rspe,`Dino/Bac`))
dataforbox_phyto <- dplyr::select(dataforbox_phyto,Code.Region:Code.parametre,Variable,value)

# Make one to one plot to agglomerate them after
TEMP <- filter(dataforbox_hydro, Variable == "TEMP")
TEMP$Variable <- "Température (°C)"
temp <- ggplot(TEMP)+
  geom_boxplot(aes(y=value,x=Month,group = Month,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(x = "Mois", y = "Mesure", colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,100, by = 5),limits = c(0,100))+
  scale_x_continuous(breaks = c(1:12))+
  facet_wrap(cluster ~ Variable, nrow = 4, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 7))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 10),
        strip.background = element_rect(fill = NULL))

SALI <- filter(dataforbox_hydro, Variable == "SALI")
SALI$Variable <- "Salinité (PSU)"
sali <- ggplot(SALI)+
  geom_boxplot(aes(y=value,x=Month,group = Month,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(x = "Mois", y = NULL,colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,100, by = 5),limits = c(0,100))+
  scale_x_continuous(breaks = c(1:12))+
  facet_wrap(cluster ~ Variable, nrow = 4, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 7))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 10),
        strip.background = element_rect(fill = NULL))
TURB <- filter(dataforbox_hydro, Variable == "TURB-FNU")
TURB$Variable <- "Turbidité (FNU)"
turb <- ggplot(TURB)+
  geom_boxplot(aes(y=value,x=Month,group = Month,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(x = "Mois", y = NULL,colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,90, by = 30),limits = c(0,90))+
  scale_x_continuous(breaks = c(1:12))+
  facet_wrap(cluster ~ Variable, nrow = 4, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 7))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 10),
        strip.background = element_rect(fill = NULL))

NH4 <- filter(dataforbox_hydro, Variable == "NH4")
NH4$Variable <- "NH4 (mg/L)"
NH4 <- ggplot(NH4)+
  geom_boxplot(aes(y=value,x=Month,group = Month,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(x = "Mois", y = NULL,colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,100, by = 5),limits = c(0,100))+
  scale_x_continuous(breaks = c(1:12))+
  facet_wrap(cluster ~ Variable, nrow = 4, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 7))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 10),
        strip.background = element_rect(fill = NULL))

NO3 <- filter(dataforbox_hydro, Variable == "NO3+NO2")
NO3$Variable <- "NO3+NO2 (mg/L)"
NO3NO2 <- ggplot(NO3)+
  geom_boxplot(aes(y=value,x=Month,group = Month,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(x = "Mois", y = NULL,colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,100, by = 5),limits = c(0,100))+
  scale_x_continuous(breaks = c(1:12))+
  facet_wrap(cluster ~ Variable, nrow = 4, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 7))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 10),
        strip.background = element_rect(fill = NULL))

PO4 <- filter(dataforbox_hydro, Variable == "PO4")
PO4$Variable <- "PO4 (mg/L)"
PO4 <- ggplot(PO4)+
  geom_boxplot(aes(y=value,x=Month,group = Month,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(x = "Mois", y = NULL,colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,100, by = 5),limits = c(0,100))+
  scale_x_continuous(breaks = c(1:12))+
  facet_wrap(cluster ~ Variable, nrow = 4, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 7))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 10),
        strip.background = element_rect(fill = NULL))

SIOH <- filter(dataforbox_hydro, Variable == "SIOH")
SIOH$Variable <- "SIOH (mg/L)"
SIOH <- ggplot(SIOH)+
  geom_boxplot(aes(y=value,x=Month,group = Month,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(x = "Mois", y = NULL,colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,100, by = 5),limits = c(0,100))+
  scale_x_continuous(breaks = c(1:12))+
  facet_wrap(cluster ~ Variable, nrow = 4, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 7))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 10),
        strip.background = element_rect(fill = NULL))

OXY <- filter(dataforbox_hydro, Variable == "OXYGENE")
OXY$Variable <- "Oxygene (mg/L)"
OXYGENE <- ggplot(OXY)+
  geom_boxplot(aes(y=value,x=Month,group = Month,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(x = "Mois", y = NULL,colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,100, by = 5),limits = c(0,100))+
  scale_x_continuous(breaks = c(1:12))+
  facet_wrap(cluster ~ Variable, nrow = 4, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 7))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 10),
        strip.background = element_rect(fill = NULL))

CHLORO <- filter(dataforbox_hydro, Variable == "log(CHLOROA+1)")
CHLORO$Variable <- "log(Chlorophylle a +1)"
CHLOROA <- ggplot(CHLORO)+
  geom_boxplot(aes(y=value,x=Month,group = Month,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(x = "Mois", y = NULL,colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,100, by = 5),limits = c(0,100))+
  scale_x_continuous(breaks = c(1:12))+
  facet_wrap(cluster ~ Variable, nrow = 4, scales="free_x")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 7))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 10),
        strip.background = element_rect(fill = NULL))

plot_grid(temp , sali , NH4 , NO3NO2 , PO4 , SIOH , turb , OXYGENE , CHLOROA, ncol = 9)
#ggsave('Hydro_Mois_lgtermcluster_final_V2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 500, width = 740, height = 426, units = 'mm',limitsize = F)

# Doing the same with some phyla
Bac <- filter(dataforbox_phyto, Variable == "Bacillariophyceae")
Bac$Variable <- "log(Bacillariophyceae +1)"
Bacilla <- ggplot(Bac)+
  geom_boxplot(aes(y=log(value+1),x=Month,group = Month,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(x = "Mois", y = "Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,17, by = 3),limits = c(0,17))+
  scale_x_continuous(breaks = c(1:12))+
  facet_wrap(cluster ~ Variable, nrow = 4)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 7))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 10),
        strip.background = element_rect(fill = NULL))
Dino <- filter(dataforbox_phyto, Variable == "Dinophyceae")
Dino$Variable <- "log(Dinophyceae +1)"
Dino <- ggplot(Dino)+
  geom_boxplot(aes(y=log(value+1),x=Month,group = Month,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(x = "Mois", y = NULL,colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,17, by = 3),limits = c(0,17))+
  scale_x_continuous(breaks = c(1:12))+
  facet_wrap(cluster ~ Variable, nrow = 4)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 7))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 10),
        strip.background = element_rect(fill = NULL))


Sha <- filter(dataforbox_phyto, Variable == "Shannon")
Sha$Variable <- "Indice de Shannon"
shannon <- ggplot(Sha)+
  geom_boxplot(aes(y=value,x=Month,group = Month,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(x = "Mois", y = NULL,colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  #scale_y_continuous(breaks = seq(0,17, by = 3),limits = c(0,17))+
  scale_x_continuous(breaks = c(1:12))+
  facet_wrap(cluster ~ Variable, nrow = 4)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 7))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 10),
        strip.background = element_rect(fill = NULL))

Piel <- filter(dataforbox_phyto, Variable == "Pielou")
Piel$Variable <- "Indice de Pielou"
pielou <- ggplot(Piel)+
  geom_boxplot(aes(y=value,x=Month,group = Month,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(x = "Mois", y = NULL,colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1, by = 0.2),limits = c(0,1))+
  scale_x_continuous(breaks = c(1:12))+
  facet_wrap(cluster ~ Variable, nrow = 4)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 7))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 10),
        strip.background = element_rect(fill = NULL))
BK <- filter(dataforbox_phyto, Variable == "BergerParker")
BK$Variable <- "Indice de Berger-Parker"
bergerparker <- ggplot(BK)+
  geom_boxplot(aes(y=value,x=Month,group = Month,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(x = "Mois", y = NULL,colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1, by = 0.2),limits = c(0,1))+
  scale_x_continuous(breaks = c(1:12))+
  facet_wrap(cluster ~ Variable, nrow = 4)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 7))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 10),
        strip.background = element_rect(fill = NULL))

Rspe <- filter(dataforbox_phyto, Variable == "Rspe")
Rspe$Variable <- "Richesse spécifique"
Rspe <- 
  ggplot(Rspe)+
  geom_boxplot(aes(y=value,x=Month,group = Month,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(x = "Mois", y = "
  
  
  
       Valeur",colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,50, by = 10),limits = c(0,50))+
  scale_x_continuous(breaks = c(1:12))+
  facet_wrap(cluster ~ Variable, nrow = 4)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 7))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 10),
        strip.background = element_rect(fill = NULL))

DB <- filter(dataforbox_phyto, Variable == "Dino/Bac")
DB$Variable <- "log(Dinophyceae/Bacillariophyceae+1)"
BacDino <- 
  ggplot(DB)+
  geom_boxplot(aes(y=log(value+1),x=Month,group = Month,fill= cluster,fill=cluster),linewidth = 0.5)+
  labs(x = "Mois", y = NULL,colour="Station")+
  scale_colour_discrete(guide= "none")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,1.5, by = 0.5),limits = c(0,1.5))+
  scale_x_continuous(breaks = c(1:12))+
  facet_wrap(cluster ~ Variable, nrow = 4)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1, size = 7))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 10),
        strip.background = element_rect(fill = NULL))

plot_grid(Bacilla, Dino,Rspe,shannon , pielou , bergerparker, ncol =6)
#ggsave('Div_Mois_lgtermcluster_final_V3.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/cluster_description",dpi = 600, width = 400, height = 380, units = 'mm')


### Difference between cluster #####
kruskal.test(data$TEMP~data$cluster)
DunnTest(data$TEMP~data$cluster,method="BH") 

kruskal.test(data$SALI~data$cluster)
DunnTest(data$SALI~data$cluster,method="BH") 

kruskal.test(data$NH4~data$cluster)
DunnTest(data$NH4~data$cluster,method="BH") 

kruskal.test(data$PO4~data$cluster)
DunnTest(data$PO4~data$cluster,method="BH") 

kruskal.test(data$SIOH~data$cluster)
DunnTest(data$SIOH~data$cluster,method="BH") 

kruskal.test(data$OXYGENE~data$cluster)
DunnTest(data$OXYGENE~data$cluster,method="BH") 

kruskal.test(data$CHLOROA~data$cluster)
DunnTest(data$CHLOROA~data$cluster,method="BH") 

kruskal.test(data$`NO3+NO2`~data$cluster)
DunnTest(data$`NO3+NO2`~data$cluster,method="BH") 

kruskal.test(data$`TURB-FNU`~data$cluster)
DunnTest(data$`TURB-FNU`~data$cluster,method="BH") 

kruskal.test(data$Bacillariophyceae~data$cluster)
DunnTest(data$Bacillariophyceae~data$cluster,method="BH") 

kruskal.test(data$Dinophyceae~data$cluster)
DunnTest(data$Dinophyceae~data$cluster,method="BH") 

kruskal.test(data$Ciliophora~data$cluster)
DunnTest(data$Ciliophora~data$cluster,method="BH") 

kruskal.test(data$Haptophyta~data$cluster)
DunnTest(data$Haptophyta~data$cluster,method="BH") 

kruskal.test(data$Cryptophyceae~data$cluster)
DunnTest(data$Cryptophyceae~data$cluster,method="BH") 

kruskal.test(data$Shannon~data$cluster)
DunnTest(data$Shannon~data$cluster,method="BH") 

kruskal.test(data$Pielou~data$cluster)
DunnTest(data$Pielou~data$cluster,method="BH") 

kruskal.test(data$BergerParker~data$cluster)
DunnTest(data$BergerParker~data$cluster,method="BH") 

kruskal.test(data$Rspe~data$cluster)
DunnTest(data$Rspe~data$cluster,method="BH") 

kruskal.test(data$`Dino/Bac`~data$cluster)
DunnTest(data$`Dino/Bac`~data$cluster,method="BH")

### Difference between cluster by season #####

data <- data |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))

# Fall
data_test <- filter(data, season == "Fall")

kruskal.test(data_test$TEMP~data_test$cluster)
DunnTest(data_test$TEMP~data_test$cluster,method="BH") 

kruskal.test(data_test$SALI~data_test$cluster)
DunnTest(data_test$SALI~data_test$cluster,method="BH") 

kruskal.test(data_test$NH4~data_test$cluster)
DunnTest(data_test$NH4~data_test$cluster,method="BH") 

kruskal.test(data_test$PO4~data_test$cluster)
DunnTest(data_test$PO4~data_test$cluster,method="BH") 

kruskal.test(data_test$SIOH~data_test$cluster)
DunnTest(data_test$SIOH~data_test$cluster,method="BH") 

kruskal.test(data_test$OXYGENE~data_test$cluster)
DunnTest(data_test$OXYGENE~data_test$cluster,method="BH") 

kruskal.test(data_test$CHLOROA~data_test$cluster)
DunnTest(data_test$CHLOROA~data_test$cluster,method="BH") 

kruskal.test(data_test$`NO3+NO2`~data_test$cluster)
DunnTest(data_test$`NO3+NO2`~data_test$cluster,method="BH") 

kruskal.test(data_test$`TURB-FNU`~data_test$cluster)
DunnTest(data_test$`TURB-FNU`~data_test$cluster,method="BH") 

kruskal.test(data_test$Bacillariophyceae~data_test$cluster)
DunnTest(data_test$Bacillariophyceae~data_test$cluster,method="BH") 

kruskal.test(data_test$Dinophyceae~data_test$cluster)
DunnTest(data_test$Dinophyceae~data_test$cluster,method="BH") 

kruskal.test(data_test$Ciliophora~data_test$cluster)
DunnTest(data_test$Ciliophora~data_test$cluster,method="BH") 

kruskal.test(data_test$Cryptophyceae~data_test$cluster)
DunnTest(data_test$Cryptophyceae~data_test$cluster,method="BH") 

kruskal.test(data_test$Haptophyta~data_test$cluster)
DunnTest(data_test$Haptophyta~data_test$cluster,method="BH") 

kruskal.test(data_test$Shannon~data_test$cluster)
DunnTest(data_test$Shannon~data_test$cluster,method="BH") 

kruskal.test(data_test$Pielou~data_test$cluster)
DunnTest(data_test$Pielou~data_test$cluster,method="BH") 

kruskal.test(data_test$BergerParker~data_test$cluster)
DunnTest(data_test$BergerParker~data_test$cluster,method="BH") 

kruskal.test(data_test$Rspe~data_test$cluster)
DunnTest(data_test$Rspe~data_test$cluster,method="BH") 

# Spring
data_test <- filter(data, season == "Spring")

kruskal.test(data_test$TEMP~data_test$cluster)
DunnTest(data_test$TEMP~data_test$cluster,method="BH") 

kruskal.test(data_test$SALI~data_test$cluster)
DunnTest(data_test$SALI~data_test$cluster,method="BH") 

kruskal.test(data_test$NH4~data_test$cluster)
DunnTest(data_test$NH4~data_test$cluster,method="BH") 

kruskal.test(data_test$PO4~data_test$cluster)
DunnTest(data_test$PO4~data_test$cluster,method="BH") 

kruskal.test(data_test$SIOH~data_test$cluster)
DunnTest(data_test$SIOH~data_test$cluster,method="BH") 

kruskal.test(data_test$OXYGENE~data_test$cluster)
DunnTest(data_test$OXYGENE~data_test$cluster,method="BH") 

kruskal.test(data_test$CHLOROA~data_test$cluster)
DunnTest(data_test$CHLOROA~data_test$cluster,method="BH") 

kruskal.test(data_test$`NO3+NO2`~data_test$cluster)
DunnTest(data_test$`NO3+NO2`~data_test$cluster,method="BH") 

kruskal.test(data_test$`TURB-FNU`~data_test$cluster)
DunnTest(data_test$`TURB-FNU`~data_test$cluster,method="BH") 

kruskal.test(data_test$Bacillariophyceae~data_test$cluster)
DunnTest(data_test$Bacillariophyceae~data_test$cluster,method="BH") 

kruskal.test(data_test$Dinophyceae~data_test$cluster)
DunnTest(data_test$Dinophyceae~data_test$cluster,method="BH") 

kruskal.test(data_test$Ciliophora~data_test$cluster)
DunnTest(data_test$Ciliophora~data_test$cluster,method="BH") 

kruskal.test(data_test$Cryptophyceae~data_test$cluster)
DunnTest(data_test$Cryptophyceae~data_test$cluster,method="BH") 

kruskal.test(data_test$Haptophyta~data_test$cluster)
DunnTest(data_test$Haptophyta~data_test$cluster,method="BH") 

kruskal.test(data_test$Shannon~data_test$cluster)
DunnTest(data_test$Shannon~data_test$cluster,method="BH") 

kruskal.test(data_test$Pielou~data_test$cluster)
DunnTest(data_test$Pielou~data_test$cluster,method="BH") 

kruskal.test(data_test$BergerParker~data_test$cluster)
DunnTest(data_test$BergerParker~data_test$cluster,method="BH") 

kruskal.test(data_test$Rspe~data_test$cluster)
DunnTest(data_test$Rspe~data_test$cluster,method="BH") 

# Summer
data_test <- filter(data, season == "Summer")

kruskal.test(data_test$TEMP~data_test$cluster)
DunnTest(data_test$TEMP~data_test$cluster,method="BH") 

kruskal.test(data_test$SALI~data_test$cluster)
DunnTest(data_test$SALI~data_test$cluster,method="BH") 

kruskal.test(data_test$NH4~data_test$cluster)
DunnTest(data_test$NH4~data_test$cluster,method="BH") 

kruskal.test(data_test$PO4~data_test$cluster)
DunnTest(data_test$PO4~data_test$cluster,method="BH") 

kruskal.test(data_test$SIOH~data_test$cluster)
DunnTest(data_test$SIOH~data_test$cluster,method="BH") 

kruskal.test(data_test$OXYGENE~data_test$cluster)
DunnTest(data_test$OXYGENE~data_test$cluster,method="BH") 

kruskal.test(data_test$CHLOROA~data_test$cluster)
DunnTest(data_test$CHLOROA~data_test$cluster,method="BH") 

kruskal.test(data_test$`NO3+NO2`~data_test$cluster)
DunnTest(data_test$`NO3+NO2`~data_test$cluster,method="BH") 

kruskal.test(data_test$`TURB-FNU`~data_test$cluster)
DunnTest(data_test$`TURB-FNU`~data_test$cluster,method="BH") 

kruskal.test(data_test$Bacillariophyceae~data_test$cluster)
DunnTest(data_test$Bacillariophyceae~data_test$cluster,method="BH") 

kruskal.test(data_test$Dinophyceae~data_test$cluster)
DunnTest(data_test$Dinophyceae~data_test$cluster,method="BH") 

kruskal.test(data_test$Ciliophora~data_test$cluster)
DunnTest(data_test$Ciliophora~data_test$cluster,method="BH") 

kruskal.test(data_test$Cryptophyceae~data_test$cluster)
DunnTest(data_test$Cryptophyceae~data_test$cluster,method="BH") 

kruskal.test(data_test$Haptophyta~data_test$cluster)
DunnTest(data_test$Haptophyta~data_test$cluster,method="BH") 

kruskal.test(data_test$Shannon~data_test$cluster)
DunnTest(data_test$Shannon~data_test$cluster,method="BH") 

kruskal.test(data_test$Pielou~data_test$cluster)
DunnTest(data_test$Pielou~data_test$cluster,method="BH") 

kruskal.test(data_test$BergerParker~data_test$cluster)
DunnTest(data_test$BergerParker~data_test$cluster,method="BH") 

kruskal.test(data_test$Rspe~data_test$cluster)
DunnTest(data_test$Rspe~data_test$cluster,method="BH") 

# Winter
data_test <- filter(data, season == "Winter")

kruskal.test(data_test$TEMP~data_test$cluster)
DunnTest(data_test$TEMP~data_test$cluster,method="BH") 

kruskal.test(data_test$SALI~data_test$cluster)
DunnTest(data_test$SALI~data_test$cluster,method="BH") 

kruskal.test(data_test$NH4~data_test$cluster)
DunnTest(data_test$NH4~data_test$cluster,method="BH") 

kruskal.test(data_test$PO4~data_test$cluster)
DunnTest(data_test$PO4~data_test$cluster,method="BH") 

kruskal.test(data_test$SIOH~data_test$cluster)
DunnTest(data_test$SIOH~data_test$cluster,method="BH") 

kruskal.test(data_test$OXYGENE~data_test$cluster)
DunnTest(data_test$OXYGENE~data_test$cluster,method="BH") 

kruskal.test(data_test$CHLOROA~data_test$cluster)
DunnTest(data_test$CHLOROA~data_test$cluster,method="BH") 

kruskal.test(data_test$`NO3+NO2`~data_test$cluster)
DunnTest(data_test$`NO3+NO2`~data_test$cluster,method="BH") 

kruskal.test(data_test$`TURB-FNU`~data_test$cluster)
DunnTest(data_test$`TURB-FNU`~data_test$cluster,method="BH") 

kruskal.test(data_test$Bacillariophyceae~data_test$cluster)
DunnTest(data_test$Bacillariophyceae~data_test$cluster,method="BH") 

kruskal.test(data_test$Dinophyceae~data_test$cluster)
DunnTest(data_test$Dinophyceae~data_test$cluster,method="BH") 

kruskal.test(data_test$Ciliophora~data_test$cluster)
DunnTest(data_test$Ciliophora~data_test$cluster,method="BH") 

kruskal.test(data_test$Cryptophyceae~data_test$cluster)
DunnTest(data_test$Cryptophyceae~data_test$cluster,method="BH") 

kruskal.test(data_test$Haptophyta~data_test$cluster)
DunnTest(data_test$Haptophyta~data_test$cluster,method="BH") 

kruskal.test(data_test$Shannon~data_test$cluster)
DunnTest(data_test$Shannon~data_test$cluster,method="BH") 

kruskal.test(data_test$Pielou~data_test$cluster)
DunnTest(data_test$Pielou~data_test$cluster,method="BH") 

kruskal.test(data_test$BergerParker~data_test$cluster)
DunnTest(data_test$BergerParker~data_test$cluster,method="BH") 

kruskal.test(data_test$Rspe~data_test$cluster)
DunnTest(data_test$Rspe~data_test$cluster,method="BH") 


