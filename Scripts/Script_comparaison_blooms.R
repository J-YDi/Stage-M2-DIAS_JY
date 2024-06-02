# Script JY.Dias - Stage M2 #

#####################################################################################
#                     NEED TO RUN Script_diversite_beta.R                           #
#####################################################################################
# Load packages
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(forcats)
library(DescTools)
library(corrplot)
library(cowplot)

# Comparison before / during / after bloom of Dinophyceae #####
# Import data
metric <- read_delim("data_modif/metrics.csv", 
                     delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                     locale = locale(decimal_mark = ",", grouping_mark = "."), 
                     trim_ws = TRUE)
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

data <- dplyr::select(data, Code_point_Libelle, Date, cluster, Bloom_Phylum,ID.interne.passage)

data_met <- left_join(metric,data)

# There are duplicates to delete
doublons <- data_met[duplicated(data_met$ID.interne.passage) |
                       duplicated(data_met$ID.interne.passage, fromLast = TRUE), ]

resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_met, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

data_met <- data_join

# Keep only the Bacillariophyceae and Dinophyceae's blooms
data <- filter(data_met, Bloom_Phylum == "Bac" | Bloom_Phylum == "Dino" | is.na(Bloom_Phylum))
data[is.na(data$Bloom_Phylum),]$Bloom_Phylum <- "Non"

# Import the data from the diversity beta analysis to keep only the date before and after the bloom already indicated
beta <- read_delim("data_modif/data_div_beta_ok_N-1_final.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)

comp_metric <- left_join(data,beta)
comp_metric$Nrow <- rownames(comp_metric)
pendant <- comp_metric[comp_metric$EpBloom == "OUI" & comp_metric$Bloom_Phylum == "Dino",]
avant <- comp_metric[as.numeric(pendant$Nrow)-1,]
avant$Bloom <- comp_metric[as.numeric(pendant$Nrow),"Bloom"]$Bloom
pendant$Nrow <- NULL
avant$Nrow <- NULL
pendant$cat <- "Pendant"
avant$cat <- "Avant"
comp_metric_ok <- bind_rows(pendant,avant)

beta <- read_delim("data_modif/data_div_beta_ok_N1_final.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)

comp_metric <- left_join(data,beta)
comp_metric$Nrow <- rownames(comp_metric)
pendant <- comp_metric[comp_metric$EpBloom == "OUI" & comp_metric$Bloom_Phylum == "Dino",]
apres <- comp_metric[as.numeric(pendant$Nrow)+1,]
apres$Bloom <- comp_metric[as.numeric(pendant$Nrow),"Bloom"]$Bloom
pendant$Nrow <- NULL
apres$Nrow <- NULL
pendant$cat <- "Pendant"
apres$cat <- "Apres"
comp_metric_ok <- bind_rows(comp_metric_ok,apres)

pendant.succession <- comp_metric[comp_metric$EpBloom == "Sucession" & comp_metric$Bloom_Phylum == "Dino",]
pendant.succession$Nrow <- NULL
pendant.succession$cat <- "Pendant"

comp_metric_ok <- bind_rows(comp_metric_ok,pendant.succession)

# Graph metrics

comp_metric_ok$cat <- as.factor(comp_metric_ok$cat)
levels(comp_metric_ok$cat)
comp_metric_ok$cat <- fct_relevel(comp_metric_ok$cat,c("Avant","Pendant","Apres"))
datal <- pivot_longer(comp_metric_ok,cols = N_noeuds:N_clust,names_to = "var")

ggplot(datal)+
  geom_boxplot(aes(x=cat, y= value,group=cat))+
  facet_wrap(~var,scales = "free_y")


kruskal.test(comp_metric_ok$N_noeuds~comp_metric_ok$cat)
DunnTest(comp_metric_ok$N_noeuds~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$Adhes~comp_metric_ok$cat)
DunnTest(comp_metric_ok$Adhes~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$Assort~comp_metric_ok$cat)
DunnTest(comp_metric_ok$Assort~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$Avg_p_length~comp_metric_ok$cat)
DunnTest(comp_metric_ok$Avg_p_length~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$C_tance~comp_metric_ok$cat)
DunnTest(comp_metric_ok$C_tance~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$D_liens~comp_metric_ok$cat)
DunnTest(comp_metric_ok$D_liens~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$Diss~comp_metric_ok$cat)
DunnTest(comp_metric_ok$Diss~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$meanN_liens~comp_metric_ok$cat)
DunnTest(comp_metric_ok$meanN_liens~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$meanN_voisins~comp_metric_ok$cat)
DunnTest(comp_metric_ok$meanN_voisins~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$Mod~comp_metric_ok$cat)
DunnTest(comp_metric_ok$Mod~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$N_clust~comp_metric_ok$cat)
DunnTest(comp_metric_ok$N_clust~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$N_liens~comp_metric_ok$cat)
DunnTest(comp_metric_ok$N_liens~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$Nat_connect~comp_metric_ok$cat)
DunnTest(comp_metric_ok$Nat_connect~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$Trans~comp_metric_ok$cat)
DunnTest(comp_metric_ok$Trans~comp_metric_ok$cat,method = "BH")

c("Adhes","Assort","C_tance","LML","Mod","N_noeuds","Nat_connect")
datagraph <- filter(datal,cluster == 1 & var %in% c("Adhes"))
datagraph$Phylum <- "Dinophyceae"
adhes <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#F8766D")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 1 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0,8,2),limits = c(0,10))+
  labs(x=NULL,y=NULL)+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 1 & var %in% c("Assort"))
datagraph$Phylum <- "Dinophyceae"
assort <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#F8766D")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 1 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  #scale_y_continuous(breaks = seq(-0.6,0.4,0.2),limits = c(0,0.4))+
  labs(x="Moment",y="")+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 1 & var %in% c("C_tance"))
datagraph$Phylum <- "Dinophyceae"
ctance <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#F8766D")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 1 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0.4,1,0.2),limits = c(0.5,1.1))+
  labs(x=NULL,y=NULL)+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"))

datal$var[datal$var== "Avg_p_length"] <- "LML"
datagraph <- filter(datal,cluster == 1 & var %in% c("LML"))
datagraph$Phylum <- "Dinophyceae"
LML <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#F8766D")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 1 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0.1,0.20,0.05),limits = c(0.1,0.22))+
  labs(x=NULL,y="Mesure")+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 1 & var %in% c("Mod"))
datagraph$Phylum <- "Dinophyceae"
Mod <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#F8766D")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 1 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(-0.1,0.10,0.05),limits = c(-0.1,0.15))+
  labs(x="Moment",y=NULL)+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 1 & var %in% c("N_noeuds"))
datagraph$Phylum <- "Dinophyceae"
noeuds <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#F8766D")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 1 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0,20,5),limits = c(0,35))+
  labs(x=NULL,y="")+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 1 & var %in% c("Nat_connect"))
datagraph$Phylum <- "Dinophyceae"
Nat_connect <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#F8766D")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 1 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0,0.3,0.05),limits = c(0.05,0.35))+
  labs(x=NULL,y=NULL)+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"))

dinometriques1 <- plot_grid(noeuds,Nat_connect,LML,ctance,assort,Mod,ncol=2)

comp_mt_cl <- filter(comp_metric_ok, cluster == 1)
kruskal.test(comp_mt_cl$N_noeuds~comp_mt_cl$cat)
DunnTest(comp_mt_cl$N_noeuds~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Adhes~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Adhes~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Assort~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Assort~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Avg_p_length~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Avg_p_length~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$C_tance~comp_mt_cl$cat)
DunnTest(comp_mt_cl$C_tance~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$D_liens~comp_mt_cl$cat)
DunnTest(comp_mt_cl$D_liens~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Diss~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Diss~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$meanN_liens~comp_mt_cl$cat)
DunnTest(comp_mt_cl$meanN_liens~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$meanN_voisins~comp_mt_cl$cat)
DunnTest(comp_mt_cl$meanN_voisins~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Mod~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Mod~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$N_clust~comp_mt_cl$cat)
DunnTest(comp_mt_cl$N_clust~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$N_liens~comp_mt_cl$cat)
DunnTest(comp_mt_cl$N_liens~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Nat_connect~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Nat_connect~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Trans~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Trans~comp_mt_cl$cat,method = "BH")



ggplot(filter(datal,cluster == 2))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  facet_wrap(~var,scales = "free_y")

datagraph <- filter(datal,cluster == 2 & var %in% c("Adhes"))
datagraph$Phylum <- "Dinophyceae"
adhes <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0,8,2),limits = c(0,10))+
  geom_point(aes(x=cat,y=value),data = filter(datagraph, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  labs(x=NULL,y=NULL)+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 2 & var %in% c("Assort"))
datagraph$Phylum <- "Dinophyceae"
assort <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  #scale_y_continuous(breaks = seq(-0.6,0.4,0.2),limits = c(0,0.4))+
  geom_point(aes(x=cat,y=value),data = filter(datagraph, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  labs(x="Moment",y="")+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 2 & var %in% c("C_tance"))
datagraph$Phylum <- "Dinophyceae"
ctance <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0.4,1,0.2),limits = c(0.5,1.1))+
  geom_point(aes(x=cat,y=value),data = filter(datagraph, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  labs(x=NULL,y=NULL)+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"))

datal$var[datal$var== "Avg_p_length"] <- "LML"
datagraph <- filter(datal,cluster == 2 & var %in% c("LML"))
datagraph$Phylum <- "Dinophyceae"
LML <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0.1,0.20,0.05),limits = c(0.1,0.22))+
  geom_point(aes(x=cat,y=value),data = filter(datagraph, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  labs(x=NULL,y="Mesure")+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 2 & var %in% c("Mod"))
datagraph$Phylum <- "Dinophyceae"
Mod <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(-0.1,0.10,0.05),limits = c(-0.1,0.15))+
  geom_point(aes(x=cat,y=value),data = filter(datagraph, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  labs(x="Moment",y=NULL)+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 2 & var %in% c("N_noeuds"))
datagraph$Phylum <- "Dinophyceae"
noeuds <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  geom_point(aes(x=cat,y=value),data = filter(datagraph, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  scale_y_continuous(breaks = seq(0,20,5),limits = c(0,35))+
  labs(x=NULL,y="")+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 2 & var %in% c("Nat_connect"))
datagraph$Phylum <- "Dinophyceae"
Nat_connect <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  geom_point(aes(x=cat,y=value),data = filter(datagraph, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  scale_y_continuous(breaks = seq(0,0.3,0.05),limits = c(0.05,0.35))+
  labs(x=NULL,y=NULL)+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"))

dinometriques2 <- plot_grid(noeuds,Nat_connect,LML,ctance,assort,Mod,ncol=2)

comp_mt_cl <- filter(comp_metric_ok, cluster == 2)
kruskal.test(comp_mt_cl$N_noeuds~comp_mt_cl$cat)
DunnTest(comp_mt_cl$N_noeuds~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Adhes~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Adhes~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Assort~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Assort~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Avg_p_length~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Avg_p_length~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$C_tance~comp_mt_cl$cat)
DunnTest(comp_mt_cl$C_tance~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$D_liens~comp_mt_cl$cat)
DunnTest(comp_mt_cl$D_liens~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Diss~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Diss~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$meanN_liens~comp_mt_cl$cat)
DunnTest(comp_mt_cl$meanN_liens~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$meanN_voisins~comp_mt_cl$cat)
DunnTest(comp_mt_cl$meanN_voisins~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Mod~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Mod~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$N_clust~comp_mt_cl$cat)
DunnTest(comp_mt_cl$N_clust~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$N_liens~comp_mt_cl$cat)
DunnTest(comp_mt_cl$N_liens~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Nat_connect~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Nat_connect~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Trans~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Trans~comp_mt_cl$cat,method = "BH")



ggplot(filter(datal,cluster == 3))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00BE67")+
  geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  facet_wrap(~var,scales = "free_y")



ggplot(filter(datal,cluster == 3))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  facet_wrap(~var,scales = "free_y")

datagraph <- filter(datal,cluster == 3 & var %in% c("Adhes"))
datagraph$Phylum <- "Dinophyceae"
adhes <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00BE67")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0,8,2),limits = c(0,10))+
  geom_point(aes(x=cat,y=value),data = filter(datagraph, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  labs(x=NULL,y=NULL)+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 3 & var %in% c("Assort"))
datagraph$Phylum <- "Dinophyceae"
assort <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00BE67")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  #scale_y_continuous(breaks = seq(-0.6,0.4,0.2),limits = c(0,0.4))+
  geom_point(aes(x=cat,y=value),data = filter(datagraph, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  labs(x="Moment",y="")+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 3 & var %in% c("C_tance"))
datagraph$Phylum <- "Dinophyceae"
ctance <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00BE67")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0.4,1,0.2),limits = c(0.5,1.1))+
  geom_point(aes(x=cat,y=value),data = filter(datagraph, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  labs(x=NULL,y=NULL)+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"))

datal$var[datal$var== "Avg_p_length"] <- "LML"
datagraph <- filter(datal,cluster == 3 & var %in% c("LML"))
datagraph$Phylum <- "Dinophyceae"
LML <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00BE67")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0.1,0.20,0.05),limits = c(0.1,0.22))+
  geom_point(aes(x=cat,y=value),data = filter(datagraph, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  labs(x=NULL,y="Mesure")+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 3 & var %in% c("Mod"))
datagraph$Phylum <- "Dinophyceae"
Mod <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00BE67")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(-0.1,0.10,0.05),limits = c(-0.1,0.15))+
  geom_point(aes(x=cat,y=value),data = filter(datagraph, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  labs(x="Moment",y=NULL)+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 3 & var %in% c("N_noeuds"))
datagraph$Phylum <- "Dinophyceae"
noeuds <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00BE67")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  geom_point(aes(x=cat,y=value),data = filter(datagraph, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  scale_y_continuous(breaks = seq(0,20,5),limits = c(0,35))+
  labs(x=NULL,y="")+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 3 & var %in% c("Nat_connect"))
datagraph$Phylum <- "Dinophyceae"
Nat_connect <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00BE67")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  geom_point(aes(x=cat,y=value),data = filter(datagraph, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  scale_y_continuous(breaks = seq(0,0.3,0.05),limits = c(0.05,0.35))+
  labs(x=NULL,y=NULL)+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"))

dinometriques3 <- plot_grid(noeuds,Nat_connect,LML,ctance,assort,Mod,ncol=2)

plot_grid(dinometriques1,dinometriques2,dinometriques3,ncol=3,labels = "AUTO")

comp_mt_cl <- filter(comp_metric_ok, cluster == 3)
kruskal.test(comp_mt_cl$N_noeuds~comp_mt_cl$cat)
DunnTest(comp_mt_cl$N_noeuds~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Adhes~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Adhes~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Assort~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Assort~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Avg_p_length~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Avg_p_length~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$C_tance~comp_mt_cl$cat)
DunnTest(comp_mt_cl$C_tance~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$D_liens~comp_mt_cl$cat)
DunnTest(comp_mt_cl$D_liens~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Diss~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Diss~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$meanN_liens~comp_mt_cl$cat)
DunnTest(comp_mt_cl$meanN_liens~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$meanN_voisins~comp_mt_cl$cat)
DunnTest(comp_mt_cl$meanN_voisins~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Mod~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Mod~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$N_clust~comp_mt_cl$cat)
DunnTest(comp_mt_cl$N_clust~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$N_liens~comp_mt_cl$cat)
DunnTest(comp_mt_cl$N_liens~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Nat_connect~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Nat_connect~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Trans~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Trans~comp_mt_cl$cat,method = "BH")

# Diversity indexes
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)
data_idiv <- dplyr::select(data,ID.interne.passage,Shannon, Pielou, BergerParker,Rspe)

comp_div_ok <- left_join(comp_metric_ok,data_idiv)
comp_div_ok <- dplyr::select(comp_div_ok,-(N_noeuds:N_clust))

comp_div_ok$cat <- as.factor(comp_div_ok$cat)
levels(comp_div_ok$cat)
comp_div_ok$cat <- fct_relevel(comp_div_ok$cat,c("Avant","Pendant","Apres"))
datal <- pivot_longer(comp_div_ok,cols = Shannon:Rspe,names_to = "var")


datal$Phylum <- "Dinophyceae"

dinodiv1 <- ggplot(filter(datal,cluster == 1))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#F8766D")+
  facet_wrap(Phylum~var,scales = "free_y",ncol=2)+
  labs(x="Moment",y="Mesure")+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1, size = 10))

dinodiv2 <- ggplot(filter(datal,cluster == 2 ))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  facet_wrap(Phylum~var,scales = "free_y",ncol=2)+
  labs(x="Moment",y="Mesure")+
  geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1, size = 10))

dinodiv3 <- ggplot(filter(datal,cluster == 3 ))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00BE67")+
  facet_wrap(Phylum~var,scales = "free_y",ncol=2)+
  labs(x="Moment",y="Mesure")+
  geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1, size = 10))

dinodiv <- plot_grid(dinodiv1,dinodiv2,dinodiv3,ncol=3)

ggplot(datal)+
  geom_boxplot(aes(x=cat, y= value,group=cat))+
  facet_wrap(~var,scales = "free_y")


kruskal.test(comp_div_ok$Rspe~comp_metric_ok$cat)
DunnTest(comp_div_ok$Rspe~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_div_ok$BergerParker~comp_metric_ok$cat)
DunnTest(comp_div_ok$BergerParker~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_div_ok$Pielou~comp_metric_ok$cat)
DunnTest(comp_div_ok$Pielou~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_div_ok$Shannon~comp_metric_ok$cat)
DunnTest(comp_div_ok$Shannon~comp_metric_ok$cat,method = "BH")



ggplot(filter(datal,cluster == 1))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#F8766D")+
  facet_wrap(~var,scales = "free_y")

comp_mt_cl <- filter(comp_div_ok, cluster == 1)
kruskal.test(comp_mt_cl$Rspe~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Rspe~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$BergerParker~comp_mt_cl$cat)
DunnTest(comp_mt_cl$BergerParker~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Pielou~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Pielou~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Shannon~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Shannon~comp_mt_cl$cat,method = "BH")


ggplot(filter(datal,cluster == 2))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  facet_wrap(~var,scales = "free_y")

comp_mt_cl <- filter(comp_div_ok, cluster == 2)
kruskal.test(comp_mt_cl$Rspe~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Rspe~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$BergerParker~comp_mt_cl$cat)
DunnTest(comp_mt_cl$BergerParker~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Pielou~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Pielou~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Shannon~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Shannon~comp_mt_cl$cat,method = "BH")


ggplot(filter(datal,cluster == 3))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00BE67")+
  geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  facet_wrap(~var,scales = "free_y")

comp_mt_cl <- filter(comp_div_ok, cluster == 3)
kruskal.test(comp_mt_cl$Rspe~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Rspe~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$BergerParker~comp_mt_cl$cat)
DunnTest(comp_mt_cl$BergerParker~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Pielou~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Pielou~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Shannon~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Shannon~comp_mt_cl$cat,method = "BH")


# Idem with hydro data

data_abio <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)
data_abio <- dplyr::select(data_abio,ID.interne.passage,CHLOROA:`TURB-FNU`)

comp_abio_ok <- left_join(comp_metric_ok,data_abio)
comp_abio_ok <- dplyr::select(comp_abio_ok,-(N_noeuds:N_clust))

comp_abio_ok$cat <- as.factor(comp_abio_ok$cat)
levels(comp_abio_ok$cat)
comp_abio_ok$cat <- fct_relevel(comp_abio_ok$cat,c("Avant","Pendant","Apres"))
datal <- pivot_longer(comp_abio_ok,cols = CHLOROA:`TURB-FNU`,names_to = "var")

ggplot(datal)+
  geom_boxplot(aes(x=cat, y= value,group=cat))+
  facet_wrap(~var,scales = "free_y")


kruskal.test(comp_abio_ok$TEMP~comp_abio_ok$cat)
DunnTest(comp_abio_ok$TEMP~comp_abio_ok$cat,method = "BH")

kruskal.test(comp_abio_ok$SALI~comp_abio_ok$cat)
DunnTest(comp_abio_ok$SALI~comp_abio_ok$cat,method = "BH")

kruskal.test(comp_abio_ok$CHLOROA~comp_abio_ok$cat)
DunnTest(comp_abio_ok$CHLOROA~comp_abio_ok$cat,method = "BH")

kruskal.test(comp_abio_ok$NH4~comp_abio_ok$cat)
DunnTest(comp_abio_ok$NH4~comp_abio_ok$cat,method = "BH")

kruskal.test(comp_abio_ok$`NO3+NO2`~comp_abio_ok$cat)
DunnTest(comp_abio_ok$`NO3+NO2`~comp_abio_ok$cat,method = "BH")

kruskal.test(comp_abio_ok$OXYGENE~comp_abio_ok$cat)
DunnTest(comp_abio_ok$OXYGENE~comp_abio_ok$cat,method = "BH")

kruskal.test(comp_abio_ok$PO4~comp_abio_ok$cat)
DunnTest(comp_abio_ok$PO4~comp_abio_ok$cat,method = "BH")

kruskal.test(comp_abio_ok$SIOH~comp_abio_ok$cat)
DunnTest(comp_abio_ok$SIOH~comp_abio_ok$cat,method = "BH")

kruskal.test(comp_abio_ok$TURB~comp_abio_ok$cat)
DunnTest(comp_abio_ok$TURB~comp_abio_ok$cat,method = "BH")

kruskal.test(comp_abio_ok$`TURB-FNU`~comp_abio_ok$cat)
DunnTest(comp_abio_ok$`TURB-FNU`~comp_abio_ok$cat,method = "BH")

datal$Phylum <- "Dinophyceae"

dinoabio1 <- ggplot(filter(datal,cluster == 1 & ! var %in% c("TEMP","TURB") ))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#F8766D")+
  facet_wrap(Phylum~var,scales = "free_y",ncol=4)+
  labs(x="Moment",y="Mesure")+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1, size = 10))

dinoabio2 <- ggplot(filter(datal,cluster == 2 & ! var %in% c("TEMP","TURB") ))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  facet_wrap(Phylum~var,scales = "free_y",ncol=4)+
  labs(x="Moment",y="Mesure")+
  geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 2 & Bloom == "Dino-Lepidodinium" & ! var %in% c("TEMP","TURB")),size=2,shape=8)+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1, size = 10))

dinoabio3 <- ggplot(filter(datal,cluster == 3 & ! var %in% c("TEMP","TURB") ))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00BE67")+
  facet_wrap(Phylum~var,scales = "free_y",ncol=4)+
  labs(x="Moment",y="Mesure")+
  geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 3 & Bloom == "Dino-Lepidodinium" & ! var %in% c("TEMP","TURB")),size=2,shape=8)+
  theme(strip.background = element_rect(fill = "#009E73"),strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1, size = 10))

dinoabio <- plot_grid(dinoabio1,dinoabio2,dinoabio3,ncol=3,labels = "AUTO")

plot_grid(dinoabio,dinodiv,ncol=1)

comp_mt_cl <- filter(comp_abio_ok, cluster == 1)
kruskal.test(comp_mt_cl$TEMP~comp_mt_cl$cat)
DunnTest(comp_mt_cl$TEMP~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$SALI~comp_mt_cl$cat)
DunnTest(comp_mt_cl$SALI~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$CHLOROA~comp_mt_cl$cat)
DunnTest(comp_mt_cl$CHLOROA~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$NH4~comp_mt_cl$cat)
DunnTest(comp_mt_cl$NH4~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$`NO3+NO2`~comp_mt_cl$cat)
DunnTest(comp_mt_cl$`NO3+NO2`~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$OXYGENE~comp_mt_cl$cat)
DunnTest(comp_mt_cl$OXYGENE~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$PO4~comp_mt_cl$cat)
DunnTest(comp_mt_cl$PO4~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$SIOH~comp_mt_cl$cat)
DunnTest(comp_mt_cl$SIOH~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$TURB~comp_mt_cl$cat)
DunnTest(comp_mt_cl$TURB~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$`TURB-FNU`~comp_mt_cl$cat)
DunnTest(comp_mt_cl$`TURB-FNU`~comp_mt_cl$cat,method = "BH")


ggplot(filter(datal,cluster == 2))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  facet_wrap(~var,scales = "free_y")

comp_mt_cl <- filter(comp_abio_ok, cluster == 2)
kruskal.test(comp_mt_cl$TEMP~comp_mt_cl$cat)
DunnTest(comp_mt_cl$TEMP~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$SALI~comp_mt_cl$cat)
DunnTest(comp_mt_cl$SALI~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$CHLOROA~comp_mt_cl$cat)
DunnTest(comp_mt_cl$CHLOROA~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$NH4~comp_mt_cl$cat)
DunnTest(comp_mt_cl$NH4~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$`NO3+NO2`~comp_mt_cl$cat)
DunnTest(comp_mt_cl$`NO3+NO2`~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$OXYGENE~comp_mt_cl$cat)
DunnTest(comp_mt_cl$OXYGENE~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$PO4~comp_mt_cl$cat)
DunnTest(comp_mt_cl$PO4~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$SIOH~comp_mt_cl$cat)
DunnTest(comp_mt_cl$SIOH~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$TURB~comp_mt_cl$cat)
DunnTest(comp_mt_cl$TURB~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$`TURB-FNU`~comp_mt_cl$cat)
DunnTest(comp_mt_cl$`TURB-FNU`~comp_mt_cl$cat,method = "BH")

ggplot(filter(datal,cluster == 3))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00BE67")+
  geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  facet_wrap(~var,scales = "free_y")

comp_mt_cl <- filter(comp_abio_ok, cluster == 3)
kruskal.test(comp_mt_cl$TEMP~comp_mt_cl$cat)
DunnTest(comp_mt_cl$TEMP~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$SALI~comp_mt_cl$cat)
DunnTest(comp_mt_cl$SALI~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$CHLOROA~comp_mt_cl$cat)
DunnTest(comp_mt_cl$CHLOROA~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$NH4~comp_mt_cl$cat)
DunnTest(comp_mt_cl$NH4~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$`NO3+NO2`~comp_mt_cl$cat)
DunnTest(comp_mt_cl$`NO3+NO2`~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$OXYGENE~comp_mt_cl$cat)
DunnTest(comp_mt_cl$OXYGENE~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$PO4~comp_mt_cl$cat)
DunnTest(comp_mt_cl$PO4~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$SIOH~comp_mt_cl$cat)
DunnTest(comp_mt_cl$SIOH~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$TURB~comp_mt_cl$cat)
DunnTest(comp_mt_cl$TURB~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$`TURB-FNU`~comp_mt_cl$cat)
DunnTest(comp_mt_cl$`TURB-FNU`~comp_mt_cl$cat,method = "BH")


# Comparison before / during / after bloom of Bacillariophyceae #####
# Import data
metric <- read_delim("data_modif/metrics.csv", 
                     delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                     locale = locale(decimal_mark = ",", grouping_mark = "."), 
                     trim_ws = TRUE)

data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

data <- dplyr::select(data, Code_point_Libelle, Date, cluster, Bloom_Phylum,ID.interne.passage)

data_met <- left_join(metric,data)

# There are duplicates to delete
doublons <- data_met[duplicated(data_met$ID.interne.passage) |
                       duplicated(data_met$ID.interne.passage, fromLast = TRUE), ]

resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_met, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

data_met <- data_join

data <- filter(data_met, Bloom_Phylum == "Bac" | Bloom_Phylum == "Dino" | is.na(Bloom_Phylum))
data[is.na(data$Bloom_Phylum),]$Bloom_Phylum <- "Non"

beta <- read_delim("data_modif/data_div_beta_ok_N-1_final.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)

comp_metric <- left_join(data,beta)
comp_metric$Nrow <- rownames(comp_metric)
pendant <- comp_metric[comp_metric$EpBloom == "OUI" & comp_metric$Bloom_Phylum == "Bac",]
avant <- comp_metric[as.numeric(pendant$Nrow)-1,]
avant$Bloom <- comp_metric[as.numeric(pendant$Nrow),"Bloom"]$Bloom
pendant$Nrow <- NULL
avant$Nrow <- NULL
pendant$cat <- "Pendant"
avant$cat <- "Avant"
comp_metric_ok <- bind_rows(pendant,avant)

beta <- read_delim("data_modif/data_div_beta_ok_N1_final.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)

comp_metric <- left_join(data,beta)
comp_metric$Nrow <- rownames(comp_metric)
pendant <- comp_metric[comp_metric$EpBloom == "OUI" & comp_metric$Bloom_Phylum == "Bac",]
apres <- comp_metric[as.numeric(pendant$Nrow)+1,]
apres$Bloom <- comp_metric[as.numeric(pendant$Nrow),"Bloom"]$Bloom
pendant$Nrow <- NULL
apres$Nrow <- NULL
pendant$cat <- "Pendant"
apres$cat <- "Apres"
comp_metric_ok <- bind_rows(comp_metric_ok,apres)

pendant.succession <- comp_metric[comp_metric$EpBloom == "Sucession" & comp_metric$Bloom_Phylum == "Bac",]
pendant.succession$Nrow <- NULL
pendant.succession$cat <- "Pendant"

comp_metric_ok <- bind_rows(comp_metric_ok,pendant.succession)


comp_metric_ok$cat <- as.factor(comp_metric_ok$cat)
levels(comp_metric_ok$cat)
comp_metric_ok$cat <- fct_relevel(comp_metric_ok$cat,c("Avant","Pendant","Apres"))
datal <- pivot_longer(comp_metric_ok,cols = N_noeuds:N_clust,names_to = "var")

ggplot(datal)+
  geom_boxplot(aes(x=cat, y= value,group=cat))+
  facet_wrap(~var,scales = "free_y")

datagraph <- filter(datal,cluster == 1 & var %in% c("Adhes"))
datagraph$Phylum <- "Bacillariophyceae"
adhes <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#F8766D")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 1 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0,8,2),limits = c(0,10))+
  labs(x=NULL,y=NULL)+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 1 & var %in% c("Assort"))
datagraph$Phylum <- "Bacillariophyceae"
assort <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#F8766D")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 1 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  #scale_y_continuous(breaks = seq(-0.6,0.4,0.2),limits = c(0,0.4))+
  labs(x="Moment",y="")+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 1 & var %in% c("C_tance"))
datagraph$Phylum <- "Bacillariophyceae"
ctance <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#F8766D")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 1 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0.4,1,0.2),limits = c(0.5,1.1))+
  labs(x=NULL,y=NULL)+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold"))

datal$var[datal$var== "Avg_p_length"] <- "LML"
datagraph <- filter(datal,cluster == 1 & var %in% c("LML"))
datagraph$Phylum <- "Bacillariophyceae"
LML <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#F8766D")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 1 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0.1,0.20,0.05),limits = c(0.1,0.22))+
  labs(x=NULL,y="Mesure")+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 1 & var %in% c("Mod"))
datagraph$Phylum <- "Bacillariophyceae"
Mod <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#F8766D")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 1 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(-0.1,0.10,0.05),limits = c(-0.1,0.15))+
  labs(x="Moment",y=NULL)+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 1 & var %in% c("N_noeuds"))
datagraph$Phylum <- "Bacillariophyceae"
noeuds <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#F8766D")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 1 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0,20,5),limits = c(0,35))+
  labs(x=NULL,y="")+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 1 & var %in% c("Nat_connect"))
datagraph$Phylum <- "Bacillariophyceae"
Nat_connect <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#F8766D")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 1 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0,0.3,0.05),limits = c(0.05,0.35))+
  labs(x=NULL,y=NULL)+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold"))

dinometriques1 <- plot_grid(noeuds,Nat_connect,LML,ctance,assort,Mod,ncol=2)


datagraph <- filter(datal,cluster == 2 & var %in% c("Adhes"))
datagraph$Phylum <- "Bacillariophyceae"
adhes <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0,8,2),limits = c(0,10))+
  labs(x=NULL,y=NULL)+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 2 & var %in% c("Assort"))
datagraph$Phylum <- "Bacillariophyceae"
assort <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  #scale_y_continuous(breaks = seq(-0.6,0.4,0.2),limits = c(0,0.4))+
  labs(x="Moment",y="")+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 2 & var %in% c("C_tance"))
datagraph$Phylum <- "Bacillariophyceae"
ctance <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0.4,1,0.2),limits = c(0.5,1.1))+
  labs(x=NULL,y=NULL)+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold"))

datal$var[datal$var== "Avg_p_length"] <- "LML"
datagraph <- filter(datal,cluster == 2 & var %in% c("LML"))
datagraph$Phylum <- "Bacillariophyceae"
LML <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0.1,0.20,0.05),limits = c(0.1,0.22))+
  labs(x=NULL,y="Mesure")+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 2 & var %in% c("Mod"))
datagraph$Phylum <- "Bacillariophyceae"
Mod <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(-0.1,0.10,0.05),limits = c(-0.1,0.15))+
  labs(x="Moment",y=NULL)+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 2 & var %in% c("N_noeuds"))
datagraph$Phylum <- "Bacillariophyceae"
noeuds <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0,20,5),limits = c(0,35))+
  labs(x=NULL,y="")+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 2 & var %in% c("Nat_connect"))
datagraph$Phylum <- "Bacillariophyceae"
Nat_connect <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0,0.3,0.05),limits = c(0.05,0.35))+
  labs(x=NULL,y=NULL)+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold"))

dinometriques2 <- plot_grid(noeuds,Nat_connect,LML,ctance,assort,Mod,ncol=2)



datagraph <- filter(datal,cluster == 3 & var %in% c("Adhes"))
datagraph$Phylum <- "Bacillariophyceae"
adhes <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00BE67")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0,8,2),limits = c(0,10))+
  labs(x=NULL,y=NULL)+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 3 & var %in% c("Assort"))
datagraph$Phylum <- "Bacillariophyceae"
assort <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00BE67")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  #scale_y_continuous(breaks = seq(-0.6,0.4,0.2),limits = c(0,0.4))+
  labs(x="Moment",y="")+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 3 & var %in% c("C_tance"))
datagraph$Phylum <- "Bacillariophyceae"
ctance <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00BE67")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0.4,1,0.2),limits = c(0.5,1.1))+
  labs(x=NULL,y=NULL)+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold"))

datal$var[datal$var== "Avg_p_length"] <- "LML"
datagraph <- filter(datal,cluster == 3 & var %in% c("LML"))
datagraph$Phylum <- "Bacillariophyceae"
LML <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00BE67")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0.1,0.20,0.05),limits = c(0.1,0.22))+
  labs(x=NULL,y="Mesure")+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 3 & var %in% c("Mod"))
datagraph$Phylum <- "Bacillariophyceae"
Mod <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00BE67")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(-0.1,0.10,0.05),limits = c(-0.1,0.15))+
  labs(x="Moment",y=NULL)+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 3 & var %in% c("N_noeuds"))
datagraph$Phylum <- "Bacillariophyceae"
noeuds <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00BE67")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0,20,5),limits = c(0,35))+
  labs(x=NULL,y="")+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold"))

datagraph <- filter(datal,cluster == 3 & var %in% c("Nat_connect"))
datagraph$Phylum <- "Bacillariophyceae"
Nat_connect <- ggplot(datagraph)+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00BE67")+
  #geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=5)+
  facet_wrap(Phylum~var,scales = "free_y")+
  scale_y_continuous(breaks = seq(0,0.3,0.05),limits = c(0.05,0.35))+
  labs(x=NULL,y=NULL)+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold"))

dinometriques3 <- plot_grid(noeuds,Nat_connect,LML,ctance,assort,Mod,ncol=2)

plot_grid(dinometriques1,dinometriques2,dinometriques3,ncol=3, labels = "AUTO")

kruskal.test(comp_metric_ok$N_noeuds~comp_metric_ok$cat)
DunnTest(comp_metric_ok$N_noeuds~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$Adhes~comp_metric_ok$cat)
DunnTest(comp_metric_ok$Adhes~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$Assort~comp_metric_ok$cat)
DunnTest(comp_metric_ok$Assort~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$Avg_p_length~comp_metric_ok$cat)
DunnTest(comp_metric_ok$Avg_p_length~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$C_tance~comp_metric_ok$cat)
DunnTest(comp_metric_ok$C_tance~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$D_liens~comp_metric_ok$cat)
DunnTest(comp_metric_ok$D_liens~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$Diss~comp_metric_ok$cat)
DunnTest(comp_metric_ok$Diss~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$meanN_liens~comp_metric_ok$cat)
DunnTest(comp_metric_ok$meanN_liens~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$meanN_voisins~comp_metric_ok$cat)
DunnTest(comp_metric_ok$meanN_voisins~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$Mod~comp_metric_ok$cat)
DunnTest(comp_metric_ok$Mod~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$N_clust~comp_metric_ok$cat)
DunnTest(comp_metric_ok$N_clust~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$N_liens~comp_metric_ok$cat)
DunnTest(comp_metric_ok$N_liens~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$Nat_connect~comp_metric_ok$cat)
DunnTest(comp_metric_ok$Nat_connect~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_metric_ok$Trans~comp_metric_ok$cat)
DunnTest(comp_metric_ok$Trans~comp_metric_ok$cat,method = "BH")


ggplot(filter(datal,cluster == 1))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#F8766D")+
  facet_wrap(~var,scales = "free_y")

comp_mt_cl <- filter(comp_metric_ok, cluster == 1)
kruskal.test(comp_mt_cl$N_noeuds~comp_mt_cl$cat)
DunnTest(comp_mt_cl$N_noeuds~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Adhes~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Adhes~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Assort~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Assort~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Avg_p_length~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Avg_p_length~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$C_tance~comp_mt_cl$cat)
DunnTest(comp_mt_cl$C_tance~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$D_liens~comp_mt_cl$cat)
DunnTest(comp_mt_cl$D_liens~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Diss~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Diss~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$meanN_liens~comp_mt_cl$cat)
DunnTest(comp_mt_cl$meanN_liens~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$meanN_voisins~comp_mt_cl$cat)
DunnTest(comp_mt_cl$meanN_voisins~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Mod~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Mod~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$N_clust~comp_mt_cl$cat)
DunnTest(comp_mt_cl$N_clust~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$N_liens~comp_mt_cl$cat)
DunnTest(comp_mt_cl$N_liens~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Nat_connect~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Nat_connect~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Trans~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Trans~comp_mt_cl$cat,method = "BH")



ggplot(filter(datal,cluster == 2))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  facet_wrap(~var,scales = "free_y")

comp_mt_cl <- filter(comp_metric_ok, cluster == 2)
kruskal.test(comp_mt_cl$N_noeuds~comp_mt_cl$cat)
DunnTest(comp_mt_cl$N_noeuds~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Adhes~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Adhes~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Assort~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Assort~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Avg_p_length~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Avg_p_length~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$C_tance~comp_mt_cl$cat)
DunnTest(comp_mt_cl$C_tance~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$D_liens~comp_mt_cl$cat)
DunnTest(comp_mt_cl$D_liens~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Diss~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Diss~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$meanN_liens~comp_mt_cl$cat)
DunnTest(comp_mt_cl$meanN_liens~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$meanN_voisins~comp_mt_cl$cat)
DunnTest(comp_mt_cl$meanN_voisins~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Mod~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Mod~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$N_clust~comp_mt_cl$cat)
DunnTest(comp_mt_cl$N_clust~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$N_liens~comp_mt_cl$cat)
DunnTest(comp_mt_cl$N_liens~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Nat_connect~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Nat_connect~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Trans~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Trans~comp_mt_cl$cat,method = "BH")



ggplot(filter(datal,cluster == 3))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00BE67")+
  facet_wrap(~var,scales = "free_y")

comp_mt_cl <- filter(comp_metric_ok, cluster == 3)
kruskal.test(comp_mt_cl$N_noeuds~comp_mt_cl$cat)
DunnTest(comp_mt_cl$N_noeuds~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Adhes~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Adhes~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Assort~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Assort~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Avg_p_length~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Avg_p_length~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$C_tance~comp_mt_cl$cat)
DunnTest(comp_mt_cl$C_tance~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$D_liens~comp_mt_cl$cat)
DunnTest(comp_mt_cl$D_liens~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Diss~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Diss~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$meanN_liens~comp_mt_cl$cat)
DunnTest(comp_mt_cl$meanN_liens~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$meanN_voisins~comp_mt_cl$cat)
DunnTest(comp_mt_cl$meanN_voisins~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Mod~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Mod~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$N_clust~comp_mt_cl$cat)
DunnTest(comp_mt_cl$N_clust~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$N_liens~comp_mt_cl$cat)
DunnTest(comp_mt_cl$N_liens~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Nat_connect~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Nat_connect~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Trans~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Trans~comp_mt_cl$cat,method = "BH")


# Diversity indexes
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)
data_idiv <- dplyr::select(data, ID.interne.passage,Shannon, Pielou, BergerParker,Rspe)

comp_div_ok <- left_join(comp_metric_ok,data_idiv)
comp_div_ok <- dplyr::select(comp_div_ok,-(N_noeuds:N_clust))

comp_div_ok$cat <- as.factor(comp_div_ok$cat)
levels(comp_div_ok$cat)
comp_div_ok$cat <- fct_relevel(comp_div_ok$cat,c("Avant","Pendant","Apres"))
datal <- pivot_longer(comp_div_ok,cols = Shannon:Rspe,names_to = "var")

ggplot(datal)+
  geom_boxplot(aes(x=cat, y= value,group=cat))+
  facet_wrap(~var,scales = "free_y")

datal$Phylum <- "Bacillariophyceae"

dinodiv1 <- ggplot(filter(datal,cluster == 1))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#F8766D")+
  facet_wrap(Phylum~var,scales = "free_y",ncol=2)+
  labs(x="Moment",y="Mesure")+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1, size = 10))

dinodiv2 <- ggplot(filter(datal,cluster == 2 ))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  facet_wrap(Phylum~var,scales = "free_y",ncol=2)+
  labs(x="Moment",y="Mesure")+
  geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 2 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1, size = 10))

dinodiv3 <- ggplot(filter(datal,cluster == 3 ))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00BE67")+
  facet_wrap(Phylum~var,scales = "free_y",ncol=2)+
  labs(x="Moment",y="Mesure")+
  geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 3 & Bloom == "Dino-Lepidodinium"),size=2,shape=8)+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1, size = 10))

bacdiv <- plot_grid(dinodiv1,dinodiv2,dinodiv3,ncol=3)


kruskal.test(comp_div_ok$Rspe~comp_metric_ok$cat)
DunnTest(comp_div_ok$Rspe~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_div_ok$BergerParker~comp_metric_ok$cat)
DunnTest(comp_div_ok$BergerParker~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_div_ok$Pielou~comp_metric_ok$cat)
DunnTest(comp_div_ok$Pielou~comp_metric_ok$cat,method = "BH")

kruskal.test(comp_div_ok$Shannon~comp_metric_ok$cat)
DunnTest(comp_div_ok$Shannon~comp_metric_ok$cat,method = "BH")



ggplot(filter(datal,cluster == 1))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#F8766D")+
  facet_wrap(~var,scales = "free_y")

comp_mt_cl <- filter(comp_div_ok, cluster == 1)
kruskal.test(comp_mt_cl$Rspe~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Rspe~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$BergerParker~comp_mt_cl$cat)
DunnTest(comp_mt_cl$BergerParker~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Pielou~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Pielou~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Shannon~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Shannon~comp_mt_cl$cat,method = "BH")


ggplot(filter(datal,cluster == 2))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  facet_wrap(~var,scales = "free_y")

comp_mt_cl <- filter(comp_div_ok, cluster == 2)
kruskal.test(comp_mt_cl$Rspe~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Rspe~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$BergerParker~comp_mt_cl$cat)
DunnTest(comp_mt_cl$BergerParker~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Pielou~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Pielou~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Shannon~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Shannon~comp_mt_cl$cat,method = "BH")


ggplot(filter(datal,cluster == 3))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00BE67")+
  facet_wrap(~var,scales = "free_y")

comp_mt_cl <- filter(comp_div_ok, cluster == 3)
kruskal.test(comp_mt_cl$Rspe~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Rspe~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$BergerParker~comp_mt_cl$cat)
DunnTest(comp_mt_cl$BergerParker~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Pielou~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Pielou~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Shannon~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Shannon~comp_mt_cl$cat,method = "BH")

# Doing it with abiotic data
# Import data
data_abio <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                        delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                            grouping_mark = ""), trim_ws = TRUE)
data_abio <- dplyr::select(data_abio,ID.interne.passage,CHLOROA:`TURB-FNU`)

comp_abio_ok <- left_join(comp_metric_ok,data_abio)
comp_abio_ok <- dplyr::select(comp_abio_ok,-(N_noeuds:N_clust))

comp_abio_ok$cat <- as.factor(comp_abio_ok$cat)
levels(comp_abio_ok$cat)
comp_abio_ok$cat <- fct_relevel(comp_abio_ok$cat,c("Avant","Pendant","Apres"))
datal <- pivot_longer(comp_abio_ok,cols = CHLOROA:`TURB-FNU`,names_to = "var")

ggplot(datal)+
  geom_boxplot(aes(x=cat, y= value,group=cat))+
  facet_wrap(~var,scales = "free_y")

datal$Phylum <- "Bacillariophyceae"

bacabio1 <- ggplot(filter(datal,cluster == 1 & ! var %in% c("TEMP","TURB") ))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#F8766D")+
  facet_wrap(Phylum~var,scales = "free_y",ncol=4)+
  labs(x="Moment",y="Mesure")+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold",size=8),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1, size = 10))

bacabio2 <- ggplot(filter(datal,cluster == 2 & ! var %in% c("TEMP","TURB") ))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  facet_wrap(Phylum~var,scales = "free_y",ncol=4)+
  labs(x="Moment",y="Mesure")+
  geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 2 & Bloom == "Dino-Lepidodinium" & ! var %in% c("TEMP","TURB")),size=2,shape=8)+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold",size=8),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1, size = 10))

bacabio3 <- ggplot(filter(datal,cluster == 3 & ! var %in% c("TEMP","TURB") ))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00BE67")+
  facet_wrap(Phylum~var,scales = "free_y",ncol=4)+
  labs(x="Moment",y="Mesure")+
  geom_point(aes(x=cat,y=value),data = filter(datal, cluster == 3 & Bloom == "Dino-Lepidodinium" & ! var %in% c("TEMP","TURB")),size=2,shape=8)+
  theme(strip.background = element_rect(fill = "#56B4E9"),strip.text = element_text(face = "bold",size = 8),
        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust = 1, size = 10))

bacabio <- plot_grid(bacabio1,bacabio2,bacabio3,ncol=3,labels = "AUTO")

plot_grid(bacabio,bacdiv,ncol=1)

kruskal.test(comp_abio_ok$TEMP~comp_abio_ok$cat)
DunnTest(comp_abio_ok$TEMP~comp_abio_ok$cat,method = "BH")

kruskal.test(comp_abio_ok$SALI~comp_abio_ok$cat)
DunnTest(comp_abio_ok$SALI~comp_abio_ok$cat,method = "BH")

kruskal.test(comp_abio_ok$CHLOROA~comp_abio_ok$cat)
DunnTest(comp_abio_ok$CHLOROA~comp_abio_ok$cat,method = "BH")

kruskal.test(comp_abio_ok$NH4~comp_abio_ok$cat)
DunnTest(comp_abio_ok$NH4~comp_abio_ok$cat,method = "BH")

kruskal.test(comp_abio_ok$`NO3+NO2`~comp_abio_ok$cat)
DunnTest(comp_abio_ok$`NO3+NO2`~comp_abio_ok$cat,method = "BH")

kruskal.test(comp_abio_ok$OXYGENE~comp_abio_ok$cat)
DunnTest(comp_abio_ok$OXYGENE~comp_abio_ok$cat,method = "BH")

kruskal.test(comp_abio_ok$PO4~comp_abio_ok$cat)
DunnTest(comp_abio_ok$PO4~comp_abio_ok$cat,method = "BH")

kruskal.test(comp_abio_ok$SIOH~comp_abio_ok$cat)
DunnTest(comp_abio_ok$SIOH~comp_abio_ok$cat,method = "BH")

kruskal.test(comp_abio_ok$TURB~comp_abio_ok$cat)
DunnTest(comp_abio_ok$TURB~comp_abio_ok$cat,method = "BH")

kruskal.test(comp_abio_ok$`TURB-FNU`~comp_abio_ok$cat)
DunnTest(comp_abio_ok$`TURB-FNU`~comp_abio_ok$cat,method = "BH")


ggplot(filter(datal,cluster == 1))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#F8766D")+
  facet_wrap(~var,scales = "free_y")

comp_mt_cl <- filter(comp_abio_ok, cluster == 1)
kruskal.test(comp_mt_cl$TEMP~comp_mt_cl$cat)
DunnTest(comp_mt_cl$TEMP~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$SALI~comp_mt_cl$cat)
DunnTest(comp_mt_cl$SALI~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$CHLOROA~comp_mt_cl$cat)
DunnTest(comp_mt_cl$CHLOROA~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$NH4~comp_mt_cl$cat)
DunnTest(comp_mt_cl$NH4~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$`NO3+NO2`~comp_mt_cl$cat)
DunnTest(comp_mt_cl$`NO3+NO2`~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$OXYGENE~comp_mt_cl$cat)
DunnTest(comp_mt_cl$OXYGENE~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$PO4~comp_mt_cl$cat)
DunnTest(comp_mt_cl$PO4~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$SIOH~comp_mt_cl$cat)
DunnTest(comp_mt_cl$SIOH~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$TURB~comp_mt_cl$cat)
DunnTest(comp_mt_cl$TURB~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$`TURB-FNU`~comp_mt_cl$cat)
DunnTest(comp_mt_cl$`TURB-FNU`~comp_mt_cl$cat,method = "BH")


ggplot(filter(datal,cluster == 2))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#CD9600")+
  facet_wrap(~var,scales = "free_y")

comp_mt_cl <- filter(comp_abio_ok, cluster == 2)
kruskal.test(comp_mt_cl$TEMP~comp_mt_cl$cat)
DunnTest(comp_mt_cl$TEMP~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$SALI~comp_mt_cl$cat)
DunnTest(comp_mt_cl$SALI~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$CHLOROA~comp_mt_cl$cat)
DunnTest(comp_mt_cl$CHLOROA~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$NH4~comp_mt_cl$cat)
DunnTest(comp_mt_cl$NH4~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$`NO3+NO2`~comp_mt_cl$cat)
DunnTest(comp_mt_cl$`NO3+NO2`~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$OXYGENE~comp_mt_cl$cat)
DunnTest(comp_mt_cl$OXYGENE~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$PO4~comp_mt_cl$cat)
DunnTest(comp_mt_cl$PO4~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$SIOH~comp_mt_cl$cat)
DunnTest(comp_mt_cl$SIOH~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$TURB~comp_mt_cl$cat)
DunnTest(comp_mt_cl$TURB~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$`TURB-FNU`~comp_mt_cl$cat)
DunnTest(comp_mt_cl$`TURB-FNU`~comp_mt_cl$cat,method = "BH")

ggplot(filter(datal,cluster == 3))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00BE67")+
  facet_wrap(~var,scales = "free_y")

comp_mt_cl <- filter(comp_abio_ok, cluster == 3)
kruskal.test(comp_mt_cl$TEMP~comp_mt_cl$cat)
DunnTest(comp_mt_cl$TEMP~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$SALI~comp_mt_cl$cat)
DunnTest(comp_mt_cl$SALI~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$CHLOROA~comp_mt_cl$cat)
DunnTest(comp_mt_cl$CHLOROA~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$NH4~comp_mt_cl$cat)
DunnTest(comp_mt_cl$NH4~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$`NO3+NO2`~comp_mt_cl$cat)
DunnTest(comp_mt_cl$`NO3+NO2`~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$OXYGENE~comp_mt_cl$cat)
DunnTest(comp_mt_cl$OXYGENE~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$PO4~comp_mt_cl$cat)
DunnTest(comp_mt_cl$PO4~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$SIOH~comp_mt_cl$cat)
DunnTest(comp_mt_cl$SIOH~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$TURB~comp_mt_cl$cat)
DunnTest(comp_mt_cl$TURB~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$`TURB-FNU`~comp_mt_cl$cat)
DunnTest(comp_mt_cl$`TURB-FNU`~comp_mt_cl$cat,method = "BH")


# Correlation between graph metrics and diversity indexes #####
# Import data 
metric <- read_delim("data_modif/metrics_final.csv", 
                     delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                     locale = locale(decimal_mark = ",", grouping_mark = "."), 
                     trim_ws = TRUE)

data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)
data <- dplyr::select(data, Code_point_Libelle, Date, cluster, Bloom_Phylum,ID.interne.passage, Shannon:Rspe)

data_met <- left_join(metric,data)

# There are duplicates to delete
doublons <- data_met[duplicated(data_met$ID.interne.passage) |
                       duplicated(data_met$ID.interne.passage, fromLast = TRUE), ]

resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_met, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

data_met <- data_join

doublons <- data_met[duplicated(data_met$ID.interne.passage) |
                       duplicated(data_met$ID.interne.passage, fromLast = TRUE), ]

resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_met, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

data_met <- data_join


doublons <- data_met[duplicated(data_met$ID.interne.passage) |
                       duplicated(data_met$ID.interne.passage, fromLast = TRUE), ]

resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

data_unique <- subset(data_met, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
data_join <- bind_rows(data_unique,resultat_filtre)
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

data_met <- data_join

data_met <- filter(data_met, Bloom_Phylum == "Bac" | Bloom_Phylum == "Dino" | is.na(Bloom_Phylum))
data_met[is.na(data_met$Bloom_Phylum),]$Bloom_Phylum <- "Non"

Table.corr_all <- dplyr::select(data_met,Shannon, Pielou, Rspe, BergerParker, Assort,Avg_p_length,C_tance,D_liens,
                                Mod,Nat_connect,N_noeuds,Adhes)
Table.corr_all[Table.corr_all == Inf] <- NA
Table.corr_all.comp <- Table.corr_all[complete.cases(Table.corr_all),]

r <- cor(Table.corr_all.comp)

# ... : Arguments supplémentaire à passer à la fonction cor.test
cor.mtest <- function(Table.corr_all.comp, ...) {
  mat <- as.matrix(Table.corr_all.comp)
  n <- ncol(Table.corr_all.comp)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# Matrice de p-value de la corrélation
p.mat <- cor.mtest(Table.corr_all.comp)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(r, method="color", col=col(200),  
         type="upper", order="alphabet",number.cex = 0.7,
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=F, 
         title = "Correlation entre métriques et diversité"
)


