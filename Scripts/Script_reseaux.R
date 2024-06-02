# Script JY.Dias - Stage M2 #

# Load packages
library(readr)
library(NetCoMi)
library(dplyr)
library(igraph)
library(highcharter)
library(pulsar)
library(lubridate)
library(tidyr)
library(DescTools)
library(FactoMineR)
library(factoextra)
library(missMDA)
library(vegan)
library(corrplot)
library(cowplot)

# Import data
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

# Compute the global network #####
### Cluster 1 ####
# Select cluster 1
CL1 <- filter(data, cluster == 1 )

# Select all genus
CL1 <- dplyr::select(CL1,Actinoptychus:Coscinodiscophycidae)

# Replace NA's by 0 to make it uniform
CL1[is.na(CL1)]<- 0

CL1 <- as.matrix(CL1)

# Build the global network
net_cluster1 <- netConstruct(data = CL1, dataType = "counts",measure = "spearman", 
                             filtTax = "numbSamp",filtTaxPar = list(numbSamp = 30),
                             filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                             normMethod = "none", dissFunc = "signed")

# Build info about the graph
net_props_cluster1 <- netAnalyze(net_cluster1,
                                 clustMethod = "cluster_fast_greedy",
                                 hubPar = c("eigenvector"),
                                 graphlet = F,
                                 connectivity = T)
# Plot the network
plt <- plot(net_props_cluster1,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Cluster 1",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edCL1ilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

### Cluster 2 ####
# Select cluster 2
CL2 <- filter(data, cluster == 2 )

# Select all genus
CL2 <- dplyr::select(CL2,Actinoptychus:Coscinodiscophycidae)

# Replace NA's by 0 to make it uniform
CL2[is.na(CL2)]<- 0

CL2 <- as.matrix(CL2)

# Build the global network
net_cluster2 <- netConstruct(data = CL2, dataType = "counts",measure = "spearman", 
                             filtTax = "numbSamp",filtTaxPar = list(numbSamp = 30),
                             filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                             normMethod = "none", dissFunc = "signed")

# Build info about the graph
net_props_cluster2 <- netAnalyze(net_cluster2,
                                 clustMethod = "cluster_fast_greedy",
                                 hubPar = c("eigenvector"),
                                 graphlet = F,
                                 connectivity = T)
# Plot the network
plt <- plot(net_props_cluster2,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Cluster 2",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edCL1ilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

### Cluster 3 ####
# Select cluster 3
CL3 <- filter(data, cluster == 3 )

# Select all genus
CL3 <- dplyr::select(CL3,Actinoptychus:Coscinodiscophycidae)

# Replace NA's by 0 to make it uniform
CL3[is.na(CL3)]<- 0

CL3 <- as.matrix(CL3)

# Build the global network
net_cluster3 <- netConstruct(data = CL3, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 30),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

# Build info about the graph
net_props_cluster3 <- netAnalyze(net_cluster3,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)
# Plot the network
plt <- plot(net_props_cluster3,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Cluster 3",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edCL1ilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

#### Working on the cluster 1 #####

# We keep only positive correlations
assoMat <- net_cluster1$assoMat1
assoMat[assoMat < 0] <- 0

cluster1 <- graph_from_adjacency_matrix(assoMat,weighted = T,mode = "undirected",diag=F)

# Compute clustering on the graph
wcini <- cluster_fast_greedy(cluster1)

# Manip to have the composition of the graph in term of phyla
# Creating a df to store the results
compo_reseau <- c("","")
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau <- wcini$names
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau$cluster <- wcini$membership
colnames(compo_reseau)[1] <- "Taxon"
# Import the taxonomic information
PhyClasse <- read.csv('data_modif/Liste_phylum.classe_REPHY_JYmodif.csv', sep =';', header = TRUE, fileEncoding = 'ISO-8859-1')
compo_reseau <- left_join(compo_reseau,PhyClasse)
compo_reseau$Nb <- 1

compo_reseau2 <- compo_reseau |>
  group_by(cluster,Phylum.Classe) |>
  summarise(Nb = sum(Nb))
write.csv2(compo_reseau2,file="data_modif/compo_reseau_cluster1.csv", row.names = FALSE,dec = ".")

compo_reseau$color <- "grey"
compo_reseau$color[compo_reseau$Phylum.Classe == "Bacillariophyceae"] <- "#56B4E9"
compo_reseau$color[compo_reseau$Phylum.Classe == "Dinophyceae"] <- "#009E73"
compo_reseau$color[compo_reseau$Phylum.Classe == "Ciliophora"] <-"#F0E442"
compo_reseau$color[compo_reseau$Phylum.Classe == "Cryptophyceae"] <-"#CC79A7"
compo_reseau$color[compo_reseau$Phylum.Classe == "Haptophyta"] <-"#996136"

dataabd <- as.data.frame(colSums(CL1))
dataabd$Taxon <- rownames(dataabd)
colnames(dataabd)[1] <- "Abd"
compo_reseau_clus1 <- left_join(compo_reseau,dataabd)


# Personalize the graph
V(cluster1)$label <- V(cluster1)$name
#V(cluster1)$name <- paste("I'm #", net$edgelist1$v1)
V(cluster1)$Phylum <- compo_reseau_clus1$Phylum.Classe
V(cluster1)$N_liens <- degree(cluster1)
V(cluster1)$size <- log(compo_reseau_clus1$Abd)
V(cluster1)$Module <- membership(wcini)
V(cluster1)$color <- compo_reseau$color
E(cluster1)$width <- E(cluster1)$weight*6
E(cluster1)$color <- "black"
layout_fr <- layout_with_fr(cluster1)
cluster1$layout <- layout_fr

# Plot it
# Other way
viz1 <- hchart(cluster1)
viz1
# Save it
#htmlwidgets::saveWidget(viz1, "output/graphs/Reseaux/HTML/cluster1.html")

# Global network metrics
S_net <-  vcount(cluster1) # Number of nodes
L_net <- ecount(cluster1) # Number of links

Z_mat <- L_net / S_net # linkage density or average number of links per nodes

C_net <- edge_density(cluster1, loops = FALSE) #connectance

# Average path length
avg_path_length <- mean_distance(
  cluster1,
  directed = FALSE,
  unconnected = TRUE # if the graphs is disconnected, only the existing paths are considered
)

Edge_connect <- edge.connectivity(cluster1) # Edge connectivity = adhesion

Modularity <- modularity(cluster1,membership = membership(wcini)) # Modularity

m_degree <- mean(degree(cluster1)) #Mean number of links per node

assort <- assortativity_degree(cluster1,directed = F) #assortativity

diss <- mean(1 - E(cluster1)$weight) # Dissilarity as defined in NetCoMi

trans <- transitivity(cluster1,type = "global") #Transitivity

mean_edge_bet <- mean(edge_betweenness(cluster1)) # Mean edge betweeness

# Compute the natural connectivity 
adj <- as.matrix(as_adjacency_matrix(cluster1, attr = "weight",)) # OK
diag(adj) <- 1
nat_connect <- natural.connectivity(as.matrix(adj)) # Natural connectivity

# Define the different hubs
hubs <- eigen_centrality(
  cluster1,
  directed = FALSE,
  scale = TRUE,
  weights = NULL
)
Hubs <- as.data.frame(hubs$vector)
Hubs$Phyto <- rownames(Hubs)

Spe_hubs <- Hubs[order(desc(hubs$vector)),]
# Store the result
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster1.csv", row.names = FALSE,dec = ".")

##### Compute subgraph by day #######
# Preparation  
# Index of species
nodes_net <- V(cluster1)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

# Table preparation to retrieve station/date information
CL1df <- filter(data,cluster == 1)

# Creating a df to store the results
data_results_reseaux <- c("","")
data_results_reseaux <- as.data.frame(data_results_reseaux)

# Compute the subgraphs
for (i in 1:nrow(CL1)){ 
  spe <- as.data.frame(CL1[i,])
  colnames(spe) <- "Count" 
  spe$phyto <- rownames(spe)
  spe <- left_join(phyto_index,spe, by = join_by(phyto))
  spe <- filter(spe,Count>0)
  spe$Pindex <- as.numeric(spe$Pindex)
  
  station <- CL1df[i,]$Code_point_Libelle
  date <- CL1df[i,]$Date
  
  if (nrow(spe) != 0){
  vids <- spe$Pindex
  sub <- igraph::subgraph(cluster1, vids)
  viz_sub <- hchart(sub, layout = layout_with_fr)
  
  # Metrics 
  S_net <-  vcount(sub) # Number of nodes
  L_net <- ecount(sub) # Number of links
  Z_mat <- L_net / S_net # linkage density or average number of links per nodes
  C_net <- edge_density(sub, loops = FALSE) #connectance
  
  # Average path length
  avg_path_length <- mean_distance(
    sub,
    directed = FALSE,
    unconnected = TRUE # if the graphs is disconnected, only the existing paths are considered
  )
  
  Edge_connect <- edge.connectivity(sub) # Edge connectivity = adhesion
  
  wc <- cluster_fast_greedy(sub)
  Modularity <- modularity(sub,membership = membership(wc)) # Modularity
  
  Vert_connect <- vertex.connectivity(sub) # Vertex connectivity = adhesion
  m_degree <- mean(degree(sub)) # mean number of links
  assort <- assortativity_degree(sub,directed = F) #assortativity
  diss <- mean(1 - E(sub)$weight) # Dissilarity as defined in NetCoMi
  trans <- transitivity(sub,type = "global") #Transitivity
  mean_edge_bet <- mean(edge_betweenness(sub)) # Mean edge betweeness
  
  adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
  diag(adj) <- 1
  nat_connect <- natural.connectivity(as.matrix(adj)) # natural connectivity
  
  data_results_reseaux[i,1] <- station
  data_results_reseaux[i,2] <- date
  data_results_reseaux[i,3] <- S_net # nombre de noeuds
  data_results_reseaux[i,4] <- L_net # nombre de liens
  data_results_reseaux[i,5] <- Z_mat # Densite des liens
  data_results_reseaux[i,6] <- C_net # connectance
  
  data_results_reseaux[i,7] <- avg_path_length # longueur moyen des liens
  data_results_reseaux[i,8] <- Edge_connect # adhesion
  data_results_reseaux[i,9] <- Modularity #modularite
  
  data_results_reseaux[i,10] <- m_degree #Nombre de liens moyens
  data_results_reseaux[i,11] <- assort # Assortativite
  data_results_reseaux[i,12] <- diss # dissimilarite
  data_results_reseaux[i,13] <- trans # transitivite
  
  data_results_reseaux[i,14] <- mean_edge_bet # Nombre moyen de voisins
  data_results_reseaux[i,15] <- nat_connect # Connectivite naturelle
  
  data_results_reseaux[i,16] <- length(wc) # Nombre de cluster
  } 
  else { 
    data_results_reseaux[i,1] <- station
    data_results_reseaux[i,2] <- date
    data_results_reseaux[i,3] <- NA # nombre de noeuds
    data_results_reseaux[i,4] <- NA # nombre de liens
    data_results_reseaux[i,5] <- NA # Densite des liens
    data_results_reseaux[i,6] <- NA # connectance
    
    data_results_reseaux[i,7] <- NA # longueur moyen des liens
    data_results_reseaux[i,8] <- NA # adhesion
    data_results_reseaux[i,9] <- NA #modularite
    
    data_results_reseaux[i,10] <- NA #Nombre de liens moyens
    data_results_reseaux[i,11] <- NA # Assortativite
    data_results_reseaux[i,12] <- NA # dissimilarite
    data_results_reseaux[i,13] <- NA # transitivite
    
    data_results_reseaux[i,14] <- NA # Nombre moyen de voisins
    data_results_reseaux[i,15] <- NA # Connectivite naturelle
    
    data_results_reseaux[i,16] <- NA # Nombre de cluster
    
    }
  
  colnames(data_results_reseaux) <- c("Code_point_Libelle","Date","N_noeuds","N_liens","D_liens","C_tance",
                                      "Avg_p_length","Adhes","Mod","meanN_liens","Assort","Diss","Trans","meanN_voisins",
                                      "Nat_connect","N_clust")
  
  #plot(sub,main = paste0(station,date),layout = layout_with_fr)
  #png(paste0("output/graphs/Reseaux/TS_CLUST1/",station,date,".png"))
  #plot(sub,main = paste0(station,date),layout = layout_with_fr)
  #dev.off()
  #htmlwidgets::saveWidget(viz_sub, paste0("output/graphs/Reseaux/HTML/",station,date,".html"))
  #print(i/nrow(CL1)*100)
}
# Save the result
write.csv2(data_results_reseaux,file="data_modif/results_metrics_reseaux_cluster1_pos.csv", row.names = FALSE,dec = ".")

# Result analysis
data_results_reseaux <- read_delim("data_modif/results_metrics_reseaux_cluster1_pos.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)
data_results_reseaux_copy <- data_results_reseaux

bloom <- dplyr::select(CL1df,Code_point_Libelle,Date,Bloom_Phylum,P_dominance)

data_reseaux <- left_join(data_results_reseaux,bloom)

# Appently there are duplicates so we delete them
data_reseaux <- data_reseaux[-c(3207:3209),]


data_reseaux <- data_reseaux %>%
  mutate(Month = month(Date, label = F)) |>
  mutate(Year = year(Date))

data_reseaux$Date2 <- as.Date(paste(data_reseaux$Year, data_reseaux$Month, "01", sep = "-"), format = "%Y-%m-%d")

datam <- summarise(group_by(data_reseaux,Code_point_Libelle,Month,Year), N_noeuds=mean(N_noeuds,na.rm=T),N_liens=mean(N_liens,na.rm=T),
                   D_liens=mean(D_liens,na.rm=T),C_tance=mean(C_tance,na.rm=T),LML=mean(Avg_p_length,na.rm=T),
                   Adhes=mean(Adhes,na.rm=T),Mod=mean(Mod,na.rm=T),meanN_liens=mean(meanN_liens,na.rm=T),
                   Assort=mean(Assort,na.rm=T),Diss=mean(Diss,na.rm=T),Trans=mean(Trans,na.rm=T),
                   meanN_voisins=mean(meanN_voisins,na.rm=T),Nat_connect=mean(Nat_connect,na.rm=T),N_clust=mean(N_clust,na.rm=T))

datamcluster1 <- datam
# Metrics seasonalities
datal <- pivot_longer(datamcluster1,names_to = "Var",cols = N_noeuds:N_clust)
datal2 <- filter(datal, Var %in% c("Assort","LML","C_tance","D_liens","Mod","Nat_connect","N_noeuds","Adhes"))
ggplot(datal)+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#F8766D",size = 1)+
  facet_wrap(~Var,scales = "free_y",ncol=2)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
#ggsave('Metriques_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')

datal2$cluster <- "1-Méditerranée"

# Create the graph I wanted
nnoeuds <- ggplot(filter(datal2,Var == "N_noeuds"))+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#F8766D",size = 0.5)+
  facet_wrap(cluster~Var,scales = "free_y",ncol=2)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  scale_y_continuous(breaks = seq(0,40,10),limits = c(0,40))+
  labs(x=NULL,y="")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 7),
        strip.background = element_rect(fill = NULL))

LML <- ggplot(filter(datal2,Var == "LML"))+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#F8766D",size = 0.5)+
  facet_wrap(cluster~Var,scales = "free_y",ncol=2)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  scale_y_continuous(breaks = seq(0,0.5,0.05),limits = c(0,0.5))+
  labs(x=NULL,y=NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 7),
        strip.background = element_rect(fill = NULL))

assort <- ggplot(filter(datal2,Var == "Assort"))+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#F8766D",size = 0.5)+
  facet_wrap(cluster~Var,scales = "free_y",ncol=2)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  scale_y_continuous(breaks = seq(-1,0.6,0.2),limits = c(-1,0.6))+
  labs(x=NULL,y=NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 7),
        strip.background = element_rect(fill = NULL))

natconnect <- ggplot(filter(datal2,Var == "Nat_connect"))+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#F8766D",size = 0.5)+
  facet_wrap(cluster~Var,scales = "free_y",ncol=2)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  scale_y_continuous(breaks = seq(0,0.7,0.1),limits = c(0,0.7))+
  labs(x=NULL,y=NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 7),
        strip.background = element_rect(fill = NULL))

ctance <- ggplot(filter(datal2,Var == "C_tance"))+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#F8766D",size = 0.5)+
  facet_wrap(cluster~Var,scales = "free_y",ncol=2)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1))+
  labs(x=NULL,y=NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 7),
        strip.background = element_rect(fill = NULL))

mod <- ggplot(filter(datal2,Var == "Mod"))+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#F8766D",size = 0.5)+
  facet_wrap(cluster~Var,scales = "free_y",ncol=2)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  scale_y_continuous(breaks = seq(-0.3,0.4,0.2),limits = c(-0.3,0.4))+
  labs(x=NULL,y=NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 7),
        strip.background = element_rect(fill = NULL))

saisonmetric1 <- plot_grid(nnoeuds,natconnect,LML,ctance,assort,mod,ncol = 6)



# Trends
ggplot(datal)+
  geom_boxplot(aes(x=Year,y=value,group=Year),fill="#F8766D",size = 1)+
  facet_wrap(~Var,scales = "free_y")+
  labs(title = "Metriques réseau cluster 1 (+)")
#ggsave('Metriques_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')



CL1_Mmetrics <- summarise(group_by(data_reseaux,Code_point_Libelle,Date2),N_noeuds=mean(N_noeuds,na.rm=T),N_liens=mean(N_liens,na.rm=T),
                          D_liens=mean(D_liens,na.rm=T),C_tance=mean(C_tance,na.rm=T),Avg_p_length=mean(Avg_p_length,na.rm=T),
                          Adhes=mean(Adhes,na.rm=T),Mod=mean(Mod,na.rm=T),meanN_liens=mean(meanN_liens,na.rm=T),
                          Assort=mean(Assort,na.rm=T),Diss=mean(Diss,na.rm=T),Trans=mean(Trans,na.rm=T),
                          meanN_voisins=mean(meanN_voisins,na.rm=T),Nat_connect=mean(Nat_connect,na.rm=T),N_clust=mean(N_clust,na.rm=T))

# Time series (boxplot by year and mounth)
datal2 <- pivot_longer(CL1_Mmetrics,names_to = "Var",cols = N_noeuds:N_clust)
ggplot(datal2)+
  geom_boxplot(aes(x=Date2,y=value,group=Date2),fill="#00BE67",size = 1)+
  facet_wrap(~Var,scales = "free_y")+
  labs(title = "Metriques réseau cluster 1 (+)")
#ggsave('Metriques_cluster_ts.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')


##### Subgraph by season ####
data<- data |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))

# Winter #
CL1 <- filter(data, cluster == 1,season == "Winter" )
nbdate <- nrow(CL1)*0.33
CL1 <- dplyr::select(CL1,Actinoptychus:Coscinodiscophycidae)

CL1[is.na(CL1)]<- 0
CL1 <- ifelse(CL1 > 0, 1,0)
Spe_w <- as.data.frame(colSums(CL1))

# Prepare the list of the present genus
Spe_w$Phyto <- rownames(Spe_w)
colnames(Spe_w)[1] <- "V1"
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- filter(Spe_w,V1 > nbdate)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index of the species
nodes_net <- V(cluster1)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)
Spe <- filter(Spe,!is.na(Spe$Count))

# Compute the subgraph
# Compute the subgraph
vids <- Spe$Pindex
compo_reseau_clus1$Pres <- "Non"
compo_reseau_clus1[vids,]$Pres <- "Oui"
compo_reseau_clus1$color <- "grey"
compo_reseau_clus1$color[compo_reseau$Phylum.Classe == "Bacillariophyceae"] <- "#56B4E9"
compo_reseau_clus1$color[compo_reseau$Phylum.Classe == "Dinophyceae"] <- "#009E73"
compo_reseau_clus1$color[compo_reseau$Phylum.Classe == "Ciliophora"] <-"#F0E442"
compo_reseau_clus1$color[compo_reseau$Phylum.Classe == "Cryptophyceae"] <-"#CC79A7"
compo_reseau_clus1$color[compo_reseau$Phylum.Classe == "Haptophyta"] <-"#996136"
compo_reseau_clus1$color[compo_reseau$Pres == "Non"] <- "transparent"

present <- compo_reseau_clus1$Taxon[compo_reseau$Pres == "Oui"]

dataabd <- as.data.frame(colSums(CL1))
dataabd$Taxon <- rownames(dataabd)
colnames(dataabd)[1] <- "Abd"
compo_reseau <- left_join(compo_reseau_clus1,dataabd)

liens_df <- get.data.frame(cluster1, what = "edges")
liens_df$color <- "black"
liens_df$color[!liens_df$from %in% present  ] <- "transparent"
liens_df$color[!liens_df$to %in% present  ] <- "transparent"

# Personalize the graph
V(cluster1)$label <- V(cluster1)$name
V(cluster1)$Phylum <- compo_reseau$Phylum.Classe
V(cluster1)$N_liens <- degree(cluster1)
V(cluster1)$size <- log(compo_reseau$Abd)
V(cluster1)$Module <- membership(wcini)
V(cluster1)$color <- compo_reseau$color
E(cluster1)$width <- E(cluster1)$weight*6
E(cluster1)$color <- liens_df$color
cluster1$layout <- layout_fr

sub <- igraph::subgraph(cluster1, vids)
sub$layout <- layout_fr[vids,]
viz_sub <- hchart(cluster1)
plot(sub)
viz_sub

# Calculate the metrics
vcount(sub) # Number of nodes
ecount(sub) # Number of links
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = TRUE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

# Compute the clustering on the graph
wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) # mean number of links
assortativity_degree(sub,directed = F) #assortativity
mean(1 - E(sub)$weight) # Dissilarity as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # natural connectivity

# Compute the hubs
hubs <- eigen_centrality(
  sub,
  directed = FALSE,
  scale = TRUE,
  weights = NULL
)
Hubs <- as.data.frame(hubs$vector)
Hubs$Phyto <- rownames(Hubs)

Spe_hubs <- Hubs[order(desc(hubs$vector)),]
Spe_hubs
# Store the results 
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster1_hiver.csv", row.names = FALSE,dec = ".")

# Manip to have the composition of the graph in term of phyla
# Creating a df to store the results
compo_reseau <- c("","")
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau <- wcini$names
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau$cluster <- wcini$membership
colnames(compo_reseau)[1] <- "Taxon"
# Import the taxonomic information
PhyClasse <- read.csv('data_modif/Liste_phylum.classe_REPHY_JYmodif.csv', sep =';', header = TRUE, fileEncoding = 'ISO-8859-1')
compo_reseau <- left_join(compo_reseau,PhyClasse)
compo_reseau$Nb <- 1
colnames(Spe_hubs)[2] <- "Taxon"
compo_reseau <- left_join(Spe_hubs,compo_reseau)

compo_reseau <- compo_reseau |>
  group_by(cluster,Phylum.Classe) |>
  summarise(Nb = sum(Nb))
write.csv2(compo_reseau,file="data_modif/compo_reseau_cluster1_hiver.csv", row.names = FALSE,dec = ".")



# Fall #
CL1 <- filter(data, cluster == 1,season == "Fall" )
nbdate <- nrow(CL1)*0.33
CL1 <- dplyr::select(CL1,Actinoptychus:Coscinodiscophycidae)

CL1[is.na(CL1)]<- 0
CL1 <- ifelse(CL1 > 0, 1,0)
Spe_w <- as.data.frame(colSums(CL1))

# Prepare the list of the present genus
Spe_w$Phyto <- rownames(Spe_w)
colnames(Spe_w)[1] <- "V1"
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- filter(Spe_w,V1 > nbdate)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index of the species
nodes_net <- V(cluster1)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)
Spe <- filter(Spe,!is.na(Spe$Count))

# Compute the subgraph
vids <- Spe$Pindex
compo_reseau_clus1$Pres <- "Non"
compo_reseau_clus1[vids,]$Pres <- "Oui"
compo_reseau_clus1$color <- "grey"
compo_reseau_clus1$color[compo_reseau$Phylum.Classe == "Bacillariophyceae"] <- "#56B4E9"
compo_reseau_clus1$color[compo_reseau$Phylum.Classe == "Dinophyceae"] <- "#009E73"
compo_reseau_clus1$color[compo_reseau$Phylum.Classe == "Ciliophora"] <-"#F0E442"
compo_reseau_clus1$color[compo_reseau$Phylum.Classe == "Cryptophyceae"] <-"#CC79A7"
compo_reseau_clus1$color[compo_reseau$Phylum.Classe == "Haptophyta"] <-"#996136"
compo_reseau_clus1$color[compo_reseau$Pres == "Non"] <- "transparent"

present <- compo_reseau_clus1$Taxon[compo_reseau_clus1$Pres == "Oui"]

dataabd <- as.data.frame(colSums(CL1))
dataabd$Taxon <- rownames(dataabd)
colnames(dataabd)[1] <- "Abd"
compo_reseau <- left_join(compo_reseau_clus1,dataabd)

liens_df <- get.data.frame(cluster1, what = "edges")
liens_df$color <- "black"
liens_df$color[!liens_df$from %in% present  ] <- "transparent"
liens_df$color[!liens_df$to %in% present  ] <- "transparent"

# Personalize the graph
V(cluster1)$label <- V(cluster1)$name
V(cluster1)$Phylum <- compo_reseau$Phylum.Classe
V(cluster1)$N_liens <- degree(cluster1)
V(cluster1)$size <- log(compo_reseau$Abd)
V(cluster1)$Module <- membership(wcini)
V(cluster1)$color <- compo_reseau$color
E(cluster1)$width <- E(cluster1)$weight*6
E(cluster1)$color <- liens_df$color
cluster1$layout <- layout_fr

sub <- igraph::subgraph(cluster1, vids)
sub$layout <- layout_fr[vids,]
viz_sub <- hchart(cluster1)
plot(sub)
viz_sub

vcount(sub) # Number of nodes
ecount(sub) # Number of links
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = TRUE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

# Compute the clustering on the graph
wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) # mean number of links
assortativity_degree(sub,directed = F) #assortativity
mean(1 - E(sub)$weight) # Dissilarity as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # natural connectivity

# Compute the hubs
hubs <- eigen_centrality(
  sub,
  directed = FALSE,
  scale = TRUE,
  weights = NULL
)
Hubs <- as.data.frame(hubs$vector)
Hubs$Phyto <- rownames(Hubs)

Spe_hubs <- Hubs[order(desc(hubs$vector)),]
Spe_hubs
# Store the results 
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster1_automne.csv", row.names = FALSE,dec = ".")

# Manip to have the composition of the graph in term of phyla
# Creating a df to store the results
compo_reseau <- c("","")
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau <- wcini$names
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau$cluster <- wcini$membership
colnames(compo_reseau)[1] <- "Taxon"
# Import the taxonomic information
PhyClasse <- read.csv('data_modif/Liste_phylum.classe_REPHY_JYmodif.csv', sep =';', header = TRUE, fileEncoding = 'ISO-8859-1')
compo_reseau <- left_join(compo_reseau,PhyClasse)
compo_reseau$Nb <- 1
colnames(Spe_hubs)[2] <- "Taxon"
compo_reseau <- left_join(Spe_hubs,compo_reseau)

compo_reseau <- compo_reseau |>
  group_by(cluster,Phylum.Classe) |>
  summarise(Nb = sum(Nb))
write.csv2(compo_reseau,file="data_modif/compo_reseau_cluster1_automne.csv", row.names = FALSE,dec = ".")

# Spring #
CL1 <- filter(data, cluster == 1,season == "Spring" )
nbdate <- nrow(CL1)*0.33
CL1 <- dplyr::select(CL1,Actinoptychus:Coscinodiscophycidae)

CL1[is.na(CL1)]<- 0
CL1 <- ifelse(CL1 > 0, 1,0)
Spe_w <- as.data.frame(colSums(CL1))

# Prepare the list of the present genus
Spe_w$Phyto <- rownames(Spe_w)
colnames(Spe_w)[1] <- "V1"
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- filter(Spe_w,V1 > nbdate)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index of the species
nodes_net <- V(cluster1)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)
Spe <- filter(Spe,!is.na(Spe$Count))

# Compute the subgraph
vids <- Spe$Pindex
compo_reseau_clus1$Pres <- "Non"
compo_reseau_clus1[vids,]$Pres <- "Oui"
compo_reseau_clus1$color <- "grey"
compo_reseau_clus1$color[compo_reseau_clus1$Phylum.Classe == "Bacillariophyceae"] <- "#56B4E9"
compo_reseau_clus1$color[compo_reseau_clus1$Phylum.Classe == "Dinophyceae"] <- "#009E73"
compo_reseau_clus1$color[compo_reseau_clus1$Phylum.Classe == "Ciliophora"] <-"#F0E442"
compo_reseau_clus1$color[compo_reseau_clus1$Phylum.Classe == "Cryptophyceae"] <-"#CC79A7"
compo_reseau_clus1$color[compo_reseau_clus1$Phylum.Classe == "Haptophyta"] <-"#996136"
compo_reseau_clus1$color[compo_reseau_clus1$Pres == "Non"] <- "transparent"

present <- compo_reseau$Taxon[compo_reseau$Pres == "Oui"]

dataabd <- as.data.frame(colSums(CL1))
dataabd$Taxon <- rownames(dataabd)
colnames(dataabd)[1] <- "Abd"
compo_reseau <- left_join(compo_reseau_clus1,dataabd)

liens_df <- get.data.frame(cluster1, what = "edges")
liens_df$color <- "black"
liens_df$color[!liens_df$from %in% present  ] <- "transparent"
liens_df$color[!liens_df$to %in% present  ] <- "transparent"

# Personalize the graph
V(cluster1)$label <- V(cluster1)$name
V(cluster1)$Phylum <- compo_reseau$Phylum.Classe
V(cluster1)$N_liens <- degree(cluster1)
V(cluster1)$size <- log(compo_reseau$Abd)
V(cluster1)$Module <- membership(wcini)
V(cluster1)$color <- compo_reseau$color
E(cluster1)$width <- E(cluster1)$weight*6
E(cluster1)$color <- liens_df$color
cluster1$layout <- layout_fr

sub <- igraph::subgraph(cluster1, vids)
sub$layout <- layout_fr[vids,]
viz_sub <- hchart(cluster1)
plot(sub)
viz_sub

vcount(sub) # Number of nodes
ecount(sub) # Number of links
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = TRUE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

# Compute the clustering on the graph
wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) # mean number of links
assortativity_degree(sub,directed = F) #assortativity
mean(1 - E(sub)$weight) # Dissilarity as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # natural connectivity

# Compute the hubs
hubs <- eigen_centrality(
  sub,
  directed = FALSE,
  scale = TRUE,
  weights = NULL
)
Hubs <- as.data.frame(hubs$vector)
Hubs$Phyto <- rownames(Hubs)

Spe_hubs <- Hubs[order(desc(hubs$vector)),]
Spe_hubs
# Store the results 
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster1_printemps.csv", row.names = FALSE,dec = ".")

# Manip to have the composition of the graph in term of phyla
# Creating a df to store the results
compo_reseau <- c("","")
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau <- wcini$names
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau$cluster <- wcini$membership
colnames(compo_reseau)[1] <- "Taxon"
# Import the taxonomic information
PhyClasse <- read.csv('data_modif/Liste_phylum.classe_REPHY_JYmodif.csv', sep =';', header = TRUE, fileEncoding = 'ISO-8859-1')
compo_reseau <- left_join(compo_reseau,PhyClasse)
compo_reseau$Nb <- 1
colnames(Spe_hubs)[2] <- "Taxon"
compo_reseau <- left_join(Spe_hubs,compo_reseau)

compo_reseau <- compo_reseau |>
  group_by(cluster,Phylum.Classe) |>
  summarise(Nb = sum(Nb))
write.csv2(compo_reseau,file="data_modif/compo_reseau_cluster1_printemps.csv", row.names = FALSE,dec = ".")

# Summer #
CL1 <- filter(data, cluster == 1,season == "Summer" )
nbdate <- nrow(CL1)*0.33
CL1 <- dplyr::select(CL1,Actinoptychus:Coscinodiscophycidae)

CL1[is.na(CL1)]<- 0
CL1 <- ifelse(CL1 > 0, 1,0)
Spe_w <- as.data.frame(colSums(CL1))

# Prepare the list of the present genus
Spe_w$Phyto <- rownames(Spe_w)
colnames(Spe_w)[1] <- "V1"
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- filter(Spe_w,V1 > nbdate)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index of the species
nodes_net <- V(cluster1)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)
Spe <- filter(Spe,!is.na(Spe$Count))

# Compute the subgraph
vids <- Spe$Pindex
compo_reseau_clus1$Pres <- "Non"
compo_reseau_clus1[vids,]$Pres <- "Oui"
compo_reseau_clus1$color <- "grey"
compo_reseau_clus1$color[compo_reseau_clus1$Phylum.Classe == "Bacillariophyceae"] <- "#56B4E9"
compo_reseau_clus1$color[compo_reseau_clus1$Phylum.Classe == "Dinophyceae"] <- "#009E73"
compo_reseau_clus1$color[compo_reseau_clus1$Phylum.Classe == "Ciliophora"] <-"#F0E442"
compo_reseau_clus1$color[compo_reseau_clus1$Phylum.Classe == "Cryptophyceae"] <-"#CC79A7"
compo_reseau_clus1$color[compo_reseau_clus1$Phylum.Classe == "Haptophyta"] <-"#996136"
compo_reseau_clus1$color[compo_reseau_clus1$Pres == "Non"] <- "transparent"

present <- compo_reseau_clus1$Taxon[compo_reseau_clus1$Pres == "Oui"]

dataabd <- as.data.frame(colSums(CL1))
dataabd$Taxon <- rownames(dataabd)
colnames(dataabd)[1] <- "Abd"
compo_reseau <- left_join(compo_reseau_clus1,dataabd)

liens_df <- get.data.frame(cluster1, what = "edges")
liens_df$color <- "black"
liens_df$color[!liens_df$from %in% present  ] <- "transparent"
liens_df$color[!liens_df$to %in% present  ] <- "transparent"

# Personalize the graph
V(cluster1)$label <- V(cluster1)$name
V(cluster1)$Phylum <- compo_reseau_clus1$Phylum.Classe
V(cluster1)$N_liens <- degree(cluster1)
V(cluster1)$size <- log(compo_reseau_clus1$Abd)
V(cluster1)$Module <- membership(wcini)
V(cluster1)$color <- compo_reseau$color
E(cluster1)$width <- E(cluster1)$weight*6
E(cluster1)$color <- liens_df$color
cluster1$layout <- layout_fr

sub <- igraph::subgraph(cluster1, vids)
sub$layout <- layout_fr[vids,]
viz_sub <- hchart(cluster1)
plot(sub)
viz_sub

vcount(sub) # Number of nodes
ecount(sub) # Number of links
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = TRUE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

# Compute the clustering on the graph
wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) # mean number of links
assortativity_degree(sub,directed = F) #assortativity
mean(1 - E(sub)$weight) # Dissilarity as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # natural connectivity

# Compute the hubs
hubs <- eigen_centrality(
  sub,
  directed = FALSE,
  scale = TRUE,
  weights = NULL
)
Hubs <- as.data.frame(hubs$vector)
Hubs$Phyto <- rownames(Hubs)

Spe_hubs <- Hubs[order(desc(hubs$vector)),]
Spe_hubs
# Store the results 
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster1_summer.csv", row.names = FALSE,dec = ".")

# Manip to have the composition of the graph in term of phyla
# Creating a df to store the results
compo_reseau <- c("","")
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau <- wcini$names
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau$cluster <- wcini$membership
colnames(compo_reseau)[1] <- "Taxon"
# Import the taxonomic information
PhyClasse <- read.csv('data_modif/Liste_phylum.classe_REPHY_JYmodif.csv', sep =';', header = TRUE, fileEncoding = 'ISO-8859-1')
compo_reseau <- left_join(compo_reseau,PhyClasse)
compo_reseau$Nb <- 1
colnames(Spe_hubs)[2] <- "Taxon"
compo_reseau <- left_join(Spe_hubs,compo_reseau)

compo_reseau <- compo_reseau |>
  group_by(cluster,Phylum.Classe) |>
  summarise(Nb = sum(Nb))
write.csv2(compo_reseau,file="data_modif/compo_reseau_cluster1_été.csv", row.names = FALSE,dec = ".")

#### Working on the cluster 2 #####

# We keep only positive correlations
assoMat <- net_cluster2$assoMat1
assoMat[assoMat < 0] <- 0

cluster2 <- graph_from_adjacency_matrix(assoMat,weighted = T,mode = "undirected",diag=F)

# Compute clustering on the graph
wcini <- cluster_fast_greedy(cluster2)

# Manip to have the composition of the graph in term of phyla
# Creating a df to store the results
compo_reseau <- c("","")
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau <- wcini$names
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau$cluster <- wcini$membership
colnames(compo_reseau)[1] <- "Taxon"
# Import the taxonomic information
PhyClasse <- read.csv('data_modif/Liste_phylum.classe_REPHY_JYmodif.csv', sep =';', header = TRUE, fileEncoding = 'ISO-8859-1')
compo_reseau <- left_join(compo_reseau,PhyClasse)
compo_reseau$Nb <- 1

compo_reseau2 <- compo_reseau |>
  group_by(cluster,Phylum.Classe) |>
  summarise(Nb = sum(Nb))
write.csv2(compo_reseau2,file="data_modif/compo_reseau_cluster2.csv", row.names = FALSE,dec = ".")

compo_reseau$color <- "grey"
compo_reseau$color[compo_reseau$Phylum.Classe == "Bacillariophyceae"] <- "#56B4E9"
compo_reseau$color[compo_reseau$Phylum.Classe == "Dinophyceae"] <- "#009E73"
compo_reseau$color[compo_reseau$Phylum.Classe == "Ciliophora"] <-"#F0E442"
compo_reseau$color[compo_reseau$Phylum.Classe == "Cryptophyceae"] <-"#CC79A7"
compo_reseau$color[compo_reseau$Phylum.Classe == "Haptophyta"] <-"#996136"

compo_reseau_clus2 <- compo_reseau

dataabd <- as.data.frame(colSums(CL2))
dataabd$Taxon <- rownames(dataabd)
colnames(dataabd)[1] <- "Abd"
compo_reseau <- left_join(compo_reseau_clus2,dataabd)


# Personalize the graph
V(cluster2)$label <- V(cluster2)$name
V(cluster2)$Phylum <- compo_reseau$Phylum.Classe
V(cluster2)$N_liens <- degree(cluster2)
V(cluster2)$size <- log(compo_reseau$Abd)
V(cluster2)$Module <- membership(wcini)
V(cluster2)$color <- compo_reseau$color
E(cluster2)$width <- E(cluster2)$weight*6
E(cluster2)$color <- "black"
layout_fr <- layout_with_fr(cluster2)
cluster2$layout <- layout_fr

# Plot it
# Other way
viz2 <- hchart(cluster2)
viz2


# Save it
#htmlwidgets::saveWidget(viz2, "output/graphs/Reseaux/HTML/cluster2.html")

# Global network metrics
S_net <-  vcount(cluster2) # Number of nodes
L_net <- ecount(cluster2) # Number of links

Z_mat <- L_net / S_net # linkage density or average number of links per nodes

C_net <- edge_density(cluster2, loops = FALSE) #connectance

# Average path length
avg_path_length <- mean_distance(
  cluster2,
  directed = FALSE,
  unconnected = TRUE # if the graphs is disconnected, only the existing paths are considered
)

Edge_connect <- edge.connectivity(cluster2) # Edge connectivity = adhesion

Modularity <- modularity(cluster2,membership = membership(wcini)) # Modularity

m_degree <- mean(degree(cluster2)) #Mean number of links per node

assort <- assortativity_degree(cluster2,directed = F) #assortativity

diss <- mean(1 - E(cluster2)$weight) # Dissilarity as defined in NetCoMi

trans <- transitivity(cluster2,type = "global") #Transitivity

mean_edge_bet <- mean(edge_betweenness(cluster2)) # Mean edge betweeness

# Compute the natural connectivity 
adj <- as.matrix(as_adjacency_matrix(cluster2, attr = "weight",)) # OK
diag(adj) <- 1
nat_connect <- natural.connectivity(as.matrix(adj)) # Natural connectivity

# Define the different hubs
hubs <- eigen_centrality(
  cluster2,
  directed = FALSE,
  scale = TRUE,
  weights = NULL
)
Hubs <- as.data.frame(hubs$vector)
Hubs$Phyto <- rownames(Hubs)

Spe_hubs <- Hubs[order(desc(hubs$vector)),]
# Store the result
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster2.csv", row.names = FALSE,dec = ".")

# Manip to have the composition of the graph in term of phyla
# Creating a df to store the results
compo_reseau <- c("","")
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau <- wcini$names
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau$cluster <- wcini$membership
colnames(compo_reseau)[1] <- "Taxon"
# Import the taxonomic information
PhyClasse <- read.csv('data_modif/Liste_phylum.classe_REPHY_JYmodif.csv', sep =';', header = TRUE, fileEncoding = 'ISO-8859-1')
compo_reseau <- left_join(compo_reseau,PhyClasse)
compo_reseau$Nb <- 1
colnames(Spe_hubs)[2] <- "Taxon"
compo_reseau <- left_join(Spe_hubs,compo_reseau)

compo_reseau <- compo_reseau |>
  group_by(cluster,Phylum.Classe) |>
  summarise(Nb = sum(Nb))
write.csv2(compo_reseau,file="data_modif/compo_reseau_cluster2.csv", row.names = FALSE,dec = ".")

##### Compute subgraph by day #######
# Preparation  
# Index of species
nodes_net <- V(cluster2)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

# Table preparation to retrieve station/date information
CL2df <- filter(data,cluster == 2)

# Creating a df to store the results
data_results_reseaux <- c("","")
data_results_reseaux <- as.data.frame(data_results_reseaux)

# Compute the subgraphs
for (i in 1:nrow(CL2)){ 
  spe <- as.data.frame(CL2[i,])
  colnames(spe) <- "Count" 
  spe$phyto <- rownames(spe)
  spe <- left_join(phyto_index,spe, by = join_by(phyto))
  spe <- filter(spe,Count>0)
  spe$Pindex <- as.numeric(spe$Pindex)
  
  station <- CL2df[i,]$Code_point_Libelle
  date <- CL2df[i,]$Date
  
  if (nrow(spe) != 0){
    vids <- spe$Pindex
    sub <- igraph::subgraph(cluster2, vids)
    viz_sub <- hchart(sub, layout = layout_with_fr)
    
    # Metrics 
    S_net <-  vcount(sub) # Number of nodes
    L_net <- ecount(sub) # Number of links
    Z_mat <- L_net / S_net # linkage density or average number of links per nodes
    C_net <- edge_density(sub, loops = FALSE) #connectance
    
    # Average path length
    avg_path_length <- mean_distance(
      sub,
      directed = FALSE,
      unconnected = TRUE # if the graphs is disconnected, only the existing paths are considered
    )
    
    Edge_connect <- edge.connectivity(sub) # Edge connectivity = adhesion
    
    wc <- cluster_fast_greedy(sub)
    Modularity <- modularity(sub,membership = membership(wc)) # Modularity
    
    Vert_connect <- vertex.connectivity(sub) # Vertex connectivity = adhesion
    m_degree <- mean(degree(sub)) # mean number of links
    assort <- assortativity_degree(sub,directed = F) #assortativity
    diss <- mean(1 - E(sub)$weight) # Dissilarity as defined in NetCoMi
    trans <- transitivity(sub,type = "global") #Transitivity
    mean_edge_bet <- mean(edge_betweenness(sub)) # Mean edge betweeness
    
    adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
    diag(adj) <- 1
    nat_connect <- natural.connectivity(as.matrix(adj)) # natural connectivity
    
    data_results_reseaux[i,1] <- station
    data_results_reseaux[i,2] <- date
    data_results_reseaux[i,3] <- S_net # nombre de noeuds
    data_results_reseaux[i,4] <- L_net # nombre de liens
    data_results_reseaux[i,5] <- Z_mat # Densite des liens
    data_results_reseaux[i,6] <- C_net # connectance
    
    data_results_reseaux[i,7] <- avg_path_length # longueur moyen des liens
    data_results_reseaux[i,8] <- Edge_connect # adhesion
    data_results_reseaux[i,9] <- Modularity #modularite
    
    data_results_reseaux[i,10] <- m_degree #Nombre de liens moyens
    data_results_reseaux[i,11] <- assort # Assortativite
    data_results_reseaux[i,12] <- diss # dissimilarite
    data_results_reseaux[i,13] <- trans # transitivite
    
    data_results_reseaux[i,14] <- mean_edge_bet # Nombre moyen de voisins
    data_results_reseaux[i,15] <- nat_connect # Connectivite naturelle
    
    data_results_reseaux[i,16] <- length(wc) # Nombre de cluster
  } 
  else { 
    data_results_reseaux[i,1] <- station
    data_results_reseaux[i,2] <- date
    data_results_reseaux[i,3] <- NA # nombre de noeuds
    data_results_reseaux[i,4] <- NA # nombre de liens
    data_results_reseaux[i,5] <- NA # Densite des liens
    data_results_reseaux[i,6] <- NA # connectance
    
    data_results_reseaux[i,7] <- NA # longueur moyen des liens
    data_results_reseaux[i,8] <- NA # adhesion
    data_results_reseaux[i,9] <- NA #modularite
    
    data_results_reseaux[i,10] <- NA #Nombre de liens moyens
    data_results_reseaux[i,11] <- NA # Assortativite
    data_results_reseaux[i,12] <- NA # dissimilarite
    data_results_reseaux[i,13] <- NA # transitivite
    
    data_results_reseaux[i,14] <- NA # Nombre moyen de voisins
    data_results_reseaux[i,15] <- NA # Connectivite naturelle
    
    data_results_reseaux[i,16] <- NA # Nombre de cluster
    
  }
  
  colnames(data_results_reseaux) <- c("Code_point_Libelle","Date","N_noeuds","N_liens","D_liens","C_tance",
                                      "Avg_p_length","Adhes","Mod","meanN_liens","Assort","Diss","Trans","meanN_voisins",
                                      "Nat_connect","N_clust")
  
  #plot(sub,main = paste0(station,date),layout = layout_with_fr)
  #png(paste0("output/graphs/Reseaux/TS_CLUST1/",station,date,".png"))
  #plot(sub,main = paste0(station,date),layout = layout_with_fr)
  #dev.off()
  #htmlwidgets::saveWidget(viz_sub, paste0("output/graphs/Reseaux/HTML/",station,date,".html"))
  #print(i/nrow(CL2)*100)
}
# Save the result
write.csv2(data_results_reseaux,file="data_modif/results_metrics_reseaux_cluster2_pos.csv", row.names = FALSE,dec = ".")

# Result analysis
data_results_reseaux <- read_delim("data_modif/results_metrics_reseaux_cluster2_pos.csv", 
                                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                       grouping_mark = ""), trim_ws = TRUE)
data_results_reseaux_copy <- data_results_reseaux

bloom <- dplyr::select(CL2df,Code_point_Libelle,Date,Bloom_Phylum,P_dominance)

data_reseaux <- left_join(data_results_reseaux,bloom)

# Appently there are duplicates so we delete them
data_reseaux <- data_reseaux[-c(904:906),]

data_reseaux <- data_reseaux %>%
  mutate(Month = month(Date, label = F)) |>
  mutate(Year = year(Date))

data_reseaux$Date2 <- as.Date(paste(data_reseaux$Year, data_reseaux$Month, "01", sep = "-"), format = "%Y-%m-%d")

datam <- summarise(group_by(data_reseaux,Code_point_Libelle,Month,Year), N_noeuds=mean(N_noeuds,na.rm=T),N_liens=mean(N_liens,na.rm=T),
                   D_liens=mean(D_liens,na.rm=T),C_tance=mean(C_tance,na.rm=T),LML=mean(Avg_p_length,na.rm=T),
                   Adhes=mean(Adhes,na.rm=T),Mod=mean(Mod,na.rm=T),meanN_liens=mean(meanN_liens,na.rm=T),
                   Assort=mean(Assort,na.rm=T),Diss=mean(Diss,na.rm=T),Trans=mean(Trans,na.rm=T),
                   meanN_voisins=mean(meanN_voisins,na.rm=T),Nat_connect=mean(Nat_connect,na.rm=T),N_clust=mean(N_clust,na.rm=T))

datamcluster2 <- datam
# Metrics seasonalities
datal <- pivot_longer(datamcluster2,names_to = "Var",cols = N_noeuds:N_clust)
datal2 <- filter(datal, Var %in% c("Assort","LML","C_tance","D_liens","Mod","Nat_connect","N_noeuds","Adhes"))
ggplot(datal2)+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#CD9600",size = 1)+
  facet_wrap(~Var,scales = "free_y",ncol=2)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  labs(title = "Metriques réseau cluster 2 (+)")
#ggsave('Metriques_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')

datal2$cluster <- "2-Manche orientale - Mer du Nord"

# Create the graph I wanted
nnoeuds <- ggplot(filter(datal2,Var == "N_noeuds"))+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#CD9600",size = 0.5)+
  facet_wrap(cluster~Var,scales = "free_y",ncol=2)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  scale_y_continuous(breaks = seq(0,40,10),limits = c(0,40))+
  labs(x=NULL,y="Mesure")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 7),
        strip.background = element_rect(fill = NULL))

LML <- ggplot(filter(datal2,Var == "LML"))+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#CD9600",size = 0.5)+
  facet_wrap(cluster~Var,scales = "free_y",ncol=2)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  scale_y_continuous(breaks = seq(0,0.5,0.05),limits = c(0,0.5))+
  labs(x=NULL,y=NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 7),
        strip.background = element_rect(fill = NULL))

assort <- ggplot(filter(datal2,Var == "Assort"))+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#CD9600",size = 0.5)+
  facet_wrap(cluster~Var,scales = "free_y",ncol=2)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  scale_y_continuous(breaks = seq(-1,0.6,0.2),limits = c(-1,0.6))+
  labs(x=NULL,y=NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 7),
        strip.background = element_rect(fill = NULL))

natconnect <- ggplot(filter(datal2,Var == "Nat_connect"))+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#CD9600",size = 0.5)+
  facet_wrap(cluster~Var,scales = "free_y",ncol=2)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  scale_y_continuous(breaks = seq(0,0.7,0.1),limits = c(0,0.7))+
  labs(x=NULL,y=NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 7),
        strip.background = element_rect(fill = NULL))

ctance <- ggplot(filter(datal2,Var == "C_tance"))+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#CD9600",size = 0.5)+
  facet_wrap(cluster~Var,scales = "free_y",ncol=2)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1))+
  labs(x=NULL,y=NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 7),
        strip.background = element_rect(fill = NULL))

mod <- ggplot(filter(datal2,Var == "Mod"))+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#CD9600",size = 0.5)+
  facet_wrap(cluster~Var,scales = "free_y",ncol=2)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  scale_y_continuous(breaks = seq(-0.3,0.4,0.2),limits = c(-0.3,0.4))+
  labs(x=NULL,y=NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 7),
        strip.background = element_rect(fill = NULL))

saisonmetric2 <- plot_grid(nnoeuds,natconnect,LML,ctance,assort,mod,ncol = 6)

# Trends
ggplot(datal)+
  geom_boxplot(aes(x=Year,y=value,group=Year),fill="#CD9600",size = 1)+
  facet_wrap(~Var,scales = "free_y")+
  labs(title = "Metriques réseau cluster 2 (+)")
#ggsave('Metriques_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')

CL2_Mmetrics <- summarise(group_by(data_reseaux,Code_point_Libelle,Date2),N_noeuds=mean(N_noeuds,na.rm=T),N_liens=mean(N_liens,na.rm=T),
                          D_liens=mean(D_liens,na.rm=T),C_tance=mean(C_tance,na.rm=T),Avg_p_length=mean(Avg_p_length,na.rm=T),
                          Adhes=mean(Adhes,na.rm=T),Mod=mean(Mod,na.rm=T),meanN_liens=mean(meanN_liens,na.rm=T),
                          Assort=mean(Assort,na.rm=T),Diss=mean(Diss,na.rm=T),Trans=mean(Trans,na.rm=T),
                          meanN_voisins=mean(meanN_voisins,na.rm=T),Nat_connect=mean(Nat_connect,na.rm=T),N_clust=mean(N_clust,na.rm=T))

# Time series (boxplot by year and mounth)
datal2 <- pivot_longer(CL2_Mmetrics,names_to = "Var",cols = N_noeuds:N_clust)
ggplot(datal2)+
  geom_boxplot(aes(x=Date2,y=value,group=Date2),fill="#CD9600",size = 1)+
  facet_wrap(~Var,scales = "free_y")+
  labs(title = "Metriques réseau cluster 2 (+)")
#ggsave('Metriques_cluster_ts.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')

##### Subgraph by season ####
data<- data |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))

# Winter #
CL2 <- filter(data, cluster == 2,season == "Winter" )
nbdate <- nrow(CL2)*0.33
CL2 <- dplyr::select(CL2,Actinoptychus:Coscinodiscophycidae)

CL2[is.na(CL2)]<- 0
CL2 <- ifelse(CL2 > 0, 1,0)
Spe_w <- as.data.frame(colSums(CL2))

# Prepare the list of the present genus
Spe_w$Phyto <- rownames(Spe_w)
colnames(Spe_w)[1] <- "V1"
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- filter(Spe_w,V1 > nbdate)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index of the species
nodes_net <- V(cluster2)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)
Spe <- filter(Spe,!is.na(Spe$Count))

# Compute the subgraph
vids <- Spe$Pindex
compo_reseau_clus2$Pres <- "Non"
compo_reseau_clus2[vids,]$Pres <- "Oui"
compo_reseau_clus2$color <- "grey"
compo_reseau_clus2$color[compo_reseau_clus2$Phylum.Classe == "Bacillariophyceae"] <- "#56B4E9"
compo_reseau_clus2$color[compo_reseau_clus2$Phylum.Classe == "Dinophyceae"] <- "#009E73"
compo_reseau_clus2$color[compo_reseau_clus2$Phylum.Classe == "Ciliophora"] <-"#F0E442"
compo_reseau_clus2$color[compo_reseau_clus2$Phylum.Classe == "Cryptophyceae"] <-"#CC79A7"
compo_reseau_clus2$color[compo_reseau_clus2$Phylum.Classe == "Haptophyta"] <-"#996136"
compo_reseau_clus2$color[compo_reseau_clus2$Pres == "Non"] <- "transparent"

present <- compo_reseau$Taxon[compo_reseau$Pres == "Oui"]

dataabd <- as.data.frame(colSums(CL2))
dataabd$Taxon <- rownames(dataabd)
colnames(dataabd)[1] <- "Abd"
compo_reseau <- left_join(compo_reseau_clus2,dataabd)

liens_df <- get.data.frame(cluster2, what = "edges")
liens_df$color <- "black"
liens_df$color[!liens_df$from %in% present  ] <- "transparent"
liens_df$color[!liens_df$to %in% present  ] <- "transparent"

# Personalize the graph
V(cluster2)$label <- V(cluster2)$name
V(cluster2)$Phylum <- compo_reseau$Phylum.Classe
V(cluster2)$N_liens <- degree(cluster2)
V(cluster2)$size <- log(compo_reseau$Abd)
V(cluster2)$Module <- membership(wcini)
V(cluster2)$color <- compo_reseau$color
E(cluster2)$width <- E(cluster2)$weight*6
E(cluster2)$color <- liens_df$color
cluster2$layout <- layout_fr

sub <- igraph::subgraph(cluster2, vids)
sub$layout <- layout_fr[vids,]
viz_sub <- hchart(cluster2)
plot(sub)
viz_sub

# Subgraph metrics
vcount(sub) # Number of nodes
ecount(sub) # Number of links
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = TRUE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

# Compute the clustering on the graph
wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) # mean number of links
assortativity_degree(sub,directed = F) #assortativity
mean(1 - E(sub)$weight) # Dissilarity as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # natural connectivity

# Compute the hubs
hubs <- eigen_centrality(
  sub,
  directed = FALSE,
  scale = TRUE,
  weights = NULL
)
Hubs <- as.data.frame(hubs$vector)
Hubs$Phyto <- rownames(Hubs)

Spe_hubs <- Hubs[order(desc(hubs$vector)),]
Spe_hubs
# Store the results 
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster2_hiver.csv", row.names = FALSE,dec = ".")

# Manip to have the composition of the graph in term of phyla
# Creating a df to store the results
compo_reseau <- c("","")
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau <- wcini$names
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau$cluster <- wcini$membership
colnames(compo_reseau)[1] <- "Taxon"
# Import the taxonomic information
PhyClasse <- read.csv('data_modif/Liste_phylum.classe_REPHY_JYmodif.csv', sep =';', header = TRUE, fileEncoding = 'ISO-8859-1')
compo_reseau <- left_join(compo_reseau,PhyClasse)
compo_reseau$Nb <- 1
colnames(Spe_hubs)[2] <- "Taxon"
compo_reseau <- left_join(Spe_hubs,compo_reseau)

compo_reseau <- compo_reseau |>
  group_by(cluster,Phylum.Classe) |>
  summarise(Nb = sum(Nb))
write.csv2(compo_reseau,file="data_modif/compo_reseau_cluster2_hiver.csv", row.names = FALSE,dec = ".")

# Fall #
CL2 <- filter(data, cluster == 2,season == "Fall" )
nbdate <- nrow(CL2)*0.33
CL2 <- dplyr::select(CL2,Actinoptychus:Coscinodiscophycidae)

CL2[is.na(CL2)]<- 0
CL2 <- ifelse(CL2 > 0, 1,0)
Spe_w <- as.data.frame(colSums(CL2))

# Prepare the list of the present genus
Spe_w$Phyto <- rownames(Spe_w)
colnames(Spe_w)[1] <- "V1"
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- filter(Spe_w,V1 > nbdate)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index of the species
nodes_net <- V(cluster2)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)
Spe <- filter(Spe,!is.na(Spe$Count))

# Compute the subgraph
# Compute the subgraph
vids <- Spe$Pindex
compo_reseau_clus2$Pres <- "Non"
compo_reseau_clus2[vids,]$Pres <- "Oui"
compo_reseau_clus2$color <- "grey"
compo_reseau_clus2$color[compo_reseau_clus2$Phylum.Classe == "Bacillariophyceae"] <- "#56B4E9"
compo_reseau_clus2$color[compo_reseau_clus2$Phylum.Classe == "Dinophyceae"] <- "#009E73"
compo_reseau_clus2$color[compo_reseau_clus2$Phylum.Classe == "Ciliophora"] <-"#F0E442"
compo_reseau_clus2$color[compo_reseau_clus2$Phylum.Classe == "Cryptophyceae"] <-"#CC79A7"
compo_reseau_clus2$color[compo_reseau_clus2$Phylum.Classe == "Haptophyta"] <-"#996136"
compo_reseau_clus2$color[compo_reseau_clus2$Pres == "Non"] <- "transparent"

present <- compo_reseau_clus2$Taxon[compo_reseau_clus2$Pres == "Oui"]

dataabd <- as.data.frame(colSums(CL2))
dataabd$Taxon <- rownames(dataabd)
colnames(dataabd)[1] <- "Abd"
compo_reseau <- left_join(compo_reseau_clus2,dataabd)

liens_df <- get.data.frame(cluster2, what = "edges")
liens_df$color <- "black"
liens_df$color[!liens_df$from %in% present  ] <- "transparent"
liens_df$color[!liens_df$to %in% present  ] <- "transparent"

# Personalize the graph
V(cluster2)$label <- V(cluster2)$name
V(cluster2)$Phylum <- compo_reseau$Phylum.Classe
V(cluster2)$N_liens <- degree(cluster2)
V(cluster2)$size <- log(compo_reseau$Abd)
V(cluster2)$Module <- membership(wcini)
V(cluster2)$color <- compo_reseau$color
E(cluster2)$width <- E(cluster2)$weight*6
E(cluster2)$color <- liens_df$color
cluster2$layout <- layout_fr

sub <- igraph::subgraph(cluster2, vids)
sub$layout <- layout_fr[vids,]
viz_sub <- hchart(cluster2)
plot(sub)
viz_sub

vcount(sub) # Number of nodes
ecount(sub) # Number of links
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = TRUE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

# Compute the clustering on the graph
wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) # mean number of links
assortativity_degree(sub,directed = F) #assortativity
mean(1 - E(sub)$weight) # Dissilarity as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # natural connectivity

# Compute the hubs
hubs <- eigen_centrality(
  sub,
  directed = FALSE,
  scale = TRUE,
  weights = NULL
)
Hubs <- as.data.frame(hubs$vector)
Hubs$Phyto <- rownames(Hubs)

Spe_hubs <- Hubs[order(desc(hubs$vector)),]
Spe_hubs
# Store the results 
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster2_automne.csv", row.names = FALSE,dec = ".")

# Manip to have the composition of the graph in term of phyla
# Creating a df to store the results
compo_reseau <- c("","")
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau <- wcini$names
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau$cluster <- wcini$membership
colnames(compo_reseau)[1] <- "Taxon"
# Import the taxonomic information
PhyClasse <- read.csv('data_modif/Liste_phylum.classe_REPHY_JYmodif.csv', sep =';', header = TRUE, fileEncoding = 'ISO-8859-1')
compo_reseau <- left_join(compo_reseau,PhyClasse)
compo_reseau$Nb <- 1
colnames(Spe_hubs)[2] <- "Taxon"
compo_reseau <- left_join(Spe_hubs,compo_reseau)

compo_reseau <- compo_reseau |>
  group_by(cluster,Phylum.Classe) |>
  summarise(Nb = sum(Nb))
write.csv2(compo_reseau,file="data_modif/compo_reseau_cluster2_automne.csv", row.names = FALSE,dec = ".")

# Spring #
CL2 <- filter(data, cluster == 2,season == "Spring" )
nbdate <- nrow(CL2)*0.33
CL2 <- dplyr::select(CL2,Actinoptychus:Coscinodiscophycidae)

CL2[is.na(CL2)]<- 0
CL2 <- ifelse(CL2 > 0, 1,0)
Spe_w <- as.data.frame(colSums(CL2))

# Prepare the list of the present genus
Spe_w$Phyto <- rownames(Spe_w)
colnames(Spe_w)[1] <- "V1"
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- filter(Spe_w,V1 > nbdate)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index of the species
nodes_net <- V(cluster2)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)
Spe <- filter(Spe,!is.na(Spe$Count))

# Compute the subgraph
vids <- Spe$Pindex
compo_reseau_clus2$Pres <- "Non"
compo_reseau_clus2[vids,]$Pres <- "Oui"
compo_reseau_clus2$color <- "grey"
compo_reseau_clus2$color[compo_reseau_clus2$Phylum.Classe == "Bacillariophyceae"] <- "#56B4E9"
compo_reseau_clus2$color[compo_reseau_clus2$Phylum.Classe == "Dinophyceae"] <- "#009E73"
compo_reseau_clus2$color[compo_reseau_clus2$Phylum.Classe == "Ciliophora"] <-"#F0E442"
compo_reseau_clus2$color[compo_reseau_clus2$Phylum.Classe == "Cryptophyceae"] <-"#CC79A7"
compo_reseau_clus2$color[compo_reseau_clus2$Phylum.Classe == "Haptophyta"] <-"#996136"
compo_reseau_clus2$color[compo_reseau_clus2$Pres == "Non"] <- "transparent"

present <- compo_reseau_clus2$Taxon[compo_reseau_clus2$Pres == "Oui"]

dataabd <- as.data.frame(colSums(CL2))
dataabd$Taxon <- rownames(dataabd)
colnames(dataabd)[1] <- "Abd"
compo_reseau <- left_join(compo_reseau_clus2,dataabd)

liens_df <- get.data.frame(cluster2, what = "edges")
liens_df$color <- "black"
liens_df$color[!liens_df$from %in% present  ] <- "transparent"
liens_df$color[!liens_df$to %in% present  ] <- "transparent"

# Personalize the graph
V(cluster2)$label <- V(cluster2)$name
V(cluster2)$Phylum <- compo_reseau$Phylum.Classe
V(cluster2)$N_liens <- degree(cluster2)
V(cluster2)$size <- log(compo_reseau$Abd)
V(cluster2)$Module <- membership(wcini)
V(cluster2)$color <- compo_reseau$color
E(cluster2)$width <- E(cluster2)$weight*6
E(cluster2)$color <- liens_df$color
cluster2$layout <- layout_fr

sub <- igraph::subgraph(cluster2, vids)
sub$layout <- layout_fr[vids,]
viz_sub <- hchart(cluster2)
plot(sub)
viz_sub

vcount(sub) # Number of nodes
ecount(sub) # Number of links
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = TRUE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

# Compute the clustering on the graph
wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) # mean number of links
assortativity_degree(sub,directed = F) #assortativity
mean(1 - E(sub)$weight) # Dissilarity as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # natural connectivity

# Compute the hubs
hubs <- eigen_centrality(
  sub,
  directed = FALSE,
  scale = TRUE,
  weights = NULL
)
Hubs <- as.data.frame(hubs$vector)
Hubs$Phyto <- rownames(Hubs)

Spe_hubs <- Hubs[order(desc(hubs$vector)),]
Spe_hubs
# Store the results 
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster2_printemps.csv", row.names = FALSE,dec = ".")

# Manip to have the composition of the graph in term of phyla
# Creating a df to store the results
compo_reseau <- c("","")
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau <- wcini$names
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau$cluster <- wcini$membership
colnames(compo_reseau)[1] <- "Taxon"
# Import the taxonomic information
PhyClasse <- read.csv('data_modif/Liste_phylum.classe_REPHY_JYmodif.csv', sep =';', header = TRUE, fileEncoding = 'ISO-8859-1')
compo_reseau <- left_join(compo_reseau,PhyClasse)
compo_reseau$Nb <- 1
colnames(Spe_hubs)[2] <- "Taxon"
compo_reseau <- left_join(Spe_hubs,compo_reseau)

compo_reseau <- compo_reseau |>
  group_by(cluster,Phylum.Classe) |>
  summarise(Nb = sum(Nb))
write.csv2(compo_reseau,file="data_modif/compo_reseau_cluster2_printemps.csv", row.names = FALSE,dec = ".")

# Summer
CL2 <- filter(data, cluster == 2,season == "Summer" )
nbdate <- nrow(CL2)*0.33
CL2 <- dplyr::select(CL2,Actinoptychus:Coscinodiscophycidae)

CL2[is.na(CL2)]<- 0
CL2 <- ifelse(CL2 > 0, 1,0)
Spe_w <- as.data.frame(colSums(CL2))

# Prepare the list of the present genus
Spe_w$Phyto <- rownames(Spe_w)
colnames(Spe_w)[1] <- "V1"
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- filter(Spe_w,V1 > nbdate)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index of the species
nodes_net <- V(cluster2)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)
Spe <- filter(Spe,!is.na(Spe$Count))

# Compute the subgraph
vids <- Spe$Pindex
compo_reseau_clus2$Pres <- "Non"
compo_reseau_clus2[vids,]$Pres <- "Oui"
compo_reseau_clus2$color <- "grey"
compo_reseau_clus2$color[compo_reseau_clus2$Phylum.Classe == "Bacillariophyceae"] <- "#56B4E9"
compo_reseau_clus2$color[compo_reseau_clus2$Phylum.Classe == "Dinophyceae"] <- "#009E73"
compo_reseau_clus2$color[compo_reseau_clus2$Phylum.Classe == "Ciliophora"] <-"#F0E442"
compo_reseau_clus2$color[compo_reseau_clus2$Phylum.Classe == "Cryptophyceae"] <-"#CC79A7"
compo_reseau_clus2$color[compo_reseau_clus2$Phylum.Classe == "Haptophyta"] <-"#996136"
compo_reseau_clus2$color[compo_reseau_clus2$Pres == "Non"] <- "transparent"

present <- compo_reseau_clus2$Taxon[compo_reseau_clus2$Pres == "Oui"]

dataabd <- as.data.frame(colSums(CL2))
dataabd$Taxon <- rownames(dataabd)
colnames(dataabd)[1] <- "Abd"
compo_reseau <- left_join(compo_reseau_clus2,dataabd)

liens_df <- get.data.frame(cluster2, what = "edges")
liens_df$color <- "black"
liens_df$color[!liens_df$from %in% present  ] <- "transparent"
liens_df$color[!liens_df$to %in% present  ] <- "transparent"

# Personalize the graph
V(cluster2)$label <- V(cluster2)$name
V(cluster2)$Phylum <- compo_reseau$Phylum.Classe
V(cluster2)$N_liens <- degree(cluster2)
V(cluster2)$size <- log(compo_reseau$Abd)
V(cluster2)$Module <- membership(wcini)
V(cluster2)$color <- compo_reseau$color
E(cluster2)$width <- E(cluster2)$weight*6
E(cluster2)$color <- liens_df$color
cluster2$layout <- layout_fr

sub <- igraph::subgraph(cluster2, vids)
sub$layout <- layout_fr[vids,]
viz_sub <- hchart(cluster2)
plot(sub)
viz_sub

# Subgraph metrics
vcount(sub) # Number of nodes
ecount(sub) # Number of links
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = TRUE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

# Compute the clustering on the graph
wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) # mean number of links
assortativity_degree(sub,directed = F) #assortativity
mean(1 - E(sub)$weight) # Dissilarity as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # natural connectivity

# Compute the hubs
hubs <- eigen_centrality(
  sub,
  directed = FALSE,
  scale = TRUE,
  weights = NULL
)
Hubs <- as.data.frame(hubs$vector)
Hubs$Phyto <- rownames(Hubs)

Spe_hubs <- Hubs[order(desc(hubs$vector)),]
Spe_hubs
# Store the results 
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster2_summer.csv", row.names = FALSE,dec = ".")

# Manip to have the composition of the graph in term of phyla
# Creating a df to store the results
compo_reseau <- c("","")
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau <- wcini$names
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau$cluster <- wcini$membership
colnames(compo_reseau)[1] <- "Taxon"
# Import the taxonomic information
PhyClasse <- read.csv('data_modif/Liste_phylum.classe_REPHY_JYmodif.csv', sep =';', header = TRUE, fileEncoding = 'ISO-8859-1')
compo_reseau <- left_join(compo_reseau,PhyClasse)
compo_reseau$Nb <- 1
colnames(Spe_hubs)[2] <- "Taxon"
compo_reseau <- left_join(Spe_hubs,compo_reseau)

compo_reseau <- compo_reseau |>
  group_by(cluster,Phylum.Classe) |>
  summarise(Nb = sum(Nb))
write.csv2(compo_reseau,file="data_modif/compo_reseau_cluster2_été.csv", row.names = FALSE,dec = ".")

#### Working on the cluster 3 #####

# We keep only positive correlations
assoMat <- net_cluster3$assoMat1
assoMat[assoMat < 0] <- 0

cluster3 <- graph_from_adjacency_matrix(assoMat,weighted = T,mode = "undirected",diag=F)

# Compute clustering on the graph
wcini <- cluster_fast_greedy(cluster3)
compo_reseau <- c("","")
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau <- wcini$names
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau$cluster <- wcini$membership
colnames(compo_reseau)[1] <- "Taxon"
# Import the taxonomic information
PhyClasse <- read.csv('data_modif/Liste_phylum.classe_REPHY_JYmodif.csv', sep =';', header = TRUE, fileEncoding = 'ISO-8859-1')
compo_reseau <- left_join(compo_reseau,PhyClasse)
compo_reseau$Nb <- 1

compo_reseau2 <- compo_reseau |>
  group_by(cluster,Phylum.Classe) |>
  summarise(Nb = sum(Nb))
write.csv2(compo_reseau2,file="data_modif/compo_reseau_cluster3.csv", row.names = FALSE,dec = ".")


compo_reseau$color <- "grey"
compo_reseau$color[compo_reseau$Phylum.Classe == "Bacillariophyceae"] <- "#56B4E9"
compo_reseau$color[compo_reseau$Phylum.Classe == "Dinophyceae"] <- "#009E73"
compo_reseau$color[compo_reseau$Phylum.Classe == "Ciliophora"] <-"#F0E442"
compo_reseau$color[compo_reseau$Phylum.Classe == "Cryptophyceae"] <-"#CC79A7"
compo_reseau$color[compo_reseau$Phylum.Classe == "Haptophyta"] <-"#996136"

compo_reseau_clus3 <- compo_reseau

dataabd <- as.data.frame(colSums(CL3))
dataabd$Taxon <- rownames(dataabd)
colnames(dataabd)[1] <- "Abd"
compo_reseau_clus3 <- left_join(compo_reseau_clus3,dataabd)


# Personalize the graph
V(cluster3)$label <- V(cluster3)$name
V(cluster3)$Phylum <- compo_reseau_clus3$Phylum.Classe
V(cluster3)$N_liens <- degree(cluster3)
V(cluster3)$size <- log(compo_reseau_clus3$Abd)
V(cluster3)$Module <- membership(wcini)
V(cluster3)$color <- compo_reseau_clus3$color
E(cluster3)$width <- E(cluster3)$weight*6
E(cluster3)$color <- "black"
layout_fr <- layout_with_fr(cluster3)
cluster3$layout <- layout_fr

# Plot it
# Other way
viz3 <- hchart(cluster3)
viz3
# Save it
#htmlwidgets::saveWidget(viz3, "output/graphs/Reseaux/HTML/cluster3.html")

# Global network metrics
S_net <-  vcount(cluster3) # Number of nodes
L_net <- ecount(cluster3) # Number of links

Z_mat <- L_net / S_net # linkage density or average number of links per nodes

C_net <- edge_density(cluster3, loops = FALSE) #connectance

# Average path length
avg_path_length <- mean_distance(
  cluster3,
  directed = FALSE,
  unconnected = TRUE # if the graphs is disconnected, only the existing paths are considered
)

Edge_connect <- edge.connectivity(cluster3) # Edge connectivity = adhesion

Modularity <- modularity(cluster3,membership = membership(wcini)) # Modularity

m_degree <- mean(degree(cluster3)) #Mean number of links per node

assort <- assortativity_degree(cluster3,directed = F) #assortativity

diss <- mean(1 - E(cluster3)$weight) # Dissilarity as defined in NetCoMi

trans <- transitivity(cluster3,type = "global") #Transitivity

mean_edge_bet <- mean(edge_betweenness(cluster3)) # Mean edge betweeness

# Compute the natural connectivity 
adj <- as.matrix(as_adjacency_matrix(cluster3, attr = "weight",)) # OK
diag(adj) <- 1
nat_connect <- natural.connectivity(as.matrix(adj)) # Natural connectivity

# Define the different hubs
hubs <- eigen_centrality(
  cluster3,
  directed = FALSE,
  scale = TRUE,
  weights = NULL
)
Hubs <- as.data.frame(hubs$vector)
Hubs$Phyto <- rownames(Hubs)

Spe_hubs <- Hubs[order(desc(hubs$vector)),]
# Store the result
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster3.csv", row.names = FALSE,dec = ".")

# Manip to have the composition of the graph in term of phyla
# Creating a df to store the results
compo_reseau <- c("","")
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau <- wcini$names
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau$cluster <- wcini$membership
colnames(compo_reseau)[1] <- "Taxon"
# Import the taxonomic information
PhyClasse <- read.csv('data_modif/Liste_phylum.classe_REPHY_JYmodif.csv', sep =';', header = TRUE, fileEncoding = 'ISO-8859-1')
compo_reseau <- left_join(compo_reseau,PhyClasse)
compo_reseau$Nb <- 1
colnames(Spe_hubs)[2] <- "Taxon"
compo_reseau <- left_join(Spe_hubs,compo_reseau)

compo_reseau <- compo_reseau |>
  group_by(cluster,Phylum.Classe) |>
  summarise(Nb = sum(Nb))
write.csv2(compo_reseau,file="data_modif/compo_reseau_cluster3.csv", row.names = FALSE,dec = ".")



##### Compute subgraph by day #######
# Preparation  
# Index of species
nodes_net <- V(cluster3)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

# Table preparation to retrieve station/date information
CL3df <- filter(data,cluster == 3)

# Creating a df to store the results
data_results_reseaux <- c("","")
data_results_reseaux <- as.data.frame(data_results_reseaux)

# Compute the subgraphs
for (i in 1:nrow(CL3)){ 
spe <- as.data.frame(CL3[i,])
colnames(spe) <- "Count" 
spe$phyto <- rownames(spe)
spe <- left_join(phyto_index,spe, by = join_by(phyto))
spe <- filter(spe,Count>0)
spe$Pindex <- as.numeric(spe$Pindex)

station <- CL3df[i,]$Code_point_Libelle
date <- CL3df[i,]$Date

vids <- spe$Pindex
sub <- igraph::subgraph(cluster3, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)

# Metrics 
S_net <-  vcount(sub) # Number of nodes
L_net <- ecount(sub) # Number of links
Z_mat <- L_net / S_net # linkage density or average number of links per nodes
C_net <- edge_density(sub, loops = FALSE) #connectance

# Average path length
avg_path_length <- mean_distance(
  sub,
  directed = FALSE,
  unconnected = TRUE # if the graphs is disconnected, only the existing paths are considered
)

Edge_connect <- edge.connectivity(sub) # Edge connectivity = adhesion

wc <- cluster_fast_greedy(sub)
Modularity <- modularity(sub,membership = membership(wc)) # Modularity

Vert_connect <- vertex.connectivity(sub) # Vertex connectivity = adhesion
m_degree <- mean(degree(sub)) # mean number of links
assort <- assortativity_degree(sub,directed = F) #assortativity
diss <- mean(1 - E(sub)$weight) # Dissilarity as defined in NetCoMi
trans <- transitivity(sub,type = "global") #Transitivity
mean_edge_bet <- mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
nat_connect <- natural.connectivity(as.matrix(adj)) # natural connectivity

data_results_reseaux[i,1] <- station
data_results_reseaux[i,2] <- date
data_results_reseaux[i,3] <- S_net # nombre de noeuds
data_results_reseaux[i,4] <- L_net # nombre de liens
data_results_reseaux[i,5] <- Z_mat # Densite des liens
data_results_reseaux[i,6] <- C_net # connectance

data_results_reseaux[i,7] <- avg_path_length # longueur moyen des liens
data_results_reseaux[i,8] <- Edge_connect # adhesion
data_results_reseaux[i,9] <- Modularity #modularite

data_results_reseaux[i,10] <- m_degree #Nombre de liens moyens
data_results_reseaux[i,11] <- assort # Assortativite
data_results_reseaux[i,12] <- diss # dissimilarite
data_results_reseaux[i,13] <- trans # transitivite

data_results_reseaux[i,14] <- mean_edge_bet # Nombre moyen de voisins
data_results_reseaux[i,15] <- nat_connect # Connectivite naturelle

data_results_reseaux[i,16] <- length(wc) # Nombre de cluster

colnames(data_results_reseaux) <- c("Code_point_Libelle","Date","N_noeuds","N_liens","D_liens","C_tance",
                                    "Avg_p_length","Adhes","Mod","meanN_liens","Assort","Diss","Trans","meanN_voisins",
                                    "Nat_connect","N_clust")

#plot(sub,main = paste0(station,date),layout = layout_with_fr)
#png(paste0("output/graphs/Reseaux/TS_CLUST3/",station,date,".png"))
#plot(sub,main = paste0(station,date),layout = layout_with_fr)
#dev.off()
#htmlwidgets::saveWidget(viz_sub, paste0("output/graphs/Reseaux/HTML/",station,date,".html"))
#print(i/nrow(CL3)*100)
}
# Save the result
write.csv2(data_results_reseaux,file="data_modif/results_metrics_reseaux_cluster3_pos.csv", row.names = FALSE,dec = ".")

# Result analysis
data_results_reseaux <- read_delim("data_modif/results_metrics_reseaux_cluster3_pos.csv", 
                                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                       grouping_mark = ""), trim_ws = TRUE)
data_results_reseaux_copy <- data_results_reseaux

bloom <- dplyr::select(CL3df,Code_point_Libelle,Date,Bloom_Phylum,P_dominance)

data_reseaux <- left_join(data_results_reseaux,bloom)

data_reseaux <- data_reseaux %>%
  mutate(Month = month(Date, label = F)) |>
  mutate(Year = year(Date))

data_reseaux$Date2 <- as.Date(paste(data_reseaux$Year, data_reseaux$Month, "01", sep = "-"), format = "%Y-%m-%d")

datam <- summarise(group_by(data_reseaux,Code_point_Libelle,Month,Year), N_noeuds=mean(N_noeuds,na.rm=T),N_liens=mean(N_liens,na.rm=T),
                   D_liens=mean(D_liens,na.rm=T),C_tance=mean(C_tance,na.rm=T),LML=mean(Avg_p_length,na.rm=T),
                   Adhes=mean(Adhes,na.rm=T),Mod=mean(Mod,na.rm=T),meanN_liens=mean(meanN_liens,na.rm=T),
                   Assort=mean(Assort,na.rm=T),Diss=mean(Diss,na.rm=T),Trans=mean(Trans,na.rm=T),
                   meanN_voisins=mean(meanN_voisins,na.rm=T),Nat_connect=mean(Nat_connect,na.rm=T),N_clust=mean(N_clust,na.rm=T))

datamcluster3 <- datam
# Metrics seasonalities
datal <- pivot_longer(datamcluster3,names_to = "Var",cols = N_noeuds:N_clust)
datal2 <- filter(datal, Var %in% c("Assort","LML","C_tance","D_liens","Mod","Nat_connect","N_noeuds","Adhes"))
# Metrics seasonnalities
ggplot(datal2)+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#00BE67",size = 1)+
  facet_wrap(~Var,scales = "free_y",ncol=2)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  labs(title = "Metriques réseau cluster 3 (+)")
#ggsave('Metriques_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')

datal2$cluster <- "3-Atlantique - Manche occidentale"

# Make the graph I wanted
nnoeuds <- ggplot(filter(datal2,Var == "N_noeuds"))+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#00BE67",size = 0.5)+
  facet_wrap(cluster~Var,scales = "free_y",ncol=2)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  scale_y_continuous(breaks = seq(0,40,10),limits = c(0,40))+
  labs(x="Mois",y="")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 7),
        strip.background = element_rect(fill = NULL))

LML <- ggplot(filter(datal2,Var == "LML"))+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#00BE67",size = 0.5)+
  facet_wrap(cluster~Var,scales = "free_y",ncol=2)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  scale_y_continuous(breaks = seq(0,0.5,0.05),limits = c(0,0.5))+
  labs(x="Mois",y=NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust =0.5, hjust = 0.5))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 7),
        strip.background = element_rect(fill = NULL))

assort <- ggplot(filter(datal2,Var == "Assort"))+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#00BE67",size = 0.5)+
  facet_wrap(cluster~Var,scales = "free_y",ncol=2)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  scale_y_continuous(breaks = seq(-1,0.6,0.2),limits = c(-1,0.6))+
  labs(x="Mois",y=NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 7),
        strip.background = element_rect(fill = NULL))

natconnect <- ggplot(filter(datal2,Var == "Nat_connect"))+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#00BE67",size = 0.5)+
  facet_wrap(cluster~Var,scales = "free_y",ncol=2)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  scale_y_continuous(breaks = seq(0,0.7,0.1),limits = c(0,0.7))+
  labs(x="Mois",y=NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 7),
        strip.background = element_rect(fill = NULL))

ctance <- ggplot(filter(datal2,Var == "C_tance"))+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#00BE67",size = 0.5)+
  facet_wrap(cluster~Var,scales = "free_y",ncol=2)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1))+
  labs(x="Mois",y=NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 7),
        strip.background = element_rect(fill = NULL))


mod <- ggplot(filter(datal2,Var == "Mod"))+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#00BE67",size = 0.5)+
  facet_wrap(cluster~Var,scales = "free_y",ncol=2)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  scale_y_continuous(breaks = seq(-0.3,0.4,0.2),limits = c(-0.3,0.4))+
  labs(x="Mois",y=NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5))+
  theme(strip.text = element_text(face = "bold", color = "black",
                                  hjust = 0.5, size = 7),
        strip.background = element_rect(fill = NULL))

saisonmetric3 <- plot_grid(nnoeuds,natconnect,LML,ctance,assort,mod,ncol = 6)

plot_grid(saisonmetric1,saisonmetric2,saisonmetric3,ncol=1)

# Trends
ggplot(datal)+
  geom_boxplot(aes(x=Year,y=value,group=Year),fill="#00BE67",size = 1)+
  facet_wrap(~Var,scales = "free_y")+
  labs(title = "Metriques réseau cluster 3 (+)")
#ggsave('Metriques_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')



CL3_Mmetrics <- summarise(group_by(data_reseaux,Code_point_Libelle,Date2),N_noeuds=mean(N_noeuds,na.rm=T),N_liens=mean(N_liens,na.rm=T),
                          D_liens=mean(D_liens,na.rm=T),C_tance=mean(C_tance,na.rm=T),Avg_p_length=mean(Avg_p_length,na.rm=T),
                          Adhes=mean(Adhes,na.rm=T),Mod=mean(Mod,na.rm=T),meanN_liens=mean(meanN_liens,na.rm=T),
                          Assort=mean(Assort,na.rm=T),Diss=mean(Diss,na.rm=T),Trans=mean(Trans,na.rm=T),
                          meanN_voisins=mean(meanN_voisins,na.rm=T),Nat_connect=mean(Nat_connect,na.rm=T),N_clust=mean(N_clust,na.rm=T))

# Time series (boxplot by year and mounth)
datal2 <- pivot_longer(CL3_Mmetrics,names_to = "Var",cols = N_noeuds:N_clust)
ggplot(datal2)+
  geom_boxplot(aes(x=Date2,y=value,group=Date2),fill="#00BE67",size = 1)+
  facet_wrap(~Var,scales = "free_y")+
  labs(title = "Metriques réseau cluster 3 (+)")
#ggsave('Metriques_cluster_ts.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')


##### Subgraph by season ####
data<- data |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))
# Winter
CL3 <- filter(data, cluster == 3,season == "Winter" )
nbdate <- nrow(CL3)*0.33
CL3 <- dplyr::select(CL3,Actinoptychus:Coscinodiscophycidae)

CL3[is.na(CL3)]<- 0
CL3 <- ifelse(CL3 > 0, 1,0)
Spe_w <- as.data.frame(colSums(CL3))

# Prepare the list of the present genus
Spe_w$Phyto <- rownames(Spe_w)
colnames(Spe_w)[1] <- "V1"
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- filter(Spe_w,V1 > nbdate)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index of the species
nodes_net <- V(cluster3)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)
Spe <- filter(Spe,!is.na(Spe$Count))

# Compute the subgraph
vids <- Spe$Pindex
compo_reseau_clus3$Pres <- "Non"
compo_reseau_clus3[vids,]$Pres <- "Oui"
compo_reseau_clus3$color <- "grey"
compo_reseau_clus3$color[compo_reseau_clus3$Phylum.Classe == "Bacillariophyceae"] <- "#56B4E9"
compo_reseau_clus3$color[compo_reseau_clus3$Phylum.Classe == "Dinophyceae"] <- "#009E73"
compo_reseau_clus3$color[compo_reseau_clus3$Phylum.Classe == "Ciliophora"] <-"#F0E442"
compo_reseau_clus3$color[compo_reseau_clus3$Phylum.Classe == "Cryptophyceae"] <-"#CC79A7"
compo_reseau_clus3$color[compo_reseau_clus3$Phylum.Classe == "Haptophyta"] <-"#996136"
compo_reseau_clus3$color[compo_reseau_clus3$Pres == "Non"] <- "transparent"

present <- compo_reseau_clus3$Taxon[compo_reseau_clus3$Pres == "Oui"]

dataabd <- as.data.frame(colSums(CL3))
dataabd$Taxon <- rownames(dataabd)
colnames(dataabd)[1] <- "Abd"
compo_reseau <- left_join(compo_reseau_clus3,dataabd)

liens_df <- get.data.frame(cluster3, what = "edges")
liens_df$color[!liens_df$from %in% present  ] <- "transparent"
liens_df$color[!liens_df$to %in% present  ] <- "transparent"

# Personalize the graph
V(cluster3)$label <- V(cluster3)$name
V(cluster3)$Phylum <- compo_reseau$Phylum.Classe
V(cluster3)$N_liens <- degree(cluster3)
V(cluster3)$size <- log(compo_reseau$Abd)
V(cluster3)$Module <- membership(wcini)
V(cluster3)$color <- compo_reseau$color
E(cluster3)$width <- E(cluster3)$weight*6
E(cluster3)$color <- liens_df$color
cluster3$layout <- layout_fr

sub <- igraph::subgraph(cluster3, vids)
sub$layout <- layout_fr[vids,]
viz_sub <- hchart(cluster3)
plot(sub)
viz_sub

# Subgraph metrics
vcount(sub) # Number of nodes
ecount(sub) # Number of links
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = TRUE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

# Compute the clustering on the graph
wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) # mean number of links
assortativity_degree(sub,directed = F) #assortativity
mean(1 - E(sub)$weight) # Dissilarity as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # natural connectivity

# Compute the hubs
hubs <- eigen_centrality(
  sub,
  directed = FALSE,
  scale = TRUE,
  weights = NULL
)
Hubs <- as.data.frame(hubs$vector)
Hubs$Phyto <- rownames(Hubs)

Spe_hubs <- Hubs[order(desc(hubs$vector)),]
Spe_hubs
# Store the results 
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster3_hiver.csv", row.names = FALSE,dec = ".")

# Manip to have the composition of the graph in term of phyla
# Creating a df to store the results
compo_reseau <- c("","")
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau <- wcini$names
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau$cluster <- wcini$membership
colnames(compo_reseau)[1] <- "Taxon"
# Import the taxonomic information
PhyClasse <- read.csv('data_modif/Liste_phylum.classe_REPHY_JYmodif.csv', sep =';', header = TRUE, fileEncoding = 'ISO-8859-1')
compo_reseau <- left_join(compo_reseau,PhyClasse)
compo_reseau$Nb <- 1
colnames(Spe_hubs)[2] <- "Taxon"
compo_reseau <- left_join(Spe_hubs,compo_reseau)

compo_reseau <- compo_reseau |>
  group_by(cluster,Phylum.Classe) |>
  summarise(Nb = sum(Nb))
write.csv2(compo_reseau,file="data_modif/compo_reseau_cluster3_hiver.csv", row.names = FALSE,dec = ".")

# Fall #
CL3 <- filter(data, cluster == 3,season == "Fall" )
nbdate <- nrow(CL3)*0.33
CL3 <- dplyr::select(CL3,Actinoptychus:Coscinodiscophycidae)

CL3[is.na(CL3)]<- 0
CL3 <- ifelse(CL3 > 0, 1,0)
Spe_w <- as.data.frame(colSums(CL3))

# Prepare the list of the present genus
Spe_w$Phyto <- rownames(Spe_w)
colnames(Spe_w)[1] <- "V1"
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- filter(Spe_w,V1 > nbdate)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index of the species
nodes_net <- V(cluster3)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)
Spe <- filter(Spe,!is.na(Spe$Count))

# Compute the subgraph
vids <- Spe$Pindex
compo_reseau_clus3$Pres <- "Non"
compo_reseau_clus3[vids,]$Pres <- "Oui"
compo_reseau_clus3$color <- "grey"
compo_reseau_clus3$color[compo_reseau_clus3$Phylum.Classe == "Bacillariophyceae"] <- "#56B4E9"
compo_reseau_clus3$color[compo_reseau_clus3$Phylum.Classe == "Dinophyceae"] <- "#009E73"
compo_reseau_clus3$color[compo_reseau_clus3$Phylum.Classe == "Ciliophora"] <-"#F0E442"
compo_reseau_clus3$color[compo_reseau_clus3$Phylum.Classe == "Cryptophyceae"] <-"#CC79A7"
compo_reseau_clus3$color[compo_reseau_clus3$Phylum.Classe == "Haptophyta"] <-"#996136"
compo_reseau_clus3$color[compo_reseau_clus3$Pres == "Non"] <- "transparent"

present <- compo_reseau_clus3$Taxon[compo_reseau_clus3$Pres == "Oui"]

dataabd <- as.data.frame(colSums(CL3))
dataabd$Taxon <- rownames(dataabd)
colnames(dataabd)[1] <- "Abd"
compo_reseau <- left_join(compo_reseau_clus3,dataabd)

liens_df <- get.data.frame(cluster3, what = "edges")
liens_df$color <- "black"
liens_df$color[!liens_df$from %in% present  ] <- "transparent"
liens_df$color[!liens_df$to %in% present  ] <- "transparent"

# Personalize the graph
V(cluster3)$label <- V(cluster3)$name
V(cluster3)$Phylum <- compo_reseau$Phylum.Classe
V(cluster3)$N_liens <- degree(cluster3)
V(cluster3)$size <- log(compo_reseau$Abd)
V(cluster3)$Module <- membership(wcini)
V(cluster3)$color <- compo_reseau$color
E(cluster3)$width <- E(cluster3)$weight*6
E(cluster3)$color <- liens_df$color
cluster3$layout <- layout_fr

sub <- igraph::subgraph(cluster3, vids)
sub$layout <- layout_fr[vids,]
viz_sub <- hchart(cluster3)
plot(sub)
viz_sub

# Subgraph metrics
vcount(sub) # Number of nodes
ecount(sub) # Number of links
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = TRUE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

# Compute the clustering on the graph
wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) # mean number of links
assortativity_degree(sub,directed = F) #assortativity
mean(1 - E(sub)$weight) # Dissilarity as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # natural connectivity

# Compute the hubs
hubs <- eigen_centrality(
  sub,
  directed = FALSE,
  scale = TRUE,
  weights = NULL
)
Hubs <- as.data.frame(hubs$vector)
Hubs$Phyto <- rownames(Hubs)

Spe_hubs <- Hubs[order(desc(hubs$vector)),]
Spe_hubs
# Store the results 
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster3_automne.csv", row.names = FALSE,dec = ".")

# Manip to have the composition of the graph in term of phyla
# Creating a df to store the results
compo_reseau <- c("","")
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau <- wcini$names
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau$cluster <- wcini$membership
colnames(compo_reseau)[1] <- "Taxon"
# Import the taxonomic information
PhyClasse <- read.csv('data_modif/Liste_phylum.classe_REPHY_JYmodif.csv', sep =';', header = TRUE, fileEncoding = 'ISO-8859-1')
compo_reseau <- left_join(compo_reseau,PhyClasse)
compo_reseau$Nb <- 1
colnames(Spe_hubs)[2] <- "Taxon"
compo_reseau <- left_join(Spe_hubs,compo_reseau)

compo_reseau <- compo_reseau |>
  group_by(cluster,Phylum.Classe) |>
  summarise(Nb = sum(Nb))
write.csv2(compo_reseau,file="data_modif/compo_reseau_cluster3_automne.csv", row.names = FALSE,dec = ".")

# Spring #
CL3 <- filter(data, cluster == 3,season == "Spring" )
nbdate <- nrow(CL3)*0.33
CL3 <- dplyr::select(CL3,Actinoptychus:Coscinodiscophycidae)

CL3[is.na(CL3)]<- 0
CL3 <- ifelse(CL3 > 0, 1,0)
Spe_w <- as.data.frame(colSums(CL3))

# Prepare the list of the present genus
Spe_w$Phyto <- rownames(Spe_w)
colnames(Spe_w)[1] <- "V1"
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- filter(Spe_w,V1 > nbdate)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index of the species
nodes_net <- V(cluster3)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)
Spe <- filter(Spe,!is.na(Spe$Count))

# Compute the subgraph
vids <- Spe$Pindex
compo_reseau_clus3$Pres <- "Non"
compo_reseau_clus3[vids,]$Pres <- "Oui"
compo_reseau_clus3$color <- "grey"
compo_reseau_clus3$color[compo_reseau_clus3$Phylum.Classe == "Bacillariophyceae"] <- "#56B4E9"
compo_reseau_clus3$color[compo_reseau_clus3$Phylum.Classe == "Dinophyceae"] <- "#009E73"
compo_reseau_clus3$color[compo_reseau_clus3$Phylum.Classe == "Ciliophora"] <-"#F0E442"
compo_reseau_clus3$color[compo_reseau_clus3$Phylum.Classe == "Cryptophyceae"] <-"#CC79A7"
compo_reseau_clus3$color[compo_reseau_clus3$Phylum.Classe == "Haptophyta"] <-"#996136"
compo_reseau_clus3$color[compo_reseau_clus3$Pres == "Non"] <- "transparent"

present <- compo_reseau_clus3$Taxon[compo_reseau_clus3$Pres == "Oui"]

dataabd <- as.data.frame(colSums(CL3))
dataabd$Taxon <- rownames(dataabd)
colnames(dataabd)[1] <- "Abd"
compo_reseau <- left_join(compo_reseau_clus3,dataabd)

liens_df <- get.data.frame(cluster3, what = "edges")
liens_df$color <- "black"
liens_df$color[!liens_df$from %in% present  ] <- "transparent"
liens_df$color[!liens_df$to %in% present  ] <- "transparent"

# Personalize the graph
V(cluster3)$label <- V(cluster3)$name
V(cluster3)$Phylum <- compo_reseau$Phylum.Classe
V(cluster3)$N_liens <- degree(cluster3)
V(cluster3)$size <- log(compo_reseau$Abd)
V(cluster3)$Module <- membership(wcini)
V(cluster3)$color <- compo_reseau$color
E(cluster3)$width <- E(cluster3)$weight*6
E(cluster3)$color <- liens_df$color
cluster3$layout <- layout_fr

sub <- igraph::subgraph(cluster3, vids)
sub$layout <- layout_fr[vids,]
viz_sub <- hchart(cluster3)
plot(sub)
viz_sub

# SUbgraph metrics
vcount(sub) # Number of nodes
ecount(sub) # Number of links
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = TRUE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

# Compute the clustering on the graph
wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) # mean number of links
assortativity_degree(sub,directed = F) #assortativity
mean(1 - E(sub)$weight) # Dissilarity as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # natural connectivity

# Compute the hubs
hubs <- eigen_centrality(
  sub,
  directed = FALSE,
  scale = TRUE,
  weights = NULL
)
Hubs <- as.data.frame(hubs$vector)
Hubs$Phyto <- rownames(Hubs)

Spe_hubs <- Hubs[order(desc(hubs$vector)),]
Spe_hubs
# Store the results 
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster3_printemps.csv", row.names = FALSE,dec = ".")

# Manip to have the composition of the graph in term of phyla
# Creating a df to store the results
compo_reseau <- c("","")
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau <- wcini$names
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau$cluster <- wcini$membership
colnames(compo_reseau)[1] <- "Taxon"
# Import the taxonomic information
PhyClasse <- read.csv('data_modif/Liste_phylum.classe_REPHY_JYmodif.csv', sep =';', header = TRUE, fileEncoding = 'ISO-8859-1')
compo_reseau <- left_join(compo_reseau,PhyClasse)
compo_reseau$Nb <- 1
colnames(Spe_hubs)[2] <- "Taxon"
compo_reseau <- left_join(Spe_hubs,compo_reseau)

compo_reseau <- compo_reseau |>
  group_by(cluster,Phylum.Classe) |>
  summarise(Nb = sum(Nb))
write.csv2(compo_reseau,file="data_modif/compo_reseau_cluster3_printemps.csv", row.names = FALSE,dec = ".")

# Summer #
CL3 <- filter(data, cluster == 3,season == "Summer" )
nbdate <- nrow(CL3)*0.33
CL3 <- dplyr::select(CL3,Actinoptychus:Coscinodiscophycidae)

CL3[is.na(CL3)]<- 0
CL3 <- ifelse(CL3 > 0, 1,0)
Spe_w <- as.data.frame(colSums(CL3))

# Prepare the list of the present genus
Spe_w$Phyto <- rownames(Spe_w)
colnames(Spe_w)[1] <- "V1"
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- filter(Spe_w,V1 > nbdate)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index of the species
nodes_net <- V(cluster3)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)
Spe <- filter(Spe,!is.na(Spe$Count))

# Compute the subgraph
vids <- Spe$Pindex
compo_reseau_clus3$Pres <- "Non"
compo_reseau_clus3[vids,]$Pres <- "Oui"
compo_reseau_clus3$color <- "grey"
compo_reseau_clus3$color[compo_reseau_clus3$Phylum.Classe == "Bacillariophyceae"] <- "#56B4E9"
compo_reseau_clus3$color[compo_reseau_clus3$Phylum.Classe == "Dinophyceae"] <- "#009E73"
compo_reseau_clus3$color[compo_reseau_clus3$Phylum.Classe == "Ciliophora"] <-"#F0E442"
compo_reseau_clus3$color[compo_reseau_clus3$Phylum.Classe == "Cryptophyceae"] <-"#CC79A7"
compo_reseau_clus3$color[compo_reseau_clus3$Phylum.Classe == "Haptophyta"] <-"#996136"
compo_reseau_clus3$color[compo_reseau_clus3$Pres == "Non"] <- "transparent"

present <- compo_reseau_clus3$Taxon[compo_reseau_clus3$Pres == "Oui"]

dataabd <- as.data.frame(colSums(CL3))
dataabd$Taxon <- rownames(dataabd)
colnames(dataabd)[1] <- "Abd"
compo_reseau <- left_join(compo_reseau_clus3,dataabd)

liens_df <- get.data.frame(cluster3, what = "edges")
liens_df$color <- "black"
liens_df$color[!liens_df$from %in% present  ] <- "transparent"
liens_df$color[!liens_df$to %in% present  ] <- "transparent"

# Personalize the graph
V(cluster3)$label <- V(cluster3)$name
V(cluster3)$Phylum <- compo_reseau$Phylum.Classe
V(cluster3)$N_liens <- degree(cluster3)
V(cluster3)$size <- log(compo_reseau$Abd)
V(cluster3)$Module <- membership(wcini)
V(cluster3)$color <- compo_reseau$color
E(cluster3)$width <- E(cluster3)$weight*6
E(cluster3)$color <- liens_df$color
cluster3$layout <- layout_fr

sub <- igraph::subgraph(cluster3, vids)
sub$layout <- layout_fr[vids,]
viz_sub <- hchart(cluster3)
plot(sub)
viz_sub

# Subgraph metrics
vcount(sub) # Number of nodes
ecount(sub) # Number of links
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = TRUE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

# Compute the clustering on the graph
wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) # mean number of links
assortativity_degree(sub,directed = F) #assortativity
mean(1 - E(sub)$weight) # Dissilarity as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # natural connectivity

# Compute the hubs
hubs <- eigen_centrality(
  sub,
  directed = FALSE,
  scale = TRUE,
  weights = NULL
)
Hubs <- as.data.frame(hubs$vector)
Hubs$Phyto <- rownames(Hubs)

Spe_hubs <- Hubs[order(desc(hubs$vector)),]
Spe_hubs
# Store the results 
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster3_summer.csv", row.names = FALSE,dec = ".")

# Manip to have the composition of the graph in term of phyla
# Creating a df to store the results
compo_reseau <- c("","")
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau <- wcini$names
compo_reseau <- as.data.frame(compo_reseau)
compo_reseau$cluster <- wcini$membership
colnames(compo_reseau)[1] <- "Taxon"
# Import the taxonomic information
PhyClasse <- read.csv('data_modif/Liste_phylum.classe_REPHY_JYmodif.csv', sep =';', header = TRUE, fileEncoding = 'ISO-8859-1')
compo_reseau <- left_join(compo_reseau,PhyClasse)
compo_reseau$Nb <- 1
colnames(Spe_hubs)[2] <- "Taxon"
compo_reseau <- left_join(Spe_hubs,compo_reseau)

compo_reseau <- compo_reseau |>
  group_by(cluster,Phylum.Classe) |>
  summarise(Nb = sum(Nb))
write.csv2(compo_reseau,file="data_modif/compo_reseau_cluster3_ete.csv", row.names = FALSE,dec = ".")


# Networks test difference ####
# Merge all the cluster's metrics
metric1 <- read_delim("data_modif/results_metrics_reseaux_cluster1_pos.csv", 
                                                              delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                                                              locale = locale(decimal_mark = ",", grouping_mark = "."), 
                                                              trim_ws = TRUE)
metric1$cluster <- 1

metric2 <- read_delim("data_modif/results_metrics_reseaux_cluster2_pos.csv", 
                      delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                      locale = locale(decimal_mark = ",", grouping_mark = "."), 
                      trim_ws = TRUE)
metric2$cluster <- 2

metric3 <- read_delim("data_modif/results_metrics_reseaux_cluster3_pos.csv", 
                      delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                      locale = locale(decimal_mark = ",", grouping_mark = "."), 
                      trim_ws = TRUE)
metric3$cluster <- 3



metric <- bind_rows(metric1,metric2,metric3)
write.csv2(metric,file="data_modif/metrics_final.csv", row.names = FALSE,dec = ".")

# Import data 
metric <- read_delim("data_modif/metrics_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF")

# Boxplots 
colnames(metric)[7] <- "LML"
datal <- pivot_longer(metric,names_to = "Var",cols = N_noeuds:N_clust)
datal2 <- filter(datal, Var %in% c("Assort","LML","C_tance","D_liens","Mod","Nat_connect","N_noeuds","Adhes"))
datal2$cluster <- as.character(datal2$cluster)
ggplot(datal2)+
  geom_boxplot(aes(x=cluster,y=value,group=cluster,fill=cluster),size = 1)+
  scale_fill_manual(values=cluster_col,guide = "none")+
  facet_wrap(~Var,scales = "free_y",ncol=4)+
  labs(title = "Metriques (+)")

# Make the graph I want
noeuds <- ggplot(filter(datal2,Var == "N_noeuds"))+
  geom_boxplot(aes(x=cluster,y=value,group=cluster,fill=cluster),size = 1)+
  scale_fill_manual(values=cluster_col,guide = "none")+
  facet_wrap(~Var,scales = "free_y",ncol=4)+
  labs(x=NULL,y="")+
  scale_y_continuous(breaks = seq(0,40,10),limits = c(0,50))

Natconnect <- ggplot(filter(datal2,Var == "Nat_connect"))+
  geom_boxplot(aes(x=cluster,y=value,group=cluster,fill=cluster),size = 1)+
  scale_fill_manual(values=cluster_col,guide = "none")+
  facet_wrap(~Var,scales = "free_y",ncol=4)+
  labs(x=NULL,y=NULL)+
  scale_y_continuous(breaks = seq(0,1,0.25),limits = c(0,1.20))

densite <- ggplot(filter(datal2,Var == "D_liens"))+
  geom_boxplot(aes(x=cluster,y=value,group=cluster,fill=cluster),size = 1)+
  scale_fill_manual(values=cluster_col,guide = "none")+
  facet_wrap(~Var,scales = "free_y",ncol=4)+
  labs(x=NULL,y=NULL)+
  scale_y_continuous(breaks = seq(0,20,5),limits = c(0,18))

LML <- ggplot(filter(datal2,Var == "LML"))+
  geom_boxplot(aes(x=cluster,y=value,group=cluster,fill=cluster),size = 1)+
  scale_fill_manual(values=cluster_col,guide = "none")+
  facet_wrap(~Var,scales = "free_y",ncol=4)+
  labs(x=NULL,y="Mesure")+
  scale_y_continuous(breaks = seq(0,0.75,0.25),limits = c(0,0.85))

ctance <- ggplot(filter(datal2,Var == "C_tance"))+
  geom_boxplot(aes(x=cluster,y=value,group=cluster,fill=cluster),size = 1)+
  scale_fill_manual(values=cluster_col,guide = "none")+
  facet_wrap(~Var,scales = "free_y",ncol=4)+
  labs(x=NULL,y=NULL)+
  scale_y_continuous(breaks = seq(0,1,0.25),limits = c(0,1.2))

assort <- ggplot(filter(datal2,Var == "Assort"))+
  geom_boxplot(aes(x=cluster,y=value,group=cluster,fill=cluster),size = 1)+
  scale_fill_manual(values=cluster_col,guide = "none")+
  facet_wrap(~Var,scales = "free_y",ncol=4)+
  labs(x="Région",y="")+
  scale_y_continuous(breaks = seq(0,1,0.25),limits = c(0,1.2))

mod <- ggplot(filter(datal2,Var == "Mod"))+
  geom_boxplot(aes(x=cluster,y=value,group=cluster,fill=cluster),size = 1)+
  scale_fill_manual(values=cluster_col,guide = "none")+
  facet_wrap(~Var,scales = "free_y",ncol=4)+
  labs(x="Région",y=NULL)+
  scale_y_continuous(breaks = seq(0,0.5,0.25),limits = c(0,0.60))

metriquesgraph <- plot_grid(noeuds,Natconnect,LML,ctance,assort,mod,ncol = 2)

#rm(list = ls()[!ls() == "metriquesgraph"])
#save.image("C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/metriquesgraphpouralleravecbraycurtis.RData")

####### Saisonnality 

data <- read_delim("data_modif/metrics_final.csv", 
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
colnames(data)[7] <- "LML"
datal <- pivot_longer(data,names_to = "Var",cols = N_noeuds:N_clust)
datal2 <- filter(datal, Var %in% c("Assort","LML","C_tance","D_liens","Mod","Nat_connect","N_noeuds","Adhes"))
datal2$cluster <- as.character(datal2$cluster)
ggplot(datal2)+
  geom_boxplot(aes(x=cluster,y=value,group=cluster,fill=cluster),size = 1)+
  scale_fill_manual(values=cluster_col,guide = "none")+
  facet_wrap(~Var,scales = "free_y",ncol=4)+
  labs(title = "Metriques (+)")



# Kruskal-Wallis test
# Assortativity
datam <- filter(datal2, Var == "Assort")
kruskal.test(datam$value~datam$cluster)
DunnTest(datam$value~datam$cluster, method = "BH")
# Modularity
datam <- filter(datal2, Var == "Mod")
kruskal.test(datam$value~datam$cluster)
DunnTest(datam$value~datam$cluster, method = "BH")
# Average path length
datam <- filter(datal2, Var == "LML")
kruskal.test(datam$value~datam$cluster)
DunnTest(datam$value~datam$cluster, method = "BH")
# Density
datam <- filter(datal2, Var == "D_liens")
kruskal.test(datam$value~datam$cluster)
DunnTest(datam$value~datam$cluster, method = "BH")
# Connectance
datam <- filter(datal2, Var == "C_tance")
kruskal.test(datam$value~datam$cluster)
DunnTest(datam$value~datam$cluster, method = "BH")
# Number of nodes
datam <- filter(datal2, Var == "N_noeuds")
kruskal.test(datam$value~datam$cluster)
DunnTest(datam$value~datam$cluster, method = "BH")
# Adhesion
datam <- filter(datal2, Var == "Adhes")
kruskal.test(datam$value~datam$cluster)
DunnTest(datam$value~datam$cluster, method = "BH")
# Natural connecivity
datam <- filter(datal2, Var == "Nat_connect")
kruskal.test(datam$value~datam$cluster)
DunnTest(datam$value~datam$cluster, method = "BH")

# RDA metrics explained by hydro factors #####
# Import data
metric <- read_delim("data_modif/metrics_final.csv", 
                     delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                     locale = locale(decimal_mark = ",", grouping_mark = "."), 
                     trim_ws = TRUE)
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_withoutliers_bloomid_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

## Working on the cluster 1 #####
data1 <- filter(data,cluster == 1)
metric1 <- filter(metric,cluster == 1)

corrdata <- left_join(metric1,data1)
corrdata <- corrdata[-c(3206:3208),]
colnames(corrdata)[14] <- "LML"

corrdata <- dplyr::select(corrdata,SALI:`TURB-FNU`,-TURB,Assort,LML,C_tance,D_liens,Mod,Nat_connect,N_noeuds)
ncomp <- estim_ncpPCA(corrdata)
# Imputation of NA's
res.imp <- imputePCA(corrdata, ncp = ncomp$ncp,method = "EM")
corrdata <- as.data.frame(res.imp$completeObs)

r <- cor(corrdata)

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
p.mat <- cor.mtest(corrdata)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(r, method="color", col=col(200),  
         type="upper", order="alphabet",
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=F
)


## Working on the cluster 2 #####
data2 <- filter(data,cluster == 2)
metric2 <- filter(metric,cluster == 2)

corrdata <- left_join(metric2,data2)
corrdata <- corrdata[-c(903:905),]
colnames(corrdata)[14] <- "LML"

corrdata <- dplyr::select(corrdata,SALI:`TURB-FNU`,-TURB,Assort,LML,C_tance,D_liens,Mod,Nat_connect,N_noeuds)
ncomp <- estim_ncpPCA(corrdata)
# Imputation of NA's
res.imp <- imputePCA(corrdata, ncp = ncomp$ncp,method = "EM")
corrdata <- as.data.frame(res.imp$completeObs)

r <- cor(corrdata)

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
p.mat <- cor.mtest(corrdata)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(r, method="color", col=col(200),  
         type="upper", order="alphabet",
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=F
)

## Working on the cluster 3 #####
data3 <- filter(data,cluster == 3)
metric3 <- filter(metric,cluster == 3)

corrdata <- left_join(metric3,data3)
colnames(corrdata)[14] <- "LML"

corrdata <- dplyr::select(corrdata,SALI:`TURB-FNU`,-TURB,Assort,LML,C_tance,D_liens,Mod,Nat_connect,N_noeuds)
ncomp <- estim_ncpPCA(corrdata)
# Imputation of NA's
res.imp <- imputePCA(corrdata, ncp = ncomp$ncp,method = "EM")
corrdata <- as.data.frame(res.imp$completeObs)

r <- cor(corrdata)

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
p.mat <- cor.mtest(corrdata)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(r, method="color", col=col(200),  
         type="upper", order="alphabet",
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=F
)

## Working on the cluster 1 #####
data1 <- filter(data,cluster == 1)
metric1 <- filter(metric,cluster == 1)

corrdata <- left_join(metric1,data1)
# Delete the duplicates
corrdata <- corrdata[-c(3206:3208),]
# Rename a column
colnames(corrdata)[14] <- "LML"

corrdata <- dplyr::select(corrdata,Rspe,Shannon, Pielou, BergerParker,CHLOROA,Assort,C_tance,D_liens,LML,Mod,N_noeuds,Nat_connect)
ncomp <- estim_ncpPCA(corrdata)
# Imputation of NA's
res.imp <- imputePCA(corrdata, ncp = ncomp$ncp,method = "EM")
corrdata <- as.data.frame(res.imp$completeObs)

r <- cor(corrdata)

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
p.mat <- cor.mtest(corrdata)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(r, method="color", col=col(200),  
         type="upper", order="original",
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=F
)


## Working on the cluster 2 #####
data2 <- filter(data,cluster == 2)
metric2 <- filter(metric,cluster == 2)

corrdata <- left_join(metric2,data2)
# Delete some duplicates
corrdata <- corrdata[-c(903:905),]
colnames(corrdata)[14] <- "LML"

corrdata <- dplyr::select(corrdata,Rspe,Shannon, Pielou, BergerParker,CHLOROA,Assort,C_tance,D_liens,LML,Mod,N_noeuds,Nat_connect)
ncomp <- estim_ncpPCA(corrdata)
# Imputation of NA's
res.imp <- imputePCA(corrdata, ncp = ncomp$ncp,method = "EM")
corrdata <- as.data.frame(res.imp$completeObs)

r <- cor(corrdata)

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
p.mat <- cor.mtest(corrdata)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(r, method="color", col=col(200),  
         type="upper", order="original",
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=F
)

## Working on the cluster 3 #####
data3 <- filter(data,cluster == 3)
metric3 <- filter(metric,cluster == 3)

corrdata <- left_join(metric3,data3)
colnames(corrdata)[14] <- "LML"

corrdata <- dplyr::select(corrdata,Rspe,Shannon, Pielou, BergerParker,CHLOROA,Assort,C_tance,D_liens,LML,Mod,N_noeuds,Nat_connect)
ncomp <- estim_ncpPCA(corrdata)
# Imputation of NA's
res.imp <- imputePCA(corrdata, ncp = ncomp$ncp,method = "EM")
corrdata <- as.data.frame(res.imp$completeObs)

r <- cor(corrdata)

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
p.mat <- cor.mtest(corrdata)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(r, method="color", col=col(200),  
         type="upper", order="original",
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=F
)

