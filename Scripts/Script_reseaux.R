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

### Cluster 4 ####
# Select cluster 4
CL4 <- filter(data, cluster == 4 )

# Select all genus
CL4 <- dplyr::select(CL4,Actinoptychus:Coscinodiscophycidae)

# Replace NA's by 0 to make it uniform
CL4[is.na(CL4)]<- 0

CL4 <- as.matrix(CL4)

# Build the global network
net_cluster4 <- netConstruct(data = CL4, dataType = "counts",measure = "spearman", 
                             filtTax = "numbSamp",filtTaxPar = list(numbSamp = 30),
                             filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                             normMethod = "none", dissFunc = "signed")

# Build info about the graph
net_props_cluster4 <- netAnalyze(net_cluster4,
                                 clustMethod = "cluster_fast_greedy",
                                 hubPar = c("eigenvector"),
                                 graphlet = F,
                                 connectivity = T)
# Plot the network
plt <- plot(net_props_cluster4,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Cluster 4",
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
wc <- cluster_fast_greedy(cluster1)

# Personalize the graph
V(cluster1)$label <- V(cluster1)$name
#V(cluster1)$name <- paste("I'm #", net$edgelist1$v1)
V(cluster1)$page_rank <- round(page.rank(cluster1)$vector, 2)
V(cluster1)$betweenness <- round(betweenness(cluster1), 2)
V(cluster1)$degree <- degree(cluster1)
V(cluster1)$size <- V(cluster1)$degree *0.3
V(cluster1)$comm <- membership(wc)
V(cluster1)$color <- colorize(membership(wc),colors = c("orange","olivedrab","turquoise"))
E(cluster1)$width <- E(cluster1)$weight*6
E(cluster1)$color <- "black"

# Plot it
plot(cluster1)
# Other way
viz1 <- hchart(cluster1, layout = layout_with_fr)
viz1
# Save it
htmlwidgets::saveWidget(viz1, "output/graphs/Reseaux/HTML/cluster1.html")

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

Modularity <- modularity(cluster1,membership = membership(wc)) # Modularity

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
  print(i/nrow(CL1)*100)
}
# Save the result
write.csv2(data_results_reseaux,file="data_modif/results_metrics_reseaux_cluster1_pos.csv", row.names = FALSE,dec = ".")

# Result analysis
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
                   D_liens=mean(D_liens,na.rm=T),C_tance=mean(C_tance,na.rm=T),Avg_p_length=mean(Avg_p_length,na.rm=T),
                   Adhes=mean(Adhes,na.rm=T),Mod=mean(Mod,na.rm=T),meanN_liens=mean(meanN_liens,na.rm=T),
                   Assort=mean(Assort,na.rm=T),Diss=mean(Diss,na.rm=T),Trans=mean(Trans,na.rm=T),
                   meanN_voisins=mean(meanN_voisins,na.rm=T),Nat_connect=mean(Nat_connect,na.rm=T),N_clust=mean(N_clust,na.rm=T))


# Metrics seasonalities
datal <- pivot_longer(datam,names_to = "Var",cols = N_noeuds:N_clust)
ggplot(datal)+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#F8766D",size = 1)+
  facet_wrap(~Var,scales = "free_y")+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  labs(title = "Metriques réseau cluster 1 (+)")
ggsave('Metriques_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')

# Trends
ggplot(datal)+
  geom_boxplot(aes(x=Year,y=value,group=Year),fill="#F8766D",size = 1)+
  facet_wrap(~Var,scales = "free_y")+
  labs(title = "Metriques réseau cluster 1 (+)")
ggsave('Metriques_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')



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
ggsave('Metriques_cluster_ts.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')



##### Subgraph by season ####
data<- data |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))

# Winter #
CL1 <- filter(data, cluster == 1,season == "Winter" )
CL1 <- dplyr::select(CL1,Actinoptychus:Coscinodiscophycidae)
# Compute the mean of all the genus
Spe_w <- as.data.frame(t(summarise_all(CL1,.funs = list(mean = ~mean(., na.rm = TRUE)))))

# Prepare the list of the present genus
Spe_w$Phyto <- colnames(CL1)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
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

# Compute the subgraph
vids <- Spe$Pindex
sub <- igraph::subgraph(cluster1, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster1_hiver.csv", row.names = FALSE,dec = ".")

# Fall #
CL1 <- filter(data, cluster == 1,season == "Fall" )
CL1 <- dplyr::select(CL1,Actinoptychus:Coscinodiscophycidae)
# Compute the mean of all the genus
Spe_w <- as.data.frame(t(summarise_all(CL1,.funs = list(mean = ~mean(., na.rm = TRUE)))))

# Prepare the list of the present genus
Spe_w$Phyto <- colnames(CL1)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
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

# Compute the subgraph
vids <- Spe$Pindex
sub <- igraph::subgraph(cluster1, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
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

# Spring #
CL1 <- filter(data, cluster == 1,season == "Spring" )
CL1 <- dplyr::select(CL1,Actinoptychus:Coscinodiscophycidae)
# Compute the mean of all the genus
Spe_w <- as.data.frame(t(summarise_all(CL1,.funs = list(mean = ~mean(., na.rm = TRUE)))))

# Prepare the list of the present genus
Spe_w$Phyto <- colnames(CL1)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
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

# Compute the subgraph
vids <- Spe$Pindex
sub <- igraph::subgraph(cluster1, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
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

# Summer #
CL1 <- filter(data, cluster == 1,season == "Summer" )
CL1 <- dplyr::select(CL1,Actinoptychus:Coscinodiscophycidae)
# Compute the mean of all the genus
Spe_w <- as.data.frame(t(summarise_all(CL1,.funs = list(mean = ~mean(., na.rm = TRUE)))))

# Prepare the list of the present genus
Spe_w$Phyto <- colnames(CL1)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
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

# Compute the subgraph
vids <- Spe$Pindex
sub <- igraph::subgraph(cluster1, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
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

#### Working on the cluster 2 #####

# We keep only positive correlations
assoMat <- net_cluster2$assoMat1
assoMat[assoMat < 0] <- 0

cluster2 <- graph_from_adjacency_matrix(assoMat,weighted = T,mode = "undirected",diag=F)

# Compute clustering on the graph
wc <- cluster_fast_greedy(cluster2)

# Personalize the graph
V(cluster2)$label <- V(cluster2)$name
#V(cluster2)$name <- paste("I'm #", net$edgelist1$v1)
V(cluster2)$page_rank <- round(page.rank(cluster2)$vector, 2)
V(cluster2)$betweenness <- round(betweenness(cluster2), 2)
V(cluster2)$degree <- degree(cluster2)
V(cluster2)$size <- V(cluster2)$degree *0.3
V(cluster2)$comm <- membership(wc)
V(cluster2)$color <- colorize(membership(wc),colors = c("orange","olivedrab","turquoise"))
E(cluster2)$width <- E(cluster2)$weight*6
E(cluster2)$color <- "black"

# Plot it
plot(cluster2)
# Other way
viz2 <- hchart(cluster2, layout = layout_with_fr)
viz2
# Save it
htmlwidgets::saveWidget(viz2, "output/graphs/Reseaux/HTML/cluster2.html")

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

Modularity <- modularity(cluster2,membership = membership(wc)) # Modularity

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
  print(i/nrow(CL2)*100)
}
# Save the result
write.csv2(data_results_reseaux,file="data_modif/results_metrics_reseaux_cluster2_pos.csv", row.names = FALSE,dec = ".")

# Result analysis
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
                   D_liens=mean(D_liens,na.rm=T),C_tance=mean(C_tance,na.rm=T),Avg_p_length=mean(Avg_p_length,na.rm=T),
                   Adhes=mean(Adhes,na.rm=T),Mod=mean(Mod,na.rm=T),meanN_liens=mean(meanN_liens,na.rm=T),
                   Assort=mean(Assort,na.rm=T),Diss=mean(Diss,na.rm=T),Trans=mean(Trans,na.rm=T),
                   meanN_voisins=mean(meanN_voisins,na.rm=T),Nat_connect=mean(Nat_connect,na.rm=T),N_clust=mean(N_clust,na.rm=T))


# Metrics seasonalities
datal <- pivot_longer(datam,names_to = "Var",cols = N_noeuds:N_clust)
ggplot(datal)+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#CD9600",size = 1)+
  facet_wrap(~Var,scales = "free_y")+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  labs(title = "Metriques réseau cluster 2 (+)")
ggsave('Metriques_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')

# Trends
ggplot(datal)+
  geom_boxplot(aes(x=Year,y=value,group=Year),fill="#CD9600",size = 1)+
  facet_wrap(~Var,scales = "free_y")+
  labs(title = "Metriques réseau cluster 2 (+)")
ggsave('Metriques_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')



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
ggsave('Metriques_cluster_ts.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')



##### Subgraph by season ####
data<- data |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))

# Winter #
CL2 <- filter(data, cluster == 2,season == "Winter" )
CL2 <- dplyr::select(CL2,Actinoptychus:Coscinodiscophycidae)
# Compute the mean of all the genus
Spe_w <- as.data.frame(t(summarise_all(CL2,.funs = list(mean = ~mean(., na.rm = TRUE)))))

# Prepare the list of the present genus
Spe_w$Phyto <- colnames(CL2)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
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

# Compute the subgraph
vids <- Spe$Pindex
sub <- igraph::subgraph(cluster2, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster2_hiver.csv", row.names = FALSE,dec = ".")

# Fall #
CL2 <- filter(data, cluster == 2,season == "Fall" )
CL2 <- dplyr::select(CL2,Actinoptychus:Coscinodiscophycidae)
# Compute the mean of all the genus
Spe_w <- as.data.frame(t(summarise_all(CL2,.funs = list(mean = ~mean(., na.rm = TRUE)))))

# Prepare the list of the present genus
Spe_w$Phyto <- colnames(CL2)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
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

# Compute the subgraph
vids <- Spe$Pindex
sub <- igraph::subgraph(cluster2, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
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

# Spring #
CL2 <- filter(data, cluster == 2,season == "Spring" )
CL2 <- dplyr::select(CL2,Actinoptychus:Coscinodiscophycidae)
# Compute the mean of all the genus
Spe_w <- as.data.frame(t(summarise_all(CL2,.funs = list(mean = ~mean(., na.rm = TRUE)))))

# Prepare the list of the present genus
Spe_w$Phyto <- colnames(CL2)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
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

# Compute the subgraph
vids <- Spe$Pindex
sub <- igraph::subgraph(cluster2, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
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

# Summer #
CL2 <- filter(data, cluster == 2,season == "Summer" )
CL2 <- dplyr::select(CL2,Actinoptychus:Coscinodiscophycidae)
# Compute the mean of all the genus
Spe_w <- as.data.frame(t(summarise_all(CL2,.funs = list(mean = ~mean(., na.rm = TRUE)))))

# Prepare the list of the present genus
Spe_w$Phyto <- colnames(CL2)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
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

# Compute the subgraph
vids <- Spe$Pindex
sub <- igraph::subgraph(cluster2, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster2_summer.csv", row.names = FALSE,dec = ".")



#### Working on the cluster 3 #####

# We keep only positive correlations
assoMat <- net_cluster3$assoMat1
assoMat[assoMat < 0] <- 0

cluster3 <- graph_from_adjacency_matrix(assoMat,weighted = T,mode = "undirected",diag=F)

# Compute clustering on the graph
wc <- cluster_fast_greedy(cluster3)

# Personalize the graph
V(cluster3)$label <- V(cluster3)$name
#V(cluster3)$name <- paste("I'm #", net$edgelist1$v1)
V(cluster3)$page_rank <- round(page.rank(cluster3)$vector, 2)
V(cluster3)$betweenness <- round(betweenness(cluster3), 2)
V(cluster3)$degree <- degree(cluster3)
V(cluster3)$size <- V(cluster3)$degree *0.3
V(cluster3)$comm <- membership(wc)
V(cluster3)$color <- colorize(membership(wc),colors = c("orange","olivedrab","turquoise"))
E(cluster3)$width <- E(cluster3)$weight*6
E(cluster3)$color <- "black"

# Plot it
plot(cluster3)
# Other way
viz3 <- hchart(cluster3, layout = layout_with_fr)
viz3
# Save it
htmlwidgets::saveWidget(viz3, "output/graphs/Reseaux/HTML/cluster3.html")

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

Modularity <- modularity(cluster3,membership = membership(wc)) # Modularity

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
print(i/nrow(CL3)*100)
}
# Save the result
write.csv2(data_results_reseaux,file="data_modif/results_metrics_reseaux_cluster3_pos.csv", row.names = FALSE,dec = ".")

# Result analysis
data_results_reseaux_copy <- data_results_reseaux

bloom <- dplyr::select(CL3df,Code_point_Libelle,Date,Bloom_Phylum,P_dominance)

data_reseaux <- left_join(data_results_reseaux,bloom)

data_reseaux <- data_reseaux %>%
  mutate(Month = month(Date, label = F)) |>
  mutate(Year = year(Date))

data_reseaux$Date2 <- as.Date(paste(data_reseaux$Year, data_reseaux$Month, "01", sep = "-"), format = "%Y-%m-%d")

datam <- summarise(group_by(data_reseaux,Code_point_Libelle,Month,Year), N_noeuds=mean(N_noeuds,na.rm=T),N_liens=mean(N_liens,na.rm=T),
                   D_liens=mean(D_liens,na.rm=T),C_tance=mean(C_tance,na.rm=T),Avg_p_length=mean(Avg_p_length,na.rm=T),
                   Adhes=mean(Adhes,na.rm=T),Mod=mean(Mod,na.rm=T),meanN_liens=mean(meanN_liens,na.rm=T),
                   Assort=mean(Assort,na.rm=T),Diss=mean(Diss,na.rm=T),Trans=mean(Trans,na.rm=T),
                   meanN_voisins=mean(meanN_voisins,na.rm=T),Nat_connect=mean(Nat_connect,na.rm=T),N_clust=mean(N_clust,na.rm=T))


# Metrics seasonnalities
datal <- pivot_longer(datam,names_to = "Var",cols = N_noeuds:N_clust)
ggplot(datal)+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#00BE67",size = 1)+
  facet_wrap(~Var,scales = "free_y")+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  labs(title = "Metriques réseau cluster 3 (+)")
ggsave('Metriques_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')

# Trends
ggplot(datal)+
  geom_boxplot(aes(x=Year,y=value,group=Year),fill="#00BE67",size = 1)+
  facet_wrap(~Var,scales = "free_y")+
  labs(title = "Metriques réseau cluster 3 (+)")
ggsave('Metriques_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')



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
ggsave('Metriques_cluster_ts.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')


##### Subgraph by season ####
data<- data |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))

# Winter #
CL3 <- filter(data, cluster == 3,season == "Winter" )
CL3 <- dplyr::select(CL3,Actinoptychus:Coscinodiscophycidae)
# Compute the mean of all the genus
Spe_w <- as.data.frame(t(summarise_all(CL3,.funs = list(mean = ~mean(., na.rm = TRUE)))))

# Prepare the list of the present genus
Spe_w$Phyto <- colnames(CL3)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
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

# Compute the subgraph
vids <- Spe$Pindex
sub <- igraph::subgraph(cluster3, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster3_hiver.csv", row.names = FALSE,dec = ".")

# Fall #
CL3 <- filter(data, cluster == 3,season == "Fall" )
CL3 <- dplyr::select(CL3,Actinoptychus:Coscinodiscophycidae)
# Compute the mean of all the genus
Spe_w <- as.data.frame(t(summarise_all(CL3,.funs = list(mean = ~mean(., na.rm = TRUE)))))

# Prepare the list of the present genus
Spe_w$Phyto <- colnames(CL3)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
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

# Compute the subgraph
vids <- Spe$Pindex
sub <- igraph::subgraph(cluster3, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster3_automne.csv", row.names = FALSE,dec = ".")

# Spring #
CL3 <- filter(data, cluster == 3,season == "Spring" )
CL3 <- dplyr::select(CL3,Actinoptychus:Coscinodiscophycidae)
# Compute the mean of all the genus
Spe_w <- as.data.frame(t(summarise_all(CL3,.funs = list(mean = ~mean(., na.rm = TRUE)))))

# Prepare the list of the present genus
Spe_w$Phyto <- colnames(CL3)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
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

# Compute the subgraph
vids <- Spe$Pindex
sub <- igraph::subgraph(cluster3, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster3_printemps.csv", row.names = FALSE,dec = ".")

# Summer #
CL3 <- filter(data, cluster == 3,season == "Summer" )
CL3 <- dplyr::select(CL3,Actinoptychus:Coscinodiscophycidae)
# Compute the mean of all the genus
Spe_w <- as.data.frame(t(summarise_all(CL3,.funs = list(mean = ~mean(., na.rm = TRUE)))))

# Prepare the list of the present genus
Spe_w$Phyto <- colnames(CL3)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
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

# Compute the subgraph
vids <- Spe$Pindex
sub <- igraph::subgraph(cluster3, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster3_summer.csv", row.names = FALSE,dec = ".")
#### Working on the cluster 4 #####

# We keep only positive correlations
assoMat <- net_cluster4$assoMat1
assoMat[assoMat < 0] <- 0

cluster4 <- graph_from_adjacency_matrix(assoMat,weighted = T,mode = "undirected",diag=F)

# Compute clustering on the graph
wc <- cluster_fast_greedy(cluster4)

# Personalize the graph
V(cluster4)$label <- V(cluster4)$name
#V(cluster4)$name <- paste("I'm #", net$edgelist1$v1)
V(cluster4)$page_rank <- round(page.rank(cluster4)$vector, 2)
V(cluster4)$betweenness <- round(betweenness(cluster4), 2)
V(cluster4)$degree <- degree(cluster4)
V(cluster4)$size <- V(cluster4)$degree *0.3
V(cluster4)$comm <- membership(wc)
V(cluster4)$color <- colorize(membership(wc),colors = c("orange","olivedrab","turquoise"))
E(cluster4)$width <- E(cluster4)$weight*6
E(cluster4)$color <- "black"

# Plot it
plot(cluster4)
# Other way
viz4 <- hchart(cluster4, layout = layout_with_fr)
viz4
# Save it
htmlwidgets::saveWidget(viz4, "output/graphs/Reseaux/HTML/cluster4.html")

# Global network metrics
S_net <-  vcount(cluster4) # Number of nodes
L_net <- ecount(cluster4) # Number of links

Z_mat <- L_net / S_net # linkage density or average number of links per nodes

C_net <- edge_density(cluster4, loops = FALSE) #connectance

# Average path length
avg_path_length <- mean_distance(
  cluster4,
  directed = FALSE,
  unconnected = TRUE # if the graphs is disconnected, only the existing paths are considered
)

Edge_connect <- edge.connectivity(cluster4) # Edge connectivity = adhesion

Modularity <- modularity(cluster4,membership = membership(wc)) # Modularity

m_degree <- mean(degree(cluster4)) #Mean number of links per node

assort <- assortativity_degree(cluster4,directed = F) #assortativity

diss <- mean(1 - E(cluster4)$weight) # Dissilarity as defined in NetCoMi

trans <- transitivity(cluster4,type = "global") #Transitivity

mean_edge_bet <- mean(edge_betweenness(cluster4)) # Mean edge betweeness

# Compute the natural connectivity 
adj <- as.matrix(as_adjacency_matrix(cluster4, attr = "weight",)) # OK
diag(adj) <- 1
nat_connect <- natural.connectivity(as.matrix(adj)) # Natural connectivity

# Define the different hubs
hubs <- eigen_centrality(
  cluster4,
  directed = FALSE,
  scale = TRUE,
  weights = NULL
)
Hubs <- as.data.frame(hubs$vector)
Hubs$Phyto <- rownames(Hubs)

Spe_hubs <- Hubs[order(desc(hubs$vector)),]
# Store the result
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster4.csv", row.names = FALSE,dec = ".")

##### Compute subgraph by day #######
# Preparation  
# Index of species
nodes_net <- V(cluster4)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

# Table preparation to retrieve station/date information
CL4df <- filter(data,cluster == 4)

# Creating a df to store the results
data_results_reseaux <- c("","")
data_results_reseaux <- as.data.frame(data_results_reseaux)

# Compute the subgraphs
for (i in 1:nrow(CL4)){ 
  spe <- as.data.frame(CL4[i,])
  colnames(spe) <- "Count" 
  spe$phyto <- rownames(spe)
  spe <- left_join(phyto_index,spe, by = join_by(phyto))
  spe <- filter(spe,Count>0)
  spe$Pindex <- as.numeric(spe$Pindex)
  
  station <- CL4df[i,]$Code_point_Libelle
  date <- CL4df[i,]$Date
  
  if (nrow(spe) != 0){
    vids <- spe$Pindex
    sub <- igraph::subgraph(cluster4, vids)
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
  #png(paste0("output/graphs/Reseaux/TS_CLUST4/",station,date,".png"))
  #plot(sub,main = paste0(station,date),layout = layout_with_fr)
  #dev.off()
  #htmlwidgets::saveWidget(viz_sub, paste0("output/graphs/Reseaux/HTML/",station,date,".html"))
  print(i/nrow(CL4)*100)
}
# Save the result
write.csv2(data_results_reseaux,file="data_modif/results_metrics_reseaux_cluster4_pos.csv", row.names = FALSE,dec = ".")

# Result analysis
data_results_reseaux_copy <- data_results_reseaux

bloom <- dplyr::select(CL4df,Code_point_Libelle,Date,Bloom_Phylum,P_dominance)

data_reseaux <- left_join(data_results_reseaux,bloom)

data_reseaux <- data_reseaux %>%
  mutate(Month = month(Date, label = F)) |>
  mutate(Year = year(Date))

data_reseaux$Date2 <- as.Date(paste(data_reseaux$Year, data_reseaux$Month, "01", sep = "-"), format = "%Y-%m-%d")

datam <- summarise(group_by(data_reseaux,Code_point_Libelle,Month,Year), N_noeuds=mean(N_noeuds,na.rm=T),N_liens=mean(N_liens,na.rm=T),
                   D_liens=mean(D_liens,na.rm=T),C_tance=mean(C_tance,na.rm=T),Avg_p_length=mean(Avg_p_length,na.rm=T),
                   Adhes=mean(Adhes,na.rm=T),Mod=mean(Mod,na.rm=T),meanN_liens=mean(meanN_liens,na.rm=T),
                   Assort=mean(Assort,na.rm=T),Diss=mean(Diss,na.rm=T),Trans=mean(Trans,na.rm=T),
                   meanN_voisins=mean(meanN_voisins,na.rm=T),Nat_connect=mean(Nat_connect,na.rm=T),N_clust=mean(N_clust,na.rm=T))


# Metrics seasonalities
datal <- pivot_longer(datam,names_to = "Var",cols = N_noeuds:N_clust)
ggplot(datal)+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#00A9FF",size = 1)+
  facet_wrap(~Var,scales = "free_y")+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  labs(title = "Metriques réseau cluster 4 (+)")
ggsave('Metriques_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')

# Trends
ggplot(datal)+
  geom_boxplot(aes(x=Year,y=value,group=Year),fill="#00A9FF",size = 1)+
  facet_wrap(~Var,scales = "free_y")+
  labs(title = "Metriques réseau cluster 4 (+)")
ggsave('Metriques_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')



CL4_Mmetrics <- summarise(group_by(data_reseaux,Code_point_Libelle,Date2),N_noeuds=mean(N_noeuds,na.rm=T),N_liens=mean(N_liens,na.rm=T),
                          D_liens=mean(D_liens,na.rm=T),C_tance=mean(C_tance,na.rm=T),Avg_p_length=mean(Avg_p_length,na.rm=T),
                          Adhes=mean(Adhes,na.rm=T),Mod=mean(Mod,na.rm=T),meanN_liens=mean(meanN_liens,na.rm=T),
                          Assort=mean(Assort,na.rm=T),Diss=mean(Diss,na.rm=T),Trans=mean(Trans,na.rm=T),
                          meanN_voisins=mean(meanN_voisins,na.rm=T),Nat_connect=mean(Nat_connect,na.rm=T),N_clust=mean(N_clust,na.rm=T))

# Time series (boxplot by year and mounth)
datal2 <- pivot_longer(CL4_Mmetrics,names_to = "Var",cols = N_noeuds:N_clust)
ggplot(datal2)+
  geom_boxplot(aes(x=Date2,y=value,group=Date2),fill="#00A9FF",size = 1)+
  facet_wrap(~Var,scales = "free_y")+
  labs(title = "Metriques réseau cluster 4 (+)")
ggsave('Metriques_cluster_ts.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')



##### Subgraph by season ####
data<- data |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))

# Winter #
CL4 <- filter(data, cluster == 4,season == "Winter" )
CL4 <- dplyr::select(CL4,Actinoptychus:Coscinodiscophycidae)
# Compute the mean of all the genus
Spe_w <- as.data.frame(t(summarise_all(CL4,.funs = list(mean = ~mean(., na.rm = TRUE)))))

# Prepare the list of the present genus
Spe_w$Phyto <- colnames(CL4)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index of the species
nodes_net <- V(cluster4)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)

# Compute the subgraph
vids <- Spe$Pindex
sub <- igraph::subgraph(cluster4, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster4_hiver.csv", row.names = FALSE,dec = ".")

# Fall #
CL4 <- filter(data, cluster == 4,season == "Fall" )
CL4 <- dplyr::select(CL4,Actinoptychus:Coscinodiscophycidae)
# Compute the mean of all the genus
Spe_w <- as.data.frame(t(summarise_all(CL4,.funs = list(mean = ~mean(., na.rm = TRUE)))))

# Prepare the list of the present genus
Spe_w$Phyto <- colnames(CL4)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index of the species
nodes_net <- V(cluster4)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)

# Compute the subgraph
vids <- Spe$Pindex
sub <- igraph::subgraph(cluster4, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster4_automne.csv", row.names = FALSE,dec = ".")

# Spring #
CL4 <- filter(data, cluster == 4,season == "Spring" )
CL4 <- dplyr::select(CL4,Actinoptychus:Coscinodiscophycidae)
# Compute the mean of all the genus
Spe_w <- as.data.frame(t(summarise_all(CL4,.funs = list(mean = ~mean(., na.rm = TRUE)))))

# Prepare the list of the present genus
Spe_w$Phyto <- colnames(CL4)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index of the species
nodes_net <- V(cluster4)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)

# Compute the subgraph
vids <- Spe$Pindex
sub <- igraph::subgraph(cluster4, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster4_printemps.csv", row.names = FALSE,dec = ".")

# Summer #
CL4 <- filter(data, cluster == 4,season == "Summer" )
CL4 <- dplyr::select(CL4,Actinoptychus:Coscinodiscophycidae)
# Compute the mean of all the genus
Spe_w <- as.data.frame(t(summarise_all(CL4,.funs = list(mean = ~mean(., na.rm = TRUE)))))

# Prepare the list of the present genus
Spe_w$Phyto <- colnames(CL4)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index of the species
nodes_net <- V(cluster4)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)

# Compute the subgraph
vids <- Spe$Pindex
sub <- igraph::subgraph(cluster4, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster4_summer.csv", row.names = FALSE,dec = ".")


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

metric4 <- read_delim("data_modif/results_metrics_reseaux_cluster4_pos.csv", 
                      delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                      locale = locale(decimal_mark = ",", grouping_mark = "."), 
                      trim_ws = TRUE)
metric4$cluster <- 4

metric <- bind_rows(metric1,metric2,metric3,metric4)
write.csv2(metric,file="data_modif/metrics_final.csv", row.names = FALSE,dec = ".")

# Import data 
metric <- read_delim("data_modif/metrics_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF")

# Boxplots 
datal2 <- filter(datal, Var %in% c("Assort","Avg_p_length","C_tance","D_liens","Mod","Nat_connect","N_noeuds","Adhes"))
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
datam <- filter(datal2, Var == "Avg_p_length")
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

# Hydro data 
abio <- dplyr::select(data1,SALI:`TURB-FNU`)
ncomp <- estim_ncpPCA(abio)
# Imputation of NA's
res.imp <- imputePCA(abio, ncp = ncomp$ncp,method = "EM")
envi <- as.data.frame(res.imp$completeObs)

# The same with the graph metrics
metric1 <- dplyr::select(metric1,N_noeuds:N_clust)
metric1[metric1 == Inf] <- NA
metric1 <- select(metric1,N_noeuds,D_liens:Mod,Assort,Nat_connect)

ncomp <- estim_ncpPCA(metric1)
res.imp <- imputePCA(metric1, ncp = ncomp$ncp,method = "EM")
metric1 <- as.data.frame(res.imp$completeObs)

# Final selected variables by backward selection (VIF is check and significativity also)
# Doing the RDA
metric_rda <- rda(metric1 ~. -TURB -PO4, data=envi,scale=T)
# Plot it
plot(metric_rda)
# RDA check
summary(metric_rda)
vif.cca(metric_rda)
anova.cca(metric_rda,by="term")
anova.cca(metric_rda,by="axis")

# Create a best biplot

info <- data1[, c("Code_point_Libelle","Date","cluster","Month","Year")]
observations_coords <- scores(metric_rda, display = "sites")

observations_df <- as.data.frame(observations_coords)
observations_df$cluster <- as.character(info$cluster)
observations_df$Month <- info$Month
observations_df$Year <- info$Year

cluster_pos <- summarise(group_by(observations_df, cluster), RDA1=mean(RDA1,na.rm=T),
                         RDA2=mean(RDA2,na.rm=T))

cluster_pos$cluster <- as.character(cluster_pos$cluster)

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF")
species_coords <- scores(metric_rda, display = "species")

# Coordinates of the metrics
species_df <- as.data.frame(species_coords)
species_df$name <- rownames(species_df)

envcoord <- as.data.frame(metric_rda[["CCA"]][["biplot"]])
envcoord$Var <- rownames(envcoord)

ggplot(cluster_pos, aes(x = RDA1, y = RDA2)) +
  geom_text(aes(x=RDA1,y=RDA2,label=name),size=3,data=species_df)+
  labs(x = "Dimension 1 (1.80%)", y = "Dimension 2 (0.23%)") +
  geom_segment(aes(x = 0, y = 0, xend = RDA1, yend = RDA2), color = "black", size = 0.5,data=species_df,linetype="dashed")+
  geom_segment(aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(length = unit(0.08, "inches")), color = "red", size = 0.4,data=envcoord)+
  geom_text(aes(x=RDA1,y=RDA2,label=Var),size=3,data=envcoord,color = "red")+
  theme_minimal()

acp <- bind_cols(select(envi,-TURB),metric1)
PCA(acp,scale.unit = T,quanti.sup = c(9:16) )


## Working on the cluster 2 #####
data2 <- filter(data,cluster == 2)
metric2 <- filter(metric,cluster == 2)

# Hydro data 
abio <- dplyr::select(data2,SALI:`TURB-FNU`)
ncomp <- estim_ncpPCA(abio)
# Imputation of NA's
res.imp <- imputePCA(abio, ncp = ncomp$ncp,method = "EM")
envi <- as.data.frame(res.imp$completeObs)

# The same with the graph metrics
metric2 <- dplyr::select(metric2,N_noeuds:N_clust)
metric2[metric2 == Inf] <- NA
metric2 <- select(metric2,N_noeuds,D_liens:Mod,Assort,Nat_connect)

ncomp <- estim_ncpPCA(metric2)
res.imp <- imputePCA(metric2, ncp = ncomp$ncp,method = "EM")
metric2 <- as.data.frame(res.imp$completeObs)

# Final selected variables by backward selection (VIF is check and significativity also)
# Doing the RDA
metric_rda <- rda(metric2 ~. -TURB -`NO3+NO2` , data=envi,scale=T)
# Plot it
plot(metric_rda)
# RDA check
summary(metric_rda)
vif.cca(metric_rda)
anova.cca(metric_rda,by="term")
anova.cca(metric_rda,by="axis")

# Create a best biplot

info <- data2[, c("Code_point_Libelle","Date","cluster","Month","Year")]
observations_coords <- scores(metric_rda, display = "sites")

observations_df <- as.data.frame(observations_coords)
observations_df$cluster <- as.character(info$cluster)
observations_df$Month <- info$Month
observations_df$Year <- info$Year

cluster_pos <- summarise(group_by(observations_df, cluster), RDA1=mean(RDA1,na.rm=T),
                         RDA2=mean(RDA2,na.rm=T))

cluster_pos$cluster <- as.character(cluster_pos$cluster)

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF")
species_coords <- scores(metric_rda, display = "species")

# Coordinates of the metrics
species_df <- as.data.frame(species_coords)
species_df$name <- rownames(species_df)

envcoord <- as.data.frame(metric_rda[["CCA"]][["biplot"]])
envcoord$Var <- rownames(envcoord)

ggplot(cluster_pos, aes(x = RDA1, y = RDA2)) +
  geom_text(aes(x=RDA1,y=RDA2,label=name),size=3,data=species_df)+
  labs(x = "Dimension 1 (17.46%)", y = "Dimension 2 (2.29%)") +
  geom_segment(aes(x = 0, y = 0, xend = RDA1, yend = RDA2), color = "black", size = 0.5,data=species_df,linetype="dashed")+
  geom_segment(aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(length = unit(0.08, "inches")), color = "#CD9600", size = 0.4,data=envcoord)+
  geom_text(aes(x=RDA1,y=RDA2,label=Var),size=3,data=envcoord,color = "#CD9600")+
  theme_minimal()

acp <- bind_cols(select(envi,-TURB),metric2)
PCA(acp,scale.unit = T,quanti.sup = c(9:16) )

## Working on the cluster 3 #####
data3 <- filter(data,cluster == 3)
metric3 <- filter(metric,cluster == 3)

# Hydro data 
abio <- dplyr::select(data3,SALI:`TURB-FNU`)
ncomp <- estim_ncpPCA(abio)
# Imputation of NA's
res.imp <- imputePCA(abio, ncp = ncomp$ncp,method = "EM")
envi <- as.data.frame(res.imp$completeObs)

# The same with the graph metrics
metric3 <- dplyr::select(metric3,N_noeuds:N_clust)
metric3[metric3 == Inf] <- NA
metric3 <- select(metric3,N_noeuds,D_liens:Mod,Assort,Nat_connect)

ncomp <- estim_ncpPCA(metric3)
res.imp <- imputePCA(metric3, ncp = ncomp$ncp,method = "EM")
metric3 <- as.data.frame(res.imp$completeObs)

# Final selected variables by backward selection (VIF is check and significativity also)
# Doing the RDA
metric_rda <- rda(metric3 ~. -TURB -`NO3+NO2` , data=envi,scale=T)
# Plot it
plot(metric_rda)
# RDA check
summary(metric_rda)
vif.cca(metric_rda)
anova.cca(metric_rda,by="term")
anova.cca(metric_rda,by="axis")

# Create a best biplot

info <- data3[, c("Code_point_Libelle","Date","cluster","Month","Year")]
observations_coords <- scores(metric_rda, display = "sites")

observations_df <- as.data.frame(observations_coords)
observations_df$cluster <- as.character(info$cluster)
observations_df$Month <- info$Month
observations_df$Year <- info$Year

cluster_pos <- summarise(group_by(observations_df, cluster), RDA1=mean(RDA1,na.rm=T),
                         RDA2=mean(RDA2,na.rm=T))

cluster_pos$cluster <- as.character(cluster_pos$cluster)

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF")
species_coords <- scores(metric_rda, display = "species")

# Coordinates of the metrics
species_df <- as.data.frame(species_coords)
species_df$name <- rownames(species_df)

envcoord <- as.data.frame(metric_rda[["CCA"]][["biplot"]])
envcoord$Var <- rownames(envcoord)

ggplot(cluster_pos, aes(x = RDA1, y = RDA2)) +
  geom_text(aes(x=RDA1,y=RDA2,label=name),size=3,data=species_df)+
  labs(x = "Dimension 1 (11.99%)", y = "Dimension 2 (0.65%)") +
  geom_segment(aes(x = 0, y = 0, xend = RDA1, yend = RDA2), color = "black", size = 0.5,data=species_df,linetype="dashed")+
  geom_segment(aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(length = unit(0.08, "inches")), color = "#00BE67", size = 0.4,data=envcoord)+
  geom_text(aes(x=RDA1,y=RDA2,label=Var),size=3,data=envcoord,color = "#00BE67")+
  theme_minimal()

acp <- bind_cols(select(envi,-TURB),metric3)
PCA(acp,scale.unit = T,quanti.sup = c(9:16) )

## Working on the cluster 4 #####
data4 <- filter(data,cluster == 4)
metric4 <- filter(metric,cluster == 4)

# Hydro data 
abio <- dplyr::select(data4,SALI:`TURB-FNU`)
ncomp <- estim_ncpPCA(abio)
# Imputation of NA's
res.imp <- imputePCA(abio, ncp = ncomp$ncp,method = "EM")
envi <- as.data.frame(res.imp$completeObs)

# The same with the graph metrics
metric4 <- dplyr::select(metric4,N_noeuds:N_clust)
metric4[metric4 == Inf] <- NA
metric4 <- select(metric4,N_noeuds,D_liens:Mod,Assort,Nat_connect)

ncomp <- estim_ncpPCA(metric4)
res.imp <- imputePCA(metric4, ncp = ncomp$ncp,method = "EM")
metric4 <- as.data.frame(res.imp$completeObs)

# Final selected variables by backward selection (VIF is check and significativity also)
# Doing the RDA
metric_rda <- rda(metric4 ~. -TURB -`NO3+NO2` , data=envi,scale=T)
# Plot it
plot(metric_rda)
# RDA check
summary(metric_rda)
vif.cca(metric_rda)
anova.cca(metric_rda,by="term")
anova.cca(metric_rda,by="axis")

# Create a best biplot

info <- data4[, c("Code_point_Libelle","Date","cluster","Month","Year")]
observations_coords <- scores(metric_rda, display = "sites")

observations_df <- as.data.frame(observations_coords)
observations_df$cluster <- as.character(info$cluster)
observations_df$Month <- info$Month
observations_df$Year <- info$Year

cluster_pos <- summarise(group_by(observations_df, cluster), RDA1=mean(RDA1,na.rm=T),
                         RDA2=mean(RDA2,na.rm=T))

cluster_pos$cluster <- as.character(cluster_pos$cluster)

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF")
species_coords <- scores(metric_rda, display = "species")

# Coordinates of the metrics
species_df <- as.data.frame(species_coords)
species_df$name <- rownames(species_df)

envcoord <- as.data.frame(metric_rda[["CCA"]][["biplot"]])
envcoord$Var <- rownames(envcoord)

ggplot(cluster_pos, aes(x = RDA1, y = RDA2)) +
  geom_text(aes(x=RDA1,y=RDA2,label=name),size=3,data=species_df)+
  labs(x = "Dimension 1 (13.64%)", y = "Dimension 2 (1.17%)") +
  geom_segment(aes(x = 0, y = 0, xend = RDA1, yend = RDA2), color = "black", size = 0.5,data=species_df,linetype="dashed")+
  geom_segment(aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(length = unit(0.08, "inches")), color = "#00A9FF", size = 0.4,data=envcoord)+
  geom_text(aes(x=RDA1,y=RDA2,label=Var),size=3,data=envcoord,color = "#00A9FF")+
  theme_minimal()

acp <- bind_cols(select(envi,-TURB),metric4)
PCA(acp,scale.unit = T,quanti.sup = c(9:16) )




