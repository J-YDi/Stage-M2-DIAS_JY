library(NetCoMi)
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

### Cluster 3 ####
CL3 <- filter(data, cluster == 3 )
#rownames(CL1) <- CL1$Date
CL3 <- dplyr::select(CL3,Actinoptychus:Coscinodiscophycidae)
CL3[is.na(CL3)]<- 0

CL3 <- as.matrix(CL3)

net <- netConstruct(data = CL3, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 121),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
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

### Cluster 2 ####
CL2 <- filter(data, cluster == 2 )
#rownames(CL1) <- CL1$Date
CL2 <- dplyr::select(CL2,Actinoptychus:Coscinodiscophycidae)
CL2[is.na(CL2)]<- 0

CL2 <- as.matrix(CL2)

net <- netConstruct(data = CL2, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 58),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
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
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

### Cluster 4 ####
CL4 <- filter(data, cluster == 4 )
#rownames(CL1) <- CL1$Date
CL4 <- dplyr::select(CL4,Actinoptychus:Coscinodiscophycidae)
CL4[is.na(CL4)]<- 0

CL4 <- as.matrix(CL4)

net <- netConstruct(data = CL4, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 39),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
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
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

### Cluster 1 ####
CL1 <- filter(data, cluster == 1 )
#rownames(CL1) <- CL1$Date
CL1 <- dplyr::select(CL1,Actinoptychus:Coscinodiscophycidae)
CL1[is.na(CL1)]<- 0

CL1 <- as.matrix(CL1)

net <- netConstruct(data = CL1, dataType = "counts",measure = "spearman", 
                    filtTax = "numbSamp",filtTaxPar = list(numbSamp = 160),
                    filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                    normMethod = "none", dissFunc = "signed")

net_props <- netAnalyze(net,
                        clustMethod = "cluster_fast_greedy",
                        hubPar = c("eigenvector"),
                        graphlet = F,
                        connectivity = T)

resume <- summary(net_props)
resume
plt <- plot(net_props,
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
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1)

# Creer un sous réseau bloom vs non bloom
#{
  data$EpBloom <- ifelse(data$P_dominance >=0, "yes", NA)
group_vec <- filter(data,Code_point_Libelle == "Ouest Loscolo" )$EpBloom
sel <- which(group_vec %in% c(NA, "yes"))
group_vec <- group_vec[sel]
group_vec[is.na(group_vec)] <- "no"
OL <- OL[sel,]

netbloom <- netConstruct(OL,dataType = "counts",measure = "spearman", 
                         group = group_vec,
                         filtTax = "numbSamp",filtTaxPar = list(numbSamp = 35),
                         filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                         normMethod = "none", dissFunc = "signed")

props_bloom <- netAnalyze(netbloom, clustMethod = "cluster_fast_greedy",
                          hubPar = c("eigenvector"),
                          graphlet = F,
                          connectivity = T,
                          centrLCC = FALSE)

summary(props_bloom)

plt <- plot(props_bloom,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "Ouest Loscolo",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "inboth",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1,
            sameLayout = T,
            groupNames = c("Non bloom", "Bloom"),)

comp_bloom <- netCompare(props_bloom, 
                          permTest = T, 
                          verbose = FALSE,
                          seed = 123456)

summary(comp_bloom, 
        groupNames = c("Non bloom", "Bloom"))
}
# Creer deux réseaux pour comparer les clusters 1 et 3
{
group_vec <- filter(data,cluster == 1 | cluster == 3 )$cluster
sel <- which(group_vec %in% c(1, 3))
group_vec <- group_vec[sel]
CL13 <- filter(data,cluster == 1 | cluster == 3 )
CL13 <- CL13[sel,]

CL13 <- dplyr::select(CL13,Actinoptychus:Coscinodiscophycidae)
CL13[is.na(CL13)]<- 0

CL13 <- as.matrix(CL13)

net13 <- netConstruct(CL13,dataType = "counts",measure = "spearman", 
                         group = group_vec,
                         filtTax = "numbSamp",filtTaxPar = list(numbSamp = 562),
                         filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                         normMethod = "none", dissFunc = "signed")

props_13 <- netAnalyze(net13, clustMethod = "cluster_fast_greedy",
                          hubPar = c("eigenvector"),
                          graphlet = F,
                          connectivity = T,
                          centrLCC = FALSE)

summary(props_13)

plt <- plot(props_13,
            layout = "spring",
            nodeColor = "cluster", 
            nodeSize = "degree",
            nodeTrans = 0,
            title1 = "",
            shortenLabels = "none",
            labelScale = F,
            labelFont = 1,
            nodeFilter = "none",
            rmSingles = "none",
            highlightHubs = T,
            edgeFilter = "none",
            negDiffCol = T,
            posCol = "green4",
            negCol = "red3",
            showTitle = TRUE,
            cexTitle = 1.3,
            cexLabels = 1,
            cexNodes = 1,
            sameLayout = T,
            groupNames = c("Cluster 1", "Cluster 3"),)

comp_bloom <- netCompare(props_13, 
                         permTest = T, 
                         verbose = FALSE,
                         seed = 123456)

summary(comp_bloom, 
        groupNames = c("Cluster 1", "Cluster 3"))

}

# Creer deux réseaux pour comparer les clusters 1 et 2
{
  group_vec <- filter(data,cluster == 1 | cluster == 2 )$cluster
  sel <- which(group_vec %in% c(1, 2))
  group_vec <- group_vec[sel]
  CL12 <- filter(data,cluster == 1 | cluster == 2 )
  CL12 <- CL12[sel,]
  
  CL12 <- dplyr::select(CL12,Actinoptychus:Coscinodiscophycidae)
  CL12[is.na(CL12)]<- 0
  
  CL12 <- as.matrix(CL12)
  
  net12 <- netConstruct(CL12,dataType = "counts",measure = "spearman", 
                        group = group_vec,
                        filtTax = "numbSamp",filtTaxPar = list(numbSamp = 437),
                        filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                        normMethod = "none", dissFunc = "signed")
  
  props_12 <- netAnalyze(net12, clustMethod = "cluster_fast_greedy",
                         hubPar = c("eigenvector"),
                         graphlet = F,
                         connectivity = T,
                         centrLCC = FALSE)
  
  summary(props_12)
  
  plt <- plot(props_12,
              layout = "spring",
              nodeColor = "cluster", 
              nodeSize = "degree",
              nodeTrans = 0,
              title1 = "",
              shortenLabels = "none",
              labelScale = F,
              labelFont = 1,
              nodeFilter = "none",
              rmSingles = "none",
              highlightHubs = T,
              edgeFilter = "none",
              negDiffCol = T,
              posCol = "green4",
              negCol = "red3",
              showTitle = TRUE,
              cexTitle = 1.3,
              cexLabels = 1,
              cexNodes = 1,
              sameLayout = T,
              groupNames = c("Cluster 1", "Cluster 2"),)
  
  comp_bloom <- netCompare(props_12, 
                           permTest = T, 
                           verbose = FALSE,
                           seed = 123456)
  
  summary(comp_bloom, 
          groupNames = c("Cluster 1", "Cluster 2"))
  
}

# Creer deux réseaux pour comparer les clusters 1 et 4
{
  group_vec <- filter(data,cluster == 1 | cluster == 4 )$cluster
  sel <- which(group_vec %in% c(1, 4))
  group_vec <- group_vec[sel]
  CL14 <- filter(data,cluster == 1 | cluster == 4 )
  CL14 <- CL14[sel,]
  
  CL14 <- dplyr::select(CL14,Actinoptychus:Coscinodiscophycidae)
  CL14[is.na(CL14)]<- 0
  
  CL14 <- as.matrix(CL14)
  
  net14 <- netConstruct(CL14,dataType = "counts",measure = "spearman", 
                        group = group_vec,
                        filtTax = "numbSamp",filtTaxPar = list(numbSamp = 398),
                        filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                        normMethod = "none", dissFunc = "signed")
  
  props_14 <- netAnalyze(net14, clustMethod = "cluster_fast_greedy",
                         hubPar = c("eigenvector"),
                         graphlet = F,
                         connectivity = T,
                         centrLCC = FALSE)
  
  summary(props_14)
  
  plt <- plot(props_14,
              layout = "spring",
              nodeColor = "cluster", 
              nodeSize = "degree",
              nodeTrans = 0,
              title1 = "",
              shortenLabels = "none",
              labelScale = F,
              labelFont = 1,
              nodeFilter = "none",
              rmSingles = "none",
              highlightHubs = T,
              edgeFilter = "none",
              negDiffCol = T,
              posCol = "green4",
              negCol = "red3",
              showTitle = TRUE,
              cexTitle = 1.3,
              cexLabels = 1,
              cexNodes = 1,
              sameLayout = T,
              groupNames = c("Cluster 1", "Cluster 4"),)
  
  comp_bloom <- netCompare(props_14, 
                           permTest = T, 
                           verbose = FALSE,
                           seed = 123456)
  
  summary(comp_bloom, 
          groupNames = c("Cluster 1", "Cluster 4"))
  
}






# Creer deux réseaux pour comparer les clusters 2 et 3
{
  group_vec <- filter(data,cluster == 2 | cluster == 3 )$cluster
  sel <- which(group_vec %in% c(2, 3))
  group_vec <- group_vec[sel]
  CL23 <- filter(data,cluster == 2 | cluster == 3 )
  CL23 <- CL23[sel,]
  
  CL23 <- dplyr::select(CL23,Actinoptychus:Coscinodiscophycidae)
  CL23[is.na(CL23)]<- 0
  
  CL23 <- as.matrix(CL23)
  
  net23 <- netConstruct(CL23,dataType = "counts",measure = "spearman", 
                        group = group_vec,
                        filtTax = "numbSamp",filtTaxPar = list(numbSamp = 398),
                        filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                        normMethod = "none", dissFunc = "signed")
  
  props_23 <- netAnalyze(net23, clustMethod = "cluster_fast_greedy",
                         hubPar = c("eigenvector"),
                         graphlet = F,
                         connectivity = T,
                         centrLCC = FALSE)
  
  summary(props_23)
  
  plt <- plot(props_23,
              layout = "spring",
              nodeColor = "cluster", 
              nodeSize = "degree",
              nodeTrans = 0,
              title1 = "",
              shortenLabels = "none",
              labelScale = F,
              labelFont = 1,
              nodeFilter = "none",
              rmSingles = "none",
              highlightHubs = T,
              edgeFilter = "none",
              negDiffCol = T,
              posCol = "green4",
              negCol = "red3",
              showTitle = TRUE,
              cexTitle = 1.3,
              cexLabels = 1,
              cexNodes = 1,
              sameLayout = T,
              groupNames = c("Cluster 2", "Cluster 3"),)
  
  comp_bloom <- netCompare(props_23, 
                           permTest = T, 
                           verbose = FALSE,
                           seed = 123456)
  
  summary(comp_bloom, 
          groupNames = c("Cluster 2", "Cluster 3"))
  
}


# Creer deux réseaux pour comparer les clusters 2 et 4
{
  group_vec <- filter(data,cluster == 2 | cluster == 4 )$cluster
  sel <- which(group_vec %in% c(2, 4))
  group_vec <- group_vec[sel]
  CL24 <- filter(data,cluster == 2 | cluster == 4 )
  CL24 <- CL24[sel,]
  
  CL24 <- dplyr::select(CL24,Actinoptychus:Coscinodiscophycidae)
  CL24[is.na(CL24)]<- 0
  
  CL24 <- as.matrix(CL24)
  
  net24 <- netConstruct(CL24,dataType = "counts",measure = "spearman", 
                        group = group_vec,
                        filtTax = "numbSamp",filtTaxPar = list(numbSamp = 195),
                        filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                        normMethod = "none", dissFunc = "signed")
  
  props_24 <- netAnalyze(net24, clustMethod = "cluster_fast_greedy",
                         hubPar = c("eigenvector"),
                         graphlet = F,
                         connectivity = T,
                         centrLCC = FALSE)
  
  summary(props_24)
  
  plt <- plot(props_24,
              layout = "spring",
              nodeColor = "cluster", 
              nodeSize = "degree",
              nodeTrans = 0,
              title1 = "",
              shortenLabels = "none",
              labelScale = F,
              labelFont = 1,
              nodeFilter = "none",
              rmSingles = "none",
              highlightHubs = T,
              edgeFilter = "none",
              negDiffCol = T,
              posCol = "green4",
              negCol = "red3",
              showTitle = TRUE,
              cexTitle = 1.3,
              cexLabels = 1,
              cexNodes = 1,
              sameLayout = T,
              groupNames = c("Cluster 2", "Cluster 4"),)
  
  comp_bloom <- netCompare(props_24, 
                           permTest = T, 
                           verbose = FALSE,
                           seed = 123456)
  
  summary(comp_bloom, 
          groupNames = c("Cluster 2", "Cluster 4"))
  
}

# Creer deux réseaux pour comparer les clusters 3 et 4
{
  group_vec <- filter(data,cluster == 3 | cluster == 4 )$cluster
  sel <- which(group_vec %in% c(3, 4))
  group_vec <- group_vec[sel]
  CL34 <- filter(data,cluster == 3 | cluster == 4 )
  CL34 <- CL34[sel,]
  
  CL34 <- dplyr::select(CL34,Actinoptychus:Coscinodiscophycidae)
  CL34[is.na(CL34)]<- 0
  
  CL34 <- as.matrix(CL34)
  
  net34 <- netConstruct(CL34,dataType = "counts",measure = "spearman", 
                        group = group_vec,
                        filtTax = "numbSamp",filtTaxPar = list(numbSamp = 328),
                        filtSamp = "none",sparsMethod = "t-test",alpha = 0.05, zeroMethod = "none",adjust = "adaptBH",
                        normMethod = "none", dissFunc = "signed")
  
  props_34 <- netAnalyze(net34, clustMethod = "cluster_fast_greedy",
                         hubPar = c("eigenvector"),
                         graphlet = F,
                         connectivity = T,
                         centrLCC = FALSE)
  
  summary(props_34)
  
  plt <- plot(props_34,
              layout = "spring",
              nodeColor = "cluster", 
              nodeSize = "degree",
              nodeTrans = 0,
              title1 = "",
              shortenLabels = "none",
              labelScale = F,
              labelFont = 1,
              nodeFilter = "none",
              rmSingles = "none",
              highlightHubs = T,
              edgeFilter = "none",
              negDiffCol = T,
              posCol = "green4",
              negCol = "red3",
              showTitle = TRUE,
              cexTitle = 1.3,
              cexLabels = 1,
              cexNodes = 1,
              sameLayout = T,
              groupNames = c("Cluster 3", "Cluster 4"),)
  
  comp_bloom <- netCompare(props_34, 
                           permTest = T, 
                           verbose = FALSE,
                           seed = 123456)
  
  summary(comp_bloom, 
          groupNames = c("Cluster 3", "Cluster 4"))
  
}


# Charger le package igraph
library(igraph)

#### Travail sur le cluster 3 #####
# Relancer le graphe global du cluster 3 avant #

# On travail uniquement sur les interactions positives
assoMat <- net$assoMat1
assoMat[assoMat < 0] <- 0

cluster3 <- graph_from_adjacency_matrix(assoMat,weighted = T,mode = "undirected",diag=F)
cluster3

# Visualisation générale avec IGRAPH
plot(cluster3)
wc <- cluster_fast_greedy(cluster3)

V(cluster3)$label <- V(cluster3)$name
#V(cluster3)$name <- paste("I'm #", net$edgelist1$v1)
V(cluster3)$page_rank <- round(page.rank(cluster3)$vector, 2)
V(cluster3)$betweenness <- round(betweenness(cluster3), 2)
V(cluster3)$degree <- degree(cluster3)
V(cluster3)$size <- V(cluster3)$degree
V(cluster3)$comm <- membership(wc)
V(cluster3)$color <- colorize(membership(wc))
E(cluster3)$width <- E(cluster3)$weight*6
E(cluster3)$color <- "black"

viz3 <- hchart(cluster3, layout = layout_with_fr)
# Enregistrement de la visualisation globale
htmlwidgets::saveWidget(viz3, "output/graphs/Reseaux/HTML/cluster3.html")

# Metriques réseau global 
S_net <-  vcount(cluster3) # nombre de noeuds
L_net <- ecount(cluster3) # nombre de liens

Z_mat <- L_net / S_net # linkage density or average number of links per nodes

C_net <- edge_density(cluster3, loops = FALSE) #connectance

# Average path length
avg_path_length <- mean_distance(
  cluster3,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)

#mean(distances(cluster3, weights=E(cluster3)$weight)) 

Edge_connect <- edge.connectivity(cluster3) # Edge connectivity = adhesion

Modularity <- modularity(cluster3,membership = membership(wc)) # Modularity

Vert_connect <- vertex.connectivity(cluster3) # Vertex connectivity = adhesion

m_degree <- mean(degree(cluster3)) #Nombre de liens moyen

assort <- assortativity_degree(cluster3,directed = F) #assortativite

diss <- mean(1 - E(cluster3)$weight) # Dissilarite as defined in NetCoMi

trans <- transitivity(cluster3,type = "global") #Transitivity

mean_edge_bet <- mean(edge_betweenness(cluster3)) # Mean edge betweeness

nat_connect_good <- natural.connectivity(assoMat) # Connectivite naturel

adj <- as.matrix(as_adjacency_matrix(cluster3, attr = "weight",)) # OK
diag(adj) <- 1
nat_connect_notgood <- natural.connectivity(as.matrix(adj)) # Connectivite naturel

hubs <- eigen_centrality(
  cluster3,
  directed = FALSE,
  scale = TRUE,
  weights = NULL
)
Hubs <- as.data.frame(hubs$vector)
Hubs$Phyto <- rownames(Hubs)

Spe_hubs <- Hubs[order(desc(hubs$vector)),]
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster3.csv", row.names = FALSE,dec = ".")

# Preparation sous réseau 
# Index des espèces = noeuds
nodes_net <- V(cluster3)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

# Preparation tableau pour recuperer les infos station/date
CL3df <- filter(data,cluster == 3)

# Creation d'un df pour stocker les resultats
data_results_reseaux <- c("","")
data_results_reseaux <- as.data.frame(data_results_reseaux)

# Sous réseau
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

# Metriques réseau global 
S_net <-  vcount(sub) # nombre de noeuds
L_net <- ecount(sub) # nombre de liens
Z_mat <- L_net / S_net # linkage density or average number of links per nodes
C_net <- edge_density(sub, loops = FALSE) #connectance

# Average path length
avg_path_length <- mean_distance(
  sub,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)

Edge_connect <- edge.connectivity(sub) # Edge connectivity = adhesion

wc <- cluster_fast_greedy(sub)
Modularity <- modularity(sub,membership = membership(wc)) # Modularity

Vert_connect <- vertex.connectivity(sub) # Vertex connectivity = adhesion
m_degree <- mean(degree(sub)) #Nombre de liens moyen
assort <- assortativity_degree(sub,directed = F) #assortativite
diss <- mean(1 - E(sub)$weight) # Dissilarite as defined in NetCoMi
trans <- transitivity(sub,type = "global") #Transitivity
mean_edge_bet <- mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
nat_connect_notgood <- natural.connectivity(as.matrix(adj)) # Connectivite naturel

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
data_results_reseaux[i,15] <- nat_connect_notgood # Connectivite naturelle

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
write.csv2(data_results_reseaux,file="data_modif/results_metrics_reseaux_cluster3_pos.csv", row.names = FALSE,dec = ".")

# Pour visualisation dynamique :
#{chemin_repertoire <- "output/graphs/Reseaux/HTML/"

# Récupérez les noms de fichiers dans le répertoire
noms_fichiers_complets <- list.files(chemin_repertoire, pattern = "\\.html$", full.names = TRUE)

# Supprimez le chemin du répertoire des noms de fichiers
noms_fichiers <- basename(noms_fichiers_complets)

# Affichez les noms de fichiers
cat("[", paste("'", noms_fichiers, "'", collapse = ", "), "]", "\n")}

# Analyse des résultats cluster 3
data_results_reseaux_copy <- data_results_reseaux

bloom <- dplyr::select(CL3df,Code_point_Libelle,Date,Bloom_Phylum,P_dominance)

data_reseaux <- left_join(data_results_reseaux,bloom)

data_reseaux <- data_reseaux %>%
  mutate(Month = month(Date, label = F)) |>
  mutate(Year = year(Date))

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=N_noeuds))+
  geom_point(aes(x=Date,y=N_noeuds,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre de noeuds",x="Date",y="Nombre de noeuds")
ggsave('N_noeuds_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=N_liens))+
  geom_point(aes(x=Date,y=N_liens,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre de liens",x="Date",y="Nombre de liens")
ggsave('N_liens_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=D_liens))+
  geom_point(aes(x=Date,y=D_liens,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Densité des liens",x="Date",y="Densité des liens")
ggsave('D_liens_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=C_tance))+
  geom_point(aes(x=Date,y=C_tance,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Connectance",x="Date",y="Connectance")
ggsave('C_tance_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Avg_p_length))+
  geom_point(aes(x=Date,y=Avg_p_length,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Longueur moyen des liens",x="Date",y="Longueur moyen des liens")
ggsave('Avg_p_length_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=N_clust))+
  geom_point(aes(x=Date,y=N_clust,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre de cluster",x="Date",y="Longueur moyen des liens")
ggsave('N_clust_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Adhes))+
  geom_point(aes(x=Date,y=Adhes,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Adhesion",x="Date",y="Adhesion")
ggsave('Adhes_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Mod))+
  geom_point(aes(x=Date,y=Mod,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Modularite",x="Date",y="Modularite")
ggsave('Mod_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=meanN_liens))+
  geom_point(aes(x=Date,y=meanN_liens,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre moyen de liens",x="Date",y="Nombre moyen de liens")
ggsave('meanN_liens_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Assort))+
  geom_point(aes(x=Date,y=Assort,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Assortativite",x="Date",y="Assortativite")
ggsave('Assort_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Diss))+
  geom_point(aes(x=Date,y=Diss,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Dissimilarite",x="Date",y="Dissimilarite")
ggsave('Diss_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Trans))+
  geom_point(aes(x=Date,y=Trans,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Transitivite",x="Date",y="Transitivite")
ggsave('Trans_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=meanN_voisins))+
  geom_point(aes(x=Date,y=meanN_voisins,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre moyen de voisins",x="Date",y="Nombre moyen de voisins")
ggsave('meanN_voisins_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Nat_connect))+
  geom_point(aes(x=Date,y=Nat_connect,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Connectivite naturelle",x="Date",y="Connectivite naturelle")
ggsave('Nat_connect_station.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


# Correlation

Table.corr_all <- dplyr::select(data_reseaux,-Code_point_Libelle,-Date,-Bloom_Phylum,-P_dominance,-Month,-Year)
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
         type="upper", order="alphabet",
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=F, 
         title = "Correlation entre les métriques"
)

data_reseaux$Date2 <- as.Date(paste(data_reseaux$Year, data_reseaux$Month, "01", sep = "-"), format = "%Y-%m-%d")

datam <- summarise(group_by(data_reseaux,Code_point_Libelle,Month,Year), N_noeuds=mean(N_noeuds,na.rm=T),N_liens=mean(N_liens,na.rm=T),
                   D_liens=mean(D_liens,na.rm=T),C_tance=mean(C_tance,na.rm=T),Avg_p_length=mean(Avg_p_length,na.rm=T),
                   Adhes=mean(Adhes,na.rm=T),Mod=mean(Mod,na.rm=T),meanN_liens=mean(meanN_liens,na.rm=T),
                   Assort=mean(Assort,na.rm=T),Diss=mean(Diss,na.rm=T),Trans=mean(Trans,na.rm=T),
                   meanN_voisins=mean(meanN_voisins,na.rm=T),Nat_connect=mean(Nat_connect,na.rm=T),N_clust=mean(N_clust,na.rm=T))

# Au niveau des stations
ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_liens,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_liens_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_noeuds,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_noeuds_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=D_liens,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('D_liens_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=C_tance,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('C_tance_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Avg_p_length,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Avg_p_length_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Adhes,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Adhes_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Mod,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Mod_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_liens,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_liens_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Assort,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Assort_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Diss,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Diss_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Trans,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Trans_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_voisins,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_voisins_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Nat_connect,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Nat_connect_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_clust,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_clust_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


# Annee
ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_liens,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('N_liens_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_noeuds,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('N_noeuds_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=D_liens,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('D_liens_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=C_tance,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('C_tance_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Avg_p_length,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Avg_p_length_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Adhes,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Adhes_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Mod,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Mod_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_liens,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('meanN_liens_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Assort,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Assort_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Diss,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Diss_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Trans,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Trans_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_voisins,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('meanN_voisins_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Nat_connect,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Nat_connect_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_clust,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('N_clust_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

# Au niveau du cluster
ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_liens,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_liens_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_noeuds,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_noeuds_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=D_liens,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('D_liens_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=C_tance,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('C_tance_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Avg_p_length,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Avg_p_length_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Adhes,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Adhes_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Mod,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Mod_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_liens,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_liens_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Assort,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Assort_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Diss,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Diss_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Trans,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Trans_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_voisins,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_voisins_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Nat_connect,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Nat_connect_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_clust,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_clust_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

datal <- pivot_longer(datam,names_to = "Var",cols = N_noeuds:N_clust)
ggplot(datal)+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#00BE67",size = 1)+
  facet_wrap(~Var,scales = "free_y")+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  labs(title = "Metriques réseau cluster 3 (+)")
ggsave('Metriques_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')

# Annee
ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_liens,group=Year),size = 1)
  
ggsave('N_liens_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_noeuds,group=Year),size = 1)
  
ggsave('N_noeuds_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=D_liens,group=Year),size = 1)
  
ggsave('D_liens_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=C_tance,group=Year),size = 1)
  
ggsave('C_tance_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Avg_p_length,group=Year),size = 1)
  
ggsave('Avg_p_length_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Adhes,group=Year),size = 1)
  
ggsave('Adhes_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Mod,group=Year),size = 1)
  
ggsave('Mod_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_liens,group=Year),size = 1)
  
ggsave('meanN_liens_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Assort,group=Year),size = 1)
  
ggsave('Assort_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Diss,group=Year),size = 1)
  
ggsave('Diss_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Trans,group=Year),size = 1)
  
ggsave('Trans_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_voisins,group=Year),size = 1)
  
ggsave('meanN_voisins_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Nat_connect,group=Year),size = 1)
  
ggsave('Nat_connect_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

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

ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=N_liens,group=Date2),fill="#00BE67")+
  labs(title="Nombre de liens",x="Date2",y="Nombre de liens")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))
ggsave('N_liens_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=N_clust,group=Date2),fill="#00BE67")+
  labs(title="Nombre de liens",x="Date2",y="Nombre de liens")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))
ggsave('N_clust_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=N_noeuds,group=Date2),fill="#00BE67")+
  labs(title="Nombre de noeuds",x="Date2",y="Nombre de noeuds")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))
ggsave('N_noeuds_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=D_liens,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Densité des liens",x="Date2",y="Densité des liens")
ggsave('D_liens_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=C_tance,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Connectance",x="Date2",y="Connectance")
ggsave('C_tance_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Avg_p_length,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Longueur moyen des liens",x="Date2",y="Longueur moyen des liens")
ggsave('Avg_p_length_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Adhes,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Adhesion",x="Date2",y="Adhesion")
ggsave('Adhes_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Mod,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Modularite",x="Date2",y="Modularite")
ggsave('Mod_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=meanN_liens,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Nombre moyen de liens",x="Date2",y="Nombre moyen de liens")
ggsave('meanN_liens_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Assort,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Assortativite",x="Date2",y="Assortativite")
ggsave('Assort_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Diss,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Dissimilarite",x="Date2",y="Dissimilarite")
ggsave('Diss_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Trans,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Transitivite",x="Date2",y="Transitivite")
ggsave('Trans_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=meanN_voisins,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Nombre moyen de voisins",x="Date2",y="Nombre moyen de voisins")
ggsave('meanN_voisins_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL3_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Nat_connect,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Connectivite naturelle",x="Date2",y="Connectivite naturelle")
ggsave('Nat_connect_cluster.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


datal2 <- pivot_longer(CL3_Mmetrics,names_to = "Var",cols = N_noeuds:N_clust)
ggplot(datal2)+
  geom_boxplot(aes(x=Date2,y=value,group=Date2),fill="#00BE67",size = 1)+
  facet_wrap(~Var,scales = "free_y")+
  labs(title = "Metriques réseau cluster 3 (+)")
ggsave('Metriques_cluster_ts.png', path = "output/graphs/Reseaux/TS_CLUST3/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')



#### Travail sur le cluster 1 #####
# Relancer le graphe global du cluster 1 avant #

# On travail uniquement sur les interactions positives
assoMat <- net$assoMat1
assoMat[assoMat < 0] <- 0

cluster1 <- graph_from_adjacency_matrix(assoMat,weighted = T,mode = "undirected",diag=F)
cluster1
# Visualisation générale avec IGRAPH
plot(cluster1)
wc <- cluster_fast_greedy(cluster1)

V(cluster1)$label <- V(cluster1)$name
V(cluster1)$name <- paste("I'm #", net$edgelist1$v1)
V(cluster1)$page_rank <- round(page.rank(cluster1)$vector, 2)
V(cluster1)$betweenness <- round(betweenness(cluster1), 2)
V(cluster1)$degree <- degree(cluster1)
V(cluster1)$size <- V(cluster1)$degree
V(cluster1)$comm <- membership(wc)
V(cluster1)$color <- colorize(membership(wc))
E(cluster1)$width <- E(cluster1)$weight*6
E(cluster1)$color <- "black"

viz1 <- hchart(cluster1, layout = layout_with_fr)
# Enregistrement de la visualisation globale
htmlwidgets::saveWidget(viz1, "output/graphs/Reseaux/HTML/cluster1.html")

# Metriques réseau global 
S_net <-  vcount(cluster1) # nombre de noeuds
L_net <- ecount(cluster1) # nombre de liens

Z_mat <- L_net / S_net # linkage density or average number of links per nodes

C_net <- edge_density(cluster1, loops = FALSE) #connectance

# Average path length
avg_path_length <- mean_distance(
  cluster1,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)

#mean(distances(cluster1, weights=E(cluster1)$weight)) 

Edge_connect <- edge.connectivity(cluster1) # Edge connectivity = adhesion

Modularity <- modularity(cluster1,membership = membership(wc)) # Modularity

Vert_connect <- vertex.connectivity(cluster1) # Vertex connectivity = adhesion

m_degree <- mean(degree(cluster1)) #Nombre de liens moyen

assort <- assortativity_degree(cluster1,directed = F) #assortativite

diss <- mean(1 - E(cluster1)$weight) # Dissilarite as defined in NetCoMi

trans <- transitivity(cluster1,type = "global") #Transitivity

mean_edge_bet <- mean(edge_betweenness(cluster1)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(cluster1, attr = "weight",)) # OK
diag(adj) <- 1
nat_connect_notgood <- natural.connectivity(as.matrix(adj)) # Connectivite naturel

hubs <- eigen_centrality(
  cluster1,
  directed = FALSE,
  scale = TRUE,
  weights = NULL
)
Hubs <- as.data.frame(hubs$vector)
Hubs$Phyto <- rownames(Hubs)

Spe_hubs <- Hubs[order(desc(hubs$vector)),]
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster1.csv", row.names = FALSE,dec = ".")


# Preparation sous réseau 
# Index des espèces = noeuds
nodes_net <- V(cluster1)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

# Preparation tableau pour recuperer les infos station/date
CL1df <- filter(data,cluster == 1)

# Creation d'un df pour stocker les resultats
data_results_reseaux <- c("","")
data_results_reseaux <- as.data.frame(data_results_reseaux)

# Sous réseau
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
  
  # Metriques réseau global 
  S_net <-  vcount(sub) # nombre de noeuds
  L_net <- ecount(sub) # nombre de liens
  Z_mat <- L_net / S_net # linkage density or average number of links per nodes
  C_net <- edge_density(sub, loops = FALSE) #connectance
  
  # Average path length
  avg_path_length <- mean_distance(
    sub,
    directed = FALSE,
    unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
  )
  
  Edge_connect <- edge.connectivity(sub) # Edge connectivity = adhesion
  
  wc <- cluster_fast_greedy(sub)
  Modularity <- modularity(sub,membership = membership(wc)) # Modularity
  
  Vert_connect <- vertex.connectivity(sub) # Vertex connectivity = adhesion
  m_degree <- mean(degree(sub)) #Nombre de liens moyen
  assort <- assortativity_degree(sub,directed = F) #assortativite
  diss <- mean(1 - E(sub)$weight) # Dissilarite as defined in NetCoMi
  trans <- transitivity(sub,type = "global") #Transitivity
  mean_edge_bet <- mean(edge_betweenness(sub)) # Mean edge betweeness
  adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
  diag(adj) <- 1
  nat_connect_notgood <- natural.connectivity(as.matrix(adj)) # Connectivite naturel
  
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
  data_results_reseaux[i,15] <- nat_connect_notgood # Connectivite naturelle
  
  data_results_reseaux[i,16] <- length(wc) # Nombre de cluster
  } else { 
    
    data_results_reseaux[i,1] <- station
    data_results_reseaux[i,2] <- date
    data_results_reseaux[i,3] <- NA # nombre de noeuds
    data_results_reseaux[i,4] <- NA # nombre de liens
    data_results_reseaux[i,5] <- NA # Densite des liens
    data_results_reseaux[i,6] <- NA # connectance
    
    data_results_reseaux[i,7] <- NA  # longueur moyen des liens
    data_results_reseaux[i,8] <- NA  # adhesion
    data_results_reseaux[i,9] <- NA #modularite
    
    data_results_reseaux[i,10] <- NA  #Nombre de liens moyens
    data_results_reseaux[i,11] <- NA  # Assortativite
    data_results_reseaux[i,12] <- NA # dissimilarite
    data_results_reseaux[i,13] <- NA  # transitivite
    
    data_results_reseaux[i,14] <- NA  # Nombre moyen de voisins
    data_results_reseaux[i,15] <- NA  # Connectivite naturelle
    
    data_results_reseaux[i,16] <- NA # Nombre de cluster
    }
  
  colnames(data_results_reseaux) <- c("Code_point_Libelle","Date","N_noeuds","N_liens","D_liens","C_tance",
                                      "Avg_p_length","Adhes","Mod","meanN_liens","Assort","Diss","Trans","meanN_voisins",
                                      "Nat_connect","N_clust")
  
 #plot(sub,main = paste0(station,date),layout = layout_with_fr)
 #png(paste0("output/graphs/Reseaux/TS_CLUST1/",station,date,".png"))
 #plot(sub,main = paste0(station,date),layout = layout_with_fr)
 #dev.off()
 #htmlwidgets::saveWidget(viz_sub, paste0("output/graphs/Reseaux/HTML/CL1/",station,date,".html"))
  print(i/nrow(CL1)*100)
}
write.csv2(data_results_reseaux,file="data_modif/results_metrics_reseaux_cluster1_pos.csv", row.names = FALSE,dec = ".")

# Pour visualisation dynamique :
#{chemin_repertoire <- "output/graphs/Reseaux/HTML/CL1/"
  
  # Récupérez les noms de fichiers dans le répertoire
  noms_fichiers_complets <- list.files(chemin_repertoire, pattern = "\\.html$", full.names = TRUE)
  
  # Supprimez le chemin du répertoire des noms de fichiers
  noms_fichiers <- basename(noms_fichiers_complets)
  
  # Affichez les noms de fichiers
  cat("[", paste("'", noms_fichiers, "'", collapse = ", "), "]", "\n")}

# Analyse des résultats cluster 1
data_results_reseaux_copy <- data_results_reseaux

bloom <- dplyr::select(CL1df,Code_point_Libelle,Date,Bloom_Phylum,P_dominance)

data_reseaux <- left_join(data_results_reseaux,bloom)

data_reseaux <- data_reseaux %>%
  mutate(Month = month(Date, label = F)) |>
  mutate(Year = year(Date))

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=N_noeuds))+
  geom_point(aes(x=Date,y=N_noeuds,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre de noeuds",x="Date",y="Nombre de noeuds")
ggsave('N_noeuds_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=N_clust))+
  geom_point(aes(x=Date,y=N_clust,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre de noeuds",x="Date",y="Nombre de noeuds")
ggsave('N_clust_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=N_liens))+
  geom_point(aes(x=Date,y=N_liens,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre de liens",x="Date",y="Nombre de liens")
ggsave('N_liens_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=D_liens))+
  geom_point(aes(x=Date,y=D_liens,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Densité des liens",x="Date",y="Densité des liens")
ggsave('D_liens_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=C_tance))+
  geom_point(aes(x=Date,y=C_tance,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Connectance",x="Date",y="Connectance")
ggsave('C_tance_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Avg_p_length))+
  geom_point(aes(x=Date,y=Avg_p_length,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Longueur moyen des liens",x="Date",y="Longueur moyen des liens")
ggsave('Avg_p_length_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Adhes))+
  geom_point(aes(x=Date,y=Adhes,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Adhesion",x="Date",y="Adhesion")
ggsave('Adhes_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Mod))+
  geom_point(aes(x=Date,y=Mod,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Modularite",x="Date",y="Modularite")
ggsave('Mod_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=meanN_liens))+
  geom_point(aes(x=Date,y=meanN_liens,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre moyen de liens",x="Date",y="Nombre moyen de liens")
ggsave('meanN_liens_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Assort))+
  geom_point(aes(x=Date,y=Assort,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Assortativite",x="Date",y="Assortativite")
ggsave('Assort_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Diss))+
  geom_point(aes(x=Date,y=Diss,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Dissimilarite",x="Date",y="Dissimilarite")
ggsave('Diss_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Trans))+
  geom_point(aes(x=Date,y=Trans,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Transitivite",x="Date",y="Transitivite")
ggsave('Trans_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=meanN_voisins))+
  geom_point(aes(x=Date,y=meanN_voisins,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre moyen de voisins",x="Date",y="Nombre moyen de voisins")
ggsave('meanN_voisins_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Nat_connect))+
  geom_point(aes(x=Date,y=Nat_connect,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Connectivite naturelle",x="Date",y="Connectivite naturelle")
ggsave('Nat_connect_station.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


# Correlation

Table.corr_all <- dplyr::select(data_reseaux,-Code_point_Libelle,-Date,-Bloom_Phylum,-P_dominance,-Month,-Year)
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
         type="upper", order="alphabet",
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=F, 
         title = "Correlation entre les métriques"
)

data_reseaux$Date2 <- as.Date(paste(data_reseaux$Year, data_reseaux$Month, "01", sep = "-"), format = "%Y-%m-%d")

datam <- summarise(group_by(data_reseaux,Code_point_Libelle,Month,Year), N_noeuds=mean(N_noeuds,na.rm=T),N_liens=mean(N_liens,na.rm=T),
                   D_liens=mean(D_liens,na.rm=T),C_tance=mean(C_tance,na.rm=T),Avg_p_length=mean(Avg_p_length,na.rm=T),
                   Adhes=mean(Adhes,na.rm=T),Mod=mean(Mod,na.rm=T),meanN_liens=mean(meanN_liens,na.rm=T),
                   Assort=mean(Assort,na.rm=T),Diss=mean(Diss,na.rm=T),Trans=mean(Trans,na.rm=T),
                   meanN_voisins=mean(meanN_voisins,na.rm=T),Nat_connect=mean(Nat_connect,na.rm=T),N_clust=mean(N_clust,na.rm=T))

# Au niveau des stations
ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_liens,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_liens_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_clust,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_clust_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_noeuds,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_noeuds_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=D_liens,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('D_liens_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=C_tance,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('C_tance_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Avg_p_length,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Avg_p_length_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Adhes,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Adhes_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Mod,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Mod_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_liens,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_liens_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Assort,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Assort_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Diss,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Diss_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Trans,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Trans_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_voisins,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_voisins_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Nat_connect,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Nat_connect_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

# Annee
ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_liens,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('N_liens_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_clust,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('N_clust_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_noeuds,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
  ggsave('N_noeuds_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=D_liens,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
  ggsave('D_liens_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=C_tance,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
  ggsave('C_tance_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Avg_p_length,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
  ggsave('Avg_p_length_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Adhes,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
  ggsave('Adhes_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Mod,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
  ggsave('Mod_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_liens,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
  ggsave('meanN_liens_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Assort,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
  ggsave('Assort_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Diss,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
  ggsave('Diss_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Trans,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
  ggsave('Trans_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_voisins,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
  ggsave('meanN_voisins_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Nat_connect,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
  ggsave('Nat_connect_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

# Au niveau du cluster
ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_liens,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_liens_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_noeuds,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_noeuds_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=D_liens,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('D_liens_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=C_tance,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('C_tance_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Avg_p_length,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Avg_p_length_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Adhes,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Adhes_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Mod,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Mod_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_liens,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_liens_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Assort,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Assort_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Diss,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Diss_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Trans,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Trans_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_voisins,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_voisins_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Nat_connect,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Nat_connect_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_clust,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_clust_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

# Annee
ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_liens,group=Year),size = 1)

ggsave('N_liens_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_noeuds,group=Year),size = 1)

ggsave('N_noeuds_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=D_liens,group=Year),size = 1)

ggsave('D_liens_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=C_tance,group=Year),size = 1)

ggsave('C_tance_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Avg_p_length,group=Year),size = 1)

ggsave('Avg_p_length_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Adhes,group=Year),size = 1)

ggsave('Adhes_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Mod,group=Year),size = 1)

ggsave('Mod_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_liens,group=Year),size = 1)

ggsave('meanN_liens_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Assort,group=Year),size = 1)

ggsave('Assort_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Diss,group=Year),size = 1)

ggsave('Diss_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Trans,group=Year),size = 1)

ggsave('Trans_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_voisins,group=Year),size = 1)

ggsave('meanN_voisins_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Nat_connect,group=Year),size = 1)

ggsave('Nat_connect_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_clust,group=Year),size = 1)

ggsave('N_clust_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

datal <- pivot_longer(datam,names_to = "Var",cols = N_noeuds:N_clust)
ggplot(datal)+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#F8766D",size = 1)+
  facet_wrap(~Var,scales = "free_y")+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  labs(title = "Metriques réseau cluster 1 (+)")
ggsave('Metriques_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')

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

datal2 <- pivot_longer(CL1_Mmetrics,names_to = "Var",cols = N_noeuds:N_clust)
ggplot(datal2)+
  geom_boxplot(aes(x=Date2,y=value,group=Date2),fill="#00BE67",size = 1)+
  facet_wrap(~Var,scales = "free_y")+
  labs(title = "Metriques réseau cluster 1 (+)")
ggsave('Metriques_cluster_ts.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')

ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=N_liens,group=Date2),fill="#00BE67")+
  labs(title="Nombre de liens",x="Date2",y="Nombre de liens")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))
ggsave('N_liens_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=N_clust,group=Date2),fill="#00BE67")+
  labs(title="Nombre de cluster",x="Date2",y="Nombre de liens")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))
ggsave('N_clust_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=N_noeuds,group=Date2),fill="#00BE67")+
  labs(title="Nombre de noeuds",x="Date2",y="Nombre de noeuds")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))
ggsave('N_noeuds_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=D_liens,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Densité des liens",x="Date2",y="Densité des liens")
ggsave('D_liens_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=C_tance,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Connectance",x="Date2",y="Connectance")
ggsave('C_tance_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Avg_p_length,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Longueur moyen des liens",x="Date2",y="Longueur moyen des liens")
ggsave('Avg_p_length_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Adhes,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Adhesion",x="Date2",y="Adhesion")
ggsave('Adhes_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Mod,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Modularite",x="Date2",y="Modularite")
ggsave('Mod_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=meanN_liens,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Nombre moyen de liens",x="Date2",y="Nombre moyen de liens")
ggsave('meanN_liens_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Assort,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Assortativite",x="Date2",y="Assortativite")
ggsave('Assort_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Diss,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Dissimilarite",x="Date2",y="Dissimilarite")
ggsave('Diss_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Trans,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Transitivite",x="Date2",y="Transitivite")
ggsave('Trans_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=meanN_voisins,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Nombre moyen de voisins",x="Date2",y="Nombre moyen de voisins")
ggsave('meanN_voisins_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL1_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Nat_connect,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Connectivite naturelle",x="Date2",y="Connectivite naturelle")
ggsave('Nat_connect_cluster.png', path = "output/graphs/Reseaux/TS_CLUST1/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')







#### Travail sur le cluster 2 #####
# Relancer le graphe global du cluster 2 avant #

# On travail uniquement sur les interactions positives
assoMat <- net$assoMat1
assoMat[assoMat < 0] <- 0

cluster2 <- graph_from_adjacency_matrix(assoMat,weighted = T,mode = "undirected",diag=F)
cluster2

# Visualisation générale avec IGRAPH
plot(cluster2)
wc <- cluster_fast_greedy(cluster2)

V(cluster2)$label <- V(cluster2)$name
#V(cluster2)$name <- paste("I'm #", net$edgelist1$v1)
V(cluster2)$page_rank <- round(page.rank(cluster2)$vector, 2)
V(cluster2)$betweenness <- round(betweenness(cluster2), 2)
V(cluster2)$degree <- degree(cluster2)
V(cluster2)$size <- V(cluster2)$degree
V(cluster2)$comm <- membership(wc)
V(cluster2)$color <- colorize(membership(wc))
E(cluster2)$width <- E(cluster2)$weight*6
E(cluster2)$color <- "black"

viz2 <- hchart(cluster2, layout = layout_with_fr)
# Enregistrement de la visualisation globale
htmlwidgets::saveWidget(viz2, "output/graphs/Reseaux/HTML/cluster2.html")

# Metriques réseau global 
S_net <-  vcount(cluster2) # nombre de noeuds
L_net <- ecount(cluster2) # nombre de liens

Z_mat <- L_net / S_net # linkage density or average number of links per nodes

C_net <- edge_density(cluster2, loops = FALSE) #connectance

# Average path length
avg_path_length <- mean_distance(
  cluster2,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)

#mean(distances(cluster2, weights=E(cluster2)$weight)) 

Edge_connect <- edge.connectivity(cluster2) # Edge connectivity = adhesion

Modularity <- modularity(cluster2,membership = membership(wc)) # Modularity

Vert_connect <- vertex.connectivity(cluster2) # Vertex connectivity = adhesion

m_degree <- mean(degree(cluster2)) #Nombre de liens moyen

assort <- assortativity_degree(cluster2,directed = F) #assortativite

diss <- mean(1 - E(cluster2)$weight) # Dissilarite as defined in NetCoMi

trans <- transitivity(cluster2,type = "global") #Transitivity

mean_edge_bet <- mean(edge_betweenness(cluster2)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(cluster2, attr = "weight",)) # OK
diag(adj) <- 1
nat_connect_notgood <- natural.connectivity(as.matrix(adj)) # Connectivite naturel

hubs <- eigen_centrality(
  cluster2,
  directed = FALSE,
  scale = TRUE,
  weights = NULL
)
Hubs <- as.data.frame(hubs$vector)
Hubs$Phyto <- rownames(Hubs)

Spe_hubs <- Hubs[order(desc(hubs$vector)),]
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster2.csv", row.names = FALSE,dec = ".")

# Preparation sous réseau 
# Index des espèces = noeuds
nodes_net <- V(cluster2)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

# Preparation tableau pour recuperer les infos station/date
CL2df <- filter(data,cluster == 2)

# Creation d'un df pour stocker les resultats
data_results_reseaux <- c("","")
data_results_reseaux <- as.data.frame(data_results_reseaux)

# Sous réseau
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
    
    # Metriques réseau global 
    S_net <-  vcount(sub) # nombre de noeuds
    L_net <- ecount(sub) # nombre de liens
    Z_mat <- L_net / S_net # linkage density or average number of links per nodes
    C_net <- edge_density(sub, loops = FALSE) #connectance
    
    # Average path length
    avg_path_length <- mean_distance(
      sub,
      directed = FALSE,
      unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
    )
    
    Edge_connect <- edge.connectivity(sub) # Edge connectivity = adhesion
    
    wc <- cluster_fast_greedy(sub)
    Modularity <- modularity(sub,membership = membership(wc)) # Modularity
    
    Vert_connect <- vertex.connectivity(sub) # Vertex connectivity = adhesion
    m_degree <- mean(degree(sub)) #Nombre de liens moyen
    assort <- assortativity_degree(sub,directed = F) #assortativite
    diss <- mean(1 - E(sub)$weight) # Dissilarite as defined in NetCoMi
    trans <- transitivity(sub,type = "global") #Transitivity
    mean_edge_bet <- mean(edge_betweenness(sub)) # Mean edge betweeness
    adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
    diag(adj) <- 1
    nat_connect_notgood <- natural.connectivity(as.matrix(adj)) # Connectivite naturel
    
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
    data_results_reseaux[i,15] <- nat_connect_notgood # Connectivite naturelle
    data_results_reseaux[i,16] <- length(wc) # Nombre de cluster
  } else { 
    
    data_results_reseaux[i,1] <- station
    data_results_reseaux[i,2] <- date
    data_results_reseaux[i,3] <- NA # nombre de noeuds
    data_results_reseaux[i,4] <- NA # nombre de liens
    data_results_reseaux[i,5] <- NA # Densite des liens
    data_results_reseaux[i,6] <- NA # connectance
    
    data_results_reseaux[i,7] <- NA  # longueur moyen des liens
    data_results_reseaux[i,8] <- NA  # adhesion
    data_results_reseaux[i,9] <- NA #modularite
    
    data_results_reseaux[i,10] <- NA  #Nombre de liens moyens
    data_results_reseaux[i,11] <- NA  # Assortativite
    data_results_reseaux[i,12] <- NA # dissimilarite
    data_results_reseaux[i,13] <- NA  # transitivite
    
    data_results_reseaux[i,14] <- NA  # Nombre moyen de voisins
    data_results_reseaux[i,15] <- NA  # Connectivite naturelle
    data_results_reseaux[i,16] <- NA # Nombre de cluster
    
  }
  
  colnames(data_results_reseaux) <- c("Code_point_Libelle","Date","N_noeuds","N_liens","D_liens","C_tance",
                                      "Avg_p_length","Adhes","Mod","meanN_liens","Assort","Diss","Trans","meanN_voisins",
                                      "Nat_connect","N_clust")
  
 #plot(sub,main = paste0(station,date),layout = layout_with_fr)
 #png(paste0("output/graphs/Reseaux/TS_CLUST2/",station,date,".png"))
 #plot(sub,main = paste0(station,date),layout = layout_with_fr)
 #dev.off()
 #htmlwidgets::saveWidget(viz_sub, paste0("output/graphs/Reseaux/HTML/CL2/",station,date,".html"))
 print(i/nrow(CL2)*100)
}
write.csv2(data_results_reseaux,file="data_modif/results_metrics_reseaux_cluster2_pos.csv", row.names = FALSE,dec = ".")

# Pour visualisation dynamique :
#{chemin_repertoire <- "output/graphs/Reseaux/HTML/CL2/"
  
  # Récupérez les noms de fichiers dans le répertoire
  noms_fichiers_complets <- list.files(chemin_repertoire, pattern = "\\.html$", full.names = TRUE)
  
  # Supprimez le chemin du répertoire des noms de fichiers
  noms_fichiers <- basename(noms_fichiers_complets)
  
  # Affichez les noms de fichiers
  cat("[", paste("'", noms_fichiers, "'", collapse = ", "), "]", "\n")}

# Analyse des résultats cluster 2
data_results_reseaux_copy <- data_results_reseaux

bloom <- dplyr::select(CL2df,Code_point_Libelle,Date,Bloom_Phylum,P_dominance)

data_reseaux <- left_join(data_results_reseaux,bloom)

data_reseaux <- data_reseaux[-c(653,654,655),] # supprimer les doublons

data_reseaux <- data_reseaux %>%
  mutate(Month = month(Date, label = F)) |>
  mutate(Year = year(Date))

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=N_noeuds))+
  geom_point(aes(x=Date,y=N_noeuds,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre de noeuds",x="Date",y="Nombre de noeuds")
ggsave('N_noeuds_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=N_liens))+
  geom_point(aes(x=Date,y=N_liens,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre de liens",x="Date",y="Nombre de liens")
ggsave('N_liens_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=D_liens))+
  geom_point(aes(x=Date,y=D_liens,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Densité des liens",x="Date",y="Densité des liens")
ggsave('D_liens_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=N_clust))+
  geom_point(aes(x=Date,y=N_clust,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Densité des liens",x="Date",y="Densité des liens")
ggsave('N_clust_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=C_tance))+
  geom_point(aes(x=Date,y=C_tance,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Connectance",x="Date",y="Connectance")
ggsave('C_tance_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Avg_p_length))+
  geom_point(aes(x=Date,y=Avg_p_length,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Longueur moyen des liens",x="Date",y="Longueur moyen des liens")
ggsave('Avg_p_length_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Adhes))+
  geom_point(aes(x=Date,y=Adhes,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Adhesion",x="Date",y="Adhesion")
ggsave('Adhes_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Mod))+
  geom_point(aes(x=Date,y=Mod,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Modularite",x="Date",y="Modularite")
ggsave('Mod_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=meanN_liens))+
  geom_point(aes(x=Date,y=meanN_liens,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre moyen de liens",x="Date",y="Nombre moyen de liens")
ggsave('meanN_liens_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Assort))+
  geom_point(aes(x=Date,y=Assort,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Assortativite",x="Date",y="Assortativite")
ggsave('Assort_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Diss))+
  geom_point(aes(x=Date,y=Diss,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Dissimilarite",x="Date",y="Dissimilarite")
ggsave('Diss_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Trans))+
  geom_point(aes(x=Date,y=Trans,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Transitivite",x="Date",y="Transitivite")
ggsave('Trans_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=meanN_voisins))+
  geom_point(aes(x=Date,y=meanN_voisins,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre moyen de voisins",x="Date",y="Nombre moyen de voisins")
ggsave('meanN_voisins_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Nat_connect))+
  geom_point(aes(x=Date,y=Nat_connect,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Connectivite naturelle",x="Date",y="Connectivite naturelle")
ggsave('Nat_connect_station.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


# Correlation

Table.corr_all <- dplyr::select(data_reseaux,-Code_point_Libelle,-Date,-Bloom_Phylum,-P_dominance,-Month,-Year)
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
         type="upper", order="alphabet",
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=F, 
         title = "Correlation entre les métriques"
)

data_reseaux$Date2 <- as.Date(paste(data_reseaux$Year, data_reseaux$Month, "01", sep = "-"), format = "%Y-%m-%d")

datam <- summarise(group_by(data_reseaux,Code_point_Libelle,Month,Year), N_noeuds=mean(N_noeuds,na.rm=T),N_liens=mean(N_liens,na.rm=T),
                   D_liens=mean(D_liens,na.rm=T),C_tance=mean(C_tance,na.rm=T),Avg_p_length=mean(Avg_p_length,na.rm=T),
                   Adhes=mean(Adhes,na.rm=T),Mod=mean(Mod,na.rm=T),meanN_liens=mean(meanN_liens,na.rm=T),
                   Assort=mean(Assort,na.rm=T),Diss=mean(Diss,na.rm=T),Trans=mean(Trans,na.rm=T),
                   meanN_voisins=mean(meanN_voisins,na.rm=T),Nat_connect=mean(Nat_connect,na.rm=T),N_clust=mean(N_clust,na.rm=T))

# Au niveau des stations
ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_liens,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_liens_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_clust,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_clust_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_noeuds,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_noeuds_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=D_liens,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('D_liens_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=C_tance,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('C_tance_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Avg_p_length,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Avg_p_length_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Adhes,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Adhes_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Mod,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Mod_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_liens,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_liens_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Assort,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Assort_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Diss,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Diss_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Trans,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Trans_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_voisins,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_voisins_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Nat_connect,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Nat_connect_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

# Annee
ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_liens,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('N_liens_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_noeuds,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('N_noeuds_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=D_liens,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('D_liens_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=C_tance,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('C_tance_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Avg_p_length,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Avg_p_length_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Adhes,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Adhes_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Mod,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Mod_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_liens,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('meanN_liens_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Assort,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Assort_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Diss,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Diss_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Trans,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Trans_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_voisins,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('meanN_voisins_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Nat_connect,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Nat_connect_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

# Au niveau du cluster
ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_liens,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_liens_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_noeuds,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_noeuds_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=D_liens,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('D_liens_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=C_tance,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('C_tance_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Avg_p_length,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Avg_p_length_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Adhes,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Adhes_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Mod,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Mod_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_liens,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_liens_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Assort,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Assort_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Diss,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Diss_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Trans,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Trans_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_voisins,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_voisins_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Nat_connect,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Nat_connect_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

# Annee
ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_liens,group=Year),size = 1)

ggsave('N_liens_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_noeuds,group=Year),size = 1)

ggsave('N_noeuds_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=D_liens,group=Year),size = 1)

ggsave('D_liens_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=C_tance,group=Year),size = 1)

ggsave('C_tance_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Avg_p_length,group=Year),size = 1)

ggsave('Avg_p_length_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Adhes,group=Year),size = 1)

ggsave('Adhes_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Mod,group=Year),size = 1)

ggsave('Mod_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_liens,group=Year),size = 1)

ggsave('meanN_liens_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Assort,group=Year),size = 1)

ggsave('Assort_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Diss,group=Year),size = 1)

ggsave('Diss_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Trans,group=Year),size = 1)

ggsave('Trans_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_voisins,group=Year),size = 1)

ggsave('meanN_voisins_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Nat_connect,group=Year),size = 1)

ggsave('Nat_connect_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


datal <- pivot_longer(datam,names_to = "Var",cols = N_noeuds:N_clust)
ggplot(datal)+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#CD9600",size = 1)+
  facet_wrap(~Var,scales = "free_y")+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  labs(title = "Metriques réseau cluster 2 (+)")
ggsave('Metriques_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')

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

ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=N_liens,group=Date2),fill="#CD9600")+
  labs(title="Nombre de liens",x="Date2",y="Nombre de liens")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))
ggsave('N_liens_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=N_clust,group=Date2),fill="#CD9600")+
  labs(title="Nombre de cluster",x="Date2",y="Nombre de liens")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))
ggsave('N_clust_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=N_noeuds,group=Date2),fill="#CD9600")+
  labs(title="Nombre de noeuds",x="Date2",y="Nombre de noeuds")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))
ggsave('N_noeuds_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=D_liens,group=Date2),fill="#CD9600")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Densité des liens",x="Date2",y="Densité des liens")
ggsave('D_liens_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=C_tance,group=Date2),fill="#CD9600")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Connectance",x="Date2",y="Connectance")
ggsave('C_tance_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Avg_p_length,group=Date2),fill="#CD9600")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Longueur moyen des liens",x="Date2",y="Longueur moyen des liens")
ggsave('Avg_p_length_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Adhes,group=Date2),fill="#CD9600")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Adhesion",x="Date2",y="Adhesion")
ggsave('Adhes_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Mod,group=Date2),fill="#CD9600")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Modularite",x="Date2",y="Modularite")
ggsave('Mod_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=meanN_liens,group=Date2),fill="#CD9600")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Nombre moyen de liens",x="Date2",y="Nombre moyen de liens")
ggsave('meanN_liens_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Assort,group=Date2),fill="#CD9600")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Assortativite",x="Date2",y="Assortativite")
ggsave('Assort_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Diss,group=Date2),fill="#CD9600")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Dissimilarite",x="Date2",y="Dissimilarite")
ggsave('Diss_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Trans,group=Date2),fill="#CD9600")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Transitivite",x="Date2",y="Transitivite")
ggsave('Trans_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=meanN_voisins,group=Date2),fill="#CD9600")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Nombre moyen de voisins",x="Date2",y="Nombre moyen de voisins")
ggsave('meanN_voisins_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL2_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Nat_connect,group=Date2),fill="#CD9600")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Connectivite naturelle",x="Date2",y="Connectivite naturelle")
ggsave('Nat_connect_cluster.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

datal2 <- pivot_longer(CL2_Mmetrics,names_to = "Var",cols = N_noeuds:N_clust)
ggplot(datal2)+
  geom_boxplot(aes(x=Date2,y=value,group=Date2),fill="#00BE67",size = 1)+
  facet_wrap(~Var,scales = "free_y")+
  labs(title = "Metriques réseau cluster 2 (+)")
ggsave('Metriques_cluster_ts.png', path = "output/graphs/Reseaux/TS_CLUST2/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')




#### Travail sur le cluster 4 #####
# Relancer le graphe global du cluster 4 avant #

# On travail uniquement sur les interactions positives
assoMat <- net$assoMat1
assoMat[assoMat < 0] <- 0

cluster4 <- graph_from_adjacency_matrix(assoMat,weighted = T,mode = "undirected",diag=F)
cluster4

# Visualisation générale avec IGRAPH
plot(cluster4)
wc <- cluster_fast_greedy(cluster4)

V(cluster4)$label <- V(cluster4)$name
#V(cluster4)$name <- paste("I'm #", net$edgelist1$v1)
V(cluster4)$page_rank <- round(page.rank(cluster4)$vector, 2)
V(cluster4)$betweenness <- round(betweenness(cluster4), 2)
V(cluster4)$degree <- degree(cluster4)
V(cluster4)$size <- V(cluster4)$degree
V(cluster4)$comm <- membership(wc)
V(cluster4)$color <- colorize(membership(wc))
E(cluster4)$width <- E(cluster4)$weight*6
E(cluster4)$color <- "black"

viz4 <- hchart(cluster4, layout = layout_with_fr)
# Enregistrement de la visualisation globale
htmlwidgets::saveWidget(viz4, "output/graphs/Reseaux/HTML/cluster4.html")

# Metriques réseau global 
S_net <-  vcount(cluster4) # nombre de noeuds
L_net <- ecount(cluster4) # nombre de liens

Z_mat <- L_net / S_net # linkage density or average number of links per nodes

C_net <- edge_density(cluster4, loops = FALSE) #connectance

# Average path length
avg_path_length <- mean_distance(
  cluster4,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)

#mean(distances(cluster4, weights=E(cluster4)$weight)) 

Edge_connect <- edge.connectivity(cluster4) # Edge connectivity = adhesion

Modularity <- modularity(cluster4,membership = membership(wc)) # Modularity

Vert_connect <- vertex.connectivity(cluster4) # Vertex connectivity = adhesion

m_degree <- mean(degree(cluster4)) #Nombre de liens moyen

assort <- assortativity_degree(cluster4,directed = F) #assortativite

diss <- mean(1 - E(cluster4)$weight) # Dissilarite as defined in NetCoMi

trans <- transitivity(cluster4,type = "global") #Transitivity

mean_edge_bet <- mean(edge_betweenness(cluster4)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(cluster4, attr = "weight",)) # OK
diag(adj) <- 1
nat_connect_notgood <- natural.connectivity(as.matrix(adj)) # Connectivite naturel

hubs <- eigen_centrality(
  cluster4,
  directed = FALSE,
  scale = TRUE,
  weights = NULL
)
Hubs <- as.data.frame(hubs$vector)
Hubs$Phyto <- rownames(Hubs)

Spe_hubs <- Hubs[order(desc(hubs$vector)),]
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster4.csv", row.names = FALSE,dec = ".")

# Preparation sous réseau 
# Index des espèces = noeuds
nodes_net <- V(cluster4)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

# Preparation tableau pour recuperer les infos station/date
CL4df <- filter(data,cluster == 4)

# Creation d'un df pour stocker les resultats
data_results_reseaux <- c("","")
data_results_reseaux <- as.data.frame(data_results_reseaux)

# Sous réseau
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
    
    # Metriques réseau global 
    S_net <-  vcount(sub) # nombre de noeuds
    L_net <- ecount(sub) # nombre de liens
    Z_mat <- L_net / S_net # linkage density or average number of links per nodes
    C_net <- edge_density(sub, loops = FALSE) #connectance
    
    # Average path length
    avg_path_length <- mean_distance(
      sub,
      directed = FALSE,
      unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
    )
    
    Edge_connect <- edge.connectivity(sub) # Edge connectivity = adhesion
    
    wc <- cluster_fast_greedy(sub)
    Modularity <- modularity(sub,membership = membership(wc)) # Modularity
    
    Vert_connect <- vertex.connectivity(sub) # Vertex connectivity = adhesion
    m_degree <- mean(degree(sub)) #Nombre de liens moyen
    assort <- assortativity_degree(sub,directed = F) #assortativite
    diss <- mean(1 - E(sub)$weight) # Dissilarite as defined in NetCoMi
    trans <- transitivity(sub,type = "global") #Transitivity
    mean_edge_bet <- mean(edge_betweenness(sub)) # Mean edge betweeness
    adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
    diag(adj) <- 1
    nat_connect_notgood <- natural.connectivity(as.matrix(adj)) # Connectivite naturel
    
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
    data_results_reseaux[i,15] <- nat_connect_notgood # Connectivite naturelle
    data_results_reseaux[i,16] <- length(wc) # Nombre de cluster
  } else { 
    
    data_results_reseaux[i,1] <- station
    data_results_reseaux[i,2] <- date
    data_results_reseaux[i,3] <- NA # nombre de noeuds
    data_results_reseaux[i,4] <- NA # nombre de liens
    data_results_reseaux[i,5] <- NA # Densite des liens
    data_results_reseaux[i,6] <- NA # connectance
    
    data_results_reseaux[i,7] <- NA  # longueur moyen des liens
    data_results_reseaux[i,8] <- NA  # adhesion
    data_results_reseaux[i,9] <- NA #modularite
    
    data_results_reseaux[i,10] <- NA  #Nombre de liens moyens
    data_results_reseaux[i,11] <- NA  # Assortativite
    data_results_reseaux[i,12] <- NA # dissimilarite
    data_results_reseaux[i,13] <- NA  # transitivite
    
    data_results_reseaux[i,14] <- NA  # Nombre moyen de voisins
    data_results_reseaux[i,15] <- NA  # Connectivite naturelle
    data_results_reseaux[i,16] <- NA # Nombre de cluster
    
  }
  
  colnames(data_results_reseaux) <- c("Code_point_Libelle","Date","N_noeuds","N_liens","D_liens","C_tance",
                                      "Avg_p_length","Adhes","Mod","meanN_liens","Assort","Diss","Trans","meanN_voisins",
                                      "Nat_connect","N_clust")
  
  #plot(sub,main = paste0(station,date),layout = layout_with_fr)
  #png(paste0("output/graphs/Reseaux/TS_CLUST4/",station,date,".png"))
  #plot(sub,main = paste0(station,date),layout = layout_with_fr)
  #dev.off()
  #htmlwidgets::saveWidget(viz_sub, paste0("output/graphs/Reseaux/HTML/CL4/",station,date,".html"))
  print(i/nrow(CL4)*100)
}
write.csv2(data_results_reseaux,file="data_modif/results_metrics_reseaux_cluster4_pos.csv", row.names = FALSE,dec = ".")

# Pour visualisation dynamique :
#{chemin_repertoire <- "output/graphs/Reseaux/HTML/CL4/"
  
  # Récupérez les noms de fichiers dans le répertoire
  noms_fichiers_complets <- list.files(chemin_repertoire, pattern = "\\.html$", full.names = TRUE)
  
  # Supprimez le chemin du répertoire des noms de fichiers
  noms_fichiers <- basename(noms_fichiers_complets)
  
  # Affichez les noms de fichiers
  cat("[", paste("'", noms_fichiers, "'", collapse = ", "), "]", "\n")}

# Analyse des résultats cluster 2
data_results_reseaux_copy <- data_results_reseaux

data_results_reseaux <- read_delim("data_modif/results_metrics_reseaux_cluster4_pos.csv", 
                                               delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                                               locale = locale(decimal_mark = ",", grouping_mark = "."), 
                                               trim_ws = TRUE)

bloom <- dplyr::select(CL4df,Code_point_Libelle,Date,Bloom_Phylum,P_dominance)

data_reseaux <- left_join(data_results_reseaux,bloom)

data_reseaux <- data_reseaux %>%
  mutate(Month = month(Date, label = F)) |>
  mutate(Year = year(Date))

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=N_noeuds))+
  geom_point(aes(x=Date,y=N_noeuds,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre de noeuds",x="Date",y="Nombre de noeuds")
ggsave('N_noeuds_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=N_liens))+
  geom_point(aes(x=Date,y=N_liens,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre de liens",x="Date",y="Nombre de liens")
ggsave('N_liens_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=D_liens))+
  geom_point(aes(x=Date,y=D_liens,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Densité des liens",x="Date",y="Densité des liens")
ggsave('D_liens_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=C_tance))+
  geom_point(aes(x=Date,y=C_tance,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Connectance",x="Date",y="Connectance")
ggsave('C_tance_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=N_clust))+
  geom_point(aes(x=Date,y=N_clust,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre de cluster",x="Date",y="Connectance")
ggsave('N_clust_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Avg_p_length))+
  geom_point(aes(x=Date,y=Avg_p_length,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Longueur moyen des liens",x="Date",y="Longueur moyen des liens")
ggsave('Avg_p_length_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Adhes))+
  geom_point(aes(x=Date,y=Adhes,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Adhesion",x="Date",y="Adhesion")
ggsave('Adhes_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Mod))+
  geom_point(aes(x=Date,y=Mod,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Modularite",x="Date",y="Modularite")
ggsave('Mod_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=meanN_liens))+
  geom_point(aes(x=Date,y=meanN_liens,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre moyen de liens",x="Date",y="Nombre moyen de liens")
ggsave('meanN_liens_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Assort))+
  geom_point(aes(x=Date,y=Assort,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Assortativite",x="Date",y="Assortativite")
ggsave('Assort_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Diss))+
  geom_point(aes(x=Date,y=Diss,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Dissimilarite",x="Date",y="Dissimilarite")
ggsave('Diss_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Trans))+
  geom_point(aes(x=Date,y=Trans,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Transitivite",x="Date",y="Transitivite")
ggsave('Trans_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=meanN_voisins))+
  geom_point(aes(x=Date,y=meanN_voisins,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Nombre moyen de voisins",x="Date",y="Nombre moyen de voisins")
ggsave('meanN_voisins_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(data_reseaux)+
  geom_line(aes(x=Date,y=Nat_connect))+
  geom_point(aes(x=Date,y=Nat_connect,colour=Bloom_Phylum))+
  facet_wrap(~Code_point_Libelle)+
  labs(title="Connectivite naturelle",x="Date",y="Connectivite naturelle")
ggsave('Nat_connect_station.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


# Correlation

Table.corr_all <- dplyr::select(data_reseaux,-Code_point_Libelle,-Date,-Bloom_Phylum,-P_dominance,-Month,-Year)
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
         type="upper", order="alphabet",
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=F, 
         title = "Correlation entre les métriques"
)

data_reseaux$Date2 <- as.Date(paste(data_reseaux$Year, data_reseaux$Month, "01", sep = "-"), format = "%Y-%m-%d")

datam <- summarise(group_by(data_reseaux,Code_point_Libelle,Month,Year), N_noeuds=mean(N_noeuds,na.rm=T),N_liens=mean(N_liens,na.rm=T),
                   D_liens=mean(D_liens,na.rm=T),C_tance=mean(C_tance,na.rm=T),Avg_p_length=mean(Avg_p_length,na.rm=T),
                   Adhes=mean(Adhes,na.rm=T),Mod=mean(Mod,na.rm=T),meanN_liens=mean(meanN_liens,na.rm=T),
                   Assort=mean(Assort,na.rm=T),Diss=mean(Diss,na.rm=T),Trans=mean(Trans,na.rm=T),
                   meanN_voisins=mean(meanN_voisins,na.rm=T),Nat_connect=mean(Nat_connect,na.rm=T),N_clust=mean(N_clust,na.rm=T))

# Au niveau des stations
ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_liens,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_liens_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_clust,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_clust_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_noeuds,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_noeuds_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=D_liens,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('D_liens_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=C_tance,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('C_tance_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Avg_p_length,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Avg_p_length_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Adhes,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Adhes_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Mod,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Mod_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_liens,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_liens_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Assort,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Assort_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Diss,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Diss_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Trans,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Trans_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_voisins,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_voisins_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Nat_connect,group=Month),size = 1)+
  facet_wrap(~Code_point_Libelle)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Nat_connect_station_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

# Annee
ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_liens,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('N_liens_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_noeuds,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('N_noeuds_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=D_liens,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('D_liens_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=C_tance,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('C_tance_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Avg_p_length,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Avg_p_length_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Adhes,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Adhes_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Mod,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Mod_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_liens,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('meanN_liens_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Assort,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Assort_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Diss,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Diss_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Trans,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Trans_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_voisins,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('meanN_voisins_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Nat_connect,group=Year),size = 1)+
  facet_wrap(~Code_point_Libelle)
ggsave('Nat_connect_station_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

# Au niveau du cluster
ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_liens,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_liens_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_noeuds,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_noeuds_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=D_liens,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('D_liens_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Month,y=C_tance,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('C_tance_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Avg_p_length,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Avg_p_length_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Adhes,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Adhes_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Mod,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Mod_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_liens,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_liens_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Assort,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Assort_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Diss,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Diss_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Trans,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Trans_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=meanN_voisins,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('meanN_voisins_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=Nat_connect,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('Nat_connect_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Month,y=N_clust,group=Month),size = 1)+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))
ggsave('N_clust_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


# Annee
ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_liens,group=Year),size = 1)

ggsave('N_liens_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_clust,group=Year),size = 1)

ggsave('N_clust_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=N_noeuds,group=Year),size = 1)

ggsave('N_noeuds_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=D_liens,group=Year),size = 1)

ggsave('D_liens_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(datam)+
  geom_boxplot(aes(x=Year,y=C_tance,group=Year),size = 1)

ggsave('C_tance_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Avg_p_length,group=Year),size = 1)

ggsave('Avg_p_length_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Adhes,group=Year),size = 1)

ggsave('Adhes_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Mod,group=Year),size = 1)

ggsave('Mod_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_liens,group=Year),size = 1)

ggsave('meanN_liens_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Assort,group=Year),size = 1)

ggsave('Assort_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Diss,group=Year),size = 1)

ggsave('Diss_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Trans,group=Year),size = 1)

ggsave('Trans_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=meanN_voisins,group=Year),size = 1)

ggsave('meanN_voisins_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(datam)+
  geom_boxplot(aes(x=Year,y=Nat_connect,group=Year),size = 1)

ggsave('Nat_connect_cluster_annee.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

datal <- pivot_longer(datam,names_to = "Var",cols = N_noeuds:N_clust)
ggplot(datal)+
  geom_boxplot(aes(x=Month,y=value,group=Month),fill="#00A9FF",size = 1)+
  facet_wrap(~Var,scales = "free_y")+
  scale_x_continuous(breaks = seq(1,12,1),limits = c(0.5,12.5))+
  labs(title = "Metriques réseau cluster 4 (+)")
ggsave('Metriques_cluster_mois.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')


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

ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=N_liens,group=Date2),fill="#00BE67")+
  labs(title="Nombre de liens",x="Date2",y="Nombre de liens")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))
ggsave('N_liens_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')



ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=N_noeuds,group=Date2),fill="#00BE67")+
  labs(title="Nombre de noeuds",x="Date2",y="Nombre de noeuds")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))
ggsave('N_noeuds_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=D_liens,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Densité des liens",x="Date2",y="Densité des liens")
ggsave('D_liens_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=C_tance,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Connectance",x="Date2",y="Connectance")
ggsave('C_tance_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Avg_p_length,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Longueur moyen des liens",x="Date2",y="Longueur moyen des liens")
ggsave('Avg_p_length_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Adhes,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Adhesion",x="Date2",y="Adhesion")
ggsave('Adhes_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Mod,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Modularite",x="Date2",y="Modularite")
ggsave('Mod_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=meanN_liens,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Nombre moyen de liens",x="Date2",y="Nombre moyen de liens")
ggsave('meanN_liens_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Assort,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Assortativite",x="Date2",y="Assortativite")
ggsave('Assort_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Diss,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Dissimilarite",x="Date2",y="Dissimilarite")
ggsave('Diss_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Trans,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Transitivite",x="Date2",y="Transitivite")
ggsave('Trans_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=meanN_voisins,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Nombre moyen de voisins",x="Date2",y="Nombre moyen de voisins")
ggsave('meanN_voisins_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')

ggplot(CL4_Mmetrics)+
  geom_boxplot(aes(x=Date2,y=Nat_connect,group=Date2),fill="#00BE67")+
  scale_x_date(date_labels = "%Y-%m",date_breaks = "2 month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size = 6))+
  labs(title="Connectivite naturelle",x="Date2",y="Connectivite naturelle")
ggsave('Nat_connect_cluster.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 400, height = 280, units = 'mm')


datal2 <- pivot_longer(CL4_Mmetrics,names_to = "Var",cols = N_noeuds:N_clust)
ggplot(datal2)+
  geom_boxplot(aes(x=Date2,y=value,group=Date2),fill="#00BE67",size = 1)+
  facet_wrap(~Var,scales = "free_y")+
  labs(title = "Metriques réseau cluster 4 (+)")
ggsave('Metriques_cluster_ts.png', path = "output/graphs/Reseaux/TS_CLUST4/Metrics",dpi = 600, width = 600, height = 480, units = 'mm')


#### Correlation avec indice de diversité #####
data_results_reseaux <- read_delim("data_modif/results_metrics_reseaux_cluster4_pos.csv", 
                                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                                   trim_ws = TRUE)

data$Rspe <- rowSums(data[,c(24:247)] != 0,na.rm = T)

met <- dplyr::select(data,Code_point_Libelle, Date, Shannon, Pielou, BergerParker, Abdtot,Rspe)
datacorr <- left_join(data_results_reseaux,met)
Table.corr_all <- dplyr::select(datacorr,-Code_point_Libelle,-Date)
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
         title = "Correlation entre métriques et diversité Cluster 4"
)


data_results_reseaux <- read_delim("data_modif/results_metrics_reseaux_cluster1.csv", 
                                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                                   trim_ws = TRUE)

data$Rspe <- rowSums(data[,c(24:247)] != 0,na.rm = T)

met <- dplyr::select(data,Code_point_Libelle, Date, Shannon, Pielou, BergerParker, Abdtot,Rspe)
datacorr <- left_join(data_results_reseaux,met)
Table.corr_all <- dplyr::select(datacorr,-Code_point_Libelle,-Date)
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
         title = "Correlation entre métriques et diversité Cluster 1"
)

data_results_reseaux <- read_delim("data_modif/results_metrics_reseaux_cluster2_pos.csv", 
                                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                                   trim_ws = TRUE)

data$Rspe <- rowSums(data[,c(24:247)] != 0,na.rm = T)

met <- dplyr::select(data,Code_point_Libelle, Date, Shannon, Pielou, BergerParker, Abdtot,Rspe)
datacorr <- left_join(data_results_reseaux,met)
Table.corr_all <- dplyr::select(datacorr,-Code_point_Libelle,-Date)
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
         title = "Correlation entre métriques et diversité Cluster 2"
)

data_results_reseaux <- read_delim("data_modif/results_metrics_reseaux_cluster3_pos.csv", 
                                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                                   trim_ws = TRUE)

data$Rspe <- rowSums(data[,c(24:247)] != 0,na.rm = T)

met <- dplyr::select(data,Code_point_Libelle, Date, Shannon, Pielou, BergerParker, Abdtot,Rspe)
datacorr <- left_join(data_results_reseaux,met)
Table.corr_all <- dplyr::select(datacorr,-Code_point_Libelle,-Date)
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
         title = "Correlation entre métriques et diversité Cluster 3"
)




### Sous graphe par saison pour chaque cluster ####
data<- data |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer",
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))

####Cluster 3##### 
# Hiver #
CL3 <- filter(data, cluster == 3,season == "Winter" )
#rownames(CL1) <- CL1$Date
CL3 <- dplyr::select(CL3,Actinoptychus:Coscinodiscophycidae)
Spe_w <- as.data.frame(t(summarise_all(CL3,.funs = list(mean = ~mean(., na.rm = TRUE)))))
Spe_w$Phyto <- colnames(CL3)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index des espèces = noeuds
nodes_net <- V(cluster3)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)

vids <- Spe$Pindex
sub <- igraph::subgraph(cluster3, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
plot(sub)

  vcount(sub) # nombre de noeuds
 ecount(sub) # nombre de liens
 ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
 edge_density(sub, loops = FALSE) #connectance

# Average path length
 mean_distance(
  sub,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)
 edge.connectivity(sub) # Edge connectivity = adhesion

wc <- cluster_fast_greedy(sub)
 modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
 mean(degree(sub)) #Nombre de liens moyen
assortativity_degree(sub,directed = F) #assortativite
mean(1 - E(sub)$weight) # Dissilarite as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # Connectivite naturel

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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster1_hiver.csv", row.names = FALSE,dec = ".")

# Automne
CL3 <- filter(data, cluster == 3,season == "Fall" )
#rownames(CL1) <- CL1$Date
CL3 <- dplyr::select(CL3,Actinoptychus:Coscinodiscophycidae)
Spe_w <- as.data.frame(t(summarise_all(CL3,.funs = list(mean = ~mean(., na.rm = TRUE)))))
Spe_w$Phyto <- colnames(CL3)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index des espèces = noeuds
nodes_net <- V(cluster3)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)

vids <- Spe$Pindex
sub <- igraph::subgraph(cluster3, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
plot(sub)

vcount(sub) # nombre de noeuds
ecount(sub) # nombre de liens
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) #Nombre de liens moyen
assortativity_degree(sub,directed = F) #assortativite
mean(1 - E(sub)$weight) # Dissilarite as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # Connectivite naturel

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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster1_automne.csv", row.names = FALSE,dec = ".")




# Printemps
CL3 <- filter(data, cluster == 3,season == "Spring" )
#rownames(CL1) <- CL1$Date
CL3 <- dplyr::select(CL3,Actinoptychus:Coscinodiscophycidae)
Spe_w <- as.data.frame(t(summarise_all(CL3,.funs = list(mean = ~mean(., na.rm = TRUE)))))
Spe_w$Phyto <- colnames(CL3)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index des espèces = noeuds
nodes_net <- V(cluster3)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)

vids <- Spe$Pindex
sub <- igraph::subgraph(cluster3, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
plot(sub)

vcount(sub) # nombre de noeuds
ecount(sub) # nombre de liens
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) #Nombre de liens moyen
assortativity_degree(sub,directed = F) #assortativite
mean(1 - E(sub)$weight) # Dissilarite as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # Connectivite naturel

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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster1_hiver.csv", row.names = FALSE,dec = ".")




# Ete
CL3 <- filter(data, cluster == 3,season == "Summer" )
#rownames(CL1) <- CL1$Date
CL3 <- dplyr::select(CL3,Actinoptychus:Coscinodiscophycidae)
Spe_w <- as.data.frame(t(summarise_all(CL3,.funs = list(mean = ~mean(., na.rm = TRUE)))))
Spe_w$Phyto <- colnames(CL3)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index des espèces = noeuds
nodes_net <- V(cluster3)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)

vids <- Spe$Pindex
sub <- igraph::subgraph(cluster3, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
plot(sub)

vcount(sub) # nombre de noeuds
ecount(sub) # nombre de liens
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) #Nombre de liens moyen
assortativity_degree(sub,directed = F) #assortativite
mean(1 - E(sub)$weight) # Dissilarite as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # Connectivite naturel

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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster1_hiver.csv", row.names = FALSE,dec = ".")


####Cluster 1##### 
# Hiver #
CL1 <- filter(data, cluster == 1,season == "Winter" )
#rownames(CL1) <- CL1$Date
CL1 <- dplyr::select(CL1,Actinoptychus:Coscinodiscophycidae)
Spe_w <- as.data.frame(t(summarise_all(CL1,.funs = list(mean = ~mean(., na.rm = TRUE)))))
Spe_w$Phyto <- colnames(CL1)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index des espèces = noeuds
nodes_net <- V(cluster1)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)

vids <- Spe$Pindex
sub <- igraph::subgraph(cluster1, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
plot(sub)

vcount(sub) # nombre de noeuds
ecount(sub) # nombre de liens
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) #Nombre de liens moyen
assortativity_degree(sub,directed = F) #assortativite
mean(1 - E(sub)$weight) # Dissilarite as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # Connectivite naturel

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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster1_hiver.csv", row.names = FALSE,dec = ".")

# Automne
CL1 <- filter(data, cluster == 1,season == "Fall" )
#rownames(CL1) <- CL1$Date
CL1 <- dplyr::select(CL1,Actinoptychus:Coscinodiscophycidae)
Spe_w <- as.data.frame(t(summarise_all(CL1,.funs = list(mean = ~mean(., na.rm = TRUE)))))
Spe_w$Phyto <- colnames(CL1)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index des espèces = noeuds
nodes_net <- V(cluster1)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)

vids <- Spe$Pindex
sub <- igraph::subgraph(cluster1, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
plot(sub)

vcount(sub) # nombre de noeuds
ecount(sub) # nombre de liens
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) #Nombre de liens moyen
assortativity_degree(sub,directed = F) #assortativite
mean(1 - E(sub)$weight) # Dissilarite as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # Connectivite naturel

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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster1_automne.csv", row.names = FALSE,dec = ".")




# Printemps
CL1 <- filter(data, cluster == 1,season == "Spring" )
#rownames(CL1) <- CL1$Date
CL1 <- dplyr::select(CL1,Actinoptychus:Coscinodiscophycidae)
Spe_w <- as.data.frame(t(summarise_all(CL1,.funs = list(mean = ~mean(., na.rm = TRUE)))))
Spe_w$Phyto <- colnames(CL1)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index des espèces = noeuds
nodes_net <- V(cluster1)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)

vids <- Spe$Pindex
sub <- igraph::subgraph(cluster1, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
plot(sub)

vcount(sub) # nombre de noeuds
ecount(sub) # nombre de liens
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) #Nombre de liens moyen
assortativity_degree(sub,directed = F) #assortativite
mean(1 - E(sub)$weight) # Dissilarite as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # Connectivite naturel

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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster1_hiver.csv", row.names = FALSE,dec = ".")




# Ete
CL1 <- filter(data, cluster == 1,season == "Summer" )
#rownames(CL1) <- CL1$Date
CL1 <- dplyr::select(CL1,Actinoptychus:Coscinodiscophycidae)
Spe_w <- as.data.frame(t(summarise_all(CL1,.funs = list(mean = ~mean(., na.rm = TRUE)))))
Spe_w$Phyto <- colnames(CL1)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index des espèces = noeuds
nodes_net <- V(cluster1)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)

vids <- Spe$Pindex
sub <- igraph::subgraph(cluster1, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
plot(sub)

vcount(sub) # nombre de noeuds
ecount(sub) # nombre de liens
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) #Nombre de liens moyen
assortativity_degree(sub,directed = F) #assortativite
mean(1 - E(sub)$weight) # Dissilarite as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # Connectivite naturel

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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster1_hiver.csv", row.names = FALSE,dec = ".")


####Cluster 2##### 
# Hiver #
CL2 <- filter(data, cluster == 2,season == "Winter" )
#rownames(CL1) <- CL1$Date
CL2 <- dplyr::select(CL2,Actinoptychus:Coscinodiscophycidae)
Spe_w <- as.data.frame(t(summarise_all(CL2,.funs = list(mean = ~mean(., na.rm = TRUE)))))
Spe_w$Phyto <- colnames(CL2)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index des espèces = noeuds
nodes_net <- V(cluster2)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)

vids <- Spe$Pindex
sub <- igraph::subgraph(cluster2, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
plot(sub)

vcount(sub) # nombre de noeuds
ecount(sub) # nombre de liens
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) #Nombre de liens moyen
assortativity_degree(sub,directed = F) #assortativite
mean(1 - E(sub)$weight) # Dissilarite as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # Connectivite naturel

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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster2_hiver.csv", row.names = FALSE,dec = ".")

# Automne
CL2 <- filter(data, cluster == 2,season == "Fall" )
#rownames(CL1) <- CL1$Date
CL2 <- dplyr::select(CL2,Actinoptychus:Coscinodiscophycidae)
Spe_w <- as.data.frame(t(summarise_all(CL2,.funs = list(mean = ~mean(., na.rm = TRUE)))))
Spe_w$Phyto <- colnames(CL2)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index des espèces = noeuds
nodes_net <- V(cluster2)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)

vids <- Spe$Pindex
sub <- igraph::subgraph(cluster2, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
plot(sub)

vcount(sub) # nombre de noeuds
ecount(sub) # nombre de liens
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) #Nombre de liens moyen
assortativity_degree(sub,directed = F) #assortativite
mean(1 - E(sub)$weight) # Dissilarite as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # Connectivite naturel

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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster2_automne.csv", row.names = FALSE,dec = ".")




# Printemps
CL2 <- filter(data, cluster == 2,season == "Spring" )
#rownames(CL1) <- CL1$Date
CL2 <- dplyr::select(CL2,Actinoptychus:Coscinodiscophycidae)
Spe_w <- as.data.frame(t(summarise_all(CL2,.funs = list(mean = ~mean(., na.rm = TRUE)))))
Spe_w$Phyto <- colnames(CL2)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index des espèces = noeuds
nodes_net <- V(cluster2)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)

vids <- Spe$Pindex
sub <- igraph::subgraph(cluster2, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
plot(sub)

vcount(sub) # nombre de noeuds
ecount(sub) # nombre de liens
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) #Nombre de liens moyen
assortativity_degree(sub,directed = F) #assortativite
mean(1 - E(sub)$weight) # Dissilarite as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # Connectivite naturel

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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster2_hiver.csv", row.names = FALSE,dec = ".")




# Ete
CL2 <- filter(data, cluster == 2,season == "Summer" )
#rownames(CL1) <- CL1$Date
CL2 <- dplyr::select(CL2,Actinoptychus:Coscinodiscophycidae)
Spe_w <- as.data.frame(t(summarise_all(CL2,.funs = list(mean = ~mean(., na.rm = TRUE)))))
Spe_w$Phyto <- colnames(CL2)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index des espèces = noeuds
nodes_net <- V(cluster2)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)

vids <- Spe$Pindex
sub <- igraph::subgraph(cluster2, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
plot(sub)

vcount(sub) # nombre de noeuds
ecount(sub) # nombre de liens
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) #Nombre de liens moyen
assortativity_degree(sub,directed = F) #assortativite
mean(1 - E(sub)$weight) # Dissilarite as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # Connectivite naturel

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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster2_hiver.csv", row.names = FALSE,dec = ".")

####Cluster 4##### 
# Hiver #
CL4 <- filter(data, cluster == 4,season == "Winter" )
#rownames(CL1) <- CL1$Date
CL4 <- dplyr::select(CL4,Actinoptychus:Coscinodiscophycidae)
Spe_w <- as.data.frame(t(summarise_all(CL4,.funs = list(mean = ~mean(., na.rm = TRUE)))))
Spe_w$Phyto <- colnames(CL4)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index des espèces = noeuds
nodes_net <- V(cluster4)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)

vids <- Spe$Pindex
sub <- igraph::subgraph(cluster4, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
plot(sub)

vcount(sub) # nombre de noeuds
ecount(sub) # nombre de liens
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) #Nombre de liens moyen
assortativity_degree(sub,directed = F) #assortativite
mean(1 - E(sub)$weight) # Dissilarite as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # Connectivite naturel

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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster4_hiver.csv", row.names = FALSE,dec = ".")

# Automne
CL4 <- filter(data, cluster == 4,season == "Fall" )
#rownames(CL1) <- CL1$Date
CL4 <- dplyr::select(CL4,Actinoptychus:Coscinodiscophycidae)
Spe_w <- as.data.frame(t(summarise_all(CL4,.funs = list(mean = ~mean(., na.rm = TRUE)))))
Spe_w$Phyto <- colnames(CL4)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index des espèces = noeuds
nodes_net <- V(cluster4)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)

vids <- Spe$Pindex
sub <- igraph::subgraph(cluster4, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
plot(sub)

vcount(sub) # nombre de noeuds
ecount(sub) # nombre de liens
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) #Nombre de liens moyen
assortativity_degree(sub,directed = F) #assortativite
mean(1 - E(sub)$weight) # Dissilarite as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # Connectivite naturel

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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster4_automne.csv", row.names = FALSE,dec = ".")




# Printemps
CL4 <- filter(data, cluster == 4,season == "Spring" )
#rownames(CL1) <- CL1$Date
CL4 <- dplyr::select(CL4,Actinoptychus:Coscinodiscophycidae)
Spe_w <- as.data.frame(t(summarise_all(CL4,.funs = list(mean = ~mean(., na.rm = TRUE)))))
Spe_w$Phyto <- colnames(CL4)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index des espèces = noeuds
nodes_net <- V(cluster4)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)

vids <- Spe$Pindex
sub <- igraph::subgraph(cluster4, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
plot(sub)

vcount(sub) # nombre de noeuds
ecount(sub) # nombre de liens
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) #Nombre de liens moyen
assortativity_degree(sub,directed = F) #assortativite
mean(1 - E(sub)$weight) # Dissilarite as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # Connectivite naturel

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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster4_printemps.csv", row.names = FALSE,dec = ".")




# Ete
CL4 <- filter(data, cluster == 4,season == "Summer" )
#rownames(CL1) <- CL1$Date
CL4 <- dplyr::select(CL4,Actinoptychus:Coscinodiscophycidae)
Spe_w <- as.data.frame(t(summarise_all(CL4,.funs = list(mean = ~mean(., na.rm = TRUE)))))
Spe_w$Phyto <- colnames(CL4)
Spe_w$V1 <- as.numeric(Spe_w$V1)
Spe_w <- Spe_w[complete.cases(Spe_w$V1),]
Spe_w <- filter(Spe_w,V1 > 0)
rownames(Spe_w) <- NULL
Spe_w$IndexTab <- rownames(Spe_w)

# Index des espèces = noeuds
nodes_net <- V(cluster4)
phyto_index <- as.data.frame(nodes_net)
phyto_index$phyto <- rownames(phyto_index)
colnames(phyto_index)[1] <- "Pindex"

colnames(Spe_w) <- c("Count","phyto","IndexTab")
Spe <- left_join(phyto_index,Spe_w, by = join_by(phyto))
Spe$Pindex <- as.numeric(Spe$Pindex)

vids <- Spe$Pindex
sub <- igraph::subgraph(cluster4, vids)
viz_sub <- hchart(sub, layout = layout_with_fr)
plot(sub)

vcount(sub) # nombre de noeuds
ecount(sub) # nombre de liens
ecount(sub) / vcount(sub) # linkage density or average number of links per nodes
edge_density(sub, loops = FALSE) #connectance

# Average path length
mean_distance(
  sub,
  directed = FALSE,
  unconnected = FALSE # if the graphs is disconnected, only the existing paths are considered
)
edge.connectivity(sub) # Edge connectivity = adhesion

wc <- cluster_fast_greedy(sub)
modularity(sub,membership = membership(wc)) # Modularity

vertex.connectivity(sub) # Vertex connectivity = adhesion
mean(degree(sub)) #Nombre de liens moyen
assortativity_degree(sub,directed = F) #assortativite
mean(1 - E(sub)$weight) # Dissilarite as defined in NetCoMi
transitivity(sub,type = "global") #Transitivity
mean(edge_betweenness(sub)) # Mean edge betweeness

adj <- as.matrix(as_adjacency_matrix(sub, attr = "weight",)) # OK
diag(adj) <- 1
natural.connectivity(as.matrix(adj)) # Connectivite naturel

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
write.csv2(Spe_hubs,file="data_modif/Hubs_cluster4_hiver.csv", row.names = FALSE,dec = ".")


#### Comparaison des réseaux ####
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
write.csv2(metric,file="data_modif/metrics.csv", row.names = FALSE,dec = ".")


cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF")


ggplot(metric)+
  geom_boxplot(aes(x=cluster,y=N_noeuds,group=cluster,fill=as.character(cluster)))+
  scale_fill_manual(values=cluster_col,guide="none")
kruskal.test(metric$N_noeuds~metric$cluster)
DunnTest(metric$N_noeuds~metric$cluster,method="BH")

ggplot(metric)+
  geom_boxplot(aes(x=cluster,y=N_liens,group=cluster,fill=as.character(cluster)))+
  scale_fill_manual(values=cluster_col,guide="none")
kruskal.test(metric$N_liens~metric$cluster)
DunnTest(metric$N_liens~metric$cluster,method="BH")

ggplot(metric)+
  geom_boxplot(aes(x=cluster,y=D_liens,group=cluster,fill=as.character(cluster)))+
  scale_fill_manual(values=cluster_col,guide="none")
kruskal.test(metric$D_liens~metric$cluster)
DunnTest(metric$D_liens~metric$cluster,method="BH")

ggplot(metric)+
  geom_boxplot(aes(x=cluster,y=C_tance,group=cluster,fill=as.character(cluster)))+
  scale_fill_manual(values=cluster_col,guide="none")
kruskal.test(metric$C_tance~metric$cluster)
DunnTest(metric$C_tance~metric$cluster,method="BH")

ggplot(metric)+
  geom_boxplot(aes(x=cluster,y=Avg_p_length,group=cluster,fill=as.character(cluster)))+
  scale_fill_manual(values=cluster_col,guide="none")
kruskal.test(metric$Avg_p_length~metric$cluster)
DunnTest(metric$Avg_p_length~metric$cluster,method="BH")

ggplot(metric)+
  geom_boxplot(aes(x=cluster,y=Adhes,group=cluster,fill=as.character(cluster)))+
  scale_fill_manual(values=cluster_col,guide="none")
kruskal.test(metric$Adhes~metric$cluster)
DunnTest(metric$Adhes~metric$cluster,method="BH")

ggplot(metric)+
  geom_boxplot(aes(x=cluster,y=Mod,group=cluster,fill=as.character(cluster)))+
  scale_fill_manual(values=cluster_col,guide="none")
kruskal.test(metric$Mod~metric$cluster)
DunnTest(metric$Mod~metric$cluster,method="BH")

ggplot(metric)+
  geom_boxplot(aes(x=cluster,y=meanN_liens,group=cluster,fill=as.character(cluster)))+
  scale_fill_manual(values=cluster_col,guide="none")
kruskal.test(metric$meanN_liens~metric$cluster)
DunnTest(metric$meanN_liens~metric$cluster,method="BH")

ggplot(metric)+
  geom_boxplot(aes(x=cluster,y=Assort,group=cluster,fill=as.character(cluster)))+
  scale_fill_manual(values=cluster_col,guide="none")
kruskal.test(metric$Assort~metric$cluster)
DunnTest(metric$Assort~metric$cluster,method="BH")

ggplot(metric)+
  geom_boxplot(aes(x=cluster,y=Diss,group=cluster,fill=as.character(cluster)))+
  scale_fill_manual(values=cluster_col,guide="none")
kruskal.test(metric$Diss~metric$cluster)
DunnTest(metric$Diss~metric$cluster,method="BH")

ggplot(metric)+
  geom_boxplot(aes(x=cluster,y=Trans,group=cluster,fill=as.character(cluster)))+
  scale_fill_manual(values=cluster_col,guide="none")
kruskal.test(metric$Trans~metric$cluster)
DunnTest(metric$Trans~metric$cluster,method="BH")

ggplot(metric)+
  geom_boxplot(aes(x=cluster,y=meanN_voisins,group=cluster,fill=as.character(cluster)))+
  scale_fill_manual(values=cluster_col,guide="none")
kruskal.test(metric$meanN_voisins~metric$cluster)
DunnTest(metric$meanN_voisins~metric$cluster,method="BH")

ggplot(metric)+
  geom_boxplot(aes(x=cluster,y=Nat_connect,group=cluster,fill=as.character(cluster)))+
  scale_fill_manual(values=cluster_col,guide="none")
kruskal.test(metric$Nat_connect~metric$cluster)
DunnTest(metric$Nat_connect~metric$cluster,method="BH")

ggplot(metric)+
  geom_boxplot(aes(x=cluster,y=N_clust,group=cluster,fill=as.character(cluster)))+
  scale_fill_manual(values=cluster_col,guide="none")
kruskal.test(metric$N_clust~metric$cluster)
DunnTest(metric$N_clust~metric$cluster,method="BH")

datal <- pivot_longer(metric,names_to = "Var",cols = N_noeuds:N_clust)
ggplot(datal)+
  geom_boxplot(aes(x=cluster,y=value,group=cluster,fill=as.character(cluster)),size = 1)+
  scale_fill_manual(values=cluster_col,guide = "none")+
  facet_wrap(~Var,scales = "free_y")+
  labs(title = "Metriques (+)")

datal2 <- filter(datal, Var %in% c("Assort","Avg_p_length","C_tance","D_liens","Mod","Nat_connect","N_noeuds","Adhes"))
ggplot(datal2)+
  geom_boxplot(aes(x=cluster,y=value,group=cluster,fill=as.character(cluster)),size = 1)+
  scale_fill_manual(values=cluster_col,guide = "none")+
  facet_wrap(~Var,scales = "free_y",ncol=4)+
  labs(title = "Metriques (+)")

# Choix des métriques #
metric_PCA <- dplyr::select(metric,-Code_point_Libelle,-Date,-cluster)
metric_PCA[metric_PCA == Inf] <- NA

PCA_metric <- PCA(metric_PCA,scale.unit = T)
fviz_eig(PCA_metric, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_var(PCA_metric, col.var = "black")

fviz_cos2(PCA_metric, choice = "var", axes = 1:2)
fviz_contrib(PCA_metric, choice = "var", axes = 1:2, top = 10)

metric_PCA <- dplyr::select(metric1,-Code_point_Libelle,-Date,-cluster)
metric_PCA[metric_PCA == Inf] <- NA

PCA_metric <- PCA(metric_PCA,scale.unit = T)
fviz_eig(PCA_metric, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_var(PCA_metric, col.var = "black",title="Cluster1")

metric_PCA <- dplyr::select(metric2,-Code_point_Libelle,-Date,-cluster)
metric_PCA[metric_PCA == Inf] <- NA

PCA_metric <- PCA(metric_PCA,scale.unit = T)
fviz_eig(PCA_metric, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_var(PCA_metric, col.var = "black",title="Cluster2")

metric_PCA <- dplyr::select(metric3,-Code_point_Libelle,-Date,-cluster)
metric_PCA[metric_PCA == Inf] <- NA

PCA_metric <- PCA(metric_PCA,scale.unit = T)
fviz_eig(PCA_metric, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_var(PCA_metric, col.var = "black",title="Cluster3")

metric_PCA <- dplyr::select(metric4,-Code_point_Libelle,-Date,-cluster)
metric_PCA[metric_PCA == Inf] <- NA

PCA_metric <- PCA(metric_PCA,scale.unit = T)
fviz_eig(PCA_metric, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_var(PCA_metric, col.var = "black",title="Cluster4")


# Comparaison type de bloom #####
metric <- read_delim("data_modif/metrics.csv", 
                      delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                      locale = locale(decimal_mark = ",", grouping_mark = "."), 
                      trim_ws = TRUE)
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

data <- dplyr::select(data, Code_point_Libelle, Date, cluster, Bloom_Phylum,ID.interne.passage)

data_met <- left_join(metric,data)

doublons <- data_met[duplicated(data_met$ID.interne.passage) |
                        duplicated(data_met$ID.interne.passage, fromLast = TRUE), ]

# Filtre des doublons hydro :
resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

# On supprime les lignes en doublon dans le jeu de données initial
data_unique <- subset(data_met, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
# On les remets ces doublons filtres
data_join <- bind_rows(data_unique,resultat_filtre)
# On remet au propre
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

data_met <- data_join

data <- filter(data_met, Bloom_Phylum == "Bac" | Bloom_Phylum == "Dino" | is.na(Bloom_Phylum))
data[is.na(data$Bloom_Phylum),]$Bloom_Phylum <- "Non"

datal <- pivot_longer(data,cols = N_noeuds:N_clust,names_to = "var")

ggplot(datal)+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum))+
  facet_wrap(~var, scales = "free_y",ncol = 5)

kruskal.test(data$N_noeuds~data$Bloom_Phylum)
DunnTest(data$N_noeuds~data$Bloom_Phylum,method = "BH")

kruskal.test(data$Adhes~data$Bloom_Phylum)
DunnTest(data$Adhes~data$Bloom_Phylum,method = "BH")

kruskal.test(data$Assort~data$Bloom_Phylum)
DunnTest(data$Assort~data$Bloom_Phylum,method = "BH")

kruskal.test(data$Avg_p_length~data$Bloom_Phylum)
DunnTest(data$Avg_p_length~data$Bloom_Phylum,method = "BH")

kruskal.test(data$C_tance~data$Bloom_Phylum)
DunnTest(data$C_tance~data$Bloom_Phylum,method = "BH")

kruskal.test(data$D_liens~data$Bloom_Phylum)
DunnTest(data$D_liens~data$Bloom_Phylum,method = "BH")

kruskal.test(data$Diss~data$Bloom_Phylum)
DunnTest(data$Diss~data$Bloom_Phylum,method = "BH")

kruskal.test(data$meanN_liens~data$Bloom_Phylum)
DunnTest(data$meanN_liens~data$Bloom_Phylum,method = "BH")

kruskal.test(data$meanN_voisins~data$Bloom_Phylum)
DunnTest(data$meanN_voisins~data$Bloom_Phylum,method = "BH")

kruskal.test(data$Mod~data$Bloom_Phylum)
DunnTest(data$Mod~data$Bloom_Phylum,method = "BH")

kruskal.test(data$N_clust~data$Bloom_Phylum)
DunnTest(data$N_clust~data$Bloom_Phylum,method = "BH")

kruskal.test(data$N_liens~data$Bloom_Phylum)
DunnTest(data$N_liens~data$Bloom_Phylum,method = "BH")

kruskal.test(data$Nat_connect~data$Bloom_Phylum)
DunnTest(data$Nat_connect~data$Bloom_Phylum,method = "BH")

kruskal.test(data$Trans~data$Bloom_Phylum)
DunnTest(data$Trans~data$Bloom_Phylum,method = "BH")

ggplot(filter(datal,cluster == 1))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#F8766D")+
  facet_wrap(~var, scales = "free_y",ncol = 5)

data1 <- filter(data,cluster == 1)
kruskal.test(data1$N_noeuds~data1$Bloom_Phylum)
DunnTest(data1$N_noeuds~data1$Bloom_Phylum,method = "BH")

kruskal.test(data1$Adhes~data1$Bloom_Phylum)
DunnTest(data1$Adhes~data1$Bloom_Phylum,method = "BH")

kruskal.test(data1$Assort~data1$Bloom_Phylum)
DunnTest(data1$Assort~data1$Bloom_Phylum,method = "BH")

kruskal.test(data1$Avg_p_length~data1$Bloom_Phylum)
DunnTest(data1$Avg_p_length~data1$Bloom_Phylum,method = "BH")

kruskal.test(data1$C_tance~data1$Bloom_Phylum)
DunnTest(data1$C_tance~data1$Bloom_Phylum,method = "BH")

kruskal.test(data1$D_liens~data1$Bloom_Phylum)
DunnTest(data1$D_liens~data1$Bloom_Phylum,method = "BH")

kruskal.test(data1$Diss~data1$Bloom_Phylum)
DunnTest(data1$Diss~data1$Bloom_Phylum,method = "BH")

kruskal.test(data1$meanN_liens~data1$Bloom_Phylum)
DunnTest(data1$meanN_liens~data1$Bloom_Phylum,method = "BH")

kruskal.test(data1$meanN_voisins~data1$Bloom_Phylum)
DunnTest(data1$meanN_voisins~data1$Bloom_Phylum,method = "BH")

kruskal.test(data1$Mod~data1$Bloom_Phylum)
DunnTest(data1$Mod~data1$Bloom_Phylum,method = "BH")

kruskal.test(data1$N_clust~data1$Bloom_Phylum)
DunnTest(data1$N_clust~data1$Bloom_Phylum,method = "BH")

kruskal.test(data1$N_liens~data1$Bloom_Phylum)
DunnTest(data1$N_liens~data1$Bloom_Phylum,method = "BH")

kruskal.test(data1$Nat_connect~data1$Bloom_Phylum)
DunnTest(data1$Nat_connect~data1$Bloom_Phylum,method = "BH")

kruskal.test(data1$Trans~data1$Bloom_Phylum)
DunnTest(data1$Trans~data1$Bloom_Phylum,method = "BH")


ggplot(filter(datal,cluster == 2))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#CD9600")+
  facet_wrap(~var, scales = "free_y",ncol = 5)

data2 <- filter(data,cluster == 2)
kruskal.test(data2$N_noeuds~data2$Bloom_Phylum)
DunnTest(data2$N_noeuds~data2$Bloom_Phylum,method = "BH")

kruskal.test(data2$Adhes~data2$Bloom_Phylum)
DunnTest(data2$Adhes~data2$Bloom_Phylum,method = "BH")

kruskal.test(data2$Assort~data2$Bloom_Phylum)
DunnTest(data2$Assort~data2$Bloom_Phylum,method = "BH")

kruskal.test(data2$Avg_p_length~data2$Bloom_Phylum)
DunnTest(data2$Avg_p_length~data2$Bloom_Phylum,method = "BH")

kruskal.test(data2$C_tance~data2$Bloom_Phylum)
DunnTest(data2$C_tance~data2$Bloom_Phylum,method = "BH")

kruskal.test(data2$D_liens~data2$Bloom_Phylum)
DunnTest(data2$D_liens~data2$Bloom_Phylum,method = "BH")

kruskal.test(data2$Diss~data2$Bloom_Phylum)
DunnTest(data2$Diss~data2$Bloom_Phylum,method = "BH")

kruskal.test(data2$meanN_liens~data2$Bloom_Phylum)
DunnTest(data2$meanN_liens~data2$Bloom_Phylum,method = "BH")

kruskal.test(data2$meanN_voisins~data2$Bloom_Phylum)
DunnTest(data2$meanN_voisins~data2$Bloom_Phylum,method = "BH")

kruskal.test(data2$Mod~data2$Bloom_Phylum)
DunnTest(data2$Mod~data2$Bloom_Phylum,method = "BH")

kruskal.test(data2$N_clust~data2$Bloom_Phylum)
DunnTest(data2$N_clust~data2$Bloom_Phylum,method = "BH")

kruskal.test(data2$N_liens~data2$Bloom_Phylum)
DunnTest(data2$N_liens~data2$Bloom_Phylum,method = "BH")

kruskal.test(data2$Nat_connect~data2$Bloom_Phylum)
DunnTest(data2$Nat_connect~data2$Bloom_Phylum,method = "BH")

kruskal.test(data2$Trans~data2$Bloom_Phylum)
DunnTest(data2$Trans~data2$Bloom_Phylum,method = "BH")


ggplot(filter(datal,cluster == 3))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#00BE67")+
  facet_wrap(~var, scales = "free_y",ncol = 5)

data3 <- filter(data,cluster == 3)
kruskal.test(data3$N_noeuds~data3$Bloom_Phylum)
DunnTest(data3$N_noeuds~data3$Bloom_Phylum,method = "BH")

kruskal.test(data3$Adhes~data3$Bloom_Phylum)
DunnTest(data3$Adhes~data3$Bloom_Phylum,method = "BH")

kruskal.test(data3$Assort~data3$Bloom_Phylum)
DunnTest(data3$Assort~data3$Bloom_Phylum,method = "BH")

kruskal.test(data3$Avg_p_length~data3$Bloom_Phylum)
DunnTest(data3$Avg_p_length~data3$Bloom_Phylum,method = "BH")

kruskal.test(data3$C_tance~data3$Bloom_Phylum)
DunnTest(data3$C_tance~data3$Bloom_Phylum,method = "BH")

kruskal.test(data3$D_liens~data3$Bloom_Phylum)
DunnTest(data3$D_liens~data3$Bloom_Phylum,method = "BH")

kruskal.test(data3$Diss~data3$Bloom_Phylum)
DunnTest(data3$Diss~data3$Bloom_Phylum,method = "BH")

kruskal.test(data3$meanN_liens~data3$Bloom_Phylum)
DunnTest(data3$meanN_liens~data3$Bloom_Phylum,method = "BH")

kruskal.test(data3$meanN_voisins~data3$Bloom_Phylum)
DunnTest(data3$meanN_voisins~data3$Bloom_Phylum,method = "BH")

kruskal.test(data3$Mod~data3$Bloom_Phylum)
DunnTest(data3$Mod~data3$Bloom_Phylum,method = "BH")

kruskal.test(data3$N_clust~data3$Bloom_Phylum)
DunnTest(data3$N_clust~data3$Bloom_Phylum,method = "BH")

kruskal.test(data3$N_liens~data3$Bloom_Phylum)
DunnTest(data3$N_liens~data3$Bloom_Phylum,method = "BH")

kruskal.test(data3$Nat_connect~data3$Bloom_Phylum)
DunnTest(data3$Nat_connect~data3$Bloom_Phylum,method = "BH")

kruskal.test(data3$Trans~data3$Bloom_Phylum)
DunnTest(data3$Trans~data3$Bloom_Phylum,method = "BH")

ggplot(filter(datal,cluster == 4))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#00A9FF")+
  facet_wrap(~var, scales = "free_y",ncol = 5)

data4 <- filter(data,cluster == 4)
kruskal.test(data4$N_noeuds~data4$Bloom_Phylum)
DunnTest(data4$N_noeuds~data4$Bloom_Phylum,method = "BH")

kruskal.test(data4$Adhes~data4$Bloom_Phylum)
DunnTest(data4$Adhes~data4$Bloom_Phylum,method = "BH")

kruskal.test(data4$Assort~data4$Bloom_Phylum)
DunnTest(data4$Assort~data4$Bloom_Phylum,method = "BH")

kruskal.test(data4$Avg_p_length~data4$Bloom_Phylum)
DunnTest(data4$Avg_p_length~data4$Bloom_Phylum,method = "BH")

kruskal.test(data4$C_tance~data4$Bloom_Phylum)
DunnTest(data4$C_tance~data4$Bloom_Phylum,method = "BH")

kruskal.test(data4$D_liens~data4$Bloom_Phylum)
DunnTest(data4$D_liens~data4$Bloom_Phylum,method = "BH")

kruskal.test(data4$Diss~data4$Bloom_Phylum)
DunnTest(data4$Diss~data4$Bloom_Phylum,method = "BH")

kruskal.test(data4$meanN_liens~data4$Bloom_Phylum)
DunnTest(data4$meanN_liens~data4$Bloom_Phylum,method = "BH")

kruskal.test(data4$meanN_voisins~data4$Bloom_Phylum)
DunnTest(data4$meanN_voisins~data4$Bloom_Phylum,method = "BH")

kruskal.test(data4$Mod~data4$Bloom_Phylum)
DunnTest(data4$Mod~data4$Bloom_Phylum,method = "BH")

kruskal.test(data4$N_clust~data4$Bloom_Phylum)
DunnTest(data4$N_clust~data4$Bloom_Phylum,method = "BH")

kruskal.test(data4$N_liens~data4$Bloom_Phylum)
DunnTest(data4$N_liens~data4$Bloom_Phylum,method = "BH")

kruskal.test(data4$Nat_connect~data4$Bloom_Phylum)
DunnTest(data4$Nat_connect~data4$Bloom_Phylum,method = "BH")

kruskal.test(data4$Trans~data4$Bloom_Phylum)
DunnTest(data4$Trans~data4$Bloom_Phylum,method = "BH")

# Comparaison bloom non bloom
datal[datal$Bloom_Phylum == "Dino",]$Bloom_Phylum <- "Oui"
datal[datal$Bloom_Phylum == "Bac",]$Bloom_Phylum <- "Oui"

data[data$Bloom_Phylum == "Dino",]$Bloom_Phylum <- "Oui"
data[data$Bloom_Phylum == "Bac",]$Bloom_Phylum <- "Oui"

ggplot(datal)+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum))+
  facet_wrap(~var, scales = "free_y",ncol = 5)

wilcox.test(data$N_noeuds~data$Bloom_Phylum)
wilcox.test(data$Adhes~data$Bloom_Phylum)
wilcox.test(data$Assort~data$Bloom_Phylum)
wilcox.test(data$Avg_p_length~data$Bloom_Phylum)
wilcox.test(data$C_tance~data$Bloom_Phylum)
wilcox.test(data$D_liens~data$Bloom_Phylum)
wilcox.test(data$Diss~data$Bloom_Phylum)
wilcox.test(data$meanN_liens~data$Bloom_Phylum)
wilcox.test(data$meanN_voisins~data$Bloom_Phylum)
wilcox.test(data$Mod~data$Bloom_Phylum)
wilcox.test(data$N_clust~data$Bloom_Phylum)
wilcox.test(data$N_liens~data$Bloom_Phylum)
wilcox.test(data$N_noeuds~data$Bloom_Phylum)
wilcox.test(data$Nat_connect~data$Bloom_Phylum)
wilcox.test(data$Trans~data$Bloom_Phylum)


ggplot(filter(datal,cluster == 1))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#F8766D")+
  facet_wrap(~var, scales = "free_y",ncol = 5)

data1 <- filter(data,cluster == 1)
wilcox.test(data1$N_noeuds~data1$Bloom_Phylum)
wilcox.test(data1$Adhes~data1$Bloom_Phylum)
wilcox.test(data1$Assort~data1$Bloom_Phylum)
wilcox.test(data1$Avg_p_length~data1$Bloom_Phylum)
wilcox.test(data1$C_tance~data1$Bloom_Phylum)
wilcox.test(data1$D_liens~data1$Bloom_Phylum)
wilcox.test(data1$Diss~data1$Bloom_Phylum)
wilcox.test(data1$meanN_liens~data1$Bloom_Phylum)
wilcox.test(data1$meanN_voisins~data1$Bloom_Phylum)
wilcox.test(data1$Mod~data1$Bloom_Phylum)
wilcox.test(data1$N_clust~data1$Bloom_Phylum)
wilcox.test(data1$N_liens~data1$Bloom_Phylum)
wilcox.test(data1$N_noeuds~data1$Bloom_Phylum)
wilcox.test(data1$Nat_connect~data1$Bloom_Phylum)
wilcox.test(data1$Trans~data1$Bloom_Phylum)

ggplot(filter(datal,cluster == 2))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#CD9600")+
  facet_wrap(~var, scales = "free_y",ncol = 5)

data1 <- filter(data,cluster == 2)
wilcox.test(data1$N_noeuds~data1$Bloom_Phylum)
wilcox.test(data1$Adhes~data1$Bloom_Phylum)
wilcox.test(data1$Assort~data1$Bloom_Phylum)
wilcox.test(data1$Avg_p_length~data1$Bloom_Phylum)
wilcox.test(data1$C_tance~data1$Bloom_Phylum)
wilcox.test(data1$D_liens~data1$Bloom_Phylum)
wilcox.test(data1$Diss~data1$Bloom_Phylum)
wilcox.test(data1$meanN_liens~data1$Bloom_Phylum)
wilcox.test(data1$meanN_voisins~data1$Bloom_Phylum)
wilcox.test(data1$Mod~data1$Bloom_Phylum)
wilcox.test(data1$N_clust~data1$Bloom_Phylum)
wilcox.test(data1$N_liens~data1$Bloom_Phylum)
wilcox.test(data1$N_noeuds~data1$Bloom_Phylum)
wilcox.test(data1$Nat_connect~data1$Bloom_Phylum)
wilcox.test(data1$Trans~data1$Bloom_Phylum)

ggplot(filter(datal,cluster == 3))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#00BE67")+
  facet_wrap(~var, scales = "free_y",ncol = 5)

data1 <- filter(data,cluster == 3)
wilcox.test(data1$N_noeuds~data1$Bloom_Phylum)
wilcox.test(data1$Adhes~data1$Bloom_Phylum)
wilcox.test(data1$Assort~data1$Bloom_Phylum)
wilcox.test(data1$Avg_p_length~data1$Bloom_Phylum)
wilcox.test(data1$C_tance~data1$Bloom_Phylum)
wilcox.test(data1$D_liens~data1$Bloom_Phylum)
wilcox.test(data1$Diss~data1$Bloom_Phylum)
wilcox.test(data1$meanN_liens~data1$Bloom_Phylum)
wilcox.test(data1$meanN_voisins~data1$Bloom_Phylum)
wilcox.test(data1$Mod~data1$Bloom_Phylum)
wilcox.test(data1$N_clust~data1$Bloom_Phylum)
wilcox.test(data1$N_liens~data1$Bloom_Phylum)
wilcox.test(data1$N_noeuds~data1$Bloom_Phylum)
wilcox.test(data1$Nat_connect~data1$Bloom_Phylum)
wilcox.test(data1$Trans~data1$Bloom_Phylum)

ggplot(filter(datal,cluster == 4))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#00A9FF")+
  facet_wrap(~var, scales = "free_y",ncol = 5)
data1 <- filter(data,cluster == 4)
wilcox.test(data1$N_noeuds~data1$Bloom_Phylum)
wilcox.test(data1$Adhes~data1$Bloom_Phylum)
wilcox.test(data1$Assort~data1$Bloom_Phylum)
wilcox.test(data1$Avg_p_length~data1$Bloom_Phylum)
wilcox.test(data1$C_tance~data1$Bloom_Phylum)
wilcox.test(data1$D_liens~data1$Bloom_Phylum)
wilcox.test(data1$Diss~data1$Bloom_Phylum)
wilcox.test(data1$meanN_liens~data1$Bloom_Phylum)
wilcox.test(data1$meanN_voisins~data1$Bloom_Phylum)
wilcox.test(data1$Mod~data1$Bloom_Phylum)
wilcox.test(data1$N_clust~data1$Bloom_Phylum)
wilcox.test(data1$N_liens~data1$Bloom_Phylum)
wilcox.test(data1$N_noeuds~data1$Bloom_Phylum)
wilcox.test(data1$Nat_connect~data1$Bloom_Phylum)
wilcox.test(data1$Trans~data1$Bloom_Phylum)


# Comparaison avant / pendant #####

beta <- read_delim("data_modif/data_div_beta_ok_N-1.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)

comp_metric <- left_join(data,beta)
comp_metric$Nrow <- rownames(comp_metric)
pendant <- comp_metric[comp_metric$EpBloom == "OUI",]
avant <- comp_metric[as.numeric(pendant$Nrow)-1,]
pendant$Nrow <- NULL
avant$Nrow <- NULL
pendant$cat <- "Pendant"
avant$cat <- "Avant"
comp_metric_ok <- bind_rows(pendant,avant)

beta <- read_delim("data_modif/data_div_beta_ok_N1.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)

comp_metric <- left_join(data,beta)
comp_metric$Nrow <- rownames(comp_metric)
pendant <- comp_metric[comp_metric$EpBloom == "OUI",]
apres <- comp_metric[as.numeric(pendant$Nrow)+1,]
pendant$Nrow <- NULL
apres$Nrow <- NULL
pendant$cat <- "Pendant"
apres$cat <- "Apres"
comp_metric_ok <- bind_rows(comp_metric_ok,apres)

pendant.succession <- comp_metric[comp_metric$EpBloom == "Sucession",]
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



ggplot(filter(datal,cluster == 4))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00A9FF")+
  facet_wrap(~var,scales = "free_y")

comp_mt_cl <- filter(comp_metric_ok, cluster == 4)
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

# Avec indices de diversite
data_idiv <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)
data_idiv$Rspe <- data$Rspe
data_idiv <- dplyr::select(data_idiv,ID.interne.passage,Shannon, Pielou, BergerParker,Rspe)

comp_div_ok <- left_join(comp_metric_ok,data_idiv)
comp_div_ok <- dplyr::select(comp_div_ok,-(N_noeuds:N_clust))

comp_div_ok$cat <- as.factor(comp_div_ok$cat)
levels(comp_div_ok$cat)
comp_div_ok$cat <- fct_relevel(comp_div_ok$cat,c("Avant","Pendant","Apres"))
datal <- pivot_longer(comp_div_ok,cols = Shannon:Rspe,names_to = "var")

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

ggplot(filter(datal,cluster == 4))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00A9FF")+
  facet_wrap(~var,scales = "free_y")

comp_mt_cl <- filter(comp_div_ok, cluster == 4)
kruskal.test(comp_mt_cl$Rspe~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Rspe~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$BergerParker~comp_mt_cl$cat)
DunnTest(comp_mt_cl$BergerParker~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Pielou~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Pielou~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Shannon~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Shannon~comp_mt_cl$cat,method = "BH")



data_abio <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
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

ggplot(filter(datal,cluster == 4))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00A9FF")+
  facet_wrap(~var,scales = "free_y")

comp_mt_cl <- filter(comp_abio_ok, cluster == 4)
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







# Comparaison avant / pendant Dinophyceae #####
metric <- read_delim("data_modif/metrics.csv", 
                     delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                     locale = locale(decimal_mark = ",", grouping_mark = "."), 
                     trim_ws = TRUE)
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

data <- dplyr::select(data, Code_point_Libelle, Date, cluster, Bloom_Phylum,ID.interne.passage)

data_met <- left_join(metric,data)

doublons <- data_met[duplicated(data_met$ID.interne.passage) |
                       duplicated(data_met$ID.interne.passage, fromLast = TRUE), ]

# Filtre des doublons hydro :
resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

# On supprime les lignes en doublon dans le jeu de données initial
data_unique <- subset(data_met, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
# On les remets ces doublons filtres
data_join <- bind_rows(data_unique,resultat_filtre)
# On remet au propre
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

data_met <- data_join

data <- filter(data_met, Bloom_Phylum == "Bac" | Bloom_Phylum == "Dino" | is.na(Bloom_Phylum))
data[is.na(data$Bloom_Phylum),]$Bloom_Phylum <- "Non"

beta <- read_delim("data_modif/data_div_beta_ok_N-1.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)

comp_metric <- left_join(data,beta)
comp_metric$Nrow <- rownames(comp_metric)
pendant <- comp_metric[comp_metric$EpBloom == "OUI" & comp_metric$Bloom_Phylum == "Dino",]
avant <- comp_metric[as.numeric(pendant$Nrow)-1,]
pendant$Nrow <- NULL
avant$Nrow <- NULL
pendant$cat <- "Pendant"
avant$cat <- "Avant"
comp_metric_ok <- bind_rows(pendant,avant)

beta <- read_delim("data_modif/data_div_beta_ok_N1.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)

comp_metric <- left_join(data,beta)
comp_metric$Nrow <- rownames(comp_metric)
pendant <- comp_metric[comp_metric$EpBloom == "OUI" & comp_metric$Bloom_Phylum == "Dino",]
apres <- comp_metric[as.numeric(pendant$Nrow)+1,]
pendant$Nrow <- NULL
apres$Nrow <- NULL
pendant$cat <- "Pendant"
apres$cat <- "Apres"
comp_metric_ok <- bind_rows(comp_metric_ok,apres)

pendant.succession <- comp_metric[comp_metric$EpBloom == "Sucession" & comp_metric$Bloom_Phylum == "Dino",]
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



ggplot(filter(datal,cluster == 4))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00A9FF")+
  facet_wrap(~var,scales = "free_y")

comp_mt_cl <- filter(comp_metric_ok, cluster == 4)
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

# Avec indices de diversite
data_idiv <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
                        delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                            grouping_mark = ""), trim_ws = TRUE)
data_idiv$Rspe <- data$Rspe
data_idiv <- dplyr::select(data_idiv,ID.interne.passage,Shannon, Pielou, BergerParker,Rspe)

comp_div_ok <- left_join(comp_metric_ok,data_idiv)
comp_div_ok <- dplyr::select(comp_div_ok,-(N_noeuds:N_clust))

comp_div_ok$cat <- as.factor(comp_div_ok$cat)
levels(comp_div_ok$cat)
comp_div_ok$cat <- fct_relevel(comp_div_ok$cat,c("Avant","Pendant","Apres"))
datal <- pivot_longer(comp_div_ok,cols = Shannon:Rspe,names_to = "var")

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

ggplot(filter(datal,cluster == 4))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00A9FF")+
  facet_wrap(~var,scales = "free_y")

comp_mt_cl <- filter(comp_div_ok, cluster == 4)
kruskal.test(comp_mt_cl$Rspe~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Rspe~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$BergerParker~comp_mt_cl$cat)
DunnTest(comp_mt_cl$BergerParker~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Pielou~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Pielou~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Shannon~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Shannon~comp_mt_cl$cat,method = "BH")



data_abio <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
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

ggplot(filter(datal,cluster == 4))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00A9FF")+
  facet_wrap(~var,scales = "free_y")

comp_mt_cl <- filter(comp_abio_ok, cluster == 4)
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



# Comparaison avant / pendant Bacillariophyceae #####
metric <- read_delim("data_modif/metrics.csv", 
                     delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                     locale = locale(decimal_mark = ",", grouping_mark = "."), 
                     trim_ws = TRUE)
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

data <- dplyr::select(data, Code_point_Libelle, Date, cluster, Bloom_Phylum,ID.interne.passage)

data_met <- left_join(metric,data)

doublons <- data_met[duplicated(data_met$ID.interne.passage) |
                       duplicated(data_met$ID.interne.passage, fromLast = TRUE), ]

# Filtre des doublons hydro :
resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

# On supprime les lignes en doublon dans le jeu de données initial
data_unique <- subset(data_met, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
# On les remets ces doublons filtres
data_join <- bind_rows(data_unique,resultat_filtre)
# On remet au propre
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

data_met <- data_join

data <- filter(data_met, Bloom_Phylum == "Bac" | Bloom_Phylum == "Dino" | is.na(Bloom_Phylum))
data[is.na(data$Bloom_Phylum),]$Bloom_Phylum <- "Non"

beta <- read_delim("data_modif/data_div_beta_ok_N-1.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)

comp_metric <- left_join(data,beta)
comp_metric$Nrow <- rownames(comp_metric)
pendant <- comp_metric[comp_metric$EpBloom == "OUI" & comp_metric$Bloom_Phylum == "Bac",]
avant <- comp_metric[as.numeric(pendant$Nrow)-1,]
pendant$Nrow <- NULL
avant$Nrow <- NULL
pendant$cat <- "Pendant"
avant$cat <- "Avant"
comp_metric_ok <- bind_rows(pendant,avant)

beta <- read_delim("data_modif/data_div_beta_ok_N1.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                   locale = locale(decimal_mark = ",", grouping_mark = "."), 
                   trim_ws = TRUE)

comp_metric <- left_join(data,beta)
comp_metric$Nrow <- rownames(comp_metric)
pendant <- comp_metric[comp_metric$EpBloom == "OUI" & comp_metric$Bloom_Phylum == "Bac",]
apres <- comp_metric[as.numeric(pendant$Nrow)+1,]
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



ggplot(filter(datal,cluster == 4))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00A9FF")+
  facet_wrap(~var,scales = "free_y")

comp_mt_cl <- filter(comp_metric_ok, cluster == 4)
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

# Avec indices de diversite
data_idiv <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
                        delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                            grouping_mark = ""), trim_ws = TRUE)
data_idiv$Rspe <- data$Rspe
data_idiv <- dplyr::select(data_idiv,ID.interne.passage,Shannon, Pielou, BergerParker,Rspe)

comp_div_ok <- left_join(comp_metric_ok,data_idiv)
comp_div_ok <- dplyr::select(comp_div_ok,-(N_noeuds:N_clust))

comp_div_ok$cat <- as.factor(comp_div_ok$cat)
levels(comp_div_ok$cat)
comp_div_ok$cat <- fct_relevel(comp_div_ok$cat,c("Avant","Pendant","Apres"))
datal <- pivot_longer(comp_div_ok,cols = Shannon:Rspe,names_to = "var")

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

ggplot(filter(datal,cluster == 4))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00A9FF")+
  facet_wrap(~var,scales = "free_y")

comp_mt_cl <- filter(comp_div_ok, cluster == 4)
kruskal.test(comp_mt_cl$Rspe~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Rspe~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$BergerParker~comp_mt_cl$cat)
DunnTest(comp_mt_cl$BergerParker~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Pielou~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Pielou~comp_mt_cl$cat,method = "BH")

kruskal.test(comp_mt_cl$Shannon~comp_mt_cl$cat)
DunnTest(comp_mt_cl$Shannon~comp_mt_cl$cat,method = "BH")



data_abio <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
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

ggplot(filter(datal,cluster == 4))+
  geom_boxplot(aes(x=cat, y= value,group=cat),fill = "#00A9FF")+
  facet_wrap(~var,scales = "free_y")

comp_mt_cl <- filter(comp_abio_ok, cluster == 4)
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


ggplot(filter(datal,cat == "Pendant"))+
  geom_boxplot(aes(x=cluster, y= value,group=cluster))+
  facet_wrap(~var,scales = "free_y")


#### Correlation métriques avec indices de diversité
metric <- read_delim("data_modif/metrics.csv", 
                     delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                     locale = locale(decimal_mark = ",", grouping_mark = "."), 
                     trim_ws = TRUE)
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

data <- dplyr::select(data, Code_point_Libelle, Date, cluster, Bloom_Phylum,ID.interne.passage)

data_met <- left_join(metric,data)

doublons <- data_met[duplicated(data_met$ID.interne.passage) |
                       duplicated(data_met$ID.interne.passage, fromLast = TRUE), ]

# Filtre des doublons hydro :
resultat_filtre <- doublons %>%
  filter(duplicated(ID.interne.passage) | n()==1)

# On supprime les lignes en doublon dans le jeu de données initial
data_unique <- subset(data_met, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
# On les remets ces doublons filtres
data_join <- bind_rows(data_unique,resultat_filtre)
# On remet au propre
data_join <- data_join |>
  arrange(Code_point_Libelle, Date)

data_met <- data_join

data_met <- filter(data_met, Bloom_Phylum == "Bac" | Bloom_Phylum == "Dino" | is.na(Bloom_Phylum))
data_met[is.na(data_met$Bloom_Phylum),]$Bloom_Phylum <- "Non"

# Avec indices de diversite
data_idiv <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
                        delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                            grouping_mark = ""), trim_ws = TRUE)
HDGUBFHIA <- data$Rspe
data_idiv$Rspe <- HDGUBFHIA
data_idiv <- dplyr::select(data_idiv,ID.interne.passage,Shannon, Pielou, BergerParker,Rspe)

comp_div_ok <- left_join(data_met,data_idiv)

Table.corr_all <- dplyr::select(comp_div_ok,Shannon, Pielou, Rspe, BergerParker, Assort,Avg_p_length,C_tance,D_liens,
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


# RDA Var metriques en fonction de abio #####

metric <- read_delim("data_modif/metrics.csv", 
                     delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                     locale = locale(decimal_mark = ",", grouping_mark = "."), 
                     trim_ws = TRUE)
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

abio <- dplyr::select(data,CHLOROA:`TURB-FNU`)
metric <- dplyr::select(metric,N_noeuds:N_clust)
metric[metric == Inf] <- NA

ncomp <- estim_ncpPCA(abio)
res.imp <- imputePCA(abio, ncp = ncomp$ncp,method = "EM")
envi <- as.data.frame(res.imp$completeObs)

ncomp <- estim_ncpPCA(metric)
res.imp <- imputePCA(metric, ncp = ncomp$ncp,method = "EM")
metric <- as.data.frame(res.imp$completeObs)

metric_rda <- rda(metric ~., data=envi)
plot(metric_rda)

info <- data[, c("Code_point_Libelle","Date","cluster","Month","Year")]
observations_coords <- scores(metric_rda, display = "sites")

# Créez un dataframe pour stocker les coordonnées des observations
observations_df <- as.data.frame(observations_coords)
observations_df$cluster <- as.character(info$cluster)
observations_df$Month <- info$Month
observations_df$Year <- info$Year

cluster_pos <- summarise(group_by(observations_df, cluster), RDA1=mean(RDA1,na.rm=T),
                         RDA2=mean(RDA2,na.rm=T))

cluster_pos$cluster <- as.character(cluster_pos$cluster)

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF")
species_coords <- scores(metric_rda, display = "species")

# Créez un dataframe pour stocker les coordonnées des espèces
species_df <- as.data.frame(species_coords)
species_df$name <- rownames(species_df)

envcoord <- as.data.frame(metric_rda[["CCA"]][["biplot"]])
envcoord$Var <- rownames(envcoord)

ggplot(observations_df, aes(x = RDA1, y = RDA2)) +
  geom_point(aes(colour=cluster),size=5) +
  geom_text(aes(x=RDA1,y=RDA2,label=name),size=3,data=species_df)+
  labs(x = "Dimension 1 (99.85%)", y = "Dimension 2 (0.001%)",colour="Cluster") +
  geom_segment(aes(x = 0, y = 0, xend = RDA1, yend = RDA2), color = "black", size = 0.5,data=species_df,linetype="dashed")+
  geom_segment(aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(length = unit(0.08, "inches")), color = "red", size = 0.4,data=envcoord)+
  geom_text(aes(x=RDA1,y=RDA2,label=Var),size=3,data=envcoord,color = "red")+
  labs(x="CCA1 (99.85%)",y="CCA2 (0.001%)",colour="Cluster")+
  scale_colour_manual(values=cluster_col)+
  theme_minimal()


acp <- bind_cols(envi,metric)
PCA(acp,scale.unit = T)
