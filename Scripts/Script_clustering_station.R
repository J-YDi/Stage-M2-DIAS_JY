# Charger les packages
library(cluster)
library(ClustGeo)
library(sp)

# Pre-requis fonction hclustgeo modifie pour avoir acces a la matrice de distance compromis

######### FONCTION HCLUSTGEO MODIFIEE ##################
HCLUSTMODIF <- function (D0, D1 = NULL, alpha = 0, scale = TRUE, wt = NULL) 
{
  if (class(D0) != "dist") 
    stop("DO must be of class dist (use as.dist)", call. = FALSE)
  if (!is.null(D1) && (class(D1) != "dist")) 
    stop("D1 must be of class dist (use as.dist)", call. = FALSE)
  n <- as.integer(attr(D0, "Size"))
  if (is.null(n)) 
    stop("invalid dissimilarities", call. = FALSE)
  if (is.na(n) || n > 65536L) 
    stop("size cannot be NA nor exceed 65536", call. = FALSE)
  if (n < 2) 
    stop("must have n >= 2 objects to cluster", call. = FALSE)
  if (!is.null(D1) && length(D0) != length(D1)) 
    stop("the two dissimilarity structures must have the same size", 
         call. = FALSE)
  if ((max(alpha) > 1) || (max(alpha) < 0)) 
    stop("Values alpha must be in [0,1]", call. = FALSE)
  if ((scale == TRUE) && (!is.null(D1))) {
    D0 <- D0/max(D0)
    D1 <- D1/max(D1)
  }
  delta0 <- wardinit(D0, wt)
  if (!is.null(D1)) 
    delta1 <- wardinit(D1, wt)
  else delta1 <- 0
  delta <- (1 - alpha) * delta0 + alpha * delta1
  res <- delta
  return(res)
}



##### Sur la selection des stations de plus de 5 ans #####
# Chargement des donnees
data <- read_delim("data_modif/Table_FLORTOT_Surf_9523_Stselect_hydro_phyto_chloro_phylum_period5_chlafilter.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

# Selection des variables pour l'ACP
Table_PCA <- data[, c("SALI","TEMP","TURB","NH4","PO4","SIOH","OXYGENE","NO3+NO2","TURB-FNU")]

# Imputation des valeurs hydro manquantes
ncomp <- estim_ncpPCA(Table_PCA)
res.imp <- imputePCA(Table_PCA, ncp = ncomp$ncp,method = "EM")

# On fait l'ACP
PCA_result <- PCA(res.imp$completeObs, ncp=20)
# On recupere les coordonnees 
coord_PCA <- PCA_result$ind$coord

# Un oeil au screeplot, les deux premiers axes expliquent largement la variance
fviz_eig(PCA_result,addlabels = T)
# On cree un jeu de donnees avec les coordonnes dans les dimensions de l'ACP
Table_abio_PCA <- cbind(data$Code_point_Libelle,data$ID.interne.passage, coord_PCA)
Table_abio_PCA <- as.data.frame(Table_abio_PCA)
Table_abio_PCA$Dim.1 <- as.numeric(Table_abio_PCA$Dim.1)
Table_abio_PCA$Dim.2 <- as.numeric(Table_abio_PCA$Dim.2)
Table_abio_PCA$Dim.3 <- as.numeric(Table_abio_PCA$Dim.3)
Table_abio_PCA$Dim.4 <- as.numeric(Table_abio_PCA$Dim.4)
Table_abio_PCA$Dim.5 <- as.numeric(Table_abio_PCA$Dim.5)
Table_abio_PCA$Dim.6 <- as.numeric(Table_abio_PCA$Dim.6)
Table_abio_PCA$Dim.7 <- as.numeric(Table_abio_PCA$Dim.7)
Table_abio_PCA$Dim.8 <- as.numeric(Table_abio_PCA$Dim.8)
Table_abio_PCA$Dim.9 <- as.numeric(Table_abio_PCA$Dim.9)

colnames(Table_abio_PCA)[1:2] <- c("Code_point_Libelle","ID.interne.passage")

# On fait la mediane des positions de chaque station dans chaque dimension de l'ACP
Table_abio_st_med <- summarise(group_by(Table_abio_PCA, Code_point_Libelle), median_dim1=median(Dim.1,na.rm=T)
                               , median_dim2=median(Dim.2,na.rm=T),
                               median_dim3=median(Dim.3,na.rm=T),
                               median_dim4=median(Dim.4,na.rm=T),
                               median_dim5=median(Dim.5,na.rm=T),
                               median_dim6=median(Dim.6,na.rm=T),
                               median_dim7=median(Dim.7,na.rm=T),
                               median_dim8=median(Dim.8,na.rm=T),
                               median_dim9=median(Dim.9,na.rm=T))

# On cree un jeu de donnees qui contient les coordonnees geographiques des stations
data_geo <- summarise(group_by(data, Code_point_Libelle,lon,lat))
data_geo <- data_geo[-c(10,12:19,26:28,42:51,58:72),]
data_geo$lat <- as.numeric(data_geo$lat)
data_geo$lon <- as.numeric(data_geo$lon)


# On defini le nombre de clusters
K <- 5

# On lance clustgeo
# Calcule la matrice de distance sur l'hydro
D0 <- dist(Table_abio_st_med)
tree <- hclustgeo(D0)

# Converti les positions geographiques en objet spatial
firstPoints <- SpatialPoints(coords = cbind(data_geo$lat,data_geo$lon))
# Fais une distance spatial entre les objets
euclidDist <- sp::spDists(firstPoints,longlat = FALSE)

# Matrice de distance sur la géographie
D1 <- as.dist(euclidDist)

# Choix du parametre alpha
range.alpha <- seq(0,1,0.1)
cr <- choicealpha(D0, D1, range.alpha, 
                  K, graph = FALSE)
cr$Q # proportion of explained inertia
# Graphique diagnostique
plot(cr)
# On choisi alpha = 0.1
tree <- hclustgeo(D0,D1,alpha=0.1)
# Fonction HCLUSTMODIF pour extraire la matrice de distance compromis entre D0 et D1 (fin de script)
diss <- HCLUSTMODIF(D0,D1,alpha=0.1)
# Arbre de la matrice HYDROGEO
tree[["labels"]] <- Table_abio_st_med$Code_point_Libelle
plot(tree,hang = -1, 
     xlab = "", sub = "",
     main = "Ward dendrogram with alpha 0.1 and 5 clusters")
# On defini les clusters
cluster <- cutree(tree,K)
data_clust_result <- as.data.frame(cbind(data_geo,cluster))
colnames(data_clust_result)[4] <- "cluster"


# On evalue la qualite du clustering 
silhouette_score <- silhouette(as.numeric(data_clust_result$cluster), diss)
# Afficher le résultat
cat("Indice de Silhouette global 0.1 :", mean(silhouette_score[, "sil_width"]), "\n")

# On fait une carte pour montrer la position des clusters
Worldmap <- map_data('worldHires')
ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)',title="Carte des stations selon leur cluster",
       subtitle = "EM-0,1-k5",colour = "Cluster")+
  theme_gdocs()+
  geom_point(data = data_clust_result, aes(x = lon, y = lat,colour=as.character(cluster)), size =5)+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('maps_clust_EM_01_k5.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/clustering_spatial", dpi = 600, width = 400, height = 200, units = 'mm')

#On enregistre les clusters 
write.csv2(data_clust_result,file="data_modif/clusters_EM_01_k5.csv", row.names = FALSE,dec = ".")


#### Sur la sélection pour les stations à long terme avec periode commune #####
# Chargement des donnees
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

# Selection des variables pour l'ACP
Table_PCA <- data[, c("SALI","TEMP","TURB","NH4","PO4","SIOH","OXYGENE","NO3+NO2","TURB-FNU")]

# Imputation des valeurs manquantes
ncomp <- estim_ncpPCA(Table_PCA)
res.imp <- imputePCA(Table_PCA, ncp = ncomp$ncp,method = "EM")

# On fait l'ACP
PCA_result <- PCA(res.imp$completeObs, ncp=20)
# On recupere les coordonnees
coord_PCA <- PCA_result$ind$coord


# Un oeil sur le screeplot
fviz_eig(PCA_result,addlabels = T)
# On cree un jeu de donnees avec les coordonnes dans les dimensions de l'ACP
Table_abio_PCA <- cbind(data$Code_point_Libelle,data$ID.interne.passage, coord_PCA)
Table_abio_PCA <- as.data.frame(Table_abio_PCA)
Table_abio_PCA$Dim.1 <- as.numeric(Table_abio_PCA$Dim.1)
Table_abio_PCA$Dim.2 <- as.numeric(Table_abio_PCA$Dim.2)
Table_abio_PCA$Dim.3 <- as.numeric(Table_abio_PCA$Dim.3)
Table_abio_PCA$Dim.4 <- as.numeric(Table_abio_PCA$Dim.4)
Table_abio_PCA$Dim.5 <- as.numeric(Table_abio_PCA$Dim.5)
Table_abio_PCA$Dim.6 <- as.numeric(Table_abio_PCA$Dim.6)
Table_abio_PCA$Dim.7 <- as.numeric(Table_abio_PCA$Dim.7)
Table_abio_PCA$Dim.8 <- as.numeric(Table_abio_PCA$Dim.8)
Table_abio_PCA$Dim.9 <- as.numeric(Table_abio_PCA$Dim.9)

colnames(Table_abio_PCA)[1:2] <- c("Code_point_Libelle","ID.interne.passage")

# On fait la mediane des positions de chaque station dans chaque dimension de l'ACP
Table_abio_st_med <- summarise(group_by(Table_abio_PCA, Code_point_Libelle), median_dim1=median(Dim.1,na.rm=T)
                               , median_dim2=median(Dim.2,na.rm=T),
                               median_dim3=median(Dim.3,na.rm=T),
                               median_dim4=median(Dim.4,na.rm=T),
                               median_dim5=median(Dim.5,na.rm=T),
                               median_dim6=median(Dim.6,na.rm=T),
                               median_dim7=median(Dim.7,na.rm=T),
                               median_dim8=median(Dim.8,na.rm=T),
                               median_dim9=median(Dim.9,na.rm=T))

# On cree un jeu de donnees qui contient les coordonnees geographiques des stations
data_geo <- summarise(group_by(data, Code_point_Libelle,lon,lat))
data_geo <- data_geo[-c(8:15,19:21,28:42),]
data_geo$lat <- as.numeric(data_geo$lat)
data_geo$lon <- as.numeric(data_geo$lon)


# On selectionne le nombre de clusters
K <- 4

# On lance clustgeo
# Calcule la matrice de distance sur l'hydro
D0 <- dist(Table_abio_st_med)
tree <- hclustgeo(D0)

# Converti les positions geographiques en objet spatial
firstPoints <- SpatialPoints(coords = cbind(data_geo$lat,data_geo$lon))
# Fais une distance spatial entre les objets
euclidDist <- sp::spDists(firstPoints,longlat = FALSE)

# Matrice de distance sur la géographie
D1 <- as.dist(euclidDist)

# Choix du parametre alpha
range.alpha <- seq(0,1,0.1)
cr <- choicealpha(D0, D1, range.alpha, 
                  K, graph = FALSE)
cr$Q # proportion of explained inertia
# Graphique diagnostique
plot(cr)
# On choisi alpha = 0 car on ne prend pas en compte la geographie
tree <- hclustgeo(D0,D1,alpha=0)
# Fonction HCLUSTMODIF pour extraire la matrice de distance compromis entre D0 et D1 (fin de script)
diss <- HCLUSTMODIF(D0,D1,alpha=0)
# Arbre de la matrice HYDROGEO
tree[["labels"]] <- Table_abio_st_med$Code_point_Libelle
plot(tree,hang = -1, 
     xlab = "", sub = "",
     main = "Ward dendrogram with alpha 0 and 4 clusters")
# On defini les clusters
cluster <- cutree(tree,K)
data_clust_result <- as.data.frame(cbind(data_geo,cluster))
colnames(data_clust_result)[4] <- "cluster"


# On evalue la qualite du clustering 
silhouette_score <- silhouette(as.numeric(data_clust_result$cluster), diss)
# Afficher le résultat
cat("Indice de Silhouette global 0.1 :", mean(silhouette_score[, "sil_width"]), "\n")

# On fait une carte pour montrer la position des clusters
Worldmap <- map_data('worldHires')
ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)',title="Carte des stations selon leur cluster",
       subtitle = "EM-0-k4",colour = "Cluster")+
  theme_gdocs()+
  geom_point(data = data_clust_result, aes(x = lon, y = lat,colour=as.character(cluster)), size =5)+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('maps_clust_EM_0_k4_longterme.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/clustering_spatial", dpi = 600, width = 400, height = 200, units = 'mm')

#On enregistre les clusters 
write.csv2(data_clust_result,file="data_modif/clusters_EM_0_k4_longterme.csv", row.names = FALSE,dec = ".")




