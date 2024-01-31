# Charger la bibliothèque
library(cluster)

# Générer des données fictives pour l'exemple
set.seed(123)
data <- data.frame(
  Lieu = rep(c("A", "B", "C"), each = 10),
  Latitude = rep(c(40.1, 41.5, 42.0), each = 10),
  Longitude = rep(c(-79.0, -79.5, -78.8), each = 10),
  Temperature = rnorm(30, 15, 5),
  Salinity = rnorm(30, 35, 2),
  NO3 = rnorm(30, 10, 3),
  PO4 = rnorm(30, 2, 0.5)
)

# Sélectionner les variables pour la classification spatiale
spatial_data <- data[, c("Latitude", "Longitude", "Temperature", "Salinity", "NO3", "PO4")]

# Normaliser les données pour éviter les biais d'échelle
spatial_data_scaled <- scale(spatial_data)

# Définir le nombre de clusters souhaité
k <- 3

# Appliquer l'algorithme K-means pour chaque lieu
clusters_by_location <- lapply(split(data, data$Lieu), function(x) kmeans(x[, -c(1, 2)], centers = k, nstart = 20))

# Ajouter l'information de cluster à votre jeu de données
data$Cluster <- unlist(lapply(clusters_by_location, function(x) x$cluster))

# Visualiser les résultats
plot(data$Longitude, data$Latitude, col = data$Cluster, pch = 16, main = "K-means Clustering by Location")
legend("topright", legend = 1:k, col = 1:k, pch = 16, title = "Cluster")





remotes::install_github("mpadge/spatialcluster")

set.seed (1)
n <- 100
xy <- matrix (runif (2 * n), ncol = 2)
dmat <- matrix (runif (n ^ 2), ncol = n)
library (spatialcluster)
scl <- scl_full (xy, dmat, ncl = 8)
plot (scl)












library(sp)
# Generate 100 random X and Y coordinates 
# with longitude and latitude in the familiar
# degrees

x_coords <- runif(n = 100, min = -100, max = -80)
y_coords <- runif(n = 100, min = 25, max = 45)



# Have a look at the first coordinates
head(cbind(x_coords,y_coords))
firstPoints <- SpatialPoints(coords = cbind(data$Latitude,data$Longitude))
euclidDist <- sp::spDists(firstPoints,longlat = FALSE)

data_abio <- data[,4:7]

distabio <- dist(data_abio)

View(distabio)



devtools::install_github("chavent/ClustGeo")

library(ClustGeo)
data(estuary)
dat <- estuary$dat
head(dat)

D.geo <- estuary$D.geo
map <- estuary$map
head(map@data[,4:8]) 


D0 <- dist(dat) # the socio-economic distances
tree <- hclustgeo(D0)
plot(tree,hang = -1, label = FALSE, 
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree ,k = 5, border = c(4,5,3,2,1))
legend("topright", legend = paste("cluster",1:5), 
       fill=1:5,bty= "n", border = "white")

D1 <- as.dist(D.geo) # the geographic distances between the municipalities


range.alpha <- seq(0,1,0.1)
K <- 5
cr <- choicealpha(D0, D1, range.alpha, 
                  K, graph = FALSE)
cr$Q # proportion of explained inertia
plot(cr)
tree <- hclustgeo(D0,D1,alpha=0.2)
P5bis <- cutree(tree,5)

sp::plot(map, border = "grey", col = P5bis, 
         main = "Partition P5bis obtained with alpha=0.2 
         and geographical distances")
legend("topleft", legend=paste("cluster",1:5), 
       fill=1:5, bty="n",border="white")








# Test de ClustGeo sur les donnees REPHY
# fonctionne seul probleme est la maniere de faire la matrice de distance par station car
# on prend la moyenne par mesure

Table <- read_delim("data_modif/Table_FLORTOT_S_HYDRO_PHYTO.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                        grouping_mark = ""), trim_ws = TRUE)

Table <- summarise(group_by(Table, Code_point_Libelle,lat,lon), mean_temp=mean(TEMP,na.rm=T), mean_sal=mean(SALI,na.rm=T))

Table_abio <- Table[,c("mean_temp", "mean_sal")]
D0 <- dist(Table_abio) # distance sur l'abio
tree <- hclustgeo(D0)
plot(tree,hang = -1, label = F, 
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree ,k = 7, border = c(4,5,3,2,1))
legend("topright", legend = paste("cluster",1:5), 
       fill=1:5,bty= "n", border = "white")
library(sp)
firstPoints <- SpatialPoints(coords = cbind(Table$lat,Table$lon))
euclidDist <- sp::spDists(firstPoints,longlat = FALSE)

D1 <- as.dist(euclidDist)
range.alpha <- seq(0,1,0.1)
K <- 7
cr <- choicealpha(D0, D1, range.alpha, 
                  K, graph = FALSE)
cr$Q # proportion of explained inertia
plot(cr)
tree <- hclustgeo(D0,D1,alpha=0.5)
P5bis <- cutree(tree,7)
Table_clust <- cbind(Table,P5bis)

Worldmap <- map_data('worldHires')

colnames(Table_clust) <- c(colnames(Table_clust)[1:5],"Cluster")

ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)')+
  theme_gdocs()+
  geom_point(data = Table_clust, aes(x = lon, y = lat,colour=as.character(Cluster), size =2))+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))

# Test ACP comme point des stations
Table <- read_delim("data_modif/Table_FLORTOT_S_HYDRO_PHYTO.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                        grouping_mark = ""), trim_ws = TRUE)

Table_test <- dplyr::filter(Table, 
                     Code_point_Libelle != "Bif" &
                       Code_point_Libelle != "Bouzigues (a)"&
                       Code_point_Libelle != "Diana centre"&
                       Code_point_Libelle != "Etang d'Urbino centre"&
                       Code_point_Libelle != "La Palmyre"&
                       Code_point_Libelle != "Marseillan (a)"&
                       Code_point_Libelle != "Parc Leucate 2")

# Ou pas Table <- Table_test 

Table_PCA <- Table[, c("SALI","TEMP","TURB","NH4","PO4","SIOH","OXYGENE","NO3+NO2","TURB-FNU")]
PCA_result <- PCA(Table_PCA,scale.unit = T,graph=T,ncp = 20)
coord_PCA <- PCA_result$ind$coord

fviz_eig(PCA_result,addlabels = T)
Table_abio_PCA <- cbind(Table$Code_point_Libelle,Table$lon,Table$lat, coord_PCA)
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

colnames(Table_abio_PCA)[1:3] <- c("Code_point_Libelle","lon","lat")

Table_abio_st <- summarise(group_by(Table_abio_PCA, Code_point_Libelle,lon,lat), mean_dim1=mean(Dim.1,na.rm=T)
                   , mean_dim2=mean(Dim.2,na.rm=T),
                   mean_dim3=mean(Dim.3,na.rm=T),
                   mean_dim4=mean(Dim.4,na.rm=T),
                   mean_dim5=mean(Dim.5,na.rm=T),
                   mean_dim6=mean(Dim.6,na.rm=T),
                   mean_dim7=mean(Dim.7,na.rm=T),
                   mean_dim8=mean(Dim.8,na.rm=T),
                   mean_dim9=mean(Dim.9,na.rm=T))

Table_abio_st <- summarise(group_by(Table_abio_PCA, Code_point_Libelle,lon,lat), median_dim1=median(Dim.1,na.rm=T)
                           , median_dim2=median(Dim.2,na.rm=T),
                           median_dim3=median(Dim.3,na.rm=T),
                           median_dim4=median(Dim.4,na.rm=T),
                           median_dim5=median(Dim.5,na.rm=T),
                           median_dim6=median(Dim.6,na.rm=T),
                           median_dim7=median(Dim.7,na.rm=T),
                           median_dim8=median(Dim.8,na.rm=T),
                           median_dim9=median(Dim.9,na.rm=T))


D0 <- dist(Table_abio_st[4:12]) # distance sur l'abio
tree <- hclustgeo(D0)
plot(tree,hang = -1, label = F, 
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree ,k = 3, border = c(4,5,3,2,1))



firstPoints <- SpatialPoints(coords = cbind(as.numeric(Table_abio_st$lat),as.numeric(Table_abio_st$lon)))
euclidDist <- sp::spDists(firstPoints,longlat = FALSE)

D1 <- as.dist(euclidDist)
range.alpha <- seq(0,1,0.1)
K <- 3
cr <- choicealpha(D0, D1, range.alpha, 
                  K, graph = FALSE)
cr$Q # proportion of explained inertia
plot(cr)

cr$Qnorm
plot(cr,norm =TRUE)

tree <- hclustgeo(D0,D1,alpha=0.1)
P5bis <- cutree(tree,3)
Table_clust <- cbind(Table_abio_st,P5bis)


Worldmap <- map_data('worldHires')

colnames(Table_clust) <- c(colnames(Table_clust)[1:12],"Cluster")

ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)')+
  theme_gdocs()+
  geom_point(data = Table_clust, aes(x = as.numeric(lon), y = as.numeric(lat),colour=as.character(Cluster),size=2))+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))


# test de ne pas se baser sur l'ACP mais la RDA : marche pas
Table_phyto <- dplyr::select(Table, `Asterionellopsis glacialis`:`Oxytoxum caudatum`)
Table_hydro <- dplyr::select(Table,"SALI","TEMP","TURB","NH4","PO4","SIOH","OXYGENE","NO3+NO2","TURB-FNU")
Table_station <- dplyr::select(Table,Code.Region:Code.parametre)

Table_phyto <- as.data.frame(lapply(Table_phyto, as.numeric), stringsAsFactors = FALSE)
Table_phyto[is.na(Table_phyto)] <- 0
envi_pca <- PCA(Table_hydro, scale=TRUE)
phyto_pca <- PCA(Table_phyto, scale=TRUE)

phyto_hel <- decostand(Table_phyto, method="hellinger")
phyto_rda <- rda(phyto_hel ~ ., data=as.data.frame(envi_pca$ind$coord))
 
summary(phyto_rda)
RsquareAdj(phyto_rda)


# Analyse de silhouette pour determiner le nombre de cluster
matrice_distance <- D1 #ou D0
for (k in 2:10) {
  kmeans_result <- kmeans(matrice_distance, centers = k)
  cluster_assignments <- kmeans_result$cluster
  t <- as.data.frame(silhouette(cluster_assignments, dist(matrice_distance)))
  silhouette_vals[k] <- mean(t$sil_width)
}
# Trouver le nombre optimal de clusters qui maximise la largeur moyenne de la silhouette
optimal_k <- which.max(silhouette_vals)
# Tracer la courbe de la silhouette
plot(x=1:10, y=silhouette_vals, type = "b", pch = 19)
# Ajouter une ligne pour indiquer le nombre optimal de clusters
abline(v = optimal_k, col = "red")
# Afficher le résultat
cat("Le nombre optimal de clusters selon la méthode de la silhouette est :", optimal_k, "\n")
