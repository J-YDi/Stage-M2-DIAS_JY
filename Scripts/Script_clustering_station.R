# Charger la bibliothèque
library(cluster)
library(ClustGeo)
library(sp)

# Test de ClustGeo sur les donnees REPHY
# fonctionne seul probleme est la maniere de faire la matrice de distance par station car
# on prend la moyenne par mesure

data <- read_delim("data_modif/Table_FLORTOT_Surf_9523_Stselect_hydro_phyto_chloro_phylum_period5_chlafilter.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                        grouping_mark = ""), trim_ws = TRUE)

Table_mean <- summarise(group_by(data, Code_point_Libelle), mean_temp=mean(TEMP,na.rm=T), mean_sal=mean(SALI,na.rm=T),
                   mean_turb=mean(TURB,na.rm=T), mean_nh4=mean(NH4,na.rm=T), mean_po4=mean(PO4,na.rm=T),
                   mean_sioh=mean(SIOH,na.rm=T), mean_oxygene=mean(OXYGENE,na.rm=T), mean_no3no2=mean(`NO3+NO2`,na.rm=T),
                   mean_turbfnu=mean(`TURB-FNU`,na.rm=T))

Table_median <- summarise(group_by(data, Code_point_Libelle), median_temp=median(TEMP,na.rm=T), median_sal=median(SALI,na.rm=T),
                          median_turb=median(TURB,na.rm=T), median_nh4=median(NH4,na.rm=T), median_po4=median(PO4,na.rm=T),
                          median_sioh=median(SIOH,na.rm=T), median_oxygene=median(OXYGENE,na.rm=T), median_no3no2=median(`NO3+NO2`,na.rm=T),
                          median_turbfnu=median(`TURB-FNU`,na.rm=T))

Table_mean_diss <- dist(Table_mean, method = "euclidean")
Table_median_diss <- dist(Table_median, method = "euclidean")

mean_clust <- hclust(Table_mean_diss, method = "ward.D2")
plot(mean_clust, labels = Table_mean$Code_point_Libelle)

median_clust <- hclust(Table_median_diss, method = "ward.D2")
plot(median_clust, labels = Table_mean$Code_point_Libelle)

# On teste en demandant 10 clusters
clust_mean <- cutree(mean_clust,k=10)
clust_median <- cutree(median_clust,k=10)

#On cree un jeu de donnees synthese
data_clust_result  <- cbind(Table$Code_point_Libelle,clust_mean,clust_median) 

# On utile la position moyenne ACP 
Table_PCA <- data[, c("SALI","TEMP","TURB","NH4","PO4","SIOH","OXYGENE","NO3+NO2","TURB-FNU")]
PCA_result <- PCA(Table_PCA,scale.unit = T,graph=T,ncp = 20)
coord_PCA <- PCA_result$ind$coord

fviz_eig(PCA_result,addlabels = T)
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

Table_abio_st <- summarise(group_by(Table_abio_PCA, Code_point_Libelle), mean_dim1=mean(Dim.1,na.rm=T)
                           , mean_dim2=mean(Dim.2,na.rm=T),
                           mean_dim3=mean(Dim.3,na.rm=T),
                           mean_dim4=mean(Dim.4,na.rm=T),
                           mean_dim5=mean(Dim.5,na.rm=T),
                           mean_dim6=mean(Dim.6,na.rm=T),
                           mean_dim7=mean(Dim.7,na.rm=T),
                           mean_dim8=mean(Dim.8,na.rm=T),
                           mean_dim9=mean(Dim.9,na.rm=T))

Table_ACP_mean_diss <- dist(Table_abio_st, method = "euclidean")

ACP_mean_clust <- hclust(Table_ACP_mean_diss, method = "ward.D2")
plot(ACP_mean_clust, labels = Table_mean$Code_point_Libelle)

clust_ACP_mean <- cutree(ACP_mean_clust,k=10)
data_clust_result <- as.data.frame(data_clust_result)
data_clust_result$clust_ACP_mean <- clust_ACP_mean

Table_abio_st_med <- summarise(group_by(Table_abio_PCA, Code_point_Libelle), median_dim1=median(Dim.1,na.rm=T)
                           , median_dim2=median(Dim.2,na.rm=T),
                           median_dim3=median(Dim.3,na.rm=T),
                           median_dim4=median(Dim.4,na.rm=T),
                           median_dim5=median(Dim.5,na.rm=T),
                           median_dim6=median(Dim.6,na.rm=T),
                           median_dim7=median(Dim.7,na.rm=T),
                           median_dim8=median(Dim.8,na.rm=T),
                           median_dim9=median(Dim.9,na.rm=T))

Table_ACP_median_diss <- dist(Table_abio_st_med, method = "euclidean")

ACP_median_clust <- hclust(Table_ACP_median_diss, method = "ward.D2")
plot(ACP_median_clust, labels = Table_mean$Code_point_Libelle)

clust_ACP_median <- cutree(ACP_median_clust,k=10)
data_clust_result <- as.data.frame(data_clust_result)
data_clust_result$clust_ACP_median <- clust_ACP_median

# On fait mtn avec la fonction de clustgeo sur que l'abio
tree <- hclustgeo(Table_mean_diss)
tree[["labels"]] <- Table_mean$Code_point_Libelle
plot(tree,hang = -1,
     main = "Ward dendrogram with D0 only")

rect.hclust(tree ,k = 10, border = c(4,5,3,2,1))
clustgeo_mean <- cutree(tree,10)
data_clust_result$clust_geo_mean <- clustgeo_mean

tree <- hclustgeo(Table_median_diss)
tree[["labels"]] <- Table_mean$Code_point_Libelle
plot(tree,hang = -1, 
     main = "Ward dendrogram with D0 only")

rect.hclust(tree ,k = 10, border = c(4,5,3,2,1))
clustgeo_median <- cutree(tree,10)
data_clust_result$clust_geo_median <- clustgeo_median

tree <- hclustgeo(Table_ACP_mean_diss)
tree[["labels"]] <- Table_mean$Code_point_Libelle
plot(tree,hang = -1, 
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree ,k = 10, border = c(4,5,3,2,1))
clustgeo_ACP_mean <- cutree(tree,10)
data_clust_result$clust_geo_ACPmean <- clustgeo_ACP_mean

tree <- hclustgeo(Table_ACP_median_diss)
tree[["labels"]] <- Table_mean$Code_point_Libelle
plot(tree,hang = -1, 
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree ,k = 10, border = c(4,5,3,2,1))
clustgeo_ACP_median <- cutree(tree,10)
data_clust_result$clust_geo_ACPmedian <- clustgeo_ACP_median

# Clustgeo avec alpha = 0 
# Associer une position unique a chaque station
data_geo <- summarise(group_by(data, Code_point_Libelle,lon,lat))
data_geo <- data_geo[-c(10,12:19,26:28,42:51,58:72),]
data_geo$lon <- as.numeric(data_geo$lon)
data_geo$lat <- as.numeric(data_geo$lat)

# moyenne direct 
D0 <- dist(Table_mean) # distance sur l'abio
tree <- hclustgeo(D0)
tree[["labels"]] <- Table_mean$Code_point_Libelle
plot(tree,hang = -1, 
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree ,k = 10, border = c(4,5,3,2,1))
legend("topright", legend = paste("cluster",1:5), 
       fill=1:5,bty= "n", border = "white")

firstPoints <- SpatialPoints(coords = cbind(data_geo$lat,data_geo$lon))
euclidDist <- sp::spDists(firstPoints,longlat = FALSE)

D1 <- as.dist(euclidDist)
range.alpha <- seq(0,1,0.1)
K <- 10
cr <- choicealpha(D0, D1, range.alpha, 
                  K, graph = FALSE)
cr$Q # proportion of explained inertia
plot(cr)
tree <- hclustgeo(D0,D1,alpha=0)
tree[["labels"]] <- Table_mean$Code_point_Libelle
plot(tree,hang = -1, 
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

clustgeo_0_mean <- cutree(tree,10)
data_clust_result$clustgeo_0_mean <- clustgeo_0_mean

# geo direct 
D0 <- dist(Table_mean) # distance sur l'abio
tree <- hclustgeo(D0)
plot(tree,hang = -1, label = F, 
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree ,k = 10, border = c(4,5,3,2,1))
legend("topright", legend = paste("cluster",1:5), 
       fill=1:5,bty= "n", border = "white")

firstPoints <- SpatialPoints(coords = cbind(data_geo$lat,data_geo$lon))
euclidDist <- sp::spDists(firstPoints,longlat = FALSE)

D1 <- as.dist(euclidDist)
range.alpha <- seq(0,1,0.1)
K <- 10
cr <- choicealpha(D0, D1, range.alpha, 
                  K, graph = FALSE)
cr$Q # proportion of explained inertia
plot(cr)
tree <- hclustgeo(D0,D1,alpha=1)
tree[["labels"]] <- Table_mean$Code_point_Libelle
plot(tree,hang = -1, 
     xlab = "", sub = "",
     main = "Ward dendrogram with D1 only")
clustgeo_geo <- cutree(tree,10)
data_clust_result$clustgeo_geo <- clustgeo_geo

plot(tree, labels = Table_mean$Code_point_Libelle)

# Analyse des silhouettes pour chaque matrice de distance 
matrice_distance <- D1
silhouette_vals <- c()
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




matrice_distance <- Table_ACP_mean_diss
silhouette_vals <- c()
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

matrice_distance <- Table_ACP_median_diss
silhouette_vals <- c()
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

matrice_distance <- Table_mean_diss
silhouette_vals <- c()
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

matrice_distance <- Table_median_diss
silhouette_vals <- c()
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


# En faisant le cheminement total avec alpha = 0.5
# Moyenne direct
tree <- hclustgeo(Table_mean_diss)
plot(tree,hang = -1, label = F, 
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree ,k = 7, border = c(4,5,3,2,1))
legend("topright", legend = paste("cluster",1:5), 
       fill=1:5,bty= "n", border = "white")

firstPoints <- SpatialPoints(coords = cbind(data_geo$lat,data_geo$lon))
euclidDist <- sp::spDists(firstPoints,longlat = FALSE)

D1 <- as.dist(euclidDist)
range.alpha <- seq(0,1,0.1)
K <- 10
cr <- choicealpha(D0, D1, range.alpha, 
                  K, graph = FALSE)
cr$Q # proportion of explained inertia
plot(cr)
tree <- hclustgeo(D0,D1,alpha=0.5)
tree[["labels"]] <- Table_mean$Code_point_Libelle
plot(tree,hang = -1, 
     xlab = "", sub = "",
     main = "Ward dendrogram with alpha 0.5")
clustgeo_mean_05 <- cutree(tree,10)
data_clust_result <- cbind(data_clust_result,clustgeo_mean_05)

# Mediane direct
tree <- hclustgeo(Table_median_diss)
plot(tree,hang = -1, label = F, 
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree ,k = 7, border = c(4,5,3,2,1))
legend("topright", legend = paste("cluster",1:5), 
       fill=1:5,bty= "n", border = "white")

firstPoints <- SpatialPoints(coords = cbind(data_geo$lat,data_geo$lon))
euclidDist <- sp::spDists(firstPoints,longlat = FALSE)

D1 <- as.dist(euclidDist)
range.alpha <- seq(0,1,0.1)
K <- 10
cr <- choicealpha(D0, D1, range.alpha, 
                  K, graph = FALSE)
cr$Q # proportion of explained inertia
plot(cr)
tree <- hclustgeo(D0,D1,alpha=0.5)
tree[["labels"]] <- Table_mean$Code_point_Libelle
plot(tree,hang = -1, 
     xlab = "", sub = "",
     main = "Ward dendrogram with alpha 0.5")
clustgeo_median_05 <- cutree(tree,10)
data_clust_result <- cbind(data_clust_result,clustgeo_median_05)

# Mean ACP
tree <- hclustgeo(Table_ACP_mean_diss)
plot(tree,hang = -1, label = F, 
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree ,k = 7, border = c(4,5,3,2,1))
legend("topright", legend = paste("cluster",1:5), 
       fill=1:5,bty= "n", border = "white")

firstPoints <- SpatialPoints(coords = cbind(data_geo$lat,data_geo$lon))
euclidDist <- sp::spDists(firstPoints,longlat = FALSE)

D1 <- as.dist(euclidDist)
range.alpha <- seq(0,1,0.1)
K <- 10
cr <- choicealpha(D0, D1, range.alpha, 
                  K, graph = FALSE)
cr$Q # proportion of explained inertia
plot(cr)
tree <- hclustgeo(D0,D1,alpha=0.5)
tree[["labels"]] <- Table_mean$Code_point_Libelle
plot(tree,hang = -1, 
     xlab = "", sub = "",
     main = "Ward dendrogram with alpha 0.5")
clustgeo_ACP_mean_05 <- cutree(tree,10)
data_clust_result <- cbind(data_clust_result,clustgeo_ACP_mean_05)


# Median ACP
tree <- hclustgeo(Table_ACP_median_diss)
plot(tree,hang = -1, label = F, 
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree ,k = 7, border = c(4,5,3,2,1))
legend("topright", legend = paste("cluster",1:5), 
       fill=1:5,bty= "n", border = "white")

firstPoints <- SpatialPoints(coords = cbind(data_geo$lat,data_geo$lon))
euclidDist <- sp::spDists(firstPoints,longlat = FALSE)

D1 <- as.dist(euclidDist)
range.alpha <- seq(0,1,0.1)
K <- 10
cr <- choicealpha(D0, D1, range.alpha, 
                  K, graph = FALSE)
cr$Q # proportion of explained inertia
plot(cr)
tree <- hclustgeo(D0,D1,alpha=0.5)
tree[["labels"]] <- Table_mean$Code_point_Libelle
plot(tree,hang = -1, 
     xlab = "", sub = "",
     main = "Ward dendrogram with alpha 0.5")
clustgeo_ACP_median_05 <- cutree(tree,10)
data_clust_result <- cbind(data_clust_result,clustgeo_ACP_median_05)



# En faisant le cheminement total avec alpha = 0.1 (recommande)
# Moyenne direct
tree <- hclustgeo(Table_mean_diss)
plot(tree,hang = -1, label = F, 
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree ,k = 7, border = c(4,5,3,2,1))
legend("topright", legend = paste("cluster",1:5), 
       fill=1:5,bty= "n", border = "white")

firstPoints <- SpatialPoints(coords = cbind(data_geo$lat,data_geo$lon))
euclidDist <- sp::spDists(firstPoints,longlat = FALSE)

D1 <- as.dist(euclidDist)
range.alpha <- seq(0,1,0.1)
K <- 10
cr <- choicealpha(D0, D1, range.alpha, 
                  K, graph = FALSE)
cr$Q # proportion of explained inertia
plot(cr)
tree <- hclustgeo(D0,D1,alpha=0.1)
tree[["labels"]] <- Table_mean$Code_point_Libelle
plot(tree,hang = -1, 
     xlab = "", sub = "",
     main = "Ward dendrogram with alpha 0.1")
clustgeo_mean_01 <- cutree(tree,10)
data_clust_result <- cbind(data_clust_result,clustgeo_mean_01)

# Mediane direct
tree <- hclustgeo(Table_median_diss)
plot(tree,hang = -1, label = F, 
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree ,k = 7, border = c(4,5,3,2,1))
legend("topright", legend = paste("cluster",1:5), 
       fill=1:5,bty= "n", border = "white")

firstPoints <- SpatialPoints(coords = cbind(data_geo$lat,data_geo$lon))
euclidDist <- sp::spDists(firstPoints,longlat = FALSE)

D1 <- as.dist(euclidDist)
range.alpha <- seq(0,1,0.1)
K <- 10
cr <- choicealpha(D0, D1, range.alpha, 
                  K, graph = FALSE)
cr$Q # proportion of explained inertia
plot(cr)
tree <- hclustgeo(D0,D1,alpha=0.1)
tree[["labels"]] <- Table_mean$Code_point_Libelle
plot(tree,hang = -1, 
     xlab = "", sub = "",
     main = "Ward dendrogram with alpha 0.1")
clustgeo_median_01 <- cutree(tree,10)
data_clust_result <- cbind(data_clust_result,clustgeo_median_01)

# Mean ACP
tree <- hclustgeo(Table_ACP_mean_diss)
plot(tree,hang = -1, label = F, 
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree ,k = 7, border = c(4,5,3,2,1))
legend("topright", legend = paste("cluster",1:5), 
       fill=1:5,bty= "n", border = "white")

firstPoints <- SpatialPoints(coords = cbind(data_geo$lat,data_geo$lon))
euclidDist <- sp::spDists(firstPoints,longlat = FALSE)

D1 <- as.dist(euclidDist)
range.alpha <- seq(0,1,0.1)
K <- 10
cr <- choicealpha(D0, D1, range.alpha, 
                  K, graph = FALSE)
cr$Q # proportion of explained inertia
plot(cr)
tree <- hclustgeo(D0,D1,alpha=0.1)
tree[["labels"]] <- Table_mean$Code_point_Libelle
plot(tree,hang = -1, 
     xlab = "", sub = "",
     main = "Ward dendrogram with alpha 0.1")
clustgeo_ACP_mean_01 <- cutree(tree,10)
data_clust_result <- cbind(data_clust_result,clustgeo_ACP_mean_01)


# Median ACP
tree <- hclustgeo(Table_ACP_median_diss)
plot(tree,hang = -1, label = F, 
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree ,k = 7, border = c(4,5,3,2,1))
legend("topright", legend = paste("cluster",1:5), 
       fill=1:5,bty= "n", border = "white")

firstPoints <- SpatialPoints(coords = cbind(data_geo$lat,data_geo$lon))
euclidDist <- sp::spDists(firstPoints,longlat = FALSE)

D1 <- as.dist(euclidDist)
range.alpha <- seq(0,1,0.1)
K <- 10
cr <- choicealpha(D0, D1, range.alpha, 
                  K, graph = FALSE)
cr$Q # proportion of explained inertia
plot(cr)
tree <- hclustgeo(D0,D1,alpha=0.1)
tree[["labels"]] <- Table_mean$Code_point_Libelle
plot(tree,hang = -1, 
     xlab = "", sub = "",
     main = "Ward dendrogram with alpha 0.1")
clustgeo_ACP_median_01 <- cutree(tree,10)
data_clust_result <- cbind(data_clust_result,clustgeo_ACP_median_01)

### representation des differents cluster sur carte
# associer la position geographique
colnames(data_clust_result)[1] <- colnames(data_geo)[1]

data_compar_clust <- right_join(data_clust_result,data_geo)

Worldmap <- map_data('worldHires')


ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)',title="clust_mean")+
  theme_gdocs()+
  geom_point(data = data_compar_clust, aes(x = lon, y = lat,colour=as.character(clust_mean), size =2))+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('maps_clust_mean.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/clustering_tests", dpi = 600, width = 200, height = 200, units = 'mm')

ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)',title="clust_median")+
  theme_gdocs()+
  geom_point(data = data_compar_clust, aes(x = lon, y = lat,colour=as.character(clust_median), size =2))+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('maps_clust_median.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/clustering_tests", dpi = 600, width = 200, height = 200, units = 'mm')

ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)',title="clust_ACP_mean")+
  theme_gdocs()+
  geom_point(data = data_compar_clust, aes(x = lon, y = lat,colour=as.character(clust_ACP_mean), size =2))+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('maps_clust_ACP_mean.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/clustering_tests", dpi = 600, width = 200, height = 200, units = 'mm')


ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)',title="clust_ACP_median")+
  theme_gdocs()+
  geom_point(data = data_compar_clust, aes(x = lon, y = lat,colour=as.character(clust_ACP_median), size =2))+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('maps_clust_ACP_median.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/clustering_tests", dpi = 600, width = 200, height = 200, units = 'mm')


ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)',title="clust_geo_mean")+
  theme_gdocs()+
  geom_point(data = data_compar_clust, aes(x = lon, y = lat,colour=as.character(clust_geo_mean), size =2))+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('maps_clust_geo_mean.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/clustering_tests", dpi = 600, width = 200, height = 200, units = 'mm')

ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)',title="clust_geo_median")+
  theme_gdocs()+
  geom_point(data = data_compar_clust, aes(x = lon, y = lat,colour=as.character(clust_geo_median), size =2))+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('maps_clust_geo_median.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/clustering_tests", dpi = 600, width = 200, height = 200, units = 'mm')

ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)',title="clust_geo_ACPmean")+
  theme_gdocs()+
  geom_point(data = data_compar_clust, aes(x = lon, y = lat,colour=as.character(clust_geo_ACPmean), size =2))+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('maps_clust_geo_ACPmean.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/clustering_tests", dpi = 600, width = 200, height = 200, units = 'mm')

ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)',title="clust_geo_ACPmedian")+
  theme_gdocs()+
  geom_point(data = data_compar_clust, aes(x = lon, y = lat,colour=as.character(clust_geo_ACPmedian), size =2))+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('maps_clust_geo_ACPmedian.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/clustering_tests", dpi = 600, width = 200, height = 200, units = 'mm')

ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)',title="clustgeo_0_mean")+
  theme_gdocs()+
  geom_point(data = data_compar_clust, aes(x = lon, y = lat,colour=as.character(clustgeo_0_mean), size =2))+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('maps_clustgeo_0_mean.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/clustering_tests", dpi = 600, width = 200, height = 200, units = 'mm')

ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)',title="clustgeo_geo")+
  theme_gdocs()+
  geom_point(data = data_compar_clust, aes(x = lon, y = lat,colour=as.character(clustgeo_geo), size =2))+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('maps_clustgeo_geo.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/clustering_tests", dpi = 600, width = 200, height = 200, units = 'mm')

ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)',title="clustgeo_mean_05")+
  theme_gdocs()+
  geom_point(data = data_compar_clust, aes(x = lon, y = lat,colour=as.character(clustgeo_mean_05), size =2))+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('maps_clustgeo_mean_05.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/clustering_tests", dpi = 600, width = 200, height = 200, units = 'mm')

ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)',title="clustgeo_median_05")+
  theme_gdocs()+
  geom_point(data = data_compar_clust, aes(x = lon, y = lat,colour=as.character(clustgeo_median_05), size =2))+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('maps_clustgeo_median_05.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/clustering_tests", dpi = 600, width = 200, height = 200, units = 'mm')

ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)',title="clustgeo_ACP_mean_05")+
  theme_gdocs()+
  geom_point(data = data_compar_clust, aes(x = lon, y = lat,colour=as.character(clustgeo_ACP_mean_05), size =2))+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('maps_clustgeo_ACP_mean_05.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/clustering_tests", dpi = 600, width = 200, height = 200, units = 'mm')

ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)',title="clustgeo_ACP_median_05")+
  theme_gdocs()+
  geom_point(data = data_compar_clust, aes(x = lon, y = lat,colour=as.character(clustgeo_ACP_median_05), size =2))+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('maps_clustgeo_ACP_median_05.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/clustering_tests", dpi = 600, width = 200, height = 200, units = 'mm')


ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)',title="clustgeo_mean_01")+
  theme_gdocs()+
  geom_point(data = data_compar_clust, aes(x = lon, y = lat,colour=as.character(clustgeo_mean_01), size =2))+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('maps_clustgeo_mean_01.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/clustering_tests", dpi = 600, width = 200, height = 200, units = 'mm')

ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)',title="clustgeo_median_01")+
  theme_gdocs()+
  geom_point(data = data_compar_clust, aes(x = lon, y = lat,colour=as.character(clustgeo_median_01), size =2))+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('maps_clustgeo_median_01.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/clustering_tests", dpi = 600, width = 200, height = 200, units = 'mm')

ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)',title="clustgeo_ACP_mean_01")+
  theme_gdocs()+
  geom_point(data = data_compar_clust, aes(x = lon, y = lat,colour=as.character(clustgeo_ACP_mean_01), size =2))+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('maps_clustgeo_ACP_mean_01.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/clustering_tests", dpi = 600, width = 200, height = 200, units = 'mm')

ggplot() + geom_polygon(data = Worldmap, aes(x = long, y = lat, group = group), fill = 'gray', color = 'gray10', size = .25)+
  coord_fixed(xlim=c(-5.5,9.5), ylim=c(41,51.5), ratio=1.4)+
  labs(y = 'Latitude (degrés)', x = 'Longitude (degrés)',title="clustgeo_ACP_median_01")+
  theme_gdocs()+
  geom_point(data = data_compar_clust, aes(x = lon, y = lat,colour=as.character(clustgeo_ACP_median_01), size =2))+
  theme(panel.grid.major = element_line(color = 'gray10', size = .25), panel.grid.minor = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = 'lightblue2'))+
  guides(color = guide_legend(override.aes = list(size = 10)))
ggsave('maps_clustgeo_ACP_median_01.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/clustering_tests", dpi = 600, width = 200, height = 200, units = 'mm')











D0 <- dist(Table_abio) # distance sur l'abio
tree <- hclustgeo(D0)
plot(tree,hang = -1, label = F, 
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree ,k = 7, border = c(4,5,3,2,1))
legend("topright", legend = paste("cluster",1:5), 
       fill=1:5,bty= "n", border = "white")

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

# Ou pas 
Table <- Table_test 

Table_PCA <- Table[, c("SALI","TEMP","TURB","NH4","PO4","SIOH","OXYGENE","NO3+NO2","TURB-FNU")]
PCA_result <- PCA(Table_PCA,scale.unit = T,graph=T,ncp = 20)
coord_PCA <- PCA_result$ind$coord

fviz_eig(PCA_result,addlabels = T)
Table_abio_PCA <- cbind(Table$Code_point_Libelle,Table$ID.interne.passage,Table$lon,Table$lat, coord_PCA)
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

colnames(Table_abio_PCA)[1:4] <- c("Code_point_Libelle","ID.interne.passage","lon","lat")

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

rect.hclust(tree ,k = 7, border = c(4,5,3,2,1))



firstPoints <- SpatialPoints(coords = cbind(as.numeric(Table_abio_st$lat),as.numeric(Table_abio_st$lon)))
euclidDist <- sp::spDists(firstPoints,longlat = FALSE)

D1 <- as.dist(euclidDist)
range.alpha <- seq(0,1,0.1)
K <- 10
cr <- choicealpha(D0, D1, range.alpha, 
                  K, graph = FALSE)
cr$Q # proportion of explained inertia
plot(cr)

cr$Qnorm
plot(cr,norm =TRUE)

tree <- hclustgeo(D0,D1,alpha=0)
P5bis <- cutree(tree,10)
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
silhouette_vals <- c()
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



# ACP temporelle
Table_abio_PCA$ID.interne.passage <- as.double(Table_abio_PCA$ID.interne.passage)


# test structure temporelle avec l'ACP
Table_abio_PCA <- left_join(select(Table, Date,ID.interne.passage),Table_abio_PCA)

ggplot(Table_abio_PCA)+
  geom_line(aes(x=Date,y=Dim.1))
  facet_wrap(~Code_point_Libelle)

  ggplot(Table_abio_PCA)+
    geom_line(aes(x=Date,y=Dim.2))+
  facet_wrap(~Code_point_Libelle)


    # Sélection des composantes principales (choisies en fonction de la visualisation)
  composantes_principales <- PCA_result$ind$coord[, c(1, 2)]
  
  # Appliquer DBSCAN
  dbscan_result <- dbscan(select(Table,SALI:OXYGENE), eps = 0.5, MinPts = 5)
  
  # Visualiser les résultats de DBSCAN
  plot(composantes_principales, col = dbscan_result$cluster + 1, pch = 16, main = "DBSCAN Results")

  Table_abio_PCA <- cbind(Table_abio_PCA,dbscan_result$cluster)  

  ggplot(Table_abio_PCA)+
    geom_line(aes(y=dbscan_result$cluster,x=Date))+
    facet_wrap(~Code_point_Libelle)
    geom_line()