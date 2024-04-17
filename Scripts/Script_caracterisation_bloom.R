data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                                  delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                                  trim_ws = TRUE)

# Correlation tout cluster 
Table.corr_all <- dplyr::select(data_bloom,-c(Code_point_Libelle,cluster,Date,Year,Bloom:Bloom_Genre,Month,season))
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
         title = "Correlation durant bloom tout cluster"
)


# Correlation cluster 1
data_bloom_1 <- filter(data_bloom,cluster ==1)
Table.corr_all <- dplyr::select(data_bloom_1,-c(Code_point_Libelle,cluster,Date,Year,Bloom:Bloom_Genre,Month,season))
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
         title = "Correlation durant bloom Cluster 1"
)


# Correlation tout cluster 
data_bloom_2 <- filter(data_bloom,cluster ==2)
Table.corr_all <- dplyr::select(data_bloom_2,-c(Code_point_Libelle,cluster,Date,Year,Bloom:Bloom_Genre,Month,season))
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
         title = "Correlation durant bloom Cluster 2"
)

# Correlation cluster 3
data_bloom_3 <- filter(data_bloom,cluster ==3)
Table.corr_all <- dplyr::select(data_bloom_3,-c(Code_point_Libelle,cluster,Date,Year,Bloom:Bloom_Genre,Month,season))
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
         title = "Correlation durant bloom Cluster 3"
)

# Correlation cluster 4 
data_bloom_4 <- filter(data_bloom,cluster ==4)
Table.corr_all <- dplyr::select(data_bloom_4,-c(Code_point_Libelle,cluster,Date,Year,Bloom:Bloom_Genre,Month,season))
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
         title = "Correlation durant bloom Cluster 4"
)

# Correlation bloom Dinophyceae
data_bloom_dino <- filter(data_bloom,Bloom_Phylum == "Dino")
Table.corr_all <- dplyr::select(data_bloom_dino,-c(Code_point_Libelle,cluster,Date,Year,Bloom:Bloom_Genre,Month,season))
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
         title = "Correlation durant bloom Dinophyceae"
)

# Correlation bloom Bacillariophyceae
data_bloom_bac <- filter(data_bloom,Bloom_Phylum == "Bac")
Table.corr_all <- dplyr::select(data_bloom_bac,-c(Code_point_Libelle,cluster,Date,Year,Bloom:Bloom_Genre,Month,season))
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
         title = "Correlation durant bloom Bacillariophyceae"
)

# Correlation bloom Cryptophyceae
data_bloom_crypto <- filter(data_bloom,Bloom_Phylum == "Cryptophyceae")
Table.corr_all <- dplyr::select(data_bloom_crypto,-c(Code_point_Libelle,cluster,Date,Year,Bloom:Bloom_Genre,Month,season))
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
         title = "Correlation durant bloom Cryptophyceae"
)

# Correlation bloom Haptophyta
data_bloom_hapto <- filter(data_bloom,Bloom_Phylum == "Hapt")
Table.corr_all <- dplyr::select(data_bloom_hapto,-c(Code_point_Libelle,cluster,Date,Year,Bloom:Bloom_Genre,Month,season))
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
         title = "Correlation durant bloom Haptophyta"
)




# Correlation bloom été
data_bloom_summer <- filter(data_bloom,season == "Summer")
Table.corr_all <- dplyr::select(data_bloom_summer,-c(Code_point_Libelle,cluster,Date,Year,Bloom:Bloom_Genre,Month,season))
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
         title = "Correlation durant bloom été"
)

# Correlation bloom printemps
data_bloom_spring <- filter(data_bloom,season == "Spring")
Table.corr_all <- dplyr::select(data_bloom_spring,-c(Code_point_Libelle,cluster,Date,Year,Bloom:Bloom_Genre,Month,season))
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
         title = "Correlation durant bloom printemps"
)

# Correlation bloom automne
data_bloom_fall <- filter(data_bloom,season == "Fall")
Table.corr_all <- dplyr::select(data_bloom_fall,-c(Code_point_Libelle,cluster,Date,Year,Bloom:Bloom_Genre,Month,season))
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
         title = "Correlation durant bloom Automne"
)

# Correlation bloom hiver
data_bloom_winter <- filter(data_bloom,season == "Winter")
Table.corr_all <- dplyr::select(data_bloom_winter,-c(Code_point_Libelle,cluster,Date,Year,Bloom:Bloom_Genre,Month,season))
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
         title = "Correlation durant bloom Hiver"
)


# Relation entre Abondance totale et chlorophylle
data_bloom$cluster <- as.factor(data_bloom$cluster)
cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

ggplot(data_bloom)+
  geom_point(aes(x=log(CHLOROA+1),y=log(Abdtot+1), colour=as.character(cluster)),size=3)+
  scale_colour_manual(values = cluster_col,guide="none")+
  geom_smooth(aes(x=log(CHLOROA+1),y=log(Abdtot+1), colour=as.character(cluster)),method = "lm")+
  facet_wrap(~cluster)

ggplot(data_bloom)+
  geom_point(aes(x=log(CHLOROA+1),y=P_dominance, colour=as.character(cluster)),size=3)+
  scale_colour_manual(values = cluster_col,guide="none")+
  geom_smooth(aes(x=log(CHLOROA+1),y=P_dominance, colour=as.character(cluster)),method = "lm")+
  facet_wrap(~cluster)

ggplot(data_bloom)+
  geom_point(aes(x=log(CHLOROA+1),y=BergerParker, colour=as.character(cluster)),size=3)+
  scale_colour_manual(values = cluster_col,guide="none")+
  geom_smooth(aes(x=log(CHLOROA+1),y=BergerParker, colour=as.character(cluster)),method = "lm")+
  facet_wrap(~cluster)

ggplot(data_bloom)+
  geom_histogram(aes(x=P_dominance, fill=as.character(cluster)),size=3,bins = 10)+
  scale_fill_manual(values = cluster_col,guide="none")+
  facet_wrap(~cluster)+
  labs(title = "Histogramme % de dominance du taxon majoritaire",
       x = "Pourcentage de dominance", y = "N")

ggplot(data_bloom)+
  geom_histogram(aes(x=P_dominance, fill=as.character(cluster)),size=3,bins = 10)+
  scale_fill_manual(values = cluster_col,guide="none")+
  facet_wrap(cluster~Bloom_Phylum,nrow = 4,scales = "free_y")+
  labs(title = "Histogramme % de dominance du taxon majoritaire",
       x = "Pourcentage de dominance", y = "N")

ggplot(data_bloom)+
  geom_histogram(aes(x=P_dominance, fill=as.character(cluster)),size=3,bins = 10)+
  scale_fill_manual(values = cluster_col,guide="none")+
  facet_wrap(~Bloom_Phylum,nrow = 4,scales = "free_y")+
  labs(title = "Histogramme % de dominance du taxon majoritaire",
       x = "Pourcentage de dominance", y = "N")

ggplot(data_bloom)+
  geom_histogram(aes(x=P_dominance, fill=as.character(cluster)),size=3,bins = 10)+
  scale_fill_manual(values = cluster_col,guide="none")+
  facet_wrap(~Bloom_Phylum,nrow = 4,scales = "free_y")+
  labs(title = "Histogramme % de dominance du taxon majoritaire",
       x = "Pourcentage de dominance", y = "N")

ggplot(data_bloom)+
  geom_boxplot(aes(x=cluster,y=P_dominance,group=cluster,fill=as.character(cluster)))+
  scale_fill_manual(values = cluster_col,guide="none")+
  #facet_wrap(~Bloom_Phylum,nrow = 4,scales = "free_y")+
  labs(title = "",
       y = "Pourcentage de dominance", x = "cluster")+
  ggplot(data_bloom)+
  geom_boxplot(aes(x= cluster,y=P_dominance, fill=as.character(cluster)))+
  scale_fill_manual(values = cluster_col,guide="none")+
  facet_wrap(~Bloom_Phylum,nrow = 4,scales = "free_y")+
  labs(title = "",
       x ="cluster", y = "% de dominance")+
  ggplot(data_bloom)+
  geom_boxplot(aes(x=cluster,y=P_dominance,group=cluster,fill=as.character(cluster)))+
  scale_fill_manual(values = cluster_col,guide="none")+
  facet_wrap(cluster~Bloom_Phylum,nrow = 4,scales = "free")+
  labs(title = "",
       y = "Pourcentage de dominance", x = "cluster")+
    ggplot(data_bloom)+
    geom_boxplot(aes(y=P_dominance),fill="grey")+
    scale_fill_manual(values = cluster_col,guide="none")+
    scale_x_continuous(breaks = 0)+
  labs(title = "",
       y = "Pourcentage de dominance")
ggsave('Boxplot_dominance.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Correlations",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data_bloom)+
  geom_boxplot(aes(y=P_dominance))+
  facet_wrap(~Bloom_Phylum,nrow = 4)+
  labs(title = "",
       y = "Pourcentage de dominance", x = "cluster")
ggsave('Boxplot_dominance_classe.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Correlations",dpi = 600, width = 400, height = 380, units = 'mm')


mean(data_bloom$P_dominance)
median(data_bloom$P_dominance)

#### Definition de bloom dominé par une spe ####
data_bloom$Monodominance <- "NON"
data_bloom$Monodominance <- ifelse(data_bloom$P_dominance >= 63.11, "OUI",data_bloom$Monodominance) 

write.csv2(data_bloom,file="data_modif/Table_bloom_VF.csv", row.names = FALSE,dec = ".")


# Description des blooms
data_bloom <- read_delim("data_modif/Table_bloom_VF.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)

descript <- filter(data_bloom, Bloom_Phylum == "Hapt" & cluster == 2)

for (i in 1:length(levels(as.factor(descript$Bloom_Genre)))){
  levels(as.factor(descript$Bloom_Genre))[i]
  descript2 <- filter(descript,Bloom_Genre == levels(as.factor(descript$Bloom_Genre))[i])
  nrow(descript2)
  P <- nrow(filter(descript2, season == "Spring"))
  E <- nrow(filter(descript2, season == "Summer"))
  A <- nrow(filter(descript2, season == "Fall"))
  W <- nrow(filter(descript2, season == "Winter"))

  cat("Taxon:",levels(as.factor(descript$Bloom_Genre))[i],nrow(descript2),"\n")
  cat(P,E,A,W,"\n")
}

descript <- filter(data_bloom, Bloom_Phylum == "Cilio")

for (i in 1:length(levels(as.factor(descript$Bloom_Genre)))){
  levels(as.factor(descript$Bloom_Genre))[i]
  descript2 <- filter(descript,Bloom_Genre == levels(as.factor(descript$Bloom_Genre))[i])
  nrow(descript2)
  P <- nrow(filter(descript2, season == "Spring"))
  E <- nrow(filter(descript2, season == "Summer"))
  A <- nrow(filter(descript2, season == "Fall"))
  W <- nrow(filter(descript2, season == "Winter"))
  
  cat("Taxon:",levels(as.factor(descript$Bloom_Genre))[i],nrow(descript2),"\n")
  cat(P,E,A,W,"\n")
}



descript <- filter(data_bloom, cluster == 2)

for (i in 1:length(levels(as.factor(descript$Year)))){
  levels(as.factor(descript$Year))[i]
  descript2 <- filter(descript,Year == levels(as.factor(descript$Year))[i])
  nrow(descript2)
  B <- nrow(filter(descript2, Bloom_Phylum == "Bac"))
  D <- nrow(filter(descript2, Bloom_Phylum == "Dino"))
  C <- nrow(filter(descript2, Bloom_Phylum == "Cryptophyceae"))
  H <- nrow(filter(descript2, Bloom_Phylum == "Hapt"))
  CI <- nrow(filter(descript2, Bloom_Phylum == "Cilio"))
  cat("Taxon:",levels(as.factor(descript$Year))[i],nrow(descript2),"\n")
  cat(H,B,D,C,CI,"\n")
}


descript <- filter(data_bloom, Monodominance == "OUI" & cluster == "1")

for (i in 1:length(levels(as.factor(descript$Bloom_Genre)))){
  levels(as.factor(descript$Bloom_Genre))[i]
  descript2 <- filter(descript,Bloom_Genre == levels(as.factor(descript$Bloom_Genre))[i])
  nrow(descript2)
  cat("Taxon:",levels(as.factor(descript$Bloom_Genre))[i],nrow(descript2),"\n")
}

descript <- filter(data_bloom, Code_point_Libelle == "Diana centre")
for (i in 1:length(levels(as.factor(descript$Year)))){
  levels(as.factor(descript$Year))[i]
  descript2 <- filter(descript,Year == levels(as.factor(descript$Year))[i])
  nrow(descript2)
  cat("Taxon:",levels(as.factor(descript$Year))[i],nrow(descript2),"\n")
  }


descript <- filter(data_bloom, cluster == 4)

for (i in 1:length(levels(as.factor(descript$Code_point_Libelle)))){
  levels(as.factor(descript$Code_point_Libelle))[i]
  descript2 <- filter(descript,Code_point_Libelle == levels(as.factor(descript$Code_point_Libelle))[i])
  nrow(descript2)
  B <- nrow(filter(descript2, Bloom_Phylum == "Bac"))
  D <- nrow(filter(descript2, Bloom_Phylum == "Dino"))
  C <- nrow(filter(descript2, Bloom_Phylum == "Cryptophyceae"))
  H <- nrow(filter(descript2, Bloom_Phylum == "Hapt"))
  CI <- nrow(filter(descript2, Bloom_Phylum == "Cilio"))
  MD <- nrow(filter(descript2, Monodominance == "OUI"))
  cat("Taxon:",levels(as.factor(descript$Code_point_Libelle))[i],nrow(descript2),"\n")
  cat(B,D,C,H,CI,"\n")
}

descript <- filter(data_bloom, cluster == 1)
for (i in 1:length(levels(as.factor(descript$Code_point_Libelle)))){
  levels(as.factor(descript$Code_point_Libelle))[i]
  descript2 <- filter(descript,Code_point_Libelle == levels(as.factor(descript$Code_point_Libelle))[i])
  nrow(descript2)
  P <- nrow(filter(descript2, season == "Spring"))
  E <- nrow(filter(descript2, season == "Summer"))
  A <- nrow(filter(descript2, season == "Fall"))
  W <- nrow(filter(descript2, season == "Winter"))
  MD <- (nrow(filter(descript2, Monodominance == "OUI"))/nrow(descript2))*100
  cat("Taxon:",levels(as.factor(descript$Code_point_Libelle))[i],nrow(descript2),"\n")
  cat(P,E,A,W,MD,"\n")
}



descript <- filter(data_bloom, cluster == 3)
for (i in 1:length(levels(as.factor(descript$Code_point_Libelle)))){
  levels(as.factor(descript$Code_point_Libelle))[i]
  descript2 <- filter(descript,Code_point_Libelle == levels(as.factor(descript$Code_point_Libelle))[i])
  nrow(descript2)
  P <- nrow(filter(descript2, Bloom_Phylum == "Hapt" & season == "Spring"))
  E <- nrow(filter(descript2, Bloom_Phylum == "Hapt" & season == "Summer"))
  A <- nrow(filter(descript2, Bloom_Phylum == "Hapt" & season == "Fall"))
  W <- nrow(filter(descript2, Bloom_Phylum == "Hapt" & season == "Winter"))
  MD <- (nrow(filter(descript2, Bloom_Phylum == "Hapt" & Monodominance == "OUI"))/nrow(filter(descript2, Bloom_Phylum == "Hapt")))*100
  cat("Taxon:",levels(as.factor(descript$Code_point_Libelle))[i],nrow(descript2),"\n")
  cat(P,E,A,W,MD,"\n")
}


# Tendance nombre de bloom au cours du temps
data_bloom_trend <- read_delim("data_modif/Trend_bloom_md.csv", 
                         delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)

mk.test(dplyr::filter(data_bloom_trend, Cluster == "1")$N_bloom)
mk.test(dplyr::filter(data_bloom_trend, Cluster == "1")$P_MD)

mk.test(dplyr::filter(data_bloom_trend, Cluster == "2")$N_bloom) #SIGN
sens.slope(dplyr::filter(data_bloom_trend, Cluster == "2")$N_bloom)

mk.test(dplyr::filter(data_bloom_trend, Cluster == "2")$P_MD)

mk.test(dplyr::filter(data_bloom_trend, Cluster == "3")$N_bloom)
mk.test(dplyr::filter(data_bloom_trend, Cluster == "3")$P_MD)

mk.test(dplyr::filter(data_bloom_trend, Cluster == "4")$N_bloom) #SIGN
sens.slope(dplyr::filter(data_bloom_trend, Cluster == "4")$N_bloom)

mk.test(dplyr::filter(data_bloom_trend, Cluster == "4")$P_MD) #SIGN
sens.slope(dplyr::filter(data_bloom_trend, Cluster == "4")$P_MD)

mk.test(dplyr::filter(data_bloom_trend, Cluster == "Tous")$N_bloom) #SIGN
sens.slope(dplyr::filter(data_bloom_trend, Cluster == "Tous")$N_bloom)

mk.test(dplyr::filter(data_bloom_trend, Cluster == "Tous")$P_MD)

# Date du bloom en fonction des années 
data_bloom <- data_bloom |>
  arrange(cluster, Date) |>
  mutate(Dayj = as.numeric(yday(Date)))

datapheno <- summarize(group_by(data_bloom, Bloom_Phylum,cluster,Year),MD = mean(Dayj,na.rm=T))

ggplot(datapheno)+
  geom_line(aes(x=Year,y=MD,colour=Bloom_Phylum),size=3)+
  facet_wrap(~cluster)

datapheno <- summarize(group_by(data_bloom,cluster,Year),MD = mean(Dayj,na.rm=T))

ggplot(datapheno)+
  geom_line(aes(x=Year,y=MD,colour=as.character(cluster)),size=3)

ggplot(data_bloom)+
  geom_point(aes(x=Year,y=Dayj,colour=Bloom_Phylum),size=3)+
  geom_smooth(aes(x=Year,y=Dayj,colour=Bloom_Phylum),method = "lm")+
  scale_y_continuous(breaks = seq(0,365, by = 50),limits = c(0,365))+
  facet_wrap(~cluster)

# Histogramme ou violin plot pour la chlorophylle par bloom
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)

cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF")

ggplot(data_bloom)+
  geom_histogram(aes(x=CHLOROA,group=Bloom_Phylum,fill=Bloom_Phylum))+
  scale_x_continuous(breaks= seq(0,120, by = 5))
ggsave('CHLOROA_Phylum.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Hist",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data_bloom)+
  geom_histogram(aes(x=CHLOROA,group=cluster,fill=as.character(cluster)))+
  scale_fill_manual(name = 'Cluster', values = cluster_col)+
  scale_x_continuous(breaks= seq(0,120, by = 5))
ggsave('CHLOROA_Cluster.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Hist",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(filter(data_bloom, cluster == 1))+
         geom_histogram(aes(x=CHLOROA,group=Bloom_Phylum,fill=Bloom_Phylum))+
         scale_x_continuous(breaks= seq(0,35, by = 5),limits = c(0,35))
ggsave('CHLOROA_Phylum_Cluster1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Hist",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(filter(data_bloom, cluster == 2))+
  geom_histogram(aes(x=CHLOROA,group=Bloom_Phylum,fill=Bloom_Phylum))+
  scale_x_continuous(breaks= seq(0,35, by = 5),limits = c(0,35))
ggsave('CHLOROA_Phylum_Cluster2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Hist",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(filter(data_bloom, cluster == 3))+
  geom_histogram(aes(x=CHLOROA,group=Bloom_Phylum,fill=Bloom_Phylum))+
  scale_x_continuous(breaks= seq(0,35, by = 5),limits = c(0,35))
ggsave('CHLOROA_Phylum_Cluster3.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Hist",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(filter(data_bloom, cluster == 4))+
  geom_histogram(aes(x=CHLOROA,group=Bloom_Phylum,fill=Bloom_Phylum))+
  scale_x_continuous(breaks= seq(0,35, by = 5),limits = c(0,35))
ggsave('CHLOROA_Phylum_Cluster4.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Hist",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data_bloom,aes(y = CHLOROA,x=as.factor(cluster)))+
  geom_violin(aes(fill=as.character(cluster)))+
    scale_color_viridis_d()+
  scale_fill_manual(name = 'Cluster', values = cluster_col)+
  scale_y_continuous(breaks= seq(0,120, by = 5),limits = c(0,120))
ggsave('CHLOROA_Cluster_Violin.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Hist",dpi = 600, width = 400, height = 380, units = 'mm')

ggplot(data_bloom,aes(y = CHLOROA,x=as.factor(Bloom_Phylum)))+
  geom_violin(aes(fill=as.character(Bloom_Phylum)))+
  scale_color_viridis_d()+
  scale_y_continuous(breaks= seq(0,120, by = 5),limits = c(0,120))
ggsave('CHLOROA_Phylum_Violin.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Hist",dpi = 600, width = 400, height = 380, units = 'mm')


ggplot(data_bloom,aes(y = CHLOROA,x=as.factor(cluster)))+
  geom_violin(aes(fill=as.character(cluster)))+
  scale_color_viridis_d()+
  scale_fill_manual(name = 'Cluster', values = cluster_col)+
  #scale_y_continuous(breaks= seq(0,120, by = 5),limits = c(0,120))+
  facet_wrap(~Bloom_Phylum, scales = "free_y")
ggsave('CHLOROA_Phylum_Cluster_Violin.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Hist",dpi = 600, width = 400, height = 380, units = 'mm')

# Pourcentage de dominance lors de bloom et non.

data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

data$Abdtot <- apply(data[,24:247],1,sum,na.rm=T)

data$Abdmax <- apply(data[,24:247],1,max,na.rm=T)

data$P_dominance <- data$Abdmax/data$Abdtot 

data <- dplyr::select(data, Code_point_Libelle,cluster, Date, P_dominance)

data_bloom <- dplyr::select(data_bloom, -cluster)

data_combo <- left_join(data,data_bloom, by = join_by(Code_point_Libelle, Date))

median(filter(data_combo, Month >= 0)$P_dominance.x,na.rm=T)



ggplot(filter(data_combo, is.na(Bloom_Phylum)))+
  geom_boxplot(aes(x=cluster,y=P_dominance.x,group=cluster,fill=as.character(cluster)))+
  scale_fill_manual(values = cluster_col,guide="none")+
  #facet_wrap(~Bloom_Phylum,nrow = 4,scales = "free_y")+
  labs(title = "",
       y = "Pourcentage de dominance", x = "cluster")+
  ggplot(filter(data_combo, is.na(Bloom_Phylum)))+
  geom_boxplot(aes(y=P_dominance.x),fill="grey")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_x_continuous(breaks = 0)+
  labs(title = "",
       y = "Pourcentage de dominance")
ggsave('Boxplot_dominance_NONBLOOM.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Correlations",dpi = 600, width = 400, height = 380, units = 'mm')

wilcox.test(filter(data_combo, is.na(Bloom_Phylum))$P_dominance.x,
            filter(data_combo, Month >= 0)$P_dominance.x)

x <- (filter(data_combo, Month >= 0)$P_dominance.x)*100
x <- as.data.frame(x)
x$Bloom <- "OUI"
y <- (filter(data_combo, is.na(Bloom_Phylum))$P_dominance.x)*100
y <- as.data.frame(y)
y$Bloom <- "NON"

median(x$P_dominance,na.rm = T)
colnames(x)[1] <- c("P_dominance")
colnames(y)[1] <- c("P_dominance")

data_viz <- rbind(x,y)
data_viz$Bloom <- as.factor(data_viz$Bloom)

ggplot(data_viz)+
  geom_boxplot(aes(y=P_dominance,x=Bloom,group=Bloom),fill="grey")+
  scale_fill_manual(values = cluster_col,guide="none")+
  scale_y_continuous(breaks = seq(0,100,20),limits =c(0,120))+
  labs(title = "",
       y = "Pourcentage de dominance",x="Episode de bloom")
ggsave('Boxplot_dominance_Comparaison.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/Bloom_description/Correlations",dpi = 600, width = 200, height = 180, units = 'mm')


# Relation abondance totale et chlorophylle a 


data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

data$Abdtot <- apply(data[,24:247],1,sum,na.rm=T)
data$Bloom <- NA
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom$Bloom <- "OUI"

data <- dplyr::select(data,Code_point_Libelle,Date, CHLOROA, Abdtot)
data_bloom$Bloom <- "OUI"
data_bloom <- dplyr::select(data_bloom,Code_point_Libelle,Date,Bloom)

t <- left_join(data,data_bloom, by = join_by(Code_point_Libelle,Date))
ggplot(t)+
  geom_point(aes(x=Abdtot,y=log(CHLOROA+1),colour=Bloom))


## LDA Parametres environnementaux ####
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

data$EpBloom <- ifelse(data$P_dominance > 0, "Oui","Non")
data[is.na(data$EpBloom),277] <- "Non"

data[,c("TEMP","SALI","PO4","NO3+NO2","TURB-FNU","SIOH","NH4","OXYGENE")] <- scale(data[,c("TEMP","SALI","PO4","NO3+NO2","TURB-FNU","SIOH","NH4","OXYGENE")])

lda_model <- lda(EpBloom ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = data)

# Résumé du modèle
print(lda_model)

coefficients <- lda_model$scaling
coefficients <- as.data.frame(coefficients)
coefficients$Var <- rownames(coefficients)
coefficients <- coefficients[order(-abs(coefficients$LD1)), ]
coefficients$Var <- as.factor(coefficients$Var)

ggplot(coefficients, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

BacDino <- filter(data, Bloom_Phylum == "Bac" | Bloom_Phylum == "Dino")

lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = BacDino)

# Résumé du modèle
print(lda_model)

coefficients <- lda_model$scaling
coefficients <- as.data.frame(coefficients)
coefficients$Var <- rownames(coefficients)
coefficients <- coefficients[order(-abs(coefficients$LD1)), ]
coefficients$Var <- as.factor(coefficients$Var)

ggplot(coefficients, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

bloom <- filter(data, P_dominance > 0)

lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = bloom)

# Résumé du modèle
print(lda_model)

coefficients <- lda_model$scaling
coefficients <- as.data.frame(coefficients)
coefficients$Var <- rownames(coefficients)
coefficients <- coefficients[order(-abs(coefficients$LD1)), ]
coefficients$Var <- as.factor(coefficients$Var)

ggplot(coefficients, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Importance des coefficients linéaires - LD1 (0.79)",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité
ggplot(coefficients, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Importance des coefficients linéaires - LD2 (0.17)",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité
ggplot(coefficients, aes(x = reorder(Var, -abs(LD3)), y = abs(LD3))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Importance des coefficients linéaires - LD3 (0.03)",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité



## LDA Parametres environnementaux Dino vs Bac ####
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_withbloom.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)


BacDino <- filter(data, Bloom_Phylum == "Bac" | Bloom_Phylum == "Dino" | is.na(Bloom_Phylum))
BacDino[is.na(BacDino$Bloom_Phylum),]$Bloom_Phylum <- "Non"

lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = BacDino)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

datam <- filter(BacDino, cluster == 1)
lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = datam)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité


datam <- filter(BacDino, cluster == 2)
lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = datam)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité



datam <- filter(BacDino, cluster == 3)
lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = datam)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité


datam <- filter(BacDino, cluster == 4)
lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = datam)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité


BacDino <- BacDino |>
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Hiver",
                            Month %in% c(03, 04, 05) ~ "Printemps",
                            Month %in% c(06, 07, 08) ~ "Ete",
                            Month %in% c(09, 10, 11) ~ "Automne", TRUE ~ NA_character_))

datam <- filter(BacDino, season == "Ete")
lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = datam)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

datam <- filter(BacDino, cluster == 1, season == "Ete")
lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = datam)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité


datam <- filter(BacDino, cluster == 2, season == "Ete")
lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = datam)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité



datam <- filter(BacDino, cluster == 3, season == "Ete")
lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = datam)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité


datam <- filter(BacDino, cluster == 4,season == "Ete")
lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = datam)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité



datam <- filter(BacDino, season == "Printemps")
lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = datam)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

datam <- filter(BacDino, cluster == 1, season == "Printemps")
lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = datam)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité


datam <- filter(BacDino, cluster == 2, season == "Printemps")
lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = datam)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité



datam <- filter(BacDino, cluster == 3, season == "Printemps")
lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = datam)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité


datam <- filter(BacDino, cluster == 4,season == "Printemps")
lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = datam)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité


datam <- filter(BacDino, season == "Hiver")
lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = datam)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

datam <- filter(BacDino, cluster == 1, season == "Hiver")
lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = datam)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité


datam <- filter(BacDino, cluster == 2, season == "Hiver")
lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = datam)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité



datam <- filter(BacDino, cluster == 3, season == "Hiver")
lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = datam)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité


datam <- filter(BacDino, cluster == 4,season == "Hiver")
lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = datam)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité



datam <- filter(BacDino, season == "Automne")
lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = datam)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

datam <- filter(BacDino, cluster == 1, season == "Automne")
lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = datam)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité


datam <- filter(BacDino, cluster == 2, season == "Automne")
lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = datam)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité



datam <- filter(BacDino, cluster == 3, season == "Automne")
lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = datam)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité


datam <- filter(BacDino, cluster == 4,season == "Automne")
lda_model <- lda(Bloom_Phylum ~ TEMP+SALI+PO4+`NO3+NO2`+`TURB-FNU`+SIOH+NH4+OXYGENE, data = datam)

# Résumé du modèle
print(lda_model)

coefficients1 <- lda_model$scaling
coefficients1 <- as.data.frame(coefficients1)
coefficients1$Var <- rownames(coefficients1)
coefficients1 <- coefficients1[order(-abs(coefficients1$LD1)), ]
coefficients1$Var <- as.factor(coefficients1$Var)

ggplot(coefficients1, aes(x = reorder(Var, -abs(LD1)), y = abs(LD1))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD1 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité

coefficients2 <- lda_model$scaling
coefficients2 <- as.data.frame(coefficients2)
coefficients2$Var <- rownames(coefficients2)
coefficients2 <- coefficients2[order(-abs(coefficients1$LD1)), ]
coefficients2$Var <- as.factor(coefficients2$Var)

ggplot(coefficients2, aes(x = reorder(Var, -abs(LD2)), y = abs(LD2))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "LD2 - Importance des coefficients linéaires",
       x = "Facteurs environnementaux",
       y = "Coefficient linéaire") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x pour une meilleure lisibilité


BacDinoNB <- dplyr::select(BacDino,Code_point_Libelle,Date,Month,cluster,Year,season,CHLOROA:`TURB-FNU`,Bloom_Phylum) 

BacDinoNB_long <- pivot_longer(BacDinoNB,cols = CHLOROA:`TURB-FNU`,names_to = "var")

ggplot(BacDinoNB_long)+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum))+
  facet_wrap(~var,scales = "free_y",ncol=5)

ggplot(filter(BacDinoNB_long,cluster == 1))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#F8766D")+
  facet_wrap(~var,scales = "free_y",ncol=5)+
  labs(title = "Cluster 1")

ggplot(filter(BacDinoNB_long,cluster == 1 & season == "Ete"))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#F8766D")+
  facet_wrap(~var,scales = "free_y",ncol=5)+
  labs(title = "Cluster 1 - Ete")

ggplot(filter(BacDinoNB_long,cluster == 1 & season == "Printemps"))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#F8766D")+
  facet_wrap(~var,scales = "free_y",ncol=5)+
  labs(title = "Cluster 1 - Printemps")

ggplot(filter(BacDinoNB_long,cluster == 1 & season == "Automne"))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#F8766D")+
  facet_wrap(~var,scales = "free_y",ncol=5)+
  labs(title = "Cluster 1 - Automne")

ggplot(filter(BacDinoNB_long,cluster == 1 & season == "Hiver"))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#F8766D")+
  facet_wrap(~var,scales = "free_y",ncol=5)+
  labs(title = "Cluster 1 - Hiver")


ggplot(filter(BacDinoNB_long,cluster == 2))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#CD9600")+
  facet_wrap(~var,scales = "free_y",ncol=5)+
  labs(title = "Cluster 2")

ggplot(filter(BacDinoNB_long,cluster == 2 & season == "Ete"))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#CD9600")+
  facet_wrap(~var,scales = "free_y",ncol=5)+
  labs(title = "Cluster 2 - Ete")

ggplot(filter(BacDinoNB_long,cluster == 2 & season == "Printemps"))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#CD9600")+
  facet_wrap(~var,scales = "free_y",ncol=5)+
  labs(title = "Cluster 2 - Printemps")

ggplot(filter(BacDinoNB_long,cluster == 2 & season == "Automne"))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#CD9600")+
  facet_wrap(~var,scales = "free_y",ncol=5)+
  labs(title = "Cluster 2 - Automne")

ggplot(filter(BacDinoNB_long,cluster == 2 & season == "Hiver"))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#CD9600")+
  facet_wrap(~var,scales = "free_y",ncol=5)+
  labs(title = "Cluster 2 - Hiver")



ggplot(filter(BacDinoNB_long,cluster == 3))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#00BE67")+
  facet_wrap(~var,scales = "free_y",ncol=5)+
  labs(title = "Cluster 3")

ggplot(filter(BacDinoNB_long,cluster == 3 & season == "Ete"))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#00BE67")+
  facet_wrap(~var,scales = "free_y",ncol=5)+
  labs(title = "Cluster 3 - Ete")

ggplot(filter(BacDinoNB_long,cluster == 3 & season == "Printemps"))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#00BE67")+
  facet_wrap(~var,scales = "free_y",ncol=5)+
  labs(title = "Cluster 3 - Printemps")

ggplot(filter(BacDinoNB_long,cluster == 3 & season == "Automne"))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#00BE67")+
  facet_wrap(~var,scales = "free_y",ncol=5)+
  labs(title = "Cluster 3 - Automne")

ggplot(filter(BacDinoNB_long,cluster == 3 & season == "Hiver"))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#00BE67")+
  facet_wrap(~var,scales = "free_y",ncol=5)+
  labs(title = "Cluster 3 - Hiver")



ggplot(filter(BacDinoNB_long,cluster == 4))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#00A9FF")+
  facet_wrap(~var,scales = "free_y",ncol=5)+
  labs(title = "Cluster 4")

ggplot(filter(BacDinoNB_long,cluster == 4 & season == "Ete"))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#00A9FF")+
  facet_wrap(~var,scales = "free_y",ncol=5)+
  labs(title = "Cluster 4 - Ete")

ggplot(filter(BacDinoNB_long,cluster == 4 & season == "Printemps"))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#00A9FF")+
  facet_wrap(~var,scales = "free_y",ncol=5)+
  labs(title = "Cluster 4 - Printemps")

ggplot(filter(BacDinoNB_long,cluster == 4 & season == "Automne"))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#00A9FF")+
  facet_wrap(~var,scales = "free_y",ncol=5)+
  labs(title = "Cluster 4 - Automne")

ggplot(filter(BacDinoNB_long,cluster == 4 & season == "Hiver"))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#00A9FF")+
  facet_wrap(~var,scales = "free_y",ncol=5)+
  labs(title = "Cluster 4 - Hiver")

BacDinoNB1 <- filter(BacDinoNB,cluster == 1)
kruskal.test(BacDinoNB1$CHLOROA~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$CHLOROA~BacDinoNB1$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB1$NH4~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$NH4~BacDinoNB1$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB1$`NO3+NO2`~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$`NO3+NO2`~BacDinoNB1$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB1$OXYGENE~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$OXYGENE~BacDinoNB1$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB1$PO4~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$PO4~BacDinoNB1$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB1$SALI~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$SALI~BacDinoNB1$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB1$SIOH~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$SIOH~BacDinoNB1$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB1$TEMP~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$TEMP~BacDinoNB1$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB1$`TURB-FNU`~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$`TURB-FNU`~BacDinoNB1$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB1$TURB~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$TURB~BacDinoNB1$Bloom_Phylum,method = "BH")


BacDinoNB2 <- filter(BacDinoNB,cluster == 2)
kruskal.test(BacDinoNB2$CHLOROA~BacDinoNB2$Bloom_Phylum)
DunnTest(BacDinoNB2$CHLOROA~BacDinoNB2$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB2$NH4~BacDinoNB2$Bloom_Phylum)
DunnTest(BacDinoNB2$NH4~BacDinoNB2$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB2$`NO3+NO2`~BacDinoNB2$Bloom_Phylum)
DunnTest(BacDinoNB2$`NO3+NO2`~BacDinoNB2$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB2$OXYGENE~BacDinoNB2$Bloom_Phylum)
DunnTest(BacDinoNB2$OXYGENE~BacDinoNB2$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB2$PO4~BacDinoNB2$Bloom_Phylum)
DunnTest(BacDinoNB2$PO4~BacDinoNB2$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB2$SALI~BacDinoNB2$Bloom_Phylum)
DunnTest(BacDinoNB2$SALI~BacDinoNB2$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB2$SIOH~BacDinoNB2$Bloom_Phylum)
DunnTest(BacDinoNB2$SIOH~BacDinoNB2$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB2$TEMP~BacDinoNB2$Bloom_Phylum)
DunnTest(BacDinoNB2$TEMP~BacDinoNB2$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB2$`TURB-FNU`~BacDinoNB2$Bloom_Phylum)
DunnTest(BacDinoNB2$`TURB-FNU`~BacDinoNB2$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB2$TURB~BacDinoNB2$Bloom_Phylum)
DunnTest(BacDinoNB2$TURB~BacDinoNB2$Bloom_Phylum,method = "BH")


BacDinoNB3 <- filter(BacDinoNB,cluster == 3)
kruskal.test(BacDinoNB3$CHLOROA~BacDinoNB3$Bloom_Phylum)
DunnTest(BacDinoNB3$CHLOROA~BacDinoNB3$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB3$NH4~BacDinoNB3$Bloom_Phylum)
DunnTest(BacDinoNB3$NH4~BacDinoNB3$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB3$`NO3+NO2`~BacDinoNB3$Bloom_Phylum)
DunnTest(BacDinoNB3$`NO3+NO2`~BacDinoNB3$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB3$OXYGENE~BacDinoNB3$Bloom_Phylum)
DunnTest(BacDinoNB3$OXYGENE~BacDinoNB3$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB3$PO4~BacDinoNB3$Bloom_Phylum)
DunnTest(BacDinoNB3$PO4~BacDinoNB3$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB3$SALI~BacDinoNB3$Bloom_Phylum)
DunnTest(BacDinoNB3$SALI~BacDinoNB3$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB3$SIOH~BacDinoNB3$Bloom_Phylum)
DunnTest(BacDinoNB3$SIOH~BacDinoNB3$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB3$TEMP~BacDinoNB3$Bloom_Phylum)
DunnTest(BacDinoNB3$TEMP~BacDinoNB3$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB3$`TURB-FNU`~BacDinoNB3$Bloom_Phylum)
DunnTest(BacDinoNB3$`TURB-FNU`~BacDinoNB3$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB3$TURB~BacDinoNB3$Bloom_Phylum)
DunnTest(BacDinoNB3$TURB~BacDinoNB3$Bloom_Phylum,method = "BH")


BacDinoNB4 <- filter(BacDinoNB,cluster == 4)
kruskal.test(BacDinoNB4$CHLOROA~BacDinoNB4$Bloom_Phylum)
DunnTest(BacDinoNB4$CHLOROA~BacDinoNB4$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB4$NH4~BacDinoNB4$Bloom_Phylum)
DunnTest(BacDinoNB4$NH4~BacDinoNB4$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB4$`NO3+NO2`~BacDinoNB4$Bloom_Phylum)
DunnTest(BacDinoNB4$`NO3+NO2`~BacDinoNB4$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB4$OXYGENE~BacDinoNB4$Bloom_Phylum)
DunnTest(BacDinoNB4$OXYGENE~BacDinoNB4$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB4$PO4~BacDinoNB4$Bloom_Phylum)
DunnTest(BacDinoNB4$PO4~BacDinoNB4$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB4$SALI~BacDinoNB4$Bloom_Phylum)
DunnTest(BacDinoNB4$SALI~BacDinoNB4$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB4$SIOH~BacDinoNB4$Bloom_Phylum)
DunnTest(BacDinoNB4$SIOH~BacDinoNB4$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB4$TEMP~BacDinoNB4$Bloom_Phylum)
DunnTest(BacDinoNB4$TEMP~BacDinoNB4$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB4$`TURB-FNU`~BacDinoNB4$Bloom_Phylum)
DunnTest(BacDinoNB4$`TURB-FNU`~BacDinoNB4$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB4$TURB~BacDinoNB4$Bloom_Phylum)
DunnTest(BacDinoNB4$TURB~BacDinoNB4$Bloom_Phylum,method = "BH")


BacDinoNBA <- filter(BacDinoNB)
kruskal.test(BacDinoNBA$CHLOROA~BacDinoNBA$Bloom_Phylum)
DunnTest(BacDinoNBA$CHLOROA~BacDinoNBA$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNBA$NH4~BacDinoNBA$Bloom_Phylum)
DunnTest(BacDinoNBA$NH4~BacDinoNBA$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNBA$`NO3+NO2`~BacDinoNBA$Bloom_Phylum)
DunnTest(BacDinoNBA$`NO3+NO2`~BacDinoNBA$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNBA$OXYGENE~BacDinoNBA$Bloom_Phylum)
DunnTest(BacDinoNBA$OXYGENE~BacDinoNBA$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNBA$PO4~BacDinoNBA$Bloom_Phylum)
DunnTest(BacDinoNBA$PO4~BacDinoNBA$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNBA$SALI~BacDinoNBA$Bloom_Phylum)
DunnTest(BacDinoNBA$SALI~BacDinoNBA$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNBA$SIOH~BacDinoNBA$Bloom_Phylum)
DunnTest(BacDinoNBA$SIOH~BacDinoNBA$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNBA$TEMP~BacDinoNBA$Bloom_Phylum)
DunnTest(BacDinoNBA$TEMP~BacDinoNBA$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNBA$`TURB-FNU`~BacDinoNBA$Bloom_Phylum)
DunnTest(BacDinoNBA$`TURB-FNU`~BacDinoNBA$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNBA$TURB~BacDinoNBA$Bloom_Phylum)
DunnTest(BacDinoNBA$TURB~BacDinoNBA$Bloom_Phylum,method = "BH")






BacDino$Rspe <- rowSums(BacDino[,c(24:247)] != 0,na.rm = T)
BacDinoNB <- dplyr::select(BacDino,Code_point_Libelle,Date,Month,cluster,Year,season,Shannon,BergerParker, Pielou,Rspe,Bloom_Phylum) 

BacDinoNB_long <- pivot_longer(BacDinoNB,cols = c(Shannon,BergerParker, Pielou,Rspe) ,names_to = "var")

ggplot(BacDinoNB_long)+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum))+
  facet_wrap(~var,scales = "free_y",ncol=5)

ggplot(filter(BacDinoNB_long,cluster == 1))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#F8766D")+
  facet_wrap(~var,scales = "free_y",ncol=5)+
  labs(title = "Cluster 1")


ggplot(filter(BacDinoNB_long,cluster == 2))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#CD9600")+
  facet_wrap(~var,scales = "free_y",ncol=5)+
  labs(title = "Cluster 2")




ggplot(filter(BacDinoNB_long,cluster == 3))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#00BE67")+
  facet_wrap(~var,scales = "free_y",ncol=5)+
  labs(title = "Cluster 3")


ggplot(filter(BacDinoNB_long,cluster == 4))+
  geom_boxplot(aes(x=Bloom_Phylum,y=value,group=Bloom_Phylum),fill="#00A9FF")+
  facet_wrap(~var,scales = "free_y",ncol=5)+
  labs(title = "Cluster 4")

kruskal.test(BacDinoNB$BergerParker~BacDinoNB$Bloom_Phylum)
DunnTest(BacDinoNB$BergerParker~BacDinoNB$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB$Pielou~BacDinoNB$Bloom_Phylum)
DunnTest(BacDinoNB$Pielou~BacDinoNB$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB$Rspe~BacDinoNB$Bloom_Phylum)
DunnTest(BacDinoNB$Rspe~BacDinoNB$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB$Shannon~BacDinoNB$Bloom_Phylum)
DunnTest(BacDinoNB$Shannon~BacDinoNB$Bloom_Phylum,method = "BH")

BacDinoNB1 <- filter(BacDinoNB,cluster == 1)
kruskal.test(BacDinoNB1$BergerParker~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$BergerParker~BacDinoNB1$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB1$Pielou~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$Pielou~BacDinoNB1$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB1$Rspe~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$Rspe~BacDinoNB1$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB1$Shannon~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$Shannon~BacDinoNB1$Bloom_Phylum,method = "BH")

BacDinoNB1 <- filter(BacDinoNB,cluster == 2)
kruskal.test(BacDinoNB1$BergerParker~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$BergerParker~BacDinoNB1$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB1$Pielou~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$Pielou~BacDinoNB1$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB1$Rspe~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$Rspe~BacDinoNB1$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB1$Shannon~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$Shannon~BacDinoNB1$Bloom_Phylum,method = "BH")

BacDinoNB1 <- filter(BacDinoNB,cluster == 3)
kruskal.test(BacDinoNB1$BergerParker~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$BergerParker~BacDinoNB1$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB1$Pielou~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$Pielou~BacDinoNB1$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB1$Rspe~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$Rspe~BacDinoNB1$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB1$Shannon~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$Shannon~BacDinoNB1$Bloom_Phylum,method = "BH")

BacDinoNB1 <- filter(BacDinoNB,cluster == 4)
kruskal.test(BacDinoNB1$BergerParker~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$BergerParker~BacDinoNB1$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB1$Pielou~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$Pielou~BacDinoNB1$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB1$Rspe~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$Rspe~BacDinoNB1$Bloom_Phylum,method = "BH")

kruskal.test(BacDinoNB1$Shannon~BacDinoNB1$Bloom_Phylum)
DunnTest(BacDinoNB1$Shannon~BacDinoNB1$Bloom_Phylum,method = "BH")

