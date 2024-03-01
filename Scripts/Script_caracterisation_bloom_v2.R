bloom <- read_delim("data_modif/Table_bloom_R_v2.csv", 
                                  delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                                  trim_ws = TRUE)

data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

data$Abdtot <- apply(data[,248:265],1,sum,na.rm=T)


data_bloom <- left_join(bloom,dplyr::select(data, -c(cluster,CHLOROA,BergerParker))
, join_by(Date,Code_point_Libelle))

data_bloom <- data_bloom |>
  mutate(Month = month(Date, label = F))
data_bloom <- data_bloom |>
  mutate(Year = lubridate::year(Date))

data_bloom <- 
  data_bloom %>% 
  mutate(season = case_when(Month %in% c(12, 01, 02) ~ "Winter",
                            Month %in% c(03, 04, 05) ~ "Spring",
                            Month %in% c(06, 07, 08) ~ "Summer", 
                            Month %in% c(09, 10, 11) ~ "Fall", TRUE ~ NA_character_))

data_bloom <- dplyr::select(data_bloom,Code_point_Libelle, cluster, Date,Year, Month,season,  Shannon, Pielou, BergerParker,CHLOROA,Abdtot,P_dominance,Bloom,Bloom_Phylum, Bloom_Genre)
data_bloom <- data_bloom[c(1:156),]
data_bloom[2,8] <- 0


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


write.csv2(data_bloom,file="data_modif/Table_bloom_Excel_v2.csv", row.names = FALSE,dec = ".")

# Description des blooms
descript <- filter(data_bloom, Bloom_Phylum == "Cryptophyceae" & cluster == 1)

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

descript <- filter(data_bloom, Bloom_Phylum == "Cryptophyceae")

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

