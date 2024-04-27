# Loading packages
library(readr)
library(interp)
library("pastecs")
library("stlplus")
library(trend)
library(dplyr)
library(ggplot2)
library(corrplot)
# Import data and make it nice
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)


data <- dplyr::select(data,Code_point_Libelle,ID.interne.passage, Date,CHLOROA:`TURB-FNU`,Shannon:Pielou,Rspe,Bacillariophyceae,Dinophyceae,Cryptophyceae,Haptophyta,Ciliophora)

data_trend <- data 

metric <- read_delim("data_modif/metrics_final.csv", 
                     delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                     locale = locale(decimal_mark = ",", grouping_mark = "."), 
                     trim_ws = TRUE)
metric[metric == Inf] <- NA

metric_trend <- metric 

trends <- left_join(data_trend,metric_trend)

# There are duplicates to delete
doublons <- trends[duplicated(trends$ID.interne.passage) |
                     duplicated(trends$ID.interne.passage, fromLast = TRUE), ]
resultat_filtre <- doublons[1,]

data_unique <- subset(trends, !(ID.interne.passage %in% unique(doublons$ID.interne.passage)))
trends <- bind_rows(data_unique,resultat_filtre)
trends <- trends |>
  arrange(Code_point_Libelle, Date)


write.csv2(trends,file="data_modif/datafortrends_final.csv", row.names = FALSE,dec = ".")




# Create the function to detect outliers

outliers <- function(x) {
  median_dev <- abs(x - median(x,na.rm = T))
  indexes <- which(median_dev > (2.323561*1.486) * mad(x,na.rm = T))
  return(indexes)
}

# Import data
data <- read_delim("data_modif/datafortrends_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                    grouping_mark = ""), trim_ws = TRUE)

datab <- dplyr::select(data,Code_point_Libelle, Date,CHLOROA:Ciliophora,N_noeuds:cluster)

# Loop to regularize, deseasonalize, detect shifts, trends and some #####

# Temperature + Salinite + CHLOROA
for (k in c(3:5)){
  data <- dplyr::select(datab,Code_point_Libelle,Date,colnames(datab)[k])
  var <- colnames(datab)[k]
for (i in c(1:21)){
  station <- levels(as.factor(data$Code_point_Libelle))[i]
  Table <- filter(data, Code_point_Libelle == station)
  #windows(title = "A regulariser")
  #print(ggplot(Table) + geom_path(aes(x=Date, y=TEMP)) + geom_rug(aes(x=Date)))
  Table <- Table[order(Table$Date), ]
  
  interval <- as.numeric(mean(diff(Table$Date)))
  tmin <- min(Table$Date)
  t_reg <- regul(x=Table$Date, y=Table[,3], xmin=tmin, n=nrow(Table), deltat=interval)
  #windows(title = "regul step 1")
  #plot(t_reg)
  data_regul_ini <- regul.screen(x=as.numeric(Table$Date),
                                 xmin=seq(from=as.numeric(tmin-10), to=as.numeric(tmin+10), by=1),
                                 deltat=seq(from=10, to=20, by=1),
                                 tol = 3
  )
  data_regul <- as.data.frame(data_regul_ini$nbr.match)
  max_index <- which(data_regul == max(data_regul, na.rm = TRUE), arr.ind = TRUE)
  newtmin <- as.Date(as.numeric(sub(".*=", "", rownames(data_regul)[max_index[1, 1]])))
  newinterval <- as.numeric(sub(".*=", "", colnames(data_regul)[max_index[1, 2]]))
  data_regul_n <- as.data.frame(data_regul_ini$n)
  new_n <- data_regul_n[max_index[1, 1],max_index[1, 2]]
  t_reg <- regul(x=Table$Date, y=Table[,3], xmin=newtmin, n=new_n, deltat=newinterval,method="linear")
  #windows(title = "regul step 2")
  #plot(t_reg)
  Table_regul <- as.data.frame(cbind(t_reg$x,t_reg$y))
  colnames(Table_regul) <- c("Date",var)
  #windows(title = "Regulated ts")
  #plot(Table_regul,type="o")
  var_ts <- extract(t_reg)
  #plot(var_ts, type="o")
  acf_var <- acf(var_ts,na.action = na.omit,plot = F)
  ACF_data <- as.data.frame(cbind(acf_var$acf,acf_var$lag))
  lag_data <- filter(ACF_data,V1<0.20)[1,2]
  #cat("Autocorrelation lag pour",station,lag_data,"
  #    ")
  
  Table_regul$year <- format(Table_regul$Date, "%Y")
  sampling <- count(Table_regul, year)
  np <- median(sampling[,2])
  
  dec <- stlplus(Table_regul[,2],t=Table_regul$Date, n.p=np, s.window="periodic", t.window=NULL)
  plot(dec, scales=list(y="free"))
  Table_regul$var_noseason <- dec$data$raw - dec$data$seasonal
  
  
  tendtest <- mk.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
  residual <- dec$data$remainder
  plot(acf(residual,na.action = na.pass,plot=F),main = paste0("Structure residu pour ",station))
  
  outliers_var<- outliers(Table_regul$var_noseason)
  #windows(title = "Outlyers")
  plot(y=Table_regul$var_noseason,x=Table_regul$Date, type="l",main = paste0("Outliers pour ",station))
  points(
    # the x coordinate is the index
    x=Table_regul[outliers_var,]$Date,
    # the y coordinate is the value
    y=Table_regul$var_noseason[outliers_var],
    # make points solid and red
    pch=16, col="red")
  nom_fichier <- paste0("Outliers_regularise_desaisonalise",var,station)
  nom_fichier <- paste0(nom_fichier)
  Table_regul$Code_point_Libelle <- station
  #Table_regul$Outlier <- "NON"
  #Table_regul[outliers_TEMP,]$Outlier <- "OUI"
  Table_regul$Changepoint <- NA
  changetest <- pettitt.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
  changepoint <- as.numeric(changetest$estimate)
  Table_regul[changepoint,]$Changepoint <- changetest$p.value
  Table_regul$Trend <- tendtest[["estimates"]][["tau"]]
  Table_regul$pvaltrend <- tendtest[["p.value"]]
  Table_regul$pvalchange <- changetest$p.value
  Sensslope <- sens.slope(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
  Table_regul$psens <- Sensslope$p.value
  Table_regul$slopesens <- as.numeric(Sensslope$estimates)
  Table_regul$interval <- newinterval
  
  cat(var,station,"Tau",tendtest[["estimates"]][["tau"]],"pval",tendtest[["p.value"]],"sens",as.numeric(Sensslope$estimates),mean(diff(Table_regul$Date[complete.cases(Table_regul$var_noseason)])),"\n")
  write.csv2(Table_regul,file=paste0("data_trends/",nom_fichier,".csv"), row.names = FALSE,dec = ".")
  
}
}


## NH4
for (k in c(7)){
  data <- dplyr::select(datab,Code_point_Libelle,Date,colnames(datab)[k])
  var <- colnames(datab)[k]
  for (i in c(1:5,7:17,19:21)){
    station <- levels(as.factor(data$Code_point_Libelle))[i]
    Table <- filter(data, Code_point_Libelle == station)
    #windows(title = "A regulariser")
    #print(ggplot(Table) + geom_path(aes(x=Date, y=TEMP)) + geom_rug(aes(x=Date)))
    Table <- Table[order(Table$Date), ]
    
    interval <- as.numeric(mean(diff(Table$Date)))
    tmin <- min(Table$Date)
    t_reg <- regul(x=Table$Date, y=Table[,3], xmin=tmin, n=nrow(Table), deltat=interval)
    #windows(title = "regul step 1")
    #plot(t_reg)
    data_regul_ini <- regul.screen(x=as.numeric(Table$Date),
                                   xmin=seq(from=as.numeric(tmin-10), to=as.numeric(tmin+10), by=1),
                                   deltat=seq(from=10, to=20, by=1),
                                   tol = 3
    )
    data_regul <- as.data.frame(data_regul_ini$nbr.match)
    max_index <- which(data_regul == max(data_regul, na.rm = TRUE), arr.ind = TRUE)
    newtmin <- as.Date(as.numeric(sub(".*=", "", rownames(data_regul)[max_index[1, 1]])))
    newinterval <- as.numeric(sub(".*=", "", colnames(data_regul)[max_index[1, 2]]))
    data_regul_n <- as.data.frame(data_regul_ini$n)
    new_n <- data_regul_n[max_index[1, 1],max_index[1, 2]]
    t_reg <- regul(x=Table$Date, y=Table[,3], xmin=newtmin, n=new_n, deltat=newinterval,method="linear")
    #windows(title = "regul step 2")
    #plot(t_reg)
    Table_regul <- as.data.frame(cbind(t_reg$x,t_reg$y))
    colnames(Table_regul) <- c("Date",var)
    #windows(title = "Regulated ts")
    #plot(Table_regul,type="o")
    var_ts <- extract(t_reg)
    #plot(var_ts, type="o")
    acf_var <- acf(var_ts,na.action = na.omit,plot = F)
    ACF_data <- as.data.frame(cbind(acf_var$acf,acf_var$lag))
    lag_data <- filter(ACF_data,V1<0.20)[1,2]
    cat("Autocorrelation lag pour",station,lag_data,"
      ")
    
    Table_regul$year <- format(Table_regul$Date, "%Y")
    sampling <- count(Table_regul, year)
    np <- median(sampling[,2])
    
    dec <- stlplus(Table_regul[,2],t=Table_regul$Date, n.p=np, s.window="periodic", t.window=NULL)
    plot(dec, scales=list(y="free"))
    Table_regul$var_noseason <- dec$data$raw - dec$data$seasonal
    
    
    tendtest <- mk.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    cat("Tendance de",tendtest[["estimates"]][["tau"]],"pour ",station,"
      ")
    residual <- dec$data$remainder
    plot(acf(residual,na.action = na.pass,plot=F),main = paste0("Structure residu pour ",station))
    
    outliers_var<- outliers(Table_regul$var_noseason)
    #windows(title = "Outlyers")
    plot(y=Table_regul$var_noseason,x=Table_regul$Date, type="l",main = paste0("Outliers pour ",station))
    points(
      # the x coordinate is the index
      x=Table_regul[outliers_var,]$Date,
      # the y coordinate is the value
      y=Table_regul$var_noseason[outliers_var],
      # make points solid and red
      pch=16, col="red")
    nom_fichier <- paste0("Outliers_regularise_desaisonalise",var,station)
    nom_fichier <- paste0(nom_fichier)
    Table_regul$Code_point_Libelle <- station
    #Table_regul$Outlier <- "NON"
    #Table_regul[outliers_TEMP,]$Outlier <- "OUI"
    Table_regul$Changepoint <- NA
    changetest <- pettitt.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    changepoint <- as.numeric(changetest$estimate)
    Table_regul[changepoint,]$Changepoint <- changetest$p.value
    Table_regul$Trend <- tendtest[["estimates"]][["tau"]]
    Table_regul$pvaltrend <- tendtest[["p.value"]]
    Table_regul$pvalchange <- changetest$p.value
    Sensslope <- sens.slope(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    Table_regul$psens <- Sensslope$p.value
    Table_regul$slopesens <- as.numeric(Sensslope$estimates)
    Table_regul$interval <- newinterval
    write.csv2(Table_regul,file=paste0("data_trends/",nom_fichier,".csv"), row.names = FALSE,dec = ".")
    
  }
}

## PO4
for (k in c(8)){
  data <- dplyr::select(datab,Code_point_Libelle,Date,colnames(datab)[k])
  var <- colnames(datab)[k]
  for (i in c(1:5,7:17,19:21)){
    station <- levels(as.factor(data$Code_point_Libelle))[i]
    Table <- filter(data, Code_point_Libelle == station)
    #windows(title = "A regulariser")
    #print(ggplot(Table) + geom_path(aes(x=Date, y=TEMP)) + geom_rug(aes(x=Date)))
    Table <- Table[order(Table$Date), ]
    
    interval <- as.numeric(mean(diff(Table$Date)))
    tmin <- min(Table$Date)
    t_reg <- regul(x=Table$Date, y=Table[,3], xmin=tmin, n=nrow(Table), deltat=interval)
    #windows(title = "regul step 1")
    #plot(t_reg)
    data_regul_ini <- regul.screen(x=as.numeric(Table$Date),
                                   xmin=seq(from=as.numeric(tmin-10), to=as.numeric(tmin+10), by=1),
                                   deltat=seq(from=10, to=20, by=1),
                                   tol = 3
    )
    data_regul <- as.data.frame(data_regul_ini$nbr.match)
    max_index <- which(data_regul == max(data_regul, na.rm = TRUE), arr.ind = TRUE)
    newtmin <- as.Date(as.numeric(sub(".*=", "", rownames(data_regul)[max_index[1, 1]])))
    newinterval <- as.numeric(sub(".*=", "", colnames(data_regul)[max_index[1, 2]]))
    data_regul_n <- as.data.frame(data_regul_ini$n)
    new_n <- data_regul_n[max_index[1, 1],max_index[1, 2]]
    t_reg <- regul(x=Table$Date, y=Table[,3], xmin=newtmin, n=new_n, deltat=newinterval,method="linear")
    #windows(title = "regul step 2")
    #plot(t_reg)
    Table_regul <- as.data.frame(cbind(t_reg$x,t_reg$y))
    colnames(Table_regul) <- c("Date",var)
    #windows(title = "Regulated ts")
    #plot(Table_regul,type="o")
    var_ts <- extract(t_reg)
    #plot(var_ts, type="o")
    acf_var <- acf(var_ts,na.action = na.omit,plot = F)
    ACF_data <- as.data.frame(cbind(acf_var$acf,acf_var$lag))
    lag_data <- filter(ACF_data,V1<0.20)[1,2]
    cat("Autocorrelation lag pour",station,lag_data,"
      ")
    
    Table_regul$year <- format(Table_regul$Date, "%Y")
    sampling <- count(Table_regul, year)
    np <- median(sampling[,2])
    
    dec <- stlplus(Table_regul[,2],t=Table_regul$Date, n.p=np, s.window="periodic", t.window=NULL)
    plot(dec, scales=list(y="free"))
    Table_regul$var_noseason <- dec$data$raw - dec$data$seasonal
    
    
    tendtest <- mk.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    cat("Tendance de",tendtest[["estimates"]][["tau"]],"pour ",station,"
      ")
    residual <- dec$data$remainder
    plot(acf(residual,na.action = na.pass,plot=F),main = paste0("Structure residu pour ",station))
    
    outliers_var<- outliers(Table_regul$var_noseason)
    #windows(title = "Outlyers")
    plot(y=Table_regul$var_noseason,x=Table_regul$Date, type="l",main = paste0("Outliers pour ",station))
    points(
      # the x coordinate is the index
      x=Table_regul[outliers_var,]$Date,
      # the y coordinate is the value
      y=Table_regul$var_noseason[outliers_var],
      # make points solid and red
      pch=16, col="red")
    nom_fichier <- paste0("Outliers_regularise_desaisonalise",var,station)
    nom_fichier <- paste0(nom_fichier)
    Table_regul$Code_point_Libelle <- station
    #Table_regul$Outlier <- "NON"
    #Table_regul[outliers_TEMP,]$Outlier <- "OUI"
    Table_regul$Changepoint <- NA
    changetest <- pettitt.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    changepoint <- as.numeric(changetest$estimate)
    Table_regul[changepoint,]$Changepoint <- changetest$p.value
    Table_regul$Trend <- tendtest[["estimates"]][["tau"]]
    Table_regul$pvaltrend <- tendtest[["p.value"]]
    Table_regul$pvalchange <- changetest$p.value
    Sensslope <- sens.slope(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    Table_regul$psens <- Sensslope$p.value
    Table_regul$slopesens <- as.numeric(Sensslope$estimates)
    Table_regul$interval <- newinterval
    write.csv2(Table_regul,file=paste0("data_trends/",nom_fichier,".csv"), row.names = FALSE,dec = ".")
    
  }
}

# SIOH
for (k in c(9)){
  data <- dplyr::select(datab,Code_point_Libelle,Date,colnames(datab)[k])
  var <- colnames(datab)[k]
  for (i in c(1:5,7:17,19:21)){
    station <- levels(as.factor(data$Code_point_Libelle))[i]
    Table <- filter(data, Code_point_Libelle == station)
    #windows(title = "A regulariser")
    #print(ggplot(Table) + geom_path(aes(x=Date, y=TEMP)) + geom_rug(aes(x=Date)))
    Table <- Table[order(Table$Date), ]
    
    interval <- as.numeric(mean(diff(Table$Date)))
    tmin <- min(Table$Date)
    t_reg <- regul(x=Table$Date, y=Table[,3], xmin=tmin, n=nrow(Table), deltat=interval)
    #windows(title = "regul step 1")
    #plot(t_reg)
    data_regul_ini <- regul.screen(x=as.numeric(Table$Date),
                                   xmin=seq(from=as.numeric(tmin-10), to=as.numeric(tmin+10), by=1),
                                   deltat=seq(from=10, to=20, by=1),
                                   tol = 3
    )
    data_regul <- as.data.frame(data_regul_ini$nbr.match)
    max_index <- which(data_regul == max(data_regul, na.rm = TRUE), arr.ind = TRUE)
    newtmin <- as.Date(as.numeric(sub(".*=", "", rownames(data_regul)[max_index[1, 1]])))
    newinterval <- as.numeric(sub(".*=", "", colnames(data_regul)[max_index[1, 2]]))
    data_regul_n <- as.data.frame(data_regul_ini$n)
    new_n <- data_regul_n[max_index[1, 1],max_index[1, 2]]
    t_reg <- regul(x=Table$Date, y=Table[,3], xmin=newtmin, n=new_n, deltat=newinterval,method="linear")
    #windows(title = "regul step 2")
    #plot(t_reg)
    Table_regul <- as.data.frame(cbind(t_reg$x,t_reg$y))
    colnames(Table_regul) <- c("Date",var)
    #windows(title = "Regulated ts")
    #plot(Table_regul,type="o")
    var_ts <- extract(t_reg)
    #plot(var_ts, type="o")
    acf_var <- acf(var_ts,na.action = na.omit,plot = F)
    ACF_data <- as.data.frame(cbind(acf_var$acf,acf_var$lag))
    lag_data <- filter(ACF_data,V1<0.20)[1,2]
    cat("Autocorrelation lag pour",station,lag_data,"
      ")
    
    Table_regul$year <- format(Table_regul$Date, "%Y")
    sampling <- count(Table_regul, year)
    np <- median(sampling[,2])
    
    dec <- stlplus(Table_regul[,2],t=Table_regul$Date, n.p=np, s.window="periodic", t.window=NULL)
    plot(dec, scales=list(y="free"))
    Table_regul$var_noseason <- dec$data$raw - dec$data$seasonal
    
    
    tendtest <- mk.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    cat("Tendance de",tendtest[["estimates"]][["tau"]],"pour ",station,"
      ")
    residual <- dec$data$remainder
    plot(acf(residual,na.action = na.pass,plot=F),main = paste0("Structure residu pour ",station))
    
    outliers_var<- outliers(Table_regul$var_noseason)
    #windows(title = "Outlyers")
    plot(y=Table_regul$var_noseason,x=Table_regul$Date, type="l",main = paste0("Outliers pour ",station))
    points(
      # the x coordinate is the index
      x=Table_regul[outliers_var,]$Date,
      # the y coordinate is the value
      y=Table_regul$var_noseason[outliers_var],
      # make points solid and red
      pch=16, col="red")
    nom_fichier <- paste0("Outliers_regularise_desaisonalise",var,station)
    nom_fichier <- paste0(nom_fichier)
    Table_regul$Code_point_Libelle <- station
    #Table_regul$Outlier <- "NON"
    #Table_regul[outliers_TEMP,]$Outlier <- "OUI"
    Table_regul$Changepoint <- NA
    changetest <- pettitt.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    changepoint <- as.numeric(changetest$estimate)
    Table_regul[changepoint,]$Changepoint <- changetest$p.value
    Table_regul$Trend <- tendtest[["estimates"]][["tau"]]
    Table_regul$pvaltrend <- tendtest[["p.value"]]
    Table_regul$pvalchange <- changetest$p.value
    Sensslope <- sens.slope(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    Table_regul$psens <- Sensslope$p.value
    Table_regul$slopesens <- as.numeric(Sensslope$estimates)
    Table_regul$interval <- newinterval
    write.csv2(Table_regul,file=paste0("data_trends/",nom_fichier,".csv"), row.names = FALSE,dec = ".")
    
  }
}

# OXYGENE
for (k in c(10)){
  data <- dplyr::select(datab,Code_point_Libelle,Date,colnames(datab)[k])
  var <- colnames(datab)[k]
  for (i in c(1:21)){
    station <- levels(as.factor(data$Code_point_Libelle))[i]
    Table <- filter(data, Code_point_Libelle == station)
    #windows(title = "A regulariser")
    #print(ggplot(Table) + geom_path(aes(x=Date, y=TEMP)) + geom_rug(aes(x=Date)))
    Table <- Table[order(Table$Date), ]
    
    interval <- as.numeric(mean(diff(Table$Date)))
    tmin <- min(Table$Date)
    t_reg <- regul(x=Table$Date, y=Table[,3], xmin=tmin, n=nrow(Table), deltat=interval)
    #windows(title = "regul step 1")
    #plot(t_reg)
    data_regul_ini <- regul.screen(x=as.numeric(Table$Date),
                                   xmin=seq(from=as.numeric(tmin-10), to=as.numeric(tmin+10), by=1),
                                   deltat=seq(from=10, to=20, by=1),
                                   tol = 3
    )
    data_regul <- as.data.frame(data_regul_ini$nbr.match)
    max_index <- which(data_regul == max(data_regul, na.rm = TRUE), arr.ind = TRUE)
    newtmin <- as.Date(as.numeric(sub(".*=", "", rownames(data_regul)[max_index[1, 1]])))
    newinterval <- as.numeric(sub(".*=", "", colnames(data_regul)[max_index[1, 2]]))
    data_regul_n <- as.data.frame(data_regul_ini$n)
    new_n <- data_regul_n[max_index[1, 1],max_index[1, 2]]
    t_reg <- regul(x=Table$Date, y=Table[,3], xmin=newtmin, n=new_n, deltat=newinterval,method="linear")
    #windows(title = "regul step 2")
    #plot(t_reg)
    Table_regul <- as.data.frame(cbind(t_reg$x,t_reg$y))
    colnames(Table_regul) <- c("Date",var)
    #windows(title = "Regulated ts")
    #plot(Table_regul,type="o")
    var_ts <- extract(t_reg)
    #plot(var_ts, type="o")
    acf_var <- acf(var_ts,na.action = na.omit,plot = F)
    ACF_data <- as.data.frame(cbind(acf_var$acf,acf_var$lag))
    lag_data <- filter(ACF_data,V1<0.20)[1,2]
    cat("Autocorrelation lag pour",station,lag_data,"
      ")
    
    Table_regul$year <- format(Table_regul$Date, "%Y")
    sampling <- count(Table_regul, year)
    np <- median(sampling[,2])
    
    dec <- stlplus(Table_regul[,2],t=Table_regul$Date, n.p=np, s.window="periodic", t.window=NULL)
    plot(dec, scales=list(y="free"))
    Table_regul$var_noseason <- dec$data$raw - dec$data$seasonal
    
    
    tendtest <- mk.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    cat("Tendance de",tendtest[["estimates"]][["tau"]],"pour ",station,"
      ")
    residual <- dec$data$remainder
    plot(acf(residual,na.action = na.pass,plot=F),main = paste0("Structure residu pour ",station))
    
    outliers_var<- outliers(Table_regul$var_noseason)
    #windows(title = "Outlyers")
    plot(y=Table_regul$var_noseason,x=Table_regul$Date, type="l",main = paste0("Outliers pour ",station))
    points(
      # the x coordinate is the index
      x=Table_regul[outliers_var,]$Date,
      # the y coordinate is the value
      y=Table_regul$var_noseason[outliers_var],
      # make points solid and red
      pch=16, col="red")
    nom_fichier <- paste0("Outliers_regularise_desaisonalise",var,station)
    nom_fichier <- paste0(nom_fichier)
    Table_regul$Code_point_Libelle <- station
    #Table_regul$Outlier <- "NON"
    #Table_regul[outliers_TEMP,]$Outlier <- "OUI"
    Table_regul$Changepoint <- NA
    changetest <- pettitt.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    changepoint <- as.numeric(changetest$estimate)
    Table_regul[changepoint,]$Changepoint <- changetest$p.value
    Table_regul$Trend <- tendtest[["estimates"]][["tau"]]
    Table_regul$pvaltrend <- tendtest[["p.value"]]
    Table_regul$pvalchange <- changetest$p.value
    Sensslope <- sens.slope(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    Table_regul$psens <- Sensslope$p.value
    Table_regul$slopesens <- as.numeric(Sensslope$estimates)
    Table_regul$interval <- newinterval
    write.csv2(Table_regul,file=paste0("data_trends/",nom_fichier,".csv"), row.names = FALSE,dec = ".")
    
  }
}


# NO3+NO2
for (k in c(11)){
  data <- dplyr::select(datab,Code_point_Libelle,Date,colnames(datab)[k])
  var <- colnames(datab)[k]
  for (i in c(1:5,7:17,19:21)){
    station <- levels(as.factor(data$Code_point_Libelle))[i]
    Table <- filter(data, Code_point_Libelle == station)
    #windows(title = "A regulariser")
    #print(ggplot(Table) + geom_path(aes(x=Date, y=TEMP)) + geom_rug(aes(x=Date)))
    Table <- Table[order(Table$Date), ]
    
    interval <- as.numeric(mean(diff(Table$Date)))
    tmin <- min(Table$Date)
    t_reg <- regul(x=Table$Date, y=Table[,3], xmin=tmin, n=nrow(Table), deltat=interval)
    #windows(title = "regul step 1")
    #plot(t_reg)
    data_regul_ini <- regul.screen(x=as.numeric(Table$Date),
                                   xmin=seq(from=as.numeric(tmin-10), to=as.numeric(tmin+10), by=1),
                                   deltat=seq(from=10, to=20, by=1),
                                   tol = 3
    )
    data_regul <- as.data.frame(data_regul_ini$nbr.match)
    max_index <- which(data_regul == max(data_regul, na.rm = TRUE), arr.ind = TRUE)
    newtmin <- as.Date(as.numeric(sub(".*=", "", rownames(data_regul)[max_index[1, 1]])))
    newinterval <- as.numeric(sub(".*=", "", colnames(data_regul)[max_index[1, 2]]))
    data_regul_n <- as.data.frame(data_regul_ini$n)
    new_n <- data_regul_n[max_index[1, 1],max_index[1, 2]]
    t_reg <- regul(x=Table$Date, y=Table[,3], xmin=newtmin, n=new_n, deltat=newinterval,method="linear")
    #windows(title = "regul step 2")
    #plot(t_reg)
    Table_regul <- as.data.frame(cbind(t_reg$x,t_reg$y))
    colnames(Table_regul) <- c("Date",var)
    #windows(title = "Regulated ts")
    #plot(Table_regul,type="o")
    var_ts <- extract(t_reg)
    #plot(var_ts, type="o")
    acf_var <- acf(var_ts,na.action = na.omit,plot = F)
    ACF_data <- as.data.frame(cbind(acf_var$acf,acf_var$lag))
    lag_data <- filter(ACF_data,V1<0.20)[1,2]
    cat("Autocorrelation lag pour",station,lag_data,"
      ")
    
    Table_regul$year <- format(Table_regul$Date, "%Y")
    sampling <- count(Table_regul, year)
    np <- median(sampling[,2])
    
    dec <- stlplus(Table_regul[,2],t=Table_regul$Date, n.p=np, s.window="periodic", t.window=NULL)
    plot(dec, scales=list(y="free"))
    Table_regul$var_noseason <- dec$data$raw - dec$data$seasonal
    
    
    tendtest <- mk.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    cat("Tendance de",tendtest[["estimates"]][["tau"]],"pour ",station,"
      ")
    residual <- dec$data$remainder
    plot(acf(residual,na.action = na.pass,plot=F),main = paste0("Structure residu pour ",station))
    
    outliers_var<- outliers(Table_regul$var_noseason)
    #windows(title = "Outlyers")
    plot(y=Table_regul$var_noseason,x=Table_regul$Date, type="l",main = paste0("Outliers pour ",station))
    points(
      # the x coordinate is the index
      x=Table_regul[outliers_var,]$Date,
      # the y coordinate is the value
      y=Table_regul$var_noseason[outliers_var],
      # make points solid and red
      pch=16, col="red")
    nom_fichier <- paste0("Outliers_regularise_desaisonalise",var,station)
    nom_fichier <- paste0(nom_fichier)
    Table_regul$Code_point_Libelle <- station
    #Table_regul$Outlier <- "NON"
    #Table_regul[outliers_TEMP,]$Outlier <- "OUI"
    Table_regul$Changepoint <- NA
    changetest <- pettitt.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    changepoint <- as.numeric(changetest$estimate)
    Table_regul[changepoint,]$Changepoint <- changetest$p.value
    Table_regul$Trend <- tendtest[["estimates"]][["tau"]]
    Table_regul$pvaltrend <- tendtest[["p.value"]]
    Table_regul$pvalchange <- changetest$p.value
    Sensslope <- sens.slope(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    Table_regul$psens <- Sensslope$p.value
    Table_regul$slopesens <- as.numeric(Sensslope$estimates)
    Table_regul$interval <- newinterval
    write.csv2(Table_regul,file=paste0("data_trends/",nom_fichier,".csv"), row.names = FALSE,dec = ".")
    
  }
}

# TURB-FNU
for (k in c(12)){
  data <- dplyr::select(datab,Code_point_Libelle,Date,colnames(datab)[k])
  var <- colnames(datab)[k]
  for (i in c(1:21)){
    station <- levels(as.factor(data$Code_point_Libelle))[i]
    Table <- filter(data, Code_point_Libelle == station)
    #windows(title = "A regulariser")
    #print(ggplot(Table) + geom_path(aes(x=Date, y=TEMP)) + geom_rug(aes(x=Date)))
    Table <- Table[order(Table$Date), ]
    
    interval <- as.numeric(mean(diff(Table$Date)))
    tmin <- min(Table$Date)
    t_reg <- regul(x=Table$Date, y=Table[,3], xmin=tmin, n=nrow(Table), deltat=interval)
    #windows(title = "regul step 1")
    #plot(t_reg)
    data_regul_ini <- regul.screen(x=as.numeric(Table$Date),
                                   xmin=seq(from=as.numeric(tmin-10), to=as.numeric(tmin+10), by=1),
                                   deltat=seq(from=10, to=20, by=1),
                                   tol = 3
    )
    data_regul <- as.data.frame(data_regul_ini$nbr.match)
    max_index <- which(data_regul == max(data_regul, na.rm = TRUE), arr.ind = TRUE)
    newtmin <- as.Date(as.numeric(sub(".*=", "", rownames(data_regul)[max_index[1, 1]])))
    newinterval <- as.numeric(sub(".*=", "", colnames(data_regul)[max_index[1, 2]]))
    data_regul_n <- as.data.frame(data_regul_ini$n)
    new_n <- data_regul_n[max_index[1, 1],max_index[1, 2]]
    t_reg <- regul(x=Table$Date, y=Table[,3], xmin=newtmin, n=new_n, deltat=newinterval,method="linear")
    #windows(title = "regul step 2")
    #plot(t_reg)
    Table_regul <- as.data.frame(cbind(t_reg$x,t_reg$y))
    colnames(Table_regul) <- c("Date",var)
    #windows(title = "Regulated ts")
    #plot(Table_regul,type="o")
    var_ts <- extract(t_reg)
    #plot(var_ts, type="o")
    acf_var <- acf(var_ts,na.action = na.omit,plot = F)
    ACF_data <- as.data.frame(cbind(acf_var$acf,acf_var$lag))
    lag_data <- filter(ACF_data,V1<0.20)[1,2]
    cat("Autocorrelation lag pour",station,lag_data,"
      ")
    
    Table_regul$year <- format(Table_regul$Date, "%Y")
    sampling <- count(Table_regul, year)
    np <- median(sampling[,2])
    
    dec <- stlplus(Table_regul[,2],t=Table_regul$Date, n.p=np, s.window="periodic", t.window=NULL)
    plot(dec, scales=list(y="free"))
    Table_regul$var_noseason <- dec$data$raw - dec$data$seasonal
    
    
    tendtest <- mk.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    cat("Tendance de",tendtest[["estimates"]][["tau"]],"pour ",station,"
      ")
    residual <- dec$data$remainder
    plot(acf(residual,na.action = na.pass,plot=F),main = paste0("Structure residu pour ",station))
    
    outliers_var<- outliers(Table_regul$var_noseason)
    #windows(title = "Outlyers")
    plot(y=Table_regul$var_noseason,x=Table_regul$Date, type="l",main = paste0("Outliers pour ",station))
    points(
      # the x coordinate is the index
      x=Table_regul[outliers_var,]$Date,
      # the y coordinate is the value
      y=Table_regul$var_noseason[outliers_var],
      # make points solid and red
      pch=16, col="red")
    nom_fichier <- paste0("Outliers_regularise_desaisonalise",var,station)
    nom_fichier <- paste0(nom_fichier)
    Table_regul$Code_point_Libelle <- station
    #Table_regul$Outlier <- "NON"
    #Table_regul[outliers_TEMP,]$Outlier <- "OUI"
    Table_regul$Changepoint <- NA
    changetest <- pettitt.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    changepoint <- as.numeric(changetest$estimate)
    Table_regul[changepoint,]$Changepoint <- changetest$p.value
    Table_regul$Trend <- tendtest[["estimates"]][["tau"]]
    Table_regul$pvaltrend <- tendtest[["p.value"]]
    Table_regul$pvalchange <- changetest$p.value
    Sensslope <- sens.slope(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    Table_regul$psens <- Sensslope$p.value
    Table_regul$slopesens <- as.numeric(Sensslope$estimates)
    Table_regul$interval <- newinterval
    write.csv2(Table_regul,file=paste0("data_trends/",nom_fichier,".csv"), row.names = FALSE,dec = ".")
    
  }
}

# indices de diversité #
for (k in c(13:17)){
  data <- dplyr::select(datab,Code_point_Libelle,Date,colnames(datab)[k])
  var <- colnames(datab)[k]
  for (i in c(1:21)){
    station <- levels(as.factor(data$Code_point_Libelle))[i]
    Table <- filter(data, Code_point_Libelle == station)
    #windows(title = "A regulariser")
    #print(ggplot(Table) + geom_path(aes(x=Date, y=TEMP)) + geom_rug(aes(x=Date)))
    Table <- Table[order(Table$Date), ]
    
    interval <- as.numeric(mean(diff(Table$Date)))
    tmin <- min(Table$Date)
    t_reg <- regul(x=Table$Date, y=Table[,3], xmin=tmin, n=nrow(Table), deltat=interval)
    #windows(title = "regul step 1")
    #plot(t_reg)
    data_regul_ini <- regul.screen(x=as.numeric(Table$Date),
                                   xmin=seq(from=as.numeric(tmin-10), to=as.numeric(tmin+10), by=1),
                                   deltat=seq(from=10, to=20, by=1),
                                   tol = 3
    )
    data_regul <- as.data.frame(data_regul_ini$nbr.match)
    max_index <- which(data_regul == max(data_regul, na.rm = TRUE), arr.ind = TRUE)
    newtmin <- as.Date(as.numeric(sub(".*=", "", rownames(data_regul)[max_index[1, 1]])))
    newinterval <- as.numeric(sub(".*=", "", colnames(data_regul)[max_index[1, 2]]))
    data_regul_n <- as.data.frame(data_regul_ini$n)
    new_n <- data_regul_n[max_index[1, 1],max_index[1, 2]]
    t_reg <- regul(x=Table$Date, y=Table[,3], xmin=newtmin, n=new_n, deltat=newinterval,method="linear")
    #windows(title = "regul step 2")
    #plot(t_reg)
    Table_regul <- as.data.frame(cbind(t_reg$x,t_reg$y))
    colnames(Table_regul) <- c("Date",var)
    #windows(title = "Regulated ts")
    #plot(Table_regul,type="o")
    var_ts <- extract(t_reg)
    #plot(var_ts, type="o")
    acf_var <- acf(var_ts,na.action = na.omit,plot = F)
    ACF_data <- as.data.frame(cbind(acf_var$acf,acf_var$lag))
    lag_data <- filter(ACF_data,V1<0.20)[1,2]
    #cat("Autocorrelation lag pour",station,lag_data,"
    #  ")
    
    Table_regul$year <- format(Table_regul$Date, "%Y")
    sampling <- count(Table_regul, year)
    np <- median(sampling[,2])
    
    dec <- stlplus(Table_regul[,2],t=Table_regul$Date, n.p=np, s.window="periodic", t.window=NULL)
    plot(dec, scales=list(y="free"))
    Table_regul$var_noseason <- dec$data$raw - dec$data$seasonal
    
    
    tendtest <- mk.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    #cat("Tendance de",tendtest[["estimates"]][["tau"]],"pour ",station,"
    #  ")
    residual <- dec$data$remainder
    plot(acf(residual,na.action = na.pass,plot=F),main = paste0("Structure residu pour ",station))
    
    outliers_var<- outliers(Table_regul$var_noseason)
    #windows(title = "Outlyers")
    plot(y=Table_regul$var_noseason,x=Table_regul$Date, type="l",main = paste0("Outliers pour ",station))
    points(
      # the x coordinate is the index
      x=Table_regul[outliers_var,]$Date,
      # the y coordinate is the value
      y=Table_regul$var_noseason[outliers_var],
      # make points solid and red
      pch=16, col="red")
    nom_fichier <- paste0("Outliers_regularise_desaisonalise",var,station)
    nom_fichier <- paste0(nom_fichier)
    Table_regul$Code_point_Libelle <- station
    #Table_regul$Outlier <- "NON"
    #Table_regul[outliers_TEMP,]$Outlier <- "OUI"
    Table_regul$Changepoint <- NA
    changetest <- pettitt.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    changepoint <- as.numeric(changetest$estimate)
    Table_regul[changepoint,]$Changepoint <- changetest$p.value
    Table_regul$Trend <- tendtest[["estimates"]][["tau"]]
    Table_regul$pvaltrend <- tendtest[["p.value"]]
    Table_regul$pvalchange <- changetest$p.value
    Sensslope <- sens.slope(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    Table_regul$psens <- Sensslope$p.value
    Table_regul$slopesens <- as.numeric(Sensslope$estimates)
    Table_regul$interval <- newinterval
    cat(var,station,"Tau",tendtest[["estimates"]][["tau"]],"pval",tendtest[["p.value"]],"sens",as.numeric(Sensslope$estimates),mean(diff(Table_regul$Date[complete.cases(Table_regul$var_noseason)])),"\n")
    write.csv2(Table_regul,file=paste0("data_trends/",nom_fichier,".csv"), row.names = FALSE,dec = ".")
    
  }
}

# Dino et Bac
for (k in c(18:19)){
  data <- dplyr::select(datab,Code_point_Libelle,Date,colnames(datab)[k])
  var <- colnames(datab)[k]
  for (i in c(1:21)){
    station <- levels(as.factor(data$Code_point_Libelle))[i]
    Table <- filter(data, Code_point_Libelle == station)
    #windows(title = "A regulariser")
    #print(ggplot(Table) + geom_path(aes(x=Date, y=TEMP)) + geom_rug(aes(x=Date)))
    Table <- Table[order(Table$Date), ]
    
    interval <- as.numeric(mean(diff(Table$Date)))
    tmin <- min(Table$Date)
    t_reg <- regul(x=Table$Date, y=Table[,3], xmin=tmin, n=nrow(Table), deltat=interval)
    #windows(title = "regul step 1")
    #plot(t_reg)
    data_regul_ini <- regul.screen(x=as.numeric(Table$Date),
                                   xmin=seq(from=as.numeric(tmin-10), to=as.numeric(tmin+10), by=1),
                                   deltat=seq(from=10, to=20, by=1),
                                   tol = 3
    )
    data_regul <- as.data.frame(data_regul_ini$nbr.match)
    max_index <- which(data_regul == max(data_regul, na.rm = TRUE), arr.ind = TRUE)
    newtmin <- as.Date(as.numeric(sub(".*=", "", rownames(data_regul)[max_index[1, 1]])))
    newinterval <- as.numeric(sub(".*=", "", colnames(data_regul)[max_index[1, 2]]))
    data_regul_n <- as.data.frame(data_regul_ini$n)
    new_n <- data_regul_n[max_index[1, 1],max_index[1, 2]]
    t_reg <- regul(x=Table$Date, y=Table[,3], xmin=newtmin, n=new_n, deltat=newinterval,method="linear")
    #windows(title = "regul step 2")
    #plot(t_reg)
    Table_regul <- as.data.frame(cbind(t_reg$x,t_reg$y))
    colnames(Table_regul) <- c("Date",var)
    #windows(title = "Regulated ts")
    #plot(Table_regul,type="o")
    var_ts <- extract(t_reg)
    #plot(var_ts, type="o")
    acf_var <- acf(var_ts,na.action = na.omit,plot = F)
    ACF_data <- as.data.frame(cbind(acf_var$acf,acf_var$lag))
    lag_data <- filter(ACF_data,V1<0.20)[1,2]
    #cat("Autocorrelation lag pour",station,lag_data,"
    #  ")
    
    Table_regul$year <- format(Table_regul$Date, "%Y")
    sampling <- count(Table_regul, year)
    np <- median(sampling[,2])
    
    dec <- stlplus(Table_regul[,2],t=Table_regul$Date, n.p=np, s.window="periodic", t.window=NULL)
    plot(dec, scales=list(y="free"))
    Table_regul$var_noseason <- dec$data$raw - dec$data$seasonal
    
    
    tendtest <- mk.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    #cat("Tendance de",tendtest[["estimates"]][["tau"]],"pour ",station,"
    #  ")
    residual <- dec$data$remainder
    plot(acf(residual,na.action = na.pass,plot=F),main = paste0("Structure residu pour ",station))
    
    outliers_var<- outliers(Table_regul$var_noseason)
    #windows(title = "Outlyers")
    plot(y=Table_regul$var_noseason,x=Table_regul$Date, type="l",main = paste0("Outliers pour ",station))
    points(
      # the x coordinate is the index
      x=Table_regul[outliers_var,]$Date,
      # the y coordinate is the value
      y=Table_regul$var_noseason[outliers_var],
      # make points solid and red
      pch=16, col="red")
    nom_fichier <- paste0("Outliers_regularise_desaisonalise",var,station)
    nom_fichier <- paste0(nom_fichier)
    Table_regul$Code_point_Libelle <- station
    #Table_regul$Outlier <- "NON"
    #Table_regul[outliers_TEMP,]$Outlier <- "OUI"
    Table_regul$Changepoint <- NA
    changetest <- pettitt.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    changepoint <- as.numeric(changetest$estimate)
    Table_regul[changepoint,]$Changepoint <- changetest$p.value
    Table_regul$Trend <- tendtest[["estimates"]][["tau"]]
    Table_regul$pvaltrend <- tendtest[["p.value"]]
    Table_regul$pvalchange <- changetest$p.value
    Sensslope <- sens.slope(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    Table_regul$psens <- Sensslope$p.value
    Table_regul$slopesens <- as.numeric(Sensslope$estimates)
    Table_regul$interval <- newinterval
    cat(var,station,"Tau",tendtest[["estimates"]][["tau"]],"pval",tendtest[["p.value"]],"sens",as.numeric(Sensslope$estimates),mean(diff(Table_regul$Date[complete.cases(Table_regul$var_noseason)])),"\n")
    write.csv2(Table_regul,file=paste0("data_trends/",nom_fichier,".csv"), row.names = FALSE,dec = ".")
    
  }
}

# Cryptophyceae
for (k in c(20)){
  data <- dplyr::select(datab,Code_point_Libelle,Date,colnames(datab)[k])
  var <- colnames(datab)[k]
  for (i in c(1:4,9:12,14:17,19,21)){
    station <- levels(as.factor(data$Code_point_Libelle))[i]
    Table <- filter(data, Code_point_Libelle == station)
    #windows(title = "A regulariser")
    #print(ggplot(Table) + geom_path(aes(x=Date, y=TEMP)) + geom_rug(aes(x=Date)))
    Table <- Table[order(Table$Date), ]
    
    interval <- as.numeric(mean(diff(Table$Date)))
    tmin <- min(Table$Date)
    t_reg <- regul(x=Table$Date, y=Table[,3], xmin=tmin, n=nrow(Table), deltat=interval)
    #windows(title = "regul step 1")
    #plot(t_reg)
    data_regul_ini <- regul.screen(x=as.numeric(Table$Date),
                                   xmin=seq(from=as.numeric(tmin-10), to=as.numeric(tmin+10), by=1),
                                   deltat=seq(from=10, to=20, by=1),
                                   tol = 3
    )
    data_regul <- as.data.frame(data_regul_ini$nbr.match)
    max_index <- which(data_regul == max(data_regul, na.rm = TRUE), arr.ind = TRUE)
    newtmin <- as.Date(as.numeric(sub(".*=", "", rownames(data_regul)[max_index[1, 1]])))
    newinterval <- as.numeric(sub(".*=", "", colnames(data_regul)[max_index[1, 2]]))
    data_regul_n <- as.data.frame(data_regul_ini$n)
    new_n <- data_regul_n[max_index[1, 1],max_index[1, 2]]
    t_reg <- regul(x=Table$Date, y=Table[,3], xmin=newtmin, n=new_n, deltat=newinterval,method="linear")
    #windows(title = "regul step 2")
    #plot(t_reg)
    Table_regul <- as.data.frame(cbind(t_reg$x,t_reg$y))
    colnames(Table_regul) <- c("Date",var)
    #windows(title = "Regulated ts")
    #plot(Table_regul,type="o")
    var_ts <- extract(t_reg)
    #plot(var_ts, type="o")
    acf_var <- acf(var_ts,na.action = na.omit,plot = F)
    ACF_data <- as.data.frame(cbind(acf_var$acf,acf_var$lag))
    lag_data <- filter(ACF_data,V1<0.20)[1,2]
    cat("Autocorrelation lag pour",station,lag_data,"
      ")
    
    Table_regul$year <- format(Table_regul$Date, "%Y")
    sampling <- count(Table_regul, year)
    np <- median(sampling[,2])
    
    dec <- stlplus(Table_regul[,2],t=Table_regul$Date, n.p=np, s.window="periodic", t.window=NULL)
    plot(dec, scales=list(y="free"))
    Table_regul$var_noseason <- dec$data$raw - dec$data$seasonal
    
    
    tendtest <- mk.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    cat("Tendance de",tendtest[["estimates"]][["tau"]],"pour ",station,"
      ")
    residual <- dec$data$remainder
    plot(acf(residual,na.action = na.pass,plot=F),main = paste0("Structure residu pour ",station))
    
    outliers_var<- outliers(Table_regul$var_noseason)
    #windows(title = "Outlyers")
    plot(y=Table_regul$var_noseason,x=Table_regul$Date, type="l",main = paste0("Outliers pour ",station))
    points(
      # the x coordinate is the index
      x=Table_regul[outliers_var,]$Date,
      # the y coordinate is the value
      y=Table_regul$var_noseason[outliers_var],
      # make points solid and red
      pch=16, col="red")
    nom_fichier <- paste0("Outliers_regularise_desaisonalise",var,station)
    nom_fichier <- paste0(nom_fichier)
    Table_regul$Code_point_Libelle <- station
    #Table_regul$Outlier <- "NON"
    #Table_regul[outliers_TEMP,]$Outlier <- "OUI"
    Table_regul$Changepoint <- NA
    changetest <- pettitt.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    changepoint <- as.numeric(changetest$estimate)
    Table_regul[changepoint,]$Changepoint <- changetest$p.value
    Table_regul$Trend <- tendtest[["estimates"]][["tau"]]
    Table_regul$pvaltrend <- tendtest[["p.value"]]
    Table_regul$pvalchange <- changetest$p.value
    Sensslope <- sens.slope(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    Table_regul$psens <- Sensslope$p.value
    Table_regul$slopesens <- as.numeric(Sensslope$estimates)
    Table_regul$interval <- newinterval
    write.csv2(Table_regul,file=paste0("data_trends/",nom_fichier,".csv"), row.names = FALSE,dec = ".")
    
  }
}

for (k in c(21)){
  data <- dplyr::select(datab,Code_point_Libelle,Date,colnames(datab)[k])
  var <- colnames(datab)[k]
  for (i in c(1:5,9:17,19,21)){
    station <- levels(as.factor(data$Code_point_Libelle))[i]
    Table <- filter(data, Code_point_Libelle == station)
    #windows(title = "A regulariser")
    #print(ggplot(Table) + geom_path(aes(x=Date, y=TEMP)) + geom_rug(aes(x=Date)))
    Table <- Table[order(Table$Date), ]
    
    interval <- as.numeric(mean(diff(Table$Date)))
    tmin <- min(Table$Date)
    t_reg <- regul(x=Table$Date, y=Table[,3], xmin=tmin, n=nrow(Table), deltat=interval)
    #windows(title = "regul step 1")
    #plot(t_reg)
    data_regul_ini <- regul.screen(x=as.numeric(Table$Date),
                                   xmin=seq(from=as.numeric(tmin-10), to=as.numeric(tmin+10), by=1),
                                   deltat=seq(from=10, to=20, by=1),
                                   tol = 3
    )
    data_regul <- as.data.frame(data_regul_ini$nbr.match)
    max_index <- which(data_regul == max(data_regul, na.rm = TRUE), arr.ind = TRUE)
    newtmin <- as.Date(as.numeric(sub(".*=", "", rownames(data_regul)[max_index[1, 1]])))
    newinterval <- as.numeric(sub(".*=", "", colnames(data_regul)[max_index[1, 2]]))
    data_regul_n <- as.data.frame(data_regul_ini$n)
    new_n <- data_regul_n[max_index[1, 1],max_index[1, 2]]
    t_reg <- regul(x=Table$Date, y=Table[,3], xmin=newtmin, n=new_n, deltat=newinterval,method="linear")
    #windows(title = "regul step 2")
    #plot(t_reg)
    Table_regul <- as.data.frame(cbind(t_reg$x,t_reg$y))
    colnames(Table_regul) <- c("Date",var)
    #windows(title = "Regulated ts")
    #plot(Table_regul,type="o")
    var_ts <- extract(t_reg)
    #plot(var_ts, type="o")
    acf_var <- acf(var_ts,na.action = na.omit,plot = F)
    ACF_data <- as.data.frame(cbind(acf_var$acf,acf_var$lag))
    lag_data <- filter(ACF_data,V1<0.20)[1,2]
    cat("Autocorrelation lag pour",station,lag_data,"
      ")
    
    Table_regul$year <- format(Table_regul$Date, "%Y")
    sampling <- count(Table_regul, year)
    np <- median(sampling[,2])
    
    dec <- stlplus(Table_regul[,2],t=Table_regul$Date, n.p=np, s.window="periodic", t.window=NULL)
    plot(dec, scales=list(y="free"))
    Table_regul$var_noseason <- dec$data$raw - dec$data$seasonal
    
    
    tendtest <- mk.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    cat("Tendance de",tendtest[["estimates"]][["tau"]],"pour ",station,"
      ")
    residual <- dec$data$remainder
    plot(acf(residual,na.action = na.pass,plot=F),main = paste0("Structure residu pour ",station))
    
    outliers_var<- outliers(Table_regul$var_noseason)
    #windows(title = "Outlyers")
    plot(y=Table_regul$var_noseason,x=Table_regul$Date, type="l",main = paste0("Outliers pour ",station))
    points(
      # the x coordinate is the index
      x=Table_regul[outliers_var,]$Date,
      # the y coordinate is the value
      y=Table_regul$var_noseason[outliers_var],
      # make points solid and red
      pch=16, col="red")
    nom_fichier <- paste0("Outliers_regularise_desaisonalise",var,station)
    nom_fichier <- paste0(nom_fichier)
    Table_regul$Code_point_Libelle <- station
    #Table_regul$Outlier <- "NON"
    #Table_regul[outliers_TEMP,]$Outlier <- "OUI"
    Table_regul$Changepoint <- NA
    changetest <- pettitt.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    changepoint <- as.numeric(changetest$estimate)
    Table_regul[changepoint,]$Changepoint <- changetest$p.value
    Table_regul$Trend <- tendtest[["estimates"]][["tau"]]
    Table_regul$pvaltrend <- tendtest[["p.value"]]
    Table_regul$pvalchange <- changetest$p.value
    Sensslope <- sens.slope(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    Table_regul$psens <- Sensslope$p.value
    Table_regul$slopesens <- as.numeric(Sensslope$estimates)
    Table_regul$interval <- newinterval
    write.csv2(Table_regul,file=paste0("data_trends/",nom_fichier,".csv"), row.names = FALSE,dec = ".")
    
  }
}



# Metriques
for (k in c(23,25:29,31,35)){
  data <- dplyr::select(datab,Code_point_Libelle,Date,colnames(datab)[k])
  var <- colnames(datab)[k]
  for (i in c(1:21)){
    station <- levels(as.factor(data$Code_point_Libelle))[i]
    Table <- filter(data, Code_point_Libelle == station)
    #windows(title = "A regulariser")
    #print(ggplot(Table) + geom_path(aes(x=Date, y=TEMP)) + geom_rug(aes(x=Date)))
    Table <- Table[order(Table$Date), ]
    
    interval <- as.numeric(mean(diff(Table$Date)))
    tmin <- min(Table$Date)
    t_reg <- regul(x=Table$Date, y=Table[,3], xmin=tmin, n=nrow(Table), deltat=interval)
    #windows(title = "regul step 1")
    #plot(t_reg)
    data_regul_ini <- regul.screen(x=as.numeric(Table$Date),
                                   xmin=seq(from=as.numeric(tmin-10), to=as.numeric(tmin+10), by=1),
                                   deltat=seq(from=10, to=20, by=1),
                                   tol = 3
    )
    data_regul <- as.data.frame(data_regul_ini$nbr.match)
    max_index <- which(data_regul == max(data_regul, na.rm = TRUE), arr.ind = TRUE)
    newtmin <- as.Date(as.numeric(sub(".*=", "", rownames(data_regul)[max_index[1, 1]])))
    newinterval <- as.numeric(sub(".*=", "", colnames(data_regul)[max_index[1, 2]]))
    data_regul_n <- as.data.frame(data_regul_ini$n)
    new_n <- data_regul_n[max_index[1, 1],max_index[1, 2]]
    t_reg <- regul(x=Table$Date, y=Table[,3], xmin=newtmin, n=new_n, deltat=newinterval,method="linear")
    #windows(title = "regul step 2")
    #plot(t_reg)
    Table_regul <- as.data.frame(cbind(t_reg$x,t_reg$y))
    colnames(Table_regul) <- c("Date",var)
    #windows(title = "Regulated ts")
    #plot(Table_regul,type="o")
    var_ts <- extract(t_reg)
    #plot(var_ts, type="o")
    acf_var <- acf(var_ts,na.action = na.omit,plot = F)
    ACF_data <- as.data.frame(cbind(acf_var$acf,acf_var$lag))
    lag_data <- filter(ACF_data,V1<0.20)[1,2]
    #cat("Autocorrelation lag pour",station,lag_data,"
    #  ")
    
    Table_regul$year <- format(Table_regul$Date, "%Y")
    sampling <- count(Table_regul, year)
    np <- median(sampling[,2])
    
    dec <- stlplus(Table_regul[,2],t=Table_regul$Date, n.p=np, s.window="periodic", t.window=NULL)
    plot(dec, scales=list(y="free"))
    Table_regul$var_noseason <- dec$data$raw - dec$data$seasonal
    
    
    tendtest <- mk.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    #cat("Tendance de",tendtest[["estimates"]][["tau"]],"pour ",station,"
    #  ")
    residual <- dec$data$remainder
    plot(acf(residual,na.action = na.pass,plot=F),main = paste0("Structure residu pour ",station))
    
    outliers_var<- outliers(Table_regul$var_noseason)
    #windows(title = "Outlyers")
    plot(y=Table_regul$var_noseason,x=Table_regul$Date, type="l",main = paste0("Outliers pour ",station))
    points(
      # the x coordinate is the index
      x=Table_regul[outliers_var,]$Date,
      # the y coordinate is the value
      y=Table_regul$var_noseason[outliers_var],
      # make points solid and red
      pch=16, col="red")
    nom_fichier <- paste0("Outliers_regularise_desaisonalise",var,station)
    nom_fichier <- paste0(nom_fichier)
    Table_regul$Code_point_Libelle <- station
    #Table_regul$Outlier <- "NON"
    #Table_regul[outliers_TEMP,]$Outlier <- "OUI"
    Table_regul$Changepoint <- NA
    changetest <- pettitt.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    changepoint <- as.numeric(changetest$estimate)
    Table_regul[changepoint,]$Changepoint <- changetest$p.value
    Table_regul$Trend <- tendtest[["estimates"]][["tau"]]
    Table_regul$pvaltrend <- tendtest[["p.value"]]
    Table_regul$pvalchange <- changetest$p.value
    Sensslope <- sens.slope(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    Table_regul$psens <- Sensslope$p.value
    Table_regul$slopesens <- as.numeric(Sensslope$estimates)
    Table_regul$interval <- newinterval
    cat(var,station,"Tau",tendtest[["estimates"]][["tau"]],"pval",tendtest[["p.value"]],"sens",as.numeric(Sensslope$estimates),mean(diff(Table_regul$Date[complete.cases(Table_regul$var_noseason)])),"\n")
    write.csv2(Table_regul,file=paste0("data_trends/",nom_fichier,".csv"), row.names = FALSE,dec = ".")
    
  }
}

# Import the CA data
data1 <- read_delim("data_modif/CA_Cluster1_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)
data2 <- read_delim("data_modif/CA_Cluster2_final.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                        grouping_mark = ""), trim_ws = TRUE)
data3 <- read_delim("data_modif/CA_Cluster3_final.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                        grouping_mark = ""), trim_ws = TRUE)
data4 <- read_delim("data_modif/CA_Cluster4_final.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                        grouping_mark = ""), trim_ws = TRUE)

datab <- bind_rows(data1,data2,data3,data4)

# CA
for (k in c(1:3)){
  data <- dplyr::select(datab,Code_point_Libelle,Date,colnames(datab)[k])
  var <- colnames(datab)[k]
  for (i in c(1:21)){
    station <- levels(as.factor(data$Code_point_Libelle))[i]
    Table <- filter(data, Code_point_Libelle == station)
    #windows(title = "A regulariser")
    #print(ggplot(Table) + geom_path(aes(x=Date, y=TEMP)) + geom_rug(aes(x=Date)))
    Table <- Table[order(Table$Date), ]
    
    interval <- as.numeric(mean(diff(Table$Date)))
    tmin <- min(Table$Date)
    t_reg <- regul(x=Table$Date, y=Table[,3], xmin=tmin, n=nrow(Table), deltat=interval)
    #windows(title = "regul step 1")
    #plot(t_reg)
    data_regul_ini <- regul.screen(x=as.numeric(Table$Date),
                                   xmin=seq(from=as.numeric(tmin-10), to=as.numeric(tmin+10), by=1),
                                   deltat=seq(from=10, to=20, by=1),
                                   tol = 3
    )
    data_regul <- as.data.frame(data_regul_ini$nbr.match)
    max_index <- which(data_regul == max(data_regul, na.rm = TRUE), arr.ind = TRUE)
    newtmin <- as.Date(as.numeric(sub(".*=", "", rownames(data_regul)[max_index[1, 1]])))
    newinterval <- as.numeric(sub(".*=", "", colnames(data_regul)[max_index[1, 2]]))
    data_regul_n <- as.data.frame(data_regul_ini$n)
    new_n <- data_regul_n[max_index[1, 1],max_index[1, 2]]
    t_reg <- regul(x=Table$Date, y=Table[,3], xmin=newtmin, n=new_n, deltat=newinterval,method="linear")
    #windows(title = "regul step 2")
    #plot(t_reg)
    Table_regul <- as.data.frame(cbind(t_reg$x,t_reg$y))
    colnames(Table_regul) <- c("Date",var)
    #windows(title = "Regulated ts")
    #plot(Table_regul,type="o")
    var_ts <- extract(t_reg)
    #plot(var_ts, type="o")
    acf_var <- acf(var_ts,na.action = na.omit,plot = F)
    ACF_data <- as.data.frame(cbind(acf_var$acf,acf_var$lag))
    lag_data <- filter(ACF_data,V1<0.20)[1,2]
    #cat("Autocorrelation lag pour",station,lag_data,"
    #  ")
    
    Table_regul$year <- format(Table_regul$Date, "%Y")
    sampling <- count(Table_regul, year)
    np <- median(sampling[,2])
    
    dec <- stlplus(Table_regul[,2],t=Table_regul$Date, n.p=np, s.window="periodic", t.window=NULL)
    plot(dec, scales=list(y="free"))
    Table_regul$var_noseason <- dec$data$raw - dec$data$seasonal
    
    
    tendtest <- mk.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    #cat("Tendance de",tendtest[["estimates"]][["tau"]],"pour ",station,"
    #  ")
    residual <- dec$data$remainder
    plot(acf(residual,na.action = na.pass,plot=F),main = paste0("Structure residu pour ",station))
    
    outliers_var<- outliers(Table_regul$var_noseason)
    #windows(title = "Outlyers")
    plot(y=Table_regul$var_noseason,x=Table_regul$Date, type="l",main = paste0("Outliers pour ",station))
    points(
      # the x coordinate is the index
      x=Table_regul[outliers_var,]$Date,
      # the y coordinate is the value
      y=Table_regul$var_noseason[outliers_var],
      # make points solid and red
      pch=16, col="red")
    nom_fichier <- paste0("Outliers_regularise_desaisonalise",var,station)
    nom_fichier <- paste0(nom_fichier)
    Table_regul$Code_point_Libelle <- station
    #Table_regul$Outlier <- "NON"
    #Table_regul[outliers_TEMP,]$Outlier <- "OUI"
    Table_regul$Changepoint <- NA
    changetest <- pettitt.test(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    changepoint <- as.numeric(changetest$estimate)
    Table_regul[changepoint,]$Changepoint <- changetest$p.value
    Table_regul$Trend <- tendtest[["estimates"]][["tau"]]
    Table_regul$pvaltrend <- tendtest[["p.value"]]
    Table_regul$pvalchange <- changetest$p.value
    Sensslope <- sens.slope(Table_regul$var_noseason[complete.cases(Table_regul$var_noseason)])
    Table_regul$psens <- Sensslope$p.value
    Table_regul$slopesens <- as.numeric(Sensslope$estimates)
    Table_regul$interval <- newinterval
    cat(var,station,"Tau",tendtest[["estimates"]][["tau"]],"pval",tendtest[["p.value"]],"sens",as.numeric(Sensslope$estimates),mean(diff(Table_regul$Date[complete.cases(Table_regul$var_noseason)])),"\n")
    write.csv2(Table_regul,file=paste0("data_trends/",nom_fichier,".csv"), row.names = FALSE,dec = ".")
    
  }
}

# Clustering on trends #####
data <- read_delim("data_modif/Tendances_tau.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                        grouping_mark = ""), trim_ws = TRUE)
data <- data[1:21,]
rownames(data) <- data$Station

distdata <- dist(data,method = "euclidean")
clust <- hclust(distdata,"complete")
plot(clust)

data <- read_delim("data_modif/Tendances_tau.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)
data <- t(data[1:21,-1])
distdata <- dist(data,method = "euclidean")
clust <- hclust(distdata,"complete")
plot(clust)
  
# Correlations on trends
data <- read_delim("data_modif/Tendances_tau.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ".", 
                                                                       grouping_mark = ""), trim_ws = TRUE)
data <- data[1:21,-1]
r <- cor(data)

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
p.mat <- cor.mtest(r)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(r, method="color", col=col(20),  
         type="upper", order="alphabet",
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=90,tl.cex = 0.8, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=F, 
         title = "Correlation entre tendances"
)

