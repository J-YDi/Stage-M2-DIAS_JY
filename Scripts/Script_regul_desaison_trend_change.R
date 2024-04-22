# Loading packages
library(readr)
library(interp)
library("pastecs")
library("stlplus")
library(trend)
library(dplyr)

# Import data
data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

# Create the function to detect outliers
outliers <- function(x) {
  median_dev <- abs(x - median(x,na.rm = T))
  indexes <- which(median_dev > (2.323561*1.486) * mad(x,na.rm = T))
  return(indexes)
}

# Loop to regularize, deseasonalize, detect shifts, trends and some

for (i in c(1:21)){
  station <- levels(as.factor(data$Code_point_Libelle))[i]
  Table <- filter(data, Code_point_Libelle == station)
  #windows(title = "A regulariser")
  #print(ggplot(Table) + geom_path(aes(x=Date, y=CHLOROA)) + geom_rug(aes(x=Date)))
  Table <- Table[order(Table$Date), ]
  
  interval <- as.numeric(mean(diff(Table$Date)))
  tmin <- min(Table$Date)
  t_reg <- regul(x=Table$Date, y=Table$CHLOROA, xmin=tmin, n=nrow(Table), deltat=interval)
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
  t_reg <- regul(x=Table$Date, y=Table$CHLOROA, xmin=newtmin, n=new_n, deltat=newinterval,method="linear")
  #windows(title = "regul step 2")
  #plot(t_reg)
  Table_regul <- as.data.frame(cbind(t_reg$x,t_reg$y))
  colnames(Table_regul) <- c("Date","CHLOROA")
  #windows(title = "Regulated ts")
  #plot(Table_regul,type="o")
  CHLOROA_ts <- extract(t_reg)
  #plot(CHLOROA_ts, type="o")
  acf_CHLOROA <- acf(CHLOROA_ts,na.action = na.omit,plot = F)
  ACF_data <- as.data.frame(cbind(acf_CHLOROA$acf,acf_CHLOROA$lag))
  lag_data <- filter(ACF_data,V1<0.20)[1,2]
  cat("Autocorrelation lag pour",station,lag_data,"
      ")
  
  Table_regul$year <- format(Table_regul$Date, "%Y")
  sampling <- count(Table_regul, year)
  np <- median(sampling[,2])
  
  dec <- stlplus(Table_regul$CHLOROA,t=Table_regul$Date, n.p=np, s.window="periodic", t.window=NULL)
  plot(dec, scales=list(y="free"))
  Table_regul$CHLOROA_noseason <- dec$data$raw - dec$data$seasonal
  
  
  tendtest <- mk.test(Table_regul$CHLOROA_noseason[complete.cases(Table_regul$CHLOROA_noseason)])
  cat("Tendance de",tendtest[["estimates"]][["tau"]],"pour ",station,"
      ")
  residual <- dec$data$remainder
  plot(acf(residual,na.action = na.pass,plot=F),main = paste0("Structure residu pour ",station))
  
  outliers_CHLOROA<- outliers(Table_regul$CHLOROA_noseason)
  #windows(title = "Outlyers")
  plot(y=Table_regul$CHLOROA_noseason,x=Table_regul$Date, type="l",main = paste0("Outliers pour ",station))
  points(
    # the x coordinate is the index
    x=Table_regul[outliers_CHLOROA,]$Date,
    # the y coordinate is the value
    y=Table_regul$CHLOROA_noseason[outliers_CHLOROA],
    # make points solid and red
    pch=16, col="red")
  nom_fichier <- paste0("Outliers_regularise_desaisonalise_CHLOROA",station)
  nom_fichier <- paste0(nom_fichier)
  Table_regul$Code_point_Libelle <- station
  Table_regul$Outlier <- "NON"
  Table_regul[outliers_CHLOROA,]$Outlier <- "OUI"
  Table_regul$Changepoint <- NA
  changetest <- pettitt.test(Table_regul$CHLOROA_noseason[complete.cases(Table_regul$CHLOROA_noseason)])
  changepoint <- as.numeric(changetest$estimate)
  Table_regul[changepoint,]$Changepoint <- changetest$p.value
  Table_regul$Trend <- tendtest[["estimates"]][["tau"]]
  Table_regul$pvaltrend <- tendtest[["p.value"]]
  Table_regul$pvalchange <- changetest$p.value
  Sensslope <- sens.slope(Table_regul$CHLOROA_noseason[complete.cases(Table_regul$CHLOROA_noseason)])
  Table_regul$psens <- Sensslope$p.value
  Table_regul$slopesens <- as.numeric(Sensslope$estimates)
  write.csv2(Table_regul,file=paste0("data_outliers/Regularisé&Desaisonnalisé/",nom_fichier,".csv"), row.names = FALSE,dec = ".")

}

