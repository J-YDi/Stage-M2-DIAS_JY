# Load packages
library(readr)
library(dplyr)
library(ggplot2)
library(trend)
library(lubridate)

##########################################
# Table_bloom_R_v3c.csv was made by hand #
##########################################
# Load data
data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                                  delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                                  trim_ws = TRUE)


data_bloom$cluster <- as.factor(data_bloom$cluster)
cluster_col <- c("1" = "#F8766D","2" = "#CD9600", "3" = "#00BE67", "4" = "#00A9FF", "5" = "#FF61CC")

# Distinguish monospecific blooms from others ####
data_bloom$Monodominance <- "NON"
data_bloom$Monodominance <- ifelse(data_bloom$P_dominance >= median(data_bloom$P_dominance), "OUI",data_bloom$Monodominance) 

write.csv2(data_bloom,file="data_modif/Table_bloom_VF_final.csv", row.names = FALSE,dec = ".")

# Trend number of blooms over time ####
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

# Difference on percentage of dominance during between bloom and no bloom ####

data_bloom <- read_delim("data_modif/Table_bloom_R_v3c.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)
data_bloom[grep("Antifer",x = data_bloom$Code_point_Libelle),"Code_point_Libelle"] <- "Antifer ponton pétrolier"

data <- read_delim("data_modif/Table_FLORTOT_Surf_0722_COM_period_Stselect_hydro_phyto_chloro_phylum_period15_chlafilter_cluster5_div_final.csv", 
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                       grouping_mark = ""), trim_ws = TRUE)

data$Abdtot <- apply(data[,24:328],1,sum,na.rm=T)

data$Abdmax <- apply(data[,24:328],1,max,na.rm=T)

data$P_dominance <- data$Abdmax/data$Abdtot 

data <- dplyr::select(data, Code_point_Libelle,cluster, Date, P_dominance)

data_bloom <- dplyr::select(data_bloom, -cluster)

data_combo <- left_join(data,data_bloom, by = join_by(Code_point_Libelle, Date))

wilcox.test(filter(data_combo, is.na(Bloom_Phylum))$P_dominance.x,
            filter(data_combo, Month >= 0)$P_dominance.x)

