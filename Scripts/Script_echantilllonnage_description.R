# Script adapte de Time_series_MP_modif et Script_echantillonage_new
# JY

# Time Series 

library(readr)
library(tidyverse)
library (ggplot2)
# for representing missing values
library(naniar)

################## FLORTOT SURFACE ################
Table <- read_delim("data_modif/Table_FLORTOT_Surf_9523_hydro_phyto_chloro_phylum.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                        grouping_mark = ""), trim_ws = TRUE)

Table <- select(Table, -c(Year,Month))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

# Getting rid of non-UTF8 characters in the Table
Table$site <- gsub('<f4>','o', Table$site)
Table$site <- gsub('<ea>','e', Table$site)

# Creating subtables for each sea / and then for each region
# Channel
Table.ECNS <- filter(Table, Code.Region %in% c(11))
Table.SB <- filter(Table, Code.Region %in% c(12))
Table.WC <- filter(Table, Code.Region %in% c(13))
# Atlantic
Table.SthBrty <- filter(Table, Code.Region %in% c(21))
Table.PLP <- filter(Table, Code.Region %in% c(22))
Table.SthBoB <- filter(Table, Code.Region %in% c(23))
# Mediterranean
Table.GoL <- filter(Table, Code.Region %in% c(31))
Table.MLC <- filter(Table, Code.Region %in% c(32))

#### FOR THE WHOLE DATASET ####
# Select the site (+code), and the informations about date

colnames(Table)[which(names(Table) == "Code.Region")] <- "Region"


Time_series_REPHY <- Table %>% 
  dplyr::select(Region, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(Region) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "Region", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# rownames(fq_month) <- fq_month$sampling

# Representing the sampling effort 
# c <- vis_miss(fq_month[3:25], show_perc = FALSE, show_perc_col = FALSE)


# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:10])
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'Region', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1995,2023,1)), '-L', sep = '') ################ ICI 
year_half <- paste(c(seq(1995,2023,1)), '-F', sep = '') ################ ICI 

ggplot(fq_month_heatmap_gg) +
  geom_point(aes(y = Region, x = date, 
                 size = factor(sampling_effort), 
                 colour = factor(sampling_effort))) +
  scale_colour_viridis_d() +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
 annotate('text', x = year_half, y = rep(8.3,29),############################### ICI
          label = unique(fq_month_heatmap_gg$year),
          size = 4, colour = 'grey20') +
  scale_x_discrete(labels = rep(seq(1,12,1),40)) +
  theme_classic(base_size = 15) +             ################ ICI 
  theme(axis.text.x = element_text(size = 4), ################ ICI 
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(size = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE),
         colour = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE))
ggsave('Ech_Regions1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = Region, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(8.3,29),
           label = unique(fq_month_heatmap_gg$year),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_Regions2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = Region, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(8.3,29),
           label = unique(fq_month_heatmap_gg$year),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_Regions3.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')

########## PAR REGION
Time_series_REPHY <- Table.WC %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')

# rownames(fq_month) <- fq_month$sampling

# Representing the sampling effort 
# c <- vis_miss(fq_month[3:25], show_perc = FALSE, show_perc_col = FALSE)


# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:16]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

# heatmap(fq_month_heatmap, Rowv = NA, Colv = NA) not working for me

# fq_month_heatmap <- as.data.frame(fq_month_heatmap)

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1995,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1995,2023,1)), '-F', sep = '') ################ IDEM

ggplot(fq_month_heatmap_gg) +
  geom_point(aes(y = site, x = date, 
                 size = factor(sampling_effort), 
                 colour = factor(sampling_effort))) +
  scale_colour_viridis_d() +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
  annotate('text', x = year_half, y = rep(14.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = unique(fq_month_heatmap_gg$year),
           size = 3, colour = 'grey20') +
  scale_x_discrete(labels = rep(seq(1,12,1),40)) + # A ADAPTER (ERREUR INDIQUE)
  theme_classic(base_size = 15) +             ################ POUR MIEUX VOIR 
  theme(axis.text.x = element_text(size = 4), ################ IDEM
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(size = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE),
         colour = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE))
ggsave('Ech_WC1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(14.3,29),
           label = unique(fq_month_heatmap_gg$year),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_WC2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(14.3,29),
           label = unique(fq_month_heatmap_gg$year),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_WC3.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')



Time_series_REPHY <- Table.SthBrty %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')


# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:14]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(c(seq(1995,2023,1)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(c(seq(1995,2023,1)), '-F', sep = '') ################ IDEM

ggplot(fq_month_heatmap_gg) +
  geom_point(aes(y = site, x = date, 
                 size = factor(sampling_effort), 
                 colour = factor(sampling_effort))) +
  scale_colour_viridis_d() +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
  annotate('text', x = year_half, y = rep(12.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = unique(fq_month_heatmap_gg$year),
           size = 3, colour = 'grey20') +
  scale_x_discrete(labels = rep(seq(1,12,1),40)) + # A ADAPTER (ERREUR INDIQUE)
  theme_classic(base_size = 15) +             ################ POUR MIEUX VOIR 
  theme(axis.text.x = element_text(size = 4), ################ IDEM
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(size = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE),
         colour = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE))
ggsave('Ech_SthBrty1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(12.5,29),
           label = unique(fq_month_heatmap_gg$year),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_SthBrty2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(12.5,29),
           label = unique(fq_month_heatmap_gg$year),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_SthBrty3.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')




Time_series_REPHY <- Table.SthBoB %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')


# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:4]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(levels(as.factor(fq_month_heatmap_gg$year)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(levels(as.factor(fq_month_heatmap_gg$year)), '-F', sep = '') ################ IDEM

ggplot(fq_month_heatmap_gg) +
  geom_point(aes(y = site, x = date, 
                 size = factor(sampling_effort), 
                 colour = factor(sampling_effort))) +
  scale_colour_viridis_d() +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
  annotate('text', x = year_half, y = rep(2.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 2, colour = 'grey20') +
  scale_x_discrete(labels = rep(seq(1,12,1),45)) + # A ADAPTER (ERREUR INDIQUE)
  theme_classic(base_size = 15) +             ################ POUR MIEUX VOIR 
  theme(axis.text.x = element_text(size = 4), ################ IDEM
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(size = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE),
         colour = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE))

ggsave('Ech_SthBob1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(2.5,29),
           label = unique(fq_month_heatmap_gg$year),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_SthBob2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(2.5,29),
           label = unique(fq_month_heatmap_gg$year),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_SthBob3.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')



Time_series_REPHY <- Table.SB %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')


# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:12]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(levels(as.factor(fq_month_heatmap_gg$year)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(levels(as.factor(fq_month_heatmap_gg$year)), '-F', sep = '') ################ IDEM

ggplot(fq_month_heatmap_gg) +
  geom_point(aes(y = site, x = date, 
                 size = factor(sampling_effort), 
                 colour = factor(sampling_effort))) +
  scale_colour_viridis_d() +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
  annotate('text', x = year_half, y = rep(10.5,28),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 2, colour = 'grey20') +
  scale_x_discrete(labels = rep(seq(1,12,1),45)) + # A ADAPTER (ERREUR INDIQUE)
  theme_classic(base_size = 15) +             ################ POUR MIEUX VOIR 
  theme(axis.text.x = element_text(size = 4), ################ IDEM
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(size = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE),
         colour = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE))

ggsave('Ech_Sb1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 1) +
  annotate('text', x = year_half, y = rep(10.5,28),
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_Sb2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(10.5,28),
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_Sb3.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')





Time_series_REPHY <- Table.PLP %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')


# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:23]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(levels(as.factor(fq_month_heatmap_gg$year)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(levels(as.factor(fq_month_heatmap_gg$year)), '-F', sep = '') ################ IDEM

ggplot(fq_month_heatmap_gg) +
  geom_point(aes(y = site, x = date, 
                 size = factor(sampling_effort), 
                 colour = factor(sampling_effort))) +
  scale_colour_viridis_d() +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
  annotate('text', x = year_half, y = rep(21.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 2, colour = 'grey20') +
  scale_x_discrete(labels = rep(seq(1,12,1),45)) + # A ADAPTER (ERREUR INDIQUE)
  theme_classic(base_size = 15) +             ################ POUR MIEUX VOIR 
  theme(axis.text.x = element_text(size = 4), ################ IDEM
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(size = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE),
         colour = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE))

ggsave('Ech_PLP1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
  annotate('text', x = year_half, y = rep(21.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_PLP2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(21.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_PLP3.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')



Time_series_REPHY <- Table.MLC %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')


# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:31]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(levels(as.factor(fq_month_heatmap_gg$year)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(levels(as.factor(fq_month_heatmap_gg$year)), '-F', sep = '') ################ IDEM

ggplot(fq_month_heatmap_gg) +
  geom_point(aes(y = site, x = date, 
                 size = factor(sampling_effort), 
                 colour = factor(sampling_effort))) +
  scale_colour_viridis_d() +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
  annotate('text', x = year_half, y = rep(29.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 2, colour = 'grey20') +
  scale_x_discrete(labels = rep(seq(1,12,1),45)) + # A ADAPTER (ERREUR INDIQUE)
  theme_classic(base_size = 15) +             ################ POUR MIEUX VOIR 
  theme(axis.text.x = element_text(size = 4), ################ IDEM
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(size = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE),
         colour = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE))

ggsave('Ech_MLC1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
  annotate('text', x = year_half, y = rep(29.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_MLC2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(29.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_MLC3.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')




Time_series_REPHY <- Table.GoL%>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')


# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:18]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(levels(as.factor(fq_month_heatmap_gg$year)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(levels(as.factor(fq_month_heatmap_gg$year)), '-F', sep = '') ################ IDEM

ggplot(fq_month_heatmap_gg) +
  geom_point(aes(y = site, x = date, 
                 size = factor(sampling_effort), 
                 colour = factor(sampling_effort))) +
  scale_colour_viridis_d() +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
  annotate('text', x = year_half, y = rep(16.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 2, colour = 'grey20') +
  scale_x_discrete(labels = rep(seq(1,12,1),45)) + # A ADAPTER (ERREUR INDIQUE)
  theme_classic(base_size = 15) +             ################ POUR MIEUX VOIR 
  theme(axis.text.x = element_text(size = 4), ################ IDEM
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(size = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE),
         colour = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE))

ggsave('Ech_GoL1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
  annotate('text', x = year_half, y = rep(16.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_GoL2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(16.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_GoL3.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')




Time_series_REPHY <- Table.ECNS%>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')


# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:8]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(levels(as.factor(fq_month_heatmap_gg$year)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(levels(as.factor(fq_month_heatmap_gg$year)), '-F', sep = '') ################ IDEM

ggplot(fq_month_heatmap_gg) +
  geom_point(aes(y = site, x = date, 
                 size = factor(sampling_effort), 
                 colour = factor(sampling_effort))) +
  scale_colour_viridis_d() +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
  annotate('text', x = year_half, y = rep(6.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 2, colour = 'grey20') +
  scale_x_discrete(labels = rep(seq(1,12,1),45)) + # A ADAPTER (ERREUR INDIQUE)
  theme_classic(base_size = 15) +             ################ POUR MIEUX VOIR 
  theme(axis.text.x = element_text(size = 4), ################ IDEM
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(size = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE),
         colour = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE))

ggsave('Ech_ECNS1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
  annotate('text', x = year_half, y = rep(6.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 3, colour = 'grey80') +
  scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_ECNS2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(6.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 3, colour = 'black') +
  scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_ECNS3.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')




#### Echantillonnage pour toutes les stations selectionnees ####
# PAR FACADE 

# Manche

Table <- select(Table.Manche_select, -c(Year,Month))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')


# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:16]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(levels(as.factor(fq_month_heatmap_gg$year)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(levels(as.factor(fq_month_heatmap_gg$year)), '-F', sep = '') ################ IDEM

ggplot(fq_month_heatmap_gg) +
  geom_point(aes(y = site, x = date, 
                 size = factor(sampling_effort), 
                 colour = factor(sampling_effort))) +
  scale_colour_viridis_d() +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
  annotate('text', x = year_half, y = rep(14.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 2, colour = 'grey20') +
  scale_x_discrete(labels = rep(seq(1,12,1),45)) + # A ADAPTER (ERREUR INDIQUE)
  theme_classic(base_size = 15) +             ################ POUR MIEUX VOIR 
  theme(axis.text.x = element_text(size = 4), ################ IDEM
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(size = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE),
         colour = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE))

ggsave('Ech_Manche_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
  annotate('text', x = year_half, y = rep(14.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 3, colour = 'grey80') +
  #scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_Manche_select5A_2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(14.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 3, colour = 'black') +
  #scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_Manche_select5A_3.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')

write.csv2(fq_month_heatmap_gg,file="output/data/Ech_Manche.csv", row.names = FALSE,dec = ".")



# Atlantique

Table <- select(Table.Atlantic_select, -c(Year,Month))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')


# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:21]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(levels(as.factor(fq_month_heatmap_gg$year)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(levels(as.factor(fq_month_heatmap_gg$year)), '-F', sep = '') ################ IDEM

ggplot(fq_month_heatmap_gg) +
  geom_point(aes(y = site, x = date, 
                 size = factor(sampling_effort), 
                 colour = factor(sampling_effort))) +
  scale_colour_viridis_d() +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
  annotate('text', x = year_half, y = rep(19.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 2, colour = 'grey20') +
  scale_x_discrete(labels = rep(seq(1,12,1),45)) + # A ADAPTER (ERREUR INDIQUE)
  theme_classic(base_size = 15) +             ################ POUR MIEUX VOIR 
  theme(axis.text.x = element_text(size = 4), ################ IDEM
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(size = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE),
         colour = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE))

ggsave('Ech_Atlantic_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
  annotate('text', x = year_half, y = rep(19.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 3, colour = 'grey80') +
  #scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_Atlantic_select5A_2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(19.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 3, colour = 'black') +
  #scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_Atlantic_select5A_3.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')
write.csv2(fq_month_heatmap_gg,file="output/data/Ech_Atlantic.csv", row.names = FALSE,dec = ".")



# Mediterranee

Table <- select(Table.Med_select, -c(Year,Month))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')


# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:17]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(levels(as.factor(fq_month_heatmap_gg$year)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(levels(as.factor(fq_month_heatmap_gg$year)), '-F', sep = '') ################ IDEM

ggplot(fq_month_heatmap_gg) +
  geom_point(aes(y = site, x = date, 
                 size = factor(sampling_effort), 
                 colour = factor(sampling_effort))) +
  scale_colour_viridis_d() +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
  annotate('text', x = year_half, y = rep(15.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 2, colour = 'grey20') +
  scale_x_discrete(labels = rep(seq(1,12,1),45)) + # A ADAPTER (ERREUR INDIQUE)
  theme_classic(base_size = 15) +             ################ POUR MIEUX VOIR 
  theme(axis.text.x = element_text(size = 4), ################ IDEM
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(size = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE),
         colour = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE))

ggsave('Ech_Med_select5A.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
  annotate('text', x = year_half, y = rep(15.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 3, colour = 'grey80') +
  #scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_Med_select5A_2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(15.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 3, colour = 'black') +
  #scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_Med_select5A_3.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')
write.csv2(fq_month_heatmap_gg,file="output/data/Ech_Med.csv", row.names = FALSE,dec = ".")


#### Echantillonnage pour toutes les stations selectionnees ET periode par station####
# PAR FACADE 

# Manche

Table <- select(Table.Manche_select, -c(Year,Month))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')


# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:16]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(levels(as.factor(fq_month_heatmap_gg$year)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(levels(as.factor(fq_month_heatmap_gg$year)), '-F', sep = '') ################ IDEM

ggplot(fq_month_heatmap_gg) +
  geom_point(aes(y = site, x = date, 
                 size = factor(sampling_effort), 
                 colour = factor(sampling_effort))) +
  scale_colour_viridis_d() +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
  annotate('text', x = year_half, y = rep(14.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 2, colour = 'grey20') +
  scale_x_discrete(labels = rep(seq(1,12,1),45)) + # A ADAPTER (ERREUR INDIQUE)
  theme_classic(base_size = 15) +             ################ POUR MIEUX VOIR 
  theme(axis.text.x = element_text(size = 4), ################ IDEM
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(size = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE),
         colour = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE))

ggsave('Ech_Manche_select_period1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
  annotate('text', x = year_half, y = rep(14.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 3, colour = 'grey80') +
  #scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_Manche_select_period2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(14.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 3, colour = 'black') +
  #scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_Manche_select_period3.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')

write.csv2(fq_month_heatmap_gg,file="output/data/Ech_Manche_period.csv", row.names = FALSE,dec = ".")



# Atlantique

Table <- select(Table.Atlantic_select, -c(Year,Month))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')


# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:21]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(levels(as.factor(fq_month_heatmap_gg$year)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(levels(as.factor(fq_month_heatmap_gg$year)), '-F', sep = '') ################ IDEM

ggplot(fq_month_heatmap_gg) +
  geom_point(aes(y = site, x = date, 
                 size = factor(sampling_effort), 
                 colour = factor(sampling_effort))) +
  scale_colour_viridis_d() +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
  annotate('text', x = year_half, y = rep(19.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 2, colour = 'grey20') +
  scale_x_discrete(labels = rep(seq(1,12,1),45)) + # A ADAPTER (ERREUR INDIQUE)
  theme_classic(base_size = 15) +             ################ POUR MIEUX VOIR 
  theme(axis.text.x = element_text(size = 4), ################ IDEM
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(size = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE),
         colour = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE))

ggsave('Ech_Atlantic__period1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
  annotate('text', x = year_half, y = rep(19.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 3, colour = 'grey80') +
  #scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_Atlantic_period2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(19.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 3, colour = 'black') +
  #scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_Atlantic_period3.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')
write.csv2(fq_month_heatmap_gg,file="output/data/Ech_Atlantic_period.csv", row.names = FALSE,dec = ".")



# Mediterranee

Table <- select(Table.Med_select, -c(Year,Month))

# Transform the sampling date as date format 
Table$sampling_date <- as.Date(Table$Date, "%d/%m/%Y")

# Create three separate column for day, month and year
Table$day <- day(Table$sampling_date)
Table$month <- month(Table$sampling_date, label = F)
Table$year <- year(Table$sampling_date)

# Change the column name to match 'site'
colnames(Table)[which(names(Table) == "Code_point_Libelle")] <- "site"
colnames(Table)[which(names(Table) == "Code_point_Mnemonique")] <- "site_code"

Time_series_REPHY <- Table %>% 
  dplyr::select(site, day, month, year, sampling_date) %>%
  unique()

# Create a fq_month to visualize the sampling frequency by month
fq_month <- Time_series_REPHY %>%
  group_by(site) %>% 
  count(year, month) %>% 
  pivot_wider(names_from = "site", values_from = "n")

# Change months' numbers into letters
fq_month$lettres <- ifelse(fq_month$month == '10', 'J',
                           ifelse(fq_month$month == '11', 'K',
                                  ifelse(fq_month$month == '12', 'L',
                                         ifelse(fq_month$month == '1', 'A',
                                                ifelse(fq_month$month == '2', 'B',
                                                       ifelse(fq_month$month == '3', 'C',
                                                              ifelse(fq_month$month == '4', 'D',
                                                                     ifelse(fq_month$month == '5', 'E',
                                                                            ifelse(fq_month$month == '6', 'F',
                                                                                   ifelse(fq_month$month == '7', 'G',
                                                                                          ifelse(fq_month$month == '8', 'H',
                                                                                                 ifelse(fq_month$month == '9', 'I', NA))))))))))))

# Order by year 
fq_month <- fq_month[order(fq_month$year,fq_month$month),] 

# Create a column merging year and month 
fq_month$sampling <- paste(fq_month$year, fq_month$lettres, sep = '-')


# Heatmap
fq_month_heatmap <- as.data.frame(fq_month[1:17]) ##NOMBRE DE STATIONS A ADAPTER
# rownames(fq_month_heatmap) <- rownames(fq_month)
fq_month_heatmap[is.na(fq_month_heatmap)] <- 0
fq_month_heatmap$date <- fq_month$sampling

fq_month_heatmap_gg <- fq_month_heatmap %>%  
  pivot_longer(!c(date, year, month), names_to = 'site', values_to = 'sampling_effort')

# Order by chronological order 
fq_month_heatmap_gg <- fq_month_heatmap_gg %>%
  group_by(year, month)

## Plot with points
vertical <- paste(levels(as.factor(fq_month_heatmap_gg$year)), '-L', sep = '') ################ ANNEES A ADAPTER
year_half <- paste(levels(as.factor(fq_month_heatmap_gg$year)), '-F', sep = '') ################ IDEM

ggplot(fq_month_heatmap_gg) +
  geom_point(aes(y = site, x = date, 
                 size = factor(sampling_effort), 
                 colour = factor(sampling_effort))) +
  scale_colour_viridis_d() +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
  annotate('text', x = year_half, y = rep(15.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 2, colour = 'grey20') +
  scale_x_discrete(labels = rep(seq(1,12,1),45)) + # A ADAPTER (ERREUR INDIQUE)
  theme_classic(base_size = 15) +             ################ POUR MIEUX VOIR 
  theme(axis.text.x = element_text(size = 4), ################ IDEM
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(size = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE),
         colour = guide_legend(title = 'Sampling effort', nrow = 1, byrow = TRUE))

ggsave('Ech_Med_period1.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - Sampling frequences - Horizontal
ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_effort))) +
  scale_fill_viridis_d('Sampling effort') +
  geom_vline(xintercept = vertical, colour = 'grey80', linetype = 'dashed', linewidth = 2) +
  annotate('text', x = year_half, y = rep(15.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 3, colour = 'grey80') +
  #scale_x_discrete(labels = rep(seq(1,12,1),37)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_Med_select_period2.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')


## Heatmap - NA or not NA - Horizontal
fq_month_heatmap_gg$sampling_na <- ifelse(fq_month_heatmap_gg$sampling_effort == '0', 0, 1)

ggplot(fq_month_heatmap_gg) +
  geom_tile(aes(y = site, x = date,  
                fill = factor(sampling_na))) +
  scale_fill_manual('Sampling effort', values = c('grey40','grey80')) +
  geom_vline(xintercept = vertical, colour = 'black', linetype = 'dashed', size = 1) +
  annotate('text', x = year_half, y = rep(15.5,29),############################### A ADAPTER (ERREUR INDIQUE) + 26.5 POSITION DES ANNEES SUR LE GRAPHE
           label = levels(as.factor(fq_month_heatmap_gg$year)),
           size = 3, colour = 'black') +
  #scale_x_discrete(labels = rep(seq(1,12,1),38)) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 5),
        axis.title = element_blank(),
        legend.position = 'top') +
  guides(nrow = 1, byrow = TRUE)
ggsave('Ech_Med_period3.png', path = "C:/Users/jeany/OneDrive - etu.sorbonne-universite.fr/Stage ISOMER M2/Projet_R/output/graphs/ech_description",dpi = 600, width = 400, height = 300, units = 'mm')
write.csv2(fq_month_heatmap_gg,file="output/data/Ech_Med_period.csv", row.names = FALSE,dec = ".")
