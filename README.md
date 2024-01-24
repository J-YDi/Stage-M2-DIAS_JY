# Stage-M2-JY
Le rep github pour les scripts et fichiers de données du stage de M2 de Jean-Yves Dias

Projet_R.Rproj pour lier l'integralite des scripts et des dossiers

Dossier Scripts :

  Script_data_import.R : Code pour le traitement des donnees initiales pour creer des donnees pour les analyses     
    (selection, filtrage...). Les nouveaux tableaux sont stockees dans un dossier data_modif.
    
  Script_echantillonnage_description.R : Code pour l'analyse de l'echantillonnage des stations (fréquence).
  
  Script_data_description.R : Code pour l'analyse de la description des donnees (Stats descriptives de chacun des 
    parametres biologiques ou abiotiques.
    
  Script_station_selection.R : Code final selection des stations : jeu de donnees final pour debut du stage 
  (redondant avec Script_data_import uniquement pour cela). 
  
  Script_CHLOROA_analysis.R : Code pour l'analyse du conflit entre les methodes de mesures de la chlorophylle et pour 
  definir la.les methodes a preserver (ou ne pas preserver la chl-a)


Dossier data :
  Dossier necessaire pour le lien avec le Projet_R.Rproj, contient les donnees initiales, qui sont utilisees dans 
  Script_data_import.R . Les donnees sont trop importantes en taille pour y etre stockées, le fichier takedata permet 
  de les telecharger a partir d'un onedrive.

Dossiers crees par les scripts (non present sur le Github) :
  data_modif : lieu de stockage des nouveaux jeu de donnees à partir de Script_data_import
  output : lieu de stockage de sorties de R, contenant lui meme des dossiers crees par les scripts d'analyse
    data : resultats sous forme de tableaux
    graphs : resultats sous forme de graphes, qui peuvent eux meme avoir des dossiers par analyse
    
