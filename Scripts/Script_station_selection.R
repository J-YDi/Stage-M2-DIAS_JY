Table <- read_delim("data_modif/Table_FLORTOT_Surf_9523_hydro_phyto_chloro_phylum.csv", 
                    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                        grouping_mark = ""), trim_ws = TRUE)

Table_select <- filter(Table, Code_point_Libelle == "Point 1 Dunkerque" |
                         Code_point_Libelle == "Point 1 Boulogne" |
                         Code_point_Libelle == "Bif" |
                         Code_point_Libelle == "At so"|
                         Code_point_Libelle == "Sète mer"|
                         Code_point_Libelle == "Parc Leucate 2"|
                         Code_point_Libelle == "Grand Rhône"|
                         Code_point_Libelle == "Bouzigues (a)"|
                         Code_point_Libelle == "Barcares"|
                         Code_point_Libelle == "Antoine"|
                         Code_point_Libelle == "Anse de Carteau 2"|
                         Code_point_Libelle == "Géfosse"|
                         Code_point_Libelle == "Cabourg"|
                         Code_point_Libelle == "Antifer ponton pétrolier"|
                         Code_point_Libelle == "Villefranche"|
                         Code_point_Libelle == "Sud Bastia"|
                         Code_point_Libelle == "Lazaret (a)"|
                         Code_point_Libelle == "Endoume"|
                         Code_point_Libelle == "Diana centre"|
                         Code_point_Libelle == "Calvi"|
                         Code_point_Libelle == "22B - Toulon gde rade"|
                         Code_point_Libelle == "Ouest Loscolo"|
                         Code_point_Libelle == "Nord Saumonards"|
                         Code_point_Libelle == "Le Croisic (a)"|
                         Code_point_Libelle == "Le Cornard"|
                         Code_point_Libelle == "La Palmyre"|
                         Code_point_Libelle == "Filière w"|
                         Code_point_Libelle == "Boyard"|
                         Code_point_Libelle == "Bois de la Chaise large"|
                         Code_point_Libelle == "Basse Michaud"|
                         Code_point_Libelle == "Auger" |
                         Code_point_Libelle == "Teychan bis"|
                         Code_point_Libelle == "Arcachon - Bouée 7"|
                         Code_point_Libelle == "Ouessant - Youc'h korz"|
                         Code_point_Libelle == "Men er Roue"|
                         Code_point_Libelle == "Men Du"|
                         Code_point_Libelle == "Le Passage (a)"|
                         Code_point_Libelle == "Lanvéoc large"|
                         Code_point_Libelle == "Kervel large"|
                         Code_point_Libelle == "Concarneau large"|
                         Code_point_Libelle == "St Pol large"|
                         Code_point_Libelle == "St Cast"|
                         Code_point_Libelle == "Pen al Lann"|
                         Code_point_Libelle == "Loguivy"|
                         Code_point_Libelle == "les Hébihens"|
                         Code_point_Libelle == "Donville"|
                         Code_point_Libelle == "Bréhat" |
                         Code_point_Libelle == "Marseillan (a)"
                       )

# Creation de tableau par facade pour les stations selectionnees
# Channel
Table.Manche_select <- filter(Table_select, Code.Region %in% c(11,12,13))
# Atlantic
Table.Atlantic_select <- filter(Table_select, Code.Region %in% c(21,22,23))
# Mediterranean
Table.Med_select <- filter(Table_select, Code.Region %in% c(31,32))

# Selection avec periode par station interet 
Table_select <- filter(Table, Code_point_Libelle == "Point 1 Dunkerque" & (Date >= as.Date("2002-04-01") & Date <= as.Date("2012-11-30"))|
                         Code_point_Libelle == "Point 1 Boulogne" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Bif" & (Date >= as.Date("2009-08-01") & Date <= as.Date("2017-06-30"))|
                         Code_point_Libelle == "At so" & (Date >= as.Date("2003-03-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Sète mer" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2023-01-31"))|
                         Code_point_Libelle == "Parc Leucate 2" & (Date >= as.Date("1995-02-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Grand Rhône" & (Date >= as.Date("2008-01-01") & Date <= as.Date("2015-08-31"))|
                         Code_point_Libelle == "Bouzigues (a)" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2023-02-28"))|
                         Code_point_Libelle == "Barcares" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Antoine" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2003-01-31"))|
                         Code_point_Libelle == "Anse de Carteau 2" & (Date >= as.Date("2003-02-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Géfosse" & (Date >= as.Date("2004-01-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Cabourg" & (Date >= as.Date("2002-03-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Antifer ponton pétrolier" & (Date >= as.Date("2002-04-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Villefranche" & (Date >= as.Date("1998-10-01") & Date <= as.Date("2014-04-30"))|
                         Code_point_Libelle == "Sud Bastia" & (Date >= as.Date("2007-04-01") & Date <= as.Date("2020-02-29"))|
                         Code_point_Libelle == "Lazaret (a)" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2015-09-30"))|
                         Code_point_Libelle == "Endoume" & (Date >= as.Date("2006-03-01") & Date <= as.Date("2016-02-29"))|
                         Code_point_Libelle == "Diana centre" & (Date >= as.Date("2001-01-01") & Date <= as.Date("2022-11-30"))|
                         Code_point_Libelle == "Calvi" & (Date >= as.Date("2006-02-01") & Date <= as.Date("2022-10-31"))|
                         Code_point_Libelle == "22B - Toulon gde rade" & (Date >= as.Date("2006-02-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Ouest Loscolo" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Nord Saumonards" & (Date >= as.Date("2009-03-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Le Croisic (a)" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2015-12-31"))|
                         Code_point_Libelle == "Le Cornard" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "La Palmyre" & (Date >= as.Date("2011-01-01") & Date <= as.Date("2020-06-30"))|
                         Code_point_Libelle == "Filière w" & (Date >= as.Date("2012-03-01") & Date <= as.Date("2023-05-31"))|
                         Code_point_Libelle == "Boyard" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2015-07-30"))|
                         Code_point_Libelle == "Bois de la Chaise large" & (Date >= as.Date("2007-01-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Basse Michaud" & (Date >= as.Date("2016-01-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Auger" & (Date >= as.Date("1995-04-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Teychan bis" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Arcachon - Bouée 7" & (Date >= as.Date("2003-02-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Ouessant - Youc'h korz" & (Date >= as.Date("2015-03-01") & Date <= as.Date("2022-01-31"))|
                         Code_point_Libelle == "Men er Roue" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2022-12-31"))|
                         Code_point_Libelle == "Men Du" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2003-12-31"))|
                         Code_point_Libelle == "Le Passage (a)" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2000-12-31"))|
                         Code_point_Libelle == "Lanvéoc large" & (Date >= as.Date("2010-01-01") & Date <= as.Date("2020-08-31"))|
                         Code_point_Libelle == "Kervel large" & (Date >= as.Date("2013-11-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Concarneau large" & (Date >= as.Date("2010-01-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "St Pol large" & (Date >= as.Date("2013-01-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "St Cast" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2004-03-31"))|
                         Code_point_Libelle == "Pen al Lann" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2006-02-28"))|
                         Code_point_Libelle == "Loguivy" & (Date >= as.Date("2007-03-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "les Hébihens" & (Date >= as.Date("2007-03-01") & Date <= as.Date("2023-06-30"))|
                         Code_point_Libelle == "Donville" & (Date >= as.Date("2002-04-01") & Date <= as.Date("2014-08-31"))|
                         Code_point_Libelle == "Bréhat" & (Date >= as.Date("1995-01-01") & Date <= as.Date("2003-06-30"))|
                         Code_point_Libelle == "Marseillan (a)" & (Date >= as.Date("2009-03-01") & Date <= as.Date("2023-02-28"))
)
              

write.csv2(Table_select,file="data_modif/Table_FLORTOT_Surf_9523_Stselect_hydro_phyto_chloro_phylum_period.csv", row.names = FALSE,dec = ".")

                         
# Creation de tableau par facade pour les stations selectionnees
# Channel
Table.Manche_select <- filter(Table_select, Code.Region %in% c(11,12,13))
# Atlantic
Table.Atlantic_select <- filter(Table_select, Code.Region %in% c(21,22,23))
# Mediterranean
Table.Med_select <- filter(Table_select, Code.Region %in% c(31,32))

