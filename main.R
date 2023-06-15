data = stat_acc_V3


###                        #Préparation des données#                        ###      



#changement de donnée pour les catégories de véhicule
data$descr_cat_veh<-factor(data$descr_cat_veh)
data$descr_cat_veh<-as.numeric(data$descr_cat_veh)


#changement de donnée pour l'agglomération ou non
data$descr_agglo<-factor(data$descr_agglo)
data$descr_agglo<-as.numeric(data$descr_agglo)


#changemenent de donnée athmoshéprique
data$descr_athmo<-factor(data$descr_athmo)
data$descr_athmo<-as.numeric(data$descr_athmo)


#changement de donnée pour lumière
data$descr_lum<-factor(data$descr_lum)
data$descr_lum<-as.numeric(data$descr_lum)


#changement de donnée pour létat de la surface
data$descr_etat_surf<-factor(data$descr_etat_surf)
data$descr_etat_surf<-as.numeric(data$descr_etat_surf)


#changement de donnée pour intersection
data$description_intersection<-factor(data$description_intersection)
data$description_intersection<-as.numeric(data$description_intersection)


#changement de donnée pour le dispositif de sécurité
data$descr_dispo_secu<-factor(data$descr_dispo_secu)
data$descr_dispo_secu<-as.numeric(data$descr_dispo_secu)


#changement de donnée pour la descritpion de la personne après l'accident
data$descr_grav<-factor(data$descr_grav)
data$descr_grav<-as.numeric(data$descr_grav)


#changement de donnée pour le motif du trajet
data$descr_motif_traj<-factor(data$descr_motif_traj)
data$descr_motif_traj<-as.numeric(data$descr_motif_traj)


#changement de donnée pour le type de collision
data$descr_type_col<-factor(data$descr_type_col)
data$descr_type_col<-as.numeric(data$descr_type_col)




# Mise au format numérique des codes INSEE

# Mise au format numérique des latitudes
data$latitude<-as.numeric(data$latitude)

# mise au format numérique des longitudes
data$longitude<-as.numeric(data$longitude)

# mise au format numérique des id_usa
data$id_usa<-as.numeric(data$id_usa)

# mise au format numérique des ages
data$age<-as.numeric(data$age)

# mise au format numérique des places
data$place<-as.numeric(data$place)




###                   Catégorisation par région                   ###

#Permet de créer une nouvelle colonne pour regrouper les départements
data$num_depart[1]=0
for(i in 1:length(data)){
  code_insee<- substr(data$id_code_insee[i], start = 1, stop = 2)
  data$num_depart[i]<-code_insee
}

#Permet de regrouper les départements par région
data$région[1]=0
for (i in 1:length(data)){
  var=data$num_depart[i]
  
  if (var==50 || var==14 || var==61 || var==76 || var==27 ){
    data$région[i]<-"Normandie";}
  if (var==44 || var==53 || var==85 || var==49 || var==72 ){
    data$région[i]<-"Pays de la Loire";}
  if (var==29 || var==22 || var==35 || var==56){
    data$région[i]<-"Bretagne";}
  if (var==28 || var==41 || var==37 || var==36 || var==18|| var==45 ){
    data$région[i]<-"Centre Val de Loire";}
  if (var==62 || var==60 || var==80 || var==02 || var==59 ){
    data$région[i]<-"Hauts-de-France";}
  if (var==77 || var==91 || var==78 || var==95 || var==92 || var==93 || var==94 || var==75 ){
    data$région[i]<-"Ile-de-France";}
  if (var==08 || var==51 || var==10 || var==52 || var==55 || var==57 || var==54 || var==88 || var==68 || var==67 || var==90 ){
    data$région[i]<-"Grandd Est";}
  if (var==89 || var==58 || var==21 || var==71 || var==39|| var==70 || var==25 ){
    data$région[i]<-"Bourgogne-Franche-Comté";}
  if (var==03 || var==63 || var==15 || var==43 || var==42|| var==69 || var==07|| var==01|| var==38|| var==26|| var==73|| var==74|| var==84){
    data$région[i]<-"Auvergne-Rhône-Alpes";}
  if (var==79 || var==86 || var==17 || var==16 || var==87|| var==23 || var==19|| var==24|| var==47|| var==33|| var==40|| var==64 ){
    data$région[i]<-"Nouvelle-Aquitaine";}
  if (var==32 || var==65 || var==31 || var==09 || var==82|| var==46 || var==81|| var==11|| var==66|| var==34|| var==30|| var==48|| var==12 ){
    data$région[i]<-"Occitanie";}
  if (var==13 || var==81 || var==83 || var==04 || var==05|| var==06 ){
    data$région[i]<-"Provence-Alpes Côte d'Azur";}
  if (var=="2A" ||var=="2B"){
    data$région[i]<-"Corse";}
  if (var==97){
    data$région[i]<-"Outre-Mer";}
}




#Changement de latitude et longitude pour les arrondissement de Marseille/Paris/Lyon car il y a des incohérences

for (i in 1:length(data)){
  # Lyon
  if (data$ville[i] %in% c("LYON 01", "LYON 02", "LYON 03", "LYON 04", "LYON 05", "LYON 06", "LYON 07", "LYON 08", "LYON 09")) {
    data$latitude[i] <- 45.75
    data$longitude[i] <- 4.85
  }
  
  # Marseille
  if (data$ville[i] %in% c("MARSEILLE 01", "MARSEILLE 02", "MARSEILLE 03", "MARSEILLE 04", "MARSEILLE 05", "MARSEILLE 06", "MARSEILLE 07", "MARSEILLE 08", "MARSEILLE 09", "MARSEILLE 10", "MARSEILLE 11", "MARSEILLE 12", "MARSEILLE 13", "MARSEILLE 14", "MARSEILLE 15", "MARSEILLE 16")) {
    data$latitude[i] <- 43.30
    data$longitude[i] <- 5.40
  }
  
  # Paris
  if(data$ville[i] %in% c("PARIS 01","PARIS 02","PARIS 03","PARIS 04","PARIS 05","PARIS 06","PARIS 07","PARIS 08","PARIS 09","PARIS 10","PARIS 11","PARIS 12","PARIS 13","PARIS 14","PARIS 15","PARIS 16","PARIS 17","PARIS 18","PARIS 19","PARIS 20")) {
    data$latitude[i] <- 48.86
    data$longitude[i] <- 2.34
  }
}

###                   Construire des séries chronologiques                    ###


# Agréger les données par jour
accidents_par_jour <- aggregate(id_usa ~ data$date, data, FUN = length)

# Agréger les données par mois
accidents_par_mois <- aggregate(id_usa ~ format(date, "%Y-%m"), data, FUN = length)

# Agréger les données par semaine
accidents_par_semaine <- aggregate(id_usa ~ format(date, "%U"), data, FUN = length)

# Agréger les données par année
accidents_par_an <- aggregate(id_usa ~ format(date, "%Y"), data, FUN = length)



# Agréger les données par catégorie de blessure et par région pour 1000.000 habitants
accidents_indemne <- aggregate(id_usa ~région +descr_grav, data, FUN = length)
nb_cas=accidents_indemne[3]
cas_reg=accidents_indemne[1]
X=c(3304000,3539000,3175000,2539000,5994000,11730000,5531000,2811000,7518000,5708000,5474000,4889000,305674,252338)
Y=c('Normandie','Pays de la Loire','Bretagne','Centre Val de Loire','Hauts-de-France',"Ile-de-France","Grandd Est","Bourgogne-Franche-Comté","Auvergne-Rhône-Alpes","Nouvelle-Aquitaine","Occitanie","Provence-Alpes Côte d'Azur","Corse","Outre-Mer")


for (i in 1:length(cas_reg)){
  for(j in 1:length(Y)){
    if (cas_reg[i,]==Y[j]){
      nb_cas=nb_cas[i]*100000
      nb_cas=nb_cas/X[j]
    }}}
accidents_indemne[3]=nb_cas



#à faire à la fin pour ne pas créer de problème
# Supprimer les lignes contenant des valeurs manquantes
data1<- na.omit(data)
data=data1

#ici je viens mettre cette ligne pour permettre la modification du tableau dans rstudio
stat_acc_V3=data

###                   Visualisation                   ###

##    Représentation graphique    ##

#Nombre d’accidents en fonction des conditions atmosphériques
x = data$descr_athmo
barplot(table(x), xlab= "condition atmosphérique", ylab="Nombre d'accident", col="green", main = "Nombre d’accidents en fonction des conditions atmosphériques" )


#Nombre d’accidents en fonction de la description de la surface
y = data$descr_etat_surf
barplot(table(y), xlab= " description de la surface", ylab="Nombre d'accident", col="blue", main = "Nombre d’accidents en fonction de la description de la surface" )


#Nombre d’accidents selon la gravité
z = data$descr_grav
barplot(table(z), xlab= " description de la gravité", ylab="Nombre d'accident", col="red", main = "Nombre d’accidents selon la gravité" )

#Nombre d’accidents par tranche d'heure

barplot(table(format(data$date, "%H")), xlab= " tranche d'heure", ylab="Nombre d'accident", col="orange", main = "Nombre d’accidents par tranche d'heure" )

#Nombre d’accidents par ville  avec barplot
b = data$ville
barplot(table(b), xlab= " ville", ylab="Nombre d'accident", main = "Nombre d’accidents par ville" )

##     Histogramme     ##

#histogramme quantité d’accidents en fonction des tranches d’âges

# Définir les tranches d'âges spécifiées
tranches_age <- c("0-10", "11-20","21-30","31-40","41-50","51-60","61 +")

# Regrouper les données en fonction des tranches d'âges spécifiées
accidents <- cut(data$age, breaks = c(0, 10, 20, 30, 40, 50, 60, max(data$age) + 1), labels = tranches_age, include.lowest = TRUE)

# Calculer le nombre d'accidents dans chaque tranche d'âges
accidents_par_tranche <- table(accidents)

# Créer le graphique à barres
barplot(accidents_par_tranche, main = "Quantité d'accidents en fonction des tranches d'âges",
        xlab = "Tranches d'âges", ylab = "Nombre d'accidents")

#histogramme moyenne mensuelle des accidents

# Créer le graphique à barres
barplot(accidents_par_mois$id_usa, main = "Moyenne mensuelle des accidents",
        xlab = "mois", ylab = "Nombre d'accidents")


library(leaflet)

# Initialisation des variables
compteur <- 0
latitude <- 0
longitude <- 0

compteur1 <- 0
latitude1 <- 0
longitude1 <- 0

compteur2 <- 0
latitude2 <- 0
longitude2 <- 0

compteur3 <- 0
latitude3 <- 0
longitude3 <- 0

compteur4 <- 0
latitude4 <- 0
longitude4 <- 0

compteur5 <- 0
latitude5 <- 0
longitude5 <- 0

compteur6 <- 0
latitude6 <- 0
longitude6 <- 0

compteur7 <- 0
latitude7 <- 0
longitude7 <- 0

compteur8 <- 0
latitude8 <- 0
longitude8 <- 0

compteur9 <- 0
latitude9 <- 0
longitude9 <- 0

compteur11 <- 0
latitude11 <- 0
longitude11 <- 0

compteur12 <- 0
latitude12 <- 0
longitude12 <- 0

compteur13 <- 0
latitude13 <- 0
longitude13 <- 0

compteur14 <- 0
latitude14 <- 0
longitude14 <- 0

# Boucle pour calculer les coordonnées moyennes
missing_values <- is.na(stat_acc_V3$latitude) | is.na(stat_acc_V3$longitude)
for (i in 1:70052) {
  if (stat_acc_V3$région[i] == "Normandie" && !missing_values[i]) {
    compteur <- compteur + 1
    latitude <- latitude + stat_acc_V3$latitude[i]
    longitude <- longitude + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Pays de la Loire"&& !missing_values[i]) {
    compteur1 <- compteur1 + 1
    latitude1 <- latitude1 + stat_acc_V3$latitude[i]
    longitude1 <- longitude1 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Bretagne"&& !missing_values[i]) {
    compteur2 <- compteur2 + 1
    latitude2 <- latitude2 + stat_acc_V3$latitude[i]
    longitude2 <- longitude2 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Centre Val de Loire"&& !missing_values[i]) {
    compteur3 <- compteur3 + 1
    latitude3 <- latitude3 + stat_acc_V3$latitude[i]
    longitude3 <- longitude3 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Hauts-de-France"&& !missing_values[i]) {
    compteur4 <- compteur4 + 1
    latitude4 <- latitude4 + stat_acc_V3$latitude[i]
    longitude4 <- longitude4 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Grandd Est"&& !missing_values[i]) {
    compteur5 <- compteur5 + 1
    latitude5 <- latitude5 + stat_acc_V3$latitude[i]
    longitude5 <- longitude5 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Ile-de-France"&& !missing_values[i]) {
    compteur6 <- compteur6 + 1
    latitude6 <- latitude6 + stat_acc_V3$latitude[i]
    longitude6 <- longitude6 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Outre-Mer"&& !missing_values[i]) {
    compteur7 <- compteur7 + 1
    latitude7 <- latitude7 + stat_acc_V3$latitude[i]
    longitude7 <- longitude7 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Bourgogne-Franche-Comté"&& !missing_values[i]) {
    compteur8 <- compteur8 + 1
    latitude8 <- latitude8 + stat_acc_V3$latitude[i]
    longitude8 <- longitude8 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Auvergne-Rhône-Alpes"&& !missing_values[i]) {
    compteur9 <- compteur9 + 1
    latitude9 <- latitude9 + stat_acc_V3$latitude[i]
    longitude9 <- longitude9 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Nouvelle-Aquitaine"&& !missing_values[i]) {
    compteur11 <- compteur11 + 1
    latitude11 <- latitude11 + stat_acc_V3$latitude[i]
    longitude11 <- longitude11 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Occitanie"&& !missing_values[i]) {
    compteur12 <- compteur12 + 1
    latitude12 <- latitude12 + stat_acc_V3$latitude[i]
    longitude12 <- longitude12 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Provence-Alpes Côte d'Azur"&& !missing_values[i]) {
    compteur13 <- compteur13 + 1
    latitude13 <- latitude13 + stat_acc_V3$latitude[i]
    longitude13 <- longitude13 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Corse"&& !missing_values[i]) {
    compteur14 = compteur14 + 1
    latitude14 =latitude14 + stat_acc_V3$latitude[i]
    longitude14 = longitude14 + stat_acc_V3$longitude[i]
  }
}

# Création de la carte
m <- leaflet() %>%
  addTiles() %>% 
  
  addCircleMarkers(lng = longitude / compteur, lat = latitude / compteur,
                   radius = compteur/1000, popup = "Normandie")%>%
  
  addCircleMarkers(lng = longitude1 / compteur1, lat = latitude1 / compteur1,
                   radius = compteur1/1000, popup = "Pays de la Loire")%>%
  
  addCircleMarkers(lng = longitude2 / compteur2, lat = latitude2 / compteur2,
                   radius = compteur2/1000, popup = "Bretagne")%>%
  
  addCircleMarkers(lng = longitude3 / compteur3, lat = latitude3 / compteur3,
                   radius = compteur3/1000, popup = "Centre Val de Loire")%>%
  
  addCircleMarkers(lng = longitude4 / compteur4, lat = latitude4 / compteur4,
                   radius = compteur4/1000, popup = "Hauts-de-France")%>%
  
  addCircleMarkers(lng = longitude5 / compteur5, lat = latitude5 / compteur5,
                   radius = compteur5/1000, popup = "Grandd Est")%>%
  
  addCircleMarkers(lng = longitude6 / compteur6, lat = latitude6 / compteur6,
                   radius = compteur6/1000, popup = "Ile-de-France")%>%
  
  addCircleMarkers(lng = longitude7 / compteur7, lat = latitude7 / compteur7,
                   radius = compteur7/1000, popup = "Outre-Mer")%>%
  
  addCircleMarkers(lng = longitude8 / compteur8, lat = latitude8 / compteur8,
                   radius = compteur8/1000, popup = "Bourgogne-Franche-Comté")%>%
  
  addCircleMarkers(lng = longitude9 / compteur9, lat = latitude9 / compteur9,
                   radius = compteur9/1000, popup = "Auvergne-Rhône-Alpes")%>%
  
  addCircleMarkers(lng = longitude11 / compteur11, lat = latitude11 / compteur11,
                   radius = compteur11/1000, popup = "Nouvelle-Aquitaine")%>%
  
  addCircleMarkers(lng = longitude12 / compteur12, lat = latitude12 / compteur12,
                   radius = compteur12/1000, popup = "Occitanie")%>%
  
  addCircleMarkers(lng = longitude13 / compteur13, lat = latitude13 / compteur13,
                   radius = compteur13/1000, popup = "Provence-Alpes Côte d'Azur")%>%
  
  addCircleMarkers(lng = longitude14/compteur14 , lat = latitude14/compteur14,
                   radius = compteur14/1000, popup = "Corse")


m














for (i in 1:87) {
  assign(paste0("compteur", i), 0)
  assign(paste0("latitude", i), 0)
  assign(paste0("longitude", i), 0)
}
compteur2B=0
# Boucle pour calculer les coordonnées moyennes
missing_values <- is.na(stat_acc_V3$latitude) | is.na(stat_acc_V3$longitude)
for (i in 1:70052) {
  if (stat_acc_V3$num_depart[i] == 10 && !missing_values[i]) {
    compteur = compteur + 1
    if(compteur ==1){
      latitude = stat_acc_V3$latitude[i]
      longitude = stat_acc_V3$longitude[i]
    }
    }
    
  if (stat_acc_V3$num_depart[i] == 11 && !missing_values[i]) {
    compteur1 = compteur1 +1
    if(compteur1 == 1){
      latitude1 = stat_acc_V3$latitude[i]
      longitude1 = stat_acc_V3$longitude[i]
    }
  }
  
  if (stat_acc_V3$num_depart[i] == 12 && !missing_values[i]) {
    compteur2 = compteur2 +1
    if(compteur2 == 1){
      latitude2 = stat_acc_V3$latitude[i]
      longitude2 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 13 && !missing_values[i]) {
    compteur3 = compteur3 +1
    if(compteur3 == 1){
      latitude3 = stat_acc_V3$latitude[i]
      longitude3 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 14 && !missing_values[i]) {
    compteur4 = compteur4 +1
    if(compteur4 == 1){
      latitude4 = stat_acc_V3$latitude[i]
      longitude4 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 15 && !missing_values[i]) {
    compteur5 = compteur5 +1
    if(compteur5 == 1){
      latitude5 = stat_acc_V3$latitude[i]
      longitude5 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 16 && !missing_values[i]) {
    compteur6 = compteur6 +1
    if(compteur6 == 1){
      latitude6 = stat_acc_V3$latitude[i]
      longitude6 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 17 && !missing_values[i]) {
    compteur7 = compteur7 +1
    if(compteur7 == 1){
      latitude7 = stat_acc_V3$latitude[i]
      longitude7 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 18 && !missing_values[i]) {
    compteur8 = compteur8 +1
    if(compteur8 == 1){
      latitude8 = stat_acc_V3$latitude[i]
      longitude8 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 19 && !missing_values[i]) {
    compteur9 = compteur9 +1
    if(compteur9 == 1){
      latitude9 = stat_acc_V3$latitude[i]
      longitude9 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == "2A" && !missing_values[i]) {
    compteur10 = compteur10 +1
    if(compteur10 == 1){
      latitude10 = stat_acc_V3$latitude[i]
      longitude10 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == "2B" && !missing_values[i]) {
    compteur2B = compteur2B +1
    if(compteur2B == 1){
      latitude2B = stat_acc_V3$latitude[i]
      longitude2B = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 21 && !missing_values[i]) {
    compteur11 = compteur11 +1
    if(compteur11 == 1){
      latitude11 = stat_acc_V3$latitude[i]
      longitude11 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 22 && !missing_values[i]) {
    compteur12 = compteur12 +1
    if(compteur12 == 1){
      latitude12 = stat_acc_V3$latitude[i]
      longitude12 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 23 && !missing_values[i]) {
    compteur13 = compteur13 +1
    if(compteur13 == 1){
      latitude13 = stat_acc_V3$latitude[i]
      longitude13 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 24 && !missing_values[i]) {
    compteur14 = compteur14 +1
    if(compteur14 == 1){
      latitude14 = stat_acc_V3$latitude[i]
      longitude14 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 25 && !missing_values[i]) {
    compteur15 = compteur15 +1
    if(compteur15 == 1){
      latitude15 = stat_acc_V3$latitude[i]
      longitude15 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 26 && !missing_values[i]) {
    compteur16 = compteur16 +1
    if(compteur16 == 1){
      latitude16 = stat_acc_V3$latitude[i]
      longitude16 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 27 && !missing_values[i]) {
    compteur17 <- compteur17 + 1
    latitude17 <- latitude17 + stat_acc_V3$latitude[i]
    longitude17 <- longitude17 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 28 && !missing_values[i]) {
    compteur18 <- compteur18 + 1
    latitude18 <- latitude18 + stat_acc_V3$latitude[i]
    longitude18 <- longitude18 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 29 && !missing_values[i]) {
    compteur19 <- compteur19 + 1
    latitude19 <- latitude19 + stat_acc_V3$latitude[i]
    longitude19 <- longitude19 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 30 && !missing_values[i]) {
    compteur20 <- compteur20 + 1
    latitude20 <- latitude20 + stat_acc_V3$latitude[i]
    longitude20 <- longitude20 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 31 && !missing_values[i]) {
    compteur21 <- compteur21 + 1
    latitude21 <- latitude21 + stat_acc_V3$latitude[i]
    longitude21 <- longitude21 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 32 && !missing_values[i]) {
    compteur22 <- compteur22 + 1
    latitude22 <- latitude22 + stat_acc_V3$latitude[i]
    longitude22 <- longitude22 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 33 && !missing_values[i]) {
    compteur23 <- compteur23 + 1
    latitude23 <- latitude23 + stat_acc_V3$latitude[i]
    longitude23 <- longitude23 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 34 && !missing_values[i]) {
    compteu24 <- compteur24 + 1
    latitude24 <- latitude24 + stat_acc_V3$latitude[i]
    longitude24 <- longitude24 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 35 && !missing_values[i]) {
    compteur25 <- compteur25 + 1
    latitude25 <- latitude25 + stat_acc_V3$latitude[i]
    longitude25 <- longitude25 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 36 && !missing_values[i]) {
    compteur26 <- compteur26 + 1
    latitude26 <- latitude26 + stat_acc_V3$latitude[i]
    longitude26 <- longitude26 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 37 && !missing_values[i]) {
    compteur27 <- compteur27 + 1
    latitude27 <- latitude27 + stat_acc_V3$latitude[i]
    longitude27 <- longitude27 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 38 && !missing_values[i]) {
    compteur28 <- compteur28 + 1
    latitude28 <- latitude28 + stat_acc_V3$latitude[i]
    longitude28 <- longitude28 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 39 && !missing_values[i]) {
    compteur29 <- compteur29 + 1
    latitude29 <- latitude29 + stat_acc_V3$latitude[i]
    longitude29 <- longitude29 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 40 && !missing_values[i]) {
    compteur30 <- compteur30 + 1
    latitude30 <- latitude30 + stat_acc_V3$latitude[i]
    longitude30 <- longitude30 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 41 && !missing_values[i]) {
    compteur31 <- compteur31 + 1
    latitude31 <- latitude31 + stat_acc_V3$latitude[i]
    longitude31 <- longitude31 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 42 && !missing_values[i]) {
    compteur32 <- compteur32 + 1
    latitude32 <- latitude32 + stat_acc_V3$latitude[i]
    longitude32 <- longitude32 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 43 && !missing_values[i]) {
    compteur33 <- compteur33 + 1
    latitude33 <- latitude33 + stat_acc_V3$latitude[i]
    longitude33 <- longitude33 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 44 && !missing_values[i]) {
    compteur34 <- compteur34 + 1
    latitude34 <- latitude34 + stat_acc_V3$latitude[i]
    longitude34 <- longitude34 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 45 && !missing_values[i]) {
    compteur35 <- compteur35 + 1
    latitude35 <- latitude35 + stat_acc_V3$latitude[i]
    longitude35 <- longitude35 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 46 && !missing_values[i]) {
    compteur36 <- compteur36 + 1
    latitude36 <- latitude36 + stat_acc_V3$latitude[i]
    longitude36 <- longitude36 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 47 && !missing_values[i]) {
    compteur37 <- compteur37 + 1
    latitude37 <- latitude37 + stat_acc_V3$latitude[i]
    longitude37 <- longitude37 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 48 && !missing_values[i]) {
    compteur38 <- compteur38 + 1
    latitude38 <- latitude38 + stat_acc_V3$latitude[i]
    longitude38 <- longitude38 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 49 && !missing_values[i]) {
    compteur39 <- compteur39 + 1
    latitude39 <- latitude39 + stat_acc_V3$latitude[i]
    longitude39 <- longitude39 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 50 && !missing_values[i]) {
    compteur40 <- compteur40 + 1
    latitude40 <- latitude40 + stat_acc_V3$latitude[i]
    longitude40 <- longitude40 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 51 && !missing_values[i]) {
    compteur41 <- compteur41 + 1
    latitude41 <- latitude41 + stat_acc_V3$latitude[i]
    longitude41 <- longitude41 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 52 && !missing_values[i]) {
    compteur42 <- compteur42 + 1
    latitude42 <- latitude42 + stat_acc_V3$latitude[i]
    longitude42 <- longitude42 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 53 && !missing_values[i]) {
    compteur43 <- compteur43 + 1
    latitude43 <- latitude43 + stat_acc_V3$latitude[i]
    longitude43 <- longitude43 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 54 && !missing_values[i]) {
    compteur44 <- compteur44 + 1
    latitude44 <- latitude44 + stat_acc_V3$latitude[i]
    longitude44 <- longitude44 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 55 && !missing_values[i]) {
    compteur45 <- compteur45 + 1
    latitude45 <- latitude45 + stat_acc_V3$latitude[i]
    longitude45 <- longitude45 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 56 && !missing_values[i]) {
    compteur46 <- compteur46 + 1
    latitude46 <- latitude46 + stat_acc_V3$latitude[i]
    longitude46 <- longitude46 + stat_acc_V3$longitude[i]
  }
  
  if (stat_acc_V3$num_depart[i] == 57 && !missing_values[i]) {
    compteur47 <- compteur47 + 1
    latitude47 <- latitude47 + stat_acc_V3$latitude[i]
    longitude47 <- longitude47 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 58 && !missing_values[i]) {
    compteur48 <- compteur48 + 1
    latitude48 <- latitude48 + stat_acc_V3$latitude[i]
    longitude48 <- longitude48 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 59 && !missing_values[i]) {
    compteur49 <- compteur49 + 1
    latitude49 <- latitude49 + stat_acc_V3$latitude[i]
    longitude49 <- longitude49 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 60 && !missing_values[i]) {
    compteur50 <- compteur50 + 1
    latitude50 <- latitude50 + stat_acc_V3$latitude[i]
    longitude50 <- longitude50 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 61 && !missing_values[i]) {
    compteur51 <- compteur51 + 1
    latitude51 <- latitude51 + stat_acc_V3$latitude[i]
    longitude51 <- longitude51 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 62 && !missing_values[i]) {
    compteur52 <- compteur52 + 1
    latitude52 <- latitude52 + stat_acc_V3$latitude[i]
    longitude52 <- longitude52 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 63 && !missing_values[i]) {
    compteur53 <- compteur53 + 1
    latitude53 <- latitude53 + stat_acc_V3$latitude[i]
    longitude53 <- longitude53 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 64 && !missing_values[i]) {
    compteur54 <- compteur54 + 1
    latitude54 <- latitude54 + stat_acc_V3$latitude[i]
    longitude54 <- longitude54 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 65 && !missing_values[i]) {
    compteur55 <- compteur55 + 1
    latitude55 <- latitude55 + stat_acc_V3$latitude[i]
    longitude55 <- longitude55 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 66 && !missing_values[i]) {
    compteur56 <- compteur56 + 1
    latitude56 <- latitude56 + stat_acc_V3$latitude[i]
    longitude56 <- longitude56 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 67 && !missing_values[i]) {
    compteur57 <- compteur57 + 1
    latitude57 <- latitude57 + stat_acc_V3$latitude[i]
    longitude57 <- longitude57 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 68 && !missing_values[i]) {
    compteur58 <- compteur58 + 1
    latitude58 <- latitude58 + stat_acc_V3$latitude[i]
    longitude58 <- longitude58 + stat_acc_V3$longitude[i]
  }
  
  if (stat_acc_V3$num_depart[i] == 69 && !missing_values[i]) {
    compteur59 <- compteur59 + 1
    latitude59 <- latitude59 + stat_acc_V3$latitude[i]
    longitude59 <- longitude59 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 70 && !missing_values[i]) {
    compteur60 <- compteur60 + 1
    latitude60 <- latitude60 + stat_acc_V3$latitude[i]
    longitude60 <- longitude60 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 71 && !missing_values[i]) {
    compteur61 <- compteur61 + 1
    latitude61 <- latitude61 + stat_acc_V3$latitude[i]
    longitude61 <- longitude61 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 72 && !missing_values[i]) {
    compteur62 <- compteur62 + 1
    latitude62 <- latitude62 + stat_acc_V3$latitude[i]
    longitude62 <- longitude62 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 73 && !missing_values[i]) {
    compteur63 <- compteur63 + 1
    latitude63 <- latitude63 + stat_acc_V3$latitude[i]
    longitude63 <- longitude63 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 74 && !missing_values[i]) {
    compteur64 <- compteur64 + 1
    latitude64 <- latitude64 + stat_acc_V3$latitude[i]
    longitude64 <- longitude64 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 75 && !missing_values[i]) {
    compteur65 <- compteur66 + 1
    latitude65 <- latitude66 + stat_acc_V3$latitude[i]
    longitude65 <- longitude66 + stat_acc_V3$longitude[i]
  }
  
  if (stat_acc_V3$num_depart[i] == 76 && !missing_values[i]) {
    compteur66 <- compteur66 + 1
    latitude66 <- latitude66 + stat_acc_V3$latitude[i]
    longitude66 <- longitude66 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 77 && !missing_values[i]) {
    compteur67 <- compteur67 + 1
    latitude67 <- latitude67 + stat_acc_V3$latitude[i]
    longitude67 <- longitude67 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 78 && !missing_values[i]) {
    compteur68 <- compteur68 + 1
    latitude68 <- latitude68 + stat_acc_V3$latitude[i]
    longitude68 <- longitude68 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 79 && !missing_values[i]) {
    compteur69 <- compteur69 + 1
    latitude69 <- latitude69 + stat_acc_V3$latitude[i]
    longitude69 <- longitude69 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 80 && !missing_values[i]) {
    compteur70 <- compteur70 + 1
    latitude70 <- latitude70 + stat_acc_V3$latitude[i]
    longitude70 <- longitude70 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 81 && !missing_values[i]) {
    compteur71 <- compteur71 + 1
    latitude71 <- latitude71 + stat_acc_V3$latitude[i]
    longitude71 <- longitude71 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 82 && !missing_values[i]) {
    compteur72 <- compteur72 + 1
    latitude72 <- latitude72 + stat_acc_V3$latitude[i]
    longitude72 <- longitude72 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 83 && !missing_values[i]) {
    compteur73 <- compteur73 + 1
    latitude73 <- latitude73 + stat_acc_V3$latitude[i]
    longitude73 <- longitude73 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 84 && !missing_values[i]) {
    compteur74 <- compteur74 + 1
    latitude74 <- latitude74 + stat_acc_V3$latitude[i]
    longitude74 <- longitude74 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 85 && !missing_values[i]) {
    compteur75 <- compteur75 + 1
    latitude75 <- latitude75 + stat_acc_V3$latitude[i]
    longitude75 <- longitude75 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 86 && !missing_values[i]) {
    compteur76 <- compteur76 + 1
    latitude76 <- latitude76 + stat_acc_V3$latitude[i]
    longitude76 <- longitude76 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 87 && !missing_values[i]) {
    compteur77 <- compteur77 + 1
    latitude77 <- latitude77 + stat_acc_V3$latitude[i]
    longitude77 <- longitude77 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 88 && !missing_values[i]) {
    compteur78 <- compteur78 + 1
    latitude78 <- latitude78 + stat_acc_V3$latitude[i]
    longitude78 <- longitude78 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 89 && !missing_values[i]) {
    compteur79 <- compteur79 + 1
    latitude79 <- latitude79 + stat_acc_V3$latitude[i]
    longitude79 <- longitude79 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 90 && !missing_values[i]) {
    compteur80 <- compteur80 + 1
    latitude80 <- latitude80 + stat_acc_V3$latitude[i]
    longitude80 <- longitude80 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 91 && !missing_values[i]) {
    compteur81 <- compteur81 + 1
    latitude81 <- latitude81 + stat_acc_V3$latitude[i]
    longitude81 <- longitude81 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 92 && !missing_values[i]) {
    compteur82 <- compteur82 + 1
    latitude82 <- latitude82 + stat_acc_V3$latitude[i]
    longitude82 <- longitude82 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 93 && !missing_values[i]) {
    compteur83 <- compteur83 + 1
    latitude83 <- latitude83 + stat_acc_V3$latitude[i]
    longitude83 <- longitude83 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 94 && !missing_values[i]) {
    compteur84 <- compteur84 + 1
    latitude84 <- latitude84 + stat_acc_V3$latitude[i]
    longitude84 <- longitude84 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 95 && !missing_values[i]) {
    compteur85 <- compteur85 + 1
    latitude85 <- latitude85 + stat_acc_V3$latitude[i]
    longitude85 <- longitude85 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 96 && !missing_values[i]) {
    compteur86 <- compteur86 + 1
    latitude86 <- latitude86 + stat_acc_V3$latitude[i]
    longitude86 <- longitude86 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$num_depart[i] == 97 && !missing_values[i]) {
    compteur87 <- compteur87 + 1
    latitude87 <- latitude87 + stat_acc_V3$latitude[i]
    longitude87 <- longitude87 + stat_acc_V3$longitude[i]
  }
}


# Création de la carte
m2 <- leaflet() %>%
  addTiles() %>% 
  
  addCircleMarkers(lng = longitude, lat = latitude ,
                   radius = compteur/1000, popup = "10")%>%
  
  addCircleMarkers(lng = longitude1, lat = latitude1 ,
                   radius = compteur1/1000, popup = "11")%>%

  addCircleMarkers(lng = longitude2, lat = latitude2 ,
                    radius = compteur2/1000, popup = "12")%>%

addCircleMarkers(lng = longitude3, lat = latitude3 ,
                 radius = compteur3/1000, popup = "13")%>%

addCircleMarkers(lng = longitude4, lat = latitude4 ,
                 radius = compteur4/1000, popup = "14")%>%

addCircleMarkers(lng = longitude5, lat = latitude5 ,
                 radius = compteur5/1000, popup = "15")%>%

  addCircleMarkers(lng = longitude6, lat = latitude6 ,
                   radius = compteur6/1000, popup = "16")%>%
  
  addCircleMarkers(lng = longitude7, lat = latitude7 ,
                   radius = compteur7/1000, popup = "17")%>%
  
  addCircleMarkers(lng = longitude8, lat = latitude8 ,
                   radius = compteur8/1000, popup = "18")%>%
  
  addCircleMarkers(lng = longitude9, lat = latitude9 ,
                   radius = compteur9/1000, popup = "19")%>%
  
  addCircleMarkers(lng = longitude10, lat = latitude10 ,
                   radius = compteur10/1000, popup = "2A")%>%
  
  addCircleMarkers(lng = longitude2B, lat = latitude2B ,
                   radius = compteur2B/1000, popup = "2B")%>%
  
  addCircleMarkers(lng = longitude11, lat = latitude11 ,
                   radius = compteur11/1000, popup = "21")%>%
  
  addCircleMarkers(lng = longitude12, lat = latitude12 ,
                   radius = compteur12/1000, popup = "22")%>%
  
  addCircleMarkers(lng = longitude13, lat = latitude13 ,
                   radius = compteur13/1000, popup = "23")%>%
  
  addCircleMarkers(lng = longitude14, lat = latitude14 ,
                   radius = compteur14/1000, popup = "24")%>%
  
  addCircleMarkers(lng = longitude15, lat = latitude15 ,
                   radius = compteur15/1000, popup = "25")




m2   
