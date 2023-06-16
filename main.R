#Légende pour le code 

#Mailys : M 
#Paul : P
#Baptiste : B



#Importation de notre base de donnée dans une variable
data = stat_acc_V3




###                        #Préparation des données#                        ###      

#M et B 

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

#P

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






#P

###                   Catégorisation par région                   ###

#Permet de créer une nouvelle colonne pour regrouper les départements

data$num_depart[1]=0
for(i in 1:length(data)){
  code_insee<- substr(data$id_code_insee[i], start = 1, stop = 2)
  data$num_depart[i]<-code_insee
}

#Permet de regrouper les départements par région dans une nouvelle colonne

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



#B
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







#B
###                   Construire des séries chronologiques                    ###


# Agréger les données par jour

accidents_par_jour <- aggregate(id_usa ~ data$date, data, FUN = length)

# Agréger les données par mois

accidents_par_mois <- aggregate(id_usa ~ format(date, "%m"), data, FUN = length)

# Agréger les données par semaine

accidents_par_semaine <- aggregate(id_usa ~ format(date, "%U"), data, FUN = length)

# Agréger les données par année

accidents_par_an <- aggregate(id_usa ~ format(date, "%Y"), data, FUN = length)


#P
# Agréger les données par catégorie de blessure et par région pour 100.000 habitants

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

#B
# Supprimer les lignes contenant des valeurs manquantes
data1<- na.omit(data)
data=data1

#ici je viens mettre cette ligne pour permettre la modification du tableau dans rstudio
stat_acc_V3=data








###                   Visualisation                   ###

##               Représentation graphique              ##    
#M
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





##                   Histogramme               ##
#B
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




#B et P
###       carte des régions pour les accidents en général       ###

#importation librairie pour l'affichage des cartes 

library(leaflet)

# Initialisation des variables méthode 1

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


m #affichage de la carte 











### carte des départements pour les accidents en général ###

#Initialisation des variables méthode 2
for (i in 1:88) {
  assign(paste0("compteur", i), 0)
  assign(paste0("latitude", i), 0)
  assign(paste0("longitude", i), 0)
}
compteur2B=0 #Cas particulier pour la Corse 

# Boucle pour calculer les coordonnées moyennes

missing_values <- is.na(stat_acc_V3$latitude) | is.na(stat_acc_V3$longitude)
for (i in 1:70052) {
  if (stat_acc_V3$num_depart[i] == 10 && !missing_values[i]) {
    compteur88 = compteur88 + 1
    if(compteur88 ==1){
      latitude88 = stat_acc_V3$latitude[i]
      longitude88 = stat_acc_V3$longitude[i]
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
    compteur17 = compteur17 +1
    if(compteur17 == 1){
      latitude17 = stat_acc_V3$latitude[i]
      longitude17 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 28 && !missing_values[i]) {
    compteur18 = compteur18 +1
    if(compteur18 == 1){
      latitude18 = stat_acc_V3$latitude[i]
      longitude18 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 29 && !missing_values[i]) {
    compteur19 = compteur19 +1
    if(compteur19 == 1){
      latitude19 = stat_acc_V3$latitude[i]
      longitude19 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 30 && !missing_values[i]) {
    compteur20 = compteur20 +1
    if(compteur20 == 1){
      latitude20 = stat_acc_V3$latitude[i]
      longitude20 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 31 && !missing_values[i]) {
    compteur21 = compteur21 +1
    if(compteur21 == 1){
      latitude21 = stat_acc_V3$latitude[i]
      longitude21 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 32 && !missing_values[i]) {
    compteur22 = compteur22 +1
    if(compteur22 == 1){
      latitude22 = stat_acc_V3$latitude[i]
      longitude22 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 33 && !missing_values[i]) {
    compteur23 = compteur23 +1
    if(compteur23 == 1){
      latitude23 = stat_acc_V3$latitude[i]
      longitude23 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 34 && !missing_values[i]) {
    compteur24 = compteur24 +1
    if(compteur24 == 1){
      latitude24 = stat_acc_V3$latitude[i]
      longitude24 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 35 && !missing_values[i]) {
    compteur25 = compteur25 +1
    if(compteur25 == 1){
      latitude25 = stat_acc_V3$latitude[i]
      longitude25 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 36 && !missing_values[i]) {
    compteur26 = compteur26 +1
    if(compteur26 == 1){
      latitude26 = stat_acc_V3$latitude[i]
      longitude26 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 37 && !missing_values[i]) {
    compteur27 = compteur27 +1
    if(compteur27 == 1){
      latitude27 = stat_acc_V3$latitude[i]
      longitude27 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 38 && !missing_values[i]) {
    compteur28 = compteur28 +1
    if(compteur28 == 1){
      latitude28 = stat_acc_V3$latitude[i]
      longitude28 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 39 && !missing_values[i]) {
    compteur29 = compteur29 +1
    if(compteur29 == 1){
      latitude29 = stat_acc_V3$latitude[i]
      longitude29 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 40 && !missing_values[i]) {
    compteur30 = compteur30 +1
    if(compteur30 == 1){
      latitude30 = stat_acc_V3$latitude[i]
      longitude30 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 41 && !missing_values[i]) {
    compteur31 = compteur31 +1
    if(compteur31 == 1){
      latitude31 = stat_acc_V3$latitude[i]
      longitude31 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 42 && !missing_values[i]) {
    compteur32 = compteur32 +1
    if(compteur32 == 1){
      latitude32 = stat_acc_V3$latitude[i]
      longitude32 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 43 && !missing_values[i]) {
    compteur33 = compteur33 +1
    if(compteur33 == 1){
      latitude33 = stat_acc_V3$latitude[i]
      longitude33 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 44 && !missing_values[i]) {
    compteur34 = compteur34 +1
    if(compteur34 == 1){
      latitude34 = stat_acc_V3$latitude[i]
      longitude34 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 45 && !missing_values[i]) {
    compteur35 = compteur35 +1
    if(compteur35 == 1){
      latitude35 = stat_acc_V3$latitude[i]
      longitude35 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 46 && !missing_values[i]) {
    compteur36 = compteur36 +1
    if(compteur36 == 1){
      latitude36 = stat_acc_V3$latitude[i]
      longitude36 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 47 && !missing_values[i]) {
    compteur37 = compteur37 +1
    if(compteur37 == 1){
      latitude37 = stat_acc_V3$latitude[i]
      longitude37 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 48 && !missing_values[i]) {
    compteur38 = compteur38 +1
    if(compteur38 == 1){
      latitude38 = stat_acc_V3$latitude[i]
      longitude38 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 49 && !missing_values[i]) {
    compteur39 = compteur39 +1
    if(compteur39 == 1){
      latitude39 = stat_acc_V3$latitude[i]
      longitude39 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 50 && !missing_values[i]) {
    compteur40 = compteur40 +1
    if(compteur40 == 1){
      latitude40 = stat_acc_V3$latitude[i]
      longitude40 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 51 && !missing_values[i]) {
    compteur41 = compteur41 +1
    if(compteur41 == 1){
      latitude41 = stat_acc_V3$latitude[i]
      longitude41 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 52 && !missing_values[i]) {
    compteur42 = compteur42 +1
    if(compteur42 == 1){
      latitude42 = stat_acc_V3$latitude[i]
      longitude42 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 53 && !missing_values[i]) {
    compteur43 = compteur43 +1
    if(compteur43 == 1){
      latitude43 = stat_acc_V3$latitude[i]
      longitude43 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 54 && !missing_values[i]) {
    compteur44 = compteur44 +1
    if(compteur44 == 1){
      latitude44 = stat_acc_V3$latitude[i]
      longitude44 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 55 && !missing_values[i]) {
    compteur45 = compteur45 +1
    if(compteur45 == 1){
      latitude45 = stat_acc_V3$latitude[i]
      longitude45 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 56 && !missing_values[i]) {
    compteur46 = compteur46 +1
    if(compteur46 == 1){
      latitude46 = stat_acc_V3$latitude[i]
      longitude46 = stat_acc_V3$longitude[i]
    }
  }
  
  if (stat_acc_V3$num_depart[i] == 57 && !missing_values[i]) {
    compteur47 = compteur47 +1
    if(compteur47 == 1){
      latitude47 = stat_acc_V3$latitude[i]
      longitude47 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 58 && !missing_values[i]) {
    compteur48 = compteur48 +1
    if(compteur48 == 1){
      latitude48 = stat_acc_V3$latitude[i]
      longitude48 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 59 && !missing_values[i]) {
    compteur49 = compteur49 +1
    if(compteur49 == 1){
      latitude49 = stat_acc_V3$latitude[i]
      longitude49 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 60 && !missing_values[i]) {
    compteur50 = compteur50 +1
    if(compteur50 == 1){
      latitude50 = stat_acc_V3$latitude[i]
      longitude50 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 61 && !missing_values[i]) {
    compteur51 = compteur51 +1
    if(compteur51 == 1){
      latitude51 = stat_acc_V3$latitude[i]
      longitude51 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 62 && !missing_values[i]) {
    compteur52 = compteur52 +1
    if(compteur52 == 1){
      latitude52 = stat_acc_V3$latitude[i]
      longitude52 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 63 && !missing_values[i]) {
    compteur53 = compteur53 +1
    if(compteur53 == 1){
      latitude53 = stat_acc_V3$latitude[i]
      longitude53 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 64 && !missing_values[i]) {
    compteur54 = compteur54 +1
    if(compteur54 == 1){
      latitude54 = stat_acc_V3$latitude[i]
      longitude54 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 65 && !missing_values[i]) {
    compteur55 = compteur55 +1
    if(compteur55 == 1){
      latitude55 = stat_acc_V3$latitude[i]
      longitude55 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 66 && !missing_values[i]) {
    compteur56 = compteur56 +1
    if(compteur56 == 1){
      latitude56 = stat_acc_V3$latitude[i]
      longitude56 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 67 && !missing_values[i]) {
    compteur57 = compteur57 +1
    if(compteur57 == 1){
      latitude57 = stat_acc_V3$latitude[i]
      longitude57 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 68 && !missing_values[i]) {
    compteur58 = compteur58 +1
    if(compteur58 == 1){
      latitude58 = stat_acc_V3$latitude[i]
      longitude58 = stat_acc_V3$longitude[i]
    }
  }
  
  if (stat_acc_V3$num_depart[i] == 69 && !missing_values[i]) {
    compteur59 = compteur59 +1
    if(compteur59 == 1){
      latitude59 = stat_acc_V3$latitude[i]
      longitude59 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 70 && !missing_values[i]) {
    compteur60 = compteur60 +1
    if(compteur60 == 1){
      latitude60 = stat_acc_V3$latitude[i]
      longitude60 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 71 && !missing_values[i]) {
    compteur61 = compteur61 +1
    if(compteur61 == 1){
      latitude61 = stat_acc_V3$latitude[i]
      longitude61 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 72 && !missing_values[i]) {
    compteur62 = compteur62 +1
    if(compteur62 == 1){
      latitude62 = stat_acc_V3$latitude[i]
      longitude62 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 73 && !missing_values[i]) {
    compteur63 = compteur63 +1
    if(compteur63 == 1){
      latitude63 = stat_acc_V3$latitude[i]
      longitude63 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 74 && !missing_values[i]) {
    compteur64 = compteur64 +1
    if(compteur64 == 1){
      latitude64 = stat_acc_V3$latitude[i]
      longitude64 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 75 && !missing_values[i]) {
    compteur65 = compteur65 +1
    if(compteur65 == 1){
      latitude65 = stat_acc_V3$latitude[i]
      longitude65 = stat_acc_V3$longitude[i]
    }
  }
  
  if (stat_acc_V3$num_depart[i] == 76 && !missing_values[i]) {
    compteur66 = compteur66 +1
    if(compteur66 == 1){
      latitude66 = stat_acc_V3$latitude[i]
      longitude66 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 77 && !missing_values[i]) {
    compteur67 = compteur67 +1
    if(compteur67 == 1){
      latitude67 = stat_acc_V3$latitude[i]
      longitude67 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 78 && !missing_values[i]) {
    compteur68 = compteur68 +1
    if(compteur68 == 1){
      latitude68 = stat_acc_V3$latitude[i]
      longitude68 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 79 && !missing_values[i]) {
    compteur69 = compteur69 +1
    if(compteur69 == 1){
      latitude69 = stat_acc_V3$latitude[i]
      longitude69 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 80 && !missing_values[i]) {
    compteur70 = compteur70 +1
    if(compteur70 == 1){
      latitude70 = stat_acc_V3$latitude[i]
      longitude70 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 81 && !missing_values[i]) {
    compteur71 = compteur71 +1
    if(compteur71 == 1){
      latitude71 = stat_acc_V3$latitude[i]
      longitude71 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 82 && !missing_values[i]) {
    compteur72 = compteur72 +1
    if(compteur72 == 1){
      latitude72 = stat_acc_V3$latitude[i]
      longitude72 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 83 && !missing_values[i]) {
    compteur73 = compteur73 +1
    if(compteur73 == 1){
      latitude73 = stat_acc_V3$latitude[i]
      longitude73 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 84 && !missing_values[i]) {
    compteur74 = compteur74 +1
    if(compteur74 == 1){
      latitude74 = stat_acc_V3$latitude[i]
      longitude74 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 85 && !missing_values[i]) {
    compteur75 = compteur75 +1
    if(compteur75 == 1){
      latitude75 = stat_acc_V3$latitude[i]
      longitude75 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 86 && !missing_values[i]) {
    compteur76 = compteur76 +1
    if(compteur76 == 1){
      latitude76 = stat_acc_V3$latitude[i]
      longitude76 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 87 && !missing_values[i]) {
    compteur77 = compteur77 +1
    if(compteur77 == 1){
      latitude77 = stat_acc_V3$latitude[i]
      longitude77 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 88 && !missing_values[i]) {
    compteur78 = compteur78 +1
    if(compteur78 == 1){
      latitude78 = stat_acc_V3$latitude[i]
      longitude78 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 89 && !missing_values[i]) {
    compteur79 = compteur79 +1
    if(compteur79 == 1){
      latitude79 = stat_acc_V3$latitude[i]
      longitude79 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 90 && !missing_values[i]) {
    compteur80 = compteur80 +1
    if(compteur80 == 1){
      latitude80 = stat_acc_V3$latitude[i]
      longitude80 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 91 && !missing_values[i]) {
    compteur81 = compteur81 +1
    if(compteur81 == 1){
      latitude81 = stat_acc_V3$latitude[i]
      longitude81 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 95 && !missing_values[i]) {
    compteur85 = compteur85 +1
    if(compteur85 == 1){
      latitude85 = stat_acc_V3$latitude[i]
      longitude85 = stat_acc_V3$longitude[i]
    }
  }

  if (stat_acc_V3$num_depart[i] == 97 && !missing_values[i]) {
    compteur87 = compteur87 +1
    if(compteur87 == 1){
      latitude87 = stat_acc_V3$latitude[i]
      longitude87 = stat_acc_V3$longitude[i]
    }
  }
}


# Création de la carte

m2 <- leaflet() %>%
  addTiles() %>% 
  
  addCircleMarkers(lng = longitude88, lat = latitude88 ,
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
                   radius = compteur15/1000, popup = "25")%>%
  
  addCircleMarkers(lng = longitude16, lat = latitude16 ,
                   radius = compteur16/1000, popup = "26")%>%
  
  addCircleMarkers(lng = longitude17, lat = latitude17 ,
                   radius = compteur17/1000, popup = "27")%>%
  
  addCircleMarkers(lng = longitude18, lat = latitude18 ,
                   radius = compteur18/1000, popup = "28")%>%
  
  addCircleMarkers(lng = longitude19, lat = latitude19 ,
                   radius = compteur19/1000, popup = "29")%>%
  
  addCircleMarkers(lng = longitude20, lat = latitude20 ,
                   radius = compteur20/1000, popup = "30")%>%
  
  addCircleMarkers(lng = longitude21, lat = latitude21 ,
                   radius = compteur21/1000, popup = "31")%>%
  
  addCircleMarkers(lng = longitude22, lat = latitude22 ,
                   radius = compteur22/1000, popup = "32")%>%
  
  addCircleMarkers(lng = longitude23, lat = latitude23 ,
                   radius = compteur23/1000, popup = "33")%>%
  
  addCircleMarkers(lng = longitude24, lat = latitude24 ,
                   radius = compteur24/1000, popup = "34")%>%
  
  addCircleMarkers(lng = longitude25, lat = latitude25 ,
                   radius = compteur25/1000, popup = "35")%>%
  
  addCircleMarkers(lng = longitude26, lat = latitude26 ,
                   radius = compteur26/1000, popup = "36")%>%
  
  addCircleMarkers(lng = longitude27, lat = latitude27 ,
                   radius = compteur27/1000, popup = "37")%>%
  
  addCircleMarkers(lng = longitude28, lat = latitude28 ,
                   radius = compteur28/1000, popup = "38")%>%
  
  addCircleMarkers(lng = longitude29, lat = latitude29 ,
                   radius = compteur29/1000, popup = "39")%>%
  
  addCircleMarkers(lng = longitude30, lat = latitude30 ,
                   radius = compteur30/1000, popup = "40")%>%
  
  addCircleMarkers(lng = longitude31, lat = latitude31 ,
                   radius = compteur31/1000, popup = "41")%>%
  
  addCircleMarkers(lng = longitude32, lat = latitude32 ,
                   radius = compteur32/1000, popup = "42")%>%
  
  addCircleMarkers(lng = longitude33, lat = latitude33 ,
                   radius = compteur33/1000, popup = "43")%>%
  
  addCircleMarkers(lng = longitude34, lat = latitude34 ,
                   radius = compteur34/1000, popup = "44")%>%
  
  addCircleMarkers(lng = longitude35, lat = latitude35 ,
                   radius = compteur35/1000, popup = "45")%>%
  
  addCircleMarkers(lng = longitude36, lat = latitude36 ,
                   radius = compteur36/1000, popup = "46")%>%
  
  addCircleMarkers(lng = longitude37, lat = latitude37 ,
                   radius = compteur37/1000, popup = "47")%>%
  
  addCircleMarkers(lng = longitude38, lat = latitude38 ,
                   radius = compteur38/1000, popup = "48")%>%
  
  addCircleMarkers(lng = longitude39, lat = latitude39 ,
                   radius = compteur39/1000, popup = "49")%>%
  
  addCircleMarkers(lng = longitude40, lat = latitude40 ,
                   radius = compteur40/1000, popup = "50")%>%
  
  addCircleMarkers(lng = longitude41, lat = latitude41 ,
                   radius = compteur41/1000, popup = "51")%>%
  
  addCircleMarkers(lng = longitude42, lat = latitude42 ,
                   radius = compteur42/1000, popup = "52")%>%
  
  addCircleMarkers(lng = longitude43, lat = latitude43 ,
                   radius = compteur43/1000, popup = "53")%>%
  
  addCircleMarkers(lng = longitude44, lat = latitude44 ,
                   radius = compteur44/1000, popup = "54")%>%
  
  addCircleMarkers(lng = longitude45, lat = latitude45 ,
                   radius = compteur45/1000, popup = "55")%>%
  
  addCircleMarkers(lng = longitude46, lat = latitude46 ,
                   radius = compteur46/1000, popup = "56")%>%
  
  addCircleMarkers(lng = longitude47, lat = latitude47 ,
                   radius = compteur47/1000, popup = "57")%>%
  
  addCircleMarkers(lng = longitude48, lat = latitude48 ,
                   radius = compteur48/1000, popup = "58")%>%
  
  addCircleMarkers(lng = longitude49, lat = latitude49 ,
                   radius = compteur49/1000, popup = "59")%>%
  
  addCircleMarkers(lng = longitude50, lat = latitude50 ,
                   radius = compteur50/1000, popup = "60")%>%
  
  addCircleMarkers(lng = longitude51, lat = latitude51 ,
                   radius = compteur51/1000, popup = "61")%>%
  
  addCircleMarkers(lng = longitude52, lat = latitude52 ,
                   radius = compteur52/1000, popup = "62")%>%
  
  addCircleMarkers(lng = longitude53, lat = latitude53 ,
                   radius = compteur53/1000, popup = "63")%>%
  
  addCircleMarkers(lng = longitude54, lat = latitude54 ,
                   radius = compteur54/1000, popup = "64")%>%
  
  addCircleMarkers(lng = longitude55, lat = latitude55 ,
                   radius = compteur55/1000, popup = "65")%>%
  
  addCircleMarkers(lng = longitude56, lat = latitude56 ,
                   radius = compteur56/1000, popup = "66")%>%
  
  addCircleMarkers(lng = longitude57, lat = latitude57 ,
                   radius = compteur57/1000, popup = "67")%>%
  
  addCircleMarkers(lng = longitude58, lat = latitude58 ,
                   radius = compteur58/1000, popup = "68")%>%
  
  addCircleMarkers(lng = longitude59, lat = latitude59 ,
                   radius = compteur59/1000, popup = "69")%>%
  
  addCircleMarkers(lng = longitude60, lat = latitude60 ,
                   radius = compteur60/1000, popup = "70")%>%
  
  addCircleMarkers(lng = longitude61, lat = latitude61 ,
                   radius = compteur61/1000, popup = "71")%>%
  
  addCircleMarkers(lng = longitude62, lat = latitude62 ,
                   radius = compteur62/1000, popup = "72")%>%
  
  addCircleMarkers(lng = longitude63, lat = latitude63 ,
                   radius = compteur63/1000, popup = "73")%>%
  
  addCircleMarkers(lng = longitude64, lat = latitude64 ,
                   radius = compteur64/1000, popup = "74")%>%
  
  addCircleMarkers(lng = longitude65, lat = latitude65 ,
                   radius = compteur65/1000, popup = "75")%>%
  
  addCircleMarkers(lng = longitude66, lat = latitude66 ,
                   radius = compteur66/1000, popup = "76")%>%
  
  addCircleMarkers(lng = longitude67, lat = latitude67 ,
                   radius = compteur67/1000, popup = "77")%>%
  
  addCircleMarkers(lng = longitude68, lat = latitude68 ,
                   radius = compteur68/1000, popup = "78")%>%
  
  addCircleMarkers(lng = longitude69, lat = latitude69 ,
                   radius = compteur69/1000, popup = "79")%>%
  
  addCircleMarkers(lng = longitude70, lat = latitude70 ,
                   radius = compteur70/1000, popup = "80")%>%
  
  addCircleMarkers(lng = longitude71, lat = latitude71 ,
                   radius = compteur71/1000, popup = "81")%>%
  
  addCircleMarkers(lng = longitude72, lat = latitude72 ,
                   radius = compteur72/1000, popup = "82")%>%
  
  addCircleMarkers(lng = longitude73, lat = latitude73 ,
                   radius = compteur73/1000, popup = "83")%>%
  
  addCircleMarkers(lng = longitude74, lat = latitude74 ,
                   radius = compteur74/1000, popup = "84")%>%
  
  addCircleMarkers(lng = longitude75, lat = latitude75 ,
                   radius = compteur75/1000, popup = "85")%>%
  
  addCircleMarkers(lng = longitude76, lat = latitude76 ,
                   radius = compteur76/1000, popup = "86")%>%
  
  addCircleMarkers(lng = longitude77, lat = latitude77 ,
                   radius = compteur77/1000, popup = "87")%>%
  
  addCircleMarkers(lng = longitude78, lat = latitude78 ,
                   radius = compteur78/1000, popup = "88")%>%
  
  addCircleMarkers(lng = longitude79, lat = latitude79 ,
                   radius = compteur79/1000, popup = "89")%>%
  
  addCircleMarkers(lng = longitude80, lat = latitude80 ,
                   radius = compteur80/1000, popup = "90")%>%
  
  addCircleMarkers(lng = longitude81, lat = latitude81 ,
                   radius = compteur81/1000, popup = "91")%>%
  
  addCircleMarkers(lng = longitude85, lat = latitude85 ,
                   radius = compteur85/1000, popup = "95")%>%
  
  addCircleMarkers(lng = longitude87, lat = latitude87 ,
                   radius = compteur87/1000, popup = "97")




m2   #affichage de la carte 


### carte des régions que pour les accidents grave ###

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

#Rajout d'une condition dans le if pour ne pas prendre en compte les personnes indemnes 

missing_values <- is.na(stat_acc_V3$latitude) | is.na(stat_acc_V3$longitude)
for (i in 1:70052) {
  if (stat_acc_V3$région[i] == "Normandie" && !missing_values[i] && stat_acc_V3$descr_grav[i]>1) {
    compteur <- compteur + 1
    latitude <- latitude + stat_acc_V3$latitude[i]
    longitude <- longitude + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Pays de la Loire"&& !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur1 <- compteur1 + 1
    latitude1 <- latitude1 + stat_acc_V3$latitude[i]
    longitude1 <- longitude1 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Bretagne"&& !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur2 <- compteur2 + 1
    latitude2 <- latitude2 + stat_acc_V3$latitude[i]
    longitude2 <- longitude2 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Centre Val de Loire"&& !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur3 <- compteur3 + 1
    latitude3 <- latitude3 + stat_acc_V3$latitude[i]
    longitude3 <- longitude3 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Hauts-de-France"&& !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur4 <- compteur4 + 1
    latitude4 <- latitude4 + stat_acc_V3$latitude[i]
    longitude4 <- longitude4 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Grandd Est"&& !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur5 <- compteur5 + 1
    latitude5 <- latitude5 + stat_acc_V3$latitude[i]
    longitude5 <- longitude5 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Ile-de-France"&& !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur6 <- compteur6 + 1
    latitude6 <- latitude6 + stat_acc_V3$latitude[i]
    longitude6 <- longitude6 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Outre-Mer"&& !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur7 <- compteur7 + 1
    latitude7 <- latitude7 + stat_acc_V3$latitude[i]
    longitude7 <- longitude7 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Bourgogne-Franche-Comté"&& !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur8 <- compteur8 + 1
    latitude8 <- latitude8 + stat_acc_V3$latitude[i]
    longitude8 <- longitude8 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Auvergne-Rhône-Alpes"&& !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur9 <- compteur9 + 1
    latitude9 <- latitude9 + stat_acc_V3$latitude[i]
    longitude9 <- longitude9 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Nouvelle-Aquitaine"&& !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur11 <- compteur11 + 1
    latitude11 <- latitude11 + stat_acc_V3$latitude[i]
    longitude11 <- longitude11 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Occitanie"&& !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur12 <- compteur12 + 1
    latitude12 <- latitude12 + stat_acc_V3$latitude[i]
    longitude12 <- longitude12 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Provence-Alpes Côte d'Azur"&& !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur13 <- compteur13 + 1
    latitude13 <- latitude13 + stat_acc_V3$latitude[i]
    longitude13 <- longitude13 + stat_acc_V3$longitude[i]
  }
  if (stat_acc_V3$région[i] == "Corse"&& !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur14 = compteur14 + 1
    latitude14 =latitude14 + stat_acc_V3$latitude[i]
    longitude14 = longitude14 + stat_acc_V3$longitude[i]
  }
}

# Création de la carte

m3 <- leaflet() %>%
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


m3 #affichage de la carte











### carte des départements ue pour les accidents grave ###

#Initialisation des variables
for (i in 1:88) {
  assign(paste0("compteur", i), 0)
  assign(paste0("latitude", i), 0)
  assign(paste0("longitude", i), 0)
}
compteur2B=0

# Boucle pour calculer les coordonnées moyennes

missing_values <- is.na(stat_acc_V3$latitude) | is.na(stat_acc_V3$longitude)
for (i in 1:70052) {
  if (stat_acc_V3$num_depart[i] == 10 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur88 = compteur88 + 1
    if(compteur88 ==1){
      latitude88 = stat_acc_V3$latitude[i]
      longitude88 = stat_acc_V3$longitude[i]
    }
  }
  
  if (stat_acc_V3$num_depart[i] == 11 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur1 = compteur1 +1
    if(compteur1 == 1){
      latitude1 = stat_acc_V3$latitude[i]
      longitude1 = stat_acc_V3$longitude[i]
    }
  }
  
  if (stat_acc_V3$num_depart[i] == 12 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur2 = compteur2 +1
    if(compteur2 == 1){
      latitude2 = stat_acc_V3$latitude[i]
      longitude2 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 13 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur3 = compteur3 +1
    if(compteur3 == 1){
      latitude3 = stat_acc_V3$latitude[i]
      longitude3 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 14 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur4 = compteur4 +1
    if(compteur4 == 1){
      latitude4 = stat_acc_V3$latitude[i]
      longitude4 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 15 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur5 = compteur5 +1
    if(compteur5 == 1){
      latitude5 = stat_acc_V3$latitude[i]
      longitude5 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 16 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur6 = compteur6 +1
    if(compteur6 == 1){
      latitude6 = stat_acc_V3$latitude[i]
      longitude6 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 17 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur7 = compteur7 +1
    if(compteur7 == 1){
      latitude7 = stat_acc_V3$latitude[i]
      longitude7 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 18 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur8 = compteur8 +1
    if(compteur8 == 1){
      latitude8 = stat_acc_V3$latitude[i]
      longitude8 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 19 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur9 = compteur9 +1
    if(compteur9 == 1){
      latitude9 = stat_acc_V3$latitude[i]
      longitude9 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == "2A" && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur10 = compteur10 +1
    if(compteur10 == 1){
      latitude10 = stat_acc_V3$latitude[i]
      longitude10 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == "2B" && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur2B = compteur2B +1
    if(compteur2B == 1){
      latitude2B = stat_acc_V3$latitude[i]
      longitude2B = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 21 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur11 = compteur11 +1
    if(compteur11 == 1){
      latitude11 = stat_acc_V3$latitude[i]
      longitude11 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 22 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur12 = compteur12 +1
    if(compteur12 == 1){
      latitude12 = stat_acc_V3$latitude[i]
      longitude12 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 23 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur13 = compteur13 +1
    if(compteur13 == 1){
      latitude13 = stat_acc_V3$latitude[i]
      longitude13 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 24 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur14 = compteur14 +1
    if(compteur14 == 1){
      latitude14 = stat_acc_V3$latitude[i]
      longitude14 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 25 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur15 = compteur15 +1
    if(compteur15 == 1){
      latitude15 = stat_acc_V3$latitude[i]
      longitude15 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 26 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur16 = compteur16 +1
    if(compteur16 == 1){
      latitude16 = stat_acc_V3$latitude[i]
      longitude16 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 27 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur17 = compteur17 +1
    if(compteur17 == 1){
      latitude17 = stat_acc_V3$latitude[i]
      longitude17 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 28 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur18 = compteur18 +1
    if(compteur18 == 1){
      latitude18 = stat_acc_V3$latitude[i]
      longitude18 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 29 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur19 = compteur19 +1
    if(compteur19 == 1){
      latitude19 = stat_acc_V3$latitude[i]
      longitude19 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 30 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur20 = compteur20 +1
    if(compteur20 == 1){
      latitude20 = stat_acc_V3$latitude[i]
      longitude20 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 31 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur21 = compteur21 +1
    if(compteur21 == 1){
      latitude21 = stat_acc_V3$latitude[i]
      longitude21 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 32 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur22 = compteur22 +1
    if(compteur22 == 1){
      latitude22 = stat_acc_V3$latitude[i]
      longitude22 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 33 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur23 = compteur23 +1
    if(compteur23 == 1){
      latitude23 = stat_acc_V3$latitude[i]
      longitude23 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 34 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur24 = compteur24 +1
    if(compteur24 == 1){
      latitude24 = stat_acc_V3$latitude[i]
      longitude24 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 35 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur25 = compteur25 +1
    if(compteur25 == 1){
      latitude25 = stat_acc_V3$latitude[i]
      longitude25 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 36 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur26 = compteur26 +1
    if(compteur26 == 1){
      latitude26 = stat_acc_V3$latitude[i]
      longitude26 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 37 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur27 = compteur27 +1
    if(compteur27 == 1){
      latitude27 = stat_acc_V3$latitude[i]
      longitude27 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 38 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur28 = compteur28 +1
    if(compteur28 == 1){
      latitude28 = stat_acc_V3$latitude[i]
      longitude28 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 39 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur29 = compteur29 +1
    if(compteur29 == 1){
      latitude29 = stat_acc_V3$latitude[i]
      longitude29 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 40 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur30 = compteur30 +1
    if(compteur30 == 1){
      latitude30 = stat_acc_V3$latitude[i]
      longitude30 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 41 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur31 = compteur31 +1
    if(compteur31 == 1){
      latitude31 = stat_acc_V3$latitude[i]
      longitude31 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 42 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur32 = compteur32 +1
    if(compteur32 == 1){
      latitude32 = stat_acc_V3$latitude[i]
      longitude32 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 43 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur33 = compteur33 +1
    if(compteur33 == 1){
      latitude33 = stat_acc_V3$latitude[i]
      longitude33 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 44 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur34 = compteur34 +1
    if(compteur34 == 1){
      latitude34 = stat_acc_V3$latitude[i]
      longitude34 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 45 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur35 = compteur35 +1
    if(compteur35 == 1){
      latitude35 = stat_acc_V3$latitude[i]
      longitude35 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 46 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur36 = compteur36 +1
    if(compteur36 == 1){
      latitude36 = stat_acc_V3$latitude[i]
      longitude36 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 47 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur37 = compteur37 +1
    if(compteur37 == 1){
      latitude37 = stat_acc_V3$latitude[i]
      longitude37 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 48 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur38 = compteur38 +1
    if(compteur38 == 1){
      latitude38 = stat_acc_V3$latitude[i]
      longitude38 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 49 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur39 = compteur39 +1
    if(compteur39 == 1){
      latitude39 = stat_acc_V3$latitude[i]
      longitude39 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 50 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur40 = compteur40 +1
    if(compteur40 == 1){
      latitude40 = stat_acc_V3$latitude[i]
      longitude40 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 51 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur41 = compteur41 +1
    if(compteur41 == 1){
      latitude41 = stat_acc_V3$latitude[i]
      longitude41 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 52 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur42 = compteur42 +1
    if(compteur42 == 1){
      latitude42 = stat_acc_V3$latitude[i]
      longitude42 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 53 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur43 = compteur43 +1
    if(compteur43 == 1){
      latitude43 = stat_acc_V3$latitude[i]
      longitude43 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 54 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur44 = compteur44 +1
    if(compteur44 == 1){
      latitude44 = stat_acc_V3$latitude[i]
      longitude44 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 55 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur45 = compteur45 +1
    if(compteur45 == 1){
      latitude45 = stat_acc_V3$latitude[i]
      longitude45 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 56 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur46 = compteur46 +1
    if(compteur46 == 1){
      latitude46 = stat_acc_V3$latitude[i]
      longitude46 = stat_acc_V3$longitude[i]
    }
  }
  
  if (stat_acc_V3$num_depart[i] == 57 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur47 = compteur47 +1
    if(compteur47 == 1){
      latitude47 = stat_acc_V3$latitude[i]
      longitude47 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 58 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur48 = compteur48 +1
    if(compteur48 == 1){
      latitude48 = stat_acc_V3$latitude[i]
      longitude48 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 59 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur49 = compteur49 +1
    if(compteur49 == 1){
      latitude49 = stat_acc_V3$latitude[i]
      longitude49 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 60 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur50 = compteur50 +1
    if(compteur50 == 1){
      latitude50 = stat_acc_V3$latitude[i]
      longitude50 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 61 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur51 = compteur51 +1
    if(compteur51 == 1){
      latitude51 = stat_acc_V3$latitude[i]
      longitude51 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 62 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur52 = compteur52 +1
    if(compteur52 == 1){
      latitude52 = stat_acc_V3$latitude[i]
      longitude52 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 63 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur53 = compteur53 +1
    if(compteur53 == 1){
      latitude53 = stat_acc_V3$latitude[i]
      longitude53 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 64 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur54 = compteur54 +1
    if(compteur54 == 1){
      latitude54 = stat_acc_V3$latitude[i]
      longitude54 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 65 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur55 = compteur55 +1
    if(compteur55 == 1){
      latitude55 = stat_acc_V3$latitude[i]
      longitude55 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 66 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur56 = compteur56 +1
    if(compteur56 == 1){
      latitude56 = stat_acc_V3$latitude[i]
      longitude56 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 67 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur57 = compteur57 +1
    if(compteur57 == 1){
      latitude57 = stat_acc_V3$latitude[i]
      longitude57 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 68 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur58 = compteur58 +1
    if(compteur58 == 1){
      latitude58 = stat_acc_V3$latitude[i]
      longitude58 = stat_acc_V3$longitude[i]
    }
  }
  
  if (stat_acc_V3$num_depart[i] == 69 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur59 = compteur59 +1
    if(compteur59 == 1){
      latitude59 = stat_acc_V3$latitude[i]
      longitude59 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 70 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur60 = compteur60 +1
    if(compteur60 == 1){
      latitude60 = stat_acc_V3$latitude[i]
      longitude60 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 71 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur61 = compteur61 +1
    if(compteur61 == 1){
      latitude61 = stat_acc_V3$latitude[i]
      longitude61 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 72 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur62 = compteur62 +1
    if(compteur62 == 1){
      latitude62 = stat_acc_V3$latitude[i]
      longitude62 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 73 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur63 = compteur63 +1
    if(compteur63 == 1){
      latitude63 = stat_acc_V3$latitude[i]
      longitude63 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 74 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur64 = compteur64 +1
    if(compteur64 == 1){
      latitude64 = stat_acc_V3$latitude[i]
      longitude64 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 75 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur65 = compteur65 +1
    if(compteur65 == 1){
      latitude65 = stat_acc_V3$latitude[i]
      longitude65 = stat_acc_V3$longitude[i]
    }
  }
  
  if (stat_acc_V3$num_depart[i] == 76 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur66 = compteur66 +1
    if(compteur66 == 1){
      latitude66 = stat_acc_V3$latitude[i]
      longitude66 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 77 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur67 = compteur67 +1
    if(compteur67 == 1){
      latitude67 = stat_acc_V3$latitude[i]
      longitude67 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 78 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur68 = compteur68 +1
    if(compteur68 == 1){
      latitude68 = stat_acc_V3$latitude[i]
      longitude68 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 79 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur69 = compteur69 +1
    if(compteur69 == 1){
      latitude69 = stat_acc_V3$latitude[i]
      longitude69 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 80 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur70 = compteur70 +1
    if(compteur70 == 1){
      latitude70 = stat_acc_V3$latitude[i]
      longitude70 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 81 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur71 = compteur71 +1
    if(compteur71 == 1){
      latitude71 = stat_acc_V3$latitude[i]
      longitude71 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 82 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur72 = compteur72 +1
    if(compteur72 == 1){
      latitude72 = stat_acc_V3$latitude[i]
      longitude72 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 83 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur73 = compteur73 +1
    if(compteur73 == 1){
      latitude73 = stat_acc_V3$latitude[i]
      longitude73 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 84 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur74 = compteur74 +1
    if(compteur74 == 1){
      latitude74 = stat_acc_V3$latitude[i]
      longitude74 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 85 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur75 = compteur75 +1
    if(compteur75 == 1){
      latitude75 = stat_acc_V3$latitude[i]
      longitude75 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 86 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur76 = compteur76 +1
    if(compteur76 == 1){
      latitude76 = stat_acc_V3$latitude[i]
      longitude76 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 87 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur77 = compteur77 +1
    if(compteur77 == 1){
      latitude77 = stat_acc_V3$latitude[i]
      longitude77 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 88 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur78 = compteur78 +1
    if(compteur78 == 1){
      latitude78 = stat_acc_V3$latitude[i]
      longitude78 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 89 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur79 = compteur79 +1
    if(compteur79 == 1){
      latitude79 = stat_acc_V3$latitude[i]
      longitude79 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 91 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur81 = compteur81 +1
    if(compteur81 == 1){
      latitude81 = stat_acc_V3$latitude[i]
      longitude81 = stat_acc_V3$longitude[i]
    }
  }
  if (stat_acc_V3$num_depart[i] == 95 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur85 = compteur85 +1
    if(compteur85 == 1){
      latitude85 = stat_acc_V3$latitude[i]
      longitude85 = stat_acc_V3$longitude[i]
    }
  }
  
  if (stat_acc_V3$num_depart[i] == 97 && !missing_values[i]&& stat_acc_V3$descr_grav[i]>1) {
    compteur87 = compteur87 +1
    if(compteur87 == 1){
      latitude87 = stat_acc_V3$latitude[i]
      longitude87 = stat_acc_V3$longitude[i]
    }
  }
}


# Création de la carte

m4 <- leaflet() %>%
  addTiles() %>% 
  
  addCircleMarkers(lng = longitude88, lat = latitude88 ,
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
                   radius = compteur15/1000, popup = "25")%>%
  
  addCircleMarkers(lng = longitude16, lat = latitude16 ,
                   radius = compteur16/1000, popup = "26")%>%
  
  addCircleMarkers(lng = longitude17, lat = latitude17 ,
                   radius = compteur17/1000, popup = "27")%>%
  
  addCircleMarkers(lng = longitude18, lat = latitude18 ,
                   radius = compteur18/1000, popup = "28")%>%
  
  addCircleMarkers(lng = longitude19, lat = latitude19 ,
                   radius = compteur19/1000, popup = "29")%>%
  
  addCircleMarkers(lng = longitude20, lat = latitude20 ,
                   radius = compteur20/1000, popup = "30")%>%
  
  addCircleMarkers(lng = longitude21, lat = latitude21 ,
                   radius = compteur21/1000, popup = "31")%>%
  
  addCircleMarkers(lng = longitude22, lat = latitude22 ,
                   radius = compteur22/1000, popup = "32")%>%
  
  addCircleMarkers(lng = longitude23, lat = latitude23 ,
                   radius = compteur23/1000, popup = "33")%>%
  
  addCircleMarkers(lng = longitude24, lat = latitude24 ,
                   radius = compteur24/1000, popup = "34")%>%
  
  addCircleMarkers(lng = longitude25, lat = latitude25 ,
                   radius = compteur25/1000, popup = "35")%>%
  
  addCircleMarkers(lng = longitude26, lat = latitude26 ,
                   radius = compteur26/1000, popup = "36")%>%
  
  addCircleMarkers(lng = longitude27, lat = latitude27 ,
                   radius = compteur27/1000, popup = "37")%>%
  
  addCircleMarkers(lng = longitude28, lat = latitude28 ,
                   radius = compteur28/1000, popup = "38")%>%
  
  addCircleMarkers(lng = longitude29, lat = latitude29 ,
                   radius = compteur29/1000, popup = "39")%>%
  
  addCircleMarkers(lng = longitude30, lat = latitude30 ,
                   radius = compteur30/1000, popup = "40")%>%
  
  addCircleMarkers(lng = longitude31, lat = latitude31 ,
                   radius = compteur31/1000, popup = "41")%>%
  
  addCircleMarkers(lng = longitude32, lat = latitude32 ,
                   radius = compteur32/1000, popup = "42")%>%
  
  addCircleMarkers(lng = longitude33, lat = latitude33 ,
                   radius = compteur33/1000, popup = "43")%>%
  
  addCircleMarkers(lng = longitude34, lat = latitude34 ,
                   radius = compteur34/1000, popup = "44")%>%
  
  addCircleMarkers(lng = longitude35, lat = latitude35 ,
                   radius = compteur35/1000, popup = "45")%>%
  
  addCircleMarkers(lng = longitude36, lat = latitude36 ,
                   radius = compteur36/1000, popup = "46")%>%
  
  addCircleMarkers(lng = longitude37, lat = latitude37 ,
                   radius = compteur37/1000, popup = "47")%>%
  
  addCircleMarkers(lng = longitude38, lat = latitude38 ,
                   radius = compteur38/1000, popup = "48")%>%
  
  addCircleMarkers(lng = longitude39, lat = latitude39 ,
                   radius = compteur39/1000, popup = "49")%>%
  
  addCircleMarkers(lng = longitude40, lat = latitude40 ,
                   radius = compteur40/1000, popup = "50")%>%
  
  addCircleMarkers(lng = longitude41, lat = latitude41 ,
                   radius = compteur41/1000, popup = "51")%>%
  
  addCircleMarkers(lng = longitude42, lat = latitude42 ,
                   radius = compteur42/1000, popup = "52")%>%
  
  addCircleMarkers(lng = longitude43, lat = latitude43 ,
                   radius = compteur43/1000, popup = "53")%>%
  
  addCircleMarkers(lng = longitude44, lat = latitude44 ,
                   radius = compteur44/1000, popup = "54")%>%
  
  addCircleMarkers(lng = longitude45, lat = latitude45 ,
                   radius = compteur45/1000, popup = "55")%>%
  
  addCircleMarkers(lng = longitude46, lat = latitude46 ,
                   radius = compteur46/1000, popup = "56")%>%
  
  addCircleMarkers(lng = longitude47, lat = latitude47 ,
                   radius = compteur47/1000, popup = "57")%>%
  
  addCircleMarkers(lng = longitude48, lat = latitude48 ,
                   radius = compteur48/1000, popup = "58")%>%
  
  addCircleMarkers(lng = longitude49, lat = latitude49 ,
                   radius = compteur49/1000, popup = "59")%>%
  
  addCircleMarkers(lng = longitude50, lat = latitude50 ,
                   radius = compteur50/1000, popup = "60")%>%
  
  addCircleMarkers(lng = longitude51, lat = latitude51 ,
                   radius = compteur51/1000, popup = "61")%>%
  
  addCircleMarkers(lng = longitude52, lat = latitude52 ,
                   radius = compteur52/1000, popup = "62")%>%
  
  addCircleMarkers(lng = longitude53, lat = latitude53 ,
                   radius = compteur53/1000, popup = "63")%>%
  
  addCircleMarkers(lng = longitude54, lat = latitude54 ,
                   radius = compteur54/1000, popup = "64")%>%
  
  addCircleMarkers(lng = longitude55, lat = latitude55 ,
                   radius = compteur55/1000, popup = "65")%>%
  
  addCircleMarkers(lng = longitude56, lat = latitude56 ,
                   radius = compteur56/1000, popup = "66")%>%
  
  addCircleMarkers(lng = longitude57, lat = latitude57 ,
                   radius = compteur57/1000, popup = "67")%>%
  
  addCircleMarkers(lng = longitude58, lat = latitude58 ,
                   radius = compteur58/1000, popup = "68")%>%
  
  addCircleMarkers(lng = longitude59, lat = latitude59 ,
                   radius = compteur59/1000, popup = "69")%>%
  
  addCircleMarkers(lng = longitude60, lat = latitude60 ,
                   radius = compteur60/1000, popup = "70")%>%
  
  addCircleMarkers(lng = longitude61, lat = latitude61 ,
                   radius = compteur61/1000, popup = "71")%>%
  
  addCircleMarkers(lng = longitude62, lat = latitude62 ,
                   radius = compteur62/1000, popup = "72")%>%
  
  addCircleMarkers(lng = longitude63, lat = latitude63 ,
                   radius = compteur63/1000, popup = "73")%>%
  
  addCircleMarkers(lng = longitude64, lat = latitude64 ,
                   radius = compteur64/1000, popup = "74")%>%
  
  addCircleMarkers(lng = longitude65, lat = latitude65 ,
                   radius = compteur65/1000, popup = "75")%>%
  
  addCircleMarkers(lng = longitude66, lat = latitude66 ,
                   radius = compteur66/1000, popup = "76")%>%
  
  addCircleMarkers(lng = longitude67, lat = latitude67 ,
                   radius = compteur67/1000, popup = "77")%>%
  
  addCircleMarkers(lng = longitude68, lat = latitude68 ,
                   radius = compteur68/1000, popup = "78")%>%
  
  addCircleMarkers(lng = longitude69, lat = latitude69 ,
                   radius = compteur69/1000, popup = "79")%>%
  
  addCircleMarkers(lng = longitude70, lat = latitude70 ,
                   radius = compteur70/1000, popup = "80")%>%
  
  addCircleMarkers(lng = longitude71, lat = latitude71 ,
                   radius = compteur71/1000, popup = "81")%>%
  
  addCircleMarkers(lng = longitude72, lat = latitude72 ,
                   radius = compteur72/1000, popup = "82")%>%
  
  addCircleMarkers(lng = longitude73, lat = latitude73 ,
                   radius = compteur73/1000, popup = "83")%>%
  
  addCircleMarkers(lng = longitude74, lat = latitude74 ,
                   radius = compteur74/1000, popup = "84")%>%
  
  addCircleMarkers(lng = longitude75, lat = latitude75 ,
                   radius = compteur75/1000, popup = "85")%>%
  
  addCircleMarkers(lng = longitude76, lat = latitude76 ,
                   radius = compteur76/1000, popup = "86")%>%
  
  addCircleMarkers(lng = longitude77, lat = latitude77 ,
                   radius = compteur77/1000, popup = "87")%>%
  
  addCircleMarkers(lng = longitude78, lat = latitude78 ,
                   radius = compteur78/1000, popup = "88")%>%
  
  addCircleMarkers(lng = longitude79, lat = latitude79 ,
                   radius = compteur79/1000, popup = "89")%>%
  
  addCircleMarkers(lng = longitude81, lat = latitude81 ,
                   radius = compteur81/1000, popup = "91")%>%
  
  addCircleMarkers(lng = longitude85, lat = latitude85 ,
                   radius = compteur85/1000, popup = "95")%>%
  
  addCircleMarkers(lng = longitude87, lat = latitude87 ,
                   radius = compteur87/1000, popup = "97")




m4  #affichage de la carte 


#P et M
###           Analyse de donnée           ###

#Etude des relations entre variables qualitatives 

table<-table(data$région,data$descr_athmo)
print(table)
mosaicplot(table, main = "Région par condition athmosphérique", las=2)
chisq.test(table)

table<-table(data$descr_lum,data$description_intersection)
print(table)
mosaicplot(table, main = "Condition lumière par type d'intersection")
chisq.test(table)

table<-table(data$descr_cat_veh,data$descr_grav)
print(table)
mosaicplot(table, main = "Gravité par catégorie de véhicule")
chisq.test(table)

table<-table(data$descr_dispo_secu,data$descr_grav)
print(table)
mosaicplot(table, main = "Gravité par catégorie de dispositif de sécurité")
chisq.test(table)

table<-table(data$descr_type_col,data$descr_grav)
print(table)
mosaicplot(table, main = "Gravité par type de collison")
chisq.test(table)

table<-table(data$description_intersection,data$descr_grav)
print(table)
mosaicplot(table, main = "Gravité par type d'intersection")
chisq.test(table)

table<-table(data$descr_athmo,data$descr_grav)
print(table)
mosaicplot(table, main = "Gravité par condition athmosphérique")
chisq.test(table)




#Calculer les régréssions linéaires 

reg_semaine=lm(as.numeric(unlist(accidents_par_semaine[2]))~as.numeric(unlist(accidents_par_semaine[1])))
plot(as.numeric(unlist(accidents_par_semaine[1])), as.numeric(unlist(accidents_par_semaine[2])))
abline(reg_semaine)
print(reg_semaine)

reg_mois=lm(as.numeric(unlist(accidents_par_mois[2]))~as.numeric(unlist(accidents_par_mois[1])))
plot(as.numeric(unlist(accidents_par_mois[1])), as.numeric(unlist(accidents_par_mois[2])))
abline(reg_mois)
print(reg_mois)

