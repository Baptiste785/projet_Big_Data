#On vient disposer nos données dans data
data = stat_acc_V3


###                        #Préparation des données#                        ###      

#à faire à la fin pour ne pas créer de problème 
# Supprimer les lignes contenant des valeurs manquantes
data1<- na.omit(data)
data=data1

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


#Changement de latitude et longitude pour les arrondissement de Marseille/Paris/Lyon car il y a des incohérences

for (i in 1:73643){
  #Lyon
  if(data$ville[i]=="LYON 01"){
    data$latitude[i] <- 45.77039337158203;
    data$longitude[i] <- 4.826514720916748;
  }
  if(data$ville[i]=="LYON 02"){
    data$latitude[i] <- 45.751625061035156;
    data$longitude[i] <- 4.828275680541992;
  }
  if(data$ville[i]=="LYON 03"){
    data$latitude[i] <- 45.7518424987793;
    data$longitude[i] <- 4.876522064208984;
  }
  if(data$ville[i]=="LYON 04"){
    data$latitude[i] <- 45.77958679199219;
    data$longitude[i] <- 4.825376987457275;
  }
  if(data$ville[i]=="LYON 05"){
    data$latitude[i] <- 45.75631332397461;
    data$longitude[i] <- 4.803836345672607;
  }
  if(data$ville[i]=="LYON 06"){
    data$latitude[i] <- 45.773250579833984;
    data$longitude[i] <- 4.85155725479126;
  }
  if(data$ville[i]=="LYON 07"){
    data$latitude[i] <- 45.73286056518555;
    data$longitude[i] <- 4.839175224304199;
  }
  if(data$ville[i]=="LYON 08"){
    data$latitude[i] <- 45.741737365722656;
    data$longitude[i] <- 4.874378204345703;
  }
  if(data$ville[i]=="LYON 09"){
    data$latitude[i] <- 45.775726318359375;
    data$longitude[i] <- 4.801545143127441;
  }
  
  #Marseille
  if(data$ville[i]=="MARSEILLE 01"){
    data$latitude[i] <- 43.298004150390625;
    data$longitude[i] <- 5.381344795227051;
  }
  if(data$ville[i]=="MARSEILLE 02"){
    data$latitude[i] <- 43.32648849487305;
    data$longitude[i] <- 5.357941627502441;
  }
  if(data$ville[i]=="MARSEILLE 03"){
    data$latitude[i] <- 43.309749603271484;
    data$longitude[i] <- 5.379581928253174;
  }
  if(data$ville[i]=="MARSEILLE 04"){
    data$latitude[i] <- 43.306800842285156;
    data$longitude[i] <- 5.399689674377441;
  }
  if(data$ville[i]=="MARSEILLE 05"){
    data$latitude[i] <- 43.292266845703125;
    data$longitude[i] <- 5.395776271820068;
  }
  if(data$ville[i]=="MARSEILLE 06"){
    data$latitude[i] <- 43.28824234008789;
    data$longitude[i] <- 5.380294322967529;
  }
  if(data$ville[i]=="MARSEILLE 07"){
    data$latitude[i] <- 43.28508377075195;
    data$longitude[i] <- 5.360000133514404;
  }
  if(data$ville[i]=="MARSEILLE 08"){
    data$latitude[i] <- 43.26696014404297;
    data$longitude[i] <- 5.389336585998535;
  }
  if(data$ville[i]=="MARSEILLE 09"){
    data$latitude[i] <- 43.26696014404297;
    data$longitude[i] <- 5.389336585998535;
  }
  if(data$ville[i]=="MARSEILLE 10"){
    data$latitude[i] <- 43.26696014404297;
    data$longitude[i] <- 5.389336585998535;
  }
  if(data$ville[i]=="MARSEILLE 11"){
    data$latitude[i] <- 43.26696014404297;
    data$longitude[i] <- 5.389336585998535;
  }
  if(data$ville[i]=="MARSEILLE 12"){
    data$latitude[i] <- 43.26696014404297;
    data$longitude[i] <- 5.389336585998535;
  }
  if(data$ville[i]=="MARSEILLE 13"){
    data$latitude[i] <- 43.26696014404297;
    data$longitude[i] <- 5.389336585998535;
  }
  if(data$ville[i]=="MARSEILLE 14"){
    data$latitude[i] <- 43.26696014404297;
    data$longitude[i] <- 5.389336585998535;
  }
  if(data$ville[i]=="MARSEILLE 15"){
    data$latitude[i] <- 43.26696014404297;
    data$longitude[i] <- 5.389336585998535;
  }
  if(data$ville[i]=="MARSEILLE 16"){
    data$latitude[i] <- 43.26696014404297;
    data$longitude[i] <- 5.389336585998535;
  }
  #Paris
  if(data$ville[i]=="Paris 01"){
    data$latitude[i] <- 48.8588897;
    data$longitude[i] <- 2.320041;
  }
  if(data$ville[i]=="Paris 02"){
    data$latitude[i] <- 48.8588897;
    data$longitude[i] <- 2.320041;
  }
  if(data$ville[i]=="Paris 03"){
    data$latitude[i] <- 48.8588897;
    data$longitude[i] <- 2.320041;
  }
  if(data$ville[i]=="Paris 04"){
    data$latitude[i] <- 48.8588897;
    data$longitude[i] <- 2.320041;
  }
  if(data$ville[i]=="Paris 05"){
    data$latitude[i] <- 48.8588897;
    data$longitude[i] <- 2.320041;
  }
  if(data$ville[i]=="Paris 06"){
    data$latitude[i] <- 48.8588897;
    data$longitude[i] <- 2.320041;
  }
  if(data$ville[i]=="Paris 07"){
    data$latitude[i] <- 48.8588897;
    data$longitude[i] <- 2.320041;
  }
  if(data$ville[i]=="Paris 08"){
    data$latitude[i] <- 48.8588897;
    data$longitude[i] <- 2.320041;
  }
  if(data$ville[i]=="Paris 09"){
    data$latitude[i] <- 48.8588897;
    data$longitude[i] <- 2.320041;
  }
  if(data$ville[i]=="Paris 10"){
    data$latitude[i] <- 48.8588897;
    data$longitude[i] <- 2.320041;
  }
  if(data$ville[i]=="Paris 11"){
    data$latitude[i] <- 48.8588897;
    data$longitude[i] <- 2.320041;
  }
  if(data$ville[i]=="Paris 12"){
    data$latitude[i] <- 48.8588897;
    data$longitude[i] <- 2.320041;
  }
  if(data$ville[i]=="Paris 13"){
    data$latitude[i] <- 48.8588897;
    data$longitude[i] <- 2.320041;
  }
  if(data$ville[i]=="Paris 14"){
    data$latitude[i] <- 48.8588897;
    data$longitude[i] <- 2.320041;
  }
  if(data$ville[i]=="Paris 15"){
    data$latitude[i] <- 48.8588897;
    data$longitude[i] <- 2.320041;
  }
  if(data$ville[i]=="Paris 16"){
    data$latitude[i] <- 48.8588897;
    data$longitude[i] <- 2.320041;
  }
  if(data$ville[i]=="Paris 17"){
    data$latitude[i] <- 48.8588897;
    data$longitude[i] <- 2.320041;
  }
  if(data$ville[i]=="Paris 18"){
    data$latitude[i] <- 48.8588897;
    data$longitude[i] <- 2.320041;
  }
  if(data$ville[i]=="Paris 19"){
    data$latitude[i] <- 48.8588897;
    data$longitude[i] <- 2.320041;
  }
  if(data$ville[i]=="Paris 20"){
    data$latitude[i] <- 48.8588897;
    data$longitude[i] <- 2.320041;
  }
}


# Mise au format numérique des codes INSEE
data$id_code_insee<-as.numeric(data$id_code_insee)

# Mise au format numérique des latitudes
data$latitude<-as.numeric(data$latitude)

#Correction du format de la date
for (i in 1:73643){
  var=data$date[i]
  data$date[i] <- as.Date(var);
}

# mise au format numérique des longitudes
data$longitude<-as.numeric(data$longitude)

# mise au format numérique des id_usa
data$id_usa[i]<-as.numeric(data$id_usa[i])

# mise au format numérique des ages
data$age<-as.numeric(data$age)

# mise au format numérique des places
data$place<-as.numeric(data$place)







###                   Construire des séries chronologiques                    ###


# Agréger les données par jour
accidents_par_jour <- aggregate(id_usa ~ data$date, data, FUN = length)

# Agréger les données par mois
accidents_par_mois <- aggregate(id_usa ~ format(date, "%Y-%m"), data, FUN = length)

# Agréger les données par semaine
accidents_par_semaine <- aggregate(id_usa ~ format(date, "%U"), data, FUN = length)

# Agréger les données par année
accidents_par_an <- aggregate(id_usa ~ format(date, "%Y"), data, FUN = length)







###                   Catégorisation par région                   ###

#Permet de créer une nouvelle colonne pour regrouper les départements
for(i in 1:length(data)){
  code_insee<- substr(data$id_code_insee[i], start = 1, stop = 2)
  data$num_depart[i]<-code_insee
}

#Permet de regrouper les départements par région
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
  if (var==08 || var==51 || var==10 || var==52 || var==55 || var==57 || var==54 || var==88 || var==68 || var==67 ){
    data$région[i]<-"Grandd Est";}
  if (var==89 || var==58 || var==21 || var==71 || var==39|| var==70 || var==25 ){
    data$région[i]<-"Bourgogne-Franche-Comté";}
  if (var==03 || var==63 || var==15 || var==43 || var==42|| var==69 || var==07|| var==01|| var==38|| var==26|| var==73|| var==74 ){
    data$région[i]<-"Auvergne-Rhône-Alpes";}
  if (var==79 || var==86 || var==17 || var==16 || var==87|| var==23 || var==19|| var==24|| var==47|| var==33|| var==40|| var==64 ){
    data$région[i]<-"Nouvelle-Aquitaine";}
  if (var==32 || var==65 || var==31 || var==09 || var==82|| var==46 || var==81|| var==11|| var==66|| var==34|| var==30|| var==48|| var==12 ){
    data$région[i]<-"Occitanie";}
  if (var==13 || var==81 || var==83 || var==04 || var==05|| var==06 ){
    data$région[i]<-"Provence-Alpes Côte d'Azur";}
  if (var=="2A" ||var=="2B"){
    data$région[i]<-"Corse";}
}

#ici je viens mettre cette ligne pour permettre la modification du tableau dans rstudio
stat_acc_V3=data


###                   Visualisation                   ###

#Nombre d’accidents en fonction des conditions atmosphériques
x = data$descr_athmo
barplot(table(x), xlab= "condition atmosphérique", ylab="Nombre d'accident", col="green", main = "Nombre d’accidents en fonction des conditions atmosphériques" )


#Nombre d’accidents en fonction de la description de la surface
y = data$descr_etat_surf
barplot(table(y), xlab= " description de la surface", ylab="Nombre d'accident", col="blue", main = "Nombre d’accidents en fonction de la description de la surface" )


#Nombre d’accidents selon la gravité
z = data$descr_grav
barplot(table(z), xlab= " description de la gravité", ylab="Nombre d'accident", col="red", main = "Nombre d’accidents selon la gravité" )


