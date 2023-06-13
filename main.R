data = stat_acc_V3


##changement de donnée pour les catégories de véhicule
data$descr_cat_veh<-factor(data$descr_cat_veh)
data$descr_cat_veh<-as.numeric(data$descr_cat_veh)


##changement de donnée pour l'agglomération ou non
data$descr_agglo<-factor(data$descr_agglo)
data$descr_agglo<-as.numeric(data$descr_agglo)


##changemenent de donnée athmoshéprique
data$descr_athmo<-factor(data$descr_athmo)
data$descr_athmo<-as.numeric(data$descr_athmo)


##changement de donnée pour lumière
data$descr_lum<-factor(data$descr_lum)
data$descr_lum<-as.numeric(data$descr_lum)


##changement de donnée pour létat de la surface
data$descr_etat_surf<-factor(data$descr_etat_surf)
data$descr_etat_surf<-as.numeric(data$descr_etat_surf)


##changement de donnée pour intersection
data$description_intersection<-factor(data$description_intersection)
data$description_intersection<-as.numeric(data$description_intersection)


##changement de donnée pour le dispositif de sécurité
data$descr_dispo_secu<-factor(data$descr_dispo_secu)
data$descr_dispo_secu<-as.numeric(data$descr_dispo_secu)


#changement de donnée pour la descritpion de la personne après l'accident
data$descr_grav<-factor(data$descr_grav)
data$descr_grav<-as.numeric(data$descr_grav)


##changement de donnée pour le motif du trajet
data$descr_motif_traj<-factor(data$descr_motif_traj)
data$descr_motif_traj<-as.numeric(data$descr_motif_traj)


##changement de donnée pour le type de collision
data$descr_type_col<-factor(data$descr_type_col)
data$descr_type_col<-as.numeric(data$descr_type_col)


#Changement de latitude et longitude pour les arrondissement de Marseille et Paris 

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

# Supprimer les lignes contenant des valeurs manquantes
data_sans_nan <- na.omit(data)


# Supprimer les lignes contenant des valeurs NULL
data_sans_nan_et_null <- data_sans_nan[complete.cases(data_sans_nan), ]

#ici je viens mettre cette ligne pour permettre la modification du tableau dans rstudio 
stat_acc_V3=data 


