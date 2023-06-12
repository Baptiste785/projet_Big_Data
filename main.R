data = stat_acc_V3

##changement de donnée pour la descritpion de la personne après l'accident
  for (i in 1:73643){
    if(data$descr_grav[i]=="Indemne"){
      data$descr_grav[i] <- 0;
    }
    if(data$descr_grav[i]=="BlessÃ© hospitalisÃ©"){
      data$descr_grav[i] <- 1;
    }
    if(data$descr_grav[i]=="TuÃ©"){
      data$descr_grav[i] <- 2;
    }
    if(data$descr_grav[i]=="BlessÃ© lÃ©ger"){
      data$descr_grav[i] <- 3;
    }
  }

##changement de donnée pour l'agglomération ou non 
for (i in 1:73643){
  if(data$descr_agglo[i]=="Hors agglomÃ©ration"){
    data$descr_agglo[i] <- 0;
  }
  if(data$descr_agglo[i]=="En agglomÃ©ration"){
    data$descr_agglo[i] <- 1;
  }
}

##changemenent de donnée athmoshéprique 
for (i in 1:73643){
  if(data$descr_athmo[i]=="Vent fort â€“ tempÃªte"){
    data$descr_athmo[i] <- 0;
  }
  if(data$descr_athmo[i]=="Temps couvert"){
    data$descr_athmo[i] <- 1;
  }
  if(data$descr_athmo[i]=="Temps Ã©blouissant"){
    data$descr_athmo[i] <- 2;
  }
  if(data$descr_athmo[i]=="Pluie lÃ©gÃ¨re"){
    data$descr_athmo[i] <- 3;
  }
  if(data$descr_athmo[i]=="Normale"){
    data$descr_athmo[i] <- 4;
  }
  if(data$descr_athmo[i]=="Brouillard â€“ fumÃ©e"){
    data$descr_athmo[i] <- 5;
  }
  
  if(data$descr_athmo[i]=="Autre"){
    data$descr_athmo[i] <- 6;
  }
  if(data$descr_athmo[i]=="Pluie forte"){
    data$descr_athmo[i] <- 7;
  }
  
  if(data$descr_athmo[i]=="Neige â€“ grÃªle"){
    data$descr_athmo[i] <- 8;
  }
  
}


##changement de donnée pour lumière
for (i in 1:73643){
  if(data$descr_lum[i]=="CrÃ©puscule ou aube"){
    data$descr_lum[i] <- 0;
  }
  if(data$descr_lum[i]=="Nuit avec Ã©clairage public allumÃ©"){
    data$descr_lum[i] <- 1;
  }
  if(data$descr_lum[i]=="Nuit sans Ã©clairage public"){
    data$descr_lum[i] <- 2;
  }
  if(data$descr_lum[i]=="Plein jour"){
    data$descr_lum[i] <- 3;
  }
  if(data$descr_lum[i]=="Nuit avec Ã©clairage public non allumÃ©"){
    data$descr_lum[i] <- 4;
  }

}

##changement de donnée pour létat de la surface
for (i in 1:73643){
  if(data$descr_etat_surf[i]=="Autre"){
    data$descr_etat_surf[i] <- 0;
  }
  if(data$descr_etat_surf[i]=="EnneigÃ©e"){
    data$descr_etat_surf[i] <- 1;
  }
  if(data$descr_etat_surf[i]=="Flaques"){
    data$descr_etat_surf[i] <- 2;
  }
  if(data$descr_etat_surf[i]=="InondÃ©e"){
    data$descr_etat_surf[i] <- 3;
  }
  if(data$descr_etat_surf[i]=="MouillÃ©e"){
    data$descr_etat_surf[i] <- 4;
  }
  if(data$descr_etat_surf[i]=="Normale"){
    data$descr_etat_surf[i] <- 5;
  }
  if(data$descr_etat_surf[i]=="VerglacÃ©e"){
    data$descr_etat_surf[i] <- 6;
  }
  if(data$descr_etat_surf[i]=="Corps gras â€“ huile"){
    data$descr_etat_surf[i] <- 7;
  }
  
}

##changement de donnée pour intersection 
for (i in 1:73643){
  if(data$description_intersection[i]=="Autre intersection"){
    data$description_intersection[i] <- 0;
  }
  if(data$description_intersection[i]=="Giratoire"){
    data$description_intersection[i] <- 1;
  }
  if(data$description_intersection[i]=="Hors intersection"){
    data$description_intersection[i] <- 2;
  }
  if(data$description_intersection[i]=="Intersection Ã  plus de 4 branches"){
    data$description_intersection[i] <- 3;
  }
  if(data$description_intersection[i]=="Intersection en T"){
    data$description_intersection[i] <- 4;
  }
  if(data$description_intersection[i]=="Intersection en X"){
    data$description_intersection[i] <- 5;
  }
  if(data$description_intersection[i]=="Intersection en Y"){
    data$description_intersection[i] <- 6;
  }
  if(data$description_intersection[i]=="Passage Ã  niveau"){
    data$description_intersection[i] <- 7;
  }
  if(data$description_intersection[i]=="Place"){
    data$description_intersection[i] <- 8;
  }
}

##changement de donnée pour le dispositif de sécurité 
for (i in 1:73643){
  if(data$descr_dispo_secu[i]=="Autre - Non dÃ©terminable"){
    data$descr_dispo_secu[i] <- 0;
  }
  if(data$descr_dispo_secu[i]=="PrÃ©sence d'un casque - Utilisation non dÃ©terminable"){
    data$descr_dispo_secu[i] <- 1;
  }
  if(data$descr_dispo_secu[i]=="Autre - UtilisÃ©"){
    data$descr_dispo_secu[i] <- 2;
  }
  if(data$descr_dispo_secu[i]=="Autre - Non utilisÃ©"){
    data$descr_dispo_secu[i] <- 3;
  }
  if(data$descr_dispo_secu[i]=="PrÃ©sence d'un casque non utilisÃ©"){
    data$descr_dispo_secu[i] <- 4;
  }
  if(data$descr_dispo_secu[i]=="PrÃ©sence d'une ceinture de sÃ©curitÃ© - Utilisation non dÃ©terminable"){
    data$descr_dispo_secu[i] <- 5;
  }
  if(data$descr_dispo_secu[i]=="Utilisation d'un casque"){
    data$descr_dispo_secu[i] <- 6;
  }
  if(data$descr_dispo_secu[i]=="PrÃ©sence de ceinture de sÃ©curitÃ© non utilisÃ©e"){
    data$descr_dispo_secu[i] <- 7;
  }
  if(data$descr_dispo_secu[i]=="Utilisation d'un Ã©quipement rÃ©flÃ©chissant"){
    data$descr_dispo_secu[i] <- 8;
  }
  if(data$descr_dispo_secu[i]=="Utilisation d'une ceinture de sÃ©curitÃ©"){
    data$descr_dispo_secu[i] <- 9;
  }
  if(data$descr_dispo_secu[i]=="PrÃ©sence dispositif enfant - Utilisation non dÃ©terminable"){
    data$descr_dispo_secu[i] <- 10;
  }
  if(data$descr_dispo_secu[i]=="Utilisation d'un dispositif enfant"){
    data$descr_dispo_secu[i] <- 11;
  }
  if(data$descr_dispo_secu[i]==" PrÃ©sence d'un Ã©quipement rÃ©flÃ©chissant non utilisÃ©"){
    data$descr_dispo_secu[i] <- 11;
  }
 
}

##changement de donnée pour le motif du trajet 
for (i in 1:73643){
  if(data$descr_motif_traj[i]=="Autre"){
    data$descr_motif_traj[i] <- 0;
  }
  if(data$descr_motif_traj[i]=="Courses â€“ achats"){
    data$descr_motif_traj[i] <- 1;
  }
  if(data$descr_motif_traj[i]=="Domicile â€“ travail"){
    data$descr_motif_traj[i] <- 2;
  }
  if(data$descr_motif_traj[i]=="Domicile â€“ Ã©cole"){
    data$descr_motif_traj[i] <- 3;
  }
  if(data$descr_motif_traj[i]=="Promenade â€“ loisirs"){
    data$descr_motif_traj[i] <- 4;
  }
  if(data$descr_motif_traj[i]=="Utilisation professionnelle"){
    data$descr_motif_traj[i] <- 5;
  }
}

##changement de donnée pour le type de collision 
for (i in 1:73643){
  if(data$descr_type_col[i]=="Deux vÃ©hicules - Frontale"){
    data$descr_type_col[i] <- 0;
  }
  if(data$descr_type_col[i]=="Deux vÃ©hicules â€“ Par lâ€™arriÃ¨re"){
    data$descr_type_col[i] <- 1;
  }
  if(data$descr_type_col[i]=="Deux vÃ©hicules â€“ Par le cotÃ©"){
    data$descr_type_col[i] <- 2;
  }
  if(data$descr_type_col[i]=="Sans collision"){
    data$descr_type_col[i] <- 3;
  }
  if(data$descr_type_col[i]=="Trois vÃ©hicules et plus â€“ Collisions multiples"){
    data$descr_type_col[i] <- 4;
  }
  if(data$descr_type_col[i]=="Trois vÃ©hicules et plus â€“ En chaÃ®ne"){
    data$descr_type_col[i] <- 5;
  }
}
