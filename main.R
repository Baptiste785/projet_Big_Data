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
data$descr_grav

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
  
}


##changement de donnée pour l'agglomération ou non 
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
}


