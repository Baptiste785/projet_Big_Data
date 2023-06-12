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
  if(data$descr_grav[i]=="Hors agglomÃ©ration"){
    data$descr_grav[i] <- 0;
  }
  if(data$descr_grav[i]=="En agglomÃ©ration"){
    data$descr_grav[i] <- 1;
  }
}

##changement de donnée pour l'agglomération ou non 

