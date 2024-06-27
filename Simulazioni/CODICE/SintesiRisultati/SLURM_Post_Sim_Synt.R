##################################################################################
#########   ELABORAZIONI POST SIMULAZIONI    #####################################
##################################################################################

# Il file riassume i risultati delle simulazioni fatte su un singolo dataset.

# Per utilizzare il codice:
#   Collocarsi nella stessa directory in cui sono stati salvati gli output della simulazione (sim.out)
#   Creare la directory in cui salvare le sintesi 

#Lo script utilizza i risultati di "SLURM_Simulations.R".
#Il numero di simulazioni condotte va specificato.
sim_number <- 100
#sim_number=50

#Directory con gli esiti della simulazione. Allo stesso livello delle cartelle "models" e "results".
setwd("D:/shared_directory_VM/simulazioniLight/dataset1_midCorr")

#Dove mettere gli output finali:
path <- "synthesis/"



####### TUTTI I QUANTILI ##

##########   ELABORAZIONE OUTPUT RIDGE    ################
quantile.error.ridge <- function(models.attr, results) {
  
  res.for.plot <- as.data.frame(t(results))
  res.for.plot$dt <- as.numeric(gsub("dt: ","",rownames(res.for.plot)))
  rownames(res.for.plot) <- NULL
  
  ## Il dataframe su cui facciamo le modifiche
  res.scale.for.plot <- res.for.plot
  
  ## Dividiamo gli MSE assoluti per renderli relativi.
  for (i in 1:nrow(models.attr)) {
    res.scale.for.plot[,i] <- res.scale.for.plot[,i]/models.attr[i,3]
  }
  
  max_dt <- nrow(res.scale.for.plot)
  
  qt <- matrix(NA, nrow = max_dt, ncol=3)
  colnames(qt) <- c("q25","q50","q75")
  rownames(qt) <- 1:max_dt
  
  ## Popoliamo la matrice con i quantili
  for (i in 1:max_dt) {
    qt[i,] <- quantile(as.numeric(res.scale.for.plot[i,-ncol(res.scale.for.plot)]), probs = c(0.25,0.5,0.75))
  }
  
  return(qt)
}

quantile_error_ridge <- list(first_qt =c(), median=c(), third_qt=c())
mean_R2_ridge <- c() #Ottenere gli R2 predittivi in ciascuna simulazione.

sim_eff_number <- 0  ##Numero effettivo di simulazioni fatte (è un controllo)
pb = txtProgressBar(min = 0, max = sim_number, initial = 0, style=3) 

for (i in 1:sim_number) {

  model_file <- paste0("models/models.ridge.",i)
  results_file <- paste0("results/results.ridge.",i)
  
  if(file.exists(model_file) & file.exists(results_file)) {
    sim_eff_number <- sim_eff_number + 1
    load(model_file);load(results_file)
    
    mean_R2_ridge <- rbind(mean_R2_ridge, models.attr.ridge$R2_pred)
    quantile_error <- quantile.error.ridge(models.attr.ridge, results.ridge)
    quantile_error_ridge$first_qt <- rbind(quantile_error_ridge$first_qt,quantile_error[,"q25"])
    quantile_error_ridge$median <- rbind(quantile_error_ridge$median,quantile_error[,"q50"])
    quantile_error_ridge$third_qt <- rbind(quantile_error_ridge$third_qt,quantile_error[,"q75"])
  }
  setTxtProgressBar(pb,i)
}; close(pb)

# Sintetizziamo i quantili degli errori e l'R2 predittivo medio.
mean_R2_ridge <- apply(mean_R2_ridge,1,mean)

cat("\nSimulazioni RIDGE: ",sim_eff_number)


######   ELABORAZIONE OUTPUT RF   #######
quantile.error.rf <- function(models.attr, results) {
  
  res.for.plot <- as.data.frame(t(results))
  res.for.plot$dt <- as.numeric(gsub("dt: ","",rownames(res.for.plot)))
  rownames(res.for.plot) <- NULL
  
  ## Il dataframe su cui facciamo le modifiche
  res.scale.for.plot <- res.for.plot
  
  ## Dividiamo gli MSE assoluti per renderli relativi.
  for (i in 1:nrow(models.attr)) {
    res.scale.for.plot[,i] <- res.scale.for.plot[,i]/models.attr[i,4]
  }
  
  max_dt <- nrow(res.scale.for.plot)
  
  qt <- matrix(NA, nrow = max_dt, ncol=3)
  colnames(qt) <- c("q25","q50","q75")
  rownames(qt) <- 1:max_dt
  
  ## Popoliamo la matrice con i quantili
  for (i in 1:max_dt) {
    qt[i,] <- quantile(as.numeric(res.scale.for.plot[i,-ncol(res.scale.for.plot)]), probs = c(0.25,0.5,0.75))
  }
  
  return(qt)
}

quantile_error_rf <- list(first_qt =c(), median=c(), third_qt=c()) 
mean_R2_rf <- c() #Ottenere gli R2 predittivi in ciascuna simulazione.

sim_eff_number <- 0  ##Numero effettivo di simulazioni fatte (è un controllo)
pb = txtProgressBar(min = 0, max = sim_number, initial = 0, style=3) 

for (i in 1:sim_number) {
  model_file <- paste0("models/models.rf.",i)
  results_file <- paste0("results/results.rf.",i)
  
  if(file.exists(model_file) & file.exists(results_file)) {
    sim_eff_number <- sim_eff_number + 1
    load(model_file);load(results_file)
    
    mean_R2_rf <- rbind(mean_R2_rf, models.attr.rf$R2_pred)
    quantile_error <- quantile.error.rf(models.attr.rf, results.rf)
    quantile_error_rf$first_qt <- rbind(quantile_error_rf$first_qt,quantile_error[,"q25"])
    quantile_error_rf$median <- rbind(quantile_error_rf$median,quantile_error[,"q50"])
    quantile_error_rf$third_qt <- rbind(quantile_error_rf$third_qt,quantile_error[,"q75"])
  }
  setTxtProgressBar(pb,i)
}; close(pb)

# Sintetizziamo i quantili degli errori e l'R2 predittivo medio.
mean_R2_rf <- apply(mean_R2_rf,1,mean)

cat("\nSimulazioni RANDOM FOREST: ",sim_eff_number)



######## ELABORAZIONE OUTPUT GRADIENT BOOSTING ##########
quantile.error.gb <- function(models.attr, results) {
  
  res.for.plot <- as.data.frame(t(results))
  res.for.plot$dt <- as.numeric(gsub("dt: ","",rownames(res.for.plot)))
  rownames(res.for.plot) <- NULL
  
  ## Il dataframe su cui facciamo le modifiche
  res.scale.for.plot <- res.for.plot
  
  ## Dividiamo gli MSE assoluti per renderli relativi.
  for (i in 1:nrow(models.attr)) {
    res.scale.for.plot[,i] <- res.scale.for.plot[,i]/models.attr[i,3]
  }
  
  max_dt <- nrow(res.scale.for.plot)
  
  qt <- matrix(NA, nrow = max_dt, ncol=3)
  colnames(qt) <- c("q25","q50","q75")
  rownames(qt) <- 1:max_dt
  
  ## Popoliamo la matrice con i quantili
  for (i in 1:max_dt) {
    qt[i,] <- quantile(as.numeric(res.scale.for.plot[i,-ncol(res.scale.for.plot)]), probs = c(0.25,0.5,0.75))
  }
  
  return(qt)
}

quantile_error_gb <- list(first_qt =c(), median=c(), third_qt=c())
mean_R2_gb <- c() #Ottenere gli R2 predittivi in ciascuna simulazione.

sim_eff_number <- 0  ##Numero effettivo di simulazioni fatte (è un controllo)
pb = txtProgressBar(min = 0, max = sim_number, initial = 0, style=3) 

for (i in 1:sim_number) {
  model_file <- paste0("models/models.gb.",i)
  results_file <- paste0("results/results.gb.",i)
  
  if(file.exists(model_file) & file.exists(results_file)) {
    sim_eff_number <- sim_eff_number + 1
    load(model_file);load(results_file)
    
    mean_R2_gb <- rbind(mean_R2_gb, models.attr.gb$R2_pred)
    quantile_error <- quantile.error.gb(models.attr.gb, results.gb)
    quantile_error_gb$first_qt <- rbind(quantile_error_gb$first_qt,quantile_error[,"q25"])
    quantile_error_gb$median <- rbind(quantile_error_gb$median,quantile_error[,"q50"])
    quantile_error_gb$third_qt <- rbind(quantile_error_gb$third_qt,quantile_error[,"q75"])
  }
  setTxtProgressBar(pb,i)
}; close(pb)

# Sintetizziamo i quantili degli errori e l'R2 predittivo medio.
mean_R2_gb <- apply(mean_R2_gb,1,mean)

cat("\nSimulazioni GB: ",sim_eff_number)

### SALVATAGGIO RISULTATI
#Ridge
save(mean_R2_ridge, file =paste0(path,"mean_R2_ridge"))
save(quantile_error_ridge, file =paste0(path,"quantile_error_ridge"))

#Random forest
save(mean_R2_rf, file =paste0(path,"mean_R2_rf"))
save(quantile_error_rf, file =paste0(path,"quantile_error_rf"))

#Gradient boosting
save(mean_R2_gb, file =paste0(path,"mean_R2_gb"))
save(quantile_error_gb, file =paste0(path,"quantile_error_gb"))


## ELABORAZIONE OUTPUT RETE NEURALE ##
library(feather)

quantile.error.nn <- function(models.attr, results) {
  
  res.for.plot <- as.data.frame(t(results))
  res.for.plot$dt <- as.numeric(gsub("dt: ","",rownames(res.for.plot)))
  rownames(res.for.plot) <- NULL
  
  ## Il dataframe su cui facciamo le modifiche
  res.scale.for.plot <- res.for.plot
  
  ## Dividiamo gli MSE assoluti per renderli relativi.
  for (i in 1:nrow(models.attr)) {
    res.scale.for.plot[,i] <- res.scale.for.plot[,i]/as.numeric(models.attr[i,3])
  }
  
  max_dt <- nrow(res.scale.for.plot)
  
  qt <- matrix(NA, nrow = max_dt, ncol=3)
  colnames(qt) <- c("q25","q50","q75")
  rownames(qt) <- 1:max_dt
  
  ## Popoliamo la matrice con i quantili
  for (i in 1:max_dt) {
    qt[i,] <- quantile(as.numeric(res.scale.for.plot[i,-ncol(res.scale.for.plot)]), probs = c(0.25,0.5,0.75))
  }
  
  return(qt)
}

quantile_error_nn <- list(first_qt =c(), median=c(), third_qt=c())
mean_R2_nn <- c() #Ottenere gli R2 predittivi in ciascuna simulazione.

sim_eff_number <- 0  ##Numero effettivo di simulazioni fatte (è un controllo)
pb = txtProgressBar(min = 0, max = sim_number, initial = 0, style=3) 

for (i in 1:sim_number) {
  model_file <- paste0("models/models.nn.",i,".feather")
  results_file <- paste0("results/results.nn.",i,".feather")
  
  if(file.exists(model_file) & file.exists(results_file)) {
    sim_eff_number <- sim_eff_number + 1
    
    models.attr.nn <- read_feather(model_file)
    models.attr.nn$`__index_level_0__` <- NULL
    results.nn <- read_feather(results_file)
    results.nn$`__index_level_0__` <- NULL
    
    mean_R2_nn <- rbind(mean_R2_nn, models.attr.nn$R2_pred)
    quantile_error <- quantile.error.nn(models.attr.nn, results.nn)
    quantile_error_nn$first_qt <- rbind(quantile_error_nn$first_qt,quantile_error[,"q25"])
    quantile_error_nn$median <- rbind(quantile_error_nn$median,quantile_error[,"q50"])
    quantile_error_nn$third_qt <- rbind(quantile_error_nn$third_qt,quantile_error[,"q75"])
  }
  setTxtProgressBar(pb,i)
}; close(pb)

# Sintetizziamo i quantili degli errori e l'R2 predittivo medio.
mean_R2_nn <- apply(mean_R2_nn,1,mean)

cat("\nSimulazioni NN: ",sim_eff_number)

save(mean_R2_nn, file =paste0(path,"mean_R2_nn"))
save(quantile_error_nn, file =paste0(path,"quantile_error_nn"))












####### TUTTI I QUANTILI ##
#Questa parte sintetizza solamente i valori di qualità iniziale. Decisamente più veloce,
#se interessa solo questo.
sim_number=100
setwd("D:/shared_directory_VM/simulazioniLight/dataset16_GradualCD")

##########   ELABORAZIONE OUTPUT RIDGE    ################
mean_R2_ridge <- c() #Ottenere gli R2 predittivi in ciascuna simulazione.

sim_eff_number <- 0  ##Numero effettivo di simulazioni fatte (è un controllo)
pb = txtProgressBar(min = 0, max = sim_number, initial = 0, style=3) 

for (i in 1:sim_number) {
  
  model_file <- paste0("models/models.ridge.",i)
  results_file <- paste0("results/results.ridge.",i)
  
  if(file.exists(model_file) & file.exists(results_file)) {
    sim_eff_number <- sim_eff_number + 1
    load(model_file);load(results_file)
    
    mean_R2_ridge <- rbind(mean_R2_ridge, models.attr.ridge$R2)
  
  }
  setTxtProgressBar(pb,i)
}; close(pb)

mean_R2_ridge <- apply(mean_R2_ridge,1,mean)

cat("\nSimulazioni RIDGE: ",sim_eff_number)


######   ELABORAZIONE OUTPUT RF   #######

mean_R2_rf <- c() #Ottenere gli R2 predittivi in ciascuna simulazione.

sim_eff_number <- 0  ##Numero effettivo di simulazioni fatte (è un controllo)
pb = txtProgressBar(min = 0, max = sim_number, initial = 0, style=3) 

for (i in 1:sim_number) {
  model_file <- paste0("models/models.rf.",i)
  results_file <- paste0("results/results.rf.",i)
  
  if(file.exists(model_file) & file.exists(results_file)) {
    sim_eff_number <- sim_eff_number + 1
    load(model_file);load(results_file)
    
    mean_R2_rf <- rbind(mean_R2_rf, models.attr.rf$R2)
  }
  setTxtProgressBar(pb,i)
}; close(pb)

mean_R2_rf <- apply(mean_R2_rf,1,mean)

cat("\nSimulazioni RANDOM FOREST: ",sim_eff_number)



######## ELABORAZIONE OUTPUT GRADIENT BOOSTING ##########
mean_R2_gb <- c() #Ottenere gli R2 predittivi in ciascuna simulazione.

sim_eff_number <- 0  ##Numero effettivo di simulazioni fatte (è un controllo)
pb = txtProgressBar(min = 0, max = sim_number, initial = 0, style=3) 

for (i in 1:sim_number) {
  model_file <- paste0("models/models.gb.",i)
  results_file <- paste0("results/results.gb.",i)
  
  if(file.exists(model_file) & file.exists(results_file)) {
    sim_eff_number <- sim_eff_number + 1
    load(model_file);load(results_file)
    
    mean_R2_gb <- rbind(mean_R2_gb, models.attr.gb$R2_pred)
  }
  setTxtProgressBar(pb,i)
}; close(pb)

mean_R2_gb <- apply(mean_R2_gb,1,mean)

cat("\nSimulazioni GB: ",sim_eff_number)


## rete neurale
library(feather)

mean_R2_nn <- c() #Ottenere gli R2 predittivi in ciascuna simulazione.

sim_eff_number <- 0  ##Numero effettivo di simulazioni fatte (è un controllo)
pb = txtProgressBar(min = 0, max = sim_number, initial = 0, style=3) 

for (i in 1:sim_number) {
  model_file <- paste0("models/models.nn.",i,".feather")
  results_file <- paste0("results/results.nn.",i,".feather")
  
  if(file.exists(model_file) & file.exists(results_file)) {
    sim_eff_number <- sim_eff_number + 1
    
    models.attr.nn <- read_feather(model_file)
    models.attr.nn$`__index_level_0__` <- NULL
    results.nn <- read_feather(results_file)
    results.nn$`__index_level_0__` <- NULL
    
    mean_R2_nn <- rbind(mean_R2_nn, models.attr.nn$R2_pred)
  }
  setTxtProgressBar(pb,i)
}; close(pb)

# Sintetizziamo i quantili degli errori e l'R2 predittivo medio.
mean_R2_nn <- apply(mean_R2_nn,1,mean)


mean(mean_R2_ridge)
mean(mean_R2_rf)
mean(mean_R2_gb)
mean(mean_R2_nn)

