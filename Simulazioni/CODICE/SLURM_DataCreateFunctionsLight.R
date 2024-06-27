###################################
##Dataset leggeri da 10 variabili##
###################################

library(MASS)
library(Matrix)
#library(corrplot)

#setwd("C:/Users/User/Documents/AI AGING/SLURM")
source("SLURM_Base_Functions.R")

#### DA QUI LE SIMULAZIONI FATTE IN TESI ###

get_dataset_1MidCorr <- function(job_index, sim_number) {
  
  set.seed(100)
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)/2.5
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  # Quantità specifiche della simulazione in oggetto.
  set.seed(seeds[job_index])
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  output.lin <- lin.out(input, coef = coeff) + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output.lin,input, daily_obs, n_giorni)
  
  rm(list = c("output.lin","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coeff))
  
}
#GB = 1500 alberi, RF = 120, NN = 1 rep, batch size = 64, patience = 2, [200,200,200]


get_dataset_1MidCorr_4var <- function(job_index, sim_number) {
  
  set.seed(350)
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 4
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)/4.7
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  
  set.seed(seeds[job_index] + 20) 
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  output.lin <- lin.out(input, coef = coeff) + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output.lin,input, daily_obs, n_giorni)
  
  rm(list = c("output.lin","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coeff))
}
#RF 120 alberi, mtry = seq(1,4, by = 1).
#GB 1500 alberi.
#NN reps = 1, batch size = 64, patience = 2, [50,50,50,50], quant = ["V1",...,"V4"]



get_dataset_1indip <- function(job_index, sim_number) {
  
  set.seed(100)
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)/2.5
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=1)$mat
  
  # Quantità specifiche della simulazione in oggetto.
  set.seed(seeds[job_index] + 1) #Cambio i seed rispetto all'altro caso.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  output.lin <- lin.out(input, coef = coeff) + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output.lin,input, daily_obs, n_giorni)
  
  rm(list = c("output.lin","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coeff))
  
}
#ridge = 0.87, rf = 0.81, gb = 0.87.
#GB = 1500 alberi, RF = 120, NN = 1 rep, batch size = 64, patience = 2. [200,200,200]


get_dataset_1HighCorr <- function(job_index, sim_number) {
  
  set.seed(100)
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)/3
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  set.seed(102)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 4, sparsity=0.25)$mat
  
  # Quantità specifiche della simulazione in oggetto.
  set.seed(seeds[job_index] + 2) #Cambio i seed rispetto all'altro caso.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  output.lin <- lin.out(input, coef = coeff) + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output.lin,input, daily_obs, n_giorni)
  
  rm(list = c("output.lin","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coeff))
  
}
#più vicine di prima le prestazioni.
#GB = 1500 alberi, RF=120, NN = 1 rep, batch size = 64, patience = 2.[200,200,200]


#Utilizza ancora la rete neurale con 200 nodi.
get_dataset_1MidCorr5obs <- function(job_index, sim_number) {
  
  set.seed(100)
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 5
  sd_WN <- sqrt(80)/2.5
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  # Quantità specifiche della simulazione in oggetto.
  set.seed(seeds[job_index] + 5) #Cambiamo anche i dataset che vengono generati.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs, mu = mu, Sigma = Sigma))
  output.lin <- lin.out(input, coef = coeff) + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output.lin,input, daily_obs, n_giorni)
  
  rm(list = c("output.lin","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coeff))
}
#Utilizza ancora la rete neurale con 200 nodi.
get_dataset_1MidCorr50obs <- function(job_index, sim_number) {
  
  set.seed(100)
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 50
  sd_WN <- sqrt(80)/2.5
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  # Quantità specifiche della simulazione in oggetto.
  set.seed(seeds[job_index] + 6) #Cambiamo anche i dataset che vengono generati.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs, mu = mu, Sigma = Sigma))
  output.lin <- lin.out(input, coef = coeff) + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output.lin,input, daily_obs, n_giorni)
  
  rm(list = c("output.lin","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coeff))
}
#Utilizza ancora la rete neurale con 200 nodi.


#10 variabili. Effetti cubici.
get_dataset_3 <- function(job_index, sim_number) {
  
  set.seed(300)
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)*1.5
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  # Quantità specifiche della simulazione in oggetto.
  set.seed(seeds[job_index])
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  output.cub <- cub.out(input, coef = coeff[-1]) + noise.WN(n_giorni*daily_obs,sd_WN) + coeff[1]
  data <- create.data(output.cub,input, daily_obs, n_giorni)
  
  rm(list = c("output.cub","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coeff))
}
#Utilizza ancora la rete neurale con 200 nodi, patience = 2, batch size = 64.
#gb = 1500 alberi, rf = 120.
#Ho mandato la rete neurale con patience 2, batch size = 64, [50,50,50] e [200,200,200]


####DATASET 4.3 MANDATO ###

#Effetti di interazione.
get_dataset_4.3 <- function(job_index, sim_number) {
  
  set.seed(409) #Invece che 400.
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)/2
  mu <- rep(0,nvar_input)
  coeff_lin <- runif(nvar_input + 1, min = -5, max = 5)
  n_int = 10 #Numero di interazioni
  coeff_int <- runif(n_int, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  set.seed(seeds[job_index])
  # Quantità specifiche della simulazione in oggetto.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  interaction_out <- interaction.out(input, coeff_lin, n_int, coeff_int, nvar_input, 406) #seed = 1000
  output <- interaction_out$output + noise.WN(n_giorni*daily_obs,sd_WN)
  
  data <- create.data(output,input, daily_obs, n_giorni)
  
  rm(list = c("interaction_out","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff_lin = coeff_lin, coeff_int = coeff_int))
}
# gb utilizza 2000 alberi, rf = 120.
#rete neurale con patience 2, batch size = 64, [200,200,200]


get_dataset_4.3_2obs <- function(job_index, sim_number) {
  
  set.seed(409) #Invece che 400.
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 2
  sd_WN <- sqrt(80)/2
  mu <- rep(0,nvar_input)
  coeff_lin <- runif(nvar_input + 1, min = -5, max = 5)
  n_int = 10 #Numero di interazioni
  coeff_int <- runif(n_int, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  set.seed(seeds[job_index])
  # Quantità specifiche della simulazione in oggetto.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  interaction_out <- interaction.out(input, coeff_lin, n_int, coeff_int, nvar_input, 406) #seed = 1000
  output <- interaction_out$output + noise.WN(n_giorni*daily_obs,sd_WN)
  
  data <- create.data(output,input, daily_obs, n_giorni)
  
  rm(list = c("interaction_out","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff_lin = coeff_lin, coeff_int = coeff_int))
}
#Patience = 4 per la rete neurale.
#gb = 2000 alberi. size nn = [50,50,50].
#rf = 120.


#10 variabili. More Noise.
get_dataset_6_moreNoise <- function(job_index, sim_number) {
  
  set.seed(500)
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)*1.15
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  # Quantità specifiche della simulazione in oggetto.
  set.seed(seeds[job_index])
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  output.lin <- lin.out(input, coef = coeff) + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output.lin,input, daily_obs, n_giorni)
  
  rm(list = c("output.lin","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coeff))
  
}
#Rf = 120, gb = 1500, nn = 64 batch size, [50,50,50], 2 patience.


## PROCESSI STAZIONARI ##
get_dataset_8_AR_moreARmid <- function(job_index, sim_number) {
  
  set.seed(800)
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)/4
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  coefAR = c(0.6,0.3); coefMA = c()
  sd_ARMA <- sqrt(10)*1.8
  
  
  # Quantità specifiche della simulazione in oggetto.
  set.seed(seeds[job_index])
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  noiseARMA <- noise.ARIMA(n = n_giorni, coefAR, coefMA, d=0, sd=sd_ARMA)
  
  #Variabile risposta.
  output.lin <- lin.out(input, coef = coeff) + noise.WN(n_giorni*daily_obs,sd_WN) + 
    rep(noiseARMA, each=daily_obs)
  
  data <- create.data(output.lin,input, daily_obs, n_giorni)
  
  #Aggiunti i primi due ritardi al dataset.
  data.dm <- get.daily.means(data)
  data$outL1 <- NA
  data$outL2 <- NA
  for (i in unique(data$Days)) {
    data$outL1[data$Days == i] <- data.dm$data$output.mean.lag1[data.dm$data$Days == i]
    data$outL2[data$Days == i] <- data.dm$data$output.mean.lag2[data.dm$data$Days == i]
  }
  
  
  rm(list = c("output.lin","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coeff))
  
}
#rf = 120, gb = 3000, nn = 64 batch size, patience = 2, [50,50,50], quant aggiunto ["outL1","outL2"]


#Devo differenziare, in qualche modo, da un processo che catturo bene ed uno che
#catturo male.
get_data_8_ARMA_7var <- function(job_index, sim_number) {
  
  set.seed(800)
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 7
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)/3
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  coefAR = c(0.5,0.3); coefMA = c(0.8,0.5) # coefMA = c(0.8,0.5)
  sd_ARMA <- sqrt(10)
  
  
  # Quantità specifiche della simulazione in oggetto.
  set.seed(seeds[job_index] + 2)
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  noiseARMA <- noise.ARIMA(n = n_giorni, coefAR, coefMA, d=0, sd=sd_ARMA)
  
  #Variabile risposta.
  output.lin <- lin.out(input, coef = coeff) + noise.WN(n_giorni*daily_obs,sd_WN) + 
    rep(noiseARMA, each=daily_obs)
  
  data <- create.data(output.lin,input, daily_obs, n_giorni)
  
  #Aggiunti i primi due ritardi al dataset.
  data.dm <- get.daily.means(data)
  data$outL1 <- NA
  for (i in unique(data$Days)) {
    data$outL1[data$Days == i] <- data.dm$data$output.mean.lag1[data.dm$data$Days == i]
  }
  
  
  rm(list = c("output.lin","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coeff))
  
}
#rf = 120 tree, mtry = seq(2,8, by = 2). gb = 1500
#nn = 64 batch size, patience = 2, [50,50,50]


##SETAR
get_dataset_10_SETAR <- function(job_index, sim_number) {
  
  set.seed(1000)
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 7
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)/5 #/3 ridge = 0.82.
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  sd_SETAR <- sqrt(10)
  r = 1; d=1; phi1=c(0,0.3,0.1); phi2 <- c(0,0.8,0.15)
  par=list(phi1,phi2);orderAR=2
  
  # Quantità specifiche della simulazione in oggetto.
  set.seed(seeds[job_index])
  #Processo SETAR simulato.
  noiseTAR <- simulate.TAR(n_giorni,sd_SETAR,r,d, par, orderAR, 365)
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  
  #Variabile risposta.
  output.lin <- lin.out(input, coef = coeff) + noise.WN(n_giorni*daily_obs,sd_WN) +
    rep(noiseTAR$simTAR, each=daily_obs)
  
  data <- create.data(output.lin,input, daily_obs, n_giorni)
  
  #Aggiunti i primi due ritardi al dataset.
  data.dm <- get.daily.means(data)
  data$outL1 <- NA
  data$outL2 <- NA
  for (i in unique(data$Days)) {
    data$outL1[data$Days == i] <- data.dm$data$output.mean.lag1[data.dm$data$Days == i]
    data$outL2[data$Days == i] <- data.dm$data$output.mean.lag2[data.dm$data$Days == i]
  }
  
  rm(list = c("output.lin","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coeff))
  
}
#RF 120 alberi, mtry = seq(2,8, by = 2),
#GB 1.5k alberi
## NN [50,50,50], batch size = 64, patience = 2.


### ARIMA
get_dataset_11_ARIMA <- function(job_index, sim_number) {
  
  set.seed(1102)
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 7
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)/2.2 #
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  coefAR = c(); coefMA = c() # E' un arima. 
  sd_ARIMA <- sqrt(10)
  
  
  # Quantità specifiche della simulazione in oggetto.
  set.seed(seeds[job_index])
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  noiseARMA <- noise.ARIMA(n = n_giorni, coefAR, coefMA, d=1, sd=sd_ARIMA)
  
  #Variabile risposta.
  output.lin <- lin.out(input, coef = coeff) + noise.WN(n_giorni*daily_obs,sd_WN) + 
    rep(noiseARMA, each=daily_obs)
  
  data <- create.data(output.lin,input, daily_obs, n_giorni)
  
  #Aggiunti i primi due ritardi al dataset.
  data.dm <- get.daily.means(data)
  data$outL1 <- NA
  for (i in unique(data$Days)) {
    data$outL1[data$Days == i] <- data.dm$data$output.mean.lag1[data.dm$data$Days == i]
  }
  
  
  rm(list = c("output.lin","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coeff))
  
}
#RF 120 alberi, mtry = seq(2,8, by = 2), gb 1.5k alberi.
## NN [50,50,50], batch size = 64, patience = 2.








## DATA DRIFT ##

#Relazioni cubiche.
get_dataset_15_cubeDataDrift <- function(job_index, sim_number) {
  
  set.seed(300)
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 10
  n_giorni <- 1825
  #n_giorni <- 7*365
  daily_obs <- 30
  sd_WN <- sqrt(80)*1.5
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)          #Variabili con gradual drift
  mu2 <-rep(1.5,length(unstable_vars))    #Secondo set di medie, per le variabili che evolvono.
  
  #Sudden drift in posizione.
  position = c(1,1825)    #Dal primo giorno all'ultimo. Non può essere cambiato
  prob <- c(0.1,0.8) #Probabilmente era questa.
  
  #Ottenimento dei drift graduali
  meanToUse <- get.processi.Gradual.Drift.Coeff(unstable_vars,n_giorni, mu[unstable_vars], mu2,
                                               position, seed=seeds[job_index] + 16, prob)
  
  # Quantità specifiche della simulazione in oggetto.
  set.seed(seeds[job_index] + 15)
  
  input <- input.STAG(unstable_vars, meanToUse, mu, Sigma, n_giorni, daily_obs)  #Permette di generare da una media che varia.
  output.cub <- cub.out(input, coeff_cub = coeff[-1]) + noise.WN(n_giorni*daily_obs,sd_WN) + coeff[1]
  data <- create.data(output.cub,input, daily_obs, n_giorni)
  
  rm(list = c("output.cub","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coeff, means = meanToUse))
}
# GB 3k alberi, RF mtry <- seq(2,10, by = 2), cores = 4.
# NN [200,200,200]. batch size = 64, patience = 2.
#50 replicazioni.

get_dataset_15_3_intDataDrift <- function(job_index, sim_number) {
  
  set.seed(1530)
  # Quantità uguali in ogni dataset.
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)/2
  mu <- rep(0,nvar_input)
  coeff_lin <- runif(nvar_input + 1, min = -5, max = 5)
  n_int = 10 #Numero di interazioni
  coeff_int <- runif(n_int, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)          #Variabili con gradual drift
  mu2 <- rep(2,length(unstable_vars))    #Secondo set di medie, per le variabili che evolvono.
  
  #Sudden drift in posizione.
  #position = c(1,1825)    #Dal primo giorno all'ultimo. Non può essere cambiato
  position = c(730,1824)
  prob <- c(0.05,0.8)
  
  #Ottenimento dei drift graduali
  #meanToUse <- get.processi.Gradual.Drift.Coeff(unstable_vars,n_giorni, mu[unstable_vars], mu2,
  #                                              position, seed=seeds[job_index] + 17, prob)
  meanToUse <- get.processi.Gradual.Drift.Coeff2(unstable_vars,n_giorni, mu[unstable_vars], mu2,
                                                position, seed=(seeds[job_index] + 17), prob,
                                                init = prob[1], last = prob[2])
  
  # Quantità specifiche della simulazione in oggetto.
  set.seed(seeds[job_index] + 18)
  
  input <- input.STAG(unstable_vars, meanToUse, mu, Sigma, n_giorni, daily_obs)  #Permette di generare da una media che varia.
  input <- as.data.frame(input)
  interaction_out <- interaction.out(input, coeff_lin, n_int, coeff_int, nvar_input, 1531) #seed = 1000
  output <- interaction_out$output + noise.WN(n_giorni*daily_obs,sd_WN)
  
  data <- create.data(output,input, daily_obs, n_giorni)
  
  rm(list = c("interaction_out","input","Sigma"))
  return(list(data = data, seeds = seeds, means = meanToUse))
}
## Rf = 120, mtry <- seq(2,10, by = 2)
## GB 3k alberi.
## NN [200,200,200], batch size = 64, patience = 2.
# 50 replicazioni


#
get_dataset_15_3_intFurtherDataDrift_002 <- function(job_index, sim_number) {
  
  set.seed(1530)
  # Quantità uguali in ogni dataset.
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)/2
  mu <- rep(0,nvar_input)
  coeff_lin <- runif(nvar_input + 1, min = -5, max = 5)
  n_int = 10 #Numero di interazioni
  coeff_int <- runif(n_int, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)          #Variabili con gradual drift
  mu2 <-rep(3,length(unstable_vars))    #Secondo set di medie, per le variabili che evolvono.
  
  #Sudden drift in posizione.
  #position = c(1,1825)    #Dal primo giorno all'ultimo. Non può essere cambiato
  position = c(730,1824)
  prob <- c(0.02,0.8)
  
  #Ottenimento dei drift graduali
  #meanToUse <- get.processi.Gradual.Drift.Coeff(unstable_vars,n_giorni, mu[unstable_vars], mu2,
  #                                              position, seed=seeds[job_index] + 17, prob)
  meanToUse <- get.processi.Gradual.Drift.Coeff2(unstable_vars,n_giorni, mu[unstable_vars], mu2,
                                                 position, seed=(seeds[job_index] + 20), prob,
                                                 init = prob[1], last = prob[2])
  
  # Quantità specifiche della simulazione in oggetto.
  set.seed(seeds[job_index] + 20)
  
  input <- input.STAG(unstable_vars, meanToUse, mu, Sigma, n_giorni, daily_obs)  #Permette di generare da una media che varia.
  input <- as.data.frame(input)
  interaction_out <- interaction.out(input, coeff_lin, n_int, coeff_int, nvar_input, 1531) #seed = 1000
  output <- interaction_out$output + noise.WN(n_giorni*daily_obs,sd_WN)
  
  data <- create.data(output,input, daily_obs, n_giorni)
  
  rm(list = c("interaction_out","input","Sigma"))
  return(list(data = data, seeds = seeds, means = meanToUse))
}
## GB 3k alberi.
## NN [200,200,200], batch size = 64, patience = 2.

get_dataset_15_3_intFurtherDataDrift_025 <- function(job_index, sim_number) {
  
  set.seed(1530)
  # Quantità uguali in ogni dataset.
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)/2
  mu <- rep(0,nvar_input)
  coeff_lin <- runif(nvar_input + 1, min = -5, max = 5)
  n_int = 10 #Numero di interazioni
  coeff_int <- runif(n_int, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)          #Variabili con gradual drift
  mu2 <-rep(3,length(unstable_vars))    #Secondo set di medie, per le variabili che evolvono.
  
  #Sudden drift in posizione.
  #position = c(1,1825)    #Dal primo giorno all'ultimo. Non può essere cambiato
  position = c(730,1824)
  prob <- c(0.25,0.8)
  
  #Ottenimento dei drift graduali
  #meanToUse <- get.processi.Gradual.Drift.Coeff(unstable_vars,n_giorni, mu[unstable_vars], mu2,
  #                                              position, seed=seeds[job_index] + 17, prob)
  meanToUse <- get.processi.Gradual.Drift.Coeff2(unstable_vars,n_giorni, mu[unstable_vars], mu2,
                                                 position, seed=(seeds[job_index] + 19), prob,
                                                 init = prob[1], last = prob[2])
  
  # Quantità specifiche della simulazione in oggetto.
  set.seed(seeds[job_index] + 30)
  
  input <- input.STAG(unstable_vars, meanToUse, mu, Sigma, n_giorni, daily_obs)  #Permette di generare da una media che varia.
  input <- as.data.frame(input)
  interaction_out <- interaction.out(input, coeff_lin, n_int, coeff_int, nvar_input, 1531) #seed = 1000
  output <- interaction_out$output + noise.WN(n_giorni*daily_obs,sd_WN)
  
  data <- create.data(output,input, daily_obs, n_giorni)
  
  rm(list = c("interaction_out","input","Sigma"))
  return(list(data = data, seeds = seeds, means = meanToUse))
}
## GB 3k alberi.
## NN [200,200,200], batch size = 64, patience = 2.

get_dataset_15_3_cubFurtherDataDrift_02 <- function(job_index, sim_number) {
  
  set.seed(1534)
  # Quantità uguali in ogni dataset.
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)*1.5
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)          #Variabili con gradual drift
  mu2 <-rep(3,length(unstable_vars))    #Secondo set di medie, per le variabili che evolvono.
  #mu2 <- rep(0,length(unstable_vars))
  
  position = c(730,1824)
  prob <- c(0.2,0.8)
  
  #Ottenimento dei drift graduali.
  meanToUse <- get.processi.Gradual.Drift.Coeff2(unstable_vars,n_giorni, mu[unstable_vars], mu2,
                                                 position, seed=(seeds[job_index] + 24), prob,
                                                 init = prob[1], last = prob[2])
  
  # Quantità specifiche della simulazione in oggetto.
  set.seed(seeds[job_index] + 25)
  
  input <- input.STAG(unstable_vars, meanToUse, mu, Sigma, n_giorni, daily_obs)  #Permette di generare da una media che varia.
  input <- as.data.frame(input)
  output.cub <- cub.out(input, coeff_cub = coeff[-1]) + 
    noise.WN(n_giorni*daily_obs,sd_WN) + coeff[1]
  data <- create.data(output.cub,input, daily_obs, n_giorni)
  
  
  rm(list = c("output.cub","input","Sigma"))
  return(list(data = data, seeds = seeds, means = meanToUse))
}
#GB 3k alberi, NN [200,200,200] batch_size = 64, patience = 2.


#### CONCEPT DRIFT ####

#Iniziamo dal drift graduale.
#Utilizza gli stessi coefficienti del caso lineare iniziale.
get_dataset_16_GradualCD <- function(job_index, sim_number) {
  
  set.seed(100)
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)/2.5
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)          #Variabili con gradual drift
  coeff2 <- round(runif(length(unstable_vars), min= -5, max = 5),2)      #Secondo set di coefficienti
  
  #Sudden drift in posizione.
  position = c(1,1825)    #Dal primo giorno all'ultimo. Non può essere cambiato
  prob <- c(0.2,1)          #Probabilità iniziale e finale di osservare il secondo regime
  
  #Ottenimento dei drift graduali
  processi <- get.processi.Gradual.Drift.Coeff(unstable_vars,n_giorni, coeff[unstable_vars + 1], coeff2,
                                               position, seed=seeds[job_index], prob)

  
  set.seed(seeds[job_index] + 5) #Per avere dei dati diversi da quelli del primo caso.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  out_variabile <- lin.out.conceptDrift(input, n_giorni, daily_obs, coeff, processi,
                                        unstable_vars)
  
  output <- out_variabile$output + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output,input,daily_obs,n_giorni)
  
  rm(list = c("output","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = out_variabile$coefficienti, processi = processi))
}
#rf = 120, gb = 1500, nn batch size = 64, patience = 2. [50,50,50]

get_dataset_16_GradualCD_2obs <- function(job_index, sim_number) {
  
  set.seed(100)
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 2
  sd_WN <- sqrt(80)/2.5
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)          #Variabili con gradual drift
  coeff2 <- round(runif(length(unstable_vars), min= -5, max = 5),2)      #Secondo set di coefficienti
  
  #Sudden drift in posizione.
  position = c(1,1825)    #Dal primo giorno all'ultimo. Non può essere cambiato
  prob <- c(0.2,1)          #Probabilità iniziale e finale di osservare il secondo regime
  
  #Ottenimento dei drift graduali
  processi <- get.processi.Gradual.Drift.Coeff(unstable_vars,n_giorni, coeff[unstable_vars + 1], coeff2,
                                               position, seed=seeds[job_index], prob)
  
  
  set.seed(seeds[job_index] + 5) 
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  out_variabile <- lin.out.conceptDrift(input, n_giorni, daily_obs, coeff, processi,
                                        unstable_vars)
  
  output <- out_variabile$output + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output,input,daily_obs,n_giorni)
  
  rm(list = c("output","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = out_variabile$coefficienti, processi = processi))
}
#rf = 120, gb = 1500, nn batch size = 64, patience = 2. [50,50,50]


#Uso questo per valutare la variazione di qualità dei modelli.
get_dataset_16_GradualCD_60var_10obs <- function(job_index, sim_number) {
  
  set.seed(102)   #Ruotiamo i parametri, seed = 100 di base.
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 60
  n_giorni <- 1825
  daily_obs <- 10
  sd_WN <- sqrt(80)/0.65
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.8)$mat
  
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)          #Variabili con gradual drift
  coeff2 <- round(runif(length(unstable_vars), min= -5, max = 5),2)      #Secondo set di coefficienti

  #Sudden drift in posizione.
  position = c(1,1825)    #Dal primo giorno all'ultimo. Non può essere cambiato
  prob <- c(0.2,1)       #Probabilità iniziale e finale di osservare il secondo regime
  
  #Ottenimento dei drift graduali
  processi <- get.processi.Gradual.Drift.Coeff(unstable_vars,n_giorni, coeff[unstable_vars + 1], coeff2,
                                               position, seed=seeds[job_index], prob)
  
  
  set.seed(seeds[job_index] + 5) 
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  out_variabile <- lin.out.conceptDrift(input, n_giorni, daily_obs, coeff, processi,
                                        unstable_vars)
  
  output <- out_variabile$output + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output,input,daily_obs,n_giorni)
  
  rm(list = c("output","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = out_variabile$coefficienti, processi = processi))
}
#rf 120 alberi, mtry = seq(5,55, by = 10), gb = 1500 alberi, nn = [50,50,50], patience 2, batch size = 64.
## Questo solo 50 replicazioni.



#Drift incrementale.
get_dataset_17_IncrementalCD <- function(job_index, sim_number) {
  
  set.seed(100)
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)/2.5
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  #Variabili influenzate dal drift
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)                   #Variabili con gradual drift
  coeff2 <- round(runif(length(unstable_vars), min= -5, max = 5),2)      #Secondo set di coefficienti

  #Ottenimento dei drift graduali
  processi <- get.processi.Incremental.Drift.Coeff(unstable_vars,n_giorni, coeff[unstable_vars + 1], coeff2,
                                                   seed=seeds[job_index])
  
  set.seed(seeds[job_index] + 6)
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs, mu = mu, Sigma = Sigma))
  out_variabile <- lin.out.conceptDrift(input, n_giorni, daily_obs, coeff, processi,
                                        unstable_vars)
  
  output <- out_variabile$output + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output,input,daily_obs,n_giorni)
  
  rm(list = c("output","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coeff, processi = processi))
  
  #Non c'è correlazione daun giorno al successivo, perciò non aggiungo altre variabili.
}
#rf = 120, gb = 1500, nn batch size = 64, patience = 2. [50,50,50]
#GB reduced = 50trees.


get_dataset_17_2_IncrementalCD <- function(job_index, sim_number) {
  
  set.seed(101)
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)/1.8
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  #Variabili influenzate dal drift
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)                   #Variabili con gradual drift
  coeff2 <- round(runif(length(unstable_vars), min= -5, max = 5),2)      #Secondo set di coefficienti
  
  #Ottenimento dei drift graduali
  processi <- get.processi.Incremental.Drift.Coeff(unstable_vars,n_giorni, coeff[unstable_vars + 1], coeff2,
                                                   seed=seeds[job_index])
  
  set.seed(seeds[job_index] + 6)
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs, mu = mu, Sigma = Sigma))
  out_variabile <- lin.out.conceptDrift(input, n_giorni, daily_obs, coeff, processi,
                                        unstable_vars)
  
  output <- out_variabile$output + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output,input,daily_obs,n_giorni)
  
  rm(list = c("output","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coeff, processi = processi))
  
  #Non c'è correlazione daun giorno al successivo, perciò non aggiungo altre variabili.
}
#rf = 120, gb = 1500, nn batch size = 64, patience = 2. [50,50,50]

#Proviamo a vedere che succede riducendo il numero di osservazioni.
get_dataset_17_IncrementalCD_2obs <- function(job_index, sim_number) {
  
  set.seed(100)
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 2
  sd_WN <- sqrt(80)/2.5
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  #Variabili influenzate dal drift
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)                   #Variabili con gradual drift
  coeff2 <- round(runif(length(unstable_vars), min= -5, max = 5),2)      #Secondo set di coefficienti
  
  #Ottenimento dei drift graduali
  processi <- get.processi.Incremental.Drift.Coeff(unstable_vars,n_giorni, coeff[unstable_vars + 1], coeff2,
                                                   seed=seeds[job_index])
  
  set.seed(seeds[job_index] + 8)
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs, mu = mu, Sigma = Sigma))
  out_variabile <- lin.out.conceptDrift(input, n_giorni, daily_obs, coeff, processi,
                                        unstable_vars)
  
  output <- out_variabile$output + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output,input,daily_obs,n_giorni)
  
  rm(list = c("output","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coeff, processi = processi))
  
  #Non c'è correlazione daun giorno al successivo, perciò non aggiungo altre variabili.
}
#rf = 120, gb = 1500, nn batch size = 64, patience = 2. [50,50,50]


#Ci siamo, questo può andare.
get_dataset_17_IncrementalCD_60var_10obs <- function(job_index, sim_number) {
  
  set.seed(102)   #Ruotiamo i parametri, seed = 100 di base.
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 60
  n_giorni <- 1825
  daily_obs <- 10
  sd_WN <- sqrt(80)/0.65
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.8)$mat
  
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)          #Variabili con gradual drift
  coeff2 <- round(runif(length(unstable_vars), min= -5, max = 5),2) 
  
  #Ottenimento dei drift graduali
  processi <- get.processi.Incremental.Drift.Coeff(unstable_vars,n_giorni, coeff[unstable_vars + 1], coeff2,
                                                   seed=seeds[job_index])
  
  set.seed(seeds[job_index] + 6)
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs, mu = mu, Sigma = Sigma))
  out_variabile <- lin.out.conceptDrift(input, n_giorni, daily_obs, coeff, processi,
                                        unstable_vars)
  
  output <- out_variabile$output + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output,input,daily_obs,n_giorni)
  
  rm(list = c("output","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coeff, processi = processi))
  
  #Non c'è correlazione daun giorno al successivo, perciò non aggiungo altre variabili.
}
#rf 120 alberi, mtry = seq(5,55, by = 10), gb = 1500 alberi, nn = [50,50,50], patience 2, batch size = 64.


#Recurrent drift.
#Proviamo a simulare un drift che evolva come un processo autoregressivo.
get_dataset_18_VarImpCD <- function(job_index, sim_number) {
  
  set.seed(100)
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)/2.5
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)          #Variabili con drift
  
  coefAR = c(0.6,0.3); coefMA = c(0.6,0.5);sd_ARMA = sqrt(0.5)/2
  
  #Ottenimento dei drift graduali
  processi <- get.processi.Recurrent.Drift.Coeff(unstable_vars,n_giorni, eachDay=3, seed=seeds[job_index],
                                                 coefAR, coefMA, sd_ARMA)

  #Se i processi rimanessero costanti?
  #processi <- matrix(0, nrow=length(unstable_vars), ncol=n_giorni,
  #                   byrow=TRUE)
  
  #Aggiungo a processi i coefficienti.
  for (i in 1:length(unstable_vars)) {
    processi[i,] <- processi[i,] + coeff[unstable_vars[i] + 1]
  }
  
  set.seed(seeds[job_index] + 9) #Per avere dei dati diversi da quelli del primo caso.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  out_variabile <- lin.out.conceptDrift(input, n_giorni, daily_obs, coeff, processi,
                                        unstable_vars)
  
  output <- out_variabile$output + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output,input,daily_obs,n_giorni)
  
  rm(list = c("output","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = out_variabile$coefficienti, processi = processi))
}
#rf = 120, gb = 1500, nn batch size = 64, patience = 2. [50,50,50]

#Valutare l'effetto del concept drift ricorrente sullo stesso dataset.
get_dataset_18_VarImpCD_60obs <- function(job_index, sim_number) {
  
  set.seed(100)
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 60
  sd_WN <- sqrt(80)/2.5
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)          #Variabili con drift
  
  coefAR = c(0.6,0.3); coefMA = c(0.6,0.5);sd_ARMA = sqrt(0.5)/2
  
  #Ottenimento dei drift graduali
  processi <- get.processi.Recurrent.Drift.Coeff(unstable_vars,n_giorni, eachDay=3, seed=seeds[job_index],
                                                 coefAR, coefMA, sd_ARMA)
  
  #Se i processi rimanessero costanti?
  #processi <- matrix(0, nrow=length(unstable_vars), ncol=n_giorni,
  #                   byrow=TRUE)
  
  #Aggiungo a processi i coefficienti.
  for (i in 1:length(unstable_vars)) {
    processi[i,] <- processi[i,] + coeff[unstable_vars[i] + 1]
  }
  
  set.seed(seeds[job_index] + 9) #Per avere dei dati diversi da quelli del primo caso.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  out_variabile <- lin.out.conceptDrift(input, n_giorni, daily_obs, coeff, processi,
                                        unstable_vars)
  
  output <- out_variabile$output + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output,input,daily_obs,n_giorni)
  
  rm(list = c("output","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = out_variabile$coefficienti, processi = processi))
}
#rf = 120, gb = 1500, nn batch size = 64, patience = 2. [50,50,50]
#Solo 50 simulazioni.

#### AVVICINARE LE PRESTAZIONI INIZIALI DI RF E GLI ALTRI MODELLI #####
get_dataset_16_GradualCD_4var <- function(job_index, sim_number) {
  
  set.seed(1600)
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 4
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)/3
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)          #Variabili con gradual drift
  coeff2 <- c(-1, -1)     #Secondo set di coefficienti
  
  #Sudden drift in posizione.
  position = c(1,1825)    #Dal primo giorno all'ultimo. Non può essere cambiato
  prob <- c(0.2,1)          #Probabilità iniziale e finale di osservare il secondo regime
  
  #Ottenimento dei drift graduali
  processi <- get.processi.Gradual.Drift.Coeff(unstable_vars,n_giorni, coeff[unstable_vars + 1], coeff2,
                                               position, seed=seeds[job_index], prob)
  
  
  set.seed(seeds[job_index] + 15) 
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  out_variabile <- lin.out.conceptDrift(input, n_giorni, daily_obs, coeff, processi,
                                        unstable_vars)
  
  output <- out_variabile$output + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output,input,daily_obs,n_giorni)
  
  rm(list = c("output","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = out_variabile$coefficienti, processi = processi))
}
#ridge = 0.589, rf = 0.572, gb = 0.581, nn = 0.583.
# No differenze finalmente.
#ridge soliti lambda, rf mtry = 1:4, gb = 1500 alberi.
#NN quant = ["V1",...,"V4"]

get_dataset_17_IncrementalCD_4var <- function(job_index, sim_number) {
  
  set.seed(1600)
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 4
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)/3
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  #Variabili influenzate dal drift
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)                   #Variabili con gradual drift
  coeff2 <- c(-1, -1)      #Secondo set di coefficienti
  
  #Ottenimento dei drift graduali
  processi <- get.processi.Incremental.Drift.Coeff(unstable_vars,n_giorni, coeff[unstable_vars + 1], coeff2,
                                                   seed=seeds[job_index])
  
  set.seed(seeds[job_index] + 6)
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs, mu = mu, Sigma = Sigma))
  out_variabile <- lin.out.conceptDrift(input, n_giorni, daily_obs, coeff, processi,
                                        unstable_vars)
  
  output <- out_variabile$output + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output,input,daily_obs,n_giorni)
  
  rm(list = c("output","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coeff, processi = processi))
}
#GB 500 alberi. RF mtry = seq(1,4,by=4),
#NN [50,50,50], b_s = 64, patience = 2.

get_dataset_17_IncrementalCD_2_4var <- function(job_index, sim_number) {
  
  set.seed(1601)
  # Quantità uguali in ogni dataset.
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 4
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)/3
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  #Variabili influenzate dal drift
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)                   #Variabili con gradual drift
  coeff2 <- runif(2, min = -5, max = 5)    #Secondo set di coefficienti
  
  #Ottenimento dei drift graduali
  processi <- get.processi.Incremental.Drift.Coeff(unstable_vars,n_giorni, coeff[unstable_vars + 1], coeff2,
                                                   seed=seeds[job_index])
  
  set.seed(seeds[job_index] + 6)
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs, mu = mu, Sigma = Sigma))
  out_variabile <- lin.out.conceptDrift(input, n_giorni, daily_obs, coeff, processi,
                                        unstable_vars)
  
  output <- out_variabile$output + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output,input,daily_obs,n_giorni)
  
  rm(list = c("output","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coeff, processi = processi))
}
#GB 500 alberi. RF mtry = seq(1,4,by=4),
#NN [50,50,50], b_s = 64, patience = 2.



## CONCEPT DRIFT con RELAZIONI più complesse ##
## Concept drift interazioni ##

get_dataset_16_GradualCD_Interazioni <- function(job_index, sim_number) {
  
  set.seed(413) #409 i primo. 411 buono, ma cambia poco le interazioni.
  # Quantità uguali in ogni dataset.
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 8
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)/2.2
  mu <- rep(0,nvar_input)
  coeff_lin1 <- runif(nvar_input + 1, min = -5, max = 5)
  n_int = 8 #Numero di interazioni
  coeff_int1 <- runif(n_int, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  coeffs <- c(coeff_lin1,coeff_int1)
  
  unstable_coeff <- sample(1:(nvar_input + n_int), floor((nvar_input + n_int)*0.5))
  
  ## Secondo set di coefficienti.
  coeffs2 <- runif(length(unstable_coeff), min = -5, max = 5)
  
  ##Variabili che vanno a costituire l'interazione
  interazioni <- int.Vars(nvar_input, n_int, seed = 411)
  
  #Specifiche del drift.
  position = c(1,1825)    #Dal primo giorno all'ultimo. Non può essere cambiato
  prob <- c(0.2,1)
  
  
  set.seed(seeds[job_index])
  # Quantità specifiche della simulazione in oggetto.
  
  processi <- get.processi.Gradual.Drift.Coeff(unstable_coeff, n_giorni,
                                               coeffs[unstable_coeff + 1], coeffs2,
                                               position, seed=seeds[job_index] + 3, prob)
  
  coefficienti <- matrix(rep(coeffs[-1], each=n_giorni), nrow=(nvar_input + n_int), ncol=n_giorni,
                         byrow=TRUE)
  
  coefficienti[unstable_coeff,] <- processi
  
  ## Generato l'input
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  
  ## Genero la componente lineare
  out.lin <- lin.out.conceptDrift2(input, n_giorni, daily_obs, coefficienti[1:(nvar_input),],
                                   beta0 = coeffs[1])
  
  out.int <- int.out.conceptDrift2(input, n_giorni, daily_obs, interazioni = interazioni,
                                   coefficienti[(nvar_input + 1):nrow(coefficienti),], n_int)
  
  
  output <- out.lin$output + out.int$output + noise.WN(n_giorni*daily_obs,sd_WN)
  
  data <- create.data(output,input, daily_obs, n_giorni)
  
  rm(list = c("out.lin","out.int","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coefficienti)) 
  
}
#RF mtry = seq(2,8,by=2), GB = 1500 alberi. size = [200,200,200], patience = 4.

get_dataset_16_GradualCD_Interazioni_5obs <- function(job_index, sim_number) {
  
  set.seed(413) #409 i primo. 411 buono, ma cambia poco le interazioni.
  # Quantità uguali in ogni dataset.
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 8
  n_giorni <- 1825
  daily_obs <- 5
  sd_WN <- sqrt(80)/2.2
  mu <- rep(0,nvar_input)
  coeff_lin1 <- runif(nvar_input + 1, min = -5, max = 5)
  n_int = 8 #Numero di interazioni
  coeff_int1 <- runif(n_int, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  coeffs <- c(coeff_lin1,coeff_int1)
  
  unstable_coeff <- sample(1:(nvar_input + n_int), floor((nvar_input + n_int)*0.5))
  
  ## Secondo set di coefficienti.
  coeffs2 <- runif(length(unstable_coeff), min = -5, max = 5)
  
  ##Variabili che vanno a costituire l'interazione
  interazioni <- int.Vars(nvar_input, n_int, seed = 411)
  
  #Specifiche del drift.
  position = c(1,1825)    #Dal primo giorno all'ultimo. Non può essere cambiato
  prob <- c(0.2,1)
  
  
  set.seed(seeds[job_index])
  # Quantità specifiche della simulazione in oggetto.
  
  processi <- get.processi.Gradual.Drift.Coeff(unstable_coeff, n_giorni,
                                               coeffs[unstable_coeff + 1], coeffs2,
                                               position, seed=seeds[job_index] + 3, prob)
  
  coefficienti <- matrix(rep(coeffs[-1], each=n_giorni), nrow=(nvar_input + n_int), ncol=n_giorni,
                         byrow=TRUE)
  
  coefficienti[unstable_coeff,] <- processi
  
  ## Generato l'input
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  
  ## Genero la componente lineare
  out.lin <- lin.out.conceptDrift2(input, n_giorni, daily_obs, coefficienti[1:(nvar_input),],
                                   beta0 = coeffs[1])
  
  out.int <- int.out.conceptDrift2(input, n_giorni, daily_obs, interazioni = interazioni,
                                   coefficienti[(nvar_input + 1):nrow(coefficienti),], n_int)
  
  
  output <- out.lin$output + out.int$output + noise.WN(n_giorni*daily_obs,sd_WN)
  
  data <- create.data(output,input, daily_obs, n_giorni)
  
  rm(list = c("out.lin","out.int","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coefficienti)) 
  
}
#RF mtry = seq(2,8,by=2), GB = 500 alberi. size = [200,200,200], patience = 4.

## Concept drift cubiche.
get_dataset_16_GradualCD_Cubiche <- function(job_index, sim_number) {
  
  set.seed(1670)
  # Quantità uguali in ogni dataset.
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)*1.5
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)          #Variabili con gradual drift
  coeff2 <- round(runif(length(unstable_vars), min= -5, max = 5),2)      #Secondo set di coefficienti
  
  #Sudden drift in posizione.
  position = c(1,1825)    #Dal primo giorno all'ultimo. Non può essere cambiato
  prob <- c(0.2,1)          #Probabilità iniziale e finale di osservare il secondo regime
  #prob <-c(0,0)
  
  #Ottenimento dei drift graduali
  processi <- get.processi.Gradual.Drift.Coeff(unstable_vars,n_giorni, coeff[unstable_vars + 1], coeff2,
                                               position, seed=seeds[job_index], prob)
  
  
  set.seed(seeds[job_index] + 5) #Per avere dei dati diversi da quelli del primo caso.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  
  out_variabile <- lin.out.conceptDrift(input^3, n_giorni, daily_obs, coeff, processi,
                                        unstable_vars)
  
  output <- out_variabile$output + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output,input,daily_obs,n_giorni)
  
  rm(list = c("output","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = out_variabile$coefficienti, processi = processi))
}
#RF 120, mtry = seq(2,10, by = 2), GB = 500trees.
## NN [200,200,200], batch_size = 64, patience = 4.


get_dataset_16_GradualCD_Cubiche_5obs <- function(job_index, sim_number) {
  
  set.seed(1670)
  # Quantità uguali in ogni dataset.
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 5
  sd_WN <- sqrt(80)*1.5
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)          #Variabili con gradual drift
  coeff2 <- round(runif(length(unstable_vars), min= -5, max = 5),2)      #Secondo set di coefficienti
  
  #Sudden drift in posizione.
  position = c(1,1825)    #Dal primo giorno all'ultimo. Non può essere cambiato
  prob <- c(0.2,1)          #Probabilità iniziale e finale di osservare il secondo regime
  #prob <-c(0,0)
  
  #Ottenimento dei drift graduali
  processi <- get.processi.Gradual.Drift.Coeff(unstable_vars,n_giorni, coeff[unstable_vars + 1], coeff2,
                                               position, seed=seeds[job_index], prob)
  
  
  set.seed(seeds[job_index] + 5) #Per avere dei dati diversi da quelli del primo caso.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  
  out_variabile <- lin.out.conceptDrift(input^3, n_giorni, daily_obs, coeff, processi,
                                        unstable_vars)
  
  output <- out_variabile$output + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output,input,daily_obs,n_giorni)
  
  rm(list = c("output","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = out_variabile$coefficienti, processi = processi))
}
#RF mtry = seq(2,10,by=2), gb = 500trees. NN patience = 4, batch_size = 64, [200,200,200].


get_dataset_17_IncrementalCD_Interazioni <- function(job_index, sim_number) {
  
  set.seed(413) #409 i primo. 411 buono, ma cambia poco le interazioni.
  # Quantità uguali in ogni dataset.
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 8
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)/2.2
  mu <- rep(0,nvar_input)
  coeff_lin1 <- runif(nvar_input + 1, min = -5, max = 5)
  n_int = 8 #Numero di interazioni
  coeff_int1 <- runif(n_int, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  coeffs <- c(coeff_lin1,coeff_int1)
  
  unstable_coeff <- sample(1:(nvar_input + n_int), floor((nvar_input + n_int)*0.5))
  
  ## Secondo set di coefficienti.
  coeffs2 <- runif(length(unstable_coeff), min = -5, max = 5)
  
  ##Variabili che vanno a costituire l'interazione
  interazioni <- int.Vars(nvar_input, n_int, seed = 411)

  set.seed(seeds[job_index]+11)
  # Quantità specifiche della simulazione in oggetto.

  processi <- get.processi.Incremental.Drift.Coeff(unstable_coeff,n_giorni,
                                                   coeffs[unstable_coeff + 1], coeffs2,
                                                   seed=seeds[job_index]+12)
  
  
  coefficienti <- matrix(rep(coeffs[-1], each=n_giorni), nrow=(nvar_input + n_int), ncol=n_giorni,
                         byrow=TRUE)
  
  coefficienti[unstable_coeff,] <- processi
  
  ## Generato l'input
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  
  ## Genero la componente lineare
  out.lin <- lin.out.conceptDrift2(input, n_giorni, daily_obs, coefficienti[1:(nvar_input),],
                                   beta0 = coeffs[1])
  
  out.int <- int.out.conceptDrift2(input, n_giorni, daily_obs, interazioni = interazioni,
                                   coefficienti[(nvar_input + 1):nrow(coefficienti),], n_int)
  
  
  output <- out.lin$output + out.int$output + noise.WN(n_giorni*daily_obs,sd_WN)
  
  data <- create.data(output,input, daily_obs, n_giorni)
  
  rm(list = c("out.lin","out.int","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coefficienti)) 
  
}
#RF mtry=seq(2,8,by=2), GB 1500 alberi. NN patience = 2, [200,200,200].
# Gb ridotta alberi = 250.

get_dataset_17_IncrementalCD_Interazioni_5obs <- function(job_index, sim_number) {
  
  set.seed(413) #409 i primo. 411 buono, ma cambia poco le interazioni.
  # Quantità uguali in ogni dataset.
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 8
  n_giorni <- 1825
  daily_obs <- 5
  sd_WN <- sqrt(80)/2.2
  mu <- rep(0,nvar_input)
  coeff_lin1 <- runif(nvar_input + 1, min = -5, max = 5)
  n_int = 8 #Numero di interazioni
  coeff_int1 <- runif(n_int, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  coeffs <- c(coeff_lin1,coeff_int1)
  
  unstable_coeff <- sample(1:(nvar_input + n_int), floor((nvar_input + n_int)*0.5))
  
  ## Secondo set di coefficienti.
  coeffs2 <- runif(length(unstable_coeff), min = -5, max = 5)
  
  ##Variabili che vanno a costituire l'interazione
  interazioni <- int.Vars(nvar_input, n_int, seed = 411)
  
  set.seed(seeds[job_index]+15)
  # Quantità specifiche della simulazione in oggetto.
  
  processi <- get.processi.Incremental.Drift.Coeff(unstable_coeff,n_giorni,
                                                   coeffs[unstable_coeff + 1], coeffs2,
                                                   seed=seeds[job_index]+14)
  
  
  coefficienti <- matrix(rep(coeffs[-1], each=n_giorni), nrow=(nvar_input + n_int), ncol=n_giorni,
                         byrow=TRUE)
  
  coefficienti[unstable_coeff,] <- processi
  
  ## Generato l'input
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  
  ## Genero la componente lineare
  out.lin <- lin.out.conceptDrift2(input, n_giorni, daily_obs, coefficienti[1:(nvar_input),],
                                   beta0 = coeffs[1])
  
  out.int <- int.out.conceptDrift2(input, n_giorni, daily_obs, interazioni = interazioni,
                                   coefficienti[(nvar_input + 1):nrow(coefficienti),], n_int)
  
  
  output <- out.lin$output + out.int$output + noise.WN(n_giorni*daily_obs,sd_WN)
  
  data <- create.data(output,input, daily_obs, n_giorni)
  
  rm(list = c("out.lin","out.int","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coefficienti)) 
  
}
#RF seq(2,10,by=2), GB 1500 alberi. NN patience = 2, [200,200,200].


get_dataset_17_IncrementalCD_Cubiche <- function(job_index, sim_number) {
  
  set.seed(1670)
  # Quantità uguali in ogni dataset.
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)*1.5
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)          #Variabili con gradual drift
  coeff2 <- round(runif(length(unstable_vars), min= -5, max = 5),2)      #Secondo set di coefficienti
  
  #Sudden drift in posizione.
  position = c(1,1825)    #Dal primo giorno all'ultimo. Non può essere cambiato

  
  processi <- get.processi.Incremental.Drift.Coeff(unstable_vars,n_giorni, coeff[unstable_vars + 1], coeff2,
                                                   seed=seeds[job_index])
  
  set.seed(seeds[job_index] + 5) #Per avere dei dati diversi da quelli del primo caso.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  
  out_variabile <- lin.out.conceptDrift(input^3, n_giorni, daily_obs, coeff, processi,
                                        unstable_vars)
  
  output <- out_variabile$output + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output,input,daily_obs,n_giorni)
  
  rm(list = c("output","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = out_variabile$coefficienti, processi = processi))
}
# RF seq(2,10,by=2), GB 1500 alberi. NN patience = 2, [200,200,200].


get_dataset_17_IncrementalCD_Cubiche_5obs <- function(job_index, sim_number) {
  
  set.seed(1670)
  # Quantità uguali in ogni dataset.
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 5
  sd_WN <- sqrt(80)*1.5
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)          #Variabili con gradual drift
  coeff2 <- round(runif(length(unstable_vars), min= -5, max = 5),2)      #Secondo set di coefficienti
  
  #Sudden drift in posizione.
  position = c(1,1825)    #Dal primo giorno all'ultimo. Non può essere cambiato
  
  
  processi <- get.processi.Incremental.Drift.Coeff(unstable_vars,n_giorni, coeff[unstable_vars + 1], coeff2,
                                                   seed=seeds[job_index])
  
  set.seed(seeds[job_index] + 5) #Per avere dei dati diversi da quelli del primo caso.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  
  out_variabile <- lin.out.conceptDrift(input^3, n_giorni, daily_obs, coeff, processi,
                                        unstable_vars)
  
  output <- out_variabile$output + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output,input,daily_obs,n_giorni)
  
  rm(list = c("output","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = out_variabile$coefficienti, processi = processi))
}
# RF seq(2,10,by=2), GB 1500 alberi. NN patience = 10, [200,200,200].


get_dataset_17_IncrementalCD_2_Cubiche <- function(job_index, sim_number) {
  
  set.seed(1883)
  # Quantità uguali in ogni dataset.
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)*1.5
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)          #Variabili con gradual drift
  coeff2 <- round(runif(length(unstable_vars), min= -5, max = 5),2)      #Secondo set di coefficienti
  
  #Sudden drift in posizione.
  position = c(1,1825)    #Dal primo giorno all'ultimo. Non può essere cambiato
  
  
  processi <- get.processi.Incremental.Drift.Coeff(unstable_vars,n_giorni, coeff[unstable_vars + 1], coeff2,
                                                   seed=seeds[job_index])
  
  set.seed(seeds[job_index] + 5) #Per avere dei dati diversi da quelli del primo caso.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  
  out_variabile <- lin.out.conceptDrift(input^3, n_giorni, daily_obs, coeff, processi,
                                        unstable_vars)
  
  output <- out_variabile$output + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output,input,daily_obs,n_giorni)
  
  rm(list = c("output","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = out_variabile$coefficienti, processi = processi))
}
# RF seq(2,10,by=2), GB 1500 alberi. NN patience = 2, [200,200,200].

get_dataset_17_IncrementalCD_2_Cubiche_5obs <- function(job_index, sim_number) {
  
  set.seed(1883)
  # Quantità uguali in ogni dataset.
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 5
  sd_WN <- sqrt(80)*1.5
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)          #Variabili con gradual drift
  coeff2 <- round(runif(length(unstable_vars), min= -5, max = 5),2)      #Secondo set di coefficienti
  
  #Sudden drift in posizione.
  position = c(1,1825)    #Dal primo giorno all'ultimo. Non può essere cambiato
  
  
  processi <- get.processi.Incremental.Drift.Coeff(unstable_vars,n_giorni, coeff[unstable_vars + 1], coeff2,
                                                   seed=seeds[job_index])
  
  set.seed(seeds[job_index] + 5) #Per avere dei dati diversi da quelli del primo caso.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  
  out_variabile <- lin.out.conceptDrift(input^3, n_giorni, daily_obs, coeff, processi,
                                        unstable_vars)
  
  output <- out_variabile$output + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output,input,daily_obs,n_giorni)
  
  rm(list = c("output","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = out_variabile$coefficienti, processi = processi))
}
# RF seq(2,10,by=2), GB 1000 alberi. NN patience = 10, [200,200,200]






## STAGIONALITA ##
get_dataset_20_stagErrore <- function(job_index, sim_number) {
  
  set.seed(2001)
  #Quantità uguali per ogni simulazione
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 4
  n_giorni <- 365*11 #Generiamo un anno in più per avere i ritardi stagionali.
  daily_obs <- 30
  sd_WN <- sqrt(80)/3.5  ## Errore nella singola obs
  #sd_stag <- sqrt(1)  ## Per sporcare il processo stagionale.
  #sd_stag <- sqrt(2) *1.2  ## In job1 poca differenza
  sd_stag <- sqrt(2)*3.5
  mu <- rep(0,nvar_input)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  coeff_lin <- runif(nvar_input + 1, min = -5, max = 5)
  
  #Stagionalità.
  seasonality <- get.seasonality(seed = seeds[job_index] + 1, n_giorni = n_giorni,
                                 s = 12, max1 = 40, max2 = 32, sd_stag = sd_stag, span = 0.02)
  
  set.seed(seeds[job_index])
  # Quantità specifiche della simulazione in oggetto.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  
  output <- lin.out(input, coeff_lin) + noise.WN(n_giorni*daily_obs,sd_WN) +
    rep(seasonality, each = daily_obs)
  
  data <- create.data(output,input,daily_obs,n_giorni)
  
  ## Aggiungo le variabili per catturare questa variazione.
  data$day_of_year <- rep(rep(1:365,each=daily_obs), n_giorni/365)
  
  # Lunghezza dei mesi.
  m <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  m1 <- c()
  for (i in 1:12) {
    m1 <- c(m1,rep(i,m[i]))
  }
  data$month_of_year <- factor(rep(rep(m1, each = daily_obs), n_giorni/365))
  
  ## Inserisco i ritardi
  data.dm <- get.daily.means(data)
  data$output.l1 <- NA
  data$output.l2 <- NA
  for (i in unique(data$Days)) {
    data$output.l1[data$Days == i] <- data.dm$data$output.mean.lag1[data.dm$data$Days == i]
    data$output.l2[data$Days == i] <- data.dm$data$output.mean.lag2[data.dm$data$Days == i]
  }
  
  #E il ritardo stagionale.
  data$output.sl1 <- NA
  for (i in unique(data$Days)) {
    data$output.sl1[data$Days == i] <- mean(data$output[data$Days == (i - 365)], na.rm=TRUE)
  }
  
  data <- na.omit(data)
  data$Days = rep(1:(n_giorni - 365),each=daily_obs)
  
  return(list(data=data, seeds=seeds, coeff=coeff_lin))
}
#RF, mtry = seq(2,8,by = 2), alberi = 120.
#GB 2k alberi
#NN [200,200,200], solite impostazioni.


get_dataset_20_stagAbitMoreSTAG <- function(job_index, sim_number) {
  
  set.seed(2001)
  #Quantità uguali per ogni simulazione
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 4
  n_giorni <- 365*11 #Generiamo un anno in più per avere i ritardi stagionali.
  daily_obs <- 30
  sd_WN <- sqrt(80)/3.5  ## Errore nella singola obs
  sd_stag <- sqrt(1)*1.35  ## Per sporcare il processo stagionale.
  mu <- rep(0,nvar_input)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  coeff_lin <- runif(nvar_input + 1, min = -5, max = 5)
  
  Gamma <- get.gamma(seed = 102, s = 12, max1 = 20, max2 = 16)
  print(Gamma)
  
  #Stagionalità.
  seasonality <- get.seasonality.Right(seed = (seeds[job_index] + 1), n_giorni = n_giorni,
                                 s = 12, Gamma = Gamma, sd_stag = sd_stag, span = 0.02)
  
  set.seed(seeds[job_index])
  # Quantità specifiche della simulazione in oggetto.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  
  output <- lin.out(input, coeff_lin) + noise.WN(n_giorni*daily_obs,sd_WN) +
    rep(seasonality, each = daily_obs)
  
  data <- create.data(output,input,daily_obs,n_giorni)
  
  ## Aggiungo le variabili per catturare questa variazione.
  data$day_of_year <- rep(rep(1:365,each=daily_obs), n_giorni/365)
  
  # Lunghezza dei mesi.
  m <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  m1 <- c()
  for (i in 1:12) {
    m1 <- c(m1,rep(i,m[i]))
  }
  data$month_of_year <- factor(rep(rep(m1, each = daily_obs), n_giorni/365))
  
  
  #Senza i ritardi stagionali.
  #data <- na.omit(data)
  data <- data[data$Days >365,]
  # Tolto il primo anno, in modo da avere gli stessi dataset di prima.
  
  data$Days = rep(1:(n_giorni - 365),each=daily_obs)
  
  return(list(data=data, seeds=seeds, coeff=coeff_lin))
}
#NN [200,200,200] patience = 4, batch_size = 64. quant = ["V1","V2","V3","V4","day_of_year"].
#GB 2000 alberi.
#RF mtry = seq(2,6,by=2).

get_dataset_20_stagAbitMoreSTAGwithSLag <- function(job_index, sim_number) {
  
  set.seed(2001)
  #Quantità uguali per ogni simulazione
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 4
  n_giorni <- 365*11 #Generiamo un anno in più per avere i ritardi stagionali.
  daily_obs <- 30
  sd_WN <- sqrt(80)/3.5  ## Errore nella singola obs
  sd_stag <- sqrt(1)*1.35  ## Per sporcare il processo stagionale.
  mu <- rep(0,nvar_input)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  coeff_lin <- runif(nvar_input + 1, min = -5, max = 5)
  
  Gamma <- get.gamma(seed = 102, s = 12, max1 = 20, max2 = 16)
  print(Gamma)
  
  #Stagionalità.
  seasonality <- get.seasonality.Right(seed = (seeds[job_index] + 1), n_giorni = n_giorni,
                                       s = 12, Gamma = Gamma, sd_stag = sd_stag, span = 0.02)
  
  set.seed(seeds[job_index])
  # Quantità specifiche della simulazione in oggetto.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  
  output <- lin.out(input, coeff_lin) + noise.WN(n_giorni*daily_obs,sd_WN) +
    rep(seasonality, each = daily_obs)
  
  data <- create.data(output,input,daily_obs,n_giorni)
  
  ## Aggiungo le variabili per catturare questa variazione.
  data$day_of_year <- rep(rep(1:365,each=daily_obs), n_giorni/365)
  
  # Lunghezza dei mesi.
  m <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  m1 <- c()
  for (i in 1:12) {
    m1 <- c(m1,rep(i,m[i]))
  }
  data$month_of_year <- factor(rep(rep(m1, each = daily_obs), n_giorni/365))
  
  #E il ritardo stagionale.
  data$output.sl1 <- NA
  for (i in unique(data$Days)) {
    data$output.sl1[data$Days == i] <- mean(data$output[data$Days == (i - 365)], na.rm=TRUE)
  }
  
  #Senza i ritardi stagionali.
  #data <- na.omit(data)
  data <- data[data$Days >365,]
  # Tolto il primo anno, in modo da avere gli stessi dataset di prima.
  
  
  data$Days = rep(1:(n_giorni - 365),each=daily_obs)
  
  return(list(data=data, seeds=seeds, coeff=coeff_lin))
}
#NN [200,200,200] patience = 4, batch_size = 64. quant = ["V1","V2","V3","V4","day_of_year"].
#GB 2000 alberi.
#RF mtry = seq(2,6,by=2).


get_dataset_20_stagAbitMoreSTAGwithLag12 <- function(job_index, sim_number) {
  
  set.seed(2001)
  #Quantità uguali per ogni simulazione
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 4
  n_giorni <- 365*11 #Generiamo un anno in più per avere i ritardi stagionali.
  daily_obs <- 30
  sd_WN <- sqrt(80)/3.5  ## Errore nella singola obs
  sd_stag <- sqrt(1)*1.35  ## Per sporcare il processo stagionale.
  mu <- rep(0,nvar_input)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  coeff_lin <- runif(nvar_input + 1, min = -5, max = 5)
  
  Gamma <- get.gamma(seed = 102, s = 12, max1 = 20, max2 = 16)
  print(Gamma)
  
  #Stagionalità.
  seasonality <- get.seasonality.Right(seed = (seeds[job_index] + 1), n_giorni = n_giorni,
                                       s = 12, Gamma = Gamma, sd_stag = sd_stag, span = 0.02)
  
  set.seed(seeds[job_index])
  # Quantità specifiche della simulazione in oggetto.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  
  output <- lin.out(input, coeff_lin) + noise.WN(n_giorni*daily_obs,sd_WN) +
    rep(seasonality, each = daily_obs)
  
  data <- create.data(output,input,daily_obs,n_giorni)
  
  ## Aggiungo le variabili per catturare questa variazione.
  data$day_of_year <- rep(rep(1:365,each=daily_obs), n_giorni/365)
  
  # Lunghezza dei mesi.
  m <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  m1 <- c()
  for (i in 1:12) {
    m1 <- c(m1,rep(i,m[i]))
  }
  data$month_of_year <- factor(rep(rep(m1, each = daily_obs), n_giorni/365))
  
  #E il ritardo stagionale.
  data.dm <- get.daily.means(data)
  data$output.l1 <- NA
  data$output.l2 <- NA
  for (i in unique(data$Days)) {
    data$output.l1[data$Days == i] <- data.dm$data$output.mean.lag1[data.dm$data$Days == i]
    data$output.l2[data$Days == i] <- data.dm$data$output.mean.lag2[data.dm$data$Days == i]
  }
  
  #Senza i ritardi stagionali.
  #data <- na.omit(data)
  data <- data[data$Days >365,]
  # Tolto il primo anno, in modo da avere gli stessi dataset di prima.
  
  data$Days = rep(1:(n_giorni - 365),each=daily_obs)
  
  return(list(data=data, seeds=seeds, coeff=coeff_lin))
}
#NN [200,200,200] patience = 4, batch_size = 64. quant = ["V1","V2","V3","V4","day_of_year"].
#GB 2000 alberi.
#RF mtry = seq(2,6,by=2)

get_dataset_20_stagAbitMoreSTAGwithEverything <- function(job_index, sim_number) {
  
  set.seed(2001)
  #Quantità uguali per ogni simulazione
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 4
  n_giorni <- 365*11 #Generiamo un anno in più per avere i ritardi stagionali.
  daily_obs <- 30
  sd_WN <- sqrt(80)/3.5  ## Errore nella singola obs
  sd_stag <- sqrt(1)*1.35  ## Per sporcare il processo stagionale.
  mu <- rep(0,nvar_input)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  coeff_lin <- runif(nvar_input + 1, min = -5, max = 5)
  
  Gamma <- get.gamma(seed = 102, s = 12, max1 = 20, max2 = 16)
  print(Gamma)
  
  #Stagionalità.
  seasonality <- get.seasonality.Right(seed = (seeds[job_index] + 1), n_giorni = n_giorni,
                                       s = 12, Gamma = Gamma, sd_stag = sd_stag, span = 0.02)
  
  set.seed(seeds[job_index])
  # Quantità specifiche della simulazione in oggetto.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  
  output <- lin.out(input, coeff_lin) + noise.WN(n_giorni*daily_obs,sd_WN) +
    rep(seasonality, each = daily_obs)
  
  data <- create.data(output,input,daily_obs,n_giorni)
  
  ## Aggiungo le variabili per catturare questa variazione.
  data$day_of_year <- rep(rep(1:365,each=daily_obs), n_giorni/365)
  
  # Lunghezza dei mesi.
  m <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  m1 <- c()
  for (i in 1:12) {
    m1 <- c(m1,rep(i,m[i]))
  }
  data$month_of_year <- factor(rep(rep(m1, each = daily_obs), n_giorni/365))
  
  #E il ritardo stagionale.
  data.dm <- get.daily.means(data)
  data$output.l1 <- NA
  data$output.l2 <- NA
  for (i in unique(data$Days)) {
    data$output.l1[data$Days == i] <- data.dm$data$output.mean.lag1[data.dm$data$Days == i]
    data$output.l2[data$Days == i] <- data.dm$data$output.mean.lag2[data.dm$data$Days == i]
  }
  
  data$output.sl1 <- NA
  for (i in unique(data$Days)) {
    data$output.sl1[data$Days == i] <- mean(data$output[data$Days == (i - 365)], na.rm=TRUE)
  }
  
  #Senza i ritardi stagionali.
  #data <- na.omit(data)
  data <- data[data$Days >365,]
  # Tolto il primo anno, in modo da avere gli stessi dataset di prima.
  
  data$Days = rep(1:(n_giorni - 365),each=daily_obs)
  
  return(list(data=data, seeds=seeds, coeff=coeff_lin))
}
#GB 2000. RF = seq(2,8,by=2). 
#NN [200,200,200] patience = 4, batch_size = 64.
#Gb 65 alberi per la versione ridotta.


get_dataset_20_stagAbitMoreSTAGwithOnlySLag <- function(job_index, sim_number) {
  
  set.seed(2001)
  #Quantità uguali per ogni simulazione
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 4
  n_giorni <- 365*11 #Generiamo un anno in più per avere i ritardi stagionali.
  daily_obs <- 30
  sd_WN <- sqrt(80)/3.5  ## Errore nella singola obs
  sd_stag <- sqrt(1)*1.35  ## Per sporcare il processo stagionale.
  mu <- rep(0,nvar_input)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  coeff_lin <- runif(nvar_input + 1, min = -5, max = 5)
  
  Gamma <- get.gamma(seed = 102, s = 12, max1 = 20, max2 = 16)
  print(Gamma)
  
  #Stagionalità.
  seasonality <- get.seasonality.Right(seed = (seeds[job_index] + 1), n_giorni = n_giorni,
                                       s = 12, Gamma = Gamma, sd_stag = sd_stag, span = 0.02)
  
  set.seed(seeds[job_index])
  # Quantità specifiche della simulazione in oggetto.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  
  output <- lin.out(input, coeff_lin) + noise.WN(n_giorni*daily_obs,sd_WN) +
    rep(seasonality, each = daily_obs)
  
  data <- create.data(output,input,daily_obs,n_giorni)
  
  #E il ritardo stagionale.
  data$output.sl1 <- NA
  for (i in unique(data$Days)) {
    data$output.sl1[data$Days == i] <- mean(data$output[data$Days == (i - 365)], na.rm=TRUE)
  }
  
  #Senza i ritardi stagionali.
  #data <- na.omit(data)
  data <- data[data$Days >365,]
  # Tolto il primo anno, in modo da avere gli stessi dataset di prima.
  
  
  data$Days = rep(1:(n_giorni - 365),each=daily_obs)
  
  return(list(data=data, seeds=seeds, coeff=coeff_lin))
}
#NN [200,200,200] patience = 4, batch_size = 64. quant = ["V1","V2","V3","V4","output.sl1"]
#GB 1500, rf mtry=seq(1,4,by=1).
#50 replicazioni.


get_dataset_20_stagAbitMoreSTAGwithOnlyLag12 <- function(job_index, sim_number) {
  
  set.seed(2001)
  #Quantità uguali per ogni simulazione
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 4
  n_giorni <- 365*11 #Generiamo un anno in più per avere i ritardi stagionali.
  daily_obs <- 30
  sd_WN <- sqrt(80)/3.5  ## Errore nella singola obs
  sd_stag <- sqrt(1)*1.35  ## Per sporcare il processo stagionale.
  mu <- rep(0,nvar_input)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  coeff_lin <- runif(nvar_input + 1, min = -5, max = 5)
  
  Gamma <- get.gamma(seed = 102, s = 12, max1 = 20, max2 = 16)
  print(Gamma)
  
  #Stagionalità.
  seasonality <- get.seasonality.Right(seed = (seeds[job_index] + 1), n_giorni = n_giorni,
                                       s = 12, Gamma = Gamma, sd_stag = sd_stag, span = 0.02)
  
  set.seed(seeds[job_index])
  # Quantità specifiche della simulazione in oggetto.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  
  output <- lin.out(input, coeff_lin) + noise.WN(n_giorni*daily_obs,sd_WN) +
    rep(seasonality, each = daily_obs)
  
  data <- create.data(output,input,daily_obs,n_giorni)
  
  #E il ritardo stagionale.
  data.dm <- get.daily.means(data)
  data$output.l1 <- NA
  data$output.l2 <- NA
  for (i in unique(data$Days)) {
    data$output.l1[data$Days == i] <- data.dm$data$output.mean.lag1[data.dm$data$Days == i]
    data$output.l2[data$Days == i] <- data.dm$data$output.mean.lag2[data.dm$data$Days == i]
  }
  
  #Senza i ritardi stagionali.
  #data <- na.omit(data)
  data <- data[data$Days >365,]
  # Tolto il primo anno, in modo da avere gli stessi dataset di prima.
  
  
  data$Days = rep(1:(n_giorni - 365),each=daily_obs)
  
  return(list(data=data, seeds=seeds, coeff=coeff_lin))
}
#NN [200,200,200] patience = 4, batch_size = 64. quant = ["V1","V2","V3","V4","output.l1","output.l1"]
#GB 1500, rf mtry=seq(2,6,by=2).



get_dataset_20_stagAbitMoreSTAGwithEverything_2evenLess <- function(job_index, sim_number) {
  
  set.seed(2002)
  #Quantità uguali per ogni simulazione
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 4
  n_giorni <- 365*11 #Generiamo un anno in più per avere i ritardi stagionali.
  daily_obs <- 30
  sd_WN <- sqrt(80)/3.5  ## Errore nella singola obs
  sd_stag <- sqrt(1)*0.65  ## Per sporcare il processo stagionale.
  mu <- rep(0,nvar_input)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  coeff_lin <- runif(nvar_input + 1, min = -5, max = 5)
  
  Gamma <- get.gamma(seed = 104, s = 12, max1 = 10, max2 = 8)
  print(Gamma)
  
  #Stagionalità.
  seasonality <- get.seasonality.Right(seed = (seeds[job_index] + 1), n_giorni = n_giorni,
                                       s = 12, Gamma = Gamma, sd_stag = sd_stag, span = 0.02)
  
  set.seed(seeds[job_index])
  # Quantità specifiche della simulazione in oggetto.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  
  output <- lin.out(input, coeff_lin) + noise.WN(n_giorni*daily_obs,sd_WN) +
    rep(seasonality, each = daily_obs)
  
  data <- create.data(output,input,daily_obs,n_giorni)
  
  ## Aggiungo le variabili per catturare questa variazione.
  data$day_of_year <- rep(rep(1:365,each=daily_obs), n_giorni/365)
  
  # Lunghezza dei mesi.
  m <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  m1 <- c()
  for (i in 1:12) {
    m1 <- c(m1,rep(i,m[i]))
  }
  data$month_of_year <- factor(rep(rep(m1, each = daily_obs), n_giorni/365))
  
  #E il ritardo stagionale.
  data.dm <- get.daily.means(data)
  data$output.l1 <- NA
  data$output.l2 <- NA
  for (i in unique(data$Days)) {
    data$output.l1[data$Days == i] <- data.dm$data$output.mean.lag1[data.dm$data$Days == i]
    data$output.l2[data$Days == i] <- data.dm$data$output.mean.lag2[data.dm$data$Days == i]
  }

  data$output.sl1 <- NA
  for (i in unique(data$Days)) {
    data$output.sl1[data$Days == i] <- mean(data$output[data$Days == (i - 365)], na.rm=TRUE)
  }
  
  #Senza i ritardi stagionali.
  #data <- na.omit(data)
  data <- data[data$Days >365,]
  # Tolto il primo anno, in modo da avere gli stessi dataset di prima.
  
  data$Days = rep(1:(n_giorni - 365),each=daily_obs)
  
  return(list(data=data, seeds=seeds, coeff=coeff_lin))
}
#RF seq(2,6, by = 2), Gb = 1500 alberi. NN [200,200,200] patience = 4.


get_dataset_20_stagAbitMoreSTAGwithEverythingAndTrend <- function(job_index, sim_number) {
  
  set.seed(2001)
  #Quantità uguali per ogni simulazione
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 4
  n_giorni <- 365*11 #Generiamo un anno in più per avere i ritardi stagionali.
  daily_obs <- 30
  sd_WN <- sqrt(80)/3.5  ## Errore nella singola obs
  sd_stag <- sqrt(1)*1.35  ## Per sporcare il processo stagionale.
  mu <- rep(0,nvar_input)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  coeff_lin <- runif(nvar_input + 1, min = -5, max = 5)
  
  Gamma <- get.gamma(seed = 102, s = 12, max1 = 20, max2 = 16)
  print(Gamma)
  
  #Stagionalità.
  seasonality <- get.seasonality.Right(seed = (seeds[job_index] + 1), n_giorni = n_giorni,
                                       s = 12, Gamma = Gamma, sd_stag = sd_stag, span = 0.02)
  
  #Aggiungiamo il trend.
  rW <- get.Random.Walk(n_giorni, sd_RW = 0.02, drift = 0.002)
  
  set.seed(seeds[job_index])
  # Quantità specifiche della simulazione in oggetto.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  
  output <- lin.out(input, coeff_lin) + noise.WN(n_giorni*daily_obs,sd_WN) +
    rep(seasonality, each = daily_obs) + rep(rW, each = daily_obs)
  
  data <- create.data(output,input,daily_obs,n_giorni)
  
  ## Aggiungo le variabili per catturare questa variazione.
  data$day_of_year <- rep(rep(1:365,each=daily_obs), n_giorni/365)
  
  # Lunghezza dei mesi.
  m <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  m1 <- c()
  for (i in 1:12) {
    m1 <- c(m1,rep(i,m[i]))
  }
  data$month_of_year <- factor(rep(rep(m1, each = daily_obs), n_giorni/365))
  
  #E il ritardo stagionale.
  data.dm <- get.daily.means(data)
  data$output.l1 <- NA
  data$output.l2 <- NA
  for (i in unique(data$Days)) {
    data$output.l1[data$Days == i] <- data.dm$data$output.mean.lag1[data.dm$data$Days == i]
    data$output.l2[data$Days == i] <- data.dm$data$output.mean.lag2[data.dm$data$Days == i]
  }
  
  data$output.sl1 <- NA
  for (i in unique(data$Days)) {
    data$output.sl1[data$Days == i] <- mean(data$output[data$Days == (i - 365)], na.rm=TRUE)
  }
  
  #Senza i ritardi stagionali.
  #data <- na.omit(data)
  data <- data[data$Days >365,]
  # Tolto il primo anno, in modo da avere gli stessi dataset di prima.
  
  data$Days = rep(1:(n_giorni - 365),each=daily_obs)
  
  return(list(data=data, seeds=seeds, coeff=coeff_lin))
}
#RF seq(2,8,by = 2), Gb = 1500 alberi. NN [200,200,200] patience = 4.

## CASI AGGIUNTIVI ##

#Più errore
get_dataset_6_evenMoreNoise <- function(job_index, sim_number) {
  
  set.seed(650)
  # Quantità uguali in ogni dataset.
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)*2.2
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  # Quantità specifiche della simulazione in oggetto.
  set.seed(seeds[job_index])
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  output.lin <- lin.out(input, coef = coeff) + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output.lin,input, daily_obs, n_giorni)
  
  rm(list = c("output.lin","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coeff))
  
}
#Rf = 120, mtry = seq(2,10,by = 2), gb = 500, nn = 64 batch size, [50,50,50], 2 patience.

#Più correlazione
get_dataset_6_2_evenMoreCorrelation <- function(job_index, sim_number) {
  
  set.seed(652)
  # Quantità uguali in ogni dataset.
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)/4
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  
  # Quantità specifiche della simulazione in oggetto.
  set.seed(seeds[job_index])
  input <- high.corr.input(nvar_input, n_obs = n_giorni*daily_obs)
  output.lin <- lin.out(input, coef = coeff) + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output.lin,input, daily_obs, n_giorni)
  
  rm(list = c("output.lin","input"))
  return(list(data = data, seeds = seeds, coeff = coeff))
  
}
#Rf = 120, mtry = seq(2,10,by = 2)
#GB 1500, NN [50,50,50], batch_size = 64, patience = 2.

#Variabili di disturbo
get_dataset_6_3_disturbance <- function(job_index, sim_number) {
  
  set.seed(653)
  # Quantità uguali in ogni dataset.
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)/10
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  disturbanceFeatures <- sample(1:nvar_input, floor(nvar_input*0.8))
  coeff[disturbanceFeatures + 1] <- 0
  segni <- sample(c(-1,1), replace=TRUE, nvar_input)
  
  # Quantità specifiche della simulazione in oggetto.
  set.seed(seeds[job_index])
  input <- high.corr.input(nvar_input, n_obs = n_giorni*daily_obs, segni = segni)
  output.lin <- lin.out(input, coef = coeff) + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output.lin,input, daily_obs, n_giorni)
  
  rm(list = c("output.lin","input"))
  return(list(data = data, seeds = seeds, coeff = coeff))
  
}
#RF mtry = seq(2,10, by = 2), GB = 1500, NN [50,50,50], batch_size = 64, patience = 2.






### REPLICHE CONCEPT DRIFT ###
# CONCEPT DRIFT GRADUALE

#Iniziamo dal drift graduale.
#Utilizza gli stessi coefficienti del caso lineare iniziale.
get_dataset_16_2_GradualCD <- function(job_index, sim_number) {
  
  set.seed(98)
  # Quantità uguali in ogni dataset.
  seeds <- sample(1000:10000,sim_number,replace =FALSE)
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 30
  sd_WN <- sqrt(80)/2.5
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)          #Variabili con gradual drift
  coeff2 <- round(runif(length(unstable_vars), min= -5, max = 5),2)      #Secondo set di coefficienti
  #coeff2 <- coeff[unstable_vars + 1]
  
  #Sudden drift in posizione.
  position = c(1,1825)    #Dal primo giorno all'ultimo. Non può essere cambiato
  prob <- c(0.2,1)          #Probabilità iniziale e finale di osservare il secondo regime
  
  #Ottenimento dei drift graduali
  processi <- get.processi.Gradual.Drift.Coeff(unstable_vars,n_giorni, coeff[unstable_vars + 1], coeff2,
                                               position, seed=seeds[job_index], prob)
  
  
  set.seed(seeds[job_index] + 5) #Per avere dei dati diversi da quelli del primo caso.
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs,mu = mu, Sigma = Sigma))
  out_variabile <- lin.out.conceptDrift(input, n_giorni, daily_obs, coeff, processi,
                                        unstable_vars)
  
  output <- out_variabile$output + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output,input,daily_obs,n_giorni)
  
  rm(list = c("output","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = out_variabile$coefficienti, processi = processi))
}
#rf = 120, gb = 1000, nn batch size = 64, patience = 2. [50,50,50]

get_dataset_17_2_IncrementalCD_2obs <- function(job_index, sim_number) {
  
  set.seed(101)
  # Quantità uguali in ogni dataset.
  seeds <- floor(runif(sim_number,min=1000, max =10000))
  nvar_input <- 10
  n_giorni <- 1825
  daily_obs <- 2
  sd_WN <- sqrt(80)/1.8
  mu <- rep(0,nvar_input)
  coeff <- runif(nvar_input + 1, min = -5, max = 5)
  Sigma <- get.Sigma.Sparse(nvar_input, rate = 7, sparsity=0.3)$mat
  
  #Variabili influenzate dal drift
  unstable_vars <- sample(1:nvar_input,nvar_input*0.5)                   #Variabili con gradual drift
  coeff2 <- round(runif(length(unstable_vars), min= -5, max = 5),2)      #Secondo set di coefficienti
  
  #Ottenimento dei drift graduali
  processi <- get.processi.Incremental.Drift.Coeff(unstable_vars,n_giorni, coeff[unstable_vars + 1], coeff2,
                                                   seed=seeds[job_index])
  
  set.seed(seeds[job_index] + 6)
  input <- as.data.frame(mvrnorm(n = n_giorni*daily_obs, mu = mu, Sigma = Sigma))
  out_variabile <- lin.out.conceptDrift(input, n_giorni, daily_obs, coeff, processi,
                                        unstable_vars)
  
  output <- out_variabile$output + noise.WN(n_giorni*daily_obs,sd_WN)
  data <- create.data(output,input,daily_obs,n_giorni)
  
  rm(list = c("output","input","Sigma"))
  return(list(data = data, seeds = seeds, coeff = coeff, processi = processi))
  
  #Non c'è correlazione daun giorno al successivo, perciò non aggiungo altre variabili.
}
#rf = 120, gb = 1000, nn batch size = 64, patience = 2. [50,50,50]

