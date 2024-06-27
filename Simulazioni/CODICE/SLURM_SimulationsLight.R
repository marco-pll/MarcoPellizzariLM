
###################################################################################################
#####  SCRIPT SIMULAZIONI  ########################################################################
###################################################################################################

# Cosa fare per lanciare:
#   Scegliere il dataset da utilizzare (get_dataset_"number"())
#   Ripulire le directory in cui salvare i risultati (sim.out/models e sim.out/results)
#   Collocarsi nella directory SLURM_Simulazioni

# For debugging
#setwd("C:/Users/User/Documents/AI AGING/SLURM")
#path = "D:/shared_directory_VM/sim.out/esempi"


start_time = Sys.time()

library(MASS)    
library(glmnet)
library(ranger)
library(gbm)
library(sm)

## Matrice di Var/Cov
library(Matrix)

# Calcolo parallelo
library(foreach)
library(doParallel)
library(parallelly)
library(doRNG)

#Due script contenenti le funzioni necessarie, da collocare nella stessa cartella di questo.
source("SLURM_Base_Functions.R")                   #Funzioni general-purpose
source("SLURM_DataCreateFunctionsLight.R")

##################     VARIABILI GLOBALI    ########################################################

path = "sim.out"     #Path alla cartella contenente "models" e "results", in cui salvare i risultati
sim_number <- 100     #Numero di simulazioni.
array_task <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")) #Simulazione numero "array_task", environment var.
#array_task=1
cores=4  ## Core da utilizzare per il calcolo parallelo
cat("\ntask: ", array_task, "\ncores: ", cores,"\n")


#########   DATI     ################################################################################

data <- get_dataset_17_IncrementalCD_2_Cubiche(job_index = array_task,sim_number)
seeds <- data$seeds  #Ogni replicazione avrà un seed di riferimento
data <- data$data    #I dati.

set.seed(seeds[array_task])

##########     PARAMETRI    ##########################################################################

nsets <- 4              #Fold della convalida incrociata
retrain <- 60           #Multiplo di 3, distanza in giorni tra una regolazione ed un'altra
max_t0 <- 365           #Spostamento massimo, in giorni, dell'insieme di stima
finestra <- 30          #Giorni nella finestar mobile
mse_window <- 60        #Giorni nel test set

## Lunghezza del periodo di degenerazione.
max_dt <- max(data$Days) - (364 + max_t0) - finestra - mse_window

## Giorni ai quali stimare i modelli.
days_selected <- seq(365,365+max_t0, by = 5)

#Calcolo MSE di previsione.
MSSE_days_sel <- na.omit(MSSE_t0(data, days_selected))


######################################################################################

vars <- colnames(data)
escluse <- c("output","Days")
X <- setdiff(vars,escluse)

quant <- c()
for(var in X) {
  if(is.numeric(data[,var])) quant <- c(quant,var)
}
qual <- setdiff(X,quant)


#########    RIDGE     ###################################################################################

ridge_estimates <- function(training, train_set, test_set, lambda) {
  
  tmp_ridge <- glmnet(x = training[train_set,-c(days.pos,out.pos)], y = training[train_set,out.pos],alpha = 0, lambda = lambda)
  
  pred_ridge = predict(tmp_ridge, newx=training[test_set,-c(days.pos,out.pos)])
  errori_ridge = apply((training[test_set, out.pos] - pred_ridge)^2,2,mean)
  
  return(errori_ridge)
}

cv_ridge <- function(training, nsets, lambda, time_wise) {
  cat("\nCV...\n")
  
  if(time_wise == FALSE) {
    order <- sample(1:nrow(training),nrow(training)) 
  }
  
  errori_ridge <- matrix(NA,nrow=nsets, ncol=length(lambda))
  
  #Ciclo per ogni fold
  for (i in 1:nsets) {
    
    if(time_wise == TRUE) {
      
      dim_subset = floor(nrow(training)/(nsets+1))
      train_set <- 1:(i*dim_subset)
      test_set <- (i*dim_subset + 1):((i+1)*dim_subset)  ## Blocco successivo.
      
    }
    if(time_wise == FALSE) {
      
      dim_subset = floor(nrow(training)/nsets)
      test_set <- ((i-1)*dim_subset + 1):(i*dim_subset)
      train_set <- order[-test_set]
      test_set <- order[test_set]
    }
    
    errori_ridge[i,] <- ridge_estimates(training, train_set, test_set, lambda)
  }
  
  return(errori_ridge)
}

## Valori della degradazione.
results <- matrix(NA,nrow = max_t0 + 1, ncol = max_dt)
colnames(results) <- paste0("dt: ",seq(1:max_dt))
rownames(results) <- seq(0:(max_t0))

##Caratteristiche dei modelli
models.attr <- matrix(NA,nrow = max_t0 + 1, ncol = 3)
rownames(models.attr) <- seq(0:(max_t0))
colnames(models.attr) <- c("R2","Lambda use","MSE a t0")

data_x <- data[,c(X,"Days","output")]
data_x <- model.matrix(~.,na.omit(data_x))[,-1]

days.pos <- ncol(data_x) - 1
out.pos <- ncol(data_x)

## Griglia di Lambda da usare
lambda = 10^seq(2, -5, length=150)
lambda.use <- NA


set.seed(seeds[array_task])
for (i in days_selected) {

  #STANDARDIZZAZIONE DEL PRIMO ANNO DI TRAINING.
  train <- (data_x[,days.pos] >= (i - 364) & data_x[,days.pos] <= i)
  data_x_anno <- data_x[train,]
  centro <- colMeans(data_x_anno[,quant])
  scala <- apply(data_x_anno[,quant],2,sd)
  data_x_anno[,quant] <- scale(data_x_anno[,quant], center = centro, scale = scala)
  
  if(((i - 365) %% retrain) == 0) {
    
    m.ridge.cv <- cv_ridge(training = data_x_anno, nsets = nsets, lambda = lambda, time_wise = TRUE)
    lambda.use <- lambda[which.min(apply(m.ridge.cv,2,mean))]
  }
  
  m.ridge <- glmnet(x = data_x_anno[,-c(days.pos,out.pos)],y = data_x_anno[,out.pos],
                    alpha = 0, lambda = lambda.use, family = "gaussian")

  
  ## Mse a t0 con finestra successiva.
  t0_train <- (data_x[,days.pos] > i & data_x[,days.pos] <= (i + mse_window))
  data_x_window <- data_x[t0_train,-c(days.pos,out.pos)]
  data_x_window[,quant] <- scale(data_x_window[,quant], center = centro, scale = scala)
  fits.t0 <- predict(m.ridge, newx = data_x_window)
  mse.t0 <- mean((fits.t0 - data_x[t0_train,out.pos])^2)
  
  ## 
  models.attr[i - 364,2] <- lambda.use
  models.attr[i - 364,3] <- mse.t0
  
  ## R2 di training
  fits <- predict(m.ridge, newx = data_x_anno[,-c(days.pos,out.pos)])
  SSE <- sum((fits - data_x[train,out.pos])^2)
  DEV <- sum((data_x[train,out.pos] - mean(data_x[train,out.pos]))^2)
  models.attr[i - 364,1] <- 1 - SSE/DEV
  
  ## Previsioni del modello
  for_fits <- data_x[,days.pos] >= (i + mse_window + 1) & data_x[,days.pos] < (i + mse_window + max_dt + finestra)
  to_predict <- data_x[for_fits,out.pos]
  to_predict_X <- data_x[for_fits,-c(days.pos,out.pos)]
  to_predict_X[,quant] <- scale(to_predict_X[,quant], center = centro, scale = scala)
  
  
  ## Mi salvo anche i giorni.
  days_to_use <- data_x[for_fits,days.pos]
  
  ##Tutte le previsioni da i + 1 fino a i + 765, finestra poi di 30
  fits <- predict(m.ridge, newx = to_predict_X)
  
  ## Finestre mobili.
  for (j in 1:max_dt) {

    test <- (days_to_use >= (i + j + mse_window) & days_to_use < (i + j + mse_window + finestra))
    mse <- mean((to_predict[test] - fits[test])^2)

    results[i-364,j] <- mse
  }
  
  print(((i - 364)/(1 + max_t0))*100)
}

#3 minuti originale

models.attr.ridge <- as.data.frame(na.omit(models.attr))
results.ridge <- as.data.frame(na.omit(results))
models.attr.ridge$R2_pred <- 1 - models.attr.ridge$`MSE a t0`/MSSE_days_sel$MSSE_t0

#plot(as.numeric(results.ridge[1,]), type="l")

######   SALVATAGGIO RISULTATI  #############
save(models.attr.ridge, file =paste0(path,"/models/models.ridge.",array_task))
save(results.ridge, file =paste0(path,"/results/results.ridge.",array_task))




##########   RANDOM FOREST    ###############################################
rf_estimates <- function(training, train_set, test_set, reg.grid, seed_off) {
  
  cl <- makeCluster(1)
  clusterExport(cl, c("X"))
  registerDoParallel(cl)
  registerDoRNG(seeds[array_task] + seed_off)   # Rende replicabile la procedura, + permette di cambiare il seed
                                                #Durante la regolazione
  
  r <- foreach (j = 1:nrow(reg.grid), .packages = "ranger") %dopar%  {
    tmp_rf <- ranger(output ~ .,
                     data=training[train_set,],
                     num.tree = reg.grid[j,1],
                     importance = "impurity",
                     mtry = reg.grid[j,2], oob.error=FALSE)
    
    test.fit <- mean((training[test_set,"output"] - predict(tmp_rf,training[test_set,X])$prediction)^2)
    
    list(errori = test.fit)
  }
  
  stopCluster(cl)
  
  errori <- rep(NA,nrow(reg.grid))
  
  for(i in 1:nrow(reg.grid)) {
    errori[i] <- r[[i]]$errori
  }
  
  return(errori)
  
}

cv_rf <- function(training, nsets, reg.grid, time_wise, reg_num) {
  
  if(time_wise == FALSE) {
    order <- sample(1:nrow(training),nrow(training)) 
  }
  
  errori_rf <- matrix(NA,nrow=nsets, ncol=nrow(reg.grid))
  
  #Inizia il ciclo per ogni subset.
  for (i in 1:nsets) {
    cat("\nFold: ",i,"\n")
    if(time_wise == TRUE) {
      
      dim_subset = floor(nrow(training)/(nsets+1))
      train_set <- 1:(i*dim_subset)
      test_set <- (i*dim_subset + 1):((i+1)*dim_subset)  ## Blocco successivo.
      
    }
    if(time_wise == FALSE) {
      
      dim_subset = floor(nrow(training)/nsets)
      test_set <- ((i-1)*dim_subset + 1):(i*dim_subset)
      train_set <- order[-test_set]
      test_set <- order[test_set]
    }
    
    errori_rf[i,] <- rf_estimates(training, train_set, test_set, reg.grid, i*reg_num)
  }
  
  return(errori_rf)
}


data_x <- na.omit(data[,c(X,"Days","output")])

days.pos <- ncol(data_x) - 1
out.pos <- ncol(data_x)


## Valori della degenerazione.
results <- matrix(NA,nrow = max_t0 + 1, ncol = max_dt)
colnames(results) <- paste0("dt: ",seq(1:max_dt))
rownames(results) <- seq(0:(max_t0))

##Caratteristiche dei modelli
models.attr <- matrix(NA,nrow = max_t0 + 1, ncol = 4)
rownames(models.attr) <- seq(0:(max_t0))
colnames(models.attr) <- c("R2","NumTrees","Mtry","MSE a t0")

## Griglia

mtry <- seq(2,10, by = 2)
#mtry <- seq(2,6, by = 2)
#mtry <- seq(2,8, by = 2)
#mtry <- seq(1,4, by = 1)
#mtry <- seq(5,55, by = 10)
#mtry <- seq(40,80, by = 10)
num.trees <- 120    
#num.trees <- 120*4  #R^2 = 0.96, r^2 pred = 0.791

reg.grid <- expand.grid(num.trees,mtry)
colnames(reg.grid) <- c("Num trees","mtry")


######  Regolazione  ########

set.seed(seeds[array_task])

regulated <- c()
mse <- list()

k = 0

for (i in 365:(365+max_t0)) {
  
  if((i - 365) %% retrain == 0) {
    
    k = k + 1
    cat("\nRegolazione numero:",k)
    
    data_to_use <- (data_x[,days.pos]>= (i - 364) & data_x[,days.pos]<=i)
    
    data_x_anno <- data_x[data_to_use,]
    centro <- colMeans(data_x_anno[,quant])
    scala <- apply(data_x_anno[,quant],2,sd)
    data_x_anno[,quant] <- scale(data_x_anno[,quant], center = centro, scale = scala)
    
    output <- cv_rf(data_x_anno[,-days.pos], nsets, reg.grid, time_wise = TRUE, k) #k è per seedare.
    
    # Salvo gli alberi.
    mse[[k]] <- output
    
    mse_medi <- apply(output,2,mean)
    
    ##Elemento ottimale della griglia.
    opt <- which.min(mse_medi)
    regulated <- rbind(regulated, c(i,opt, as.numeric(reg.grid[opt,])))
  }
}

colnames(regulated) <- c("Numero","Optimal","Trees","Mtry")
regulated.rf <- regulated


#####  DEGENERAZIONE  #####

mtry_use <- NA
numTrees_use <- NA

cl <- makeCluster(1)
registerDoParallel(cl)
registerDoRNG(seeds[array_task])

r <- foreach (i = days_selected, .packages = "ranger") %dopar% {
  
  ## Dati da usare
  data_to_use <- (data_x[,days.pos]>= (i - 364) & data_x[,days.pos]<=i)
  data_x_anno <- data_x[data_to_use,]
  centro <- colMeans(data_x_anno[,quant])
  scala <- apply(data_x_anno[,quant],2,sd)
  data_x_anno[,quant] <- scale(data_x_anno[,quant], center = centro, scale = scala)
  
  ## Aggiorniamo la struttura.
  reg_to_use <- sum(i >= regulated[,1])
  opt <- regulated[reg_to_use,2]
  
  m.rf <- ranger(output ~ . ,
                 data=data_x_anno[,c(X,"output")],
                 num.tree = reg.grid[opt,1],
                 importance = "impurity",
                 mtry = reg.grid[opt,2], oob.error=FALSE)
  
  ## R2.
  fits.rf <- predict(m.rf,data_x_anno[,X])$prediction
  SSE <- sum((data_x[data_to_use,out.pos] - fits.rf)^2)
  DEV <- sum((data_x[data_to_use,out.pos] - mean(data_x[data_to_use,out.pos]))^2)
  
  R2 <- 1 - SSE/DEV
  
  ## MSE a t0 mse_window.
  t0_train <- (data_x[,days.pos] > i & data_x[,days.pos] <= (i + mse_window))
  data_x_window <- data_x[t0_train,-c(days.pos,out.pos)]
  data_x_window[,quant] <- scale(data_x_window[,quant], center = centro, scale = scala)
  
  fits.t0 <- predict(m.rf, data_x_window)$prediction
  mse.t0 <- mean((fits.t0 - data_x[t0_train,out.pos])^2)
  
  ## Previsioni del modello
  for_fits <- data_x[,days.pos] >= (i + mse_window + 1) & data_x[,days.pos] < (i + mse_window + max_dt + finestra)
  to_predict <- data_x[for_fits,out.pos]
  to_predict_X <- data_x[for_fits,-c(days.pos,out.pos)]
  to_predict_X[,quant] <- scale(to_predict_X[,quant], center = centro, scale = scala)
  
  ## Mi salvo anche i giorni.
  days_to_use <- data_x[for_fits,days.pos]
  
  ##Tutte le previsioni da i + 1 fino a i + max_dt, finestra poi di finestra
  fits <- predict(m.rf, to_predict_X)$prediction
  
  prestazioni <- rep(NA,max_dt)
  for (j in 1:max_dt) {
    
    test <- (days_to_use >= (i + j + mse_window) & days_to_use < (i + j + mse_window + finestra))
    mse <- mean((to_predict[test] - fits[test])^2)
    
    prestazioni[j] <- mse
  }
  
  list(R2 = R2, optimal = opt, mse_t0 = mse.t0, prestazioni = prestazioni, mtry = reg.grid[opt,2], numTrees =reg.grid[opt,1])
}
stopCluster(cl)

for (j in 1:length(days_selected)) {
  models.attr[days_selected[j] - 364,"R2"] <- r[[j]]$R2
  models.attr[days_selected[j] - 364,"NumTrees"] <- r[[j]]$numTrees
  models.attr[days_selected[j] - 364,"MSE a t0"] <- r[[j]]$mse_t0
  models.attr[days_selected[j] - 364,"Mtry"] <- r[[j]]$mtry
  results[days_selected[j] - 364,] <- r[[j]]$prestazioni
}


models.attr.rf <- as.data.frame(na.omit(models.attr))
results.rf <- as.data.frame(na.omit(results))
models.attr.rf$R2_pred <- 1 - models.attr.rf$`MSE a t0`/MSSE_days_sel$MSSE_t0

#plot(as.numeric(results.rf[60,]))
#lines(as.numeric(results.ridge[2,]), col="red")

######   SALVATAGGIO RISULTATI  #############
save(models.attr.rf, file =paste0(path,"/models/models.rf.",array_task))
save(results.rf, file =paste0(path,"/results/results.rf.",array_task))



############################   GRADIENT BOOSTING   ###############################################

#Numero di alberi da stimare come funzione della profondità.
tree.num <- function(shrink, depth) {
  
  #trees = 65
  trees = 80
  #trees = 500
  #trees = 100
  #trees = 250
  #trees = 3000
  #trees = 4000
  #trees = 1500
  #trees = 1000
  #trees = 2000
  #trees = 6000
  
  if(depth == 1) trees = trees*2.5
  if(depth == 2) trees = trees*1.8
  if(depth ==3) trees = trees*1.5
  if(depth>=4) trees = trees*1.2
  
  return(trees)
}

gb_estimates_par <- function(training, train_set, test_set, reg.grid, seed_off) {
  
  #Ridefinita all'interno, perché da fuori dava problemi.
  tree.num <- function(shrink, depth) {
    
    #trees = 65
    trees = 80
    #trees = 500
    #trees = 100
    #trees = 250
    #trees = 3000
    #trees = 4000
    #trees = 1500
    #trees = 1000
    #trees = 2000
    #trees = 6000
    
    if(depth == 1) trees = trees*2.5
    if(depth == 2) trees = trees*1.8
    if(depth ==3) trees = trees*1.5
    if(depth>=4) trees = trees*1.2
    
    return(trees)
  }
  
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  registerDoRNG(seeds[array_task] + seed_off)
  
  r <- foreach (j = 1:nrow(reg.grid), .packages = "gbm") %dopar%  {
    
    num.tree <- tree.num(shrink = reg.grid[j,2], depth =reg.grid[j,1])
    
    tmp_gb <- gbm(output~., data = training[train_set,], distribution = "gaussian",
                  n.trees = num.tree,
                  interaction.depth = reg.grid[j,1], shrinkage = reg.grid[j,2] )
    
    test.fit <- predict(tmp_gb, newdata = training[test_set,-ncol(training)], n.trees=1:num.tree)
    
    mses <- apply(test.fit, 2, function (pred) mean((training[test_set,"output"] - pred)^2))
    
    list(errori = mses[which.min(mses)], alberi = round(which.min(mses)/num.tree,2))
  }
  
  stopCluster(cl)
  
  errori <- rep(NA,nrow(grid))
  alberi <- rep(NA,nrow(grid))
  for(i in 1:nrow(grid)) {
    errori[i] <- r[[i]]$errori
    alberi[i] <- r[[i]]$alberi
  }
  
  risultati_list <- list(Errori = errori, Num_Alberi = alberi)
  return(risultati_list)
}

cv_gb <- function(training, nsets, reg.grid, time_wise) {
  
  if(time_wise == FALSE) {
    order <- sample(1:nrow(training),nrow(training)) 
  }
  
  errori_gb <- matrix(NA,nrow=nsets, ncol=nrow(reg.grid))
  num_alberi <- matrix(NA,nrow=nsets, ncol=nrow(reg.grid))
  
  #Inizia il ciclo per ogni subset.
  for (i in 1:nsets) {
    
    if(time_wise == TRUE) {
      
      dim_subset = floor(nrow(training)/(nsets+1))
      train_set <- 1:(i*dim_subset)
      test_set <- (i*dim_subset + 1):((i+1)*dim_subset)  ## Blocco successivo.
      
    }
    if(time_wise == FALSE) {
      
      dim_subset = floor(nrow(training)/nsets)
      test_set <- ((i-1)*dim_subset + 1):(i*dim_subset)
      train_set <- order[-test_set]
      test_set <- order[test_set]
    }
    
    cat("\n",i,":\n")
    estimates <- gb_estimates_par(training, train_set, test_set, reg.grid, i)
    errori_gb[i,] <- estimates$Errori
    num_alberi[i,] <- estimates$Num_Alberi
  }
  
  return(list("results" = errori_gb,"alberi" = num_alberi))
}

#Sceglie la profondità dell'albero sulla base del primo anno.
tree_depth <- function(data_x, grid) {
  
  data_to_use <- (data_x[,days.pos]>= 1 & data_x[,days.pos]<=365) #Usa il primo anno
  #Primo anno e standardizzo.
  data_x_anno <- data_x[data_to_use,]
  centro <- colMeans(data_x_anno[,quant])
  scala <- apply(data_x_anno[,quant],2,sd)
  data_x_anno[,quant] <- scale(data_x_anno[,quant], center = centro, scale = scala)
  
  output <- cv_gb(data_x_anno[,-days.pos], nsets, grid,time_wise = TRUE)
  print(output$alberi)
  opt <- which.min(apply(output$results,2,mean))
  return(c(365,opt,as.numeric(grid[opt,])))
}

## Se input correlati agli output, usare una X diversa.

data_x <- na.omit(data[,c(X,"Days","output")])
days.pos <- ncol(data_x) - 1
out.pos <- ncol(data_x)


##Valori della degenerazione
results <- matrix(NA,nrow = max_t0 + 1, ncol = max_dt)
colnames(results) <- paste0("dt: ",seq(1:(max_dt)))
rownames(results) <- seq(0:(max_t0))

##Caratteristiche dei modelli
models.attr <- matrix(NA,nrow = max_t0 + 1, ncol = 4)
rownames(models.attr) <- seq(0:(max_t0))
colnames(models.attr) <- c("R2","Grid_opt","MSE a t0","Alberi")

##Griglia
shrinkage <- c(0.08)
depths <- 1:4
#depths <- 4:6
grid <- expand.grid(depths,shrinkage)
colnames(grid) <- c("depth","shrink")

## Profondità degli alberi. La scelgo all'inizio tramite convalida incrociata.
set.seed(seeds[array_task])
regulated <- c()
regulated <- rbind(regulated,tree_depth(data_x,grid))

###### Degenerazione ######

cl <- makeCluster(cores)
registerDoParallel(cl)
registerDoRNG(seeds[array_task])              #Rende la parallelizazzione replicabile.

system.time(
  r <- foreach (i = days_selected, .packages = "gbm") %dopar% {
    ## Dati da usare
    data_to_use <- (data_x[,days.pos]>= (i - 364) & data_x[,days.pos]<=i)
    
    ##PRIMO ANNO STANDARDIZZATO
    data_x_anno <- data_x[data_to_use,]
    centro <- colMeans(data_x_anno[,quant])
    scala <- apply(data_x_anno[,quant],2,sd)
    data_x_anno[,quant] <- scale(data_x_anno[,quant], center = centro, scale = scala)
    ## Aggiorniamo la struttura.
    reg_to_use <- sum(i >= regulated[,1])
    
    ## Creo i training e i test set.
    train <- sample(1:nrow(data_x_anno), floor(nrow(data_x_anno)*0.85))
    training <- data_x_anno[train,-c(days.pos)]
    testing <- data_x_anno[-train,-c(days.pos)]
    
    opt <- regulated[reg_to_use,2]
    num.tree <- tree.num(shrink = grid[opt,2], depth = grid[opt,1])
    m.gb <- gbm(output~., data = training, distribution = "gaussian",
                n.trees = num.tree,
                interaction.depth = grid[opt,1], shrinkage = grid[opt,2] )
    
    ## Errori sul testing, per trovare il modello migliore.
    test.fit <- predict(m.gb, newdata = testing[,X], n.trees=1:num.tree)
    
    mses <- apply(test.fit, 2, function (pred) mean((testing[,"output"] - pred)^2))
    
    ##Numero di alberi ottimale.
    opt_tree <- which.min(mses)
    
    ## R2.
    fits.gb <- predict(m.gb,data_x_anno[,-c(days.pos,out.pos)], n.trees=opt_tree)
    SSE <- sum((data_x_anno[,out.pos] - fits.gb)^2)
    DEV <- sum((data_x_anno[,out.pos] - mean(data_x_anno[,out.pos]))^2)
    
    R2 <- 1 - SSE/DEV
    
    ##Elemento utilizzato.
    optimal_used <- regulated[reg_to_use,2]
    
    ## MSE a t0 mse_window.
    t0_train <- (data_x[,days.pos] > i & data_x[,days.pos] <= (i + mse_window))
    data_x_window <- data_x[t0_train,-c(days.pos,out.pos)]
    data_x_window[,quant] <- scale(data_x_window[,quant], center = centro, scale = scala)
    
    fits.t0 <- predict(m.gb, data_x_window, n.trees=opt_tree)
    mse.t0 <- mean((fits.t0 - data_x[t0_train,out.pos])^2)
    
    ## SALVO l'MSE a t0.
    
    for_fits <- data_x[,days.pos] >= (i + mse_window + 1) & data_x[,days.pos] < (i + mse_window + max_dt + finestra)
    to_predict <- data_x[for_fits,out.pos]
    to_predict_X <- data_x[for_fits,-c(days.pos,out.pos)]
    to_predict_X[,quant] <- scale(to_predict_X[,quant], center = centro, scale = scala)
    
    ## Mi salvo anche i giorni.
    days_to_use <- data_x[for_fits,days.pos]
    
    ##Tutte le previsioni da i + 1 fino a i + max_dt, finestra poi di finestra
    fits <- predict(m.gb, to_predict_X, n.trees=opt_tree)
    
    ## Finestre mobili
    
    prestazioni <- rep(NA,max_dt)
    for (j in 1:max_dt) {
      
      test <- (days_to_use >= (i + j + mse_window) & days_to_use < (i + j + mse_window + finestra))
      mse <- mean((to_predict[test] - fits[test])^2)
      
      prestazioni[j] <- mse
    }
    
    list(R2 = R2, optimal = optimal_used,opt_tree = round(opt_tree/num.tree,2), mse_t0 = mse.t0, prestazioni = prestazioni)
    
  }
)

stopCluster(cl)

for (j in 1:length(days_selected)) {
  models.attr[days_selected[j] - 364,"R2"] <- r[[j]]$R2
  models.attr[days_selected[j] - 364,"Grid_opt"] <- r[[j]]$optimal
  models.attr[days_selected[j] - 364,"MSE a t0"] <- r[[j]]$mse_t0
  models.attr[days_selected[j] - 364,"Alberi"] <- r[[j]]$opt_tree
  results[days_selected[j] - 364,] <- r[[j]]$prestazioni
}

models.attr.gb <- as.data.frame(na.omit(models.attr))
results.gb <- as.data.frame(na.omit(results))
models.attr.gb$R2_pred <- 1 - models.attr.gb$`MSE a t0`/MSSE_days_sel$MSSE_t0


# mod = 1
# plot(as.numeric(results.rf[1,]), type="l", col="blue", ylim=c(15,20))
# plot(as.numeric(results.gb[1,]), type="l", col="blue")
# plot(as.numeric(results.rf[mod,]), type="l", col="blue")
# lines(as.numeric(results.ridge[mod,]), col="red")
# lines(as.numeric(results.gb[mod,]), type="l", col = "green")

######   SALVATAGGIO RISULTATI   ########
save(models.attr.gb, file = paste0(path,"/models/models.gb.",array_task))
save(results.gb, file =paste0(path,"/results/results.gb.",array_task))


end_time <- Sys.time()
difftime(end_time,start_time)

