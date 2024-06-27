
##################################################
#### FUNZIONI PER SIMULAZIONI ####################
##################################################


######### FUNZIONI DI BASE  ################################################################################

## Matrice di varianza e covarianza con correlazione.
get_Sigma <- function(nvar_input, rho) {
  Sigma <- matrix(runif(nvar_input*nvar_input,min=-rho,max=rho),nrow=nvar_input, ncol=nvar_input)
  Sigma[lower.tri(Sigma)] <- t(Sigma)[lower.tri(Sigma)]
  diag(Sigma) <- rep(1, nvar_input)
  Sigma <- nearPD(Sigma, corr =TRUE, base.matrix = TRUE)
  return(list(mat = Sigma$mat, convergence = Sigma$converged))
}

get.Sigma.Sparse <- function(nvar_input, rate, sparsity) {
    
    fn <- function(x) {
      a = x
      a[sample(1:length(x), length(x)*sparsity)] <- 0
      return(a)
    }
    
    changeSign <- function(x) {
      a = x
      c <- sample(1:length(x), length(x)*0.5)
      a[c] <- a[c]*(-1)
      return(a)
    }
    
    Sigma <- matrix(rexp(nvar_input*nvar_input,rate = rate), nrow=nvar_input, ncol=nvar_input)
    Sigma <- apply(Sigma,1,fn)
    Sigma <- apply(Sigma,1,changeSign)
    Sigma[lower.tri(Sigma)] <- t(Sigma)[lower.tri(Sigma)]
    diag(Sigma) <- rep(1, nvar_input)
    Sigma <- nearPD(Sigma, corr = TRUE, base.matrix = TRUE)
    
    return(list(mat = Sigma$mat, convergence = Sigma$converged))
}

get.Sigma.Dense <- function(nvar_input, range) {
  
  changeSign <- function(x) {
    a = x
    c <- sample(1:length(x), length(x)*0.5)
    a[c] <- a[c]*(-1)
    return(a)
  }
  
  Sigma <- matrix(runif(nvar_input*nvar_input,min=range[1],max=range[2]),nrow=nvar_input, ncol=nvar_input)
  Sigma <- apply(Sigma,1,changeSign)
  Sigma[lower.tri(Sigma)] <- t(Sigma)[lower.tri(Sigma)]
  diag(Sigma) <- rep(1, nvar_input)
  Sigma <- nearPD(Sigma, corr =TRUE, base.matrix = TRUE)
  return(list(mat = Sigma$mat, convergence = Sigma$converged))
  
}

## Crea il dataset.
create.data <- function(output,input,daily_obs, n_giorni) {
  data <- cbind(input,output,rep(1:n_giorni,each=daily_obs))
  colnames(data)[ncol(data)] <- "Days"
  return(as.data.frame(data))
}

## Ottenimento degli MSE.
MSSE_t0 <- function(data_x, days_selected)  {
  
  MSSE <- data.frame(MSSE_t0 = NA)
  
  for (i in days_selected) {
    t0_train <- (data_x[,"Days"] > i & data_x[,"Days"] <= (i + mse_window))
    MSSE[i - 364,] <- mean((data_x[t0_train,"output"] - mean(data_x[t0_train,"output"]))^2)
  }
  
  return(MSSE)
}


## Noise normale
noise.WN <- function(n, sd_WN) {
  return(rnorm(n = n,mean = 0, sd = sd_WN))
}

## Output funzione lineare
lin.out <- function(input, coef) {
  ## coef[1] è l'intercetta, il resto i coefficienti.
  output <- apply(input,1,function(x) coef[1] + t(as.matrix(x))%*%as.matrix(coef[2:length(coef)]))
  return(output)
}

## Relazione quadratica con l'output
quad.out <- function(input, coeff_lin, coeff_quad) {
  
  ## Effetto lineare della variabile
  main.eff <- lin.out(input, coeff_lin)
  
  ## Effetti quadratici
  sq.eff <- apply(input,1,function(x) t(as.matrix(x^2))%*%as.matrix(coeff_quad))

  output <- main.eff + sq.eff
  rm(list=c("main.eff","sq.eff"))
  
  return(output)
}

## Relazione cubica con l'output
cub.out <- function(input, coeff_cub) {
  
  print(coeff_cub)
  ## Effetti cubici
  return(apply(input,1,function(x) t(as.matrix(x^3))%*%as.matrix(coeff_cub)))
}


## Interazione tra gli input
interaction.out <- function(input, coeff.lin, n_int, coeff.int, nvar_input, seed) {
  
  ## Definisco gli oggetti necessari all'interno
  ints <- matrix("V0", nrow=n_int, ncol=2)
  var_inter <- rep(0,ncol(input))
  names(var_inter) <- colnames(input)
  
  main.eff <- lin.out(input, coeff.lin)
  
  ## Interazioni casuali tra variabili
  ## Scegliamo le combinazioni tra cui inserire interazione.
  
  set.seed(seed)  #Per avere sempre le stesse interazioni in ogni simulazione
  int.eff <- 0
  
  for (i in 1:n_int){
    
    ## Primo termine di interazione
    possible_first <- names(which(var_inter < (nvar_input - 1)))
    first <- sample(possible_first,1)
    
    ## Secondo termine di interazione
    possible_second <- setdiff(colnames(input), first)
    possible_second <- setdiff(possible_second,ints[ints[,1] == first,2])
    possible_second <- setdiff(possible_second,ints[ints[,2] == first,1])
    second <- sample(possible_second,1)
    
    ## Aggiorno la matrice di controllo.
    ints[i,] <- c(first,second)
    ## E aggiorno il numero di interazioni per ciascun oggetto.
    var_inter[names(var_inter) == first] <- var_inter[names(var_inter) == first] + 1
    var_inter[names(var_inter) == second] <- var_inter[names(var_inter) == second] + 1
    
    
    primo_t <- colnames(input) == first
    secondo_t <- colnames(input) == second

    int.eff <- int.eff + apply(input,1,function(x) coeff.int[i]*x[primo_t]*x[secondo_t])
  }
  
  cat("\nInteract effects:",coeff.int)
  output <- main.eff + int.eff
  return(list(output = output,interazioni = ints,singole_int = var_inter))
  
}

high.corr.input <- function(nvar_input, sd_dist = 0.3, n_obs = n_giorni*daily_obs, segni) {
  
  x1 <- rnorm(n_obs, mean = 0, sd = 1)
  input <- matrix(NA, nrow=n_obs, ncol=nvar_input)
  for (j in 1:nvar_input) {
    sgn <- segni[j]
    input[,j] <- sgn*x1 + rnorm(n_obs, mean=0, sd = sd_dist)
  }
  return(as.data.frame(input))
}

#Funzione che mi tira fuori le variabili che fanno interazione
int.Vars <- function(nvar_input, n_int, seed) {
  
  ## Definisco gli oggetti necessari all'interno
  ints <- matrix("V0", nrow=n_int, ncol=2)
  var_inter <- rep(0,nvar_input)
  names(var_inter) <- paste0("V",1:nvar_input)
  
  set.seed(seed)  #Per avere sempre le stesse interazioni in ogni simulazione
  
  for (i in 1:n_int){
    
    ## Primo termine di interazione
    possible_first <- names(which(var_inter < (nvar_input - 1)))
    first <- sample(possible_first,1)
    
    ## Secondo termine di interazione
    possible_second <- setdiff(names(var_inter), first)
    possible_second <- setdiff(possible_second,ints[ints[,1] == first,2])
    possible_second <- setdiff(possible_second,ints[ints[,2] == first,1])
    second <- sample(possible_second,1)
    
    ## Aggiorno la matrice di controllo.
    ints[i,] <- c(first,second)
    ## E aggiorno il numero di interazioni per ciascun oggetto.
    var_inter[names(var_inter) == first] <- var_inter[names(var_inter) == first] + 1
    var_inter[names(var_inter) == second] <- var_inter[names(var_inter) == second] + 1
  }
  
  return(list(interazioni = ints,singole_int = var_inter))
  
}

## SECONDA PARTE DELLE FUNZIONI ##

## Ritorna un processo ARIMA simulato
noise.ARIMA <- function(n, coefAR, coefMA, d, sd) {
  arima.sim(n = n+365, list(order = c(length(coefAR), d, length(coefMA)),ar = coefAR, ma = coefMA),
            sd = sd)[366:(n+365)]
}


get.Random.Walk <- function(n, sd_RW, drift = 0) {
  rW <- rep(0,n+730)
  for (i in 2:(n+730)){
    rW[i] <- rW[i-1] + drift + rnorm(1,mean=0,sd = sd_RW)
  }
  return(rW[731:(n+730)])
}


## Ritorna le medie giornaliere del processo.
get.daily.means <- function(data) {
  data.dm <- data.frame(Days = unique(data$Days))
  data.dm$output.mean <- apply(as.matrix(data.dm$Days),1,function(x) mean(data$output[data$Days == x])) 
  data.dm$output.mean.lag1 <-  c(NA,data.dm$output.mean[1:(nrow(data.dm)-1)])
  data.dm$output.mean.lag2 <-  c(rep(NA,2),data.dm$output.mean[1:(nrow(data.dm)-2)])
  data.dm$output.mean.lag3 <-  c(rep(NA,3),data.dm$output.mean[1:(nrow(data.dm)-3)])
  data.dm$output.mean.lag4 <-  c(rep(NA,4),data.dm$output.mean[1:(nrow(data.dm)-4)])
  data.dm$output.mean.lag5 <-  c(rep(NA,5),data.dm$output.mean[1:(nrow(data.dm)-5)])
  data.dm$output.mean.lag6 <-  c(rep(NA,6),data.dm$output.mean[1:(nrow(data.dm)-6)])
  cor.l1 <- cor(data.dm$output.mean[-1], data.dm$output.mean.lag1[-1])
  cor.l2 <- cor(data.dm$output.mean[-c(1,2)], data.dm$output.mean.lag2[-c(1,2)])
  return(list(data = data.dm, cor.l1 = cor.l1,cor.l2 = cor.l2))
}

#Produce un andamento stagionale. Primo approccio.
noise.STAG.1 <- function(n_giorni, seed) {
  
  set.seed(seed)
  ## Generiamo 24 valori.
  a <- sample(2:23,1)
  Gamma <- c(sort(round(runif(a, min=0, max=100),2), decreasing =FALSE), sort(round(runif(24-a, min=0, max=80),2),decreasing=TRUE))
  Gamma <- Gamma - mean(Gamma)

  n_gamma <- (n_giorni)/365*24 + 6
  for(i in 25:n_gamma) {
    Gamma <- c(Gamma,-sum(Gamma[(i-23):(i - 1)]))
  }
  
  x <- floor(rep(seq(1,365, length.out = 24),n_giorni/365) + rep(365*c(0:((n_giorni/365)-1)), each=24))
  x <- c(c(-44,-29,-14),x,c(n_giorni + 15,n_giorni + 30,n_giorni + 45)) # Aggiungiamo valori prima e dopo.
  fit = loess(Gamma ~ x, span=0.065, degree=2)
  
  stag_average <- as.numeric(predict(fit, data.frame(x = 1:(n_giorni))))

  #Restituiamo le previsioni
  return(stag_average)
}

#Secondo approccio alla stagionalità
noise.STAG.2 <- function(n_giorni, sd_stag, seed) {
  set.seed(seed)
  ## Generiamo 24 valori.
  a <- sample(2:23,1)
  Gamma <- c(sort(round(runif(a, min=0, max=100),2), decreasing =FALSE), sort(round(runif(24-a, min=0, max=80),2),decreasing=TRUE))
  Gamma <- Gamma - mean(Gamma)
  
  n_gamma <- (n_giorni)/365*24 + 6
  for(i in 25:n_gamma) {
    Gamma <- c(Gamma,(-sum(Gamma[(i-23):(i - 1)]) + rnorm(1,sd = sd_stag)))
  }

  
  x <- floor(rep(seq(1,365, length.out = 24),n_giorni/365) + rep(365*c(0:((n_giorni/365)-1)), each=24))
  x <- c(c(-44,-29,-14),x,c(n_giorni + 15,n_giorni + 30,n_giorni + 45)) # Aggiungiamo valori prima e dopo.
  fit = loess(Gamma ~ x, span=0.065, degree=2)
  
  stag_average <- as.numeric(predict(fit, data.frame(x = 1:(n_giorni))))
  
  #Restituiamo le previsioni
  return(stag_average)
  
}

#Restituisce una serie di processi stagionali, usando la logica di noise.STAG.2
get.processi.STAG <- function(nProc, n, sd_STAG, seed) {
  
  set.seed(seed) #Dev'essere uguale ad ogni simulazione
  processi <- matrix(NA, nrow=nProc, ncol=n)
  seeds_STAG <- runif(nProc,min=1000, max=10000)
  
  for(i in 1:nProc) {
    processi[i,] <- noise.STAG.2(n, sd_STAG, seeds_STAG[i])
  }
  
  return(processi)
}

#Effetti stagionali senza il termine di disturbo
get.processi.STAG2 <- function(nProc, n, seed) {
  
  set.seed(seed) #Dev'essere uguale ad ogni simulazione
  processi <- matrix(NA, nrow=nProc, ncol=n)
  seeds_STAG <- runif(nProc,min=1000, max=10000)
  
  for(i in 1:nProc) {
    processi[i,] <- noise.STAG.1(n, seeds_STAG[i])
  }
  
  return(processi)
}


get.seasonality <- function(seed, n_giorni, s, max1, max2, sd_stag,
                            span = 0.02) {
  set.seed(seed)
  a <- sample(2:(s-1),1)
  
  #Valori iniziali
  firstSegment <- sort(round(runif(a, min=0, max=max1),2), decreasing =FALSE)
  secondSegment <- sort(round(runif(s-a, min=0, max=max2),2),decreasing=TRUE)
  Gamma <- c(firstSegment, secondSegment)
  Gamma <- Gamma - mean(Gamma)
  
  #Valori perturbati nel periodo successivo, con un anno di buffer iniziali e finale.
  n_gamma <- (n_giorni)/365*s + s*2 + 1
  for(i in (s+1):n_gamma) {
    Gamma <- c(Gamma,(-sum(Gamma[(i-s+1):(i - 1)]) + rnorm(1,sd = sd_stag)))
  }
  
  #Sequenza di giorni
  x <- floor(rep(seq(1,365, length.out = s + 1)[1:s],n_giorni/365)) +
    rep(365*c(0:((n_giorni/365)-1)), each=s)
  x <- floor(c(seq(1,365, length.out = s + 1)[1:(s)] - 365,
               x,
               seq(1,365, length.out = s + 1)[1:(s+1)] + n_giorni))
  
  GammaFull <- Gamma[1]
  xFull <- x[1]
  for (j in 1:(length(Gamma)-1)) {
    xFull <- c(xFull,(x[j]+1):x[j+1])
    GammaFull <- c(GammaFull,seq(Gamma[j],Gamma[j+1],length.out = length(x[j]:x[j+1]))[-1])
  }
  
  fit = loess(GammaFull ~ xFull, span=span, degree=2)
  stag_average <- as.numeric(predict(fit, data.frame(xFull = 1:(n_giorni))))
  
  return(stag_average)
}


get.gamma <- function(seed, s, max1, max2) {
  set.seed(seed)
  a <- sample(2:(s-1),1)
  
  #Valori iniziali
  firstSegment <- sort(round(runif(a, min=0, max=max1),2), decreasing =FALSE)
  secondSegment <- sort(round(runif(s-a, min=0, max=max2),2),decreasing=TRUE)
  Gamma <- c(firstSegment, secondSegment)
  Gamma <- Gamma - mean(Gamma)

  return(Gamma)
  }


get.seasonality.Right <- function(seed, n_giorni, s, Gamma, sd_stag,
                                  span = 0.02) {
  
  set.seed(seed)
  #Valori perturbati nel periodo successivo, con un anno di buffer iniziali e finale.
  n_gamma <- (n_giorni)/365*s + s*2 + 1
  for(i in (s+1):n_gamma) {
    Gamma <- c(Gamma,(-sum(Gamma[(i-s+1):(i - 1)]) + rnorm(1,sd = sd_stag)))
  }
  
  #Sequenza di giorni
  x <- floor(rep(seq(1,365, length.out = s + 1)[1:s],n_giorni/365)) +
    rep(365*c(0:((n_giorni/365)-1)), each=s)
  x <- floor(c(seq(1,365, length.out = s + 1)[1:(s)] - 365,
               x,
               seq(1,365, length.out = s + 1)[1:(s+1)] + n_giorni))
  
  GammaFull <- Gamma[1]
  xFull <- x[1]
  for (j in 1:(length(Gamma)-1)) {
    xFull <- c(xFull,(x[j]+1):x[j+1])
    GammaFull <- c(GammaFull,seq(Gamma[j],Gamma[j+1],length.out = length(x[j]:x[j+1]))[-1])
  }
  
  fit = loess(GammaFull ~ xFull, span=span, degree=2)
  stag_average <- as.numeric(predict(fit, data.frame(xFull = 1:(n_giorni))))
  
  return(stag_average)
}



#Output lineare variabile in funzione dei processi.
lin.out.variabile <- function(input, n_giorni, daily_obs, coeff_lin, processi, unstable_vars, seed) {
  set.seed(seed)
  coeff_int <- runif(length(unstable_vars), min=-1, max=1)
  
  #Matrice dei coefficienti
  coefficienti <- matrix(rep(coeff_lin[2:(ncol(input)+1)], each=n_giorni),
                         nrow=ncol(input), ncol=n_giorni, byrow = TRUE)
  
  for (j in 1:nrow(processi)) {
    processi[j,] <- processi[j,] * coeff_int[j]
  }
  #Coefficienti con andamento stagionale.
  coefficienti[unstable_vars,] <- coefficienti[unstable_vars,] + processi
  
  output <- rep(NA, nrow(input))
  for (i in 1:n_giorni) {
    output[((i-1)*daily_obs + 1):(i*daily_obs)] <- as.matrix(input[((i-1)*daily_obs + 1):(i*daily_obs),]) %*% as.matrix(coefficienti[,i])
  }
  output = output + coeff_lin[1]
  
  return(list(output = output, coefficienti = coefficienti,interazioni = coeff_int))
}

#Senza l'interazione
lin.out.variabile2 <- function(input, n_giorni, daily_obs, coeff_lin, processi, unstable_vars, seed) {
  
  set.seed(seed)
  
  #Matrice dei coefficienti
  coefficienti <- matrix(rep(coeff_lin[2:(ncol(input)+1)], each=n_giorni),
                         nrow=ncol(input), ncol=n_giorni, byrow = TRUE)

  #Coefficienti con andamento stagionale.
  coefficienti[unstable_vars,] <- coefficienti[unstable_vars,] + processi
  
  output <- rep(NA, nrow(input))
  for (i in 1:n_giorni) {
    output[((i-1)*daily_obs + 1):(i*daily_obs)] <- as.matrix(input[((i-1)*daily_obs + 1):(i*daily_obs),]) %*% as.matrix(coefficienti[,i])
  }
  output = output + coeff_lin[1]
  
  return(list(output = output, coefficienti = coefficienti))
}

#Input con media variabile
input.STAG <- function(unstable_vars, processi, mu, Sigma, n_giorni, daily_obs) {
  
  mu_v <- mu
  input <- NULL
  for (j in 1:n_giorni) {
    mu_v[unstable_vars] <- processi[,j]
    input <- rbind(input,mvrnorm(n = daily_obs, mu = mu_v, Sigma = Sigma))
  }
  
  return(input)
}


#processo TAR
simulate.TAR <- function(n,sd_SETAR, r, d, par, orderAR, burn_in){
  dRegime <- rep(NA, n + burn_in)
  simTAR <-  rep(NA,n + burn_in)
  simTAR[1:orderAR] <- rnorm(orderAR, sd_SETAR)
  for (i in (orderAR+1):(n + burn_in)){
    dRegime[i] <- regime <- sum(simTAR[i-d] >= r) + 1 #Il regime a cui appartiene
    coeff <- par[[regime]]
    simTAR[i] <- t(as.matrix(c(1,simTAR[(i-orderAR):(i-1)])))%*%as.matrix(coeff) + rnorm(1,sd=sd_SETAR)
  }
  return(list(simTAR=ts(simTAR[-c(1:burn_in)]),dRegime=dRegime[-c(1:burn_in)]))
}
  


## DATA DRIFT
get.processi.Sudden.Drift <- function(nProc, n, position, seed, range) {
  
  set.seed(seed)
  shift <- round(runif(nProc,min=range[1], max=range[2]),2)
  processi <- matrix(0, nrow=nProc, ncol=n)
  processi[, position:n] <- matrix(rep(shift, each = (n - position + 1)),
                                   byrow=TRUE,nrow=nProc)
  return(processi)
}

get.processi.ARMA <- function(nProc, n, seed, coefAR, coefMA, d, sd) {
  
  set.seed(seed)
  
  processi <- matrix(0, nrow=nProc, ncol=n)
  for (i in 1:nProc) {
    processi[i,] <- noise.ARIMA(n, coefAR, coefMA, d, sd)
  }

  return(processi)
}

get.processi.Gradual.Drift <- function(nProc, n, position, seed, prob, range) {
  
  set.seed(seed)
  shift <- round(runif(nProc,min=range[1], max=range[2]),2)
  processi <- matrix(0, nrow=nProc, ncol=n)
  probs <- rep(0,n)
  probs[position[1]:position[2]] <- round(seq(from=prob[1], to=prob[2],
                                        length.out = position[2] - position[1] + 1),3)
  
  
  ## Medie samplate
  for(i in 1:nProc){
    means <- c(0,shift[i])
    for(j in 1:n){
      processi[i,j] <- sample(means,1,prob=c(1-probs[j],probs[j]))
    }
  }
  
  return(processi)
}

#Per mostrare gli andamenti dei coefficienti
coeff.plot <- function(coefficienti, n_giorni) {
  
  #coefficienti <- output$coefficienti
  coeff.df <- as.data.frame(t(coefficienti))
  coeff.df$Days <- 1:n_giorni
  
  coeff.for.plot <- coeff.df[,c("V1","Days")]
  colnames(coeff.for.plot) <- c("Coeff","Days")
  coeff.for.plot$Var <- "V1"
  
  if(nrow(coefficienti) >=2) {
    for ( i in 2:(ncol(coeff.df)-1) ) {
      tmp <- cbind(coeff.df[,c(i,ncol(coeff.df))],colnames(coeff.df)[i])
      colnames(tmp) <- c("Coeff","Days","Var")
      coeff.for.plot <- rbind(coeff.for.plot, tmp)
    }
  }
  
  coeff.for.plot$Var <- factor(coeff.for.plot$Var, levels = colnames(coeff.df)[-ncol(coeff.df)])
  
  plot <- ggplot(coeff.for.plot, aes(x = Days, y=Coeff, color = Var)) + geom_line() +
    labs(title="Coefficienti simulati - andamento giornaliero", x="giorni",y ="coefficienti") + 
    theme_bw() + theme(panel.border = element_rect(color = "black", linewidth = 0.3, fill = NA))
  
  return(plot)
}



## REAL CONCEPT DRIFT
get.processi.Gradual.Drift.Coeff <- function(vars,n, coeff1, coeff2,
                                 position, seed=seeds[job_index], prob) {
  
  set.seed(seed)
  #Nuovi coefficienti
  
  processi <- matrix(0, nrow=length(vars), ncol=n)
  probs <- rep(0,n)
  probs[position[1]:position[2]] <- round(seq(from=prob[1], to=prob[2],
                                              length.out = position[2] - position[1] + 1),3)
  
  
  #Utilizzo un sistema o l'altro.
  for (j in 1:n) {
    mod <- sample(1:2,1,prob=c(1-probs[j],probs[j]))
    if (mod == 1) processi[,j] <- coeff1
    if (mod == 2) processi[,j] <- coeff2
  }
  
  return(processi)
}

get.processi.Gradual.Drift.Coeff2 <- function(vars,n, coeff1, coeff2,
                                             position, seed=seeds[job_index], prob, init, last) {
  
  set.seed(seed)
  #Nuovi coefficienti
  
  processi <- matrix(0, nrow=length(vars), ncol=n)
  probs <- rep(init,n)
  probs[position[1]:position[2]] <- round(seq(from=prob[1], to=prob[2],
                                              length.out = position[2] - position[1] + 1),3)
  probs[(position[2] + 1):n] <- last 
  
  #Utilizzo un sistema o l'altro.
  for (j in 1:n) {
    mod <- sample(1:2,1,prob=c(1-probs[j],probs[j]))
    if (mod == 1) processi[,j] <- coeff1
    if (mod == 2) processi[,j] <- coeff2
  }
  
  return(processi)
}



get.processi.Incremental.Drift.Coeff <- function(vars,n, coeff1, coeff2,
                                             seed=seeds[job_index]) {
  
  set.seed(seed)
  
  processi <- matrix(0, nrow=length(vars), ncol=n)
  
  ## Medie samplate
  for(i in 1:length(vars)){
    processi[i,] <- seq(from = coeff1[i], to = coeff2[i], length.out = n)
  }
  
  return(processi)
  
}

get.processi.Recurrent.Drift.Coeff <- function(vars,n_giorni, eachDay, 
                                                 seed=seeds[job_index],
                                               coefAR, coefMA, sd_ARMA) {
  
  set.seed(seed)
  processi <- matrix(0, nrow=length(vars), ncol=n_giorni)
  
  ## Medie samplate
  for(i in 1:length(vars)){
    processi[i,] <- rep(noise.ARIMA(n = ceiling(n_giorni/eachDay), coefAR, coefMA, d=0, sd=sd_ARMA),
                        each = eachDay)[1:n_giorni]
  }
  
  return(processi)
}

get.processi.Sudden.Drift.Coeff <- function(vars, n, coeff1, coeff2,
                                            position) {

  processi <- matrix(rep(coeff1, each=n), nrow=length(vars), ncol=n, byrow=TRUE)
  
  for (i in 1:nrow(processi)){
    processi[i, position[1]:ncol(processi)] <- coeff2[i]
  }
  
  return(processi)
}



lin.out.conceptDrift <- function(input, n_giorni, daily_obs,
                                 coeff_lin, processi, unstable_vars) {
  
  coefficienti <- matrix(rep(coeff_lin[2:(ncol(input)+1)], each=n_giorni),
                         nrow=ncol(input), ncol=n_giorni, byrow = TRUE)
  
  #Coefficienti con andamento stagionale.
  coefficienti[unstable_vars,] <- processi
  
  output <- rep(NA, nrow(input))
  for (i in 1:n_giorni) {
    output[((i-1)*daily_obs + 1):(i*daily_obs)] <- as.matrix(input[((i-1)*daily_obs + 1):(i*daily_obs),]) %*% as.matrix(coefficienti[,i])
  }
  output = output + coeff_lin[1]
  
  return(list(output = output, coefficienti = coefficienti))
  
}



lin.out.conceptDrift2 <- function(input, n_giorni, daily_obs,
                                 coefficienti, beta0) {
  
  output <- rep(NA, nrow(input))
  for (i in 1:n_giorni) {
    output[((i-1)*daily_obs + 1):(i*daily_obs)] <- as.matrix(input[((i-1)*daily_obs + 1):(i*daily_obs),]) %*% as.matrix(coefficienti[,i])
  }
  output = output + beta0
  
  return(list(output = output, coefficienti = coefficienti))
  
}

int.out.conceptDrift2 <- function(input, n_giorni, daily_obs, interazioni,
                                coefficienti, n_int) {
  
  #Creo le variabili per l'interazione
  inputInt <- matrix(NA, nrow=nrow(input), ncol=n_int)
  for (j in 1:n_int){
    inputInt[,j] <- input[,interazioni$interazioni[j,]][,1] * input[,interazioni$interazioni[j,]][,2]
  }

  
  output <- rep(NA, nrow(input))
  for (i in 1:n_giorni) {
    output[((i-1)*daily_obs + 1):(i*daily_obs)] <- as.matrix(inputInt[((i-1)*daily_obs + 1):(i*daily_obs),]) %*% as.matrix(coefficienti[,i])
  }
  
  return(list(output = output, coefficienti = coefficienti))
  
}








