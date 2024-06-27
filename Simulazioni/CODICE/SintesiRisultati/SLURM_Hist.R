### Similarity ###

library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(patchwork)
library(feather)
# Inclinazione


## Calcolo delle pendenze per ogni simulazione.
setwd("D:/shared_directory_VM/simulazioniLight/dataset3_cubiche")
sim_number=100
#sim_number=50
max_dt = 1006
#max_dt = 2831

simulations <- list()
#Conterrà l'escursione del singolo modello in termini assoluti. (La pendenza della mediana).
#Conterrà la media del terzo quartile per ciascun modello.


theme_axis <- theme(
  axis.title.x = element_text(size = 14, family = "serif"),
  axis.text.x = element_text(size = 12,family = "serif"),
  axis.title.y = element_text(size = 14, family = "serif"),
  axis.text.y = element_text(size = 12,family = "serif"))

theme_axis_hist <- theme(
  axis.title.x = element_text(size = 14, family = "serif"),
  axis.text.x = element_text(size = 12,family = "serif"),
  axis.title.y = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y=element_blank()
)

theme_title <- theme(
  plot.title = element_text(
    hjust = 0.5,  # Center the title
    size = 15,    # Set the font size
    color = "black",  # Set the font color
    family = "serif",
    face ="italic",
    margin = margin(b = 10)  # Add margin at the bottom
  ))

col <- brewer.pal(9, "Set1")[1:4]

quantLines <- function(models.attr, results) {
  
  res.for.plot <- as.data.frame(t(results))
  res.for.plot$dt <- as.numeric(gsub("dt: ","",rownames(res.for.plot)))
  rownames(res.for.plot) <- NULL
  
  ## Togliamo 
  res.scale.for.plot <- res.for.plot
  
  ## Dividiamo gli MSE assoluti per renderli relativi.
  for (i in 1:nrow(models.attr)) {
    res.scale.for.plot[,i] <- res.scale.for.plot[,i]/as.numeric(models.attr$`MSE a t0`[i])   ## Dividiamo per MSE a t0
  }
  
  ## Calcoliamo i quantili
  qt <- matrix(NA, nrow = max_dt, ncol = 4)
  colnames(qt) <- c("q25","q50","q75","dt")
  rownames(qt) <- 1:max_dt
  
  ## Popoliamo la matrice con i quantili
  for (i in 1:max_dt) {
    qt[i,] <- c(quantile(as.numeric(res.scale.for.plot[i,-ncol(res.scale.for.plot)]), probs = c(0.25,0.5,0.75)),i)
  }
  return(as.data.frame(qt))
}

effective_number = 0

for (sim in 1:sim_number ){
  print(sim)
  modelli <- matrix(NA, nrow = 4, ncol = 4)
  rownames(modelli) <- c("Ridge","RF","GB","NN")
  colnames(modelli) <- c("escursione","escursione_rel","q75","q75_rel")
  
  if (file.exists(paste0("models/models.ridge.",sim))) {
    load(file = paste0("models/models.ridge.",sim))
    load(file =paste0("results/results.ridge.",sim))
         } else {effective_number <- c(effective_number,sim)}
  if (file.exists(paste0("models/models.rf.",sim))) {
    load(file = paste0("models/models.rf.",sim))
    load(file =paste0("results/results.rf.",sim))
  } else {effective_number <- c(effective_number,sim)}
  if (file.exists(paste0("models/models.gb.",sim))) {
    load(file = paste0("models/models.gb.",sim))
    load(file =paste0("results/results.gb.",sim))
  }  else {effective_number <- c(effective_number,sim)}
  
  if (file.exists(paste0("models/models.nn.",sim,".feather"))) {
  models.attr.nn <- read_feather(paste0("models/models.nn.",sim,".feather"))
  models.attr.nn$`__index_level_0__` <- NULL
  results.nn <- read_feather(paste0("results/results.nn.",sim,".feather"))
  results.nn$`__index_level_0__` <- NULL
  } else {effective_number <- c(effective_number,sim)}
  
  qtRidge <- quantLines(models.attr.ridge, results.ridge)  
  qtRf <- quantLines(models.attr.rf, results.rf)  
  qtGb <- quantLines(models.attr.gb, results.gb)
  qtNn <- quantLines(models.attr.nn, results.nn)
  
  #Escursione ed escursione relativa
  modelli["Ridge","escursione"] <- lm(qtRidge$q50 ~ qtRidge$dt)$coefficients[2]*max_dt
  modelli["RF","escursione"] <- lm(qtRf$q50 ~ qtRf$dt)$coefficients[2]*max_dt
  modelli["GB","escursione"] <- lm(qtGb$q50 ~ qtGb$dt)$coefficients[2]*max_dt
  modelli["NN","escursione"] <- lm(qtNn$q50 ~ qtNn$dt)$coefficients[2]*max_dt
  
  m <- mean(modelli[,"escursione"])
  modelli["Ridge","escursione_rel"] <- modelli["Ridge","escursione"] - m
  modelli["RF","escursione_rel"] <- modelli["RF","escursione"] - m
  modelli["GB","escursione_rel"] <- modelli["GB","escursione"] - m
  modelli["NN","escursione_rel"] <- modelli["NN","escursione"] - m
  
  #Livello medio e relativo del terzo quartile
  modelli["Ridge","q75"] <- mean(qtRidge$q75)
  modelli["RF","q75"] <- mean(qtRf$q75)
  modelli["GB","q75"] <- mean(qtGb$q75)
  modelli["NN","q75"] <- mean(qtNn$q75)
  m <- mean(modelli[,"q75"])
  
  modelli["Ridge","q75_rel"] <- modelli["Ridge","q75"] - m
  modelli["RF","q75_rel"] <- modelli["RF","q75"] - m
  modelli["GB","q75_rel"] <- modelli["GB","q75"] - m
  modelli["NN","q75_rel"] <- modelli["NN","q75"] - m
  
  modelli <- as.data.frame(modelli)
  simulations[[sim]] <- modelli
}

effective_number

#Escludo i risultati non pervenuti, solo un caso.
#simulations[[5]] <- NA
#simulations[[92]] <- NA

#save(simulations, file =paste0("synthesis/simuHist"))
load("synthesis/simuHist") #simulations

##Aggiungo la rete neurale

## Estraiamo tutto e calcoliamo l'inclinazione media
modelWiseSim <- as.data.frame(matrix(NA, nrow = sim_number*4, ncol = 5))
colnames(modelWiseSim) <- c("modello","escursione","escursione_rel","q75","q75_rel")
modelWiseSim[,"modello"] <- factor(rep(c("Ridge","RF","GB","NN"), each = sim_number),
                                   levels = c("Ridge","RF","GB","NN"))

for (j in 1:sim_number) {
  if (sum(is.na(simulations[[j]])) != 1) {
  #escursione di ogni modello nella simulazione
  modelWiseSim[j,"escursione"] <- simulations[[j]]["Ridge","escursione"]
  modelWiseSim[sim_number + j,"escursione"] <- simulations[[j]]["RF","escursione"]
  modelWiseSim[sim_number*2 + j,"escursione"] <- simulations[[j]]["GB","escursione"]
  modelWiseSim[sim_number*3 + j,"escursione"] <- simulations[[j]]["NN","escursione"]
  
  #escursione relativa
  modelWiseSim[j,"escursione_rel"] <- simulations[[j]]["Ridge","escursione_rel"]
  modelWiseSim[sim_number + j,"escursione_rel"] <- simulations[[j]]["RF","escursione_rel"]
  modelWiseSim[sim_number*2 + j,"escursione_rel"] <- simulations[[j]]["GB","escursione_rel"]
  modelWiseSim[sim_number*3 + j,"escursione_rel"] <- simulations[[j]]["NN","escursione_rel"]
  
  #livello medio del terzo quartile
  modelWiseSim[j,"q75"] <- simulations[[j]]["Ridge","q75"]
  modelWiseSim[sim_number + j,"q75"] <- simulations[[j]]["RF","q75"]
  modelWiseSim[sim_number*2 + j,"q75"] <- simulations[[j]]["GB","q75"]
  modelWiseSim[sim_number*3 + j,"q75"] <- simulations[[j]]["NN","q75"]
  
  #livello medio del terzo quartile relativo
  modelWiseSim[j,"q75_rel"] <- simulations[[j]]["Ridge","q75_rel"]
  modelWiseSim[sim_number + j,"q75_rel"] <- simulations[[j]]["RF","q75_rel"]
  modelWiseSim[sim_number*2 + j,"q75_rel"] <- simulations[[j]]["GB","q75_rel"]
  modelWiseSim[sim_number*3 + j,"q75_rel"] <- simulations[[j]]["NN","q75_rel"]
  } else {print(paste0("Manca sim:",j))}
}


col <- brewer.pal(9, "Set1")[1:4]
modelWiseSim <- modelWiseSim %>% group_by(modello) %>% mutate(mEsc = mean(escursione, na.rm=TRUE),
                                                              mEscRel = mean(escursione_rel, na.rm=TRUE),
                                                              mq75 = mean(q75, na.rm=TRUE),
                                                              mq75Rel = mean(q75_rel, na.rm=TRUE))

modelWiseSim <- modelWiseSim %>% group_by(modello) %>% mutate(medianEsc = median(escursione, na.rm=TRUE))

#Escursioni
meanVar <- unique(modelWiseSim[, c("mEsc", "modello")])
medianVar <- unique(modelWiseSim[, c("medianEsc", "modello")])

escHistBetween <- ggplot(modelWiseSim, aes(x=escursione))+
  geom_histogram(color="black", fill="white", aes(y = ..density..))+
  facet_grid(modello ~ ., switch = "y") +
  theme_bw() + theme_title + theme_axis_hist +
  geom_density(aes(fill = modello), alpha=.4) +
  scale_fill_manual(values = c("Ridge" = col[1], "RF" = col[2], "GB" = col[3], "NN" = col[4])) + 
   theme(
     legend.position = "none",  # Remove legend
     panel.grid = element_blank(),  # Remove background grid of histograms
     strip.background = element_rect(fill = "white", color = "black"),  # Remove the boxes around the facet labels
#     strip.text.y = element_text(hjust = 0)  # Adjust facet label position
   ) + 
  geom_vline(aes(xintercept = mEsc, group = modello), colour = 'red', linewidth = 0.8,
             linetype = "solid") + 
  xlab("variazione e_rel") +
  geom_label(data = meanVar, 
             aes(x = mEsc, y = 0, label = round(mEsc,2)), 
             vjust = -0.5, size = 4, color = "red", fill = "white")
escHistBetween


escHistBetween <- ggplot(modelWiseSim, aes(x=escursione))+
  geom_histogram(color="black", fill="white", aes(y = ..density..))+
  facet_grid(modello ~ ., switch = "y") +
  theme_bw() + theme_title + theme_axis_hist +
  geom_density(aes(fill = modello), alpha=.4) +
  scale_fill_manual(values = c("Ridge" = col[1], "RF" = col[2], "GB" = col[3], "NN" = col[4])) + 
  theme(
    legend.position = "none",  # Remove legend
    panel.grid = element_blank(),  # Remove background grid of histograms
    strip.background = element_rect(fill = "white", color = "black"),  # Remove the boxes around the facet labels
    #     strip.text.y = element_text(hjust = 0)  # Adjust facet label position
  ) + 
  geom_vline(aes(xintercept = mEsc, group = modello), colour = 'red', linewidth = 0.8,
             linetype = "solid") + 
  geom_vline(aes(xintercept = medianEsc, group = modello), colour = 'orange', linewidth = 0.8,
             linetype = "solid") + 
  xlab("variazione e_rel") +
  geom_label(data = meanVar, 
             aes(x = mEsc, y = 0, label = round(mEsc,2)), 
             vjust = -0.5, size = 4, color = "red", fill = "white") + 
  geom_label(data = medianVar, 
             aes(x = medianEsc, y = 2, label = round(medianEsc,2)), 
             vjust = -0.5, size = 4, color = "orange", fill = "white")
escHistBetween





hist(modelWiseSim$escursione[modelWiseSim$modello == "Ridge"])
hist(modelWiseSim$escursione[modelWiseSim$modello == "RF"])
hist(modelWiseSim$escursione[modelWiseSim$modello == "GB"])

escHistWithin <- ggplot(modelWiseSim, aes(x=escursione_rel))+
  geom_histogram(color="black", fill="white", aes(y = ..density..))+
  facet_grid(modello ~ .) +
  theme_bw() + theme_axis + theme_title + theme_axis_hist +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("escursione rel") +
  geom_vline(aes(xintercept = mEscRel, group = modello), colour = 'blue', linewidth = 0.8,
             linetype = "dashed")
escHistWithin


escPlot <- escHistBetween + escHistWithin + 
  plot_layout(guides="collect", ncol=2, nrow=1, byrow=TRUE) &
  theme(legend.box.background = element_rect(color = "black", linewidth = 1)) +
  theme(legend.position="bottom")
escPlot


#qt75
means <- unique(modelWiseSim[, c("mq75", "modello")])
means$mq75Perc <- paste0(round(means$mq75 - 1,4)*100,"%")

q75HistBetween <- ggplot(modelWiseSim, aes(x=q75))+
  geom_histogram(color="black", fill="white", aes(y = ..density..))+
  facet_grid(modello ~ ., switch = "y") +
  theme_bw() + theme_title + theme_axis_hist +
  geom_density(aes(fill = modello), alpha=.4) +
  scale_fill_manual(values = c("Ridge" = col[1], "RF" = col[2], "GB" = col[3], "NN" = col[4])) + 
  theme(
    legend.position = "none",  # Remove legend
    panel.grid = element_blank(),  # Remove background grid of histograms
    strip.background = element_rect(fill = "white", color = "black"),  # Remove the boxes around the facet labels
    #     strip.text.y = element_text(hjust = 0)  # Adjust facet label position
  ) + xlab("75esimo percentile") + 
  geom_vline(aes(xintercept = mq75, group = modello), colour = 'red', linewidth = 0.8,
             linetype = "solid") +
  geom_label(data = means, 
             aes(x = mq75, y = 0, label = mq75Perc), 
             vjust = -0.5, size = 4, color = "red", fill = "white")
q75HistBetween

q75HistWithin <- ggplot(modelWiseSim, aes(x=q75_rel))+
  geom_histogram(color="black", fill="white", aes(y = ..density..))+
  facet_grid(modello ~ .) +
  theme_bw() + theme_axis + theme_title + theme_axis_hist +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("75esimo perc. rel") +
  geom_vline(aes(xintercept = mq75Rel, group = modello), colour = 'blue', linewidth = 0.8,
             linetype = "dashed")
q75HistWithin

finale <- escHistBetween + escHistWithin + q75HistBetween + q75HistWithin +
  plot_layout(guides="collect", ncol=2, nrow=2, byrow=TRUE) &
  theme(legend.box.background = element_rect(color = "black", linewidth = 1))
finale

ggsave(filename="synthesis/dataset17_IncrementalCD_Interazioni_5obs_VarE_rel.pdf", plot=escHistBetween, width=6, height=4, device="pdf")
ggsave(filename="synthesis/dataset20_STAGaBitMoreLessWithEverything_hist75th.pdf", plot=q75HistBetween, width=6, height=4, device="pdf")







## Differenze tra ridge e RF, e test?
setwd("D:/shared_directory_VM/simulazioniLight/dataset4.3_2obs")
RFDiff <- list()

for (sim in 1:sim_number ){
  print(sim)
  modelli <- matrix(NA, nrow = 1, ncol = 3)
  rownames(modelli) <- c("RF")
  colnames(modelli) <- c("Ridge","GB","NN")
  
  if (file.exists(paste0("models/models.ridge.",sim))) {
    load(file = paste0("models/models.ridge.",sim))
    load(file =paste0("results/results.ridge.",sim))
  } else {effective_number <- c(effective_number,sim)}
  if (file.exists(paste0("models/models.rf.",sim))) {
    load(file = paste0("models/models.rf.",sim))
    load(file =paste0("results/results.rf.",sim))
  } else {effective_number <- c(effective_number,sim)}
  if (file.exists(paste0("models/models.gb.",sim))) {
    load(file = paste0("models/models.gb.",sim))
    load(file =paste0("results/results.gb.",sim))
  }  else {effective_number <- c(effective_number,sim)}
  
  if (file.exists(paste0("models/models.nn.",sim,".feather"))) {
    models.attr.nn <- read_feather(paste0("models/models.nn.",sim,".feather"))
    models.attr.nn$`__index_level_0__` <- NULL
    results.nn <- read_feather(paste0("results/results.nn.",sim,".feather"))
    results.nn$`__index_level_0__` <- NULL
  } else {effective_number <- c(effective_number,sim)}
  
  qtRidge <- quantLines(models.attr.ridge, results.ridge)  
  qtRf <- quantLines(models.attr.rf, results.rf)  
  qtGb <- quantLines(models.attr.gb, results.gb)
  qtNn <- quantLines(models.attr.nn, results.nn)
  
  #Livello medio e relativo del terzo quartile
  q75RF <- mean(qtRf$q75)
  modelli["RF","Ridge"] <- q75RF - mean(qtRidge$q75)
  modelli["RF","GB"] <- q75RF - mean(qtGb$q75)
  modelli["RF","NN"] <- q75RF - mean(qtNn$q75)

  RFDiff[[sim]] <- modelli
}
save(RFDiff, file =paste0("synthesis/simuRFDiff"))
load("synthesis/simuRFDiff")
RFDiff1 <- as.data.frame(matrix(unlist(RFDiff), byrow =TRUE, ncol=3))
colnames(RFDiff1) <- colnames(RFDiff[[1]])

sum(RFDiff1[,"NN"] < 0)

meanDiff <- colMeans(RFDiff1)

q75RFvsRidge <- ggplot(RFDiff1, aes(x=NN))+
  geom_histogram(color="black", fill="white", aes(y = ..density..))+
  theme_bw() + theme_title + theme_axis_hist + 
  xlab("RF vs NN: 75esimo percentile") + 
  geom_vline(xintercept = meanDiff["NN"], colour = 'red', linewidth = 0.8,
             linetype = "solid") + 
  geom_label(aes(x = meanDiff["NN"], y = 0, label = paste0(round(meanDiff["NN"]*100,2),"%")), 
            vjust = -0.5, size = 4, color = "red", fill = "white")
q75RFvsRidge

ggsave(filename="synthesis/dataset4.3_2obs_RFvsNN.pdf", plot=q75RFvsRidge, width=6, height=2, device="pdf")








## TEST PER VALUTARE SIGNIFICATIVITA' DELLE DIFFERENZE ##
setwd("D:/shared_directory_VM/simulazioniLight/dataset1_midCorr")
load("synthesis/simuRFDiff")
RFDiff1 <- as.data.frame(matrix(unlist(RFDiff), byrow =TRUE, ncol=3))
colnames(RFDiff1) <- colnames(RFDiff[[1]])

xBar = mean(RFDiff1$Ridge)
xBar
n = length(RFDiff1$Ridge)
S2 = sum((RFDiff1$Ridge - xBar)^2)/(n - 1)
S2
Z = xBar/sqrt(S2/n)
Z
pnorm(Z, lower.tail = FALSE)




##Escursioni, caso per caso.
meanVar <- unique(modelWiseSim[, c("mEsc", "modello")])
head(modelWiseSim)

escHistRidge <- ggplot(modelWiseSim[modelWiseSim$modello == "Ridge",], aes(x=escursione))+
  geom_histogram(color="black", fill="white", aes(y = ..density..)) +
  theme_bw() + theme_title + theme_axis_hist +
  geom_density(fill = col[1], alpha=.4) + 
  theme(
    legend.position = "none",  # Remove legend
    panel.grid = element_blank(),  # Remove background grid of histograms
    strip.background = element_rect(fill = "white", color = "black"),  # Remove the boxes around the facet labels
    #     strip.text.y = element_text(hjust = 0)  # Adjust facet label position
  ) + 
  geom_vline(aes(xintercept = mEsc), colour = 'red', linewidth = 0.8,
             linetype = "solid") + 
  xlab("variazione e_rel")
escHistRidge


escHistRF <- ggplot(modelWiseSim[modelWiseSim$modello == "RF",], aes(x=escursione))+
  geom_histogram(color="black", fill="white", aes(y = ..density..)) +
  theme_bw() + theme_title + theme_axis_hist +
  geom_density(fill = col[2], alpha=.4) + 
  theme(
    legend.position = "none",  # Remove legend
    panel.grid = element_blank(),  # Remove background grid of histograms
    strip.background = element_rect(fill = "white", color = "black"),  # Remove the boxes around the facet labels
    #     strip.text.y = element_text(hjust = 0)  # Adjust facet label position
  ) + 
  geom_vline(aes(xintercept = mEsc), colour = 'red', linewidth = 0.8,
             linetype = "solid") + 
  xlab("variazione e_rel")
escHistRF


escHistGB <- ggplot(modelWiseSim[modelWiseSim$modello == "GB",], aes(x=escursione))+
  geom_histogram(color="black", fill="white", aes(y = ..density..)) +
  theme_bw() + theme_title + theme_axis_hist +
  geom_density(fill = col[3], alpha=.4) + 
  theme(
    legend.position = "none",  # Remove legend
    panel.grid = element_blank(),  # Remove background grid of histograms
    strip.background = element_rect(fill = "white", color = "black"),  # Remove the boxes around the facet labels
    #     strip.text.y = element_text(hjust = 0)  # Adjust facet label position
  ) + 
  geom_vline(aes(xintercept = mEsc), colour = 'red', linewidth = 0.8,
             linetype = "solid") + 
  xlab("variazione e_rel")
escHistGB


escHistNN <- ggplot(modelWiseSim[modelWiseSim$modello == "NN",], aes(x=escursione))+
  geom_histogram(color="black", fill="white", aes(y = ..density..)) +
  theme_bw() + theme_title + theme_axis_hist +
  geom_density(fill = col[4], alpha=.4) + 
  theme(
    legend.position = "none",  # Remove legend
    panel.grid = element_blank(),  # Remove background grid of histograms
    strip.background = element_rect(fill = "white", color = "black"),  # Remove the boxes around the facet labels
    #     strip.text.y = element_text(hjust = 0)  # Adjust facet label position
  ) + 
  geom_vline(aes(xintercept = mEsc), colour = 'red', linewidth = 0.8,
             linetype = "solid") + 
  xlab("variazione e_rel")
escHistNN


finale <- escHistRidge + escHistRF + escHistGB + escHistNN +
  plot_layout(guides="collect", ncol=1, nrow=4, byrow=TRUE) &
  theme(legend.box.background = element_rect(color = "black", linewidth = 1))
finale





