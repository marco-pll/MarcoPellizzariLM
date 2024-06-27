### MSE FUTURI DEI MODELLI ###

library(RColorBrewer)
library(ggplot2)
library(patchwork)
library(feather)

setwd("D:/shared_directory_VM/simulazioniLight/dataset17_IncrementalCD")

#max_dt = 2831 se simulazione con stagionalità
max_dt=1006
#Prendo la prima simulazione.

## Prendo i quantili degli errori assoluti.
quantile.MSE <- function(results) {
  
  res.for.plot <- as.data.frame(t(results))
  res.for.plot$dt <- as.numeric(gsub("dt: ","",rownames(res.for.plot)))
  rownames(res.for.plot) <- NULL
  
  ## Il dataframe su cui facciamo le modifiche
  res.scale.for.plot <- res.for.plot
  
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

theme_title_median <- theme(
  plot.title = element_text(
    hjust = 0.5,  # Center the title
    size = 15,    # Set the font size
    color = "black",  # Set the font color
    family = "serif",
    face ="italic",
    margin = margin(b = 10)  # Add margin at the bottom
  ))

theme_legend_median <- theme(legend.title = element_blank(),
                             panel.border = element_rect(colour = "black", fill=NA),
                             legend.box.margin = margin(t = 1.5, l = 1.5,b =1,r=1.5),
                             legend.box.background = element_rect(colour = "black"),
                             legend.key.width = unit(0.5,"cm"),
                             legend.text = element_text(family ="serif", size =15))
theme_axis_median <- theme(
  axis.title.x = element_text(size = 14, family = "serif"),
  axis.text.x = element_text(size = 12,family = "serif"),
  axis.title.y = element_text(size = 14, family = "serif"),
  axis.text.y = element_text(size = 12,family = "serif"))


sim_number = 100
#sim_number=50
ridge = list(first = c(), median = c(), last = c()) #Conterrà gli andamenti primi e ultimi.
rf = list(first = c(), median = c(), last = c())
gb = list(first = c(), median = c(), last = c())
nn = list(first = c(), median = c(), last = c())

effective_number = c()

for (sim in 1:sim_number){
  print(sim)
  
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
  
  quantRidge <- quantile.MSE(results.ridge)
  quantRF <- quantile.MSE(results.rf)
  quantGB <- quantile.MSE(results.gb)
  quantNN <- quantile.MSE(results.nn)

  ridge$median <- rbind(ridge$median,quantRidge[,"q50"])
  rf$median <- rbind(rf$median,quantRF[,"q50"])
  gb$median <- rbind(gb$median,quantGB[,"q50"])  
  nn$median <- rbind(nn$median,quantNN[,"q50"])

  ridge$first <- rbind(ridge$first,quantRidge[,"q25"])
  rf$first <- rbind(rf$first,quantRF[,"q25"])
  gb$first <- rbind(gb$first,quantGB[,"q25"])  
  nn$first <- rbind(nn$first,quantNN[,"q25"])

  ridge$last <- rbind(ridge$last,quantRidge[,"q75"])
  rf$last <- rbind(rf$last,quantRF[,"q75"])
  gb$last <- rbind(gb$last,quantGB[,"q75"])  
  nn$last <- rbind(nn$last,quantNN[,"q75"])
  
}

col <- brewer.pal(9, "Set1")[1:4]



###### PLOT MSE MEDIANI ######
lw = 0.7
MSEqt <- list(ridge = ridge, rf = rf, gb = gb, nn = nn)
#save(MSEqt, file =paste0("synthesis/MSEqt"))
#load("synthesis/MSEqt")

#ridge = MSEqt$ridge; rf = MSEqt$rf; gb = MSEqt$gb; nn = MSEqt$nn


load("synthesis/mean_R2_ridge");load("synthesis/mean_R2_rf");load("synthesis/mean_R2_gb");
load("synthesis/mean_R2_nn")

library(RColorBrewer)
library(ggplot2)
library(patchwork)

col <- brewer.pal(9, "Set1")[1:4]

theme_title_median <- theme(
  plot.title = element_text(
    hjust = 0.5,  # Center the title
    size = 15,    # Set the font size
    color = "black",  # Set the font color
    family = "serif",
    face ="italic",
    margin = margin(b = 10)  # Add margin at the bottom
  ))

theme_legend_median <- theme(legend.title = element_blank(),
                             panel.border = element_rect(colour = "black", fill=NA),
                             legend.box.margin = margin(t = 1.5, l = 1.5,b =1,r=1.5),
                             legend.box.background = element_rect(colour = "black"),
                             legend.key.width = unit(0.5,"cm"),
                             legend.text = element_text(family ="serif", size =12))
theme_axis_median <- theme(
  axis.title.x = element_text(size = 14, family = "serif"),
  axis.text.x = element_text(size = 12,family = "serif"),
  axis.title.y = element_text(size = 14, family = "serif"),
  axis.text.y = element_text(size = 12,family = "serif"))


qtRidgeMedianMinMax <- as.data.frame(t(as.matrix(apply(MSEqt$ridge$median,2,function(x) quantile(x,probs=c(0.25,0.75))))))
colnames(qtRidgeMedianMinMax) <- c("q25","q75")
qtRidgeMedianMinMax$dT <- 1:nrow(qtRidgeMedianMinMax)

qtRFMedianMinMax <- as.data.frame(t(as.matrix(apply(MSEqt$rf$median,2,function(x)quantile(x,probs=c(0.25,0.75))))))
colnames(qtRFMedianMinMax) <- c("q25","q75")
qtRFMedianMinMax$dT <- 1:nrow(qtRFMedianMinMax)

qtGBMedianMinMax <- as.data.frame(t(as.matrix(apply(MSEqt$gb$median,2,function(x)quantile(x,probs=c(0.25,0.75))))))
colnames(qtGBMedianMinMax) <- c("q25","q75")
qtGBMedianMinMax$dT <- 1:nrow(qtGBMedianMinMax)

qtNNMedianMinMax <- as.data.frame(t(as.matrix(apply(MSEqt$nn$median,2,function(x)quantile(x,probs=c(0.25,0.75))))))
colnames(qtNNMedianMinMax) <- c("q25","q75")
qtNNMedianMinMax$dT <- 1:nrow(qtNNMedianMinMax)


RFvsRIDGEqtMSEPlot <- ggplot(qtRidgeMedianMinMax, aes(x = dT)) + 
  geom_line(stat = "identity", aes(y = q25, color = "Ridge"), linewidth = lw) + 
  geom_line(stat = "identity", aes(y = q75, color = "Ridge"), linewidth = lw) + 
  geom_line(data = qtRFMedianMinMax, aes(x=dT, y = q25, color="RF"), linewidth = lw) +
  geom_line(data = qtRFMedianMinMax, aes(x=dT, y = q75, color="RF"), linewidth = lw) + 
  scale_color_manual(
    values = c("Ridge" = col[1], "RF" = col[2]),
    labels = c(paste0("Ridge ","(",round(mean(mean_R2_ridge),2),")"),
               paste0("RF ","(",round(mean(mean_R2_rf),2),")")),
    breaks = c("Ridge","RF")) + theme_bw() + theme_legend_median +
  theme_title_median + theme_axis_median +
  theme(legend.position = c(0.25,0.88), legend.direction = "horizontal") +
  ylab("MSE") +
  guides(color = guide_legend(title = "Plot1"))
RFvsRIDGEqtMSEPlot


GBvsRIDGEqtMSEPlot <- ggplot(qtRidgeMedianMinMax, aes(x = dT)) + 
  geom_line(stat = "identity", aes(y = q25, color = "Ridge"), linewidth = lw) + 
  geom_line(stat = "identity", aes(y = q75, color = "Ridge"), linewidth = lw) + 
  geom_line(data = qtGBMedianMinMax, aes(x=dT, y = q25, color="GB"), linewidth = lw) +
  geom_line(data = qtGBMedianMinMax, aes(x=dT, y = q75, color="GB"), linewidth = lw) + 
  ylab("") + 
  scale_color_manual(
    values = c("Ridge" = col[1], "GB" = col[3]),
    labels = c(paste0("Ridge ","(",round(mean(mean_R2_ridge),2),")"),
               paste0("GB ","(",round(mean(mean_R2_gb),2),")")),
    breaks = c("Ridge","GB")) + theme_bw() + theme_legend_median +
  theme_title_median + theme_axis_median +
  theme(legend.position = c(0.25,0.88), legend.direction = "horizontal") +
  ylab("MSE") + 
  guides(color = guide_legend(title = "Plot2"))
GBvsRIDGEqtMSEPlot

NNvsRIDGEqtMSEPlot <- ggplot(qtRidgeMedianMinMax, aes(x = dT)) + 
  geom_line(stat = "identity", aes(y = q25, color = "Ridge"), linewidth = lw) + 
  geom_line(stat = "identity", aes(y = q75, color = "Ridge"), linewidth = lw) + 
  geom_line(data = qtNNMedianMinMax, aes(x=dT, y = q25, color="NN"), linewidth = lw) +
  geom_line(data = qtNNMedianMinMax, aes(x=dT, y = q75, color="NN"), linewidth = lw) + 
  scale_color_manual(
    values = c("Ridge" = col[1], "NN" = col[4]),
    labels = c(paste0("Ridge ","(",round(mean(mean_R2_ridge),2),")"),
               paste0("NN ","(",round(mean(mean_R2_nn),2),")")),
    breaks = c("Ridge","NN")) + theme_bw() + theme_legend_median +
  theme_title_median + theme_axis_median +
  theme(legend.position = c(0.25,0.88), legend.direction = "horizontal") +
  ylab("MSE") + 
  guides(color = guide_legend(title = "Plot3"))
NNvsRIDGEqtMSEPlot



MSEmedianPlot <- wrap_elements(RFvsRIDGEqtMSEPlot) / wrap_elements(GBvsRIDGEqtMSEPlot) / wrap_elements(NNvsRIDGEqtMSEPlot) 
ggsave(filename="dataset17_MSEMedianPlot.pdf", plot = MSEmedianPlot,
       device ="pdf", width=6, height = 10)
ggsave(filename="dataset17_RFvsGBqtMSEMedianPlot.pdf", plot = RFvsGBqtMSEPlot,
       device ="pdf", width=6, height = 3.3)


### ANDAMENTI DEL TERZO QUARTILE DELL'MSE ###
qtRFMedianMinMax <- as.data.frame(t(as.matrix(apply(MSEqt$rf$last,2,function(x) quantile(x,probs=c(0.25,0.75))))))
colnames(qtRFMedianMinMax) <- c("q25","q75")
qtRFMedianMinMax$dT <- 1:nrow(qtRFMedianMinMax)

qtGBMedianMinMax <- as.data.frame(t(as.matrix(apply(MSEqt$gb$last,2,function(x)quantile(x,probs=c(0.25,0.75))))))
colnames(qtGBMedianMinMax) <- c("q25","q75")
qtGBMedianMinMax$dT <- 1:nrow(qtGBMedianMinMax)

qtNNMedianMinMax <- as.data.frame(t(as.matrix(apply(MSEqt$nn$last,2,function(x)quantile(x,probs=c(0.25,0.75))))))
colnames(qtNNMedianMinMax) <- c("q25","q75")
qtNNMedianMinMax$dT <- 1:nrow(qtNNMedianMinMax)

qtRidgeMedianMinMax <- as.data.frame(t(as.matrix(apply(MSEqt$ridge$last,2,function(x)quantile(x,probs=c(0.25,0.75))))))
colnames(qtRidgeMedianMinMax) <- c("q25","q75")
qtRidgeMedianMinMax$dT <- 1:nrow(qtRidgeMedianMinMax)


RIDGEvsGBqtMSEPlot <- ggplot(qtGBMedianMinMax, aes(x = dT)) + 
  geom_line(stat = "identity", aes(y = q25, color = "GB"), linewidth = lw) + 
  geom_line(stat = "identity", aes(y = q75, color = "GB"), linewidth = lw) + 
  geom_line(data = qtRidgeMedianMinMax, aes(x=dT, y = q25, color="Ridge"), linewidth = lw) +
  geom_line(data = qtRidgeMedianMinMax, aes(x=dT, y = q75, color="Ridge"), linewidth = lw) + 
  scale_color_manual(
    values = c("GB" = col[3], "Ridge" = col[1]),
    labels = c(paste0("GB","(",round(mean(mean_R2_gb),2),")"),
               paste0("Ridge ","(",round(mean(mean_R2_ridge),2),")")),
    breaks = c("GB","Ridge")) + theme_bw() + theme_legend_median +
  theme_title_median + theme_axis_median +
  theme(legend.position = c(0.25,0.88), legend.direction = "horizontal") +
  ylab("MSE") +
  guides(color = guide_legend(title = "Plot1"))
RIDGEvsGBqtMSEPlot


RFvsGBqtMSEPlot <- ggplot(qtGBMedianMinMax, aes(x = dT)) + 
  geom_line(stat = "identity", aes(y = q25, color = "GB"), linewidth = lw) + 
  geom_line(stat = "identity", aes(y = q75, color = "GB"), linewidth = lw) + 
  geom_line(data = qtRFMedianMinMax, aes(x=dT, y = q25, color="RF"), linewidth = lw) +
  geom_line(data = qtRFMedianMinMax, aes(x=dT, y = q75, color="RF"), linewidth = lw) + 
  scale_color_manual(
    values = c("GB" = col[3], "RF" = col[2]),
    labels = c(paste0("GB","(",round(mean(mean_R2_gb),2),")"),
               paste0("RF ","(",round(mean(mean_R2_rf),2),")")),
    breaks = c("GB","RF")) + theme_bw() + theme_legend_median +
  theme_title_median + theme_axis_median +
  theme(legend.position = c(0.25,0.88), legend.direction = "horizontal") +
  ylab("MSE") +
  guides(color = guide_legend(title = "Plot1"))
RFvsGBqtMSEPlot

NNvsGBqtMSEPlot <- ggplot(qtGBMedianMinMax, aes(x = dT)) + 
  geom_line(stat = "identity", aes(y = q25, color = "GB"), linewidth = lw) + 
  geom_line(stat = "identity", aes(y = q75, color = "GB"), linewidth = lw) + 
  geom_line(data = qtNNMedianMinMax, aes(x=dT, y = q25, color="NN"), linewidth = lw) +
  geom_line(data = qtNNMedianMinMax, aes(x=dT, y = q75, color="NN"), linewidth = lw) + 
  scale_color_manual(
    values = c("GB" = col[3], "NN" = col[4]),
    labels = c(paste0("GB","(",round(mean(mean_R2_gb),2),")"),
               paste0("NN ","(",round(mean(mean_R2_nn),2),")")),
    breaks = c("GB","NN")) + theme_bw() + theme_legend_median +
  theme_title_median + theme_axis_median +
  theme(legend.position = c(0.25,0.88), legend.direction = "horizontal") +
  ylab("MSE") +
  guides(color = guide_legend(title = "Plot1"))
NNvsGBqtMSEPlot



MSEmedianPlot <- wrap_elements(RIDGEvsGBqtMSEPlot) / wrap_elements(RFvsGBqtMSEPlot) / wrap_elements(NNvsGBqtMSEPlot) 
ggsave(filename="dataset17_MSEMedianPlot.pdf", plot = MSEmedianPlot,
       device ="pdf", width=6, height = 10)
ggsave(filename="dataset17_RFvsGBqtMSEMedianPlot.pdf", plot = RFvsGBqtMSEPlot,
       device ="pdf", width=6, height = 3.3)



## Grafico three-way.

RFvsRIDGEvsGBqtMSEPlot <- ggplot(qtRidgeMedianMinMax, aes(x = dT)) + 
  geom_line(stat = "identity", aes(y = q25, color = "Ridge"), linewidth = lw) + 
  geom_line(stat = "identity", aes(y = q75, color = "Ridge"), linewidth = lw) + 
  geom_line(data = qtGBMedianMinMax, aes(x=dT, y = q25, color="GB"), linewidth = lw) +
  geom_line(data = qtGBMedianMinMax, aes(x=dT, y = q75, color="GB"), linewidth = lw) + 
  geom_line(data = qtNNMedianMinMax, aes(x=dT, y = q25, color="NN"), linewidth = lw) +
  geom_line(data = qtNNMedianMinMax, aes(x=dT, y = q75, color="NN"), linewidth = lw) + 
  scale_color_manual(
    values = c("Ridge" = col[1],"GB" = col[3], "NN" = col[4]),
    labels = c(paste0("Ridge","(",round(mean(mean_R2_ridge),2),")"),
               paste0("GB ","(",round(mean(mean_R2_gb),2),")"),
               paste0("NN ","(",round(mean(mean_R2_nn),2),")")),
    breaks = c("Ridge","GB", "NN")) + theme_bw() + theme_legend_median +
  theme_title_median + theme_axis_median +
  theme(legend.position = c(0.35,0.88), legend.direction = "horizontal") +
  ylab("MSE") +
  guides(color = guide_legend(title = "Plot1"))
RFvsRIDGEvsGBqtMSEPlot


## Plot del terzo quartile, MSE, per una singola replicazione.
library(RColorBrewer)
library(ggplot2)
library(patchwork)
library(feather)

setwd("D:/shared_directory_VM/simulazioniLight/dataset8_AR_moreARmid")
sim = 3
#Prendo la prima simulazione.

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


## Prendo i quantili degli errori assoluti.
quantile.MSE <- function(results) {
  
  res.for.plot <- as.data.frame(t(results))
  res.for.plot$dt <- as.numeric(gsub("dt: ","",rownames(res.for.plot)))
  rownames(res.for.plot) <- NULL
  
  ## Il dataframe su cui facciamo le modifiche
  res.scale.for.plot <- res.for.plot
  
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

ridge.qMSE <- as.data.frame(quantile.MSE(results.ridge))
rf.qMSE <- as.data.frame(quantile.MSE(results.rf))
gb.qMSE <- as.data.frame(quantile.MSE(results.gb))
nn.qMSE <- as.data.frame(quantile.MSE(results.nn))


thirdDF <- data.frame(ridge = ridge.qMSE$q75,
                          rf = rf.qMSE$q75,
                          gb = gb.qMSE$q75,
                          nn = nn.qMSE$q75,
                          dT = 1:length(ridge.qMSE$q75))


head(thirdDF)
lw = 0.7

col <- brewer.pal(9, "Set1")[1:4]

quantile.relMSE <- function(models.attr, results) {
  
  ## Dataframed
  res.for.plot <- as.data.frame(t(results))
  res.for.plot$dt <- as.numeric(gsub("dt: ","",rownames(res.for.plot)))
  rownames(res.for.plot) <- NULL
  
  ## Togliamo 
  res.scale.for.plot <- res.for.plot
  
  ## Dividiamo gli MSE assoluti per renderli relativi.
  for (i in 1:nrow(models.attr)) {
    res.scale.for.plot[,i] <- res.scale.for.plot[,i]/models.attr[i,"MSE a t0"]   ## Dividiamo per MSE a t0
  }
  
  max_dt = ncol(results)
  ## Calcoliamo i quantili
  qt <- matrix(NA, nrow = max_dt, ncol = 4)
  colnames(qt) <- c("q25","q50","q75","dt")
  rownames(qt) <- 1:max_dt
  
  ## Popoliamo la matrice con i quantili
  for (i in 1:max_dt) {
    qt[i,] <- c(quantile(as.numeric(res.scale.for.plot[i,-ncol(res.scale.for.plot)]), probs = c(0.25,0.5,0.75)),i)
  }
  qt.for.plot <- as.data.frame(qt)
  
  return(qt.for.plot)
}

ridge.relMSE <- as.data.frame(quantile.relMSE(models.attr.ridge,results.ridge))
rf.relMSE <- as.data.frame(quantile.relMSE(models.attr.rf,results.rf))
gb.relMSE <- as.data.frame(quantile.relMSE(models.attr.gb,results.gb))
nn.relMSE <- as.data.frame(quantile.relMSE(as.data.frame(models.attr.nn),as.data.frame(results.nn)))

thirdDFrel <- data.frame(ridge = ridge.relMSE$q75,
                      rf = rf.relMSE$q75,
                      gb = gb.relMSE$q75,
                      nn = nn.relMSE$q75,
                      dT = 1:length(ridge.relMSE$q75))


MSEthird <- ggplot(thirdDF, aes(x = dT)) + 
  geom_line(stat = "identity", aes(y = ridge, color = "Ridge"), linewidth = lw) + 
  geom_line(stat = "identity", aes(y = rf, color = "RF"), linewidth = lw) + 
  geom_line(stat = "identity", aes(y = gb, color = "GB"), linewidth = lw) + 
  geom_line(stat = "identity", aes(y = nn, color = "NN"), linewidth = lw) +
  scale_color_manual(
    values = c("Ridge" = col[1], "RF" = col[2], "GB" = col[3], "NN" = col[4]),
    labels = c("Ridge", "RF", "GB","NN"),
    breaks = c("Ridge", "RF", "GB","NN")) + 
  ylab("") + ggtitle("Terzo quartile MSE") + 
  theme_bw() + theme_title_median + theme_axis_median + theme_legend_median + 
  theme(legend.position=c(0.22,0.9), legend.direction = "horizontal") +
  xlab("dT, giorni dopo la stima")
MSEthird

MSEthirdRel <- ggplot(thirdDFrel, aes(x = dT)) + 
  geom_line(stat = "identity", aes(y = ridge, color = "Ridge"), linewidth = lw) + 
  geom_line(stat = "identity", aes(y = rf, color = "RF"), linewidth = lw) + 
  geom_line(stat = "identity", aes(y = gb, color = "GB"), linewidth = lw) + 
  geom_line(stat = "identity", aes(y = nn, color = "NN"), linewidth = lw) +
  scale_color_manual(
    values = c("Ridge" = col[1], "RF" = col[2], "GB" = col[3], "NN" = col[4]),
    labels = c("Ridge", "RF", "GB","NN"),
    breaks = c("Ridge", "RF", "GB","NN")) + 
  ylab("") + ggtitle("Terzo quartile e_rel") + 
  theme_bw() + theme_title_median + theme_axis_median + theme_legend_median + 
  theme(legend.position=c(0.22,0.9), legend.direction = "horizontal") +
  xlab("dT, giorni dopo la stima")
MSEthirdRel

ggsave(filename="dataset8_AR_moreARmid_MSEThird.pdf", plot = MSEthurd,
       device ="pdf", width=8, height = 4)
ggsave(filename="dataset8_AR_moreARmid_MSEThirdRel.pdf", plot = MSEthurdRel,
       device ="pdf", width=8, height = 4)


#### GRAFICI DEL TERZO QUARTILE NON RELATIVO ####

## TRACCIATI TERZO QUARTILE, su una singola replicazione. ##
setwd("D:/shared_directory_VM/simulazioniLight/dataset20_StagWithTrend")

#Utilizzano la sintesi prodotta inizialmente.
load("synthesis/MSEqt")
col <- brewer.pal(9, "Set1")[1:4]

model = 1
ridge.qMSE <- list(q50 = NA, q75 = NA); rf.qMSE <- list(q50 = NA, q75 = NA); gb.qMSE <- list(q50 = NA, q75 = NA) 
nn.qMSE <- list(q50 = NA, q75 = NA)

ridge.qMSE$q75 = MSEqt$ridge$last[model,]
rf.qMSE$q75 = MSEqt$rf$last[model,]
gb.qMSE$q75 = MSEqt$gb$last[model,]
nn.qMSE$q75 = MSEqt$nn$last[model,]

thirdDF <- data.frame(ridge = ridge.qMSE$q75,
                      rf = rf.qMSE$q75,
                      gb = gb.qMSE$q75,
                      nn = nn.qMSE$q75,
                      dT = 1:length(ridge.qMSE$q75))

lw=0.7
MSEthird <- ggplot(thirdDF, aes(x = dT)) + 
  geom_line(stat = "identity", aes(y = ridge, color = "Ridge"), linewidth = lw) + 
  geom_line(stat = "identity", aes(y = rf, color = "RF"), linewidth = lw) + 
  geom_line(stat = "identity", aes(y = gb, color = "GB"), linewidth = lw) + 
  geom_line(stat = "identity", aes(y = nn, color = "NN"), linewidth = lw) +
  scale_color_manual(
    values = c("Ridge" = col[1], "RF" = col[2], "GB" = col[3], "NN" = col[4]),
    labels = c("Ridge", "RF", "GB","NN"),
    breaks = c("Ridge", "RF", "GB","NN")) + 
  ylab("") + ggtitle("Terzo quartile MSE") + 
  theme_bw() + theme_title_median + theme_axis_median + theme_legend_median + 
  theme(legend.position=c(0.22,0.9), legend.direction = "horizontal") +
  xlab("dT, giorni dopo la stima")
MSEthird



## TRACCIATI MEDIANA MSE ##
ridge.qMSE$q50 = MSEqt$ridge$median[model,]
rf.qMSE$q50 = MSEqt$rf$median[model,]
gb.qMSE$q50 = MSEqt$gb$median[model,]
nn.qMSE$q50 = MSEqt$nn$median[model,]

medDF <- data.frame(ridge = ridge.qMSE$q50,
                      rf = rf.qMSE$q50,
                      gb = gb.qMSE$q50,
                      nn = nn.qMSE$q50,
                      dT = 1:length(ridge.qMSE$q50))


MSEmed <- ggplot(medDF, aes(x = dT)) + 
  geom_line(stat = "identity", aes(y = ridge, color = "Ridge"), linewidth = lw) + 
  geom_line(stat = "identity", aes(y = rf, color = "RF"), linewidth = lw) + 
  geom_line(stat = "identity", aes(y = gb, color = "GB"), linewidth = lw) + 
  geom_line(stat = "identity", aes(y = nn, color = "NN"), linewidth = lw) +
  scale_color_manual(
    values = c("Ridge" = col[1], "RF" = col[2], "GB" = col[3], "NN" = col[4]),
    labels = c("Ridge", "RF", "GB","NN"),
    breaks = c("Ridge", "RF", "GB","NN")) + 
  ylab("") + ggtitle("Tracciato MSE mediano") + 
  theme_bw() + theme_title_median + theme_axis_median + theme_legend_median + 
  theme(legend.position=c(0.22,0.9), legend.direction = "horizontal") +
  xlab("dT, giorni dopo la stima")
MSEmed


ggsave(filename="data17_incrementalCD_NNvsGBlessT_MSEmedianoSingolo.pdf",
       plot=NNvsGBqtMSEPlot, width=8, height=4, device="pdf")


### GBeverything vs GBonlyLag12.

gbEverything
gbOnlyLag12

qtGBMedianMinMaxEverything <- as.data.frame(t(as.matrix(apply(gbEverything$median,2,function(x)quantile(x,probs=c(0.25,0.75))))))
colnames(qtGBMedianMinMaxEverything) <- c("q25","q75")
qtGBMedianMinMaxEverything$dT <- 1:nrow(qtGBMedianMinMaxEverything)

qtGBMedianMinMaxLag12 <- as.data.frame(t(as.matrix(apply(gbOnlyLag12$median,2,function(x)quantile(x,probs=c(0.25,0.75))))))
colnames(qtGBMedianMinMaxLag12) <- c("q25","q75")
qtGBMedianMinMaxLag12$dT <- 1:nrow(qtGBMedianMinMaxLag12)

#Plot che mette a confronto.
col <- brewer.pal(11, "PiYG")[7:11]

GBvsGBqtMSEPlot <- ggplot(qtGBMedianMinMaxEverything, aes(x = dT)) + 
  geom_line(stat = "identity", aes(y = q25, color = "GB tutte"), linewidth = lw) + 
  geom_line(stat = "identity", aes(y = q75, color = "GB tutte"), linewidth = lw) + 
  geom_line(data = qtGBMedianMinMaxLag12, aes(x=dT, y = q25, color="GB lag12"), linewidth = lw) +
  geom_line(data = qtGBMedianMinMaxLag12, aes(x=dT, y = q75, color="GB lag12"), linewidth = lw) + 
  scale_color_manual(
    values = c("GB tutte" = col[5], "GB lag12" = col[2]),
    labels = c("GB tutte","GB lag12"),
    breaks = c("GB tutte","GB lag12")) + theme_bw() + theme_legend_median +
  theme_title_median + theme_axis_median +
  theme(legend.position = c(0.25,0.88), legend.direction = "horizontal") +
  ylab("MSE") +
  guides(color = guide_legend(title = "Plot1"))
GBvsGBqtMSEPlot


ggsave(filename="STAG_GBvsGB_dataCreation.pdf",
       plot=GBvsGBqtMSEPlot,width=6, height = 3.5, device="pdf")
