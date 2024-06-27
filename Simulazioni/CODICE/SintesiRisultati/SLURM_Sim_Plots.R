###########################################################################################
######   PLOT DEI RISULTATI DI UNA SIMULAZIONE   ##########################################
###########################################################################################

# Gli oggetti necessari sono contenuti nella directory "sim.out/synthesis".

library(RColorBrewer)
library(ggplot2)
library(patchwork)

#### PLOT DATA ####
#Questo permette di creare i grafici dei dataset.
setwd("C:/Users/User/Documents/AI AGING/SLURM")
source("SLURM_Base_Functions.R")
source("SLURM_DataCreateFunctionsLight.R")

theme_title_data <- theme(
  plot.title = element_text(
    hjust = 0.5,  # Center the title
    size = 20,    # Set the font size
    color = "black",  # Set the font color
    family = "serif",
    face ="italic",
    margin = margin(b = 10)  # Add margin at the bottom
  ))

theme_axis_data <- theme(
  axis.title.x = element_text(size = 18, family = "serif"),
  axis.text.x = element_text(size = 14,family = "serif"),
  axis.title.y = element_text(size = 18, family = "serif"),
  axis.text.y = element_text(size = 14,family = "serif"))

theme_legend_data <- theme(legend.position = c(0.5, 0.9), legend.title = element_blank(),
                         panel.border = element_rect(colour = "black", fill=NA),
                         legend.box.margin = margin(t = 1, l = 1,b =1.5,r=1),
                         legend.box.background = element_rect(colour = "black"),
                         legend.direction = "horizontal", legend.key.width = unit(0.5,"cm"),
                         legend.text = element_text(family ="serif", size =15))


get.daily.means.1 <- function(data) {
  data.dm <- data.frame(Days = unique(data$Days))
  data.dm$output.mean <- apply(as.matrix(data.dm$Days),1,function(x) mean(data$output[data$Days == x])) 
  return(data.dm)
}

mobile.average <- function(order = m, data = df_for_plot){
  ma <- apply(as.matrix(data$Days),1,function(x) mean(data$output.mean[data$Days >= (x-order) & data$Days <= (x+order)]))
  ma[c(1:order,(length(ma)-order+1):length(ma))] <- NA
  return(ma)
}

data <- get_dataset_20_stagAbitMoreSTAGwithEverythingAndTrend(job_index = 1, sim_number=100)$data

m <- 15 #Per la media mobile
df_for_plot <- get.daily.means.1(data)[,c("Days","output.mean")]
df_for_plot$mobile_av <- mobile.average(m, df_for_plot)

data_plot <- ggplot(data=df_for_plot, aes(x=Days)) + geom_line(aes(y = output.mean, color = "Media giornaliera"), linewidth = 1) + 
  geom_line(aes(y = mobile_av, color = "Media mobile"),linewidth=0.5) + 
  scale_color_manual(
    values = c("Media giornaliera" = "lightblue", "Media mobile" = "black"),
    labels = c("Media giornaliera", "Media mobile"),
    breaks = c("Media giornaliera", "Media mobile")) + 
  ggtitle("") + ylab("Y") + xlab("giorni")


data_plot <- data_plot + theme_bw() + theme_title_data + theme_axis_data + theme_legend_data
data_plot



ggsave(filename="dataset20_StagWithTrendplot.pdf", plot = data_plot,
       path = "D:/shared_directory_VM/simulazioniLight/dataset20_StagWithTrend",
       device = "pdf", width = 10, height = 4)


##############   PLOT R2   ################################

setwd("D:/shared_directory_VM/simulazioniLight/dataset1_sim.out/synthesis")

## R2 BARPLOT

theme_title_r2 <- theme(
  plot.title = element_text(
    hjust = 0.5,  # Center the title
    size = 12,    # Set the font size
    color = "black",  # Set the font color
    family = "serif",
    face ="italic",
    margin = margin(b = 10)  # Add margin at the bottom
  ))

theme_axis_r2 <- theme(
  axis.text.x = element_text(size = 10,family = "serif"),
  axis.text.y = element_text(size = 10,family = "serif"))

load("mean_R2_ridge");load("mean_R2_rf");load("mean_R2_gb");
load("mean_R2_nn")

R2s <- c(mean(mean_R2_ridge), mean(mean_R2_rf), mean(mean_R2_gb),
         mean(mean_R2_nn)) 
names(R2s) <- c("Ridge","RF","GB","NN")
mean_R2s <- data.frame(model = names(R2s), R2= R2s)
mean_R2s$model <- factor(mean_R2s$model, levels = rev(names(R2s)))

col <- brewer.pal(9, "Set1")[1:4]


plot_R2 <- ggplot(mean_R2s, aes(x = model, y = R2, fill = model)) +
  geom_bar(stat = "identity", position=position_dodge(width = 0.58), width = 0.5, color ="black") +
  theme_classic() + 
  scale_fill_manual(
    values = c("Ridge" = col[1], "RF" = col[2], "GB" = col[3], "NN" = col[4]),
    labels = c("Ridge", "RF", "GB", "NN")) + 
  xlab("") + ylab("") + ylim(0,1) + 
  ggtitle("R^2 predittivo") + coord_flip() + theme(legend.position = "none") + 
  geom_text(aes(label=round(R2,2)), hjust=2, color="white", size=3.5) + theme_title_r2 + theme_axis_r2

plot_R2

ggsave(filename="dataset20_STAGaBitMoreLessWithOnlyLag12_R2.pdf", plot = plot_R2, device ="pdf", width=8, height = 2)




######## PLOT ERRORI QUANTILI ########
#SETTING GRAFICI#
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


load("quantile_error_ridge");load("quantile_error_rf");load("quantile_error_gb");
load("quantile_error_nn")

quantile.ic <- function(quantiles) {
  as.data.frame(t(apply(quantiles,2,function(x) quantile(x, probs=c(0.05,0.5,0.95)))))
}


col = c("#525252","#FED976","#E31A1C")


##############    RIDGE    ###############

#Aggiungere un intervallo per il quantile della ridge
ridge_ic_25 <- quantile.ic(quantile_error_ridge$first_qt)
ridge_ic_50 <- quantile.ic(quantile_error_ridge$median)
ridge_ic_75 <- quantile.ic(quantile_error_ridge$third_qt)
colnames(ridge_ic_25) <- gsub("%","",paste0("q25_",colnames(ridge_ic_25)))
colnames(ridge_ic_50) <- gsub("%","",paste0("q50_",colnames(ridge_ic_50)))
colnames(ridge_ic_75) <- gsub("%","",paste0("q75_",colnames(ridge_ic_75)))
ridge_for_plot <- cbind(ridge_ic_25,ridge_ic_50,ridge_ic_75)
ridge_for_plot$Days <- 1:nrow(ridge_for_plot)

plot_ridge <- ggplot(ridge_for_plot, aes(x = Days)) + 
  geom_line(aes(y = q25_50, color = "25th"), linewidth =0.7) +
  geom_line(aes(y = q50_50, color = "50th"), linewidth =0.7) +
  geom_line(aes(y = q75_50, color = "75th"), linewidth =0.7) +
  scale_color_manual(
    values = c("25th" = col[2], "50th" = col[1], "75th" = col[3]),
    labels = c("25esimo percentile", "mediana", "75esimo percentile"))+
  scale_x_continuous(breaks = c(0,1000,2000,2800)) + 
  #  ylim(0.9,1.1) + 
  ggtitle("RIDGE") +
  ylab("e_rel") + xlab("dT, giorni dopo la stima")

plot_ridge <- plot_ridge + theme_bw() + theme_legend_median +
  theme_title_median + theme_axis_median + ylim(0.8,1.2)
plot_ridge




#########    RANDOM FOREST    ##########
rf_ic_25 <- quantile.ic(quantile_error_rf$first_qt)
rf_ic_50 <- quantile.ic(quantile_error_rf$median)
rf_ic_75 <- quantile.ic(quantile_error_rf$third_qt)
colnames(rf_ic_25) <- gsub("%","",paste0("q25_",colnames(rf_ic_25)))
colnames(rf_ic_50) <- gsub("%","",paste0("q50_",colnames(rf_ic_50)))
colnames(rf_ic_75) <- gsub("%","",paste0("q75_",colnames(rf_ic_75)))
rf_for_plot <- cbind(rf_ic_25,rf_ic_50,rf_ic_75)
rf_for_plot$Days <- 1:nrow(rf_for_plot)

plot_rf <- ggplot(rf_for_plot, aes(x = Days)) + 
  geom_line(aes(y = q25_50, color = "25th"), linewidth =0.7) +
  geom_line(aes(y = q50_50, color = "50th"), linewidth =0.7) +
  geom_line(aes(y = q75_50, color = "75th"), linewidth =0.7) +
  scale_color_manual(
    values = c("25th" = col[2], "50th" = col[1], "75th" = col[3]),
    labels = c("25esimo percentile", "mediana", "75esimo percentile")) +
  scale_x_continuous(breaks = c(0,1000,2000,2800)) + 
  #  ylim(0.9,1.1) + 
  ggtitle("RF") +
  ylab("e_rel") + xlab("dT, giorni dopo la stima")

plot_rf <- plot_rf + theme_bw() + theme_legend_median +
  theme_title_median + theme_axis_median + ylim(0.8,1.3)
plot_rf


###########    GRADIENT BOOSTING    ############
gb_ic_25 <- quantile.ic(quantile_error_gb$first_qt)
gb_ic_50 <- quantile.ic(quantile_error_gb$median)
gb_ic_75 <- quantile.ic(quantile_error_gb$third_qt)
colnames(gb_ic_25) <- gsub("%","",paste0("q25_",colnames(gb_ic_25)))
colnames(gb_ic_50) <- gsub("%","",paste0("q50_",colnames(gb_ic_50)))
colnames(gb_ic_75) <- gsub("%","",paste0("q75_",colnames(gb_ic_75)))
gb_for_plot <- cbind(gb_ic_25,gb_ic_50,gb_ic_75)
gb_for_plot$Days <- 1:nrow(gb_for_plot)

plot_gb <- ggplot(gb_for_plot, aes(x = Days)) + 
  geom_line(aes(y = q25_50, color = "25th"), linewidth =0.7) +
  geom_line(aes(y = q50_50, color = "50th"), linewidth =0.7) +
  geom_line(aes(y = q75_50, color = "75th"), linewidth =0.7) +
  scale_color_manual(
    values = c("25th" = col[2], "50th" = col[1], "75th" = col[3]),
    labels = c("25esimo percentile", "mediana", "75esimo percentile")) +
  scale_x_continuous(breaks = c(0,1000,2000,2800)) + 
  ggtitle("GB") +
  ylab("e_rel") + xlab("dT, giorni dopo la stima")

plot_gb <- plot_gb + theme_bw() + theme_legend_median +
  theme_title_median + theme_axis_median + ylim(0.8,1.3)
plot_gb


###########    RETE NEURALE    ############
nn_ic_25 <- quantile.ic(quantile_error_nn$first_qt)
nn_ic_50 <- quantile.ic(quantile_error_nn$median)
nn_ic_75 <- quantile.ic(quantile_error_nn$third_qt)
colnames(nn_ic_25) <- gsub("%","",paste0("q25_",colnames(nn_ic_25)))
colnames(nn_ic_50) <- gsub("%","",paste0("q50_",colnames(nn_ic_50)))
colnames(nn_ic_75) <- gsub("%","",paste0("q75_",colnames(nn_ic_75)))
nn_for_plot <- cbind(nn_ic_25,nn_ic_50,nn_ic_75)
nn_for_plot$Days <- 1:nrow(nn_for_plot)

plot_nn <- ggplot(nn_for_plot, aes(x = Days)) + 
  geom_line(aes(y = q25_50, color = "25th"), linewidth =0.7) +
  geom_line(aes(y = q50_50, color = "50th"), linewidth =0.7) +
  geom_line(aes(y = q75_50, color = "75th"), linewidth =0.7) +
  scale_color_manual(
    values = c("25th" = col[2], "50th" = col[1], "75th" = col[3]),
    labels = c("25esimo percentile", "mediana", "75esimo percentile")) +
  scale_x_continuous(breaks = c(0,1000,2000,2800)) + 
  #  ylim(0.9,1.1) + 
  ggtitle("NN") +
  ylab("e_rel") + xlab("dT, giorni dopo la stima") + ylim(0.8,1.2)

plot_nn <- plot_nn + theme_bw() + theme_legend_median +
  theme_title_median + theme_axis_median 
plot_nn






## Combinazione.
plot_finale <- plot_ridge + plot_rf + plot_gb +
  plot_nn +
  plot_layout(guides="collect", ncol=2, nrow=2, byrow=TRUE) &
  theme(legend.box.background = element_rect(color = "black", linewidth = 1)) +
  theme(legend.position="bottom")
plot_finale




ggsave(filename="dataset1_degenPlot.pdf",
       plot=plot_finale, width=8, height=6, device="pdf")


## Questo è utilizzato se vogliono essere confrontati singoli plot.

plot_finaleGBvsNN <- plot_gb + plot_nn +
  plot_layout(guides="collect", ncol=2, nrow=1, byrow=TRUE) &
  theme(legend.box.background = element_rect(color = "black", linewidth = 1)) +
  theme(legend.position="bottom")
plot_finaleGBvsNN


ggsave(filename="dataset15_cubeFurherDD_GBvsNN_02_degen.png", plot=plot_finaleGBvsNN, width=8, height=4, device="png")
