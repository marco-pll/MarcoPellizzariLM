
##############################################################
###### DATASETS FOR PYTHON USE ###############################
##############################################################


#setwd("C:/Users/User/Documents/AI AGING/SLURM")   #Debugging
library(feather)                                   #Formato
source("SLURM_Base_Functions.R")
source("SLURM_DataCreateFunctionsLight.R")
#source("SLURM_Dataset_Creating_Functions.R")

sim_number <- 100     #Numero di simulazioni.

path = "pyData/"

##########   SINGOLO DATASET    ##########
#Sezione dedicata al debugging.
# array_task = 1
# path = "python prove/"
# 
# data <- get_dataset_20_stagAbitMoreSTAGwithEverythingAndTrend(job_index = array_task,sim_number)$data
# 
# # ### STANDARDIZZAZIONE ###
# vars <- colnames(data)
# escluse <- c("output","Days")
# X <- setdiff(vars,escluse)
# 
# quant <- c()
# for(var in X) {
#   if(is.numeric(data[,var])) quant <- c(quant,var)
# }
# qual <- setdiff(X,quant)
# #data[,quant] <- scale(data[,quant])
# 
# ### QUAL TO QUANT ###
# data <- na.omit(data)
# data_x <- data[,X]  #DataFrame delle sole esplicative
# data_x <- model.matrix(~.,na.omit(data_x))[,-1] #Qualitative rese dummy.
# data <- cbind(data_x,data[,escluse])
# 
# write_feather(data, paste0(path,"dataset",array_task,".feather"))
# head(data)



#########   DATI     #########################################################
#Dove mettiamo i dataset per la rete neurale?
#In una cartella chiamata pyData

for (df in 1:sim_number){
  cat("\ndataset",df,"...")
  data <- get_dataset_20_stagAbitMoreSTAGwithEverythingAndTrend(job_index = df,sim_number)$data
  
  ### STANDARDIZZAZIONE ### NO IN REALTA ###
  vars <- colnames(data)
  escluse <- c("output","Days")
  X <- setdiff(vars,escluse)
  
  quant <- c()
  for(var in X) {
    if(is.numeric(data[,var])) quant <- c(quant,var)
  }
  qual <- setdiff(X,quant)
  
  ### QUAL TO QUANT ###
  data <- na.omit(data)
  data_x <- data[,X]  #DataFrame delle sole esplicative
  data_x <- model.matrix(~.,na.omit(data_x))[,-1] #Qualitative rese dummy.
  data <- cbind(data_x,data[,escluse])
  
  write_feather(data, paste0(path,"dataset",df,".feather"))
}

