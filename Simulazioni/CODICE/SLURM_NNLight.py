#Pacchetti generici
import sys
import random    #Random seed
import feather   #Per leggere i dataset
import time
from sklearn.model_selection import train_test_split
import math
import os

#Reti neurali
import numpy as np
import pandas as pd
import keras
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense
from tensorflow.keras import initializers
from tensorflow.keras import callbacks
from tensorflow.random import set_seed as tf_set_seed
#from tensorflow.config.experimental import enable_op_determinism

print(sys.version)
print(os.environ["PYTHONHASHSEED"])

#Simulazione numero
sim_number = 100 # Numero complessivo di simulazioni
array_task = int(sys.argv[1])
path = "sim.out" #Dove mettere i risultati

#Lettura dei dati
#data = feather.read_dataframe("dataset1.feather")  #Un oggetto pd.dataFrame
dataPath = "pyData"    #Percorso fino alla cartella contenente i dataset.
data = feather.read_dataframe(dataPath + "/dataset" + str(array_task) + ".feather")
#Le variabili qualitative sono già state traformate in dummy quantitative e standardizzate.


#Variabili quantitativa + funzione per standardizzare le colonne.
#quant = ["V1","V2","V3","V4","V5","V6","V7","V8","V9","V10"]
#quant = ["V1","V2","V3","V4"]
#quant = ["V" + str(item) for item in range(1, data.shape[1] - 1)]
#quant = ["V1","V2","V3","V4","V5","V6","V7","outL1"]
#quant = ["V1","V2","V3","V4","V5","V6","V7","outL1","outL2"]
quant = ["V1","V2","V3","V4","day_of_year","output.l1","output.l2","output.sl1"]
#quant = ["V1","V2","V3","V4","day_of_year","output.sl1"]
#quant = ["V1","V2","V3","V4","day_of_year","output.l1","output.l2"]
#quant = ["V1","V2","V3","V4","output.l1","output.l2"]
def scale_center (column, centro, scala):
    return (column - centro[column.name])/scala[column.name]


#Genera i seed per rendere le simulazioni replicabili
def genSeeds(n):
    seeds = []
    for i in range(0,n):
        seeds.append(int(random.random()*10000))
    return seeds

#Genera sempre gli stessi n_sim semi, in modo da rendere ciascuna simulazione replicabile.
random.seed(5014)
seeds_for_sims = genSeeds(sim_number)

#array_task = 1 #La simulazione corrente.
print("\nSeme usato: ")
print(seeds_for_sims[array_task - 1]) #Il seme che sarà usato

#Funzione per il calcolo degli MSE, necessaria per calcolare gli R2 previsivi
def MSSE_t0(data_X, days_selected):
    lista = []
    for i in days_selected:
        train_t0 = data_X.loc[(data_X["Days"] > i) & (data_X["Days"] <= (i + mse_window))]
        lista.append(((train_t0["output"] - train_t0["output"].mean())**2).mean())
    d = pd.DataFrame({"MSSE_t0":lista})
    return d 
#Restituisce lo stesso output che su R


#Parametri di degenerazione
nsets = 4
retrain = 60  ## Multiplo di 3
max_t0 = 365
finestra = 30
mse_window = 60
## Lunghezza del periodo di degenerazione.
max_dt = int(data["Days"].max()) - (364 + max_t0) - finestra - mse_window
print(max_dt)
## Giorni ai quali stimare i modelli.
days_selected = range(365,365+max_t0 + 5, 5)

#MSSE iniziali
MSSE_days_sel = MSSE_t0(data, days_selected)

#Funzione per definire la struttura di una rete neurale
def create_model(layers, size, n_input, seed):
    #Per rendere la stima replicabile
    random.seed(seed)
    seeds= genSeeds(4)

    #Struttura del modello
    model = Sequential()
    model.add(Dense(size[0], input_shape=(n_input,), activation ="relu", name="Hidden1", kernel_initializer=initializers.RandomNormal(seed = seeds[0])))
    model.add(Dense(size[1], activation='relu', name="Hidden2", kernel_initializer=initializers.RandomNormal(seed = seeds[1])))
    model.add(Dense(size[2], activation='relu', name="Hidden3", kernel_initializer=initializers.RandomNormal(seed = seeds[2])))
    model.add(Dense(1, activation="linear", name="Output", kernel_initializer=initializers.RandomNormal(seed = seeds[3])))

    #Metodo di stima da utilizzare
    model.compile(loss='mean_squared_error', optimizer='Adam', metrics = ["mean_squared_error"])
    
    return model 
    

vars = set(data.columns)
escluse = {"output","Days"}
X = vars.difference(escluse)

#Le posizioni di Days e output
days_pos = data.shape[1]
out_pos = data.shape[1]-1

#Per memorizzare le quantità necessarie per valutare la degenerazione
results = pd.DataFrame(np.empty((max_t0 + 1,max_dt,)))
results[:] = np.nan

models_attr = pd.DataFrame(np.empty((max_t0 + 1,4,)))
models_attr[:] = np.nan
models_attr.rename(columns = {0:"R2", 1:"Grid_opt", 2:"MSE a t0", 3:"Epoche"}, inplace = True)

#Specifiche della rete neurale
reps = 1 #Numero di punti di partenza per ciascuna rete.
layers = 3
size = [200,200,200]
#size = [50,50,50]

#Per rendere i risultati riproducibili. Fissiamo il seed della simulazione.
random.seed(seeds_for_sims[array_task -1])
np.random.seed(seeds_for_sims[array_task -1])
tf_set_seed(seeds_for_sims[array_task -1])
keras.utils.set_random_seed(seeds_for_sims[array_task -1])
#keras.utils.set_random_seed() fissa gli stessi seed precedenti, però voglio essere sicuro.

start = time.time()

for i in days_selected:
    
    print("\nCurrently: ",i)
    data_to_use = data.loc[(data["Days"]<=i) & (data["Days"] >= (i-364))].copy(deep = True)

    #Primo anno standardizzato.
    centro = data_to_use[quant].mean()
    scala = data_to_use[quant].std()
    data_to_use[quant] = data_to_use[quant].apply(lambda x: scale_center(x, centro, scala))

    #Definire i dataset da usare per stima
    train, val = train_test_split(data_to_use, test_size=0.2)
    x_train = train[list(X)]
    y_train = train["output"]
    x_val = val[list(X)]
    y_val = val["output"]

    #Generare i seed da usare per le replicazioni
    seeds = genSeeds(reps)
    #Metriche dei modelli
    models_reps = []
    epochs_reps = []

    #Stimiamo le reti neurali da diversi punti di partenza.
    for j in range(0,reps):
        print("\n Rep: ",j)
        model_nn = create_model(layers,size,data_to_use.shape[1] - 2,seeds[j])
        storia = model_nn.fit(x = x_train, y = y_train, batch_size = 64 , epochs = 1000, validation_data=(x_val,y_val), verbose =0, 
                   callbacks = callbacks.EarlyStopping(monitor ="val_mean_squared_error", min_delta =0, patience =4))

        #Salvare le metriche per scegliere il seed migliore.
        models_reps.append(min(storia.history["val_mean_squared_error"]))
        epochs_reps.append(storia.history["val_mean_squared_error"].index(min(storia.history["val_mean_squared_error"])) + 1)

    seed_best = seeds[models_reps.index(min(models_reps))]
    epoche_best = epochs_reps[models_reps.index(min(models_reps))]

    #Trovare il modello migliore
    print("\n Ristimando il modello migliore ...",)
    model_nn = create_model(layers,size,data_to_use.shape[1] - 2,seed_best)
    model_nn.fit(x = x_train, y = y_train, batch_size = 64, epochs = epoche_best, validation_data=(x_val,y_val), verbose =0)


    #Memorizzo le epoche utilizzate
    models_attr.iloc[i-365,3] = epoche_best

    #R2
    fit = model_nn.predict(x=x_train, verbose = 0).flatten()
    SSE = ((y_train.to_numpy() - fit)**2).sum()
    DEV = ((y_train.to_numpy() - y_train.mean())**2).sum()
    models_attr.iloc[i-365,0] = 1 - SSE/DEV
    
    #Questo è un refuso
    models_attr.iloc[i-365,1] = 1
    
    ## MSE a t0 mse_window.
    t0_train = data.loc[(data["Days"]>i) & (data["Days"] <= (i + mse_window))].copy(deep = True)
    t0_train[quant] = t0_train[quant].apply(lambda x: scale_center(x, centro, scala))
    fit = model_nn.predict(x=t0_train[list(X)]).flatten()
    #Memorizzo l'MSE nella finestra immediatamente successiva al momento della stima.
    models_attr.iloc[i-365,2] = ((t0_train["output"].to_numpy() - fit)**2).mean() 

    ## Previsioni per i 1006 giorni successivi
    print("\nPrevisioni...")
    to_predict_x = data.loc[(data["Days"] >= (i + mse_window + 1)) & (data["Days"] < (i + mse_window + max_dt + finestra)), list(X)].copy(deep=True)
    to_predict_x[quant] = to_predict_x[quant].apply(lambda x: scale_center(x, centro, scala))
    to_predict = data.loc[(data["Days"] >= (i + mse_window + 1)) & (data["Days"] < (i + mse_window + max_dt + finestra)),"output"]
    days_to_use = data.loc[(data["Days"] >= (i + mse_window + 1)) & (data["Days"] < (i + mse_window + max_dt + finestra)),"Days"]
    fits = model_nn.predict(x=to_predict_x, verbose = 0).flatten()

    prestazioni = [] #Memorizziamo l'MSE del modello nel "futuro"
    for j in range(1,max_dt+1):
        window = (days_to_use >= (i + j + mse_window)) & (days_to_use < (i + j + mse_window + finestra))
        mse = ((to_predict[window] - pd.Series(fits, index = to_predict.index)[window])**2).mean()
        results.iloc[i-365,j-1] = mse

end = time.time()
print(end - start)

models_attr = models_attr.dropna()
models_attr["R2_pred"] = 1 - models_attr["MSE a t0"].to_numpy()/MSSE_days_sel["MSSE_t0"].to_numpy()
models_attr_nn = models_attr
results_nn = results.dropna()

models_attr_nn.to_feather(path + "/models/models.nn." + str(array_task) + ".feather")
results_nn.to_feather(path + "/results/results.nn." + str(array_task) + ".feather")
