La cartella contiene gli script specifici per sintetizzare i risultati e produrre i grafici. Queste operazioni non vengono eseguite su calculus, e prevedono quindi di aver scaricato la cartella "sim.out" con i risultati. Questi sono i risultati complessivi, relativi a tutti e quattro i modelli.

I risultati ottenuti sono separati in due cartelle, "models" e "results", allo stesso livello creare la cartella synthesis. 

## SINTETIZZARE I RISULTATI ##

Lo script "SLURM_Post_Sim_Synt.R" crea degli oggetti con la sintesi dei risultati e li salva nella cartella "synthesis".

Lo script "SLURM_Sim_Plots.R" utilizza quei risultati per produrre i grafici contenenti l'R^2 predittivo iniziale e la combinazione dei grafici di "AI Aging".

Lo script "SLURM_Hist.R" non si basa invece sulla sintesi fatta da "SLURM_Post_Sim_Synt.R". Permette però di creare i grafici contenti i livelli medi del terzo quartile e le variazioni di errore relativo.

Lo script "SLURM_qMSE_Plots.R" fa nuovamente una sintesi separata degli oggetti iniziali, ma produce i grafici contenenti gli andamenti degli MSE mediani. Permette di creare anche i grafici degli andamenti del terzo quartile dell'MSE. Utilizza però i valori di R^2 prodotti dal primo script, quindi va eseguito dopo.

### SLURM_Post_Sim_Synt
Il file produce due oggetti per ciascuna categoria di modello (indicate come 'modello', che può assumere i valori ridge, gb, rf e nn):
- mean_R2_'modello', un vettore contenente l'R^2 predittivo medio in ciascuna replicazione (lunghezza quindi pari a 100, in un caso regolare).
- quantile_errore_'modello', una lista contenente 3 elementi, "first_qt", "median" e "third_qt". Queste sono 3 matrici, di dimensione "numero_di_simulazioni" X "dT massimo". Questi raccolgono i tracciati dei quartili dei grafici di "AI Aging" delle singoe replicazioni. Ciasuna riga della matrice "first_qt" contiene quindi il tracciato del primo quartile della replicazione corrispondente, e in modo analogo gli altri due elementi raccolgono gli altri quartili.

Vengono prodotti due oggetti per categoria di modello, quindi 8 in totale, che vengono utilizzati dallo script "SLURM_Sim_Plots.R" per produrre i grafici.



