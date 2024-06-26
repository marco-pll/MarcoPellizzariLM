La cartella contiene gli script specifici per sintetizzare i risultati e produrre i grafici. Queste operazioni non vengono eseguite su calculus, e prevedono quindi di aver scaricato la cartella "sim.out" con i risultati. Questi sono i risultati complessivi, relativi a tutti e quattro i modelli.

I risultati ottenuti sono separati in due cartelle, "models" e "results", allo stesso livello creare la cartella synthesis. 

## SINTETIZZARE I RISULTATI ##

Lo script "SLURM_Post_Sim_Synt.R" crea degli oggetti con la sintesi dei risultati e li salva nella cartella "synthesis".

Lo script "SLURM_Sim_Plots.R" utilizza quei risultati per produrre i grafici contenenti l'R^2 predittivo iniziale e la combinazione dei grafici di "AI Aging".

Lo script "SLURM_Hist.R" non si basa invece sulla sintesi fatta da "SLURM_Post_Sim_Synt.R". Permette però di creare i grafici contenti i livelli medi del terzo quartile e le variazioni di errore relativo.

Lo script "SLURM_qMSE_Plots.R" fa nuovamente una sintesi separata degli oggetti iniziali, ma produce i grafici contenenti gli andamenti degli MSE mediani. Permette di creare anche i grafici degli andamenti del terzo quartile dell'MSE. Utilizza però i valori di R^2 prodotti dal primo script, quindi va eseguito dopo.

### SLURM_Post_Sim_Synt.R
Il file produce due oggetti per ciascuna categoria di modello (indicate come 'modello', che può assumere i valori ridge, gb, rf e nn):
- mean_R2_'modello', un vettore contenente l'R^2 predittivo medio in ciascuna replicazione (lunghezza quindi pari a 100, in un caso regolare).
- quantile_errore_'modello', una lista contenente 3 elementi, "first_qt", "median" e "third_qt". Queste sono 3 matrici, di dimensione "numero_di_simulazioni" X "dT massimo". Questi raccolgono i tracciati dei quartili dei grafici di "AI Aging" delle singoe replicazioni. Ciasuna riga della matrice "first_qt" contiene quindi il tracciato del primo quartile della replicazione corrispondente, e in modo analogo gli altri due elementi raccolgono gli altri quartili.

Vengono prodotti due oggetti per categoria di modello, quindi 8 in totale, che vengono utilizzati dallo script "SLURM_Sim_Plots.R" per produrre i grafici.

### SLURM_Sim_Plots.R
Questo file può fare più cose:
- Può produrre un grafico di un dataset simulato. La prima parte è dedicata a ciò, e utilizzarla è piuttosto immediato. Si tratta solo di specificare la funzione "get_dataset()" corretta.
- Può produrre la rappresentazione grafica dei risultati di "SLURM_Sim_Plots.R". Produce quindi i grafici dell'R^2 predittivo iniziale medio e la combinazione dei grafici di "AI Aging".

### SLURM_Hist.R
Questo script produce gli istogrammi delle pendenze della mediana e del livello medio del terzo quartile.
Per farlo, nella prima parte elabora i risultati delle simulazioni. Non si basa quindi sugli script precedenti. Viene quindi prodotto un oggetto, "simuHist", che può essere salvato (e utilizzato solo per produrre i grafici) nella cartella "synthesis". Che dire, le righe successive producono i grafici e li salvano nella stessa cartella.

Una parte finale produce invece il grafico riportato nel riassunto del capitolo 3, che riporta la distribuzione delle differenze nel terzo quartile tra RF e NN. Può essere facilmente modificato per mostrare la distribuzione delle differenze tra altri modelli.

### SLURM_qMSE_Plots.R
Questo script permette di produrre i grafici degli andamenti degli MSE mediani (ed eventualmente di altri quartili).

La prima parte dello script permette il calcolo degli andamenti degli MSE mediani. Per farlo, è necessario collocarsi nella directory contenente i risultati (sim.out). Lo script produce quindi il grafico.

La seconda parte dello script permette di il calcolo degli andamenti del terzo quartile dell'MSE. Perché funzioni è necessario mandare le librerie e le variabili iniziali: è pensato per andare subito dopo alla prima parte.

Queste prime due parti utilizzano i valori dell'R^2 predittivo iniziale. Lo script può quindi essere utilizzato solo dopo aver utilizzato anche "SLURM_Post_Sim_Synt.R".

La terza parte permette di selezionare una specifica replicazione, e confrontare i tracciati dell'MSE mediano, per i diversi modelli, e del terzo quartile dell'MSE.



