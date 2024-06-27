Questa cartella contiene i codici e gli ambienti utilizzati nei capitoli 3, 4 e 5.

La cartella "CALCULUS" contiene i container utilizzati per eseguire le simulazioni sul cluster del Dipartimento di Scienze Statistiche e i file bash (.sh) per inviare i job.

La cartella "CODICE" contiene invece i file, in R e in Python, per eseguire le simulazioni. I file più importanti sono riassunti di seguito:
- "SLURM_Base_Functions.R", un file contenente alcune funzioni di base necessarie per simulare i dataset.
- "SLURM_DataCreateFunctionsLight.R", un file che contiene le funzioni per simulare i dataset.
- "SLURM_SimulationsLight.R" applica il "test" di degradazione temporale al singolo dataset simulato (in una replicazione). Questo produce i risultati per lo stimatore ridge, la foresta casuale e il gradient boosting.
- "SLURM_NNLight.py" applica il "test" di degradazione temporale al singolo dataset simulato (in una replicazione), e produce i risultati per la rete neurale.

Questi sono i file fondamentali per eseguire le simulazioni.
"SLURM_SimulationsLight.R" e "SLURM_NNLight.py" applicano il "test" nella singola replicazione della procedura, e producono degli oggetti, questi sono di due tipi:
- "models.attr.'modello'.'numero_della_replicazione'" e
- "results.'modello'.'numero_della_replicazione'".

dove 'modello' può indicare uno tra "ridge","rf","gb" o "nn", mentre 'numero_della_replicazione' va da 1 a 100. Per ogni modello, e ogni replicazione della simulazione, vengono quindi prodotti due oggetti, due matrici.

Gli oggetti di tipo "models.attr" contengono alcune informazioni legate alla tipologia di modello associato, e ad ogni volta che questo è stato stimato su un sottoinsieme di stima. L'oggetto è una matrice, di dimensione "numero_di_sottoinsiemi_di_stima" X "numero_di_metriche_monitorate" (cambiano da un modello all'altro).
Le metriche principali sono però "MSE a t0", che indica l'MSE iniziale del modello, stimato sul dataset di stima, e "R2_pred", che indica il valore di R^2 predittivo iniziale del singolo modello. Le righe sono ordinate: la prima contiene le metriche associate al modello stimato sul primo sottoinsieme di stima, l'ultima riga sull'ultimo.
Questo oggetto non può essere utilizzato da solo per valutare la degradazione temporale del modello.

Gli oggetti di tipo "results" contengono i valori di MSE(dT) calcolati al variare della finestra mobile. L'oggetto è una matrice di dimensione pari a "numero_di_sottoinsiemi_di_stima" X "dT massimo". Ogni riga contiene quindi il tracciato dell'errore (MSE) di una specifico modello, le cui metriche sono riportate nella corrispondente riga dell'oggetto di tipo "models".

Un esempio:

"models.attr.ridge.1" e "results.ridge.1" contengono le informazioni relative al modello ridge, ed ad ogni volta che questo è stato stimato nella prima replicazione. Possono essere combinati per produrre il grafico di AI Aging per il modello ridge sulla prima replicazione. Come?
Ogni riga di "results.ridge.1" può essere divisa per l'elemento "MSE a t0" della corrispondente riga in "models.attr.ridge.1" per ottenere un tracciato di errore relativo. Avendo a disposizione, alla fine, "numero_di_sottoinsiemi_di_stima" tracciati, questi possono essere combinati, tramite il calcolo dei quartili.

Questo può essere fatto per ogni tipologia di modello. La differenza principale è che per la rete neurale gli oggetti prodotti sono di tipo ".feather", un formato che permette di leggere dati sia in Python che in R.



# ESEGUIRE LE SIMULAZIONI #
Come possono essere utilizzati questi script per produrre i risultati?

La procedura non è semplice, in quanto differisce per la rete neurale e gli altri modelli. Partendo da questi ultimi:

## ridge, rf e gb ##
- Scegliere la simulazione da eseguire. Questo viene fatto scegliendo la funzione appropriata dal file "SLURM_DataCreateFunctionsLight.R". La legenda, che indica cosa ciascuna delle funzioni produce, è riportata nel file "legendaDataset.txt".
- Scelta la funzione, questa deve essere sostituita nella riga 50 del file "SLURM_SimulationsLight.R", in modo che: data <- get_dataset_scelto(). Questo indica che lo script deve essere eseguito sullo specifico insieme di dati.
- Una volta scelto il dataset è necessario definire le specifiche per stimare i modelli. Per ridurre i tempi infatti, per ogni dataset, vengono utilizzate configurazioni specifiche per ciascun modello. Per lo stimatore ridge non c'é nulla da fare. Per la foresta casuale è necessario definire la griglia per la regolazione dello split ottimale (dalla riga 327 scegliere un mtry = () appropriato). Per il gradient boosting è necessario indicare il numero massimo di alberi (variabile num.trees, nelle funzioni "tree.num()" e "gb_estimates_par()", che deve assumere lo stesso valore). La scelta è importante, in quanto solo quella corretta permette di riprodurre i risultati. Nel file "SLURM_DataCreateFunctionsLight.R" sono comunque indicate, come commetto alle singole funzioni, le specifiche utilizzate nelle simulazioni, che permettono di riprodurre i risultati.
- È necessario indicare una cartella in cui salvare gli output. Questa cartella deve contenere le sotto-cartelle "models" e "results", e il suo path va specificato in corrispondenza della variabile "path" (riga 40). Per non modificare nulla è sufficiente creare la cartella "sim.out" nella stessa directory.
- Collocare i 3 file "SLURM_Base_Functions.R", "SLURM_DataCreateFunctionsLight.R" e "SLURM_SimulationsLight.R" nella stessa directory, in Calculus.
- 






















