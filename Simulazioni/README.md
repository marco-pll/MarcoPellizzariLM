Questa cartella contiene i codici e gli ambienti utilizzati nei capitoli 3, 4 e 5.

La cartella "CALCULUS" contiene i container utilizzati per eseguire le simulazioni sul cluster del Dipartimento di Scienze Statistiche e i file bash (.sh) per inviare i job.

La cartella "CODICE" contiene invece i file, in R e in Python, per eseguire le simulazioni. I file più importanti sono riassunti di seguito:
- "SLURM_Base_Functions.R", un file contenente alcune funzioni di base necessarie per simulare i dataset.
- "SLURM_DataCreateFunctionsLight.R", un file che contiene le funzioni per simulare i dataset.
- "SLURM_SimulationsLight.R" applica il "test" di degradazione temporale al singolo dataset simulato (in una replicazione). Questo produce i risultati per lo stimatore ridge, la foresta casuale e il gradient boosting.
- "SLURM_NNLight.py" applica il "test" di degradazione temporale al singolo dataset simulato (in una replicazione), e produce i risultati per la rete neurale.

Questi sono i file fondamentali per eseguire le simulazioni.
"SLURM_SimulationsLight.R" e "SLURM_NNLight.py" applicano il "test" nella singola replicazione della procedura, e producono due tipi di oggetti:
- "models.'modello'.'numero_della_replicazione'" e
- "results.'modello'.'numero_della_replicazione'".

dove 'modello' può indicare uno tra "ridge","rf","gb" o "nn", mentre 'numero_della_replicazione' va da 1 a 100. Per ogni modello, e ogni replicazione della simulazione, vengono quindi prodotti due oggetti, due matrici.

Gli oggetti di tipo "models." contengono alcune metriche misurate ogni volta che 'modello' è stato stimato su un sottoinsieme di stima. L'oggetto è una matrice, di dimensione "numero_di_sottoinsiemi_di_stima" X "numero_di_metriche_monitorate" (cambiano da un modello all'altro).
Le metriche principali sono però "MSE a t0", che indica l'MSE iniziale del modello, stimato sull'insieme di verifica, e "R2_pred", che indica il valore di R^2 predittivo iniziale del singolo modello. Le righe sono ordinate: la prima contiene le metriche associate al modello stimato sul primo sottoinsieme di stima, l'ultima riga sull'ultimo.
Questo oggetto non può essere utilizzato da solo per valutare la degradazione temporale del modello.

Gli oggetti di tipo "results." contengono i valori di MSE(dT) calcolati al variare della finestra mobile. L'oggetto è una matrice di dimensione pari a "numero_di_sottoinsiemi_di_stima" X "dT massimo". Ogni riga contiene quindi il tracciato dell'errore (MSE) di una specifico modello, le cui metriche sono riportate nella corrispondente riga dell'oggetto di tipo "models".

Un esempio:

"models.ridge.1" e "results.ridge.1" contengono le informazioni relative al modello ridge, ed ad ogni volta che questo è stato stimato nella prima replicazione. Possono essere combinati per produrre il grafico di AI Aging per il modello ridge sulla prima replicazione. Come?
Ogni riga di "results.ridge.1" può essere divisa per l'elemento "MSE a t0" della corrispondente riga in "models.ridge.1" per ottenere un tracciato di errore relativo. Avendo a disposizione, alla fine, "numero_di_sottoinsiemi_di_stima" tracciati, questi possono essere combinati, tramite il calcolo dei quartili.

Questo può essere fatto per ogni tipologia di modello. La differenza principale è che per la rete neurale gli oggetti prodotti sono di tipo ".feather", un formato che permette di leggere dati sia in Python che in R.



# ESEGUIRE LE SIMULAZIONI #
Come possono essere utilizzati questi script per produrre i risultati?

La procedura non è semplice, in quanto differisce per la rete neurale e gli altri modelli. Molte componenti variabili sono inoltre in codifica fissa, e devono essere modificate di volta in volta.

## Ridge, Foresta Casuale (rf) e Gradient Boosting (gb) ##
- Scegliere la simulazione da eseguire. Questo viene fatto scegliendo la funzione appropriata dal file "SLURM_DataCreateFunctionsLight.R". La legenda, che indica cosa ciascuna delle funzioni produce, è riportata nel file "legendaDataset.txt". Queste funzioni accettano due argomenti, che sono sempre gli stessi: l'indice della replicazione (job_index), che identifica in modo univoco un dataset nell'esperimento di simulazione, e il numero di simulazioni da eseguire (sim_number, SEMPRE = 100, anche quando vogliono essere eseguite 50 replicazioni). Questo argomento non va mai modificato. 
- Scelta la funzione, questa deve essere sostituita nella riga 50 del file "SLURM_SimulationsLight.R", in modo che: data <- get_dataset_scelto() (gli argomenti non vanno toccati). Permette allo script di caricare i dati corretti. La funzione crea ogni dataset delle replicazioni in modo autonomo, quindi "get_dataset_scelto()" produce il dataset di ciascuna iterazione, di volta in volta.
- Una volta scelto il dataset è necessario definire le specifiche per stimare i modelli, componenti variabili ma in codifica fissa. Per ridurre i tempi infatti, vengono utilizzate, per ciascuna simulazione, configurazioni specifiche per ciascun modello. Per lo stimatore ridge non c'é nulla da fare. Per la foresta casuale è necessario definire la griglia per la regolazione dello split ottimale (dalla riga 327 scegliere un mtry = () appropriato, mentre il numero di alberi, 120, non è mai stato modificato). Per il gradient boosting è necessario indicare il numero massimo di alberi (variabile "num.trees", nelle funzioni "tree.num()" e "gb_estimates_par()", che deve assumere lo stesso valore). La scelta è importante, in quanto solo quella corretta permette di riprodurre i risultati. Nel file "SLURM_DataCreateFunctionsLight.R" sono comunque indicate, come commetto alle singole funzioni (le funzioni "get_dataset_()"), le specifiche utilizzate nelle simulazioni, che permettono di riprodurre i risultati.
- Collocare i 3 file "SLURM_Base_Functions.R", "SLURM_DataCreateFunctionsLight.R" e "SLURM_SimulationsLight.R" nella stessa directory, in Calculus. Le funzioni nei primi due vengono caricate dal terzo script quando viene lanciato.
- Nella stessa directory creare una cartella per salvare gli output (i percorsi sono in codifica fissa). Creare una cartella "sim.out", contenente due distinte sottocartelle, "models" e "results". I risultati vengono quindi separati nelle due cartelle.

Queste sono le operazioni preliminari per lanciare la simulazione desiderata, e ottenere gli stessi risultati riportati nel lavoro.

Per lanciare la simulazione possono essere seguiti i seguenti passaggi:
- Prendere il file bash (.sh) "simulations.sh", disponibile nella cartella "CALCULUS". È necessario modificare i percorsi in cui printare l'esito della procedura, un'operazione da poco e irrilevante.
- Specificare il percorso dove pescare il container ("MyCustomImage.sif"), disponibile nella cartella "CALCULUS". Modificare il percorso all'interno del file bash perché lo trovi correttamente (nella riga di "srun singularity exec: 'percorso container'").
- Modificare il comando, nel file bash, "##SBATCH --array=1-100%2", sostituendo a 100 un altro valore, permette di cambiare il numero di replicazioni eseguite. Il numero massimo è comunque 100. QUESTO È IL VALORE CHE DEVE ESSERE MODIFICATO NEI CASI LE SIMULAZIONI UTILIZZATE FOSSERO MENO DI 100, E NIENT'ALTRO.
- Lanciare il file bash, nella directory in cui sono presenti gli script ".R", tramite "sbatch simulations.sh".

La simulazione è stata lanciata! I risultati vengono salvati un po' alla volta nella directory "sim.out".


## Rete Neurale (nn) ##
La procedura per applicare il test di degradazione temporale con la rete neurale è più complessa. Vengono infatti utilizzati degli script in Python (.py).
I dataset sono però creati utilizzando le funzioni "get_dataset_()", scritte in R.

La procedura si articola quindi in 3 fasi. La prima consiste nel preparare gli script necessari e collocarli nella stessa directory, come fatto in precedenza.

La seconda consiste nel pre-creare i dataset. Non potendo pescarli direttamente come fa lo script "SLURM_SimulationsLight.R", questi devono essere pre-creati e
salvati con estensione ".feather", che permette a Python di leggerli. Questo fa in modo che tutti i modelli utilizzino veramente gli stessi risultati.

La terza consiste nel lanciare lo script e salvare i risultati.

Prima fase, preparazione degli script:
- Lo script per produrre i risultati è "SLURM_NNLight.py". È necessario individuare la simulazione che si vuole eseguire (dal file "SLURM_DataCreateFunctionsLight.R").
- Anche qui è necessario scegliere la configurazione corretta per la rete: le quantità che possono variare, ma che sono in codifica fissa, sono molteplici. "reps" (riga 127, default = 1), batch_size (riga 168, default = 64, argomento della funzione "model_nn.fit()") sono pensate per poter cambiare, ma nella pratica non cambiano mai (come il numero di alberi della foresta casuale). Le modifiche principali riguardano il numero di nodi latenti per strato, due possibilità (50 o 200); queste vanno specificate nelle righe 129 e 130, una delle due va scelta (size = [50,50,50] o size = [200,200,200]). Il secondo parametro più importante è quello di patience (riga 169, argomento della funzione "model_nn.fit()"), di default è 2 (ma in alcuni casi è stato aumentato). Come capire quali utilizzare? I commenti delle funzioni "get_dataset_()" indicano la configurazione utilizzata per la specifica simulazione. Scegliere quella corretta rende i risultati riproducibili. Se non vengono indicate, le scelte sono quelle di default (size = [50,50,50] e patience = 2).
- Una volta scelta la simulazione da eseguire e fissate le specifiche per la rete, è necessario collocare i file nella stessa directory utilizzata per gli altri modelli. In questa directory, oltre a "SLURM_NNLight.py", è necessario includere "SLURM_DataCreateFunctionsLight.R", "SLURM_Base_Functions.R" e "dataset_Feathering.R". I risultati possono essere salvati in modo analogo nella cartella "sim.out", distinguendo gli oggetti "models" e "results".

Una volta conclusi i passaggi è possibile passare alla creazione dei dataset. Gli script coinvolti in questa fare sono 4: "SLURM_DataCreateFunctionsLight.R", "SLURM_Base_Functions.R", "dataset_Feathering.R" (che genera i dataset usando le funzioni del primo file) e "pyDataFeather.sh" (che lancia "dataset_Feathering.R").

La procedura è la seguente:
- Scegliere la simulazione da eseguire e prendere la funzione corrispondente dal primo script.
- In "dataset_Feathering.R" sostituire la funzione alla riga 53, senza toccare gli argomenti.
- Creare una cartella "pyData" nella directory di lavoro, in cui salvare i dataset creati.
- Per lanciare lo script usare il file bash "pyDataFeather.sh", per il quale deve essere specificato il percorso al container "MyCustomImage.sif" (container per R).

Vengono quindi prodotti 100 dataset e salvati nella cartella "pyData". Adesso che sono disponibili i dati è possibile lanciare lo script "SLURM_NNLight.py", che pesca i dati da lì. Questo viene fatto lanciando il file bash "simulationsPy.sh", nel quale è necessario specificare due percorsi:
- Il primo è ad un file testuale, "env_file", che permette di fissare un seed nascosto ("PYTHONHASHSEED") che rende i risultati riproducibili (è disponibile nella cartella "CALCULUS").
- Il secondo è al container con Pyhton, "pythonImageCPU.sif", disponibile sempre in "CALCULUS".
La riga in cui specificare i percorsi è la seguente:
"srun singularity exec --env-file 'percorso a env_file' 'percorso a pythonImageCPU.sif' python ..."

Una volta fatto la simulazione può essere lanciata tramite "sbatch simulationsPy.sh" I risultati vengono creati un po' alla volta nella cartella "sim.out", e vanno ad aggiungersi a quelli già presenti provenienti dagli altri modelli.

# SINTETIZZARE I RISULTATI E PRODURRE I GRAFICI #
I risultati ottenuti sono separati in un grande insieme di oggetti, 2 per ciascun modello e replicazione, separati nelle cartelle "sim.out/models" e "sim.out/results".

Questi devono essere scaricati e salvati da qualche parte. Nella cartella in cui vengono salvati è inoltre necessario creare una cartella aggiuntiva, allo stesso livello, chiamata "synthesis", che andrà a contenere i risultati combinati, sia sotto forma di oggetti (in R) che di grafici.

Le istruzioni per utilizzare gli script che sintetizzano i risultati e producono i grafici sono riportate nel file "README" della cartella "CODICE/sintesiRisultati".


# ECCEZIONI #
Le procedure descritte in precedenza per lanciare le simulazioni sono sufficienti nella maggior parte dei casi. In alcuni casi la procedura da seguire può differire, e ripercorrere passo-passo quanto fatto nel lavoro è l'unico modo per riprodurre esattamente i risultati.

































