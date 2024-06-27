La cartella contiene gli script specifici per sintetizzare i risultati e produrre i grafici. Queste operazioni non vengono eseguite su calculus, e prevedono quindi di aver scaricato la cartella sim.out con i risultati.

I risultati ottenuti sono separati in due cartelle, "models" e "results", allo stesso livello creare la cartella synthesis. 

## SINTETIZZARE I RISULTATI ##

Lo script "SLURM_Post_Sim_Synt.R" crea degli oggetti con la sintesi dei risultati e li salva nella cartella "synthesis".

Lo script "SLURM_Sim_Plots.R" utilizza quei risultati per produrre i grafici contenenti l'R^2 predittivo iniziale e la combinazione dei grafici di "AI Aging".

Lo script "SLURM_Hist.R" non si basa invece sulla sintesi fatta da "SLURM_Post_Sim_Synt.R". Permette però di creare i grafici contenti i livelli medi del terzo quartile e le variazioni di errore relativo.

Lo script "SLURM_qMSE_Plots.R" fa nuovamente una sintesi separata degli oggetti iniziali, ma produce i grafici contenenti gli andamenti degli MSE mediani. Permette di creare anche i grafici degli andamenti del terzo quartile dell'MSE.
