# ANALISI SU DATASET REALI #

Questa cartella contiene i dati utilizzati e il codice relativi alle analisi del capitolo 2: la cartella "EnvironForCompactSimulations" contiene l'environment utilizzato per produrre i risultati (renv), i dati puliti, il codice e i risultati stessi.

I file disponibili sono i seguenti: 
-  La cartella "HOSPITAL DATA/HospitalDataDefinitivo.zip" contiene i dati utilizzati nel capitolo, su cui è applicato il "test" di degradazione temporale. Contengono perciò tutte le variabili definite. I dati originali sono disponibili al seguente link: https://medicalanalytics.group/operational-data-challenge/ (visitato l'ultima volta il 16/04/2024).
- "HOSPITAL DATA/Analisi_Preliminari_HospitalDataF#_Fixed.R" contiene il codice utilizzato per elaborare i dati della struttura F# e applicare il framework. Una      prima parte è dedicata alle operazioni di pulizia e trasformazione dei dati originali, scaricabili al link già riportato. Una seconda parte alla stima dei            quattro modelli sugli interi dati disponibili. Una terza parte è invece dedicata ad applicare il framework sul dataset F#. I percorsi per salvare i risultati         devono essere modificati.
- "HOSPITAL DATA/GraficiHospitalData.R" e "HOSPITAL DATA/confrontoStabilitaModelli.R" contengono il codice utilizzato per produrre i grafici e le tabelle del capitolo 2.
- "HOSPITAL DATA/F#_sim_out" contiene invece i risultati dei codici precedenti: "models" e "results" contengono gli oggetti in cui sono salvati i risultati del framework, entrambi necessari per produrre i grafici di "AI Aging". "models" contiene file di tipo models.attr.ridge (.rf, .gb, .nn), che contengono alcuni valori per ciascuno dei modelli stimati (tra cui la qualità iniziale), per ciascuna tipologia (indicati dal .nome), e allo stesso modo results."modello" contiene i valori di MSE calcolati sulle finestre mobili. Sono inoltre contenuti i grafici riportati nell'elaborato finale.

Ogni dataset ha un file "HOSPITAL DATA/Analisi_Preliminari_HospitalDataF#_Fixed.R" e una cartella "HOSPITAL DATA/F#_sim_out" a se.
