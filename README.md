Codis creats i emprats en l'elaboració del projecte final del Màster Interuniversitari en Estadística i Investigació Operativa UPC-UB (MESIO). En aquest repositori trobem dos arxius d'R i dos directoris, els quals detallem a continuació.


  - L'arxiu mesures.R conté la funció mesures, la qual donada un conjunt de dades retorna el resultat de les diferents mesures de rendiment estudiades (C-Index, AUC-ROC i IBS) pels diferents mètodes implementats, juntament amb el temps necessari per a la modelització, la predicció i el càlcul de les mesures de rendiment per cadascun d'aquests mètodes. 
  En particular els mètodes implementats són:
      - Model de Cox
      - Arbre únic de regressió
      - Agregació de boostraps
      - Bosc aleatori
      - Potenciació
      - Xarxes neuronals
      - Màquines de suport vectorial (regressió i rànquing)
      - Aprenentatge multitasca
   
  - L'arxiu influencia_proporcionalitat.R conté el codi necessari per a l'estudi de la qualitat de les dades, on n'estudiem la proporció de variables que són influents sobre el temps fins a l'esdeveniment i la proporció d'aquestes que verifiquen la propietat de proporcionalitat.

  - El directori "Simulació Conjunts", on recollim els scripts d'R emprats a l'hora de simular els conjunts de dades simulats.

  - El directori "Anàlisi Conjunts", on recollim els scripts d'R emprats a l'hora d'obtenir els resultats de les mesures de rendiment dels conjunts de dades simulats.
