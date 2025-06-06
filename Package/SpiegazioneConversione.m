(* ::Package:: *)

(* :Title: Tutorial *)
(* :Context: Tutorial` *)
(* :Author: Matteo Rontini, Daniele Russo *)
(* :Version: 1.6 *)
(* :Date: 2025-05-09 *)

(* :Summary: 
   Questo pacchetto crea il tutorial per ripassare le conversioni in varie basi (2-8-10-16) prima di giocare.
*)

(* :Copyright: A colpi di Bit (C) 2025 *)
(* :Keywords: battaglia navale, gioco, interfaccia *)
(* :Requirements: Mathematica 12.0+, Util`, Interaction` *)

BeginPackage["SpiegazioneConversione`", {"Util`","Interaction`"}]
(* Inizio del package Tutorial, specificando che importa anche il package Util` *)

createExplainationConversion::usage = "createExplainationConversion[] inserisce il tutorial nel notebook corrente."
(* Definisce la descrizione della built-in createExplainationConversion *)

introSection::usage = "introSection[] crea l'intestazione e il sommario."
(* Definisce la descrizione della built-in introSection *)

binToDecSection::usage = "binToDecSection[] crea la sezione conversione da binario a decimale."
(* Definisce la descrizione della built-in binToDecSection *)

octToDecSection::usage = "octToDecSection[] crea la sezione conversione da ottale a decimale."
(* Definisce la descrizione della built-in octToDecSection *)

hexToDecSection::usage = "hexToDecSection[] crea la sezione conversione da esadecimale a decimale."
(* Definisce la descrizione della built-in hexToDecSection *)

decToBinSection::usage = "decToBinSection[] crea la sezione conversione da decimale a binario."
(* Definisce la descrizione della built-in decToBinSection *)

decToOctSection::usage = "decToOctSection[] crea la sezione conversione da decimale a ottale."
(* Definisce la descrizione della built-in decToOctSection *)


decToHexSection::usage = "decToHexSection[] crea la sezione conversione da decimale a esadecimale."
(* Definisce la descrizione della built-in decToHexSection *)

Begin["`Private`"]
(* Inizio della sezione privata del package *)

(* =================== SEZIONI =================== *)
(* Sezione che contiene le funzioni per generare ciascuna parte del tutorial *)

        
introSection[] := {
(* Definizione della built-in introSection:
   - Non ha argomenti
   - Restituisce un blocco grafico che rappresenta l'introduzione generale del tutorial
*)

  TextCell[
    "Tutorial: Conversioni tra Sistemi Numerici", 
    "Section", 
    FontSize -> 22, 
    FontWeight -> "Bold", 
    FontColor -> RGBColor[0.1, 0.1, 0.7], 
    TextAlignment -> Center, CellTags->"Tutorial0"
  ],
  (* Crea il titolo principale del tutorial:
     - Tipo: "Section" (titolo principale)
     - FontSize 22
     - FontWeight "Bold" (grassetto)
     - FontColor blu (RGBColor[0.1, 0.1, 0.7])
     - Allineamento al centro della pagina
  *)

  Spacer[20],
  (* Spazio verticale di 20 punti *)

  TextCell[
    "Questo tutorial ti guider\[AGrave] attraverso le conversioni tra diversi sistemi numerici: binario, ottale, esadecimale e decimale.\n\nOgni sistema numerico posizionale si basa su potenze della base stessa. In base 10 (decimale), la cifra pi\[UGrave] a sinistra vale 10\:2070, la successiva 10\.b9, e cos\[IGrave] via. In binario vale 2\:2070, 2\.b9, 2\.b2, etc.\nQuesto concetto si generalizza per qualunque base b.", 
    FontSize -> 14
  ],
  (* Crea un paragrafo descrittivo *)

  Spacer[10]
  (* Spazio verticale di 10 punti *)
};
(* Fine della built-in introSection *)


binToDecSection[] := {
(* Definizione della built-in binToDecSection che restituisce il blocco grafico per la sezione del tutorial dedicata alla conversione da binario a decimale *)

  TextCell["Conversione da Binario a Decimale", "Subsection",
    FontSize -> 18, FontWeight -> "Bold", 
    FontColor -> RGBColor[1, 0, 0], 
    TextAlignment -> Left],
  (* Titolo della sezione:
     - Tipo: sottosezione
     - FontSize 18, in grassetto, colore rosso
     - Allineamento a sinistra
  *)
 (* Spazio verticale di 10 punti *)
  Spacer[10],
  (* Riquadro che contiene la spiegazione teorica *)
  Framed[TextCell[
  Row[{"Per convertire un numero binario in decimale, moltiplica ciascuna cifra per la potenza di 2 corrispondente alla sua posizione e somma i risultati ", 
  Button["[2]", NotebookLocate["Bibliografia0"], 
     Appearance -> "Frameless", Evaluator -> Automatic, 
     BaseStyle -> {FontSize -> 12}], "."}], FontSize -> 12], 
     Background -> Lighter[Gray, 0.9]],
  (* Testo esplicativo:
     - Descrive come convertire da binario a decimale
     - FontSize 12
     - Incorniciato in un riquadro con sfondo grigio chiaro
  *)

  Spacer[10],
  (* Spazio verticale di 10 punti *)

  Framed[
    (* Riquadro che racchiude tutta la parte interattiva *)

    DynamicModule[{d = ""},
    (* DynamicModule: ambiente dinamico con variabile locale d (inizializzata come stringa vuota)
       - d serve per catturare e gestire l'input utente in tempo reale
    *)

    Column[{
      (* Colonna: imposta il layout verticale dei vari componenti interattivi *)

      TextCell[
        "Esempio interattivo (inserisci un numero e premi Invio)", 
        FontSize -> 14, FontWeight -> "Bold"
      ],
      (* Testo guida sopra il campo input:
         - Font size 14
         - In grassetto
         - Indica all'utente cosa fare
      *)

      Grid[{
        {
          TextCell["Inserisci un numero binario:"],
          (* Etichetta che accompagna il campo di input, per spiegare cosa deve essere inserito *)

          InputField[Dynamic[d], String]
          (* Campo input dinamico:
             - Dynamic[d]: collega il contenuto del campo alla variabile d
             - String: gestisce l'input come stringa di testo
          *)
        }
      }],
      (* Griglia con una riga e due colonne:
         - Prima colonna: testo statico di etichetta
         - Seconda colonna: campo input dinamico
      *)

      Dynamic[
        (* Blocca dinamico che si aggiorna automaticamente ogni volta che l'utente modifica l'input *)

        conversionToDec[2, d]
        (* Chiama la built-in conversionToDec:
           - base = 2 (binario)
           - d = numero inserito dall'utente
           - La built-in mostrer\[AGrave] la conversione passo passo, con tabella e risultato finale
        *)
      ]
      (* Mostra dinamicamente la conversione oppure un messaggio d'errore se l'input \[EGrave] invalido *)
    }]
    ],
    Background -> Lighter[Gray, 0.95]
    (* Sfondo grigio molto chiaro per la parte interattiva *)
  ]
  (* Chiude il Framed della parte interattiva *)
};
(* Fine della built-in binToDecSection *)


octToDecSection[] := {
(* Definizione della built-in octToDecSection che restituisce il blocco grafico della sezione dedicata alla conversione da ottale a decimale *)

  TextCell["Conversione da Ottale a Decimale", "Subsection",
    FontSize -> 18, FontWeight -> "Bold", 
    FontColor -> RGBColor[1, 0, 0], 
    TextAlignment -> Left],
  (* Titolo della sezione:
     - Tipo: sottosezione ("Subsection")
     - Font size 18, in grassetto, colore rosso
     - Allineamento a sinistra
  *)

  Spacer[10],
  (* Spazio verticale di 10 punti *)

    Framed[TextCell[
     Row[{"Per convertire un numero ottale in decimale, moltiplica ciascuna cifra per la potenza di 8 corrispondente alla sua posizione e somma i risultati ", 
     Button["[3]", NotebookLocate["Bibliografia0"], 
     Appearance -> "Frameless", Evaluator -> Automatic, 
     BaseStyle -> {FontSize -> 12}], "."}], FontSize -> 12], 
     Background -> Lighter[Gray, 0.9]],
  (* Testo esplicativo:
     - Descrive come convertire da ottale a decimale (cifra \[Times] potenza di 8)
     - Font size 12
     - Incorniciato con sfondo grigio chiaro
  *)

  Spacer[10],
  (* Spazio verticale di 10 punti *)

  Framed[
    (* Riquadro che racchiude tutta la parte interattiva *)

    DynamicModule[{d = ""},
    (* DynamicModule: ambiente locale dinamico con variabile d inizializzata come stringa vuota
       - d servir\[AGrave] per catturare l'input utente
    *)

    Column[{
      (* Colonna: organizza i vari elementi della parte interattiva in verticale *)

      TextCell[
        "Esempio interattivo (inserisci un numero e premi Invio)", 
        FontSize -> 14, FontWeight -> "Bold"
      ],
      (* Testo guida sopra il campo input:
         - Font size 14
         - In grassetto
         - Dice all'utente cosa fare
      *)

      Grid[{
        {
          TextCell["Inserisci un numero ottale:"],
          (* Etichetta accanto al campo input che indica cosa inserire *)

          InputField[Dynamic[d], String]
          (* Campo input dinamico:
             - Dynamic[d]: collega il campo input alla variabile d
             - String: indica che l'input \[EGrave] trattato come stringa (testo)
          *)
        }
      }],
      (* Griglia con una riga e due colonne:
         - Prima colonna: etichetta
         - Seconda colonna: campo di input
      *)

      Dynamic[
        (* Blocca dinamico che valuta e aggiorna automaticamente quando l'utente cambia il contenuto di d *)

        conversionToDec[8, d]
        (* Chiama la built-in conversionToDec con:
           - base = 8 (ottale)
           - d = il numero inserito dall'utente
           - La built-in mostrer\[AGrave] la conversione passo passo, con tabella e risultato finale
        *)
      ]
      (* Visualizza dinamicamente il risultato della conversione oppure un messaggio d'errore se input non valido *)
    }]
    ],
    Background -> Lighter[Gray, 0.95]
    (* Sfondo ancora pi\[UGrave] chiaro per la parte interattiva *)
  ]
  (* Chiude il Framed della parte interattiva *)
};
(* Fine della built-in octToDecSection *)


hexToDecSection[] := {
(* Definizione della built-in hexToDecSection che non ha argomenti e restituisce il blocco grafico per la sezione dedicata alla conversione da esadecimale a decimale *)

  TextCell["Conversione da Esadecimale a Decimale", "Subsection",
    FontSize -> 18, FontWeight -> "Bold", 
    FontColor -> RGBColor[1, 0, 0], 
    TextAlignment -> Left],
  (* Crea il titolo della sezione:
     - Tipo: sottosezione ("Subsection")
     - Font size 18, grassetto, colore rosso
     - Allineamento a sinistra
  *)

  Spacer[10],
  (* Spazio verticale di 10 punti *)

  Framed[TextCell[
  Row[{"Il sistema esadecimale usa 16 simboli (0\[Dash]9 e A\[Dash]F). Per convertire in decimale, sostituisci le lettere con i valori 10\[Dash]15, poi moltiplica ciascuna cifra per 16 elevato alla sua posizione e somma ", 
  Button["[3]", NotebookLocate["Bibliografia0"], 
     Appearance -> "Frameless", Evaluator -> Automatic, 
     BaseStyle -> {FontSize -> 12}], "."}], FontSize -> 12], 
     Background -> Lighter[Gray, 0.9]],
  (* Contenuto esplicativo:
     - Spiega il sistema esadecimale e come convertire in decimale
     - Font size 12
     - Incorniciato in un riquadro con sfondo grigio chiaro
  *)

  Spacer[10],
  (* Spazio verticale di 10 punti *)

  Framed[
    (* Riquadro che racchiude la parte interattiva *)

    DynamicModule[{h = ""},
    (* DynamicModule crea un ambiente locale con una variabile dinamica h (inizializzata a stringa vuota)
       - h serve per catturare l'input dinamico dell'utente
    *)

    Column[{
      (* Colonna che organizza i componenti verticalmente *)

      TextCell[
        "Esempio interattivo (inserisci un numero e premi Invio)", 
        FontSize -> 14, FontWeight -> "Bold"
      ],
      (* Testo guida sopra il campo input:
         - Font size 14
         - Grassetto
         - Spiega all'utente cosa deve fare
      *)

      Grid[{
        {
          TextCell["Inserisci un numero esadecimale:"],
          (* Etichetta statica che dice all'utente di inserire un numero esadecimale *)

          InputField[Dynamic[h], String]
          (* Campo input dinamico:
             - Dynamic[h]: collega il campo alla variabile dinamica h
             - String: gestisce l'input come testo
          *)
        }
      }],
      (* Griglia a una riga e due colonne:
         - Prima colonna: etichetta
         - Seconda colonna: campo input
      *)

      Dynamic[
            conversionToDec[16, h]
            (* Chiama la built-in conversionToDec:
           - base = 16 (esadecimale)
           - h \[EGrave] il numero inserito dall'utente
           - La built-in mostrer\[AGrave] la conversione passo passo, con tabella e risultato finale
        *)    
        ]

      (* Questo blocco mostra in tempo reale la conversione oppure un messaggio d'errore se l'input non \[EGrave] valido *)
    }]
    ],
    Background -> Lighter[Gray, 0.95]
    (* Sfondo pi\[UGrave] chiaro per evidenziare la parte interattiva *)
  ]
  (* Chiude il Framed della parte interattiva *)
};
(* Fine della built-in hexToDecSection *)


decToBinSection[] := {
  (* Definizione della built-in decToBinSection che restituisce il blocco grafico per la sezione dedicata alla conversione da decimale a binario *)

    TextCell["Conversione da Decimale a Binario", "Subsection",
      FontSize -> 18, FontWeight -> "Bold", 
      FontColor -> RGBColor[1, 0, 0], 
      TextAlignment -> Left],
  (* Titolo della sezione:
     - Tipo: sottosezione ("Subsection")
     - Font size 18, in grassetto, colore rosso
     - Allineamento a sinistra
  *)
    Spacer[10],
    (* Spazio verticale di 10 punti *)

Framed[TextCell[
  Row[{"Per convertire un numero decimale in binario, dividilo ripetutamente per 2 e raccogli i resti. Leggi i resti dal basso verso l'alto ", 
    Button["[2]", NotebookLocate["Bibliografia0"], 
     Appearance -> "Frameless", Evaluator -> Automatic, 
     BaseStyle -> {FontSize -> 12}], "."}], FontSize -> 12], 
     Background -> Lighter[Gray, 0.9]],
    (* Testo esplicativo:
        - Descrive il metodo delle divisioni successive per convertire da decimale a binario
        - Include un pulsante di riferimento alla bibliografia
        - Font size 12
        - Incorniciato in un riquadro con sfondo grigio chiaro
      *)
    Spacer[10],
  (* Spazio verticale di 10 punti *)

    Framed[
      (* Riquadro che racchiude tutta la parte interattiva *)

      DynamicModule[{d = ""},
      (* DynamicModule: ambiente dinamico con variabile locale d (inizializzata come stringa vuota)
       - d serve per catturare e gestire l'input utente in tempo reale
      *)
      Column[{
        (* Colonna: imposta il layout verticale dei vari componenti interattivi *)
        TextCell[
          "Esempio interattivo (inserisci un numero e premi Invio)", 
          FontSize -> 14, FontWeight -> "Bold"
        ],
        (* Testo guida sopra il campo input:
         - Font size 14
         - In grassetto
         - Indica all'utente cosa fare
        *)

        Grid[{
          {
            TextCell["Inserisci un numero decimale:"],
            (* Etichetta che accompagna il campo di input, per spiegare cosa deve essere inserito *)
            InputField[Dynamic[d], String]
            (* Campo input dinamico:
             - Dynamic[d]: collega il contenuto del campo alla variabile d
             - String: gestisce l'input come stringa di testo
            *)
          }
        }],
        (* Griglia con una riga e due colonne:
         - Prima colonna: testo statico di etichetta
         - Seconda colonna: campo input dinamico
        *)


        Dynamic[
          (* Blocco dinamico che si aggiorna automaticamente quando l'utente modifica l'input *)
          If[isSeed[d],
            (* Verifica se l'input \[EGrave] un numero valido (usando la funzione isSeed dal package Util che prende una stringa e ci dice se \[EGrave] un numero) -  \[EGrave] stata riciclate per non duplicare codice *)
            Module[{numD = ToExpression[d]},
              (* Converte la stringa in un numero *)
              If[NumericQ[numD] && numD >= 0,
                (* Verifica che sia un numero positivo o zero *)
                conversionFromDec[2, numD],
                (* Chiama la funzione di conversione da decimale con:
                 - base = 2 (binario)
                 - numD = il numero inserito convertito in valore numerico
                *)
                "Inserisci un numero decimale valido (>= 0)"
                (* Messaggio di errore se il numero non \[EGrave] valido o \[EGrave] negativo *)
              ]
            ],
            "Inserisci un numero decimale valido (>= 0)"
            (* Messaggio di errore se l'input non \[EGrave] un numero *)
          ]
        ]
      (* Mostra dinamicamente la conversione o un messaggio d'errore *)
      }]
      ],
      Background -> Lighter[Gray, 0.95]
      (* Sfondo grigio molto chiaro per la parte interattiva *)
    ]
    (* Chiude il Framed della parte interattiva *)
  };
(* Fine della built-in decToBinSection *)


decToOctSection[] := {
  (* Definizione della built-in decToOctSection che restituisce il blocco grafico per la sezione dedicata alla conversione da decimale a ottale *)

  TextCell["Conversione da Decimale a Ottale", "Subsection",
    FontSize -> 18, FontWeight -> "Bold", 
    FontColor -> RGBColor[1, 0, 0], 
    TextAlignment -> Left],
    (* Titolo della sezione:
     - Tipo: sottosezione ("Subsection")
     - Font size 18, in grassetto, colore rosso
     - Allineamento a sinistra
    *)

    Spacer[10],
    (* Spazio verticale di 10 punti *)

  Framed[TextCell[
    Row[{"Per convertire un numero decimale in ottale, dividilo ripetutamente per 8 e raccogli i resti, oppure converti prima in binario e poi raggruppa le cifre in gruppi di 3.\nQuesto \[EGrave] possibile perch\[EGrave] ogni cifra ottale si rappresenta esattamente con 3 bit, cio\[EGrave] ", Superscript[2, 3], " = 8.\n\nNell'esempio di seguito adotteremo il secondo metodo ", 
      Button["[3]", NotebookLocate["Bibliografia0"], 
      Appearance -> "Frameless", Evaluator -> Automatic, 
      BaseStyle -> {FontSize -> 12}], "."}], FontSize -> 12], 
      Background -> Lighter[Gray, 0.9]],
      (* Testo esplicativo:
        - Descrive i due metodi di conversione da decimale a ottale:
          1. Divisioni successive per 8
          2. Conversione in binario e raggruppamento in gruppi di 3 bit
        - Spiega che 2^3 = 8, quindi ogni cifra ottale corrisponde a 3 bit
        - Include un pulsante di riferimento alla bibliografia
        - Font size 12
        - Incorniciato con sfondo grigio chiaro
      *)
      Spacer[10],
      (* Spazio verticale di 10 punti *)

      Framed[
        (* Riquadro che racchiude la parte interattiva *)
        DynamicModule[{d = ""},
        (* DynamicModule: ambiente dinamico con variabile locale d (stringa vuota)
          - d serve per catturare l'input utente
        *)
        Column[{
          (* Colonna: organizza verticalmente i componenti interattivi *)
          TextCell[
            "Esempio interattivo (inserisci un numero e premi Invio)", 
            FontSize -> 14, FontWeight -> "Bold"
          ],
          (* Testo guida sopra il campo input:
            - Font size 14
            - In grassetto
            - Indica all'utente cosa fare
            *)
          Grid[{
            {
              TextCell["Inserisci un numero decimale:"],
              (* Etichetta accanto al campo input *)
              InputField[Dynamic[d], String]
              (* Campo input dinamico:
                - Dynamic[d]: collega il campo alla variabile d
                - String: gestisce l'input come stringa di testo
              *)
            }
          }],
          (* Griglia con una riga e due colonne:
            - Prima colonna: etichetta
            - Seconda colonna: campo input
          *)

          Dynamic[
            (* Blocco dinamico che si aggiorna quando l'utente modifica l'input *)
            If[isSeed[d],
            (* Verifica se l'input \[EGrave] un numero valido (usando la funzione isSeed dal package Util che prende una stringa e ci dice se \[EGrave] un numero) -  \[EGrave] stata riciclate per non duplicare codice *)
              Module[{numD = ToExpression[d]},
                (* Converte la stringa input in un numero *)
                If[NumericQ[numD] && numD >= 0,
                  (* Verifica che sia un numero positivo o zero *)
                  conversionFromDec[8, numD],
                  (* Chiama la funzione di conversione da decimale con:
                    - base = 8 (ottale)
                    - numD = il numero inserito convertito
                  *)
                  "Inserisci un numero decimale valido (>= 0)"
                  (* Messaggio di errore se il numero non \[EGrave] valido o \[EGrave] negativo *)
                ]
              ],
              "Inserisci un numero decimale valido (>= 0)"
              (* Messaggio di errore se l'input non \[EGrave] un numero *)
            ]
          ]
          (* Mostra dinamicamente la conversione o un messaggio d'errore *)
        }]
        ],
        Background -> Lighter[Gray, 0.95]
        (* Sfondo grigio molto chiaro per la parte interattiva *)
      ]
      (* Chiude il Framed della parte interattiva *)
  };
(* Fine della built-in decToOctSection *)

decToHexSection[] := {
(* Definizione della built-in decToHexSection che restituisce il blocco grafico per la sezione dedicata alla conversione da decimale a esadecimale *)
  TextCell["Conversione da Decimale a Esadecimale", "Subsection",
    FontSize -> 18, FontWeight -> "Bold", 
    FontColor -> RGBColor[1, 0, 0], 
    TextAlignment -> Left],
  (* Titolo della sezione:
      - Tipo: sottosezione ("Subsection")
      - Font size 18, in grassetto, colore rosso
      - Allineamento a sinistra
    *)
    Spacer[10],
  (* Spazio verticale di 10 punti *)

  Framed[TextCell[
    Row[{"Per convertire un numero decimale in esadecimale, dividilo ripetutamente per 16 e raccogli i resti, oppure converti prima in binario e raggruppa le cifre in gruppi di 4.\nQuesto \[EGrave] possibile perch\[EGrave] ogni cifra esadecimale si rappresenta esattamente con 4 bit, cio\[EGrave] ", Superscript[2, 4], 
      " = 16.\n\nNell'esempio di seguito adotteremo il secondo metodo ",
      Button["[4]", NotebookLocate["Bibliografia0"], 
      Appearance -> "Frameless", Evaluator -> Automatic, 
      BaseStyle -> {FontSize -> 12}], "."}], FontSize -> 12], 
      Background -> Lighter[Gray, 0.9]],
      (* Testo esplicativo:
        - Descrive i due metodi di conversione da decimale a esadecimale:
          1. Divisioni successive per 16
          2. Conversione in binario e raggruppamento in gruppi di 4 bit
        - Spiega che 2^4 = 16, quindi ogni cifra esadecimale corrisponde a 4 bit
        - Include un pulsante di riferimento alla bibliografia
        - Font size 12
        - Incorniciato con sfondo grigio chiaro
      *)
      Spacer[10],
      (* Spazio verticale di 10 punti *)

      Framed[
        (* Riquadro che racchiude la parte interattiva *)
        DynamicModule[{d = ""},
      (* DynamicModule: ambiente dinamico con variabile locale d (stringa vuota)
        - d serve per catturare l'input utente
      *)
        Column[{
          (* Colonna: organizza verticalmente i componenti interattivi *)
          TextCell[
            "Esempio interattivo (inserisci un numero e premi Invio)", 
            FontSize -> 14, FontWeight -> "Bold"
          ],
        (* Testo guida sopra il campo input:
            - Font size 14
            - In grassetto
            - Indica all'utente cosa fare
        *)
          Grid[{
            {
              TextCell["Inserisci un numero decimale:"],
              (* Etichetta accanto al campo input *)
              InputField[Dynamic[d], String]
              (* Campo input dinamico:
                  - Dynamic[d]: collega il campo alla variabile d
                  - String: gestisce l'input come stringa di testo
              *)
            }
          }],
        (* Griglia con una riga e due colonne:
            - Prima colonna: etichetta
            - Seconda colonna: campo input
        *)

          Dynamic[
            (* Blocco dinamico che si aggiorna quando l'utente modifica l'input *)
            If[isSeed[d],
              (* Verifica se l'input \[EGrave] un numero valido (usando la funzione isSeed dal package Util che prende una stringa e ci dice se \[EGrave] un numero) -  \[EGrave] stata riciclate per non duplicare codice *)
              Module[{numD = ToExpression[d]},
                (* Converte la stringa input in un numero *)
                If[NumericQ[numD] && numD >= 0,
                  (* Verifica che sia un numero positivo o zero *)
                  conversionFromDec[16, numD],
                  (* Chiama la funzione di conversione da decimale con:
                      - base = 16 (esadecimale)
                      - numD = il numero inserito convertito
                      - La funzione mostrer\[AGrave] la conversione passo passo
                    *)
                  "Inserisci un numero decimale valido (>= 0)"
                (* Messaggio di errore se il numero non \[EGrave] valido o \[EGrave] negativo *)
                ]
              ],
              "Inserisci un numero decimale valido (>= 0)"
              (* Messaggio di errore se l'input non \[EGrave] un numero *)
            ]
          ]
          (* Mostra dinamicamente la conversione o un messaggio d'errore *)
        }]
        ],
        Background -> Lighter[Gray, 0.95]
        (* Sfondo grigio molto chiaro per la parte interattiva *)
      ]
      (* Chiude il Framed della parte interattiva *)
  };
(* Fine della built-in decToHexSection *)



(* =================== FUNZIONE PRINCIPALE =================== *)
(* Definizione della built-in principale che unisce tutte le sezioni *)
createExplainationConversion[] := Module[{},
(* Definizione della built-in createExplainationConversion che non ha argomenti:
   - Usa Module per definire variabili locali
   - Non abbiamo pi\[UGrave] bisogno della variabile 'sezioni' perch\[EAcute] creiamo celle separate
*)
  (* Creiamo una cella per ciascuna sezione separatamente *)
  
  (* Cella iniziale vuota con tag *)
  
  (* Cella per la sezione introduttiva *)
  CellPrint[Cell[
    BoxData[ToBoxes[Style[Column[introSection[], Alignment -> Left], FontFamily -> "Arial"]]],
    "Output",
    CellTags -> "Tutorial: Conversioni tra sistemi numerici"
  ]];
  
  (* Cella per la sezione binario a decimale *)
  CellPrint[Cell[
    BoxData[ToBoxes[Style[Column[binToDecSection[], Alignment -> Left], FontFamily -> "Arial"]]],
    "Output",
    CellTags -> "Conversione da Binario a Decimale"
  ]];
  
  (* Cella per la sezione ottale a decimale *)
  CellPrint[Cell[
    BoxData[ToBoxes[Style[Column[octToDecSection[], Alignment -> Left], FontFamily -> "Arial"]]],
    "Output",
    CellTags -> "Conversione da Ottale a Decimale"
  ]];
  
  (* Cella per la sezione esadecimale a decimale *)
  CellPrint[Cell[
    BoxData[ToBoxes[Style[Column[hexToDecSection[], Alignment -> Left], FontFamily -> "Arial"]]],
    "Output",
    CellTags -> "Conversione da Esadecimale a Decimale"
  ]];
  
  (* Cella per la sezione decimale a binario *)
  CellPrint[Cell[
    BoxData[ToBoxes[Style[Column[decToBinSection[], Alignment -> Left], FontFamily -> "Arial"]]],
    "Output",
    CellTags -> "Conversione da Decimale a Binario"
  ]];
  
  (* Cella per la sezione decimale a ottale *)
  CellPrint[Cell[
    BoxData[ToBoxes[Style[Column[decToOctSection[], Alignment -> Left], FontFamily -> "Arial"]]],
    "Output",
    CellTags -> "Conversione da Decimale a Ottale"
  ]];
  
  (* Cella per la sezione decimale a esadecimale *)
  CellPrint[Cell[
    BoxData[ToBoxes[Style[Column[decToHexSection[], Alignment -> Left], FontFamily -> "Arial"]]],
    "Output",
    CellTags -> "Conversione da Decimale a Esadecimale"
  ]];
  
  (* Non restituiamo nulla perch\[EAcute] abbiamo gi\[AGrave] stampato tutte le celle *)
  Null
];
(* Fine della built-in createExplainationConversion *)



End[]
(* Fine della sezione privata *)

EndPackage[]
(* Fine del package *)

