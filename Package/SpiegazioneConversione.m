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
(* :Requirements: Mathematica 12.0+, Util`, Battle`, Interaction` *)

BeginPackage["SpiegazioneConversione`", {"Util`"}]
(* Inizio del package Tutorial, specificando che importa anche il package Util` *)

CreateExplainationConversion::usage = "CreateExplainationConversion[] inserisce il tutorial nel notebook corrente."
(* Definisce la descrizione della built-in CreateTutorial *)

IntroSection::usage = "IntroSection[] crea l'intestazione e il sommario."
(* Definisce la descrizione della built-in IntroSection *)

BinToDecSection::usage = "BinToDecSection[] crea la sezione conversione da binario a decimale."
(* Definisce la descrizione della built-in BinToDecSection *)

OctToDecSection::usage = "OctToDecSection[] crea la sezione conversione da ottale a decimale."
(* Definisce la descrizione della built-in OctToDecSection *)

HexToDecSection::usage = "HexToDecSection[] crea la sezione conversione da esadecimale a decimale."
(* Definisce la descrizione della built-in HexToDecSection *)

DecToBinSection::usage = "DecToBinSection[] crea la sezione conversione da decimale a binario."
(* Definisce la descrizione della built-in DecToBinSection *)

DecToOctSection::usage = "DecToOctSection[] crea la sezione conversione da decimale a ottale."
(* Definisce la descrizione della built-in DecToOctSection *)


DecToHexSection::usage = "DecToHexSection[] crea la sezione conversione da decimale a esadecimale."
(* Definisce la descrizione della built-in DecToHexSection *)

Begin["`Private`"]
(* Inizio della sezione privata del package *)

(* =================== SEZIONI =================== *)
(* Sezione che contiene le funzioni per generare ciascuna parte del tutorial *)

IntroSection[] := {
(* Definizione della built-in IntroSection:
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
(* Fine della built-in IntroSection *)


BinToDecSection[] := {
(* Definizione della built-in BinToDecSection che restituisce il blocco grafico per la sezione del tutorial dedicata alla conversione da binario a decimale *)

  TextCell["Conversione da Binario a Decimale", "Subsection",
    FontSize -> 18, FontWeight -> "Bold", 
    FontColor -> RGBColor[1, 0, 0], 
    TextAlignment -> Left, CellTags->"Tutorial1"],
  (* Titolo della sezione:
     - Tipo: sottosezione
     - FontSize 18, in grassetto, colore rosso
     - Allineamento a sinistra
  *)

  Spacer[10],
  (* Spazio verticale di 10 punti *)

  Framed[
    (* Riquadro che contiene la spiegazione teorica *)
    Row[{
    TextCell[
      "Per convertire un numero binario in decimale, moltiplica ciascuna cifra per la potenza di 2 corrispondente alla sua posizione e somma i risultati. ",
      FontSize -> 12
    ], Button["[2]",
        NotebookLocate["Bibliografia0"],
        Appearance -> "Frameless",
        BaselinePosition -> Baseline,
        Evaluator -> Automatic,    
        BaseStyle -> {FontSize -> 12}
      ]}],
    Background -> Lighter[Gray, 0.9]
  ],
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
(* Fine della built-in BinToDecSection *)


OctToDecSection[] := {
(* Definizione della built-in OctToDecSection che restituisce il blocco grafico della sezione dedicata alla conversione da ottale a decimale *)

  TextCell["Conversione da Ottale a Decimale", "Subsection",
    FontSize -> 18, FontWeight -> "Bold", 
    FontColor -> RGBColor[1, 0, 0], 
    TextAlignment -> Left, CellTags->"Tutorial2"],
  (* Titolo della sezione:
     - Tipo: sottosezione ("Subsection")
     - Font size 18, in grassetto, colore rosso
     - Allineamento a sinistra
  *)

  Spacer[10],
  (* Spazio verticale di 10 punti *)

    Framed[
    (* Riquadro che racchiude il testo esplicativo *)
    Row[{
    TextCell[
      "Per convertire un numero ottale in decimale, moltiplica ciascuna cifra per la potenza di 8 corrispondente alla sua posizione e somma i risultati. ",
      FontSize -> 12
    ], Button["[3]",
        NotebookLocate["Bibliografia0"],
        Appearance -> "Frameless",
        BaselinePosition -> Baseline,
        Evaluator -> Automatic,
   BaseStyle -> {FontSize -> 12}
      ]}],
    Background -> Lighter[Gray, 0.9]
  ],
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
(* Fine della built-in OctToDecSection *)


HexToDecSection[] := {
(* Definizione della built-in HexToDecSection che non ha argomenti e restituisce il blocco grafico per la sezione dedicata alla conversione da esadecimale a decimale *)

  TextCell["Conversione da Esadecimale a Decimale", "Subsection",
    FontSize -> 18, FontWeight -> "Bold", 
    FontColor -> RGBColor[1, 0, 0], 
    TextAlignment -> Left, CellTags->"Tutorial3"],
  (* Crea il titolo della sezione:
     - Tipo: sottosezione ("Subsection")
     - Font size 18, grassetto, colore rosso
     - Allineamento a sinistra
  *)

  Spacer[10],
  (* Spazio verticale di 10 punti *)

  Framed[
    (* Riquadro per il testo esplicativo *)
    Row[{
    TextCell[
      "Il sistema esadecimale usa 16 simboli (0\[Dash]9 e A\[Dash]F) . Per convertire in decimale, sostituisci le lettere con i valori 10\[Dash]15, poi moltiplica ciascuna cifra per 16 elevato alla sua posizione e somma. ",
      FontSize -> 12
    ], Button["[4]",
        NotebookLocate["Bibliografia0"],
        Appearance -> "Frameless",
        BaselinePosition -> Baseline,
        Evaluator -> Automatic,
   BaseStyle -> {FontSize -> 12}
      ]}],
    Background -> Lighter[Gray, 0.9]
  ],
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
        (* Blocco dinamico che si aggiorna automaticamente quando l'utente inserisce qualcosa *)

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
(* Fine della built-in HexToDecSection *)


DecToBinSection[] := {
(* Definizione della built-in DecToBinSection, che non ha argomenti e restituisce la parte grafica della sezione tutorial per la conversione da decimale a binario *)

  TextCell["Conversione da Decimale a Binario", "Subsection",
    FontSize -> 18, FontWeight -> "Bold", 
    FontColor -> RGBColor[1, 0, 0], 
    TextAlignment -> Left, CellTags->"Tutorial4"],
  (* Titolo della sezione:
     - Tipo: sottosezione ("Subsection")
     - Font size 18, grassetto, colore rosso
     - Allineamento a sinistra
  *)

  Spacer[10],
  (* Spazio verticale di 10 punti *)

    Framed[
    (* Riquadro per il testo esplicativo *)
    Row[{
    TextCell[
      "Per convertire un numero decimale in binario, dividilo ripetutamente per 2 e raccogli i resti. Leggi i resti dal basso verso l'alto. ",
      FontSize -> 12
    ], Button["[2]",
        NotebookLocate["Bibliografia0"],
        Appearance -> "Frameless",
        BaselinePosition -> Baseline,
        Evaluator -> Automatic,
   BaseStyle -> {FontSize -> 12}
      ]}],
    Background -> Lighter[Gray, 0.9]
  ],
  (* Riquadro con il testo esplicativo:
     - Spiega il metodo della conversione decimale \[RightArrow] binario
     - Font size 12
     - Sfondo grigio chiaro
  *)

  Spacer[10],
  (* Spazio verticale di 10 punti *)

  Framed[
    (* Riquadro che contiene la parte interattiva *)

    DynamicModule[{d = ""},
    (* DynamicModule: crea un ambiente con una variabile dinamica locale d (inizializzata come stringa vuota)
       - d serve per catturare l'input dell'utente in tempo reale
    *)

    Column[{
      (* Colonna che organizza verticalmente tutti i componenti interattivi *)

      TextCell[
        "Esempio interattivo (inserisci un numero e premi Invio)", 
        FontSize -> 14, FontWeight -> "Bold"
      ],
      (* Testo guida sopra il campo input:
         - Font size 14, grassetto
         - Spiega all'utente cosa fare
      *)

      Grid[{
        {
          TextCell["Inserisci un numero decimale:"],
          (* Etichetta che dice all'utente di inserire un numero *)

          InputField[Dynamic[d], String]
          (* Campo di input dinamico:
             - Dynamic[d]: collega l'input alla variabile d
             - String: specifica che l'input viene gestito come stringa (testo)
          *)
        }
      }],
      (* Griglia con una riga e due colonne:
         1. La prima colonna mostra l'etichetta
         2. La seconda colonna contiene il campo input
      *)

      Dynamic[
        (* Blocca dinamico che aggiorna in tempo reale quando cambia d *)
        Module[{numD = ToExpression[d]},
          If[NumericQ[numD] && numD >= 0,
            conversionFromDec[16, numD],
            "Inserisci un numero decimale valido (>= 0)"
          ]
        ]
      ]
      (* Questo blocco mostra dinamicamente il risultato della conversione oppure il messaggio d'errore *)
    }]
    ],
    Background -> Lighter[Gray, 0.95]
    (* Sfondo ancora pi\[UGrave] chiaro per la parte interattiva *)
  ]
  (* Chiude il Framed della parte interattiva *)
};
(* Fine della built-in DecToBinSection *)


DecToOctSection[] := {
(* Definizione della built-in DecToOctSection che non ha argomenti e restituisce un blocco grafico per la sezione del tutorial dedicata alla conversione da decimale a ottale *)

  TextCell["Conversione da Decimale a Ottale", "Subsection",
    FontSize -> 18, FontWeight -> "Bold", 
    FontColor -> RGBColor[1, 0, 0], 
    TextAlignment -> Left, CellTags->"Tutorial5"],
  (* Crea il titolo della sezione:
     - FontSize 18, in grassetto, colore rosso
     - Allineamento a sinistra
  *)

  Spacer[10],
  (* Spazio verticale di 10 punti *)

  Framed[
    (* Riquadro per il testo esplicativo *)
    Row[{
    TextCell[
      Row[{"Per convertire un numero decimale in ottale, dividilo ripetutamente per 8 e raccogli i resti, oppure converti prima in binario e poi raggruppa le cifre in gruppi di 3.\nQuesto \[EGrave] possibile perch\[EGrave] ogni cifra ottale si rappresenta esattamente con 3 bit, cio\[EGrave] ",Superscript[2,3]," = 8.\n\nNell'esempio di seguito adotteremo il secondo metodo."}],
      FontSize -> 12
    ], Button["[3]",
        NotebookLocate["Bibliografia0"],
        Appearance -> "Frameless",
        BaselinePosition -> Baseline,
        Evaluator -> Automatic,
   BaseStyle -> {FontSize -> 12}
      ]}],
    Background -> Lighter[Gray, 0.9]
  ],
  (* Riquadro che contiene il testo esplicativo:
     - Testo che spiega come funziona la conversione
     - FontSize 12
     - Sfondo grigio chiaro (pi\[UGrave] leggibile)
  *)

  Spacer[10],
  (* Spazio verticale di 10 punti *)

  Framed[
    (* Riquadro che racchiude tutta la parte interattiva *)

    DynamicModule[{d = ""},
    (* DynamicModule crea un ambiente locale dove la variabile dinamica d viene definita e gestita;
       d \[EGrave] inizializzata come stringa vuota, serve per catturare l'input utente *)

    Column[{
      (* Organizza i componenti della parte interattiva verticalmente *)

      TextCell[
        "Esempio interattivo (inserisci un numero e premi Invio)", 
        FontSize -> 14, FontWeight -> "Bold"
      ],
      (* Testo guida sopra il campo input:
         - Font size 14
         - Grassetto
         - Spiega all'utente cosa fare
      *)

      Grid[{
        {
          TextCell["Inserisci un numero decimale:"],
          (* Cella che mostra l'etichetta accanto al campo input *)

          InputField[Dynamic[d], String]
          (* Campo di input dinamico:
             - Dynamic[d]: collega il contenuto del campo alla variabile d
             - String: specifica che l'input \[EGrave] trattato come stringa (testo)
          *)
        }
      }],
      (* Griglia a una riga e due colonne:
         1. Etichetta "Inserisci un numero decimale:"
         2. Campo input
      *)

      Dynamic[
        (* Blocca dinamico che aggiorna in tempo reale quando cambia d *)
        Module[{numD = ToExpression[d]},
          If[NumericQ[numD] && numD >= 0,
            conversionFromDec[16, numD],
            "Inserisci un numero decimale valido (>= 0)"
          ]
        ]
      ]
      (* Visualizza dinamicamente o la conversione o il messaggio d'errore *)
    }]
    ],
    Background -> Lighter[Gray, 0.95]
    (* Sfondo del riquadro: ancora pi\[UGrave] chiaro rispetto a quello sopra, per evidenziare la parte interattiva *)
  ]
  (* Chiude il Framed della parte interattiva *)
};
(* Fine della built-in DecToOctSection *)


DecToHexSection[] := {
(* Definizione della built-in DecToHexSection che non ha argomenti e restituisce un blocco grafico *)

  TextCell["Conversione da Decimale a Esadecimale", "Subsection",
    FontSize -> 18, FontWeight -> "Bold", 
    FontColor -> RGBColor[1, 0, 0], 
    TextAlignment -> Left, CellTags->"Tutorial6"],
  (* Crea un'intestazione per la sezione:
     - Stile: font grande (18), in grassetto, colore rosso
     - Allineamento: a sinistra
  *)

  Spacer[10],
  (* Spazio verticale di 10 punti *)

  Framed[
    (*  Incornicia il testo esplicativo  *)
    Row[{
    TextCell[
      Row[{"Per convertire un numero decimale in esadecimale, dividilo ripetutamente per 16 e raccogli i resti, oppure converti prima in binario e raggruppa le cifre in gruppi di 4.\nQuesto \[EGrave] possibile perch\[EGrave] ogni cifra esadecimale si rappresenta esattamente con 4 bit, cio\[EGrave] ",Superscript[2,4]," = 16.\n\nNell'esempio di seguito adotteremo il secondo metodo."}],
      FontSize -> 12
    ], Button["[4]",
        NotebookLocate["Bibliografia0"],
        Appearance -> "Frameless",
        BaselinePosition -> Baseline,
        Evaluator -> Automatic,
   BaseStyle -> {FontSize -> 12}
      ]}],
    Background -> Lighter[Gray, 0.9]
  ],
  (* Questo blocco mostra il testo che spiega il metodo di conversione decimale \[RightArrow] esadecimale *)

  Spacer[10],
  (* Spazio verticale di 10 punti *)

  Framed[
    (* Incornicia la parte interattiva *)
    DynamicModule[{d = ""},
    (* Crea un DynamicModule che definisce una variabile locale dinamica d (inizializzata come stringa vuota) *)
    Column[{
      (* Organizza i componenti verticalmente in una colonna *)

      TextCell[
        "Esempio interattivo (inserisci un numero e premi Invio)", 
        FontSize -> 14, FontWeight -> "Bold"
      ],
      (* Testo guida per l'utente, in font grande e grassetto *)

      Grid[{
        {
          TextCell["Inserisci un numero decimale:"],
          (* Testo statico che etichetta il campo di input *)

          InputField[Dynamic[d], String]
          (* Campo di input dinamico:
             - Dynamic[d]: collega il valore dell'input alla variabile dinamica d
             - String: specifica che l'input \[EGrave] trattato come stringa
          *)
        }
      }],
      (* Una griglia con una riga e due colonne:
         1. Testo di etichetta
         2. Campo di input
      *)

     Dynamic[
        (* Blocca dinamico che aggiorna in tempo reale quando cambia d *)
        Module[{numD = ToExpression[d]},
          If[NumericQ[numD] && numD >= 0,
            conversionFromDec[16, numD],
            "Inserisci un numero decimale valido (>= 0)"
          ]
        ]
        (* Se il numero non \[EGrave] valido (es. testo non numerico o numero negativo): mostra un messaggio d'errore, 
             Se il numero \[EGrave] valido: chiama conversionFromDec per convertire da decimale a binario (base 2)
            *)           
      ]
      (* Visualizza dinamicamente il risultato della conversione o il messaggio di errore *)
    }]
    ],
    Background -> Lighter[Gray, 0.95]
    (* Imposta uno sfondo ancora pi\[UGrave] chiaro per la parte interattiva *)
  ]
  (* Chiude il Framed della parte interattiva *)
};
(* Fine della built-in DecToHexToDecSection *)



(* =================== FUNZIONE PRINCIPALE =================== *)
(* Definizione della built-in principale che unisce tutte le sezioni *)

CreateExplainationConversion[] := Module[{sezioni},
(* Definizione della built-in CreateTutorial che non ha argomenti:
   - Usa Module per definire variabili locali
   - La variabile locale qui \[EGrave] 'sezioni', che conterr\[AGrave] la lista di tutte le sezioni del tutorial
*)

  sezioni = Join[
    (* Unisce tutte le sezioni del tutorial in un'unica lista *)

    IntroSection[],
    (* Sezione introduttiva con titolo e spiegazione generale *)

    BinToDecSection[],
    (* Sezione che spiega come convertire da binario a decimale *)

    OctToDecSection[],
    (* Sezione che spiega come convertire da ottale a decimale *)

    HexToDecSection[],
    (* Sezione che spiega come convertire da esadecimale a decimale *)

    DecToBinSection[],
    (* Sezione che spiega come convertire da decimale a binario *)

    DecToOctSection[],
    (* Sezione che spiega come convertire da decimale a ottale *)

    DecToHexSection[]
    (* Sezione che spiega come convertire da decimale a esadecimale *)
  ];
  (* Tutte queste sezioni sono combinate in una sola lista
     - Join serve a concatenare le singole liste restituite da ciascuna sezione
  *)

  Style[Column[sezioni, Alignment -> Left],FontFamily->"Arial"]
  (* Crea un'unica colonna che contiene tutte le sezioni combinate:
     - La colonna \[EGrave] allineata a sinistra (Alignment -> Left)
     - Questo restituisce l'intero tutorial come un unico blocco visivo continuo
  *)
];
(* Fine della built-in CreateTutorial *)



End[]
(* Fine della sezione privata *)

EndPackage[]
(* Fine del package *)

