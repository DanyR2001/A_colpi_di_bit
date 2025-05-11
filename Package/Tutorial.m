(* ::Package:: *)

(* :Title: Tutorial *)
(* :Context: Tutorial` *)
(* :Author: Matteo Rontini, Daniele Russo, Matilde Nardi *)
(* :Version: 1.6 *)
(* :Date: 2025-05-09 *)

(* :Summary: 
   Questo pacchetto crea il tutorial per ripassare le conversioni in varie basi (2-8-10-16) prima di giocare.
*)

(* :Copyright: A colpi di Bit (C) 2025 *)
(* :Keywords: battaglia navale, gioco, interfaccia *)
(* :Requirements: Mathematica 12.0+, Util`, Battle`, Interaction` *)

BeginPackage["Tutorial`", {"Util`"}]
(* Inizio del package Tutorial, specificando che importa anche il package Util` *)

CreateTutorial::usage = "CreateTutorial[] inserisce il tutorial nel notebook corrente."
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


DecToHexToDecSection::usage = "DecToHexToDecSection[] crea la sezione conversione da decimale a esadecimale."
(* Definisce la descrizione della built-in DecToHexToDecSection *)

conversionToDec::usage = "conversionToDec[base, number] restituisce i passaggi per la conversione di un numero da base qualsiasi a base 10."
(* Definisce la descrizione della built-in conversionToDec *)

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
    TextAlignment -> Center
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
    "Questo tutorial ti guider\[AGrave] attraverso le conversioni tra diversi sistemi numerici: binario, ottale, esadecimale e decimale.", 
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
    TextAlignment -> Left],
  (* Titolo della sezione:
     - Tipo: sottosezione
     - FontSize 18, in grassetto, colore rosso
     - Allineamento a sinistra
  *)

  Spacer[10],
  (* Spazio verticale di 10 punti *)

  Framed[
    (* Riquadro che contiene la spiegazione teorica *)
    TextCell[
      "Per convertire un numero binario in decimale, moltiplica ciascuna cifra per la potenza di 2 corrispondente alla sua posizione e somma i risultati.",
      FontSize -> 12
    ], 
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
           - La built-in mostrerà la conversione passo passo, con tabella e risultato finale
        *)
      ]
      (* Mostra dinamicamente la conversione oppure un messaggio d'errore se l'input è invalido *)
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
    TextAlignment -> Left],
  (* Titolo della sezione:
     - Tipo: sottosezione ("Subsection")
     - Font size 18, in grassetto, colore rosso
     - Allineamento a sinistra
  *)

  Spacer[10],
  (* Spazio verticale di 10 punti *)

  Framed[
    (* Riquadro che racchiude il testo esplicativo *)
    TextCell[
      "Per convertire un numero ottale in decimale, moltiplica ciascuna cifra per la potenza di 8 corrispondente alla sua posizione e somma i risultati.",
      FontSize -> 12
    ],
    Background -> Lighter[Gray, 0.9]
  ],
  (* Testo esplicativo:
     - Descrive come convertire da ottale a decimale (cifra × potenza di 8)
     - Font size 12
     - Incorniciato con sfondo grigio chiaro
  *)

  Spacer[10],
  (* Spazio verticale di 10 punti *)

  Framed[
    (* Riquadro che racchiude tutta la parte interattiva *)

    DynamicModule[{d = ""},
    (* DynamicModule: ambiente locale dinamico con variabile d inizializzata come stringa vuota
       - d servirà per catturare l'input utente
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
             - String: indica che l'input è trattato come stringa (testo)
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
           - La built-in mostrerà la conversione passo passo, con tabella e risultato finale
        *)
      ]
      (* Visualizza dinamicamente il risultato della conversione oppure un messaggio d'errore se input non valido *)
    }]
    ],
    Background -> Lighter[Gray, 0.95]
    (* Sfondo ancora più chiaro per la parte interattiva *)
  ]
  (* Chiude il Framed della parte interattiva *)
};
(* Fine della built-in OctToDecSection *)


HexToDecSection[] := {
(* Definizione della built-in HexToDecSection che non ha argomenti e restituisce il blocco grafico per la sezione dedicata alla conversione da esadecimale a decimale *)

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

  Framed[
    (* Riquadro per il testo esplicativo *)
    TextCell[
      "Il sistema esadecimale usa 16 simboli (0\[Dash]9 e A\[Dash]F). Per convertire in decimale, sostituisci le lettere con i valori 10\[Dash]15, poi moltiplica ciascuna cifra per 16 elevato alla sua posizione e somma.",
      FontSize -> 12
    ],
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
           - h è il numero inserito dall'utente
           - La built-in mostrerà la conversione passo passo, con tabella e risultato finale
        *)
      ]
      (* Questo blocco mostra in tempo reale la conversione oppure un messaggio d'errore se l'input non è valido *)
    }]
    ],
    Background -> Lighter[Gray, 0.95]
    (* Sfondo più chiaro per evidenziare la parte interattiva *)
  ]
  (* Chiude il Framed della parte interattiva *)
};
(* Fine della built-in HexToDecSection *)


DecToBinSection[] := {
(* Definizione della built-in DecToBinSection, che non ha argomenti e restituisce la parte grafica della sezione tutorial per la conversione da decimale a binario *)

  TextCell["Conversione da Decimale a Binario", "Subsection",
    FontSize -> 18, FontWeight -> "Bold", 
    FontColor -> RGBColor[1, 0, 0], 
    TextAlignment -> Left],
  (* Titolo della sezione:
     - Tipo: sottosezione ("Subsection")
     - Font size 18, grassetto, colore rosso
     - Allineamento a sinistra
  *)

  Spacer[10],
  (* Spazio verticale di 10 punti *)

  Framed[
    TextCell[
      "Per convertire un numero decimale in binario, dividilo ripetutamente per 2 e raccogli i resti. Leggi i resti dal basso verso l'alto.",
      FontSize -> 12
    ],
    Background -> Lighter[Gray, 0.9]
  ],
  (* Riquadro con il testo esplicativo:
     - Spiega il metodo della conversione decimale → binario
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
        (* Blocco dinamico: valuta e aggiorna automaticamente quando cambia d *)

        Module[{n = ToExpression[d]},
          (* Converte il valore inserito (stringa) in espressione numerica:
             - Es. se d = "25", allora n = 25
          *)

          If[IntegerQ[n] && n >= 0,
            (* Controlla se n è un numero intero e positivo o zero *)

            conversionFromDec[2, n],
            (* Se il numero è valido:
               - chiama conversionFromDec per convertire da decimale a binario (base 2)
            *)

            "Inserisci un numero decimale valido (>= 0)"
            (* Se il valore non è valido (es. testo non numerico o numero negativo):
               - mostra un messaggio di errore all'utente
            *)
          ]
        ]
      ]
      (* Questo blocco mostra dinamicamente il risultato della conversione oppure il messaggio d'errore *)
    }]
    ],
    Background -> Lighter[Gray, 0.95]
    (* Sfondo ancora più chiaro per la parte interattiva *)
  ]
  (* Chiude il Framed della parte interattiva *)
};
(* Fine della built-in DecToBinSection *)


DecToOctSection[] := {
(* Definizione della built-in DecToOctSection che non ha argomenti e restituisce un blocco grafico per la sezione del tutorial dedicata alla conversione da decimale a ottale *)

  TextCell["Conversione da Decimale a Ottale", "Subsection",
    FontSize -> 18, FontWeight -> "Bold", 
    FontColor -> RGBColor[1, 0, 0], 
    TextAlignment -> Left],
  (* Crea il titolo della sezione:
     - FontSize 18, in grassetto, colore rosso
     - Allineamento a sinistra
  *)

  Spacer[10],
  (* Spazio verticale di 10 punti *)

  Framed[
    TextCell[
      "Per convertire un numero decimale in ottale, dividilo ripetutamente per 8 e raccogli i resti oppure converti prima in binario e raggruppa le cifre in gruppi di 3.",
      FontSize -> 12
    ],
    Background -> Lighter[Gray, 0.9]
  ],
  (* Riquadro che contiene il testo esplicativo:
     - Testo che spiega come funziona la conversione
     - FontSize 12
     - Sfondo grigio chiaro (più leggibile)
  *)

  Spacer[10],
  (* Spazio verticale di 10 punti *)

  Framed[
    (* Riquadro che racchiude tutta la parte interattiva *)

    DynamicModule[{d = ""},
    (* DynamicModule crea un ambiente locale dove la variabile dinamica d viene definita e gestita;
       d è inizializzata come stringa vuota, serve per catturare l'input utente *)

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
             - String: specifica che l'input è trattato come stringa (testo)
          *)
        }
      }],
      (* Griglia a una riga e due colonne:
         1. Etichetta "Inserisci un numero decimale:"
         2. Campo input
      *)

      Dynamic[
        (* Parte dinamica: valuta e aggiorna automaticamente quando cambia d *)

        Module[{n = ToExpression[d]},
          (* Converte la stringa inserita in un'espressione matematica:
             - es. se d = "10", allora n = 10
          *)

          If[IntegerQ[n] && n >= 0,
            (* Controlla se n è un numero intero positivo o zero *)

            conversionFromDec[8, n],
            (* Se è valido: chiama la built-in conversionFromDec per convertire n da decimale a ottale *)

            "Inserisci un numero decimale valido (>= 0)"
            (* Se non è valido (es. testo non numerico o numero negativo): mostra un messaggio d'errore *)
          ]
        ]
      ]
      (* Visualizza dinamicamente o la conversione o il messaggio d'errore *)
    }]
    ],
    Background -> Lighter[Gray, 0.95]
    (* Sfondo del riquadro: ancora più chiaro rispetto a quello sopra, per evidenziare la parte interattiva *)
  ]
  (* Chiude il Framed della parte interattiva *)
};
(* Fine della built-in DecToOctSection *)


DecToHexToDecSection[] := {
(* Definizione della built-in DecToHexToDecSection che non ha argomenti e restituisce un blocco grafico *)

  TextCell["Conversione da Decimale a Esadecimale", "Subsection",
    FontSize -> 18, FontWeight -> "Bold", 
    FontColor -> RGBColor[1, 0, 0], 
    TextAlignment -> Left],
  (* Crea un'intestazione per la sezione:
     - Stile: font grande (18), in grassetto, colore rosso
     - Allineamento: a sinistra
  *)

  Spacer[10],
  (* Spazio verticale di 10 punti *)

  Framed[
    (* Incornicia il testo esplicativo *)
    TextCell[
      "Per convertire un numero decimale in esadecimale, dividilo ripetutamente per 16 e raccogli i resti oppure converti prima in binario e raggruppa le cifre in gruppi di 4.",
      FontSize -> 12
    ],
    Background -> Lighter[Gray, 0.9]
    (* Imposta lo sfondo del riquadro a un grigio chiaro *)
  ],
  (* Questo blocco mostra il testo che spiega il metodo di conversione decimale → esadecimale *)

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
             - String: specifica che l'input è trattato come stringa
          *)
        }
      }],
      (* Una griglia con una riga e due colonne:
         1. Testo di etichetta
         2. Campo di input
      *)

      Dynamic[
        (* Blocca dinamico che aggiorna in tempo reale quando cambia d *)
        Module[{n = ToExpression[d]},
          (* Converte la stringa d in espressione matematica e la assegna alla variabile n *)

          If[IntegerQ[n] && n >= 0,
            (* Controlla se n è un numero intero e >= 0 *)

            conversionFromDec[16, n],
            (* Se valido: chiama la built-in conversionFromDec per convertire da decimale a base 16 (esadecimale) *)

            "Inserisci un numero decimale valido (>= 0)"
            (* Se non valido: mostra un messaggio d'errore *)
          ]
        ]
      ]
      (* Visualizza dinamicamente il risultato della conversione o il messaggio di errore *)
    }]
    ],
    Background -> Lighter[Gray, 0.95]
    (* Imposta uno sfondo ancora più chiaro per la parte interattiva *)
  ]
  (* Chiude il Framed della parte interattiva *)
};
(* Fine della built-in DecToHexToDecSection *)


conversionToDec[base_, number_] := Module[{colors, numberDec, digits, powers, terms},
(* Definizione della built-in conversionToDec che prende due argomenti:
   - base: la base numerica da cui convertire
   - number: il numero da convertire
   All'interno definisce un Module con variabili locali:
   - colors: lista di colori per colorare le cifre
   - numberDec: il numero convertito in base decimale
   - digits: le cifre del numero in quella base
   - powers: le potenze associate a ciascuna cifra
   - terms: i risultati intermedi della moltiplicazione cifra × potenza
*)

  colors = {Blue, Orange, Purple, Red, Brown, Pink, Green};
  (* Definisce una lista di colori per evidenziare graficamente ogni cifra *)

  numberDec = convertToDecimal[number, base];
  (* Converte il numero da base "base" a base decimale usando la built-in convertToDecimal *)

  If[numberDec === $Failed,
  (* Controlla se la conversione ha fallito (es. input non valido); in tal caso restituisce un messaggio d'errore *)

    "Inserisci un numero intero in base " <> ToString[base],
    (* Messaggio di errore personalizzato che dice di inserire un numero valido *)

    digits = IntegerDigits[numberDec, base];
    (* Scompone il numero decimale in una lista di cifre nella base specificata *)

    Column[{ 
    (* Crea un layout verticale con tutti i componenti visuali che seguono *)

      " \[Bullet] Scomponiamo il numero nelle sue cifre in base " <> ToString[base] <> ":",
      (* Testo che spiega il primo passo: scomporre il numero in cifre *)

      Row[
        (* Dispone i pannelli delle cifre in una riga *)
        Table[
          (* Genera un pannello per ogni cifra *)
          Panel[
            (* Pannello con bordo che contiene la cifra *)
            Style[
              digits[[n]], 
              12, 
              colors[[Mod[n - 1, Length[colors]] + 1]]
            ]
            (* Stile della cifra: font size 12 e colore selezionato ciclicamente dalla lista di colori *)
          ],
          {n, 1, Length[digits]}
          (* Crea un pannello per ogni cifra (da 1 a numero delle cifre) *)
        ],
        Alignment -> Center, 
        (* Allinea la riga al centro *)
        ImageSize -> Full
        (* Imposta la larghezza della riga per occupare tutto lo spazio disponibile *)
      ],
      (* Fine della riga di pannelli per le cifre *)

      " \[Bullet] Ogni cifra viene moltiplicata per la potenza della base corrispondente alla sua posizione (da destra a sinistra):",
      (* Testo che spiega il secondo passo: la moltiplicazione per le potenze *)

      Module[{len},
      (* Crea un modulo locale per questa parte del calcolo; variabile len per la lunghezza delle cifre *)

        len = Length[digits];
        (* Calcola quante cifre ci sono *)

        powers = Reverse[Range[0, len - 1]];
        (* Crea una lista di esponenti: {len-1, ..., 0}, cioè le potenze della base da associare a ciascuna cifra *)

        terms = Table[
          digits[[i]] base^powers[[i]],
          {i, len}
        ];
        (* Calcola per ogni cifra: cifra × (base ^ potenza) *)

        Row[{
          (* Mostra la tabella in una riga *)
          Panel[
            (* Pannello che racchiude la tabella *)
            Style[
              Grid[
                (* Crea la tabella *)
                Table[{
                  Style[
                    digits[[n]], 
                    colors[[Mod[n - 1, Length[colors]] + 1]]
                  ],
                  (* Prima colonna: la cifra, colorata *)

                  "\[Times]",
                  (* Seconda colonna: simbolo × *)

                  ToString[base] <> "^" <> ToString[powers[[n]]],
                  (* Terza colonna: stringa che mostra base^esponente *)

                  "=",
                  (* Quarta colonna: simbolo = *)

                  Style[terms[[n]], Bold]
                  (* Quinta colonna: risultato del calcolo, in grassetto *)
                }, {n, len}]
                (* Crea una riga della tabella per ogni cifra *)
              ],
              12
              (* Imposta font size 12 per l'intera tabella *)
            ]
          ]
        }, 
        Alignment -> Center, 
        (* Allinea la tabella al centro *)
        ImageSize -> Full
        (* Fa sì che la tabella occupi tutta la larghezza disponibile *)
        ]
        (* Fine della visualizzazione della tabella *)
      ],
      (* Fine del Module per la tabella *)

      Spacer[5],
      (* Spazio verticale di 5 punti *)

      " \[Bullet] Sommiamo tutti i valori ottenuti:",
      (* Testo che introduce la somma dei termini *)

      Row[{
        (* Riga che contiene il pannello della somma *)
        Panel[
          (* Pannello che incornicia la somma *)
          Style[
            Row[{
              (* Riga che unisce tutti i termini + segni più + risultato *)
              Row[
                Riffle[
                  Table[
                    Style[
                      terms[[n]], 
                      colors[[Mod[n - 1, Length[colors]] + 1]]
                    ],
                    {n, Length[terms]}
                  ],
                  " + "
                  (* Inserisce un + tra ogni termine *)
                ]
              ],
              " = ",
              (* Aggiunge il simbolo uguale *)

              ToString[numberDec]
              (* Aggiunge il numero decimale finale *)
            }],
            FontSize -> 14
            (* Imposta la dimensione del font della somma a 14 *)
          ]
        ]
      }, 
      Alignment -> Center, 
      (* Allinea la riga al centro *)
      ImageSize -> Full
      (* Imposta la larghezza a tutta la linea *)
      ],
      (* Fine della visualizzazione della somma *)

      Spacer[5],
      (* Spazio verticale di 5 punti *)

      Row[{
        (* Riga per il risultato finale *)
        Style["Risultato: ", Italic, 13, Bold],
        (* Testo "Risultato:" in corsivo, font size 13 e grassetto *)

        BaseForm[number, base],
        (* Mostra il numero originale in notazione di base, es. 1010 (base 2) *)

        " = ",

        Total[terms],
        (* Mostra la somma dei termini, cioè il numero convertito in decimale *)

        " in base 10."
        (* Testo finale che spiega che il risultato è in base 10 *)
      }]
      (* Fine della riga finale *)
    }]
    (* Fine della Column che racchiude tutta la spiegazione *)
  ]
  (* Fine del blocco If *)
]
(* Fine del Module e della built-in *)






(*FUNZIONE che indica i passagi di una conversione da base specificata a base 10, usata negli esempi del tutorial*)
(*prende in input il numero e la base numerica del numero stesso*) 
conversionToDec[base_, number_] := Module[{colors,numberDec, digits, powers, terms},
  colors = {Blue, Orange, Purple, Red, Brown, Pink, Green};
  numberDec=convertToDecimal[number,base]; (*richiamo la funzioni in Util.m *)
  (*la funzione controlla che il numero sia corretto nella base specificata e se lo \[EGrave] lo converte in decimale*)
  
  If[numberDec===$Failed, (*se la conversione fallisce allora il numero non \[EGrave] nella base specificata*)
  "Inserisci un numero intero in base "<>ToString[base], (*mostro messaggio*)
  (*altrimenti vado avanti*)
  digits = IntegerDigits[numberDec, base]; (*lista delle cifre in base specificata del numero decimale*)
  Column[{ (*raggruppo in colonna la spiegazione della conversione*)
      " \[Bullet] Scomponiamo il numero nelle sue cifre in base " <> ToString[base] <> ":", (*prima fase*)
      Row[ (*in una riga mostro le cifre che compongono il numero in base specificata*)
      (*ogni cifra avr\[AGrave] un colore diverso*)
        Table[Panel[Style[digits[[n]],12, colors[[Mod[n - 1, Length[colors]] + 1]]]],{n,1,Length[digits]}]
      , Alignment -> Center, ImageSize -> Full],
      (*seconda fse*)
      " \[Bullet] Ogni cifra viene moltiplicata per la potenza della base corrispondente alla sua posizione (da destra a sinistra):",
      Module[{len},
        len = Length[digits]; (*numero di cifre che costituiscono il numero*)
        powers = Reverse[Range[0, len - 1]]; (* posizione delle cifre, 
        Reverse perch\[EGrave] la posizione delle cifre in un numero va da sinistra verso destra *)
        terms = Table[digits[[i]] base^powers[[i]], {i, len}]; (*moltiplico ogni cifra per la potenza della base, 
        terms contiene i prodotti, cio\[EGrave] i termini dell'addizione*)

        Row[{ (*al centro popsiziono la lista delle moltiplicazioni del tipo cifra \[Times] base^posizione *)
			Panel[Style[
				Grid[Table[{
					Style[digits[[n]], colors[[Mod[n - 1, Length[colors]] + 1]]], (*cifra*)
					"\[Times]", 
					ToString[base]<>"^"<>ToString[powers[[n]]], (*potenza*)
					"=", 
					Style[terms[[n]], Bold](*risultato della moltiplicazione*)
				},{n, len}]]
			,12]]
        }, Alignment -> Center, ImageSize -> Full]
      ],
      Spacer[5],
      " \[Bullet] Sommiamo tutti i valori ottenuti:",
      Row[{(*in una riga mostro la somma dei prodotti e il risultato che si ottiene*)
        (*la riga sar\[AGrave] del tipo: cifra \[Times] base^posizione + ... + cifra \[Times] base^posizione = somma*)
        (*dove cifra \[Times] base^posizione viene sostituito dal risultato del prodotto*)
        Panel[Style[
          Row[{
            Row[Riffle[ (*Riffle permette di scrivere il + tra ogni coppia di elementi in terms*)
              Table[Style[terms[[n]], colors[[Mod[n - 1, Length[colors]] + 1]]], {n, Length[terms]}], (*assegno un colore ad ogni termine*)
              " + "
            ]],
            " = ", numberDec}(*risultato della somma, gi\[AGrave] calcolato prima con la funzione convertToDec[]*)
          ], 14]]
      }, Alignment -> Center, ImageSize -> Full],
      
      Spacer[5],
      Row[{ (*mostro il risultato finale della conversione del tipo: numero in base = numero decimale*)
        Style["Risultato: ", Italic, 13, Bold],
        BaseForm[number, base], " = ", Total[terms], " in base 10."
      }]
   }]
  ]
];


(* =================== FUNZIONE PRINCIPALE =================== *)
(* Definizione della built-in principale che unisce tutte le sezioni *)

CreateTutorial[] := Module[{sezioni},
(* Definizione della built-in CreateTutorial che non ha argomenti:
   - Usa Module per definire variabili locali
   - La variabile locale qui è 'sezioni', che conterrà la lista di tutte le sezioni del tutorial
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

    DecToHexToDecSection[]
    (* Sezione che spiega come convertire da decimale a esadecimale *)
  ];
  (* Tutte queste sezioni sono combinate in una sola lista
     - Join serve a concatenare le singole liste restituite da ciascuna sezione
  *)

  Column[sezioni, Alignment -> Left]
  (* Crea un'unica colonna che contiene tutte le sezioni combinate:
     - La colonna è allineata a sinistra (Alignment -> Left)
     - Questo restituisce l'intero tutorial come un unico blocco visivo continuo
  *)
];
(* Fine della built-in CreateTutorial *)



End[]
(* Fine della sezione privata *)

EndPackage[]
(* Fine del package *)

