(* ::Package:: *)

(* :Title: Tutorial *)
(* :Context: Tutorial` *)
(* :Author: Matteo Rontini, Daniele Russo *)
(* :Version: 1.6 *)
(* :Date: 2025-05-09 *)

(* :Summary: 
   Questo pacchetto crea il tutorial per ripassare le conversioni in varie basi prima di giocare.
*)

(* :Copyright: A colpi di Bit (C) 2025 *)
(* :Keywords: battaglia navale, gioco, interfaccia *)
(* :Requirements: Mathematica 12.0+, Util`, Battle`, Interaction` *)

(* File: Tutorial.m *)

BeginPackage["Tutorial`", {"Util`"}]

CreateTutorial::usage = "CreateTutorial[] inserisce il tutorial nel notebook corrente."
IntroSection::usage = "IntroSection[] crea l'intestazione e il sommario."
BinarySection::usage = "BinarySection[] crea la sezione sul sistema binario."
OctalSection::usage = "OctalSection[] crea la sezione sul sistema ottale."
HexSection::usage = "HexSection[] crea la sezione sul sistema esadecimale."
DecToBinSection::usage = "DecToBinSection[] crea la sezione conversione da decimale a binario."
DecToOctSection::usage = "DecToOctSection[] crea la sezione conversione da decimale a ottale."
DecToHexSection::usage = "DecToHexSection[] crea la sezione conversione da decimale a esadecimale."
conversionToDec::usage = "conversionToDec[base, number] restituisce i passaggi per la conversione di un numero da base qualsiasi a base 10."


Begin["`Private`"]

(* =================== SEZIONI =================== *)

IntroSection[] := {
  TextCell["Tutorial: Conversioni tra Sistemi Numerici", "Section", FontSize -> 22, FontWeight -> "Bold", FontColor -> RGBColor[0.1, 0.1, 0.7], TextAlignment -> Center],
  Spacer[20],
  TextCell["Questo tutorial ti guider\[AGrave] attraverso le conversioni tra diversi sistemi numerici: binario, ottale, esadecimale e decimale.", FontSize -> 14],
  Spacer[10]
};

BinarySection[] := {
  TextCell["Conversione da Binario a Decimale", "Subsection",
    FontSize -> 18, FontWeight -> "Bold", FontColor -> RGBColor[1, 0, 0], TextAlignment -> Left],
  Spacer[10],
  Framed[TextCell[
    "Per convertire un numero binario in decimale, moltiplica ciascuna cifra per la potenza di 2 corrispondente alla sua posizione e somma i risultati.",
    FontSize -> 12], Background -> Lighter[Gray, 0.9]],
  Spacer[10],
  Framed[DynamicModule[{d = ""},
    Column[{
      TextCell["Esempio interattivo (inserisci un numero e premi Invio)", FontSize -> 14, FontWeight -> "Bold"],
      Grid[{{TextCell["Inserisci un numero binario:"], InputField[Dynamic[d], String]}}],
      Dynamic[conversionToDec[2, d]]
    }]
  ], Background -> Lighter[Gray, 0.95]]
};

OctalSection[] := {
  TextCell["Conversione da Ottale a Decimale", "Subsection",
    FontSize -> 18, FontWeight -> "Bold", FontColor -> RGBColor[1, 0, 0], TextAlignment -> Left],
  Spacer[10],
  Framed[TextCell[
    "Per convertire un numero ottale in decimale, moltiplica ciascuna cifra per la potenza di 8 corrispondente alla sua posizione e somma i risultati.",
    FontSize -> 12], Background -> Lighter[Gray, 0.9]],
  Spacer[10],
  Framed[DynamicModule[{d = ""},
    Column[{
      TextCell["Esempio interattivo (inserisci un numero e premi Invio)", FontSize -> 14, FontWeight -> "Bold"],
      Grid[{{TextCell["Inserisci un numero ottale:"], InputField[Dynamic[d], String]}}],
      Dynamic[conversionToDec[8, d]]
    }]
  ], Background -> Lighter[Gray, 0.95]]
};

HexSection[] := {
  TextCell["Conversione da Esadecimale a Decimale", "Subsection",
    FontSize -> 18, FontWeight -> "Bold", FontColor -> RGBColor[1, 0, 0], TextAlignment -> Left],
  Spacer[10],
  Framed[TextCell[
    "Il sistema esadecimale usa 16 simboli (0\[Dash]9 e A\[Dash]F). Per convertire in decimale, sostituisci le lettere con i valori 10\[Dash]15, poi moltiplica ciascuna cifra per 16 elevato alla sua posizione e somma.",
    FontSize -> 12], Background -> Lighter[Gray, 0.9]],
  Spacer[10],
  Framed[DynamicModule[{h = ""},
    Column[{
      TextCell["Esempio interattivo (inserisci un numero e premi Invio)", FontSize -> 14, FontWeight -> "Bold"],
      Grid[{{TextCell["Inserisci un numero esadecimale:"], InputField[Dynamic[h], String]}}],
      Dynamic[conversionToDec[16, h]]
    }]
  ], Background -> Lighter[Gray, 0.95]]
};

DecToBinSection[] := {
  TextCell["Conversione da Decimale a Binario", "Subsection",
    FontSize -> 18, FontWeight -> "Bold", FontColor -> RGBColor[1, 0, 0], TextAlignment -> Left],
  Spacer[10],
  Framed[TextCell[
    "Per convertire un numero decimale in binario, dividilo ripetutamente per 2 e raccogli i resti. Leggi i resti dal basso verso l'alto.",
    FontSize -> 12], Background -> Lighter[Gray, 0.9]],
  Spacer[10],
  Framed[DynamicModule[{d = ""},
    Column[{
      TextCell["Esempio interattivo (inserisci un numero e premi Invio)", FontSize -> 14, FontWeight -> "Bold"],
      Grid[{{TextCell["Inserisci un numero decimale:"], InputField[Dynamic[d], String]}}],
      Dynamic[
        Module[{n = ToExpression[d]},
          If[IntegerQ[n] && n >= 0,
            conversionFromDec[2, n],
            "Inserisci un numero decimale valido (>= 0)"
          ]
        ]
      ]
    }]
  ], Background -> Lighter[Gray, 0.95]]
};

DecToOctSection[] := {
  TextCell["Conversione da Decimale a Ottale", "Subsection",
    FontSize -> 18, FontWeight -> "Bold", FontColor -> RGBColor[1, 0, 0], TextAlignment -> Left],
  Spacer[10],
  Framed[TextCell[
    "Per convertire un numero decimale in ottale, dividilo ripetutamente per 8 e raccogli i resti oppure converti prima in binario e raggruppa le cifre in gruppi di 3.",
    FontSize -> 12], Background -> Lighter[Gray, 0.9]],
  Spacer[10],
  Framed[DynamicModule[{d = ""},
    Column[{
      TextCell["Esempio interattivo (inserisci un numero e premi Invio)", FontSize -> 14, FontWeight -> "Bold"],
      Grid[{{TextCell["Inserisci un numero decimale:"], InputField[Dynamic[d], String]}}],
      Dynamic[
        Module[{n = ToExpression[d]},
          If[IntegerQ[n] && n >= 0,
            conversionFromDec[8, n],
            "Inserisci un numero decimale valido (>= 0)"
          ]
        ]
      ]
    }]
  ], Background -> Lighter[Gray, 0.95]]
};

DecToHexSection[] := {
  TextCell["Conversione da Decimale a Esadecimale", "Subsection",
    FontSize -> 18, FontWeight -> "Bold", FontColor -> RGBColor[1, 0, 0], TextAlignment -> Left],
  Spacer[10],
  Framed[TextCell[
    "Per convertire un numero decimale in esadecimale, dividilo ripetutamente per 16 e raccogli i resti oppure converti prima in binario e raggruppa le cifre in gruppi di 4.",
    FontSize -> 12], Background -> Lighter[Gray, 0.9]],
  Spacer[10],
  Framed[DynamicModule[{d = ""},
    Column[{
      TextCell["Esempio interattivo (inserisci un numero e premi Invio)", FontSize -> 14, FontWeight -> "Bold"],
      Grid[{{TextCell["Inserisci un numero decimale:"], InputField[Dynamic[d], String]}}],
      Dynamic[
        Module[{n = ToExpression[d]},
          If[IntegerQ[n] && n >= 0,
            conversionFromDec[16, n],
            "Inserisci un numero decimale valido (>= 0)"
          ]
        ]
      ]
    }]
  ], Background -> Lighter[Gray, 0.95]]
};

(* =================== FUNZIONE PRINCIPALE =================== *)

CreateTutorial[] := Module[{sezioni},
  sezioni = Join[
    IntroSection[],
    BinarySection[],
    OctalSection[],
    HexSection[],
    DecToBinSection[],
    DecToOctSection[],
    DecToHexSection[]
  ];
  Column[sezioni, Alignment -> Left]
];

End[]
EndPackage[]
