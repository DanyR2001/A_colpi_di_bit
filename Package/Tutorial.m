(* ::Package:: *)

(* :Title: Tutorial *)
(* :Context: Tutorial` *)
(* :Author: Matteo Rontini, Daniele Russo *)
(* :Version: 1.3 *)
(* :Date: 2025-05-06 *)

(* :Summary: 
   Questo pacchetto crea il tutorial per ripassare le conversioni in varie basi prima di giocare.
*)

(* :Copyright: A colpi di Bit (C) 2025 *)
(* :Keywords: battaglia navale, gioco, interfaccia *)
(* :Requirements: Mathematica 12.0+, Util`, Battle`, Interaction` *)

(* File: Main.m *)

BeginPackage["Tutorial`", {"Util`"}]

CreateTutorial::usage = "CreateTutorial[] inserisce il tutorial nel notebook corrente."

Begin["`Private`"]
IntroSection::usage = "IntroSection[] crea l'intestazione e il sommario."
BinarySection::usage = "BinarySection[] crea la sezione sul sistema binario."
OctalSection::usage = "OctalSection[] crea la sezione sul sistema ottale."
HexSection::usage = "HexSection[] crea la sezione sul sistema esadecimale."
conversionToDec::usage="conversionToDec[base,number] restituisce i passaggi per la conversione di un numero da base qualsiasi a base 10";

(*indica i passagi per effettuare una conversione da base 10 a base qualsiasi*)
conversionToDec[base_, number_] := Module[{colors,numberDec, digits, powers, terms, result},
  colors = {Blue, Orange, Purple, Red, Brown, Pink, Green};
  numberDec=convertToDecimal[number,base];
  
  If[numberDec===$Failed,
  "Inserisci un numero intero in base "<>ToString[base],
  digits = IntegerDigits[numberDec, base];
  Column[{
      " \[Bullet] Scomponiamo il numero nelle sue cifre in base " <> ToString[base] <> ":",
      Row[
        Table[Panel[Style[digits[[n]],12, colors[[Mod[n - 1, Length[colors]] + 1]]]],{n,1,Length[digits]}]
      , Alignment -> Center, ImageSize -> Full],
      
      " \[Bullet] Ogni cifra viene moltiplicata per la potenza della base corrispondente alla sua posizione (da destra a sinistra):",
      
      Module[{len},
        len = Length[digits];
        powers = Reverse[Range[0, len - 1]]; (* posizione delle cifre: esponente *)
        terms = Table[digits[[i]] base^powers[[i]], {i, len}];

        Row[{
			Panel[Style[
				Grid[Table[{
					Style[digits[[n]], colors[[Mod[n - 1, Length[colors]] + 1]]],
					"\[Times]", 
					ToString[base]<>"^"<>ToString[powers[[n]]],
					"=", 
					Style[terms[[n]], Bold]
				},{n, len}]]
			,12]]
        }, Alignment -> Center, ImageSize -> Full]
      ],
      Spacer[5],
      " \[Bullet] Sommiamo tutti i valori ottenuti:",
      Row[{
        Panel[Style[
          Row[{
            Row[Riffle[
              Table[Style[terms[[n]], colors[[Mod[n - 1, Length[colors]] + 1]]], {n, Length[terms]}],
              " + "
            ]],
            " = ", numberDec}
          ], 14]]
      }, Alignment -> Center, ImageSize -> Full],
      
      Spacer[5],
      Row[{
        Style["Risultato: ", Italic, 13, Bold],
        BaseForm[number, base], " = ", Total[terms], " in base 10."
      }]
   }]
  ]
];


IntroSection[] := {
  TextCell["Tutorial su Conversioni di Base", "Section", FontSize -> 24, 
    FontWeight -> "Bold", FontColor -> RGBColor[1, 0, 0], TextAlignment -> Left],
  Spacer[12],
  Pane[TextCell["Sommario:\n- Binario \[RightArrow] Decimale\n- Ottale \[RightArrow] Decimale\n- Esadecimale \[RightArrow] Decimale"],
    Background -> Lighter[Gray, 0.95]],
  Spacer[12]
}

BinarySection[] := {
  TextCell["Conversione da Binario a Decimale", "Subsection", FontSize -> 18, 
    FontWeight -> "Bold", FontColor -> RGBColor[1, 0, 0], TextAlignment -> Left],
  Spacer[10],
  Framed[TextCell[
    "Per convertire un numero binario in decimale, moltiplica ciascuna cifra binaria per 2 elevato alla posizione (da destra verso sinistra, a partire da 0) e somma i risultati.", 
    FontSize -> 12], Background -> Lighter[Gray, 0.9]],
  Spacer[10],
  Framed[DynamicModule[{b = ""}, 
    Column[{
      TextCell["Esempio interattivo", FontSize -> 14, FontWeight -> "Bold"],
      Grid[{{TextCell["Inserisci un numero binario:"], InputField[Dynamic[b], String]}}],
      Dynamic[conversionToDec[2,b]]
     }]
  ], Background -> Lighter[Gray, 0.95]],
  Spacer[10],
  Framed[DynamicModule[{n = RandomInteger[{1, 127}], risposta = "", esito = Null, showReplay = False},
    Column[{
      TextCell["Mini-esercizio: Converti in decimale il seguente numero binario", FontSize -> 14, FontWeight -> "Bold"],
      Framed[Dynamic[TextCell[IntegerString[n, 2], FontSize -> 16, FontWeight -> "Bold"]], 
        Background -> Lighter[Gray, 0.85]],
      Grid[{{TextCell["Risposta:"], InputField[Dynamic[risposta], Number]}}],
      Row[{
        Button["Verifica",
          If[ToExpression[risposta] === n,
            (esito = TextCell["\:2705 Corretto!", FontSize -> 14, FontColor -> DarkGreen]; showReplay = True;),
            esito = TextCell["\:274c Riprovare\[Ellipsis]", FontSize -> 14, FontColor -> Red]
          ], ImageSize -> Large, Appearance -> "Frameless", Background -> LightGray, 
          FrameMargins -> 6, FrameStyle -> Black, RoundingRadius -> 5],
        Spacer[10],
        Dynamic[If[showReplay, 
          Button["Rigioca", (n = RandomInteger[{1, 127}]; risposta = ""; esito = Null; showReplay = False;), 
           ImageSize -> Large, Appearance -> "Frameless", Background -> LightBlue, 
           FrameMargins -> 6, FrameStyle -> Black, RoundingRadius -> 5], 
        Spacer[0]]]
      }],
      Dynamic[If[esito === Null, Spacer[0], esito]]
    }]
  ], Background -> Lighter[Gray, 0.95]]
}

OctalSection[] := {
  TextCell["Conversione da Ottale a Decimale", "Subsection", FontSize -> 18, 
    FontWeight -> "Bold", FontColor -> RGBColor[1, 0, 0], TextAlignment -> Left],
  Spacer[10],
  Framed[TextCell[
    "Ogni cifra di un numero ottale rappresenta una potenza di 8. Per convertire in decimale, moltiplica ogni cifra per 8 elevato alla sua posizione e somma i risultati.", 
    FontSize -> 12], Background -> Lighter[Gray, 0.9]],
  Spacer[10],
  Framed[DynamicModule[{o = ""}, 
    Column[{
      TextCell["Esempio interattivo", FontSize -> 14, FontWeight -> "Bold"],
      Grid[{{TextCell["Inserisci un numero ottale:"], InputField[Dynamic[o], String]}}],
      Dynamic[conversionToDec[8,o]]
    }]
  ], Background -> Lighter[Gray, 0.95]],
  Spacer[10],
  Framed[DynamicModule[{n = RandomInteger[{1, 127}], risposta = "", esito = Null, showReplay = False},
    Column[{
      TextCell["Mini-esercizio: Converti in decimale il seguente numero ottale", FontSize -> 14, FontWeight -> "Bold"],
      Framed[Dynamic[TextCell[IntegerString[n, 8], FontSize -> 16, FontWeight -> "Bold"]], 
        Background -> Lighter[Gray, 0.85]],
      Grid[{{TextCell["Risposta:"], InputField[Dynamic[risposta], Number]}}],
      Row[{
        Button["Verifica",
          If[ToExpression[risposta] === n,
            (esito = TextCell["\:2705 Corretto!", FontSize -> 14, FontColor -> DarkGreen]; showReplay = True;),
            esito = TextCell["\:274c Riprovare\[Ellipsis]", FontSize -> 14, FontColor -> Red]
          ], ImageSize -> Large, Appearance -> "Frameless", Background -> LightGray, 
          FrameMargins -> 6, FrameStyle -> Black, RoundingRadius -> 5],
        Spacer[10],
        Dynamic[If[showReplay, 
          Button["Rigioca", (n = RandomInteger[{1, 127}]; risposta = ""; esito = Null; showReplay = False;), 
           ImageSize -> Large, Appearance -> "Frameless", Background -> LightBlue, 
           FrameMargins -> 6, FrameStyle -> Black, RoundingRadius -> 5], 
        Spacer[0]]]
      }],
      Dynamic[If[esito === Null, Spacer[0], esito]]
    }]
  ], Background -> Lighter[Gray, 0.95]]
}

HexSection[] := {
  TextCell["Conversione da Esadecimale a Decimale", "Subsection", FontSize -> 18, 
    FontWeight -> "Bold", FontColor -> RGBColor[1, 0, 0], TextAlignment -> Left],
  Spacer[10],
  Framed[TextCell[
    "Il sistema esadecimale usa 16 simboli (0\[Dash]9 e A\[Dash]F). Per convertire in decimale, sostituisci le lettere con i valori 10\[Dash]15, poi moltiplica ciascuna cifra per 16 elevato alla sua posizione e somma.", 
    FontSize -> 12], Background -> Lighter[Gray, 0.9]],
  Spacer[10],
  Framed[DynamicModule[{h = ""}, 
    Column[{
      TextCell["Esempio interattivo", FontSize -> 14, FontWeight -> "Bold"],
      Grid[{{TextCell["Inserisci un numero esadecimale:"], InputField[Dynamic[h], String]}}],
      Dynamic[conversionToDec[16,h]]
    }]
  ], Background -> Lighter[Gray, 0.95]],
  Spacer[10],
  Framed[DynamicModule[{n = RandomInteger[{1, 127}], risposta = "", esito = Null, showReplay = False},
    Column[{
      TextCell["Mini-esercizio: Converti in decimale il seguente numero esadecimale", FontSize -> 14, FontWeight -> "Bold"],
      Framed[Dynamic[TextCell[IntegerString[n, 16], FontSize -> 16, FontWeight -> "Bold"]], 
        Background -> Lighter[Gray, 0.85]],
      Grid[{{TextCell["Risposta:"], InputField[Dynamic[risposta], Number]}}],
      Row[{
        Button["Verifica",
          If[ToExpression[risposta] === n,
            (esito = TextCell["\:2705 Corretto!", FontSize -> 14, FontColor -> DarkGreen]; showReplay = True;),
            esito = TextCell["\:274c Riprovare\[Ellipsis]", FontSize -> 14, FontColor -> Red]
          ], ImageSize -> Large, Appearance -> "Frameless", Background -> LightGray, 
          FrameMargins -> 6, FrameStyle -> Black, RoundingRadius -> 5],
        Spacer[10],
        Dynamic[If[showReplay, 
          Button["Rigioca", (n = RandomInteger[{1, 127}]; risposta = ""; esito = Null; showReplay = False;), 
           ImageSize -> Large, Appearance -> "Frameless", Background -> LightBlue, 
           FrameMargins -> 6, FrameStyle -> Black, RoundingRadius -> 5], 
        Spacer[0]]]
      }],
      Dynamic[If[esito === Null, Spacer[0], esito]]
    }]
  ], Background -> Lighter[Gray, 0.95]]
}

CreateTutorial[] := Module[{sezioni},
  sezioni = Join[
    IntroSection[],
    BinarySection[],
    OctalSection[],
    HexSection[]
  ];
  
  Column[sezioni, Alignment -> Left]
]

End[]
EndPackage[]
