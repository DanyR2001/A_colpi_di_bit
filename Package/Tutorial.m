BeginPackage["Tutorial`"]

IntroSection::usage = "IntroSection[] crea l'intestazione e il sommario."
BinarySection::usage = "BinarySection[] crea la sezione sul sistema binario."
OctalSection::usage = "OctalSection[] crea la sezione sul sistema ottale."
HexSection::usage = "HexSection[] crea la sezione sul sistema esadecimale."
CreaTutorialNotebook::usage = "CreaTutorialNotebook[] inserisce il tutorial nel notebook corrente."

Begin["`Private`"]

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
      Dynamic[TextCell["Valore decimale: " <> ToString[FromDigits[b, 2]], FontSize -> 12]]
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
            (esito = TextCell["✅ Corretto!", FontSize -> 14, FontColor -> DarkGreen]; showReplay = True;),
            esito = TextCell["❌ Riprovare…", FontSize -> 14, FontColor -> Red]
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
      Dynamic[TextCell["Valore decimale: " <> ToString[FromDigits[o, 8]], FontSize -> 12]]
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
            (esito = TextCell["✅ Corretto!", FontSize -> 14, FontColor -> DarkGreen]; showReplay = True;),
            esito = TextCell["❌ Riprovare…", FontSize -> 14, FontColor -> Red]
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
    "Il sistema esadecimale usa 16 simboli (0–9 e A–F). Per convertire in decimale, sostituisci le lettere con i valori 10–15, poi moltiplica ciascuna cifra per 16 elevato alla sua posizione e somma.", 
    FontSize -> 12], Background -> Lighter[Gray, 0.9]],
  Spacer[10],
  Framed[DynamicModule[{h = ""}, 
    Column[{
      TextCell["Esempio interattivo", FontSize -> 14, FontWeight -> "Bold"],
      Grid[{{TextCell["Inserisci un numero esadecimale:"], InputField[Dynamic[h], String]}}],
      Dynamic[TextCell["Valore decimale: " <> ToString[FromDigits[ToUpperCase[h], 16]], FontSize -> 12]]
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
            (esito = TextCell["✅ Corretto!", FontSize -> 14, FontColor -> DarkGreen]; showReplay = True;),
            esito = TextCell["❌ Riprovare…", FontSize -> 14, FontColor -> Red]
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

CreaTutorialNotebook[] := Module[{nb = EvaluationNotebook[], sezioni},
  sezioni = Join[
    IntroSection[],
    BinarySection[],
    OctalSection[],
    HexSection[]
  ];
  NotebookWrite[nb, Cell[BoxData@ToBoxes[#], "Output"] & /@ sezioni];
]

End[]
EndPackage[]
