(* ::Package:: *)

(* :Title: Util *)
(* :Context: Util` *)
(* :Author: Dan Cernei, Daniele Russo *)
(* :Version: 2.0 *)
(* :Date: 2025-05-06 *)

(* :Summary: 
   Questo pacchetto fornisce funzioni di utility per il gioco della battaglia navale,
   incluse funzioni per la conversione di coordinate, la visualizzazione della griglia e
   la gestione dell'input dell'utente.
*)

(* :Copyright: A colpi di Bit (C) 2025 *)
(* :Keywords: battaglia navale, utility, griglia, conversione *)
(* :Requirements: Mathematica 12.0+ *)

(* File: Util.m *)

BeginPackage["Util`"];

initSeed::usage="initSeed[seed] serve per inizializzare il PRNG prima di richimare le varie random"
convertToDecimal::usage = "convertToDecimal[input,base] converte una stringa da base specificata a base 10";
mapCoordinate::usage = "mapCoordinate[decimal, gridSize] converte un numero decimale in coordinate {riga, colonna}";
verifyInput::usage = "verifyInput[gridSize, base, input]controlla che l'input sia accettabile secondo la base scelta e la dimensione della griglia";
createGrid::usage="createGrid[ships, gridSize] crea una matrice gridSizexgridSize a partire dalle navi";
showGrid::usage="drawGrid[grid_, gridSize_, ships_Bool] disegna la griglia di gioco mostrando le navi e gli attacchi";

$Colpito::usage = "Valore per cella colpita.";
$Mancato::usage = "Valore per cella mancata.";
$Nave::usage = "Valore per cella contenente nave.";
$Vuoto::usage = "Valore per cella vuota.";
$Affondato::usage = "Valore per cella di nave affondata.";


$Colpito = 2;
$Mancato = -1;
$Nave = 1;
$Vuoto = 0;
$Affondato = 3;
$GridSize=10;

Begin["Private`"];


(* FUNZIONE per convertire da una base specificata a base 10 *)
convertToDecimal[input_String, base_Integer] := Module[
  {validChars, result},
  
  (* Definizione dei caratteri validi per ciascuna base *)
  validChars = Switch[base,
    2, {"0", "1"},
    8, CharacterRange["0", "7"],
    16, Join[CharacterRange["0", "9"], CharacterRange["A", "F"], CharacterRange["a", "f"]],
    _, {}
  ];
  
  (* Verifica che l'input contenga solo caratteri validi per la base specificata *)
  If[!AllTrue[Characters[input], MemberQ[validChars, #] &],
    Return[$Failed]
  ];
  
  (* Conversione da base specificata a base 10 *)
  result = Quiet[FromDigits[input, base]];
  
  (* Verifica che la conversione sia avvenuta correttamente *)
  If[!IntegerQ[result] || result < 0,
    Return[$Failed],
    result
  ]
];

(* FUNZIONE per inizializzare il PRNG la prima volta*)
initSeed[seed_Integer?NonNegative] := Module[{},
  SeedRandom[seed];
  (* restituiamo Null, l’importante è che il generatore sia resettato *)
  Null
]


verifyInput[gridSize_, base_, input_] := Module[{decimal, coordinates, row, col, errorMsg = ""},
  (* Conversione dell'input in base 10 *)
  decimal = convertToDecimal[input, base];
  
  If[decimal === $Failed,
    (* Messaggio di errore specifico per conversione fallita *)
    errorMsg = "Input non valido! Inserisci un numero corretto in base " <> ToString[base];
    Return[{$Failed, errorMsg}]  (* Restituisce una tupla con $Failed e il messaggio *)
  ];
  
  (* Calcolo della riga e colonna direttamente qui *)
  col = Mod[decimal, 10];
  row = Quotient[decimal, 10];
  
  
  (* Verifica che sia riga che colonna siano all'interno dei limiti della griglia *)
  If[row < 0 || row >= gridSize || col < 0 || col >= gridSize,
    (* Messaggio di errore specifico per coordinate fuori griglia *)
    errorMsg = "Coordinate fuori dalla griglia! Riga e colonna devono essere comprese tra 0 e " <> 
               ToString[gridSize - 1];
    Return[{$Failed, errorMsg}]
  ];
  
  (* Se arriva qui, le coordinate sono valide *)
  {{row, col}, ""}  (* Restituisce le coordinate e un messaggio vuoto in caso di successo *)
];



createGrid[ships_,gridSize_]:=Module[{grid=ConstantArray[$Vuoto,{gridSize,gridSize}]},
	Do[grid[[coord[[1]]+1,coord[[2]]+1]]=1,{coordList,ships},{coord,coordList}];
	grid
];

showGrid[grid_, ships_] := Module[{
    gridSize = Length[grid],
    gridWithLabels
  },
  (* Crea una nuova griglia con spazio per le etichette *)
  gridWithLabels = ConstantArray["", {gridSize + 1, gridSize + 1}];
  
  (* Aggiungi gli indici riga (0-9) sul lato sinistro *)
  Do[
    gridWithLabels[[i + 1, 1]] = ToString[i - 1],
    {i, 1, gridSize}
  ];
  
  (* Aggiungi gli indici colonna (0-9) in alto *)
  Do[
    gridWithLabels[[1, j + 1]] = ToString[j - 1],
    {j, 1, gridSize}
  ];
  
  (* Copia i contenuti della griglia *)
  Do[
    gridWithLabels[[i + 1, j + 1]] = 
      If[grid[[i, j]] == $Colpito,
        Style["\[FilledSquare]", Red],      (* colpito *)
      If[grid[[i, j]] == $Mancato,
        Style["\[FilledSquare]", Gray],     (* mancato *)
      If[grid[[i, j]] == $Affondato,
        Style["\[Dagger]", Blue, Bold, 18],     (* affondato *)
      If[grid[[i, j]] == $Nave && ships,
        Style["\[FilledSquare]", Black],    (* nave visibile solo se ships=True *)
        ""                   (* altrimenti vuoto *)
      ]]]],
    {i, 1, gridSize}, {j, 1, gridSize}
  ];
  
  Grid[gridWithLabels, 
    Frame -> All,
    FrameStyle -> GrayLevel[0.5],
    Background -> {
      {GrayLevel[0.9], None, None, None, None, None, None, None, None, None, None},  (* Prima colonna in grigio *)
      {GrayLevel[0.9], None, None, None, None, None, None, None, None, None, None},  (* Prima riga in grigio *)
      {None, None}
    },
    ItemSize -> {1.5, 1.5}
  ]
];
  


End[];
EndPackage[];
