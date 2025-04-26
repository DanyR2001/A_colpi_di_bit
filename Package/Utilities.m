(* ::Package:: *)

BeginPackage["Util`"];

convertToDecimal::usage = "convertToDecimal[input,base] converte una stringa da base specificata a base 10";
mapCoordinate::usage = "mapCoordinate[decimal, gridSize] converte un numero decimale in coordinate {riga, colonna}";
verifyInput::usage = "verifyInput[gridSize, base, input]controlla che l'input sia accettabile secondo la base scelta e la dimensione della griglia";
createGrid::usage="createGrid[ships, gridSize] crea una matrice gridSizexgridSize a partire dalle navi";
showGrid::usage = "drawGrid[grid, gridSize, ships_Bool] disegna la griglia di gioco mostrando le navi (se ships=True) e gli attacchi";

Begin["Private`"];

$Colpito=2;
$Mancato=-1;
$Nave=1;
$Vuota=0;
$Affondata=3;

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

(* FUNZIONE per mappare un numero decimale in coordinate {riga, colonna} *)
mapCoordinate[decimal_Integer, gridSize_] := Module[
  {row, col},
  (* Verifica che il numero sia valido per la dimensione della griglia *)
  If[decimal < 0 || decimal >= gridSize^2,
    Return[$Failed]
  ];
  
  (* Calcolo della colonna (0-based) *)
  col = Mod[decimal, gridSize];
  
  (* Calcolo della riga (0-based) *)
  row = Quotient[decimal, gridSize];
  
  (* Return delle coordinate come {riga, colonna} *)
  {row, col}
];

verifyInput[gridSize_, base_, input_]:=Module[{decimal, coordinates},
  (* Conversione dell'input in base 10 *)
  decimal = convertToDecimal[input, base];
  If[decimal === $Failed,
    Return[$Failed]
  ];
  
  (* Mappatura del numero decimale in coordinate *)
  coordinates = mapCoordinate[decimal, gridSize];
  If[coordinates === $Failed,
    Return[$Failed]
  ];
  
  (* Return delle coordinate di attacco *)
  coordinates
];


createGrid[ships_,gridSize_]:=Module[{grid=ConstantArray[$Vuota,{gridSize,gridSize}]},
	Do[grid[[coord[[1]]+1,coord[[2]]+1]]=1,{coordList,ships},{coord,coordList}];
	grid
];

showGrid[grid_, ships_] := Grid[Map[
	If[#== $Colpito,
        Style["\[FilledSquare]", Red],      (* colpito *)
      If[ #== $Mancato,
        Style["\[FilledSquare]", Gray],     (* mancato *)
      If[#== $Affondata,
        Style["\[FilledSquare]", Blue],     (* affondato *)
      If[#== $Nave && ships,
        Style["\[FilledSquare]", Black],    (* nave visibile solo se ships=True *)
        "\[EmptySquare]"                    (* altrimenti vuoto *)
      ]]]] &, grid, {2}], 
    Frame -> All,
    FrameStyle -> GrayLevel[0.5],
    Background -> {None, None, {GrayLevel[0.9], None}},
    ItemSize -> {1.5, 1.5}];


End[];
EndPackage[];
