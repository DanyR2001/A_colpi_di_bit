(* ::Package:: *)

(* File: battle.m *)

BeginPackage["BattagliaNavale`Battle`", {"BattagliaNavale`"}];

convertToDecimal::usage = "converte una stringa da base specificata a base 10";
mapCoordinate::usage = "converte un numero decimale in coordinate {riga, colonna}";
$GridSize::usage = "$GridSize \[EGrave] dimensione della griglia di gioco";
cpuAttack::usage = "converte l'input della CPU in coordinate di attacco";
userAttack::usage = "converte l'input dell'utente in coordinate di attacco";
generateCPUShips::usage = "creazione delle navi della CPU";

Begin["`Private`"];


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
mapCoordinate[decimal_Integer] := Module[
  {gridSize = $GridSize, row, col},
  
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


(* FUNZIONE per l'attacco della CPU *)
cpuAttack[gridSize_Integer] := Module[
  {decimal, coordinates},
  
  (* Genera un numero casuale tra 0 e gridSize^2 - 1 *)
  decimal = RandomInteger[{0, gridSize^2 - 1}];
  
  (* Mappatura del numero decimale in coordinate *)
  coordinates = mapCoordinate[decimal];
  
  (* Return delle coordinate di attacco *)
  coordinates
];


(* FUNZIONE per l'attacco dell'utente *)
userAttack[input_String, base_Integer] := Module[
  {decimal, coordinates},
  
  (* Verifica che l'input sia valido per la base specificata *)
  If[!verifyInputField[input, base],
    Return[$Failed]
  ];
  
  (* Conversione dell'input in base 10 *)
  decimal = convertToDecimal[input, base];
  If[decimal === $Failed,
    Return[$Failed]
  ];
  
  (* Mappatura del numero decimale in coordinate *)
  coordinates = mapCoordinate[decimal];
  If[coordinates === $Failed,
    Return[$Failed]
  ];
  
  (* Return delle coordinate di attacco *)
  coordinates
];


(* FUNZIONE per generare le navi della CPU *)
generateCPUShips[gridSize_Integer, seed_Integer] := Module[
  {ships = {}, shipLengths = {5, 4, 3, 2, 2, 1, 1}, grid, orientation, startRow, startCol, shipCoords, valid},
  
  (* Inizializza il generatore di numeri casuali con il seed fornito *)
  SeedRandom[seed];
  
  (* Inizializza la griglia vuota *)
  grid = ConstantArray[0, {gridSize, gridSize}];
  
  (* Genera ogni nave *)
  Do[
    valid = False;
    
    (* Continua a provare fino a quando non si trova una posizione valida *)
    While[!valid,
      (* Sceglie casulmente l'orientamento: orizzontale (1) o verticale (2) *)
      orientation = RandomInteger[{1, 2}];
      
      If[orientation == 1, (* Orizzontale *)
        startRow = RandomInteger[{0, gridSize - 1}];
        startCol = RandomInteger[{0, gridSize - shipLength}];
        shipCoords = Table[{startRow, startCol + i}, {i, 0, shipLength - 1}],
        (* Verticale *)
        startRow = RandomInteger[{0, gridSize - shipLength}];
        startCol = RandomInteger[{0, gridSize - 1}];
        shipCoords = Table[{startRow + i, startCol}, {i, 0, shipLength - 1}]
      ];
      
      (* Verifica che tutte le coordinate siano libere *)
      valid = AllTrue[shipCoords, grid[[#[[1]] + 1, #[[2]] + 1]] == 0 &];
    ];
    
    (* Marca le celle occupate dalla nave *)
    Do[
      grid[[coord[[1]] + 1, coord[[2]] + 1]] = 1,
      {coord, shipCoords}
    ];
    
    (* Adding della nave alla lista *)
    AppendTo[ships, shipCoords],
    
    {shipLength, shipLengths}
  ];
  
  (* Return della lista delle navi *)
  ships
];


End[];

EndPackage[];



