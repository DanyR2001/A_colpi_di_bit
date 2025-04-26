(* ::Package:: *)

(* File: Battle.m *)

BeginPackage["BattagliaNavale`"];

convertToDecimal::usage = "converte una stringa da base specificata a base 10";
mapCoordinate::usage = "converte un numero decimale in coordinate {riga, colonna}";
createGrid::usage="createGrid[ships, gridSize] crea una matrice gridSizexgridSize a partire dalle navi 'ships'!";
generateCoordinate::usage = "converte l'input della CPU in coordinate di attacco";
attack::usage = "converte l'input dell'utente in coordinate di attacco";
generateCPUShips::usage = "creazione delle navi della CPU";
StartGame::usage = "StartGame[userShips_, CPUShips_, userBase_, gridSize_, seed_] avvia il gioco";
showGrid::usage="drawGrid[grid_, gridSize_, ships_Bool] disegna la griglia di gioco mostrando le navi e gli attacchi";
verifyInput::usage="controlla che l'input sia accettabile secondo la base scelta e la dimensione della griglia";

Begin["`Private`"];

$Colpito=2;
$Mancato=-1;
$Nave=1;
$Vuota=0;
$Affondata=3;


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

(* FUNZIONE per l'attacco della CPU *)
generateCoordinate[gridSize_Integer, seed_] := Module[
  {decimal, coordinates},
  (*SeedRandom[seed]; --> se si imposta poi ad ogni attacco colpisce sempre la stessa cella*)
  (* Genera un numero casuale tra 0 e gridSize^2 - 1 *)
  decimal = RandomInteger[{0, gridSize^2 - 1}];
  
  (* Mappatura del numero decimale in coordinate *)
  coordinates = mapCoordinate[decimal, gridSize];
  (* Return delle coordinate di attacco *)
  coordinates
];

(* FUNZIONE per l'attacco dell'utente *)
attack[attackCoords_, grid_, ships_]:=Module[
	{attackResult="", newGrid=grid, r,c, hit=False},
	If[attackCoords===$Failed, attackResult = "Input non valido. Riprova.",
	r=attackCoords[[1]]+1;
	c=attackCoords[[2]]+1;
	Switch[grid[[r, c]], 
		$Nave, (* Colpito *)
			newGrid[[r,c]]=$Colpito;
			hit=True;
			(*userHits++;*)
			(* Controlla se \[EGrave] affondato *)
			Module[{naveAffondata = False},
			Do[
				If[MemberQ[nave, {r - 1, c - 1}],
					If[AllTrue[nave, (newGrid[[#[[1]] + 1, #[[2]] + 1]] == $Colpito) &], 
						naveAffondata = True;
							    Do[
								    newGrid[[coord[[1]]+1,coord[[2]]+1]]=$Affondata;,
								{coord, nave}];
						    ];
							Break[];],
					{nave, ships}];
					
					If[naveAffondata,
						attackResult = "Colpito e affondato!",
						attackResult = "Colpito!"
					];
				];,       
		$Vuota, (* Mancato *)
			hit=True;
			newGrid[[r, c]] = $Mancato;
			attackResult = "Mi discpiace. Colpo non andato a segno, tenta di nuovo...";,
		_, (* Gi\[AGrave] colpito *)
			attackResult = "Hai gi\[AGrave] colpito qui!"
	];];
	{attackResult,newGrid,hit}
];

(* FUNZIONE per generare le navi della CPU *)
generateCPUShips[gridSize_Integer, seed_Integer] := Module[
  {ships = {}, shipLengths = {5, 4, 3, 2, 2, 1, 1}, grid, orientation, startRow, startCol, shipCoords, valid},
  
  (* Inizializza il generatore di numeri casuali con il seed fornito *)
  SeedRandom[seed];
  
  (* Inizializza la griglia vuota *)
  grid = ConstantArray[$Vuoto, {gridSize, gridSize}];
  
  (* Genera ogni nave *)
  Do[
    valid = False;
    
    (* Continua a provare fino a quando non si trova una posizione valida *)
    While[!valid,
      (* Sceglie casulmente l'orientamento: orizzontale (1) o verticale (2) *)
      orientation = RandomInteger[{1, 2}];
      
      If[orientation == 1, 
        (* Orizzontale *)
        startRow = RandomInteger[{0, gridSize - 1}];
        startCol = RandomInteger[{0, gridSize - shipLength}];
        shipCoords = Table[{startRow, startCol + i}, {i, 0, shipLength - 1}],
        (* Verticale *)
        startRow = RandomInteger[{0, gridSize - shipLength}];
        startCol = RandomInteger[{0, gridSize - 1}];
        shipCoords = Table[{startRow + i, startCol}, {i, 0, shipLength - 1}]
      ];
      
      (* Verifica che tutte le coordinate siano libere *)
      valid = AllTrue[shipCoords, grid[[#[[1]] + 1, #[[2]] + 1]] == $Vuota &];
    ];
    
    (* Marca le celle occupate dalla nave *)
    Do[
      grid[[coord[[1]] + 1, coord[[2]] + 1]] = $Nave,
      {coord, shipCoords}
    ];
    
    (* Adding della nave alla lista *)
    AppendTo[ships, shipCoords],
    
    {shipLength, shipLengths}
  ];
  
  (* Return della lista delle navi *)
  ships
];

(* FUNZIONE per fare lo StartGame *)
StartGame[userShips_, CPUShips_, userGridInit_, cpuGridInit_,userBase_, gridSize_] := Module[
  {userHits = 0, cpuHits = 0, gameOver = False, winner = None, 
   attackCoords, attackCpuCoords,result, userAttack,cpuAttack},

  DynamicModule[
  {input = "", gameState = "In corso...",messageUser="", messageCpu="",cpuGrid=cpuGridInit, userGrid=userGridInit},
    Column[{
      (* Titolo *)
      Style["Inizia la battaglia!!!", Bold, 24, Red],
      Row[{
        (* Griglia utente *)
        Column[{
          Style["Le tue navi", Bold, 14],
          Dynamic[showGrid[userGrid,True]]
        }],
        Spacer[30],
        (* Campo d'attacco *)
        Column[{
          Style["Campo d'attacco", Bold, 14],
          Dynamic[showGrid[cpuGrid,False]]
        }]
      }],
      Spacer[20],
      (* Controlli di gioco *)
      Grid[{
        {"Inserisci la cella da attaccare:", 
          InputField[Dynamic[input], String, ImageSize -> {150, 30}],
          Button["Fire!", 
			  (*Attacco dell'Utente*)
			  
			  attackCoords = verifyInput[gridSize, userBase,input];
			  userAttack = attack[attackCoords,cpuGrid, CPUShips];
			  messageUser="Coordinate attaccate"<>ToString[attackCoords]<>". "<>userAttack[[1]];
			  
			  If[userAttack[[3]], (*input valido e attacco utente effettuato*)
				  (*Attacco della CPU*)
				  cpuGrid=userAttack[[2]];
				  attackCpuCoords=generateCoordinate[gridSize,seed];
				  cpuAttack=attack[attackCpuCoords,userGrid,userShips];
				  userGrid=cpuAttack[[2]];
				  messageCpu="Coordinate attaccate"<>ToString[attackCpuCoords]<>". "<>cpuAttack[[1]];
			  ];
			, ImageSize -> {80, 30}]
		}
      }],
      Spacer[10],
      Style["Attacco Utente:", Bold, 12],
        Dynamic[messageUser],
        Spacer[15],
        Style["Attacco CPU:", Bold, 12],
        Dynamic[messageCpu],
      Dynamic[Style[gameState, Bold, 14, Darker[Green]]]
    }]
  ]
]

End[];

EndPackage[];

