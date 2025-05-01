(* ::Package:: *)

(* File: Battle.m *)

BeginPackage["Battle`", {"Util`"}];

generateCoordinate::usage = "generateCoordinate[gridSize] genera casualmente una coppia di coordinate(x,y)";
attack::usage = "attack[attackCoords, grid, ships] attacca alle coordinate specificate, restituisce: attackResult \[RightArrow] messaggio, newGrid \[RightArrow] griglia aggiornata, hit\[RightArrow]booleano, indica se l'attacco \[EGrave] andato a buon fine, naveAffondata \[RightArrow] booleano, indica se l'attacco ha affondato una nave";
generateCPUShips::usage = "generateCPUShips[gridSize, seed] genera casualmente le navi della CPU";
StartGame::usage = "StartGame[userShips, CPUShips, userGridInit, cpuGridInit,userBase, gridSize] avvia il gioco (battaglia)";

Begin["`Private`"];


(* FUNZIONE per convertire da una base specificata a base 10 *)

(* FUNZIONE per mappare un numero decimale in coordinate {riga, colonna} *)

(* FUNZIONE per la verifica dell'input *)


(* FUNZIONE per l'attacco della CPU *)
generateCoordinate[gridSize_Integer] := Module[
  {decimal, coordinates},
  (* Genera un numero casuale tra 0 e gridSize^2 - 1 *)
  decimal = RandomInteger[{0, gridSize^2 - 1}];
  
  (* Mappatura del numero decimale in coordinate *)
  coordinates = mapCoordinate[decimal, gridSize];
  (* Return delle coordinate di attacco *)
  coordinates
];

(* FUNZIONE per l'attacco dell'utente *)
attack[attackCoords_, grid_, ships_]:=Module[
	{attackResult="", newGrid=grid, r,c, hit=False, naveAffondata = False},
	If[attackCoords===$Failed, attackResult = "Input non valido. Riprova.",
	r=attackCoords[[1]]+1;
	c=attackCoords[[2]]+1;
	Switch[grid[[r, c]], 
		$Nave, (* Colpito *)
			newGrid[[r,c]]=$Colpito;
			hit=True;
			(*userHits++;*)
			(* Controlla se \[EGrave] affondato *)
			Do[
				If[MemberQ[nave, {r - 1, c - 1}],
					If[AllTrue[nave, (newGrid[[#[[1]] + 1, #[[2]] + 1]] == $Colpito) &], 
						naveAffondata = True;
							    Do[
								    newGrid[[coord[[1]]+1,coord[[2]]+1]]=$Affondato;,
								{coord, nave}];
						    ];
							Break[];],
					{nave, ships}];
					
					If[naveAffondata,
						attackResult = "Colpito e affondato!",
						attackResult = "Colpito!"
					];,       
		$Vuoto, (* Mancato *)
			hit=True;
			newGrid[[r, c]] = $Mancato;
			attackResult = "Mi discpiace. Colpo non andato a segno, tenta di nuovo...";,
		_, (* Gi\[AGrave] colpito *)
			attackResult = "Hai gi\[AGrave] colpito qui!"
	];];
	{attackResult,newGrid,hit,naveAffondata}
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
      valid = AllTrue[shipCoords, grid[[#[[1]] + 1, #[[2]] + 1]] == $Vuoto &];
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
  {attackCoords, attackCpuCoords,result, userAttack, cpuAttack, countAffondato = 0, cpuAffondato = 0},

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
			  attackCoords = verifyInput[gridSize, userBase, input];
			  userAttack = attack[attackCoords, cpuGrid, CPUShips];
			  
			  (* Stato del gioco parte utente *)
			  If[userAttack[[4]], countAffondato++];
			  If[countAffondato==7, gameState="Complimenti! Hai vinto!"];
			  
			  messageUser="Coordinate attaccate"<>ToString[attackCoords]<>". "<>userAttack[[1]];
			  
			  If[userAttack[[3]], (*input valido e attacco utente effettuato*)
				  
				  (*Attacco della CPU*)
				  cpuGrid=userAttack[[2]];
				  attackCpuCoords=generateCoordinate[gridSize];
				  cpuAttack=attack[attackCpuCoords,userGrid,userShips];
				  userGrid=cpuAttack[[2]];
				  
				  (* Stato del gioco parte CPU *)
				  If[cpuAttack[[4]], cpuAffondato++];
                  If[cpuAffondato == 7, gameState = "La CPU ha vinto!"];
				  
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

