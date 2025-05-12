(* ::Package:: *)

(* :Title: Battle *)
(* :Context: Battle` *)
(* :Author: Dan Cernei, Daniele Russo *)
(* :Version: 2.1 *)
(* :Date: 2025-05-06 *)

(* :Summary: 
   Questo pacchetto implementa la logica di gioco della battaglia navale,
   inclusa la generazione delle navi, il sistema di attacco e la gestione del turno.
*)

(* :Copyright: A colpi di Bit (C) 2025 *)
(* :Keywords: battaglia navale, attacco, navi, CPU *)
(* :Requirements: Mathematica 12.0+, Util` *)

(* File: Battle.m *)
BeginPackage["Battle`", {"Util`", "Interaction`"}]; 


generateCoordinate::usage = "generateCoordinate[gridSize] genera casualmente una coppia di coordinate(x,y)";
attack::usage = "attack[attackCoords, grid, ships] attacca alle coordinate specificate, restituisce: attackResult \[RightArrow] messaggio, newGrid \[RightArrow] griglia aggiornata, hit\[RightArrow]booleano, indica se l'attacco \[EGrave] andato a buon fine, naveAffondata \[RightArrow] booleano, indica se l'attacco ha affondato una nave";
generateCPUShips::usage = "generateCPUShips[gridSize, seed] genera casualmente le navi della CPU";
StartGame::usage = "StartGame[userShips, CPUShips, userGridInit, cpuGridInit,userBase, gridSize] avvia il gioco (battaglia)";
InitPhase::usage =
  "InitPhase[base, gridSize] inizializza il gioco: base \[Element] {2,8,16}, gridSize \[EGrave] lato griglia.\
Imposta UserBase, GridSize, AutomaticGrid e UserGrid, resetta gli stati.";
ResetGame::usage =
  "ResetGame[] svuota tutte le liste e ripristina le variabili globali all'inizio del gioco.";

Begin["`Private`"];

(* Inizializza contesto PC e utente *)
InitPhase[seed_Integer, base_Integer, difficultyLevel_Integer] := Module[
  {gridSize, shipLengths},
  
  (* Reset variabili per sicurezza *)
  ResetGame[];
  
  (* Ottieni gridSize e shipLengths in base al livello di difficolt\[AGrave] *)
  gridSize = Interaction`GetDifficultyLevels[][[difficultyLevel, 2]];
  shipLengths = Interaction`GetDifficultyLevels[][[difficultyLevel, 3]];
  
  (* Inizializza tutti i valori necessari *)
  SetShipLengths[shipLengths];
  SetSeed[seed];
  initSeed[seed];
  SetUserBase[base];
  SetGridSize[gridSize];
  
  (* Genera navi CPU *)
  Block[{cpuShips = generateCPUShips[gridSize]},
    SetAutomaticShips[cpuShips];
    SetAutomaticGrid[createGrid[cpuShips, gridSize]];
  ];
  
  (* Inizializza griglia utente vuota *)
  SetUserShips[{}];
  SetUserGrid[ConstantArray[Vuoto, {gridSize, gridSize}]];
  
  True (* Segnala successo *)
];

(* Reset di tutte le variabili e liste *)
ResetGame[] := Module[{},
  SetUserBase[10];
  SetGridSize[10];
  SetAutomaticShips[{}];
  SetUserShips[{}];
  SetAutomaticGrid[{}];
  SetUserGrid[{}];
  SetSeed[0];
  SetShipLengths[{5, 4, 3, 2, 1}];
  CPUAttackedCoordinates = {};
];


(* Utilizziamo un insieme per tenere traccia delle coordinate gi\[AGrave] utilizzate dalla CPU *)
CPUAttackedCoordinates = {};

(* FIXED: generateCoordinate per gli attacchi della CPU *)
generateCoordinate[gridSize_Integer] := Module[
  {coordinates, attemptCount = 0, maxAttempts = gridSize^2},
  
  (* Per evitare cicli infiniti *)
  While[attemptCount < maxAttempts,
    (* Genera direttamente riga e colonna casuali *)
    coordinates = {RandomInteger[{0, gridSize - 1}], RandomInteger[{0, gridSize - 1}]};
    
    (* Verifica se queste coordinate sono gi\[AGrave] state usate *)
    If[!MemberQ[CPUAttackedCoordinates, coordinates],
      (* Aggiunge le coordinate all'insieme per non riutilizzarle *)
      AppendTo[CPUAttackedCoordinates, coordinates];
      Return[coordinates];
    ];
    
    attemptCount++;
  ];
  
  (* Fallback: cerca sistematicamente una posizione libera *)
  Do[
    Do[
      coordinates = {i, j};
      If[!MemberQ[CPUAttackedCoordinates, coordinates],
        AppendTo[CPUAttackedCoordinates, coordinates];
        Return[coordinates];
      ],
      {j, 0, gridSize - 1}
    ],
    {i, 0, gridSize - 1}
  ];
  
  (* Se tutte le posizioni sono occupate, restituisci coordinate di fallback *)
  {0, 0}
];

(* Funzione attack che gestisce il formato delle coordinate *)
attack[attackCoordsWithMsg_, grid_, ships_] := Module[
  {attackResult = "", newGrid = grid, r, c, hit = False, naveAffondata = False, attackCoords},
  
  (* Se l'input \[EGrave] una tupla {$Failed, errorMsg}, estraiamo il messaggio di errore specifico *)
  If[ListQ[attackCoordsWithMsg] && Length[attackCoordsWithMsg] == 2 && attackCoordsWithMsg[[1]] === $Failed, 
    Return[{attackCoordsWithMsg[[2]], grid, False, False}]
  ];
  
  (* Se \[EGrave] valido, estrai le coordinate corrette *)
  If[ListQ[attackCoordsWithMsg] && Length[attackCoordsWithMsg] == 2 && Head[attackCoordsWithMsg[[1]]] =!= List,
    (* Se attackCoordsWithMsg \[EGrave] nella forma {coordinate, messaggio} *)
    attackCoords = attackCoordsWithMsg,
    (* Se attackCoordsWithMsg \[EGrave] gi\[AGrave] direttamente le coordinate *)
    attackCoords = attackCoordsWithMsg[[1]]
  ];
  
  (* Controllo che attackCoords sia una lista di due elementi *)
  If[!ListQ[attackCoords] || Length[attackCoords] != 2,
    Return[{"Coordinate non valide!", grid, False, False}]
  ];
  
  (* Converte coordinate based-0 a based-1 per accedere alla griglia *)
  r = attackCoords[[1]] + 1;
  c = attackCoords[[2]] + 1;
  
  (* Controlla che le coordinate siano all'interno della griglia *)
  If[r < 1 || r > Length[grid] || c < 1 || c > Length[grid[[1]]],
    Return[{"Coordinate fuori dalla griglia!", grid, False, False}]
  ];
  (* Controlla il contenuto della cella (r, c) della griglia *)
  Switch[grid[[r, c]], 
    Nave, (* COLPITO *)
	(* Aggiorna la nuova griglia (newGrid) segnando la cella come colpita *)    
      newGrid[[r, c]] = Colpito;
      hit = True;
      (* Controlla se \[EGrave] affondato *)
      Do[
      (* Controlla se la cella attaccata fa parte di questa nave.
       L'indice \[EGrave] scalato di -1, perch\[EGrave] le coordinate delle navi siano basate su 0 *)
        If[MemberQ[nave, {r - 1, c - 1}],
        (* Controlla se tutte le celle di questa nave sono state colpite (Colpito). Se s\[IGrave], la nave \[EGrave] affondata *)
          If[AllTrue[nave, (newGrid[[#[[1]] + 1, #[[2]] + 1]] == Colpito) &], 
            naveAffondata = True;
            (* Per ogni coordinata della nave, si aggiorna la griglia per segnare tutte le sue celle come affondate *)
            Do[
              newGrid[[coord[[1]] + 1, coord[[2]] + 1]] = Affondato;,
              {coord, nave}
            ];
          ];
          Break[];
        ],
        {nave, ships}
      ];
      (* Assegna un messaggio *)    
      If[naveAffondata,
        attackResult = "Colpito e affondato!",
        attackResult = "Colpito!"
      ];,       
    Vuoto, (* MANCATO *)
      hit = True;
      newGrid[[r, c]] = Mancato;
      attackResult = "Mi dispiace. Colpo non andato a segno, tenta di nuovo...";,
    _, (* GI\[CapitalAGrave] COLPITO *)
      attackResult = "Hai gi\[AGrave] colpito qui!"
  ];
  (*  Ritorna un quattro-uple *)
  {attackResult, newGrid, hit, naveAffondata}
];

(* FUNZIONE per generare le navi della CPU *)
generateCPUShips[gridSize_Integer] := Module[
  {ships = {}, shipLengths, grid, orientation, startRow, startCol, shipCoords, valid, surroundingCells},
  
  (* Inizializza la griglia vuota *)
  grid = ConstantArray[Vuoto, {gridSize, gridSize}];

  (* Utilizza le lunghezze delle navi definite dalla variabile globale ShipLengths *)
  shipLengths = GetRemainingShipLengths[];
  
  (* Debug - stampa le lunghezze delle navi 
  Print["Generating CPU ships with lengths: ", shipLengths];*)
  
  (* Genera ogni nave - iteriamo su ogni lunghezza di nave *)
  Do[
    valid = False;
    
    (* Verifica che la nave corrente possa entrare nella griglia *)
    If[shipLength > gridSize, 
      Print["Ship too long: ", shipLength, " > ", gridSize];
      Continue[];
    ];

    (* Continua a provare fino a quando non si trova una posizione valida *)
    While[!valid,
      (* Sceglie casualmente l'orientamento: orizzontale (1) o verticale (2) *)
      orientation = RandomInteger[{1, 2}];
      
      If[orientation == 1, 
        (* Orizzontale *)
        startRow = RandomInteger[{0, gridSize - 1}];
        startCol = RandomInteger[{0, gridSize - shipLength}];
        (* Crea le coordinate per una nave orizzontale *)
        shipCoords = Table[{startRow, startCol + i}, {i, 0, shipLength - 1}],
        (* Verticale *)
        startRow = RandomInteger[{0, gridSize - shipLength}];
        startCol = RandomInteger[{0, gridSize - 1}];
        (* Crea le coordinate per una nave verticale *)
        shipCoords = Table[{startRow + i, startCol}, {i, 0, shipLength - 1}]
      ];
      
      (* Calcolo celle circostanti per ogni cella della nave *)
      surroundingCells = Flatten[
        Table[
          (* Per ogni cella della nave, genera le 8 celle circostanti *)
          Table[
            {shipCoords[[i, 1]] + dr, shipCoords[[i, 2]] + dc}, 
            {dr, -1, 1}, {dc, -1, 1}
          ],
          {i, 1, Length[shipCoords]}
        ], 2];
      
      (* Filtriamo le celle circostanti per rimuovere le celle effettive della nave 
         e quelle fuori dalla griglia *)
      surroundingCells = Select[surroundingCells, 
        !MemberQ[shipCoords, #] && 
        0 <= #[[1]] < gridSize && 
        0 <= #[[2]] < gridSize &
      ];
      
      (* Verifica che tutte le coordinate della nave siano libere *)
      valid = AllTrue[shipCoords, grid[[#[[1]] + 1, #[[2]] + 1]] == Vuoto &];
      
      (* Verifica che non ci siano navi adiacenti *)
      If[valid, 
        valid = AllTrue[surroundingCells, grid[[#[[1]] + 1, #[[2]] + 1]] == Vuoto &];
      ];
    ];
    
    (* Marca le celle occupate dalla nave *)
    Do[
      grid[[coord[[1]] + 1, coord[[2]] + 1]] = Nave,
      {coord, shipCoords}
    ];
    
    (* Aggiunta della nave alla lista *)
    AppendTo[ships, shipCoords];
    (* Debug - visualizzazione della lunghezza e delle coordinate
    Print["Added ship of length: ", shipLength, " at coordinates: ", shipCoords];*)
    
  , {shipLength, shipLengths}]; (* <-- Qui \[EGrave] la correzione principale: iteriamo su ogni elemento di shipLengths *)
  
  (* Debug - stampa il numero finale di navi generate 
  Print["Total ships generated: ", Length[ships]];*)
  
  (* Return della lista delle navi *)
  ships
];
(* FUNZIONE per fare lo StartGame *)
StartGame[userShips_, CPUShips_, userGridInit_, cpuGridInit_, userBase_, gridSize_] := Module[
  {attackCoordsResult, attackCpuCoords, result, userAttack, cpuAttack},

  DynamicModule[
  {gameState = "In corso...", messageUser = "", messageCpu = "", cpuGrid = cpuGridInit, userGrid = userGridInit, 
   countAffondato = 0, cpuAffondato = 0, gameOver = False, input = ""},
    (* Reset dell'insieme delle coordinate attaccate dalla CPU *)
    CPUAttackedCoordinates = {};
    
    Column[{
      (* Titolo *)
      Style["Inizia la battaglia!!!", Bold, 24, Red],
      Row[{
        (* Griglia utente *)
        Column[{
          Style["Le tue navi", Bold, 14],
          Dynamic[showGrid[userGrid, True]]
        }],
        Spacer[30],
        (* Campo d'attacco *)
        Column[{
          Style["Campo d'attacco", Bold, 14],
          Dynamic[showGrid[cpuGrid, False]]
        }]
      }],
      Spacer[20],
      (* Controlli di gioco *)
      Grid[{
        {"Inserisci la cella da attaccare:", 
          InputField[Dynamic[input], String, ImageSize -> {150, 30}, Enabled -> Dynamic[!gameOver]],
          Button["Fire!",
            (*Attacco dell'Utente*)
            attackCoordsResult = verifyInput[gridSize, userBase, input];
            
            (* Gestione dei messaggi di errore di verifyInput *)
            If[ListQ[attackCoordsResult] && Length[attackCoordsResult] == 2 && attackCoordsResult[[1]] === $Failed,
              messageUser = attackCoordsResult[[2]],
              (* Esegui l'attacco se l'input \[EGrave] valido *)
              userAttack = attack[attackCoordsResult[[1]], cpuGrid, CPUShips];
              
              (* Stato del gioco parte utente *)
              messageUser = "Coordinate attaccate " <> ToString[attackCoordsResult[[1]]] <> ". " <> userAttack[[1]];
              
              (* Controllo nave affondata e incremento del contatore *)
              If[userAttack[[4]], 
                (* Assegnazione esplicita invece di incremento *)
                countAffondato = countAffondato + 1
              ];
              (* Controllo in caso di vittoria del UTENTE *)
              If[userAttack[[3]] && countAffondato >= Length[CPUShips] && Length[CPUShips]>0, 
                gameState = "Complimenti! Hai vinto!";
                gameOver = True;
                cpuGrid = userAttack[[2]];
              ];
              
              (* Solo se l'attacco dell'utente \[EGrave] andato a buon fine, la CPU attacca *)
              If[userAttack[[3]] && !gameOver, (*input valido e attacco utente effettuato*)
              
                (*Attacco della CPU*)
                cpuGrid = userAttack[[2]];
                (* Genera nuove coordinate CPU fino a trovare una cella non attaccata *)
                attackCpuCoords = generateCoordinate[gridSize];
                cpuAttack = attack[attackCpuCoords, userGrid, userShips];
                userGrid = cpuAttack[[2]];
                
                (* Stato del gioco parte CPU *)
                If[cpuAttack[[4]], 
                  (* Assegnazione esplicita invece di incremento *)
                  cpuAffondato = cpuAffondato + 1
                ];
                (* Controllo in caso di vittoria della CPU *)
                If[cpuAffondato >= Length[userShips], 
                  gameState = "La CPU ha vinto!";
                  gameOver = True;
                ];
                (* Impostazione di un messaggio *)
                messageCpu = Column[{
                "Coordinate attaccate " <> ToString[attackCpuCoords] <> ". " <> cpuAttack[[1]],
                Row[{"La conversione \[EGrave]: ", Row[attackCpuCoords], " = ",BaseForm[FromDigits[attackCpuCoords],userBase]}]
                }];
              ];
            ]
            (* Impostazione della dimensione e abilitiamo l\[CloseCurlyQuote]interazione dinamicamente *)
          , ImageSize -> {80, 30}, Enabled -> Dynamic[!gameOver]]
        }
      }],
      Row[{helpUser[userBase],Spacer[10], helpUserPersonalized[userBase]}],
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
