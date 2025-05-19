(* ::Package:: *)

(* :Title: Battle *)
(* :Context: Battle` *)
(* :Author: Dan Cernei, Daniele Russo *)
(* :Version: 2.1 *)
(* :Date: 2025-05-06 *)

(* :Summary: 
   Questo pacchetto implementa la logica di gioco della battaglia navale,
   inclusa la generazione delle navi della CPU,
   l'inizializzazione del gioco,
   il sistema di attacco e la gestione del turno.
*)

(* :Copyright: A colpi di Bit (C) 2025 *)
(* :Keywords: battaglia navale, attacco, navi, CPU *)
(* :Requirements: Mathematica 12.0+, Util` *)

(* File: Battle.m *)
BeginPackage["Battle`", {"Util`", "Interaction`"}]; 


generateCoordinate::usage = "generateCoordinate[gridSize] genera casualmente una coppia di coordinate (x,y).";
attack::usage = "attack[attackCoords, grid, ships] attacca alle coordinate specificate, restituisce:\
attackResult \[RightArrow] messaggio,
newGrid \[RightArrow] griglia aggiornata,
hit\[RightArrow]booleano che indica se l'attacco \[EGrave] andato a buon fine,
naveAffondata \[RightArrow] booleano, indica se l'attacco ha affondato una nave.";
generateCPUShips::usage = "generateCPUShips[gridSize, seed] genera casualmente le navi della CPU.";
startGame::usage = "startGame[userShips, CPUShips, userGridInit, cpuGridInit,userBase, gridSize] avvia il gioco (battaglia).";
initPhase::usage =
  "initPhase[base, gridSize] inizializza il gioco: base \[Element] {2,8,16}, gridSize \[EGrave] la dimensione della griglia.\
Imposta UserBase, GridSize, CpuGrid e UserGrid, resetta gli stati.";
resetGame::usage =
  "resetGame[] svuota tutte le liste e ripristina le variabili globali all'inizio del gioco.";

Begin["`Private`"];
(* Utilizziamo una lista per tenere traccia delle coordinate gi\[AGrave] attaccate dalla CPU 
cos\[IGrave] possiamo evitare di far attaccare al pc una stessa cella*)
CPUAttackedCoordinates = {};

(* Inizializza contesto CPU e utente *)
(*Prende i valori inseriti dall'utente per il seed, la base e il livello di difficolt\[AGrave]*)
initPhase[seed_Integer, base_Integer, difficultyLevel_Integer] := Module[
  {gridSize, shipLengths},
  
  (* Reset delle variabili per sicurezza (stati globali usati per la gestione del gioco)*)
  resetGame[];
  
  (* Ottieni gridSize e shipLengths in base al livello di difficolt\[AGrave].*)
  gridSize = getDifficultyLevels[][[difficultyLevel, 2]]; (*prendo il secondo elemento di difficultyLevels (dimensione della griglia)*)
  shipLengths = getDifficultyLevels[][[difficultyLevel, 3]]; (*prendo il terzo elemento di difficultyLevels (lunghezza delle navi)*)
  (*difficultyLevels \[EGrave] definita in Interaction.m e definisce le impostazioni dei livelli di difficolt\[AGrave]:
  - livello (primo elemento della lista)
  - dimensione della griglia (secondo elemento della lista)
  - lunghezza delle navi (terzo elemento della lista)*)
 
  (* Inizializza tutti i valori necessari tramite setters definiti in Interaction.m*)
  setShipLengths[shipLengths]; (*lunghezza delle navi*)
  initSeed[seed]; (*imposto il seed*)
  setUserBase[base]; (*imposto la base*)
  setGridSize[gridSize]; (*imposto la dimensione della griglia*)
  
  (* Genera casualmente navi CPU *)
  (*la generazione avviene tramite generateCPUShips*)
  Block[{cpuShips = generateCPUShips[gridSize]},
  (*setters definiti in Interaction.m*)
    setCPUShips[cpuShips]; (*imposto le navi della CPU appena generate*)
    setCPUGrid[createGrid[cpuShips, gridSize]]; (*imposto la griglia della CPU in base alle navi generate*)
  ];
  
  (* Inizializza la lista delle navi e la griglia dell'utente vuote *)
  setUserShips[{}];
  setUserGrid[ConstantArray[Vuoto, {gridSize, gridSize}]];
  
  True (* Segnala successo *)
];

(* Reset di tutte le variabili e liste *)
resetGame[] := Module[{},
  setUserBase[10];
  setGridSize[10];
  setCPUShips[{}];
  setUserShips[{}];
  setCPUGrid[{}];
  setUserGrid[{}];
  setSeed[RandomInteger[1024]];
  setShipLengths[{5, 4, 3, 2, 1}];
  CPUAttackedCoordinates = {};
];


(* Funzione per la generazione casuale degli attacchi della CPU*)
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

(* Funzione attack che gestisce l'attacco dei giocatori alle coordinate specificate *)
attack[attackCoordsWithMsg_, grid_, ships_] := Module[
  {attackResult = "", newGrid = grid, r, c, hit = False, naveAffondata = False, attackCoords},
  (*- attackResult \[RightArrow] messaggio ottenuto dopo l'attacco, per indicare all'utente se l'attacco \[EGrave] andato a buon fine o meno,
   - newGrid \[RightArrow] griglia del giocatore ottenuta dopo l'attacco, si modifica la cella attaccata
   - r, c \[RightArrow] riga e colonna della cella attaccata,
   - hit \[RightArrow] booleano che indica se l'attacco \[EGrave] stato effettuato (rimane False se le coordinate attaccate non sono valide),
   - naveAffondata \[RightArrow] booleano che indica se l'attacco ha affondato una nave.
   - attackCoords \[RightArrow] coordinate attaccate estratte da attackCoordsWithMsg
   
   I due booleani hit e naveAffondata servono in startGame, definita in questo pacchetto, per
   conoscere lo stato dell'attacco e aggiornare di conseguenza lo stato della partita*)
   
  (* Se l'input attackCoordsWithMsg \[EGrave] una tupla {$Failed, errorMsg}, estraiamo il messaggio di errore specifico.
  Questo ci dice che le coordinate non sono valide quindi non si deve eseguire l'attacco*)
  If[ListQ[attackCoordsWithMsg] && Length[attackCoordsWithMsg] == 2 && attackCoordsWithMsg[[1]] === $Failed, 
    Return[{attackCoordsWithMsg[[2]], grid, False, False}]
  ];
  
  (* Se l'input attackCoordsWithMsg \[EGrave] valido, estraiamo le coordinate corrette *)
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
  
  (* uso r e c per far riferimento a riga e colonna di attacco con pi\[UGrave] facilit\[AGrave]*)
  r = attackCoords[[1]] + 1; (*estraggo la riga*)
  c = attackCoords[[2]] + 1; (*estraggo la colonna*)
  
  (* Controlla che le coordinate estratte siano all'interno della griglia *)
  If[r < 1 || r > Length[grid] || c < 1 || c > Length[grid[[1]]],
    Return[{"Coordinate fuori dalla griglia!", grid, False, False}]
  ];
  
  (* Controlla il contenuto della cella (r, c) della griglia *)
  Switch[grid[[r, c]], 
    Nave, (* nella cella c'\[EGrave] una nave: quindi COLPITO *)
	(* Aggiorna la nuova griglia (newGrid) segnando la cella come colpita *)    
      newGrid[[r, c]] = Colpito; 
      hit = True; (*l'attacco ha avuto successo*)
      
      (* Controlla se oltre a colpire ha anche affondato la nave*)
      Do[
      (* Per ogni nave controlla se la cella attaccata \[EGrave] una cella occupata della nave stessa.
       L'indice \[EGrave] scalato di -1, perch\[EGrave] le coordinate delle navi siano basate su 0 *)
        If[MemberQ[nave, {r - 1, c - 1}],
        (*Se appartiene alla nave*)
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
      
      (* Assegna un messaggio di esito *)    
      If[naveAffondata,
        attackResult = "Colpito e affondato!",
        attackResult = "Colpito!"
      ];,       
      
    Vuoto, (* la cella attaccata \[EGrave] vuota: quindi MANCATO *)
      hit = True; (*l'attacco \[EGrave] andato a buon fine*)
      newGrid[[r, c]] = Mancato;
      (*Assegna un messaggio di esito*)
      attackResult = "Mi dispiace. Colpo non andato a segno, tenta di nuovo...";,
    Colpito, (* la cella attaccata \[EGrave] colpita: quindi GI\[CapitalAGrave] COLPITO *)
      attackResult = "Hai gi\[AGrave] colpito qui!"
  ];
  (*  Ritorna una quadrupla *)
  {attackResult, newGrid, hit, naveAffondata}
];

(* FUNZIONE per generare le navi della CPU *)
generateCPUShips[gridSize_Integer] := Module[
  {ships = {}, shipLengths, grid, orientation, startRow, startCol, shipCoords, valid, surroundingCells},
  
  (* Inizializza della griglia vuota *)
  grid = ConstantArray[Vuoto, {gridSize, gridSize}];

  (* Utilizza le lunghezze delle navi definite dalla variabile globale ShipLengths *)
  shipLengths = getRemainingShipLengths[];
  
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

attackHandler[input_, gridSize_, userBase_, cpuGrid_, CPUShips_, userGrid_, userShips_, 
              gameState_, gameOver_, countAffondato_, cpuAffondato_] := Module[
  {attackCoordsResult, messageUser = "", messageCpu = "", userAttack, cpuAttack, 
   attackCpuCoords, newCpuGrid = cpuGrid, newUserGrid = userGrid, 
   newGameState = gameState, newGameOver = gameOver, 
   newCountAffondato = countAffondato, newCpuAffondato = cpuAffondato},
  
  (* Attacco dell'Utente *)
  attackCoordsResult = verifyInput[gridSize, userBase, input]; (* controllo l'input inserito con verifyInput definita in Util.m *)
  
  (* Gestione dei messaggi di errore di verifyInput *)
  If[ListQ[attackCoordsResult] && Length[attackCoordsResult] == 2 && attackCoordsResult[[1]] === $Failed,
    messageUser = attackCoordsResult[[2]],
    (* Esegui l'attacco se l'input è valido *)
    userAttack = attack[attackCoordsResult[[1]], cpuGrid, CPUShips]; 
    
    (* Stato del gioco parte utente *)
    messageUser = "Coordinate attaccate " <> ToString[attackCoordsResult[[1]]] <> ". " <> userAttack[[1]];
    
    (* Controllo nave affondata e incremento del contatore *)
    If[userAttack[[4]], 
      (* Assegnazione esplicita invece di incremento *)
      newCountAffondato = countAffondato + 1
    ];
    
    (* Controllo in caso di vittoria dell'UTENTE *)
    If[userAttack[[3]] && newCountAffondato >= Length[CPUShips] && Length[CPUShips] > 0, 
      newGameState = "Complimenti! Hai vinto!";
      newGameOver = True;
      newCpuGrid = userAttack[[2]];
    ];
    
    (* Solo se l'attacco dell'utente è andato a buon fine, la CPU attacca *)
    If[userAttack[[3]] && !newGameOver, (* input valido e attacco utente effettuato *)
      
      (* Attacco della CPU *)
      newCpuGrid = userAttack[[2]];
      (* Genera nuove coordinate CPU fino a trovare una cella non attaccata *)
      attackCpuCoords = generateCoordinate[gridSize];
      cpuAttack = attack[attackCpuCoords, userGrid, userShips];
      newUserGrid = cpuAttack[[2]];
      
      (* Stato del gioco parte CPU *)
      If[cpuAttack[[4]], 
        (* Assegnazione esplicita invece di incremento *)
        newCpuAffondato = cpuAffondato + 1
      ];
      
      (* Controllo in caso di vittoria della CPU *)
      If[newCpuAffondato >= Length[userShips], 
        newGameState = "La CPU ha vinto!";
        newGameOver = True;
      ];
      
      (* Impostazione di un messaggio *)
      messageCpu = Column[{
        "Coordinate attaccate " <> ToString[attackCpuCoords] <> ". " <> cpuAttack[[1]],
        (* mostro anche conversione in binario della cella attaccata dalla cpu come aiuto per l'utente *)
        Row[{"La conversione è: ", Subscript[FromDigits[attackCpuCoords, 10], 10], " = ", BaseForm[FromDigits[attackCpuCoords], userBase]}]
      }];
    ];
  ];
  
  (* Ritorna una Association con tutti i valori aggiornati *)
  <|
    "messageUser" -> messageUser,
    "messageCpu" -> messageCpu,
    "cpuGrid" -> newCpuGrid,
    "userGrid" -> newUserGrid,
    "gameState" -> newGameState,
    "gameOver" -> newGameOver,
    "countAffondato" -> newCountAffondato,
    "cpuAffondato" -> newCpuAffondato
  |>
];

(*ANNIDATO DynamicModule in module*)
(* FUNZIONE per fare lo startGame *)
startGame[userShips_, CPUShips_, userGridInit_, cpuGridInit_, userBase_, gridSize_] := 
DynamicModule[
  {gameState = "In corso...", messageUser = "", messageCpu = "", cpuGrid = cpuGridInit, 
   userGrid = userGridInit, countAffondato = 0, cpuAffondato = 0, gameOver = False, input = "",
   attackCoordsResult, attackCpuCoords, result, userAttack, cpuAttack},
    (* Reset dell'insieme delle coordinate attaccate dalla CPU *)
    CPUAttackedCoordinates = {};
    
    Column[{
      (* Titolo *)
      Style["Inizia la battaglia!!!", Bold, 24, Red],
      (*Stato della partita*)
      Dynamic[Style["Stato del gioco: "<>gameState, Bold, 14, Darker[Green]]],
      Spacer[5],
      Row[{
        (* Griglia utente *)
        Column[{
          Style["Le tue navi", Bold, 14],
          Dynamic[showGrid[userGrid, True]] (*richiamo showGrid, definita in Util.m, per mostrare la griglia dell'utente*)
        }],
        Spacer[30],
        (* Campo d'attacco *)
        Column[{
          Style["Campo d'attacco", Bold, 14],
          Dynamic[showGrid[cpuGrid, False]] (*richiamo showGrid, definita in Util.m, per mostrare la griglia della cpu*)
        }]
      }],
      Spacer[20],
      (* Controlli di gioco *)
      Grid[{
        {"Inserisci la cella da attaccare:", 
          InputField[Dynamic[input], String, ImageSize -> {150, 30}, Enabled -> Dynamic[!gameOver]],
          Button["Fire!",
          result = attackHandler[input, gridSize, userBase, cpuGrid, CPUShips, 
                                 userGrid, userShips, gameState, gameOver, 
                                 countAffondato, cpuAffondato];
          
          (* Aggiorna tutte le variabili con i risultati *)
          messageUser = result["messageUser"];
          messageCpu = result["messageCpu"];
          cpuGrid = result["cpuGrid"];
          userGrid = result["userGrid"];
          gameState = result["gameState"];
          gameOver = result["gameOver"];
          countAffondato = result["countAffondato"];
          cpuAffondato = result["cpuAffondato"];
          
          (* Pulisce l'input dopo l'attacco *)
          input = "";, 
          ImageSize -> {80, 30}, Enabled -> Dynamic[!gameOver]]
        }
      }],
      Row[{helpUser[userBase],Spacer[10], helpUserPersonalized[userBase]}], (*pulsanti per chiedere suggerimento*)
      (*helpUser e helpUserPersonalized sono definite in Interaction.m*)
      Spacer[10],
      Style["Attacco Utente:", Bold, 12],
        Dynamic[messageUser],
        Spacer[15],
        Style["Attacco CPU:", Bold, 12],
        Dynamic[messageCpu]     
    }]
]

End[];

EndPackage[];
