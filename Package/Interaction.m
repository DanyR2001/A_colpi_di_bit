(* ::Package:: *)

(* file: package.m *)

BeginPackage["Interaction`"];
AskSeedInput::usage = "AskSeedInput[inputSeed] chiede all'untete di inserire il seed"
AskBaseChoice::usage = "AskBasechoice[inputBase] chiede all'utente di inserire la base su cui si vuole esercitare (2, 8, 16)"

isBase::usage="isBase[] controlla che la base sia stata inserita e sia 2, 8 o 16";
isSeed::usage="isSeed[] controlla che il seed sia stato inserito correttamente (se \[EGrave] un numero intero)";

GenerateShips::usage =
  "GenerateShips[seed] genera 4 navi automatiche (lunghezze 5,4,3,2) in griglia di lato $GridSize senza overlap.";

InitPhase::usage =
  "InitPhase[base, gridSize] inizializza il gioco: base \[Element] {2,8,16}, gridSize \[EGrave] lato griglia.\
Imposta $UserBase, $GridSize, $AutoGrid e $UserGrid, resetta gli stati.";

PlaceUserShip::usage =
  "PlaceUserShip[startRaw, endRaw] piazza in $UserGrid una nave.\
startRaw e endRaw sono stringhe di un intero in base $UserBase (0..gridSize^2-1),\
con 0 corrispondente in basso a sinistra. Si possono piazzare al massimo 4 navi, una per ciascuna lunghezza 5,4,3,2.";

ResetGame::usage =
  "ResetGame[] svuota tutte le liste e ripristina le variabili globali all'inizio del gioco.";

GetAutomaticShips::usage     = "GetAutomaticShips[] restituisce lista di blocchi delle navi PC.";
GetUserShips::usage          = "GetUserShips[] restituisce lista di blocchi delle navi utente.";
GetAutomaticEndpoints::usage = "GetAutomaticEndpoints[] restituisce lista di coppie {start,end} decimali per ciascuna nave PC.";
GetUserEndpoints::usage      = "GetUserEndpoints[] restituisce lista di coppie {start,end} decimali per ciascuna nave utente.";
GetAutoGrid::usage           = "GetAutoGrid[] restituisce matrice griglia PC.";
GetUserGrid::usage           = "GetUserGrid[] restituisce matrice griglia utente.";

GetSeed::usage="GetSeed[] restituisce il valore del seed"
GetBase::usage="GerBase[] restituisce la base (2, 8, 16)"

ShowPCGrid::usage            = "ShowPCGrid[] mostra dinamicamente la griglia PC.";
ShowUserGrid::usage          = "ShowUserGrid[] mostra dinamicamente la griglia utente.";
ShowState::usage             = "ShowState[] mostra entrambe le griglie e le coordinate di tutte le navi.";

showMessage::usage="showMessage[message] stampa un messaggio";
showError::usage="showError[error] stampa un messaggio di errore (in rosso)";

Begin["`Private`"];

(* Stati globali *)
$AutomaticShips = {};
$UserShips      = {};
$AutoGrid       = {};
$UserGrid       = {};
$UserBase       = 10;
$GridSize       = 10;
$Seed= ""; 
(* Reset di tutte le variabili e liste *)
ResetGame[] := (
  $UserBase       = 10;
  $GridSize       = 10;
  $AutomaticShips = {};
  $UserShips      = {};
  $AutoGrid       = {};
  $UserGrid       = {};
  $Seed= ""; 
);

(* Inizializza contesto PC e utente *)
InitPhase[seed_Integer,base_Integer, gridSize_Integer] := (
  $Seed= seed; 
  $UserBase       = base;
  $GridSize       = gridSize;
  $AutomaticShips = {};
  $UserShips      = {};
  $AutoGrid       = ConstantArray[0, {gridSize, gridSize}];
  $UserGrid       = ConstantArray[0, {gridSize, gridSize}];
);


(*richiedi seed e base*)
AskSeedInput[inputSeed_]:= DynamicModule[{value="", message=""},
	Column[{
		Row[{
			"Inserisci seed: ",
			InputField[Dynamic[value], Number, ImageSize->Small],
			Button["Imposta", 
				If[isSeed[value], 
					inputSeed[value];
					message="Seed inserito: "<>ToString[value];,
					message="Seed non valido. Inserisci un numero intero!";
				]
			]
		}],
		Dynamic[message]
	}]
];

AskBaseChoice[inputBase_]:= DynamicModule[{value=2, message=""},
	Column[{
		Row[{
			"Inserisci la base su cui ti vuoi esercitare: ",
			PopupMenu[Dynamic[value], {2,8,16}],
			Button["Inserisci",inputBase[value]; message="Base inserita: "<>ToString[value];]
		}],
		Dynamic[message]
	}]
];

(*Mostra messaggi e errori*)
showMessage[message_String]:=Style[message,Blue];
showError[error_String]:=Style["Attenzione: "<>error, Red];

(*Controlla che i valori di input siano corretti*)
isBase[base_]:=MemberQ[{2,8,16}, base];
isSeed[seed_]:=IntegerQ[seed]; 

(* Genera navi PC *)
GenerateShips[seed_Integer] := Module[
  {lengths = {5, 4, 3, 2}, placed = {}, orient, coords, r0, c0, grid},
  SeedRandom[seed];
  grid = ConstantArray[0, {$GridSize, $GridSize}];
  Do[
    While[True,
      orient = RandomChoice[{"H","V"}];
      If[orient === "H",
        r0 = RandomInteger[{1, $GridSize}];
        c0 = RandomInteger[{1, $GridSize + 1 - shipLen}];
        coords = Table[{r0, c0 + i}, {i, 0, shipLen - 1}],
        (* verticale *)
        r0 = RandomInteger[{1, $GridSize + 1 - shipLen}];
        c0 = RandomInteger[{1, $GridSize}];
        coords = Table[{r0 + i, c0}, {i, 0, shipLen - 1}]
      ];
      If[AllTrue[coords, (1 <= #[[1]] <= $GridSize && 1 <= #[[2]] <= $GridSize && grid[[Sequence @@ #]] == 0) &], Break[]];
    ];
    Scan[(grid[[Sequence @@ #]] = 1) &, coords];
    AppendTo[placed, coords],
    {shipLen, lengths}
  ];
  $AutomaticShips = placed;
  $AutoGrid       = grid;
  grid
];

(* Conversione raw String -> {r,c} *)
parseCoord[str_String] := Module[{n, r, c},
  n = Quiet@FromDigits[str, $UserBase];
  If[! IntegerQ[n] || n < 0 || n >= $GridSize^2, Return[$Failed]];
  c = Mod[n, $GridSize] + 1;
  r = $GridSize - Quotient[n, $GridSize];
  {r, c}
];

(* Piazzamento nave utente con vincoli lunghezza e numero *)
PlaceUserShip[startRaw_String, endRaw_String] := Module[
  {start, end, r1, c1, r2, c2, len, coords, usedLens},
  If[Length[$UserShips] >= 4, Return[$Failed]];
  start = parseCoord[startRaw];  end   = parseCoord[endRaw];
  If[start === $Failed || end === $Failed, Return[$Failed]];
  {r1, c1} = start; {r2, c2} = end;
  If[Not[r1 == r2 || c1 == c2], Return[$Failed]];
  len = Max[Abs[r2 - r1], Abs[c2 - c1]] + 1;
  usedLens = Length /@ $UserShips;
  If[! MemberQ[{5, 4, 3, 2}, len] || MemberQ[usedLens, len], Return[$Failed]];
  coords = Table[{r1 + i Sign[r2 - r1], c1 + i Sign[c2 - c1]}, {i, 0, len - 1}];
  If[Not[AllTrue[coords, 1 <= #[[1]] <= $GridSize && 1 <= #[[2]] <= $GridSize &]], Return[$Failed]];
  If[Not[AllTrue[coords, $UserGrid[[Sequence @@ #]] == 0 &]], Return[$Failed]];
  $UserGrid = ReplacePart[$UserGrid, Thread[coords -> 1]];
  AppendTo[$UserShips, coords];
  $UserGrid
];

(* Getters *)
GetAutomaticShips[] := $AutomaticShips;
GetUserShips[]      := $UserShips;
GetAutoGrid[]       := $AutoGrid;
GetUserGrid[]       := $UserGrid;

(* Endpoint come raw decimal *)
GetAutomaticEndpoints[] := Module[{gs = $GridSize},
  Map[Function[coords,
    With[{s = First[coords], e = Last[coords]},
      {(gs - s[[1]])*gs + (s[[2]] - 1), (gs - e[[1]])*gs + (e[[2]] - 1)}]],
    $AutomaticShips
  ]
];

GetUserEndpoints[] := Module[{gs = $GridSize},
  Map[Function[coords,
    With[{s = First[coords], e = Last[coords]},
      {(gs - s[[1]])*gs + (s[[2]] - 1), (gs - e[[1]])*gs + (e[[2]] - 1)}]],
    $UserShips
  ]
];

(* Visualizzazione dinamica *)
ShowPCGrid[] := Dynamic[Grid[Map[If[# == 1, "\[FilledSquare]", "\[EmptySquare]"] &, $AutoGrid, {2}], Frame -> True]];

ShowUserGrid[] := Dynamic[Grid[Map[If[# == 1, "\[FilledSquare]", "\[EmptySquare]"] &, $UserGrid, {2}], Frame -> True]];

(* Stato completo *)
ShowState[] := Column[{"PC Grid:", ShowPCGrid[], "User Grid:", ShowUserGrid[],
    "Automatic Ships:", $AutomaticShips, "User Ships:", $UserShips}];

End[];
EndPackage[];
