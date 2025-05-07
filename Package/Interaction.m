(* ::Package:: *)

(* :Title: Interaction *)
(* :Context: Interaction` *)
(* :Author: Dan Cernei, Matilde Nardi, Daniele Russo *)
(* :Version: 2.0 *)
(* :Date: 2025-05-06 *)

(* :Summary: 
   Questo pacchetto gestisce l'interazione con l'utente, incluso l'input del seed,
   la scelta della base numerica e il posizionamento delle navi.
*)

(* :Copyright: A colpi di Bit (C) 2025 *)
(* :Keywords: battaglia navale, interazione, posizionamento, input *)
(* :Requirements: Mathematica 12.0+, Util` *)

(* File: Interaction.m *)

BeginPackage["Interaction`", {"Util`"}];
AskSeedInput::usage = "AskSeedInput[inputSeed] chiede all'untete di inserire il seed";
AskBaseChoice::usage = "AskBasechoice[inputBase] chiede all'utente di inserire la base su cui si vuole esercitare (2, 8, 16)";

isBase::usage="isBase[base] controlla che la base sia inserita sia 2, 8 o 16";
isSeed::usage="isSeed[seed] controlla che il seed sia stato inserito correttamente (se \[EGrave] un numero intero)";
helpUser::usage="helpUser[base] apre finestra per mostrare un suggerimento all'utente (ripasso conversioni tra basi)";
helpUserPersonalized::usage="helpUserPersonalized[base] apre finestra per chiedere all'utente di inserire un numero decimale e mostra la sua conversione in base scelta";
exercise::usage="exercise[base,numberDec] mostra come avviene la conversione del numero da base 10 a base scelta";

PlaceUserShip::usage =
  "PlaceUserShip[startRaw, endRaw] piazza in $UserGrid una nave.\
startRaw e endRaw sono stringhe di un intero in base $UserBase (0..gridSize^2-1),\
con 0 corrispondente in basso a sinistra. Si possono piazzare al massimo 4 navi, una per ciascuna lunghezza 5,4,3,2.\
Restituisce {True, grid} in caso di successo o {False, errorMsg} in caso di errore.";


(* Aggiungiamo esplicitamente questi getter mancanti *)
GetUserShips::usage = "GetUserShips[] restituisce lista di blocchi delle navi utente.";
GetUserGrid::usage = "GetUserGrid[] restituisce matrice griglia utente.";
GetRemainingShipLengths::usage = "GetRemainingShipLengths[] restituisce le lunghezze delle navi ancora da piazzare.";
GetDifficultyLevels::usage = "GetDifficultyLevels[] restituisce i livelli di difficolt\[AGrave] disponibili";
GetCpuShip::usage="GetCpuShip[] restituisce le navi della CPU";
GetCpuGrid::usage="GetCpuGrid[] resitiruisce la griglia delle CPU";

SetShipLengths::unsage="SetShipLengths[shipLengths_List] := $ShipLengths = shipLengths";
SetUserBase::usage="SetUserBase[base_Integer] := If[isBase[base], $UserBase = base, $UserBase]";
SetGridSize::usega="SetGridSize[size_Integer] := If[size > 0, $GridSize = size, $GridSize]";
SetAutomaticShips::usage="SetAutomaticShips[ships_List] := $AutomaticShips = ships";
SetUserShips::usage="SetUserShips[ships_List] := $UserShips = ships";
SetAutomaticGrid::usage="SetAutomaticGrid[grid_List] := $AutomaticGrid = grid";
SetUserGrid::usage="SetUserGrid[grid_List] := $UserGrid = grid";
SetSeed::usage="SetSeed[seed_] := If[isSeed[seed], $Seed = seed, $Seed]";

showMessage::usage="showMessage[message] stampa un messaggio";
showError::usage="showError[error] stampa un messaggio di errore (in rosso)";

Begin["`Private`"];

(*livelli di difficolt\[AGrave]*)
$DifficultyLevels = {
  {"Facile", 3, {1}},         (* Livello facile: griglia 3x3, 1 nave di lunghezza 1 *)
  {"Medio", 6, {3, 2, 1}},    (* Livello medio: griglia 6x6, 3 navi di lunghezza 3,2,1 *)
  {"Difficile", 10, {5, 4, 3, 2, 1}}  (* Livello difficile: griglia 10x10, 5 navi di lunghezza 5,4,3,2,1 *)
};


(* Stati globali *)
$AutomaticShips = {};
$UserShips      = {};
$AutomaticGrid  = {};
$UserGrid       = {};
$UserBase       = 10;
$GridSize       = 10;
$Seed= ""; 
$ShipLengths = {5, 4, 3, 2, 1}; 

(*richiedi seed e base*)
AskSeedInput[inputSeed_] := DynamicModule[{value = RandomInteger[1024]},
  Row[{
    "Inserisci seed: ",
    InputField[Dynamic[value, (value = #; inputSeed[value])&], Number, ImageSize -> Small]
  }]
];

(* Funzione AskBaseChoice modificata *)
AskBaseChoice[inputBase_]:= DynamicModule[{value = 2},
  Row[{
    "Inserisci la base su cui ti vuoi esercitare: ",
    PopupMenu[Dynamic[value, (value = #; inputBase[value])&], {2, 8, 16}]
  }]
];

(*Mostra messaggi e errori*)
showMessage[message_String]:=Style[message,Blue];
showError[error_String]:=Style["Attenzione: "<>error, Red];

(*Controlla che i valori di input siano corretti*)
isBase[base_]:=MemberQ[{2,8,16}, base];
isSeed[seed_]:=IntegerQ[seed]; 

helpUserPersonalized[base_Integer]:=PopupWindow[
	Button["Chiedi Aiuto"],
	DynamicModule[{numberDec=0,error="",ex=""},
		Style[
			Column[{
				"Inserisci un numero intero positivo da convertire in base "<>ToString[base],
				Row[{
					InputField[Dynamic[numberDec], Number, ImageSize -> Small],
					Button["Converti in base "<>ToString[base],
						If[IntegerQ[numberDec]&&numberDec>=0,
							error="";
							ex=Column[{
								Style["Conversione del numero: "<>ToString[numberDec],15,Red,Bold],
								Spacer[5],
								exercise[base,numberDec]
							}];,
							ex="";
							error=showError["Inserisci un numero intero positivo"];]
					]
				}],
				Dynamic[error],
				Dynamic[ex]
			}]
		,12]
	]

, WindowTitle -> "Suggerimento", WindowFloating -> True, WindowMargins -> {{0, Automatic}, {0, Automatic}}];

exercise[base_, numberDec_]:=Module[{numberBase, helpDescription, colors},
		colors = {Blue, Orange, Purple, Red, Brown, Pink,Green};
		
		If[!isBase[base], 
			"Per favore, prima inserisci una base valida!",
			Column[{
				If[base == 2, 
					(*Divisioni successive per 2*)
					Module[{quotients, modules, i},
						quotients = {numberDec};
						modules = {};
						i = 1;
						
						(*memorizzo i quozienti e i resti*)
						While[quotients[[i]] != 0,
							AppendTo[quotients, Quotient[quotients[[i]], 2]];
							AppendTo[modules, Mod[quotients[[i]], 2]];
							i++;
						];
						
						(*spiegazione della conversione, mostro divisioni successive e resti ottenuti *)
						Column[{" \[Bullet] Dividiamolo per 2 e annotiamo il resto fino ad arrivare a 0",
						 Row[{
							Panel[Style[ 
								Grid[
									Table[{
										(*divisione*)
										Style[ToString[quotients[[n]]], colors[[Mod[n-1, Length[colors]]+1]]],
										" \[Divide] 2 = ", 
										(*quoziente*)
										Spacer[5], Style[ToString[quotients[[n+1]]], colors[[Mod[n, Length[colors]]+1]]],
										(*resto*)
										Spacer[5], " Resto = ",
										Style[ToString[modules[[n]]], Bold]
									}, {n, 1, Length[quotients]-1}]
								], 12]
							], 
							(*freccia dal basso verso l'alto, mostra come leggere i resti*)
							Graphics[{Red, Arrowheads[0.5], Arrow[{{0, 0}, {0, i - 1}}]}, ImageSize -> 20]
						 }, Alignment -> Center, ImageSize -> Full],
						 Row[{" \[Bullet] La conversione in binario si ottiene leggendo i resti dal basso verso l'alto ", Style["(Resti \[UpArrow])", Red]}]
						}]
					],
					(*conversione in basi 8 o 16, converto in binario, raggruppo e converto in decimale*)
					Module[{digits, digitsGroups, mod, exp, table},
						(*lista delle cifre che compongono il numero binario \[RightArrow] usata per la suddivisione in gruppi*)
						digits = IntegerDigits[numberDec, 2]; 
						(*potenza di due: 16=2^4, 8=2^3 \[RightArrow] l'esponente indica la dimensione del gruppo in cui raggruppare il numero binario*)
						exp = Log2[base]; 
						(*resto \[RightArrow] indica quante cifre a sinistra rimangono escluse dal raggruppamento, serve per sapere quanti 0 aggiungere*)
						mod = Mod[Length[digits], exp]; 
						(*suddivisione del numero binario in gruppi da 3 o 4 cifre, se il resto \[EGrave] diverso da 0 vengono aggiunti degli 0 a sx*)
						digitsGroups = If[mod === 0, 
	                        Partition[digits, exp], 
	                        Partition[Flatten[{Table[0, {mod, exp - mod}], digits}], exp]
	                    ];
						
						(*spiegazione della conversione*)     
						Column[{" \[Bullet] convertiamolo in base 2",
							(*conversione in binario*)
							Row[{Panel[Style[BaseForm[numberDec, 2], 12]]}, Alignment -> Center, ImageSize -> Full],
							" \[Bullet] raccogliamo le cifre binarie in gruppi da " <> ToString[exp] <> " partendo dalla posizione pi\[UGrave] a destra (cifra meno significativa),",
							(*raggruppamento cifre*)
							Row[
								Table[
									Panel[Style[
										Row[digitsGroups[[n]]],
										colors[[Mod[n-1, Length[colors]]+1]]
									]], {n, 1, Length[digitsGroups]}
								]
							, Alignment -> Center, ImageSize -> Full],
							" \[Bullet] convertiamo ogni gruppo in decimale, ogni numero ottenuto corrisponde ad una cifra in base " <> ToString[base] <> ".",
							(*conversione in decimale*)
							Row[{Panel[Style[Grid[ 
								Table[
									{
		                                Style[Row[digitsGroups[[n]]], colors[[Mod[n-1, Length[colors]]+1]]], (*gruppo di cifre binarie*)
									    "\[RightArrow]", 
		                                FromDigits[digitsGroups[[n]], 2], (*conversione in decimale*)
									    "\[RightArrow]",
		                                BaseForm[FromDigits[digitsGroups[[n]], 2], base] (*cifra in base 8/16 corrispondente*)
		                            },
								    {n, 1, Length[digitsGroups]}
								]
							], 12]]}, Alignment -> Center, ImageSize -> Full]
						}]
					]
				],
				Spacer[5],
				Row[{
					Style["Risultato: ", Italic, 13, Bold],
					numberDec, " = ", BaseForm[numberDec, base], (*conversione da decimale a base 8/16*)
					"."
				}]
			}]
		]
	];
		
(* Versione corretta di helpUser con gestione corretta dei colori e indici *)
helpUser[base_Integer]:=PopupWindow[
	Button["Visualizza un suggerimento"],
		Module[{numberDec=RandomInteger[{1,300}]},
			(*suggerimento*)
			Style[
			Column[{
				Style["Ripassiamo le conversioni tra basi 10 e " <> ToString[base], Bold, Red, 15, TextAlignment -> Center],
				Spacer[10],
				Style["Da base 10 a base " <> ToString[base] <> " :", Underlined, Italic, 13],
				Spacer[5],
				Style["Consideriamo il numero: " <> ToString[numberDec], Bold],
				exercise[base,numberDec]
			}], 12]
		]
, WindowTitle -> "Suggerimento", WindowFloating -> True, WindowMargins -> {{0, Automatic}, {0, Automatic}}];



(* Funzione per generare una rappresentazione del numero massimo nel sistema numerico corrente *)
getMaxNumberInBase[base_, gridSize_] := Module[{maxDecimal, result},
  maxDecimal = gridSize^2 - 1;
  result = IntegerString[maxDecimal, base];
  result
];

(* Versione corretta della funzione PlaceUserShip *)
PlaceUserShip[startRaw_String, endRaw_String] := Module[
  {start, end, r1, c1, r2, c2, len, coords, usedLens, surroundingCells, isValid = True, errorMsg = "", maxDecimal, maxInBase},
  
  (* Valore massimo per la griglia attuale *)
  maxDecimal = $GridSize^2 - 1;
  maxInBase = getMaxNumberInBase[$UserBase, $GridSize];
  
  (* Controllo numero navi *)
  If[Length[$ShipLengths] == 0, 
    Return[{False, "Hai gi\[AGrave] posizionato tutte le navi permesse!"}]
  ];
  
  (* Conversione coordinate *)
  start = verifyInput[$GridSize, $UserBase, startRaw];  
  end = verifyInput[$GridSize, $UserBase, endRaw];
  
  If[start[[1]] === $Failed, 
    Return[{False, start[[2]]}]
  ];

  If[end[[1]] === $Failed, 
    Return[{False, end[[2]]}]
  ];

  (* Otteniamo solo le coordinate effettive da start ed end *)
  {r1, c1} = start[[1]]; {r2, c2} = end[[1]];
  
  (* Controllo allineamento *)
  If[Not[r1 == r2 || c1 == c2], 
    Return[{False, "La nave deve essere allineata orizzontalmente o verticalmente!"}]
  ];
  
  (* Calcolo lunghezza *)
  len = Max[Abs[r2 - r1], Abs[c2 - c1]] + 1;
  
  (* Controllo lunghezze usate *)
  usedLens = Length /@ $UserShips;
  
  (* Controllo vincoli sulle lunghezze *)
  If[! MemberQ[$ShipLengths, len], 
    Return[{False, "Lunghezza non valida! Le navi devono essere di lunghezza " <> 
        StringJoin[Riffle[ToString /@ $ShipLengths, ", "]] <> "."}]
  ];
  
  If[MemberQ[usedLens, len], 
    (* Calcola e mostra le lunghezze mancanti *)
    errorMsg = "Hai gi\[AGrave] piazzato una nave di lunghezza " <> ToString[len] <> "!";
    
    (* Aggiungi informazioni sulle navi mancanti *)
    If[Length[$ShipLengths] > 0,
      errorMsg = errorMsg <> " Mancano ancora le navi di lunghezza: " <> 
                ToString[Complement[$ShipLengths, usedLens]]
    ];
    
    Return[{False, errorMsg}]
  ];
  
  (* Calcolo coordinate della nave *)
  coords = If[r1 == r2,
    (* Nave orizzontale *)
    Table[{r1, Min[c1, c2] + i}, {i, 0, Abs[c1 - c2]}],
    (* Nave verticale *)
    Table[{Min[r1, r2] + i, c1}, {i, 0, Abs[r1 - r2]}]
  ];
  
  (* Controllo limiti griglia *)
  If[Not[AllTrue[coords, 0 <= #[[1]] < $GridSize && 0 <= #[[2]] < $GridSize &]], 
    Return[{False, "La nave uscirebbe dalla griglia!"}]
  ];
  
  (* Calcolo celle circostanti per ogni cella della nave *)
  surroundingCells = Flatten[
    Table[
      (* Per ogni cella della nave, genera le 8 celle circostanti *)
      Table[
        {coords[[i, 1]] + dr, coords[[i, 2]] + dc}, 
        {dr, -1, 1}, {dc, -1, 1}
      ],
      {i, 1, Length[coords]}
    ], 2];
  
  (* Filtriamo le celle circostanti per rimuovere le celle effettive della nave 
     e quelle fuori dalla griglia *)
  surroundingCells = Select[surroundingCells, 
    !MemberQ[coords, #] && 
    0 <= #[[1]] < $GridSize && 
    0 <= #[[2]] < $GridSize &
  ];
  
  (* Controlliamo prima tutte le celle della nave *)
  isValid = AllTrue[coords, $UserGrid[[#[[1]] + 1, #[[2]] + 1]] == $Vuoto &];
  If[!isValid,
    Return[{False, "La nave si sovrappone a una nave gi\[AGrave] posizionata!"}]
  ];
  
  (* Verifichiamo che non ci siano navi adiacenti *)
  isValid = AllTrue[surroundingCells, $UserGrid[[#[[1]] + 1, #[[2]] + 1]] != $Nave &];
  If[!isValid,
    Return[{False, "Non puoi posizionare una nave adiacente ad un'altra nave!"}]
  ];
  
  (* Solo se arriviamo qui, aggiorniamo la griglia inserendo le navi *)
  Do[
    $UserGrid[[coords[[i, 1]] + 1, coords[[i, 2]] + 1]] = $Nave,
    {i, 1, Length[coords]}
  ];
  
  AppendTo[$UserShips, coords];
  
  (* Rimuovi la lunghezza della nave appena posizionata dall'elenco disponibile *)
  $ShipLengths = DeleteCases[$ShipLengths, len];
  
  {True, "Nave piazzata con successo!"}
];


(* Getters *)
GetUserShips[] := $UserShips;
GetUserGrid[] := $UserGrid;
GetRemainingShipLengths[] := $ShipLengths;
GetShipSize[] := $ShipSize;
GetDifficultyLevels[] := $DifficultyLevels;
GetCpuShip[]:=$AutomaticShips;
GetCpuGrid[]:=$AutomaticGrid;

(*Setter*)
SetShipLengths[shipLengths_List] := $ShipLengths = shipLengths;
SetUserBase[base_Integer] := If[isBase[base], $UserBase = base, $UserBase];
SetGridSize[size_Integer] := If[size > 0, $GridSize = size, $GridSize];
SetAutomaticShips[ships_List] := $AutomaticShips = ships;
SetUserShips[ships_List] := $UserShips = ships;
SetAutomaticGrid[grid_List] := $AutomaticGrid = grid;
SetUserGrid[grid_List] := $UserGrid = grid;
SetSeed[seed_] := If[isSeed[seed], $Seed = seed, $Seed];


End[];
EndPackage[];
