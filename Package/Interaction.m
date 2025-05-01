(* ::Package:: *)

(* file: package.m *)

BeginPackage["Interaction`", {"Util`"}];
AskSeedInput::usage = "AskSeedInput[inputSeed] chiede all'untete di inserire il seed";
AskBaseChoice::usage = "AskBasechoice[inputBase] chiede all'utente di inserire la base su cui si vuole esercitare (2, 8, 16)";

isBase::usage="isBase[base] controlla che la base sia inserita sia 2, 8 o 16";
isSeed::usage="isSeed[seed] controlla che il seed sia stato inserito correttamente (se \[EGrave] un numero intero)";
helpUser::usage="helpUser[base] apre finestra per mostrare un suggerimento all'utente (ripasso conversioni tra basi)";

InitPhase::usage =
  "InitPhase[base, gridSize] inizializza il gioco: base \[Element] {2,8,16}, gridSize \[EGrave] lato griglia.\
Imposta $UserBase, $GridSize, $AutoGrid e $UserGrid, resetta gli stati.";

PlaceUserShip::usage =
  "PlaceUserShip[startRaw, endRaw] piazza in $UserGrid una nave.\
startRaw e endRaw sono stringhe di un intero in base $UserBase (0..gridSize^2-1),\
con 0 corrispondente in basso a sinistra. Si possono piazzare al massimo 4 navi, una per ciascuna lunghezza 5,4,3,2.\
Restituisce {True, grid} in caso di successo o {False, errorMsg} in caso di errore.";

ResetGame::usage =
  "ResetGame[] svuota tutte le liste e ripristina le variabili globali all'inizio del gioco.";

(* Aggiungiamo esplicitamente questi getter mancanti *)
GetUserShips::usage = "GetUserShips[] restituisce lista di blocchi delle navi utente.";
GetUserGrid::usage = "GetUserGrid[] restituisce matrice griglia utente.";
GetRemainingShipLengths::usage = "GetRemainingShipLengths[] restituisce le lunghezze delle navi ancora da piazzare.";

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
$ShipLengths = {5, 4, 3, 2, 1};  (* Lunghezze disponibili delle navi *)

(* Reset di tutte le variabili e liste *)
ResetGame[] := (
  $UserBase       = 10;
  $GridSize       = 10;
  $AutomaticShips = {};
  $UserShips      = {};
  $AutoGrid       = {};
  $UserGrid       = {};
  $Seed= ""; 
  $ShipLengths = {5, 4, 3, 2, 1};
);

(* Inizializza contesto PC e utente *)
InitPhase[seed_Integer, base_Integer, gridSize_Integer] := (
  If[!isBase[base] || !isSeed[seed], 
    showError["Parametri non validi!"];
    Return[];
  ];
  $Seed = seed; 
  $UserBase = base;
  $GridSize = gridSize;
  $AutomaticShips = {};
  $UserShips = {};
  $AutoGrid = ConstantArray[$Vuoto, {gridSize, gridSize}];
  $UserGrid = ConstantArray[$Vuoto, {gridSize, gridSize}];
  $ShipLengths = {5, 4, 3, 2, 1};
  Print["Inizializzazione completata! Base: ", base, ", Seed: ", seed];
);


(*richiedi seed e base*)
AskSeedInput[inputSeed_] := DynamicModule[{value = "", message = "", error = False},
  Column[{
    Row[{
      "Inserisci seed: ",
      InputField[Dynamic[value], Number, ImageSize -> Small],
      Button["Imposta", 
        If[isSeed[value], 
          inputSeed[value];
          error = False;
          message = "Seed inserito: " <> ToString[value];
          (* Forza l'aggiornamento del seed nel modulo principale *)
          seed = value;,
          error = True;
          message = "Seed non valido. Inserisci un numero intero!";
        ]
      ]
    }],
    Dynamic[If[error, showError[message], showMessage[message]]]
  }]
];

AskBaseChoice[inputBase_]:= DynamicModule[{value=2, message=""},
	Column[{
		Row[{
			"Inserisci la base su cui ti vuoi esercitare: ",
			PopupMenu[Dynamic[value], {2,8,16}],
			Button["Inserisci",inputBase[value]; message="Base inserita: "<>ToString[value];]
		}],
		Dynamic[showMessage[message]]
	}]
];

(*Mostra messaggi e errori*)
showMessage[message_String]:=Style[message,Blue];
showError[error_String]:=Style["Attenzione: "<>error, Red];

(*Controlla che i valori di input siano corretti*)
isBase[base_]:=MemberQ[{2,8,16}, base];
isSeed[seed_]:=IntegerQ[seed]; 

helpUser[base_Integer]:=PopupWindow[
	Button["Suggerimento"],
	Module[{numberDec,numberBase,helpDescription,colors},
		numberDec=RandomInteger[{1,300}]; 
		colors = {Green,Blue,Orange,Purple,Red,Brown,Pink};
		If[!isBase[base],"Per favore, prima inserisci una base valida!",
			If[base==2, 
				(*Divisioni successive per 2*)
				Module[{quotients={numberDec},modules={},i},{
					i=1;
					(*memorizzo i quozienti e i resti*)
					While[quotients[[i]]!=0,
						AppendTo[quotients,Quotient[quotients[[i]],2]];
						AppendTo[modules,Mod[quotients[[i]],2]];
						i++;
					];
					(*spiegazione della conversione, mostro divisioni successive e resti ottenuti *)
					helpDescription={" \[Bullet] Dividiamolo per 2 e annotiamo il resto fino ad arrivare a 0",
						Row[{
							Panel[Style[ 
								Grid[
									Table[{
										(*divisione*)
										Style[ToString[quotients[[n]]],colors[[Mod[Length[colors]+n,Length[colors]]+1]]],
										" \[Divide] 2 = ", 
										(*quoziente*)
										Spacer[5],Style[ToString[quotients[[n+1]]],colors[[Mod[Length[colors]+n+1,Length[colors]]+1]]],
										(*resto*)
										Spacer[5]," Resto = ",
										Style[ToString[modules[[n]]],Bold]},
									{n,1,Length[quotients]-1}]
								],12]
							], 
							(*freccia dal basso verso l'alto, mostra come leggere i resti*)
							Graphics[{Red,Arrowheads[0.5], Arrow[{{0, 0}, {0, i - 1}}]}, ImageSize -> 20]},
						Alignment->Center,ImageSize->Full],
						Row[{" \[Bullet] La conversione in binario si ottiene leggendo i resti dal basso verso l'alto ",Style["(Resti \[UpArrow])",Red]}]
					}
				}];,
				(*conversione in basi 8 o 16, converto in binario, raggruppo e converto in decimale*)
				Module[{digits, digitsGroups,mod,exp,table},
					(*lista delle cifre che compongono il numero binario \[RightArrow] usata per la suddivisione in gruppi*)
	        		digits=IntegerDigits[numberDec,2]; 
	        		(*potenza di due: 16=2^4, 8=2^3 \[RightArrow] l'esponente indica la dimensione del gruppo in cui raggruppare il numero binario*)
					exp=Log2[base]; 
					(*resto \[RightArrow] indica quante cifre a sinistra rimangono escluse dal raggruppamento, serve per sapere quanti 0 aggiungere*)
					mod=Mod[Length[digits],exp]; 
					(*suddivisione del numero binario in gruppi da 3 o 4 cifre, se il resto \[EGrave] diverso da 0 vengono aggiunti degli 0 a sx*)
					digitsGroups=If[mod===0, Partition[digits,exp,exp],Partition[digits, exp, exp,{-mod,exp}, 0]];
			        
			        (*spiegazione della conversione*)     
	        		helpDescription={" \[Bullet] convertiamolo in base 2",
	        		(*conversione in binario*)
					Row[{Panel[Style[BaseForm[numberDec,2],12]]},Alignment->Center,ImageSize->Full],
					" \[Bullet] raccogliamo le cifre binarie in gruppi da "<>ToString[exp]<>" partendo dalla posizione pi\[UGrave] a destra (cifra meno significativa),",
					(*raggruppamento cifre*)
					Row[ 
						Table[
							Panel[Style[
								Row[digitsGroups[[n]]],
								colors[[Mod[Length[colors]+n,Length[colors]]+1]]
							,12]],
						{n,1,Length[digitsGroups]}]
					,Alignment->Center, ImageSize->Full],
					" \[Bullet] convertiamo ogni gruppo in decimale, ogni numero ottenuto corrisponde ad una cifra in base "<>ToString[base]<>".",
					(*conversione in decimale*)
					Row[{Panel[Style[Grid[ 
						Table[
							{Style[Row[digitsGroups[[n]]],colors[[Mod[Length[colors]+n,Length[colors]]+1]]], (*gruppo di cifre binarie*)
							"\[RightArrow]", FromDigits[digitsGroups[[n]], 2], (*conversione in decimale*)
							"\[RightArrow]",BaseForm[FromDigits[digitsGroups[[n]], 2],base]},(*cifra in base 8/16 corrispondente*)
						{n, Length[digitsGroups]}]
					],12]]},Alignment->Center, ImageSize->Full]};
				]
			]
		];
		
		(*suggerimento*)
		Style[
		Column[{
			Style["Ripassiamo le conversioni tra basi 10 e "<>ToString[base],Bold,Red, 15, TextAlignment->Center],
			Spacer[10],
			Style["Da base 10 a base "<>ToString[base]<>" :",Underlined,Italic,13],
			Spacer[5],
			Style["Consideriamo il numero: "<>ToString[numberDec],Bold],
			Column[helpDescription],
			Spacer[5],
			Row[{
				Style["Risultato: ", Italic,13,Bold],
				numberDec," = ",BaseForm[numberDec,base], (*conversione da decimale a base 8/16*)
				"."
			}]
		}],12]
		
	]
, WindowTitle->"Suggerimento", WindowFloating->True, WindowMargins -> {{0, Automatic}, {0, Automatic}}];


(* Helper di debug per parseCoord - versione senza Print *)
parseCoord[str_String] := Module[{n, r, c},
  (* Convertiamo la stringa in numero decimale *)
  n = Quiet@FromDigits[str, $UserBase];
  
  (* Verifichiamo se la conversione è valida *)
  If[! IntegerQ[n] || n < 0 || n >= $GridSize^2, 
    Return[$Failed]
  ];
  
  (* Calcoliamo le coordinate *)
  c = Mod[n, $GridSize] + 1;
  r = $GridSize - Quotient[n, $GridSize];
  
  {r, c}
];

(* Versione modificata della funzione PlaceUserShip che restituisce un messaggio di errore invece di usare Print *)
PlaceUserShip[startRaw_String, endRaw_String] := Module[
  {start, end, r1, c1, r2, c2, len, coords, usedLens, surroundingCells, isValid = True, errorMsg = ""},
  
  (* Controllo numero navi *)
  If[Length[$UserShips] >= 5, 
    Return[{False, "Hai già posizionato tutte le navi permesse!"}]
  ];
  
  (* Conversione coordinate *)
  start = parseCoord[startRaw];  
  end = parseCoord[endRaw];
  
  If[start === $Failed || end === $Failed, 
    Return[{False, "Coordinate non valide per la base corrente!"}]
  ];
  
  {r1, c1} = start; {r2, c2} = end;
  
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
    errorMsg = "Hai già piazzato una nave di lunghezza " <> ToString[len] <> "!";
    
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
  If[Not[AllTrue[coords, 1 <= #[[1]] <= $GridSize && 1 <= #[[2]] <= $GridSize &]], 
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
    1 <= #[[1]] <= $GridSize && 
    1 <= #[[2]] <= $GridSize &
  ];
  
  (* Controlliamo prima tutte le celle della nave *)
  isValid = AllTrue[coords, $UserGrid[[Sequence @@ #]] == $Vuoto &];
  If[!isValid,
    Return[{False, "La nave si sovrappone a una nave già posizionata!"}]
  ];
  
  (* Verifichiamo che non ci siano navi adiacenti *)
  isValid = AllTrue[surroundingCells, $UserGrid[[Sequence @@ #]] != $Nave &];
  If[!isValid,
    Return[{False, "Non puoi posizionare una nave adiacente ad un'altra nave!"}]
  ];
  
  (* Solo se arriviamo qui, aggiorniamo la griglia inserendo le navi *)
  Do[
    $UserGrid[[coords[[i, 1]], coords[[i, 2]]]] = $Nave,
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


End[];
EndPackage[];