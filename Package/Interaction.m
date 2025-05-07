(* ::Package:: *)

(* :Title: Interaction *)
(* :Context: Interaction` *)
(* :Author: Matteo Rontini, Matilde Nardi, Daniele Russo *)
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

isBase::usage="isBase[base] controlla che base sia 2, 8 o 16";
isSeed::usage="isSeed[seed] controlla che seed sia un numero intero";
helpUser::usage="helpUser[base] mostra in una nuova finestra la conversione in base scelta di un numero decimale casuale";
helpUserPersonalized::usage="helpUserPersonalized[base] chiede in una nuova finestra all'utente di inserire un numero decimale e mostra la sua conversione in base scelta";
conversionFromDec::usage="conversionFromDec[base,numberDec] mostra i passaggi della conversione di un numero da base 10 a base scelta";

PlaceUserShip::usage =
  "PlaceUserShip[startRaw, endRaw] piazza in $UserGrid una nave.\
startRaw e endRaw sono stringhe di un intero in base $UserBase (0..gridSize^2-1),\
con 0 corrispondente in basso a sinistra. Si possono piazzare al massimo 4 navi, una per ciascuna lunghezza 5,4,3,2.\
Restituisce {True, grid} in caso di successo o {False, errorMsg} in caso di errore.";


(*Getters delle variabili globali *)
GetUserShips::usage = "GetUserShips[] restituisce lista di blocchi delle navi utente.";
GetUserGrid::usage = "GetUserGrid[] restituisce matrice griglia utente.";
GetRemainingShipLengths::usage = "GetRemainingShipLengths[] restituisce le lunghezze delle navi ancora da piazzare.";
GetDifficultyLevels::usage = "GetDifficultyLevels[] restituisce i livelli di difficolt\[AGrave] disponibili";
GetCpuShip::usage="GetCpuShip[] restituisce le navi della CPU";
GetCpuGrid::usage="GetCpuGrid[] resitiruisce la griglia delle CPU";
(*Setters delle variabili globali*)
SetShipLengths::unsage="SetShipLengths[shipLengths_List] := $ShipLengths = shipLengths";
SetUserBase::usage="SetUserBase[base_Integer] := If[isBase[base], $UserBase = base, $UserBase]";
SetGridSize::usega="SetGridSize[size_Integer] := If[size > 0, $GridSize = size, $GridSize]";
SetAutomaticShips::usage="SetAutomaticShips[ships_List] := $AutomaticShips = ships";
SetUserShips::usage="SetUserShips[ships_List] := $UserShips = ships";
SetAutomaticGrid::usage="SetAutomaticGrid[grid_List] := $AutomaticGrid = grid";
SetUserGrid::usage="SetUserGrid[grid_List] := $UserGrid = grid";
SetSeed::usage="SetSeed[seed_] := If[isSeed[seed], $Seed = seed, $Seed]";

Begin["`Private`"];

(*livelli di difficolt\[AGrave]*)
$DifficultyLevels = {
  {"Facile", 3, {1}},         (* Livello facile: griglia 3x3, 1 nave di lunghezza 1 *)
  {"Medio", 6, {3, 2, 1}},    (* Livello medio: griglia 6x6, 3 navi di lunghezza 3,2,1 *)
  {"Difficile", 10, {5, 4, 3, 2, 1}}  (* Livello difficile: griglia 10x10, 5 navi di lunghezza 5,4,3,2,1 *)
};


(* Stati globali *)
$AutomaticShips = {}; (*lista delle navi della cpu \[RightArrow] ogni nave \[EGrave] a sua volta una lista delle coordinate delle celle occupate dalla nave stessa *)
(*esempio a scopo puramente illustrativo (per mostrare la gestione delle navi): 
{1,2} coordinate di una cella
{{1,2},{1,3},{1,4}} nave (lista delle coordinate di tre celle)
{ {{1,2},{1,3},{1,4}} , {{2,1},{2,3},{2,4}} } lista di navi (lista di due navi, entrambe occupano 3 celle)
*)

$UserShips      = {}; (*lista delle navi dell'utente*)
$AutomaticGrid  = {}; (*griglia di gioco con le navi della cpu*)
$UserGrid       = {}; (*griglia di gioco con le navi dell'utente*)
$UserBase       = 10; (*base di conversione scelta dall'utente, sar\[AGrave] la base su cui si eserciter\[AGrave] a fare le conversioni*)
$GridSize       = 10; (*dimensione della griglia di gioco (rappresenta sia il numero di righe che di colonne)*)
$Seed= "";  (*seed inserito dall'utente, cos\[IGrave] che l'utente possa ripetere una stessa partita pi\[UGrave] volte*)
$ShipLengths = {5, 4, 3, 2, 1}; (*lista delle lunghezze possibili, quante celle una nave pu\[OGrave] occupare*)

(* SEED E BASE *)
(*richiedi seed*)
AskSeedInput[inputSeed_] := DynamicModule[{value = RandomInteger[1024]},
  Row[{
    "Inserisci seed: ",
    (*Tramite InputField si richiede all'utente di inserire un numero*)
    (*il valore inserito viene assegnato a value dinamicamente 
    e passato alla funzione inputSeed[value] ricevuta come parametro di AskSeedInput *)
    InputField[Dynamic[value, (value = #; inputSeed[value])&], Number, ImageSize -> Small]
    (*Number \[RightArrow] non permette di inserire lettere o caratteri speciali, 
	permette esclusivamente di inserire valori numerici:
	- positivi (+),
	- negativi (-)
	- e con la virgola (.) *)
  }]
];

(* richiedi base *)
AskBaseChoice[inputBase_]:= DynamicModule[{value = 2}, (*il valore della variabile \[EGrave] impostato di default a 2*)
  Row[{
    "Inserisci la base su cui ti vuoi esercitare: ",
    (*Mostra un menu a tendina (PopUpMenu) che permette di selezionare solo uno tra i valori 2,8 e 16.
    Questi rappresentano le tre basi possibili su cui l'utente pu\[OGrave] esercitarsi*)
    PopupMenu[Dynamic[value, (value = #; inputBase[value])&], {2, 8, 16}]
    (*come in AskSeedInput (vedi funzione sopra) il valore selezionato viene dinamicamente assegnato a value 
    e passato in input alla funzione inputBase[value], ricevuta come parametro di AskBaseChoice*)
  }]
];

(*controlli per la base e il seed*)
isBase[base_]:=MemberQ[{2,8,16}, base]; (*restituisce vero se base \[EGrave] un numero tra 2,8 e 16, falso altrimenti*)
isSeed[seed_]:=IntegerQ[seed];  (*restutisce vero se seed \[EGrave] un numero intero, falso altrimenti*)

(*SUGGERIMENTO*)
(*funzione di suggerimento personalizzata*)
helpUserPersonalized[base_Integer]:=PopupWindow[
	Button["Chiedi una cella"], 
	(*al click del bottone viene aperta una finestra 
		che chiede all'utente di inserire un numero decimale,
		poi mostra la conversione in base scelta del numero inserito*)
	DynamicModule[{numberDec=0,error="",ex=""}, (*numero da convertire, messaggio di errore, esercizio (conversione passaggio per passaggio del numero)*)
		Style[ (*imposto uno stesso stile di base per tutti i testi nella finestra*)
			Column[{ (*uno di seguito all'altro (in colonna) vengono mostrati:
						- inserimento del numero e bottone, 
						- eventuale messaggio di errore,
						- conversione del numero se inserito *)
				"Inserisci un numero intero positivo da convertire in base "<>ToString[base],
				Row[{
					(*viene mostrato in una sola riga:
					- il campo per l'inserimento del numero
					- e il bottone per mostrare la conversione*)
					InputField[Dynamic[numberDec], Number, ImageSize -> Small], (*permette di inserire solo valori numerici*)
					Button["Converti in base "<>ToString[base],
						(*al click del bottone viene controllato che il valore sia un numero intero positivo*)
						If[IntegerQ[numberDec]&&numberDec>=0,
							error=""; (*se i controlli vanno a buon fine non c'\[EGrave] nessun messaggio di errore*)
							(*aggiorno la variabile ex assegnandogli i passaggi per la conversione del numero*)
							ex=Column[{
								Style["Conversione del numero: "<>ToString[numberDec],15,Red,Bold], (*indico numero da convertire*)
								Spacer[5],
								conversionFromDec[base,numberDec] (*richiamo conversionFromDec[base, numberDec] che si trova in questo package
									 e restituisce i passaggi per convertire un numero (numberDec) da decimale a base scelta*)
							}];,
							(*se i controlli non vanno a buon fine, cio\[EGrave] il numero inserito non \[EGrave] accettabile (con la virgola o negativo)
							imposto un messaggio di errore e non faccio nessuna conversione*)
							ex="";
							error=Style["Attenzione: Inserisci un numero intero negativo!", Red];
						]
					]
				}],
				Dynamic[error], (*mostro dinamicamente un messaggio di errore*)
				Dynamic[ex] (*mostro dinamicamente i passaggi per effettuare la converisione del numero*)
			}]
		,12] (*dimensione del testo*)
	]
, WindowTitle -> "Suggerimento" (*titolo della finestra*), WindowFloating -> True, WindowMargins -> {{0, Automatic}, {0, Automatic}}];

(*indica i passagi per effettuare una conversione da base 10 a base scelta*)
conversionFromDec[base_, numberDec_]:=Module[{numberBase, colors}, (*numero convertito, lista di colori usati per mostrare i passaggi della converione*)
		colors = {Blue, Orange, Purple, Red, Brown, Pink,Green};
		
		If[!isBase[base], (*se la base non \[EGrave] tra quelle accettate non viene fatta nessuna conversione 
		e non viene dato nessun suggerimeto*)
			"Per favore, prima inserisci una base valida!", (*messaggio di errore*)
			Column[{
				If[base == 2, (*se la base \[EGrave] 2 procedo con il metodo delle divisioni successive*)
					
					(*Divisioni successive per 2*)
					Module[{quotients, modules, i}, 
						quotients={numberDec};(*iste dei risultati delle divisioni, il primo elemento \[EGrave] il numero da convertire*)
						modules={};(*lista dei resti ottenuti dalle divisioni*)
						i=1(*numero di passaggi effettuati (usato per sapere qual'\[EGrave] l'indice dell'ultimo quoziente ottenuto)*)
								
						(*memorizzo i quozienti e i resti*)
						While[quotients[[i]] != 0, (*procedo con le divisioni finch\[EGrave] non arrivo a 0*)
							AppendTo[quotients, Quotient[quotients[[i]], 2]]; (*memorizzo i quozienti per la divisione successiva*)
							AppendTo[modules, Mod[quotients[[i]], 2]]; (*memorizzo i resti per ottenere il numero in binario*)
							i++; (*incremento il numero di passaggi effettuati, elementi in quotients*)
						];
						
						(*spiegazione della conversione, mostro le divisioni successive e i resti ottenuti *)
						Column[{" \[Bullet] Dividiamolo per 2 e annotiamo il resto fino ad arrivare a 0", (*spiegazione del metodo*)
						 Row[{ (*Raggruppo la spiegazione in Panel in una riga cos\[IGrave] da posizionare tutto al centro*)
							Panel[Style[ 
								Grid[ (*griglia dove ogni riga rappresenta un passaggio delle divisioni successive*)
									Table[{ (*tabella delle descrizioni di tutti i passaggi*)
									(*un passaggio \[EGrave] descritto come:*)
										(*divisione*)
										Style[ToString[quotients[[n]]], colors[[Mod[n-1, Length[colors]]+1]]],
										" \[Divide] 2 = ", 
										(*quoziente*)
										Spacer[5], Style[ToString[quotients[[n+1]]], colors[[Mod[n, Length[colors]]+1]]],
										(*resto*)
										Spacer[5], " Resto = ", Style[ToString[modules[[n]]], Bold]
									}, {n, 1, Length[quotients]-1}]
								], 12]
							], 
							(*freccia dal basso verso l'alto posizionata a destra della griglia con i passaggi, mostra in che direzione leggere i resti*)
							Graphics[{Red, Arrowheads[0.5], Arrow[{{0, 0}, {0, i - 1}}]}, ImageSize -> 20]
						 }, Alignment -> Center, ImageSize -> Full], (*allineo Panel al centro*)
						 (*spiegazione di come ottenere il numero binario dalle divisioni*)
						 Row[{" \[Bullet] La conversione in binario si ottiene leggendo i resti dal basso verso l'alto ", Style["(Resti \[UpArrow])", Red]}]
						}]
					],
					(*se la base \[EGrave] 8 o 16 \[RightArrow] converto in binario, raggruppo le cifre e converto ogni gruppo in decimale*)
					Module[{digits, digitsGroups, mod, exp},
						(*lista delle cifre che compongono il numero binario \[RightArrow] usata per la suddivisione in gruppi*)
						digits = IntegerDigits[numberDec, 2]; 
						(*potenza di due: 16=2^4, 8=2^3 \[RightArrow] l'esponente indica la dimensione del gruppo in cui raggruppare il numero binario*)
						exp = Log2[base]; 
						(*la suddivisione in gruppe parte dalle cifre a destra e potrebbe capitare che a sinistra si debbano aggiungere degli 0 se 
						il numero totale di cifre non \[EGrave] multiplo di exp*)
						(*resto (mod) \[RightArrow] indica quante cifre a sinistra rimangono escluse dal raggruppamento, 
						serve poi per sapere quanti 0 aggiungere per formare un nuovo gruppo con le cifre escluse*)
						mod = Mod[Length[digits], exp]; 
						
						(*lista dei gruppi*)
						digitsGroups = If[mod === 0, 
							(*suddivisione del numero binario in gruppi da 3 o 4 cifre (exp)*)
	                        Partition[digits, exp], (*se resto \[EGrave] 0 suddivido senza aggiungere 0*)
	                        Partition[Flatten[{Table[0, {mod, exp - mod}], digits}], exp] (*se resto diverso da 0, con Table aggiungo gli 0 necessari per formare un gruppo *)
	                    ];
						
						(*spiegazione della conversione in base 8 o 16*)     
						Column[{" \[Bullet] convertiamolo in base 2",
							(*indico la conversione in binario*)
							Row[{Panel[Style[BaseForm[numberDec, 2], 12]]}, Alignment -> Center, ImageSize -> Full],
							" \[Bullet] raccogliamo le cifre binarie in gruppi da " <> ToString[exp] <> " partendo dalla posizione pi\[UGrave] a destra (cifra meno significativa),",
							(*mostro come raggruppare le cifre*)
							Row[
								(*i gruppi vengono mostrati in sequenza in una singola riga*)
								Table[
									Panel[(*ogni gruppo viene scritto con colore diverso dentro a un pannello grigio (Panel)*)
										Style[Row[digitsGroups[[n]]],colors[[Mod[n-1, Length[colors]]+1]]]
									]
								, {n, 1, Length[digitsGroups]}]
							, Alignment -> Center, ImageSize -> Full],
							" \[Bullet] convertiamo ogni gruppo in decimale, ogni numero ottenuto corrisponde ad una cifra in base " <> ToString[base] <> ".",
							(*mostro la conversione in decimale*)
							Row[{Panel[Style[Grid[ 
								Table[
									{(*per ogni gruppo indico:
									cifre binarie \[RightArrow] conversione in decimale \[RightArrow] cifra in base 8/16 corrispondente *)
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
				(*indipendentemente dalla base se 2,8 o 16 per ultimo mostro il risultato della conversione*)
				Spacer[5],
				Row[{
					Style["Risultato: ", Italic, 13, Bold],
					numberDec, " = ", BaseForm[numberDec, base], (*conversione da decimale a base scelta*)
					"."
				}]
			}]
		]
	];
		
(* suggerimento non personalizzato, esempio di conversione*)
helpUser[base_Integer]:=PopupWindow[
	Button["Suggerimento"], (*al click del pulsante viene aperta una finestra che mostra
	tutti i passagi per fare la conversione di un numero casuale tra 1 e 300 in base scelta*)
		Module[{numberDec=RandomInteger[{1,300}]},(*numero casuale da convertire*)
			(*suggerimento*)
			Style[ (*imposta stile di base per il testo*)
			Column[{
				Style["Ripassiamo le conversioni tra basi 10 e "<>ToString[base],Bold,Red,15,TextAlignment->Center], (*titolo*)
				Spacer[10],
				Style["Da base 10 a base "<>ToString[base]<>" :",Underlined,Italic,13], (*sottotitolo*)
				Spacer[5],
				Style["Consideriamo il numero: "<>ToString[numberDec],Bold], (*indico il numero da convertire*)
				(*conversionFromDec[base,numberDec] \[EGrave] una funzione definita in questo package*)
				conversionFromDec[base,numberDec] (*richiamo conversionFromDec[base,numberDec] per mostrare i passaggi della conversione del numero in base scelta*)
			}],12] (*dimensione del testo*)
		]
,WindowTitle->"Suggerimento", WindowFloating->True,WindowMargins->{{0,Automatic},{0,Automatic}}];



(* Funzione per generare una rappresentazione del numero massimo nel sistema numerico corrente *)
getMaxNumberInBase[base_, gridSize_] := Module[{maxDecimal, result},(*\[RightArrow] QUESTA FUNZIONE LA ELIMINEREI*)
  maxDecimal = gridSize^2 - 1;
  result = IntegerString[maxDecimal, base];
  result
];

(* Versione corretta della funzione PlaceUserShip *)
PlaceUserShip[startRaw_String, endRaw_String] := Module[
  {start, end, r1, c1, r2, c2, len, coords, usedLens, surroundingCells, isValid = True, errorMsg = "", maxDecimal, maxInBase},
  
  (* Valore massimo per la griglia attuale *)
  maxDecimal = $GridSize^2 - 1;
  maxInBase = getMaxNumberInBase[$UserBase, $GridSize];(* SCRIVEREI INVECE:
  maxInBase = IntegerString[maxDecimal, $UserBase];*)
   
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
