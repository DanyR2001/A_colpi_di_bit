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
askSeedInput::usage = "askSeedInput[inputSeed] chiede all'utente di inserire il seed.";
askBaseChoice::usage = "askBaseChoice[inputBase] chiede all'utente di inserire la base su cui si vuole esercitare (2, 8, 16).";

isBase::usage="isBase[base] controlla che la base sia 2, 8 o 16.";
isSeed::usage="isSeed[seedStr_String] controlla che il seed sia un numero intero decimale.";
helpUser::usage="helpUser[base] mostra in una nuova finestra la conversione in base scelta di un numero decimale casuale.";
helpUserPersonalized::usage="helpUserPersonalized[base] chiede in una nuova finestra all'utente di inserire un numero decimale e mostra la sua conversione in base scelta.";
placeUserShip::usage =
  "placeUserShip[startRaw, endRaw] piazza in UserGrid una nave.\
startRaw e endRaw sono stringhe di un intero in base UserBase (0..gridSize^2-1),\
con 0 corrispondente in basso a sinistra.\
Restituisce {True, grid} in caso di successo o {False, errorMsg} in caso di errore.";


(*Getters delle variabili globali *)
getUserShips::usage = "getUserShips[] restituisce la lista di blocchi delle navi utente.";
getUserGrid::usage = "getUserGrid[] restituisce la matrice della griglia utente.";
getRemainingShipLengths::usage = "getRemainingShipLengths[] restituisce le lunghezze delle navi ancora da piazzare.";
getDifficultyLevels::usage = "getDifficultyLevels[] restituisce i livelli di difficolt\[AGrave] disponibili.";
getCpuShip::usage="getCpuShip[] restituisce le navi della CPU.";
getCpuGrid::usage="getCpuGrid[] restituisce la griglia della CPU.";
getGridSize::usage="getGridSize[] restituisce la grandezza della griglia.";

(*Setters delle variabili globali*)
setShipLengths::usage="setShipLengths[shipLengths_List] imposta le lunghezze delle navi.";
setUserBase::usage="setUserBase[base_Integer] imposta la base.";
setGridSize::usage="setGridSize[size_Integer] imposta la dimensione delle griglie.";
setUserShips::usage="setUserShips[ships_List] imposta le navi dell'utente.";
setUserGrid::usage="setUserGrid[grid_List] imposta la matrice della griglia utente.";
setCPUShips::usage="setCPUShips[ships_List] imposta le navi della CPU.";
setCPUGrid::usage="setCPUGrid[grid_List] imposta la matrice della griglia della CPU.";
setSeed::usage="setSeed[number_Integer] imposta il seed.";

Begin["`Private`"];

(* Livelli di difficolt\[AGrave]
Un livello \[EGrave] definito da "livello", dimensione della griglia, lista delle lunghezze delle navi.
- La dimensione equivale sia al numero di colonne che di righe (si ha una griglia quadrata).
- La lista delle lunghezze delle navi indica quante navi devono avere una certa lunghezza. 
  Quindi, se ci sono due 5 (per esempio), due navi devono avere lunghezza 5;
  se invece c'\[EGrave] solo un 5, allora solo una nave deve avere lunghezza 5. *)
DifficultyLevels = {
  {"Facile", 3, {1}},         (* Livello facile: griglia 3x3, 1 nave di lunghezza 1 *)
  {"Medio", 6, {3, 2, 1}},    (* Livello medio: griglia 6x6, 3 navi di lunghezza 3,2,1 *)
  {"Difficile", 10, {5, 4, 3, 2, 1}}  (* Livello difficile: griglia 10x10, 5 navi di lunghezza 5,4,3,2,1 *)
};


(* Stati globali *)
CpuShips = {}; (*lista delle navi della CPU \[RightArrow] ogni nave \[EGrave] a sua volta una lista delle celle occupate dalla nave stessa *)

UserShips      = {}; (*lista delle navi dell'utente*)
(*Di seguito un esempio a scopo puramente illustrativo per mostrare la gestione delle navi.
Tenendo a mente che una cella \[EGrave] definita dalle sue coordinate abbiamo:
{1,2} \[RightArrow] cella (riga 1 colonna 2)
{{1,2},{1,3},{1,4}} \[RightArrow] nave \[RightArrow] lista delle celle occupate da una nave, in questo caso la nave ha dimensione 3.
{ {{1,2},{1,3},{1,4}} , {{2,1},{2,3},{2,4}} } \[RightArrow] lista di navi (in questo caso \[EGrave] una lista di due navi che occupano entrambe 3 celle)
*)

(*La griglia di gioco \[EGrave] invece una matrice dove ogni cella \[EGrave] rappresentata da un numero che ne indica lo stato.
(vedi Util.m per la spiegazione degli stati di una cella)*)
CpuGrid        = {}; (*griglia di gioco con le navi della cpu*)
UserGrid       = {}; (*griglia di gioco con le navi dell'utente*)
UserBase       = 10; (*base di conversione scelta dall'utente, sar\[AGrave] la base su cui si eserciter\[AGrave] a fare le conversioni*)
ShipLengths    = {5, 4, 3, 2, 1}; (*lista delle lunghezze possibili, quante celle una nave pu\[OGrave] occupare*)
GridSize       = 10; (*variabile che indica la grandezza del campo da gioco, pi\[UGrave] precisamente indica il numero di righe e di colonne*)
Seed           = 0; (*variabile per il seed*)

(* SEED E BASE *)
(*richiedi seed*)
askSeedInput[inputSeed_] := DynamicModule[{value = ToString[Seed]},
(*il valore della variabile value \[EGrave] impostato di default a Seed (variabile globale del Seed) che , 
in questo modo se l'utente non seleziona nulla comunque un valore alla base viene assegnato*)
  Row[{
    "Inserisci seed: ",
   (*Tramite InputField si richiede all'utente di inserire un numero*)
    (*il valore inserito viene assegnato a value dinamicamente e passato alla funzione inputSeed[value].
    inputSeed \[EGrave] una funzione anonima, passata ad askSeedInput in Main.m da PlacementUI, che prende in input 
    il valore inserito dall'utente in askSeedInput e lo assegna ad una variabile dinamica 
    (variabile usata per il seed in PlacementUI in Main.m) *)
    InputField[Dynamic[value, (value = #; inputSeed[value])&], String, ImageSize -> Small]
  }]
];

(* richiedi base *)
(*askBaseChoice prende in input una funzione inputBase. 
inputBase \[EGrave] una funzione anonima passata ad askBaseChoice quando quest'ultima viene richiamata da PlacementUI in Main.m.
Il compito di inputBase \[EGrave] quello di assegnare il valore scelto per la base tramite askBaseChoice ad una variabile dinamica in Main.m.*)
askBaseChoice[inputBase_]:= DynamicModule[{value = 2}, 
(*Il valore della variabile value \[EGrave] impostato di default a 2.
In questo modo se l'utente non seleziona nulla, comunque un valore alla base viene assegnato*)
  Row[{
    "Inserisci la base su cui ti vuoi esercitare: ",
    (*Mostra un menu a tendina (PopUpMenu) che permette di selezionare solo uno tra i valori 2,8 e 16
    (le tre basi possibili su cui l'utente pu\[OGrave] esercitarsi).
     Il menu a tendina permette di evitare che l'utente sbagli l'inserimento della base.*)
    PopupMenu[Dynamic[value, (value = #; inputBase[value])&], {2, 8, 16}]
    (*Come in askSeedInput (vedi funzione sopra) il valore selezionato viene dinamicamente assegnato a value 
    e passato in input alla funzione inputBase[value], ricevuta come parametro*)
  }]
];

(*controlli per la base*) 
isBase[base_]:=MemberQ[{2,8,16}, base]; (*restituisce vero se base \[EGrave] un numero tra 2,8 e 16, falso altrimenti*)

(*controlli per il seed*)
(*visto che askSeedInput restituisce un valore come stringa, isSeed che controlla se tale valore sia corretto o meno
 prende in input una stringa*)
isSeed[seedStr_String] := Module[{validChars = CharacterRange["0", "9"], chars},
(*validChars \[EGrave] la lista di caratteri accettabili.
Dato che il seed deve essere decimale, i caratteri accettabili sono i numeri da 0 a 9*)
  chars = Characters[seedStr]; (*lista dei caratteri in seedStr*)
  
  (* Gestisce il caso di numeri con segno (+/-) *)
  If[Length[chars] > 0 && MemberQ[{"+", "-"}, First[chars]],
  (*quando c'\[EGrave] almeno un caratterre,cio\[EGrave] la stringa non \[EGrave] vuota, e il primo carattere \[EGrave] un segno:*)
    chars = Rest[chars]; (* Rimuove il primo carattere (il segno) *)
  ];
  (*Il segno viene rimosso, solo se \[EGrave] il primo carattere, perch\[EGrave] si accettano numeri negativi e positivi.
  Il segno non rientra tra i caratteri accettabili perch\[EGrave] comunque rimane non accettabile 
  se si trova in una posizione diversa dalla prima cifra*)
  
  (* Verifica che tutti i caratteri restanti siano cifre accettabili e che ci sia almeno un carattere *)
  Length[chars] > 0 && AllTrue[chars, MemberQ[validChars, #]&]
];

(*funzione per evitare di aprire una finestra se \[EGrave] gi\[AGrave] aperta*)
singlePopup[popupWindow_] := With[{p = Unique["popup"]}, (*creo nome unico che far\[AGrave] riferimento al popup*)
  popupWindow /. Button[a_, b_, c___] :> (*cerca tutti i Button per popupWindow e li sostituisce con una nuova versione*)
    Button[a, If[! ValueQ[p] || Options[p] == $Failed, p = b], c]]; (*la nuova versione dei Button esegue l'azione solo se
    non esiste ancora un valore unico associato (ValueQ[p]) o il valore associato non \[EGrave] valido*)

(*funzione di suggerimento personalizzata*)
helpUserPersonalized[base_Integer]:=PopupWindow[
	Button["Chiedi una cella"], 
	(*al click del bottone viene aperta una finestra 
		che chiede all'utente di inserire un numero decimale,
		poi mostra la conversione in base scelta del numero inserito*)
	DynamicModule[{numberDec="",error="",ex=""}, 
	(*numberDec \[RightArrow] numero da convertire, 
	error\[RightArrow] messaggio di errore, 
	ex \[RightArrow] esercizio (conversione passaggio per passaggio del numero)*)
		Style[ (*imposto uno stesso stile di base per tutti i testi nella finestra*)
			Column[{ (*uno di seguito all'altro (in colonna) vengono mostrati:
						- inserimento del numero e bottone, 
						- eventuale messaggio di errore,
						- conversione del numero *)
				"Inserisci un numero decimale intero e positivo da convertire in base "<>ToString[base],
				Row[{ (*richiesta del numero da convertire*)
					(*viene mostrato in una sola riga:
					- il campo per l'inserimento del numero decimale
					- e il bottone per mostrare la conversione*)
					InputField[Dynamic[numberDec], String, ImageSize -> Small], 
					(*numberDec deve essere un numero intero decimale positivo, quindi faccio il controllo con convertToDecimal in Util.m*)
										
					Button["Converti in base "<>ToString[base],
						(*al click del bottone viene controllato che il valore sia un numero intero positivo*)
						If[convertToDecimal[numberDec,10]=!=$Failed, (*per il controllo richiamo convertToDecimal definita i Util.m*)
							numberDec=convertToDecimal[numberDec,10];
							error=""; (*se i controlli vanno a buon fine non c'\[EGrave] nessun messaggio di errore*)
							(*aggiorno la variabile ex assegnandogli i passaggi per la conversione del numero*)
							ex=Column[{
								Style["Conversione del numero: "<>ToString[numberDec],15,Red,Bold], (*indico numero da convertire*)
								Spacer[5],
								conversionFromDec[base,numberDec] (*richiamo conversionFromDec[base, numberDec], definita in Util.m,
									 restituisce i passaggi per convertire un numero (numberDec) da decimale a base scelta*)
							}];,
							(*se i controlli non vanno a buon fine, cio\[EGrave] il numero inserito non \[EGrave] accettabile (con la virgola o negativo)
							imposto un messaggio di errore e non faccio nessuna conversione*)
							ex="";
							error=Style["Attenzione: Inserisci un numero intero positivo!", Red];
						]
					]
				}],
				Dynamic[error], (*mostro dinamicamente un messaggio di errore*)
				Dynamic[ex] (*mostro dinamicamente i passaggi per effettuare la conversione del numero*)
			}]
		,12] (*dimensione del testo*)
	]
, WindowTitle -> "Chiedi Una Cella" (*titolo della finestra*), WindowFloating -> True]//singlePopup (*per evitare di aprire la finestra pi\[UGrave] volte se gi\[AGrave] \[EGrave] aperta*); 

(* suggerimento non personalizzato, esempio di conversione*)
helpUser[base_Integer]:=PopupWindow[
	Button["Suggerimento"], (*al click del pulsante viene aperta una finestra che mostra
	tutti i passagi per fare la conversione di un numero casuale tra 1 e 300 in base scelta*)
		Module[{numberDec},(*numero casuale da convertire*)
			numberDec=RandomInteger[{1,300}];
			(*suggerimento*)
			Style[ (*imposta stile di base per il testo*)
			Column[{
				(*titolo*)
				Style["Ripassiamo le conversioni tra basi 10 e "<>ToString[base],Bold,Red,15,TextAlignment->Center], (*titolo*)
				Spacer[10],
				(*sottotitolo*)
				Style["Da base 10 a base "<>ToString[base]<>" :",Underlined,Italic,13], (*sottotitolo*)
				Spacer[5],
				(*esercizio conversione*)
				Row[{"Consideriamo il numero: ",Subscript[numberDec,10]}], (*indico il numero da convertire, specificando con Subscript la base numerica*)
				(*conversionFromDec[base,numberDec] \[EGrave] una funzione definita in Util.m*)
				conversionFromDec[base,numberDec] (*richiamo conversionFromDec[base,numberDec] per mostrare i passaggi della conversione del numero in base scelta*)
			}],12] (*dimensione del testo*)
		]
,WindowTitle->"Suggerimento", WindowFloating->True]//singlePopup (*per evitare di aprire la finestra pi\[UGrave] volte se gi\[AGrave] \[EGrave] aperta*);




placeUserShip[startRaw_String, endRaw_String] := Module[
(* Definizione della funzione placeUserShip:
   - Argomenti:
     - startRaw: coordinate di inizio della nave (come stringa)
     - endRaw: coordinate di fine della nave (come stringa)
   - Usa Module per avere variabili locali
*)

  {start, end, r1, c1, r2, c2, len, coords, usedLens, surroundingCells, isValid = True, errorMsg = ""},
  (* Definizione delle variabili locali:
     - start, end: coordinate elaborate
     - r1, c1, r2, c2: righe e colonne di inizio/fine
     - len: lunghezza della nave
     - coords: tutte le celle che la nave occuper\[AGrave]
     - usedLens: lunghezze gi\[AGrave] usate
     - surroundingCells: celle attorno alla nave
     - isValid: flag booleano per controlli
     - errorMsg: messaggio di errore
  *)

  If[Length[ShipLengths] == 0, 
    (* Controlla se non ci sono pi\[UGrave] navi disponibili da posizionare *)
    Return[{False, "Hai gi\[AGrave] posizionato tutte le navi permesse!"}]
    (* Restituisce errore se hai finito le navi *)
  ];

  start = verifyInput[GridSize, UserBase, startRaw];  
  (* Verifica la validit\[AGrave] della coordinata iniziale e la converte.
  verifyInput \[EGrave] definita in Util.m *)

  end = verifyInput[GridSize, UserBase, endRaw];
  (* Verifica la validit\[AGrave] della coordinata finale e la converte.
  verifyInput \[EGrave] definita in Util.m *)

  If[start[[1]] === $Failed, 
    (* Se la verifica di start ha fallito *)
    Return[{False, start[[2]]}]
    (* Restituisce errore con il messaggio di errore specifico *)
  ];

  If[end[[1]] === $Failed, 
    (* Se la verifica di end ha fallito *)
    Return[{False, end[[2]]}]
    (* Restituisce errore con il messaggio di errore specifico *)
  ];

  {r1, c1} = start[[1]]; {r2, c2} = end[[1]];
  (* Estrae le coordinate riga e colonna da start ed end *)

  If[Not[r1 == r2 || c1 == c2], 
    (* Controlla che la nave sia orizzontale o verticale *)
    Return[{False, "La nave deve essere allineata orizzontalmente o verticalmente!"}]
    (* Restituisce errore se la nave \[EGrave] diagonale *)
  ];

  len = Max[Abs[r2 - r1], Abs[c2 - c1]] + 1;
  (* Calcola la lunghezza della nave basandosi sulla distanza tra start ed end *)

  usedLens = Length /@ UserShips;
  (* Ottiene le lunghezze delle navi gi\[AGrave] piazzate *)

  If[! MemberQ[ShipLengths, len], 
    (* Controlla se la lunghezza \[EGrave] valida *)
    Return[{False, "Lunghezza non valida! Le navi devono essere di lunghezza " <> 
        StringJoin[Riffle[ToString /@ ShipLengths, ", "]] <> "."}]
    (* Restituisce un messaggio di errore con le lunghezze valide *)
  ];

  If[MemberQ[usedLens, len], 
    (* Controlla se gi\[AGrave] esiste una nave con la stessa lunghezza *)
    errorMsg = "Hai gi\[AGrave] piazzato una nave di lunghezza " <> ToString[len] <> "!";

    If[Length[ShipLengths] > 0,
      (* Se ci sono altre lunghezze da piazzare, le elenca. *)
      errorMsg = errorMsg <> " Mancano ancora le navi di lunghezza: " <> 
                ToString[Complement[ShipLengths, usedLens]]
    ];

    Return[{False, errorMsg}]
    (* Restituisce un messaggio dettagliato sugli errori *)
  ];

  coords = If[r1 == r2,
    (* Se la nave \[EGrave] orizzontale *)
    Table[{r1, Min[c1, c2] + i}, {i, 0, Abs[c1 - c2]}],
    (* Se la nave \[EGrave] verticale *)
    Table[{Min[r1, r2] + i, c1}, {i, 0, Abs[r1 - r2]}]
  ];
  (* Genera tutte le coordinate della nave *)

  If[Not[AllTrue[coords, 0 <= #[[1]] < GridSize && 0 <= #[[2]] < GridSize &]], 
    (* Controlla che tutte le coordinate siano dentro la griglia *)
    Return[{False, "La nave uscirebbe dalla griglia!"}]
    (* Restituisce errore se esce fuori *)
  ];

  surroundingCells = Flatten[
    Table[
      Table[
        {coords[[i, 1]] + dr, coords[[i, 2]] + dc}, 
        {dr, -1, 1}, {dc, -1, 1}
      ],
      {i, 1, Length[coords]}
    ], 2];
  (* Calcola le celle circostanti alla nave:
     - Per ogni cella occupata, genera tutte le 8 celle intorno (matrice 3x3), cos\[IGrave] non si possono piazzare navi adiacenti *)

  surroundingCells = Select[surroundingCells, 
    !MemberQ[coords, #] && 
    0 <= #[[1]] < GridSize && 
    0 <= #[[2]] < GridSize &
  ];
  (* Rimuove le celle che fanno parte della nave e quelle fuori dalla griglia *)

  isValid = AllTrue[coords, UserGrid[[#[[1]] + 1, #[[2]] + 1]] == Vuoto &];
  (* Controlla che tutte le celle in cui si vuole piazzare la nave siano vuote *)

  If[!isValid,
    (* Se almeno una cella \[EGrave] occupata *)
    Return[{False, "La nave si sovrappone a una nave gi\[AGrave] posizionata!"}]
  ];

  isValid = AllTrue[surroundingCells, UserGrid[[#[[1]] + 1, #[[2]] + 1]] != Nave &];
  (* Controlla che le celle circostanti non abbiano gi\[AGrave] una nave *)

  If[!isValid,
    (* Se trova una nave adiacente *)
    Return[{False, "Non puoi posizionare una nave adiacente a un'altra nave!"}]
  ];

  Do[
    (* Aggiorna la griglia: piazza la nave in ogni cella di coords *)
    UserGrid[[coords[[i, 1]] + 1, coords[[i, 2]] + 1]] = Nave,
    {i, 1, Length[coords]}
  ];

  AppendTo[UserShips, coords];
  (* Aggiunge la nave alla lista delle navi posizionate *)

  ShipLengths = DeleteCases[ShipLengths, len];
  (* Rimuove la lunghezza appena piazzata dall'elenco delle navi da piazzare *)

  {True, "Nave piazzata con successo!"}
  (* Restituisce successo *)
];
(* Fine della funzione placeUserShip *)


(* Getters *)

getUserShips[] := UserShips;
(* Restituisce la lista delle navi dell'utente:
   - Ogni elemento della lista \[EGrave] una nave (cio\[EGrave] un'altra lista di coordinate),
   - Serve per leggere le navi gi\[AGrave] piazzate.
*)

getUserGrid[] := UserGrid;
(* Restituisce la matrice della griglia utente:
   - Griglia di gioco che contiene celle vuote e celle occupate da navi,
   - Utile per visualizzare lo stato attuale della griglia.
*)

getRemainingShipLengths[] := ShipLengths;
(* Restituisce le lunghezze delle navi che devono ancora essere piazzate:
   - Serve per capire quali navi sono ancora disponibili.
*)

getDifficultyLevels[] := DifficultyLevels;
(* Restituisce la lista dei livelli di difficolt\[AGrave]:
   - Ogni livello \[EGrave] una lista del tipo: {Nome, DimensioneGriglia, ListaLunghezzeNavi},
   - Serve per mostrare le opzioni di difficolt\[AGrave] disponibili.
*)

getShipSize[]:=ShipSize;

getCpuShip[]:=CpuShips;
(* Restituisce la lista delle navi della CPU:
   - Ogni elemento della lista \[EGrave] una nave (cio\[EGrave] un'altra lista di coordinate),
   - Serve per conoscere le navi gi\[AGrave] piazzate.
*)
getCpuGrid[]:=CpuGrid;
(* Restituisce la matrice della griglia della CPU:
   - Griglia di gioco che contiene celle vuote e celle occupate da navi,
   - Utile per visualizzare lo stato attuale della griglia.
*)

getGridSize[]:= GridSize;
(* Restituisce la GridSize, dimensione della griglia di gioco*)





(* Setter *)

setShipLengths[shipLengths_List] := ShipLengths = shipLengths;
(* Imposta la variabile globale ShipLengths:
   - Deve essere una lista (shipLengths_List),
   - Serve per definire le lunghezze delle navi disponibili.
*)

setUserBase[base_Integer] := If[isBase[base], UserBase = base, UserBase];
(* Imposta la base numerica scelta dall'utente:
   - Solo se la base \[EGrave] valida (controllata da isBase[base], deve essere 2, 8 o 16),
   - Se non \[EGrave] valida, lascia invariato il valore attuale.
*)

setGridSize[size_Integer] := If[size > 0, GridSize = size, GridSize];
(* Imposta la dimensione della griglia:
   - Solo se size > 0 (griglia almeno 1x1),
   - Altrimenti lascia invariato il valore.
*)

setUserShips[ships_List] := UserShips = ships;
(* Imposta la lista delle navi dell'utente:
   - Permette di caricare direttamente uno stato salvato o ripristinare navi.
*)

setUserGrid[grid_List] := UserGrid = grid;
(* Imposta la griglia utente:
   - Griglia di gioco con le navi piazzate dall'utente,
   - Utile per ripristinare una partita salvata.
*)

setCPUShips[ships_List] := CpuShips = ships;
(* Imposta la lista delle navi della CPU:
   - Permette di caricare direttamente uno stato salvato o ripristinare navi.
*)


setCPUGrid[grid_List] := CpuGrid = grid;
(* Imposta la griglia della CPU:
   - Griglia di gioco con le navi piazzate dalla CPU.
*)

setSeed[number_Integer] := Seed = number;
(*Imposta il seed della partita:
- Il seed permettere all'utente di ripetere una stessa partita pi\[UGrave] volte.*)


End[];
EndPackage[];
