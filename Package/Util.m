(* ::Package:: *)

(* :Title: Util *)
(* :Context: Util` *)
(* :Author: Dan Cernei, Daniele Russo *)
(* :Version: 2.0 *)
(* :Date: 2025-05-06 *)

(* :Summary: 
   Questo pacchetto fornisce funzioni di utility per il gioco della battaglia navale,
   incluse funzioni per la conversione di coordinate, la visualizzazione della griglia e
   la gestione dell'input dell'utente.
*)

(* :Copyright: A colpi di Bit (C) 2025 *)
(* :Keywords: battaglia navale, utility, griglia, conversione *)
(* :Requirements: Mathematica 12.0+ *)

(* File: Util.m *)

BeginPackage["Util`"];

initSeed::usage="initSeed[seed] serve per inizializzare il PRNG prima di richimare le varie random"
convertToDecimal::usage = "convertToDecimal[input,base] converte una stringa da base specificata a base 10";
mapCoordinate::usage = "mapCoordinate[decimal, gridSize] converte un numero decimale in coordinate {riga, colonna}";
verifyInput::usage = "verifyInput[gridSize, base, input]controlla che l'input sia accettabile secondo la base scelta e la dimensione della griglia";
createGrid::usage="createGrid[ships, gridSize] crea una matrice gridSizexgridSize a partire dalle navi";
showGrid::usage="drawGrid[grid_, gridSize_, ships_Bool] disegna la griglia di gioco mostrando le navi e gli attacchi";
conversionFromDec::usage="conversionFromDec[base,numberDec] mostra i passaggi della conversione di un numero da base 10 a base qualsiasi tra 2,8 e 16";

$Colpito::usage = "Valore per cella colpita.";
$Mancato::usage = "Valore per cella mancata.";
$Nave::usage = "Valore per cella contenente nave.";
$Vuoto::usage = "Valore per cella vuota.";
$Affondato::usage = "Valore per cella di nave affondata.";
$GridSize::usage= "Dimensione della griglia di gioco"


(*VARIABILI GLOBALI*)
(*le seguenti variabili indicano lo stato di una cella nella griglia,
ogni stato \[EGrave] associato ad un valore intero*)
$Colpito = 2; (*cella corrispondente a una nave attaccata*)
$Mancato = -1; (*cella vuota attaccata*)
$Nave = 1; (*cella corrispondente ad una nave non ancora colpita*)
$Vuoto = 0; (*cella vuota non ancora attaccata*)
$Affondato = 3; (*cella corrispondente ad una nave affondata*)

$GridSize=10;

Begin["Private`"];


(*FUNZIONI PER LA CONVERSIONE*)

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
  (* N.B.  MemberQ[list, elem] restituisce True se elem \[EGrave] presente in list, altrimenti False. #  \[DoubleLongRightArrow] rappresenta l'argomento & \[DoubleLongRightArrow] termina la funzione *)
  If[!AllTrue[Characters[input], MemberQ[validChars, #] &] || input=="" || input==" ",
    Return[$Failed]
  ];
  
  (* Conversione da base specificata a base 10 *)
  result = Quiet[FromDigits[input, base]];
  
  (* Verifica che la conversione sia avvenuta correttamente *)
  (* Controlla se il risultato non \[EGrave] un intero oppure \[EGrave] negativo *)
  If[!IntegerQ[result] || result < 0,
    Return[$Failed],
    result
  ]
];

(*FUNZIONE per mostrare i passagi da eseguire per fare una conversione da base 10 a base specificata*)
(*prende in input la base e il numero decimale*)
conversionFromDec[base_, numberDec_]:=Module[{colors}, (*numero convertito, lista di colori usati per mostrare i passaggi della converione*)
	colors = {Blue, Orange, Purple, Red, Brown, Pink,Green};
		
	Column[{
		If[base != 16 && base !=8, (*se la base non \[EGrave] n\[EAcute] 8 n\[EAcute] 16 procedo con il metodo delle divisioni successive*)
			(*Divisioni successive*)
			Module[{quotients, modules, i}, 
				quotients={numberDec};(*iste dei risultati delle divisioni, il primo elemento \[EGrave] il numero da convertire*)
				modules={};(*lista dei resti ottenuti dalle divisioni*)
				i=1;(*numero di passaggi effettuati (usato per sapere qual'\[EGrave] l'indice dell'ultimo quoziente ottenuto)*)
						
				(*memorizzo i quozienti e i resti*)
				While[quotients[[i]] != 0, (*procedo con le divisioni finch\[EGrave] non arrivo a 0*)
					AppendTo[quotients, Quotient[quotients[[i]], base]]; (*memorizzo i quozienti per la divisione successiva*)
					AppendTo[modules, Mod[quotients[[i]], base]]; (*memorizzo i resti per ottenere il numero in binario*)
					i++; (*incremento il numero di passaggi effettuati, elementi in quotients*)
				];
				
				(*spiegazione della conversione, mostro le divisioni successive e i resti ottenuti *)
				Column[{" \[Bullet] Dividiamolo per "<>ToString[base]<>" e annotiamo il resto fino ad arrivare a 0", (*spiegazione del metodo*)
					Row[{ (*Raggruppo la spiegazione in Panel in una riga cos\[IGrave] da posizionare tutto al centro*)
						Panel[Style[ Grid[ (*griglia dove ogni riga rappresenta un passaggio delle divisioni successive*)
							Table[{ (*tabella delle descrizioni di tutti i passaggi*)
							(*un passaggio \[EGrave] descritto come: 
							quoziente ottenuto dalla divisione \[Divide] base =  quoziente successivo    Resto = resto della divisione*)
							(*divisione*)
								Style[ToString[quotients[[n]]], colors[[Mod[n-1, Length[colors]]+1]]], 
								(*colors[[Mod[n-1, Length[colors]]+1]] mi serve per ricominciare dal primo colore 
								quando l'indice del passaggio corrente n \[EGrave] maggiore del numero di colori disponibili*)
								" \[Divide] ",base," = ", 
								(*quoziente*)
								Spacer[5], Style[ToString[quotients[[n+1]]], colors[[Mod[n, Length[colors]]+1]]],
								(*resto*)
								Spacer[5], " Resto = ", Style[ToString[modules[[n]]], Bold]
							}, {n, 1,Length[modules]}](*il numero di resti ottenuti indica anche il numero di passaggi da effettuare*)
						], 12]], 
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
						Table[{
							(*per ogni gruppo indico:
							cifre binarie \[RightArrow] conversione in decimale \[RightArrow] cifra in base 8/16 corrispondente *)
		                     Style[Row[digitsGroups[[n]]], colors[[Mod[n-1, Length[colors]]+1]]], (*gruppo di cifre binarie*)
							 "\[RightArrow]", 
		                     FromDigits[digitsGroups[[n]], 2], (*conversione in decimale*)
							 "\[RightArrow]",
		                     BaseForm[FromDigits[digitsGroups[[n]], 2], base] (*cifra in base 8/16 corrispondente*)
	                    }, {n, 1, Length[digitsGroups]}]
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
];
		

(* funzione per inizializzare il seed*)
initSeed[seed_Integer?NonNegative] := Module[{},
  SeedRandom[seed];
  (* restituiamo Null, l\[CloseCurlyQuote]importante \[EGrave] che il generatore sia resettato *)
  Null
]


(* FUNZIONE di verifica per l'input *)
(*prende in input
- la dimensione della griglia
- la base su cui l'utente si esercita 
- e l'input inserito dall'utente*)
verifyInput[gridSize_, base_, input_] := Module[{decimal, coordinates, row, col, errorMsg = ""},
  (* Conversione dell'input in base 10 *)
  decimal = convertToDecimal[input, base];
  
  If[decimal === $Failed,
    (* Messaggio di errore specifico per conversione fallita *)
    errorMsg = "Input non valido! Inserisci un numero corretto in base " <> ToString[base];
    Return[{$Failed, errorMsg}]  (* Restituisce una tupla con $Failed e il messaggio *)
  ];
  
  (* Calcolo della riga e colonna direttamente qui *)
  col = Mod[decimal, 10];
  row = Quotient[decimal, 10];
  
  
  (* Verifica che sia riga che colonna siano all'interno dei limiti della griglia *)
  If[row < 0 || row >= gridSize || col < 0 || col >= gridSize,
    (* Messaggio di errore specifico per coordinate fuori griglia *)
    errorMsg = "Coordinate fuori dalla griglia! Riga e colonna devono essere comprese tra 0 e " <> 
               ToString[gridSize - 1];
    Return[{$Failed, errorMsg}]
  ];
  
  (* Se arriva qui, le coordinate sono valide *)
  {{row, col}, ""}  (* Restituisce le coordinate e un messaggio vuoto in caso di successo *)
];



(* FUNZIONE per creare la griglia *)
(*prende in input le navi del giocatore e la dimensione della griglia*)
createGrid[ships_,gridSize_]:=Module[{grid=ConstantArray[$Vuoto,{gridSize,gridSize}]},(*griglia inizialmente con celle senza navi*)
	(*inserimento delle navi nella griglia*)
	(*per ogni coordinata in ships*)
	Do[grid[[coord[[1]]+1,coord[[2]]+1]]=$Nave, (*inserisco le navi nella griglia*)
    {coordList,ships},(*coordList \[EGrave] un nave in ships definita da una lista di coordinate *)
    {coord,coordList}]; (*per ogni coordinata della nave imposto la cella corrispondente della griglia = $Nave*)
	grid
];

(* FUNZIONE per mostrare la griglia*)
(*prende in input la griglia del giocatore 
e un valore booleano che indica se la posizione delle navi deve essere nascosta o meno*)
showGrid[grid_, ships_] := Module[{
    gridSize = Length[grid],
    gridWithLabels
  },
  (*di seguito si aggiungono una colonna a sinistra e una riga in alto 
  per inserire la numerazione delle righe e delle colonne*)
  
  (* Creazione di una nuova griglia con spazio per le etichette (numerazione)*)
  gridWithLabels = ConstantArray["", {gridSize + 1, gridSize + 1}];
  (*la nuova griglia avr\[AGrave] dimensione gridSize+1*)
  
  (* Aggiungi gli indici riga (range 0-9) sul lato sinistro (prima colonna a sinistra) *)
  Do[
    gridWithLabels[[i + 1, 1]] = ToString[i - 1], 
    (*gli indici partono dalla cella (2,1) e arrivano all'ultima cella (gridSize+1, 1)*)
    {i, 1, gridSize}
  ];
  
  (* Aggiungi gli indici colonna (range 0-9) in alto (prima riga in alto) *)
  Do[
    gridWithLabels[[1, j + 1]] = ToString[j - 1],
    (*gli indici partono dalla cella (1,2) e arrivano all'ultima cella (1, gridSize+1*)
    {j, 1, gridSize}
  ];
  
  (* copio nella nuova griglia (gridWithLabels) il contenuto della griglia del giocatore (grid)
  ma al posto dei valori numerici inserisco un quadrato colorato 
  cos\[IGrave] da poter mostrare all'utente una griglia di pi\[UGrave] facile comprensione *)
  Do[
	gridWithLabels[[i + 1, j + 1]] = 
      If[grid[[i, j]] == $Colpito, (* Se Nave Colpita*)
        Style["\[FilledSquare]", Red], (*inserisco un quadrato rosso*)
      If[grid[[i, j]] == $Mancato, (*Se Cella Colpita senza nave*)
        Style["\[FilledSquare]", Gray],     (* inserisco un quadrato grigio *)
      If[grid[[i, j]] == $Affondato, (*Se Nave Affondata *)
        Style["\[Dagger]", Blue, Bold, 18],  (* inserisco una croce *)
      If[grid[[i, j]] == $Nave && ships, (*Se c'\[EGrave] una Nave *)
      (* nave visibile solo se ships=True, in questo modo per la cpu possiamo non mostrare le navi impostando ships=False*)
        Style["\[FilledSquare]", Black],    (* inserisco un quadrato nero *)
        ""                   (* altrimenti vuoto *)
      ]]]],
    {i, 1, gridSize}, {j, 1, gridSize}
  ];
  
  (*mostro la griglia con gli indici*)
  Grid[gridWithLabels, 
    Frame -> All, 
    FrameStyle -> GrayLevel[0.5],
    Background -> {
      {GrayLevel[0.9], None, None, None, None, None, None, None, None, None, None},  (* Prima colonna in grigio *)
      {GrayLevel[0.9], None, None, None, None, None, None, None, None, None, None},  (* Prima riga in grigio *)
      {None, None}
    },
    ItemSize -> {1.5, 1.5}
  ]
];
  


End[];
EndPackage[];
