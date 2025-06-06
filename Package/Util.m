(* ::Package:: *)

(* :Title: Util *)
(* :Context: Util` *)
(* :Author: Dan Cernei, Daniele Russo, Matilde Nardi *)
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

initSeed::usage="initSeed[seed] serve per inizializzare il seed prima di richiamare le varie random"
(*convertToDecimal::usage = "convertToDecimal[input,base] converte una stringa da base specificata a base 10";*)
verifyInput::usage = "verifyInput[gridSize, base, input]controlla che l'input sia accettabile secondo la base scelta e la dimensione della griglia";
createGrid::usage="createGrid[ships, gridSize] crea una matrice gridSize x gridSize a partire dalle navi";
showGrid::usage="showGrid[grid_, gridSize_] disegna la griglia di gioco mostrando le navi e gli attacchi";
conversionFromDec::usage="conversionFromDec[base,numberDec] restituisce i passaggi della conversione di un numero da base 10 a base qualsiasi";
conversionToDec::usage = "conversionToDec[base, number] restituisce i passaggi per la conversione di un numero da base qualsiasi a base 10."
helpUser::usage="helpUser[base] mostra in una nuova finestra la conversione in base scelta di un numero decimale casuale.";
helpUserPersonalized::usage="helpUserPersonalized[base] chiede in una nuova finestra all'utente di inserire un numero decimale e mostra la sua conversione in base scelta.";

Colpito::usage = "Valore costante per cella colpita.";
Mancato::usage = "Valore costante per cella mancata.";
Nave::usage = "Valore costante per cella contenente nave.";
Vuoto::usage = "Valore costante per cella vuota.";
Affondato::usage = "Valore costante per cella di nave affondata.";




Begin["Private`"];

(*Nota la griglia di gioco \[EGrave] composta da celle che possono avere una permutazione dei seguenti stati:
	- Contiene navi o parti di una nave  
	- Non contiene navi o parti di nave
	- La cella sia stata attaccata o meno
	- La cella contiene parti di una nave affondata
Gli stati della cella possono quindi essere:
	- Contiene navi o parti di una nave e la cella non \[EGrave] stata attaccata (Cella Nera se dell'Utente, Cella Bianca se della CPU per nascondere le navi della CPU)
		- Il valore nella cella \[EGrave] quello della costante Nave, ovvero 1
	- Contiene navi o parti di una nave e la cella \[EGrave] stata attaccata (Cella Rossa)
		- Ovvero al posto di Nave, assegnamo Colpito, cio\[EGrave] 2
	- Contiene navi o parti di una nave che sono state affondate (Croce blue)
		- Viene assegnato a tutte le celle di una nave il valore di Affondato, ovvero 3, quando tutte le celle di una nave vengono colpite
	- Non contiene navi o parti di nave, e non \[EGrave] stata attaccata (Cella Bianca)
		- La cella \[EGrave] stata inizializzata con il valore della costante Vuoto, ovvero 0
	- Non contiene navi o parti di nave ed \[EGrave] stata attaccata (Cella Grigia)
		- Il valore della cella \[EGrave] Mancato, ovvero -1
	*)

Colpito := 2;
Mancato := -1;
Nave := 1;
Vuoto := 0;
Affondato := 3;


(*FUNZIONI PER LA CONVERSIONE*)

(* FUNZIONE per convertire da una base specificata a base 10 *)
convertToDecimal[input_String, base_Integer] := Module[
  {validChars, result},
  
  (* Definizione dei caratteri validi per ciascuna base *)
  validChars = Switch[base,
    2, {"0", "1"},                                                                              (* Base binaria *)
    8, CharacterRange["0", "7"],                                                                (* Base ottale *)
    10, CharacterRange["0", "9"],                                                               (* Base decimale *)
    16, Join[CharacterRange["0", "9"], CharacterRange["A", "F"], CharacterRange["a", "f"]],     (* Base esadecimale *)
    _, {}                                                                                       (* Base non supportata - restituisce lista vuota *)
  ];
  
  (* Verifica che l'input contenga solo caratteri validi per la base specificata *)
  If[!AllTrue[Characters[input], MemberQ[validChars, #] &] || input=="" || input==" ",
    Return[$Failed] (* Restituisce errore se l'input non \[EGrave] valido *)
  ];
  
  (* Conversione da base specificata a base 10 *)
  result = Quiet[FromDigits[input, base]];
  
  (* Verifica che la conversione sia avvenuta correttamente *)
  If[!IntegerQ[result] || result < 0,
    Return[$Failed],      (* Conversione fallita *)
    result                (* Restituisce il numero convertito *)
  ]
];

(*indica i passagi da eseguire per fare una conversione da base 10 a base qualsiasi tra 2,8,16*)
conversionFromDec[base_, numberDec_]:=Module[{colors, isNumberDec}, (*numero convertito, lista di colori usati per mostrare i passaggi della converione*)
		colors = {Blue, Orange, Purple, Red, Brown, Pink,Green};
		isNumberDec=convertToDecimal[numberDec,10];
		If[isNumberDec===$Failed, (*se la conversione fallisce allora il numero non \[EGrave] nella base specificata*)
			"Inserisci un numero decimale valido (>= 0)", (*mostro messaggio*)
			(*convertToDecimal controlla che il numero sia corretto nella base specificata e se lo \[EGrave] lo converte in decimale*)
  
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
										Style[quotients[[n]], colors[[Mod[n-1, Length[colors]]+1]]], 
										(*colors[[Mod[n-1, Length[colors]]+1]] mi serve per ricominciare dal primo colore 
										quando l'indice del passaggio corrente n \[EGrave] maggiore del numero di colori disponibili*)
										" \[Divide] ",base," = ", 
										(*quoziente*)
										Spacer[5], Style[quotients[[n+1]], colors[[Mod[n, Length[colors]]+1]]],
										(*resto*)
										Spacer[5], " Resto = ", Style[modules[[n]], Bold]
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
								Table[
									(*per ogni gruppo indico:
									cifre binarie \[RightArrow] conversione in decimale \[RightArrow] cifra in base 8/16 corrispondente *)
			                         {Style[Row[digitsGroups[[n]]], colors[[Mod[n-1, Length[colors]]+1]]], (*gruppo di cifre binarie*)
									 "\[RightArrow]", 
			                         FromDigits[digitsGroups[[n]], 2], (*conversione in decimale*)
									 "\[RightArrow]",
			                         BaseForm[FromDigits[digitsGroups[[n]], 2], base] (*cifra in base 8/16 corrispondente*)
			                         },
								 {n, 1, Length[digitsGroups]}]
							], 12]]}, Alignment -> Center, ImageSize -> Full]
						}]
					]
				],
				(*indipendentemente dalla base se 2,8 o 16 per ultimo mostro il risultato della conversione*)
				Spacer[5],
				Row[{
					Style["Risultato: ", Italic, 13, Bold],
					Subscript[numberDec,10], " = ", BaseForm[numberDec, base], (*conversione da decimale a base scelta*)
					"."
				}]
			}]
		]
	];
		

(* funzione per inizializzare il seed la prima volta*)
initSeed[seed_Integer] := Module[{},
  SeedRandom[seed];
  (* restituiamo Null, l\[CloseCurlyQuote]importante \[EGrave] che il generatore sia resettato *)
  Null
]


verifyInput[gridSize_, base_, input_] := Module[{decimal, coordinates, row, col, errorMsg = ""},
  (* Conversione dell'input in base 10 *)
  decimal = convertToDecimal[input, base];
  
  (* Verifica se la conversione \[EGrave] avvenuta correttamente *)
  If[decimal === $Failed,
    (* Messaggio di errore specifico per conversione fallita *)
    errorMsg = "Input non valido! Inserisci un numero corretto in base " <> ToString[base];
    Return[{$Failed, errorMsg}]  (* Restituisce una tupla con $Failed e il messaggio *)
  ];
  
  (* Calcolo delle coordinate dalla rappresentazione decimale *)
  (* La cifra meno significativa rappresenta la colonna *)
  col = Mod[decimal, 10];
  (* La parte intera della divisione per 10 rappresenta la riga *)
  row = Quotient[decimal, 10];
  
  
  (* Verifica che sia riga che colonna siano all'interno dei limiti della griglia *)
  (* Entrambe devono essere >= 0 e < gridSize *)
  If[row < 0 || row >= gridSize || col < 0 || col >= gridSize,
    (* Messaggio di errore specifico per coordinate fuori griglia *)
    errorMsg = "Coordinate fuori dalla griglia! Riga e colonna devono essere comprese tra 0 e " <> 
               ToString[gridSize - 1];
    Return[{$Failed, errorMsg}]
  ];
  
  (* Se arriva qui, le coordinate sono valide *)
  {{row, col}, ""}  (* Restituisce le coordinate e un messaggio vuoto in caso di successo *)
];


(* Crea una griglia di gioco posizionando le navi *)
createGrid[ships_,gridSize_]:=Module[{
  grid=ConstantArray[Vuoto,{gridSize,gridSize}]},     (* Inizializza griglia vuota *)
  (* Itera attraverso tutte le coordinate delle navi e le posiziona sulla griglia *)
  (* ships \[EGrave] una lista di liste, dove ogni sottolista contiene coordinate {riga, colonna} *)
	Do[
    (* Imposta la cella alla posizione indicata come contenente una nave (valore = 1) *)
    (* +1 perch\[EAcute] gli indici in Mathematica partono da 1, ma le coordinate sono 0-based *)
    grid[[coord[[1]]+1,coord[[2]]+1]]=Nave,
    {coordList,ships},      (* Itera attraverso ogni nave *)
    {coord,coordList}];     (* Itera attraverso ogni coordinata della nave *)
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
    (*gli indici partono dalla cella (1,2) e arrivano all'ultima cella (1, gridSize+1)*)
    {j, 1, gridSize}
  ];
  
  (* copio nella nuova griglia (gridWithLabels) il contenuto della griglia del giocatore (grid)
  ma al posto dei valori numerici inserisco un quadrato colorato 
  cos\[IGrave] da poter mostrare all'utente una griglia di pi\[UGrave] facile comprensione *)
  Do[
	gridWithLabels[[i + 1, j + 1]] = 
      If[grid[[i, j]] == Colpito,          (* Se Nave Colpita*)
        Style["\[FilledSquare]", Red],      (*inserisco un quadrato rosso*)
      If[grid[[i, j]] == Mancato,          (*Se Cella Colpita senza nave*)
        Style["\[FilledSquare]", Gray],     (* inserisco un quadrato grigio *)
      If[grid[[i, j]] == Affondato,        (*Se Nave Affondata *)
        Style["\[Dagger]", Blue, Bold, 18], (* inserisco una croce *)
      If[grid[[i, j]] == Nave && ships,    (*Se c'\[EGrave] una Nave *)
                                            (* nave visibile solo se ships=True, in questo modo per la cpu possiamo non mostrare le navi impostando ships=False*)
        Style["\[FilledSquare]", Black],    (* inserisco un quadrato nero *)
        ""                                  (* altrimenti vuoto *)
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

(*FUNZIONE che indica i passagi di una conversione da base specificata a base 10, usata negli esempi del tutorial*)
(*prende in input il numero e la base numerica del numero stesso*) 
conversionToDec[base_, number_] := Module[{colors, numberDec, digits, powers, terms, len},
  colors = {Blue, Orange, Purple, Red, Brown, Pink, Green};
  numberDec = convertToDecimal[number, base]; (*richiamo la funzioni in Util.m *)
  (*la funzione controlla che il numero sia corretto nella base specificata e se lo è lo converte in decimale*)
  
  If[numberDec === $Failed, (*se la conversione fallisce allora il numero non è nella base specificata*)
    "Inserisci un numero intero in base " <> ToString[base], (*mostro messaggio*)
    (*altrimenti vado avanti*)
    digits = IntegerDigits[numberDec, base]; (*lista delle cifre in base specificata del numero decimale*)
    len = Length[digits]; (*numero di cifre che costituiscono il numero*)
    powers = Reverse[Range[0, len - 1]]; (* posizione delle cifre, 
                                           Reverse perché la posizione delle cifre in un numero va da sinistra verso destra *)
    terms = Table[digits[[i]] base^powers[[i]], {i, len}]; (*moltiplico ogni cifra per la potenza della base, 
                                                              terms contiene i prodotti, cioè i termini dell'addizione*)
    
    Column[{ (*raggruppo in colonna la spiegazione della conversione*)
      " • Scomponiamo il numero nelle sue cifre in base " <> ToString[base] <> ":", (*prima fase*)
      Row[ (*in una riga mostro le cifre che compongono il numero in base specificata*)
        (*ogni cifra avrà un colore diverso*)
        Table[Panel[Style[digits[[n]], 12, colors[[Mod[n - 1, Length[colors]] + 1]]]], {n, 1, Length[digits]}]
        , Alignment -> Center, ImageSize -> Full],
      (*seconda fase*)
      " • Ogni cifra viene moltiplicata per la potenza della base corrispondente alla sua posizione (da destra a sinistra):",
      Row[{ (*al centro posiziono la lista delle moltiplicazioni del tipo cifra × base^posizione *)
        Panel[Style[
          Grid[Table[{
            Style[digits[[n]], colors[[Mod[n - 1, Length[colors]] + 1]]], (*cifra*)
            "×", 
            Superscript[base, powers[[n]]], (*potenza*)
            "=", 
            Style[terms[[n]], Bold] (*risultato della moltiplicazione*)
          }, {n, len}]]
          , 12]]
      }, Alignment -> Center, ImageSize -> Full],
      Spacer[5],
      " • Sommiamo tutti i valori ottenuti:",
      Row[{(*in una riga mostro la somma dei prodotti e il risultato che si ottiene*)
        (*la riga sarà del tipo: cifra × base^posizione + ... + cifra × base^posizione = somma*)
        (*dove cifra × base^posizione viene sostituito dal risultato del prodotto*)
        Panel[Style[
          Row[{
            Row[Riffle[ (*Riffle permette di scrivere il + tra ogni coppia di elementi in terms*)
              Table[Style[terms[[n]], colors[[Mod[n - 1, Length[colors]] + 1]]], {n, Length[terms]}], (*assegno un colore ad ogni termine*)
              " + "
            ]],
            " = ", numberDec (*risultato della somma, già calcolato prima con la funzione convertToDecimal[]*)
          }], 14]]
      }, Alignment -> Center, ImageSize -> Full],
      
      Spacer[5],
      Row[{ (*mostro il risultato finale della conversione del tipo: numero in base = numero decimale*)
        Style["Risultato: ", Italic, 13, Bold],
        Subscript[number, base], " = ", Subscript[numberDec, 10]
      }]
    }]
  ]
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



End[];
EndPackage[];
