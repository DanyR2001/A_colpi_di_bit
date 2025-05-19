(* ::Package:: *)

(* :Title: Main *)
(* :Context: Main` *)
(* :Author: Daniele Russo, Nicola Modugno *)
(* :Version: 3.5 *)
(* :Date: 2025-05-19 *)

(* :Summary: 
   Questo pacchetto coordina le diverse fasi del gioco, dall'inizializzazione
   alla battaglia, e gestisce l'interfaccia utente principale.
*)

(* :Copyright: A colpi di Bit (C) 2025 *)
(* :Keywords: battaglia navale, gioco, interfaccia *)
(* :Requirements: Mathematica 12.0+, Util`, Battle`, Interaction` *)

(* File: AcolpiDiBit.m *)

BeginPackage["AcolpiDiBit`", {"Util`", "Battle`", "Interaction`"}];  (* Apre il pacchetto Main e importa gli altri pacchetti richiesti *)

placementUI::usage = "placementUI[] interfaccia per il posizionamento navi";  (* Descrizione accessibile dell'interfaccia pubblica *)

Begin["`Private`"];  (* Entra nella sezione Private, dove si definisce la logica interna del pacchetto *)

(* Eliminato l'approccio con funzioni separate che ricevono parametri *)







(* Funzione principale ristrutturata senza moduli annidati *)
placementUI[] := DynamicModule[
  {  (* Inizio della lista delle variabili locali del modulo principale *)
    seedValue = ToString[RandomInteger[1024]],  (* Genera un numero casuale da usare come seme *)
    baseValue = 2,                          (* Imposta come base numerica iniziale il binario (2) *)
    difficultyLevel = 3,                    (* Livello di difficolt\[AGrave] predefinito *)
    phase = 1,                              (* Fase del gioco attuale *)
    initDone = False,                       (* Flag per indicare se l'inizializzazione \[EGrave] stata completata *)
    message = "",                           (* Stringa per mostrare messaggi informativi *)
    battleStarted = False,                  (* Flag che indica se la fase di battaglia \[EGrave] cominciata *)
    difficultyLevels,                       (* Conterr\[AGrave] i livelli di difficolt\[AGrave] disponibili *)
    
    (* Variabili precedentemente nel modulo annidato della fase 2 *)
    currentShip = 1, 
    start = "", 
    end = "",
    shipPlacementMsg = "", 
    placementDone = False
  },
  
  (* Inizializzazione *)
  difficultyLevels = getDifficultyLevels[];  (* Ottiene l'elenco dei livelli di difficolt\[AGrave] *)
  setSeed[ToExpression[seedValue]];
  
  Style[Column[{  (* La struttura principale \[EGrave] una colonna verticale di elementi *)
    Dynamic[Which[  (* Dynamic aggiorna l'interfaccia quando cambiano variabili *)
      
      phase == 1,  (* Fase 1: configurazione iniziale *)
      Column[{  (* Crea una colonna di elementi per l'interfaccia di configurazione *)
        askSeedInput[Function[input, seedValue = input]],  (* Campo per inserire il seed, aggiorna seedValue *)
        Spacer[10],  (* Aggiunge spazio verticale di 10 punti *)
        askBaseChoice[Function[input, baseValue = input]],  (* Scelta della base numerica, aggiorna baseValue *)
        Spacer[10],  (* Altro spazio verticale *)
        Row[{  (* Riga con etichetta e menu a tendina per la difficolt\[AGrave] *)
          "Livello di difficolt\[AGrave]: ",
          PopupMenu[  (* Menu a tendina per scegliere la difficolt\[AGrave] *)
            Dynamic[difficultyLevel],  (* Selezione dinamica collegata a difficultyLevel *)
            Table[i -> difficultyLevels[[i, 1]], {i, Length[difficultyLevels]}]  (* Popola il menu con i nomi dei livelli *)
          ]
        }],
        Spacer[10],  (* Aggiunge spazio verticale *)
        Button["Conferma Impostazioni",(* Bottone per confermare le impostazioni iniziali *)
          If[isSeed[seedValue] && isBase[baseValue], message="";(* Controlla che il seed e la base siano validi *)
            If[initPhase[ToExpression[seedValue], baseValue, difficultyLevel],  (* Inizializza la fase, se va a buon fine... *)
              phase = 2;  (* Passa alla fase 2: posizionamento navi *)
              initDone = True;,  (* Segna che l'inizializzazione \[EGrave] avvenuta *)
              message = "Errore durante l'inizializzazione. Riprova.";  (* Altrimenti mostra errore *)
            ],
            message = "Seed non valido!\nInserisci un numero decimale intero negativo o positivo.";  (* Messaggio se input non valido *)
          ]
        ],
        Spacer[10],  (* Spazio verticale *)
        Dynamic[Style[message, Red]]  (* Mostra messaggi dinamicamente in base alla variabile message *)
      }],
      
      phase == 2,  (* Fase 2: posizionamento delle navi *)
      Column[{  (* Colonna per la UI della fase 2 *)
        Style["Fase di Posizionamento Navi", Bold, 16],  (* Titolo con stile *)
        Style[  (* Riga informativa su difficolt\[AGrave] e dimensione griglia *)
          "Livello di difficolt\[AGrave]: " <> difficultyLevels[[difficultyLevel, 1]] <>
          " - Griglia " <> ToString[getGridSize[]] <> "\[Times]" <> ToString[getGridSize[]], Italic
        ],
        Row[{  (* Riga contenente input e griglia *)
          Column[{  (* Colonna sinistra con controlli utente *)
            Dynamic[Row[{"Nave ", currentShip, ":"}]],  (* Indica la nave che si sta posizionando *)
            Grid[{  (* Griglia per input coordinate e pulsanti *)
              {"Inizio:", InputField[Dynamic[start], String, Enabled -> Dynamic[!placementDone]]},
              {"Fine:", InputField[Dynamic[end], String, Enabled -> Dynamic[!placementDone]]},
              {"", Button["Conferma",  (* Bottone per posizionare la nave *)
                Module[{result = placeUserShip[start, end]},  (* Chiama la funzione che tenta il posizionamento *)
                  If[result[[1]],  (* Se il posizionamento ha successo... *)
                    start = ""; end = "";  (* Pulisce gli input *)
                    If[Length[getRemainingShipLengths[]] == 0, (* Controlla se restano navi da posizionare *)
                      placementDone = True;  (* Flag: tutte le navi piazzate *)
                      shipPlacementMsg = "Tutte le navi sono state posizionate!";,
                      currentShip++;  (* Altrimenti passa alla successiva *)
                      shipPlacementMsg = result[[2]];
                    ],
                    shipPlacementMsg = result[[2]];  (* Messaggio di errore *)
                  ]
                ],
                Enabled -> Dynamic[!placementDone]
              ]},
              {"", Button["Avvia Battaglia",  (* Bottone per iniziare la battaglia *)
                phase = 3;,  (* Passa alla fase 3 *)
                Enabled -> Dynamic[placementDone]
              ]},
              {"", Row[{  (* Pulsanti di aiuto per la base selezionata *)
                helpUser[baseValue],
                Spacer[30],
                helpUserPersonalized[baseValue]
              }]}
            }],
            Dynamic[Style[  (* Messaggi dinamici per feedback del posizionamento *)
              shipPlacementMsg,
              If[StringMatchQ[shipPlacementMsg, "Nave piazzata*" | "Tutte le navi*"], Darker[Green], Red]
            ]]
          }],  (* Fine colonna sinistra *)
          Spacer[30],  (* Spazio orizzontale tra input e griglia *)
          Column[{  (* Colonna destra con visualizzazione griglia *)
            Style["La tua flotta", Bold, 14],
            Dynamic[showGrid[getUserGrid[], True]],
            Style["Navi da posizionare:", Bold, 12],
            Dynamic[Module[{remaining = getRemainingShipLengths[]},
              If[Length[remaining] > 0,
                "Lunghezze rimanenti: " <> ToString[remaining],
                "Tutte le navi sono state posizionate!"
              ]
            ]]
          }]  (* Fine colonna destra *)
        }],  (* Fine Row della fase 2 *)
        Button["Reset Game", phase = 1;]  (* Pulsante per ricominciare da capo *)
      }],
      
      phase == 3,  (* Fase 3: battaglia *)
      Column[{  (* Interfaccia della battaglia *)
        Style["Battaglia Navale in Base " <> ToString[baseValue], Bold, 20, Red],
        Spacer[10],
        Dynamic[startGame[  (* Inizia la battaglia vera e propria *)
          getUserShips[],
          getCpuShip[],
          getUserGrid[],
          getCpuGrid[],
          baseValue,
          getGridSize[]
        ]],
        Spacer[10],
        Button["Reset Game", resetGame[]; phase = 1;]  (* Pulsante per ricominciare *)
      }]
      
    ]]  (* Fine Dynamic[Which[...] ] *)
  }], FontFamily->"Arial"]  (* Imposto Arial come font per tutto il testo *)
];

End[];  (* Chiude il contesto privato *)
EndPackage[];  (* Chiude il pacchetto Main *)
