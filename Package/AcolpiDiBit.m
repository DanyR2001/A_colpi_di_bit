(* ::Package:: *)

(* File: AcolpiDiBit.m *)

BeginPackage["AcolpiDiBit`", {"Util`", "Battle`", "Interaction`"}];  (* Apre il pacchetto Main e importa gli altri pacchetti richiesti *)

placementUI::usage = "placementUI[] crea un'interfaccia grafica interattiva per il gioco della battaglia navale.";

Begin["`Private`"];  (* Entra nella sezione Private, dove si definisce la logica interna del pacchetto *)

placementUI[] := DynamicModule[
  {  
    (* VARIABILI DI CONFIGURAZIONE INIZIALE *)
    seedValue = ToString[RandomInteger[1024]],  (* Valore seed per la generazione casuale *)
    baseValue = 2, (* Base numerica del gioco (2, 8, 10, 16) *)
    difficultyLevel = 3, (* Livello di difficolt\[AGrave] selezionato *)
    phase = 1,  (* Fase corrente del gioco (1=config, 2=posizionamento, 3=battaglia) *)
    initDone = False, (* Flag per indicare se l'inizializzazione \[EGrave] completata *)
    message = "", (* Messaggio di errore o informativo *)
    battleStarted = False,  (* Flag per indicare se la battaglia \[EGrave] iniziata *)
    difficultyLevels = getDifficultyLevels[], (* Lista dei livelli di difficolt\[AGrave] disponibili *)
    
    (* VARIABILI PER IL POSIZIONAMENTO DELLE NAVI *)
    currentShip = 1,  (* Numero della nave corrente da posizionare *)
    start = "",(* Coordinata di inizio nave (es. "A1") *)
    end = "", (* Coordinata di fine nave (es. "A3") *)
    shipPlacementMsg = "",(* Messaggio di feedback per il posizionamento *)
    placementDone = False (* Flag per indicare se tutte le navi sono state posizionate *)
  },
  
  (* INIZIALIZZAZIONE DEL SEED *)
  setSeed[ToExpression[seedValue]];
  
  (* INTERFACCIA UTENTE PRINCIPALE *)
  Style[
    Dynamic[
      If[phase == 1, 
        (* FASE 1: CONFIGURAZIONE INIZIALE DEL GIOCO *)
        Column[{
          (* Input per il seed di generazione casuale *)
          askSeedInput[Function[input, seedValue = input]],
          Spacer[10],
          
          (* Selezione della base numerica *)
          askBaseChoice[Function[input, baseValue = input]],
          Spacer[10],
          
          (* Menu a tendina per la selezione del livello di difficolt\[AGrave] *)
          Row[{
            "Livello di difficolt\[AGrave]: ",
            PopupMenu[Dynamic[difficultyLevel], 
              Table[i -> difficultyLevels[[i, 1]], {i, Length[difficultyLevels]}]]
          }],
          Spacer[10],
          
          (* Pulsante di conferma delle impostazioni *)
          Button["Conferma Impostazioni",
            (* Validazione dei parametri inseriti *)
            If[isSeed[seedValue] && isBase[baseValue], 
              message = "";
              (* Tentativo di inizializzazione del gioco *)
              If[initPhase[ToExpression[seedValue], baseValue, difficultyLevel],
                phase = 2; (* Passa alla fase di posizionamento *)
                initDone = True;, (* Segna l'inizializzazione come completata *)
                message = "Errore durante l'inizializzazione. Riprova.";
              ],
              (* Messaggio di errore per parametri non validi *)
              message = "Seed non valido!\nInserisci un numero decimale intero negativo o positivo.";
            ]
          ],
          Spacer[10],
          
          (* Visualizzazione dinamica dei messaggi di errore *)
          Dynamic[Style[message, Red]]
        }],
        
        If[phase == 2,
          (* FASE 2: POSIZIONAMENTO DELLE NAVI DEL GIOCATORE *)
          Column[{
            (* Titolo della fase *)
            Style["Fase di Posizionamento Navi", Bold, 16],
            
            (* Informazioni sul livello di difficolt\[AGrave] e dimensioni griglia *)
            Style[
              "Livello di difficolt\[AGrave]: " <> difficultyLevels[[difficultyLevel, 1]] <>
              " - Griglia " <> ToString[getGridSize[]] <> "\[Times]" <> ToString[getGridSize[]], 
              Italic
            ],
            
            (* Layout a due colonne: controlli a sinistra, griglia a destra *)
            Row[{
              (* COLONNA SINISTRA: CONTROLLI PER IL POSIZIONAMENTO *)
              Column[{
                (* Indicatore della nave corrente *)
                Dynamic[Row[{"Nave ", currentShip, ":"}]],
                
                (* Griglia dei controlli di input *)
                Grid[{
                  (* Campo di input per la coordinata di inizio *)
                  {"Inizio:", InputField[Dynamic[start], String, Enabled -> Dynamic[!placementDone]]},
                  
                  (* Campo di input per la coordinata di fine *)
                  {"Fine:", InputField[Dynamic[end], String, Enabled -> Dynamic[!placementDone]]},
                  
                  (* Pulsante per confermare il posizionamento della nave *)
                  {"", Button["Conferma", 
                    (* Logica di posizionamento della nave *)
                    With[{tmpResult = placeUserShip[start, end]},
                      If[tmpResult[[1]], (* Se il posizionamento \[EGrave] riuscito *)
                        start = ""; end = ""; (* Pulisce i campi di input *)
                        (* Controlla se tutte le navi sono state posizionate *)
                        If[Length[getRemainingShipLengths[]] == 0,
                          placementDone = True;  (* Segna il posizionamento come completato *)
                          shipPlacementMsg = "Tutte le navi sono state posizionate!";,
                          currentShip++;  (* Passa alla nave successiva *)
                          shipPlacementMsg = tmpResult[[2]];(* Mostra messaggio di successo *)
                        ],
                        shipPlacementMsg = tmpResult[[2]];(* Mostra messaggio di errore *)
                      ]
                    ],
                    Enabled -> Dynamic[!placementDone]]},
                  
                  (* Pulsante per avviare la battaglia (abilitato solo quando tutte le navi sono posizionate) *)
                  {"", Button["Avvia Battaglia", phase = 3, Enabled -> Dynamic[placementDone]]},
                  
                  (* Pulsanti di aiuto per il giocatore *)
                  {"", Row[{
                    helpUser[baseValue],(* Aiuto generale *)
                    Spacer[30],
                    helpUserPersonalized[baseValue] (* Aiuto personalizzato *)
                  }]}
                }],
                
                (* Messaggio di feedback per il posizionamento *)
                Dynamic[Style[
                  shipPlacementMsg,
                  (* Colore verde per i messaggi di successo, rosso per gli errori *)
                  If[StringMatchQ[shipPlacementMsg, "Nave piazzata*" | "Tutte le navi*"], Darker[Green], Red]
                ]]
              }],
              Spacer[30],
              
              (* COLONNA DESTRA: VISUALIZZAZIONE DELLA GRIGLIA *)
              Column[{
                (* Titolo della griglia del giocatore *)
                Style["La tua flotta", Bold, 14],
                
                (* Griglia dinamica che mostra le navi posizionate *)
                Dynamic[showGrid[getUserGrid[], True]],
                
                (* Sezione informativa sulle navi rimanenti *)
                Style["Navi da posizionare:", Bold, 12],
                Dynamic[
                  If[Length[getRemainingShipLengths[]] > 0,
                    "Lunghezze rimanenti: " <> ToString[getRemainingShipLengths[]],
                    "Tutte le navi sono state posizionate!"
                  ]
                ]
              }]
            }],
            
            (* Pulsante per resettare il gioco *)
            Button["Reset Game", 
              (* Reset di tutte le variabili di stato *)
              resetGame[]; 
              placementDone = False;
              shipPlacementMsg = "";
              message = ""; 
              phase = 1
            ]
          }],
          
          (* FASE 3: BATTAGLIA NAVALE   *)
          Column[{
            (* Titolo della fase di battaglia *)
            Style["Battaglia Navale in Base " <> ToString[baseValue], Bold, 20, Red],
            Spacer[10],
            
            (* Interfaccia dinamica del gioco di battaglia *)
            Dynamic[startGame[
              getUserShips[],(* Navi del giocatore *)
              getCpuShip[], (* Navi del computer *)
              getUserGrid[],(* Griglia del giocatore *)
              getCpuGrid[],(* Griglia del computer *)
              baseValue,(* Base numerica utilizzata *)
              getGridSize[](* Dimensioni della griglia *)
            ]],
            Spacer[10],
            
            (* Pulsante per resettare il gioco *)
            Button["Reset Game", 
              (* Reset completo di tutte le variabili *)
              resetGame[]; 
              placementDone = False;
              shipPlacementMsg = "";
              message = ""; 
              phase = 1
            ]
          }]
        ]
      ]
    ],
    FontFamily -> "Arial"  (* Imposta il font dell'interfaccia *)
  ]
];

End[];  (* Chiude il contesto privato *)
EndPackage[];  (* Chiude il pacchetto Main *)
