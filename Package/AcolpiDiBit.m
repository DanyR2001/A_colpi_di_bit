(* ::Package:: *)

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

placementUI[] := DynamicModule[
  {  
    seedValue = ToString[RandomInteger[1024]],
    baseValue = 2,
    difficultyLevel = 3,
    phase = 1,
    initDone = False,
    message = "",
    battleStarted = False,
    difficultyLevels = getDifficultyLevels[],
    
    (* Variabili per il posizionamento navi *)
    currentShip = 1, 
    start = "", 
    end = "",
    shipPlacementMsg = "", 
    placementDone = False
  },
  
  (* Inizializzazione *)
  setSeed[ToExpression[seedValue]];
  
  Style[
    Dynamic[
      If[phase == 1, 
        (* FASE 1: Configurazione iniziale *)
        Column[{
          askSeedInput[Function[input, seedValue = input]],
          Spacer[10],
          askBaseChoice[Function[input, baseValue = input]],
          Spacer[10],
          Row[{
            "Livello di difficolt\[AGrave]: ",
            PopupMenu[Dynamic[difficultyLevel], 
              Table[i -> difficultyLevels[[i, 1]], {i, Length[difficultyLevels]}]]
          }],
          Spacer[10],
          Button["Conferma Impostazioni",
            If[isSeed[seedValue] && isBase[baseValue], 
              message = "";
              If[initPhase[ToExpression[seedValue], baseValue, difficultyLevel],
                phase = 2;
                initDone = True;,
                message = "Errore durante l'inizializzazione. Riprova.";
              ],
              message = "Seed non valido!\nInserisci un numero decimale intero negativo o positivo.";
            ]
          ],
          Spacer[10],
          Dynamic[Style[message, Red]]
        }],
        
        If[phase == 2,
          (* FASE 2: Posizionamento navi *)
          Column[{
            Style["Fase di Posizionamento Navi", Bold, 16],
            Style[
              "Livello di difficolt\[AGrave]: " <> difficultyLevels[[difficultyLevel, 1]] <>
              " - Griglia " <> ToString[getGridSize[]] <> "\[Times]" <> ToString[getGridSize[]], 
              Italic
            ],
            Row[{
              (* Colonna sinistra con controlli *)
              Column[{
                Dynamic[Row[{"Nave ", currentShip, ":"}]],
                Grid[{
                  {"Inizio:", InputField[Dynamic[start], String, Enabled -> Dynamic[!placementDone]]},
                  {"Fine:", InputField[Dynamic[end], String, Enabled -> Dynamic[!placementDone]]},
                  {"", Button["Conferma", 
                    (* Blocco di codice per posizionamento nave *)
                    Module[{result = placeUserShip[start, end]},
                      If[result[[1]],
                        start = ""; end = "";
                        If[Length[getRemainingShipLengths[]] == 0,
                          placementDone = True;
                          shipPlacementMsg = "Tutte le navi sono state posizionate!";,
                          currentShip++;
                          shipPlacementMsg = result[[2]];
                        ],
                        shipPlacementMsg = result[[2]];
                      ]
                    ],
                    Enabled -> Dynamic[!placementDone]]},
                  {"", Button["Avvia Battaglia", phase = 3, Enabled -> Dynamic[placementDone]]},
                  {"", Row[{
                    helpUser[baseValue],
                    Spacer[30],
                    helpUserPersonalized[baseValue]
                  }]}
                }],
                Dynamic[Style[
                  shipPlacementMsg,
                  If[StringMatchQ[shipPlacementMsg, "Nave piazzata*" | "Tutte le navi*"], Darker[Green], Red]
                ]]
              }],
              Spacer[30],
              (* Colonna destra con griglia *)
              Column[{
                Style["La tua flotta", Bold, 14],
                Dynamic[showGrid[getUserGrid[], True]],
                Style["Navi da posizionare:", Bold, 12],
                Dynamic[
                  If[Length[getRemainingShipLengths[]] > 0,
                    "Lunghezze rimanenti: " <> ToString[getRemainingShipLengths[]],
                    "Tutte le navi sono state posizionate!"
                  ]
                ]
              }]
            }],
            Button["Reset Game", phase = 1]
          }],
          
          (* FASE 3: Battaglia *)
          Column[{
            Style["Battaglia Navale in Base " <> ToString[baseValue], Bold, 20, Red],
            Spacer[10],
            Dynamic[startGame[
              getUserShips[],
              getCpuShip[],
              getUserGrid[],
              getCpuGrid[],
              baseValue,
              getGridSize[]
            ]],
            Spacer[10],
            Button["Reset Game", resetGame[]; phase = 1]
          }]
        ]
      ]
    ],
    FontFamily -> "Arial"
  ]
];

End[];  (* Chiude il contesto privato *)
EndPackage[];  (* Chiude il pacchetto Main *)
