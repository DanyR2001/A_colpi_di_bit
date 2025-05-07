(* ::Package:: *)

(* :Title: Main *)
(* :Context: Main` *)
(* :Author: Daniele Russo, Nicola Modugno *)
(* :Version: 3.4 *)
(* :Date: 2025-05-06 *)

(* :Summary: 
   Questo pacchetto coordina le diverse fasi del gioco, dall'inizializzazione
   alla battaglia, e gestisce l'interfaccia utente principale.
*)

(* :Copyright: A colpi di Bit (C) 2025 *)
(* :Keywords: battaglia navale, gioco, interfaccia *)
(* :Requirements: Mathematica 12.0+, Util`, Battle`, Interaction` *)

(* File: Main.m *)

BeginPackage["Main`", {"Util`", "Battle`", "Interaction`"}]; 
PlacementUI::usage = "PlacementUI[] interfaccia per il posizionamento navi";

Begin["`Private`"];

(* Interfaccia utente principale *)
PlacementUI[] := DynamicModule[
  {seedValue = RandomInteger[1024], baseValue = 2, difficultyLevel = 3, phase = 1, initDone = False, message = "", 
   cpuShips, battleStarted = False, userShips, userGrid, cpuGrid, difficultyLevels}, 
  
  (* Carica i livelli di difficoltà *)
  difficultyLevels = GetDifficultyLevels[];
  
  Column[{
    Dynamic[Which[
      phase == 1,
        Column[{
          (* Utilizziamo le funzioni AskSeedInput e AskBaseChoice dal pacchetto Interaction *)
          AskSeedInput[Function[input, seedValue = input]],
          Spacer[10],
          AskBaseChoice[Function[input, baseValue = input]],
          Spacer[10],
          (* Aggiungi selezione livello difficoltà *)
          Row[{
            "Livello di difficoltà: ",
            PopupMenu[Dynamic[difficultyLevel], 
              Table[i -> difficultyLevels[[i, 1]], {i, Length[difficultyLevels]}]]
          }],
          Spacer[10],
          Button["Conferma Impostazioni",
            If[isSeed[seedValue] && isBase[baseValue],
              If[InitPhase[seedValue, baseValue, difficultyLevel],
              phase = 2;
              initDone = True;,
               message = "Errore durante l'inizializzazione. Riprova.";
              ],
              message = "Seed o base non validi!";
            ]
          ],
          Spacer[10],
          Dynamic[message]
        }],
      
      phase == 2,
        Column[{
          DynamicModule[
            {currentShip = 1, start = "", end = "", 
             shipPlacementMsg = "", placementDone = False, gridSize = $GridSize},
            
            Column[{
              Style["Fase di Posizionamento Navi", Bold, 16],
              Style["Livello di difficoltà: " <> difficultyLevels[[difficultyLevel, 1]] <> 
                    " - Griglia " <> ToString[gridSize] <> "×" <> ToString[gridSize], Italic],
              Row[{
                (* Parte sinistra: input e comandi *)
                Column[{
                  Dynamic[Row[{"Nave ", currentShip, ":"}]],
                  Grid[{
                    {"Inizio:", InputField[Dynamic[start], String, Enabled -> Dynamic[!placementDone]]},
                    {"Fine:", InputField[Dynamic[end], String, Enabled -> Dynamic[!placementDone]]},
                    {"", Button["Conferma",
                      (* Chiamiamo PlaceUserShip che ora ritorna {success, message} *)
                      Module[{result = PlaceUserShip[start, end]},
                        If[result[[1]], (* Success = True *)
                          (* Nave piazzata con successo *)
                          start = ""; end = "";
                          
                          If[Length[GetRemainingShipLengths[]] == 0,
                            placementDone = True;
                            shipPlacementMsg = "Tutte le navi sono state posizionate!";
                          ,
                            currentShip++;
                            shipPlacementMsg = result[[2]]; (* Messaggio di successo *)
                          ]
                        ,
                          (* Errore - mostriamo il messaggio specifico ritornato *)
                          shipPlacementMsg = result[[2]];
                        ]
                      ],
                      Enabled -> Dynamic[!placementDone]
                    ]},
                    {"", Button["Avvia Battaglia",
                        (* Salva i dati necessari e passa alla fase 3 *)
                        userShips = GetUserShips[];
                        userGrid = GetUserGrid[];
                        (*cpuShips = generateCPUShips[$GridSize];
                        cpuGrid = createGrid[cpuShips, $GridSize]; queste cose sono state spostate in InitPhase*)
                        phase = 3;
                      ,
                        Enabled -> Dynamic[placementDone]
                      ]
                    },
                    {"",Row[{helpUser[baseValue],Spacer[30],helpUserPersonalized[baseValue]}]}
                  }],
                  Dynamic[Style[shipPlacementMsg, If[StringMatchQ[shipPlacementMsg, "Nave piazzata*" | "Tutte le navi*"], Darker[Green], Red]]]
                }],
                
                Spacer[30],
                
                (* Parte destra: visualizzazione griglia *)
                Column[{
                  Style["La tua flotta", Bold, 14],
                  (* Utilizziamo un Dynamic qui per assicurarci che la griglia si aggiorni *)
                  Dynamic[Util`showGrid[GetUserGrid[], True]],
                  
                  (* Mostra le navi rimanenti da posizionare *)
                  Style["Navi da posizionare:", Bold, 12],
                  Dynamic[
                    Module[{remaining = GetRemainingShipLengths[]},
                      If[Length[remaining] > 0,
                        "Lunghezze rimanenti: " <> ToString[remaining],
                        "Tutte le navi sono state posizionate!"
                      ]
                    ]
                  ]
                }]
              }]
            }]
          ],
          Button["Reset Game", phase = 1;]
        }],
      
      phase == 3,
        Column[{
          (* Fase di battaglia *)
          Style["Battaglia Navale in Base " <> ToString[baseValue], Bold, 20, Red],
          Spacer[10],
          (* Chiamiamo StartGame con tutti i parametri salvati *)
          Dynamic[StartGame[GetUserShips[], 
                            GetCpuShip[], 
                            GetUserGrid[], 
                            GetCpuGrid[], 
                            baseValue, 
                            $GridSize]
                            ],
          Spacer[10],
          Button["Reset Game",  ResetGame[];phase = 1;]
        }]
    ]]
  }]
];

End[];
EndPackage[];