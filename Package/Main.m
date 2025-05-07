(* ::Package:: *)

(* :Title: Main *)
(* :Context: Main` *)
(* :Author: Daniele Russo, Nicola Modugno *)
(* :Version: 1.0 *)
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

(* Questa funzione non \[EGrave] pi\[UGrave] necessaria perch\[EAcute] il suo contenuto \[EGrave] stato integrato
   direttamente nella PlacementUI come DynamicModule interno. La mantengo commentata
   per riferimento ma non viene pi\[UGrave] chiamata.*)

StartPlacementPhase[userBase_] := DynamicModule[
  {currentShip = 1, shipLengths = {5, 4, 3, 2, 1}, start = "", end = "", 
   message = "", shipsPlaced = 0, placementDone = False},
   
  Column[{
    Style["Fase di Posizionamento Navi", Bold, 16],
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
                shipsPlaced++;
                start = ""; end = "";
                
                If[shipsPlaced == Length[shipLengths],
                  placementDone = True;
                  message = "Tutte le navi sono state posizionate!";
                ,
                  currentShip++;
                  message = result[[2]]; (* Messaggio di successo *)
                ]
              ,
                (* Errore - mostriamo il messaggio specifico ritornato *)
                message = result[[2]];
              ]
            ],
            Enabled -> Dynamic[!placementDone]
          ]},
          {"", Button["Avvia Battaglia",
              Module[{cpuShips = generateCPUShips[$GridSize]},
                (* Passiamo tutti i parametri necessari a StartGame *)
                StartGame[GetUserShips[], cpuShips, $UserGrid, 
                        createGrid[cpuShips,$GridSize], 
                        userBase, $GridSize]
              ],
              Enabled -> Dynamic[placementDone]
            ]
          }
        }],
        Dynamic[Style[message, If[StringMatchQ[message, "Nave piazzata*" | "Tutte le navi*"], Darker[Green], Red]]]
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
];


(* Interfaccia utente principale *)

PlacementUI[] := DynamicModule[
  {seedValue = RandomInteger[1024], baseValue = 2, gridSize = 10, phase = 1, initDone = False, message = "", 
   cpuShips, battleStarted = False, userShips, userGrid, cpuGrid}, 
  
  Column[{
    Dynamic[Which[
      phase == 1,
        Column[{
          Row[{
            "Inserisci seed: ",
            InputField[Dynamic[seedValue], Number, ImageSize -> Small]
          }],
          Spacer[10],
          Row[{
            "Inserisci la base su cui ti vuoi esercitare: ",
            PopupMenu[Dynamic[baseValue], {2, 8, 16}]
          }],
          Spacer[10],
          Dynamic[message],
          Spacer[10],
          Button["Conferma Impostazioni",
            If[isSeed[seedValue] && isBase[baseValue],
              InitPhase[seedValue, baseValue, gridSize];
              phase = 2;
              initDone = True;
              message = "Impostazioni confermate! Seed: " <> ToString[seedValue] <> ", Base: " <> ToString[baseValue];,
              message = "Seed o base non validi!";
            ]
          ]
        }],
      
      phase == 2,
        Column[{
          DynamicModule[
            {currentShip = 1, shipLengths = {5, 4, 3, 2, 1}, start = "", end = "", 
             shipPlacementMsg = "", shipsPlaced = 0, placementDone = False},
            
            Column[{
              Style["Fase di Posizionamento Navi", Bold, 16],
              Row[{
                Column[{
                  Dynamic[Row[{"Nave ", currentShip, ":"}]],
                  Grid[{
                    {"Inizio:", InputField[Dynamic[start], String, Enabled -> Dynamic[!placementDone]]},
                    {"Fine:", InputField[Dynamic[end], String, Enabled -> Dynamic[!placementDone]]},
                    {"", Button["Conferma",
                      Module[{result = PlaceUserShip[start, end]},
                        If[result[[1]],
                          shipsPlaced++;
                          start = ""; end = "";
                          If[shipsPlaced == Length[shipLengths],
                            placementDone = True;
                            shipPlacementMsg = "Tutte le navi sono state posizionate!";,
                            currentShip++;
                            shipPlacementMsg = result[[2]];
                          ]
                        ,
                          shipPlacementMsg = result[[2]];
                        ]
                      ],
                      Enabled -> Dynamic[!placementDone]
                    ]},
                    {"", Button["Avvia Battaglia",
                        userShips = GetUserShips[];
                        userGrid = GetUserGrid[];
                        cpuShips = generateCPUShips[$GridSize];
                        cpuGrid = createGrid[cpuShips, $GridSize];
                        phase = 3;
                      ,
                        Enabled -> Dynamic[placementDone]
                      ]
                    },
                    {"", helpUser[baseValue]}
                  }],
                  Dynamic[Style[shipPlacementMsg, If[
                    StringMatchQ[shipPlacementMsg, "Nave piazzata*" | "Tutte le navi*"],
                    Darker[Green], Red]]]
                }],
                Spacer[30],
                Column[{
                  Style["La tua flotta", Bold, 14],
                  Dynamic[Util`showGrid[GetUserGrid[], True]],
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
          Style["Battaglia Navale in Base " <> ToString[baseValue], Bold, 20, Red],
          Spacer[10],
          Dynamic[StartGame[userShips, cpuShips, userGrid, cpuGrid, baseValue, $GridSize]],
          Spacer[10],
          Button["Nuova Partita", phase = 1; ResetGame[];]
        }]
    ]]
  }]
];


End[];
EndPackage[];
