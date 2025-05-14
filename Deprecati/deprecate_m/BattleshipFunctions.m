(* ::Package:: *)

BeginPackage["BattleshipFunctions`"]

(* Dichiarazione dei simboli esportati *)
CoordinateToRC::usage = 
  "CoordinateToRC[coord_Integer] converte un numero (0-99) nelle coordinate {riga, colonna} per una griglia 10\[Times]10.";
InBounds::usage = 
  "InBounds[coords_List] verifica che tutte le coordinate siano comprese tra 0 e 99.";
IsContiguous::usage = 
  "IsContiguous[coords_List] controlla se le coordinate sono contigue, in orizzontale o in verticale.";
HasNoOverlap::usage = 
  "HasNoOverlap[coords_List, board] verifica che nessuna delle coordinate sia gi\[AGrave] occupata sulla board.";
UpdateBoard::usage = 
  "UpdateBoard[board, coords_List] aggiorna la board contrassegnando le posizioni fornite con 1.";
ValidateShipInput::usage = 
  "ValidateShipInput[input_String, shipLength_Integer, board] valida l'input fornito dall'utente e restituisce le coordinate se valide o $Failed altrimenti.";
BoardDisplay::usage = 
  "BoardDisplay[board] restituisce una griglia che mostra la board di gioco (le celle occupate sono contrassegnate con \"X\").";
ShowBattleshipInterface::usage = 
  "ShowBattleshipInterface[base_String] restituisce un'interfaccia dinamica per il posizionamento delle navi, in base alla base scelta.";

Begin["`Private`"]

ClearAll[CoordinateToRC, InBounds, IsContiguous, HasNoOverlap, UpdateBoard,
  ValidateShipInput, BoardDisplay, ShowBattleshipInterface];

(* Converte un numero (0-99) in coordinate {riga, colonna} per una griglia 10\[Times]10 *)
CoordinateToRC[coord_Integer] := {Floor[coord/10] + 1, Mod[coord, 10] + 1};

(* Verifica che tutti i numeri siano compresi fra 0 e 99 *)
InBounds[coords_List] := AllTrue[coords, 0 <= # < 100 &];

(* Verifica se le coordinate sono contigue (orizzontale o verticale) *)
IsContiguous[coords_List] := Module[{sorted = Sort[coords], rows, cols},
  If[Length[coords] == 1, Return[True]];
  rows = CoordinateToRC /@ sorted;
  cols = rows[[All, 2]];
  If[SameQ @@ (rows[[All, 1]]),
    Return[And @@ Thread[Differences[cols] == 1]];
  ];
  If[SameQ @@ (rows[[All, 2]]),
    Return[And @@ Thread[Differences[rows[[All, 1]]] == 1]];
  ];
  False
];

(* Controlla che nessuna coordinate sia gi\[AGrave] occupata nella board *)
HasNoOverlap[coords_List, board_] := Module[{rc},
  Do[
    rc = CoordinateToRC[coord];
    If[board[[rc[[1]], rc[[2]]]] =!= 0, Return[False]],
    {coord, coords}
  ];
  True
];

(* Aggiorna la board impostando il valore 1 nelle posizioni indicate *)
UpdateBoard[board_, coords_List] := Module[{newBoard = board, rc},
  Do[
    rc = CoordinateToRC[coord];
    newBoard[[rc[[1]], rc[[2]]]] = 1,
    {coord, coords}
  ];
  newBoard
];

(* Valida l'input dell'utente per il posizionamento di una nave:
   - L'input \[EGrave] una stringa con numeri separati da virgola,
   - Deve contenere esattamente shipLength numeri,
   - I numeri devono essere interi, compresi fra 0 e 99,
   - Le coordinate devono essere contigue e non sovrapporsi con quelle gi\[AGrave] occupate *)
ValidateShipInput[input_String, shipLength_Integer, board_] := Module[{nums, valid},
  nums = ToExpression["{" <> input <> "}"];
  valid = ListQ[nums] && Length[nums] === shipLength && AllTrue[nums, IntegerQ] &&
    InBounds[nums] && IsContiguous[nums] && HasNoOverlap[nums, board];
  If[valid, nums, $Failed]
];

(* Mostra la board come una griglia; le celle occupate vengono contrassegnate con "X" *)
BoardDisplay[board_] := 
  Grid[Table[
    If[board[[i, j]] === 1, Style["X", Red, Bold], ""],
    {i, Length[board]}, {j, Length[board[[1]]]}],
   Frame -> All, Spacings -> {1, 1}];

(* Interfaccia interattiva per il posizionamento delle navi.
   Le dimensioni delle navi sono {5, 4, 3, 2}. L'utente inserisce una stringa con le coordinate. *)
ShowBattleshipInterface[base_String] := DynamicModule[
  {
    board = Table[0, {10}, {10}],
    shipSizes = {5, 4, 3, 2},
    currentShip = 1,
    input = "",
    message = ""
  },
  Column[{
    Style["Base scelta: " <> base, "Section"],
    Dynamic@BoardDisplay[board],
    Dynamic[
      If[currentShip <= Length[shipSizes],
        Column[{
          Style["Inserisci le coordinate per la nave di lunghezza " <>
              ToString[shipSizes[[currentShip]]], "Section"],
          InputField[Dynamic[input], String, FieldSize -> {30, 1}],
          Button["Invia",
            Module[{coords},
              coords = ValidateShipInput[input, shipSizes[[currentShip]], board];
              If[coords === $Failed,
                message = "Input non valido. Verifica numero, adiacenza, range (0-99) ed eventuali sovrapposizioni.",
                board = UpdateBoard[board, coords];
                currentShip++;
                input = "";
                message = "Nave aggiunta correttamente!"
              ]
            ],
            Method -> "Queued"
          ],
          Dynamic@message
        }],
        Style["Tutte le navi sono state posizionate!", "Section"]
      ]
    ]
  },
  Spacings -> 2
  ]
];

End[]
EndPackage[]
