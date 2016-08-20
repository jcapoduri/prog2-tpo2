unit oca.game;

interface

uses
  oca.space,
  oca.movements,
  oca.modifiers;

CONST
  GAMEFILESNAME = 'ocasaves';
  GAMEFILESMVMT = 'ocamoves';
  GAMEFILESRULE = 'ocarules';
  GAMEFILESPATH = 'games/';

const
  NMBSPACES = 63;

type
  tOcaPlayerInfo = record
                     looseTurns : integer;    
                     overTurns  : integer;
                     currentPos : integer;
                   end;
  tOcaGameData    = record
                      path      : tListOcaSpace;
                      rules     : tStackOca;
                      movements : tQueueOcaMvmt;
                    end;
  tOcaGameControl = record
                      players     : array[1..4] of tOcaPlayerInfo;
                      playersNbr  : integer;
                      currentPlay : integer;
                    end;
  tOcaGame        = record
                      data        : tOcaGameData;
                      control     : tOcaGameControl;
                    end;

  procedure create    (var this : tOcaGame);
  procedure generate  (var this : tOcaGame);
  procedure setupGame (var this : tOcaGame; players: integer);

  procedure setCurrentPlayer  (var this : tOcaGame; player: integer);
  function  getTotalCells     (var this : tOcaGame) : integer;
  function  currentPlayer     (var this : tOcaGame) : integer;
  function  currentPlayerInfo (var this : tOcaGame) : tOcaPlayerInfo;
  function  nextPlayer        (var this: tOcaGame) : integer;
  function  getCell           (var this: tOcaGame; number: integer) : tOcaSpace;

implementation

procedure create   (var this : tOcaGame);
begin
  oca.space.newEmptyList(this.data.path, GAMEFILESPATH, GAMEFILESNAME);
  oca.modifiers.newEmptyStack(this.data.rules, GAMEFILESPATH, GAMEFILESRULE);
  oca.movements.newEmptyQueue(this.data.movements, GAMEFILESPATH, GAMEFILESMVMT)
end;

procedure setupGame (var this : tOcaGame; players: integer);
var
  i: Integer;
begin
  this.control.playersNbr := players;
  for i := 1 to players do;
    begin
      this.control.players[i].looseTurns := 0;
      this.control.players[i].overTurns  := 0;
      this.control.players[i].currentPos := NULLIDX;
    end;

  this.control.currentPlay := 1;
end;

function insertCell(var this: tOcaGame; item: tOcaModifier): Boolean;
begin
  if oca.modifiers.existsCell(this.data.rules, item.cell) then
    insertCell := false
  else
    begin
      oca.modifiers.push(this.data.rules, item);
      insertCell := true;
    end;
end;

procedure generateGooseCells (var this : tOcaGame);
var 
  gooseCells, i : integer;
  cellNumber    : integer;
  item          : tOcaModifier;
begin
  gooseCells := (NMBSPACES div 2) - 1;
  for i:= 0 to gooseCells do
    begin
      cellNumber := Random(gooseCells) + (i * gooseCells);
      item       := oca.modifiers.generateModifier(this.data.rules, Goose, cellNumber);
      insertCell(this, item);
    end;
end;

procedure generateBridges(var this : tOcaGame);
var
  item : tOcaModifier;
begin
  
end;

procedure generate (var this : tOcaGame);
var
  spaces       : array[1..NMBSPACES] of integer;
  i, j, k, aux : integer;
  item         : tOcaSpace;
begin
  //load the array
  for i := 1 to NMBSPACES do
    spaces[i] := i;

  //scramble the array
  randomize;
  for i := 1 to NMBSPACES do
    begin
      j         := spaces[Random(NMBSPACES) + 1];
      k         := spaces[Random(NMBSPACES) + 1];
      aux       := spaces[j];
      spaces[j] := spaces[k];
      spaces[k] := aux;
    end;

  //load random array into path
  for i := 1 to NMBSPACES do
    begin
      item := oca.space.generateSpace(this.data.path, spaces[i]);
      oca.space.insert(this.data.path, item);
    end;

  //TODO: populate stack of fortune
  generateGooseCells(this);
end;

function getCell (var this: tOcaGame; number: integer) : tOcaSpace;
begin
  
end;

procedure setCurrentPlayer  (var this : tOcaGame; player: integer);
begin
  this.control.currentPlay := player;
end;

function getTotalCells (var this : tOcaGame) : integer;
begin
  getTotalCells := NMBSPACES;
end;

function  currentPlayer     (var this : tOcaGame) : integer;
begin
  currentPlayer := this.control.currentPlay;
end;

function  currentPlayerInfo (var this : tOcaGame) : tOcaPlayerInfo;
begin
  currentPlayerInfo := this.control.players[this.control.currentPlay];
end;

function  nextPlayer        (var this: tOcaGame) : integer;
var
  i: integer;
begin

end;

end.