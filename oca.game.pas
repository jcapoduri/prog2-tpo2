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
  tOcaCellInfo   = record
                     cellNmb  : integer;
                     players  : array [1..4] of boolean;
                     modifier : tModifiers;
                   end;
  tOcaPlayerInfo = record
                     looseTurns  : integer;    
                     overTurns   : integer;
                     currentCell : tOcaSpace;
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
  function  getCellInfo (var this: tOcaGame; number: integer) : tOcaCellInfo;

implementation

procedure create (var this : tOcaGame);
begin
  oca.space.newEmptyList(this.data.path, GAMEFILESPATH, GAMEFILESNAME);
  oca.modifiers.newEmptyStack(this.data.rules, GAMEFILESPATH, GAMEFILESRULE);
  oca.movements.newEmptyQueue(this.data.movements, GAMEFILESPATH, GAMEFILESMVMT)
end;

procedure setupGame (var this : tOcaGame; players: integer);
var
  i   : Integer;
  idx : oca.space.idxRange;
begin
  this.control.playersNbr := players;
  for i := 1 to players do;
    begin
      this.control.players[i].looseTurns := 0;
      this.control.players[i].overTurns  := 0;
      oca.space.search(this.data.path, 1, idx);
      this.control.players[i].currentCell := oca.space.get(this.data.path, idx);
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
  item1, item2    : tOcaModifier;
  cell1, cell2    : integer;
  bridgesInserted : boolean;
begin
  bridgesInserted := false;
  while not bridgesInserted do
    begin
      cell1 := random(NMBSPACES - 6);
      cell2 := cell1 + 6;
      if ( not oca.modifiers.existsCell(this.data.rules, cell1) ) and 
         ( not oca.modifiers.existsCell(this.data.rules, cell2) ) then
         begin
           item1 := oca.modifiers.generateModifier(this.data.rules, Bridge, cell1); 
           item2 := oca.modifiers.generateModifier(this.data.rules, Bridge, cell2); 
           insertCell(this, item1);
           insertCell(this, item2);
           bridgesInserted := true;
         end;
    end;
end;

procedure generateDices(var this:tOcaGame);
var
  item1, item2        : tOcaModifier;
  cell1, cell2, space : integer;
  dicesInserted       : boolean;
begin
  dicesInserted := false;
  while not dicesInserted do
    begin
      space := random(NMBSPACES - 20) + 20; //to have a random number upper than 20 but below NMBSPACES
      cell1 := random(NMBSPACES - space);
      cell2 := cell1 + space;
      if ( not oca.modifiers.existsCell(this.data.rules, cell1) ) and 
         ( not oca.modifiers.existsCell(this.data.rules, cell2) ) then
         begin
           item1 := oca.modifiers.generateModifier(this.data.rules, Dice, cell1); 
           item2 := oca.modifiers.generateModifier(this.data.rules, Dice, cell2); 
           insertCell(this, item1);
           insertCell(this, item2);
           dicesInserted := true;
         end;
    end;
end;

procedure generateRuleCell(var this:tOcaGame; modifier : tModifiers; lowerLimit : integer);
var
  item     : tOcaModifier;
  cell     : integer;
  inserted : boolean;
begin
  inserted := false;
  while not inserted do
    begin
      cell := random(NMBSPACES - lowerLimit) + lowerLimit;
      if not oca.modifiers.existsCell(this.data.rules, cell) then
         begin
           item := oca.modifiers.generateModifier(this.data.rules, modifier, cell); 
           insertCell(this, item);
           inserted := true;
         end;
    end;
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
  generateBridges(this);
  generateDices(this);
  generateRuleCell(this, Inn, 0);
  generateRuleCell(this, Prison, 0);
  generateRuleCell(this, Pit, 0);
  generateRuleCell(this, Labyrinth, NMBSPACES div 2);
  generateRuleCell(this, Death, NMBSPACES div 2);
end;

function getCellInfo (var this: tOcaGame; number: integer) : tOcaCellInfo;
var
  i        : integer;
  item     : tOcaCellInfo;
  player   : tOcaPlayerInfo;
  modifier : tModifiers;
begin
  item.players[1] := false;
  item.players[2] := false;
  item.players[3] := false;
  item.players[4] := false;
  item.cellNmb    := number;
  item.modifier   := None;

  //update player info
  for i := 1 to this.control.playersNbr do
    begin
      player := this.control.players[i];
      if player.currentCell.cell = number then
        item.players[i] := true;
    end;

  //update tile info
  if oca.modifiers.search(this.data.rules, number, modifier) then
    item.modifier := modifier;

  getCellInfo    := item
end;


// retrieves the next tOcaSpace with the same modifier that item AFTER the item itself from the stack
function retrieveNextItemNotThisOne (var this : tOcaGame; item : tOcaSpace; var resultItem : tOcaSpace) : boolean;
var
  keepLooping : boolean;
  keepLooking : boolean;
  found       : boolean;
  currItem    : tOcaModifier;
  modifier    : tModifiers;
  tempStack   : tStackOca;
  pos         : oca.space.idxRange;
begin
  oca.modifiers.newEmptyStack(tempStack, GAMEFILESPATH, GAMEFILESRULE + '.tmp');
  keepLooping := not oca.modifiers.isEmpty(this.data.rules);
  keepLooking := true;
  found       := false;

  // loop until find the item
  while keepLooping and not found do
    begin
      currItem    := oca.modifiers.pop(this.data.rules);
      keepLooping := not oca.modifiers.isEmpty(this.data.rules);
      if keepLooking then
        if currItem.cell = item.cell then 
          begin
            keepLooking := false;
            modifier    := currItem.modifier;
          end
      else
          if currItem.modifier = modifier then
            found       := true;
      oca.modifiers.push(tempStack, currItem);
    end;

  //update the result
  if found then
    begin
      found      := oca.space.search(this.data.path, currItem.cell, pos);
      resultItem := oca.space.get(this.data.path, pos);
    end;
  
  // move all items into the rules stack
  while not oca.modifiers.isEmpty(tempStack) do
    oca.modifiers.push(this.data.rules, pop(tempStack));

  retrieveNextItemNotThisOne := found;
end;

procedure reactToGoose (var this : tOcaGame; player: integer);
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

procedure movePlayer (var this : tOcaGame; player, movements : integer);
var
  i: Integer;
begin
  
end;

procedure playerReactToCell (var this : tOcaGame; player : integer);
begin
  
end;

function  nextPlayer        (var this: tOcaGame) : integer;
var
  i: integer;
begin

end;

end.