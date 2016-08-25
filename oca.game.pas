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
                     overTurns   : integer;
                     currentCell : oca.space.idxRange;
                     currentNumb : integer;
                   end;
  tOcaGameData    = record
                      path      : tListOcaSpace;
                      rules     : tStackOca;
                      movements : tQueueOcaMvmt;
                    end;
  tOcaGameControl = record
                      players     : array[1..4] of tOcaPlayerInfo;
                      playersNbr  : integer;
                      currentPlayer : integer;
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
  function  getCellInfo       (var this: tOcaGame; number: integer) : tOcaCellInfo;
  function  getCurrentPlayerCellInfo (var this: tOcaGame) : tOcaCellInfo;

  procedure movePlayer        (var this : tOcaGame; player, movements : integer);

  procedure playerReactToCell (var this : tOcaGame; player : integer);
  function currentPlayerWon   (var this: tOcaGame) : boolean;

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
  for i := 1 to players do
    begin
      this.control.players[i].overTurns   := 0;
      this.control.players[i].currentNumb := 1;
      this.control.players[i].currentCell := oca.space.first(this.data.path);
    end;

  this.control.currentPlayer := 1;
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
  gooseCells := 1
  cellNumber := NMBSPACES;
  repeat
    begin
      cellNumber := cellNumber - Random(2) - 4;
      item       := oca.modifiers.generateModifier(this.data.rules, Goose, cellNumber);
      insertCell(this, item);
    end;
  until cellNumber >= gooseCells;
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
  generateRuleCell(this, Inn, 1);
  generateRuleCell(this, Prison, 1);
  generateRuleCell(this, Pit, 1);
  generateRuleCell(this, Labyrinth, NMBSPACES div 2);
  generateRuleCell(this, Death, NMBSPACES div 2);
end;

function getCellInfo (var this: tOcaGame; number: integer) : tOcaCellInfo;
var
  i        : integer;
  tile     : oca.space.idxRange;
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

  //oca.space.search(this.data.path, number, tile);
  //update player info
  for i := 1 to this.control.playersNbr do
    begin
      player := this.control.players[i];
      if number = player.currentNumb then
        item.players[i] := true;
    end;

  //update tile info
  if oca.modifiers.search(this.data.rules, number, modifier) then
    item.modifier := modifier;

  getCellInfo    := item
end;

function  getCurrentPlayerCellInfo (var this: tOcaGame) : tOcaCellInfo;
var
  item     : tOcaCellInfo;
  tile     : tOcaSpace;
  player   : tOcaPlayerInfo;
  modifier : tModifiers;
begin
  //clear up item
  item.players[1] := false;
  item.players[2] := false;
  item.players[3] := false;
  item.players[4] := false;
  item.cellNmb    := 0;
  item.modifier   := None;

  //retrieve current player info
  player       := currentPlayerInfo(this);
  tile         := oca.space.get(this.data.path, player.currentCell);
  item.cellNmb := tile.cell;

  //update tile info
  if oca.modifiers.search(this.data.rules, item.cellNmb, modifier) then
    item.modifier := modifier;

  getCurrentPlayerCellInfo := item
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

function searchStackByModifier (var this : tOcaGame; modifier : tModifiers) : integer;
var
  item : tOcaModifier;
begin
  item := oca.modifiers.pop(this.data.rules);
  if item.modifier = modifier then
    searchStackByModifier := item.cell
  else
    searchStackByModifier := searchStackByModifier(this, modifier);
  push(this.data.rules, item);
end;

procedure reactToGoose (var this : tOcaGame; var player: tOcaPlayerInfo);
var
  newTile  : integer;
  modifier : tOcaModifier;
  item     : tOcaSpace;
begin
  item     := oca.space.get(this.data.path, player.currentCell);
  modifier := oca.modifiers.generateModifier(this.data.rules, Goose, item.cell);
  if oca.modifiers.nextAfter(this.data.rules, modifier, newTile) then
    begin
      oca.space.search(this.data.path, newTile, player.currentCell);
    end;
  player.overTurns := player.overTurns + 1;
end;

procedure reactToTile      (var this : tOcaGame; var player: tOcaPlayerInfo; overturns : integer);
begin
  player.overturns := player.overturns + overturns;
end;

procedure reactToBridge (var this : tOcaGame; var player: tOcaPlayerInfo);
var
  i        : integer;
  item     : tOcaSpace;
  modifier : tOcaModifier;
begin
  item := oca.space.get(this.data.path, player.currentCell);
  modifier := oca.modifiers.generateModifier(this.data.rules, Bridge, item.cell);
  if oca.modifiers.nextAfter(this.data.rules, modifier, i) then
    oca.space.search(this.data.path, i, player.currentCell)
  else
    begin
      i := searchStackByModifier(this, Bridge);
      oca.space.search(this.data.path, i, player.currentCell);
    end;
  player.currentNumb := i;
end;

procedure reactToLabyrinth (var this : tOcaGame; var player: tOcaPlayerInfo);
var
  i        : integer;
  item     : tOcaSpace;
  modifier : tModifiers;
begin
  //go back 12 positions
  for i := 0 to 12 do player.currentCell := oca.space.prev(this.data.path, player.currentCell);
  //keep going back until normal position found
  item := oca.space.get(this.data.path, player.currentCell);
  while oca.modifiers.search(this.data.rules, item.cell, modifier) do
    begin
      player.currentCell := oca.space.prev(this.data.path, player.currentCell);
      item := oca.space.get(this.data.path, player.currentCell);
      player.currentNumb := item.cell;
    end;
end;

procedure reactToDeath     (var this : tOcaGame; var player: tOcaPlayerInfo);
begin
  player.currentCell := oca.space.first(this.data.path);
  player.currentNumb := 1;
end;


procedure setCurrentPlayer  (var this : tOcaGame; player: integer);
begin
  this.control.currentPlayer := player;
end;

function getTotalCells (var this : tOcaGame) : integer;
begin
  getTotalCells := NMBSPACES;
end;

function  currentPlayer     (var this : tOcaGame) : integer;
begin
  currentPlayer := this.control.currentPlayer;
end;

function  currentPlayerInfo (var this : tOcaGame) : tOcaPlayerInfo;
begin
  currentPlayerInfo := this.control.players[this.control.currentPlayer];
end;

procedure applyPlayerMovement (var this : tOcaGame; player, movements : integer);
var
  moveForward : boolean;
  i           : integer;
  tempIdx     : oca.space.idxRange;
  playerInfo  : tOcaPlayerInfo;
  item        : tOcaSpace;
begin
  moveForward := true;
  playerInfo  := this.control.players[player];
  for i  := movements downto 1 do
    begin
      if moveForward then
        begin
          tempIdx := oca.space.next(this.data.path, playerInfo.currentCell);
          if tempIdx = oca.space.last(this.data.path) then
            moveForward := false;
        end
      else
        tempIdx := oca.space.prev(this.data.path, playerInfo.currentCell);
      playerInfo.currentCell := tempIdx;
      item                   := oca.space.get(this.data.path, playerInfo.currentCell);
      playerInfo.currentNumb := item.cell;
    end;
  this.control.players[player] := playerInfo;
end;

procedure movePlayer (var this : tOcaGame; player, movements : integer);
var
  item : tOcaMovement;
begin
  //add movement to queue
  item := oca.movements.createMovement(this.data.movements, player, movements);
  oca.movements.queue(this.data.movements, item);

  //apply movement
  applyPlayerMovement(this, player, movements);
end;

procedure playerReactToCell (var this : tOcaGame; player : integer);
var
  moveForward : boolean;
  i           : integer;
  tempIdx     : oca.space.idxRange;
  modifier    : tModifiers;
  tile        : tOcaSpace;
  playerInfo  : tOcaPlayerInfo;
begin
  playerInfo := this.control.players[player];
  tile       := oca.space.get(this.data.path, playerInfo.currentCell);

  oca.modifiers.search(this.data.rules, tile.cell, modifier);
  case modifier of
    Goose     : reactToGoose(this, playerInfo);
    Dice      : reactToTile(this, playerInfo, 1);
    Prison    : reactToTile(this, playerInfo, -3);
    Inn       : reactToTile(this, playerInfo, -2);
    Pit       : reactToTile(this, playerInfo, -4);
    Bridge    : reactToBridge(this, playerInfo);
    Labyrinth : reactToLabyrinth(this, playerInfo);
    Death     : reactToDeath(this, playerInfo);
  end;
  this.control.players[player] := playerInfo;
end;

function  nextPlayer        (var this: tOcaGame) : integer;
var
  next    : integer;
  current : tOcaPlayerInfo;
  found   : boolean;
begin
  next    := this.control.currentPlayer;
  current := this.control.players[next];
  found   := false;
  if (current.overturns > 0) then
    begin
      current.overturns := current.overturns - 1;
      this.control.players[next] := current;
    end
  else
    while not found do
      begin
        next := (next mod this.control.playersNbr) + 1;
        current := this.control.players[next];
        if current.overTurns < 0 then
          begin
            current.overTurns := current.overTurns + 1;
            this.control.players[next] := current;
          end
        else
          found := true;
      end;
  this.control.currentPlayer := next;
  nextPlayer := next;
end;

function currentPlayerWon (var this: tOcaGame) : boolean;
var
  current : tOcaPlayerInfo;
begin
  current := currentPlayerInfo(this);
  currentPlayerWon := current.currentCell = oca.space.last(this.data.path);
end;

end.
