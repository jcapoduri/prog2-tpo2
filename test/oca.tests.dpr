program oca.tests;

uses
  sysutils,
  oca.game      in '../oca.game.pas',
  oca.space     in '../libs/oca.space.pas',
  oca.movements in '../libs/oca.movements.pas',
  oca.modifiers in '../libs/oca.modifiers.pas';

const
  n = 20;

var
  game        : oca.game.tOcaGame;
  mvt         : oca.movements.tOcaMovement;
  i, j, k     : integer;
  path        : tListOcaSpace;
  rules       : tStackOca;
  tile        : tOcaSpace;
  modifier    : oca.modifiers.tModifiers;
  playerInfo  : tOcaPlayerInfo;

procedure dumpPathData(path : string);
var
  pos  : integer;
  data : oca.space.tData;
  item : oca.space.tOcaSpace;
begin
  pos := 0;
  assign(data, path + '.dat');
  reset(data);
  while not eof(data) do
    begin
      seek(data, pos);
      read(data, item);
      writeln(pos, ' | ', item.cell, ' | ', item.next);
      pos := pos + 1
    end;
  close(data);  
end;

procedure showPath(path : string);
var
  pos  : integer;
  data : oca.space.tData;
  ctrl : oca.space.tControl;
  item : oca.space.tOcaSpace;
  Rc   : oca.space.tControlRecord;
begin
  assign(data, path + '.dat');
  reset(data);
  assign(ctrl, path + '.ctrl');
  reset(ctrl);
  seek(ctrl, 0);
  read(ctrl, Rc);
  writeln('Control: f: ', Rc.first, ' l: ', Rc.last, ' c: ', Rc.count);
  pos := Rc.first;
  repeat
    begin
      seek(data, pos);
      read(data, item);
      writeln(pos, ' | ', item.cell, ' | ', item.next);
      pos := item.next;
    end;
  until  pos = NULLIDX;
  close(data);
  close(ctrl);
end;

procedure showStack(path : string);
var
  pos  : integer;
  data : oca.modifiers.tData;
  ctrl : oca.modifiers.tControl;
  item : oca.modifiers.tOcaModifier;
  Rc   : oca.modifiers.tControlRecord;
  stack: oca.modifiers.tStackOca;
begin
  assign(data, path + '.dat');
  reset(data);
  assign(ctrl, path + '.ctrl');
  reset(ctrl);
  seek(ctrl, 0);
  read(ctrl, Rc);
  writeln('Control: f: ', Rc.first, ' e: ', Rc.erased);
  pos := Rc.first;
  repeat
    begin
      seek(data, pos);
      read(data, item);
      writeln(pos, ' | ', item.modifier, ' | ', item.cell, ' | ', item.next);
      pos := item.next;
    end;
  until  pos = NULLIDX;

  close(data);
  close(ctrl);
end;

begin
  writeln('instanciate the game');
  oca.game.create(game);

  writeln('generate new game');
  oca.game.generate(game);

  writeln('dump generated game');
  dumpPathData(GAMEFILESPATH + GAMEFILESNAME);

  writeln('show path');
  showPath(GAMEFILESPATH + GAMEFILESNAME);

  writeln('show stack');
  showStack(GAMEFILESPATH + GAMEFILESRULE);

  writeln('set up a 3 players game');
  oca.game.setupGame(game, 3);


  writeln('load ', n, ' movements to this play');
  Randomize;
  for i := 0 to n do
    begin
      j := oca.game.currentPlayer(game);
      k := Random(2) + 1; //1 to 3 - more chances to get some special tile
      
      writeln('before mvmnt');
      playerInfo  := game.control.players[j];
      writeln('p:', j, ' mvt:', k, ' pos: ', playerInfo.currentCell);
      oca.game.movePlayer(game, j, k);
      
      writeln('after mvmnt');
      playerInfo  := game.control.players[j];
      writeln('p:', j, ' mvt:', k, ' pos: ', playerInfo.currentCell);
      tile     := oca.space.get(game.data.path, playerInfo.currentCell);
      oca.modifiers.search(game.data.rules, tile.cell, modifier);
      writeln('casillero actual: ', tile.cell, ' modifier: ', modifier);
      
      oca.game.playerReactToCell(game, j);
      writeln('after react: ');
      playerInfo  := game.control.players[j];
      writeln('p:', j, ' mvt:', k, ' pos: ', playerInfo.currentCell);
      tile     := oca.space.get(game.data.path, playerInfo.currentCell);
      oca.modifiers.search(game.data.rules, tile.cell, modifier);
      writeln('casillero actual: ', tile.cell, ' modifier: ', modifier);
      writeln('-----------------------------------------------------');
      oca.game.nextPlayer(game);
    end;
  //it should call in a loop
  // [X] current player 
  // [X] movePlayer
  // [X] applyMovement
  // [X] react
  // [ ] render
  // [ ] checkwinner
  // [X] nextplayer

  writeln('All tests runned');
end.
