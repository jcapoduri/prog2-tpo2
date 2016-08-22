program oca.tests;

uses
  sysutils,
  oca.game      in '../oca.game.pas',
  oca.space     in '../libs/oca.space.pas',
  oca.movements in '../libs/oca.movements.pas',
  oca.modifiers in '../libs/oca.modifiers.pas';

const
  n = 2;

var
  game  : oca.game.tOcaGame;
  mvt   : oca.movements.tOcaMovement;
  i     : integer;
  path  : tListOcaSpace;
  rules : tStackOca;

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
  until  pos = NULLIDX
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

  {oca.modifiers.loadStack(stack, path, '');
  while not oca.modifiers.isEmpty(stack) do
    item := oca.modifiers.pop(stack);

seek(ctrl, 0);
  read(ctrl, Rc);
   writeln('Control: f: ', Rc.first, ' e: ', Rc.erased);

  oca.modifiers.push(stack, item);

  seek(ctrl, 0);
  read(ctrl, Rc);
   writeln('Control: f: ', Rc.first, ' e: ', Rc.erased);
   pos := Rc.erased;
  repeat
    begin
      seek(data, pos);
      read(data, item);
      writeln(pos, ' | ', item.modifier, ' | ', item.cell, ' | ', item.next);
      pos := item.next;
    end;
  until  pos = NULLIDX}
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

  writeln('All tests runned');
end.
