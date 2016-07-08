unit oca.game;

interface

uses
  oca.space.list;

CONST
  GAMEFILESNAME = 'ocasaves';
  GAMEFILESPATH = 'games/';

const
  NMBSPACES = 63;

type
  tOcaGame = record
              path  : tListOcaSpace;
              rules : integer;
            end;

  procedure create   (var this : tOcaGame);
  procedure generate (var this : tOcaGame);
  procedure load     (var this : tOcaGame);

implementation

procedure create   (var this : tOcaGame);
begin
  oca.space.list.newEmptyList(this.path, GAMEFILESPATH, GAMEFILESNAME);
  //TO DO create rules
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
      item.number := spaces[i];
      oca.space.list.insert(this.path, item);
    end;
end;

procedure load     (var this : tOcaGame);
begin
  
end;


end.