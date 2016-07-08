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
  
end;

procedure generate (var this : tOcaGame);
begin
  
end;

procedure load     (var this : tOcaGame);
begin
  
end;


end.