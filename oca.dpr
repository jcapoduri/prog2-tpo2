program oca;

uses
  sysutils,
  oca.game,
  oca.space.list in 'libs\oca.space.list.pas',
  mainOCA in 'frm\mainOCA.pas' {mainOCAWidget};

var
  game : oca.game.tOcaGame;

begin
  oca.game.create(game);
  oca.game.generate(game);
end.