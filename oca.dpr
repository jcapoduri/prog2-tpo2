program oca;

uses
  sysutils,
  oca.game,
  oca.space     in 'libs\oca.space.pas',
  oca.movements in 'libs\oca.movements.pas',
  oca.modifiers in 'libs\oca.modifiers.pas',
  mainOCA       in 'frm\mainOCA.pas' {mainOCAWidget};

var
  game : oca.game.tOcaGame;
  ui   : mainOCA.TForm1;

begin
  oca.game.create(game);
  oca.game.generate(game);
  ui := TForm1.new(game);
  ui.Show;
end.