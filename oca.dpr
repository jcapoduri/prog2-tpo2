program oca;

uses
  sysutils,
  Forms,
  oca.game,
  oca.space     in 'libs\oca.space.pas',
  oca.movements in 'libs\oca.movements.pas',
  oca.modifiers in 'libs\oca.modifiers.pas',
  mainOCA       in 'frm\mainOCA.pas' {mainOCAWidget};

var
  game : oca.game.tOcaGame;
  ui   : mainOCA.TForm1;

begin
  Randomize;
  Application.Initialize;
  Application.CreateForm(TForm1, ui);
  ui.initGame(game);
  Application.Run;
end.