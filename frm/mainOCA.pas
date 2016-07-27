unit mainOCA;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids,
  oca.game;

type
  TForm1 = class(TForm)
    StringGrid1: TStringGrid;
  protected
    ocaGame : oca.game.tOcaGame;
    procedure updateUI();
  public
    Constructor new      (owner:  TComponent);
    procedure   initGame (var game : oca.game.tOcaGame);
  end;

var
  mainOCAWidget: TForm1;

implementation

{$R *.dfm}


{ TForm1 }

procedure TForm1.initGame(var game: tOcaGame);
begin
     Self.ocaGame := game;
end;

constructor TForm1.new(owner:  TComponent);
begin
  inherited Create(owner);
end;

procedure TForm1.updateUI;
begin

end;

end.
