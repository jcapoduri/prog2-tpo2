unit mainOCA;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids;

type
  TForm1 = class(TForm)
    StringGrid1: TStringGrid;
  protected
    ocaGame : oca.game.tOcaGame
    procedure updateUI();
  public
    Constructor new (var game : oca.game.tOcaGame);
    
  end;

var
  mainOCAWidget: TForm1;

implementation

{$R *.dfm}


{ TForm1 }

constructor TForm1.new(var game: tOcaGame);
begin

end;

end.
