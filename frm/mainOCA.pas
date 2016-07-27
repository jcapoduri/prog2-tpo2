unit mainOCA;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids,
  oca.game, StdCtrls, Buttons;

type
  TForm1 = class(TForm)
    tableBoardGrid: TStringGrid;
    gameGroupBox: TGroupBox;
    ingameGroupBox: TGroupBox;
    Label1: TLabel;
    playersComboBox: TComboBox;
    startButton: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Button3: TButton;
    BitBtn1: TBitBtn;
    procedure startButtonClick(Sender: TObject);
  protected
    ocaGame : oca.game.tOcaGame;
    procedure updateUI();
    procedure renderMatrix();
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

procedure TForm1.startButtonClick(Sender: TObject);
var
  players: Integer;
begin
  Self.gameGroupBox.Enabled   := false;
  Self.ingameGroupBox.Enabled := true;
  players := 2;
  if (Self.playersComboBox.Text = '3 Jugadores') then players := 3;
  if (Self.playersComboBox.Text = '4 Jugadores') then players := 4;
  setupGame(Self.ocaGame, players);
  updateUI;
end;

procedure TForm1.renderMatrix;
var
  i: integer;
begin

end;

end.
