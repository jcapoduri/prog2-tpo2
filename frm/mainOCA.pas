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
    playerLabel: TLabel;
    diceEdit: TEdit;
    diceThrowButton: TButton;
    Label4: TLabel;
    infoLabel: TLabel;
    nextTurnButton: TButton;
    procedure startButtonClick(Sender: TObject);
    procedure diceThrowEvent(Sender: TObject);
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

uses Math;

{$R *.dfm}


{ TForm1 }

procedure TForm1.initGame(var game: tOcaGame);
begin
     Self.ocaGame := game;
end;

constructor TForm1.new(owner:  TComponent);
begin
  inherited Create(owner);
  Randomize;
end;

procedure TForm1.updateUI;
begin
  Self.renderMatrix();
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
  i, total : integer;
  item     : tOcaCellInfo;
begin
  total := oca.game.getTotalCells(Self.ocaGame);
  for i := 0 to total - 1 do
    begin
      item := oca.game.getCellInfo(Self.ocaGame, i + 1);
      Self.tableBoardGrid.Cells[i mod 8, i div 8] := IntToStr(item.cellNmb) 
    end;
end;

procedure TForm1.diceThrowEvent(Sender: TObject);
var
  dice : integer;
begin
  dice := StrToIntDef(Self.diceEdit.Text, 0);
  if ((dice < 1) or (dice > 6)) then
     begin
       dice := RandomRange(1, 6);
       Self.diceEdit.Text := IntToStr(dice);
     end;
end;

end.
