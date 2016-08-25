unit mainOCA;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids,
  oca.game, oca.modifiers, Buttons, StdCtrls;

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
    procedure startButtonClick(Sender: TObject);
    procedure diceThrowEvent(Sender: TObject);
    procedure renderTile(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
  protected
    ocaGame   : oca.game.tOcaGame;
    gameReady : boolean;
    procedure updateUI();
    procedure renderMatrix();
    procedure processThrown();
    procedure processMovement();
  public
    Constructor new      (owner:  TComponent);
    procedure   initGame (var game : oca.game.tOcaGame);
  end;

var
  mainOCAWidget: TForm1;

implementation

uses Math, DateUtils;

{$R *.dfm}


{ TForm1 }

procedure TForm1.initGame(var game: tOcaGame);
begin
     Self.ocaGame := game;
end;

constructor TForm1.new(owner:  TComponent);
begin
  inherited Create(owner);
  //if System.RandSeed = 0 then Randomize;
  Self.gameReady := false;
end;

procedure TForm1.updateUI;
begin
  Self.playerLabel.Caption := IntToStr(oca.game.currentPlayer(Self.ocaGame));
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
  Self.gameReady := true;
  updateUI;
end;

procedure TForm1.renderMatrix;
var
  i, total : integer;
begin
  total := oca.game.getTotalCells(Self.ocaGame);
  for i := 0 to total - 1 do
    Self.tableBoardGrid.Cells[i mod 8, i div 8] := IntToStr(i + 1);

end;

procedure TForm1.diceThrowEvent(Sender: TObject);
var
  dice : integer;
begin
  dice := StrToIntDef(Self.diceEdit.Text, 0);
  dice := Random(5) + 1;
  Self.diceEdit.Text := IntToStr(dice);
  Self.processThrown;
end;

procedure TForm1.processMovement;
var
  current : integer;
  item    : tOcaCellInfo;
begin
  item := oca.game.getCurrentPlayerCellInfo(Self.ocaGame);
  case item.modifier of
    Goose     : MessageDlg('Has caido en un casillero de la oca, seguiras hasta la siguiente oca y tiras de nuevo', mtInformation, [mbOk], 0);
    Bridge    : MessageDlg('Has caido en un casillero puente, seguras trasladado al otro punte y tiras de nuevo', mtInformation, [mbOk], 0);
    Dice      : MessageDlg('Has caido en un casillero dado, tiras de nuevo', mtInformation, [mbOk], 0);
    Inn       : MessageDlg('Has caido en un casillero posada, pierdes 2 turnos', mtInformation, [mbOk], 0);
    Prison    : MessageDlg('Has caido en un casillero prision, pierdes 3 turnos', mtInformation, [mbOk], 0);
    Pit       : MessageDlg('Has caido en un casillero pozo, pierdes 4 turnos', mtInformation, [mbOk], 0);
    Labyrinth : MessageDlg('Has caido en un casillero laberinto, retrocedes al menos 12 casilleros hasta la siguiente posicion neutra', mtInformation, [mbOk], 0);
    Death     : MessageDlg('Has caido en un casillero muerte, vuelves a empezar', mtInformation, [mbOk], 0);
  end;
  if oca.game.currentPlayerWon(Self.ocaGame) then
    MessageDlg('Has ganado el juego de la oca!', mtInformation, [mbOk], 0)
  else
    begin
      oca.game.playerReactToCell(Self.ocaGame, oca.game.currentPlayer(Self.ocaGame));
      oca.game.nextPlayer(Self.ocaGame);
    end;
  Self.updateUI;
end;

procedure TForm1.processThrown;
var
  dice   : integer;
  current: integer;
begin
  dice    := StrToInt(Self.diceEdit.Text);
  current := oca.game.currentPlayer(Self.ocaGame);
  oca.game.movePlayer(Self.ocaGame, current, dice);
  Self.updateUI;
  Self.processMovement;
end;

procedure TForm1.renderTile(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  tileNumber : integer;
  i          : integer;
  tileInfo   : tOcaCellInfo;
begin
  tileNumber := StrToIntDef(Self.tableBoardGrid.Cells[ACol, ARow], 0);
  if Self.gameReady and (tileNumber <> 0) then begin
  OutputDebugString('init');
  OutputDebugString(PChar(IntToStr(DateUtils.MilliSecondOf(Now))));

  tileInfo   := oca.game.getCellInfo(Self.ocaGame, tileNumber);
  OutputDebugString(PChar(IntToStr(DateUtils.MilliSecondOf(Now))));
  with Self.tableBoardGrid.Canvas do
  begin
    //paint background
    case tileInfo.modifier of
      Goose     : Brush.Color := clYellow;
      Bridge    : Brush.Color := clSkyBlue;
      Dice      : Brush.Color := RGB(255, 180, 0);
      Inn       : Brush.Color := clLtGray;
      Prison    : Brush.Color := clGray;
      Pit       : Brush.Color := clDkGray;
      Labyrinth : Brush.Color := clLime;
      Death     : Brush.Color := clRed;
      else Brush.Color := clWhite;
    end;
    Rectangle(Rect);
    Brush.Style := bsClear;
    Brush.Color := clWhite;
    Font.Color := clBlack;
    Font.Name := 'Tahoma';
    Font.Size := 7;
    TextOut( Rect.Left + 5, Rect.Top + 5, IntToStr(tileNumber));
    for i := 1 to 4 do
      if tileInfo.players[i] then TextOut(Rect.Left + 5, (Rect.Bottom - 45) + 9*i, 'Jug. ' + IntToStr(i));
    OutputDebugString(PChar(IntToStr(DateUtils.MilliSecondOf(Now))));
    OutputDebugString('end.')
  end;
  end;//if
end;

end.
