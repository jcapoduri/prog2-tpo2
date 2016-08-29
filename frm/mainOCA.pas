unit mainOCA;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids,
  oca.game, oca.modifiers, Buttons, StdCtrls, typinfo,
  ExtCtrls;

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
    BitBtn1: TBitBtn;
    playerLabel: TLabel;
    diceEdit: TEdit;
    diceThrowButton: TButton;
    Label4: TLabel;
    infoLabel: TLabel;
    toManualButton: TButton;
    Label6: TLabel;
    Label7: TLabel;
    Timer1: TTimer;
    replyButton: TButton;
    movementsText: TMemo;
    mimicMovementsText: TMemo;
    Label16: TLabel;
    Label17: TLabel;
    toNormalButton: TButton;
    GroupBox1: TGroupBox;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Shape7: TShape;
    Shape8: TShape;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    replyModeButton: TButton;
    stepButton: TButton;
    procedure startButtonClick(Sender: TObject);
    procedure diceThrowEvent(Sender: TObject);
    procedure renderTile(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure toManualButtonClick(Sender: TObject);
    procedure toNormalButtonClick(Sender: TObject);
    procedure replyButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ReplyMovement(Sender: TObject);
    procedure replyModeButtonClick(Sender: TObject);
  protected
    ocaGame   : oca.game.tOcaGame;
    gameReady : boolean;
    justPaint : boolean;
    procedure updateUI();
    procedure renderMatrix();
    procedure processThrown();
    procedure processMovement();
    // UI management
    procedure setUiForStartGame();
    procedure setUiForInGame();
    procedure setUiForEndGame();
    procedure setUiForReplyGame();
    procedure setUiForNormalGame();
    procedure setUiForManualGame();
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
  Self.justPaint := false;
  Self.setUiForStartGame;
end;

procedure TForm1.updateUI;
begin
  Self.playerLabel.Caption := IntToStr(oca.game.currentPlayer(Self.ocaGame));
  //Self.Label5.Caption      := 'Jugador Actual:' + IntToStr(oca.game.currentPlayer(self.ocaGame));
  Self.renderMatrix();
end;

procedure TForm1.startButtonClick(Sender: TObject);
var
  players: Integer;
begin
  Self.gameReady := false;
  Self.playersComboBox.Enabled := false;
  Self.startButton.Enabled     := false;
  players := 2;
  if (Self.playersComboBox.Text = '3 Jugadores') then players := 3;
  if (Self.playersComboBox.Text = '4 Jugadores') then players := 4;
  oca.game.create(Self.ocaGame);
  oca.game.generate(Self.ocaGame);
  setupGame(Self.ocaGame, players);
  Self.gameReady := true;
  Self.setUiForInGame;
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
  if Self.toManualButton.Enabled then //normal mode activated
    begin
      dice := StrToIntDef(Self.diceEdit.Text, 0);
      dice := Random(6) + 1;
      Self.diceEdit.Text := IntToStr(dice);
    end;
  Self.processThrown;
end;

procedure TForm1.processMovement;
var
  current : integer;
  item    : tOcaCellInfo;
begin
  item := oca.game.getCurrentPlayerCellInfo(Self.ocaGame);
  Self.movementsText.Lines.Append('Jugador ' + Self.playerLabel.Caption + ' tirada: ' + Self.diceEdit.Text + ' casillero: ' + inttostr(item.cellNmb));
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
  if item.modifier <> None then
    Self.movementsText.Lines.Append('Cayo en casilla ' + GetEnumName(TypeInfo(tModifiers), Ord(item.modifier)));
  if oca.game.currentPlayerWon(Self.ocaGame) then
   begin
    MessageDlg('Has ganado el juego de la oca!', mtInformation, [mbOk], 0);
    Self.setUiForEndGame;
   end
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
  dice    := StrToIntDef(Self.diceEdit.Text, 0);
  if dice > 0 then
    begin
      current := oca.game.currentPlayer(Self.ocaGame);
      oca.game.movePlayer(Self.ocaGame, current, dice);
      Self.updateUI;
      Self.processMovement;
    end;
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

procedure TForm1.toManualButtonClick(Sender: TObject);
begin
  Self.setUiForManualGame;
end;

procedure TForm1.toNormalButtonClick(Sender: TObject);
begin
  Self.setUiForNormalGame;
end;

procedure TForm1.replyButtonClick(Sender: TObject);
var
  players:integer;

begin

  if self.playersComboBox.Text='2 Jugadores' then players:=2;
  if self.playersComboBox.Text='3 Jugadores' then players:=3;
  if self.playersComboBox.Text='4 Jugadores' then players:=4;

  setupGame(self.ocaGame,players);

  timer1.Enabled:=true;

  Self.setUiForReplyGame;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 shape3.Brush.Color:= RGB(255, 180, 0);
 shape6.Brush.Color:= clDkGray;
 shape4.Brush.Color:= clLtGray;
end;


procedure TForm1.ReplyMovement(Sender: TObject);
var 
  movement, player : integer;
  item    : tOcaCellInfo;
begin  
  movement := oca.game.nextMovement(Self.ocaGame, player);
  if not Self.justPaint then
    if player = 0{not(oca.game.ReplyGame(self.ocaGame))} then
      begin
        timer1.Enabled:=false;
        Self.setUiForStartGame;
      end
    else
      begin
        UpdateUi;
        oca.game.ReplyGame(self.ocaGame);
        item := oca.game.getCurrentPlayerCellInfo(Self.ocaGame);
        Self.mimicMovementsText.Lines.Append('Jugador ' + IntToStr(player) + ' tirada: ' +IntToStr(movement) + ' casillero: ' + inttostr(item.cellNmb));
      end
  else
    begin
      UpdateUi;
      oca.game.playerReactToCell(Self.ocaGame, oca.game.currentPlayer(Self.ocaGame));
      item := oca.game.getCurrentPlayerCellInfo(Self.ocaGame);
      if item.modifier <> None then
        Self.mimicMovementsText.Lines.Append('Cayo en casilla ' + GetEnumName(TypeInfo(tModifiers), Ord(item.modifier)));
      oca.game.nextPlayer(Self.ocaGame);
    end;
  Self.justPaint := not Self.justPaint;


end;

procedure TForm1.setUiForEndGame;
begin
  Self.startButton.Enabled     := false;
  Self.diceThrowButton.Enabled := false;
  Self.toManualButton.Enabled  := false;
  Self.toNormalButton.Enabled  := false;
  Self.replyButton.Enabled     := true;
  Self.stepButton.Enabled      := false;
  Self.replyModeButton.Enabled := false;
  Self.playersComboBox.Enabled := false;

end;

procedure TForm1.setUiForInGame;
begin
  Self.startButton.Enabled     := false;
  Self.diceThrowButton.Enabled := true;
  Self.replyButton.Enabled     := false;
  Self.playersComboBox.Enabled := false;
  Self.stepButton.Enabled      := false;
  Self.replyModeButton.Enabled := false;
  Self.setUiForNormalGame;
end;

procedure TForm1.setUiForManualGame;
begin
  Self.toManualButton.Enabled  := false;
  Self.toNormalButton.Enabled  := true;
end;

procedure TForm1.setUiForNormalGame;
begin
  Self.toManualButton.Enabled  := true;
  Self.toNormalButton.Enabled  := false;
end;

procedure TForm1.setUiForReplyGame;
begin
  Self.startButton.Enabled     := false;
  Self.diceThrowButton.Enabled := false;
  Self.toManualButton.Enabled  := false;
  Self.toNormalButton.Enabled  := false;
  Self.replyButton.Enabled     := false;
  Self.playersComboBox.Enabled := false;
  Self.stepButton.Enabled      := false;
  Self.replyModeButton.Enabled := true;
end;

procedure TForm1.setUiForStartGame;
begin
  Self.startButton.Enabled     := true;
  Self.diceThrowButton.Enabled := false;
  Self.replyButton.Enabled     := false;
  Self.playersComboBox.Enabled := true;
  Self.setUiForNormalGame;
  Self.stepButton.Enabled      := false;
  Self.replyModeButton.Enabled := false;
end;

procedure TForm1.replyModeButtonClick(Sender: TObject);
begin
  Self.Timer1.enabled := not Self.Timer1.enabled;
  if Self.Timer1.enabled then
    Self.replyModeButton.Caption := 'Ir a repetición manual'
  else
    Self.replyModeButton.Caption := 'Ir a repetición automatica';
  Self.stepButton.enabled := not Self.Timer1.enabled;
end;

end.
