unit mainOCA;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids,
  oca.game, StdCtrls;

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
    quitButton: TButton;
    Button3: TButton;
    procedure startButtonClick(Sender: TObject);
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

procedure TForm1.startButtonClick(Sender: TObject);
begin
  Self.gameGroupBox.Enabled   := false;
  Self.ingameGroupBox.Enabled := false;
end;

end.
