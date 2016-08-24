object Form1: TForm1
  Left = 448
  Top = 163
  Width = 760
  Height = 552
  Caption = 'OCA'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object tableBoardGrid: TStringGrid
    Left = 8
    Top = 8
    Width = 497
    Height = 497
    ColCount = 8
    DefaultColWidth = 60
    DefaultRowHeight = 60
    FixedCols = 0
    RowCount = 8
    FixedRows = 0
    ScrollBars = ssNone
    TabOrder = 0
    OnDrawCell = renderTile
  end
  object gameGroupBox: TGroupBox
    Left = 512
    Top = 8
    Width = 225
    Height = 161
    Caption = 'Juego'
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 24
      Width = 54
      Height = 13
      Caption = 'Jugadores:'
    end
    object playersComboBox: TComboBox
      Left = 72
      Top = 24
      Width = 145
      Height = 21
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = '2 Jugadores'
      Items.Strings = (
        '2 Jugadores'
        '3 Jugadores'
        '4 Jugadores')
    end
    object startButton: TButton
      Left = 144
      Top = 128
      Width = 75
      Height = 25
      Caption = 'Iniciar Juego'
      TabOrder = 1
      OnClick = startButtonClick
    end
  end
  object ingameGroupBox: TGroupBox
    Left = 512
    Top = 176
    Width = 225
    Height = 297
    Caption = 'In game'
    Enabled = False
    TabOrder = 2
    object Label2: TLabel
      Left = 8
      Top = 24
      Width = 76
      Height = 13
      Caption = 'Jugador Actual:'
    end
    object Label3: TLabel
      Left = 48
      Top = 56
      Width = 34
      Height = 13
      Caption = 'Tirada:'
    end
    object playerLabel: TLabel
      Left = 88
      Top = 24
      Width = 3
      Height = 13
    end
    object Label4: TLabel
      Left = 56
      Top = 96
      Width = 24
      Height = 13
      Caption = 'Info:'
    end
    object infoLabel: TLabel
      Left = 88
      Top = 96
      Width = 90
      Height = 13
      Caption = '                              '
    end
    object Button3: TButton
      Left = 104
      Top = 264
      Width = 115
      Height = 25
      Caption = 'Replicar Juego actual'
      TabOrder = 0
    end
    object diceEdit: TEdit
      Left = 88
      Top = 56
      Width = 33
      Height = 21
      TabOrder = 1
    end
    object diceThrowButton: TButton
      Left = 136
      Top = 56
      Width = 75
      Height = 25
      Caption = 'Tirar'
      TabOrder = 2
      OnClick = diceThrowEvent
    end
    object nextTurnButton: TButton
      Left = 88
      Top = 160
      Width = 121
      Height = 25
      Caption = 'Sigiente Turno'
      TabOrder = 3
    end
  end
  object BitBtn1: TBitBtn
    Left = 656
    Top = 480
    Width = 75
    Height = 25
    Caption = '&Salir'
    TabOrder = 3
    Kind = bkClose
  end
end
