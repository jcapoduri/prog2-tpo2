object Form1: TForm1
  Left = 496
  Top = 176
  Width = 1077
  Height = 560
  Caption = 'OCA'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label6: TLabel
    Left = 424
    Top = 272
    Width = 31
    Height = 13
    Caption = 'Label6'
  end
  object Label7: TLabel
    Left = 432
    Top = 280
    Width = 31
    Height = 13
    Caption = 'Label7'
  end
  object Label16: TLabel
    Left = 752
    Top = 16
    Width = 59
    Height = 13
    Caption = 'Movimientos'
  end
  object Label17: TLabel
    Left = 904
    Top = 16
    Width = 139
    Height = 13
    Caption = 'Movimientos de la repeticion:'
  end
  object tableBoardGrid: TStringGrid
    Left = 8
    Top = 16
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
    Width = 233
    Height = 81
    Caption = 'Inicio:'
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
      Width = 153
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
      Left = 152
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Iniciar Juego'
      TabOrder = 1
      OnClick = startButtonClick
    end
  end
  object ingameGroupBox: TGroupBox
    Left = 512
    Top = 96
    Width = 233
    Height = 217
    Caption = 'Juego'
    TabOrder = 2
    object Label2: TLabel
      Left = 8
      Top = 16
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
      Top = 16
      Width = 3
      Height = 13
    end
    object Label4: TLabel
      Left = 56
      Top = 32
      Width = 24
      Height = 13
      Caption = 'Info:'
    end
    object infoLabel: TLabel
      Left = 88
      Top = 32
      Width = 90
      Height = 13
      Caption = '                              '
    end
    object diceEdit: TEdit
      Left = 88
      Top = 56
      Width = 33
      Height = 21
      TabOrder = 0
    end
    object diceThrowButton: TButton
      Left = 144
      Top = 56
      Width = 75
      Height = 25
      Caption = 'Tirar'
      Enabled = False
      TabOrder = 1
      OnClick = diceThrowEvent
    end
    object toManualButton: TButton
      Left = 128
      Top = 96
      Width = 97
      Height = 25
      Caption = 'Modo Manual'
      Enabled = False
      TabOrder = 2
      OnClick = toManualButtonClick
    end
    object replyButton: TButton
      Left = 8
      Top = 136
      Width = 115
      Height = 25
      Caption = 'Replicar Juego actual'
      Enabled = False
      TabOrder = 3
      OnClick = replyButtonClick
    end
    object toNormalButton: TButton
      Left = 8
      Top = 96
      Width = 97
      Height = 25
      Caption = 'Modo Normal'
      Enabled = False
      TabOrder = 4
      OnClick = toNormalButtonClick
    end
    object replyModeButton: TButton
      Left = 8
      Top = 184
      Width = 113
      Height = 25
      Caption = 'Repetici'#243'n manual'
      Enabled = False
      TabOrder = 5
      OnClick = replyModeButtonClick
    end
    object stepButton: TButton
      Left = 128
      Top = 184
      Width = 97
      Height = 25
      Caption = 'Paso'
      Enabled = False
      TabOrder = 6
      OnClick = ReplyMovement
    end
  end
  object BitBtn1: TBitBtn
    Left = 664
    Top = 488
    Width = 75
    Height = 25
    Caption = '&Salir'
    TabOrder = 3
    Kind = bkClose
  end
  object movementsText: TMemo
    Left = 752
    Top = 32
    Width = 145
    Height = 481
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object mimicMovementsText: TMemo
    Left = 904
    Top = 32
    Width = 145
    Height = 479
    ScrollBars = ssVertical
    TabOrder = 5
  end
  object GroupBox1: TGroupBox
    Left = 512
    Top = 320
    Width = 233
    Height = 161
    Caption = 'Leyenda'
    TabOrder = 6
    object Shape1: TShape
      Left = 8
      Top = 16
      Width = 25
      Height = 25
      Brush.Color = clYellow
    end
    object Shape2: TShape
      Left = 8
      Top = 48
      Width = 25
      Height = 25
      Brush.Color = clSkyBlue
    end
    object Shape3: TShape
      Left = 8
      Top = 80
      Width = 25
      Height = 25
    end
    object Shape4: TShape
      Left = 8
      Top = 112
      Width = 25
      Height = 25
    end
    object Shape5: TShape
      Left = 120
      Top = 16
      Width = 25
      Height = 25
      Brush.Color = clGray
    end
    object Shape6: TShape
      Left = 120
      Top = 48
      Width = 25
      Height = 25
    end
    object Shape7: TShape
      Left = 120
      Top = 80
      Width = 25
      Height = 25
      Brush.Color = clLime
    end
    object Shape8: TShape
      Left = 120
      Top = 112
      Width = 25
      Height = 25
      Brush.Color = clRed
    end
    object Label8: TLabel
      Left = 35
      Top = 20
      Width = 78
      Height = 13
      Caption = 'Casilla de la Oca'
    end
    object Label9: TLabel
      Left = 37
      Top = 116
      Width = 68
      Height = 13
      Caption = 'Casilla Posada'
    end
    object Label10: TLabel
      Left = 153
      Top = 20
      Width = 64
      Height = 13
      Caption = 'Casilla Prision'
    end
    object Label11: TLabel
      Left = 34
      Top = 84
      Width = 63
      Height = 13
      Caption = 'Casilla Dados'
    end
    object Label12: TLabel
      Left = 153
      Top = 52
      Width = 56
      Height = 13
      Caption = 'Casilla Pozo'
    end
    object Label13: TLabel
      Left = 147
      Top = 84
      Width = 78
      Height = 13
      Caption = 'Casilla Laberinto'
    end
    object Label14: TLabel
      Left = 150
      Top = 116
      Width = 67
      Height = 13
      Caption = 'Casilla Muerte'
    end
    object Label15: TLabel
      Left = 38
      Top = 52
      Width = 67
      Height = 13
      Caption = 'Casilla Puente'
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 500
    OnTimer = ReplyMovement
    Left = 512
    Top = 488
  end
end
