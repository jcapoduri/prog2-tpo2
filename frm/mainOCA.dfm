object Form1: TForm1
  Left = 407
  Top = 129
  Width = 919
  Height = 558
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
  object Shape1: TShape
    Left = 752
    Top = 24
    Width = 49
    Height = 49
  end
  object Shape2: TShape
    Left = 752
    Top = 360
    Width = 49
    Height = 49
  end
  object Shape3: TShape
    Left = 752
    Top = 416
    Width = 49
    Height = 49
  end
  object Shape4: TShape
    Left = 752
    Top = 80
    Width = 49
    Height = 49
  end
  object Shape5: TShape
    Left = 752
    Top = 136
    Width = 49
    Height = 49
  end
  object Shape6: TShape
    Left = 752
    Top = 192
    Width = 49
    Height = 49
  end
  object Shape7: TShape
    Left = 752
    Top = 248
    Width = 49
    Height = 49
  end
  object Shape8: TShape
    Left = 752
    Top = 304
    Width = 49
    Height = 49
  end
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
  object Label8: TLabel
    Left = 816
    Top = 24
    Width = 3
    Height = 13
  end
  object Label9: TLabel
    Left = 814
    Top = 80
    Width = 3
    Height = 13
  end
  object Label10: TLabel
    Left = 808
    Top = 144
    Width = 3
    Height = 13
  end
  object Label11: TLabel
    Left = 808
    Top = 200
    Width = 3
    Height = 13
  end
  object Label12: TLabel
    Left = 808
    Top = 256
    Width = 3
    Height = 13
  end
  object Label13: TLabel
    Left = 808
    Top = 312
    Width = 3
    Height = 13
  end
  object Label14: TLabel
    Left = 808
    Top = 368
    Width = 3
    Height = 13
  end
  object Label15: TLabel
    Left = 808
    Top = 424
    Width = 3
    Height = 13
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
    Width = 233
    Height = 201
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
      Top = 64
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
      Left = 112
      Top = 160
      Width = 115
      Height = 25
      Caption = 'Replicar Juego actual'
      TabOrder = 0
      OnClick = Button3Click
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
    object Button1: TButton
      Left = 112
      Top = 128
      Width = 113
      Height = 25
      Caption = 'Ir a Modo Manual'
      TabOrder = 3
      OnClick = Button1Click
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
  object GroupBox1: TGroupBox
    Left = 512
    Top = 384
    Width = 233
    Height = 97
    Caption = 'Modo Manual'
    TabOrder = 4
    object Label5: TLabel
      Left = 8
      Top = 24
      Width = 76
      Height = 13
      Caption = 'Jugador Actual:'
    end
    object Button2: TButton
      Left = 112
      Top = 64
      Width = 113
      Height = 25
      Caption = 'Volver a Modo Normal'
      TabOrder = 0
      OnClick = Button2Click
    end
    object Button4: TButton
      Left = 168
      Top = 16
      Width = 49
      Height = 25
      Caption = 'Tirar'
      TabOrder = 1
      OnClick = Button4Click
    end
    object Edit1: TEdit
      Left = 104
      Top = 16
      Width = 49
      Height = 21
      TabOrder = 2
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 500
    OnTimer = ReplyMovement
    Left = 760
    Top = 480
  end
end
