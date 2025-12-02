object fmEasyThematic: TfmEasyThematic
  Left = 310
  Top = 171
  BorderStyle = bsDialog
  Caption = 'Kolay Tematik Olu'#351'tur'
  ClientHeight = 256
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnClose = FormClose
  TextHeight = 15
  object Bevel2: TBevel
    Left = 18
    Top = 173
    Width = 287
    Height = 49
    Visible = False
  end
  object Label1: TLabel
    Left = 46
    Top = 176
    Width = 125
    Height = 15
    Caption = 'Temeatik Renk Aral'#305'klar'#305
    Visible = False
  end
  object Label2: TLabel
    Left = 21
    Top = 191
    Width = 50
    Height = 15
    Caption = 'Ba'#351'lang'#305#231
    Visible = False
  end
  object Label10: TLabel
    Left = 268
    Top = 177
    Width = 24
    Height = 15
    Caption = 'Art'#305#351
    Visible = False
  end
  object Label11: TLabel
    Left = 8
    Top = 10
    Width = 86
    Height = 15
    Caption = 'Tabaka Kolonlar'#305
  end
  object Label12: TLabel
    Left = 20
    Top = 203
    Width = 30
    Height = 15
    Caption = 'Rengi'
    Visible = False
  end
  object Label3: TLabel
    Left = 161
    Top = 191
    Width = 22
    Height = 15
    Caption = 'Biti'#351
    Visible = False
  end
  object Label13: TLabel
    Left = 159
    Top = 203
    Width = 30
    Height = 15
    Caption = 'Rengi'
    Visible = False
  end
  object Label4: TLabel
    Left = 234
    Top = 141
    Width = 96
    Height = 15
    Caption = 'De'#287'erlerini Yoksay'
    Visible = False
  end
  object CLStart: TdxColorEdit
    Left = 72
    Top = 193
    Properties.ColorPalette = cpAll
    Properties.Buttons = <
      item
      end>
    TabOrder = 5
    Width = 69
  end
  object CLEnd: TdxColorEdit
    Left = 197
    Top = 193
    Properties.ColorPalette = cpAll
    Properties.Buttons = <
      item
      end>
    TabOrder = 6
    Width = 69
  end
  object cmdSearchCompXY_0: TButton
    Left = -6240
    Top = 244
    Width = 88
    Height = 20
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
  end
  object MERange: TMaskEdit
    Left = 271
    Top = 194
    Width = 20
    Height = 23
    TabOrder = 2
    Text = '10'
    Visible = False
  end
  object UpDown5: TUpDown
    Left = 291
    Top = 194
    Width = 16
    Height = 23
    Associate = MERange
    Max = 20
    Position = 10
    TabOrder = 4
    Visible = False
  end
  object CBZero: TCheckBox
    Left = 235
    Top = 120
    Width = 71
    Height = 16
    Caption = 'S'#305'f'#305'r'
    TabOrder = 1
    Visible = False
  end
  object cmdDraw: TButton
    Left = 31
    Top = 228
    Width = 130
    Height = 21
    Caption = 'Tematik Olu'#351'tur'
    Enabled = False
    ModalResult = 1
    TabOrder = 3
    OnClick = cmdDrawClick
  end
  object CBfields: TListBox
    Left = 32
    Top = 32
    Width = 257
    Height = 129
    ItemHeight = 15
    TabOrder = 7
    OnClick = CBfieldsClick
  end
  object btnClose: TButton
    Left = 205
    Top = 227
    Width = 86
    Height = 22
    Cancel = True
    Caption = '&Kapat'
    ModalResult = 2
    TabOrder = 8
    OnClick = btnCloseClick
  end
end
