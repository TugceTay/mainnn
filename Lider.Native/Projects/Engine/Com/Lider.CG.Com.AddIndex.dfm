object fmAddIndex: TfmAddIndex
  Left = 317
  Top = 124
  HelpContext = 640
  BorderStyle = bsDialog
  Caption = 'Indeks Ekle'
  ClientHeight = 267
  ClientWidth = 390
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 3
    Top = 6
    Width = 65
    Height = 13
    Caption = '&Kolon Listesi :'
    FocusControl = List2
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 6
    Top = 183
    Width = 30
    Height = 13
    Caption = '&'#304'fade :'
    FocusControl = Edit2
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 6
    Top = 223
    Width = 30
    Height = 13
    Caption = '&Kriter :'
    FocusControl = Edit3
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object OKBtn: TButton
    Left = 302
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Tamam'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 302
    Top = 38
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Vazge'#231
    ModalResult = 2
    TabOrder = 1
  end
  object List2: TListBox
    Left = 6
    Top = 24
    Width = 160
    Height = 151
    ItemHeight = 15
    TabOrder = 2
    OnClick = List2Click
  end
  object GroupBox1: TGroupBox
    Left = 171
    Top = 99
    Width = 206
    Height = 76
    Caption = '&Ayarlar'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    object Check1: TCheckBox
      Left = 9
      Top = 18
      Width = 154
      Height = 17
      Caption = '&Tek'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object Check2: TCheckBox
      Left = 9
      Top = 45
      Width = 151
      Height = 17
      Caption = '&Azalan S'#305'rada'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
  end
  object Edit2: TEdit
    Left = 6
    Top = 198
    Width = 375
    Height = 23
    TabOrder = 4
  end
  object Edit3: TEdit
    Left = 6
    Top = 238
    Width = 375
    Height = 23
    TabOrder = 5
  end
end
