object fmConnDialog: TfmConnDialog
  Left = 453
  Top = 226
  BorderStyle = bsDialog
  Caption = 'Veritaban'#305' Ba'#287'lant'#305' Ayarlar'#305
  ClientHeight = 343
  ClientWidth = 320
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 31
    Top = 152
    Width = 45
    Height = 15
    Alignment = taRightJustify
    Caption = 'Kullan'#305'c'#305
  end
  object Label2: TLabel
    Left = 53
    Top = 176
    Width = 23
    Height = 15
    Alignment = taRightJustify
    Caption = #350'ifre'
  end
  object Label3: TLabel
    Left = 24
    Top = 208
    Width = 52
    Height = 15
    Alignment = taRightJustify
    Caption = 'Veritaban'#305
  end
  object Label4: TLabel
    Left = 36
    Top = 232
    Width = 40
    Height = 15
    Alignment = taRightJustify
    Caption = 'Sunucu'
  end
  object Label5: TLabel
    Left = 54
    Top = 256
    Width = 22
    Height = 15
    Alignment = taRightJustify
    Caption = 'Port'
  end
  object dxBevel1: TdxBevel
    Left = 16
    Top = 136
    Width = 289
    Height = 57
  end
  object dxBevel2: TdxBevel
    Left = 16
    Top = 195
    Width = 289
    Height = 81
  end
  object dxBevel3: TdxBevel
    Left = 1
    Top = 1
    Width = 317
    Height = 321
  end
  object Editus: TEdit
    Left = 87
    Top = 144
    Width = 217
    Height = 23
    TabOrder = 0
  end
  object Editpa: TEdit
    Left = 87
    Top = 168
    Width = 217
    Height = 23
    TabOrder = 1
  end
  object Editdb: TEdit
    Left = 87
    Top = 200
    Width = 217
    Height = 23
    TabOrder = 2
  end
  object Editse: TEdit
    Left = 87
    Top = 224
    Width = 217
    Height = 23
    TabOrder = 3
  end
  object Editpo: TEdit
    Left = 87
    Top = 248
    Width = 217
    Height = 23
    TabOrder = 4
  end
  object Button1: TButton
    Left = 184
    Top = 288
    Width = 91
    Height = 25
    Caption = 'Ba'#287'lan'
    ModalResult = 1
    TabOrder = 5
  end
  object Button2: TButton
    Left = 80
    Top = 288
    Width = 75
    Height = 25
    Caption = #304'ptal'
    ModalResult = 2
    TabOrder = 6
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 324
    Width = 320
    Height = 19
    Panels = <
      item
        Width = 250
      end>
    SimplePanel = True
  end
  object cxGroupBox1: TcxGroupBox
    Left = 16
    Top = 8
    Caption = 'Ba'#287'lant'#305' Listesi-> Sunucu/Veritaban'#305'/Kullan'#305'c'#305'/'#350'ifre/Port'
    TabOrder = 8
    Height = 121
    Width = 289
    object cxListBox1: TcxListBox
      Left = 3
      Top = 18
      Width = 282
      Height = 71
      ItemHeight = 15
      TabOrder = 0
      OnClick = cxListBox1Click
    end
    object Button3: TButton
      Left = 204
      Top = 93
      Width = 75
      Height = 21
      Caption = 'Temizle'
      TabOrder = 1
      OnClick = Button3Click
    end
  end
end
