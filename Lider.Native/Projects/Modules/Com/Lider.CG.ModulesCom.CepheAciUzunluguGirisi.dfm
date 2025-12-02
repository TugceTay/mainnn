object fmCepheAciUzunluguGirisi: TfmCepheAciUzunluguGirisi
  Left = 0
  Top = 0
  ActiveControl = ceCephe
  BorderStyle = bsDialog
  Caption = 'Cephe Uzunlu'#287'undan '#304'fraz'
  ClientHeight = 134
  ClientWidth = 333
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object cxLabel1: TcxLabel
    Left = 8
    Top = 8
    AutoSize = False
    Caption = 'Parsel Alan'#305':'
    ParentFont = False
    Transparent = True
    Height = 21
    Width = 115
  end
  object ceParselAlani: TcxCurrencyEdit
    Left = 124
    Top = 8
    TabStop = False
    ParentFont = False
    Properties.Alignment.Horz = taRightJustify
    Properties.DisplayFormat = ',0.00;-,0.00'
    Properties.EditFormat = ',0.00;-,0.00'
    Properties.ReadOnly = True
    Properties.Buttons = <>
    TabOrder = 1
    Width = 200
  end
  object cxLabel2: TcxLabel
    Left = 8
    Top = 30
    AutoSize = False
    Caption = 'Cephe Uzunlu'#287'u:'
    ParentFont = False
    Transparent = True
    Height = 21
    Width = 115
  end
  object ceCepheUzunlugu: TcxCurrencyEdit
    Left = 124
    Top = 30
    TabStop = False
    ParentFont = False
    Properties.Alignment.Horz = taRightJustify
    Properties.DisplayFormat = ',0.00;-,0.00'
    Properties.EditFormat = ',0.00;-,0.00'
    Properties.ReadOnly = True
    Properties.UseDisplayFormatWhenEditing = True
    Properties.Buttons = <>
    TabOrder = 3
    Width = 200
  end
  object cxLabel3: TcxLabel
    Left = 8
    Top = 52
    AutoSize = False
    Caption = #304'stenilen Cephe:'
    ParentFont = False
    Transparent = True
    Height = 21
    Width = 115
  end
  object btnTamam: TcxButton
    Left = 168
    Top = 101
    Width = 75
    Height = 25
    Caption = '&Tamam'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object cxButton1: TcxButton
    Left = 249
    Top = 101
    Width = 75
    Height = 25
    Cancel = True
    Caption = #304'p&tal'
    ModalResult = 2
    TabOrder = 6
  end
  object ceCephe: TcxCurrencyEdit
    Left = 124
    Top = 52
    ParentFont = False
    Properties.Alignment.Horz = taRightJustify
    Properties.DisplayFormat = ',0.00;-,0.00'
    Properties.EditFormat = ',0.00;-,0.00'
    Properties.UseDisplayFormatWhenEditing = True
    Properties.UseLeftAlignmentOnEditing = False
    Properties.UseThousandSeparator = True
    Properties.Buttons = <>
    TabOrder = 7
    Width = 200
  end
  object cxLabel4: TcxLabel
    Left = 8
    Top = 74
    AutoSize = False
    Caption = #304'stenilen A'#231#305' (Derece):'
    ParentFont = False
    Transparent = True
    Height = 21
    Width = 115
  end
  object ceAci: TcxCurrencyEdit
    Left = 124
    Top = 74
    EditValue = 90.000000000000000000
    ParentFont = False
    Properties.Alignment.Horz = taRightJustify
    Properties.DisplayFormat = ',0.00;-,0.00'
    Properties.EditFormat = ',0.00;-,0.00'
    Properties.UseDisplayFormatWhenEditing = True
    Properties.UseLeftAlignmentOnEditing = False
    Properties.UseThousandSeparator = True
    Properties.Buttons = <>
    TabOrder = 9
    Width = 200
  end
end
