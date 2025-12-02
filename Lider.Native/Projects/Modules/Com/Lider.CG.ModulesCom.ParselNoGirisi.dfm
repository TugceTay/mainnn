object fmParselNoGirisi: TfmParselNoGirisi
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Yeni Parsel No Giri'#351'i'
  ClientHeight = 68
  ClientWidth = 297
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
    Caption = 'Yeni Parsel No:'
    ParentFont = False
    Transparent = True
    Height = 21
    Width = 75
  end
  object teParselNo: TcxTextEdit
    Left = 90
    Top = 8
    ParentFont = False
    Properties.Buttons = <>
    TabOrder = 1
    Width = 200
  end
  object btnTamam: TcxButton
    Left = 134
    Top = 36
    Width = 75
    Height = 25
    Caption = '&Tamam'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object cxButton1: TcxButton
    Left = 215
    Top = 36
    Width = 75
    Height = 25
    Cancel = True
    Caption = #304'p&tal'
    ModalResult = 2
    TabOrder = 3
  end
end
