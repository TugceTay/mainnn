object fmGenSingleInput: TfmGenSingleInput
  Left = 394
  Top = 311
  BorderStyle = bsToolWindow
  ClientHeight = 77
  ClientWidth = 180
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 96
  DesignSize = (
    180
    77)
  TextHeight = 15
  object edtGeneral: TLabeledEdit
    Left = 15
    Top = 21
    Width = 154
    Height = 23
    CharCase = ecUpperCase
    EditLabel.Width = 106
    EditLabel.Height = 15
    EditLabel.Caption = '&'#304#351'aretleme Karakteri:'
    TabOrder = 0
    Text = ''
  end
  object btnOK: TBitBtn
    Left = 40
    Top = 45
    Width = 64
    Height = 25
    Anchors = [akRight]
    Caption = 'Tamam'
    Default = True
    ModalResult = 1
    NumGlyphs = 2
    TabOrder = 1
  end
  object btnCancel: TBitBtn
    Left = 105
    Top = 45
    Width = 64
    Height = 25
    Anchors = [akRight]
    Cancel = True
    Caption = #304'ptal'
    ModalResult = 2
    NumGlyphs = 2
    TabOrder = 2
  end
end
