object fmTextFileProperties: TfmTextFileProperties
  Left = 494
  Top = 258
  Caption = 'Metin Dosyas'#305' Ayarlar'#305
  ClientHeight = 101
  ClientWidth = 236
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  PixelsPerInch = 96
  DesignSize = (
    236
    101)
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 116
    Height = 15
    Caption = 'Kolon Ay'#305'ra'#231' Karakteri'
  end
  object cbDelimiterChar: TcxComboBox
    Left = 8
    Top = 32
    Properties.Items.Strings = (
      'TAB'
      'BO'#350'LUK'
      '"'
      ','
      ';'
      '|'
      ':')
    Properties.Buttons = <
      item
      end>
    TabOrder = 0
    Text = 'TAB'
    Width = 217
  end
  object btnOK: TcxButton
    Left = 77
    Top = 68
    Width = 75
    Height = 25
    Anchors = [akRight]
    Caption = 'Tamam'
    LookAndFeel.Kind = lfStandard
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TcxButton
    Left = 153
    Top = 68
    Width = 75
    Height = 25
    Anchors = [akRight]
    Caption = #304'ptal'
    LookAndFeel.Kind = lfStandard
    ModalResult = 2
    TabOrder = 2
  end
end
