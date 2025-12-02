object fmGoogleTileSettings: TfmGoogleTileSettings
  Left = 315
  Top = 174
  BorderStyle = bsDialog
  Caption = 'Tile Ayarlar'#305
  ClientHeight = 176
  ClientWidth = 399
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  DesignSize = (
    399
    176)
  TextHeight = 15
  object cxButtonEdit1: TcxButtonEdit
    Left = 40
    Top = 32
    Properties.Buttons = <
      item
        Default = True
        Kind = bkEllipsis
      end>
    Properties.OnButtonClick = cxButtonEdit1PropertiesButtonClick
    Properties.OnEditValueChanged = cxButtonEdit1PropertiesEditValueChanged
    TabOrder = 0
    Width = 321
  end
  object cxLabel1: TcxLabel
    Left = 16
    Top = 8
    Caption = 'Klas'#246'r Se'#231':'
  end
  object cxSpinEdit1: TcxSpinEdit
    Left = 80
    Top = 69
    Properties.OnChange = cxSpinEdit1PropertiesChange
    TabOrder = 2
    Width = 57
  end
  object cxSpinEdit2: TcxSpinEdit
    Left = 256
    Top = 70
    Properties.OnChange = cxSpinEdit2PropertiesChange
    TabOrder = 3
    Width = 57
  end
  object cxLabel2: TcxLabel
    Left = 16
    Top = 72
    Caption = 'Min.Level : '
  end
  object cxLabel3: TcxLabel
    Left = 192
    Top = 72
    Caption = 'Max.Level:'
  end
  object cxButton1: TcxButton
    Left = 183
    Top = 117
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Tamam'
    LookAndFeel.SkinName = ''
    ModalResult = 1
    TabOrder = 6
  end
  object cxButton2: TcxButton
    Left = 278
    Top = 117
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #304'ptal'
    LookAndFeel.SkinName = ''
    ModalResult = 2
    TabOrder = 7
  end
  object cxButton3: TcxButton
    Left = 9
    Top = 118
    Width = 121
    Height = 25
    Caption = 'S'#305'n'#305'r Koordinatlar'#305
    TabOrder = 8
    Visible = False
    OnClick = cxButton3Click
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoOverWritePrompt, fdoPickFolders]
    Left = 144
    Top = 72
  end
end
