object fmSelectCBox: TfmSelectCBox
  Left = 455
  Top = 269
  Caption = 'fmSelectCBox'
  ClientHeight = 107
  ClientWidth = 384
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 15
  object cxGroupBox1: TcxGroupBox
    Left = 0
    Top = 0
    Align = alClient
    PanelStyle.Active = True
    TabOrder = 0
    DesignSize = (
      384
      107)
    Height = 107
    Width = 384
    object cxComboBox1: TcxComboBox
      Left = 24
      Top = 16
      Anchors = [akLeft, akTop, akRight]
      Properties.DropDownListStyle = lsFixedList
      Properties.Buttons = <
        item
        end>
      TabOrder = 0
      Width = 345
    end
    object cxButton2: TcxButton
      Left = 285
      Top = 61
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = #304'ptal'
      ModalResult = 2
      TabOrder = 1
    end
    object cxButton1: TcxButton
      Left = 182
      Top = 61
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Tamam'
      ModalResult = 1
      TabOrder = 2
    end
  end
end
