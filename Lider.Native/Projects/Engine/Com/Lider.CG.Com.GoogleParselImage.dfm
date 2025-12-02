object fmGoogleParselImg: TfmGoogleParselImg
  Left = 224
  Top = 129
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Google '#304'maj '
  ClientHeight = 459
  ClientWidth = 684
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  WindowState = wsMaximized
  PixelsPerInch = 96
  TextHeight = 15
  object Image1: TImage
    Left = 0
    Top = 38
    Width = 684
    Height = 421
    Align = alClient
    ExplicitWidth = 905
    ExplicitHeight = 651
  end
  object cxGroupBox1: TcxGroupBox
    Left = 0
    Top = 0
    Align = alTop
    PanelStyle.Active = True
    Style.LookAndFeel.SkinName = ''
    StyleDisabled.LookAndFeel.SkinName = ''
    TabOrder = 0
    DesignSize = (
      684
      38)
    Height = 38
    Width = 684
    object cxButton1: TcxButton
      Left = 514
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Tamam'
      LookAndFeel.SkinName = ''
      ModalResult = 1
      TabOrder = 0
    end
    object cxButton2: TcxButton
      Left = 600
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = #304'ptal'
      LookAndFeel.SkinName = ''
      ModalResult = 2
      TabOrder = 1
    end
    object cxButton3: TcxButton
      Left = 7
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Kaydet'
      LookAndFeel.SkinName = ''
      TabOrder = 2
      OnClick = cxButton3Click
    end
  end
end
