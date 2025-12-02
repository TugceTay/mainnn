object fmOleFileDef: TfmOleFileDef
  Left = 630
  Top = 235
  BorderIcons = [biSystemMenu]
  Caption = 'Dosya Ekle'
  ClientHeight = 215
  ClientWidth = 368
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object Image1: TImage
    Left = 81
    Top = 0
    Width = 287
    Height = 215
    Align = alClient
    AutoSize = True
    ExplicitWidth = 295
    ExplicitHeight = 223
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 81
    Height = 215
    Align = alLeft
    BevelInner = bvLowered
    TabOrder = 0
    object btnLoadFile: TButton
      Left = 6
      Top = 6
      Width = 69
      Height = 24
      Caption = 'Y'#252'kle'
      TabOrder = 0
      OnClick = btnLoadFileClick
    end
    object Button2: TButton
      Left = 6
      Top = 55
      Width = 69
      Height = 24
      Caption = 'Temizle'
      TabOrder = 1
      OnClick = Button2Click
    end
    object OKBtn: TButton
      Left = 6
      Top = 123
      Width = 69
      Height = 24
      Caption = 'Tamam'
      Default = True
      ModalResult = 1
      TabOrder = 2
      OnClick = OKBtnClick
    end
    object CancelBtn: TButton
      Left = 6
      Top = 147
      Width = 69
      Height = 24
      Cancel = True
      Caption = #304'ptal'
      ModalResult = 2
      TabOrder = 3
    end
    object btnShow: TButton
      Left = 6
      Top = 31
      Width = 69
      Height = 24
      Caption = 'G'#246'r'#252'nt'#252'le'
      TabOrder = 4
      OnClick = btnShowClick
    end
    object btnSaveChanges: TButton
      Left = 6
      Top = 79
      Width = 69
      Height = 24
      Caption = 'Sakla'
      TabOrder = 5
      OnClick = btnSaveChangesClick
    end
  end
end
