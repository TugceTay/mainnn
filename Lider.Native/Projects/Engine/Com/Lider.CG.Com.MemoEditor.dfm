object fmMemoEditor: TfmMemoEditor
  Left = 433
  Top = 191
  Caption = 'Metin Edit'#246'r'#252
  ClientHeight = 362
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Bevel1: TBevel
    Left = 0
    Top = 23
    Width = 426
    Height = 305
    Align = alClient
    Shape = bsFrame
  end
  object Memo1: TMemo
    Left = 0
    Top = 23
    Width = 426
    Height = 305
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    OnChange = Memo1Change
  end
  object Panel1: TPanel
    Left = 0
    Top = 328
    Width = 426
    Height = 34
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    DesignSize = (
      426
      34)
    object OKBtn: TButton
      Left = 268
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akRight]
      Caption = 'Tamam'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object CancelBtn: TButton
      Left = 344
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akRight]
      Cancel = True
      Caption = #304'ptal'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 426
    Height = 23
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 3
      Width = 34
      Height = 15
      Caption = 'Label1'
    end
  end
end
