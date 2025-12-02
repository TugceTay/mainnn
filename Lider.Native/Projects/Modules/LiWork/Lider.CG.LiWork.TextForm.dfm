object fmTextForm: TfmTextForm
  Left = 0
  Top = 0
  Caption = 'fmTextForm'
  ClientHeight = 168
  ClientWidth = 352
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object pnlOrta: TPanel
    Left = 0
    Top = 46
    Width = 352
    Height = 82
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object chkAci: TcxCheckBox
      Left = 8
      Top = 55
      Caption = 'A'#231#305'y'#305' metinden al'
      ParentBackground = False
      ParentColor = False
      State = cbsChecked
      Style.Color = clNone
      Style.TransparentBorder = False
      TabOrder = 0
      Transparent = True
    end
    object cxLabel1: TcxLabel
      Left = 8
      Top = 9
      Caption = 'Metin:'
      Transparent = True
    end
    object teMetin: TcxTextEdit
      Left = 182
      Top = 8
      Properties.Alignment.Horz = taRightJustify
      Properties.Buttons = <>
      TabOrder = 2
      Width = 156
    end
    object cxLabel4: TcxLabel
      Left = 8
      Top = 33
      Caption = 'Boyut:'
      Transparent = True
    end
    object ceBoyut: TcxCurrencyEdit
      Left = 182
      Top = 32
      EditValue = 5.000000000000000000
      Properties.Alignment.Horz = taRightJustify
      Properties.DecimalPlaces = 0
      Properties.DisplayFormat = '0'
      Properties.Nullable = False
      Properties.Nullstring = '1'
      Properties.UseNullString = True
      Properties.OnChange = ceBoyutPropertiesChange
      Properties.Buttons = <>
      TabOrder = 4
      Width = 156
    end
  end
  object pnlUst: TPanel
    Left = 0
    Top = 0
    Width = 352
    Height = 46
    Align = alTop
    BevelOuter = bvNone
    Constraints.MaxHeight = 46
    Constraints.MinHeight = 46
    Constraints.MinWidth = 172
    ParentColor = True
    TabOrder = 1
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 45
      Height = 46
      Align = alLeft
      Center = True
      Transparent = True
    end
    object cxLabel2: TcxLabel
      Left = 50
      Top = 8
      Caption = 'Parsel Bilgisi Ekleme'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -11
      Style.Font.Name = 'Tahoma'
      Style.Font.Style = [fsBold]
      Style.IsFontAssigned = True
      Transparent = True
    end
    object cxLabel3: TcxLabel
      Left = 51
      Top = 23
      Caption = 'Parsel Bilgisi Ekleme Se'#231'eneklerini buradan de'#287'i'#351'tirebilirsiniz.'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -11
      Style.Font.Name = 'Tahoma'
      Style.Font.Style = []
      Style.IsFontAssigned = True
      Transparent = True
    end
  end
  object pnlAlt: TPanel
    Left = 0
    Top = 128
    Width = 352
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    Constraints.MaxHeight = 40
    Constraints.MinHeight = 40
    Constraints.MinWidth = 172
    ParentColor = True
    TabOrder = 2
    DesignSize = (
      352
      40)
    object btnTamam: TcxButton
      Left = 182
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight]
      Caption = 'Tamam'
      Constraints.MaxWidth = 75
      Constraints.MinWidth = 75
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnIptal: TcxButton
      Left = 263
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight]
      Cancel = True
      Caption = #304'ptal'
      Constraints.MaxWidth = 75
      Constraints.MinWidth = 75
      ModalResult = 2
      TabOrder = 1
    end
  end
end
