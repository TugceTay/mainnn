object fmTips: TfmTips
  Left = 338
  Top = 231
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'G'#252'n'#252'n Trickleri'
  ClientHeight = 253
  ClientWidth = 420
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 15
  object Panel1: TPanel
    Left = 12
    Top = 12
    Width = 401
    Height = 201
    BevelOuter = bvLowered
    Color = clWhite
    TabOrder = 0
    object Bevel1: TBevel
      Left = 60
      Top = 36
      Width = 337
      Height = 9
      Shape = bsBottomLine
    end
    object Label1: TLabel
      Left = 62
      Top = 12
      Width = 215
      Height = 22
      Caption = 'Bunlar'#305' biliyor muydunuz?'
      Font.Charset = TURKISH_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object TipLabel: TLabel
      Left = 60
      Top = 52
      Width = 333
      Height = 145
      AutoSize = False
      Font.Charset = TURKISH_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 56
      Height = 199
      Align = alLeft
      BevelOuter = bvNone
      Color = clBtnShadow
      TabOrder = 0
      object Image1: TImage
        Left = 8
        Top = 8
        Width = 37
        Height = 37
        Picture.Data = {
          055449636F6E0000010001002020100000000000E80200001600000028000000
          2000000040000000010004000000000080020000000000000000000000000000
          0000000000000000000080000080000000808000800000008000800080800000
          C0C0C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
          FFFFFF0000000000000000008000000000000000000000000000000888000000
          0000000000000000000000888880000000000000000000000000088808880000
          000000000000000000008880E088800000000000000000000008880EFE088800
          0000000000000000008880EFEFE08880000000000000000008880EFEFEFE0888
          00000000000000008880EFE000EFE0888000000000000008880EFE00000EFE08
          880000000000008880EFE88F7700EFE088800000000008880EFEF8F00070FEFE
          0888000000000080EFEF880F77000FEFE08000000000000EFEFE8FF000770EFE
          FE000000000000EFEFEF800333000FEFEFE000000000008EFEFE033BBB330EFE
          FE00000000000008EFE03BBBBBBB30EFE0000000000000008EF03BBFBFBF30FE
          00000000000000000803BBB808BBB3000000000000000000003BBFB000BFBF30
          000000000000000000BBBBF808FBBB3000000000000000003BBFBFBFBFBFBFB3
          00000000000000003BBBFBFB0BFBFBB300000000000000003FBFFFFF0FBFBFB3
          00000000000000003BFFFFF808FBFBB300000000000000003BFFFFF000BFBFB3
          000000000000000003FFFFF000FBFBB0000000000000000003BFFFF000BFBFB0
          0000000000000000003BFFF808FBBB0000000000000000000003BFBFBFBFB300
          0000000000000000000033BBBBB3300000000000000000000000003333300000
          00000000FFFF7FFFFFFE3FFFFFFC1FFFFFF80FFFFFF007FFFFE003FFFFC001FF
          FF8000FFFF00007FFE00003FFC00001FF800000FFC00001FFC00001FF800000F
          FC00001FFE00003FFF00007FFF8000FFFF8000FFFF8000FFFF00007FFF00007F
          FF00007FFF00007FFF00007FFF8000FFFF8000FFFFC001FFFFE003FFFFF007FF
          FFFC1FFF}
      end
    end
  end
  object chkShow: TCheckBox
    Left = 12
    Top = 225
    Width = 125
    Height = 17
    Caption = 'Ba'#351'lang'#305#231'ta G'#246'ster'
    Checked = True
    Font.Charset = TURKISH_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    State = cbChecked
    TabOrder = 1
  end
  object BtnNextTip: TButton
    Left = 252
    Top = 221
    Width = 75
    Height = 25
    Caption = #304'leri'
    TabOrder = 2
  end
  object BtnClose: TButton
    Left = 336
    Top = 221
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Kapat'
    ModalResult = 2
    TabOrder = 3
  end
end
