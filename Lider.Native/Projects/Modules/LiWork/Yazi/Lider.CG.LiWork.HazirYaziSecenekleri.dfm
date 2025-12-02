object fmHazirYaziSecenekleri: TfmHazirYaziSecenekleri
  Left = 0
  Top = 0
  AutoSize = True
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Haz'#305'r Yaz'#305' Se'#231'enekleri'
  ClientHeight = 305
  ClientWidth = 553
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  ShowHint = True
  TextHeight = 15
  object IcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 553
    Height = 305
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    OnDblClick = IcMainDblClick
    object cbMetin1: TcxComboBox
      Tag = 1
      Left = 63
      Top = 74
      AutoSize = False
      Properties.Buttons = <
        item
        end>
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 1
      Height = 23
      Width = 200
    end
    object cbMetin2: TcxComboBox
      Tag = 2
      Left = 63
      Top = 104
      AutoSize = False
      Properties.Buttons = <
        item
        end>
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Height = 23
      Width = 200
    end
    object cbMetin3: TcxComboBox
      Tag = 3
      Left = 63
      Top = 134
      AutoSize = False
      Properties.Buttons = <
        item
        end>
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Height = 23
      Width = 200
    end
    object cbMetin4: TcxComboBox
      Tag = 4
      Left = 63
      Top = 164
      Properties.Buttons = <
        item
        end>
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Width = 200
    end
    object cbMetin5: TcxComboBox
      Tag = 5
      Left = 63
      Top = 192
      AutoSize = False
      Properties.Buttons = <
        item
        end>
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 9
      Height = 23
      Width = 200
    end
    object cbMetin6: TcxComboBox
      Tag = 6
      Left = 320
      Top = 74
      AutoSize = False
      Properties.Buttons = <
        item
        end>
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 15
      Height = 23
      Width = 200
    end
    object cbMetin7: TcxComboBox
      Tag = 7
      Left = 320
      Top = 104
      AutoSize = False
      Properties.Buttons = <
        item
        end>
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 16
      Height = 23
      Width = 200
    end
    object cbMetin8: TcxComboBox
      Tag = 8
      Left = 320
      Top = 134
      AutoSize = False
      Properties.Buttons = <
        item
        end>
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 17
      Height = 23
      Width = 200
    end
    object cbMetin9: TcxComboBox
      Tag = 9
      Left = 320
      Top = 164
      AutoSize = False
      Properties.Buttons = <
        item
        end>
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 18
      Height = 23
      Width = 200
    end
    object cbMetin10: TcxComboBox
      Tag = 10
      Left = 320
      Top = 194
      AutoSize = False
      Properties.Buttons = <
        item
        end>
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 19
      Height = 21
      Width = 200
    end
    object teTamMetin: TcxTextEdit
      Left = 12
      Top = 236
      ParentShowHint = False
      Properties.Buttons = <>
      ShowHint = True
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 20
      TextHint = 'Yaz'#305'lar'#305'n Kombinasyonu Burada G'#246'sterilecektir.'
      Width = 522
    end
    object btnAyarlar: TcxButton
      Left = 12
      Top = 264
      Width = 74
      Height = 25
      Caption = '&Ayarlar'
      TabOrder = 21
    end
    object btnTamam: TcxButton
      Left = 377
      Top = 264
      Width = 75
      Height = 25
      Caption = 'Tamam'
      ModalResult = 1
      TabOrder = 22
    end
    object btnIptal: TcxButton
      Left = 459
      Top = 264
      Width = 75
      Height = 25
      Caption = #304'ptal'
      ModalResult = 2
      TabOrder = 23
    end
    object chkAyarlarKullan: TcxCheckBox
      Left = 93
      Top = 264
      Caption = 'Ayarlar'#305' &Kullan'
      ParentShowHint = False
      ShowHint = True
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 24
      Transparent = True
    end
    object chkMetin2: TcxCheckBox
      Tag = 2
      Left = 26
      Top = 104
      AutoSize = False
      Caption = '0'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
      Height = 23
      Width = 30
    end
    object chkMetin3: TcxCheckBox
      Tag = 3
      Left = 26
      Top = 134
      Caption = '0'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Transparent = True
    end
    object chkMetin4: TcxCheckBox
      Tag = 4
      Left = 26
      Top = 164
      Caption = '0'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Transparent = True
    end
    object chkMetin5: TcxCheckBox
      Tag = 5
      Left = 26
      Top = 192
      Caption = '0'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 8
      Transparent = True
    end
    object chkMetin6: TcxCheckBox
      Tag = 6
      Left = 283
      Top = 74
      Caption = '0'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 10
      Transparent = True
    end
    object chkMetin7: TcxCheckBox
      Tag = 7
      Left = 283
      Top = 100
      AutoSize = False
      Caption = '0'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 11
      Transparent = True
      Height = 28
      Width = 30
    end
    object chkMetin8: TcxCheckBox
      Tag = 8
      Left = 283
      Top = 135
      AutoSize = False
      Caption = '0'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 12
      Transparent = True
      Height = 19
      Width = 30
    end
    object chkMetin9: TcxCheckBox
      Tag = 9
      Left = 283
      Top = 161
      AutoSize = False
      Caption = '0'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 13
      Transparent = True
      Height = 28
      Width = 30
    end
    object chkMetin10: TcxCheckBox
      Tag = 10
      Left = 283
      Top = 196
      Caption = '0'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 14
      Transparent = True
    end
    object chkMetin1: TcxCheckBox
      Tag = 1
      Left = 26
      Top = 74
      AutoSize = False
      Caption = '0'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Transparent = True
      Height = 19
      Width = 30
    end
    object Image1: TImage
      Left = 4
      Top = 4
      Width = 45
      Height = 46
      Center = True
      Picture.Data = {
        055449636F6E0000010002002020000001002000A81000002600000010100000
        0100200068040000CE1000002800000020000000400000000100200000000000
        8010000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000000000000000000000006F6C6BA1
        6F6C6BFC706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE
        706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE
        706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE
        706D6CFE6F6C6BFC6F6C6BA1000000000000000000000000000000006F6C6BFC
        F9F9F9FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDF9F9F9FD6F6C6BFC00000000000000000000000000000000706D6CFE
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFD706D6CFE00000000000000000000000000000000706D6CFE
        FDFDFDFDFDFDFDFDBBBABAFDB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FE
        B3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FE
        B3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEBBBABAFD
        FDFDFDFDFDFDFDFD706D6CFE00000000000000000000000000000000706D6CFE
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFD706D6CFE00000000000000000000000000000000706D6CFE
        FDFDFDFDFDFDFDFDBBBABAFDB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FE
        B3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FE
        B3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEBBBABAFD
        FDFDFDFDFDFDFDFD706D6CFE00000000000000000000000000000000706D6CFE
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFD706D6CFE00000000000000000000000000000000706D6CFE
        FDFDFDFDFDFDFDFDBBBABAFDB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FE
        B3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FE
        B3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEBBBABAFD
        FDFDFDFDFDFDFDFD706D6CFE00000000000000000000000000000000706D6CFE
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFD706D6CFE00000000000000000000000000000000706D6CFE
        FDFDFDFDFDFDFDFDBBBABAFDB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FE
        B3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FE
        B3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEBBBABAFD
        FDFDFDFDFDFDFDFD706D6CFE00000000000000000000000000000000706D6CFE
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFD706D6CFE00000000000000000000000000000000706D6CFE
        FDFDFDFDFDFDFDFDBBBABAFDB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FE
        B3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FE
        B3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEBBBABAFD
        FDFDFDFDFDFDFDFD706D6CFE00000000000000000000000000000000706D6CFE
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFD706D6CFE00000000000000000000000000000000706D6CFE
        FDFDFDFDFDFDFDFDBBBABAFDB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FE
        B3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FE
        B3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEBBBABAFD
        FDFDFDFDFDFDFDFD706D6CFE00000000000000000000000000000000706D6CFE
        FDFDFDFDFDFDFDFDFBFAF9FCFCFCFCFCFDFDFDFCFDFDFDFCFDFDFDFCFDFDFDFC
        FDFDFDFCFBFAF9FCFCFCFCFCFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFD706D6CFE00000000000000000000000000000000706D6CFE
        FDFDFDFDFDFDFDFDC79D73FDD6B99BFDFDFDFDFCFDFDFDFCFDFDFDFCFDFDFDFC
        E1CCB7FCBC8A57FDFCFCFCFCBBBABAFDB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FE
        B3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEBBBABAFD
        FDFDFDFDFDFDFDFD706D6CFE00000000000000000000000000000000706D6CFE
        FDFDFDFDFDFDFDFDE3CFBAFDBB8854FDFCFCFBFCFDFDFDFCFDFDFDFCFDFDFDFC
        C59B6FFDD7BB9EFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFD706D6CFE00000000000000000000000000000000706D6CFE
        FDFDFDFDFDFDFDFDFAF8F6FCB68048FDCAA37CFDCEAA86FDCEAA86FDCEAA85FD
        B37A40FDF3ECE4FDFDFDFDFDBBBABAFDB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FE
        B3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEBBBABAFD
        FDFDFDFDFDFDFDFD706D6CFE00000000000000000000000000000000706D6CFE
        FDFDFDFDFDFDFDFDFDFDFDFDCFAC89FDC19465FDE3CFBBFCE3CFBBFCC9A179FD
        C4996CFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFD706D6CFE00000000000000000000000000000000706D6CFE
        FDFDFDFDFDFDFDFDFDFDFDFDEBDED0FDB67F48FDFAF8F6FCFDFDFDFCBF8F5FFD
        E0CAB4FDFDFDFDFDBBBABAFDB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FE
        B3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEBBBABAFD
        FDFDFDFDFDFDFDFD706D6CFE00000000000000000000000000000000706D6CFE
        FDFDFDFDFDFDFDFDFDFDFDFDFCFCFCFCBC8A58FDE4D0BDFCEFE5DAFCB57D45FD
        F9F6F3FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFD706D6CFE00000000000000000000000000000000706D6CFE
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDD8BB9FFDC9A179FDD4B596FDCDA882FD
        FDFDFDFDFDFDFDFDBBBABAFDB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FE
        B3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEBBBABAFD
        FDFDFDFDFDFDFDFD706D6CFE00000000000000000000000000000000706D6CFE
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDF3ECE5FDB47D44FDBB8855FDE8D9C9FD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFD706D6CFE00000000000000000000000000000000706D6CFE
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDC4996DFDBA8752FDFCFBFBFC
        FDFDFDFDFDFDFDFDBBBABAFDB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FE
        B3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEBBBABAFD
        FDFDFDFDFDFDFDFD706D6CFE00000000000000000000000000000000706D6CFE
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFD706D6CFE000000000000000000000000000000006F6C6BFC
        F9F9F9FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDF9F9F9FD6F6C6BFC000000000000000000000000000000006F6C6BA0
        6F6C6BFC706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE
        706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE
        706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE
        706D6CFE6F6C6BFC6F6C6BA00000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF
        C0000003C0000003C0000003C0000003C0000003C0000003C0000003C0000003
        C0000003C0000003C0000003C0000003C0000003C0000003C0000003C0000003
        C0000003C0000003C0000003C0000003C0000003C0000003C0000003C0000003
        C0000003C0000003C0000003FFFFFFFFFFFFFFFF280000001000000020000000
        010020000000000040040000000000000000000000000000000000006F6C6BB9
        6F6C6BFD706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE
        706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE6F6C6BFD6F6C6BB96F6C6BFD
        FCFCFCFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFCFCFCFD6F6C6BFD706D6CFE
        FDFDFDFDBBBABAFDB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FE
        B3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEBBBABAFDFDFDFDFD706D6CFE706D6CFE
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD706D6CFE706D6CFE
        FDFDFDFDBBBABAFDB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FE
        B3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEBBBABAFDFDFDFDFD706D6CFE706D6CFE
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD706D6CFE706D6CFE
        FDFDFDFDBBBABAFDB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FE
        B3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEBBBABAFDFDFDFDFD706D6CFE706D6CFE
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD706D6CFE706D6CFE
        FCFCFCFCBE8D5BFDEDE1D4FCFDFDFDFCFCFCFCFCBD8D5BFDEDE1D4FDBBBABAFD
        B3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEBBBABAFDFDFDFDFD706D6CFE706D6CFE
        FDFDFDFDD9BDA1FDC69C71FDDABFA4FCD5B696FDBE8D5CFDFCFCFCFCFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD706D6CFE706D6CFE
        FDFDFDFDF4EDE6FDB37B41FDDDC5ACFCC29466FDD9BEA3FDFDFDFDFDBBBABAFD
        B3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEBBBABAFDFDFDFDFD706D6CFE706D6CFE
        FDFDFDFDFDFDFDFDC59B6FFDE6D4C2FCB9854FFDF5EEE8FDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD706D6CFE706D6CFE
        FDFDFDFDFDFDFDFDE1CCB7FDBB8855FDC69C71FDFDFDFDFDFDFDFDFDBBBABAFD
        B3B2B2FEB3B2B2FEB3B2B2FEB3B2B2FEBBBABAFDFDFDFDFD706D6CFE706D6CFE
        FDFDFDFDFDFDFDFDF9F7F4FDB57E46FDE2CDB9FDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD706D6CFE6F6C6BFD
        FCFCFCFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
        FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFCFCFCFD6F6C6BFD6F6C6BB9
        6F6C6BFD706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE
        706D6CFE706D6CFE706D6CFE706D6CFE706D6CFE6F6C6BFD6F6C6BB900000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000000000000000000000000000000000000000000000000}
    end
    object IcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      Hidden = True
      ItemIndex = 3
      ShowBorder = False
      Index = -1
    end
    object lblInfoHader: TdxLayoutLabeledItem
      Parent = dxLayoutAutoCreatedGroup4
      AlignVert = avClient
      Offsets.Top = 8
      CaptionOptions.Text = '[B]Haz'#305'r Yaz'#305' Se'#231'enekleri[/B]'
      Index = 0
    end
    object lblInfo: TdxLayoutLabeledItem
      Parent = dxLayoutAutoCreatedGroup4
      AlignHorz = ahLeft
      AlignVert = avClient
      Offsets.Top = -4
      CaptionOptions.Text = 
        'Eklemek istedi'#287'iniz yaz'#305'lar'#305'n se'#231'eneklerini buradan de'#287'i'#351'tirebil' +
        'irsiniz.'
      Index = 1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = True
      SizeOptions.SizableVert = True
      SizeOptions.Width = 200
      CaptionOptions.Text = 'cxComboBox1'
      CaptionOptions.Visible = False
      Control = cbMetin1
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 200
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem2: TdxLayoutItem
      Tag = 1
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahClient
      CaptionOptions.Text = 'cxComboBox2'
      CaptionOptions.Visible = False
      Control = cbMetin2
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem3: TdxLayoutItem
      Tag = 2
      Parent = dxLayoutAutoCreatedGroup8
      AlignHorz = ahClient
      AlignVert = avTop
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = True
      SizeOptions.SizableVert = True
      SizeOptions.Width = 200
      CaptionOptions.Text = 'cxComboBox3'
      CaptionOptions.Visible = False
      Control = cbMetin3
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Tag = 3
      Parent = dxLayoutAutoCreatedGroup9
      AlignHorz = ahClient
      CaptionOptions.Text = 'cxComboBox4'
      CaptionOptions.Visible = False
      Control = cbMetin4
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem5: TdxLayoutItem
      Tag = 4
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'cxComboBox5'
      CaptionOptions.Visible = False
      Control = cbMetin5
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem6: TdxLayoutItem
      Tag = 5
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahRight
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = True
      SizeOptions.SizableVert = True
      SizeOptions.Width = 200
      CaptionOptions.Text = 'cxComboBox6'
      CaptionOptions.Visible = False
      Control = cbMetin6
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 200
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem7: TdxLayoutItem
      Tag = 6
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahLeft
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = True
      SizeOptions.SizableVert = True
      SizeOptions.Width = 200
      CaptionOptions.Text = 'cxComboBox7'
      CaptionOptions.Visible = False
      Control = cbMetin7
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 200
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem8: TdxLayoutItem
      Tag = 7
      Parent = dxLayoutAutoCreatedGroup2
      CaptionOptions.Text = 'cxComboBox8'
      CaptionOptions.Visible = False
      Control = cbMetin8
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem9: TdxLayoutItem
      Tag = 8
      Parent = dxLayoutAutoCreatedGroup2
      CaptionOptions.Text = 'cxComboBox9'
      CaptionOptions.Visible = False
      Control = cbMetin9
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem10: TdxLayoutItem
      Tag = 9
      Parent = dxLayoutAutoCreatedGroup2
      AlignVert = avClient
      CaptionOptions.Text = 'cxComboBox10'
      CaptionOptions.Visible = False
      Control = cbMetin10
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxteTamMetin: TdxLayoutItem
      Parent = IcMainGroup_Root
      CaptionOptions.Text = 'cxTextEdit1'
      CaptionOptions.Visible = False
      Control = teTamMetin
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object btnOptions: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnAyarlar
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 74
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object btnOk: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnTamam
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = IcMainGroup_Root
      LayoutDirection = ldHorizontal
      Index = 3
    end
    object btnCancel: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Text = 'cxButton3'
      CaptionOptions.Visible = False
      Control = btnIptal
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object chkAyarlarKullanilsin: TdxLayoutItem
      Tag = 11
      Parent = dxLayoutAutoCreatedGroup1
      AlignVert = avClient
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chkAyarlarKullan
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 96
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = IcMainGroup_Root
      AlignHorz = ahLeft
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      SizeOptions.Height = 150
      SizeOptions.Width = 400
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      Index = 1
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutGroup1
      Index = 3
    end
    object dxLayoutItem17: TdxLayoutItem
      Tag = 1
      Parent = dxLayoutAutoCreatedGroup7
      AlignVert = avClient
      SizeOptions.AssignedValues = [sovSizableHorz]
      SizeOptions.SizableHorz = True
      SizeOptions.Width = 30
      CaptionOptions.Text = 'cxCheckBox2'
      CaptionOptions.Visible = False
      Control = chkMetin2
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 30
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutGroup1
      Index = 0
    end
    object dxLayoutItem18: TdxLayoutItem
      Tag = 2
      Parent = dxLayoutAutoCreatedGroup8
      AlignVert = avClient
      SizeOptions.Width = 30
      CaptionOptions.Text = 'cxCheckBox3'
      CaptionOptions.Visible = False
      Control = chkMetin3
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 26
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem19: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      AlignVert = avClient
      SizeOptions.Width = 30
      CaptionOptions.Text = 'cxCheckBox4'
      CaptionOptions.Visible = False
      Control = chkMetin4
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 26
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem20: TdxLayoutItem
      Tag = 4
      Parent = dxLayoutAutoCreatedGroup3
      AlignVert = avClient
      SizeOptions.Width = 30
      CaptionOptions.Text = 'cxCheckBox5'
      CaptionOptions.Visible = False
      Control = chkMetin5
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 26
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem21: TdxLayoutItem
      Tag = 5
      Parent = dxLayoutAutoCreatedGroup10
      SizeOptions.Width = 30
      CaptionOptions.Text = 'cxCheckBox6'
      CaptionOptions.Visible = False
      Control = chkMetin6
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 24
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem22: TdxLayoutItem
      Tag = 6
      Parent = dxLayoutAutoCreatedGroup10
      AlignHorz = ahLeft
      AlignVert = avClient
      SizeOptions.Width = 30
      CaptionOptions.Text = 'cxCheckBox7'
      CaptionOptions.Visible = False
      Control = chkMetin7
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 30
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem23: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup10
      SizeOptions.Width = 30
      CaptionOptions.Text = 'cxCheckBox8'
      CaptionOptions.Visible = False
      Control = chkMetin8
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 24
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem24: TdxLayoutItem
      Tag = 8
      Parent = dxLayoutAutoCreatedGroup10
      AlignHorz = ahClient
      AlignVert = avClient
      SizeOptions.Width = 30
      CaptionOptions.Text = 'cxCheckBox9'
      CaptionOptions.Visible = False
      Control = chkMetin9
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 24
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem25: TdxLayoutItem
      Tag = 9
      Parent = dxLayoutAutoCreatedGroup10
      SizeOptions.AssignedValues = [sovSizableVert]
      SizeOptions.SizableVert = False
      SizeOptions.Width = 30
      CaptionOptions.Text = 'cxCheckBox10'
      CaptionOptions.Visible = False
      Control = chkMetin10
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 24
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem16: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahLeft
      SizeOptions.AssignedValues = [sovSizableVert]
      SizeOptions.SizableVert = True
      SizeOptions.Width = 30
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = chkMetin1
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 30
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup5
      LayoutDirection = ldHorizontal
      Index = 0
    end
    object dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup5
      LayoutDirection = ldHorizontal
      Index = 1
    end
    object dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup5
      LayoutDirection = ldHorizontal
      Index = 2
    end
    object dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup5
      LayoutDirection = ldHorizontal
      Index = 3
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup5
      LayoutDirection = ldHorizontal
      Index = 4
    end
    object dxLayoutAutoCreatedGroup10: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutGroup1
      Index = 2
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Image1'
      CaptionOptions.Visible = False
      Control = Image1
      ControlOptions.OriginalHeight = 46
      ControlOptions.OriginalWidth = 45
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = IcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      Offsets.Bottom = -4
      Offsets.Left = -8
      Offsets.Top = -8
      Hidden = True
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      Index = 1
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'Separator'
      Index = 1
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 500
    Top = 10
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
