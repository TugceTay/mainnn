object fmConnDialogExt: TfmConnDialogExt
  Left = 408
  Top = 212
  BorderStyle = bsDialog
  Caption = 'Veritaban'#305' Ba'#287'lant'#305' Ayarlar'#305
  ClientHeight = 328
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 39
    Top = 64
    Width = 45
    Height = 15
    Alignment = taRightJustify
    Caption = 'Kullan'#305'c'#305
  end
  object Label2: TLabel
    Left = 61
    Top = 88
    Width = 23
    Height = 15
    Alignment = taRightJustify
    Caption = #350'ifre'
  end
  object Label3: TLabel
    Left = 32
    Top = 144
    Width = 52
    Height = 15
    Alignment = taRightJustify
    Caption = 'Veritaban'#305
  end
  object Label4: TLabel
    Left = 44
    Top = 168
    Width = 40
    Height = 15
    Alignment = taRightJustify
    Caption = 'Sunucu'
  end
  object Label5: TLabel
    Left = 62
    Top = 192
    Width = 22
    Height = 15
    Alignment = taRightJustify
    Caption = 'Port'
  end
  object Label6: TLabel
    Left = 16
    Top = 2
    Width = 78
    Height = 15
    Caption = 'Ba'#287'lant'#305' Listesi'
    Visible = False
  end
  object Editus: TEdit
    Left = 104
    Top = 56
    Width = 200
    Height = 23
    TabOrder = 0
  end
  object Editpa: TEdit
    Left = 104
    Top = 80
    Width = 200
    Height = 23
    TabOrder = 1
  end
  object Editdb: TEdit
    Left = 104
    Top = 136
    Width = 200
    Height = 23
    TabOrder = 2
  end
  object Editse: TEdit
    Left = 104
    Top = 160
    Width = 200
    Height = 23
    TabOrder = 3
  end
  object Editpo: TEdit
    Left = 104
    Top = 184
    Width = 200
    Height = 23
    TabOrder = 4
  end
  object Button1: TButton
    Left = 216
    Top = 264
    Width = 91
    Height = 25
    Caption = 'Ba'#287'lan'
    ModalResult = 1
    TabOrder = 5
  end
  object Button2: TButton
    Left = 112
    Top = 264
    Width = 75
    Height = 25
    Caption = #304'ptal'
    ModalResult = 2
    TabOrder = 6
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 309
    Width = 337
    Height = 19
    Panels = <
      item
        Width = 250
      end>
    SimplePanel = True
  end
  object ComboBox1: TComboBox
    Left = 24
    Top = 26
    Width = 281
    Height = 23
    TabOrder = 8
    Text = 'ComboBox1'
    Visible = False
  end
  object cxPropertiesStore1: TcxPropertiesStore
    Components = <
      item
        Component = Editdb
        Properties.Strings = (
          'Text')
      end
      item
        Component = Editpa
        Properties.Strings = (
          'Text')
      end
      item
        Component = Editpo
        Properties.Strings = (
          'Text')
      end
      item
        Component = Editse
        Properties.Strings = (
          'Text')
      end
      item
        Component = Editus
        Properties.Strings = (
          'Text')
      end>
    StorageName = 'Settings\PropertiesStore.ini'
    StorageType = stRegistry
    Left = 56
    Top = 216
  end
end
