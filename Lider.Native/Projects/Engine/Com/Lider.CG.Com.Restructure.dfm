object fmRestructDlg: TfmRestructDlg
  Left = 296
  Top = 210
  HelpContext = 640
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  ClientHeight = 357
  ClientWidth = 483
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnDestroy = FormDestroy
  DesignSize = (
    483
    357)
  TextHeight = 15
  object cxGroupBox1: TcxGroupBox
    Left = 1
    Top = 1
    Anchors = [akLeft, akTop, akRight, akBottom]
    PanelStyle.Active = True
    TabOrder = 0
    Height = 320
    Width = 475
    object DBNavigator1: TDBNavigator
      Left = 2
      Top = 2
      Width = 471
      Height = 25
      DataSource = DataSource1
      VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete]
      Align = alTop
      Flat = True
      Hints.Strings = (
        #304'lk'
        #214'nceki'
        'Sonraki'
        'Son'
        'Ekle'
        'Sil'
        'G'#252'ncelle'
        'Yaz'
        'Vazge'#231
        'Yenile')
      ConfirmDelete = False
      TabOrder = 0
      Visible = False
      OnClick = DBNavigator1Click
    end
    object DBGrid2: TDBGrid
      Left = 2
      Top = 52
      Width = 471
      Height = 225
      Align = alClient
      BorderStyle = bsNone
      Ctl3D = True
      DataSource = DataSource1
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Options = [dgEditing, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgCancelOnExit]
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 1
      TitleFont.Charset = ANSI_CHARSET
      TitleFont.Color = clBlack
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
      OnColEnter = DBGrid2ColEnter
      Columns = <
        item
          Expanded = False
          FieldName = 'FIELDNAME'
          Title.Caption = 'Kolon Ad'#305
          Width = 144
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'TYPE'
          PickList.Strings = (
            'C - KARAKTER'
            'L - MANTIKSAL (Y/N or T/F)'
            'D - TAR'#304'H'
            'N - SAYISAL'
            'M - MET'#304'N'
            'B - BINARY'
            'G - GRAF'#304'K')
          Title.Caption = 'T'#252'r'
          Width = 114
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'SIZE'
          Title.Caption = 'Geni'#351'lik'
          Width = 56
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'DEC'
          Title.Caption = 'Ondal'#305'k'
          Width = 40
          Visible = True
        end>
    end
    object Panel2: TPanel
      Left = 2
      Top = 277
      Width = 471
      Height = 41
      Align = alBottom
      TabOrder = 2
      object LblStatus: TcxLabel
        Left = 1
        Top = 1
        Align = alClient
        Caption = '...'
        Properties.WordWrap = True
        TabOrder = 0
        Transparent = True
        Width = 469
      end
    end
    object cxDBNavigator1: TcxDBNavigator
      Left = 2
      Top = 27
      Width = 471
      Height = 25
      Buttons.OnButtonClick = cxDBNavigator1ButtonsButtonClick
      Buttons.CustomButtons = <>
      Buttons.PriorPage.Visible = False
      Buttons.NextPage.Visible = False
      Buttons.Insert.Visible = False
      Buttons.Append.Visible = True
      Buttons.Edit.Visible = True
      Buttons.Refresh.Visible = False
      Buttons.SaveBookmark.Visible = False
      Buttons.GotoBookmark.Visible = False
      Buttons.Filter.Visible = False
      DataSource = DataSource1
      Align = alTop
      TabOrder = 3
    end
  end
  object BtnAddFlds: TcxButton
    Left = 8
    Top = 328
    Width = 169
    Height = 25
    Action = Action1
    TabOrder = 1
  end
  object cxButton1: TcxButton
    Left = 392
    Top = 328
    Width = 75
    Height = 25
    Caption = 'Kapat'
    ModalResult = 2
    TabOrder = 2
  end
  object cxButton2: TcxButton
    Left = 240
    Top = 328
    Width = 75
    Height = 25
    Action = Action2
    TabOrder = 3
  end
  object DataSource1: TDataSource
    OnDataChange = DataSource1DataChange
    Left = 212
    Top = 184
  end
  object ActionList1: TActionList
    Left = 92
    Top = 155
    object Action1: TAction
      Caption = 'Kolonlar'#305' DBF Dosyadan Al'
      OnExecute = Action1Execute
    end
    object Action2: TAction
      Caption = 'Kaydet'
      OnExecute = Action2Execute
    end
    object Action3: TAction
      Caption = 'Nesne Eklenirken De'#287'erini Di'#287'er Tabaka DBF Tablosundan AL'
    end
  end
end
