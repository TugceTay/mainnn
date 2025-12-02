object fmShowEntityList: TfmShowEntityList
  Left = 461
  Top = 249
  BorderIcons = [biSystemMenu]
  Caption = 
    'Di'#287'er kullan'#305'c'#305'lar taraf'#305'ndan eklenmi'#351',silinmi'#351' veya de'#287'i'#351'tirilm' +
    'i'#351' nesneler bulundu.'
  ClientHeight = 374
  ClientWidth = 341
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 15
  object cxTreeList1: TcxTreeList
    Left = 0
    Top = 0
    Width = 341
    Height = 355
    Align = alClient
    Bands = <
      item
      end>
    Navigator.Buttons.CustomButtons = <>
    OptionsData.CancelOnExit = False
    OptionsData.Editing = False
    OptionsData.AnsiSort = True
    OptionsData.Deleting = False
    ScrollbarAnnotations.CustomAnnotations = <>
    TabOrder = 0
    OnDblClick = cxTreeList1DblClick
    object cxTreeList1Column1: TcxTreeListColumn
      PropertiesClassName = 'TcxTextEditProperties'
      Properties.ReadOnly = True
      Properties.Buttons = <>
      Caption.Text = 'Tabaka'
      Options.Editing = False
      Options.Moving = False
      Options.Sorting = False
      Width = 99
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxTreeList1Column2: TcxTreeListColumn
      PropertiesClassName = 'TcxTextEditProperties'
      Properties.ReadOnly = True
      Properties.Buttons = <>
      Caption.Text = 'Tipi'
      Options.Editing = False
      Options.Moving = False
      Options.Sorting = False
      Width = 64
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxTreeList1Column3: TcxTreeListColumn
      PropertiesClassName = 'TcxTextEditProperties'
      Properties.ReadOnly = True
      Properties.Buttons = <>
      Caption.Text = 'ID'
      Options.Editing = False
      Options.Moving = False
      Options.Sorting = False
      Width = 51
      Position.ColIndex = 2
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxTreeList1Column4: TcxTreeListColumn
      PropertiesClassName = 'TcxTextEditProperties'
      Properties.Buttons = <>
      Caption.Text = 'A'#231#305'klama'
      Options.Editing = False
      Options.Moving = False
      Options.Sorting = False
      Width = 120
      Position.ColIndex = 3
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 355
    Width = 341
    Height = 19
    Panels = <
      item
        Text = 
          'Nesne sat'#305'rlar'#305'n'#305'n '#252'zerinde '#231'ift t'#305'klayarak nesne konumuna gideb' +
          'ilirsiniz...'
        Width = 200
      end>
  end
end
