object fmKatmanOznitelikTablosu: TfmKatmanOznitelikTablosu
  Left = 0
  Top = 0
  Caption = 'Katmanznitelik Tablosu'
  ClientHeight = 720
  ClientWidth = 1080
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object pnlRibbon: TPanel
    Left = 0
    Top = 0
    Width = 1080
    Height = 88
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 1078
    object sbFilterArea: TSpeedButton
      Left = 224
      Top = 24
      Width = 72
      Height = 32
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'Alan'
      OnClick = sbFilterClick
    end
    object sbFilterLine: TSpeedButton
      Left = 304
      Top = 24
      Width = 72
      Height = 32
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'Cizgi'
      OnClick = sbFilterClick
    end
    object sbFilterPoint: TSpeedButton
      Left = 384
      Top = 24
      Width = 72
      Height = 32
      AllowAllUp = True
      GroupIndex = 1
      Down = True
      Caption = 'Nokta'
      OnClick = sbFilterClick
    end
    object btnParseGeometry: TcxButton
      Left = 16
      Top = 16
      Width = 180
      Height = 56
      Caption = 'Geometriye Gre Ayrr'
      OptionsImage.Spacing = 8
      TabOrder = 0
      OnClick = btnParseGeometryClick
    end
    object btnValidateArea: TcxButton
      Left = 488
      Top = 16
      Width = 136
      Height = 32
      Caption = 'Alan Do'#287'rulama'
      PopupMenu = pmValidateArea
      TabOrder = 1
      OnClick = btnValidateAreaClick
    end
    object btnValidateLine: TcxButton
      Left = 632
      Top = 16
      Width = 136
      Height = 32
      Caption = 'Cizgi Do'#287'rulama'
      PopupMenu = pmValidateLine
      TabOrder = 2
      OnClick = btnValidateLineClick
    end
    object btnValidatePoint: TcxButton
      Left = 776
      Top = 16
      Width = 136
      Height = 32
      Caption = 'Nokta Do'#287'rulama'
      PopupMenu = pmValidatePoint
      TabOrder = 3
      OnClick = btnValidatePointClick
    end
  end
  object pnlSummary: TPanel
    Left = 0
    Top = 88
    Width = 1080
    Height = 64
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 1078
    object lblAreaSummary: TcxLabel
      Left = 16
      Top = 16
      Caption = 'Alan: 0 kayt | Toplam Alan: 0'
      TabOrder = 0
      Transparent = True
    end
    object lblLineSummary: TcxLabel
      Left = 280
      Top = 16
      Caption = 'Cizgi: 0 kayt | Toplam Uzunluk: 0'
      TabOrder = 1
      Transparent = True
    end
    object lblPointSummary: TcxLabel
      Left = 544
      Top = 16
      Caption = 'Nokta: 0 kayt'
      TabOrder = 2
      Transparent = True
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 152
    Width = 1080
    Height = 544
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitWidth = 1078
    ExplicitHeight = 536
    object pcGeometry: TcxPageControl
      Left = 8
      Top = 0
      Width = 760
      Height = 544
      Align = alClient
      TabOrder = 0
      Properties.ActivePage = tsArea
      Properties.CustomButtons.Buttons = <>
      OnChange = pcGeometryChange
      ExplicitWidth = 758
      ExplicitHeight = 536
      ClientRectBottom = 544
      ClientRectRight = 760
      ClientRectTop = 24
      object tsArea: TcxTabSheet
        Caption = 'Alan'
        ImageIndex = 0
        ExplicitWidth = 758
        ExplicitHeight = 512
        object grdArea: TcxGrid
          Left = 0
          Top = 0
          Width = 760
          Height = 520
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 758
          ExplicitHeight = 512
          object tvArea: TcxGridTableView
            Navigator.Buttons.CustomButtons = <>
            ScrollbarAnnotations.CustomAnnotations = <>
            DataController.Summary.DefaultGroupSummaryItems = <>
            DataController.Summary.FooterSummaryItems = <>
            DataController.Summary.SummaryGroups = <>
            OptionsBehavior.IncSearch = True
            OptionsData.Appending = True
            OptionsView.ColumnAutoWidth = True
            Styles.OnGetContentStyle = tvAreaStylesGetContentStyle
            object tvAreaGEOMETRYTYPE: TcxGridColumn
              Caption = 'Geometri Tipi'
              Options.Editing = False
            end
            object tvAreaLAYERUID: TcxGridColumn
              Caption = 'LayerUid'
            end
            object tvAreaREVISION: TcxGridColumn
              Caption = 'Revision'
            end
            object tvAreaNAME: TcxGridColumn
              Caption = 'Adi'
            end
            object tvAreaAREA: TcxGridColumn
              Caption = 'Alan'
            end
            object tvAreaISCLOSED: TcxGridColumn
              Caption = 'Kapali'
            end
            object tvAreaVALIDATIONNOTE: TcxGridColumn
              Caption = 'Uyar'
              Options.Editing = False
            end
          end
          object lvArea: TcxGridLevel
            GridView = tvArea
          end
        end
      end
      object tsLine: TcxTabSheet
        Caption = 'Cizgi'
        ImageIndex = 1
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object grdLine: TcxGrid
          Left = 0
          Top = 0
          Width = 760
          Height = 520
          Align = alClient
          TabOrder = 0
          object tvLine: TcxGridTableView
            Navigator.Buttons.CustomButtons = <>
            ScrollbarAnnotations.CustomAnnotations = <>
            DataController.Summary.DefaultGroupSummaryItems = <>
            DataController.Summary.FooterSummaryItems = <>
            DataController.Summary.SummaryGroups = <>
            OptionsBehavior.IncSearch = True
            OptionsData.Appending = True
            OptionsView.ColumnAutoWidth = True
            Styles.OnGetContentStyle = tvLineStylesGetContentStyle
            object tvLineGEOMETRYTYPE: TcxGridColumn
              Caption = 'Geometri Tipi'
              Options.Editing = False
            end
            object tvLineLAYERUID: TcxGridColumn
              Caption = 'LayerUid'
            end
            object tvLineREVISION: TcxGridColumn
              Caption = 'Revision'
            end
            object tvLineNAME: TcxGridColumn
              Caption = 'Adi'
            end
            object tvLineLENGTH: TcxGridColumn
              Caption = 'Uzunluk'
            end
            object tvLineVERTEXCOUNT: TcxGridColumn
              Caption = 'Kose Sayisi'
            end
            object tvLineVALIDATIONNOTE: TcxGridColumn
              Caption = 'Uyari'
              Options.Editing = False
            end
          end
          object lvLine: TcxGridLevel
            GridView = tvLine
          end
        end
      end
      object tsPoint: TcxTabSheet
        Caption = 'Nokta'
        ImageIndex = 2
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object grdPoint: TcxGrid
          Left = 0
          Top = 0
          Width = 760
          Height = 520
          Align = alClient
          TabOrder = 0
          object tvPoint: TcxGridTableView
            Navigator.Buttons.CustomButtons = <>
            ScrollbarAnnotations.CustomAnnotations = <>
            DataController.Summary.DefaultGroupSummaryItems = <>
            DataController.Summary.FooterSummaryItems = <>
            DataController.Summary.SummaryGroups = <>
            OptionsBehavior.IncSearch = True
            OptionsData.Appending = True
            OptionsView.ColumnAutoWidth = True
            Styles.OnGetContentStyle = tvPointStylesGetContentStyle
            object tvPointGEOMETRYTYPE: TcxGridColumn
              Caption = 'Geometri Tipi'
              Options.Editing = False
            end
            object tvPointLAYERUID: TcxGridColumn
              Caption = 'LayerUid'
            end
            object tvPointREVISION: TcxGridColumn
              Caption = 'Revision'
            end
            object tvPointNAME: TcxGridColumn
              Caption = 'Adi'
            end
            object tvPointELEVATION: TcxGridColumn
              Caption = 'Yukseklik'
            end
            object tvPointXCOORD: TcxGridColumn
              Caption = 'X'
            end
            object tvPointYCOORD: TcxGridColumn
              Caption = 'Y'
            end
            object tvPointVALIDATIONNOTE: TcxGridColumn
              Caption = 'Uyar'
              Options.Editing = False
            end
          end
          object lvPoint: TcxGridLevel
            GridView = tvPoint
          end
        end
      end
    end
    object splDetail: TcxSplitter
      Left = 0
      Top = 0
      Width = 8
      Height = 544
      ExplicitHeight = 536
    end
    object pnlDetail: TPanel
      Left = 768
      Top = 0
      Width = 312
      Height = 544
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 766
      ExplicitHeight = 536
      object lblDetail: TcxLabel
        Left = 8
        Top = 8
        Caption = 'Se'
        TabOrder = 1
        Transparent = True
      end
      object vgDetails: TcxDBVerticalGrid
        Left = 0
        Top = 0
        Width = 312
        Height = 544
        Align = alClient
        OptionsView.RowHeaderWidth = 160
        Navigator.Buttons.CustomButtons = <>
        ScrollbarAnnotations.CustomAnnotations = <>
        TabOrder = 0
        ExplicitHeight = 536
        Version = 1
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 696
    Width = 1080
    Height = 24
    Panels = <>
    SimplePanel = True
    SimpleText = 'Haz'
    ExplicitTop = 688
    ExplicitWidth = 1078
  end
  object pmValidateArea: TPopupMenu
    Left = 928
    Top = 24
    object mniAreaMandatory: TMenuItem
      Caption = 'Zorunlu alanlar kontrol et'
      OnClick = mniAreaMandatoryClick
    end
    object mniAreaMetrics: TMenuItem
      Caption = 'Alan metriklerinincelle'
      OnClick = mniAreaMetricsClick
    end
  end
  object pmValidateLine: TPopupMenu
    Left = 960
    Top = 24
    object mniLineMandatory: TMenuItem
      Caption = 'Zorunlu alanlarkontrol et'
      OnClick = mniLineMandatoryClick
    end
    object mniLineMetrics: TMenuItem
      Caption = 'Uzunluk ve  '
      OnClick = mniLineMetricsClick
    end
  end
  object pmValidatePoint: TPopupMenu
    Left = 992
    Top = 24
    object mniPointMandatory: TMenuItem
      Caption = 'Zorunlu alanlar kontrol et'
      OnClick = mniPointMandatoryClick
    end
    object mniPointMetrics: TMenuItem
      Caption = 'Koordinat/do'
      OnClick = mniPointMetricsClick
    end
  end
  object dsArea: TDataSource
    DataSet = mdArea
    OnDataChange = dsDataChange
    Left = 224
    Top = 448
  end
  object dsLine: TDataSource
    DataSet = mdLine
    OnDataChange = dsDataChange
    Left = 256
    Top = 448
  end
  object dsPoint: TDataSource
    DataSet = mdPoint
    OnDataChange = dsDataChange
    Left = 288
    Top = 448
  end
  object dsDetail: TDataSource
    OnDataChange = dsDataChange
    Left = 320
    Top = 448
  end
  object mdArea: TdxMemData
    Indexes = <>
    SortOptions = []
    Left = 224
    Top = 400
  end
  object mdLine: TdxMemData
    Indexes = <>
    SortOptions = []
    Left = 256
    Top = 400
  end
  object mdPoint: TdxMemData
    Indexes = <>
    SortOptions = []
    Left = 288
    Top = 400
  end
end
