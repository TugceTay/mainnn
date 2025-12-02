object Form1: TForm1
  Left = 200
  Top = 120
  Caption = 'Fields - TatukGIS DK11 Sample'
  ClientHeight = 657
  ClientWidth = 832
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 638
    Width = 832
    Height = 19
    Panels = <>
    SimplePanel = True
    SimpleText = 'Open a layer properties form to change parameters'
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 832
    Height = 29
    Caption = 'ToolBar1'
    TabOrder = 1
    object Button1: TButton
      Left = 0
      Top = 0
      Width = 75
      Height = 22
      Caption = 'Create Layer'
      TabOrder = 0
      OnClick = Button1Click
    end
    object ToolButton1: TToolButton
      Left = 75
      Top = 0
      Width = 8
      Caption = 'ToolButton1'
      Style = tbsSeparator
    end
    object chkUseSymbols: TCheckBox
      Left = 83
      Top = 0
      Width = 97
      Height = 22
      Caption = 'Use symbols'
      TabOrder = 1
    end
  end
  object GIS: TGIS_ViewerWnd
    Left = 129
    Top = 29
    Width = 703
    Height = 489
    Cursor = 16
    Align = alClient
    Ctl3D = True
    ParentColor = False
    ParentCtl3D = False
    TabOrder = 2
    IncrementalPaint = False
    SelectionTransparency = 100
  end
  object GIS_ControlLegend: TGIS_ControlLegend
    Left = 0
    Top = 29
    Width = 129
    Height = 489
    GIS_Viewer = GIS
    Mode = Layers
    Options = [AllowMove, AllowActive, AllowExpand, AllowParams, AllowSelect, ShowSubLayers, AllowParamsVisible]
    ReverseOrder = False
    Align = alLeft
    ParentColor = False
    TabStop = True
    TabOrder = 3
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 518
    Width = 832
    Height = 120
    Align = alBottom
    DataSource = DataSource1
    TabOrder = 4
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object GIS_DataSet1: TGIS_DataSet
    Left = 208
    Top = 80
  end
  object DataSource1: TDataSource
    DataSet = GIS_DataSet1
    Left = 272
    Top = 80
  end
end
