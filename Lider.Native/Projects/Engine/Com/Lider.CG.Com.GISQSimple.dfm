object fmGISQSimple: TfmGISQSimple
  Left = 362
  Top = 155
  HelpContext = 290
  Caption = 'GIS Sorgular'#305
  ClientHeight = 427
  ClientWidth = 527
  Color = clBtnFace
  Constraints.MinWidth = 526
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  Position = poScreenCenter
  OnClose = FormClose
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 185
    Width = 527
    Height = 222
    Align = alClient
    TabOrder = 0
    object Panel2: TPanel
      Left = 1
      Top = 188
      Width = 525
      Height = 33
      Align = alBottom
      TabOrder = 0
      DesignSize = (
        525
        33)
      object Button4: TSpeedButton
        Left = 9
        Top = 5
        Width = 112
        Height = 24
        Anchors = [akLeft, akBottom]
        Caption = '&Kay'#305'tlar'#305' Haritada Se'#231
        Flat = True
        OnClick = Button4Click
      end
      object Button5: TSpeedButton
        Left = 129
        Top = 5
        Width = 100
        Height = 24
        Anchors = [akLeft, akBottom]
        Caption = 'Se'#231'ili Kay'#305'tlara Git'
        Flat = True
        OnClick = Button5Click
      end
      object Button3: TSpeedButton
        Left = 240
        Top = 5
        Width = 88
        Height = 24
        Anchors = [akLeft, akBottom]
        Caption = '&Se'#231'ili Kayda Git'
        Flat = True
        OnClick = Button3Click
      end
      object Button2: TSpeedButton
        Left = 332
        Top = 6
        Width = 88
        Height = 23
        Anchors = [akLeft, akBottom]
        Caption = '&Yak/S'#246'nd'#252'r'
        Flat = True
        OnClick = Button2Click
      end
    end
    object Panel7: TPanel
      Left = 1
      Top = 1
      Width = 525
      Height = 77
      Align = alTop
      TabOrder = 1
      DesignSize = (
        525
        77)
      object CheckList1: TcxCheckListBox
        Left = 0
        Top = 0
        Width = 524
        Height = 76
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = 4
        EditValueFormat = cvfCaptions
        Items = <>
        TabOrder = 0
      end
    end
    object cxGrid1: TcxGrid
      Left = 1
      Top = 78
      Width = 525
      Height = 110
      Align = alClient
      TabOrder = 2
      object cxGrid1DBTableView1: TcxGridDBTableView
        Navigator.Buttons.CustomButtons = <>
        ScrollbarAnnotations.CustomAnnotations = <>
        DataController.DataSource = DataSource1
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        OptionsView.GroupByBox = False
      end
      object cxGrid1Level1: TcxGridLevel
        GridView = cxGrid1DBTableView1
      end
    end
  end
  object cxGroupBox4: TcxGroupBox
    Left = 0
    Top = 57
    Align = alTop
    PanelStyle.Active = True
    TabOrder = 1
    Height = 128
    Width = 527
    object cxGroupBox1: TcxGroupBox
      Left = 8
      Top = 26
      Caption = '1.Ko'#351'ul'
      PanelStyle.Active = True
      TabOrder = 0
      Height = 31
      Width = 508
      object cxComboBox_f1: TcxComboBox
        Left = 113
        Top = 5
        Properties.DropDownListStyle = lsFixedList
        Properties.OnChange = cxComboBox_f1PropertiesChange
        Properties.OnEditValueChanged = cxComboBox_f1PropertiesEditValueChanged
        Properties.Buttons = <
          item
          end>
        TabOrder = 0
        Width = 121
      end
      object cxComboBox_o1: TcxComboBox
        Left = 250
        Top = 5
        Properties.DropDownListStyle = lsFixedList
        Properties.OnEditValueChanged = cxComboBox_f1PropertiesEditValueChanged
        Properties.Buttons = <
          item
          end>
        TabOrder = 1
        Width = 121
      end
      object cxComboBox_d1: TcxComboBox
        Left = 385
        Top = 5
        Properties.OnChange = cxComboBox_f1PropertiesChange
        Properties.Buttons = <
          item
          end>
        TabOrder = 2
        Width = 121
      end
    end
    object cxGroupBox2: TcxGroupBox
      Left = 8
      Top = 61
      Caption = '2.Ko'#351'ul'
      PanelStyle.Active = True
      TabOrder = 1
      Height = 31
      Width = 509
      object cxComboBox_f2: TcxComboBox
        Left = 113
        Top = 4
        Properties.DropDownListStyle = lsFixedList
        Properties.OnChange = cxComboBox_f1PropertiesChange
        Properties.OnEditValueChanged = cxComboBox_f1PropertiesEditValueChanged
        Properties.Buttons = <
          item
          end>
        TabOrder = 0
        Width = 121
      end
      object cxComboBox_o2: TcxComboBox
        Left = 250
        Top = 4
        Properties.DropDownListStyle = lsFixedList
        Properties.OnEditValueChanged = cxComboBox_f1PropertiesEditValueChanged
        Properties.Buttons = <
          item
          end>
        TabOrder = 1
        Width = 121
      end
      object cxComboBox_d2: TcxComboBox
        Left = 385
        Top = 4
        Properties.OnChange = cxComboBox_f1PropertiesChange
        Properties.Buttons = <
          item
          end>
        TabOrder = 2
        Width = 121
      end
      object cxComboBox_k2: TcxComboBox
        Left = 50
        Top = 4
        Properties.Items.Strings = (
          'VE'
          'VEYA')
        Properties.OnChange = cxComboBox_k2PropertiesChange
        Properties.Buttons = <
          item
          end>
        TabOrder = 3
        Text = 'VE'
        Width = 56
      end
    end
    object cxGroupBox3: TcxGroupBox
      Left = 8
      Top = 94
      Caption = '3.Ko'#351'ul'
      PanelStyle.Active = True
      TabOrder = 2
      Height = 31
      Width = 510
      object cxComboBox_f3: TcxComboBox
        Left = 113
        Top = 4
        Properties.DropDownListStyle = lsFixedList
        Properties.OnChange = cxComboBox_f1PropertiesChange
        Properties.OnEditValueChanged = cxComboBox_f1PropertiesEditValueChanged
        Properties.Buttons = <
          item
          end>
        TabOrder = 0
        Width = 121
      end
      object cxComboBox_o3: TcxComboBox
        Left = 250
        Top = 4
        Properties.DropDownListStyle = lsFixedList
        Properties.OnEditValueChanged = cxComboBox_f1PropertiesEditValueChanged
        Properties.Buttons = <
          item
          end>
        TabOrder = 1
        Width = 121
      end
      object cxComboBox_d3: TcxComboBox
        Left = 385
        Top = 4
        Properties.OnChange = cxComboBox_f1PropertiesChange
        Properties.Buttons = <
          item
          end>
        TabOrder = 2
        Width = 121
      end
      object cxComboBox_k3: TcxComboBox
        Left = 50
        Top = 4
        Properties.Items.Strings = (
          'VE'
          'VEYA')
        Properties.OnChange = cxComboBox_k3PropertiesChange
        Properties.Buttons = <
          item
          end>
        TabOrder = 3
        Text = 'VE'
        Width = 56
      end
    end
    object cxLabel1: TcxLabel
      Left = 168
      Top = 8
      Caption = 'Kolon'
      Transparent = True
    end
    object cxLabel2: TcxLabel
      Left = 296
      Top = 8
      Caption = 'Operat'#246'r'
      Transparent = True
    end
    object cxLabel3: TcxLabel
      Left = 432
      Top = 8
      Caption = 'De'#287'er'
      Transparent = True
    end
  end
  object Memo1: TcxMemo
    Left = 0
    Top = 0
    Align = alTop
    Properties.ReadOnly = False
    Properties.ScrollBars = ssVertical
    TabOrder = 2
    Height = 57
    Width = 527
  end
  object dxStatusBar1: TdxStatusBar
    Left = 0
    Top = 407
    Width = 527
    Height = 20
    Panels = <
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        Width = 200
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        Width = 150
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        Width = 150
      end>
    PaintStyle = stpsUseLookAndFeel
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  object ColorDialog1: TColorDialog
    Left = 440
    Top = 200
  end
  object Table1: TlicgTable
    ReadOnly = True
    MaxRecords = 0
    UseDeleted = False
    Left = 240
    Top = 208
  end
  object DataSource1: TDataSource
    DataSet = Table1
    Left = 56
    Top = 198
  end
end
