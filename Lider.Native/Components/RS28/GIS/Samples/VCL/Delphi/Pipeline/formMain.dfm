object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Pipeline - TatukGIS DK11 Sample'
  ClientHeight = 611
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    784
    611)
  TextHeight = 13
  object lblCommands: TLabel
    Left = 8
    Top = 8
    Width = 52
    Height = 13
    Caption = 'Commands'
  end
  object lblCode: TLabel
    Left = 8
    Top = 357
    Width = 25
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Code'
  end
  object lbl1: TLabel
    Left = 210
    Top = 8
    Width = 38
    Height = 13
    Caption = 'Preview'
  end
  object GIS: TGIS_ViewerWnd
    Left = 207
    Top = 27
    Width = 434
    Height = 324
    Cursor = 18
    CursorForDrag = crDefault
    CursorForEdit = crDefault
    CursorForSelect = crDefault
    CursorForUserDefined = crDefault
    CursorForZoom = crDefault
    CursorForZoomEx = crDefault
    CursorForCameraPosition = crDefault
    CursorForCameraRotation = crDefault
    CursorForCameraXYZ = crDefault
    CursorForCameraXY = crDefault
    CursorForCameraZoom = crDefault
    CursorForSunPosition = crDefault
    CursorFor3DSelect = crDefault
    Anchors = [akLeft, akTop, akRight, akBottom]
    Ctl3D = True
    ParentColor = False
    ParentCtl3D = False
    TabOrder = 0
    KeepScale = False
    DelayedUpdate = 700
    ProgressiveUpdate = 2500
    Mode = Zoom
    Color = clWhite
    BorderStyle = bsNone
    ExplicitWidth = 430
    ExplicitHeight = 323
  end
  object memoCode: TMemo
    Left = 8
    Top = 376
    Width = 633
    Height = 196
    Anchors = [akLeft, akRight, akBottom]
    Lines.Strings = (
      'Say Text="Hello!"'
      
        'Layer.Open Result=$layer Path=C:\Lider.Native\Components\RS28\GI' +
        'S\Data\Samples11\World\VisibleEarth\world_8km.jpg'
      'Map.FullExtent'
      'Say Text="Done!"')
    ScrollBars = ssBoth
    TabOrder = 1
    OnDblClick = memoCodeDblClick
    ExplicitTop = 375
    ExplicitWidth = 629
  end
  object btnExecute: TButton
    Left = 620
    Top = 578
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Execute'
    Default = True
    TabOrder = 2
    OnClick = btnExecuteClick
    ExplicitLeft = 616
    ExplicitTop = 577
  end
  object btnExit: TButton
    Left = 701
    Top = 578
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Exit'
    TabOrder = 3
    OnClick = btnExitClick
    ExplicitLeft = 697
    ExplicitTop = 577
  end
  object btnHelp: TButton
    Left = 8
    Top = 578
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    TabOrder = 4
    OnClick = btnHelpClick
    ExplicitTop = 577
  end
  object lstbxCommands: TListBox
    Left = 11
    Top = 27
    Width = 193
    Height = 324
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 5
    OnDblClick = lstbxCommandsDblClick
    ExplicitHeight = 323
  end
  object btnOpen: TButton
    Left = 89
    Top = 578
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Open...'
    TabOrder = 6
    OnClick = btnOpenClick
    ExplicitTop = 577
  end
  object btnSave: TButton
    Left = 170
    Top = 578
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save...'
    TabOrder = 7
    OnClick = btnSaveClick
    ExplicitTop = 577
  end
  object GIS_ControlLegend1: TGIS_ControlLegend
    Left = 647
    Top = 27
    Width = 130
    Height = 324
    GIS_Viewer = GIS
    Options = [AllowMove, AllowActive, AllowExpand, AllowParams, AllowSelect, ShowSubLayers, AllowParamsVisible]
    BorderStyle = bsNone
    Anchors = [akTop, akRight, akBottom]
    TabOrder = 8
    Touch.InteractiveGestures = [igPan]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerVertical]
    ExplicitLeft = 643
    ExplicitHeight = 323
  end
  object pnlDynamicProgress: TPanel
    Left = 647
    Top = 376
    Width = 129
    Height = 196
    Anchors = [akRight, akBottom]
    TabOrder = 9
    ExplicitLeft = 643
    ExplicitTop = 375
  end
  object dlgOpen: TOpenDialog
    DefaultExt = '.ttkpipeline'
    Filter = 'ttkpipeline files|*.ttkpipeline'
    Left = 584
    Top = 248
  end
  object dlgSave: TSaveDialog
    DefaultExt = '.ttkpipeline'
    Filter = 'ttkpipeline files|*.ttkpipeline'
    Left = 552
    Top = 248
  end
end
