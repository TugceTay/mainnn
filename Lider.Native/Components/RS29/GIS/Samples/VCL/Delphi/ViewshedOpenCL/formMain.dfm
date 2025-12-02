object frmMain: TfrmMain
  Left = 0
  Top = 0
  Anchors = [akLeft, akTop, akRight]
  Caption = 'Viewshed OpenCL - TatukGIS DK11 Sample'
  ClientHeight = 480
  ClientWidth = 640
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    640
    480)
  TextHeight = 13
  object lblInfo: TLabel
    Left = 8
    Top = 8
    Width = 624
    Height = 13
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Click on the map and drag for real-time viewshed.'
  end
  object lblTime: TLabel
    Left = 111
    Top = 453
    Width = 173
    Height = 13
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
  end
  object GIS: TGIS_ViewerWnd
    Left = 8
    Top = 27
    Width = 624
    Height = 414
    Cursor = 20
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
    Mode = UserDefined
    Color = clWhite
    BorderStyle = bsNone
    OnMouseDown = GISMouseDown
    OnMouseMove = GISMouseMove
    OnMouseUp = GISMouseUp
    ExplicitWidth = 620
    ExplicitHeight = 413
  end
  object btnOpenCLInfo: TButton
    Left = 290
    Top = 447
    Width = 168
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Show OpenCL info'
    TabOrder = 1
    OnClick = btnOpenCLInfoClick
    ExplicitLeft = 286
    ExplicitTop = 446
  end
  object btnDeviceInfo: TButton
    Left = 464
    Top = 447
    Width = 168
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Show active device info'
    TabOrder = 2
    OnClick = btnDeviceInfoClick
    ExplicitLeft = 460
    ExplicitTop = 446
  end
  object chkbxUseOpenCL: TCheckBox
    Left = 8
    Top = 451
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Use OpenCL'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = chkbxUseOpenCLClick
    ExplicitTop = 450
  end
end
