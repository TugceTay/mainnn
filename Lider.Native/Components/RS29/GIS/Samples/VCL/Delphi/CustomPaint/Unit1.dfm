object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'CustomPaint - TatukGIS DK11 Sample'
  ClientHeight = 600
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    800
    600)
  PixelsPerInch = 96
  TextHeight = 13
  object GIS: TGIS_ViewerWnd
    Left = 8
    Top = 39
    Width = 784
    Height = 553
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
    ProgressiveUpdate = 2500
    Mode = Zoom
    BorderStyle = bsNone
    PaintExtraEvent = GISPaintExtraEvent
  end
  object btnChangeRenderer: TButton
    Left = 8
    Top = 8
    Width = 113
    Height = 25
    Caption = 'Change to GDI32'
    TabOrder = 1
    OnClick = btnChangeRendererClick
  end
end
