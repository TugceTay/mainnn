object fmExprDlg: TfmExprDlg
  Left = 464
  Top = 223
  HelpContext = 290
  BorderStyle = bsDialog
  Caption = 'Expression Assistant'
  ClientHeight = 431
  ClientWidth = 499
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnClose = FormClose
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 3
    Top = 281
    Width = 94
    Height = 16
    Caption = '<Expression>'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object OKBtn: TButton
    Left = 268
    Top = 402
    Width = 69
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 341
    Top = 402
    Width = 69
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object BtnVerify: TButton
    Left = 3
    Top = 402
    Width = 57
    Height = 25
    Caption = '&Verify'
    TabOrder = 2
    OnClick = BtnVerifyClick
  end
  object GB1: TGroupBox
    Left = 140
    Top = 3
    Width = 277
    Height = 183
    Caption = '&Native database and custom functions'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object BtnAddField: TButton
      Left = 208
      Top = 154
      Width = 62
      Height = 22
      Caption = 'A&dd'
      TabOrder = 0
      OnClick = BtnAddFieldClick
    end
    object lbColumns: TListBox
      Left = 2
      Top = 15
      Width = 273
      Height = 135
      Style = lbOwnerDrawFixed
      Align = alTop
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = lbColumnsClick
      OnDblClick = BtnAddFieldClick
      OnDrawItem = lbColumnsDrawItem
    end
  end
  object GB2: TGroupBox
    Left = 3
    Top = 3
    Width = 134
    Height = 183
    Caption = '&Functions'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object BtnAddFunct: TButton
      Left = 65
      Top = 157
      Width = 62
      Height = 22
      Caption = '&Add'
      TabOrder = 0
      OnClick = BtnAddFunctClick
    end
    object lbFuncs: TListBox
      Left = 2
      Top = 15
      Width = 130
      Height = 135
      Align = alTop
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      Sorted = True
      TabOrder = 1
      OnClick = lbFuncsClick
      OnDblClick = BtnAddFunctClick
    end
  end
  object BtnClear: TButton
    Left = 62
    Top = 402
    Width = 57
    Height = 25
    Caption = '&Clear'
    TabOrder = 3
    OnClick = BtnClearClick
  end
  object BtnUndo: TButton
    Left = 120
    Top = 402
    Width = 57
    Height = 25
    Caption = '&Undo'
    TabOrder = 4
    OnClick = BtnUndoClick
  end
  object Group1: TGroupBox
    Left = 420
    Top = 0
    Width = 79
    Height = 431
    Align = alRight
    Caption = 'Operators'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    object BtnAdd: TSpeedButton
      Left = 4
      Top = 12
      Width = 32
      Height = 18
      Caption = ' + '
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object BtnSubct: TSpeedButton
      Left = 4
      Top = 32
      Width = 32
      Height = 18
      Caption = ' - '
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object BtnMult: TSpeedButton
      Left = 4
      Top = 52
      Width = 32
      Height = 18
      Caption = ' * '
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object BtnDiv: TSpeedButton
      Left = 4
      Top = 72
      Width = 32
      Height = 17
      Caption = ' / '
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object BtnIDiv: TSpeedButton
      Left = 4
      Top = 92
      Width = 32
      Height = 18
      Caption = ' DIV '
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object BtnMod: TSpeedButton
      Left = 4
      Top = 112
      Width = 32
      Height = 17
      Caption = ' MOD '
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object BtnEq: TSpeedButton
      Left = 4
      Top = 152
      Width = 32
      Height = 18
      Caption = ' = '
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object BtnDiff: TSpeedButton
      Left = 4
      Top = 172
      Width = 32
      Height = 18
      Caption = ' <> '
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object BtnLT: TSpeedButton
      Left = 4
      Top = 188
      Width = 32
      Height = 18
      Caption = ' < '
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object BtnGT: TSpeedButton
      Left = 4
      Top = 208
      Width = 32
      Height = 18
      Caption = ' > '
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object BtnLE: TSpeedButton
      Left = 4
      Top = 228
      Width = 32
      Height = 18
      Caption = ' <= '
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object BtnGE: TSpeedButton
      Left = 40
      Top = 228
      Width = 32
      Height = 18
      Caption = ' >= '
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object BtnExp: TSpeedButton
      Left = 4
      Top = 132
      Width = 32
      Height = 18
      Caption = ' ^ '
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object BtnNOT: TSpeedButton
      Left = 4
      Top = 248
      Width = 32
      Height = 18
      Caption = ' NOT '
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object BtnAND: TSpeedButton
      Left = 40
      Top = 248
      Width = 32
      Height = 18
      Caption = ' AND '
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object BtnOR: TSpeedButton
      Left = 4
      Top = 268
      Width = 32
      Height = 18
      Caption = ' OR '
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object BtnOpen: TSpeedButton
      Left = 40
      Top = 268
      Width = 32
      Height = 18
      Caption = ' (...)'
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnOpenClick
    end
    object Btn0: TSpeedButton
      Left = 40
      Top = 32
      Width = 31
      Height = 18
      Caption = '0'
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object Btn1: TSpeedButton
      Left = 40
      Top = 52
      Width = 31
      Height = 18
      Caption = '1'
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object Btn3: TSpeedButton
      Left = 40
      Top = 92
      Width = 31
      Height = 18
      Caption = '3'
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object Btn2: TSpeedButton
      Left = 40
      Top = 72
      Width = 31
      Height = 18
      Caption = '2'
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object Btn5: TSpeedButton
      Left = 40
      Top = 128
      Width = 31
      Height = 18
      Caption = '5'
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object Btn4: TSpeedButton
      Left = 40
      Top = 108
      Width = 31
      Height = 18
      Caption = '4'
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object Btn7: TSpeedButton
      Left = 40
      Top = 168
      Width = 31
      Height = 18
      Caption = '7'
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object Btn6: TSpeedButton
      Left = 40
      Top = 148
      Width = 31
      Height = 18
      Caption = '6'
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object Btn9: TSpeedButton
      Left = 40
      Top = 208
      Width = 31
      Height = 18
      Caption = '9'
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object Btn8: TSpeedButton
      Left = 40
      Top = 188
      Width = 31
      Height = 18
      Caption = '8'
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object BtnPeriod: TSpeedButton
      Left = 40
      Top = 12
      Width = 31
      Height = 18
      Caption = '.'
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object btnBetween: TSpeedButton
      Left = 4
      Top = 288
      Width = 69
      Height = 18
      Caption = ' BETWEEN '
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object btnLike: TSpeedButton
      Left = 5
      Top = 307
      Width = 69
      Height = 18
      Caption = ' LIKE "" '
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object btnIN: TSpeedButton
      Left = 5
      Top = 327
      Width = 69
      Height = 18
      Caption = ' IN () '
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
    object btnWithin: TSpeedButton
      Left = 5
      Top = 346
      Width = 69
      Height = 18
      Caption = 'Graphic Op'
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = btnWithinClick
    end
    object SpeedButton1: TSpeedButton
      Left = 6
      Top = 364
      Width = 69
      Height = 18
      Caption = ' ORDER BY '
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BtnAddClick
    end
  end
  object Panel1: TPanel
    Left = 4
    Top = 190
    Width = 413
    Height = 90
    BevelOuter = bvLowered
    TabOrder = 8
    object LblSyntax: TLabel
      Left = 1
      Top = 1
      Width = 411
      Height = 43
      Align = alClient
      AutoSize = False
      Color = clBackground
      Font.Charset = ANSI_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      WordWrap = True
    end
    object LblDesc: TLabel
      Left = 1
      Top = 44
      Width = 411
      Height = 45
      Align = alBottom
      AutoSize = False
      Color = clInfoBk
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      WordWrap = True
    end
  end
  object Memo1: TMemo
    Left = 3
    Top = 298
    Width = 407
    Height = 98
    Cursor = crIBeam
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 9
  end
  object PopupMenu1: TPopupMenu
    Left = 408
    Top = 348
    object Within1: TMenuItem
      Caption = 'Within'
      OnClick = Within1Click
    end
    object EntirelyWithin1: TMenuItem
      Caption = 'Entirely Within'
      OnClick = Within1Click
    end
    object EntirelyWithinNotEdgeTouch1: TMenuItem
      Caption = 'Entirely Within Not Edge Touch'
      OnClick = Within1Click
    end
    object Contains1: TMenuItem
      Caption = 'Contains'
      OnClick = Within1Click
    end
    object ContainsEntire1: TMenuItem
      Caption = 'Contains Entire'
      OnClick = Within1Click
    end
    object ContainsEntireNotEdgeTouch1: TMenuItem
      Caption = 'Contains Entire Not Edge Touch'
      OnClick = Within1Click
    end
    object Intersects1: TMenuItem
      Caption = 'Intersects'
      OnClick = Within1Click
    end
    object ExtentOverlaps1: TMenuItem
      Caption = 'Extent Overlaps'
      OnClick = Within1Click
    end
    object ShareCommonPoint1: TMenuItem
      Caption = 'Share Common Point'
      OnClick = Within1Click
    end
    object ShareCommonLine1: TMenuItem
      Caption = 'Share Common Line'
      OnClick = Within1Click
    end
    object LineCross1: TMenuItem
      Caption = 'Line Cross'
      OnClick = Within1Click
    end
    object CommonPointOrLineCross1: TMenuItem
      Caption = 'Common Point Or Line Cross'
      OnClick = Within1Click
    end
    object EdgeTouch1: TMenuItem
      Caption = 'Edge Touch'
      OnClick = Within1Click
    end
    object EdgeTouchOrIntersects1: TMenuItem
      Caption = 'Edge Touch Or Intersects'
      OnClick = Within1Click
    end
    object PointInPolygon1: TMenuItem
      Caption = 'Point In Polygon'
      OnClick = Within1Click
    end
    object CentroidInPolygon1: TMenuItem
      Caption = 'Centroid In Polygon'
      OnClick = Within1Click
    end
    object Identical1: TMenuItem
      Caption = 'Identical'
      OnClick = Within1Click
    end
  end
end
