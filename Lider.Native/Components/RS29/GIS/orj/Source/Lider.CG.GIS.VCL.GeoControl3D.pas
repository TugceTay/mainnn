//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DKv100.1.37476
// (c)2000-2025 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// ILKER#LIDERYAZILIM.COM-481078-KSVX7UYN-1D12B8B5
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================
{
  Encapsulation of a 3D Control component class.
}

unit VCL.GisControl3D ;
{$HPPEMIT '#pragma link "VCL.GisControl3D"'}

interface

{$INCLUDE GisInclude.inc}

uses
  {$IFDEF MSWINDOWS}
    Winapi.Windows,
  {$ENDIF}
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Math,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.Graphics,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.Forms,
  Vcl.ImgList,

  {$IFDEF GIS_XDK}
    XDK.Core,
  {$ENDIF}

  GisRtl,
  GisInterfaces,
  GisTypes,
  GisFunctions,
  GisResource,
  VCL.GisControlVarious,
  VCL.GisViewerWnd ;


type
  /// <summary>
  ///  Defines which controls are to be displayed within the component.
  /// </summary>
  TGIS_Control3DOption =
  (
    /// <summary>
    /// No options enabled.
    /// </summary>
    NoOptions,
    /// <summary>
    /// Navigation option visible.
    /// </summary>
    ShowNavigation,
    /// <summary>
    ///  Coordinates option visible.
    /// </summary>
    ShowCoordinates,
    /// <summary>
    ///  Reference Point option visible.
    /// </summary>
    ShowReferencePoint,
    /// <summary>
    ///  Lights option visible.
    /// </summary>
    ShowLights,
    /// <summary>
    /// Frame Modes option visible.
    /// </summary>
    ShowFrameModes,
    /// <summary>
    /// Scalings option visible.
    /// </summary>
    ShowScalings,
    /// <summary>
    ///  Floods option visible.
    /// </summary>
    ShowFloods,
    /// <summary>
    ///  Walls option visible.
    /// </summary>
    ShowWalls
  ) ;

  /// <summary>
  ///  set of TGIS_Control3DOption
  /// </summary>
  TGIS_Control3DOptions = set of TGIS_Control3DOption ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisControl3DOptions_NoOptions          = TGIS_Control3DOption.NoOptions          ;
      gisControl3DOptions_ShowNavigation     = TGIS_Control3DOption.ShowNavigation     ;
      gisControl3DOptions_ShowCoordinates    = TGIS_Control3DOption.ShowCoordinates    ;
      gisControl3DOptions_ShowReferencePoint = TGIS_Control3DOption.ShowReferencePoint ;
      gisControl3DOptions_ShowLights         = TGIS_Control3DOption.ShowLights         ;
      gisControl3DOptions_ShowFrameModes     = TGIS_Control3DOption.ShowFrameModes     ;
      gisControl3DOptions_ShowScalings       = TGIS_Control3DOption.ShowScalings       ;
      gisControl3DOptions_ShowFloods         = TGIS_Control3DOption.ShowFloods         ;
      gisControl3DOptions_ShowWalls          = TGIS_Control3DOption.ShowWalls          ;
  {$ENDIF}

type

  /// <summary>
  ///   A component which allows easy control of the 3D view inside a
  ///   TGIS_ViewerWnd component instance.
  /// </summary>
  [ComponentPlatformsAttribute( pfidWindows )]
  TGIS_Control3D = class( TCustomPanel, IGIS_Subscribe )
    private
      imgList16          : TImageList    ;
      imgList32          : TImageList    ;
      imgList16scld      : TImageList    ;
      imgList32scld      : TImageList    ;
      btnUp              : TSpeedButton  ;
      btnLeft            : TSpeedButton  ;
      btnCenter          : TSpeedButton  ;
      btnRight           : TSpeedButton  ;
      btnDown            : TSpeedButton  ;
      btnPlus            : TSpeedButton  ;
      btnMinus           : TSpeedButton  ;
      bvlCoordinates     : TGIS_Bevel    ;
      lblX               : TLabel        ;
      edtX               : TEdit         ;
      lblY               : TLabel        ;
      edtY               : TEdit         ;
      lblZ               : TLabel        ;
      edtZ               : TEdit         ;
      bvlReferencePoint  : TGIS_Bevel    ;
      lblReference       : TLabel        ;
      cmbReference       : TComboBox     ;
      spnReference       : TGIS_SpinEdit ;
      bvlLights          : TGIS_Bevel    ;
      chkLights          : TCheckBox     ;
      lblShadowIntensity : TLabel        ;
      spnShadowIntensity : TGIS_SpinEdit ;
      bvlFrameModes      : TGIS_Bevel    ;
      chkBasePlane       : TCheckBox     ;
      spnBasePlane       : TGIS_SpinEdit ;
      bvlBasePlane       : TGIS_Bevel    ;
      chkWireframe       : TCheckBox     ;
      chkEdges           : TCheckBox     ;
      bvlScalings        : TGIS_Bevel    ;
      lblScaleZ          : TLabel        ;
      spnScaleZ          : TGIS_SpinEdit ;
      lblScaleM          : TLabel        ;
      spnScaleM          : TGIS_SpinEdit ;
      bvlFloods          : TGIS_Bevel    ;
      chkFlood           : TCheckBox     ;
      spnFlood           : TGIS_SpinEdit ;
      bvlWalls           : TGIS_Bevel    ;
      lblWall            : TLabel        ;
      cmbWall            : TComboBox     ;

      tmrMain            : TTimer        ;
      iParentPPI         : Integer ;
      iCurrentPPI        : Integer ;
      parentForm         : TCustomForm ;
    private
      iOperation  : Integer  ;
      iStartTime  : Cardinal ;
      bTimer      : Boolean  ;
      iUponUpdate : Integer  ;

    private
      FGIS_Viewer : TGIS_ViewerWnd ;
      FAutosize   : Boolean ;
      FMode       : TGIS_Viewer3DMode ;
      FOptions    : TGIS_Control3DOptions ;
      FBiDiModeFromTranslation : Boolean ;
    private

      /// <summary>
      ///   Property GIS_Viewer access routine.
      /// </summary>
      procedure fset_GIS_Viewer( const _value : TGIS_ViewerWnd        ) ;

      /// <summary>
      ///   Property AutosizeEx access routine.
      /// </summary>
      procedure fset_AutosizeEx( const _value : Boolean               ) ;

      /// <summary>
      ///   Property Mode access routine.
      /// </summary>
      procedure fset_Mode      ( const _value : TGIS_Viewer3DMode     ) ;

      /// <summary>
      ///   Property Options access routine.
      /// </summary>
      procedure fset_Options   ( const _value : TGIS_Control3DOptions ) ;

      ///  <summary>
      ///    Property Options access routine.
      ///  </summary>
      ///
      procedure fset_BiDiModeFromTranslation ( const _value : Boolean ) ;
      ///  <summary>
      ///    Property Options access routine.
      ///  </summary>
      procedure fset_ParentBiDiMode ( const _value : Boolean ) ;
      procedure fset_BiDiMode ( const _value : TBiDiMode ) ;
    private

      /// <summary>
      ///   Change YI language.
      /// </summary>
      procedure updateLanguage    ;

      procedure updateImages      ;

      procedure doOnResize        ( _sender : TObject
                                  ) ;

      procedure enableTimer ;
      procedure disableTimer ;

      procedure doTimer           (     _delta  : Cardinal
                                  ) ;
      procedure tmrMainTimer      (     _sender : TObject
                                  ) ;
      procedure btnCenterMouseDown(     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Integer      ;
                                        _y      : Integer
                                  ) ;
      procedure btnUpMouseDown    (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Integer      ;
                                        _y      : Integer
                                  ) ;
      procedure btnUpMouseUp      (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Integer      ;
                                        _y      : Integer
                                  ) ;
      procedure btnLeftMouseDown  (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Integer      ;
                                        _y      : Integer
                                  ) ;
      procedure btnLeftMouseUp    (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Integer      ;
                                        _y      : Integer
                                  ) ;
      procedure btnRightMouseDown (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Integer      ;
                                        _y      : Integer
                                  ) ;
      procedure btnRightMouseUp   (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Integer      ;
                                        _y      : Integer
                                  ) ;
      procedure btnDownMouseDown  (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Integer      ;
                                        _y      : Integer
                                  ) ;
      procedure btnDownMouseUp    (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Integer      ;
                                        _y      : Integer
                                  ) ;
      procedure btnPlusMouseDown  (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Integer      ;
                                        _y      : Integer
                                  ) ;
      procedure btnPlusMouseUp    (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Integer      ;
                                        _y      : Integer
                                  ) ;
      procedure btnMinusMouseDown (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Integer      ;
                                        _y      : Integer
                                  ) ;
      procedure btnMinusMouseUp   (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Integer      ;
                                        _y      : Integer
                                  ) ;
      procedure cmbReferenceChange(     _sender : TObject
                                  ) ;
      procedure spnReferenceChange
                                  (     _sender : TObject
                                  ) ;
      procedure chkLightsClick    (     _sender : TObject
                                  ) ;
      procedure spnShadowIntensityChange
                                  (     _sender : TObject
                                  ) ;
      procedure chkBasePlaneClick (     _sender : TObject
                                  ) ;
      procedure spnBasePlaneChange(     _sender : TObject
                                  ) ;
      procedure chkWireframeClick (     _sender : TObject
                                  ) ;
      procedure chkEdgesClick     (     _sender : TObject
                                  ) ;
      procedure spnScaleZChange   (     _sender : TObject
                                  ) ;
      procedure spnScaleMChange   (     _sender : TObject
                                  ) ;
      procedure spnFloodChange    (     _sender : TObject
                                  ) ;
      procedure cmbWallChange     (     _sender : TObject
                                  ) ;
      procedure edtKeyDown        (     _sender : TObject      ;
                                    var _key    : Word         ;
                                        _shift  : TShiftState
                                  ) ;
      function  ppiFix             ( const _value : Integer
                                   ) : Integer ;

    private
      procedure doOnUpdate        (      _sender : TObject
                                  ) ;
    public
      /// <inheritdoc/>
      constructor Create    ( _aowner : TComponent ) ; override;

      /// <inheritdoc/>
      destructor  Destroy         ; override;

      /// <inheritdoc/>
      procedure Paint ; override;

      /// <inheritdoc form="IGIS_Subscribe"/>
      procedure SubscribedEvent (       _sender  : TObject ;
                                        _event   : Integer ;
                                        _context : TObject
                                ) ;

    published

      /// <summary>
      ///   If true the size of the component is being automatically adjusted.
      /// </summary>
      property AutoSize : Boolean read FAutosize write fset_AutosizeEx ;

      /// <summary>
      ///   Defines the purpose of mouse actions.
      /// </summary>
      property Mode : TGIS_Viewer3DMode read FMode write fset_Mode ;

      /// <summary>
      ///   Defines which controls are to be displayed within the component.
      /// </summary>
      property Options : TGIS_Control3DOptions read FOptions write fset_Options ;

      /// <summary>
      ///   Defines which BiDi we are using the one from property or the one from
      ///   translation
      /// </summary>
      property BiDiModeFromTranslation : Boolean read FBiDiModeFromTranslation
                                                 write fset_BiDiModeFromTranslation
                                                 default True ;

      /// <summary>
      ///   Attached TGIS_ViewerWnd component instance.
      /// </summary>
      property GIS_Viewer : TGIS_ViewerWnd read FGIS_Viewer write fset_GIS_Viewer ;

    published // properties derived from base class

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      ///
      property ParentBiDiMode write fset_ParentBiDiMode ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property BiDiMode write fset_BiDiMode ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property Align ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property Anchors ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property BevelInner ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property BevelOuter ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property BevelWidth ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property Ctl3D ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property BorderStyle ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property BorderWidth ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property Color default clWindow ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property Enabled ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property ParentFont ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property Font ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property HelpContext ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property Hint ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property ParentColor ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property ParentCtl3D ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property ParentShowHint ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property PopupMenu ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property TabStop  ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property TabOrder ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property Visible ;

    published // events derived from base class

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnClick
               {$IFDEF GENDOC}
                 : TNotifyEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnDblClick
               {$IFDEF GENDOC}
                 : TNotifyEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnEnter ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnExit ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnKeyDown
               {$IFDEF GENDOC}
                 : TKeyEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnKeyPress
               {$IFDEF GENDOC}
                 : TKeyPressEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnKeyUp
               {$IFDEF GENDOC}
                 : TKeyEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnMouseDown
               {$IFDEF GENDOC}
                 : TMouseEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnMouseMove
               {$IFDEF GENDOC}
                 : TMouseMoveEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnMouseUp
               {$IFDEF GENDOC}
                 : TMouseEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnMouseWheel
               {$IFDEF GENDOC}
                 : TMouseWheelEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnMouseWheelUp
               {$IFDEF GENDOC}
                 : TMouseWheelUpDownEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnMouseWheelDown
               {$IFDEF GENDOC}
                 : TMouseWheelUpDownEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnDragDrop ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnDragOver ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnEndDrag ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnStartDrag ;

    {$IFDEF GIS_XDK}
      public
        {#gendoc:hide:GENXDK}
        XDK : TGIS_ControlXDK ;
    {$ENDIF}

  end ;

  {#gendoc:hide}
  procedure Register;

//##############################################################################
implementation

uses
  GisTypes3D,
  VCL.GisFramework,
  VCL.GisControlHelper ;

{$R GisControl3D_16x16.RES}

type
  T_op = ( opLeft, opRight, opUp, opDown, opPlus, opMinus ) ;

const
  BUTTON_LATENCY = 500 ;

  DESIGN_PPI = 96 ;


  constructor TGIS_Control3D.Create( _aowner : TComponent ) ;
  var
    bmp   : TBitmap ;
  begin
    inherited Create( _aowner ) ;

    ControlStyle := ControlStyle + [csNeedsBorderPaint];
    ParentBackground := False ;
    {$IFDEF LEVEL_RX101_VCL}
      Self.ControlState := [csReadingState] ;
    {$ENDIF}

    BiDiModeFromTranslation := True ;

    iUponUpdate := 0 ;

    bmp := TBitmap.Create ;
    try
      bmp.LoadFromResourceName( hInstance,'TGIS_CONTROL3D_IMAGES_16x16' );

      imgList16 := TImageList.Create( Self ) ;
      imgList16.Width  := 16 ;
      imgList16.Height := 16;
      imgList16.Add( bmp, nil ) ;

      imgList16scld := TImageList.Create( Self ) ;
      imgList16scld.Width  := 16 ;
      imgList16scld.Height := 16;
      imgList16scld.Add( bmp, nil ) ;

      bmp.LoadFromResourceName( hInstance,'TGIS_CONTROL3D_IMAGES_32x32' );

      imgList32 := TImageList.Create( Self ) ;
      imgList32.Width  := 32 ;
      imgList32.Height := 32;
      imgList32.Add( bmp, nil ) ;

      imgList32scld := TImageList.Create( Self ) ;
      imgList32scld.Width  := 32 ;
      imgList32scld.Height := 32;
      imgList32scld.Add( bmp, nil ) ;
    finally
      FreeObject( bmp ) ;
    end ;

    btnUp := TSpeedButton.Create( self ) ;
    with btnUp do begin
      Parent      := Self ;
      Flat        := True ;
      OnMouseDown := btnUpMouseDown ;
      OnMouseUp   := btnUpMouseUp   ;
    end ;

    btnLeft := TSpeedButton.Create( self ) ;
    with btnLeft do begin
      Parent      := Self ;
      Flat        := True ;
      OnMouseDown := btnLeftMouseDown ;
      OnMouseUp   := btnLeftMouseUp   ;
    end ;

    btnCenter  := TSpeedButton.Create( self ) ;
    with btnCenter do begin
      Parent      := Self ;
      Flat        := True ;
      OnMouseDown := btnCenterMouseDown ;
    end ;

    btnRight := TSpeedButton.Create( self ) ;
    with btnRight do begin
      Parent      := Self ;
      Flat        := True ;
      OnMouseDown := btnRightMouseDown ;
      OnMouseUp   := btnRightMouseUp   ;
    end ;

    btnDown := TSpeedButton.Create( self ) ;
    with btnDown do begin
      Parent      := Self  ;
      Flat        := True ;
      OnMouseDown := btnDownMouseDown ;
      OnMouseUp   := btnDownMouseUp   ;
    end ;

    btnPlus := TSpeedButton.Create( self ) ;
    with btnPlus do begin
      Parent      := Self  ;
      Flat        := True ;
      OnMouseDown := btnPlusMouseDown ;
      OnMouseUp   := btnPlusMouseUp   ;
    end ;

    btnMinus := TSpeedButton.Create( self ) ;
    with btnMinus do begin
      Parent      := Self  ;
      Flat        := True ;
      OnMouseDown := btnMinusMouseDown ;
      OnMouseUp   := btnMinusMouseUp   ;
    end ;

    bvlCoordinates := TGIS_Bevel.Create( self ) ;
    with bvlCoordinates do begin
      Parent := Self  ;
      Height := 5 ;
      Shape  := TBevelShape.bsTopLine ;
    end ;

    lblX := TLabel.Create( self ) ;
    with lblX do begin
      Parent     := Self   ;
      Caption    := 'lblX' ;
    end ;

    edtX := TEdit.Create( self ) ;
    with edtX do begin
      Parent     := Self  ;
      OnKeyDown  := edtKeyDown ;
    end ;

    lblY := TLabel.Create( self ) ;
    with lblY do begin
      Parent     := Self   ;
      Caption    := 'lblY' ;
    end ;

    edtY := TEdit.Create( self ) ;
    with edtY do begin
      Parent     := Self  ;
      OnKeyDown  := edtKeyDown ;
    end ;

    lblZ := TLabel.Create( self ) ;
    with lblZ do begin
      Parent     := Self   ;
      Caption    := 'lblZ' ;
    end ;

    edtZ := TEdit.Create( self ) ;
    with edtZ do begin
      Parent     := Self  ;
      OnKeyDown  := edtKeyDown ;
    end ;

    bvlReferencePoint := TGIS_Bevel.Create( self ) ;
    with bvlReferencePoint do begin
      Parent := Self ;
      Height := 5 ;
      Shape  := TBevelShape.bsTopLine ;
    end ;

    lblReference := TLabel.Create( self ) ;
    with lblReference do begin
      Parent     := Self  ;
      Caption    := 'lblReference' ;
    end ;

    cmbReference := TComboBox.Create( self ) ;
    with cmbReference do begin
      Parent   := Self  ;
      OnChange := cmbReferenceChange ;
      Style    := csDropDownList ;
    end ;

    spnReference := TGIS_SpinEdit.Create( self ) ;
    with spnReference do begin
      Parent     := Self ;
      Increment  := 1 ;
      OnChange   := spnReferenceChange ;
      MinVal     := -9999999 ;
      MaxVal     :=  9999999 ;
      Precision  :=  2 ;
      Value      :=  0 ;
    end ;

    bvlLights := TGIS_Bevel.Create( self ) ;
    with bvlLights do begin
      Parent := Self ;
      Height := 5 ;
      Shape  := TBevelShape.bsTopLine ;
    end ;

    chkLights := TCheckBox.Create( self ) ;
    with chkLights do begin
      Parent     := Self  ;
      OnClick    := chkLightsClick ;
      Caption    := 'chkLights' ;
    end ;

    lblShadowIntensity := TLabel.Create( self ) ;
    with lblShadowIntensity do begin
      Parent     := Self  ;
      Caption    := 'lblShadowIntensity' ;
    end ;

    spnShadowIntensity := TGIS_SpinEdit.Create( self ) ;
    with spnShadowIntensity do begin
      Parent     := Self ;
      Increment  := 5 ;
      OnChange   := spnShadowIntensityChange ;
      MinVal     :=   0 ;
      MaxVal     := 100 ;
      Value      :=  60 ;
    end ;

    bvlFrameModes := TGIS_Bevel.Create( self ) ;
    with bvlFrameModes do begin
      Parent := Self ;
      Height := 5 ;
      Shape  := TBevelShape.bsTopLine ;
    end ;

    chkBasePlane := TCheckBox.Create( self ) ;
    with chkBasePlane do begin
      Parent     := Self ;
      OnClick    := chkBasePlaneClick ;
      Caption    := 'chkBasePlane' ;
    end ;

    spnBasePlane := TGIS_SpinEdit.Create( self ) ;
    with spnBasePlane do begin
      Parent     := Self ;
      Increment  := 0.1 ;
      OnChange   := spnBasePlaneChange ;
      MinVal     := -9999999 ;
      MaxVal     :=  9999999 ;
      Precision  :=  2 ;
      Value      :=  0 ;
    end ;

    bvlBasePlane := TGIS_Bevel.Create( self ) ;
    with bvlBasePlane do begin
      Parent := Self ;
      Height := 5 ;
      Shape  := TBevelShape.bsTopLine ;
    end ;

    chkWireframe := TCheckBox.Create( self ) ;
    with chkWireframe do begin
      Parent     := Self ;
      OnClick    := chkWireframeClick ;
      Caption    := 'chkWireframe' ;
    end ;

    chkEdges := TCheckBox.Create( self ) ;
    with chkEdges do begin
      Parent     := Self ;
      OnClick    := chkEdgesClick ;
      Caption    := 'chkEdges' ;
    end ;

    bvlScalings := TGIS_Bevel.Create( self ) ;
    with bvlScalings do begin
      Parent := Self ;
      Height := 5 ;
      Shape  := TBevelShape.bsTopLine ;
    end ;

    lblScaleZ := TLabel.Create( self ) ;
    with lblScaleZ do begin
      Parent     := Self  ;
      Caption    := 'lblScaleZ' ;
    end ;

    spnScaleZ := TGIS_SpinEdit.Create( self ) ;
    with spnScaleZ do begin
      Parent     := Self ;
      Increment  := 10 ;
      OnChange   := spnScaleZChange ;
      MinVal     := -999 ;
      MaxVal     :=  999 ;
      Value      :=  100 ;
    end ;

    lblScaleM := TLabel.Create( self ) ;
    with lblScaleM do begin
      Parent     := Self  ;
      Caption    := 'lblScaleM' ;
    end ;

    spnScaleM := TGIS_SpinEdit.Create( self ) ;
    with spnScaleM do begin
      Parent     := Self ;
      Increment  := 10 ;
      OnChange   := spnScaleMChange ;
      MinVal     := -999 ;
      MaxVal     :=  999 ;
      Value      :=  100 ;
    end ;

    bvlFloods := TGIS_Bevel.Create( self ) ;
    with bvlFloods do begin
      Parent := Self ;
      Height := 5 ;
      Shape  := TBevelShape.bsTopLine ;
    end ;

    chkFlood := TCheckBox.Create( self ) ;
    with chkFlood do begin
      Parent     := Self  ;
      OnClick    := spnFloodChange ;
      Caption    := 'chkFlood' ;
    end ;

    spnFlood := TGIS_SpinEdit.Create( self ) ;
    with spnFlood do begin
      Parent     := Self ;
      Increment  := 0.1 ;
      OnChange   := spnFloodChange ;
      MinVal     := -9999999 ;
      MaxVal     :=  9999999 ;
      Precision  :=  2 ;
      Value      :=  0 ;
    end ;

    bvlWalls := TGIS_Bevel.Create( self ) ;
    with bvlWalls do begin
      Parent := Self  ;
      Height := 5 ;
      Shape  := TBevelShape.bsTopLine ;
    end ;

    lblWall := TLabel.Create( self ) ;
    with lblWall do begin
      Parent     := Self  ;
      Caption    := 'lblWall' ;
    end ;

    cmbWall := TComboBox.Create( self ) ;
    with cmbWall do begin
      Parent   := Self ;
      OnChange := cmbWallChange ;
      Style    := csDropDownList ;
    end ;

    tmrMain := TTimer.Create( self ) ;
    with tmrMain do begin
      Interval := 1     ;
      OnTimer  := tmrMainTimer ;
      Enabled  := False ;
    end;

    FOptions := [ TGIS_Control3DOption.ShowNavigation,
                  TGIS_Control3DOption.ShowCoordinates,
                  TGIS_Control3DOption.ShowReferencePoint,
                  TGIS_Control3DOption.ShowLights,
                  TGIS_Control3DOption.ShowFrameModes,
                  TGIS_Control3DOption.ShowScalings,
                  TGIS_Control3DOption.ShowFloods,
                  TGIS_Control3DOption.ShowWalls
                ] ;

    Mode := TGIS_Viewer3DMode.CameraPosition ;

    OnResize := doOnResize ;

    Width  := 100 ;
    Height := 400 ;

    iParentPPI  := DESIGN_PPI ;
    iCurrentPPI := 0 ;

    ControlState := [] ;

    LocalizedNotification.Subscribe(
      updateLanguage
    ) ;

    updateImages ;
  end ;

  destructor TGIS_Control3D.Destroy ;
  begin
    if Assigned( FGIS_Viewer ) then
      FGIS_Viewer.UnSubscribe( Self ) ;

    FreeObject( imgList16 );
    FreeObject( imgList16scld );
    FreeObject( imgList32 );
    FreeObject( imgList32scld );

    inherited ;
  end ;


  procedure TGIS_Control3D.fset_GIS_Viewer(
    const _value : TGIS_ViewerWnd
  ) ;
  begin
    if FGIS_Viewer = _value then exit ;

    if Assigned( FGIS_Viewer ) then
      FGIS_Viewer.UnSubscribe( Self ) ;
    FGIS_Viewer := _value ;
    if Assigned( FGIS_Viewer ) then
      FGIS_Viewer.Subscribe( Self ) ;
  end ;

  procedure TGIS_Control3D.fset_AutosizeEx(
    const _value : Boolean
  ) ;
  begin
    FAutosize := _value ;
    if csReading in ComponentState then
      exit;
    Resize ;
  end ;

  procedure TGIS_Control3D.fset_Mode(
    const _value : TGIS_Viewer3DMode
  ) ;
  begin
    case _value of
      TGIS_Viewer3DMode.CameraPosition :
        begin
          FMode := _value ;
        end ;
      TGIS_Viewer3DMode.CameraXYZ      :
        begin
          FMode := _value ;
        end ;
      TGIS_Viewer3DMode.CameraRotation :
        begin
          FMode := _value ;
        end ;
      TGIS_Viewer3DMode.SunPosition    :
        begin
          FMode := _value ;
        end ;
      else
        begin
          exit ;
        end ;
    end;

    FMode := _value ;

    btnCenter.Glyph := nil ;

    case FMode of
      TGIS_Viewer3DMode.CameraPosition :
        begin
          btnCenter.Hint := _rsrc( GIS_RS_3D_ROTATION ) ;
        end ;
      TGIS_Viewer3DMode.CameraXYZ      :
        begin
          btnCenter.Hint := _rsrc( GIS_RS_3D_XYZ_POSITION ) ;
        end ;
      TGIS_Viewer3DMode.CameraRotation :
        begin
          btnCenter.Hint := _rsrc( GIS_RS_3D_CAMERA_ANGLE ) ;
        end ;
      TGIS_Viewer3DMode.SunPosition    :
        begin
          btnCenter.Hint := _rsrc( GIS_RS_3D_SUN_POSITION ) ;
        end ;
      else
        begin
          btnCenter.Hint := _rsrc( GIS_RS_3D_ROTATION ) ;
        end ;
    end;

    updateImages ;

    case FMode of
      TGIS_Viewer3DMode.CameraPosition :
        begin
          lblX.Caption := _rsrc( GIS_RS_3D_COORDINATE_VERTICAL ) ;
          lblY.Caption := _rsrc( GIS_RS_3D_COORDINATE_HORIZONTAL ) ;
          lblZ.Caption := _rsrc( GIS_RS_3D_COORDINATE_DISTANCE ) ;

          lblX.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          edtX.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          lblY.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          edtY.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          lblZ.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          edtZ.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
        end ;

      TGIS_Viewer3DMode.CameraXYZ :
        begin
          lblX.Caption := _rsrc( GIS_RS_3D_COORDINATE_X ) ;
          lblY.Caption := _rsrc( GIS_RS_3D_COORDINATE_Y ) ;
          lblZ.Caption := _rsrc( GIS_RS_3D_COORDINATE_Z ) ;

          lblX.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          edtX.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          lblY.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          edtY.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          lblZ.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          edtZ.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
        end ;

      TGIS_Viewer3DMode.CameraRotation :
        begin
          lblX.Caption := _rsrc( GIS_RS_3D_COORDINATE_VERTICAL ) ;
          lblY.Caption := _rsrc( GIS_RS_3D_COORDINATE_TILT ) ;
          lblZ.Caption := _rsrc( GIS_RS_3D_COORDINATE_HORIZONTAL ) ;

          lblX.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          edtX.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          lblY.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          edtY.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          lblZ.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          edtZ.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
        end ;

      TGIS_Viewer3DMode.SunPosition :
        begin
          lblX.Caption := _rsrc( GIS_RS_3D_COORDINATE_VERTICAL ) ;
          lblY.Caption := _rsrc( GIS_RS_3D_COORDINATE_HORIZONTAL ) ;

          lblX.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          edtX.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          lblY.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          edtY.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          lblZ.Visible := False ;
          edtZ.Visible := False ;
        end ;
      else
        begin
          Assert( False );
        end ;
    end ;

    doOnUpdate( Self ) ;
  end ;

  procedure TGIS_Control3D.fset_Options(
    const _value : TGIS_Control3DOptions
  ) ;
  begin
    FOptions := _value ;

    if csReading in ComponentState then
      exit;
    Resize ;
  end ;

  procedure TGIS_Control3D.fset_BiDiModeFromTranslation(
    const _value : Boolean
  ) ;
  begin
    if _value <> FBiDiModeFromTranslation then begin
      FBiDiModeFromTranslation := _value ;
      ParentBiDiMode := not FBiDiModeFromTranslation ;
      if FBiDiModeFromTranslation then
      begin
        if _rsbidi then
        begin
          if BiDiMode <> TBiDiMode.bdRightToLeft then
            inherited BiDiMode := TBiDiMode.bdRightToLeft
        end
        else
        begin
          if BiDiMode <> TBiDiMode.bdLeftToRight then
            inherited BiDiMode := TBiDiMode.bdLeftToRight
        end;
      end;
    end;
  end ;

  procedure TGIS_Control3D.fset_ParentBiDiMode(
    const _value : Boolean
  ) ;
  begin
    if _value <> ParentBiDiMode then begin
      inherited ParentBiDiMode := _value ;
      if ParentBiDiMode = true then
        FBiDiModeFromTranslation := not ParentBiDiMode ;
    end;
  end ;

  procedure TGIS_Control3D.fset_BiDiMode ( const _value : TBiDiMode ) ;
  begin
    if not FBiDiModeFromTranslation then
      inherited BiDiMode := _value ;
  end;

  procedure TGIS_Control3D.updateLanguage ;
  begin
    lblReference.Caption       := _rsrc( GIS_RS_3D_REFERENCE_POINT ) ;
    chkLights.Caption          := _rsrc( GIS_RS_3D_LIGHTS ) ;
    lblShadowIntensity.Caption := _rsrc( GIS_RS_3D_SHADOW_INTESITY ) ;
    chkBasePlane.Caption       := _rsrc( GIS_RS_3D_BASEPLANE ) ;
    chkWireframe.Caption       := _rsrc( GIS_RS_3D_WIREFRAME ) ;
    chkEdges.Caption           := _rsrc( GIS_RS_3D_EDGES ) ;
    lblScaleZ.Caption          := _rsrc( GIS_RS_3D_SCALE_Z ) ;
    lblScaleM.Caption          := _rsrc( GIS_RS_3D_SCALE_M ) ;
    chkFlood.Caption           := _rsrc( GIS_RS_3D_FLOOD ) ;
    lblWall.Caption            := _rsrc( GIS_RS_3D_WALL ) ;

    if cmbReference.Items.Count <= 0 then begin
      cmbReference.Items.Add( '' ) ;
      cmbReference.Items.Add( '' ) ;
      cmbReference.Items.Add( '' ) ;
      cmbReference.Items.Add( '' ) ;
      cmbReference.Items.Add( '' ) ;
      cmbReference.Items.Add( '' ) ;
    end;
    cmbReference.Items[ 0] := _rsrc( GIS_RS_3D_REFERENCE_BASE       ) ;
    cmbReference.Items[ 1] := _rsrc( GIS_RS_3D_REFERENCE_ZERO       ) ;
    cmbReference.Items[ 2] := _rsrc( GIS_RS_3D_REFERENCE_LOWEST     ) ;
    cmbReference.Items[ 3] := _rsrc( GIS_RS_3D_REFERENCE_HIGHEST    ) ;
    cmbReference.Items[ 4] := _rsrc( GIS_RS_3D_REFERENCE_ON_DEM     ) ;
    cmbReference.Items[ 5] := _rsrc( GIS_RS_3D_REFERENCE_FLY_ON_DEM ) ;

    if cmbWall.Items.Count <= 0 then begin
      cmbWall.Items.Add( '' ) ;
      cmbWall.Items.Add( '' ) ;
      cmbWall.Items.Add( '' ) ;
    end ;
    cmbWall.Items[ 0] := _rsrc( GIS_RS_3D_WALLS_OFF     ) ;
    cmbWall.Items[ 1] := _rsrc( GIS_RS_3D_WALLS_COLOR   ) ;
    cmbWall.Items[ 2] := _rsrc( GIS_RS_3D_WALLS_TEXTURE ) ;
  end;

  procedure TGIS_Control3D.Paint;
  begin
    if not assigned( parentForm ) then
      parentForm := GetParentForm( self ) ;

    if assigned( parentForm ) and ( parentForm is TForm ) then begin
      {$IFDEF LEVEL_RX101_VCL}
        iParentPPI := TForm(parentForm).Monitor.PixelsPerInch ;
      {$ELSE}
        iParentPPI := Screen.PixelsPerInch ;
      {$ENDIF}
    end ;

    if iParentPPI <> iCurrentPPI  then begin
      doOnResize( self ) ;
      exit ;
    end;

    inherited ;
  end ;

  function TGIS_Control3D.ppifix(
    const _value : Integer
  ) : Integer ;
  begin
    Result := MulDiv( _value, iParentPPI, DESIGN_PPI ) ;
  end;

  procedure TGIS_Control3D.updateImages ;
  begin
    if iParentPPI <> iCurrentPPI then begin
      iCurrentPPI := iParentPPI ;

      GisScaleImageList( imgList16, imgList16scld, iParentPPI, DESIGN_PPI );
      GisScaleImageList( imgList32, imgList32scld, iParentPPI, DESIGN_PPI );
    end ;

    with btnUp do begin
      Glyph := nil ;
      imgList16scld.GetBitmap( 4, Glyph ) ;
    end ;

    with btnLeft do begin
      Glyph := nil ;
      imgList16scld.GetBitmap( 2, Glyph ) ;
    end ;

    with btnRight do begin
      Glyph := nil ;
      imgList16scld.GetBitmap( 3, Glyph ) ;
    end ;

    with btnDown do begin
      Glyph := nil ;
      imgList16scld.GetBitmap( 5, Glyph ) ;
    end ;

    with btnPlus do begin
      Glyph := nil ;
      imgList16scld.GetBitmap( 0, Glyph ) ;
    end ;

    with btnMinus do begin
      Glyph := nil ;
      imgList16scld.GetBitmap( 1, Glyph ) ;
    end ;

    case Mode of
      TGIS_Viewer3DMode.CameraPosition :
        begin
          btnCenter.Glyph := nil ;
          imgList32scld.GetBitmap( 1, btnCenter.Glyph ) ;
        end ;
      TGIS_Viewer3DMode.CameraXYZ      :
        begin
          btnCenter.Glyph := nil ;
          imgList32scld.GetBitmap( 2, btnCenter.Glyph ) ;
        end ;
      TGIS_Viewer3DMode.CameraRotation :
        begin
          btnCenter.Glyph := nil ;
          imgList32scld.GetBitmap( 3, btnCenter.Glyph ) ;
        end ;
      TGIS_Viewer3DMode.SunPosition    :
        begin
          btnCenter.Glyph := nil ;
          imgList32scld.GetBitmap( 0, btnCenter.Glyph ) ;
        end ;
      else
        begin
          btnCenter.Glyph := nil ;
          imgList32scld.GetBitmap( 1, btnCenter.Glyph ) ;
        end ;
    end;
  end;

  procedure TGIS_Control3D.doOnResize(
    _sender : TObject
  ) ;
  const
    BTN1_SIZE   = 20 ;
    BTN2_SIZE   = 40 ;
    BTN1_GAP    =  0 ;
    BTN1_GAP2   =  5 ;
    TOP_MARGIN  = 10 ;
    LEFT_MARGIN =  5 ;
    OFFWINDOW   = -100000 ;
    MINUS_GAP   = 0 ;
  var
    l      : Integer ;
    l_lbl  : Integer ;
    t      : Integer ;
    w      : Integer ;
    center : Integer ;
    cnt    : Integer ;
  begin
    updateLanguage ;

    updateImages ;

    cnt := 0 ; // number of visible items
    center := self.ClientWidth div 2  ;
    t := ppiFix( TOP_MARGIN ) ;
    l := center - ppiFix( BTN1_SIZE ) div 2 ;

    // Navigation panel
    if TGIS_Control3DOption.ShowNavigation in Options then begin
      with btnUp do begin
        Visible      := True ;
        Width        := ppiFix( BTN1_SIZE ) ;
        Height       := ppiFix( BTN1_SIZE ) ;
        Top          := t ;
        Left         := l ;
      end ;
      t := t + ppiFix( BTN1_SIZE ) + ppiFix( BTN1_GAP  );

      l := center - ppiFix( BTN2_SIZE  )div 2 ;
      l := l - ppiFix( BTN1_SIZE ) - ppiFix( BTN1_GAP ) ;

      with btnLeft do begin
        Visible      := True ;
        Width        := ppiFix( BTN1_SIZE ) ;
        Height       := ppiFix( BTN1_SIZE ) ;
        Top          := t + ( ppiFix( BTN2_SIZE-BTN1_SIZE )  ) div 2 ;
        Left         := l ;
      end ;
      l := l + ppiFix( BTN1_SIZE ) + ppiFix( BTN1_GAP ) ;

      with btnCenter do begin
        Visible      := True ;
        Width        := ppiFix( BTN2_SIZE ) ;
        Height       := ppiFix( BTN2_SIZE ) ;
        Top          := t ;
        Left         := l ;
      end ;
      l := l + ppiFix( BTN2_SIZE ) + ppiFix( BTN1_GAP ) ;

      with btnRight do begin
        Visible      := True ;
        Width        := ppiFix( BTN1_SIZE ) ;
        Height       := ppiFix( BTN1_SIZE ) ;
        Top          := t + ( ppiFix( BTN2_SIZE )- ppiFix( BTN1_SIZE ) ) div 2 ;
        Left         := l ;
      end ;
      t := t + ppiFix( BTN2_SIZE ) + ppiFix( BTN1_GAP ) ;

      l := center - ppiFix( BTN1_SIZE ) div 2 ;

      with btnDown do begin
        Visible      := True ;
        Width        := ppiFix( BTN1_SIZE ) ;
        Height       := ppiFix( BTN1_SIZE ) ;
        Top          := t ;
        Left         := l ;
      end ;
      t := t + ppiFix( BTN1_SIZE ) + ppiFix( BTN1_GAP2 ) ;

      with btnPlus do begin
        Visible      := True ;
        Width        := ppiFix( BTN1_SIZE ) ;
        Height       := ppiFix( BTN1_SIZE ) ;
        Top          := t ;
        Left         := l ;
      end ;
      t := t + ppiFix( BTN1_SIZE ) + ppiFix( BTN1_GAP ) ;

      with btnMinus do begin
        Visible      := True ;
        Width        := ppiFix( BTN1_SIZE ) ;
        Height       := ppiFix( BTN1_SIZE ) ;
        Top          := t ;
        Left         := l ;
      end ;
      t := t + ppiFix( BTN1_SIZE ) + ppiFix( BTN1_GAP ) ;

      t := t + ppiFix( BTN1_GAP2 ) ;

      Inc( cnt ) ;
    end
    else begin
      with btnUp do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;

      with btnLeft do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;

      with btnCenter do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;

      with btnRight do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;

      with btnDown do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;

      with btnPlus do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;

      with btnMinus do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;
    end;

    l := ppiFix( LEFT_MARGIN ) ;
    l_lbl := l ;
    w := self.ClientWidth - ppiFix( LEFT_MARGIN ) - ppiFix( LEFT_MARGIN ) ;

    // Coordinates panel
    if TGIS_Control3DOption.ShowCoordinates in Options then begin
      with bvlCoordinates do begin
        Visible    := cnt > 0 ;
        Width      := Self.ClientWidth ;
        Height     := ppiFix( BTN1_GAP2 )  ;
        Top        := t ;
        if Visible then
          Left     := self.ClientRect.Left
        else
          Left     := OFFWINDOW ; // for designer purposes
      end ;
      if cnt > 0 then
        t := t + bvlCoordinates.Height ;

      with lblX do begin
        Visible      := True ;
        Width        := w ;
        Top          := t ;
        Left         := l_lbl ;
      end ;
      t := t + lblX.Height ;

      with edtX do begin
        Visible      := True ;
        Width        := w ;
        Top          := t ;
        Left         := l ;
      end ;
      t := t + edtX.Height ;

      with lblY do begin
        Visible      := True ;
        Width        := w ;
        Top          := t ;
        Left         := l_lbl ;
      end ;
      t := t + lblY.Height ;

      with edtY do begin
        Visible      := True ;
        Width        := w ;
        Top          := t ;
        Left         := l ;
      end ;
      t := t + edtY.Height ;//+ BTN1_GAP2 ;

      with lblZ do begin
        Visible      := True ;
        Width        := w ;
        Top          := t ;
        Left         := l_lbl ;
      end ;
      t := t + lblZ.Height ;

      with edtZ do begin
        Visible      := True ;
        Width        := w ;
        Top          := t ;
        Left         := l ;
      end ;
      t := t + edtZ.Height + BTN1_GAP2 ;

      Inc( cnt ) ;
    end
    else begin
      with bvlCoordinates do begin
        Visible    := False ;
        Left       := OFFWINDOW ; // for designer purposes
      end ;

      with lblX do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;

      with edtX do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;

      with lblY do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;

      with edtY do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;

      with lblZ do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;

      with edtZ do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;
    end;

    // Reference point panel
    if TGIS_Control3DOption.ShowReferencePoint in Options then begin
      with bvlReferencePoint do begin
        Visible    := cnt > 0 ;
        Width      := Self.ClientWidth ;
        Height     := ppiFix( BTN1_GAP2 ) ;
        Top        := t ;
        if Visible then
          Left     := self.ClientRect.Left
        else
          Left     := OFFWINDOW ; // for designer purposes
      end ;
      if cnt > 0 then
        t := t + bvlReferencePoint.Height ;

      with lblReference do begin
        Visible      := True ;
        Width        := w ;
        Top          := t ;
        Left         := l_lbl ;
      end ;
      t := t + lblReference.Height ;

      with cmbReference do begin
        Visible      := True ;
        Width        := w ;
        Top          := t  ;
        Left         := l  ;
      end ;
      t := t + cmbReference.Height ;//+ BTN1_GAP2 ;

      with spnReference do begin
        Visible      := True ;
        Width        := w  ;
        Top          := t  ;
        Left         := l  ;
      end ;
      t := t + spnReference.Height + BTN1_GAP2 ;

      Inc( cnt ) ;
    end
    else begin
      with bvlReferencePoint do begin
        Visible    := False ;
        Left       := OFFWINDOW ; // for designer purposes
      end ;

      with lblReference do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;

      with cmbReference do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;

      with spnReference do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;
    end;

    // Lights panel
    if TGIS_Control3DOption.ShowLights in Options then begin
      with bvlLights do begin
        Visible    := cnt > 0 ;
        Width      := Self.ClientWidth ;
        Height     := ppiFix( BTN1_GAP2 ) ;
        Top        := t ;
        if Visible then
          Left   := self.ClientRect.Left
        else
          Left     := OFFWINDOW ; // for designer purposes
      end ;
      if cnt > 0 then
        t := t + bvlLights.Height ;

      with chkLights do begin
        Visible      := True ;
        Width        := w ;
        Top          := t ;
        Left         := l ;
      end ;
      t := t + chkLights.Height ;//+ BTN1_GAP2 ;

      with lblShadowIntensity do begin
        Visible      := True ;
        Width        := w ;
        Top          := t ;
        Left         := l_lbl ;
      end ;
      t := t + lblShadowIntensity.Height ;

      with spnShadowIntensity do begin
        Visible      := True ;
        Top          := t  ;
        PlaceControl( BiDiMode, nil, spnShadowIntensity, l, ppiFix(50) );
      end ;
      t := t + spnShadowIntensity.Height + BTN1_GAP2 ;

      Inc( cnt ) ;
    end
    else begin
      with bvlLights do begin
        Visible    := False ;
        Left       := OFFWINDOW ; // for designer purposes
      end ;

      with chkLights do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;

      with lblShadowIntensity do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;

      with spnShadowIntensity do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;
    end;

    // Wireframes and edges panel
    if TGIS_Control3DOption.ShowFrameModes in Options then begin
      with bvlFrameModes do begin
        Visible    := cnt > 0 ;
        Width      := Self.ClientWidth ;
        Height     := ppiFix( BTN1_GAP2 ) ;
        Top        := t ;
        if Visible then
          Left   := self.ClientRect.Left
        else
          Left     := OFFWINDOW ; // for designer purposes
      end ;
      if cnt > 0 then
        t := t + bvlFrameModes.Height ;

      with chkBasePlane do begin
        Visible      := True ;
        Width        := w ;
        Top          := t ;
        Left         := l ;
      end ;
      t := t + chkBasePlane.Height ;//+ BTN1_GAP2 ;

      with spnBasePlane do begin
        Visible      := True ;
        Width        := w ;
        Top          := t  ;
        Left         := l  ;
      end ;
      t := t + spnBasePlane.Height + BTN1_GAP2 ;

      with bvlBasePlane do begin
        Visible    := cnt > 0 ;
        Width      := Self.ClientWidth ;
        Height     := ppiFix( BTN1_GAP2 ) ;
        Top        := t ;
        if Visible then
          Left   := self.ClientRect.Left
        else
          Left     := OFFWINDOW ; // for designer purposes
      end ;
      if cnt > 0 then
        t := t + bvlBasePlane.Height ;

      with chkWireframe do begin
        Visible      := True ;
        Width        := w ;
        Top          := t ;
        Left         := l ;
      end ;
      t := t + chkWireframe.Height ;//+ BTN1_GAP2 ;

      with chkEdges do begin
        Visible      := True ;
        Width        := w ;
        Top          := t ;
        Left         := l ;
      end ;
      t := t + chkEdges.Height + BTN1_GAP2 ;

      Inc( cnt ) ;
    end
    else begin
      with bvlFrameModes do begin
        Visible    := False ;
        Left       := OFFWINDOW ; // for designer purposes
      end ;

      with chkBasePlane do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;

      with spnBasePlane do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;

      with bvlBasePlane do begin
        Visible    := False ;
        Left       := OFFWINDOW ; // for designer purposes
      end ;

      with chkWireframe do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;

      with chkEdges do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;
    end;

    // Scallings panel
    if TGIS_Control3DOption.ShowScalings in Options then begin
      with bvlScalings do begin
        Visible    := cnt > 0 ;
        Width      := Self.ClientWidth ;
        Height     := ppiFix( BTN1_GAP2 ) ;
        Top        := t ;
        if Visible then
          Left   := self.ClientRect.Left
        else
          Left     := OFFWINDOW ; // for designer purposes
      end ;
      if cnt > 0 then
        t := t + bvlScalings.Height ;

      with lblScaleZ do begin
        Visible      := True ;
        Width        := w ;
        Top          := t ;
        Left         := l_lbl ;
      end ;
      t := t + lblScaleZ.Height ;

      with spnScaleZ do begin
        Visible      := True ;
        Top          := t  ;
        PlaceControl( BiDiMode, nil, spnScaleZ, l, ppiFix(50) );
      end ;
      t := t + spnScaleZ.Height + BTN1_GAP2 ;

      with lblScaleM do begin
        Visible      := True ;
        Width        := w ;
        Top          := t ;
        Left         := l_lbl ;
      end ;
      t := t + lblScaleM.Height ;

      with spnScaleM do begin
        Visible      := True ;
        Top          := t  ;
        PlaceControl( BiDiMode, nil, spnScaleM, l, ppiFix(50) );
      end ;
      t := t + spnScaleM.Height + BTN1_GAP2 ;

      Inc( cnt ) ;
    end
    else begin
      with bvlScalings do begin
        Visible    := False ;
        Left       := OFFWINDOW ; // for designer purposes
      end ;

      with lblScaleZ do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;

      with spnScaleZ do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;

      with lblScaleM do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;

      with spnScaleM do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;
    end ;

    // Floods panel
    if TGIS_Control3DOption.ShowFloods in Options then begin
      with bvlFloods do begin
        Visible    := cnt > 0 ;
        Width      := Self.ClientWidth ;
        Height     := ppiFix( BTN1_GAP2 ) ;
        Top        := t ;
        if Visible then
          Left   := self.ClientRect.Left
        else
          Left     := OFFWINDOW ; // for designer purposes
      end ;
      if cnt > 0 then
        t := t + bvlFloods.Height ;

      with chkFlood do begin
        Visible      := True ;
        Width        := w ;
        Top          := t  ;
        Left         := l  ;
      end ;
      t := t + chkFlood.Height ;

      with spnFlood do begin
        Visible      := True ;
        Width        := w ;
        Top          := t  ;
        Left         := l  ;
      end ;
      t := t + spnFlood.Height + BTN1_GAP2 ;

      Inc( cnt ) ;
    end
    else begin
      with bvlFloods do begin
        Visible    := False ;
        Left       := OFFWINDOW ; // for designer purposes
      end ;

      with chkFlood do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;

      with spnFlood do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;
    end;

    // Walls panel
    if TGIS_Control3DOption.ShowWalls in Options then begin
      with bvlWalls do begin
        Visible    := cnt > 0 ;
        Width      := Self.ClientWidth ;
        Height     := ppiFix( BTN1_GAP2 ) ;
        Top        := t ;
        if Visible then
          Left   := self.ClientRect.Left
        else
          Left     := OFFWINDOW ; // for designer purposes
      end ;
      if cnt > 0 then
        t := t + bvlWalls.Height ;

      with lblWall do begin
        Visible      := True ;
        Width        := w ;
        Top          := t  ;
        Left         := l_lbl  ;
      end ;
      t := t + lblWall.Height ;

      with cmbWall do begin
        Visible      := True ;
        Width        := w ;
        Top          := t  ;
        Left         := l  ;
      end ;
      t := t + cmbWall.Height + ppiFix( BTN1_GAP2 ) ;

      // Inc( cnt ) ;
    end
    else begin
      with bvlWalls do begin
        Visible    := False ;
        Left       := OFFWINDOW ; // for designer purposes
      end ;

      with lblWall do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;

      with cmbWall do begin
        Visible      := False ;
        Left         := OFFWINDOW ; // for designer purposes
      end ;
    end;

    if AutoSize then
      Height := t ;
  end;

  procedure TGIS_Control3D.enableTimer ;
  begin
    if not Assigned( GIS_Viewer ) or not Assigned( GIS_Viewer.Viewer3D ) then exit ;

    iStartTime := GetTickCount ;
    bTimer := True ;

    GIS_Viewer.Viewer3D.LockUpdates() ;
    tmrMain.Enabled := True ;
  end;

  procedure TGIS_Control3D.disableTimer ;
  begin
    if not Assigned( GIS_Viewer ) or not Assigned( GIS_Viewer.Viewer3D ) then exit ;

    tmrMain.Enabled := False ;
    bTimer := False ;

    if ( GetTickCount - iStartTime ) < BUTTON_LATENCY then
      doTimer( 8000 ) ;

    GIS_Viewer.Viewer3D.UnlockUpdates() ;
  end;

  procedure TGIS_Control3D.doTimer(
    _delta  : Cardinal
  ) ;
  var
    factor : Double  ;

    procedure do_pos ;
    var
      val : Double       ;
      cam : TGIS_Point3D ;
    begin
      if GIS_Viewer.Viewer3D.OrthoView then exit;
      val := factor * DegToRad( 15 ) ;

      cam := GIS_Viewer.Viewer3D.CameraPosition ;
      case T_op( iOperation ) of
        T_op.opLeft  :
          begin
            cam.Y := cam.Y - val ;
          end ;
        T_op.opRight :
          begin
            cam.Y := cam.Y + val ;
          end ;
        T_op.opUp    :
          begin
            cam.X := cam.X + val ;
          end ;
        T_op.opDown :
          begin
            cam.X := cam.X - val ;
          end;
      end;
      GIS_Viewer.Viewer3D.CameraPosition := cam ;
    end ;

    procedure do_posex ;
    var
      val : Double       ;
      cam : TGIS_Point3D ;
    begin
      val := factor *
             GIS_Viewer.Viewer3D.PixelSize.X * GIS_Viewer.Width
             / 10 ;

      cam := GIS_Viewer.Viewer3D.CameraPositionEx ;
      case T_op( iOperation ) of
        T_op.opLeft  :
          begin
            cam.X := cam.X - val ;
          end ;
        T_op.opRight :
          begin
            cam.X := cam.X + val ;
          end ;
        T_op.opUp    :
          begin
            cam.Y := cam.Y + val ;
          end ;
        T_op.opDown :
          begin
            cam.Y := cam.Y - val ;
          end;
      end;
      GIS_Viewer.Viewer3D.CameraPositionEx := cam ;
    end ;

    procedure do_rot ;
    var
      val : Double       ;
      cam : TGIS_Point3D ;
    begin
      if GIS_Viewer.Viewer3D.OrthoView then exit;
      val := factor * DegToRad( 15 ) ;

      cam := GIS_Viewer.Viewer3D.CameraRotation ;
      case T_op( iOperation ) of
        T_op.opLeft  :
          begin
            cam.Z := cam.Z - val ;
          end ;
        T_op.opRight :
          begin
            cam.Z := cam.Z + val ;
          end ;
        T_op.opUp    :
          begin
            cam.X := cam.X + val ;
          end ;
        T_op.opDown :
          begin
            cam.X := cam.X - val ;
          end;
      end;
      GIS_Viewer.Viewer3D.CameraRotation := cam ;
    end ;

    procedure do_sun ;
    var
      val : Double     ;
      cam : TGIS_Point ;
    begin
      val := factor * DegToRad( 15 ) ;

      cam := GIS_Viewer.Viewer3D.SunPosition ;
      case T_op( iOperation ) of
        T_op.opLeft  :
          begin
            cam.Y := cam.Y - val ;
          end ;
        T_op.opRight :
          begin
            cam.Y := cam.Y + val ;
          end ;
        T_op.opUp    :
          begin
            cam.X := cam.X + val ;
          end ;
        T_op.opDown :
          begin
            cam.X := cam.X - val ;
          end;
      end;
      GIS_Viewer.Viewer3D.SunPosition := cam ;
    end ;

    procedure do_scale ;
    var
      val  : Double       ;
      cam  : TGIS_Point3D ;
      flag : Boolean      ;
    begin
      val := factor * 1/2 ;

      cam := GIS_Viewer.Viewer3D.CameraPosition ;

      flag := False ;
      case T_op( iOperation ) of
        T_op.opPlus  :
          begin
            cam.Z := cam.Z  / ( 1 + val ) ;
            flag := True  ;
          end;
        T_op.opMinus :
          begin
            cam.Z := cam.Z  * ( 1 + val ) ;
            flag := True  ;
          end;
      end;
      if flag then
        GIS_Viewer.Viewer3D.CameraPosition := cam ;
    end ;

  begin
    factor := Min( 8000, _delta ) / 8000.0 ;

    case FMode of
      TGIS_Viewer3DMode.CameraPosition :
        begin
          do_pos ;
        end ;

      TGIS_Viewer3DMode.CameraXYZ :
        begin
          do_posex ;
        end ;

      TGIS_Viewer3DMode.CameraRotation :
        begin
          do_rot ;
        end ;

      TGIS_Viewer3DMode.SunPosition :
        begin
          do_sun ;
        end ;

      else
        begin
          Assert( False );
        end;
    end;

    do_scale ;

  end;

  procedure TGIS_Control3D.tmrMainTimer(
    _sender : TObject
  ) ;
  begin
    if not Assigned( GIS_Viewer.Viewer3D ) then exit ;
    if ( GetTickCount - iStartTime ) < BUTTON_LATENCY then exit ;

    tmrMain.Enabled := False ;
    try
      doTimer( GetTickCount - iStartTime - BUTTON_LATENCY ) ;
    finally
      if bTimer then
        tmrMain.Enabled := True ;
    end;
  end;

  procedure TGIS_Control3D.btnCenterMouseDown(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Integer      ;
    _y      : Integer
  ) ;
  begin
    case FMode of
      TGIS_Viewer3DMode.CameraPosition :
        Mode := TGIS_Viewer3DMode.CameraXYZ ;
      TGIS_Viewer3DMode.CameraXYZ      :
        Mode := TGIS_Viewer3DMode.CameraRotation ;
      TGIS_Viewer3DMode.CameraRotation :
        Mode := TGIS_Viewer3DMode.SunPosition ;
      TGIS_Viewer3DMode.SunPosition    :
        Mode := TGIS_Viewer3DMode.CameraPosition ;
      TGIS_Viewer3DMode.Zoom           :
        Mode := TGIS_Viewer3DMode.Zoom ;
      else
        Mode := TGIS_Viewer3DMode.CameraPosition ;
    end;
  end;

  procedure TGIS_Control3D.btnUpMouseDown(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Integer      ;
    _y      : Integer
  ) ;
  begin
    iOperation := Ord( T_op.opUp ) ;
    enableTimer ;
  end;

  procedure TGIS_Control3D.btnUpMouseUp(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Integer      ;
    _y      : Integer
  ) ;
  begin
    disableTimer ;
  end;

  procedure TGIS_Control3D.btnLeftMouseDown(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Integer      ;
    _y      : Integer
  ) ;
  begin
    iOperation := Ord( T_op.opLeft ) ;
    enableTimer ;
  end;

  procedure TGIS_Control3D.btnLeftMouseUp(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Integer      ;
    _y      : Integer
  ) ;
  begin
    disableTimer ;
  end;

  procedure TGIS_Control3D.btnRightMouseDown(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Integer      ;
    _y      : Integer
  ) ;
  begin
    iOperation := Ord( T_op.opRight ) ;
    enableTimer ;
  end;

  procedure TGIS_Control3D.btnRightMouseUp(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Integer      ;
    _y      : Integer
  ) ;
  begin
    disableTimer ;
  end;

  procedure TGIS_Control3D.btnDownMouseDown(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Integer      ;
    _y      : Integer
  ) ;
  begin
    iOperation := Ord( T_op.opDown ) ;
    enableTimer ;
  end;

  procedure TGIS_Control3D.btnDownMouseUp(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Integer      ;
    _y      : Integer
  ) ;
  begin
    disableTimer ;
  end;

  procedure TGIS_Control3D.btnPlusMouseDown(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Integer      ;
    _y      : Integer
  ) ;
  begin
    iOperation := Ord( T_op.opPlus ) ;
    enableTimer ;
  end;

  procedure TGIS_Control3D.btnPlusMouseUp(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Integer      ;
    _y      : Integer
  ) ;
  begin
    disableTimer ;
  end;

  procedure TGIS_Control3D.btnMinusMouseDown(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Integer      ;
    _y      : Integer
  ) ;
  begin
    iOperation := Ord( T_op.opMinus ) ;
    enableTimer ;
  end;

  procedure TGIS_Control3D.btnMinusMouseUp(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Integer      ;
    _y      : Integer
  ) ;
  begin
    disableTimer ;
  end;

  procedure TGIS_Control3D.cmbReferenceChange(
    _sender : TObject
  ) ;
  begin
    if not Assigned( GIS_Viewer ) then exit ;
    if not Assigned( GIS_Viewer.Viewer3D ) then exit ;
    if iUponUpdate > 0 then exit ;

    case cmbReference.ItemIndex of
      0 : GIS_Viewer.Viewer3D.ReferencePointMode := TGIS_Viewer3DReferenceMode.Base     ;
      1 : GIS_Viewer.Viewer3D.ReferencePointMode := TGIS_Viewer3DReferenceMode.Zero     ;
      2 : GIS_Viewer.Viewer3D.ReferencePointMode := TGIS_Viewer3DReferenceMode.Lowest   ;
      3 : GIS_Viewer.Viewer3D.ReferencePointMode := TGIS_Viewer3DReferenceMode.Highest  ;
      4 : GIS_Viewer.Viewer3D.ReferencePointMode := TGIS_Viewer3DReferenceMode.OnDem    ;
      5 : GIS_Viewer.Viewer3D.ReferencePointMode := TGIS_Viewer3DReferenceMode.FlyOnDem ;
    end;
  end ;

  procedure TGIS_Control3D.spnReferenceChange(
    _sender : TObject
  );
  begin
    if not Assigned( GIS_Viewer ) then exit ;
    if not Assigned( GIS_Viewer.Viewer3D ) then exit ;
    if iUponUpdate > 0 then exit ;

    if not spnReference.Valid then exit ;

    GIS_Viewer.Viewer3D.ReferencePointOffsetZ := spnReference.Value ;
  end;

  procedure TGIS_Control3D.chkLightsClick(
    _sender : TObject
  ) ;
  begin
    if not Assigned( GIS_Viewer ) then exit ;
    if not Assigned( GIS_Viewer.Viewer3D ) then exit ;

    GIS_Viewer.Viewer3D.ShowLights := chkLights.Checked ;
  end;

  procedure TGIS_Control3D.spnShadowIntensityChange(
    _sender : TObject
  ) ;
  begin
    if not Assigned( GIS_Viewer ) then exit ;
    if not Assigned( GIS_Viewer.Viewer3D ) then exit ;
    if iUponUpdate > 0 then exit ;

    if not spnShadowIntensity.Valid then exit ;

    GIS_Viewer.Viewer3D.ShadowsLevel := RoundS( 100 - spnShadowIntensity.Value ) ;
  end ;


  procedure TGIS_Control3D.chkBasePlaneClick(
    _sender : TObject
  ) ;
  var
    bp : TGIS_Viewer3DBasePlane ;
  begin
    if not Assigned( GIS_Viewer ) then exit ;
    if not Assigned( GIS_Viewer.Viewer3D ) then exit ;

    bp := GIS_Viewer.Viewer3D.BasePlane ;
    bp.Active := chkBasePlane.Checked ;
    GIS_Viewer.Viewer3D.BasePlane := bp ;
  end;

  procedure TGIS_Control3D.spnBasePlaneChange(
    _sender : TObject
  ) ;
  var
    bp : TGIS_Viewer3DBasePlane ;
  begin
    if not Assigned( GIS_Viewer ) then exit ;
    if not Assigned( GIS_Viewer.Viewer3D ) then exit ;
    if iUponUpdate > 0 then exit ;

    if not spnBasePlane.Valid then exit ;

    bp := GIS_Viewer.Viewer3D.BasePlane ;
    bp.Level := spnBasePlane.Value ;
    GIS_Viewer.Viewer3D.BasePlane := bp ;
  end;

  procedure TGIS_Control3D.chkWireframeClick(
    _sender : TObject
  ) ;
  begin
    if not Assigned( GIS_Viewer ) then exit ;
    if not Assigned( GIS_Viewer.Viewer3D ) then exit ;

    GIS_Viewer.Viewer3D.ShowWireframe := chkWireframe.Checked ;
  end;

  procedure TGIS_Control3D.chkEdgesClick(
    _sender : TObject
  ) ;
  begin
    if not Assigned( GIS_Viewer ) then exit ;
    if not Assigned( GIS_Viewer.Viewer3D ) then exit ;

    GIS_Viewer.Viewer3D.ShowVectorEdges := chkEdges.Checked ;
  end;

  procedure TGIS_Control3D.spnScaleZChange(
    _sender : TObject
  ) ;
  begin
    if not Assigned( GIS_Viewer ) then exit ;
    if not Assigned( GIS_Viewer.Viewer3D ) then exit ;
    if iUponUpdate > 0 then exit ;

    if not spnScaleZ.Valid then exit ;

    GIS_Viewer.Viewer3D.ScaleZ := spnScaleZ.Value / 100 ;
  end ;

  procedure TGIS_Control3D.spnScaleMChange(
    _sender : TObject
  ) ;
  begin
    if not Assigned( GIS_Viewer ) then exit ;
    if not Assigned( GIS_Viewer.Viewer3D ) then exit ;
    if iUponUpdate > 0 then exit ;

    if not spnScaleM.Valid then exit ;

    GIS_Viewer.Viewer3D.ScaleM := spnScaleM.Value / 100 ;
  end ;

  procedure TGIS_Control3D.cmbWallChange(
    _sender : TObject
  ) ;
  begin
    if not Assigned( GIS_Viewer ) then exit ;
    if not Assigned( GIS_Viewer.Viewer3D ) then exit ;
    if iUponUpdate > 0 then exit ;

    case cmbWall.ItemIndex of
      0 : GIS_Viewer.Viewer3D.DemWalls := TGIS_Viewer3DDemWall.Invisible ;
      1 : GIS_Viewer.Viewer3D.DemWalls := TGIS_Viewer3DDemWall.Solid     ;
      2 : GIS_Viewer.Viewer3D.DemWalls := TGIS_Viewer3DDemWall.Texture   ;
    end;

  end ;

  procedure TGIS_Control3D.spnFloodChange(
    _sender : TObject
  ) ;
  var
    t : TGIS_Viewer3DFlood ;
  begin
    if not Assigned( GIS_Viewer ) then exit ;
    if not Assigned( GIS_Viewer.Viewer3D ) then exit ;
    if iUponUpdate > 0 then exit ;

    if not spnFlood.Valid then exit ;

    t := GIS_Viewer.Viewer3D.Flood ;
    t.Active := chkFlood.Checked ;
    t.Level  := spnFlood.Value   ;
    GIS_Viewer.Viewer3D.Flood := t ;
  end ;

  procedure TGIS_Control3D.edtKeyDown(
        _sender : TObject ;
    var _key    : Word    ;
        _shift  : TShiftState
  ) ;
  var
    ref : TGIS_Point3D ;
  begin
    if _key <> 13 then exit ; // accept on CR

    if not Assigned( GIS_Viewer.Viewer3D ) then exit ;

    case FMode of
      TGIS_Viewer3DMode.CameraPosition :
        begin
          ref := GIS_Viewer.Viewer3D.CameraPosition ;

          try
            ref.X := DegToRad( StrToFloat( edtX.Text ) ) ;
            edtX.Font.Color := clBlack ;
          except
            edtX.Font.Color := clRed ;
            exit ;
          end;

          try
            ref.Y := DegToRad( StrToFloat( edtY.Text ) ) ;
            edtY.Font.Color := clBlack ;
          except
            edtY.Font.Color := clRed ;
            exit ;
          end;

          try
            ref.Z := StrToFloat( edtZ.Text )   ;
            edtZ.Font.Color := clBlack ;
          except
            edtZ.Font.Color := clRed ;
            exit ;
          end;

          GIS_Viewer.Viewer3D.CameraPosition := ref ;
        end ;

      TGIS_Viewer3DMode.CameraXYZ :
        begin
          ref := GIS_Viewer.Viewer3D.CameraPositionEx ;

          try
            ref.X := StrToFloat( edtX.Text )   ;
            edtX.Font.Color := clBlack ;
          except
            edtX.Font.Color := clRed ;
            exit ;
          end;

          try
            ref.Y := StrToFloat( edtY.Text )   ;
            edtY.Font.Color := clBlack ;
          except
            edtY.Font.Color := clRed ;
            exit ;
          end;

          try
            ref.Z := StrToFloat( edtZ.Text )   ;
            edtZ.Font.Color := clBlack ;
          except
            edtZ.Font.Color := clRed ;
            exit ;
          end;

          GIS_Viewer.Viewer3D.CameraPositionEx := ref ;
        end ;

      TGIS_Viewer3DMode.CameraRotation :
        begin
          ref := GIS_Viewer.Viewer3D.CameraRotation ;

          try
            ref.X := DegToRad( StrToFloat( edtX.Text ) ) ;
            edtX.Font.Color := clBlack ;
          except
            edtX.Font.Color := clRed ;
            exit ;
          end;

          try
            ref.Y := DegToRad( StrToFloat( edtY.Text ) ) ;
            edtY.Font.Color := clBlack ;
          except
            edtY.Font.Color := clRed ;
            exit ;
          end;

          try
            ref.Z := DegToRad( StrToFloat( edtZ.Text ) ) ;
            edtZ.Font.Color := clBlack ;
          except
            edtZ.Font.Color := clRed ;
            exit ;
          end;

          GIS_Viewer.Viewer3D.CameraRotation := ref ;
        end ;

      TGIS_Viewer3DMode.SunPosition :
        begin
          ref := GisPoint3DFrom2D( GIS_Viewer.Viewer3D.SunPosition );

          try
            ref.X := DegToRad( StrToFloat( edtX.Text ) ) ;
            edtX.Font.Color := clBlack ;
          except
            edtX.Font.Color := clRed ;
            exit ;
          end;

          try
            ref.Y := DegToRad( StrToFloat( edtY.Text ) ) ;
            edtY.Font.Color := clBlack ;
          except
            edtY.Font.Color := clRed ;
            exit ;
          end;

          GIS_Viewer.Viewer3D.SunPosition := GisPoint2DFrom3D( ref ) ;
        end ;

      else
        begin
          Assert( False );
        end;
    end;
  end ;

  procedure TGIS_Control3D.doOnUpdate(
    _sender : TObject
  ) ;
  var
    ref : TGIS_Point3D       ;
  begin
    if not Assigned( GIS_Viewer ) then exit ;
    if not Assigned( GIS_Viewer.Viewer3D ) then exit ;

    Inc( iUponUpdate ) ;
    try
      case FMode of
        TGIS_Viewer3DMode.CameraPosition :
          begin
            ref := GIS_Viewer.Viewer3D.CameraPosition ;

            edtX.Text := Format( '%.0f', [ RadToDeg( ref.X ) ] ) ;
            edtY.Text := Format( '%.0f', [ RadToDeg( ref.Y ) ] ) ;
            edtZ.Text := Format( '%.2f', [ ref.Z             ] ) ;
          end ;

        TGIS_Viewer3DMode.CameraXYZ :
          begin
            ref := GIS_Viewer.Viewer3D.CameraPositionEx ;

            edtX.Text := Format( '%.0f', [ ref.X ] ) ;
            edtY.Text := Format( '%.0f', [ ref.Y ] ) ;
            edtZ.Text := Format( '%.2f', [ ref.Z ] ) ;
          end ;

        TGIS_Viewer3DMode.CameraRotation :
          begin
            ref := GIS_Viewer.Viewer3D.CameraRotation ;

            edtX.Text := Format( '%.0f', [ RadToDeg( ref.X ) ] ) ;
            edtY.Text := Format( '%.0f', [ RadToDeg( ref.Y ) ] ) ;
            edtZ.Text := Format( '%.0f', [ RadToDeg( ref.Z ) ] ) ;
          end ;

        TGIS_Viewer3DMode.SunPosition :
          begin
            ref := GisPoint3DFrom2D( GIS_Viewer.Viewer3D.SunPosition ) ;

            edtX.Text := Format( '%.0f', [ RadToDeg( ref.X ) ] ) ;
            edtY.Text := Format( '%.0f', [ RadToDeg( ref.Y ) ] ) ;
          end ;

        else
          begin
            Assert( False );
          end;
      end;

      case GIS_Viewer.Viewer3D.ReferencePointMode of
        TGIS_Viewer3DReferenceMode.Base     : cmbReference.ItemIndex := 0 ;
        TGIS_Viewer3DReferenceMode.Zero     : cmbReference.ItemIndex := 1 ;
        TGIS_Viewer3DReferenceMode.Lowest   : cmbReference.ItemIndex := 2 ;
        TGIS_Viewer3DReferenceMode.Highest  : cmbReference.ItemIndex := 3 ;
        TGIS_Viewer3DReferenceMode.OnDem    : cmbReference.ItemIndex := 4 ;
        TGIS_Viewer3DReferenceMode.FlyOnDem : cmbReference.ItemIndex := 5 ;
      end;

      spnReference.Value   := GIS_Viewer.Viewer3D.ReferencePointOffsetZ ;

      chkLights.Checked    := GIS_Viewer.Viewer3D.ShowLights      ;
      chkBasePlane.Checked := GIS_Viewer.Viewer3D.BasePlane.Active ;
      if spnBasePlane.Value <> GIS_Viewer.Viewer3D.BasePlane.Level then
        spnBasePlane.Value := GIS_Viewer.Viewer3D.BasePlane.Level  ;

      chkWireframe.Checked := GIS_Viewer.Viewer3D.ShowWireframe   ;
      chkEdges.Checked     := GIS_Viewer.Viewer3D.ShowVectorEdges ;

      spnScaleZ.Value      := RoundS( GIS_Viewer.Viewer3D.ScaleZ * 100 ) ;
      spnScaleM.Value      := RoundS( GIS_Viewer.Viewer3D.ScaleM * 100 ) ;

      edtX.Font.Color := clBlack ;
      edtY.Font.Color := clBlack ;
      edtZ.Font.Color := clBlack ;

      chkFlood.Checked := GIS_Viewer.Viewer3D.Flood.Active ;
      if spnFlood.Value <> GIS_Viewer.Viewer3D.Flood.Level then
        spnFlood.Value   := GIS_Viewer.Viewer3D.Flood.Level  ;

      case GIS_Viewer.Viewer3D.DemWalls of
        TGIS_Viewer3DDemWall.Texture   : cmbWall.ItemIndex := 2 ;
        TGIS_Viewer3DDemWall.Invisible : cmbWall.ItemIndex := 0 ;
        TGIS_Viewer3DDemWall.Solid     : cmbWall.ItemIndex := 1 ;
      end;

    finally
      Dec( iUponUpdate ) ;
    end;

  end;

  procedure TGIS_Control3D.SubscribedEvent(
    _sender  : TObject ;
    _event   : Integer ;
    _context : TObject
  ) ;
  begin
    case _event of
      GIS_SUBSCRIBED_DESTROY   :
        begin
          FGIS_Viewer := nil ;
        end ;
      GIS_SUBSCRIBED_3D_UPDATE :
        begin
          Enabled := FGIS_Viewer.View3D ;
          doOnUpdate( _sender ) ;
        end ;
    end;
  end;


  procedure Register;
  begin
    RegisterComponents( 'TatukGIS', [TGIS_Control3D] ) ;
  end;

//==================================== END =====================================
end.


