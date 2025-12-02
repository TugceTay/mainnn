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

unit FMX.GisControl3D ;
{$HPPEMIT '#pragma link "FMX.GisControl3D"'}

{$INCLUDE GisInclude.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Math,
  System.UITypes,
  FMX.Objects,
  FMX.Types,
  FMX.Controls,
  FMX.ExtCtrls,
  FMX.Graphics,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.ListBox,
  FMX.GisControlHelper,

  GisRtl,
  GisInterfaces,
  GisTypes,
  GisFunctions,
  GisResource,
  FMX.GisControlVarious,
  FMX.GisViewerWnd ;


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
  [ComponentPlatformsAttribute( pidAllPlatforms )]
  TGIS_Control3D = class( TPanel, IGIS_Subscribe )
    private
      tmrUpdate          : TTimer ;
      img16              : TImage ;
      img32              : TImage ;
      btnUp              : TCircle       ;
      btnLeft            : TCircle       ;
      btnCenter          : TCircle       ;
      btnRight           : TCircle       ;
      btnDown            : TCircle       ;
      btnPlus            : TCircle       ;
      btnMinus           : TCircle       ;
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
      FBiDiMode   : TBiDiMode ;
      FBiDiModeFromTranslation : Boolean ;
    private

      /// <summary>
      ///   Property GIS_Viewer access routine.
      /// </summary>
      procedure fset_GIS_Viewer( const _value : TGIS_ViewerWnd        ) ;

      procedure fset_BiDiMode       ( const _value  : TBiDiMode
                                    );
      procedure fset_BiDiModeFromTranslation( const _value  : Boolean );

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
    private

      /// <summary>
      ///   Change YI language.
      /// </summary>
      procedure updateLanguage  ;

      procedure doOnResize        (      _sender : TObject
                                  ) ;

      procedure enableTimer ;
      procedure disableTimer ;

      procedure doTimer           (     _delta  : Cardinal
                                  ) ;
      procedure tmrMainTimer      (     _sender : TObject
                                  ) ;
      procedure btnMouseEnter     (     _sender : TObject
                                  ) ;
      procedure btnMouseLeave     (     _sender : TObject
                                  ) ;
      procedure btnCenterMouseDown(     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Single      ;
                                        _y      : Single
                                  ) ;
      procedure btnCenterMouseUp  (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Single      ;
                                        _y      : Single
                                  ) ;
      procedure btnUpMouseDown    (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Single      ;
                                        _y      : Single
                                  ) ;
      procedure btnUpMouseUp      (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Single      ;
                                        _y      : Single
                                  ) ;
      procedure btnLeftMouseDown  (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Single      ;
                                        _y      : Single
                                  ) ;
      procedure btnLeftMouseUp    (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Single      ;
                                        _y      : Single
                                  ) ;
      procedure btnRightMouseDown (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Single      ;
                                        _y      : Single
                                  ) ;
      procedure btnRightMouseUp   (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Single      ;
                                        _y      : Single
                                  ) ;
      procedure btnDownMouseDown  (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Single      ;
                                        _y      : Single
                                  ) ;
      procedure btnDownMouseUp    (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Single      ;
                                        _y      : Single
                                  ) ;
      procedure btnPlusMouseDown  (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Single      ;
                                        _y      : Single
                                  ) ;
      procedure btnPlusMouseUp    (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Single      ;
                                        _y      : Single
                                  ) ;
      procedure btnMinusMouseDown (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Single      ;
                                        _y      : Single
                                  ) ;
      procedure btnMinusMouseUp   (     _sender : TObject      ;
                                        _button : TMouseButton ;
                                        _shift  : TShiftState  ;
                                        _x      : Single      ;
                                        _y      : Single
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
                                    var _keyChar: Char ;
                                        _shift  : TShiftState
                                  ) ;
    private
      procedure doOnUpdate        (      _sender : TObject
                                  ) ;
    public
      /// <inheritdoc/>
      constructor Create    ( _owner : TComponent ) ; override;

      /// <inheritdoc/>
      destructor  Destroy ; override;

      /// <inheritdoc/>
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
      ///   Attached TGIS_ViewerWnd component instance.
      /// </summary>
      property GIS_Viewer : TGIS_ViewerWnd read FGIS_Viewer write fset_GIS_Viewer ;

      ///  <summary>
      ///    Emulation of VCL BiDiMode for translation purpose
      ///  </summary>
      property BiDiMode      : TBiDiMode
                                       read FBiDiMode
                                       write fset_BiDiMode
                                       default TBiDiMode.bdLeftToRight ;

      ///  <summary>
      ///    Decides where use BiDiMode from property or
      ///    from translation
      ///  </summary>
      property BiDiModeFromTranslation : Boolean
                                       read FBiDiModeFromTranslation
                                       write fset_BiDiModeFromTranslation
                                       default True;

    published // properties derived from base class

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
      property Enabled ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property HelpContext ;

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
      property OnClick ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnDblClick ;

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
      property OnKeyDown ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnKeyUp ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnMouseDown ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnMouseMove ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnMouseUp ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnMouseWheel ;

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

  end ;

  {#gendoc:hide}
  procedure Register;

//##############################################################################
implementation

uses
  GisTypesUI,
  GisTypes3D ;

{$R FMX.GisControl3D_16x16.RES}

type
  T_op = ( opLeft, opRight, opUp, opDown, opPlus, opMinus ) ;

const
  BUTTON_LATENCY = 500 ;

    procedure load_image_from_resource(
      const _img     : TImage ;
      const _resName : String
    ) ;
    var
      InStream : TResourceStream ;
      bmp      : TGIS_Bitmap ;
    begin
      if FindResource(HInstance, PWideChar(_resName), RT_RCDATA) = 0 then
      exit ;

      InStream := TResourceStream.Create(HInstance, _resName, RT_RCDATA);
      try
        InStream.Position := 0 ;

        _img.Bitmap.LoadFromStream(InStream);
      finally
        InStream.Free ;
      end ;
    end ;

    procedure addResBmp(
      const _imgList  : TImage ;
      const _index    : Integer ;
      const _size     : Integer ;
      const _parent   : TControl
    ) ;
    var
      img : TImage  ;
      bmp : TBitmap ;
      i   : Integer ;
    begin
      img := nil ;
      for i := 0 to _parent.Controls.Count-1 do begin
        if _parent.Controls[i].Name = 'img' then begin
          img := _parent.Controls[i] as TImage ;
          break ;
        end ;
      end ;

      if not assigned( img ) then begin
        img := TImage.Create( _parent ) ;
        img.Parent := _parent ;
        img.Name := 'img' ;
        img.Align := TAlignLayout.Center ;
        img.HitTest := False ;
        img.WrapMode := TImageWrapMode.Fit ;
        img.Stored := False ;
      end ;

      assert( img <> nil ) ;
      img.Width  := _size ;
      img.Height := _size ;

      bmp := TBitmap.Create( _size, _size ) ;
      try
        bmp.Canvas.BeginScene ;
        bmp.Canvas.DrawBitmap(
          _imgList.Bitmap,
          RectF(_index*_size,0,(_index+1)*_size,_size),
          RectF(0,0,_size,_size),
          1
        ) ;
        bmp.Canvas.EndScene ;

        img.Bitmap.Assign( bmp ) ;
      finally
        FreeObject( bmp ) ;
      end ;
    end ;

  constructor TGIS_Control3D.Create( _owner : TComponent ) ;
  begin
    inherited Create( _owner ) ;

    iUponUpdate := 0 ;

    BiDiModeFromTranslation := True ;

    tmrUpdate := TTimer.Create( Self ) ;
    with tmrUpdate do begin
      Stored      := False ;
      Interval    := 10 ;
      Enabled     := False ;
      Parent      := Self ;
      OnTimer     := doOnUpdate ;
    end;

    img16 := TImage.Create( self ) ;
    img16.Name := 'img16' ;
    img16.Stored := False ;
    load_image_from_resource( img16, 'TGIS_CONTROL3D_IMAGES_16x16' ) ;

    img32 := TImage.Create( self ) ;
    img32.Name := 'img32' ;
    img32.Stored := False ;
    load_image_from_resource( img32, 'TGIS_CONTROL3D_IMAGES_32x32' ) ;

    btnUp := TCircle.Create( self ) ;
    with btnUp do begin
      Parent      := Self ;
      Fill.Color  := 0 ;
      Stroke.Thickness := 0 ;
      OnMouseDown  := btnUpMouseDown ;
      OnMouseUp    := btnUpMouseUp   ;
      OnMouseEnter := btnMouseEnter ;
      OnMouseLeave := btnMouseleave ;
      addResBmp( img16, 4, 16, btnUp ) ;
      Stored := False ;
    end ;

    btnLeft := TCircle.Create( self ) ;
    with btnLeft do begin
      Parent      := Self ;
      Fill.Color  := 0 ;
      Stroke.Thickness := 0 ;
      OnMouseDown  := btnLeftMouseDown ;
      OnMouseUp    := btnLeftMouseUp   ;
      OnMouseEnter := btnMouseEnter ;
      OnMouseLeave := btnMouseLeave ;
      addResBmp( img16, 2, 16, btnLeft ) ;
      Stored := False ;
    end ;

    btnCenter  := TCircle.Create( self ) ;
    with btnCenter do begin
      Parent      := Self ;
      Fill.Color  := 0 ;
      Stroke.Thickness := 0 ;
      OnMouseDown  := btnCenterMouseDown ;
      OnMouseUp    := btnCenterMouseUp    ;
      OnMouseEnter := btnMouseEnter ;
      OnMouseLeave := btnMouseLeave ;
      Stored := False ;
    end ;

    btnRight := TCircle.Create( self ) ;
    with btnRight do begin
      Parent      := Self ;
      Fill.Color  := 0 ;
      Stroke.Thickness := 0 ;
      OnMouseDown  := btnRightMouseDown ;
      OnMouseUp    := btnRightMouseUp   ;
      OnMouseEnter := btnMouseEnter ;
      OnMouseLeave := btnMouseLeave ;
      addResBmp( img16, 3, 16, btnRight ) ;
      Stored := False ;
    end ;

    btnDown := TCircle.Create( self ) ;
    with btnDown do begin
      Parent      := Self  ;
      Fill.Color  := 0 ;
      Stroke.Thickness := 0 ;
      OnMouseDown  := btnDownMouseDown ;
      OnMouseUp    := btnDownMouseUp   ;
      OnMouseEnter := btnMouseEnter ;
      OnMouseLeave := btnMouseLeave ;
      addResBmp( img16, 5, 16, btnDown ) ;
      Stored := False ;
    end ;

    btnPlus := TCircle.Create( self ) ;
    with btnPlus do begin
      Parent      := Self  ;
      Fill.Color  := 0 ;
      Stroke.Thickness := 0 ;
      OnMouseDown  := btnPlusMouseDown ;
      OnMouseUp    := btnPlusMouseUp   ;
      OnMouseEnter := btnMouseEnter ;
      OnMouseLeave := btnMouseLeave ;
      addResBmp( img16, 0, 16, btnPlus ) ;
      Stored := False ;
    end ;

    btnMinus := TCircle.Create( self ) ;
    with btnMinus do begin
      Parent      := Self  ;
      Fill.Color  := 0 ;
      Stroke.Thickness := 0 ;
      OnMouseDown  := btnMinusMouseDown ;
      OnMouseUp    := btnMinusMouseUp   ;
      OnMouseEnter := btnMouseEnter ;
      OnMouseLeave := btnMouseLeave ;
      addResBmp( img16, 1, 16, btnMinus ) ;
      Stored := False ;
    end ;

    bvlCoordinates := TGIS_Bevel.Create( self ) ;
    with bvlCoordinates do begin
      Parent := Self  ;
      Height := 2 ;
      Stroke.Color := TAlphaColorRec.Silver ;
      Stored := False ;
    end ;

    lblX := TLabel.Create( self ) ;
    with lblX do begin
      Parent     := Self   ;
      Text    := 'lblX' ;
      Stored := False ;
      FixSize ;
    end ;

    edtX := TEdit.Create( self ) ;
    with edtX do begin
      Parent     := Self  ;
      OnKeyDown  := edtKeyDown ;
      Stored := False ;
      FixSize ;
      KillFocusByReturn := True ;
    end ;

    lblY := TLabel.Create( self ) ;
    with lblY do begin
      Parent     := Self   ;
      Text    := 'lblY' ;
      Stored := False ;
      FixSize ;
    end ;

    edtY := TEdit.Create( self ) ;
    with edtY do begin
      Parent     := Self  ;
      OnKeyDown  := edtKeyDown ;
      Stored := False ;
      FixSize ;
      KillFocusByReturn := True ;
    end ;

    lblZ := TLabel.Create( self ) ;
    with lblZ do begin
      Parent     := Self   ;
      Text    := 'lblZ' ;
      Stored := False ;
      FixSize ;
    end ;

    edtZ := TEdit.Create( self ) ;
    with edtZ do begin
      Parent     := Self  ;
      OnKeyDown  := edtKeyDown ;
      Stored := False ;
      FixSize ;
      KillFocusByReturn := True ;
    end ;

    bvlReferencePoint := TGIS_Bevel.Create( self ) ;
    with bvlReferencePoint do begin
      Parent := Self ;
      Height := 2 ;
      Stroke.Color := TAlphaColorRec.Silver ;
      Stored := False ;
    end ;

    lblReference := TLabel.Create( self ) ;
    with lblReference do begin
      Parent  := Self  ;
      Text    := 'lblReference' ;
      Stored := False ;
      FixSize ;
    end ;

    cmbReference := TComboBox.Create( self ) ;
    with cmbReference do begin
      Parent   := Self  ;
      OnChange := cmbReferenceChange ;
      Stored := False ;
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
      Stored := False ;
    end ;

    bvlLights := TGIS_Bevel.Create( self ) ;
    with bvlLights do begin
      Parent := Self ;
      Height := 2 ;
      Stroke.Color := TAlphaColorRec.Silver ;
      Stored := False ;
    end ;

    chkLights := TCheckBox.Create( self ) ;
    with chkLights do begin
      Parent     := Self  ;
      OnChange   := chkLightsClick ;
      Text    := 'chkLights' ;
      Stored := False ;
    end ;

    lblShadowIntensity := TLabel.Create( self ) ;
    with lblShadowIntensity do begin
      Parent     := Self  ;
      Text    := 'lblShadowIntensity' ;
      Stored := False ;
      FixSize ;
    end ;

    spnShadowIntensity := TGIS_SpinEdit.Create( self ) ;
    with spnShadowIntensity do begin
      Parent     := Self ;
      Increment  := 5 ;
      OnChange   := spnShadowIntensityChange ;
      MinVal     :=   0 ;
      MaxVal     := 100 ;
      Value      :=  60 ;
      Stored := False ;
    end ;

    bvlFrameModes := TGIS_Bevel.Create( self ) ;
    with bvlFrameModes do begin
      Parent := Self ;
      Height := 2 ;
      Stroke.Color := TAlphaColorRec.Silver ;
      Stored := False ;
    end ;

    chkBasePlane := TCheckBox.Create( self ) ;
    with chkBasePlane do begin
      Parent     := Self ;
      OnChange   := chkBasePlaneClick ;
      Text    := 'chkBasePlane' ;
      Stored := False ;
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
      Stored     := False ;
    end ;

    bvlBasePlane := TGIS_Bevel.Create( self ) ;
    with bvlBasePlane do begin
      Parent := Self ;
      Height := 5 ;
      Stroke.Color := TAlphaColorRec.Silver ;
      Stored := False ;
    end ;

    chkWireframe := TCheckBox.Create( self ) ;
    with chkWireframe do begin
      Parent     := Self ;
      OnChange   := chkWireframeClick ;
      Text    := 'chkWireframe' ;
      Stored := False ;
      Visible := False ;
    end ;

    chkEdges := TCheckBox.Create( self ) ;
    with chkEdges do begin
      Parent     := Self ;
      OnChange   := chkEdgesClick ;
      Text    := 'chkEdges' ;
      Stored := False ;
    end ;

    bvlScalings := TGIS_Bevel.Create( self ) ;
    with bvlScalings do begin
      Parent := Self ;
      Height := 2 ;
      Stroke.Color := TAlphaColorRec.Silver ;
      Stored := False ;
    end ;

    lblScaleZ := TLabel.Create( self ) ;
    with lblScaleZ do begin
      Parent     := Self  ;
      Text    := 'lblScaleZ' ;
      Stored := False ;
      FixSize ;
    end ;

    spnScaleZ := TGIS_SpinEdit.Create( self ) ;
    with spnScaleZ do begin
      Parent     := Self ;
      Increment  := 10 ;
      OnChange   := spnScaleZChange ;
      MinVal     := -999 ;
      MaxVal     :=  999 ;
      Value      :=  100 ;
      Stored := False ;
    end ;

    lblScaleM := TLabel.Create( self ) ;
    with lblScaleM do begin
      Parent     := Self  ;
      Text    := 'lblScaleM' ;
      Stored := False ;
      FixSize ;
    end ;

    spnScaleM := TGIS_SpinEdit.Create( self ) ;
    with spnScaleM do begin
      Parent     := Self ;
      Increment  := 10 ;
      OnChange   := spnScaleMChange ;
      MinVal     := -999 ;
      MaxVal     :=  999 ;
      Value      :=  100 ;
      Stored := False ;
    end ;

    bvlFloods := TGIS_Bevel.Create( self ) ;
    with bvlFloods do begin
      Parent := Self ;
      Height := 2 ;
      Stroke.Color := TAlphaColorRec.Silver ;
      Stored := False ;
    end ;

    chkFlood := TCheckBox.Create( self ) ;
    with chkFlood do begin
      Parent     := Self  ;
      OnChange   := spnFloodChange ;
      Text    := 'chkFlood' ;
      Stored := False ;
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
      Stored := False ;
    end ;

    bvlWalls := TGIS_Bevel.Create( self ) ;
    with bvlWalls do begin
      Parent := Self  ;
      Height := 2 ;
      Stroke.Color := TAlphaColorRec.Silver ;
      Stored := False ;
    end ;

    lblWall := TLabel.Create( self ) ;
    with lblWall do begin
      Parent     := Self  ;
      Text    := 'lblWall' ;
      Stored := False ;
      FixSize ;
    end ;

    cmbWall := TComboBox.Create( self ) ;
    with cmbWall do begin
      Parent   := Self ;
      OnChange := cmbWallChange ;
      Stored := False ;
    end ;

    tmrMain := TTimer.Create( self ) ;
    with tmrMain do begin
      Stored      := False ;
      Interval := 1     ;
      OnTimer  := tmrMainTimer ;
      Enabled  := False ;
      Stored := False ;
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

    LocalizedNotification.Subscribe(
      updateLanguage
    ) ;
  end ;

  destructor TGIS_Control3D.Destroy ;
  begin
    {$IFDEF GIS_PDK}
      RemoveFreeNotifications ;
    {$ENDIF}

    if Assigned( FGIS_Viewer ) then
      FGIS_Viewer.UnSubscribe( Self ) ;

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

  procedure TGIS_Control3D.fset_BiDiMode(
    const _value : TBiDiMode
  );
  begin
    if _value <> FBiDiMode then
      FBiDiMode := _value ;
  end;

  procedure TGIS_Control3D.fset_BiDiModeFromTranslation(
    const _value : Boolean
  );
  begin
    if _value <> FBiDiModeFromTranslation then
      FBiDiModeFromTranslation := _value ;
  end;

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

    case FMode of
      TGIS_Viewer3DMode.CameraPosition :
        begin
          addResBmp( img32, 1, 32, btnCenter ) ;
          {$IFDEF LEVEL_RX10_FMX}
          btnCenter.Hint := _rsrc( GIS_RS_3D_ROTATION ) ;
          {$ENDIF}
        end ;
      TGIS_Viewer3DMode.CameraXYZ      :
        begin
          addResBmp( img32, 2, 32, btnCenter ) ;
          {$IFDEF LEVEL_RX10_FMX}
          btnCenter.Hint := _rsrc( GIS_RS_3D_XYZ_POSITION ) ;
          {$ENDIF}
        end ;
      TGIS_Viewer3DMode.CameraRotation :
        begin
          addResBmp( img32, 3, 32, btnCenter ) ;
          {$IFDEF LEVEL_RX10_FMX}
          btnCenter.Hint := _rsrc( GIS_RS_3D_CAMERA_ANGLE ) ;
          {$ENDIF}
        end ;
      TGIS_Viewer3DMode.SunPosition    :
        begin
          addResBmp( img32, 0, 32, btnCenter ) ;
          {$IFDEF LEVEL_RX10_FMX}
          btnCenter.Hint := _rsrc( GIS_RS_3D_SUN_POSITION ) ;
          {$ENDIF}
        end ;
      else
        begin
          addResBmp( img32, 1, 32, btnCenter ) ;
          {$IFDEF LEVEL_RX10_FMX}
          btnCenter.Hint := _rsrc( GIS_RS_3D_ROTATION ) ;
          {$ENDIF}
        end ;
    end;

    case FMode of
      TGIS_Viewer3DMode.CameraPosition :
        begin
          lblX.Text := _rsrcna( GIS_RS_3D_COORDINATE_VERTICAL ) ;
          lblY.Text := _rsrcna( GIS_RS_3D_COORDINATE_HORIZONTAL ) ;
          lblZ.Text := _rsrcna( GIS_RS_3D_COORDINATE_DISTANCE ) ;

          lblX.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          edtX.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          lblY.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          edtY.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          lblZ.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          edtZ.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
        end ;

      TGIS_Viewer3DMode.CameraXYZ :
        begin
          lblX.Text := _rsrcna( GIS_RS_3D_COORDINATE_X ) ;
          lblY.Text := _rsrcna( GIS_RS_3D_COORDINATE_Y ) ;
          lblZ.Text := _rsrcna( GIS_RS_3D_COORDINATE_Z ) ;

          lblX.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          edtX.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          lblY.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          edtY.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          lblZ.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          edtZ.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
        end ;

      TGIS_Viewer3DMode.CameraRotation :
        begin
          lblX.Text := _rsrcna( GIS_RS_3D_COORDINATE_VERTICAL ) ;
          lblY.Text := _rsrcna( GIS_RS_3D_COORDINATE_TILT ) ;
          lblZ.Text := _rsrcna( GIS_RS_3D_COORDINATE_HORIZONTAL ) ;

          lblX.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          edtX.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          lblY.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          edtY.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          lblZ.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
          edtZ.Visible := TGIS_Control3DOption.ShowCoordinates in Options ;
        end ;

      TGIS_Viewer3DMode.SunPosition :
        begin
          lblX.Text := _rsrcna( GIS_RS_3D_COORDINATE_VERTICAL ) ;
          lblY.Text := _rsrcna( GIS_RS_3D_COORDINATE_HORIZONTAL ) ;

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

  procedure TGIS_Control3D.updateLanguage ;
  begin
    lblReference.Text       := _rsrcna( GIS_RS_3D_REFERENCE_POINT ) ;
    chkLights.Text          := _rsrcna( GIS_RS_3D_LIGHTS ) ;
    lblShadowIntensity.Text := _rsrcna( GIS_RS_3D_SHADOW_INTESITY ) ;
    chkBasePlane.Text       := _rsrcna( GIS_RS_3D_BASEPLANE ) ;
    chkWireframe.Text       := _rsrcna( GIS_RS_3D_WIREFRAME ) ;
    chkEdges.Text           := _rsrcna( GIS_RS_3D_EDGES ) ;
    lblScaleZ.Text          := _rsrcna( GIS_RS_3D_SCALE_Z ) ;
    lblScaleM.Text          := _rsrcna( GIS_RS_3D_SCALE_M ) ;
    chkFlood.Text           := _rsrcna( GIS_RS_3D_FLOOD ) ;
    lblWall.Text            := _rsrcna( GIS_RS_3D_WALL ) ;

    if cmbReference.Items.Count <= 0 then begin
      cmbReference.Items.Add( '' ) ;
      cmbReference.Items.Add( '' ) ;
      cmbReference.Items.Add( '' ) ;
      cmbReference.Items.Add( '' ) ;
      cmbReference.Items.Add( '' ) ;
      cmbReference.Items.Add( '' ) ;
    end;
    cmbReference.Items[ 0] := _rsrcna( GIS_RS_3D_REFERENCE_BASE       ) ;
    cmbReference.Items[ 1] := _rsrcna( GIS_RS_3D_REFERENCE_ZERO       ) ;
    cmbReference.Items[ 2] := _rsrcna( GIS_RS_3D_REFERENCE_LOWEST     ) ;
    cmbReference.Items[ 3] := _rsrcna( GIS_RS_3D_REFERENCE_HIGHEST    ) ;
    cmbReference.Items[ 4] := _rsrcna( GIS_RS_3D_REFERENCE_ON_DEM     ) ;
    cmbReference.Items[ 5] := _rsrcna( GIS_RS_3D_REFERENCE_FLY_ON_DEM ) ;

    if cmbWall.Items.Count <= 0 then begin
      cmbWall.Items.Add( '' ) ;
      cmbWall.Items.Add( '' ) ;
      cmbWall.Items.Add( '' ) ;
    end ;
    cmbWall.Items[ 0] := _rsrcna( GIS_RS_3D_WALLS_OFF     ) ;
    cmbWall.Items[ 1] := _rsrcna( GIS_RS_3D_WALLS_COLOR   ) ;
    cmbWall.Items[ 2] := _rsrcna( GIS_RS_3D_WALLS_TEXTURE ) ;
  end;

  procedure TGIS_Control3D.doOnResize(
    _sender : TObject
  ) ;
  const
    {$IFDEF GIS_MOBILE}
      BTN1_SIZE   = 30 ;
      BTN2_SIZE   = 40 ;
    {$ELSE}
      BTN1_SIZE   = 20 ;
      BTN2_SIZE   = 40 ;
    {$ENDIF}
    BTN1_GAP    =  0 ;
    BTN1_GAP2   =  5 ;
    VERT_GAP    =  5 ;
    BVL_SIZE    =  2 ;
    TOP_MARGIN  = 10 ;
    LEFT_MARGIN =  5 ;
    MINUS_GAP   =  0 ;
    OFFWINDOW   = -100000 ;
  var
    l      : Integer ;
    l_lbl  : Integer ;
    t      : Single ;
    w      : Integer ;
    center : Integer ;
    cnt    : Integer ;
  begin

    if FBiDiModeFromTranslation then
    begin
      if _rsbidi then
      begin
        if BiDiMode <> TBiDiMode.bdRightToLeft then
          BiDiMode := TBiDiMode.bdRightToLeft
      end
      else
      begin
        if BiDiMode <> TBiDiMode.bdLeftToRight then
          BiDiMode := TBiDiMode.bdLeftToRight
      end;

    end;

    updateLanguage ;

    cnt := 0 ; // number of visible items
    center := TruncS(self.Width / 2)  ;
    t := TOP_MARGIN ;
    l := center - BTN1_SIZE div 2 ;

    // Navigation panel
    if TGIS_Control3DOption.ShowNavigation in Options then begin
      with btnUp do begin
        Visible      := True ;
        Width        := BTN1_SIZE ;
        Height       := BTN1_SIZE ;
        Position.Y   := t ;
        Position.X   := l ;
      end ;
      t := t + BTN1_SIZE + BTN1_GAP ;

      l := center - BTN2_SIZE div 2 ;
      l := l - BTN1_SIZE - BTN1_GAP ;

      with btnLeft do begin
        Visible      := True ;
        Width        := BTN1_SIZE ;
        Height       := BTN1_SIZE ;
        Position.Y   := t + ( BTN2_SIZE-BTN1_SIZE ) div 2 ;
        Position.X   := l ;
      end ;
      l := l + BTN1_SIZE + BTN1_GAP ;

      with btnCenter do begin
        Visible      := True ;
        Width        := BTN2_SIZE ;
        Height       := BTN2_SIZE ;
        Position.Y   := t ;
        Position.X   := l ;
      end ;
      l := l + BTN2_SIZE + BTN1_GAP ;

      with btnRight do begin
        Visible      := True ;
        Width        := BTN1_SIZE ;
        Height       := BTN1_SIZE ;
        Position.Y   := t + ( BTN2_SIZE-BTN1_SIZE ) div 2 ;
        Position.X   := l ;
      end ;
      t := t + BTN2_SIZE + BTN1_GAP ;

      l := center - BTN1_SIZE div 2 ;

      with btnDown do begin
        Visible      := True ;
        Width        := BTN1_SIZE ;
        Height       := BTN1_SIZE ;
        Position.Y   := t ;
        Position.X   := l ;
      end ;
      t := t + BTN1_SIZE + BTN1_GAP ;

      with btnPlus do begin
        Visible      := True ;
        Width        := BTN1_SIZE ;
        Height       := BTN1_SIZE ;
        Position.Y   := t ;
        {$IFDEF GIS_MOBILE}
          Position.X   := btnLeft.Position.X ;
        {$ELSE}
          Position.X   := l ;
        {$ENDIF}
      end ;
      {$IFNDEF GIS_MOBILE}
        t := t + BTN1_SIZE + BTN1_GAP ;
      {$ENDIF}

      with btnMinus do begin
        Visible      := True ;
        Width        := BTN1_SIZE ;
        Height       := BTN1_SIZE ;
        Position.Y   := t ;
        {$IFDEF GIS_MOBILE}
          Position.X   := btnRight.Position.X ;
        {$ELSE}
          Position.X   := l ;
        {$ENDIF}
      end ;
      t := t + BTN1_SIZE + BTN1_GAP ;

      t := t + VERT_GAP ;

      Inc( cnt ) ;
    end
    else begin
      with btnUp do begin
        Visible      := False ;
        Position.X   := OFFWINDOW ; // for designer purposes
      end ;

      with btnLeft do begin
        Visible      := False ;
        Position.X   := OFFWINDOW ; // for designer purposes
      end ;

      with btnCenter do begin
        Visible      := False ;
        Position.X   := OFFWINDOW ; // for designer purposes
      end ;

      with btnRight do begin
        Visible      := False ;
        Position.X   := OFFWINDOW ; // for designer purposes
      end ;

      with btnDown do begin
        Visible      := False ;
        Position.X   := OFFWINDOW ; // for designer purposes
      end ;

      with btnPlus do begin
        Visible      := False ;
        Position.X   := OFFWINDOW ; // for designer purposes
      end ;

      with btnMinus do begin
        Visible      := False ;
        Position.X   := OFFWINDOW ; // for designer purposes
      end ;
    end;
    l := LEFT_MARGIN ;
    l_lbl := l ;
    w := TruncS( self.Width - LEFT_MARGIN - LEFT_MARGIN ) ;

    // Coordinates panel
    if TGIS_Control3DOption.ShowCoordinates in Options then begin
      if cnt > 0 then
        t := t + VERT_GAP ;
      with bvlCoordinates do begin
        Visible    := cnt > 0 ;
        Height     := BVL_SIZE ;
        Position.Y := t  ;
        if Visible then
          PlaceControl(BiDiMode, nil, bvlCoordinates, self.ClipRect.Left, Self.Width)
        else
          PlaceControl(BiDiMode, nil, bvlCoordinates, OFFWINDOW, Self.Width)
      end ;
      if cnt > 0 then
        t := t + bvlCoordinates.Height + VERT_GAP;

      with lblX do begin
        Visible      := True ;
        Position.Y   := t ;
        PlaceControl(BiDiMode, nil, lblX, l_lbl, w);
        if BiDiMode = TBiDiMode.bdRightToLeft then
          TextAlign := TTextAlign.Trailing
        else
          TextAlign := TTextAlign.Leading;
      end ;
      t := t + lblX.Height ;

      with edtX do begin
        Visible      := True ;
        Position.Y   := t ;
        PlaceControl(BiDiMode, nil, edtX, l, w);
        if BiDiMode = TBiDiMode.bdRightToLeft then
          TextAlign := TTextAlign.Trailing
        else
          TextAlign := TTextAlign.Leading;
      end ;
      t := t + edtX.Height ;

      with lblY do begin
        Visible      := True ;
        Position.Y   := t ;
        PlaceControl(BiDiMode, nil, lblY, l_lbl, w);
        if BiDiMode = TBiDiMode.bdRightToLeft then
          TextAlign := TTextAlign.Trailing
        else
          TextAlign := TTextAlign.Leading;
      end ;
      t := t + lblY.Height ;

      with edtY do begin
        Visible      := True ;
        Position.Y   := t ;
        PlaceControl(BiDiMode, nil, edtY, l, w);
        if BiDiMode = TBiDiMode.bdRightToLeft then
          TextAlign := TTextAlign.Trailing
        else
          TextAlign := TTextAlign.Leading;
      end ;
      t := t + edtY.Height ;

      with lblZ do begin
        Visible      := True ;
        Position.Y   := t ;
        PlaceControl(BiDiMode, nil, lblZ, l_lbl, w);
        if BiDiMode = TBiDiMode.bdRightToLeft then
          TextAlign := TTextAlign.Trailing
        else
          TextAlign := TTextAlign.Leading;
      end ;
      t := t + lblZ.Height ;

      with edtZ do begin
        Visible      := True ;
        Position.Y   := t ;
        PlaceControl(BiDiMode, nil, edtZ, l, w);
        if BiDiMode = TBiDiMode.bdRightToLeft then
          TextAlign := TTextAlign.Trailing
        else
          TextAlign := TTextAlign.Leading;
      end ;
      t := t + edtZ.Height + VERT_GAP ;

      Inc( cnt ) ;
    end
    else begin
      with bvlCoordinates do begin
        Visible    := False ;
        Position.X := OFFWINDOW ; // for designer purposes
      end ;

      with lblX do begin
        Visible    := False ;
        Position.X := OFFWINDOW ; // for designer purposes
      end ;

      with edtX do begin
        Visible    := False ;
        Position.X := OFFWINDOW ; // for designer purposes
      end ;

      with lblY do begin
        Visible    := False ;
        Position.X := OFFWINDOW ; // for designer purposes
      end ;

      with edtY do begin
        Visible    := False ;
        Position.X := OFFWINDOW ; // for designer purposes
      end ;

      with lblZ do begin
        Visible    := False ;
        Position.X := OFFWINDOW ; // for designer purposes
      end ;

      with edtZ do begin
        Visible    := False ;
        Position.X := OFFWINDOW ; // for designer purposes
      end ;
    end;

    // Reference point panel
    if TGIS_Control3DOption.ShowReferencePoint in Options then begin
      if cnt > 0 then
        t := t + VERT_GAP ;
      with bvlReferencePoint do begin
        Visible    := cnt > 0 ;
        Height     := BVL_SIZE ;
        Position.Y := t ;
        if Visible then
          PlaceControl(BiDiMode, nil, bvlReferencePoint, self.ClipRect.Left, Self.Width)
        else
          PlaceControl(BiDiMode, nil, bvlReferencePoint, OFFWINDOW, Self.Width);
      end ;
      if cnt > 0 then
        t := t + bvlReferencePoint.Height + VERT_GAP;

      with lblReference do begin
        Visible      := True ;
        Position.Y   := t ;
        PlaceControl(BiDiMode, nil, lblReference, l_lbl, w);
        if BiDiMode = TBiDiMode.bdRightToLeft then
          TextAlign := TTextAlign.Trailing
        else
          TextAlign := TTextAlign.Leading;
      end ;
      t := t + lblReference.Height + VERT_GAP ;

      with cmbReference do begin
        Visible      := True ;
        Position.Y   := t  ;
        PlaceControl(BiDiMode, nil, cmbReference, l, w);
        if BiDiMode = TBiDiMode.bdRightToLeft then
      //?    no text align property for comboboxes
        else
      //?    no text align property for comboboxes
      end ;
      t := t + cmbReference.Height + VERT_GAP ;

      with spnReference do begin
        Visible      := True ;
        Position.Y   := t  ;
        PlaceControl(BiDiMode, nil, spnReference, l, w);
        if BiDiMode = TBiDiMode.bdRightToLeft then
          TextAlign := TTextAlign.Trailing
        else
          TextAlign := TTextAlign.Leading;
      end ;
      t := t + spnReference.Height + VERT_GAP ;

      Inc( cnt ) ;
    end
    else begin
      with bvlReferencePoint do begin
        Visible    := False ;
        Position.X := OFFWINDOW ; // for designer purposes
      end ;

      with lblReference do begin
        Visible    := False ;
        Position.X := OFFWINDOW ; // for designer purposes
      end ;

      with cmbReference do begin
        Visible    := False ;
        Position.X := OFFWINDOW ; // for designer purposes
      end ;

      with spnReference do begin
        Visible    := False ;
        Position.X := OFFWINDOW ; // for designer purposes
      end ;
    end;

    // Lights panel
    if TGIS_Control3DOption.ShowLights in Options then begin
      if cnt > 0 then
        t := t + VERT_GAP ;
      with bvlLights do begin
        Visible    := cnt > 0 ;
        Height     := BVL_SIZE ;
        Position.Y := t ;
        if Visible then
          PlaceControl(BiDiMode, nil, bvlLights, self.ClipRect.Left, self.Width)
        else
          PlaceControl(BiDiMode, nil, bvlLights, OFFWINDOW, self.Width);
      end ;
      if cnt > 0 then
        t := t + bvlLights.Height + VERT_GAP ;

      with chkLights do begin
        Visible    := True ;
        Position.Y := t ;
        PlaceControl(BiDiMode, nil, chkLights, l, w);
        if BiDiMode = TBiDiMode.bdRightToLeft then
          TextAlign := TTextAlign.Trailing
        else
          TextAlign := TTextAlign.Leading;
      end ;
      t := t + chkLights.Height + VERT_GAP ;

      with lblShadowIntensity do begin
        Visible    := True ;
        Position.Y := t ;
        PlaceControl(BiDiMode, nil, lblShadowIntensity, l_lbl, w);
        if BiDiMode = TBiDiMode.bdRightToLeft then
          TextAlign := TTextAlign.Trailing
        else
          TextAlign := TTextAlign.Leading;
      end ;
      t := t + lblShadowIntensity.Height ;

      with spnShadowIntensity do begin
        Visible    := True ;
        Position.Y := t  ;
        PlaceControl(BiDiMode, nil, spnShadowIntensity, l, w);
        if BiDiMode = TBiDiMode.bdRightToLeft then
          TextAlign := TTextAlign.Trailing
        else
          TextAlign := TTextAlign.Leading;
      end ;
      t := t + spnShadowIntensity.Height + VERT_GAP ;

      Inc( cnt ) ;
    end
    else begin
      with bvlLights do begin
        Visible    := False ;
        Position.X := OFFWINDOW ; // for designer purposes
      end ;

      with chkLights do begin
        Visible    := False ;
        Position.X := OFFWINDOW ; // for designer purposes
      end ;

      with lblShadowIntensity do begin
        Visible    := False ;
        Position.X := OFFWINDOW ; // for designer purposes
      end ;

      with spnShadowIntensity do begin
        Visible    := False ;
        Position.X := OFFWINDOW ; // for designer purposes
      end ;
    end;

    // Wireframes and edges panel
    if TGIS_Control3DOption.ShowFrameModes in Options then begin
      if cnt > 0 then
        t := t + VERT_GAP ;
      with bvlFrameModes do begin
        Visible    := cnt > 0 ;
        Height     := BVL_SIZE ;
        Position.Y := t ;
        if Visible then
          PlaceControl(BiDiMode, nil, bvlFrameModes, self.ClipRect.Left, Self.Width)
        else
          PlaceControl(BiDiMode, nil, bvlFrameModes, OFFWINDOW, Self.Width);
      end ;
      if cnt > 0 then
        t := t + bvlFrameModes.Height + VERT_GAP ;

      with chkBasePlane do begin
        Visible      := True ;
        Width        := w ;
        Position.Y   := t ;
        Position.X   := l ;
        PlaceControl(BiDiMode, nil, chkBasePlane, l, w);
        if BiDiMode = TBiDiMode.bdRightToLeft then
          TextAlign := TTextAlign.Trailing
        else
          TextAlign := TTextAlign.Leading;
      end ;
      t := t + chkBasePlane.Height + VERT_GAP ;

      with spnBasePlane do begin
        Visible      := True ;
        Width        := w ;
        Position.Y   := t  ;
        Position.X   := l  ;
        PlaceControl(BiDiMode, nil, spnBasePlane, l, w);
      end ;
      t := t + spnBasePlane.Height + BTN1_GAP2 ;

      with bvlBasePlane do begin
        Visible    := cnt > 0 ;
        Height     := BVL_SIZE ;
        Position.Y := t ;
        if Visible then
          PlaceControl(BiDiMode, nil, bvlBasePlane, self.ClipRect.Left, Self.Width)
        else
          PlaceControl(BiDiMode, nil, bvlBasePlane, OFFWINDOW, Self.Width);
      end ;
      if cnt > 0 then
        t := t + bvlBasePlane.Height + VERT_GAP ;

      with chkWireframe do begin
        Visible      := False ;  // set True when this mode works
        Position.Y   := t ;
        PlaceControl(BiDiMode, nil, chkWireframe, l, w);
        if BiDiMode = TBiDiMode.bdRightToLeft then
          TextAlign := TTextAlign.Trailing
        else
          TextAlign := TTextAlign.Leading;
      end ;

      with chkEdges do begin
        Visible      := True ;
        Position.Y   := t ;
        PlaceControl(BiDiMode, nil, chkEdges, l, w);
        if BiDiMode = TBiDiMode.bdRightToLeft then
          TextAlign := TTextAlign.Trailing
        else
          TextAlign := TTextAlign.Leading;
      end ;
      t := t + chkEdges.Height + VERT_GAP ;

      Inc( cnt ) ;
    end
    else begin
      with bvlFrameModes do begin
        Visible    := False ;
        Position.X := OFFWINDOW ; // for designer purposes
      end ;

      with chkBasePlane do begin
        Visible    := False ;
        Position.X := OFFWINDOW ; // for designer purposes
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
        Visible    := False ;
        Position.X := OFFWINDOW ; // for designer purposes
      end ;

      with chkEdges do begin
        Visible    := False ;
        Position.X := OFFWINDOW ; // for designer purposes
      end ;
    end;

    // Scallings panel
    if TGIS_Control3DOption.ShowScalings in Options then begin
      if cnt > 0 then
        t := t + VERT_GAP ;
      with bvlScalings do begin
        Visible    := cnt > 0 ;
        Height     := BVL_SIZE ;
        Position.Y := t ;
        if Visible then
          PlaceControl(BiDiMode, nil, bvlScalings, self.ClipRect.Left, Self.Width)
        else
          PlaceControl(BiDiMode, nil, bvlScalings, OFFWINDOW, Self.Width)
      end ;
      if cnt > 0 then
        t := t + bvlScalings.Height + VERT_GAP ;

      with lblScaleZ do begin
        Visible      := True ;
        Position.Y   := t ;
        PlaceControl(BiDiMode, nil, lblScaleZ, l_lbl, w);
        if BiDiMode = TBiDiMode.bdRightToLeft then
          TextAlign := TTextAlign.Trailing
        else
          TextAlign := TTextAlign.Leading;
      end ;
      t := t + lblScaleZ.Height ;

      with spnScaleZ do begin
        Visible      := True ;
        Position.Y   := t  ;
        PlaceControl(BiDiMode, nil, spnScaleZ, l, w);
        if BiDiMode = TBiDiMode.bdRightToLeft then
          TextAlign := TTextAlign.Trailing
        else
          TextAlign := TTextAlign.Leading;
      end ;
      t := t + spnScaleZ.Height + VERT_GAP ;

      with lblScaleM do begin
        Visible      := True ;
        Position.Y   := t ;
        PlaceControl(BiDiMode, nil, lblScaleM, l_lbl, w);
        if BiDiMode = TBiDiMode.bdRightToLeft then
          TextAlign := TTextAlign.Trailing
        else
          TextAlign := TTextAlign.Leading;
      end ;
      t := t + lblScaleM.Height ;

      with spnScaleM do begin
        Visible      := True ;
        Position.Y   := t  ;
        PlaceControl(BiDiMode, nil, spnScaleM, l, w);
        if BiDiMode = TBiDiMode.bdRightToLeft then
          TextAlign := TTextAlign.Trailing
        else
          TextAlign := TTextAlign.Leading;
      end ;
      t := t + spnScaleM.Height + VERT_GAP ;

      Inc( cnt ) ;
    end
    else begin
      with bvlScalings do begin
        Visible      := False ;
        Position.X   := OFFWINDOW ; // for designer purposes
      end ;

      with lblScaleZ do begin
        Visible      := False ;
        Position.X   := OFFWINDOW ; // for designer purposes
      end ;

      with spnScaleZ do begin
        Visible      := False ;
        Position.X   := OFFWINDOW ; // for designer purposes
      end ;

      with lblScaleM do begin
        Visible      := False ;
        Position.X   := OFFWINDOW ; // for designer purposes
      end ;

      with spnScaleM do begin
        Visible      := False ;
        Position.X   := OFFWINDOW ; // for designer purposes
      end ;
    end ;

    // Floods panel
    if TGIS_Control3DOption.ShowFloods in Options then begin
      if cnt > 0 then
        t := t + VERT_GAP ;
      with bvlFloods do begin
        Visible    := cnt > 0 ;
        Height     := BVL_SIZE ;
        Position.Y := t ;
        if Visible then
          PlaceControl(BiDiMode, nil, bvlFloods, self.ClipRect.Left, Self.Width)
        else
          PlaceControl(BiDiMode, nil, bvlFloods, OFFWINDOW, Self.Width);
      end ;
      if cnt > 0 then
        t := t + bvlFloods.Height  + VERT_GAP ;

      with chkFlood do begin
        Visible      := True ;
        Position.Y   := t  ;
        PlaceControl(BiDiMode, nil, chkFlood, l, w);
        if BiDiMode = TBiDiMode.bdRightToLeft then begin
          TextAlign := TTextAlign.Trailing
        end
        else begin
          TextAlign := TTextAlign.Leading;
        end;
      end ;
      t := t + chkFlood.Height ;

      with spnFlood do begin
        Visible      := True ;
        Position.Y   := t  ;
        PlaceControl(BiDiMode, nil, spnFlood, l, w);
        if BiDiMode = TBiDiMode.bdRightToLeft then
          TextAlign := TTextAlign.Trailing
        else
          TextAlign := TTextAlign.Leading;
      end ;
      t := t + spnFlood.Height + VERT_GAP ;

      Inc( cnt ) ;
    end
    else begin
      with bvlFloods do begin
        Visible      := False ;
        Position.X   := OFFWINDOW ; // for designer purposes
      end ;

      with chkFlood do begin
        Visible      := False ;
        Position.X   := OFFWINDOW ; // for designer purposes
      end ;

      with spnFlood do begin
        Visible      := False ;
        Position.X   := OFFWINDOW ; // for designer purposes
      end ;
    end;

    // Walls panel
    if TGIS_Control3DOption.ShowWalls in Options then begin
      if cnt > 0 then
        t := t + VERT_GAP ;
      with bvlWalls do begin
        Visible    := cnt > 0 ;
        Height     := BVL_SIZE ;
        Position.Y := t ;
        if Visible then
          PlaceControl(BiDiMode, nil, bvlWalls, self.ClipRect.Left, Self.Width)
        else
          PlaceControl(BiDiMode, nil, bvlWalls, OFFWINDOW, Self.Width);
      end ;
      if cnt > 0 then
        t := t + bvlWalls.Height  + VERT_GAP;

      with lblWall do begin
        Visible      := True ;
        Position.Y   := t  ;
        PlaceControl(BiDiMode, nil, lblWall, l_lbl, w);
        if BiDiMode = TBiDiMode.bdRightToLeft then
          TextAlign := TTextAlign.Trailing
        else
          TextAlign := TTextAlign.Leading;
      end ;
      t := t + lblWall.Height ;

      with cmbWall do begin
        Visible      := True ;
        Position.Y   := t  ;
        PlaceControl(BiDiMode, nil, cmbWall, l, w);
        if BiDiMode = TBiDiMode.bdRightToLeft then
      //?    no text align property for comboboxes
        else
      //?    no text align property for comboboxes
      end ;
      t := t + cmbWall.Height + VERT_GAP ;

      // Inc( cnt ) ;
    end
    else begin
      with bvlWalls do begin
        Visible      := False ;
        Position.X   := OFFWINDOW ; // for designer purposes
      end ;

      with lblWall do begin
        Visible      := False ;
        Position.X   := OFFWINDOW ; // for designer purposes
      end ;

      with cmbWall do begin
        Visible      := False ;
        Position.X   := OFFWINDOW ; // for designer purposes
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

  procedure TGIS_Control3D.btnMouseEnter(
    _sender : TObject
  ) ;
  begin
    TCircle( _sender ).Fill.Color := $10000000 ;
  end;

  procedure TGIS_Control3D.btnMouseLeave(
    _sender : TObject
  ) ;
  begin
    TCircle( _sender ).Fill.Color := 0 ;
  end;

  procedure TGIS_Control3D.btnCenterMouseDown(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single      ;
    _y      : Single
  ) ;
  begin
    TCircle( _sender ).Fill.Color := $20000000 ;

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

  procedure TGIS_Control3D.btnCenterMouseUp(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single      ;
    _y      : Single
  ) ;
  begin
    TCircle( _sender ).Fill.Color := $10000000 ;
  end;

  procedure TGIS_Control3D.btnUpMouseDown(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single      ;
    _y      : Single
  ) ;
  begin
    TCircle( _sender ).Fill.Color := $20000000 ;

    iOperation := Ord( T_op.opUp ) ;
    enableTimer ;
  end;

  procedure TGIS_Control3D.btnUpMouseUp(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single      ;
    _y      : Single
  ) ;
  begin
    TCircle( _sender ).Fill.Color := $10000000 ;
    disableTimer ;
  end;

  procedure TGIS_Control3D.btnLeftMouseDown(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single      ;
    _y      : Single
  ) ;
  begin
    TCircle( _sender ).Fill.Color := $20000000 ;

    iOperation := Ord( T_op.opLeft ) ;
    enableTimer ;
  end;

  procedure TGIS_Control3D.btnLeftMouseUp(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single      ;
    _y      : Single
  ) ;
  begin
    TCircle( _sender ).Fill.Color := $10000000 ;
    disableTimer ;
  end;

  procedure TGIS_Control3D.btnRightMouseDown(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single      ;
    _y      : Single
  ) ;
  begin
    TCircle( _sender ).Fill.Color := $20000000 ;

    iOperation := Ord( T_op.opRight ) ;
    enableTimer ;
  end;

  procedure TGIS_Control3D.btnRightMouseUp(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single      ;
    _y      : Single
  ) ;
  begin
    TCircle( _sender ).Fill.Color := $10000000 ;
    disableTimer ;
  end;

  procedure TGIS_Control3D.btnDownMouseDown(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single      ;
    _y      : Single
  ) ;
  begin
    TCircle( _sender ).Fill.Color := $20000000 ;

    iOperation := Ord( T_op.opDown ) ;
    enableTimer ;
  end;

  procedure TGIS_Control3D.btnDownMouseUp(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single      ;
    _y      : Single
  ) ;
  begin
    TCircle( _sender ).Fill.Color := $10000000 ;
    disableTimer ;
  end;

  procedure TGIS_Control3D.btnPlusMouseDown(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single      ;
    _y      : Single
  ) ;
  begin
    TCircle( _sender ).Fill.Color := $20000000 ;

    iOperation := Ord( T_op.opPlus ) ;
    enableTimer ;
  end;

  procedure TGIS_Control3D.btnPlusMouseUp(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single      ;
    _y      : Single
  ) ;
  begin
    TCircle( _sender ).Fill.Color := $10000000 ;
    disableTimer ;
  end;

  procedure TGIS_Control3D.btnMinusMouseDown(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single      ;
    _y      : Single
  ) ;
  begin
    TCircle( _sender ).Fill.Color := $20000000 ;

    iOperation := Ord( T_op.opMinus ) ;
    enableTimer ;
  end;

  procedure TGIS_Control3D.btnMinusMouseUp(
    _sender : TObject      ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single      ;
    _y      : Single
  ) ;
  begin
    TCircle( _sender ).Fill.Color := $10000000 ;
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

    GIS_Viewer.Viewer3D.ShowLights := chkLights.IsChecked ;
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
    bp.Active := chkBasePlane.IsChecked ;
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

    GIS_Viewer.Viewer3D.ShowWireframe := chkWireframe.IsChecked ;
  end;

  procedure TGIS_Control3D.chkEdgesClick(
    _sender : TObject
  ) ;
  begin
    if not Assigned( GIS_Viewer ) then exit ;
    if not Assigned( GIS_Viewer.Viewer3D ) then exit ;

    GIS_Viewer.Viewer3D.ShowVectorEdges := chkEdges.IsChecked ;
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
    t.Active := chkFlood.IsChecked ;
    t.Level  := spnFlood.Value   ;
    GIS_Viewer.Viewer3D.Flood := t ;
  end ;

  procedure TGIS_Control3D.edtKeyDown(
        _sender : TObject ;
    var _key    : Word    ;
    var _keyChar: Char ;
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
            edtX.FontColor := TAlphaColorRec.Black ;
          except
            edtX.FontColor := TAlphaColorRec.Red ;
            exit ;
          end;

          try
            ref.Y := DegToRad( StrToFloat( edtY.Text ) ) ;
            edtY.FontColor := TAlphaColorRec.Black ;
          except
            edtY.FontColor := TAlphaColorRec.Red ;
            exit ;
          end;

          try
            ref.Z := StrToFloat( edtZ.Text )   ;
            edtZ.FontColor := TAlphaColorRec.Black ;
          except
            edtZ.FontColor := TAlphaColorRec.Red ;
            exit ;
          end;

          GIS_Viewer.Viewer3D.CameraPosition := ref ;
        end ;

      TGIS_Viewer3DMode.CameraXYZ :
        begin
          ref := GIS_Viewer.Viewer3D.CameraPositionEx ;

          try
            ref.X := StrToFloat( edtX.Text )   ;
            edtX.FontColor := TAlphaColorRec.Black ;
          except
            edtX.FontColor := TAlphaColorRec.Red ;
            exit ;
          end;

          try
            ref.Y := StrToFloat( edtY.Text )   ;
            edtY.FontColor := TAlphaColorRec.Black ;
          except
            edtY.FontColor := TAlphaColorRec.Red ;
            exit ;
          end;

          try
            ref.Z := StrToFloat( edtZ.Text )   ;
            edtZ.FontColor := TAlphaColorRec.Black ;
          except
            edtZ.FontColor := TAlphaColorRec.Red ;
            exit ;
          end;

          GIS_Viewer.Viewer3D.CameraPositionEx := ref ;
        end ;

      TGIS_Viewer3DMode.CameraRotation :
        begin
          ref := GIS_Viewer.Viewer3D.CameraRotation ;

          try
            ref.X := DegToRad( StrToFloat( edtX.Text ) ) ;
            edtX.FontColor := TAlphaColorRec.Black ;
          except
            edtX.FontColor := TAlphaColorRec.Red ;
            exit ;
          end;

          try
            ref.Y := DegToRad( StrToFloat( edtY.Text ) ) ;
            edtY.FontColor := TAlphaColorRec.Black ;
          except
            edtY.FontColor := TAlphaColorRec.Red ;
            exit ;
          end;

          try
            ref.Z := DegToRad( StrToFloat( edtZ.Text ) ) ;
            edtZ.FontColor := TAlphaColorRec.Black ;
          except
            edtZ.FontColor := TAlphaColorRec.Red ;
            exit ;
          end;

          GIS_Viewer.Viewer3D.CameraRotation := ref ;
        end ;

      TGIS_Viewer3DMode.SunPosition :
        begin
          ref := GisPoint3DFrom2D( GIS_Viewer.Viewer3D.SunPosition );

          try
            ref.X := DegToRad( StrToFloat( edtX.Text ) ) ;
            edtX.FontColor := TAlphaColorRec.Black ;
          except
            edtX.FontColor := TAlphaColorRec.Red ;
            exit ;
          end;

          try
            ref.Y := DegToRad( StrToFloat( edtY.Text ) ) ;
            edtY.FontColor := TAlphaColorRec.Black ;
          except
            edtY.FontColor := TAlphaColorRec.Red ;
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
    tmrUpdate.Enabled := False  ;

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

      chkLights.IsChecked    := GIS_Viewer.Viewer3D.ShowLights      ;
      chkBasePlane.IsChecked := GIS_Viewer.Viewer3D.BasePlane.Active ;
      if spnBasePlane.Value <> GIS_Viewer.Viewer3D.BasePlane.Level then
        spnBasePlane.Value := GIS_Viewer.Viewer3D.BasePlane.Level  ;

      chkWireframe.IsChecked := GIS_Viewer.Viewer3D.ShowWireframe   ;
      chkEdges.IsChecked     := GIS_Viewer.Viewer3D.ShowVectorEdges ;

      spnScaleZ.Value      := RoundS( GIS_Viewer.Viewer3D.ScaleZ * 100 ) ;
      spnScaleM.Value      := RoundS( GIS_Viewer.Viewer3D.ScaleM * 100 ) ;

      edtX.FontColor := TAlphaColorRec.Black ;
      edtY.FontColor := TAlphaColorRec.Black ;
      edtZ.FontColor := TAlphaColorRec.Black ;

      chkFlood.IsChecked := GIS_Viewer.Viewer3D.Flood.Active ;
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
          tmrUpdate.Enabled := True ;
        end ;
    end;
  end;


  procedure Register;
  begin
    RegisterComponents( 'TatukGIS', [TGIS_Control3D] ) ;
  end;

//==================================== END =====================================
end.


