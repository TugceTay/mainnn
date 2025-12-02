//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.85.0.33382-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// 
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================
{
  Visual control for displaying scale.
}

unit Lider.CG.GIS.VCL.GeoControlScale ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.VCL.GeoControlScale"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.Clipbrd,
  Vcl.Forms,

  {$IFDEF GIS_XDK}
    XDK.Core,
  {$ENDIF}

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoInterfaces,
  Lider.CG.GIS.GeoClasses,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoFunctions,
  Lider.CG.GIS.GeoCsBase,

  Lider.CG.GIS.GeoRendererAbstract,
  Lider.CG.GIS.VCL.GeoFramework,
  Lider.CG.GIS.VCL.GeoPrinters,
  Lider.CG.GIS.VCL.GeoViewerWnd;

type

  /// <summary>
  ///   Event fired on preparing size of TGIS_ControlSize component.
  /// </summary>
  /// <param name="_ptA">
  ///   starting point of distance measurement
  /// </param>
  /// <param name="_ptB">
  ///   ending of distance measurement
  /// </param>
  /// <param name="_distance">
  ///   computed distance
  /// </param>
  /// <param name="_units">
  ///   string with map units
  /// </param>
  TGIS_ControlScalePrepareEvent = procedure(
    const _ptA      : TGIS_Point ;
    const _ptB      : TGIS_Point ;
    var   _distance : Double ;
    var   _units    : String
  ) of object ;

  /// <summary>
  ///   Visual control for displaying map scale.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     Place this component on a form and connect GIS_Viewer to
  ///     TGIS_ViewerWnd.
  ///   </para>
  ///   <para>
  ///     Based on TGIS_Viewer.CS units settings the component will try to
  ///     compute the scale.
  ///   </para>
  /// </remarks>
  [ComponentPlatformsAttribute( pfidWindows )]
  TGIS_ControlScale = class( TCustomPanel, IGIS_Subscribe, IGIS_PrintableControl )
    private // properties internal value
      FVisible        : Boolean ;
      FFont           : TFont ;
      FGlow           : Boolean ;
      FTransparent    : Boolean ;
      FBitmap         : TBitmap  ;
      FGIS_Viewer     : TGIS_ViewerWnd ;
      FDividers       : Integer ;
      FDividerColor1  : TColor ;
      FDividerColor2  : TColor ;
      FUnits          : TGIS_CSUnits ;
      FUnitsEPSG      : Integer ;
      FInternalName   : String ;

      FOnPrepare      : TGIS_ControlScalePrepareEvent ;

    private
      forceUpdate     : Boolean ;
      oldRect         : TRect   ;
      oldPPI          : Integer ;
      oldFontScale    : Integer ;
      oldRenderer     : Integer ;
      oldEPSG         : Integer ;
      oldScale        : Double  ;
      oldScaleAsFloat : Double  ;
      oldColor        : TColor  ;
      oldFontColor    : TColor  ;
      oldFontName     : String  ;
      oldFontSize     : Integer ;
      oldFontStyle    : TFontStyles ;
      oldGlowColor    : TColor  ;
      oldUnitsFactor  : Double  ;

    protected // properties access routines
      procedure fset_GIS_Viewer   ( const _value : TGIS_ViewerWnd
                                  ) ;

      function  fget_Visible      : Boolean ;
      procedure fset_Visible      ( const _value : Boolean
                                  ) ;
      {$IFDEF GIS_XDK}
        function  fget_Hidden     : Boolean ;
        procedure fset_Hidden     ( const _value   : Boolean
                                  ) ;
      {$ENDIF}
      function  fget_Font         : TFont ;
      procedure fset_Font         ( const _value : TFont
                                  ) ;
      procedure fset_Glow         ( const _value : Boolean
                                  ) ;
      procedure fset_Transparent  ( const _value : Boolean
                                  ) ;
      procedure fset_Dividers     ( const _value : Integer
                                  ) ;
      procedure fset_DividerColor1( const _value : TColor
                                  ) ;
      procedure fset_DividerColor2( const _value : TColor
                                  ) ;
      function  fget_Units        : TGIS_CSUnits ;

      procedure fset_Units        ( const _value : TGIS_CSUnits
                                  ) ;
      function  fget_UnitsEPSG    : Integer ;
      procedure fset_UnitsEPSG    ( const _value : Integer
                                  ) ;

    private // IGIS_PrintableControl property access routines
      function  fget_InternalName: String ;
      procedure fset_InternalName( const _value : String
                                  ) ;

    private
      /// <summary>
      ///   Notify of properyt change (e.g. Font).
      /// </summary>
      procedure doChange          (      _sender : TObject ) ;

      /// <summary>
      ///   Fully repaint control (clear control cached bitmap).
      /// </summary>
      procedure doFullRepaint     ;

      /// <summary>
      ///   Subscribed TGIS_Viewer notification.
      /// </summary>
      /// <remarks>
      ///   Called upon TGIS_Viewer.Paint.
      /// </remarks>
      procedure doSubscribedAfterPaint
                                  ( const _context : TObject ) ;

      /// <summary>
      ///   Subscribed TGIS_Viewer notification.
      /// </summary>
      /// <remarks>
      ///   Called upon viewer zooing, dragging etc.
      /// </remarks>
      procedure doSubscribedTransparentPaint(
                                    const _context : TObject ;
                                    const _update  : Boolean
                                  ) ;

    private
      /// <summary>
      ///   True if component is in design mode.
      /// </summary>
      function isDesignMode : Boolean ;

    private
      /// <summary>
      ///   True, if object is upon destroy.
      /// </summary>
      uponDestroy : Boolean    ;
    public

      /// <summary>
      ///   Create the control.
      /// </summary>
      /// <param name="_owner">
      ///   control owner
      /// </param>
      constructor Create       ( _owner     : TComponent
                               ) ; override;

      /// <summary>
      ///   Destroy the control.
      /// </summary>
      destructor  Destroy      ; override;

    protected
      {#gendoc:hide}
      procedure   doCreate     ;
      procedure   doDestroy    ;

    protected
      /// <summary>
      ///   Paint the control.
      /// </summary>
      procedure Paint          ; override ;

    public
      /// <summary>
      ///   Invalidates contol.
      /// </summary>
      procedure Invalidate     ; override ;

      /// <summary>
      ///   Draw scale.
      /// </summary>
      procedure Draw           ; overload ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_PrintableControl"/>
      function CreateCopy      : IGIS_PrintableControl ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_PrintableControl"/>
      procedure FreeCopy       ( const _control : IGIS_PrintableControl
                               ) ;

      /// <inheritdoc from="IGIS_PrintableControl"/>
      procedure PrintBmp       ( const _bitmap  : TGIS_Bitmap
                               ) ;

      /// <summary>
      ///   Draw control on a provided bitmap.
      /// </summary>
      /// <param name="_bmp">
      ///   bitmap on which the drawing will be performed; if null then bitmap
      ///   will be created based on control size and returned by function
      /// </param>
      /// <param name="_scale">
      ///   scale of the map; if &lt;= 0 then map scale will be used
      /// </param>
      /// <param name="_ppi">
      ///   force PPI resolution; if 0 then set by corresponding GIS_Viewer
      ///   object
      /// </param>
      /// <returns>
      ///   Bitmap (newly create bitmap if _bmp is nil)
      /// </returns>
      function  DrawBmp        ( const _bmp     : TBitmap ;
                                 const _scale   : Double  ;
                                 const _ppi     : Integer
                               ) : TBitmap ;

      /// <summary>
      ///   Print control on the clipboard.
      /// </summary>
      /// <param name="_scale">
      ///   scale of the map; if &lt;= 0 then map scale will be used
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    See PrintDC for more advanced print options.
      ///    </note>
      /// </remarks>
      procedure PrintClipboard ( const _scale   : Double
                               ) ; virtual;

      /// <summary>
      ///   Update the control.
      /// </summary>
      procedure Update         ; override;

      /// <inheritdoc form="IGIS_Subscribe"/>
      procedure SubscribedEvent(       _sender  : TObject ;
                                       _event   : Integer ;
                                       _context : TObject
                               ) ;

    public
      /// <summary>
      ///   Bitmap. Valid only if control was created by Create with a
      ///   parameter.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Size of the bitmap is valid after redraw.
      ///    </note>
      /// </remarks>
      property Bitmap : TBitmap read FBitmap ;

      /// <summary>
      ///   Units used for scale output.
      /// </summary>
      property Units : TGIS_CSUnits read  fget_Units
                                    write fset_Units ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_PrintableControl"/>
      property InternalName : String
                                  read  fget_InternalName
                                  write fset_InternalName ;

    published
      /// <summary>
      ///   Attached viewer.
      /// </summary>
      property GIS_Viewer : TGIS_ViewerWnd read  FGIS_Viewer
                                           write fset_GIS_Viewer ;

      {$IFNDEF GIS_XDK}
        /// <summary>
        ///   Control visibility.
        /// </summary>
      {$ELSE}
        /// <summary>
        ///   Use property Hidden instead.
        /// </summary>
      {$ENDIF}
        property Visible    : Boolean
                              read  fget_Visible
                              write fset_Visible ;
      {$IFDEF GIS_XDK}
        /// <summary>
        ///   Turn control on/off. Use it instead of property Visible.
        /// </summary>
        property Hidden     : Boolean
                              read  fget_Hidden
                              write fset_Hidden ;
      {$ENDIF}


      /// <summary>
      ///   Number of dividers.
      /// </summary>
      property Dividers : Integer read    FDividers
                                  write   fset_Dividers
                                  default 5 ;

      /// <summary>
      ///   Color of the first divider.
      /// </summary>
      property DividerColor1 : TColor read    FDividerColor1
                                      write   fset_DividerColor1
                                      default clBlack ;

      /// <summary>
      ///   Color of the second divider.
      /// </summary>
      property DividerColor2 : TColor read    FDividerColor2
                                      write   fset_DividerColor2
                                      default clWhite ;

      /// <summary>
      ///   Draw "glowing" shadow around scale.
      /// </summary>
      property Glow        : Boolean read    FGlow
                                     write   fset_Glow
                                     default True ;

      /// <summary>
      ///   If True then control will be drawn as transparent.
      /// </summary>
      property Transparent : Boolean read    FTransparent
                                     write   fset_Transparent
                                     default True ;

      /// <summary>
      ///   Units EPSG code for scale output.
      ///   If set to 0 then automatic Matric or Imperial system will be used
      ///   based on location.
      /// </summary>
      property UnitsEPSG : Integer   read  fget_UnitsEPSG
                                     write fset_UnitsEPSG ;


      /// <event/>
      /// <summary>
      ///   Event fired before calculating scale.
      /// </summary>
      property   PrepareEvent : TGIS_ControlScalePrepareEvent
                                          read  FOnPrepare
                                          write FOnPrepare ;

    published // properties derived from base class
      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property Align ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property Anchors ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property BevelInner;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property BevelOuter;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property BevelWidth;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property BorderStyle;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property Ctl3D ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property Color default clWindow ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property Enabled ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property FullRepaint;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property Font : TFont read fget_Font write fset_Font ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property HelpContext ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property Hint ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property ParentColor ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property ParentCtl3D ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property ParentFont;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property ParentShowHint;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property PopupMenu ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property ShowHint;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property TabStop  ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property TabOrder ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property DragCursor ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property DragKind ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property DragMode ;

      /// <summary>
      ///   Standard VCL property. See platform documentation.
      /// </summary>
      property Touch  ;

    // events derived from base class
    published
      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnCanResize ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnClick
               {$IFDEF GENDOC}
                 : TNotifyEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnContextPopup;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnDblClick
               {$IFDEF GENDOC}
                 : TNotifyEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnEnter ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnExit ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnKeyDown
               {$IFDEF GENDOC}
                 : TKeyEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnKeyPress
               {$IFDEF GENDOC}
                 : TKeyPressEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnKeyUp
               {$IFDEF GENDOC}
                 : TKeyEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnMouseDown
               {$IFDEF GENDOC}
                 : TMouseEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnMouseMove
               {$IFDEF GENDOC}
                 : TMouseMoveEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnMouseUp
               {$IFDEF GENDOC}
                 : TMouseEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnMouseWheel
               {$IFDEF GENDOC}
                 : TMouseWheelEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnMouseWheelUp
               {$IFDEF GENDOC}
                 : TMouseWheelUpDownEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnMouseWheelDown
               {$IFDEF GENDOC}
                 : TMouseWheelUpDownEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnResize;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnDragDrop ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnDragOver ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnEndDrag ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnStartDrag ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnGesture ;

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

{$R GisControlScale_16x16.RES}

uses
  System.Math,
  System.Types,

  Lider.CG.GIS.VCL.GeoRendererGdi32,
  Lider.CG.GIS.VCL.GeoRendererGdiPlus,
  Lider.CG.GIS.VCL.GeoRendererDirect2D ;


constructor TGIS_ControlScale.Create( _owner : TComponent ) ;
begin
  inherited Create( _owner ) ;
  {$IFDEF LEVEL_XE3_RTL}
    StyleElements := [] ;
  {$ENDIF}

  doCreate ;

  inherited Font.Size := 8 ;

  FFont := TFont.Create ;
  FFont.PixelsPerInch := 96 ;
  FFont.Assign( inherited Font ) ;
  FFont.OnChange := doChange ;

  Height := 40  ;
  Width  := 185 ;
  forceUpdate := False ;
end ;

procedure TGIS_ControlScale.doCreate ;
begin
  if csDesigning in ComponentState then
    EnsureFramework ;

  DoubleBuffered := not IsWin11 ;

  ControlStyle := ControlStyle + [csNeedsBorderPaint];

  FVisible       := True    ;
  FGlow          := True    ;
  Transparent    := True    ;
  FDividers      := 5       ;
  FDividerColor1 := clBlack ;
  FDividerColor2 := clWhite ;

  if GisIsMetricSystem then
    FUnits  := CSUnitsList.ByEPSG( 904201 )
  else
    FUnits  := CSUnitsList.ByEPSG( 904202 ) ;

  FUnitsEPSG     := 0     ;

  FBitmap        := nil   ;
  uponDestroy    := False ;
end ;

destructor TGIS_ControlScale.Destroy ;
begin
  if not uponDestroy then
    doDestroy ;
  inherited Destroy ;
end ;

procedure TGIS_ControlScale.doDestroy ;
begin
  uponDestroy := True ;

  if Assigned( FGIS_Viewer ) then
    FGIS_Viewer.UnSubscribe( Self ) ;

  FreeObject( FBitmap ) ;
  FreeObject( FFont   ) ;
end ;

procedure TGIS_ControlScale.fset_GIS_Viewer(
  const _value : TGIS_ViewerWnd
) ;
begin
  if FGIS_Viewer = _value then exit ;

  if Assigned( FGIS_Viewer ) then
    FGIS_Viewer.UnSubscribe( Self ) ;
  FGIS_Viewer := _value ;
  if Assigned( FGIS_Viewer ) then
    FGIS_Viewer.Subscribe( Self ) ;

  doFullRepaint ;
end ;

function TGIS_ControlScale.fget_Visible
  : Boolean ;
begin
  Result := FVisible ;
end;

procedure TGIS_ControlScale.fset_Visible(
  const _value : Boolean
) ;
begin
  FVisible := _value ;
  if not Transparent then begin
    inherited Visible := _value  ;
  end
  else if Assigned( GIS_Viewer ) then
    GIS_Viewer.Invalidate ;
end;

{$IFDEF GIS_XDK}
  function TGIS_ControlScale.fget_Hidden
    : Boolean ;
  begin
    Result := not Visible ;
  end;

  procedure TGIS_ControlScale.fset_Hidden(
    const _value   : Boolean
  ) ;
  begin
    Visible := not _value ;
  end;
{$ENDIF}

function TGIS_ControlScale.fget_Font
  : TFont ;
begin
  Result := FFont ;
end;

procedure TGIS_ControlScale.fset_Font(
  const _value : TFont
) ;
begin
  FFont.Assign( _value ) ;

  doFullRepaint ;
end;

procedure TGIS_ControlScale.fset_Glow(
  const _value : Boolean
) ;
begin
  FGlow := _value ;

  doFullRepaint ;
end;

procedure TGIS_ControlScale.fset_Transparent(
  const _value : Boolean
) ;
begin
  FTransparent := _value ;
  if not isDesignMode then
  begin
    if FTransparent then
      inherited Visible := False
    else if FVisible then
      inherited Visible := True ;
  end ;

  doFullRepaint ;
end;

procedure TGIS_ControlScale.fset_Dividers(
  const _value : Integer
) ;
begin
  if FDividers = _value then exit ;

  if      _value < 1   then FDividers := 1
  else if _value > 100 then FDividers := 50
  else                      FDividers := _value ;

  doFullRepaint ;
end ;

procedure TGIS_ControlScale.fset_DividerColor1(
  const _value : TColor
) ;
begin
  if FDividerColor1 = _value then exit ;

  FDividerColor1 := _value ;

  doFullRepaint ;
end ;

procedure TGIS_ControlScale.fset_DividerColor2(
  const _value : TColor
) ;
begin
  if FDividerColor2 = _value then exit ;

  FDividerColor2 := _value ;

  doFullRepaint ;
end ;

function TGIS_ControlScale.fget_Units
  : TGIS_CSUnits ;
begin
  Result := FUnits ;
end ;

procedure TGIS_ControlScale.fset_Units(
  const _value : TGIS_CSUnits
) ;
begin
  if Assigned( _value ) then begin
    FUnits := _value ;
    FUnitsEPSG := _value.EPSG ;
  end
  else begin
    if GisIsMetricSystem then
      FUnits  := CSUnitsList.ByEPSG( 904201 )
    else
      FUnits  := CSUnitsList.ByEPSG( 904202 ) ;

    FUnitsEPSG := 0 ;
  end ;

  doFullRepaint ;
end ;

function TGIS_ControlScale.fget_UnitsEPSG
  : Integer ;
begin
  Result := FUnitsEPSG ;
end ;

procedure TGIS_ControlScale.fset_UnitsEPSG(
  const _value : Integer
) ;
var
  unt : TGIS_CSUnits ;
begin
  unt := CSUnitsList.ByEPSG( _value ) ;

  if Assigned( unt ) then begin
    FUnits := unt ;
    FUnitsEPSG := unt.EPSG ;
  end
  else begin
    if GisIsMetricSystem then
      FUnits  := CSUnitsList.ByEPSG( 904201 )
    else
      FUnits  := CSUnitsList.ByEPSG( 904202 ) ;

    FUnitsEPSG := 0 ;
  end ;

  doFullRepaint ;
end ;

function TGIS_ControlScale.fget_InternalName
  : String ;
begin
  Result := FInternalName ;
end ;


procedure TGIS_ControlScale.fset_InternalName(
  const _value : String
) ;
begin
  FInternalName := _value ;
end ;

procedure TGIS_ControlScale.doChange(
  _sender : TObject
) ;
begin
  doFullRepaint ;
end;

procedure TGIS_ControlScale.doFullRepaint ;
begin
  {$ifndef GIS_XDK}
    if not Assigned( Parent ) then exit ;
  {$endif}

  forceUpdate := True ;

  if isDesignMode then
    Update
  else if Assigned( GIS_Viewer ) then
    GIS_Viewer.Invalidate ;
end;

procedure TGIS_ControlScale.doSubscribedAfterPaint(
  const _context : TObject
) ;
begin
  if ( not Assigned( FGIS_Viewer ) ) or FGIS_Viewer.IsEmpty then exit ;

  if Transparent then exit ;

  {$IFDEF GIS_XDK}
    if Assigned( XDK ) then
      XDK.Control.Visible := not Transparent ;
  {$ENDIF}

  Paint ;
end ;

procedure TGIS_ControlScale.doSubscribedTransparentPaint(
  const _context : TObject ;
  const _update  : Boolean
) ;
var
  r      : TRect ;
  update : Boolean ;

  function nothing_changed
    : Boolean ;
  begin
    Result := True ;
    if ( r.Left <> oldRect.Left ) or
       ( r.Top <> oldRect.Top ) or
       ( r.Right <> oldRect.Right ) or
       ( r.Bottom <> oldRect.Bottom ) then
      Result := False ;
    if Result and
       ( ( oldPPI <> GIS_Viewer.PPI ) or
         ( oldFontScale <> GIS_Viewer.FontScale ) or
         ( oldEPSG <> GIS_Viewer.CS.EPSG ) or
         ( oldScale <> GIS_Viewer.Scale ) or
         ( oldScaleAsFloat <> GIS_Viewer.ScaleAsFloat ) or
         ( Glow and Transparent and
           ( oldGlowColor <> GIS_Viewer.Color ) ) ) then
      Result := False ;
    if Result and
       ( ( ( oldRenderer = 0 ) and not ( GIS_Viewer.Renderer is TGIS_RendererVclDirect2D ) ) or
         ( ( oldRenderer = 1 ) and not ( GIS_Viewer.Renderer is TGIS_RendererVclGdiPlus ) ) or
         ( ( oldRenderer = 2 ) and not ( GIS_Viewer.Renderer is TGIS_RendererVclGdi32 ) ) ) then
      Result := False ;
    if Result and
       ( ( oldColor <> Color ) or
         ( oldFontColor <> Font.Color ) or
         ( oldFontName  <> Font.Name  ) or
         ( oldFontSize  <> Font.Size  ) or
         ( oldFontStyle <> Font.Style ) or
         ( oldUnitsFactor <> FUnits.Factor ) ) then
      Result := False ;
  end ;

begin
  {$IFDEF GIS_XDK}
    if Assigned( XDK ) then
      if ( XDK.Control.Tag = 1 ) and ( not Transparent ) then
        XDK.Control.Visible := True ;
  {$ENDIF}

  if not Transparent then exit ;
  if not Visible then exit ;

  {$IFDEF GIS_XDK}
    if Assigned( XDK ) then
      XDK.Control.Visible := not Transparent ;
  {$ENDIF}

  if ( not Assigned( FGIS_Viewer ) ) or FGIS_Viewer.IsEmpty then exit ;

  if not Assigned( _context ) then begin
    FreeObject( FBitmap ) ;
    exit ;
  end ;

  r := ClientRect ;
  AdjustClientRect( r ) ;

  if not Assigned( FBitmap ) then
    update := True
  else
  if _update and
     not isDesignMode and
     not forceUpdate and
     nothing_changed then
    update := False
  else
    update := _update ;
  if update then begin

    FreeObject( FBitmap ) ;
    FBitmap := DrawBmp( nil, GIS_Viewer.Scale, 0 ) ;

    if not Assigned( FBitmap ) then
      exit ;

    forceUpdate := False ;

    VCLMakeCompatibleBitmap( FBitmap ) ;
  end;

  if _context is TD2DCanvas then
    TD2DCanvas( _context ).DrawBitmapPremultiplied(
      r.Left + self.Left - GIS_Viewer.Left,
      r.Top  + self.Top  - GIS_Viewer.Top,
      FBitmap
    )
  else
    TCanvas( _context ).Draw(
      r.Left + self.Left - GIS_Viewer.Left,
      r.Top  + self.Top  - GIS_Viewer.Top,
      FBitmap,
      255
    );
end ;

function TGIS_ControlScale.isDesignMode
  : Boolean ;
begin
  Result := csdesigning in ComponentState ;

  {$IFDEF GIS_XDK}
    if ( not Result ) and Assigned( XDK ) then
      Result := XDK.IsDesignMode ;
  {$ENDIF}
end;

procedure TGIS_ControlScale.Invalidate;
begin
  inherited;

  if Transparent and Assigned( GIS_Viewer ) then
    GIS_Viewer.InvalidateTopmost ;
end;

procedure TGIS_ControlScale.Paint ;
var
  r       : TRect ;
  bmp     : TBitmap ;
begin
  {$IFDEF GIS_XDK}
    if not isDesignMode then
      if Assigned( XDK ) then
        if Hidden then begin
          XDK.Control.Visible := False ;
          exit ;
        end;
  {$ENDIF}

  bmp := nil ;
  try
    inherited ;

    if isDesignMode or
       ( Assigned( GIS_Viewer ) and ( not GIS_Viewer.IsEmpty ) )
    then begin
      r := ClientRect ;
      AdjustClientRect( r ) ;

      if ( r.Width > 5 ) and ( r.Height > 5 ) then begin

        if not isDesignMode then
          bmp := DrawBmp( nil, GIS_Viewer.Scale, 0 )
        else
          bmp := DrawBmp( nil, 1/500000, 0 ) ;

        if assigned( bmp ) then
          Canvas.Draw( r.Left, r.Top, bmp );
      end;
    end;
  finally
    FreeObject( bmp ) ;
  end;
end ;

procedure TGIS_ControlScale.Update ;
begin
  if not Assigned( FBitmap ) then
    inherited ;

  Paint ;
end ;

procedure TGIS_ControlScale.Draw ;
begin
  Paint ;
end ;

function TGIS_ControlScale.CreateCopy
  : IGIS_PrintableControl ;
begin
  Result := TGIS_ControlScale.Create( Parent ) ;
  TGIS_ControlScale(Result).Parent := Parent ;
  TGIS_ControlScale(Result).InternalName := Name ;
  TGIS_ControlScale(Result).Visible := False ;
  TGIS_ControlScale(Result).GIS_Viewer := GIS_Viewer ;
  TGIS_ControlScale(Result).Font.Color := Font.Color ;
  TGIS_ControlScale(Result).Font.Name := Font.Name ;
  TGIS_ControlScale(Result).Font.Size := Font.Size ;
  TGIS_ControlScale(Result).Font.Style := Font.Style ;
  TGIS_ControlScale(Result).Units := Units ;
  TGIS_ControlScale(Result).UnitsEPSG := UnitsEPSG ;
end ;

procedure TGIS_ControlScale.FreeCopy(
  const _control : IGIS_PrintableControl
) ;
var
  scale : TGIS_ControlScale ;
begin
  scale := TGIS_ControlScale( _control ) ;
  FreeObject( scale ) ;
end ;

procedure TGIS_ControlScale.PrintBmp(
  const _bitmap : TGIS_Bitmap
) ;
var
  btrans : Boolean ;
begin
  assert( assigned( _bitmap ) ) ;
  btrans := Transparent ;
  try
    DrawBmp( VCL.Graphics.TBitmap(_bitmap.NativeBitmap),
             GIS_Viewer.ScaleAsFloat, GIS_Viewer.PPI ) ;
  finally
    Transparent := btrans ;
  end;
end ;

function TGIS_ControlScale.DrawBmp(
  const _bmp   : TBitmap ;
  const _scale : Double  ;
  const _ppi   : Integer
) : TBitmap ;
var
  i         : Integer     ;
  r_tmp     : TRect       ;
  r         : TRect       ;
  bmp       : TGIS_Bitmap ;
  ppi       : Integer     ;
  fontscale : Integer     ;
  iscs      : Boolean     ;

  rnd       : TGIS_RendererAbstract ;
  {$IFNDEF LEVEL_XE7_RTL}
    arpt    : TGIS_DrawBuf ;
  {$ENDIF}
  ctx       : TGIS_RendererContext ;

  x, y      : Integer ;
  dx, dy    : Integer ;
  sx        : Integer ;
  ddx       : Double  ;
  dtxt      : Integer ;
  htxt      : Integer ;
  w, h      : Integer ;
  scale     : Double  ;
  distance1 : Double  ;
  distance2 : Double  ;
  txt_out   : String  ;
  rct_txt   : TRect   ;
  ount      : TGIS_CSUnits ;

  cl_color         : TGIS_Color ;
  cl_textcolor     : TGIS_Color ;
  cl_divdiercolor1 : TGIS_Color ;
  cl_divdiercolor2 : TGIS_Color ;
  cl_glow          : TGIS_Color ;

  function prepare_distance(
    const _width : Integer
  ) : Double ;
  begin
    Result := (1/scale) / ( 100/2.54 * ppi) *
              _width *
              1/ ount.Factor ;
  end;

  function pixels_to_twips(
    const _size : Integer
  ) : Double ;
  begin
    Result := 1.0*_size * 1440 / ppi ;
  end ;

  function normalize_distance(
    const _distance : Double
  ) : Double ;
  var
    ii      : Integer ;
    m1      : Boolean ;
    m2      : Boolean ;
    txt     : TStringBuilder ;
  begin
    txt := TStringBuilder.Create( DotFloatToStr( _distance / FDividers ) ) ;
    try
      m1 := True  ;
      m2 := False ;
      for ii := 0 to txt.Length - 1 do begin
        case txt[ii] of
          '0'      : continue ;
          '1'..'9' : begin
                       if m1 and m2 then txt[ii] := '0' ;
                       m2 := True ;
                     end ;
          '.' : continue ;
          else  m1 := False ;
        end ;
      end ;

      Result := DotStrToFloat( txt.ToString ) * FDividers ;
    finally
      FreeObject( txt ) ;
    end ;
  end ;

begin
  Result := _bmp ;

  if Assigned( Result ) then begin
    r := Rect( 0, 0, Result.Width , Result.Height  ) ;
  end
  else begin
    r_tmp := ClientRect ;
    AdjustClientRect( r_tmp ) ;
    r := Rect( 0, 0, r_tmp.Width , r_tmp.Height ) ;
  end;

  if not isDesignMode then begin
    if _ppi = 0 then begin
      ppi       := GIS_Viewer.PPI ;
      fontscale := GIS_Viewer.FontScale ;
    end
    else begin
      ppi       := _ppi ;
      fontscale := 100 ;
    end ;
  end
  else begin
    ppi       := Screen.PixelsPerInch ;
    fontscale := 100 ;
  end ;

  rnd := nil ;
  bmp := TGIS_Bitmap.Create( r.Width, r.Height, True );
  try

    if Assigned( GIS_Viewer ) then begin
      if Assigned( _bmp ) and
         ( GIS_Viewer.Renderer is TGIS_RendererVclDirect2D ) then
        // only for printing
        rnd := TGIS_RendererVclGdiPlus.Create
      else
        rnd :=  GIS_Viewer.Renderer.CreateInstance ;
    end else
      rnd := TGIS_RendererVclGdi32.Create ;

    if not Assigned( rnd ) then
      exit ;

    ctx := TGIS_RendererContext.Create ;
    try
      iscs := Assigned( GIS_Viewer ) and ( GIS_Viewer.CS.EPSG <> 0 ) ;

      ctx.AssignBaseMap( bmp.NativeBitmap, False ) ;

      rnd.CreateContext( nil, nil, ctx, Point( 0, 0 ),
                         RoundS( r.Width), RoundS( r.Height ),
                         ppi, fontscale
                       ) ;

      cl_color         := GISColor( Color ) ;
      cl_textcolor     := GISColor( Font.Color ) ;
      cl_divdiercolor1 := GISColor( DividerColor1 ) ;
      cl_divdiercolor2 := GISColor( DividerColor2 ) ;

      rnd.CanvasFont.Name := Font.Name ;
      rnd.CanvasFont.Size := Abs( Font.Size ) ;
      rnd.CanvasFont.Style := GISFontStyle( Font.Style );

      txt_out := '' ;

      rnd.CanvasBrush.Color := cl_color ;
      rnd.CanvasBrush.Style := TGIS_BrushStyle.Solid ;
      rnd.CanvasPen.Color   := cl_color ;
      rnd.CanvasPen.Style   := TGIS_PenStyle.Solid ;

      if ( not Transparent ) or isDesignMode then begin
        rnd.CanvasDrawRectangle( r );
      end ;

      scale := _scale ;

      if scale <= 0  then begin
        if Assigned( GIS_Viewer ) then
          scale := GIS_Viewer.ScaleAsFloat ;
      end ;

      if scale <= 0 then exit ;

      w := ( r.Right - r.Left ) - 2 * rnd.CanvasTextExtent( '0' ).Y ;
      w := w - w div 10 ;
      if w < 5 then exit ;

      ount := FUnits ;

      // prepare starting value
      distance1 := prepare_distance( w ) ;

      // select best matching unit
      if iscs then
        ount := FUnits.AutoSelect( False, distance1 ) ;

      // recalculate bar sizes
      distance1 := prepare_distance( w ) ;
      distance2 := normalize_distance( distance1 ) ;

      if iscs then
        txt_out := ount.AsLinear( distance2 * ount.Factor, False )
      else
        txt_out := FloatToStr( distance2 ) ;


      if distance2 = 0 then exit ;

      w := RoundS( w * distance2 / distance1 ) ;
      ddx  := 1.0*w / FDividers ;
      dx   := TruncS( ddx ) ;

      sx   := r.Left + ( ( r.Right - r.Left ) - dx * FDividers ) div 2 ;
      x    := sx ;

      htxt := rnd.CanvasTextExtent( txt_out ).Y ;
      dy   := System.Math.Max( 3, htxt div 4 ) ;
      dtxt := System.Math.Max( 2, htxt div 8 ) ;
      h    := dy + dtxt + htxt ;

      y    := System.Math.Max( r.Top,
                               r.Top + ( r.Bottom - r.Top ) div 2 - h div 2
                             ) ;

      rnd.CanvasBrush.Style  := TGIS_BrushStyle.Solid ;
      rnd.CanvasPen.Style    := TGIS_PenStyle.Solid ;
      rnd.CanvasPen.LineJoin := TGIS_LineJoin.Bevel ;
      rnd.CanvasPen.Width    := 1       ;
      rnd.CanvasPen.Color    := cl_textcolor ;

      for i:=1 to FDividers do begin
        if i mod 2 = 1 then
          rnd.CanvasBrush.Color.ARGB := cl_divdiercolor1.ARGB
        else
          rnd.CanvasBrush.Color.ARGB := cl_divdiercolor2.ARGB ;

        {$IFNDEF LEVEL_XE7_RTL}
          SetLength( arpt, 5 );
          arpt[0] := Point( x     , y      ) ;
          arpt[1] := Point( x + dx, y      ) ;
          arpt[2] := Point( x + dx, y + dy ) ;
          arpt[3] := Point( x     , y + dy ) ;
          arpt[4] := Point( x     , y      ) ;
          rnd.CanvasDrawPolygon( arpt ) ;
        {$ELSE}
          rnd.CanvasDrawPolygon(
             [ Point( x     , y      ), Point( x + dx, y      ),
               Point( x + dx, y + dy ), Point( x     , y + dy ),
               Point( x     , y      )
             ]
          ) ;
        {$ENDIF}

        x := sx + i * dx ;
      end ;
      rnd.CanvasBrush.Color := cl_color ;
      rnd.CanvasPen.Color := rnd.CanvasBrush.Color ;
      rnd.CanvasBrush.Style := TGIS_BrushStyle.Clear ;
      rnd.CanvasFont.Color :=  cl_textcolor ;

      rct_txt := Rect(
                   r.Left +
                   ( r.Right - r.Left - rnd.CanvasTextExtent( txt_out).X ) div 2 ,
                   y + dy + System.Math.Max( 2, rnd.CanvasTextExtent( 'Ay').Y div 8 ),
                   r.Right,
                   r.Bottom
                 ) ;
      rnd.CanvasDrawText( rct_txt, txt_out ) ;

      rnd.ReleaseContext ;
    finally
      if rnd is TGIS_RendererVclDirect2D then
        oldRenderer := 0
      else
      if rnd is TGIS_RendererVclGdiPlus then
        oldRenderer := 1
      else
        oldRenderer := 2 ;
      FreeObject( rnd ) ;
      FreeObject( ctx );
    end ;

    if not Assigned( Result ) then
      Result := TBitmap.Create ;

    if Glow and Transparent then begin
      if Assigned( GIS_Viewer ) then
        cl_glow := GISColor( GIS_Viewer.Color )
      else
        cl_glow := TGIS_Color.White ;
      cl_glow := TGIS_Color.FromARGB( 128, cl_glow.R, cl_glow.G, cl_glow.B ) ;
      bmp.MakeGlowing( cl_glow, 5 * ppi div 96 ) ;
    end ;

    Result.Assign( TBitmap( bmp.NativeBitmap ) ) ;

    oldRect := r_tmp ;
    oldPPI := ppi ;
    oldFontScale := fontscale ;
    if assigned( GIS_Viewer ) then begin
      oldEPSG := GIS_Viewer.CS.EPSG ;
      oldScale := GIS_Viewer.Scale ;
      oldScaleAsFloat := GIS_Viewer.ScaleAsFloat ;
      oldGlowColor := GIS_Viewer.Color ;
    end
    else begin
      oldEPSG := 0 ;
      oldScale := 0 ;
      oldScaleAsFloat := 0 ;
      oldGlowColor := clWhite ;
    end ;
    oldColor := Color ;
    oldFontColor := Font.Color ;
    oldFontName := Font.Name ;
    oldFontSize := Font.Size ;
    oldFontStyle := Font.Style ;
    oldUnitsFactor := FUnits.Factor ;

  finally
    FreeObject( bmp );
  end ;

end ;

procedure TGIS_ControlScale.PrintClipboard(
  const _scale : Double
) ;
var
  bmp : TBitmap ;
begin
  bmp := nil ;
  try
    bmp := DrawBmp( nil, _scale, 0 ) ;
    Clipboard.Assign( bmp ) ;
  finally
    FreeObject( bmp ) ;
  end ;
end ;

procedure  TGIS_ControlScale.SubscribedEvent(
  _sender  : TObject ;
  _event   : Integer ;
  _context : TObject
) ;
begin
  case _event of
    GIS_SUBSCRIBED_DESTROY :
      begin
        FGIS_Viewer := nil ;
      end ;
    GIS_SUBSCRIBED_AFTERPAINT :
      begin
        // ignore events from a basemap thread
        if assigned( _context ) then
          doSubscribedAfterPaint( _context ) ;
      end;
    GIS_SUBSCRIBED_TRANSPARENT_CONTROL_PAINT :
      begin
        if Transparent then
          doSubscribedTransparentPaint( _context, False )
        else
          doSubscribedAfterPaint( _context ) ;
      end ;
    GIS_SUBSCRIBED_TRANSPARENT_CONTROL_UPDATE :
      begin
        if Transparent then
          doSubscribedTransparentPaint( _context, True  )
        else
          doSubscribedAfterPaint( _context ) ;
      end ;
  end ;
end ;

procedure Register ;
begin
  RegisterComponents( 'TatukGIS', [ TGIS_ControlScale ] ) ;
end ;

{==================================== END =====================================}
end.


