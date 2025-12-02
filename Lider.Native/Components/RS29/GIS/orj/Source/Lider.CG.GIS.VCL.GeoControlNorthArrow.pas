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
  Visual control for displaying north arrow.
}

unit VCL.GisControlNorthArrow ;
{$HPPEMIT '#pragma link "VCL.GisControlNorthArrow"'}

{$INCLUDE GisInclude.inc}

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  VCL.Graphics,
  VCL.Controls,
  VCL.ExtCtrls,
  VCL.Clipbrd,

  {$IFDEF GIS_XDK}
    XDK.Core,
  {$ENDIF}

  GisRtl,
  GisInterfaces,
  GisFunctions,
  GisClasses,
  GisTypes,
  GisTypesUI,
  GisLayerVector,
  GisSymbol,
  GisParams,
  VCL.GisFramework,
  VCL.GisViewerWnd,
  VCL.GisViewerBmp;

type

  /// <summary>
  ///   Visual control for displaying map North Arrow.
  /// </summary>
  /// <remarks>
  ///   Place this component on a form and connect GIS_Viewer to
  ///   TGIS_ViewerWnd.
  /// </remarks>
  [ComponentPlatformsAttribute( pfidWindows )]
  TGIS_ControlNorthArrow = class( TCustomPanel, IGIS_Subscribe,
                                  IGIS_PrintableControl )
    private // properties internal value
      FVisible      : Boolean ;
      FGlow         : Boolean ;
      FTransparent  : Boolean ;
      FBitmap       : TGIS_Bitmap  ;
      FGIS_Viewer   : TGIS_ViewerWnd ;
      FColor1       : TColor   ;
      FColor2       : TColor   ;
      FPath         : String   ;
      FStyle        : TGIS_ControlNorthArrowStyle ;
      FInternalName : String   ;

    private // other private values
      lstSymbolStreams : TObjectList<TResourceStream> ;

    private
      forceUpdate     : Boolean ;
      oldRect         : TRect   ;
      oldPPI          : Integer ;
      oldGISWidth     : Integer ;
      oldGISHeight    : Integer ;
      oldAngle        : Double  ;
      oldGlowColor    : TColor  ;
      oldRenderer     : String  ;
      oldColor        : TColor  ;
      bLock           : Boolean ;
      cpyStyle        : TGIS_ControlNorthArrowStyle ;
      cpyColor1       : TColor ;
      cpyColor2       : TColor ;
      cpyPath         : String ;
      cpyBitmap       : TGIS_Bitmap ;

    protected // properties access routines

      function  fget_Bitmap       : TBitmap ;
      procedure fset_GIS_Viewer   ( const _value   : TGIS_ViewerWnd
                                  ) ;
      function  fget_Visible      : Boolean ;
      procedure fset_Visible      ( const _value   : Boolean
                                  ) ;
      {$IFDEF GIS_XDK}
        function  fget_Hidden     : Boolean ;
        procedure fset_Hidden     ( const _value   : Boolean
                                  ) ;
      {$ENDIF}
      procedure fset_Glow         ( const _value   : Boolean
                                  ) ;
      procedure fset_Transparent  ( const _value   : Boolean
                                  ) ;
      procedure fset_Color1       ( const _value   : TColor
                                  ) ;
      procedure fset_Color2       ( const _value   : TColor
                                  ) ;
      procedure fset_Style        ( const _value   : TGIS_ControlNorthArrowStyle
                                  ) ;
      procedure fset_Path         ( const _value   : String
                                  ) ;

    private // IGIS_PrintableControl property access routines
      function  fget_InternalName : String ;
      procedure fset_InternalName ( const _value : String
                                  ) ;

    private
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
      procedure doSubscribedAfterPaint(
                                    const _context : TObject
                                  ) ;
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
      procedure Paint          ; override;


    public

      /// <summary>
      ///   Draw north arrow.
      /// </summary>
      procedure Draw           ; overload;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_PrintableControl"/>
      function  CreateCopy     : IGIS_PrintableControl ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_PrintableControl"/>
      procedure FreeCopy       ( const _control : IGIS_PrintableControl
                               ) ;

      /// <inheritdoc from="IGIS_PrintableControl"/>
      procedure PrintBmp       ( const _bitmap  : TGIS_Bitmap
                               ) ;

      /// <summary>
      ///   Draw control on a provided bitmap
      /// </summary>
      /// <param name="_bmp">
      ///   bitmap on which the drawing will be performed; if null then bitmap
      ///   will be created based on control size and returned by function
      /// </param>
      /// <param name="_ppi">
      ///   force PPI resolution; if 0 then set by corresponding GIS_Viewer
      ///   object
      /// </param>
      /// <returns>
      ///   Bitmap (newly create bitmap if _bmp is nil)
      /// </returns>
      function  DrawBmp        ( const _bmp     : TBitmap ;
                                 const _ppi     : Integer
                               ) : TBitmap ; overload ;

      /// <summary>
      ///   Draw control on a provided bitmap
      /// </summary>
      /// <param name="_bmp">
      ///   bitmap on which the drawing will be performed; if null then bitmap
      ///   will be created based on control size and returned by function
      /// </param>
      /// <param name="_ppi">
      ///   force PPI resolution; if 0 then set by corresponding GIS_Viewer
      ///   object
      /// </param>
      /// <returns>
      ///   Bitmap (newly create bitmap if _bmp is nil)
      /// </returns>
      function  DrawBmp        ( const _bmp     : TGIS_Bitmap ;
                                 const _ppi     : Integer
                               ) : TGIS_Bitmap ; overload ;

      /// <summary>
      ///   Print control clipboard.
      /// </summary>
      procedure PrintClipboard ;

      /// <summary>
      ///   Update the control.
      /// </summary>
      procedure Update         ; override;

      /// <inheritdoc form="IGIS_Subscribe"/>
      procedure SubscribedEvent  ( _sender  : TObject ;
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
      property Bitmap : TBitmap read fget_Bitmap ;

      /// <summary>
      ///   Bitmap. Valid only if control was created by Create with a
      ///   parameter.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Size of the bitmap is valid after redraw.
      ///    </note>
      /// </remarks>
      property GIS_Bitmap : TGIS_Bitmap read FBitmap ;

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
      property GIS_Viewer   : TGIS_ViewerWnd
                              read  FGIS_Viewer
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
      ///   Fill color.
      /// </summary>
      property Color1       : TColor
                              read    FColor1
                              write   fset_Color1
                              default clBlack ;

      /// <summary>
      ///   Outline color.
      /// </summary>
      property Color2       : TColor
                              read    FColor2
                              write   fset_Color2
                              default clBlack ;

      /// <summary>
      ///   Symbol style, used if Path is empty.
      /// </summary>
      property Style        : TGIS_ControlNorthArrowStyle
                              read  FStyle
                              write fset_Style ;

      /// <summary>
      ///   Symbol path. If empty then Style property is used.
      /// </summary>
      property Path         : String
                              read  FPath
                              write fset_Path ;

      /// <summary>
      ///   Draw "glowing" shadow around the symbol.
      /// </summary>
      property Glow         : Boolean
                              read    FGlow
                              write   fset_Glow
                              default True ;
      /// <summary>
      ///   If True then the control will be drawn as transparent.
      /// </summary>
      property Transparent  : Boolean
                              read    FTransparent
                              write   fset_Transparent
                              default True ;

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
      property Font;

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

    published // events derived from base class

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

{$R GisControlNorthArrow_16x16.RES}
{$R GisControlNorthArrow_SVG.RES}

uses
  System.Math,
  VCL.GisRendererGdiPlus ;

constructor TGIS_ControlNorthArrow.Create( _owner : TComponent ) ;
begin
  inherited Create( _owner ) ;

  {$IFDEF LEVEL_XE3_RTL}
  StyleElements := [] ;
  {$ENDIF}
  doCreate ;

  Width  := 128  ;
  Height := 128  ;
  forceUpdate := False ;
end ;

procedure TGIS_ControlNorthArrow.doCreate ;
begin
  if isDesignMode then
    EnsureFramework ;

  DoubleBuffered := not IsWin11 ;

  ControlStyle := ControlStyle + [csNeedsBorderPaint] ;

  bLock          := False   ;
  FVisible       := True    ;
  FGlow          := True    ;
  Transparent    := True    ;
  FColor1        := clBlack ;
  FColor2        := clBlack ;

  FBitmap := nil    ;

  lstSymbolStreams := TObjectList<TResourceStream>.Create( True ) ;

  lstSymbolStreams.Add(
    TResourceStream.Create(hInstance, 'NORTHARROW_ARROW1'   , RT_RCDATA)
  ) ;
  lstSymbolStreams.Add(
    TResourceStream.Create(hInstance, 'NORTHARROW_ARROW2'   , RT_RCDATA)
  ) ;
  lstSymbolStreams.Add(
    TResourceStream.Create(hInstance, 'NORTHARROW_NEEDLE1'  , RT_RCDATA)
  ) ;
  lstSymbolStreams.Add(
    TResourceStream.Create(hInstance, 'NORTHARROW_NEEDLE2'  , RT_RCDATA)
  ) ;
  lstSymbolStreams.Add(
    TResourceStream.Create(hInstance, 'NORTHARROW_NEEDLE3'  , RT_RCDATA)
  ) ;
  lstSymbolStreams.Add(
    TResourceStream.Create(hInstance, 'NORTHARROW_ROSE1'    , RT_RCDATA)
  ) ;
  lstSymbolStreams.Add(
    TResourceStream.Create(hInstance, 'NORTHARROW_ROSE2'    , RT_RCDATA)
  ) ;
  lstSymbolStreams.Add(
    TResourceStream.Create(hInstance, 'NORTHARROW_ROSE3'    , RT_RCDATA)
  ) ;
  lstSymbolStreams.Add(
    TResourceStream.Create(hInstance, 'NORTHARROW_DISK1'    , RT_RCDATA)
  ) ;
  lstSymbolStreams.Add(
    TResourceStream.Create(hInstance, 'NORTHARROW_DISK2'    , RT_RCDATA)
  ) ;
  lstSymbolStreams.Add(
    TResourceStream.Create(hInstance, 'NORTHARROW_DISK3'    , RT_RCDATA)
  ) ;
  lstSymbolStreams.Add(
    TResourceStream.Create(hInstance, 'NORTHARROW_TRIANGLE1', RT_RCDATA)
  ) ;

  FStyle := TGIS_ControlNorthArrowStyle.Arrow1 ;

  uponDestroy := False ;
end ;

destructor TGIS_ControlNorthArrow.Destroy ;
begin
  if not uponDestroy then
    doDestroy ;
  inherited Destroy ;
end ;

procedure TGIS_ControlNorthArrow.doDestroy ;
begin
  uponDestroy := True ;

  if Assigned( FGIS_Viewer ) then
    FGIS_Viewer.UnSubscribe( Self ) ;

  FreeObject( FBitmap ) ;

  FreeObject( lstSymbolStreams ) ;
end ;

function TGIS_ControlNorthArrow.fget_Bitmap
  : TBitmap ;
begin
  Result := TBitmap( FBitmap.NativeBitmap ) ;
end ;

procedure TGIS_ControlNorthArrow.fset_GIS_Viewer(
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

function TGIS_ControlNorthArrow.fget_Visible
  : Boolean ;
begin
  Result := FVisible ;
end;

procedure TGIS_ControlNorthArrow.fset_Visible(
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
  function TGIS_ControlNorthArrow.fget_Hidden
    : Boolean ;
  begin
    Result := not Visible ;
  end;

  procedure TGIS_ControlNorthArrow.fset_Hidden(
    const _value   : Boolean
  ) ;
  begin
    Visible := not _value ;
  end;
{$ENDIF}

procedure TGIS_ControlNorthArrow.fset_Glow(
  const _value : Boolean
) ;
begin
  FGlow := _value ;

  doFullRepaint ;
end;

procedure TGIS_ControlNorthArrow.fset_Transparent(
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
  end;


  if isDesignMode then begin
    forceUpdate := True ;
    Invalidate ;
  end else
    doFullRepaint ;
end;

procedure TGIS_ControlNorthArrow.fset_Color1(
  const _value   : TColor
) ;
begin
  FColor1 := _value ;

  doFullRepaint ;
end;

procedure TGIS_ControlNorthArrow.fset_Color2(
  const _value   : TColor
) ;
begin
  FColor2 := _value ;

  doFullRepaint ;
end;

procedure TGIS_ControlNorthArrow.fset_Style(
  const _value : TGIS_ControlNorthArrowStyle
) ;
begin
  FStyle := _value ;

  doFullRepaint ;
end;

procedure TGIS_ControlNorthArrow.fset_Path(
  const _value : String
) ;
begin
  FPath := _value ;

  doFullRepaint ;
end;

function TGIS_ControlNorthArrow.fget_InternalName
  : String ;
begin
  Result := FInternalName ;
end ;


procedure TGIS_ControlNorthArrow.fset_InternalName(
  const _value : String
) ;
begin
  FInternalName := _value ;
end ;

procedure TGIS_ControlNorthArrow.doFullRepaint ;
begin
  forceUpdate := True ;

  if isDesignMode then
    Update
  else if Assigned( GIS_Viewer ) then
    GIS_Viewer.Invalidate ;
end ;

procedure TGIS_ControlNorthArrow.doSubscribedAfterPaint(
  const _context : TObject
) ;
begin
  if ( not Assigned( FGIS_Viewer ) ) or  FGIS_Viewer.IsEmpty then exit ;

  if Transparent then exit ;

  {$IFDEF GIS_XDK}
    if Assigned( XDK ) then
      XDK.Control.Visible := not Transparent ;
  {$ENDIF}

  Paint ;
end ;

procedure TGIS_ControlNorthArrow.doSubscribedTransparentPaint(
  const _context : TObject ;
  const _update  : Boolean
) ;
var
  r      : TRect ;
  update : Boolean ;

  function nothing_changed
    : Boolean ;
  var
    r : TRect ;
  begin
    Result := True ;
    r := ClientRect ;
    if ( r.Left <> oldRect.Left ) or
       ( r.Top <> oldRect.Top ) or
       ( r.Right <> oldRect.Right ) or
       ( r.Bottom <> oldRect.Bottom ) then
      Result := False ;
    if Result and
       ( ( oldPPI <> GIS_Viewer.PPI ) or
         ( oldGISWidth <> GIS_Viewer.Width ) or
         ( oldGISHeight <> GIS_Viewer.Height ) or
         ( oldAngle <> GIS_Viewer.RotationAngle ) or
         ( Glow and Transparent and
           ( oldGlowColor <> GIS_Viewer.Color ) ) ) then
      Result := False ;
    if Result and
       assigned( GIS_Viewer ) and
       ( oldRenderer <> GIS_Viewer.Renderer.FriendlyName ) then
      Result := False ;
    if Result and
       ( oldColor <> Color ) then
      Result := False ;
  end;
begin
  {$IFDEF GIS_XDK}
    if Assigned( XDK ) then
      if ( XDK.Control.Tag = 1 ) and ( not Transparent ) then
        XDK.Control.Visible := True ;
  {$ENDIF}

  if not Transparent then exit ;
  if not Visible then exit ;
  if ( not Assigned( FGIS_Viewer ) ) or FGIS_Viewer.IsEmpty then exit ;

  {$IFDEF GIS_XDK}
    if Assigned( XDK ) then
      XDK.Control.Visible := not Transparent ;
  {$ENDIF}

  if not Assigned( _context ) then begin
    FreeObject( FBitmap ) ;
    exit ;
  end ;

  r := Rect( 0, 0, Width, Height ) ;
  AdjustClientRect( r ) ;

  if bLock then begin
    if assigned( cpyBitmap ) then
      GIS_Viewer.Renderer.ControlDrawTransparent(
        _context, cpyBitmap,
        r.Left + self.Left - GIS_Viewer.Left,
        r.Top  + self.Top  - GIS_Viewer.Top
      ) ;
    exit ;
  end ;

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
    FBitmap := DrawBmp( FBitmap, 0 ) ;

    forceUpdate := False ;
  end;

  GIS_Viewer.Renderer.ControlDrawTransparent(
    _context, FBitmap,
    r.Left + self.Left - GIS_Viewer.Left,
    r.Top  + self.Top  - GIS_Viewer.Top
  ) ;
end ;

procedure TGIS_ControlNorthArrow.Paint ;
var
  r   : TRect ;
  bmp : TGIS_Bitmap ;
begin
  {$IFDEF GIS_XDK}
    if not isDesignMode then
      if Assigned( XDK ) then
        if Hidden then begin
          XDK.Control.Visible := False ;
          exit ;
        end;
  {$ENDIF}

  if bLock then begin
    if assigned( cpyBitmap ) then
      Canvas.Draw( 0, 0, TBitmap(cpyBitmap.NativeBitmap) ) ;
    exit ;
  end ;

  bmp := nil ;
  try
    inherited ;

    if isDesignMode or
       ( Assigned( GIS_Viewer ) and ( not GIS_Viewer.IsEmpty ) )
    then begin
      r := Rect( 0, 0, Width, Height ) ;
      AdjustClientRect( r ) ;

      if ( r.Width > 5 ) and ( r.Height > 5 ) then begin

        bmp := DrawBmp( bmp, 0 ) ;

        if assigned( bmp ) then begin
          if not Assigned( GIS_Viewer ) then begin
            Canvas.Draw( r.Left, r.Top, TBitmap(bmp.NativeBitmap) ) ;
          end
          else
            GIS_Viewer.Renderer.ControlDrawNonTransparent( Canvas, bmp, r.Left, r.Top ) ;
        end ;
      end;
    end;
  finally
    FreeObject( bmp ) ;
  end;
end ;

function TGIS_ControlNorthArrow.isDesignMode
  : Boolean ;
begin
  Result := csdesigning in ComponentState ;

  {$IFDEF GIS_XDK}
    if ( not Result ) and Assigned( XDK ) then
      Result := XDK.IsDesignMode ;
  {$ENDIF}
end;

procedure TGIS_ControlNorthArrow.Update ;
begin
  if not Assigned( FBitmap ) then
    inherited ;

  Paint ;
end ;

procedure TGIS_ControlNorthArrow.Draw ;
begin
  Paint ;
end ;

function TGIS_ControlNorthArrow.CreateCopy
  : IGIS_PrintableControl ;
begin
  bLock := True ;
  cpyStyle  := Style ;
  cpyColor1 := Color1 ;
  cpyColor2 := Color2 ;
  cpyPath   := Path ;
  if assigned( FBitmap ) then
    cpyBitmap := TGIS_Bitmap.Create( FBitmap )
  else
    cpyBitmap := nil ;
  InternalName := Name ;
  Result := Self ;
end ;

procedure TGIS_ControlNorthArrow.FreeCopy(
  const _control : IGIS_PrintableControl
) ;
begin
  FreeObject( cpyBitmap ) ;
  Style  := cpyStyle ;
  Color1 := cpyColor1 ;
  Color2 := cpyColor2 ;
  Path   := cpyPath   ;
  InternalName := '' ;
  bLock := False ;
  Repaint ;
end ;

procedure TGIS_ControlNorthArrow.PrintBmp(
  const _bitmap : TGIS_Bitmap
) ;
var
  btrans : Boolean ;
begin
  assert( assigned( _bitmap ) ) ;
  btrans := Transparent ;
  try
    DrawBmp( _bitmap, 0 ) ;
  finally
    Transparent := btrans ;
  end;
end ;

function TGIS_ControlNorthArrow.DrawBmp(
  const _bmp : TBitmap ;
  const _ppi : Integer
) : TBitmap ;
var
  bmp : TGIS_Bitmap ;
begin
  if assigned( _bmp ) then begin
    bmp := TGIS_Bitmap.Create ;
    bmp.NativeBitmap := _bmp ;
    bmp := DrawBmp( bmp, _ppi ) ;
    _bmp.Assign( TBitmap( bmp.NativeBitmap ) ) ;
    Result := _bmp ;
    FreeObject( bmp ) ;
  end
  else begin
    bmp := nil ;
    bmp := DrawBmp( bmp, _ppi ) ;
    Result := TBitmap.Create ;
    Result.Assign( TBitmap( bmp.NativeBitmap ) ) ;
    FreeObject( bmp ) ;
  end ;
end ;

function TGIS_ControlNorthArrow.DrawBmp(
  const _bmp : TGIS_Bitmap ;
  const _ppi : Integer
) : TGIS_Bitmap ;
var
  r_tmp    : TRect       ;
  r        : TRect       ;
  pt       : TPoint      ;
  pt_a     : TGIS_Point  ;
  pt_b     : TGIS_Point  ;
  vwr      : TGIS_ViewerBmp ;
  shp      : TGIS_Shape  ;
  lv       : TGIS_LayerVector ;
  bmp_glow : TGIS_Bitmap ;
  cl_glow  : TGIS_Color  ;
begin
  Result := _bmp ;

  r_tmp := ClientRect ;
  if Assigned( Result ) then begin
    r := Rect( 0, 0, Result.Width , Result.Height  ) ;
  end
  else begin
    r := Rect( 0, 0, Width, Height ) ;
    AdjustClientRect( r ) ;
  end;

  vwr := TGIS_ViewerBmp.Create( r.Width, r.Height );
  try
    if Assigned( GIS_Viewer ) then begin
      vwr.Renderer := GIS_Viewer.Renderer.CreateInstance ;
      vwr.CustomPPI := GIS_Viewer.PPI ;
    end
    else
      vwr.Renderer := TGIS_RendererVclGdiPlus.Create ;
    {$IFDEF DCC}
    // verify; for printing
    if assigned( _bmp ) and
       ( _bmp.BitmapFactory <> vwr.Renderer.BitmapFactory ) then begin
      vwr.Renderer := TGIS_RendererVclGdiPlus.Create ;
    end ;
    {$ENDIF}

    if _ppi <> 0 then
      vwr.CustomPPI := _ppi ;

    lv := TGIS_LayerVector.Create ;
    vwr.Add( lv ) ;
    lv.Extent := GisExtent( -90, -90, 90, 90 ) ;
    vwr.FullExtent ;
    shp := lv.CreateShape( TGIS_ShapeType.Point ) ;
    shp.AddPart ;
    shp.AddPoint( GisPoint( 0, 0 ) );

    if IsStringEmpty( FPath ) then
      shp.Params.Marker.Symbol :=
        SymbolList.Prepare( Format( 'priv://northarrowres_%d.svg',
                                    [ Ord( FStyle ) ]
                                  ),
                            TStream( lstSymbolStreams[ Ord( FStyle )] )
                          )
    else
      shp.Params.Marker.Symbol :=
        SymbolList.Prepare( FPath ) ;

    if not Assigned( shp.Params.Marker.Symbol ) then
      shp.Params.Marker.Symbol :=
         SymbolList.Prepare( 'Wingdings:225:NORMAL' ) ;

    if not Assigned( shp.Params.Marker.Symbol ) then
      exit ;

    shp.Params.Marker.Size :=
      - Min( vwr.ControlCanvasWidth, vwr.ControlCanvasHeight ) * 8 div 9 ;

    if (not Transparent ) or isDesignMode then
      vwr.Color := GISColor( Color )
    else
      vwr.Color := TGIS_Color.None ;

    shp.Params.Marker.Color        := GISColor( Color1  ) ;
    shp.Params.Marker.OutlineColor := GISColor( Color2  ) ;

    if ( not Assigned( GIS_Viewer ) ) or GIS_Viewer.IsEmpty then
      shp.Params.Marker.SymbolRotate := 0
    else begin
      pt := Point( GIS_Viewer.Width div 2, GIS_Viewer.Height div 2 ) ;
      pt_a := GIS_Viewer.ScreenToMap( pt ) ;
      pt_b := GIS_Viewer.ScreenToMap( Point( pt.X, pt.Y + 10 ) ) ;
      pt_a := GIS_Viewer.CS.ToWGS( pt_a ) ;
      pt_b := GIS_Viewer.CS.ToWGS( pt_b ) ;
      shp.Params.Marker.SymbolRotate :=
        ArcTan2( pt_a.Y - pt_b.Y, pt_a.X - pt_b.X )
        + GIS_Viewer.RotationAngle
        - Pi/2 ;
    end ;

    vwr.Draw ;

    if not Assigned( Result ) then
      Result := TGIS_Bitmap.Create ;

    if Glow and Transparent then begin
      bmp_glow := TGIS_Bitmap.Create ;
      try
        bmp_glow.Assign( vwr.GIS_Bitmap ) ;
        bmp_glow.Premultiplied := True ;

        if Assigned( GIS_Viewer ) then
          cl_glow := GisColor( GIS_Viewer.Color )
        else
          cl_glow := TGIS_Color.White ;
        cl_glow := TGIS_Color.FromARGB( 128, cl_glow.R, cl_glow.G, cl_glow.B ) ;
        bmp_glow.MakeGlowing( cl_glow, 5 * vwr.PPI div 96 ) ;
        Result.Assign( bmp_glow );
      finally
        FreeObject( bmp_glow ) ;
      end;
    end
    else
      Result.Assign( vwr.GIS_Bitmap ) ;

    oldRect := r_tmp ;
    if assigned( GIS_Viewer ) then begin
      oldPPI := GIS_Viewer.PPI ;
      oldGISWidth := GIS_Viewer.Width ;
      oldGISHeight := GIS_Viewer.Height ;
      oldAngle := GIS_Viewer.RotationAngle ;
      oldGlowColor := GIS_Viewer.Color ;
    end
    else begin
      oldPPI := 0 ;
      oldGISWidth := 0 ;
      oldGISHeight := 0 ;
      oldAngle := 0 ;
      oldGlowColor := clWhite ;
    end;
    oldRenderer := vwr.Renderer.FriendlyName ;
    oldColor := Color ;
  finally
    FreeObject( vwr );
  end ;
end ;

procedure TGIS_ControlNorthArrow.PrintClipboard ;
var
  bmp : TBitmap ;
begin
  bmp := nil ;
  try
    bmp := DrawBmp( bmp, 0 ) ;
    Clipboard.Assign( bmp ) ;
  finally
    FreeObject( bmp ) ;
  end ;
end ;

procedure  TGIS_ControlNorthArrow.SubscribedEvent(
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
      end ;
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
  RegisterComponents( 'TatukGIS', [ TGIS_ControlNorthArrow ] ) ;
end ;

{==================================== END =====================================}
end.

