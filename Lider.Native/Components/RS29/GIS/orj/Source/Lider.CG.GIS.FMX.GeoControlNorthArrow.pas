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

unit FMX.GisControlNorthArrow ;
{$HPPEMIT '#pragma link "FMX.GisControlNorthArrow"'}

{$INCLUDE GisInclude.inc}

interface

uses
  System.Classes,
  System.UITypes,
  System.Types,
  System.SysUtils,
  System.Generics.Collections,
  FMX.Controls,
  FMX.Types,
  FMX.Objects,
  FMX.Graphics,

  GisRtl,
  GisInterfaces,
  GisFunctions,
  GisClasses,
  GisTypes,
  GisTypesUI,
  GisLayerVector,
  GisSymbol,
  GisViewer,
  FMX.GisFramework,
  FMX.GisViewerWnd,
  FMX.GisViewerBmp ;

type
  {$IFDEF LEVEL_RX10_FMX}
    /// <summary>
    ///   Styled settings.
    /// </summary>
    TGIS_ControlNorthArrowStyledSetting = (

      /// <summary>
      ///   Set symbol color.
      /// </summary>
      Color
    ) ;

    /// <summary>
    ///   Set of styled settings.
    /// </summary>
    TGIS_ControlNorthArrowStyledSettings = set of TGIS_ControlNorthArrowStyledSetting ;
  {$ENDIF}

  /// <summary>
  ///   Visual control for displaying map North Arrow.
  /// </summary>
  /// <remarks>
  ///   Place this component on a form and connect GIS_Viewer to
  ///   TGIS_ViewerWnd.
  /// </remarks>
  [ComponentPlatformsAttribute( pidAllPlatforms )]
  TGIS_ControlNorthArrow = class( TStyledControl, IGIS_Subscribe, IGIS_PrintableControl )
    private // properties internal value
      FGIS_Viewer  : TGIS_ViewerWnd ;
      FGlow        : Boolean ;
      FColor1      : TAlphaColor ;
      FColor2      : TAlphaColor ;
      FPath        : String   ;
      FStyle       : TGIS_ControlNorthArrowStyle ;
      {$IFDEF LEVEL_RX10_FMX}
        FStyledSettings : TGIS_ControlNorthArrowStyledSettings ;
      {$ENDIF}

      FStyledColor : TAlphaColor ;
    private // other private values
      oBitmap      : TBitmap  ;
      inPaint      : Boolean ;
      lstSymbolStreams : TObjectList< TResourceStream > ;


    protected // properties access routines

      procedure fset_GIS_Viewer   ( const _value   : TGIS_ViewerWnd
                                  ) ;
      procedure fset_Glow         ( const _value   : Boolean
                                  ) ;
      procedure fset_Color1       ( const _value   : TAlphaColor
                                  ) ;
      procedure fset_Color2       ( const _value   : TAlphaColor
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
                                    _context : TObject
                                  ) ;

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
      {#ownership:_owner:ownif_empty}
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
      procedure DoPaint        ; override;

      {$IFDEF LEVEL_RX10_FMX}
        {#gendoc:hide}
        procedure DoStyleChanged ; override;
      {$ENDIF}

    public

      /// <inheritdoc from="IGIS_PrintableControl"/>
      function CreateCopy      : IGIS_PrintableControl ;

      /// <inheritdoc from="IGIS_PrintableControl"/>
      procedure FreeCopy       ( const _control : IGIS_PrintableControl
                               ) ;

      /// <inheritdoc from="IGIS_PrintableControl"/>
      procedure PrintBmp       ( const _bitmap : TGIS_Bitmap
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
                               ) : TBitmap ;

      /// <summary>
      ///   Print control cliboard.
      /// </summary>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNIMPLEMENTED if not supported.
      /// </exception>
      /// <remarks>
      ///   Available only on RAD Studio 10.1 Seattle or newer.
      /// </remarks>
      procedure PrintClipboard ;

      /// <inheritdoc form="IGIS_Subscribe"/>
      procedure SubscribedEvent  (       _sender  : TObject ;
                                         _event   : Integer ;
                                         _context : TObject
                                 ) ;

    published
      /// <summary>
      ///   Attached viewer.
      /// </summary>
      property GIS_Viewer   : TGIS_ViewerWnd
                              read  FGIS_Viewer
                              write fset_GIS_Viewer ;

      /// <summary>
      ///   Draw "glowing" shadow around  the symbol.
      /// </summary>
      property Glow         : Boolean
                              read    FGlow
                              write   fset_Glow
                              default True ;
      /// <summary>
      ///   Fill color.
      /// </summary>
      property Color1       : TAlphaColor
                              read    FColor1
                              write   fset_Color1 ;

      /// <summary>
      ///   Outline color.
      /// </summary>
      property Color2       : TAlphaColor
                              read    FColor2
                              write   fset_Color2 ;
                              //default clBlack ;

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

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_PrintableControl"/>
      property InternalName : String
                                  read  fget_InternalName
                                  write fset_InternalName ;

      {$IFDEF LEVEL_RX10_FMX}
        /// <summary>
        ///   Styled settings.
        /// </summary>
        property StyledSettings : TGIS_ControlNorthArrowStyledSettings
                              read  FStyledSettings
                              write FStyledSettings ;
      {$ENDIF}


    published // inherited from TControl
      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Align;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Anchors;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property ClipChildren default False;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property ClipParent default False;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Cursor default crDefault;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property DragMode default TDragMode.dmManual;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property EnableDragHighlight default True;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Enabled default True;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Locked default False;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Height;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property HitTest default False;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Padding;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Opacity;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Margins;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property PopupMenu;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Position;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property RotationAngle;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property RotationCenter;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Scale;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Size;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Visible;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Width;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnDragEnter;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnDragLeave;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnDragOver;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnDragDrop;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnDragEnd;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnClick;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnDblClick;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnMouseDown;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnMouseMove;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnMouseUp;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnMouseWheel;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnMouseEnter;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnMouseLeave;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnPainting;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnPaint;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnResize;

  end ;

  {#gendoc:hide}
  procedure Register;

//##############################################################################
implementation

{$IFDEF MSWINDOWS}
  {$R GisControlNorthArrow_16x16.RES}
{$ENDIF}
{$R GisControlNorthArrow_SVG.RES}

uses
  FMX.Surfaces,
  {$IFDEF LEVEL_RX101_FMX}
    FMX.Clipboard,
  {$ENDIF}
  FMX.Platform,

  System.Math,

  GisResource ;

//==============================================================================
// TGIS_ControlNorthArrow
//==============================================================================

constructor TGIS_ControlNorthArrow.Create( _owner : TComponent ) ;
begin
  inherited Create( _owner ) ;

  doCreate ;

  Width  := 128  ;
  Height := 128  ;
end ;

procedure TGIS_ControlNorthArrow.doCreate ;
begin
  if csDesigning in ComponentState then
    EnsureFramework ;

  HitTest := False   ;
  Visible := True    ;

  FGlow   := True    ;

  FColor1 := TAlphaColorRec.Black ;
  FColor2 := TAlphaColorRec.Black ;

  oBitmap := nil    ;
  inPaint := False  ;

  lstSymbolStreams := TObjectList< TResourceStream >.Create( True ) ;

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

  {$IFDEF LEVEL_RX10_FMX}
    FStyledSettings := [] ;
  {$ENDIF}

  uponDestroy := False ;
end ;

destructor TGIS_ControlNorthArrow.Destroy ;
begin
  if not uponDestroy then
    doDestroy ;

  {$IFDEF GIS_PDK}
    RemoveFreeNotifications ;
  {$ENDIF}

  inherited Destroy ;
end ;

procedure TGIS_ControlNorthArrow.doDestroy ;
begin
  uponDestroy := True ;

  if Assigned( FGIS_Viewer ) then
    FGIS_Viewer.UnSubscribe( Self ) ;

  FreeObject( oBitmap ) ;

  FreeObject( lstSymbolStreams ) ;
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

procedure TGIS_ControlNorthArrow.fset_Glow(
  const _value   : Boolean
) ;
begin
  FGlow := _value ;

  doFullRepaint ;
end;

procedure TGIS_ControlNorthArrow.fset_Color1(
  const _value   : TAlphaColor
) ;
begin
  FColor1 := _value ;

  doFullRepaint ;
end;

procedure TGIS_ControlNorthArrow.fset_Color2(
  const _value   : TAlphaColor
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
  Result := 'nothing' ;
end ;

procedure TGIS_ControlNorthArrow.fset_InternalName(
  const _value : String
) ;
begin

end ;

procedure TGIS_ControlNorthArrow.doFullRepaint ;
begin
  FreeObject( oBitmap ) ;

  inPaint := True ;
  try
    oBitmap := DrawBmp( nil, 0 ) ;
  finally
    inPaint := False ;
  end;

  Repaint ;
end ;

procedure TGIS_ControlNorthArrow.doSubscribedAfterPaint(
  _context : TObject
) ;
begin
  doFullRepaint ;
end ;

procedure TGIS_ControlNorthArrow.DoPaint ;
var
  r   : TRect ;
begin
  inherited ;

  inPaint := True ;
  try
    if ( csDesigning in ComponentState ) or
       ( Assigned( GIS_Viewer ) and ( not GIS_Viewer.IsEmpty ) )
    then begin
      r := Rect( RoundS( 0  ),
                 RoundS( 0  ),
                 RoundS( Width   ),
                 RoundS (Height  )
               ) ;

      if ( r.Width > 5 ) and ( r.Height > 5 ) then begin
        if ( csDesigning in ComponentState ) then
          FreeObject( oBitmap );

        if not Assigned( oBitmap ) then
          oBitmap := DrawBmp( nil, 0 ) ;

        if assigned( oBitmap ) then
          Canvas.DrawBitmap(
            oBitmap,
            TRectF.Create( 0, 0, oBitmap.Width, oBitmap.Height ),
            TRectF.Create( r.Left, r.Top, r.Right, r.Bottom ),
            1
          ) ;
      end;
    end;
  finally
    inPaint := False ;
  end;
end ;

{$IFDEF LEVEL_RX10_FMX}
procedure TGIS_ControlNorthArrow.DoStyleChanged;
var
  stl    : TFmxObject  ;
  stl2   : TFmxObject  ;
  stlwnd : TControl    ;
  stlctl : TControl    ;
  bmp    : T_FMXBitmap ;
  dat    : TBitmapData ;
  c      : TGIS_Color  ;
  cnt    : Integer     ;
  r,g,b  : Integer     ;
  x,y    : Integer     ;
begin
  FreeObject( oBitmap );

  inherited ;

  FStyledColor := TAlphaColorRec.Black ;

  stl := TStyledControl.LookupStyleObject(
           Self, Self, Scene, '', 'backgroundstyle', '', False
         );

  if not ( Assigned( stl ) and ( stl is TControl ) ) then
    exit ;

  stlwnd := TControl( stl ) ;

  stl := TStyledControl.LookupStyleObject(
           Self, Self, Scene, '', 'listboxstyle.background', '', False
         );

  if not ( Assigned( stl ) and ( stl is TControl ) ) then
    exit ;

  stlctl := TControl( stl ) ;

  stl2 := stl.FindStyleResource( 'content') ;
  if Assigned( stl2 ) and ( stl2 is TControl ) then
    TControl( stl2 ).Visible := False ;

  bmp := T_FMXBitmap.Create( 16, 16 ) ;
  try
    bmp.Canvas.BeginScene ;
    try
      stlwnd.PaintTo( bmp.Canvas, RectF( 0, 0, bmp.Width, bmp.Height ) ) ;
      stlctl.PaintTo( bmp.Canvas, RectF( 0, 0, bmp.Width, bmp.Height ) ) ;
    finally
      bmp.Canvas.EndScene ;
    end;

    bmp.Map( TMapAccess.Read, dat);
    try
      cnt := 0  ;
      r := 0 ;
      g := 0;
      b := 0 ;
      for x := RoundS( 2 + stlctl.Padding.Left )
          to RoundS( bmp.Width - stlctl.Padding.Right - 1 )
      do begin
        for y := RoundS( 2 + stlctl.Padding.Top )
           to RoundS( bmp.Height - stlctl.Padding.Bottom - 1 )
        do begin
          c := GISColor( dat.GetPixel( x-1, y-1 ) );
          r := r + c.fget_R ;
          g := g + c.fget_G ;
          b := b + c.fget_B ;
          Inc(cnt);
        end;
      end;
      FStyledColor := FMXColor(
                        TGIS_Color.FromRGB(
                          r div cnt,
                          g div cnt,
                          b div cnt
                        )
                      ) ;
    finally
      bmp.Unmap(dat);
    end;
  finally
    FreeObject( bmp ) ;
  end;

  stl := TStyledControl.LookupStyleObject(
           Self, Self, Scene, '', 'listboxitemstyle.text', '', False
         );

  if Assigned( stl ) and ( stl is TText ) then
    FStyledColor := TText( stl ).TextSettings.FontColor ;
end;
{$ENDIF}

function TGIS_ControlNorthArrow.CreateCopy
  : IGIS_PrintableControl ;
begin

end ;

procedure TGIS_ControlNorthArrow.FreeCopy(
  const _control : IGIS_PrintableControl
) ;
begin

end ;

procedure TGIS_ControlNorthArrow.PrintBmp(
  const _bitmap : TGIS_Bitmap
) ;
begin
  DrawBmp( TBitmap(_bitmap.NativeBitmap), 0 ) ;
end ;

function TGIS_ControlNorthArrow.DrawBmp(
  const _bmp   : TBitmap ;
  const _ppi   : Integer
) : TBitmap ;
var
  r        : TRect             ;
  pt       : TPoint            ;
  pt_a     : TGIS_Point        ;
  pt_b     : TGIS_Point        ;
  vwr      : TGIS_ViewerBmp    ;
  pvwr     : IGIS_ViewerParent ;
  shp      : TGIS_Shape        ;
  lv       : TGIS_LayerVector  ;
  bmp_glow : TGIS_Bitmap       ;
  cl_glow  : TGIS_Color        ;

begin
  Result := _bmp ;

  if Assigned( Result ) then begin
    r := Rect( 0, 0, Result.Width , Result.Height  ) ;
  end
  else begin
    if Assigned( GIS_Viewer ) then begin
      pvwr := GIS_Viewer ;
      r := Rect( RoundS( 0  ),
                 RoundS( 0  ),
                 RoundS( Width  * pvwr.ControlCanvasScale ),
                 RoundS (Height * pvwr.ControlCanvasScale )
               ) ;
    end
    else
      r := Rect( RoundS( 0  ),
                 RoundS( 0  ),
                 RoundS( Width  ),
                 RoundS (Height )
               ) ;
  end;

  vwr := TGIS_ViewerBmp.Create( r.Width, r.Height );
  try
    if Assigned( GIS_Viewer ) then begin
      vwr.CustomPPI := GIS_Viewer.PPI ;
    end;

    vwr.Color := TGIS_Color.None ;

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

    shp.Params.Marker.Color        := TGIS_Color.FromARGB( Color1 ) ;
    shp.Params.Marker.OutlineColor := TGIS_Color.FromARGB( Color2 ) ;

    {$IFDEF LEVEL_RX10_FMX}
      if ( TGIS_ControlNorthArrowStyledSetting.Color in StyledSettings )
         and inPaint
      then begin
        shp.Params.Marker.Color        :=
          TGIS_Color.FromARGB( FStyledColor ) ;
        shp.Params.Marker.OutlineColor :=
          TGIS_Color.FromARGB( FStyledColor ) ;
      end ;
    {$ENDIF}

    if ( not Assigned( GIS_Viewer ) ) or GIS_Viewer.IsEmpty then
      shp.Params.Marker.SymbolRotate := 0
    else begin
      pt := Point( RoundS(GIS_Viewer.Width / 2), RoundS(GIS_Viewer.Height / 2) ) ;
      pt_a := GIS_Viewer.ScreenToMap( pt ) ;
      pt_b := GIS_Viewer.ScreenToMap( Point( pt.X, pt.Y + 10 ) ) ;
      pt_a := GIS_Viewer.CS.ToWGS( pt_a ) ;
      pt_b := GIS_Viewer.CS.ToWGS( pt_b ) ;
      shp.Params.Marker.SymbolRotate :=
        ArcTan2( pt_a.Y - pt_b.Y, pt_a.X - pt_b.X )
        + GIS_Viewer.RotationAngleEx
        - Pi/2
    end ;

    vwr.Draw ;

    if not Assigned( Result ) then
      Result := TBitmap.Create ;

    if Glow then begin
      bmp_glow := TGIS_Bitmap.Create ;
      try
        bmp_glow.LoadFromBitmap( TBitmap(vwr.Bitmap),''  ) ;
        bmp_glow.Premultiplied := True ;

        if Assigned( GIS_Viewer ) and inPaint then
          cl_glow := GISColor( GIS_Viewer.ActiveBackgroundColor )
        else
          cl_glow := TGIS_Color.White ;
        cl_glow := TGIS_Color.FromARGB( 128, cl_glow.R, cl_glow.G, cl_glow.B ) ;

        bmp_glow.MakeGlowing( cl_glow, 5 * vwr.PPI div 96 ) ;

        Result.Assign( TBitmap(bmp_glow.NativeBitmap) );
      finally
        FreeObject( bmp_glow ) ;
      end;
    end
    else
      Result.Assign( TBitmap(vwr.Bitmap) ) ;
  finally
    FreeObject( vwr );
  end ;
end ;

procedure TGIS_ControlNorthArrow.PrintClipboard ;
{$IFNDEF LEVEL_RX101_FMX}
  begin
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNIMPLEMENTED ) );
  end;
{$ELSE}
  var
    bmp : T_FMXBitmap ;
    srf : TBitmapSurface ;
    clp : IFMXExtendedClipboardService ;
  begin
    clp := IFMXExtendedClipboardService(
             TPlatformServices.Current.GetPlatformService(
               IFMXExtendedClipboardService
             )
           ) ;

    if not Assigned( clp ) then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNIMPLEMENTED ) );
      exit ;
    end ;

    srf := nil ;
    bmp := nil ;
    try
      bmp := DrawBmp( nil, 0 ) ;

      srf := TBitmapSurface.Create ;
      srf.Assign( bmp );

      clp.SetImage( srf );
    finally
      FreeObject( srf ) ;
      FreeObject( bmp ) ;
    end ;
  end ;
{$ENDIF}

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
  end ;
end ;

procedure Register ;
begin
  RegisterComponents( 'TatukGIS', [ TGIS_ControlNorthArrow ] ) ;
end ;

{==================================== END =====================================}
end.


