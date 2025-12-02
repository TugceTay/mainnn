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
  Visual control for displaying scale.
}

unit FMX.GisControlScale ;
{$HPPEMIT '#pragma link "FMX.GisControlScale"'}

{$INCLUDE GisInclude.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.UITypes,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Types,
  FMX.Objects,

  GisRtl,
  GisInterfaces,
  GisClasses,
  GisTypes,
  GisTypesUI,
  GisFunctions,
  GisCsBase,
  GisResource,
  GisViewer,
  GisRendererAbstract,
  FMX.GisFramework,
  FMX.GisViewerWnd,
  FMX.GisViewerBmp,
  FMX.GisRenderer;

type
  {$IFDEF LEVEL_RX10_FMX}
    /// <summary>
    ///   Styled settings.
    /// </summary>
    TGIS_ControlScaleStyledSetting = (

      /// <summary>
      ///   Set color for dividers.
      /// </summary>
      DividerColors,

      /// <summary>
      ///   Set font color.
      /// </summary>
      FontColor
    ) ;

    /// <summary>
    ///   Set of styled settings.
    /// </summary>
    TGIS_ControlScaleStyledSettings = set of TGIS_ControlScaleStyledSetting ;
  {$ENDIF}


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
  [ComponentPlatformsAttribute( pidAllPlatforms )]
  TGIS_ControlScale = class( TStyledControl, IGIS_Subscribe, IGIS_PrintableControl )
    private // properties internal value
      FGIS_Viewer     : TGIS_ViewerWnd ;
      FTextColor      : TAlphaColor ;
      FDividers       : Integer ;
      FFont           : TFont ;
      FGlow           : Boolean ;
      FDividerColor1  : TAlphaColor ;
      FDividerColor2  : TAlphaColor ;
      FShadowColor    : TAlphaColor ;
      FUnitsEPSG      : Integer ;
      FUnits          : TGIS_CSUnits ;

      FOnPrepare      : TGIS_ControlScalePrepareEvent ;

      {$IFDEF LEVEL_RX10_FMX}
        FStyledSettings : TGIS_ControlScaleStyledSettings ;
      {$ENDIF}

      FStyledColor    : TAlphaColor ;
      FStyledFontColor: TAlphaColor ;

    private // other private values
      oBitmap         : TBitmap  ;
      inPaint         : Boolean  ;

    protected // properties access routines

      procedure fset_GIS_Viewer   ( const _value : TGIS_ViewerWnd
                                  ) ;
      procedure fset_Font         ( const _value : TFont
                                  ) ;
      procedure fset_Glow         ( const _value : Boolean
                                  ) ;
      procedure fset_TextColor    ( const _value : TAlphaColor
                                  ) ;
      procedure fset_Dividers     ( const _value : Integer
                                  ) ;
      procedure fset_DividerColor1( const _value : TAlphaColor
                                  ) ;
      procedure fset_DividerColor2( const _value : TAlphaColor
                                  ) ;
      procedure fset_ShadowColor  ( const _value : TAlphaColor
                                  ) ;
      function  fget_Units        : TGIS_CSUnits ;

      procedure fset_Units        ( const _value : TGIS_CSUnits
                                  ) ;
      function  fget_UnitsEPSG    : Integer ;
      procedure fset_UnitsEPSG    ( const _value : Integer
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
      procedure doSubscribedAfterPaint
                                  ( const _context : TObject ) ;

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
      procedure DoPaint         ; override;

      {$IFDEF LEVEL_RX10_FMX}
        {#gendoc:hide}
        procedure DoStyleChanged; override;
      {$ENDIF}

    public

      /// <inheritdoc from="IGIS_PrintableControl"/>
      function CreateCopy      : IGIS_PrintableControl ;

      /// <inheritdoc from="IGIS_PrintableControl"/>
      procedure FreeCopy       ( const _control : IGIS_PrintableControl
                               ) ;

      /// <inheritdoc form="IGIS_PrintableControl"/>
      procedure PrintBmp       ( const _bitmap : TGIS_Bitmap
                               ) ;

      /// <summary>
      ///   Draw control on a provided bitmap
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
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNIMPLEMENTED if not supported.
      /// </exception>
      /// <remarks>
      ///   Available only on RAD Studio 10.1 Seattle or newer.
      ///   <note type="note">
      ///    See PrintDC for more advanced print options.
      ///    </note>
      /// </remarks>
      procedure PrintClipboard ( const _scale   : Double
                               ) ; virtual;

      /// <inheritdoc form="IGIS_Subscribe"/>
      procedure SubscribedEvent(       _sender  : TObject ;
                                       _event   : Integer ;
                                       _context : TObject
                               ) ;

    public
      /// <summary>
      ///   Units used for scale output.
      /// </summary>
      property Units : TGIS_CSUnits read  fget_Units
                                    write fset_Units ;

    published
      /// <summary>
      ///   Attached viewer.
      /// </summary>
      property GIS_Viewer : TGIS_ViewerWnd read  FGIS_Viewer
                                           write fset_GIS_Viewer ;

      /// <summary>
      ///   Font of the text.
      /// </summary>
      property Font          : TFont       read    FFont
                                           write   fset_Font ;

      /// <summary>
      ///   Draw "glowing" shadow around scale.
      /// </summary>
      property Glow          : Boolean     read    FGlow
                                           write   fset_Glow
                                           default True ;
      /// <summary>
      ///   Color of the text.
      /// </summary>
      property TextColor     : TAlphaColor read    FTextColor
                                           write   fset_TextColor ;

      /// <summary>
      ///   Number of dividers.
      /// </summary>
      property Dividers : Integer read    FDividers
                                  write   fset_Dividers
                                  default 5 ;

      /// <summary>
      ///   Color of the first divider.
      /// </summary>
      property DividerColor1 : TAlphaColor read    FDividerColor1
                                           write   fset_DividerColor1 ;

      /// <summary>
      ///   Color of the second divider.
      /// </summary>
      property DividerColor2 : TAlphaColor read    FDividerColor2
                                           write   fset_DividerColor2 ;

      /// <summary>
      ///   Color of the text shadow.
      ///   Drawn if different from TextColor.
      /// </summary>
      property ShadowColor : TAlphaColor   read    FShadowColor
                                           write   fset_ShadowColor ;

      /// <summary>
      ///   Units EPSG code for scale output.
      ///   If set to 0 then automatic Matric or Imperial system will be used
      ///   based on location.
      /// </summary>
      property UnitsEPSG : Integer   read  fget_UnitsEPSG
                                     write fset_UnitsEPSG
                                     default 0 ;

      /// <event/>
      /// <summary>
      ///   Event fired before calculating scale.
      /// </summary>
      property PrepareEvent          : TGIS_ControlScalePrepareEvent
                                       read  FOnPrepare
                                       write FOnPrepare ;

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
        property StyledSettings      : TGIS_ControlScaleStyledSettings
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

{$R GisControlScale_16x16.RES}

uses
  FMX.Surfaces,
  {$IFDEF LEVEL_RX101_FMX}
    FMX.Clipboard,
  {$ENDIF}
  FMX.Platform,

  System.Math,
  System.Types;


constructor TGIS_ControlScale.Create( _owner : TComponent ) ;
begin
  inherited Create( _owner ) ;

  doCreate ;

  Height := 40  ;
  Width  := 185 ;
end ;

procedure TGIS_ControlScale.doCreate ;
begin
  if csDesigning in ComponentState then
    EnsureFramework ;

  HitTest        := False   ;
  Visible        := True    ;

  FFont          := TFont.Create ;
  FFont.Family   := 'Tahoma' ;
  FFont.Size     := 8 ;
  FFont.Style    := [] ;
  FGlow          := True ;
  FTextColor     := TAlphaColorRec.Black ;
  FDividers      := 5       ;
  FDividerColor1 := TAlphaColorRec.Black ;
  FDividerColor2 := TAlphaColorRec.White ;
  FShadowColor   := TextColor ;

  if GisIsMetricSystem then
    FUnits  := CSUnitsList.ByEPSG( 904201 )
  else
    FUnits  := CSUnitsList.ByEPSG( 904202 ) ;

  FUnitsEPSG    := 0     ;

  oBitmap       := nil   ;
  inPaint       := False ;
  uponDestroy   := False ;

  FOnPrepare := nil ;

  {$IFDEF LEVEL_RX10_FMX}
    FStyledSettings := [] ;
  {$ENDIF}
end ;


destructor TGIS_ControlScale.Destroy ;
begin
  if not uponDestroy then
    doDestroy ;

  {$IFDEF GIS_PDK}
    RemoveFreeNotifications ;
  {$ENDIF}

  inherited Destroy ;
end ;


procedure TGIS_ControlScale.doDestroy ;
begin
  uponDestroy := True ;

  if Assigned( FGIS_Viewer ) then
    FGIS_Viewer.UnSubscribe( Self ) ;

  FreeObject( FFont ) ;
  FreeObject( oBitmap ) ;
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

procedure TGIS_ControlScale.fset_Font(
  const _value : TFont
) ;
begin
  FFont.Assign( _value ) ;

  doFullRepaint ;
end ;

procedure TGIS_ControlScale.fset_Glow(
  const _value : Boolean
) ;
begin
  FGlow := _value ;

  doFullRepaint ;
end ;

procedure TGIS_ControlScale.fset_TextColor(
  const _value : TAlphaColor
) ;
begin
  if FTextColor = _value then exit ;

  FTextColor := _value ;

  doFullRepaint ;
end ;

procedure TGIS_ControlScale.fset_DividerColor1(
  const _value : TAlphaColor
) ;
begin
  if FDividerColor1 = _value then exit ;

  FDividerColor1 := _value ;

  doFullRepaint ;
end ;

procedure TGIS_ControlScale.fset_DividerColor2(
  const _value : TAlphaColor
) ;
begin
  if FDividerColor2 = _value then exit ;

  FDividerColor2 := _value ;

  doFullRepaint ;
end ;

procedure TGIS_ControlScale.fset_ShadowColor(
  const _value : TAlphaColor
) ;
begin
  if FShadowColor = _value then exit ;

  FShadowColor := _value ;

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
  Result := 'nothing' ;
end ;

procedure TGIS_ControlScale.fset_InternalName(
  const _value : String
) ;
begin

end ;

procedure TGIS_ControlScale.doFullRepaint ;
begin
  FreeObject( oBitmap ) ;

  Repaint ;
end;

procedure TGIS_ControlScale.doSubscribedAfterPaint(
  const _context : TObject
) ;
begin
  if ( not Assigned( FGIS_Viewer ) ) or FGIS_Viewer.IsEmpty then exit ;

  doFullRepaint ;
end ;

procedure TGIS_ControlScale.DoPaint ;
var
  r         : TRect ;
begin
  inherited ;

  inPaint := True ;
  try
    if ( csDesigning in ComponentState ) or
       ( Assigned( GIS_Viewer ) and ( not GIS_Viewer.IsEmpty ) )
    then begin
      r := Rect( RoundS( 0 ),
                 RoundS( 0 ),
                 RoundS( Width  ),
                 RoundS( Height )
               ) ;

      if ( r.Width > 5 ) and ( r.Height > 5 ) then begin

        if ( csDesigning in ComponentState ) then
          FreeObject( oBitmap ) ;

        if not Assigned( oBitmap ) then begin
          if not ( csDesigning in ComponentState ) then
            oBitmap := DrawBmp( nil, GIS_Viewer.ScaleAsFloat, 0 )
          else
            oBitmap := DrawBmp( nil, 1/500000, 0 ) ;
        end ;

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
procedure TGIS_ControlScale.DoStyleChanged;
var
  stl      : TFmxObject  ;
  stl2     : TFmxObject  ;
  stlwnd   : TControl    ;
  stlctl   : TControl    ;
  bmp      : T_FMXBitmap ;
  dat      : TBitmapData ;
  c        : TGIS_Color  ;
  cnt      : Integer     ;
  r,g,b    : Integer     ;
  x,y      : Integer     ;
begin
  FreeObject( oBitmap ) ;

  inherited ;

  FStyledColor := TAlphaColorRec.White ;
  FStyledFontColor := TAlphaColorRec.Black ;

  stl := TStyledControl.LookupStyleObject(
           Self, Self, Scene, '', 'backgroundstyle', '', False
         );

  if not ( Assigned( stl ) and ( stl is TControl ) ) then
    exit ;

  stlwnd := TControl( stl ) ;

  stl := TStyledControl.LookupStyleObject(
            Self, Self, Scene, '', 'treeviewstyle.background', '', False
          );

  if not ( Assigned( stl ) and ( stl is TControl ) ) then
    exit ;

  stlctl := TControl( stl ) ;

  stl2 := stlctl.FindStyleResource( 'content') ;
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
            Self, Self, Scene, '', 'treeviewitemstyle', '', False
          );
  if not ( Assigned( stl ) and ( stl is TControl ) ) then
    exit ;

  stl2 := stl.FindStyleResource('text') ;

  if Assigned( stl2 ) and ( stl2 is TText ) then
    FStyledFontColor := TText( stl2 ).TextSettings.FontColor ;
end;
{$ENDIF}

function TGIS_ControlScale.CreateCopy
  : IGIS_PrintableControl ;
begin

end ;

procedure TGIS_ControlScale.FreeCopy(
  const _control : IGIS_PrintableControl
) ;
begin

end ;

procedure TGIS_ControlScale.PrintBmp(
  const _bitmap : TGIS_Bitmap
) ;
begin
  assert( assigned( _bitmap ) ) ;
  DrawBmp( TBitmap(_bitmap.NativeBitmap), GIS_Viewer.ScaleAsFloat, 0 ) ;
end ;

function TGIS_ControlScale.DrawBmp(
  const _bmp   : TBitmap ;
  const _scale : Double  ;
  const _ppi   : Integer
) : TBitmap ;
var
  i         : Integer     ;
  r         : TRect       ;
  bmp       : TGIS_Bitmap ;
  ppi       : Integer     ;
  fontscale : Integer     ;
  iscs      : Boolean ;

  pvwr      : IGIS_ViewerParent ;

  rnd       : TGIS_RendererAbstract ;
  ctx       : TGIS_RendererContext  ;

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

  cl_textcolor     : TGIS_Color ;
  cl_divdiercolor1 : TGIS_Color ;
  cl_divdiercolor2 : TGIS_Color ;
  cl_shadowcolor   : TGIS_Color ;
  cl_glow          : TGIS_Color ;
  shadow_width : Integer ;
  rct_tmp      : TRect ;
  si, sj       : Integer ;

  function prepare_distance(
    const _width : Integer
  ) : Double ;
  begin
    Result := (1/scale) / ( 100/2.54 * ppi ) *
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
    ii  : Integer ;
    m1  : Boolean ;
    m2  : Boolean ;
    txt : TStringBuilder ;
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

  if not ( csdesigning in ComponentState ) then begin
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
    ppi       := 96 ;
    fontscale := 100 ;
  end ;

  rnd := nil ;
  bmp := TGIS_Bitmap.Create( r.Width, r.Height, True );
  try
    iscs := Assigned( GIS_Viewer ) and ( GIS_Viewer.CS.EPSG <> 0 ) ;

    rnd := TGIS_RendererFMX.Create ;

    ctx := TGIS_RendererContext.Create ;
    try
      ctx.AssignBaseMap( bmp.NativeBitmap, False ) ;

      rnd.CreateContext( nil, nil, ctx, Point( 0, 0 ),
                         RoundS( r.Width), RoundS( r.Height ),
                         ppi, fontscale
                       ) ;

      cl_textcolor     := TGIS_Color.FromARGB( TextColor ) ;
      cl_divdiercolor1 := TGIS_Color.FromARGB( DividerColor1 ) ;
      cl_divdiercolor2 := TGIS_Color.FromARGB( DividerColor2 ) ;
      cl_shadowcolor   := TGIS_Color.FromARGB( ShadowColor ) ;

      {$IFDEF LEVEL_RX10_FMX}
        if ( TGIS_ControlScaleStyledSetting.DividerColors in StyledSettings )
           and inPaint
        then begin
          cl_divdiercolor1 := GISColor( FStyledFontColor ) ;
          cl_divdiercolor2 := GISColor( FStyledColor ) ;
        end ;
        if  ( TGIS_ControlScaleStyledSetting.FontColor in StyledSettings )
            and inPaint
        then begin
          cl_textcolor     := GISColor( FStyledFontColor ) ;
          cl_shadowcolor   := GISColor( FStyledFontColor ) ;
        end ;
      {$ENDIF}

      rnd.CanvasFont.Name := FFont.Family ;
      rnd.CanvasFont.Size := RoundS( FFont.Size ) ;

      rnd.CanvasFont.Style := [];
      if TFontStyle.fsBold      in FFont.Style then
        rnd.CanvasFont.Style := rnd.CanvasFont.Style + [TGIS_FontStyle.Bold     ] ;
      if TFontStyle.fsItalic    in FFont.Style then
        rnd.CanvasFont.Style := rnd.CanvasFont.Style + [TGIS_FontStyle.Italic   ] ;
      if TFontStyle.fsUnderline in FFont.Style then
        rnd.CanvasFont.Style := rnd.CanvasFont.Style + [TGIS_FontStyle.Underline] ;
      if TFontStyle.fsStrikeOut in FFont.Style then
        rnd.CanvasFont.Style := rnd.CanvasFont.Style + [TGIS_FontStyle.StrikeOut] ;

      txt_out := '' ;

      rnd.CanvasBrush.Style := TGIS_BrushStyle.Clear ;
      rnd.CanvasPen.Style   := TGIS_PenStyle.Clear ;

      scale := _scale ;

      if scale <= 0  then begin
        if Assigned( GIS_Viewer ) then
          scale := GIS_Viewer.ScaleAsFloat ;
      end ;

      if scale <= 0 then exit ;

      w := ( r.Right - r.Left ) ;
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

      rnd.CanvasBrush.Style := TGIS_BrushStyle.Solid ;
      rnd.CanvasPen.Style   := TGIS_PenStyle.Solid ;
      rnd.CanvasPen.LineJoin := TGIS_LineJoin.Bevel ;
      rnd.CanvasPen.Width   := 1       ;
      rnd.CanvasPen.Color   := cl_divdiercolor1 ;

      for i:=1 to FDividers do begin
        if i mod 2 = 1 then
          rnd.CanvasBrush.Color.ARGB := cl_divdiercolor1.ARGB
        else
          rnd.CanvasBrush.Color.ARGB := cl_divdiercolor2.ARGB ;

        rnd.CanvasDrawRectangle(
            Rect( x, y, x +dx, y + dy ) ) ;

        x := sx + i * dx ;
      end ;

      rnd.CanvasPen.Color := rnd.CanvasBrush.Color ;
      rnd.CanvasBrush.Style := TGIS_BrushStyle.Clear ;

      rct_txt := Rect(
                   r.Left +
                   ( r.Right - r.Left - rnd.CanvasTextExtent( txt_out ).X ) div 2 ,
                   y + dy + System.Math.Max( 2, rnd.CanvasTextExtent( 'Ay' ).Y div 8 ),
                   r.Right,
                   r.Bottom
                 ) ;

      if cl_textcolor <> cl_shadowcolor then begin
        shadow_width := Max( 1, ppi div 96 ) ;
        if shadow_width > 0 then begin
          rnd.CanvasFont.Color :=  cl_shadowcolor ;
          for si := -shadow_width to shadow_width do begin
            for sj := -shadow_width to shadow_width do begin
              rct_tmp := Rect( rct_txt.Left  + si, rct_txt.Top    + sj,
                               rct_txt.Right + si, rct_txt.Bottom + sj ) ;
              rnd.CanvasDrawText( rct_tmp, txt_out ) ;
            end ;
          end ;
        end ;
      end ;

      rnd.CanvasFont.Color := cl_textcolor ;
      rnd.CanvasDrawText( rct_txt, txt_out ) ;

      rnd.ReleaseContext ;
    finally
      FreeObject( rnd ) ;
      FreeObject( ctx ) ;
    end ;

    if not Assigned( Result ) then
      Result := TBitmap.Create ;

    if Glow then begin
      if Assigned( GIS_Viewer ) and inPaint then
        cl_glow := TGIS_Color.FromARGB( GIS_Viewer.ActiveBackgroundColor )
      else
        cl_glow := TGIS_Color.White ;

      cl_glow := TGIS_Color.FromARGB( 128, cl_glow.R, cl_glow.G, cl_glow.B ) ;

      bmp.MakeGlowing( cl_glow, 5 * ppi div 96 ) ;
    end ;

    Result.Assign( TBitmap( bmp.NativeBitmap ) ) ;
  finally
    FreeObject( bmp );
  end ;

end ;

procedure TGIS_ControlScale.PrintClipboard ;
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
      bmp := DrawBmp( nil, _scale, 0 ) ;

      srf := TBitmapSurface.Create ;
      srf.Assign( bmp );

      clp.SetImage( srf );
    finally
      FreeObject( srf ) ;
      FreeObject( bmp ) ;
    end ;
  end ;
{$ENDIF}

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
      end ;
  end ;
end ;

procedure Register ;
begin
  RegisterComponents( 'TatukGIS', [ TGIS_ControlScale ] ) ;
end ;

{==================================== END =====================================}
end.


