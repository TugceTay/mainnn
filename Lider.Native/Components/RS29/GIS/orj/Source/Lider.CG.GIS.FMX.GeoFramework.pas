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
  Framework interface implementation. The purpose of this unit is to implement
  based conversion and definition that are platform depended like Bitmap, color
  conversion etc.
}

unit FMX.GisFramework ;
{$HPPEMIT '#pragma link "FMX.GisFramework"'}

{$INCLUDE GisInclude.inc}

interface

uses
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  FMX.Dialogs,

  FMX.Types,
  FMX.Graphics,

  GisRtl,
  GisTypesUI;

{$IFDEF MSWINDOWS}
  {$DEFINE MSWINDOWS_MULTITOUCH_PAN_FIX}
{$ENDIF}

const
  {#GENDOC:HIDE}
  _GLES     = 1  ;

  {#GENDOC:HIDE}
  _Metal    = 2  ;

  {#GENDOC:HIDE}
  _DirectX  = 4  ;

  {#GENDOC:HIDE}
  _Direct2D = 8  ;

  {#GENDOC:HIDE}
  _GdiPlus  = 16 ;

  {#GENDOC:HIDE}
  _Quartz   = 32 ;

  {#GENDOC:HIDE}
  _Skia     = 64 ;

var
  {#GENDOC:HIDE}
  ActiveRenderer : DWORD ;

type
  /// <summary>
  ///   Platform dependent Bitmap implementation.
  /// </summary>
  /// <remarks>
  ///   This platform supports optional premultipily way of storing
  ///   bitmaps.
  /// </remarks>
  TGIS_BitmapFMX = class( TGIS_BitmapAbstract )
    private
      FBitmap     : TBitmap ;
      FBitmapData : TBitmapData ;
      FPixelData  : TGIS_Pixels ;
      FWritable   : Boolean ;
      FFormat     : TGIS_BitmapFormat ;
      FLineOrder  : TGIS_BitmapLinesOrder ;
      FViewer     : TComponent ;
    protected
      function  fget_Width        : Integer ; override;
      function  fget_Height       : Integer ; override;
      function  fget_PPI          : Integer ; override;
      procedure fset_PPI          ( const _value : Integer
                                  ) ; override;
      function  fget_Data         : TObject ; override;
      procedure fset_Data         ( const _value : TObject
                                  ) ; override;
    protected
      procedure doDestroy         ; override;
    public
      /// <summary>
      ///   Construct bitmap with given dimensions.
      /// </summary>
      /// <param name="_width">
      ///   width in pixels
      /// </param>
      /// <param name="_height">
      ///   height in pixels
      /// </param>
      constructor Create          ( const _width  : Integer ;
                                    const _height : Integer
                                  ) ; overload;

      /// <inheritdoc/>
      class function FromBitmap   ( const _bmp    : TObject
                                  ) : TGIS_BitmapAbstract ; override;

      /// <inheritdoc/>
      class function FromFile     ( const _path   : String
                                  ) : TGIS_BitmapAbstract ; override;

      /// <inheritdoc/>
      class function FromStream   ( const _stream : TObject
                                  ) : TGIS_BitmapAbstract ; override;

    public
      /// <inheritdoc/>
      procedure   ToFile          ( const _path   : String
                                  ) ; override;

      /// <inheritdoc/>
      procedure   ToStream        ( const _stream : TObject
                                  ) ; override;

      /// <inheritdoc/>
      procedure   ToStream        ( const _stream : TObject ;
                                    const _format       : TGIS_PixelFormat ;
                                    const _subformat    : TGIS_PixelSubFormat ;
                                    const _compression  : Integer
                                  ) ; override;

      /// <inheritdoc/>
      procedure   MakeTransparent ; override;

      /// <inheritdoc/>
      procedure   Clear           ( const _color       : TGIS_Color
                                  ) ; override;

      /// <inheritdoc/>
      procedure   LockPixels      ( var   _pixels   : TGIS_Pixels ;
                                    const _writable : Boolean     ;
                                    const _format   : TGIS_BitmapFormat ;
                                    const _order    : TGIS_BitmapLinesOrder
                                  ) ; override;

      /// <inheritdoc/>
      procedure   UnlockPixels    ; override;

      /// <inheritdoc/>
      procedure   DrawShape       ( const _shape    :  TObject ;
                                    const _outline  :  Boolean ;
                                    var   _scale    :  Double  ;
                                    var   _offset   :  TPoint
                                  ) ; overload; override;
      /// <inheritdoc/>
      procedure   DrawShape       ( const _shape    :  TObject ;
                                    const _ppi      :  Integer ;
                                    const _outline  :  Boolean ;
                                    var   _scale    :  Double  ;
                                    var   _offset   :  TPoint
                                  ) ; overload; override;

      /// <inheritdoc/>
      procedure   DrawShape       ( const _shape    :  TObject    ;
                                    const _ppi      :  Integer    ;
                                    const _outline  :  Boolean    ;
                                    const _areacolor:  TGIS_Color ;
                                    const _linecolor:  TGIS_Color ;
                                    var   _scale    :  Double     ;
                                    var   _offset   :  TPoint
                                  ) ; overload; override;

      /// <inheritdoc/>
      procedure   DrawSymbol      ( const _name     :  String
                                  ) ; overload ; override;

      /// <inheritdoc/>
      procedure   DrawSymbol      ( const _name     :  String ;
                                    const _ppi      :  Integer
                                  ) ; overload ; override;


      /// <inheritdoc/>
      procedure   DrawSymbol      ( const _name     :  String     ;
                                    const _ppi      :  Integer    ;
                                    const _areacolor:  TGIS_Color ;
                                    const _linecolor:  TGIS_Color
                                  ) ; overload ; override;
      /// <inheritdoc/>
      procedure   DrawGlyph       ( const _symbol   :  TObject    ;
                                    const _ppi      :  Integer    ;
                                    const _color    :  TGIS_Color ;
                                    const _disabled :  Boolean
                                  ) ; overload ; override;

      /// <inheritdoc/>
      function CreateViewer       : IInterface ; override ;

  end;

  /// <summary>
  ///   Factory for platform dependent Bitmap implementation.
  /// </summary>
  TGIS_BitmapFactoryFMX = class( TGIS_BitmapFactory )
    public
      /// <inheritdoc/>
      function DoCreate           ( const _parent : TGIS_Bitmap ;
                                    const _width  : Integer ;
                                    const _height : Integer
                                  ) : TGIS_BitmapAbstract ;
                                  override;

      /// <inheritdoc/>
      function DoCreateFromBitmap ( const _parent : TGIS_Bitmap ;
                                    const _bmp    : TObject
                                  ) : TGIS_BitmapAbstract ;
                                  override;

      /// <inheritdoc/>
      function DoCreateFromFile   ( const _parent : TGIS_Bitmap ;
                                    const _path   : String
                                  ) : TGIS_BitmapAbstract ;
                                  override;

      /// <inheritdoc/>
      function DoCreateFromStream ( const _parent : TGIS_Bitmap ;
                                    const _stream : TObject
                                  ) : TGIS_BitmapAbstract ;
                                  override;

      /// <inheritdoc/>
      function DoCreateFromResource(
                                    const _parent : TGIS_Bitmap ;
                                    const _ref    : IntPtr ;
                                    const _name   : String
                                  ) : TGIS_BitmapAbstract ;
                                  override;

      /// <inheritdoc/>
      function NativeFormat       : TGIS_BitmapFormat ;
                                  override;

      /// <inheritdoc/>
      function NativeLineOrder    : TGIS_BitmapLinesOrder ;
                                  override;

      /// <inheritdoc/>
      function BitmapType         : TGIS_BitmapType ;
                                  override;
  end;


  /// <summary>
  ///   Factory for platform dependent Font implementation.
  /// </summary>
  TGIS_FontFactoryFMX = {$IFDEF OXYGENE} public {$ENDIF}
                        class ( TGIS_FontFactory )
    public
      /// <inheritdoc/>
      function DoCreate          ( const _name  : String ;
                                   const _size  : Integer ;
                                   const _style : TGIS_FontStyles ;
                                   const _color : TGIS_Color
                                 ) : TGIS_Font ; override ;
      /// <inheritdoc/>
      function DoCreateFromFont( const _font    : TObject
                                 ) : TGIS_Font ; override ;
  end ;


  /// <summary>
  ///   Platform dependent Timer implementation.
  /// </summary>
  TGIS_TimerFMX = class( TGIS_TimerAbstract )
    private
      FTimer : TTimer ;
    protected
      function  fget_NativeTimer : TObject ;
                                 override;
      function  fget_Enabled     : Boolean ;
                                 override;
      procedure fset_Enabled     ( const _value : Boolean      ) ;
                                 override;
      function  fget_Interval    : Cardinal ;
                                 override;
      procedure fset_Interval    ( const _value : Cardinal     ) ;
                                 override;
      function  fget_OnTimer     : TNotifyEvent ;
                                 override;
      procedure fset_OnTimer     ( const _value : TNotifyEvent ) ;
                                 override;
    protected
      procedure doDestroy        ; override;

    public
      /// <summary>
      ///   Standard constructor
      /// </summary>
      constructor Create        ;

    public
      /// <summary>
      ///   Same as TTimer.
      /// </summary>
      property  NativeTimer     : TObject      read  fget_NativeTimer ;

      /// <summary>
      ///   Same as TTimer.
      /// </summary>
      property  Enabled         : Boolean      read  fget_Enabled
                                               write fset_Enabled  ;

      /// <summary>
      ///   Same as TTimer.
      /// </summary>
      property  Interval        : Cardinal     read  fget_Interval
                                               write fset_Interval ;

      /// <event/>
      /// <summary>
      ///   Same as TTimer.
      /// </summary>
      property  OnTimer         : TNotifyEvent read  fget_OnTimer
                                               write fset_OnTimer  ;
  end;

  /// <summary>
  ///   Factory for platform dependent Timer implementation.
  /// </summary>
  TGIS_TimerFactoryFMX =  class( TGIS_TimerFactory )
    public
      /// <inheritdoc/>
      function DoCreate          : TGIS_TimerAbstract ;
                                 override;
  end;

  /// <summary>
  ///   A class that groups platform's specific methods.
  /// </summary>
  TGIS_FrameworkUtils = class
    public
      /// <summary>
      ///   Convert platform independent color to native FMX color.
      /// </summary>
      /// <param name="_color">
      ///   color to be converted
      /// </param>
      /// <returns>
      ///   native FMX color
      /// </returns>
      class function ToPlatformColor   ( const _color : TGIS_Color
                                       ) : TAlphaColor ;
                                       static;
                                       {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}


      /// <summary>
      ///   Convert platform native FMX color to platform independent color.
      /// </summary>
      /// <param name="_color">
      ///   color to be converted
      /// </param>
      /// <returns>
      ///   platform independent color
      /// </returns>
      class function FromPlatformColor ( const _color : TAlphaColor
                                       ) : TGIS_Color ;
                                       static;
                                       {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
  end ;

  /// <summary>
  ///   Force framework. To be used only upon design time.
  /// </summary>
  procedure EnsureFramework ;

  /// <summary>
  ///   Convert platform inependent color to native FMX color.
  /// </summary>
  /// <param name="_color">
  ///   color to be converted
  /// </param>
  /// <returns>
  ///   native FMX color
  /// </returns>
  function FMXColor( const _color : TGIS_Color ) : TAlphaColor ; inline ;

  /// <summary>
  ///   Convert platform native FMX color to platform independent color.
  /// </summary>
  /// <param name="_color">
  ///   color to be converted
  /// </param>
  /// <returns>
  ///   platform independent color
  /// </returns>
  function GISColor( const _color : TAlphaColor ) : TGIS_Color ; inline ;

  /// <summary>
  ///   Convert platform inependent font style to native FMX font style.
  /// </summary>
  /// <param name="_style">
  ///   font style to be converted
  /// </param>
  /// <returns>
  ///   native FMX font style
  /// </returns>
  function FMXFontStyle( const _style : TGIS_FontStyles ) : TFontStyles ; inline ;

  /// <summary>
  ///   Convert platform native FMX font style to platform independent font style.
  /// </summary>
  /// <param name="_style">
  ///   font style to be converted
  /// </param>
  /// <returns>
  ///   platform independent font style
  /// </returns>
  function GISFontStyle( const _style : TFontStyles ) : TGIS_FontStyles ; inline ;

  /// <summary>
  ///   Create a native FMX font object from platform independent font.
  /// </summary>
  /// <param name="_font">
  ///   platform independent font
  /// </param>
  /// <returns>
  ///   native FMX font
  /// </returns>
  function FMXFont( const _font : TGIS_Font ) : TFont ;

  /// <summary>
  ///   Get list of fonts available on FMX.
  /// </summary>
  /// <param name="_list">
  ///   list of fonts; must be allocated before call
  /// </param>
  /// <remarks>
  ///   Return only list of build in fonts. Any custom fonts must be added
  ///   separately.
  /// </remarks>
  procedure GetFontList( const _list : TStringList ) ;


//##############################################################################
implementation

uses
  {$IFDEF MSWINDOWS}
    Winapi.Windows,
  {$ENDIF}
  {$IFDEF MACOSX_OS}
    Macapi.Foundation,
    Macapi.AppKit,
  {$ENDIF}
  {$IFDEF IOS_OS}
    iOSapi.Foundation,
    iOSapi.UIKit,
  {$ENDIF}
  {$IFDEF LINUX_OS}
    FMUX.Api,
  {$ENDIF}

  System.UIConsts,
  FMX.Surfaces,
  FMX.Consts,
  {$IFDEF MSWINDOWS_MULTITOUCH_PAN_FIX}
    FMX.MultiTouch.Win,
  {$ENDIF}

  GisTypes,
  GisClasses,
  GisFunctions,
  GisResource,
  GisLayerVector,
  GisSymbol,
  GisInterfaces,
  GisPrintPreviewHelperAbstract,
  GisLegendUtils,
  FMX.GisControlPrintPreview,
  FMX.GisLegendUtilsFactory,
  FMX.GisViewerBmp;

{$IFDEF MSWINDOWS_MULTITOUCH_PAN_FIX}
  type
    // enhace Windows FMX with two fingers panning
    T_MultiTouchManagerWinFix = class (TMultiTouchManagerWin)
    private
       class var oHijackCreate    : TGIS_Hijack ;
       class var oHijackDestroy   : TGIS_Hijack ;
       var       oHijackTouchMove : TGIS_Hijack ;
    protected
       procedure TouchMoveFix;
    public
      constructor Create(const AParent: TComponent); override;
      destructor Destroy ; override;

      class procedure Hijack ;
      class procedure ReleaseHijack ;

      procedure HijackTouchMove ;
      procedure ReleaseHijackTouchMove ;
    end;


  procedure T_MultiTouchManagerWinFix.TouchMoveFix;
  begin
    if Length(Touches) = 2 then
    begin
      {$IFDEF LEVEL_RX12_FMX}
        if (Touches[0].Location.Distance(FFirstPointerDownCoordinates) > LongTaptreshold) and
          (TInteractiveGesture.Pan in (EnabledInteractiveGestures - ActiveInteractiveGestures)) and
          SendCMGestureMessage(CreateGestureEventInfo(TInteractiveGesture.Pan))
        then
          BeginInteractiveGesture(TInteractiveGesture.Pan, GestureControl);

        // Since the pointer moved a bit, we could have a pan.
        if (Touches[0].Location.Distance(FOldPoint1) > 0) and (TInteractiveGesture.Pan in ActiveInteractiveGestures) then
          SendCMGestureMessage(CreateGestureEventInfo(TInteractiveGesture.Pan));
      {$ELSE}
        if (Touches[0].Location.Distance(FFirstPointerDownCoordinates) > GetLongTapAllowedMovement) and
          (TInteractiveGesture.Pan in (FEnabledInteractiveGestures - FActiveInteractiveGestures)) and
          SendCMGestureMessage(CreateGestureEventInfo(TInteractiveGesture.Pan))
        then
          Include(FActiveInteractiveGestures, TInteractiveGesture.Pan);

        // Since the pointer moved a bit, we could have a pan.
        if (Touches[0].Location.Distance(FOldPoint1) > 0) and (TInteractiveGesture.Pan in FActiveInteractiveGestures) then
          SendCMGestureMessage(CreateGestureEventInfo(TInteractiveGesture.Pan));
      {$ENDIF}
    end;

    ReleaseHijackTouchMove ;

    TouchMove ;

    HijackTouchMove ;
  end;

  constructor T_MultiTouchManagerWinFix.Create(const AParent: TComponent);
  begin
    ReleaseHijack ;

    inherited ;

    Hijack ;
    HijackTouchMove ;
  end;

  destructor T_MultiTouchManagerWinFix.Destroy ;
  begin
    ReleaseHijackTouchMove ;
    ReleaseHijack ;

    inherited ;

    Hijack ;
  end;

  class procedure T_MultiTouchManagerWinFix.Hijack ;
  begin
    oHijackCreate := TGIS_Hijack.Create(
      @TMultiTouchManagerWin.Create,
      @T_MultiTouchManagerWinFix.Create
    ) ;
    oHijackDestroy := TGIS_Hijack.Create(
      @TMultiTouchManagerWin.Destroy,
      @T_MultiTouchManagerWinFix.Destroy
    ) ;
  end;

  class procedure T_MultiTouchManagerWinFix.ReleaseHijack  ;
  begin
    FreeObject( oHijackCreate  ) ;
    FreeObject( oHijackDestroy ) ;
  end;

  procedure T_MultiTouchManagerWinFix.HijackTouchMove ;
  begin
    oHijackTouchMove := TGIS_Hijack.Create(
      @TMultiTouchManagerWin.TouchMove,
      @T_MultiTouchManagerWinFix.TouchMoveFix
    ) ;
  end;

  procedure T_MultiTouchManagerWinFix.ReleaseHijackTouchMove ;
  begin
    FreeObject( oHijackTouchMove ) ;
  end;
{$ENDIF}

{$REGION 'TGIS_Bitmap'}

function TGIS_BitmapFMX.fget_Width
  : Integer ;
begin
  Result := FBitmap.Width ;
end;

function TGIS_BitmapFMX.fget_Height
  : Integer ;
begin
  Result := FBitmap.Height ;
end;

function TGIS_BitmapFMX.fget_PPI
  : Integer ;
begin
  Result := 96 ;
end ;

procedure TGIS_BitmapFMX.fset_PPI(
  const _value : Integer
) ;
begin

end ;

function TGIS_BitmapFMX.fget_Data
  : TObject ;
begin
  Result := FBitmap ;
end ;

procedure TGIS_BitmapFMX.fset_Data(
  const _value : TObject
) ;
begin
  FreeObject( FBitmap ) ;
  FBitmap := FMX.Graphics.TBitmap.Create() ;
  FBitmap.Assign( FMX.Graphics.TBitmap( _value ) ) ;
end ;

procedure TGIS_BitmapFMX.doDestroy ;
begin
  FreeObject( FViewer ) ;
  FreeObject( FBitmap ) ;
  inherited ;
end ;

constructor TGIS_BitmapFMX.Create(
  const _width  : Integer ;
  const _height : Integer
) ;
begin
  FBitmap := FMX.Graphics.TBitmap.Create( _width, _height ) ;
  FBitmap.Clear( TColorRec.Null );
  FViewer := nil ;
end;

class function TGIS_BitmapFMX.FromBitmap(
  const _bmp : TObject
) : TGIS_BitmapAbstract ;
var
  res : TGIS_BitmapFMX ;
begin
  res := TGIS_BitmapFMX.Create ;
  res.FBitmap := FMX.Graphics.TBitmap.Create() ;
  res.FBitmap.Assign( FMX.Graphics.TBitmap( _bmp ) ) ;
  Result := res ;
end;

class function TGIS_BitmapFMX.FromFile(
  const _path   : String
) : TGIS_BitmapAbstract ;
var
  strm : TFileStream ;
  res : TGIS_BitmapFMX ;
begin
  Result := nil ;
  res := nil ;

  try
    res := TGIS_BitmapFMX.Create ;

    // by stream because normal loader is not multi-user friendly
    strm := TFileStream.Create( _path,
                                fmOpenRead or
                                fmShareDenyWrite
                              ) ;
    try
      res.FBitmap := FMX.Graphics.TBitmap.CreateFromStream( strm ) ;
    finally
      strm.Free ;
    end;
    Result := res ;
  except
    FreeObject( res ) ;
    raise ;
  end;
end;

class function TGIS_BitmapFMX.FromStream(
  const _stream : TObject
) : TGIS_BitmapAbstract ;
var
  res : TGIS_BitmapFMX ;
begin
  Result := nil ;
  res := nil ;

  try
    res := TGIS_BitmapFMX.Create ;
    res.FBitmap := FMX.Graphics.TBitmap.CreateFromStream( TStream( _stream ) ) ;
    Result := res ;
  except
    FreeObject( res ) ;
    raise ;
  end;
end;

procedure TGIS_BitmapFMX.ToFile(
  const _path   : String
) ;
begin
  Assert( Assigned( FBitmap ) ) ;
  FBitmap.SaveToFile( _path ) ;
end;

procedure TGIS_BitmapFMX.ToStream(
  const _stream : TObject
) ;
begin
  Assert( Assigned( FBitmap ) ) ;
  FBitmap.SaveToStream( TStream( _stream ) ) ;
end;

procedure TGIS_BitmapFMX.ToStream(
  const _stream       : TObject ;
  const _format       : TGIS_PixelFormat ;
  const _subformat    : TGIS_PixelSubFormat ;
  const _compression  : Integer
) ;
var
  Surf : TBitmapSurface;
  ext  : String ;
  cp   : TBitmapCodecSaveParams ;
begin
  Assert( Assigned( FBitmap ) ) ;

  Surf := TBitmapSurface.Create ;
  try
    Surf.Assign( FBitmap ) ;

    case _subformat of
      TGIS_PixelSubFormat.BMP   : ext := '.bmp' ;
      TGIS_PixelSubFormat.JPEG  : ext := '.jpg' ;
      TGIS_PixelSubFormat.PNG   : ext := '.png' ;
      else
        Assert( False, 'Unsupported' ) ;
    end ;

    cp.Quality := _compression ;
    TBitmapCodecManager.SaveToStream(TStream( _stream ), Surf, ext, @cp ) ;
  finally
    FreeObject( Surf ) ;
  end ;
end;

procedure TGIS_BitmapFMX.MakeTransparent ;
var
  k      : Integer ;
  x,y    : Integer ;
  w,h    : Integer ;
  w1     : Integer ;
  co, cl : Cardinal ;
begin
  Assert( Assigned( FBitmap ) ) ;

  FBitmap.Map( TMapAccess.ReadWrite, FBitmapData ) ;

  co := FBitmapData.GetPixel( 0, 0 ) ;

  h := FBitmap.Height ;
  w := FBitmap.Width  ;

  k := 0 ;
  y := 0 ;

  for y := 0 to h - 1 do begin
    w1 := y * FBitmapData.Pitch ;
    for x := 0 to w - 1 do begin
      cl := PCardinal(
              IntPtr( FBitmapData.Data ) + w1 + x*4
            )^ ;
      if cl = co then
        PCardinal(
              IntPtr( FBitmapData.Data ) + w1 + x*4
            )^ := TAlphaColors.Null ;

      Inc( k ) ;
    end;
  end;

  FBitmap.Unmap( FBitmapData ) ;
end ;

procedure TGIS_BitmapFMX.Clear(
  const _color : TGIS_Color
) ;
begin
  FBitmap.Canvas.Fill.Kind := TBrushKind.Solid ;
  FBitmap.Canvas.Fill.Color := FMXColor( _color ) ;

  FBitmap.Canvas.FillRect( RectF( 0, 0, FBitmap.Width, FBitmap.Height ),
                           0, 0,
                           [ TCorner.TopLeft,
                             TCorner.TopRight,
                             TCorner.BottomLeft,
                             TCorner.BottomRight
                           ]
                           , 1
                         ) ;
end ;

procedure TGIS_BitmapFMX.LockPixels(
  var   _pixels   : TGIS_Pixels ;
  const _writable : Boolean     ;
  const _format   : TGIS_BitmapFormat ;
  const _order    : TGIS_BitmapLinesOrder
) ;
var
  i,k       : Integer ;
  x,y       : Integer ;
  w,h       : Integer ;
  step      : Integer ;
  revcolors : Boolean ;
  revlines  : Boolean ;
  w1        : Integer ;
  cl        : Int32 ;
  a,r,g,b   : Byte ;
begin
  if ( Length( FPixelData ) <> FBitmap.Width * FBitmap.Height )
     or
     ( _writable <> FWritable )
     or
     ( _format <> FFormat )
     or
     ( _order <> FLineOrder )
  then begin
    if      _format = TGIS_BitmapFormat.Native then
            revcolors := False
    else if _format = TGIS_Bitmap.NativeFormat then
            revcolors := False
    else    revcolors := True ;

    if      _order = TGIS_BitmapLinesOrder.Native then
            revlines := False
    else if _order = TGIS_Bitmap.NativeLineOrder then
            revlines := False
    else    revlines := True ;

    UnlockPixels ;

    if _writable then begin
      if not FBitmap.Map( TMapAccess.ReadWrite, FBitmapData ) then
        exit ;
    end
    else begin
      if not FBitmap.Map( TMapAccess.Read, FBitmapData ) then
        exit ;
    end;

    SetLength( FPixelData, FBitmap.Width * FBitmap.Height ) ;

    h := FBitmap.Height ;
    w := FBitmap.Width  ;

    if revlines then begin
      step := -1 ;
      y    :=  h - 1 ;
    end
    else begin
      step :=  1 ;
      y    :=  0 ;
    end;

    k := 0 ;
    if revcolors then begin
      for i := 0 to h - 1 do begin
        w1 := y * FBitmapData.Pitch ;
        for x := 0 to w - 1 do begin
          cl := PInteger(
                  IntPtr( FBitmapData.Data ) + w1 + x*4
                )^ ;
          a := ( cl shr 24 ) and $ff ;
          r := ( cl shr 16 ) and $ff ;
          g := ( cl shr  8 ) and $ff ;
          b := ( cl        ) and $ff ;

          if Premultiplied and ( a <> 255 ) and ( a <> 0 ) then begin
            r := Byte( r * 255 div a ) ;
            g := Byte( g * 255 div a ) ;
            b := Byte( b * 255 div a ) ;
          end;

          FPixelData[k]
            := a shl 24 + b shl 16 + g shl 8 + r ;
          Inc( k ) ;
        end;
        y := y + step ;
      end;
    end
    else begin
      for i := 0 to h - 1 do begin
        w1 := y * FBitmapData.Pitch ;

        if Premultiplied then begin
          for x := 0 to w - 1 do begin
            cl := PInteger(
                    IntPtr( FBitmapData.Data ) + w1 + x*4
                  )^ ;
            a := ( cl shr 24 ) and $ff ;
            r := ( cl shr 16 ) and $ff ;
            g := ( cl shr  8 ) and $ff ;
            b := ( cl        ) and $ff ;

            if ( a <> 255 ) and ( a <> 0 ) then begin
              r := Byte( r * 255 div a ) ;
              g := Byte( g * 255 div a ) ;
              b := Byte( b * 255 div a ) ;
            end;

            FPixelData[k]
              := a shl 24 + r shl 16 + g shl 8 + b ;
            Inc( k ) ;
          end;
        end
        else begin
          for x := 0 to w - 1 do begin
            FPixelData[k]
              := PInteger(
                   IntPtr( FBitmapData.Data ) + w1 + x*4
                 )^ ;
            Inc( k ) ;
          end;
        end;
        y := y + step ;
      end;
    end;

    FWritable  := _writable ;
    FFormat    := _format   ;
    FLineOrder := _order    ;

    if not _writable then begin
      FBitmap.UnMap( FBitmapData ) ;
    end;
  end ;

  _pixels := FPixelData  ;
end;

procedure TGIS_BitmapFMX.UnlockPixels ;
var
  i,k       : Integer ;
  x,y       : Integer ;
  w,h       : Integer ;
  step      : Integer ;
  revcolors : Boolean ;
  revlines  : Boolean ;
  w1        : Integer ;
  cl        : Int32 ;
  a,r,g,b   : Byte ;
begin
  if Length( FPixelData ) = 0 then
    exit ;

  if FWritable then begin
    if      FFormat = TGIS_BitmapFormat.Native then
            revcolors := False
    else if FFormat = TGIS_Bitmap.NativeFormat then
            revcolors := False
    else    revcolors := True ;

    if      FLineOrder = TGIS_BitmapLinesOrder.Native then
            revlines := False
    else if FLineOrder = TGIS_Bitmap.NativeLineOrder then
            revlines := False
    else    revlines := True ;

    h := FBitmap.Height ;
    w := FBitmap.Width  ;

    if revlines then begin
      step := -1 ;
      y    :=  h - 1 ;
    end
    else begin
      step :=  1 ;
      y    :=  0 ;
    end;

    k := 0 ;

    if revcolors then begin
      for i := 0 to h - 1 do begin
        w1 := y * FBitmapData.Pitch ;
        for x := 0 to w - 1 do begin
          cl := FPixelData[k] ;

          a := ( cl shr 24 ) and $ff ;
          b := ( cl shr 16 ) and $ff ;
          g := ( cl shr  8 ) and $ff ;
          r := ( cl        ) and $ff ;

          if Premultiplied and ( a <> 255 ) then begin
            r := Byte( r * a div 255 ) ;
            g := Byte( g * a div 255 ) ;
            b := Byte( b * a div 255 ) ;
          end;

          PInteger( IntPtr( FBitmapData.Data ) + w1 + x*4 )^
            := a shl 24 + r shl 16 + g shl 8 + b ;
          Inc( k ) ;
        end;
        y := y + step ;
      end;
    end
    else begin
      for i := 0 to h - 1 do begin
        w1 := y * FBitmapData.Pitch ;
        if Premultiplied then begin
          for x := 0 to w - 1 do begin
            cl := FPixelData[k] ;

            a := ( cl shr 24 ) and $ff ;
            r := ( cl shr 16 ) and $ff ;
            g := ( cl shr  8 ) and $ff ;
            b := ( cl        ) and $ff ;

            if a <> 255 then begin
              r := Byte( r * a div 255 ) ;
              g := Byte( g * a div 255 ) ;
              b := Byte( b * a div 255 ) ;
            end;

            PInteger( IntPtr( FBitmapData.Data ) + w1 + x*4 )^
              := a shl 24 + r shl 16 + g shl 8 + b ;
            Inc( k ) ;
          end;
        end
        else begin
          for x := 0 to w - 1 do begin
            PInteger( IntPtr( FBitmapData.Data ) + w1 +  x*4 )^
              := FPixelData[k] ;
            Inc( k ) ;
          end;
        end;
        y := y + step ;
      end;
    end;

    FBitmap.UnMap( FBitmapData ) ;
  end;

  SetLength( FPixelData, 0 ) ;
end ;

procedure TGIS_BitmapFMX.DrawShape(
  const _shape    :  TObject ;
  const _outline  :  Boolean ;
  var   _scale    :  Double  ;
  var   _offset   :  TPoint
) ;
begin
  DrawShape( _shape, 0, _outline, _scale, _offset ) ;
end;

procedure TGIS_BitmapFMX.DrawShape(
  const _shape    :  TObject ;
  const _ppi      :  Integer ;
  const _outline  :  Boolean ;
  var   _scale    :  Double  ;
  var   _offset   :  TPoint
) ;
begin
  DrawShape(  _shape, _ppi, _outline,
             TGIS_Color.LightGray, TGIS_Color.DimGray,
             _scale, _offset
          ) ;
end;

procedure TGIS_BitmapFMX.DrawSymbol(
  const _name     :  String ;
  const _ppi      :  Integer
) ;
begin
  DrawSymbol( _name, _ppi, TGIS_Color.RenderColor, TGIS_Color.RenderColor ) ;
end;

procedure TGIS_BitmapFMX.DrawShape(
  const _shape    :  TObject    ;
  const _ppi      :  Integer    ;
  const _outline  :  Boolean    ;
  const _areacolor:  TGIS_Color ;
  const _linecolor:  TGIS_Color ;
  var   _scale    :  Double     ;
  var   _offset   :  TPoint
) ;
var
  lv      : TGIS_LayerVector ;
  shp     : TGIS_Shape       ;
  shp_tmp : TGIS_Shape       ;
  ext_tmp : TGIS_Extent      ;
  vwr     : TGIS_ViewerBmp   ;
  pix     : TGIS_Pixels      ;
  ptg1    : TGIS_Point       ;
  pt1     : TPoint           ;
  ptg2    : TGIS_Point       ;
  pt2     : TPoint           ;
begin
  shp := TGIS_Shape( _shape ) ;

  // verify
  vwr := TGIS_ViewerBmp.Create( FMX.Graphics.TBitmap( self.Data ) ) ;
  try
    if _ppi > 0  then
      vwr.CustomPPI := _ppi ;

    vwr.Color := TGIS_Color.White ;
    lv := TGIS_LayerVector.Create ;

    if Assigned( shp.Layer ) then
      lv.CS := shp.Layer.CS ;

    shp_tmp := lv.AddShape( shp ) ;

    vwr.Add( lv ) ;

    if shp.LockLevel < TGIS_Lock.Projection then begin
      if Assigned( shp.Layer ) and Assigned( shp.Layer.Viewer ) then
        vwr.CS := shp.Layer.Viewer.Ref.CS ;
    end;

    vwr.FullExtent ;

    lv.IgnoreShapeParams := True ;
    lv.Params.Line.Width := -3 ;
    lv.Params.Line.Color := _linecolor ;
    if _outline then begin
      lv.Params.Area.OutlineWidth := -3 ;
      lv.Params.Area.OutlineColor := _linecolor ;
    end
    else begin
      lv.Params.Area.OutlineWidth := 0 ;
    end ;
    lv.Params.Area.Color := _areacolor ;

    vwr.Draw ;

    ext_tmp := shp_tmp.ProjectedExtent ;
    ptg1 := GisPoint( ext_tmp.XMin, ext_tmp.YMax ) ;
    pt1  := vwr.MapToScreen( ptg1 ) ;
    ptg2 := GisPoint( ext_tmp.XMax, ext_tmp.YMin ) ;
    pt2  := vwr.MapToScreen( ptg2 ) ;

    _scale  := Sqrt( Sqr( pt1.X  - pt2.X  ) + Sqr( pt1.Y  - pt2.Y  ) ) /
               Sqrt( Sqr( ptg1.X - ptg2.X ) + Sqr( ptg1.Y - ptg2.Y ) ) ;

    _offset := pt1 ;
  finally
    FreeObject( vwr ) ;
  end;
end;

procedure TGIS_BitmapFMX.DrawSymbol(
  const _name     :  String
) ;
begin
  DrawSymbol( _name, 0 ) ;
end;

procedure TGIS_BitmapFMX.DrawSymbol(
  const _name     :  String     ;
  const _ppi      :  Integer    ;
  const _areacolor:  TGIS_Color ;
  const _linecolor:  TGIS_Color
) ;
var
  lv      : TGIS_LayerVector ;
  shp     : TGIS_Shape       ;
  vwr     : TGIS_ViewerBmp   ;
  old_cnt : Boolean          ;
begin
  // verify
  vwr := TGIS_ViewerBmp.Create( FMX.Graphics.TBitmap( self.Data ) ) ;
  try
    if _ppi > 0  then
      vwr.CustomPPI := _ppi ;

    vwr.Color := TGIS_Color.None ;
    lv := TGIS_LayerVector.Create ;
    lv.Open ;

    shp := lv.CreateShape( TGIS_ShapeType.Point ) ;
    shp.AddPart ;
    shp.AddPoint( GisPoint( 0, 0 ) ) ;
    shp.AddPart ;
    lv.Extent := GisExtent( -90, -90, 90, 90 ) ;

    lv.Params.Marker.Color := _areacolor ;
    lv.Params.Marker.Size := -vwr.Width * 2 div 3 ;
    lv.Params.Marker.OutlineColor := _linecolor ;
    lv.Params.Marker.Symbol := SymbolList.Prepare( _name ) ;

    vwr.Add( lv ) ;

    old_cnt := lv.Params.Marker.Symbol.AutoCenter ;
    lv.Params.Marker.Symbol.AutoCenter := True ;

    vwr.FullExtent ;
    vwr.Draw ;

    lv.Params.Marker.Symbol.AutoCenter := old_cnt ;
  finally
    FreeObject( vwr ) ;
  end;
end;

procedure TGIS_BitmapFMX.DrawGlyph(
  const _symbol   :  TObject    ;
  const _ppi      :  Integer    ;
  const _color    :  TGIS_Color ;
  const _disabled :  Boolean
) ;
var
  lv      : TGIS_LayerVector ;
  shp     : TGIS_Shape       ;
  vwr     : TGIS_ViewerBmp   ;
  old_cnt : Boolean          ;
begin
  // verify
  vwr := TGIS_ViewerBmp.Create( FMX.Graphics.TBitmap( self.Data ) ) ;
  try
    vwr.CustomPPI := _ppi ;
    vwr.Color := TGIS_Color.None ;
    lv := TGIS_LayerVector.Create ;
    lv.Open ;

    shp := lv.CreateShape( TGIS_ShapeType.Point ) ;
    shp.AddPart ;
    shp.AddPoint( GisPoint( 0, 0 ) ) ;
    shp.AddPart ;
    lv.Extent := GisExtent( -90, -90, 90, 90 ) ;

    lv.Params.Marker.Color := _color ;
    lv.Params.Marker.Size := -vwr.Height ;
    lv.Params.Marker.OutlineColor := _color ;
    lv.Params.Marker.Symbol := TGIS_SymbolAbstract( _symbol ) ;

    vwr.Add( lv ) ;

    old_cnt := lv.Params.Marker.Symbol.AutoCenter ;
    lv.Params.Marker.Symbol.AutoCenter := True ;

    vwr.FullExtent ;
    vwr.Draw ;

    lv.Params.Marker.Symbol.AutoCenter := old_cnt ;
  finally
    FreeObject( vwr ) ;
  end;
end;

function TGIS_BitmapFMX.CreateViewer : IInterface ;
begin
  FreeObject( FViewer ) ;
  // verify
  FViewer := TGIS_ViewerBmp.Create( T_FMXBitmap( self.Data ) ) ;
  Result := FViewer as IGIS_Viewer;
end;

function TGIS_BitmapFactoryFMX.DoCreate(
  const _parent : TGIS_Bitmap ;
  const _width  : Integer ;
  const _height : Integer
) : TGIS_BitmapAbstract ;
begin
  Result := TGIS_BitmapFMX.Create( _width, _height ) ;
  Result.Master := _parent ;
end;

function TGIS_BitmapFactoryFMX.DoCreateFromBitmap(
  const _parent : TGIS_Bitmap ;
  const _bmp    : TObject
) : TGIS_BitmapAbstract ;
begin
  Result := TGIS_BitmapFMX.FromBitmap( _bmp ) ;
  Result.Master := _parent ;
end;

function TGIS_BitmapFactoryFMX.DoCreateFromFile(
  const _parent : TGIS_Bitmap ;
  const _path   : String
) : TGIS_BitmapAbstract ;
begin
  Result := TGIS_BitmapFMX.FromFile( _path ) ;
  Result.Master := _parent ;
  Result.Premultiplied := True ;
end;

function TGIS_BitmapFactoryFMX.DoCreateFromStream(
  const _parent : TGIS_Bitmap ;
  const _stream : TObject
) : TGIS_BitmapAbstract ;
begin
  Result := TGIS_BitmapFMX.FromStream( _stream ) ;
  Result.Master := _parent ;
  Result.Premultiplied := True ;
end;

function TGIS_BitmapFactoryFMX.DoCreateFromResource(
  const _parent : TGIS_Bitmap ;
  const _ref   : IntPtr ;
  const _name  : String
) : TGIS_BitmapAbstract ;
var
  strm : TStream ;
  href : IntPtr  ;
begin

  if _ref = 0 then
    href := HInstance
  else
    href := _ref ;

  strm := TResourceStream.Create( href, _name, RT_RCDATA ) ;
  try
    if not assigned( strm ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEMAPPING ),
                                   _name,
                                   0
                                 ) ;

    Result := DoCreateFromStream( _parent, strm );
  finally
    FreeObject( strm ) ;
  end ;
end;

function TGIS_BitmapFactoryFMX.NativeFormat
  : TGIS_BitmapFormat ;
begin
  {$UNDEF IMPL_TMP}

  {$IFDEF MSWINDOWS_OS}
    Result := TGIS_BitmapFormat.ARGB ;
    {$DEFINE IMPL_TMP}
  {$ENDIF}

  {$IFDEF MACOSX_OS}
    if ( ActiveRenderer and _Skia ) <> 0 then
      Result := TGIS_BitmapFormat.ARGB
    else
    if ( ActiveRenderer and _Metal ) <> 0 then
      Result := TGIS_BitmapFormat.ARGB
    else
      Result := TGIS_BitmapFormat.ABGR ;
    {$DEFINE IMPL_TMP}
  {$ENDIF}

  {$IFDEF IOS_OS}
    if ( ActiveRenderer and _Metal ) <> 0 then
      Result := TGIS_BitmapFormat.ARGB
    else
      Result := TGIS_BitmapFormat.ABGR ;
    {$DEFINE IMPL_TMP}
  {$ENDIF}

  {$IFDEF ANDROID_OS}
    Result := TGIS_BitmapFormat.ABGR ;
    {$DEFINE IMPL_TMP}
  {$ENDIF}

  {$IFDEF LINUX_OS}
    Result := TGIS_BitmapFormat.ARGB ;
    {$DEFINE IMPL_TMP}
  {$ENDIF}

  {$IFNDEF IMPL_TMP}
    {$MESSAGE ERROR 'UNDEFINED BitmapFormat'}
  {$ENDIF}
end;

function TGIS_BitmapFactoryFMX.NativeLineOrder
  : TGIS_BitmapLinesOrder ;
begin
  {$UNDEF IMPL_TMP}

  {$IFDEF MSWINDOWS}
    Result := TGIS_BitmapLinesOrder.Down ;
    {$DEFINE IMPL_TMP}
  {$ENDIF}

  {$IFDEF MACOSX_OS}
    Result := TGIS_BitmapLinesOrder.Down ;
    {$DEFINE IMPL_TMP}
  {$ENDIF}

  {$IFDEF IOS_OS}
    Result := TGIS_BitmapLinesOrder.Down ;
    {$DEFINE IMPL_TMP}
  {$ENDIF}

  {$IFDEF ANDROID_OS}
    Result := TGIS_BitmapLinesOrder.Down ;
    {$DEFINE IMPL_TMP}
  {$ENDIF}

  {$IFDEF LINUX_OS}
    Result := TGIS_BitmapLinesOrder.Down ;
    {$DEFINE IMPL_TMP}
  {$ENDIF}

  {$IFNDEF IMPL_TMP}
    {$MESSAGE ERROR 'UNDEFINED BitmapLineOrder'}
  {$ENDIF}
end;

function TGIS_BitmapFactoryFMX.BitmapType
  : TGIS_BitmapType ;
begin
  Result := TGIS_BitmapType.FMX;
end ;
{$ENDREGION}

{$REGION 'TGIS_FontFactoryFMX'}

function TGIS_FontFactoryFMX.DoCreate(
  const _name  : String ;
  const _size  : Integer ;
  const _style : TGIS_FontStyles ;
  const _color : TGIS_Color
) : TGIS_Font ;
begin
  Result := TGIS_Font.Create ;
  Result.Name      := _name  ;
  Result.Size      := _size  ;
  Result.Style     := _style ;
  Result.Color     := _color ;
end ;


function TGIS_FontFactoryFMX.DoCreateFromFont(
  const _font : TObject
) : TGIS_Font ;
var
  fnt : TFont ;
begin  fnt := TFont(_font) ;
  Result := DoCreate( fnt.Family,
                      TruncS( fnt.Size ),
                      GisFontStyle( fnt.Style ),
                      TGIS_Color.Black//GISColor( fnt.Color )
                     ) ;
end ;

{$ENDREGION 'TGIS_FontFactoryFMX'}

{$REGION 'TGIS_Timer'}

function TGIS_TimerFMX.fget_NativeTimer
  :  TObject ;
begin
  Result := FTimer ;
end;

function TGIS_TimerFMX.fget_Enabled
  : Boolean ;
begin
  Result := FTimer.Enabled ;
end;

procedure TGIS_TimerFMX.fset_Enabled(
  const _value : Boolean
) ;
begin
  FTimer.Enabled := _value ;
end;

function TGIS_TimerFMX.fget_Interval
  : Cardinal ;
begin
  Result := FTimer.Interval ;
end;

procedure TGIS_TimerFMX.fset_Interval(
  const _value: Cardinal
) ;
begin
  FTimer.Interval := _value ;
end;

function TGIS_TimerFMX.fget_OnTimer
  : TNotifyEvent ;
begin
  Result := FTimer.OnTimer ;
end;

procedure TGIS_TimerFMX.fset_OnTimer(
  const _value : TNotifyEvent
) ;
begin
  FTimer.OnTimer := _value ;
end;

procedure TGIS_TimerFMX.doDestroy ;
begin
  FreeObject( FTimer ) ;
  inherited ;
end ;

constructor TGIS_TimerFMX.Create ;
begin
  FTimer := TTimer.Create( nil ) ;
end;

function TGIS_TimerFactoryFMX.DoCreate
  : TGIS_TimerAbstract ;
begin
  Result := TGIS_TimerFMX.Create ;
end;

{$ENDREGION}

{$REGION 'TGIS_FrameworkUtils'}
class function TGIS_FrameworkUtils.ToPlatformColor(
  const _color : TGIS_Color
) : TAlphaColor ;
begin
  Result := FMXColor( _color ) ;
end;

class function TGIS_FrameworkUtils.FromPlatformColor(
  const _color : TAlphaColor
) : TGIS_Color ;
begin
  Result := GISColor( _color )
end;
{$ENDREGION 'TGIS_FrameworkUtils'}

{$REGION 'Public methods'}

function FMXColor( const _color : TGIS_Color ) : TAlphaColor ;
var
  cl : TGIS_Color ;
begin
  if _color.ARGB = TGIS_Color.RenderColor.ARGB then
    Result := TAlphaColorRec.Null
  else
    Result := MakeColor( _color.R, _color.G, _color.B, _color.A ) ;
end ;

function GISColor( const _color : TAlphaColor ) : TGIS_Color ;
begin
  Result := TGIS_Color.FromARGB( _color ) ;
end ;

function FMXFontStyle( const _style : TGIS_FontStyles ) : TFontStyles ;
begin
  Result := [] ;
  if TGIS_FontStyle.Bold in _style then
    Result := Result + [ TFontStyle.fsBold ] ;
  if TGIS_FontStyle.Italic in _style then
    Result := Result + [ TFontStyle.fsItalic ] ;
  if TGIS_FontStyle.Underline in _style then
    Result := Result + [ TFontStyle.fsUnderline ] ;
  if TGIS_FontStyle.StrikeOut in _style then
    Result := Result + [ TFontStyle.fsStrikeout ] ;
end ;

function GISFontStyle( const _style : TFontStyles ) : TGIS_FontStyles ;
begin
  Result := [] ;
  if TFontStyle.fsBold in _style then
    Result := Result + [ TGIS_FontStyle.Bold ] ;
  if TFontStyle.fsItalic in _style then
    Result := Result + [ TGIS_FontStyle.Italic ] ;
  if TFontStyle.fsUnderline in _style then
    Result := Result + [ TGIS_FontStyle.Underline ] ;
  if TFontStyle.fsStrikeout in _style then
    Result := Result + [ TGIS_FontStyle.StrikeOut ] ;
end ;

function FMXFont( const _font : TGIS_Font ) : TFont ;
begin
  Result := TFont.Create ;
  Result.Family := _font.Name ;
  Result.Size   := _font.Size * 96 / 72 ;
  Result.Style  := FMXFontStyle( _font.Style ) ;
end ;

{$IFDEF MSWINDOWS}
  function enum_fonts(
    var _logfont    : TLogFont    ;
    var _textmetric : TTextMetric ;
        _fonttype   : Integer     ;
        _data       : Pointer
  ): Integer; stdcall;
  var
    lst   : TStrings ;
    fname : String   ;
  begin
    lst   := TStrings( _data );
    fname := _logfont.lfFaceName;

    Result := 1 ;

    if _logfont.lfOutPrecision <> OUT_STROKE_PRECIS then
      exit ;

    if fname[1] = '@' then
      exit ;

    if ( lst.Count > 0 ) and
       ( CompareText( lst[ lst.Count-1 ], fname ) = 0 )
    then
      exit ;

    lst.Add( fname );
  end;

  procedure GetFontList(
    const _list : TStringlist
  );
  var
    dc      : HDC;
    logfont : TLogFont;
  begin
    dc := GetDC(0);
    FillChar( logfont, SizeOf( logfont ), 0 );
    logfont.lfCharset := DEFAULT_CHARSET;
    EnumFontFamiliesEx(
      dc,
      logfont,
      @enum_fonts,
      Winapi.Windows.LPARAM(_list), 0
    );
    ReleaseDC(0, dc);

    _list.Sort ;
  end;
{$ENDIF}

{$IFDEF MACOSX_OS}
  procedure GetFontList(
    const _list : TStringlist
  );
  var
    i     : Integer;
    fonts : NsFontManager;
    list  : NSArray ;
    item  : NSString;
  begin
    fonts := TNsFontManager.Wrap(TNsFontManager.OCClass.sharedFontManager);
    list := fonts.availableFontFamilies;
    if (list <> nil) and (list.count > 0) then begin
      for i := 0 to list.Count-1 do begin
        item := TNSString.Wrap(list.objectAtIndex(i));
        _list.Add(String(item.UTF8String));
      end;
    end;
  end ;
{$ENDIF}

{$IFDEF IOS_OS}
  procedure GetFontList(
    const _list : TStringlist
  );
  var
    i     : Integer;
    list  : NSArray ;
    item  : NSString;
  begin
    list := TUIFont.OCClass.familyNames ;
    if (list <> nil) and (list.count > 0) then begin
      for i := 0 to list.Count-1 do begin
        item := TNSString.Wrap(list.objectAtIndex(i));
       _list.Add(String(item.UTF8String));
      end;
     _list.Sort ;
    end;
  end ;
{$ENDIF}

{$IFDEF ANDROID}
  procedure GetFontList(
    const _list : TStringlist
  );
  begin
    _list.Add( 'Droid Sans' ) ;
    _list.Add( 'Droid Serif' ) ;
    _list.Add( 'Droid Sans Mono' ) ;
    _list.Sort ;
  end ;
{$ENDIF}

{$IFDEF LINUX}
  procedure GetFontList(
    const _list : TStringlist
  );
  var
    i : Integer ;
  begin
    for i := 0 to FmuxGetFontCount - 1 do begin
      _list.Add( FmuxGetFontName(i) ) ;
    end;
    _list.Sort ;
  end ;
{$ENDIF}

{$ENDREGION}

procedure EnsureFramework ;
begin
  FreeObject( NativeBitmapFactory ) ;
  FreeObject( TimerHelper ) ;
  FreeObject( FontHelper ) ;
  FreeObject( PrintPreviewHelper ) ;
  FreeObject( LegendViewerFactory ) ;

  NativeBitmapFactory := TGIS_BitmapFactoryFMX.Create ;
  TimerHelper  := TGIS_TimerFactoryFMX.Create ;
  FontHelper   := TGIS_FontFactoryFMX.Create ;

  PrintPreviewHelper := TGIS_PrintPreviewFactoryFMX.Create ;
  LegendViewerFactory := TGIS_LegendViewerFactoryFMX.Create ;
end;


initialization
  EnsureFramework ;
  {$IFDEF MSWINDOWS_MULTITOUCH_PAN_FIX}
    T_MultiTouchManagerWinFix.Hijack ;
  {$ENDIF}

finalization
  FreeObject( NativeBitmapFactory ) ;
  FreeObject( TimerHelper ) ;
  FreeObject( FontHelper ) ;
  FreeObject( PrintPreviewHelper ) ;
  FreeObject( LegendViewerFactory ) ;
  {$IFDEF MSWINDOWS_MULTITOUCH_PAN_FIX}
    T_MultiTouchManagerWinFix.ReleaseHijack ;
  {$ENDIF}

//==================================== END =====================================
end .


