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

unit VCL.GisFramework ;

{$INCLUDE GisInclude.inc}

interface

uses
  {$IFDEF MSWINDOWS}
    Winapi.Windows,
    Winapi.D2D1,
  {$ENDIF}
  System.SysUtils,
  System.Classes,
  Vcl.ExtCtrls,
  Vcl.ImgList,
  Vcl.Graphics,
  Vcl.Imaging.PngImage,
  Vcl.Imaging.Jpeg,
  Vcl.Direct2D,

  GisRtl,
  GisTypesUI;

  {$HPPEMIT '#pragma comment( lib, "vclimg" )'}

type
  /// <summary>
  ///   Platform dependent Bitmap implementation.
  /// </summary>
  TGIS_BitmapVCL = class( TGIS_BitmapAbstract )
    private
      FBitmap    : TBitmap ;
      FPixelData : TGIS_Pixels ;
      FWritable  : Boolean ;
      FFormat    : TGIS_BitmapFormat ;
      FLineOrder : TGIS_BitmapLinesOrder ;
      FViewer    : TComponent ;
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
      ///   Standard constructor
      /// </summary>
      constructor Create          ; overload;

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
      procedure   ToFile          ( const _path         : String ;
                                    const _format       : TGIS_PixelFormat ;
                                    const _subformat    : TGIS_PixelSubFormat ;
                                    const _compression  : Integer
                                  ) ; override;

      /// <inheritdoc/>
      procedure   ToStream        ( const _stream : TObject
                                  ) ; override;

      /// <inheritdoc/>
      procedure   ToStream        ( const _stream       : TObject ;
                                    const _format       : TGIS_PixelFormat ;
                                    const _subformat    : TGIS_PixelSubFormat ;
                                    const _compression  : Integer
                                  ) ; override;

      /// <inheritdoc/>
      procedure   MakeTransparent ; override;

      /// <inheritdoc/>
      procedure   Clear           ( const _color        : TGIS_Color
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
                                  ) ; overload; override;

      /// <inheritdoc/>
      procedure   DrawSymbol      ( const _name     :  String  ;
                                    const _ppi      :  Integer
                                  ) ; overload; override;

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
                                    const _enabled  :  Boolean
                                  ) ; overload ; override;

      {#gendoc:hide:GENXDK}
       {#gendoc:hide:GENSCR}
      /// <inheritdoc/>
      function CreateViewer       : IInterface ; override ;
  end;

  /// <summary>
  ///   Factory for platform dependent Bitmap implementation.
  /// </summary>
  TGIS_BitmapFactoryVCL = class( TGIS_BitmapFactory )
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
                                    const _ref   : IntPtr ;
                                    const _name  : String
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
  TGIS_FontFactoryVCL = class( TGIS_FontFactory )
    public
      /// <inheritdoc/>
      function DoCreate           ( const _name  : String ;
                                    const _size  : Integer ;
                                    const _style : TGIS_FontStyles ;
                                    const _color : TGIS_Color
                                  ) : TGIS_Font ; override;

      /// <inheritdoc/>
      function DoCreateFromFont   ( const _font    : TObject
                                  ) : TGIS_Font ; override;
  end ;

  /// <summary>
  ///   Platform dependent Timer implementation.
  /// </summary>
  TGIS_TimerVCL = class( TGIS_TimerAbstract )
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
  TGIS_TimerFactoryVCL =  class( TGIS_TimerFactory )
    public

      /// <inheritdoc/>
      function DoCreate          : TGIS_TimerAbstract ;
                                 override;
  end;

  TD2DCanvas = class ;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Wrapper for D2D & VCL objects used during Paint().
  ///   For internal use only.
  /// </summary>
  TGIS_BitmapInternal = class
    private
      FBitmap    : VCL.Graphics.TBitmap ;
      FGISBitmap : TGIS_Bitmap ;
      FD2DBitmap : ID2D1Bitmap ;
      FD2DCanvas : TD2DCanvas ;
      FD2DCanvasWnd : TD2DCanvas ;
      function fget_Width  : Integer ;
      function fget_Height : Integer ;
    public
      ///<inheritdoc/>
      destructor  Destroy ; override ;
    public
      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_target">
      ///   render target for whom
      ///   a compatible render target will be created
      ///   and encapsulated
      /// </param>
      constructor Create ( const _target : ID2D1RenderTarget
                         ) ; overload ;

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_canvasD2D">
      ///   TD2DCanvas object
      /// </param>
      /// <param name="_recreate">
      ///   recreate flag
      /// </param>
      constructor Create ( const _canvasD2D : TD2DCanvas ;
                           const _recreate  : Boolean
                         ) ; overload ;

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_bitmap">
      ///   encapsulated D2D bitmap
      /// </param>
      constructor Create ( const _bitmap   : ID2D1Bitmap
                         ) ; overload ;

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_bitmap">
      ///   encapsulated VCL bitmap
      /// </param>
      constructor Create ( const _bitmap : VCL.Graphics.TBitmap
                         ) ; overload ;

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_bitmap">
      ///   encapsulated GIS bitmap
      /// </param>
      constructor Create ( const _bitmap : TGIS_Bitmap
                         ) ; overload ;

      /// <summary>
      ///   Clear background with the given color.
      /// </summary>
      /// <param name="_color">
      ///   background color
      /// </param>
      procedure Clear    ( _color : TColor
                         ) ;
      /// <summary>
      ///   Bitmap used during standard rendering.
      /// </summary>
      property GISBitmap : TGIS_Bitmap read  FGISBitmap ;
      /// <summary>
      ///   D2D bitmap used during full D2D rendering.
      /// </summary>
      property D2DBitmap : ID2D1Bitmap read  FD2DBitmap
                                       write FD2DBitmap ;
      /// <summary>
      ///   D2D canvas used during full D2D rendering.
      /// </summary>
      property D2DCanvas : TD2DCanvas  read  FD2DCanvas ;
      /// <summary>
      ///   D2D window canvas used during full D2D rendering.
      /// </summary>
      property D2DCanvasWnd : TD2DCanvas  read  FD2DCanvasWnd ;
      /// <summary>
      ///   Width of the encapsulated object.
      /// </summary>
      property Width     : Integer     read  fget_Width ;
      /// <summary>
      ///   Height of the encapsulated object.
      /// </summary>
      property Height    : Integer     read  fget_Height ;
  end;

  /// <summary>
  ///   Direct2D Canvas.
  /// </summary>
  TD2DCanvas = class( TDirect2DCanvas )
    public

      /// <summary>
      ///   BitmapRenderTarget object compatible to the passed render target.
      /// </summary>
      CompatibleRenderTarget : ID2D1BitmapRenderTarget ;

    public
      /// <summary>
      ///   Create instance.
      /// </summary>
      /// <param name="_hwnd">
      ///   window handle
      /// </param>
      constructor Create( _hwnd : HWND ); overload ;

      /// <summary>
      ///   Create instance.
      /// </summary>
      /// <param name="_renderTarget">
      ///   render target for whom a compatible render target will be created.
      /// </param>
      constructor Create( const _renderTarget : ID2D1RenderTarget); overload;

      /// <summary>
      ///   Create VCL bitmap from D2D bitmap.
      /// </summary>
      /// <param name="_bmp">
      ///   D2D source bitmap
      /// </param>
      /// <returns>
      ///   created bitmap
      /// </returns>
      function CreateBitmap  ( const _bmp  : ID2D1Bitmap
                             ) : VCL.Graphics.TBitmap ;

      /// <summary>
      ///   Create native transparent pre-multiplied bitmap.
      /// </summary>
      /// <param name="_bmp">
      ///   VCL source bitmap
      /// </param>
      /// <returns>
      ///   created bitmap
      /// </returns>
      function CreateBitmapPremultiplied(
                               const _bmp  : VCL.Graphics.TBitmap
                             ) : ID2D1Bitmap ; overload ;
      /// <summary>
      ///   Create native transparent pre-multiplied bitmap.
      /// </summary>
      /// <param name="_bmp">
      ///   VCL source bitmap
      /// </param>
      /// <param name="_size">
      ///   bitmap size
      /// </param>
      /// <returns>
      ///   created bitmap
      /// </returns>
      function CreateBitmapPremultiplied(
                               const _bmp  : TGIS_Pixels ;
                               const _size : TPoint
                             ) : ID2D1Bitmap ; overload ;
      /// <summary>
      ///   Draw bitmap assuming that bitmap is already pre-multiplied.
      /// </summary>
      /// <param name="_left">
      ///   left corner
      /// </param>
      /// <param name="_top">
      ///   upper corner
      /// </param>
      /// <param name="_bmp">
      ///   bitmap to be drawn
      /// </param>
      procedure DrawBitmapPremultiplied(
                               const _left : Integer ;
                               const _top  : Integer ;
                               const _bmp  : VCL.Graphics.TBitmap
                             );
      /// <summary>
      ///   Draw bitmap assuming that bitmap is already pre-multiplied.
      /// </summary>
      /// <param name="_rect">
      ///   rectangle to fit provide bitmap
      /// </param>
      /// <param name="_bmp">
      ///   bitmap to be drawn
      /// </param>
      procedure StretchBitmapPremultiplied(
                               const _rect : TRect ;
                               const _bmp  : ID2D1Bitmap
                             ) ; overload ;

      /// <summary>
      ///   Draw bitmap assuming that bitmap is already pre-multiplied.
      /// </summary>
      /// <param name="_rect">
      ///   rectangle to fit provide bitmap
      /// </param>
      /// <param name="_bmp">
      ///   bitmap to be drawn
      /// </param>
      procedure StretchBitmapPremultiplied(
                               const _rect : TRect ;
                               const _bmp  : VCL.Graphics.TBitmap
                             ) ; overload ;
      /// <summary>
      ///   Draw bitmap with alpha ignoring background transparency.
      /// </summary>
      /// <param name="_bmp">
      ///   bitmap to be drawn
      /// </param>
      /// <param name="_transparency">
      ///   transparency for the bitmap
      /// </param>
      procedure DrawBitmap   ( const _bmp  : ID2D1Bitmap ;
                               const _transparency : Integer
                             ) ; overload ;
      /// <summary>
      ///   Draw bitmap with alpha ignoring background transparency.
      /// </summary>
      /// <param name="_bmp">
      ///   bitmap to be drawn
      /// </param>
      /// <param name="_transparency">
      ///   transparency for the bitmap
      /// </param>
      procedure DrawBitmap   ( const _bmp  : TBitmap ;
                               const _transparency : Integer
                             ) ; overload ;
      /// <summary>
      ///   Draw bitmap with alpha ignoring background transparency.
      /// </summary>
      /// <param name="_rect">
      ///   rectangle to fit provide bitmap
      /// </param>
      /// <param name="_bmp">
      ///   bitmap to be drawn
      /// </param>
      /// <param name="_transparency">
      ///   transparency for the bitmap
      /// </param>
      procedure StretchBitmap( const _rect : TRect ;
                               const _bmp  : ID2D1Bitmap ;
                               const _transparency
                                           : Integer
                             ) ; overload ;
      /// <summary>
      ///   Draw bitmap with alpha ignoring background transparency.
      /// </summary>
      /// <param name="_rect">
      ///   rectangle to fit provide bitmap
      /// </param>
      /// <param name="_bmp">
      ///   bitmap to be drawn
      /// </param>
      procedure StretchBitmap( const _rect : TRect ;
                               const _bmp  : VCL.Graphics.TBitmap
                             ) ; overload ;
      /// <summary>
      ///   Draw bitmap with alpha ignoring background transparency.
      /// </summary>
      /// <param name="_rect">
      ///   rectangle to fit provide bitmap
      /// </param>
      /// <param name="_bmp">
      ///   bitmap to be drawn
      /// </param>
      /// <param name="_size">
      ///   bitmap size
      /// </param>
      procedure StretchBitmap( const _rect : TRect ;
                               const _bmp  : TGIS_Pixels ;
                               const _size : TPoint
                             ) ; overload ;
  end ;

  /// <summary>
  ///   A class that groups platform's specific methods.
  /// </summary>
  TGIS_FrameworkUtils = class
    public
      /// <summary>
      ///   Convert platform independent color to native VCL color.
      /// </summary>
      /// <param name="_color">
      ///   color to be converted
      /// </param>
      /// <returns>
      ///   native VCL color
      /// </returns>
      class function ToPlatformColor   ( const _color : TGIS_Color
                                       ) : TColor ;
                                       static;
                                       {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}


      /// <summary>
      ///   Convert platform native VCL color to platform independent color.
      /// </summary>
      /// <param name="_color">
      ///   color to be converted
      /// </param>
      /// <returns>
      ///   platform independent color
      /// </returns>
      class function FromPlatformColor ( const _color : TColor
                                       ) : TGIS_Color ;
                                       static;
                                       {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
  end ;

  /// <summary>
  ///   Create TD2DCanvas object for given window handle.
  /// </summary>
  /// <param name="_hwnd">
  ///   window handle
  /// </param>
  /// <returns>
  ///   D2D canvas
  /// </returns>
  function CreateHwndD2DCanvas( _hwnd : HWND
                              ) : TD2DCanvas;

  /// <summary>
  ///   Scale image list for DPI aware application and apply colors based on active style.
  /// </summary>
  /// <param name="_in_list">
  ///   Source image list
  /// </param>
  /// <param name="_out_list">
  ///   Image list with scanned images. Must be allocated. Can be the same as
  ///   _in_list.
  /// </param>
  /// <param name="_n">
  ///   Scale factor nominator
  /// </param>
  /// <param name="_disabled">
  ///   If tue, prepare list in a disbaled state
  /// </param>
  /// <param name="_d">
  ///   Scale factor denominator
  /// </param>
  procedure GisScaleImageList(
   const _in_list  : TCustomImageList ;
   const _out_list : TCustomImageList ;
   const _n        : Integer    ;
   const _d        : Integer    ;
   const _disabled : Boolean = False
  ) ;

  /// <summary>
  ///   Force framework. To be used only upon design time.
  /// </summary>
  procedure EnsureFramework ;

  /// <summary>
  ///   Make bitmap ARGB compatible with VCL ARGB schema.
  /// </summary>
  /// <param name="_bmp">
  ///   bitmap to be modified
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///     VCL/GDI32 bitmap drawing expected non typical ARGB organization.
  ///     Mainly a full transparency must be encoded differently. See also
  ///   </note>
  /// </remarks>
  procedure VCLMakeCompatibleBitmap(
    const _bmp    : TBitmap
  ) ;

  /// <summary>
  ///   Draw bitmap on VCL canvas with proper ARGB handling.
  /// </summary>
  /// <param name="_canvas">
  ///   VCL canvas
  /// </param>
  /// <param name="_rect">
  ///   draw rectangle
  /// </param>
  /// <param name="_bmp">
  ///   bitmap to be drawn
  /// </param>
  /// <remarks>
  ///   See also VCLMakeCompatibleBitmap.
  /// </remarks>
  procedure VCLCanvasDrawBitmap(
    const _canvas : TCanvas ;
    const _rect   : TRect ;
    const _bmp    : TBitmap
  ) ;

  /// <summary>
  ///   Draw bitmap on VCL canvas using StretchDIBits.
  /// </summary>
  /// <param name="_canvas">
  ///   VCL canvas
  /// </param>
  /// <param name="_rect">
  ///   draw rectangle
  /// </param>
  /// <param name="_bmp">
  ///   bitmap to be drawn
  /// </param>
  /// <remarks>
  ///   To draw bitmap on some printers. See also VCLCanvasDrawBitmap.
  /// </remarks>
  procedure VCLCanvasDrawBitmapEx(
    const _canvas : TCanvas ;
    const _rect   : TRect ;
    const _bmp    : TBitmap
  ) ;

  /// <summary>
  ///   Convert native VCL color to BGR
  /// </summary>
  /// <param name="_color">
  ///   color to be converted
  /// </param>
  /// <returns>
  ///   BGR color
  /// </returns>
  function ColorToBGR( const _color : TColor ) : Integer ; inline ;

  /// <summary>
  ///   Convert platform independent color to native VCL color.
  /// </summary>
  /// <param name="_color">
  ///   color to be converted
  /// </param>
  /// <returns>
  ///   native VCL color
  /// </returns>
  function VCLColor( const _color : TGIS_Color ) : TColor ; inline ;

  /// <summary>
  ///   Convert platform native VCL color to platform independent color.
  /// </summary>
  /// <param name="_color">
  ///   color to be converted
  /// </param>
  /// <returns>
  ///   platform independent color
  /// </returns>
  function GISColor( const _color : TColor ) : TGIS_Color ; inline ;

  /// <summary>
  ///   Convert platform independent font style to native VCL font style.
  /// </summary>
  /// <param name="_style">
  ///   font style to be converted
  /// </param>
  /// <returns>
  ///   native VCL font style
  /// </returns>
  function VCLFontStyle( const _style : TGIS_FontStyles ) : TFontStyles ; inline ;

  /// <summary>
  ///   Convert platform native VCL font style to platform independent font style.
  /// </summary>
  /// <param name="_style">
  ///   font style to be converted
  /// </param>
  /// <returns>
  ///   platform independent font style
  /// </returns>
  function GISFontStyle( const _style : TFontStyles ) : TGIS_FontStyles ; inline ;

  /// <summary>
  ///   Prepare a non-transparent 'imitation' of transparent color.
  ///   If color is opaque the function returns the same color.
  /// </summary>
  /// <param name="_color">
  ///   color to be imitated
  /// </param>
  /// <returns>
  ///   imitation color
  /// </returns>
  function ImitationColor( _color : TGIS_Color ) : TGIS_Color ;

  /// <summary>
  ///   Create a native VCL font object from platform independent font.
  /// </summary>
  /// <param name="_font">
  ///   platform independent font
  /// </param>
  /// <returns>
  ///   native VCL font
  /// </returns>
  function VCLFont( const _font : TGIS_Font ) : TFont ;

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
    Winapi.DxgiFormat,
    Winapi.Wincodec,
    Winapi.ActiveX,
  {$ENDIF}
  System.Types,
  System.UITypes,
  System.Math,
  System.Win.ComObj,
  System.Generics.Collections,

  VCL.Themes,

  GisInterfaces,
  GisTypes,
  GisClasses,
  GisFunctions,
  GisResource,
  GisLayerVector,
  GisSymbol,
  GisUtils,
  GisPrintPreviewHelperAbstract,
  VCL.GisViewerBmp,
  GisRendererAbstract,
  VCL.GisRendererGdiPlus,
  VCL.GisControlPrintPreview;

{$REGION 'T_Direct2DCanvasHack'}
type
  T_Direct2DCanvasHack = class( TCustomCanvas )
  //-- the following code MUST be the same as
  //-- in Vcl.Direct2D.TDirect2DCanvas
  //---- start --------------------------
  strict private
    class var
      FSupported: TUncertainState;
      {$IFDEF LEVEL_RX103_RTL}
        FDefaultDrawTextOption: Integer;
      {$ENDIF}
  public
    FPen: TDirect2DPen;
    FFont: TDirect2DFont;
    FBrush:  TDirect2DBrush;

    FD2DObjects: TObject;
    {$IFDEF LEVEL_RX103_RTL}
      FDrawTextOption: Integer;
    {$ENDIF}

    //Attached GDI Resources
    FDC: HDC;
    FHwnd: HWND;
    FSubRect: TRect;

    FRenderTarget: ID2D1RenderTarget;
  //---- end  -----------------------------
  end;
{$ENDREGION}

{$REGION 'TGIS_Bitmap'}

procedure fix_alpha(
  const _bmp : TBitmap
) ;
var
  scanline : IntPtr  ;
  x        : Integer ;
  y        : Integer ;
  balpha   : Boolean ;
  bany     : Boolean ;
  pargb    : PCardinal ;
begin
  balpha := False ;
  bany   := False ;
  for y := 0 to _bmp.Height -1 do begin
    scanline := IntPtr( _bmp.ScanLine[y] ) ;
    for x := 0 to _bmp.Width -1 do begin
      pargb  := PCardinal( scanline + x*4 ) ;

      if ( pargb^ <> $FFFFFFFF ) and ( ( pargb^ and $FF000000 ) <> $00000000 )
      then begin
        balpha := True ;
        break ;
      end
      else
      if ( pargb^ <> $00000000 )
      then begin
        bany := True ;
      end;
    end;
    if balpha then begin
      bany := True ;
      break ;
    end;
  end;

  if not bany then
    exit ;

  // set FF only if image is not alpha aware
  if not balpha then begin
    for y := 0 to _bmp.Height -1 do begin
      scanline := IntPtr( _bmp.ScanLine[y] ) ;
      for x := 0 to _bmp.Width -1 do begin
        PByte( scanline + 3 + x*4 )^ := $FF ;
      end;
    end;
  end ;
end;

procedure apply_transparency(
  const _bmp   : TBitmap;
  const _color : TGIS_Color
) ;
var
  scanline : IntPtr  ;
  x        : Integer ;
  y        : Integer ;
  pargb    : PCardinal ;
begin
  if _color.ARGB = TGIS_Color.None.ARGB then
    exit ;

  for y := 0 to _bmp.Height -1 do begin
    scanline := IntPtr( _bmp.ScanLine[y] ) ;
    for x := 0 to _bmp.Width -1 do begin
      pargb  := PCardinal( scanline + x*4 ) ;
      if ( ( pargb^ xor _color.ARGB ) and $00FFFFFF ) = 0 then
        pargb^ := 0
      else begin
        pargb^ := pargb^ or $FF000000
      end;
    end;
  end;
end;

function bmp_from_picture(
  const _pct : TPicture
) : TGIS_BitmapVCL ;
var
  ctransp : TGIS_Color     ;
  pargb   : PCardinal      ;
begin
  ctransp := TGIS_Color.None ;

  if ( _pct.Graphic is TPngImage ) and
     ( TPngImage( _pct.Graphic ).TransparencyMode =
       TPNGTransparencyMode.ptmPartial
     )
  then begin
    Result := TGIS_BitmapVCL.Create ;
    Result.FBitmap := TBitmap.Create ;
    Result.FBitmap.Canvas.Lock ;
    try
      Result.FBitmap.Assign( _pct.Graphic );
    finally
      Result.FBitmap.Canvas.Unlock ;
    end;
    Assert( Result.FBitmap.PixelFormat = pf32bit );
    exit ;
  end
  else
  if ( _pct.Graphic is TPngImage ) and
     ( TPngImage( _pct.Graphic ).TransparencyMode =
       TPNGTransparencyMode.ptmBit
     )
  then begin
    Result := TGIS_BitmapVCL.Create ;
    Result.FBitmap := TBitmap.Create ;
    Result.FBitmap.Canvas.Lock ;
    try
      Result.FBitmap.Assign( _pct.Graphic );
      Result.FBitmap.PixelFormat := pf32bit;
    finally
      Result.FBitmap.Canvas.Unlock ;
    end;
    Assert( Result.FBitmap.PixelFormat = pf32bit );
    ctransp := TGIS_Color.FromBGR( TPngImage( _pct.Graphic ).TransparentColor ) ;
  end
  else
  if ( _pct.Graphic is TIcon )
  then begin
    Result := TGIS_BitmapVCL.Create ;
    Result.FBitmap := TBitmap.Create ;
    Result.FBitmap.Canvas.Lock ;
    try
      Result.FBitmap.Assign( _pct.Graphic );
      Result.FBitmap.PixelFormat := pf32bit;
    finally
      Result.FBitmap.Canvas.Unlock ;
    end;
    Assert( Result.FBitmap.PixelFormat = pf32bit );
    if TIcon( _pct.Graphic ).Transparent then begin
      pargb   := Result.FBitmap.ScanLine[0] ;
      ctransp := TGIS_Color.FromBGR( pargb^ ) ;
    end;
  end
  else begin
    Result := TGIS_BitmapVCL.Create ;
    Result.FBitmap := TBitmap.Create ;
    Result.FBitmap.Canvas.Lock ;
    try
      Result.FBitmap.Assign( _pct.Graphic );
      Result.FBitmap.PixelFormat := pf32bit;
    finally
      Result.FBitmap.Canvas.Unlock ;
    end;
    Assert( Result.FBitmap.PixelFormat = pf32bit );
  end ;

  fix_alpha( Result.FBitmap ) ;
  apply_transparency( Result.FBitmap, ctransp ) ;

  if Assigned( Result ) then
    Result.FBitmap.Modified := False ;
end;

function TGIS_BitmapVCL.fget_Width
  : Integer ;
begin
  Result := FBitmap.Width ;
end;

function TGIS_BitmapVCL.fget_Height
  : Integer ;
begin
  Result := FBitmap.Height ;
end;

function TGIS_BitmapVCL.fget_PPI
  : Integer ;
begin
  Result := 96 ;
end ;

procedure TGIS_BitmapVCL.fset_PPI(
  const _value : Integer
) ;
begin

end ;

function TGIS_BitmapVCL.fget_Data
  : TObject ;
begin
  Result := FBitmap ;
end ;

procedure TGIS_BitmapVCL.fset_Data(
  const _value : TObject
) ;
begin
  FreeObject( FBitmap ) ;
  FBitmap := TBitmap.Create() ;
  FBitmap.Canvas.Lock ;
  try
    FBitmap.Assign( TBitmap( _value ) );
    FBitmap.PixelFormat := pf32bit;
  finally
    FBitmap.Canvas.Unlock ;
  end;
  FBitmap.Modified := False ;
  Assert( FBitmap.PixelFormat = pf32bit ) ;
end ;

procedure TGIS_BitmapVCL.doDestroy ;
begin
  FreeObject( FViewer   ) ;
  FreeObject( FBitmap   ) ;
  inherited ;
end ;

constructor TGIS_BitmapVCL.Create ;
begin
  inherited ;

  FBitmap    := nil ;
  FPixelData := nil ;
  FWritable  := False ;
  FFormat    := TGIS_BitmapFormat.Native ;
  FViewer    := nil ;
end ;

constructor TGIS_BitmapVCL.Create(
  const _width  : Integer ;
  const _height : Integer
) ;
var
  y : Integer ;
begin
  inherited Create;

  FBitmap := TBitmap.Create ;
  FBitmap.PixelFormat := pf32bit ;
  FBitmap.Width  := _width ;
  FBitmap.Height := _height ;
  FBitmap.Modified := False ;

  for y := 0 to FBitmap.Height - 1 do begin
    FillChar( PByte( FBitmap.ScanLine[y] )^, FBitmap.Width * 4, $00 ) ;
  end ;
end;

class function TGIS_BitmapVCL.FromBitmap(
  const _bmp : TObject
) : TGIS_BitmapAbstract ;
var
  pct : TPicture ;
begin
  Result := nil ;
  if not ( _bmp is TBitmap ) then exit ;

  pct := TPicture.Create ;
  try
    pct.Assign(TBitmap(_bmp));

    Result := bmp_from_picture( pct ) ;
  finally
    FreeObject( pct ) ;
  end;
end;

class function TGIS_BitmapVCL.FromFile(
  const _path   : String
) : TGIS_BitmapAbstract ;
var
  pct : TPicture ;
begin
  pct := TPicture.Create ;
  try
    pct.LoadFromFile( _path ) ;
    Result := bmp_from_picture( pct ) ;
    Result.Premultiplied := True ;
  finally
    FreeObject( pct ) ;
  end;
end;

class function TGIS_BitmapVCL.FromStream(
  const _stream : TObject
) : TGIS_BitmapAbstract ;
{$IFDEF LEVEL_RX102_RTL}
  var
    pct : TPicture ;
  begin
    pct := TPicture.Create ;
    try
      pct.LoadFromStream( TStream( _stream ) );

      Result := bmp_from_picture( pct ) ;
      Result.Premultiplied := True;
    finally
      FreeObject( pct ) ;
    end;
  end;
{$ELSE}
  var
    pct  : TPicture   ;
    grp  : TGraphic   ;
    bmp  : TBitmap    ;
    png  : TPngImage  ;
    jpg  : TJPEGImage ;
    ipos : Int64      ;
    syg  : Word       ;
  begin
    Result := nil ;


    ipos := TStream( _stream ).Position ;
    TStream( _stream ).Read(syg, 2) ;
    TStream( _stream ).Position := ipos ;

    grp := nil ;

    try
      if syg = $4D42 then begin
        grp := TBitmap.Create ;
        TStream( _stream ).Position := ipos ;
        grp.LoadFromStream( TStream( _stream ) ) ;
      end
      else
      if syg = $5089 then begin
        grp := TPngImage.Create ;
        grp.LoadFromStream( TStream( _stream ) ) ;
      end
      else
      if syg = $D8FF then begin
        grp := TJPEGImage.Create ;
        grp.LoadFromStream( TStream( _stream ) ) ;
      end;

      if Assigned( grp ) then begin
        pct := TPicture.Create ;
        try
          pct.Assign( grp ) ;
          Result := bmp_from_picture( pct ) ;
        finally
          FreeObject( pct ) ;
        end;
      end
      else begin
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEBADFORMAT ), '', 0) ;
      end;
    finally
      FreeObject( grp ) ;
    end;
  end;
{$ENDIF}

procedure TGIS_BitmapVCL.ToFile(
  const _path   : String
) ;
begin
  Assert( Assigned( FBitmap ) ) ;
  FBitmap.SaveToFile( _path ) ;
end;

procedure TGIS_BitmapVCL.ToFile(
  const _path         : String ;
  const _format       : TGIS_PixelFormat ;
  const _subformat    : TGIS_PixelSubFormat ;
  const _compression  : Integer
) ;
begin
  Assert( Assigned( FBitmap ) ) ;
  FBitmap.SaveToFile( _path ) ;
end ;

procedure TGIS_BitmapVCL.ToStream(
  const _stream : TObject
) ;
begin
  Assert( Assigned( FBitmap ) ) ;
  FBitmap.SaveToStream( TStream( _stream ) ) ;
end;

procedure TGIS_BitmapVCL.ToStream(
  const _stream       : TObject ;
  const _format       : TGIS_PixelFormat ;
  const _subformat    : TGIS_PixelSubFormat ;
  const _compression  : Integer
) ;
type
  TRGB = packed record B, G, R : Byte end ;
var
  grp   : TGraphic   ;
  png   : TPngImage  ;
  i, k  : Integer ;
  ippix : IntPtr ;
  pa    : PByte ;
  a     : Byte ;
  cl    : TGIS_Color ;
  prgb  : ^TRGB ;
begin
  Assert( Assigned( FBitmap ) ) ;

  FBitmap.Canvas.Lock ;
  try
    case _subformat of
      TGIS_PixelSubFormat.BMP   :
        begin
          grp := TBitmap.Create ;
          try
            TBitmap( grp ).Canvas.Lock ;
            try
              grp.Assign( FBitmap ) ;
              grp.SaveToStream( TStream(_stream) ) ;
            finally
              TBitmap( grp ).Canvas.UnLock ;
            end;
          finally
            FreeObject( grp );
          end ;
        end ;
      TGIS_PixelSubFormat.JPEG  :
        begin
          grp := TJPEGImage.Create ;
          try
            TJPEGImage(grp).CompressionQuality := Max( 1, _compression ) ;
            TJPEGImage(grp).Grayscale := _format = TGIS_PixelFormat.Bit8 ;
            grp.Assign( FBitmap ) ;
            grp.SaveToStream( TStream(_stream) ) ;
          finally
            FreeObject( grp );
          end ;
        end ;
      TGIS_PixelSubFormat.PNG   :
        begin
          if _format = TGIS_PixelFormat.ARGB then
            png := TPngImage.CreateBlank(COLOR_RGBALPHA, 8, FBitmap.Width, FBitmap.Height)
          else if _format = TGIS_PixelFormat.Bit8 then
            png := TPngImage.CreateBlank(COLOR_PALETTE, 8, FBitmap.Width, FBitmap.Height)
          else
            png := TPngImage.Create ;

          grp := png ;
          png.Canvas.Lock ;
          try
            if _format = TGIS_PixelFormat.ARGB then begin
              png.CreateAlpha ;
              png.Canvas.CopyMode := cmSrcCopy ;
              png.Canvas.Draw( 0, 0, FBitmap ) ;

              for i := 0 to png.Height -1 do begin
                pa    := PByte( png.AlphaScanLine[i] ) ;
                ippix := IntPtr( FBitmap.ScanLine[i] ) ;
                prgb  := png.Scanline[i] ;

                for k := 0 to png.Width -1 do begin
                  cl  := TGIS_Color.FromARGB( PCardinal( ippix + k*4 )^ ) ;
                  a   := cl.A ;
                  pa^ := Byte( a )  ;
                  if pa^ <> 0 then begin
                    prgb^.B := Byte( cl.B * 255 div a ) ;
                    prgb^.R := Byte( cl.R * 255 div a ) ;
                    prgb^.G := Byte( cl.G * 255 div a ) ;
                  end
                  else begin
                    prgb^.B := Byte( cl.B * 255 ) ;
                    prgb^.R := Byte( cl.R * 255 ) ;
                    prgb^.G := Byte( cl.G * 255 ) ;
                  end ;

                  inc( pa ) ;
                  Inc( prgb ) ;
                end;
              end;
              png.Modified := True ;
            end
            else if _format = TGIS_PixelFormat.Bit8 then begin
              png.Canvas.CopyMode := cmSrcCopy ;
              png.Canvas.Draw( 0, 0, FBitmap ) ;
              png.Modified := True ;
            end
            else begin
              png.Canvas.Lock ; //?
              grp.Assign( FBitmap ) ;
              png.Canvas.UnLock ;
            end;

            grp.SaveToStream( TStream(_stream) ) ;
          finally
            png.Canvas.Unlock ;
            FreeObject( grp );
          end ;
        end ;
      else
        Assert( False, 'Unsupported' ) ;
    end;
  finally
    Fbitmap.Canvas.Unlock ;
  end;

end;

procedure TGIS_BitmapVCL.MakeTransparent ;
var
  scanline : IntPtr   ;
  x,y      : Integer  ;
  w,h      : Integer  ;
  co,cl    : Cardinal ;
begin
  Assert( Assigned( FBitmap ) ) ;


  co := GISColor( FBitmap.Canvas.Pixels[0,0] ).ARGB ;

  h := FBitmap.Height ;
  w := FBitmap.Width  ;

  for y := 0 to h - 1 do begin
    scanline := IntPtr( FBitmap.ScanLine[ y ] ) ;
    for x := 0 to w - 1 do begin
      cl := PCardinal( scanline + x*4 )^ ;
      if cl = co then
        PCardinal( scanline + x*4 )^ := TGIS_Color.None.ARGB ;
    end;
  end;

end ;

procedure TGIS_BitmapVCL.Clear(
  const _color : TGIS_Color
) ;
var
  i, j  : Integer ;
  prgbr : PRGBQuad ;
  r,g,b : Byte ;
  c     : Cardinal ;
begin
    FBitmap.Canvas.Lock ;
    try
      if _color.ARGB = TGIS_Color.None.ARGB then begin
        for i := 0 to FBitmap.Height-1 do begin
          prgbr := PRGBQuad(FBitmap.ScanLine[i]);
          for j := 0 to FBitmap.Width-1 do begin
            with prgbr^ do begin
              rgbReserved := 0 ;
              rgbRed      := 0 ;
              rgbGreen    := 0 ;
              rgbBlue     := 0 ;
            end ;
            Inc( prgbr );
          end ;
        end ;
      end
      else begin
        c := _color.ToBGR ;
        r := GetRValue( c ) ;
        g := GetGValue( c ) ;
        b := GetBValue( c ) ;
        for i := 0 to FBitmap.Height-1 do begin
          prgbr := PRGBQuad(FBitmap.ScanLine[i]);
          for j := 0 to FBitmap.Width-1 do begin
            with prgbr^ do begin
              rgbReserved := $FF ;
              rgbRed      := r ;
              rgbGreen    := g ;
              rgbBlue     := b ;
            end ;
            Inc( prgbr );
          end ;
        end ;
      end ;
    finally
      FBitmap.Canvas.Unlock ;
    end ;
end ;

procedure TGIS_BitmapVCL.LockPixels(
  var   _pixels   : TGIS_Pixels ;
  const _writable : Boolean     ;
  const _format   : TGIS_BitmapFormat ;
  const _order    : TGIS_BitmapLinesOrder
) ;
var
  scanline  : IntPtr  ;
  i,k       : Integer ;
  x,y       : Integer ;
  w,h       : Integer ;
  step      : Integer ;
  revcolors : Boolean ;
  revlines  : Boolean ;
  cl        : Int32   ;
  a,r,g,b   : Byte    ;
begin
  if ( Length( FPixelData ) <> FBitmap.Width * FBitmap.Height )
     or
     ( _writable <> FWritable )
     or
     ( _format <> FFormat )
     or
     ( _order  <> FLineOrder )
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
        scanline := IntPtr( FBitmap.ScanLine[ y ] ) ;
        for x := 0 to w - 1 do begin
          cl := PInteger( scanline + x*4 )^ ;
          if ( ( cl and Int32( $FF000000 ) ) =  0 )
             and
             ( ( cl and Int32( $00FFFFFF ) ) <> 0 )
          then
            cl := cl or Int32( $FF000000 ) ;

          a := ( cl shr 24 ) and $FF ;
          r := ( cl shr 16 ) and $FF ;
          g := ( cl shr  8 ) and $FF ;
          b := ( cl        ) and $FF ;

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
        scanline := IntPtr( FBitmap.ScanLine[ y ] ) ;

        if Premultiplied then begin
          for x := 0 to w - 1 do begin
            cl := PInteger( scanline + x*4 )^ ;
            if ( ( cl and Int32( $FF000000 ) ) =  0 )
               and
               ( ( cl and Int32( $00FFFFFF ) ) <> 0 )
            then
              cl := cl or Int32( $FF000000 ) ;

            a := ( cl shr 24 ) and $FF ;
            r := ( cl shr 16 ) and $FF ;
            g := ( cl shr  8 ) and $FF ;
            b := ( cl        ) and $FF ;

            if Premultiplied and ( a <> 255 ) and ( a <> 0 ) then begin
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
            cl := PInteger( scanline + x*4 )^ ;
            if ( ( cl and Int32( $FF000000 ) ) =  0 )
               and
               ( ( cl and Int32( $00FFFFFF ) ) <> 0 )
            then
              cl := cl or Int32( $FF000000 ) ;

            FPixelData[k] := cl ;
            Inc( k ) ;
          end;
        end;
        y := y + step ;
      end;
    end;

    FWritable  := _writable ;
    FFormat    := _format   ;
    FLineOrder := _order    ;
  end;

  _pixels := FPixelData  ;
end;

procedure TGIS_BitmapVCL.UnlockPixels ;
var
  scanline  : IntPtr  ;
  i,k       : Integer ;
  x,y       : Integer ;
  w,h       : Integer ;
  step      : Integer ;
  revcolors : Boolean ;
  revlines  : Boolean ;
  cl        : Int32   ;
  a,r,g,b   : Byte    ;
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
        scanline := IntPtr( FBitmap.ScanLine[ y ] ) ;
        for x := 0 to w - 1 do begin
          cl := FPixelData[k] ;

          a := ( cl shr 24 ) and $FF ;
          b := ( cl shr 16 ) and $FF ;
          g := ( cl shr  8 ) and $FF ;
          r := ( cl        ) and $FF ;

          if Premultiplied and ( a <> 255 ) then begin
            r := Byte( r * a div 255 ) ;
            g := Byte( g * a div 255 ) ;
            b := Byte( b * a div 255 ) ;
          end;

          PInteger( scanline + x*4 )^ := a shl 24 + r shl 16 + g shl 8 + b ;
          Inc( k ) ;
        end;
        y := y + step ;
      end;
    end
    else begin
      for i := 0 to h - 1 do begin
        scanline := IntPtr( FBitmap.ScanLine[ y ] ) ;
        if Premultiplied then begin
          for x := 0 to w - 1 do begin
            cl := FPixelData[k] ;

            a := ( cl shr 24 ) and $FF ;
            r := ( cl shr 16 ) and $FF ;
            g := ( cl shr  8 ) and $FF ;
            b := ( cl        ) and $FF ;

            if Premultiplied and ( a <> 255 ) then begin
              r := Byte( r * a div 255 ) ;
              g := Byte( g * a div 255 ) ;
              b := Byte( b * a div 255 ) ;
            end;

            PInteger( scanline + x*4 )^ := a shl 24 + r shl 16 + g shl 8 + b ;
            Inc( k ) ;
          end
        end
        else begin
          for x := 0 to w - 1 do begin
            PInteger( scanline + x*4 )^ := FPixelData[k] ;
            Inc( k ) ;
          end;
        end ;
        y := y + step ;
      end;
    end;
  end;

  SetLength( FPixelData, 0 ) ;
end ;

procedure TGIS_BitmapVCL.DrawShape(
  const _shape    :  TObject ;
  const _outline  :  Boolean ;
  var   _scale    :  Double  ;
  var   _offset   :  TPoint
) ;
begin
  DrawShape( _shape, 0, _outline, _scale, _offset ) ;
end;

procedure TGIS_BitmapVCL.DrawShape(
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

procedure TGIS_BitmapVCL.DrawShape(
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
  ptg1    : TGIS_Point       ;
  pt1     : TPoint           ;
  ptg2    : TGIS_Point       ;
  pt2     : TPoint           ;
  rdr     : TGIS_RendererAbstract ;
begin
  shp := TGIS_Shape( _shape ) ;

  rdr := TGIS_RendererVclGdiPlus.Create ;
  try
    vwr := TGIS_ViewerBmp.Create( self.Master, rdr ) ;
    try
      if _ppi > 0 then
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
      lv.Params.Line.Width := -3  ;
      lv.Params.Line.Color := _linecolor ;
      if _outline then
        lv.Params.Area.OutlineWidth := -3
      else
        lv.Params.Area.OutlineWidth := 0  ;
      lv.Params.Area.OutlineColor := _linecolor ;
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
  finally
    FreeObject( rdr ) ;
  end;
end;

procedure TGIS_BitmapVCL.DrawSymbol(
  const _name     :  String
) ;
begin
  DrawSymbol( _name, 0 ) ;
end;

procedure TGIS_BitmapVCL.DrawSymbol(
  const _name     :  String ;
  const _ppi      :  Integer
) ;
begin
  DrawSymbol( _name, _ppi, TGIS_Color.RenderColor, TGIS_Color.RenderColor ) ;
end;

procedure TGIS_BitmapVCL.DrawSymbol(
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
  rnd     : TGIS_RendererAbstract ;
begin
  rnd := TGIS_RendererVclGdiPlus.Create ;
  try
    vwr := TGIS_ViewerBmp.Create( Self.Master, rnd ) ;
    try
      if _ppi > 0 then
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
    end ;
  finally
    FreeObject( rnd ) ;
  end;
end;

procedure TGIS_BitmapVCL.DrawGlyph(
  const _symbol   :  TObject ;
  const _ppi      :  Integer ;
  const _color    :  TGIS_Color ;
  const _enabled  :  Boolean
) ;
var
  lv      : TGIS_LayerVector ;
  shp     : TGIS_Shape       ;
  vwr     : TGIS_ViewerBmp   ;
  old_cnt : Boolean          ;
  rnd     : TGIS_RendererAbstract ;
begin
  rnd := TGIS_RendererVclGdiPlus.Create ;
  try
    vwr := TGIS_ViewerBmp.Create( Self.Master ) ;
    //vwr := TGIS_ViewerBmp.Create( Self.Master, rnd ) ;
    try
      vwr.CustomPPI := _ppi ;
      vwr.Color := TGIS_Color.None ;
      lv := TGIS_LayerVector.Create ;
      lv.Open ;
      vwr.CustomPPI := 192 ;

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

      vwr.GIS_Bitmap.GetData( NativeBitmapFactory ) ;
      lv.Params.Marker.Symbol.AutoCenter := old_cnt ;
    finally
      FreeObject( vwr ) ;
    end;
  finally
    FreeObject( rnd ) ;
  end;
//  StyleServices.GetSystemColor( clBtnFace ) ;

//  Master.MakeDisabled( Vcl
end;

function TGIS_BitmapVCL.CreateViewer : IInterface ;
begin
  FreeObject( FViewer ) ;
  FViewer := TGIS_ViewerBmp.Create( TBitmap( self.Data ), nil ) ;
  Result := FViewer as IGIS_Viewer;
end;

function TGIS_BitmapFactoryVCL.DoCreate(
  const _parent : TGIS_Bitmap ;
  const _width  : Integer ;
  const _height : Integer
) : TGIS_BitmapAbstract ;
begin
  Result := TGIS_BitmapVCL.Create( _width, _height ) ;
  Result.Master := _parent;
end;

function TGIS_BitmapFactoryVCL.DoCreateFromBitmap(
  const _parent : TGIS_Bitmap ;
  const _bmp    : TObject
) : TGIS_BitmapAbstract ;
begin
  Result := TGIS_BitmapVCL.FromBitmap( _bmp ) ;
  Result.Master := _parent;
end;

function TGIS_BitmapFactoryVCL.DoCreateFromFile(
  const _parent : TGIS_Bitmap ;
  const _path   : String
) : TGIS_BitmapAbstract ;
begin
  Result := TGIS_BitmapVCL.FromFile( _path ) ;
  Result.Master := _parent;
end;

function TGIS_BitmapFactoryVCL.DoCreateFromStream(
  const _parent : TGIS_Bitmap ;
  const _stream : TObject
) : TGIS_BitmapAbstract ;
begin
  Result := TGIS_BitmapVCL.FromStream( _stream ) ;
  Result.Master := _parent;
end;

function TGIS_BitmapFactoryVCL.DoCreateFromResource(
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
{$ENDREGION}

{$REGION 'TGIS_Timer'}

function TGIS_TimerVCL.fget_NativeTimer
  :  TObject ;
begin
  Result := FTimer ;
end;

function TGIS_TimerVCL.fget_Enabled
  : Boolean ;
begin
  Result := FTimer.Enabled ;
end;

procedure TGIS_TimerVCL.fset_Enabled(
  const _value : Boolean
) ;
begin
  FTimer.Enabled := _value ;
end;

function TGIS_TimerVCL.fget_Interval
  : Cardinal ;
begin
  Result := FTimer.Interval ;
end;

procedure TGIS_TimerVCL.fset_Interval(
  const _value: Cardinal
) ;
begin
  FTimer.Interval := _value ;
end;

function TGIS_TimerVCL.fget_OnTimer
  : TNotifyEvent ;
begin
  Result := FTimer.OnTimer ;
end;

procedure TGIS_TimerVCL.fset_OnTimer(
  const _value : TNotifyEvent
) ;
begin
  FTimer.OnTimer := _value ;
end;

procedure TGIS_TimerVCL.doDestroy ;
begin
  FreeObject( FTimer ) ;
  inherited ;
end ;

constructor TGIS_TimerVCL.Create ;
begin
  FTimer := TTimer.Create( nil ) ;
end;

function TGIS_TimerFactoryVCL.DoCreate
  : TGIS_TimerAbstract ;
begin
  Result := TGIS_TimerVCL.Create ;
end;

function TGIS_BitmapFactoryVCL.NativeFormat
  : TGIS_BitmapFormat ;
begin
  Result := TGIS_BitmapFormat.ARGB ;
end;

function TGIS_BitmapFactoryVCL.NativeLineOrder
  : TGIS_BitmapLinesOrder ;
begin
  Result := TGIS_BitmapLinesOrder.Down ;
end;

function TGIS_BitmapFactoryVCL.BitmapType
  : TGIS_BitmapType ;
begin
  Result := TGIS_BitmapType.VCL ;
end ;
{$ENDREGION}

{$REGION 'TGIS_FontFactory'}

function TGIS_FontFactoryVCL.DoCreate(
  const _name   : String ;
  const _size   : Integer ;
  const _style  : TGIS_FontStyles ;
  const _color  : TGIS_Color
) : TGIS_Font ;
begin
  Result := TGIS_Font.Create ;
  Result.Name      := _name  ;
  Result.Size      := _size  ;
  Result.Style     := _style ;
  Result.Color     := _color ;
end;

function TGIS_FontFactoryVCL.DoCreateFromFont(
  const _font : TObject
) : TGIS_Font ;
var
  fnt : TFont ;
begin
  fnt := TFont(_font) ;
  Result := DoCreate( fnt.Name,
                      fnt.Size,
                      GisFontStyle( fnt.Style ),
                      GISColor( fnt.Color )
                     ) ;
end;

{$ENDREGION}


{$REGION 'TGIS_BitmapInternal'}
  function TGIS_BitmapInternal.fget_Width  : Integer ;
  var
    sz : D2D_SIZE_U ;
  begin
    if assigned( FD2DCanvas ) then begin
      FD2DCanvas.RenderTarget.GetPixelSize(sz);
      Result := RoundS(sz.width) ;
    end
    else if assigned( FD2DBitmap ) then begin
      FD2DBitmap.GetPixelSize(sz);
      Result := RoundS(sz.width) ;
    end
    else if assigned( FBitmap ) then
      Result := FBitmap.Width
    else if assigned( FGISBitmap ) then
      Result := FGISBitmap.Width
    else
      Result := 0 ;
  end ;

  function TGIS_BitmapInternal.fget_Height : Integer ;
  var
    sz : D2D_SIZE_U ;
  begin
    if assigned( FD2DCanvas ) then begin
      FD2DCanvas.RenderTarget.GetPixelSize(sz);
      Result := RoundS(sz.height) ;
    end
    else if assigned( FD2DBitmap ) then begin
      FD2DBitmap.GetPixelSize(sz);
      Result := RoundS(sz.height) ;
    end else if assigned( FBitmap ) then
      Result := FBitmap.Height
    else if assigned( FGISBitmap ) then
      Result := FGISBitmap.Height
    else
      Result := 0 ;
  end ;

  destructor TGIS_BitmapInternal.Destroy ;
  begin
    FD2DCanvasWnd := nil ;
    if assigned( FD2DCanvas ) then begin
      T_Direct2DCanvasHack(FD2DCanvas).FRenderTarget := nil ;
      FreeObject( FD2DCanvas ) ;
    end;
    if assigned( FD2DBitmap ) then
      FD2DBitmap := nil ;
    if assigned( FBitmap ) then
      FreeObject( FBitmap ) ;
    if assigned( FGISBitmap ) then
      FreeObject( FGISBitmap ) ;
  end ;

  constructor TGIS_BitmapInternal.Create(
    const _target : ID2D1RenderTarget
  ) ;
    procedure d2dCanvas_initialization ;
    begin
      T_Direct2DCanvasHack(FD2DCanvas).FRenderTarget :=
        TD2DCanvas(FD2DCanvas).CompatibleRenderTarget ;
      T_Direct2DCanvasHack(FD2DCanvas).FD2DObjects :=
        TList<TDirect2DGraphicsObject>.Create();
      T_Direct2DCanvasHack(FD2DCanvas).FPen   := TDirect2DPen.Create(FD2DCanvas);
      T_Direct2DCanvasHack(FD2DCanvas).FFont  := TDirect2DFont.Create(FD2DCanvas);
      T_Direct2DCanvasHack(FD2DCanvas).FBrush := TDirect2DBrush.Create(FD2DCanvas);
      FD2DCanvas.PenPos := Point(0,0);
      {$IFDEF LEVEL_RX103_RTL}
        FD2DCanvas.DrawTextOption := D2D1_DRAW_TEXT_OPTIONS_NONE ;
      {$ENDIF}
    end;

  begin
    inherited Create;
    FD2DCanvasWnd := nil ;
    FD2DCanvas := TD2DCanvas.Create( _target ) ;
    d2dCanvas_initialization ;
    FD2DBitmap := nil ;
    FBitmap := nil ;
    FGISBitmap := nil ;
  end ;

  constructor TGIS_BitmapInternal.Create(
    const _canvasD2D : TD2DCanvas ;
    const _recreate  : Boolean
  ) ;
    procedure d2dCanvas_initialization ;
    begin
      T_Direct2DCanvasHack(FD2DCanvas).FRenderTarget :=
        TD2DCanvas(FD2DCanvas).CompatibleRenderTarget ;
      T_Direct2DCanvasHack(FD2DCanvas).FD2DObjects :=
        TList<TDirect2DGraphicsObject>.Create();
      T_Direct2DCanvasHack(FD2DCanvas).FPen   := TDirect2DPen.Create(FD2DCanvas);
      T_Direct2DCanvasHack(FD2DCanvas).FFont  := TDirect2DFont.Create(FD2DCanvas);
      T_Direct2DCanvasHack(FD2DCanvas).FBrush := TDirect2DBrush.Create(FD2DCanvas);
      FD2DCanvas.PenPos := Point(0,0);
      {$IFDEF LEVEL_RX103_RTL}
        FD2DCanvas.DrawTextOption := D2D1_DRAW_TEXT_OPTIONS_NONE ;
      {$ENDIF}
    end;

  begin
    inherited Create;
    FD2DCanvasWnd := _canvasD2D ;
    if _recreate then begin
      FD2DCanvas := TD2DCanvas.Create( FD2DCanvasWnd.RenderTarget ) ;
      d2dCanvas_initialization ;
    end
    else
      FD2DCanvas := nil ;
    FD2DBitmap := nil ;
    FBitmap := nil ;
    FGISBitmap := nil ;
  end ;

  constructor TGIS_BitmapInternal.Create(
    const _bitmap : ID2D1Bitmap
  ) ;
  begin
    inherited Create;
    FD2DCanvasWnd := nil ;
    FD2DCanvas := nil ;
    FD2DBitmap := _bitmap ;
    FBitmap := nil ;
    FGISBitmap := nil ;
  end ;

  constructor TGIS_BitmapInternal.Create(
    const _bitmap : VCL.Graphics.TBitmap
  ) ;
  begin
    inherited Create;
    FD2DCanvasWnd := nil ;
    FD2DCanvas := nil ;
    FD2DBitmap := nil ;
    FBitmap := _bitmap ;
    FGISBitmap := nil ;
  end ;

  constructor TGIS_BitmapInternal.Create(
    const _bitmap : TGIS_Bitmap
  ) ;
  begin
    inherited Create;
    FD2DCanvasWnd := nil ;
    FD2DCanvas := nil ;
    FD2DBitmap := nil ;
    FBitmap    := nil ;
    FGISBitmap := _bitmap ;
  end ;

procedure TGIS_BitmapInternal.Clear(
  _color : TColor
) ;
var
  sz : D2D_SIZE_F ;
begin
  if assigned( D2DCanvas ) then begin
    D2DCanvas.RenderTarget.GetSize(sz);
    D2DCanvas.Brush.Style := bsSolid ;
    D2DCanvas.Brush.Color := _color ;
    D2DCanvas.FillRect( Rect(0, 0, RoundS(sz.width), RoundS(sz.height) ) );
  end
  else if assigned( GISBitmap ) then
    GISBitmap.Clear( GisColor( _color ) ) ;
end;
{$ENDREGION}

{$REGION 'TD2DCanvas'}

function CreateHwndD2DCanvas(
  _hwnd : HWND
) : TD2DCanvas ;

  procedure d2dCanvas_initialization(
    _d2dCanvas : TD2DCanvas
  ) ;
  begin
    T_Direct2DCanvasHack(_d2dCanvas).FD2DObjects :=
      TList<TDirect2DGraphicsObject>.Create();
    T_Direct2DCanvasHack(_d2dCanvas).FPen   := TDirect2DPen.Create(_d2dCanvas);
    T_Direct2DCanvasHack(_d2dCanvas).FFont  := TDirect2DFont.Create(_d2dCanvas);
    T_Direct2DCanvasHack(_d2dCanvas).FBrush := TDirect2DBrush.Create(_d2dCanvas);
    _d2dCanvas.PenPos := Point(0,0);
    {$IFDEF LEVEL_RX103_RTL}
      _d2dCanvas.DrawTextOption := D2D1_DRAW_TEXT_OPTIONS_NONE ;
    {$ENDIF}
  end;

begin
  Result := TD2DCanvas.Create(_hwnd);
  d2dCanvas_initialization( Result ) ;
end;

constructor TD2DCanvas.Create(
  _hwnd : HWND
) ;
var
  rct   : TRect ;
  props : TD2D1RenderTargetProperties ;
  hres  : HRESULT ;
begin
  inherited Create ;
  GetClientRect( _hwnd, rct ) ;
  props.pixelFormat.format    := DXGI_FORMAT_UNKNOWN ;
  props.pixelFormat.alphaMode := D2D1_ALPHA_MODE_UNKNOWN ;
  props.&type := D2D1_RENDER_TARGET_TYPE_SOFTWARE ;
  props.usage := D2D1_RENDER_TARGET_USAGE_GDI_COMPATIBLE ;
  props.minLevel := D2D1_FEATURE_LEVEL_DEFAULT ;
  props.dpiX := 0 ;
  props.dpiY := 0 ;
  hres := D2DFactory.CreateHwndRenderTarget(
    props,
    D2D1HwndRenderTargetProperties(
      _hwnd,
      D2D1SizeU( rct.Right - rct.Left, rct.Bottom - rct.Top )
    ),
    ID2D1HwndRenderTarget( T_Direct2DCanvasHack(self).FRenderTarget )
  ) ;
  System.Win.ComObj.OleCheck( hres );
end;

constructor TD2DCanvas.Create(
  const _renderTarget : ID2D1RenderTarget
) ;
var
  fmt : D2D1_PIXEL_FORMAT ;
begin
  inherited Create ;
  fmt.format := DXGI_FORMAT_UNKNOWN ;
  fmt.alphaMode := D2D1_ALPHA_MODE_UNKNOWN ;
  _renderTarget.CreateCompatibleRenderTarget(
    nil, nil, @fmt, D2D1_COMPATIBLE_RENDER_TARGET_OPTIONS_NONE,
    CompatibleRenderTarget
  ) ;
end;

function TD2DCanvas.CreateBitmap(
  const _bmp : ID2D1Bitmap
) : VCL.Graphics.TBitmap ;
var
  sz_f : D2D_SIZE_F ;

  procedure d2Dbitmap_to_tbitmap(
    _bmp  : ID2D1Bitmap ;
    _tbmp : VCL.Graphics.TBitmap
  ) ;
  var
    factory    : ID2D1Factory ;
    WICfactory : IWICImagingFactory ;
    WICbitmap  : IWICBitmap ;
    properties : TD2D1RenderTargetProperties ;
    target     : ID2D1RenderTarget ;
    w, h : Integer ;
    buf  : TBytes ;
    bmp_info : TBitmapInfo ;
    hbmp : HBitmap ;
  begin
    // get factory
    RenderTarget.GetFactory( factory ) ;
    // create WIC factory
    CoCreateInstance( CLSID_WICImagingFactory,
                      nil,
                      CLSCTX_INPROC_SERVER,
                      IUnknown,
                      WICfactory
                    ) ;
    w := _tbmp.Width ;
    h := _tbmp.Height ;
    try
      // create WIC bitmap
      WICfactory.CreateBitmap( w, h,
                               @GUID_WICPixelFormat32bppPBGRA,
                               WICBitmapCacheOnLoad,
                               WICbitmap
                             ) ;
      properties.pixelFormat.format    := DXGI_FORMAT_B8G8R8A8_UNORM;
      properties.pixelFormat.alphaMode := D2D1_ALPHA_MODE_PREMULTIPLIED;
      properties.&type := D2D1_RENDER_TARGET_TYPE_SOFTWARE ;
      properties.usage := D2D1_RENDER_TARGET_USAGE_GDI_COMPATIBLE ;
      properties.minLevel := D2D1_FEATURE_LEVEL_DEFAULT ;
      properties.dpiX := 0 ;
      properties.dpiY := 0 ;
      // create render target for WIC bitmap
      factory.CreateWicBitmapRenderTarget( WICBitmap, properties, target ) ;
      target.BeginDraw ;
      try
        // render D2D1Bitmap
        target.DrawBitmap( _bmp );
      finally
        target.EndDraw ;
        target := nil ;
      end;
      // copy pixels to buffer
      SetLength( buf, w*h*4 );
      WICbitmap.CopyPixels(nil, w*4, w*h*4, @buf[0]) ;
      // copy buffer to TBitmap
      FillChar( bmp_info, SizeOf(BitmapInfo), 0 );
      bmp_info.bmiHeader.biSize     := Sizeof( bmp_info.bmiHeader );
      bmp_info.bmiHeader.biHeight   := -h;
      bmp_info.bmiHeader.biWidth    := w;
      bmp_info.bmiHeader.biPlanes   := 1;
      bmp_info.bmiHeader.biBitCount := 32;
      hbmp := _tbmp.Handle ;
      SetDIBits( _tbmp.Canvas.Handle,
                 hbmp, 0,
                 _tbmp.Height,
                 @buf[0],
                 bmp_info,
                 DIB_RGB_COLORS
               );
    finally
      WICbitmap := nil ;
      WICfactory := nil ;
    end;
  end ;

begin
  _bmp.GetSize(sz_f);
  Result := VCL.Graphics.TBitmap.Create ;
  Result.Width := RoundS(sz_f.width) ;
  Result.Height := RoundS(sz_f.height) ;
  Result.PixelFormat := pf32bit ;
  // copy cache to TBitmap
  d2Dbitmap_to_tbitmap( _bmp, Result ) ;
end ;

function TD2DCanvas.CreateBitmapPremultiplied(
  const _bmp  : VCL.Graphics.TBitmap
) : ID2D1Bitmap ;
var
  bmp_info       : TBitmapInfo ;
  buf            : TBytes      ;
  bmp_properties : TD2D1BitmapProperties;
  hbmp           : HBitmap     ;
begin
  FillChar( bmp_info, SizeOf(BitmapInfo), 0 );
  bmp_info.bmiHeader.biSize     := Sizeof( bmp_info.bmiHeader );
  bmp_info.bmiHeader.biHeight   := -_bmp.Height;
  bmp_info.bmiHeader.biWidth    := _bmp.Width;
  bmp_info.bmiHeader.biPlanes   := 1;
  bmp_info.bmiHeader.biBitCount := 32;

  SetLength (buf, _bmp.Height * _bmp.Width * 4 );

  hbmp := _bmp.Handle;
  GetDIBits( _bmp.Canvas.Handle,
             hbmp, 0,
             _bmp.Height,
             @buf[0],
             bmp_info,
             DIB_RGB_COLORS
           );

  bmp_properties.dpiX := 0;
  bmp_properties.dpiY := 0;
  bmp_properties.pixelFormat.format    := DXGI_FORMAT_B8G8R8A8_UNORM;
  bmp_properties.pixelFormat.alphaMode := D2D1_ALPHA_MODE_PREMULTIPLIED;

  RenderTarget.CreateBitmap(
    D2D1SizeU(_bmp.Width, _bmp.Height),
    @buf[0],
    4*_bmp.Width,
    bmp_properties,
    Result
  );
end ;

function TD2DCanvas.CreateBitmapPremultiplied(
  const _bmp  : TGIS_Pixels ;
  const _size : TPoint
) : ID2D1Bitmap ;
var
  width  : Integer ;
  height : Integer ;
  cl     : Integer ;
  i      : Integer ;
  a, r, g, b : Byte ;
  bmp_properties : TD2D1BitmapProperties;
begin
  width  := _size.X ;
  height := _size.Y ;

  // make premultiplied
  for i := 0 to height * width - 1 do begin
    cl := _bmp[i] ;
    a := ( cl shr 24 ) and $FF ;
    r := ( cl shr 16 ) and $FF ;
    g := ( cl shr  8 ) and $FF ;
    b := ( cl        ) and $FF ;

    r := Byte( r * a div 255 ) ;
    g := Byte( g * a div 255 ) ;
    b := Byte( b * a div 255 ) ;

    _bmp[i] := a shl 24 + r shl 16 + g shl 8 + b ;
  end;

  bmp_properties.dpiX := 0;
  bmp_properties.dpiY := 0;
  bmp_properties.pixelFormat.format    := DXGI_FORMAT_B8G8R8A8_UNORM;
  bmp_properties.pixelFormat.alphaMode := D2D1_ALPHA_MODE_PREMULTIPLIED;

  RenderTarget.CreateBitmap(
    D2D1SizeU(width, height),
    @_bmp[0],
    4*width,
    bmp_properties,
    Result
  );
end;

procedure TD2DCanvas.DrawBitmapPremultiplied(
  const _left : Integer ;
  const _top  : Integer ;
  const _bmp  : VCL.Graphics.TBitmap
);
begin
  StretchBitmapPremultiplied(
    Rect( _left, _top, _left + _bmp.Width, _top + _bmp.Height ),
    _bmp
  );
end;

procedure TD2DCanvas.StretchBitmapPremultiplied(
  const _rect : TRect ;
  const _bmp  : ID2D1Bitmap
) ;
var
  rct : TD2DRectF ;
begin
  rct.Left   := _rect.Left   ;
  rct.Right  := _rect.Right  ;
  rct.Top    := _rect.Top    ;
  rct.Bottom := _rect.Bottom ;
  RenderTarget.DrawBitmap( _bmp, @rct, 1 ) ;
end ;

procedure TD2DCanvas.StretchBitmapPremultiplied(
  const _rect : TRect ;
  const _bmp  : VCL.Graphics.TBitmap
);
var
  bmp            : ID2D1Bitmap ;
  rct            : TD2DRectF   ;
  bmp_info       : TBitmapInfo ;
  buf            : TBytes      ;
  bmp_properties : TD2D1BitmapProperties;
  hbmp           : HBitmap     ;
begin
  FillChar( bmp_info, SizeOf(BitmapInfo), 0 );
  bmp_info.bmiHeader.biSize     := Sizeof( bmp_info.bmiHeader );
  bmp_info.bmiHeader.biHeight   := -_bmp.Height;
  bmp_info.bmiHeader.biWidth    := _bmp.Width;
  bmp_info.bmiHeader.biPlanes   := 1;
  bmp_info.bmiHeader.biBitCount := 32;

  SetLength (buf, _bmp.Height * _bmp.Width * 4 );

  hbmp := _bmp.Handle;
  GetDIBits( _bmp.Canvas.Handle,
             hbmp, 0,
             _bmp.Height,
             @buf[0],
             bmp_info,
             DIB_RGB_COLORS
           );

  bmp_properties.dpiX := 0;
  bmp_properties.dpiY := 0;
  bmp_properties.pixelFormat.format    := DXGI_FORMAT_B8G8R8A8_UNORM;
  bmp_properties.pixelFormat.alphaMode := D2D1_ALPHA_MODE_PREMULTIPLIED;

  RenderTarget.CreateBitmap(
    D2D1SizeU(_bmp.Width, _bmp.Height),
    @buf[0],
    4*_bmp.Width,
    bmp_properties,
    bmp
  );

  rct.Left   := _rect.Left   ;
  rct.Right  := _rect.Right  ;
  rct.Top    := _rect.Top    ;
  rct.Bottom := _rect.Bottom ;
  RenderTarget.DrawBitmap( bmp, @rct, 1);
end;

procedure TD2DCanvas.DrawBitmap(
  const _bmp : ID2D1Bitmap ;
  const _transparency : Integer
) ;
var
  rct : TD2DRectF ;
  sz : D2D_SIZE_F ;
begin
  RenderTarget.GetSize(sz);
  rct.Left   := 0 ;
  rct.Right  := sz.width ;
  rct.Top    := 0 ;
  rct.Bottom := sz.height ;
  RenderTarget.DrawBitmap( _bmp, @rct, _transparency/100);
end ;

procedure TD2DCanvas.DrawBitmap(
  const _bmp : TBitmap ;
  const _transparency : Integer
) ;
var
  bb : ID2D1Bitmap ;
begin
  bb := CreateBitmapPremultiplied( _bmp ) ;
  try
    DrawBitmap( bb, _transparency ) ;
  finally
    bb := nil ;
  end ;
end ;

procedure TD2DCanvas.StretchBitmap(
  const _rect : TRect ;
  const _bmp  : VCL.Graphics.TBitmap
);
var
  bmp            : ID2D1Bitmap ;
  rct            : TD2DRectF   ;
  bmp_info       : TBitmapInfo ;
  buf            : TBytes      ;
  bmp_properties : TD2D1BitmapProperties;
  hbmp           : HBitmap     ;
begin
  FillChar( bmp_info, SizeOf(BitmapInfo), 0 );
  bmp_info.bmiHeader.biSize     := Sizeof( bmp_info.bmiHeader );
  bmp_info.bmiHeader.biHeight   := -_bmp.Height;
  bmp_info.bmiHeader.biWidth    := _bmp.Width;
  bmp_info.bmiHeader.biPlanes   := 1;
  bmp_info.bmiHeader.biBitCount := 32;

  SetLength (buf, _bmp.Height * _bmp.Width * 4 );

  hbmp := _bmp.Handle;
  GetDIBits( _bmp.Canvas.Handle,
             hbmp, 0,
             _bmp.Height,
             @buf[0],
             bmp_info,
             DIB_RGB_COLORS
           );

  bmp_properties.dpiX := 0;
  bmp_properties.dpiY := 0;
  bmp_properties.pixelFormat.format    := DXGI_FORMAT_B8G8R8A8_UNORM;
  bmp_properties.pixelFormat.alphaMode := D2D1_ALPHA_MODE_IGNORE;

  RenderTarget.CreateBitmap(
    D2D1SizeU(_bmp.Width, _bmp.Height),
    @buf[0],
    4*_bmp.Width,
    bmp_properties,
    bmp
  );

  rct.Left   := _rect.Left   ;
  rct.Right  := _rect.Right  ;
  rct.Top    := _rect.Top    ;
  rct.Bottom := _rect.Bottom ;
  RenderTarget.DrawBitmap( bmp, @rct, 1);
end;

procedure TD2DCanvas.StretchBitmap(
  const _rect : TRect ;
  const _bmp  : TGIS_Pixels ;
  const _size : TPoint
);
var
  bmp : ID2D1Bitmap ;
  rct : TD2DRectF   ;
begin
  bmp := CreateBitmapPremultiplied( _bmp, _size ) ;
  try
    rct.Left   := _rect.Left   ;
    rct.Right  := _rect.Right  ;
    rct.Top    := _rect.Top    ;
    rct.Bottom := _rect.Bottom ;
    RenderTarget.DrawBitmap( bmp, @rct, 1);
  finally
    bmp := nil ;
  end;
end;

procedure TD2DCanvas.StretchBitmap(
  const _rect         : TRect ;
  const _bmp          : ID2D1Bitmap ;
  const _transparency : Integer
) ;
var
  rct : TD2DRectF   ;
begin
  rct.Left   := _rect.Left   ;
  rct.Right  := _rect.Right  ;
  rct.Top    := _rect.Top    ;
  rct.Bottom := _rect.Bottom ;
  RenderTarget.DrawBitmap( _bmp, @rct, _transparency/100);
end ;
{$ENDREGION 'TD2DCanvas'}

{$REGION 'TGIS_FrameworkUtils'}
class function TGIS_FrameworkUtils.ToPlatformColor(
  const _color : TGIS_Color
) : TColor ;
begin
  Result := VCLColor( _color ) ;
end;

class function TGIS_FrameworkUtils.FromPlatformColor(
  const _color : TColor
) : TGIS_Color ;
begin
  Result := GISColor( _color )
end;
{$ENDREGION 'TGIS_FrameworkUtils'}

{$REGION 'Public methods'}
procedure GisScaleImageList(
 const _in_list  : TCustomImageList ;
 const _out_list : TCustomImageList ;
 const _n        : Integer ;
 const _d        : Integer ;
 const _disabled : Boolean = False
) ;
var
  i       : Integer    ;
  x,y     : Integer    ;
  factor  : Integer    ;
  imglist : TCustomImageList ;
  bmp     : TBitmap    ;
  bmp2    : TBitmap    ;
  clr     : TColor     ;
  scan    : IntPtr     ;
begin
  factor := RoundS( _n / _d - 0.2 ) ; // prefer smaller image

  if factor <= 0.1 then exit ;

  imglist := TCustomImageList.Create( nil ) ;
  try
    imglist.Width  := _in_list.Width  * factor ;
    imglist.Height := _in_list.Height * factor ;

    if _disabled then
      clr := TStyleManager.ActiveStyle.GetStyleFontColor( sfButtonTextDisabled )
    else
      clr := TStyleManager.ActiveStyle.GetStyleFontColor( sfButtonTextNormal );

    for i:=0 to _in_list.Count -1 do begin
      bmp := TBitmap.Create ;
      try
        bmp.PixelFormat := pf24bit ;
        bmp.Width  := _in_list.Width  ;
        bmp.Height := _in_list.Height ;

        bmp.Canvas.Brush.Color := clFuchsia;
        bmp.Canvas.FillRect( Rect( 0, 0, bmp.Width, bmp.Height ) ) ;

        _in_list.GetBitmap( i, bmp ) ;

        bmp.PixelFormat := pf32bit ;

        for y := 0 to bmp.Height -1 do begin
          scan := IntPtr(bmp.ScanLine[y]) ;
          for x := 0 to bmp.Width -1 do begin
            if PInteger( scan + x*4 )^ = 0  then
              PInteger( scan + x*4 )^ :=  clr ;
          end;
        end;

        bmp2 := TBitmap.Create ;
        try
          bmp2.PixelFormat := pf24bit ;
          bmp2.Width  := imglist.Width  ;
          bmp2.Height := imglist.Height ;

          bmp2.Canvas.StretchDraw( bmp2.Canvas.ClipRect, bmp );

          imglist.AddMasked( bmp2, clFuchsia ) ;

        finally
          bmp2.Free ;
        end;

      finally
        bmp.Free ;
      end;
    end;

    _out_list.Clear ;
    _out_list.Width  := imglist.Width ;
    _out_list.Height := imglist.Height ;
    _out_list.AddImages( imglist );

  finally
    FreeObject( imglist ) ;
  end;

end;

procedure VCLMakeCompatibleBitmap(
  const _bmp : TBitmap
) ;
var
  i, j  : Integer ;
  prgbr : PRGBQuad;
begin
  for i := 0 to _bmp.Height-1 do
  begin
    prgbr := PRGBQuad(_bmp.ScanLine[i]);
    for j := 0 to _bmp.Width-1 do begin
      with prgbr^ do begin
        if ( rgbReserved = 0  )
           and
           (
             ( rgbRed      <> 0 ) or
             ( rgbGreen    <> 0 ) or
             ( rgbBlue     <> 0 )
           )
        then
          rgbReserved := 255 ;

        rgbRed   := (rgbRed   * 255) div $FF;
        rgbGreen := (rgbGreen * 255) div $FF;
        rgbBlue  := (rgbBlue  * 255) div $FF;
      end;

      Inc( prgbr );
    end ;
  end ;
end ;

procedure VCLCanvasDrawBitmap(
  const _canvas : TCanvas ;
  const _rect   : TRect ;
  const _bmp    : TBitmap
) ;
begin
  VCLMakeCompatibleBitmap( _bmp ) ;
  VCLCanvasDrawBitmapEx( _canvas, _rect, _bmp ) ;
end ;

procedure VCLCanvasDrawBitmapEx(
  const _canvas : TCanvas ;
  const _rect   : TRect ;
  const _bmp    : TBitmap
) ;
var
  bitmapHeader:  pBitmapInfo;
  bitmapImage :  Pointer;
  headerSize  :  DWORD;    // Use DWORD for D3-D5 compatibility
  imageSize   :  DWORD;
begin
  if _bmp.Transparent then
    _canvas.StretchDraw( _rect, _bmp )
  else begin
    GetDIBSizes(_bmp.Handle, headerSize, imageSize);
    GetMem(bitmapHeader, headerSize);
    GetMem(bitmapImage,  imageSize);
    TRY
      GetDIB(_bmp.Handle, _bmp.Palette, bitmapHeader^, bitmapImage^);
      StretchDIBits( _canvas.Handle,
                     _rect.Left, _rect.Top,     // Destination Origin
                     _rect.Right  - _rect.Left, // Destination Width
                     _rect.Bottom - _rect.Top,  // Destination Height
                     0, 0,                            // Source Origin
                     _bmp.Width, _bmp.Height,     // Source Width & Height
                     bitmapImage,
                     TBitmapInfo(bitmapHeader^),
                     DIB_RGB_COLORS,
                     SRCCOPY) ;
    finally
      FreeMem(bitmapHeader);
      FreeMem(bitmapImage)
    end ;
  end ;
end;

function ColorToBGR( const _color : TColor ) : Integer ;
begin
  Result := ColorToRGB( _color ) ;
end;

function VCLColor( const _color : TGIS_Color ) : TColor ;
begin
  if _color.ARGB = TGIS_Color.RenderColor.ARGB then
    Result := clNone
  else
    Result := _color.ToBGR and $00FFFFFF ;

  if Result = $00000000 then
    Result := $00010101 ; // to avoid bad interpretation of NULL
end ;

function GISColor( const _color : TColor ) : TGIS_Color ;
begin
  Result := TGIS_Color.FromBGR( ColorToBGR(_color) ) ;
end ;

function VCLFontStyle(
  const _style : TGIS_FontStyles
) : TFontStyles ;
begin
  Result := [];
  if TGIS_FontStyle.Bold in _style then
    Result := Result + [TFontStyle.fsBold] ;
  if TGIS_FontStyle.Italic in _style then
    Result := Result + [TFontStyle.fsItalic] ;
  if TGIS_FontStyle.Underline in _style then
    Result := Result + [TFontStyle.fsUnderline] ;
  if TGIS_FontStyle.StrikeOut in _style then
    Result := Result + [TFontStyle.fsStrikeOut] ;
end ;

function GISFontStyle(
  const _style : TFontStyles
) : TGIS_FontStyles ;
begin
  Result := [];
  if TFontStyle.fsBold      in _style then
    Result := Result + [TGIS_FontStyle.Bold     ] ;
  if TFontStyle.fsItalic    in _style then
    Result := Result + [TGIS_FontStyle.Italic   ] ;
  if TFontStyle.fsUnderline in _style then
    Result := Result + [TGIS_FontStyle.Underline] ;
  if TFontStyle.fsStrikeOut in _style then
    Result := Result + [TGIS_FontStyle.StrikeOut] ;
end ;

function ImitationColor( _color : TGIS_Color ) : TGIS_Color ;
var
  h, s, l : Single ;
begin
  h := _color.H ;
  s := _color.S ;
  l := _color.L ;
  l := l + (1- l)*(255-_color.A)/255 ;
  Result := TGIS_Color.FromHSL( h, s, l ) ;
end;

function VCLFont( const _font : TGIS_Font ) : TFont ;
begin
  Result := TFont.Create ;
  Result.Name  := _font.Name ;
  Result.Size  := _font.Size ;
  Result.Color := VCLColor( _font.Color ) ;
  Result.Style := VCLFontStyle( _font.Style ) ;
end ;

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
{$ENDREGION}


procedure EnsureFramework ;
begin
  FreeObject( NativeBitmapFactory ) ;
  FreeObject( FontHelper   ) ;
  FreeObject( TimerHelper  ) ;
  FreeObject( PrintPreviewHelper ) ;

  FontHelper  := TGIS_FontFactoryVCL.Create ;
  TimerHelper := TGIS_TimerFactoryVCL.Create ;
  PrintPreviewHelper := TGIS_PrintPreviewFactoryVCL.Create ;
end ;

initialization
  RegisterBitmapFactory( 'TGIS_BitmapFactoryVCL', TGIS_BitmapFactoryVCL ) ;
  EnsureFramework ;

finalization
  FreeObject( NativeBitmapFactory ) ;
  FreeObject( BitmapFactoryList ) ;
  FreeObject( FontHelper   ) ;
  FreeObject( TimerHelper  ) ;
  FreeObject( PrintPreviewHelper ) ;

//==================================== END =====================================
end .


