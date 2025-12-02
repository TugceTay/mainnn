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
  Gdi+ wrapper.
}

unit VCL.GisGdiPlus ;
{$HPPEMIT '#pragma link "VCL.GisGdiPlus"'}

interface

{$INCLUDE GisInclude.inc}

uses
  System.Types,
  Winapi.Windows,
  Winapi.GDIPAPI,
  VCL.Graphics,

  GisRtl,
  GisTypesUI;

// force proper header and bypass number of Delphi bugs

{$HPPEMIT OPENNAMESPACE}
{$HPPEMIT 'typedef int Status;'}
{$HPPEMIT 'typedef int MatrixOrder;'}
{$HPPEMIT 'typedef int Unit_;'}
{$HPPEMIT 'typedef int SmoothingMode;'}
{$HPPEMIT 'typedef int PixelOffsetMode;'}
{$HPPEMIT 'typedef int InterpolationMode;'}
{$HPPEMIT 'typedef int CompositingMode;'}
{$HPPEMIT 'typedef int CompositingQuality;'}
{$HPPEMIT CLOSENAMESPACE}

{$IFNDEF LEVEL_XE8_RTL}
  // there is no Winapi.GDIPAPI.hpp provided before XE8!
  // so we declared here few missed things directly
  // do not remove event commented out HPPEMPT lines!
  {$NOINCLUDE  Winapi.GDIPAPI}
  (*$HPPEMIT  'namespace Winapi  {*)
  (*$HPPEMIT  '  namespace Gdipapi  {*)
  (*$HPPEMIT  '    struct TGPPointF;'*)
  (*$HPPEMIT  '  }*)
  (*$HPPEMIT  '};*)
  (*$HPPEMIT  '#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VCL_GISGDIPLUS)'*)
  (*$HPPEMIT  '  using namespace Winapi::Gdipapi;'*)
  (*$HPPEMIT  '#endif'*)
{$ENDIF}

type
  /// <summary>
  ///   Type representing PixelFormat.
  /// </summary>
  TGIS_GdipPixelFormat = Integer ;

type
  /// <summary>
  ///   Type representing BitmapData.
  /// </summary>
  TGIS_GdipBitmapData = record

    /// <summary>
    ///   Bitmap width.
    /// </summary>
    Width       : UINT    ;

    /// <summary>
    ///   Bitmap height.
    /// </summary>
    Height      : UINT    ;

    /// <summary>
    ///   Stride width of the bitmap.
    /// </summary>
    Stride      : Integer ;

    /// <summary>
    ///   Pixel format of the bitmap.
    /// </summary>
    PixelFormat : TGIS_GdipPixelFormat ;

    /// <summary>
    ///   Address of the first pixel data of the bitmap.
    /// </summary>
    Scan0       : Pointer ;

    /// <summary>
    ///   Reserved. Do not use.
    /// </summary>
    Reserved    : UINT    ;
  end ;

type

  /// <summary>
  ///   Platform independent pen type.
  /// </summary>
  TGIS_PenType = (

    /// <summary>
    ///   Solid color pen.
    /// </summary>
    SolidColor       =  0,

    /// <summary>
    ///   Hatch fill pen.
    /// </summary>
    HatchFill        =  1,

    /// <summary>
    ///   Texture pen.
    /// </summary>
    TextureFill      =  2,

    /// <summary>
    ///   Path gradient pen.
    /// </summary>
    PathGradient     =  3,

    /// <summary>
    ///   Linear gradient pen.
    /// </summary>
    LinearGradient   =  4,

    /// <summary>
    ///   Unknown type pen.
    /// </summary>
    Unknown          = -1

  ) ;

type

  /// <summary>
  ///   Type representing RectangleF.
  /// </summary>
  TGIS_GdipRectF = packed record

    /// <summary>
    ///   X-coordinate of the upper-left corner.
    /// </summary>
    X      : Single ;

    /// <summary>
    ///   Y-coordinate of the upper-left corner.
    /// </summary>
    Y      : Single ;

    /// <summary>
    ///   Width of the rectangle
    /// </summary>
    Width  : Single ;

    /// <summary>
    ///   Height of the rectangle
    /// </summary>
    Height : Single ;
  end ;

  /// <summary>
  ///   Type representing Rect.
  /// </summary>
  TGIS_GdipRect = packed record

    /// <summary>
    ///   X-coordinate of the upper-left corner.
    /// </summary>
    X      : Integer ;

    /// <summary>
    ///   Y-coordinate of the upper-left corner.
    /// </summary>
    Y      : Integer ;

    /// <summary>
    ///   Width of the rectangle
    /// </summary>
    Width  : Integer ;

    /// <summary>
    ///   Height of the rectangle
    /// </summary>
    Height : Integer ;
  end ;

  /// <summary>
  ///   Type representing SizeF.
  /// </summary>
  TGIS_GdipSizeF = packed record

    /// <summary>
    ///   Horizontal component of the size
    /// </summary>
    Width  : Single ;

    /// <summary>
    ///   Vertical component of the size
    /// </summary>
    Height : Single ;
  end ;

  /// <summary>
  ///   Encapsulation of Gdi+ brush.
  /// </summary>
  TGIS_GdipBrush = class ( TGIS_ObjectDisposable )
    private

      /// <summary>
      ///   Checks status of the last called function.
      /// </summary>
      function  setStatus      ( _status : GPSTATUS
                               ) : GPSTATUS ;
    protected

      /// <summary>
      ///   Status of last called Gdi+ function.
      /// </summary>
      lastResult  : Status ;

      /// <summary>
      ///   Native Gdi+ brush.
      /// </summary>
      nativeBrush : GPBRUSH ;
    protected

      /// <summary>
      ///   Release the instance.
      /// </summary>
      procedure doDestroy  ; override;
  end ;

  /// <summary>
  ///   Encapsulation of Gdi+ solid brush.
  /// </summary>
  TGIS_GdipSolidBrush = class ( TGIS_GdipBrush )
    public

      /// <summary>
      ///   Create a solid brush.
      /// </summary>
      /// <param name="_color">
      ///   brush color
      /// </param>
      constructor Create   ( _color      : TGIS_Color
                           ) ;
  end ;

  /// <summary>
  ///   Encapsulation of Gdi+ hatched brush.
  /// </summary>
  TGIS_GdipHatchBrush = class ( TGIS_GdipBrush )
    public

      /// <summary>
      ///   Create a hatch brush.
      /// </summary>
      /// <param name="_pattern">
      ///   brush pattern
      /// </param>
      /// <param name="_foreColor">
      ///   pattern color
      /// </param>
      constructor Create   ( _pattern    : TGIS_BrushStyle ;
                             _foreColor  : TGIS_Color
                           ) ;
  end ;

  /// <summary>
  ///   Encapsulation of Gdi+ texture brush.
  /// </summary>
  TGIS_GdipTextureBrush = class ( TGIS_GdipBrush )
    public

      /// <summary>
      ///   Create a texture brush.
      /// </summary>
      /// <param name="_bitmap">
      ///   brush bitmap
      /// </param>
      constructor Create       ( _bitmap     : TGIS_Bitmap
                               ) ;

      /// <summary>
      ///   Resets the transformation.
      /// </summary>
      procedure ResetTransform ;

      /// <summary>
      ///   Translates the local geometric transformation of this texture brush object
      ///   by the specified dimensions.
      /// </summary>
      /// <param name="_x">
      ///   dimension by which to translate the transformation in the x direction
      /// </param>
      /// <param name="_y">
      ///   dimension by which to translate the transformation in the y direction
      /// </param>
      procedure TranslateTransform
                               ( _x : Single ;
                                 _y : Single
                               ) ;

      /// <summary>
      ///   Scales the local geometric transformation of this texture brush object
      ///   by the specified amounts in the specified order.
      /// </summary>
      /// <param name="_x">
      ///   amount by which to scale the transformation in the x direction
      /// </param>
      /// <param name="_y">
      ///   amount by which to scale the transformation in the y direction
      /// </param>
      procedure ScaleTransform ( _x : Single ;
                                 _y : Single
                               ) ;
  end ;

  /// <summary>
  ///   Encapsulation of Gdi+ pen.
  /// </summary>
  TGIS_GdipPen = class ( TGIS_ObjectDisposable )
    private
      FWidth : Single ;
      /// <summary>
      ///   Checks status of the last called function.
      /// </summary>
      function  setStatus      ( _status : GPSTATUS
                               ) : GPSTATUS ;
    protected

      /// <summary>
      ///   Status of last called Gdi+ function.
      /// </summary>
      lastResult : GPSTATUS ;

      /// <summary>
      ///   Native Gdi+ pen.
      /// </summary>
      nativePen  : GPPEN ;

      /// <summary>
      ///   Brush used to render with the pen.
      /// </summary>
      FBrush     : TGIS_GdipBrush ;

    protected

      function  fget_DashStyle : TGIS_PenStyle ;
      procedure fset_DashStyle ( _value : TGIS_PenStyle
                               ) ;
      procedure fset_LineJoin  ( _value : TGIS_LineJoin
                               ) ;
      procedure fset_LineCap   ( _value : TGIS_LineCap
                               ) ;
      procedure fset_LineDash  ( _value : TGIS_DashArray
                               ) ;
      function  fget_PenType   : TGIS_PenType ;
    protected

      /// <summary>
      ///   Release the instance.
      /// </summary>
      procedure doDestroy      ; override;
    public

      /// <summary>
      ///   Create a solid pen.
      /// </summary>
      /// <param name="_color">
      ///   pen color
      /// </param>
      /// <param name="_width">
      ///   pen width
      /// </param>
      constructor Create       ( _color : TGIS_Color ;
                                 _width : Single = 1.0
                               ) ; overload;

      /// <summary>
      ///   Create a texture pen.
      /// </summary>
      /// <param name="_brush">
      ///   brush that fills surface drawn by pen
      /// </param>
      /// <param name="_width">
      ///   pen width
      /// </param>
      constructor Create       ( _brush : TGIS_GdipBrush ;
                                 _width : Single = 1.0
                               ) ; overload;

      /// <summary>
      ///   Set a new transformation for a texture pen.
      /// </summary>
      /// <param name="_originX">
      ///   dimension by which to translate the transformation in the x direction
      /// </param>
      /// <param name="_originY">
      ///   dimension by which to translate the transformation in the y direction
      /// </param>
      /// <param name="_scaleX">
      ///   amount by which to scale the transformation in the x direction
      /// </param>
      /// <param name="_scaleY">
      ///   amount by which to scale the transformation in the y direction
      /// </param>
      procedure SetBrushFillTransform(
                                 const _originX : Integer ;
                                 const _originY : Integer ;
                                 const _scaleX  : Double  ;
                                 const _scaleY  : Double
                               ) ;
    public

      /// <summary>
      ///   Style used for dashed lines.
      /// </summary>
      property DashStyle : TGIS_PenStyle
                           read  fget_DashStyle
                           write fset_DashStyle ;

      /// <summary>
      ///   Style used for joining lines.
      /// </summary>
      property LineJoin  : TGIS_LineJoin
                           write fset_LineJoin ;

      /// <summary>
      ///   Cap style used for lines.
      /// </summary>
      property LineCap  : TGIS_LineCap
                          write fset_LineCap ;

      /// <summary>
      ///   Dash array used for dashed lines.
      /// </summary>
      property LineDash : TGIS_DashArray
                          write fset_LineDash ;

      /// <summary>
      ///   texture brush.
      /// </summary>
      property PenType  : TGIS_PenType
                          read  fget_PenType ;
  end ;

  /// <summary>
  ///   Encapsulation of Gdi+ font family.
  /// </summary>
  TGIS_GdipFontFamily = class ( TGIS_ObjectDisposable )
    private

      /// <summary>
      ///   Checks status of the last called function.
      /// </summary>
      function  setStatus      ( _status     : GPSTATUS
                               ) : GPSTATUS ;
    protected

      /// <summary>
      ///   Status of last called Gdi+ function.
      /// </summary>
      lastResult   : GPSTATUS ;

      /// <summary>
      ///   Native Gdi+ font family.
      /// </summary>
      nativeFamily : GPFONTFAMILY ;
    protected

      /// <summary>
      ///   Release the instance.
      /// </summary>
      procedure doDestroy      ; override;
    public

      /// <summary>
      ///   Create a font family.
      /// </summary>
      /// <param name="_familyName">
      ///   name of the font family
      /// </param>
      constructor Create       ( _familyName : WideString
                               ) ; reintroduce ; overload;

      /// <summary>
      ///   Get a generic sans serif font family.
      /// </summary>
      /// <returns>
      ///   Object that represents a generic sans serif font.
      /// </returns>
      class function GenericSansSerif
                         : TGIS_GdipFontFamily ;
                           {$IFDEF GIS_STATIC}static;{$ENDIF}
  end ;

  /// <summary>
  ///   Encapsulation of Gdi+ font.
  /// </summary>
  TGIS_GdipFont = class ( TGIS_ObjectDisposable )
    private

      /// <summary>
      ///   If True, font family has been already created.
      /// </summary>
      fontFamily_created : Boolean ;

      /// <summary>
      ///   Font style.
      /// </summary>
      fontStyleSet : FONTSTYLE ;
    protected

      /// <summary>
      ///   Status of last called Gdi+ function.
      /// </summary>
      lastResult : GPSTATUS ;

      /// <summary>
      ///   Native Gdi+ font.
      /// </summary>
      nativeFont : GPFONT ;

      /// <summary>
      ///   Font family.
      /// </summary>
      fontFamily : TGIS_GdipFontFamily ;
    protected

      function fget_Style      : TGIS_FontStyles ;

    protected

      /// <summary>
      ///   Release the instance.
      /// </summary>
      procedure doDestroy      ; override;
    public

      /// <summary>
      ///   Create a font.
      /// </summary>
      /// <param name="_familyName">
      ///   name of the font family
      /// </param>
      /// <param name="_emSize">
      ///   em-size in points
      /// </param>
      /// <param name="_style">
      ///   font style
      /// </param>
      constructor Create       ( _familyName : WideString ;
                                 _emSize     : Single ;
                                 _style      : TGIS_FontStyles
                                               {$IFNDEF GENDOC} = [] {$ENDIF}
                               ) ;

      /// <summary>
      ///   Get status of the last operation.
      /// </summary>
      /// <returns>
      ///   Status.
      /// </returns>
      function  GetLastStatus  : GPSTATUS ;

    public

      /// <summary>
      ///   Get font style.
      /// </summary>
      property Style           : TGIS_FontStyles read  fget_Style ;
  end ;

  /// <summary>
  ///   Encapsulation of Gdi+ string format.
  /// </summary>
  TGIS_GdipStringFormat = class ( TGIS_ObjectDisposable )
    private

      /// <summary>
      ///   Checks status of the last called function.
      /// </summary>
      function  setStatus      ( _status     : GPSTATUS
                               ) : GPSTATUS ;
    protected

      /// <summary>
      ///   Status of last called Gdi+ function.
      /// </summary>
      lastResult   : GPSTATUS ;

      /// <summary>
      ///   Native Gdi+ string format.
      /// </summary>
      nativeFormat : GPSTRINGFORMAT ;
    protected
      function  fget_FormatFlags : Integer ;
      procedure fset_FormatFlags ( _value : Integer
                                 ) ;
    protected

      /// <summary>
      ///   Release the instance.
      /// </summary>
      procedure doDestroy      ; override;
    public

      /// <summary>
      ///   Create a string format.
      /// </summary>
      /// <param name="_formatFlags">
      ///   enumerate display and layout information
      /// </param>
      /// <param name="_language">
      ///   language of text
      /// </param>
      constructor Create       ( _formatFlags : Integer = 0 ;
                                 _language    : LANGID = LANG_NEUTRAL
                               ) ; overload;

      /// <summary>
      ///   Create a string format.
      /// </summary>
      /// <param name="_format">
      ///   object from which to initialize the new object
      /// </param>
      constructor Create       ( _format      : TGIS_GdipStringFormat
                               ) ; overload;

      /// <summary>
      ///   Get a generic typographic string format.
      /// </summary>
      /// <returns>
      ///   Object that represents a generic typographic string format.
      /// </returns>
      class function GenericTypographic
                               : TGIS_GdipStringFormat ;

    public

      /// <summary>
      ///   Enumerate display and layout information.
      /// </summary>
      property FormatFlags : Integer read  fget_FormatFlags
                                     write fset_FormatFlags ;
  end ;

  /// <summary>
  ///   Encapsulation of Gdi+ image attributes.
  /// </summary>
  TGIS_GdipImageAttributes = class ( TGIS_ObjectDisposable )
    private

      /// <summary>
      ///   Checks status of the last called function.
      /// </summary>
      function  setStatus      ( _status     : GPSTATUS
                               ) : GPSTATUS ;
    protected

      /// <summary>
      ///   Status of last called Gdi+ function.
      /// </summary>
      lastResult : GPSTATUS ;

      /// <summary>
      ///   Native Gdi+ image attributes.
      /// </summary>
      nativeImageAttr : GPIMAGEATTRIBUTES ;
    public

      /// <summary>
      ///   Create an image attributes structure.
      /// </summary>
      constructor Create       ; reintroduce; overload;

      /// <summary>
      ///   Release the instance.
      /// </summary>
      procedure doDestroy      ; override;

      /// <summary>
      ///   Set the range of colors can be made transparent.
      /// </summary>
      /// <param name="_colorLow">
      ///   low color key value
      /// </param>
      /// <param name="_colorHigh">
      ///   high color key value
      /// </param>
      procedure SetColorKey    ( _colorLow       : TGIS_Color ;
                                 _colorHigh      : TGIS_Color
                               ) ;
  end ;

  /// <summary>
  ///   Encapsulation of Gdi+ image.
  /// </summary>
  TGIS_GdipImage = class ( TGIS_ObjectDisposable )
    protected

      /// <summary>
      ///   Status of last called Gdi+ function.
      /// </summary>
      lastResult  : GPSTATUS ;

      /// <summary>
      ///   Native Gdi+ image.
      /// </summary>
      nativeImage : GPIMAGE ;

      /// <summary>
      ///   Checks status of the last called function.
      /// </summary>
      /// <param name="_status">
      ///   status value to check
      /// </param>
      /// <returns>
      ///   Status value.
      /// </returns>
      function  setStatus           ( _status         : GPSTATUS
                                    ) : GPSTATUS ;

    protected
      function  fget_PixelFormat      : TGIS_GdipPixelFormat ;

    protected

      /// <summary>
      ///   Release the instance.
      /// </summary>
      procedure doDestroy        ; override;
    public

      /// <summary>
      ///   Pixel format of the image.
      /// </summary>
      property  PixelFormat         : TGIS_GdipPixelFormat
                                      read  fget_PixelFormat ;
  end ;

  /// <summary>
  ///   Encapsulation of Gdi+ bitmap.
  /// </summary>
  TGIS_GdipBitmap = class( TGIS_GdipImage )
    public

      /// <summary>
      ///   Create a bitmap.
      /// </summary>
      /// <param name="_hbm">
      ///   bitmap handle from which to initialize the new bitmap
      /// </param>
      /// <param name="_hpal">
      ///   bitmap palette
      /// </param>
      constructor Create         ( _hbm            : HBITMAP     ;
                                   _hpal           : HPALETTE
                                 ) ; overload;

      /// <summary>
      ///   Create a bitmap.
      /// </summary>
      /// <param name="_width">
      ///   bitmap width
      /// </param>
      /// <param name="_height">
      ///   bitmap height
      /// </param>
      /// <param name="_stride">
      ///   stride width
      /// </param>
      /// <param name="_format">
      ///   pixel format
      /// </param>
      /// <param name="_scan0">
      ///   pointer to bitmap data
      /// </param>
      constructor Create         ( _width          : Integer     ;
                                   _height         : Integer     ;
                                   _stride         : Integer     ;
                                   _format         : PixelFormat ;
                                   _scan0          : PBYTE
                                  ) ; overload;

      /// <summary>
      ///   Create a bitmap.
      /// </summary>
      /// <param name="_bmp">
      ///   source bitmap
      /// </param>
      constructor Create          ( const _bmp      : TGIS_Bitmap
                                  ) ; overload;


      /// <summary>
      ///   Lock a bitmap into system memory.
      /// </summary>
      /// <param name="_rect">
      ///   portion of the bitmap to lock
      /// </param>
      /// <param name="_flags">
      ///   access level (read/write) for the bitmap
      /// </param>
      /// <param name="_format">
      ///   pixel format of the bitmap
      /// </param>
      /// <returns>
      ///   Bitmap data object.
      /// </returns>
      function  LockBits         ( _rect           : TRect       ;
                                   _flags          : UINT        ;
                                   _format         : TGIS_GdipPixelFormat
                                 ) : TGIS_GdipBitmapData ;

      /// <summary>
      ///   Unlock a bitmap from system memory.
      /// </summary>
      /// <param name="_lockedBitmapData">
      ///   information about the lock operation
      /// </param>
      procedure UnlockBits       ( var _lockedBitmapData
                                                   : TGIS_GdipBitmapData
                                 ) ;
  end ;

  /// <summary>
  ///   Encapsulation of Gdi+ graphics.
  /// </summary>
  TGIS_GdipGraphics = class ( TGIS_ObjectDisposable )
    private

      /// <summary>
      ///   Checks status of the last called function.
      /// </summary>
      function  setStatus        ( _status         : GPSTATUS
                                 ) : GPSTATUS ;
    protected

      /// <summary>
      ///   Status of last called Gdi+ function.
      /// </summary>
      lastResult     : GPSTATUS ;

      /// <summary>
      ///   Native Gdi+ image.
      /// </summary>
      nativeGraphics : GPGRAPHICS ;
    protected

      /// <summary>
      ///   Release the instance.
      /// </summary>
      procedure doDestroy        ; override;


    public

      /// <summary>
      ///   Create a graphics from the specified handle to a device context.
      /// </summary>
      /// <param name="_hdc">
      ///   device context handle
      /// </param>
      constructor Create         ( _hdc            : HDC
                                 ) ;

      /// <summary>
      ///   Set rendering origin for hatched brush and patterns
      /// </summary>
      /// <param name="_x">
      ///   x origin
      /// </param>
      /// <param name="_y">
      ///   y origin
      /// </param>
      procedure SetRenderingOrigin( _x             : Integer        ;
                                    _y             : Integer
                                  ) ;

      /// <summary>
      ///   Draw an ellipse.
      /// </summary>
      /// <param name="_pen">
      ///   pen for the ellipse
      /// </param>
      /// <param name="_x">
      ///   x coordinate of the upper-left corner of the bounding rectangle
      /// </param>
      /// <param name="_y">
      ///   y coordinate of the upper-left corner of the bounding rectangle
      /// </param>
      /// <param name="_width">
      ///   width of the bounding rectangle
      /// </param>
      /// <param name="_height">
      ///   height of the bounding rectangle
      /// </param>
      procedure DrawEllipse      ( _pen            : TGIS_GdipPen   ;
                                   _x              : Integer        ;
                                   _y              : Integer        ;
                                   _width          : Integer        ;
                                   _height         : Integer
                                 ) ;

      /// <summary>
      ///   Draw an image.
      /// </summary>
      /// <param name="_image">
      ///   image to draw
      /// </param>
      /// <param name="_x">
      ///   x coordinate of the upper-left corner of the drawn image
      /// </param>
      /// <param name="_y">
      ///   y coordinate of the upper-left corner of the drawn image
      /// </param>
      procedure DrawImage        ( _image          : TGIS_GdipImage ;
                                   _x              : Integer        ;
                                   _y              : Integer
                                 ) ; overload;

      /// <summary>
      ///   Draw an image.
      /// </summary>
      /// <param name="_image">
      ///   image to draw
      /// </param>
      /// <param name="_destRect">
      ///   location and size of the drawn image
      /// </param>
      /// <param name="_srcX">
      ///   x coordinate of the upper-left corner of the portion of the source image
      /// </param>
      /// <param name="_srcY">
      ///   y coordinate of the upper-left corner of the portion of the source image
      /// </param>
      /// <param name="_srcWidth">
      ///   width of the portion of the source image
      /// </param>
      /// <param name="_srcHeight">
      ///   height of the portion of the source image
      /// </param>
      /// <param name="_imageAttr">
      ///   image attributes for the image
      /// </param>
      procedure DrawImage        ( _image          : TGIS_GdipImage ;
                                   _destRect       : TRect          ;
                                   _srcX           : Integer        ;
                                   _srcY           : Integer        ;
                                   _srcWidth       : Integer        ;
                                   _srcHeight      : Integer        ;
                                   _imageAttr      : TGIS_GdipImageAttributes
                                                     = nil
                                 ) ; overload;

      /// <summary>
      ///   Draw a series of line segments.
      /// </summary>
      /// <param name="_pen">
      ///   pen for the line segments
      /// </param>
      /// <param name="_points">
      ///   points to connect
      /// </param>
      /// <param name="_count">
      ///   count of points
      /// </param>
      procedure DrawLines        (       _pen      : TGIS_GdipPen   ;
                                   const _points   : array of TPoint ;
                                         _count    : Integer
                                 ) ; overload;

      /// <summary>
      ///   Draw a series of line segments.
      /// </summary>
      /// <param name="_pen">
      ///   pen for the line segments
      /// </param>
      /// <param name="_points">
      ///   points to connect
      /// </param>
      /// <param name="_count">
      ///   count of points
      /// </param>
      procedure DrawLines        (       _pen      : TGIS_GdipPen   ;
                                   const _points   : array of TGPPointF ;
                                         _count    : Integer
                                 ) ; overload;

      /// <summary>
      ///   Draw a pie.
      /// </summary>
      /// <param name="_pen">
      ///   pen for the pie
      /// </param>
      /// <param name="_x">
      ///   x-coordinate of the upper-left corner of the bounding rectangle
      /// </param>
      /// <param name="_y">
      ///   y-coordinate of the upper-left corner of the bounding rectangle
      /// </param>
      /// <param name="_width">
      ///   width of the bounding rectangle
      /// </param>
      /// <param name="_height">
      ///   height of the bounding rectangle
      /// </param>
      /// <param name="_startAngle">
      ///   angle measured in degrees clockwise from the x-axis to the first side of the pie
      /// </param>
      /// <param name="_sweepAngle">
      ///   angle measured in degrees clockwise from the _startAngle to the second side of the pie
      /// </param>
      procedure DrawPie          ( _pen            : TGIS_GdipPen  ;
                                   _x              : Integer       ;
                                   _y              : Integer       ;
                                   _width          : Integer       ;
                                   _height         : Integer       ;
                                   _startAngle     : Integer       ;
                                   _sweepAngle     : Integer
                                 ) ;
      /// <summary>
      ///   Draw an arc.
      /// </summary>
      /// <param name="_pen">
      ///   pen for the arc
      /// </param>
      /// <param name="_x">
      ///   x-coordinate of the upper-left corner of the bounding rectangle
      /// </param>
      /// <param name="_y">
      ///   y-coordinate of the upper-left corner of the bounding rectangle
      /// </param>
      /// <param name="_width">
      ///   width of the bounding rectangle
      /// </param>
      /// <param name="_height">
      ///   height of the bounding rectangle
      /// </param>
      /// <param name="_startAngle">
      ///   angle measured in degrees clockwise from the x-axis to the first side of the arc
      /// </param>
      /// <param name="_sweepAngle">
      ///   angle measured in degrees clockwise from the _startAngle to the second side of the arc
      /// </param>
      procedure DrawArc          ( _pen            : TGIS_GdipPen  ;
                                   _x              : Integer       ;
                                   _y              : Integer       ;
                                   _width          : Integer       ;
                                   _height         : Integer       ;
                                   _startAngle     : Single       ;
                                   _sweepAngle     : Single
                                 ) ;
      /// <summary>
      ///   Draw a polygon.
      /// </summary>
      /// <param name="_pen">
      ///   pen for the polygon
      /// </param>
      /// <param name="_points">
      ///   vertices of the polygon
      /// </param>
      procedure DrawPolygon      (       _pen      : TGIS_GdipPen   ;
                                   const _points   : array of TPoint
                                 ) ;

      /// <summary>
      ///   Draw a rectangle.
      /// </summary>
      /// <param name="_pen">
      ///   pen for the polygon
      /// </param>
      /// <param name="_rect">
      ///   vertices of the rectangle
      /// </param>
      procedure DrawRectangle    ( _pen            : TGIS_GdipPen   ;
                                   _rect           : TRect
                                 ) ; overload;

      /// <summary>
      ///   Draw a rectangle.
      /// </summary>
      /// <param name="_pen">
      ///   pen for the polygon
      /// </param>
      /// <param name="_left">
      ///   x coordinate of the upper corner of the rectangle
      /// </param>
      /// <param name="_top">
      ///   y coordinate of the upper corner of the rectangle
      /// </param>
      /// <param name="_width">
      ///   width of the rectangle
      /// </param>
      /// <param name="_height">
      ///   height of the rectangle
      /// </param>
      procedure DrawRectangle    ( _pen            : TGIS_GdipPen   ;
                                   _left           : Integer        ;
                                   _top            : Integer        ;
                                   _width          : Integer        ;
                                   _height         : Integer
                                 ) ; overload;

      /// <summary>
      ///   Draw a string.
      /// </summary>
      /// <param name="_s">
      ///   text to draw
      /// </param>
      /// <param name="_font">
      ///   text format of the text
      /// </param>
      /// <param name="_brush">
      ///   color and texture of the text
      /// </param>
      /// <param name="_layoutRect">
      ///   location of the drawn text
      /// </param>
      /// <param name="_format">
      ///   formatting attributes of the drawn text
      /// </param>
      procedure DrawString       ( _s              : WideString     ;
                                   _font           : TGIS_GdipFont  ;
                                   _brush          : TGIS_GdipBrush ;
                                   _layoutRect     : TRect          ;
                                   _format         : TGIS_GdipStringFormat
                                 ) ; overload;

      /// <summary>
      ///   Draw a string.
      /// </summary>
      /// <param name="_s">
      ///   text to draw
      /// </param>
      /// <param name="_font">
      ///   text format of the text
      /// </param>
      /// <param name="_brush">
      ///   color and texture of the text
      /// </param>
      /// <param name="_x">
      ///   x coordinate of the upper-left corner of the drawn text
      /// </param>
      /// <param name="_y">
      ///   y coordinate of the upper-left corner of the drawn text
      /// </param>
      /// <param name="_format">
      ///   formatting attributes of the drawn text
      /// </param>
      procedure DrawString       ( _s              : WideString     ;
                                   _font           : TGIS_GdipFont  ;
                                   _brush          : TGIS_GdipBrush ;
                                   _x              : Single         ;
                                   _y              : Single         ;
                                   _format         : TGIS_GdipStringFormat
                                 ) ; overload;

      /// <summary>
      ///   Fill an ellipse.
      /// </summary>
      /// <param name="_brush">
      ///   characteristics of the fill
      /// </param>
      /// <param name="_x">
      ///   x coordinate of the upper-left corner of the bounding rectangle
      /// </param>
      /// <param name="_y">
      ///   y coordinate of the upper-left corner of the bounding rectangle
      /// </param>
      /// <param name="_width">
      ///   width of the bounding rectangle
      /// </param>
      /// <param name="_height">
      ///   height of the bounding rectangle
      /// </param>
      procedure FillEllipse      ( _brush          : TGIS_GdipBrush ;
                                   _x              : Integer        ;
                                   _y              : Integer        ;
                                   _width          : Integer        ;
                                   _height         : Integer
                                 ) ;

      /// <summary>
      ///   Fill a pie.
      /// </summary>
      /// <param name="_brush">
      ///   brush for the pie
      /// </param>
      /// <param name="_x">
      ///   x-coordinate of the upper-left corner of the bounding rectangle
      /// </param>
      /// <param name="_y">
      ///   y-coordinate of the upper-left corner of the bounding rectangle
      /// </param>
      /// <param name="_width">
      ///   width of the bounding rectangle
      /// </param>
      /// <param name="_height">
      ///   height of the bounding rectangle
      /// </param>
      /// <param name="_startAngle">
      ///   angle measured in degrees clockwise from the x-axis to the first side of the pie
      /// </param>
      /// <param name="_sweepAngle">
      ///   angle measured in degrees clockwise from the _startAngle to the second side of the pie
      /// </param>
      procedure FillPie          ( _brush          : TGIS_GdipBrush ;
                                   _x              : Integer       ;
                                   _y              : Integer       ;
                                   _width          : Integer       ;
                                   _height         : Integer       ;
                                   _startAngle     : Integer       ;
                                   _sweepAngle     : Integer
                                 ) ;

      /// <summary>
      ///   Fill a polygon.
      /// </summary>
      /// <param name="_brush">
      ///   characteristics of the fill
      /// </param>
      /// <param name="_points">
      ///   vertices of the polygon
      /// </param>
      procedure FillPolygon      (       _brush    : TGIS_GdipBrush ;
                                   const _points   : array of TPoint
                                 ) ; overload;

      /// <summary>
      ///   Fill a polygon.
      /// </summary>
      /// <param name="_brush">
      ///   characteristics of the fill
      /// </param>
      /// <param name="_points">
      ///   vertices of the polygon
      /// </param>
      procedure FillPolygon      (       _brush    : TGIS_GdipBrush ;
                                   const _points   : array of TGPPointF
                                 ) ; overload;

      /// <summary>
      ///   Fill a rectangle.
      /// </summary>
      /// <param name="_brush">
      ///   characteristics of the fill
      /// </param>
      /// <param name="_rect">
      ///   vertices of the rectangle
      /// </param>
      procedure FillRectangle    ( _brush          : TGIS_GdipBrush ;
                                   _rect           : TRect
                                 ) ; overload;

      /// <summary>
      ///   Fill a rectangle.
      /// </summary>
      /// <param name="_brush">
      ///   characteristics of the fill
      /// </param>
      /// <param name="_left">
      ///   x coordinate of the upper corner of the rectangle
      /// </param>
      /// <param name="_top">
      ///   y coordinate of the upper corner of the rectangle
      /// </param>
      /// <param name="_width">
      ///   width of the rectangle
      /// </param>
      /// <param name="_height">
      ///   height of the rectangle
      /// </param>
      procedure FillRectangle    ( _brush          : TGIS_GdipBrush ;
                                   _left           : Integer        ;
                                   _top            : Integer        ;
                                   _width          : Integer        ;
                                   _height         : Integer
                                 ) ; overload;

      /// <summary>
      ///   Get the handle to the device context associated with this graphics.
      /// </summary>
      /// <returns>
      ///   Handle to the device context associated with this graphics.
      /// </returns>
      function  GetHdc           : HDC ;

      /// <summary>
      ///   Measure the specified string.
      /// </summary>
      /// <param name="_text">
      ///   string to measure
      /// </param>
      /// <param name="_font">
      ///   font that defines text format
      /// </param>
      /// <param name="_width">
      ///   maximum width of the string
      /// </param>
      /// <param name="_stringFormat">
      ///   formatting information
      /// </param>
      /// <returns>
      ///   Size of the string.
      /// </returns>
      function  MeasureString    ( _text           : WideString     ;
                                   _font           : TGIS_GdipFont  ;
                                   _width          : Integer        ;
                                   _stringFormat   : TGIS_GdipStringFormat
                                 ) : TGIS_GdipSizeF ;

      /// <summary>
      ///   Release a device context handle.
      /// </summary>
      /// <param name="_hdc">
      ///   handle to a device context
      /// </param>
      procedure ReleaseHdc       ( _hdc            : HDC
                                 ) ;

      /// <summary>
      ///   Reset the world transformation.
      /// </summary>
      procedure ResetTransform   ;

      /// <summary>
      ///   Apply the specified rotation to the world transformation.
      /// </summary>
      /// <param name="_angle">
      ///   angle of rotation
      /// </param>
      /// <param name="_order">
      ///   whether the rotation is appended or prepended to the transformation
      /// </param>
      procedure RotateTransform  ( _angle          : Single         ;
                                   _order          : GPMATRIXORDER
                                                     = MatrixOrderPrepend
                                 ) ;

      /// <summary>
      ///   Change the origin to the world transformation.
      /// </summary>
      /// <param name="_dx">
      ///   x coordinate of the translation
      /// </param>
      /// <param name="_dy">
      ///   y coordinate of the translation
      /// </param>
      /// <param name="_order">
      ///   whether the rotation is appended or prepended to the transformation
      /// </param>
      procedure TranslateTransform
                                 ( _dx             : Single         ;
                                   _dy             : Single         ;
                                   _order          : GPMATRIXORDER
                                                     = MatrixOrderPrepend
                                 ) ;

      /// <summary>
      ///   Set the unit of measure esed for page coordinates.
      /// </summary>
      /// <param name="_unit">
      ///   unit value
      /// </param>
      procedure SetPageUnit      ( _unit           : TUnit
                                 ) ;

      /// <summary>
      ///   Set the rendering quality.
      /// </summary>
      /// <param name="_mode">
      ///   smoothing mode value
      /// </param>
      procedure SetSmoothingMode ( _mode           : TSmoothingMode
                                 ) ;

      /// <summary>
      ///   Set a value specifying how pixels are offset during rendering.
      /// </summary>
      /// <param name="_mode">
      ///   pixel offset mode value
      /// </param>
      procedure SetPixelOffsetMode
                                 ( _mode           : TPixelOffsetMode
                                 ) ;

      /// <summary>
      ///   Set the interpolation mode.
      /// </summary>
      /// <param name="_mode">
      ///   interpolation mode value
      /// </param>
      procedure SetInterpolationMode
                                 ( _mode           : TInterpolationMode
                                 ) ;

      /// <summary>
      ///   Set the gamma correction value for rendering text.
      /// </summary>
      /// <param name="_contrast">
      ///   gamma correction value
      /// </param>
      procedure SetTextContrast  ( _contrast       : Cardinal
                                 ) ;

      /// <summary>
      ///   Set a value that specifies how composited images are drawn.
      /// </summary>
      /// <param name="_mode">
      ///   compositing mode value
      /// </param>
      procedure SetCompositingMode
                                 ( _mode           : TCompositingMode
                                 ) ;

      /// <summary>
      ///   Set the rendering quality of composited images.
      /// </summary>
      /// <param name="_quality">
      ///   compositing quality value
      /// </param>
      procedure SetCompositingQuality
                                 ( _quality        : TCompositingQuality
                                 ) ;

    public
      /// <summary>
      ///   Native Gdi+ context.
      /// </summary>
      property Native  : GPGRAPHICS  read nativeGraphics ;

  end ;


//##############################################################################

implementation

var
   GenericSansSerifFontFamily     : TGIS_GdipFontFamily   = nil ;
   GenericTypographicStringFormat : TGIS_GdipStringFormat = nil ;

   StartupInput   : GdiplusStartupInput ;
   StartupOutput  : GdiplusStartupOutput ;
   GdiPlusToken   : {$IFDEF LEVEL_RX104_RTL} NativeUInt {$ELSE} ULONG {$ENDIF} ;

  function GDIPColor(
    const _color : TGIS_Color
  ) : Cardinal ; inline ;
  begin
    if _color.ARGB = _color.RenderColor.ARGB then
      Result := TGIS_Color.Black.ARGB
    else
      Result := _color.ARGB
  end;


  function ToGdiPlusRect(
    _rect : TRect
  ) : TGIS_GdipRect ;
  begin
    Result.X := _rect.Left ;
    Result.Y := _rect.Top ;
    Result.Width  := _rect.Right - _rect.Left ;
    Result.Height := _rect.Bottom - _rect.Top ;
  end ;

  function ToGdiPlusRectF(
    _rect : TRect
  ) : TGIS_GdipRectF ;
  begin
    Result.X := _rect.Left ;
    Result.Y := _rect.Top ;
    Result.Width  := _rect.Right - _rect.Left ;
    Result.Height := _rect.Bottom - _rect.Top ;
  end ;

  function GetShortStatusMessage(
    _status : GPSTATUS
  ) : string ;
  begin
    case _status of
      Ok                        : Result := 'Ok' ;
      GenericError              : Result := 'GenericError' ;
      InvalidParameter          : Result := 'InvalidParameter' ;
      OutOfMemory               : Result := 'OutOfMemory' ;
      ObjectBusy                : Result := 'ObjectBusy' ;
      InsufficientBuffer        : Result := 'InsufficientBuffer' ;
      NotImplemented            : Result := 'NotImplemented' ;
      Win32Error                : Result := 'Win32Error' ;
      WrongState                : Result := 'WrongState' ;
      Aborted                   : Result := 'Aborted' ;
      FileNotFound              : Result := 'FileNotFound' ;
      ValueOverflow             : Result := 'ValueOverflow' ;
      AccessDenied              : Result := 'AccessDenied' ;
      UnknownImageFormat        : Result := 'UnknownImageFormat' ;
      FontFamilyNotFound        : Result := 'FontFamilyNotFound' ;
      FontStyleNotFound         : Result := 'FontStyleNotFound' ;
      NotTrueTypeFont           : Result := 'NotTrueTypeFont' ;
      UnsupportedGdiplusVersion : Result := 'UnsupportedGdiplusVersion' ;
      GdiplusNotInitialized     : Result := 'GdiplusNotInitialized' ;
      PropertyNotFound          : Result := 'PropertyNotFound' ;
      PropertyNotSupported      : Result := 'PropertyNotSupported' ;
      //ProfileNotFound           : Result := 'ProfileNotFound' ;
      else                        Result := 'unknown' ;
    end ;
  end ;

//=============================================================================
// Gdi+ object wrappers
//=============================================================================

  function  TGIS_GdipPen.setStatus(
    _status : GPSTATUS
  ) : GPSTATUS ;
  begin
    lastResult := _status ;
    Result := _status ;
    assert( lastResult = Ok ) ;
  end ;

  function  TGIS_GdipPen.fget_DashStyle
    : TGIS_PenStyle ;
  var
    dashStyle : GPDASHSTYLE ;
    ps : TGIS_PenStyle ;
  begin
    setStatus(
      GdipGetPenDashStyle( nativePen, dashStyle )
    ) ;
    case dashStyle of
      DashStyleDash       : ps := TGIS_PenStyle.Dash ;
      DashStyleDot        : ps := TGIS_PenStyle.Dot ;
      DashStyleDashDot    : ps := TGIS_PenStyle.DashDot ;
      DashStyleDashDotDot : ps := TGIS_PenStyle.DashDotDot ;
      else                  ps := TGIS_PenStyle.Solid ;
    end ;
    Result := ps ;
  end ;

  procedure TGIS_GdipPen.fset_DashStyle(
    _value : TGIS_PenStyle
  ) ;
  var
    dashStyle : GPDASHSTYLE ;
  begin
    case _value of
      TGIS_PenStyle.Dash       : dashStyle := DashStyleDash ;
      TGIS_PenStyle.Dot        : dashStyle := DashStyleDot ;
      TGIS_PenStyle.DashDot    : dashStyle := DashStyleDashDot ;
      TGIS_PenStyle.DashDotDot : dashStyle := DashStyleDashDotDot ;
      else                       dashStyle := DashStyleSolid ;
    end ;
    setStatus(
      GdipSetPenDashStyle( nativePen, dashStyle )
    ) ;
  end ;

  procedure TGIS_GdipPen.fset_LineJoin(
    _value : TGIS_LineJoin
  ) ;
  var
    lineJoin : GPLINEJOIN ;
  begin
    case _value of
      TGIS_LineJoin.Bevel : lineJoin := LineJoinBevel ;
      TGIS_LineJoin.Miter : lineJoin := LineJoinMiter ;
      TGIS_LineJoin.Round : lineJoin := LineJoinRound ;
      else                  lineJoin := LineJoinRound ;
    end ;
    setStatus(
      GdipSetPenLineJoin( nativePen, lineJoin )
    ) ;
  end ;

  procedure TGIS_GdipPen.fset_LineCap(
    _value : TGIS_LineCap
  ) ;
  var
    lineCap : GPLINECAP ;
  begin
    case _value of
      TGIS_LineCap.Flat   : lineCap := LineCapFlat ;
      TGIS_LineCap.Square : lineCap := LineCapSquare ;
      TGIS_LineCap.Round  : lineCap := LineCapRound  ;
      else                  lineCap := LineCapRound  ;
    end ;
    setStatus(
      GdipSetPenStartCap( nativePen, lineCap )
    ) ;
    setStatus(
      GdipSetPenEndCap( nativePen, lineCap )
    ) ;
  end ;

  procedure TGIS_GdipPen.fset_LineDash(
    _value : TGIS_DashArray
  ) ;
  var
    arr  : TGIS_DashArray ;
    i, j : Integer ;
  begin
    if length(_value) > 0 then begin
      j := 0 ;
      SetLength( arr, 2*length( _value ) ) ;
      for i := 0 to length(_value)-1 do
        if (i = 0) and (_value[i] < 0) then begin
          arr[j] := 0.01 ;
          inc(j);
          arr[j] := abs(_value[i]/FWidth) ;
          inc(j);
        end
        else begin
          if _value[i] = 0 then
            arr[j] := 0.1
          else
            arr[j] := abs(_value[i]/FWidth) ;
          inc(j);
        end;

      setStatus(
        GdipSetPenDashArray( nativePen, @arr[0], j )
      ) ;
    end;
  end ;

  function  TGIS_GdipPen.fget_PenType : TGIS_PenType ;
  var
    penType : GpPenType ;
  begin
    setStatus(
      GdipGetPenFillType( nativePen, penType )
    ) ;
    case penType of
      PenTypeSolidColor     : Result := TGIS_PenType.SolidColor ;
      PenTypeHatchFill      : Result := TGIS_PenType.HatchFill  ;
      PenTypeTextureFill    : Result := TGIS_PenType.TextureFill ;
      PenTypePathGradient   : Result := TGIS_PenType.PathGradient ;
      PenTypeLinearGradient : Result := TGIS_PenType.LinearGradient ;
      else                    Result := TGIS_PenType.Unknown ;
    end ;
  end ;

  constructor TGIS_GdipPen.Create(
    _color : TGIS_Color ;
    _width : Single = 1.0
  ) ;
  begin
    inherited Create ;
    nativePen := nil ;

    lastResult := GdipCreatePen1( GDIPColor( _color ), _width,
                                  UnitWorld, nativePen
                  ) ;
    FWidth := _width ;
  end ;

  constructor TGIS_GdipPen.Create(
    _brush : TGIS_GdipBrush ;
    _width : Single = 1.0
  ) ;
  begin
    inherited Create ;
    nativePen := nil ;
    lastResult := GdipCreatePen2( _brush.nativeBrush, _width,
                                  UnitWorld, nativePen
                  ) ;
    FWidth := _width ;
  end ;

  procedure TGIS_GdipPen.doDestroy ;
  begin
    GdipDeletePen( nativePen ) ;
  end ;

  procedure TGIS_GdipPen.SetBrushFillTransform(
    const _originX : Integer ;
    const _originY : Integer ;
    const _scaleX  : Double  ;
    const _scaleY  : Double
  ) ;
  var
    brush : GPBRUSH ;
  begin
    if PenType = TGIS_PenType.TextureFill then begin
      try
        GdipGetPenBrushFill( nativePen, brush ) ;
        GdipResetTextureTransform( brush ) ;
        GdipTranslateTextureTransform( brush, _originX, _originY, MatrixOrderPrepend ) ;
        GdipScaleTextureTransform( brush, _scaleX, _scaleY, MatrixOrderPrepend ) ;
        GdipSetPenBrushFill( nativePen, brush )
      except
      end;
    end ;
  end ;

  function  TGIS_GdipBrush.setStatus(
    _status : GPSTATUS
  ) : GPSTATUS ;
  begin
    lastResult := _status ;
    Result := _status ;
    assert( lastResult = Ok ) ;
  end ;

  procedure TGIS_GdipBrush.doDestroy ;
  begin
    GdipDeleteBrush( nativeBrush ) ;
  end ;

  constructor TGIS_GdipSolidBrush.Create(
    _color : TGIS_Color
  ) ;
  begin
    inherited Create ;
    nativeBrush := nil ;
    lastResult := GdipCreateSolidFill( GDIPColor( _color ), nativeBrush ) ;
  end;

  constructor TGIS_GdipHatchBrush.Create(
    _pattern   : TGIS_BrushStyle ;
    _foreColor : TGIS_Color
  ) ;
  var
    hatchStyle : GPHATCHSTYLE ;
  begin
    case (_pattern) of
      TGIS_BrushStyle.Horizontal :
            hatchStyle := HatchStyleHorizontal       ;
      TGIS_BrushStyle.Vertical   :
            hatchStyle := HatchStyleVertical         ;
      TGIS_BrushStyle.FDiagonal  :
            hatchStyle := HatchStyleForwardDiagonal  ;
      TGIS_BrushStyle.BDiagonal  :
            hatchStyle := HatchStyleBackwardDiagonal ;
      TGIS_BrushStyle.Cross      :
            hatchStyle := HatchStyleCross            ;
      TGIS_BrushStyle.DiagCross  :
            hatchStyle := HatchStyleDiagonalCross    ;
      else  hatchStyle := HatchStyleHorizontal       ;
    end ;
    nativeBrush := nil;
    lastResult := GdipCreateHatchBrush( Integer(hatchStyle),
                                        _foreColor.ARGB,
                                        TGIS_Color.None.ARGB,
                                        nativeBrush
                  ) ;
  end;

  constructor TGIS_GdipTextureBrush.Create(
    _bitmap : TGIS_Bitmap
  ) ;
  var
    img : GPBITMAP ;
    scan0 : TGIS_Pixels ;
  begin
    inherited Create ;
    nativeBrush := nil ;
    img := nil ;
    // not to loose transparency
    _bitmap.LockPixels( scan0 );
    GdipCreateBitmapFromScan0( _bitmap.Width, _bitmap.Height, 4 * _bitmap.Width,
                               PixelFormat32bppARGB, PBYTE(scan0), img ) ;
    lastResult := GdipCreateTexture( img, WrapModeTile, nativeBrush ) ;
    GdipDisposeImage( img ) ;
  end ;

  procedure TGIS_GdipTextureBrush.ResetTransform ;
  begin
    setStatus(
      GdipResetTextureTransform( nativeBrush )
    ) ;
  end ;

  procedure TGIS_GdipTextureBrush.TranslateTransform(
    _x : Single ;
    _y : Single
  ) ;
  begin
    setStatus(
      GdipTranslateTextureTransform( nativeBrush, _x, _y, MatrixOrderPrepend )
    ) ;
  end;

  procedure TGIS_GdipTextureBrush.ScaleTransform(
    _x : Single ;
    _y : Single
  ) ;
  begin
    setStatus(
      GdipScaleTextureTransform( nativeBrush, _x, _y, MatrixOrderPrepend )
    ) ;
  end ;

  function TGIS_GdipFontFamily.setStatus(
    _status : GPSTATUS
  ) : GPSTATUS ;
  begin
    lastResult := _status ;
    Result := _status;
    Assert( lastResult = Ok ) ;
  end ;

  procedure TGIS_GdipFontFamily.doDestroy ;
  begin
    GdipDeleteFontFamily( nativeFamily ) ;
    inherited ;
  end ;

  constructor TGIS_GdipFontFamily.Create(
    _familyName : WideString
  ) ;
  begin
    inherited Create ;
    nativeFamily := nil ;
    lastResult := GdipCreateFontFamilyFromName( PWideChar( _familyName ),
                                                nil, nativeFamily
                  ) ;
  end ;

  class function TGIS_GdipFontFamily.GenericSansSerif
    : TGIS_GdipFontFamily ;
  var
    ff : GPFONTFAMILY ;
  begin
    if not assigned( GenericSansSerifFontFamily ) then
    begin
      GenericSansSerifFontFamily := TGIS_GdipFontFamily.Create ;
      GenericSansSerifFontFamily.setStatus(
        GdipGetGenericFontFamilySansSerif( ff )
      ) ;
      GenericSansSerifFontFamily.nativeFamily := ff ;
    end;
    Result := GenericSansSerifFontFamily ;
  end ;

  procedure TGIS_GdipFont.doDestroy ;
  begin
    GdipDeleteFont( nativeFont ) ;
    if fontFamily_created then
      FreeObject( fontFamily ) ;
    inherited ;
  end ;

  constructor TGIS_GdipFont.Create(
    _familyName : WideString ;
    _emSize     : Single ;
    _style      : TGIS_FontStyles = []
  ) ;
  var
    st : Integer ;

    function get_style : FONTSTYLE ;
    begin
      if _style = [TGIS_FontStyle.Bold] then
        Result := FontStyleBold
      else if _style = [TGIS_FontStyle.Italic] then
        Result := FontStyleItalic
      else if _style = [TGIS_FontStyle.Bold, TGIS_FontStyle.Italic] then
        Result := FontStyleBoldItalic
      else if _style = [TGIS_FontStyle.Underline] then
        Result := FontStyleUnderline
      else if _style = [TGIS_FontStyle.Strikeout] then
        Result := FontStyleStrikeout
      else
        Result := FontStyleRegular ;
    end ;

  begin
    inherited Create ;
    nativeFont  := nil ;
    if _emSize < 0 then
      _emSize := -_emSize ;
    fontFamily_created := False ;

    fontFamily := TGIS_GdipFontFamily.Create( _familyName ) ;
    lastResult := fontFamily.lastResult ;
    if lastResult <> Ok then
    begin
      FreeObject( fontFamily ) ;
      fontFamily := TGIS_GdipFontFamily.GenericSansSerif ;
      lastResult := TGIS_GdipFontFamily.GenericSansSerif.lastResult ;
      if lastResult <> Ok then
        exit;
    end else
      fontFamily_created := True ;

    fontStyleSet := get_style ;
    lastResult := GdipCreateFont( fontFamily.nativeFamily, _emSize,
                                  Integer(fontStyleSet), Integer(UnitPoint),
                                  nativeFont
                                ) ;
    if lastResult = FontStyleNotFound then
      exit;
    if lastResult <> Ok then
    begin
      if fontFamily_created = True then
      begin
        FreeObject( fontFamily ) ;
        fontFamily_created := False ;
      end;
      fontFamily := TGIS_GdipFontFamily.GenericSansSerif ;
      lastResult := TGIS_GdipFontFamily.GenericSansSerif.lastResult ;
      if lastResult <> Ok then
        exit;
      lastResult := GdipCreateFont( fontFamily.nativeFamily, _emSize,
                                    Integer(fontStyleSet), Integer(UnitPoint),
                                    nativeFont
                                  ) ;
    end ;
    Assert( lastResult = Ok ) ;
  end ;

  function TGIS_GdipFont.fget_Style
    : TGIS_FontStyles ;
  begin
    Result := [] ;
    if fontStyleSet = FontStyleBold then
      Result := [TGIS_FontStyle.Bold] ;
    if fontStyleSet = FontStyleItalic then
      Result := [TGIS_FontStyle.Italic] ;
    if fontStyleSet = FontStyleBoldItalic then
      Result := [TGIS_FontStyle.Bold,TGIS_FontStyle.Italic] ;
    if fontStyleSet = FontStyleUnderline then
      Result := Result + [TGIS_FontStyle.Underline] ;
    if fontStyleSet = FontStyleStrikeOut then
      Result := Result + [TGIS_FontStyle.StrikeOut] ;
  end ;

  function TGIS_GdipFont.GetLastStatus : GPSTATUS ;
  begin
    Result := lastResult ;
    lastResult := Ok ;
  end ;

  function TGIS_GdipStringFormat.setStatus(
    _status : GPSTATUS
  ) : GPSTATUS ;
  begin
    lastResult := _status ;
    Result := _status ;
  end ;

  function  TGIS_GdipStringFormat.fget_FormatFlags
    : Integer ;
  begin
    setStatus(
      GdipGetStringFormatFlags( nativeFormat, Result )
    ) ;
  end ;

  procedure TGIS_GdipStringFormat.fset_FormatFlags(
    _value : Integer
  ) ;
  begin
    setStatus(
      GdipSetStringFormatFlags( nativeFormat, _value )
    ) ;
  end ;

  procedure TGIS_GdipStringFormat.doDestroy ;
  begin
    GdipDeleteStringFormat( nativeFormat ) ;
    inherited ;
  end ;

  constructor TGIS_GdipStringFormat.Create(
    _formatFlags : Integer = 0 ;
    _language    : LANGID = LANG_NEUTRAL
  ) ;
  begin
    inherited Create ;
    nativeFormat := nil ;
    lastResult := GdipCreateStringFormat( _formatFlags, _language, nativeFormat ) ;
  end ;

  constructor TGIS_GdipStringFormat.Create(
    _format : TGIS_GdipStringFormat
  ) ;
  begin
    inherited Create ;
    nativeFormat := nil;
    assert( assigned(_format) ) ;
    lastResult := GdipCloneStringFormat( _format.nativeFormat, nativeFormat ) ;
  end ;

  class function TGIS_GdipStringFormat.GenericTypographic
    : TGIS_GdipStringFormat ;
  begin
    if not assigned( GenericTypographicStringFormat ) then
    begin
      GenericTypographicStringFormat := TGIS_GdipStringFormat.Create ;
      GenericTypographicStringFormat.lastResult :=
        GdipStringFormatGetGenericTypographic(
          GenericTypographicStringFormat.nativeFormat
        ) ;
      GenericTypographicStringFormat.FormatFlags
        := GenericTypographicStringFormat.FormatFlags or
           StringFormatFlagsNoWrap or
           StringFormatFlagsMeasureTrailingSpaces ;
    end;
    Result := GenericTypographicStringFormat ;
  end ;

  constructor TGIS_GdipImageAttributes.Create ;
  begin
    nativeImageAttr := nil ;
    lastResult := GdipCreateImageAttributes( nativeImageAttr ) ;
  end ;

  procedure TGIS_GdipImageAttributes.doDestroy ;
  begin
    GdipDisposeImageAttributes( nativeImageAttr ) ;
    inherited ;
  end ;

  function TGIS_GdipImageAttributes.setStatus(
    _status : GPSTATUS
  ) : GPSTATUS ;
  begin
    lastResult := _status ;
    Result := _status ;
    Assert( lastResult = Ok ) ;
  end;

  procedure TGIS_GdipImageAttributes.SetColorKey(
    _colorLow  : TGIS_Color ;
    _colorHigh : TGIS_Color
  ) ;
  begin
    setStatus(
      GdipSetImageAttributesColorKeys( nativeImageAttr,
                                       ColorAdjustTypeDefault, True,
                                       _colorLow.ARGB,
                                       _colorHigh.ARGB
      )
    ) ;
  end ;

  procedure TGIS_GdipImage.doDestroy ;
  begin
    GdipDisposeImage( nativeImage ) ;
    inherited ;
  end ;

  function TGIS_GdipImage.setStatus(
    _status: GPSTATUS
  ) : GPSTATUS ;
  begin
    lastResult := _status ;
    Result := _status ;
    assert( lastResult = Ok ) ;
  end ;

  function TGIS_GdipImage.fget_PixelFormat :
    TGIS_GdipPixelFormat ;
  begin
    setStatus(
      GdipGetImagePixelFormat( nativeImage, Result )
    ) ;
  end ;

  constructor TGIS_GdipBitmap.Create(
    _hbm  : HBITMAP ;
    _hpal : HPALETTE
  ) ;
  var
    bitmap : GPBITMAP ;
  begin
    bitmap := nil ;
    lastResult := GdipCreateBitmapFromHBITMAP( _hbm, _hpal, bitmap ) ;
    nativeImage := bitmap ;
  end ;

  constructor TGIS_GdipBitmap.Create(
    _width      : Integer     ;
    _height     : Integer     ;
    _stride     : Integer     ;
    _format     : PixelFormat ;
    _scan0      : PBYTE
  ) ;
  var
    bitmap : GPBITMAP ;
  begin
    bitmap := nil ;
    lastResult := GdipCreateBitmapFromScan0( _width, _height, _stride,
                                             _format, _scan0, bitmap ) ;
    nativeImage := bitmap ;
  end ;

  constructor TGIS_GdipBitmap.Create(
    const _bmp : TGIS_Bitmap
  ) ;
  var
    bitmap : GPBITMAP ;
    scan0  : TGIS_Pixels ;
  begin
    inherited Create ;
    bitmap := nil ;
    // not to loose transparency
    _bmp.LockPixels( scan0 );
    lastResult := GdipCreateBitmapFromScan0( _bmp.Width, _bmp.Height,
                                             4 * _bmp.Width,
                                             PixelFormat32bppPARGB,
                                             PBYTE(scan0), bitmap ) ;
    nativeImage := bitmap ;
  end ;

  function TGIS_GdipBitmap.LockBits(
    _rect   : TRect ;
    _flags  : UINT  ;
    _format : TGIS_GdipPixelFormat
  ) : TGIS_GdipBitmapData ;
  var
    bd : TGIS_GdipBitmapData ;
  begin
    setStatus(
      GdipBitmapLockBits( GpBitmap(nativeImage), @_rect,
                          _flags, _format, @bd )
    ) ;
    Result := bd ;
  end ;

  procedure TGIS_GdipBitmap.UnlockBits(
    var _lockedBitmapData : TGIS_GdipBitmapData
  ) ;
  begin
    setStatus(
      GdipBitmapUnlockBits( GpBitmap(nativeImage),
                            @_lockedBitmapData
                          )
    ) ;
  end ;

  function TGIS_GdipGraphics.setStatus(
    _status : GPSTATUS
  ) : GPSTATUS ;
  begin
    lastResult := _status;
    Result := _status;
    if _status = GenericError
    then
      Assert( False, 'GenericError on TGIS_GdipGraphics' )
    else
      Assert( lastResult = Ok, 'error = ' + GetShortStatusMessage( _status ) ) ;
  end ;

  procedure TGIS_GdipGraphics.doDestroy ;
  begin
    GdipDeleteGraphics( nativeGraphics ) ;
    inherited ;
  end ;

  constructor TGIS_GdipGraphics.Create(
    _hdc : HDC
  ) ;
  begin
    inherited Create ;

    nativeGraphics := nil ;
    lastResult := GdipCreateFromHdc( _hdc, nativeGraphics ) ;
    if lastResult = Ok then begin
      lastResult := GdipSetSmoothingMode( nativeGraphics,
                                          SmoothingModeAntiAlias
                                        ) ;
      lastResult := GdipSetTextRenderingHint(
                      nativeGraphics,
                      TextRenderingHintAntiAlias
                   ) ;
    end ;
  end ;

  procedure TGIS_GdipGraphics.SetRenderingOrigin(
    _x      : Integer ;
    _y      : Integer
  ) ;
  begin
    setStatus(
      GdipSetRenderingOrigin( nativeGraphics, _x, _y )
    ) ;
  end ;


  procedure TGIS_GdipGraphics.DrawEllipse(
    _pen    : TGIS_GdipPen ;
    _x      : Integer ;
    _y      : Integer ;
    _width  : Integer ;
    _height : Integer
  ) ;
  begin
    setStatus(
      GdipDrawEllipseI( nativeGraphics, _pen.nativePen,
                        _x, _y, _width, _height )
    ) ;
  end ;

  procedure TGIS_GdipGraphics.DrawImage(
    _image : TGIS_GdipImage ;
    _x     : Integer ;
    _y     : Integer
  ) ;
  var
    image : GPIMAGE ;
  begin
    if assigned( _image ) then
      image := _image.nativeImage
    else
      image := nil ;
    if not assigned( image ) then exit ;

    setStatus(
      GdipDrawImageI( nativeGraphics, image, _x, _y )
    ) ;
  end ;

  procedure TGIS_GdipGraphics.DrawImage(
    _image     : TGIS_GdipImage ;
    _destRect  : TRect   ;
    _srcX      : Integer ;
    _srcY      : Integer ;
    _srcWidth  : Integer ;
    _srcHeight : Integer ;
    _imageAttr : TGIS_GdipImageAttributes = nil
  ) ;
  var
    image     : GPIMAGE ;
    imageAttr : GPIMAGEATTRIBUTES ;
  begin
    if ( _destRect.Left = _destRect.Right ) or
       ( _destRect.Top  = _destRect.Bottom ) or
       ( _srcWidth = 0 ) or ( _srcHeight = 0 ) then exit ;

    if assigned( _image ) then
      image := _image.nativeImage
    else
      image := nil ;
    if not assigned( image ) then exit ;

    if assigned( _imageAttr ) then
      imageAttr := _imageAttr.nativeImageAttr
    else
      imageAttr := nil ;
    setStatus(
      GdipDrawImageRectRectI( nativeGraphics, image,
                              _destRect.Left, _destRect.Top,
                              _destRect.Right - _destRect.Left,
                              _destRect.Bottom - _destRect.Top,
                              _srcX, _srcY, _srcWidth, _srcHeight,
                              UnitPixel, imageAttr, nil, nil )
    ) ;
  end ;

  procedure TGIS_GdipGraphics.DrawLines(
          _pen    : TGIS_GdipPen ;
    const _points : array of TPoint ;
          _count  : Integer
  ) ;
  begin
    Assert( Length( _points ) > 0 ) ;
    Assert( Length( _points ) > 1 ) ;
    setStatus(
      GdipDrawLinesI( nativeGraphics, _pen.nativePen,
                      @_points[0], _count
      )
    ) ;
  end ;

  procedure TGIS_GdipGraphics.DrawLines(
          _pen    : TGIS_GdipPen ;
    const _points : array of TGPPointF ;
          _count  : Integer
  ) ;
  begin
    Assert( Length( _points ) > 0 ) ;
    Assert( Length( _points ) > 1 ) ;
    setStatus(
      GdipDrawLines( nativeGraphics, _pen.nativePen,
                     @_points[0], _count
      )
    ) ;
  end ;

  procedure TGIS_GdipGraphics.DrawPie(
    _pen        : TGIS_GdipPen ;
    _x          : Integer ;
    _y          : Integer ;
    _width      : Integer ;
    _height     : Integer ;
    _startAngle : Integer ;
    _sweepAngle : Integer
  ) ;
  begin
    setStatus(
      GdipDrawPieI( nativeGraphics, _pen.nativePen, _x, _y, _width, _height,
                    _startAngle, _sweepAngle
      )
    ) ;
  end;

  procedure TGIS_GdipGraphics.DrawArc(
    _pen        : TGIS_GdipPen ;
    _x          : Integer ;
    _y          : Integer ;
    _width      : Integer ;
    _height     : Integer ;
    _startAngle : Single ;
    _sweepAngle : Single
  ) ;
  begin
    setStatus(
      GdipDrawArcI( nativeGraphics, _pen.nativePen, _x, _y, _width, _height,
                    _startAngle, _sweepAngle
      )
    ) ;
  end;

  procedure TGIS_GdipGraphics.DrawPolygon(
          _pen    : TGIS_GdipPen ;
    const _points : array of TPoint
  ) ;
  begin
    setStatus(
      GdipDrawPolygonI( nativeGraphics, _pen.nativePen,
                        @_points[0], High(_points)+1
      )
    ) ;
  end ;

  procedure TGIS_GdipGraphics.DrawRectangle(
    _pen  : TGIS_GdipPen ;
    _rect : TRect
  ) ;
  begin
    setStatus(
      GdipDrawRectangleI( nativeGraphics, _pen.nativePen,
                          _rect.Left, _rect.Top,
                          _rect.Right  - _rect.Left,
                          _rect.Bottom - _rect.Top )
    ) ;
  end ;

  procedure TGIS_GdipGraphics.DrawRectangle(
    _pen    : TGIS_GdipPen ;
    _left   : Integer ;
    _top    : Integer ;
    _width  : Integer ;
    _height : Integer
  ) ;
  begin
    setStatus(
      GdipDrawRectangleI( nativeGraphics, _pen.nativePen,
                          _left, _top, _width, _height )
    ) ;
  end ;

  procedure TGIS_GdipGraphics.DrawString(
    _s          : WideString     ;
    _font       : TGIS_GdipFont  ;
    _brush      : TGIS_GdipBrush ;
    _layoutRect : TRect          ;
    _format     : TGIS_GdipStringFormat
  ) ;
  var
    font         : GPFONT ;
    stringFormat : GPSTRINGFORMAT ;
    brush        : GPBRUSH ;
    rct          : TGIS_GdipRectF ;
  begin
    if Assigned( _font ) then
      font := _font.nativeFont
    else
      font := nil ;
    if assigned( _brush ) then
      brush := _brush.nativeBrush
    else
      brush := nil ;
    if Assigned( _format ) then
      stringFormat := _format.nativeFormat
    else
      stringFormat := nil ;

    rct := ToGdiPlusRectF( _layoutRect );
    setStatus(
      GdipDrawString( nativeGraphics, PWideChar(_s), Length(_s), font, @rct,
                      stringFormat, brush )
    ) ;
  end ;

  procedure TGIS_GdipGraphics.DrawString(
    _s      : WideString     ;
    _font   : TGIS_GdipFont  ;
    _brush  : TGIS_GdipBrush ;
    _x      : Single         ;
    _y      : Single         ;
    _format : TGIS_GdipStringFormat
  ) ;
  var
    rct : TRect ;
  begin
    rct.Left := RoundS( _x ) ;
    rct.Top  := RoundS( _y ) ;
    rct.Right  := rct.Left ;
    rct.Bottom := rct.Top  ;
    DrawString( _s, _font, _brush, rct, _format ) ;
  end ;

  procedure TGIS_GdipGraphics.FillEllipse(
    _brush  : TGIS_GdipBrush ;
    _x      : Integer ;
    _y      : Integer ;
    _width  : Integer ;
    _height : Integer
  ) ;
  begin
    setStatus(
      GdipFillEllipseI( nativeGraphics, _brush.nativeBrush,
                        _x, _y, _width, _height
      )
    ) ;
  end ;

  procedure TGIS_GdipGraphics.FillPie(
    _brush      : TGIS_GdipBrush ;
    _x          : Integer ;
    _y          : Integer ;
    _width      : Integer ;
    _height     : Integer ;
    _startAngle : Integer ;
    _sweepAngle : Integer
  ) ;
  begin
    setStatus(
      GdipFillPieI( nativeGraphics, _brush.nativeBrush, _x, _y, _width, _height,
                    _startAngle, _sweepAngle
      )
    ) ;
  end;

  procedure TGIS_GdipGraphics.FillPolygon(
          _brush  : TGIS_GdipBrush ;
    const _points : array of TPoint
  ) ;
  begin
    setStatus(
      GdipFillPolygonI( nativeGraphics, _brush.nativeBrush,
                        @_points[0], High(_points)+1,
                        FillModeAlternate
      )
    ) ;
  end ;

  procedure TGIS_GdipGraphics.FillPolygon(
          _brush  : TGIS_GdipBrush ;
    const _points : array of TGPPointF
  ) ;
  begin
    setStatus(
      GdipFillPolygon( nativeGraphics, _brush.nativeBrush,
                       @_points[0], High(_points)+1,
                       FillModeAlternate
      )
    ) ;
  end ;

  procedure TGIS_GdipGraphics.FillRectangle(
    _brush : TGIS_GdipBrush ;
    _rect  : TRect
  ) ;
  begin
    setStatus(
      GdipFillRectangleI( nativeGraphics, _brush.nativeBrush,
                          _rect.Left, _rect.Top,
                          _rect.Right  - _rect.Left,
                          _rect.Bottom - _rect.Top )
    ) ;
  end ;

  procedure TGIS_GdipGraphics.FillRectangle(
    _brush  : TGIS_GdipBrush ;
    _left   : Integer ;
    _top    : Integer ;
    _width  : Integer ;
    _height : Integer
  ) ;
  begin
    setStatus(
      GdipFillRectangleI( nativeGraphics, _brush.nativeBrush,
                          _left, _top, _width, _height )
    ) ;
  end ;

  function TGIS_GdipGraphics.GetHdc
    : HDC ;
  begin
    setStatus(
      GdipGetDc( nativeGraphics, Result )
    ) ;
  end ;

  function TGIS_GdipGraphics.MeasureString(
    _text         : WideString ;
    _font         : TGIS_GdipFont ;
    _width        : Integer    ;
    _stringFormat : TGIS_GdipStringFormat
  ) : TGIS_GdipSizeF ;
  var
    rct          : TGIS_GdipRectF ;
    rct_out      : TGIS_GdipRectF ;
    font         : GPFONT ;
    stringFormat : GPSTRINGFORMAT ;
  begin
    rct.X := 0.0 ;
    rct.Y  := 0.0 ;
    rct.Width  := 0.0 ;
    rct.Height := 0.0 ;

    if Assigned( _font ) then
      font := _font.nativeFont
    else
      font := nil ;
    if Assigned( _stringFormat ) then
      stringFormat := _stringFormat.nativeFormat
    else
      stringFormat := nil ;

    setStatus(
      GdipMeasureString( nativeGraphics, PWideChar(_text), Length(_text), font,
                         @rct, stringFormat, @rct_out, nil, nil )
    ) ;


    Result.Width  := RoundS( rct_out.Width  ) ;
    Result.Height := RoundS( rct_out.Height ) ;
  end ;

  procedure TGIS_GdipGraphics.ReleaseHdc(
    _hdc : HDC
  ) ;
  begin
    setStatus(
      GdipReleaseDc( nativeGraphics, _hdc )
    ) ;
  end;

  procedure TGIS_GdipGraphics.ResetTransform ;
  begin
    setStatus(
      GdipResetWorldTransform( nativeGraphics )
    ) ;
  end ;

  procedure TGIS_GdipGraphics.RotateTransform(
    _angle : Single ;
    _order : GPMATRIXORDER = MatrixOrderPrepend
  ) ;
  begin
    setStatus(
      GdipRotateWorldTransform( nativeGraphics, _angle, _order )
    ) ;
  end ;

  procedure TGIS_GdipGraphics.TranslateTransform(
    _dx    : Single ;
    _dy    : Single ;
    _order : GPMATRIXORDER = MatrixOrderPrepend
  ) ;
  begin
    setStatus(
      GdipTranslateWorldTransform( nativeGraphics, _dx, _dy, _order )
    ) ;
  end ;

  procedure TGIS_GdipGraphics.SetPageUnit(
    _unit : TUnit
  ) ;
  begin
    setStatus(
      GdipSetPageUnit( nativeGraphics, GpUnit(_unit) )
    ) ;
  end ;

  procedure TGIS_GdipGraphics.SetSmoothingMode(
    _mode : TSmoothingMode
  ) ;
  begin
    setStatus(
      GdipSetSmoothingMode( nativeGraphics, _mode )
    ) ;
  end ;

  procedure TGIS_GdipGraphics.SetPixelOffsetMode(
    _mode : TPixelOffsetMode
  ) ;
  begin
    setStatus(
      GdipSetPixelOffsetMode( nativeGraphics, _mode )
    ) ;
  end ;

  procedure TGIS_GdipGraphics.SetInterpolationMode(
    _mode : TInterpolationMode
  ) ;
  begin
    setStatus(
      GdipSetInterpolationMode( nativeGraphics, _mode )
    ) ;
  end ;

  procedure TGIS_GdipGraphics.SetTextContrast(
    _contrast : Cardinal
  ) ;
  begin
    setStatus(
      GdipSetTextContrast( nativeGraphics, _contrast )
    ) ;
  end ;

  procedure TGIS_GdipGraphics.SetCompositingMode(
    _mode : TCompositingMode
  ) ;
  begin
    setStatus(
      GdipSetCompositingMode( nativeGraphics, _mode )
    ) ;
  end;

  procedure TGIS_GdipGraphics.SetCompositingQuality(
    _quality : TCompositingQuality
  ) ;
  begin
    setStatus(
      GdipSetCompositingQuality( nativeGraphics, _quality )
    ) ;
  end ;

initialization
begin
  StartupInput.DebugEventCallback       := nil   ;
  StartupInput.SuppressBackgroundThread := True  ;
  StartupInput.SuppressExternalCodecs   := False ;
  StartupInput.GdiplusVersion           := 1     ;
  StartupOutput.NotificationHook        := nil   ;
  StartupOutput.NotificationUnhook      := nil   ;
  GdiPlusStartup( GdiPlusToken, @StartupInput, @StartupOutput ) ;
end;

finalization
begin
  if Assigned( GenericSansSerifFontFamily ) then
    GenericSansSerifFontFamily.Free ;
  if Assigned( GenericTypographicStringFormat ) then
    GenericTypographicStringFormat.Free ;
  if not IsLibrary then
    GdiPlusShutdown( GdiPlusToken ) ;
end;

{==================================== END =====================================}
end.

