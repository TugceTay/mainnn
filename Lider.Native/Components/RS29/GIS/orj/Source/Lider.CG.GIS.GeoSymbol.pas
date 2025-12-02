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
  Encapsulation of Symbols (Picture, Font or CGM).
}

{$IFDEF DCC}
  unit GisSymbol ;
  {$HPPEMIT '#pragma link "GisSymbol"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}
{$IFDEF COCOA}
  namespace TatukGIS.OSDK ;
{$ENDIF}
{$IFDEF ISLAND}
  namespace TatukGIS ;
{$ENDIF}


{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    System.Drawing,
    System.Runtime.InteropServices,
    System.Data,
    System.Text,
    System.IO,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.Types,
    System.Generics.Collections,

    GisRtl,
    GisInterfaces,
    GisClasses,
    GisTypes,
    GisTypesUI,
    GisStreams;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    java.awt.*,
    tatukgis.rtl ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    TatukGIS.RTL ;
{$ENDIF}

type

  // forward declarations
  TGIS_SymbolAbstract = class ;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Position of the symbol relative to the point (0,0).
  /// </summary>
  TGIS_SymbolPosition = TGIS_LabelPosition ;

  /// <summary>
  ///   Encapsulation of the symbol list (cache). Use it to efficiently manage
  ///   symbols.
  /// </summary>
  TGIS_SymbolList = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ThreadClass )

    private
      symbolList : TDictionary<String,TGIS_SymbolAbstract> ;

    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy ; override;

    public

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create ;

      /// <summary>
      ///   Clear the whole list (remove all items from list).
      /// </summary>
      procedure   Clear  ;

      /// <summary>
      ///   Clear the whole cache (all symbol will persist but the cached
      ///   version will be destroyed.
      /// </summary>
      /// <remarks>
      ///   This function simply frees as much memory as possible.
      /// </remarks>
      procedure   ClearCache ;

      /// <summary>
      ///   Prepare a new symbol.
      /// </summary>
      /// <param name="_name">
      ///   name of the symbol; see comment for TGIS_SymbolFont.Create and
      ///   TGIS_SymbolPicture.Create and TGIS_SymbolLine.Create
      /// </param>
      /// <remarks>
      ///   If the symbol exists in memory, then pointer to the existing
      ///   symbol will be returned.
      /// </remarks>
      /// <returns>
      ///   symbol object
      /// </returns>
      function    Prepare( const _name     : String
                         ) : TGIS_SymbolAbstract ; overload;

      /// <summary>
      ///   Prepare a new symbol.
      /// </summary>
      /// <param name="_name">
      ///   name of the symbol; see comment for TGIS_SymbolFont.Create and
      ///   TGIS_SymbolPicture.Create and TGIS_SymbolLine.Create
      /// </param>
      /// <param name="_stream">
      ///   graphics stream for bitmap, for non\-file symbols
      /// </param>
      /// <remarks>
      ///   If the symbol exists in memory, then pointer to the existing symbol
      ///   will be returned.
      /// </remarks>
      /// <returns>
      ///   symbol object
      /// </returns>
      function    Prepare( const _name     : String ;
                           {$IFDEF CLR}
                             const _stream : Stream
                           {$ELSE}
                             const _stream : TStream
                           {$ENDIF}
                         ) : TGIS_SymbolAbstract ; overload;

      /// <summary>
      ///   Delete a symbol from the list (for internal purposes).
      /// </summary>
      /// <remarks>
      ///   For internal use of TatukGIS.
      /// </remarks>
      /// <param name="_name">
      ///   name of the symbol
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///     This method is for internal use only.
      ///   </note>
      /// </remarks>
      procedure   InternalDelete
                         ( const _name     : String
                         ) ; overload;

      /// <summary>
      ///   Delete a symbol from the list (for internal purposes).
      /// </summary>
      /// <param name="_symbol">
      ///   symbol to delete
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///     This method is for internal use only.
      ///   </note>
      /// </remarks>
      procedure   InternalDelete
                         ( const _symbol   : TGIS_SymbolAbstract
                         ) ; overload;
  end ;

  /// <summary>
  ///   Encapsulation of symbols.
  /// </summary>
  TGIS_SymbolAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF} class ( TGIS_ObjectDisposable )

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FName        : String  ;
      FNativeSize  : Single  ;  // size of the symbol in original units
      FNativeHeight: Single  ;  // height of the symbol in original units
      FNativeWidth : Single  ;  // width of the symbol in original units
      FSize        : Integer ;
      FWidth       : Integer ;
      FHeight      : Integer ;
      FCenter      : TPointF ;
      FIsFileBased : Boolean ;
      FAutoCenter  : Boolean ;

    private
      oThread       : TGIS_ThreadClass ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <summary>
      ///   Returns size of the symbol.
      /// </summary>
      /// <returns>
      ///   symbol size
      /// </returns>
      function getSymSize       : Integer ; virtual;

      /// <summary>
      ///   Returns width of the symbol.
      /// </summary>
      /// <returns>
      ///   symbol width
      /// </returns>
      function getSymWidth      : Integer ; virtual;

      /// <summary>
      ///   Returns height of the symbol.
      /// </summary>
      /// <returns>
      ///   symbol height
      /// </returns>
      function getSymHeight     : Integer ; virtual;

      /// <summary>
      ///   Returns the real width of the symbol in pixels.
      /// </summary>
      /// <returns>
      ///   symbol width
      /// </returns>
      function getRealSymWidth  : Integer ; virtual; abstract;

      /// <summary>
      ///   Returns the real height of the symbol in pixels.
      /// </summary>
      /// <returns>
      ///   symbol height
      /// </returns>
      function getRealSymHeight : Integer ; virtual; abstract;

      /// <summary>
      ///   Set some parameters of symbol.
      /// </summary>
      /// <param name="_marker">
      ///   if True the symbol is a marker symbol; symbols connected to
      ///   markers are not rotated with the viewer
      /// </param>
      procedure set_symbol_parameters ( _marker : Boolean
                                      ) ; virtual;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      symRot       : Double ;
      symFullRot   : Double ;
      symSin       : Double ;
      symCos       : Double ;
      symFinalRot  : Double ;

      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      symViewer    : IGIS_Viewer ;
      symRenderer  : TObject ;
      symInTwips   : Double  ;
      symSize      : Double  ;
      symScale     : Double  ;
      symScaleX    : Double  ;
      symColor1    : TGIS_Color ;
      symColor2    : TGIS_Color ;
      symAngle     : Double  ;
      symGap       : Integer ;
      symPosition  : TGIS_SymbolPosition  ;
      symOffset    : TPoint  ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <summary>
      ///   Convert a value to the scaled pixel value.
      /// </summary>
      /// <param name="_value">
      ///   value to be converted
      /// </param>
      /// <returns>
      ///   converted value
      /// </returns>
      function  toPixelI         ( const _value : Single
                                 ) : Integer ;

      /// <summary>
      ///   Convert a value to the scaled pixel value.
      /// </summary>
      /// <param name="_value">
      ///   value to be converted
      /// </param>
      /// <returns>
      ///   converted value
      /// </returns>
      function  toPixelF         ( const _value : Single
                                 ) : Double ;

      /// <summary>
      ///   Convert a value from the scaled pixel value.
      /// </summary>
      /// <param name="_value">
      ///   value to be converted (in pixels)
      /// </param>
      /// <returns>
      ///   converted value
      /// </returns>
      function  fromPixel        ( const _value : Single
                                 ) : Integer ;

      /// <summary>
      ///   Scale to pixels and rotate a given point.
      /// </summary>
      /// <param name="_pt">
      ///   point to be scaled and rotated
      /// </param>
      /// <returns>
      ///   new point after operation
      /// </returns>
      function  scaleAndRotate   ( const _pt    : TPoint
                                 ) : TPointF  ;

      /// <summary>
      ///   Set the symbol position (related to point origin).
      /// </summary>
      /// <param name="_position">
      ///   symbol position
      /// </param>
      procedure setPosition      ( const _position : TGIS_SymbolPosition
                                 ) ; virtual;

      procedure fset_AutoCenter  ( const _value : Boolean
                                 ) ;


      function  fget_ShieldLabel : TRectF ; virtual ;
      function  fget_ShieldBounds: TRectF ; virtual ;

      /// <summary>
      ///   Realize color. If color is not defined, then the internal symbol
      ///   color will be used.
      /// </summary>
      /// <param name="_color">
      ///   defined color
      /// </param>
      /// <returns>
      ///   color to be used
      /// </returns>
      function  realizeColor1 ( const _color : TGIS_Color
                              ) : TGIS_Color  ;

      /// <summary>
      ///   Realize color. If color is not defined, then the internal symbol
      ///   color will be used.
      /// </summary>
      /// <param name="_color">
      ///   defined color
      /// </param>
      /// <returns>
      ///   color to be used
      /// </returns>
      function  realizeColor2 ( const _color : TGIS_Color
                              ) : TGIS_Color  ;

      /// <summary>
      ///   Prepare symbol rotation
      /// </summary>
      /// <param name="_angle">
      ///   angle in radians
      /// </param>
      /// <param name="_marker">
      ///   if True the symbol is a marker symbol; symbols connected to markers
      ///   are not rotated with the viewer
      /// </param>
      procedure prepareRotate ( const _angle  : Single ;
                                const _marker : Boolean
                              ) ; virtual;

    protected

      /// <summary>
      ///   Destroy the instance.
      /// </summary>
      procedure doDestroy     ; override;

    public

      /// <summary>
      ///   Create a symbol.
      /// </summary>
      /// <param name="_name">
      ///   symbol name; see comment for TGIS_SymbolFont.Create and
      ///   TGIS_SymbolPicture.Create
      /// </param>
      constructor Create      ( const _name : String
                              ) ;
      /// <summary>
      ///   Prepare symbol
      /// </summary>
      /// <param name="_viewer">
      ///   pointer to the viewer
      /// </param>
      /// <param name="_size">
      ///   desired size
      /// </param>
      /// <param name="_color1">
      ///   color to be used as the main color for the symbol; if
      ///   GIS_RENDER_COLOR is used, then original symbol color will be used
      /// </param>
      /// <param name="_color2">
      ///   color to be used as the secondary color for the symbol; if
      ///   GIS_RENDER_COLOR is used, then original symbol color will be used
      /// </param>
      /// <param name="_angle">
      ///   rotation angle in radians (clockwise)
      /// </param>
      /// <param name="_gap">
      ///   gap between symbols
      /// </param>
      /// <param name="_position">
      ///   symbol position
      /// </param>
      /// <param name="_marker">
      ///   if True the symbol is a marker symbol; symbols connected to
      ///   markers are not rotated with the viewer
      /// </param>
      procedure Prepare     ( const _viewer    : IGIS_Viewer ;
                              const _size      : Integer ;
                              const _color1    : TGIS_Color ;
                              const _color2    : TGIS_Color ;
                              const _angle     : Double  ;
                              const _gap       : Integer ;
                              const _position  : TGIS_SymbolPosition ;
                              const _marker    : Boolean
                            ) ; overload; virtual;

      /// <summary>
      ///   Prepare symbol
      /// </summary>
      /// <param name="_viewer">
      ///   pointer to the viewer
      /// </param>
      /// <param name="_size">
      ///   desired size
      /// </param>
      /// <param name="_color1">
      ///   color to be used as the main color for the symbol; if
      ///   GIS_RENDER_COLOR is used, then original symbol color will be used
      /// </param>
      /// <param name="_color2">
      ///   color to be used as the secondary color for the symbol; if
      ///   GIS_RENDER_COLOR is used, then original symbol color will be used
      /// </param>
      /// <param name="_angle">
      ///   rotation angle in radians (clockwise)
      /// </param>
      /// <param name="_gap">
      ///   gap between symbols
      /// </param>
      /// <param name="_position">
      ///   symbol position
      /// </param>
      /// <param name="_marker">
      ///   if True the symbol is a marker symbol; symbols connected to markers
      ///   are not rotated with the viewer
      /// </param>
      /// <param name="_renderer2">
      ///   if not nil then symbol will be drawn on provided renderer
      /// </param>
      procedure Prepare     ( const _viewer    : IGIS_Viewer ;
                              const _size      : Integer     ;
                              const _color1    : TGIS_Color  ;
                              const _color2    : TGIS_Color  ;
                              const _angle     : Double      ;
                              const _gap       : Integer     ;
                              const _position  : TGIS_SymbolPosition ;
                              const _marker    : Boolean     ;
                              const _renderer2 : TObject
                            ) ; overload; virtual;

      /// <summary>
      ///   Prepare symbol
      /// </summary>
      /// <param name="_viewer">
      ///   pointer to the viewer
      /// </param>
      /// <param name="_size">
      ///   desired size
      /// </param>
      /// <param name="_scaleX">
      ///   extra width scaling
      /// </param>
      /// <param name="_color1">
      ///   color to be used as the main color for the symbol; if
      ///   GIS_RENDER_COLOR is used, then original symbol color will be used
      /// </param>
      /// <param name="_color2">
      ///   color to be used as the secondary color for the symbol; if
      ///   GIS_RENDER_COLOR is used, then original symbol color will be used
      /// </param>
      /// <param name="_angle">
      ///   rotation angle in radians (clockwise)
      /// </param>
      /// <param name="_gap">
      ///   gap between symbols
      /// </param>
      /// <param name="_position">
      ///   symbol position
      /// </param>
      /// <param name="_marker">
      ///   if True the symbol is a marker symbol; symbols connected to markers
      ///   are not rotated with the viewer
      /// </param>
      /// <param name="_renderer2">
      ///   if not nil then symbol will be drawn on provided renderer
      /// </param>
      procedure Prepare     ( const _viewer    : IGIS_Viewer ;
                              const _size      : Integer     ;
                              const _scaleX    : Double      ;
                              const _color1    : TGIS_Color  ;
                              const _color2    : TGIS_Color  ;
                              const _angle     : Double      ;
                              const _gap       : Integer     ;
                              const _position  : TGIS_SymbolPosition ;
                              const _marker    : Boolean     ;
                              const _renderer2 : TObject
                            ) ; overload; virtual;

      /// <summary>
      ///   Unprepare symbol.
      /// </summary>
      /// <remarks>
      ///   Call it always after finishing the draw. See TGIS_SymbolList.Prepare
      ///   for example.
      /// </remarks>
      procedure Unprepare     ; virtual;

      /// <summary>
      ///   Clear the internal representation of a symbol. If this symbol is
      ///   used in a future, then it will reloaded automatically.
      /// </summary>
      procedure ClearCache    ; virtual;

      /// <summary>
      ///   Draw the symbol.
      /// </summary>
      /// <param name="_x">
      ///   draw origin
      /// </param>
      /// <param name="_y">
      ///   draw origin
      /// </param>
      procedure Draw          ( const _x       : Integer ;
                                const _y       : Integer
                              ) ; overload; virtual;

      /// <summary>
      ///   Draw the symbol.
      /// </summary>
      /// <param name="_x">
      ///   draw origin
      /// </param>
      /// <param name="_y">
      ///   draw origin
      /// </param>
      procedure Draw          ( const _x       : Single ;
                                const _y       : Single
                              ) ; overload; virtual;
    public

      /// <summary>
      ///   Name of the symbol.
      /// </summary>
      property Name         : String  read FName ;

      /// <summary>
      ///   If True, then symbol is file based - name is same as path.
      /// </summary>
      property IsFileBased  : Boolean read FIsFileBased ;

      /// <summary>
      ///   Size of the symbol in pixels. Can be more than the real size
      ///   because it is maximum size related to a center point.
      /// </summary>
      property Size         : Integer read FSize ;

      /// <summary>
      ///   Size of the symbol in original symbol units.
      /// </summary>
      property NativeSize    : Single read FNativeSize ;

      /// <summary>
      ///   Height of the symbol in original symbol units.
      /// </summary>
      property NativeHeight : Single read FNativeHeight ;

      /// <summary>
      ///   Width of the symbol in original symbol units.
      /// </summary>
      property NativeWidth  : Single read FNativeWidth ;

      /// <summary>
      ///   Width of the symbol in pixels. Can be more than the real width.
      /// </summary>
      property Width        : Integer read FWidth ;

      /// <summary>
      ///   Height of symbol in pixels. Can be more than the real height.
      /// </summary>
      property Height       : Integer read FHeight ;

      /// <summary>
      ///   Center point of the symbol in percent of the size. Used for
      ///   modifying center of the symbol (mostly for corrupted CGM's).
      /// </summary>
      property Center       : TPointF read FCenter write FCenter ;

      /// <summary>
      ///   Autocentering symbol in 50% of the size. Used for autocentering
      ///   of the symbol (mostly for corrupted CGM's).
      /// </summary>
      property AutoCenter : Boolean read FAutoCenter write fset_AutoCenter ;

      /// <summary>
      ///   Rectangle in original symbol units reserved for labels in a shield
      ///   symbols.
      ///   Used mostly for SVG Shields. If not defined in symbol then
      ///   represents whole symbol area.
      /// </summary>
      property ShieldLabel : TRectF read fget_ShieldLabel ;

      /// <summary>
      ///   Rectangle in original symbol units representing part of a symbol
      ///   that represent actual symbols. Used to avoid symbol white margins
      ///   to be used in labels overlapping aluclations,
      ///   Used mostly for SVG Shields. If not defined in symbol then
      ///   represents whole symbol area.
      /// </summary>
      property ShieldBounds : TRectF read fget_ShieldBounds ;

  end ;

  /// <summary>
  ///   Encapsulation of picture symbols.
  /// </summary>
  TGIS_SymbolPicture = {$IFDEF OXYGENE} public {$ENDIF}
                       class( TGIS_SymbolAbstract )
    private
      FBitmap : TGIS_Bitmap ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <inheritdoc/>
      function getRealSymWidth  : Integer ; override;

      /// <inheritdoc/>
      function getRealSymHeight : Integer ; override;

    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure   doDestroy       ; override;

    public

      /// <summary>
      ///   Create a picture symbol.
      /// </summary>
      /// <param name="_name">
      ///   symbol name; Transparent by default; can be override by providing
      ///   name?FALSE Picture name and transparency in the form of:
      ///   path?[TRUE|FALSE]. Transparent by default.
      /// </param>
      constructor Create          ( const _name   : String
                                  ) ; overload;

      /// <summary>
      ///   Create a picture symbol from a file or the stream.
      /// </summary>
      /// <param name="_name">
      ///   symbol name; Transparent by default; can be override by providing
      ///   name?FALSE Picture name and transparency in the form of:
      ///   path?[TRUE|FALSE]. Transparent by default.
      /// </param>
      /// <param name="_stream">
      ///   graphics stream for picture bitmap.
      /// </param>
      constructor Create          ( const _name   : String ;
                                    {$IFDEF CLR}
                                      const _stream : Stream
                                    {$ELSE}
                                      const _stream : TStream
                                    {$ENDIF}
                                  ) ; overload;

      /// <inheritdoc/>
      /// <remarks>
      ///   See TGIS_SymbolPicture.Create for example.
      /// </remarks>
      procedure   Draw            ( const _x,_y : Integer
                                  ) ; override;
  end ;

  /// <summary>
  ///   Encapsulation of font symbols.
  /// </summary>
  TGIS_SymbolFont = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_SymbolAbstract )

    private
      fntFont    : TGIS_Font ;
      fntChar    : Char  ;

    private

      /// <summary>
      ///   Return font size. For sizes greater then 0 but smaller then 2
      ///   return value will always be true to avoid fonts to small to render.
      /// </summary>
      function    calcFontSize    : Integer ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <inheritdoc/>
      function getRealSymWidth  : Integer ; override;

      /// <inheritdoc/>
      function getRealSymHeight : Integer ; override;

      /// <inheritdoc/>
      procedure   prepareRotate   ( const _angle  : Single ;
                                    const _marker : Boolean
                                  ) ; override;

      /// <inheritdoc/>
      procedure   set_symbol_parameters
                                  ( _marker : Boolean
                                  ) ; override;

      /// <summary>
      ///   Set font of symbol.
      /// </summary>
      procedure   set_symbol_font ;

    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy     ; override;

    public

      /// <summary>
      ///   Create a symbol.
      /// </summary>
      /// <param name="_name">
      ///   Font name and attributes in the form of:
      ///   font_name:char_character_code[:bold][:italic][:underline]
      ///   default is 'Arial:?'
      /// </param>
      constructor Create          ( const _name : String
                                  ) ;

      /// <inheritdoc/>
      /// <remarks>
      ///   See TGIS_SymbolPicture.Create for similar example.
      /// </remarks>
      procedure Draw              ( const _x,_y : Integer
                                  ) ; override;

    public

      /// <summary>
      ///   assigned font.
      /// </summary>
      property Font : TGIS_Font read fntFont ;

      /// <summary>
      ///   assigned character.
      /// </summary>
      property Char : Char      read fntChar ;
  end ;

  /// <summary>
  ///   Encapsulation of CGM symbols.
  /// </summary>
  TGIS_SymbolCGM = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_SymbolAbstract )

    private
      FBinary  : Boolean  ;
      drawBuf  : TGIS_DrawBufF ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <inheritdoc/>
      function getSymWidth      : Integer ; override;

      /// <inheritdoc/>
      function getSymHeight     : Integer ; override;

      /// <inheritdoc/>
      function getRealSymWidth  : Integer ; override;

      /// <inheritdoc/>
      function getRealSymHeight : Integer ; override;

    private
      cgmStream        : TGIS_BaseStream ;
      metaStream       : TGIS_MemoryStream ;
      endMarker        : Boolean       ;
      binLength        : Integer       ;
      cgmPos           : Integer       ;
      {$IFDEF MANAGED}
        cgmMeta        : TObject       ;
      {$ELSE}
        cgmMeta        : Pointer       ;
      {$ENDIF}
      cgmEof           : Boolean       ;

      cgmVdcScale      : Single        ;

      cgmCOLRMODE      : Integer       ;
      cgmLINEWIDTHMODE : Integer       ;
      cgmEDGEWIDTHMODE : Integer       ;

      cgmVDCTYPE       : Boolean       ;
      cgmINTEGERPREC   : Integer       ;
      cgmREALPREC      : Integer       ;

      cgmVDCEXT        : TRect ;
      cgmVDCINTEGERPREC: Integer       ;
      cgmVDCREALPREC   : Integer       ;

      cgmCOLRTABLE     : array[0..255] of TGIS_Color ;
      cgmINTSTYLE      : TGIS_BrushStyle ;
      cgmFILLCOLR      : TGIS_Color ;
      cgmEDGECOLR      : TGIS_Color ;
      cgmEDGEWIDTH     : Integer ;
      cgmEDGEVIS       : Boolean ;
      cgmLINECOLR      : TGIS_Color ;
      cgmLINEWIDTH     : Integer ;

    private

      /// <summary>
      ///   Prepare an elliptic arc element (and store in metastream as a
      ///   polyline).
      /// </summary>
      function  ellipticArc         ( const _cx,_cy, _ax,_ay, _bx,_by,
                                            _sx,_sy, _ex,_ey : Integer
                                    ) : Integer ;

      /// <summary>
      ///   Get the token from Clear-Text CGM.
      /// </summary>
      function  getTokenClearText   : String   ;

      /// <summary>
      ///   Get command token.
      /// </summary>
      function  getTokenCommand     : Integer  ;

      /// <summary>
      ///   Get the byte token.
      /// </summary>
      function  getTokenByte        : Integer  ;

      /// <summary>
      ///   Get word token.
      /// </summary>
      function  getTokenWord        : Cardinal ;

      /// <summary>
      ///   Get number token.
      /// </summary>
      /// <param name="_float">
      ///   False - read as integer; True -read as float
      /// </param>
      /// <param name="_vdc">
      ///   False - read as normal value; True read as VDC
      /// </param>
      function  getTokenNumber      ( const _float : Boolean ;
                                      const _vdc   : Boolean
                                    ): Single ;

      /// <summary>
      ///   Get integer token.
      /// </summary>
      function  getTokenInteger     : Integer  ;

      /// <summary>
      ///   Get float token.
      /// </summary>
      function  getTokenFloat       : Single   ;

      /// <summary>
      ///   Get token VDC.
      /// </summary>
      function  getTokenVdc         : Integer  ;

      /// <summary>
      ///   Get token COLOR.
      /// </summary>
      function  getTokenColor       : TGIS_Color ;

      /// <summary>
      ///   Parse CGM file - build metaStream representation.
      /// </summary>
      procedure parseFile           ; overload;

      /// <summary>
      ///   Parse CGM file - build metaStream representation.
      /// </summary>
      /// <param name="_stream">
      ///   symbol stream
      /// </param>
      procedure parseFile           ( const _stream : TStream
                                    ) ; overload;

      /// <summary>
      ///   Parse single command.
      /// </summary>
      /// <param name="_command">
      ///   code of command to be parsed
      /// </param>
      procedure parseCommand        ( const _command : Integer ) ;

      /// <summary>
      ///   Parse line primitive.
      /// </summary>
      procedure prepareEntryLine    ;

      /// <summary>
      ///   Parse surface primitive.
      /// </summary>
      procedure prepareEntrySurface ;

      /// <summary>
      ///   Finish primitive.
      /// </summary>
      procedure finishEntry         ( const _cnt : Integer ) ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doANY           ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doVDCTYPE       ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doINTEGERPREC   ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doREALPREC      ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doMETAFILEREPL  ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doSCALEMODE     ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doCOLRMODE      ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doLINEWIDTHMODE ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doEDGEWIDTHMODE ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doVDCEXT        ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doVDCINTEGERPREC;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doVDCREALPREC   ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doCOLRTABLE     ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doINTSTYLE      ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doFILLCOLR      ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doEDGECOLR      ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doEDGEWIDTH     ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doEDGEVIS       ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doLINECOLR      ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doLINEWIDTH     ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doRECTANGLE     ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doPOLYGON       ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doPOLYGONSET    ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doLINE          ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doCIRCLE        ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doARCCTR        ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doELLIPSE       ;

      /// <summary>
      ///   Parse CGM element.
      /// </summary>
      procedure doELLIPARC      ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <inheritdoc/>
      procedure setPosition( const _position : TGIS_SymbolPosition ) ; override;

    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy     ; override;

    public

      /// <summary>
      ///   Create a CGM symbol.
      /// </summary>
      /// <param name="_name">
      ///   symbol name
      /// </param>
      constructor Create          ( const _name     : String
                                  ) ; overload;

      /// <summary>
      ///   Create a CGM symbol.
      /// </summary>
      /// <param name="_name">
      ///   symbol name or path
      /// </param>
      /// <param name="_stream">
      ///   symbol stream to parse
      /// </param>
      constructor Create          ( const _name     : String ;
                                    {$IFDEF CLR}
                                      const _stream : Stream
                                    {$ELSE}
                                      const _stream : TStream
                                    {$ENDIF}
                                  ) ; overload;

      /// <inheritdoc/>
      procedure   ClearCache      ; override;

      /// <inheritdoc/>
      /// <remarks>
      ///   See TGIS_SymbolPicture.Create for similar example.
      /// </remarks>
      procedure   Draw            ( const _x,_y : Integer
                                  ) ; override;

    public

      /// <summary>
      ///   True if CGM is in binary format; False if CGM is in text format.
      /// </summary>
      property Binary : Boolean  read FBinary  ;
  end ;

  /// <summary>
  ///   Encapsulation of line symbols.
  /// </summary>
  TGIS_SymbolLine = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_SymbolAbstract )

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      arStyle      : Array of Integer ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <inheritdoc/>
      function getSymSize       : Integer ; override;

      /// <inheritdoc/>
      function getRealSymWidth  : Integer ; override;

      /// <inheritdoc/>
      function getRealSymHeight : Integer ; override;

    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy     ; override;

    public

      /// <summary>
      ///   Create a CGM symbol.
      /// </summary>
      /// <param name="_name">
      ///   symbol name
      /// </param>
      constructor Create     ( const _name : String ) ;

      /// <inheritdoc/>
      /// <remarks>
      ///   Do nothing - just for save inheritance
      /// </remarks>
      procedure   Draw       ( const _x,_y : Integer
                             ) ; override;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Internal class for TGIS_SymbolLineEx.
  /// </summary>
  T_LineEntry = {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
    /// <summary>
    ///   Field no 1.
    /// </summary>
    tp     : ShortInt ;

    /// <summary>
    ///   Field no 2.
    /// </summary>
    w      : Single   ;

    /// <summary>
    ///   Field no 3.
    /// </summary>
    w2     : Single   ;

    /// <summary>
    ///   Field no 4.
    /// </summary>
    secant : Single   ;

    /// <summary>
    ///   Field no 5.
    /// </summary>
    angle  : Single   ;
  end;

  {#gendoc:hide}
  /// <summary>
  ///   Internal class for TGIS_SymbolLineEx.
  /// </summary>
  T_Command = {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
    /// <summary>
    ///   Field no 1.
    /// </summary>
    Token : SmallInt ;

    /// <summary>
    ///   Field no 2.
    /// </summary>
    Value : Integer ;
  end ;

  /// <summary>
  ///   Encapsulation of extended line symbols.
  /// </summary>
  TGIS_SymbolLineEx = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_SymbolAbstract )

    private
      arDrawBuf    : TGIS_DrawBufF ;
      sDefinition  : String  ;
      iDefinition  : Integer ;
      iTokenType   : Integer ;
      iTokenValue  : Integer ;
      lineLen      : Single ;
      lineBuf      : array of T_LineEntry ;
      arDef        : array of T_Command ;
      stLoop       : Integer ;

      refX    : Single ;
      refY    : Single ;
      refRot  : Single ; // shape segment rotation at the current point
      posX    : Single ; // current point position along the shape
      posY    : Single ; // current point position perpendicular the shape
      loopEnd : Single ; // loop length for repeated content
      symrefX : Single ;
      symrefY : Single ;
      symposX : Single ;
      symposY : Single ;
      symRot  : Single ; // shape segment rotation at the symbol origin

      curNode : Integer ;
      outBuf  : TGIS_DrawBufF ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <inheritdoc/>
      function getSymSize       : Integer ; override;

      /// <inheritdoc/>
      function getRealSymWidth  : Integer ; override;

      /// <inheritdoc/>
      function getRealSymHeight : Integer ; override;

    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy     ; override;

    private
      function  getChar        : Char ;
      procedure undoChar       ;

      function  getToken       : Boolean  ;
      procedure debug_token    ;

      procedure addDef         ( const _command : Integer;
                                 const _val     : Integer ) ;
      procedure parseLoopBegin ;
      procedure parseLoopEnd   ;
      procedure parseGoto      ;
      procedure parseMove      ;
      procedure parseLine      ;
      procedure parseDraw      ;
      procedure parseOutline   ;
      procedure parseFill      ;
      procedure parseColor     ;
      procedure parseWidth     ;
      procedure prepareLine    ( const _cnt : Integer
                               ) ;

      procedure clearBuffer    ;
      procedure drawPoint      ;
      procedure drawPointSym   ;

      /// <summary>
      ///   Move current position along the shape
      /// </summary>
      /// <param name="_dx">
      ///   relative position along the shape
      /// </param>
      /// <param name="_dy">
      ///   relative position perpendicular the shape
      /// </param>
      procedure movePosition   ( const _dx    : Single ;
                                 const _dy    : Single
                               ) ;
      procedure moveSymbolPosition
                               ( const _dx    : Single ;
                                 const _dy    : Single
                               ) ;

      /// <summary>
      ///   Set a symbol origin (used as a rotation point) at active position
      /// </summary>
      procedure setSymbolOrigin ;

      /// <summary>
      ///   Add point to the symbol. Actual point is added relatively to the
      ///   point rotation at setSymbolOrigin() call.
      /// </summary>
      /// <param name="_dx">
      ///   relative position along the shape at the origin point
      /// </param>
      /// <param name="_dy">
      ///   relative position perpendicular the shape at the origin point
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///     addSymbolPoint does not change active symbol position
      ///   </note>
      /// </remarks>
      procedure addSymbolPoint ( const _dx    : Single ;
                                 const _dy    : Single
                               ) ;

      /// <summary>
      ///   Add perpendicular point to the shape. Upon adding points we do not
      ///   care for how the line behaves on the shape crossing vertices.
      /// </summary>
      /// <param name="_dx">
      ///   relative position along the shape
      /// </param>
      /// <param name="_dy">
      ///   relative position perpendicular the shape
      /// </param>
      procedure addFreePoint   ( const _dx    : Single ;
                                 const _dy    : Single
                               ) ;
      procedure doDraw         ;
      procedure doLine         ( const _len   : Single ;
                                 const _off   : Single
                               ) ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <summary>
      ///   Draw a line on the canvas.
      /// </summary>
      /// <param name="_drawbuf">
      ///   line representation
      /// </param>
      /// <param name="_cnt">
      ///   number of parts in Viewer.DrawBuf
      /// </param>
      procedure DrawLine       ( const _drawbuf : TGIS_DrawBufF ;
                                 const _cnt     : Integer
                               ) ;

    public

      /// <summary>
      ///   Create a Line symbol.
      /// </summary>
      /// <param name="_name">
      ///   line symbol name
      /// </param>
      constructor Create       ( const _name  : String
                               ) ;

      /// <inheritdoc/>
      /// <remarks>
      ///   Does nothing - just for safe inheritance
      /// </remarks>
      procedure   Draw         ( const _x     : Integer ;
                                 const _y     : Integer
                               ) ; override;
  end ;

  /// <summary>
  ///   Encapsulation of SVG symbols.
  /// </summary>
  TGIS_SymbolSVG = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_SymbolAbstract )

    private
       oSvg : TObject ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <inheritdoc/>
      function getSymWidth      : Integer ; override;

      /// <inheritdoc/>
      function getSymHeight     : Integer ; override;

      /// <inheritdoc/>
      function getRealSymWidth  : Integer ; override;

      /// <inheritdoc/>
      function getRealSymHeight : Integer ; override;

    protected
      function fget_ShieldLabel : TRectF ; override ;
      function fget_ShieldBounds: TRectF ; override ;

    private

      /// <summary>
      ///   Parse SVG file - build metaStream representation.
      /// </summary>
      procedure parseFile           ; overload;

      /// <summary>
      ///   Parse SVG file - build metaStream representation.
      /// </summary>
      /// <param name="_stream">
      ///   symbol stream
      /// </param>
      procedure parseFile           ( const _stream : TStream
                                    ) ; overload;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <inheritdoc/>
      procedure setPosition( const _position : TGIS_SymbolPosition ) ; override;

    private
      procedure doCreate           ({$IFDEF CLR}
                                      const _stream : Stream
                                    {$ELSE}
                                      const _stream : TStream
                                    {$ENDIF}
                                  );
    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy         ; override;

    public

      /// <summary>
      ///   Create a SVG symbol.
      /// </summary>
      /// <param name="_name">
      ///   symbol name
      /// </param>
      constructor Create          ( const _name     : String
                                  ) ; overload;

      /// <summary>
      ///   Create a SVG symbol.
      /// </summary>
      /// <param name="_name">
      ///   symbol name or file
      /// </param>
      /// <param name="_stream">
      ///   symbol stream to parse
      /// </param>
      constructor Create          ( const _name     : String ;
                                    {$IFDEF CLR}
                                      const _stream : Stream
                                    {$ELSE}
                                      const _stream : TStream
                                    {$ENDIF}
                                  ) ; overload;

      /// <inheritdoc/>
      procedure   ClearCache      ; override;

      /// <inheritdoc/>
      /// <remarks>
      ///   See TGIS_SymbolPicture.Create for similar example.
      /// </remarks>
      procedure   Draw            ( const _x,_y : Integer
                                  ) ; override;

      /// <inheritdoc/>
      /// <remarks>
      ///   See TGIS_SymbolPicture.Create for similar example.
      /// </remarks>
      procedure   Draw            ( const _x,_y : Single
                                  ) ; override;
  end ;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Encapsulation of hatch definition.
  /// </summary>
  TGIS_HatchDefinition = {$IFDEF OXYGENE} public {$ENDIF} record
    public
      /// <summary>
      ///   Hatch rotation.
      /// </summary>
      Angle  : Double ;
      /// <summary>
      ///   Base point.
      /// </summary>
      Base   : TPointF ;
      /// <summary>
      ///   Offset point.
      /// </summary>
      Offset : TPointF ;
      /// <summary>
      ///   Dash line array.
      /// </summary>
      Dash   : TGIS_SingleArray;
  end ;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Encapsulation of hatch definition array.
  /// </summary>
  TGIS_HatchDefinitionArray = {$IFDEF OXYGENE} public {$ENDIF} TArray<TGIS_HatchDefinition> ;

  /// <summary>
  ///   Encapsulation of hatch fill symbol.
  /// </summary>
  TGIS_SymbolHatch = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_SymbolAbstract )
    private
      FHatches : TGIS_HatchDefinitionArray ;
    private
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      /// <inheritdoc/>
      function getSymSize       : Integer ; override;
      /// <inheritdoc/>
      function getRealSymWidth  : Integer ; override;
      /// <inheritdoc/>
      function getRealSymHeight : Integer ; override;
    private
      procedure parseDefinition ( const _lines : String
                                ) ;
      procedure drawPattern     ( const _renderer : TObject ;
                                  const _rect     : TRectF ;
                                  const _angle    : Double ;
                                  const _scale    : Double ;
                                  const _origin   : TPointF ;
                                  const _offset   : TPointF ;
                                  const _pattern  : TGIS_SingleArray
                                ) ;
    protected
      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy     ; override;
    public
      /// <summary>
      ///   Create a hatch symbol.
      /// </summary>
      /// <param name="_name">
      ///   hatch definition
      /// </param>
      constructor Create       ( const _name  : String
                               ) ;
      /// <inheritdoc/>
      procedure   Draw         ( const _x     : Integer ;
                                 const _y     : Integer
                               ) ; override;
  end ;

  /// <summary>
  ///   Internal use only. Helper for drawing line advanced symbology
  /// </summary>
  TGIS_SymbolLineHelper = {$IFDEF OXYGENE} public {$ENDIF} class
    private

      /// <summary>
      ///   Draw a line on the canvas using individual symbols. To be
      ///   used internally by TatukGIS.
      /// </summary>
      /// <param name="_viewer">
      ///   viewer object
      /// </param>
      /// <param name="_drawbuf">
      ///   array of line coordinates
      /// </param>
      /// <param name="_sym">
      ///   symbol to be drawn
      /// </param>
      /// <param name="_cnt">
      ///   number of parts in Viewer.DrawBuf
      /// </param>
      class  procedure drawFromSymbols(
                                    const _viewer  : IGIS_Viewer          ;
                                    const _drawbuf : TGIS_DrawBufF        ;
                                    const _sym     : TGIS_SymbolAbstract  ;
                                    const _cnt     : Integer
                                  );

      /// <summary>
      ///   Draw a line on the canvas using TGIS_SymbolLine. To be used
      ///   internally by TatukGIS.
      /// </summary>
      /// <param name="_viewer">
      ///   viewer object
      /// </param>
      /// <param name="_drawbuf">
      ///   array of line coordinates
      /// </param>
      /// <param name="_sym">
      ///   symbol to be drawn
      /// </param>
      /// <param name="_cnt">
      ///   number of parts in Viewer.DrawBuf
      /// </param>
      class procedure drawDashLine( const _viewer  : IGIS_Viewer         ;
                                    const _drawbuf : TGIS_DrawBufF       ;
                                    const _sym     : TGIS_SymbolAbstract ;
                                    const _cnt     : Integer
                                  );
    public

      /// <summary>
      ///   Draw a line on the canvas. To be used internally by TatukGIS.
      /// </summary>
      /// <param name="_viewer">
      ///   viewer object
      /// </param>
      /// <param name="_drawbuf">
      ///   line representation
      /// </param>
      /// <param name="_sym">
      ///   symbol to be drawn
      /// </param>
      /// <param name="_cnt">
      ///   number of parts in Viewer.DrawBuf
      /// </param>
      class procedure DrawLine    ( const _viewer  : IGIS_Viewer         ;
                                    const _drawbuf : TGIS_DrawBufF       ;
                                    const _sym     : TGIS_SymbolAbstract ;
                                    const _cnt     : Integer
                                  ) ;
  end;

  /// <summary>
  ///   Cached symbol list. Use SymbolList.Prepare to attach symbols to the
  ///   layers and shapes.
  /// </summary>
  /// <returns>
  ///   symbol list object
  /// </returns>
  function SymbolList : TGIS_SymbolList ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Math,
    System.IniFiles,
    System.SyncObjs,
    GisFunctions,
    GisInternals,
    GisFileSVG,
    GisParams,
    GisResource,
    GisRendererAbstract,
    GisLibrarySVG;
{$ENDIF}

var
  oSymbolList : TGIS_SymbolList = nil ;

const
  CGM_ENTRY_LINE        = 1 ;
  CGM_ENTRY_SURFACE     = 2 ;
  CGM_ENTRY_END         = 0 ;

  CGM_INTEGERPREC       = 'INTEGERPREC'   ;
  CGM_REALPREC          = 'REALPREC'      ;

  CGM_SCALEMODE         = 'SCALEMODE'     ;
  CGM_COLRMODE          = 'COLRMODE'      ;
  CGM_LINEWIDTHMODE     = 'LINEWIDTHMODE' ;
  CGM_EDGEWIDTHMODE     = 'EDGEWIDTHMODE' ;
  CGM_VDCINTEGERPREC    = 'VDCINTEGERPREC';
  CGM_VDCREALPREC       = 'VDCREALPREC'   ;
  CGM_VDCEXT            = 'VDCEXT'        ;
  CGM_COLRTABLE         = 'COLRTABLE'     ;
  CGM_INTSTYLE          = 'INTSTYLE'      ;
  CGM_FILLCOLR          = 'FILLCOLR'      ;

  CGM_EDGECOLR          = 'EDGECOLR'      ;
  CGM_EDGEWIDTH         = 'EDGEWIDTH'     ;
  CGM_EDGEVIS           = 'EDGEVIS'       ;
  CGM_LINECOLR          = 'LINECOLR'      ;
  CGM_LINEWIDTH         = 'LINEWIDTH'     ;

  CGM_RECTANGLE         = 'RECTANGLE'     ;
  CGM_RECT              = 'RECT'          ;
  CGM_POLYGON           = 'POLYGON'       ;
  CGM_LINE              = 'LINE'          ;
  CGM_CIRCLE            = 'CIRCLE'        ;
  CGM_ARCCTR            = 'ARCCTR'        ;
  CGM_ELLIPSE           = 'ELLIPSE'       ;
  CGM_ELLIPARC          = 'ELLIPARC'      ;

  CGM_ON                = 'ON'            ;
  CGM_OFF               = 'ON'            ;
  CGM_SOLID             = 'SOLID'         ;
  CGM_HOLLOW            = 'HOLLOW'        ;
  CGM_EMPTY             = 'EMPTY'         ;
  CGM_INDEXED           = 'INDEXED'       ;
  CGM_ABS               = 'ABS'           ;
  CGM_SCALED            = 'SCALED'        ;
  CGM_FRACTIONAL        = 'FRACTIONAL'    ;
  CGM_MM                = 'MM'            ;

  CGM_BIN_VDCTYPE       = 103 ;
  CGM_BIN_INTEGERPREC   = 104 ;
  CGM_BIN_REALPREC      = 105 ;
  CGM_BIN_METAFILEREPL  = 112 ;
  CGM_BIN_SCALEMODE     = 201 ;
  CGM_BIN_COLRMODE      = 202 ;
  CGM_BIN_LINEWIDTHMODE = 203 ;
  CGM_BIN_EDGEWIDTHMODE = 205 ;
  CGM_BIN_VDCEXT        = 206 ;
  CGM_BIN_VDCINTEGERPREC= 301 ;
  CGM_BIN_VDCREALPREC   = 302 ;
  CGM_BIN_RECTANGLE     = 411 ;
  CGM_BIN_POLYGON       = 407 ;
  CGM_BIN_POLYGONSET    = 408 ;
  CGM_BIN_LINE          = 401 ;
  CGM_BIN_CIRCLE        = 412 ;
  CGM_BIN_ARCCTR        = 415 ;
  CGM_BIN_ELLIPSE       = 417 ;
  CGM_BIN_ELLIPARC      = 418 ;
  CGM_BIN_LINEWIDTH     = 503 ;
  CGM_BIN_LINECOLR      = 504 ;
  CGM_BIN_INTSTYLE      = 522 ;
  CGM_BIN_FILLCOLR      = 523 ;
  CGM_BIN_EDGEWIDTH     = 528 ;
  CGM_BIN_EDGECOLR      = 529 ;
  CGM_BIN_EDGEVIS       = 530 ;
  CGM_BIN_COLRTABLE     = 534 ;

const
  TOKEN_ENDCOMMAND     =   1 ;

  TOKEN_LOOPBEGIN      =   2 ;
  TOKEN_LOOPEND        =   3 ;

  TOKEN_GOTO           =   4 ;
  TOKEN_MOVE           =   5 ;
  TOKEN_LINE           =   6 ;
  TOKEN_DRAW           =   7 ;
  TOKEN_OUTLINE        =   8 ;
  TOKEN_FILL           =   9 ;

  TOKEN_COLOR          =  10 ;
  TOKEN_WIDTH          =  11 ;

  TOKEN_VALUE_DEFAULT  =  20 ; // default - device independent pixel (96PPI)
  TOKEN_VALUE_PERCENT  =  21 ; // percentage of line length
  TOKEN_VALUE_PIXELS   =  22 ; // device independent pixel (96PPI)
  TOKEN_VALUE_TWIPS    =  23 ; // twip
  TOKEN_VALUE_WIDTH    =  24 ; // 1/10 of line width
  TOKEN_VALUE_SWIDTH   =  25 ; // scaled line width
  TOKEN_VALUE_SCRPIX   =  26 ; // screen pixel

  TOKEN_ERROR          = 998 ;
  TOKEN_UNKNOWN        = 999 ;

  ERR_SYNTAX = '999 Syntax %s, line %d' ;
  ERR_UNKNOWN_TOKEN = 'UNKNOWN TOKEN' ;
  ERR_BAD_ARGUMENT_COUNT = 'BAD NUMBER OF ARGUMENTS' ;

type

  // Encapsulation of CGM primitives - for internal use.
  T_SymbolCgmPrimitive = {$IFDEF OXYGENE} class {$ELSE} packed record {$ENDIF}
    {$IFDEF OXYGENE}
      public
    {$ENDIF}
      Kind         : Integer     ;
      BrushColor   : Cardinal     ;
      BrushStyle   : TGIS_BrushStyle ;
      PenColor     : Cardinal     ;
      PenWidth     : Integer     ;
      PenStyle     : TGIS_PenStyle ;
      Points       : Integer     ;
      {$IFDEF OXYGENE}
        // Read object from a stream.
        function Read  ( _stream : TGIS_BaseStream
                       ) : Integer ;
        // Write object to a stream.
        function Write ( _stream : TGIS_BaseStream
                       ) : Integer ;
      {$ENDIF}
  end ;

//==============================================================================
// Utility routines
//==============================================================================

  // Test for and calculate if exist all crossing point between circle and
  // line (vector).
  // _ptA    starting point of line
  // _ptB    ending point of line
  // _ptC    center point of circle
  // _radius radius of circle
  // _pt1    first crossing point, or undefined if not exist
  // _pt2    second crossing point, or undefined if not exist
  // return  0 - no crossings, 1 - one crossing or tangent,
  //         2 - two crossings
  function getLineCrossCircle(
    const _ptA    : TPointF ;
    const _ptB    : TPointF ;
    const _ptC    : TPointF ;
    const _radius : Single ;
    var   _pt1    : TPointF ;
    var   _pt2    : TPointF
  ) : Integer ;
  var
     aa, bb : Single ;
     A,B,C  : Single ;
     delta  : Single ;
     m2, r  : Single ;
     x      : Single ;
  begin
    {$IFDEF GIS_NORECORDS}
      _pt1 := new TPointF(0,0) ;
      _pt2 := new TPointF(0,0) ;
    {$ENDIF}
    if _ptB.X - _ptA.X = 0 then begin
      A:= 1;
      B:= (-2)*_ptC.Y;
      C:=  Sqr( _ptC.Y ) + Sqr( _ptA.X - _ptC.X ) - Sqr( _radius ) ;

      delta := Sqr( B ) - 4*A*C ;
      Result := 0;
      if delta = 0 then begin
        Result := 1;
        _pt1.X := _ptA.X;
        _pt1.Y := RoundS((-B)/(2*A));
      end ;
      if delta > 0 then begin
        Result := 2;
        _pt1.X := _ptA.X;
        _pt1.Y := RoundS(((-B )- Sqrt(delta))/(2*A));
        _pt2.X := _ptA.X;
        _pt2.Y := RoundS(((-B )+ Sqrt(delta))/(2*A));
      end;
    end
    else begin
      aa := 1.0 * (_ptB.Y - _ptA.Y) / (_ptB.X - _ptA.X) ;
      bb := _ptA.Y - aa*_ptA.X ;

      A:= Sqr( aa ) + 1 ;
      B:= 2*(aa*bb - _ptC.X - aa*_ptC.Y);
      C:= Sqr( bb ) - 2*bb*_ptC.Y + Sqr( _ptC.Y ) + Sqr( _ptC.X ) - Sqr( _radius ) ;

      delta := Sqr(B) - 4*A*C;
      Result := 0;
      if delta = 0 then begin
        Result := 1;
        x := (-B)/(2*A);
        _pt1.Y := aa*x + bb;
        _pt1.X := x ;
      end
      else if delta > 0 then begin
        Result := 2;

        x := ((-B )- Sqrt(delta))/(2*A);
        _pt1.Y := aa*x + bb;
        _pt1.X := x ;

        x := ((-B )+ Sqrt(delta))/(2*A);
        _pt2.Y := aa*x + bb;
        _pt2.X := x ;
      end
      else begin
        _pt1 := _TPointF( _ptC ) ;
        _pt2 := _TPointF( _ptC ) ;
      end ;
    end;

    if Result <= 0 then exit ;

    // check for being on vector
       m2 := Sqr( 1.0 * ( _ptB.X -_ptA.X ) ) +
             Sqr( 1.0 * ( _ptB.Y -_ptA.Y ) ) ;
       if m2 > 0 then begin
         r := 1.0 * ( (_ptA.Y - _pt1.Y) * (_ptA.Y -_ptB.Y) -
                      (_ptA.X - _pt1.X) * (_ptB.X -_ptA.X)
                    ) / m2 ;
         if (r < 0) or (r > 1) then begin
           dec( Result ) ;
          _pt1 := _TPointF( _pt2 ) ;

           r := 1.0 * ( (_ptA.Y - _pt1.Y) * (_ptA.Y -_ptB.Y) -
                        (_ptA.X - _pt1.X) * (_ptB.X -_ptA.X)
                      ) / m2 ;
           if (r < 0) or (r > 1) then begin
             dec( Result ) ;
           end ;
         end ;
         if Result = 2 then begin
           r := 1.0 * ( (_ptA.Y - _pt2.Y) * (_ptA.Y -_ptB.Y) -
                        (_ptA.X - _pt2.X) * (_ptB.X -_ptA.X)
                      ) / m2 ;
           if (r < 0) or (r > 1) then begin
             dec( Result ) ;
           end ;
         end ;
       end
       else
         Result := 0 ;

    // totally unexpected situation
       if ( (_ptC.X = _pt1.X) and (_ptC.Y = _pt1.Y) )
       then
         Result := 0 ;
  end ;

//==============================================================================
// Utility routines
//==============================================================================

{$IFDEF OXYGENE}
  function T_SymbolCgmPrimitive.Read (
    _stream : TGIS_BaseStream
  ) : Integer ;
  var
    i : Integer ;
  begin
    Result := _stream.ReadInteger( Kind ) ;
    Result := Result + _stream.ReadCardinal( BrushColor ) ;
    Result := Result + _stream.ReadInteger( i ) ;

    case i of
      0 : BrushStyle := TGIS_BrushStyle.Solid      ;
      1 : BrushStyle := TGIS_BrushStyle.Clear      ;
      2 : BrushStyle := TGIS_BrushStyle.Horizontal ;
      3 : BrushStyle := TGIS_BrushStyle.Vertical   ;
      4 : BrushStyle := TGIS_BrushStyle.FDiagonal  ;
      5 : BrushStyle := TGIS_BrushStyle.BDiagonal  ;
      6 : BrushStyle := TGIS_BrushStyle.Cross      ;
      7 : BrushStyle := TGIS_BrushStyle.DiagCross
    else  BrushStyle := TGIS_BrushStyle.Solid      ;
    end ;

    Result := Result + _stream.ReadCardinal( PenColor ) ;
    Result := Result + _stream.ReadInteger( PenWidth ) ;
    Result := Result + _stream.ReadInteger( i ) ;

    case i of
      0 : PenStyle := TGIS_PenStyle.Solid       ;
      1 : PenStyle := TGIS_PenStyle.Dash        ;
      2 : PenStyle := TGIS_PenStyle.Dot         ;
      3 : PenStyle := TGIS_PenStyle.DashDot     ;
      4 : PenStyle := TGIS_PenStyle.DashDotDot  ;
      5 : PenStyle := TGIS_PenStyle.Clear       ;
    else  PenStyle := TGIS_PenStyle.Solid       ;
    end ;

    Result := Result + _stream.ReadInteger( Points ) ;
  end ;

  function T_SymbolCgmPrimitive.Write (
    _stream : TGIS_BaseStream
  ) : Integer ;
  var
    i : Integer ;
  begin
    Result := 0 ;
    _stream.WriteInteger( Kind ) ;
    _stream.WriteCardinal( BrushColor ) ;
    case BrushStyle of
      TGIS_BrushStyle.Solid       : i := 0 ;
      TGIS_BrushStyle.Clear       : i := 1 ;
      TGIS_BrushStyle.Horizontal  : i := 2 ;
      TGIS_BrushStyle.Vertical    : i := 3 ;
      TGIS_BrushStyle.FDiagonal   : i := 4 ;
      TGIS_BrushStyle.BDiagonal   : i := 5 ;
      TGIS_BrushStyle.Cross       : i := 6 ;
      TGIS_BrushStyle.DiagCross   : i := 7
    else                            i := 0 ;
    end ;
    Result := Result + _stream.WriteInteger( i ) ;

    _stream.WriteCardinal( PenColor ) ;
    _stream.WriteInteger( PenWidth ) ;

    case PenStyle of
      TGIS_PenStyle.Solid       : i := 0 ;
      TGIS_PenStyle.Dash        : i := 1 ;
      TGIS_PenStyle.Dot         : i := 2 ;
      TGIS_PenStyle.DashDot     : i := 3 ;
      TGIS_PenStyle.DashDotDot  : i := 4 ;
      TGIS_PenStyle.Clear       : i := 5 ;
    else                          i := 0 ;
    end;
    Result := Result + _stream.WriteInteger( i ) ;

    _stream.WriteInteger( Points ) ;
  end ;
{$ENDIF}

//==============================================================================
// TGIS_SymbolList
//==============================================================================

  constructor TGIS_SymbolList.Create ;
  begin
    inherited ;

    symbolList := TDictionary<String,TGIS_SymbolAbstract>.Create ;
  end ;

  procedure TGIS_SymbolList.doDestroy ;
  begin
    Clear ;
    FreeObject( symbolList ) ;
    inherited ;
  end ;

  procedure TGIS_SymbolList.Clear ;
  {$IFNDEF OXYGENE}
    var
      itm : TPair<String,TGIS_SymbolAbstract> ;
  {$ENDIF}
  begin
    LockThread ;
    try
      {$IFNDEF NEXTGEN}
        for itm in symbolList do
          FreeObjectNotNil( itm.Value ) ;
      {$ENDIF}

      symbolList.Clear ;
    finally
      UnlockThread ;
    end;
  end ;

  procedure TGIS_SymbolList.ClearCache ;
  {$IFNDEF OXYGENE}
    var
      itm : TPair<String,TGIS_SymbolAbstract> ;
  {$ENDIF}
  begin
    LockThread ;
    try
      for itm in symbolList do
        TGIS_SymbolAbstract(itm.Value).ClearCache ;
    finally
      UnlockThread ;
    end;
  end ;

  function TGIS_SymbolList.Prepare(
    const _name    : String
  ) : TGIS_SymbolAbstract ;
  begin
    Result := Prepare( _name, nil ) ;
  end;

  function TGIS_SymbolList.Prepare(
    const _name    : String ;
    {$IFDEF CLR}
      const _stream  : Stream
    {$ELSE}
      const _stream  : TStream
    {$ENDIF}
  ) : TGIS_SymbolAbstract ;
  var
    sname : String  ;
    spath : String  ;
    smode : String  ;
    obj   : TGIS_SymbolAbstract ;
    ext   : String ;
    lst   : TStringList ;
    tkn   : TGIS_Tokenizer ;
  begin
    LockThread ;
    try
      Result := nil ;
      obj := nil ;

      if IsStringEmpty( _name ) then exit ;

      ext  := UpperCase( GetFileExt( _name ) ) ;
      if Pos( String( 'LIBSVG:' ), _name ) = StringFirst then begin
        sname := _name ;
        spath := _name ;
      end
      else if Pos( GIS_PARAMTXT_TYPE_HATCH, _name ) = StringFirst then begin
        sname := _name ;
        spath := _name ;
      end
      else if Pos( String( '<' ), _name ) = StringFirst then begin
        sname := _name ;
        spath := _name ;
      end
      else if IsStringEmpty( ext ) then begin
        sname := _name ;
        spath := _name ;
      end
      else begin
        tkn := TGIS_Tokenizer.Create ;
        try
          tkn.Execute( _name, ['?'], True ) ;
          if tkn.Result.Count > 0 then
            spath := tkn.Result[0]
          else
            spath := '' ;
          if tkn.Result.Count > 1 then
            smode := '?' + tkn.Result[1]
          else
            smode := '' ;
        finally
          FreeObject( tkn ) ;
        end;
        sname := GetPathAbsolute( '', spath ) + smode ;
        ext := UpperCase( GetFileExt( spath ) ) ;
        {$IFDEF MSWINDOWS}
        spath := UpperCase( spath ) ;
        {$ENDIF}
      end;

      if not symbolList.TryGetValue( sname, Result ) then begin
        try
          if assigned( _stream ) then begin
            if ext = '.SYM' then begin
              lst := TStringList.Create ;
              try
                lst.LoadFromStream( _stream ) ;
                obj := TGIS_SymbolLineEx.Create( '&' + lst.Text ) ;
                obj.FName := sname ;
              finally
                FreeObject( lst ) ;
              end;
            end
            else if ext = '.CGM' then
              obj := TGIS_SymbolCGM.Create( sname, _stream )
            else if (ext = '.SVG') or (ext = '.SVGZ') then
              obj := TGIS_SymbolSVG.Create( sname, _stream )
            else
              obj := TGIS_SymbolPicture.Create( sname, _stream ) ;
          end
          else if Pos( String( 'LIBSVG:' ), sname ) = StringFirst then
            obj := TGIS_SymbolLibrarySVG.Prepare( sname )
          else if Pos( String( '<' ), sname ) = StringFirst then
            obj := TGIS_SymbolLine.Create( sname )
          else if Pos( String( '&' ), sname ) = StringFirst then
            obj := TGIS_SymbolLineEx.Create( sname )
          else if Pos( String( 'HATCH:' ), sname ) = StringFirst then
            obj := TGIS_SymbolHatch.Create( sname )
          else if IsStringEmpty( ext ) then begin
            if Pos( ':', sname ) > StringFirst then
              obj := TGIS_SymbolFont.Create( sname )
          end
          else begin
            if ext = '.SYM' then begin
              lst := TStringList.Create ;
              try
                if _stream = nil then
                  lst.LoadFromFile( sname )
                else
                  lst.LoadFromStream( _stream ) ;

                obj := TGIS_SymbolLineEx.Create( '&' + lst.Text ) ;
                obj.FName := sname ;
              finally
                FreeObject( lst ) ;
              end;
            end
            else if ext = '.CGM' then begin
              if _stream = nil then
                obj := TGIS_SymbolCGM.Create( sname )
              else
                obj := TGIS_SymbolCGM.Create( sname, _stream )
            end
            else if (ext = '.SVG') or (ext = '.SVGZ') then begin
              if _stream = nil then
                obj := TGIS_SymbolSVG.Create( sname )
              else
                obj := TGIS_SymbolSVG.Create( sname, _stream )
            end
            else begin
              if _stream = nil then
                obj := TGIS_SymbolPicture.Create( sname )
              else
                obj := TGIS_SymbolPicture.Create( sname, _stream ) ;
            end;
          end ;
          if assigned( obj ) then
            symbolList.Add( sname, obj ) ;

          Result := obj ;
        except
        end ;
      end ;

    finally
      UnlockThread ;
    end;
  end ;

  procedure TGIS_SymbolList.InternalDelete(
    const _name : String
  ) ;
  var
    {$IFNDEF OXYGENE}
      itm : TPair<String,TGIS_SymbolAbstract> ;
    {$ENDIF}
    sym   : TGIS_SymbolAbstract ;
  begin
    LockThread ;
    try
      {$IFNDEF NEXTGEN}
        sym := nil ;
        for itm in symbolList do
          if itm.Key = _name then begin
            sym := itm.Value ;
            break ;
          end ;
      {$ENDIF}

      symbolList.Remove( _name ) ;

      {$IFNDEF NEXTGEN}
        if assigned( sym ) then
          FreeObjectNotNil( sym ) ;
      {$ENDIF}
    finally
      UnlockThread ;
    end;
  end ;

  procedure TGIS_SymbolList.InternalDelete(
    const _symbol : TGIS_SymbolAbstract
  ) ;
  begin
    assert( assigned( _symbol ) ) ;
    LockThread ;
    try
      symbolList.Remove( _symbol.Name ) ;
    finally
      UnlockThread ;
    end;
  end ;

//==============================================================================
// TGIS_Symbol
//==============================================================================

  constructor TGIS_SymbolAbstract.Create(
    const _name : String
  ) ;
  var
    ini : TMemIniFile ;
  begin
    inherited Create ;

    oThread      := TGIS_ThreadClass.Create ;

    FName        := _name ;
    FIsFileBased := False ;
    FAutoCenter  := False ;
    FNativeSize  := 1 ;
    FNativeWidth := 1 ;
    FNativeHeight:= 1 ;
    symScale     := 1 ;
    symScaleX    := 1 ;

    {$IFDEF GIS_NORECORDS}
      FCenter := new TPointF ;
    {$ENDIF}
    if IsServerPath( _name ) or
       IsStringEmpty( GetFileExt( _name ) ) then begin
      FCenter.X := 0 ;
      FCenter.Y := 0 ;
    end
    else begin
      ini := TMemIniFile.Create( _name + GIS_INI_EXT ) ;
      try
        FCenter.X := DotStrToFloat(
                       ini.ReadString( GIS_INI_SYMBOL_HEADER,
                                       GIS_INI_SYMBOL_CENTERX,
                                       '0'
                                     )
                     ) ;
        FCenter.Y := DotStrToFloat(
                       ini.ReadString( GIS_INI_SYMBOL_HEADER,
                                       GIS_INI_SYMBOL_CENTERY,
                                       '0'
                                     )
                     ) ;
      finally
        FreeObject( ini ) ;
      end ;
    end ;
  end ;

  procedure TGIS_SymbolAbstract.doDestroy ;
  begin
    FreeObject( oThread ) ;
    inherited ;
  end;

  function TGIS_SymbolAbstract.getSymSize
    : Integer ;
  begin
    Result := Max( Width, Height ) ;
  end ;

  function TGIS_SymbolAbstract.getSymWidth
    : Integer ;
  begin
    Result := getRealSymWidth ;
  end ;

  function TGIS_SymbolAbstract.getSymHeight
    : Integer ;
  begin
    Result := getRealSymHeight ;
  end ;

  procedure TGIS_SymbolAbstract.set_symbol_parameters(
    _marker : Boolean
  ) ;
  begin
    FWidth  := getSymWidth  ;
    FHeight := getSymHeight ;
    FSize   := getSymSize   ;
    prepareRotate( symAngle, _marker ) ;
  end ;

  function TGIS_SymbolAbstract.toPixelI(
    const _value : Single
  ) : Integer ;
  begin
    Result := RoundS( _value * symScale ) ;
  end ;

  function TGIS_SymbolAbstract.toPixelF(
    const _value : Single
  ) : Double ;
  begin
    Result := _value * symScale ;
  end ;

  function TGIS_SymbolAbstract.fromPixel(
    const _value : Single
  ) : Integer ;
  begin
    if symScale = 0 then
      Result := 0
    else
      Result := RoundS( _value / symScale ) ;
  end ;

  function TGIS_SymbolAbstract.scaleAndRotate(
    const _pt : TPoint
  ) : TPointF ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TPointF(0,0) ;
    {$ENDIF}
    Result.X := toPixelF( _pt.X * symCos  -  _pt.Y * symSin ) ;
    Result.Y := toPixelF( _pt.X * symSin  +  _pt.Y * symCos ) ;
  end ;

  procedure TGIS_SymbolAbstract.setPosition(
    const _position : TGIS_SymbolPosition
  ) ;
  begin
    case _position of
      TGIS_LabelPosition.UpLeft       : begin
         symOffset := Point(
                        - Width div 2 ,
                        - Height div 2
                      ) ;
      end ;
      TGIS_LabelPosition.UpCenter     : begin
       symOffset := Point(
                     0, - Height div 2
                    ) ;
      end ;
      TGIS_LabelPosition.UpRight      : begin
        symOffset := Point(
                         Width div 2,
                       - Height div 2
                     ) ;
      end ;
      TGIS_LabelPosition.MiddleLeft   : begin
        symOffset := Point(
                       - Width div 2, 0
                     ) ;
      end ;
      TGIS_LabelPosition.MiddleCenter : begin
        symOffset := Point(
                       0, 0
                     ) ;
      end ;
      TGIS_LabelPosition.MiddleRight  : begin
        symOffset := Point(
                       Width div 2, 0
                     ) ;
      end ;
      TGIS_LabelPosition.DownLeft     : begin
        symOffset := Point(
                       - Width div 2,
                       Height div 2
                     ) ;
      end ;
      TGIS_LabelPosition.DownCenter   : begin
        symOffset := Point(
                       0, Height div 2
                     ) ;
      end ;
      TGIS_LabelPosition.DownRight    : begin
        symOffset := Point(
                       Width div 2,
                       Height div 2
                     ) ;
      end ;
    end ;

    if Width <> 0 then
      symOffset := Point( symOffset.X +
                          RoundS( 1.0 * FCenter.X * Size  / 100 ),
                          symOffset.Y
                        ) ;
    if Height <> 0 then
      symOffset := Point( symOffset.X,
                          symOffset.Y +
                          RoundS( 1.0 * FCenter.Y * Size  / 100 )
                        ) ;
  end ;

  procedure TGIS_SymbolAbstract.fset_AutoCenter(
    const _value : Boolean
  ) ;
  begin
    FAutoCenter := _value ;
  end;

  function TGIS_SymbolAbstract.fget_ShieldLabel
    : TRectF ;
  begin
    Result := RectF( 0, 0, NativeWidth, NativeHeight ) ;
  end;

  function TGIS_SymbolAbstract.fget_ShieldBounds
    : TRectF ;
  begin
    Result := RectF( 0, 0, NativeWidth, NativeHeight ) ;
  end;

  function TGIS_SymbolAbstract.realizeColor1(
    const _color : TGIS_Color
  ) : TGIS_Color ;
  begin
    if    ( symColor1.ARGB <> TGIS_Color.RenderColor.ARGB ) then
            Result := symColor1
    else    Result := _color ;
  end ;

  function TGIS_SymbolAbstract.realizeColor2(
    const _color : TGIS_Color
  ) : TGIS_Color ;
  begin
    if    ( symColor2.ARGB <> TGIS_Color.RenderColor.ARGB ) then
            Result := symColor2
    else    Result := _color ;
  end ;

  procedure TGIS_SymbolAbstract.prepareRotate(
    const _angle  : Single ;
    const _marker : Boolean
  ) ;
  begin
    if assigned(symViewer) then
      symRot     := IGIS_Viewer(symViewer).RotationAngle
    else
      symRot     := 0 ;
    if _marker then
      symFullRot := _angle
    else
      symFullRot := _angle + symRot ;

    SinCos( symFullRot, symSin, symCos ) ;

    setPosition( symPosition ) ;
    symOffset := Point(
                   RoundS( symOffset.X * symCos  -  symOffset.Y * symSin ),
                   RoundS( symOffset.X * symSin  +  symOffset.Y * symCos )
                 ) ;
  end ;

  {$IFDEF CLR}
    procedure TGIS_SymbolAbstract.Prepare(
      const _viewer   : IGIS_Viewer ;
      const _size     : Integer ;
      const _color1   : TGIS_Color ;
      const _color2   : TGIS_Color ;
      const _angle    : Double  ;
      const _gap      : Integer ;
      const _position : TGIS_SymbolPosition ;
      const _marker   : Boolean
    ) ;
    begin
      Prepare( _viewer, _size, _color1, _color2,
               _angle, _gap, _position, _marker, nil
             ) ;
    end ;
  {$ELSE}
    procedure TGIS_SymbolAbstract.Prepare(
      const _viewer   : IGIS_Viewer ;
      const _size     : Integer ;
      const _color1   : TGIS_Color ;
      const _color2   : TGIS_Color ;
      const _angle    : Double  ;
      const _gap      : Integer ;
      const _position : TGIS_SymbolPosition ;
      const _marker   : Boolean
    ) ;
    begin
      Prepare( _viewer, _size, _color1, _color2,
               _angle, _gap, _position, _marker, nil
             ) ;
    end ;
  {$ENDIF}

  procedure TGIS_SymbolAbstract.Prepare(
    const _viewer    : IGIS_Viewer ;
    const _size      : Integer ;
    const _color1    : TGIS_Color ;
    const _color2    : TGIS_Color ;
    const _angle     : Double  ;
    const _gap       : Integer ;
    const _position  : TGIS_SymbolPosition ;
    const _marker    : Boolean ;
    const _renderer2 : TObject
  ) ;
  begin
    oThread.LockThread ;

    symViewer := _viewer;
    if assigned( _renderer2 )
      then symRenderer := _renderer2
      else symRenderer := _viewer.ViewerParent.ControlRenderer ;

    symSize   := TGIS_RendererAbstract( symRenderer ).TwipsToPixels( _size ) ;
    symColor1 := _color1 ;
    symColor2 := _color2 ;
    symAngle  := _angle ;
    symGap    := TGIS_RendererAbstract( symRenderer ).TwipsToPixels( _gap ) ;
    symPosition := _position ;

    if ( _size <> 0 ) and ( FNativeSize <> 0 ) then
            begin
              symScale := symSize / FNativeSize ;
            end
    else if ( _size =  0 ) then
            begin
              symSize  := 0 ;
              symScale := 1   ;
            end
    else begin
      symSize  := 280 ;
      symScale := 1   ;
    end ;

    symScaleX := 1 ;

    set_symbol_parameters ( _marker ) ;
  end ;

  procedure TGIS_SymbolAbstract.Prepare(
    const _viewer    : IGIS_Viewer ;
    const _size      : Integer     ;
    const _scaleX    : Double      ;
    const _color1    : TGIS_Color  ;
    const _color2    : TGIS_Color  ;
    const _angle     : Double      ;
    const _gap       : Integer     ;
    const _position  : TGIS_SymbolPosition ;
    const _marker    : Boolean     ;
    const _renderer2 : TObject
  ) ;
  begin
    Prepare( _viewer,
             _size,
             _color1, _color2,
             _angle,
             _gap,
             TGIS_SymbolPosition.MiddleCenter,
             False,
             nil
           ) ;
    symScaleX := _scaleX ;
  end;

  procedure TGIS_SymbolAbstract.Unprepare ;
  begin
    symViewer := nil ;
    symRenderer := nil ;

    oThread.UnlockThread ;
  end ;

  procedure TGIS_SymbolAbstract.ClearCache ;
  begin
    // do nothing
  end ;

  procedure TGIS_SymbolAbstract.Draw(
    const _x : Integer ;
    const _y : Integer
  ) ;
  begin
    // do noting
  end ;

  procedure TGIS_SymbolAbstract.Draw(
    const _x : Single ;
    const _y : Single
  ) ;
  begin
    Draw( Integer(TruncS( _x )), Integer(TruncS( _y )) ) ;
  end ;

//==============================================================================
// TGIS_SymbolFont
//==============================================================================

  constructor TGIS_SymbolFont.Create( const _name : String ) ;
  var
    i      : Integer ;
    params : String  ;
    path   : String  ;
    tkn    : TGIS_Tokenizer ;
    name   : String ;

    function get_char( _idx : Integer ) : {$IFNDEF JAVA OR ISLAND}System.{$ENDIF}Char ;
    var
      arr : TBytes ;
    begin
      SetLength( arr, 2 ) ;
      arr[0] := ( _idx and $000000FF ) ;
      arr[1] := ( _idx and $0000FF00 ) shr 8 ;
      {$IFDEF JAVA OR ISLAND}
        Result := TEncoding.UTF16LE.GetString( arr, 0, 2 )[StringFirst] ;
      {$ELSE}
        Result := TEncoding.Unicode.GetString( arr, 0, 2 )[StringFirst] ;
      {$ENDIF}
    end;

  begin
    inherited Create( _name ) ;

    if IsStringEmpty( _name ) then path := 'Arial:?'
                              else path := _name     ;

    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.Execute( path, [':'], True ) ;
      if tkn.Result.Count >= 1 then name := tkn.Result[0] ;
      if tkn.Result.Count >= 2 then begin
                                      if length( tkn.Result[1] ) = 1 then
                                        fntChar := tkn.Result[1][StringFirst]
                                      else
                                        try
                                          fntChar :=
                                            get_char( StrToInt(tkn.Result[1]) ) ;
                                        except
                                        end ;
                                    end ;
      params := '' ;
      for i := 2 to tkn.Result.Count - 1 do begin
        params := params + tkn.Result[i] ;
        if i < tkn.Result.Count -1 then
          params := params + ':' ;
      end ;
    finally
      FreeObject( tkn ) ;
    end ;

    fntFont       := TGIS_Font.Create ;
    fntFont.Size  := 8 ;
    fntFont.Name  := name ;
    fntFont.Style := ParamFontStyle( params, GisGetEmptyFontStyle ) ;

    FWidth  := getRealSymWidth ;
    FHeight := getRealSymHeight ;
  end ;

  procedure TGIS_SymbolFont.doDestroy ;
  begin
    FreeObject( fntFont ) ;
    inherited ;
  end ;

  function TGIS_SymbolFont.calcFontSize
    : Integer ;
  begin
    if symSize = 0 then
      Result := 0
    else
      Result := Max(
                  2,
                  TGIS_RendererAbstract( symRenderer ).PixelsToTwips( TruncS(symSize) )
                  * 72 div 1440
                ) ;
  end ;

  function TGIS_SymbolFont.getRealSymWidth
    : Integer ;
  var
    sz : Integer ;
    pt : TPoint ;
  begin
    sz := calcFontSize ;

    if ( symRenderer <> nil ) and ( sz <> 0 ) then begin
      fntFont.Size := sz ;
      set_symbol_font ;
    end ;

    if sz = 0 then
      Result := 0
    else if symRenderer <> nil then
    begin
      pt := TGIS_RendererAbstract( symRenderer ).CanvasTextExtent( fntChar ) ;
      Result := pt.X ;
    end
    else
      Result := 1 ;
  end ;

  function TGIS_SymbolFont.getRealSymHeight
    : Integer ;
  var
    sz : Integer ;
    pt : TPoint ;
  begin
    sz := calcFontSize ;

    if ( symRenderer <> nil ) and ( sz <> 0 ) then begin
      fntFont.Size := sz ;
      set_symbol_font ;
    end ;

    if sz = 0 then
      Result := 0
    else if symRenderer <> nil then
    begin
      pt := TGIS_RendererAbstract( symRenderer ).CanvasTextExtent( fntChar ) ;
      Result := pt.Y ;
    end
    else
      Result := 1 ;
  end ;

  procedure TGIS_SymbolFont.prepareRotate(
    const _angle  : Single ;
    const _marker : Boolean
  ) ;
  var
    sz     : Integer ;
  begin
    inherited ;

    symFinalRot := - ( symFullRot ) ;

    if symFinalRot < 0 then symFinalRot := 2*Pi + symFinalRot ;

    sz := calcFontSize ;

    if ( symRenderer <> nil ) and ( sz <> 0 ) then begin
      fntFont.Size := sz ;
      set_symbol_font ;
    end ;
  end ;

  procedure TGIS_SymbolFont.set_symbol_parameters(
    _marker : Boolean
  ) ;
  var
    sz    : Integer ;
    ext   : TPoint  ;
    angle : Single  ;

  begin
    sz := calcFontSize ;
    if ( symRenderer <> nil ) and ( sz <> 0 ) then begin
      fntFont.Size := sz ;
      set_symbol_font ;
    end ;

    if sz = 0 then begin
      FWidth  := 0 ;
      FHeight := 0 ;
    end
    else if symRenderer <> nil then begin
      ext := TGIS_RendererAbstract( symRenderer ).CanvasTextExtent( fntChar ) ;
      FWidth  := ext.X ;
      FHeight := ext.Y ;
    end
    else begin
      FWidth  := 1 ;
      FHeight := 1 ;
    end ;
    FSize := Max( Width, Height ) ;

    // prepareRotate
    inherited prepareRotate( symAngle, _marker ) ;
    angle := - ( symFullRot ) ;
    if angle < 0 then angle := 2*Pi + angle ;
    symFinalRot := angle ;
  end ;

  procedure TGIS_SymbolFont.set_symbol_font ;
  var
    rnd : TGIS_RendererAbstract ;
  begin
    rnd := TGIS_RendererAbstract( symRenderer ) ;

    rnd.CanvasFont.Name  := fntFont.Name  ;
    rnd.CanvasFont.Style := fntFont.Style ;
    rnd.CanvasFont.Size  := fntFont.Size  ;
  end ;

  procedure TGIS_SymbolFont.Draw(
    const _x : Integer ;
    const _y : Integer
  ) ;
  var
    pixels     : Integer ;
    pixelmove  : Integer ;
    x,y        : Integer ;
    rct        : TRect   ;
    rnd        : TGIS_RendererAbstract ;
  begin
    if symSize < 1 then exit ;

    rnd := TGIS_RendererAbstract( symRenderer ) ;
    rnd.CanvasBrush.Style := TGIS_BrushStyle.Clear ;

    if symFinalRot <> 0 then begin
      rnd.CanvasSetTransformation( -symFinalRot, _x, _y ) ;
      x := - ( Width  div 2 ) ;
      y := - ( Height div 2 ) ;
    end else begin
      x := _x - ( Width  div 2 ) ;
      y := _y - ( Height div 2 ) ;
    end ;

    try

      if symColor1.ARGB <> symColor2.ARGB then begin
        // draw outline

        // prepare size of outline in pixels
        // generally outline must be 1 screen pixel (1/96 of inch)
        // for hi DPI (like printes) outline will be a bit stronger then usual
        if rnd.PPI <> 96 then
          pixels := rnd.PPI div 96
        else
          pixels := 1 ;

        rnd.CanvasFont.Color := symColor2 ;
        for pixelmove := 1 to pixels do begin
          rct := Rect( x - pixelmove        , y                     ,
                       x - pixelmove + Width, y             + Height ) ;
          rnd.CanvasDrawText( rct , fntChar ) ;

          rct := Rect( x + pixelmove        , y                     ,
                       x + pixelmove + Width, y             + Height ) ;
          rnd.CanvasDrawText( rct , fntChar ) ;

          rct := Rect( x                    , y - pixelmove         ,
                       x             + Width, y - pixelmove + Height ) ;
          rnd.CanvasDrawText( rct , fntChar ) ;

          rct := Rect( x                    , y + pixelmove         ,
                       x             + Width, y + pixelmove + Height ) ;
          rnd.CanvasDrawText( rct , fntChar ) ;
        end ;
      end ;

      rnd.CanvasFont.Color := symColor1 ;
      rct := Rect( x, y, x + Width, y + Height ) ;
      rnd.CanvasDrawText( rct, fntChar ) ;

    finally
      if symFinalRot <> 0 then
        rnd.CanvasClearTransformation ;
    end ;

  end ;

//==============================================================================
// TGIS_SymbolPicture
//==============================================================================

  constructor TGIS_SymbolPicture.Create( const _name : String ) ;
  var
    tkn : TGIS_Tokenizer ;
  begin
    inherited Create( GetPathAbsolute( '', _name ) );
    FIsFileBased := True ;

    FBitmap  := TGIS_Bitmap.Create ;
    try
      tkn := TGIS_Tokenizer.Create ;
      try
        tkn.Execute( _name, ['?'], True ) ;

        if tkn.Result.Count >= 1 then begin
          if SafeFileExists( tkn.Result[0] ) then
            FBitmap.LoadFromFile( tkn.Result[0] )
          else
            Abort ;
        end ;

        if ( tkn.Result.Count >= 2 ) and ParamBoolean( tkn.Result[1], True ) then
          FBitmap.MakeTransparent ;

        FNativeHeight := FBitmap.Height ;
        FNativeWidth  := FBitmap.Width  ;
        FNativeSize   := Max( FNativeHeight, FNativeWidth ) ;
        if FNativeSize = 0 then
          FNativeSize := 1 ;

        FWidth  := getRealSymWidth ;
        FHeight := getRealSymHeight ;
      finally
        FreeObject( tkn ) ;
      end ;
    except
      FreeObject( FBitmap ) ;
      raise ;
    end ;
  end ;

  constructor TGIS_SymbolPicture.Create(
    const _name     : String ;
    {$IFDEF CLR}
      const _stream : Stream
    {$ELSE}
      const _stream : TStream
    {$ENDIF}
  ) ;
  var
    tkn  : TGIS_Tokenizer ;
  begin
    inherited Create( GetPathAbsolute( '', _name ) );
    FIsFileBased := False ;

    FBitmap := TGIS_Bitmap.Create ;
    try
      tkn := TGIS_Tokenizer.Create ;
      try
        tkn.Execute( _name, ['?'], True ) ;

        if assigned( _stream ) then begin
          if tkn.Result.Count >= 1 then
            FBitmap.LoadFromStream( _stream ) ;
          if ( tkn.Result.Count >= 2 ) and ParamBoolean( tkn.Result[1], True ) then
            FBitmap.MakeTransparent ;

          FNativeHeight := FBitmap.Height ;
          FNativeWidth  := FBitmap.Width  ;
          FNativeSize   := Max( FNativeHeight, FNativeWidth ) ;
          if FNativeSize = 0 then
            FNativeSize := 1 ;

          FWidth  := getRealSymWidth ;
          FHeight := getRealSymHeight ;
        end ;
      finally
        FreeObject( tkn ) ;
      end ;
    except
      FreeObject( FBitmap ) ;
      raise ;
    end ;
  end ;

  procedure TGIS_SymbolPicture.doDestroy ;
  begin
    FreeObject( FBitmap ) ;
    inherited ;
  end ;

  function TGIS_SymbolPicture.getRealSymWidth
    : Integer ;
  begin
    if assigned( FBitmap ) then
      Result := RoundS( FBitmap.Width * symScale )
    else
      Result := 0 ;
  end ;

  function TGIS_SymbolPicture.getRealSymHeight
    : Integer ;
  begin
    if assigned( FBitmap ) then
      Result := RoundS( FBitmap.Height * symScale )
    else
      Result := 0 ;
  end ;

  procedure TGIS_SymbolPicture.Draw(
    const _x : Integer ;
    const _y : Integer
  ) ;
  var
    rct  : TRect   ;
    x0   : Integer ;
    y0   : Integer ;
    rnd  : TGIS_RendererAbstract ;
  begin
    if symSize < 1 then exit ;

    assert( assigned( symRenderer ) ) ;
    rnd := TGIS_RendererAbstract( symRenderer ) ;

    if symFullRot <> 0 then begin
      rnd.CanvasSetTransformation( symFullRot, _x, _y ) ;
      x0 := - Width  div 2 ;
      y0 := - Height div 2 ;
    end else begin
      x0 := _x - Width  div 2 ;
      y0 := _y - Height div 2 ;
    end ;

    try
      rct := Rect( x0 + symOffset.X,
                   y0 + symOffset.Y,
                   x0 + Width  + symOffset.X,
                   y0 + Height + symOffset.Y
                 ) ;

      rnd.RenderBitmap( nil, FBitmap, rct, True ) ;
    finally
      if symFullRot <> 0 then
        rnd.CanvasClearTransformation ;
    end ;
  end ;

//==============================================================================
// TGIS_SymbolCGM
//==============================================================================

  constructor TGIS_SymbolCGM.Create( const _name : String ) ;
  begin
    inherited Create( GetPathAbsolute( '', _name ) ) ;
    FIsFileBased := True ;

    {$IFDEF MANAGED}
      cgmMeta := T_SymbolCgmPrimitive.Create ;
    {$ELSE}
      GetMem( cgmMeta, sizeOf( T_SymbolCgmPrimitive ) ) ;
    {$ENDIF}

    parseFile ;

    FWidth  := getRealSymWidth ;
    FHeight := getRealSymHeight ;
  end ;

  constructor TGIS_SymbolCGM.Create(
    const _name     : String ;
    {$IFDEF CLR}
      const _stream : Stream
    {$ELSE}
      const _stream : TStream
    {$ENDIF}
  ) ;
  begin
    inherited Create( GetPathAbsolute( '', _name ) ) ;
    FIsFileBased := False ;

    {$IFDEF MANAGED}
      cgmMeta := T_SymbolCgmPrimitive.Create ;
    {$ELSE}
      GetMem( cgmMeta, sizeOf( T_SymbolCgmPrimitive ) ) ;
    {$ENDIF}

    parseFile( _stream ) ;

    FWidth  := getRealSymWidth ;
    FHeight := getRealSymHeight ;
  end;

  procedure TGIS_SymbolCGM.doDestroy ;
  begin
    FreeObject( metaStream ) ;

    {$IFDEF MANAGED}
      FreeObject( cgmMeta ) ;
    {$ELSE}
      Dispose( cgmMeta ) ;
    {$ENDIF}
    inherited ;
  end ;

  function TGIS_SymbolCGM.getSymWidth
    : Integer ;
  begin
    Result := toPixelI( Max( Abs( cgmVDCEXT.Right ), Abs( cgmVDCEXT.Left ) ) ) * 2 ;
  end ;

  function TGIS_SymbolCGM.getSymHeight
    : Integer ;
  begin
    Result := toPixelI( Max( Abs( cgmVDCEXT.Bottom ), Abs( cgmVDCEXT.Top ) ) ) * 2 ;
  end ;

  function TGIS_SymbolCGM.getRealSymWidth
    : Integer ;
  begin
    Result := toPixelI( cgmVDCEXT.Right  ) -
              toPixelI( cgmVDCEXT.Left   ) ;
  end ;

  function TGIS_SymbolCGM.getRealSymHeight
    : Integer ;
  begin
    Result := toPixelI( cgmVDCEXT.Bottom  ) -
              toPixelI( cgmVDCEXT.Top     ) ;
  end ;

  function TGIS_SymbolCGM.ellipticArc(
    const _cx,_cy,
          _ax,_ay,
          _bx,_by,
          _sx,_sy,
          _ex,_ey : Integer
  ) : Integer ;
  const
    MAX_STEPS = 90 ;
  var
    xa,ya,
    xb,yb,
    xc,yc,
    xs,ys,
    xe,ye     : Integer ;
    pt        : TPoint  ;
    cnt       : Integer ;
    da,db     : Single  ;
    xn,yn     : Integer ;
    start     : Single  ;
    stop      : Single  ;
    arcangle  : Single  ;
    step      : Single  ;
    steps     : Integer ;
    rotate    : Single  ;
    rotateSin : Single  ;
    rotateCos : Single  ;
    sa, ca    : Single  ;

    function fmod( a, b : Single ) : Single ;
    var
     f : Integer ;
    begin
      f := TruncS( a/b ) ;
      Result := a - (b*f) ;
    end ;
  begin
    // rotation
       rotate := ArcTan2( _ay - _cy, -_ax + _cx ) ;
       SinCos( rotate, rotateSin, rotateCos ) ;

       if rotate = 0 then begin
                            xc := _cx ; yc := _cy ;
                            xa := _ax ; ya := _ay ;
                            xb := _bx ; yb := _by ;
                            xs := _sx ; ys := _sy ;
                            xe := _ex ; ye := _ey ;
                          end
                     else begin
                            xc := RoundS( _cx * rotateCos  -  _cy * rotateSin ) ;
                            yc := RoundS( _cx * rotateSin  +  _cy * rotateCos ) ;

                            xa := RoundS( _ax * rotateCos  -  _ay * rotateSin ) ;
                            ya := RoundS( _ax * rotateSin  +  _ay * rotateCos ) ;

                            xb := RoundS( _bx * rotateCos  -  _by * rotateSin ) ;
                            yb := RoundS( _bx * rotateSin  +  _by * rotateCos ) ;

                            xs := RoundS( _sx * rotateCos  -  _sy * rotateSin ) ;
                            ys := RoundS( _sx * rotateSin  +  _sy * rotateCos ) ;

                            xe := RoundS( _ex * rotateCos  -  _ey * rotateSin ) ;
                            ye := RoundS( _ex * rotateSin  +  _ey * rotateCos ) ;
                         end ;

       da := Sqrt( Sqr( xa - xc ) + Sqr( ya - yc ) ) ;
       db := Sqrt( Sqr( xb - xc ) + Sqr( yb - yc ) ) ;

    // calculate angle of pie
       if      ys = yc then begin
                              if xs < xc then start := Pi
                                         else start := 0  ;
                            end
       else if xs = xc then begin
                              if ys < yc then start := -Pi/2
                                         else start :=  Pi/2 ;
                            end
       else begin
         if Abs(xs - xc) > Abs(da) then start := da
                                   else start := ArcCos( (xs - xc)/ da ) ;
         if ys < yc then start := - start ;

       end ;

       if      ye = yc then begin
                              if xe < xc then stop := Pi
                                         else stop := 0  ;
                            end
       else if xe = xc then begin
                              if ye < yc then stop := -Pi/2
                                         else stop :=  Pi/2 ;
                            end
       else begin
         if Abs(ye - yc) > Abs(db) then stop := db
                                   else stop := ArcSin( (ye - yc) / db )  ;

         if ye < yc then begin
                             if xe < xc then stop := -Pi - stop ;
                           end
         else              begin
                             if xe < xc then stop := Pi - stop ;
                          end ;
       end ;

       if (xe = xs) and (ye = ys) then begin
         // full ellipse
         start := 0    ;
         arcangle := 2*Pi ;
       end
       else
         arcangle := fmod( (-stop+start) +4*Pi, 2*Pi) ;

    // calculate number of segments - and minimize it
       if      arcangle < Pi/4 then steps := MAX_STEPS div 8
       else if arcangle < Pi/2 then steps := MAX_STEPS div 4
       else if arcangle < Pi   then steps := MAX_STEPS div 2
       else steps := MAX_STEPS - 1 ;

       step := - Abs( arcangle ) / steps ;

      {$IFDEF GIS_NORECORDS}
        pt := new TPoint(0,0) ;
      {$ENDIF}
    // calculate elliptical arc
       for cnt := 0 to steps do begin
         SinCos( start, sa, ca ) ;
         yn := RoundS( -db * sa * rotateCos +
                        da * ca * rotateSin
                     ) ;
         xn := RoundS(  da * ca * rotateCos +
                        db * sa * rotateSin
                     ) ;
         pt := Point( _cx+xn, _cy-yn ) ;
         {$IFDEF MANAGED}
           metaStream.WriteInteger( pt.X ) ;
           metaStream.WriteInteger( pt.Y ) ;
         {$ELSE}
           metaStream.Write( pt, sizeOf( pt ) ) ;
         {$ENDIF}

         start := start + step ;
       end ;

       Result := steps + 1  ;
  end ;

  function TGIS_SymbolCGM.getTokenClearText
    : String ;
  var
    {$IFDEF OXYGENE}
      {$IFDEF JAVA OR ISLAND}
        c   : Byte    ;
      {$ELSE}
        c   : Char    ;
      {$ENDIF}
    {$ELSE}
      c   : Byte    ;
    {$ENDIF}
    state : Integer ;
  begin
    Result  := '' ;
    state   := 0 ;
    while cgmStream.Position < cgmStream.Size  do begin
      {$IFNDEF OXYGENE}
        cgmStream.ReadByte( c ) ;
      {$ELSE}
        {$IFDEF JAVA OR ISLAND}
          cgmStream.ReadByte( c ) ;
        {$ELSE}
        cgmStream.ReadChar( c ) ;
        {$ENDIF}
      {$ENDIF}
      case state of
        0 :  case c of // skip spaces
               {$IFDEF CLR}
                 ' ',
                 #13,
                 #10,
                 #7   : continue ;
                 else   begin
                          Result := Result + c ;
                          state := 1 ;
                        end ;
               {$ELSE}
                 ord(' '),
                 ord(#13),
                 ord(#10),
                 ord(#7 )  : continue ;
                 else        begin
                               Result := Result + Char(c) ;
                               state := 1 ;
                             end ;
               {$ENDIF}
             end ;
        1 :  case c of
               {$IFDEF CLR}
                 ' ',
                 #13,
                 #10,
                 #7   : begin
                          endMarker := False ;
                          exit ;
                        end ;
                 ','  : begin
                          endMarker := False ;
                          exit ;
                        end ;
                 ';'  : begin
                          endMarker := True ;
                          exit ;
                        end ;
                 else   begin
                          Result := Result + c ;
                          continue ;
                        end ;
               {$ELSE}
                 ord(' '),
                 ord(#13),
                 ord(#10),
                 ord(#7 )  : begin
                               endMarker := False ;
                               exit ;
                             end ;
                 ord(',')  : begin
                               endMarker := False ;
                               exit ;
                             end ;
                 ord(';')  : begin
                               endMarker := True ;
                               exit ;
                             end ;
                 else        begin
                               Result := Result + Char(c) ;
                               continue ;
                             end ;
               {$ENDIF}
             end ;
        else assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
      end ;
    end ;
    endMarker := True ;
    cgmEof := True ;
  end ;

  function TGIS_SymbolCGM.getTokenCommand
    : Integer ;
  var
    b         : Byte     ;
    header    : Word     ;
    elm_class : ShortInt ;
    elm_id    : ShortInt ;
    param_len : Word     ;
    code      : Integer  ;
    command   : String   ;

    function t( const _cmd : String ) : Boolean ;
    begin
      Result := CompareText( _cmd, command ) = 0 ;
    end ;

  begin
    Result  := 0 ;

    if FBinary then begin
      endMarker := False ;
      while cgmStream.Position < cgmStream.Size  do begin
        while binLength > 0 do begin
          assert( sizeOf( b ) = sizeOf( Byte ) ) ;
          if cgmStream.ReadByte( b ) <> sizeOf( Byte ) then begin
            endMarker := True ;
            cgmEof    := True ;
            exit ;
          end ;
          dec( binLength ) ;
        end ;
        assert( sizeOf( header ) = sizeOf( Word ) ) ;
        cgmStream.ReadWord( header ) ;
        {$IFDEF CLR}
          {$IFNDEF OXYGENE}
            {$WARN UNSAFE_CODE OFF}
          {$ENDIF}
        {$ENDIF}
          header := Swap( header ) ;
        {$IFDEF CLR}
          {$IFNDEF OXYGENE}
            {$WARN UNSAFE_CODE ON}
          {$ENDIF}
        {$ENDIF}

        elm_class := header shr 12 ;
        elm_id    := ( header shr 5 ) and $3f ;

        param_len := header and $1f ;
        if param_len >= 31 then begin
          assert( sizeOf( param_len ) = sizeOf( Word ) ) ;
          cgmStream.ReadWord( param_len ) ;
          {$IFDEF CLR}
            {$IFNDEF OXYGENE}
              {$WARN UNSAFE_CODE OFF}
            {$ENDIF}
          {$ENDIF}
            param_len := Swap( param_len ) ;
          {$IFDEF CLR}
            {$IFNDEF OXYGENE}
              {$WARN UNSAFE_CODE ON}
            {$ENDIF}
          {$ENDIF}
        end ;

        binLength := param_len ;

        if binLength mod 2 <> 0 then inc( binLength ) ;

        code := elm_class * 100 + elm_id ;
        Result := code ;
        exit ;
      end ;
      endMarker := True ;
      cgmEof := True ;
    end
    else begin
      command := getTokenClearText ;
      if      t( CGM_SCALEMODE      ) then Result := CGM_BIN_SCALEMODE
      else if t( CGM_INTEGERPREC    ) then Result := CGM_BIN_INTEGERPREC
      else if t( CGM_REALPREC       ) then Result := CGM_BIN_REALPREC
      else if t( CGM_COLRMODE       ) then Result := CGM_BIN_COLRMODE
      else if t( CGM_LINEWIDTHMODE  ) then Result := CGM_BIN_LINEWIDTHMODE
      else if t( CGM_EDGEWIDTHMODE  ) then Result := CGM_BIN_EDGEWIDTHMODE
      else if t( CGM_VDCEXT         ) then Result := CGM_BIN_VDCEXT
      else if t( CGM_VDCINTEGERPREC ) then Result := CGM_BIN_VDCINTEGERPREC
      else if t( CGM_VDCREALPREC    ) then Result := CGM_BIN_VDCREALPREC
      else if t( CGM_COLRTABLE      ) then Result := CGM_BIN_COLRTABLE
      else if t( CGM_INTSTYLE       ) then Result := CGM_BIN_INTSTYLE
      else if t( CGM_FILLCOLR       ) then Result := CGM_BIN_FILLCOLR
      else if t( CGM_EDGECOLR       ) then Result := CGM_BIN_EDGECOLR
      else if t( CGM_EDGEWIDTH      ) then Result := CGM_BIN_EDGEWIDTH
      else if t( CGM_EDGEVIS        ) then Result := CGM_BIN_EDGEVIS
      else if t( CGM_LINECOLR       ) then Result := CGM_BIN_LINECOLR
      else if t( CGM_LINEWIDTH      ) then Result := CGM_BIN_LINEWIDTH
      else if t( CGM_RECTANGLE      ) then Result := CGM_BIN_RECTANGLE
      else if t( CGM_RECT           ) then Result := CGM_BIN_RECTANGLE
      else if t( CGM_POLYGON        ) then Result := CGM_BIN_POLYGON
      else if t( CGM_LINE           ) then Result := CGM_BIN_LINE
      else if t( CGM_CIRCLE         ) then Result := CGM_BIN_CIRCLE
      else if t( CGM_ARCCTR         ) then Result := CGM_BIN_ARCCTR
      else if t( CGM_ELLIPSE        ) then Result := CGM_BIN_ELLIPSE
      else if t( CGM_ELLIPARC       ) then Result := CGM_BIN_ELLIPARC    ;
    end ;
  end ;

  function TGIS_SymbolCGM.getTokenByte
    : Integer ;
  var
    buf : Byte  ;
  begin
    Result  := 0 ;
    if FBinary then begin
      if binLength > 0 then begin
        assert( sizeOf( buf ) = sizeOf( Byte ) ) ;
        if cgmStream.ReadByte( buf ) <> sizeOf( Byte ) then begin
          endMarker := True ;
          cgmEof    := True ;
          exit ;
        end ;
        dec( binLength, sizeOf( Byte ) ) ;
        Result := buf ;
      end ;
      if binLength <= 0 then endMarker := True ;
    end
    else
      Result := StrToInt( getTokenClearText ) ;
  end ;

  function TGIS_SymbolCGM.getTokenWord
    : Cardinal ;
  var
    buf : Word ;
  begin
    Result  := 0 ;
    if FBinary then begin
      if binLength > 0 then begin
        assert( sizeOf( buf ) = sizeOf( Word ) ) ;
        if cgmStream.ReadWord( buf ) <> sizeOf( Word ) then begin
          endMarker := True ;
          cgmEof    := True ;
          exit ;
        end ;
        dec( binLength, sizeOf( Word ) ) ;

        {$IFDEF CLR}
          {$IFNDEF OXYGENE}
            {$WARN UNSAFE_CODE OFF}
          {$ENDIF}
        {$ENDIF}
          buf := Swap( buf ) ;
        {$IFDEF CLR}
          {$IFNDEF OXYGENE}
            {$WARN UNSAFE_CODE OFF}
          {$ENDIF}
        {$ENDIF}
        Result := RoundS(buf) ;
      end ;
      if binLength <= 0 then endMarker := True ;
    end
    else
      Result := StrToInt( getTokenClearText ) ;
  end ;

  function TGIS_SymbolCGM.getTokenNumber(
    const _float : Boolean ;
    const _vdc   : Boolean
  ) : Single ;
  var
    buf  : Array of Byte ;
    val  : Integer             ;

    procedure readBuffer( const _size : Integer ) ;
    var
      i : Integer ;
      b : Byte    ;
    begin

      if cgmStream.Read( buf{$IFDEF DCC}[0]{$ENDIF}, _size ) <> _size then begin
        endMarker := True ;
        cgmEof    := True ;
        Abort ;
      end ;
      dec( binLength, _size ) ;

      for i:=0 to _size div 2 - 1 do begin // swap bytes
        b := buf[ i ] ;
        buf[i] := buf[ _size - 1 -i ] ;
        buf[ _size - 1 -i ] := b ;
      end ;
    end ;

    function readInt8 : Integer ;
    begin
      assert( sizeOf( Byte ) = 1 ) ;
      readBuffer( sizeOf( Byte ) ) ;

      {$IFDEF MANAGED}
        Result := buf[0] ;
      {$ELSE}
        Result := PByte( @buf[0] )^ ;
      {$ENDIF}
    end ;

    function readInt16 : Integer ;
    begin
      assert( sizeOf( SmallInt ) = 2 ) ;

      readBuffer( sizeOf( SmallInt ) ) ;
      {$IFDEF MANAGED}
        Result := BitConverter.ToInt16( buf, 0 ) ;
      {$ELSE}
        Result := PSmallInt( @buf[0] )^ ;
      {$ENDIF}
    end ;

    function readUInt16 : Cardinal ;
    begin
      assert( sizeOf( Word ) = 2 ) ;

      readBuffer( sizeOf( Word ) ) ;
      {$IFDEF MANAGED}
        Result := BitConverter.ToUInt16( buf, 0 ) ;
      {$ELSE}
        Result := PWORD( @buf[0] )^ ;
      {$ENDIF}
    end ;

    function readInt24 : Integer ;
    var
      i : Integer ;
    begin
      i := readInt16 ;
      Result := ( i shl 8 ) + readInt8 ;
    end ;

    function readInt32 : Integer ;
    begin
      assert( sizeOf( Integer ) = 4 ) ;

      readBuffer( sizeOf( Integer ) ) ;
      {$IFDEF MANAGED}
        Result := BitConverter.ToInt32( buf, 0 ) ;
      {$ELSE}
        Result := PInteger( @buf[0] )^ ;
      {$ENDIF}
    end ;

    function readUInt32 : Cardinal ;
    begin
      assert( sizeOf( Cardinal ) = 4 ) ;

      readBuffer( sizeOf( Cardinal ) ) ;
      {$IFDEF MANAGED}
        Result := BitConverter.ToUInt32( buf, 0 ) ;
      {$ELSE}
        Result := PCardinal( @buf[0] )^ ;
      {$ENDIF}
    end ;

    function readFloat32 : Single ;
    begin
      assert( sizeOf( Single ) = 4 ) ;

      readBuffer( sizeOf( Single ) ) ;
      try
        {$IFDEF MANAGED}
          Result := BitConverter.ToSingle( buf, 0 ) ;
        {$ELSE}
          Result := PSingle( @buf[0] )^ ;
        {$ENDIF}
      except
        Result := 0 ;
      end ;
    end ;

    function readFloat64 : Double ;
    begin
      assert( sizeOf( Double ) = 8 ) ;
      readBuffer( sizeOf( Double ) ) ;

      try
        {$IFDEF MANAGED}
          Result := BitConverter.ToDouble( buf, 0 ) ;
        {$ELSE}
          Result := PDouble( @buf[0] )^ ;
        {$ENDIF}
      except
        Result := 0 ;
      end ;
    end ;

    function readFixed32 : Single ;
    var
      i : Integer ;
    begin
      i := readInt16 ;
      try
        Result := i * ( readUInt16 / GIS_MAX_DWORD ) ;
      except
        Result := 0 ;
      end ;
    end ;

    function readFixed64 : Double ;
    var
      i : Integer ;
    begin
      i := readInt32 ;
      try
        Result := i * ( readUInt32 / GIS_MAX_CARDINAL ) ;
      except
        Result := 0 ;
      end ;
    end ;

  begin
    Result  := 0 ;
    if FBinary then begin
      SetLength( buf, 8 ) ;
      if binLength > 0 then begin
        try
          if not _float then begin // integer
            if _vdc then val := cgmVDCINTEGERPREC
                    else val := cgmINTEGERPREC    ;
            case val of
              8  : Result := readInt8  ;
              16 : Result := readInt16 ;
              24 : Result := readInt24 ;
              32 : Result := readInt32 ;
              else begin
                     assert( False ) ;
                   end ;
            end ;
          end
          else begin // real
            if _vdc then val := cgmVDCREALPREC
                    else val := cgmREALPREC    ;
            case val of
              9  : Result := readFloat32 ;
              12 : Result := readFloat64 ;
              16 : Result := readFixed32 ;
              32 : Result := readFixed64 ;
              else begin
                     assert( False ) ;
                   end ;
            end ;
          end ;
        except
          Result := 0 ;
        end ;
      end ;
      if binLength <= 0 then endMarker := True ;
    end
    else
      Result := DotStrToFloat( getTokenClearText ) ;
  end ;

  function TGIS_SymbolCGM.getTokenInteger
    : Integer ;
  begin
    Result := RoundS( getTokenNumber( False, False ) ) ;
  end ;

  function TGIS_SymbolCGM.getTokenFloat
    : Single ;
  begin
    Result := getTokenNumber( True, False ) ;
  end ;

  function TGIS_SymbolCGM.getTokenVdc
    : Integer ;
  begin
    Result := RoundS( getTokenNumber( cgmVDCTYPE, True ) * cgmVdcScale ) ;
  end ;

  function TGIS_SymbolCGM.getTokenColor
    : TGIS_Color ;
  var
    ival : Integer ;
    r   : Byte    ;
    g   : Byte    ;
    b   : Byte    ;
  begin
    if cgmCOLRMODE = 0 then begin
      ival := getTokenByte ;
      if not ( ival in [0..255] ) then ival := 0 ;

      Result := cgmCOLRTABLE[ ival ] ;
    end
    else begin
      r := getTokenByte ;
      g := getTokenByte ;
      b := getTokenByte ;
      Result := TGIS_Color.FromRGB( r,g,b ) ;
    end ;
  end ;

  procedure TGIS_SymbolCGM.parseFile ;
  begin
    parseFile( nil ) ;
  end;

  procedure TGIS_SymbolCGM.parseFile(
    const _stream : TStream
  ) ;
  var
    i        : Integer ;
    sentinel : Array of Byte ;
    old_pos  : Int64 ;
    stream_created : Boolean ;

    function compareLeader : Boolean ;
    var
      i1 : Integer ;
      {$IFDEF OXYGENE}
        s : String     ;
      {$ELSE}
        s : AnsiString ;
      {$ENDIF}
      b  : Byte   ;
    begin
      s := 'BEGMF' ;
      Result := False ;
      for i1 := 0 to 4 do begin
        b := Byte( s[i1+StringFirst] ) ;
        if ( sentinel[i1] <> b      ) and
           ( sentinel[i1] <> b + 32 )     // lowercase
        then exit ;
      end ;
      Result := True ;
    end ;

  begin
    FreeObject( metaStream ) ;
    old_pos := 0 ;
    stream_created := False ;

    if _stream = nil then begin
      cgmStream := TGIS_FileStream.Create( FName,
                                           fmOpenRead or
                                           fmShareDenyWrite
                                         ) ;
    end
    else begin
      old_pos := _stream.Position ;
      _stream.Position := 0 ;
      if _stream is TGIS_HandleStream then
        cgmStream := TGIS_HandleStream( _stream )
      else begin
        cgmStream := TGIS_HandleStream.Create( _stream ) ;
        stream_created := True ;
      end ;
    end ;

    try
      metaStream := TGIS_MemoryStream.Create ;

      SetLength( sentinel, 5 ) ;
      sentinel[0] := 33 ;
      sentinel[1] := 33 ;
      sentinel[2] := 33 ;
      sentinel[3] := 33 ;
      sentinel[4] := 33 ;

      {$IFDEF OXYGENE}
        cgmStream.Read( sentinel, 5 ) ;
      {$ELSE}
        cgmStream.Read( sentinel[0], 5 ) ;
      {$ENDIF}

      if      sentinel[0] = 0   then FBinary := True
      else if compareLeader     then FBinary := False
      else                      {$IFDEF OXYGENE}
                                  raise EGIS_Exception.Create('','',1) ;
                                {$ELSE}
                                  raise EConvertError.Create('') ;
                                {$ENDIF}

      if FBinary then cgmStream.Position := 0
                 else cgmStream.Position := 0 ;

      endMarker :=  False ;
      cgmEof := False ;

      for i:= low( cgmCOLRTABLE ) to high( cgmCOLRTABLE ) do
        cgmCOLRTABLE[i] := TGIS_Color.Black ;

      cgmVdcScale      := 1     ;
      cgmVDCTYPE       := False ;
      cgmCOLRMODE      := 0     ;
      cgmLINEWIDTHMODE := 0     ;
      cgmEDGEWIDTHMODE := 0     ;
      cgmINTEGERPREC   := 16    ;
      cgmREALPREC      := 9     ;
      cgmVDCINTEGERPREC:= 16    ;
      cgmVDCREALPREC   := 9     ;
      cgmINTSTYLE      := TGIS_BrushStyle.Clear ;
      cgmFILLCOLR      := TGIS_Color.Black ;
      cgmEDGECOLR      := TGIS_Color.Black ;
      cgmEDGEWIDTH     := 1 ;
      cgmEDGEVIS       := True ;
      cgmLINECOLR      := TGIS_Color.Black ;
      cgmLINEWIDTH     := 1 ;

      while not cgmEof do begin
        parseCommand( getTokenCommand ) ;
      end ;

      {$IFDEF MANAGED}
        T_SymbolCgmPrimitive( cgmMeta ).Kind := 0 ;
        T_SymbolCgmPrimitive( cgmMeta ).Write( metaStream ) ;
      {$ELSE}
        T_SymbolCgmPrimitive(cgmMeta^).Kind := 0 ;
        metaStream.Write( T_SymbolCgmPrimitive(cgmMeta^),
                          sizeOf( T_SymbolCgmPrimitive ) ) ;
      {$ENDIF}

    finally
      if _stream = nil then
        FreeObject( cgmStream )
      else begin
        if stream_created then
          FreeObject( cgmStream ) ;
        _stream.Position := old_pos ;
      end ;
      if FNativeSize = 0 then
        FNativeSize := 1 ;
    end ;
  end ;

  procedure TGIS_SymbolCGM.parseCommand(
    const _command : Integer
  ) ;
  begin

    case _command of
      CGM_BIN_VDCTYPE        : doVDCTYPE        ;
      CGM_BIN_INTEGERPREC    : doINTEGERPREC    ;
      CGM_BIN_REALPREC       : doREALPREC       ;
      CGM_BIN_METAFILEREPL   : doMETAFILEREPL   ;
      CGM_BIN_SCALEMODE      : doSCALEMODE      ;
      CGM_BIN_COLRMODE       : doCOLRMODE       ;
      CGM_BIN_LINEWIDTHMODE  : doLINEWIDTHMODE  ;
      CGM_BIN_EDGEWIDTHMODE  : doEDGEWIDTHMODE  ;
      CGM_BIN_VDCEXT         : doVDCEXT         ;
      CGM_BIN_VDCINTEGERPREC : doVDCINTEGERPREC ;
      CGM_BIN_VDCREALPREC    : doVDCREALPREC    ;
      CGM_BIN_COLRTABLE      : doCOLRTABLE      ;
      CGM_BIN_INTSTYLE       : doINTSTYLE       ;
      CGM_BIN_FILLCOLR       : doFILLCOLR       ;
      CGM_BIN_EDGECOLR       : doEDGECOLR       ;
      CGM_BIN_EDGEWIDTH      : doEDGEWIDTH      ;
      CGM_BIN_EDGEVIS        : doEDGEVIS        ;
      CGM_BIN_LINECOLR       : doLINECOLR       ;
      CGM_BIN_LINEWIDTH      : doLINEWIDTH      ;
      CGM_BIN_RECTANGLE      : doRECTANGLE      ;
      CGM_BIN_POLYGON        : doPOLYGON        ;
      CGM_BIN_POLYGONSET     : doPOLYGONSET     ;
      CGM_BIN_LINE           : doLINE           ;
      CGM_BIN_CIRCLE         : doCIRCLE         ;
      CGM_BIN_ARCCTR         : doARCCTR         ;
      CGM_BIN_ELLIPSE        : doELLIPSE        ;
      CGM_BIN_ELLIPARC       : doELLIPARC       ;
      else                     doANY            ;
    end ;
  end ;

  procedure TGIS_SymbolCGM.prepareEntryLine ;
  begin
    cgmPos := metaStream.Position ;
    {$IFDEF MANAGED}
      with T_SymbolCgmPrimitive(cgmMeta)  do begin
    {$ELSE}
      with T_SymbolCgmPrimitive(cgmMeta^) do begin
    {$ENDIF}
        Kind       := CGM_ENTRY_LINE ;
        BrushColor := cgmFILLCOLR.ARGB ;
        BrushStyle := cgmINTSTYLE    ;
        PenColor   := cgmLINECOLR.ARGB ;
        PenWidth   := cgmLINEWIDTH   ;
        PenStyle   := TGIS_PenStyle.Solid ;
    end ;
    {$IFDEF MANAGED}
      T_SymbolCgmPrimitive( cgmMeta ).Write( metaStream ) ;
    {$ELSE}
      metaStream.Write( cgmMeta^, sizeOf( T_SymbolCgmPrimitive ) ) ;
    {$ENDIF}
  end ;

  procedure TGIS_SymbolCGM.prepareEntrySurface ;
  begin
    cgmPos := metaStream.Position ;
    {$IFDEF MANAGED}
      with T_SymbolCgmPrimitive(cgmMeta)  do begin
    {$ELSE}
      with T_SymbolCgmPrimitive(cgmMeta^) do begin
    {$ENDIF}
        Kind       := CGM_ENTRY_SURFACE ;
        BrushColor := cgmFILLCOLR.ARGB  ;
        BrushStyle := cgmINTSTYLE       ;
        PenColor   := cgmEDGECOLR.ARGB  ;
        PenWidth   := cgmEDGEWIDTH      ;
        if cgmEDGEVIS then PenStyle := TGIS_PenStyle.Solid
                      else PenStyle := TGIS_PenStyle.Clear ;
    end ;
    {$IFDEF MANAGED}
      T_SymbolCgmPrimitive( cgmMeta ).Write( metaStream ) ;
    {$ELSE}
      metaStream.Write( cgmMeta^, sizeOf( T_SymbolCgmPrimitive ) ) ;
    {$ENDIF}
  end ;

  procedure TGIS_SymbolCGM.finishEntry(
    const _cnt : Integer
  ) ;
  begin
    metaStream.Position := cgmPos ;
    {$IFDEF MANAGED}
      T_SymbolCgmPrimitive(cgmMeta).Points  := _cnt ;
      T_SymbolCgmPrimitive(cgmMeta).Write( metaStream ) ;
    {$ELSE}
      T_SymbolCgmPrimitive(cgmMeta^).Points := _cnt ;
      metaStream.Write( cgmMeta^, sizeOf( T_SymbolCgmPrimitive ) ) ;
    {$ENDIF}
    metaStream.Position := metaStream.Size ;
  end ;

  procedure TGIS_SymbolCGM.doANY ;
  begin
    if FBinary then begin
    end
    else begin
      while not endMarker do
        getTokenClearText ;
    end ;
  end ;

  procedure TGIS_SymbolCGM.doVDCTYPE ;
  begin
    cgmVDCTYPE := getTokenWord <> 0 ;
  end ;

  procedure TGIS_SymbolCGM.doINTEGERPREC ;
  var
    v1, v2 : Integer ;
  begin
    if FBinary then begin
      cgmINTEGERPREC := getTokenWord ;
    end
    else begin
      v1 := getTokenInteger ;
      v2 := getTokenInteger ;
      cgmINTEGERPREC := length( IntToStr( Max( Abs(v1), Abs(v2) ) ) ) ;
    end ;
  end ;

  procedure TGIS_SymbolCGM.doREALPREC ;
  begin
    if FBinary then begin
      getTokenWord ;
      cgmREALPREC := getTokenWord ;
    end
    else begin
      getTokenFloat ;
      getTokenFloat ;
      cgmREALPREC := getTokenWord ;
    end;
  end ;

  procedure TGIS_SymbolCGM.doMETAFILEREPL ;
  begin
    binLength := 0 ;
  end ;

  procedure TGIS_SymbolCGM.doCOLRMODE ;
  var
    stmp : String ;

    function t( const _val : String ) : Boolean ;
    begin
      Result := CompareText( _val, stmp ) = 0 ;
    end ;
  begin
    if FBinary then begin
      cgmCOLRMODE := getTokenWord ;
    end
    else begin
      stmp := getTokenClearText ;

      if t( CGM_INDEXED  ) then cgmCOLRMODE := 0
                           else cgmCOLRMODE := 1
    end ;
  end ;

  procedure TGIS_SymbolCGM.doLINEWIDTHMODE ;
  var
    stmp : String ;

    function t( const _val : String ) : Boolean ;
    begin
      Result := CompareText( _val, stmp ) = 0 ;
    end ;
  begin
    if FBinary then begin
      cgmLINEWIDTHMODE := getTokenWord ;
    end
    else begin
      stmp := getTokenClearText ;

      if      t( CGM_ABS        ) then cgmLINEWIDTHMODE := 0
      else if t( CGM_SCALED     ) then cgmLINEWIDTHMODE := 1
      else if t( CGM_FRACTIONAL ) then cgmLINEWIDTHMODE := 2
      else if t( CGM_MM         ) then cgmLINEWIDTHMODE := 3
    end ;
  end ;

  procedure TGIS_SymbolCGM.doEDGEWIDTHMODE ;
  var
    stmp : String ;

    function t( const _val : String ) : Boolean ;
    begin
      Result := CompareText( _val, stmp ) = 0 ;
    end ;
  begin
    if FBinary then begin
      cgmEDGEWIDTHMODE := getTokenWord ;
    end
    else begin
      stmp := getTokenClearText ;

      if      t( CGM_ABS        ) then cgmEDGEWIDTHMODE := 0
      else if t( CGM_SCALED     ) then cgmEDGEWIDTHMODE := 1
      else if t( CGM_FRACTIONAL ) then cgmEDGEWIDTHMODE := 2
      else if t( CGM_MM         ) then cgmEDGEWIDTHMODE := 3
    end ;
  end ;

  procedure TGIS_SymbolCGM.doSCALEMODE ;
  begin
    if FBinary then begin
      //? not fully implemented
    end
    else begin
      getTokenClearText ;
      symInTwips := DotStrToFloat( getTokenClearText ) * 1440/ 25.40 ;
    end ;
  end ;

  procedure TGIS_SymbolCGM.doVDCEXT ;
  var
    ext   : TGIS_Extent ;
    delta : Single ;
    w, h  : Single ;
  begin
    {$IFDEF GIS_NORECORDS}
      ext := new TGIS_Extent ;
    {$ENDIF}
    ext.XMin := getTokenNumber( cgmVDCTYPE, True ) ;
    ext.YMin := getTokenNumber( cgmVDCTYPE, True ) ;
    ext.XMax := getTokenNumber( cgmVDCTYPE, True ) ;
    ext.YMax := getTokenNumber( cgmVDCTYPE, True ) ;
    delta := Max( ext.XMax - ext.XMin, ext.YMax - ext.YMin ) ;

    if delta < 1000 then
      cgmVdcScale := 1000 / delta ;

    //RectEx
    cgmVDCEXT := Rect( RoundS( ext.XMin * cgmVdcScale ) ,
                       RoundS( ext.YMin * cgmVdcScale ) ,
                       RoundS( ext.XMax * cgmVdcScale ) ,
                       RoundS( ext.YMax * cgmVdcScale )
                     ) ;
    w := Max( Abs(cgmVDCEXT.Right-cgmVDCEXT.Left), 1 ) ;
    h := Max( Abs(cgmVDCEXT.Top-cgmVDCEXT.Bottom), 1 ) ;

    FNativeHeight := h ;
    FNativeWidth  := w ;
    FNativeSize := Max( FNativeHeight, FNativeWidth  ) ;
    if FNativeSize = 0 then
      FNativeSize := 1 ;
  end ;

  procedure TGIS_SymbolCGM.doVDCINTEGERPREC ;
  var
    v1, v2 : Integer ;
  begin
    if FBinary then begin
      cgmVDCINTEGERPREC := getTokenWord ;
    end
    else begin
      v1 := getTokenInteger ;
      v2 := getTokenInteger ;
      cgmINTEGERPREC := length( IntToStr( Max( Abs(v1), Abs(v2) ) ) ) ;
    end ;
  end ;

  procedure TGIS_SymbolCGM.doVDCREALPREC ;
  begin
    if FBinary then begin
      getTokenWord ;
      cgmVDCREALPREC := getTokenWord ;
    end
    else begin
      getTokenFloat ;
      getTokenFloat ;
      cgmVDCREALPREC := getTokenWord ;
    end;
  end ;

  procedure TGIS_SymbolCGM.doCOLRTABLE ;
  var
    cnt   : Integer ;
    r,g,b : Integer ;
  begin
    cnt := getTokenByte ;
    repeat
      if cnt > high( cgmCOLRTABLE ) then exit ;

      r := getTokenByte ;
      g := getTokenByte ;
      b := getTokenByte ;
      cgmCOLRTABLE[cnt] := TGIS_Color.FromRGB( r,g,b ) ;
      inc( cnt ) ;
    until endMarker ;
  end ;

  procedure TGIS_SymbolCGM.doINTSTYLE ;
  var
    stmp : String ;

    function t( const _val : String ) : Boolean ;
    begin
      Result := CompareText( _val, stmp ) = 0 ;
    end ;
  begin
    if FBinary then begin
      case getTokenWord of
       0   : cgmINTSTYLE := TGIS_BrushStyle.Clear ;
       1   : cgmINTSTYLE := TGIS_BrushStyle.Solid ;
       4   : cgmINTSTYLE := TGIS_BrushStyle.Clear ;
       else  cgmINTSTYLE := TGIS_BrushStyle.Solid ;
      end ;
    end
    else begin
      stmp := getTokenClearText ;

      if      t( CGM_SOLID  ) then cgmINTSTYLE := TGIS_BrushStyle.Solid
      else if t( CGM_HOLLOW ) then cgmINTSTYLE := TGIS_BrushStyle.Clear
      else if t( CGM_EMPTY  ) then cgmINTSTYLE := TGIS_BrushStyle.Clear
      else                         cgmINTSTYLE := TGIS_BrushStyle.Solid ;
    end ;
  end ;

  procedure TGIS_SymbolCGM.doFILLCOLR ;
  begin
    cgmFILLCOLR := getTokenColor ;
  end ;

  procedure TGIS_SymbolCGM.doEDGECOLR ;
  begin
    cgmEDGECOLR := getTokenColor ;
  end ;

  procedure TGIS_SymbolCGM.doEDGEWIDTH ;
  var
   ival : Integer ;
  begin
    case cgmLINEWIDTHMODE of
      0  :  ival := getTokenInteger ;
      1  :  ival := RoundS( getTokenFloat * 1440 / 25.4 ) ; // aproximation
      2  :  ival := RoundS( getTokenFloat * ( cgmVDCEXT.Right - cgmVDCEXT.Left )) ;
      3  :  ival := RoundS( getTokenFloat * 96 / 25.4 ) ;   // aproximation
      else  ival := 0 ;
    end ;

    assert( ival >= 0 ) ;

    cgmEDGEWIDTH := ival ;
  end ;

  procedure TGIS_SymbolCGM.doEDGEVIS ;
  var
    stmp : String ;

    function t( const _val : String ) : Boolean ;
    begin
      Result := CompareText( _val, stmp ) = 0 ;
    end ;
  begin
    if FBinary then begin
      cgmEDGEVIS := Boolean( getTokenWord ) ;
    end
    else begin
      stmp := getTokenClearText ;

      if t( CGM_ON  ) then cgmEDGEVIS := True
                      else cgmEDGEVIS := False ;
    end ;
  end ;

  procedure TGIS_SymbolCGM.doLINECOLR ;
  begin
    cgmLINECOLR := getTokenColor ;
  end ;

  procedure TGIS_SymbolCGM.doLINEWIDTH ;
  var
   ival : Integer ;
  begin
    case cgmLINEWIDTHMODE of
      0  :  ival := getTokenInteger ;
      1  :  ival := RoundS( getTokenFloat * 1440 / 25.4 ) ; // approximation
      2  :  ival := RoundS( getTokenFloat * ( cgmVDCEXT.Right - cgmVDCEXT.Left )) ;
      3  :  ival := RoundS( getTokenFloat * 96 / 25.4 ) ;   // approximation
      else  ival := 0 ;
    end ;

    assert( ival >= 0 ) ;

    cgmLINEWIDTH := ival ;
  end ;

  procedure TGIS_SymbolCGM.doRECTANGLE ;
  var
    cnt  : Integer ;
    ptA  : TPoint  ;
    ptB  : TPoint  ;
    pt   : TPoint  ;
  begin
    prepareEntrySurface ;
    {$IFDEF GIS_NORECORDS}
      ptA := new TPoint(0,0) ;
      ptB := new TPoint(0,0) ;
      pt  := new TPoint(0,0) ;
    {$ENDIF}
    cnt := 0 ;
    repeat
      ptA.X :=   getTokenVdc ;
      ptA.Y := - getTokenVdc ;

      ptB.X :=   getTokenVdc ;
      ptB.Y := - getTokenVdc ;

      pt.X := ptA.X ;
      pt.Y := ptA.Y ;
      {$IFDEF MANAGED}
        metaStream.WriteInteger( pt.X ) ;
        metaStream.WriteInteger( pt.Y ) ;
      {$ELSE}
        metaStream.Write( pt, sizeOf( pt ) ) ;
      {$ENDIF}
      inc( cnt ) ;

      pt.X := ptB.X ;
      {$IFDEF MANAGED}
        metaStream.WriteInteger( pt.X ) ;
        metaStream.WriteInteger( pt.Y ) ;
      {$ELSE}
        metaStream.Write( pt, sizeOf( pt ) ) ;
      {$ENDIF}
      inc( cnt ) ;

      pt.Y := ptB.Y ;
      {$IFDEF MANAGED}
        metaStream.WriteInteger( pt.X ) ;
        metaStream.WriteInteger( pt.Y ) ;
      {$ELSE}
        metaStream.Write( pt, sizeOf( pt ) ) ;
      {$ENDIF}
      inc( cnt ) ;

      pt.X := ptA.X ;
      {$IFDEF MANAGED}
        metaStream.WriteInteger( pt.X ) ;
        metaStream.WriteInteger( pt.Y ) ;
      {$ELSE}
        metaStream.Write( pt, sizeOf( pt ) ) ;
      {$ENDIF}
      inc( cnt ) ;

      pt.Y := ptA.Y ;
      {$IFDEF MANAGED}
        metaStream.WriteInteger( pt.X ) ;
        metaStream.WriteInteger( pt.Y ) ;
      {$ELSE}
        metaStream.Write( pt, sizeOf( pt ) ) ;
      {$ENDIF}
      inc( cnt ) ;

    until endMarker ;

    finishEntry( cnt ) ;
  end ;

  procedure TGIS_SymbolCGM.doPOLYGON ;
  var
    cnt  : Integer  ;
    pt   : TPoint   ;
  begin
    prepareEntrySurface ;
    {$IFDEF GIS_NORECORDS}
      pt := new TPoint(0,0) ;
    {$ENDIF}
    cnt := 0 ;
    repeat
      pt.X :=   getTokenVdc ;
      pt.Y := - getTokenVdc ;

      {$IFDEF MANAGED}
        metaStream.WriteInteger( pt.X ) ;
        metaStream.WriteInteger( pt.Y ) ;
      {$ELSE}
        metaStream.Write( pt, sizeOf( pt ) ) ;
      {$ENDIF}

      inc( cnt ) ;
    until endMarker ;

    finishEntry( cnt ) ;
  end ;

  procedure TGIS_SymbolCGM.doPOLYGONSET ;
  var
    cnt  : Integer  ;
    pt   : TPoint   ;
  begin
    prepareEntrySurface ;
    {$IFDEF GIS_NORECORDS}
      pt := new TPoint(0,0) ;
    {$ENDIF}
    cnt := 0 ;
    repeat
      pt.X :=   getTokenVdc ;
      pt.Y := - getTokenVdc ;
      getTokenVdc ; //?

      {$IFDEF MANAGED}
        metaStream.WriteInteger( pt.X ) ;
        metaStream.WriteInteger( pt.Y ) ;
      {$ELSE}
        metaStream.Write( pt, sizeOf( pt ) ) ;
      {$ENDIF}

      inc( cnt ) ;
    until endMarker ;

    finishEntry( cnt ) ;
  end ;

  procedure TGIS_SymbolCGM.doLINE ;
  var
    cnt : Integer ;
    pt   : TPoint   ;
  begin
    prepareEntryLine ;
    {$IFDEF GIS_NORECORDS}
      pt := new TPoint(0,0) ;
    {$ENDIF}
    cnt := 0 ;
    repeat
      pt.X :=   getTokenVdc ;
      pt.Y := - getTokenVdc ;

      {$IFDEF MANAGED}
        metaStream.WriteInteger( pt.X ) ;
        metaStream.WriteInteger( pt.Y ) ;
      {$ELSE}
        metaStream.Write( pt, sizeOf( pt ) ) ;
      {$ENDIF}

      inc( cnt ) ;
    until endMarker ;

    finishEntry( cnt ) ;
  end ;

  procedure TGIS_SymbolCGM.doCIRCLE ;
  var
    x, y : Integer ;
    r : Integer ;
  begin
    prepareEntrySurface ;

    // center
       x :=   getTokenVdc ;
       y := - getTokenVdc ;
    // radius
       r := Max( 1, getTokenVdc ) ;

    finishEntry( ellipticArc( x, y, x-r, y, x, y + r, x-r, y, x-r, y ) ) ;
  end ;

  procedure TGIS_SymbolCGM.doARCCTR ;
  var
    x, y : Integer ;
    sx, sy : Integer ;
    ex, ey : Integer ;
    r : Integer ;
  begin
    prepareEntryLine ;

    // center
       x :=   getTokenVdc ;
       y := - getTokenVdc ;
    // start point
       sx :=   getTokenVdc + x ;
       sy := - getTokenVdc + y ;
    // end point
       ex :=   getTokenVdc + x ;
       ey := - getTokenVdc + y ;
    // radius
       r := Max( 1, getTokenVdc ) ;

    finishEntry( ellipticArc( x, y, x-r, y, x, y + r, sx, sy, ex, ey ) ) ;
  end ;

  procedure TGIS_SymbolCGM.doELLIPSE ;
  var
    x, y : Integer ;
    xa, ya : Integer ;
    xb, yb : Integer ;
  begin
    prepareEntrySurface ;

    // center
       x  :=   getTokenVdc ;
       y  := - getTokenVdc ;
    // first axis
       xa :=   getTokenVdc ;
       ya := - getTokenVdc ;
    // second axis
       xb :=   getTokenVdc ;
       yb := - getTokenVdc ;

    finishEntry( ellipticArc( x, y, xa, ya, xb, yb, xa, ya, xa, ya ) ) ;
  end ;

  procedure TGIS_SymbolCGM.doELLIPARC ;
  var
    x, y : Integer ;
    xa, ya : Integer ;
    xb, yb : Integer ;
    xs, ys : Integer ;
    xe, ye : Integer ;
  begin
    prepareEntryLine ;

    // center
       x  :=   getTokenVdc ;
       y  := - getTokenVdc ;
    // first axis
       xa :=   getTokenVdc ;
       ya := - getTokenVdc ;
    // second axis
       xb :=   getTokenVdc ;
       yb := - getTokenVdc ;
    // start point
       xs :=   getTokenVdc + x ;
       ys := - getTokenVdc + y ;
    // end point
       xe :=   getTokenVdc + x ;
       ye := - getTokenVdc + y ;

    finishEntry( ellipticArc( x,y, xa,ya, xb,yb, xs,ys, xe,ye ) ) ;
  end ;

  procedure TGIS_SymbolCGM.setPosition(
    const _position : TGIS_SymbolPosition
  ) ;
  var
    vsize : Integer ;
  begin
    if not FAutoCenter then begin
      vsize := Max(  cgmVDCEXT.Right  - cgmVDCEXT.Left,
                    -cgmVDCEXT.Bottom - cgmVDCEXT.Top
                  ) ;

      case _position of
        TGIS_LabelPosition.UpLeft       : begin
                                            symOffset := Point(
                                                           - cgmVDCEXT.Right,
                                                           - cgmVDCEXT.Bottom
                                                         ) ;
                                          end ;
        TGIS_LabelPosition.UpCenter     : begin
                                            symOffset := Point(
                                                           0, - cgmVDCEXT.Bottom
                                                         ) ;
                                          end ;
        TGIS_LabelPosition.UpRight      : begin
                                            symOffset := Point(
                                                           - cgmVDCEXT.Left,
                                                           - cgmVDCEXT.Bottom
                                                         ) ;
                                          end ;
        TGIS_LabelPosition.MiddleLeft   : begin
                                            symOffset := Point(
                                                          - cgmVDCEXT.Right, 0
                                                        ) ;
                                         end ;
        TGIS_LabelPosition.MiddleCenter : begin
                                            symOffset := Point(
                                                          0, 0
                                                        ) ;
                                         end ;
        TGIS_LabelPosition.MiddleRight  : begin
                                            symOffset := Point(
                                                           - cgmVDCEXT.Left, 0
                                                         ) ;
                                          end ;
        TGIS_LabelPosition.DownLeft     : begin
                                            symOffset := Point(
                                                           - cgmVDCEXT.Right,
                                                           - cgmVDCEXT.Top
                                                         ) ;
                                          end ;
        TGIS_LabelPosition.DownCenter   : begin
                                            symOffset := Point(
                                                           - cgmVDCEXT.Right,
                                                           - cgmVDCEXT.Top
                                                         ) ;
                                          end ;
        TGIS_LabelPosition.DownRight    : begin
                                            symOffset := Point(
                                                           - cgmVDCEXT.Left,
                                                           - cgmVDCEXT.Top
                                                         ) ;
                                          end ;
      end ;

      if vsize <> 0 then
        symOffset := Point( symOffset.X +
                            RoundS( FCenter.X * vsize / 100.0 ),
                            symOffset.Y
                          ) ;
      if vsize <> 0 then
        symOffset := Point( symOffset.X,
                            symOffset.Y +
                            RoundS( FCenter.Y * vsize / 100.0 )
                          ) ;

    end
    else
      symOffset := Point(
                     symOffset.X -
                     ( cgmVDCEXT.Right  + cgmVDCEXT.Left ) div 2,
                     symOffset.Y +
                     ( cgmVDCEXT.Bottom + cgmVDCEXT.Top  ) div 2
                   ) ;

    symOffset := Point( toPixelI( symOffset.X ),
                        toPixelI( symOffset.Y )
                      ) ;

  end ;

  procedure TGIS_SymbolCGM.ClearCache ;
  begin
    FreeObject( metaStream ) ;
  end ;

  procedure TGIS_SymbolCGM.Draw(
    const _x : Integer ;
    const _y : Integer
  ) ;
  var
    i      : Integer ;
    pt     : TPoint  ;
    {$IFDEF OXYGENE}
      pt_X : Integer ;
      pt_Y : Integer ;
    {$ENDIF}
    rnd    : TGIS_RendererAbstract ;
    pnts   : TGIS_IntegerArray ;
  begin
    if symSize < 1 then exit ;

    rnd := TGIS_RendererAbstract( symRenderer ) ;

    try
      if metaStream = nil then parseFile ;
      metaStream.Position := 0 ;

      while True do begin
        {$IFDEF OXYGENE}
          T_SymbolCgmPrimitive( cgmMeta ).Read( metaStream ) ;
        {$ELSE}
          metaStream.Read( cgmMeta^, sizeOf( T_SymbolCgmPrimitive ) ) ;
        {$ENDIF}

        {$IFDEF OXYGENE}
          with T_SymbolCgmPrimitive(cgmMeta)  do begin
        {$ELSE}
          with T_SymbolCgmPrimitive(cgmMeta^) do begin
        {$ENDIF}
          if Kind = CGM_ENTRY_END then break ;

          rnd.CanvasBrush.Color := realizeColor1(
                                     TGIS_Color.FromARGB( BrushColor ) ) ;
          rnd.CanvasBrush.Style := BrushStyle  ;

          rnd.CanvasPen.Width   := Max( 1, toPixelI( PenWidth ) ) ;
          rnd.CanvasPen.Style   := PenStyle    ;
          if rnd.CanvasPen.Style <> TGIS_PenStyle.Clear then
            rnd.CanvasPen.Color := realizeColor2(
                                     TGIS_Color.FromARGB( PenColor ) ) ;

          if rnd.CanvasBrush.Style = TGIS_BrushStyle.Clear then
            rnd.CanvasPen.Color := realizeColor1(
                                     TGIS_Color.FromARGB( PenColor ) ) ;

          if Points > high( drawBuf ) then
            SetLength( drawBuf, Points ) ;

          for i:=0 to Points-1 do begin
            {$IFDEF OXYGENE}
              metaStream.ReadInteger( pt_X );
              metaStream.ReadInteger( pt_Y );

              {$IFDEF GIS_NORECORDS}
                pt := new TPoint(0,0) ;
              {$ENDIF}
              pt.X := pt_X ;
              pt.Y := pt_Y ;
            {$ELSE}
              metaStream.Read( pt, SizeOf( pt ) ) ;
            {$ENDIF}
            drawBuf[ i ]   := scaleAndRotate( pt ) ;
            drawBuf[ i ].X := drawBuf[ i ].X + _x + symOffset.X ;
            drawBuf[ i ].Y := drawBuf[ i ].Y + _y + symOffset.Y ;
          end ;

          SetLength( pnts, 1 ) ;
          pnts[0] := Points ;
          if Kind = CGM_ENTRY_LINE then
            if (drawBuf[0].X = drawBuf[ Points -1 ].X) and
               (drawBuf[0].Y = drawBuf[ Points -1 ].Y) then
              // looped line - treat as polygon
              rnd.CanvasDrawPolygon( drawBuf, pnts )
            else
              rnd.CanvasDrawPolyLine( drawBuf, pnts[0] )
          else
            rnd.CanvasDrawPolygon( drawBuf, pnts ) ;
        end ;
      end ;
    except
    end ;
  end ;

//==============================================================================
// TGIS_SymbolLineHelper
//==============================================================================

  class procedure TGIS_SymbolLineHelper.drawFromSymbols(
    const _viewer  : IGIS_Viewer         ;
    const _drawbuf : TGIS_DrawBufF       ;
    const _sym     : TGIS_SymbolAbstract ;
    const _cnt     : Integer
  );
  var
    angle   : Single  ;
    i       : Integer ;
    len     : Single  ;
    wdth    : Single  ;
    curpos  : Single ;
    curlen  : Single ;
    lastlen : Single ;
    delta   : Single ;
    sa, ca  : Single  ;
    dx, dy  : Single  ;
    x, y    : Single  ;
    {$IFDEF CLR}
      rct   : System.Drawing.Rectangle ;
    {$ELSE}
      rct   : TRect   ;
    {$ENDIF}

    function vistest : Boolean ;
    begin
      Result := False ;

      if      x < rct.Left   - _sym.Size then exit
      else if x > rct.Right  + _sym.Size then exit
      else if y < rct.Top    - _sym.Size then exit
      else if y > rct.Bottom + _sym.Size then exit
      else Result := True ;
    end ;


  begin
    rct := Rect( 0, 0, IGIS_Viewer(_viewer).ViewerParent.ControlCanvasWidth,
                       IGIS_Viewer(_viewer).ViewerParent.ControlCanvasHeight ) ;

    SinCos( _sym.symAngle, sa, ca ) ;
    dx := ca * _sym.getRealSymWidth ;
    dy := sa * _sym.getRealSymHeight ;

    wdth := ( Max( Abs( dx ), Abs(dy) ) ) - 0 + _sym.symGap ;
    if wdth < 2 then wdth := 2 ;

    
    curpos := wdth / 2 ;
    lastlen := 0 ;
    curlen := 0 ;

    i := -1 ;
    while true do begin

      if curpos < curlen then begin
        angle := ArcTan2( _drawbuf[i+1].Y - _drawbuf[i].Y,
                          _drawbuf[i+1].X - _drawbuf[i].X ) ;
        SinCos( angle, sa, ca ) ;
        delta := curpos - lastlen ;

        x := ( delta * ca ) + _drawbuf[i].X ;
        y := ( delta * sa ) + _drawbuf[i].Y ;

        if vistest then begin
          _sym.prepareRotate( angle + _sym.symAngle, False ) ;
          _sym.Draw( x, y ) ;
        end ;

        curpos := curpos + wdth ;
      end
      else begin
        inc( i ) ;
        if i > _cnt - 2 then
          break;
        len := Sqrt( Sqr( _drawbuf[i+1].Y - _drawbuf[i].Y ) +
                     Sqr( _drawbuf[i+1].X - _drawbuf[i].X )
                   ) ;
        lastlen := curlen ;
        if i < _cnt -2 then
          curlen := curlen + len
        else
          curlen := curlen + len - wdth/4 ;
      end;
    end ;

  end;

  class procedure TGIS_SymbolLineHelper.drawDashLine(
    const _viewer  : IGIS_Viewer         ;
    const _drawbuf : TGIS_DrawBufF       ;
    const _sym     : TGIS_SymbolAbstract ;
    const _cnt     : Integer
  );
  var
    {$IFDEF DCC}
      [weak]
    {$ENDIF}
    viewer   : IGIS_Viewer ;
    canvas   : TGIS_RendererAbstract ;
    cnt      : Integer     ;

    i        : Integer     ;
    len      : Integer     ;
    size     : Integer     ;

    last_pos : Integer     ;
    pt1      : TGIS_Point  ;
    pt2      : TGIS_Point  ;
    off      : Integer     ;
    res      : Boolean     ;
    fg_color : TGIS_Color  ;
    bk_color : TGIS_Color  ;
    color    : TGIS_Color  ;
    buf      : TGIS_DrawBufF ;

    procedure set_buffer ;
    begin
      if cnt > size then begin
        inc( size ) ;

        SetLength( buf, size + 1 ) ;
      end ;
      buf[ cnt ] := PointF( RoundS( pt1.X ), RoundS( pt1.Y ) ) ;
      inc( cnt ) ;
    end ;

    function eat_line( _width : Integer ) : Boolean ;
    var
      w    : Single ;
      wdth : Single ;

    begin
      wdth := _width ;

      cnt := 0 ;
      set_buffer ;

      while last_pos < _cnt - 1 do begin
        pt2 := GisPoint( _drawbuf[last_pos+1].X,
                         _drawbuf[last_pos+1].Y
                       ) ;

        w := Sqrt( Sqr( pt1.X - pt2.X ) + Sqr( pt1.Y - pt2.Y ) ) ;

        if wdth < w then begin
          // set new temporary point
          pt1.X := wdth/w * ( pt2.X - pt1.X ) + pt1.X ;
          pt1.Y := wdth/w * ( pt2.Y - pt1.Y ) + pt1.Y ;
          set_buffer ;
          Result := True ;
          exit ;
        end
        else begin
          // add point to output
          pt1 := _TGIS_Point( pt2 ) ;
          set_buffer ;
          inc( last_pos ) ;
          wdth := wdth - w ;
        end ;
      end ;

      Result := False ;
    end ;
  begin
    viewer := IGIS_Viewer(_viewer) ;
    canvas := TGIS_RendererAbstract( viewer.ViewerParent.ControlRenderer ) ;

    size := high( buf ) ;
    last_pos   := 0 ;
    i := 0 ;
    pt1 := GisPoint( _drawbuf[0].X, _drawbuf[0].Y ) ;

    fg_color := _sym.symColor1 ;
    bk_color := viewer.Color ;

    if length( TGIS_SymbolLine( _sym ).arStyle ) >= 2 then begin
      // valid line

      while True do begin
        off := i mod ( high( TGIS_SymbolLine( _sym ).arStyle ) + 1 ) ;
        len := TGIS_SymbolLine( _sym ).arStyle[ off ] ;
        if len > 0 then len := canvas.TwipsToPixels( len )
                   else len := Abs( len ) * _sym.Size ;
        if len = 0 then break ;

        res := eat_line( len ) ;

        if (off mod 2) = 0 then begin
          color := fg_color ;
          canvas.CanvasPen.Color := color ;
          canvas.CanvasPen.Width := _sym.Size ;
          canvas.CanvasDrawPolyLine( buf, cnt ) ;
        end
        else begin
          // do nothing
        end ;


        if not res then exit ;
        inc( i ) ;
      end ;
    end
    else begin
      // invalid line
      canvas.CanvasDrawPolyLine( _drawbuf ) ;
    end ;

  end ;

  class procedure TGIS_SymbolLineHelper.DrawLine(
    const _viewer  : IGIS_Viewer         ;
    const _drawbuf : TGIS_DrawBufF       ;
    const _sym     : TGIS_SymbolAbstract ;
    const _cnt     : Integer
  );
  begin
    if _cnt <= 0 then exit ;

   if _sym is TGIS_SymbolLine then begin
      // build line using continuous line
      drawDashLine( _viewer, _drawbuf, _sym, _cnt ) ;
    end
    else if _sym is TGIS_SymbolLineEx then begin
      // build line using continuous line
      TGIS_SymbolLineEx( _sym ).DrawLine( _drawbuf, _cnt ) ;
    end
    else begin
      // build line from individual symbols
      drawFromSymbols( _viewer, _drawbuf, _sym, _cnt )
    end ;
  end;

  function SymbolList : TGIS_SymbolList ;
  var
    thc : TGIS_ThreadClass ;
  begin
    if not assigned( oSymbolList ) then begin
      thc := TGIS_ThreadClass.Create ;
      try
        thc.LockThread ;
        try
          if not assigned( oSymbolList ) then
            oSymbolList
              := TGIS_SymbolList.Create ;
        finally
          thc.UnlockThread ;
        end;
      finally
        FreeObject( thc );
      end;
    end;

    Result := oSymbolList ;
  end ;

//==============================================================================
// TGIS_SymbolLine
//==============================================================================

  constructor TGIS_SymbolLine.Create( const _name : String ) ;
  var
    i      : Integer ;
    c      : Char    ;
    sbegin : String  ;
    send   : String  ;
    sstyle : String  ;
    sval   : String  ;
    state  : Integer ;
    chs1   : TCharSet ;
    chs2   : TCharSet ;
    chs3   : TCharSet ;

    procedure addtostyle( const _txt : String ) ;
    begin
      if not IsStringEmpty( _txt ) then begin
        SetLength( arStyle, length( arStyle ) + 1 ) ;
        arStyle[ length( arStyle ) - 1 ] := StrToIntDef( _txt, 0 ) ;
        if arStyle[ length( arStyle ) - 1 ] = 0 then
          SetLength( arStyle, length( arStyle ) - 1 ) ;
      end ;
    end ;

  begin
    inherited Create( _name );

    // split symbol
      sbegin := '' ;
      send   := '' ;
      sstyle := '' ;
      state := 0 ;
      for i:=StringFirst to StringLast( _name ) do begin
        c := _name[ i ] ;
        case state of
          0 : if      c = '[' then state := 1
              else if c = ' ' then continue
              else                 sbegin := sbegin + c ;

          1 : if      c = ']' then state := 2
              else if c = ' ' then continue
              else                 sstyle := sstyle + c ;
          2 : if      c = ' ' then continue
              else                 send := send + c ;
        end ;
      end ;
    // build style
      state := 0  ;
      sval  := '' ;
      SetLength( arStyle, 0 ) ;

      chs1 := PrepareCharSet( [ '09'     ] ) ;
      chs2 := PrepareCharSet( [ 'w','W'  ] ) ;
      chs3 := PrepareCharSet( [ 't','T'  ] ) ;

      for i:=StringFirst to StringLast( sstyle ) do begin
        c := sstyle[ i ] ;
        case state of
          0 : if      InCharSet( c, chs1 ) then begin
                                                     sval  := c ;
                                                     state := 1 ;
                                                   end ;
          1 : if      InCharSet( c, chs1 ) then
                                                     sval := sval + c
              else if InCharSet( c, chs2 ) then begin
                                                     addtostyle( '-'+sval ) ;
                                                     sval := '' ;
                                                     state := 0 ;
                                                   end
              else if InCharSet( c, chs3 ) then begin
                                                     addtostyle( '+'+sval ) ;
                                                     sval := '' ;
                                                     state := 0 ;
                                                   end
        end ;
      end ;

    FWidth  := getRealSymWidth ;
    FHeight := getRealSymHeight ;
  end ;

  procedure TGIS_SymbolLine.doDestroy ;
  begin
    inherited ;
  end ;

  function TGIS_SymbolLine.getSymSize
    : Integer ;
  begin
    Result := RoundS( symSize ) ;
  end ;

  function TGIS_SymbolLine.getRealSymWidth
    : Integer ;
  begin
    Result := 1;
  end ;

  function TGIS_SymbolLine.getRealSymHeight
    : Integer ;
  begin
    Result := 1;
  end ;

  procedure TGIS_SymbolLine.Draw(
    const _x : Integer ;
    const _y : Integer
  ) ;
  begin
  end ;

//==============================================================================
// TGIS_SymbolLineEx
//==============================================================================

  constructor TGIS_SymbolLineEx.Create( const _name : String ) ;
  begin
    inherited Create( _name ) ;

    sDefinition := _name ;
    iDefinition := StringFirst ;

    while getToken do begin
      case iTokenType of
        TOKEN_VALUE_DEFAULT  ,
        TOKEN_VALUE_PERCENT  ,
        TOKEN_VALUE_PIXELS   ,
        TOKEN_VALUE_TWIPS    ,
        TOKEN_VALUE_WIDTH    ,
        TOKEN_VALUE_SWIDTH   ,
        TOKEN_VALUE_SCRPIX   :
          begin
            debug_token ;
          end ;
        TOKEN_ENDCOMMAND :
          begin
            debug_token ;
          end ;
        TOKEN_LOOPBEGIN  :
          begin
            debug_token ;
            parseLoopBegin ;
          end ;
        TOKEN_LOOPEND    :
          begin
            debug_token ;
            parseLoopEnd ;
          end ;
        TOKEN_GOTO       :
          begin
            debug_token  ;
            parseGoto    ;
          end ;
        TOKEN_MOVE       :
          begin
            debug_token ;
            parseMove ;
          end ;
        TOKEN_LINE       :
          begin
            debug_token ;
            parseLine ;
          end ;
        TOKEN_DRAW       :
          begin
            debug_token ;
            parseDraw ;
          end ;
        TOKEN_OUTLINE    :
          begin
            debug_token ;
            parseOutline ;
          end ;
        TOKEN_FILL       :
          begin
            debug_token ;
            parseFill ;
          end ;
        TOKEN_COLOR      :
          begin
            debug_token ;
            parseColor ;
          end ;
        TOKEN_WIDTH      :
          begin
            debug_token ;
            parseWidth ;
          end ;
        TOKEN_UNKNOWN    :
          begin
            debug_token ;
          end ;
        else
          begin
            debug_token ;
            raise EGIS_Exception.Create( ERR_SYNTAX, ERR_UNKNOWN_TOKEN, iDefinition )
          end ;

      end ;
    end ;

    FWidth  := getRealSymWidth ;
    FHeight := getRealSymHeight ;
  end ;

  procedure TGIS_SymbolLineEx.doDestroy ;
  begin
    inherited ;
  end ;

  function TGIS_SymbolLineEx.getSymSize
    : Integer ;
  begin
    Result := RoundS( symSize ) ;
  end ;

  function TGIS_SymbolLineEx.getRealSymWidth
    : Integer ;
  begin
    Result := 1;
  end ;

  function TGIS_SymbolLineEx.getRealSymHeight
    : Integer ;
  begin
    Result := 1;
  end ;

  procedure TGIS_SymbolLineEx.Draw(
    const _x : Integer ;
    const _y : Integer
  ) ;
  begin
  end ;

  //?
  function TGIS_SymbolLineEx.getChar
    : Char ;
  begin
    if iDefinition <= StringLast( sDefinition ) then begin
      Result := sDefinition[iDefinition] ;
      inc( iDefinition ) ;
    end
    else
      Result := #0 ;
  end;

  procedure TGIS_SymbolLineEx.undoChar ;
  begin
    dec( iDefinition ) ;
  end;

  function  TGIS_SymbolLineEx.getToken
    : Boolean  ;
  var
    c     : Char    ;
    cmd   : String  ;
    state : Integer ;
    chs1  : TCharSet ;
    chs2  : TCharSet ;

  const
    STATE_INITIAL       =   0 ;
    STATE_COMMENT       =   1 ;
    STATE_BEGINTOKEN    =   2 ;
    STATE_ENDTOKEN      =   3 ;
    STATE_LOOPBEGIN     =   4 ;
    STATE_LOOPEND       =   5 ;
    STATE_NUMBER        =   6 ;
    STATE_COLLECTNUMBER =   7 ;
    STATE_COLLECTMODE   =   8 ;
    STATE_COMMAND       =   9 ;
    STATE_ERROR         = 999 ;

  begin
    Result := False ;

    iTokenType  := 0 ;
    iTokenValue := 0 ;

    state := STATE_INITIAL ;
    c := getChar ;

    if c = #0 then begin
      exit ;
    end;

    if c = '&' then begin
      c := getChar ;
    end;

    chs1 := PrepareCharSet( ['-', '+', '09' ] ) ;
    chs2 := PrepareCharSet( ['AZ'] ) ;

    while c <> #0 do begin
      case state of
        STATE_INITIAL :
          begin
            if      c = #13         then begin
                                           c := getChar ;
                                           continue ;
                                         end
            else if c = #10         then begin
                                           c := getChar ;
                                           continue ;
                                         end
            else if c = ';'         then state := STATE_COMMENT
            else if c = ' '         then begin
                                           c := getChar ;
                                           continue ;
                                         end
            else if c = ','         then begin
                                           c := getChar ;
                                           continue ;
                                         end
            else if c = ')'         then state := STATE_ENDTOKEN
            else if InCharSet(c, chs1)
                                    then state := STATE_NUMBER
            else if InCharSet(c, chs2)
                                    then state := STATE_COMMAND
            else                         state := STATE_ERROR
          end ;
        STATE_COMMENT :
            begin
              c := getChar ;
              if c = #13 then
                state := STATE_INITIAL ;
            end ;
        STATE_ENDTOKEN :
          begin
            iTokenType  := TOKEN_ENDCOMMAND ;
            Result := True ;
            break ;
          end ;
        STATE_NUMBER :
          begin
            iTokenType  := TOKEN_VALUE_DEFAULT ;
            if InCharSet(c, chs1) then begin
              cmd := cmd + c ;
              c := getChar ;
            end ;
            case c of
              '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' :
                begin
                  // do nothing
                end;
              '-', '+' :
                begin
                  undoChar ;
                  iTokenValue := StrToInt( cmd ) ;
                  Result := True ;
                  break ;
                end;
              ' ' :
                begin
                  iTokenValue := StrToInt( cmd ) ;
                  Result := True ;
                  break ;
                end;
              ',' :
                begin
                  iTokenValue := StrToInt( cmd ) ;
                  Result := True ;
                  break ;
                end;
              ')' :
                begin
                  iTokenValue := StrToInt( cmd ) ;
                  undoChar ;
                  Result := True ;
                  break ;
                end;
              '%' :
                begin
                  //?
                  iTokenValue := StrToInt( cmd ) ;
                  iTokenType := TOKEN_VALUE_PERCENT ;
                  Result := True ;
                  break ;
                end;
              'P' :
                begin
                  iTokenValue := StrToInt( cmd ) ;
                  iTokenType := TOKEN_VALUE_SCRPIX ;
                  Result := True ;
                  break ;
                end;
              'X' :
                begin
                  //?
                  iTokenValue := StrToInt( cmd ) ;
                  iTokenType := TOKEN_VALUE_PIXELS ;
                  Result := True ;
                  break ;
                end;
              'T' :
                begin
                  iTokenValue := StrToInt( cmd ) ;
                  iTokenType := TOKEN_VALUE_TWIPS ;
                  Result := True ;
                  break ;
                end;
              'W' :
                begin
                  iTokenValue := StrToInt( cmd ) ;
                  iTokenType := TOKEN_VALUE_WIDTH ;
                  Result := True ;
                  break ;
                end;
              'S' :
                begin
                  iTokenValue := StrToInt( cmd ) ;
                  iTokenType := TOKEN_VALUE_SWIDTH ;
                  Result := True ;
                  break ;
                end;
              else begin
                iTokenType := TOKEN_UNKNOWN ;
                iTokenValue := iDefinition - 1;
                Result := True ;
                break ;
              end;

            end ;
          end ;
        STATE_COMMAND :
          begin
            if c = '(' then begin
              cmd := Trim( cmd ) ;
              state := STATE_BEGINTOKEN ;
            end
            else begin
              cmd := cmd + c ;
              c := getChar ;
            end;
          end ;
        STATE_BEGINTOKEN :
          begin
            if      cmd = 'GOTO'    then iTokenType := TOKEN_GOTO
            else if cmd = 'G'       then iTokenType := TOKEN_GOTO
            else if cmd = 'MOVE'    then iTokenType := TOKEN_MOVE
            else if cmd = 'M'       then iTokenType := TOKEN_MOVE
            else if cmd = 'FOR'     then iTokenType := TOKEN_LOOPBEGIN
            else if cmd = 'F'       then iTokenType := TOKEN_LOOPBEGIN
            else if cmd = 'END'     then iTokenType := TOKEN_LOOPEND
            else if cmd = 'E'       then iTokenType := TOKEN_LOOPEND
            else if cmd = 'DRAW'    then iTokenType := TOKEN_DRAW
            else if cmd = 'D'       then iTokenType := TOKEN_DRAW
            else if cmd = 'OUTLINE' then iTokenType := TOKEN_OUTLINE
            else if cmd = 'O'       then iTokenType := TOKEN_OUTLINE
            else if cmd = 'FILL'    then iTokenType := TOKEN_FILL
            else if cmd = 'I'       then iTokenType := TOKEN_FILL
            else if cmd = 'DRAW'    then iTokenType := TOKEN_DRAW
            else if cmd = 'D'       then iTokenType := TOKEN_DRAW
            else if cmd = 'LINE'    then iTokenType := TOKEN_LINE
            else if cmd = 'L'       then iTokenType := TOKEN_LINE
            else if cmd = 'WIDTH'   then iTokenType := TOKEN_WIDTH
            else if cmd = 'W'       then iTokenType := TOKEN_WIDTH
            else if cmd = 'COLOR'   then iTokenType := TOKEN_COLOR
            else if cmd = 'C'       then iTokenType := TOKEN_COLOR
            else                    begin
                                      raise EGIS_Exception.Create( ERR_SYNTAX, ERR_UNKNOWN_TOKEN, iDefinition )
                                    end ;
            Result := True ;
            break ;
          end;
        STATE_ERROR :
          begin
            iTokenType  := TOKEN_ERROR ;
            Result := True ;
            break ;
          end ;
      end;

    end;
  end;

  procedure TGIS_SymbolLineEx.debug_token ;
  {$IFNDEF DEBUG_TOKEN}
    begin
    end ;
  {$ELSE}
    var
      msg : String ;
    begin
      case iTokenType of
        TOKEN_ENDCOMMAND     : msg := 'TOKEN_ENDCOMMAND'   ;
        TOKEN_LOOPBEGIN      : msg := 'TOKEN_LOOPBEGIN'        ;
        TOKEN_LOOPEND        : msg := 'TOKEN_LOOPEND'          ;
        TOKEN_GOTO           : msg := 'TOKEN_GOTO'             ;
        TOKEN_MOVE           : msg := 'TOKEN_MOVE'             ;
        TOKEN_LINE           : msg := 'TOKEN_LINE'             ;
        TOKEN_DRAW           : msg := 'TOKEN_DRAW'             ;
        TOKEN_OUTLINE        : msg := 'TOKEN_OUTLINE'          ;
        TOKEN_FILL           : msg := 'TOKEN_FILL'             ;
        TOKEN_COLOR          : msg := 'TOKEN_COLOR'            ;
        TOKEN_WIDTH          : msg := 'TOKEN_WIDTH'            ;
        TOKEN_VALUE_DEFAULT  : msg := 'TOKEN_VALUE_DEFAULT'    ;
        TOKEN_VALUE_PERCENT  : msg := 'TOKEN_VALUE_PERCE'      ;
        TOKEN_VALUE_PIXELS   : msg := 'TOKEN_VALUE_PIXEL'      ;
        TOKEN_VALUE_SCRPIX   : msg := 'TOKEN_VALUE_SCRPIX'     ;
        TOKEN_VALUE_TWIPS    : msg := 'TOKEN_VALUE_TWIPS'      ;
        TOKEN_VALUE_WIDTH    : msg := 'TOKEN_VALUE_WIDTH'      ;
        TOKEN_VALUE_SWIDTH   : msg := 'TOKEN_VALUE_SWIDTH'     ;
        else                   msg := 'BADTOKEN'               ;
      end;

      log := log + Format( '%s(%d,%d);',
                           [ msg, iTokenType, iTokenValue ]
                         ) ;
    end ;
  {$ENDIF}

  procedure TGIS_SymbolLineEx.addDef(
    const _command : Integer;
    const _val     : Integer
  ) ;
  begin
    SetLength( arDef, length( arDef ) + 1 ) ;
    {$IFDEF GIS_NORECORDS}
      arDef[ length( arDef ) -1 ] := new T_Command ;
    {$ENDIF}
    arDef[ length( arDef ) -1 ].Token := _command ;
    arDef[ length( arDef ) -1 ].Value := _val ;
  end;

  procedure TGIS_SymbolLineEx.parseLoopBegin ;
  var
    cnt : Integer ;
  begin
    cnt := 0 ;
    while getToken do begin
      case iTokenType of
        TOKEN_VALUE_DEFAULT  ,
        TOKEN_VALUE_PERCENT  ,
        TOKEN_VALUE_PIXELS   ,
        TOKEN_VALUE_TWIPS    ,
        TOKEN_VALUE_WIDTH    ,
        TOKEN_VALUE_SWIDTH   ,
        TOKEN_VALUE_SCRPIX   :
          begin
            debug_token ;
            inc( cnt ) ;
            addDef( iTokenType, iTokenValue ) ;
          end;
        TOKEN_ENDCOMMAND :
          begin
            debug_token ;
            addDef( iTokenType, 0 ) ;
            break ;
          end;
        else
          begin
            debug_token ;
            raise EGIS_Exception.Create( ERR_SYNTAX, ERR_UNKNOWN_TOKEN, iDefinition )
          end;
      end;
    end;

    if cnt > 1 then
      raise EGIS_Exception.Create( ERR_SYNTAX, ERR_BAD_ARGUMENT_COUNT, iDefinition ) ;

    if cnt = 0 then
      addDef( TOKEN_VALUE_PERCENT, 100 ) ;

    addDef( TOKEN_LOOPBEGIN, 0 ) ;
    stLoop := length( arDef ) ;
  end;

  procedure TGIS_SymbolLineEx.parseLoopEnd ;
  var
    cnt : Integer ;
  begin
    cnt := 0 ;
    while getToken do begin
      case iTokenType of
        TOKEN_VALUE_DEFAULT  ,
        TOKEN_VALUE_PERCENT  ,
        TOKEN_VALUE_PIXELS   ,
        TOKEN_VALUE_TWIPS    ,
        TOKEN_VALUE_WIDTH    ,
        TOKEN_VALUE_SWIDTH   ,
        TOKEN_VALUE_SCRPIX   :
          begin
            debug_token ;
            inc( cnt ) ;
            addDef( iTokenType, iTokenValue ) ;
          end;
        TOKEN_ENDCOMMAND :
          begin
            debug_token ;
            addDef( iTokenType, 0 ) ;
            break ;
          end;
        else
          begin
            debug_token ;
            raise EGIS_Exception.Create( ERR_SYNTAX, ERR_UNKNOWN_TOKEN, iDefinition )
          end;
      end;
    end;

    if cnt <> 0 then
      raise EGIS_Exception.Create( ERR_SYNTAX, ERR_BAD_ARGUMENT_COUNT, iDefinition ) ;

    addDef( TOKEN_LOOPEND, stLoop ) ;
  end;

  procedure TGIS_SymbolLineEx.parseGoto ;
  var
    cnt : Integer ;
  begin
    cnt := 0 ;

    while getToken do begin
      case iTokenType of
        TOKEN_VALUE_DEFAULT  ,
        TOKEN_VALUE_PERCENT  ,
        TOKEN_VALUE_PIXELS   ,
        TOKEN_VALUE_TWIPS    ,
        TOKEN_VALUE_WIDTH    ,
        TOKEN_VALUE_SWIDTH   ,
        TOKEN_VALUE_SCRPIX   :
          begin
            debug_token ;
            inc( cnt ) ;
            addDef( iTokenType, iTokenValue ) ;
          end;
        TOKEN_ENDCOMMAND :
          begin
            debug_token ;
            addDef( iTokenType, 0 ) ;
            break ;
          end;
        else
          begin
            debug_token ;
            raise EGIS_Exception.Create( ERR_SYNTAX, ERR_UNKNOWN_TOKEN, iDefinition )
          end;
      end;
    end;

    if cnt <> 1 then
      raise EGIS_Exception.Create( ERR_SYNTAX, ERR_BAD_ARGUMENT_COUNT, iDefinition ) ;

    addDef( TOKEN_GOTO, 0 ) ;
  end;

  procedure TGIS_SymbolLineEx.parseMove ;
  var
    cnt : Integer ;
  begin
    cnt := 0 ;

    while getToken do begin
      case iTokenType of
        TOKEN_VALUE_DEFAULT  ,
        TOKEN_VALUE_PERCENT  ,
        TOKEN_VALUE_PIXELS   ,
        TOKEN_VALUE_TWIPS    ,
        TOKEN_VALUE_WIDTH    ,
        TOKEN_VALUE_SWIDTH   ,
        TOKEN_VALUE_SCRPIX   :
          begin
            debug_token ;
            inc( cnt ) ;
            addDef( iTokenType, iTokenValue ) ;
          end;
        TOKEN_ENDCOMMAND :
          begin
            debug_token ;
            addDef( iTokenType, 0 ) ;
            break ;
          end;
        else
          begin
            debug_token ;
            raise EGIS_Exception.Create( ERR_SYNTAX, ERR_UNKNOWN_TOKEN, iDefinition )
          end;
      end;
    end;

    if cnt <> 2 then
      raise EGIS_Exception.Create( ERR_SYNTAX, ERR_BAD_ARGUMENT_COUNT, iDefinition ) ;

    addDef( TOKEN_MOVE, 0 ) ;
  end;

  procedure TGIS_SymbolLineEx.parseLine ;
  var
    cnt : Integer ;
  begin
    cnt := 0 ;

    while getToken do begin
      case iTokenType of
        TOKEN_VALUE_DEFAULT  ,
        TOKEN_VALUE_PERCENT  ,
        TOKEN_VALUE_PIXELS   ,
        TOKEN_VALUE_TWIPS    ,
        TOKEN_VALUE_WIDTH    ,
        TOKEN_VALUE_SWIDTH   ,
        TOKEN_VALUE_SCRPIX   :
          begin
            debug_token ;
            inc( cnt ) ;
            addDef( iTokenType, iTokenValue ) ;
          end;
        TOKEN_ENDCOMMAND :
          begin
            debug_token ;
            addDef( iTokenType, 0 ) ;
            break ;
          end;
        else
          begin
            debug_token ;
            raise EGIS_Exception.Create( ERR_SYNTAX, ERR_UNKNOWN_TOKEN, iDefinition )
          end;
      end;
    end;

    if cnt <> 1 then
      raise EGIS_Exception.Create( ERR_SYNTAX, ERR_BAD_ARGUMENT_COUNT, iDefinition ) ;

    addDef( TOKEN_LINE, 0 ) ;
  end ;

  procedure TGIS_SymbolLineEx.parseDraw ;
  var
    cnt : Integer ;
  begin
    cnt := 0 ;

    while getToken do begin
      case iTokenType of
        TOKEN_VALUE_DEFAULT  ,
        TOKEN_VALUE_PERCENT  ,
        TOKEN_VALUE_PIXELS   ,
        TOKEN_VALUE_TWIPS    ,
        TOKEN_VALUE_WIDTH    ,
        TOKEN_VALUE_SWIDTH   ,
        TOKEN_VALUE_SCRPIX   :
          begin
            debug_token ;
            inc( cnt ) ;
            addDef( iTokenType, iTokenValue ) ;
          end;
        TOKEN_ENDCOMMAND :
          begin
            debug_token ;
            addDef( iTokenType, 0 ) ;
            break ;
          end;
        else
          begin
            debug_token ;
            raise EGIS_Exception.Create( ERR_SYNTAX, ERR_UNKNOWN_TOKEN, iDefinition )
          end;
      end;
    end;

    if cnt mod 2 <> 0 then
      raise EGIS_Exception.Create( ERR_SYNTAX, ERR_BAD_ARGUMENT_COUNT, iDefinition ) ;

    addDef( TOKEN_DRAW, 0 ) ;
  end;

  procedure TGIS_SymbolLineEx.parseOutline ;
  var
    cnt : Integer ;
  begin
    cnt := 0 ;

    while getToken do begin
      case iTokenType of
        TOKEN_VALUE_DEFAULT  ,
        TOKEN_VALUE_PERCENT  ,
        TOKEN_VALUE_PIXELS   ,
        TOKEN_VALUE_TWIPS    ,
        TOKEN_VALUE_WIDTH    ,
        TOKEN_VALUE_SWIDTH   ,
        TOKEN_VALUE_SCRPIX   :
          begin
            debug_token ;
            inc( cnt ) ;
            addDef( iTokenType, iTokenValue ) ;
          end;
        TOKEN_ENDCOMMAND :
          begin
            debug_token ;
            addDef( iTokenType, 0 ) ;
            break ;
          end;
        else
          begin
            debug_token ;
            raise EGIS_Exception.Create( ERR_SYNTAX, ERR_UNKNOWN_TOKEN, iDefinition )
          end;
      end;
    end;

    if cnt mod 2 <> 0 then
      raise EGIS_Exception.Create( ERR_SYNTAX, ERR_BAD_ARGUMENT_COUNT, iDefinition ) ;

    addDef( TOKEN_OUTLINE, 0 ) ;
  end;

  procedure TGIS_SymbolLineEx.parseFill ;
  var
    cnt : Integer ;
  begin
    cnt := 0 ;

    while getToken do begin
      case iTokenType of
        TOKEN_VALUE_DEFAULT  ,
        TOKEN_VALUE_PERCENT  ,
        TOKEN_VALUE_PIXELS   ,
        TOKEN_VALUE_TWIPS    ,
        TOKEN_VALUE_WIDTH    ,
        TOKEN_VALUE_SWIDTH   ,
        TOKEN_VALUE_SCRPIX   :
          begin
            debug_token ;
            inc( cnt ) ;
            addDef( iTokenType, iTokenValue ) ;
          end;
        TOKEN_ENDCOMMAND :
          begin
            debug_token ;
            addDef( iTokenType, 0 ) ;
            break ;
          end;
        else
          begin
            debug_token ;
            raise EGIS_Exception.Create( ERR_SYNTAX, ERR_UNKNOWN_TOKEN, iDefinition )
          end;
      end;
    end;

    if cnt mod 2 <> 0 then
      raise EGIS_Exception.Create( ERR_SYNTAX, ERR_BAD_ARGUMENT_COUNT, iDefinition ) ;

    addDef( TOKEN_FILL, 0 ) ;
  end;

  procedure TGIS_SymbolLineEx.parseColor ;
  var
    cnt : Integer ;
  begin
    cnt := 0 ;

    while getToken do begin
      case iTokenType of
        TOKEN_VALUE_DEFAULT :
          begin
            debug_token ;
            inc( cnt ) ;
            addDef( iTokenType, iTokenValue ) ;
          end;
        TOKEN_ENDCOMMAND :
          begin
            debug_token ;
            break ;
          end;
        else
          begin
            debug_token ;
            raise EGIS_Exception.Create( ERR_SYNTAX, ERR_UNKNOWN_TOKEN, iDefinition )
          end;
      end;
    end;

    if ( cnt <> 3 ) and ( cnt <> 1 ) then
      raise EGIS_Exception.Create( ERR_SYNTAX, ERR_BAD_ARGUMENT_COUNT, iDefinition ) ;

    addDef( TOKEN_COLOR, 0 ) ;
  end;

  procedure TGIS_SymbolLineEx.parseWidth ;
  var
    cnt : Integer ;
  begin
    cnt := 0 ;

    while getToken do begin
      case iTokenType of
        TOKEN_VALUE_DEFAULT  ,
        TOKEN_VALUE_PERCENT  ,
        TOKEN_VALUE_PIXELS   ,
        TOKEN_VALUE_TWIPS    ,
        TOKEN_VALUE_WIDTH    ,
        TOKEN_VALUE_SWIDTH   ,
        TOKEN_VALUE_SCRPIX   :
          begin
            debug_token ;
            inc( cnt ) ;
            addDef( iTokenType, iTokenValue ) ;
          end;
        TOKEN_ENDCOMMAND :
          begin
            debug_token ;
            break ;
          end;
        else
          begin
            debug_token ;
            raise EGIS_Exception.Create( ERR_SYNTAX, ERR_UNKNOWN_TOKEN, iDefinition )
          end;
      end;
    end;

    if cnt <> 1 then
      raise EGIS_Exception.Create( ERR_SYNTAX, ERR_BAD_ARGUMENT_COUNT, iDefinition ) ;

    addDef( TOKEN_WIDTH, 0 ) ;
  end;

  procedure TGIS_SymbolLineEx.prepareLine(
     const _cnt    : Integer
  ) ;
  var
    pos        : Integer ;
    pt_a       : TPointF ;
    pt_b       : TPointF ;
    w          : Single  ;
    delta      : Single  ;
    tmp_angle  : Single  ;
    last_angle : Single  ;
  begin
    lineLen := 0 ;
    SetLength( lineBuf, _cnt ) ;
    last_angle := 0 ;
    w := 0 ;

    for pos := 0 to _cnt -1 do begin
      {$IFDEF GIS_NORECORDS}
        lineBuf[ pos ] := new T_LineEntry ;
      {$ENDIF}
      if pos = 0 then begin
        pt_b := _TPointF( arDrawBuf[ pos ] ) ;

        // we have no info about this
        lineBuf[ pos ].w      := w ;
        lineBuf[ pos ].w2     := w ;
        lineBuf[ pos ].tp     := 0 ;
        lineBuf[ pos ].secant := 0 ;
        lineBuf[ pos ].angle  := 0 ;

      end
      else begin
        pt_a := _TPointF( pt_b ) ;
        pt_b := _TPointF( arDrawBuf[ pos ] ) ;

        w := Sqrt( Sqr( pt_b.X - pt_a.X ) + Sqr( pt_b.Y - pt_a.Y ) ) ;

        lineBuf[ pos ].w  := w ;
        lineBuf[ pos ].w2 := lineLen ;
        lineBuf[ pos ].tp     := 0 ;
        lineBuf[ pos ].secant := 0 ;
        lineBuf[ pos ].angle  := 0 ;

        lineLen := lineLen + w ;

        tmp_angle := ArcTan2( pt_a.Y - pt_b.Y, pt_a.X - pt_b.X ) ;

        lineBuf[ pos-1 ].angle  := tmp_angle ;

        if pos > 1 then begin
          delta := last_angle - tmp_angle ;

          if      delta < 0 then lineBuf[ pos - 1 ].tp  := -1  // exterior
          else if delta > 0 then lineBuf[ pos - 1 ].tp  := 1   // interior
          else                   lineBuf[ pos - 1 ].tp  := 0 ; // flat

          lineBuf[ pos - 1 ].secant := delta / 2 ;
        end ;

        last_angle := tmp_angle ;
      end ;

    end ;
  end ;

  procedure TGIS_SymbolLineEx.clearBuffer ;
  begin
    SetLength( outBuf, 0 ) ;
  end;

  procedure TGIS_SymbolLineEx.drawPoint ;
  var
    l : Integer ;
    pt : TPointF {$IFDEF GIS_NORECORDS} := new TPointF(0, 0) {$ENDIF} ;
    sa, ca : Single ;
  begin
    l := length( outBuf ) ;
    SetLength( outBuf, l + 1 ) ;

    SinCos( refRot +Pi/2, sa, ca ) ;
    pt.X := RoundS( refX + ca * posY ) ;
    pt.Y := RoundS( refY + sa * posY ) ;

    outBuf[ l ] := pt ;
  end ;

  procedure TGIS_SymbolLineEx.drawPointSym ;
  var
    l : Integer ;
    pt : TPointF {$IFDEF GIS_NORECORDS} := new TPointF(0, 0) {$ENDIF} ;
    ssin : Single ;
    scos : Single ;
  begin
    l := length( outBuf ) ;
    { TODO -cImprove : Verify .NET performance }
    SetLength( outBuf, l + 1 ) ;

    SinCos( symRot + Pi, ssin, scos ) ;

    pt.X := RoundS( symrefX + ( symposX * scos  -  symposY * ssin ) ) ;
    pt.Y := RoundS( symrefY + ( symposX * ssin  +  symposY * scos ) ) ;

    outBuf[ l ] := pt ;
  end ;

  procedure TGIS_SymbolLineEx.movePosition(
    const _dx : Single ;
    const _dy : Single
  ) ;
  var
    nd    : Integer ;
    pt_A  : TPointF ;
    pt_B  : TPointF ;
    w     : Single  ;
    w2    : Single  ;
    w_tmp : Single  ;
  begin
    posX := posX + _dx ;
    posY := posY + _dy ;

    if ( loopEnd > 0 ) and ( posX >= loopEnd ) then begin
      posX := loopEnd ;
      posY := 0 ; // align to base line at the end
    end ;

    if  _dx >= 0 then begin // search forward
      for nd := curNode + 1 to length( lineBuf ) - 1 do begin
        pt_A := _TPointF( arDrawBuf[ nd-1 ] ) ;
        pt_B := _TPointF( arDrawBuf[ nd   ] ) ;

        w    := lineBuf[ nd ].w ;
        w2   := lineBuf[ nd ].w2 ;

        if posX < w2 + w then begin // point at this segment

          w_tmp := posX - w2 ;

          if w > 0 then begin
            refX   := w_tmp/w * ( pt_B.X - pt_A.X ) + pt_A.X ;
            refY   := w_tmp/w * ( pt_B.Y - pt_A.Y ) + pt_A.Y ;
          end ;
          refRot := lineBuf[ nd-1 ].angle ;

          curNode := nd -1 ;
          break ;
        end ;

        if nd = length( lineBuf ) - 1 then begin // we reached the end

          refX   := pt_B.X ;
          refY   := pt_B.Y ;
          refRot := lineBuf[ nd-1 ].angle ;

          curNode := nd -1 ;
          break ;
        end;
      end;
    end
    else begin // search backward
      for nd := curNode + 1 downto 1 do begin
        pt_A := _TPointF( arDrawBuf[ nd-1 ] ) ;
        pt_B := _TPointF( arDrawBuf[ nd   ] ) ;

        w    := lineBuf[ nd ].w ;
        w2   := lineBuf[ nd ].w2 ;

        if posX >= w2 then begin // point at this segment

          w_tmp := posX - w2 ;

          if w > 0 then begin

          refX   := w_tmp/w * ( pt_B.X - pt_A.X ) + pt_A.X ;
          refY   := w_tmp/w * ( pt_B.Y - pt_A.Y ) + pt_A.Y ;
          end;
          refRot := lineBuf[ nd-1 ].angle ;

          curNode := nd -1 ;
          break ;
        end ;

        if nd = 1 then begin // we reached the end

          refX   := pt_A.X ;
          refY   := pt_A.Y ;
          refRot := lineBuf[ nd-1 ].angle ;

          curNode := nd -1 ;
          break ;
        end;
      end;
    end ;
  end;

  procedure TGIS_SymbolLineEx.moveSymbolPosition(
    const _dx    : Single ;
    const _dy    : Single
  ) ;
  begin
    symposX := symposX + _dx ;
    symposY := symposY + _dy ;
  end;

  procedure TGIS_SymbolLineEx.setSymbolOrigin ;
  begin
    symRot  := refRot ;

    symrefX := refX ;
    symrefY := refY ;
    symposX := 0 ;
    symposY := 0 ;
  end;

  procedure TGIS_SymbolLineEx.addSymbolPoint(
    const _dx : Single ;
    const _dy : Single
  ) ;
  begin
    moveSymbolPosition( _dx, _dy );
    drawPointSym ;
  end;

  procedure TGIS_SymbolLineEx.addFreePoint(
    const _dx : Single ;
    const _dy : Single
  ) ;
  begin
    movePosition( _dx, _dy );

    drawPoint ;
  end;

  procedure TGIS_SymbolLineEx.doDraw ;
  var
    i          : Integer ;
    k          : Integer ;
    r          : T_Command ;
    ar         : array of T_Command ;
    cnt        : Integer ;
    tmp_x      : Single ;
    tmp_y      : Single ;
    last_x     : Single ;
    org_y      : Single ;
    rnd        : TGIS_RendererAbstract ;

    function get_units( const _cmd : T_Command ) : Single ;
    begin
      Result := 0 ;
      case _cmd.Token of
        TOKEN_VALUE_DEFAULT,
        TOKEN_VALUE_PIXELS    :
          Result := TGIS_RendererAbstract( symRenderer ).TwipsToPixels(
                      Abs( RoundS( 1440 / 96 * _cmd.Value ) )
                    )
                    * Sign( _cmd.Value ) ;
        TOKEN_VALUE_PERCENT   :
          Result := ( lineLen * _cmd.Value / 100 ) ;
        TOKEN_VALUE_TWIPS     :
          Result := TGIS_RendererAbstract( symRenderer ).TwipsToPixels(
                      Abs( _cmd.Value )
                    )
                    * Sign( _cmd.Value ) ;
        TOKEN_VALUE_WIDTH     :
          // strange but to ensure that rounding for positive and negative
          // values will have same impact
          Result := ( Abs( _cmd.Value )
                      * symSize / 10
                    )
                    * Sign( _cmd.Value ) ;
        TOKEN_VALUE_SWIDTH    :
          // strange but to ensure that rounding for positive and negative
          // values will have same impact
          Result := ( Abs( _cmd.Value )
                      * ( Sqrt( Max( 1, symSize * 96 /
                        rnd.PPI - 2 ) ) )
                      * Max( 1, symSize / 2 * 96 /
                        rnd.PPI )
                      * rnd.PPI / 96
                    )
                    * Sign( _cmd.Value ) / 10 ;
        TOKEN_VALUE_SCRPIX    :
          Result := _cmd.Value ;
        else begin
          assert( False, '??' ) ;
        end;
      end;

    end;
  begin
    if lineLen < 1 then
      exit ;

    rnd := TGIS_RendererAbstract( symRenderer ) ;

    curNode := 0 ;

    k := 0 ;
    cnt := 0 ;
    posX := 0 ;
    posY := 0 ;
    loopEnd := 0 ;
    org_y  := 0 ;
    tmp_x  := 0 ;
    last_x := 0 ;

    rnd.CanvasPen.Color    := symColor1 ;
    rnd.CanvasPen.Width    := 1 ;
    rnd.CanvasBrush.Color  := symColor1 ;
    rnd.CanvasBrush.Style  := TGIS_BrushStyle.Solid ;
    rnd.CanvasPen.Width    := RoundS( symSize ) ;
    rnd.CanvasPen.LineCap  := TGIS_LineCap.Flat ;
    rnd.CanvasPen.LineJoin := TGIS_LineJoin.Round ;
    rnd.CanvasPen.Style    := TGIS_PenStyle.Solid ;

    movePosition( 0, 0 ) ;  // initial setup

    while k < length( arDef ) do begin
      r := arDef[k] ;

      case r.Token of
        TOKEN_LOOPBEGIN :
          begin
            assert( length( ar ) = 1 ) ;

            loopEnd := get_units( ar[0] ) ;
            if loopEnd < 0 then
              loopEnd := RoundS( lineLen + loopEnd ) ;

            last_x := posX ;
            org_y  := posY ;

            SetLength( ar, 0 ) ;
            cnt := 0 ;
          end ;
        TOKEN_LOOPEND :
          begin
            if posX <= last_x then begin
              break ;
            end ;

            last_x := posX ;

            posY := org_y ; // reset Y offset

            if posX < loopEnd then begin
              k := r.Value ;
              continue ;
            end ;

            loopEnd := 0 ;
          end ;
        TOKEN_GOTO :
          begin
            posX := 0 ;
            posY := 0 ;
            curNode := 0 ;
            tmp_x := get_units( ar[0] ) ;
            movePosition( tmp_x, 0 ) ;
            SetLength( ar, 0 ) ;
            cnt := 0 ;
          end ;
        TOKEN_MOVE :
          begin
            assert( length( ar ) = 2 ) ;

            tmp_x := get_units( ar[0] ) ;
            tmp_y := get_units( ar[1] ) ;

            movePosition( tmp_x, tmp_y ) ;
            SetLength( ar, 0 ) ;
            cnt := 0 ;
          end ;
        TOKEN_LINE :
          begin
            clearBuffer ;

            doLine( get_units( ar[0] ), posY ) ;

            SetLength( ar, 0 ) ;
            cnt := 0 ;
          end ;
        TOKEN_DRAW :
          begin
            clearBuffer ;
            addFreePoint( 0, 0 ) ;
            for i:=0 to cnt -1  do begin
              if i mod 2 = 0 then begin
                tmp_x := get_units( ar[i] ) ;
              end
              else begin
                tmp_y := get_units( ar[i] ) ;
                addFreePoint( tmp_x, tmp_y ) ;
              end ;
            end ;

            rnd.CanvasDrawPolyLine( outBuf ) ;

            SetLength( ar, 0 ) ;
            cnt := 0 ;
          end ;
        TOKEN_OUTLINE :
          begin
            clearBuffer ;

            setSymbolOrigin ;

            tmp_x := get_units( ar[0] ) ;
            tmp_y := get_units( ar[1] ) ;
            moveSymbolPosition( tmp_x, tmp_y) ;

            addSymbolPoint( 0, 0 ) ;

            for i := 2 to cnt - 1 do begin
              if i mod 2 = 0 then begin
                tmp_x := get_units( ar[i] ) ;
              end
              else begin
                tmp_y := get_units( ar[i] ) ;
                addSymbolPoint( tmp_x, tmp_y ) ;
              end ;
            end ;

            rnd.CanvasDrawPolyLine( outBuf ) ;
            SetLength( ar, 0 ) ;
            cnt := 0 ;
          end ;
        TOKEN_FILL :
          begin
            clearBuffer ;

            setSymbolOrigin ;

            tmp_x := get_units( ar[0] ) ;
            tmp_y := get_units( ar[1] ) ;
            moveSymbolPosition( tmp_x, tmp_y ) ;

            addSymbolPoint( 0, 0 ) ;

            for i := 2 to cnt - 1 do begin
              if i mod 2 = 0 then begin
                tmp_x := get_units( ar[i] ) ;
              end
              else begin
                tmp_y := get_units( ar[i] ) ;
                addSymbolPoint( tmp_x, tmp_y ) ;
              end ;
            end ;

            rnd.CanvasDrawPolygon( outBuf ) ;
            SetLength( ar, 0 ) ;
            cnt := 0 ;
          end ;
        TOKEN_COLOR :
          begin
            assert( ( cnt = 3 ) or ( cnt = 1 ) ) ;

            if cnt = 3 then begin
              rnd.CanvasPen.Color   :=
                TGIS_Color.FromRGB( ar[0].Value, ar[1].Value, ar[2].Value ) ;
              rnd.CanvasBrush.Color :=
                TGIS_Color.FromRGB( ar[0].Value, ar[1].Value, ar[2].Value ) ;
            end
            else if cnt = 1 then begin
              if ar[0].Value = 0 then begin
                rnd.CanvasPen.Color   := symColor1 ;
                rnd.CanvasBrush.Color := symColor1 ;
              end
              else begin
                rnd.CanvasPen.Color   := symColor2 ;
                rnd.CanvasBrush.Color := symColor2 ;
              end ;
            end ;

            SetLength( ar, 0 ) ;
            cnt := 0 ;
          end ;
        TOKEN_WIDTH :
          begin
            assert( cnt = 1 ) ;

            rnd.CanvasPen.Width := RoundS( get_units( ar[0] ) ) ;

            if ( rnd.CanvasPen.Width = 0 ) and ( ar[0].Value <> 0 ) then begin
              // force at least one pixel width if any width has been set
              rnd.CanvasPen.Width := 1 ;
            end ;

            SetLength( ar, 0 ) ;
            cnt := 0 ;
          end ;
        TOKEN_VALUE_DEFAULT  ,
        TOKEN_VALUE_PIXELS   ,
        TOKEN_VALUE_PERCENT  ,
        TOKEN_VALUE_TWIPS    ,
        TOKEN_VALUE_WIDTH    ,
        TOKEN_VALUE_SWIDTH   ,
        TOKEN_VALUE_SCRPIX   :
          begin
            inc( cnt ) ;
            if cnt mod 2 = 1 then begin
              SetLength( ar, length( ar ) + 1 ) ;
              ar[ length(ar) -1 ] := r ;
            end
            else begin
              SetLength( ar, length( ar ) + 1 ) ;
              ar[ length(ar) -1 ] := r ;
            end ;
          end ;
      end ;

      inc( k ) ;
    end ;

  end ;


  procedure TGIS_SymbolLineEx.doLine(
    const _len : Single ;
    const _off : Single
  ) ;
  const
    LOCAL_SQRT2 : Double = 1.414213 ;
  var
    rnd : TGIS_RendererAbstract ;
    buf : TGIS_DrawBufF ;
    tmp : TGIS_DrawBufF ;
    vx  : Single ;
    vy  : Single ;
    dbl : Single ;
    len : Single ;
    dst : Single ;
    i   : Integer ;
    j   : Integer ;
    cnt : Integer ;
    num : Integer ;
    dx1 : Single ;
    dy1 : Single ;
    dx2 : Single ;
    dy2 : Single ;
    t   : Single ;
    nd  : Integer ;
    sgn : Integer ;
    sid : Integer ;
    trn : Integer ;
  begin
    if ( _len = 0.0 ) or ( posX > lineLen ) or ( posX < 0 ) then
      exit ;

    nd := curNode + 1 ;

    if nd = 0 then
      nd := 1 ;

    if _len > 0 then
      len := _len
    else
      len := lineLen + _len - posX ;

    if loopEnd > 0 then begin
      if len > loopEnd - posX then
        len := loopEnd - posX ;
      if len < 1e-7 then begin
        posX := loopEnd ;
        exit ;
      end ;
    end ;

    sgn := 1 ;
    sid := 0 ;

    // backward drawing; left "just in case"
    //sgn := Sign( len ) ;
    //if sgn > 0 then
    //  sid := 0
    //else
    //  sid := 1 ;
    //
    //len := Abs( _len ) ;

    if ( nd >= length( lineBuf ) ) or ( nd < 0 ) then
      exit ;

    vx := arDrawBuf[nd].X - arDrawBuf[nd-1].X ;
    vy := arDrawBuf[nd].Y - arDrawBuf[nd-1].Y ;

    dst := lineBuf[nd].w ;

    if dst < 1e-14 then begin
      dbl := 0.0 ;
      vx := 0.0 ;
      vy := 0.0 ;
    end
    else begin
      dbl := posX - lineBuf[nd].w2 ;
      vx := dbl*vx/dst ;
      vy := dbl*vy/dst ;
    end ;

    vx := arDrawBuf[nd-1].X + vx ;
    vy := arDrawBuf[nd-1].Y + vy ;

    cnt := length( lineBuf ) ;
    SetLength( buf, cnt ) ;
    {$IFDEF JAVA}
      for i := 0 to cnt-1 do
        buf[i] := new TPointF(0,0) ;
    {$ENDIF}

    buf[0].X := RoundS( vx ) ;
    buf[0].Y := RoundS( vy ) ;
    cnt := 1 ;

    if sgn > 0 then
      dbl := -dbl
    else begin
      dbl := dbl - lineBuf[nd].w ;
      inc( nd, sgn ) ;
    end ;

    while ( nd < length( lineBuf ) ) and ( nd >= 0 ) do begin
      dbl := dbl + lineBuf[nd+sid].w ;
      if Abs( dbl ) > len then begin
        if cnt = 1 then
          dbl := 0.0
        else
          dbl := dbl - lineBuf[nd+sid].w ;
        break ;
      end ;
      buf[cnt].X := arDrawBuf[nd].X ;
      buf[cnt].Y := arDrawBuf[nd].Y ;
      inc( nd, sgn ) ;
      inc( cnt ) ;
      inc( curNode );
    end ;

    if ( Abs( dbl ) <= len      ) and
       ( nd < length( lineBuf ) ) and
       ( nd >= 0                ) then begin

      vx := arDrawBuf[nd].X - arDrawBuf[nd-sgn].X ;
      vy := arDrawBuf[nd].Y - arDrawBuf[nd-sgn].Y ;

      dst := lineBuf[nd+sid].w ;

      if dst > 1e-14 then begin

        vx := ( len - dbl )*vx/dst ;
        vy := ( len - dbl )*vy/dst ;

        vx := buf[cnt-1].X + vx ;
        vy := buf[cnt-1].Y + vy ;

        buf[cnt].X := RoundS( vx ) ;
        buf[cnt].Y := RoundS( vy ) ;

        inc( nd, sgn ) ;
        inc( cnt ) ;

      end ;

    end ;

    posX := posX + len ;
    if posX > lineLen - 1e-1 then begin
      posX := lineLen ;
      curNode := length( lineBuf ) - 2 ;
    end ;

    if _off = 0.0 then begin

      outBuf := buf ;
      num := cnt ;

    end
    else begin

      SetLength( tmp, 2*( cnt - 2 ) + 2 ) ;
      {$IFDEF JAVA}
        for i := 0 to 2*( cnt - 2 ) + 2-1 do
          tmp[i] := new TPointF(0,0) ;
      {$ENDIF}
      num := cnt ;
      i := 0 ;
      j := 1 ;
      cnt := 0 ;
      while j < num do begin
        vx := buf[j].X - buf[i].X ;
        vy := buf[j].Y - buf[i].Y ;

        dst := Sqrt( vx*vx + vy*vy ) ;

        if dst < 1e-14 then begin
          inc( j ) ;
          continue ;
        end ;

        vx := _off*vx/dst ;
        vy := _off*vy/dst ;

        dbl := vx ;
        vx  := vy ;
        vy  := -dbl ;

        tmp[cnt].X := buf[i].X + RoundS( vx ) ;
        tmp[cnt].Y := buf[i].Y + RoundS( vy ) ;
        inc( cnt ) ;
        tmp[cnt].X := buf[j].X + RoundS( vx ) ;
        tmp[cnt].Y := buf[j].Y + RoundS( vy ) ;
        inc( cnt ) ;

        i := j ;
        inc( j ) ;
      end ;

      if cnt < 2 then
        exit ;

      SetLength( outBuf, cnt ) ;
      {$IFDEF JAVA}
        for i := 0 to cnt-1 do
          outBuf[i] := new TPointF(0,0) ;
      {$ENDIF}
      outBuf[0].X := tmp[0].X ;
      outBuf[0].Y := tmp[0].Y ;

      num := 1 ;
      i := 0 ;
      j := 2 ;
      while j < cnt - 1 do begin

        dx1 := tmp[i+1].X - tmp[i].X ;
        dy1 := tmp[i+1].Y - tmp[i].Y ;
        dx2 := tmp[j+1].X - tmp[j].X ;
        dy2 := tmp[j+1].Y - tmp[j].Y ;

        t := dx1*dy2 - dy1*dx2 ;

        if t >= 0.0 then
          trn := 1
        else
          trn := -1;

        if Abs( t ) < 1e-14 then
          t := -1.0
        else
          t := (tmp[j].X*dy2 - tmp[j].Y*dx2 - tmp[i].X*dy2 + tmp[i].Y*dx2)/t ;

        if t < 0.0 then begin

          i := j ;
          inc( j, 2 ) ;
          continue ;

        end
        else
        if t < 1.0 then begin

          outBuf[num] := TPointF.Create(
                           RoundS( tmp[i].X + t*dx1 ),
                           RoundS( tmp[i].Y + t*dy1 )
                         ) ;
          inc( num ) ;

        end
        else begin

          vx := tmp[i+1].X - tmp[i].X ;
          vy := tmp[i+1].Y - tmp[i].Y ;

          dst := Sqrt( vx*vx + vy*vy ) ;

          vx := _off*vx/dst ;
          vy := _off*vy/dst ;

          if ( t - 1.0 )*dst > Abs( _off )*LOCAL_SQRT2 then begin

            vx := vx/LOCAL_SQRT2 ;
            vy := vy/LOCAL_SQRT2 ;

            tmp[i+1].X := tmp[i+1].X + trn * RoundS( vx ) ;
            tmp[i+1].Y := tmp[i+1].Y + trn * RoundS( vy ) ;

            outBuf[num].X := tmp[i+1].X ;
            outBuf[num].Y := tmp[i+1].Y ;
            inc( num ) ;

            vx := tmp[j].X - tmp[j+1].X ;
            vy := tmp[j].Y - tmp[j+1].Y ;

            dst := Sqrt( vx*vx + vy*vy ) ;

            vx := _off*vx/( dst*LOCAL_SQRT2 ) ;
            vy := _off*vy/( dst*LOCAL_SQRT2 ) ;

            tmp[j].X := tmp[j].X + trn * RoundS( vx ) ;
            tmp[j].Y := tmp[j].Y + trn * RoundS( vy ) ;

            outBuf[num].X := tmp[j].X ;
            outBuf[num].Y := tmp[j].Y ;
            inc( num ) ;

          end
          else begin

            outBuf[num] := TPointF.Create(
                             RoundS( tmp[i].X + t*dx1 ),
                             RoundS( tmp[i].Y + t*dy1 )
                         ) ;
            inc( num ) ;

          end ;

        end ;

        i := j ;
        inc( j, 2 ) ;

      end ;

      outBuf[num].X := tmp[cnt-1].X ;
      outBuf[num].Y := tmp[cnt-1].Y ;
      inc( num ) ;

    end ;

    refX := outBuf[num-1].X ;
    refY := outBuf[num-1].Y ;

    SetLength( outBuf, num ) ;

    rnd := TGIS_RendererAbstract( symRenderer ) ;
    rnd.CanvasDrawPolyLine( outBuf ) ;
  end ;


  procedure TGIS_SymbolLineEx.DrawLine(
    const _drawbuf : TGIS_DrawBufF ;
    const _cnt     : Integer
  ) ;
  begin
    arDrawBuf := _drawbuf ;

    prepareLine( _cnt ) ;

    doDraw ;
  end ;

//==============================================================================
// TGIS_SymbolSVG
//==============================================================================

  procedure TGIS_SymbolSVG.doCreate(
    {$IFDEF CLR}
      const _stream : Stream
    {$ELSE}
      const _stream : TStream
    {$ENDIF}
  ) ;
  var
    ct : TPointF ;
    vb : TRectF  ;
    dx : Single  ;
    dy : Single  ;
    sz : Single  ;
    {$IFNDEF OXYGENE}
      pos : LongInt ;
    {$ENDIF}
  begin
    FIsFileBased := False ;
    FAutoCenter  := False ;

    oSvg := TGIS_FileSVG.Create ;

    if assigned( _stream ) then begin
      {$IFNDEF OXYGENE}
        pos := _stream.Position ;
      {$ENDIF}
      try
        _stream.Position := 0 ;
        parseFile( _stream ) ;
      finally
        {$IFNDEF OXYGENE}
          _stream.Position := pos ;
        {$ENDIF}
      end;
    end;

    ct := TGIS_FileSVG( oSvg ).SVGCenterPoint ;
    vb := TGIS_FileSVG( oSvg ).SVGViewBox     ;

    sz := Max( vb.Right-vb.Left, vb.Bottom - vb.Top) ;

    dx := ( vb.Right  - vb.Left ) / 2 - ( ct.X - vb.Left ) ;
    dy := ( vb.Bottom - vb.Top  ) / 2 - ( ct.Y - vb.Top  ) ;

    FCenter.X := dx / sz * 100 ;
    FCenter.Y := dy / sz * 100 ;

    FWidth  := getRealSymWidth ;
    FHeight := getRealSymHeight ;
  end;

  constructor TGIS_SymbolSVG.Create( const _name : String ) ;
  var
    strm  : TGIS_FileStream ;
  begin
    inherited Create( GetPathAbsolute( '', _name ) ) ;

    strm := TGIS_FileStream.Create( FName, fmShareDenyWrite ) ;
    try
      doCreate( strm  ) ;
    finally
      FreeObject( strm ) ;
    end;

    FIsFileBased := True ;
  end ;

  constructor TGIS_SymbolSVG.Create(
    const _name     : String ;
    {$IFDEF CLR}
      const _stream : Stream
    {$ELSE}
      const _stream : TStream
    {$ENDIF}
  ) ;
  begin
    inherited Create( GetPathAbsolute( '', _name ) ) ;

    doCreate( _stream ) ;

    FIsFileBased := False ;
  end;

  procedure TGIS_SymbolSVG.doDestroy ;
  begin
    FreeObject( oSvg ) ;
    inherited ;
  end ;

  function TGIS_SymbolSVG.getSymWidth
    : Integer ;
  begin
    Result := toPixelI( Max( Abs( TGIS_FileSVG( oSvg ).SVGViewBox.Right ),
                             Abs( TGIS_FileSVG( oSvg ).SVGViewBox.Left  )
                           )
                      )  ;
  end ;

  function TGIS_SymbolSVG.getSymHeight
    : Integer ;
  begin
    Result := toPixelI( Max( Abs( TGIS_FileSVG( oSvg ).SVGViewBox.Bottom ),
                             Abs( TGIS_FileSVG( oSvg ).SVGViewBox.Top    )
                           )
                      )  ;
  end ;

  function TGIS_SymbolSVG.getRealSymWidth
    : Integer ;
  begin
    Result := toPixelI( TGIS_FileSVG( oSvg ).SVGViewBox.Right  ) -
              toPixelI( TGIS_FileSVG( oSvg ).SVGViewBox.Left   ) ;
  end ;

  function TGIS_SymbolSVG.getRealSymHeight
    : Integer ;
  begin
    Result := toPixelI( TGIS_FileSVG( oSvg ).SVGViewBox.Bottom  ) -
              toPixelI( TGIS_FileSVG( oSvg ).SVGViewBox.Top     ) ;
  end ;

  function TGIS_SymbolSVG.fget_ShieldLabel : TRectF ;
  begin
   Result := TGIS_FileSVG( oSvg ).SVGLabelBox ;
  end;

  function TGIS_SymbolSVG.fget_ShieldBounds : TRectF ;
  begin
   Result := TGIS_FileSVG( oSvg ).SVGBoundryBox ;
  end;

  procedure TGIS_SymbolSVG.parseFile ;
  begin
    parseFile( nil ) ;
  end;

  procedure TGIS_SymbolSVG.parseFile(
    const _stream : TStream
  ) ;
  var
    w, h : Single ;
  begin
    if not assigned( _stream ) then begin
      if SafeFileExists( FName ) then
        TGIS_FileSVG( oSvg ).Load( FName, _stream )
      else
        Abort ;
    end
    else
      TGIS_FileSVG( oSvg ).Load( '',  _stream ) ;

    w := Max( Abs( TGIS_FileSVG( oSvg ).SVGViewBox.Right-
                   TGIS_FileSVG( oSvg ).SVGViewBox.Left
                  ), 1 ) ;
    h := Max( Abs( TGIS_FileSVG( oSvg ).SVGViewBox.Top-
                   TGIS_FileSVG( oSvg ).SVGViewBox.Bottom
                  ), 1 ) ;

    FNativeHeight := h ;
    FNativeWidth  := w ;
    FNativeSize := Max( FNativeHeight, FNativeWidth  ) ;
    if FNativeSize = 0 then
      FNativeSize := 1 ;
  end ;

  procedure TGIS_SymbolSVG.setPosition(
    const _position : TGIS_SymbolPosition
  ) ;
  var
    vsize       : Single  ;
    svg_viewbox : TRectF  ;
    half_h      : Integer ;
    half_w      : Integer ;
  begin
    svg_viewbox := TGIS_FileSVG( oSvg ).SVGViewBox ;
    half_w      := Width div 2 ;
    half_h      := Height div 2 ;

    if not FAutoCenter then begin
      vsize := Max(  svg_viewbox.Right  - svg_viewbox.Left,
                    -svg_viewbox.Bottom - svg_viewbox.Top
                  ) ;

      case _position of
        TGIS_LabelPosition.UpLeft       :
          symOffset := Point(
                         - half_w,
                         - half_h
                       ) ;
        TGIS_LabelPosition.UpCenter     :
          symOffset := Point(
                           0,
                         - half_h
                       ) ;
        TGIS_LabelPosition.UpRight      :
          symOffset := Point(
                           half_w,
                         - half_h
                        ) ;
        TGIS_LabelPosition.MiddleLeft   :
          symOffset := Point(
                         - half_w,
                           0
                       ) ;
        TGIS_LabelPosition.MiddleCenter :
          symOffset := Point(
                           0,
                           0
                       ) ;
        TGIS_LabelPosition.MiddleRight  :
          symOffset := Point(
                           half_w,
                           0
                       ) ;
        TGIS_LabelPosition.DownLeft     :
           symOffset := Point(
                           half_w,
                           0
                        ) ;
        TGIS_LabelPosition.DownCenter   :
           symOffset := Point(
                           0,
                           half_h
                        ) ;
        TGIS_LabelPosition.DownRight    :
           symOffset := Point(
                           half_w,
                           half_h
                        ) ;
      end ;

      if vsize <> 0 then
        symOffset := Point( symOffset.X +
                            RoundS( FCenter.X * vsize / 100.0 ),
                            symOffset.Y
                          ) ;
      if vsize <> 0 then
        symOffset := Point( symOffset.X,
                            symOffset.Y +
                            RoundS( FCenter.Y * vsize / 100.0 )
                          ) ;

    end
    else
      symOffset := Point( 0, 0 ) ;

    symOffset := Point( toPixelI( symOffset.X ),
                        toPixelI( symOffset.Y )
                      ) ;

  end ;

  procedure TGIS_SymbolSVG.ClearCache ;
  begin

  end ;

  procedure TGIS_SymbolSVG.Draw(
    const _x : Integer ;
    const _y : Integer
  ) ;
  begin
    if symSize < 1 then exit ;

    TGIS_FileSVG( oSvg ).Draw(
       TGIS_RendererAbstract( symRenderer ),
       Point(_x,_y),
       symOffset,
       symSin,
       symCos,
       symFullRot,
       symScale,
       symScaleX,
       symColor1,
       symColor2
     ) ;
  end ;

  procedure TGIS_SymbolSVG.Draw(
    const _x : Single ;
    const _y : Single
  ) ;
  begin
    if symSize < 1 then exit ;

    TGIS_FileSVG( oSvg ).Draw(
       TGIS_RendererAbstract( symRenderer ),
       PointF( _x, _y ),
       PointF( symOffset.X, symOffset.Y ),
       symSin,
       symCos,
       symFullRot,
       symScale,
       symScaleX,
       symColor1,
       symColor2
     ) ;
  end ;

  { TGIS_SymbolHatch }

  constructor TGIS_SymbolHatch.Create(
    const _name : String
  ) ;
  begin
    inherited Create( _name ) ;

    FName   := _name.Substring( length('HATCH:') ) ;
    FWidth  := getRealSymWidth ;
    FHeight := getRealSymHeight ;

    parseDefinition( FName ) ;
  end ;

  procedure TGIS_SymbolHatch.doDestroy ;
  begin
    inherited ;

  end ;

  function TGIS_SymbolHatch.getRealSymHeight : Integer ;
  begin
    Result := 1;
  end ;

  function TGIS_SymbolHatch.getRealSymWidth : Integer ;
  begin
    Result := 10;
  end ;

  function TGIS_SymbolHatch.getSymSize : Integer ;
  begin
    Result := RoundS( symSize ) ;
  end ;

  procedure TGIS_SymbolHatch.parseDefinition(
    const _lines : String
  );
  var
    i, j   : Integer ;
    d, len : Integer ;
    lines  : TArray<String> ;
    line   : String ;
    split  : TArray<String> ;
  begin
    if length( _lines ) = 0 then exit ;

    try
      {$IFDEF JAVA OR ISLAND}
        lines := _lines.Split( ';' ).ToArray ;
      {$ELSE}
        lines := _lines.Split([';']) ;
      {$ENDIF}

      SetLength( FHatches, length( lines ) ) ;
      for i := 0 to length( lines )-1 do begin
        line := Trim( lines[i] ) ;
        if line = '' then continue ;
        if line[1] = '#' then continue ;

        {$IFDEF JAVA OR ISLAND}
          split := line.Split( ',' ).ToArray ;
        {$ELSE}
          split := line.Split([','] );
        {$ENDIF}
        len := length( split ) ;
        {$IFDEF GIS_NORECORDS}
          FHatches[i] := new TGIS_HatchDefinition();
        {$ENDIF}
        if len > 0 then
          FHatches[i].Angle   := DotStrToFloat( split[0] ) ;
        if len > 2 then
          FHatches[i].Base    := PointF( DotStrToFloat( split[1] ), DotStrToFloat( split[2] ) ) ;
        if len > 4 then
          FHatches[i].Offset  := PointF( DotStrToFloat( split[3] ), DotStrToFloat( split[4] ) ) ;

        j := 5 ;
        d := 0 ;
        if len-j > 0 then begin
          SetLength( FHatches[i].Dash, len-j ) ;
          while j < len do begin
            FHatches[i].Dash[d] := DotStrToFloat( split[j] ) ;
            inc(d) ;
            inc(j) ;
          end ;
        end ;
      end ;
    except
      FHatches := nil ;
    end;
  end ;

  procedure TGIS_SymbolHatch.drawPattern(
    const _renderer : TObject ;
    const _rect     : TRectF ;
    const _angle    : Double ;
    const _scale    : Double ;
    const _origin   : TPointF ;
    const _offset   : TPointF ;
    const _pattern  : TGIS_SingleArray
  ) ;
  var
    i, len   : Integer ;
    rnd : TGIS_RendererAbstract ;
    dcos, dsin, x, y, dx, dy, x_offset, y_offset : Double ;
    mid_x, mid_y, end_x, end_y : Double ;
  begin
    rnd := TGIS_RendererAbstract( _renderer ) ;
    rnd.CanvasPen.Color := symColor1 ;
    rnd.CanvasPen.Style := TGIS_PenStyle.Solid ;
    rnd.CanvasPen.Width := Max(1, symGap ) ;

    dcos := Cos(_angle *  Pi / -180);
    dsin := Sin(_angle *  Pi / -180);
    x_offset := dcos * 4096;
    y_offset := dsin * 4096;
    x := _origin.X * _scale;
    y := _origin.Y * -_scale;
    dx := (_offset.X*dcos - _offset.Y*dsin) * _scale;
    dy := (_offset.X*dsin + _offset.Y*dcos) * _scale;

    if (dx = 0) and (dy = 0) then exit;

    len := length( _pattern ) ;
    if len > 0 then begin
      rnd.CanvasPen.LineDash := [] ;
      SetLength( rnd.CanvasPen.LineDash, len ) ;
      for i := 0 to len-1 do
        rnd.CanvasPen.LineDash[i] := _pattern[i] * _scale ;
    end
    else
      rnd.CanvasPen.LineDash := [] ;

    for i := -256 to 255 do begin
      mid_x := x + dx * i;
      mid_y := y + dy * i;
      end_x := mid_x + x_offset;
      end_y := mid_y + y_offset;

      rnd.CanvasDrawLine( RoundS(mid_x), RoundS(mid_y), RoundS(end_x), RoundS(end_y) );
    end ;

    if len > 0 then begin
      rnd.CanvasPen.LineDash := [] ;
      SetLength( rnd.CanvasPen.LineDash, len ) ;
      for i := len-1 downto 0 do
        rnd.CanvasPen.LineDash[len-i-1] := _pattern[i] * _scale ;
    end;

    for i := -256 to 255 do begin
      mid_x := x + dx * i;
      mid_y := y + dy * i;
      end_x := mid_x - x_offset;
      end_y := mid_y - y_offset;

      rnd.CanvasDrawLine( RoundS(mid_x), RoundS(mid_y), RoundS(end_x), RoundS(end_y) );
    end ;

  end ;

  procedure TGIS_SymbolHatch.Draw(
    const _x : Integer ;
    const _y : Integer
  );
  var
    i : Integer ;
  begin
    for i := 0 to length( FHatches ) - 1 do
      drawPattern( symRenderer, RectF(0, 0, _x, _y), FHatches[i].Angle + RadToDeg(symAngle),
                   symScale, FHatches[i].Base, FHatches[i].Offset, FHatches[i].Dash
                 ) ;
  end ;


//==============================================================================
// initialization/finalization
//==============================================================================

{$IFDEF DCC}
  initialization

  finalization
    FreeObject( oSymbolList ) ;
{$ENDIF}

{==================================== END =====================================}
end.
