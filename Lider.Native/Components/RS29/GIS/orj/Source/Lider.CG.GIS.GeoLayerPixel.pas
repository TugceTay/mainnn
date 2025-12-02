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
  Encapsulation of a Pixel Layer.
}

{$IFDEF DCC}
  unit GisLayerPixel ;
  {$HPPEMIT '#pragma link "GisLayerPixel"'}
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
    System.Collections.Generic,
    System.Collections,
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Generics.Collections,
    System.Generics.Defaults,
    System.Classes,
    System.Math,
    System.Types,

    GisRtl,
    GisTypes,
    GisTypesUI,
    GisClasses,
    GisCSSystems,
    GisInterfaces,
    GisLayer,
    GisLayerVector,
    GisParams,
    GisRendererAbstract,
    GisStreams,
    GisTopology;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    TatukGIS.RTL ;
{$ENDIF}

type

  // forwarded type
  TGIS_LayerPixel = class ;

  {$IFDEF OXYGENE}
    /// <summary>
    ///    Grid operation function.
    /// </summary>
    /// <returns>
    ///   True if data transformed.
    /// </returns>
    /// <param name="_layer">
    ///   owner of data
    /// </param>
    /// <param name="_extent">
    ///   extent of the grid array
    /// </param>
    /// <param name="_source">
    ///   input grid data
    /// </param>
    /// <param name="_output">
    ///   output grid data
    /// </param>
    /// <param name="_width">
    ///   width of input/output area in pixels
    /// </param>
    /// <param name="_height">
    ///   height of input/output area in pixels
    /// </param>
    /// <param name="_minz">
    ///   Minimal value of grid data
    /// </param>
    /// <param name="_maxz">
    ///   Maximal value of grid data
    /// </param>
    TGIS_GridOperation =  public
                          function (       _layer  : TObject ;
                                     const _extent : TGIS_Extent ;
                                     const _source : array of array of Single ;
                                     {$IFDEF GIS_PDK}
                                       const
                                     {$ELSE}
                                       var
                                     {$ENDIF}
                                           _output : array of array of Single ;
                                     const _width  : Integer ;
                                     const _height : Integer ;
                                     var   _minz   : Single ;
                                     var   _maxz   : Single
                                   ) : Boolean of object ;
  {$ELSE}
    /// <summary>
    ///    Grid operation function.
    /// </summary>
    /// <returns>
    ///   True if data transformed.
    /// </returns>
    /// <param name="_layer">
    ///   owner of data
    /// </param>
    /// <param name="_extent">
    ///   extent of the grid array
    /// </param>
    /// <param name="_source">
    ///   input grid data
    /// </param>
    /// <param name="_output">
    ///   output grid data
    /// </param>
    /// <param name="_width">
    ///   width of input/output area in pixels
    /// </param>
    /// <param name="_height">
    ///   height of input/output area in pixels
    /// </param>
    /// <param name="_minz">
    ///   Minimal value of grid data
    /// </param>
    /// <param name="_maxz">
    ///   Maximal value of grid data
    /// </param>
    TGIS_GridOperation =  function (       _layer  : TObject ;
                                     const _extent : TGIS_Extent ;
                                     const _source : TGIS_GridArray ;
                                     {$IFDEF GIS_PDK}
                                       const
                                     {$ELSE}
                                       var
                                     {$ENDIF}
                                           _output : TGIS_GridArray ;
                                     const _width  : Integer ;
                                     const _height : Integer ;
                                     var   _minz   : Single ;
                                     var   _maxz   : Single
                                   ) : Boolean of object ;
  {$ENDIF}

  /// <summary>
  ///    Pixel (ARGB) operation function.
  /// </summary>
  /// <returns>
  ///   True if data transformed.
  /// </returns>
  /// <param name="_layer">
  ///   owner of data
  /// </param>
  /// <param name="_extent">
  ///   extent of the pixels array
  /// </param>
  /// <param name="_source">
  ///   input pixels data
  /// </param>
  /// <param name="_output">
  ///   output pixels data
  /// </param>
  /// <param name="_width">
  ///   width of input/output area in pixels
  /// </param>
  /// <param name="_height">
  ///   height of input/output area in pixels
  /// </param>
  TGIS_PixelOperation = {$IFDEF OXYGENE} public {$ENDIF}
                        function (       _layer  : TObject ;
                                   const _extent : TGIS_Extent ;
                                   const _source : {$IFNDEF OXYGENE}TGIS_Pixels{$ELSE}array of Int32{$ENDIF} ;
                                   {$IFDEF GIS_PDK}
                                     const
                                   {$ELSE}
                                     var
                                   {$ENDIF}
                                         _output : {$IFNDEF OXYGENE}TGIS_Pixels{$ELSE}array of Int32{$ENDIF} ;
                                   const _width  : Integer ;
                                   const _height : Integer
                                 ) : Boolean of object ;


  /// <summary>
  ///   Zone from 0 to 255 definition.
  /// </summary>
  TGIS_Zone = {$IFDEF GIS_PACKED} packed {$ENDIF} record
    /// <summary>
    ///   Starting value of zone.
    /// </summary>
    Start : Integer ;
    /// <summary>
    ///   Ending value of zone.
    /// </summary>
    Stop  : Integer ;
  end ;

  /// <summary>
  ///   Zone mapping. Y - new values for X zone.
  /// </summary>
  TGIS_MapZone = {$IFDEF OXYGENE} public {$ENDIF} record
    /// <summary>
    ///   Old zone.
    /// </summary>
    X : TGIS_Zone ;
    /// <summary>
    ///   New zone.
    /// </summary>
    Y : TGIS_Zone ;
  end ;

  /// <summary>
  ///   Array of mapping between altitude and color.
  /// </summary>
  TGIS_AltitudeZone = {$IFDEF OXYGENE} public {$ENDIF} record
    /// <summary>
    ///   Lower value of the zone.
    /// </summary>
    MinVal : Single     ;

    /// <summary>
    ///   Upper value of the zone.
    /// </summary>
    MaxVal : Single     ;

    /// <summary>
    ///   Color of the zone.
    /// </summary>
    Color  : TGIS_Color ;

    /// <summary>
    ///   Legend of the zone.
    /// </summary>
    Legend : String ;
  end ;

  /// <summary>
  ///   Structure of tile information.
  /// </summary>
  TGIS_Tile = {$IFDEF OXYGENE} public {$ENDIF} record
    /// <summary>
    ///   Tile level.
    /// </summary>
    Level   : Integer ;
    /// <summary>
    ///   Tile column.
    /// </summary>
    Column  : Integer ;
    /// <summary>
    ///   Tile row.
    /// </summary>
    Row     : Integer ;
  end ;

  /// <summary>
  ///   Structure of locked image area.
  /// </summary>
  TGIS_LayerPixelLock = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Object )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FGrid      : TGIS_GridArray    ;
      FBitmap    : TGIS_Pixels       ;
      FBand      : TGIS_GridArray    ;
      FBandNo    : Integer           ;
      FExtent    : TGIS_Extent       ;
      FBounds    : TRect             ;
      FCS        : TGIS_CSCoordinateSystem ;
      FPixelSize : TGIS_Point ;
      FWritable  : Boolean    ;
      FIsTiled   : Boolean    ;
      FTileInfo  : TGIS_Tile  ;
      FParent    : TGIS_LayerPixel ;

      pixWidth   : Integer    ;
      pixHeight  : Integer    ;
    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      constructor Create ; overload ;

      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_parent">
      ///   the owner of the lock
      /// </param>
      constructor Create( const _parent : TGIS_LayerPixel ) ; overload ;

      /// <summary>
      ///   Convert x and y image pixel location to position in Bitmap array
      /// </summary>
      /// <returns>
      ///   calculated position
      /// </returns>
      /// <param name="_x">
      ///   image column
      /// </param>
      /// <param name="_y">
      ///   image raw
      /// </param>
      function BitmapPos  ( const _x    : Integer ;
                            const _y    : Integer
                          ) : Integer ;

      /// <summary>
      ///   Convert map coordinates to a pixel location within locked buffer
      /// </summary>
      /// <returns>
      ///   calculated point
      /// </returns>
      /// <param name="_ptg">
      ///   point to be converted
      /// </param>
      /// <param name="_cs">
      ///   coordinate system of _ptg; if nil that coordinate system same as
      ///   layer coordinate system
      /// </param>
      function MapToRaster    ( const _ptg    : TGIS_Point  ;
                                const _cs     : TGIS_CSCoordinateSystem
                              ) : TPoint ;

      /// <summary>
      ///   Convert locked buffer location to map coordinates
      /// </summary>
      /// <returns>
      ///   calculated point
      /// </returns>
      /// <param name="_pt">
      ///   location to be converted
      /// </param>
      /// <param name="_cs">
      ///   coordinate system of returned value; if nil that coordinate system
      ///   same as layer coordinate system
      /// </param>
      function RasterToMap    ( const _pt     : TPoint  ;
                                const _cs     : TGIS_CSCoordinateSystem
                              ) : TGIS_Point ;

      /// <summary>
      ///   Convert map coordinates to a pixel location within locked buffer
      /// </summary>
      /// <returns>
      ///   calculated extent
      /// </returns>
      /// <param name="_ext">
      ///   extent to be converted
      /// </param>
      /// <param name="_cs">
      ///   coordinate system of _ext; if nil that coordinate system same as
      ///   layer coordinate system
      /// </param>
      function MapToRasterRect( const _ext    : TGIS_Extent ;
                                const _cs     : TGIS_CSCoordinateSystem
                              ) : TRect ;

      /// <summary>
      ///   Convert locked buffer location to map coordinates
      /// </summary>
      /// <returns>
      ///   calculated extent
      /// </returns>
      /// <param name="_rct">
      ///   location to be converted
      /// </param>
      /// <param name="_cs">
      ///   coordinate system of returned value; if nil that coordinate system
      ///   same as layer coordinate system
      /// </param>
      function RasterToMapRect( const _rct    : TRect       ;
                                const _cs     : TGIS_CSCoordinateSystem
                              ) : TGIS_Extent ;

      {$IFDEF GIS_PDK}
        {$IFDEF HELPGEN}
          /// <summary>
          ///   Returns currect lock scope as NumPy array.
          /// </summary>
          /// <returns>
          ///   Numpy array
          /// </returns>
          function AsArray : numpy.array ;

          /// <summary>
          ///   Fill current lock scope Returns with NumPy array content.
          /// </summary>
          /// <param name="_array">
          ///   NumPy array
          /// </param>
          procedure FromArray( _array : numpy.array ) ;
        {$ENDIF}
      {$ENDIF}

    public
      /// <summary>
      ///   The reference to the parent object.
      ///   Pixel layer to which the lock object belongs.
      /// </summary>
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      property Parent    : TGIS_LayerPixel     read FParent ;

      /// <summary>
      ///   Locked grid buffer. Null if locked area is bitmap type.
      /// </summary>
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      property Grid      : TGIS_GridArray      read FGrid ;

      /// <summary>
      ///   Locked bitmap buffer. Null if locked area is grid type.
      /// </summary>
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      property Bitmap    : TGIS_Pixels         read FBitmap ;

      /// <summary>
      ///   Locked band buffer. If band has not ben locked then null.
      /// </summary>
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      property Band      : TGIS_GridArray      read FBand ;

      /// <summary>
      ///   Locked band number.
      /// </summary>
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      property BandNo    : Integer             read FBandNo ;

      /// <summary>
      ///   Locked extent.
      /// </summary>
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      property Extent    : TGIS_Extent         read FExtent ;

      /// <summary>
      ///   Bounds of locked buffer. For in-memory layer bounds are not
      ///   necessary 0 based!
      /// </summary>
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      property Bounds    : TRect               read FBounds ;

      /// <summary>
      ///   Coordinate system of locked area.
      /// </summary>
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      property CS        : TGIS_CSCoordinateSystem
                                                        read FCS ;
      /// <summary>
      ///   Pixels size of locked buffer in locked CS units.
      /// </summary>
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      property PixelSize : TGIS_Point          read FPixelSize ;

      /// <summary>
      ///   If true then area is locked in read/write mode.
      /// </summary>
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      property &Writable : Boolean             read FWritable ;

      /// <summary>
      ///   If true then locked area represents a tile.
      /// </summary>
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      property IsTiled   : Boolean             read FIsTiled ;

      /// <summary>
      ///   Additional information about tile if locked area is a tile.
      /// </summary>
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      property TileInfo  : TGIS_Tile           read FTileInfo ;

    public
      {$IFDEF GIS_XDK}
        {#gendoc:hide}
        procedure AssignBuffers(
                    const _grid      : TGIS_GridArray ;
                    const _bitmap    : TGIS_Pixels    ;
                    const _band      : TGIS_GridArray
                 ) ;
      {$ENDIF}

      {$IFDEF GIS_EDITOR}
        procedure GetInternal(
                    var   _grid      : TGIS_GridArray    ;
                    var   _bitmap    : TGIS_Pixels       ;
                    var   _band      : TGIS_GridArray    ;
                    var   _bandNo    : Integer           ;
                    var   _extent    : TGIS_Extent       ;
                    var   _bounds    : TRect             ;
                    var   _cs        : TGIS_CSCoordinateSystem ;
                    var   _pixelSize : TGIS_Point        ;
                    var   _writable  : Boolean           ;
                    var   _isTiled   : Boolean           ;
                    var   _tileInfo  : TGIS_Tile         ;
                    var   _pixWidth  : Integer           ;
                    var   _pixHeight : Integer
                  ) ;
        procedure SetInternal(
                    const _grid      : TGIS_GridArray    ;
                    const _bitmap    : TGIS_Pixels       ;
                    const _band      : TGIS_GridArray    ;
                    const _bandNo    : Integer           ;
                    const _extent    : TGIS_Extent       ;
                    const _bounds    : TRect             ;
                    const _cs        : TGIS_CSCoordinateSystem ;
                    const _pixelSize : TGIS_Point        ;
                    const _writable  : Boolean           ;
                    const _isTiled   : Boolean           ;
                    const _tileInfo  : TGIS_Tile         ;
                    const _pixWidth  : Integer           ;
                    const _pixHeight : Integer
                  ) ;
      {$ENDIF}

  end ;

  /// <summary>
  ///   Structure of pixel tile.
  /// </summary>
  TGIS_LayerPixelTile = {$IFDEF OXYGENE} public {$ENDIF} record
    /// <summary>
    ///   Location in all image.
    /// </summary>
    Rect  : TRect   ;
    /// <summary>
    ///   Data orientation.
    /// </summary>
    Order : Integer ; // 0x0 Whole image
                      // 0x10 By Row Down , 0x11 by row up
                      // 0x20 By Col Right , 0x21 by Col left
                      // 0x30 By Tile DownRight  , 0x31 by Tile UpRight
                      // 0x32 By Tile RightDown  , 0x33 by Tile RightUp
                      // 0x34 By Tile DownLeft   , 0x35 by Tile UpLeft
                      // 0x36 By Tile LeftDown   , 0x37 by Tile LeftUp
  end ;

  /// <summary>
  ///   Pixel value used mainly in iterators.
  /// </summary>
  TGIS_PixelItem = {$IFDEF OXYGENE} public {$ENDIF} class
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FCenter   : TGIS_Point  ;
      FExtent   : TGIS_Extent ;
      FDistance : Double      ;
      FWritable : Boolean     ;
    public
      /// <summary>
      ///   Coordinates of the center of the pixel in layer coordinate system.
      /// </summary>
      property Center : TGIS_Point  read FCenter ;

      /// <summary>
      ///   Extent of the pixel in layer coordinate system.
      /// </summary>
      property Extent : TGIS_Extent read FExtent ;

      /// <summary>
      ///   Distance of pixel along line. Valid only if looped on TGIS_ShapeArc
      ///   features.
      /// </summary>
      property Distance : Double read FDistance ;

      /// <summary>
      ///   If true then pixel is in a writable mode
      ///   (changes will be posted back after leaving loop)
      /// </summary>
      property Writable : Boolean read FWritable ;
    public
      /// <summary>
      ///   Color value of the pixel. Meaningful only for bitmap based layers.
      /// </summary>
      /// <remarks>
      ///   If Loop() is in a writable mode then changing Color will
      ///   result in posting change back to the layer.
      /// </remarks>
      Color  : TGIS_Color  ;

      /// <summary>
      ///   Value of the pixel. Meaningful only for grid layers and Loop().
      /// </summary>
      /// <remarks>
      ///   If Loop() is in a writable mode then changing Value will
      ///   result in posting change back to the layer.
      /// </remarks>
      Value  : Single ;

      /// <summary>
      ///   Values of individual bands. Meaningful only for Loop()
      ///   constructed on specific or all bands.
      /// </summary>
      /// <remarks>
      ///   If Loop() is in a writable mode then changing Bands value will
      ///   result in posting change back to the layer.
      /// </remarks>
      Bands  : TGIS_SingleArray ;
  end ;


  /// <summary>
  ///   Definition of how layer should be interpreted
  /// </summary>
  TGIS_PixelBandSet = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Unknown definition.
    /// </summary>
    Unknown,

    /// <summary>
    ///   Interpret layer as color RGB image.
    /// </summary>
    Rgb,

    /// <summary>
    ///   Interpret layer as color ARGB image.
    /// </summary>
    Argb,

    /// <summary>
    ///   Interpret layer as greyscale image.
    /// </summary>
    Greyscale,

    /// <summary>
    ///   Interpret layer as greyscale image with reverted luminance.
    /// </summary>
    Negative,

    /// <summary>
    ///   Interpret layer as grid (digital elevation model).
    /// </summary>
    Dem
  ) ;

  /// <summary>
  ///   Definition of the meaning of a particular band.
  /// </summary>
  TGIS_PixelBandContent = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Unknown definition.
    /// </summary>
    Unknown,

    /// <summary>
    ///   Band is an alpha channel.
    /// </summary>
    Alpha,

    /// <summary>
    ///   Band is a red channel.
    /// </summary>
    Red,

    /// <summary>
    ///   Band is a green channel.
    /// </summary>
    Green,

    /// <summary>
    ///   Band is a blue channel.
    /// </summary>
    Blue,

    /// <summary>
    ///   Band is an other kind of a photo channel (like infrared).
    /// </summary>
    Photo,

    /// <summary>
    ///   Band is grid / digital elevation model.
    /// </summary>
    Dem
  );

  /// <summary>
  ///   Definition of how particular band is stored.
  /// </summary>
  TGIS_PixelBandCoding = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Band is an other kind of a photo channel (like infrared).
    /// </summary>
    Unknown,

    /// <summary>
    ///   Band is stored as palette.
    /// </summary>
    Palette,

    /// <summary>
    ///   Band is stored as signed integer.
    /// </summary>
    Integer,

    /// <summary>
    ///   Band is stored as unsigned integer.
    /// </summary>
    Cardinal,

    /// <summary>
    ///   Band is stored as float.
    /// </summary>
    Float
  ) ;


  /// <summary>
  ///   Complete definition of a band.
  /// </summary>
  TGIS_PixelBand = {$IFDEF OXYGENE} public {$ENDIF}
                   class( TGIS_Object )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FContent    : TGIS_PixelBandContent ;
      FCoding     : TGIS_PixelBandCoding ;
      FSkipBefore : Cardinal ;
      FValidBits  : Cardinal ;
      FSkipAfter  : Cardinal ;

    public
      /// <summary>
      ///   Content type of a band.
      /// </summary>
      property Content : TGIS_PixelBandContent read FContent ;

      /// <summary>
      ///   Coding method of a band.
      /// </summary>
      property Coding : TGIS_PixelBandCoding read FCoding ;

      /// <summary>
      ///   How many pixels should be ignored at the beginning tail of a band.
      /// </summary>
      property SkipBefore : Cardinal read FSkipBefore ;

      /// <summary>
      ///   How many pixels should be treated as a band value.
      /// </summary>
      property ValidBits : Cardinal read FValidBits ;

      /// <summary>
      ///   How many pixels should be ignored at the tail tail of a band.
      /// </summary>
      property SkipAfter : Cardinal read FSkipAfter ;
  end ;

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  /// <summary>
  ///   List of TGIS_PixelBand.
  /// </summary>
  {$IFNDEF GIS_NOGENERICS}
    TGIS_PixelBandList = {$IFDEF OXYGENE} public {$ENDIF}
                         TList< TGIS_PixelBand > ;
  {$ELSE}
    TGIS_PixelBandList = class (
                           TList< TGIS_PixelBand >
                         ) ;
  {$ENDIF}

  /// <summary>
  ///   Complete definition of all bands of a layer.
  /// </summary>
  TGIS_PixelBandsDefinition = {$IFDEF OXYGENE} public {$ENDIF}
                              class( TGIS_Object )
    private
      FFileInfo : String ;
      FBandSet  : TGIS_PixelBandSet ;
      FBand1    : Cardinal ;
      FBand2    : Cardinal ;
      FBand3    : Cardinal ;
      FBand4    : Cardinal ;
      FBands    : TGIS_PixelBandList ;

    protected
      procedure doDestroy ; override;

    public
      /// <summary>
      ///   Standard constructor.
      /// </summary>
      constructor Create ;

      /// <summary>
      ///   Set band use.
      /// </summary>
      /// <param name ="_fileinfo">
      ///   unique value for a file producer like camera producer + camera model
      /// </param>
      /// <param name ="_bandset">
      ///   band interpretation
      /// </param>
      /// <param name ="_band1">
      ///   first band; should be &gt; 0;
      /// </param>
      /// <param name ="_band2">
      ///   second band; Is &gt; 0 only if _bandset=Rgb or Argb
      /// </param>
      /// <param name ="_band3">
      ///   third band. Is &gt; 0 only if _bandset=Rgb or Argb
      /// </param>
      /// <param name ="_band4">
      ///   third band. Is &gt; 0 only if _bandset=Argb
      /// </param>
      procedure SetBandSet(
        const _fileinfo : String   ;
        const _bandset  : TGIS_PixelBandSet ;
        const _band1    : Cardinal ;
        const _band2    : Cardinal ;
        const _band3    : Cardinal ;
        const _band4    : Cardinal
      ) ;

      /// <summary>
      ///   Clear all band definitions.
      /// </summary>
      procedure ClearBands ;

      /// <summary>
      ///   Add a band definition.
      /// </summary>
      /// <param name ="_content">
      ///   Content type of a band.
      /// </param>
      /// <param name ="_coding">
      ///   Coding method of a band.
      /// </param>
      /// <param name ="_skip_before">
      ///   How many pixels should be ignored at the beginning tail of a band.
      /// </param>
      /// <param name ="_valid_bits">
      ///   How many pixels should be trated as a band value.
      /// </param>
      /// <param name ="_skip_after">
      ///   How many pixels should be ignored at the tail tail of a band.
      /// </param>
      procedure AddBand(
        const _content     : TGIS_PixelBandContent ;
        const _coding      : TGIS_PixelBandCoding  ;
        const _skip_before : Cardinal ;
        const _valid_bits  : Cardinal ;
        const _skip_after  : Cardinal
      );

      /// <summary>
      ///   Convert a band definition to a string representation.
      /// </summary>
      /// <returns>
      ///   String representation of band definition.
      /// </returns>
      function ToString
        : String ; reintroduce ;

      /// <summary>
      ///   Convert a string representation to a band definition.
      /// </summary>
      /// <param name="_value">
      ///   string represtantion of band definition
      /// </param>
      procedure FromString(
        const _value : String
      ) ;

    public
      /// <summary>
      ///   File producer info (like camera manufacture).
      /// </summary>
      property FileInfo : String read FFileInfo ;

      /// <summary>
      ///   How bands should be interpreted.
      /// </summary>
      property BandSet : TGIS_PixelBandSet read FBandSet ;

      /// <summary>
      ///   First band. Should be &gt; 0;
      /// </summary>
      property Band1 : Cardinal read FBand1 ;

      /// <summary>
      ///   Second band of BandSet. Is &gt; 0 only if Bandset=Rgb or Argb
      /// </summary>
      property Band2 : Cardinal read FBand2 ;

      /// <summary>
      ///   Third band of BandSet. Is &gt; 0 only if Bandset=Rgb or Argb
      /// </summary>
      property Band3 : Cardinal read FBand3 ;

      /// <summary>
      ///   Fourth band of BandSet. Is &gt; 0 only if Bandset=Argb
      /// </summary>
      property Band4 : Cardinal read FBand4 ;

      /// <summary>
      ///   Definitions of all bands.
      /// </summary>
      property Bands : TGIS_PixelBandList read FBands ;
  end ;


  // encapsulation of layer enumerator
  TGIS_LayerPixelEnumerator = class ;

  // encapsulation of layer enumerator factory
  TGIS_LayerPixelEnumeratorFactory = class ;


  /// <summary>
  ///   Encapsulation of a common image layer - abstract class.
  /// </summary>
  TGIS_LayerPixel = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Layer )

    protected
      /// <summary>
      ///   Actual bitmap format - ARGB available now .
      /// </summary>
      FBitmapFormat     : TGIS_BitmapFormat ;
      /// <summary>
      ///   Actual lines order - Down available now.
      /// </summary>
      FBitmapLinesOrder : TGIS_BitmapLinesOrder ;

      /// <summary>
      ///   List of available image format in this layer (assigned with name extension).
      /// </summary>
      FCapabilities     : TGIS_LayerPixelSubFormatList  ;

      /// <summary>
      ///   Path to TAB referencing file.
      /// </summary>
      FPathTAB : String ;

      /// <summary>
      ///   Width of a layer in pixels.
      /// </summary>
      FBitWidth : Integer ;

      /// <summary>
      ///   Height of a layer in pixels.
      /// </summary>
      FBitHeight : Integer ;

      /// <summary>
      ///   Width of a cell in pixels.
      /// </summary>
      FCellWidth  : Integer ;

      /// <summary>
      ///   Height of a cell in pixels.
      /// </summary>
      FCellHeight : Integer ;

      /// <summary>
      ///   Number of bands in file.
      /// </summary>
      FBandsCount : Integer ;

      /// <summary>
      ///   Rotation around image center.
      /// </summary>
      FRotationAngle : Double ;

      /// <summary>
      ///   Band number in file used as grid band.
      /// </summary>
      FGridBand : Integer ;

      /// <summary>
      ///   Value in the grid file assigned to cells whose value in unknown
      /// </summary>
      FNoDataValue : Single ;

      /// <summary>
      ///   Cutting polygon
      /// </summary>
      FCuttingPolygon : TGIS_ShapePolygon ;

      /// <summary>
      ///   Minimum elevation value in grid file.
      /// </summary>
      FMinZ  : Single ;

      /// <summary>
      ///   Maximum elevation value in grid file.
      /// </summary>
      FMaxZ  : Single ;

      /// <summary>
      ///   Minimum threshold of the grid elevation value to display.
      /// </summary>
      FMinThresholdZ  : Single ;

      /// <summary>
      ///   Maximum threshold of the grid elevation value to display.
      /// </summary>
      FMaxThresholdZ  : Single ;

      /// <summary>
      ///   Modify draw extent calcualtion t be perforemd on mid pixel rather then
      ///   on border.
      /// </summary>
      FExtentPixelAdjustment  : Boolean ;

      /// <summary>
      ///   True if image is grid type.
      /// </summary>
      FIsGridImage : Boolean ;

      /// <summary>
      ///   Forced layer interpretation.
      /// </summary>
      FInterpretation : TGIS_LayerPixelInterpretation ;

      /// <summary>
      ///   True if image has automatically enhanced contrast.
      /// </summary>
      FIsContrastEnhanced  : Boolean ;

      /// <summary>
      ///   True if image is really grid type.
      /// </summary>
      FIsNativeGridImage : Boolean ;

      /// <summary>
      ///   Antialias scaling (for some layers only like PNG and JPG)
      /// </summary>
      FAntialias : Boolean ;

      /// <summary>
      ///   Scaling filter (like Linear, Lanczos) to be used for scaling
      ///   images and grids.
      /// </summary>
      FAntialiasFilter : TGIS_ScalingFilter ;

      /// <summary>
      ///   If true (default) then layer can be drawn as progressive.
      /// </summary>
      FProgressive : Boolean ;

      /// <summary>
      ///   Number of pages in an image file (TIF).
      /// </summary>
      FPagesCount : Integer ;

      /// <summary>
      ///   Color property to define default color to be used to fill "empty"
      ///   area upon PixelExportManager.
      /// </summary>
      FNoDataColor : TGIS_Color ;

      /// <summary>
      ///   Current page i image file (TIF).
      /// </summary>
      FCurrentPage  : Integer ;

      /// <summary>
      ///   Grid data of generated (built) layer.
      /// </summary>
      oGrid         : TGIS_GridArray ;

      /// <summary>
      ///   ARGB data of generated (built) layer.
      /// </summary>
      oBitmap       : TGIS_Pixels    ;

      /// <summary>
      ///   Set when layer is new generated.
      /// </summary>
      isBuilt        : Boolean ;

      /// <summary>
      ///   Single data locked Band of layer when band data are greater
      ///   than 8-bits.
      /// </summary>
      oBand         : TGIS_GridArray ;

      /// <summary>
      ///   Temporary file name - for big data.
      /// </summary>
      tempFileName : String ;

      /// <summary>
      ///   Temporary file - for big data.
      /// </summary>
      tempFileStream : TGIS_FileStream ;

      /// <summary>
      ///   Layer used for data changing.
      /// </summary>
      oLayer        : TObject ;

    private
      /// <summary>
      ///   Topmost cache.
      /// </summary>
      tmCache : TGIS_RendererAbstractCache ;

      /// <summary>
      ///   Tolerance for altitude map zones.
      /// </summary>
      dZEpsilon : Single ;

      /// <summary>
      ///   If True interpolate colors from color ramp,
      ///   otherwise fixed colors will be used (default).
      /// </summary>
      gridSmoothColors : Boolean ;

      /// <summary>
      ///   Altitude or color ramp zone index for previous pixel.
      /// </summary>
      lastColorZoneIndex : Integer ;

      /// <summary>
      ///   List of layers used for data changing.
      /// </summary>
      oLockListW   : TObjectList< TGIS_LayerPixel >;

      /// <summary>
      ///   List of layers used for data reading.
      /// </summary>
      oLockListR   : TObjectList< TGIS_LayerPixel >;

      /// <summary>
      ///   List of layers used for data changing of the multilayer holder
      ///   like WMTS.
      /// </summary>
      oParentLockList : TObjectList< TGIS_LayerPixel >;

      /// <summary>
      ///   last value of Params.Pixel.Serial when retrieving
      ///   the altitude map zone using "GetAltitudeMapZone"
      /// </summary>
      /// <remarks>
      ///   this value indicates if it is need to call "prepareAltitudeMapTable"
      /// </remarks>
      lastAltitudeMapZoneSerial : Integer ;

      /// <summary>
      ///   A hint wich bitmap factory shpuld be used internally if applicable.
      /// </summary>
      /// <remarks>
      ///   Use mainly internally by TatukGIS.
      /// </remarks>
      FBitmapFactory : TGIS_BitmapFactory ;

    protected
      /// <summary>
      ///   Band definition.
      /// </summary>
      bandsDefinition : TGIS_PixelBandsDefinition ;

      /// <summary>
      /// True if image must be gray scaled by map; False by default.
      /// </summary>
      makeGrayMap : Boolean;

      /// <summary>
      /// True if image bands are saved as BGR .
      /// </summary>
      isBGR : Boolean;

      /// <summary>
      ///   True when plannar configuration is 1; for false configuration is 2
      /// </summary>
      isPlanar1   : Boolean ;

      /// <summary>
      /// True if pixel image is forced to grid.
      /// </summary>
      rgbAsGrid : Boolean ;

      /// <summary>
      /// True if white and black are swapped.
      /// </summary>
      swapBW    : Boolean ;

      /// <summary>
      /// True if image must be colorized by map; False by default.
      /// </summary>
      makeRGBMap : Boolean;

      /// <summary>
      /// True if image must be colorized by map; False by default.
      /// </summary>
      makeColorsByVals : Boolean;

      /// <summary>
      /// True if image must be colorized by red map; False by default.
      /// </summary>
      makeRedMap : Boolean;

      /// <summary>
      /// True if image must be colorized by green map; False by default.
      /// </summary>
      makeGreenMap : Boolean;

      /// <summary>
      /// True if image must be colorized by blue map; False by default.
      /// </summary>
      makeBlueMap : Boolean;

      /// <summary>
      /// True if histogram exist.
      /// </summary>
      isHistogram : Boolean;

      /// <summary>
      /// True if basic histogram exist.
      /// </summary>
      isBasicHistogram : Boolean;

      /// <summary>
      /// True if image is inverted.
      /// </summary>
      isInverted : Boolean;

      /// <summary>
      /// Size of one image pixel in layer units.
      /// </summary>
      FPixelSize : TGIS_Point ;

      /// <summary>
      /// True if layer is tiled.
      /// </summary>
      FIsTiled : Boolean ;

    // various protected variables
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      /// <summary>
      ///   Used to convert to 24-bit format.
      /// </summary>
      testMask      : Array [0..7] of Byte ;

      /// <summary>
      ///   Horizontal scale factor.
      /// </summary>
      scaleX : Double ;

      /// <summary>
      ///   Vertical scale factor.
      /// </summary>
      scaleY : Double ;

      /// <summary>
      /// Total bits count per pixel (for uncompressed data)
      /// </summary>
      bitsPerPixel : Integer ;

      /// <summary>
      ///   Image with associated alpha data.
      /// </summary>
      alphaAssociated : Boolean ;

      /// <summary>
      ///   Byte order from most significant to least significant.
      /// </summary>
      bigEndian : Boolean;

      /// <summary>
      ///   rotation about y-axis divided by scaleY
      /// </summary>
      yRotDivSc : Double ;

      /// <summary>
      ///   rotation about x-axis divided by scaleY
      /// </summary>
      xRotDivSc : Double ;

      /// <summary>
      ///   Center of rotation x
      /// </summary>
      cntrRotX : Double ;

      /// <summary>
      ///   Auxiliary x factor
      /// </summary>
      cntrRotFactX : Double ;

      /// <summary>
      ///   Center of rotation y
      /// </summary>
      cntrRotY : Double ;

      /// <summary>
      ///   Auxiliary y factor
      /// </summary>
      cntrRotFactY : Double ;

      /// <summary>
      ///   scaleXFactor minus yRotDivSc multiplied by xRotDivSc and scaleYFactor
      /// </summary>
      scxF_yRotDivScxRotDivSMcsyF : Double ;

      /// <summary>
      ///   scaleYFactor minus yRotDivSc multiplied by xRotDivSc and scaleXFactor
      /// </summary>
      scyF_yRotDivScxRotDivSMcsxF : Double ;

      /// <summary>
      ///   Scale X factor -1 when scaleX in World File is negative otherwise 1
      /// </summary>
      scaleXFactor : Integer ;

      /// <summary>
      ///   Scale $ factor -1 when scaleY in World File is positive otherwise 1
      /// </summary>
      scaleYFactor : Integer ;

      /// <summary>
      ///   Central point of image
      /// </summary>
      rotationPoint : TGIS_Point ;

      /// <summary>
      ///   Helper for rotations.
      /// </summary>
      rotateSin : Double ;

      /// <summary>
      ///   Helper for rotations.
      /// </summary>
      rotateCos : Double ;

      /// <summary>
      ///   Helper for rotations.
      /// </summary>
      unrotateSin : Double ;

      /// <summary>
      ///   Helper for rotations.
      /// </summary>
      unrotateCos : Double ;

      /// <summary>
      ///   Rotation from World file
      /// </summary>
      baseRotation : Boolean ;

      /// <summary>
      ///   Rotation around image center
      /// </summary>
      internalRotation : Boolean ;

      /// <summary>
      ///   Viewer params are valid
      /// </summary>
      forViewer : Boolean ;

      /// <summary>
      ///   Output CS used in transformation
      /// </summary>
      outCS : TGIS_CSCoordinateSystem ;

      /// <summary>
      ///   Used for mapping 16-bits band to 8-bits
      /// </summary>
      wordDivider  : Single ;

      /// <summary>
      ///   Used for mapping 16-bits band to 8-bits
      /// </summary>
      wordShift  : Integer ;

      /// <summary>
      ///   Bits per pixel for each band
      /// </summary>
      bitsPerBand : array [0..19] of Cardinal ;

      /// <summary>
      ///   Used for mapping 16-bits band to 8-bits
      /// </summary>
      wordMultiply  : Integer ;

      /// <summary>
      ///   Bits per pixel.
      /// </summary>
      realBitCount : Integer;

      /// <summary>
      ///   Line width of layer in bytes.
      /// </summary>
      realLineWidth : Integer;

      /// <summary>
      ///   Internal line width of layer in bytes.
      /// </summary>
      intLineWidth : Integer;

      /// <summary>
      ///   To restore CellWidth after transformation.
      /// </summary>
      baseCellWidth  : Integer ;

      /// <summary>
      ///   To restore CellHeight after transformation.
      /// </summary>
      baseCellHeight : Integer ;

      /// <summary>
      ///   Used in scaling 2D
      /// </summary>
      scaledWidth    : Integer ;

      /// <summary>
      ///   Used in scaling 2D
      /// </summary>
      scaledHeight   : Integer ;

      /// <summary>
      ///   To save last draw destination parameters.
      /// </summary>
      workDestRect : TRect;

      /// <summary>
      ///   To save last draw source parameters.
      /// </summary>
      workSrcRect : TRect ;

      /// <summary>
      ///   Used for importing grid (with association to setFileScale).
      /// </summary>
      extZoom         : Double ;

      /// <summary>
      ///   Used for importing grid (with association to setFileScaleXY).
      /// </summary>
      extZoomX        : Double ;

      /// <summary>
      ///   Used for importing grid (with association to setFileScaleXY).
      /// </summary>
      extZoomY        : Double ;

      /// <summary>
      ///   True if 48 bits per pixel
      /// </summary>
      is48BitsPerPixel : Boolean ;

      /// <summary>
      ///   Number of colors in palette
      /// </summary>
      colorsNo : Cardinal ;

      /// <summary>
      ///   Original layer grid data
      /// </summary>
      srcGrid   : TGIS_GridArray ;

      /// <summary>
      ///    Manipulated grid data
      /// </summary>
      dstGrid   : TGIS_GridArray ;

      /// <summary>
      ///   Bitmap file stream.
      /// </summary>
      fileStream   : TGIS_HandleStream ;

      /// <summary>
      ///   Bitmap palette (must follow infoHeader).
      /// </summary>
      bitPalette : Array of TGIS_Color ;

      /// <summary>
      ///   Copy of bitmap palette
      /// </summary>
      paletteCpy : Array [0..255] of TGIS_Color ;

      /// <summary>
      ///   Histograms for Red channel.
      /// </summary>
      hisRed : Array [0..255] of Integer;

      /// <summary>
      ///   Histograms for Green channel.
      /// </summary>
      hisGreen : Array [0..255] of Integer;

      /// <summary>
      ///   Histograms for Blue channel.
      /// </summary>
      hisBlue : Array [0..255] of Integer;

      /// <summary>
      ///    Histograms for Gray channel..
      /// </summary>
      hisGray : Array [0..255] of Integer;

      /// <summary>
      ///  Color correction.
      /// </summary>
      corRGB : Array [0..255] of Integer;

      /// <summary>
      ///  RGB color mapping.
      /// </summary>
      mapRGB : Array of Integer;

      /// <summary>
      ///   Full RGB color mapping.
      /// </summary>
      corFullRGB : Array [0..255] of Integer;

      /// <summary>
      ///   Full RGB color mapping -red band.
      /// </summary>
      isCorFullRGBred : Array [0..255] of Int64 ;

      /// <summary>
      ///   Full RGB color mapping -red band.
      /// </summary>
      isCorFullRGBgreen : Array [0..255] of Int64 ;

      /// <summary>
      ///   Full RGB color mapping -red band.
      /// </summary>
      isCorFullRGBblue : Array [0..255] of Int64 ;

      /// <summary>
      ///   Gray mapping table.
      /// </summary>
      mapGray2RGB : Array of Integer;

      /// <summary>
      ///   Helper for rotations.
      /// </summary>
      baseProjectedExtent : TGIS_Extent  ;

      /// <summary>
      ///   True if transformation was previously set.
      /// </summary>
      wasTransform : Boolean ;

      /// <summary>
      ///   True if image must be colorized by full RGB map; False by
      ///   default.
      /// </summary>
      makeFullRGBMap : Boolean ;

      /// <summary>
      ///   Reading image from net.
      /// </summary>
      isFromNet : Boolean ;

      /// <summary>
      ///   If any bitmap pixels changes is needed.
      /// </summary>
      makeSomeCorrection : Boolean ;

      /// <summary>
      ///   True when Transform is assigned with Active flag set on True.
      /// </summary>
      activeTransform : Boolean ;

      /// <summary>
      ///   Color used for not defined data.
      /// </summary>
      colorNoData : TGIS_Color  ;

      /// <summary>
      ///  Numbers of bands using as red, green, blue and alpha
      /// </summary>
      bandsMap : Array [0..19] of Integer ;

      /// <summary>
      ///   True if unused alpha band
      /// </summary>
      unusedAlpha : Boolean ;

      /// <summary>
      ///   True if unused red band
      /// </summary>
      unusedRed : Boolean ;

      /// <summary>
      ///   True if unused green band
      /// </summary>
      unusedGreen : Boolean ;

      /// <summary>
      ///   True if unused blue band
      /// </summary>
      unusedBlue : Boolean ;

      /// <summary>
      ///   Layer created in memory.
      /// </summary>
      memoryResident : Boolean ;

      /// <summary>
      ///   MinHeight is set by user.
      /// </summary>
      modifiedMinHeight : Boolean ;

      /// <summary>
      ///   MaxHeight is set by user.
      /// </summary>
      modifiedMaxHeight : Boolean ;

      /// <summary>
      ///   True if alpha band is effective used.
      /// </summary>
      ///
      isPartialTransparent : Boolean ;

      /// <summary>
      ///   No data color is defined.
      /// </summary>
      isColorNoDataGDAL : Boolean ;

      /// <summary>
      ///   Default using of alpha band.
      /// </summary>
      defaultPartialTransparent : Boolean ;

      /// <summary>
      ///   Native transparent color used in some layers
      /// </summary>
      internalTransparentColor : TGIS_Color  ;

      /// <summary>
      ///   Alpha data buffer.
      /// </summary>
      alphaBuffer : TBytes ;

      /// <summary>
      ///   To work on alpha data.
      /// </summary>
      workAlpha : TBytes ;

      /// <summary>
      ///   line buffer (line of bytes).
      /// </summary>
      lineAlphaBuffer : TBytes ;

      /// <summary>
      ///   Approximate pixel size in meters. Important form proper
      ///   high/width angle calculations.
      /// </summary>
      sizeShadow : Double ;

      /// <summary>
      ///   Last shadow value.
      /// </summary>
      prevShadowValue : Double ;

      /// <summary>
      ///   Last shadow value.
      /// </summary>
      prevShadowDelta : Double ;

      /// <summary>
      ///   Should shadowing be used?
      /// </summary>
      isShadow : Boolean ;

      /// <summary>
      ///   True if single values are mapped to color values using altitude map zones
      /// </summary>
      useAltitudeZones : Boolean ;

      /// <summary>
      ///   Table with altitude zones
      /// </summary>
      altitudeZones : Array of TGIS_AltitudeZone ;

      /// <summary>
      ///   True if single values are mapped to color value using color ramp
      /// </summary>
      useColorRamp : Boolean ;

      /// <summary>
      ///   increase coloring performance by storing Params.Pixel.ColorRamp property
      /// </summary>
      assignedColorRamp : TGIS_ColorMapArray ;

      /// <summary>
      ///   True if image is gray scaled.
      /// </summary>
      isGrayScaleImage : Boolean;

      /// <summary>
      ///   Min max valid vor Gid Band number
      /// </summary>
      mmzgbn : Integer ;

      /// <summary>
      ///   True if image must be transparent; False by default.
      /// </summary>
      makeTransparent : Boolean;

      /// <summary>
      ///   Bits to skio on lfet for each band
      /// </summary>
      bitsSkipLeft : array [0..19] of Cardinal ;

      /// <summary>
      ///   Bits to skio on right for each band
      /// </summary>
      bitsSkipRight : array [0..19] of Cardinal ;

      /// <summary>
      ///   Bytes per pixel for each band
      /// </summary>
      bytesPerBand : array [0..19] of Cardinal ;

      /// <summary>
      ///   Number of band used by function LockPixels.
      /// </summary>
      fromBand : Integer ;

      /// <summary>
      ///   LockPixels used as writable.
      /// </summary>
      asWritable : Boolean ;

      /// <summary>
      ///   Number of using LockPixels.
      /// </summary>
      lockUsingNo : Integer ;

      /// <summary>
      ///   Number of using as readable LockPixels.
      /// </summary>
      readableNo : Integer ;

      /// <summary>
      ///   From function LockPixels on band.
      /// </summary>
      bandMask : Integer ;

      /// <summary>
      ///   Image file data as bits string.
      /// </summary>
      isBitsString : Boolean ;

      /// <summary>
      ///   Summary number bytes per pixel for all bands
      /// </summary>
      bytesPerPixel : Integer ;

      /// <summary>
      ///   True if image will be imported; False by default.
      /// </summary>
      importMode : Boolean;

      /// <summary>
      ///   List of transparent colors in Red channel.
      /// </summary>
      redTransp : Array [0..255] of Int64;

      /// <summary>
      ///   List of transparent colors in Green channel.
      /// </summary>
      greenTransp : Array [0..255] of Int64;

      /// <summary>
      ///   List of transparent colors in Blue channel.
      /// </summary>
      blueTransp : Array [0..255] of Int64;

      /// <summary>
      ///   To save last grid cell lines values}.
      /// </summary>
      gridCol : TGIS_GridArray ;

      /// <summary>
      ///   Grid operation event handle.
      /// </summary>
      FGridOperation : TGIS_GridOperation ;

      /// <summary>
      ///   Pixel operation event handle.
      /// </summary>
      FPixelOperation : TGIS_PixelOperation ;

      /// <summary>
      ///   Shadow angle used for grid shadowing.
      /// </summary>
      FShadowAngle : Double ;

      /// <summary>
      ///   Current sub-format information.
      /// </summary>
      FSubFormat : TGIS_LayerPixelSubFormat ;

      /// <summary>
      ///   Forced BandsDefinition.
      /// </summary>
      FForcedBandsDefinition : String ;

    {$IFDEF OXYGENE} unit or protected {$ELSE} protected {$ENDIF}

      /// <summary>
      ///   Read double (8 bytes) from little endian stored bytes.
      /// </summary>
      /// <returns>
      ///   converted double
      /// </returns>
      function  readDoubleLE     : Double ;

      /// <summary>
      ///   Read double (8 bytes) from big endian stored bytes.
      /// </summary>
      /// <returns>
      ///   converted double
      /// </returns>
      function  readDoubleBE     : Double ;

      /// <summary>
      ///   Compute position of the point after the defined in world file
      ///   rotation.
      /// </summary>
      /// <returns>
      ///   rotated point
      /// </returns>
      /// <param name="_pt">
      ///   point to be rotated
      /// </param>
      function  basePointRot  (const _pt : TGIS_Point) : TGIS_Point ;

      /// <summary>
      ///   Compute original position of the rotated point.
      /// </summary>
      /// <returns>
      ///   original point
      /// </returns>
      /// <param name="_pt">
      ///   rotated point
      /// </param>
      function  basePointUnRot(const _pt : TGIS_Point) : TGIS_Point ;

      /// <summary>
      ///   Compute extent (encompassing area of the extent) after defined in
      ///   world file rotation.
      /// </summary>
      /// <returns>
      ///   rotated extent
      /// </returns>
      /// <param name="_ext">
      ///   extent to be rotated
      /// </param>
      function  baseRotatedExtent
                              ( const _ext    : TGIS_Extent
                              ) : TGIS_Extent ;

      /// <summary>
      ///   Compute original extent of the rotated extent.
      /// </summary>
      /// <returns>
      ///   extent bigger then original with center placed in original center
      /// </returns>
      /// <param name="_ext">
      ///   rotated extent
      /// </param>
      function  baseUnrotatedExtent
                              ( const _ext    : TGIS_Extent
                              ) : TGIS_Extent ;

      /// <summary>
      ///   Gray scale from ARGB  bitmap.
      /// </summary>
      /// <param name="_pixels">
      ///   Buffer with ARGB pixels.
      /// </param>
      /// <param name="_size">
      ///   Buffer size.
      /// </param>
      procedure ARGB2Gray( const _pixels : TGIS_Pixels  ;
                           const _size   : Integer
                          ) ;

      /// <summary>
      ///   Prepare final mapping table for full color (32-bits) images.
      /// </summary>
      procedure prepFinalCorTable ;

      /// <summary>
      ///   Applying  brightness, contrast and colors correction from pixel
      ///   params to final correction table.
      /// </summary>
      procedure  applyColorsCorrectionByVals ;

      /// <summary>
      ///   Prepare final mapping table for full color (32-bits) images.
      /// </summary>
      /// <param name="_pix">
      ///   Pixels area with _width*_height size
      /// </param>
      /// <param name="_width">
      ///   With of tables (image to view)
      /// </param>
      /// <param name="_height">
      ///   Height of tables (image to view)
      /// </param>
      procedure fullCorrection ( const _pix    : TGIS_Pixels ;
                                 const _width  : Integer ;
                                 const _height : Integer
                                ) ;

      /// <summary>
      ///   Adding transparency defined in transparent zones to image pixels .
      /// </summary>
      /// <param name="_pix">
      ///   Image buffer, [pixels area with _width*_height size
      /// </param>
      /// <param name="_width">
      ///   With of tables (image to view)
      /// </param>
      /// <param name="_height">
      ///   Height of tables (image to view)
      /// </param>
      procedure addTransparency ( const _pix    : TGIS_Pixels ;
                                  const _width  : Integer ;
                                  const _height : Integer
                                 ) ;

      /// <summary>
      ///  Does final ARGB bands mapping and set on/off.
      /// </summary>
      /// <param name="_pix">
      ///   Image buffer, [ixels area with _width*_height size
      /// </param>
      /// <param name="_width">
      ///   With of tables (image to view)
      /// </param>
      /// <param name="_height">
      ///   Height of tables (image to view)
      /// </param>
      procedure finalARGBMap ( const _pix    : TGIS_Pixels ;
                               const _width  : Integer ;
                               const _height : Integer
                             ) ;



      /// <summary>
      ///   Mapping full color (32-bits) images by .
      /// </summary>
      /// <param name="_pix">
      ///   Pixels area with _width*_height size
      /// </param>
      /// <param name="_width">
      ///   With of tables (image to view)
      /// </param>
      /// <param name="_height">
      ///   Height of tables (image to view)
      /// </param>
      procedure fullRGBCorrection ( const _pix    : TGIS_Pixels ;
                                    const _width  : Integer ;
                                    const _height : Integer
                                  ) ;

      /// <summary>
      ///   Mapping red, green, blue colors bands if image.
      /// </summary>
      /// <param name="_pix">
      ///   Pixels area with _width*_height size
      /// </param>
      /// <param name="_width">
      ///   With of tables (image to view)
      /// </param>
      /// <param name="_height">
      ///   Height of tables (image to view)
      /// </param>
      procedure simpleRGBCorrection ( const _pix    : TGIS_Pixels ;
                                      const _width  : Integer ;
                                     const _height : Integer
                                    ) ;





      /// <summary>
      ///   Enhance contrast by changing RGB values proportionally from 0 to 255.
      /// </summary>
      /// <param name="_pix">
      ///   Pixels area with _width*_height size
      /// </param>
      /// <param name="_width">
      ///   With of tables (image to view)
      /// </param>
      /// <param name="_height">
      ///   Height of tables (image to view)
      /// </param>
      procedure enhanceContrast ( const _pix    : TGIS_Pixels ;
                                 const _width  : Integer ;
                                 const _height : Integer
                                ) ;


      /// <summary>
      ///   Fills pixels array with ARGB values from source grid table.
      /// </summary>
      /// <param name="_extent">
      ///   Current extent
      /// </param>
      /// <param name="_argb">
      ///   Output pixels array
      /// </param>
      /// <param name="_grid">
      ///   Input grid data
      /// </param>
      /// <param name="_width">
      ///   With of tables (image to view)
      /// </param>
      /// <param name="_height">
      ///   Height of tables (image to view)
      /// </param>
      procedure gridToARGBTable( const _extent : TGIS_Extent ;
                                 const _argb   : TGIS_Pixels ;
                                 const _grid   : TGIS_GridArray ;
                                 const _width  : Integer       ;
                                 const _height : Integer
                               ) ;

      /// <summary>
      ///   Prepare corFullRGB table.
      /// </summary>
      procedure prepFullRGBMapTbl ;

      /// <summary>
      ///   Replces origninal image by modified (temporary).
      /// </summary>
      /// <param name="_tmpname">
      ///   Working file name
      /// </param>
      procedure replaceWorkingFiles(const _tmpname : String) ;


    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <inheritdoc from="TGIS_Layer"/>
      function  drawExtentEx   : TGIS_Extent ; override ;

  protected // properties access routines

      function  fget_AltitudeMapZonesCount : Integer ;

      procedure fset_GridImage      ( const _value : Boolean
                                    ) ; virtual;
      procedure fset_MinThresholdHeight  ( const _value : Single
                                    ) ; virtual;
      procedure fset_MaxThresholdHeight  ( const _value : Single
                                    ) ; virtual;
      procedure fset_CuttingPolygon ( const _value : TGIS_ShapePolygon
                                    ) ; virtual;
      procedure fset_Antialias      ( const _value : Boolean
                                    ) ; virtual;
      function  fget_ParamsPixel    : TGIS_ParamsSectionPixel ;
      function  fget_Extent         : TGIS_Extent ; override ;
      procedure fset_ParamsPixel    ( const _value : TGIS_ParamsSectionPixel
                                    ) ;
      function  fget_GridOperation  : TGIS_GridOperation ;
      procedure fset_GridOperation  ( const _value : TGIS_GridOperation
                                    ) ;

      procedure fset_Interpretation ( const _value : TGIS_LayerPixelInterpretation
                                    ) ; virtual ;


      function  fget_PixelOperation : TGIS_PixelOperation ;
      procedure fset_PixelOperation ( const _value : TGIS_PixelOperation
                                    ) ;

      function  fget_Page           : Integer ; virtual;

      function  fget_MinHeight      : Single ; virtual;
      procedure fset_MinHeight      ( const _value : Single
                                    ) ; virtual;
      procedure fset_RotationAngle  ( const _value : Double
                                    ) ;
      function  fget_MaxHeight      : Single ; virtual;
      procedure fset_MaxHeight      ( const _value : Single
                                    ) ; virtual;

      function  fget_Capabilities   : TGIS_LayerPixelSubFormatList ; virtual;
      function  fget_DefaultSubFormat : TGIS_LayerPixelSubFormat ; virtual;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <summary>
      ///   If image transformation is needed.
      /// </summary>
      /// <returns>
      ///   True if image transformation is needed.
      /// </returns>
      function  transformNeed  : Boolean ;

      /// <summary>
      ///   Checks locked pixels.
      /// </summary>
      /// <param name="_ext">
      ///   area to be locked; if bigger then layer extent then will be
      ///   truncated to layer extent
      /// </param>
      /// <param name="_cs">
      ///   coordinate system of _cs; if nil then same as layer CS
      /// </param>
      /// <param name="_pixelsize">
      ///   minimum requested size of image pixel; by providing 0 a maximum
      ///   possible resolution will be used
      /// </param>
      /// <param name="_band">
      ///   band number if specific band is locked or 0
      /// </param>
      /// <param name="_writable">
      ///   True when searching for writable lock
      /// </param>
      /// <returns>
      ///   Index for oLayerList (-1 if not find).
      /// </returns>
      function  findProperPixelLock ( const _ext  : TGIS_Extent ;
                                      const _cs   : TGIS_CSCoordinateSystem ;
                                      const _pixelsize : Double;
                                      const _band : Integer ;
                                      const _writable : Boolean
                                    ) : Integer ;

      /// <summary>
      ///   If image reprojection is needed.
      /// </summary>
      /// <returns>
      ///   True if image reprojection is needed.
      /// </returns>
      function  projectionNeed : Boolean ;

      /// <summary>
      ///   If image rotation is needed.
      /// </summary>
      /// <returns>
      ///   True if image rotation is needed.
      /// </returns>
      function  rotationNeed   : Boolean ;

      /// <summary>
      ///   Prepare altitude map array.
      /// </summary>
      procedure prepareAltitudeMapTable ; virtual;

      /// <summary>
      ///   Prepare color ramp array.
      /// </summary>
      procedure prepareColorRamp ; virtual;

      /// <summary>
      ///   Set bitmap palette to gray scale image.
      /// </summary>
      procedure setBitmapPalette ;

      /// <summary>
      ///   Prepare histograms for ARGB (32-bits) bitmap scan line.
      /// </summary>
      /// <param name="_buffer">
      ///   image pixels buffer
      /// </param>
      /// <param name="_width">
      ///   image width in pixels.
      /// </param>
      /// <param name="_height">
      ///   image height in pixels.
      /// </param>
      procedure prepRGBHist( const _buffer   : TGIS_Pixels ;
                             const _width    : Integer ;
                             const _height   : Integer ) ;

      /// <summary>
      ///   Read, or make and save, the histogram.
      /// </summary>
      procedure makeHistogram ;

      /// <summary>
      ///   Prepare mapping table with no correction
      ///   for full color images.
      /// </summary>
      procedure prepCorTbl ;

      /// <summary>
      ///   Prepare correction (mapping) table with histogram equalization
      ///   for full color images.
      /// </summary>
      procedure histEqPav ;

      /// <summary>
      ///  Prepare an array (corRGB) for mapping full RGB colors.
      ///  points (redPointList, greenPointList, bluePointlist).
      /// </summary>
      procedure prepColorMapTbl ;

      /// <summary>
      ///   Preparing map table for making a gray scale image from a full
      ///   color (32-bits) bitmap scan line.
      /// </summary>
      procedure prepFullGrayTable ;

      /// <summary>
      ///   Prepare an array (mapGray2RGB) for mapping gray scale to colors by
      ///   points (grayMapList).
      /// </summary>
      procedure prepGrayMapTbl ;

      /// <summary>
      ///   Prepare one histogram from RGB channel histograms.
      /// </summary>
      procedure histRGB2Gray ;

      /// <summary>
      ///    Set mapping zone in color map table.
      /// </summary>
      /// <param name="_maparray">
      ///  mapped colors array
      /// </param>
      /// <param name="_color">
      ///   effective color.
      /// </param>
      /// <param name="_zone">
      ///   define map zone.
      /// </param>
      procedure setMapZone    ( var   _maparray   : array  of Integer ;
                                const _color      : TGIS_Color  ;
                                const _zone       : TGIS_MapZone
                               ) ;

      /// <summary>
      ///  Getting mapping zone from String
      /// </summary>
      /// <returns>
      ///   True if success
      /// </returns>
      /// <param name="_zonestr">
      ///  string usually from pixel params
      /// </param>
      /// <param name="_zone">
      ///   define map zone.
      /// </param>
      function getMapZoneVal    ( const _zonestr    : String  ;
                                  var   _zone       : TGIS_MapZone
                                ) : Boolean ;




      /// <summary>
      ///   Prepare transparency table.
      /// </summary>
      procedure prepTransparent ;

      /// <summary>
      ///   Prepare a negative from the RGB palette.
      /// </summary>
      procedure inversePalette ;

      /// <summary>
      ///   setting up pixel layer parameters
      /// </summary>
      procedure setUpInternal     ; virtual;

      /// <summary>
      ///   Imports and saves data form pixel layer.
      /// </summary>
      /// <returns>
      ///   True if success
      /// </returns>
      /// <param name="_layer">
      ///   source layer
      /// </param>
      function  importPixelData   ( const _layer       : TGIS_LayerPixel
                                  ) : Boolean ; virtual;

      /// <summary>
      ///   makes a general layer setup
      /// </summary>
      procedure setUp  ; override;

      /// <inheritdoc/>
      procedure setUp3             ; override;

      /// <inheritdoc/>
      procedure applyConfigOptions ( const _cfg        : TGIS_ConfigAbstract
                                   ) ; override;

      /// <inheritdoc/>
      procedure storeConfigOptions ( const _cfg        : TGIS_ConfigAbstract
                                   ) ; override;

      /// <summary>
      ///   Set extent based on world file (like .tfw, .bpw) itp.
      ///  Also read projection information form the .prj if world file exists.
      /// If world file does not exists tries to read world/projection form the TAB
      /// file.
      /// </summary>
      /// <param name="_ext">
      ///   extension of the world file (like .tfw, .bpw)
      /// </param>
      procedure setWorldFile( const _ext : String ) ;

      /// <summary>
      ///   Set internal imagery scale.
      /// </summary>
      /// <param name="_dwidth">
      ///   destination width
      /// </param>
      /// <param name="_swidth">
      ///   source width.
      /// </param>
      /// <returns>
      ///   Current scale.
      /// </returns>
      function  setFileScale      ( const _dwidth : Double ;
                                    const _swidth : Double
                                  ) : Double ; virtual;

      /// <summary>
      ///   Set internal imagery scale.
      /// </summary>
      /// <param name="_dwidth">
      ///   destination width
      /// </param>
      /// <param name="_swidth">
      ///   source width.
      /// </param>
      /// <param name="_dheight">
      ///   destination height
      /// </param>
      /// <param name="_sheight">
      ///   source height.
      /// </param>
      /// <returns>
      ///   Current X scale.
      /// </returns>
      function  setFileScaleXY    ( const _dwidth  : Double ;
                                    const _swidth  : Double ;
                                    const _dheight : Double ;
                                    const _sheight : Double
                                  ) : Double ; virtual;


      /// <summary>
      ///   Sets CurrentPage in multipages image
      /// </summary>
      /// <param name="_value">
      ///   chosen page number
      /// </param>
      procedure setPage             ( const _value : Integer
                                    ) ; virtual;


      /// <summary>
      ///   Set up parameters cache. Please do not call this directly.
      /// </summary>
      procedure setupParams       ; virtual;

      /// <summary>
      ///   Checks bands mapping.
      /// </summary>
      /// <returns>
      ///   True if mapping is different to default setting.
      /// </returns>
     function bandsMappingChanging : Boolean ; virtual ;


      /// <summary>
      ///   Resolve bands definition from ForceBandLayout or metadata.
      /// </summary>
      /// <returns>
      ///   True if definitions were modified.
      /// </returns>
      function resolveBandDefintion : Boolean ;


      /// <summary>
      ///   Fills provided _grid array with values defined by _extent. If the
      ///   current layer does not fully cover the _extent then values outside
      ///   the layer scope should be left untouched.
      /// </summary>
      /// <returns>
      ///   True if success
      /// </returns>
      /// <param name="_extent">
      ///   extent of the _grid
      /// </param>
      /// <param name="_grid">
      ///   allocated array (with/height ratio of the _grid should be the same
      ///   as width/height ration of the _extent)
      /// </param>
      /// <remarks>
      ///   Function provide "raw" data - before coordinate system applied.
      /// </remarks>
      function  getGridData       ( const _extent       : TGIS_Extent      ;
                                    const _grid         : TGIS_GridArray
                                  ) : Boolean ; virtual;

      /// <summary>
      ///   Fills provided _grid array with NoDataValue
      /// </summary>
      /// <param name="_grid">
      ///   allocated grid table(with/height ratio of the _bitmap should be the
      ///   same as width/height ration of the _extent)
      /// </param>
      procedure setNoDataTable (  const _grid   : TGIS_GridArray
                               ) ;

      /// <summary>
      ///   Set internal imagery view.
      /// </summary>
      /// <param name="_viewRect">
      ///   define part of image which will be displaying
      /// </param>
      procedure setFileView       ( const _viewRect : TRect
                                  ) ; virtual;

      /// <summary>
      ///   Fills provided _bitmap array with values defined by _extent. If the
      ///   current layer does not fully cover the _extent then values outside
      ///   the layer scope should be left untouched.
      /// </summary>
      /// <returns>
      ///   True if success; False if anything is still pending
      /// </returns>
      /// <param name="_extent">
      ///   extent of the _bitmap
      /// </param>
      /// <param name="_bitmap">
      ///   allocated bitmap(with/height ratio of the _bitmap should be the
      ///   same as width/height ration of the _extent)
      /// </param>
      /// <param name="_width">
      ///   width of needed area in pixels
      /// </param>
      /// <param name="_height">
      ///   height of needed area in pixels
      /// </param>
      /// <remarks>
      ///   Function provide "raw" data - before coordinate system applied.
      /// </remarks>
      function  getBitmapData     ( const _extent   : TGIS_Extent ;
                                    const _bitmap   : TGIS_Pixels ;
                                    const _width    : Integer ;
                                    const _height    : Integer
                                  ) : Boolean ; virtual;

      /// <summary>
      ///   Fills provided _bitmap array with values defined by _extent. If the
      ///   current layer does not fully cover the _extent then values outside
      ///   the layer scope should be left untouched.
      /// </summary>
      /// <returns>
      ///   True if success
      /// </returns>
      /// <param name="_extent">
      ///   extent of the _grid
      /// </param>
      /// <param name="_bitmap">
      ///   allocated bitmap(with/height ratio of the _bitmap should be the
      ///   same as width/height ration of the _extent)
      /// </param>
      /// <param name="_width">
      ///   width of needed area in pixels
      /// </param>
      /// <param name="_height">
      ///   height of needed area in pixels
      /// </param>
      /// <remarks>
      ///   Function operates on "raw" data - before coordinate system applied.
      /// </remarks>
      function  putBitmapRawData  ( const _extent   : TGIS_Extent ;
                                    const _bitmap   : TGIS_Pixels ;
                                    const _width    : Integer ;
                                    const _height    : Integer
                                  ) : Boolean ;

      /// <summary>
      ///   Fills provided _bitmap array with Alpha value = $FF (full
      ///   transparent).
      /// </summary>
      /// <param name="_bitmap">
      ///   allocated bitmap(with/height ratio of the _bitmap should be the
      ///   same as width/height ration of the _extent)
      /// </param>
      procedure setBmpTransparent (  const _bitmap   : TGIS_Pixels
                                  ) ;

      /// <summary>
      ///   Report data fetching state from asynchronous sources like ECW.
      /// </summary>
      /// <param name="_abort">
      ///   if True, process should be aborted
      /// </param>
      /// <param name="_ready">
      ///   if True than all data has been read
      /// </param>
      /// <param name="_nodata">
      ///   if True than data are not yet available
      /// </param>
      procedure getAsyncState     ( var _abort  : Boolean ;
                                    var _ready  : Boolean ;
                                    var _nodata : Boolean
                                  ) ; virtual;



      /// <summary>
      ///   Internal use only. For reading an image line.
      /// </summary>
      /// <returns>
      ///   number of read bytes
      /// </returns>
      /// <param name="_buffer">
      ///   bytes array
      /// </param>
      /// <param name="_offset">
      ///   start in _buffer
      /// </param>
      /// <param name="_linenr">
      ///   line number
      /// </param>
      /// <param name="_start">
      ///   left margin (bytes to skip)
      /// </param>
      /// <param name="_bytes">
      ///   bytes count
      /// </param>
      function  getLine         ( const _buffer : TBytes  ;
                                  const _offset : Integer ;
                                  const _linenr : Integer ;
                                  const _start  : Integer ;
                                  const _bytes  : Integer
                                ) : Integer ; virtual;


      /// <summary>
      ///   Internal use only. For reading an image line.
      /// </summary>
      /// <returns>
      ///   number of read pixels
      /// </returns>
      /// <param name="_buffer">
      ///   pixels array
      /// </param>
      /// <param name="_offset">
      ///   start in _buffer
      /// </param>
      /// <param name="_linenr">
      ///   line number
      /// </param>
      /// <param name="_pixStart">
      ///   left margin (pixels to skip)
      /// </param>
      /// <param name="_pixCount">
      ///   number of image pixels to read
      /// </param>
      function  getLinePixels     ( const _buffer   : TGIS_Pixels  ;
                                    const _offset   : Integer ;
                                    const _linenr   : Integer ;
                                    const _pixStart : Integer ;
                                    const _pixCount : Integer
                                  ) : Integer; virtual;

      /// <summary>
      ///   Internal use only. For reading an image line data.
      /// </summary>
      /// <returns>
      ///   number of read bytes
      /// </returns>
      /// <param name="_buffer">
      ///   bytes array
      /// </param>
      /// <param name="_offset">
      ///   start in _buffer
      /// </param>
      /// <param name="_linenr">
      ///   line number
      /// </param>
      /// <param name="_pixStart">
      ///   left margin (pixels to skip)
      /// </param>
      /// <param name="_pixCount">
      ///   number of image pixels to read
      /// </param>
      function  getLineBits      ( const _buffer   : TBytes  ;
                                   const _offset   : Integer ;
                                   const _linenr   : Integer ;
                                   const _pixStart : Integer ;
                                   const _pixCount : Integer
                                 ) : Integer; virtual;

      /// <summary>
      ///   Internal use only. For converting data bytes to an image line pixels.
      /// </summary>
      /// <returns>
      ///   number of read pixels
      /// </returns>
      /// <param name="_buffSrc">
      ///   bytes array with initial data
      /// </param>
      /// <param name="_srcOffset">
      ///   start in _buffSrc
      /// </param>
      /// <param name="_buffDst">
      ///   pixels array with output data
      /// </param>
      /// <param name="_dstOffset">
      ///   start in _buffDst
      /// </param>
      /// <param name="_pixStart">
      ///   left margin (pixels to skip)
      /// </param>
      /// <param name="_pixCount">
      ///   number of image pixels to read
      /// </param>
      function  convertBitsToPixels
                                   ( const _buffSrc  : TBytes  ;
                                     const _srcOffset : Integer ;
                                     const _buffDst   : TGIS_Pixels ;
                                     const _dstOffset : Integer ;
                                     const _pixStart   : Integer ;
                                     const _pixCount  : Integer
                                   ) : Integer ; virtual ;


      /// <summary>
      ///   Internal use only. For reading an image line.
      /// </summary>
      /// <returns>
      ///   number of read bytes
      /// </returns>
      /// <param name="_buffer">
      ///   bytes array
      /// </param>
      /// <param name="_offset">
      ///   start in _buffer
      /// </param>
      /// <param name="_linenr">
      ///   line number
      /// </param>
      /// <param name="_start">
      ///   left margin (bytes to skip)
      /// </param>
      /// <param name="_bytes">
      ///   bytes count
      /// </param>
      function  getLineBasic    ( const _buffer : TBytes  ;
                                  const _offset : Integer ;
                                  const _linenr : Integer ;
                                  const _start  : Integer ;
                                  const _bytes  : Integer
                                ) : Integer ;



      /// <summary>
      ///   Internal use only. For reading an image line alpha information.
      /// </summary>
      /// <returns>
      ///   number of read bytes
      /// </returns>
      /// <param name="_buffer">
      ///   bytes array
      /// </param>
      /// <param name="_offset">
      ///   start in _buffer
      /// </param>
      /// <param name="_linenr">
      ///   line number
      /// </param>
      /// <param name="_start">
      ///   left margin (bytes to skip)
      /// </param>
      /// <param name="_bytes">
      ///   bytes count
      /// </param>
      function  getAlphaLine    ( const _buffer : TBytes  ;
                                  const _offset : Integer ;
                                  const _linenr : Integer ;
                                  const _start  : Integer ;
                                  const _bytes  : Integer
                                ) : Integer ; virtual;

      /// <summary>
      ///   Lanczos filtering function.
      /// </summary>
      /// <returns>
      ///   Filtered value
      /// </returns>
      /// <param name="_dval">
      ///   input value
      /// </param>
      function lanczos3(_dval : Single) : Single ;

      /// <summary>
      ///   Linear (triangle) filtering function.
      /// </summary>
      /// <returns>
      ///   Filtered value
      /// </returns>
      /// <param name="_dval">
      ///   input value
      /// </param>
      function linear(_dval : Single) : Single ;

      /// <summary>
      ///   Internal use only. For reading original grid value of point.
      /// </summary>
      /// <returns>
      ///   True if success
      /// </returns>
      /// <param name="_pt">
      ///   needed point
      /// </param>
      /// <param name="_ar">
      ///   array of float values for _pt
      /// </param>
      function  getNativeValue    ( const _pt       : TPoint  ;
                                    const _ar       : TGIS_DoubleArray
                                  ) : Boolean ; virtual;

      /// <summary>
      ///   Internal use only.
      /// </summary>
      /// <returns>
      ///   number of read bytes
      /// </returns>
      /// <param name="_buffer">
      ///   predefined buffer of singles
      /// </param>
      /// <param name="_linenr">
      ///   number of needed grid line
      /// </param>
      /// <param name="_startIdx">
      ///   left margin (pixels to skip)
      /// </param>
      /// <param name="_count">
      ///   pixel count
      /// </param>
      function  getNativeLine     ( const _buffer   : TGIS_SingleArray ;
                                    const _linenr   : Integer          ;
                                    const _startIdx : Integer          ;
                                    const _count    : Integer
                                  ) : Integer ; virtual;

      /// <summary>
      ///   Internal use only.
      /// </summary>
      /// <returns>
      ///   number of read pixels
      /// </returns>
      /// <param name="_buffer">
      ///   predefined buffer of singles
      /// </param>
      /// <param name="_linenr">
      ///   number of needed grid line
      /// </param>
      /// <param name="_startIdx">
      ///   left margin (pixels to skip)
      /// </param>
      /// <param name="_count">
      ///   pixel count
      /// </param>
      function  getNativeLineGrayGrid( const _buffer   : TGIS_SingleArray ;
                                       const _linenr   : Integer          ;
                                       const _startIdx : Integer          ;
                                       const _count    : Integer
                                     ) : Integer ; virtual;


      /// <summary>
      ///   Scan grid to prepare min/max value of altitude
      /// </summary>
      /// <param name="_zoom">
      ///   will calculate value only for a defined zoom; will increase the speed
      ///   but calculation will not be exact; if -1 then will scale to the 200
      ///   pixels size
      /// </param>
      procedure prepareMinMaxZ    ( const _zoom : Double = -1
                                  ) ; virtual ;

      /// <summary>
      ///   Scan image to prepare min/max value of altitude when
      ///   interpreting RGB as single channel "AsGrid".
      /// </summary>
      /// <param name="_zoom">
      ///   will calculate value only for a given zoom; will increase the speed
      ///   but calculation will not be exact; if -1 then will scale to the 200
      ///   pixels size
      /// </param>
      procedure prepareMinMaxZGray( const _zoom : Double = -1
                                  ) ; virtual ;


      /// <summary>
      ///   Calculate pixel size in layer units.
      /// </summary>
      /// <param name="_pixelsize">
      ///   requested size of an image pixel; by providing 0 an actual
      ///   viewer scale should be used to calculate a layer's
      ///   pixel size; &gt;0 - pixel size is expressed in map units;
      ///   &lt;0 - pixels is expressed in layer units
      /// </param>
      /// <returns>
      ///    pixel size in layer units
      /// </returns>
      /// <remarks>
      ///    To be used in LocateEx() calculations.
      /// </remarks>
      function  calculatePixelsize( const _pixelsize  : Double
                                  ) : Double ;

      /// <summary>
      ///   Gets bitmap layer data in ARGB format.
      /// </summary>
      /// <returns>
      ///   True if success
      /// </returns>
      /// <param name="_extent">
      ///   extent of the _bitmap
      /// </param>
      /// <param name="_bitmap">
      ///   predefined buffer of Integers (_width*_height size)
      /// </param>
      /// <param name="_width">
      ///   width of needed area in pixels
      /// </param>
      /// <param name="_height">
      ///   height of needed area in pixels
      /// </param>
      /// <remarks>
      ///   Function provide data with coordinate system applied.
      /// </remarks>
      function  getBitmapPixels ( const _extent : TGIS_Extent ;
                                  const _bitmap : TGIS_Pixels ;
                                  const _width  : Integer     ;
                                  const _height : Integer
                                ) : Boolean ; virtual;

      /// <summary>
      ///   Open a file stream in read mode.
      /// </summary>
      /// <param name="_path">
      ///   file path
      /// </param>
      /// <returns>
      ///   handle to file
      /// </returns>
      function openBufferedFileStream( const _path : String
                                     ) : TGIS_HandleStream ;
    protected
      procedure doDestroy         ; override;

    public // API
      /// <inheritdoc/>
      constructor Create          ; override;

      /// <inheritdoc/>
      procedure ReOpen            ; override;

      /// <summary>
      ///   Assigns a parent layer. To be used only in layers like WMTS.
      /// </summary>
      /// <param name="_parent">
      ///   parent layer
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///     This method is used only for internal use on layer reading.
      ///   </note>
      /// </remarks>
      procedure AssignedParentLayerInternal( const _parent : TGIS_LayerPixel
                                           ) ;

      /// <summary>
      ///   Applies antialias settings used in scaling operations.
      /// </summary>
      /// <param name="_antialias">
      ///   If True, antialias will be active
      /// </param>
      procedure ApplyAntialiasSettings( const _antialias       : Boolean
                                      ) ; overload ;

      /// <summary>
      ///   Applies antialias settings used in scaling operations.
      /// </summary>
      /// <param name="_antialias">
      ///   If True, antialias will be active
      /// </param>
      /// <param name="_antialiasFilter">
      ///   scaling filtering type
      /// </param>
      procedure ApplyAntialiasSettings( const _antialias       : Boolean ;
                                        const _antialiasFilter : TGIS_ScalingFilter
                                      ) ; overload ;

      /// <inheritdoc/>
      function  DrawEx            ( const _extent : TGIS_Extent
                                  ) : Boolean ; override;
      /// <summary>
      ///   Does nothing (for compatibility only).
      /// </summary>
      procedure DrawFlash        ; override;

      /// <summary>
      ///   Gets an altitude zone map definition.
      /// </summary>
      /// <param name="_zoneIndex">
      ///   zone index
      /// </param>
      /// <returns>
      ///   Desired altitude map zone.
      /// </returns>
      function GetAltitudeMapZone ( const _zoneIndex : Integer
                                  ) : TGIS_AltitudeZone ;

      /// <summary>
      ///   Calculates default color ramp RGB values.
      /// </summary>
      /// <returns>
      ///   calculated color
      /// </returns>
      /// <param name="_val">
      ///   grid value for which color will be calculated
      /// </param>
      function  GetColorRamp      ( const _val : Single
                                  ) : TGIS_Color ; virtual;

      /// <summary>
      ///   Generates a grid ramp.
      /// </summary>
      /// <param name="_startColor">
      ///   start color
      /// </param>
      /// <param name="_middleColor">
      ///   middle color
      /// </param>
      /// <param name="_endColor">
      ///   end color
      /// </param>
      /// <param name="_minValue">
      ///   minimum value
      /// </param>
      /// <param name="_midValue">
      ///   middle value
      /// </param>
      /// <param name="_maxValue">
      ///   maximum value
      /// </param>
      /// <param name="_useMiddle">
      ///   If True, a middle value will be used
      /// </param>
      /// <param name="_rampInterval">
      ///   ramp interval
      /// </param>
      /// <param name="_legendInterval">
      ///   legend interval
      /// </param>
      /// <param name="_params">
      ///   parameters to be altered, if nil then a layer parameters will be altered
      /// </param>
      /// <param name="_clearMapZones">
      ///   if True, zones list will be cleared before adding a ramp
      /// </param>
      /// <param name="_colorSpace">
      ///   color space for color interpolation
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BADPIXELFORMAT
      /// </exception>
      procedure GenerateRamp      ( const _startColor     : TGIS_Color ;
                                    const _middleColor    : TGIS_Color ;
                                    const _endColor       : TGIS_Color ;
                                    const _minValue       : Double ;
                                    const _midValue       : Double ;
                                    const _maxValue       : Double ;
                                    const _useMiddle      : Boolean ;
                                    const _rampInterval   : Double ;
                                    const _legendInterval : Double ;
                                    const _params         : TGIS_ParamsSectionPixel ;
                                    const _clearMapZones  : Boolean ;
                                    const _colorSpace     : TGIS_ColorInterpolationMode
                                   ) ; overload ;

      /// <summary>
      ///   Generates a grid ramp.
      /// </summary>
      /// <param name="_startColor">
      ///   start color
      /// </param>
      /// <param name="_middleColor">
      ///   middle color
      /// </param>
      /// <param name="_endColor">
      ///   end color
      /// </param>
      /// <param name="_minValue">
      ///   minimum value
      /// </param>
      /// <param name="_midValue">
      ///   middle value
      /// </param>
      /// <param name="_maxValue">
      ///   maximum value
      /// </param>
      /// <param name="_useMiddle">
      ///   If True, a middle value will be used
      /// </param>
      /// <param name="_rampInterval">
      ///   ramp interval
      /// </param>
      /// <param name="_legendInterval">
      ///   legend interval
      /// </param>
      /// <param name="_params">
      ///   parameters to be altered, if nil then a layer parameters will be altered
      /// </param>
      /// <param name="_clearMapZones">
      ///   if True, zones list will be cleared before adding a ramp
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BADPIXELFORMAT
      /// </exception>
      procedure GenerateRamp      ( const _startColor     : TGIS_Color ;
                                    const _middleColor    : TGIS_Color ;
                                    const _endColor       : TGIS_Color ;
                                    const _minValue       : Double ;
                                    const _midValue       : Double ;
                                    const _maxValue       : Double ;
                                    const _useMiddle      : Boolean ;
                                    const _rampInterval   : Double ;
                                    const _legendInterval : Double ;
                                    const _params         : TGIS_ParamsSectionPixel ;
                                    const _clearMapZones  : Boolean
                                   ) ; overload ;

      /// <summary>
      ///   Generates a grid ramp from a mapped color array.
      /// </summary>
      /// <param name="_minValue">
      ///   minimum value
      /// </param>
      /// <param name="_maxValue">
      ///   maximum value
      /// </param>
      /// <param name="_colorMap">
      ///   mapped color array
      /// </param>
      /// <param name="_params">
      ///   parameters to be altered, if nil then a layer parameters will be altered
      /// </param>
      /// <param name="_cInterpMode">
      ///   color space for color interpolation
      /// </param>
      procedure GenerateRampEx    ( const _minValue    : Double ;
                                    const _maxValue    : Double ;
                                    const _colorMap    : TGIS_ColorMapArray ;
                                    const _params      : TGIS_ParamsSectionPixel ;
                                    const _cInterpMode : TGIS_ColorInterpolationMode
                                   ) ; overload ;

      /// <summary>
      ///   Generates a grid ramp from a mapped color array.
      /// </summary>
      /// <param name="_minValue">
      ///   minimum value
      /// </param>
      /// <param name="_maxValue">
      ///   maximum value
      /// </param>
      /// <param name="_colorMap">
      ///   mapped color array
      /// </param>
      /// <param name="_params">
      ///   parameters to be altered, if nil then a layer parameters will be altered
      /// </param>
      procedure GenerateRampEx    ( const _minValue : Double ;
                                    const _maxValue : Double ;
                                    const _colorMap : TGIS_ColorMapArray ;
                                    const _params   : TGIS_ParamsSectionPixel
                                   ) ; overload ;

      /// <summary>
      ///   Maps grid value to pixel color.
      /// </summary>
      /// <param name="_params">
      ///   corresponding pixel parameters
      /// </param>
      /// <param name="_val">
      ///   value
      /// </param>
      /// <returns>
      ///   pixel color.
      /// </returns>
      function  MapGridValue      ( const _params       : TGIS_ParamsSectionPixel ;
                                    const _val          : Single
                                  ) : TGIS_Color ; virtual;

      /// <summary>
      ///   Maps color value to pixel color.
      /// </summary>
      /// <returns>
      ///   mapped color
      /// </returns>
      /// <param name="_params">
      ///   corresponding pixel parameters
      /// </param>
      /// <param name="_color">
      ///   value
      /// </param>
      function  MapColorValue     ( const _params       : TGIS_ParamsSectionPixel ;
                                    const _color        : TGIS_Color
                                  ) : TGIS_Color ; virtual;

      /// <summary>
      ///  Reads a specific line of the grid into the preallocated buffer.
      /// </summary>
      /// <returns>
      ///   number of read values
      /// </returns>
      /// <param name="_buffer">
      ///   predefined buffer of singles
      /// </param>
      /// <param name="_line">
      ///   number of grid line to be read
      /// </param>
      /// <param name="_left">
      ///   left margin (columns to skip)
      /// </param>
      /// <param name="_count">
      ///    number of columns to be read
      /// </param>
      function  ReadGridLine      ( var   _buffer       : TGIS_SingleArray ;
                                    const _line         : Integer          ;
                                    const _left         : Integer          ;
                                    const _count        : Integer
                                  ) : Integer ; virtual;


      /// <summary>
      ///   Recalculates the projected extent. Do nothing - only for safe inheritance.
      /// </summary>
      procedure RecalcProjectedExtent ; override;

      /// <summary>
      ///  Sets world parameters (equivalent to worldfile).
      /// </summary>
      /// <param name="_a">
      ///   pixel size in the x-direction in map units/pixel
      /// </param>
      /// <param name="_d">
      ///   rotation about y-axis
      /// </param>
      /// <param name="_b">
      ///   rotation about x-axis
      /// </param>
      /// <param name="_e">
      ///   pixel size in the y-direction in map units,
      ///        almost always negative
      /// </param>
      /// <param name="_c">
      ///   x-coordinate of the center of the upper left pixel
      /// </param>
      /// <param name="_f">
      ///    y-coordinate of the center of the upper left pixel
      /// </param>
      procedure SetWorld( const _a : Double ;
                          const _d : Double ;
                          const _b : Double ;
                          const _e : Double ;
                          const _c : Double ;
                          const _f : Double
                        ) ;


      /// <summary>
      ///   If a given point is located in the image area, returns true and sets
      ///   variables.
      /// </summary>
      /// <returns>
      ///   True if success
      /// </returns>
      /// <param name="_ptg">
      ///   reference point /searching point/;
      ///   if the layer has been attached to the Viewer then expected
      ///   _ptg units are in a  Viewer coordinate space;
      ///   otherwise expected _ptg units are in a Layer coordinate space
      /// </param>
      /// <param name="_nativesVals">
      ///   dynamic array of double with original values in
      ///   _ptg point (for example R, G, B),
      ///   Length(_nativesVals) is equal to number of bands
      /// </param>
      /// <param name="_rgbMapped">
      ///   color in _ptg point after mapping (the same as
      ///   _nativesVals if no mapping, in this case
      ///   _rgbMapped and $FF = Byte(_nativesVals[0] and so on).
      /// </param>
      /// <param name="_transparency">
      ///   set on True if point is transparent
      /// </param>
      /// <remarks>
      ///   same meaning as LocateEx() with _pixelsize=0
      /// </remarks>
      function Locate             ( const _ptg          : TGIS_Point       ;
                                    var   _rgbMapped    : TGIS_Color       ;
                                    var   _nativesVals  : TGIS_DoubleArray ;
                                    var   _transparency : Boolean
                                  ) : Boolean ; virtual;

      /// <summary>
      ///   If a given point is located in the image area, returns true and sets
      ///   variables.
      /// </summary>
      /// <returns>
      ///   True if success
      /// </returns>
      /// <param name="_ptg">
      ///   reference point /searching point/;
      ///   if the layer has been attached to the Viewer then expected
      ///   _ptg units are in a  Viewer coordinate space;
      ///   otherwise expected _ptg units are in a Layer coordinate space
      /// </param>
      /// <param name="_nativesVals">
      ///   dynamic array of double with original values in
      ///   _ptg point (for example R, G, B),
      ///   Length(_nativesVals) is equal to number of bands
      /// </param>
      /// <param name="_rgbMapped">
      ///   color in _ptg point after mapping (the same as
      ///   _nativesVals if no mapping, in this case
      ///   _rgbMapped and $FF = Byte(_nativesVals[0] and so on).
      /// </param>
      /// <param name="_transparency">
      ///   set on True if point is transparent
      /// </param>
      /// <param name="_pixelsize">
      ///   requested size of image pixel; by providing 0 an actual
      ///   viewer scale should be used to calculate am optimal pixel size;
      ///   &gt;0 - pixel size is expressed in map units; &lt;0 - pixels is
      ///   expressed in layer units
      /// </param>
      /// <remarks>
      ///  If _pixelsize=0 then meaning same as Locate().
      /// </remarks>
      function LocateEx           ( const _ptg          : TGIS_Point       ;
                                    var   _rgbMapped    : TGIS_Color       ;
                                    var   _nativesVals  : TGIS_DoubleArray ;
                                    var   _transparency : Boolean          ;
                                    const _pixelsize    : Double
                                  ) : Boolean ; virtual;

      /// <summary>
      ///   Fills provided _grid array with values defined by _extent. If the
      ///   current layer does not fully cover the _extent then values outside
      ///   the layer scope should be left untouched.
      /// </summary>
      /// <returns>
      ///   True if success
      /// </returns>
      /// <param name="_extent">
      ///   extent of the _grid
      /// </param>
      /// <param name="_grid">
      ///   allocated array (width/height ratio of the _grid should be the same
      ///   as width/height ration of the _extent)
      /// </param>
      /// <remarks>
      ///   Function provide data with coordinate system applied.
      /// </remarks>
      function  GetGrid           ( const _extent       : TGIS_Extent      ;
                                    const _grid         : TGIS_GridArray
                                  ) : Boolean ; virtual;

      /// <summary>
      ///   Fills provided _bitmap array with values defined by _extent. If the
      ///   current layer does not fully cover the _extent then values outside
      ///   the layer scope should be left untouched.
      /// </summary>
      /// <returns>
      ///   True if success
      /// </returns>
      /// <param name="_extent">
      ///   extent of the _bitmap
      /// </param>
      /// <param name="_bitmap">
      ///   allocated array (width/height ratio of the _bitmap should be the
      ///   same as width/height ration of the _extent)
      /// </param>
      /// <param name="_width">
      ///   width of the _bitmap
      /// </param>
      /// <param name="_height">
      ///   height of the _bitmap
      /// </param>
      /// <remarks>
      ///   Function provide data with coordinate system applied.
      /// </remarks>
      function  GetBitmap         ( const _extent   : TGIS_Extent ;
                                    const _bitmap   : TGIS_Pixels ;
                                    const _width    : Integer ;
                                    const _height    : Integer
                                  ) : Boolean ; virtual;

      /// <summary>
      ///   Fills provided _bitmap array with values defined by _extent. If the
      ///   current layer does not fully cover the _extent then values outside
      ///   the layer scope should be left untouched.
      /// </summary>
      /// <returns>
      ///   True if success
      /// </returns>
      /// <param name="_extent">
      ///   extent of the _bitmap
      /// </param>
      /// <param name="_bitmap">
      ///   allocated array (width/height ratio of the _bitmap should be the
      ///   same as width/height ration of the _extent)
      /// </param>
      /// <param name="_width">
      ///   width of the _bitmap
      /// </param>
      /// <param name="_height">
      ///   height of the _bitmap
      /// </param>
      /// <remarks>
      ///   Function provide data without coordinate system applied.
      /// </remarks>
      function  GetRawBitmap      ( const _extent   : TGIS_Extent ;
                                    const _bitmap   : TGIS_Pixels ;
                                    const _width    : Integer ;
                                    const _height    : Integer
                                  ) : Boolean ; virtual;

      /// <summary>
      ///   Imports layer from an existing pixel layer.
      ///   Use this method to import a layer. To do this, simply create a new
      ///   layer, choose a layer to import from and use ImportLayer method of with
      ///   proper parameters.
      /// </summary>
      /// <param name="_layer">
      ///   layer to be exported to
      /// </param>
      /// <param name="_extent">
      ///   extent to be exported
      /// </param>
      procedure ImportLayer       ( const _layer        : TGIS_LayerPixel  ;
                                    const _extent       : TGIS_Extent
                                  ) ; overload; virtual;

      /// <summary>
      ///   Imports layer from an existing pixel layer.
      ///   Use this method to import a layer. To do this, simply create a new
      ///   layer, choose a layer to import from and use ImportLayer method of with
      ///   proper parameters.
      /// </summary>
      /// <param name="_layer">
      ///   layer to be exported from
      /// </param>
      /// <param name="_extent">
      ///   extent to be exported
      /// </param>
      /// <param name="_cs">
      ///   coordinate system to be exported
      /// </param>
      /// <param name="_width">
      ///   bit width of a layer; if 0 then _height will be used
      ///   as to calculate
      /// </param>
      /// <param name="_height">
      ///   bit height of a layer; if 0 then _width will be used
      ///   as to calculate
      /// </param>
      procedure ImportLayer     ( const _layer     : TGIS_LayerPixel  ;
                                  const _extent    : TGIS_Extent ;
                                  const _cs        : TGIS_CSCoordinateSystem ;
                                  const _width     : Cardinal ;
                                  const _height    : Cardinal
                                ) ; overload; virtual;

      /// <summary>
      ///   Imports layer from an existing pixel layer.
      ///   Use this method to import a layer. To do this, simply create a new
      ///   layer, choose a layer to import from and use ImportLayer method of with
      ///   proper parameters.
      /// </summary>
      /// <param name="_layer">
      ///   layer to be exported from
      /// </param>
      /// <param name="_extent">
      ///   extent to be exported in _cs coordinate system
      /// </param>
      /// <param name="_cs">
      ///   coordinate system to be exported in
      /// </param>
      /// <param name="_width">
      ///   bit width of a layer; if 0 then _height will be used
      ///   as to calculate
      /// </param>
      /// <param name="_height">
      ///   bit height of a layer; if 0 then _width will be used
      ///   as to calculate
      /// </param>
      /// <param name="_subformat">
      ///   Pixel file capabilities.
      /// </param>
      procedure ImportLayer     ( const _layer     : TGIS_LayerPixel  ;
                                  const _extent    : TGIS_Extent ;
                                  const _cs        : TGIS_CSCoordinateSystem ;
                                  const _width     : Cardinal ;
                                  const _height    : Cardinal ;
                                  const _subformat : TGIS_LayerPixelSubFormat
                                ) ; overload; virtual;

      /// <summary>
      ///   Merge layer to an existing layer.
      ///   Use this method to merge a layer. To do this, simply create a new
      ///   layer, choose a layer to merge from and use MergeLayer method of with
      ///   proper parameters.
      /// </summary>
      /// <param name="_layer">
      ///   layer to be merged from
      /// </param>
      /// <param name="_extent">
      ///   extent to be merged
      /// </param>
      procedure MergeLayer      ( const _layer     : TGIS_LayerPixel  ;
                                  const _extent    : TGIS_Extent
                                ) ; virtual;

       /// <summary>
       ///   Export layer.
       /// </summary>
       /// <param name="_layer">
       ///   layer to be exported to
       /// </param>
       /// <param name="_extent">
       ///   starting extent of layer - can't be zero sized
       /// </param>
       /// <remarks>
       ///   <note type="note">
       ///    Remember to assign 'path' property to layer you want to export
       ///    to for proper export a new layer to file.
       ///    </note>
       ///   Use this method to export a layer. To do this choose a layer to
       ///   export to, and use ExportLayer method of with proper params. See
       ///   an opposite method ImportLayer.
       /// </remarks>
       procedure ExportLayer    ( const _layer       : TGIS_LayerPixel ;
                                  const _extent      : TGIS_Extent
                                ) ;



      /// <summary>
      ///   Puts _grid array into layer at the place defined by _extent. If the
      ///   current layer does not fully cover the _extent then values outside
      ///   the layer scope should be left untouched.
      /// </summary>
      /// <returns>
      ///   True if success
      /// </returns>
      /// <param name="_extent">
      ///   extent of the _grid
      /// </param>
      /// <param name="_grid">
      ///   array (with/height ratio of the _grid should be the same as
      ///   width/height ration of the _extent)
      /// </param>
      function  PutGrid           ( const _extent       : TGIS_Extent      ;
                                    const _grid         : TGIS_GridArray
                                  ) : Boolean ; virtual;

      /// <summary>
      ///   Scales grid array using a linear filter by default.
      /// </summary>
      /// <param name="_srcGrid">
      ///   source grid array
      /// </param>
      /// <param name="_dstGrid">
      ///   destination grid array
      /// </param>
      /// <param name="_lstart">
      ///   for zoom in - omitted left part
      /// </param>
      /// <param name="_tstart">
      ///   for zoom in - omitted top part
      /// </param>
      procedure ScaleGrid         ( const _srcGrid : TGIS_GridArray ;
                                    const _dstGrid : TGIS_GridArray ;
                                    const _lstart  : Integer ;
                                    const _tstart  : Integer
                                  ) ; overload;



      /// <summary>
      ///   Scales grid array using defined filtering.
      /// </summary>
      /// <param name="_srcGrid">
      ///   source grid array
      /// </param>
      /// <param name="_dstGrid">
      ///   destination grid array
      /// </param>
      /// <param name="_lstart">
      ///   for zoom in - omitted left part
      /// </param>
      /// <param name="_tstart">
      ///   for zoom in - omitted top part
      /// </param>
      /// <param name="_filtering">
      ///   scaling resampling (filtering) method
      /// </param>
      procedure ScaleGrid         ( const _srcGrid : TGIS_GridArray ;
                                    const _dstGrid : TGIS_GridArray ;
                                    const _lstart  : Integer ;
                                    const _tstart  : Integer ;
                                    const _filtering : TGIS_ScalingFilter
                                  ) ; overload;

      /// <summary>
      ///   Sets internal file scales - (1.0, 1.0) is always possible
      /// </summary>
      /// <param name="_dwidth">
      ///   source width
      /// </param>
      /// <param name="_swidth">
      ///   destination width
      /// </param>
      procedure SetCurrentFileScale( const _dwidth : Double ;
                                     const _swidth : Double
                                    ) ;

      /// <summary>
      ///   Builds an image layer.
      /// </summary>
      /// <param name="_path">
      ///   path to a layer; if empty then layer in-memory
      /// </param>
      /// <param name="_cs">
      ///   Coordinate system of the layer
      /// </param>
      /// <param name="_ext">
      ///   extent of the layer expresses in Coordinate System units (_cs)
      /// </param>
      /// <param name="_width">
      ///   width in pixels
      /// </param>
      /// <param name="_height">
      ///   height in pixels; if 0 then is calculated according to _width and
      ///   proportions of _ext
      /// </param>
      /// <remarks>
      ///   To create a grid layer see method overloads.
      /// </remarks>
      procedure Build   ( const _path   : String ;
                          const _cs     : TGIS_CSCoordinateSystem ;
                          const _ext    : TGIS_Extent ;
                          const _width  : Integer ;
                          const _height : Integer
                        ) ; overload; virtual;

      /// <summary>
      ///   Builds a layer.
      /// </summary>
      /// <param name="_path">
      ///   path to a layer; if empty then layer in-memory
      /// </param>
      /// <param name="_grid">
      ///   if true then layer will be treated as grid a image layer will be
      ///   created otherwise
      /// </param>
      /// <param name="_cs">
      ///   Coordinate system of the layer
      /// </param>
      /// <param name="_ext">
      ///   extent of the layer expresses in Coordinate System units (_cs)
      /// </param>
      /// <param name="_width">
      ///   width in pixels
      /// </param>
      /// <param name="_height">
      ///   height in pixels; if 0 then is calculated according to _width and
      ///   proportions of _ext
      /// </param>
      /// <param name="_subformat">
      ///   details specification of layer format like compression etc.
      /// </param>
      procedure Build   ( const _path   : String ;
                          const _grid   : Boolean ;
                          const _cs     : TGIS_CSCoordinateSystem ;
                          const _ext    : TGIS_Extent ;
                          const _width  : Integer ;
                          const _height : Integer ;
                          const _subformat : TGIS_LayerPixelSubFormat
                        ) ; overload; virtual;

      /// <summary>
      ///   Builds an in-memory layer.
      /// </summary>
      /// <param name="_grid">
      ///   if true then layer will be treated as grid a image layer will be
      ///   created otherwise
      /// </param>
      /// <param name="_cs">
      ///   Coordinate system of the layer
      /// </param>
      /// <param name="_ext">
      ///   extent of the layer expresses in Coordinate System units (_cs)
      /// </param>
      /// <param name="_width">
      ///   width in pixels
      /// </param>
      /// <param name="_height">
      ///   height in pixels; if 0 then is calculated according to _width and
      ///   proportions of _ext
      /// </param>
      /// <remarks>
      ///   To create file based layer - see method overloads.
      /// </remarks>
      procedure Build   ( const _grid   : Boolean ;
                          const _cs     : TGIS_CSCoordinateSystem ;
                          const _ext    : TGIS_Extent ;
                          const _width  : Integer ;
                          const _height : Integer
                        ) ; overload; virtual;

      /// <summary>
      ///   Builds a layer.
      /// </summary>
      /// <param name="_path">
      ///   path to a layer; if empty then layer in-memory
      /// </param>
      /// <param name="_grid">
      ///   if true then layer will be treated as grid a image layer will be
      ///   created otherwise
      /// </param>
      /// <param name="_cs">
      ///   Coordinate system of the layer
      /// </param>
      /// <param name="_ext">
      ///   extent of the layer expresses in Coordinate System units (_cs)
      /// </param>
      /// <param name="_pixelsize">
      ///   pixel layer resolution in Coordinate System units
      /// </param>
      /// <param name="_subformat">
      ///   details specification of layer format like compression etc.
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   throws if _pixelsize &lt;= 0
      /// </exception>
      /// <remarks>
      ///   Layer's extent might be little expanded to ensure exact _pixelsize.
      /// </remarks>
      procedure Build   ( const _path      : String ;
                          const _grid      : Boolean ;
                          const _cs        : TGIS_CSCoordinateSystem ;
                          const _ext       : TGIS_Extent ;
                          const _pixelsize : Double ;
                          const _subformat : TGIS_LayerPixelSubFormat
                        ) ; overload; virtual;

      /// <summary>
      ///   Builds an image layer.
      /// </summary>
      /// <param name="_path">
      ///   path to a layer; if empty then layer in-memory
      /// </param>
      /// <param name="_cs">
      ///   Coordinate system of the layer
      /// </param>
      /// <param name="_ext">
      ///   extent of the layer expresses in Coordinate System units (_cs)
      /// </param>
      /// <param name="_pixelsize">
      ///   pixel layer resolution in Coordinate System units
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   throws if _pixelsize &lt;= 0
      /// </exception>
      /// <remarks>
      ///   Layer's extent might be little expanded to ensure exact _pixelsize.
      ///   To create a grid layer see method overloads.
      /// </remarks>
      procedure Build   ( const _path   : String ;
                          const _cs     : TGIS_CSCoordinateSystem ;
                          const _ext    : TGIS_Extent ;
                          const _pixelsize : Double
                        ) ; overload; virtual;

      /// <summary>
      ///   Builds an in-memory layer.
      /// </summary>
      /// <param name="_grid">
      ///   if true then layer will be treated as grid a image layer will be
      ///   created otherwise
      /// </param>
      /// <param name="_cs">
      ///   Coordinate system of the layer
      /// </param>
      /// <param name="_ext">
      ///   extent of the layer expresses in Coordinate System units (_cs)
      /// </param>
      /// <param name="_pixelsize">
      ///   pixel layer resolution in Coordinate System units
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   throws if _pixelsize &lt;= 0
      /// </exception>
      /// <remarks>
      ///   Layer's extent might be little expanded to ensure exact _pixelsize.
      ///   To create file based layer - see method overloads.
      /// </remarks>
      procedure Build   ( const _grid   : Boolean ;
                          const _cs     : TGIS_CSCoordinateSystem ;
                          const _ext    : TGIS_Extent ;
                          const _pixelsize : Double
                        ) ; overload; virtual;

      /// <summary>
      ///   Estimates memory size for pixel layer to be update.
      /// </summary>
      /// <returns>
      ///   estimated size
      /// </returns>
      /// <param name="_ext">
      ///   area to be locked; if bigger then layer extent then will be
      ///   truncated to layer extent
      /// </param>
      /// <param name="_cs">
      ///   coordinate system of _cs; if nil then same as layer CS
      /// </param>
      /// <param name="_pixelsize">
      ///   minimum requested size of image pixel; by providing 0 a maximum
      ///   possible resolution will be used;
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    this particular implementation is valid only for in-memory layers
      ///    </note>
      /// </remarks>
      function LockEstimate   ( const _ext       : TGIS_Extent ;
                                const _cs        : TGIS_CSCoordinateSystem ;
                                const _pixelsize : Double
                              ) : Integer ;


      /// <summary>
      ///   Locks a fragment of the pixel layer for updating.
      /// </summary>
      /// <returns>
      ///   locked area
      /// </returns>
      /// <param name="_ext">
      ///   area to be locked; if bigger then layer extent then will be
      ///   truncated to layer extent
      /// </param>
      /// <param name="_cs">
      ///   coordinate system of _cs; if nil then same as layer CS
      /// </param>
      /// <param name="_writable">
      ///   if true than layer will post back any changes in locked area;
      ///   if write is not possible then TGIS_LayerPixelLock.Writable will be
      ///   set accordingly
      /// </param>
      /// <remarks>
      ///   Must be paired with UnlockPixels.
      /// </remarks>
      function    LockPixels       ( const _ext       : TGIS_Extent ;
                                     const _cs        : TGIS_CSCoordinateSystem ;
                                     const _writable  : Boolean
                                   ) : TGIS_LayerPixelLock ; overload; virtual;


      /// <summary>
      ///   Locks a fragment of the pixel layer for updating.
      /// </summary>
      /// <param name="_ext">
      ///   area to be locked; if bigger then layer extent then will be
      ///   truncated to layer extent
      /// </param>
      /// <param name="_cs">
      ///   coordinate system of _cs; if nil then same as layer CS
      /// </param>
      /// <param name="_pixelsize">
      ///   minimum requested size of image pixel; by providing 0 a maximum
      ///   possible resolution will be used, which is also a fastest method
      ///   if a layer is full in-memory because in such situation a pointer to
      ///   existing memory will be used
      /// </param>
      /// <param name="_writable">
      ///   if true than layer will post back any changes in locked area; if
      ///   write is not possible then TGIS_LayerPixelLock.Writable will be set
      ///   accordingly
      /// </param>
      /// <returns>
      ///   locked area
      /// </returns>
      /// <remarks>
      ///   Must be paired with UnlockPixels.
      /// </remarks>
      function    LockPixels       ( const _ext       : TGIS_Extent ;
                                     const _cs        : TGIS_CSCoordinateSystem ;
                                     const _pixelsize : Double ;
                                     const _writable  : Boolean
                                   ) : TGIS_LayerPixelLock ; overload; virtual;

      /// <summary>
      ///   Locks a fragment of the pixel layer for updating.
      /// </summary>
      /// <returns>
      ///   locked area
      /// </returns>
      /// <param name="_rct">
      ///   rectangle on raster to be locked; if bigger then raster size then
      ///   will be truncated
      /// </param>
      /// <param name="_writable">
      ///   if true than layer will post back any changes in locked area;
      ///   if write is not possible then TGIS_LayerPixelLock.Writable will be
      ///   set accordingly
      /// </param>
      /// <remarks>
      ///   Must be paired with UnlockPixels.
      /// </remarks>
      function    LockPixels       ( const _rct       : TRect ;
                                     const _writable  : Boolean
                                   ) : TGIS_LayerPixelLock ; overload; virtual;

      /// <summary>
      ///   Locks a fragment of the pixel layer for updating.
      /// </summary>
      /// <param name="_ext">
      ///   area to be locked; if bigger then layer extent then will be
      ///   truncated to layer extent
      /// </param>
      /// <param name="_cs">
      ///   coordinate system of _cs; if nil then same as layer CS
      /// </param>
      /// <param name="_pixelsize">
      ///   minimum requested size of image pixel; by providing 0 a maximum
      ///   possible resolution will be used, which is also a fastest method if
      ///   a layer is full in-memory because in such situation a pointer to
      ///   existing memory will be used
      /// </param>
      /// <param name="_band">
      ///   specific band to be locked
      /// </param>
      /// <param name="_writable">
      ///   if true than layer will post back any changes in locked area; if
      ///   write is not possible then TGIS_LayerPixelLock.Writable will be set
      ///   accordingly
      /// </param>
      /// <returns>
      ///   locked area
      /// </returns>
      /// <remarks>
      ///   Must be paired with UnlockPixels.
      /// </remarks>
      function    LockPixels       ( const _ext       : TGIS_Extent ;
                                     const _cs        : TGIS_CSCoordinateSystem ;
                                     const _pixelsize : Double ;
                                     const _band      : Integer ;
                                     const _writable  : Boolean
                                   ) : TGIS_LayerPixelLock ; overload; virtual;


      /// <summary>
      ///   Locks a fragment of the pixel layer for updating.
      /// </summary>
      /// <returns>
      ///   locked area
      /// </returns>
      /// <param name="_ext">
      ///   area to be locked; if bigger then layer extent then will be
      ///   truncated to layer extent
      /// </param>
      /// <param name="_cs">
      ///   coordinate system of _cs; if nil then same as layer CS
      /// </param>
      /// <param name="_band">
      ///   specific band to be locked
      /// </param>
      /// <param name="_writable">
      ///   if true than layer will post back any changes in locked area;
      ///   if write is not possible then TGIS_LayerPixelLock.Writable will be
      ///   set accordingly
      /// </param>
      /// <remarks>
      ///   Must be paired with UnlockPixels.
      /// </remarks>
      function    LockPixels       ( const _ext       : TGIS_Extent ;
                                     const _cs        : TGIS_CSCoordinateSystem;
                                     const _band      : Integer ;
                                     const _writable  : Boolean
                                   ) : TGIS_LayerPixelLock ; overload; virtual;

      /// <summary>
      ///   Locks a fragment of the pixel layer for updating.
      /// </summary>
      /// <returns>
      ///   locked area
      /// </returns>
      /// <param name="_rct">
      ///   rectangle on raster to be locked; if bigger then raster size then
      ///   will be truncated
      /// </param>
      /// <param name="_band">
      ///   specific band to be locked
      /// </param>
      /// <param name="_writable">
      ///   if true than layer will post back any changes in locked area;
      ///   if write is not possible then TGIS_LayerPixelLock.Writable will be
      ///   set accordingly
      /// </param>
      /// <remarks>
      ///   Must be paired with UnlockPixels.
      /// </remarks>
      function    LockPixels       ( const _rct       : TRect ;
                                     const _band      : Integer ;
                                     const _writable  : Boolean
                                   ) : TGIS_LayerPixelLock ; overload; virtual;

      /// <summary>
      ///   Locks a fragment of the pixel layer for updating.
      /// </summary>
      /// <returns>
      ///   locked area
      /// </returns>
      /// <param name="_rct">
      ///   rectangle on raster to be locked; if bigger then raster size then
      ///   will be truncated
      /// </param>
      /// <param name="_ext">
      ///   area to be locked; if bigger then layer extent then will be
      ///   truncated to layer extent
      /// </param>
      /// <param name="_tilelevel">
      ///   tile level
      /// </param>
      /// <param name="_tilerow">
      ///   tile row
      /// </param>
      /// <param name="_tilecolumn">
      ///   tile column
      /// </param>
      /// <param name="_writable">
      ///   if true than layer will post back any changes in locked area;
      ///   if write is not possible then TGIS_LayerPixelLock.Writable will be
      ///   set accordingly
      /// </param>
      /// <remarks>
      ///   Must be paired with UnlockPixels.
      /// </remarks>
      function    LockPixels       ( const _rct        : TRect ;
                                     const _ext        : TGIS_Extent ;
                                     const _tilelevel  : Integer ;
                                     const _tilerow    : Integer ;
                                     const _tilecolumn : Integer ;
                                     const _writable   : Boolean
                                   ) : TGIS_LayerPixelLock ; overload; virtual;

      /// <summary>
      ///   UnLocks a fragment. Populate changes to the layer.
      /// </summary>
      /// <param name="_lock">
      ///   lock obtained upon LockPixels() call
      /// </param>                         s
      {#ownership:_lock:release}
      procedure   UnlockPixels     ( {$IFDEF GIS_PDK}
                                       const
                                     {$ELSE}
                                       var
                                     {$ENDIF}
                                           _lock       : TGIS_LayerPixelLock
                                   ) ; virtual;

      /// <summary>
      ///   Prepares enumerator to iterate the layer.
      /// </summary>
      /// <param name="_extent">
      ///   extent of items to be found; expected _extent units are in the Layer
      ///   coordinate space
      /// </param>
      /// <param name="_pixelsize">
      ///   minimum requested size of image pixel; by providing 0, the maximum
      ///   possible resolution will be used, which is also the fastest method if
      ///   a layer is fully in-memory because, in such situation, a pointer to
      ///   existing memory is used
      /// </param>
      /// <param name="_fatpixel">
      ///   if false then pixel will be treated as point; if true then pixels
      ///   will be treated as small polygon, which is much slower but provides
      ///   some more advanced DE-9IM testing possibilities
      /// </param>
      /// <param name="_shape">
      ///   if not nil, then only pixels matching _de9im matrix with be found;
      /// </param>
      /// <param name="_de9im">
      ///   DE-9IM matrix of comparison
      /// </param>
      /// <param name="_writable">
      ///   if true than layer will post back any changes in locked area; if
      ///   write is not possible then TGIS_PixelItem.Writable will be set
      ///   accordingly
      /// </param>
      /// <returns>
      ///   Pixel item returned row by row, column by column.
      /// </returns>
      function    &Loop            ( const _extent      : TGIS_Extent ;
                                     const _pixelsize   : Double      ;
                                     const _fatpixel    : Boolean     ;
                                     const _shape       : TGIS_Shape  ;
                                     const _de9im       : String      ;
                                     const _writable    : Boolean
                                   ) : TGIS_LayerPixelEnumeratorFactory ;
                                   overload; virtual;

      /// <summary>
      ///   Prepares enumerator to iterate the layer.
      /// </summary>
      /// <param name="_extent">
      ///   extent of items to be found; expected _extent units are in a Layer
      ///   coordinate space
      /// </param>
      /// <param name="_pixelsize">
      ///   minimum requested size of image pixel; by providing 0 a maximum
      ///   possible resolution will be used, which is also a fastest method if a
      ///   layer is full in-memory because in such situation a pointer to
      ///   existing memory will be used
      /// </param>
      /// <param name="_fatpixel">
      ///   if false then pixel will be treated as point; if true then pixels will
      ///   be treated as small polygon, which is much slower but provides some
      ///   more advanced DE-9IM testing possibilities
      /// </param>
      /// <param name="_shape">
      ///   if not nil, then only pixels matching _de9im matrix with be found;
      /// </param>
      /// <param name="_de9im">
      ///   DE-9IM matrix of comparison
      /// </param>
      /// <param name="_band">
      ///   band specification
      ///   <list type="number">
      ///     <item>
      ///       0 - if default Loop must be used; in such bands are
      ///       interpreted as image or grid depends or layer; Color or Value
      ///       are filled accordingly
      ///     </item>
      ///     <item>
      ///       number of specific band; in such situation only Bands property
      ///       is filled
      ///     </item>
      ///     <item>
      ///       -1 - all bands are retrieved; only Bands property is filled
      ///     </item>
      ///   </list>
      /// </param>
      /// <param name="_writable">
      ///   if true than layer will post back any changes in locked area; if write
      ///   is not possible then TGIS_PixelItem.Writable will be set accordingly
      /// </param>
      /// <returns>
      ///   Pixel item returned row by row, column by column.
      /// </returns>
      function    &Loop            ( const _extent      : TGIS_Extent ;
                                     const _pixelsize   : Double      ;
                                     const _fatpixel    : Boolean     ;
                                     const _shape       : TGIS_Shape  ;
                                     const _de9im       : String      ;
                                     const _band        : Integer     ;
                                     const _writable    : Boolean
                                   ) : TGIS_LayerPixelEnumeratorFactory ;
                                   overload; virtual;

      /// <summary>
      ///   Prepares enumerator to iterate the layer (pixels are treated as
      ///   points).
      /// </summary>
      /// <param name="_extent">
      ///   extent of items to be found; expected _extent units are in the Layer
      ///   coordinate space
      /// </param>
      /// <param name="_pixelsize">
      ///   minimum requested size of image pixel; by providing 0, the maximum
      ///   possible resolution will be used, which is also the fastest method if
      ///   a layer is fully in-memory because, in such situation, a pointer to
      ///   existing memory is used
      /// </param>
      /// <param name="_shape">
      ///   if not nil, then only pixels matching _de9im matrix with be found;
      /// </param>
      /// <param name="_de9im">
      ///   DE-9IM matrix of comparison
      /// </param>
      /// <param name="_writable">
      ///   if true than layer will post back any changes in locked area; if
      ///   write is not possible then TGIS_PixelItem.Writable will be set
      ///   accordingly
      /// </param>
      /// <returns>
      ///   Pixel item returned row by row, column by column.
      /// </returns>
      function    &Loop            ( const _extent      : TGIS_Extent ;
                                     const _pixelsize   : Double      ;
                                     const _shape       : TGIS_Shape  ;
                                     const _de9im       : String      ;
                                     const _writable    : Boolean
                                   ) : TGIS_LayerPixelEnumeratorFactory ;
                                   overload; virtual;

      /// <summary>
      ///   Prepares enumerator to iterate the layer (pixels are treated as
      ///   points).
      /// </summary>
      /// <param name="_extent">
      ///   extent of items to be found; expected _extent units are in a Layer
      ///   coordinate space
      /// </param>
      /// <param name="_pixelsize">
      ///   minimum requested size of image pixel; by providing 0 a maximum
      ///   possible resolution will be used, which is also a fastest method if
      ///   a layer is full in-memory because in such situation a pointer to
      ///   existing memory will be used
      /// </param>
      /// <param name="_shape">
      ///   if not nil, then only pixels matching _de9im matrix with be found;
      /// </param>
      /// <param name="_de9im">
      ///   DE-9IM matrix of comparison
      /// </param>
      /// <param name="_band">
      ///   band specification
      ///   <list type="number">
      ///     <item>
      ///       0 - if default Loop must be used; in such bands are
      ///       interpreted as image or grid depends or layer; Color or Value
      ///       are filled accordingly
      ///     </item>
      ///     <item>
      ///       number of specific band; in such situation only Bands property
      ///       is filled
      ///     </item>
      ///     <item>
      ///       -1 - all bands are retrieved; only Bands property is filled
      ///     </item>
      ///   </list>
      /// </param>
      /// <param name="_writable">
      ///   if true than layer will post back any changes in locked area; if
      ///   write is not possible then TGIS_PixelItem.Writable will be set
      ///   accordingly
      /// </param>
      /// <returns>
      ///   Pixel item returned row by row, column by column.
      /// </returns>
      function    &Loop            ( const _extent      : TGIS_Extent ;
                                     const _pixelsize   : Double      ;
                                     const _shape       : TGIS_Shape  ;
                                     const _de9im       : String      ;
                                     const _band        : Integer     ;
                                     const _writable    : Boolean
                                   ) : TGIS_LayerPixelEnumeratorFactory ;
                                   overload; virtual;

      /// <summary>
      ///   Prepares enumerator to iterate the layer through the linear shape.
      ///   Lines are interpreted as-line-flow for terrain profile like analysis.
      ///   Pixels are treated as points.
      /// </summary>
      /// <param name="_pixelsize">
      ///   minimum requested size of image pixel; by providing 0 a maximum
      ///   possible resolution will be used, which is also a fastest method if
      ///   a layer is full in-memory because in such situation a pointer to
      ///   existing memory will be used
      /// </param>
      /// <param name="_shape">
      ///   if not nil, then only pixels within the shape will be found
      /// </param>
      /// <param name="_writable">
      ///   if true than layer will post back any changes in locked area; if
      ///   write is not possible then TGIS_PixelItem.Writable will be set
      ///   accordingly
      /// </param>
      /// <returns>
      ///   Pixel item returned row by row, column by column with the except:
      ///   <list type="bullet">
      ///     <item>
      ///       if _shape is liner then pixel are returned as-line-flow
      ///       order.
      ///     </item>
      ///     <item>
      ///       if _shape is point or multi-point then pixels are returned
      ///       in a point-by-point order
      ///     </item>
      ///   </list>
      /// </returns>
      /// <remarks>
      ///   <note type="important">
      ///     This overload has a very special meaning for linear shapes! <br />
      ///   </note>
      /// </remarks>
      function    &Loop            ( const _pixelsize   : Double      ;
                                     const _shape       : TGIS_Shape  ;
                                     const _writable    : Boolean
                                   ) : TGIS_LayerPixelEnumeratorFactory ;
                                   overload; virtual;

      /// <summary>
      ///   Prepares enumerator to iterate the layer through the linear shape.
      ///   Lines are interpreted as-line-flow for terrain profile like analysis.
      ///   Pixels are treated as points.
      /// </summary>
      /// <param name="_pixelsize">
      ///   minimum requested size of image pixel; by providing 0, the maximum
      ///   possible resolution will be used, which is also the fastest method if
      ///   a layer is fully in-memory because, in such situation, a pointer to
      ///   existing memory is used
      /// </param>
      /// <param name="_shape">
      ///   if not nil, then only pixels within the shape will be found
      /// </param>
      /// <param name="_band">
      ///   band specification
      ///   <list type="number">
      ///     <item>
      ///       0 - if default Loop must be used; in such bands are
      ///       interpreted as image or grid depends or layer; Color or Value
      ///       are filled accordingly
      ///     </item>
      ///     <item>
      ///       number of specific band; in such situation only Bands property
      ///       is filled
      ///     </item>
      ///     <item>
      ///       -1 - all bands are retrieved; only Bands property is filled
      ///     </item>
      ///   </list>
      /// </param>
      /// <param name="_writable">
      ///   if true than layer will post back any changes in locked area; if
      ///   write is not possible then TGIS_PixelItem.Writable will be set
      ///   accordingly
      /// </param>
      /// <returns>
      ///   Pixel item returned row by row, column by column with the except:
      ///   <list type="bullet">
      ///     <item>
      ///       if _shape is liner then pixel are returned as-line-flow
      ///       order.
      ///     </item>
      ///     <item>
      ///       if _shape is point or multi-point then pixels are returned
      ///       in a point-by-point order
      ///     </item>
      ///   </list>
      /// </returns>
      /// <remarks>
      ///   <note type="important">
      ///     This overload has a very special meaning for linear shapes! <br />
      ///   </note>
      /// </remarks>
      function    &Loop            ( const _pixelsize   : Double      ;
                                     const _shape       : TGIS_Shape  ;
                                     const _band        : Integer     ;
                                     const _writable    : Boolean
                                   ) : TGIS_LayerPixelEnumeratorFactory ;
                                   overload; virtual;

      /// <summary>
      ///   Prepares enumerator to iterate the layer (pixels are treated as
      ///   points).
      /// </summary>
      /// <param name="_extent">
      ///   extent of items to be found; expected _extent units are in the Layer
      ///   coordinate space
      /// </param>
      /// <param name="_pixelsize">
      ///   minimum requested size of image pixel; by providing 0, the maximum
      ///   possible resolution will be used, which is also the fastest method if
      ///   a layer is fully in-memory because, in such situation, a pointer to
      ///   existing memory is used
      /// </param>
      /// <param name="_writable">
      ///   if true than layer will post back any changes in locked area; if
      ///   write is not possible then TGIS_PixelItem.Writable will be set
      ///   accordingly
      /// </param>
      /// <returns>
      ///   Pixel item returned row by row, column by column.
      /// </returns>
      function    &Loop            ( const _extent      : TGIS_Extent ;
                                     const _pixelsize   : Double      ;
                                     const _writable    : Boolean
                                   ) : TGIS_LayerPixelEnumeratorFactory ;
                                   overload; virtual;

      /// <summary>
      ///   Prepares enumerator to iterate the layer (pixels are treated as
      ///   points).
      /// </summary>
      /// <param name="_extent">
      ///   extent of items to be found; expected _extent units are in a Layer
      ///   coordinate space
      /// </param>
      /// <param name="_pixelsize">
      ///   minimum requested size of image pixel; by providing 0 a maximum
      ///   possible resolution will be used, which is also a fastest method if
      ///   a layer is full in-memory because in such situation a pointer to
      ///   existing memory will be used
      /// </param>
      /// <param name="_band">
      ///   band specification
      ///   <list type="number">
      ///     <item>
      ///       0 - if default Loop must be used; in such bands are
      ///       interpreted as image or grid depends or layer; Color or Value
      ///       are filled accordingly
      ///     </item>
      ///     <item>
      ///       number of specific band; in such situation only Bands property
      ///       is filled
      ///     </item>
      ///     <item>
      ///       -1 - all bands are retrieved; only Bands property is filled
      ///     </item>
      ///   </list>
      /// </param>
      /// <param name="_writable">
      ///   if true than layer will post back any changes in locked area; if
      ///   write is not possible then TGIS_PixelItem.Writable will be set
      ///   accordingly
      /// </param>
      /// <returns>
      ///   Pixel item returned row by row, column by column.
      /// </returns>
      function    &Loop            ( const _extent      : TGIS_Extent ;
                                     const _pixelsize   : Double      ;
                                     const _band        : Integer     ;
                                     const _writable    : Boolean
                                   ) : TGIS_LayerPixelEnumeratorFactory ;
                                   overload; virtual;

      /// <summary>
      ///   Recalculates layer MinZ and MaxZ values.
      /// </summary>
      procedure   Recalculate      () ;

      /// <summary>
      ///   Converts map coordinates to raster pixel location.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be converted
      /// </param>
      /// <param name="_cs">
      ///   coordinate system of _ptg; if nil that coordinate system same as
      ///   layer coordinate system
      /// </param>
      /// <returns>
      ///   calculated location
      /// </returns>
      function    MapToRaster      ( const _ptg    : TGIS_Point  ;
                                     const _cs     : TGIS_CSCoordinateSystem
                                   ) : TPoint ;

      /// <summary>
      ///   Converts map extent to raster rectangle.
      /// </summary>
      /// <param name="_ext">
      ///   extent to be converted
      /// </param>
      /// <param name="_cs">
      ///   coordinate system of _ext; if nil that coordinate system same as
      ///   layer coordinate system
      /// </param>
      /// <returns>
      ///   calculated rectangle
      /// </returns>
      function    MapToRasterRect  ( const _ext    : TGIS_Extent ;
                                     const _cs     : TGIS_CSCoordinateSystem
                                   ) : TRect ;

      /// <summary>
      ///   Converts raster pixel location to map coordinates
      /// </summary>
      /// <param name="_pt">
      ///   location to be converted
      /// </param>
      /// <param name="_cs">
      ///   coordinate system of returned value; if nil that coordinate system
      ///   same as layer coordinate system
      /// </param>
      /// <returns>
      ///   calculated location
      /// </returns>
      function    RasterToMap      ( const _pt     : TPoint  ;
                                     const _cs     : TGIS_CSCoordinateSystem
                                   ) : TGIS_Point ;

      /// <summary>
      ///   Convert raster rectangle to map extent
      /// </summary>
      /// <param name="_rct">
      ///   rectangle to be converted
      /// </param>
      /// <param name="_cs">
      ///   coordinate system of returned value; if nil that coordinate system
      ///   same as layer coordinate system
      /// </param>
      /// <returns>
      ///   map extent.
      /// </returns>
      function    RasterToMapRect  ( const _rct    : TRect ;
                                     const _cs     : TGIS_CSCoordinateSystem
                                   ) : TGIS_Extent ;

      /// <summary>
      ///   Save the entire layer to the file.
      /// </summary>
      /// <remarks>
      ///   <para>
      ///      See also TGIS_Layer.SaveAll.
      ///   </para>
      /// </remarks>
      procedure SaveData   ; override;

      /// <inheritdoc/>
      function  MustSave           : Boolean ; override;


      /// <inheritdoc/>
      function IsPixel : Boolean ; override;

      /// <inheritdoc/>
      function IsGrid : Boolean ; override;

      /// <inheritdoc/>
      procedure ApplyCuttingPolygon( const _wkt : String
                                    ) ; override;

      /// <inheritdoc/>
      procedure RevertAll        ; override;

      /// <summary>
      ///   Performs any operation required to safely initialize write operation,
      ///   such as batch operations for TGIS_FilePixelStore.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Should be called before writing the whole image.
      ///    </note>
      /// </remarks>
      procedure   InitializeWrite      ; virtual;

      /// <summary>
      ///   Performs any operation required to safely finalize write operation,
      ///   such as building quad trees for TGIS_FilePixelStore.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Should be called after writing the whole image.
      ///    </note>
      /// </remarks>
      procedure   FinalizeWrite      ; virtual;

    public  // properties

     /// <summary>
     ///   Path to a TAB referencing file. If not exist then should be empty.
     /// </summary>
     property PathTAB : String read FPathTAB write FPathTAB ;

     /// <summary>
     ///   Number of altitude map zones.
     /// </summary>
     /// <remarks>
     ///   Property returns the value of Params.Pixel.AltitudeMapZones.Count.
     /// </remarks>
     property AltitudeMapZonesCount : Integer read fget_AltitudeMapZonesCount ;

     /// <summary>
     ///   Width of the layer in pixels.
     /// </summary>
     property BitWidth : Integer read FBitWidth ;

     /// <summary>
     ///   Height of the layer in pixels.
     /// </summary>
     property BitHeight : Integer read FBitHeight ;

     /// <summary>
     ///   Width of a cell in pixels. It is the same as BitWidth for BMP,
     ///   TIFF etc. It is different than BitWidth for PixelStore and any
     ///   other tiled formats.
     /// </summary>
     property CellWidth : Integer read FCellWidth ;

     /// <summary>
     ///   Height of a cell in pixels. It is the same as BitHeight for BMP,
     ///   TIFF etc. It is different than BitHeight for PixelStore and any
     ///   other tiled formats.
     /// </summary>
     property CellHeight : Integer read FCellHeight ;

     /// <summary>
     ///   Minimum elevation value of the grid.
     /// </summary>
     property MinHeight : Single read  fget_MinHeight
                                 write fset_MinHeight ;

     /// <summary>
     ///   Maximum elevation value of the grid.
     /// </summary>
     property MaxHeight : Single read  fget_MaxHeight
                                 write fset_MaxHeight ;

     /// <summary>
     ///   Minimum threshold of the grid elevation values to display.
     /// </summary>
     property MinHeightThreshold : Single read  FMinThresholdZ ;

     /// <summary>
     ///   Maximum threshold of the grid elevation values to display.
     /// </summary>
     property MaxHeightThreshold : Single read  FMaxThresholdZ ;

     /// <summary>
     ///   Modify draw extent calcualtion t be perforemd on mid pixel rather then
     ///   on border.
     /// </summary>
     /// <remarks>
     ///    Requires on some WMS servers.
     /// </remarks>
     property ExtentPixelAdjustment : Boolean read  FExtentPixelAdjustment
                                              write FExtentPixelAdjustment;

     /// <summary>
     ///   Number of bands in a file.
     /// </summary>
     property BandsCount : Integer read FBandsCount ;

     /// <summary>
     /// Grid band number. Allows selection which band will be used as for
     /// grid values channel. Bands are numbered starting from 1.
     /// </summary>
     property GridBand : Integer read FGridBand  ;

     /// <summary>
     ///   True if image is a grid.
     /// </summary>
     property IsGridImage : Boolean read FIsGridImage write fset_GridImage ;

     /// <summary>
     ///   The way the layer is interpreted: the default is as a grid, or as an image.
     /// </summary>
     property Interpretation : TGIS_LayerPixelInterpretation
                               read  FInterpretation
                               write fset_Interpretation ;

     /// <summary>
     ///   True if image is a native grid.
     /// </summary>
     property IsNativeGridImage : Boolean read FIsNativeGridImage ;

     /// <summary>
     /// True if layer is tiled.
     /// </summary>
     property IsTiled : Boolean read FIsTiled ;

     /// <summary>
     ///   Grid NoData value. Allows to specify the value which will be used
     ///   to discover no-assigned-data areas on a grid. By providing 32768
     ///   default (embed into the file) value be used.
     /// </summary>
     /// <remarks>
     ///   NoData in rasters are pixels that represent the absence of data.
     /// </remarks>
     property NoDataValue : Single read FNoDataValue  ;

     /// <summary>
     ///   Cutting polygon that defines an input image's valid area.
     /// </summary>
     /// <remarks>
     ///   <note type="important">
     ///     <para>
     ///       Attached polygon must be a copy of a existing polygon or
     ///       newly created object (to be managed by TGIS_LayerPixel).
     ///     </para>
     ///     <para>
     ///       Polygon must be in the same Coordinate System as the
     ///       layer.
     ///     </para>
     ///   </note>
     /// </remarks>
     property CuttingPolygon : TGIS_ShapePolygon read  FCuttingPolygon
                                                 write fset_CuttingPolygon ;

     /// <summary>
     ///   Antialias scaling (for some layers only like PNG and JPG).
     /// </summary>
     property Antialias : Boolean read FAntialias ;

     /// <summary>
     ///   Scaling filter (like Linear, Lanczos) to be used for scaling
     ///   images and grids.
     /// </summary>
     /// <remarks>
     ///   It is treated just as a suggestion. If layer has an Antialias=True
     ///   and antialiasing is not managed by layer driver then a selected filer
     ///   will be used.
     /// </remarks>
     property AntialiasFilter : TGIS_ScalingFilter
                                read FAntialiasFilter ;

     /// <summary>
     ///   If true (default), then layer can be drawn as progressive.
     /// </summary>
     /// <remarks>
     ///   Can be automatically turn off if layer like WMTS is translucent.
     /// </remarks>
     property Progressive     : Boolean read  FProgressive
                                        write FProgressive ;

     /// <summary>
     ///   Number of pages in an image file. Example is a multipage TIFF.
     /// </summary>
     property PageCount : Integer read FPagesCount ;

     /// <summary>
     ///   Color property to define default color to be used to fill "empty"
     ///   area upon PixelExportManager.
     /// </summary>
     property NoDataColor : TGIS_Color read  FNoDataColor
                                       write FNoDataColor ;

     /// <summary>
     ///   Returns parameters for pixel layer.
     /// </summary>
     property Params : TGIS_ParamsSectionPixel read  fget_ParamsPixel
                                               write fset_ParamsPixel ;
                                               {$IFDEF OXYGENE}
                                                 reintroduce ;
                                               {$ENDIF}
     /// <summary>
     ///   Current page of multipage file. Example is a multipage TIFF.
     /// </summary>
     property CurrentPage : Integer read  fget_Page ;

     /// <summary>
     ///   Pixel sub-format list that is available for write operation.
     /// </summary>
     property Capabilities : TGIS_LayerPixelSubFormatList read fget_Capabilities ;

     /// <summary>
     ///   Current sub-format information.
     /// </summary>
     property SubFormat : TGIS_LayerPixelSubFormat read FSubFormat ;

     /// <summary>
     ///   Default subformat information.
     /// </summary>
     property DefaultSubFormat : TGIS_LayerPixelSubFormat
                                 read fget_DefaultSubFormat ;

     /// <summary>
     ///   Forced interpretation of bands in a layer.
     ///   Supported only selected layers (mainly TIFF).
     /// </summary>
     property ForcedBandsDefinition : String
                                      read  FForcedBandsDefinition
                                      write FForcedBandsDefinition ;

     {#gendoc:hide:GENXDK}
     {#gendoc:hide:GENPDK}
     {#gendoc:hide:GENSCR}
     /// <summary>
     ///   A hint wich bitmap factory shpuld be used internally if applicable.
     /// </summary>
     /// <remarks>
     ///   Uses internally by TatukGIS.
     /// </remarks>
     property BitmapFactory : TGIS_BitmapFactory
                              read  FBitmapFactory
                              write FBitmapFactory;


   published //events
     /// <event/>
     /// <summary>
     ///   Grid operation event. Attach to manipulate grid data before display.
     /// </summary>
     property GridOperationEvent : TGIS_GridOperation read  fget_GridOperation
                                                      write fset_GridOperation ;

     /// <event/>
     /// <summary>
     ///   Pixel operation event. Attach to manipulate pixel (ARGB) data before
     ///   display.
     /// </summary>
     property PixelOperationEvent : TGIS_PixelOperation read  fget_PixelOperation
                                                        write fset_PixelOperation ;
  end ;


    /// <summary>
    ///   Layer enumerator.
    /// </summary>
    {#typehint:iterator:TGIS_PixelItem}
    TGIS_LayerPixelEnumerator = {$IFDEF OXYGENE} public {$ENDIF} class( TObject
                                  {$IFDEF CLR} , IEnumerator<TObject> {$ENDIF}
                                  {$IFDEF JAVA}, java.util.Iterator<TObject> {$ENDIF}
                               )

      private
        oFactory         : TGIS_LayerPixelEnumeratorFactory ;
        paramLayer       : TGIS_LayerPixel     ;
        paramExtent      : TGIS_Extent         ;
        paramPixelSize   : Double              ;
        paramFatPixel    : Boolean             ;
        paramShape       : TGIS_Shape          ;
        paramDe9im       : String              ;
        paramBand        : Integer             ;
        paramWritable    : Boolean             ;

        currBof          : Boolean             ;
        currLock         : array of TGIS_LayerPixelLock ;
        currX            : Integer             ;
        currY            : Integer             ;
        currPixel        : TGIS_PixelItem      ;
        currPoint        : TGIS_ShapePoint     ;
        currPolygon      : TGIS_ShapePolygon   ;
        currTopology     : TGIS_Topology       ;
        currOffset       : Double              ;
        currLength       : Double              ;
        currPos          : Integer             ;
        currShape        : TGIS_Shape          ;
        currShapeType    : TGIS_ShapeType      ;
        currSpecialMode  : Boolean             ;

      private
        {$IFDEF CLR}
          function   fget_current_obj : TObject ;
        {$ENDIF}
      public
        /// <summary>
        ///   Create an enumerator instance.
        /// </summary>
        /// <param name="_factory">
        ///   reference to factory object which creates this enumerator
        /// </param>
        /// <param name="_lp">
        ///   pixel layer handle
        /// </param>
        /// <param name="_extent">
        ///   extent of items to be found; expected _extent units are in a
        ///   Layer coordinate space
        /// </param>
        /// <param name="_pixelsize">
        ///   minimum requested size of image pixel; by providing 0 a maximum
        ///   possible resolution will be used, which is also a fastest method
        ///   if a layer is full in-memory because in such situation a pointer
        ///   to existing memory will be used
        /// </param>
        /// <param name="_fatpixel">
        ///   if false then pixel will be treated as point; if true then pixels
        ///   will be treated as small polygon, which is much slower but
        ///   provides some more advanced DE-9IM testing possibilities
        /// </param>
        /// <param name="_shape">
        ///   if not nil, then only shapes matched dm9 matrix with _shape will
        ///   be found
        /// </param>
        /// <param name="_de9im">
        ///   DE-9IM matrix of comparison
        /// </param>
        /// <param name="_writable">
        ///   if true than layer will post back any changes in locked area; if
        ///   write is not possible then TGIS_PixelItem.Writable will be set
        ///   accordingly
        /// </param>
        /// <remarks>
        ///   This override should be used only within internal use of
        ///   TGIS_LayerPixelEnumeratorFactory
        /// </remarks>
        constructor Create         ( const _factory     : TGIS_LayerPixelEnumeratorFactory ;
                                     const _lp          : TGIS_LayerPixel  ;
                                     const _extent      : TGIS_Extent      ;
                                     const _pixelsize   : Double           ;
                                     const _fatpixel    : Boolean          ;
                                     const _shape       : TGIS_Shape       ;
                                     const _de9im       : String           ;
                                     const _writable    : Boolean
                                   ) ; overload;

        /// <summary>
        ///   Create an enumerator instance.
        /// </summary>
        /// <param name="_factory">
        ///   reference to factory object which creates this enumerator
        /// </param>
        /// <param name="_lp">
        ///   pixel layer handle
        /// </param>
        /// <param name="_extent">
        ///   extent of items to be found; expected _extent units are in a
        ///   Layer coordinate space
        /// </param>
        /// <param name="_pixelsize">
        ///   minimum requested size of image pixel; by providing 0 a maximum
        ///   possible resolution will be used, which is also a fastest method
        ///   if a layer is full in-memory because in such situation a pointer
        ///   to existing memory will be used
        /// </param>
        /// <param name="_fatpixel">
        ///   if false then pixel will be treated as point; if true then pixels
        ///   will be treated as small polygon, which is much slower but
        ///   provides some more advanced DE-9IM testing possibilities
        /// </param>
        /// <param name="_shape">
        ///   if not nil, then only shapes matched dm9 matrix with _shape will
        ///   be found
        /// </param>
        /// <param name="_de9im">
        ///   DE-9IM matrix of comparison
        /// </param>
        /// <param name="_band">
        ///   band specification
        ///   <list type="number">
        ///     <item>
        ///       0 - if default Loop must be used; in such bands are
        ///       interpreted as image or grid depends or layer; Color or Value
        ///       are filled accordingly
        ///     </item>
        ///     <item>
        ///       number of specific band; in such situation only Bands property
        ///       is filled
        ///     </item>
        ///     <item>
        ///       -1 - all bands are retrieved; only Bands property is filled
        ///     </item>
        ///   </list>
        /// </param>
        /// <param name="_writable">
        ///   if true than layer will post back any changes in locked area; if
        ///   write is not possible then TGIS_PixelItem.Writable will be set
        ///   accordingly
        /// </param>
        /// <remarks>
        ///   This override should be used only within internal use of
        ///   TGIS_LayerPixelEnumeratorFactory
        /// </remarks>
        constructor Create         ( const _factory     : TGIS_LayerPixelEnumeratorFactory ;
                                     const _lp          : TGIS_LayerPixel  ;
                                     const _extent      : TGIS_Extent      ;
                                     const _pixelsize   : Double           ;
                                     const _fatpixel    : Boolean          ;
                                     const _shape       : TGIS_Shape       ;
                                     const _de9im       : String           ;
                                     const _band        : Integer          ;
                                     const _writable    : Boolean
                                   ) ; overload;

        /// <summary>
        ///   Create an enumerator instance.
        /// </summary>
        /// <param name="_lp">
        ///   pixel layer handle
        /// </param>
        /// <param name="_extent">
        ///   extent of items to be found; expected _extent units are in a
        ///   Layer coordinate space
        /// </param>
        /// <param name="_pixelsize">
        ///   minimum requested size of image pixel; by providing 0 a maximum
        ///   possible resolution will be used, which is also a fastest method
        ///   if a layer is full in-memory because in such situation a pointer
        ///   to existing memory will be used
        /// </param>
        /// <param name="_fatpixel">
        ///   if false then pixel will be treated as point; if true then pixels
        ///   will be treated as small polygon, which is much slower but
        ///   provides some more advanced DE-9IM testing possibilities
        /// </param>
        /// <param name="_shape">
        ///   if not nil, then only shapes matched dm9 matrix with _shape will
        ///   be found
        /// </param>
        /// <param name="_de9im">
        ///   DE-9IM matrix of comparison
        /// </param>
        /// <param name="_writable">
        ///   if true than layer will post back any changes in locked area; if
        ///   write is not possible then TGIS_PixelItem.Writable will be set
        ///   accordingly
        /// </param>
        constructor Create         ( const _lp          : TGIS_LayerPixel  ;
                                     const _extent      : TGIS_Extent      ;
                                     const _pixelsize   : Double           ;
                                     const _fatpixel    : Boolean          ;
                                     const _shape       : TGIS_Shape       ;
                                     const _de9im       : String           ;
                                     const _writable    : Boolean
                                   ) ; overload;
        {$IFNDEF MANAGED}
          /// <summary>
          ///   Destroy an object.
          /// </summary>
          destructor Destroy       ; override;
        {$ELSE}
          /// <summary>
          ///   Destroy an object.
          /// </summary>
          procedure  Dispose       ;
        {$ENDIF}

        /// <summary>
        ///   Reset enumerator.
        /// </summary>
        procedure  Reset           ;

        /// <summary>
        ///   Move to next record
        /// </summary>
        /// <returns>
        ///   If false then there is no more shapes.
        /// </returns>
        function   MoveNext        : Boolean ;

      {$IFDEF GENXDK}
        protected
      {$ENDIF}

        /// <summary>
        ///   Get current enumerator value.
        /// </summary>
        /// <returns>
        ///   Shape itself or nil.
        /// </returns>
        function   GetCurrent      : TGIS_PixelItem ;

      public

        {$IFDEF JAVA}
          /// <summary>
          ///   Java specific enumerator support method.
          /// </summary>
          /// <returns>
          ///   Java specific enumerator return value.
          /// </returns>
          method hasNext : Boolean ;

          /// <summary>
          ///   Java specific enumerator support method.
          /// </summary>
          /// <returns>
          ///   Java specific enumerator return value.
          /// </returns>
          method next : TObject;

          /// <summary>
          ///   Java specific enumerator support method.
          /// </summary>
          /// <returns>
          ///   Java specific enumerator return value.
          /// </returns>
          method &remove ;
        {$ENDIF}

      public

        /// <summary>
        ///   Current enumerator value.
        /// </summary>
        {$IFNDEF CLR}
          property  Current : TGIS_PixelItem  read GetCurrent      ;
        {$ELSE}
          property  Current : TObject         read fget_current_obj ;
        {$ENDIF}
    end ;

    {$IFDEF CLR}
      /// <summary>
      ///   Enumerator class for LINQ.
      /// </summary>
      TGIS_LayerPixelEnumeratorFactoryLinq = {$IFDEF OXYGENE} public {$ENDIF}
                                             class( TObject, IEnumerable )
        private
         en : TGIS_LayerPixelEnumerator ;
        public

          /// <summary>
          ///   Constructor.
          /// </summary>
          /// <param name ="_en">
          ///   Enumerator reference
          /// </param>
          constructor Create( const _en : TGIS_LayerPixelEnumerator
                             ) ;

          /// <summary>
          ///   Get constructed enumerator.
          /// </summary>
          /// <returns>
          ///   Linq enumerator.
          /// </returns>
          function  GetEnumerator : IEnumerator ;
      end ;
    {$ENDIF}

    /// <summary>
    ///   Factory class for TGIS_LayerPixelEnumerator.
    /// </summary>
    /// <remarks>
    ///   To be used internally by TGIS_LayerPixel.Loop().
    /// </remarks>
    {#typehint:iterator:TGIS_PixelItem}
    TGIS_LayerPixelEnumeratorFactory = {$IFDEF OXYGENE} public {$ENDIF} class
                                       {$IFDEF JAVA} (Iterable<TObject>) {$ENDIF}

      private
        stateLayer       : TGIS_LayerPixel  ;
        stateExtent      : TGIS_Extent      ;
        statePixelSize   : Double           ;
        stateShape       : TGIS_Shape       ;
        stateDe9im       : String           ;
        stateBand        : Integer          ;
        stateWritable    : Boolean          ;
        stateFatPixel    : Boolean          ;
      public

        /// <summary>
        ///   Prepare enumerator.
        /// </summary>
        /// <param name="_lp">
        ///   pixel layer handle
        /// </param>
        /// <param name="_extent">
        ///   extent of items to be found; expected _extent units are in a
        ///   Layer coordinate space
        /// </param>
        /// <param name="_pixelsize">
        ///   minimum requested size of image pixel; by providing 0 a maximum
        ///   possible resolution will be used, which is also a fastest method
        ///   if a layer is full in-memory because in such situation a pointer
        ///   to existing memory will be used
        /// </param>
        /// <param name="_fatpixel">
        ///   if false then pixel will be treated as point; if true then pixels
        ///   will be treated as small polygon, which is much slower but
        ///   provides some more advanced DE-9IM testing possibilities
        /// </param>
        /// <param name="_shape">
        ///   if not nil, then only shapes matched dm9 matrix with _shape will
        ///   be found
        /// </param>
        /// <param name="_de9im">
        ///   DE-9IM matrix of comparison
        /// </param>
        /// <param name="_writable">
        ///   if true than layer will post back any changes in locked area; if
        ///   write is not possible then TGIS_PixelItem.Writable will be set
        ///   accordingly
        /// </param>
        procedure SetUp          ( const _lp          : TGIS_LayerPixel  ;
                                   const _extent      : TGIS_Extent      ;
                                   const _pixelsize   : Double           ;
                                   const _fatpixel    : Boolean          ;
                                   const _shape       : TGIS_Shape       ;
                                   const _de9im       : String           ;
                                   const _writable    : Boolean
                                 ) ; overload ;

        /// <summary>
        ///   Prepare enumerator.
        /// </summary>
        /// <param name="_lp">
        ///   pixel layer handle
        /// </param>
        /// <param name="_extent">
        ///   extent of items to be found; expected _extent units are in a
        ///   Layer coordinate space
        /// </param>
        /// <param name="_pixelsize">
        ///   minimum requested size of image pixel; by providing 0 a maximum
        ///   possible resolution will be used, which is also a fastest method
        ///   if a layer is full in-memory because in such situation a pointer
        ///   to existing memory will be used
        /// </param>
        /// <param name="_fatpixel">
        ///   if false then pixel will be treated as point; if true then pixels
        ///   will be treated as small polygon, which is much slower but
        ///   provides some more advanced DE-9IM testing possibilities
        /// </param>
        /// <param name="_shape">
        ///   if not nil, then only shapes matched dm9 matrix with _shape will
        ///   be found
        /// </param>
        /// <param name="_de9im">
        ///   DE-9IM matrix of comparison
        /// </param>
        /// <param name="_band">
        ///   band specification
        ///   <list type="number">
        ///     <item>
        ///       0 - if default Loop must be used; in such bands are
        ///       interpreted as image or grid depends or layer; Color or Value
        ///       are filled accordingly
        ///     </item>
        ///     <item>
        ///       number of specific band; in such situation only Bands property
        ///       is filled
        ///     </item>
        ///     <item>
        ///       -1 - all bands are retrieved; only Bands property is filled
        ///     </item>
        ///   </list>
        /// </param>
        /// <param name="_writable">
        ///   if true than layer will post back any changes in locked area; if
        ///   write is not possible then TGIS_PixelItem.Writable will be set
        ///   accordingly
        /// </param>
        procedure SetUp          ( const _lp          : TGIS_LayerPixel  ;
                                   const _extent      : TGIS_Extent      ;
                                   const _pixelsize   : Double           ;
                                   const _fatpixel    : Boolean          ;
                                   const _shape       : TGIS_Shape       ;
                                   const _de9im       : String           ;
                                   const _band        : Integer          ;
                                   const _writable    : Boolean
                                 ) ; overload ;
        {$IFDEF JAVA}
          /// <summary>
          ///   Java specific enumerator support method.
          /// </summary>
          /// <returns>
          ///   Java enumerator.
          /// </returns>
          method &iterator : java.util.Iterator<TObject> ;
        {$ENDIF}

        /// <summary>
        ///   Get constructed enumerator.
        /// </summary>
        /// <returns>
        ///   Enumerator object.
        /// </returns>
        function  GetEnumerator  : TGIS_LayerPixelEnumerator ;
        {$IFDEF CLR}

        /// <summary>
        ///   Get constructed enumerator for LINQ.
        /// </summary>
        /// <returns>
        ///   Linq enumerator.
        /// </returns>
        function  ToLinq         : TGIS_LayerPixelEnumeratorFactoryLinq ;
        {$ENDIF}
    end ;

const
  /// <summary>
  ///   Maximal buffer size for keeping decompressed lines.
  /// </summary>
  MAX_STRIP_BUFFER_SIZE   = 1024000  ;
  /// <summary>
  ///   Base transformed rectangle with in pixels.
  /// </summary>
  HPIXELS = 10 ;
  /// <summary>
  ///   Base transformed rectangle height in pixels.
  /// </summary>
  VPIXELS = 3 ;
  /// <summary>
  ///   First transparent zone marker (is shifted left for next zones) .
  /// </summary>
  BASE_TRANSPARENT_FLAG = $0000000000000001 ;
{#gendoc:hide}
  METADATA_MEMORYLAYERLIMIT
   = 'TGIS_LayerPixel.MemoryLayerLimit' ;
{#gendoc:hide}
  METADATA_TIMEOUT
   = 'TGIS_LayerPixel.Timeout' ;


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisConfig,
    GisFunctions,
    GisFileTAB,
    GisGeometryFactory,
    GisInternals,
    GisResource,
    GisViewer;
{$ENDIF}

const
  // Based on Recommendation ITU-R BT.601-7,
  // this approach is used by MATLAB and OpenCV
  // Red factor for gray computation.
  FACTOR_RED        = 0.299 ;
  // Green factor for gray computation.
  FACTOR_GREEN      = 0.587 ;
  // Blue factor for gray computation.
  FACTOR_BLUE       = 0.114  ;
  // Maximum waiting time for bitmap fetch.
  GETBITMAP_TIMEOUT = 30000 ;
  // Maximum size of kept in memory layer in MB.
  MEMORY_LAYER_LIMIT = 100 ;

  // calculate pixels with proper rounding to avoid
  // imprecise calculations on a border
  // use this function for extent/rect
  function round_pixel(
    const _offset    : Double ;
    const _pixelsize : Double
  ) : Integer ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
  begin
    Result := RoundS( _offset / _pixelsize ) ;
  end ;

  // use this function for points
  function floor_pixel(
    const _offset    : Double ;
    const _pixelsize : Double
  ) : Integer ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
  var
    idx       : Double ;
    floor_idx : Integer ;
  begin
    idx := _offset / _pixelsize ;
    floor_idx := FloorS( idx ) ;

    // avoid rounding error close to cell border
    if GisIsSameValue( idx, floor_idx+1, GIS_DOUBLE_RESOLUTION ) then
      Result := floor_idx + 1
    else
      Result := floor_idx ;
  end ;

  // use for determining loop's extent
  function ceil_pixel(
    const _offset    : Double ;
    const _pixelsize : Double
  ) : Integer ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
  var
    idx      : Double ;
    ceil_idx : Integer ;
  begin
    idx := _offset / _pixelsize ;
    ceil_idx := CeilS( idx ) ;

    // avoid rounding error close to cell border
    if GisIsSameValue( idx, ceil_idx-1, GIS_DOUBLE_RESOLUTION ) then
      Result := ceil_idx - 1
    else
      Result := ceil_idx ;
  end ;

type
  //T_TransMatrix = array [0..1] of array [0..1] of Double ;
  {$IFDEF CLR}
    T_TransMatrix = array [0..1, 0..1] of Double ;
  {$ELSE}
    T_TransMatrix = array [0..1] of array [0..1] of Double ;
  {$ENDIF}
  T_PointSet = array [0..3] of TGIS_Point ;
  T_IndexSet = array [0..2] of Integer ;

{$REGION 'T_TransClass'}
T_TransClass = class
  private
    internalTransparentColor : TGIS_Color ; //Native transparent color used in some layers
    mpi         : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF} ; //input point translation
    mpt         : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF} ; //corresponding transformed point translation
    scX         : Double ;
    //set but not read
    ccw_scY,
    cw_scY      : Double ;
    ccw_shX,
    cw_shX      : Double ;

    inPoints    : T_PointSet ; // clockwise
    trPoints    : T_PointSet ; // clockwise
    cwPart      : T_IndexSet ; // clockwise  triangle
    ccwPart     : T_IndexSet ; // counter-clockwise triangle
    cwTrMatrix  : T_TransMatrix ;
    ccwTrMatrix : T_TransMatrix ;
    player      : TGIS_LayerPixel ;
    baseRotation  : Boolean ;
    internalRotation : Boolean ;

    in_img_rect : TRect ;
    iowidth,
    ioheight    : Integer ;
    prows_max,
    pcols_max   : Integer ;
    cx, cy      : Double ;
    ur_up_extent: TGIS_Extent {$IFDEF GIS_NORECORDS} := new TGIS_Extent {$ENDIF} ;
    w, h        : Double ;
    r_p_extent  : TGIS_Extent {$IFDEF GIS_NORECORDS} := new TGIS_Extent {$ENDIF} ;
    pw, ph      : Double ;
    dxx, dyy    : Double ;
    maxrow_out,
    maxcol_out  : Integer ;
    lpw, lph    : Double ;
    ni, nk      : Integer ;
    ix, iy      : Integer ;
    w4b_in      : Integer ;

    row_start,
    row_stop,
    col_start,
    col_stop    : Integer ;

    cutpolygon  : TGIS_ShapePolygon ;
    cut_idx_y   : array of Integer ;
    cut_idx_x   : array of Integer ;
    cutpixels   : TGIS_Pixels ;
    cutsize     : Integer ;
    xcutsize    : Integer ;
    ycutsize    : Integer ;
    cutscale    : Double ;
    xcutv       : Double ;
    ycutv       : Double ;
    cutoffset   : TPoint ;
    xcutidx     : Integer ;
    ycutidx     : Integer ;
  public
    RangeError : Boolean ;
  private
    // Calculates doubled signed area of triangle given by three points.
    // Only for internal use of TatukGIS.
    // _a, _b, _c   given points
    // return       Signed doubled triangle area.
    //              = 0 - points are on line (no triangle)
    //              > 0 - ccw type (each point on left from others two)
    //              < 0 - cw type (each point on right from others two)
    function  area2       ( const _a, _b, _c    : TGIS_Point
                          ) : Double ; inline ;

    // Tests single point and its projected spot
    // return   True if OK
    function  pt_projected_ppt
                          ( const _pt, _ppt : TGIS_Point
                          ) : Boolean ;

    // Divides input and output image rectangle in two triangles
    // return   True if OK
    function  divideInTriangles : Boolean ;

    // Calculates transformation matrices for given for input four points
    // and corresponding output for points.
    // Only for internal use of TatukGIS.
    procedure calculateMatrices ;

    // Sets input four points, culates corresponding output for points then
    // calculates transformation matrices
    // _inPoints   input four points
    function  setTransform( const _inPoints     : T_PointSet
                          ) : Boolean ;

    // Calculates params for image transforming
    // _inImageExtent   extent in transformed coordinates
    // _inImgRect       corresponding rectangle in pixels
    // _iowidth         input/output width in pixels
    // _ioheight        input/output height in pixels
    procedure calcDataParams( const _inImageExtent : TGIS_Extent ;
                              var   _iowidth       : Integer     ;
                              var   _ioheight      : Integer
                            ) ;

    // Find original coordinates of the single transformed point.
    // _trPt    transformed point
    // _inPt    corresponding original point (if exists)
    // return   True if original point exists
    function  inputPoint  ( const _trPt         : TGIS_Point  ;
                            var _inPt     : TGIS_Point
                          ) : Boolean ;

  {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
    // Project a defined extent.
    // _ext     extent to be projected
    // return   projected extent
    function  projectedExt( const _ext    : TGIS_Extent
                          ) : TGIS_Extent ;

    // Unproject a defined extent.
    // _ext     extent to be unprojected
    // return   unprojected extent
    function  unprojectedExt
                          ( const _ext    : TGIS_Extent
                          ) : TGIS_Extent ;

    // Apply a projection on the single point.
    // _ptg     point to be projected
    // return   projected point
    function  projected   ( const _ptg    : TGIS_Point
                          ) : TGIS_Point ;

    // Find not projected coordinates of the single projected point.
    // _ptg     projected point
    // return   original point
    function  unprojected ( const _ptg    : TGIS_Point
                          ) : TGIS_Point ;

    // Transforms grid data
    // _inImageExtent   extent in transformed coordinates
    // _inImgRect       corresponding rectangle in pixels
    // _iowidth         input/output width in pixels
    // _ioheight        input/output height in pixels
    // return           true if ready
    function  getTransformedGridData
                            ( const _bufOut        : TGIS_GridArray ;
                              const _inImageExtent : TGIS_Extent    ;
                              var   _iowidth       : Integer        ;
                              var   _ioheight      : Integer
                            ) : Boolean ;

    // Transforms bitmap.
    // _inImageExtent   extent in transformed coordinates
    // _inImgRect       corresponding rectangle in pixels
    // _iowidth         input/output width in pixels
    // _ioheight        input/output height in pixels
    // return           true if ready
    function  getTransformedBitmapData
                            ( const _bmp           : TGIS_Pixels  ;
                              const _inImageExtent : TGIS_Extent ;
                              var   _iowidth       : Integer     ;
                              var   _ioheight      : Integer
                            ) : Boolean ;
  public
    // Create a T_TransClass instance.
    constructor Create    ( const _l      : TGIS_LayerPixel
                          ) ;

    {$IFDEF DCC}
      destructor Destroy  ; override;
    {$ENDIF}
end ;

T_ParseZones = class( TGIS_Object )
  private
    FCount : Integer ;
    FIsRGB : Boolean ;
  private
    tkn : TGIS_Tokenizer ;
  protected
    procedure doDestroy ; override;
  public
    constructor Create( const _txt : String ) ;
  public
    function ReadRGB  ( const _idx : Integer ) : TGIS_Color  ;
    function ReadFloat( const _idx : Integer ) : Double ;
  public
    property Count : Integer read FCount ;
    property IsRGB : Boolean read FIsRGB ;
end ;

TGridContributor = record
  weight : Single ;
  pixel_idx : Integer; // src Pixel
end ;
{$IFNDEF MANAGED}
  PGridContributor = ^TGridContributor;
{$ENDIF}

TGridContributors = array of TGridContributor;
TGridContributorEntry = record
  num : Integer;
  contributors : TGridContributors;
end ;
TGridContributorList = array of TGridContributorEntry;

T_FilterFunc = function ( _val : Single) : Single of object ;


constructor T_TransClass.Create (
  const _l : TGIS_LayerPixel
) ;
var
  shptmp  : TGIS_ShapePolygon;
  is_rot  : Boolean ;
  npart, npoint : Integer ;
begin
  inherited Create;

  {$IFDEF GIS_NORECORDS}
   var i : Integer ;
   for i := 0 to 3 do
     inPoints[i] := new TGIS_Point ;
   for i := 0 to 3 do
     trPoints[i] := new TGIS_Point ;
  {$ENDIF}

  player := _l ;
  is_rot := False ;
  if ( not assigned( player.CuttingPolygon ) ) or
     ( assigned(player.Transform) and (not player.activeTransform) )
  then
    cutpolygon := nil
  else begin
    if assigned(player.Transform) then begin
      shptmp := TGIS_ShapePolygon.Create ;

      for npart := 0 to player.CuttingPolygon.GetNumParts - 1 do begin
        shptmp.AddPart() ;
        for npoint := 0 to player.CuttingPolygon.GetPartSize( npart ) - 1 do begin
          shptmp.AddPoint( player.Transform.Transform(
                             player.CuttingPolygon.GetPoint( npart, npoint )
                           )
                         );
        end;
      end;



    end
    else begin
      if assigned( player.Viewer ) then begin
        if player.Viewer.Ref.RotationAngle <> 0 then begin
          shptmp := TGIS_ShapePolygon.Create ;
          is_rot := True ;

          for npart := 0 to player.CuttingPolygon.GetNumParts - 1 do begin
            shptmp.AddPart() ;
            for npoint := 0 to player.CuttingPolygon.GetPartSize( npart ) - 1 do begin
              shptmp.AddPoint( player.CuttingPolygon.GetPoint( npart, npoint ) );
            end;
          end;
        end
        else
          shptmp := player.CuttingPolygon ;
      end
      else
        shptmp := player.CuttingPolygon ;
    end;

    if ( not assigned( player.Viewer ) ) or
       ( player.CS = player.Viewer.Ref.CS )
    then
      cutpolygon := TGIS_ShapePolygon( shptmp.CreateCopy )
    else
      cutpolygon := TGIS_ShapePolygon(
                      TGIS_GeometryFactory.GisCreateReprojectedShape(
                        shptmp,
                        player.CS,
                        player.Viewer.Ref.CS
                      )
                    ) ;

    if assigned(player.Transform) or is_rot then begin
      FreeObject( shptmp );
    end;


  end ;

  internalTransparentColor := player.internalTransparentColor ;
  baseRotation := player.baseRotation ;
  internalRotation := player.internalRotation ;
  if baseRotation then begin
    player.cntrRotX := player.FExtent.XMin + player.scaleX/2;
    player.cntrRotY := player.FExtent.YMax + player.scaleY/2;
    player.cntrRotFactX := player.cntrRotX*(1-player.scaleXFactor) ;
    player.cntrRotFactY := player.cntrRotY*(1-player.scaleYFactor) ;
  end ;
end ;

{$IFDEF DCC}
  destructor T_TransClass.Destroy ;
  begin
    FreeObject( cutpolygon ) ;
    inherited ;
  end;
{$ENDIF}

function  T_TransClass.projectedExt(
  const _ext    : TGIS_Extent
) : TGIS_Extent ;
var
  extent  : TGIS_Extent ;
  extentr : TGIS_Extent ;
  extent1 : TGIS_Extent ;
  must_rotate : Boolean ;
begin
  must_rotate := True ;
  if assigned(player.Viewer) or  assigned( player.outCS ) then begin
    if baseRotation or internalRotation  then
      extentr := player.baseRotatedExtent( _ext )
    else
      extentr := _TGIS_Extent(_ext) ;
    must_rotate := False ;
    extent  := player.transformExtent( True, extentr ) ;
    if assigned( player.outCS ) then begin
      extent1 := player.outCS.ExtentFromCS( player.CS, extent ) ;
      if GisIsValidExtent( extent1 ) then
        extent := _TGIS_Extent(extent1) ;
    end ;
  end
  else
    extent := _TGIS_Extent(player.FExtent) ;
  if (baseRotation or internalRotation) and must_rotate then
    Result := player.baseRotatedExtent( extent )
  else
    Result := _TGIS_Extent(extent) ;
end ;

function  T_TransClass.unprojectedExt(
  const _ext    : TGIS_Extent
) : TGIS_Extent ;
var
  extent : TGIS_Extent ;
begin
  extent := player.CS.ExtentFromCS(player.outCS, _ext ) ;
  if baseRotation or internalRotation then
    Result := player.baseUnrotatedExtent( extent )
  else
    Result := _TGIS_Extent(extent) ;
  Result := player.transformExtent( False, Result ) ;
end ;

function T_TransClass.projected(
  const _ptg : TGIS_Point
) : TGIS_Point ;

  function _project_outCS(
    const _ptg : TGIS_Point
  ) : TGIS_Point ;
  var
    tmp : TGIS_Point3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      tmp := new TGIS_Point3D ;
    {$ENDIF}
    tmp.X := _ptg.X ;
    tmp.Y := _ptg.Y ;
    tmp.Z := 0 ;
    tmp.M := 0 ;

//    Project3D_Ref( tmp ) ;
    if assigned( player.Transform ) and player.Transform.Active then begin
      player.Transform.Transform3D_Ref( tmp ) ;
    end;
    try
      player.CS.ToWGS3D_Ref( tmp ) ;
      player.outCS.FromWGS3D_Ref(tmp ) ;
    except
      // do nothing
    end ;

    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point ;
    {$ENDIF}
    Result.X := tmp.X ;
    Result.Y := tmp.Y ;
  end ;

begin
  RangeError := False ;
  Result := _TGIS_Point(_ptg) ;
  if baseRotation or internalRotation then
    Result := player.basePointRot( Result ) ;

  if player.forViewer then
    Result := player.Project( Result )
  else
    Result := _project_outCS(Result) ;

  if (Result.X > 1e30) or (Result.Y > 1e30) then
    RangeError := True
  else
  if (Result.X < -1e30) or (Result.Y < -1e30) then
    RangeError := True ;
end ;

function T_TransClass.unprojected(
  const _ptg : TGIS_Point
) : TGIS_Point ;

  function _unproject_outCS(
    const _ptg : TGIS_Point
  ) : TGIS_Point ;
  var
    tmp : TGIS_Point3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      tmp := new TGIS_Point3D ;
    {$ENDIF}
    tmp.X := _ptg.X ;
    tmp.Y := _ptg.Y ;
    tmp.Z := 0 ;
    tmp.M := 0 ;

    player.outCS.ToWGS3D_Ref( tmp ) ;
    player.CS.FromWGS3D_Ref( tmp ) ;

    if assigned( player.Transform ) and player.Transform.Active then begin
      player.Transform.Untransform3D_Ref( tmp ) ;
    end;

    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point ;
    {$ENDIF}
    Result.X := tmp.X ;
    Result.Y := tmp.Y ;
  end ;

begin
  if player.forViewer then
    Result := player.Unproject(_ptg)
  else
    Result := _unproject_outCS(_ptg) ;

  if baseRotation or internalRotation then
    Result := player.basePointUnRot( Result ) ;
end ;

function T_TransClass.area2(
  const _a, _b, _c : TGIS_Point
) : Double ;
begin
  Result := (_b.X -_a.X) * (_c.Y -_a.Y) - (_c.X -_a.X) * (_b.Y -_a.Y) ;
end ;

function  T_TransClass.pt_projected_ppt(
  const _pt, _ppt : TGIS_Point
) : Boolean ;
var
  ar2, proj_ar2 : Double ;
  p00, p01,
  pp00, pp01    : TGIS_Point ;
begin
  p00 := GisPoint(0, 0) ;
  p01 := GisPoint(0, 1) ;
  pp00 := projected(p00) ;
  pp01 := projected(p01) ;
  ar2 := area2(p00, p01, _pt) ;
  proj_ar2 := area2(pp00, pp01, _ppt) ;

  if Sign(ar2) = Sign(proj_ar2) then
    Result := True
  else
    Result := False ;
end ;

function  T_TransClass.setTransform(
  const _inPoints     : T_PointSet
) : Boolean ;
var
  i : Integer ;

  xmino, xmaxo, ymino, ymaxo : Double ;

begin
  inPoints := _inPoints ;
  Result := False ;

  trPoints[0] := projected(inPoints[0]) ;
  if RangeError then
    exit ;
  trPoints[1] := projected(inPoints[1]) ;
  if RangeError then
    exit ;
  trPoints[2] := projected(inPoints[2]) ;
  if RangeError then
    exit ;
  trPoints[3] := projected(inPoints[3]) ;
  if RangeError then
    exit ;

  if not divideInTriangles then begin
    exit ;
  end ;

  xmino := trPoints[0].X ;
  xmaxo := trPoints[0].X ;
  ymino := trPoints[0].Y ;
  ymaxo := trPoints[0].Y ;

  for i := 1 to 3 do begin
    if xmino > trPoints[i].X then
      xmino := trPoints[i].X
    else
    if xmaxo < trPoints[i].X then
      xmaxo := trPoints[i].X ;

    if ymino > trPoints[i].Y then
      ymino := trPoints[i].Y
    else
    if ymaxo < trPoints[i].Y then
      ymaxo := trPoints[i].Y ;
  end ;

  if (xmino >= r_p_extent.XMax) or
     (ymino >= r_p_extent.YMax) or
     (xmaxo <= r_p_extent.XMin) or
     (ymaxo <= r_p_extent.YMin) then
  begin
    exit ;
  end ;

  row_start := RoundS((ymino/ph) -0.5) -RoundS((dyy/ph) -0.5) ;
  if row_start < 0 then
    row_start := 0
  else
  if row_start > maxrow_out then
    row_start := maxrow_out ;

  row_stop := RoundS(ymaxo/ph) -RoundS(dyy/ph) ;
  if row_stop < 0 then
    row_stop := 0
  else
  if row_stop > maxrow_out then
    row_stop := maxrow_out ;

  col_start := RoundS((xmino -dxx)/pw) ;
  if col_start < 0 then
    col_start := 0
  else
  if col_start > maxcol_out then
    col_start := maxcol_out ;

  col_stop := RoundS((xmaxo -dxx)/pw) ;
  if col_stop < 0 then
    col_stop := 0
  else
  if col_stop > maxcol_out then
    col_stop := maxcol_out ;

  if (col_stop < col_start) then
    lpw := (xmaxo -xmino)/(col_start -col_stop+1)
  else
    lpw := (xmaxo -xmino)/(col_stop -col_start +1) ;
  if (row_stop < row_start) then
    lph := (ymaxo -ymino)/(row_start -row_stop +1)
  else
    lph := (ymaxo -ymino)/(row_stop -row_start +1) ;

  if lpw < 1e-7 then
    exit ;
  if lph < 1e-7 then
    exit ;

  for i := 0 to 3 do begin
    inPoints[i].X := (inPoints[i].X -ur_up_extent.XMin)/w ;
    inPoints[i].Y := (inPoints[i].Y -ur_up_extent.YMin)/h ;
  end ;

  for i := 0 to 3 do begin
    trPoints[i].X := (trPoints[i].X -xmino)/lpw ;
    trPoints[i].Y := (trPoints[i].Y -ymino)/lph ;
  end ;
  Result := True ;
  calculateMatrices ;
end ;

function T_TransClass.divideInTriangles : Boolean ;
var
  a0, a1 : Double ;
  b0, b1 : Double ;
  c0, c1 : Double ;
  v : Integer ;
  i, k : Integer ;
  pt_valid : array [0..3] of Boolean ;
  procedure move_point2base(_idx, _bidx : Integer) ;
  var
    j, steps : Integer ;
    dx, dy : Double ;
    pt, ppt, ptb : TGIS_Point ;
  begin
    steps := 2 * HPIXELS ;
    pt  := _TGIS_Point(inPoints[_idx]) ;
    ptb := _TGIS_Point(inPoints[_bidx]) ;

    dx := (ptb.X -pt.X)/steps ;
    dy := (ptb.Y -pt.Y)/steps ;
    dec(steps) ;
    pt.X := pt.X +dx ;
    pt.Y := pt.Y +dy ;
    for j := 0 to steps do begin
      ppt := projected(pt) ;
      if pt_projected_ppt(pt, ppt) then begin
        trPoints[_idx] := _TGIS_Point(ppt) ;
        exit ;
      end ;
      pt.X := pt.X +dx ;
      pt.Y := pt.Y +dy ;
    end ;
  end ;

begin
  Result := True ;
  a0 := -area2(trPoints[0], trPoints[1], trPoints[2]) ;
  b0 := -area2(trPoints[2], trPoints[3], trPoints[0]) ;
  if (a0 > 0) and (b0 > 0) then begin
    if a0 >= b0 then
      c0 := b0/a0
    else
      c0 := a0/b0 ;
    v := 0 ;
  end
  else
  if (a0 < 0) and (b0 < 0) then begin
    if a0 <= b0 then
      c0 := b0/a0
    else
      c0 := a0/b0 ;
    v := 3 ;
  end
  else
  begin
    k := 0 ;
    for i := 0 to 3 do begin
      pt_valid[i] := pt_projected_ppt(inPoints[i], trPoints[i]) ;
      if pt_valid[i] then
        inc(k) ;
    end ;
    if k = 0 then begin
      Result := False ;
      exit ;
    end ;

    if not pt_valid[0] then begin
      if pt_valid[3] then
        move_point2base(0, 3)
      else
      if pt_valid[1] then
        move_point2base(0, 1)
      else
        move_point2base(0, 2) ;
    end ;

    if not pt_valid[1] then begin
      if pt_valid[2] then
        move_point2base(1, 2)
      else
      if pt_valid[0] then
        move_point2base(1, 0)
      else
        move_point2base(1, 3) ;
    end ;

    if not pt_valid[2] then begin
      if pt_valid[1] then
        move_point2base(2, 1)
      else
      if pt_valid[3] then
        move_point2base(2, 3)
      else
        move_point2base(2, 0) ;
    end ;

    if not pt_valid[3] then begin
      if pt_valid[0] then
        move_point2base(3, 0)
      else
      if pt_valid[2] then
        move_point2base(3, 2)
      else
        move_point2base(3, 1) ;
    end ;

    a0 := -area2(trPoints[0], trPoints[1], trPoints[2]) ;
    b0 := -area2(trPoints[2], trPoints[3], trPoints[0]) ;
    if ( a0 > GIS_DOUBLE_RESOLUTION ) and ( b0 > GIS_DOUBLE_RESOLUTION )  then begin
      if a0 >= b0 then
        c0 := b0/a0
      else
        c0 := a0/b0 ;
    end
    else
      c0 := -1 ;
    v := 0 ;
  end ;
  if (1-c0) > GIS_DOUBLE_RESOLUTION then begin
    a1 := -area2(trPoints[1], trPoints[2], trPoints[3]) ;
    b1 := -area2(trPoints[3], trPoints[0], trPoints[1]) ;
    if (a1 > 0) and (b1 > 0) then begin
      if a1 >= b1 then
        c1 := b1/a1
      else
        c1 := a1/b1 ;
    end
    else
    begin
      Result := False ;
      exit ;
    end ;

    if c0 < c1 then
      v := 1 ;
  end ;

  case v of
    0 :
      begin
        cwPart[0]  := 0 ;
        cwPart[1]  := 2 ;
        cwPart[2]  := 3 ;

        ccwPart[0] := 0 ;
        ccwPart[1] := 2 ;
        ccwPart[2] := 1 ;
      end ;
    1 :
      begin
        cwPart[0]  := 1 ;
        cwPart[1]  := 3 ;
        cwPart[2]  := 0 ;

        ccwPart[0] := 1 ;
        ccwPart[1] := 3 ;
        ccwPart[2] := 2 ;
      end ;
    3 :
      begin
        cwPart[0]  := 0 ;
        cwPart[1]  := 2 ;
        cwPart[2]  := 1 ;

        ccwPart[0] := 0 ;
        ccwPart[1] := 2 ;
        ccwPart[2] := 3 ;
      end ;
  end ;
end ;

procedure T_TransClass.calculateMatrices ;
var
  bpi : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF} ;
  bpt : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF} ;

  rottr        : T_TransMatrix ;
  rotin        : T_TransMatrix ;
  wm           : T_TransMatrix ;
  scale_tr_in  : T_TransMatrix ;
  shear        : T_TransMatrix ;

  inangle      : Double   ;
  trangle      : Double   ;
  cosin        : Double   ;
  sinin        : Double   ;
  costr        : Double   ;
  sintr        : Double  ;

  wpt1         : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
  wpt2         : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
  rpt1         : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
  rpt2         : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
  xin0, xtr0   : Double ;
  yin0, ytr0   : Double ;

  function calcAngle(_p : TGIS_Point) : Double ;
  begin
    if _p.X = 0 then begin
      if _p.Y > 0 then
        Result := 0.5 * Pi
      else
        Result := 1.5 * Pi ;
    end
    else begin
      Result := ArcTan2(_p.Y, _p.X) ;
      if Result < 0 then
        Result := Result + 2 * Pi ;
    end ;
  end ;

  function matircesMultiplication (_m1, _m2 : T_TransMatrix) : T_TransMatrix ;
  begin
    Result[0, 0] := _m1[0, 0]*_m2[0, 0] +_m1[0, 1]*_m2[1, 0] ;
    Result[0, 1] := _m1[0, 0]*_m2[0, 1] +_m1[0, 1]*_m2[1, 1] ;
    Result[1, 0] := _m1[1, 0]*_m2[0, 0] +_m1[1, 1]*_m2[1, 0] ;
    Result[1, 1] := _m1[1, 0]*_m2[0, 1] +_m1[1, 1]*_m2[1, 1] ;
  end ;

  function calcMatrix(_pidx : Integer; _isCW : Boolean) : T_TransMatrix ;
  begin
    //scaling transformed to input
    wpt1 := _TGIS_Point(inPoints[_pidx]) ;
    wpt1.X := wpt1.X -mpi.X ;
    wpt1.Y := wpt1.Y -mpi.Y ;

    wpt2 := _TGIS_Point(trPoints[_pidx]) ;
    wpt2.X := wpt2.X -mpt.X ;
    wpt2.Y := wpt2.Y -mpt.Y ;

    rpt1.X := wpt1.X*cosin +wpt1.Y*sinin ;
    rpt1.Y := wpt1.Y*cosin -wpt1.X*sinin ;

    rpt2.X := wpt2.X*costr +wpt2.Y*sintr ;
    rpt2.Y := wpt2.Y*costr -wpt2.X*sintr ;

    //test if 0
    yin0 := (inPoints[cwPart[1]].Y -mpi.Y)*cosin -inPoints[cwPart[1]].X*sinin ;
    ytr0 := (trPoints[cwPart[1]].Y -mpt.Y)*costr -trPoints[cwPart[1]].X*sintr ;
    scale_tr_in[0, 0] := xin0 / xtr0 ;
    scale_tr_in[0, 1] := 0 ;
    scale_tr_in[1, 0] := 0 ;
    if Abs(rpt2.Y) > GIS_DOUBLE_RESOLUTION then
      scale_tr_in[1, 1] := rpt1.Y / rpt2.Y
    else
      scale_tr_in[1, 1] := 1 ;
    //set but not read
    if _isCW then
      cw_scY :=  scale_tr_in[1, 1]
    else
      ccw_scY :=  scale_tr_in[1, 1] ;
    //shearing
    shear[0, 0] := 1 ;
    if Abs(rpt1.Y) > GIS_DOUBLE_RESOLUTION then
      shear[0, 1] := (rpt1.X -rpt2.X*scX)/rpt1.Y
    else
      shear[0, 1] := 0 ;

    if Abs(shear[0, 1]) <= GIS_DOUBLE_RESOLUTION then
      shear[0, 1] := 0 ;
    shear[1, 0] := 0 ;
    shear[1, 1] := 1 ;

    //set but not read
    if _isCW then
      cw_shX := shear[0, 1]
    else
      ccw_shX := shear[0, 1] ;

    Result := matircesMultiplication(scale_tr_in, rottr) ;
    wm     := matircesMultiplication(shear, Result) ;
    Result := matircesMultiplication(rotin, wm) ;

    if Abs(Result[0, 0]) < GIS_DOUBLE_RESOLUTION then
      Result[0, 0] := 0 ;
    if Abs(Result[0, 1]) < GIS_DOUBLE_RESOLUTION then
      Result[0, 1] := 0 ;
    if Abs(Result[1, 0]) < GIS_DOUBLE_RESOLUTION then
      Result[1, 0] := 0 ;
    if Abs(Result[1, 1]) < GIS_DOUBLE_RESOLUTION then
      Result[1, 1] := 0 ;
  end ;
begin

  mpi := inPoints[cwPart[0]] ;
  bpi.X := inPoints[cwPart[1]].X -mpi.X ;
  bpi.Y := inPoints[cwPart[1]].Y -mpi.Y ;

  mpt := _TGIS_Point(trPoints[cwPart[0]]) ;
  bpt.X := trPoints[cwPart[1]].X -mpt.X ;
  bpt.Y := trPoints[cwPart[1]].Y -mpt.Y ;

  inangle := calcAngle(bpi) ;
  trangle := calcAngle(bpt) ;

  //rotation of transformed points
  SinCos( trangle, sintr, costr ) ;

  rottr[0, 0] :=  costr ;
  rottr[0, 1] :=  sintr ;
  rottr[1, 0] := -sintr ;
  rottr[1, 1] :=  costr ;

  //rotation to input points
  sinin := Sin( inangle ) ;
  cosin := Cos( sinin   ) ;

  rotin[0, 0] :=  cosin ;
  rotin[0, 1] := -sinin ;
  rotin[1, 0] :=  sinin ;
  rotin[1, 1] :=  cosin ;

  //scaling transformed X to input X
  wpt1 := _TGIS_Point(inPoints[cwPart[1]]) ;
  wpt1.X := wpt1.X -mpi.X ;
  wpt1.Y := wpt1.Y -mpi.Y ;

  wpt2 := trPoints[cwPart[1]] ;
  wpt2.X := wpt2.X -mpt.X ;
  wpt2.Y := wpt2.Y -mpt.Y ;

  xin0 := wpt1.X*cosin +wpt1.Y*sinin ;
  xtr0 := wpt2.X*costr +wpt2.Y*sintr ;

  scX := xin0/xtr0 ;

  cwTrMatrix  := calcMatrix(cwPart[2], True) ;
  ccwTrMatrix := calcMatrix(ccwPart[2], False) ;

end ;

function T_TransClass.inputPoint(
  const _trPt : TGIS_Point;
  var   _inPt : TGIS_Point
) : Boolean ;
var
  pt     : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
  p1, p2 : TGIS_Point ;
  i      : Integer ;
begin
  Result := True ;
  pt.X := _trPt.X ;//-mpt.X ;
  pt.Y := _trPt.Y ;//-mpt.Y ;
  for i := 0 to 2 do begin
    p1 := trPoints[ccwPart[i]] ;
    if i < 2 then
      p2 := trPoints[ccwPart[i +1]]
    else
      p2 := trPoints[ccwPart[0]] ;
    if area2(p1, p2, pt) < 0 then begin
      Result := False ;
      break ;
    end ;
  end ;
  if not Result then begin
    Result := True ;
    for i := 0 to 2 do begin
      p1 := trPoints[cwPart[i]] ;
      if i < 2 then
        p2 := trPoints[cwPart[i +1]]
      else
        p2 := trPoints[cwPart[0]] ;
      if area2(p1, p2, pt) > 0 then begin
        Result := False ;
        break ;
      end ;
    end ;
  end
  else begin
    pt.X := pt.X -mpt.X ;
    pt.Y := pt.Y -mpt.Y ;

    _inPt.X := pt.X*ccwTrMatrix[0, 0] + pt.Y*ccwTrMatrix[0, 1] + mpi.X ;
    _inPt.Y := pt.X*ccwTrMatrix[1, 0] + pt.Y*ccwTrMatrix[1, 1] + mpi.Y ;

    exit ;
  end ;

  if Result then begin
    pt.X := pt.X -mpt.X ;
    pt.Y := pt.Y -mpt.Y ;

    _inPt.X := pt.X*cwTrMatrix[0, 0] + pt.Y*cwTrMatrix[0, 1] + mpi.X ;
    _inPt.Y := pt.X*cwTrMatrix[1, 0] + pt.Y*cwTrMatrix[1, 1] + mpi.Y ;
  end ;
end ;

procedure T_TransClass.calcDataParams(
const _inImageExtent : TGIS_Extent ;
var   _iowidth       : Integer     ;
var   _ioheight      : Integer
) ;
var
  scrpx, scrpy : Double ;
  scrnx, scrny : Double ;
  scinx, sciny : Double ;
  screcx, screcy : Double ;
  whw : Double ;
  ur_p_extent        : TGIS_Extent ;
  p_ur_up_extent     : TGIS_Extent ;
  r_p_ur_up_extent   : TGIS_Extent ;
  do_rot  : Boolean ;
  do_proj : Boolean ;
  lcs : TGIS_CSCoordinateSystem ;
  pcs : TGIS_CSProjectedCoordinateSystem ;
  cm  : Double ;
  saeved_rect : TRect ;
  lextent, lpextent : TGIS_Extent ;
  nc : Boolean ;
  max_count : Integer ;
  cut_common_ext : TGIS_Extent ;
  cut_ext : TGIS_Extent ;

  ptg1 : TGIS_Point ;
  icc : Integer ;
  lw, lh : Double ;
  cutpolygon_ext : TGIS_ShapePolygon ;
  tiowidth  : Integer ;
  tioheight : Integer ;
  miowidth  : Integer ;
  mioheight : Integer ;
  prext   : TGIS_Extent ;
  bprext  : TGIS_Extent ;
  procedure set_ext_drawarea ;
  var
    lwd : Double ;
    lmaxpix : Integer ;
    lm, lw : Integer ;
    lgrx, lgry : Double ;
    lscx, lscy : Double ;
  const
    SAFE_M = (HPIXELS +1) div 2 ;
  begin
    lscx := ( player.FExtent.XMax - player.FExtent.XMin ) / player.BitWidth  ;
    lscy := ( player.FExtent.YMax - player.FExtent.YMin ) / player.BitHeight  ;

    lgrx := lscx * HPIXELS ;
    lgry := lscy * VPIXELS ;
    if ur_up_extent.XMin > lextent.XMin then begin
      lmaxpix := TruncS((ur_up_extent.XMin - lextent.XMin)/lscx) ;
      if lmaxpix > 0 then begin
        lw := TruncS((ur_up_extent.XMin - lextent.XMin)/lgrx) ;
        if lw = 0 then begin
          ur_up_extent.XMin := lextent.XMin ;
        end
        else begin
          lwd := lextent.XMin +lw*lgrx ;
          lm := TruncS((ur_up_extent.XMin - lwd)/lscx) ;
          if lm >= SAFE_M then
            ur_up_extent.XMin := lwd
          else
            ur_up_extent.XMin := lwd -lgrx ;
        end ;
      end ;
    end ;

    if lextent.XMax > ur_up_extent.XMax   then begin
      lw := TruncS((lextent.XMax - ur_up_extent.XMax )/lscx) ;
      if lw >= SAFE_M then
        ur_up_extent.XMax := ur_up_extent.XMax +SAFE_M*lscx
      else
       ur_up_extent.XMax := lextent.XMax ;
    end ;

    if ur_up_extent.YMin > lextent.YMin then begin
      lmaxpix := TruncS((ur_up_extent.YMin - lextent.YMin)/lscy) ;
      if lmaxpix > 0 then begin
        lw := TruncS((ur_up_extent.YMin - lextent.YMin)/lgry) ;
        if lw = 0 then begin
          ur_up_extent.YMin := lextent.YMin ;
        end
        else begin
          lwd := lextent.YMin +lw*lgry ;
          lm := TruncS((ur_up_extent.YMin - lwd)/lscy) ;
          if lm >= SAFE_M then
            ur_up_extent.YMin := lwd
          else
            ur_up_extent.YMin := lwd -lgry ;
        end ;
      end ;
    end ;

    if lextent.YMax > ur_up_extent.YMax   then begin
      lw := TruncS((lextent.YMax - ur_up_extent.YMax )/lscy) ;
      if lw >= SAFE_M then
        ur_up_extent.YMax := ur_up_extent.YMax +SAFE_M*lscy
      else
       ur_up_extent.YMax := lextent.YMax ;
    end ;
  end ;

  function check_set_ur_up_extent : Boolean ;
  var
    rpext : TGIS_Extent ;
    dxl, dxr, dx, udx : Double ;
    dyb, dyt, dy, udy : Double ;
    pwl, phl : Double ;
    n : Integer ;
  begin
    Result := True ;
    rpext := projectedExt( ur_up_extent ) ;

    dx := _inImageExtent.XMax -_inImageExtent.XMin ;
    udx := ur_up_extent.XMax - ur_up_extent.XMin ;
    dy := _inImageExtent.YMax -_inImageExtent.YMin ;
    udy := ur_up_extent.YMax - ur_up_extent.YMin ;

    if rpext.XMin > _inImageExtent.XMin then begin
      dxl := rpext.XMin - _inImageExtent.XMin ;
    end
    else
      dxl := 0 ;

    if rpext.XMax < _inImageExtent.XMax then begin
      dxr := _inImageExtent.XMax -rpext.XMax;
    end
    else
      dxr := 0 ;
    if dxl <> 0 then begin
      Result := False ;
      ur_up_extent.XMin := ur_up_extent.XMin -udx*dxl/(dx -dxr) ;
    end ;

    if dxr <> 0 then begin
      Result := False ;
      ur_up_extent.XMax := ur_up_extent.XMax +udx*dxr/(dx -dxl) ;
    end ;

    if rpext.YMin > _inImageExtent.YMin then begin
      dyb := rpext.YMin - _inImageExtent.YMin ;
    end
    else
      dyb := 0 ;

    if rpext.YMax < _inImageExtent.YMax then begin
      dyt := _inImageExtent.YMax -rpext.YMax;
    end
    else
      dyt := 0 ;

    if dyb <> 0 then begin
      Result := False ;
      ur_up_extent.YMin := ur_up_extent.YMin -udy*dyb/(dy -dyt) ;
    end ;

    if dyt <> 0 then begin
      Result := False ;
      ur_up_extent.YMax := ur_up_extent.YMax +udy*dyt/(dy -dyb) ;
    end ;
    if Result then begin
      pwl := (player.FExtent.XMax -player.FExtent.XMin)/player.baseCellWidth ;
      phl := (player.FExtent.YMax -player.FExtent.YMin)/player.baseCellHeight ;
      n := 100 ;
      ur_up_extent.XMax := ur_up_extent.XMax +n*pwl ;
      ur_up_extent.XMin := ur_up_extent.XMin -n*pwl ;

      ur_up_extent.YMax := ur_up_extent.YMax +n*phl ;
      ur_up_extent.YMin := ur_up_extent.YMin -n*phl ;
    end ;

  end ;

begin

 lextent  := _TGIS_Extent(player.FExtent) ;

 if GisIsSameValue( lextent.YMax, lextent.YMin ) or
    GisIsSameValue( lextent.XMax, lextent.XMin )
 then
   exit ;

  if (_ioheight <= 0) or (_iowidth <= 0) then
    exit ;

  prext := projectedExt(player.FExtent) ;
  bprext := prext ;
  if player.forViewer then
    prext := player.Viewer.Ref.RotatedExtent(prext) ;
  lpextent := _TGIS_Extent(prext) ;


  do_proj := player.projectionNeed or baseRotation or player.activeTransform
              or internalRotation ;
  do_rot  := player.rotationNeed ;

  scrpy := -player.scaleY ;
  scrpx :=  player.scaleX ;

  r_p_extent := _TGIS_Extent(_inImageExtent) ;

  scinx := (r_p_extent.XMax - r_p_extent.XMin )/_iowidth ;
  sciny := (r_p_extent.YMax - r_p_extent.YMin)/_ioheight ;

  scrny := (player.FExtent.YMax -player.FExtent.YMin)
         /player.BitHeight ;
  scrnx := (player.FExtent.XMax -player.FExtent.XMin)
         /player.BitWidth ;

  if not do_rot then begin
    ur_p_extent := _TGIS_Extent(r_p_extent) ;
  end
  else begin
    ur_p_extent := player.Viewer.Ref.UnrotatedExtent( r_p_extent ) ;
    ur_p_extent := GisCommonExtent(ur_p_extent, bprext) ;
  end ;
  pcols_max := RoundS((1.0*_iowidth) / HPIXELS) ;
  prows_max := RoundS((1.0*_ioheight)/ VPIXELS) ;

  if do_proj then begin
    ur_up_extent := unprojectedExt( ur_p_extent ) ;
    if ( ur_up_extent.XMin > 1e38 ) or
       ( ur_up_extent.YMin > 1e38 ) or
       ( ur_up_extent.XMax > 1e38 ) or
       ( ur_up_extent.XMax > 1e38 )
    then
      exit ;

    if ur_up_extent.XMin > ur_up_extent.XMax then
      exit ;

    lcs := player.outCS ;
    cm := 0 ;
    if lcs is TGIS_CSProjectedCoordinateSystem then begin
      pcs := TGIS_CSProjectedCoordinateSystem( lcs ) ;
      if not IsNan( pcs.Projection.CentralMeridian ) then begin
        if (pcs.Projection.CentralMeridian <> 0) and
          ((Abs(lextent.XMin) <= 189) and (Abs(lextent.XMax) <= 189))
        then begin  //Transforming in two parts
          cm := ( pcs.Projection.CentralMeridian * 180 ) / Pi ;
        end ;
      end ;
    end ;

  {
    uw01 := 0.1*(ur_up_extent.XMax -ur_up_extent.XMin) ;
    uh01 := 0.1*(ur_up_extent.YMax -ur_up_extent.YMin) ;
    ur_up_extent.XMin := ur_up_extent.XMin -uw01 ;
    ur_up_extent.XMax := ur_up_extent.XMax +uw01 ;
    ur_up_extent.YMin := ur_up_extent.YMin -uh01 ;
    ur_up_extent.YMax := ur_up_extent.YMax +uh01 ;
   }
    if cm <> 0 then begin
      if ur_up_extent.XMin < (cm -180) then
        ur_up_extent.XMin := (cm -180) ;
      if ur_up_extent.XMax > (cm +180) then
        ur_up_extent.XMax := (cm +180) ;
    end ;

    ur_up_extent := GisCommonExtent(lextent, ur_up_extent) ;
    if player.activeTransform then begin

      max_count := 0 ;
      repeat
        nc := check_set_ur_up_extent ;
        inc(max_count) ;
        if max_count >= 10 then
          break ;
      until nc ;
      ur_up_extent := GisCommonExtent(lextent, ur_up_extent) ;
    end

  end
  else
    ur_up_extent := _TGIS_Extent(ur_p_extent) ;

  set_ext_drawarea ;

  pw := (r_p_extent.XMax -r_p_extent.XMin)/_iowidth ;
  ph := (r_p_extent.YMax -r_p_extent.YMin)/_ioheight ;

  dxx := r_p_extent.XMin ;
  dyy := r_p_extent.YMin ;

  if do_proj then begin
    p_ur_up_extent := projectedExt( ur_up_extent ) ;
    p_ur_up_extent := GisCommonExtent( p_ur_up_extent, bprext) ;
  end
  else
    p_ur_up_extent := _TGIS_Extent(ur_up_extent) ;

  if do_rot then begin
    r_p_ur_up_extent := player.Viewer.Ref.RotatedExtent(p_ur_up_extent) ;
    r_p_ur_up_extent := GisCommonExtent(r_p_ur_up_extent, lpextent) ;
  end
  else
    r_p_ur_up_extent := _TGIS_Extent(p_ur_up_extent) ;

  dxx := r_p_ur_up_extent.XMin ;
  dyy := r_p_ur_up_extent.YMin ;
  h := player.BitHeight/ (lextent.YMax -lextent.YMin) ;
  w := player.BitWidth / (lextent.XMax -lextent.XMin) ;

  in_img_rect := Rect(
                       TruncS((ur_up_extent.XMin -lextent.XMin)*w),
                       TruncS((lextent.YMax -ur_up_extent.YMax)*h),
                       RoundS((ur_up_extent.XMax -lextent.XMin)*w) -1,
                       RoundS((lextent.YMax -ur_up_extent.YMin)*h) - 1
                     ) ;

  if (in_img_rect.Left   >= player.BitWidth ) or
     (in_img_rect.Right  <= 0                    ) or
     (in_img_rect.Top    >= player.BitHeight) or
     (in_img_rect.Bottom <  0                    )
  then begin
    iowidth  := 0 ;
    ioheight := 0 ;
    exit ;
  end
  else begin
    if player.forViewer then begin
      screcx := (scrnx*scinx)/scrpx ;
      screcy := (scrny*sciny)/scrpy ;
    end
    else begin
      screcx := scrpx ;
      screcy := scrpy ;
    end;
    iowidth   := RoundS((ur_up_extent.XMax -ur_up_extent.XMin)/screcx) ;
    ioheight  := RoundS((ur_up_extent.YMax -ur_up_extent.YMin)/screcy) ;

    if (iowidth <= 0) or (ioheight <= 0) then begin
      iowidth := _iowidth ;
      ioheight := _ioheight ;
    end
    else
    if (iowidth > (2*_iowidth)) and (ioheight > (2*_ioheight))  then begin
      whw := iowidth/ioheight ;
      iowidth := _iowidth ;
      ioheight := RoundS(iowidth/whw) ;
    end
    else begin
      tiowidth   := RoundS(((r_p_ur_up_extent.XMax -r_p_ur_up_extent.XMin)/
                         (r_p_extent.XMax -r_p_extent.XMin))*
                          _iowidth);
      tioheight  := RoundS(((r_p_ur_up_extent.YMax -r_p_ur_up_extent.YMin)/
                         (r_p_extent.YMax -r_p_extent.YMin))*
                          _ioheight); ;

      if (iowidth > tiowidth) and (ioheight > tioheight) then begin
        if (iowidth > _iowidth) or (iowidth = 0) then
          iowidth := in_img_rect.Right -in_img_rect.Left +1;
        if iowidth > player.BitWidth then
          iowidth := player.BitWidth
        else
        if (iowidth > tiowidth) or (iowidth < _iowidth) then
          iowidth := _iowidth ;
        if (ioheight > _ioheight)  or (ioheight = 0) then
          ioheight := in_img_rect.Bottom -in_img_rect.Top +1;
        if ioheight > player.BitHeight then
          ioheight := player.BitHeight
        else
        if (ioheight > tioheight) or (ioheight < _ioheight) then
          ioheight := _ioheight ;
      end ;
    end;

    if do_rot then begin
      miowidth  := RoundS(_iowidth*1.315) ;
      mioheight := RoundS(_ioheight*1.315) ;
      if (iowidth > miowidth) and (ioheight > mioheight) then begin
        if iowidth > ioheight then begin
          ioheight := mioheight ;
          iowidth := RoundS(ioheight*((ur_up_extent.XMax -ur_up_extent.XMin)/
                                      (ur_up_extent.YMax -ur_up_extent.YMin))) ;
        end
        else begin
          iowidth  := miowidth ;
          ioheight := RoundS(iowidth*((ur_up_extent.YMax -ur_up_extent.YMin)/
                                      (ur_up_extent.XMax -ur_up_extent.XMin))) ;
        end;
      end ;
    end ;

    if (iowidth <= 0) or (ioheight <= 0) then
      exit ;
    pcols_max := RoundS((1.0*iowidth) / HPIXELS) ;
    prows_max := RoundS((1.0*ioheight)/ VPIXELS) ;
  end ;

  if GisIsSameValue( pw, 0 ) or GisIsSameValue( ph, 0 ) then
  begin
    exit ;
  end ;

  ix := RoundS(((r_p_extent.XMin -r_p_ur_up_extent.XMin))/pw) ;
  iy := RoundS(((r_p_extent.YMin)/ph) -0.5) -RoundS(((r_p_ur_up_extent.YMin)/ph) -0.5) ;

  if ( prows_max +1 = 0 ) or ( pcols_max +1 = 0 ) then
  begin
    exit ;
  end ;

  cy := (ur_up_extent.YMax -ur_up_extent.YMin)/(prows_max +1) ;
  cx := (ur_up_extent.XMax -ur_up_extent.XMin)/(pcols_max +1) ;

  if ( ioheight = 0 ) or ( iowidth = 0 ) then
  begin
    exit ;
  end ;

  h := (ur_up_extent.YMax -ur_up_extent.YMin)/ioheight ;
  w := (ur_up_extent.XMax -ur_up_extent.XMin)/iowidth ;

  maxrow_out := RoundS((r_p_ur_up_extent.YMax -r_p_ur_up_extent.YMin)/ph) ;
  maxcol_out := RoundS((r_p_ur_up_extent.XMax -r_p_ur_up_extent.XMin)/pw) ;

  w4b_in := (((iowidth  -1 +2)*3) div 4) * 4 ;

  saeved_rect := player.workSrcRect ;

  if ( iowidth  / ( in_img_rect.Right - in_img_rect.Left +1 ) )
     >
     ( ioheight / ( in_img_rect.Bottom - in_img_rect.Top +1 ) )
  then
    player.extZoom :=
      player.setFileScale(iowidth, in_img_rect.Right - in_img_rect.Left +1)
  else
    player.extZoom :=
      player.setFileScale(ioheight, in_img_rect.Bottom - in_img_rect.Top +1) ;

  player.workSrcRect := saeved_rect ;
  if player.extZoom < 1 then begin
    if in_img_rect.Right = (player.baseCellWidth -1) then
      in_img_rect := Rect(
                           TruncS(in_img_rect.Left*player.extZoom),
                           TruncS(in_img_rect.Top*player.extZoom),
                           TruncS(in_img_rect.Right*player.extZoom),
                           RoundS(in_img_rect.Bottom*player.extZoom)
                         )
    else
      in_img_rect := Rect(
                           TruncS(in_img_rect.Left*player.extZoom),
                           TruncS(in_img_rect.Top*player.extZoom),
                           RoundS(in_img_rect.Right*player.extZoom),
                           RoundS(in_img_rect.Bottom*player.extZoom)
                         ) ;
  end ;
  if player.extZoom = 1 then
    if in_img_rect.Right >= player.baseCellWidth then
      in_img_rect := Rect(
                           in_img_rect.Left,
                           in_img_rect.Top,
                           player.baseCellWidth -1,
                           in_img_rect.Bottom
                         ) ;
  cutsize := 0 ;
  if assigned(cutpolygon) then begin
    cut_ext := _TGIS_Extent(cutpolygon.ProjectedExtent) ;
    cut_common_ext := GisCommonExtent(cut_ext, r_p_extent) ;

    xcutv := iowidth  / (r_p_extent.XMax -r_p_extent.XMin) ;
    ycutv := ioheight / (r_p_extent.YMax -r_p_extent.YMin) ;

    if (cutpolygon.ProjectedExtent.XMin < r_p_extent.XMin) or
       (cutpolygon.ProjectedExtent.XMax > r_p_extent.XMax) or
       (cutpolygon.ProjectedExtent.YMin < r_p_extent.YMin) or
       (cutpolygon.ProjectedExtent.YMax > r_p_extent.YMax)
    then
      cutpolygon_ext := TGIS_ShapePolygon(cutpolygon.GetIntersection(cut_common_ext, False))
    else
      cutpolygon_ext := cutpolygon ;

    if not assigned( cutpolygon_ext ) then begin
      _iowidth := 0 ;
      _ioheight := 0 ;
      exit ;
    end ;

    cut_ext := TGIS_Extent(cutpolygon_ext.Extent) ;
    xcutsize := RoundS((cut_ext.XMax -cut_ext.XMin)*xcutv) ;
    ycutsize := RoundS((cut_ext.YMax -cut_ext.YMin)*ycutv) ;

    cutsize := Max(xcutsize +50, ycutsize +50) ;
    cutsize := Max(cutsize, 512) ;

    cutpolygon_ext.PrepareContourInternal(cutsize, false, cutpixels, cutscale, cutoffset) ;

    xcutidx := RoundS( (cut_ext.XMin -r_p_extent.XMin ) * cutscale ) -
               cutoffset.X;
    ycutidx := RoundS( (r_p_extent.YMax -cut_ext.YMax ) * cutscale ) -
               cutoffset.Y;

    SetLength(cut_idx_y, _ioheight) ;
    SetLength(cut_idx_x, _iowidth) ;

    lh := (r_p_extent.YMax - r_p_extent.YMin) / _ioheight ;
    lw := (r_p_extent.XMax - r_p_extent.XMin) / _iowidth ;


    ptg1 := GisPoint( r_p_extent.XMin, r_p_extent.YMax ) ;

    for icc := low(cut_idx_y) to high(cut_idx_y) do begin
      ycutv := ptg1.Y -icc*lh ;
      if ( ycutv >= ( cut_ext.YMax + lh/2 ) ) // half pixel tolerance
         or
         ( ycutv <= ( cut_ext.YMin - lh/2 ) )
      then
        cut_idx_y[_ioheight -icc -1] := -1
      else
        cut_idx_y[_ioheight -icc -1]  :=
              RoundS( ( cut_ext.YMax - ycutv) * cutscale ) + cutoffset.Y ;
    end ;


    for icc := low(cut_idx_x) to high(cut_idx_x) do begin
      xcutv := ptg1.X +icc*lw ;
      if ( xcutv >=  (cut_ext.XMax + lw/2 ) ) // half pixel tolerance
         or
         ( xcutv <= ( cut_ext.XMin - lw/2 ) )
      then
        cut_idx_x[icc] := -1
      else
      cut_idx_x[icc] :=
            RoundS( ( xcutv  -cut_ext.XMin) * cutscale ) + cutoffset.X ;
    end ;

    if cutpolygon_ext <> cutpolygon then
      FreeObject(cutpolygon_ext) ;
  end ;
end ;

function T_TransClass.getTransformedGridData(
  const _bufOut        : TGIS_GridArray ;
  const _inImageExtent : TGIS_Extent    ;
  var   _iowidth       : Integer        ;
  var   _ioheight      : Integer
) : Boolean ;
const
  TRANSP_LEVEL = 0.5 ;
var
  bufin : TGIS_GridArray ;

  i, k, l, m : Integer ;
  psd : T_PointSet ;
  pto : TGIS_Point {$IFDEF GIS_NORECORDS} = new TGIS_Point{$ENDIF} ;
  pti : TGIS_Point {$IFDEF GIS_NORECORDS} = new TGIS_Point{$ENDIF} ;
  col_in, row_in : Integer ;

  //used for anti-aliasing
  col_in_a, row_in_a : Integer ;
  d_col_in, d_row_in : Double ;
  d_w_00, d_w_01 : Double ;
  d_w_10, d_w_11 : Double ;
  bd_w_00, bd_w_01 : Double ;
  bd_w_10, bd_w_11 : Double ;
  s1 : Single ;

  transp_fact : Double ;
  d_w_t, d_w_i2 : Double ;
  cni : Integer ;
begin
  Result := True ;

  maxrow_out := _ioheight -1 ;
  maxcol_out := _iowidth  -1 ;
  calcDataParams( _inImageExtent, _iowidth, _ioheight ) ;
  if (ioheight <= 0) or (iowidth <= 0) then
    exit ;

  bufin := InitializeGrid(ioheight, iowidth) ;
  player.setNoDataTable(bufin) ;

  Result := player.getGridData( ur_up_extent, bufin ) ;

  {$IFDEF GIS_NORECORDS}
    psd[0] := new TGIS_Point;
    psd[1] := new TGIS_Point;
    psd[2] := new TGIS_Point;
    psd[3] := new TGIS_Point;
  {$ENDIF}

  // Big loop begin

  for l := 0 to prows_max do begin
    for m := 0 to pcols_max do begin

      psd[0].X := m*cx +ur_up_extent.XMin;
      if m > 0 then
        psd[0].X := psd[0].X -w ;

      psd[0].Y := ur_up_extent.YMin + l*cy;
      if l > 0 then
        psd[0].Y := psd[0].Y -h ;

      psd[1].X := psd[0].X ;
      psd[1].Y := ur_up_extent.YMin +(l +1)*cy;
      if l < prows_max then
        psd[1].Y := psd[1].Y +h ;

      if psd[1].Y > ur_up_extent.YMax then
        psd[1].Y := ur_up_extent.YMax ;

      psd[2].X := (m +1)*cx  +ur_up_extent.XMin;
      if m < pcols_max then
        psd[2].X := psd[2].X +w ;
      if psd[2].X > ur_up_extent.XMax then
        psd[2].X := ur_up_extent.XMax ;
      psd[2].Y := psd[1].Y ;

      psd[3].X := psd[2].X ;
      psd[3].Y := psd[0].Y ;

      if not setTransform(psd) then
        continue ;

      for i := row_start to row_stop do begin   //filling with image pixels

        if true then begin
          ni := _ioheight -(i -iy) -1 ;

          if ni < 0 then
            continue
          else
          if ni >= _ioheight then
            continue ;
          cni := _ioheight -ni -1;

          pto.Y := (i -row_start) ; //i

          for k := col_start to col_stop do begin
            if true then begin
              pto.X :=(k -col_start) ;

              if inputPoint(pto, pti) then begin
                row_in := ioheight -RoundS(pti.Y) -1;//-iy;
                col_in := RoundS(pti.X) ;//-ix;
                if (row_in >= 0) and (row_in < ioheight) then begin
                  if (col_in >= 0) and (col_in <iowidth) then begin
                    nk := k -ix ;
                    if nk < 0 then
                      continue ;

                    if (nk < _iowidth) and (ni < _ioheight) then begin

                      if assigned(cutpixels) then begin
                        if cut_idx_x[nk] = -1 then
                          continue ;

                       if cut_idx_y[cni] = -1 then
                          continue ;


                        if(cutpixels[cut_idx_y[cni]*cutsize +cut_idx_x[nk]] = -1) then
                          continue ;
                      end ;

                      d_row_in := row_in +pti.Y -ioheight +1 ;
                      d_col_in := col_in -pti.X ;

                      if d_row_in > 0 then begin
                        row_in_a := row_in -1 ;
                        if row_in_a < 0 then begin
                          row_in_a := row_in ;
                          bd_w_10 := 1 ;
                          bd_w_11 := 0 ;
                        end
                        else begin
                          bd_w_10 := 1 -d_row_in ;
                          bd_w_11 := d_row_in ;
                        end ;
                      end
                      else
                      if d_row_in < 0 then begin
                        row_in_a := row_in +1 ;
                        if row_in_a >= ioheight then begin
                          row_in_a := row_in ;
                          bd_w_10 := 1 ;
                          bd_w_11 := 0 ;
                        end
                        else begin
                          bd_w_10 := 1 +d_row_in ;
                          bd_w_11 := -d_row_in ;
                        end ;
                      end
                      else begin
                        row_in_a := row_in ;
                        bd_w_10 := 1 ;
                        bd_w_11 := 0 ;
                      end ;

                      if d_col_in > 0 then begin
                        col_in_a := col_in -1 ;
                        if col_in_a < 0 then begin
                          col_in_a := col_in ;
                          bd_w_00 := 1 ;
                          bd_w_01 := 0 ;
                        end
                        else begin
                          bd_w_00 := 1 -d_col_in ;
                          bd_w_01 := d_col_in ;
                        end ;
                      end
                      else
                      if d_col_in < 0 then begin
                        col_in_a := col_in +1 ;
                        if col_in_a >= iowidth then begin
                          col_in_a := col_in ;
                          bd_w_00 := 1 ;
                          bd_w_01 := 0 ;
                        end
                        else begin
                          bd_w_00 := 1 +d_col_in ;
                          bd_w_01 := -d_col_in ;
                        end ;
                      end
                      else begin
                        col_in_a := col_in ;
                        bd_w_00 := 1 ;
                        bd_w_01 := 0 ;
                      end ;
                      d_w_00 := bd_w_00 * bd_w_10 ;
                      d_w_01 := bd_w_01 * bd_w_10 ;
                      d_w_10 := bd_w_11 * bd_w_00 ;
                      d_w_11 := bd_w_11 * bd_w_01 ;

                      transp_fact := 0 ;

                      {$IFDEF OXYGENE}
                        if ( bufin[row_in][col_in] = GIS_GRID_NOVALUE ) then
                      {$ELSE}
                        if ( bufin[row_in, col_in] = GIS_GRID_NOVALUE ) then
                      {$ENDIF}
                      begin
                        if d_w_00 > 0 then begin
                          d_w_t := d_w_01 + d_w_10 + d_w_11 ;
                          if d_w_t > 0 then begin
                            d_w_i2 := d_w_00/d_w_t ;
                            d_w_01 := d_w_01 + (d_w_i2*d_w_01) ;
                            d_w_10 := d_w_10 + (d_w_i2*d_w_10) ;
                            d_w_11 := d_w_11 + (d_w_i2*d_w_11) ;
                          end ;
                          transp_fact := transp_fact +d_w_00 ;
                          if transp_fact > TRANSP_LEVEL then
                            continue ;
                          d_w_00 := 0 ;
                        end ;

                      end ;
                      {$IFDEF OXYGENE}
                        if ( bufin[row_in][col_in_a] = GIS_GRID_NOVALUE ) then
                      {$ELSE}
                        if ( bufin[row_in, col_in_a] = GIS_GRID_NOVALUE ) then
                      {$ENDIF}
                      begin
                        if d_w_01 > 0 then begin
                          d_w_t := d_w_00 + d_w_10 + d_w_11 ;
                          if d_w_t > 0 then begin
                            d_w_i2 := d_w_01/d_w_t ;
                            d_w_00 := d_w_00 + (d_w_i2*d_w_00) ;
                            d_w_10 := d_w_10 + (d_w_i2*d_w_10) ;
                            d_w_11 := d_w_11 + (d_w_i2*d_w_11) ;
                          end ;
                          transp_fact := transp_fact +d_w_01 ;
                          if transp_fact > TRANSP_LEVEL then
                            continue ;
                          d_w_01 := 0 ;
                        end ;

                      end ;
                      {$IFDEF OXYGENE}
                        if ( bufin[row_in_a][col_in] = GIS_GRID_NOVALUE ) then
                      {$ELSE}
                        if ( bufin[row_in_a, col_in] = GIS_GRID_NOVALUE ) then
                      {$ENDIF}
                      begin
                        if d_w_10 > 0 then begin
                          d_w_t := d_w_00 + d_w_01 + d_w_11 ;
                          if d_w_t > 0 then begin
                            d_w_i2 := d_w_10/d_w_t ;
                            d_w_00 := d_w_00 + (d_w_i2*d_w_00) ;
                            d_w_01 := d_w_01 + (d_w_i2*d_w_01) ;
                            d_w_11 := d_w_11 + (d_w_i2*d_w_11) ;
                          end ;
                          transp_fact := transp_fact +d_w_10 ;
                          if transp_fact > TRANSP_LEVEL then
                            continue ;
                          d_w_10 := 0 ;
                        end ;
                      end ;
                      {$IFDEF OXYGENE}
                        if ( bufin[row_in_a][col_in_a] = GIS_GRID_NOVALUE ) then
                      {$ELSE}
                        if ( bufin[row_in_a, col_in_a] = GIS_GRID_NOVALUE ) then
                      {$ENDIF}
                      begin
                        if d_w_11 > 0 then begin
                          d_w_t := d_w_00 + d_w_01 + d_w_10 ;
                          if d_w_t > 0 then begin
                            d_w_i2 := d_w_11/d_w_t ;
                            d_w_00 := d_w_00 + (d_w_i2*d_w_00) ;
                            d_w_01 := d_w_01 + (d_w_i2*d_w_01) ;
                            d_w_10 := d_w_10 + (d_w_i2*d_w_10) ;
                          end ;
                          transp_fact := transp_fact +d_w_11 ;
                          if transp_fact > TRANSP_LEVEL then
                            continue ;
                          d_w_11 := 0 ;
                        end ;
                      end ;

                      s1 :=  bufin[row_in  ][col_in  ]*d_w_00 +
                             bufin[row_in  ][col_in_a]*d_w_01 +
                             bufin[row_in_a][col_in  ]*d_w_10 +
                             bufin[row_in_a][col_in_a]*d_w_11 ;
                      {$IFDEF OXYGENE}
                        _bufOut[ni][nk] := s1 ;
                      {$ELSE}
                        _bufOut[ni, nk] := s1 ;
                      {$ENDIF}

                    end ;
                  end ;
                end ;
              end ;
            end ;
          end ; //cols
        end ;
      end ; //rows
    end ;
  end ;
  //Big loop end

  if assigned(cutpixels) then begin
    cutpixels := nil ;
    cut_idx_x := nil ;
    cut_idx_y := nil ;
  end ;
  bufin := nil ;

end ;

function T_TransClass.getTransformedBitmapData(
  const _bmp            : TGIS_Pixels ;
  const _inImageExtent  : TGIS_Extent;
  var   _iowidth        : Integer;
  var   _ioheight       : Integer
 ) : Boolean ;

var
  bmpin : TGIS_Pixels   ;
  bufin0x  : Integer ;
  bufin1x  : Integer ;
  rowx  : Integer ;

  i, k, l, m : Integer ;
  psd : T_PointSet ;
  pto : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF} ;
  pti : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF} ;
  col_in, row_in : Integer ;

  //used for anti-aliasing
  col_in_a, row_in_a : Integer ;
  d_col_in, d_row_in : Double ;
  d_w_00, d_w_01 : Double ;
  d_w_10, d_w_11 : Double ;
  bd_w_00, bd_w_01 : Double ;
  bd_w_10, bd_w_11 : Double ;
  b1, b2, b3, b4 : Integer ;
  b00, b01, b10, b11 : Integer ;
  idx00, idx01, idx10, idx11 : Integer ;

  inAlpha  : TBytes ;
  outAlpha : TBytes ;
  olidx : Integer ;
  isAlpha : Boolean ;

  pixin00, pixin01 : Integer ;
  pixin10, pixin11 : Integer ;
  pixout : Integer ;

  ready  : Boolean ;
  bempty : Boolean ;
begin
  Result := true ;

  ready := False ;

  inAlpha  := nil ;
  outAlpha := nil ;

  maxrow_out := _ioheight -1 ;
  maxcol_out := _iowidth  -1 ;

  if player.internalTransparentColor.A = 00 then begin
    internalTransparentColor := TGIS_Color.None ;
    player.internalTransparentColor := internalTransparentColor ;
  end ;

  calcDataParams( _inImageExtent, _iowidth, _ioheight ) ;
  if (_ioheight <= 0) or (_iowidth <= 0) then
    exit ;

  if assigned(player.alphaBuffer) then begin
    isAlpha := True ;
    outAlpha := player.alphaBuffer ;
    SetLength(inAlpha, iowidth*ioheight) ;
    player.alphaBuffer := inAlpha ;
  end
  else
    isAlpha := False ;

  {$IFDEF GIS_NORECORDS}
    bmpin := new Integer[iowidth*ioheight] ;
    for i := 0 to 3 do
      psd[i] := new TGIS_Point ;
  {$ELSE}
    SetLength(bmpin, iowidth*ioheight) ;
  {$ENDIF}

  for i := 0 to iowidth*ioheight -1 do
    bmpin[i] := 0 ;

  Result := player.getBitmapData( ur_up_extent,
                                  bmpin,
                                  iowidth,
                                  ioheight
                                ) ;

  bempty := True ;
  for i := 0 to iowidth*ioheight -1 do begin
    if bmpin[i] <> 0 then begin
      bempty := False ;
      break ;
    end;
  end ;
  if bempty then exit ;

  if isAlpha then
    player.alphaBuffer := outAlpha ;

  {$IFDEF GIS_NORECORDS}
    psd[0] := new TGIS_Point;
    psd[1] := new TGIS_Point;
    psd[2] := new TGIS_Point;
    psd[3] := new TGIS_Point;
  {$ENDIF}

  // Big loop begin
  for l := 0 to prows_max do begin
    for m := 0 to pcols_max do begin

      psd[0].X := m*cx +ur_up_extent.XMin;
      if m > 0 then
        psd[0].X := psd[0].X -w ;

      psd[0].Y := ur_up_extent.YMin + l*cy;
      if l > 0 then
        psd[0].Y := psd[0].Y -h ;

      psd[1].X := psd[0].X ;
      psd[1].Y := ur_up_extent.YMin +(l +1)*cy;
      if l < prows_max then
        psd[1].Y := psd[1].Y +h ;

      if psd[1].Y > ur_up_extent.YMax then
        psd[1].Y := ur_up_extent.YMax ;

      psd[2].X := (m +1)*cx  +ur_up_extent.XMin;
      if m < pcols_max then
        psd[2].X := psd[2].X +w ;
      if psd[2].X > ur_up_extent.XMax then
        psd[2].X := ur_up_extent.XMax ;
      psd[2].Y := psd[1].Y ;

      psd[3].X := psd[2].X ;
      psd[3].Y := psd[0].Y ;

      if not setTransform(psd) then
        continue ;

      for i := row_start to row_stop do begin   //filling with image pixels

        if true then begin
          ni :=i -iy ;

          if ni < 0 then
            continue
          else
          if ni >= _ioheight then
            continue ;

          olidx := _ioheight -ni -1 ;
          rowx := olidx*_iowidth ;
          pto.Y := (i -row_start) ; //i

          for k := col_start to col_stop do begin
            if true then begin
              pto.X :=(k -col_start) ;
              if inputPoint(pto, pti) then begin
                row_in := ioheight -RoundS(pti.Y) -1;//-iy;
                col_in := RoundS(pti.X) ;//-ix;

                if (row_in >= 0) and (row_in < ioheight) then begin
                  if (col_in >= 0) and (col_in <iowidth) then begin
                    nk := k -ix ;
                    if nk < 0 then
                      continue ;

                    if (nk < _iowidth) and (ni < _ioheight) then begin
                      if assigned(cutpixels) then begin
                        if cut_idx_x[nk] = -1 then
                          continue ;

                       if cut_idx_y[ni] = -1 then
                          continue ;


                        if(cutpixels[cut_idx_y[ni]*cutsize +cut_idx_x[nk]] = -1) then
                          continue ;
                      end ;

                      d_row_in := row_in +pti.Y -ioheight +1 ;
                      d_col_in := col_in -pti.X ;

                      if d_row_in > 0 then begin
                        row_in_a := row_in -1 ;
                        if row_in_a < 0 then begin
                          row_in_a := row_in ;
                          bd_w_10 := 1 ;
                          bd_w_11 := 0 ;
                        end
                        else begin
                          bd_w_10 := 1 -d_row_in ;
                          bd_w_11 := d_row_in ;
                        end ;
                      end
                      else
                      if d_row_in < 0 then begin
                        row_in_a := row_in +1 ;
                        if row_in_a >= ioheight then begin
                          row_in_a := row_in ;
                          bd_w_10 := 1 ;
                          bd_w_11 := 0 ;
                        end
                        else begin
                          bd_w_10 := 1 +d_row_in ;
                          bd_w_11 := -d_row_in ;
                        end ;
                      end
                      else begin
                        row_in_a := row_in ;
                        bd_w_10 := 1 ;
                        bd_w_11 := 0 ;
                      end ;

                      if d_col_in > 0 then begin
                        col_in_a := col_in -1 ;
                        if col_in_a < 0 then begin
                          col_in_a := col_in ;
                          bd_w_00 := 1 ;
                          bd_w_01 := 0 ;
                        end
                        else begin
                          bd_w_00 := 1 -d_col_in ;
                          bd_w_01 := d_col_in ;
                        end ;
                      end
                      else
                      if d_col_in < 0 then begin
                        col_in_a := col_in +1 ;
                        if col_in_a >= iowidth then begin
                          col_in_a := col_in ;
                          bd_w_00 := 1 ;
                          bd_w_01 := 0 ;
                        end
                        else begin
                          bd_w_00 := 1 +d_col_in ;
                          bd_w_01 := -d_col_in ;
                        end ;
                      end
                      else begin
                        col_in_a := col_in ;
                        bd_w_00 := 1 ;
                        bd_w_01 := 0 ;
                      end ;
                      d_w_00 := bd_w_00 * bd_w_10 ;
                      d_w_01 := bd_w_01 * bd_w_10 ;
                      d_w_10 := bd_w_11 * bd_w_00 ;
                      d_w_11 := bd_w_11 * bd_w_01 ;

                      idx00 := col_in   ;
                      idx01 := col_in_a ;
                      bufin0x := iowidth*row_in ;

                      idx10 := col_in   ;
                      idx11 := col_in_a ;
                      bufin1x := iowidth*row_in_a ;

                      if isAlpha then
                        outAlpha[_iowidth*olidx +nk] :=
                          inAlpha[iowidth*row_in +col_in] ;

                      pixin00 := bmpin[bufin0x +idx00] ;
                      pixin01 := bmpin[bufin0x +idx01] ;
                      pixin10 := bmpin[bufin1x +idx10] ;
                      pixin11 := bmpin[bufin1x +idx11] ;

                      b00 := pixin00 and $FF ;
                      b01 := pixin01 and $FF ;
                      b10 := pixin10 and $FF ;
                      b11 := pixin11 and $FF ;
                      b1 := RoundS(b00*d_w_00 +
                                   b01*d_w_01 +
                                   b10*d_w_10 +
                                   b11*d_w_11  ) ;
                      if b1 > 255 then
                        b1 := 255 ;

                      b00 := (pixin00 shr 8) and $FF ;
                      b01 := (pixin01 shr 8) and $FF ;
                      b10 := (pixin10 shr 8) and $FF ;
                      b11 := (pixin11 shr 8) and $FF ;
                      b2 := RoundS(b00*d_w_00 +
                                   b01*d_w_01 +
                                   b10*d_w_10 +
                                   b11*d_w_11  ) ;
                      if b2 > 255 then
                        b2 := 255 ;
                      b00 := (pixin00 shr 16) and $FF ;
                      b01 := (pixin01 shr 16) and $FF ;
                      b10 := (pixin10 shr 16) and $FF ;
                      b11 := (pixin11 shr 16) and $FF ;
                      b3 := RoundS(b00*d_w_00 +
                                   b01*d_w_01 +
                                   b10*d_w_10 +
                                   b11*d_w_11  ) ;
                      if b3 > 255 then
                        b3 := 255 ;

                      b00 := (pixin00 shr 24) and $FF ;
                      b01 := (pixin01 shr 24) and $FF ;
                      b10 := (pixin10 shr 24) and $FF ;
                      b11 := (pixin11 shr 24) and $FF ;
                      b4 := RoundS(b00*d_w_00 +
                                   b01*d_w_01 +
                                   b10*d_w_10 +
                                   b11*d_w_11  ) ;


                      if b4 <> 0 then begin

                        pixout := Integer((b4 shl 24)  or
                                          (b3 shl 16)  or
                                          (b2 shl 8 )  or
                                           b1       ) ;
                        _bmp[rowx +nk] := pixout ;
                      end ;
                    end ;
                  end ;
                end ;
              end ;
            end ;
          end ;
        end ;
      end ;
    end ;
  end ;

  if assigned(cutpixels) then begin
    cutpixels := nil ;
    cut_idx_x := nil ;
    cut_idx_y := nil ;
  end ;
  bmpin := nil ;
  if isAlpha then
    inAlpha := nil ;
end ;

constructor T_ParseZones.Create(
  const _txt : String
) ;
begin
  inherited Create ;

  FIsRGB := True ;

  tkn := TGIS_Tokenizer.Create ;
  tkn.ExecuteEx( _txt, ',' ) ;

  FCount := tkn.Result.Count ;

  if FCount > 0 then begin
    if      CompareText( tkn.Result[FCount-1], 'RGB' ) = 0 then begin
              FIsRGB := True ;
              dec( FCount ) ;
            end
    else if CompareText( tkn.Result[FCount-1], 'BGR' ) = 0 then begin
              FIsRGB := False ;
              dec( FCount ) ;
            end ;
  end ;
end ;

procedure T_ParseZones.doDestroy ;
begin
  FreeObject( tkn ) ;
  inherited ;
end ;

function T_ParseZones.ReadRGB(
  const _idx : Integer
) : TGIS_Color ;
var
  txt   : String ;
  res   : TGIS_Color {$IFDEF GIS_NORECORDS} = new TGIS_Color {$ENDIF} ;

  procedure switch_colors ;
  begin
    res.ARGB := (Cardinal(res.A) shl 24) or
                (Cardinal(res.R) shl 16) or
                (Cardinal(res.G) shl 8 ) or
                (Cardinal(res.B)       ) ;
  end ;
begin
  Result := TGIS_Color.FromARGB(0)  ;
  res    := Result ;
  try
    txt := tkn.Result[ _idx ] ;

    if IsStringEmpty( txt ) then exit ;

    res := ParamColor( txt, TGIS_Color.Black ) ;

    if     ( Pos( ':', txt ) > 1) then begin
            if not FIsRGB then
              switch_colors ;
    end
    {$IFDEF JAVA OR ISLAND}
      else if CharInSet( txt[0], cNumericSet) then begin

    {$ELSE}
      else if CharInSet( txt[1], [ '$', '0'..'9' ] ) then begin
    {$ENDIF}
              if FIsRGB then
                switch_colors ;
            end
    else    begin
              switch_colors ;
            end ;
  finally
    Result := res ;
  end ;
end ;

function T_ParseZones.ReadFloat(
  const _idx : Integer
) : Double ;
begin
  Result := DotStrToFloat( tkn.Result[ _idx ] ) ;
end ;
{$ENDREGION}

{$REGION 'T_AltitudeZoneGenerator'}
type
  /// <summary>
  ///   Helper class for generating AltitudeMapZones.
  /// </summary>
  T_AltitudeZoneGenerator = class
    private
      FZoneDefinitions : TStringList ;

    private
      colorSpace       : TGIS_ColorInterpolationMode ;
      minVal, maxVal   : Double ;
      dZEpsilon        : Double ;
      lastLegendVal    : Double ;
      precDigits       : Integer ;
      precFactor       : Double;

    private
      /// <summary>
      ///   Calculate precision for futher operations.
      /// </summary>
      /// <param name="_min_val">
      ///   minimum value
      /// </param>
      /// <param name="_max_val">
      ///   maximum value
      /// </param>
      procedure calcPrec ( const _min_val : Double ;
                           const _max_val : Double
                         ) ;

      /// <summary>
      ///   Round value according to precision.
      /// </summary>
      /// <param name="_val">
      ///   value
      /// </param>
      /// <param name="_offset">
      ///   offset to round one precision point up or down
      /// </param>
      /// <returns>
      ///   rounded value
      /// </returns>
      function doPrec ( const _val    : Double ;
                        const _offset : Integer = 0
                      ) : Double ;

    public
      /// <summary>
      ///   List of altitude map zone definitions.
      /// </summary>
      property ZoneDefinitions : TStringList
                                 read FZoneDefinitions ;

      {$IFDEF DCC}
        /// <summary>
        ///   Destructor.
        /// </summary>
        destructor Destroy ; override ;
      {$ENDIF}

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_color_space">
      ///   color space for color interpolation
      /// </param>
      /// <param name="_min_val">
      ///   global minimum value (for estimating precision)
      /// </param>
      /// <param name="_max_val">
      ///   global maximum value (for estimating precision)
      /// </param>
      /// <param name="_dZEpsilon">
      ///   pixel layer's tolerance
      /// </param>
      constructor Create ( const _color_space : TGIS_ColorInterpolationMode ;
                           const _min_val     : Double ;
                           const _max_val     : Double ;
                           const _dZEpsilon   : Double
                         ) ;

      /// <summary>
      ///   Generate altitude map zone definitions.
      /// </summary>
      /// <param name="_start_color">
      ///   start color
      /// </param>
      /// <param name="_end_color">
      ///   end color
      /// </param>
      /// <param name="_min_val">
      ///   value for first zone
      /// </param>
      /// <param name="_max_val">
      ///   value for last zone
      /// </param>
      /// <param name="_middle_mode">
      ///   If True, last zone will exact match '_max_value'
      /// </param>
      /// <param name="_ramp_interval">
      ///   ramp interval
      /// </param>
      /// <param name="_legend_interval">
      ///   add legend label at this interval
      /// </param>
      procedure GenerateZoneDefinitions( const _start_color     : TGIS_Color ;
                                         const _end_color       : TGIS_Color ;
                                         const _min_val         : Double ;
                                         const _max_val         : Double ;
                                         const _middle_mode     : Boolean ;
                                         const _ramp_interval   : Double ;
                                         const _legend_interval : Double
                                       ) ;

      /// <summary>
      ///   Prepares and adds zone definition to Params.Pixel.AltitudeMapZones property.
      /// </summary>
      /// <param name="_min">
      ///   zone min
      /// </param>
      /// <param name="_max">
      ///   zone max
      /// </param>
      /// <param name="_color">
      ///   zone color
      /// </param>
      /// <param name="_addZoneLegend">
      ///   indicates whether the zone definition should include a legend
      /// </param>
      /// <param name="_isLastZone">
      ///   indicates whether the zone is last
      /// </param>
      procedure AddZoneDefinition      ( const _min           : Double ;
                                         const _max           : Double ;
                                         const _color         : TGIS_Color ;
                                         const _addZoneLegend : Boolean ;
                                         const _isLastZone    : Boolean
                                       ) ;
  end;

  procedure T_AltitudeZoneGenerator.calcPrec(
    const _min_val : Double ;
    const _max_val : Double
  ) ;
  const
    // max number of digits that will be correctly rounded based on Int64
    MAX_PREC     = 18 ;
    // for 1 gives precision 1.000; for 10 gives 10.00, etc.
    DEFAULT_PREC = 3 ;
  var
    delta : Double ;
  begin
   // new simpler precision
    delta := Abs( _max_val - _min_val ) ;
    if GisIsSameValue( delta, 0, dZEpsilon ) then
      precDigits := 0
    else begin
      precDigits := RoundS( Log10( delta ) ) ;

      if ( precDigits > MAX_PREC ) then
        precDigits := precDigits - MAX_PREC
      else if ( precDigits < -MAX_PREC ) then
        precDigits := precDigits + MAX_PREC
      else
        precDigits := Min( 0, precDigits - DEFAULT_PREC ) ;
    end ;

    precFactor  := RoundTo( IntPower( 10, precDigits ), precDigits ) ;
  end;

  function T_AltitudeZoneGenerator.doPrec(
    const _val    : Double ;
    const _offset : Integer = 0
  ) : Double  ;
  begin
    // rounding based on System.Math.SimpleRoundTo, but not the same
    Result := ( TruncS( _val / precFactor ) + _offset ) * precFactor ;
  end;

  constructor T_AltitudeZoneGenerator.Create(
    const _color_space : TGIS_ColorInterpolationMode ;
    const _min_val     : Double ;
    const _max_val     : Double ;
    const _dZEpsilon   : Double
  ) ;
  begin
    colorSpace := _color_space ;
    minVal := _min_val ;
    maxVal := _max_val ;
    dZEpsilon := _dZEpsilon ;
    lastLegendVal := minVal ;

    FZoneDefinitions := TStringList.Create ;

    calcPrec( minVal, maxVal ) ;
  end;

  {$IFDEF DCC}
    destructor T_AltitudeZoneGenerator.Destroy ;
    begin
      FreeObject( FZoneDefinitions ) ;
      inherited ;
    end;
  {$ENDIF}

  procedure T_AltitudeZoneGenerator.AddZoneDefinition(
    const _min           : Double ;
    const _max           : Double ;
    const _color         : TGIS_Color ;
    const _addZoneLegend : Boolean ;
    const _isLastZone    : Boolean
  ) ;
  var
    zone_def      : String ;
    zone_min_prec : Double ;
    zone_max_prec : Double ;
    legend        : String ;
  begin
    zone_min_prec := doPrec( _min ) ;

    // max value in both modes must be exact, that's why precision offset = 0
    zone_max_prec := doPrec( _max ) ;
    // but if rounded max value doesn't match exact max, use precision offset = 1
    if _isLastZone and not GisIsSameValue( zone_max_prec, _max, dZEpsilon ) then
      zone_max_prec := doPrec( _max, 1 ) ;

    // DotFloatToSrePrec function uses the precision argument
    // in the opposite way than RoundTo
    legend := '';
    if _addZoneLegend then
      legend := DotFloatToStrPrec( zone_min_prec, -precDigits );

    zone_def := ConstructParamAltitudeMapZone(
      zone_min_prec, zone_max_prec, _color, legend, True, -precDigits
    ) ;

    FZoneDefinitions.Add( zone_def ) ;
  end ;

  procedure T_AltitudeZoneGenerator.GenerateZoneDefinitions(
    const _start_color     : TGIS_Color ;
    const _end_color       : TGIS_Color ;
    const _min_val         : Double ;
    const _max_val         : Double ;
    const _middle_mode     : Boolean ;
    const _ramp_interval   : Double ;
    const _legend_interval : Double
  ) ;
  var
    delta            : Double ;
    zones_count      : Integer ;
    gradient_max_val : Double ;
    i                : Integer;
    is_last_zone     : Boolean ;
    zone_min         : Double ;
    zone_max         : Double ;
    add_zone_legend  : Boolean ;
    zone_color       : TGIS_Color ;
  begin
    delta := _max_val - _min_val ;
    zones_count := CeilS( delta / _ramp_interval ) ;

    // ensure the correct number of zones
    if ( not _middle_mode ) and
       ( zones_count = FloorS( delta / _ramp_interval ) )
    then
      inc( zones_count ) ;

    // assure that the _end_color appears in the last zone
    // the problem occurs when the number of ramps is small,
    // e.g. instead of red we get orange
    if _middle_mode then
      gradient_max_val := _max_val
    else
      gradient_max_val := _min_val + ( zones_count - 1 ) * _ramp_interval ;

    for i := 0 to zones_count - 1 do begin
      is_last_zone := ( i = zones_count - 1 ) ;

      zone_min := _min_val + i * _ramp_interval ;

      // Determine zone definition format.
      // legend interval may not be a multiplicity of the ramp interval, so we
      // must ensure that it appears as close to the expected value as possible
      add_zone_legend := False ;

      // legend label for the next nearest value to expected legend value
      if ( lastLegendVal - zone_min <= dZEpsilon ) then begin
        add_zone_legend := True ;

        lastLegendVal := lastLegendVal + _legend_interval ;  // next expected legend val
      end
      // legend label for each first zone
      else if ( i = 0 ) then begin
        add_zone_legend := True ;

        if _middle_mode then
          lastLegendVal := lastLegendVal + _legend_interval ;
      end
      // legend label for the last global zone
      else if is_last_zone and ( not _middle_mode ) then begin
        add_zone_legend := True ;
      end ;

      // Determine zone max.
      if is_last_zone then
        zone_max := _max_val
      else
        zone_max := _min_val + ( i + 1 ) * _ramp_interval ;

      zone_color := GradientColor(
        _start_color, _end_color, _min_val, gradient_max_val, zone_min, colorSpace
      ) ;

      AddZoneDefinition(
        zone_min, zone_max, zone_color, add_zone_legend, is_last_zone
      ) ;

    end ;
  end;
{$ENDREGION}

{$REGION 'TGIS_PixelBandsDefinition'}
constructor TGIS_PixelBandsDefinition.Create ;
begin
  FBands := TGIS_PixelBandList.Create ;
end;

procedure TGIS_PixelBandsDefinition.doDestroy ;
begin
  ClearBands ;

  FreeObject( FBands ) ;
end;


procedure TGIS_PixelBandsDefinition.SetBandSet(
  const _fileinfo : String   ;
  const _bandset  : TGIS_PixelBandSet ;
  const _band1    : Cardinal ;
  const _band2    : Cardinal ;
  const _band3    : Cardinal ;
  const _band4    : Cardinal
) ;
begin
  // replace '=', ':', and ' ' with underscore
  FFileInfo := StringReplaceAll( _fileinfo, ':', '_' ) ;
  FFileInfo := StringReplaceAll( FFileInfo, ' ', '_' ) ;
  FFileInfo := StringReplaceAll( FFileInfo, '=', '_' ) ;

  FBandSet := _bandset ;
  FBand1   := _band1 ;
  FBand2   := _band2 ;
  FBand3   := _band3 ;
  FBand4   := _band4 ;
end;

procedure TGIS_PixelBandsDefinition.ClearBands ;
{$IFNDEF OXYGENE}
  var
    bnd : TGIS_PixelBand ;
{$ENDIF}
begin
  {$IFNDEF NEXTGEN}
    for bnd in Bands do begin
      FreeObjectNotNil( bnd ) ;
    end;
  {$ENDIF}

  Bands.Clear ;
end;

procedure TGIS_PixelBandsDefinition.AddBand(
  const _content     : TGIS_PixelBandContent ;
  const _coding      : TGIS_PixelBandCoding  ;
  const _skip_before : Cardinal ;
  const _valid_bits  : Cardinal ;
  const _skip_after  : Cardinal
) ;
var
  bnd : TGIS_PixelBand ;
begin

  bnd := TGIS_PixelBand.Create ;

  bnd.FContent    := _content ;
  bnd.FCoding     := _coding  ;
  bnd.FSkipBefore := _skip_before ;
  bnd.FValidBits  := _valid_bits  ;
  bnd.FSkipAfter  := _skip_after  ;

  Bands.Add( bnd ) ;
end;

function TGIS_PixelBandsDefinition.ToString : String ;
{$IFNDEF OXYGENE}
  var
    bnd : TGIS_PixelBand ;
{$ENDIF}
begin
  Result := FFileInfo + ':' ;

  case FBandSet of
    TGIS_PixelBandSet.Rgb       : Result := Result + 'RGB.'  ;
    TGIS_PixelBandSet.Argb      : Result := Result + 'ARGB.' ;
    TGIS_PixelBandSet.Greyscale : Result := Result + 'G.'    ;
    TGIS_PixelBandSet.Negative  : Result := Result + 'N.'    ;
    TGIS_PixelBandSet.Dem       : Result := Result + 'D.'    ;
    else begin
      Result := Result + 'U.' ;
      assert( False ) ;
     end;
  end;

  if FBand1 > 0 then
    Result := Result + IntToStr( FBand1 ) ;
  if FBand2 > 0 then
    Result := Result + '.' + IntToStr( FBand2 ) ;
  if FBand3 > 0 then
    Result := Result + '.' + IntToStr( FBand3 ) ;
  if FBand4 > 0 then
    Result := Result + '.' + IntToStr( FBand4 ) ;

  for bnd in Bands do begin
    if not IsStringEmpty( Result ) then
      Result := Result + ':' ;

    case bnd.Content of
       TGIS_PixelBandContent.Alpha : Result := Result + 'A.' ;
       TGIS_PixelBandContent.Red   : Result := Result + 'R.' ;
       TGIS_PixelBandContent.Green : Result := Result + 'G.' ;
       TGIS_PixelBandContent.Blue  : Result := Result + 'B.' ;
       TGIS_PixelBandContent.Photo : Result := Result + 'P.' ;
       TGIS_PixelBandContent.Dem   : Result := Result + 'D.' ;
       else begin
         Result := Result + 'U.' ;
         assert( False ) ;
       end;
    end;

    assert( bnd.SkipBefore <=64 ) ;
    assert( bnd.ValidBits  <=64 ) ;
    assert( bnd.SkipAfter  <=64 ) ;

    Result := Result + IntToStr( bnd.SkipBefore ) + '.' ;
    Result := Result + IntToStr( bnd.ValidBits  ) + '.' ;
    Result := Result + IntToStr( bnd.SkipAfter  ) + '.' ;

    case bnd.Coding of
       TGIS_PixelBandCoding.Palette  : Result := Result + 'P' ;
       TGIS_PixelBandCoding.Integer  : Result := Result + 'I' ;
       TGIS_PixelBandCoding.Cardinal : Result := Result + 'C' ;
       TGIS_PixelBandCoding.Float    : Result := Result + 'F' ;
       else begin
         Result := Result + 'U' ;
         assert( False ) ;
       end;
    end;
  end;
end;

procedure TGIS_PixelBandsDefinition.FromString(
  const _value : String
) ;
  procedure bandset( const _text : String )  ;
  var
    {$IFNDEF OXYGENE}
      stmp   : String  ;
    {$ENDIF}
    istate   : Integer ;
    bndcnt   : Cardinal ;
  begin
    FBandSet := TGIS_PixelBandSet.Unknown ;
    FBand1 := 0 ;
    FBand2 := 0 ;
    FBand3 := 0 ;
    FBand4 := 0 ;
    bndcnt := 0 ;

    istate := 0 ;
    {$IFDEF JAVA OR ISLAND}
      for stmp in _text.Split( '.' ) do
    {$ELSE}
      for stmp in _text.Split( ['.'] ) do
    {$ENDIF}
    begin
      case istate of
        0 : // bandset
          begin
            if stmp = 'U' then
              FBandSet := TGIS_PixelBandSet.Unknown
            else
            if stmp = 'RGB' then
              FBandSet := TGIS_PixelBandSet.Rgb
            else
            if stmp = 'ARGB' then
              FBandSet := TGIS_PixelBandSet.Argb
            else
            if stmp = 'G' then
              FBandSet := TGIS_PixelBandSet.Greyscale
            else
            if stmp = 'N' then
              FBandSet := TGIS_PixelBandSet.Negative
            else
            if stmp = 'D' then
              FBandSet := TGIS_PixelBandSet.Dem
            else begin
              assert( False, 'UNKNOWN OPTION ' + stmp ) ;
            end;

            istate := 1 ;
          end;
        1 : // band 1
          begin
            FBand1 := StrToInt( stmp ) ;
            inc( bndcnt ) ;

            istate := 2 ;
          end;
        2 : // band 2
          begin
            FBand2 := StrToInt( stmp ) ;
            inc( bndcnt ) ;

            istate := 3 ;
          end;
        3 : // band 3
          begin
            FBand3 := StrToInt( stmp ) ;
            inc( bndcnt ) ;

            istate := 4 ;
          end;
        4 : // band 4
          begin
            FBand4 := StrToInt( stmp ) ;
            inc( bndcnt ) ;

            istate := 5 ;
          end;
        else
          begin
            assert( False, 'UNKNOWN OPTION ' + stmp ) ;
          end;

      end;
    end;

    assert( bndcnt > 0 ) ;

  end;

  procedure band( const _text : String )  ;
  var
    {$IFNDEF OXYGENE}
      stmp  : String  ;
    {$ENDIF}
    istate  : Integer ;
    content : TGIS_PixelBandContent  ;
    coding  : TGIS_PixelBandCoding ;
    before  : Cardinal ;
    after   : Cardinal ;
    bits    : Cardinal ;
  begin
    content := TGIS_PixelBandContent.Unknown  ;
    coding  := TGIS_PixelBandCoding.Unknown ;
    before  := 0 ;
    after   := 0 ;
    bits    := 0 ;

    istate := 0 ;
    {$IFDEF JAVA OR ISLAND}
      for stmp in _text.Split( '.' ) do
    {$ELSE}
      for stmp in _text.Split( ['.'] ) do
    {$ENDIF}
    begin
      case istate of
        0 : // bandtype
          begin
            if stmp = GIS_BAND_U then
              content := TGIS_PixelBandContent.Unknown
            else
            if stmp = GIS_BAND_R then
              content := TGIS_PixelBandContent.Red
            else
            if stmp = GIS_BAND_G then
              content := TGIS_PixelBandContent.Green
            else
            if stmp = GIS_BAND_B then
              content := TGIS_PixelBandContent.Blue
            else
            if stmp = GIS_BAND_A then
              content := TGIS_PixelBandContent.Alpha
            else
            if stmp = GIS_BAND_P then
              content := TGIS_PixelBandContent.Photo
            else
            if stmp = GIS_BAND_D then
              content := TGIS_PixelBandContent.Dem
            else begin
              assert( False, 'UNKNOWN OPTION ' + stmp ) ;
            end;

            istate := 1 ;
          end;
        1 : // before
          begin
            before := StrToInt( stmp ) ;

            istate := 2 ;
          end;
        2 : // bits
          begin
            bits := StrToInt( stmp ) ;

            istate := 3 ;
          end;
        3 : // after
          begin
            after := StrToInt( stmp ) ;

            istate := 4 ;
          end;
        4 : // content
          begin
            if stmp = 'U' then
              coding := TGIS_PixelBandCoding.Unknown
            else
            if stmp = 'P' then
              coding := TGIS_PixelBandCoding.Palette
            else
            if stmp = 'I' then
              coding := TGIS_PixelBandCoding.Integer
            else
            if stmp = 'C' then
              coding := TGIS_PixelBandCoding.Cardinal
            else
            if stmp = 'F' then
              coding := TGIS_PixelBandCoding.Float
            else begin
              assert( False, 'UNKNOWN OPTION ' + stmp ) ;
            end;

            istate := 5 ;
          end;
        else
          begin
            assert( False, 'UNKNOWN OPTION ' + stmp ) ;
          end;

      end;
    end;

    AddBand( content, coding, before, bits, after );
  end;

  procedure doparse ;
  var
    {$IFNDEF OXYGENE}
      stmp   : String  ;
    {$ENDIF}
    istate   : Integer ;
    sbandset : String ;
  begin
    istate := 0 ;
    {$IFDEF JAVA OR ISLAND}
      for stmp in _value.ToUpper.Split( ':' ) do
    {$ELSE}
      for stmp in _value.ToUpper.Split( [':'] ) do
    {$ENDIF}
    begin
      case istate of
        0 :  begin // fileinf
              // ignore
              istate := 2 ;
             end;
        2 :  begin // band set
               sbandset := stmp ;
               // ignore exif names
               istate := 3 ;
            end;
        3 :  begin // band
               band( stmp ) ;
             istate := 3 ;
             end;
        else begin
                assert( False, 'UNKNOWN OPTION ' + stmp ) ;
             end;
      end;
    end ;

    bandset( sbandset ) ;
  end ;

begin
  doparse ;
end;
{$ENDREGION}

  {$IFDEF JAVA}
  type

    T_CompAltitudeZone = class( java.util.Comparator<TGIS_AltitudeZone> )
      public
        function    compare ( _left    : TGIS_AltitudeZone ;
                              _right    : TGIS_AltitudeZone
                            ) : Integer ;
    end ;

    function T_CompAltitudeZone.compare(
      _left : TGIS_AltitudeZone ;
      _right : TGIS_AltitudeZone
    ) : Integer ;
    begin
      if GisIsSameValue(_left.MinVal, _right.MinVal, 1E-15) and
         GisIsSameValue(_left.MaxVal, _right.MaxVal, 1E-15) then
        Result := 0
      else if _left.MaxVal <= _right.MinVal  then
        Result := -1
      else if _left.MaxVal > _right.MinVal then begin // range overlapp
        if _left.MinVal < _right.MinVal then // check min val
          Result := -1
        else
          Result := 1
      end
      else
        Result := 0
    end ;
  {$ENDIF}

{$REGION 'TGIS_LayerPixel'}
constructor TGIS_LayerPixel.Create ;
var
  s : Integer ;
begin
  inherited Create;

  FPathTAB := '' ;

  ParamsList.SetUp( TGIS_ParamsSectionPixel.Create ) ;
  FPagesCount         := 0               ;
  FCurrentPage        := 0               ;
  FMinThresholdZ      := -GIS_MAX_SINGLE ;
  FMaxThresholdZ      := GIS_MAX_SINGLE  ;
  FMaxZ               := -GIS_MAX_SINGLE ;
  FMinZ               := GIS_MAX_SINGLE  ;
  mmzgbn              := -1              ;
  extZoom             := 1               ;
  baseRotation        := False           ;
  importMode          := False           ;
  forViewer           := True            ;
  scaleXFactor        := 1               ;
  scaleYFactor        := 1               ;
  modifiedMinHeight   := False           ;
  modifiedMaxHeight   := False           ;
  isBuilt             := False           ;
  oLockListW          := nil             ;
  oLockListR          := nil             ;
  fromBand            := 0               ;
  bandMask            := 0               ;
  lockUsingNo         := 0               ;
  readableNo          := 0               ;
  asWritable          := False           ;
  isBGR               := False           ;
  bandsMap[0]         := 0               ;
  bandsMap[1]         := 1               ;
  bandsMap[2]         := 2               ;
  bandsMap[3]         := 3               ;

  {$IFDEF GIS_NORECORDS}
  baseProjectedExtent := new TGIS_Extent  ;
  colorNoData := new TGIS_Color  ;
  internalTransparentColor := new TGIS_Color ;
  {$ENDIF}

  internalTransparentColor := TGIS_Color.FromARGB($FF, 0, 0, 0) ;

  FNoDataValue := Params.Pixel.GridNoValue ;

  FShadowAngle := 90 ;
  FForcedBandsDefinition := '' ;

  s := 256 ;
  SetLength( mapRGB     , s ) ;
  SetLength( mapGray2RGB, s ) ;
  SetLength( bitPalette , s ) ;

  FBitmapFormat      := TGIS_BitmapFormat.ARGB ;
  FBitmapLinesOrder  := TGIS_BitmapLinesOrder.Down ;
  FCapabilities      := TGIS_LayerPixelSubFormatList.Create ;

  FCapabilities.Add( TGIS_LayerPixelSubFormat.Create(
                      TGIS_PixelFormat.ARGB,
                      False,
                      TGIS_PixelSubFormat.None,
                      TGIS_CompressionType.None,
                      0
                     )
                  ) ;
  FIsTiled := False ;
  FSubFormat := DefaultSubFormat;

  FAntialias := False;
  FNoDataColor := TGIS_Color.FromARGB(0) ;
  FAntialiasFilter := TGIS_ScalingFilter.Linear ;

  bandsDefinition := TGIS_PixelBandsDefinition.Create ;

  dZEpsilon := GIS_SINGLE_RESOLUTION ;
  FInterpretation := TGIS_LayerPixelInterpretation.Default ;

  FBitmapFactory := nil ;
end ;

procedure TGIS_LayerPixel.ReOpen ;
var
  s : Integer ;
begin
  Dormant ;
  RevertAll ;
  ParamsList.ResetSerial ;

  FIsPrepared := False ;


  FPathTAB := '' ;

  FPagesCount         := 0               ;
  FCurrentPage        := 0               ;
  FMinThresholdZ      := -GIS_MAX_SINGLE ;
  FMaxThresholdZ      := GIS_MAX_SINGLE  ;
  FMaxZ               := -GIS_MAX_SINGLE ;
  FMinZ               := GIS_MAX_SINGLE  ;
  mmzgbn              := -1              ;
  extZoom             := 1               ;
  baseRotation        := False           ;
  importMode          := False           ;
  forViewer           := True            ;
  scaleXFactor        := 1               ;
  scaleYFactor        := 1               ;
  modifiedMinHeight   := False           ;
  modifiedMaxHeight   := False           ;
  isBuilt             := False           ;
  fromBand            := 0               ;
  bandMask            := 0               ;
  lockUsingNo         := 0               ;
  readableNo          := 0               ;
  asWritable          := False           ;
  isBGR               := False           ;
  bandsMap[0]         := 0               ;
  bandsMap[1]         := 1               ;
  bandsMap[2]         := 2               ;
  bandsMap[3]         := 3               ;
  FExtent             := GisExtent(1, 1, -1, -1) ;
  FProjectedExtent    := GisExtent(0, 0, 0, 0) ;
  scaleX              := 0 ;
  scaleY              := 0 ;
  FBitWidth           := 0 ;
  FBitHeight          := 0 ;
  baseCellWidth       := 0 ;
  baseCellHeight      := 0 ;

  alphaAssociated           := False ;
  isPartialTransparent      := False ;
  defaultPartialTransparent := False ;


  {$IFDEF GIS_NORECORDS}
  baseProjectedExtent := new TGIS_Extent  ;
  colorNoData := new TGIS_Color  ;
  internalTransparentColor := new TGIS_Color ;
  {$ENDIF}

  FNoDataValue := Params.Pixel.GridNoValue ;

  FShadowAngle := 90 ;
  FForcedBandsDefinition := '' ;

  s := 256 ;
  SetLength( mapRGB     , s ) ;
  SetLength( mapGray2RGB, s ) ;
  SetLength( bitPalette , s ) ;

  FBitmapFormat      := TGIS_BitmapFormat.ARGB ;
  FBitmapLinesOrder  := TGIS_BitmapLinesOrder.Down ;
  FIsTiled := False ;
  FSubFormat := DefaultSubFormat;

  FAntialias := False;
  FNoDataColor := TGIS_Color.FromARGB(0) ;
  FAntialiasFilter := TGIS_ScalingFilter.Linear ;

  dZEpsilon := GIS_SINGLE_RESOLUTION ;
  Interpretation := TGIS_LayerPixelInterpretation.Default ;

  if assigned( Viewer ) and (not FIsLocked) then
    Viewer.Ref.RecalcExtent ;

  FIsOpened   := False ;
  FreeObject(fileStream) ;
  Open ;
end ;


procedure TGIS_LayerPixel.AssignedParentLayerInternal(
  const _parent : TGIS_LayerPixel
) ;
begin
  oParentLockList := _parent.oLockListW ;
end;

procedure TGIS_LayerPixel.ApplyAntialiasSettings(
  const _antialias : Boolean
) ;
begin
  ApplyAntialiasSettings( _antialias, FAntialiasFilter ) ;
end ;

procedure TGIS_LayerPixel.ApplyAntialiasSettings(
  const _antialias       : Boolean ;
  const _antialiasFilter : TGIS_ScalingFilter
) ;
begin
  fset_Antialias( _antialias ) ;
  FAntialiasFilter := _antialiasFilter
end ;

function TGIS_LayerPixel.DrawEx(
  const _extent : TGIS_Extent
) : Boolean ;
var
  bmp     : TGIS_Pixels ;
  ex      : TGIS_Extent ;
  ex_tmp  : TGIS_Extent ;
  hnd     : TObject  ;
  width,
  height  : Integer  ;
  left,
  top     : Integer  ;
  right,
  bottom  : Integer  ;
  ready   : Boolean  ;
  ipix    : Integer  ;
  bempty  : Boolean  ;
  tm      : Int64    ;
  usetm   : Boolean  ;
  tol     : Double   ;
  rct     : TRect    ;
  btpmst  : Boolean  ;
  timeout : Int64    ;
  i           : Integer ;
  cnt         : Integer ;
  is_visible  : Boolean ;
begin
  Result := inherited DrawEx( _extent ) ;
  if not Result then exit ;

  prepareParamsCache( '' ) ;
  cnt := paramsCache.Count ;

  if cnt <= 0 then exit ;

  is_visible := False ;
  for i := cnt -1 downto 0 do begin
    optimizeParamsCache( i, False ) ;
    is_visible := is_visible or TGIS_ParamsSectionPixel( paramsCache[i] ).Visible ;
    if is_visible then break ;
  end ;

  if not is_visible then exit ;

  timeout := GisMetadataAsInteger( METADATA_TIMEOUT, GETBITMAP_TIMEOUT ) ;
  tm := GetTickCount ;
  ready := False ;
  ex := GisCommonExtent( _extent, FProjectedExtent) ;

  ex_tmp :=  Viewer.Ref.VisibleExtent ;

  if GisIsWholeWorld( Viewer.Ref.RestrictedExtent ) then begin
    if ex_tmp.XMax < FProjectedExtent.XMax then
      ex.XMax := ex_tmp.XMax ;

    if ex_tmp.YMin > FProjectedExtent.YMin then
      ex.YMin := ex_tmp.YMin ;
  end ;

  left   := RoundS( ( ex.XMin - ex_tmp.XMin ) *  Viewer.Ref.Zoom ) ;
  top    := RoundS( ( ex_tmp.YMax - ex.YMax ) *  Viewer.Ref.Zoom ) ;

  right  := RoundS( ( ex.XMax - ex_tmp.XMin ) *  Viewer.Ref.Zoom ) ;
  bottom := RoundS( ( ex_tmp.YMax - ex.YMin ) *  Viewer.Ref.Zoom ) ;

  rct := Rect( left, top, right, bottom ) ;
  width  := right -left ;
  height := bottom -top ;

  btpmst := IsTopmost ;
  usetm := False ;
  if btpmst then begin
    if assigned(tmCache) then begin
      if (tmCache.Size.X = width) and (tmCache.Size.Y = height) then begin
        if width > height then
          tol := (ex.XMax - ex.XMin)/width
        else
          tol := (ex.YMax - ex.YMin)/height ;
       if ( Abs(tmCache.Extent.XMin -ex.XMin) < tol ) and
          ( Abs(tmCache.Extent.YMin -ex.YMin) < tol ) and
          ( Abs(tmCache.Extent.XMax -ex.XMax) < tol ) and
          ( Abs(tmCache.Extent.YMax -ex.YMax) < tol ) and
          ( tmCache.Serial = Params.Serial )
       then
          usetm := True ;
      end;
    end;
  end;

  if not usetm then
    FreeObject( tmCache ) ;

  if assigned( tmCache ) then begin
    hnd := TGIS_RendererAbstract( Renderer ).RenderBitmapBegin ;
    TGIS_RendererAbstract( Renderer ).RenderBitmapCache( hnd, tmCache, rct ) ;
    TGIS_RendererAbstract( Renderer ).RenderBitmapEnd( hnd ) ;
    exit ;
  end ;

  SetLength(bmp, width * height ) ;
  setupParams ;
  setBmpTransparent(bmp) ;

  Alive ;

  if ( ex_tmp.XMin < FProjectedExtent.XMax) and
     ( ex_tmp.YMin < FProjectedExtent.YMax)
  then begin
    {$IFDEF ISLAND}
      while ( not ready ) do begin
    {$ELSE}
      while ( not ready ) and ( ( GetTickCount - tm ) < timeout ) do begin
    {$ENDIF}
      if tm = 0 then begin

      end;

      ready := getBitmapPixels(ex, bmp, width, height) ;

      if ( not ready ) and assigned( Viewer ) then
        if Viewer.Ref.HourglassShake then
          break ;

      if not Progressive then
        continue ;

      bempty := True ;
      for ipix := 0 to width*height -1 do begin
        if bmp[ipix] <> 0 then begin
          bempty := False ;
          break ;
        end;
      end ;
      if bempty then continue ;

      if FIsContrastEnhanced then
        enhanceContrast(bmp, width, height) ;

      hnd := TGIS_RendererAbstract( Renderer ).RenderBitmapBegin ;
      TGIS_RendererAbstract( Renderer ).RenderBitmap(
                                          hnd,
                                          bmp,
                                          Point( width, height ),
                                          rct,
                                          FBitmapFormat,
                                          FBitmapLinesOrder
                                        ) ;
      TGIS_RendererAbstract( Renderer ).RenderBitmapEnd( hnd ) ;
      if ( not ready ) and assigned( Viewer ) then
        if Viewer.Ref.HourglassShake then
          break ;
    end ;

  end;

  if not Progressive then begin
    if FIsContrastEnhanced then
      enhanceContrast(bmp, width, height) ;

    hnd := TGIS_RendererAbstract( Renderer ).RenderBitmapBegin ;
    TGIS_RendererAbstract( Renderer ).RenderBitmap(
                                        hnd,
                                        bmp,
                                        Point( width, height ),
                                        rct,
                                        FBitmapFormat,
                                        FBitmapLinesOrder
                                      ) ;
    TGIS_RendererAbstract( Renderer ).RenderBitmapEnd( hnd ) ;
  end ;

  if btpmst then begin
    if not usetm then begin
      assert( not assigned( tmCache ) ) ;

      tmCache := TGIS_RendererAbstract( Renderer ).PrepareBitmapCache(
                   bmp,
                   ex,
                   Point( width, height ),
                   Params.Serial,
                   FBitmapFormat,
                   FBitmapLinesOrder
                 ) ;
    end;
  end;
end ;

procedure TGIS_LayerPixel.DrawFlash ;
begin

end ;

function  TGIS_LayerPixel.getNativeValue(
  const _pt : TPoint ;
  const _ar : TGIS_DoubleArray
) : Boolean  ;
var
  la   : TGIS_SingleArray ;
  s, i : Integer ;
begin
  if IsGridImage then
    s := 1
  else
    s := 3 ;
  if length(_ar) < s then begin
    Result := False ;
    exit ;
  end ;
  SetLength(la, s) ;
  Result := getNativeLine( la, _pt.Y, _pt.X, 1 ) = 1 ;
  if Result then begin
    for i := 0 to s -1 do
      _ar[i] := la[i] ;
  end ;
  SetLength(la, s) ;
end ;

function  TGIS_LayerPixel.getNativeLine(
  const _buffer   : TGIS_SingleArray ;
  const _linenr   : Integer          ;
  const _startIdx : Integer          ;
  const _count    : Integer
) : Integer ;
var
  pixbuf  : TGIS_Pixels ;
  pixels  : Integer ;
  i       : Integer ;
  maxidx  : Integer ;
  count   : Integer ;
  function find_in_palette (const _pix : Integer) : Integer ;
  var
    palhigh : Integer ;
    kk : Integer ;
  begin
    Result := -1 ;
    palhigh := RoundS(Power(2, bitsPerPixel)) -1  ;
    for kk := 0 to palhigh do begin
      if bitPalette[kk].B = (_pix and $FF) then
        if bitPalette[kk].G = ((_pix shr 8)and $FF) then
          if bitPalette[kk].R = ((_pix shr 16)and $FF) then begin
            Result := kk ;
            break ;
          end;
    end;
  end ;
begin
  if not assigned( _buffer ) then begin
    Result := 0 ;
    exit ;
  end ;

  if (_startIdx + _count) > baseCellWidth then
    count := baseCellWidth - _startIdx
  else
    count := _count ;
  if count <= 0 then begin
    Result := 0 ;
    exit ;
  end ;

  Result := count ;
  pixels := count ;
  if assigned(tempFileStream) then begin
    tempFileStream.Position := Int64( Int64(_linenr) * baseCellWidth + _startIdx) * sizeOf( Single ) ;
    {$IFDEF OXYGENE}
      for i := 0 to count-1 do
        tempFileStream.ReadSingle( _buffer[i], sizeOf( Single ) ) ;
    {$ELSE}
      tempFileStream.Read( ( Addr( _buffer[0]))^, count * sizeOf( Single ) ) ;
    {$ENDIF}
    exit ;
  end ;

  if assigned( oGrid ) then begin

    if _linenr < FBitHeight then begin
      for i := 0 to count -1 do
        _buffer[i] := oGrid[_linenr][i + _startIdx] ;
    end
    else
      Result := 0 ;
  end
  else begin
    SetLength( pixbuf, pixels ) ;

    getLinePixels( pixbuf, 0, _linenr, _startIdx, pixels ) ;
    maxidx := length(_buffer) ;

    if maxidx = 3*count then begin
      for i := 0 to count -1 do begin
        _buffer[i*FBandsCount +0] := (pixbuf[i] shr 16) and $FF ;
        _buffer[i*FBandsCount +1] := (pixbuf[i] shr 08) and $FF ;
        _buffer[i*FBandsCount +2] := (pixbuf[i] shr 00) and $FF ;
      end;
    end
    else
    if maxidx = 4*count then begin
      for i := 0 to count -1 do begin
        _buffer[i*FBandsCount +0] := (pixbuf[i] shr 16) and $FF ;
        _buffer[i*FBandsCount +1] := (pixbuf[i] shr 08) and $FF ;
        _buffer[i*FBandsCount +2] := (pixbuf[i] shr 00) and $FF ;
        _buffer[i*FBandsCount +3] := (pixbuf[i] shr 24) and $FF ;
      end;
    end
    else begin
      if (bitsPerPixel >= 1) and (bitsPerPixel <= 8) then begin
         for i := 0 to maxidx -1 do
          _buffer[i]  := find_in_palette(pixbuf[i]) ;
      end
      else begin
        if maxidx > length(_buffer) then
          maxidx := length(_buffer) ;
        if maxidx > length(pixbuf) then
          maxidx := length(pixbuf) ;
        case GridBand  of
          1 :
            begin
              for i := 0 to maxidx -1 do
                _buffer[i]  := (pixbuf[i] shr 16) and $FF ;
            end ;
          2 :
            begin
              for i := 0 to maxidx -1 do
                _buffer[i]  := (pixbuf[i] shr 8) and $FF ;
            end ;
          3 :
            begin
              for i := 0 to maxidx -1 do
                _buffer[i]  := pixbuf[i] and $FF ;
            end ;
        end;
      end;
    end ;
    pixbuf := nil ;
  end ;
end ;

function  TGIS_LayerPixel.getNativeLineGrayGrid(
  const _buffer   : TGIS_SingleArray ;
  const _linenr   : Integer          ;
  const _startIdx : Integer          ;
  const _count    : Integer
) : Integer ;
var
  i    : Integer ;
  count, bcount     : Integer ;
  wsbuffer   : TGIS_SingleArray ;
  wpbuffer   : TGIS_Pixels ;
  ri, gi, bi : Integer ;
  mc : Integer ;
  bpb : Integer ;
begin
  if not assigned( _buffer ) then begin
    Result := 0 ;
    exit ;
  end ;


  if (_startIdx + _count) > baseCellWidth then
    count := baseCellWidth - _startIdx
  else
    count := _count ;
  if count <= 0 then begin
    Result := 0 ;
    exit ;
  end ;

  Result := count ;
  if bitsPerPixel = 0 then
    bitsPerPixel := 8*FBandsCount ;
  ri := 0 ;
  gi := 1 ;
  bi := 2 ;
  if FBandsCount > 1 then begin
    if isPlanar1 then
      mc := FBandsCount
    else
      mc := 3 ;
  end
  else begin
    if (bitsPerPixel <= 8) or (FBandsCount = 1) then begin
      mc := FBandsCount ;
      if mc = 1 then begin
        gi := 0 ;
        bi := 0 ;
      end;
    end
    else
      mc := 3 ;
  end;
  bcount := mc * count ;
  if FBandsCount > 0 then
    bpb := bitsPerPixel div FBandsCount
  else
    bpb := 8 ;

  if bpb <= 8  then begin
    SetLength(wpbuffer, bcount) ;
    getLinePixels(wpbuffer, 0, _linenr, _startIdx, count) ;
    if isPartialTransparent then begin
      for i := 0 to count -1 do begin

        if ((wpbuffer[i] shr 24) and $FF) < Integer($7F) then
          _buffer[i] := GIS_GRID_NOVALUE
        else
          _buffer[i] := (((wpbuffer[i] shr 16) and $FF) * FACTOR_RED) +
                        (((wpbuffer[i] shr 08) and $FF) * FACTOR_GREEN) +
                        (((wpbuffer[i] shr 00) and $FF) * FACTOR_BLUE) ;
      end ;
    end
    else begin
      for i := 0 to count -1 do
        _buffer[i] := (((wpbuffer[i] shr 16) and $FF) * FACTOR_RED) +
                      (((wpbuffer[i] shr 08) and $FF) * FACTOR_GREEN) +
                      (((wpbuffer[i] shr 00) and $FF) * FACTOR_BLUE) ;
    end;
    wpbuffer := nil ;
  end
  else begin
    SetLength(wsbuffer, bcount) ;
    getNativeLine(wsbuffer, _linenr, _startIdx, count) ;

    if (FBandsCount = 2) and alphaAssociated then begin
      for i := 0 to count -1 do begin
        if wsbuffer[2*i + 1] = 0 then
          _buffer[i] := GIS_GRID_NOVALUE
        else
          _buffer[i] := wsbuffer[2*i] ;
      end ;

    end
    else begin
      for i := 0 to count -1 do begin
        _buffer[i] := (wsbuffer[mc*i +ri] * FACTOR_RED) +
                      (wsbuffer[mc*i +gi] * FACTOR_GREEN) +
                      (wsbuffer[mc*i +bi] * FACTOR_BLUE) ;
      end ;
    end;
    wsbuffer := nil ;
  end ;
end ;


procedure TGIS_LayerPixel.prepareMinMaxZ(
  const _zoom : Double = -1
) ;
var
  i, k,
  l_no  : Integer ;
  zoom  : Double ;
  lines : Integer ;
  arr   : TGIS_SingleArray ;
const
  MAX_LINES  = 900 ;
  WORD_MAX_Z = 32760 ;
begin
  if  FBitWidth > 64000 then begin
    if not FIsNativeGridImage and rgbAsGrid then begin
      FMinZ :=  0 ;
      FMaxZ := 255 ;
      exit ;
    end;
  end;

  FMinZ :=  GIS_MAX_SINGLE ;
  FMaxZ := -GIS_MAX_SINGLE ;
  mmzgbn := FGridBand ;
  if (FGridBand = 0) and rgbAsGrid then begin
    prepareMinMaxZGray ;
    exit ;
  end;

  Alive ;

  if (_zoom > 0) AND (_zoom <= 1) then begin
    lines := RoundS(FBitHeight * _zoom) ;
    if lines = 0 then
      lines := 1 ;
  end
  else
  begin
    if MAX_LINES > FBitHeight then
      lines := FBitHeight
    else
      lines := MAX_LINES ;
  end ;

  zoom := (1.0 *FBitHeight) /lines ;

  SetLength(arr, FBitWidth*FBandsCount ) ;
  for i := 0 to lines -1 do begin

    l_no := TruncS(i*zoom) ;
    getNativeLine(arr, l_no, 0, FBitWidth) ;

    for k := 0 to FBitWidth -1 do begin
      if (arr[k] <> FNoDataValue) and (arr[k] <> GIS_GRID_NOVALUE)then
      begin
        if (arr[k] <= -GIS_MAX_SINGLE) or
           (arr[k] >=  GIS_MAX_SINGLE) then
        begin
          Params.Pixel.GridNoValue := arr[k] ;
          FNoDataValue := Params.Pixel.GridNoValue ;
          continue ;
        end
        else
        if arr[k] = GIS_GRID_NOVALUE  then
        begin
          Params.Pixel.GridNoValue := arr[k] ;
          FNoDataValue := Params.Pixel.GridNoValue ;
          continue ;
        end ;
        if arr[k] < FMinZ then
          FMinZ := arr[k]
        else
        if arr[k] > FMaxZ then
          FMaxZ := arr[k] ;
      end ;
    end ;
  end ;
  SetLength(arr, 0 ) ;
  FExtent3D.ZMin := FMinZ ;
  FExtent3D.ZMax := FMaxZ ;
end ;

procedure TGIS_LayerPixel.prepareMinMaxZGray(
  const _zoom : Double = -1
) ;
var
  i, k,
  l_no  : Integer ;
  zoom  : Double ;
  lines : Integer ;
  arr   : TGIS_SingleArray ;
  is_nodata : Boolean ;
  is_abort  : Boolean ;
  is_ready  : Boolean ;
  w, h : Integer ;
  rc : TRect ;
const
  MAX_LINES  = 900 ;
begin
  Alive ;

  if (_zoom > 0) AND (_zoom <= 1) then begin
    lines := RoundS(FBitHeight * _zoom) ;
    if lines = 0 then
      lines := 1 ;
  end
  else
  begin
    if MAX_LINES > FBitHeight then
      lines := FBitHeight
    else
      lines := MAX_LINES ;
  end ;

  h := lines ;
  zoom := setFileScale((1.0 *FBitHeight) /h, FBitHeight) ;
  w := TruncS(zoom*FBitWidth) ;
  rc := Rect( 0, 0, w, h) ;
  setFileView(rc) ;

  if zoom < 1 then begin
    zoom := (zoom *FBitHeight) /lines ;
  end
  else begin
    zoom := (1.0 *FBitHeight) /lines ;
  end;

  is_nodata := True ;
  is_abort  := False ;
  is_ready  := False ;
  while is_nodata do begin
    getAsyncState(is_abort, is_ready, is_nodata) ;
    if ( not is_ready ) and assigned( Viewer ) then begin
        if Viewer.Ref.HourglassShake then begin
          is_abort := True ;
          getAsyncState(is_abort, is_ready, is_nodata) ;
          FMaxZ := 255 ;
          FMinZ := 0 ;
          FExtent3D.ZMin := FMinZ ;
          FExtent3D.ZMax := FMaxZ ;
          exit ;
        end;
    end;
  end;

  SetLength(arr, FBitWidth ) ;
  for i := 0 to lines -1 do begin

    l_no := TruncS(i*zoom) ;
    getNativeLineGrayGrid(arr, l_no, 0, w) ;

    for k := 0 to FBitWidth -1 do begin
      if (arr[k] <> FNoDataValue)  and (arr[k] <> GIS_GRID_NOVALUE) then
      begin
        if (arr[k] <= -GIS_MAX_SINGLE) or
           (arr[k] >=  GIS_MAX_SINGLE) then
        begin
          Params.Pixel.GridNoValue := arr[k] ;
          FNoDataValue := Params.Pixel.GridNoValue ;
          continue ;
        end
        else
        if arr[k] = GIS_GRID_NOVALUE  then
        begin
          Params.Pixel.GridNoValue := arr[k] ;
          FNoDataValue := Params.Pixel.GridNoValue ;
          continue ;
        end ;
        if arr[k] < FMinZ then
          FMinZ := arr[k]
        else
        if arr[k] > FMaxZ then
          FMaxZ := arr[k] ;
      end ;
    end ;
  end ;
  SetLength(arr, 0 ) ;
  FExtent3D.ZMin := FMinZ ;
  FExtent3D.ZMax := FMaxZ ;
end ;

function  TGIS_LayerPixel.calculatePixelsize(
  const _pixelsize : Double
) : Double ;
var
  layerres  : Double ;
  projres   : Double ;
begin
  layerres := Sqrt( FPixelSize.X * FPixelSize.X ) ;
  if _pixelsize = 0 then
    Result := layerres
  else
  if _pixelsize < 0 then begin
    Result := layerres ;
    exit ;
  end
  else
    Result := _pixelsize ;

  if not assigned( Viewer ) then
    exit ;

  projres := ( ProjectedExtent.XMax - ProjectedExtent.XMin )
             / ( Extent.XMax - Extent.XMin ) ;

  if _pixelsize = 0 then
    Result := layerres / projres / Viewer.Ref.Zoom
  else
    Result := _pixelsize / projres  ;
end;

function TGIS_LayerPixel.getBitmapPixels(
  const _extent   : TGIS_Extent ;
  const _bitmap   : TGIS_Pixels ;
  const _width    : Integer       ;
  const _height   : Integer
) : Boolean ;
var
  do_transform : Boolean ;
  cext         : TGIS_Extent ;
  width,
  height       : Integer ;
  trans        : T_TransClass ;
  lmaxz, lminz : Single ;
  cmaxz, cminz : Single ;
  dstpix       : TGIS_Pixels ;
begin
  Result := True ;

  if (_width    = 0) or (_height    = 0) or
     (FBitWidth = 0) or (FBitHeight = 0) then begin
    exit ;
  end ;

  do_transform := transformNeed or baseRotation;
  height := _height ;
  width  := _width ;
  if do_transform then begin
    if not assigned(outCS) then begin
      outCS := Viewer.Ref.CS ;
      cext := GisCommonExtent(_extent, FProjectedExtent) ;
    end
    else
      cext := _extent ;

    if ( cext.XMin = cext.XMax ) or ( cext.YMin = cext.YMax )then begin
      exit ;
    end ;

    trans := T_TransClass.Create(self) ;
    try
      if FIsGridImage then begin
        srcGrid := InitializeGrid( height, width ) ;
        setNoDataTable(srcGrid) ;
        Result := trans.getTransformedGridData(srcGrid, _extent, width, height ) ;

        if assigned( FGridOperation) and (not importMode) then begin
          dstGrid := InitializeGrid( height, width ) ;
          setNoDataTable( dstGrid ) ;

          if FGridOperation( Self, _extent, srcGrid, dstGrid, width, height, lminz,
              lmaxz )
          then begin
            cminz := FMinZ ;
            cmaxz := FMaxZ ;
            FMinZ := lminz ;
            FMaxZ := lmaxz ;
            gridToARGBTable( _extent, _bitmap, dstGrid ,width, height ) ;
            FMinZ := cminz ;
            if FMinThresholdZ < FMinZ then begin
              FMinThresholdZ := FMinZ ;
              TGIS_ParamsSectionPixel(Params).Pixel.MinHeightThreshold := FMinZ ;
            end;
            FMaxZ := cmaxz ;
            if FMaxThresholdZ > FMaxZ then begin
              FMaxThresholdZ := FMaxZ ;
              TGIS_ParamsSectionPixel(Params).Pixel.MaxHeightThreshold := FMaxZ ;
            end;
            dstGrid := nil ;
          end
          else begin
            gridToARGBTable( _extent, _bitmap, srcGrid ,width, height ) ;
          end ;
        end
        else
          gridToARGBTable( _extent, _bitmap, srcGrid ,width, height ) ;
        srcGrid := nil ;
      end
      else begin
        Result := trans.getTransformedBitmapData(_bitmap, _extent, width, height ) ;
        if not importMode then begin
          if makeTransparent then
            addTransparency(_bitmap, _width, _height) ;

          if makeRGBMap then
            simpleRGBCorrection(_bitmap, width, height) ;
          if makeFullRGBMap then
            fullRGBCorrection(_bitmap, width, height) ;

          if assigned( FPixelOperation ) then begin
            SetLength(dstpix, height*width ) ;

            if FPixelOperation(self, _extent, _bitmap, dstpix, width, height) then
              GisCopyPixels(dstpix, 0, _bitmap, 0, width*height) ;
            dstpix := nil ;
          end ;
          if Params.Pixel.GrayScale or isGrayScaleImage then
            ARGB2Gray( _bitmap, width*height) ;
          if makeSomeCorrection or makeColorsByVals then
            fullCorrection(_bitmap, width, height) ;
        end;
      end ;
    finally
      FreeObject( trans ) ;
    end ;
  end
  else begin
    if FIsGridImage then begin
      srcGrid := InitializeGrid( height, width ) ;
      assert( assigned(srcGrid) ) ;
      setNoDataTable( srcGrid ) ;
      Result := getGridData( _extent , srcGrid ) ;

      if assigned( FGridOperation ) and (not importMode) then begin
        dstGrid := InitializeGrid( height, width ) ;
        setNoDataTable( dstGrid ) ;

        if FGridOperation( Self, _extent, srcGrid, dstGrid, width, height, lminz,
            lmaxz )
        then begin
          cminz := FMinZ ;
          cmaxz := FMaxZ ;
          FMinZ := lminz ;
          FMaxZ := lmaxz ;
          gridToARGBTable( _extent, _bitmap, dstGrid ,width, height ) ;
          FMinZ := cminz ;
          if FMinThresholdZ < FMinZ then begin
            FMinThresholdZ := FMinZ ;
            TGIS_ParamsSectionPixel(Params).Pixel.MinHeightThreshold := FMinZ ;
          end;
          FMaxZ := cmaxz ;
          if FMaxThresholdZ > FMaxZ then begin
            FMaxThresholdZ := FMaxZ ;
            TGIS_ParamsSectionPixel(Params).Pixel.MaxHeightThreshold := FMaxZ ;
          end;
          dstGrid := nil ;
        end
        else begin
          gridToARGBTable( _extent, _bitmap, srcGrid ,width, height ) ;
        end ;
      end
      else begin
        assert( assigned(srcGrid) ) ;
        gridToARGBTable( _extent, _bitmap, srcGrid ,width, height ) ;
      end ;
      srcGrid := nil ;
    end
    else begin
      Result := getBitmapData( _extent , _bitmap, width, height ) ;
      if not importMode then begin
        if makeTransparent then
          addTransparency(_bitmap, _width, _height) ;

        if makeRGBMap then
          simpleRGBCorrection(_bitmap, _width, _height) ;
        if makeFullRGBMap or makeColorsByVals then
          fullRGBCorrection(_bitmap, _width, _height) ;

        if assigned( FPixelOperation ) and Result then begin
          SetLength(dstpix, height*width ) ;
          if FPixelOperation( Self, _extent, _bitmap, dstpix, width, height) then
            GisCopyPixels(dstpix, 0, _bitmap, 0, width*height) ;
          dstpix := nil ;
        end ;

        if Params.Pixel.GrayScale or isGrayScaleImage then
          ARGB2Gray( _bitmap, width*height) ;
        if makeSomeCorrection or makeColorsByVals then
          fullCorrection(_bitmap, width, height) ;
      end ;
    end;
  end ;
end ;

function TGIS_LayerPixel.openBufferedFileStream(
  const _path : String
) : TGIS_HandleStream ;
begin
  {$IFDEF OXYGENE}
    Result := TGIS_BufferedFileStream.Create( _path, TGIS_StreamMode.Read ) ;
  {$ELSE}
    Result := TGIS_FileStream.Create( _path, fmOpenRead or fmShareDenyWrite ) ;
  {$ENDIF}
end ;

function TGIS_LayerPixel.GetAltitudeMapZone(
  const _zoneIndex : Integer
) : TGIS_AltitudeZone ;
begin
  if AltitudeMapZonesCount = 0 then
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BAD_CALL ), '_zoneIndex', 0 ) ;

  if lastAltitudeMapZoneSerial <> Params.Pixel.Serial then
    prepareAltitudeMapTable;

  Result := altitudeZones[_zoneIndex];
end;

function TGIS_LayerPixel.GetColorRamp(
  const _val : Single
) : TGIS_Color ;

  function apply_luminance(
    const _color     : TGIS_Color;
    const _luminance : Double
  ) : TGIS_Color ; {$IFDEF DCC} inline {$ENDIF}
  var
    h,s,l : Double ;
  begin
    _color.ToHSL( h, s, l ) ;
    Result :=  TGIS_Color.FromHSL( h, s, _luminance ) ;
  end ;

  function multiply_luminance(
    const _color  : TGIS_Color ;
    const _factor : Double
  ) : TGIS_Color ; {$IFDEF DCC} inline {$ENDIF}
  var
    h,s,l : Double ;
  begin
    _color.ToHSL( h, s, l ) ;
    Result := TGIS_Color.FromHSL( h, s, Max( 0, l * _factor ) ) ;
  end ;

  function apply_shadow(
    const _color : TGIS_Color
  ) : TGIS_Color ;
  var
    ds, lm : Double ;
    cc : TGIS_Color {$IFDEF GIS_NORECORDS} = new TGIS_Color {$ENDIF};
  begin
    cc := _color ;

    if (_val <> FNoDataValue) and (_val <> GIS_GRID_NOVALUE) then begin
      if IsNan( prevShadowValue ) then begin
        ds := 0 ;
      end
      else begin
        if IsNan( prevShadowDelta ) then
          ds := 0
        else
          ds := 2.0/3*prevShadowDelta + 1.0/3*(_val-prevShadowValue) ;
        prevShadowDelta := ds ;
      end ;
      lm := RadToDeg( ArcTan2( ds, sizeShadow ) ) ;
      if ds > 0 then begin
        lm := lm / FShadowAngle ;
      end
      else if ds < 0 then begin
        lm := lm / FShadowAngle * 3.0/2 ;
      end
      else
        lm := 0 ;

      cc := multiply_luminance( cc, (4.0/5 + lm ) )  ;
      Result := cc.FromARGB(cc.A, cc.R, cc.G, cc.B) ;
    end
    else begin
      prevShadowDelta := NaN ;

      Result := cc ;
    end ;

    prevShadowValue := _val ;
  end ;

  function colorFromAltitudeZones(
    const _val                : Single ;
    var   _lastColorZoneIndex : Integer
  ) : TGIS_Color ;
  var
    idx                 : Integer ;
    idx_low             : Integer ;
    idx_high            : Integer ;
    upper_altitude_zone : Integer ;
    is_val_above        : Boolean ;
    is_val_below        : Boolean ;
    min_val             : Single ;
    max_val             : Single ;
    mid_val             : Single ;
    mid_val_start       : Single ;
    mid_val_end         : Single ;
  begin
    upper_altitude_zone := high( altitudeZones ) ;

    idx := 0 ;
    min_val := 0 ;
    max_val := 0 ;
    is_val_below := False ;
    is_val_above := False ;
    idx_low := 0 ;
    idx_high := upper_altitude_zone ;

    while ( idx_low <= idx_high ) do begin

      // find zone index
      if _lastColorZoneIndex >= 0 then begin
        idx := _lastColorZoneIndex ;
        _lastColorZoneIndex := -1 ;
      end
      else
        idx := ( idx_high + idx_low ) div 2 ;

      min_val := altitudeZones[idx].MinVal ;
      max_val := altitudeZones[idx].MaxVal ;

      // if value is contained in 0-range zone, break immediately
      if GisIsSameValue( min_val, max_val, dZEpsilon ) and
         GisIsSameValue( min_val, _val, dZEpsilon ) then begin
        is_val_below := False ;
        is_val_above := False ;
        break ;
      end ;

      is_val_below := ( _val < min_val ) ;
      is_val_above := ( _val >= max_val ) ;

      if is_val_below then
        idx_high := idx - 1
      else if is_val_above then
        idx_low := idx + 1
      else
        break ;
    end ;

    // assign a color if the altitude zone was found,
    // or if the value is the upper limit of the last zone,
    // otherwise use colorNoData
    if ( ( not is_val_below ) and ( not is_val_above ) ) or
       ( GisIsSameValue( _val, max_val, dZEpsilon ) and ( idx = upper_altitude_zone ) )
    then begin
      _lastColorZoneIndex := idx ;

      // interpolate colors
      if gridSmoothColors then begin
        mid_val := ( min_val + max_val ) / 2 ;

        // find nearest zone
        if _val < mid_val then begin
          idx_low := Max( 0, idx - 1 ) ;
          idx_high := idx ;

          mid_val_start :=  ( altitudeZones[idx_low].MinVal + altitudeZones[idx_low].MaxVal ) / 2 ;
          mid_val_end := mid_val ;
        end
        else begin
          idx_low := idx ;
          idx_high := Min( upper_altitude_zone,  idx + 1 ) ;

          mid_val_start := mid_val ;
          mid_val_end := ( altitudeZones[idx_high].MinVal + altitudeZones[idx_high].MaxVal ) / 2 ;
        end ;

        Result := GradientColor(
          altitudeZones[idx_low].Color ,
          altitudeZones[idx_high].Color ,
          mid_val_start,
          mid_val_end,
          _val,
          TGIS_ColorInterpolationMode.RGB
        ) ;
      end
      else begin
        Result := altitudeZones[idx].Color ;
      end ;

      if isShadow then
        Result := apply_shadow( Result ) ;
    end
    else
      Result := colorNoData
  end;

begin
  {$IFDEF GIS_NORECORDS} Result := new TGIS_Color ; {$ENDIF}
  Result := colorNoData ;

  if _val = GIS_GRID_NOVALUE then
    exit ;

  // color ramp has higher priority
  if useColorRamp then begin
    Result := ColorFromColorRamp(
      assignedColorRamp,
      FMinZ,
      FMaxZ,
      _val,
      lastColorZoneIndex
    ) ;
    if isShadow then
      Result := apply_shadow( Result ) ;
    exit ;
  end
  // than altitude map zones
  else if useAltitudeZones then begin
    Result := colorFromAltitudeZones( _val, lastColorZoneIndex ) ;
    exit ;
  end ;

  if not GisIsSameValue( FMaxZ, FMinZ, dZEpsilon ) then begin
    Result := TGIS_Color.Gray ;

    if isShadow then
      Result := apply_shadow( Result )
    else
      Result := apply_luminance(
        Result,
        ( _val -FMinZ ) / ( FMaxZ - FMinZ ) * 4.0/5 + 1.0/5
      ) ;
  end
  else if not GisIsSameValue( FMaxZ, FNoDataValue, dZEpsilon ) then begin
    // degenereted case FMinZ = FMaxz
    Result := TGIS_Color.Gray ;

    if isShadow then
      Result := apply_shadow( Result )
    else
      Result := apply_luminance(
        Result,
        ( _val - FMinZ ) / ( 1 ) * 4.0/5 + 1.0/5
      ) ;
  end ;
end ;

procedure TGIS_LayerPixel.doDestroy ;
begin
  self.RevertAll ;

  FreeObject( tempFileStream  ) ;
  if FileExists(tempFileName) then
    DeleteFile(tempFileName) ;
  FreeObject( FCuttingPolygon ) ;
  FreeObject( bandsDefinition ) ;
  FreeObject( fileStream      ) ;
  FreeObject( FCapabilities   ) ;
  FreeObject( tmCache         ) ;

  inherited ;
end ;

procedure TGIS_LayerPixel.GenerateRamp(
  const _startColor     : TGIS_Color ;
  const _middleColor    : TGIS_Color ;
  const _endColor       : TGIS_Color ;
  const _minValue       : Double ;
  const _midValue       : Double ;
  const _maxValue       : Double ;
  const _useMiddle      : Boolean ;
  const _rampInterval   : Double ;
  const _legendInterval : Double ;
  const _params         : TGIS_ParamsSectionPixel ;
  const _clearMapZones  : Boolean
) ;
begin
  GenerateRamp(
    _startColor,
    _middleColor,
    _endColor,
    _minValue,
    _midValue,
    _maxValue,
    _useMiddle,
    _rampInterval,
    _legendInterval,
    _params,
    _clearMapZones,
    TGIS_ColorInterpolationMode.RGB
  ) ;
 end;

procedure TGIS_LayerPixel.GenerateRamp(
  const _startColor     : TGIS_Color ;
  const _middleColor    : TGIS_Color ;
  const _endColor       : TGIS_Color ;
  const _minValue       : Double ;
  const _midValue       : Double ;
  const _maxValue       : Double ;
  const _useMiddle      : Boolean ;
  const _rampInterval   : Double ;
  const _legendInterval : Double ;
  const _params         : TGIS_ParamsSectionPixel ;
  const _clearMapZones  : Boolean ;
  const _colorSpace     : TGIS_ColorInterpolationMode
 ) ;
const
  MAX_NUM_INTERVALS = 1000 ;
var
  prm      : TGIS_ParamsSectionPixel ;
  delta    : Double ;
  zone_gen : T_AltitudeZoneGenerator ;
  min_ramp : String ;
begin
  delta := _maxValue - _minValue ;

  // check inputs
  if delta <= dZEpsilon then
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_GRIDWIZARD_MINMAX ), '', 0 )  ;

  if _useMiddle and
     ( ( _midValue - _minValue <= dZEpsilon ) or
       ( _maxValue - _midValue <= dZEpsilon ) )
  then
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_GRIDWIZARD_MIDDLE ), '', 0 )  ;

  min_ramp := FloatToStr( delta / MAX_NUM_INTERVALS ) ;
  if ( _rampInterval <= dZEpsilon ) or
     ( CeilS( delta / _rampInterval ) > MAX_NUM_INTERVALS )
  then
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_GRIDWIZARD_RAMPINTERVAL ), min_ramp, 0 ) ;

  if ( _legendInterval <= dZEpsilon ) or
     ( CeilS( delta / _legendInterval ) > MAX_NUM_INTERVALS )
  then
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_GRIDWIZARD_LEGENDINTERVAL ), min_ramp, 0 ) ;

  prm := TGIS_ParamsSectionPixel.Create ;
  zone_gen := T_AltitudeZoneGenerator.Create(
    _colorSpace,
    _minValue,
    _maxValue,
    dZEpsilon
  ) ;
  try
    if _useMiddle then begin
      zone_gen.GenerateZoneDefinitions(
        _startColor,
        _middleColor,
        _minValue,
        _midValue,
        True,
        _rampInterval,
        _legendInterval
      ) ;

      zone_gen.GenerateZoneDefinitions(
        _middleColor,
        _endColor,
        _midValue,
        _maxValue,
        False,
        _rampInterval,
        _legendInterval
      ) ;
    end
    else begin
      // no middle (only start and end)
      zone_gen.GenerateZoneDefinitions(
        _startColor,
        _endColor,
        _minValue,
        _maxValue,
        False,
        _rampInterval,
        _legendInterval
      ) ;
    end ;

    prm.Pixel.AltitudeMapZones.AddStrings( zone_gen.ZoneDefinitions ) ;

    // leave the original Params
    prm.FalseZAsText      := Params.FalseZAsText ;
    prm.ScaleZ            := Params.ScaleZ ;
    prm.NormalizedZ       := Params.NormalizedZ ;
    prm.Pixel.GridBand    := Params.Pixel.GridBand    ;
    prm.Pixel.GridNoValue := Params.Pixel.GridNoValue ;
    prm.Pixel.GridShadow  := Params.Pixel.GridShadow ;
    prm.Pixel.Antialias   := Params.Pixel.Antialias ;
    prm.Pixel.ShowLegend  := True ;

    if _clearMapZones then begin
      if assigned( _params ) then
        _params.Assign( prm )
      else
        Params.Assign( prm ) ;
    end
    else begin
      if assigned( _params ) then
        _params.Pixel.AltitudeMapZones.AddStrings( prm.Pixel.AltitudeMapZones )
      else
        Params.Pixel.AltitudeMapZones.AddStrings( prm.Pixel.AltitudeMapZones ) ;
    end ;
  finally
    FreeObject( zone_gen ) ;
    FreeObject( prm ) ;
  end ;
end ;

procedure TGIS_LayerPixel.GenerateRampEx(
  const _minValue : Double ;
  const _maxValue : Double ;
  const _colorMap : TGIS_ColorMapArray ;
  const _params   : TGIS_ParamsSectionPixel
) ;
begin
  GenerateRampEx(
    _minValue,
    _maxValue,
    _colorMap,
    _params,
    TGIS_ColorInterpolationMode.RGB
  ) ;
end;

procedure TGIS_LayerPixel.GenerateRampEx(
  const _minValue    : Double ;
  const _maxValue    : Double ;
  const _colorMap    : TGIS_ColorMapArray ;
  const _params      : TGIS_ParamsSectionPixel ;
  const _cInterpMode : TGIS_ColorInterpolationMode
 ) ;
const
  MAX_LEGEND_COUNT = 10 ;
var
  delta              : Double ;
  prm                : TGIS_ParamsSectionPixel ;
  zone_gen           : T_AltitudeZoneGenerator ;
  color_map_len      : Word ;
  is_last_colormap   : Boolean ;
  idx                : Integer ;
  zone_color         : TGIS_Color {$IFDEF GIS_NORECORDS} = new TGIS_Color {$ENDIF} ;
  zone_min           : Double ;
  zone_max           : Double ;
  curr_color         : TGIS_Color {$IFDEF GIS_NORECORDS} = new TGIS_Color {$ENDIF} ;
  curr_min           : Double ;
  curr_max           : Double ;
  add_zone_legend    : Boolean ;
  legend_factor      : Double ;
  next_legend_factor : Double ;
begin
  delta := _maxValue - _minValue ;
  if delta < dZEpsilon then
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_GRIDWIZARD_MINMAX ), '', 0 ) ;

  prm := TGIS_ParamsSectionPixel.Create ;
  zone_gen := T_AltitudeZoneGenerator.Create(
    _cInterpMode, _minValue, _maxValue, dZEpsilon
  ) ;
  try
    color_map_len := length( _colorMap ) ;

    // first zone
    curr_color := _colorMap[0].RGB ;
    curr_min := _minValue + delta * _colorMap[0].Index / 100 ;
    if color_map_len = 1 then
      curr_max := _minValue + delta * _colorMap[0].Index / 100
    else
      curr_max := _minValue + delta * _colorMap[1].Index / 100 ;

    next_legend_factor := 0 ;
    for idx := 1 to color_map_len - 1 do begin
      is_last_colormap := ( idx = color_map_len - 1 ) ;

      // zone characteristics
      zone_color := _colorMap[idx].RGB ;
      zone_min := _minValue + delta * _colorMap[idx].Index / 100 ;

      if is_last_colormap then
        zone_max := _maxValue
      else
        zone_max := _minValue + delta * _colorMap[idx+1].Index / 100 ;

      // merge overlapping zones
      if ( zone_color = curr_color ) and
        GisIsSameValue( zone_min, curr_max, dZEpsilon ) then
      begin
        curr_max := zone_max ;
      end
      else begin
        // zone format
        // set max 10 legend items
        legend_factor := idx / color_map_len * MAX_LEGEND_COUNT ;
        if ( legend_factor > next_legend_factor ) then begin
          add_zone_legend := True ;
          next_legend_factor := legend_factor + 1 ;
        end
        else begin
          add_zone_legend := False ;
        end ;

        zone_gen.AddZoneDefinition(
          curr_min, curr_max, curr_color, add_zone_legend, False
        ) ;

        curr_min := zone_min ;
        curr_max := zone_max ;
        curr_color := zone_color ;
      end ;
    end ;

    // add last zone definition
    zone_gen.AddZoneDefinition( curr_min, curr_max, curr_color, True, True ) ;

    prm.Pixel.AltitudeMapZones.AddStrings( zone_gen.ZoneDefinitions ) ;

    // leave the original Params
    prm.FalseZAsText      := Params.FalseZAsText ;
    prm.ScaleZ            := Params.ScaleZ ;
    prm.NormalizedZ       := Params.NormalizedZ ;
    prm.Pixel.GridBand    := Params.Pixel.GridBand ;
    prm.Pixel.GridNoValue := Params.Pixel.GridNoValue ;
    prm.Pixel.GridShadow  := Params.Pixel.GridShadow ;
    prm.Pixel.Antialias   := Params.Pixel.Antialias ;
    prm.Pixel.ShowLegend  := True ;

    // set only GridSmoothColors param,according to _cInterpMode
    if _cInterpMode = TGIS_ColorInterpolationMode.None then
      prm.Pixel.GridSmoothColors := False
    else
      prm.Pixel.GridSmoothColors := True ;

    if assigned( _params ) then
      _params.Assign( prm )
    else
      Params.Assign( prm ) ;
  finally
    FreeObject( zone_gen ) ;
    FreeObject( prm ) ;
  end ;
end ;

function TGIS_LayerPixel.MapGridValue(
  const _params : TGIS_ParamsSectionPixel ;
  const _val    : Single
) : TGIS_Color ;
begin
  Result := MapColorValue(_params, GetColorRamp(_val)) ;
end ;

function TGIS_LayerPixel.ReadGridLine(
  var   _buffer   : TGIS_SingleArray ;
  const _line     : Integer          ;
  const _left     : Integer          ;
  const _count    : Integer
) : Integer ;
begin
  Alive ;
  if (_left < 0) or (_count <= 0) then begin
    Result := 0 ;
    exit ;
  end ;

  if _line >= baseCellHeight then begin
    Result := 0 ;
    exit ;
  end ;
  if (_left + _count > baseCellWidth) then begin
    Result := 0 ;
    exit ;
  end ;

  setFileScale(1.0, 1.0) ;
  if (Params.Pixel.GridBand > 0) or FIsNativeGridImage then
    Result := getNativeLine( _buffer, _line, _left, _count )
  else
    Result := getNativeLineGrayGrid( _buffer, _line, _left, _count ) ;
end ;

function TGIS_LayerPixel.MapColorValue(
  const _params : TGIS_ParamsSectionPixel ;
  const _color  : TGIS_Color
) : TGIS_Color ;
var
  make_some_correction : Boolean ;
  make_transparent     : Boolean ;
  make_red_map         : Boolean ;
  make_green_map       : Boolean ;
  make_blue_map        : Boolean ;
  make_fullRGB_map     : Boolean ;
  make_RGB_map         : Boolean ;
begin
  Result := _color ;

  if IsGridImage then begin
    prepareAltitudeMapTable ;
    prepareColorRamp ;
  end ;

  make_transparent := (_params.Pixel.TransparentZones.Count > 0) or
                      (internalTransparentColor.A  = 0 ) ;
  makeGrayMap      := _params.Pixel.GrayMapZones.Count     > 0 ;

  make_red_map     := _params.Pixel.RedMapZones.Count      > 0 ;
  make_green_map   := _params.Pixel.GreenMapZones.Count    > 0 ;
  make_blue_map    := _params.Pixel.BlueMapZones.Count     > 0 ;

  make_fullRGB_map := _params.Pixel.FullRGBMapZones.Count  > 0 ;

  if make_fullRGB_map then
    prepFullRGBMapTbl ;

  make_RGB_map := make_red_map   or
                  make_green_map or
                  make_blue_map  or
                  ( _params.Pixel.Red        <> 0 ) or
                  ( _params.Pixel.Blue       <> 0 ) or
                  ( _params.Pixel.Green      <> 0 ) or
                  ( _params.Pixel.Brightness <> 0 ) or
                  ( _params.Pixel.Contrast   <> 0 )
                  ;

  make_some_correction := make_transparent        or
                          make_RGB_map            or
                          _params.Pixel.Inversion or
                          _params.Pixel.Histogram or
                          _params.Pixel.GrayScale ;
  if _params.Pixel.Histogram and not isHistogram then
      makeHistogram;

  if make_some_correction then
    prepFinalCorTable;

  if make_transparent then
    if _color.A = 0 then
      prepTransparent ;

end ;

procedure TGIS_LayerPixel.RecalcProjectedExtent ;
var
  do_rot  : Boolean ;
  do_proj : Boolean ;
  trans   : T_TransClass ;
  ptg1,
  ptg2,
  ptg3,
  ptg4    : TGIS_Point ;
  val_f_x,
  val_f_y : Double ;

  procedure set_validity_factors ;
  var
    lcs  : TGIS_CSCoordinateSystem ;
    pcs  : TGIS_CSProjectedCoordinateSystem ;
    vext : TGIS_Extent ;
    lext : TGIS_Extent ;
  begin
    val_f_x := 1.0 ;
    val_f_y := 1.0 ;
    if not assigned(Viewer) then
      exit ;
    if CS is TGIS_CSUnknownCoordinateSystem then
      exit ;
    lcs := Viewer.Ref.CS ;
    if not activeTransform then begin
      if lcs is TGIS_CSProjectedCoordinateSystem then begin
          pcs := TGIS_CSProjectedCoordinateSystem( lcs ) ;
          vext := _TGIS_Extent(pcs.ValidityExtentWGS) ;
          lext := self.CS.ExtentToWGS(FExtent) ;
          vext := GisCommonExtent(vext, lext) ;
          if vext.XMax <> vext.XMin then
            val_f_x := (vext.XMax -vext.XMin)/(lext.XMax - lext.XMin) ;
          if vext.YMax <> vext.YMin then
            val_f_y := (vext.YMax -vext.YMin)/(lext.YMax - lext.YMin) ;
      end ;
    end ;
  end ;

  procedure recalc_params ;
  var
    dwt, dht,
    dcw        : Double ;
    bcw, bch   : Double ;
    area       : Double ;
    earea      : Double ;
    edcw       : Double ;
    ebcw, ebch : Double ;
    rext       : TGIS_Extent ;
    rtxf, rtyf : Double ;
  begin
    dwt := FProjectedExtent.XMax - FProjectedExtent.XMin ;
    dht := FProjectedExtent.YMax - FProjectedExtent.YMin ;
    if (dwt <= 0) or (dht <= 0) then
      exit ;

    set_validity_factors ;

    if  do_rot  then begin
      rext := Viewer.Ref.RotatedExtent( baseProjectedExtent ) ;
      rtxf := (rext.XMax -rext.XMin)/
              (baseProjectedExtent.XMax -baseProjectedExtent.XMin) ;
      rtyf := (rext.YMax -rext.YMin)/
              (baseProjectedExtent.YMax -baseProjectedExtent.YMin) ;
    end
    else begin
      rtxf := 1 ;
      rtyf := 1 ;
    end ;

    if baseCellWidth > 0 then begin
      bcw  := baseCellWidth*val_f_x*rtxf ;
      ebcw := FBitWidth*val_f_x*rtxf ;
    end
    else begin
      bcw  := 1 ;
      ebcw := 1 ;
    end;
    if baseCellHeight > 0 then begin
      bch  := baseCellHeight*val_f_y*rtyf ;
      ebch := FBitHeight*val_f_y*rtyf ;
    end
    else begin
      bch  := 1 ;
      ebch := 1 ;
    end;
    area  := bcw * bch ;
    earea := ebcw * ebch ;
    dcw   := Sqrt((area*dwt)/dht) ;
    edcw  := Sqrt((earea*dwt)/dht) ;

    FCellWidth  := RoundS(dcw) ;
    FCellHeight := RoundS(area/dcw) ;

    assert( FCellWidth > 0 ) ;
    assert( FCellHeight > 0 ) ;

    scaleX :=   dwt / RoundS(edcw) ;
    scaleY :=  -dht / RoundS(earea/edcw) ;

    assert( Abs( scaleX ) > 0 ) ;
    assert( Abs( scaleY ) > 0 ) ;
  end ;

begin
  if not ( assigned( Viewer ) ) or GisIsNoWorld( FExtent ) then begin
    ProjectedExtent := FExtent ;
    FProjectedExtentViewerEPSG  := -1 ;
    FProjectedExtentEPSG        := -1 ;
    resetMustReproject ;
    exit ;
  end ;

  if (FBitWidth = 0) or (FBitHeight = 0) then
    exit ;

  resetMustReproject ;

  if not assigned( self.FTransform ) then
    activeTransform := False
  else begin
    if self.FTransform.Active then
      activeTransform := True
    else
      activeTransform := False ;
  end ;
  if assigned( Viewer ) then begin
    if ( FProjectedExtentViewerEPSG    = Viewer.Ref.CS.EPSG        ) and
       ( FProjectedExtentEPSG          = CS.EPSG                   ) and
       ( FProjectedExtentRotationAngle = Viewer.Ref.RotationAngle  ) and
       ( GisIsSamePoint ( FProjectedExtentRotationPoint ,
                          Viewer.Ref.RotationPoint
                        )                                          ) and
       ( not activeTransform                                       ) and
       ( not wasTransform                                          ) and
       ( GisIsSameExtent( FProjectedExtentBase, FExtent )        ) then
    begin
      if FProjectedExtent.XMin = FProjectedExtent.XMax then
        FProjectedExtent := _TGIS_Extent(FExtent) ;
      if baseProjectedExtent.XMin = baseProjectedExtent.XMax then
        baseProjectedExtent := _TGIS_Extent(FExtent) ;
    end ;
  end ;
  if  FExtent.XMax <= FExtent.XMin then
    exit ;

  wasTransform := activeTransform ;

  do_proj := (assigned( Viewer ) and
             (((Viewer.Ref.CS.EPSG <> 0) and (Viewer.Ref.CS.EPSG <> CS.EPSG)))) or
             activeTransform ;

  do_rot  := assigned( Viewer ) and ( Viewer.Ref.RotationAngle <> 0 ) ;

  if do_proj or do_rot or baseRotation or internalRotation then begin

    if (baseCellWidth = 0) or (baseCellHeight = 0) then begin
      FCellWidth  := FBitWidth ;
      FCellHeight := FBitHeight ;

      baseCellWidth  := FBitWidth ;
      baseCellHeight := FBitHeight ;
      exit ;
    end  ;

    trans := T_TransClass.Create(self) ;
    try
      if assigned( Viewer ) and forViewer then
        outCS := Viewer.Ref.CS ;

      if do_proj or baseRotation  or internalRotation then
        baseProjectedExtent := trans.projectedExt( FExtent )
      else
        baseProjectedExtent := _TGIS_Extent(FExtent) ;

      if FBitWidth <> 0 then
        scaleX := ( baseProjectedExtent.XMax - baseProjectedExtent.XMin ) /
                    FBitWidth ;
      if FBitHeight <> 0 then
        scaleY := -( baseProjectedExtent.YMax - baseProjectedExtent.YMin ) /
                     FBitHeight ;

      if not do_rot then begin
        FProjectedExtent := _TGIS_Extent(baseProjectedExtent) ;

        if not GisIsEmptyExtent( FProjectedExtent ) and
          ( ( FBitWidth <> 0 ) and ( FBitHeight <> 0 ) ) then
          recalc_params ;
      end
      else begin
        if do_proj then
          FProjectedExtent := Viewer.Ref.RotatedExtent( baseProjectedExtent )
        else begin
          ptg1 := trans.projected( GisPoint( FExtent.XMin, FExtent.YMin ) ) ;
          ptg2 := trans.projected( GisPoint( FExtent.XMin, FExtent.YMax ) ) ;
          ptg3 := trans.projected( GisPoint( FExtent.XMax, FExtent.YMax ) ) ;
          ptg4 := trans.projected( GisPoint( FExtent.XMax, FExtent.YMin ) ) ;

          FProjectedExtent.XMin := Min( Min(Min(ptg1.X,ptg2.X),ptg3.X),ptg4.X ) ;
          FProjectedExtent.YMin := Min( Min(Min(ptg1.Y,ptg2.Y),ptg3.Y),ptg4.Y ) ;
          FProjectedExtent.XMax := Max( Max(Max(ptg1.X,ptg2.X),ptg3.X),ptg4.X ) ;
          FProjectedExtent.YMax := Max( Max(Max(ptg1.Y,ptg2.Y),ptg3.Y),ptg4.Y ) ;
        end ;
        recalc_params ;
      end ;
    finally
      FreeObject( trans ) ;
    end ;
  end
  else begin
    baseProjectedExtent := _TGIS_Extent(FExtent) ;
    FProjectedExtent    := _TGIS_Extent(FExtent) ;
    FCellWidth          := baseCellWidth ;
    FCellHeight         := baseCellHeight ;

    if FBitWidth <> 0 then
      scaleX := ( FExtent.XMax - FExtent.XMin ) / FBitWidth  ;
    if FBitHeight <> 0 then
      scaleY := -( FExtent.YMax - FExtent.YMin ) / FBitHeight ;
  end ;

  if assigned(Viewer) then begin
    FProjectedExtentViewerEPSG    := Viewer.Ref.CS.EPSG ;
    FProjectedExtentRotationAngle := Viewer.Ref.RotationAngle ;
    FProjectedExtentRotationPoint := Viewer.Ref.RotationPoint ;
  end ;
  FProjectedExtentEPSG := CS.EPSG ;
  FProjectedExtentBase := _TGIS_Extent(FExtent) ;
end ;

procedure TGIS_LayerPixel.SetWorld(
  const _a : Double ;
  const _d : Double ;
  const _b : Double ;
  const _e : Double ;
  const _c : Double ;
  const _f : Double
) ;
var
  ext  : TGIS_Extent {$IFDEF GIS_NORECORDS} := new TGIS_Extent {$ENDIF} ;
  yrot : Double ;
  xrot : Double ;
  rot90, recalc : Boolean ;
  aw, bw, cw  : Double ;
  dw, ew, fw : Double ;
begin
  aw := _a ;
  dw := _d ;
  bw := _b ;
  ew := _e ;
  cw := _c ;
  fw := _f ;

  rot90 := False ;
  baseRotation := False ;
  recalc := False ;
  yrot  := dw ;
  xrot  := bw ;
  scaleX := aw ;
  if aw < 0 then begin
    scaleXFactor := -1 ;
    scaleX := -scaleX ;
    baseRotation := True ;
  end
  else
  if aw = 0 then begin
    rot90 := True ;
    scaleXFactor := 0 ;
    scaleX := dw ;
    if scaleX < 0 then begin
      scaleX := -scaleX ;
    end ;
    baseRotation := True ;
  end ;
  scaleY := ew ;
  if ew > 0 then begin
    scaleYFactor := -1 ;
    scaleY:= -scaleY;
    baseRotation := True ;
  end
  else
  if ew = 0 then begin
    scaleY := bw ;
    if scaleY > 0 then begin
      scaleY := -scaleY ;
    end ;
    scaleYFactor := 0 ;
    baseRotation := True ;
  end ;

  ext.XMin := cw -0.5*scaleX;
  ext.XMax := ext.XMin +scaleX*FBitWidth ;

  ext.YMax := fw -0.5*scaleY;;
  ext.YMin := ext.YMax +scaleY*FBitHeight ;
  if (xrot <> 0) or (yrot <> 0) or baseRotation then begin
    if scaleX <> 0 then
      yRotDivSc := yrot/scaleX ;
    if scaleY <> 0 then
      xRotDivSc := xrot/scaleY ;
    scxF_yRotDivScxRotDivSMcsyF := scaleXFactor - yRotDivSc*xRotDivSc*scaleYFactor ;
    scyF_yRotDivScxRotDivSMcsxF := scaleYFactor - yRotDivSc*xRotDivSc*scaleXFactor ;
    if (scxF_yRotDivScxRotDivSMcsyF <> 0) and (scyF_yRotDivScxRotDivSMcsxF <> 0)then
      baseRotation := True
    else begin
      if rot90 then begin
          if dw > 0 then begin
            xRotDivSc := 1 ; //right
            yRotDivSc := 1 ;
          end
          else begin
            xRotDivSc := -1 ; //left
            yRotDivSc := -1 ;
          end;
          if bw*dw > 0 then
           xRotDivSc := -xRotDivSc ;

      end
      else
        baseRotation := False ;
    end ;
  end ;
  FExtent := _TGIS_Extent(ext) ;
  if assigned( Viewer ) then begin
    Viewer.Ref.RecalcExtent ;
  end
  else begin
    if baseRotation then
      RecalcProjectedExtent
    else begin
      FProjectedExtent := _TGIS_Extent(FExtent) ;
      baseProjectedExtent := _TGIS_Extent(FExtent) ;
    end;
  end;
end ;

function TGIS_LayerPixel.readDoubleLE : Double ;
begin
  fileStream.ReadDouble( Result ) ;
end ;

function TGIS_LayerPixel.readDoubleBE : Double ;
var
  {$IFDEF OXYGENE}
    b : Byte    ;
  {$ENDIF}
  i : Integer ;
  a : array of Byte ;
begin
  SetLength( a, 8 ) ;
  assert( length(a) = 8 ) ;
  assert( sizeOf(Double) = 8 ) ;
  {$IFDEF OXYGENE}
    fileStream.Read( a, 8 ) ;

    for i := 0 to 3 do begin
      b      := a[i]   ;
      a[i]   := a[7-i] ;
      a[7-i] := b      ;
    end ;
    Result := BitConverter.ToDouble( a, 0 ) ;
  {$ELSE}
    fileStream.Read( a[0], 8 ) ;

    for i:=0 to 7 do // swap bytes
      Byte(Pointer( NativeInt( @Result ) + i)^) := a[7 -i] ;
  {$ENDIF}
end ;

function TGIS_LayerPixel.basePointRot(
  const _pt : TGIS_Point
) : TGIS_Point ;
var
    tmp : TGIS_Point ;
begin
  {$IFDEF GIS_NORECORDS}Result := new TGIS_Point;{$ENDIF}
  if baseRotation then begin
    if scxF_yRotDivScxRotDivSMcsyF <> 0 then begin
      Result.X := scaleXFactor*_pt.X +cntrRotFactX  +(_pt.Y -cntrRotY)*xRotDivSc ;
      Result.Y := scaleYFactor*_pt.Y +cntrRotFactY  +(_pt.X -cntrRotX)*yRotDivSc ;
    end
    else begin
      Result.X := xRotDivSc*_pt.Y ;
      Result.Y := yRotDivSc*_pt.X ;
    end;
    tmp := _TGIS_Point(Result) ;
  end
  else
    tmp := _TGIS_Point(_pt) ;

  if internalRotation then begin
    if FRotationAngle = 0 then
      exit ;

    Result.X := ( ( tmp.X - rotationPoint.X ) * rotateCos  -
                  ( tmp.Y - rotationPoint.Y ) * rotateSin
                ) + rotationPoint.X ;
    Result.Y := ( ( tmp.X - rotationPoint.X ) * rotateSin  +
                  ( tmp.Y - rotationPoint.Y ) * rotateCos
                ) + rotationPoint.Y ;

  end ;
end ;

function TGIS_LayerPixel.basePointUnRot(
  const _pt : TGIS_Point
) : TGIS_Point ;
var
    tmp : TGIS_Point ;
begin
  {$IFDEF GIS_NORECORDS}Result := new TGIS_Point;{$ENDIF}
  if internalRotation then begin
    if FRotationAngle <> 0 then begin
      Result.X := ( ( _pt.X - rotationPoint.X ) * unrotateCos  -
                  ( _pt.Y - rotationPoint.Y ) * unrotateSin
                ) + rotationPoint.X ;
      Result.Y := ( ( _pt.X - rotationPoint.X ) * unrotateSin  +
                ( _pt.Y - rotationPoint.Y ) * unrotateCos
              ) + rotationPoint.Y ;
      tmp := _TGIS_Point(Result) ;
    end
    else
      tmp := _TGIS_Point(_pt) ;
  end
  else
    tmp := _TGIS_Point(_pt) ;

  if baseRotation then begin
    if (scxF_yRotDivScxRotDivSMcsyF <> 0) then begin
      Result.X := (tmp.X -cntrRotFactX -xRotDivSc*(scaleYFactor*(tmp.Y -cntrRotFactY +yRotDivSc*cntrRotX) -cntrRotY))/
                  scxF_yRotDivScxRotDivSMcsyF ;

      Result.Y := (tmp.Y -cntrRotFactY -yRotDivSc*(scaleXFactor*(tmp.X -cntrRotFactX +xRotDivSc*cntrRotY) -cntrRotX))/
                  scyF_yRotDivScxRotDivSMcsxF ;
    end
    else begin
      Result.X := tmp.Y/yRotDivSc ;

      Result.Y := tmp.X/xRotDivSc ;
    end;
  end;
end ;

function TGIS_LayerPixel.baseRotatedExtent(
  const _ext : TGIS_Extent
) : TGIS_Extent ;
var
  ptg1, ptg2, ptg3, ptg4 : TGIS_Point ;
begin
  ptg1 := basePointRot( GisPoint( _ext.XMin, _ext.YMin ) ) ;
  ptg2 := basePointRot( GisPoint( _ext.XMin, _ext.YMax ) ) ;
  ptg3 := basePointRot( GisPoint( _ext.XMax, _ext.YMax ) ) ;
  ptg4 := basePointRot( GisPoint( _ext.XMax, _ext.YMin ) ) ;

  {$IFDEF GIS_NORECORDS}Result := new TGIS_Extent;{$ENDIF}
  Result.XMin := Min( Min( Min( ptg1.X, ptg2.X ), ptg3.X ), ptg4.X ) ;
  Result.YMin := Min( Min( Min( ptg1.Y, ptg2.Y ), ptg3.Y ), ptg4.Y ) ;
  Result.XMax := Max( Max( Max( ptg1.X, ptg2.X ), ptg3.X ), ptg4.X ) ;
  Result.YMax := Max( Max( Max( ptg1.Y, ptg2.Y ), ptg3.Y ), ptg4.Y ) ;
end ;

function TGIS_LayerPixel.baseUnrotatedExtent(
  const _ext : TGIS_Extent
) : TGIS_Extent ;
var
  ptg1, ptg2, ptg3, ptg4 : TGIS_Point ;
begin
  ptg1 := basePointUnRot( GisPoint( _ext.XMin, _ext.YMin ) ) ;
  ptg2 := basePointUnRot( GisPoint( _ext.XMin, _ext.YMax ) ) ;
  ptg3 := basePointUnRot( GisPoint( _ext.XMax, _ext.YMax ) ) ;
  ptg4 := basePointUnRot( GisPoint( _ext.XMax, _ext.YMin ) ) ;

  {$IFDEF GIS_NORECORDS}Result := new TGIS_Extent;{$ENDIF}
  Result.XMin := Min( Min( Min( ptg1.X, ptg2.X ), ptg3.X ), ptg4.X ) ;
  Result.YMin := Min( Min( Min( ptg1.Y, ptg2.Y ), ptg3.Y ), ptg4.Y ) ;
  Result.XMax := Max( Max( Max( ptg1.X, ptg2.X ), ptg3.X ), ptg4.X ) ;
  Result.YMax := Max( Max( Max( ptg1.Y, ptg2.Y ), ptg3.Y ), ptg4.Y ) ;
end ;

procedure TGIS_LayerPixel.ApplyCuttingPolygon(
  const _wkt : String
) ;
begin
  if IsStringEmpty( _wkt ) then
    CuttingPolygon := nil
  else
    CuttingPolygon := TGIS_ShapePolygon (
                        TGIS_GeometryFactory.GisCreateShapeFromWKT( _wkt )
                      ) ;
end ;

procedure TGIS_LayerPixel.ARGB2Gray(
  const _pixels : TGIS_Pixels  ;
  const _size   : Integer
) ;
var
  i       : Integer ;
  pix     : Integer ;
  red,
  green,
  blue,
  gray    : Integer    ;
begin
  if (not makeGrayMap) and (not Params.Pixel.GrayScale) then
    exit ;

   for i := 0 to _size -1  do begin
     pix := _pixels[i] ;

     if  isGrayScaleImage  and (not Params.Pixel.GrayScale) then begin
       gray := pix and $FF ;
     end
     else begin
       red   := (corRGB[(pix and $00FF0000) shr 16 ] and $00FF0000) shr 16 ;
       green := (corRGB[(pix and $0000FF00) shr 08 ] and $0000FF00) shr 8 ;
       blue  := (corRGB[(pix and $000000FF)        ] and $000000FF) ;
       if (red <> green) or (green <> blue) then begin
         gray := (Integer(paletteCpy[red  ].R)) +
               (Integer(paletteCpy[green].G)) +
               (Integer(paletteCpy[blue ].B)) ;
        if gray > 255 then
          gray := 255 ;
       end
       else
        gray := green ;
     end;
    if makeGrayMap then begin
      _pixels[i] := (pix and Integer($FF000000)) or mapGray2RGB[gray] ;
     end
     else begin
         _pixels[i] := (pix and Integer($FF000000)) or
                       (gray shl 16) or (gray shl 08) or gray ;
     end ;
   end ;
end ;

function TGIS_LayerPixel.drawExtentEx
  : TGIS_Extent ;
var
  {$IFDEF CLR}
    rct : TRect ;
  {$ELSE}
    rct : TRect ;
  {$ENDIF}
  margin : Integer ;
begin
  with Viewer do begin
    rct := TGIS_RendererAbstract(
             Viewer.Ref.ViewerParent.ControlRenderer
           ).ViewRect ;
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Extent ;
    {$ENDIF}

    if ExtentPixelAdjustment then begin
      rct := Rect( rct.Left, rct.Top, rct.Right + 1, rct.Top + 1 )
    end;

    margin := Ref.OverlappedExtentMargin ;
    if margin > 0 then
      margin := 3 ;
    Result.XMin := Ref.Extent.XMin + Ref.Viewport.X +
                   ( rct.Left   - margin ) / Ref.Zoom ;
    Result.XMax := Ref.Extent.XMin + Ref.Viewport.X +
                   ( rct.Right  + margin ) / Ref.Zoom ;
    Result.YMin := Ref.Extent.YMax - Ref.Viewport.Y -
                   ( rct.Bottom + margin ) / Ref.Zoom ;
    Result.YMax := Ref.Extent.YMax - Ref.Viewport.Y -
                   ( rct.Top    - margin ) / Ref.Zoom ;

    if GisIsCommonExtent( Result, Ref.Extent ) then
      Result := GisCommonExtent( Result, Ref.Extent )
    else
      Result := GisNoWorld ;
  end ;
end ;

procedure TGIS_LayerPixel.prepFullRGBMapTbl ;
var
  i, s,
  start,
  stop      : Integer;
  startW    : TGIS_Color {$IFDEF GIS_NORECORDS} = new TGIS_Color {$ENDIF};
  stopW     : TGIS_Color {$IFDEF GIS_NORECORDS} = new TGIS_Color {$ENDIF};
  colorW    : TGIS_Color {$IFDEF GIS_NORECORDS} = new TGIS_Color {$ENDIF};
  par       : T_ParseZones ;
  pixparams : TGIS_ParamsSectionPixel ;
  mask      : Int64 ;
begin
  pixparams := TGIS_ParamsSectionPixel(Params) ;
  for i := 0 to 255 do begin
    corFullRGB[i] := (i shl 16) or (i shl 8) or i ;
  end ;

  if pixparams.Pixel.FullRGBMapZones.Count > 0 then begin
    mask := 1 ;
    for i := 0 to pixparams.Pixel.FullRGBMapZones.Count -1 do begin
      par := T_ParseZones.Create(pixparams.Pixel.FullRGBMapZones.Strings[i]) ;
      if par.Count < 3 then begin
        FreeObject( par ) ;
        continue ;
      end ;
      startW := par.ReadRGB(0) ;
      stopW  := par.ReadRGB(1) ;
      colorW := par.ReadRGB(2) ;

      //Red
      start := startW.R ;
      stop  := stopW.R ;

      if start > stop then begin
        s     := start ;
        start := stop  ;
        stop  := s     ;
      end ;
      for s := start to stop do begin
        corFullRGB[s] := (Integer(colorW.R) shl 16) or (corFullRGB[s] and $00FFFF) ;
        if i = 0 then
          isCorFullRGBred[s] := mask
        else
          isCorFullRGBred[s] := isCorFullRGBred[s] or mask ;
      end ;

      //Green
      start := startW.G ;
      stop  := stopW.G ;

      if start > stop then begin
        s     := start ;
        start := stop  ;
        stop  := s     ;
      end ;
      for s := start to stop do begin
        corFullRGB[s] := ( corFullRGB[s] and $FF00FF) or (Integer(colorW.G) shl 8) ;
        if i = 0 then
          isCorFullRGBgreen[s] := mask
        else
          isCorFullRGBgreen[s] := isCorFullRGBgreen[s] or mask
      end ;

      //Blue
      start := startW.B ;
      stop  := stopW.B ;
      if start > stop then begin
        s     := start ;
        start := stop  ;
        stop  := s     ;
      end ;
      for s := start to stop do begin
        corFullRGB[s] := ( corFullRGB[s] and $FFFF00) or (Integer(colorW.B))  ;
        if i = 0 then
          isCorFullRGBblue[s] := mask
        else
          isCorFullRGBblue[s] := isCorFullRGBblue[s] or mask ;
      end ;
      FreeObject( par ) ;
      mask := mask shl 1 ;
    end ;
  end ;
end ;

procedure TGIS_LayerPixel.prepFinalCorTable ;
var
  pixparams : TGIS_ParamsSectionPixel ;
begin
  pixparams := TGIS_ParamsSectionPixel(Params) ;

  // no correction - only color inversion if any
     prepCorTbl ;

  // gray
     if pixparams.Pixel.GrayScale or isGrayScaleImage then
       prepFullGrayTable ;

  // gray mapping
     if makeGrayMap and ( pixparams.Pixel.GrayScale or isGrayScaleImage ) then
         prepGrayMapTbl ;

     if pixparams.Pixel.Histogram then begin
       if not isBasicHistogram then
        makeHistogram ;
        histEqPav ;
     end;

  // color mapping
     if makeRGBMap then
      prepColorMapTbl ;

end ;

procedure TGIS_LayerPixel.fset_GridImage(
  const _value : Boolean
) ;
var
  pixparams : TGIS_ParamsSectionPixel ;
  aa : Boolean ;
begin
  pixparams := TGIS_ParamsSectionPixel(Params) ;
  if (FIsGridImage <> _value) or (pixparams.Pixel.GridBand <> FGridBand)
  then begin
    FIsGridImage := _value ;
    if FIsGridImage then begin
      if pixparams.Pixel.GridBand > FBandsCount then
        pixparams.Pixel.GridBand := 1 ;
      FGridBand :=  pixparams.Pixel.GridBand ;
    end else
      pixparams.Pixel.GridBand := 0 ;
      if not FIsNativeGridImage then begin
        rgbAsGrid := True ;
      end;
    if FIsGridImage then begin
      if  (pixparams.Pixel.GridBand = 0) and (not rgbAsGrid) then begin
        if not FIsNativeGridImage then
        pixparams.Pixel.GridBand := 1 ;
        FGridBand :=  pixparams.Pixel.GridBand ;
      end;
    end;
    aa := FAntialias ;
    setupParams ;
    FAntialias := aa ;
  end ;
  if FIsGridImage and (FMaxZ < FMinZ) then
    prepareMinMaxZ ;
end ;

procedure TGIS_LayerPixel.fset_CuttingPolygon(
  const _value : TGIS_ShapePolygon
) ;
begin
  if assigned(_value) then begin
    if _value is TGIS_ShapePolygon then begin
      if assigned(FCuttingPolygon) then
        FreeObject(FCuttingPolygon) ;
      FCuttingPolygon := _value ;
    end ;
  end
  else begin
    if assigned(FCuttingPolygon) then
      FreeObject(FCuttingPolygon) ;
  end ;
end ;

procedure TGIS_LayerPixel.fset_MinThresholdHeight(
  const _value : Single
) ;
begin
  if (_value >= FMinZ) and (_value <= FMaxZ) then begin
    FMinThresholdZ := _value ;
    TGIS_ParamsSectionPixel(Params).Pixel.MinHeightThreshold := _value ;
  end;
end ;

procedure TGIS_LayerPixel.fset_MaxThresholdHeight(
  const _value : Single
) ;
begin
  if (_value >= FMinZ) and (_value <= FMaxZ) then begin
    FMaxThresholdZ := _value ;
    TGIS_ParamsSectionPixel(Params).Pixel.MaxHeightThreshold := _value ;
  end;
end ;

procedure TGIS_LayerPixel.fullCorrection(
  const _pix    : TGIS_Pixels ;
  const _width  : Integer ;
  const _height : Integer
) ;
var
  i       : Integer ;
  size    : Integer ;
  r, g, b : Integer ;
  a       : Integer ;
begin
  size := _width*_height ;

  for i := 0 to size - 1 do begin
    a := _pix[i] and Integer($FF000000) ;
    if a = 0 then
      continue ;

    r := ((corRGB[((_pix[i] shr 16) and $000000FF)]) shr 16 ) and $000000FF ;
    g := ((corRGB[((_pix[i] shr 08) and $000000FF)])  shr 8 ) and $000000FF ;
    b := ( corRGB[(_pix[i]          and $000000FF)])          and $000000FF ;

    _pix[i] := Integer( a or
                       (r shl 16) or
                       (g shl 08) or b
                      ) ;
  end ;
end ;

procedure TGIS_LayerPixel.addTransparency(
  const _pix    : TGIS_Pixels ;
  const _width  : Integer ;
  const _height : Integer
) ;
var
  i       : Integer ;
  size    : Integer ;
  r, g, b : Integer ;
begin
  size := _width*_height ;

  for i := 0 to size - 1 do begin
    r := (_pix[i] shr 16) and $000000FF ;
    g := (_pix[i] shr 08) and $000000FF ;
    b :=  _pix[i]         and $000000FF ;
    if (redTransp[r] and greenTransp[g] and blueTransp[b]) <> 0 then
      _pix[i] := _pix[i] and $00FFFFFF ;
  end ;
end ;

procedure TGIS_LayerPixel.finalARGBMap(
  const _pix    : TGIS_Pixels ;
  const _width  : Integer ;
  const _height : Integer
) ;
var
  i       : Integer ;
  size    : Integer ;
  argb    : Array [0..3] of Integer ;
  r, g, b, a : Integer ;
begin
  size := _width*_height ;
  if bandsMap[0] <= 3 then begin
    r := bandsMap[0] ;
    if r < 0 then
      r := 0 ;
  end
  else
    r := 0 ;

  if bandsMap[1] <= 3 then begin
    g := bandsMap[1] ;
    if g < 0 then
      g := 1 ;
  end
  else
    g := 1 ;

  if bandsMap[2] <= 3 then begin
    b := bandsMap[2] ;
    if b < 0 then
      b := 2 ;
  end
  else
    b := 2 ;

  if bandsMap[3] <= 3 then begin
    a := bandsMap[3] ;
    if a < 0 then
      a := 3 ;
  end
  else
    a := 3 ;

  for i := 0 to size - 1 do begin
    if unusedRed then
      argb[0] := 0
    else
      argb[0] := (_pix[i] shr 16) and $FF ;

    if unusedGreen then
      argb[1] := 0
    else
      argb[1] := (_pix[i] shr  8) and $FF ;

    if unusedBlue then
      argb[2] := 0
    else
      argb[2] := (_pix[i]       ) and $FF ;

    if unusedAlpha then
      argb[3] := $FF
    else
      argb[3] := (_pix[i] shr  24) and $FF ;
    _pix[i] := (argb[a] shl 24)
            or (argb[r] shl 16)
            or (argb[g] shl  8)
            or  argb[b]  ;
  end;
end ;



procedure TGIS_LayerPixel.fullRGBCorrection(
  const _pix    : TGIS_Pixels ;
  const _width  : Integer ;
  const _height : Integer
) ;
var
  i       : Integer ;
  size    : Integer ;
  r, g, b : Integer ;
  ri, gi, bi : Integer ;
  a       : Integer ;
begin
  size := _width*_height ;

  for i := 0 to size - 1 do begin
    ri := (_pix[i] and $00FF0000) shr 16 ;
    gi := (_pix[i] and $0000FF00) shr 08 ;
    bi := (_pix[i] and $000000FF) ;
    if (isCorFullRGBred[ri] and isCorFullRGBgreen[gi] and  isCorFullRGBblue[bi]) <> 0 then
    begin
      r := (corFullRGB[ri] and $00FF0000) ; ;
      g := (corFullRGB[gi] and $0000FF00) ;
      b := (corFullRGB[bi] and $000000FF) ;
      a := Integer(_pix[i] and $FF000000) ;

     _pix[i] := a or r or g or b ;
    end ;
  end ;
end ;

procedure TGIS_LayerPixel.simpleRGBCorrection(
  const _pix    : TGIS_Pixels ;
  const _width  : Integer ;
  const _height : Integer
) ;
var
  i       : Integer ;
  size    : Integer ;
  r, g, b : Integer ;
  a       : Integer ;
begin
  size := _width*_height ;

  for i := 0 to size - 1 do begin
    r := ((_pix[i] and $00FF0000) shr 16) ;
    g := ((_pix[i] and $0000FF00) shr 08) ;
    b := ((_pix[i] and $000000FF)) ;
    a := Integer(_pix[i] and  $FF000000) ;

    _pix[i] := a or  (mapRGB[r] and $00FF0000) or
                     (mapRGB[g] and $0000FF00) or
                     (mapRGB[b] and $000000FF) ;
  end ;
end ;



procedure TGIS_LayerPixel.enhanceContrast(
  const _pix    : TGIS_Pixels ;
  const _width  : Integer ;
  const _height : Integer
) ;
var
  i       : Integer ;
  size    : Integer ;
  r, g, b : Integer ;
  br, bg, bb : Integer ;
  tr, tg, tb : Integer ;
  start, pixval : Integer ;
  deltar, deltag, deltab : Integer ;
begin
  tr := 0 ;
  tg := 0 ;
  tb := 0 ;
  br := 0 ;
  bg := 0 ;
  bb := 0 ;
  size := _width*_height ;
  start := -1 ;
  for i := 1 to size - 1 do begin
    if (_pix[i] and Integer($FF000000)) = 0 then
      continue ;

    tr := Integer((_pix[i] and $00FF0000) shr 16) ;
    br := tr ;
    tg := Integer((_pix[i] and $0000FF00) shr 08) ;
    bg := tg ;
    tb := Integer((_pix[i] and $000000FF)) ;
    bb := tb ;
    start := i +1 ;
    break ;
  end ;


  if start > 0 then begin
    for i := start to size - 1 do begin
      r := Integer((_pix[i] and $00FF0000) shr 16) ;
      g := Integer((_pix[i] and $0000FF00) shr 08) ;
      b := Integer((_pix[i] and $000000FF)) ;
      if r > tr then
        tr := r
      else
      if r < br then
        br := r ;

      if g > tg then
        tg := g
      else
      if g < bg then
        bg := g ;

      if b > tb then
        tb := b
      else
      if b < bb then
        bb := b ;
    end ;
  end ;
  deltar := tr -br ;
  deltag := tg -bg ;
  deltab := tb -bb ;

  for i := 0 to size -1 do begin
    pixval := _pix[i] ;
    if (pixval and Integer($FF000000)) = 0 then
      continue ;
    r := (pixval and $00FF0000 ) shr 16 ;
    g := (pixval and $0000FF00 ) shr 08 ;
    b := (pixval and $000000FF )  ;
    if deltar <> 0 then
      r := ((r -br)*255) div deltar;
    if deltag <> 0 then
     g := ((g -bg)*255) div deltag;
    if deltab <> 0 then
      b := ((b -bb)*255) div deltab;
    pixval := Integer(Integer(Cardinal(pixval) and $FF000000) or (r shl 16) or (g shl 8) or b) ;
   _pix[i] := pixval ;
  end ;

end ;

procedure TGIS_LayerPixel.gridToARGBTable(
  const _extent : TGIS_Extent ;
  const _argb  : TGIS_Pixels ;
  const _grid   : TGIS_GridArray ;
  const _width  : Integer       ;
  const _height : Integer
) ;
var
  i, k   : Integer ;
  idx    : Integer ;
  ccolor : TGIS_Color {$IFDEF GIS_NORECORDS} = new TGIS_Color {$ENDIF};
  sval   : Single ;
  sc_x   : Double ;
  lcs    : TGIS_CSCoordinateSystem ;
begin
  idx := 0 ;
  if assigned(Viewer) then
    lcs := Viewer.Ref.CS
  else
    lcs := CS ;

  if isShadow then begin
    if (
         ( lcs is TGIS_CSGeographicCoordinateSystem )
         or
         (
           ( ( FProjectedExtent.XMax - FProjectedExtent.XMin ) <= 360 )
           and
           ( Abs( FProjectedExtent.XMax ) <= 360 )
           and
           ( Abs( FProjectedExtent.XMin ) <= 360 )
           and
           ( lcs is TGIS_CSUnknownCoordinateSystem )
         )
       )
    then begin
      // probably lat/lon
      sc_x := ( FProjectedExtent.XMax - FProjectedExtent.XMin ) / BitWidth ;
      sizeShadow :=
         sc_x * 40000000 / 360 *
         ( ( ( _extent.XMax - _extent.XMin ) / _width )
           /
           ( ( FProjectedExtent.XMax - FProjectedExtent.XMin ) / BitWidth )
       ) ;
    end
    else
      sizeShadow := ( _extent.XMax - _extent.XMin ) / _width ;
  end ;

  for i := 0 to _height -1 do begin
     prevShadowValue := NaN ;
     prevShadowDelta := NaN ;
    for k := 0 to _width -1 do begin
      sval := _grid[i][k] ;
      if sval <> NoDataValue then begin
        ccolor := GetColorRamp(sval) ;
        _argb[idx] :=  Integer(ccolor.ARGB) ;
      end;
      inc(idx) ;
    end ;
  end ;
end ;

procedure TGIS_LayerPixel.replaceWorkingFiles(const _tmpname : String) ;
var
 sext    : String ;
 oext    : String ;
 tmpname : String ;
 ssname  : String ;
begin
  try
    Dormant ;
    RevertAll ;
    FIsOpened   := False ;
    FreeObject(fileStream) ;
    ssname := GetBackupName( Path ) ;
    DeleteFile(ssname) ;
    RenameFile( Path  , ssname) ;
    tmpname := _tmpname ;
    RenameFile( tmpname  , Path ) ;
    DeleteFile(tmpname) ;
    ssname := GetFilePath(tmpname) +GetFileNameNoExt(tmpname) ;
    oext := GetFileExt(tmpname) ;
    if length(oext) > 3 then begin
      sext := '.'+ oext[StringFirst+1]+oext[StringFirst+3] +'w' ;
    end ;
    DeleteFile(ssname +sext) ;
    DeleteFile(ssname +'.kml') ;
    DeleteFile(ssname +'.tab') ;
    DeleteFile(ssname +'.png.prj') ;
  finally
    ReOpen ;
  end;
end;

function  TGIS_LayerPixel.fget_AltitudeMapZonesCount : Integer ;
begin
  Result := Params.Pixel.AltitudeMapZones.Count ;
end;

function TGIS_LayerPixel.fget_Page : Integer ;
begin
  Result := FCurrentPage ;
end ;

function TGIS_LayerPixel.fget_MinHeight : Single ;
begin
  Result := FMinZ ;
end ;

procedure TGIS_LayerPixel.fset_MinHeight( const _value : Single
                                        ) ;
begin
  if FMinZ <> _value  then begin
    FMinZ := _value ;
    modifiedMinHeight := True ;
  end;
end;

procedure TGIS_LayerPixel.fset_RotationAngle( const _value : Double
                                        ) ;
begin
  if FRotationAngle <> _value then begin
    FRotationAngle := _value ;
    if FRotationAngle <> 0  then begin
      internalRotation := True ;
      rotationPoint.X := FExtent.XMin +(FExtent.XMax -FExtent.XMin)/2 ;
      rotationPoint.Y := FExtent.YMin +(FExtent.YMax -FExtent.YMin)/2 ;
      SinCos( -FRotationAngle, rotateSin, rotateCos     ) ;
      SinCos(  FRotationAngle, unrotateSin, unrotateCos ) ;
    end
    else
      internalRotation := False ;
    FProjectedExtent := FExtent ;
  end;
end;

function TGIS_LayerPixel.fget_MaxHeight : Single ;
begin
  Result := FMaxZ ;
end ;

procedure TGIS_LayerPixel.fset_MaxHeight( const _value : Single
                                        ) ;
begin
  if FMaxZ <> _value  then begin
    FMaxZ := _value ;
    modifiedMaxHeight := True ;
  end;
end;

procedure TGIS_LayerPixel.fset_Antialias(
  const _value : Boolean
) ;
begin
  FAntialias := _value ;
end ;

function TGIS_LayerPixel.fget_Capabilities : TGIS_LayerPixelSubFormatList ;
begin
  Result := FCapabilities ;
end ;

function TGIS_LayerPixel.fget_DefaultSubFormat : TGIS_LayerPixelSubFormat ;
begin
  if Capabilities.Count > 0 then
    Result := Capabilities[0]
  else
    Result := TGIS_LayerPixelSubFormat.DefaultSubFormat ;
end ;

function TGIS_LayerPixel.fget_GridOperation: TGIS_GridOperation;
begin
  Result := FGridOperation ;
end ;

procedure TGIS_LayerPixel.fset_GridOperation(
  const _value : TGIS_GridOperation
) ;
begin
  FGridOperation := _value ;
end ;

procedure TGIS_LayerPixel.fset_Interpretation (
  const _value : TGIS_LayerPixelInterpretation
) ;
begin
  if FInterpretation = _value then exit ;

  FInterpretation := _value ;
  case FInterpretation of
    TGIS_LayerPixelInterpretation.Grid :
      begin
        if not FIsNativeGridImage then begin
          rgbAsGrid := True ;
          FGridBand := 0 ;
        end;
        FIsGridImage := True ;
      end;
    TGIS_LayerPixelInterpretation.Pixel :
      begin
        rgbAsGrid := False ;
        FGridBand := 0 ;
        FIsGridImage := False ;
      end
    else begin
      FInterpretation := TGIS_LayerPixelInterpretation.Default ;
      rgbAsGrid := False ;
      FIsGridImage := FIsNativeGridImage ;
      FGridBand := 0 ;
      Params.Pixel.GridBand := FGridBand ;
    end;
  end;
end ;


function TGIS_LayerPixel.fget_PixelOperation: TGIS_PixelOperation;
begin
  Result := FPixelOperation ;
end ;

procedure TGIS_LayerPixel.fset_PixelOperation(
  const _value : TGIS_PixelOperation
) ;
begin
  FPixelOperation := _value ;
end ;

function TGIS_LayerPixel.fget_ParamsPixel : TGIS_ParamsSectionPixel ;
begin
  Result := TGIS_ParamsSectionPixel( inherited Params ) ;
end ;

function  TGIS_LayerPixel.fget_Extent : TGIS_Extent ;
begin
  if (not baseRotation) and (not internalRotation) then
    Result := FExtent
  else
    Result := baseRotatedExtent(FExtent) ;
end;


procedure TGIS_LayerPixel.fset_ParamsPixel(
  const _value : TGIS_ParamsSectionPixel
) ;
begin
  inherited Params := _value ;
end ;

procedure TGIS_LayerPixel.setUpInternal ;
begin
  colorsNo := 256 ;

  workSrcRect := Rect( 0, 0,
                       FCellWidth  - 1,
                       FCellHeight - 1
                     ) ;
end ;

procedure TGIS_LayerPixel.setUp  ;
var
  ext      : TGIS_Extent ;
  worldext : String ;
  fext     : String ;
begin
  inherited setUp ;

  if (baseCellWidth = 0) or (baseCellHeight = 0) then begin
    FCellWidth  := FBitWidth ;
    FCellHeight := FBitHeight ;

    baseCellWidth  := FBitWidth ;
    baseCellHeight := FBitHeight ;
  end
  else
  if (FCellWidth = 0) or (FCellHeight = 0) then begin
    FCellWidth  := FBitWidth ;
    FCellHeight := FBitHeight ;
  end ;

  bandsMap[0] := 0 ;
  bandsMap[1] := 1 ;
  bandsMap[2] := 2 ;
  bandsMap[3] := 3 ;

  setUpInternal ;

  FProgressive := True ;

  if FPagesCount = 0 then begin
    FPagesCount  := 1 ;
    FCurrentPage := 1 ;
  end ;

  if FBandsCount = 0 then begin
    if IsGrid then
      FBandsCount := 1
    else
      FBandsCount := 3 ;
  end ;

  fext := GetFileExt(FPath) ;
  if length(fext) > 3 then begin
    worldext := '.'+ fext[StringFirst+1]+fext[StringFirst+3] +'W' ;
    setWorldFile(worldext) ;
  end ;
  if scaleX = 0 then begin
    scaleX :=  1 ;
    scaleY := -1 ;
  end ;

  if FExtent.XMin >= FExtent.XMax then begin
    ext := _TGIS_Extent(FExtent) ;
    ext.XMin := ext.XMin - 0.5 * scaleX ;
    ext.YMax := ext.YMax - 0.5 * scaleY ;
    ext.XMax := ext.XMin + ( FBitWidth  ) * scaleX ;
    ext.YMin := ext.YMax + ( FBitHeight ) * scaleY ;
    Extent   := ext ;
  end ;

  if ( FBitWidth  <> 0 ) and ( FBitHeight <> 0 ) then
    FPixelSize := GisPoint(
                            ( FExtent.XMax - FExtent.XMin ) / FBitWidth,
                            ( FExtent.YMax - FExtent.YMin ) / FBitHeight
                          ) ;

  if FIsGridImage then begin
    fset_GridImage(True) ;
    if IsStringEmpty( FFileInfo ) then
      FFileInfo := Format( 'Generic Pixel-Grid Layer' +#13#10 +
                           '%d x %d',
                           [FBitWidth, FBitHeight]
                         ) ;
  end
  else begin
    if IsStringEmpty( FFileInfo ) then
      FFileInfo := Format( 'Generic Pixel Layer' +#13#10 +
                           '%d x %d',
                           [FBitWidth, FBitHeight]
                         ) ;
  end ;

end ;

procedure TGIS_LayerPixel.setUp3 ;
begin
  if IsGridImage then
    fset_View3DMode( TGIS_3DLayerType.Dem ) ;
end ;

procedure TGIS_LayerPixel.applyConfigOptions(
  const _cfg : TGIS_ConfigAbstract
) ;
begin
  inherited ;

  with TGIS_Config( _cfg ) do begin
    FMinZ := ReadFloat( GIS_INI_MINZ, FMinZ ) ;
    FMaxZ := ReadFloat( GIS_INI_MAXZ, FMaxZ ) ;
    Interpretation := ReadInterpretation( GIS_INI_INTERPRETATION, FInterpretation ) ;
  end ;
end ;

procedure TGIS_LayerPixel.storeConfigOptions(
  const _cfg : TGIS_ConfigAbstract
) ;
begin
  inherited ;

  with TGIS_Config( _cfg ) do begin
    WriteFloat(
      GIS_INI_MINZ, FMinZ ,  0
    ) ;
    WriteFloat (
      GIS_INI_MAXZ, FMaxZ ,  0
    ) ;
    WriteInterpretation(
      GIS_INI_INTERPRETATION, FInterpretation, TGIS_LayerPixelInterpretation.Default
    ) ;
  end ;
end ;

procedure TGIS_LayerPixel.setWorldFile(
  const _ext : String
) ;
var
  f        : TGIS_BufferedFileStream ;
  fwrdname : String   ;
  ftab     : String   ;
  ext      : TGIS_Extent {$IFDEF GIS_NORECORDS} := new TGIS_Extent {$ENDIF};
  yrot     : Double ;
  xrot     : Double ;
  rot90    : Boolean ;
  bexists  : Boolean ;

  aw, bw, cw  : Double ;
  dw, ew, fw : Double ;

  {$IFNDEF MONO OR ISLAND OR BLAZOR}
    tab    : TGIS_FileTAB ;
  {$ENDIF}

  function read_param : Double ;
  var
    sval : String ;
  begin
    sval := Trim( f.ReadLine) ;
    if sval[StringFirst] = '.' then
      sval := '0' + sval ;

    Result := DotStrToFloat( sval ) ;
  end ;

begin
  ext := GisNoWorld ;

  fwrdname := GetPathNoExt( Path ) + UpperCase( _ext ) ;

  if IsStringEmpty( PathTAB ) then
    ftab  := GetPathNoExt( Path ) + '.tab'
  else
    ftab  := PathTAB ;

  bexists := SafeFileExists( fwrdname ) ;
  if not bexists then begin
    fwrdname := GetPathNoExt( Path ) + LowerCase( _ext ) ;
    bexists := SafeFileExists( fwrdname ) ;
  end ;

  if not bexists then begin
    fwrdname := GetPathNoExt( Path ) + LowerCase( _ext ) +'x';
    bexists := SafeFileExists( fwrdname ) ;
  end ;

  if bexists then begin
    try
      f := TGIS_BufferedFileStream.Create( fwrdname, TGIS_StreamMode.Read ) ;
      try
        aw := read_param ;
        dw := read_param ;
        bw := read_param ;
        ew := read_param ;
        cw := read_param ;
        fw := read_param ;
        rot90 := False ;
        yrot  := dw ;
        xrot  := bw ;
        scaleX := aw ;
        if aw < 0 then begin
          scaleXFactor := -1 ;
          scaleX := -scaleX ;
          baseRotation := True ;
        end
        else
        if aw = 0 then begin
          rot90 := True ;
          scaleXFactor := 0 ;
          scaleX := dw ;
          if scaleX < 0 then begin
            scaleX := -scaleX ;
          end ;
          baseRotation := True ;
        end ;
        scaleY := ew ;
        if ew > 0 then begin
          scaleYFactor := -1 ;
          scaleY:= -scaleY;
          baseRotation := True ;
        end
        else
        if ew = 0 then begin
          scaleY := bw ;
          if scaleY > 0 then begin
            scaleY := -scaleY ;
          end ;
          scaleYFactor := 0 ;
          baseRotation := True ;
        end ;

        ext.XMin := cw ;
        ext.XMax := cw ;

        ext.YMin := fw ;
        ext.YMax := fw ;
        if (xrot <> 0) or (yrot <> 0) or baseRotation then begin
          if scaleX <> 0 then
            yRotDivSc := yrot/scaleX ;
          if scaleY <> 0 then
            xRotDivSc := xrot/scaleY ;
          scxF_yRotDivScxRotDivSMcsyF := scaleXFactor - yRotDivSc*xRotDivSc*scaleYFactor ;
          scyF_yRotDivScxRotDivSMcsxF := scaleYFactor - yRotDivSc*xRotDivSc*scaleXFactor ;
          if (scxF_yRotDivScxRotDivSMcsyF <> 0) and (scyF_yRotDivScxRotDivSMcsxF <> 0)then
            baseRotation := True
          else begin
            if rot90 then begin
                if dw > 0 then begin
                  xRotDivSc := 1 ; //right
                  yRotDivSc := 1 ;
                end
                else begin
                  xRotDivSc := -1 ; //left
                  yRotDivSc := -1 ;
                end;
                if bw*dw > 0 then
                 xRotDivSc := -xRotDivSc ;
            end
            else
              baseRotation := False ;
          end ;
        end ;

      finally
        FreeObject( f ) ;
      end ;
    except
      scaleX := 0 ;
      scaleY := 0 ;
    end ;
    FExtent := _TGIS_Extent(ext) ;
  end
  else if SafeFileExists( ftab ) then begin
    {$IFDEF MONO OR ISLAND}
      {$WARNING '### Verify MONO code'}
    {$ELSE}
      {$IFNDEF BLAZOR}
        tab := TGIS_FileTAB.Create( ftab, False ) ;
        try
          if tab.Prerecognize = 3 then
            if CompareText( tab.FilePath, ExtractFileName( Path ) ) = 0 then
              tab.ReadRaster( Self ) ;
        finally
          FreeObject( tab ) ;
        end ;
      {$ENDIF}
    {$ENDIF}
  end
  else begin
    if (FExtent.YMin = 1) and (FExtent.YMax = -1) then begin
      FExtent.XMin :=   0.5 ;
      FExtent.YMin :=   0.5 ;
      FExtent.XMax :=  -0.5 ;
      FExtent.YMax :=  -0.5 ;
    end ;
  end ;

end ;

function  TGIS_LayerPixel.transformNeed  : Boolean ;
var
  forview : Boolean ;
begin
  if assigned(outCS) and assigned(CS) then begin
    if outCS.EPSG = CS.EPSG then
      Result := False
    else
      Result := True ;
    if not forViewer then
      exit ;
  end;

  if assigned(Viewer) then
    forview := (((CS.EPSG > 0) and (Viewer.Ref.CS.EPSG > 0) and
               (CS.EPSG <> Viewer.Ref.CS.EPSG)))
               or (Viewer.Ref.RotationAngle <> 0)
  else
    forview := False ;

  Result := forview or baseRotation or assigned(CuttingPolygon) or
                  activeTransform or internalRotation;
end ;

function  TGIS_LayerPixel.findProperPixelLock
                              ( const _ext : TGIS_Extent ;
                                const _cs  : TGIS_CSCoordinateSystem ;
                                const _pixelsize : Double ;
                                const _band : Integer ;
                                const _writable : Boolean
                               ) : Integer ;
var
  i : Integer ;
  l : TGIS_LayerPixel ;
  pixs : Double ;
  dif : Double ;
  lplist : TObjectList< TGIS_LayerPixel >;
begin
  Result := -1 ;
  if _writable then
    lplist := oLockListW
  else
    lplist := oLockListR ;

  if not assigned(lplist) then
    exit ;

  for i := lplist.Count -1 downto 0 do begin
    l := lplist.Items[i] ;
    if l.fromBand <> _band  then
      continue ;

    if _cs = l.CS then
      if _cs.EPSG <> l.CS.EPSG then
        continue ;
    if fromBand <> l.fromBand then
      continue ;
    if _pixelsize <> 0 then begin
      pixs := (l.ProjectedExtent.XMax -l.ProjectedExtent.XMin)/l.FBitWidth ;
      dif := pixs/(l.BitWidth) ;
      if Abs(pixs - _pixelsize) < dif then begin
        if (l.ProjectedExtent.XMax >= _ext.XMax) and
           (l.ProjectedExtent.YMax >= _ext.YMax) and
           (l.ProjectedExtent.XMin <= _ext.XMin) and
           (l.ProjectedExtent.YMin <= _ext.YMin) then
        begin
          Result := i ;
          exit ;
        end ;
      end ;
    end ;
  end ;
end ;


function  TGIS_LayerPixel.projectionNeed : Boolean ;
begin
  if assigned(outCS) then begin
    if outCS.EPSG = CS.EPSG then
      Result := False
    else
      Result := True ;
    if not forViewer then
      exit ;
  end;
  if assigned(Viewer) then
    Result := (((CS.EPSG > 0) and (Viewer.Ref.CS.EPSG > 0) and
               (CS.EPSG <> Viewer.Ref.CS.EPSG)))
  else
    Result := False ;
end ;

function  TGIS_LayerPixel.rotationNeed   : Boolean ;
begin
  if assigned(Viewer) then
    Result := Viewer.Ref.RotationAngle <> 0
  else
    Result := False ;
end ;

function doCompareAltitude( const _left, _right : TGIS_AltitudeZone ) : Integer ;
begin
  if GisIsSameValue(_left.MinVal, _right.MinVal, 1E-15) and
     GisIsSameValue(_left.MaxVal, _right.MaxVal, 1E-15) then
    Result := 0
  else if _left.MaxVal <= _right.MinVal  then
    Result := -1
  else if _left.MaxVal > _right.MinVal then begin // range overlapp
    if _left.MinVal < _right.MinVal then // check min val
      Result := -1
    else
      Result := 1
  end
  else
    Result := 0
end ;

procedure TGIS_LayerPixel.prepareColorRamp ;
begin
  assignedColorRamp := Params.Pixel.ColorRamp ;
  if assigned( Params.Pixel.ColorRamp ) then
    useColorRamp := True
  else
    useColorRamp := False ;
end;

procedure TGIS_LayerPixel.prepareAltitudeMapTable ;
var
  i           : Integer        ;
  tkn         : TGIS_Tokenizer ;
  pixparams   : TGIS_ParamsSectionPixel ;
  must_sort   : Boolean ;
  old_min     : Single ;
  zones_count : Integer ;
begin
  pixparams := TGIS_ParamsSectionPixel(Params) ;
  lastAltitudeMapZoneSerial := pixparams.Pixel.Serial ;

  zones_count := pixparams.Pixel.AltitudeMapZones.Count ;
  if zones_count = 0 then begin
    useAltitudeZones := False ;
    SetLength( altitudeZones, 0 ) ;
    exit ;
  end ;

  useAltitudeZones := True ;
  SetLength( altitudeZones, zones_count ) ;

  must_sort := False ;
  old_min   := -GIS_MAX_SINGLE ;

  tkn := TGIS_Tokenizer.Create ;
  try
    for i := 0 to zones_count -1 do begin
      {$IFDEF GIS_NORECORDS}
        altitudeZones[i] := new TGIS_AltitudeZone ;
      {$ENDIF}
      tkn.ExecuteEx( pixparams.Pixel.AltitudeMapZones.Strings[i], ',' ) ;

      altitudeZones[i].MinVal := -GIS_MAX_SINGLE ;
      altitudeZones[i].MaxVal :=  GIS_MAX_SINGLE ;
      altitudeZones[i].Color  := TGIS_Color.Black ;
      altitudeZones[i].Legend := '' ;

      try
        if tkn.Result.Count > 0 then
          altitudeZones[i].MinVal := DotStrToFloat( tkn.Result[0] ) ;
        if tkn.Result.Count > 1 then
          altitudeZones[i].MaxVal := DotStrToFloat( tkn.Result[1])  ;
        if tkn.Result.Count > 2 then
          altitudeZones[i].Color  := ParamColor( tkn.Result[2],  TGIS_Color.Black  ) ;
        if tkn.Result.Count > 3 then
          altitudeZones[i].Legend := tkn.Result[3] ;

        if ( i > 0 ) and not must_sort then
          if old_min > altitudeZones[i].MinVal then
            must_sort := True ;

        old_min := altitudeZones[i].MinVal ;
      except
      end ;
    end ;
  finally
    FreeObject( tkn ) ;
  end ;

  if must_sort then
    // ensure the list is sorted
    {$IFDEF DCC}
      TArray.Sort<TGIS_AltitudeZone>(
        altitudeZones,
        TDelegatedComparer<TGIS_AltitudeZone>.Construct(doCompareAltitude )
      ) ;
    {$ELSE}
      {$IFDEF JAVA}
        java.util.Arrays.sort(
          altitudeZones,
          new T_CompAltitudeZone
        ) ;
      {$ELSE}
        TArray<TGIS_AltitudeZone>.Sort( altitudeZones, @doCompareAltitude ) ;
      {$ENDIF}
    {$ENDIF}
end ;

procedure TGIS_LayerPixel.setBitmapPalette ;
var
  i, ival : Integer  ;
  bval    : Integer  ;
begin
  bval := 0;
  if colorsNo > 0 then begin
    ival := 255 div ( colorsNo -1 ) ;
    for i := 0 to colorsNo -1 do begin

      bitPalette[i] := TGIS_Color.FromRGB(bval, bval, bval) ;

      bval := bval + Byte(ival) ;
    end ;
  end ;
end ;

procedure TGIS_LayerPixel.prepRGBHist(
            const _buffer   : TGIS_Pixels ;
            const _width    : Integer ;
            const _height   : Integer ) ;
var
  i, r, g, b : Integer ;
begin

   if makeTransparent then
     addTransparency(_buffer, _width, _height) ;
  if makeRGBMap then
    simpleRGBCorrection(_buffer, _width, _height) ;
  if makeFullRGBMap then
    fullRGBCorrection(_buffer, _width, _height) ;
  if Params.Pixel.GrayScale or isGrayScaleImage then
    ARGB2Gray( _buffer, _width*_height) ;

  for i := 0 to 255 do begin
     hisBlue [ i ] := 0 ;
     hisGreen[ i ] := 0 ;
     hisRed  [ i ] := 0 ;
  end;

  for i := 0 to  _width*_height -1 do begin
    if(_buffer[i] and Integer($FF000000)) = 0 then
      continue ;
     r := (_buffer[i] shr 16) and $FF ;
     g := (_buffer[i] shr  8) and $FF ;
     b :=  _buffer[i]         and $FF ;
     inc( hisBlue [ b ] ) ;
     inc( hisGreen[ g ] ) ;
     inc( hisRed  [ r ] ) ;
   end ;
   histRGB2Gray ;
end ;

procedure TGIS_LayerPixel.makeHistogram ;
var
  pixels : TGIS_Pixels ;
  w, h   : Integer ;
const
  MAX_LINES_TO_HISTOGRAM = 200 ;
begin
  Alive ;

  h := MAX_LINES_TO_HISTOGRAM ;

  if h >= baseCellHeight then begin
    h := baseCellHeight ;
    w := baseCellWidth ;
  end
  else
    w := FloorS(Int64(baseCellWidth)*h/baseCellHeight) ;

  SetLength( pixels, w*h ) ;
  if assigned( pixels ) then begin
    getBitmapData(FExtent, pixels, w, h) ;
    prepRGBHist(pixels, w, h) ;
    SetLength(pixels, 0);
  end ;
  isBasicHistogram := True ;
end ;

procedure TGIS_LayerPixel.prepCorTbl ;
var
  i         : Integer ;
  ni        : Integer ;
  pixparams : TGIS_ParamsSectionPixel ;
begin
  pixparams := TGIS_ParamsSectionPixel(Params) ;

  if pixparams.Pixel.Inversion then
    for i := 0 to 255 do begin
      ni := 255 -i ;
      corRGB[i] := (ni shl 16) or (ni shl 8) or ni ;
    end
  else
    for i := 0 to 255 do begin
      corRGB[i] := (i shl 16) or (i shl 8) or i ;
    end ;
end ;

procedure TGIS_LayerPixel.histEqPav ;
var
  j, amp     : Integer ;
  hav, hint,
  oval, nval,
  lval, rval : Integer ;
begin

  amp := 0 ;

  for j := 0 to 255 do
    amp := amp + hisGray[j] ;

  hav := amp div 255;
  if hav = 0 then
    exit;
  nval := 0;
  hint := 0;

  for oval := 0 to 255 do begin
    lval := nval ;
    hint := hint + hisGray[oval] ;
    while hint > hav do begin
      hint := hint - hav ;
      nval := nval + 1 ;
    end ;
    rval := nval ;
    amp := (lval + rval) div 2 ;
    if amp > 255 then
      amp := 255 ;

    corRGB[oval]  := (amp shl 16) or (amp shl 8) or amp ;

  end ;
  if isInverted then begin
    for oval := 0 to 255 do
      corRGB[oval]  := $00FFFFFF -corRGB[oval];
  end ;
  isHistogram := True ;
end ;

procedure TGIS_LayerPixel.prepColorMapTbl ;
var
  i         : Integer ;
  mzone     : TGIS_MapZone ;
  pixparams : TGIS_ParamsSectionPixel ;

  procedure move2red ;
  begin
    mzone.Y.Start := (mzone.Y.Start and $FF) shl 16 ;
    mzone.Y.Stop  := (mzone.Y.Stop  and $FF) shl 16 ;
  end;
  procedure move2green ;
  begin
    mzone.Y.Start := (mzone.Y.Start and $FF) shl 8 ;
    mzone.Y.Stop  := (mzone.Y.Stop  and $FF) shl 8 ;
  end;


  procedure moveWords2Bytes ;
  var
    cmp : Cardinal ;
  begin
    cmp := ($FFFF shr (16 -bitsPerBand[0])) shr wordShift ;

    if (mzone.X.Start > Integer(cmp)) or (mzone.X.Stop > Integer(cmp)) then begin
      mzone.X.Start := (mzone.X.Start shr wordShift ) and $FF ;
      mzone.X.Stop  := (mzone.X.Stop  shr wordShift ) and $FF ;
    end;
  end;

begin
  pixparams := TGIS_ParamsSectionPixel(Params) ;
  {$IFDEF GIS_NORECORDS}mzone := new TGIS_MapZone;{$ENDIF}



  for i := 0 to 255 do begin
   mapRGB[i] :=  (i shl 16) or (i shl 8) or i ;
  end ;


  if pixparams.Pixel.RedMapZones.Count > 0 then begin
    for i := 0 to pixparams.Pixel.RedMapZones.Count - 1 do begin
      if getMapZoneVal( pixparams.Pixel.RedMapZones.Strings[i], mzone ) then begin
        if is48BitsPerPixel then begin
          moveWords2Bytes ;
          move2red ;
          setMapZone( mapRGB, TGIS_Color.Red, mzone ) ;
        end
        else begin
         if not memoryResident then begin
           move2red ;
           setMapZone( mapRGB, TGIS_Color.Red, mzone ) ;
         end ;
        end ;
      end ;
    end ;
  end ;

  if pixparams.Pixel.GreenMapZones.Count > 0 then begin
    for i := 0 to pixparams.Pixel.GreenMapZones.Count - 1 do begin
      if getMapZoneVal( pixparams.Pixel.GreenMapZones.Strings[i], mzone ) then begin
        if is48BitsPerPixel then begin
          moveWords2Bytes ;
          move2green ;
          setMapZone( mapRGB, TGIS_Color.Green, mzone ) ;
        end
        else begin
          if not memoryResident then begin
            move2green ;
            setMapZone( mapRGB, TGIS_Color.Green, mzone ) ;
          end ;
        end ;
      end ;
    end ;
  end ;

  if pixparams.Pixel.BlueMapZones.Count > 0 then begin
    for i := 0 to pixparams.Pixel.BlueMapZones.Count -1 do begin
      if getMapZoneVal( pixparams.Pixel.BlueMapZones.Strings[i], mzone ) then begin
        if is48BitsPerPixel then begin
          moveWords2Bytes ;
          setMapZone( mapRGB, TGIS_Color.Blue, mzone ) ;
        end
        else begin
          if not memoryResident then
            setMapZone( mapRGB, TGIS_Color.Blue, mzone ) ;
        end ;
      end ;
    end ;
  end ;

//  for i := 0 to 255 do  begin
//    corRGB[i] := TGIS_Color.FromRGB(mapRGB[corRGB[i].R].R,
//                                    mapRGB[corRGB[i].G].G,
//                                    mapRGB[corRGB[i].B].B ) ;
//  end ;

end ;

procedure TGIS_LayerPixel.applyColorsCorrectionByVals ;
var
  pixparams : TGIS_ParamsSectionPixel ;
  r, g, b : Integer ;

  function brightness_contrast(
     const _color    :  Integer ;
    const _bright   : Integer ;
    const _contrast : Integer
  ) : Byte ;
  var
    v : Integer ;
  begin
    v := _color + ( _bright * 255 ) div 100 ;

    if v < 128 then v := TruncS( v - Abs(128-v)* (1.0*_contrast)/100 )
               else v := TruncS( v + Abs(128-v)* (1.0*_contrast)/100 ) ;

    if      v < 0   then Result := 1
    else if v > 255 then Result := 255
    else                 Result := v

  end ;

  procedure  prepare_rgb(
    const _red      : Integer          ;
    const _green    : Integer          ;
    const _blue     : Integer          ;
    const _contrast : Integer
   ) ;
  var
    j       : Integer ;
  begin
    for j := 0 to 255 do begin
       r := brightness_contrast( (corRGB[j] shr 16) and $000000FF ,
                                 _red,
                                 _contrast
                                ) ;
       g := brightness_contrast( (corRGB[j] shr 08) and $000000FF ,
                                 _green,
                                _contrast
                                ) ;
       b := brightness_contrast( (corRGB[j] and $000000FF) ,
                                 _blue,
                                 _contrast
                                ) ;
      corRGB[j] := (Integer(corRGB[j] and $FF000000)) or (r shl 16)
                                             or (g shl 08)
                                             or  b  ;
    end ;
  end ;


begin
  pixparams := TGIS_ParamsSectionPixel(Params) ;

  prepare_rgb( pixparams.Pixel.Red   + pixparams.Pixel.Brightness,
               pixparams.Pixel.Green + pixparams.Pixel.Brightness,
               pixparams.Pixel.Blue  + pixparams.Pixel.Brightness,
               pixparams.Pixel.Contrast
             ) ;
end ;



procedure TGIS_LayerPixel.setMapZone(
  var   _maparray  : array of Integer ;
  const _color     : TGIS_Color ;
  const _zone      : TGIS_MapZone
  ) ;
var
  dx, p, k   : Integer ;
  yl, dy, cy : Integer ;
  lzone      : TGIS_MapZone ;
  ptmp       : Integer ;
begin
  {$IFDEF GIS_NORECORDS}lzone := new TGIS_MapZone;{$ENDIF}

  lzone.X.Start := _zone.X.Start and $FF ;
  lzone.X.Stop  := _zone.X.Stop  and $FF ;

  if _color =  TGIS_Color.Red then begin
    if lzone.X.Start > lzone.X.Stop then begin
      lzone.Y.Stop  := (_zone.Y.Start shr 16) and $FF ;
      lzone.Y.Start := (_zone.Y.Stop  shr 16) and $FF ;
    end
    else begin
      lzone.Y.Start := (_zone.Y.Start shr 16) and $FF ;
      lzone.Y.Stop  := (_zone.Y.Stop  shr 16) and $FF ;
    end;
  end
  else
  if _color = TGIS_Color.Green then begin
    if lzone.X.Start > lzone.X.Stop then begin
      lzone.Y.Stop  := (_zone.Y.Start shr 08) and $FF ;
      lzone.Y.Start := (_zone.Y.Stop  shr 08) and $FF ;
    end
    else begin
      lzone.Y.Start := (_zone.Y.Start shr 08) and $FF ;
      lzone.Y.Stop  := (_zone.Y.Stop  shr 08) and $FF ;
    end;
  end
  else
  if _color = TGIS_Color.Blue then begin
    if lzone.X.Start > lzone.X.Stop then begin
      lzone.Y.Stop := (_zone.Y.Start       ) and $FF ;
      lzone.Y.Start  := (_zone.Y.Stop      ) and $FF ;
    end
    else begin
      lzone.Y.Start := (_zone.Y.Start       ) and $FF ;
      lzone.Y.Stop  := (_zone.Y.Stop        ) and $FF ;
    end;
  end;



  if (lzone.X.Start > lzone.X.Stop) then begin
    k := lzone.X.Start ;
    lzone.X.Start := lzone.X.Stop ;
    lzone.X.Stop  := k ;
  end ;

  dx := lzone.X.Stop - lzone.X.Start ;
  dy := lzone.Y.Stop - lzone.Y.Start ;
  if dy > 0 then
    cy := 1
  else
  if dy < 0 then begin
    cy := -1 ;
    dy := -dy ;
  end
  else
    cy := 0 ;

  yl := lzone.Y.Start ;
  p  := 0 ;

  ptmp := lzone.X.Start ;

  for k := lzone.X.Start to lzone.X.Stop do begin
    if _color =  TGIS_Color.Red then
        _maparray[ptmp] := (yl shl 16) or (_maparray[ptmp] and $0000FFFF)
    else
    if _color = TGIS_Color.Green then
        _maparray[ptmp] := (yl shl 08) or (_maparray[ptmp] and $00FF00FF)
    else
    if _color = TGIS_Color.Blue then
        _maparray[ptmp]:= (yl        ) or (_maparray[ptmp] and $00FFFF00) ;

    inc( ptmp ) ;

    p := p + dy ;
    if dx > 0 then
      while p >= dx do begin
        p  := p -  dx ;
        yl := yl + cy ;
      end ;
  end ;
end ;

function TGIS_LayerPixel.getMapZoneVal(
  const _zonestr : String ;
  var   _zone    : TGIS_MapZone
) : Boolean ;
var
  ival, s : Integer ;
  iarr    : array of Integer ;

  function str2int_array( const _str  : String ) : Integer;
  var
    i, r, b : Integer ;
    tkn : TGIS_Tokenizer ;
  begin
    for i:= low( iarr ) to high( iarr ) do
      iarr[i] := 0 ;
    try
      tkn := TGIS_Tokenizer.Create ;
      tkn.ExecuteEx( _str, ',' ) ;

      Result := Min( tkn.Result.Count -1, Integer(high( iarr )) ) ;
      for i:=0 to Result do begin
        if is48BitsPerPixel then begin
          iarr[i] := StrToInt(tkn.Result[i]) ;
          Result := i+1 ;
          continue ;
        end
        else
          iarr[i] := Integer(ParamColor( tkn.Result[i], TGIS_Color.Black ).ARGB ) ;
        b := (iarr[i] shr 16) and $FF ;
        r := ((iarr[i] and $FF) shl 16) or b ;
        iarr[i] := (iarr[i] and Integer($FF00FF00)) or r ;
        Result := i+1 ;
      end ;
    finally
      FreeObject( tkn ) ;
    end ;
  end ;

begin
  Result := False ;
  s := 4 ;
  SetLength(iarr, s) ;

  ival   := str2int_array(_zonestr) ;
  if ival >= 2 then begin
    _zone.X.Start := iarr[0] ;
    _zone.X.Stop  := iarr[1] ;
    Result := True;
  end ;
  if ival = 3 then begin
    _zone.Y.Start := iarr[2] ;
    _zone.Y.Stop  := iarr[2] ;
  end
  else
  if ival = 4 then begin
    _zone.Y.Start := iarr[2] ;
    _zone.Y.Stop  := iarr[3] ;
  end ;
  SetLength(iarr, 0) ;
end ;

procedure TGIS_LayerPixel.prepFullGrayTable;
var
  i  : Integer ;
begin
  colorsNo := 256 ;

  for i := 0 to colorsNo -1 do begin
    paletteCpy[i] := TGIS_Color.FromRGB(TruncS(FACTOR_RED   *i),
                                        TruncS(FACTOR_GREEN *i),
                                        TruncS(FACTOR_BLUE  *i)) ;
  end ;
end ;

procedure TGIS_LayerPixel.prepGrayMapTbl ;
var
  i         : Integer ;
  mzone     : TGIS_MapZone ;
  nzone     : TGIS_MapZone ;
  pixparams : TGIS_ParamsSectionPixel ;

  function _getBValue(_c : Integer) : Integer ;
  begin
    Result := _c and $000000FF ;
  end ;

  function _getGValue(_c : Integer) : Integer ;
  begin
    Result := (_c shr 8) and $000000FF ;
  end ;

  function _getRValue(_c : Integer) : Integer ;
  begin
    Result := (_c shr 16) and $000000FF ;
  end ;

begin
  pixparams := TGIS_ParamsSectionPixel(Params) ;
 {$IFDEF GIS_NORECORDS}mzone := new TGIS_MapZone;{$ENDIF}
 {$IFDEF GIS_NORECORDS}nzone := new TGIS_MapZone;{$ENDIF}


  for i := 0 to 255 do begin
    mapGray2RGB[i]  := (i shl 16) or (i shl 8) or i ;
  end ;

  if pixparams.Pixel.GrayMapZones.Count > 0 then
    for i := 0 to pixparams.Pixel.GrayMapZones.Count -1 do
      if getMapZoneVal( pixparams.Pixel.GrayMapZones.Strings[i], mzone ) then
      begin
        nzone.X := mzone.X ;
        setMapZone( mapGray2RGB, TGIS_Color.Red,   mzone ) ;

        setMapZone( mapGray2RGB, TGIS_Color.Green, mzone ) ;

        setMapZone( mapGray2RGB, TGIS_Color.Blue,  mzone ) ;
      end ;
end ;

procedure TGIS_LayerPixel.histRGB2Gray ;
var
  i : Integer ;
begin
  for i := 0 to 255 do  begin
    hisGray[i] := RoundS( FACTOR_RED   * hisRed[i]   +
                          FACTOR_GREEN * hisGreen[i] +
                          FACTOR_BLUE  * hisBlue[i]
                        ) ;
  end ;
end ;

procedure TGIS_LayerPixel.prepTransparent ;
var
  i, s,
  start,
  stop      : Integer;
  tr_flag   : Int64 ;
  pixparams : TGIS_ParamsSectionPixel ;
  par       : T_ParseZones ;
  startW    : TGIS_Color {$IFDEF GIS_NORECORDS} = new TGIS_Color {$ENDIF};
  stopW     : TGIS_Color {$IFDEF GIS_NORECORDS} = new TGIS_Color {$ENDIF};
begin

  for i := 0 to 255 do begin
    redTransp[i]   := 0 ;
    greenTransp[i] := 0 ;
    blueTransp[i]  := 0 ;
  end ;

  pixparams := TGIS_ParamsSectionPixel(Params) ;

  tr_flag := BASE_TRANSPARENT_FLAG ;
  if internalTransparentColor.A = 0 then begin
    redTransp[internalTransparentColor.R] := tr_flag ;

    greenTransp[internalTransparentColor.G] := tr_flag ;
    blueTransp[internalTransparentColor.B] :=tr_flag ;
    tr_flag := tr_flag shl 1 ;
  end ;

  for i := 0 to pixparams.Pixel.TransparentZones.Count -1 do begin
    par := T_ParseZones.Create(pixparams.Pixel.TransparentZones.Strings[i]) ;
    if par.Count < 2 then begin
      FreeObject( par ) ;
      continue ;
    end ;

    startW := par.ReadRGB(0) ;
    stopW  := par.ReadRGB(1) ;

    start := Integer(startW.R) ;
    stop  := Integer(stopW.R) ;
    if start > stop then begin
      s     := start ;
      start := stop  ;
      stop  := s     ;
    end ;

    for s := start to stop do
      redTransp[s]   := redTransp[s] or tr_flag ;

    start := Integer(startW.G) ;
    stop  := Integer(stopW.G) ;
    if start > stop then begin
      s     := start ;
      start := stop  ;
      stop  := s     ;
    end ;
    for s := start to stop do
      greenTransp[s]   := greenTransp[s] or tr_flag ;

    start := Integer(startW.B) ;
    stop  := Integer(stopW.B) ;

    if start > stop then begin
      s     := start ;
      start := stop  ;
      stop  := s     ;
    end ;
    for s := start to stop do
      blueTransp[s]   := blueTransp[s] or tr_flag ;

    tr_flag := tr_flag shl 1 ;
    FreeObject( par ) ;
  end ;
end ;

procedure TGIS_LayerPixel.inversePalette ;
var
  lp  : Integer  ;
  a, r, g, b : Integer ;
begin
  for lp := 0 to colorsNo - 1 do begin
    // reversed order
    a := bitPalette[ lp ].A ;
    r := $000000FF -bitPalette[ lp ].R ;
    g := $000000FF -bitPalette[ lp ].G ;
    b := $000000FF -bitPalette[ lp ].B ;
    bitPalette[ lp ] := TGIS_Color.FromARGB(a, r, g, b) ;
  end ;
end ;

function  TGIS_LayerPixel.setFileScale(
  const _dwidth : Double ;
  const _swidth : Double
) : Double ;
begin
  Result := 1.0 ;
end ;

function  TGIS_LayerPixel.setFileScaleXY(
  const _dwidth  : Double ;
  const _swidth  : Double ;
  const _dheight : Double ;
  const _sheight : Double
) : Double ;
begin
  Result := 1.0 ;
  extZoomX := 1.0 ;
  extZoomY := 1.0 ;
end ;

procedure TGIS_LayerPixel.setPage(
  const _value : Integer
) ;
begin
  // just for safe inheritance
end ;

procedure TGIS_LayerPixel.setupParams ;
begin
  if Params.Pixel.AlphaBand  = -1 then begin
    isPartialTransparent := False ;
  end else
  begin
    if Params.Pixel.AlphaBand  = 0 then begin
      isPartialTransparent := defaultPartialTransparent ;
    end
    else
      isPartialTransparent :=  True ;
  end ;

  //Bands managing
  unusedAlpha := False  ;
  unusedRed   := False  ;
  unusedGreen := False  ;
  unusedBlue  := False  ;

  if Params.Pixel.GridBand > 0 then begin
    if FInterpretation <> TGIS_LayerPixelInterpretation.Pixel then
      if not assigned(oBitmap) then
        FIsGridImage         := True ;
  end
  else begin
    if FInterpretation <> TGIS_LayerPixelInterpretation.Pixel then begin
      FIsGridImage := FIsNativeGridImage or rgbAsGrid ;
    end
    else
    if FIsNativeGridImage then begin
      SetLength( altitudeZones, 0 ) ;
      ParamsList.ClearAndSetDefaults ;
      isShadow := False ;
      FAntialias := False;
    end;

  end;

  if FIsGridImage  then begin
    if ( Params.Pixel.GridBand > FBandsCount ) and (not assigned(oGrid)) and
       ( not rgbAsGrid) then
      Params.Pixel.GridBand  := 1 ;
    if FGridBand <> Params.Pixel.GridBand then begin
      FGridBand := Params.Pixel.GridBand ;
      FMinZ :=  GIS_MAX_SINGLE ;
      FMaxZ := -GIS_MAX_SINGLE ;
    end;
    isShadow := Params.Pixel.GridShadow ;
    FShadowAngle := Params.Pixel.GridShadowAngle ;
    gridSmoothColors := Params.Pixel.GridSmoothColors ;
  end ;
  bandsMap[0] := 0 ;
  bandsMap[1] := 1 ;
  bandsMap[2] := 2 ;
  bandsMap[3] := 3 ;

  if Params.Pixel.RedBand > 0 then
    bandsMap[0] := Params.Pixel.RedBand -1
  else
  if Params.Pixel.RedBand = 0 then begin
    bandsMap[0] := 0 ;
  end
  else begin
    bandsMap[0] := -1 ;
    unusedRed := True ;
  end ;

  if Params.Pixel.GreenBand > 0 then
    bandsMap[1] := Params.Pixel.GreenBand -1
  else
  if Params.Pixel.GreenBand = 0 then begin
    bandsMap[1] := 1 ;
  end
  else begin
    bandsMap[1] := -1 ;
    unusedGreen := True ;
  end ;

  if Params.Pixel.BlueBand > 0 then
    bandsMap[2] := Params.Pixel.BlueBand -1
  else
  if Params.Pixel.BlueBand = 0 then begin
    bandsMap[2] := 2 ;
  end
  else begin
    bandsMap[2] := -1 ;
    unusedBlue := True ;
  end ;

  if Params.Pixel.AlphaBand > 0 then
    bandsMap[3] := Params.Pixel.AlphaBand -1
  else
  if (Params.Pixel.AlphaBand = 0) then begin
    if (FBandsCount > 3) then
      bandsMap[3] := 3 ;
  end
  else
  if Params.Pixel.AlphaBand = -1 then begin
    if (FBandsCount > 3) then
      bandsMap[3] := -1 ;
    unusedAlpha := True ;
  end ;

  makeTransparent := (Params.Pixel.TransparentZones.Count > 0) or
                     (internalTransparentColor.A = 0) ;
  if makeTransparent then
    prepTransparent ;

  makeSomeCorrection := False;

  if not FIsGridImage then begin
    if Params.Pixel.GridBand <> 0 then begin
      if Params.Pixel.GridBand > FBandsCount then
        Params.Pixel.GridBand := 1 ;
      FGridBand := Params.Pixel.GridBand ;
      if FInterpretation <> TGIS_LayerPixelInterpretation.Pixel then
        if not assigned(oBitmap) then
          FIsGridImage         := True ;
      bandsMap[0] := FGridBand -1 ;
      bandsMap[1] := bandsMap[0] ;
      bandsMap[2] := bandsMap[0] ;
    end ;
  end ;



  FIsContrastEnhanced := Params.Pixel.ContrastEnhanced ;
  if FPagesCount <= 1 then begin
    FMinThresholdZ      := Params.Pixel.MinHeightThreshold ;
    FMaxThresholdZ      := Params.Pixel.MaxHeightThreshold ;
  end;

  if IsGridImage then begin
    if mmzgbn < 0 then
      mmzgbn := FGridBand ;

    if (FGridBand <> Params.Pixel.GridBand) or (FGridBand <> mmzgbn) or
       (FMinZ > FMaxZ)
    then begin
      FGridBand := Params.Pixel.GridBand ;
      prepareMinMaxZ ;
      FMinThresholdZ := -GIS_MAX_SINGLE ;
      FMaxThresholdZ :=  GIS_MAX_SINGLE ;
      Params.Pixel.MinHeightThreshold := FMinThresholdZ ;
      Params.Pixel.MaxHeightThreshold := FMaxThresholdZ ;
    end;
    prepareAltitudeMapTable ;
    prepareColorRamp ;
  end ;

  makeGrayMap     := Params.Pixel.GrayMapZones.Count     > 0 ;
  makeRedMap      := Params.Pixel.RedMapZones.Count      > 0 ;
  makeGreenMap    := Params.Pixel.GreenMapZones.Count    > 0 ;
  makeBlueMap     := Params.Pixel.BlueMapZones.Count     > 0 ;
  makeFullRGBMap  := Params.Pixel.FullRGBMapZones.Count  > 0 ;

  if makeFullRGBMap then
    prepFullRGBMapTbl ;

  makeRGBMap         := makeRedMap   or
                        makeGreenMap or
                        makeBlueMap  ;

  makeColorsByVals   := ( Params.Pixel.Red        <> 0 ) or
                        ( Params.Pixel.Blue       <> 0 ) or
                        ( Params.Pixel.Green      <> 0 ) or
                        ( Params.Pixel.Brightness <> 0 ) or
                        ( Params.Pixel.Contrast   <> 0 )
                        ;
  makeSomeCorrection := makeRGBMap       or
                        makeColorsByVals or
                        makeGrayMap      or
                        Params.Pixel.Inversion or
                        Params.Pixel.Histogram or
                        Params.Pixel.GrayScale ;

  if isInverted <> Params.Pixel.Inversion then begin
    isHistogram := False ;
    isInverted := Params.Pixel.Inversion ;
  end ;

  if makeSomeCorrection then
    prepFinalCorTable ;

  if makeColorsByVals then
    applyColorsCorrectionByVals ;

  if FIsGridImage then begin
    if FMinZ > FMaxZ then begin
      prepareMinMaxZ ;
    end
    else
    if FNoDataValue <> Params.Pixel.GridNoValue then begin
      if Params.Pixel.GridNoValue <> GIS_GRID_NOVALUE then
        FNoDataValue := Params.Pixel.GridNoValue
      else
        Params.Pixel.GridNoValue := FNoDataValue ;
    end;
  end ;

  lastColorZoneIndex := -1 ;
  if IsGridImage or IsTiled then
    FAntialias := Params.Pixel.Antialias ;

  // tolerance
  if FMinZ > FMaxZ then
    dZEpsilon := GIS_SINGLE_RESOLUTION
  else if FMinZ = 0 then
    dZEpsilon := Abs(FMaxZ) * GIS_SINGLE_RESOLUTION
  else if FMaxZ = 0 then
    dZEpsilon := Abs(FMinZ) * GIS_SINGLE_RESOLUTION
  else
    // based on System.Math
    dZEpsilon := Min(Abs(FMinZ), Abs(FMaxZ)) * GIS_SINGLE_RESOLUTION ;
end ;

function TGIS_LayerPixel.bandsMappingChanging : Boolean ;
begin
  Result := False ;
end;

function TGIS_LayerPixel.resolveBandDefintion : Boolean ;
var
  sorg    : String ;
  sorg_pg : String ;
  str     : String ;
  str_pg  : String ;
  lst     : TStringList ;
  k       : Integer ;
  i       : Integer ;
  stmp    : String  ;
  spg     : String  ;
const
  PAGE_INFO  = ':PG' ;
  BANDS_INFO = '.BandsDef.' ;
begin
  Result := False ;
  k := 0 ;
  spg := PAGE_INFO + IntToStr( FCurrentPage ) ;

  sorg := bandsDefinition.ToString ;
  sorg_pg := sorg + spg ;

  str := GisMetadataAsString( GetClassName( self ) + BANDS_INFO + sorg, sorg ) ;

  str_pg := GisMetadataAsString( GetClassName( self ) + BANDS_INFO + sorg_pg, sorg ) ;

  lst := TStringList.Create ;
  try
    lst.Text := FForcedBandsDefinition ;

    if lst.Count > 0 then begin
      // try to find first non page definition
      for i := 0 to lst.Count - 1 do begin
        stmp := lst[k] ;
        k := Pos( PAGE_INFO, stmp ) - StringFirst ;
        if k < 0 then begin
          str := stmp ;
          break ;
        end;
      end;

      // try to find page based definition andure that it is the last parameter
      for i := 0 to lst.Count - 1 do begin
        stmp := lst[k] ;
        k := Pos( spg + ';', stmp + ';' ) ;
        if k > 1 then begin
          str := stmp ;
          break ;
        end;
      end;
    end;

  finally
    FreeObject( lst ) ;
  end;

  // trunate any page info
  k := Pos(PAGE_INFO, str ) ;
  if k > 1 then
    str := Copy( str, StringFirst, k - StringFirst ) ;

  if sorg = str then
    exit ;

  bandsDefinition.FromString( str );

  Result := True ;
end;

procedure TGIS_LayerPixel.setFileView(
  const _viewRect    : TRect
) ;
begin

end ;


function  TGIS_LayerPixel.getGridData(
  const _extent : TGIS_Extent ;
  const _grid   : TGIS_GridArray
) : Boolean ;
var
  buf         : TGIS_SingleArray ;
  bidx,
  oheight,
  owidth      : Integer ;
  iheight,
  iwidth      : Integer ;
  width,
  height      : Integer ;
  osx, osy    : Double ;
  sosx, sosy  : Double ;
  isx, isy    : Double ;
  il, it,
  ol, ot      : Integer ;
  olskip,
  otskip      : Integer ;
  vrec        : TRect ;
  vext        : TGIS_Extent ;
  ext_zoom    : Double ;
  line, i, k  : Integer ;
  bw, bh      : Double ;
  cyh, cxw    : Double ;

  opl, opt      : Integer ; //relative outpust position left, top
  opr, opb      : Integer ; //relative position right, bottom

  i_width, o_width : Double ;
  o_sc_x, i_sc_x : Double ;
  i_height, o_height : Double ;
  o_sc_y, i_sc_y : Double ;
  o_dx, o_dy  : Double ;

  is_abort   : Boolean ;
  is_ready   : Boolean ;
  is_nodata  : Boolean ;
  sw         : Integer ;
  src_grid   : TGIS_GridArray ;

  curr_line : Integer ;
  lposx, wlposx : Double ;
  tposy, wtposy : Double ;
  buf_length : Integer ;
  use_rgb_grid : Boolean ;
  lantialis   : Boolean ;

  dd          : Double ;
  hnpix       : Int64 ;
  hsnpix      : Int64 ;
  vnpix       : Int64 ;
  vsnpix      : Int64 ;
  scalePixX,
  scalePixY   : Single ;
const
  LOCAL_LIMIT = -1e20 ;

  function local_linear(_dval : Single) : Single ;
  begin
    if _dval < 0 then
      _dval := -_dval;

    if (_dval < 1.0) then
      Result := 1.0 - _dval
    else
      Result := 0.0;
  end ;


  procedure check_values ;
  var
    kk : Integer ;
  begin
    for kk := 0 to iwidth -1 do begin
      if buf[kk] <> GIS_GRID_NOVALUE then begin
        if buf[kk] = FNoDataValue then begin
          buf[kk] := GIS_GRID_NOVALUE ;
        end
        else
        if buf[kk] < LOCAL_LIMIT then begin
          buf[kk] := GIS_GRID_NOVALUE ;
        end
        else
        if buf[kk] > -LOCAL_LIMIT then begin
          buf[kk] := GIS_GRID_NOVALUE ;
        end
        else
        if buf[kk] < FMinThresholdZ then begin
          if FMinThresholdZ = FMinZ then begin
            FMinZ := buf[kk] ;
            FMinThresholdZ := FMinZ ;
            TGIS_ParamsSectionPixel(Params).Pixel.MinHeightThreshold := FMinZ ;
          end
          else begin
            if (FMinThresholdZ - buf[kk]) > 1e-5 then
              buf[kk] := GIS_GRID_NOVALUE
          end ;
        end
        else
        if buf[kk] > FMaxThresholdZ then begin
          if FMaxThresholdZ = FMaxZ then begin
            FMaxZ := buf[kk] ;
            FMaxThresholdZ := FMaxZ ;
            TGIS_ParamsSectionPixel(Params).Pixel.MaxHeightThreshold := FMaxZ ;
          end
          else begin
            if (buf[kk] - FMaxThresholdZ) > 1e-5 then
              buf[kk] := GIS_GRID_NOVALUE
          end ;
        end ;
      end;
    end;
  end;

  function local_apply_grid_contribs(
    const _num          : Integer ;
    const _contributors : TGridContributors ;
    const _currRow      : TGIS_SingleArray

  ) : Single ;
  var
    h3           : Integer;
    lvgrid3      : Single ;
    tvweight3    : Single ;
    nvweight3    : Single ;
    weight3      : Single ;
    pixel_idx3   : Integer ;
  begin
    lvgrid3   := 0;
    tvweight3 := 0 ;
    nvweight3 := 0 ;

    for h3 := 0 to _num - 1 do begin
      weight3    := _contributors[h3].weight;
      pixel_idx3 := _contributors[h3].pixel_idx;

      if pixel_idx3 >= 0 then begin
        if _currRow[pixel_idx3] = GIS_GRID_NOVALUE then begin
          nvweight3 := nvweight3 +weight3 ;
        end
        else begin
          tvweight3 := tvweight3 +weight3 ;
          lvgrid3 := lvgrid3 +_currRow[pixel_idx3] * weight3 ;
        end ;
      end ;
    end ;

    if nvweight3 < tvweight3 then begin
      if tvweight3 = 0 then
        Result := lvgrid3
      else
        Result := lvgrid3 / tvweight3 ;
    end
    else
      Result := GIS_GRID_NOVALUE ;
  end ;

  procedure localScaleGrid(
    const _srcGrid   : TGIS_GridArray ;
    const _dstGrid   : TGIS_GridArray ;
    const _lstart    : Integer ;
    const _tstart    : Integer
  ) ;
  var
    scalex12,
    scaley12: Single;
    i2, j2,
    k2, n2: Integer;
    center2: Single;
    weight2: Single;
    left2,
    right2: Integer;
    wgrid2: TGIS_SingleArray ;
    gridContributorList2 : TGridContributorList;
    pdestpixel_idx2      : Integer;
    srcHeight2,
    srcWidth2,
    dstHeight2,
    dstWidth2: Integer;

    sw2 : Single ;
    prun_idx2 : Integer ;

    cbrlst_len2 : Integer ;
    ngridval2 : Single ;
    cbr_width2  : Integer ;
    cbr_nbr2 : Integer ;
    cbr_fact_x2 : Single ;
    cbr_fact_y2 : Single ;
    currLine2 : TGIS_SingleArray ;
  begin
    if (not assigned(_srcGrid)) or (not assigned(_dstGrid)) then exit ;

    srcHeight2 := length(_srcGrid) ;
    srcWidth2  := length(_srcGrid[0]) ;
    dstHeight2 := oheight ;
    dstWidth2  := owidth ;

    if (srcWidth2  = 0) or (dstWidth2  = 0) then exit ;
    if (srcHeight2 = 0) or (dstHeight2 = 0) then exit ;

    if  dstWidth2 <= 6 then
      sw2 := 1
    else
      sw2 := 3 ;

    try

      scalex12 := scalePixX ;
      scaley12 := scalePixY ;

      cbrlst_len2 := dstWidth2 ;
      if dstWidth2 < dstHeight2 then
        cbrlst_len2 := dstHeight2 ;

      if srcHeight2 > srcWidth2 then
        cbr_width2 := srcHeight2
      else
        cbr_width2 := srcWidth2 ;

      if cbr_width2 < cbrlst_len2 then
        cbr_width2 := cbrlst_len2 ;

      SetLength( currLine2, cbr_width2 ) ;
      SetLength( wgrid2, cbrlst_len2*cbr_width2 ) ;
      SetLength( gridContributorList2, cbrlst_len2 ) ;

      {$IFDEF GIS_NORECORDS}
        for i2 := 0 to cbrlst_len2 - 1 do
          gridContributorList2[i2] := new TGridContributorEntry ;
      {$ENDIF}

      if scalex12 < 1.0 then
        cbr_fact_x2 := sw2 / scalex12
      else
        cbr_fact_x2 := sw2 ;

      if scaley12 < 1.0 then
        cbr_fact_y2 := sw2 / scaley12
      else
        cbr_fact_y2 := sw2 ;

      if cbr_fact_x2 > cbr_fact_y2 then
        cbr_nbr2  := TruncS(2 * cbr_fact_x2 + 1)
      else
        cbr_nbr2  := TruncS(2 * cbr_fact_y2 + 1) ;


      for i2 := _lstart to dstWidth2 +_lstart - 1 do begin
        gridContributorList2[i2 -_lstart].num := 0;
        SetLength(gridContributorList2[i2 -_lstart].contributors, cbr_nbr2 +3) ;

        {$IFDEF GIS_NORECORDS}
          for k2 := 0 to cbr_nbr2 +3-1 do
            gridContributorList2[i2 -_lstart].contributors[k2] := new TGridContributor ;
        {$ENDIF}

        center2 := (i2 -0.5*scalex12)/scalex12 ;
        left2   := FloorS(center2 - cbr_fact_x2) ;
        right2  := CeilS(center2 + cbr_fact_x2) ;

        for j2 := left2 to right2 do begin
          if scalex12 < 1.0 then
            weight2 :=   local_linear( (center2 - j2) * scalex12 ) * scalex12 * 256
          else
            weight2 := local_linear(center2 - j2) * 256;

          if weight2 <> 0 then begin
            if j2 < 0 then
              n2 := -j2
            else if j2 >= srcWidth2 then
              n2 := srcWidth2 - j2 + srcWidth2 - 1
            else
              n2 := j2;
            k2 := gridContributorList2[i2 -_lstart].num;
            inc(gridContributorList2[i2 -_lstart].num) ;

            gridContributorList2[i2 -_lstart].contributors[k2].pixel_idx := n2;
            gridContributorList2[i2 -_lstart].contributors[k2].weight    := weight2;
          end ;
        end ;
      end ;

      for k2 := 0 to srcHeight2 - 1 do begin
        for i2 := 0 to srcWidth2 - 1 do
          currLine2[i2] := _srcGrid[k2][i2] ; ;

        pdestpixel_idx2 := k2*dstWidth2 ;
        for i2 := 0 to dstWidth2- 1 do begin
          sw2 := local_apply_grid_contribs(gridContributorList2[i2].num,
                    gridContributorList2[i2].contributors, currLine2) ;
          wgrid2[pdestpixel_idx2 +i2] := sw2 ;
        end ;
      end ;

      for i2 := _tstart to dstHeight2 + _tstart - 1 do begin
        gridContributorList2[i2 -_tstart].num := 0;
        SetLength( gridContributorList2[i2 -_tstart].contributors, cbr_nbr2 +3 ) ;

        {$IFDEF GIS_NORECORDS}
          for k2 := 0 to cbr_nbr2 +3-1 do
            gridContributorList2[i2 -_tstart].contributors[k2] := new TGridContributor ;
        {$ENDIF}

        center2 := (i2 -0.5*scaley12)/ scaley12;
        left2   := FloorS(center2 - cbr_fact_y2) ;
        right2  := CeilS(center2 + cbr_fact_y2) ;

        for j2 := left2 to right2 do begin
          if scaley12 < 1.0 then
            weight2 := local_linear((center2 - j2) * scaley12) * scaley12 * 256
          else
            weight2 := local_linear(center2 - j2) * 256;

          if weight2 <> 0 then begin
            if j2 < 0 then
              n2 := -j2
            else if j2 >= srcHeight2 then
              n2 := srcHeight2 - j2 + srcHeight2 - 1
            else
              n2 := j2;
            k2 := gridContributorList2[i2 -_tstart].num;
            inc(gridContributorList2[i2 -_tstart].num) ;
            gridContributorList2[i2 -_tstart].contributors[k2].pixel_idx := n2;
            gridContributorList2[i2 -_tstart].contributors[k2].weight    := weight2;
          end ;
        end ;
      end ;

      for k2 := 0 to dstWidth2 -1 do begin
        prun_idx2 := 0 ;

        for i2 := 0 to srcHeight2 -1 do
          currLine2[i2] := wgrid2[k2 +i2*dstWidth2] ;

        for i2 := 0 to dstHeight2 -1 do begin
          ngridval2 := local_apply_grid_contribs(gridContributorList2[i2].num,
                                        gridContributorList2[i2].contributors,
                                        currLine2
                                       ) ;
          if ngridval2 <> GIS_GRID_NOVALUE then
            _dstGrid[i2 +ot][k2 +ol] :=  ngridval2 ;
        end ;
      end ;

      for i2 := cbrlst_len2 - 1 downto 0 do
        SetLength(gridContributorList2[i2].contributors, 0) ;
      SetLength(gridContributorList2, 0) ;
    finally
      wgrid2    := nil ;
      currLine2 := nil ;
    end ;
  end ;

  procedure localScaleRow(
    const _srcRow   : TGIS_SingleArray ;
    const _dstRow   : TGIS_SingleArray ;
    const _lstartr   : Integer
  ) ;
  var
    scalex12r : Single;
    i2r, j2r,
    k2r, n2r: Integer;
    center2r: Single;
    weight2r: Single;
    left2r,
    right2r: Integer;
    gridContributorList2r : TGridContributorList;
    srcWidth2r : Integer;
    dstWidth2r : Integer;
    sw2r : Single ;
    cbrlst_len2r : Integer ;
    cbr_nbr2r : Integer ;
    cbr_fact_x2r : Single ;
  begin
    if scalePixX < 0.1/scaledWidth then
      exit ;

    srcWidth2r  := iwidth ;
    dstWidth2r  := owidth ;

    if  dstWidth2r <= 6 then
      sw2r := 1
    else
      sw2r := 3 ;

    scalex12r := scalePixX ;

    cbrlst_len2r := dstWidth2r ;

    SetLength( gridContributorList2r, cbrlst_len2r ) ;

    {$IFDEF GIS_NORECORDS}
      for i2r := 0 to cbrlst_len2r - 1 do
        gridContributorList2r[i2r] := new TGridContributorEntry ;
    {$ENDIF}

    if scalex12r < 1.0 then
      cbr_fact_x2r := sw2r / scalex12r
    else
      cbr_fact_x2r := sw2r ;


    cbr_nbr2r  := TruncS(2 * cbr_fact_x2r + 1) ;

    for i2r := _lstartr to dstWidth2r +_lstartr - 1 do begin
      gridContributorList2r[i2r -_lstartr].num := 0;
      SetLength(gridContributorList2r[i2r -_lstartr].contributors, cbr_nbr2r +3) ;

      {$IFDEF GIS_NORECORDS}
        for k2r := 0 to cbr_nbr2r +3-1 do
          gridContributorList2r[i2r -_lstartr].contributors[k2r] := new TGridContributor ;
      {$ENDIF}

      center2r := i2r / scalex12r;
      left2r   := FloorS(center2r - cbr_fact_x2r) ;
      right2r  := CeilS(center2r + cbr_fact_x2r) ;

      for j2r := left2r to right2r do begin
        if scalex12r < 1.0 then
          weight2r :=   local_linear( (center2r - j2r) * scalex12r ) * scalex12r * 256
        else
          weight2r := local_linear(center2r - j2r) * 256;

        if weight2r <> 0 then begin
          if j2r < 0 then
            n2r := -j2r
          else if j2r >= srcWidth2r then
            n2r := srcWidth2r - j2r + srcWidth2r - 1
          else
            n2r := j2r;
          k2r := gridContributorList2r[i2r -_lstartr].num;
          inc(gridContributorList2r[i2r -_lstartr].num) ;

          gridContributorList2r[i2r -_lstartr].contributors[k2r].pixel_idx := n2r;
          gridContributorList2r[i2r -_lstartr].contributors[k2r].weight    := weight2r;
        end ;
      end ;
    end ;

    for i2r := 0 to dstWidth2r- 1 do begin
      sw2r := local_apply_grid_contribs(gridContributorList2r[i2r].num, gridContributorList2r[i2r].contributors
                          , _srcRow) ;
      if sw2r <> GIS_GRID_NOVALUE then
        _dstRow[i2r +ol] :=  sw2r ;
    end ;

    for i2r := cbrlst_len2r - 1 downto 0 do
      SetLength(gridContributorList2r[i2r].contributors, 0) ;
    SetLength(gridContributorList2r, 0) ;
  end ;
begin
  Result := True ;

  if (_extent.XMin >= _extent.XMax) or
     (_extent.YMin >= _extent.YMax) then
    exit ;

  if (_extent.XMin > FExtent.XMax) or
     (_extent.XMax < FExtent.XMin) or
     (_extent.YMin > FExtent.YMax) or
     (_extent.YMax < FExtent.YMin) then
    exit ;

  if ( FCellWidth  = 0 ) or
     ( FCellHeight = 0 ) then
    exit ;
  height := length(_grid) ;
  width  := length(_grid[0]) ;

  o_dx := (_extent.XMax -_extent.XMin)/width ;
  o_dy := (_extent.YMax -_extent.YMin)/height ;

  i_sc_x := baseCellWidth/(FExtent.XMax -FExtent.XMin) ;
  o_sc_x := width/(_extent.XMax -_extent.XMin) ;
  bw := (FExtent.XMax -FExtent.XMin)/FCellWidth ;

  i_sc_y := baseCellHeight/(FExtent.YMax -FExtent.YMin) ;
  o_sc_y := height/(_extent.YMax -_extent.YMin) ;
  bh := (FExtent.YMax -FExtent.YMin)/FCellHeight ;

  i_width := (_extent.XMax -_extent.XMin)*i_sc_x ;
  o_width := i_width*(o_sc_x/i_sc_x) ;

  i_height := (_extent.YMax -_extent.YMin)*i_sc_y ;
  o_height := i_height*(o_sc_y/i_sc_y) ;

  lantialis := FAntialias ;
  FAntialias := False ;
  ext_zoom := setFileScaleXY( o_width, i_width, o_height, i_height ) ;

  if ext_zoom = 1 then begin
    if scaledWidth = 0 then begin
      scaledWidth  := baseCellWidth ;
      scaledHeight := baseCellHeight ;
    end;
    sw := scaledWidth ;
    if ( o_width / i_width )
       >
       ( o_height / i_height )
    then
      ext_zoom := setFileScale( o_width, i_width)
    else
      ext_zoom := setFileScale( o_height, i_height) ;

    if (ext_zoom <> 1) and FAntialias then begin
      if (scaledWidth <= 1) or (scaledHeight <= 1) then  begin
        exit ;
      end ;
    end ;
    extZoomY := ext_zoom ;
    extZoomX := ext_zoom ;
    if  ext_zoom <> 1 then begin
      scaledWidth  := RoundS(baseCellWidth*extZoomX) ;
      scaledHeight := RoundS(baseCellHeight*extZoomY) ;
    end ;
  end
  else begin
    if extZoomX <> 1 then
      scaledWidth  := RoundS(baseCellWidth*extZoomX) ;
    if extZoomY <> 1 then
      scaledHeight := RoundS(baseCellHeight*extZoomY) ;
  end ;

  extZoom := ext_zoom ;

  Alive ;

  if extZoom <> 1 then begin
    isx := (FExtent.XMax -FExtent.XMin)/scaledWidth ;
    isy := (FExtent.YMax -FExtent.YMin)/scaledHeight ;
  end
  else begin
    isx := (FExtent.XMax -FExtent.XMin)/baseCellWidth ;
    isy := (FExtent.YMax -FExtent.YMin)/baseCellHeight ;
  end;

  osx := (_extent.XMax -_extent.XMin)/width  ;
  osy := (_extent.YMax -_extent.YMin)/height ;

  if( osx = 0) or (osy = 0) then
    exit ;

  {$IFDEF GIS_NORECORDS}
    vext := new TGIS_Extent ;
  {$ENDIF}

  cyh := isy/8192 ; // small increment for one pixel in coordinates
  cxw := isx/8192 ;

  if _extent.XMin > FExtent.XMin then begin
    vext.XMin := _extent.XMin ;
    ol := 0 ;

    il := FloorS((_extent.XMin -FExtent.XMin +cxw)/isx) ;
    if osx < isx then
      sosx :=(il +1)*isx -(_extent.XMin -FExtent.XMin)
    else
      sosx := isx ;

    if sosx < 0 then
      sosx := 0 ;
  end
  else begin
    vext.XMin := FExtent.XMin ;
    il := 0 ;
    ol := RoundS((FExtent.XMin - _extent.XMin)/osx) ;
    sosx := isx ;
  end ;
  if Abs(osx -isx) < bw/i_width then begin
    if osx/isx > 0.5 then begin
     osx :=isx ;
     sosx := isx ;
    end;
  end;

  if _extent.YMin > FExtent.YMin then
    vext.YMin := _extent.YMin
  else begin
    vext.YMin := FExtent.YMin ;
  end ;

  if _extent.XMax < FExtent.XMax then
    vext.XMax := _extent.XMax
  else
    vext.XMax  := FExtent.XMax ;

  if _extent.YMax <= FExtent.YMax then begin
    vext.YMax := _extent.YMax ;
    it := FloorS((FExtent.YMax -_extent.YMax +cyh)/isy) ;
    ot := 0 ;
    if osy < isy then
      sosy :=  isy -(FExtent.YMax -it*isy -_extent.YMax)
    else
      sosy := isy ;
  end
  else begin
    vext.YMax  := FExtent.YMax ;
    it := 0 ;
    ot := RoundS((_extent.YMax -FExtent.YMax)/osy) ;
    if osy < isy then
      sosy := osy
    else
      sosy := isy ;
  end ;
  if Abs(osy -isy) < bh/i_height then begin
    if osy/isy > 0.5 then begin
     osy  := isy ;
     sosy := isy ;
    end;
  end;

  if Abs(o_width -i_width) > 1/i_width then begin
    if o_width/i_width < 64 then begin
      iwidth  := FloorS((vext.XMax -FExtent.XMin +isx/i_width)/isx)
                -FloorS((vext.XMin -FExtent.XMin +isx/i_width)/isx) +1;
    end
    else begin //enlargement over 64 times
      iwidth  := FloorS((vext.XMax -FExtent.XMin +isx)/isx)
                -FloorS((vext.XMin -FExtent.XMin +isx)/isx) +1;
    end;

    owidth  := FloorS((vext.XMax -_extent.XMin +osx/o_width)/osx)
              -FloorS((vext.XMin -_extent.XMin +osx/o_width)/osx) +1;
  end
  else begin
    owidth := RoundS(o_width) ;
    iwidth := RoundS(i_width) ;
  end ;

  if Abs(o_height -i_height) > 1/i_height then begin
    if o_height/i_height < 64 then begin
      iheight := FloorS((vext.YMax -FExtent.YMin +isy/i_height)/isy)
                -FloorS((vext.YMin -FExtent.YMin +isy/i_height)/isy) +1;
    end
    else begin //enlargement over 64 times
      iheight := FloorS((vext.YMax -FExtent.YMin +isy)/isy)
                -FloorS((vext.YMin -FExtent.YMin +isy)/isy) +1;
    end;
    oheight := FloorS((vext.YMax -_extent.YMin +osy/o_height)/osy)
              -FloorS((vext.YMin -_extent.YMin +osy/o_height)/osy) +1;
  end
  else begin
    oheight := RoundS(o_height) ;
    iheight := RoundS(i_height) ;
  end ;


  if oheight +ot > height then begin
    if ot > 0 then begin
      oheight := height -ot ;
    end
    else begin
      oheight := height ;
      ot := 0 ;
    end ;
  end ;

  if owidth +ol > width then begin
    if ol > 0 then begin
      owidth := width -ol ;
    end
    else begin
      owidth := width ;
      ol := 0 ;
    end ;
  end ;

  if baseCellWidth > scaledWidth then
    buf_length := baseCellWidth
  else
    buf_length := scaledWidth ;


  if ext_zoom <> 1 then begin
    if iwidth +il > scaledWidth then begin
      if iwidth = owidth then begin
        iwidth := scaledWidth - il;
        owidth := iwidth ;
       end
       else
        iwidth := scaledWidth - il;
    end ;

    if (iwidth < 0) or (il < 0) then begin
      il := 0 ;
      iwidth := scaledWidth ;
    end ;
    if iheight +it > scaledHeight then begin
      if iheight = oheight then begin
        iheight := scaledHeight -it ;
        oheight := iheight ;
      end
      else
        iheight := scaledHeight -it ;
    end ;
    if (iheight < 0)  or (it < 0) then begin
      it := 0 ;
      iheight := scaledHeight ;
    end;
  end
  else begin
    scaledWidth  := baseCellWidth ;
    scaledHeight := baseCellHeight ;
    if iwidth +il > baseCellWidth then begin
      if iwidth = owidth then begin
        iwidth := baseCellWidth - il;
        owidth := iwidth ;
       end
       else
        iwidth := baseCellWidth - il;
    end ;
    if (iwidth < 0)  or (il < 0) then begin
      il := 0 ;
      iwidth := baseCellWidth ;
    end;
    if iheight +it > baseCellHeight then begin
      if iheight = oheight then begin
        iheight := baseCellHeight -it ;
        oheight := iheight ;
      end
      else
        iheight := baseCellHeight -it ;
    end ;
    if (iheight < 0)  or (it < 0) then begin
      it := 0 ;
      iheight := baseCellHeight ;
    end;
  end;

  if (iwidth <= 0) or (iheight <= 0) then
    exit ;

  vrec := Rect( il, it, il +iwidth, it +iheight) ;

  setFileView(vrec) ;

  use_rgb_grid := false ;

  if not FIsNativeGridImage then begin
    if (FGridBand <= 0) and rgbAsGrid then begin
      use_rgb_grid := true ;
    end ;
  end ;

  if FMinZ > FMaxZ then
    prepareMinMaxZ ;

  is_nodata := True ;
  is_abort  := False ;
  is_ready  := False ;
  while is_nodata do begin
    getAsyncState(is_abort, is_ready, is_nodata) ;
    if ( not is_ready ) and assigned( Viewer ) then begin
        if Viewer.Ref.HourglassShake then begin
          Result := True ;
          is_abort := True ;
          getAsyncState(is_abort, is_ready, is_nodata) ;
          exit ;
        end;
    end;
  end;
  Result := is_ready ;

  lposx := isx*il +(isx -sosx);
  tposy := isy*it +(isy -sosy);

  if FExtent.XMin >_extent.XMin then
    opl := RoundS((FExtent.XMin -_extent.XMin )/o_dx)
  else
    opl := 0 ;

  if _extent.XMax > FExtent.XMax then
    opr := RoundS((_extent.XMax - FExtent.XMax)/o_dx)
  else
    opr := 0 ;

  owidth := width -opl -opr ;

  if _extent.YMax > FExtent.YMax then
    opt := RoundS((_extent.YMax -FExtent.YMax)/o_dy)
  else
    opt := 0 ;
  if FExtent.YMin > _extent.YMin  then begin
    opb := RoundS((FExtent.YMin -_extent.YMin)/o_dy) ;
  end
  else
    opb := 0 ;
  oheight := height -opt -opb;


  curr_line := -1 ;
  wtposy := tposy ;

  if (iwidth = owidth) and (iheight = oheight) then begin
    line := it ;
    SetLength( buf, buf_length) ;
    for k := ot to oheight +ot -1 do begin
      if use_rgb_grid then
        getNativeLineGrayGrid(buf, line, il, iwidth)
      else
        getNativeLine(buf, line, il, iwidth) ;

      check_values ;
      inc(line) ;
      for i := 0 to owidth -1 do
        if buf[i] <> GIS_GRID_NOVALUE then
          _grid[k, i +ol] := buf[i] ;
    end ;
  end
  else begin
    if (not lantialis) or (oheight < iheight) or (owidth = iwidth) then begin
      SetLength( buf, buf_length) ;
      scalePixX := isx/osx ;
      for k := ot to oheight +ot -1 do begin
        line := TruncS(wtposy/isy) ;
        if line >= scaledHeight then
          line := scaledHeight -1 ;

        if curr_line <> line then begin
          if use_rgb_grid then
            getNativeLineGrayGrid(buf, line, il, iwidth)
          else
            getNativeLine(buf, line, il, iwidth) ;

          check_values ;
          curr_line := line ;

        end
        else begin
          for i := ol to owidth +ol -1 do
            if _grid[k -1, i] <> GIS_GRID_NOVALUE then
              _grid[k, i] := _grid[k -1, i] ;
          wtposy := wtposy +osy ;
          continue ;
        end;

        if (not FAntialias) then
         begin
          wlposx := lposx ;
          for i := ol to owidth +ol -1 do begin
            bidx := TruncS(wlposx/isx) -il;
            if bidx < 0 then
              bidx := 0
            else
            if bidx >= buf_length then
              bidx := buf_length -1 ;

            if buf[bidx] <> GIS_GRID_NOVALUE then
              _grid[k, i] := buf[bidx]  ;
            wlposx := wlposx + osx ;
          end;
        end
        else
        begin
          if ol = 0 then begin
            if sosx > 0 then
              olskip := RoundS(((isx -sosx)/isx)*(isx/osx))
            else
              olskip := 0 ;
          end
          else
            olskip := 0 ;
          localScaleRow(buf, _grid[k], olskip) ;
        end ;
        wtposy := wtposy +osy ;
      end;
      buf := nil ;
    end
    else begin
      scalePixX := isx/osx ;
      olskip := 0 ;
      if il > 0 then begin
        dd := _extent.XMin - FExtent.XMin ;
        hnpix := TruncS (dd/osx) ;
        hsnpix := TruncS(dd/isx) ;
        olskip := TruncS(hnpix-hsnpix*scalePixX) ;
        if olskip < 0 then begin
          dec(il) ;
          inc(iwidth) ;
          olskip := RoundS(scalePixX) +olskip ;
        end;
      end;
      scalePixY := isy/osy ;
      otskip := 0 ;
      if it > 0 then begin
        dd := FExtent.YMax -_extent.YMax ;
        vnpix := TruncS(dd/osy) ;
        vsnpix := TruncS(dd/isy) ;
        otskip := TruncS(vnpix-vsnpix*scalePixY) ;
        if otskip < 0 then begin
          dec(it) ;
          inc(iheight) ;
          otskip := RoundS(scalePixY) +otskip ;
        end;
      end;


      if (it + iheight) < scaledHeight then
        inc(iheight) ;
      if (il + iwidth) < scaledWidth then
        inc(iwidth) ;
      src_grid := InitializeGrid( iheight, iwidth ) ;
      setNoDataTable( src_grid ) ;
      curr_line := -1 ;

      for k := it to iheight +it -1 do begin
        line := k ;
        if line >= scaledHeight then
          line := scaledHeight -1 ;

        if curr_line <> line then begin
          buf := src_grid[k -it] ;
          if use_rgb_grid then
            getNativeLineGrayGrid(buf, line, il, iwidth)
          else
            getNativeLine(buf, line, il, iwidth) ;

          check_values ;
          curr_line := line ;
        end
        else begin
          for i := il to iwidth +il -1 do
            src_grid[k, i] := src_grid[k -1, i] ;
          continue ;
        end;
      end;

      if (owidth = iwidth) and (oheight = iheight)then begin
        for k := ot to oheight +ot -1 do begin
          for i := ol to owidth +ol -1 do
            if src_grid[k, i] <> GIS_GRID_NOVALUE then
              _grid[k, i] := src_grid[k, i] ;
        end;
      end
      else begin
        localScaleGrid(src_grid, _grid, olskip, otskip) ;
      end ;
      src_grid := nil ;
    end ;
  end ;

  extZoom := 1 ;

  FAntialias := lantialis ;
  if assigned(oLockListW) then begin
    for i := 0 to oLockListW.Count -1do begin
      if fromBand = oLockListW.Items[i].fromBand then begin
        oLockListW.Items[i].FAntialias := FAntialias ;
        oLockListW.Items[i].getGridData(_extent, _grid ) ;
      end;
    end ;
  end ;

end ;

function  TGIS_LayerPixel.importPixelData( const _layer: TGIS_LayerPixel
                                         ) : Boolean ;
begin
  Result := False ;
end;

function TGIS_LayerPixel.getBitmapData(
  const _extent : TGIS_Extent ;
  const _bitmap : TGIS_Pixels ;
  const _width  : Integer ;
  const _height : Integer
) : Boolean ;
var
  pix         : Integer ;
  buf         : TGIS_Pixels ;
  bidx,
  oheight,
  owidth      : Integer ;
  iheight,
  iwidth      : Integer ;
  width,
  height      : Integer ;
  il, it,
  opl, opt      : Integer ; //relative outpust position left, top
  opr, opb      : Integer ; //relative position right, bottom
  vrec        : TRect ;
  cext        : TGIS_Extent ; //Common ext
  ext_zoom    : Double ;
  line, i, k  : Integer ;
  i_dx, i_dy  : Double ;
  o_dx, o_dy  : Double ;
  oi_dx, oi_dy  : Double ;
  i_width, o_width : Double ;
  i_height, o_height : Double ;
  is_abort   : Boolean ;
  is_ready   : Boolean ;
  is_nodata  : Boolean ;
  sw         : Integer ;
  src_grid   : TGIS_GridArray ;
  curr_line : Integer ;
  lidx, llidx : Integer ;
  lposx, wlposx : Double ;
  rposx, bposy  : Double ;
  tposy, wtposy : Double ;
  buf_length    : Integer ;
  no_zoom       : Boolean ;
begin
  Result := True ;
  if (_extent.XMin >= _extent.XMax) or
     (_extent.YMin >= _extent.YMax) then
    exit ;

  if (_extent.XMin > FExtent.XMax) or
     (_extent.XMax < FExtent.XMin) or
     (_extent.YMin > FExtent.YMax) or
     (_extent.YMax < FExtent.YMin) then
    exit ;

  if ( FCellWidth  = 0 ) or
     ( FCellHeight = 0 ) then
    exit ;
  if( _width <= 0) or (_height <= 0) then
    exit ;
  if (_width*_height) > length(_bitmap) then
    exit ;

  if FIsNativeGridImage then begin //grid as ARGB
    src_grid := InitializeGrid( _height, _width ) ;
    setNoDataTable( src_grid ) ;
    Result := getGridData( _extent , src_grid ) ;
    if Result then
      gridToARGBTable( _extent, _bitmap, src_grid ,_width, _height ) ;
    src_grid := nil ;
    exit ;
  end;

  height := _height ;
  width  := _width ;

  i_dx := (FExtent.XMax -FExtent.XMin)/baseCellWidth ;
  i_dy := (FExtent.YMax -FExtent.YMin)/baseCellHeight ;

  o_dx := (_extent.XMax -_extent.XMin)/width ;
  o_dy := (_extent.YMax -_extent.YMin)/height ;

  i_width := (FExtent.XMax -FExtent.XMin)/i_dx ;
  o_width := (FExtent.XMax -FExtent.XMin)/o_dx ; ;

  i_height := (FExtent.YMax -FExtent.YMin)/i_dy ;
  o_height := (FExtent.YMax -FExtent.YMin)/o_dy ;

  ext_zoom := setFileScaleXY( o_width, i_width, o_height, i_height ) ;

  if ext_zoom <> 1 then begin
    i_dx := (FExtent.XMax -FExtent.XMin)/(baseCellWidth*extZoomX) ;
    i_dy := (FExtent.YMax -FExtent.YMin)/(baseCellHeight*extZoomY) ;
  end;

  if ext_zoom = 1 then begin
    if scaledWidth = 0 then begin
      scaledWidth  := baseCellWidth ;
      scaledHeight := baseCellHeight ;
    end;

    sw := scaledWidth ;
    scaledWidth  := baseCellWidth ;
    scaledHeight := baseCellHeight ;

    if ( o_width / i_width )
       >
       ( o_height / i_height )
    then
      ext_zoom := setFileScale( o_width, i_width)
    else
      ext_zoom := setFileScale( o_height, i_height) ;

    if sw <> scaledWidth then
      setupParams ;

    if (ext_zoom <> 1) and FAntialias then begin
      if (scaledWidth <= 1) or (scaledHeight <= 1) then  begin
        exit ;
      end ;
    end ;
    extZoomY := ext_zoom ;
    extZoomX := ext_zoom ;
    if  ext_zoom <> 1 then begin
      scaledWidth   := RoundS(baseCellWidth*ext_zoom) ;
      scaledHeight := RoundS(baseCellHeight*ext_zoom) ;
      i_dx := (FExtent.XMax -FExtent.XMin)/(baseCellWidth*extZoomX) ;
      i_dy := (FExtent.YMax -FExtent.YMin)/(baseCellHeight*extZoomY) ;
    end ;
  end
  else begin
    if extZoomX <> 1 then begin
      scaledWidth  := RoundS(baseCellWidth*extZoomX) ;
    end ;
    if extZoomY <> 1 then begin
      scaledHeight := RoundS(baseCellHeight*extZoomY) ;
    end ;
  end ;

  extZoom  := ext_zoom ;

  if _extent.XMin > FExtent.XMin then
    il := FloorS((_extent.XMin - FExtent.XMin)/i_dx)
  else
    il := 0 ;

  iwidth := scaledWidth -il ;
  if FExtent.XMax > _extent.XMax then begin
    iwidth := iwidth -TruncS((FExtent.XMax - _extent.XMax)/i_dx) ;
    if iwidth = 0 then
      inc(iwidth) ;
  end;

  if FExtent.YMax > _extent.YMax then
    it := FloorS((FExtent.YMax - _extent.YMax)/i_dy)
  else
    it := 0 ;

  iheight := scaledHeight -it;
  if _extent.YMin > FExtent.YMin then begin
    if RoundS((_extent.YMin - FExtent.YMin)/i_dy) <= iheight then
      iheight := iheight -RoundS((_extent.YMin -FExtent.YMin)/i_dy) ;
  end;

  if FExtent.XMin >_extent.XMin then
    opl := RoundS((FExtent.XMin -_extent.XMin )/o_dx)
  else
    opl := 0 ;

  if _extent.XMax > FExtent.XMax then
    opr := RoundS((_extent.XMax - FExtent.XMax)/o_dx)
  else
    opr := 0 ;

  owidth := width -opl -opr ;

  if _extent.YMax > FExtent.YMax then
    opt := RoundS((_extent.YMax -FExtent.YMax)/o_dy)
  else
    opt := 0 ;
  if FExtent.YMin > _extent.YMin  then begin
    opb := RoundS((FExtent.YMin -_extent.YMin)/o_dy) ;
  end
  else
    opb := 0 ;
  oheight := height -opt -opb;

  cext := GisCommonExtent(_extent, FExtent) ;
  i_width := (cext.XMax -cext.XMin)/i_dx ;
  o_width := (cext.XMax -cext.XMin)/o_dx ; ;

  i_height := (cext.YMax -cext.YMin)/i_dy ;
  o_height := (cext.YMax -cext.YMin)/o_dy ;;


  Alive ;

  if Abs(o_width -i_width) < 1 then
    no_zoom := True
  else
    no_zoom := False ;


  vrec := Rect( il, it, il +iwidth, it +iheight) ;

  setFileView(vrec) ;

  is_nodata := True ;
  is_abort  := False ;
  is_ready  := False ;
  while is_nodata do begin
    getAsyncState(is_abort, is_ready, is_nodata) ;
    if ( not is_ready ) and assigned( Viewer ) then begin
        if Viewer.Ref.HourglassShake then begin
          Result := True ;
          is_abort := True ;
          getAsyncState(is_abort, is_ready, is_nodata) ;
          exit ;
        end;
    end;
  end;
  Result := is_ready ;

  if cext.XMin > FExtent.XMin then
    lposx :=  cext.XMin -FExtent.XMin
  else
    lposx := 0 ;

  rposx :=  cext.XMax - cext.XMin + lposx;

  if cext.YMax < FExtent.YMax then begin
    tposy := FExtent.YMax -cext.YMax ;
    bposy := FExtent.YMax -cext.YMin ;
  end
  else begin
    tposy := 0 ;
    bposy := cext.YMax - cext.YMin ;
  end;


  wtposy := tposy ;

  curr_line := -1 ;

  if no_zoom then begin
    if iwidth < RoundS(i_width) then
      iwidth := RoundS(i_width) ;
    if iheight < RoundS(i_height) then
      iheight := RoundS(i_height) ;

    if (opl +iwidth) > width then begin
      if (width -opl) >= 0 then
        iwidth := width -opl
      else
        opl := width -iwidth ;
    end;

    lidx := width*opt +opl; ;
    line := it ;

    for k := 0 to iheight -1 do begin
      if line >= scaledHeight then
        break ;
      if lidx >= length(_bitmap) then
        break ;

      getLinePixels(_bitmap, lidx, line, il, iwidth) ;

      inc(line) ;
      lidx := lidx + width ;
      if lidx >= width*height then
        break ;
    end ;
  end
  else begin
    oi_dx := o_dx/i_dx ;
    oi_dy := o_dy/i_dy ;
    oheight := RoundS(i_height/oi_dy) ;
    owidth  := RoundS(i_width/oi_dx) ;
    if scaledWidth > baseCellWidth then begin
      SetLength( buf, scaledWidth) ;
      buf_length := scaledWidth ;
    end
    else begin
      SetLength( buf, baseCellWidth) ;
      buf_length := baseCellWidth ;
    end;

    k := opt ;
    while wtposy < bposy do begin
      lidx := width*k ;
      if lidx +owidth > length(_bitmap) then
        break ;

      line := TruncS(wtposy/i_dy) ;
      if line >= scaledHeight then
        line := scaledHeight -1 ;

      if curr_line <> line then begin
        getLinePixels(buf, 0, line, il, iwidth) ;
        curr_line := line ;
      end
      else begin

        llidx := lidx -width ;
        for i := opl to owidth +opl -1 do begin
          _bitmap[lidx +i] := _bitmap[llidx +i] ;
        end;
        wtposy := wtposy +o_dy ;
        inc(k) ;
        continue ;
      end;
      wlposx := lposx ;

      i := opl ;
      while wlposx < rposx do begin
        bidx := TruncS(wlposx/i_dx) -il;
        if bidx < 0 then
          bidx := 0
        else
        if bidx >= buf_length then
          bidx := buf_length -1 ;

        if bandsMap[3] = -1 then begin
          pix := buf[bidx] or Integer($FF000000) ;
          _bitmap[lidx +i] := pix  ;
        end
        else begin
          if bidx >= length(buf) then begin
            exit ;
          end;

          pix := buf[bidx] ;
          if bandMask <> 0 then
            _bitmap[lidx +i] := (_bitmap[lidx +i] and bandMask) or pix
          else
          if (pix and Integer($FF000000)) <> 0 then begin
            _bitmap[lidx +i] := pix  ;
          end;
        end;
        wlposx := wlposx + o_dx ;
        inc(i) ;
        if i >= width  then
          break ;
      end;
      wtposy := wtposy +o_dy ;
      inc(k) ;
    end;
  end;
  buf  := nil ;

  if assigned(oLockListW) then begin
    for i := 0 to oLockListW.Count -1do
      if assigned(oLockListW.Items[i].CS) and assigned(CS)then begin
        if oLockListW.Items[i].CS.EPSG = CS.EPSG then
          oLockListW.Items[i].getBitmapData(
            _extent, _bitmap, _width, _height
          ) ;
      end ;
  end
  else
  if assigned(oParentLockList) then begin
    for i := 0 to oParentLockList.Count -1do
      oParentLockList.Items[i].getBitmapData(
        _extent, _bitmap, _width, _height
      ) ;
  end ;

  if bandsMappingChanging then
    finalARGBMap(_bitmap, _width, _height) ;

end ;

function TGIS_LayerPixel.putBitmapRawData(
  const _extent : TGIS_Extent ;
  const _bitmap : TGIS_Pixels ;
  const _width  : Integer ;
  const _height : Integer
) : Boolean ;
var
  portion : Integer ;
  wl       : TGIS_LayerPixel ;
  w, h     : Integer ;
  sImpBmp  : TGIS_Pixels ;
  ot, ol   : Integer ;
  ow, oh   : Integer ;
  bt, bl   : Integer ;
  bpos     : Integer ;
  pos      : Int64 ;
  {$IFDEF OXYGENE}
    buf    : TBytes  ;
  {$ENDIF}

begin

  if assigned(tempFileStream) then begin
    if FExtent.XMin > _extent.XMin then begin
      bl := RoundS((FExtent.XMin - _extent.XMin)/FPixelSize.X) ;
      ol := 0 ;
    end
    else begin
      bl := 0 ;
      ol := RoundS((_extent.XMin -FExtent.XMin )/FPixelSize.X) ;
    end;

    if FExtent.YMax < _extent.YMax then begin
      bt := RoundS((_extent.YMax - FExtent.YMax)/FPixelSize.Y) ;
      ot := 0 ;
    end
    else begin
      bt := 0 ;
      ot := RoundS((FExtent.YMax - _extent.YMax )/FPixelSize.Y) ;
    end;
    ow := _width ;
    if (ol +ow) > FBitWidth then
      ow := FBitWidth -ol ;

    oh := _height ;
    if (ot +oh) > FBitHeight then
      oh := FBitHeight -ot ;

    if (ow > 0) and (oh > 0) then begin
      portion := sizeOf(Integer) * ow ;
      pos      := Int64(sizeOf(Integer)) *( Int64(FBitWidth) * ot + ol) ;
      bpos     := bt*_width + bl ;

      for h := ot to ot +oh-1 do begin
        tempFileStream.Position := pos ;
        {$IFDEF OXYGENE}
         for w := 0 to ow-1 do begin
           buf := BitConverter.GetBytes( _bitmap[bpos +w] ) ;
           tempFileStream.Write( buf, length( buf ) ) ;
         end;
        {$ELSE}

           tempFileStream.Write( _bitmap[bpos], portion ) ;
        {$ENDIF}
        pos := pos + FBitWidth * sizeOf(Integer) ;
        bpos := bpos +_width ;
      end ;
    end;
    Result := True ;
    exit ;
  end;
  Result := False ;

  if not assigned(oBitmap) then
    exit ;

  if ( _extent.XMax <= _extent.XMin ) or
     ( _extent.YMax <= _extent.YMin )
  then
    exit ;

  if ( FExtent.XMin >= _extent.XMax ) or
     ( FExtent.XMax <= _extent.XMin ) or
     ( FExtent.YMin >= _extent.YMax ) or
     ( FExtent.YMax <= _extent.YMin )
  then
    exit ;

  if not assigned(_bitmap) then
    exit ;

  if ( _width = 0 ) or ( _height = 0 ) then
    exit ;
  if length(_bitmap) <> (_width*_height) then
    exit ;

  w := _width  ;
  h := _height ;
  //Make work layer
  wl := TGIS_LayerPixel.Create ;
  wl.Build('', self.FCS, _extent, w, h) ;

  sImpBmp := wl.oBitmap ;
  wl.oBitmap := _bitmap ;

  wl.Viewer := Viewer ;
  wl.getBitmapPixels(FProjectedExtent, oBitmap, FBitWidth, FBitHeight) ;
  wl.oBitmap := sImpBmp ;
  FreeObject(wl) ;
  Result := True ;
end ;

procedure TGIS_LayerPixel.setBmpTransparent(
  const _bitmap   : TGIS_Pixels
) ;
var
 k, max : Integer ;
begin
  max := length(_bitmap) -1 ;
  for k := 0 to max do
//    _bitmap[k] := Integer($00FFFFFF) ;
    _bitmap[k] := 0 ;
end ;

procedure TGIS_LayerPixel.setNoDataTable(
  const _grid   : TGIS_GridArray
) ;
var
 k, i, imaxr, imaxc : Integer ;
begin
  imaxr := length(_grid) -1 ;
  if imaxr < 0 then exit ;

  imaxc := length(_grid[0]) -1 ;
  for k := 0 to imaxr do
    for i := 0 to imaxc do
    _grid[k][i] := GIS_GRID_NOVALUE ;
end ;

procedure TGIS_LayerPixel.getAsyncState(
  var _abort  : Boolean ;
  var _ready  : Boolean ;
  var _nodata : Boolean
) ;
begin
  // for safe inheritance only
  _ready  := True  ;
  _nodata := False ;
end ;


function TGIS_LayerPixel.getLine(
  const _buffer : TBytes  ;
  const _offset : Integer ;
  const _linenr : Integer ;
  const _start  : Integer ;
  const _bytes  : Integer
) : Integer ;
var
  i      : Integer ;
  lpix   : Integer ;
  maxpix : Integer ;
  pixno,
  spix   : Integer ;
  offset : Integer ;
  ccolor : TGIS_Color {$IFDEF GIS_NORECORDS} = new TGIS_Color {$ENDIF} ;
  c      : TGIS_Color {$IFDEF GIS_NORECORDS} = new TGIS_Color {$ENDIF} ;
begin
  // using by imported layer
  Result := 0 ;
  if assigned(oBitmap) then begin
    if _linenr < FBitHeight then begin
      pixno := _bytes div 3 ;
      lpix := _start div 3 ;
      maxpix := FBitWidth -lpix ;
      if maxpix < pixno then
        pixno := maxpix ;
      if pixno > 0 then begin
        Result := pixno * 3 ;
        spix := _linenr*FBitWidth +lpix ;
        for i := 0 to pixno -1 do begin
          _buffer[_offset +i*3 +0] :=  Byte(oBitmap[spix +i] ) ;
          _buffer[_offset +i*3 +1] :=  Byte(oBitmap[spix +i] shr 08) ;
          _buffer[_offset +i*3 +2] :=  Byte(oBitmap[spix +i] shr 16) ;
        end ;
      end ;
    end ;
  end
  else
  if assigned(oGrid) then begin
    if _linenr < FCellHeight then begin
      pixno := _bytes div 3 ;
      spix := _start div 3 ;
      maxpix := FCellWidth -spix ;
      if maxpix < pixno then
        pixno := maxpix ;
      if pixno > 0 then begin
        Result := 3*pixno ;

      offset := 0 ;
      pixno := pixno +spix ;

      GetColorRamp( FNoDataValue ) ;
      for i := spix to pixno -2 do begin
        if oGrid[_linenr][i] = FNoDataValue then begin
          c := colorNoData ;
          _buffer[_offset+offset  ] := c.B ;
          _buffer[_offset+offset+1] := c.G ;
          _buffer[_offset+offset+2] := c.R ;
          makeTransparent := True ;
        end
        else begin
          c := GetColorRamp( oGrid[_linenr][i] ) ;
          _buffer[_offset+offset  ] := c.B ;
          _buffer[_offset+offset+1] := c.G ;
          _buffer[_offset+offset+2] := c.R ;
        end ;
        offset := offset +3 ;
      end ;

      // last triple
      if oGrid[_linenr][pixno -1] = FNoDataValue then begin
        ccolor := colorNoData ;
        makeTransparent := True ;
      end
      else
        ccolor := GetColorRamp( oGrid[_linenr][pixno -1] ) ;

      _buffer[_offset+offset] := ccolor.B ;
      _buffer[_offset+offset+1] := ccolor.G ;
      _buffer[_offset+offset+2] := ccolor.R ;
      end ;
    end ;
  end
end ;

function TGIS_LayerPixel.getLineBasic(
  const _buffer : TBytes  ;
  const _offset : Integer ;
  const _linenr : Integer ;
  const _start  : Integer ;
  const _bytes  : Integer
) : Integer ;
var
  i      : Integer ;
  lpix   : Integer ;
  maxpix : Integer ;
  pixno,
  spix   : Integer ;
  offset : Integer ;
  ccolor : TGIS_Color {$IFDEF GIS_NORECORDS} = new TGIS_Color {$ENDIF};
  c      : TGIS_Color {$IFDEF GIS_NORECORDS} = new TGIS_Color {$ENDIF};
begin
  // using by imported layer
  Result := 0 ;
  if assigned(oBitmap) then begin
    if _linenr < FBitHeight then begin
      pixno := _bytes div 3 ;
      lpix := _start div 3 ;
      maxpix := FBitWidth -lpix ;
      if maxpix < pixno then
        pixno := maxpix ;
      if pixno > 0 then begin
        Result := pixno * 3 ;
        spix := _linenr*FBitWidth +lpix ;
        for i := 0 to pixno -1 do begin
          _buffer[_offset +i*3 +0] :=  Byte(oBitmap[spix +i] ) ;
          _buffer[_offset +i*3 +1] :=  Byte(oBitmap[spix +i] shr 08) ;
          _buffer[_offset +i*3 +2] :=  Byte(oBitmap[spix +i] shr 16) ;
        end ;
      end ;
    end ;
  end
  else
  if assigned(oGrid) then begin
    if _linenr < FCellHeight then begin
      pixno := _bytes div 3 ;
      spix := _start div 3 ;
      maxpix := FCellWidth -spix ;
      if maxpix < pixno then
        pixno := maxpix ;
      if pixno > 0 then begin
        Result := 3*pixno ;

      offset := 0 ;
      pixno := pixno +spix ;

      GetColorRamp( FNoDataValue ) ;
      for i := spix to pixno -2 do begin
        if oGrid[_linenr][i] = FNoDataValue then begin
          c := colorNoData ;
          _buffer[_offset+offset  ] := c.B ;
          _buffer[_offset+offset+1] := c.G ;
          _buffer[_offset+offset+2] := c.R ;
          makeTransparent := True ;
        end
        else begin
          c := GetColorRamp( oGrid[_linenr][i] ) ;
          _buffer[_offset+offset  ] := c.B ;
          _buffer[_offset+offset+1] := c.G ;
          _buffer[_offset+offset+2] := c.R ;
        end ;
        offset := offset +3 ;
      end ;

      // last triple
      if oGrid[_linenr][pixno -1] = FNoDataValue then begin
        ccolor := colorNoData ;
        makeTransparent := True ;
      end
      else
        ccolor := GetColorRamp( oGrid[_linenr][pixno -1] ) ;

      _buffer[_offset+offset] := ccolor.B ;
      _buffer[_offset+offset+1] := ccolor.G ;
      _buffer[_offset+offset+2] := ccolor.R ;
      end ;
    end ;
  end
end ;

function TGIS_LayerPixel.getAlphaLine(
  const _buffer : TBytes  ;
  const _offset : Integer ;
  const _linenr : Integer ;
  const _start  : Integer ;
  const _bytes  : Integer
) : Integer ;
var
  i         : Integer ;
  ss        : Integer ;
  bn        : Integer ;
  pixparams : TGIS_ParamsSectionPixel ;
  px        : TGIS_Color {$IFDEF GIS_NORECORDS} = new TGIS_Color {$ENDIF} ;
begin
  Result := _bytes ;
  if assigned(oBitmap) then begin
    ss := _linenr*baseCellWidth +_start ;
    for i := 0 to _bytes - 1 do begin
      px.ARGB := Cardinal(oBitmap[ss + i]) ;
      _buffer[_offset +i] := px.A ;
    end ;
  end
  else
  if isPartialTransparent then  begin
    pixparams := TGIS_ParamsSectionPixel(Params) ;
    bn := pixparams.Pixel.AlphaBand - 1 ;
    if assigned(workAlpha) then begin
      for i := 0 to _bytes - 1 do
        _buffer[_offset +i] := workAlpha[3*i +bn] ;
    end
    else begin
      for i := 0 to _bytes - 1 do
        _buffer[_offset +i] := $FF ;
    end;
  end ;
end ;

function  TGIS_LayerPixel.getLinePixels ( const _buffer   : TGIS_Pixels  ;
                                          const _offset   : Integer ;
                                          const _linenr   : Integer ;
                                          const _pixStart : Integer ;
                                          const _pixCount : Integer
                                        ) : Integer;
var
  i           : Integer ;
  ioff, count : Integer ;
  abuf, buf   : TBytes ;
  use_tr      : Boolean ;
  rr, rg, rb  : Integer ;
  dr          : Integer ;
  w3          : Integer ;
  unmask      : Integer ;
  sfl         : Integer ;
begin
  Result := _pixCount ;
  if (_pixStart +_pixCount) > baseCellWidth then begin
    count := baseCellWidth - _pixStart ;
  end
  else
    count := _pixCount ;
 if count <= 0 then
  exit ;
  sfl := 0 ;
  if assigned(tempFileStream) then begin
    tempFileStream.Position := (Int64(_linenr)*baseCellWidth +_pixStart)*sizeOf(Integer) ;
    {$IFDEF OXYGENE}
      for i := 0 to count-1 do
        tempFileStream.ReadInteger( _buffer[_offset +i], sizeOf(Integer) ) ;
    {$ELSE}
      tempFileStream.Read( ( Addr( _buffer[_offset]))^, count * sizeOf(Integer) ) ;
    {$ENDIF}
    exit ;
  end ;
  if assigned(oBitmap) then begin
    ioff := _linenr*baseCellWidth +_pixStart ;
    for i := 0 to count -1 do
      _buffer[_offset +i] := oBitmap[ioff +i] ;
    exit ;
  end
  else
  if assigned(oGrid) then begin
    case fromBand of
      1 :
        begin
          bandMask := $00FF0000 ;
          sfl := 16 ;
        end ;
      2 :
        begin
          bandMask := $0000FF00 ;
          sfl := 8 ;
        end ;
      3 :
        begin
          bandMask := $000000FF ;
          sfl := 0 ;
        end ;
      4 :
        begin
          bandMask := Integer($FF000000) ;
          sfl := 24 ;
        end ;
    end;

    for i := 0 to count -1 do begin
      _buffer[_offset +i] := (Integer(RoundS(oGrid[_linenr][_pixStart +i])) shl sfl) and bandMask ;
    end;

    bandMask := bandMask xor Integer($FFFFFFFF) ;
    exit ;
  end;

  if baseCellWidth > scaledWidth then
    w3 := 3*baseCellWidth
  else
    w3 := 3*scaledWidth ;

  SetLength( buf, w3 ) ;


  if getLine(buf, 3*_offset, _linenr, 3*_pixStart, 3*_pixCount) <= 0 then
    exit ;

  use_tr := isPartialTransparent or isColorNoDataGDAL ;
  if importMode then begin
    rr := 2 ;
    rg := 1 ;
    rb := 0 ;
  end
  else begin
    if assigned(oBitmap) then begin
      rr := 2 ;
      rg := 1 ;
      rb := 0 ;
    end
    else begin
      if BandsCount <= 4 then begin
        dr := BandsCount -2 ;
        rr := BandsCount -bandsMap[0] -dr ;
        rg := BandsCount -bandsMap[1] -dr ;
        rb := BandsCount -bandsMap[2] -dr ;
      end
      else begin
        rr := 2 ;
        rg := 1 ;
        rb := 0 ;
      end;
    end;
  end ;

  if use_tr then begin
    SetLength( abuf, w3 ) ;
    getAlphaLine( abuf, _offset, _linenr, _pixStart, _pixCount ) ;
    for i := _offset to _pixCount +_offset -1 do
      _buffer[i] := (abuf[i    ] shl 24) or
                    (buf[3*i +rr] shl 16) or
                    (buf[3*i +rg] shl 08) or
                     buf[3*i +rb] ;

    SetLength( abuf, 0 ) ;
  end
  else begin
    for i := _offset to _pixCount +_offset -1 do
      _buffer[i] := Integer($FF000000)   or
                    (buf[3*i +rr] shl 16) or
                    (buf[3*i +rg] shl 08) or
                     buf[3*i +rb] ;

  end;
  SetLength( buf, 0 ) ;
  if not importMode then begin
    unmask := Integer($FFFFFFFF) ;
    if unusedRed then
      unmask := unmask and Integer($FF00FFFF) ;
    if unusedGreen then
      unmask := unmask and Integer($FFFF00FF) ;
    if unusedBlue then
      unmask := unmask and Integer($FFFFFF00) ;
    if unmask <> Integer($FFFFFFFF) then begin
      for i := _offset to _pixCount +_offset -1 do
        _buffer[i] := _buffer[i] and unmask ;
    end;
  end;
end;

function  TGIS_LayerPixel.getLineBits( const _buffer   : TBytes  ;
                                       const _offset   : Integer ;
                                       const _linenr   : Integer ;
                                       const _pixStart : Integer ;
                                       const _pixCount : Integer
                                     ) : Integer ;
begin
  Result := _pixCount ;
end;

function  TGIS_LayerPixel.convertBitsToPixels (
  const _buffSrc    : TBytes  ;
  const _srcOffset  : Integer ;
  const _buffDst    : TGIS_Pixels ;
  const _dstOffset  : Integer ;
  const _pixStart   : Integer ;
  const _pixCount   : Integer
) : Integer ;
var
  i, n : Integer ;
  sidx, didx : Integer ;
  lmask   : Integer ;
  flmargin : Integer ;
  wbits  : Integer ;
  val1, val1w  : Cardinal ;
  val2, val2w  : Cardinal ;
  val3, val3w  : Cardinal ;
  val4, val4w  : Cardinal ;
  testbyte     : Byte  ;
  band_bytes_start       : Array [0..19] of Integer ; //actual bytes pier pixel per band
  bval         : Array [0..19] of Byte ;
  abitsno      : Integer ;
  workbyte     : Integer ;
  workbyte2    : Byte ;
  pal_idx      : Integer ;
  lidx         : Integer ;
  blidx         : Integer ;
  hibits       : Integer ;
  spa    : Boolean  ;

  bpos           : Integer ;
  isw            : Boolean ;
  colorsmapping : Boolean ;
  bands_local_map : Array [0..19] of Integer ;
  maxval        : Cardinal ;
  lur, lug, lub : Boolean ;
  ridx, gidx, bidx : Integer ;
const
  L_MASK = $FF ;
begin
  for i := 0 to FBandsCount -1 do
    bands_local_map[i] := bandsMap[i] ;

  lur := unusedRed   ;
  lug := unusedGreen ;
  lub := unusedBlue  ;


  if isBGR then begin
    i := bands_local_map[0] ;
    bands_local_map[0] := bands_local_map[2] ;
    bands_local_map[2] := i ;
    if lur then
      bands_local_map[2] := 0 ;
    if lub then
      bands_local_map[0] := 2 ;
  end ;
  ridx := 0 ;
  gidx := 1 ;
  bidx := 2 ;

  if alphaAssociated then begin
    if bands_local_map[FBandsCount -1] = -1 then
      alphaAssociated := False ;
  end;
  spa := False ;

  if FBandsCount < 3 then begin
    if not alphaAssociated then begin
      bands_local_map[1] := bands_local_map[0] ;
      bands_local_map[2] := bands_local_map[0]
    end
    else
      spa := True ;
  end;

  blidx := 0 ;
  for i := 0 to FBandsCount -1 do begin
    band_bytes_start[i] := blidx ;
    blidx := blidx +Integer(bytesPerBand[i]) ;
  end;

  colorsmapping := False ;
  if wordShift < 8 then
    maxval := (Integer($01FFFF) shr (8 -wordShift))
  else
    maxval := (Integer($00FFFF) shr (8 -wordShift)) ;

  sidx := _srcOffset ;
  didx := _dstOffset ;
  flmargin := ( _pixStart  * bitsPerPixel ) mod 8 ;
  bpos := flmargin ;
  wbits := 0 ;
  testbyte := 0 ;
  isw := bigEndian or isBitsString ;

  for i := 0 to _pixCount -1 do begin
    blidx := 0 ;
    for n := 0 to FBandsCount -1 do begin
      abitsno := bitsPerBand[n] ;
      hibits := abitsno mod 8 ;
      if hibits = 0 then begin
        hibits := 8 ;
      end ;
      if bpos = 0 then
        lmask := Byte(L_MASK)
      else
        lmask := Byte(L_MASK) shr bpos ;

      workbyte2 := 0 ;
      repeat
        if wbits = 0 then begin
          wbits := 8 -flmargin ;
          testbyte :=  _buffSrc[sidx] ;
          inc(sidx) ;
          flmargin := 0 ;
          if hibits = 8 then
            lmask := Byte(L_MASK) ;
        end;
        workbyte := testbyte and lmask ;
        if hibits <= wbits then begin
          if hibits <  wbits then begin
            dec(wbits, hibits) ;
            workbyte := workbyte shr wbits ;
            lmask := lmask shr hibits ;
            inc(bpos, hibits) ;
            dec(abitsno, hibits) ;
          end
          else begin
            dec(abitsno, wbits) ;
            wbits := 0 ;
            bpos  := 0 ;
          end;
          bval[blidx] := workbyte or workbyte2 ;
          inc(blidx) ;
          hibits := 8 ;
        end
        else begin
          workbyte2 := workbyte shl (abitsno -wbits) ;
          abitsno := abitsno -wbits ;
          hibits := abitsno mod 8 ;
          lmask := Byte(L_MASK) ;
          wbits := 0 ;
          bpos  := 0 ;
        end;
      until abitsno <= 0 ;
    end ;

    if (bitsPerPixel > 8) and (not spa) then begin

      if lur then
        val1 := 0
      else begin
        lidx := band_bytes_start[bands_local_map[ridx]] ;
        val1 := Cardinal(bval[lidx]) ;
        val1w := val1 ;
        if bitsPerBand[bands_local_map[ridx]] > 8 then begin
          inc(lidx) ;
          if isw then
            val1w := (val1w shl 8) or (Cardinal(bval[lidx]))
          else
            val1w := val1w or (Cardinal(bval[lidx]) shl 8) ;
          if val1w >= maxval then
            val1w := maxval ;
          val1 := (val1w shr wordShift) and $FF ;
        end ;
      end;

      if lug then
        val2 := 0
      else begin
        lidx := band_bytes_start[bands_local_map[gidx]] ;
        val2 := Cardinal(bval[lidx]) ;
        val2w := val2 ;
        if bitsPerBand[bands_local_map[gidx]] > 8 then begin
          inc(lidx) ;
          if isw then
            val2w := (val2w shl 8) or (Cardinal(bval[lidx]))
          else
            val2w := val2w or (Cardinal(bval[lidx]) shl 8) ;
          if val2w >= maxval then
            val2w := maxval ;
          val2 := (val2w shr wordShift) and $FF ;
        end ;
      end;

      if lub then
        val3 := 0
      else begin
        lidx := band_bytes_start[bands_local_map[bidx]] ;
        val3 := Cardinal(bval[lidx]) ;
        val3w := val3 ;
        if bitsPerBand[bands_local_map[bidx]] > 8 then begin
          inc(lidx) ;
          if isw then
            val3w := (val3w shl 8) or (Cardinal(bval[lidx]))
          else
            val3w := val3w or (Cardinal(bval[lidx]) shl 8) ;
          if val3w >= maxval then
            val3w := maxval ;
          val3 := (val3w shr wordShift) and $FF ;
        end ;
      end;
      if alphaAssociated then begin
        lidx := band_bytes_start[bands_local_map[FBandsCount -1]] ;
        val4 := Cardinal(bval[lidx]) ;
        val4w := val4 ;
        if bitsPerBand[bands_local_map[FBandsCount -1]] > 8 then begin
          inc(lidx) ;
          if isw then
            val4w := (val4w shl 8) or (Cardinal(bval[lidx]))
          else
            val4w := val4w or (Cardinal(bval[lidx]) shl 8) ;
          if val4w >= maxval then
            val4w := maxval ;
          val4 := (val4w shr wordShift) and $FF ;
          if val4 > 0 then
            val4 := $FF ;
        end ;


        _buffDst[didx] :=  Integer(val3 or ( val2 shl 8 ) or ( val1 shl 16)) or
                          (Integer(val4) shl 24 )
      end
      else begin
        _buffDst[didx] :=  Integer(val3 or ( val2 shl 8 ) or ( val1 shl 16)) or
                         Integer($FF000000) ;

      end;
    end
    else begin
      if bitsPerBand[0] = 16 then begin
        val1w := (Cardinal(bval[1]) shl 8) or Cardinal(bval[0]) ;
        pal_idx := (val1w shr wordShift) and $FF ;
      end
      else
        pal_idx := bval[0];

      if unusedBlue then
        val3 := 0
      else
        val3 := Integer(bitPalette[pal_idx].B ) ;
      if unusedGreen then
        val2 := 0
      else
        val2 := Integer(bitPalette[pal_idx].G ) ;
      if unusedRed then
        val1 := 0
      else
        val1 := Integer(bitPalette[pal_idx].R ) ;
      if spa then begin
        lidx := band_bytes_start[bands_local_map[FBandsCount -1]] ;
        val4 := Cardinal(bval[lidx]) ;
        _buffDst[didx] :=  Integer(val3 or ( val2 shl 8 ) or ( val1 shl 16)) or
                          (Integer(val4) shl 24 ) ;
      end
      else
        _buffDst[didx] :=  Integer(val3 or ( val2 shl 8 ) or ( val1 shl 16)) or
                         Integer($FF000000) ;
    end;

    inc(didx) ;
  end;
  Result := _pixCount ;

end;

function  TGIS_LayerPixel.Locate(
  const _ptg          : TGIS_Point       ;
  var   _rgbMapped    : TGIS_Color       ;
  var   _nativesVals  : TGIS_DoubleArray ;
  var   _transparency : Boolean
) : Boolean ;
begin
  Result := LocateEx( _ptg, _rgbMapped, _nativesVals, _transparency, 0 ) ;
end;

function  TGIS_LayerPixel.LocateEx(
  const _ptg          : TGIS_Point       ;
  var   _rgbMapped    : TGIS_Color       ;
  var   _nativesVals  : TGIS_DoubleArray ;
  var   _transparency : Boolean          ;
  const _pixelsize    : Double
) : Boolean ;
var
  pt            : TPoint ;
  ptg, uptg     : TGIS_Point ;
  part_rect     : TRect ;
  sx, sy        : Double ;
  pixparams     : TGIS_ParamsSectionPixel ;
  corpix        : TGIS_Pixels ;
  mr, mg, mb,
  ma            : Byte ;
  i             : Integer ;
  rs            : Boolean ;
  svals         : TGIS_SingleArray ;
  sc            : Double ;
  ccolor : TGIS_Color {$IFDEF GIS_NORECORDS} = new TGIS_Color {$ENDIF};
begin
  Result := False ;

  if assigned( Viewer ) and
     Viewer.Ref.InPaint
  then
    exit ;

  if Basemap then
    exit ;

  if not IsOpened then
    Open ;
  if FIsNativeGridImage then begin
    if FMinZ > FMaxZ then begin
      prepareMinMaxZ ;
      prepareAltitudeMapTable ;
      prepareColorRamp ;
    end;
  end;

  _transparency := False ;

  if (baseCellWidth = 0) or (baseCellHeight = 0) then
    exit ;

  uptg := Unproject(_ptg) ;

  if (uptg.X > FExtent.XMax) or
     (uptg.Y > FExtent.YMax) or
     (uptg.X < FExtent.XMin) or
     (uptg.Y < FExtent.YMin) then begin
    exit ;
  end ;
  {$IFDEF GIS_NORECORDS}
    ptg := new TGIS_Point;
    pt := new TPoint(0,0) ;
  {$ENDIF}
  ptg.X := uptg.X -FExtent.XMin ;
  ptg.Y := uptg.Y -FExtent.YMax ;

  sx := ( FExtent.XMax - FExtent.XMin ) / baseCellWidth ;
  sy := ( FExtent.YMin - FExtent.YMax ) / baseCellHeight ;

  pt.X := FloorS(ptg.X / sx) ;
  pt.Y := FloorS(ptg.Y / sy) ;

  if pt.Y <  0              then exit ;
  if pt.Y >= baseCellHeight then exit ;
  if pt.X <  0              then exit ;
  if pt.X >= baseCellWidth  then exit ;

  Alive ;

  if _pixelsize > sx then begin
    sc := setFileScale( (baseCellWidth*sx)/_pixelsize, baseCellWidth) ;
    if sc < 1 then begin
      pt.X := FloorS((ptg.X / sx)*sc) ;
      pt.Y := FloorS((ptg.Y / sy)*sc) ;
    end;
  end
  else
    setFileScale( baseCellWidth, baseCellWidth) ;

  try
    part_rect := Rect( pt.X, pt.Y, pt.X +1, pt.Y +1 ) ;
    setFileView(part_rect) ;
    SetLength(corpix, 1 ) ;

    if IsGridImage then begin
      SetLength(_nativesVals, FBandsCount) ;
      SetLength(svals, FBandsCount) ;

      getNativeLine(svals, pt.Y, pt.X, 1) ;
      for i := 0 to length(svals) -1 do
        _nativesVals[i] := svals[i] ;
      SetLength(svals, 0) ;
    end
    else begin

      if realBitCount <= 8 then begin
        SetLength(_nativesVals, 1) ;
        SetLength(svals, 1)
      end
      else begin
        SetLength(_nativesVals, FBandsCount) ;
        SetLength(svals, FBandsCount) ;
      end;

      getNativeLine(svals, pt.Y, pt.X, 1) ;
      for i := 0 to length(svals) -1 do
        _nativesVals[i] := svals[i] ;
      SetLength(svals, 0) ;
    end;


    pixparams := TGIS_ParamsSectionPixel(Params) ;
    if not IsGridImage then begin
      getLinePixels( corpix, 0, pt.Y, pt.X, 1 ) ;

      if makeRGBMap then
        simpleRGBCorrection(corpix, 1, 1) ;

      if makeFullRGBMap or makeColorsByVals then
        fullRGBCorrection(corpix, 1, 1) ;

      if Params.Pixel.GrayScale or isGrayScaleImage then
        ARGB2Gray( corpix, 1) ;
      if makeSomeCorrection or makeColorsByVals then
        fullCorrection(corpix, 1, 1) ;
    end
    else begin
      rs := isShadow ;
      isShadow := False ;
      i := pixparams.Pixel.GridBand ;
      if (i <= FBandsCount) and (i > 0) then
        i := i -1
      else
        i := 0 ;
      ccolor := GetColorRamp(_nativesVals[i]) ;
      if _nativesVals[i] =  FNoDataValue then
        _transparency := True ;

      isShadow := rs ;
      corpix[0] := Integer(ccolor.ARGB) ;
    end;


    ma := Byte(Integer(corpix[0] and $00FF000000) shr 24) ;
    mr := Byte(Integer(corpix[0] and $0000FF0000) shr 16) ;
    mg := Byte(Integer(corpix[0] and $000000FF00) shr 08) ;
    mb := Byte((corpix[0] and $000000FF)) ;

    _rgbMapped := TGIS_Color.FromARGB(ma ,        //alpha
                                      mr ,        //red
                                      mg ,        //green
                                      mb ) ;      //blue

    if makeTransparent then begin
      if ( redTransp  [ mr ]  and
           greenTransp[ mg ]  and
           blueTransp [ mb ] ) <> 0 then
        _transparency := True ;
    end ;

  finally
    corpix := nil ;
  end ;
  Result := True ;
end ;

function  TGIS_LayerPixel.GetGrid(
  const _extent : TGIS_Extent ;
  const _grid   : TGIS_GridArray
 ) : Boolean ;
var
  trans         : T_TransClass ;
  do_transform  : Boolean ;
  cext          : TGIS_Extent ;
  width, height : Integer ;
  sp            : Integer ;
  tm            : Int64   ;
  fv            : Boolean ;
  timeout       : Int64   ;
begin
  sp := CurrentPage ;

  fv := forViewer ;
  forViewer := False ;
  do_transform := transformNeed  or baseRotation ;
  if not do_transform then begin
    cext := GisCommonExtent(_extent, FExtent) ;
    if ( cext.XMin = cext.XMax ) or ( cext.YMin = cext.YMax ) then begin
      Result := False ;
      forViewer := fv ;
      exit ;
    end ;
  end;
  height := length(_grid) ;
  width  := length(_grid[0]) ;

  timeout := GisMetadataAsInteger( METADATA_TIMEOUT, GETBITMAP_TIMEOUT ) ;
  tm := GetTickCount ;
  Result := False ;
  if do_transform then begin
    trans := T_TransClass.Create(Self) ;
    try
      while ( not Result ) and ( ( GetTickCount - tm ) < timeout ) do
        Result := trans.getTransformedGridData( _grid, _extent, width, height ) ;
    finally
      FreeObject( trans ) ;
    end;
  end
  else begin
    while ( not Result ) and ( ( GetTickCount - tm ) < timeout ) do
      Result := getGridData( _extent , _grid ) ;
  end;
  forViewer := fv ;
end ;

function TGIS_LayerPixel.GetBitmap(
  const _extent   : TGIS_Extent ;
  const _bitmap   : TGIS_Pixels ;
  const _width  : Integer       ;
  const _height : Integer
) : Boolean ;
var
  tm      : Int64 ;
  fv      : Boolean ;
  timeout : Int64 ;
begin
  setupParams ;

  timeout := GisMetadataAsInteger( METADATA_TIMEOUT, GETBITMAP_TIMEOUT ) ;
  tm := GetTickCount ;
  Result := False ;
  fv := forViewer ;
  forViewer := False ;

  while ( not Result ) and ( ( GetTickCount - tm ) < timeout ) do begin
    Result := getBitmapPixels( _extent, _bitmap, _width, _height ) ;
  end ;
  forViewer := fv ;
end ;

function TGIS_LayerPixel.GetRawBitmap(
  const _extent   : TGIS_Extent ;
  const _bitmap   : TGIS_Pixels ;
  const _width  : Integer       ;
  const _height : Integer
 ) : Boolean ;
var
  tm      : Int64 ;
  timeout : Int64 ;
begin
  timeout := GisMetadataAsInteger( METADATA_TIMEOUT, GETBITMAP_TIMEOUT ) ;
  tm := GetTickCount ;
  Result := False ;
  while ( not Result ) and ( ( GetTickCount - tm ) < timeout ) do begin
    Result := getBitmapData( _extent, _bitmap, _width, _height ) ;
  end;
end ;

procedure  TGIS_LayerPixel.ImportLayer(
  const _layer     : TGIS_LayerPixel ;
  const _extent    : TGIS_Extent
) ;
begin
  ImportLayer(_layer, _extent, nil, 0, 0) ;
end ;

procedure  TGIS_LayerPixel.ImportLayer(
  const _layer     : TGIS_LayerPixel ;
  const _extent    : TGIS_Extent ;
  const _cs        : TGIS_CSCoordinateSystem ;
  const _width     : Cardinal ;
  const _height    : Cardinal  ) ;
begin
  ImportLayer( _layer,
               _extent,
               _cs,
               _width,
               _height,
               FSubFormat
              ) ;
end ;

procedure  TGIS_LayerPixel.ImportLayer(
  const _layer     : TGIS_LayerPixel ;
  const _extent    : TGIS_Extent ;
  const _cs        : TGIS_CSCoordinateSystem ;
  const _width     : Cardinal ;
  const _height    : Cardinal ;
  const _subformat : TGIS_LayerPixelSubFormat
) ;
const
  MAX_MEMORY_RESIDENT_SIZE = 3000*3000 ;
var
  rows              : Cardinal ;
  columns           : Cardinal ;
  i, j              : Integer ;
  dx, dy            : Double ;
  odx, ody          : Double ;
  iextent           : TGIS_Extent ;
  wextent           : TGIS_Extent ;
  layerAlphaBuffer  : TBytes ;
  sp                : Integer ;
  imp_as_grid       : Boolean ;
  ig                : Boolean ;

  bidx, mr, w, h    : Integer ;
  wGrid             : TGIS_GridArray ;
  wBitmap           : TGIS_Pixels    ;
  pix               : TGIS_Pixels ;
  grid              : TGIS_GridArray ;
  trcolor           : Integer ;
  or_cs             : TGIS_CSCoordinateSystem ;
  const
    MAX_CELL_WH = 1024 ;

  procedure set_transparent ;
    var
      ii : Integer ;
  begin
    for ii := low(pix) to high(pix) do
      if (pix[ii] and $00FFFFFF) = trcolor then
        pix[ii] := 0 ;
  end;

  procedure copy_to_file;
  var
    ii, kk      : Integer ;
    cw, ch    : Integer ;
    lw, lh    : Integer ;
    maxc,maxr : Integer ;
    ext       : TGIS_Extent {$IFDEF GIS_NORECORDS} = new TGIS_Extent{$ENDIF} ;
    sx, sy    : Double ;
  begin

    if not assigned(tempFileStream) then exit ;

    if FBitHeight <= MAX_CELL_WH then
      ch := FBitHeight
    else
      ch := MAX_CELL_WH ;

    if FBitWidth <= MAX_CELL_WH then
      cw := FBitWidth
    else
      cw := MAX_CELL_WH ;

    maxc := FBitWidth div cw ;
    lw := FBitWidth mod cw ;
    if lw > 0 then begin
      lw := 0 ;
      inc(maxc) ;
    end;
    maxr := FBitHeight div ch ;
    lh := FBitHeight mod ch ;
    if lh > 0 then begin
      inc(maxr) ;
    end;

    sx := (FExtent.XMax -FExtent.XMin)/FBitWidth  ;
    sy := (FExtent.YMax -FExtent.YMin)/FBitHeight ;

    if maxc = 0 then begin
      maxc := 1 ;
      cw := lw ;
    end;
    try
       if FIsGridImage then begin
        {$IFDEF OXYGENE}
          grid := new array of Single[ch] ;
          for ii := 0 to ch-1 do
            grid[ii] := new Single[cw] ;
        {$ELSE}
          SetLength( grid, ch, cw ) ;
        {$ENDIF}

        for ii := 0 to maxr -1 do begin
          ext.YMax := FExtent.YMax -ii*(ch*sy) ;
          ext.YMin := ext.YMax -ch*sy ;
          for kk := 0 to maxc -1 do begin
            ext.XMin := FExtent.XMin +kk*(cw*sx) ;
            ext.XMax := ext.XMin +cw*sx ;
            setNoDataTable( grid ) ;
            if FIsModified then
              getGridData(ext, grid) ;
            _layer.getGridData(ext, grid) ;
            PutGrid(ext, grid );
          end;
        end ;
      end
      else begin

        SetLength(pix, cw*ch) ;
        for ii := 0 to maxr -1 do begin
          ext.YMax := FExtent.YMax -ii*(ch*sy) ;
          ext.YMin := ext.YMax -ch*sy ;
          for kk := 0 to maxc -1 do begin
            ext.XMin := FExtent.XMin +kk*(cw*sx) ;
            ext.XMax := ext.XMin +cw*sx ;
            set_transparent ;
            getBitmapData(ext, pix, cw, ch) ;
            _layer.getBitmapData(ext, pix, cw, ch) ;
            putBitmapRawData( ext,
                              pix,
                              cw,
                              ch
                             ) ;

          end;
        end ;
      end;
    finally
       if FIsGridImage then
        grid := nil
      else
        pix := nil ;
    end ;
  end;

  // this step was added to refactor procedure body avoid
  // D5 C1141 internal error on command line compiler
  procedure do_step2 ;
  var
    k : Integer ;
  begin
    if assigned( _cs ) and _layer.CS.CanConvert( _cs ) then
      CS := _cs
    else if assigned(_layer.outCS) then
      CS := _layer.outCS
    else if assigned(_layer.Viewer) then
      CS := _layer.Viewer.Ref.CS
    else
      CS := _layer.CS ;

    isHistogram := False ;

    isInverted      := _layer.isInverted ;
    makeTransparent := _layer.makeTransparent ;
    makeTransparent := True ;

    isShadow        := _layer.isShadow ;
    realBitCount    := 24 ;

    for k := 0 to 255 do
      redTransp[k] := _layer.redTransp[k] ;

    for k := 0 to 255 do
      greenTransp[k] := _layer.greenTransp[k] ;

    for k := 0 to 255 do
      blueTransp[k] := _layer.blueTransp[k] ;

    for k := 0 to 255 do
      corRGB[k] := _layer.corRGB[k] ;

  end ;
begin
  if not assigned( _layer ) then exit ;


  if _subformat.Subformat = TGIS_PixelSubFormat.GRID then begin
    FIsNativeGridImage := True ;
    FIsGridImage := True ;
  end ;
  iextent := _TGIS_Extent(_extent) ;
  ig := _layer.FIsGridImage ;
  sp := _layer.CurrentPage ;
  dx := iextent.XMax - iextent.XMin ;
  _layer.IsGridImage := FIsGridImage ;
  if dx = 0 then
    exit ;
  dy := iextent.YMax - iextent.YMin ;
  if dy = 0 then
    exit ;

  odx := FExtent.XMax - FExtent.XMin ;
  if odx = 0 then
    exit ;
  ody := FExtent.YMax - FExtent.YMin ;
  if dy = 0 then
    exit ;


  if _width = 0 then begin
    if _height = 0 then begin
      if FBitWidth > 0 then
        columns := RoundS((FBitWidth*dx)/odx)
      else
        columns := RoundS(dx/_layer.FPixelSize.X) ;
    end
    else
      columns := RoundS(Abs(_height*(dx/dy)))

  end
  else
    columns := _width ;

  if columns = 0 then
    columns := 1 ;

  if _height = 0 then begin
    if FBitHeight > 0 then
      rows := RoundS(Abs((FBitHeight*dy)/ody))
    else
      rows := RoundS(Abs(columns*(dy/dx))) ;
  end
  else
    rows := _height ;

  if rows = 0 then
    rows := 1 ;

  isBuilt := True ;

  if assigned(oGrid) or assigned(oBitmap) then begin
    memoryResident := True ;
  end
  else begin
    { TODO : Think of a better way to secure against buffer allocation in memory
             and avoid infinite loop when ImportLayer is called from Lock method
             on big area exceeding MAX_MEMORY_RESIDENT_SIZE.
    }
    if IsStringEmpty( Path ) then
      memoryResident := True
    else
      memoryResident := False ;
  end ;

  internalTransparentColor := _layer.internalTransparentColor ;
  FSubFormat := _TGIS_LayerPixelSubFormat(_subformat) ;

  imp_as_grid := FIsGridImage ;
  if (FBandsCount = 0) and (not assigned(oBitmap))  then // Pure TGIS_LayerPxel
    if _layer.Interpretation <> TGIS_LayerPixelInterpretation.Pixel then
      imp_as_grid := True ;


  if memoryResident or assigned(tempFileStream) then begin
    isPartialTransparent :=  True ;
    defaultPartialTransparent := True ;

    if imp_as_grid and (_layer.FIsNativeGridImage or _layer.FIsGridImage ) then
    begin

      FNoDataValue := _layer.NoDataValue ;
      if _layer.FGridBand <> _layer.Params.Pixel.GridBand then begin
        _layer.setupParams ;
        FGridBand := 1;
        FMinZ := _layer.FMinZ ;
        FMaxZ := _layer.FMaxZ ;
        FMinThresholdZ := -GIS_MAX_SINGLE ;
        FMaxThresholdZ :=  GIS_MAX_SINGLE ;
        Params.Pixel.MinHeightThreshold := FMinThresholdZ ;
        Params.Pixel.MaxHeightThreshold := FMaxThresholdZ ;
      end ;
    // Transparency
      redTransp[0]    := BASE_TRANSPARENT_FLAG ;
      greenTransp[0]  := BASE_TRANSPARENT_FLAG ;
      blueTransp[0]   := BASE_TRANSPARENT_FLAG ;


      if not assigned(oGrid) then begin

        if assigned(tempFileStream) then begin
          wextent := GisCommonExtent(iextent, _layer.ProjectedExtent) ;
          columns := RoundS((iextent.XMax -iextent.XMin)/FPixelSize.X) ;
          rows    := RoundS((iextent.YMax -iextent.YMin)/FPixelSize.Y) ;
          copy_to_file ;
        end
        else begin
          wextent := iextent ;

          wGrid := InitializeGrid( rows, columns ) ;
          setNoDataTable(wGrid) ;
        end ;
      end
      else begin
        wGrid := oGrid ;
        wextent := FExtent ;
      end ;
      _layer.Alive ;
      _layer.importMode := True ;

      if not assigned(tempFileStream) then begin
        _layer.GetGrid(wextent, wGrid) ;
        if FNoDataValue <> GIS_GRID_NOVALUE then begin
          for h := low(wGrid) to high(wGrid) do
            for w := low(wGrid[h]) to high(wGrid[h]) do
              if wGrid[h][w] = GIS_GRID_NOVALUE then
                wGrid[h][w] := FNoDataValue ;
        end ;

        if fromBand > 0 then
          oBand := wGrid ;
      end ;
      if not assigned(oGrid) then
         oGrid := wGrid ;
      _layer.importMode := False ;
      FIsNativeGridImage := True ;
      Params.Pixel.GridBand := _layer.FGridBand ;
      FIsGridImage := True ;
      FBandsCount := 1 ;
    end
    else begin
      layerAlphaBuffer := _layer.alphaBuffer ;
      if assigned(tempFileStream) then begin
        wextent := GisCommonExtent(iextent, _layer.ProjectedExtent) ;
        columns := RoundS((wextent.XMax -wextent.XMin)/FPixelSize.X) ;
        rows    := RoundS((wextent.YMax -wextent.YMin)/FPixelSize.Y) ;
      end
      else
        wextent := iextent ;

      if not assigned(oBitmap) then begin
        if not assigned(tempFileStream) then
          SetLength(wBitmap, rows*columns )
      end
      else
        wBitmap := oBitmap ;

      FIsGridImage := False ;
      FIsNativeGridImage := False ;

      _layer.Alive ;
      _layer.importMode := True ;
      if assigned(wBitmap) then
        _layer.GetBitmap(wextent, wBitmap, columns, rows)
      else
        copy_to_file ;
      _layer.importMode := False ;
      FBandsCount := 4 ;


      if fromBand > 0 then begin
        bidx := fromBand -1 ;

        oBand := InitializeGrid( rows, columns) ;
        if _layer.bitsPerBand[bidx] <= 8 then begin
          mr := 0 ;
          for i := 0 to FBandsCount do begin
            if bandsMap[i] = bidx then begin
              mr := 8*i ;
              break ;
            end;
          end;

           for h := 0 to rows -1 do begin
             for w := 0 to columns -1 do
               oBand[h][w] := (wBitmap[Integer(columns)*h +w] shr mr) and $FF ;
           end;
         end
         else begin
          _layer.Params.Pixel.GridBand := fromBand ;
          _layer.FGridBand := fromBand ;
          _layer.asWritable := asWritable ;
          _layer.FIsGridImage := True ;
          _layer.Alive ;
          _layer.importMode := True ;
          _layer.GetGrid(iextent, oBand) ;
          _layer.importMode := False ;
          _layer.Params.Pixel.GridBand := 0 ;
          _layer.FGridBand := 0 ;
         _layer.FIsGridImage := False ;
          _layer.Alive ;
          wordDivider := _layer.wordDivider ;
          wordShift := _layer.wordShift ;
          for j := 0 to rows -1 do
            for i := 0 to columns -1 do
              if oBand[j][i] = NoDataValue then
                oBand[j][i] := 0 ;
         end;
      end;
      if not assigned(oBitmap) then
        oBitmap := wBitmap ;
    end ;
  end ;

  if FBitWidth <= 0 then begin
    FBitWidth   := columns  ;
    FCellWidth  := columns ;
    baseCellWidth  := columns ;
    intLineWidth := 3*columns ;
  end ;
  if FBitHeight <= 0 then begin
    FBitHeight  := rows ;
    FCellHeight := rows ;
    baseCellHeight := rows ;
  end ;
  FExtent := _TGIS_Extent(iextent) ;
  FProjectedExtent := _TGIS_Extent(iextent) ;
  FPixelSize := GisPoint(
                        ( FExtent.XMax - FExtent.XMin ) / FBitWidth,
                        ( FExtent.YMax - FExtent.YMin ) / FBitHeight
                      ) ;
  if (scaleX = 0) or (scaleY = 0 )then begin
    scaleX := FPixelSize.X ;
    scaleY := -FPixelSize.Y ;
  end ;
  FPagesCount := 1 ;
  FCurrentPage := 1 ;
  if FIsGridImage then begin
    Params.Assign(_layer.Params) ;
    FIsNativeGridImage := True ;
    if FMinZ >= FMaxZ then begin
      FMinZ := _layer.FMinZ ;
      FMaxZ := _layer.FMaxZ ;
    end
    else begin
      FMinZ := Min(FMinZ, _layer.FMinZ) ;
      FMaxZ := Max(FMaxZ, _layer.FMaxZ) ;
    end ;
    FMinThresholdZ := _layer.FMinThresholdZ ;
    FMaxThresholdZ := _layer.FMaxThresholdZ ;
    TGIS_ParamsSectionPixel(Params).Pixel.MinHeightThreshold := FMinThresholdZ ;
    TGIS_ParamsSectionPixel(Params).Pixel.MaxHeightThreshold := FMaxThresholdZ ;
    FBandsCount := 1 ;
    FGridBand :=  _layer.FGridBand  ;
    FAntialias := _layer.FAntialias ;
    Params.Pixel.GridBand := _layer.FGridBand ;
    Params.Pixel.GridNoValue := NoDataValue ;
  end ;
  FIsContrastEnhanced := _layer.FIsContrastEnhanced ;

  makeSomeCorrection := _layer.makeSomeCorrection ;
  isFromNet := _layer.isFromNet ;

  do_step2 ;
  or_cs := _layer.outCS ;
  _layer.outCS := _cs ;

  if (not memoryResident) and (not assigned(tempFileStream))  then
    importPixelData(_layer)
  else
    FIsModified := True ;

  _layer.outCS := or_cs ;
  _layer.FIsGridImage := ig ;



end ;

procedure TGIS_LayerPixel.ExportLayer(
  const _layer     : TGIS_LayerPixel ;
  const _extent    : TGIS_Extent
) ;
begin
  _layer.ImportLayer( self, _extent ) ;
end ;

procedure TGIS_LayerPixel.MergeLayer(
  const _layer     : TGIS_LayerPixel  ;
  const _extent    : TGIS_Extent
) ;
begin
  { TODO : Implement }
end ;

function  TGIS_LayerPixel.PutGrid(
  const _extent : TGIS_Extent ;
  const _grid   : TGIS_GridArray
) : Boolean ;
var
  wl        : TGIS_LayerPixel ;
  w, h      : Integer ;
  ow, oh    : Integer ;
  wgrid     : TGIS_GridArray ;

  procedure prepareMinMaxZ ;
  var
    i, k, m,  n: Integer ;
    ar : TGIS_SingleArray ;
  begin
    if assigned(_grid) then begin
      n := length(_grid) ;
      m := length(_grid[0]) ;
      for i := 0 to n - 1 do begin
        ar := TGIS_SingleArray(_grid[i]) ;
        for k := 0 to m - 1 do begin
          if (ar[k] <> FNoDataValue) and (ar[k] <> GIS_GRID_NOVALUE) then begin
            if ar[k] = 0 then
              ar[k] :=  ar[k] ;
            if ar[k] < FMinZ then
             FMinZ := ar[k] ;
            if ar[k] > FMaxZ then
              FMaxZ := ar[k] ;
          end ;
        end ;
      end ;
    end ;
  end ;

  procedure write2tmpfile ;
  var
    pos     : Int64 ;
    t, it   : Integer ;
    l, il   : Integer ;
    i       : Integer ;
    {$IFDEF OXYGENE}
     j      : Integer ;
     buf    : TBytes  ;
    {$ELSE}
      portion : Integer ;
    {$ENDIF}
  begin
    if not assigned( tempFileStream ) then exit ;

    if FExtent.YMax >= _extent.YMax then begin
      t := RoundS((FExtent.YMax - _extent.YMax)/-scaleY) ;
      it := 0 ;
    end
    else begin
      t := 0 ;
      it := RoundS((_extent.YMax - FExtent.YMax)/-scaleY) ; ;
    end;
    if _extent.XMin >= FExtent.XMin then begin
      l := RoundS((_extent.XMin -FExtent.XMin)/scaleX) ;
      il := 0 ;
    end
    else begin
      l := 0 ;
      il := RoundS((FExtent.XMin -_extent.XMin)/scaleX) ;
    end;
    if (ow +l) > FBitWidth then
      ow := FBitWidth -l ;
    if (oh +t) > FBitHeight then
      oh := FBitHeight -t ;
    if (ow > 0) and (oh > 0) then begin
      {$IFDEF OXYGENE}
      {$ELSE}
         portion := sizeOf(Single) * ow ;
      {$ENDIF}
      pos      := Int64(sizeOf(Single)) *( Int64(FBitWidth) * t + l) ;

      for i := it to it +oh-1 do begin
        tempFileStream.Position := pos ;
        {$IFDEF OXYGENE}
         for j := il to il +ow-1 do begin
           buf := BitConverter.GetBytes( wgrid[i][j] ) ;
           tempFileStream.Write( buf, length( buf ) ) ;
         end;
        {$ELSE}
         tempFileStream.Write( wgrid[i][il], portion ) ;
        {$ENDIF}
        pos := pos + FBitWidth * sizeOf(Single) ;
      end ;
    end;


  end;
begin
  Result := False ;

  if not isBuilt then
    exit ;

  if ( _extent.XMax <= _extent.XMin ) or
     ( _extent.YMax <= _extent.YMin )
  then
    exit ;

  if ( FExtent.XMin >= _extent.XMax ) or
     ( FExtent.XMax <= _extent.XMin ) or
     ( FExtent.YMin >= _extent.YMax ) or
     ( FExtent.YMax <= _extent.YMin )
  then
    exit ;

  if not assigned(_grid) then
    exit ;

  h := length(_grid) ;
  w := length(_grid[low(_grid)]) ;
  if (w = 0 ) or ( h = 0 ) then
    exit ;

  //Make work layer
  wl := TGIS_LayerPixel.Create ;
  try
    wl.scaleX := (_extent.XMax - _extent.XMin)/w ;
    ow := RoundS((_extent.XMax - _extent.XMin)/scaleX) ;

    wl.scaleY := (_extent.YMin - _extent.YMax)/h ;
    oh := RoundS((_extent.YMin - _extent.YMax)/scaleY) ;

    wl.oGrid := _grid ;
    wl.FIsGridImage := True ;
    wl.FAntialias := False ;

    wl.FExtent := _TGIS_Extent(_extent) ;
    wl.FProjectedExtent := _TGIS_Extent(_extent) ;
    wl.baseProjectedExtent := _TGIS_Extent(_extent) ;
    wl.FBitWidth  := w ;
    wl.FCellWidth  := w ;
    wl.FBandsCount := FBandsCount ;
    wl.baseCellWidth  := w ;
    wl.intLineWidth := 3*w ;
    wl.realBitCount  := 24;

    wl.FBitHeight := h ;
    wl.FCellHeight := h ;
    wl.baseCellHeight := h ;
    wl.realLineWidth := ((3* (w +1)) div 4)*4 ;
    wl.Viewer := Viewer ;

    if assigned(oGrid) then begin
      wl.GetGrid(FExtent, oGrid) ;
    end
    else
    begin
      if (ow <> w) or (oh <> h)  then begin
        wgrid := InitializeGrid( oh, ow ) ;
        wl.GetGrid(_extent, wgrid) ;
      end
      else
        wgrid := _grid ;
      write2tmpfile ;
      if wgrid <> _grid then
        wgrid := nil ;
    end;
  finally
    FreeObject(wl) ;
  end ;
  prepareMinMaxZ ;
  Result := True ;
end ;

function TGIS_LayerPixel.MustSave : Boolean ;
begin
  if not ( TGIS_LayerSubType.Persistent in SubType ) then begin
    Result := False ;
    exit ;
  end;

  if IsReadOnly then begin
    Result := False ;
    exit ;
  end;

  Result := inherited MustSave or FIsModified  ;
end ;

procedure TGIS_LayerPixel.SaveData ;
begin
  inherited ;
end ;

function TGIS_LayerPixel.lanczos3(_dval : Single) : Single ;
  function onSin(_dv: Single): Single ;
  begin
    if _dv <> 0 then begin
      _dv := _dv * Pi;
      Result := Sin(_dv) / _dv;
    end
    else
      Result := 1;
  end ;
begin
  if _dval < 0 then
    _dval := -_dval;

  if _dval < 3 then
    Result := onSin(_dval) * onSin(_dval / 3)
  else
    Result := 0;
end ;

function TGIS_LayerPixel.linear(_dval : Single) : Single ;
begin
  if _dval < 0 then
    _dval := -_dval;

  if (_dval < 1.0) then
    Result := 1.0 - _dval
  else
    Result := 0.0;
end ;

procedure TGIS_LayerPixel.ScaleGrid(
  const _srcGrid   : TGIS_GridArray ;
  const _dstGrid   : TGIS_GridArray ;
  const _lstart    : Integer ;
  const _tstart    : Integer
) ;
begin
  ScaleGrid(_srcGrid, _dstGrid, _lstart, _tstart, TGIS_ScalingFilter.Linear) ;
end ;

procedure TGIS_LayerPixel.ScaleGrid(
  const _srcGrid   : TGIS_GridArray ;
  const _dstGrid   : TGIS_GridArray ;
  const _lstart    : Integer ;
  const _tstart    : Integer ;
  const _filtering : TGIS_ScalingFilter
) ;
var
  scalex1,
  scaley1: Single;
  i, j,
  k, n: Integer;
  center: Single;
  weight: Single;
  left,
  right: Integer;
  wgrid: Array of Single;
  gridContributorList : TGridContributorList;
  pdestpixel_idx      : Integer;
  srcHeight,
  srcWidth,
  dstHeight,
  dstWidth: Integer;
  currLine: array of Single;

  sw : Single ;
  prun_idx : Integer ;

  cbrlst_len : Integer ;
  ngridval : Single ;
  cbr_width  : Integer ;
  cbr_nbr : Integer ;
  cbr_fact_x : Single ;
  cbr_fact_y : Single ;
  ff  : T_FilterFunc ;

  procedure simple_grid_scale ;
  var
    o_ii, o_kk : Integer ;
    ir, ic : Integer ;
  begin
    for o_ii := _tstart to dstHeight +_tstart -1 do begin
      ir := TruncS(o_ii/scaley1) ;
      if ir >= srcHeight then
        ir := srcHeight -1 ;
      for o_kk := _lstart to dstWidth +_lstart -1 do begin
         ic := TruncS(o_kk/scalex1) ;
         if ic >= srcWidth then
           ic := srcWidth -1 ;
         if  _srcGrid[ir][ic] <> GIS_GRID_NOVALUE then
           _dstGrid[o_ii -_tstart][o_kk -_lstart] := _srcGrid[ir][ic] ;
      end ;
    end ;
  end ;

  function apply_grid_contribs(
    const _num          : Integer ;
    const _contributors : TGridContributors
  ) : Single ;
  var
    h           : Integer;
    lvgrid      : Single ;
    tvweight    : Single ;
    nvweight    : Single ;
    pixel_idx   : Integer ;
  begin
    lvgrid   := 0;
    tvweight := 0 ;
    nvweight := 0 ;

    for h := 0 to _num - 1 do begin
      weight    := _contributors[h].weight;
      pixel_idx := _contributors[h].pixel_idx;

      if pixel_idx >= 0 then begin
        if currLine[pixel_idx] = GIS_GRID_NOVALUE then begin
          nvweight := nvweight +weight ;
        end
        else begin
          tvweight := tvweight +weight ;
          lvgrid := lvgrid +currLine[pixel_idx] * weight ;
        end ;
      end ;
    end ;

    if nvweight < tvweight then begin
      if tvweight = 0 then
        Result := lvgrid
      else
        Result := lvgrid / tvweight ;
    end
    else
      Result := GIS_GRID_NOVALUE ;
  end ;

begin
  if (not assigned(_srcGrid)) or (not assigned(_dstGrid)) then exit ;

  srcHeight := length(_srcGrid) ;
  srcWidth  := length(_srcGrid[0]) ;
  dstHeight := length(_dstGrid) ;
  dstWidth  := length(_dstGrid[0]) ;

  if (srcWidth  = 0) or (dstWidth  = 0) then exit ;
  if (srcHeight = 0) or (dstHeight = 0) then exit ;

  if  dstWidth <= 6 then
    sw := 1
  else
    sw := 3 ;

  try

    if scaleX <= 0 then
      scaleX := srcWidth/dstWidth ;
    scalex1 := scaleX ;
    scaley1 := scalex1*((srcWidth/dstWidth)/(srcHeight/dstHeight)) ;

    if not FAntialias then begin
      simple_grid_scale ;
      exit ;
    end ;

    cbrlst_len := dstWidth ;
    if dstWidth < dstHeight then
      cbrlst_len := dstHeight ;

    if srcHeight > srcWidth then
      cbr_width := srcHeight
    else
      cbr_width := srcWidth ;

    if cbr_width < cbrlst_len then
      cbr_width := cbrlst_len ;

    SetLength( currLine, cbr_width ) ;
    SetLength( wgrid, cbrlst_len*cbr_width ) ;
    SetLength( gridContributorList, cbrlst_len ) ;

    {$IFDEF GIS_NORECORDS}
      for i := 0 to cbrlst_len - 1 do
        gridContributorList[i] := new TGridContributorEntry ;
    {$ENDIF}

    if scalex1 < 1.0 then
      cbr_fact_x := sw / scalex1
    else
      cbr_fact_x := sw ;

    if scaley1 < 1.0 then
      cbr_fact_y := sw / scaley1
    else
      cbr_fact_y := sw ;

    if cbr_fact_x > cbr_fact_y then
      cbr_nbr  := TruncS(2 * cbr_fact_x + 1)
    else
      cbr_nbr  := TruncS(2 * cbr_fact_y + 1) ;

    if _filtering = TGIS_ScalingFilter.Lanczos3 then
      {$IFDEF OXYGENE}
        ff := @lanczos3
      {$ELSE}
        ff := lanczos3
      {$ENDIF}
    else
      {$IFDEF OXYGENE}
        ff := @linear ;
      {$ELSE}
        ff := linear ;
      {$ENDIF}

    for i := _lstart to dstWidth +_lstart - 1 do begin
      gridContributorList[i -_lstart].num := 0;
      SetLength(gridContributorList[i -_lstart].contributors, cbr_nbr +3) ;

      {$IFDEF GIS_NORECORDS}
        for k := 0 to cbr_nbr +3-1 do
          gridContributorList[i -_lstart].contributors[k] := new TGridContributor ;
      {$ENDIF}

      center := i / scalex1;
      left   := FloorS(center - cbr_fact_x) ;
      right  := CeilS(center + cbr_fact_x) ;

      for j := left to right do begin
        if scalex1 < 1.0 then
          weight :=   ff( (center - j) * scalex1 ) * scalex1 * 256
        else
          weight := ff(center - j) * 256;

        if weight <> 0 then begin
          if j < 0 then
            n := -j
          else if j >= srcWidth then
            n := srcWidth - j + srcWidth - 1
          else
            n := j;
          k := gridContributorList[i -_lstart].num;
          inc(gridContributorList[i -_lstart].num) ;

          gridContributorList[i -_lstart].contributors[k].pixel_idx := n;
          gridContributorList[i -_lstart].contributors[k].weight    := weight;
        end ;
      end ;
    end ;

    for k := 0 to srcHeight - 1 do begin
      for i := 0 to srcWidth - 1 do
        currLine[i] := _srcGrid[k][i] ; ;

      pdestpixel_idx := k*dstWidth ;
      for i := 0 to dstWidth - 1 do begin
        sw := apply_grid_contribs(gridContributorList[i].num, gridContributorList[i].contributors) ;
        wgrid[pdestpixel_idx +i] := sw ;
      end ;
    end ;

    for i := _tstart to dstHeight + _tstart - 1 do begin
      gridContributorList[i -_tstart].num := 0;
      SetLength( gridContributorList[i -_tstart].contributors, cbr_nbr +3 ) ;

      {$IFDEF GIS_NORECORDS}
        for k := 0 to cbr_nbr +3-1 do
          gridContributorList[i -_tstart].contributors[k] := new TGridContributor ;
      {$ENDIF}

      center := i / scaley1;
      left   := FloorS(center - cbr_fact_y) ;
      right  := CeilS(center + cbr_fact_y) ;

      for j := left to right do begin
        if scaley1 < 1.0 then
          weight := ff((center - j) * scaley1) * scaley1 * 256
        else
          weight := ff(center - j) * 256;

        if weight <> 0 then begin
          if j < 0 then
            n := -j
          else if j >= srcHeight then
            n := srcHeight - j + srcHeight - 1
          else
            n := j;
          k := gridContributorList[i -_tstart].num;
          inc(gridContributorList[i -_tstart].num) ;
          gridContributorList[i -_tstart].contributors[k].pixel_idx := n;
          gridContributorList[i -_tstart].contributors[k].weight    := weight;
        end ;
      end ;
    end ;

    for k := 0 to dstWidth -1 do begin
      prun_idx := 0 ;

      for i := 0 to srcHeight -1 do
        currLine[i] := wgrid[k +i*dstWidth] ;

      for i := 0 to dstHeight -1 do begin
        ngridval := apply_grid_contribs(gridContributorList[i].num,
                                      gridContributorList[i].contributors
                                     ) ;
        if ngridval <> GIS_GRID_NOVALUE then
          _dstGrid[i][k] :=  ngridval ;
      end ;
    end ;

    for i := cbrlst_len - 1 downto 0 do
      SetLength(gridContributorList[i].contributors, 0) ;
    SetLength(gridContributorList, 0) ;
  finally
    wgrid    := nil ;
    currLine := nil ;
  end ;
end ;

procedure TGIS_LayerPixel.SetCurrentFileScale(
  const _dwidth : Double ;
  const _swidth : Double
) ;
begin
  setFileScale( _dwidth, _swidth ) ;
end ;

constructor TGIS_LayerPixelLock.Create ;
begin
  FParent          := nil ;
  FGrid            := nil ;
  FBitmap          := nil ;
  FBand            := nil ;
  FBandNo          := 0 ;
  FExtent          := GisExtent( 0, 0, 0, 0 ) ;
  FBounds          := Rect( 0, 0, 0, 0) ;
  FCS              := nil ;
  FPixelSize       := GisPoint( 0, 0 ) ;
  FWritable        := False ;
  FIsTiled         := False  ;
  {$IFDEF GIS_NORECORDS}
    FTileInfo      := new TGIS_Tile() ;
  {$ENDIF}
  FTileInfo.Level  := 0 ;
  FTileInfo.Column := 0 ;
  FTileInfo.Row    := 0 ;
  pixWidth         := 0 ;
  pixHeight        := 0 ;
end ;

constructor TGIS_LayerPixelLock.Create(
  const _parent : TGIS_LayerPixel
) ;
begin
  Create ;
  FParent := _parent ;
end ;

function TGIS_LayerPixelLock.BitmapPos(
  const _x : Integer ;
  const _y : Integer
) : Integer ;
var
  x : Integer ;
  y : Integer ;
begin
  Result := 0 ;
  if (_x < 0) or (_y < 0) then
    exit ;

  if not assigned(FBitmap) then
    exit ;
  x := _x ;
  y := _y ;
  if x >= pixWidth then
    x := x -FBounds.Left ;
  if y >= pixHeight then
    y := y - FBounds.Top ;
  Result := y*(pixWidth) +x ;
end ;

function TGIS_LayerPixelLock.MapToRaster(
  const _ptg : TGIS_Point  ;
  const _cs  : TGIS_CSCoordinateSystem
) : TPoint ;
var
  ptg : TGIS_Point ;
begin
  if assigned( _cs ) and ( _cs.EPSG > 0 )
     and
     assigned( FCS ) and ( FCS.EPSG > 0 )
  then
    ptg := _cs.ToCS( FCS, _ptg )
  else
    ptg := _TGIS_Point(_ptg) ;

  Result := Point(
    floor_pixel( ptg.X - FExtent.XMin, FPixelSize.X ),
    ceil_pixel( FExtent.YMax - ptg.Y, FPixelSize.Y ) - 1
  ) ;
end ;

function TGIS_LayerPixelLock.RasterToMap(
  const _pt : TPoint  ;
  const _cs : TGIS_CSCoordinateSystem
) : TGIS_Point ;
var
  res : TGIS_Point ;
begin
  res := GisPoint(
           FExtent.XMin + ( _pt.X + 0.5 ) * FPixelSize.X,
           FExtent.YMax - ( _pt.Y + 0.5 ) * FPixelSize.Y
         ) ;

  if assigned( _cs ) and ( _cs.EPSG > 0 )
     and
     assigned( FCS ) and ( FCS.EPSG > 0 )
  then
    res := _cs.FromCS( FCS, res ) ;

  Result := res ;
end ;

function TGIS_LayerPixelLock.MapToRasterRect(
  const _ext : TGIS_Extent ;
  const _cs  : TGIS_CSCoordinateSystem
) : TRect ;
var
  ext : TGIS_Extent ;
begin
  if assigned( _cs ) and ( _cs.EPSG > 0 )
     and
     assigned( FCS ) and ( FCS.EPSG > 0 )
  then
    ext := _cs.ExtentToCS( FCS, _ext )
  else
    ext := _TGIS_Extent(_ext) ;

  // avoid exceeding the lock bounds
  ext := GisCommonExtent( ext, FExtent ) ;

  Result := Rect(
    floor_pixel( ext.XMin     - FExtent.XMin, FPixelSize.X ),
    floor_pixel( FExtent.YMax - ext.YMax    , FPixelSize.Y ),
    ceil_pixel ( ext.XMax     - FExtent.XMin, FPixelSize.X ) - 1,
    ceil_pixel ( FExtent.YMax - ext.YMin    , FPixelSize.Y ) - 1
  ) ;
end ;

function TGIS_LayerPixelLock.RasterToMapRect(
  const _rct : TRect ;
  const _cs  : TGIS_CSCoordinateSystem
) : TGIS_Extent ;
var
  res : TGIS_Extent ;
begin
  res := GisExtent(
           FExtent.XMin + _rct.Left   * FPixelSize.X,
           FExtent.YMax - _rct.Bottom * FPixelSize.Y,
           FExtent.XMin + _rct.Right  * FPixelSize.X,
           FExtent.YMax - _rct.Top    * FPixelSize.Y
         ) ;

  if assigned( _cs ) and ( _cs.EPSG > 0 )
     and
     assigned( FCS ) and ( FCS.EPSG > 0 )
  then
    res := _cs.ExtentFromCS( FCS, res ) ;

  Result := res ;
end ;

{$IFDEF GIS_XDK}
  procedure TGIS_LayerPixelLock.AssignBuffers(
    const _grid      : TGIS_GridArray ;
    const _bitmap    : TGIS_Pixels    ;
    const _band      : TGIS_GridArray
  ) ;
  begin
    FGrid      := _grid      ;
    FBitmap    := _bitmap    ;
    FBand      := _band      ;
  end ;
{$ENDIF}

{$IFDEF GIS_EDITOR}
  procedure TGIS_LayerPixelLock.GetInternal(
    var   _grid      : TGIS_GridArray    ;
    var   _bitmap    : TGIS_Pixels       ;
    var   _band      : TGIS_GridArray    ;
    var   _bandNo    : Integer           ;
    var   _extent    : TGIS_Extent       ;
    var   _bounds    : TRect             ;
    var   _cs        : TGIS_CSCoordinateSystem ;
    var   _pixelSize : TGIS_Point        ;
    var   _writable  : Boolean           ;
    var   _isTiled   : Boolean           ;
    var   _tileInfo  : TGIS_Tile         ;
    var   _pixWidth  : Integer           ;
    var   _pixHeight : Integer
  ) ;
  begin
    _grid      := FGrid      ;
    _bitmap    := FBitmap    ;
    _band      := FBand      ;
    _bandNo    := FBandNo    ;
    _extent    := FExtent    ;
    _bounds    := FBounds    ;
    _cs        := FCS        ;
    _pixelSize := FPixelSize ;
    _writable  := FWritable  ;
    _isTiled   := FIsTiled   ;
    _tileInfo  := FTileInfo  ;
    _pixWidth  := pixWidth   ;
    _pixHeight := pixHeight  ;
  end ;

  procedure TGIS_LayerPixelLock.SetInternal(
    const _grid      : TGIS_GridArray    ;
    const _bitmap    : TGIS_Pixels       ;
    const _band      : TGIS_GridArray    ;
    const _bandNo    : Integer           ;
    const _extent    : TGIS_Extent       ;
    const _bounds    : TRect             ;
    const _cs        : TGIS_CSCoordinateSystem ;
    const _pixelSize : TGIS_Point        ;
    const _writable  : Boolean           ;
    const _isTiled   : Boolean           ;
    const _tileInfo  : TGIS_Tile         ;
    const _pixWidth  : Integer           ;
    const _pixHeight : Integer
  ) ;
  begin
    FGrid      := _grid      ;
    FBitmap    := _bitmap    ;
    FBand      := _band      ;
    FBandNo    := _bandNo    ;
    FExtent    := _extent    ;
    FBounds    := _bounds    ;
    FCS        := _cs        ;
    FPixelSize := _pixelSize ;
    FWritable  := _writable  ;
    FIsTiled   := _isTiled   ;
    FTileInfo  := _tileInfo  ;
    pixWidth   := _pixWidth  ;
    pixHeight  := _pixHeight ;
  end ;
{$ENDIF}


procedure TGIS_LayerPixel.Build(
  const _path   : String         ;
  const _cs     : TGIS_CSCoordinateSystem  ;
  const _ext    : TGIS_Extent    ;
  const _width  : Integer        ;
  const _height : Integer
) ;
begin
  Build( _path,
         FIsNativeGridImage,
         _cs,
         _ext,
         _width,
         _height,
         DefaultSubFormat
       ) ;
end ;

procedure TGIS_LayerPixel.Build(
  const _grid   : Boolean ;
  const _cs     : TGIS_CSCoordinateSystem ;
  const _ext    : TGIS_Extent ;
  const _width  : Integer ;
  const _height : Integer
) ;
begin
  Build(
    '',
    FIsNativeGridImage or _grid,
    _cs,
    _ext,
    _width,
    _height,
    DefaultSubFormat
  ) ;
end ;

procedure TGIS_LayerPixel.Build(
  const _path      : String ;
  const _grid      : Boolean ;
  const _cs        : TGIS_CSCoordinateSystem  ;
  const _ext       : TGIS_Extent ;
  const _pixelsize : Double ;
  const _subformat : TGIS_LayerPixelSubFormat
) ;
const
  LOCAL_PARAM_PIXELSIZE = '_pixelsize' ;
var
  width   : Integer ;
  height  : Integer ;
  new_ext : TGIS_Extent ;
begin
  if _pixelsize <= 0.0 then begin
    raise EGIS_Exception.Create(
      _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_PIXELSIZE, 0
    ) ;
  end ;

  // ceil because we want extent cover with little excess
  width := CeilS( ( _ext.XMax - _ext.XMin ) / _pixelsize ) ;
  height := CeilS( ( _ext.YMax - _ext.YMin ) / _pixelsize ) ;

  // ensure quadratic cell shape
  new_ext := GisExtent(
    _ext.XMin,
    _ext.YMin,
    _ext.XMin + width * _pixelsize,
    _ext.YMin + height * _pixelsize ) ;

  Build(
    _path,
    _grid,
    _cs,
    new_ext,
    width,
    height,
    DefaultSubFormat
  ) ;
end ;

procedure TGIS_LayerPixel.Build(
  const _path   : String ;
  const _cs     : TGIS_CSCoordinateSystem ;
  const _ext    : TGIS_Extent ;
  const _pixelsize : Double
) ;
begin
  Build(
    _path,
    FIsNativeGridImage,
    _cs,
    _ext,
    _pixelsize,
    DefaultSubFormat
  ) ;
end ;

procedure TGIS_LayerPixel.Build(
  const _grid   : Boolean ;
  const _cs     : TGIS_CSCoordinateSystem ;
  const _ext    : TGIS_Extent ;
  const _pixelsize : Double
) ;
begin
  Build(
    '',
    FIsNativeGridImage or _grid,
    _cs,
    _ext,
    _pixelsize,
    DefaultSubFormat
  ) ;
end ;

procedure TGIS_LayerPixel.Build(
  const _path      : String ;
  const _grid      : Boolean ;
  const _cs        : TGIS_CSCoordinateSystem ;
  const _ext       : TGIS_Extent ;
  const _width     : Integer ;
  const _height    : Integer ;
  const _subformat : TGIS_LayerPixelSubFormat
) ;
var
  i      : Integer ;
  width  : Int64 ;
  height : Int64 ;
  dx, dy : Double ;
  ext    : TGIS_Extent ;
  do_temp : Boolean ;
  memory_treshold : Integer ;
  {$IFDEF OXYGENE}
    buf  : TBytes  ;
    ndbb : TBytes  ;
    k    : Integer ;
  {$ELSE}
    ndb  : Array of Single ;
  {$ENDIF}
begin
  if not IsStringEmpty( _path ) then begin
    // try to recognize format and build layer based on format
    // and set internal structure
      Path := _path ;
  end  ;

  do_temp := False ;
  width := _width ;
  height := _height ;
  ext := _ext ;
  if (width <= 0) and (height <= 0) then
    width := 512 ;

  dx := ext.XMax - ext.XMin ;
  if dx = 0 then
    exit ;
  if dx < 0 then begin
    dx := -dx ;
    ext.XMin := ext.XMax ;
    ext.XMax := ext.XMin +dx ;
  end;

  dy := ext.YMax - ext.YMin ;
  if dy = 0 then
    exit ;
  if dy < 0 then begin
    dy := -dy ;
    ext.YMin := ext.YMax ;
    ext.YMax := ext.YMin +dy ;
  end;

  if width <= 0 then begin
    width := RoundS((dx * height) / dy) ;
  end;
  if height <= 0 then begin
    height := RoundS((dy * width) / dx) ;
  end;

  // build fully in memory layer
  FIsGridImage := _grid ;
  FIsNativeGridImage := _grid ;
  isGrayScaleImage := _subformat.GrayScale ;
  Params.Pixel.GrayScale := isGrayScaleImage ;
  isBuilt := True ;

  memory_treshold := GisMetadataAsInteger(
                       METADATA_MEMORYLAYERLIMIT,
                       MEMORY_LAYER_LIMIT
                     ) * 1024 * 1024 div 4 ; // in MB
  if FIsGridImage then begin
    FNoDataValue := Params.Pixel.GridNoValue ;
    if (height * width) <= memory_treshold then begin
      oGrid := InitializeGrid( height, width ) ;
      setNoDataTable(oGrid) ;
    end
    else
    begin
      do_temp := True ;
    end;
    FSubFormat := TGIS_LayerPixelSubFormat.Create(
                    TGIS_PixelFormat.Custom,
                    False,
                    TGIS_PixelSubFormat.GRID,
                    TGIS_CompressionType.None,
                    0
                  ) ;
  end
  else begin
    if (height * width) <= memory_treshold then begin
      //this interface should be added
      if (not assigned(oBitmap)) or
         ((width*height) <> (FBitWidth*FBitHeight))
      then begin
        SetLength(oBitmap, width*height ) ;
      end;
    end
    else begin
      do_temp := True ;
    end;
    FSubFormat := _TGIS_LayerPixelSubFormat(_subformat) ;
  end ;

  isPartialTransparent := True ;
  defaultPartialTransparent := True ;
  FCS     := _cs     ;
  FExtent := _TGIS_Extent(ext)    ;
  FProjectedExtent := _TGIS_Extent(ext)    ;

  FBitWidth  := width  ;
  FBitHeight := height ;

  FCellWidth  := width  ;
  FCellHeight := height ;

  baseCellWidth  := width  ;
  baseCellHeight := height ;

  FPixelSize := GisPoint(
                  ( FExtent.XMax - FExtent.XMin ) / FBitWidth,
                  ( FExtent.YMax - FExtent.YMin ) / FBitHeight
                ) ;
  scaleX :=  FPixelSize.X ;
  scaleY := -FPixelSize.Y ;

  FIsModified := True ;

  case FSubFormat.PixelFormat of
    TGIS_PixelFormat.Bit1   : FBandsCount := 1 ;
    TGIS_PixelFormat.Bit4   : FBandsCount := 1 ;
    TGIS_PixelFormat.Bit8   : FBandsCount := 1 ;
    TGIS_PixelFormat.RGB    : FBandsCount := 3 ;
    TGIS_PixelFormat.ARGB   : FBandsCount := 4 ;
    TGIS_PixelFormat.Custom : FBandsCount := 1 ;
  end ;

  if do_temp then begin
    if FileExists(tempFileName) then
      DeleteFile(tempFileName) ;
    tempFileName := GetTempFileName ;
    tempFileStream := TGIS_FileStream.Create( tempFileName, fmCreate ) ;
    try
      if FIsGridImage then begin
        tempFileStream.Position := 0 ;
        {$IFDEF OXYGENE}
          buf := BitConverter.GetBytes(Single(GIS_GRID_NOVALUE)) ;
          SetLength(ndbb, width*sizeOf(Single)) ;
          for i := 0 to width -1 do
            for k := low(buf) to high(buf) do
              ndbb[i*sizeOf(Single) +k ] := buf[k] ;
        {$ELSE}
          SetLength(ndb, width) ;
          for i := low(ndb) to high(ndb) do
            ndb[i] := GIS_GRID_NOVALUE ;
        {$ENDIF}

        for i := 0 to height -1 do
          {$IFDEF OXYGENE}
            tempFileStream.Write(ndbb, width*sizeOf(Single)) ;
          {$ELSE}
            tempFileStream.Write(ndb[0], width*sizeOf(Single)) ;
          {$ENDIF}
        {$IFDEF OXYGENE}
          SetLength(ndbb, 0) ;
        {$ELSE}
          SetLength(ndb, 0) ;
        {$ENDIF}
      end
      else //Bitmap
        tempFileStream.Size := Int64(FBitWidth) * FBitHeight * 4 ;

    except
      // do nothing
    end ;
  end ;
end;

function TGIS_LayerPixel.LockEstimate(
  const _ext : TGIS_Extent ;
  const _cs  : TGIS_CSCoordinateSystem ;
  const _pixelsize : Double
) : Integer ;
var
  pixelsize : Double ;
  rct       : TRect                     ;
  w, h      : Integer                   ;
  lps       : Double                    ;
  lck       : TGIS_LayerPixelLock       ;
begin
  Result := 0 ;
  pixelsize := _pixelsize ;
  if pixelsize = 0 then
    pixelsize := (FExtent.XMax -FExtent.XMin)/BitWidth ;
  if (not assigned(oBitmap)) and (not assigned(oGrid)) then begin
    if not  assigned(oLockListW) then begin
      w := RoundS((_ext.XMax -_ext.XMin)/pixelsize) ;
      h := RoundS((_ext.YMax -_ext.YMin)/pixelsize) ;
      Result := w*h ;
    end
    else begin
      if self.findProperPixelLock(_ext, _cs, pixelsize, 0, True) = -1 then begin
        w := RoundS((_ext.XMax -_ext.XMin)/pixelsize) ;
        h := RoundS((_ext.YMax -_ext.YMin)/pixelsize) ;
        Result := w*h ;
      end ;
    end ;
    exit ;
  end
  else begin
    lps := ( FExtent.XMax - FExtent.XMin )
                       / FBitWidth ;
    if (not IsTiled) and (Abs(lps -pixelsize) < 1e-10) then begin
      Result := - FBitWidth*FBitHeight ;
    end
    else
    begin
      lck := TGIS_LayerPixelLock.Create( Self ) ;
      try
        lck.FExtent    := _TGIS_Extent(_ext) ;
        lck.FPixelSize := GisPoint(_pixelsize, _pixelsize) ;
        lck.FCS        := CS      ;
        rct := lck.MapToRasterRect(_ext, CS) ;
        Result := rct.Width*rct.Height ;
        exit ;
      finally
        FreeObject( lck ) ;
      end;
    end ;
  end ;
end ;

function TGIS_LayerPixel.LockPixels(
  const _rct   : TRect ;
  const _writable  : Boolean
) : TGIS_LayerPixelLock ;
begin
  Result := LockPixels( _rct, 0, _writable ) ;
end ;

function TGIS_LayerPixel.LockPixels(
  const _rct      : TRect ;
  const _band     : Integer ;
  const _writable : Boolean
) : TGIS_LayerPixelLock ;
var
  ext : TGIS_Extent ;
begin
  ext := RasterToMapRect( _rct, CS ) ;
  Result := LockPixels( ext, CS, 0, 0, _writable ) ;
end ;

function TGIS_LayerPixel.LockPixels(
  const _ext      : TGIS_Extent ;
  const _cs       : TGIS_CSCoordinateSystem ;
  const _writable : Boolean
) : TGIS_LayerPixelLock ;
begin
  Result := LockPixels(_ext, _cs, 0, 0, _writable) ;
end ;


function TGIS_LayerPixel.LockPixels(
  const _ext      : TGIS_Extent ;
  const _cs       : TGIS_CSCoordinateSystem;
  const _band     : Integer ;
  const _writable : Boolean
) : TGIS_LayerPixelLock ;
begin
  Result := LockPixels(_ext, _cs, 0, _band, _writable) ;
end ;

function TGIS_LayerPixel.LockPixels(
  const _ext       : TGIS_Extent ;
  const _cs        : TGIS_CSCoordinateSystem ;
  const _pixelsize : Double ;
  const _writable  : Boolean
) : TGIS_LayerPixelLock ;
begin
  Result := LockPixels(_ext, _cs, _pixelsize, 0, _writable) ;
end ;

function TGIS_LayerPixel.LockPixels(
  const _ext       : TGIS_Extent ;
  const _cs        : TGIS_CSCoordinateSystem ;
  const _pixelsize : Double ;
  const _band      : Integer ;
  const _writable  : Boolean
) : TGIS_LayerPixelLock ;
var
  ext         : TGIS_Extent ;
  rct         : TRect ;
  w, h        : Integer ;
  lps         : Double ;
  pixelsize   : Double ;
  pixelsizex  : Double ;
  opixelsizex : Double ;
  pixelsizey  : Double ;
  r, b        : Integer ;
  dif         : Double ;
  bready      : Boolean ;
  bndscount   : Integer ;
  ogridband   : Integer ;
  workext     : TGIS_Extent ;
  ig          : Boolean ;
  trans       : T_TransClass ;
  orgCS       : TGIS_CSCoordinateSystem ;
  slp         : TGIS_LayerPixel ;
  lplist      : TObjectList<TGIS_LayerPixel> ;
  genlayer    : Boolean ;
begin
  Result := nil ;

  if (_band > FBandsCount) or (_band < 0) then
    exit ;

  slp := nil ;
  lplist := nil ;
  ig := FIsGridImage ;
  ogridband := Params.Pixel.GridBand ;
  orgCS := outCS ;
  try
    genlayer := False ;

    if assigned(_cs) then
      outCS := _cs
    else
      outCS := self.CS ;
    if _writable then
      lplist := oLockListW
    else
      lplist := oLockListR ;
    forViewer := False ;

    if assigned( self.CS ) then begin
      if  outCS.EPSG <> self.CS.EPSG then begin
        trans := T_TransClass.Create( Self ) ;
        workext := trans.projectedExt( FExtent ) ;
        FreeObject( trans ) ;
      end
      else
        workext := FExtent ;
    end
    else
      workext := FExtent ;

    ext := GisCommonExtent(_ext , workext );
    if ( ext.XMin = ext.XMax ) or ( ext.YMin = ext.YMax ) then begin
      if ( _ext.XMin <> _ext.XMax ) and ( _ext.YMin <> ext.YMax ) then begin
        Result := TGIS_LayerPixelLock.Create( Self ) ;
        exit ;
      end;
    end;

    if ( _band > 0 ) and ( _band <= FBandsCount ) then
      fromBand := _band ;

    asWritable := _writable ;

    if (Params.Pixel.GridBand > 0) or (_band > 0) then begin
      IsGridImage := True ;
      if _band > 0 then begin
        if Params.Pixel.GridBand <> _band then begin
          Params.Pixel.GridBand := _band ;
          setupParams ;
        end;
      end;
    end;

    Result := TGIS_LayerPixelLock.Create( Self ) ;

    pixelsize := _pixelsize ;
    if pixelsize = 0 then
      pixelsize := (workext.XMax -workext.XMin)/BitWidth ;
    pixelsizex := pixelsize ;
    opixelsizex := (workext.XMax -workext.XMin)/BitWidth  ;
    pixelsizey := (workext.YMax -workext.YMin)/BitHeight ;
    if pixelsizex <> opixelsizex then begin
      pixelsizey := pixelsizey*(pixelsize/opixelsizex) ;
      genlayer := True ;
    end;

    ext.XMin := RoundS(
      ( ext.XMin - workext.XMin ) / pixelsizex
    ) * pixelsizex + workext.XMin ;
    ext.XMax := RoundS(
      ( ext.XMax - workext.XMin ) / pixelsizex
    ) * pixelsizex + workext.XMin ;

    if ext.XMax > workext.XMax then
      ext.XMax := workext.XMax ;
    if GisIsSameValue( ext.XMin, ext.XMax ) then
      ext.XMax := ext.XMax + pixelsizex ;

    ext.YMin := RoundS(
      ( ext.YMin - workext.YMin ) / pixelsizey
    ) * pixelsizey + workext.YMin ;
    ext.YMax := RoundS(
      ( ext.YMax - workext.YMin ) / pixelsizey
    ) * pixelsizey + workext.YMin ;

    if ext.YMax > workext.YMax then
      ext.YMax := workext.YMax ;
    if GisIsSameValue( ext.YMin, ext.YMax ) then
      ext.YMax := ext.YMax + pixelsizex ;

    if (not assigned(oBitmap)) and (not assigned(oGrid)) then
      genlayer := True ;

    if genlayer then begin
      w := RoundS( (ext.XMax -ext.XMin) / pixelsizex ) ;
      h := RoundS( (ext.YMax -ext.YMin) / pixelsizey ) ;

      r := self.findProperPixelLock(ext, CS, pixelsizex, fromBand, _writable) ;
      if r >= 0  then begin
        slp := lplist.Items[r] ;
        w := slp.BitWidth ;
        h := slp.BitHeight;
      end
      else begin
        slp := TGIS_LayerPixel.Create ;
        slp.fromBand := fromBand ;
        slp.asWritable := asWritable ;
        slp.FIsGridImage := FIsGridImage ;
        if not IsGridImage then
          slp.FBandsCount := 4
        else
          slp.FBandsCount := 1 ;

        bndscount := FBandsCount  ;
        slp.ImportLayer(self, ext, outCS, w, h) ;
        FBandsCount := bndscount ; // restore bouind count
        if not assigned(lplist) then
          lplist := TObjectList< TGIS_LayerPixel >.Create ;
        lplist.Add(slp) ;
      end;

      Result.pixWidth  := w ;
      Result.pixHeight := h ;
    end
    else begin //?

    end ;

    lps := ( workext.XMax - workext.XMin ) / FBitWidth ;
    dif := pixelsize / FBitWidth ;

    bready := False ;
    if fromBand > 0 then
      bready := bready or assigned( oBand )
    else
    if FIsGridImage then
      bready := bready or assigned( oGrid )
    else
      bready := bready or (assigned( oBitmap ) and (fromBand = 0));

    if bready and (not IsTiled) and (Abs(lps -pixelsize) < dif) then begin
      Result.pixWidth  := FBitWidth ;
      Result.pixHeight := FBitHeight ;
    end
    else
    begin
      if not assigned(slp) then begin
        slp := TGIS_LayerPixel.Create ;
        slp.fromBand := fromBand ;
        slp.asWritable := asWritable ;
        slp.FIsGridImage := FIsGridImage ;
        if not IsGridImage then
          slp.FBandsCount := 4
        else
          slp.FBandsCount := 1 ;
        w := RoundS((ext.XMax -ext.XMin)/pixelsizex) ;
        h := RoundS((ext.YMax -ext.YMin)/pixelsizey) ;

        if w = 0 then begin
          ext.XMin := FloorS( ext.XMin/pixelsize)*pixelsizex ;
          ext.XMax := ext.XMin +pixelsizex ;
          w := 1 ;
        end ;
        if h = 0 then begin
          ext.YMin := FloorS( ext.YMin/pixelsizey)*pixelsizey ;
          ext.YMax := ext.YMin +pixelsizey ;
          h := 1 ;
        end ;

        bndscount := FBandsCount  ;
        slp.ImportLayer(self, ext, CS, w, h) ;
        FBandsCount := bndscount ; // restore bound count
        if not assigned(lplist) then
          lplist := TObjectList< TGIS_LayerPixel >.Create ;
        lplist.Add(slp) ;
        Result.pixWidth  := w ;
        Result.pixHeight := h ;
      end;
    end ;
    if not assigned(slp) then begin
      if _writable or bready then
        slp := self
      else begin
        slp := TGIS_LayerPixel.Create ;
        slp.fromBand := fromBand ;
        slp.asWritable := asWritable ;
        slp.FIsGridImage := FIsGridImage ;
        if not IsGridImage then
          slp.FBandsCount := 4
        else
          slp.FBandsCount := 1 ;
        w := RoundS((ext.XMax -ext.XMin)/pixelsizex) ;
        h := RoundS((ext.YMax -ext.YMin)/pixelsizey) ;

        if w = 0 then begin
          ext.XMin := FloorS( ext.XMin/pixelsize)*pixelsizex ;
          ext.XMax := ext.XMin +pixelsizex ;
          w := 1 ;
        end ;
        if h = 0 then begin
          ext.YMin := FloorS( ext.YMin/pixelsizey)*pixelsizey ;
          ext.YMax := ext.YMin +pixelsizey ;
          h := 1 ;
        end ;

        bndscount := FBandsCount  ;
        slp.ImportLayer(self, ext, CS, w, h) ;
        FBandsCount := bndscount ; // restore bound count
        if not assigned(lplist) then
          lplist := TObjectList< TGIS_LayerPixel >.Create ;
        lplist.Add(slp) ;
        Result.pixWidth  := w ;
        Result.pixHeight := h ;
      end ;
    end ;

    rct := slp.MapToRasterRect( ext, CS ) ;
    if rct.Right > Result.pixWidth then
      r := Result.pixWidth
    else
      r := rct.Right ;
    if rct.Bottom > Result.pixHeight then
      b := Result.pixHeight
    else
      b := rct.Bottom ;

    Result.FExtent := _TGIS_Extent( slp.FExtent ) ;
    Result.FBounds := Rect( rct.Left, rct.Top, r-1, b-1 ) ;  // decreased for loops
    Result.FPixelSize := GisPoint( pixelsizex, pixelsizey ) ;
    Result.FWritable := _writable or (slp = self);
    Result.FCS := CS      ;
    Result.FBandNo := fromBand ;
    FIsGridImage := ig ;
    if FIsGridImage then
      Result.FGrid := slp.oGrid
    else
      Result.FBitmap := slp.oBitmap ;
    if fromBand <> 0 then
      Result.FBand := slp.oBand ;
  finally
    outCS := orgCS ;
    Params.Pixel.GridBand := ogridband ;
    forViewer := True ;
    if slp <> self then begin
      if assigned(slp) then begin
        inc(slp.lockUsingNo) ;
        if _writable then
          oLockListW := lplist
        else
          oLockListR := lplist ;
      end;
    end;
  end;
end ;

function TGIS_LayerPixel.LockPixels(
  const _rct        : TRect ;
  const _ext        : TGIS_Extent ;
  const _tilelevel  : Integer ;
  const _tilerow    : Integer ;
  const _tilecolumn : Integer ;
  const _writable   : Boolean
) : TGIS_LayerPixelLock ;
var
  ps         : Double ;
  ext        : TGIS_Extent ;
  rct        : TRect ;
  w, h       : Integer ;
  pixelsize  : Double ;
  pixelsizex : Double ;
  pixelsizey : Double ;
  r, b       : Integer ;
begin
  ps := (_ext.XMax -_ext.XMin)/_rct.Width ;

  Result := TGIS_LayerPixelLock.Create( Self ) ;

  ext := _ext ;
  if (ext.XMin = ext.XMax) or (ext.YMin = ext.YMax) then
    exit ;

  pixelsize  := ps ;
  pixelsizex := (_ext.XMax -_ext.XMin)/_rct.Width  ;
  pixelsizey := (_ext.YMax -_ext.YMin)/_rct.Height ;
  pixelsizey := pixelsizey*(pixelsize/pixelsizex) ;
  pixelsizex := pixelsize ;

  w := RoundS((ext.XMax -ext.XMin)/pixelsizex) ;
  h := RoundS((ext.YMax -ext.YMin)/pixelsizey) ;

  if w = 0 then begin
    ext.XMin := FloorS( ext.XMin/pixelsize)*pixelsizex ;
    ext.XMax := ext.XMin +pixelsizex ;
    w := 1 ;
  end ;

  if h = 0 then begin
    ext.YMin := FloorS( ext.YMin/pixelsizey)*pixelsizey ;
    ext.YMax := ext.YMin +pixelsizey ;
    h := 1 ;
  end ;

  rct := Rect(
      FloorS( ( ext.XMin + 0.1*pixelsizex) / pixelsizex ) -
      FloorS( ( ext.XMin + 0.1*pixelsizex) / pixelsizex ),
      FloorS( ( ext.YMax + 0.1*pixelsizey) / pixelsizey ) -
      FloorS( ( ext.YMax + 0.1*pixelsizey) / pixelsizey ),
      FloorS( ( ext.XMax + 0.1*pixelsizex) / pixelsizex ) -
      FloorS( ( ext.XMin + 0.1*pixelsizex) / pixelsizex ),
      FloorS( ( ext.YMax + 0.1*pixelsizey) / pixelsizey ) -
      FloorS( ( ext.YMin + 0.1*pixelsizey) / pixelsizey )
    ) ;

  Result.pixWidth  := w ;
  Result.pixHeight := h ;
  Result.FExtent   := _TGIS_Extent(ext) ;

  if rct.Right > Result.pixWidth then
    r := Result.pixWidth
  else
    r := rct.Right ;

  if rct.Bottom > Result.pixHeight then
    b := Result.pixHeight
  else
    b := rct.Bottom ;

  Result.FBounds          := Rect(rct.Left, rct.Top, r -1, b -1) ;
  Result.FPixelSize       := GisPoint(pixelsizex, pixelsizey) ;
  Result.FWritable        := _writable ;
  Result.FCS              := CS      ;
  Result.FGrid            := oGrid   ;
  Result.FBitmap          := oBitmap ;

  Result.FIsTiled         := True ;
  Result.FTileInfo.Level  := _tilelevel ;
  Result.FTileInfo.Row    := _tilerow ;
  Result.FTileInfo.Column := _tilecolumn ;
end ;

procedure TGIS_LayerPixel.UnlockPixels(
{$IFDEF GIS_PDK}
  const
{$ELSE}
  var
{$ENDIF}
    _lock : TGIS_LayerPixelLock
) ;
var
  i, idx   : Integer ;
  w, h     : Integer ;
  pix      : TGIS_Pixels ;
  slp      : TGIS_LayerPixel ;
  lplist   : TObjectList< TGIS_LayerPixel > ;
  is_saved : Boolean ;

  procedure set_min_max_z ;
  var
    m, n             : Integer ;
    was_first_change : Boolean ;
    ll, tt, rr, bb   : Integer ;
  begin
    was_first_change := False ;
    if length(_lock.FGrid[0]) = FBitWidth then begin
      ll := _lock.FBounds.Left ;
      if _lock.FBounds.Right < _lock.pixWidth then
        rr := _lock.FBounds.Right
      else
        rr := _lock.pixWidth -1 ;
    end
    else begin
      ll := 0 ;
      rr := _lock.pixWidth -1 ;
    end;

    if length(_lock.FGrid) = FBitHeight then begin
      tt := _lock.FBounds.Top ;
      if _lock.FBounds.Bottom < _lock.pixHeight then
        bb := _lock.FBounds.Bottom
      else
        bb :=  _lock.pixHeight -1 ;
    end
    else begin
      tt := 0 ;
      bb :=  _lock.pixHeight -1 ;
    end;


    for m := tt to bb do begin
      for n := ll to rr do begin
        if _lock.FGrid[m, n] <> NoDataValue then begin
          if not was_first_change then begin
            if ( _lock.FGrid[m, n] < FMinZ ) then
              FMinZ := _lock.FGrid[m, n] ;
            if ( _lock.FGrid[m][n] > FMaxZ ) then
              FMaxZ := _lock.FGrid[m, n] ;
            was_first_change := True ;
          end
          else begin
            if ( _lock.FGrid[m, n] < FMinZ ) then
              FMinZ := _lock.FGrid[m, n]
            else if ( _lock.FGrid[m, n] > FMaxZ ) then
              FMaxZ := _lock.FGrid[m, n] ;
          end ;
        end ;
      end ;
    end ;
  end ;
begin
  if not assigned(_lock) then exit ;

  slp := nil ;
  idx := -1 ;
  lplist := nil ;
  i := 0 ;
  is_saved := False ;
  try
    if not _lock.Writable then begin
      lplist := oLockListR ;
    end
    else begin
     lplist := oLockListW ;
     if assigned( tmCache ) then
       tmCache.ResetSerial ;
    end ;

    if assigned(lplist) then begin
      for i := lplist.Count -1 downto 0 do begin
        slp := lplist.Items[i] ;

        if assigned(_lock.FGrid) then begin
          if _lock.FGrid = slp.oGrid then begin
            idx := i ;
            break ;
          end ;
        end  else
        if assigned(_lock.FBitmap) then begin
          if _lock.FBitmap = slp.oBitmap then begin
            idx := i ;
            break ;
          end
        end else
        if assigned(_lock.FBand) then begin
          if _lock.FBand = slp.oBand then begin
            idx := i ;
            break ;
          end ;
        end;
      end ;
    end ;

    if (idx >= 0)  then begin
      if slp.FIsGridImage <> FIsGridImage then
         exit ;

      if isBuilt then begin
        is_saved := True ;
        if assigned(oGrid) then begin
          if _lock.FGrid <> oGrid  then begin
            PutGrid(_lock.FExtent,
                     slp.oGrid
                   ) ;
          end;
        end
        else
        if assigned(oBitmap) then begin
          w := _lock.FBounds.Right -_lock.FBounds.Left +1 ;
          h := _lock.FBounds.Bottom -_lock.FBounds.Top +1 ;
          if assigned(slp.oGrid) then begin //band locked
            SetLength(pix, w * h) ;
            getBitmapData( _lock.FExtent,
                              slp.oBitmap,
                              w,
                              h
                             ) ;
            putBitmapRawData( _lock.FExtent,
                              pix,
                              w,
                              h
                             ) ;
            pix := nil ;
          end
          else  begin
            putBitmapRawData( _lock.FExtent,
                              slp.oBitmap,
                              _lock.pixWidth,
                              h
                             ) ;
          end;
        end
        else begin // Temporary file is used
          if IsGridImage then begin
            PutGrid(_lock.FExtent,
                     slp.oGrid
                   ) ;
          end
          else
          begin
            w := _lock.FBounds.Right -_lock.FBounds.Left +1 ;
            h := _lock.FBounds.Bottom -_lock.FBounds.Top +1 ;
            if assigned(slp.oGrid) then begin //band locked
              SetLength(pix, w * h) ;
              getBitmapData( _lock.FExtent,
                                slp.oBitmap,
                                w,
                                h
                               ) ;
              putBitmapRawData( _lock.FExtent,
                                pix,
                                w,
                                h
                               ) ;
              pix := nil ;
            end
            else  begin
              putBitmapRawData( _lock.FExtent,
                                slp.oBitmap,
                                _lock.pixWidth,
                                h
                               ) ;
            end;
          end
        end;
      end;
    end;
  finally
    if idx >= 0 then begin
      dec(lplist.Items[i].lockUsingNo) ;
      if oLockListR = lplist then begin //not writable only
        if oLockListR.Items[i].lockUsingNo = 0 then begin
          oLockListR.Delete(idx);
          if oLockListR.Count = 0 then
            FreeObject(oLockListR) ;
        end;
      end
      else
      if is_saved then begin //writable only
        if oLockListW.Items[i].lockUsingNo = 0 then begin
          oLockListW.Delete(idx);
          if oLockListW.Count = 0 then
            FreeObject(oLockListW) ;
        end;
      end;

    end ;
    if _lock.Writable then begin
      if FIsGridImage then
        set_min_max_z ;
      FIsModified := True ;
    end ;

    {$IFDEF GIS_PDK}
       FreeObjectNotNil( _lock ) ;
    {$ELSE}
       FreeObject( _lock ) ;
    {$ENDIF}
  end;
end ;

function TGIS_LayerPixel.&Loop(
  const _extent      : TGIS_Extent ;
  const _pixelsize   : Double      ;
  const _fatpixel    : Boolean     ;
  const _shape       : TGIS_Shape  ;
  const _de9im       : String      ;
  const _writable    : Boolean
) : TGIS_LayerPixelEnumeratorFactory ;
begin
  Result := TGIS_LayerPixelEnumeratorFactory.Create ;

  Result.SetUp( self, _extent, _pixelsize, _fatpixel, _shape, _de9im, _writable );
end ;

function TGIS_LayerPixel.&Loop(
  const _extent      : TGIS_Extent ;
  const _pixelsize   : Double      ;
  const _fatpixel    : Boolean     ;
  const _shape       : TGIS_Shape  ;
  const _de9im       : String      ;
  const _band        : Integer     ;
  const _writable    : Boolean
) : TGIS_LayerPixelEnumeratorFactory ;
begin
  Result := TGIS_LayerPixelEnumeratorFactory.Create ;

  Result.SetUp( self, _extent, _pixelsize, _fatpixel, _shape, _de9im, _band, _writable );
end ;


function TGIS_LayerPixel.&Loop(
  const _extent      : TGIS_Extent ;
  const _pixelsize   : Double      ;
  const _shape       : TGIS_Shape  ;
  const _de9im       : String      ;
  const _writable    : Boolean
) : TGIS_LayerPixelEnumeratorFactory ;
begin
  Result := TGIS_LayerPixelEnumeratorFactory.Create ;

  Result.SetUp( self, _extent, _pixelsize, false, _shape, _de9im, _writable )
end ;

function TGIS_LayerPixel.&Loop(
  const _extent      : TGIS_Extent ;
  const _pixelsize   : Double      ;
  const _shape       : TGIS_Shape  ;
  const _de9im       : String      ;
  const _band        : Integer     ;
  const _writable    : Boolean
) : TGIS_LayerPixelEnumeratorFactory ;
begin
  Result := TGIS_LayerPixelEnumeratorFactory.Create ;

  Result.SetUp( self, _extent, _pixelsize, false, _shape, _de9im, _band, _writable )
end ;

function TGIS_LayerPixel.&Loop(
  const _pixelsize   : Double      ;
  const _shape       : TGIS_Shape  ;
  const _writable    : Boolean
) : TGIS_LayerPixelEnumeratorFactory ;
begin
  Result := TGIS_LayerPixelEnumeratorFactory.Create ;

  Result.SetUp( self, _shape.Extent, _pixelsize, false, _shape, '#', _writable )
end ;

function TGIS_LayerPixel.&Loop(
  const _pixelsize   : Double      ;
  const _shape       : TGIS_Shape  ;
  const _band        : Integer     ;
  const _writable    : Boolean
) : TGIS_LayerPixelEnumeratorFactory ;
begin
  Result := TGIS_LayerPixelEnumeratorFactory.Create ;

  Result.SetUp( self, _shape.Extent, _pixelsize, false, _shape, '#', _band, _writable )
end ;

function TGIS_LayerPixel.&Loop(
  const _extent      : TGIS_Extent ;
  const _pixelsize   : Double      ;
  const _writable    : Boolean
) : TGIS_LayerPixelEnumeratorFactory ;
begin
  Result := TGIS_LayerPixelEnumeratorFactory.Create ;

  Result.SetUp( self, _extent, _pixelsize, false, nil, '', _writable );
end ;

function TGIS_LayerPixel.&Loop(
  const _extent      : TGIS_Extent ;
  const _pixelsize   : Double      ;
  const _band        : Integer     ;
  const _writable    : Boolean
) : TGIS_LayerPixelEnumeratorFactory ;
begin
  Result := TGIS_LayerPixelEnumeratorFactory.Create ;

  Result.SetUp( self, _extent, _pixelsize, false, nil, '', _band, _writable );
end ;

procedure TGIS_LayerPixel.Recalculate ;
var
  i, j : Integer ;
begin
  FNoDataValue := Params.Pixel.GridNoValue ;
  if assigned(oGrid) then begin
    FMinZ :=  GIS_MAX_SINGLE ;
    FMaxZ := -GIS_MAX_SINGLE ;
    for j := 0 to FBitHeight -1 do begin
      for i := 0 to FBitWidth -1 do begin
          if oGrid[j][i] <> NoDataValue then begin
            if oGrid[j][i] < FMinZ then
              FMinZ := oGrid[j][i]
            else
            if oGrid[j][i] > FMaxZ then
              FMaxZ := oGrid[j][i] ;
          end ;
      end ;
    end ;
  end
  else
    prepareMinMaxZ ;
end ;

function TGIS_LayerPixel.MapToRaster(
  const _ptg    : TGIS_Point  ;
  const _cs     : TGIS_CSCoordinateSystem
) : TPoint ;
var
  ptg : TGIS_Point ;
begin
  if assigned( _cs ) and ( _cs.EPSG > 0 )
     and
     assigned(  CS ) and (  CS.EPSG > 0 )
  then
    ptg := _cs.ToCS( CS, _ptg )
  else
    ptg := _TGIS_Point(_ptg) ;

  Result := Point(
    floor_pixel( ptg.X - FExtent.XMin, FPixelSize.X ),
    floor_pixel( FExtent.YMax - ptg.Y, FPixelSize.Y )
  ) ;
end ;

function TGIS_LayerPixel.RasterToMap(
  const _pt     : TPoint ;
  const _cs     : TGIS_CSCoordinateSystem
) : TGIS_Point ;
var
  res : TGIS_Point ;
begin
  res := GisPoint(
           FExtent.XMin + ( _pt.X + 0.5 ) * FPixelSize.X,
           FExtent.YMax - ( _pt.Y + 0.5 ) * FPixelSize.Y
         ) ;

  if assigned( _cs ) and ( _cs.EPSG > 0 )
     and
     assigned(  CS ) and (  CS.EPSG > 0 )
  then
    res := _cs.FromCS( CS, res ) ;

  Result := res ;
end ;

function TGIS_LayerPixel.MapToRasterRect(
  const _ext    : TGIS_Extent ;
  const _cs     : TGIS_CSCoordinateSystem
) : TRect ;
var
  ext : TGIS_Extent ;
begin
  if assigned( _cs ) and ( _cs.EPSG > 0 )
     and
     assigned(  CS ) and (  CS.EPSG > 0 )
  then
    ext := _TGIS_Extent(_cs.ExtentToCS( CS, _ext ))
  else
    ext := _TGIS_Extent(_ext) ;

  Result := Rect(
    round_pixel( ext.XMin -FExtent.XMin, FPixelSize.X ),
    round_pixel( FExtent.YMax- ext.YMax, FPixelSize.Y ),
    round_pixel( ext.XMax -FExtent.XMin, FPixelSize.X ),
    round_pixel( FExtent.YMax -ext.YMin, FPixelSize.Y )
  ) ;
end ;

function TGIS_LayerPixel.RasterToMapRect(
  const _rct    : TRect ;
  const _cs     : TGIS_CSCoordinateSystem
) : TGIS_Extent ;
var
  res : TGIS_Extent ;
begin
  res := GisExtent(
           FExtent.XMin + _rct.Left   * FPixelSize.X,
           FExtent.YMax - _rct.Bottom * FPixelSize.Y,
           FExtent.XMin + _rct.Right  * FPixelSize.X,
           FExtent.YMax - _rct.Top    * FPixelSize.Y
         ) ;

  if assigned( _cs ) and ( _cs.EPSG > 0 )
     and
     assigned(  CS ) and (  CS.EPSG > 0 )
  then
    res := _TGIS_Extent(_cs.ExtentFromCS( CS, res )) ;
  Result := res ;
end ;

function TGIS_LayerPixel.IsPixel : Boolean ;
begin
  Result := True ;
end ;

function TGIS_LayerPixel.IsGrid : Boolean ;
begin
  Result := IsGridImage ;
end ;

procedure TGIS_LayerPixel.RevertAll ;
begin
  FreeObject(oLockListW) ;
  FreeObject(oLockListR) ;
end ;

procedure TGIS_LayerPixel.InitializeWrite ;
begin
  // only for safe inheritance
end ;

procedure TGIS_LayerPixel.FinalizeWrite ;
begin
  // only for safe inheritance
end ;
{$ENDREGION}

{$REGION 'TGIS_LayerPixelEnumerator'}
constructor TGIS_LayerPixelEnumerator.Create(
  const _factory     : TGIS_LayerPixelEnumeratorFactory ;
  const _lp          : TGIS_LayerPixel  ;
  const _extent      : TGIS_Extent      ;
  const _pixelsize   : Double           ;
  const _fatpixel    : Boolean          ;
  const _shape       : TGIS_Shape       ;
  const _de9im       : String           ;
  const _writable    : Boolean
);
begin
  Create( _factory, _lp, _extent, _pixelsize, _fatpixel, _shape, _de9im, 0, _writable ) ;
end ;

constructor TGIS_LayerPixelEnumerator.Create(
  const _factory     : TGIS_LayerPixelEnumeratorFactory ;
  const _lp          : TGIS_LayerPixel  ;
  const _extent      : TGIS_Extent      ;
  const _pixelsize   : Double           ;
  const _fatpixel    : Boolean          ;
  const _shape       : TGIS_Shape       ;
  const _de9im       : String           ;
  const _band        : Integer          ;
  const _writable    : Boolean
);
begin
  inherited Create ;

  oFactory         := _factory     ;
  paramLayer       := _lp          ;
  paramExtent      := _extent      ;
  paramPixelSize   := _pixelsize   ;
  paramFatPixel    := _fatpixel    ;
  paramShape       := _shape       ;
  paramDe9im       := _de9im       ;
  paramBand        := _band        ;
  paramWritable    := _writable    ;

  currPixel := TGIS_PixelItem.Create ;

  if _band = 0 then begin
    SetLength( currPixel.Bands, 0 ) ;
    SetLength( currLock, 1 ) ;
  end
  else
  if _band > 0 then begin
    SetLength( currPixel.Bands, 1 ) ;
    SetLength( currLock, 1 );
  end
  else begin
    SetLength( currPixel.Bands, _lp.BandsCount ) ;
    SetLength( currLock, _lp.BandsCount ) ;
  end;

  Reset ;
end ;

constructor TGIS_LayerPixelEnumerator.Create(
  const _lp          : TGIS_LayerPixel  ;
  const _extent      : TGIS_Extent      ;
  const _pixelsize   : Double           ;
  const _fatpixel    : Boolean          ;
  const _shape       : TGIS_Shape       ;
  const _de9im       : String           ;
  const _writable    : Boolean
);
begin
  Create(
    nil          ,
    _lp          ,
    _extent      ,
    _pixelsize   ,
    _fatpixel    ,
    _shape       ,
    _de9im       ,
    _writable
  ) ;
end ;

{$IFNDEF MANAGED}
  destructor TGIS_LayerPixelEnumerator.Destroy ;
  var
    band : Integer ;
  begin
    for band := low( currLock ) to high( currLock ) do begin
      paramLayer.UnlockPixels( currLock[ band ] ) ;
    end ;

    FreeObject( currPixel    ) ;
    FreeObject( currPoint    ) ;
    FreeObject( currPolygon  ) ;
    FreeObject( currShape    ) ;
    FreeObject( currTopology ) ;
    FreeObject( oFactory     ) ;
    inherited ;
  end ;
{$ELSE}
  procedure TGIS_LayerPixelEnumerator.Dispose ;
  var
    band : Integer ;
  begin
    for band := low( currLock ) to high( currLock ) do begin
      paramLayer.UnlockPixels( currLock[ band ] ) ;
    end ;

    {$IFNDEF OXYGENE}
      inherited ;
    {$ENDIF}
  end ;
{$ENDIF}

procedure TGIS_LayerPixelEnumerator.Reset ;
var
  band : Integer ;
begin
  if paramBand = 0 then begin
    currLock[ 0 ] := paramLayer.LockPixels(
                       paramExtent, paramLayer.CS,
                       paramPixelSize, paramWritable
                     ) ;
  end
  else begin
    for band := low( currLock ) to high( currLock ) do begin
      currLock[ band ] := paramLayer.LockPixels(
                            paramExtent, paramLayer.CS,
                            paramPixelSize, band + 1, paramWritable
                          ) ;
    end ;
  end;

  currBof := True ;

  if paramDe9im = '#' then begin
    // very special case for lines etc
    currSpecialMode := True ;
    paramDe9im := 'T' ;
  end
  else
    currSpecialMode := False ;

  if not assigned( paramShape ) then
    currShapeType := TGIS_ShapeType.Null
  else begin
    currShapeType := paramShape.ShapeType ;

    currShape := paramShape.CreateCopyCS( paramLayer.CS ) ;
    assert( assigned( currShape ) ) ;
  end ;

  case currShapeType of
    TGIS_ShapeType.Null       ,
    TGIS_ShapeType.Point      ,
    TGIS_ShapeType.MultiPoint ,
    TGIS_ShapeType.Arc        :
        // do nothing
    else
      currSpecialMode := False ;
  end ;

  if assigned( currShape ) and ( not currSpecialMode )then begin
   // build topology relation
   currTopology := TGIS_Topology.Create ;
   currTopology.RelatePrepare( currShape );
  end ;


  currPoint := TGIS_ShapePoint.Create ;
  currPoint.AddPart ;

  currPolygon := TGIS_ShapePolygon.Create ;
  currX := -1 ;
  currY := -1 ;
end ;

function TGIS_LayerPixelEnumerator.MoveNext: Boolean;
var
  band       : Integer ;
  skip       : Boolean ;
  ptg        : TGIS_Point ;
  pt         : TPoint ;
  ext        : TGIS_Extent ;
  doff       : Double ;
  first_iter : Boolean ;
begin
  doff := Min( currLock[0].PixelSize.X, currLock[0].PixelSize.Y ) ;

  skip := False ;
  first_iter := True ;
  while True do begin
    if ( not currBof ) and paramWritable and ( not skip ) then begin
       if assigned( currLock[0].Bitmap ) then
         currLock[0].Bitmap[
           currLock[0].BitmapPos( currX, currY )
         ] := Integer( currPixel.Color.ARGB ) ;

      if assigned( currLock[0].Grid ) then
        currLock[0].Grid[currY][currX] := currPixel.Value ;
    end ;

    currPixel.FDistance := -1 ;

    if currSpecialMode then begin
      case currShapeType of
        TGIS_ShapeType.Point,
        TGIS_ShapeType.MultiPoint : begin
          if currBof then begin
            currBof := False ;
            currPos := 0
          end
          else begin
            inc( currPos ) ;
          end ;

          if currPos < paramShape.GetPartSize( 0 ) then begin
            ptg := currShape.GetPoint( currPos, 0) ;

            if not GisIsPointInsideExtent( ptg, currLock[0].Extent ) then begin
              skip := True ;
              continue ;
            end ;
            if not first_iter and
               ( ( pt.X < currLock[0].Bounds.Left ) or
                 ( pt.X > currLock[0].Bounds.Right ) or
                 ( pt.Y < currLock[0].Bounds.Top ) or
                 ( pt.Y > currLock[0].Bounds.Bottom ) ) then
            begin
              skip := True ;
              continue ;
            end ;

            pt := currLock[0].MapToRaster( ptg, paramLayer.CS ) ;
            ptg.X := currLock[0].PixelSize.X * pt.X +currLock[0].PixelSize.X/2
                     + currLock[0].FExtent.XMin ;
            ptg.Y := currLock[0].FExtent.YMax
                     - (currLock[0].PixelSize.Y * pt.Y +currLock[0].PixelSize.Y/2) ;

            currX := pt.X ;
            currY := pt.Y ;

            Result := True ;
          end
          else
            Result := False ;

          break ;
        end ;

        TGIS_ShapeType.Arc : begin
          if currBof then begin
            currBof    := False ;
            currOffset := 0 ;
            currLength := currShape.Length ;
          end
          else begin
            currOffset := currOffset + doff ;
          end ;

          if currOffset < (currLength +doff) then begin
            ptg := TGIS_ShapeArc( currShape ).GetPointOnLine( currOffset, 0, -1, False ) ;
            if not GisIsPointInsideExtent( ptg, currLock[0].Extent ) then begin
              skip := True ;
              continue ;
            end ;

            pt := currLock[0].MapToRaster( ptg, nil ) ;
            if pt.X = (currLock[0].Bounds.Right +1)then
              pt.X := currLock[0].Bounds.Right ;
            if pt.Y = (currLock[0].Bounds.Bottom +1)then
              pt.Y := currLock[0].Bounds.Bottom ;

            if (pt.X = currX) and (pt.Y = currY) then begin
              skip := True ;
              continue ;
            end ;

            if ( pt.X < currLock[0].Bounds.Left   ) or
               ( pt.X > currLock[0].Bounds.Right  ) or
               ( pt.Y < currLock[0].Bounds.Top    ) or
               ( pt.Y > currLock[0].Bounds.Bottom ) then
            begin
              skip := True ;
              continue ;
            end ;
            ptg.X := currLock[0].PixelSize.X * pt.X +currLock[0].PixelSize.X/2
                     + currLock[0].FExtent.XMin ;
            ptg.Y := currLock[0].FExtent.YMax
                     - (currLock[0].PixelSize.Y * pt.Y +currLock[0].PixelSize.Y/2) ;

            currX := pt.X ;
            currY := pt.Y ;
            currPixel.FDistance := currOffset ;

            Result := True ;
          end
          else begin
            Result := False ;
          end ;

          break ;
        end ;

        else begin
          assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) );
        end ;

        first_iter := False ;
      end ;
    end
    else begin
      if currBof then begin
        currBof := False ;
        currX   := currLock[0].Bounds.Left-1 ;
        currY   := currLock[0].Bounds.Top    ;
      end ;

      inc( currX ) ;
      if currX > currLock[0].Bounds.Right then begin
        currX := currLock[0].Bounds.Left ;
        inc( currY ) ;
      end ;

      if currY <= Abs( currLock[0].Bounds.Bottom ) then begin
        currPixel.FWritable := currLock[0].FWritable ;
        currPixel.FCenter := currLock[0].RasterToMap( Point( currX, currY ), nil ) ;

        ptg := currLock[0].RasterToMap( Point( currX, currY ), nil ) ;

        if assigned( currTopology ) then begin
          if paramFatPixel then begin

            ext := GisExtent( ptg.X - currLock[0].PixelSize.X / 2,
                              ptg.Y - currLock[0].PixelSize.Y / 2,
                              ptg.X + currLock[0].PixelSize.X / 2,
                              ptg.Y + currLock[0].PixelSize.Y / 2
                            ) ;

            currPolygon.Reset ;
            currPolygon.AddPart ;
            currPolygon.AddPoint( GisPoint( ext.XMin, ext.YMin ) ) ;
            currPolygon.AddPoint( GisPoint( ext.XMin, ext.YMax ) ) ;
            currPolygon.AddPoint( GisPoint( ext.XMax, ext.YMax ) ) ;
            currPolygon.AddPoint( GisPoint( ext.XMax, ext.YMin ) ) ;
            currPolygon.AddPoint( GisPoint( ext.XMin, ext.YMin ) ) ;

            if not currTopology.Relate( currShape, currPolygon, paramDe9im  ) then
            begin
              skip := True ;
              continue ;
            end ;
          end
          else begin
            currPoint.AddPoint( currPixel.FCenter );
            if not currTopology.Relate( currShape, currPoint, paramDe9im  ) then
            begin
              skip := True ;
              continue ;
            end ;
          end ;
        end ;

        Result := True ;
      end
      else begin
        Result := False ;
      end ;

      break ;
    end ;

    case currShapeType of
      TGIS_ShapeType.Point,
      TGIS_ShapeType.MultiPoint : begin
        if currBof then begin
          currBof := False ;
          currPos := 0
        end
        else begin
          inc( currPos ) ;
        end ;

        if currPos < paramShape.GetPartSize( 0 ) then begin
          ptg := currShape.GetPoint( currPos, 0) ;

          if ( not IsStringEmpty( paramDe9im ) )
             and
             (
               ( paramDe9im[ StringFirst ] = 'F' )
               or
               ( paramDe9im[ StringFirst ] = 'f' )
             )
          then begin
            if GisIsPointInsideExtent( ptg, currLock[0].Extent ) then begin
              skip := True ;
              continue ;
            end ;
          end
          else begin
            if not GisIsPointInsideExtent( ptg, currLock[0].Extent ) then begin
              skip := True ;
              continue ;
            end ;
          end ;

          pt := currLock[0].MapToRaster( ptg, paramLayer.CS ) ;
          currX := pt.X ;
          currY := pt.Y ;

          Result := True ;
        end
        else
          Result := False ;

        break ;
      end ;

      TGIS_ShapeType.Arc : begin
        if currBof then begin
          currBof    := False ;
          currOffset := 0 ;
          currLength := currShape.Length ;
        end
        else begin
          currOffset := currOffset +
                        ( currLock[0].PixelSize.X + currLock[0].PixelSize.Y ) / 2
        end ;

        if currOffset <= currLength then begin
          ptg := TGIS_ShapeArc( currShape ).GetPointOnLine( currOffset, 0 ) ;
          if not GisIsPointInsideExtent( ptg, currLock[0].Extent ) then begin
            skip := True ;
            continue ;
          end ;

          pt := currLock[0].MapToRaster( ptg, nil ) ;
          currX := pt.X ;
          currY := pt.Y ;
          currPixel.FDistance := currOffset ;

          Result := True ;
        end
        else begin
          Result := False ;
        end ;

        break ;
      end ;
    end ;
  end ;

  if Result then begin
    currPixel.FCenter := _TGIS_Point(ptg) ;

    if paramFatPixel then
      currPixel.FExtent := ext
    else
      currPixel.FExtent := GisExtent( ptg.X - currLock[0].PixelSize.X / 2,
                                      ptg.Y - currLock[0].PixelSize.Y / 2,
                                      ptg.X + currLock[0].PixelSize.X / 2,
                                      ptg.Y + currLock[0].PixelSize.Y / 2
                                    ) ;

    {$IFDEF GIS_NORECORDS}
      currPixel.Color := TGIS_Color.create;
    {$ENDIF}
    if assigned( currLock[0].Bitmap ) then
      currPixel.Color.ARGB := Cardinal(
        currLock[0].Bitmap[currLock[0].BitmapPos( currX, currY )]
      )
    else
      currPixel.Color.ARGB := 0 ;

    if assigned( currLock[0].Grid ) then
      currPixel.Value := currLock[0].Grid[currY][currX]
    else
      currPixel.Value := paramLayer.NoDataValue ;

    for band := low( currPixel.Bands ) to high( currPixel.Bands ) do begin
      currPixel.Bands[ band ] := currLock[band].Band[currY][currX] ;
    end ;
  end ;
end ;

function TGIS_LayerPixelEnumerator.GetCurrent
  : TGIS_PixelItem ;
begin
  Result := currPixel ;
end ;

{$IFDEF CLR}
  function TGIS_LayerPixelEnumerator.fget_current_obj
    : TObject ;
  begin
    Result := currPixel ;
  end ;
{$ENDIF}

{$IFDEF JAVA}
  method TGIS_LayerPixelEnumerator.hasNext : Boolean ;
  begin
    Result := MoveNext ;
  end ;

  method TGIS_LayerPixelEnumerator.next : TObject;
  begin
    Result := GetCurrent ;
  end ;

  method TGIS_LayerPixelEnumerator.&remove ;
  begin

  end ;
{$ENDIF}

{$ENDREGION}

{$REGION 'TGIS_LayerPixelEnumeratorFactory'}
procedure TGIS_LayerPixelEnumeratorFactory.SetUp(
  const _lp          : TGIS_LayerPixel  ;
  const _extent      : TGIS_Extent      ;
  const _pixelsize   : Double           ;
  const _fatpixel    : Boolean          ;
  const _shape       : TGIS_Shape       ;
  const _de9im       : String           ;
  const _writable    : Boolean
);
begin
  SetUp( _lp, _extent, _pixelsize, _fatpixel, _shape, _de9im, 0, _writable ) ;
end ;

procedure TGIS_LayerPixelEnumeratorFactory.SetUp(
  const _lp          : TGIS_LayerPixel ;
  const _extent      : TGIS_Extent     ;
  const _pixelsize   : Double          ;
  const _fatpixel    : Boolean         ;
  const _shape       : TGIS_Shape      ;
  const _de9im       : String          ;
  const _band        : Integer         ;
  const _writable    : Boolean
);
var
  lp_ext        : TGIS_Extent ;
  wide_ext      : TGIS_Extent ;
  lp_pixel_size : TGIS_Point ;
begin
  lp_ext := _lp.FExtent ;

  if GisIsSameValue( _pixelsize, 0 ) then
    // original pixel size
    lp_pixel_size := _lp.FPixelSize
  else
    lp_pixel_size := GisPoint( _pixelsize, _pixelsize ) ;

  wide_ext := GisExtent(
    floor_pixel( _extent.XMin - lp_ext.XMin, lp_pixel_size.X ) * lp_pixel_size.X + lp_ext.XMin,
    floor_pixel( _extent.YMin - lp_ext.YMin, lp_pixel_size.Y ) * lp_pixel_size.Y + lp_ext.YMin,
    ceil_pixel( _extent.XMax - lp_ext.XMin, lp_pixel_size.X ) * lp_pixel_size.X + lp_ext.XMin,
    ceil_pixel( _extent.YMax - lp_ext.YMin, lp_pixel_size.Y ) * lp_pixel_size.Y  + lp_ext.YMin
  ) ;

  stateLayer       := _lp        ;
  stateExtent      := wide_ext   ;
  statePixelSize   := _pixelsize ;
  stateFatPixel    := _fatpixel  ;
  stateShape       := _shape     ;
  stateDe9im       := _de9im     ;
  stateBand        := _band      ;
  stateWritable    := _writable  ;
end ;

{$IFDEF JAVA}
  method TGIS_LayerPixelEnumeratorFactory.&iterator
    : java.util.Iterator<TObject> ;
  begin
    Result := GetEnumerator ;
  end ;
{$ENDIF}

function TGIS_LayerPixelEnumeratorFactory.GetEnumerator
  : TGIS_LayerPixelEnumerator ;
begin
  Result := TGIS_LayerPixelEnumerator.Create(
    self,
    stateLayer,
    stateExtent,
    statePixelSize,
    stateFatPixel,
    stateShape,
    stateDe9im,
    stateBand,
    stateWritable
  ) ;
end ;

{$IFDEF CLR}
  function TGIS_LayerPixelEnumeratorFactory.ToLinq
    : TGIS_LayerPixelEnumeratorFactoryLinq ;
  begin
    Result := TGIS_LayerPixelEnumeratorFactoryLinq.Create(
      TGIS_LayerPixelEnumerator.Create(
        self,
        stateLayer,
        stateExtent,
        statePixelSize,
        stateFatPixel,
        stateShape,
        stateDe9im,
        stateBand,
        stateWritable
      )
    ) ;
  end ;

  constructor TGIS_LayerPixelEnumeratorFactoryLinq.Create(
    const _en :TGIS_LayerPixelEnumerator
   ) ;
  begin
    inherited Create ;
    en := _en ;
  end ;

  function TGIS_LayerPixelEnumeratorFactoryLinq.GetEnumerator
    : IEnumerator ;
  begin
    Result := en ;
  end ;
{$ENDIF}
{$ENDREGION}

  {$IFNDEF GENXDK}
  {$ELSE}
    procedure TGIS_ColorMapArray.SetLength( const _size : Integer ) ;
    begin
    end ;
  {$ENDIF}

{==================================== END =====================================}
end.
