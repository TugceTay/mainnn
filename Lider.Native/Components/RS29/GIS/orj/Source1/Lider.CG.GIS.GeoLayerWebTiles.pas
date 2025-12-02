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
  Support for Web Tiles
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerWebTiles ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerWebTiles"'}
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

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    System.Collections.Generic,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,
    System.SysUtils,
    System.Classes,
    System.Generics.Collections,
    System.SyncObjs,

    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoLayerPixel ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl,
    tatukgis.rtl.xml ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerWebTiles = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  /// <summary>
  ///   Level of details (LOD) specification.
  /// </summary>
  TGIS_LayerWebTileLod = {$IFDEF OXYGENE} public class
                         {$ELSE}          record
                         {$ENDIF}

    private
      FId         : String     ;
      FResolution : Double     ;
      FTopLeft    : TGIS_Point ;
      FTileSize   : TPoint     ;
      FTilesRange : TRect      ;

    public
      /// <summary>
      ///   Create an instance of LOD.
      /// </summary>
      /// <param name="_id">
      ///   level identifier
      /// </param>
      /// <param name="_resolution">
      ///   level resolution (extent divided by pixel width)
      /// </param>
      /// <param name="_topleft">
      ///   top-left origin of a level (coordinates of top-left tile corner)
      /// </param>
      /// <param name="_tilesize">
      ///   tile size of level in pixels
      /// </param>
      /// <param name="_tilesrange">
      ///   first/Last Row/Column of a tile
      /// </param>
      constructor Create(
         _id         : String ;
         _resolution : Double ;
         _topleft    : TGIS_Point ;
         _tilesize   : TPoint ;
         _tilesrange : TRect
      ) ;

      /// <summary>
      ///   Level identifier.
      /// </summary>
      property Id         : String
                            read FId ;

      /// <summary>
      ///   Level resolution (extent divided by pixel width).
      /// </summary>
      property Resolution : Double
                            read FResolution ;

      /// <summary>
      ///   Top-left origin of a level (coordinates of top-left tile corner).
      /// </summary>
      property TopLeft    : TGIS_Point
                            read FTopLeft ;

      /// <summary>
      ///   Tile size of level in pixels.
      /// </summary>
      property TileSize   : TPoint
                           read FTileSize ;

      /// <summary>
      ///   First/Last Row/Column of a tile.
      /// </summary>
      property TilesRange : TRect
                            read FTilesRange ;
  end ;


  /// <summary>
  ///   Encapsulation of tiled image layer (usually web tiles).
  /// </summary>
  TGIS_LayerWebTiles = {$IFDEF OXYGENE} public {$ENDIF}
                       class( TGIS_LayerPixel )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

    bBatch : Boolean ;
      /// <summary>
      ///   List of Url
      /// </summary>
      lstUrl : TStringList ;

      /// <summary>
      ///   levels smaller then this value will not be presented
      /// </summary>
      iStartLevel : Integer ;

      /// <summary>
      ///   Levels bigger then this value will not be presented
      /// </summary>
      iEndLevel : Integer ;

      /// <summary>
      ///   Force starting level. Levels smaller then this value will be forced to
      ///   this value (but to be visible level must be at least iStartLevel)
      /// </summary>
      iStartLevelForced : Integer ;

      /// <summary>
      ///   Level offset. If initial level is not a real zero. Level 0 means that
      ///     whole map is covered in a single tile
      /// </summary>
      iOffsetLevel : Integer ;

      /// <summary>
      ///   Rows offset. If starting row is not 0.
      /// </summary>
      iOffsetRow : Integer ;

      /// <summary>
      ///   Columns offset. If starting column is not 0.
      /// </summary>
      iOffsetColumn : Integer ;

      /// <summary>
      ///   Columns offset. If starting column is not 0.
      /// </summary>
      sFileExtension : String ;

      /// <summary>
      ///   Format type: 0=JPG; 1=PNG
      /// </summary>
      iFormat : Integer ;

      /// <summary>
      ///   tiles numbering scheme: 0=DOWNRIGHT; 1=UPRIGHT
      /// </summary>
      iTilesOrder : Integer ;

      /// <summary>
      ///   Extent of a whole tiling zone (typically whole world);
      ///   used to calculate tiles
      /// </summary>
      extTilesArea : TGIS_Extent ;

      /// <summary>
      ///   Extent of the layer; normally same as extTilesExtent
      /// </summary>
      extLayer : TGIS_Extent ;

      /// <summary>
      ///   Origin point; normally same as extTilesExtent
      /// </summary>
      ptTopLeft : TGIS_Point ;

      /// <summary>
      ///   Single tile size
      /// </summary>
      ptTileSize : TPoint ;

      /// <summary>
      ///   Tile count multiplier
      /// </summary>
      ptTilesCount : TPoint ;

      /// <summary>
      ///   Tiles resolution in PPI
      /// </summary>
      iPpi : Integer ;

      /// <summary>
      ///   If true then cache never expires.
      /// </summary>
      bNoExpiration : Boolean ;

      /// <summary>
      ///   If true then layer is purely offline.
      /// </summary>
      bOffline : Boolean ;

      /// <summary>
      ///   True if file cache enabled.
      /// </summary>
      bFileCache : Boolean ;

      /// <summary>
      ///   Cache folder.
      /// </summary>
      sCacheFolder : String ;

      /// <summary>
      ///   Cache name.
      /// </summary>
      sCacheName : String ;

      /// <summary>
      ///   User Agent string
      /// </summary>
      FUserAgent : String ;

      /// <summary>
      ///   Referer string
      /// </summary>
      FReferer : String ;

      /// <summary>
      ///   User name string
      /// </summary>
      FUserName : String ;

      /// <summary>
      ///   User pass string
      /// </summary>
      FUserPass : String ;

      /// <summary>
      ///   Proxy URL as for ESRI proxy.ashx.
      /// </summary>
      FProxyUrl : String ;


      /// <summary>
      ///   Fetch timeout
      /// </summary>
      iTimeOut : Integer ;

      /// <summary>
      ///   Visible tiles cache
      /// </summary>
      oVisibleTiles : TObject ;

      oExtendedCache : TObject;

      /// <summary>
      ///   Event handle to trace url
      /// </summary>
      FOnUrlLog   : TGetStrProc ;

      /// <summary>
      ///   Maximum allowed working threads
      /// </summary>
      iMaxThreads : Integer ;

      /// <summary>
      ///   Sqlite database config
      /// </summary>
      oDbCfg : TStringList ;

      /// <summary>
      ///   Is Sqlite database active
      /// </summary>
      bIsDbActive : Boolean ;

      /// <summary>
      ///   style for vector tiles
      /// </summary>
      sStyle : String ;

      /// <summary>
      ///   style password to access file
      /// </summary>
      sStylePassword : String ;

      /// <summary>
      ///   sprites for vector tiles
      /// </summary>
      sSprites : String ;

      /// <summary>
      ///   style pixel size factor
      /// </summary>
      dStylePixelSizeFactor : Double ;

      /// <summary>
      ///   styler for vector tiles
      /// </summary>
      oStyler : TObject ;

      /// <summary>
      ///   Is layer progressive
      /// </summary>
      bProgressive : Boolean ;

    protected
      /// <summary>
      ///   List of Levels of Detail (LODs)
      /// </summary>
      lstLod : TList< TGIS_LayerWebTileLod > ;

      /// <summary>
      ///   Internal path set by other layers to direct web tiles
      /// </summary>
      FPathEx : String ;

    private
      function fget_lods      ( const _index : Integer
                              ) : TGIS_LayerWebTileLod;
      function fget_lodscount : Integer ;


    private
      /// <summary>
      ///   Prepare tiles for a current drawing context.
      /// </summary>
      /// <returns>
      ///   tile Lod level
      /// </returns>
      /// <remarks>
      ///   <para>
      ///     Internal cache will be prepared and downloading threads will  be
      ///     started.
      ///   </para>
      ///   <para>
      ///     This obverload is used onl;y if layer is attached to the viewer.
      ///   </para>
      /// </remarks>
      function prepareCache  : Integer ; overload ;

      /// <summary>
      ///   Prepare tiles for fetch.
      /// </summary>
      /// <param name="_width">
      ///   width of a viewer; if -1 then all possible tiles for current LOD
      ///   level and extent will be used
      /// </param>
      /// <param name="_height">
      ///   height of a viewer; if -1 then all possible tiles for current LOD
      ///   level and extent will be used
      /// </param>
      /// <param name="_extent">
      ///   extent of the area (in layer coordinate units!)
      /// </param>
      /// <param name="_level">
      ///   LOD level
      /// </param>
      /// <param name="_computeonly">
      ///   if true then function will not prepare tiles to be fetched but will
      ///   only count how many tiles will be affected
      /// </param>
      /// <returns>
      ///   Number of affected tiles
      /// </returns>
      function prepareCache(
        const _width       : Integer     ;
        const _height      : Integer     ;
        const _extent      : TGIS_Extent ;
        const _level       : Integer     ;
        const _computeonly : Boolean
      ) : Integer ; overload ;

      /// <summary>
      ///   Prepare tiles for fetch.
      /// </summary>
      /// <param name="_ptg">
      ///   point which must be included in fetched layers.
      /// </param>
      /// <param name="_scale">
      ///   scale to be used;
      ///   if 0 then attached viewer zoom will be used;
      ///   if &lt;0 then will be interpreted as a zoom
      ///   if &gt;0 then will be interpreted as a scale
      /// </param>
      /// <returns>
      ///   Number of affected tiles
      /// </returns>
      /// <remarks>
      ///   <para>
      ///     This overload is used mainly for LocateEx() function purposes.
      ///   </para>
      ///   <para>
      ///     Parameter _scale&lt0 (aka zoom) is always meaningful.
      ///     For _scale=0 and _scale&gt;0 the layer must be attached to the
      ///     viewer; otherwise a highest resolution level will be used.
      ///   </para>
      /// </remarks>
      function prepareCache(
        const _ptg   : TGIS_Point ;
        const _scale : Double
      ) : Integer ; overload ;

    protected

      /// <summary>
      ///   Get level index.
      /// </summary>
      /// <param name="_res">
      ///   resolution
      /// </param>
      /// <returns>
      ///   level value
      /// </returns>
      function  getLevel          ( const _res   : Double
                                  ) : Integer ;

      /// <summary>
      ///   Prepare custom LOD table
      /// </summary>
      procedure customLod         ; virtual;

    protected
    {$IFDEF OXYGENE} assembly or protected {$ENDIF} // other functions

      /// <inheritdoc/>
      procedure setUp        ; override;

      /// <summary>
      ///   Process tokens for embedded password tokens.
      /// </summary>
      /// <param name="_token">
      ///   token value
      /// </param>
      /// <returns>
      ///   Expanded value of a token or a token name itself if not handled
      /// </returns>
      function  passwordCallBackW ( const _token : String
                                  ) : String ; virtual;

      /// <inheritdoc/>
      function  getBitmapData     ( const _extent   : TGIS_Extent ;
                                    const _bitmap   : TGIS_Pixels ;
                                    const _width    : Integer ;
                                    const _height   : Integer
                                  ) : Boolean ; override;

      /// <inheritdoc/>
      function  getGridData       ( const _extent       : TGIS_Extent      ;
                                    const _grid         : TGIS_GridArray
                                  ) : Boolean ; override;

    public

      /// <inheritdoc/>
      constructor Create     ; override;

    protected

      procedure doDestroy    ; override;

    public

      /// <inheritdoc/>
      function  PreRecognize      ( const _path     : String ;
                                    var   _new_path : String
                                  ) : Boolean ; override;

      /// <inheritdoc/>
      function  LocateEx          ( const _ptg          : TGIS_Point       ;
                                    var   _rgbMapped    : TGIS_Color       ;
                                    var   _nativesVals  : TGIS_DoubleArray ;
                                    var   _transparency : Boolean          ;
                                    const _pixelsize    : Double
                                  ) : Boolean ; override;


      /// <summary>
      ///   Fetch number of tiles. Useful when ttkwp file set FileCache=True
      /// </summary>
      /// <param name="_extent">
      ///   extent for which tiles will be fetched
      /// </param>
      /// <param name="_startlevel">
      ///   starting LOD level
      /// </param>
      /// <param name="_endlevel">
      ///   ending LOD level
      /// </param>
      /// <returns>
      ///   Number of tiles that were not properly fetched.
      /// </returns>
      /// <remarks>
      ///   Fired busy event has _pos and _end values filled with actual
      ///   number of tiles processed and to be processed.
      /// </remarks>
      function FetchTiles       ( const _extent       : TGIS_Extent      ;
                                  const _startlevel   : Integer          ;
                                  const _endlevel     : Integer
                                ) : Integer ;

      /// <summary>
      ///   Count all tiles covering provided levels and extent. Useful for
      ///   estimating FechTiles() cost.
      /// </summary>
      /// <param name="_extent">
      ///   extent for which tiles will be counted
      /// </param>
      /// <param name="_startlevel">
      ///   starting LOD level
      /// </param>
      /// <param name="_endlevel">
      ///   ending LOD level
      /// </param>
      /// <returns>
      ///   Number of tiles.
      /// </returns>
      function  CountTiles       ( const _extent       : TGIS_Extent      ;
                                   const _startlevel   : Integer          ;
                                   const _endlevel     : Integer
                                 ) : Integer ;
    public

      /// <summary>
      ///   User Agent string.
      /// </summary>
      property UserAgent : String          read  FUserAgent
                                           write FUserAgent ;

      /// <summary>
      ///   Referer string.
      /// </summary>
      property Referer : String            read  FReferer
                                           write FReferer ;

      /// <summary>
      ///   Proxy URL as for ESRI proxy.ashx.
      /// </summary>
      property ProxyUrl  : String          read  FProxyUrl
                                           write FProxyUrl ;

      /// <summary>
      ///   List of LODs (Level of details).
      /// </summary>
      /// <param name="_index">
      ///   index of the LOD starting form 0
      /// </param>
      property Lods[ const _index: Integer ] : TGIS_LayerWebTileLod
                                           read  fget_lods ;
      /// <summary>
      ///   Number of defined LODs (Level of details).
      /// </summary>
      property LodsCount : Integer
                                           read  fget_lodscount ;


      /// <event/>
      /// <summary>
      ///   Will be fired upon any url execution to trace image downloads.
      /// </summary>
      property OnUrlLog : TGetStrProc      read  FOnUrlLog
                                           write FOnUrlLog ;
  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    System.Math,
    System.DateUtils,
    System.IOUtils,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoCsFactory,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoLayerJPG,
    Lider.CG.GIS.GeoLayerPNG,
    Lider.CG.GIS.GeoLayerMVT,
    Lider.CG.GIS.GeoMVTStyler,
    Lider.CG.GIS.GeoLayer,
    Lider.CG.GIS.GeoDbSqlite,
    Lider.CG.GIS.GeoRegistredLayers,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoStreams;
{$ENDIF}

const
  WEBTILES_EPSG_3785_HALF_SIZE  = 20037508.342789244 ;
  WEBTILES_CACHE_SIZE           = 100 ;
  WEBTILES_MAXLEVEL             = 32  ;

  WEBTILES_THREAD_WAITING       = 0 ;
  WEBTILES_THREAD_RUNNING       = 1 ;
  WEBTILES_THREAD_RETRY         = 2 ;
  WEBTILES_THREAD_DONE          = 3 ;
  WEBTILES_THREAD_ERROR         = 4 ;

  WEBTILES_MAX_CACHE_MEMSIZE    = 1024 * 1024 * 10 ; // 10MB of memory cache
  WEBTILES_MIN_CACHE_TIME       = 1 * 60           ; // 1 minute

  WEBTILES_CACHE_EXT            = '.tile' ;
  WEBTILES_CACHEFOLDER_METADATA = 'TGIS_LayerWebTiles.CacheFolder' ;

  WEBTILES_RETRY_COUNT          =  4 ;

type
  T_tilesCacheItem  = class( TGIS_ObjectDisposable )
    public
      Uid            : String    ;
      Url            : String    ;
      ContentStream  : TStream   ;
      ContentType    : String    ;
      ContentExpires : TDateTime ;
      LastAccess     : TDateTime ;
      Prev : T_tilesCacheItem ;
      Next : T_tilesCacheItem ;
    public
      constructor Create ;

    protected
      procedure   doDestroy ; override;
  end ;


  T_tilesCache = class( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      oCacheItems : TDictionary< String, T_tilesCacheItem >;
      oFirstLRU   : T_tilesCacheItem;
      oLastLRU    : T_tilesCacheItem;
      iCacheSize  : Int64 ;
      oLock       : TCriticalSection ;

    private
      procedure addLRU   ( const _item    : T_tilesCacheItem
                          ) ;
      procedure removeLRU( const _item    : T_tilesCacheItem
                         ) ;
    protected
      procedure   doDestroy ; override;

    public
      oParent : TGIS_LayerWebTiles ;
      constructor Create ;

    public
      function    GetUrl( const _uid     : String  ;
                          const _url     : String  ;
                          var   _stream  : TStream ;
                          var   _type    : String
                        ) : Boolean ;
      procedure   PutUrl( const _uid     : String  ;
                          const _url     : String  ;
                          const _stream  : TStream;
                          const _type    : String ;
                          const _expires : TDateTime
                        ) ;
      procedure Clear ;
  end;

  { Sublayers. Each sublayer contain a single tile.
  }
  T_subLayer = class( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      Parent  : TGIS_LayerWebTiles ;
      Layer   : TGIS_LayerPixel ; // created sublayr
      Extent  : TGIS_Extent ;     // tile extent
      Size    : Integer ;         // Suggested tile zied for vecto rendering
      Level   : Integer ;         // tile level
      Col     : Integer ;         // tile number
      Row     : Integer ;         // tile number
      Uid     : String  ;         // item identifier (URL idenpendent)
      Url     : String  ;         // tile URL
      State   : Integer ;         // thread state
      RetryCnt: Integer ;         // thread state
      Drawn   : Boolean ;         // mark that tile alredy drawn
      Used    : Boolean ;         // mark that tile is used by current drawing
      ReRender: Boolean ;         // mark that tile is to be rerendered
      Prev    : T_subLayer ;
      Next    : T_subLayer ;
    protected

      procedure doDestroy ; override;

    public
      constructor Create ;

      // <summary>
      //   Start thread to fetch the tile.
      // </summary>
      procedure Start ;
  end ;

  { Thread to fetch a single tile.
  }
  T_thread = class( TThread )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      oParent : T_subLayer ;
    protected

      // <summary>
      //  Execute tile loading tread.
      //  Read tile, attach data to the sublayer.
      // </summary>
      procedure Execute ; override;
  end ;

  { List of tiles managed as cache.
  }
  T_visibleTiles = class( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      oCacheSublayers : TDictionary<String,T_subLayer>;
      oFirstLRU       : T_subLayer ;
      oLastLRU        : T_subLayer ;
      oLayer          : TGIS_LayerWebTiles ;
      oLock           : TCriticalSection ;

    private
      procedure addLRU   ( const _item    : T_subLayer
                          ) ;

    {$IFDEF OXYGENE} unit {$ENDIF}
      procedure removeLRU( const _item    : T_subLayer
                         ) ;

    public
      // <summary>
      //   Create cache instance.
      // </summary>
      constructor Create  ;

    protected
      procedure doDestroy ; override;

    public
      // <summary>
      //   Prepare cache entry.
      // </summary>
      // <remarks>
      //   Call it before calling Prepare(). Must be paired with EndPrepare().
      // </remarks>
      procedure BeginPrepare( const _batch : Boolean ) ;

      // <summary>
      //   Prepare cache entry.
      // </summary>
      // <remarks>
      //   Call it before calling Prepare(). Must be paired with BeginPrepare().
      // </remarks>
      procedure EndPrepare( const _batch : Boolean; const _cnt : Integer ) ;

      // <summary>
      //   Prepare cache entry.
      //   Entries which already exists will be be only moved up on LRU list
      //   Tiles above ache size limit will be deleted
      // </summary>
      // <param name="_level">
      //   tiles level
      // </param>
      // <param name="_col">
      //   column
      // </param>
      // <param name="_row">
      //   row
      // </param>
      // <param name="_url">
      //   requested url
      // </param>
      // <param name="_ext">
      //   requested extent
      // </param>
      // <param name="_ext">
      //   if true the tiles will be added to queue as a first
      // </param>
      // <remarks>
      //   Must be sorrounded with BeginPrepare()..EndPrepare().
      // </remarks>
      procedure   Prepare      ( const _level    : Integer ;
                                 const _col      : Integer ;
                                 const _row      : Integer ;
                                 const _url      : String  ;
                                 const _ext      : TGIS_Extent ;
                                 const _tile_size: Integer ;
                                 const _priority : Boolean
                               ) ;

      // <summary>
      //   Reset cache. Mark all items as not drawn.
      // </summary>
      procedure   Reset        ;

      // <summary>
      //   Clear cache.
      // </summary>
      procedure   Clear        ;

      // <summary>
      //   Starts bunch (max 2 threads per URL) of waiting items.
      // </summary>
      // <param name="_level">
      //   only tiles on this level will be started
      // </param>
      procedure   Run          ( const _level   : Integer
                               ) ;

      // <summary>
      //   Mark all items marked as not fully downloaded for retry.
      // </summary>
      procedure   Retry        ;

      /// <summary>
      ///   Fills provided _bitmap array with values defined by _extent. If the
      ///   current layer does not fully cover the _extent then values outside
      ///   the layer scope should be left untouched.
      /// </summary>
      /// <param name="_extent">
      ///   extent of the _grid
      /// </param>
      /// <param name="_bitmap">
      ///   allocated bitmap(with/height ratio of the _bitmap should be the
      ///   same as width/height ration of the _extent)
      /// </param>
      function  getBitmapData     ( const _extent   : TGIS_Extent ;
                                    const _bitmap   : TGIS_Pixels ;
                                    const _width    : Integer ;
                                    const _height   : Integer ;
                                    const _level    : Integer
                                  ) : Boolean ; virtual;

      // <summary>
      //   If given point is located in image area, return true and set variables:
      // </summary>
      // <param name="_ptg">
      //   reference point /searching point/;
      //   if the layer has been attached to the Viewer then
      //   expected
      //   _ptg units are in a  Viewer coordinate space;
      //   otherwise expected _ptg units are in a Layer
      //   coordinate space
      // </param>
      // <param name="_nativesVals">
      //   dynamic array of double with original values in
      //   _ptg point (for example R, G, B),
      //   Length(_nativesVals) is equal to number of bands
      // </param>
      // <param name="_rgbMapped">
      //   color in _ptg point after mapping (the same as
      //   _nativesVals if no mapping, in this case
      //   _rgbMapped.Blue = Byte(_nativesVals[0] and so on).
      // </param>
      // <param name="_transparency">
      //   return True if point is transparent
      // </param>
      // <param name="_level">
      //   cache level
      // </param>
      // <returns>
      //   False when point _ptg is outside the image;
      //   True when _ptg is inside the image
      // <.return>
     function Locate            ( const _ptg          : TGIS_Point       ;
                                  var   _rgbMapped    : TGIS_Color       ;
                                  var   _nativesVals  : TGIS_DoubleArray ;
                                  var   _transparency : Boolean          ;
                                  const _level        : Integer
                                ) : Boolean ;
  end ;


{$REGION 'T_tilesCache'}
constructor T_tilesCacheItem.Create ;
begin
  Url            := ''  ;
  ContentStream  := nil ;
  ContentType    := ''  ;
  ContentExpires := DateTimeToUTC( Now ) ;
  LastAccess     := DateTimeToUTC( Now ) ;
  Prev           := nil ;
  Next           := nil ;
end;

procedure T_tilesCacheItem.doDestroy ;
begin
  FreeObject( ContentStream );
  inherited ;
end;

procedure T_tilesCache.doDestroy ;
begin
  Clear ;

  FreeObject( oCacheItems ) ;
  FreeObject( oLock ) ;
end;

constructor T_tilesCache.Create ;
begin
  oCacheItems := TDictionary< String, T_tilesCacheItem >.Create ;
  oLock := TCriticalSection.Create ;
end;

procedure T_tilesCache.addLRU(
  const _item : T_tilesCacheItem
) ;
begin
  _item.Prev := nil ;
  _item.Next := oFirstLRU ;

  if assigned( oFirstLRU ) then
    oFirstLRU.Prev := _item ;
  oFirstLRU := _item ;

  if not assigned( oLastLRU ) then
    oLastLRU := _item ;
end;

procedure T_tilesCache.removeLRU(
  const _item : T_tilesCacheItem
) ;
begin
  if assigned( _item.Prev ) then
    _item.Prev.Next := _item.Next
  else
    oFirstLRU := _item.Next ;

  if assigned( _item.Next ) then
    _item.Next.Prev := _item.Prev
  else
    oLastLRU := _item.Prev;
end;

procedure T_tilesCache.Clear ;
{$IFDEF DCC}
  {$IFNDEF NEXTGEN}
    var
      item : TPair< String, T_tilesCacheItem> ;
  {$ENDIF}
{$ENDIF}
begin
  {$IFNDEF NEXTGEN}
    for item in oCacheItems do begin
      FreeObjectNotNil( item.Value ) ;
    end;
  {$ENDIF}

  oCacheItems.Clear ;
  oFirstLRU   := nil ;
  oLastLRU    := nil ;
  iCacheSize  := 0 ;
end ;

function T_tilesCache.GetUrl(
  const _uid     : String  ;
  const _url     : String  ;
  var   _stream  : TStream ;
  var   _type    : String
) : Boolean ;
var
  item       : T_tilesCacheItem ;
  reftime    : TDateTime        ;
  cache_path : String           ;
  strm       : TGIS_FileStream  ;
  age        : TDateTime        ;
begin
  oLock.Enter ;
  try
    Result := False ;


    {$IFDEF DCC}
      reftime := DateTimeToUTC(Now) - 1/24/60/60 * WEBTILES_MIN_CACHE_TIME ;
    {$ELSE}
      reftime := DateTimeToUTC(Now).AddSeconds( - WEBTILES_MIN_CACHE_TIME ) ;
    {$ENDIF}

    if oCacheItems.TryGetValue( _uid, item ) then begin

      if ( item.ContentExpires >= reftime ) or oParent.bNoExpiration then begin
        Result := True ;
        _stream := item.ContentStream ;
        _type   := item.ContentType ;
        item.LastAccess := DateTimeToUTC(Now) ;

        // move item to the top of LRU change
        removeLRU( item );
        addLRU( item );
      end
      else begin
        iCacheSize := iCacheSize - item.ContentStream.Size ;
        oCacheItems.Remove( _uid ) ;

        _stream := nil ;
        _type   := 'EMPTY' ;

        removeLRU( item );
        FreeObject( item ) ;
      end;
    end
    else
    if oParent.bFileCache then begin
      age := EncodeDateTime( 1900, 1, 1, 0, 0, 0, 0 ) ;


      cache_path := oParent.sCacheFolder +
                    GisEnvironmentInfo.DirSep +
                    oParent.sCacheName +
                    GisEnvironmentInfo.DirSep +
                    _uid.Replace( ':', GisEnvironmentInfo.DirSep ) +
                    WEBTILES_CACHE_EXT ;

      if FileExists( cache_path ) then begin
        {$UNDEF PLATFORMCASE}
        {$IFDEF DCC}
          age := FileDateToDateTime( FileAge( cache_path ) ) ;
          {$DEFINE PLATFORMCASE}
        {$ENDIF}
        {$IFDEF CLR}
          age := ( New System.IO.FileInfo( cache_path ) ).LastWriteTime ;
          {$DEFINE PLATFORMCASE}
        {$ENDIF}
        {$IFDEF JAVA}
          var dt := new java.util.Date( New java.io.File( cache_path ).lastModified() ) ;
          age := New DateTime( dt ) + TimeZone.Local.getOffsetToUTC() ;
          {$DEFINE PLATFORMCASE}
        {$ENDIF}
        {$IFNDEF PLATFORMCASE}
           {$MESSAGE Error 'Platform must be defined'}
        {$ENDIF}
        {$UNDEF PLATFORMCASE}

        age := DateTimeToUTC( age ) ;

        if ( age >= reftime ) or oParent.bNoExpiration   then begin
          Result := True;

          strm := TGIS_FileStream.Create( cache_path, fmOpenRead or fmShareDenyNone ) ;
          try
            _stream := TMemoryStream.Create ;
            _stream.CopyFrom( strm, strm.Size ) ;
          finally
            FreeObject( strm ) ;
          end;


          case oParent.iFormat of
            0 : _type := GIS_CONTENTTYPE_JPG ;
            1 : _type := GIS_CONTENTTYPE_PNG ;
            2 : _type := GIS_CONTENTTYPE_BINARY2 ;
            else begin
              assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
            end;
          end;

          item := T_tilesCacheItem.Create ;

          item.Uid := _uid ;
          item.Url := _url ;
          item.ContentStream  := _stream ;
          item.ContentType    := _type ;
          item.ContentExpires := age ;
          item.LastAccess     := DateTimeToUTC(Now) ;
          if assigned( _stream ) then
            iCacheSize := iCacheSize + _stream.Size ;
          oCacheItems.Add( _uid, item ) ;

          addLRU( item );
        end ;

      end ;
    end ;

    // trim cache
    while iCacheSize > WEBTILES_MAX_CACHE_MEMSIZE do begin
      if not assigned( oLastLRU ) then
        break ;

      item := oLastLRU ;

      iCacheSize := iCacheSize - item.ContentStream.Size ;
      oCacheItems.Remove( item.Uid ) ;
      removeLRU( item );
      FreeObject( item ) ;
    end ;

  finally
    oLock.Leave ;
  end;
end;

procedure T_tilesCache.PutUrl(
  const _uid     : String  ;
  const _url     : String  ;
  const _stream  : TStream ;
  const _type    : String  ;
  const _expires : TDateTime
) ;
var
  item       : T_tilesCacheItem ;
  strm       : TGIS_FileStream  ;
  cache_path : String           ;
  tkn        : TGIS_Tokenizer   ;
begin
  oLock.Enter ;
  try
    if oCacheItems.TryGetValue( _uid, item ) then begin
      if assigned( _stream ) then
        iCacheSize := iCacheSize - _stream.Size ;

      oCacheItems.Remove( _uid ) ;
      removeLRU( item );

      FreeObject( item ) ;
    end
    else begin
      item := T_tilesCacheItem.Create ;

      item.Uid := _uid ;
      item.Url := _url ;
      item.ContentStream  := _stream ;
      item.ContentType    := _type ;
      item.ContentExpires := _expires ;
      item.LastAccess     := DateTimeToUTC(Now) ;
      if assigned( _stream ) then
        iCacheSize := iCacheSize + _stream.Size ;
      oCacheItems.Add( _uid, item ) ;

      if oParent.bFileCache then begin
        cache_path := oParent.sCacheFolder +
                      GisEnvironmentInfo.DirSep +
                      oParent.sCacheName ;
        tkn := TGIS_Tokenizer.Create ;
        try
          tkn.Execute( _uid, [':'] );

          if tkn.Result.Count >0 then
            cache_path := cache_path +
                          GisEnvironmentInfo.DirSep +
                          tkn.Result[0] ;

          if tkn.Result.Count >1 then begin
            cache_path := cache_path +
                          GisEnvironmentInfo.DirSep +
                          tkn.Result[1] ;

            {$UNDEF PLATFORMCASE}
            {$IFDEF DCC}
              ForceDirectories( cache_path ) ;
              {$DEFINE PLATFORMCASE}
            {$ENDIF}
            {$IFDEF CLR}
              System.IO.Directory.CreateDirectory( cache_path ) ;
              {$DEFINE PLATFORMCASE}
            {$ENDIF}
            {$IFDEF JAVA}
              (new java.io.File( cache_path )).mkdirs() ;
              {$DEFINE PLATFORMCASE}
            {$ENDIF}
            {$IFNDEF PLATFORMCASE}
               {$MESSAGE Error 'Platform must be defined'}
            {$ENDIF}
            {$UNDEF PLATFORMCASE}
          end;

          if tkn.Result.Count > 2 then
            cache_path := cache_path +
                          GisEnvironmentInfo.DirSep +
                          tkn.Result[2] ;
        finally
          FreeObject( tkn ) ;
        end;

        cache_path := cache_path + WEBTILES_CACHE_EXT ;

        strm := TGIS_FileStream.Create( cache_path, fmCreate or fmShareDenyNone );
        try
          _stream.Position := 0 ;
          strm.CopyFrom( _stream, _stream.Size );
        finally
          FreeObject( strm ) ;
        end ;

        {$UNDEF PLATFORMCASE}
        {$IFDEF DCC}
          System.IOUtils.TFile.SetLastWriteTimeUtc( cache_path, _expires ) ;
          {$DEFINE PLATFORMCASE}
        {$ENDIF}
        {$IFDEF CLR}
          System.IO.File.SetLastWriteTimeUtc( cache_path, _expires ) ;
          {$DEFINE PLATFORMCASE}
        {$ENDIF}
        {$IFDEF JAVA}
          new java.io.File( cache_path ).setLastModified(
            java.util.Calendar( _expires ).getTime().getTime()
          ) ;
          {$DEFINE PLATFORMCASE}
        {$ENDIF}
        {$IFNDEF PLATFORMCASE}
           {$MESSAGE Error 'Platform must be defined'}
        {$ENDIF}
        {$UNDEF PLATFORMCASE}
      end;

      addLRU( item );
    end;

  finally
    oLock.Leave ;
  end;
end;
{$ENDREGION}

{$REGION 'T_subLayer'}

procedure T_subLayer.doDestroy ;
begin
  FreeObject( Layer  ) ;
  inherited ;
end ;

constructor T_subLayer.Create ;
begin
  RetryCnt := 0 ;
end;

procedure T_subLayer.Start ;
var
  th : T_thread ;
begin
  State := WEBTILES_THREAD_RUNNING ;
  th := T_thread.Create( True ) ;
  th.oParent := self ;
  th.FreeOnTerminate := True ;
  th.Start ;
end ;

{$ENDREGION 'T_tilesCache'}

{$REGION 'T_visibleTiles'}

procedure T_visibleTiles.addLRU(
  const _item : T_subLayer
) ;
begin
  _item.Prev := nil ;
  _item.Next := oFirstLRU ;

  if assigned( oFirstLRU ) then
    oFirstLRU.Prev := _item ;
  oFirstLRU := _item ;

  if not assigned( oLastLRU ) then
    oLastLRU := _item ;
end;

procedure T_visibleTiles.removeLRU(
  const _item : T_subLayer
) ;
begin
  if assigned( _item.Prev ) then
    _item.Prev.Next := _item.Next
  else
    oFirstLRU := _item.Next ;

  if assigned( _item.Next ) then
    _item.Next.Prev := _item.Prev
  else
    oLastLRU := _item.Prev;
end;

constructor T_visibleTiles.Create ;
begin
  inherited ;

  oCacheSublayers := TDictionary< String, T_subLayer >.Create ;
  oLock  := TCriticalSection.Create ;
end ;

procedure T_visibleTiles.doDestroy ;
{$IFDEF DCC}
  {$IFNDEF NEXTGEN}
    var
      itm : TPair< String, T_subLayer > ;
  {$ENDIF}
{$ENDIF}
begin
  {$IFNDEF NEXTGEN}
    for itm in oCacheSublayers do begin
      while itm.Value.State = WEBTILES_THREAD_RUNNING do begin
        Sleep( 100 ) ;
      end ;

      FreeObjectNotNil( itm.Value ) ;
    end ;
  {$ENDIF}

  FreeObject( oCacheSublayers ) ;
  FreeObject( oLock ) ;

  inherited ;
end ;

procedure T_visibleTiles.BeginPrepare(
  const _batch : Boolean
) ;
{$IFDEF DCC}
  var
    itm : TPair< String, T_subLayer > ;
{$ENDIF}
begin
  oLock.Enter ;
  try
    if not _batch then begin
      for itm in oCacheSublayers do begin
        itm.Value.Used := False ;
      end ;
    end;
  finally
    oLock.Leave ;
  end;
end;

procedure T_visibleTiles.EndPrepare(
  const _batch : Boolean ;
  const _cnt   : Integer
) ;
var
  lst : TList<String> ;
  tmp : T_subLayer ;
  str : String ;
begin
  oLock.Enter ;
  try
    lst := TList<String>.Create ;
    try
      if not _batch then begin

        while oCacheSublayers.Count > _cnt * 3 do begin
          str := oLastLRU.Uid ;
          tmp := oLastLRU ;
          removeLRU( oLastLRU );
          {$IFNDEF NEXTGEN}
            FreeObjectNotNil( tmp ) ;
          {$ENDIF}
          oCacheSublayers.Remove( str );
       end ;
      end;

    finally
      FreeObject( lst ) ;
    end;
  finally
    oLock.Leave ;
  end;
end;

procedure T_visibleTiles.Prepare(
  const _level    : Integer ;
  const _col      : Integer ;
  const _row      : Integer ;
  const _url      : String  ;
  const _ext      : TGIS_Extent ;
  const _tile_size: Integer ;
  const _priority : Boolean
) ;
var
  itm : T_subLayer ;
  uid : String     ;
  ext : String ;
begin
  assert( _url <> '' ) ;

  if _level < 0 then
    uid := _url
  else
    uid := IntToStr(_level) + ':' + IntToStr(_col) + ':' + IntToStr(_row) ;

 // Assert( _tile_size  <= 512 ) ;

  if oCacheSublayers.TryGetValue( uid, itm ) then begin
    itm.Used  := True  ;
    itm.Drawn := False ;

    removeLRU( itm );
    addLRU( itm );

    if ( itm.Layer is TGIS_LayerMVTPixel )
       and
       ( itm.Size <> _tile_size )
    then begin
      itm.ReRender := True ;
      itm.Size   := _tile_size ;
      itm.State  := WEBTILES_THREAD_WAITING ;
      if _priority then
        itm.Start ;
    end ;

  end
  else begin
    ext := Self.oLayer.sFileExtension ;

    itm := T_subLayer.Create ;
    itm.Uid     := uid    ;
    itm.Url     := _url   ;
    itm.Level   := _level ;
    itm.Col     := _col   ;
    itm.Row     := _row   ;
    itm.Parent  := oLayer ;
    itm.Extent  := _TGIS_Extent(_ext)   ;
    itm.Size    := _tile_size ;
    itm.Layer   := nil    ;
    itm.Drawn   := False  ;
    itm.Used    := True   ;
    itm.ReRender:= False  ;

    itm.State  := WEBTILES_THREAD_WAITING ;

    oCacheSublayers.Add( itm.Uid, itm );
    addLRU( itm );

    if _priority then
      itm.Start ;
  end ;
end ;

procedure T_visibleTiles.Reset ;
var
  lst : TList<String>;
  {$IFDEF DCC}
    str : String ;
    itm : TPair< String, T_subLayer > ;
  {$ENDIF}
begin
  oLock.Enter ;
  try
    lst := TList<String>.Create ;
    try

      for itm in oCacheSublayers do begin
        if itm.Value.State = WEBTILES_THREAD_WAITING then begin
          removeLRU( itm.Value ) ;
          {$IFNDEF NEXTGEN}
            FreeObjectNotNil( itm.Value ) ;
          {$ENDIF}
          lst.Add( itm.Key );
        end
        else
          itm.Value.Drawn := False ;
      end ;

      for str in lst do begin
        oCacheSublayers.Remove( str );
      end;

    finally
      FreeObject( lst );
    end;
  finally
    oLock.Leave ;
  end;
end ;

procedure T_visibleTiles.Clear ;
var
  lst : TList<String>;
  {$IFDEF DCC}
    str : String ;
    itm : TPair< String, T_subLayer > ;
  {$ENDIF}
begin
  oLock.Enter ;
  try
    lst := TList<String>.Create ;
    try

      for itm in oCacheSublayers do begin
        removeLRU( itm.Value ) ;
        {$IFNDEF NEXTGEN}
          FreeObjectNotNil( itm.Value ) ;
        {$ENDIF}
        lst.Add( itm.Key );
      end ;

      for str in lst do begin
        oCacheSublayers.Remove( str );
      end;

    finally
      FreeObject( lst );
    end;
  finally
    oLock.Leave ;
  end;
end ;

procedure T_visibleTiles.Run(
  const _level   : Integer
) ;
var
  {$IFDEF DCC}
    itm  : TPair< String, T_subLayer > ;
  {$ENDIF}
  irun : Integer    ;
  imax : Integer    ;
begin
  oLock.Enter ;
  try
    irun := 0 ;
    for itm in oCacheSublayers do begin
      if itm.Value.State = WEBTILES_THREAD_RUNNING then
        inc( irun ) ;
    end ;

    imax := oLayer.lstUrl.Count * oLayer.iMaxThreads - irun ;

    for itm in oCacheSublayers do begin
      if imax <= 0 then
        exit ;

      if ( _level >= 0 ) and ( _level <> itm.Value.Level ) then
        continue ;


      if itm.Value.State = WEBTILES_THREAD_WAITING then begin
        dec( imax ) ;
        itm.Value.Start ;
      end
      else
      if itm.Value.State = WEBTILES_THREAD_RETRY then begin
        dec( imax ) ;
        itm.Value.Start ;
      end ;
    end ;
  finally
    oLock.Leave ;
  end;
end ;

procedure T_visibleTiles.Retry ;
{$IFDEF DCC}
  var
    itm  : TPair< String, T_subLayer > ;
{$ENDIF}
begin
  oLock.Enter ;
  try
    for itm in oCacheSublayers do begin
      if itm.Value.State = WEBTILES_THREAD_RETRY then
        itm.Value.State := WEBTILES_THREAD_WAITING ;
    end ;
  finally
    oLock.Leave ;
  end;
end ;

function T_visibleTiles.getBitmapData(
  const _extent   : TGIS_Extent ;
  const _bitmap   : TGIS_Pixels ;
  const _width    : Integer ;
  const _height   : Integer ;
  const _level    : Integer
) : Boolean ;
var
  {$IFDEF DCC}
    itm  : TPair< String, T_subLayer > ;
  {$ENDIF}
  itml  : T_subLayer ;
  ext2  : TGIS_Extent ;
  style : String ;
begin
  oLock.Enter ;
  try
    Result := True ;

    ext2 := _extent ;

    for itm in oCacheSublayers do begin
      itml := itm.Value ;

      if itml.Level <> _level then
        continue ;

      if not GisIsCommonExtent( itml.Extent, ext2 ) then
        continue ;


      if itml.Drawn then
        continue ;

      if itml.State = WEBTILES_THREAD_RETRY then begin
        continue ;
      end;

      if itml.State = WEBTILES_THREAD_ERROR then begin
        continue ;
      end;

      if itml.State = WEBTILES_THREAD_DONE then begin
        if not itml.Drawn then begin
          if assigned( itml.Layer ) and ( itml.Used ) then begin
            style := itml.Layer.Params.Style ;
            itml.Layer.Params.Assign( itml.Parent.Params );
            itml.Layer.Params.Style := style ;
            itml.Layer.CS := itml.Parent.CS ;
            if GisIsCommonExtent(_extent, itml.Layer.Extent) then begin

              itml.Layer.ApplyAntialiasSettings( itml.Parent.Antialias, itml.Parent.AntialiasFilter );
              itml.Layer.AssignedParentLayerInternal( itml.Parent ) ;

              itml.Layer.GetRawBitmap( _extent, _bitmap, _width, _height ) ;
              if not itml.Layer.Progressive then
                itml.Parent.Progressive := False ;
            end;
          end;
        end;
        itml.Drawn := True ;
        continue ;
      end ;

      Result := False ;
    end ;

    if Result then
      exit ;
  finally
    oLock.Leave ;
  end;

  Run( _level ) ;
end ;


function T_visibleTiles.Locate(
  const _ptg          : TGIS_Point       ;
  var   _rgbMapped    : TGIS_Color       ;
  var   _nativesVals  : TGIS_DoubleArray ;
  var   _transparency : Boolean          ;
  const _level        : Integer
) : Boolean ;
var
  {$IFDEF DCC}
    itm  : TPair< String, T_subLayer > ;
  {$ENDIF}
  itml : T_subLayer ;
begin
  oLock.Enter ;
  try
    Result := False ;

    Run( _level ) ;
    for itm in oCacheSublayers do begin
      itml := itm.Value ;

      if itml.Level <> _level then
        continue ;

      if not GisIsPointInsideExtent( _ptg, itml.Extent ) then
        continue ;

      while itml.State <> WEBTILES_THREAD_DONE do begin
        Run( _level ) ;
      end ;

      if not assigned(itml.Layer) then
        continue ;



      Result := itml.Layer.Locate( _ptg, _rgbMapped, _nativesVals,
                                   _transparency
                                 ) ;
      if Result then
        break ;
    end ;
  finally
    oLock.Leave ;
  end;
end ;

{$ENDREGION 'T_visibleTiles'}

{$REGION 'TGIS_LayerWebTileLod'}
constructor TGIS_LayerWebTileLod.Create(
  _id         : String     ;
  _resolution : Double     ;
  _topleft    : TGIS_Point ;
  _tilesize   : TPoint     ;
  _tilesrange : TRect
) ;
begin
  FId         := _id         ;
  FResolution := _resolution ;
  FTopLeft    := _TGIS_Point( _topleft ) ;

  FTileSize   := Point(
                   _tilesize.X,
                   _tilesize.Y
                 ) ;
  FTilesRange := Rect(
                   _tilesrange.Left,
                   _tilesrange.Top,
                   _tilesrange.Right,
                   _tilesrange.Bottom
                 ) ;
end;
{$ENDREGION 'TGIS_LayerWebTileLod'}

{$REGION 'T_thread'}
procedure T_thread.Execute ;
var
  r        : TGIS_HttpResponse ;
  stype    : String ;
  strm     : TStream ;
  bfound   : Boolean ;
  holdstrm : Boolean ;
  {$IFDEF JAVA}
    odb    : TGIS_DbJdbc ;
  {$ELSE}
    odb    : TGIS_DbSqlite ;
  {$ENDIF}
begin
  if oParent.ReRender then begin
    if oParent.Layer is TGIS_LayerMVTPixel then
      TGIS_LayerMVTPixel( oParent.Layer ).RenderSize( oParent.Size, oParent.Parent.oStyler )
    else
      assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;

    oParent.State := WEBTILES_THREAD_DONE ;
    exit ;
  end ;

  bfound   := False ;
  holdstrm := False ;
  try
    try
      strm := nil ;
      if T_tilesCache( oParent.Parent.oExtendedCache ).GetUrl(
        oParent.Uid,
        oParent.Parent.FProxyUrl + oParent.Url,
        strm,
        stype
      ) then
      begin
        bfound   := True ;
        holdstrm := True ;
        strm.Position := 0 ;
      end
      else begin
        bfound := False ;

        if oParent.Parent.bIsDbActive then begin
          strm := nil ;
          {$IFDEF JAVA}
            odb := TGIS_DbJdbc.Create ;
          {$ELSE}
            odb := TGIS_DbSqlite.Create ;
          {$ENDIF}
          try
            odb.InitializeProvider ;
            odb.sqlInitialize( oParent.Parent.oDbCfg, oParent.Parent.oDbCfg ) ;

            odb.sqlConnect( odb.sqlBaseFolder( oParent.Parent ), oParent.Parent.oDbCfg ) ;

            odb.sqlQueryOpen( oParent.Url, 0 ) ;
            try
              if not odb.sqlQueryEof(0) then
                strm := odb.sqlQueryGetBlob( 'tile_data', 0 )
              else begin
                oParent.State := WEBTILES_THREAD_DONE ;
                exit ;
              end;
            finally
              odb.sqlQueryClose(0);
            end;
          finally
            FreeObject( odb ) ;
          end ;

          stype := '' ;
          case oParent.Parent.iFormat of
            0 : stype := GIS_CONTENTTYPE_JPG ;
            1 : stype := GIS_CONTENTTYPE_PNG ;
            2 : stype := GIS_CONTENTTYPE_BINARY2 ;
            else begin
              assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
            end;
          end;
        end
        else begin
          if oParent.Parent.bOffline then begin
            oParent.State := WEBTILES_THREAD_DONE ;
            exit ;
          end ;

          strm := TMemoryStream.Create ;

          stype := '' ;

          r := TGIS_WebUtils.HttpFetch(
                 oParent.Parent.FProxyUrl + oParent.Url,
                 strm,
                 nil,
                 True,
                 oParent.Parent.iTimeOut,
                 oParent.Parent.FUserAgent,
                 oParent.Parent.FReferer,
                 oParent.Parent.FUserName,
                 oParent.Parent.FUserPass
              ) ;

          if r.Status = GIS_HTTP_FETCH_TIMEOUT then begin
            if oParent.RetryCnt >= WEBTILES_RETRY_COUNT then begin
              oParent.State := WEBTILES_THREAD_DONE ;
            end
            else begin
              oParent.State := WEBTILES_THREAD_RETRY ;
              Sleep( 500 );
              inc( oParent.RetryCnt ) ;
            end ;
            exit ;
          end
          else if r.Status =  GIS_HTTP_FETCH_NOTCOMPLETED then begin
            oParent.State := WEBTILES_THREAD_DONE ;
            exit
          end
          else if r.Status =  GIS_HTTP_SERVICEUNAVAILABLE then begin
            oParent.State := WEBTILES_THREAD_DONE ;
            exit
          end
          else if r.Status =  GIS_HTTP_GATEWAYTIMEOUT then begin
            oParent.State := WEBTILES_THREAD_DONE ;
            exit
          end
          else if r.Status =  GIS_HTTP_NOTFOUND then begin
            if Pos ( '://', oParent.Url ) < StringFirst then begin
              oParent.State := WEBTILES_THREAD_DONE ;
              exit ;
            end ;

            if oParent.RetryCnt >= WEBTILES_RETRY_COUNT then begin
              oParent.State := WEBTILES_THREAD_DONE ;
            end
            else begin
              oParent.State := WEBTILES_THREAD_RETRY ; // OSM often returns 404
              Sleep( 500 );
              inc( oParent.RetryCnt ) ;
            end;
            exit ;
          end
          else if r.Status = GIS_HTTP_OK then begin
            stype := r.ContentType ;
          end
          else if r.Status <> GIS_HTTP_OK then begin
            oParent.State := WEBTILES_THREAD_DONE ;
            exit ;
          end ;
        end ;
      end ;
    except
      oParent.State := WEBTILES_THREAD_ERROR ;
      exit ;
    end ;

    if not assigned( oParent.Parent ) then
      exit ;

    case DecodeContentType( stype, True ) of
      TGIS_ContentType.Png :
        oParent.Layer := TGIS_LayerPNG.Create ;
      TGIS_ContentType.Jpg :
        oParent.Layer := TGIS_LayerJPG.Create ;
      TGIS_ContentType.Binary :
        oParent.Layer := TGIS_LayerMVTPixel.Create( oParent.Level ) ;
      else begin
        oParent.State := WEBTILES_THREAD_ERROR ;
        exit  ;
      end ;
    end;

    try
      if not oParent.Parent.bBatch then begin

        oParent.Layer.Stream  := strm ;
        oParent.Layer.Caption := Format( '%d-%d-%d',
                                         [oParent.Level, oParent.Col, oParent.Row]
                                       ) ;
        oParent.Layer.Extent        := oParent.Extent ;
        oParent.Layer.Params.Style  := oParent.Parent.sStyle ;

        {$IFDEF DCC}
          if Assigned( BitmapHelper )
             and
             ( BitmapHelper.ClassName = 'TGIS_BitmapFactoryFMX' )
             and
             ( not System.IsConsole )
          then
            // FMX WND requires synchronization
            Synchronize(
              procedure
              begin
                oParent.Layer.Open ;
              end
            )
          else
            oParent.Layer.Open ;
        {$ELSE}
          oParent.Layer.Open ;
        {$ENDIF}

        oParent.Layer.Extent := oParent.Extent ;
        oParent.Layer.Viewer := oParent.Parent.Viewer ;

        if oParent.Layer is TGIS_LayerMVTPixel then
          TGIS_LayerMVTPixel( oParent.Layer ).RenderSize(
            oParent.Size, oParent.Parent.oStyler
          ) ;

        oParent.State := WEBTILES_THREAD_DONE ;
      end;
    except
      FreeObject( oParent.Layer ) ;
      oParent.State := WEBTILES_THREAD_RETRY ;
      exit ;
    end;
    if not bfound then begin
      holdstrm := True ;
      T_tilesCache( oParent.Parent.oExtendedCache ).PutUrl(
        oParent.Uid,
        oParent.Parent.FProxyUrl + oParent.Url,
        strm,
        r.ContentType,
        r.ContentExpires
      );
    end;


    {$IFDEF GIS_TEST}
      if assigned( oParent.Parent.FOnUrlLog ) then
        oParent.Parent.FOnUrlLog( oParent.Url + ' ['+ IntToStr( r.Status ) + ']' ) ;
    {$ENDIF}

  finally
    if not holdstrm then
      FreeObject( strm ) ;
  end ;

end ;
{$ENDREGION 'T_thread'}

{$REGION 'TGIS_LayerWebTiles'}
constructor TGIS_LayerWebTiles.Create ;
begin
  inherited ;

  oVisibleTiles := T_visibleTiles.Create ;
  oExtendedCache := T_tilesCache.Create ;

  T_tilesCache( oExtendedCache ).oParent  := Self ;


  FSubType := FSubType + [ TGIS_LayerSubType.Persistent ] ;

  FAntialias        := True ;
  FAntialiasFilter  := TGIS_ScalingFilter.Lanczos3 ;
  FIsTiled          := False ;

  lstLod := TList<TGIS_LayerWebTileLod>.Create ;
  lstUrl := TStringList.Create ;

  oDbCfg      := nil ;
  bIsDbActive := False ;

  FUserAgent := GetDefaultUserAgent( 'ttkWP' ) ;
  FUserName  := '' ;
  FUserPass  := '' ;
  FProxyUrl  := '' ;
  FReferer   := '' ;
  oStyler    := nil ;
end ;

procedure TGIS_LayerWebTiles.doDestroy ;
begin
  FreeObject( lstLod  ) ;
  FreeObject( oDbCfg  ) ;
  FreeObject( oStyler ) ;
  FreeObject( lstUrl  ) ;

  FreeObject( oVisibleTiles  ) ;
  FreeObject( oExtendedCache ) ;

  inherited ;
end ;

function TGIS_LayerWebTiles.fget_lods(
  const _index: Integer
) : TGIS_LayerWebTileLod;
begin
  Result := lstLod[ _index ] ;
end;

function TGIS_LayerWebTiles.fget_lodscount
: Integer ;

begin
  Result := lstLod.Count ;
end;

function TGIS_LayerWebTiles.prepareCache
  : Integer ;
var
  w     : Integer     ;
  h     : Integer     ;
  ext   : TGIS_Extent ;
  vz    : Double      ;
  level : Integer     ;
begin
  Result := 0 ;
  w := Viewer.Ref.ViewerParent.ControlCanvasWidth  ;
  h := Viewer.Ref.ViewerParent.ControlCanvasHeight ;

  ext  := Viewer.Ref.ScreenToMapRect(
            Rect( 0, 0,
                  Viewer.Ref.ViewerParent.ControlCanvasWidth,
                  Viewer.Ref.ViewerParent.ControlCanvasHeight
            )
          ) ;
  ext  := self.UnprojectExtent( ext ) ;


  if ( ptTilesCount.X > 0 ) and ( ptTilesCount.Y > 0 ) then begin
    if (ext.XMax - ext.XMin) > 1e-10 then begin
      vz := Min(
              w / (ext.XMax - ext.XMin),
              h / (ext.YMax - ext.YMin)
          ) ;
    end
    else
      vz := Viewer.Ref.ViewerParent.ControlCanvasWidth / 1e-10;
    level := getLevel( vz ) ;

    if level + iStartLevel < ( iStartLevelForced + iOffsetLevel ) then
      level := ( iStartLevelForced + iOffsetLevel ) - iStartLevel ;

    if level < iOffsetLevel then
      exit ;

    if level + iStartLevel > iEndLevel then
      level := iEndLevel - iStartLevel ;

    if level >= lstLod.Count then
      level := lstLod.Count - 1 ;  end
  else
    level := -1 ;

  prepareCache( w, h, ext, level, False ) ;

  Result := level ;
end ;

function TGIS_LayerWebTiles.prepareCache(
  const _ptg   : TGIS_Point;
  const _scale : Double
)  : Integer ;
var
  zoom  : Double      ;
  w     : Integer     ;
  h     : Integer     ;
  ew    : Double      ;
  eh    : Double      ;
  ext   : TGIS_Extent ;
  vz    : Double      ;
  level : Integer     ;

begin
  w := 64 ;
  h := 64 ;
  Result := 0 ;
  zoom := 1 ;
  if _scale = 0  then begin
    if assigned( Viewer ) then
      zoom := Viewer.Ref.Zoom ;
  end
  else
  if _scale > 0 then begin
    if assigned( Viewer ) then begin
      zoom  := Viewer.Ref.Zoom ;
      zoom  := zoom / Viewer.Ref.ScaleAsFloat * _scale ;
    end;
  end
  else
  begin
    zoom := Abs( _scale ) ;
  end;

  ew := w / zoom ;
  eh := w / zoom ;
  ext := GisExtent( _ptg.X - ew/2, _ptg.Y - eh/2, _ptg.X + ew/2, _ptg.Y + eh/2 ) ;

  if ( ptTilesCount.X > 0 ) and ( ptTilesCount.Y > 0 ) then begin
    if (ext.XMax - ext.XMin) > 1e-10 then begin
      vz := Min(
              w / (ext.XMax - ext.XMin),
              h / (ext.YMax - ext.YMin)
          ) ;
    end
    else
      vz := w / 1e-10;
    level := getLevel( vz ) ;

    if level + iStartLevel < ( iStartLevelForced + iOffsetLevel ) then
      level := ( iStartLevelForced + iOffsetLevel ) - iStartLevel ;

    if level < iOffsetLevel then
      exit ;

    if level + iStartLevel > iEndLevel then
      level := iEndLevel - iStartLevel ;

    if level >= lstLod.Count then
      level := lstLod.Count - 1 ;  end
  else
    level := -1 ;

  prepareCache( w, h, ext, level, False ) ;

  Result := level ;
end ;

function TGIS_LayerWebTiles.prepareCache(
  const _width  : Integer     ;
  const _height : Integer     ;
  const _extent : TGIS_Extent ;
  const _level  : Integer     ;
  const _computeonly : Boolean
) : Integer ;
var
  ext         : TGIS_Extent ;
  level       : Integer     ;
  ntilesx     : Integer     ;
  ntilesy     : Integer     ;
  startcol    : Integer     ;
  startrow    : Integer     ;
  endcol      : Integer     ;
  endrow      : Integer     ;
  icnt        : Integer     ;
  x,y         : Integer     ;
  w,h         : Integer     ;
  tile_col    : Integer     ;
  tile_row    : Integer     ;
  tile_ext    : TGIS_Extent {$IFDEF GIS_NORECORDS} := new TGIS_Extent {$ENDIF} ;
  tile_width  : Integer     ;
  tile_height : Integer     ;
  tmp         : String      ;
  url         : String      ;
  tsx         : Double      ;
  tsy         : Double      ;
  lod         : TGIS_LayerWebTileLod ;
  priority    : Boolean     ;
  tile_size   : Integer     ;

  function compute_quad(
    const _level : Integer ;
    const _col   : Integer ;
    const _row   : Integer
  ) : String ;
  var
   xx  : Integer ;
   yy  : Integer ;
   zz  : Integer ;
   str : String  ;
   c   : Char    ;
  begin
    str := '' ;
    for zz := 1 to _level do begin
      xx := FloorS( _col / Power( 2, _level- zz ) ) ;
      yy := FloorS( ( Power( 2, _level ) - 1 - _row ) / Power( 2, _level - zz ) ) ;

      case xx mod 2 of
        0 : case yy mod 2 of
              0 : c := '0' ;
            else
                  c := '2' ;
            end ;
      else
            case yy mod 2 of
              0 : c := '1' ;
            else
                  c := '3' ;
            end ;
      end ;

      str := str + c ;
    end ;
    Result := str ;
  end ;

begin
  Result := 0 ;

  level := _level ;
  tile_col := 0 ;
  tile_row := 0 ;
  tsx      := 0 ;
  tsy      := 0 ;
  w := 0 ;
  h := 0 ;
  ntilesy := 0 ;
  ext  := GisCommonExtent( _extent, Extent ) ;

  if ( ptTilesCount.X > 0 ) and ( ptTilesCount.Y > 0 ) then begin
    lod := lstLod[_level] ;

    ntilesx  := lod.TilesRange.Right  - lod.TilesRange.Left + 1;
    ntilesy  := lod.TilesRange.Bottom - lod.TilesRange.Top  + 1 ;

    tsx := lod.TileSize.X / ( 1 / lod.Resolution ) ;
    tsy := lod.TileSize.Y / ( 1 / lod.Resolution ) ;

    startcol := TruncS( ( ext.XMin - lod.TopLeft.X ) / tsx ) ;
    endcol   := Min( startcol + TruncS( ( ext.XMax - ext.XMin ) / tsx ) + 1,
                  startcol+ntilesx - 1
                ) ;
    startrow := TruncS( -( ext.YMax - lod.TopLeft.Y ) / tsx ) ;
    endrow   := Min( startrow + TruncS( ( ext.YMax - ext.YMin ) / tsx ) + 1,
                 startrow+ntilesy -1
               ) ;
    startcol := Max( startcol, lod.TilesRange.Left   ) ;
    endcol   := Min( endcol  , lod.TilesRange.Right  ) ;
    startrow := Max( startrow, lod.TilesRange.Top    ) ;
    endrow   := Min( endrow  , lod.TilesRange.Bottom ) ;

    if ( _width > 0 ) and ( _height > 0 ) then begin
      // max tiles in a view
      if (endcol-startcol)*(endrow-startrow) >
         ( _width*_height ) / ( lod.TileSize.X/4 * lod.TileSize.Y/4 )
       then
         exit ;
    end ;
  end
  else begin
    // no tiling!

    if ( ( ext.XMax - ext.XMin ) > 0 ) and
       ( ( ext.YMax - ext.YMin ) > 0)  then
    begin
      w := RoundS( _width * (ext.XMax - ext.XMin ) /
                   ( _extent.XMax - _extent.XMin )
                 ) ;
      h := RoundS( (_extent.YMax - _extent.YMin) *
                   ( w / (_extent.XMax - _extent.XMin) )
                 ) ;
    end
    else begin
      w := 0 ;
      h := 0 ;
    end ;

    level := -1 ;
    startcol := 0 ;
    startrow := 0 ;

    endcol := 0 ;
    endrow := 0 ;
  end ;

  Result := ( endcol - startcol + 1 ) * ( endrow - startrow + 1 ) ;

  if _computeonly then exit ;

  icnt := 0 ;
  T_visibleTiles( oVisibleTiles ).BeginPrepare( bBatch ) ;
  try
    // add all tiles to thread pool for download
    for x := startcol to endcol do begin
       for y := startrow to endrow do begin

         if level >= 0 then begin
           //tiling enabled with toilernae of half pixel
           tile_ext.XMin := lstLod[ level ].TopLeft.X + tsx * x ;
           tile_ext.XMax := tile_ext.XMin + tsx + lod.Resolution / 2 ;
           tile_ext.YMax := lstLod[ level ].TopLeft.Y - tsy * y ;
           tile_ext.YMin := tile_ext.YMax - tsy - lod.Resolution / 2 ;

           if not GisIsCommonExtent( ext, tile_ext ) then
             continue ;

           case iTilesOrder of
             0 :  begin
                    tile_col := x ;
                    tile_row := y ;
                  end ;
             1 :  begin
                    tile_col := x ;
                    tile_row := ntilesy - y - 1 ;
                  end ;
             else begin
                    assert( False, 'Untested case' ) ;
                  end ;
           end ;

           tile_width  := ptTileSize.X ;
           tile_height := ptTileSize.Y ;
         end
         else begin
           // tiling disabled
           tile_ext := ext ;

           tile_width  := w ;
           tile_height := h ;
         end ;

         // for selected tile always same url should be selected to
         // allow proper caching
         if lstUrl.Count > 0 then
           tmp := lstUrl[ tile_col mod ( lstUrl.Count  ) ]
         else
           tmp := '' ;

         {$IFDEF DCC}
           if lstLod.Count > 0 then
             tmp := StringReplace( tmp,
                                   '{level}' ,
                                   lstLod[level - iOffsetLevel].Id,
                                   [ rfIgnoreCase, rfReplaceAll ]
                                 ) ;
           tmp := StringReplace( tmp,
                                 '{col}',
                                 IntToStr( tile_col - iOffsetColumn ),
                                 [ rfIgnoreCase, rfReplaceAll ]
                               ) ;
           tmp := StringReplace( tmp,
                                 '{row}',
                                 IntToStr( tile_row - iOffsetRow ),
                                 [ rfIgnoreCase, rfReplaceAll ]
                               ) ;
           tmp := StringReplace( tmp,
                                 '{quads}',
                                 compute_quad(
                                   level    - iOffsetLevel ,
                                   tile_col - iOffsetColumn,
                                   tile_row - iOffsetRow
                                 ),
                                 [ rfIgnoreCase, rfReplaceAll ]
                               ) ;
           tmp := StringReplace( tmp,
                                 '{ext}',
                                 sFileExtension,
                                 [ rfIgnoreCase, rfReplaceAll ]
                               ) ;

           tmp := StringReplace( tmp,
                                 '{width}',
                                 IntToStr( tile_width ),
                                 [ rfIgnoreCase, rfReplaceAll ]
                               ) ;
           tmp := StringReplace( tmp,
                                 '{height}',
                                 IntToStr( tile_height ),
                                 [ rfIgnoreCase, rfReplaceAll ]
                               ) ;
           tmp := StringReplace( tmp,
                                 '{xmin}',
                                 DotFloatToStr( tile_ext.XMin ),
                                 [ rfIgnoreCase, rfReplaceAll ]
                               ) ;
           tmp := StringReplace( tmp,
                                 '{ymin}',
                                 DotFloatToStr( tile_ext.YMin ),
                                 [ rfIgnoreCase, rfReplaceAll ]
                               ) ;
           tmp := StringReplace( tmp,
                                 '{xmax}',
                                 DotFloatToStr( tile_ext.XMax ),
                                 [ rfIgnoreCase, rfReplaceAll ]
                               ) ;
           tmp := StringReplace( tmp,
                                 '{ymax}',
                                 DotFloatToStr( tile_ext.YMax ),
                                 [ rfIgnoreCase, rfReplaceAll ]
                               ) ;
           tmp := StringReplace( tmp,
                                 '{epsg}',
                                 IntToStr( CS.EPSG ),
                                 [ rfIgnoreCase, rfReplaceAll ]
                               ) ;
         {$ENDIF}
         {$IFDEF OXYGENE}
           if lstLod.Count > 0 then
             tmp := StringReplace( tmp,
                                   '{level}' ,
                                   lstLod[level - iOffsetLevel].Id,
                                   [ TReplaceFlag.rfIgnoreCase,
                                     TReplaceFlag.rfReplaceAll ]
                                 ) ;
           tmp := StringReplace( tmp,
                                 '{col}',
                                 IntToStr( tile_col - iOffsetColumn ),
                                 [ TReplaceFlag.rfIgnoreCase,
                                   TReplaceFlag.rfReplaceAll ]
                               ) ;
           tmp := StringReplace( tmp,
                                 '{row}',
                                 IntToStr( tile_row - iOffsetRow ),
                                 [ TReplaceFlag.rfIgnoreCase,
                                   TReplaceFlag.rfReplaceAll ]
                               ) ;
           tmp := StringReplace( tmp,
                                 '{quads}',
                                 compute_quad(
                                   level    - iOffsetLevel ,
                                   tile_col - iOffsetColumn,
                                   tile_row - iOffsetRow
                                 ),
                                 [ TReplaceFlag.rfIgnoreCase,
                                   TReplaceFlag.rfReplaceAll ]
                               ) ;
           tmp := StringReplace( tmp,
                                 '{ext}',
                                 sFileExtension,
                                 [ TReplaceFlag.rfIgnoreCase,
                                   TReplaceFlag.rfReplaceAll ]
                               ) ;

           tmp := StringReplace( tmp,
                                 '{width}',
                                 IntToStr( tile_width ),
                                 [ TReplaceFlag.rfIgnoreCase,
                                   TReplaceFlag.rfReplaceAll ]
                               ) ;
           tmp := StringReplace( tmp,
                                 '{height}',
                                 IntToStr( tile_height ),
                                 [ TReplaceFlag.rfIgnoreCase,
                                   TReplaceFlag.rfReplaceAll ]
                               ) ;
           tmp := StringReplace( tmp,
                                 '{xmin}',
                                 DotFloatToStr( tile_ext.XMin ),
                                 [ TReplaceFlag.rfIgnoreCase,
                                   TReplaceFlag.rfReplaceAll ]
                               ) ;
           tmp := StringReplace( tmp,
                                 '{ymin}',
                                 DotFloatToStr( tile_ext.YMin ),
                                 [ TReplaceFlag.rfIgnoreCase,
                                   TReplaceFlag.rfReplaceAll ]
                               ) ;
           tmp := StringReplace( tmp,
                                 '{xmax}',
                                 DotFloatToStr( tile_ext.XMax ),
                                 [ TReplaceFlag.rfIgnoreCase,
                                   TReplaceFlag.rfReplaceAll ]
                               ) ;
           tmp := StringReplace( tmp,
                                 '{ymax}',
                                 DotFloatToStr( tile_ext.YMax ),
                                 [ TReplaceFlag.rfIgnoreCase,
                                   TReplaceFlag.rfReplaceAll ]
                               ) ;
           tmp := StringReplace( tmp,
                                 '{epsg}',
                                 IntToStr( CS.EPSG ),
                                 [ TReplaceFlag.rfIgnoreCase,
                                   TReplaceFlag.rfReplaceAll ]
                               ) ;

         {$ENDIF}

         if not bIsDbActive then
           url := GetPathAbsolute( GetFileDir( Path ), tmp )
         else
           url := tmp ;

         {$IFDEF OXYGENE}
           url := TemplateProducer( url, nil, @passwordCallBackW, False ) ;
         {$ELSE}
           url := TemplateProducer( url, nil, passwordCallBackW, False ) ;
         {$ENDIF}

         // prioritize cental tile
         priority := ( RoundS( endcol / 2 + startcol / 2 ) = tile_col )
                     and
                     ( RoundS( endrow / 2 + startrow / 2 ) = tile_row ) ;
         tile_size := RoundS( ( tile_ext.XMax - tile_ext.XMin ) /
                              ( _extent.XMax - _extent.XMin ) *
                              Viewer.Ref.ViewerParent.ControlCanvasWidth
                             ) ;
         T_visibleTiles( oVisibleTiles ).Prepare(
                                           level, tile_col, tile_row,
                                           url, tile_ext, tile_size,
                                           priority
                                         ) ;

         inc( icnt ) ;
       end ;
    end ;
  finally
    T_visibleTiles( oVisibleTiles ).EndPrepare( bBatch, icnt ) ;
  end;
end ;

function TGIS_LayerWebTiles.getLevel(
  const _res : Double
) : Integer ;
var
  i        : Integer ;
  rp_zoom  : Double ;
  tz       : Double ;
  prv      : Double ;
  lvl      : Integer ;
begin
  rp_zoom := Viewer.Ref.PPI / iPpi ;

  // calculate level
  lvl := iEndLevel ;

  prv := 1e308 ;
  for i := 0 to lstLod.Count-1 do begin
    tz := 1 / lstLod[i].Resolution ;
    if (_res/rp_zoom) < tz then begin
      lvl := i;
      if ( (_res/rp_zoom) / tz ) < 0.7 then begin // do not use tiles to small
        if i > 0 then
          lvl := Max( 0, lvl -1 ) ;
      end;
      break ;

    end ;
    prv := lstLod[i].Resolution ;
  end ;

  Result := lvl ;
end ;

procedure TGIS_LayerWebTiles.customLod ;
begin
  // just to safe inheritance
end;

function TGIS_LayerWebTiles.passwordCallBackW(
  const _token : String
) : String ;
begin
  if assigned( FOnPassword ) then
    {$IFDEF OXYGENE}
      Result := FOnPassword( oLayer,
                             TGIS_TemplateProducerEventArgs.Create( _token )
                           )
    {$ELSE}
      Result := FOnPassword( oLayer, _token )
    {$ENDIF}
  else
    Result := _token ;
end;

function  TGIS_LayerWebTiles.getBitmapData(
  const _extent   : TGIS_Extent ;
  const _bitmap   : TGIS_Pixels ;
  const _width    : Integer ;
  const _height   : Integer
) : Boolean ;
var
  level : Integer ;
begin
  Result := False ;

  T_visibleTiles( oVisibleTiles ).oLayer := Self ;
  T_visibleTiles( oVisibleTiles ).Reset ;

  level := prepareCache ;

  Result := T_visibleTiles( oVisibleTiles ).getBitmapData(
              _extent, _bitmap,
              _width, _height,
              level
            ) ;
end ;

function  TGIS_LayerWebTiles.getGridData( const _extent       : TGIS_Extent      ;
                                             const _grid         : TGIS_GridArray
                                        ) : Boolean ;
var
  bmp      : TGIS_Pixels ;
  tw, th   : Integer ;
  gg       : Boolean ;
  bandno  : Integer ;
  i, k    : Integer ;
const
  MAX_LINES = 900 ;
  FACTOR_RED = 0.35 ;
  FACTOR_GREEN = 0.45 ;
  FACTOR_BLUE = 0.2  ;
begin
  if FInterpretation = TGIS_LayerPixelInterpretation.Grid then begin
    gg := True ;
    bandno := 0 ;
  end
  else begin
    gg := False ;
    bandno := Params.Pixel.GridBand ;
  end;

  if FMinZ >= FMaxZ then begin
    FMinZ :=  0 ;
    FMaxZ := 255 ;
  end;

  tw := length(_grid[0]) ;
  th := length(_grid) ;
  SetLength(bmp, tw * th ) ;
  FIsGridImage := False ;
  Result := getBitmapData(_extent, bmp, tw, th) ;
  FIsGridImage := True ;
  if not Result then
    exit ;

  if gg then begin
    for i := low(_grid) to high(_grid) do begin
      for k := low(_grid[0]) to high(_grid[0]) do
        if (bmp[i*tw +k] and (Integer($FF000000))) = 0 then
          _grid[i, k] := GIS_GRID_NOVALUE
        else
          _grid[i, k] := (((bmp[i*tw +k] shr 16) and $FF) * FACTOR_RED) +
                         (((bmp[i*tw +k] shr 08) and $FF) * FACTOR_GREEN) +
                         (((bmp[i*tw +k] shr 00) and $FF) * FACTOR_BLUE) ;
    end;
  end
  else begin
    case bandno of
      1 :
        begin
          for i := low(_grid) to high(_grid) do begin
            for k := low(_grid[0]) to high(_grid[0]) do
              if (bmp[i*tw +k] and (Integer($FF000000))) = 0 then
                _grid[i, k] := GIS_GRID_NOVALUE
              else
                _grid[i, k] := (((bmp[i*tw +k] shr 16) and $FF) * FACTOR_RED) ;
          end;
        end;
      2 :
        begin
          for i := low(_grid) to high(_grid) do begin
            for k := low(_grid[0]) to high(_grid[0]) do
              if (bmp[i*tw +k] and (Integer($FF000000))) = 0 then
                _grid[i, k] := GIS_GRID_NOVALUE
              else
                _grid[i, k] := (((bmp[i*tw +k] shr 08) and $FF) * FACTOR_GREEN) ;
          end;
        end;
      3 :
        begin
          for i := low(_grid) to high(_grid) do begin
            for k := low(_grid[0]) to high(_grid[0]) do
              if (bmp[i*tw +k] and (Integer($FF000000))) = 0 then
                _grid[i, k] := GIS_GRID_NOVALUE
              else
                _grid[i, k] := (((bmp[i*tw +k] shr 00) and $FF) * FACTOR_BLUE) ;
          end;
        end;
    end;
  end;
  SetLength(bmp, 0) ;
end;


function TGIS_LayerWebTiles.FetchTiles(
  const _extent      : TGIS_Extent    ;
  const _startlevel  : Integer        ;
  const _endlevel    : Integer
) : Integer ;
var
  i         : Integer ;
  cnt       : Integer ;
  ready     : Boolean ;
  noexprtn  : Boolean ;
  filecache : Boolean ;
  offln     : Boolean ;
  {$IFDEF DCC}
    itm  : TPair< String, T_subLayer > ;
  {$ENDIF}
  itml : T_subLayer ;
begin
  filecache := bFileCache ;
  noexprtn  := bNoExpiration ;
  offln     := bOffline ;
  try
    RaiseBusyPrepare( Self, _rsrc( GIS_RS_BUSY_DATA_LOAD ) ) ;
    bBatch        := True  ;
    bFileCache    := True  ;
    bNoExpiration := True  ;
    bOffline      := False ;

    cnt := 0 ;
    for i := _startlevel to _endlevel do
      cnt := cnt + prepareCache( -1, -1, _extent, i, False ) ;

    Result := cnt ;

    RaiseBusyShake( self, 0, cnt ) ;

    T_visibleTiles( oVisibleTiles ).Run( -1 ) ;

    ready := False ;
    while not ready do begin

      T_visibleTiles( oVisibleTiles ).oLock.Enter ;
      try
        ready := True ;
        for itm in T_visibleTiles( oVisibleTiles ).oCacheSublayers do begin
          itml := itm.Value ;

          if itml.Drawn then
            continue ;

          if itml.State = WEBTILES_THREAD_RETRY then begin
            continue ;
          end;

          if itml.State = WEBTILES_THREAD_ERROR then begin
            continue ;
          end;


          if itml.State = WEBTILES_THREAD_DONE then begin
            if RaiseBusyShake(
                 self,
                 cnt - T_visibleTiles( oVisibleTiles ).oCacheSublayers.Count,
                 cnt
               )
            then
              Abort ;
            T_visibleTiles( oVisibleTiles ).removeLRU( itml ) ;
            T_visibleTiles( oVisibleTiles ).oCacheSublayers.Remove( itml.Uid );
            FreeObject( itml );
            continue ;
          end ;

          ready := False ;
        end ;
      finally
        T_visibleTiles( oVisibleTiles ).oLock.Leave ;
      end;

      T_visibleTiles( oVisibleTiles ).Run( -1 ) ;
    end
  finally
    RaiseBusyShake( self, -1, -1 ) ;
    RaiseBusyRelease( Self ) ;

    bBatch        := False ;
    bOffline      := offln     ;
    bFileCache    := filecache ;
    bNoExpiration := noexprtn  ;

    Result := T_visibleTiles( oVisibleTiles ).oCacheSublayers.Count ;
  end;
end;

function TGIS_LayerWebTiles.CountTiles(
  const _extent      : TGIS_Extent ;
  const _startlevel  : Integer     ;
  const _endlevel    : Integer
) : Integer ;
var
  i : Integer ;
begin
  Result := 0 ;

  for i := _startlevel to _endlevel do
    Result := Result + prepareCache( -1, -1, _extent, i, True ) ;
end;


procedure TGIS_LayerWebTiles.setUp ;
var
  i           : Integer ;
  url1        : String  ;
  url2        : String  ;
  url3        : String  ;
  url4        : String  ;
  epsg        : Integer ;
  sname       : String  ;
  svalue      : String  ;
  stilesorder : String  ;
  lst         : TStringList ;
  lod         : TGIS_LayerWebTileLod ;
  dx, dy      : Double ;
  fld_name    : String ;
  fld_value   : String ;
  sparams     : TArray<String> ;
  ext         : TGIS_Extent ;
  res         : Double ;
  rct         : TRect ;
  dir         : String ;
  style_pass  : String ;

  {$IFDEF JAVA}
    odb       : TGIS_DbJdbc ;
  {$ELSE}
    odb       : TGIS_DbSqlite ;
  {$ENDIF}

  procedure do_raise( const _name, _value : String ) ;
  begin
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPARAM ),
                                 _name + '=' + _value, 0
                               ) ;
  end ;

  function _c( const _value : String ) : Boolean ;
  begin
    Result := CompareText( svalue, _value ) = 0  ;
  end ;

  procedure prepare_cache_name ;
  var
    c     : Char    ;
    i     : Integer ;
    state : Integer ;
    stmp  : String  ;
    bld   : TStringBuilder ;
  begin
    stmp := LowerCase( lstUrl[0] ) ;

    stmp := StringReplace( stmp, 'http://' , '', [] ) ;
    stmp := StringReplace( stmp, 'https://', '', [] ) ;
    stmp := StringReplace( stmp, 'file://' , '', [] ) ;

    bld := TStringBuilder.Create ;
    try
      // remove any special character and macros like {level}
      state := 0 ;
      for i := StringFirst to StringLast( stmp ) do begin
        c := stmp[i] ;

        case state of
          0 : begin
                if      c = '{' then state := 1
                else if c = '?' then break
                else if c in ['a'..'z', '0'..'9', '.' ]
                                then bld.Append( c )
                else                 bld.Append( '_' ) ;
              end ;
          1 : begin
                if      c = '}' then state := 0 ;
              end ;
        end ;
      end;

      // trailin underscores
      state := 0 ;
      for i := bld.Length -1 downto 0 do begin
        if      bld.Chars[i] = '_' then
                bld.Length := bld.Length - 1
        else if bld.Chars[i] = '.' then
                bld.Length := bld.Length - 1
        else    break ;
      end;

      sCacheName := bld.ToString ;
    finally
      FreeObject( bld ) ;
    end;
  end;


begin
  {$IFDEF GIS_NORECORDS}
    extTilesArea := new TGIS_Extent ;
    extLayer     := new TGIS_Extent ;
    ptTopLeft    := new TGIS_Point  ;
  {$ENDIF}

  FBandsCount := 4 ;
  bIsDbActive := False ;

  try
    lst := TStringList.Create ;
    try
      if not IsStringEmpty( FPathEx ) then
        ReadParamsFromPath( FPathEx, lst )
      else
        ReadParamsFromPath( Path, lst ) ;

      FUserAgent := LstReadString ( lst,
                                    'UserAgent', FUserAgent
                                  ) ;
      FReferer   := LstReadString ( lst,
                                    'Referer', FReferer
                                  ) ;
      FUserName  := LstReadString ( lst,
                                    'User', FUserName
                                  ) ;
      FUserPass  := LstReadString ( lst,
                                    'Pass', FUserPass
                                  ) ;

      FProxyUrl  := LstReadString( lst, 'ProxyUrl', FProxyUrl ) ;
      if not IsStringEmpty( FProxyUrl ) then begin
        if FProxyUrl[ StringLast( FProxyUrl ) ] <> '?' then
          FProxyUrl := FProxyUrl + '?' ;
      end ;

      iTimeOut   := LstReadInteger( lst,
                                    'TimeOut', 20000
                                  ) ;
      iMaxThreads := LstReadInteger( lst,
                                    'MaxThreads', 32
                                  ) ;
      if iMaxThreads <= 0 then
        iMaxThreads := 8 ;

      epsg              := 3785 ;
      stilesorder       := 'DOWNRIGHT' ;
      iStartLevel       := 0    ;
      iStartLevelForced := 0    ;
      iEndLevel         := 20   ;

      ptTileSize        := Point( 256, 256 ) ;
      ptTilesCount      := Point(   1,   1 ) ;

      Name    := LstReadString( lst, 'Name'   , Name    ) ;
      Caption := LstReadString( lst, 'Caption', Caption ) ;


      url1    := URLFixed( LstReadString( lst, 'Url1'   , '' ) ) ;
      url2    := URLFixed( LstReadString( lst, 'Url2'   , '' ) ) ;
      url3    := URLFixed( LstReadString( lst, 'Url3'   , '' ) ) ;
      url4    := URLFixed( LstReadString( lst, 'Url4'   , '' ) ) ;

      sname := 'Space' ;
      svalue := LstReadString( lst, sname, 'WORLDMERCATOR' ) ;
      if _c( 'WGS' ) then begin
        extTilesArea.XMin := -180 ;
        extTilesArea.YMin := - 90 ;
        extTilesArea.XMax :=  180 ;
        extTilesArea.YMax :=   90 ;
      end
      else if _c( 'WORLDMERCATOR' ) then begin
        extTilesArea.XMin := -WEBTILES_EPSG_3785_HALF_SIZE ;
        extTilesArea.YMin := -WEBTILES_EPSG_3785_HALF_SIZE ;
        extTilesArea.XMax :=  WEBTILES_EPSG_3785_HALF_SIZE ;
        extTilesArea.YMax :=  WEBTILES_EPSG_3785_HALF_SIZE ;
      end
      else if _c( 'CUSTOM' ) then begin
        extTilesArea.XMin := GisNoWorld.XMin ;
        extTilesArea.YMin := GisNoWorld.YMin ; ;
        extTilesArea.XMax := GisNoWorld.XMax ; ;
        extTilesArea.YMax := GisNoWorld.XMax ; ;
      end
      else
        do_raise( sname, svalue ) ;

      extTilesArea.XMin := LstReadFloat( lst, 'XMin'       , extTilesArea.XMin ) ;
      extTilesArea.XMax := LstReadFloat( lst, 'XMax'       , extTilesArea.XMax ) ;
      extTilesArea.YMin := LstReadFloat( lst, 'YMin'       , extTilesArea.YMin ) ;
      extTilesArea.YMax := LstReadFloat( lst, 'YMax'       , extTilesArea.YMax ) ;

      extLayer.XMin     := LstReadFloat( lst, 'Extent.XMin', extTilesArea.XMin ) ;
      extLayer.XMax     := LstReadFloat( lst, 'Extent.XMax', extTilesArea.XMax ) ;
      extLayer.YMin     := LstReadFloat( lst, 'Extent.YMin', extTilesArea.YMin ) ;
      extLayer.YMax     := LstReadFloat( lst, 'Extent.YMax', extTilesArea.YMax ) ;

      ptTopLeft.X       := LstReadFloat( lst, 'TopLeft.X'  , extTilesArea.XMin ) ;
      ptTopLeft.Y       := LstReadFloat( lst, 'TopLeft.Y'  , extTilesArea.YMax ) ;

      sname  := 'Format' ;
      svalue := LstReadString( lst, sname, 'jpg' ) ;
      if      _c( 'JPG' )           then iFormat := 0
      else if _c( 'PNG' )           then iFormat := 1
      else if _c( 'PBF' )           then iFormat := 2
      else if _c( 'MVT' )           then iFormat := 2
      else    do_raise( sname, svalue ) ;

      sFileExtension    := svalue ;

      sname  := 'Type' ;
      svalue := LstReadString( lst, sname, 'CUSTOM' ) ;
      if _c( 'CUSTOM' ) then begin
        // nothing special
      end
      else if _c( 'TMS' ) then begin
        iStartLevel       := 0  ;
        iStartLevelForced := 0  ;
        iEndLevel         := 17 ;
        ptTileSize.X      := 256 ;
        ptTileSize.Y      := 256 ;

        if not IsStringEmpty( url1 ) then
          url1 := url1 + '{level}/{col}/{row}.{ext}' ;
        if not IsStringEmpty( url2 ) then
          url2 := url2 + '{level}/{col}/{row}.{ext}' ;
        if not IsStringEmpty( url3 ) then
          url3 := url3 + '{level}/{col}/{row}.{ext}' ;
        if not IsStringEmpty( url4 ) then
          url4 := url4 + '{level}/{col}/{row}.{ext}' ;
      end
      else if _c( 'GOOGLE' ) then begin
        stilesorder       := 'UPRIGHT' ;

        iStartLevel       := 0  ;
        iStartLevelForced := 0  ;
        iEndLevel         := 17 ;
        ptTileSize.X      := 256 ;
        ptTileSize.Y      := 256 ;

        if not IsStringEmpty( url1 ) then
          url1 := url1 + '{level}-{col}-{row}.{ext}' ;
        if not IsStringEmpty( url2 ) then
          url2 := url2 + '{level}-{col}-{row}.{ext}' ;
        if not IsStringEmpty( url3 ) then
          url3 := url3 + '{level}-{col}-{row}.{ext}' ;
        if not IsStringEmpty( url4 ) then
          url4 := url4 + '{level}-{col}-{row}.{ext}' ;
      end
      else if _c( 'GOOGLE2' ) then begin // ilker ekleme baþlama
        stilesorder       := 'DOWNRIGHT' ;

        iStartLevel       := 0  ;
        iStartLevelForced := 0  ;
        iEndLevel         := 19 ;
        ptTileSize.X      := 256 ;
        ptTileSize.Y      := 256 ;

        if not IsStringEmpty( url1 ) then
          url1 := url1 + '&x={col}&y={row}&z={level}';
        if not IsStringEmpty( url2 ) then
          url2 := url2 + '&x={col}&y={row}&z={level}';
        if not IsStringEmpty( url3 ) then
          url3 := url3 + '&x={col}&y={row}&z={level}';
        if not IsStringEmpty( url4 ) then
          url4 := url4 + '&x={col}&y={row}&z={level}';

      end // ilker ekleme bitiþ
      else if _c( 'MBTILES' ) then begin
        iStartLevel       := 0  ;
        iStartLevelForced := 0  ;
        iEndLevel         := 17 ;
        ptTileSize.X      := 256 ;
        ptTileSize.Y      := 256 ;
        stilesorder       := 'UPRIGHT' ;

        {$IFDEF JAVA}
          odb := TGIS_DbJdbc.Create ;
        {$ELSE}
          odb := TGIS_DbSqlite.Create ;
        {$ENDIF}
        try
          odb.InitializeProvider ;

          oDbCfg := TStringList.Create ;
          {$IFDEF JAVA}
            oDbCfg.Values[ GIS_INI_LAYERSQL_CONNECTOR_JDBC  ] := Format( 'jdbc:sqlite:%s', [url1] ) ;
            oDbCfg.Values[ GIS_INI_LAYERSQL_CONNECTOR_DRIVER] := 'org.sqlite.JDBC' ;
          {$ELSE}
            oDbCfg.Values[ GIS_INI_LAYERSQL_CONNECTOR_SQLITE ] := url1 ;
          {$ENDIF}
          oDbCfg.Text := oDbCfg.Text + #13#10 + GetSQLDialect( GIS_SQL_DIALECT_NAME_SQLITE ) ;
          odb.sqlInitialize( oDbCfg, oDbCfg ) ;

          odb.sqlConnect( odb.sqlBaseFolder(self), oDbCfg ) ;
          bIsDbActive := True ;

          odb.sqlQueryOpen( 'SELECT name, value FROM metadata', 0 ) ;
          try
            while not odb.sqlQueryEof(0) do begin
              fld_name   := VarToString( odb.sqlQueryGetFieldById(0,0) ) ;
              fld_value  := VarToString( odb.sqlQueryGetFieldById(1,0) ) ;

              if  fld_name = 'format' then begin
                svalue := fld_value ;
                if      _c( 'JPG' ) then iFormat := 0
                else if _c( 'PNG' ) then iFormat := 1
                else if _c( 'PBF' ) then iFormat := 2
                else begin
                        assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
                end;
              end
              else if fld_name = 'minzoom' then begin
                iStartLevel := StrToInt( fld_value ) ;
                iStartLevelForced := iStartLevel ;
              end
              else if fld_name = 'maxzoom' then
                iEndLevel := StrToInt( fld_value )
              else if fld_name = 'bounds'  then begin
                {$IFDEF JAVA OR ISLAND}
                  sparams := fld_value.Split( ',' ).ToArray ;
                {$ELSE}
                  sparams := fld_value.Split( [','] ) ;
                {$ENDIF}
                if length( sparams ) > 3 then begin
                  ext := GisExtent( DotStrToFloat( sparams[0] ),
                                    DotStrToFloat( sparams[1] ),
                                    DotStrToFloat( sparams[2] ),
                                    DotStrToFloat( sparams[3] )
                                  ) ;
                  extLayer := TGIS_CSFactory.ByEPSG(4326).ExtentToCS(
                                TGIS_CSFactory.ByEPSG(epsg), ext
                              ) ;
                end ;
              end ;
              odb.sqlQueryMoveNext(0) ;
            end ;
          finally
            odb.sqlQueryClose(0) ;
          end ;
        finally
          FreeObject( odb ) ;
        end ;

        url1 := 'select tile_data from tiles where zoom_level={level} ' +
                'and tile_column={col} and tile_row={row}' ;
      end
      else
        do_raise( sname, svalue ) ;

      sname  := 'TilesOrder' ;
      svalue := LstReadString( lst, sname, stilesorder ) ;
      if      _c( 'DOWNRIGHT' ) then iTilesOrder := 0
      else if _c( 'UPRIGHT'   ) then iTilesOrder := 1
      else    do_raise( sname, svalue ) ;

      iStartLevel       := LstReadInteger( lst,
                                           'StartLevel',
                                           iStartLevel
                                         ) ;
      iStartLevelForced := LstReadInteger( lst,
                                           'StartLevelForced',
                                           iStartLevelForced
                                         ) ;
      iEndLevel         := LstReadInteger( lst,
                                           'EndLevel',
                                           iEndLevel
                                         ) ;

      ptTileSize.X      := LstReadInteger( lst,
                                           'TileWidth',
                                           ptTileSize.X
                                         ) ;
      ptTileSize.Y      := LstReadInteger( lst,
                                           'TileHeight',
                                           ptTileSize.X
                                         ) ;

      ptTilesCount.X    := LstReadInteger( lst,
                                           'TilesColumns'  ,
                                           1
                                         ) ;
      ptTilesCount.Y    := LstReadInteger( lst,
                                           'TilesRows',
                                           1
                                         ) ;
      iPpi              := LstReadInteger( lst,
                                           'Dpi',
                                           96
                                         ) ;
      iPpi              := LstReadInteger( lst,
                                           'PPI',
                                           iPpi
                                         ) ;

      iOffsetLevel      := LstReadInteger( lst,
                                           'OffsetLevel',
                                           0
                                         ) ;
      iOffsetColumn     := LstReadInteger( lst,
                                           'OffsetColumn',
                                           0
                                         ) ;
      iOffsetRow        := LstReadInteger( lst,
                                           'OffsetRow',
                                           0
                                         ) ;

      bOffline          := LstReadBoolean( lst,
                                           'Offline',
                                           False
                                         ) ;
      bFileCache        := LstReadBoolean( lst,
                                           'FileCache',
                                           bOffline
                                         ) ;
      bNoExpiration    := LstReadBoolean( lst,
                                           'NoExpiration',
                                           bOffline
                                         ) ;
      sStyle           := LstReadString( lst, 'Style', '' ) ;
      sStylePassword   := LstReadString( lst, 'StylePassword', '' ) ;
      dStylePixelSizeFactor := DotStrToFloat( LstReadString( lst, 'PixelSizeFactor', '1' ) ) ;

      if not IsStringEmpty( sStyle ) then begin
        dir  := GetFileDir( Path ) ;

        oStyler := TGIS_MVTStyler.Create ;
        TGIS_MVTStyler(oStyler).PixelSizeFactor := dStylePixelSizeFactor ;
        TGIS_MVTStyler(oStyler).ParseStyle( GetPathAbsolute( dir, sStyle ) ) ;
        {$IFDEF OXYGENE}
          style_pass := TemplateProducer( sStylePassword, nil, @passwordCallBackW, False ) ;
        {$ELSE}
          style_pass := TemplateProducer( sStylePassword, nil, passwordCallBackW, False ) ;
        {$ENDIF}

        sSprites := LstReadString( lst, 'Sprites', TGIS_MVTStyler(oStyler).SpritesUrl ) ;
        TGIS_MVTStyler(oStyler).ParseSprites( sSprites, style_pass ) ;
      end ;

      {$UNDEF PLATFORMCASE}
      {$IFDEF DCC}
        sCacheFolder := TPath.GetDocumentsPath ;
        {$DEFINE PLATFORMCASE}
      {$ENDIF}
      {$IFDEF CLR}
        sCacheFolder := Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments);
        {$DEFINE PLATFORMCASE}
      {$ENDIF}
      {$IFDEF JAVA}
        sCacheFolder := javax.swing.filechooser.FileSystemView.FileSystemView.getFileSystemView().getDefaultDirectory().getPath(); ;
        {$DEFINE PLATFORMCASE}
      {$ENDIF}
      {$IFNDEF PLATFORMCASE}
         {$MESSAGE Error 'Platform must be defined'}
      {$ENDIF}
      {$UNDEF PLATFORMCASE}

      sCacheFolder := sCacheFolder +
                      GisEnvironmentInfo.DirSep +
                      'LicadGIS' +
                      GisEnvironmentInfo.DirSep +
                      'WebTilesCache' ; // ilker deðiþtirme

      sCacheFolder := GisMetadataAsString(
                        WEBTILES_CACHEFOLDER_METADATA,
                        sCacheFolder
                      ) ;

      sCacheFolder := LstReadString( lst,
                                     'CacheFolder',
                                     sCacheFolder
                                   ) ;

      // construct url list
      lstUrl.Clear ;

      if not IsStringEmpty( url1 ) then lstUrl.Add( url1 ) ;
      if not IsStringEmpty( url2 ) then lstUrl.Add( url2 ) ;
      if not IsStringEmpty( url3 ) then lstUrl.Add( url3 ) ;
      if not IsStringEmpty( url4 ) then lstUrl.Add( url4 ) ;

      if lstUrl.Count < 1 then
        do_raise( 'Url1', '' ) ;

      prepare_cache_name ;
      sCacheName := LstReadString( lst,
                                     'CacheName',
                                     sCacheName
                                   ) ;

      FBitWidth  := Min(
                      RoundS( ptTileSize.X * Power( 2, 15 ) ),
                      RoundS( ptTileSize.X * Power( 2, iEndLevel ) )
                    ) ;
      FBitHeight := Min(
                      RoundS( ptTileSize.Y * Power( 2, 15 ) ),
                      RoundS( ptTileSize.Y * Power( 2, iEndLevel ) )
                    ) ;
      inherited ;

      bProgressive     := LstReadBoolean( lst,
                                          'Progressive',
                                           Progressive
                                         ) ;
      // false fixes transparency issue of translucent vector tiles
      Progressive := bProgressive ;

      CS := TGIS_CSFactory.ByEPSG( LstReadInteger( lst, 'EPSG', epsg ) ) ;

      Extent := extLayer ;
      dx := extLayer.XMax -extLayer.XMin ;
      dy := extLayer.YMax -extLayer.YMin ;

      if dx > dy then begin
        if (dy/dx) < 0.95  then begin
          FBitHeight := RoundS((dy/dx)*FBitHeight) ;
        end;
      end
      else begin
        if (dx/dy) < 0.95  then begin
          FBitWidth := RoundS((dx/dy)*FBitWidth) ;
        end;
      end;

      RecalcProjectedExtent ;

      FFileInfo := 'Web Tiles' ;

      svalue := LstReadString( lst, 'Info', '' ) ;
      if not IsStringEmpty( svalue ) then
        FFileInfo := FFileInfo +
                     #13#10 + #13#10 +
                     svalue ;

      svalue := LstReadString( lst, 'Copyright', '' ) ;
      if not IsStringEmpty( svalue ) then
        FFileInfo := FFileInfo +
                     #13#10 + #13#10 +
                     svalue ;

      FFileCopyright := svalue ;

      customLod ;

      // custom Lod
      if lstLod.Count = 0 then begin
        for i := 0 to 99 do begin

          res := LstReadFloat( lst, Format( 'Lod%d.Resolution', [i] ), 0 ) ;
          if res = 0 then
            break ;

          lod := TGIS_LayerWebTileLod.Create(
                   IntToStr( i ),
                   res,
                   ptTopLeft,
                   ptTileSize,
                   Rect( 0,
                         0,
                         TruncS( Abs( extTilesArea.XMax - ptTopLeft.X )
                                 / res / ptTileSize.X
                               ),
                         TruncS( Abs( extTilesArea.YMin - ptTopLeft.Y )
                                 / res / ptTileSize.X
                               )
                       )
                 );


          lstLod.Add( lod ) ;
        end;
      end ;

      // default Lod
      if (lstLod.Count = 0) and (ptTilesCount.X > 0) and (ptTilesCount.Y > 0) then begin
        for i := iStartLevel to iEndLevel do begin
          if ((i * ptTilesCount.X - 1) > 31) or
             ((i * ptTilesCount.Y - 1) > 31) then begin
             iEndLevel := i - 1 ;
             break ;
          end ;

          rct := Rect( 0,
                       0,
                       1 shl i * ptTilesCount.X - 1,
                       1 shl i * ptTilesCount.Y - 1
                 ) ;
          lod := TGIS_LayerWebTileLod.Create(
                   IntToStr(i),
                   ( ( extTilesArea.XMax - extTilesArea.XMin )
                     /
                     ( 1.0*ptTileSize.X
                       *
                       ( rct.Right - rct.Left + 1 )
                     )
                   ),
                   GisPoint( extTilesArea.XMin, extTilesArea.YMax ),
                   ptTileSize,
                   rct
                 ) ;

          lstLod.Add( lod ) ;
        end;
      end;
    finally
      FreeObject( lst ) ;
    end ;
    T_tilesCache( oExtendedCache ).Clear  ;
    T_visibleTiles( oVisibleTiles ).Clear ;
  except
    on ex : Exception do begin
      if FileExists( Path ) then
        svalue := Path
      else if not IsStringEmpty( Name ) then
        svalue := Name
      else if not IsStringEmpty( Caption ) then
        svalue := Name
      else
        svalue := GetClassName( Self ) ;

      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_LAYERBADFORMAT ),
        svalue,
        0,
        ex
      ) ;

    end ;
  end ;

end ;

function TGIS_LayerWebTiles.LocateEx(
  const _ptg          : TGIS_Point       ;
  var   _rgbMapped    : TGIS_Color       ;
  var   _nativesVals  : TGIS_DoubleArray ;
  var   _transparency : Boolean          ;
  const _pixelsize    : Double
) : Boolean ;
var
  level : Integer ;
begin
  Result := False ;

  if not IsOpened then begin
    Result := False ;
    exit ;
  end ;

  level := prepareCache( _ptg, _pixelsize ) ;
  Result := T_visibleTiles( oVisibleTiles ).Locate(
               _ptg,
               _rgbMapped, _nativesVals,
               _transparency,
               level
            ) ;
end ;

function TGIS_LayerWebTiles.PreRecognize(
  const _path     : String ;
    var _new_path : String
) : Boolean ;
begin
  Result := inherited PreRecognize( _path, _new_path ) and
            (
              ( GetParamFromPath( _path, GIS_INI_LAYERSQL_STORAGE ) = 'WEBTILES' )
            ) ;
end ;
{$ENDREGION 'TGIS_LayerWebTiles'}

{$REGION 'Initialization'}
{ Perform initialization section.
}
class procedure Unit_GisLayerWebTiles.SelfRegisterLayer() ;
begin
  RegisterLayer( 'DK-TTKWP', GIS_PROTOCOL_LAYER_CONNECTOR,
                 TGIS_LayerWebTiles, GIS_TTKLAYER_WEB_FILTER,
                 TGIS_RegisteredLayerType.Pixel,
                 TGIS_RegisteredFormatType.Protocol,
                  [ TGIS_RegisteredOperationType.Read
                  ],
                 False
               ) ;
end ;

{$IFNDEF OXYGENE}
initialization
    Unit_GisLayerWebTiles.SelfRegisterLayer() ;
{$ENDIF}
{$ENDREGION  'Initialization'}

//==================================== END =====================================
end.
