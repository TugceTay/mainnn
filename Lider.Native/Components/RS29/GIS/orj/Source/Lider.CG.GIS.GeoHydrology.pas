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
  Hydrology toolset.
}

{$IFDEF DCC}
  unit GisHydrology ;
  {$HPPEMIT '#pragma link "GisHydrology"'}
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
    System.Runtime.InteropServices,
    System.Linq,
    TatukGIS.RTL,
    TatukGIS.NDK
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Generics.Collections,
    System.Math,
    System.Types,
    GisClasses,
    GisLayerPixel,
    GisLayerVector,
    GisRtl,
    GisTypes
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl
{$ENDIF}
{$IFDEF ISLAND}
  uses
    TatukGIS.RTL
{$ENDIF}

// uses for unit tests
{$IFDEF GIS_UNIT_TESTS}
  {$IFDEF CLR}
    ,
    ttkNDUnit,
    Defines,
    NUnit.Framework,
    TatukGIS.NDK.Common;
  {$ENDIF}
  {$IFDEF DCC}
    ,
    ttkNDUnit,
    Defines;
  {$ENDIF}
  {$IFDEF JAVA}
    ,
    tatukgis.junit;
  {$ENDIF}
{$ELSE}
  ;
{$ENDIF}

type
  {#gendoc:hide}
  Unit_GisHydrology = class
    public
      class procedure SelfRegisterPipeline ;
  end;

  /// <summary>
  ///   Specifies stream ordering methods.
  /// </summary>
  TGIS_HydrologyStreamOrderMethod = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   It is a classic stream order with a bottom-up hierarchy that allocates
    ///   the first order to the river with its mouth at the sea or DEM border
    ///   (the main stem). Its tributaries are given a number one greater than
    ///   that of the river or stream into which they discharge.
    /// </summary>
    Hack,
    /// <summary>
    ///   This ordering method gives the outermost tributaries the first order
    ///   and at a confluence the two orders are added together.
    /// </summary>
    Shreve,
    /// <summary>
    ///   It is a top-down method where rivers of the first order are
    ///   the outermost tributaries. If two streams of the same order merge,
    ///   the resulting stream is given a number that is one higher.
    ///   If two rivers with different stream orders merge, the resulting stream
    ///   is given the higher of the two numbers.
    /// </summary>
    Strahler,
    /// <summary>
    ///   This bottom-up method of ordering calculates the topological distance
    ///   of every stream from the outlet.
    /// </summary>
    Topological
  ) ;

  /// <summary>
  ///   A class containing the most commonly used hydrological tools.
  /// </summary>
  /// <remarks>
  ///   <note type="caution">
  ///     All pixel layers used in methods from this class must be grids and
  ///     have the same CS and resolution.
  ///   </note>
  /// </remarks>
  /// <exception cref="EGIS_Exception">
  ///   GIS_RS_ERR_LAYERNOEXIST:
  ///   code 0 - a layer is not assigned
  ///   GIS_RS_ERR_BADPARAM:
  ///   code 0 - a layer is not a grid;
  ///   code 1 - layer CS does not match;
  ///   code 2 - layer resolution does not match
  /// </exception>
  TGIS_Hydrology = {$IFDEF OXYGENE} public {$ENDIF}
                   class( TGIS_BaseObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      const
      FILL_APPLY_SLOPE          = True ;
      FLOWDIR_RESOLVE_FLAT      = False ;
      BASIN_THRESHOLD           = 0 ;
      STREAM_ORDER_METHOD       = TGIS_HydrologyStreamOrderMethod.Strahler ;
      STREAM_ORDER_THRESHOLD    = -1 ;
      STREAM_POLYLINE_THRESHOLD = 0 ;
      STREAM_POLYLINE_FIELD     = '' ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      busyEventManager : TGIS_BusyEventManager ;

      {$IFDEF CLR}
        procedure fadd_BusyEvent    ( const _value : TGIS_BusyEvent  ) ;
        procedure fremove_BusyEvent ( const _value : TGIS_BusyEvent  ) ;
      {$ELSE}
        function  fget_BusyEvent    : TGIS_BusyEvent ;
        procedure fset_BusyEvent    ( const _value : TGIS_BusyEvent ) ;
      {$ENDIF}

    protected
      procedure doDestroy ; override ;

    public
      /// <summary>
      ///   Creates an instance of a TGIS_Hydrology class.
      /// </summary>
      constructor Create ;

    public
      /// <summary>
      ///   Generates a basin grid from a flow direction grid.
      /// </summary>
      /// <param name="_flowDir">
      ///   an input flow direction grid
      /// </param>
      /// <param name="_extent">
      ///   an extent to be processed
      /// </param>
      /// <param name="_basins">
      ///   the output grid layer
      /// </param>
      /// <param name="_threshold">
      ///   use this property to limit basins with a small number of cells;
      ///   default parameter is 0
      /// </param>
      procedure Basin           ( const _flowDir     : TGIS_LayerPixel ;
                                  const _extent      : TGIS_Extent ;
                                  const _basins      : TGIS_LayerPixel ;
                                  const _threshold   : Integer = BASIN_THRESHOLD
                                ) ;

      /// <summary>
      ///   Generates the hydrologically conditioned DEM.
      /// </summary>
      /// <param name="_dem">
      ///   an input DEM
      /// </param>
      /// <param name="_extent">
      ///   an extent to be processed
      /// </param>
      /// <param name="_hydroDem">
      ///   the output grid layer
      /// </param>
      /// <param name="_applySlope">
      ///   if True, algorithm applies a slight slope to ensure the drainage
      ///   of water for each cell;
      ///   if False, sinks are flooded and form flat areas;
      ///   default parameter is True
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///     Applying slope is strongly recommended. A DEM with flat areas may
      ///     still cause unexpected results and such models need futher
      ///     preparation. The ResolveFlat tool may be use for this purpose.
      ///   </note>
      /// </remarks>
      procedure Fill            ( const _dem         : TGIS_LayerPixel ;
                                  const _extent      : TGIS_Extent ;
                                  const _hydroDem    : TGIS_LayerPixel ;
                                  const _applySlope  : Boolean
                                                     = FILL_APPLY_SLOPE
                                ) ;

      /// <summary>
      ///   Generates a flow accumulation grid from a flow direction grid.
      /// </summary>
      /// <param name="_flowDir">
      ///   an input flow direction grid
      /// </param>
      /// <param name="_extent">
      ///   an extent to be processed
      /// </param>
      /// <param name="_flowAcc">
      ///   the output grid layer
      /// </param>
      procedure FlowAccumulation( const _flowDir     : TGIS_LayerPixel ;
                                  const _extent      : TGIS_Extent ;
                                  const _flowAcc     : TGIS_LayerPixel
                                ) ;

      /// <summary>
      ///   Generates a flow direction grid from a DEM.
      /// </summary>
      /// <param name="_dem">
      ///   an input DEM
      /// </param>
      /// <param name="_extent">
      ///   an extent to be processed
      /// </param>
      /// <param name="_flowDir">
      ///   the output grid layer
      /// </param>
      /// <param name="_resolveFlat">
      ///   if True, the ResolveFlat tool is performed for remaining cells within
      ///   flat areas, where the dominant direction can not be determined;
      ///   if False, above mentioned cells have encoded the direction
      ///   by summing direction codes of neighbors with the drop value equals 0;
      ///   default parameter is False
      /// </param>
      /// <remarks>
      ///   The flow direction for a cell is the direction water will flow out
      ///   of the cell. It is encoded to correspond to the orientation of one
      ///   of the eight cells that surround the cell as follows:
      ///   <list type="bullet">
      ///     <item>
      ///       East (E) - 1
      ///     </item>
      ///     <item>
      ///       Southeast (SE) - 2
      ///     </item>
      ///     <item>
      ///       South (S) - 4
      ///     </item>
      ///     <item>
      ///       Southwest (SW) - 8
      ///     </item>
      ///     <item>
      ///       West (W) - 16
      ///     </item>
      ///     <item>
      ///       Northwest (NW) - 32
      ///     </item>
      ///     <item>
      ///       North (N) - 64
      ///     </item>
      ///     <item>
      ///       Northeast (NE) - 128
      ///     </item>
      ///   </list>
      ///   There are four possible conditions to consider in determining flow
      ///   direction:
      ///   <list type="number">
      ///     <item>
      ///       All eight neighboring cells have elevations higher than the
      ///       center cell. The flow direction is encoded as negative.
      ///     </item>
      ///     <item>
      ///       The distance-weighted drop from the center cell is higher for
      ///       one cell in the neighborhood over all of the other seven and
      ///       theflow direction is assigned to this cell.
      ///       This condition occurs for most cells (all cells for
      ///       a hydrologically conditionedd DEM.
      ///     </item>
      ///     <item>
      ///       When two or more cells are equal in having the greatest
      ///       distance-weighted drop, the flow direction is arbitrarily
      ///       assigned at the last occurence of the greatest drop.
      ///     </item>
      ///     <item>
      ///       All cells are equal. In this case, the cell is located in a flat
      ///       area and the locations of those neighbors is encoded by summing
      ///       their neighbor location codes.
      ///     </item>
      ///   </list>
      /// </remarks>
      procedure FlowDirection   ( const _dem         : TGIS_LayerPixel ;
                                  const _extent      : TGIS_Extent ;
                                  const _flowDir     : TGIS_LayerPixel ;
                                  const _resolveFlat : Boolean
                                                     = FLOWDIR_RESOLVE_FLAT
                                ) ;

      /// <summary>
      ///   Resolves flat areas in a DEM.
      /// </summary>
      /// <param name="_dem">
      ///   an input DEM
      /// </param>
      /// <param name="_flowDir">
      ///   a flow direction grid
      /// </param>
      /// <param name="_extent">
      ///   an extent to be processed
      /// </param>
      /// <param name="_alteredDem">
      ///   the optional output grid layer
      /// </param>
      /// <remarks>
      ///   If _alteredDem is not assigned (nil), flow directions across flats
      ///   is reassigned in _flowDir
      /// </remarks>
      procedure ResolveFlat     ( const _dem         : TGIS_LayerPixel ;
                                  const _flowDir     : TGIS_LayerPixel ;
                                  const _extent      : TGIS_Extent ;
                                  const _alteredDem  : TGIS_LayerPixel
                                ) ;

      /// <summary>
      ///   Generates the grid with pit cells and cells for which
      ///   the flow direction can not be determined.
      /// </summary>
      /// <param name="_dem">
      ///   an input DEM
      /// </param>
      /// <param name="_extent">
      ///   an extent to be processed
      /// </param>
      /// <param name="_sinks">
      ///   the output grid layer
      /// </param>
      /// <remarks>
      ///   Problematic cells or group of cells are ordered from 0 and assigned
      ///   as an integer grid value.
      /// </remarks>
      procedure Sink            ( const _dem         : TGIS_LayerPixel ;
                                  const _extent      : TGIS_Extent ;
                                  const _sinks       : TGIS_LayerPixel
                                ) ;

      /// <summary>
      ///   Generates a stream order grid from a flow direction and flow
      ///   accumulation grids and a specified method.
      /// </summary>
      /// <param name="_flowDir">
      ///   an input flow direction grid
      /// </param>
      /// <param name="_flowAcc">
      ///   an input flow accumulation grid
      /// </param>
      /// <param name="_extent">
      ///   an extent to be processed
      /// </param>
      /// <param name="_streams">
      ///   the output grid layer
      /// </param>
      /// <param name="_threshold">
      ///   use this property to limit processed cells from the flow
      ///   accumulation grid; default parameter is -1, which means the
      ///   recommended value for this tool is automatically assigned
      /// </param>
      /// <param name="_method">
      ///   stream ordering method; default parameter is Strahler
      /// </param>
      /// <remarks>
      ///   Recommended threshold for this tool is 1% of the maximum value from
      ///   flow the accumulation grid.
      /// </remarks>
      procedure StreamOrder     ( const _flowDir     : TGIS_LayerPixel ;
                                  const _flowAcc     : TGIS_LayerPixel ;
                                  const _extent      : TGIS_Extent ;
                                  const _streams     : TGIS_LayerPixel ;
                                  const _method      : TGIS_HydrologyStreamOrderMethod
                                                     = STREAM_ORDER_METHOD ;
                                  const _threshold   : Integer
                                                     = STREAM_ORDER_THRESHOLD
                                ) ;

      /// <summary>
      ///   Converts a stream network to polylines.
      /// </summary>
      /// <param name="_flowDir">
      ///   an input flow direction grid
      /// </param>
      /// <param name="_streamsPix">
      ///   an input grid representing a stream network
      /// </param>
      /// <param name="_extent">
      ///   an extent to be processed
      /// </param>
      /// <param name="_streamsVec">
      ///   the output vector layer
      /// </param>
      /// <param name="_orderField">
      ///   an optional numerical field from _streamsVec to populate stream
      ///   network value (flow accumulation, or stream order value from grid);
      ///   default parameter is empty string, this means that no attribute
      ///   information is transferred to vector layer
      /// </param>
      /// <param name="_threshold">
      ///   use this property to limit processed cells from stream network;
      ///   default parameter is 0
      /// </param>
      /// <remarks>
      ///   The stream network is represented by flow accumulation, or stream
      ///   order grid. If the flow accumulation grid is used, recommended
      ///   threshold is 1% of the maximum value from the flow accumulation grid.
      /// </remarks>
      procedure StreamToPolyline( const _flowDir     : TGIS_LayerPixel ;
                                  const _streamsPix  : TGIS_LayerPixel ;
                                  const _extent      : TGIS_Extent ;
                                  const _streamsVec  : TGIS_LayerVector ;
                                  const _orderField  : String
                                                     = STREAM_POLYLINE_FIELD ;
                                  const _threshold   : Integer
                                                     = STREAM_POLYLINE_THRESHOLD
                                ) ;

      /// <summary>
      ///   Generates a watershed grid from a flow direction grid and vector
      ///   layer with pour points.
      /// </summary>
      /// <param name="_flowDir">
      ///   an input flow direction grid
      /// </param>
      /// <param name="_outlets">
      ///   an input vector layer with pour points
      /// </param>
      /// <param name="_field">
      ///   a numerical field from _outlets to use in ordering watersheds;
      ///   if empty GIS_UID is used
      /// </param>
      /// <param name="_extent">
      ///   an extent to be processed
      /// </param>
      /// <param name="_watershed">
      ///   the output grid layer
      /// </param>
      /// <remarks>
      ///   The precision of locating pour points is important. Outlets should
      ///   be located to cells of high accumulated flow.
      /// </remarks>
      procedure Watershed       ( const _flowDir     : TGIS_LayerPixel ;
                                  const _outlets     : TGIS_LayerVector ;
                                  const _field       : String ;
                                  const _extent      : TGIS_Extent ;
                                  const _watershed   : TGIS_LayerPixel
                                ) ; overload ;

      /// <summary>
      ///   Generates a watershed grid from a flow direction grid and grid
      ///   layer with pour points.
      /// </summary>
      /// <param name="_flowDir">
      ///   an input flow direction grid
      /// </param>
      /// <param name="_outlets">
      ///   an input vector layer with pour points;
      /// </param>
      /// <param name="_extent">
      ///   an extent to be processed
      /// </param>
      /// <param name="_watershed">
      ///   the output grid layer
      /// </param>
      /// <remarks>
      ///   The precision of locating pour points is important. Outlets should
      ///   be located to cells of high accumulated flow.
      /// </remarks>
      procedure Watershed       ( const _flowDir     : TGIS_LayerPixel ;
                                  const _outlets     : TGIS_LayerPixel ;
                                  const _extent      : TGIS_Extent ;
                                  const _watershed   : TGIS_LayerPixel
                                ) ; overload ;

    published // events
      /// <summary>
      ///   Event fired upon progress of the hydrology process.
      /// </summary>
      {$IFDEF CLR}
        event BusyEvent    : TGIS_BusyEvent
                             add fadd_BusyEvent
                             remove fremove_BusyEvent ;
      {$ELSE}
        /// <event/>
        property BusyEvent : TGIS_BusyEvent
                             read  fget_BusyEvent
                             write fset_BusyEvent ;
      {$ENDIF}
  end;

  {$IFDEF GIS_UNIT_TESTS}
    // Unit test for types from implementation section
    [TestFixture]
    TestUnitHydrology = {$IFDEF OXYGENE} public {$ENDIF} class( TttkNDUnit )
      public
        [Test] procedure TestHUtilsCellOnlyFlowsToOneCell ;
        [Test] procedure TestHUtilsDirectionToIndex ;
        [Test] procedure TestHUtilsGetCellSize ;
        [Test] procedure TestHUtilsGetNeighbor ;
        [Test] procedure TestHUtilsIndexToDirection ;
        [Test] procedure TestHUtilsQueueIsEmpty ;
        [Test] procedure TestHUtilsReverseDirection ;
        [Test] procedure TestPixelLockTiler ;
        [Test] procedure TestPixelLockAdapter ;

    end;
  {$ENDIF}

implementation

{$IFDEF DCC}
  uses
    System.SysUtils,

    GisCsSystems,
    GisFunctions,
    GisInternals,
    GisInterpolation,
    GisLayer,
    GisPipeline,
    GisResource,
    GisStatistics,
    GisVectorization ;
{$ENDIF}

{$REGION 'Constants'}
const
  DZMIN               = 1e-3 ;  // height difference of a slight slope
  NEIGHBOR_COUNT      = 8 ;
  DC_NEIGHBOR         : array [0..NEIGHBOR_COUNT-1] of Integer
                      = ( 1, 1, 0,-1,-1,-1, 0, 1 ) ;
  DR_NEIGHBOR         : array [0..NEIGHBOR_COUNT-1] of Integer
                      = ( 0, 1, 1, 1, 0,-1,-1,-1 ) ;
  DIRECTIONS          : array [0..NEIGHBOR_COUNT-1] of Integer
                      = ( 1, 2, 4, 8, 16, 32, 64, 128 ) ;
  REVERSED_DIRECTIONS : array [0..NEIGHBOR_COUNT-1] of Integer
                      = ( 16, 32, 64, 128, 1, 2, 4, 8 ) ;
{$ENDREGION}

// Helper classes //

{$REGION 'T_HUtils'}
type
  T_HUtils = class
    public
      /// <summary>
      ///   Checks if cell only flows to one cell.
      /// </summary>
      /// <param name="_dirCode">
      ///   a direction code, 1 means East and goes clockwise
      /// </param>
      /// <returns>
      ///   a boolean True if yes
      /// </returns>
      class function  CellOnlyFlowsToOneCell( const _dirCode  : Integer
                                            ) : Boolean ;
                                            {$IFDEF GIS_INLINE} inline ; {$ENDIF}

      /// <summary>
      ///   Converts a flow direction code [1,2,4,...,128] to direction index [0..7].
      /// </summary>
      /// <param name="_dirCode">
      ///   a direction code, 1 means East and goes clockwise
      /// </param>
      /// <returns>
      ///   a direction index, 0 means East and goes clockwise
      /// </returns>
      class function  DirectionToIndex      ( const _dirCode  : Integer
                                            ) : Integer ;
                                            {$IFDEF GIS_INLINE} inline ; {$ENDIF}

      /// <summary>
      ///   Gets pixel layer cell size.
      /// </summary>
      /// <param name="_lp">
      ///   a pixel layer
      /// </param>
      class function  GetCellSize           ( const _lp       : TGIS_LayerPixel
                                            ) : TGIS_Point ;
                                            {$IFDEF GIS_INLINE} inline ; {$ENDIF}

      /// <summary>
      ///   Gets neighbor position in specified direction.
      /// </summary>
      /// <param name="_row">
      ///   a row index
      /// </param>
      /// <param name="_col">
      ///   a column index
      /// </param>
      /// <param name="_dirIndex">
      ///   a direction index [0..7]; 0 means East and goes clockwise
      /// </param>
      /// <param name="_rowN">
      ///   an output neighbor row index
      /// </param>
      /// <param name="_colN">
      ///   an output neighbor colun index
      /// </param>
      class procedure GetNeighbor           ( const _row      : Integer ;
                                              const _col      : Integer ;
                                              const _dirIndex : Integer ;
                                              var   _rowN     : Integer ;
                                              var   _colN     : Integer
                                            ) ; {$IFDEF GIS_INLINE} inline ; {$ENDIF}

      /// <summary>
      ///   Converts direction index [0..7] to flow direction codes [1,2,4,...,128].
      /// </summary>
      /// <param name="_dirIndex">
      ///   a direction index [0..7]; 0 means East and goes clockwise
      /// </param>
      /// <returns>
      ///   a direction code; 1 means East and goes clockwise
      /// </returns>
      class function  IndexToDirection      ( const _dirIndex : Integer
                                            ) : Integer ;
                                            {$IFDEF GIS_INLINE} inline ; {$ENDIF}

      /// <summary>
      ///   Reverses a specified direction code, e.g. 4 (South) -> 64 (North)
      /// </summary>
      /// <param name="_dirCode">
      ///   a direction code, 1 means East and goes clockwise
      /// </param>
      /// <returns>
      ///   a reverse direction code
      /// </returns>
      class function  ReverseDirection      ( const _dirCode  : Integer
                                            ) : Integer ;
                                            {$IFDEF GIS_INLINE} inline ; {$ENDIF}

      /// <summary>
      ///   Checks if queue of TPoint is empty.
      /// </summary>
      /// <param name="_queue">
      ///   a given queue
      /// </param>
      class function  QueueIsEmpty          ( const _queue    : TQueue<TPoint>
                                            ) : Boolean ; overload ;
                                            {$IFDEF GIS_INLINE} inline ; {$ENDIF}

      /// <summary>
      ///   Checks if queue of TGIS_Point3DInt is empty.
      /// </summary>
      /// <param name="_queue">
      ///   a given queue
      /// </param>
      class function  QueueIsEmpty          ( const _queue    : TQueue<TGIS_Point3DInt>
                                            ) : Boolean ; overload ;
                                            {$IFDEF GIS_INLINE} inline ; {$ENDIF}
  end;

class procedure T_HUtils.GetNeighbor(
  const _row      : Integer ;
  const _col      : Integer ;
  const _dirIndex : Integer ;
  var   _rowN     : Integer ;
  var   _colN     : Integer
) ;
begin
  _colN := _col + DC_NEIGHBOR[_dirIndex] ;
  _rowN := _row + DR_NEIGHBOR[_dirIndex] ;
end;

class function T_HUtils.GetCellSize(
  const _lp : TGIS_LayerPixel
) : TGIS_Point ;
begin
  Result := GisPoint(
    ( _lp.Extent.XMax - _lp.Extent.XMin ) / _lp.BitWidth,
    ( _lp.Extent.YMax - _lp.Extent.YMin ) / _lp.BitHeight
  ) ;
end;

class function T_HUtils.IndexToDirection(
  const _dirIndex : Integer
) : Integer ;
begin
  if ( _dirIndex < 0 ) or ( _dirIndex > 7 ) then begin
    Result := -1 ;
    exit ;
  end ;

  Result := DIRECTIONS[ _dirIndex ] ;
end ;

class function T_HUtils.DirectionToIndex(
  const _dirCode : Integer
) : Integer ;
var
  dir_id : Integer;
begin
  Result := -1 ;
  for dir_id := 0 to NEIGHBOR_COUNT-1 do
    if ( _dirCode = DIRECTIONS[dir_id] ) then
      Result := dir_id ;
end ;

class function T_HUtils.CellOnlyFlowsToOneCell(
  const _dirCode : Integer
) : Boolean ;
begin
  if ( _dirCode < 0 ) or ( _dirCode > 128 ) then begin
    Result := False;
    exit ;
  end ;

  Result := ( _dirCode <> 0 ) and ( ( _dirCode and ( _dirCode - 1 ) ) = 0 )  ;
end;

class function T_HUtils.ReverseDirection(
  const _dirCode : Integer
) : Integer ;
begin
  // multiple directions are not considered
  if not CellOnlyFlowsToOneCell( _dirCode ) then begin
    Result := -1 ;
    exit ;
  end ;

  if _dirCode < 16 then
    Result := _dirCode * 16
  else
    Result := _dirCode div 16 ;
end ;

class function T_HUtils.QueueIsEmpty(
  const _queue : TQueue<TPoint>
) : Boolean ;
begin
  Result := ( _queue.Count = 0 ) ;
end;

class function T_HUtils.QueueIsEmpty(
  const _queue : TQueue<TGIS_Point3DInt>
) : Boolean ;
begin
  Result := ( _queue.Count = 0 ) ;
end;
{$ENDREGION}

{$REGION 'T_PixelLockTiler'}
type
  T_PixelLockTiler = class
    private
      FLayer       : TGIS_LayerPixel ;
      FCurrentItem : Integer ;
      FExtent      : TGIS_Extent;
      FOverlap     : Integer;
      FTileSize    : Integer;
      FExtentRect  : TRect;

    private
      extentOffsetRect    : TRect ;
      cellSize            : TGIS_Point ;
      tilesCount          : Integer ;
      xyTilesCount        : TPoint ;
      doubleOverlap       : Integer ;
      overlappingTileSize : Integer ;
      width               : Integer ;
      height              : Integer ;

    private
      procedure fset_TileSize  ( const _value        : Integer ) ;

      // get the i-th tile XY-index
      function  getTileIndex   ( const _i            : Integer ;
                                 const _tiles_no     : TPoint
                               ) : TPoint ;

      // prepare the rectangle for locking pixels
      function  getTileRect    ( const _tileIndex    : TPoint ;
                                 const _xyTilesCount : TPoint ;
                                 const _width        : Integer ;
                                 const _height       : Integer
                               ) : TRect ;

      // determine the number of the tiles for given pixel layer size
      function  getXYTilesCount( const _width        : Integer ;
                                 const _height       : Integer
                               ) : TPoint ;

    public
      constructor Create( const _layer    : TGIS_LayerPixel ;
                          const _extent   : TGIS_Extent ;
                          const _tileSize : Integer ;
                          const _overlap  : Integer
                        ) ;

    public
      function  CurrentRect : TRect ;

      function  Next        : Boolean ;

      procedure Reset ;

    public
      property Count      : Integer
                            read tilesCount ;

      property Layer      : TGIS_LayerPixel
                            read FLayer ;

      property Extent     : TGIS_Extent
                            read FExtent ;

      property Overlap    : Integer
                            read FOverlap ;

      property TileSize   : Integer
                            read FTileSize ;

      property ExtentRect : TRect
                            read FExtentRect ;
  end;

constructor T_PixelLockTiler.Create(
  const _layer    : TGIS_LayerPixel ;
  const _extent   : TGIS_Extent ;
  const _tileSize : Integer ;
  const _overlap  : Integer
) ;
begin
  {$IFNDEF GIS_UNIT_TESTS}
    assert( _tileSize > 2 ) ;
  {$ENDIF}

  FLayer := _layer ;
  FExtent := _extent ;
  FTileSize := _tileSize ;
  FOverlap := _overlap ;

  cellSize := T_HUtils.GetCellSize( FLayer ) ;
  doubleOverlap := 2 * FOverlap ;
  overlappingTileSize := FTileSize - doubleOverlap ;

  Reset ;
end;

function T_PixelLockTiler.Next : Boolean ;
begin
  Result := ( FLayer <> nil ) and ( FCurrentItem < tilesCount-1 ) ;
  if Result then
    inc( FCurrentItem ) ;
end;

function T_PixelLockTiler.CurrentRect : TRect ;
var
  tile_index : TPoint ;
begin
  //?  simplified methods
  tile_index := getTileIndex( FCurrentItem, xyTilesCount ) ;
  Result := getTileRect( tile_index, xyTilesCount, width, height ) ; //? tilescount hide in class
end;

procedure T_PixelLockTiler.Reset ;
var
  ext_lyr : TGIS_Extent ;
  ext_given : TGIS_Extent ;
begin
  FCurrentItem := -1 ;

  width := FLayer.BitWidth ;
  height := FLayer.BitHeight ;

  extentOffsetRect := Rect( 0, 0, 0, 0 ) ;

  ext_lyr := FLayer.Extent ;
  if not  GisIsSameExtent( ext_lyr, FExtent ) then begin
    ext_given := GisCommonExtent( ext_lyr, FExtent ) ;
    if GisIsEmptyExtent( ext_given ) then begin
      width := 0 ;
      height := 0 ;
    end
    else begin
      extentOffsetRect := Rect(
        FloorS( ( ext_given.XMin - ext_lyr.XMin ) / cellSize.X ),  // Left
        FloorS( ( ext_lyr.YMax - ext_given.YMax ) / cellSize.Y ),  // Top
        FloorS( ( ext_lyr.XMax - ext_given.XMax ) / cellSize.X ),  // Right
        FloorS( ( ext_given.YMin - ext_lyr.YMin ) / cellSize.Y )   // Bottom
      ) ;
      width := width - extentOffsetRect.Left - extentOffsetRect.Right ;
      height := height - extentOffsetRect.Top - extentOffsetRect.Bottom ;
    end ;
  end ;

  FExtentRect := Rect(
    extentOffsetRect.Left,
    extentOffsetRect.Top,
    extentOffsetRect.Left + width,
    extentOffsetRect.Top + height
  ) ;

  // tiles count
  xyTilesCount := getXYTilesCount( width, height ) ;
  tilesCount := xyTilesCount.X * xyTilesCount.Y ;
end;

procedure T_PixelLockTiler.fset_TileSize ( const _value : Integer ) ;
begin
  FTileSize := _value ;
  Reset ;
end ;

function T_PixelLockTiler.getTileIndex(
  const _i        : Integer ;
  const _tiles_no : TPoint
) : TPoint ;
var
  x, y      : Integer ;
begin
  x := _i mod _tiles_no.X ;
  y := _i div _tiles_no.X ;  // X is OK here!

  Result := TPoint.Create( x, y ) ;
end ;

function T_PixelLockTiler.getTileRect(
  const _tileIndex    : TPoint ;
  const _xyTilesCount : TPoint ;
  const _width        : Integer ;
  const _height       : Integer
) : TRect ;
var
  left, top, right, bottom : Integer ;
begin
  left := _tileIndex.X * overlappingTileSize + extentOffsetRect.Left ;
  top := _tileIndex.Y * overlappingTileSize + extentOffsetRect.Top ;

  right := left + FTileSize ;
  bottom := top + FTileSize ;

  // if last X tile, cut at width or extent X
  if _tileIndex.X = _xyTilesCount.X - 1 then
    right := Min( right, extentOffsetRect.Left + _width ) ;  //? h and w are global

  // if last Y tile, cut at height or extent Y
  if _tileIndex.Y = _xyTilesCount.Y - 1 then
    bottom := Min( bottom, extentOffsetRect.Top + _height ) ;

  Result := Rect( left, top, right, bottom ) ;
end;

function T_PixelLockTiler.getXYTilesCount(
  const _width  : Integer ;
  const _height : Integer
) : TPoint ;
var
  x, y      : Integer ;
  h_overlap : Integer ;
  w_overlap : Integer ;
begin
  w_overlap := _width - doubleOverlap ;
  h_overlap := _height - doubleOverlap ;

  x := w_overlap div overlappingTileSize ;
  y := h_overlap div overlappingTileSize ;

  if ( w_overlap mod overlappingTileSize ) <> 0 then
    inc( x ) ;

  if ( h_overlap mod overlappingTileSize ) <> 0 then
    inc( y ) ;

  Result := Point( x, y ) ;
end;
{$ENDREGION}

{$REGION 'T_PixelLockAdapter'}
type
  /// <summary>
  ///   The TGIS_LayerPixelLock class adapter (wrapper). This wrapper class
  ///   significantly speeds up and facilitates work with a grid lock.
  /// </summary>
  T_PixelLockAdapter = class
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FNodata   : Single;
      FNCols    : Integer ;
      FNRows    : Integer ;
      FCellSize : TGIS_Point ;
      FLock     : TGIS_LayerPixelLock;

      top       : Integer ;
      bottom    : Integer ;
      left      : Integer ;
      right     : Integer ;

    private
      // getters & setters
      function  fget_GridRel   ( const _row   : Integer ;
                                 const _col   : Integer
                               ) : Single ;

      procedure fset_GridRel   ( const _row   : Integer ;
                                 const _col   : Integer ;
                                 const _value : Single
                               ) ;

      function  fget_GridRelInt( const _row   : Integer ;
                                 const _col   : Integer
                               ) : Integer ;

      procedure fset_GridRelInt( const _row   : Integer ;
                                 const _col   : Integer ;
                                 const _value : Integer
                               ) ;

    public
      /// <summary>
      ///   Creates an object
      /// </summary>
      /// <param name="_sender">
      ///   an instance of a grid lock
      /// </param>
      constructor Create( const _sender : TGIS_LayerPixelLock ) ;

    public
      /// <summary>
      ///   Checks if a cell is within the grid.
      /// </summary>
      /// <param name="_row">
      ///   row index (Y position)
      /// </param>
      /// <param name="_col">
      ///   column index (X position)
      /// </param>
      /// <returns>
      ///   True if a cell is within the grid
      /// </returns>
      function  CellIsInGrid            ( const _row        : Integer ;
                                          const _col        : Integer
                                        ) : Boolean ;

      /// <summary>
      ///   Checks if a cell is NODATA.
      /// </summary>
      /// <param name="_row">
      ///   row index (Y position)
      /// </param>
      /// <param name="_col">
      ///   column index (X position)
      /// </param>
      /// <returns>
      ///   True if a cell is NODATA
      /// </returns>
      function  CellIsNodata            ( const _row        : Integer ;
                                          const _col        : Integer
                                        ) : Boolean ;

      /// <summary>
      ///   Checks if a cell is NODATA and if not, assigns the value as Single.
      /// </summary>
      /// <param name="_row">
      ///   row index (Y position)
      /// </param>
      /// <param name="_col">
      ///   column index (X position)
      /// </param>
      /// <param name="_value">
      ///   variable to assign the value if exist (Single)
      /// </param>
      /// <returns>
      ///   True if a cell is NODATA
      /// </returns>
      function  CellIsNodataElseGetValue( const _row        : Integer ;
                                          const _col        : Integer ;
                                          out   _value      : Single
                                        ) : Boolean ; overload ;

      /// <summary>
      ///   Checks if a cell is NODATA and if not, assigns the value as Integer.
      /// </summary>
      /// <param name="_row">
      ///   row index (Y position)
      /// </param>
      /// <param name="_col">
      ///   column index (X position)
      /// </param>
      /// <param name="_value">
      ///   variable to assign the value if exist (Integer)
      /// </param>
      /// <returns>
      ///   True if a cell is NODATA
      /// </returns>
      function  CellIsNodataElseGetValue( const _row        : Integer ;
                                          const _col        : Integer ;
                                          var   _value      : Integer
                                        ) : Boolean ; overload ;

      /// <summary>
      ///   Fills the entire grid with the specified value.
      /// </summary>
      /// <param name="_value">
      ///   a value to be assigned
      /// </param>
      procedure Fill                    ( const _value      : Single
                                        ) ;

      /// <summary>
      ///   Fills the entire grid with NODATA.
      /// </summary>
      procedure FillWithNodata ;


      /// <summary>
      ///   Fills the grid with the specified value
      ///   using non-NODATA cell positions from the mask.
      /// </summary>
      /// <param name="_value">
      ///   a value to be assigned
      /// </param>
      /// <param name="_maskLockEx">
      ///    a mask defining cells to fill
      /// </param>
      procedure FillMask                ( const _value      : Single ;
                                          const _maskLockEx : T_PixelLockAdapter
                                        ) ;

      /// <summary>
      ///   Fills the grid with the specified value at cell positions where
      ///   mask valuee are greater or equal Threshhold value.
      /// </summary>
      /// <param name="_value">
      ///   a value to be assigned
      /// </param>
      /// <param name="_maskLockEx">
      ///    a mask defining cells to fill
      /// </param>
      /// <remarks>
      ///   Only non-NODATA cells are considered.
      /// </remarks>
      procedure FillMaskWithThreshold   ( const _value      : Single ;
                                          const _threshold  : Single ;
                                          const _maskLockEx : T_PixelLockAdapter
                                        ) ;


      /// <summary>
      ///   Sets NODATA to the specified.
      /// </summary>
      /// <param name="_row">
      ///   row index (Y position)
      /// </param>
      /// <param name="_col">
      ///   column index (X position)
      /// </param>
      procedure SetNodata               ( const _row        : Integer ;
                                          const _col        : Integer
                                        ) ;

    public
      /// <summary>
      ///   Unified access to grid values with relative indexes.
      /// </summary>
      /// <param name="_row">
      ///   row index (Y position), from 0 to Bounds.Height (rows count - 1)
      /// </param>
      /// <param name="_col">
      ///   column index (X position), from 0 to Bounds.Width (columns count - 1)
      /// </param>
      /// <returns>
      ///   grid value at specified position (Single)
      /// </returns>
      property GridRel   [ const _row : Integer ; const _col : Integer ] : Single
                           read fget_GridRel
                           write fset_GridRel ;

      /// <summary>
      ///   Unified access to grid values with relative indexes.
      /// </summary>
      /// <param name="_row">
      ///   row index (Y position), from 0 to Bounds.Height (rows count - 1)
      /// </param>
      /// <param name="_col">
      ///   column index (X position), from 0 to Bounds.Width (columns count - 1)
      /// </param>
      /// <returns>
      ///   grid value at specified position (Integer)
      /// </returns>
      property GridRelInt[ const _row : Integer ; const _col : Integer ] : Integer
                           read fget_GridRelInt
                           write fset_GridRelInt ;

      /// <summary>
      ///   Specifies the NODATA value in grid.
      /// </summary>
      property Nodata    : Single read FNodata ;

      /// <summary>
      ///   Specifies the parent pixel lock object.
      /// </summary>
      property Lock      : TGIS_LayerPixelLock read FLock ;

      /// <summary>
      ///   Specifies the number of columns in the locked grid.
      /// </summary>
      property NCols     : Integer read FNCols ;

      /// <summary>
      ///   Specifies the number of rows in the locked grid.
      /// </summary>
      property NRows     : Integer read FNRows ;

      /// <summary>
      ///   Specifies the number of columns in the locked grid.
      /// </summary>
      property CellSize  : TGIS_Point read FCellSize ;
  end;

constructor T_PixelLockAdapter.Create(const _sender: TGIS_LayerPixelLock);
var
  grid_size : TPoint ;
begin
  FLock := _sender;
  FNodata := FLock.Parent.NoDataValue ;

  top := FLock.Bounds.Top ;
  bottom := FLock.Bounds.Bottom ;
  left := FLock.Bounds.Left ;
  right := FLock.Bounds.Right ;

  FNRows := bottom - top + 1 ;
  FNCols := right - left + 1 ;

  grid_size := Point( length( FLock.Grid[0] ), length( FLock. Grid) );

  // lock on file-based layers returns grid with size defindex by rect
  // for in-memory layers size is the same as layer
  if ( grid_size.X = FNCols ) and ( grid_size.Y = FNRows) then begin
    top := top - FLock.Bounds.Top ;
    bottom := bottom - FLock.Bounds.Top ;
    left := left - FLock.Bounds.Left ;
    right := right - FLock.Bounds.Left ;
  end ;
  FCellSize :=  _TGIS_Point( FLock.PixelSize ) ;
end;

function T_PixelLockAdapter.CellIsInGrid(
  const _row      : Integer ;
  const _col      : Integer
) : Boolean ;
begin
  Result := ( _row >= 0 ) and ( _row < NRows ) and
            ( _col >= 0 ) and ( _col < NCols ) ;
end;

function T_PixelLockAdapter.fget_GridRel(
  const _row      : Integer ;
  const _col      : Integer
) : Single ;
begin
  Result := FLock.Grid[_row + top, _col + left] ;
end;

procedure T_PixelLockAdapter.fset_GridRel(
  const _row      : Integer ;
  const _col      : Integer ;
  const _value    : Single
) ;
begin
  FLock.Grid[_row + top, _col + left] := _value ;
end;

function T_PixelLockAdapter.fget_GridRelInt(
  const _row      : Integer ;
  const _col      : Integer
) : Integer ;
begin
  Result := RoundS( GridRel[_row, _col] ) ;
end;

procedure T_PixelLockAdapter.Fill(
  const _value : Single
) ;
var
  c, r : Integer ;
begin
  for r := 0 to NRows-1 do
    for c := 0 to NCols-1 do
      GridRel[r, c] := _value ;
end;

procedure T_PixelLockAdapter.FillMask(
  const _value      : Single ;
  const _maskLockEx : T_PixelLockAdapter
) ;
var
  c, r : Integer ;
begin
  for r := 0 to NRows-1 do
    for c := 0 to NCols-1 do
      if _maskLockEx.CellIsNodata( r, c ) then
        SetNodata( r, c )
      else
        GridRel[r, c] := _value ;
end;

procedure T_PixelLockAdapter.FillMaskWithThreshold(
  const _value      : Single ;
  const _threshold  : Single ;
  const _maskLockEx : T_PixelLockAdapter
) ;
var
  c, r : Integer ;
  val  : Single ;
begin
  for r := 0 to NRows-1 do
    for c := 0 to NCols-1 do
      if _maskLockEx.CellIsNodataElseGetValue( r, c, val ) or
         ( val < _threshold ) then
        SetNodata( r, c )
      else
        GridRel[r, c] := _value ;
end;

procedure T_PixelLockAdapter.FillWithNodata;
begin
  Fill( Nodata ) ;
end;

procedure T_PixelLockAdapter.fset_GridRelInt(
  const _row      : Integer ;
  const _col      : Integer ;
  const _value    : Integer
) ;
begin
  GridRel[_row, _col ] := _value ;
end;

function T_PixelLockAdapter.CellIsNodata(
  const _row      : Integer ;
  const _col      : Integer
) : Boolean ;
var
  val : Single ;
begin
  Result := CellIsNodataElseGetValue( _row, _col, val ) ;
end;

function T_PixelLockAdapter.CellIsNodataElseGetValue(
  const _row     : Integer ;
  const _col     : Integer ;
  out   _value : Single
) : Boolean ;
begin
  _value := GridRel[_row, _col] ;
  Result := GisIsSameValue( _value, FNodata, GIS_SINGLE_RESOLUTION ) ;
end;

function T_PixelLockAdapter.CellIsNodataElseGetValue(
  const _row   : Integer ;
  const _col   : Integer ;
  var   _value : Integer
) : Boolean ;
var
  val : Single ;
begin
  Result := CellIsNodataElseGetValue( _row, _col, val ) ;
  _value := RoundS( val ) ;
end;

procedure T_PixelLockAdapter.SetNodata(
  const _row : Integer ;
  const _col : Integer
) ;
begin
  GridRel[_row, _col] := FNodata ;
end ;
{$ENDREGION}

{$REGION 'Hydrology classes declarations'}
type
  /// <summary>
  ///   The abstract class for all hydrology classes.
  /// </summary>
  /// <remarks>
  ///   <note type="caution">
  ///     All pixel layer used in methods from this class must have the same
  ///     CS and resolution.
  ///   </note>
  /// </remarks>
  T_HydrologyAbstract = class
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      busyEventManager : TGIS_BusyEventManager ;
      FThreshold       : Integer ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      /// <summary>
      ///   Verifies input and output layer
      /// </summary>
      /// <param name="_in">
      ///   an input grid layer
      /// </param>
      /// <param name="_out">
      ///   an output grid layer compared to the input
      /// </param>
      procedure verifyLayer( const _in  : TGIS_LayerPixel ;
                             const _out : TGIS_LayerPixel
                           ) ;

    public
      /// <summary>
      ///   Creates an object.
      /// </summary>
      constructor Create ;
  end;

  /// <summary>
  ///   The class that creates a depressionless digital elevation model (DEM).
  /// </summary>
  /// <remarks>
  ///   Due to errors and artifacts in DEM, hydrological analyzes reuire
  ///   the use of the hydrologically conditioned DEM. It means, DEM without
  ///   local pits, sinks or continuous flat areas.
  /// </remarks>
  T_HydrologyFill = class( T_HydrologyAbstract )
    const
      MAX_DEPTH   = 1024 * 1024 ;  // maximum recursion depth

    private
      FApplySlope : Boolean;

    private
      // 8-th direction DEM scan
      R0, C0 : array of Integer ;  // initial point
      dR, dC : array of Integer ;  // shift between neighbor points
      fR, fC : array of Integer ;  // shift when the DEM border is reached

      // minimum value between two cells:
      // step to climb when walking upwards (depends on direction)
      epsilonArr : array [0..7] of Single ;

    private
      // initializes R0, C0, dR, dC, fR and fC
      procedure initializeConstans( const _nRows       : Integer ;
                                    const _nCols       : Integer
                                  ) ;

      // fills epsilonArr
      procedure initializeEpsilon ( const _applySlope  : Boolean ) ;


      // Stage 1 (Table 3)
      procedure initializeSurface ( const _zLockEx     : T_PixelLockAdapter ;
                                    const _wLockEx     : T_PixelLockAdapter;
                                    const _borderCells : TQueue< TPoint >
                                  ) ;

      // improved implementation of Stage 2 (Table 7)
      procedure drainExcessWater  ( const _zLockEx     : T_PixelLockAdapter ;
                                    const _wLockEx     : T_PixelLockAdapter;
                                    const _borderCells : TQueue< TPoint >
                                  ) ;

      // implementation of Explore (Table 6)
      procedure dryUpwardCell     ( const _row         : Integer ;
                                    const _col         : Integer ;
                                    const _zLockEx     : T_PixelLockAdapter ;
                                    const _wLockEx     : T_PixelLockAdapter ;
                                    var   _depth       : Integer
                                  );
      // For the given cell (R,C) and the scan direction i, the function
      // nextCell calculates the coordinates of the next cell to consider
      function  nextCell          ( var   _row         : Integer ;
                                    var   _col         : Integer ;
                                    const _iScanDir    : Integer ;
                                    const _nRows       : Integer ;
                                    const _nCols       : Integer
                                  ) : Boolean ;

    public
      /// <inheritdoc/>
      constructor Create ;

    public
      /// <summary>
      ///   Generates the hydrologically conditioned DEM.
      /// </summary>
      /// <param name="_dem">
      ///   an input DEM
      /// </param>
      /// <param name="_extent">
      ///   an extent to be processed
      /// </param>
      /// <param name="_hydroDem">
      ///   the output grid layer
      /// </param>
      procedure Generate( const _dem      : TGIS_LayerPixel ;
                          const _extent   : TGIS_Extent ;
                          const _hydroDem : TGIS_LayerPixel
                        ) ;

    public
      /// <summary>
      ///   If True, algorithm applies a slight slope to ensure the drainage
      ///   of water for each cell.
      ///   If False, sinks are flooded and form flat areas.
      /// </summary>
      /// <remarks>
      ///   Default is True and this method is strongly recommended.
      ///   A DEM with flat areas may still cause unexpected results
      ///   and such models need futher preparation.
      ///   The T_HydrologyResolveFlat class may be use for this purpose.
      /// </remarks>
      property ApplySlope : Boolean read FApplySlope write FApplySlope;
  end;

  /// <summary>
  ///   The class that creates a grid with sinks from a DEM.
  /// </summary>
  T_HydrologySink = class( T_HydrologyAbstract )
    public
      /// <inheritdoc/>
      constructor Create ;

    public
      /// <summary>
      ///   Generates the grid with pit cells and cells for which
      ///   the flow direction can not be determined.
      /// </summary>
      /// <param name="_dem">
      ///   an input DEM
      /// </param>
      /// <param name="_extent">
      ///   an extent to be processed
      /// </param>
      /// <param name="_sinks">
      ///   the output grid layer
      /// </param>
      /// <remarks>
      ///   Problematic cells or group of cells are ordered from 0 and assigned
      ///   as an integer grid value.
      /// </remarks>
      procedure Generate( const _dem    : TGIS_LayerPixel ;
                          const _extent : TGIS_Extent ;
                          const _sinks  : TGIS_LayerPixel
                        ) ;
  end;

  /// <summary>
  ///   The class that resolves flats in a DEM.
  /// </summary>
  T_HydrologyResolveFlat = class( T_HydrologyAbstract )
    private
      // Algorithm 1
      procedure resolveFlats    ( const _demLockEx     : T_PixelLockAdapter ;
                                  const _flowDirLockEx : T_PixelLockAdapter ;
                                  var   _flatMask      : TGIS_IntegerGridArray ;
                                  var   _labels        : TGIS_IntegerGridArray
                                ) ;

      // Stage 2 of Algorithm 1
      function  findFlats       ( const _lowEdges      : TQueue< TPoint > ;
                                  var   _labels        : TGIS_IntegerGridArray ;
                                  const _demLockEx     : T_PixelLockAdapter
                                ) : Integer ;

      // Algorithm 2
      procedure d8FlowDirs      ( const _dem           : TGIS_LayerPixel ;
                                  const _flowDir       : TGIS_LayerPixel
                                ) ;

      // Algorithm 3
      procedure flatEdges       ( const _highEdges     : TQueue< TPoint > ;
                                  const _lowEdges      : TQueue< TPoint > ;
                                  const _demLockEx     : T_PixelLockAdapter ;
                                  const _flowDirLockEx : T_PixelLockAdapter
                                ) ;

      // Algorithm 4
      procedure labelFlats      ( const _cell          : TPoint ;
                                  const _labelId       : Integer ;
                                  const _demLockEx     : T_PixelLockAdapter ;
                                  const _labelsGrd     : TGIS_IntegerGridArray
                                ) ;

      // Algorithm 5
      procedure awayFromHigher  ( const _highEdges     : TQueue< TPoint > ;
                                  const _flatHeight    : TArray< Integer > ;
                                  const _labelsGrd     : TGIS_IntegerGridArray ;
                                  const _flatMaskGrd   : TGIS_IntegerGridArray ;
                                  const _flowDirLockEx : T_PixelLockAdapter
                                ) ;

      // Algorithm 6
      procedure towardsLower    ( const _lowEdges      : TQueue< TPoint > ;
                                  const _flatHeight    : TArray< Integer > ;
                                  const _labelsGrd     : TGIS_IntegerGridArray ;
                                  const _flatMaskGrd   : TGIS_IntegerGridArray ;
                                  const _flowDirLockEx : T_PixelLockAdapter
                                ) ;

      // Algorithm 7
      procedure d8MaskedFlowDirs( const _flatMask      : TGIS_IntegerGridArray ;
                                  const _flowDirLockEx : T_PixelLockAdapter    ;
                                  const _labels        : TGIS_IntegerGridArray
                                ) ;

      // Algorithm 8
      procedure alterDem        ( const _demLockEx     : T_PixelLockAdapter ;
                                  const _flatMask      : TGIS_IntegerGridArray ;
                                  const _labels        : TGIS_IntegerGridArray ;
                                  const _altDemLockEx  : T_PixelLockAdapter
                                ) ;

      /// <summary>
      ///   Determines if there is no flow.
      /// </summary>
      /// <param name="_dirCode">
      ///   flow direction code
      /// </param>
      /// <returns>
      ///   True, if a cell is a pit, or direction code is not a power of two
      /// </returns>
      function  isDirectionNoFlow     ( const _dirCode       : Integer
                                ) : Boolean ;



      // main algorithm
      procedure generateEx      ( const _demLockEx     : T_PixelLockAdapter ;
                                  const _flowDirLockEx : T_PixelLockAdapter ;
                                  const _altDemLockEx  : T_PixelLockAdapter
                                ) ;
    public
      /// <inheritdoc/>
      constructor Create ;

    public
      /// <summary>
      ///   Resolve flat ares in a DEM.
      /// </summary>
      /// <param name="_dem">
      ///   an input DEM
      /// </param>
      /// <param name="_flowDir">
      ///   a flow direction grid
      /// </param>
      /// <param name="_extent">
      ///   an extent to be processed
      /// </param>
      /// <param name="_alteredDem">
      ///   the optional output grid layer
      /// </param>
      /// <remarks>
      ///   If _alteredDem is not assigned (nil), flow directions across flats
      ///   will be reassigned in _flowDir
      /// </remarks>
      procedure Generate( const _dem        : TGIS_LayerPixel ;
                          const _flowDir    : TGIS_LayerPixel ;
                          const _extent     : TGIS_Extent ;
                          const _alteredDem : TGIS_LayerPixel
                        ) ;
  end;

  /// <summary>
  ///   The class that creates a flow direction grid from a DEM.
  /// </summary>
  T_HydrologyFlowDirection = class( T_HydrologyAbstract )
    private
      FResolveFlats : Boolean ;

    public
      /// <inheritdoc/>
      constructor Create ;

    public
      /// <summary>
      ///   Generates a flow direction grid from a DEM.
      /// </summary>
      /// <param name="_dem">
      ///   an input DEM
      /// </param>
      /// <param name="_extent">
      ///   an extent to be processed
      /// </param>
      /// <param name="_flowDir">
      ///   the output grid layer
      /// </param>
      /// <remarks>
      ///   The flow direction for a cell is the direction water will flow out
      ///   of the cell. It is encoded to correspond to the orientation of one
      ///   of the eight cells that surround the cell as follows:
      ///   <list type="bullet">
      ///     <item>
      ///       East (E) - 1
      ///     </item>
      ///     <item>
      ///       Southeast (SE) - 2
      ///     </item>
      ///     <item>
      ///       South (S) - 4
      ///     </item>
      ///     <item>
      ///       Southwest (SW) - 8
      ///     </item>
      ///     <item>
      ///       West (W) - 16
      ///     </item>
      ///     <item>
      ///       Northwest (NW) - 32
      ///     </item>
      ///     <item>
      ///       North (N) - 64
      ///     </item>
      ///     <item>
      ///       Northeast (NE) - 128
      ///     </item>
      ///   </list>
      ///   There are four possible conditions to consider in determining flow
      ///   direction:
      ///   <list type="number">
      ///     <item>
      ///       All eight neighboring cells have elevations higher than the
      ///       center cell. The flow direction is encoded as negative.
      ///     </item>
      ///     <item>
      ///       The distance-weighted drop from the center cell is higher for
      ///       one cell in the neighborhood over all of the other seven and
      ///       theflow direction is assigned to this cell.
      ///       This condition occurs for most cells (all cells for
      ///       a hydrologically conditionedd DEM.
      ///     </item>
      ///     <item>
      ///       When two or more cells are equal in having the greatest
      ///       distance-weighted drop, the flow direction is arbitrarily
      ///       assigned at the last occurence of the greatest drop.
      ///     </item>
      ///     <item>
      ///       All cells are equal. In this case, the cell is located in a flat
      ///       area and the locations of those neighbors is encoded by summing
      ///       their neighbor location codes.
      ///     </item>
      ///   </list>
      /// </remarks>
      procedure Generate( const _dem     : TGIS_LayerPixel ;
                          const _extent  : TGIS_Extent ;
                          const _flowDir : TGIS_LayerPixel
                        ) ;

    public
      /// <summary>
      ///   If True, the ResolveFlat tool is performed for remaining cells within
      ///   flat areas, where the dominant direction can not be determined.
      ///   This is the default.
      ///   If False, above mentioned cells have encoded the direction
      ///   by summing direction codes of neighbors with the drop value equals 0.
      /// </summary>
      property ResolveFlat : Boolean read FResolveFlats write FResolveFlats ;
  end ;

  /// <summary>
  ///   The class that creates a flow accumulation grid from a flow direction grid.
  /// </summary>
  T_HydrologyFlowAccumulation = class( T_HydrologyAbstract )

    public
      /// <inheritdoc/>
      constructor Create ;

    public
      /// <summary>
      ///   Generates a flow accumulation grid from a flow direction grid.
      /// </summary>
      /// <param name="_flowDir">
      ///   an input flow direction grid
      /// </param>
      /// <param name="_extent">
      ///   an extent to be processed
      /// </param>
      /// <param name="_flowAcc">
      ///   the output grid layer
      /// </param>
      procedure Generate( const _flowDir : TGIS_LayerPixel ;
                          const _extent  : TGIS_Extent ;
                          const _flowAcc : TGIS_LayerPixel
                        ) ;
  end;

  /// <summary>
  ///   Tha class that creates drainage basins from a flow directions grid.
  /// </summary>
  T_HydrologyBasin = class( T_HydrologyAbstract )
    public
      /// <inheritdoc/>
      constructor Create ;

    public
      /// <summary>
      ///   Generates a basin grid from a flow direction grid.
      /// </summary>
      /// <param name="_flowDir">
      ///   an input flow direction grid
      /// </param>
      /// <param name="_extent">
      ///   an extent to be processed
      /// </param>
      /// <param name="_basins">
      ///   the output grid layer
      /// </param>
      procedure Generate( const _flowDir : TGIS_LayerPixel ;
                          const _extent  : TGIS_Extent ;
                          const _basins  : TGIS_LayerPixel
                        ) ;

    public
      /// <summary>
      ///   Use this property to limit basins with a small number of cells.
      /// </summary>
      property Threshold : Integer read FThreshold write FThreshold ;
  end;

  /// <summary>
  ///   The class that creates a watershed grid from the flow directions grid
  ///   and pour points.
  /// </summary>
  T_HydrologyWatershed = class( T_HydrologyAbstract )

    private
      procedure prepareOutletsPixelLayer ( const _outlets : TGIS_LayerPixel ;
                                           const _flowDir : TGIS_LayerPixel
                                         ) ;

    public
      /// <inheritdoc/>
      constructor Create ;

    public
      /// <summary>
      ///   Generates a watershed grid from a flow direction grid and vector
      ///   layer with pour points.
      /// </summary>
      /// <param name="_flowDir">
      ///   an input flow direction grid
      /// </param>
      /// <param name="_outlets">
      ///   an input vector layer with outlets (pour points)
      /// </param>
      /// <param name="_field">
      ///   a numerical field from _outlets to use in ordering watersheds;
      ///   if empty GIS_UID is used
      /// </param>
      /// <param name="_extent">
      ///   an extent to be processed
      /// </param>
      /// <param name="_watershed">
      ///   the output grid layer
      /// </param>
      /// <remarks>
      ///   The precision of locating pour points is important. Outlets should
      ///   be located to cells of high accumulated flow.
      /// </remarks>
      procedure Generate( const _flowDir   : TGIS_LayerPixel ;
                          const _outlets   : TGIS_LayerVector ;
                          const _field     : String ;
                          const _extent    : TGIS_Extent ;
                          const _watershed : TGIS_LayerPixel
                        ) ; overload ;

      /// <summary>
      ///   Generates a watershed grid from a flow direction grid and grid
      ///   layer with pour points.
      /// </summary>
      /// <param name="_flowDir">
      ///   an input flow direction grid
      /// </param>
      /// <param name="_outlets">
      ///   an input vector layer with pour points;
      /// </param>
      /// <param name="_extent">
      ///   an extent to be processed
      /// </param>
      /// <param name="_watershed">
      ///   the output grid layer
      /// </param>
      /// <remarks>
      ///   The precision of locating pour points is important. Outlets should
      ///   be located to cells of high accumulated flow.
      /// </remarks>
      procedure Generate( const _flowDir    : TGIS_LayerPixel ;
                          const _outlets    : TGIS_LayerPixel ;
                          const _extent     : TGIS_Extent ;
                          const _watershed  : TGIS_LayerPixel
                        ) ; overload ;
  end;

  /// <summary>
  ///   The class that converts a stream network to polyines.
  /// </summary>
  T_HydrologyStreamToPolyline = class( T_HydrologyAbstract )
    public
      /// <inheritdoc/>
      constructor Create ;

    public
      /// <summary>
      ///   Converts a stream network to polylines.
      /// </summary>
      /// <param name="_flowDir">
      ///   an input flow direction grid
      /// </param>
      /// <param name="_streamsPix">
      ///   an input grid representing a stream network
      /// </param>
      /// <param name="_extent">
      ///   an extent to be processed
      /// </param>
      /// <param name="_streamsVec">
      ///   the output vector layer
      /// </param>
      /// <param name="_orderField">
      ///   an optional numerical field from _streamsVec to populate stream
      ///   network value (flow accumulation, or stream order value from grid);
      ///   default argument is empty string, this means that no attribute
      ///   information is transferred to vector layer
      /// </param>
      /// <remarks>
      ///   The stream network is represented as flow accumulation, or
      ///   stream order grid.
      /// </remarks>
      procedure Generate( const _flowDir    : TGIS_LayerPixel ;
                          const _streamsPix : TGIS_LayerPixel ;
                          const _extent     : TGIS_Extent ;
                          const _streamsVec : TGIS_LayerVector ;
                          const _orderField : String
                        ) ;

    public
      /// <summary>
      ///   Use this property to limit processed cells from stream network.
      /// </summary>
      /// <remarks>
      ///   If the stream network is represented by a flow accumulation grid,
      ///   recommended threshold is 1% of the maximum value from the flow
      ///   accumulation grid.
      /// </remarks>
      property Threshold : Integer read FThreshold write FThreshold ;
  end;

  /// <summary>
  ///   The class that creates a stream order.
  /// </summary>
  T_HydrologyStreamOrder = class( T_HydrologyAbstract )
    private
      FMethod : TGIS_HydrologyStreamOrderMethod ;

    public
      /// <inheritdoc/>
      constructor Create ;

    public
      /// <summary>
      ///   Generates a stream order grid from a flow direction and flow
      ///   grids and specified method.
      /// </summary>
      /// <param name="_flowDir">
      ///   an input flow direction grid
      /// </param>
      /// <param name="_flowAcc">
      ///   an input flow accumulation grid
      /// </param>
      /// <param name="_extent">
      ///   an extent to be processed
      /// </param>
      /// <param name="_streams">
      ///   the output grid layer
      /// </param>
      procedure Generate( const _flowDir : TGIS_LayerPixel ;
                          const _flowAcc : TGIS_LayerPixel ;
                          const _extent  : TGIS_Extent ;
                          const _streams : TGIS_LayerPixel
                        ) ;

    public
      /// <summary>
      ///   Stream ordering method. Strahler is default.
      /// </summary>
      property &Method   : TGIS_HydrologyStreamOrderMethod
                           read FMethod
                           write FMethod ;

      /// <summary>
      ///   Use this property to limit processed cells from the flow accumulation
      ///   grid.
      /// </summary>
      /// <remarks>
      ///   Recommended threshold is 1% of the maximum value from the flow
      ///   accumulation grid.
      /// </remarks>
      property Threshold : Integer
                           read FThreshold
                           write FThreshold ;
  end;
{$ENDREGION}

// Low-level hydrological algorithms //

{$REGION 'T_HydrologyAlgorithmAbtract'}
type
  T_HydrologyAlgorithmAbstract = class
    protected
      busyEventManager : TGIS_BusyEventManager ;

    public
      constructor Create( const _busyEventManager : TGIS_BusyEventManager ) ;
end;

constructor T_HydrologyAlgorithmAbstract.Create(
  const _busyEventManager : TGIS_BusyEventManager
) ;
begin
  busyEventManager := _busyEventManager ;
end;
{$ENDREGION}

{$REGION 'T_FlowDirection'}
type
  /// <summary>
  ///   Specifies the flow direction based algorithm
  /// </summary>
  T_FlowDirectionAlgorithm = (
    /// <summary>
    ///   Flow Direction
    /// </summary>
    FlowDirection,
    /// <summary>
    ///   Sink
    /// </summary>
    Sink
  ) ;

  /// <summary>
  ///   The helper class used by FlowDirection and Sink algorithms
  /// </summary>
  T_FlowDirection = class( T_HydrologyAlgorithmAbstract )
    const
      //? consider introducing this property for FlowDirections
      EDGE_CELLS_FLOW_OUTWARDS = False ;

      // max recursion depth for ExpandSink method
      MAX_DEPTH = 4096;

    private
      resolveFlat         : Boolean ;
      useFlowDirAlgorithm : Boolean ;

    public
      /// <summary>
      ///   Decodes cell row & column from a big number (unhashing).
      /// </summary>
      /// <param name="_value">
      ///   hashed cell position
      /// </param>
      /// <returns>
      ///   returns decoded col & row as TPoint
      /// </returns>
      function DecodeCellPosition(
        const _value               : UInt64
      ) : TPoint ; {$IFDEF GIS_INLINE} inline ; {$ENDIF}

      /// <summary>
      ///   Determines the Flow Direction across flat areas.
      /// </summary>
      /// <param name="_demLockEx">
      ///   an instance of pixel lock adapter for a dem grid
      /// </param>
      /// <param name="_outLockEx">
      ///   an instance of pixel lock adapter for an output grid
      /// </param>
      procedure DetermineFlowDirAcrossFlats(
        const _demLockEx           : T_PixelLockAdapter ;
        const _outLockEx           : T_PixelLockAdapter
      ) ;

      /// <remarks>
      ///   Original algorithm proposition not used (left for historical reasons).
      ///   Replaced by ResolveFlats.
      /// </remarks>
      /// <param name="_demLockEx">
      ///   an instance of pixel lock adapter for a dem grid
      /// </param>
      /// <param name="_outLockEx">
      ///   an instance of pixel lock adapter for an output grid
      /// </param>
      /// <param name="_undefinedCells">
      ///   queue of the undefined cells
      /// </param>
      /// <param name="_dropGrd">
      ///? need to be documented
      /// </param>
      procedure DetermineFlowDirForUndefinedCells(
        const _demLockEx           : T_PixelLockAdapter ;
        const _outLockEx           : T_PixelLockAdapter ;
        const _undefinedCells      : TQueue< TPoint > ;
        const _dropGrd             : TGIS_GridArray
      ) ;

      /// <summary>
      ///   Encodes cell row & column to a big number (hashing).
      /// </summary>
      /// <param name="_row">
      ///   a cell y-position (row)
      /// </param>
      /// <param name="_column">
      ///   a cell x-position (column)
      /// </param>
      /// <returns>
      ///   hashed cell position
      /// </returns>
      function EncodeCellPosition(
        const _row                 : Integer ;
        const _column              : Integer
      ) : UInt64 ; {$IFDEF GIS_INLINE} inline ; {$ENDIF}

      /// <summary>
      ///   Recursively expands a sink.
      /// </summary>
      /// <param name="_outLockEx">
      ///    an instance of pixel lock adapter for an output grid
      /// </param>
      /// <param name="_notExpandedCellsDct">
      ///   a dictionary for cells that were not expanded
      ///   due to the maximum recursion depth limit (MAX_DEPTH)
      /// </param>
      /// <param name="_potentialSinksLst">
      ///   a list for cells detected as sinks
      /// </param>
      /// <param name="_visitedCellsDct">
      ///   a dictionary for already visited cells by a recursive algorithm
      /// </param>
      /// <param name="_row">
      ///   starting cell y-position
      /// </param>
      /// <param name="_col">
      ///   starting cell z-position
      /// </param>
      /// <param name="_depth">
      ///   current recursion depth
      /// </param>
      /// <returns>
      ///   True, if an algorithm detects a sink, otherwise False.
      /// </returns>
      function ExpandSink(
        const _outLockEx           : T_PixelLockAdapter ;
        const _notExpandedCellsDct : TDictionary< UInt64, Integer > ;
        const _potentialSinksLst   : TList< TPoint > ;
        const _visitedCellsDct     : TDictionary< UInt64, Integer > ;
        const _row                 : Integer ;
        const _col                 : Integer ;
        var   _depth               : Integer
      ) : Boolean ;

      /// <summary>
      ///   Extracts location codes [1,2,4,...,128] from multiple-direction flows.
      /// </summary>
      /// <param name="_dirCode">
      ///   a direction code
      /// </param>
      /// <returns>
      ///   an array of base direction codes
      /// </returns>
      function ExtractLocationCodesFromDir(
        const _dirCode             : Integer
      ) : TArray< Integer > ;

      /// <summary>
      ///   Finds flat sinks.
      /// </summary>
      /// <param name="_outLockEx">
      ///    an instance of pixel lock adapter for an output grid
      /// </param>
      /// <param name="_undefinedCells">
      ///   queue of the undefined cells
      /// </param>
      /// <param name="_sinkId">
      ///   current sink ID
      /// </param>
      procedure FindFlatSinks(
        const _outLockEx           : T_PixelLockAdapter ;
        const _undefinedCells      : TQueue< TPoint > ;
        var   _sinkId              : Integer
      ) ;

      /// <summary>
      ///   Fills boundary cells.
      /// </summary>
      /// <param name="_demLockEx">
      ///    an instance of pixel lock adapter for a DEM grid
      /// </param>
      /// <param name="_outLockEx">
      ///    an instance of pixel lock adapter for an output grid
      /// </param>
      /// <param name="_lockTiler">
      ///    an instance of T_PixelLockTiler
      /// </param>
      /// <param name="_undefinedCells">
      ///   queue of the undefined cells
      /// </param>
      /// <param name="_sinkId">
      ///   current sink ID
      /// </param>
      procedure FillBoundaryCells(
        const _demLockEx           : T_PixelLockAdapter ;
        const _outLockEx           : T_PixelLockAdapter ;
        const _lockTiler           : T_PixelLockTiler ;
        const _undefinedCells      : TQueue< TPoint > ;
        var   _sinkId              : Integer
      ) ;

      /// <summary>
      ///   Labels sinks.
      /// </summary>
      /// <param name="_outLockEx">
      ///    an instance of pixel lock adapter for an output grid
      /// </param>
      /// <param name="_sinks">
      ///    a dictionary where the key is a hashed cell position
      ///    and the value is a sink ID
      /// </param>
      /// <param name="_setNodata">
      ///   If True, set NODATA instead of the value from a dictionary;
      ///   the default value is False
      /// </param>
      procedure LabelSinks(
        const _outLockEx           : T_PixelLockAdapter ;
        const _sinks               : TDictionary< UInt64, Integer > ;
        const _setNodata           : Boolean = False
      ) ;

      /// <summary>
      ///   Sets Flow Direction or NODATA
      /// </summary>
      /// <param name="_row">
      ///   starting cell y-position
      /// </param>
      /// <param name="_col">
      ///   starting cell z-position
      /// </param>
      /// <param name="_borderCell">
      ///   if True, set values only for border cells
      /// </param>
      /// <param name="_demLockEx">
      ///   an instance of pixel lock adapter for a DEM grid
      /// </param>
      /// <param name="_outLockEx">
      ///   an instance of pixel lock adapter for an output grid
      /// </param>
      /// <param name="_undefinedCells">
      ///   queue of the undefined cells
      /// </param>
      /// <param name="_sinkId">
      ///   current sink ID
      /// </param>
      function SetFlowDirOrNodata(
        const _row                 : Integer ;
        const _col                 : Integer ;
        const _borderCell          : Boolean ;
        const _demLockEx           : T_PixelLockAdapter ;
        const _outLockEx           : T_PixelLockAdapter ;
        const _undefinedCells      : TQueue< TPoint > ;
        var   _sinkId              : Integer
      ) : Boolean ;

      /// <inheritdoc/>
      constructor Create(
        const _busyEventManager    : TGIS_BusyEventManager ;
        const _algorithm           : T_FlowDirectionAlgorithm
      ) ;

      /// <summary>
      ///   Creates FlowDirection or Sink grid layer from DEM.
      /// </summary>
      /// <param name="_dem">
      ///   a digital elevation model as a grid layer
      /// </param>
      /// <param name="_extent">
      ///   an extent to be processed
      /// </param>
      /// <param name="_out">
      ///   an output grid layer;
      ///   this layer must have the same CS and resolution as _dem
      /// </param>
      /// <param name="_resolveFlat">
      ///   FlowDirection variant.
      ///   If true, the longer and iterative process of calculation
      ///   is performed for cells within flat areas, where the dominant
      ///   direction cannot be determined.
      ///   If false, above mentioned cells have encoded the direction
      ///   by summing direction codes of neighbors with the drop value equals 0.
      ///   This is the default.
      /// </param>
      procedure Generate(
        const _dem         : TGIS_LayerPixel ;
        const _extent      : TGIS_Extent ;
        const _out         : TGIS_LayerPixel ;
        const _resolveFlat : Boolean = TGIS_Hydrology.FLOWDIR_RESOLVE_FLAT
      ) ;
  end ;

constructor T_FlowDirection.Create(
  const _busyEventManager : TGIS_BusyEventManager ;
  const _algorithm        : T_FlowDirectionAlgorithm
) ;
begin
  inherited Create( _busyEventManager ) ;
  resolveFlat := False ;
  useFlowDirAlgorithm := ( _algorithm = T_FlowDirectionAlgorithm.FlowDirection ) ;
end;

function T_FlowDirection.ExtractLocationCodesFromDir(
  const _dirCode : Integer
) : TArray< Integer > ;
var
  codes   : TList< Integer > ;
  code    : Integer ;
  div_val : Integer ;
  mod_val : Integer ;
begin
  codes := TList< Integer >.Create ;
  try
    mod_val := _dirCode ;
    code := 128 ;
    while ( mod_val <> 0 ) do begin
      div_val := mod_val div code ;
      if ( div_val = 1 ) then
        codes.Add( code ) ;
      mod_val := ( mod_val - code * div_val ) mod code ;
      code := code div 2 ;
    end ;
    SetLength( Result, codes.Count ) ;
    Result := codes.ToArray ;
  finally
    FreeObject( codes ) ;
  end ;
end;

function T_FlowDirection.EncodeCellPosition(
  const _row    : Integer ;
  const _column : Integer
) : UInt64 ;
begin
  Result := UInt64( _row ) shl 32 + UInt64( _column ) ;
end;

function T_FlowDirection.DecodeCellPosition(
  const _value : UInt64
) : TPoint ;
var
  r, c : Integer ;
begin
  r := _value shr 32 ;
  c := _value - ( UInt64( r ) shl 32 ) ;
  Result := Point( c, r ) ;
end;



procedure T_FlowDirection.FillBoundaryCells(
  const _demLockEx      : T_PixelLockAdapter ;
  const _outLockEx      : T_PixelLockAdapter ;
  const _lockTiler      : T_PixelLockTiler ;
  const _undefinedCells : TQueue< TPoint > ;
  var  _sinkId          : Integer
) ;
const
  DIR_E  = 1 ;
  DIR_SE = 2 ;
  DIR_S  = 4 ;
  DIR_SW = 8 ;
  DIR_W  = 16 ;
  DIR_NW = 32 ;
  DIR_N  = 64 ;
  DIR_NE = 128 ;
var
  c, r, c_start, c_end, r_start, r_end : Integer ;
  tile_rect, ext_rect : TRect ;

  procedure fill_boundary_cell(
    const _row     : Integer ;  // Y-position
    const _col     : Integer ;  // X-position
    const _dirCode : Integer
  ) ;
  begin
    if SetFlowDirOrNodata(
        _row, _col,
        True,
        _demLockEx, _outLockEx,
        _undefinedCells,
        _sinkId
      )
    then
      exit ;

    if useFlowDirAlgorithm then
      _outLockEx.GridRel[_row, _col] := _dirCode ;
  end;
begin
  tile_rect := _lockTiler.CurrentRect ;
  ext_rect := _lockTiler.ExtentRect ;

  // omit tile corners, beside the grid "real"| corners
  if ( tile_rect.Top = 0 ) then
    r_start := 0
  else
    r_start := 1 ;

  if ( tile_rect.Bottom = ext_rect.Bottom ) then
    r_end := _demLockEx.NRows-1
  else
    r_end := _demLockEx.NRows-2 ;

  // the edge left (first column in left tiles)
  if ( tile_rect.Left = ext_rect.Left ) then begin
    c := 0 ;
    for r := r_start to r_end do
      fill_boundary_cell( r, c, DIR_W ) ;
  end ;

  // right (last column in right tiles)
  if ( tile_rect.Right = ext_rect.Right ) then begin
    c := _demLockEx.NCols-1 ;
    for r := r_start to r_end do
      fill_boundary_cell( r, c, DIR_E ) ;
  end ;

  // omit tile corners, beside the grid "real"| corners
  if ( tile_rect.Left = 0 ) then
    c_start := 0
  else
    c_start := 1 ;

  if ( tile_rect.Right = ext_rect.Right ) then
    c_end := _demLockEx.NCols-1
  else
    c_end := _demLockEx.NCols-2 ;

  // top (first row in top tiles)
  if ( tile_rect.Top = ext_rect.Top ) then begin
    r:= 0 ;
    for c:= c_start to c_end do
      fill_boundary_cell( r, c, DIR_N ) ;
  end ;

  // bottom (last row in bottom tiles)
  if ( tile_rect.Bottom = ext_rect.Bottom ) then begin
    r := _demLockEx.NRows-1 ;
    for c:= c_start to c_end do
      fill_boundary_cell( r, c, DIR_S ) ;
  end ;
end;

procedure T_FlowDirection.Generate(
  const _dem         : TGIS_LayerPixel ;
  const _extent      : TGIS_Extent ;
  const _out         : TGIS_LayerPixel ;
  const _resolveFlat : Boolean
) ;
const
  MAX_TILE_SIZE = 4096 ;
  TILE_OFFSET   = 1 ;
var
  lock_dem          : TGIS_LayerPixelLock ;
  lock_out          : TGIS_LayerPixelLock ;

  lockEx_dem        : T_PixelLockAdapter ;
  lockEx_out        : T_PixelLockAdapter ;

  row, col          : Integer ;

  sink_id           : Integer ;
  stages_count      : Integer ;

  undefined_cells_q : TQueue< TPoint > ;

  i_tile            : Integer ;
  tiles_count       : Integer ;
  tile_rect         : TRect ;
  lock_iterator     : T_PixelLockTiler ;
begin
  // remember _resolveFlats for Sink is False!
  resolveFlat := _resolveFlat ;

  _dem.Open ;
  _out.Open ;

  _out.MinHeight := GIS_MAX_SINGLE ;
  _out.MaxHeight := -GIS_MAX_SINGLE ;

  lock_iterator := T_PixelLockTiler.Create( _dem, _extent, MAX_TILE_SIZE, TILE_OFFSET ) ;
  tiles_count := lock_iterator.Count ;
  // main busy event stage
  busyEventManager.StartEvent(
    _rsrc( GIS_RS_BUSY_HYDROLOGY_DIRECTIONS ),
    tiles_count
  ) ;
  // for cells with undefined flow or sink
  undefined_cells_q := TQueue< TPoint >.Create ;
  try
    i_tile := 0 ;
    sink_id := 0 ;
    while lock_iterator.Next do begin
      inc( i_tile ) ;
      tile_rect := lock_iterator.CurrentRect ;

      lock_dem := _dem.LockPixels( tile_rect, False ) ;
      lock_out := _out.LockPixels( tile_rect, True  ) ;

      lockEx_dem := T_PixelLockAdapter.Create( lock_dem ) ;
      lockEx_out := T_PixelLockAdapter.Create( lock_out ) ;

      if resolveFlat or not useFlowDirAlgorithm then begin
        // drop does not need an offset because it is a plain grid
        stages_count := 2 ;
        undefined_cells_q.Clear ;
      end
      else
        stages_count := 1 ;

      // processing tiles
      busyEventManager.StartEvent(
        Format( _rsrc( GIS_RS_BUSY_TILING_TILEIOFN ), [i_tile, tiles_count] ),
        stages_count
      ) ;
      try
        // main algorithm - all cells
        busyEventManager.StartEvent(
          _rsrc( GIS_RS_BUSY_PROCESSING_CELLS ),
          lockEx_dem.NRows-2
        ) ;
        try
          // for all cells on the edges assign the flow direction to flow to
          FillBoundaryCells(
            lockEx_dem,
            lockEx_out,
            lock_iterator,
            undefined_cells_q,
            sink_id
          ) ;

          // for the remaining cells compute the distance-weighted drop in elevation...
          for row := 1 to lockEx_dem.NRows-2 do begin
            if busyEventManager.PushEvent then
              exit ;

            for col := 1 to lockEx_dem.NCols-2 do begin
              SetFlowDirOrNodata(
                row, col,
                False,
                lockEx_dem, lockEx_out,
                undefined_cells_q,
                sink_id
              ) ;
            end ;
          end ;
        finally
          busyEventManager.EndEvent ;
        end ;

        // all cells are determined
        if T_HUtils.QueueIsEmpty( undefined_cells_q ) then
          continue ;

        // fill all undefined cells in an iterative process
        if useFlowDirAlgorithm then begin
          // if undefined_cells is not empty flat areas detected
          // resolve it using ResolveFlats algorithm
          if resolveFlat then
            DetermineFlowDirAcrossFlats( lockEx_dem, lockEx_out ) ;
        end
        else begin
          FindFlatSinks( lockEx_out, undefined_cells_q, sink_id ) ;
        end ;
      finally
        FreeObject( lockEx_dem ) ;
        FreeObject( lockEx_out ) ;
        _dem.UnlockPixels( lock_dem ) ;
        _out.UnlockPixels( lock_out ) ;
        busyEventManager.EndEvent ;
      end ;
    end ;
  finally
    FreeObject( undefined_cells_q ) ;
    FreeObject( lock_iterator ) ;
    busyEventManager.EndEvent ;
  end ;
end;

function T_FlowDirection.SetFlowDirOrNodata(
  const _row            : Integer ;
  const _col            : Integer ;
  const _borderCell     : Boolean ;
  const _demLockEx      : T_PixelLockAdapter ;
  const _outLockEx      : T_PixelLockAdapter ;
  const _undefinedCells : TQueue< TPoint > ;
  var   _sinkId         : Integer
) : Boolean ;
const
  DIST_ORTHOGONAL = 1.0 ;
  DIST_DIAGONAL   = 1.414 ;
  // starts from east and go clockwise
  DIST_FACTORS    : array [0..NEIGHBOR_COUNT-1] of Double
                  = ( DIST_ORTHOGONAL,
                      DIST_DIAGONAL,
                      DIST_ORTHOGONAL,
                      DIST_DIAGONAL,
                      DIST_ORTHOGONAL,
                      DIST_DIAGONAL,
                      DIST_ORTHOGONAL,
                      DIST_DIAGONAL ) ;
var
  rn, cn            : Integer ;
  dir_id            : Integer ;
  drop_max_count    : Integer ;
  drop_max_dir      : Integer ;
  last_border_dir   : Integer ;
  last_drop_max_dir : Integer ;
  elev, elev_n      : Single ;
  dist_factor       : Double ;
  drop, drop_max    : Double ;
  is_border         : Boolean ;
begin
  Result := False ;
  // skip nodata
  if _demLockEx.CellIsNodataElseGetValue( _row, _col, elev ) then begin
    _outLockEx.SetNodata( _row, _col ) ;
    Result := True ;
    exit ;
  end ;

  // fill Sink with NODATA by default (the most likely case)
  if not useFlowDirAlgorithm then
    _outLockEx.SetNodata( _row, _col ) ;

  // initialize internal variables
  drop_max := -GIS_MAX_DOUBLE ;
  drop_max_dir := 0 ;
  last_drop_max_dir := 0 ;
  drop_max_count := 0 ;
  is_border := False ;
  last_border_dir := -1 ;

  // ...to each of the cell's eight neighbors
  for dir_id := 0 to NEIGHBOR_COUNT-1 do begin
    // tricky way to get neighbor index
    cn := _col + DC_NEIGHBOR[dir_id] ;
    rn := _row + DR_NEIGHBOR[dir_id] ;

    if _borderCell then begin
      if not _demLockEx.CellIsInGrid( rn, cn ) then begin
        is_border := True ;
        last_border_dir := T_HUtils.IndexToDirection( dir_id ) ;
        continue ;
      end ;
    end ;

    // cells surrounded by NODATA are treated as EDGES!
    // if cell is a border immediately stop scanning neighbors
    // and set direction outside of the DEM (first NODATA occurence)
    if _demLockEx.CellIsNodataElseGetValue( rn, cn, elev_n )then begin
      is_border := True ;
      last_border_dir := T_HUtils.IndexToDirection( dir_id ) ;
      // break immediately, if edge cells flow outwards,
      // otherwise try to find positive flow direction
      if EDGE_CELLS_FLOW_OUTWARDS then
        break
      else
        continue ;
    end ;

    dist_factor := DIST_FACTORS[dir_id] ;
    // examine the drop value to determine the neighbor(s)
    // with the largest drop
    drop := ( elev - elev_n ) / dist_factor ;

    if GisIsSameValue( drop, drop_max, GIS_DOUBLE_RESOLUTION ) then begin
      // include duplicates
    end
    else if ( drop > drop_max ) then begin
      drop_max := drop ;

      // the new largest drop clears duplicates
      drop_max_dir := 0 ;
      drop_max_count := 0 ;
    end
    else
      continue ;

    // encode neighbor direction and increment variables
    last_drop_max_dir := T_HUtils.IndexToDirection( dir_id ) ;

    // 3a. If the largest drop < 0, assign a negative flow direction
    // to indicate undefined
    if ( drop_max < -GIS_DOUBLE_RESOLUTION ) then
      last_drop_max_dir := -last_drop_max_dir ;

    inc( drop_max_dir, last_drop_max_dir ) ;
    inc( drop_max_count ) ;
  end ;

  // FlowDirection algorithm
  if useFlowDirAlgorithm then begin
    if is_border then begin
      if EDGE_CELLS_FLOW_OUTWARDS then begin
        // use first occurence of border to set the flow direction
        _outLockEx.GridRel[_row, _col] := T_HUtils.IndexToDirection( dir_id ) ;
        exit ;
      end
      else begin
        // if no specific flow direction determined,
        // for real borders assign outwards flow direction,
        // otherwise use last occurence of border
        if not ( ( drop_max_count = 1 ) and
                 ( drop_max > 0 ) ) then
        begin
          _outLockEx.GridRel[_row, _col] := last_border_dir ;
          exit ;
        end ;
      end ;
    end ;
    // 3b. If the largest drop >= 0 and occurs at only one neighbor,
    // assign the flow direction to that neighbor
    if drop_max_count = 1 then begin
      _outLockEx.GridRel[_row, _col] := drop_max_dir ;

      if ( drop_max_dir >= 0 ) then
        Result := True ;
      // otherwise negative flow direction (3a)
    end
    // rare case for single cell surronded by NODATA
    else if drop_max_count = 0 then begin
      _outLockEx.GridRel[_row, _col] := 0 ;
    end
    else begin
      // 3d. If the largest drop = 0 and occurs at more than one neighbor,
      // encode the locations of those neighbors by summing their neighbor
      // location codes. The cell is located in a flat area.
      if GisIsSameValue( drop_max, 0, GIS_DOUBLE_RESOLUTION ) then begin
        _outLockEx.GridRel[_row, _col] := drop_max_dir ;
        if resolveFlat then
          _undefinedCells.Enqueue( Point( _col, _row ) ) ;
      end
      // 3c. If the |largest drop| > 0 and occurs at more than one neighbor,
      // assign the flow directions logically according to a table look-up.
      // Now, arbitrarily chosen at the last occurence of drop_max.
      else begin
        _outLockEx.GridRel[_row, _col] := last_drop_max_dir ;
      end ;
    end ;
  end
  // Sink algorithm
  else if ( not is_border ) and ( last_drop_max_dir < -GIS_DOUBLE_RESOLUTION ) then begin
    // assign Sink
    _outLockEx.GridRel[_row, _col] := _sinkId ;
    inc( _sinkId ) ;
  end
  // flat detected, sink can not be determined now
  else if GisIsSameValue( drop_max, 0, GIS_DOUBLE_RESOLUTION ) then begin
    // temporarily use out grid to store direction
    _outLockEx.GridRel[_row, _col] := drop_max_dir ;
    _undefinedCells.Enqueue( Point( _col, _row ) )
  end ;
end;

procedure T_FlowDirection.DetermineFlowDirForUndefinedCells(
  const _demLockEx      : T_PixelLockAdapter ;
  const _outLockEx      : T_PixelLockAdapter ;
  const _undefinedCells : TQueue< TPoint > ;
  const _dropGrd        : TGIS_GridArray
) ;
var
  cell              : TPoint ;
  c, r, cn, rn      : Integer ;
  dir_id            : Integer ;
  neighbor_dir_id   : Integer ;
  drop_max_count    : Integer ;
  last_drop_max_dir : Integer ;
  elev, elev_n      : Single ;
  drop              : Single ;
  drop_max          : Double ;
  drop_nodata       : Single ;
begin
  drop_nodata := GIS_GRID_NOVALUE ;  //? remove in future

  busyEventManager.StartEvent(
    _rsrc( GIS_RS_BUSY_PROCESSING_CELLS ),
    _undefinedCells.Count
  ) ;
  try
    while _undefinedCells.Count > 0 do begin
      cell := _undefinedCells.Dequeue ;

      // initialize internal variables
      c := cell.X ;
      r := cell.Y ;
      drop_max := -GIS_MAX_DOUBLE ;
      drop_max_count := 0 ;
      last_drop_max_dir := 0 ;
      elev := _demLockEx.GridRel[r, c] ;

      // examine the neighbor cells with the largest drop,
      // and the neighbor does not flow to the center cell,
      // assign the center cell a flow direction which flows to this neighbor
      for dir_id := 0 to NEIGHBOR_COUNT-1 do begin

        cn := c + DC_NEIGHBOR[dir_id] ;
        rn := r + DR_NEIGHBOR[dir_id] ;

        // neighbor's flow direction
        neighbor_dir_id := RoundS( _outLockEx.GridRel[r, c] ) ;

        // reversed direction is needed
        // because flow direction can't point back to the tested cell
        if T_HUtils.IndexToDirection( dir_id ) =
           T_HUtils.ReverseDirection( neighbor_dir_id )
        then
          continue ;

        // start with comparing elevations to avoid infinitive loops in flat areas
        elev_n := _demLockEx.GridRel[rn, cn] ;
        if GisIsSameValue( elev_n, elev, GIS_SINGLE_RESOLUTION ) then begin
          drop_max_count := 1 ;
          drop_max := 0 ;
          last_drop_max_dir := T_HUtils.IndexToDirection( dir_id ) ;
          break ;
        end ;

        // the drop value is compared
        drop := _dropGrd[rn, cn] ;

        // skip NODATA
        if GisIsSameValue( drop, drop_nodata, GIS_SINGLE_RESOLUTION ) then
          continue ;

        if GisIsSameValue( drop, drop_max, GIS_DOUBLE_RESOLUTION ) then begin
          // include duplicates
        end
        else if ( drop > drop_max ) then begin
          drop_max := drop ;

          // the new largest drop clears duplicates
          drop_max_count := 0 ;
          last_drop_max_dir := neighbor_dir_id ;
        end
        else
          continue ;

        inc( drop_max_count ) ;
      end ;

      // if direction not set, give back a cell to the queue
      if drop_max_count = 0 then
        _undefinedCells.Enqueue( cell )
      else begin
        _outLockEx.GridRel[r, c] := last_drop_max_dir; //? was drop_max_dir ;
        _dropGrd[r, c] := drop_max ;
        if busyEventManager.PushEvent then
          exit ;
      end ;
    end ;
  finally
    busyEventManager.EndEvent ;
  end ;
end;

procedure T_FlowDirection.DetermineFlowDirAcrossFlats(
  const _demLockEx : T_PixelLockAdapter ;
  const _outLockEx : T_PixelLockAdapter
) ;
var
  flat_resolver : T_HydrologyResolveFlat ;
begin
  flat_resolver := T_HydrologyResolveFlat.Create ;
  try
    flat_resolver.busyEventManager := busyEventManager ;
    flat_resolver.generateEx( _demLockEx, _outLockEx, nil ) ;
  finally
    FreeObject( flat_resolver ) ;
  end ;
end;


procedure T_FlowDirection.FindFlatSinks(
  const _outLockEx      : T_PixelLockAdapter ;
  const _undefinedCells : TQueue< TPoint > ;
  var   _sinkId         : Integer
) ;
var
  is_detected_sink       : Boolean ;
  depth                  : Integer ;
  c, r                   : Integer ;
  cell                   : TPoint ;
  encoded_cell           : UInt64 ;
  potential_sinks_lst    : TList< TPoint > ;
  visited_cells_dct      : TDictionary< UInt64, Integer > ;
  detected_sinks_dct     : TDictionary< UInt64, Integer > ;
  not_expanded_cells_dct : TDictionary< UInt64, Integer > ;
begin
  busyEventManager.StartEvent(
    _rsrc( GIS_RS_BUSY_PROCESSING_CELLS ),
    _undefinedCells.Count
  ) ;

  not_expanded_cells_dct := TDictionary< UInt64, Integer >.Create ;
  potential_sinks_lst := TList< TPoint >.Create ;
  visited_cells_dct := TDictionary< UInt64, Integer >.Create ;
  detected_sinks_dct := TDictionary< UInt64, Integer >.Create ;
  try
    while _undefinedCells.Count > 0 do begin

      if busyEventManager.PushEvent then
        exit ;

      cell := _undefinedCells.Dequeue ;
      c := cell.X ;
      r := cell.Y ;

      encoded_cell := EncodeCellPosition( r, c ) ;
      if detected_sinks_dct.ContainsKey( encoded_cell ) then
        continue ;

      potential_sinks_lst.Clear ;
      visited_cells_dct.Clear ;
      not_expanded_cells_dct.Clear ;
      is_detected_sink := False ;

      // treat current undefined cell as a first not expanded cell
      not_expanded_cells_dct.Add( encoded_cell, 1 ) ;

      // inner while loop is necessary to ensure the continuity
      // of the Expand Sink algorithm after reaching the recursion depth limit
      while not_expanded_cells_dct.Count > 0 do begin

        // get first not expanded cell...
        {$IFDEF JAVA}
        encoded_cell := UInt64(not_expanded_cells_dct.Keys.ToArray[0]);
        {$ELSE}
        encoded_cell := not_expanded_cells_dct.Keys.ToArray[0];
        {$ENDIF}
        cell := DecodeCellPosition( encoded_cell );
        c := cell.X ;
        r := cell.Y ;

        // ...and remove it from a dictionary
        not_expanded_cells_dct.Remove( encoded_cell ) ;

        // start recursive ExpandSink algorithm
        depth := 0;
        if ExpandSink(
          _outLockEx,
          not_expanded_cells_dct,
          potential_sinks_lst,
          visited_cells_dct,
          r, c,
          depth
        ) then begin
          is_detected_sink := True ;
        end ;
      end ;

      if is_detected_sink then begin
        for cell in potential_sinks_lst do begin
          c := cell.X ;
          r := cell.Y ;
          detected_sinks_dct.AddOrSetValue( EncodeCellPosition( r, c ), _sinkId ) ;
        end ;
        inc( _sinkId ) ;
      end
      else begin
        // assign NODATA
        LabelSinks( _outLockEx, visited_cells_dct, True ) ;
      end ;
    end ;

    LabelSinks( _outLockEx, detected_sinks_dct ) ;
  finally
    FreeObject( not_expanded_cells_dct ) ;
    FreeObject( potential_sinks_lst ) ;
    FreeObject( visited_cells_dct ) ;
    FreeObject( detected_sinks_dct ) ;
    busyEventManager.EndEvent ;
  end ;
end ;

function T_FlowDirection.ExpandSink(
  const _outLockEx           : T_PixelLockAdapter ;
  const _notExpandedCellsDct : TDictionary< UInt64, Integer > ;
  const _potentialSinksLst   : TList< TPoint > ;
  const _visitedCellsDct     : TDictionary< UInt64, Integer > ;
  const _row                 : Integer ;
  const _col                 : Integer ;
  var   _depth               : Integer
) : Boolean ;
var
  cn, rn        : Integer ;
  code_id       : Integer ;
  dir_codes_arr : TArray< Integer > ;
  dir_id        : Integer ;
  encoded_cell  : UInt64 ;
  flow_dir      : Integer ;
begin
  Result := True ;

  encoded_cell := EncodeCellPosition( _row, _col ) ;
  if _visitedCellsDct.ContainsKey( encoded_cell ) then begin
    exit ;
  end
  else begin
    // recursion control
    if _depth >= MAX_DEPTH then begin
      _notExpandedCellsDct.AddOrSetValue( encoded_cell, 1 ) ;
      exit ;
    end
    else begin
      _visitedCellsDct.Add( encoded_cell, 1 ) ;
    end ;
  end ;

  inc( _depth ) ;

  // if cell is NODATA, whole examined flat area is not a sink
  if _outLockEx.CellIsNodataElseGetValue( _row, _col, flow_dir ) then begin
    Result := False ;
    dec( _depth ) ;
    exit ;
  end ;

  // direction value might be a sum of the direction codes
  // from cells with the same drop value (flat areas),
  // e.g. 145=1+16+128
  dir_codes_arr := ExtractLocationCodesFromDir( flow_dir ) ;

  for code_id := low( dir_codes_arr ) to high( dir_codes_arr ) do begin
    dir_id := T_HUtils.DirectionToIndex( dir_codes_arr[code_id] ) ;
    if ( dir_id < 0 ) then
      exit ;

    cn := _col + DC_NEIGHBOR[dir_id] ;
    rn := _row + DR_NEIGHBOR[dir_id] ;

    Result := ExpandSink(
      _outLockEx,
      _notExpandedCellsDct,
      _potentialSinksLst,
      _visitedCellsDct,
      rn,
      cn,
      _depth
    ) ;
    if not Result then begin
      dec( _depth ) ;
      exit ;
    end ;
  end ;

  // the cell is already resolved, so it needs to be removed from further investigation
  _notExpandedCellsDct.Remove( encoded_cell ) ;

  _potentialSinksLst.Add( Point( _col, _row ) ) ;

  dec( _depth ) ;
end;

procedure T_FlowDirection.LabelSinks(
  const _outLockEx : T_PixelLockAdapter ;
  const _sinks     : TDictionary< UInt64, Integer > ;
  const _setNodata : Boolean
) ;
var
  c, r   : Integer ;
  cell   : TPoint ;
  {$IFDEF DCC}
    sink : TPair<UInt64, Integer> ;
  {$ENDIF}
begin
  for sink in _sinks do begin
    cell := DecodeCellPosition( sink.Key ) ;
    c := cell.X ;
    r := cell.Y ;
    if _setNodata then
      _outLockEx.SetNodata( r, c )
    else
      _outLockEx.GridRel[r, c] := sink.Value ;
  end ;
end;
{$ENDREGION}

{$REGION 'T_Downstream'}
type
  /// <summary>
  ///   Specifies the donwstream-based algorithm
  /// </summary>
  T_DownstreamAlgorithm = (
    FlowAccumulation,
    StreamOrderShreve,
    StreamOrderStrahler
  ) ;

  T_DownstreamAlgorithmSet = set of T_DownstreamAlgorithm ;

  // FlowAccumulation, StreamOrder (Shreve, Strahler)
  T_Downstream = class( T_HydrologyAlgorithmAbstract )
    const
      STREAM_ORDER_ALGORITHMS : T_DownstreamAlgorithmSet = [
        T_DownstreamAlgorithm.StreamOrderShreve,
        T_DownstreamAlgorithm.StreamOrderStrahler
      ] ;

    private
      function  createNIDPMatrix( const _flowDirLockEx : T_PixelLockAdapter ;
                                  const _flowAccLockEx : T_PixelLockAdapter = nil ;
                                  const _threshold     : Integer = 0 ;
                                  const _algorithm     : T_DownstreamAlgorithm = T_DownstreamAlgorithm.FlowAccumulation
                                ) : TGIS_IntegerGridArray ;

      function  nextCell        ( var   _rn                : Integer ;
                                  var   _cn                : Integer ;
                                  const _dirCode           : Integer;
                                  const _flowDirLockEx     : T_PixelLockAdapter ;
                                  const _dontAllowBackflow : Boolean = False
                                ) : Boolean;

      procedure traceDownstream ( const _flowDirLockEx     : T_PixelLockAdapter ;
                                  const _outLockEx         : T_PixelLockAdapter ;
                                  const _NIDPMatrix        : TGIS_IntegerGridArray ;
                                  const _algorithm         : T_DownstreamAlgorithm;
                                  const _dontAllowBackflow : Boolean
                                ) ;

    public
      constructor Create( const _busyEventManager : TGIS_BusyEventManager ) ;

    public
      procedure GenerateFlowAccumulation   ( const _flowDirLockEx : T_PixelLockAdapter ;
                                             const _flowAccLockEx : T_PixelLockAdapter
                                           ) ;

      procedure GenerateStreamOrderShreve  ( const _flowDirLockEx : T_PixelLockAdapter ;
                                             const _flowAccLockEx : T_PixelLockAdapter ;
                                             const _streamLockEx  : T_PixelLockAdapter ;
                                             const _threshold     : Integer
                                           ) ;

      procedure GenerateStreamOrderStrahler( const _flowDirLockEx : T_PixelLockAdapter ;
                                             const _flowAccLockEx : T_PixelLockAdapter ;
                                             const _streamLockEx  : T_PixelLockAdapter ;
                                             const _threshold     : Integer
                                           ) ;
  end;

constructor T_Downstream.Create(
  const _busyEventManager : TGIS_BusyEventManager
) ;
begin
  inherited ;
end;

// Zhou algrithm
procedure T_Downstream.GenerateFlowAccumulation(
  const _flowDirLockEx : T_PixelLockAdapter ;
  const _flowAccLockEx : T_PixelLockAdapter
) ;
const
  ALLOW_BACKFLOW = False ;
var
  NIDP_matrix : TGIS_IntegerGridArray ;
begin
  busyEventManager.StartEvent(
    _rsrc( GIS_RS_BUSY_HYDROLOGY_ACCUMULATION ),
    2
  ) ;
  try
    NIDP_matrix := createNIDPMatrix( _flowDirLockEx ) ;
    if busyEventManager.Aborted then
      exit ;

    // original algorithm modification (was 1)
    _flowAccLockEx.Fill(0) ;

    traceDownstream(
      _flowDirLockEx,
      _flowAccLockEx,
      NIDP_matrix,
      T_DownstreamAlgorithm.FlowAccumulation,
      ALLOW_BACKFLOW
    ) ;
  finally
    busyEventManager.EndEvent ;
  end ;
end;

procedure T_Downstream.GenerateStreamOrderShreve(
  const _flowDirLockEx : T_PixelLockAdapter ;
  const _flowAccLockEx : T_PixelLockAdapter ;
  const _streamLockEx  : T_PixelLockAdapter ;
  const _threshold     : Integer
) ;
const
  DONT_ALLOW_BACKFLOW = True ;
var
  NIDP_matrix : TGIS_IntegerGridArray ;
begin
  busyEventManager.StartEvent(
    _rsrc( GIS_RS_BUSY_HYDROLOGY_ACCUMULATION ),
    2
  ) ;
  try
    NIDP_matrix := createNIDPMatrix(
      _flowDirLockEx,
      _flowAccLockEx,
      _threshold,
      T_DownstreamAlgorithm.StreamOrderShreve
    ) ;
    if busyEventManager.Aborted then
      exit ;

    _streamLockEx.Fill( 0 ) ;

    traceDownstream(
      _flowDirLockEx,
      _streamLockEx,
      NIDP_matrix,
      T_DownstreamAlgorithm.StreamOrderShreve,
      DONT_ALLOW_BACKFLOW
    ) ;
  finally
    busyEventManager.EndEvent ;
  end ;
end;

procedure T_Downstream.GenerateStreamOrderStrahler(
  const _flowDirLockEx : T_PixelLockAdapter ;
  const _flowAccLockEx : T_PixelLockAdapter ;
  const _streamLockEx  : T_PixelLockAdapter ;
  const _threshold     : Integer
) ;
const
  DONT_ALLOW_BACKFLOW = True ;
var
  NIDP_matrix : TGIS_IntegerGridArray ;
begin
  busyEventManager.StartEvent(
    _rsrc( GIS_RS_BUSY_HYDROLOGY_ACCUMULATION ),
    2
  ) ;
  try
    NIDP_matrix := createNIDPMatrix(
      _flowDirLockEx,
      _flowAccLockEx,
      _threshold,
      T_DownstreamAlgorithm.StreamOrderStrahler
    ) ;
    if busyEventManager.Aborted then
      exit ;

    _streamLockEx.Fill( 0 );
    traceDownstream(
      _flowDirLockEx,
      _streamLockEx,
      NIDP_matrix,
      T_DownstreamAlgorithm.StreamOrderStrahler,
      DONT_ALLOW_BACKFLOW
    ) ;
  finally
    busyEventManager.EndEvent ;
  end ;
end;

// A function returning a Boolean value.
// If the input cell c drains towards the outside of the DEM or it drains to
// a NODATA cell, the function returns a false value.
// Otherwise, the function returns value and cell c is updated as
// the downstream cell to which it drains.
function T_Downstream.nextCell(
  var   _rn                : Integer ;
  var   _cn                : Integer ;
  const _dirCode           : Integer ;
  const _flowDirLockEx     : T_PixelLockAdapter ;
  const _dontAllowBackflow : Boolean
) : Boolean ;
var
  dir_n : Integer ;
begin
  Result := False ;

  // determine neighbor position
  case _dirCode of
    1   : inc( _cn ) ;
    2   : begin inc( _cn ) ; inc( _rn ) ; end ;
    4   : inc( _rn ) ;
    8   : begin dec( _cn ) ; inc( _rn ) ; end ;
    16  : dec( _cn ) ;
    32  : begin dec( _cn ) ; dec( _rn ) ; end ;
    64  : dec( _rn ) ;
    128 : begin inc( _cn ) ; dec( _rn ) ; end ;
  else
    exit ;
  end ;

  // cell drains towards the outside od the DEM
  if not _flowDirLockEx.CellIsInGrid( _rn, _cn ) then
    exit ;

  // cell drains to NODATA
  if _flowDirLockEx.CellIsNodataElseGetValue( _rn, _cn, dir_n ) then
    exit ;

  // cell drains to each other
  if _dontAllowBackflow and ( dir_n = T_HUtils.ReverseDirection( _dirCode ) ) then
    exit ;

  Result := True ;
end ;

// NIDP - number of input drainage paths
function T_Downstream.createNIDPMatrix(
  const _flowDirLockEx : T_PixelLockAdapter ;
  const _flowAccLockEx : T_PixelLockAdapter;
  const _threshold     : Integer ;
  const _algorithm     : T_DownstreamAlgorithm
) : TGIS_IntegerGridArray ;
var
  dir                 : Integer ;
  c, r, cn, rn        : Integer ;
  use_threshold       : Boolean ;
  dont_allow_backflow : Boolean ;
begin
  SetLength( Result, _flowDirLockEx.NRows, _flowDirLockEx.NCols ) ;
  {$IFNDEF GIS_UNIT_TESTS}
    assert( Result[0, 0] = 0 ) ;
  {$ENDIF}

  use_threshold := assigned( _flowAccLockEx ) and ( _threshold > 0 ) ;
  dont_allow_backflow := _algorithm in STREAM_ORDER_ALGORITHMS ;

  busyEventManager.StartEvent(
    _rsrc( GIS_RS_BUSY_HYDROLOGY_ACC_NIDP ),
    _flowDirLockEx.NRows
  ) ;
  try
    for r := 0 to _flowDirLockEx.NRows-1 do begin
      if busyEventManager.PushEvent then
        exit ;

      for c := 0 to _flowDirLockEx.NCols-1 do begin
        if _flowDirLockEx.CellIsNodata( r, c ) then
          continue ;

        if use_threshold and
           ( _flowAccLockEx.GridRelInt[r, c] < _threshold ) then
        begin
          // mark NODATA to avoid double sweep of accumulation layer
          Result[r, c] := GIS_GRID_NOVALUE ;
          continue ;
        end ;

        rn := r ;
        cn := c ;

        dir := _flowDirLockEx.GridRelInt[r, c] ;
        if nextCell( rn, cn, dir, _flowDirLockEx, dont_allow_backflow ) and
           ( Result[rn, cn] <> GIS_GRID_NOVALUE )
        then
          Result[rn, cn] := Result[rn, cn] + 1 ;
      end ;
    end ;
  finally
    busyEventManager.EndEvent ;
  end ;
end;

procedure T_Downstream.traceDownstream(
  const _flowDirLockEx     : T_PixelLockAdapter ;
  const _outLockEx         : T_PixelLockAdapter ;
  const _NIDPMatrix        : TGIS_IntegerGridArray ;
  const _algorithm         : T_DownstreamAlgorithm ;
  const _dontAllowBackflow : Boolean
) ;
var
  nidp            : Integer ;
  dir_n           : Integer ;
  c, r, cn, rn    : Integer ;
  order_factor    : Integer ;
  current_acc     : Integer ;
  acc_cells_count : Integer ;

  procedure accumulate_flowacc ;
  begin
    current_acc := _outLockEx.GridRelInt[rn, cn] ;
    current_acc := current_acc + acc_cells_count ;
    _outLockEx.GridRelInt[rn, cn] := current_acc ;
    // original algorithm modification (was 0)
    acc_cells_count := current_acc + 1 ;
  end ;

  procedure accumulate_shreve ;
  begin
    current_acc := _outLockEx.GridRelInt[rn, cn] ;

    current_acc := current_acc + acc_cells_count ;
    _outLockEx.GridRelInt[rn, cn] := current_acc ;

    if nidp = -1 then
      acc_cells_count := current_acc ;
  end ;

  procedure accumulate_strahler ;
  begin
    current_acc := _outLockEx.GridRelInt[rn, cn] ;
    // value and sign separation of current_acc:
    // negative fator (-1) means that current order was from temporary
    // joint of 2 equal-order streams, e.g. 1 & 1 -> -2
    // positive factor (1) mean that current order was from temporary
    // joint of 2 not equal-order stream, e.g. 1 & 2 -> 2,
    // or -2 & 2 -> 2
    if current_acc >= 0 then
      order_factor := 1
    else begin
      order_factor := -1 ;
      current_acc := Abs( current_acc ) ;
    end ;

    // recalculate order at junction
    if ( nidp >= 2 ) or ( nidp = -1 ) then begin
      if current_acc = acc_cells_count then begin
        // 1 & 1 -> -2
        if order_factor > 0 then begin
          inc( current_acc ) ;
          order_factor := -1 ;
        end
        // -2 & 2 -> 2
        else begin
          current_acc := acc_cells_count ;
          order_factor := 1 ;
        end
      end
      // -2 & 1 -> -2, or
      //  1 & 2 -> 2
      else begin
        current_acc := Max( current_acc, acc_cells_count ) ;
      end ;

      // last pass always is positive
      if nidp = -1 then begin
        order_factor := 1 ;
        acc_cells_count := current_acc ;
      end ;

      current_acc := order_factor * current_acc ;
    end
    // first and connection cells (0 or 1)
    else begin
      current_acc := current_acc + acc_cells_count ;
    end;

    _outLockEx.GridRelInt[rn, cn] := current_acc ;
    if current_acc < 0 then
      _outLockEx.GridRelInt[rn, cn] := current_acc ;
  end ;

begin
  busyEventManager.StartEvent(
    _rsrc( GIS_RS_BUSY_HYDROLOGY_DOWNSTREAM ),
    _flowDirLockEx.NRows
  ) ;
  try
    for r := 0 to _flowDirLockEx.NRows-1 do begin
      if busyEventManager.PushEvent then
        exit ;

      for c := 0 to _flowDirLockEx.NCols-1 do begin

        // get flow direction if cell is not NODATA
        if _flowDirLockEx.CellIsNodata( r, c ) then begin
          _outLockEx.SetNodata( r, c ) ;
          continue ;
        end ;

        nidp := _NIDPMatrix[r, c] ;
        // NODATA is marked in NIDP matrix
        if ( _algorithm in STREAM_ORDER_ALGORITHMS ) and
           ( nidp = GIS_GRID_NOVALUE ) then
        begin
          _outLockEx.SetNodata( r, c ) ;
          continue ;
        end;

        // start at source cell (with no input flow)
        if nidp <> 0 then
          continue ;

        rn := r ;
        cn := c ;

        if _algorithm = T_DownstreamAlgorithm.FlowAccumulation then
          acc_cells_count := 0
        else  // stream order (Shreve, Strahler)
          acc_cells_count := 1 ;

        repeat
          nidp := _NIDPMatrix[rn, cn] ;
          if ( _algorithm in STREAM_ORDER_ALGORITHMS ) and
             ( nidp = GIS_GRID_NOVALUE ) then
          begin
            _outLockEx.SetNodata( rn, cn ) ;
            break ;
          end;

          case _algorithm of
            T_DownstreamAlgorithm.FlowAccumulation    : accumulate_flowacc ;
            T_DownstreamAlgorithm.StreamOrderShreve   : accumulate_shreve ;
            T_DownstreamAlgorithm.StreamOrderStrahler : accumulate_strahler ;
          end ;

          if ( nidp >= 2 ) then begin
            dec( nidp ) ;
            _NIDPMatrix[rn, cn] := nidp ;

            // Shreve - mark last downstream flow
            if ( _algorithm in STREAM_ORDER_ALGORITHMS ) and ( nidp = 1 ) then
              _NIDPMatrix[rn, cn] := -1 ;

            break ;
          end ;

          dir_n := _flowDirLockEx.GridRelInt[rn, cn] ;
        until not nextCell( rn, cn, dir_n, _flowDirLockEx, _dontAllowBackflow ) ;
      end ;
    end ;
  finally
    busyEventManager.EndEvent ;
  end ;
end;
{$ENDREGION}

{$REGION 'T_Upstream'}
type
  /// <summary>
  ///   Specifies the donwstream-based algorithm
  /// </summary>
  T_UpstreamAlgorithm = (
    StreamOrderHack,
    StreamOrderTopological
  ) ;

type
  T_Upstream = class( T_HydrologyAlgorithmAbstract )
    public
      function  CellIsUpstream ( const _row           : Integer ;
                                 const _col           : Integer ;
                                 const _reversedDir   : Integer ;
                                 const _flowDirLockEx : T_PixelLockAdapter
                               ) : Boolean ; inline ;

      procedure CollectOutlets ( const _outlets       : TQueue<TGIS_Point3DInt> ;
                                 const _outletsLockEx : T_PixelLockAdapter ;
                                 const _basins        : TList<Integer>
                               ) ;

      procedure CreatePolylines( const _outlets       : TQueue<TGIS_Point3DInt> ;
                                 const _flowdirLockEx : T_PixelLockAdapter ;
                                 const _streamLockEx  : T_PixelLockAdapter ;
                                 const _streamsVec    : TGIS_LayerVector ;
                                 const _orderField    : String ;
                                 const _threshold     : Integer
                               ) ;

      procedure CreateStreamOrder(
                                 const outlets_q      : TQueue<TGIS_Point3DInt> ;
                                 const _flowDirLockEx : T_PixelLockAdapter ;
                                 const _flowAccLockEx : T_PixelLockAdapter ;
                                 const _streamLockEx  : T_PixelLockAdapter ;
                                 const _threshold     : Integer ;
                                 const _algorithm     : T_UpstreamAlgorithm
                               ) ;

      procedure DelineateBasins( const _outlets       : TQueue<TGIS_Point3DInt> ;
                                 const _flowDirLockEx : T_PixelLockAdapter ;
                                 const _outLockEx     : T_PixelLockAdapter ;
                                 const _basins        : TDictionary<Integer, Integer>
                               ) ;

      procedure FindOutlets    ( const _outlets       : TQueue< TGIS_Point3DInt > ;
                                 const _flowDirLockEx : T_PixelLockAdapter ;
                                 const _basins        : TList<Integer>
                               ) ;

      function  LabelBasins    ( const _basinsLockEx  : T_PixelLockAdapter ;
                                 const _basinsSize        : TDictionary<Integer, Integer> ;
                                 const _basinsOrder   : TList<Integer> ;
                                 const _threshold     : Integer
                               ) : Integer ;

    public
      /// <inheritdoc/>
      constructor Create( const _busyEventManager : TGIS_BusyEventManager ) ;

    public
      procedure GenerateBasins     ( const _flowDir       : TGIS_LayerPixel ;
                                     const _extent        : TGIS_Extent ;
                                     const _out           : TGIS_LayerPixel ;
                                     const _threshold     : Integer
                                   ) ;

      // if _outlets is nil generate basins, otherwise watersheds
      procedure GenerateWatershed  ( const _flowDir       : TGIS_LayerPixel ;
                                     const _outlets       : TGIS_LayerPixel ;
                                     const _extent        : TGIS_Extent ;
                                     const _out           : TGIS_LayerPixel ;
                                     const _threshold     : Integer = 0
                                   ) ;

      // _streamsPix may be flow accumulation or stream order
      procedure GenerateStreamPolyline(
                                     const _flowDir       : TGIS_LayerPixel ;
                                     const _streamsPix    : TGIS_LayerPixel ;
                                     const _extent        : TGIS_Extent ;
                                     const _streamsVec    : TGIS_LayerVector ;
                                     const _orderField    : String ;
                                     const _threshold     : Integer
                                   ) ;

      procedure GenerateStreamOrder( const _flowDirLockEx : T_PixelLockAdapter ;
                                     const _flowAccLockEx : T_PixelLockAdapter ;
                                     const _streamLockEx  : T_PixelLockAdapter ;
                                     const _threshold     : Integer ;
                                     const _algorithm     : T_UpstreamAlgorithm
                                   ) ;

      procedure GenerateStreamOrderHack(
                                     const _flowDirLockEx : T_PixelLockAdapter ;
                                     const _flowAccLockEx : T_PixelLockAdapter ;
                                     const _streamLockEx  : T_PixelLockAdapter ;
                                     const _threshold     : Integer
                                   ) ;

      procedure GenerateStreamOrderTopological(
                                     const _flowDirLockEx : T_PixelLockAdapter ;
                                     const _flowAccLockEx : T_PixelLockAdapter ;
                                     const _streamLockEx  : T_PixelLockAdapter ;
                                     const _threshold     : Integer
                                   ) ;
  end;

constructor T_Upstream.Create(
  const _busyEventManager : TGIS_BusyEventManager
) ;
begin
  inherited Create ( _busyEventManager ) ;
end;

procedure T_Upstream.GenerateBasins(
  const _flowDir   : TGIS_LayerPixel ;
  const _extent    : TGIS_Extent ;
  const _out       : TGIS_LayerPixel ;
  const _threshold : Integer
) ;
begin
  GenerateWatershed( _flowDir, nil, _extent, _out, _threshold ) ;
end;

procedure T_Upstream.GenerateStreamOrderHack(
  const _flowDirLockEx : T_PixelLockAdapter ;
  const _flowAccLockEx : T_PixelLockAdapter ;
  const _streamLockEx  : T_PixelLockAdapter ;
  const _threshold     : Integer
) ;
begin
  GenerateStreamOrder(
    _flowDirLockEx,
    _flowAccLockEx,
    _streamLockEx,
    _threshold,
    T_UpstreamAlgorithm.StreamOrderHack
  ) ;
end ;

procedure T_Upstream.GenerateStreamOrderTopological(
  const _flowDirLockEx : T_PixelLockAdapter ;
  const _flowAccLockEx : T_PixelLockAdapter ;
  const _streamLockEx  : T_PixelLockAdapter ;
  const _threshold     : Integer
) ;
begin
  GenerateStreamOrder(
    _flowDirLockEx,
    _flowAccLockEx,
    _streamLockEx,
    _threshold,
    T_UpstreamAlgorithm.StreamOrderTopological
  ) ;
end ;

procedure T_Upstream.GenerateStreamOrder(
  const _flowDirLockEx : T_PixelLockAdapter ;
  const _flowAccLockEx : T_PixelLockAdapter ;
  const _streamLockEx  : T_PixelLockAdapter ;
  const _threshold     : Integer ;
  const _algorithm     : T_UpstreamAlgorithm
) ;
var
  outlets_q    : TQueue< TGIS_Point3DInt > ;
begin
  busyEventManager.StartEvent(
    _rsrc( GIS_RS_BUSY_HYDROLOGY_STREAM_ORDER ),
    2
  ) ;
  outlets_q := TQueue< TGIS_Point3DInt >.Create ;
  try
    FindOutlets( outlets_q, _flowDirLockEx, nil ) ;

    CreateStreamOrder(
      outlets_q,
      _flowDirLockEx,
      _flowAccLockEx,
      _streamLockEx,
      _threshold,
      _algorithm
    ) ;
  finally
    FreeObject( outlets_q ) ;
    busyEventManager.EndEvent ;
  end ;
end;

procedure T_Upstream.CreateStreamOrder(
  const outlets_q      : TQueue< TGIS_Point3DInt > ;
  const _flowDirLockEx : T_PixelLockAdapter ;
  const _flowAccLockEx : T_PixelLockAdapter ;
  const _streamLockEx  : T_PixelLockAdapter ;
  const _threshold     : Integer ;
  const _algorithm     : T_UpstreamAlgorithm
) ;
const
  MAX_DEPTH = 1024 * 1024 ;
var
  c, r         : Integer ;
  order        : Integer ;
  depth        : Integer ;
  find_max_acc : Boolean ;
  cell         : TGIS_Point3DInt ;

  function find_upstreams(
    const _row        : Integer ;
    const _col        : Integer ;
    const _threshold  : Integer ;
    const _findMaxAcc : Boolean ;
    var   _maxAccId   : Integer
  ) : TList< Integer > ;
  var
    cn, rn, n  : Integer ;
    stream_val : Integer ;
    max_val    : Integer ;
  begin
    Result := TList< Integer >.Create ;

    max_val := GIS_MIN_INTEGER ;
    _maxAccId := -1 ;
    for n := 0 to NEIGHBOR_COUNT-1 do begin
      cn := _col + DC_NEIGHBOR[n] ;
      rn := _row + DR_NEIGHBOR[n] ;

      if not CellIsUpstream( rn, cn, REVERSED_DIRECTIONS[n], _flowDirLockEx ) then
        continue ;

      stream_val := _flowAccLockEx.GridRelInt[rn, cn] ;

      if stream_val < _threshold then
        continue ;

      // Hack Stream Order
      if _findMaxAcc then begin
        if stream_val > max_val then begin
          max_val := stream_val ;
          _maxAccId := Result.Count ;
        end ;
      end ;
      Result.Add( n ) ;
    end ;
  end;

  procedure trace_upstream(
    const _row        : Integer ;
    const _col        : Integer ;
    const _order      : Integer ;
    const _threshold  : Integer ;
    const _findMaxAcc : Boolean ;
    var   _depth      : Integer
  ) ;
  var
    cn, rn, i       : Integer ;
    order_n         : Integer ;
    main_stream_id  : Integer ;
    upstreams_codes : TList< Integer > ;
  begin
    inc( _depth ) ;

    _streamLockEx.GridRelInt[_row, _col] := _order ;

    // count upstreams and determine main stream
    upstreams_codes := find_upstreams(
      _row,
      _col,
      _threshold,
      _findMaxAcc,
      main_stream_id
    ) ;
    try
      order_n := 0 ;

      // Topological Stream Order
      if not _findMaxAcc then begin
        if upstreams_codes.Count = 1 then
          order_n := _order
        else
          // increment order at every junction
          order_n := _order + 1 ;
      end ;

      for i := 0 to upstreams_codes.Count-1 do begin
        cn := _col + DC_NEIGHBOR[upstreams_codes[i]] ;
        rn := _row + DR_NEIGHBOR[upstreams_codes[i]] ;

        // Hack Stream Order
        if _findMaxAcc then begin
          if i = main_stream_id then
            order_n := _order
          else
            // increment order only for mainstream tributaries
            order_n := _order + 1 ;
        end ;

        // control recursion depth to avoid stack overflow
        if ( _depth < MAX_DEPTH ) then
          trace_upstream( rn, cn, order_n, _threshold, _findMaxAcc, _depth )
        else
          outlets_q.Enqueue( GisPoint3DInt( cn, rn, order_n ) ) ;
      end ;
    finally
      FreeObject( upstreams_codes ) ;
    end ;
    dec( _depth ) ;
  end;

begin
  _streamLockEx.FillWithNodata ;

  // True for Hack, False for Topological
  find_max_acc := _algorithm = T_UpstreamAlgorithm.StreamOrderHack ;

  busyEventManager.StartEvent(
    _rsrc( GIS_RS_BUSY_HYDROLOGY_UPSTREAM ),
    outlets_q.Count
  ) ;
  try
    while not T_HUtils.QueueIsEmpty( outlets_q ) do begin
      if busyEventManager.PushEvent then
        exit ;

      cell := outlets_q.Dequeue ;
      c := cell.X ;
      r := cell.Y ;
      order := cell.Z ;
      depth := 0 ;

      trace_upstream( r, c, order, _threshold, find_max_acc, depth ) ;
    end ;
  finally
    busyEventManager.EndEvent ;
  end ;
end;

procedure T_Upstream.GenerateStreamPolyline(
  const _flowDir    : TGIS_LayerPixel ;
  const _streamsPix : TGIS_LayerPixel ;
  const _extent     : TGIS_Extent;
  const _streamsVec : TGIS_LayerVector;
  const _orderField : String ;
  const _threshold  : Integer
) ;
var
  outlets_q      : TQueue< TGIS_Point3DInt > ;
  flowdir_lock   : TGIS_LayerPixelLock ;
  stream_lock    : TGIS_LayerPixelLock ;
  flowdir_lockex : T_PixelLockAdapter ;
  stream_lockex  : T_PixelLockAdapter ;
begin
  busyEventManager.StartEvent( _rsrc( GIS_RS_BUSY_HYDROLOGY_STREAMTOVEC ), 2 ) ;

  flowdir_lock := _flowDir.LockPixels( _extent, _flowDir.CS, False ) ;
  stream_lock := _streamsPix.LockPixels( _extent, _flowDir.CS,  False ) ;

  flowdir_lockex := T_PixelLockAdapter.Create( flowdir_lock ) ;
  stream_lockex := T_PixelLockAdapter.Create( stream_lock ) ;

  outlets_q := TQueue< TGIS_Point3DInt >.Create ;
  try
    FindOutlets( outlets_q, flowdir_lockex, nil ) ;

    CreatePolylines(
      outlets_q,
      flowdir_lockex,
      stream_lockex,
      _streamsVec,
      _orderField,
      _threshold
    ) ;
  finally
    FreeObject( outlets_q ) ;

    FreeObject( flowdir_lockex  ) ;
    FreeObject( stream_lockex ) ;
    _flowDir.UnlockPixels( flowdir_lock ) ;
    _streamsPix.UnlockPixels( stream_lock ) ;

    busyEventManager.EndEvent ;
  end ;
end ;

procedure T_Upstream.CreatePolylines(
  const _outlets       : TQueue< TGIS_Point3DInt > ;
  const _flowdirLockEx : T_PixelLockAdapter ;
  const _streamLockEx  : T_PixelLockAdapter ;
  const _streamsVec    : TGIS_LayerVector ;
  const _orderField    : String ;
  const _threshold     : Integer
) ;
var
  points         : TList< TPoint > ;
  cell           : TGIS_Point3DInt ;
  r, c, order    : Integer ;

  procedure add_stream_and_clean_points(
    const _order  : Integer ;
    const _points : TList< TPoint >
  ) ;
  var
    x, y         : Double ;
    x_min, y_min : Double ;
    shp          : TGIS_Shape ;
    cell_size    : TGIS_Point ;
    {$IFNDEF OXYGENE}
      pt         : TPoint ;
    {$ENDIF}
  begin
    if _points.Count > 1 then begin
      cell_size := _flowdirLockEx.CellSize ;
      x_min := _flowdirLockEx.Lock.Extent.XMin ;
      y_min := _flowdirLockEx.Lock.Extent.YMax ;

      shp := _streamsVec.CreateShape( TGIS_ShapeType.Arc ) ;
      shp.Lock( TGIS_Lock.Projection ) ;
      shp.AddPart ;
      for pt in _points do begin
        x := x_min + ( pt.X + 0.5 ) * cell_size.X  ;
        y := y_min - ( pt.Y + 0.5 ) * cell_size.Y ;

        shp.AddPoint( GisPoint( x, y ) ) ;
      end ;
      shp.Unlock ;

      //? SMOOTH feature will be implemented in future

      if not IsStringEmpty( _orderField  ) then
        shp.SetField( _orderField, _order ) ;
    end ;

    _points.Clear ;
  end;

  // list of direction codes with upstreams and values
  function find_upstreams(
    const _row       : Integer ;
    const _col       : Integer ;
    const _threshold : Integer
  ) : TList< TGIS_Point3DInt > ;
  var
    cn, rn, n  : Integer ;
    stream_val : Integer ;
  begin
    Result := TList< TGIS_Point3DInt >.Create ;
    for n := 0 to NEIGHBOR_COUNT-1 do begin
      cn := _col + DC_NEIGHBOR[n] ;
      rn := _row + DR_NEIGHBOR[n] ;

      if not CellIsUpstream( rn, cn, REVERSED_DIRECTIONS[n], _flowdirLockEx ) then
        continue ;

      stream_val := _streamLockEx.GridRelInt[rn, cn] ;
      if stream_val < _threshold then
        continue ;

      Result.Add( GisPoint3DInt( cn, rn, stream_val ) ) ;
    end ;
  end;

  procedure trace_upstream(
    const _row       : Integer ;
    const _col       : Integer ;
    const _order     : Integer ;
    const _points    : TList< TPoint > ;
    const _threshold : Integer
  ) ;
  var
    cn, rn, i       : Integer ;
    order_n         : Integer ;
    upstreams_codes : TList< TGIS_Point3DInt > ;
  begin
    if ( _order < _threshold ) then
      exit ;

    _points.Add( Point( _col, _row ) ) ;

    // count upstreams to detect junctions
    upstreams_codes := find_upstreams( _row, _col, _threshold ) ;
    try
      // close arc
      if upstreams_codes.Count = 0 then begin
        add_stream_and_clean_points( _order, _points ) ;
      end
      // add new vertex
      else if ( upstreams_codes.Count = 1 ) and
              ( upstreams_codes[0].Z = _order ) then
      begin
        cn := upstreams_codes[0].X ;
        rn := upstreams_codes[0].Y ;

        trace_upstream( rn, cn, _order, _points, _threshold ) ;
      end
      // add new outlets and close arc
      else begin
        add_stream_and_clean_points( _order, _points ) ;

        for i := 0 to upstreams_codes.Count-1 do begin
          cn := upstreams_codes[i].X ;
          rn := upstreams_codes[i].Y ;
          order_n := upstreams_codes[i].Z ;

          _points.Add( Point( _col, _row ) ) ;
          _points.Add( Point( cn, rn ) ) ;
          add_stream_and_clean_points( order_n, _points ) ;

          _outlets.Enqueue( GisPoint3DInt( cn, rn, order_n ) ) ;
        end ;
      end ;
    finally
      FreeObject( upstreams_codes ) ;
    end ;
  end;
begin
  busyEventManager.StartEvent(
    _rsrc( GIS_RS_BUSY_HYDROLOGY_UPSTREAM ),
    _outlets.Count
  ) ;
  points := TList< TPoint >.Create ;
  try
    while not T_HUtils.QueueIsEmpty( _outlets ) do begin
      if busyEventManager.PushEvent then
        exit ;

      cell := _outlets.Dequeue ;
      c := cell.X ;
      r := cell.Y ;
      order := _streamLockEx.GridRelInt[r, c] ;

      trace_upstream( r, c, order, points, _threshold ) ;
    end ;
  finally
    FreeObject( points ) ;
    busyEventManager.EndEvent ;
  end ;
end ;

procedure T_Upstream.GenerateWatershed(
  const _flowDir   : TGIS_LayerPixel ;
  const _outlets   : TGIS_LayerPixel ;
  const _extent    : TGIS_Extent ;
  const _out       : TGIS_LayerPixel ;
  const _threshold : Integer
) ;
var
  outlets_q       : TQueue<TGIS_Point3DInt> ;
  basins_size     : TDictionary<Integer, Integer> ;
  basins_order    : TList<Integer> ;
  basin_id        : Integer ;
  flowdir_lock    : TGIS_LayerPixelLock ;
  out_lock        : TGIS_LayerPixelLock ;
  outlets_lock    : TGIS_LayerPixelLock ;

  flowdir_lockex  : T_PixelLockAdapter ;
  out_lockex      : T_PixelLockAdapter ;
  outlets_lockex  : T_PixelLockAdapter ;

  stages_count    : Integer ;
  busy_text       : String ;
  basin_algorithm : Boolean ;
begin
  basin_algorithm := not assigned( _outlets ) ;  // otherwise watershed

  _flowDir.Open ;
  _out.Open ;

  _out.MinHeight := GIS_MAX_SINGLE ;
  _out.MaxHeight := -GIS_MAX_SINGLE ;

  flowdir_lock := _flowDir.LockPixels( _flowDir.Extent, _flowDir.CS, False ) ;
  out_lock := _out.LockPixels( _flowDir.Extent, _flowDir.CS, True ) ;

  flowdir_lockex := T_PixelLockAdapter.Create( flowdir_lock ) ;
  out_lockex := T_PixelLockAdapter.Create( out_lock ) ;

  // basin
  if basin_algorithm then begin
    stages_count := 3 ;
    busy_text := _rsrc( GIS_RS_BUSY_HYDROLOGY_BASIN ) ;
  end
  // watershed
  else begin
    outlets_lock := _outlets.LockPixels( _flowDir.Extent, _flowDir.CS, False ) ;
    outlets_lockex := T_PixelLockAdapter.Create( outlets_lock ) ;
    stages_count := 2 ;
    busy_text := _rsrc( GIS_RS_BUSY_HYDROLOGY_BASIN ) ;
  end;

  outlets_q := TQueue< TGIS_Point3DInt >.Create ;
  basins_size := TDictionary<Integer, Integer>.Create ;
  // the basins order must be ensured regardless of the platform
  basins_order := TList<Integer>.Create;
  busyEventManager.StartEvent( busy_text, stages_count ) ;
  try
    // 1st stage:
    if basin_algorithm then
      // find outlets on the edges,
      FindOutlets( outlets_q, flowdir_lockex, basins_order )
    else
      // or collect outlets from raster
      CollectOutlets( outlets_q, outlets_lockex, basins_order ) ;

    if busyEventManager.Aborted then
      exit ;

    // initialize a new basin with size=1
    for basin_id in basins_order do
      basins_size.Add( basin_id, 1 ) ;

    // 2nd stage: start at outlets and scan higher cell
    DelineateBasins( outlets_q, flowdir_lockex, out_lockex, basins_size ) ;
    if busyEventManager.Aborted then
      exit ;

    if basin_algorithm then
      // 3rd stage: relabel basins
      LabelBasins( out_lockex, basins_size, basins_order, _threshold ) ;

  finally
    if not basin_algorithm then begin
      FreeObject( outlets_lockex ) ;
      _outlets.UnlockPixels( outlets_lock ) ;
    end ;

    FreeObject( outlets_q ) ;
    FreeObject( basins_size ) ;
    FreeObject( out_lockex ) ;
    FreeObject( flowdir_lockex ) ;
    _flowDir.UnlockPixels( flowdir_lock ) ;
    _out.UnlockPixels( out_lock ) ;
    busyEventManager.EndEvent ;
  end ;
end;

function T_Upstream.CellIsUpstream(
  const _row           : Integer ;
  const _col           : Integer ;
  const _reversedDir   : Integer ;
  const _flowDirLockEx : T_PixelLockAdapter
) : Boolean ;
var
  dir_n : Integer ;
begin
  Result := False ;

  if not _flowDirLockEx.CellIsInGrid( _row, _col ) then
    exit ;

  if _flowDirLockEx.CellIsNodataElseGetValue( _row, _col, dir_n ) then
    exit ;

  Result := ( dir_n = _reversedDir ) ;
end;

procedure T_Upstream.CollectOutlets(
  const _outlets       : TQueue<TGIS_Point3DInt> ;
  const _outletsLockEx : T_PixelLockAdapter ;
  const _basins        : TList<Integer>
) ;
var
  c, r, id       : Integer;
begin
  for r := 0 to _outletsLockEx.NRows-1 do begin
    for c := 0 to _outletsLockEx.NCols-1 do begin
      if not _outletsLockEx.CellIsNodataElseGetValue( r, c, id ) then begin
        _outlets.Enqueue( GisPoint3DInt( c, r, id ) ) ;
        _basins.Add( id ) ;
      end ;
    end ;
  end ;
end;

// _basins is nil, Z store initial value,
// otherwise Z store id and dict store initial value
procedure T_Upstream.FindOutlets(
  const _outlets       : TQueue< TGIS_Point3DInt > ;
  const _flowDirLockEx : T_PixelLockAdapter ;
  const _basins        : TList<Integer>
) ;
var
  id, dir_id      : Integer ;
  dir_code, dir_n : Integer ;
  r, c, rn, cn    : Integer ;
  store_basin_id  : Boolean ;
begin
  store_basin_id := assigned( _basins ) ;

  busyEventManager.StartEvent(
    _rsrc( GIS_RS_BUSY_HYDROLOGY_BASIN_OUTLETS ),
    _flowDirLockEx.NRows
  ) ;
  try
    id := 0 ;
    for r := 0 to _flowDirLockEx.NRows-1 do begin
      if busyEventManager.PushEvent then
        exit ;
      for c := 0 to  _flowDirLockEx.NCols-1 do begin
        if _flowDirLockEx.CellIsNodataElseGetValue( r, c, dir_code ) then
          continue ;

        dir_id := T_HUtils.DirectionToIndex( dir_code ) ;

        // the cell is an outlet,
        // if it is a pit (dir_id<0), or ...
        if dir_id >= 0 then begin
          cn := c + DC_NEIGHBOR[dir_id] ;
          rn := r + DR_NEIGHBOR[dir_id] ;

          // ... if it flows outward the DEM, or
          // if it backflows to neighbor that flows into that cell
          if _flowDirLockEx.CellIsInGrid( rn, cn ) and
             not _flowDirLockEx.CellIsNodataElseGetValue( rn, cn, dir_n ) and
             ( dir_n <> REVERSED_DIRECTIONS[dir_id] )
          then
            continue ;
        end ;

        // give the following ID and store initial size in the dictionary
        // (for Basin and Watershed)
        if store_basin_id then begin
          _basins.Add( id ) ;
          _outlets.Enqueue( GisPoint3DInt( c, r, id ) ) ;
          inc( id ) ;
        end
        // only store initial value (for StreamsToPolyline)
        else begin
          _outlets.Enqueue( GisPoint3DInt( c, r, 1 ) ) ;
        end ;

      end ;
    end ;
  finally
    busyEventManager.EndEvent ;
  end ;
end;

procedure T_Upstream.DelineateBasins(
  const _outlets       : TQueue< TGIS_Point3DInt > ;
  const _flowDirLockEx : T_PixelLockAdapter ;
  const _outLockEx     : T_PixelLockAdapter ;
  const _basins        : TDictionary<Integer, Integer>
) ;
var
  r, c, rn, cn  : Integer ;
  id, id_tmp, n : Integer ;
  basin_size    : {$IFDEF JAVA} nullable {$ENDIF} Integer ;
  cell          : TGIS_Point3DInt ;
begin
  busyEventManager.StartEvent(
    _rsrc( GIS_RS_BUSY_HYDROLOGY_BASIN_DELINEATE ),
    _flowDirLockEx.NRows * _flowDirLockEx.NCols
  ) ;
  try
    _outLockEx.FillWithNodata ;

    while not T_HUtils.QueueIsEmpty( _outlets ) do begin
      if busyEventManager.PushEvent then
        exit ;

      cell := _outlets.Dequeue ;
      c := cell.X ;
      r := cell.Y ;
      id := cell.Z ;

      _outLockEx.GridRel[r, c] := id ;

      for n := 0 to NEIGHBOR_COUNT-1 do begin
        T_HUtils.GetNeighbor( r, c, n, rn, cn ) ;

        if CellIsUpstream( rn, cn, REVERSED_DIRECTIONS[n], _flowDirLockEx ) then begin
          // first entry to a cell
          if _outLockEx.CellIsNodata( rn, cn ) then begin
            // increase basin size
            if _basins.TryGetValue( id, basin_size ) then
              _basins.AddOrSetValue( id, basin_size + 1 ) ;

            // add new seed of current basin
            _outlets.Enqueue( GisPoint3DInt( cn, rn, id ) ) ;

            _outLockEx.GridRel[rn, cn] := id ;
          end
          else begin
            // decrease basin size
            id_tmp := _outLockEx.GridRelInt[rn, cn] ;
            if _basins.TryGetValue( id_tmp, basin_size ) then
              _basins.AddOrSetValue( id_tmp, basin_size - 1 ) ;

            // reassign basin id
            _outLockEx.GridRel[rn, cn] := id ;
          end ;
        end ;
      end ;
    end ;
  finally
    busyEventManager.EndEvent ;
  end ;
end;

function T_Upstream.LabelBasins(
  const _basinsLockEx : T_PixelLockAdapter ;
  const _basinsSize   : TDictionary<Integer, Integer> ;
  const _basinsOrder  : TList<Integer> ;
  const _threshold    : Integer
) : Integer ;
var
  new_id     : {$IFDEF JAVA} nullable {$ENDIF} Integer ;
  old_id     : Integer ;
  initial_id,
  basin_size : {$IFDEF JAVA} nullable {$ENDIF} Integer ;
  r, c       : Integer ;
  basins_dct : TDictionary< Integer, Integer > ;
begin
  Result := 0 ;

  basins_dct := TDictionary<Integer, Integer>.Create ;
  try
    // renumber basin ids based on size Threshold
    new_id := 0 ;
    for initial_id in _basinsOrder do begin
      if not _basinsSize.TryGetValue( initial_id, basin_size ) then
        continue;

      // size = value
      if basin_size < _threshold then
        continue ;

      // replace old id with the next free id
      basins_dct.Add( initial_id, new_id ) ;
      inc( new_id ) ;
    end ;

    // apply new ids from basin_dct
    busyEventManager.StartEvent(
      _rsrc( GIS_RS_BUSY_HYDROLOGY_BASIN_LABEL ),
      _basinsLockEx.NRows
    ) ;
    try
      for r := 0 to _basinsLockEx.NRows-1 do begin
        if busyEventManager.PushEvent then
          exit ;

        for c := 0 to _basinsLockEx.NCols-1 do begin
          if not _basinsLockEx.CellIsNodataElseGetValue( r, c, old_id ) then begin
            if basins_dct.TryGetValue( old_id, new_id ) then
              _basinsLockEx.GridRel[r, c] := new_id
            else
              _basinsLockEx.SetNodata( r, c ) ;
          end ;
        end ;
      end ;
    finally
      busyEventManager.EndEvent ;
    end ;

    Result := basins_dct.Count ;
  finally
    FreeObject( basins_dct ) ;
  end ;
end;
{$ENDREGION}

{$REGION 'T_StreamOrderAlgorithmFactory'}
type
  T_StreamOrderAlgorithmFactory = class
    public
      class function CreateAlgorithm( const _busyEventManager : TGIS_BusyEventManager ;
                                      const _method           : TGIS_HydrologyStreamOrderMethod
                                    ) : T_HydrologyAlgorithmAbstract ;
  end;

class function T_StreamOrderAlgorithmFactory.CreateAlgorithm(
  const _busyEventManager : TGIS_BusyEventManager ;
  const _method           : TGIS_HydrologyStreamOrderMethod
): T_HydrologyAlgorithmAbstract;
begin
  case _method of
    TGIS_HydrologyStreamOrderMethod.Hack,
    TGIS_HydrologyStreamOrderMethod.Topological :
      Result := T_Upstream.Create( _busyEventManager ) ;

    TGIS_HydrologyStreamOrderMethod.Shreve,
    TGIS_HydrologyStreamOrderMethod.Strahler :
      Result := T_Downstream.Create( _busyEventManager ) ;
  else
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPARAM ), '_method', 0 ) ;
  end ;
end;
{$ENDREGION}

// High-level hydrological algorithms //

{$REGION 'T_HydrologyAbstract'}
constructor T_HydrologyAbstract.Create ;
begin
  inherited ;
  FThreshold := 0 ;
end;

procedure T_HydrologyAbstract.verifyLayer(
  const _in  : TGIS_LayerPixel ;
  const _out : TGIS_LayerPixel
) ;
begin
  // layer does not exist
  if not assigned( _in ) then
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERNOEXIST ), _in.Name, 0 ) ;

  if not assigned( _out ) then
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERNOEXIST ), _out.Name, 0 ) ;

  // layer is not a grid
  if not _in.IsGridImage then
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPARAM ), _in.Name, 0 ) ;

  if not _out.IsGridImage then
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPARAM ), _out.Name, 0 ) ;

   // CS does not match
   if _out.CS.EPSG <> _in.CS.EPSG then
     raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPARAM ), _out.Name, 1 ) ;

   // resolution does not match
   if T_HUtils.GetCellSize( _out ).X <> T_HUtils.GetCellSize( _in ).X then
     raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPARAM ), _out.Name, 2 ) ;
end;

{$ENDREGION}

{$REGION 'T_HydrologySink'}
constructor T_HydrologySink.Create ;
begin
  inherited ;
end;

procedure T_HydrologySink.Generate(
  const _dem    : TGIS_LayerPixel ;
  const _extent : TGIS_Extent ;
  const _sinks  : TGIS_LayerPixel
) ;
var
  algorithm : T_FlowDirection ;
begin
  verifyLayer( _dem, _sinks ) ;

  algorithm := T_FlowDirection.Create(
    busyEventManager,
    T_FlowDirectionAlgorithm.Sink
  ) ;
  try
    algorithm.Generate(
      _dem,
      _extent,
      _sinks
    ) ;
  finally
    FreeObject( algorithm ) ;
  end ;
end;
{$ENDREGION}

{$REGION 'T_HydrologyResolveFlat'}
constructor T_HydrologyResolveFlat.Create ;
begin
  inherited ;
end;

procedure T_HydrologyResolveFlat.Generate(
  const _dem        : TGIS_LayerPixel ;
  const _flowDir    : TGIS_LayerPixel ;
  const _extent     : TGIS_Extent ;
  const _alteredDem : TGIS_LayerPixel
) ;
var
  altdem_lock    : TGIS_LayerPixelLock ;
  dem_lock       : TGIS_LayerPixelLock ;
  flowdir_lock   : TGIS_LayerPixelLock ;

  altdem_lockex  : T_PixelLockAdapter ;
  dem_lockex     : T_PixelLockAdapter ;
  flowdir_lockex : T_PixelLockAdapter ;
begin
  verifyLayer( _dem, _flowDir ) ;

  _dem.Open ;
  _flowDir.Open ;

  dem_lock := _dem.LockPixels( _extent, _dem.CS, False ) ;
  flowdir_lock := _flowDir.LockPixels( _extent, _dem.CS, False ) ;
  dem_lockex :=  T_PixelLockAdapter.Create( dem_lock ) ;
  flowdir_lockex :=  T_PixelLockAdapter.Create( flowdir_lock ) ;

  if assigned( _alteredDem ) then begin
    verifyLayer( _dem, _alteredDem ) ;

    _alteredDem.Open ;
    _alteredDem.MinHeight := GIS_MAX_SINGLE ;
    _alteredDem.MaxHeight := -GIS_MAX_SINGLE ;
    altdem_lock := _alteredDem.LockPixels( dem_lock.Extent, dem_lock.CS, True ) ;
    altdem_lockex :=  T_PixelLockAdapter.Create( altdem_lock ) ;
  end
  else
    // assure nil for x64
    altdem_lockex := nil ;

  try
    generateEx( dem_lockex, flowdir_lockex, altdem_lockex ) ;
  finally
    FreeObject( dem_lockex ) ;
    FreeObject( flowdir_lockex ) ;
    _dem.UnlockPixels( dem_lock ) ;
    _flowDir.UnlockPixels( flowdir_lock ) ;
    if assigned( _alteredDem ) then begin
      FreeObject( altdem_lockex ) ;
      _alteredDem.UnlockPixels( altdem_lock ) ;
    end ;
  end ;
end;

procedure T_HydrologyResolveFlat.generateEx(
  const _demLockEx     : T_PixelLockAdapter ;
  const _flowDirLockEx : T_PixelLockAdapter ;
  const _altDemLockEx  : T_PixelLockAdapter
) ;
const
  ALGORITHM_STAGES = 5 ;
var
  flat_mask      : TGIS_IntegerGridArray ;
  labels         : TGIS_IntegerGridArray ;
begin
  busyEventManager.StartEvent( GIS_RS_BUSY_HYDROLOGY_FLAT, ALGORITHM_STAGES ) ;
  try
    // stages 1-4
    resolveFlats( _demLockEx, _flowDirLockEx, flat_mask, labels ) ;

    if busyEventManager.Aborted then
      exit ;

    // stage 5
    if assigned( _altDemLockEx ) then
      // increasing the elevation of cells from the original DEM
      alterDem( _demLockEx, flat_mask, labels, _altDemLockEx )
    else
      // or determine flow directions across flats,
      d8MaskedFlowDirs( flat_mask, _flowDirLockEx, labels ) ;
  finally
    busyEventManager.EndEvent ;
  end;
end;

function T_HydrologyResolveFlat.findFlats(
  const _lowEdges  : TQueue< TPoint > ;
  var   _labels    : TGIS_IntegerGridArray ;
  const _demLockEx : T_PixelLockAdapter
) : Integer ;
var
  label_id : Integer ;
  {$IFNDEF OXYGENE OR ISLAND}
    cell   : TPoint ;
  {$ENDIF}
begin
  Result := 0 ;

  busyEventManager.StartEvent(
    GIS_RS_BUSY_HYDROLOGY_FLAT_LABELS,
    _lowEdges.Count
  ) ;
  try
    label_id := 1 ;
    for cell in _lowEdges do begin
      if busyEventManager.PushEvent then
        exit ;

      if ( _labels[cell.Y, cell.X] = 0 )  then begin
        labelFlats( cell, label_id, _demLockEx, _labels ) ;
        inc( label_id ) ;
      end ;
    end ;
  finally
    busyEventManager.EndEvent ;
  end ;

  Result := label_id ;
end;

procedure T_HydrologyResolveFlat.resolveFlats(
  const _demLockEx     : T_PixelLockAdapter ;
  const _flowDirLockEx : T_PixelLockAdapter  ;
  var   _flatMask      : TGIS_IntegerGridArray ;
  var   _labels        : TGIS_IntegerGridArray
) ;
var
  label_no    : Integer ;
  high_edges  : TQueue< TPoint > ;
  low_edges   : TQueue< TPoint > ;
  tmp_edges   : TQueue< TPoint > ;
  flat_height : TArray< Integer > ;
  {$IFNDEF OXYGENE OR ISLAND}
    cell      : TPoint ;
  {$ENDIF}
begin
  high_edges := TQueue< TPoint >.Create ;
  low_edges := TQueue< TPoint >.Create ;
  tmp_edges := TQueue< TPoint >.Create ;
  try
    SetLength( _flatMask, _demLockEx.NRows, _demLockEx.NCols ) ;
    SetLength( _labels, _demLockEx.NRows, _demLockEx.NCols ) ;

    {$IFNDEF GIS_UNIT_TESTS}
      assert( _flatMask[0,0] = 0, 'grid not initialized withg 0' ) ;
      assert( _labels[_demLockEx.NRows-1, _demLockEx.NCols-1] = 0, 'grid not initialized with 0' ) ;
    {$ENDIF}

    // stage 1 - low & high edges searching
    flatEdges( high_edges, low_edges, _demLockEx, _flowDirLockEx ) ;
    if busyEventManager.Aborted then
      exit ;

    // No flats or flats does not have outlets
    if T_HUtils.QueueIsEmpty( low_edges ) then
     exit ;

    // stage 2 - finding & labelling flats
    label_no := findFlats( low_edges, _labels, _demLockEx ) ;
    if busyEventManager.Aborted then
      exit ;

    // removing flats without outlets from the queue
    for cell in high_edges do begin
      if ( _labels[cell.Y, cell.X] <> 0 ) then
        tmp_edges.Enqueue( cell ) ;
    end ;

    // if any cell was removed from HighEdges, not all flats have outlets
    if ( tmp_edges.Count < high_edges.Count ) then begin
      // replace HighEdges queue
      high_edges.Clear ;

      while not T_HUtils.QueueIsEmpty( tmp_edges ) do
        high_edges.Enqueue( tmp_edges.Dequeue ) ;
    end ;

    // an array with length equal to the value of Label
    SetLength( flat_height, label_no ) ;

    // stage 3 - building a gradient away from higher terrain
    awayFromHigher( high_edges, flat_height, _labels, _flatMask, _flowDirLockEx ) ;
    if busyEventManager.Aborted then
      exit ;

    // stage 4 - building a gradient towards lower terrain
    towardsLower( low_edges, flat_height, _labels, _flatMask, _flowDirLockEx ) ;
    if busyEventManager.Aborted then
      exit ;
  finally
    FreeObject( low_edges ) ;
    FreeObject( high_edges ) ;
    FreeObject( tmp_edges ) ;
  end ;
end;

procedure T_HydrologyResolveFlat.d8FlowDirs(
  const _dem     : TGIS_LayerPixel ;
  const _flowDir : TGIS_LayerPixel
) ;
begin
  // not implemented
end;

function T_HydrologyResolveFlat.isDirectionNoFlow(
  const _dirCode : Integer
) : Boolean ;
begin
  Result := ( _dirCode < 0 ) or not T_HUtils.CellOnlyFlowsToOneCell( _dirCode ) ;
end;

procedure T_HydrologyResolveFlat.flatEdges(
  const _highEdges   : TQueue< TPoint > ;
  const _lowEdges    : TQueue< TPoint > ;
  const _demLockEx     : T_PixelLockAdapter ;
  const _flowDirLockEx : T_PixelLockAdapter
) ;
var
  n            : Integer ;
  c, r, cn, rn : Integer ;
  dir, dir_n   : Integer ;
  elev, elev_n : Single ;
begin
  busyEventManager.StartEvent( GIS_RS_BUSY_HYDROLOGY_FLAT_EDGES, _demLockEx.NRows ) ;
  try
    for r := 0 to _demLockEx.FNRows-1 do begin
      if busyEventManager.PushEvent then
        exit ;

      for c := 0 to _demLockEx.FNCols-1 do begin
        if _flowDirLockEx.CellIsNodataElseGetValue( r, c, dir ) then
          continue ;

        elev := _demLockEx.GridRel[r, c] ;

        for n := 0 to NEIGHBOR_COUNT-1 do begin
          T_HUtils.GetNeighbor( r, c, n, rn, cn ) ;

          if not _flowDirLockEx.CellIsInGrid( rn, cn ) then
            continue ;

          if _flowDirLockEx.CellIsNodataElseGetValue( rn, cn, dir_n ) then
            continue ;

          elev_n := _demLockEx.GridRel[rn, cn] ;

          if not isDirectionNoFlow( dir ) and
             isDirectionNoFlow( dir_n ) and
             GisIsSameValue( elev, elev_n, GIS_SINGLE_RESOLUTION ) then
          begin
            _lowEdges.Enqueue( Point( c, r ) ) ;
            break ;
          end
          else if ( isDirectionNoFlow( dir ) and ( elev < elev_n ) ) then begin
            _highEdges.Enqueue( Point( c, r ) ) ;
            break ;
          end ;
        end ;
      end ;
    end ;
  finally
    busyEventManager.EndEvent ;
  end;
end;

procedure T_HydrologyResolveFlat.labelFlats(
  const _cell      : TPoint ;
  const _labelId   : Integer ;
  const _demLockEx : T_PixelLockAdapter ;
  const _labelsGrd : TGIS_IntegerGridArray
) ;
var
  n            : Integer ;
  c, r, cn, rn : Integer ;
  elev         : Single ;
  cell         : TPoint ;
  to_fill      : TQueue< TPoint > ;
begin
  to_fill := TQueue< TPoint >.Create ;
  try
    to_fill.Enqueue( _cell ) ;

    elev := _demLockEx.GridRel[_cell.Y, _cell.X] ;

    while not T_HUtils.QueueIsEmpty( to_fill ) do begin
      cell := to_fill.Dequeue ;
      c := cell.X ;
      r := cell.Y ;

      // cell is not a part of the flat
      if not GisIsSameValue( _demLockEx.GridRel[r, c], elev, GIS_SINGLE_RESOLUTION ) then
        continue ;

      // cell is already labeled
      if ( _labelsGrd[r, c] > 0 ) then
        continue ;

      _labelsGrd[r, c] := _labelId ;

      // push all neighors of cell onto ToFill
      for n := 0 to 7 do begin
        T_HUtils.GetNeighbor( r, c, n, rn, cn ) ;

        if _demLockEx.CellIsInGrid( rn, cn ) then
          to_fill.Enqueue( Point( cn, rn ) ) ;
      end ;
    end ;
  finally
    FreeObject( to_fill ) ;
  end ;
end;

procedure T_HydrologyResolveFlat.awayFromHigher(
  const _highEdges     : TQueue< TPoint > ;
  const _flatHeight    : TArray< Integer > ;
  const _labelsGrd     : TGIS_IntegerGridArray ;
  const _flatMaskGrd   : TGIS_IntegerGridArray ;
  const _flowDirLockEx : T_PixelLockAdapter
) ;
var
  n              : Integer ;
  c, r, cn, rn   : Integer ;
  loops          : Integer ;
  marker, cell   : TPoint ;
begin
  loops := 1 ;
  marker := Point( -1, -1 ) ;

  busyEventManager.StartEvent(
    GIS_RS_BUSY_HYDROLOGY_FLAT_HIGHER,
    _highEdges.Count + 1
  ) ;
  try
    _highEdges.Enqueue( marker ) ;
    while ( _highEdges.Count > 1 ) do begin
      if busyEventManager.PushEvent then
        exit ;

      cell := _highEdges.Dequeue ;
      c := cell.X ;
      r := cell.Y ;

      // cell is a marker
      if ( c = -1 ) then begin
        inc( loops ) ;
        _highEdges.Enqueue( marker ) ;
        continue ;
      end ;

      if ( _flatMaskGrd[r, c] > 0 ) then
        continue ;

      _flatMaskGrd[r, c] := loops ;
      _flatHeight[_labelsGrd[r,c]] := loops ;

      for n := 0 to 7 do begin
        T_HUtils.GetNeighbor( r, c, n, rn, cn ) ;

        if _flowDirLockEx.CellIsInGrid( rn, cn ) and
           ( _labelsGrd[rn, cn] = _labelsGrd[r, c] ) and
           isDirectionNoFlow( _flowDirLockEx.GridRelInt[rn, cn] )
        then
          _highEdges.Enqueue( Point( cn, rn ) ) ;
      end ;
    end ;
  finally
    busyEventManager.EndEvent ;
  end;
end;

procedure T_HydrologyResolveFlat.towardsLower(
  const _lowEdges      : TQueue< TPoint > ;
  const _flatHeight    : TArray< Integer > ;
  const _labelsGrd     : TGIS_IntegerGridArray ;
  const _flatMaskGrd   : TGIS_IntegerGridArray ;
  const _flowDirLockEx : T_PixelLockAdapter
) ;
var
  n              : Integer ;
  c, r, cn, rn   : Integer ;
  loops          : Integer ;
  marker, cell   : TPoint ;
begin
  loops := 1 ;
  marker := Point(-1, -1) ;

  busyEventManager.StartEvent(
    GIS_RS_BUSY_HYDROLOGY_FLAT_LOWER,
    _lowEdges.Count + 1
  ) ;
  try
    // make awayFromHigher mask negative
    for r := 0 to _flowDirLockEx.NRows-1 do
      for c := 0 to _flowDirLockEx.NCols-1 do
        _flatMaskGrd[r, c] := -1 * _flatMaskGrd[r, c] ;

    _lowEdges.Enqueue( marker ) ;
    while ( _lowEdges.Count > 1 ) do begin
      if busyEventManager.PushEvent then
        exit ;

      cell := _lowEdges.Dequeue ;
      c := cell.X ;
      r := cell.Y ;
      // cell is a marker
      if ( c = -1 ) then begin
        inc( loops ) ;
        _lowEdges.Enqueue( marker ) ;
        continue;
      end;

      if ( _flatMaskGrd[r, c] > 0 ) then
        continue;

      if ( _flatMaskGrd[r, c] <> 0 ) then
        _flatMaskGrd[r, c] := _flatHeight[_labelsGrd[r, c]] +
         _flatMaskGrd[r, c] + 2 * loops
      else
        _flatMaskGrd[r, c] := 2 * loops ;

      for n := 0 to NEIGHBOR_COUNT-1 do begin
        T_HUtils.GetNeighbor( r, c, n, rn, cn ) ;

        if _flowDirLockEx.CellIsInGrid( rn, cn ) and
           ( _labelsGrd[rn, cn] = _labelsGrd[r, c] ) and
           isDirectionNoFlow( _flowDirLockEx.GridRelInt[rn, cn] )
        then
          _lowEdges.Enqueue( Point( cn, rn ) ) ;
      end ;
    end ;
  finally
    busyEventManager.EndEvent ;
  end ;
end;

procedure T_HydrologyResolveFlat.d8MaskedFlowDirs(
  const _flatMask      : TGIS_IntegerGridArray ;
  const _flowDirLockEx : T_PixelLockAdapter    ;
  const _labels        : TGIS_IntegerGridArray
) ;
const
  NOFLOW = 0 ;
var
  c, r, cn, rn  : Integer;
  flow_dir      : Integer;
  e_min, e_mask : Integer  ;
  n, n_min      : Integer;
begin
  busyEventManager.StartEvent( GIS_RS_BUSY_HYDROLOGY_FLAT_ALTER, _flowDirLockEx.NRows ) ;
  try
    for r := 0 to _flowDirLockEx.NRows-1 do begin
      if busyEventManager.PushEvent then
        exit ;

      for c := 0 to _flowDirLockEx.NCols-1 do begin
        if _flowDirLockEx.CellIsNodataElseGetValue( r, c, flow_dir ) then
          continue ;

        if not isDirectionNoFlow( flow_dir ) then
          continue ;

        e_min := _flatMask[r, c] ;
        n_min := NOFLOW ;
        for n := 0 to NEIGHBOR_COUNT-1 do begin
          T_HUtils.GetNeighbor( r, c, n, rn, cn ) ;

          if not _flowDirLockEx.CellIsInGrid( rn, cn ) then
            continue ;

          // neighbour cell will always be inside grid
          if ( _labels[rn, cn] <> _labels[r, c]  ) then
            continue ;

          e_mask := _flatMask[rn, cn] ;
          if ( e_mask < e_min ) or
             ( ( e_mask = e_min ) and
               ( n_min > 0 ) and
               ( ( n_min mod 2 ) = 0 ) and
               ( ( n mod 2 ) = 1 ) ) then
          begin
            e_min := e_mask ;
            n_min := n ;
          end ;
        end ;

        _flowDirLockEx.GridRel[r, c] := T_HUtils.IndexToDirection( n_min ) ;
      end ;
    end ;
  finally
    busyEventManager.EndEvent ;
  end;
end ;

procedure T_HydrologyResolveFlat.alterDem(
  const _demLockEx    : T_PixelLockAdapter ;
  const _flatMask     : TGIS_IntegerGridArray ;
  const _labels       : TGIS_IntegerGridArray ;
  const _altDemLockEx : T_PixelLockAdapter
) ;
var
  c, r : Integer;
  elev : Single ;
begin
  busyEventManager.StartEvent(
    GIS_RS_BUSY_HYDROLOGY_FLAT_ALTER,
    _demLockEx.NRows
  ) ;
  try
    for r := 0 to _demLockEx.NRows-1 do begin
      if busyEventManager.PushEvent then
        exit ;

      for c := 0 to _demLockEx.NCols-1 do begin
        elev := _demLockEx.GridRel[r, c] ;

        if ( _labels[r, c] > 0 ) then
          elev := elev + _flatMask[r, c] * DZMIN ;

        _altDemLockEx.GridRel[r, c] := elev ;
      end ;
    end ;
  finally
    busyEventManager.EndEvent ;
  end ;
end;
{$ENDREGION}

{$REGION 'T_HydrologyFlowDirection'}
constructor T_HydrologyFlowDirection.Create ;
begin
  inherited ;

  FResolveFlats := TGIS_Hydrology.FLOWDIR_RESOLVE_FLAT ;
end;

procedure T_HydrologyFlowDirection.Generate(
  const _dem       : TGIS_LayerPixel ;
  const _extent    : TGIS_Extent ;
  const _flowDir  : TGIS_LayerPixel
) ;
var
  algorithm : T_FlowDirection ;
begin
  verifyLayer( _dem, _flowDir ) ;

  algorithm := T_FlowDirection.Create(
    busyEventManager,
    T_FlowDirectionAlgorithm.FlowDirection
  ) ;
  try  //? refactor and create methods GenerateFlowDirection and GenerateSinks
    algorithm.Generate(
      _dem,
      _extent,
      _flowDir,
      ResolveFlat
    ) ;
  finally
    FreeObject( algorithm ) ;
  end ;
end;
{$ENDREGION}

{$REGION 'T_HydrologyFlowAccumulation'}
constructor T_HydrologyFlowAccumulation.Create ;
begin
  inherited ;
end;

procedure T_HydrologyFlowAccumulation.Generate(
  const _flowDir : TGIS_LayerPixel ;
  const _extent  : TGIS_Extent ;
  const _flowAcc : TGIS_LayerPixel
) ;
var
  algorithm      : T_Downstream ;
  flowdir_lock   : TGIS_LayerPixelLock ;
  flowacc_lock   : TGIS_LayerPixelLock ;
  flowdir_lockex : T_PixelLockAdapter ;
  flowacc_lockex : T_PixelLockAdapter ;
begin
  verifyLayer( _flowDir, _flowAcc ) ;

  _flowDir.Open ;
  _flowAcc.Open ;

  _flowAcc.MinHeight := GIS_MAX_SINGLE ;
  _flowAcc.MaxHeight := -GIS_MAX_SINGLE ;

  flowdir_lock := _flowDir.LockPixels( _extent, _flowDir.CS, False ) ;
  flowacc_lock := _flowAcc.LockPixels( _extent, _flowDir.CS, True ) ;
  flowdir_lockex := T_PixelLockAdapter.Create( flowdir_lock ) ;
  flowacc_lockex := T_PixelLockAdapter.Create( flowacc_lock ) ;
  algorithm := T_Downstream.Create( busyEventManager ) ;
  try
    algorithm.GenerateFlowAccumulation( flowdir_lockex, flowacc_lockex ) ;
  finally
    FreeObject( flowdir_lockex ) ;
    FreeObject( flowacc_lockex ) ;
    _flowDir.UnlockPixels( flowdir_lock ) ;
    _flowAcc.UnlockPixels( flowacc_lock ) ;
    FreeObject( algorithm ) ;
  end ;
end;
{$ENDREGION}

{$REGION 'T_HydrologyBasin'}
constructor T_HydrologyBasin.Create ;
begin
  inherited ;
  FThreshold := TGIS_Hydrology.BASIN_THRESHOLD ;
end;

procedure T_HydrologyBasin.Generate(
  const _flowDir : TGIS_LayerPixel ;
  const _extent  : TGIS_Extent ;
  const _basins  : TGIS_LayerPixel
) ;
var
  algorithm : T_Upstream ;
begin
  verifyLayer( _flowDir, _basins ) ;

  algorithm := T_Upstream.Create( busyEventManager ) ;
  try
    algorithm.GenerateBasins( _flowDir, _extent, _basins, Threshold ) ;
    //? refactor and use lockex?
  finally
    FreeObject( algorithm ) ;
  end;
end ;
{$ENDREGION}

{$REGION 'T_HydrologyWatershed'}
constructor T_HydrologyWatershed.Create ;
begin
  inherited ;
end;

procedure T_HydrologyWatershed.prepareOutletsPixelLayer(
  const _outlets : TGIS_LayerPixel ;
  const _flowDir     : TGIS_LayerPixel
) ;
const
  OUTLETS_LAYER_NAME = 'outlets' ;
begin
  _outlets.Build(
    True,
    _flowDir.CS,
    _flowDir.Extent,
    _flowDir.BitWidth,
    _flowDir.BitHeight
  ) ;
  _outlets.Name := OUTLETS_LAYER_NAME ;
  _outlets.Params.Pixel.Antialias := False ;
  _outlets.Params.Pixel.GridShadow := False ;
end;

procedure T_HydrologyWatershed.Generate(
  const _flowDir   : TGIS_LayerPixel ;
  const _outlets   : TGIS_LayerPixel ;
  const _extent    : TGIS_Extent ;
  const _watershed : TGIS_LayerPixel
) ;
var
  algorithm : T_Upstream ;
begin
  verifyLayer( _flowDir, _outlets ) ;
  verifyLayer( _flowDir, _watershed ) ;

  algorithm := T_Upstream.Create( busyEventManager ) ;
  try
    algorithm.GenerateWatershed( _flowDir, _outlets, _extent, _watershed ) ;
  finally
    FreeObject( algorithm ) ;
  end;
end;

procedure T_HydrologyWatershed.Generate(
  const _flowDir   : TGIS_LayerPixel ;
  const _outlets   : TGIS_LayerVector ;
  const _field     : String ;
  const _extent    : TGIS_Extent ;
  const _watershed : TGIS_LayerPixel
) ;
var
  field      : String ;
  field_id   : Integer ;
  pc_to_grid : TGIS_PointCloudToGrid ;
  outlets_lp : TGIS_LayerPixel ;
begin
  verifyLayer( _flowDir, _watershed ) ;

  field_id := _outlets.FindField( _field ) ;
  if ( field_id < 0 ) or ( field_id >= GIS_FIELD_ID_UID )  then
    field := GIS_FIELD_UID
  else if ( _outlets.Fields[ field_id ].FieldType <> TGIS_FieldType.Number ) then
    field := GIS_FIELD_UID
  else
    field := _field ;

  outlets_lp := TGIS_LayerPixel.Create ;
  pc_to_grid := TGIS_PointCloudToGrid.Create ;
  try
    prepareOutletsPixelLayer( outlets_lp, _flowDir ) ;

    pc_to_grid.Assignment := TGIS_PointCloudAssignment.First ;
    pc_to_grid.WindowSize := 1 ;
    pc_to_grid.Generate( _outlets, _extent, field, outlets_lp, _extent ) ;

    Generate( _flowDir, outlets_lp, _extent, _watershed ) ;
  finally
    FreeObject( outlets_lp ) ;
    FreeObject( pc_to_grid) ;
  end ;
end;
{$ENDREGION}

{$REGION 'T_HydrologyFill'}
constructor T_HydrologyFill.Create ;
begin
  inherited ;
  FApplySlope := TGIS_Hydrology.FILL_APPLY_SLOPE ;
end;

{$IFDEF DCC}
  {$IFNDEF LEVEL_XE7_RTL}
     {$DEFINE NODYNAMIC_ARRAY_INITIALIZATION}
  {$ENDIF}
{$ENDIF}

procedure T_HydrologyFill.initializeConstans(
  const _nRows : Integer ;
  const _nCols : Integer
) ;
begin
  {$IFDEF NODYNAMIC_ARRAY_INITIALIZATION}
    // description below
    SetLength( R0, NEIGHBOR_COUNT ) ;
    R0[0] := 1        ;
    R0[1] := _nRows-2 ;
    R0[2] := 1        ;
    R0[3] := _nRows-2 ;
    R0[4] := 1        ;
    R0[5] := _nRows-2 ;
    R0[6] := 1        ;
    R0[7] := _nRows-2 ;

    SetLength( C0, NEIGHBOR_COUNT ) ;
    C0[0] := 1        ;
    C0[1] := _nCols-2 ;
    C0[2] := _nCols-2 ;
    C0[3] := 1        ;
    C0[4] := _nCols-2 ;
    C0[5] := 1        ;
    C0[6] := 1        ;
    C0[7] := _nCols-2 ;

    SetLength( dR, NEIGHBOR_COUNT) ;
    dR[0] :=  0 ;
    dR[1] :=  0 ;
    dR[2] :=  1 ;
    dR[3] := -1 ;
    dR[4] :=  0 ;
    dR[5] :=  0 ;
    dR[6] :=  1 ;
    dR[7] := -1 ;

    SetLength( dC, NEIGHBOR_COUNT) ;
    dC[0] :=  1 ;
    dC[1] := -1 ;
    dC[2] :=  0 ;
    dC[3] :=  0 ;
    dC[4] := -1 ;
    dC[5] :=  1 ;
    dC[6] :=  0 ;
    dC[7] :=  0 ;

    SetLength( fR, NEIGHBOR_COUNT) ;
    fR[0] :=  1        ;
    fR[1] := -1        ;
    fR[2] := -_nRows+2 ;
    fR[3] :=  _nRows-2 ;
    fR[4] :=  1        ;
    fR[5] := -1        ;
    fR[6] := -_nRows+2 ;
    fR[7] :=  _nRows-2 ;

    SetLength( fC, NEIGHBOR_COUNT) ;
    fC[0] := -_nCols+2 ;
    fC[1] :=  _nCols-2 ;
    fC[2] := -1        ;
    fC[3] :=  1        ;
    fC[4] :=  _nCols-2 ;
    fC[5] := -_nCols+2 ;
    fC[6] :=  1        ;
    fC[7] := -1        ;
  {$ELSE}
    // starts at:
    // top-left, bottom-right, top-right, bottom-left, top-right, bottom-left, top-left, bottom-right
    R0 := [   1,     _nRows-2,         1,    _nRows-2,         1,    _nRows-2,        1,     _nRows-2 ] ;
    // then go:
    // rightwards, leftwards,     down, up, leftwards, rightwards, down,       up
    C0 := [     1,  _nCols-2, _nCols-2,  1,  _nCols-2,          1,    1, _nCols-2 ] ;

    dR := [ 0,  0,  1, -1,  0,  0,  1, -1 ] ;
    dC := [ 1, -1,  0,  0, -1,  1,  0,  0 ] ;

    // row/column change when a border is reached
    // go to row:
    // next down, next up,     first,     last, next down, next up,     first,     last
    fR := [    1,      -1, -_nRows+2, _nRows-2,         1,      -1, -_nRows+2, _nRows-2 ] ;
    // and column:
    //          first,     last, next left, next right,     last,     first, next right, next left
    fC := [ -_nCols+2, _nCols-2,        -1,          1, _nCols-2, -_nCols+2,          1,        -1 ] ;
  {$ENDIF}
end;

procedure T_HydrologyFill.initializeEpsilon(
  const _applySlope : Boolean
) ;
  var
    i      : Integer ;
    sqrt2  : Single ;
    factor : Single ;
  begin
    factor := 1 ;
    sqrt2 := Sqrt( 2 ) ;
    for i := 0 to NEIGHBOR_COUNT-1 do begin
      if _applySlope then
        case ( i mod 2 ) of
          0 : factor := sqrt2 ;
          1 : factor := 1 ;
        end
      else
        factor := 0 ;

      epsilonArr[i] := DZMIN * factor ;
    end ;
  end ;

procedure T_HydrologyFill.Generate(
  const _dem      : TGIS_LayerPixel ;
  const _extent   : TGIS_Extent ;
  const _hydroDem : TGIS_LayerPixel
) ;
var
  Z_lock         : TGIS_LayerPixelLock ;
  W_lock         : TGIS_LayerPixelLock ;
  Z_lockex       : T_PixelLockAdapter ;
  W_lockex       : T_PixelLockAdapter ;
  border_cells_q : TQueue< TPoint > ;
begin
  _dem.Open ;
  _hydroDem.Open ;

  _hydroDem.MinHeight := GIS_MAX_SINGLE ;
  _hydroDem.MaxHeight := -GIS_MAX_SINGLE ;

  Z_lock := _dem.LockPixels( _extent, _dem.CS, False ) ;
  W_lock := _hydroDem.LockPixels( _extent, _dem.CS, True ) ;
  Z_lockex := T_PixelLockAdapter.Create( Z_lock ) ;
  W_lockex := T_PixelLockAdapter.Create( W_lock ) ;
  border_cells_q := TQueue< TPoint >.Create ;
  busyEventManager.StartEvent( GIS_RS_BUSY_HYDROLOGY_FILL, 2 ) ;
  try
    // Stage 1
    initializeSurface( Z_lockex, W_lockex, border_cells_q ) ;
    if busyEventManager.Aborted then
      exit ;

    // Stage 2
    drainExcessWater( Z_lockex, W_lockex, border_cells_q ) ;
    if busyEventManager.Aborted then
      exit ;
  finally
    FreeObject( border_cells_q ) ;
    FreeObject( Z_lockex ) ;
    FreeObject( W_lockex ) ;
    _dem.UnlockPixels( Z_lock ) ;
    _hydroDem.UnlockPixels( W_lock ) ;
    busyEventManager.EndEvent ;
  end ;
end ;

procedure T_HydrologyFill.initializeSurface(
  const _zLockEx      : T_PixelLockAdapter ;
  const _wLockEx      : T_PixelLockAdapter;
  const _borderCells : TQueue< TPoint >
) ;
var
  c, r, cn, rn : Integer ;
  i            : Integer ;
  Zc           : Single ;
  is_border    : Boolean ;
begin
  busyEventManager.StartEvent(
    GIS_RS_BUSY_HYDROLOGY_FILL_STAGE1,
    _zLockEx.NRows * _wLockEx.NCols
  ) ;
  try
    // For each cell of the DEM (in any order)
    for r := 0 to _zLockEx.NRows-1 do begin
      for c := 0 to _zLockEx.NCols-1 do begin
        if busyEventManager.PushEvent then
          exit ;

        // handle NODATA
        if _zLockEx.CellIsNodata( r, c ) then begin
          _wLockEx.SetNodata( r, c ) ;
          continue ;
        end ;

        is_border := False ;

        // if neighbor is NOT in the grid, the cell is on the border
        for i := 0 to NEIGHBOR_COUNT-1 do begin
          cn := c + DC_NEIGHBOR[i] ;
          rn := r + DR_NEIGHBOR[i] ;

          // treat NODATA as border
          if not _zLockEx.CellIsInGrid( rn, cn ) or
            _zLockEx.CellIsNodata( rn, cn ) then
          begin
            is_border := True ;
            break ;
          end ;
        end ;

        if is_border then begin
          // W(c) = Z(c)
          Zc := _zLockEx.GridRel[r, c] ;
          _wLockEx.GridRel[r, c] := Zc ;

          // collect border cells, stage 2 will start drying from them
          _borderCells.Enqueue( Point( c, r ) ) ;
        end
        else
          // W(c) = infinitite altitude
          _wLockEx.GridRel[r, c] := GIS_MAX_SINGLE ;
      end ;
    end ;
  finally
    busyEventManager.EndEvent ;
  end;
end;

procedure T_HydrologyFill.dryUpwardCell(
  const _row       : Integer ;
  const _col       : Integer ;
  const _zLockEx : T_PixelLockAdapter ;
  const _wLockEx : T_PixelLockAdapter ;
  var   _depth    : Integer
) ;
var
  n      : Integer;
  rn, cn : Integer ;
  Zc, Zn : Single ;
  Wc, Wn : Single ;
begin
  inc( _depth ) ;

  if ( _depth <= MAX_DEPTH ) then begin
    rn := _row ;
    cn := _col ;

    // for each neighbor n of c (in any order)
    for n := 0 to NEIGHBOR_COUNT-1 do begin
      rn := _row + DR_NEIGHBOR[n] ;
      cn := _col + DC_NEIGHBOR[n] ;

      if not _zLockEx.CellIsInGrid( rn, cn ) then
        continue ;

      // get Zn, if cell is not NODATA
      if _zLockEx.CellIsNodataElseGetValue( rn, cn, Zn ) then
        continue ;

      Wn := _wLockEx.GridRel[rn, cn];

      // and W(n) = aHugeNumber then (statement changed based on Planchom's code)
      if not GisIsSameValue( Wn, Zn, GIS_SINGLE_RESOLUTION ) then begin
        Zc := _zLockEx.GridRel[_row, _col] ;
        Wc := _wLockEx.GridRel[_row, _col] ;

        // if operation (1) is applicable
        if ( Zn >= Wc + epsilonArr[n] ) then begin
          Wn := Zn ;  // W(n) = Z(n)
          _wLockEx.GridRel[rn, cn] := Wn ;

          // recursive call DryUpwardCell(n)
          dryUpwardCell( rn, cn, _zLockEx, _wLockEx, _depth ) ;
        end ;
      end ;
    end ;
  end ;

  dec( _depth ) ;
end;

procedure T_HydrologyFill.drainExcessWater(
  const _zLockEx     : T_PixelLockAdapter ;
  const _wLockEx     : T_PixelLockAdapter ;
  const _borderCells : TQueue< TPoint >
) ;
const
  COMPLEXITY_EXPONENT = 1.2 ;  // time-complexity from Planchon & Durboux paper
var
  depth          : Integer ;
  n, scan        : Integer;
  r, c, rn, cn   : Integer ;
  Zc, Wc, Wn     : Single ;
  all_scan_done  : Boolean ;
  something_done : Boolean ;
  cell           : TPoint ;

begin
  busyEventManager.StartEvent( GIS_RS_BUSY_HYDROLOGY_FILL_STAGE2, 2 ) ;
  try
    initializeConstans( _zLockEx.NRows, _zLockEx.NCols ) ;
    initializeEpsilon( FApplySlope ) ;

    // Section 1 - Explore all ascending paths from the border
    busyEventManager.StartEvent(
      GIS_RS_BUSY_HYDROLOGY_FILL_SECTION1,
      _borderCells.Count
    ) ;
    try
      // recursion depth
      depth := 0 ;

      // for each cell on the border (in any order) call DryUpwardCell(cell):
      // first row, last row, first column, last column, and cells adjacent to NODATA
      while not T_HUtils.QueueIsEmpty( _borderCells ) do begin
        if busyEventManager.PushEvent then
          exit ;

        cell := _borderCells.Dequeue ;
        c := cell.X ;
        r := cell.Y ;
        dryUpwardCell( r, c, _zLockEx, _wLockEx, depth ) ;
      end ;
    finally
      busyEventManager.EndEvent ;
    end;

    // Section 2 - Iteratively scan the DEM
    busyEventManager.StartEvent(
      GIS_RS_BUSY_HYDROLOGY_FILL_SECTION1,
      RoundS( Power( _zLockEx.NRows * _zLockEx.NCols, COMPLEXITY_EXPONENT ) )
    ) ;
    try
      repeat
        // original algorithm modified to break
        // if during all 8 direction scans, no cell was modified
        all_scan_done := False ;

        for scan := 0 to NEIGHBOR_COUNT-1 do begin
          r := R0[scan] ;
          c := C0[scan] ;
          something_done := False ;

          repeat
            Wc := _wLockEx.GridRel[r, c] ;

            // get Zc, if not NODATA
            if _zLockEx.CellIsNodataElseGetValue( r, c, Zc ) then
              continue ;

            if ( Wc > Zc ) then begin                       // (4)
              if busyEventManager.PushEvent then
                exit ;

              // for each existing neighbor n of cell (in any order)
              for n := 0 to NEIGHBOR_COUNT-1 do begin
                T_HUtils.GetNeighbor( r, c, n, rn, cn ) ;

                // get Wn from neigbor, if cell is not NODATA
                if _wLockEx.CellIsNodataElseGetValue( rn, cn, Wn ) then
                  continue ;

                if ( Zc >= Wn + epsilonArr[n] ) then begin  // operation (1)
                  Wc := Zc ;                                // W(c) = Z(c)
                  _wLockEx.GridRel[r, c] := Wc ;

                  something_done := True ;
                  dryUpwardCell( r, c, _zLockEx, _wLockEx, depth ) ;
                  break ;                                   // goto 22
                end ;

                if ( Wc > Wn + epsilonArr[n] ) then begin   // operation (2)
                  Wc := Wn + epsilonArr[n] ;                // W(c) = W(n) + epsilon(c,n)
                  _wLockEx.GridRel[r, c] := Wc ;
                  something_done := True ;
                end ;
              end ;
            end ;
          until not nextCell( r, c, scan, _zLockEx.NRows, _zLockEx.NCols ) ;  // (22)

          all_scan_done := all_scan_done or something_done ;
        end ;
      until all_scan_done ;

    finally
      busyEventManager.EndEvent ;
    end ;
  finally
    busyEventManager.EndEvent ;
  end ;
end;

function T_HydrologyFill.nextCell(
  var   _row      : Integer ;  // next cell row
  var   _col      : Integer ;  // next cell column
  const _iScanDir : Integer ;   // i-th scanning direction
  const _nRows    : Integer ;  // number of rows
  const _nCols    : Integer    // number of columns
) : Boolean ;
begin
  Result := True ;

  // shift to the neighbor point
  _row := _row + dR[_iScanDir] ;
  _col := _col + dC[_iScanDir] ;

  // cell out of the DEM
  if ( _row < 1 ) or ( _col < 1 ) or ( _row > _nRows-2 ) or ( _col > _nCols-2 ) then begin

    // shift the point to the beginning of a new row or a new column
    _row := _row + fR[_iScanDir] ;
    _col := _col + fC[_iScanDir] ;

    // cell out of the DEM
    if ( _row < 1 ) or ( _col < 1 ) or ( _row > _nRows-2 ) or ( _col > _nCols-2 ) then
      Result := False ;
  end ;
end;
{$ENDREGION}

{$REGION 'T_HydrologyStreamToPolyline'}
constructor T_HydrologyStreamToPolyline.Create;
begin
  inherited ;
  FThreshold := TGIS_Hydrology.STREAM_POLYLINE_THRESHOLD ;
end;

procedure T_HydrologyStreamToPolyline.Generate(
  const _flowDir    : TGIS_LayerPixel ;
  const _streamsPix : TGIS_LayerPixel ;  // accumulation or stream order
  const _extent     : TGIS_Extent ;
  const _streamsVec : TGIS_LayerVector ;
  const _orderField : String
) ;
var
  algorithm      : T_Upstream ;
begin
  verifyLayer( _flowDir, _streamsPix ) ;

  algorithm := T_Upstream.Create( busyEventManager ) ;
  try
    algorithm.GenerateStreamPolyline(
      _flowDir,
      _streamsPix,
      _extent,
      _streamsVec,
      _orderField,
      Threshold
    ) ;
  finally
    FreeObject( algorithm ) ;
  end ;
end;
{$ENDREGION}

{$REGION 'T_HydrologyStreamOrder'}
constructor T_HydrologyStreamOrder.Create;
begin
  inherited ;
  FMethod := TGIS_HydrologyStreamOrderMethod.Strahler ;
end ;

procedure T_HydrologyStreamOrder.Generate(
  const _flowDir : TGIS_LayerPixel ;
  const _flowAcc : TGIS_LayerPixel ;
  const _extent  : TGIS_Extent ;
  const _streams : TGIS_LayerPixel
) ;
var
  flowdir_lock   : TGIS_LayerPixelLock ;
  flowacc_lock   : TGIS_LayerPixelLock ;
  stream_lock    : TGIS_LayerPixelLock ;

  flowdir_lockex : T_PixelLockAdapter ;
  flowacc_lockex : T_PixelLockAdapter ;
  stream_lockex  : T_PixelLockAdapter ;

  algorithm      : T_HydrologyAlgorithmAbstract ;
begin
  verifyLayer( _flowDir, _flowAcc ) ;
  verifyLayer( _flowDir, _streams ) ;

  _flowDir.Open ;
  _flowAcc.Open ;
  _streams.Open ;

  _streams.MinHeight := GIS_MAX_SINGLE ;
  _streams.MaxHeight := -GIS_MAX_SINGLE ;

  flowdir_lock := _flowDir.LockPixels( _extent, _flowDir.CS, False ) ;
  flowacc_lock := _flowAcc.LockPixels( _extent, _flowDir.CS, False ) ;
  stream_lock := _streams.LockPixels( _extent, _flowDir.CS, True ) ;

  flowdir_lockex := T_PixelLockAdapter.Create( flowdir_lock ) ;
  flowacc_lockex := T_PixelLockAdapter.Create( flowacc_lock ) ;
  stream_lockex  := T_PixelLockAdapter.Create( stream_lock ) ;

  algorithm := T_StreamOrderAlgorithmFactory.CreateAlgorithm( busyEventManager, FMethod ) ;
  try
    case FMethod of
      TGIS_HydrologyStreamOrderMethod.Hack :
        T_Upstream( algorithm ).GenerateStreamOrderHack(
          flowdir_lockex,
          flowacc_lockex,
          stream_lockex,
          Threshold
        ) ;
      TGIS_HydrologyStreamOrderMethod.Shreve :
        T_Downstream( algorithm ).GenerateStreamOrderShreve(
          flowdir_lockex,
          flowacc_lockex,
          stream_lockex,
          Threshold
        ) ;
      TGIS_HydrologyStreamOrderMethod.Strahler :
        T_Downstream( algorithm ).GenerateStreamOrderStrahler(
          flowdir_lockex,
          flowacc_lockex,
          stream_lockex,
          Threshold
        ) ;
      TGIS_HydrologyStreamOrderMethod.Topological :
        T_Upstream( algorithm ).GenerateStreamOrderTopological(
          flowdir_lockex,
          flowacc_lockex,
          stream_lockex,
          Threshold
        ) ;

    end ;
  finally
    FreeObject( algorithm ) ;

    FreeObject( flowdir_lockex ) ;
    FreeObject( flowacc_lockex ) ;
    FreeObject( stream_lockex ) ;

    _flowDir.UnlockPixels( flowdir_lock ) ;
    _flowAcc.UnlockPixels( flowacc_lock ) ;
    _streams.UnlockPixels( stream_lock ) ;
  end ;
end;
{$ENDREGION}

{$REGION 'TGIS_Hydrology'}
constructor TGIS_Hydrology.Create;
begin
  busyEventManager := TGIS_BusyEventManager.Create( Self ) ;
end;

procedure TGIS_Hydrology.doDestroy;
begin
  FreeObject( busyEventManager ) ;
  inherited;
end;

{$IFDEF CLR}
  procedure TGIS_Hydrology.fadd_BusyEvent(
    const _value : TGIS_BusyEvent
  ) ;
  begin
    busyEventManager.BusyEvent += _value ;
  end ;

  procedure TGIS_Hydrology.fremove_BusyEvent(
    const _value : TGIS_BusyEvent
  ) ;
  begin
    busyEventManager.BusyEvent -= _value ;
  end ;
{$ELSE}
  procedure TGIS_Hydrology.fset_BusyEvent(
    const _value : TGIS_BusyEvent
  ) ;
  begin
    busyEventManager.BusyEvent := _value ;
  end ;

  function TGIS_Hydrology.fget_BusyEvent : TGIS_BusyEvent ;
  begin
    Result := busyEventManager.BusyEvent ;
  end ;
{$ENDIF}

procedure TGIS_Hydrology.Basin(
  const _flowDir   : TGIS_LayerPixel ;
  const _extent    : TGIS_Extent ;
  const _basins    : TGIS_LayerPixel ;
  const _threshold : Integer
);
var
  hydro_basin : T_HydrologyBasin ;
begin
  hydro_basin := T_HydrologyBasin.Create ;
  try
    hydro_basin.busyEventManager := busyEventManager ;
    hydro_basin.Threshold := _threshold ;
    hydro_basin.Generate( _flowDir, _extent, _basins ) ;
  finally
    FreeObject( hydro_basin ) ;
  end ;
end;

procedure TGIS_Hydrology.Fill(
  const _dem        : TGIS_LayerPixel ;
  const _extent     : TGIS_Extent ;
  const _hydroDem   : TGIS_LayerPixel ;
  const _applySlope : Boolean
) ;
var
  hydro_fill : T_HydrologyFill ;
begin
  hydro_fill := T_HydrologyFill.Create ;
  try
    hydro_fill.busyEventManager := busyEventManager ;
    hydro_fill.ApplySlope := _applySlope ;
    hydro_fill.Generate( _dem, _extent, _hydroDem ) ;
  finally
    FreeObject( hydro_fill ) ;
  end ;
end;

procedure TGIS_Hydrology.FlowAccumulation(
  const _flowDir : TGIS_LayerPixel ;
  const _extent  : TGIS_Extent ;
  const _flowAcc : TGIS_LayerPixel
) ;
var
  hydro_acc : T_HydrologyFlowAccumulation ;
begin
  hydro_acc := T_HydrologyFlowAccumulation.Create ;
  try
    hydro_acc.busyEventManager := busyEventManager ;
    hydro_acc.Generate( _flowDir, _extent, _flowAcc ) ;
  finally
    FreeObject( hydro_acc ) ;
  end ;
end;

procedure TGIS_Hydrology.FlowDirection(
  const _dem         : TGIS_LayerPixel ;
  const _extent      : TGIS_Extent ;
  const _flowDir     : TGIS_LayerPixel ;
  const _resolveFlat : Boolean
) ;
var
  hydro_dir : T_HydrologyFlowDirection ;
begin
  hydro_dir := T_HydrologyFlowDirection.Create ;
  try
    hydro_dir.busyEventManager := busyEventManager ;
    hydro_dir.ResolveFlat := _resolveFlat ;
    hydro_dir.Generate( _dem, _extent, _flowDir ) ;
  finally
    FreeObject( hydro_dir ) ;
  end ;
end;

procedure TGIS_Hydrology.ResolveFlat(
  const _dem        : TGIS_LayerPixel ;
  const _flowDir    : TGIS_LayerPixel ;
  const _extent     : TGIS_Extent ;
  const _alteredDem : TGIS_LayerPixel
) ;
var
  hydro_flat : T_HydrologyResolveFlat ;
begin
  hydro_flat := T_HydrologyResolveFlat.Create ;
  try
    hydro_flat.busyEventManager := busyEventManager ;
    hydro_flat.Generate( _dem, _flowDir, _extent, _alteredDem ) ;
  finally
    FreeObject( hydro_flat ) ;
  end ;
end;

procedure TGIS_Hydrology.Sink(
  const _dem    : TGIS_LayerPixel ;
  const _extent : TGIS_Extent ;
  const _sinks  : TGIS_LayerPixel
) ;
var
  hydro_sink : T_HydrologySink ;
begin
  hydro_sink := T_HydrologySink.Create ;
  try
    hydro_sink.busyEventManager := busyEventManager ;
    hydro_sink.Generate( _dem, _extent, _sinks ) ;
  finally
    FreeObject( hydro_sink ) ;
  end ;
end;

procedure TGIS_Hydrology.StreamOrder(
  const _flowDir   : TGIS_LayerPixel ;
  const _flowAcc   : TGIS_LayerPixel ;
  const _extent    : TGIS_Extent ;
  const _streams   : TGIS_LayerPixel ;
  const _method    : TGIS_HydrologyStreamOrderMethod ;
  const _threshold : Integer
) ;
var
  threshold   : Integer ;
  hydro_order : T_HydrologyStreamOrder ;
begin
  hydro_order := T_HydrologyStreamOrder.Create ;
  try
    hydro_order.busyEventManager := busyEventManager ;
    hydro_order.Method := _method ;
    if _threshold = -1 then
      threshold := RoundS( _flowAcc.MaxHeight / 100 )
    else
      threshold := _threshold ;

    hydro_order.Threshold := threshold ;
    hydro_order.Generate( _flowDir, _flowAcc, _extent, _streams ) ;
  finally
    FreeObject( hydro_order ) ;
  end ;
end;

procedure TGIS_Hydrology.StreamToPolyline(
  const _flowDir    : TGIS_LayerPixel ;
  const _streamsPix : TGIS_LayerPixel ;
  const _extent     : TGIS_Extent ;
  const _streamsVec : TGIS_LayerVector ;
  const _orderField : String ;
  const _threshold  : Integer
) ;
var
  hydro_polyline : T_HydrologyStreamToPolyline ;
begin
  hydro_polyline := T_HydrologyStreamToPolyline.Create ;
  try
    hydro_polyline.busyEventManager := busyEventManager ;
    hydro_polyline.Threshold := _threshold ;
    hydro_polyline.Generate( _flowDir, _streamsPix, _extent, _streamsVec, _orderField ) ;
  finally
    FreeObject( hydro_polyline ) ;
  end ;
end;

procedure TGIS_Hydrology.Watershed(
  const _flowDir   : TGIS_LayerPixel ;
  const _outlets   : TGIS_LayerVector ;
  const _field     : String ;
  const _extent    : TGIS_Extent ;
  const _watershed : TGIS_LayerPixel
) ;
var
  hydro_watershed : T_HydrologyWatershed ;
begin
  hydro_watershed := T_HydrologyWatershed.Create ;
  try
    hydro_watershed.busyEventManager := busyEventManager ;
    hydro_watershed.Generate( _flowDir, _outlets, _field, _extent, _watershed ) ;
  finally
    FreeObject( hydro_watershed ) ;
  end ;
end;

procedure TGIS_Hydrology.Watershed(
  const _flowDir   : TGIS_LayerPixel ;
  const _outlets   : TGIS_LayerPixel ;
  const _extent    : TGIS_Extent ;
  const _watershed : TGIS_LayerPixel
) ;
var
  hydro_watershed : T_HydrologyWatershed ;
begin
  hydro_watershed := T_HydrologyWatershed.Create ;
  try
    hydro_watershed.busyEventManager := busyEventManager ;
    hydro_watershed.Generate( _flowDir, _outlets, _extent, _watershed ) ;
  finally
    FreeObject( hydro_watershed ) ;
  end ;
end;
{$ENDREGION}

// Pipeline //

{$REGION 'T_Pipeline_HydrologyAbstract'}
type
  /// <summary>
  ///   Pipeline base hydrologic process adds Extent params for all derived operations
  /// </summary>
  T_Pipeline_HydrologyAbstract = class( TGIS_PipelineOperationAbstract)
    const
      PARAM_SOURCE_DEM      = GIS_PIPELINE_PARAM_SOURCE + 'DEM' ;
      PARAM_SOURCE_FLOW_DIR = GIS_PIPELINE_PARAM_SOURCE + 'FlowDirection' ;
      PARAM_SOURCE_FLOW_ACC = GIS_PIPELINE_PARAM_SOURCE + 'FlowAccumulation' ;
      PARAM_SOURCE_STREAM   = GIS_PIPELINE_PARAM_SOURCE + 'Stream' ;
      PARAM_SOURCE_OUTLETS  = GIS_PIPELINE_PARAM_SOURCE + 'Outlets' ;
      PARAM_THRESHOLD       = 'Threshold' ;

    public
      /// <summary>
      ///   Defines 2 params: "Extent" and "Save".
      /// </summary>
      procedure Initialize ; override ;

      /// <summary>
      ///   Performs destination layer saving if needed.
      /// </summary>
      procedure Execute ; override ;
  end ;

procedure T_Pipeline_HydrologyAbstract.Initialize ;
begin
  inherited ;

  defineParam(
    GIS_PIPELINE_PARAM_DESTINATION,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    '',
    ''
  ) ;
  defineParam(
    GIS_PIPELINE_PARAM_SAVE,
    TGIS_PipelineParameterType.Boolean,
    NaN,
    NaN,
    False,
    BoolToStr( GIS_PIPELINE_DEFAULT_SAVE ),
    ''
  ) ;
  defineParam(
    GIS_PIPELINE_PARAM_EXTENT,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    False,
    '',
    ''
  ) ;
end;

procedure T_Pipeline_HydrologyAbstract.Execute ;
var
  dst : TGIS_Layer ;
begin
  dst := ParamAsLayer( GIS_PIPELINE_PARAM_DESTINATION ) ;
  if ParamAsBool( GIS_PIPELINE_PARAM_SAVE, GIS_PIPELINE_DEFAULT_SAVE ) then
    dst.SaveAll ;
end;
{$ENDREGION}

{$REGION 'Pipeline - Basin'}
type
  /// <summary>
  ///   Implementation of the 'Hydrology.Basin' pipeline command.
  /// </summary>
  T_Pipeline_HydrologyBasin = class( T_Pipeline_HydrologyAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

procedure T_Pipeline_HydrologyBasin.Initialize ;
begin
  defineParam(
    PARAM_SOURCE_FLOW_DIR,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    '',
    ''
  ) ;

  inherited ;

  defineParam(
    PARAM_THRESHOLD,
    TGIS_PipelineParameterType.Int,
    0,
    NaN,
    False,
    IntToStr( TGIS_Hydrology.STREAM_ORDER_THRESHOLD ),
    ''
  ) ;
end;

procedure T_Pipeline_HydrologyBasin.Execute;
var
  src_flowdir : TGIS_LayerPixel ;
  dst         : TGIS_LayerPixel ;
  ext         : TGIS_Extent ;
  threshold   : Integer ;
  hydro_tool  : TGIS_Hydrology ;
begin
  src_flowdir := ParamAsLayerPixel( PARAM_SOURCE_FLOW_DIR ) ;
  dst := ParamAsLayerPixel( GIS_PIPELINE_PARAM_DESTINATION ) ;
  ext := ParamAsExtent( GIS_PIPELINE_PARAM_EXTENT, src_flowdir.Extent ) ;
  threshold := ParamAsInt( PARAM_THRESHOLD, TGIS_Hydrology.BASIN_THRESHOLD ) ;

  hydro_tool := TGIS_Hydrology.Create ;
  try
    hydro_tool.busyEventManager.UseProgressThreshold := False ;
    {$IFDEF CLR}
      hydro_tool.BusyEvent += DoBusyEvent ;
    {$ELSE}
      hydro_tool.BusyEvent := {$IFDEF OXYGENE}@{$ENDIF}DoBusyEvent ;
    {$ENDIF}
    hydro_tool.Basin( src_flowdir, ext, dst, threshold ) ;
  finally
    FreeObject( hydro_tool ) ;
  end ;

  inherited ;
end;
{$ENDREGION}

{$REGION 'Pipeline - Fill'}
type
  /// <summary>
  ///   Implementation of the 'Hydrology.Fill' pipeline command.
  /// </summary>
  T_Pipeline_HydrologyFill = class( T_Pipeline_HydrologyAbstract )
    const
      PARAM_APPLY_SLOPE = 'ApplySlope' ;

    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

procedure T_Pipeline_HydrologyFill.Initialize ;
begin
  defineParam(
    PARAM_SOURCE_DEM,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    '',
    ''
  ) ;

  inherited ;

  defineParam(
    PARAM_APPLY_SLOPE,
    TGIS_PipelineParameterType.Boolean,
    NaN,
    NaN,
    False,
    BoolToStr( TGIS_Hydrology.FILL_APPLY_SLOPE ),
    ''
  ) ;
end;

procedure T_Pipeline_HydrologyFill.Execute;
var
  src_dem     : TGIS_LayerPixel ;
  dst         : TGIS_LayerPixel ;
  ext         : TGIS_Extent ;
  apply_slope : Boolean ;
  hydro_tool  : TGIS_Hydrology ;
begin
  src_dem := ParamAsLayerPixel( PARAM_SOURCE_DEM ) ;
  dst := ParamAsLayerPixel( GIS_PIPELINE_PARAM_DESTINATION ) ;
  ext := ParamAsExtent( GIS_PIPELINE_PARAM_EXTENT, src_dem.Extent ) ;
  apply_slope := ParamAsBool( PARAM_APPLY_SLOPE, TGIS_Hydrology.FILL_APPLY_SLOPE) ;

  hydro_tool := TGIS_Hydrology.Create ;
  try
    hydro_tool.busyEventManager.UseProgressThreshold := False ;
    {$IFDEF CLR}
      hydro_tool.BusyEvent += DoBusyEvent ;
    {$ELSE}
      hydro_tool.BusyEvent := {$IFDEF OXYGENE}@{$ENDIF}DoBusyEvent ;
    {$ENDIF}
    hydro_tool.Fill( src_dem, ext, dst, apply_slope ) ;
  finally
    FreeObject( hydro_tool ) ;
  end ;

  inherited ;
end;
{$ENDREGION}

{$REGION 'Pipeline - FlowAccumulation'}
type
  /// <summary>
  ///   Implementation of the 'Hydrology.FlowAccumulation' pipeline command.
  /// </summary>
  T_Pipeline_HydrologyFlowAccumulation = class( T_Pipeline_HydrologyAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

procedure T_Pipeline_HydrologyFlowAccumulation.Initialize ;
begin
  defineParam(
    PARAM_SOURCE_FLOW_DIR,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    '',
    ''
  ) ;

  inherited ;
end;

procedure T_Pipeline_HydrologyFlowAccumulation.Execute;
var
  src_flowdir : TGIS_LayerPixel ;
  dst         : TGIS_LayerPixel ;
  ext         : TGIS_Extent ;
  hydro_tool  : TGIS_Hydrology ;
begin
  src_flowdir := ParamAsLayerPixel( PARAM_SOURCE_FLOW_DIR ) ;
  dst := ParamAsLayerPixel( GIS_PIPELINE_PARAM_DESTINATION ) ;
  ext := ParamAsExtent( GIS_PIPELINE_PARAM_EXTENT, src_flowdir.Extent ) ;

  hydro_tool := TGIS_Hydrology.Create ;
  try
    hydro_tool.busyEventManager.UseProgressThreshold := False ;
    {$IFDEF CLR}
      hydro_tool.BusyEvent += DoBusyEvent ;
    {$ELSE}
      hydro_tool.BusyEvent := {$IFDEF OXYGENE}@{$ENDIF}DoBusyEvent ;
    {$ENDIF}
    hydro_tool.FlowAccumulation( src_flowdir, ext, dst ) ;
  finally
    FreeObject( hydro_tool ) ;
  end ;

  inherited ;
end;
{$ENDREGION}

{$REGION 'Pipeline - FlowDirection'}
type
  /// <summary>
  ///   Implementation of the 'Hydrology.FlowDirection' pipeline command.
  /// </summary>
  T_Pipeline_HydrologyFlowDirection = class( T_Pipeline_HydrologyAbstract )
    const
      PARAM_RESOLVE_FLAT = 'ResolveFlat' ;

    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

procedure T_Pipeline_HydrologyFlowDirection.Initialize ;
begin
  defineParam(
    PARAM_SOURCE_DEM,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    '',
    ''
  ) ;

  inherited ;

  defineParam(
    PARAM_RESOLVE_FLAT,
    TGIS_PipelineParameterType.Boolean,
    NaN,
    NaN,
    False,
    BoolToStr( TGIS_Hydrology.FLOWDIR_RESOLVE_FLAT),
    ''
  ) ;
end;

procedure T_Pipeline_HydrologyFlowDirection.Execute;
var
  src_dem      : TGIS_LayerPixel ;
  dst          : TGIS_LayerPixel ;
  ext          : TGIS_Extent ;
  resolve_flat : Boolean ;
  hydro_tool   : TGIS_Hydrology ;
begin
  src_dem := ParamAsLayerPixel( PARAM_SOURCE_DEM ) ;
  dst := ParamAsLayerPixel( GIS_PIPELINE_PARAM_DESTINATION ) ;
  ext := ParamAsExtent( GIS_PIPELINE_PARAM_EXTENT, src_dem.Extent ) ;
  resolve_flat := ParamAsBool( PARAM_RESOLVE_FLAT, TGIS_Hydrology.FILL_APPLY_SLOPE) ;

  hydro_tool := TGIS_Hydrology.Create ;
  try
    hydro_tool.busyEventManager.UseProgressThreshold := False ;
    {$IFDEF CLR}
      hydro_tool.BusyEvent += DoBusyEvent ;
    {$ELSE}
      hydro_tool.BusyEvent := {$IFDEF OXYGENE}@{$ENDIF}DoBusyEvent ;
    {$ENDIF}
    hydro_tool.FlowDirection( src_dem, ext, dst, resolve_flat ) ;
  finally
    FreeObject( hydro_tool ) ;
  end ;

  inherited ;
end;
{$ENDREGION}

{$REGION 'Pipeline - ResolveFlat'}
type
  /// <summary>
  ///   Implementation of the 'Hydrology.ResolveFlat' pipeline command.
  /// </summary>
  T_Pipeline_HydrologyResolveFlat = class( T_Pipeline_HydrologyAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

procedure T_Pipeline_HydrologyResolveFlat.Initialize ;
begin
  defineParam(
    PARAM_SOURCE_DEM,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    '',
    ''
  ) ;
  defineParam(
    PARAM_SOURCE_FLOW_DIR,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    '',
    ''
  ) ;

  inherited ;
end;

procedure T_Pipeline_HydrologyResolveFlat.Execute;
var
  src_dem     : TGIS_LayerPixel ;
  src_flowdir : TGIS_LayerPixel ;
  dst         : TGIS_LayerPixel ;
  ext         : TGIS_Extent ;
  hydro_tool  : TGIS_Hydrology ;
begin
  src_dem := ParamAsLayerPixel( GIS_PIPELINE_PARAM_SOURCE ) ;
  src_flowdir := ParamAsLayerPixel( GIS_PIPELINE_PARAM_SOURCE ) ;
  dst := ParamAsLayerPixel( GIS_PIPELINE_PARAM_DESTINATION ) ;
  ext := ParamAsExtent( GIS_PIPELINE_PARAM_EXTENT, src_dem.Extent ) ;

  hydro_tool := TGIS_Hydrology.Create ;
  try
    hydro_tool.busyEventManager.UseProgressThreshold := False ;
    {$IFDEF CLR}
      hydro_tool.BusyEvent += DoBusyEvent ;
    {$ELSE}
      hydro_tool.BusyEvent := {$IFDEF OXYGENE}@{$ENDIF}DoBusyEvent ;
    {$ENDIF}
    hydro_tool.ResolveFlat( src_dem, src_flowdir, ext, dst ) ;
  finally
    FreeObject( hydro_tool ) ;
  end ;

  if not assigned( dst ) and
     ParamAsBool( GIS_PIPELINE_PARAM_SAVE, GIS_PIPELINE_DEFAULT_SAVE )
  then
    dst.SaveAll ;

  inherited ;
end;
{$ENDREGION}

{$REGION 'Pipeline - Sink'}
type
  /// <summary>
  ///   Implementation of the 'Hydrology.Sink' pipeline command.
  /// </summary>
  T_Pipeline_HydrologySink = class( T_Pipeline_HydrologyAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

procedure T_Pipeline_HydrologySink.Initialize ;
begin
  defineParam(
    PARAM_SOURCE_DEM,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    '',
    ''
  ) ;

  inherited ;
end;

procedure T_Pipeline_HydrologySink.Execute;
var
  src_dem    : TGIS_LayerPixel ;
  dst        : TGIS_LayerPixel ;
  ext        : TGIS_Extent ;
  hydro_tool : TGIS_Hydrology ;
begin
  src_dem := ParamAsLayerPixel( PARAM_SOURCE_DEM ) ;
  dst := ParamAsLayerPixel( GIS_PIPELINE_PARAM_DESTINATION ) ;
  ext := ParamAsExtent( GIS_PIPELINE_PARAM_EXTENT, src_dem.Extent ) ;

  hydro_tool := TGIS_Hydrology.Create ;
  try
    hydro_tool.busyEventManager.UseProgressThreshold := False ;
    {$IFDEF CLR}
      hydro_tool.BusyEvent += DoBusyEvent ;
    {$ELSE}
      hydro_tool.BusyEvent := {$IFDEF OXYGENE}@{$ENDIF}DoBusyEvent ;
    {$ENDIF}
    hydro_tool.Sink( src_dem, ext, dst ) ;
  finally
    FreeObject( hydro_tool ) ;
  end ;

  inherited ;
end;
{$ENDREGION}

{$REGION 'Pipeline - StreamOrder'}
type
  /// <summary>
  ///   Implementation of the 'Hydrology.StreamOrder' pipeline command.
  /// </summary>
  T_Pipeline_HydrologyStreamOrder = class( T_Pipeline_HydrologyAbstract )
    const
      PARAM_METHOD = 'Method' ;

    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

procedure T_Pipeline_HydrologyStreamOrder.Initialize ;
begin
  defineParam(
    PARAM_SOURCE_FLOW_DIR,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    '',
    ''
  ) ;
  defineParam(
    PARAM_SOURCE_FLOW_ACC,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    '',
    ''
  ) ;

  inherited ;

  defineParam(
    PARAM_METHOD,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    False,
    'Strahler',
    '!Hack|Shreve|Strahler|Topological'
  ) ;
  defineParam(
    PARAM_THRESHOLD,
    TGIS_PipelineParameterType.Int,
    -1,
    NaN,
    False,
    IntToStr( TGIS_Hydrology.STREAM_ORDER_THRESHOLD ),
    ''
  ) ;
end;

procedure T_Pipeline_HydrologyStreamOrder.Execute;
var
  src_flowdir : TGIS_LayerPixel ;
  src_flowacc : TGIS_LayerPixel ;
  dst         : TGIS_LayerPixel ;
  ext         : TGIS_Extent ;
  threshold   : Integer ;
  method_str  : String ;
  method_so   : TGIS_HydrologyStreamOrderMethod ;
  hydro_tool  : TGIS_Hydrology ;
begin
  src_flowdir := ParamAsLayerPixel( PARAM_SOURCE_FLOW_DIR ) ;
  src_flowacc := ParamAsLayerPixel( PARAM_SOURCE_FLOW_ACC ) ;
  dst := ParamAsLayerPixel( GIS_PIPELINE_PARAM_DESTINATION ) ;
  ext := ParamAsExtent( GIS_PIPELINE_PARAM_EXTENT, src_flowdir.Extent ) ;
  threshold := ParamAsInt( PARAM_THRESHOLD, TGIS_Hydrology.STREAM_ORDER_THRESHOLD ) ;
  method_str := ParamAsString( PARAM_METHOD, 'Strahler' ) ;

  hydro_tool := TGIS_Hydrology.Create ;
  try
    if method_str = 'Hack' then
      method_so := TGIS_HydrologyStreamOrderMethod.Hack
    else if method_str = 'Shreve' then
      method_so := TGIS_HydrologyStreamOrderMethod.Shreve
    else if method_str = 'Strahler' then
      method_so := TGIS_HydrologyStreamOrderMethod.Strahler
    else if method_str = 'Topological' then
      method_so := TGIS_HydrologyStreamOrderMethod.Topological
    else
      method_so := TGIS_HydrologyStreamOrderMethod.Strahler ;

    hydro_tool.busyEventManager.UseProgressThreshold := False ;
    {$IFDEF CLR}
      hydro_tool.BusyEvent += DoBusyEvent ;
    {$ELSE}
      hydro_tool.BusyEvent := {$IFDEF OXYGENE}@{$ENDIF}DoBusyEvent ;
    {$ENDIF}
    hydro_tool.StreamOrder( src_flowdir, src_flowacc, ext, dst, method_so, threshold ) ;
  finally
    FreeObject( hydro_tool ) ;
  end ;

  inherited ;
end;
{$ENDREGION}

{$REGION 'Pipeline - StreamToPolyline'}
type
  /// <summary>
  ///   Implementation of the 'Hydrology.StreamToPolyline' pipeline command.
  /// </summary>
  T_Pipeline_HydrologyStreamToPolyline = class( T_Pipeline_HydrologyAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

procedure T_Pipeline_HydrologyStreamToPolyline.Initialize ;
begin
  defineParam(
    PARAM_SOURCE_FLOW_DIR,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    '',
    ''
  ) ;
  defineParam(
    PARAM_SOURCE_STREAM,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    '',
    ''
  ) ;

  inherited ;

  defineParam(
    GIS_PIPELINE_PARAM_FIELD,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    False,
    '',
    ''
  ) ;
  defineParam(
    PARAM_THRESHOLD,
    TGIS_PipelineParameterType.Int,
    -1,
    NaN,
    False,
    IntToStr( TGIS_Hydrology.STREAM_ORDER_THRESHOLD ),
    ''
  ) ;
end;

procedure T_Pipeline_HydrologyStreamToPolyline.Execute;
var
  src_flowdir : TGIS_LayerPixel ;
  src_stream  : TGIS_LayerPixel ;
  dst         : TGIS_LayerVector ;
  ext         : TGIS_Extent ;
  field       : String ;
  threshold   : Integer ;
  hydro_tool  : TGIS_Hydrology ;
begin
  src_flowdir := ParamAsLayerPixel( PARAM_SOURCE_FLOW_DIR ) ;
  src_stream := ParamAsLayerPixel( PARAM_SOURCE_STREAM ) ;
  dst := ParamAsLayerVector( GIS_PIPELINE_PARAM_DESTINATION ) ;
  ext := ParamAsExtent( GIS_PIPELINE_PARAM_EXTENT, src_flowdir.Extent ) ;
  threshold := ParamAsInt( PARAM_THRESHOLD, TGIS_Hydrology.STREAM_ORDER_THRESHOLD ) ;
  field := ParamAsString( GIS_PIPELINE_PARAM_FIELD, '' ) ;

  hydro_tool := TGIS_Hydrology.Create ;
  try
    hydro_tool.busyEventManager.UseProgressThreshold := False ;
    {$IFDEF CLR}
      hydro_tool.BusyEvent += DoBusyEvent ;
    {$ELSE}
      hydro_tool.BusyEvent := {$IFDEF OXYGENE}@{$ENDIF}DoBusyEvent ;
    {$ENDIF}
    hydro_tool.StreamToPolyline( src_flowdir, src_stream, ext, dst, field, threshold ) ;
  finally
    FreeObject( hydro_tool ) ;
  end ;

  inherited ;
end;
{$ENDREGION}

{$REGION 'Pipeline - Watershed'}
type
  /// <summary>
  ///   Implementation of the 'Hydrology.Watershed' pipeline command.
  /// </summary>
  T_Pipeline_HydrologyWatershed = class( T_Pipeline_HydrologyAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

procedure T_Pipeline_HydrologyWatershed.Initialize ;
begin
  defineParam(
    PARAM_SOURCE_FLOW_DIR,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    '',
    ''
  ) ;
  defineParam(
    PARAM_SOURCE_OUTLETS,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    '',
    ''
  ) ;

  inherited ;

  defineParam(
    GIS_PIPELINE_PARAM_FIELD,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    False,
    '',
    ''
  ) ;
end;

procedure T_Pipeline_HydrologyWatershed.Execute;
var
  src_flowdir : TGIS_LayerPixel ;
  src_outlets : TGIS_Layer ;
  dst         : TGIS_LayerPixel ;
  ext         : TGIS_Extent ;
  field       : String ;
  hydro_tool  : TGIS_Hydrology ;
begin
  src_flowdir := ParamAsLayerPixel( PARAM_SOURCE_FLOW_DIR ) ;
  // outlets may be a vector layer or a grid
  src_outlets := ParamAsLayer( PARAM_SOURCE_OUTLETS ) ;
  dst := ParamAsLayerPixel( GIS_PIPELINE_PARAM_DESTINATION ) ;
  ext := ParamAsExtent( GIS_PIPELINE_PARAM_EXTENT, src_flowdir.Extent ) ;
  field := ParamAsString( GIS_PIPELINE_PARAM_FIELD, '' ) ;

  hydro_tool := TGIS_Hydrology.Create ;
  try
    hydro_tool.busyEventManager.UseProgressThreshold := False ;
    {$IFDEF CLR}
      hydro_tool.BusyEvent += DoBusyEvent ;
    {$ELSE}
      hydro_tool.BusyEvent := {$IFDEF OXYGENE}@{$ENDIF}DoBusyEvent ;
    {$ENDIF}
    if src_outlets.IsVector then
      hydro_tool.Watershed( src_flowdir, TGIS_LayerVector( src_outlets ), field, ext, dst )
    else if src_outlets.IsGrid then
      hydro_tool.Watershed( src_flowdir, TGIS_LayerPixel( src_outlets ), ext, dst ) ;
  finally
    FreeObject( hydro_tool ) ;
  end ;

  inherited ;
end;
{$ENDREGION}

{$REGION 'Unit_GisHydrology'}
class procedure Unit_GisHydrology.SelfRegisterPipeline ;
begin
  RegisterPipeline(
    'Hydrology.Basin',
    T_Pipeline_HydrologyBasin
  ) ;

  RegisterPipeline(
    'Hydrology.Fill',
    T_Pipeline_HydrologyFill
  ) ;

  RegisterPipeline(
    'Hydrology.FlowAccumulation',
    T_Pipeline_HydrologyFlowAccumulation
  ) ;

  RegisterPipeline(
    'Hydrology.FlowDirection',
    T_Pipeline_HydrologyFlowDirection
  ) ;

  RegisterPipeline(
    'Hydrology.ResolveFlat',
    T_Pipeline_HydrologyResolveFlat
  ) ;

  RegisterPipeline(
    'Hydrology.Sink',
    T_Pipeline_HydrologySink
  ) ;
//
  RegisterPipeline(
    'Hydrology.StreamOrder',
    T_Pipeline_HydrologyStreamOrder
  ) ;

  RegisterPipeline(
    'Hydrology.StreamToPolyline',
    T_Pipeline_HydrologyStreamToPolyline
  ) ;

  RegisterPipeline(
    'Hydrology.Watershed',
    T_Pipeline_HydrologyWatershed
  ) ;
end;
{$ENDREGION}

// Unit tests //

{$REGION 'TestUnitHydrology'}
{$IFDEF GIS_UNIT_TESTS}
procedure TestUnitHydrology.TestHUtilsCellOnlyFlowsToOneCell ;
const
  TEST_VALUES : array [0..7] of Integer = ( -129, -128, -127 -1, 0, 3, 127, 129, 255 ) ;
var
  dir : Integer;
begin
  SET_TEST_NAME( 'TestUnitHydrology.TestHUtilsCellOnlyFlowsToOneCell' ) ;

  // True
  for dir := low( DIRECTIONS ) to high( DIRECTIONS ) do
    Assert.IsTrue(
      T_HUtils.CellOnlyFlowsToOneCell( DIRECTIONS[dir] ),
      Format( '#00010 - Incorrectly interpreted input value: %d', [DIRECTIONS[dir]] )
    ) ;

  // False
  for dir := low( TEST_VALUES ) to high( TEST_VALUES ) do
    Assert.IsTrue(
      not T_HUtils.CellOnlyFlowsToOneCell( TEST_VALUES[dir] ),
      Format( '#01010 - Incorrectly interpreted input value: %d', [TEST_VALUES[dir]] )
    ) ;
end;

procedure TestUnitHydrology.TestHUtilsDirectionToIndex ;
const
  TEST_VALUES : array [0..7] of Integer = ( -129, -128, -127 -1, 0, 3, 127, 129, 255 ) ;
var
  dir : Integer;
begin
  SET_TEST_NAME( 'TestUnitHydrology.TestHUtilsDirectionToIndex' ) ;

  for dir := low( DIRECTIONS ) to high( DIRECTIONS ) do
    Assert.AreEqual(
      dir,
      T_HUtils.DirectionToIndex( DIRECTIONS[dir] ),
      Format( '#00020 - Incorrectly converted input value: %d', [DIRECTIONS[dir]] )
     ) ;

  // Other values
  for dir := low( TEST_VALUES ) to high( TEST_VALUES ) do
    Assert.AreEqual(
      -1,
      T_HUtils.DirectionToIndex( TEST_VALUES[dir] ),
      Format( '#00020 - Incorrectly converted input value: %d', [TEST_VALUES[dir]] )
     ) ;
end;

procedure TestUnitHydrology.TestHUtilsGetCellSize ;
var
  lp        : TGIS_LayerPixel ;
  cell_size : TGIS_Point ;
begin
  SET_TEST_NAME( 'TestUnitHydrology.TestHUtilsGetCellSize' ) ;

  lp := TGIS_LayerPixel.Create ;
  try
    lp.Build( True, nil, GisExtent(-180, -90, 180, 90),180, 180 ) ;

    cell_size := T_HUtils.GetCellSize( lp ) ;
  finally
    FreeObject( lp ) ;
  end;

  Assert.AreEqual(
    2,
    cell_size.X,
    GIS_DOUBLE_RESOLUTION,
    '#00030 - Incorrectly calculated cell size (X)'
  ) ;
  Assert.AreEqual(
    1,
    cell_size.Y,
    GIS_DOUBLE_RESOLUTION,
    '#01030 - Incorrectly calculated cell size (Y)'
  ) ;
end;

procedure TestUnitHydrology.TestHUtilsGetNeighbor ;
var
  rn, cn : Integer ;
begin
  SET_TEST_NAME( 'TestUnitHydrology.TestHUtilsGetNeighbor' ) ;

  // E
  T_HUtils.GetNeighbor( 1, 1, 0, rn, cn ) ;
  Assert.AreEqual( 1, rn, '#00040 - Incorrect row' ) ;
  Assert.AreEqual( 2, cn, '#00040 - Incorrect column' ) ;

  // SE
  T_HUtils.GetNeighbor( 1, 1, 1, rn, cn ) ;
  Assert.AreEqual( 2, rn, '#01040 - Incorrect row' ) ;
  Assert.AreEqual( 2, cn, '#01040 - Incorrect column' ) ;

  // S
  T_HUtils.GetNeighbor( 1, 1, 2, rn, cn ) ;
  Assert.AreEqual( 2, rn, '#02040 - Incorrect row' ) ;
  Assert.AreEqual( 1, cn, '#02040 - Incorrect column' ) ;

  // SW
  T_HUtils.GetNeighbor( 1, 1, 3, rn, cn ) ;
  Assert.AreEqual( 2, rn, '#03040 - Incorrect row' ) ;
  Assert.AreEqual( 0, cn, '#03040 - Incorrect column' ) ;

  // W
  T_HUtils.GetNeighbor( 1, 1, 4, rn, cn ) ;
  Assert.AreEqual( 1, rn, '#04040 - Incorrect row' ) ;
  Assert.AreEqual( 0, cn, '#04040 - Incorrect column' ) ;

  // NW
  T_HUtils.GetNeighbor( 1, 1, 5, rn, cn ) ;
  Assert.AreEqual( 0, rn, '#05040 - Incorrect row' ) ;
  Assert.AreEqual( 0, cn, '#04040 - Incorrect column' ) ;

  // N
  T_HUtils.GetNeighbor( 1, 1, 5, rn, cn ) ;
  Assert.AreEqual( 0, rn, '#05040 - Incorrect row' ) ;
  Assert.AreEqual( 0, cn, '#04040 - Incorrect column' ) ;

  // N
  T_HUtils.GetNeighbor( 1, 1, 6, rn, cn ) ;
  Assert.AreEqual( 0, rn, '#06040 - Incorrect row' ) ;
  Assert.AreEqual( 1, cn, '#06040 - Incorrect column' ) ;

  // NE
  T_HUtils.GetNeighbor( 1, 1, 7, rn, cn ) ;
  Assert.AreEqual( 0, rn, '#07040 - Incorrect row' ) ;
  Assert.AreEqual( 2, cn, '#07040 - Incorrect column' ) ;
end;

procedure TestUnitHydrology.TestHUtilsIndexToDirection ;
const
  TEST_VALUES : array [0..2] of Integer = ( -1, 8, 9 ) ;
var
  dir : Integer;
begin
  SET_TEST_NAME( 'TestUnitHydrology.TestHUtilsIndexToDirection' ) ;

  for dir := low( DIRECTIONS ) to high( DIRECTIONS ) do
    Assert.AreEqual(
      DIRECTIONS[dir],
      T_HUtils.IndexToDirection( dir ),
      Format( '#00080 - Incorrectly converted input value: %d', [dir] )
     ) ;

  // Other values
  for dir := low( TEST_VALUES ) to high( TEST_VALUES ) do
    Assert.AreEqual(
      -1,
      T_HUtils.IndexToDirection( TEST_VALUES[dir] ),
      Format( '#01080 - Incorrectly converted input value: %d', [TEST_VALUES[dir]] )
     ) ;
end;

procedure TestUnitHydrology.TestHUtilsQueueIsEmpty;
var
  q_point : TQueue<TPoint> ;
begin
  SET_TEST_NAME( 'TestUnitHydrology.TestHUtilsQueueIsEmpty' ) ;

  q_point := TQueue<TPoint>.Create ;
  try
    Assert.IsTrue( T_HUtils.QueueIsEmpty( q_point ), '#00090 - Queue should be empty' ) ;

    q_point.Enqueue( Point( 1, 1 ) );
    Assert.IsTrue( not T_HUtils.QueueIsEmpty( q_point ), '#01090 - Queue should not be empty' ) ;

    q_point.Dequeue;
    Assert.IsTrue( T_HUtils.QueueIsEmpty( q_point ), '#02090 - Queue should be empty' ) ;
  finally
    FreeObject( q_point) ;
  end ;
end;

procedure TestUnitHydrology.TestHUtilsReverseDirection ;
var
  msg : String ;
begin
  SET_TEST_NAME( 'TestUnitHydrology.TestHUtilsReverseDirection' ) ;

  msg := '#00100 - Incorrect reverse direction' ;
  Assert.AreEqual(  16, T_HUtils.ReverseDirection(   1 ), msg ) ;
  Assert.AreEqual(  32, T_HUtils.ReverseDirection(   2 ), msg ) ;
  Assert.AreEqual(  64, T_HUtils.ReverseDirection(   4 ), msg ) ;
  Assert.AreEqual( 128, T_HUtils.ReverseDirection(   8 ), msg ) ;
  Assert.AreEqual(   1, T_HUtils.ReverseDirection(  16 ), msg ) ;
  Assert.AreEqual(   2, T_HUtils.ReverseDirection(  32 ), msg ) ;
  Assert.AreEqual(   4, T_HUtils.ReverseDirection(  64 ), msg ) ;
  Assert.AreEqual(   8, T_HUtils.ReverseDirection( 128 ), msg ) ;
  // wrong
  Assert.AreEqual(  -1, T_HUtils.ReverseDirection(  -1 ), msg ) ;
  Assert.AreEqual(  -1, T_HUtils.ReverseDirection(   0 ), msg ) ;
  Assert.AreEqual(  -1, T_HUtils.ReverseDirection(   3 ), msg ) ;
  Assert.AreEqual(  -1, T_HUtils.ReverseDirection(   129 ), msg ) ;
end;

procedure TestUnitHydrology.TestPixelLockTiler ;
var
  lp      : TGIS_LayerPixel ;
  lock    : TGIS_LayerPixelLock ;
  r, c    : Integer ;
  tiler   : T_PixelLockTiler ;
  size    : Integer ;
  overlap : Integer ;
  tile    : TRect ;
  rct     : TRect ;
  msg     : String ;
begin
  SET_TEST_NAME( 'TestUnitHydrology.TestPixelLockTiler' ) ;

  lp := TGIS_LayerPixel.Create ;
  try
    lp.Build( True, nil, GisExtent( 0, 0, 35, 25 ), 35, 25 ) ;

    // fill with zeros
    lock := lp.LockPixels( lp.Extent, lp.CS, True ) ;
    try
      for c := lock.Bounds.Left to lock.Bounds.Right do
        for r := lock.Bounds.Top to lock.Bounds.Bottom do
          lock.Grid[r,c] := 0 ;
    finally
      lp.UnlockPixels( lock ) ;
    end ;

    size := 10 ;
    overlap := 1 ;
    tiler := T_PixelLockTiler.Create( lp, lp.Extent, size, overlap ) ;
    try
      while tiler.Next do begin
        tile := tiler.CurrentRect ;

        lock := lp.LockPixels( tile, True ) ;
        try
          for c := lock.Bounds.Left to lock.Bounds.Right do
            for r := lock.Bounds.Top to lock.Bounds.Bottom do
              lock.Grid[r,c] := lock.Grid[r,c] + 1 ;
        finally
          lp.UnlockPixels( lock ) ;
        end ;
      end ;
    finally
      FreeObject( tiler ) ;
    end ;

    lock := lp.LockPixels( lp.Extent, lp.CS, True ) ;
    try
      rct := lock.Bounds ;
      // corners must equal 1
      msg := '#00110 - Incorrect value in at the grid corner' ;
      Assert.AreEqual( 1, lock.Grid[rct.Top, rct.Left], msg ) ;
      Assert.AreEqual( 1, lock.Grid[rct.Top, rct.Right], msg ) ;
      Assert.AreEqual( 1, lock.Grid[rct.Bottom, rct.Right], msg ) ;
      Assert.AreEqual( 1, lock.Grid[rct.Bottom, rct.Left], msg ) ;

      // 2 edge with overlap must equal 2
      msg := '#01110 - Incorrect value at the tile edge ' ;
      Assert.AreEqual( 2, lock.Grid[0, 8], msg ) ;
      Assert.AreEqual( 2, lock.Grid[0, 9], msg ) ;
      Assert.AreEqual( 2, lock.Grid[0, 16], msg ) ;
      Assert.AreEqual( 2, lock.Grid[0, 17], msg ) ;

      Assert.AreEqual( 2, lock.Grid[8, 0], msg ) ;
      Assert.AreEqual( 2, lock.Grid[9, 0], msg ) ;
      Assert.AreEqual( 2, lock.Grid[16, 0], msg ) ;
      Assert.AreEqual( 2, lock.Grid[17, 0], msg ) ;

      // 4 edge with overlap must equal 4
      msg := '#02110 - Incorrect value at the tile corner' ;
      Assert.AreEqual( 4, lock.Grid[8, 8], msg ) ;
      Assert.AreEqual( 4, lock.Grid[9, 9], msg ) ;
      Assert.AreEqual( 4, lock.Grid[8, 9], msg ) ;
      Assert.AreEqual( 4, lock.Grid[9, 8], msg ) ;
    finally
      lp.UnlockPixels( lock ) ;
    end ;
  finally
    FreeObject( lp ) ;
  end ;
end;

procedure TestUnitHydrology.TestPixelLockAdapter ;
begin
  SET_TEST_NAME( 'TestUnitHydrology.TestPixelLockAdapter' ) ;

  //? will be implemented
end;
{$ENDIF}
{$ENDREGION}

{$REGION 'initialization'}
{$IFNDEF OXYGENE}
  initialization
    {$IFDEF DCC}
      Unit_GisHydrology.SelfRegisterPipeline ;
    {$ENDIF}

    {$IFDEF GIS_UNIT_TESTS}
      TestUnitHydrology.Register ;
    {$ENDIF}
{$ENDIF}
{$ENDREGION}

end.
