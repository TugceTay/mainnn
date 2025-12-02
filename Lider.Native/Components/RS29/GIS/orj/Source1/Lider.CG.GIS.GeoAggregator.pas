//==============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.85.0.33382-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// 
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//==============================================================================
{
   Provides the following dynamic aggregation methods for vector layers:
  * Binning - Flat-topped Hexagons
  * Binning - Pointy-topped hexagons
  * Binning - Squares
  * Clustering - Flat-topped Hexagonal grid
  * Clustering - Pointy-topped Hexagonal grid
  * Clustering - Moving Average
  * Clustering - Square Grid
  * Shape reduction
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoAggregator;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoAggregator"'}
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

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  {$IFDEF CLR}
    System.Collections.Generic,
    TatukGIS.RTL
  {$ENDIF}
  {$IFDEF DCC}
    System.Generics.Collections,
    System.Types,

    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoParams,
    Lider.CG.GIS.GeoTypes
  {$ENDIF}
  {$IFDEF JAVA}
    remobjects.elements.rtl.*,
    tatukgis.rtl
  {$ENDIF}
  {$IFDEF ISLAND}
    remobjects.elements.rtl.*,
    TatukGIS.RTL ;
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
  // Initialization section handler
  GisAggregator = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  /// <summary>
  ///   Extension of base class TGIS_DynamicAggregatorAbstract for
  ///   TGIS_LayerVector.DynamicAggregator implementation.
  /// </summary>
  TGIS_AggregatorAbstract
    = {$IFDEF OXYGENE} public abstract {$ENDIF}
    class( TGIS_DynamicAggregatorAbstract )

    private const
      DEFAULT_RENDERER_COLOR_TXT  = 'ARGB:FFC0C0C0' ;  // gray
      DEFAULT_START_COLOR_TXT     = 'ARGB:FFFFFF00' ;  // yellow
      DEFAULT_END_COLOR_TXT       = 'ARGB:FFFF0000' ;  // red
      DEFAULT_LABEL_COLOR         = 'ARGB:FFFFFFFF' ;
      DEFAULT_START_SIZE_TXT      = 'SIZE:10 pt' ;
      DEFAULT_END_SIZE_TXT        = 'SIZE:50 pt' ;
      DEFAULT_LABEL_COUNT         = True ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      isLayerSupported            : Boolean ;
      radiusInternal              : Double ;
      useLinearUnits              : Boolean ;
      showLabel                   : Boolean ;
      oldRadius                   : Integer ;
      oldThreshold                : Integer ;

    private
      // set Params which are common for all aggregator types
      procedure setUpCommonParams ;

      // loop over aggregator items;
      // add and hide assigned shapes;
      // get minimum and maximum aggregator item count
      function  addHideShapesAndGetMinMax
                                  ( const _agg_items : TObjectList< TObject > ;
                                    const _extent    : TGIS_Extent ;
                                    const _offset    : TPoint ;
                                    const _cs        : TGIS_CSCoordinateSystem
                                  ) : TPoint ;

      function projectShape       ( const _shp       : TGIS_Shape ;
                                    const _viewer_cs : TGIS_CSCoordinateSystem ;
                                    const _layer_cs  : TGIS_CSCoordinateSystem
                                  ) : TGIS_Shape ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      // prepare class internal fields
      procedure doUpdate          ; virtual ;

      // update radius-dependent fields
      procedure doUpdateRadius    ; virtual ;

      // update threshold-dependent fields
      procedure doUpdateThreshold ; virtual ;

      procedure updateMinMaxVal   ( const _agg_min_max : TPoint ) ; virtual ;

      // find aggregator item to which shape will be assigned;
      // must be implemented in derived classes
      procedure assignShapeToAggregatorItem
                                  ( const _shp       : TGIS_Shape ;
                                    const _agg_items : TObjectList< TObject > ;
                                    const _cols_rows : TPoint ;
                                    const _offset    : TPoint ;
                                    const _cs        : TGIS_CSCoordinateSystem
                                  ) ; virtual ; abstract ;

      // if required, initialize aggregator items, i.e. grid;
      // must be implemented in derived classes
      procedure initializeAggregatorItems
                                  ( const _agg_items : TObjectList< TObject > ;
                                    const _cols_rows : TPoint ;
                                    const _offset    : TPoint
                                  ) ; virtual ; abstract ;

      // set Params that depend on the aggregator type
      procedure setUpAggregatorTypeSpecificSections ; virtual ; abstract ;

      // calculate a grid size for grid-like aggregators
      function  calculateColsRows ( const _width     : Double ;
                                    const _height    : Double
                                  ) : TPoint ; virtual ; abstract ;

      // create a shape representing the specific aggregator,
      // i.e. point, rectangle, hexagon
      function  createAggregatedShapeInMapCS
                                  ( const _agg_item  : TObject ;
                                    const _offset    : TPoint
                                  ) : TGIS_Shape ; virtual ; abstract ;

      // determine if Aggregator need to hide shapes ;
      // True for Clustering, False for Binning and ShapeReduction
      function  doHideShape       : Boolean ; virtual ; abstract ;

      // when working in linear units extent must be extended
      // to provide fixed grid lines
      function  prepareExtentForLinearUnits
                                  ( const _ext_draw  : TGIS_Extent ;
                                    const _ext_view  : TGIS_Extent
                                  ) : TGIS_Extent ; virtual ; abstract ;

    public
      /// <inheritdoc/>
      constructor Create          ( const _layer     : TGIS_LayerVector
                                  ) ; override ;

    public
      /// <inheritdoc/>
      procedure Prepare           ( const _extent    : TGIS_Extent;
                                    const _query     : String
                                  ) ;  override ;

      /// <inheritdoc/>
      procedure SetUp             ; override ;

  end;

  /// <summary>
  ///   Base class for dynamic clustering.
  /// </summary>
  TGIS_AggregatorClusteringAbstract
    = {$IFDEF OXYGENE} public abstract {$ENDIF}
    class( TGIS_AggregatorAbstract )

    private const
      DEFAULT_CLUSTER_RADIUS_TXT             = 'SIZE:40 pt' ;
      HIDE_SHAPES_CLUSTERING                 = True  ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      procedure setUpAggregatorTypeSpecificSections ; override ;

      function  doHideShape                  : Boolean ; override ;

      function  createAggregatedShapeInMapCS ( const _agg_item : TObject ;
                                               const _offset   : TPoint
                                             ) : TGIS_Shape ; override ;

      function  createClusterPoint           ( const _ptg    : TGIS_Point
                                             ) : TGIS_Shape ;

    public
      /// <inheritdoc/>
      constructor Create                     ( const _layer  : TGIS_LayerVector
                                             ) ; override ;

  end;

  /// <summary>
  ///   This class implements the clustering method using a moving average
  ///   technique. This smart method creates clusters in non-grid positions.
  /// </summary>
  TGIS_AggregatorClusteringMovingAverage
    = {$IFDEF OXYGENE} public {$ENDIF}
    class( TGIS_AggregatorClusteringAbstract )

    private const
      DEFAULT_MOV_AVG_RADIUS_TXT  = 'SIZE:60 pt' ;
      DEFAULT_MOV_AVG_THRESHOLD   = 2 ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      procedure assignShapeToAggregatorItem
                                  ( const _shp       : TGIS_Shape ;
                                    const _agg_items : TObjectList< TObject > ;
                                    const _cols_rows : TPoint ;
                                    const _offset    : TPoint ;
                                    const _cs        : TGIS_CSCoordinateSystem
                                  ) ; override ;

      procedure initializeAggregatorItems
                                  ( const _agg_items : TObjectList< TObject > ;
                                    const _cols_rows : TPoint ;
                                    const _offset    : TPoint
                                  ) ; override ;

      function  calculateColsRows ( const _width     : Double ;
                                    const _height    : Double
                                  ) : TPoint ; override ;

      function prepareExtentForLinearUnits(
                                    const _ext_draw  : TGIS_Extent ;
                                    const _ext_map   : TGIS_Extent
                                  ) : TGIS_Extent ; override ;
    protected
      /// <inheritdoc/>
      function  fget_Caption      : String ; override ;

    public
      /// <inheritdoc/>
      constructor Create          ( const _layer     : TGIS_LayerVector
                                  ) ; override ;

  end;

  // Forwarded types to use in clustering with hexagonal grid
  TGIS_AggregatorBinningHexagonAbstract = class ;
  TGIS_AggregatorBinningHexagonFlat     = class ;
  TGIS_AggregatorBinningHexagonPointy   = class ;
  TGIS_AggregatorBinningSquare          = class ;

  /// <summary>
  ///   Base class for dynamic clustering using hexagonal grid.
  /// </summary>
  TGIS_AggregatorClusteringHexagonalGridAbstract
    = {$IFDEF OXYGENE} public abstract {$ENDIF}
    class( TGIS_AggregatorClusteringAbstract )

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      binHex                      : TGIS_AggregatorBinningHexagonAbstract ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      procedure assignShapeToAggregatorItem
                                  ( const _shp       : TGIS_Shape ;
                                    const _agg_items : TObjectList< TObject > ;
                                    const _cols_rows : TPoint ;
                                    const _offset    : TPoint ;
                                    const _cs        : TGIS_CSCoordinateSystem
                                  ) ; override ;

      procedure doUpdateRadius    ; override ;

      procedure initializeAggregatorItems
                                  ( const _agg_items : TObjectList< TObject > ;
                                    const _cols_rows : TPoint ;
                                    const _offset    : TPoint
                                  ) ; override ;

      function  calculateColsRows ( const _width     : Double ;
                                    const _height    : Double
                                  ) : TPoint ; override ;

      function  createAggregatedShapeInMapCS
                                  ( const _agg_item  : TObject ;
                                    const _offset    : TPoint
                                  ) : TGIS_Shape ; override ;

      function  prepareExtentForLinearUnits
                                  ( const _ext_draw  : TGIS_Extent ;
                                    const _ext_map   : TGIS_Extent
                                  ) : TGIS_Extent ; override ;

    protected
      /// <inheritdoc/>
      procedure doDestroy         ; override ;

    public
      /// <inheritdoc/>
      constructor Create          ( const _layer     : TGIS_LayerVector
                                  ) ; override ;

  end;

  /// <summary>
  ///   This class implements the clustering method using a flat-topped
  ///   hexagonal grid.
  /// </summary>
  TGIS_AggregatorClusteringHexagonalGridFlat
    = {$IFDEF OXYGENE} public {$ENDIF}
    class( TGIS_AggregatorClusteringHexagonalGridAbstract )

    protected
      /// <inheritdoc/>
      function  fget_Caption : String ; override ;

    public
      /// <inheritdoc/>
      constructor Create     ( const _layer : TGIS_LayerVector
                             ) ; override ;

  end;

  /// <summary>
  ///   This class implements the clustering method using a pointy-topped
  ///   hexagonal grid.
  /// </summary>
  TGIS_AggregatorClusteringHexagonalGridPointy
    = {$IFDEF OXYGENE} public {$ENDIF}
    class( TGIS_AggregatorClusteringHexagonalGridAbstract )

    protected
      /// <inheritdoc/>
      function  fget_Caption : String ; override ;

    public
      /// <inheritdoc/>
      constructor Create     ( const _layer : TGIS_LayerVector
                             ) ; override ;

  end;

  /// <summary>
  ///   This class implements the clustering method using a square grid.
  /// </summary>
  TGIS_AggregatorClusteringSquareGrid
    = {$IFDEF OXYGENE} public {$ENDIF}
    class( TGIS_AggregatorClusteringAbstract )

    private
      binSq                       : TGIS_AggregatorBinningSquare ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      procedure assignShapeToAggregatorItem
                                  ( const _shp       : TGIS_Shape ;
                                    const _agg_items : TObjectList< TObject > ;
                                    const _cols_rows : TPoint ;
                                    const _offset    : TPoint ;
                                    const _cs        : TGIS_CSCoordinateSystem
                                  ) ; override ;

      procedure doUpdateRadius ; override ;

      procedure initializeAggregatorItems
                                  ( const _agg_items : TObjectList< TObject > ;
                                    const _cols_rows : TPoint ;
                                    const _offset    : TPoint
                                  ) ; override ;

      function  calculateColsRows ( const _width     : Double ;
                                    const _height    : Double
                                  ) : TPoint ; override ;

      function  prepareExtentForLinearUnits
                                  ( const _ext_draw  : TGIS_Extent ;
                                    const _ext_map   : TGIS_Extent
                                  ) : TGIS_Extent ; override ;

    protected
      /// <inheritdoc/>
      procedure doDestroy         ; override ;

      /// <inheritdoc/>
      function  fget_Caption      : String ; override ;

    public
      /// <inheritdoc/>
      constructor Create          ( const _layer     : TGIS_LayerVector
                                  ) ; override ;

  end;

  /// <summary>
  ///   This class implements the aggregation method based on shape reduction.
  ///   This method is designed to work with dense point data.
  /// </summary>
  TGIS_AggregatorShapeReduction
    = {$IFDEF OXYGENE} public {$ENDIF}
    class( TGIS_AggregatorClusteringAbstract )

    private const
      DEFAULT_SHP_RED_RADIUS_TXT  = 'SIZE:2 pt' ;
      DEFAULT_SHP_RED_LABEL_COUNT = False ;
      HIDE_SHAPES_SHP_RED         = False ;

    private
      binSq                       : TGIS_AggregatorBinningSquare ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      procedure assignShapeToAggregatorItem
                                  ( const _shp       : TGIS_Shape ;
                                    const _agg_items : TObjectList< TObject > ;
                                    const _cols_rows : TPoint ;
                                    const _offset    : TPoint ;
                                    const _cs        : TGIS_CSCoordinateSystem
                                  ) ; override ;

      procedure doUpdateRadius ; override ;

      procedure doUpdateThreshold ; override ;

      procedure initializeAggregatorItems
                                  ( const _agg_items : TObjectList< TObject > ;
                                    const _cols_rows : TPoint ;
                                    const _offset    : TPoint
                                  ) ; override ;

      procedure updateMinMaxVal   ( const _agg_min_max : TPoint ) ; override ;

      function  calculateColsRows ( const _width     : Double ;
                                    const _height    : Double
                                  ) : TPoint ; override ;

      procedure setUpAggregatorTypeSpecificSections  ; override ;

      function  doHideShape       : Boolean ; override ;

      function  prepareExtentForLinearUnits
                                  ( const _ext_draw  : TGIS_Extent ;
                                    const _ext_map   : TGIS_Extent
                                  ) : TGIS_Extent ; override ;

    protected
      /// <inheritdoc/>
      procedure doDestroy         ; override ;

      /// <inheritdoc/>
      function  fget_Caption      : String ; override ;

    public
      /// <inheritdoc/>
      constructor Create          ( const _layer     : TGIS_LayerVector
                                  ) ; override ;

  end;

  /// <summary>
  ///   Base class for dynamic binning.
  /// </summary>
  TGIS_AggregatorBinningAbstract
    = {$IFDEF OXYGENE} public abstract {$ENDIF}
    class( TGIS_AggregatorAbstract )

    private const
      DEFAULT_BIN_RADIUS_TXT = 'SIZE:20 pt' ;
      HIDE_SHAPES_BINNING    = False ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      procedure setUpAggregatorTypeSpecificSections ; override ;

      function  doHideShape : Boolean ; override ;

    public
      /// <inheritdoc/>
      constructor Create    ( const _layer : TGIS_LayerVector
                            ) ; override ;

  end;

  /// <summary>
  ///   This class implements the binning method using a square grid.
  /// </summary>
  TGIS_AggregatorBinningSquare
    = {$IFDEF OXYGENE} public {$ENDIF}
    class( TGIS_AggregatorBinningAbstract )

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      procedure assignShapeToAggregatorItem
                                  ( const _shp       : TGIS_Shape ;
                                    const _agg_items : TObjectList< TObject > ;
                                    const _cols_rows : TPoint ;
                                    const _offset    : TPoint ;
                                    const _cs        : TGIS_CSCoordinateSystem
                                  ) ; override ;

      procedure initializeAggregatorItems
                                  ( const _agg_items : TObjectList< TObject > ;
                                    const _cols_rows : TPoint ;
                                    const _offset    : TPoint
                                  ) ; override ;

      function  calculateColsRows ( const _width     : Double ;
                                    const _height    : Double
                                  ) : TPoint ; override ;

      function  createAggregatedShapeInMapCS
                                  ( const _agg_item  : TObject ;
                                    const _offset    : TPoint
                                  ) : TGIS_Shape ; override ;

      function  prepareExtentForLinearUnits
                                  ( const _ext_draw  : TGIS_Extent ;
                                    const _ext_map   : TGIS_Extent
                                  ) : TGIS_Extent ; override ;

    protected
      /// <inheritdoc/>
      function  fget_Caption      : String ; override ;

    public
      /// <inheritdoc/>
      constructor Create          ( const _layer     : TGIS_LayerVector
                                  ) ; override ;

  end;

  /// <summary>
  ///   Base class for dynamic binning using hexagonal grid.
  /// </summary>
  TGIS_AggregatorBinningHexagonAbstract
    = {$IFDEF OXYGENE} public abstract {$ENDIF}
    class( TGIS_AggregatorBinningAbstract )

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      // store hexagon dimensions to avoid frequent calculations
      hexHalfSize                 : Double ;
      hexSize                     : Double ;
      hexOneAndHalfSize           : Double ;
      hexDoubleSize               : Double ;
      hexTripleSize               : Double ;
      hexHeight                   : Double ;
      hexDoubleHeight             : Double ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      procedure assignShapeToAggregatorItem
                                  ( const _shp       : TGIS_Shape ;
                                    const _agg_items : TObjectList< TObject > ;
                                    const _cols_rows : TPoint ;
                                    const _offset    : TPoint ;
                                    const _cs        : TGIS_CSCoordinateSystem
                                  ) ; override ;

      procedure doUpdateRadius    ; override ;

      procedure initializeAggregatorItems
                                  ( const _agg_items : TObjectList< TObject > ;
                                    const _cols_rows : TPoint ;
                                    const _offset    : TPoint
                                  ) ; override ;

      function  createAggregatedShapeInMapCS
                                  ( const _agg_item  : TObject ;
                                    const _offset    : TPoint
                                  ) : TGIS_Shape ; override ;

      function  createHexagonPoints
                                  ( const _center    : TGIS_Point
                                  ) : TGIS_PointList ; virtual ; abstract ;

      function  findEvenIndex     ( const _ptg       : TGIS_Point ;
                                    const _cols_rows : TPoint ;
                                    const _offset    : TPoint
                                  ) : Integer ; virtual ; abstract ;

      function  findOddIndex      ( const _ptg       : TGIS_Point ;
                                    const _cols_rows : TPoint ;
                                    const _offset    : TPoint
                                  ) : Integer ; virtual ; abstract ;

      function  getHexagonCenter  ( const _x         : Integer ;
                                    const _y         : Integer ;
                                    const _offset    : TPoint
                                  ) : TGIS_Point ; virtual ; abstract ;

    public
      /// <inheritdoc/>
      constructor Create          ( const _layer     : TGIS_LayerVector
                                  ) ; override ;

  end;

  /// <summary>
  ///   This class implements the binning method using
  ///   a flat-topped hexagonal grid.
  /// </summary>
  TGIS_AggregatorBinningHexagonFlat
    = {$IFDEF OXYGENE} public {$ENDIF}
    class( TGIS_AggregatorBinningHexagonAbstract )

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      function calculateColsRows   ( const _width     : Double ;
                                     const _height    : Double
                                   ) : TPoint ; override ;

      function createHexagonPoints ( const _center    : TGIS_Point
                                   ) : TGIS_PointList ; override ;

      function findEvenIndex       ( const _ptg       : TGIS_Point ;
                                     const _cols_rows : TPoint ;
                                     const _offset    : TPoint
                                   ) : Integer ; override ;

      function findOddIndex        ( const _ptg       : TGIS_Point ;
                                     const _cols_rows : TPoint ;
                                     const _offset    : TPoint
                                   ) : Integer ; override ;

      function getHexagonCenter    ( const _x         : Integer ;
                                     const _y         : Integer ;
                                     const _offset    : TPoint
                                   ) : TGIS_Point ; override ;

      function prepareExtentForLinearUnits(
                                     const _ext_draw  : TGIS_Extent ;
                                     const _ext_map   : TGIS_Extent
                                   ) : TGIS_Extent ; override ;

    protected
      /// <inheritdoc/>
      function fget_Caption        : String ; override ;

    public
      /// <inheritdoc/>
      constructor Create           ( const _layer     : TGIS_LayerVector
                                   ) ; override ;

  end;

  /// <summary>
  ///   This class implements the binning method using
  ///   a pointy-topped hexagonal grid.
  /// </summary>
  TGIS_AggregatorBinningHexagonPointy
    = {$IFDEF OXYGENE} public {$ENDIF}
    class( TGIS_AggregatorBinningHexagonAbstract )

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      function calculateColsRows         ( const _width     : Double ;
                                           const _height    : Double
                                         ) : TPoint ; override ;

      function createHexagonPoints       ( const _center    : TGIS_Point
                                         ) : TGIS_PointList ; override ;

      function findEvenIndex             ( const _ptg       : TGIS_Point ;
                                           const _cols_rows : TPoint ;
                                           const _offset    : TPoint
                                         ) : Integer ; override ;

      function findOddIndex              ( const _ptg       : TGIS_Point ;
                                           const _cols_rows : TPoint ;
                                           const _offset    : TPoint
                                         ) : Integer ; override ;

      function getHexagonCenter          ( const _x         : Integer ;
                                           const _y         : Integer ;
                                           const _offset    : TPoint
                                          ) : TGIS_Point ; override ;

      function prepareExtentForLinearUnits( const _ext_draw : TGIS_Extent ;
                                            const _ext_map  : TGIS_Extent
                                          ) : TGIS_Extent ; override ;

    protected
      /// <inheritdoc/>
      function fget_Caption               : String ; override ;

    public
      /// <inheritdoc/>
      constructor Create                  ( const _layer    : TGIS_LayerVector
                                          ) ; override ;

   end;

  {$IFDEF GIS_UNIT_TESTS}
    // Unit test for types from implementation section
    [TestFixture]
    TestUnitAggregator = {$IFDEF OXYGENE} public {$ENDIF} class( TttkNDUnit )
      public
        [Test] procedure TestAggregatorItemOnlyCount ;
        [Test] procedure TestAggregatorItemOnlyFirst ;
        [Test] procedure TestAggregatorItemMovingAverage ;
    end;
  {$ENDIF}

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Math,
    System.SysUtils,
    System.SyncObjs,
    Lider.CG.GIS.GeoCsFactory,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTopology,
    Lider.CG.GIS.GeoUtils;
{$ENDIF}

type
  /// <summary>
  ///   Base class for elementary aggregation cell.
  /// </summary>
  T_AggregatorItemAbstract = class( TGIS_Object )
    private
      FPosition         : TGIS_Point ;
      FPositionOnScreen : TGIS_Point ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      uids              : TList< Int64 > ;
      viewer            : IGIS_Viewer ;

    private
      function  fget_Count      : Integer ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      procedure fset_Position   ( const _value : TGIS_Point
                                ) ; virtual ;

    protected
      /// <inheritdoc/>
      procedure doDestroy       ; override ;

    public
      /// <summary>
      ///   Create an instance
      /// </summary>
      constructor Create ;

    public
      /// <summary>
      ///   Add shape's localization to aggregator item.
      /// </summary>
      /// <param name="_ptg">
      ///   shape's localization, e.g. centroid
      /// </param>
      /// <param name="_uid">
      ///   shape's unique identity
      /// </param>
      /// <remarks>
      ///   Must be derived if custom behavior is needed.
      ///   By default, only Count is increased.
      /// </remarks>
      procedure Add             ( const _ptg : TGIS_Point ;
                                  const _uid : Int64
                                ) ; virtual ;

    public
      /// <summary>
      ///   Number of shapes assigned to aggregator item.
      /// </summary>
      property Count            : Integer
                                  read fget_Count ;

      /// <summary>
      ///   Current item's position on the map in layer's CS.
      /// </summary>
      /// <remarks>
      ///   Position setting might be implemented in Add method.
      /// </remarks>
      property Position         : TGIS_Point
                                  read FPosition
                                  write FPosition ;

      /// <summary>
      ///   Current item's position on the screen.
      /// </summary>
      property PositionOnScreen : TGIS_Point
                                  read FPositionOnScreen
                                  write FPositionOnScreen ;

  end;

  /// <summary>
  ///   This class represents aggregator item that uses moving average technique.
  /// </summary>
  T_AggregatorItemMovingAverage = class ( T_AggregatorItemAbstract )
    public
      /// <inheritdoc/>
      /// <remarks>
      ///   Adding new position to aggregator item, recalculates object position
      ///   using moving average.
      /// </remarks>
      procedure Add ( const _ptg : TGIS_Point ;
                      const _uid : Int64
                    ) ; override ;

  end;

  /// <summary>
  ///   This class represents aggregator item that remember only first position.
  /// </summary>
  T_AggregatorItemOnlyFirst = class ( T_AggregatorItemAbstract )
    public
      /// <inheritdoc/>
      /// <remarks>
      ///   Only first added object is stored; counter is off.
      /// </remarks>
      procedure Add ( const _ptg : TGIS_Point ;
                      const _uid : Int64
                    ) ; override ;

  end;

  /// <summary>
  ///   This class represents aggregator item that only counts objects.
  /// </summary>
  T_AggregatorItemOnlyCount = class ( T_AggregatorItemAbstract )

  end;

{$REGION 'T_AggregatorItemAbstract'}
procedure T_AggregatorItemAbstract.doDestroy ;
begin
  FreeObject( uids ) ;
  inherited ;
end;

constructor T_AggregatorItemAbstract.Create ;
begin
  uids := TList< Int64 >.Create ;
  FPosition := GisPoint( 0, 0 ) ;
  FPositionOnScreen := GisPoint( 0, 0 ) ;
end;

procedure T_AggregatorItemAbstract.Add(
  const _ptg : TGIS_Point ;
  const _uid : Int64
) ;
begin
  uids.Add( _uid ) ;
end;

function T_AggregatorItemAbstract.fget_Count : Integer ;
begin
  Result := uids.Count ;
end;

procedure T_AggregatorItemAbstract.fset_Position(
  const _value : TGIS_Point
) ;
begin
  FPosition := _TGIS_Point( _value ) ;
end;
{$ENDREGION}

{$REGION 'T_AggregatorItemMovingAverage'}
procedure T_AggregatorItemMovingAverage.Add(
  const _ptg : TGIS_Point ;
  const _uid : Int64
) ;
var
  x, y : Double ;
begin
  inherited ;

  if Count = 1 then begin
    Position := _TGIS_Point( _ptg ) ; // record copy
  end
  else begin
    x := Position.X ;
    y := Position.Y ;
    x := x + ( _ptg.X - x ) / Count ;
    y := y + ( _ptg.Y - y ) / Count ;
    Position := GisPoint( x, y ) ;
  end ;
end;
{$ENDREGION}

{$REGION 'T_AggregatorItemOnlyFirst'}
procedure T_AggregatorItemOnlyFirst.Add(
  const _ptg : TGIS_Point ;
  const _uid : Int64
) ;
begin
  if Count = 0 then begin
    inherited Add( _ptg, _uid ) ;
    Position := _ptg ;
  end ;
end;
{$ENDREGION}

{$REGION 'TGIS_AggregatorAbstract'}
constructor TGIS_AggregatorAbstract.Create(
  const _layer : TGIS_LayerVector
) ;
begin
  inherited Create ( _layer ) ;

  isLayerSupported := True ;
  showLabel := DEFAULT_LABEL_COUNT ;
  oldRadius := -1 ;
  oldThreshold := -1 ;
end;

procedure TGIS_AggregatorAbstract.SetUp;
begin
  if not isLayerSupported then
    exit ;

  setUpAggregatorTypeSpecificSections ;
  setUpCommonParams ;
  doUpdate ;
end;

procedure TGIS_AggregatorAbstract.setUpCommonParams ;
begin
  if useConfig then exit ;

  Layer.ParamsList.Selected := Layer.ParamsList.Count - 1 ;

  // renderer
  Layer.Params.Render.Expression := GIS_FIELD_AGGREGATED_VALUE ;
  Layer.Params.Render.Zones := 5 ;
  Layer.Params.Render.ColorDefaultAsText := DEFAULT_RENDERER_COLOR_TXT ;
  Layer.Params.Render.StartColorAsText := DEFAULT_START_COLOR_TXT ;
  Layer.Params.Render.StartSizeAsText := DEFAULT_START_SIZE_TXT ;
  Layer.Params.Render.EndColorAsText := DEFAULT_END_COLOR_TXT ;
  Layer.Params.Render.EndSizeAsText := DEFAULT_END_SIZE_TXT ;
  Layer.Params.Render.ZonesEx := 0 ;

  // labels
  if showLabel then begin
    Layer.Params.Labels.Value := Format( '{%s}', [GIS_FIELD_AGGREGATED_VALUE] ) ;
    Layer.Params.Labels.Position := GisGetLabelPosition( TGIS_LabelPosition.MiddleCenter ) ;
    Layer.Params.Labels.ColorAsText := DEFAULT_LABEL_COLOR ;
    Layer.Params.Labels.Visible := DEFAULT_LABEL_COUNT ;
  end ;
end;

procedure TGIS_AggregatorAbstract.doUpdate ;
begin
  if oldRadius <> Radius then begin
    doUpdateRadius ;
    oldRadius := Radius ;
  end ;

  if oldThreshold <> Threshold then begin
    doUpdateThreshold ;
    oldThreshold := Threshold ;
  end ;
end;

procedure TGIS_AggregatorAbstract.doUpdateRadius ;
begin
  // screen or map (linear) units
  if ( Pos( GIS_PARAMTXT_SIZE_PX, RadiusAsText ) > StringFirst ) or
     ( Pos( GIS_PARAMTXT_SIZE_DIP, RadiusAsText ) > StringFirst ) or
     ( Pos( GIS_PARAMTXT_SIZE_TWIPS, RadiusAsText ) > StringFirst ) or
     ( Pos( GIS_PARAMTXT_SIZE_PT, RadiusAsText ) > StringFirst ) then
  begin
    useLinearUnits := False ;
    if assigned( Layer.Viewer ) then
      radiusInternal := Layer.Viewer.Ref.TwipsToPixels( Radius )
    else
      radiusInternal := Abs( Radius ) ;
  end
  else begin
    useLinearUnits := True ;
    // fast conversion to meters
    radiusInternal := -0.001 * ( Radius + GIS_AUTOSIZE_SIZE ) ;
  end ;
end;

procedure TGIS_AggregatorAbstract.doUpdateThreshold ;
begin
  // safe inheritance
end;

procedure TGIS_AggregatorAbstract.Prepare(
  const _extent : TGIS_Extent;
  const _query  : String
) ;
const
  EPSG_EQUIDISTANT = 4087 ; // WGS 84 World Equidistant Cylindrical
var
  viewer             : IGIS_Viewer ;
  ext                : TGIS_Extent ;
  working_CS         : TGIS_CSCoordinateSystem ;
  working_ext        : TGIS_Extent ;
  working_viewer_ext : TGIS_Extent ;
  offset             : TPoint ;
  cols_rows          : TPoint ;
  rect_screen        : TRect ;
  agg_items          : TObjectList< TObject > ;
  {$IFDEF DCC}
    obj              : TGIS_Shape ;
  {$ENDIF}
  shp                : TGIS_Shape;
  agg_min_max        : TPoint ;
begin
  // some layers are not supported for now
  // e.g. 'ShapeReduction' works only for point layers
  if not isLayerSupported then
    exit ;

  // internal update always
  doUpdate ;
  if ( radiusInternal <= 0 ) then
    exit ;

  viewer := Layer.Viewer.Ref ;
  ext := _extent ;

  // prepare CS, extent, offset, and cols&rows
  if useLinearUnits then begin
    // use projected CS in the following order:
    // Layer -> world equidistant projection
    if ( Layer.CS is TGIS_CSProjectedCoordinateSystem )
    then begin
      // best projected CS for aggregation in linear units is layer CS
      // assume that the distortions are minimized
      working_CS := Layer.CS ;
    end
    else begin
      // change CS to equidistant projection to minimisee distance errors
      // 'WGS 84 World Equidistant Cylindrical' (EPSG:4087) is chosen
      working_CS := TGIS_CSFactory.ByEPSG( EPSG_EQUIDISTANT ) ;
    end ;

    // working extent is extent in projected (equidistant) CS
    working_ext := viewer.CS.ExtentToCS( working_CS, ext ) ;
    working_viewer_ext := viewer.CS.ExtentToCS( working_CS, viewer.Extent ) ;

    // to assure fixed grid lines the extent must be expanded
    // origin point is always left top corner of viewer's extent
    // not for all aggregation methods
    working_ext := prepareExtentForLinearUnits( working_ext, working_viewer_ext ) ;
    ext := working_CS.ExtentToCS( viewer.CS, working_ext ) ;

    // disable aggregation when extent size is smaller than doubled radius
    if ( 2 * radiusInternal >= ( working_ext.XMax - working_ext.XMin ) ) or
       ( 2 * radiusInternal >= ( working_ext.YMax - working_ext.YMin ) )
    then
      exit ;

    offset := Point( RoundS( working_ext.XMin ), RoundS( working_ext.YMin ) ) ;
    cols_rows := calculateColsRows(
      working_ext.XMax - working_ext.XMin,
      working_ext.YMax - working_ext.YMin
    ) ;
  end
  else begin
    working_CS := viewer.CS ;
    rect_screen := viewer.MapToScreenRect( ext ) ;
    offset := Point( rect_screen.Left, rect_screen.Top ) ;
    cols_rows := calculateColsRows(
      rect_screen.Right - rect_screen.Left,
      rect_screen.Bottom - rect_screen.Top
    ) ;
  end ;

  // main algorithm
  agg_items := TObjectList< TObject >.Create ;
  try
    initializeAggregatorItems( agg_items, cols_rows, offset ) ;

    for obj in Layer.Loop(
      viewer.CS.ExtentToCS( Layer.CS, ext )
    ) do begin
      {$IFDEF DCC}
        shp := obj ;
      {$ELSE}
        shp := TGIS_Shape( obj ) ;
      {$ENDIF}

      // when working with different CS
      // loop can return shape out from viewer's extent
      if ( viewer.CS.EPSG <> Layer.CS.EPSG ) and
         not GisIsCommonExtent( shp.ProjectedExtent, ext )
      then continue ;

      assignShapeToAggregatorItem(
        shp,
        agg_items,
        cols_rows,
        offset,
        working_CS
      ) ;
    end ;

    // get minimum and maximum number of shapes from items
    agg_min_max := addHideShapesAndGetMinMax(
      agg_items,
      ext,
      offset,
      working_CS
    ) ;

    if agg_items.Count > 0 then
      updateMinMaxVal( agg_min_max ) ;
  finally
    FreeObject( agg_items ) ;
  end ;
end;

function TGIS_AggregatorAbstract.addHideShapesAndGetMinMax(
  const _agg_items : TObjectList< TObject > ;
  const _extent    : TGIS_Extent ;
  const _offset    : TPoint ;
  const _cs        : TGIS_CSCoordinateSystem
) : TPoint ;
var
  vmin, vmax      : Integer ;
  i               : Integer ;
  num_empty_items : Integer ;
  agg_item        : T_AggregatorItemAbstract ;
  count           : Integer ;
  {$IFDEF DCC}
    uid           : Int64 ;
  {$ENDIF}
  shp_working_cs  : TGIS_Shape ;
  uid_arr         : TGIS_UidArray ;
  viewer          : IGIS_Viewer ;
  cs_polyg        : TGIS_ShapePolygon ;
  shp_layer_cs    : TGIS_Shape ;
begin
  vmin := GIS_MAX_INTEGER ;
  vmax := -GIS_MAX_INTEGER ;
  num_empty_items := 0 ;

  viewer := Layer.Viewer.Ref ;

  cs_polyg := TGIS_ShapePolygon( _cs.BoundingPolygon( _cs ) ) ;
  try
    for i := 0 to _agg_items.Count - 1 do begin
      agg_item := T_AggregatorItemAbstract( _agg_items[i] ) ;
      count := agg_item.Count ;
      if count < Threshold then begin
        inc( num_empty_items ) ;
        continue ;
      end ;

      // only 1 clustered section -> all original shapes will not be shown, faster!
      if doHideShape then begin
        for uid in agg_item.uids do
          HideShape( uid ) ;
      end ;

      if count > vmax then
        vmax := count ;
      if count < vmin then
        vmin := count ;

      shp_working_cs := createAggregatedShapeInMapCS( agg_item, _offset ) ;
      if ( Layer.CS.EPSG <> _cs.EPSG ) then begin
        if assigned( cs_polyg ) then begin
          shp_working_cs := shp_working_cs.Combine(
            cs_polyg,
            TGIS_TopologyCombineType.Intersection,
            False
          ) ;
        end ;
        if shp_working_cs = nil then
          break ;

        shp_layer_cs := projectShape( shp_working_cs, _cs, Layer.CS ) ;
        FreeObject( shp_working_cs ) ;
      end
      else begin
        shp_layer_cs := shp_working_cs ;
      end ;

      SetLength( uid_arr, 0 ) ;
      AddShape( shp_layer_cs, count, uid_arr ) ;
    end ;
  finally
    FreeObject( cs_polyg ) ;
  end ;

  if _agg_items.Count = num_empty_items then
    _agg_items.Clear ;

  Result := Point( vmin, vmax ) ;
end;

procedure TGIS_AggregatorAbstract.updateMinMaxVal(
  const _agg_min_max : TPoint
) ;
var
  vmin, vmax : Integer ;
begin
  vmin := Max( 1, _agg_min_max.X ) ;
  vmax := Max( 1, _agg_min_max.Y ) ;

  if vmin = vmax then
    if vmin = 1 then
      vmax := 2
    else
      vmin := 1 ;

  Layer.ParamsList.Selected := Layer.ParamsList.Count - 1 ;
  Layer.Params.Render.MinVal := vmin ;
  Layer.Params.Render.MaxVal := vmax ;
end;

function TGIS_AggregatorAbstract.projectShape(
  const _shp       : TGIS_Shape ;
  const _viewer_cs : TGIS_CSCoordinateSystem ;
  const _layer_cs  : TGIS_CSCoordinateSystem
) : TGIS_Shape ;
var
  i, j : Integer ;
begin
  case _shp.ShapeType of
    TGIS_ShapeType.Point   : Result := TGIS_ShapePoint.Create ;
    TGIS_ShapeType.Polygon : Result := TGIS_ShapePolygon.Create ;
  else
    Result := _shp.CreateCopy ;
    exit ;
  end ;

  Result.Lock( TGIS_Lock.Extent ) ;
  Result.AddPart ;
  for i := 0 to _shp.GetNumParts - 1  do begin
    for j := 0 to _shp.GetPartSize( i ) - 1 do begin
      Result.AddPoint( _viewer_cs.ToCS( _layer_cs, _shp.GetPoint( i, j ) ) ) ;
    end ;
  end ;
  Result.Unlock ;
end ;

{$ENDREGION}

{$REGION 'TGIS_AggregatorClusteringAbstract'}
constructor TGIS_AggregatorClusteringAbstract.Create(
  const _layer : TGIS_LayerVector
) ;
begin
  inherited Create ( _layer ) ;

  RadiusAsText := DEFAULT_CLUSTER_RADIUS_TXT;
end;

function TGIS_AggregatorClusteringAbstract.doHideShape : Boolean ;
begin
  Result := HIDE_SHAPES_CLUSTERING ;
end;

procedure TGIS_AggregatorClusteringAbstract.setUpAggregatorTypeSpecificSections ;
var
  orig_params : TGIS_ParamsSectionVector ;
begin
  useShapeType( TGIS_ShapeType.Point ) ;

  if useConfig then exit ;

  orig_params := TGIS_ParamsSectionVector.Create ;
  try
    orig_params.Assign( Layer.Params ) ;

    // Non-clustered
    Layer.ParamsList.Clear ;
    Layer.Params.Assign( orig_params ) ;
    Layer.Params.Query := Format( '%s IS NULL', [GIS_FIELD_AGGREGATED_VALUE] ) ;
    Layer.Params.Legend := 'Non-clustered' ;

    // Clustered
    Layer.ParamsList.Add ;
    Layer.Params.Query := Format( '%s IS NOT NULL', [GIS_FIELD_AGGREGATED_VALUE] ) ;
    Layer.Params.Legend := 'Clustered' ;
    Layer.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
    Layer.Params.Marker.OutlineWidthAsText := 'SIZE:HAIRLINE' ;
    Layer.Params.Marker.ColorAsText := 'RENDERER' ;
    Layer.Params.Marker.SizeAsText := 'RENDERER' ;
    Layer.Params.Marker.SmartSize := 0 ;
    Layer.Params.Marker.ShowLegend := orig_params.Marker.ShowLegend or
                                      orig_params.Area.ShowLegend or
                                      orig_params.Line.ShowLegend ;

    // don't show original symbol in 'Clustered' section
    Layer.Params.Line.ShowLegend := False ;
    Layer.Params.Area.ShowLegend := False ;

  finally
    FreeObject( orig_params ) ;
  end ;
end;

function TGIS_AggregatorClusteringAbstract.createClusterPoint (
  const _ptg : TGIS_Point
) : TGIS_Shape ;
begin
  Result := TGIS_ShapePoint.Create ;
  Result.Lock( TGIS_Lock.Extent ) ;
  Result.AddPart ;
  Result.AddPoint( _ptg ) ;
  Result.Unlock ;
end;

function TGIS_AggregatorClusteringAbstract.createAggregatedShapeInMapCS (
  const _agg_item : TObject ;
  const _offset   : TPoint
) : TGIS_Shape ;
var
  ptg : TGIS_Point ;
begin
  ptg := T_AggregatorItemAbstract( _agg_item ).Position ;
  Result := createClusterPoint( ptg ) ;
end;

{$ENDREGION}

{$REGION 'TGIS_AggregatorClusteringMovingAverage'}
constructor TGIS_AggregatorClusteringMovingAverage.Create(
  const _layer : TGIS_LayerVector
) ;
begin
  inherited Create ( _layer ) ;

  RadiusAsText := DEFAULT_MOV_AVG_RADIUS_TXT ;
  Threshold := DEFAULT_MOV_AVG_THRESHOLD ;
end;

function TGIS_AggregatorClusteringMovingAverage.fget_Caption : String ;
begin
  Result := _rsrc( GIS_RS_AGGREGATION_METHOD_CLSTR_MOVING_AVG ) ;
end;

function TGIS_AggregatorClusteringMovingAverage.prepareExtentForLinearUnits(
  const _ext_draw : TGIS_Extent ;
  const _ext_map  : TGIS_Extent
) : TGIS_Extent ;
begin
  Result := _ext_draw ;
end;

function TGIS_AggregatorClusteringMovingAverage.calculateColsRows(
  const _width  : Double ;
  const _height : Double
) : TPoint ;
begin
  Result := Point( 0, 0 ) ;
end ;

procedure TGIS_AggregatorClusteringMovingAverage.initializeAggregatorItems(
  const _agg_items : TObjectList< TObject > ;
  const _cols_rows : TPoint ;
  const _offset    : TPoint
) ;
begin
  // initialization is not needed
end;

procedure TGIS_AggregatorClusteringMovingAverage.assignShapeToAggregatorItem(
  const _shp       : TGIS_Shape ;
  const _agg_items : TObjectList< TObject > ;
  const _cols_rows : TPoint ;
  const _offset    : TPoint ;
  const _cs        : TGIS_CSCoordinateSystem
) ;
const
  DIST_ERR_FACTOR = 2 ;
var
  viewer        : IGIS_Viewer ;
  ptg_map       : TGIS_Point ;
  ptg_examined  : TGIS_Point ;
  shortest_dist : Double ;
  shortest_idx  : Integer ;
  i             : Integer ;
  agg_item      : T_AggregatorItemAbstract ;
  dist          : Double ;

  procedure add_point_to_agg_item(
    const _agg_item : T_AggregatorItemAbstract ;
    const _ptg_map  : TGIS_Point ;
    const _uid      : Int64
  ) ;
  var
    ptg_screen : TGIS_Point ;
  begin
    _agg_item.Add( _ptg_map, _uid ) ;

    if not useLinearUnits then begin
      ptg_screen := viewer.MapToScreenEx( _agg_item.Position ) ;
      _agg_item.PositionOnScreen := GisPoint( ptg_screen.X - _offset.X, ptg_screen.Y - _offset.Y ) ;
    end ;
  end;

begin
  viewer := Layer.Viewer.Ref ;
  ptg_map := _shp.Centroid ;

  if useLinearUnits then begin
    ptg_map := viewer.CS.ToCS( _cs, ptg_map ) ;
    ptg_examined := ptg_map ;
  end
  else begin
    ptg_examined := viewer.MapToScreenEx( ptg_map ) ;
    ptg_examined := GisPoint( ptg_examined.X - _offset.X, ptg_examined.Y - _offset.Y ) ;
  end ;

  shortest_dist := GIS_MAX_DOUBLE ;
  shortest_idx := -1 ;

  // loop over all clusters and find the nearest
  for i := 0 to _agg_items.Count - 1 do begin
    agg_item := T_AggregatorItemAbstract( _agg_items[i] ) ;

    if useLinearUnits then begin
      dist := GisPoint2Point( ptg_examined, agg_item.Position ) ;

      if dist < ( radiusInternal * DIST_ERR_FACTOR ) then
        dist := _cs.Distance( ptg_examined, agg_item.Position ) ;
    end
    else begin
      dist := GisPoint2Point( ptg_examined, agg_item.PositionOnScreen ) ;
    end ;

    if ( dist <= radiusInternal ) then begin
      if ( dist < shortest_dist ) then begin
        shortest_idx := i ;
        shortest_dist := dist ;
      end ;
    end ;
  end ;

  // cluster was found, so update it
  if ( shortest_idx > -1 ) then begin
    agg_item := T_AggregatorItemAbstract( _agg_items[shortest_idx] ) ;
    add_point_to_agg_item( agg_item, ptg_map, _shp.Uid ) ;
    exit ;
  end ;

  // cluster was not found, so create a new
  agg_item := T_AggregatorItemMovingAverage.Create ;
  add_point_to_agg_item( agg_item, ptg_map, _shp.Uid ) ;
  _agg_items.Add( agg_item ) ;
end;
{$ENDREGION}

{$REGION 'TGIS_AggregatorClusteringSquareGrid'}
constructor TGIS_AggregatorClusteringSquareGrid.Create(
  const _layer : TGIS_LayerVector
) ;
begin
  inherited Create ( _layer ) ;

  binSq := TGIS_AggregatorBinningSquare.Create( _layer ) ;
end;

procedure TGIS_AggregatorClusteringSquareGrid.doDestroy ;
begin
  FreeObject( binSq ) ;
  inherited ;
end;

function TGIS_AggregatorClusteringSquareGrid.fget_Caption : String ;
begin
  Result := _rsrc( GIS_RS_AGGREGATION_METHOD_CLSTR_SQUARE_GRID ) ;
end;

procedure TGIS_AggregatorClusteringSquareGrid.doUpdateRadius ;
begin
  inherited ;
  if binSq.Radius = Radius then
    exit ;

  binSq.Radius := Radius ;
  binSq.doUpdateRadius ;
end;

function TGIS_AggregatorClusteringSquareGrid.prepareExtentForLinearUnits(
  const _ext_draw : TGIS_Extent ;
  const _ext_map  : TGIS_Extent
) : TGIS_Extent ;
begin
  Result := binSq.prepareExtentForLinearUnits( _ext_draw, _ext_map ) ;
end;

function TGIS_AggregatorClusteringSquareGrid.calculateColsRows(
  const _width  : Double ;
  const _height : Double
) : TPoint ;
begin
  Result := binSq.calculateColsRows( _width, _height ) ;
end;

procedure TGIS_AggregatorClusteringSquareGrid.initializeAggregatorItems(
  const _agg_items : TObjectList< TObject > ;
  const _cols_rows : TPoint ;
  const _offset    : TPoint
) ;
var
  cols, rows    : Integer ;
  double_radius : Double ;
  x, y          : Integer ;
  x_examined,
  y_examined    : Integer ;
  agg_item      : T_AggregatorItemAbstract ;
  ptg_examined  : TGIS_Point ;
begin
  cols := _cols_rows.X ;
  rows := _cols_rows.Y ;

  {$IFNDEF JAVA}
    _agg_items.Capacity := cols * rows ;
  {$ENDIF}

  double_radius := radiusInternal * 2 ;

  for x := 0 to cols - 1 do begin
    x_examined := RoundS( x * double_radius + radiusInternal ) + _offset.X ;

    for y := 0 to rows - 1 do begin
      agg_item := T_AggregatorItemOnlyCount.Create ;

      y_examined := RoundS( y * double_radius + radiusInternal ) + _offset.Y ;

      // PositionOnScreen stores left top square corner
      ptg_examined := GisPoint( x_examined, y_examined ) ;
      agg_item.PositionOnScreen := ptg_examined ;

      if useLinearUnits then begin
        agg_item.Position := ptg_examined ;
      end
      else begin
        agg_item.PositionOnScreen := ptg_examined ;
        agg_item.Position := Layer.Viewer.Ref.ScreenToMapEx( agg_item.PositionOnScreen ) ;
      end ;

      _agg_items.Add( agg_item ) ;
    end ;
  end ;
end;

procedure TGIS_AggregatorClusteringSquareGrid.assignShapeToAggregatorItem(
  const _shp       : TGIS_Shape ;
  const _agg_items : TObjectList< TObject > ;
  const _cols_rows : TPoint ;
  const _offset    : TPoint ;
  const _cs        : TGIS_CSCoordinateSystem
) ;
begin
  binSq.assignShapeToAggregatorItem( _shp, _agg_items, _cols_rows, _offset, _cs ) ;
end;
{$ENDREGION}

{$REGION 'TGIS_AggregatorClusteringHexagonalGridAbstract'}
constructor TGIS_AggregatorClusteringHexagonalGridAbstract.Create(
  const _layer : TGIS_LayerVector
) ;
begin
  inherited Create ( _layer ) ;
end;

procedure TGIS_AggregatorClusteringHexagonalGridAbstract.doDestroy ;
begin
  FreeObject( binHex ) ;
  inherited ;
end;

procedure TGIS_AggregatorClusteringHexagonalGridAbstract.doUpdateRadius ;
begin
  inherited ;
  if binHex.Radius = Radius then exit ;

  binHex.Radius := Radius ;
  binHex.doUpdateRadius ;
end;

function TGIS_AggregatorClusteringHexagonalGridAbstract.prepareExtentForLinearUnits(
  const _ext_draw : TGIS_Extent ;
  const _ext_map  : TGIS_Extent
) : TGIS_Extent ;
begin
  Result := binHex.prepareExtentForLinearUnits( _ext_draw, _ext_map ) ;
end;

function TGIS_AggregatorClusteringHexagonalGridAbstract.calculateColsRows(
  const _width  : Double ;
  const _height : Double
) : TPoint ;
begin
  Result := binHex.calculateColsRows( _width, _height ) ;
end;

procedure TGIS_AggregatorClusteringHexagonalGridAbstract.initializeAggregatorItems(
  const _agg_items : TObjectList< TObject > ;
  const _cols_rows : TPoint ;
  const _offset    : TPoint
) ;
begin
  binHex.initializeAggregatorItems( _agg_items, _cols_rows, _offset ) ;
end;

procedure TGIS_AggregatorClusteringHexagonalGridAbstract.assignShapeToAggregatorItem(
  const _shp       : TGIS_Shape ;
  const _agg_items : TObjectList< TObject > ;
  const _cols_rows : TPoint ;
  const _offset    : TPoint ;
  const _cs        : TGIS_CSCoordinateSystem
) ;
begin
  binHex.assignShapeToAggregatorItem( _shp, _agg_items, _cols_rows, _offset, _cs ) ;
end;

function TGIS_AggregatorClusteringHexagonalGridAbstract.createAggregatedShapeInMapCS (
  const _agg_item : TObject ;
  const _offset   : TPoint
) : TGIS_Shape ;
var
  ptg_viewer_cs : TGIS_Point ;
  ptg_examined  : TGIS_Point ;
begin
  ptg_examined := T_AggregatorItemAbstract( _agg_item ).PositionOnScreen ;

  if useLinearUnits then
    ptg_viewer_cs := ptg_examined
  else
    ptg_viewer_cs := Layer.Viewer.Ref.ScreenToMapEx( ptg_examined ) ;

  Result := createClusterPoint( ptg_viewer_cs ) ;
end;
{$ENDREGION}

{$REGION 'TGIS_AggregatorClusteringHexagonalGridFlat'}
constructor TGIS_AggregatorClusteringHexagonalGridFlat.Create(
  const _layer : TGIS_LayerVector
) ;
begin
  inherited Create( _layer ) ;

  binHex := TGIS_AggregatorBinningHexagonFlat.Create( _layer ) ;
end;

function TGIS_AggregatorClusteringHexagonalGridFlat.fget_Caption : String ;
begin
  Result := _rsrc( GIS_RS_AGGREGATION_METHOD_CLSTR_HEX_GRID_FLAT ) ;
end;
{$ENDREGION}

{$REGION 'TGIS_AggregatorClusteringHexagonalGridPointy'}
constructor TGIS_AggregatorClusteringHexagonalGridPointy.Create(
  const _layer : TGIS_LayerVector
) ;
begin
  inherited Create( _layer ) ;

  binHex := TGIS_AggregatorBinningHexagonPointy.Create( _layer ) ;
end;

function TGIS_AggregatorClusteringHexagonalGridPointy.fget_Caption : String ;
begin
  Result := _rsrc( GIS_RS_AGGREGATION_METHOD_CLSTR_HEX_GRID_POINTY ) ;
end;
{$ENDREGION}

{$REGION 'TGIS_AggregatorShapeReduction'}
constructor TGIS_AggregatorShapeReduction.Create(
  const _layer : TGIS_LayerVector
) ;
begin
  inherited Create( _layer ) ;

  binSq := TGIS_AggregatorBinningSquare.Create( _layer ) ;

  if not ( ( Layer.DefaultShapeType = TGIS_ShapeType.Point ) or
           ( Layer.DefaultShapeType = TGIS_ShapeType.MultiPoint ) )
  then
    isLayerSupported := False ;

  showLabel := DEFAULT_SHP_RED_LABEL_COUNT ;
  RadiusAsText := DEFAULT_SHP_RED_RADIUS_TXT ;
end;

procedure TGIS_AggregatorShapeReduction.doDestroy ;
begin
  FreeObject( binSq ) ;
  inherited ;
end;

function TGIS_AggregatorShapeReduction.fget_Caption : String ;
begin
  Result := _rsrc( GIS_RS_AGGREGATION_METHOD_SHAPE_REDUCTION ) ;
end;

procedure TGIS_AggregatorShapeReduction.setUpAggregatorTypeSpecificSections ;
begin
  if useConfig then exit ;

  Layer.Params.Query := Format( '%s IS NOT NULL', [GIS_FIELD_AGGREGATED_VALUE] ) ;
  Layer.Params.Legend := 'Reduced shapes' ;
  Layer.Params.Marker.SmartSize := 0 ;
end;

procedure TGIS_AggregatorShapeReduction.doUpdateRadius ;
begin
  inherited ;
  if binSq.Radius = Radius then
    exit ;

  binSq.Radius := Radius ;
  binSq.doUpdateRadius ;
end;

procedure TGIS_AggregatorShapeReduction.doUpdateThreshold ;
begin
  inherited ;
  Threshold := 1 ;
end;

function TGIS_AggregatorShapeReduction.prepareExtentForLinearUnits(
  const _ext_draw : TGIS_Extent ;
  const _ext_map  : TGIS_Extent
) : TGIS_Extent ;
begin
  Result := _ext_draw ;
end;

function TGIS_AggregatorShapeReduction.calculateColsRows(
  const _width  : Double ;
  const _height : Double
) : TPoint ;
begin
  Result := binSq.calculateColsRows( _width, _height ) ;
end;

procedure TGIS_AggregatorShapeReduction.initializeAggregatorItems(
  const _agg_items : TObjectList< TObject > ;
  const _cols_rows : TPoint ;
  const _offset    : TPoint
) ;
var
  cols, rows : Integer ;
  x, y       : Integer ;
  agg_item   : T_AggregatorItemAbstract ;
begin
  cols := _cols_rows.X ;
  rows := _cols_rows.Y ;

  {$IFNDEF JAVA}
    _agg_items.Capacity := cols * rows ;
  {$ENDIF}

  for x := 0 to cols - 1 do begin
    for y := 0 to rows - 1 do begin
      agg_item := T_AggregatorItemOnlyFirst.Create ;
      _agg_items.Add( agg_item ) ;
    end ;
  end ;
end;

function TGIS_AggregatorShapeReduction.doHideShape : Boolean ;
begin
  Result := HIDE_SHAPES_SHP_RED ;
end;

procedure TGIS_AggregatorShapeReduction.assignShapeToAggregatorItem(
  const _shp       : TGIS_Shape ;
  const _agg_items : TObjectList< TObject > ;
  const _cols_rows : TPoint ;
  const _offset    : TPoint ;
  const _cs        : TGIS_CSCoordinateSystem
) ;
begin
  binSq.assignShapeToAggregatorItem( _shp, _agg_items, _cols_rows, _offset, _cs ) ;
end;

procedure TGIS_AggregatorShapeReduction.updateMinMaxVal(
  const _agg_min_max : TPoint
) ;
begin
  // ShapeReduction does not update the min&max values on the legend
end;

{$ENDREGION}

{$REGION 'TGIS_AggregatorBinningAbstract'}
constructor TGIS_AggregatorBinningAbstract.Create(
  const _layer : TGIS_LayerVector
) ;
begin
  inherited Create ( _layer ) ;
  RadiusAsText := DEFAULT_BIN_RADIUS_TXT;
end;

function TGIS_AggregatorBinningAbstract.doHideShape : Boolean ;
begin
  Result := HIDE_SHAPES_BINNING ;
end;

procedure TGIS_AggregatorBinningAbstract.setUpAggregatorTypeSpecificSections ;
var
  orig_params : TGIS_ParamsSectionVector ;
begin
  useShapeType( TGIS_ShapeType.Polygon ) ;

  if useConfig then exit ;

  orig_params := TGIS_ParamsSectionVector.Create ;
  try
    orig_params.Assign( Layer.Params ) ;

    // Binned section
    Layer.ParamsList.Clear ;
    Layer.Params.Query := Format( '%s IS NOT NULL', [GIS_FIELD_AGGREGATED_VALUE] ) ;
    Layer.Params.Legend := 'Binned' ;
    Layer.Params.Area.ColorAsText := 'RENDERER' ;
    Layer.Params.Marker.ShowLegend := False ;
    Layer.Params.Area.ShowLegend := orig_params.Area.ShowLegend or
                                    orig_params.Line.ShowLegend or
                                    orig_params.Marker.ShowLegend ;
  finally
    FreeObject( orig_params ) ;
  end;
end;
{$ENDREGION}

{$REGION 'TGIS_AggregatorBinningSquare'}
constructor TGIS_AggregatorBinningSquare.Create(
  const _layer : TGIS_LayerVector
) ;
begin
  inherited Create( _layer ) ;
end;

function TGIS_AggregatorBinningSquare.fget_Caption : String ;
begin
  Result := _rsrc( GIS_RS_AGGREGATION_METHOD_BIN_SQUARE ) ;
end;

function TGIS_AggregatorBinningSquare.prepareExtentForLinearUnits(
  const _ext_draw        : TGIS_Extent ;
  const _ext_map : TGIS_Extent
) : TGIS_Extent ;
var
  double_radius       : Double ;
  left_extent_offset  : TGIS_Point ;
  right_extent_offset : TGIS_Point ;
begin
  double_radius := 2 * radiusInternal ;

  left_extent_offset := GisPoint(
    double_radius * FloorS( ( _ext_draw.XMin - _ext_map.XMin ) / double_radius ),
    double_radius * FloorS( ( _ext_draw.YMin - _ext_map.YMin ) / double_radius )
  ) ;

  right_extent_offset := GisPoint(
    double_radius * CeilS( ( _ext_draw.XMax - _ext_map.XMin ) / double_radius ),
    double_radius * CeilS( ( _ext_draw.YMax - _ext_map.YMin ) / double_radius )
  ) ;

  Result := GisExtent(
    _ext_map.XMin + left_extent_offset.X,
    _ext_map.YMin + left_extent_offset.Y,
    _ext_map.XMin + right_extent_offset.X,
    _ext_map.YMin + right_extent_offset.Y
  ) ;
end;

function TGIS_AggregatorBinningSquare.calculateColsRows(
  const _width  : Double ;
  const _height : Double
) : TPoint ;
var
  double_radius : Double ;
  cols, rows    : Integer ;
begin
  double_radius := 2 * radiusInternal ;
  cols := CeilS( _width / double_radius ) ;
  rows := CeilS( _height / double_radius ) ;

  Result := Point( cols, rows ) ;
end ;

procedure TGIS_AggregatorBinningSquare.initializeAggregatorItems(
  const _agg_items : TObjectList< TObject > ;
  const _cols_rows : TPoint ;
  const _offset    : TPoint
) ;
var
  cols, rows : Integer ;
  x, y       : Integer ;
  agg_item   : T_AggregatorItemAbstract ;
begin
  cols := _cols_rows.X ;
  rows := _cols_rows.Y ;

  {$IFNDEF JAVA}
    _agg_items.Capacity := cols * rows ;
  {$ENDIF}

  for x := 0 to cols - 1 do begin
    for y := 0 to rows - 1 do begin
      agg_item := T_AggregatorItemOnlyCount.Create ;
      agg_item.PositionOnScreen := GisPoint( x, y ) ;

      _agg_items.Add( agg_item ) ;
    end ;
  end ;
end;

procedure TGIS_AggregatorBinningSquare.assignShapeToAggregatorItem(
  const _shp       : TGIS_Shape ;
  const _agg_items : TObjectList< TObject > ;
  const _cols_rows : TPoint ;
  const _offset    : TPoint ;
  const _cs        : TGIS_CSCoordinateSystem
) ;
var
  viewer        : IGIS_Viewer ;
  ptg_map       : TGIS_Point ;
  ptg_examined  : TPoint ;
  cols, rows    : Integer ;
  double_radius : Integer ;
  x, y          : Integer ;
  idx           : Integer ;
  agg_item      : T_AggregatorItemAbstract ;
begin
  viewer := Layer.Viewer.Ref ;
  ptg_map := _shp.Centroid ;

  if useLinearUnits then begin
    ptg_map := viewer.CS.ToCS( _cs, ptg_map ) ;
    ptg_examined := Point(
      RoundS( ptg_map.X - _offset.X ),
      RoundS( ptg_map.Y - _offset.Y )
    ) ;
  end
  else begin
    ptg_examined := viewer.MapToScreen( ptg_map ) ;
    ptg_examined := Point(
      ptg_examined.X - _offset.X ,
      ptg_examined.Y - _offset.Y
    ) ;
  end ;

  cols := _cols_rows.X ;
  rows := _cols_rows.Y ;
  double_radius := RoundS( 2 * radiusInternal ) ;

  //? compare performance with doubles
  // x & y can not be negative and exceed cols & rows
  x := Max( 0 , Min( cols - 1, ptg_examined.X div double_radius ) ) ;
  y := Max( 0 , Min( rows - 1, ptg_examined.Y div double_radius ) ) ;
  idx := x * rows + y ;

  agg_item := T_AggregatorItemAbstract( _agg_items[ idx ] ) ;

  // agg_item stores points in working CS (_cs)
  agg_item.Add( ptg_map, _shp.Uid ) ;
end;

function TGIS_AggregatorBinningSquare.createAggregatedShapeInMapCS (
  const _agg_item : TObject ;
  const _offset   : TPoint
) : TGIS_Shape ;
var
  ptg_examined  : TGIS_Point ;
  x, y          : Integer ;
  double_radius : Integer ;
  square_rect   : TRect ;
  square_ext    : TGIS_Extent ;
begin
  ptg_examined := T_AggregatorItemAbstract( _agg_item ).PositionOnScreen ;

  x := RoundS( ptg_examined.X ) ;
  y := RoundS( ptg_examined.Y ) ;

  double_radius := RoundS( 2 * radiusInternal ) ;

  square_rect := Rect(
    x * double_radius + _offset.X ,
    y * double_radius + _offset.Y ,
    ( x + 1 ) * double_radius + _offset.X ,
    ( y + 1 ) * double_radius + _offset.Y
  ) ;

  if useLinearUnits then
    square_ext := GisExtent(
      square_rect.Left,
      square_rect.Top,
      square_rect.Right,
      square_rect.Bottom
    )
  else
    square_ext := Layer.Viewer.Ref.ScreenToMapRect( square_rect ) ;

  // polygon in viewer.CS
  Result := TGIS_ShapePolygon.Create ;
  Result.Lock( TGIS_Lock.Extent ) ;
  Result.AddPart ;
  Result.AddPoint( GisPoint( square_ext.XMin, square_ext.YMin ) ) ;
  Result.AddPoint( GisPoint( square_ext.XMin, square_ext.YMax ) ) ;
  Result.AddPoint( GisPoint( square_ext.XMax, square_ext.YMax ) ) ;
  Result.AddPoint( GisPoint( square_ext.XMax, square_ext.YMin ) ) ;
  Result.Unlock ;
end;
{$ENDREGION}

{$REGION 'TGIS_AggregatorBinningHexagonAbstract'}
constructor TGIS_AggregatorBinningHexagonAbstract.Create(
  const _layer : TGIS_LayerVector
) ;
begin
  inherited Create( _layer ) ;
end;

procedure TGIS_AggregatorBinningHexagonAbstract.doUpdateRadius ;
begin
  inherited ;

  // size is actually hexagon shorter radius
  hexSize := 2 * Sqrt(3) / 3 * radiusInternal ;
  hexHalfSize := hexSize / 2 ;
  hexOneAndHalfSize := 1.5 * hexSize ;
  hexDoubleSize := 2 * hexSize;
  hexTripleSize := 3 * hexSize;
  hexDoubleHeight := 2 * radiusInternal ;
  hexHeight := radiusInternal ;
end;

procedure TGIS_AggregatorBinningHexagonAbstract.initializeAggregatorItems(
  const _agg_items : TObjectList< TObject > ;
  const _cols_rows : TPoint ;
  const _offset    : TPoint
) ;
var
  cols, rows : Integer ;
  x, y       : Integer ;
  agg_item   : T_AggregatorItemAbstract ;
begin
  cols := _cols_rows.X ;
  rows := _cols_rows.Y ;

  {$IFNDEF JAVA}
    _agg_items.Capacity := cols * rows ;
  {$ENDIF}

  for x := 0 to cols - 1 do begin
    for y := 0 to rows - 1 do begin
      agg_item := T_AggregatorItemOnlyCount.Create ;
      agg_item.PositionOnScreen := getHexagonCenter( x, y, _offset ) ;

      _agg_items.Add( agg_item ) ;
    end ;
  end ;
end;

//? improve algorithm in future
procedure TGIS_AggregatorBinningHexagonAbstract.assignShapeToAggregatorItem(
  const _shp       : TGIS_Shape ;
  const _agg_items : TObjectList< TObject > ;
  const _cols_rows : TPoint ;
  const _offset    : TPoint ;
  const _cs        : TGIS_CSCoordinateSystem
) ;
var
  viewer              : IGIS_Viewer ;
  ptg_map             : TGIS_Point ;
  ptg_examined        : TGIS_Point ;
  idx_even, idx_odd   : Integer ;
  agg_item            : T_AggregatorItemAbstract ;
  ptg_even, ptg_odd   : TGIS_Point ;
  dist_even, dist_odd : Double ;
  idx                 : Integer ;
begin
  viewer := Layer.Viewer.Ref ;
  ptg_map := _shp.Centroid ;

  if useLinearUnits then begin
    ptg_map := viewer.CS.ToCS( _cs, ptg_map ) ;
    ptg_examined := ptg_map ;
  end
  else begin
    ptg_examined := viewer.MapToScreenEx( ptg_map ) ;
  end ;
  // find even and odd positions in 'hexagonal grid'
  idx_even := findEvenIndex( ptg_examined, _cols_rows, _offset ) ;
  idx_odd := findOddIndex( ptg_examined, _cols_rows, _offset ) ;

  // two candidate hexagonal bins lie in even and odd 'hex grid'
  // columns (flat) / rows (pointy)
  agg_item := T_AggregatorItemAbstract( _agg_items[ idx_even ] ) ;
  ptg_even := agg_item.PositionOnScreen ;

  agg_item := T_AggregatorItemAbstract( _agg_items[ idx_odd ] ) ;
  ptg_odd := agg_item.PositionOnScreen ;

  // find the nearest
  if useLinearUnits then begin
    dist_even := _cs.Distance( ptg_even, ptg_examined ) ;
    dist_odd := _cs.Distance( ptg_odd, ptg_examined ) ;
  end
  else begin
    dist_even := GisPoint2Point( ptg_even, ptg_examined ) ;
    dist_odd := GisPoint2Point( ptg_odd, ptg_examined ) ;
  end ;

  if dist_even < dist_odd then
    idx := idx_even
  else
    idx := idx_odd ;

  agg_item := T_AggregatorItemAbstract( _agg_items[idx] ) ;
  agg_item.Add( ptg_map, _shp.Uid ) ;
end;

function TGIS_AggregatorBinningHexagonAbstract.createAggregatedShapeInMapCS (
  const _agg_item : TObject ;
  const _offset   : TPoint
) : TGIS_Shape ;
var
  ptg_examined  : TGIS_Point ;
  points        : TGIS_PointList ;
  i             : Integer ;
  ptg_viewer_cs : TGIS_Point ;
begin
  // use center position on the screen
  ptg_examined := T_AggregatorItemAbstract( _agg_item ).PositionOnScreen ;
  points := createHexagonPoints( ptg_examined ) ;

  Result := TGIS_ShapePolygon.Create ;
  try
    Result.Lock( TGIS_Lock.Extent ) ;
    Result.AddPart ;
    for i := 0 to points.Count - 1 do begin
      if useLinearUnits then
        ptg_viewer_cs := points[i]
      else
        ptg_viewer_cs := Layer.Viewer.Ref.ScreenToMapEx( points[i] ) ;

      Result.AddPoint( ptg_viewer_cs ) ;
    end ;
  finally
    Result.Unlock ;
    FreeObject( points ) ;
  end ;
end;
{$ENDREGION}

{$REGION 'TGIS_AggregatorBinningHexagonFlat'}
// size - horizontal dimension
// height - vertical dimension
constructor TGIS_AggregatorBinningHexagonFlat.Create(
  const _layer : TGIS_LayerVector
) ;
begin
  inherited Create( _layer ) ;
end;

function TGIS_AggregatorBinningHexagonFlat.fget_Caption : String ;
begin
  Result := _rsrc( GIS_RS_AGGREGATION_METHOD_BIN_HEX_FLAT ) ;
end;

function TGIS_AggregatorBinningHexagonFlat.prepareExtentForLinearUnits(
  const _ext_draw : TGIS_Extent ;
  const _ext_map  : TGIS_Extent
) : TGIS_Extent ;
var
  left_extent_offset  : TPoint ;
  right_extent_offset : TPoint ;
begin
  left_extent_offset := calculateColsRows(
    _ext_draw.XMin - _ext_map.XMin,
    _ext_draw.YMin - _ext_map.YMin
  )  ;

  // ensure even index of starting hexagon
  if Odd( left_extent_offset.X )  then
    left_extent_offset.X := left_extent_offset.X - 1 ;

  right_extent_offset := calculateColsRows(
    _ext_draw.XMax - _ext_map.XMin,
    _ext_draw.YMax - _ext_map.YMin
  ) ;

  Result := GisExtent(
    _ext_map.XMin + ( left_extent_offset.X - 2 ) * hexOneAndHalfSize,
    _ext_map.YMin + ( left_extent_offset.Y - 2 ) * hexDoubleHeight,
    _ext_map.XMin + ( right_extent_offset.X  )  * hexOneAndHalfSize,
    _ext_map.YMin + ( right_extent_offset.Y  ) * hexDoubleHeight
  ) ;
end;

function TGIS_AggregatorBinningHexagonFlat.calculateColsRows(
  const _width  : Double ;
  const _height : Double
) : TPoint ;
var
  cols, rows : Integer ;
begin
  cols := FloorS( ( _width + hexHalfSize ) / hexOneAndHalfSize ) + 1 ;
  rows := FloorS( ( _height + hexHeight ) / hexDoubleHeight ) + 1 ;

  Result := Point( cols, rows ) ;
end ;

function TGIS_AggregatorBinningHexagonFlat.getHexagonCenter(
  const _x      : Integer ;
  const _y      : Integer ;
  const _offset : TPoint
) : TGIS_Point ;
var
  x_hex_center,
  y_hex_center : Double ;
begin
  x_hex_center := _x * hexOneAndHalfSize + hexHalfSize + _offset.X ;
  y_hex_center := _y * hexDoubleHeight + hexHeight + _offset.Y;

  if ( _x mod 2 ) = 1  then
    y_hex_center := y_hex_center - hexHeight ;

  Result := GisPoint( x_hex_center, y_hex_center ) ;
end;

function TGIS_AggregatorBinningHexagonFlat.findEvenIndex(
  const _ptg       : TGIS_Point ;
  const _cols_rows : TPoint ;
  const _offset    : TPoint
) : Integer ;
var
  cols, rows     : Integer ;
  count          : Integer ;
  x_even, y_even : Integer ;
begin
  cols := _cols_rows.X ;
  rows := _cols_rows.Y ;
  count := cols * rows ;

  x_even := FloorS( ( _ptg.X - _offset.X + hexSize ) / hexTripleSize ) * 2 ;
  y_even := FloorS( ( _ptg.Y - _offset.Y ) / hexDoubleHeight ) ;
  Result := Max( 0, Min( count - 1,  x_even * rows + y_even ) );
end;

function TGIS_AggregatorBinningHexagonFlat.findOddIndex(
  const _ptg       : TGIS_Point ;
  const _cols_rows : TPoint ;
  const _offset    : TPoint
) : Integer ;
var
  cols, rows   : Integer ;
  count        : Integer ;
  x_odd, y_odd : Integer ;
begin
  cols := _cols_rows.X ;
  rows := _cols_rows.Y ;
  count := cols * rows ;

  x_odd := FloorS( ( _ptg.X - _offset.X - hexHalfSize ) / hexTripleSize ) * 2 + 1 ;
  y_odd := FloorS( ( _ptg.Y - _offset.Y + hexHeight ) / hexDoubleHeight ) ;
  Result := Max( 0, Min( count - 1,  x_odd * rows + y_odd ) );
end;

function TGIS_AggregatorBinningHexagonFlat.createHexagonPoints(
  const _center : TGIS_Point
) : TGIS_PointList ;
var
  x, y : Double ;
begin
  x := _center.X ;
  y := _center.Y ;

  Result := TGIS_PointList.Create ;
  Result.Add( GisPoint( x - hexHalfSize, y - hexHeight ) ) ;
  Result.Add( GisPoint( x - hexSize,     y             ) ) ;
  Result.Add( GisPoint( x - hexHalfSize, y + hexHeight ) ) ;
  Result.Add( GisPoint( x + hexHalfSize, y + hexHeight ) ) ;
  Result.Add( GisPoint( x + hexSize ,    y             ) ) ;
  Result.Add( GisPoint( x + hexHalfSize, y - hexHeight ) ) ;
end;
{$ENDREGION}

{$REGION 'TGIS_AggregatorBinningHexagonPointy'}
// size - vertical  dimension
// height - horizontal dimension

constructor TGIS_AggregatorBinningHexagonPointy.Create(
  const _layer : TGIS_LayerVector
) ;
begin
  inherited Create( _layer ) ;
end;

function TGIS_AggregatorBinningHexagonPointy.fget_Caption : String ;
begin
  Result := _rsrc( GIS_RS_AGGREGATION_METHOD_BIN_HEX_POINTY ) ;
end;

function TGIS_AggregatorBinningHexagonPointy.prepareExtentForLinearUnits(
  const _ext_draw        : TGIS_Extent ;
  const _ext_map : TGIS_Extent
) : TGIS_Extent ;
var
  left_extent_offset  : TPoint ;
  right_extent_offset : TPoint ;
begin
  left_extent_offset := calculateColsRows(
    _ext_draw.XMin - _ext_map.XMin,
    _ext_draw.YMin - _ext_map.YMin
  )  ;

  // ensure even index of starting hexagon
  if Odd( left_extent_offset.Y )  then
    left_extent_offset.Y := left_extent_offset.Y - 1 ;

  right_extent_offset := calculateColsRows(
    _ext_draw.XMax - _ext_map.XMin,
    _ext_draw.YMax - _ext_map.YMin
  ) ;

  Result := GisExtent(
    _ext_map.XMin + ( left_extent_offset.X - 2 ) * hexDoubleHeight,
    _ext_map.YMin + ( left_extent_offset.Y - 2 ) * hexOneAndHalfSize,
    _ext_map.XMin + ( right_extent_offset.X  )  * hexDoubleHeight,
    _ext_map.YMin + ( right_extent_offset.Y  ) * hexOneAndHalfSize
  ) ;
end;

function TGIS_AggregatorBinningHexagonPointy.calculateColsRows(
  const _width  : Double ;
  const _height : Double
) : TPoint ;
var
  cols, rows : Integer ;
begin
  rows := FloorS( ( _height + hexHalfSize ) / hexOneAndHalfSize ) + 1 ;
  cols := FloorS( ( _width + hexHeight ) / hexDoubleHeight ) + 1 ;

  Result := Point( cols, rows ) ;
end ;

function TGIS_AggregatorBinningHexagonPointy.getHexagonCenter(
  const _x      : Integer ;
  const _y      : Integer ;
  const _offset : TPoint
) : TGIS_Point ;
var
  x_screen, y_screen : Double ;
begin
  x_screen := _x * hexDoubleHeight + hexHeight + _offset.X ;
  y_screen := _y * hexOneAndHalfSize + hexHalfSize + _offset.Y ;

  if ( _y mod 2 ) = 1  then
    x_screen := x_screen - hexHeight ;

  Result := GisPoint( x_screen, y_screen ) ;
end;

function TGIS_AggregatorBinningHexagonPointy.findEvenIndex(
  const _ptg       : TGIS_Point ;
  const _cols_rows : TPoint ;
  const _offset    : TPoint
) : Integer ;
var
  cols, rows     : Integer ;
  count          : Integer ;
  x_even, y_even : Integer ;
begin
  cols := _cols_rows.X ;
  rows := _cols_rows.Y ;
  count := cols * rows ;

  x_even := FloorS( ( _ptg.X - _offset.X ) / hexDoubleHeight ) ;
  y_even := FloorS( ( _ptg.Y - _offset.Y + hexSize ) / hexTripleSize ) * 2 ;
  Result := Max( 0, Min( count - 1,  x_even * rows + y_even ) );
end;

function TGIS_AggregatorBinningHexagonPointy.findOddIndex(
  const _ptg       : TGIS_Point ;
  const _cols_rows : TPoint ;
  const _offset    : TPoint
) : Integer ;
var
  cols, rows   : Integer ;
  count        : Integer ;
  x_odd, y_odd : Integer ;
begin
  cols := _cols_rows.X ;
  rows := _cols_rows.Y ;
  count := cols * rows ;

  x_odd := FloorS( ( _ptg.X - _offset.X + hexHeight ) / hexDoubleHeight ) ;
  y_odd := FloorS( ( _ptg.Y - _offset.Y - hexHalfSize ) / hexTripleSize ) * 2 + 1 ;
  Result := Max( 0, Min( count - 1,  x_odd * rows + y_odd ) ) ;
end;

function TGIS_AggregatorBinningHexagonPointy.createHexagonPoints(
  const _center : TGIS_Point
) : TGIS_PointList ;
var
  x, y : Double ;
begin
  x := _center.X ;
  y := _center.Y ;

  Result := TGIS_PointList.Create ;
  Result.Add( GisPoint( x - hexHeight, y - hexHalfSize ) ) ;
  Result.Add( GisPoint( x,             y - hexSize     ) ) ;
  Result.Add( GisPoint( x + hexHeight, y - hexHalfSize ) ) ;
  Result.Add( GisPoint( x + hexHeight, y + hexHalfSize ) ) ;
  Result.Add( GisPoint( x,             y + hexSize     ) ) ;
  Result.Add( GisPoint( x - hexHeight, y + hexHalfSize ) ) ;
end;
{$ENDREGION}

{$REGION 'Lider.CG.GIS.GeoAggregator'}
class procedure GisAggregator.SelfRegisterLayer() ;
var
  thc : TGIS_ThreadClass ;
begin
  thc := TGIS_ThreadClass.Create ;
  try
    thc.LockThread ;
    try
      TGIS_DynamicAggregatorFactory.Register(
        GIS_AGGREGATION_BIN_HEX_FLAT,
        TGIS_AggregatorBinningHexagonFlat
      ) ;
      TGIS_DynamicAggregatorFactory.Register(
        GIS_AGGREGATION_BIN_HEX_POINTY,
        TGIS_AggregatorBinningHexagonPointy
      ) ;
      TGIS_DynamicAggregatorFactory.Register(
        GIS_AGGREGATION_BIN_SQUARE,
        TGIS_AggregatorBinningSquare
      ) ;
      TGIS_DynamicAggregatorFactory.Register(
        GIS_AGGREGATION_CLUSTERING_HEX_FLAT,
        TGIS_AggregatorClusteringHexagonalGridFlat
      ) ;
      TGIS_DynamicAggregatorFactory.Register(
        GIS_AGGREGATION_CLUSTERING_HEX_POINTY,
        TGIS_AggregatorClusteringHexagonalGridPointy
      ) ;
      TGIS_DynamicAggregatorFactory.Register(
        GIS_AGGREGATION_CLUSTERING_MOVING_AVG,
        TGIS_AggregatorClusteringMovingAverage
      ) ;
      TGIS_DynamicAggregatorFactory.Register(
        GIS_AGGREGATION_CLUSTERING_SQUARE,
        TGIS_AggregatorClusteringSquareGrid
      ) ;
      TGIS_DynamicAggregatorFactory.Register(
        GIS_AGGREGATION_SHAPE_REDUCTION,
        TGIS_AggregatorShapeReduction
      ) ;
    finally
      thc.UnlockThread ;
    end;
  finally
    FreeObject( thc );
  end;
end;
{$ENDREGION}

{$REGION 'UnitTests'}
{$IFDEF GIS_UNIT_TESTS}
  function randomGisPoint : TGIS_Point ;
  begin
    Result := GisPoint( GetRandom * 1000, GetRandom * 1000 ) ;
  end;

  function randomUid : Integer ;
  begin
    Result := GetRandom( 1000 ) ;
  end;

  procedure TestUnitAggregator.TestAggregatorItemOnlyCount ;
  var
    i        : Integer ;
    agg_item : T_AggregatorItemAbstract ;
  begin
    SET_TEST_NAME( 'TestUnitAggregator.TestAggregatorItemOnlyCount' ) ;

    agg_item := T_AggregatorItemOnlyCount.Create ;
    try
      // default values for empty item
      Assert.AreEqual( 0, agg_item.Position.X, '#01001 - Incorrect default Position.X' ) ;
      Assert.AreEqual( 0, agg_item.Position.Y, '#01002 - Incorrect default Position.Y' ) ;

      Assert.AreEqual( 0, agg_item.PositionOnScreen.X, '#01003 - Incorrect default PositionOnScreen.X' ) ;
      Assert.AreEqual( 0, agg_item.PositionOnScreen.Y, '#01004 - Incorrect default PositionOnScreen.Y' ) ;

      Assert.AreEqual( 0, agg_item.Count, '#01005 - Incorrect default Count' ) ;

      // add first point
      agg_item.Add( randomGisPoint, randomUid ) ;
      Assert.AreEqual( 1, agg_item.Count, '#01006 - Incorrect Count' ) ;

      // add next ten points
      for i := 1 to 10 do
        agg_item.Add( randomGisPoint, randomUid ) ;

      Assert.AreEqual( 11, agg_item.Count, '#01007 - Incorrect Count' ) ;
    finally
      FreeObject( agg_item ) ;
    end ;
  end;

  procedure TestUnitAggregator.TestAggregatorItemOnlyFirst ;
  var
    i         : Integer ;
    agg_item  : T_AggregatorItemAbstract ;
    first_ptg : TGIS_Point ;
  begin
    SET_TEST_NAME( 'TestUnitAggregator.TestAggregatorItemOnlyFirst' ) ;

    agg_item := T_AggregatorItemOnlyFirst.Create ;
    try
      Assert.AreEqual( 0, agg_item.Count, '#02001 - Incorrect Count' ) ;

      // store first point
      first_ptg := randomGisPoint ;

      agg_item.Add( first_ptg, randomUid ) ;
      Assert.AreEqual( 1 , agg_item.Count, '#02002 - Incorrect Count' ) ;
      Assert.AreEqual( first_ptg.X, agg_item.Position.X, '#02003 - Incorrect Position.X' ) ;
      Assert.AreEqual( first_ptg.Y, agg_item.Position.Y, '#02004 - Incorrect Position.Y' ) ;

      // add next random points
      for i := 1 to 10 do
        agg_item.Add( randomGisPoint, randomUid ) ;

      // AggregatorItem stores only first object
      Assert.AreEqual( 1 , agg_item.Count, '#02005 - Incorrect Count' ) ;
      Assert.AreEqual( first_ptg.X, agg_item.Position.X, '#02006 - Incorrect Position.X' ) ;
      Assert.AreEqual( first_ptg.Y, agg_item.Position.Y, '#02007 - Incorrect Position.Y' ) ;
    finally
      FreeObject( agg_item ) ;
    end ;
  end;

  procedure TestUnitAggregator.TestAggregatorItemMovingAverage ;
  var
    i        : Integer ;
    agg_item : T_AggregatorItemAbstract ;
  begin
    SET_TEST_NAME( 'TestUnitAggregator.TestAggregatorItemMovingAverage' ) ;

    agg_item := T_AggregatorItemMovingAverage.Create ;
    try
      Assert.AreEqual( 0, agg_item.Count, '#03001 - Incorrect Count' ) ;

      // add ten next points
      for i := 1 to 10 do
        agg_item.Add( GisPoint( i, -1 * i ), i ) ;

      // AggregatorItem should dynamically calculate average position
      Assert.AreEqual( 10, agg_item.Count, '#03002 - Incorrect Count' ) ;
      // sum from 1 to 10 divided by 10
      Assert.AreEqual( 5.5, agg_item.Position.X, '#03003 - Incorrect Position.X' ) ;
      Assert.AreEqual( -5.5, agg_item.Position.Y, '#03004 - Incorrect Position.Y' ) ;
    finally
      FreeObject( agg_item ) ;
    end ;
  end;
{$ENDIF}
{$ENDREGION}

{$IFNDEF OXYGENE}
  initialization
    GisAggregator.SelfRegisterLayer() ;

    {$IFDEF GIS_UNIT_TESTS}
      TestUnitAggregator.Register ;
    {$ENDIF}
{$ENDIF}
end.



