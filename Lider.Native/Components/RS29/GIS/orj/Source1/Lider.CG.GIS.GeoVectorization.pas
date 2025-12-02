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
  Provides classes to vectorize pixel layers.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoVectorization;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoVectorization"'}
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
    TatukGIS.RTL ;
  {$ENDIF}
  {$IFDEF DCC}
    System.Generics.Collections,
    System.Generics.Defaults,
    System.Types,

    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoLayerPixel,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypes ;
  {$ENDIF}
  {$IFDEF JAVA}
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
  {$ENDIF}
  {$IFDEF ISLAND}
    remobjects.elements.rtl.*,
    TatukGIS.RTL ;
  {$ENDIF}

type

  {#gendoc:hide}
  GisVectorization = class
    public
      class procedure SelfRegisterPipeline ;
  end;

  /// <summary>
  ///   Abstract class for all classes which implement vector generation from
  ///   grid layers.
  /// </summary>
  TGIS_GridToVectorAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF}
                              class( TGIS_Object )

   {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF} const
     DEFAULT_IGNORENODATA = True ;
     MAX_TILE_SIZE        = 4096 ;
     NORMALIZED_NODATA    = GIS_MIN_INTEGER ;

    private
      FIgnoreNoData    : Boolean ;
      FTolerance       : Single ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      busyEventManager : TGIS_BusyEventManager ;
      noData           : Single ;
      cellSize         : TGIS_Point ;
      xyTilesCount  : TPoint ;
      tilesCount       : Integer ;

    private
      {$IFDEF CLR}
        procedure fadd_BusyEvent    ( const _value : TGIS_BusyEvent ) ;
        procedure fremove_BusyEvent ( const _value : TGIS_BusyEvent  ) ;
      {$ELSE}
        function  fget_BusyEvent    : TGIS_BusyEvent ;
        procedure fset_BusyEvent    ( const _value : TGIS_BusyEvent ) ;
      {$ENDIF}

      // True, if the field exist and is of a numeric type
      function  numericFieldExist   ( const _field : String ;
                                      const _lv    : TGIS_LayerVector
                                    ) : Boolean ;

      procedure prepareTiles        ( const _grid  : TGIS_LayerPixel ) ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      procedure fset_Tolerance      ( const _value  : Single ) ; virtual ;

      // determine cell size for given pixel layer extent and size
      function getCellSize          ( const _extent : TGIS_Extent ;
                                      const _width  : Integer ;
                                      const _height : Integer
                                    ) : TGIS_Point ;

      // determine number of tiles for given pixel layer size
      function getXYTilesCount      ( const _width          : Integer ;
                                      const _height         : Integer
                                    ) : TPoint ;

      // get next tile XY index
      function getTileIndex          ( const _i              : Integer ;
                                       const _tiles_no       : TPoint
                                     ) : TPoint ;

      // prepare rectangle for locking pixels
      function getTileRect           ( const _tile_index     : TPoint ;
                                       const _xy_tiles_count : TPoint ;
                                       const _width          : Integer ;
                                       const _height         : Integer
                                     ) : TRect ;

      // convert floating-point grid value to unique integer value
      // (based on Tolerance)
      function normalizeGridValue   ( const _val      : Single
                                    ) : Integer ; overload ;
                                    {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      function normalizeGridValue   ( const _val      : Single ;
                                      const _isNoData : Boolean
                                    ) : Integer ; overload ;
                                    {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      // integer to single (this process works like rounding with fixed Tolerance)
      function unnormalizeGridValue ( const _val      : Integer
                                    ) : Single ;
                                    {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}


    published // events
      /// <summary>
      ///   Event fired upon progress of the interpolation process.
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

    public
      /// <summary>
      ///   A minimum distinguishing value of the grid cells.
      /// </summary>
      /// <remarks>
      ///   Grid values are rounded using the Tolerance to avoid a problem with
      ///   distinguishing floating-point cell values.
      ///   For example, if Tolerance=0.1, cell values of 0.123 and 0.111 are
      ///   considered the same, but 0.211 is not.
      /// </remarks>
      property Tolerance      : Single
                                read FTolerance
                                write fset_Tolerance ;

      /// <summary>
      ///   If true (default), cells of NoData is ommitted.
      /// </summary>
      property IgnoreNoData   : Boolean
                                read FIgnoreNoData
                                write FIgnoreNoData ;

    protected
      procedure doDestroy ; override ;

    public
      /// <summary>
      ///   Creates an object.
      /// </summary>
      constructor Create ;

      /// <summary>
      ///   Creates vector shapes from a grid layer.
      /// </summary>
      /// <param name="_grid">
      ///   the input grid layer
      /// </param>
      /// <param name="_vector">
      ///   the output vector layer
      /// </param>
      /// <param name="_field">
      ///   if not empty, the field of the specified name will be populated
      ///   with the grid value to the created shape
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BADPARAM
      /// </exception>
      procedure Generate      ( const _grid   : TGIS_LayerPixel ;
                                const _vector : TGIS_LayerVector ;
                                const _field  : String
                              ) ; virtual ;
  end;

  /// <summary>
  ///   Class that enables grid to vector (point) conversion.
  /// </summary>
  TGIS_GridToPoint = {$IFDEF OXYGENE} public {$ENDIF}
                     class( TGIS_GridToVectorAbstract )

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF} const
      DEFAULT_TOLERANCE = 0.0 ;

    private
      FPointSpacing : Double ;

    private
      function addPointToLayer  ( const _vector : TGIS_LayerVector ;
                                  const _ptg    : TGIS_Point
                                ) : TGIS_Shape ;
      function addPoint3DToLayer( const _vector : TGIS_LayerVector ;
                                  const _ptg    : TGIS_Point3D
                                ) : TGIS_Shape ;
    public
      /// <summary>
      ///   Poins spacing in layer's CS units for sampling grid.
      ///   PointSpacing=0 (default) means that points are generated at each
      ///   grid cell.
      /// </summary>
      property PointSpacing   : Double
                                read FPointSpacing
                                write FPointSpacing ;

    public
      /// <inheritdoc/>
      constructor Create ;

      /// <inheritdoc/>
      procedure Generate      ( const _grid   : TGIS_LayerPixel ;
                                const _vector : TGIS_LayerVector ;
                                const _field  : String
                              ) ; override ;
  end;

  /// <summary>
  ///   Class that enables grid to vector (polygon) conversion.
  /// </summary>
  TGIS_GridToPolygon = {$IFDEF OXYGENE} public {$ENDIF}
                       class( TGIS_GridToVectorAbstract )

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF} const
      DEFAULT_TOLERANCE   = 1.0 ;
      DEFAULT_SPLITSHAPES = True ;

    private
      FSplitShapes : Boolean ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      procedure fset_Tolerance( const _value  : Single ) ; override ;

    private
      {$IFNDEF GENDOC}
      function falseMatrix           ( const _height         : Integer ;
                                       const _width          : Integer
                                     ) : TArray<TArray<Boolean>> ;

      function gridPosToTilePos      ( const _row            : Integer ;
                                       const _col            : Integer ;
                                       const _bounds         : TRect
                                     ) : TPoint ;
                                     {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      function findLargestRectangle  ( const _value          : Integer ;
                                       const _start_cell     : TPoint ;
                                       const _lock   : TGIS_LayerPixelLock ;
                                       const _matrix : TArray<TArray<Boolean>>
                                     ) : TRect ;

      function tryRight              ( const _val            : Integer ;
                                       const _rect           : TRect ;
                                       const _lock   : TGIS_LayerPixelLock ;
                                       const _matrix : TArray<TArray<Boolean>>
                                     ) : Boolean ;

      function tryDown               ( const _val            : Integer ;
                                       const _rect           : TRect ;
                                       const _lock   : TGIS_LayerPixelLock ;
                                       const _matrix : TArray<TArray<Boolean>>
                                     ) : Boolean ;

      function tryLeft               ( const _val            : Integer ;
                                       const _rect           : TRect ;
                                       const _lock   : TGIS_LayerPixelLock ;
                                       const _matrix : TArray<TArray<Boolean>>
                                     ) : Boolean ;

      function tryUp                 ( const _val            : Integer ;
                                       const _rect           : TRect ;
                                       const _lock   : TGIS_LayerPixelLock ;
                                       const _matrix : TArray<TArray<Boolean>>
                                     ) : Boolean ;

      function cellWasProcessed      ( const _row            : Integer ;
                                       const _col            : Integer ;
                                       const _bounds         : TRect ;
                                       const _matrix : TArray<TArray<Boolean>>
                                     ) : Boolean ;

      function cellHasDifferentValue ( const _row            : Integer ;
                                       const _col            : Integer ;
                                       const _val            : Integer ;
                                       const _grid           : TGIS_GridArray
                                     ) : Boolean ;

      function markProcessed         ( const _rect           : TRect ;
                                       const _bounds         : TRect ;
                                       const _matrix : TArray<TArray<Boolean>>
                                     ) : Integer ;

      procedure addNewCandidates     ( const _candidates     : TStack< TPoint >;
                                       const _rect           : TRect;
                                       const _bounds         : TRect
                                     ) ;

      function createPolygonFromRect ( const _rect : TRect ;
                                       const _bounds       : TRect ;
                                       const _ext          : TGIS_Extent ;
                                       const _tile_rect    : TRect ;
                                       const _cell_size    : TGIS_Point ;
                                       const _value        : Single ;
                                       const _make3d       : Boolean
                                     ) : TGIS_Shape ;

      function gridRectToTileRect    ( const _rect : TRect ;
                                       const _bounds : TRect
                                     ) : TRect ;
                                     {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      procedure addShapeToDict       ( const _shape : TGIS_Shape ;
                                       const _dict  : TDictionary< Integer,
                                                      TGIS_ShapeList > ;
                                       const _value : Integer
                                     ) ;

      procedure freeDict             ( const _dict  : TDictionary< Integer,
                                                      TGIS_ShapeList >
                                     ) ;

      {$ENDIF}

    public
      /// <summary>
      ///   If True (default), single-part polygon shapes are created;
      ///   otherwise multipart shapes are created.
      /// </summary>
      property SplitShapes           : Boolean
                                       read FSplitShapes
                                       write FSplitShapes ;

    public
      /// <inheritdoc/>
      constructor Create ;

      /// <inheritdoc/>
      procedure Generate             ( const _grid   : TGIS_LayerPixel ;
                                       const _vector : TGIS_LayerVector ;
                                       const _field  : String
                                     ) ; override ;
  end;

implementation

{$IFDEF DCC}
  uses
    System.Math,
    System.SysUtils,

    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoPipeline,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoTopology,
    Lider.CG.GIS.GeoTypesUI ;
{$ENDIF}

type
  T_Pipeline_GridToPolygon = class( TGIS_PipelineOperationExtendedAbstract )
    const
      PARAM_GRIDTOPOLYGON_TOLERANCE    = 'Tolerance' ;
      PARAM_GRIDTOPOLYGON_SPLITSHAPES  = 'SplitShapes' ;
      PARAM_GRIDTOPOLYGON_IGNORENODATA = 'IgnoreNoData' ;

    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end;

{$REGION 'TGIS_GridToVectorAbstract'}
constructor TGIS_GridToVectorAbstract.Create ;
begin
  inherited ;

  FTolerance := 0 ;
  FIgnoreNoData := DEFAULT_IGNORENODATA ;
  busyEventManager := TGIS_BusyEventManager.Create( Self ) ;
end;

procedure TGIS_GridToVectorAbstract.doDestroy ;
begin
  FreeObject( busyEventManager ) ;

  inherited ;
end ;

{$IFDEF CLR}
  procedure TGIS_GridToVectorAbstract.fadd_BusyEvent(
    const _value : TGIS_BusyEvent
  ) ;
  begin
    busyEventManager.BusyEvent += _value ;
  end ;

  procedure TGIS_GridToVectorAbstract.fremove_BusyEvent(
    const _value : TGIS_BusyEvent
  ) ;
  begin
    busyEventManager.BusyEvent -= _value ;
  end ;
{$ELSE}
  procedure TGIS_GridToVectorAbstract.fset_BusyEvent(
    const _value : TGIS_BusyEvent
  ) ;
  begin
    busyEventManager.BusyEvent := _value ;
  end ;

  function TGIS_GridToVectorAbstract.fget_BusyEvent : TGIS_BusyEvent ;
  begin
    Result := busyEventManager.BusyEvent ;
  end ;
{$ENDIF}

procedure TGIS_GridToVectorAbstract.Generate(
  const _grid   : TGIS_LayerPixel ;
  const _vector : TGIS_LayerVector ;
  const _field  : String
) ;
begin
  if not assigned( _grid ) then
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERNOEXIST ),'_grid', 0 ) ;

  if not _grid.IsGridImage then
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERNOTGRID ), _grid.Name, 0 ) ;

  if not assigned( _vector ) then
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERNOEXIST ), '_vector', 0 ) ;

  _vector.Open ;

  if IsStringEmpty( _field ) or not numericFieldExist( _field, _vector ) then
    raise EGIS_Exception.Create(  _rsrc( GIS_RS_ERR_WRONGFIELD ), _field, 0 ) ;

  noData := _grid.NoDataValue ;
  prepareTiles( _grid ) ;
end;

procedure TGIS_GridToVectorAbstract.prepareTiles(
  const _grid: TGIS_LayerPixel
) ;
begin
  cellSize := getCellSize( _grid.Extent, _grid.BitWidth, _grid.BitHeight ) ;

  // tiles count
  xyTilesCount := getXYTilesCount( _grid.BitWidth, _grid.BitHeight ) ;
  tilesCount := xyTilesCount.X * xyTilesCount.Y;
end;

procedure TGIS_GridToVectorAbstract.fset_Tolerance(
  const _value  : Single
) ;
begin
  FTolerance := _value ;
end ;

function TGIS_GridToVectorAbstract.numericFieldExist(
  const _field : String ;
  const _lv    : TGIS_LayerVector
) : Boolean ;
var
  fld_info : TGIS_FieldInfo ;
  fld_indx : Integer ;
begin
  Result := False ;

  fld_indx := _lv.FindField( _field ) ;
  if fld_indx < 0 then
    exit ;

  fld_info := _lv.FieldInfo( fld_indx ) ;
  case fld_info.FieldType of
    TGIS_FieldType.Number,
    TGIS_FieldType.Float  : Result := True ;
  end ;
end ;

function TGIS_GridToVectorAbstract.getCellSize(
  const _extent : TGIS_Extent ;
  const _width  : Integer ;
  const _height : Integer
) : TGIS_Point ;
begin
  Result := GisPoint(
    ( _extent.XMax - _extent.XMin ) / _width,
    ( _extent.YMax - _extent.YMin ) / _height
  ) ;
end;

function TGIS_GridToVectorAbstract.getXYTilesCount(
  const _width  : Integer ;
  const _height : Integer
) : TPoint ;
var
  x, y : Integer ;
begin
  x := _width div MAX_TILE_SIZE ;
  y := _height div MAX_TILE_SIZE ;

  if ( _width mod MAX_TILE_SIZE ) <> 0 then
    inc( x ) ;

  if ( _height mod MAX_TILE_SIZE ) <> 0 then
    inc( y ) ;

  Result := Point( x, y ) ;
end;

function TGIS_GridToVectorAbstract.getTileIndex(
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

function TGIS_GridToVectorAbstract.getTileRect(
  const _tile_index     : TPoint ;
  const _xy_tiles_count : TPoint ;
  const _width          : Integer ;
  const _height         : Integer
) : TRect ;
var
  left, top, right, bottom : Integer ;
begin
  left := _tile_index.X * MAX_TILE_SIZE ;
  top := _tile_index.Y * MAX_TILE_SIZE ;
  right := ( _tile_index.X + 1 ) * MAX_TILE_SIZE ;
  bottom := ( _tile_index.Y + 1 ) * MAX_TILE_SIZE ;

  // if last X tile, cut at width
  if _tile_index.X = _xy_tiles_count.X - 1 then
    right := Min( right, _width ) ;

  // if last Y tile, cut at height
  if _tile_index.Y = _xy_tiles_count.Y - 1 then
    bottom := Min( bottom, _height ) ;

  Result := Rect( left, top, right, bottom ) ;
end;

function TGIS_GridToVectorAbstract.normalizeGridValue(
  const _val : Single
) : Integer ;
begin
  Result := normalizeGridValue(
    _val,
    GisIsSameValue( _val, noData, GIS_SINGLE_RESOLUTION )
  ) ;
end;

function TGIS_GridToVectorAbstract.normalizeGridValue(
  const _val      : Single ;
  const _isNoData : Boolean
) : Integer ;
var
  normalized_val : Single ;
begin
  if _isNoData then begin
    Result := NORMALIZED_NODATA ;
    exit ;
  end ;

  normalized_val := _val / FTolerance ;

  if normalized_val <= GIS_MIN_INTEGER then
    // the lowest integer value reserved for normalized nodata
    Result := GIS_MIN_INTEGER + 1
  else if normalized_val >= GIS_MAX_INTEGER then
    Result := GIS_MAX_INTEGER
  else
    Result := RoundS( normalized_val ) ;
end;

function TGIS_GridToVectorAbstract.unnormalizeGridValue(
  const _val : Integer
) : Single ;
begin
  if _val = NORMALIZED_NODATA then begin
    Result := noData ;
    exit ;
  end ;

  Result := _val * FTolerance ;
end;
{$ENDREGION}

{$REGION 'TGIS_GridToPoint'}
constructor TGIS_GridToPoint.Create ;
begin
  inherited ;

  FTolerance := DEFAULT_TOLERANCE ;
end;

procedure TGIS_GridToPoint.Generate(
  const _grid   : TGIS_LayerPixel ;
  const _vector : TGIS_LayerVector ;
  const _field  : String
) ;
var
  rounding_on  : Boolean ;
  i, j         : Integer ;
  i_tile_pos   : Integer ;
  j_tile_pos   : Integer ;
  x, y         : Double ;
  ptg          : TGIS_Point ;
  ptg3D        : TGIS_Point3D ;
  total        : Int64 ;

  val_base     : Integer ;
  val          : Single ;
  isNoData     : Boolean;
  shp          : TGIS_Shape ;

  i_tile       : Integer ;
  tile_index   : TPoint ;
  tile_rect    : TRect ;

  lock         : TGIS_LayerPixelLock ;

  rgb_mapped   : TGIS_Color ;
  natives_vals : TGIS_DoubleArray ;
  transparency : Boolean ;
  make3D       : Boolean ;
begin
  inherited ;

  // if Tolerance=0, normalization is off
  rounding_on := not GisIsSameValue( Tolerance, 0 ) ;

  make3D := (_vector.DefaultDimension = TGIS_DimensionType.XYZ) ;

  // all cells with tiling (default algorithm)
  // PointSpacing=0 or PointSpacing=CellSize
  if ( FPointSpacing < GIS_DOUBLE_RESOLUTION ) or
     GisIsSameValue( FPointSpacing, cellSize.X, GIS_DOUBLE_RESOLUTION ) and
     GisIsSameValue( FPointSpacing, cellSize.Y, GIS_DOUBLE_RESOLUTION ) then
  begin
    // main progress stage
    busyEventManager.StartEvent( _rsrc( GIS_RS_BUSY_TILING ), tilesCount ) ;
    try
      for i_tile := 0 to tilesCount - 1 do begin
        tile_index := getTileIndex( i_tile, xyTilesCount ) ;
        tile_rect := getTileRect(
          tile_index, xyTilesCount, _grid.BitWidth, _grid.BitHeight
        ) ;

        lock := _grid.LockPixels( tile_rect, False ) ;
        total := ( lock.Bounds.Width + 1 ) * ( lock.Bounds.Height + 1 ) ;
        // new progress stage
        busyEventManager.StartEvent(
          Format( _rsrc( GIS_RS_BUSY_TILING_TILEIOFN ), [i_tile+1, tilesCount] ), total
        ) ;
        try
          for j := lock.Bounds.Top to lock.Bounds.Bottom do begin
            j_tile_pos := j - lock.Bounds.Top ;
            y := _grid.Extent.YMax - ( tile_rect.Top + j_tile_pos + 0.5 ) * cellSize.Y ;

            for i := lock.Bounds.Left to lock.Bounds.Right do begin
              // push event at each cell
              if busyEventManager.PushEvent then
                exit ;

              i_tile_pos := i - lock.Bounds.Left ;
              x := _grid.Extent.XMin + ( tile_rect.Left + i_tile_pos + 0.5 ) * cellSize.X ;

              val := lock.Grid[j, i] ;

              // hadling NoData
              isNoData := GisIsSameValue( val, noData, GIS_SINGLE_RESOLUTION ) ;
              if IgnoreNoData and isNoData then
                continue ;

              // if Tolerance is set, values are normalized (rounded)
              if rounding_on then begin
                val_base := normalizeGridValue( val, isNoData ) ;
                val := unnormalizeGridValue( val_base ) ;
              end ;

              if make3D then begin
                ptg3D := GisPoint3D( x, y, val ) ;
                shp   := addPoint3DToLayer( _vector, ptg3D ) ;
              end
              else begin
                ptg := GisPoint( x, y ) ;
                shp := addPointToLayer( _vector, ptg ) ;
              end ;
              shp.SetField( _field, val ) ;
            end ;
          end ;
        finally
          _grid.UnlockPixels( lock ) ;
          busyEventManager.EndEvent ;
        end ;
      end ;
    finally
      busyEventManager.EndEvent ;
    end ;
  end
  // generate points according to PointSpacing
  else begin
    // points count
    xyTilesCount := Point(
      CeilS( ( _grid.Extent.XMax - _grid.Extent.XMin ) / FPointSpacing ),
      CeilS( ( _grid.Extent.YMax - _grid.Extent.YMin ) / FPointSpacing )
    ) ;

    total := xyTilesCount.X * xyTilesCount.Y ;
    busyEventManager.StartEvent( _rsrc( GIS_RS_BUSY_READ ), total ) ;
    try
      // start at center of left top grid cell
      for j := 0 to xyTilesCount.Y do begin
        y := _grid.Extent.YMax - ( j + 0.5 ) * FPointSpacing ;

        for i := 0 to xyTilesCount.X do begin
          if busyEventManager.PushEvent then
            exit ;

          x := _grid.Extent.XMin + ( i + 0.5 ) * FPointSpacing ;

          ptg := GisPoint( x, y ) ;
          if _grid.Locate( ptg, rgb_mapped, natives_vals, transparency ) then
            val := natives_vals[0]
          else
            continue ;

          // hadling NoData
          isNoData := GisIsSameValue( val, noData, GIS_SINGLE_RESOLUTION ) ;
          if IgnoreNoData and isNoData then
            continue ;

          // if Tolerance is set, values are normalized (rounded)
          if rounding_on then begin
            val_base := normalizeGridValue( val, isNoData ) ;
            val := unnormalizeGridValue( val_base ) ;
          end ;

          if make3D then begin
            ptg3D := GisPoint3D( x, y, val ) ;
            shp   := addPoint3DToLayer( _vector, ptg3D ) ;
          end
          else begin
            shp := addPointToLayer( _vector, ptg ) ;
          end ;
          shp.SetField( _field, val ) ;
        end ;
      end ;
    finally
      busyEventManager.EndEvent ;
    end ;
  end ;
  _vector.SaveData ;
end;

function TGIS_GridToPoint.addPointToLayer(
  const _vector : TGIS_LayerVector ;
  const _ptg    : TGIS_Point
) : TGIS_Shape ;
begin
  Result := _vector.CreateShape( TGIS_ShapeType.Point ) ;
  Result.Lock( TGIS_Lock.Extent ) ;
  Result.AddPart ;
  Result.AddPoint( _ptg ) ;
  Result.Unlock ;
end ;

function TGIS_GridToPoint.addPoint3DToLayer(
  const _vector : TGIS_LayerVector ;
  const _ptg    : TGIS_Point3D
) : TGIS_Shape ;
begin
  Result := _vector.CreateShape( TGIS_ShapeType.Point, TGIS_DimensionType.XYZ ) ;
  Result.Lock( TGIS_Lock.Extent ) ;
  Result.AddPart ;
  Result.AddPoint3D( _ptg ) ;
  Result.Unlock ;
end ;

{$ENDREGION}

{$REGION 'TGIS_GridToPolygon'}
constructor TGIS_GridToPolygon.Create ;
begin
  inherited ;

  FTolerance := DEFAULT_TOLERANCE ;
  FSplitShapes := DEFAULT_SPLITSHAPES ;
end;

procedure TGIS_GridToPolygon.fset_Tolerance(
  const _value  : Single
) ;
begin
  if GisIsSameValue( _value, 0 ) then
    FTolerance := DEFAULT_TOLERANCE
  else
    FTolerance := _value ;
end;

procedure TGIS_GridToPolygon.Generate(
  const _grid   : TGIS_LayerPixel ;
  const _vector : TGIS_LayerVector ;
  const _field  : String
) ;
var
  pair_count            : Integer ;

  i_tile                : Integer ;
  tile_cells_count      : Integer ;
  tile_index, tile_pos  : TPoint ;
  tile_rect             : TRect ;

  processed_cells_matrix: TArray<TArray<Boolean>> ;
  processed_cells_count : Integer ;

  {$IFNDEF OXYGENE}
    pair                : TPair< Integer, TGIS_ShapeList > ;
    shp_part            : TGIS_Shape ;
  {$ENDIF}
  polygs_dict           : TDictionary< Integer, TGIS_ShapeList > ;
  tile_polygs_dict      : TDictionary< Integer, TGIS_ShapeList > ;

  shp_list              : TGIS_ShapeList ;
  singleparts_list      : TGIS_ShapeList ;

  lock                  : TGIS_LayerPixelLock ;

  val_base              : Integer ;
  val                   : Single ;
  isNoData              : Boolean;
  growing_rect          : TRect ;
  candidate_pt          : TPoint ;
  candidates            : TStack< TPoint > ;
  make3D                : Boolean ;
  shp                   : TGIS_Shape ;
  shp_new               : TGIS_Shape ;
  shp_need_freeing      : Boolean ;

  topo                  : TGIS_Topology ;
begin
  inherited ;

  make3D := ( _vector.DefaultDimension = TGIS_DimensionType.XYZ ) ;

  // sub polygons (rectangles) are collected and grouped by normalized grid value
  polygs_dict := TDictionary< Integer, TGIS_ShapeList >.Create ;

  // stack stores subsequent base cells
  // base cell is cell from which the algorithm starts to form a new rectangle
  candidates := TStack< TPoint >.Create ;

  // create one topology object for whole algorithm
  topo := TGIS_Topology.Create ;
  // 2 progress stages: (1) processing tiles, (2) processing multipolygons
  busyEventManager.StartEvent(
    _rsrc( GIS_RS_BUSY_VECTORIZATION_GRIDTOPOLYGON ), 2
  ) ;
  try
    // 50% of total progress is intended for tile processing (0..50%)
    busyEventManager.StartEvent( _rsrc( GIS_RS_BUSY_TILING ), tilesCount * 2 ) ;
    try
      for i_tile := 0 to tilesCount - 1 do begin

        if busyEventManager.PushEvent(
          Format( _rsrc( GIS_RS_BUSY_TILING_TILEIOFN ), [i_tile+1, tilesCount] )
        ) then
          exit ;

        tile_index := getTileIndex( i_tile, xyTilesCount ) ;
        tile_rect := getTileRect(
          tile_index, xyTilesCount, _grid.BitWidth, _grid.BitHeight
        ) ;
        tile_cells_count := tile_rect.Width * tile_rect.Height ;

        // dictionary for rectangles within a tile
        tile_polygs_dict := TDictionary< Integer, TGIS_ShapeList >.Create ;

        lock := _grid.LockPixels( tile_rect, False ) ;
        try
          // initialization
          processed_cells_count := 0 ;

          // boolean matrix filled with False value (shape same as tile_rect)
          processed_cells_matrix := falseMatrix(
            tile_rect.Width, tile_rect.Height
          ) ;

          //clear candidates for each new tile
          candidates.Clear ;

          // first base cell is in the center of tile
          candidate_pt := Point(
            RoundS( ( lock.Bounds.Left + lock.Bounds.Right ) / 2 ),
            RoundS( ( lock.Bounds.Bottom + lock.Bounds.Top) / 2 )
          ) ;
          candidates.Push( candidate_pt );

          while ( processed_cells_count < tile_cells_count ) and
                ( candidates.Count > 0 ) do // at least one candidate must exist
          begin
            // get next candidate
            candidate_pt := candidates.Pop ;

            // depending on file type, LockPixels returns whole grid
            // or only demanded chunk, so candidate position must be standardize
            tile_pos := gridPosToTilePos(
              candidate_pt.Y, candidate_pt.X, lock.Bounds
            ) ;

            // check if pixel was processed
            if processed_cells_matrix[tile_pos.Y, tile_pos.X] then
              continue ;

            // values are normalized to unique based on fixed Tolerance
            val := lock.Grid[candidate_pt.Y, candidate_pt.X] ;
            isNoData := GisIsSameValue( val, noData, GIS_SINGLE_RESOLUTION ) ;
            val_base := normalizeGridValue( val, isNoData ) ;

            // find the largest rectangle with the same normalized value
            growing_rect := findLargestRectangle(
              val_base,
              candidate_pt,
              lock,
              processed_cells_matrix
            ) ;

            // update processed cells counter and processed cells array
            processed_cells_count := processed_cells_count +
              markProcessed( growing_rect, lock.Bounds, processed_cells_matrix ) ;

            // add new candidates cells - two on each corner
            addNewCandidates( candidates, growing_rect, lock.Bounds ) ;

            // hadling NoData
            if IgnoreNoData and isNoData then
              continue ;

            // create polygon shape
            shp := createPolygonFromRect(
              growing_rect,
              lock.Bounds,
              _grid.Extent,
              tile_rect,
              cellSize,
              val,
              make3D
            ) ;

            // shape polygons are stored in dictionary per tile
            addShapeToDict( shp, tile_polygs_dict, val_base ) ;
          end ;

          // after completing all polygons for unique classes in a tile...
          pair_count := tile_polygs_dict.Count ;
          busyEventManager.StartEvent( GIS_RS_BUSY_TOPOLOGY_UNION, pair_count ) ;
          try
            for pair in tile_polygs_dict do begin
              if busyEventManager.PushEvent then
                exit ;

              val_base := pair.Key ;
              shp_list := pair.Value ;

              // make union from them and free object list
              shp := topo.UnionOnList( shp_list ) ;

              // and add to global polygons dictionary
              addShapeToDict( shp, polygs_dict, val_base ) ;
            end ;
          finally
            busyEventManager.EndEvent ;
          end;
        finally
          _grid.UnlockPixels( lock ) ;

          // shape lists stored in dictionary need freeing
          freeDict( tile_polygs_dict ) ;
          FreeObject( tile_polygs_dict ) ;
        end ;
      end ;
    finally
      busyEventManager.EndEvent ;
    end;

    // multipolygons are processed and stored separately for each tile
    // to avoid high memory consumptions for big areas
    pair_count := polygs_dict.Count ;
    // remaining 50% of progress (50..100%)
    busyEventManager.StartEvent( GIS_RS_BUSY_TOPOLOGY_UNION, pair_count ) ;
    try
      for pair in polygs_dict do begin
        if busyEventManager.PushEvent then begin
          _vector.RevertShapes ;
          exit ;
        end ;

        val_base := pair.Key ;
        shp_list := pair.Value ;

        // do not run topology union only for one object
        if shp_list.Count = 1 then begin
          shp := TGIS_Shape( shp_list[0] ) ;
          shp_need_freeing := False ;  // shp_list owns it
        end
        else begin
          shp := topo.UnionOnList( shp_list ) ;
          shp_need_freeing := True ;  // new shape
        end ;

        if not assigned( shp ) then
          continue ;

        val :=  unnormalizeGridValue( val_base ) ;
        if FSplitShapes and ( shp.GetNumParts > 1 ) then begin

          // split polygons and keep holes
          singleparts_list := TGIS_ShapePolygon( shp ).Split( True ) ;
          try
            for shp_part in singleparts_list do begin
              shp_new := _vector.AddShape( shp_part ) ;
              shp_new.SetField( _field, val ) ;
            end ;
          finally
            FreeObject( singleparts_list ) ;
          end ;
        end
        else begin
          shp_new := _vector.AddShape( shp ) ;
          shp_new.SetField( _field, val ) ;
        end ;

        if shp_need_freeing then
          FreeObject( shp ) ;
      end ;
    finally
      busyEventManager.EndEvent ;
    end;
  finally
    // object lists stored in dictionary need freeing
    freeDict( polygs_dict ) ;
    FreeObject( polygs_dict ) ;
    FreeObject( topo ) ;
    FreeObject( candidates ) ;

    busyEventManager.EndEvent ;
  end ;
end;

function TGIS_GridToPolygon.falseMatrix(
  const _height : Integer ;
  const _width  : Integer
) : TArray<TArray<Boolean>> ;
var
  i, j : Integer ;
begin
  SetLength( Result, _height, _width ) ;
  for i := 0 to _width - 1 do
    for j := 0 to _height - 1 do
      Result[j, i] := False ;
end;

function TGIS_GridToPolygon.gridPosToTilePos(
  const _row    : Integer ;
  const _col    : Integer ;
  const _bounds : TRect
) : TPoint ;
begin
  Result := Point( _row - _bounds.Top, _col - _bounds.Left ) ;
end;

// growing-rectangle algorithm expands the rectangle from base cell
// clockwise from the right
function TGIS_GridToPolygon.findLargestRectangle(
  const _value      : Integer ;
  const _start_cell : TPoint ;
  const _lock       : TGIS_LayerPixelLock;
  const _matrix     : TArray<TArray<Boolean>>
) : TRect ;
var
  growing_rect  : TRect ;
  can_try_right : Boolean ;
  can_try_down  : Boolean ;
  can_try_left  : Boolean ;
  can_try_up    : Boolean ;
begin
  growing_rect := Rect(
    _start_cell.X,  // left
    _start_cell.Y,  // top
    _start_cell.X,  // right
    _start_cell.Y   // bottom
  ) ;

  can_try_right := True ;
  can_try_down := True ;
  can_try_left := True ;
  can_try_up := True ;

  while can_try_right or
        can_try_down or
        can_try_left or
        can_try_up do
  begin
    if can_try_right then
      if tryRight( _value, growing_rect, _lock, _matrix ) then
        {$IFDEF DCC}
          inc( growing_rect.Right )
        {$ELSE}
          growing_rect := Rect(
            growing_rect.Left,
            growing_rect.Top,
            growing_rect.Right + 1,
            growing_rect.Bottom
          )
        {$ENDIF}
      else
        can_try_right := False ;

    if can_try_down then
      if tryDown( _value, growing_rect, _lock, _matrix ) then
        {$IFDEF DCC}
          inc( growing_rect.Bottom )
        {$ELSE}
          growing_rect := Rect(
            growing_rect.Left,
            growing_rect.Top,
            growing_rect.Right,
            growing_rect.Bottom + 1
          )
        {$ENDIF}
      else
        can_try_down := False ;

    if can_try_left then
      if tryLeft( _value, growing_rect, _lock, _matrix ) then
        {$IFDEF DCC}
          dec( growing_rect.Left )
        {$ELSE}
          growing_rect := Rect(
            growing_rect.Left - 1,
            growing_rect.Top,
            growing_rect.Right,
            growing_rect.Bottom
          )
        {$ENDIF}
      else
        can_try_left := False ;

    if can_try_up then
      if tryUp( _value, growing_rect, _lock, _matrix ) then
        {$IFDEF DCC}
          dec( growing_rect.Top )
        {$ELSE}
          growing_rect := Rect(
            growing_rect.Left,
            growing_rect.Top - 1,
            growing_rect.Right,
            growing_rect.Bottom
          )
        {$ENDIF}
      else
        can_try_up := False ;
  end ;

  Result := growing_rect ;
end;

function TGIS_GridToPolygon.tryRight(
  const _val    : Integer ;
  const _rect   : TRect ;
  const _lock   : TGIS_LayerPixelLock ;
  const _matrix : TArray<TArray<Boolean>>
) : Boolean ;
var
  id, row, col, count : Integer ;
begin
  Result := False ;

  row := _rect.Top ;
  col := _rect.Right + 1 ;

  if ( col = _lock.Bounds.Right + 1 ) then
    exit ;

  count := _rect.Bottom - _rect.Top + 1 ;

  for id := 0 to count - 1 do begin
    if cellWasProcessed( row + id, col, _lock.Bounds, _matrix ) or
       cellHasDifferentValue( row+id, col, _val, _lock.Grid ) then
      exit ;
  end ;

  Result := True ;
end;

function TGIS_GridToPolygon.tryLeft(
  const _val    : Integer ;
  const _rect   : TRect ;
  const _lock   : TGIS_LayerPixelLock ;
  const _matrix : TArray<TArray<Boolean>>
) : Boolean ;
var
  id, row, col, count : Integer ;
begin
  Result := False ;

  row := _rect.Top ;
  col := _rect.Left - 1 ;

  if col = _lock.Bounds.Left-  1 then
    exit ;

  count := _rect.Bottom - _rect.Top + 1 ;

  for id := 0 to count - 1 do begin
    if cellWasProcessed( row + id, col, _lock.Bounds, _matrix ) or
       cellHasDifferentValue( row + id, col, _val, _lock.Grid ) then
      exit ;
  end ;

  Result := True ;
end;

function TGIS_GridToPolygon.tryDown(
  const _val    : Integer ;
  const _rect   : TRect ;
  const _lock   : TGIS_LayerPixelLock ;
  const _matrix : TArray<TArray<Boolean>>
) : Boolean ;
var
  id, row, col, count : Integer ;
begin
  Result := False ;

  row := _rect.Bottom + 1 ;
  col := _rect.Left ;

  if row = _lock.Bounds.Bottom + 1 then
    exit ;

  count := _rect.Right - _rect.Left + 1;
  for id := 0 to count - 1 do begin
    if cellWasProcessed( row, col + id, _lock.Bounds, _matrix ) or
       cellHasDifferentValue( row, col + id, _val, _lock.Grid ) then
      exit ;
  end ;

  Result := True ;
end;

function TGIS_GridToPolygon.tryUp(
  const _val    : Integer ;
  const _rect   : TRect ;
  const _lock   : TGIS_LayerPixelLock ;
  const _matrix : TArray<TArray<Boolean>>
) : Boolean ;
var
  id, row, col, count : Integer ;
begin
  Result := False ;

  row := _rect.Top - 1 ;
  col := _rect.Left ;

  if row = _lock.Bounds.Top - 1 then
    exit ;

  count := _rect.Right - _rect.Left + 1;
  for id := 0 to count - 1 do begin
    if cellWasProcessed( row, col + id, _lock.Bounds, _matrix ) or
       cellHasDifferentValue( row, col + id, _val, _lock.Grid ) then
      exit ;
  end ;

  Result := True ;
end;

// do not visit already processes cell
function TGIS_GridToPolygon.cellWasProcessed(
  const _row    : Integer ;
  const _col    : Integer ;
  const _bounds : TRect ;
  const _matrix : TArray<TArray<Boolean>>
) : Boolean ;
var
  tile_pos : TPoint ;
begin
  tile_pos := gridPosToTilePos( _row, _col, _bounds ) ;
  Result := _matrix[tile_pos.Y, tile_pos.X] ;
end;

// compare base cell value with the next value
function TGIS_GridToPolygon.cellHasDifferentValue(
  const _row  : Integer ;
  const _col  : Integer ;
  const _val  : Integer ;
  const _grid : TGIS_GridArray
) : Boolean ;
var
  val_curr : Integer ;
  val_grid : Single ;
begin
  Result := False ;

  val_grid :=  _grid[_row, _col] ;
  if IsNan( val_grid ) then
    Result:= False;

  val_curr := normalizeGridValue( val_grid ) ;
  if _val <> val_curr then
    Result := True ;
end;

function TGIS_GridToPolygon.markProcessed(
  const _rect   : TRect ;
  const _bounds : TRect ;
  const _matrix : TArray<TArray<Boolean>>
) : Integer ;
var
  col, row : Integer ;
  arr_pos  : TPoint ;
begin
  for col := _rect.Left to _rect.Right do
    for row := _rect.Top to _rect.Bottom do begin
      arr_pos := gridPosToTilePos( row, col, _bounds ) ;
      _matrix[arr_pos.Y, arr_pos.X] := True ;
    end;

  Result := ( _rect.Width + 1 ) * ( _rect.Height + 1 ) ;
end ;

procedure TGIS_GridToPolygon.addNewCandidates(
  const _candidates : TStack< TPoint >;
  const _rect       : TRect;
  const _bounds     : TRect
) ;
begin
  // left-top corner
  // add left
  if _rect.Left > _bounds.Left then
    _candidates.Push( Point( _rect.Left - 1, _rect.Top ) ) ;
  // add top
  if _rect.Top > _bounds.Top then
    _candidates.Push( Point( _rect.Left, _rect.Top - 1 ) ) ;

  // right-top corner
  // add right
  if _rect.Right < _bounds.Right then
    _candidates.Push( Point( _rect.Right + 1, _rect.Top ) ) ;
  // add top
  if _rect.Top > _bounds.Top then
    _candidates.Push( Point( _rect.Right, _rect.Top - 1 ) ) ;

  // left-bottom corner
  // add left
  if _rect.Left > _bounds.Left then
    _candidates.Push( Point( _rect.Left - 1, _rect.Bottom ) ) ;
  // add bottom
  if _rect.Bottom < _bounds.Bottom then
    _candidates.Push( Point( _rect.Left, _rect.Bottom + 1 ) ) ;

  // right-bottom corner
  // add right
  if _rect.Right < _bounds.Right then
    _candidates.Push( Point( _rect.Right + 1, _rect.Bottom ) ) ;
  // add bottom
  if _rect.Bottom < _bounds.Bottom then
    _candidates.Push( Point( _rect.Right, _rect.Bottom + 1 ) ) ;
end;

function TGIS_GridToPolygon.createPolygonFromRect(
  const _rect      : TRect ;
  const _bounds    : TRect ;
  const _ext       : TGIS_Extent ;
  const _tile_rect : TRect ;
  const _cell_size : TGIS_Point ;
  const _value     : Single ;
  const _make3d    : Boolean
) : TGIS_Shape ;
var
  growing_rect  : TRect ;
  growing_rectF : TRectF ;
begin
  // growing rectangle coordinates must be standardize
  // for the same reason as grid position
  growing_rect := gridRectToTileRect( _rect, _bounds ) ;

  // convert rectangle to map units
  growing_rectF := RectF(
    _ext.XMin + ( _tile_rect.Left + growing_rect.Left)        * _cell_size.X,
    _ext.YMax - ( _tile_rect.Top  + growing_rect.Top )        * _cell_size.Y,
    _ext.XMin + ( _tile_rect.Left + growing_rect.Right  + 1 ) * _cell_size.X,
    _ext.YMax - ( _tile_rect.Top  + growing_rect.Bottom + 1 ) * _cell_size.Y
  ) ;

  // return polygon constructed clockwise
  if _make3d then begin
    Result := TGIS_ShapePolygon.Create( TGIS_DimensionType.XYZ ) ;
    Result.Lock( TGIS_Lock.Extent ) ;
    Result.AddPart ;
    Result.AddPoint3D( GisPoint3D( growing_rectF.Left, growing_rectF.Top, _value ) ) ;
    Result.AddPoint3D( GisPoint3D( growing_rectF.Right, growing_rectF.Top, _value ) ) ;
    Result.AddPoint3D( GisPoint3D( growing_rectF.Right, growing_rectF.Bottom, _value ) ) ;
    Result.AddPoint3D( GisPoint3D( growing_rectF.Left, growing_rectF.Bottom, _value ) ) ;
    Result.AddPoint3D( GisPoint3D( growing_rectF.Left, growing_rectF.Top, _value ) ) ;
    Result.Unlock ;
  end
  else begin
    Result := TGIS_ShapePolygon.Create( TGIS_DimensionType.XY ) ;
    Result.Lock( TGIS_Lock.Extent ) ;
    Result.AddPart ;
    Result.AddPoint( GisPoint( growing_rectF.Left, growing_rectF.Top ) ) ;
    Result.AddPoint( GisPoint( growing_rectF.Right, growing_rectF.Top ) ) ;
    Result.AddPoint( GisPoint( growing_rectF.Right, growing_rectF.Bottom ) ) ;
    Result.AddPoint( GisPoint( growing_rectF.Left, growing_rectF.Bottom ) ) ;
    Result.AddPoint( GisPoint( growing_rectF.Left, growing_rectF.Top ) ) ;
    Result.Unlock ;
  end ;
end ;

function TGIS_GridToPolygon.gridRectToTileRect(
  const _rect   : TRect ;
  const _bounds : TRect
) : TRect ;
begin
  Result := Rect(
    _rect.Left   - _bounds.Left,
    _rect.Top    - _bounds.Top,
    _rect.Right  - _bounds.Left,
    _rect.Bottom - _bounds.Top
  ) ;
end;

procedure TGIS_GridToPolygon.addShapeToDict(
  const _shape : TGIS_Shape ;
  const _dict  : TDictionary< Integer, TGIS_ShapeList > ;
  const _value : Integer
) ;
var
  shp_list : TGIS_ShapeList ;
begin
  if not _dict.TryGetValue( _value, shp_list ) then begin
    shp_list := TGIS_ShapeList.Create ;
    _dict.Add( _value, shp_list )
  end ;
  shp_list.Add( _shape ) ;
end;

procedure TGIS_GridToPolygon.freeDict(
  const _dict : TDictionary< Integer, TGIS_ShapeList >
) ;
var
  {$IFNDEF OXYGENE}
  pair     : TPair< Integer, TGIS_ShapeList > ;
  {$ENDIF}
  shp_list : TGIS_ShapeList ;
begin
  for pair in _dict do begin
    shp_list := pair.Value ;
    FreeObjectNotNil( shp_list ) ;
  end ;
end;
{$ENDREGION}

{$REGION 'Pipeline'}
  class procedure GisVectorization.SelfRegisterPipeline ;
  begin
    RegisterPipeline(
      'Vectorization.GridToPolygon',
      T_Pipeline_GridToPolygon
    ) ;
  end ;

  procedure T_Pipeline_GridToPolygon.Initialize ;
  begin
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
      PARAM_GRIDTOPOLYGON_TOLERANCE,
      TGIS_PipelineParameterType.Float,
      NaN,
      NaN,
      False,
      FloatToStr( TGIS_GridToPolygon.DEFAULT_TOLERANCE ),
      ''
    ) ;
    defineParam(
      PARAM_GRIDTOPOLYGON_SPLITSHAPES,
      TGIS_PipelineParameterType.Boolean,
      NaN,
      NaN,
      False,
      BoolToStr( TGIS_GridToPolygon.DEFAULT_SPLITSHAPES ),
      ''
    ) ;
    defineParam(
      PARAM_GRIDTOPOLYGON_IGNORENODATA,
      TGIS_PipelineParameterType.Boolean,
      NaN,
      NaN,
      False,
      BoolToStr( TGIS_GridToPolygon.DEFAULT_IGNORENODATA ),
      ''
    ) ;
  end;

  procedure T_Pipeline_GridToPolygon.Execute;
  var
    src          : TGIS_LayerPixel ;
    dst          : TGIS_LayerVector ;
    dst_fld      : String ;
    polygonizer  : TGIS_GridToPolygon ;
  begin
    src := ParamAsLayerPixel( GIS_PIPELINE_PARAM_SOURCE ) ;
    dst := ParamAsLayerVector( GIS_PIPELINE_PARAM_DESTINATION ) ;
    dst_fld := ParamAsField( dst, GIS_PIPELINE_PARAM_FIELD, '') ;

   polygonizer := TGIS_GridToPolygon.Create ;
    try
      polygonizer.Tolerance:= ParamAsFloat(
        PARAM_GRIDTOPOLYGON_TOLERANCE,
        TGIS_GridToPolygon.DEFAULT_TOLERANCE
      ) ;
      polygonizer.SplitShapes := ParamAsBool(
        PARAM_GRIDTOPOLYGON_SPLITSHAPES,
        TGIS_GridToPolygon.DEFAULT_SPLITSHAPES
      ) ;
      polygonizer.IgnoreNoData := ParamAsBool(
        PARAM_GRIDTOPOLYGON_IGNORENODATA,
        TGIS_GridToPolygon.DEFAULT_IGNORENODATA
      ) ;
      polygonizer.busyEventManager.UseProgressThreshold := False ;
      {$IFDEF CLR}
        polygonizer.BusyEvent += DoBusyEvent ;
      {$ELSE}
        polygonizer.BusyEvent := {$IFDEF OXYGENE}@{$ENDIF}DoBusyEvent ;
      {$ENDIF}
      polygonizer.Generate( src, dst, dst_fld ) ;
    finally
      FreeObject( polygonizer ) ;
    end ;

    inherited ;
  end ;
{$ENDREGION}

{$REGION 'initialization / finalization'}

{$IFDEF DCC}
  initialization
    GisVectorization.SelfRegisterPipeline ;
{$ENDIF}

{$ENDREGION}

end.
