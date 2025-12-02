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
  Encapsulation of a LAS file access.
}

{$IFDEF DCC}
  unit GisLayerLAS ;
  {$HPPEMIT '#pragma link "GisLayerLAS"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Variants,
    System.Generics.Collections,
    System.Generics.Defaults,

    GisTypes,
    GisLayerVector,
    GisFileLAS ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerLAS = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  {$IFDEF OXYGENE}
    T_cursorLas nested in TGIS_LayerLAS = record
      public
      /// <summary>
      ///  Is cursor in use.
      /// </summary>
      curInUse : Boolean ;

      /// <summary>
      ///  Current shape. Layer access is based on record-by-record access.
      /// </summary>
      curShape : TGIS_Shape ;

      /// <summary>
      ///  Preallocated shape. Recreating the shape on each cursor movement is
      ///  much faster than full Create constructor
      /// </summary>
      curShapePoint : TGIS_ShapePoint ;

    end ;
  {$ENDIF}

  /// <summary>
  ///   Layer that can read LAS file.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    SmartSize property can be used to control density of displayed points.
  ///    </note>
  /// </remarks>
  TGIS_LayerLAS = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )

    private

      /// <summary>
      ///   Shape file encapsulation.
      /// </summary>
      lasFile : TGIS_FileLAS ;

      /// <summary>
      ///   Last UID in a shape file. If not set then -1.
      /// </summary>
      lastUid : TGIS_Uid ;

      /// <summary>
      ///   if True, open as a point cloud shape.
      /// </summary>
      asPointCloud : Boolean ;

      /// <summary>
      ///   if True, open as a point cloud filtered shape.
      /// </summary>
      asPointCloudFilter : Boolean ;

      /// <summary>
      ///   Number of points after filtering.
      /// </summary>
      numPointsFilterd : Int64 ;
    private
      {$IFDEF OXYGENE}
        cursorLas : array of T_cursorLas ;
      {$ELSE}
        cursorLas : array of record
          /// <summary>
          ///  Is cursor in use.
          /// </summary>
          curInUse : Boolean ;

          /// <summary>
          ///  Current shape. Layer access is based on record-by-record access.
          /// </summary>
          curShape : TGIS_Shape ;

          /// <summary>
          ///  Preallocated shape. Recreating the shape on each cursor movement is
          //   much faster than full Create constructor
          /// </summary>
          curShapePoint : TGIS_ShapePoint ;

        end ;
      {$ENDIF}


    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // property access methods

      function  fget_PathRTree           : String ; override;
         

    private
      // private methods

      /// <summary>
      ///   Read a shape from the shape file and recreate it.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor to be used
      /// </param>
      procedure readShape                ( const _cursor      : Integer
                                         ) ;

      /// <summary>
      ///   Build a point cloud shape.
      /// </summary>
      procedure buildPointCloud           ;

      /// <summary>
      ///   Check mode of a file from the path.
      /// </summary>
      /// <param name="_path">
      ///   file path
      /// </param>
      procedure checkMode                ( const _path : String
                                         ) ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // for internal use of TGIS_Viewer 

      /// <inheritdoc/>
      function  getFieldInternal         ( const _uid         : TGIS_Uid   ;
                                           const _name        : String  ;
                                           const _cursor      : Integer
                                         ) : Variant ; override;

      /// <inheritdoc/>
      function  getBindedFieldInternal   ( const _shape       : TObject ;
                                           const _field       : Integer ;
                                           const _cursor      : Integer
                                         ) : Variant ; override;

      procedure fset_UseRTree            ( const _value       : Boolean
                                         ) ; override;

      /// <inheritdoc/>
      procedure setUp                    ; override;
         
      // cursor access function(s)

      /// <inheritdoc/>
      function  cursorOpen               :  Integer ; override;

      /// <inheritdoc/>
      procedure cursorClose              ( const _cursor  : Integer
                                         ) ; override;

      /// <inheritdoc/>
      procedure cursorFirst              ( const _cursor      : Integer     ;
                                           const _viewerCS    : Boolean     ;
                                           const _extent      : TGIS_Extent ;
                                           const _query       : String      ;
                                           const _shape       : TGIS_Shape  ;
                                           const _de9im       : String      ;
                                           const _skipDeleted : Boolean
                                         ) ; override;

      /// <inheritdoc/>
      procedure cursorNext               ( const _cursor      : Integer
                                         ) ; override;

      /// <inheritdoc/>
      function  cursorEof                ( const _cursor      : Integer
                                         ) : Boolean ; override;

      /// <inheritdoc/>
      function  cursorShape              ( const _cursor      : Integer
                                         ) : TGIS_Shape ; override;

    protected
      // destructors

      procedure doDestroy                ; override;
      
    public
      // constructors
    
      /// <inheritdoc/>
      constructor Create                 ; override;

      /// <inheritdoc/>
      function  DrawEx                   ( const _extent   : TGIS_Extent
                                         ) : Boolean ; override;

      /// <inheritdoc/>
      function  PreRecognize             ( const _path     : String ;
                                             var _new_path : String
                                         ) : Boolean ; override;
    public
      // shape access function(s)

      /// <inheritdoc/>
      function  GetShape                 ( const _uid         : TGIS_Uid ;
                                           const _cursor      : Integer
                                         ) : TGIS_Shape ; override;

      /// <inheritdoc/>
      function  GetLastUid               : TGIS_Uid ; override;

      /// <inheritdoc/>
      procedure RevertShapes ; override;
  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    System.Math,
    System.SyncObjs,
    GisFunctions,
    GisInternals,
    GisResource,
    GisLayer,
    GisRtl,
    GisTypesUI,
    GisShapes3D,
    GisRegistredLayers ;
{$ENDIF}

type
  T_voxel = class
    numPoints : Integer ;
    x, y, z   : Double ;
    c         : Cardinal ;
    constructor Create ;
    procedure UpdateVoxel( const _ptg   : TGIS_Point3D ;
                           const _color : Cardinal
                         ) ;
    function  GetPoint( var _ptg   : TGIS_Point3D ;
                        var _color : Cardinal
                      ) : Boolean ;
  end ;

  constructor T_voxel.Create ;
  begin
    inherited ;
    numPoints := 0 ;
    x := 0 ;
    y := 0 ;
    z := 0 ;
    c := 0 ;
  end ;

  procedure T_voxel.UpdateVoxel(
    const _ptg   : TGIS_Point3D ;
    const _color : Cardinal
  ) ;
  begin
    x := x + _ptg.X ;
    y := y + _ptg.Y ;
    z := z + _ptg.Z ;
    if c = 0 then
      c := _color ;
    inc( numPoints ) ;
  end;

  function T_voxel.GetPoint(
    var _ptg   : TGIS_Point3D ;
    var _color : Cardinal
  ) : Boolean ;
  begin
    if numPoints > 0 then begin
      _ptg.X := x / numPoints ;
      _ptg.Y := y / numPoints ;
      _ptg.Z := z / numPoints ;
      _color := c ;
      Result := True ;
    end
    else
      Result := False ;
  end ;

//=============================================================================
// TGIS_LayerLAS
//=============================================================================

  constructor TGIS_LayerLAS.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent ] ;
    lasFile  := nil ;

    asPointCloud       := False ;
    asPointCloudFilter := False ;
    numPointsFilterd   := 0 ;
  end ;

  procedure TGIS_LayerLAS.doDestroy ;
  var
    i : Integer ;
  begin
    for i := 0 to high( cursorLas ) do
      FreeObject( cursorLas[i].curShapePoint ) ;

    FreeObject( lasFile ) ;

    inherited ;
  end ;

  procedure TGIS_LayerLAS.readShape(
    const _cursor : Integer
  ) ;
  var
    uid : TGIS_Uid ;
  begin
    uid := lasFile.GetPointIndex( _cursor ) ;

    // TODO: Think of skipping recreate, just update uid
    cursorLas[_cursor].curShapePoint.Recreate(
      nil, nil, False, uid, Self, TGIS_DimensionType.XYZ
    ) ;
    cursorLas[_cursor].curShapePoint.Reset ;
    cursorLas[_cursor].curShapePoint.AddPart ;

    cursorLas[_cursor].curShapePoint.Lock( TGIS_Lock.Internal ) ;
    cursorLas[_cursor].curShapePoint.AddPoint3D( lasFile.GetPoint( _cursor ) ) ;
    cursorLas[_cursor].curShapePoint.Unlock ;
    cursorLas[_cursor].curShape := getEdited( cursorLas[_cursor].curShapePoint );
  end ;

  procedure TGIS_LayerLAS.RevertShapes ;
  var
    i : Integer ;
  begin
    inherited ;

    for i := 0 to BUILTIN_CURSORS - 1 do
      cursorLas[i].curShape := nil ;
  end ;

  procedure TGIS_LayerLAS.buildPointCloud ;
  var
    pshp        : TGIS_ShapePointCloud ;
    ptg         : TGIS_Point3D ;
    ihglass     : Int64 ;
    indices     : TObjectList<T_voxel> ;
    leafCount   : TGIS_SingleVector ;
    hasColor    : Boolean ;
    minmaxp     : TGIS_Extent3D ;
    leafSize    : TGIS_SingleVector ;
    invLeafSize : TGIS_SingleVector ;
    minmaxb     : TGIS_Extent3D ;
    divb        : TGIS_SingleVector ;
    divb_mul    : TGIS_SingleVector ;
    i           : Cardinal ;
    k0, k1, k2  : Integer ;
    idx         : Int64 ;
    centroid    : TGIS_Point3D ;
    color       : Cardinal ;
    numVox      : Cardinal ;
    ext         : TGIS_Extent ;
  begin
    ihglass := 0 ;
    hasColor := lasFile.HasPointColor ;
    // block selection to avoid gdi hanging
    FUnSupportedOperations := GisAddOperationType( FUnSupportedOperations,
                                                   TGIS_OperationType.Select
                                                  ) ;
    // block editing to avoid gdi hanging
    FUnSupportedOperations := GisAddOperationType( FUnSupportedOperations,
                                                   TGIS_OperationType.Edit
                                                  ) ;
    pshp := CreateShape( TGIS_ShapePointCloud ) as TGIS_ShapePointCloud ;
    pshp.AddPart ;
    pshp.Lock( TGIS_Lock.Projection ) ;
    try
      if asPointCloud then begin
        Params.Marker.Size := 20 ;

        ext := GisExtent2DFrom3D( lasFile.Extent ) ;
        lasFile.MoveFirst(0, ext ) ;
        while not lasFile.Eof(0) do begin
           inc( ihglass ) ;
           if ihglass mod GIS_PROGRESS_TRESHOLD = 0 then begin
             if HourglassShake then
               break ;
           end ;
          lasFile.MoveNext(0) ;

          ptg := lasFile.GetPoint(0) ;

          if ( ( ptg.X >= ext.XMin ) and ( ptg.X <= ext.XMax ) ) and
             ( ( ptg.Y >= ext.YMin ) and ( ptg.Y <= ext.YMax ) ) then begin
            if hasColor then
              pshp.AddVertex( ptg, TGIS_Color.FromBGR( lasFile.GetPointColor(0) ) )
            else
              pshp.AddPoint3D( ptg ) ;
          end
        end ;
      end
      else begin
        minmaxp   := lasFile.Extent ;
        leafCount := GisSingleVector( 500, 500, 50 ) ;

        leafSize := GisSingleVector( (minmaxp.XMax - minmaxp.XMin) / leafCount.X,
                                     (minmaxp.YMax - minmaxp.YMin) / leafCount.Y,
                                     (minmaxp.ZMax - minmaxp.ZMin) / leafCount.Z
                                    ) ;
        invLeafSize  := GisSingleVector( 1.0 / leafSize.X,
                                         1.0 / leafSize.Y,
                                         1.0 / leafSize.Z
                                        ) ;
        minmaxb := GisExtent3D( FloorS(minmaxp.XMin * invLeafSize.X),
                                FloorS(minmaxp.YMin * invLeafSize.Y),
                                FloorS(minmaxp.ZMin * invLeafSize.Z),
                                FloorS(minmaxp.XMax * invLeafSize.X),
                                FloorS(minmaxp.YMax * invLeafSize.Y),
                                FloorS(minmaxp.ZMax * invLeafSize.Z)
                               ) ;
        divb := GisSingleVector( minmaxb.XMax - minmaxb.XMin + 1,
                                 minmaxb.YMax - minmaxb.YMin + 1,
                                 minmaxb.ZMax - minmaxb.ZMin + 1
                               ) ;
        divb_mul := GisSingleVector(1, divb.X, divb.X * divb.Y) ;

        indices := TObjectList<T_voxel>.Create ;
        try
          numVox := TruncS( divb.X * divb.Y * divb.Z ) ;
          {$IFNDEF JAVA}
          indices.Capacity := numVox ;
          {$ENDIF}
          for i := 0 to numVox-1 do
            indices.Add( T_voxel.Create ) ;

          lasFile.MoveFirst(0, GisExtent2DFrom3D( lasFile.Extent ) ) ;
          while not lasFile.Eof(0) do begin

            lasFile.MoveNext(0) ;

            ptg := lasFile.GetPoint(0) ;

            k0  := TruncS( FloorS(ptg.X * invLeafSize.X) - minmaxb.XMin ) ;
            k1  := TruncS( FloorS(ptg.Y * invLeafSize.Y) - minmaxb.YMin ) ;
            k2  := TruncS( FloorS(ptg.Z * invLeafSize.Z) - minmaxb.ZMin ) ;
            idx := TruncS(k0 * divb_mul.X + k1 * divb_mul.Y + k2 * divb_mul.Z) ;

            if (idx >= 0) and (idx < indices.Count) then begin
              if hasColor then
                indices[idx].UpdateVoxel(
                  ptg,
                  TGIS_Color.FromBGR( lasFile.GetPointColor(0) ).ARGB
                )
              else
                indices[idx].UpdateVoxel( ptg, 0 ) ;
            end;

             inc( ihglass ) ;
             if ihglass mod GIS_PROGRESS_TRESHOLD = 0 then begin
               if HourglassShake then
                 break ;
             end ;
          end ;

          {$IFDEF GIS_NORECORDS}
          centroid := new TGIS_Point3D ;
          {$ENDIF}
          for i := 0 to indices.Count-1 do begin
            if indices[i].GetPoint( centroid, color ) then begin
              if hasColor then
                pshp.AddVertex( centroid, TGIS_Color.FromARGB( color ) )
              else
                pshp.AddPoint3D( centroid ) ;
            end ;

            if i mod GIS_PROGRESS_TRESHOLD = 0 then begin
              if HourglassShake then break ;
            end ;
          end ;
          numPointsFilterd := pshp.GetNumPoints ;
        finally
          FreeObject( indices ) ;
        end ;
      end ;
    finally
      pshp.Unlock ;
    end ;
  end ;

  procedure TGIS_LayerLAS.fset_UseRTree(
    const _value : Boolean
  ) ;
  begin
//    if not asPointCloud then
//      inherited fset_UseRTree( _value ) ;
  end ;

  function TGIS_LayerLAS.getFieldInternal(
    const _uid      : TGIS_Uid;
    const _name     : String ;
    const _cursor   : Integer
  ) : Variant ;
  begin
    Result := Unassigned ;

    lockThread ;
    try
      if asPointCloud or asPointCloudFilter then exit ;

      // if index file
      if assigned( lasFile ) then begin
        if lasFile.GetPointIndex( _cursor ) <> _uid then
          if not lasFile.LocatePoint( _cursor, _uid ) then
            exit ;
      end
      else exit ;

      if _name = 'Color' then
        Result := lasFile.GetPointColor( _cursor )
      else if _name = 'Intensity' then
        Result := lasFile.GetPointIntensity(_cursor)
      else if _name = 'Classification' then
        Result := lasFile.GetPointClassification(_cursor)
      else if _name = 'ScanAngleRank' then
        Result := lasFile.GetPointScanAngleRank(_cursor)
      else if _name = 'ReturnNumber' then
        Result := lasFile.GetPointReturnNumber(_cursor)
      else if _name = 'NumberOfReturns' then
        Result := lasFile.GetPointNumberOfReturns(_cursor)
      else if _name = 'ScanDirectionFlag' then
        Result := lasFile.GetPointScanDirectionFlag(_cursor)
      else if _name = 'EdgeOfFlightLine' then
        Result := lasFile.GetPointEdgeOfFlightLine(_cursor)
      else if _name = 'GPSTime' then
        Result := lasFile.GetPointGPSTime(_cursor)
      else if _name = 'ClassificationFlag' then
        Result := lasFile.GetPointClassificationFlag(_cursor)
      else if _name = 'ScannerChannel' then
        Result := lasFile.GetPointScannerChannel(_cursor)
      else
        Result := Unassigned ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerLAS.getBindedFieldInternal(
    const _shape  : TObject ;
    const _field  : Integer ;
    const _cursor : Integer
  ) : Variant ;
  var
    shp : TGIS_Shape ;
  begin
    Result := Unassigned ;
    if asPointCloud or asPointCloudFilter then exit ;

    shp := TGIS_Shape( _shape ) ;
    if not assigned( shp ) then Exit ;

    // if index file
    if assigned( lasFile ) then begin
      if lasFile.GetPointIndex( _cursor ) <> shp.Uid then
        if not lasFile.LocatePoint( _cursor, shp.Uid ) then
          exit ;
    end
    else exit ;

    lockThread ;
    try
      if _field = 0 then
        Result := lasFile.GetPointColor( _cursor )
      else if _field = 1 then
        Result := lasFile.GetPointIntensity(_cursor)
      else if _field = 2 then
        Result := lasFile.GetPointClassification(_cursor)
      else if _field = 3 then
        Result := lasFile.GetPointScanAngleRank(_cursor)
      else if _field = 4 then
        Result := lasFile.GetPointReturnNumber(_cursor)
      else if _field = 5 then
        Result := lasFile.GetPointNumberOfReturns(_cursor)
      else if _field = 6 then
        Result := lasFile.GetPointScanDirectionFlag(_cursor)
      else if _field = 7 then
        Result := lasFile.GetPointEdgeOfFlightLine(_cursor)
      else if _field = 8 then
        Result := lasFile.GetPointGPSTime(_cursor)
      else if _field = 9 then
        Result := lasFile.GetPointClassificationFlag(_cursor)
      else if _field = 10 then
        Result := lasFile.GetPointScannerChannel(_cursor)
      else
        Result := Unassigned ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerLAS.setUp ;
  var
    i : Integer ;
  begin
    inherited ;

    lastUid := -1 ;
    if not assigned( lasFile ) then
      lasFile := TGIS_FileLAS.Create( Path ) ;

    if not lasFile.Open then
      Abort ;

    for i := 0 to length( cursorLas )-1 do begin
      if cursorLas[i].curInUse then
        lasFile.CursorOpen( i ) ;
    end ;

    if CompareText( Driver, 'pc' ) = 0 then
      asPointCloud := True
    else if CompareText( Driver, 'pcf' ) = 0 then
      asPointCloudFilter := True ;

    if asPointCloud or asPointCloudFilter then begin
      FSubType          := FSubType + [ TGIS_LayerSubType.InMemory ] ;
      FSupportedShapes  := GisGetShapeType( TGIS_ShapeType.MultiPoint ) ;
    end
    else
      FSupportedShapes   := GisGetShapeType( TGIS_ShapeType.Point ) ;

    FSupportedDimensions := GisGetDimensionType( TGIS_DimensionType.XYZ ) ;

    Extent3D := lasFile.Extent ;
    CS := lasFile.CS ;

    if asPointCloud or asPointCloudFilter then
      buildPointCloud
    else begin
      Params.Marker.Size         := 2 ;
      Params.Marker.OutlineWidth := 0 ;

      AddFieldInternal( 'Color'             , TGIS_FieldType.Number, 18, 0 ) ;
      AddFieldInternal( 'Intensity'         , TGIS_FieldType.Number,  8, 0 ) ;
      AddFieldInternal( 'Classification'    , TGIS_FieldType.Number,  4, 0 ) ;
      AddFieldInternal( 'ScanAngleRank'     , TGIS_FieldType.Number,  8, 0 ) ;
      AddFieldInternal( 'ReturnNumber'      , TGIS_FieldType.Number,  4, 0 ) ;
      AddFieldInternal( 'NumberOfReturns'   , TGIS_FieldType.Number,  4, 0 ) ;
      AddFieldInternal( 'ScanDirectionFlag' , TGIS_FieldType.Number,  4, 0 ) ;
      AddFieldInternal( 'EdgeOfFlightLine'  , TGIS_FieldType.Number,  4, 0 ) ;
      AddFieldInternal( 'GPSTime'           , TGIS_FieldType.Number, 18, 8 ) ;
      AddFieldInternal( 'ClassificationFlag', TGIS_FieldType.Number,  4, 0 ) ;
      AddFieldInternal( 'ScannerChannel'    , TGIS_FieldType.Number,  4, 0 ) ;

      if lasFile.HasPointColor then
        Params.Marker.ColorAsText := 'FIELD:Color' ;
    end ;

    if SafeFileExists( Path ) then
      FAge := GisFileAge( Path ) ;

    FFileInfo := 'LiDAR LAS (ASPRS)' + #13#10 + lasFile.GetInfo ;
    if asPointCloudFilter then
      FFileInfo := FFileInfo + #13#10 +
                   Format( 'Number of filtered points : %d', [numPointsFilterd] ) ;
  end ;

  function TGIS_LayerLAS.fget_PathRTree : String ;
  begin
    Result := ExpandForcedRTreePath(
                Path + GIS_RTREE_EXT
              ) ;
  end ;

  function  TGIS_LayerLAS.cursorOpen :  Integer ;
  begin
    lockThread ;
    try
      Result := inherited cursorOpen ;

      if asPointCloud or asPointCloudFilter then exit ;

      if Result >= length( cursorLas )  then
        SetLength( cursorLas, Result + 1 ) ;
      {$IFDEF GIS_NORECORDS}
        if not assigned( cursorLas[Result] ) then
          cursorLas[Result] := new T_cursorLas ;
      {$ENDIF}
      cursorLas[Result].curInUse := True ;

      cursorLas[Result].curShapePoint := TGIS_ShapePoint.Create(
                                          nil, nil, False, -1, self,
                                          TGIS_DimensionType.XYZ
                                        ) ;
      if assigned( lasFile ) then
        lasFile.CursorOpen( Result );
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerLAS.cursorClose(
    const _cursor : Integer
  ) ;
  var
    i : Integer ;
  begin
    lockThread ;
    try
      if asPointCloud or asPointCloudFilter then begin
        inherited cursorClose( _cursor ) ;
        exit ;
      end ;

      cursorLas[_cursor].curInUse := False ;
      FreeObject( cursorLas[_cursor].curShapePoint ) ;

      // truncate cursorState at the tail;
      for i := length( cursorLas ) - 1 downto 0 do begin
        if not cursorLas[i].curInUse then begin
          SetLength( cursorLas, i ) ;
        end
        else
          break ;
      end ;

      inherited cursorClose( _cursor ) ;

      if assigned( lasFile ) then
        lasFile.CursorClose( _cursor );
    finally
      unlockThread ;
    end ;
  end ;


  procedure TGIS_LayerLAS.cursorFirst(
    const _cursor      : Integer          ;
    const _viewerCS    : Boolean          ;
    const _extent      : TGIS_Extent      ;
    const _query       : String           ;
    const _shape       : TGIS_Shape       ;
    const _de9im       : String           ;
    const _skipDeleted : Boolean
  ) ;
  begin
    lockThread ;
    try
      if asPointCloud or asPointCloudFilter then
        inherited cursorFirst(
                    _cursor, _viewerCS,
                    _extent, _query, _shape, _de9im, _skipDeleted
                  )
      else begin
        cursorLas[_cursor].curShape := nil ;

        if GisIsNoWorld( _extent ) then
          exit ;
          
        inherited cursorFirstInternal(
                    _cursor, _viewerCS,
                    _extent, _query, _shape, _de9im, _skipDeleted
                  ) ;

        if assigned( lasFile ) then
          lasFile.MoveFirst( _cursor, _extent ) ;

        cursorNext( _cursor ) ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerLAS.cursorNext(
    const _cursor : Integer
  ) ;
  var
    ihglass : Integer ;
  begin
    lockThread ;
    try
      if asPointCloud or asPointCloudFilter then
        inherited cursorNext( _cursor )
      else begin
        // read each shape
         ihglass := 0 ;
         while True do begin
           inc( ihglass ) ;
           if ihglass mod GIS_PROGRESS_TRESHOLD = 0 then begin
             if HourglassShake then begin
               cursorLas[_cursor].curShape := nil ;
               break ;
             end ;
           end ;

           cursorLas[_cursor].curShape := nil ;

           if assigned( lasFile ) then begin
             if not lasFile.Eof( _cursor ) then begin
               lasFile.MoveNext( _cursor ) ;
               readShape( _cursor ) ;
             end ;
           end ;

           if cursorLas[_cursor].curShape = nil then begin
             // using an xxxInternalVersion may be a bit too secure
             // but better is to be too restrictive than too lose
             if not inherited cursorEofInternal( _cursor ) then begin
               cursorLas[_cursor].curShape
                 := inherited cursorShapeInternal( _cursor ) ;
               inherited cursorNextInternal( _cursor ) ;
             end ;
             if cursorLas[_cursor].curShape = nil then exit ;
           end ;

           if cursorState[_cursor].curSkipDeleted and
              cursorLas[_cursor].curShape.IsDeleted then
           begin
             continue ;
           end ;

           if not isInScope( cursorLas[_cursor].curShape, _cursor ) then
             continue
           else
             exit ;
         end ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerLAS.cursorEof(
    const _cursor : Integer
  ) : Boolean ;
  begin
    if asPointCloud or asPointCloudFilter then
      Result := inherited cursorEof( _cursor )
    else
      Result := cursorLas[_cursor].curShape = nil ;
  end ;

  function TGIS_LayerLAS.cursorShape(
    const _cursor : Integer
  ) : TGIS_Shape ;
  begin
    if not asPointCloud and not asPointCloudFilter and
       assigned( cursorLas[_cursor].curShape )
    then
       Result := cursorLas[_cursor].curShape
    else
       Result := inherited cursorShape(_cursor) ;
  end ;

  procedure TGIS_LayerLAS.checkMode(
    const _path : String
  ) ;
  var
    mode : String  ;
    k    : Integer ;
  begin
    if not IsServerPath( _path ) then begin
      k := Pos( '?', _path ) ;
      if k = 0 then
        mode := ''
      else
        mode := Copy( _path, k+1, MaxInt ) ;
    end
    else
      mode := '' ;

    if CompareText( mode, 'pc' ) = 0 then
      asPointCloud := True
    else if CompareText( mode, 'pcf' ) = 0 then
      asPointCloudFilter := True ;
  end ;

  function TGIS_LayerLAS.GetShape(
    const _uid    : TGIS_Uid ;
    const _cursor : Integer
  ) : TGIS_Shape ;
  begin
    lockThread ;
    try
      Result := nil ;

      if _uid <= 0 then exit ;

      // if it is in edited list
      Result := inherited GetShape( _uid, _cursor ) ;
      if Result <> nil then exit ;
      if asPointCloud or asPointCloudFilter then exit ;

      if not assigned( lasFile ) then exit ;

      // is it a current shape
      if ( cursorLas[_cursor].curShape     <> nil ) and
         ( cursorLas[_cursor].curShape.Uid = _uid ) then
      begin
        Result := cursorLas[_cursor].curShape ;
        exit ;
      end ;

      // if index file
      if assigned( lasFile ) then begin
        if lasFile.LocatePoint( _cursor, _uid ) then begin
          readShape( _cursor ) ;
          Result := cursorLas[_cursor].curShape ;
        end ;
        exit ;
      end ;

      // if no index file, traverse normally
      inherited cursorStateSave( _cursor ) ;
      try
        cursorFirst( _cursor, False,
                     GisWholeWorld, '', nil, '', True
                   ) ;

        while not cursorEof( _cursor ) do begin
          if cursorShape(_cursor).Uid = _uid then begin
            Result := cursorShape(_cursor) ;
            exit ;
          end ;
          cursorNext(_cursor) ;
        end ;
      finally
        inherited cursorStateRestore( _cursor ) ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerLAS.GetLastUid : TGIS_Uid ;
  var
    shp       : TGIS_Shape ;
    old_scope : String     ;
  begin
    lockThread ;
    try
      if asPointCloud or asPointCloudFilter then
        Result := inherited GetLastUid
      else begin
        if assigned( lasFile ) then begin
          lastUid := Max( inherited GetLastUid, lasFile.GetPointsCount ) ;
          Result  := lastUid ;
        end
        else begin
          old_scope := Scope ;
          try
            if lastUid < 0 then begin
              shp := nil ;
              cursorFirst( 0, False,
                           GisWholeWorld, '', nil, '', True
                         ) ;

              while not cursorEof(0) do begin // iterate all shapes
                shp := cursorShape(0) ;
                cursorNext(0) ;
              end ;

              if assigned( shp ) then
                lastUid := shp.Uid
              else
                lastUid := 0 ;
            end ;

            Result := lastUid ;
          finally
            Scope := old_scope ;
          end ;
        end ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerLAS.DrawEx(
    const _extent : TGIS_Extent
  ) : Boolean ;
  var
    shp : TGIS_Shape ;
  begin
    if not asPointCloud then begin
      Result := inherited DrawEx( _extent ) ;
      exit ;
    end
    else begin
      Result := IsVisible( _extent ) ;
      if not Result then exit ;
    end ;

    // just a fake area covering the model to avoid drawing mess in gdi
    shp := TGIS_ShapePolygon.Create( nil, nil, False, -1, Self, TGIS_DimensionType.XY ) ;
    try
      shp.Lock( TGIS_Lock.Internal ) ;
      shp.AddPart ;
      shp.AddPoint( GisPoint( Extent.XMin, Extent.YMin ) ) ;
      shp.AddPoint( GisPoint( Extent.XMax, Extent.YMin ) ) ;
      shp.AddPoint( GisPoint( Extent.XMax, Extent.YMax ) ) ;
      shp.AddPoint( GisPoint( Extent.XMin, Extent.YMax ) ) ;
      shp.Unlock ;
      shp.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      shp.Draw ;
    finally
      FreeObject( shp ) ;
    end ;
  end ;

  function TGIS_LayerLAS.PreRecognize(
    const _path     : String ;
      var _new_path : String
  ) : Boolean ;
  var
    blaz : Boolean ;
  begin
    Result := inherited PreRecognize( _path, _new_path );

    checkMode( _path ) ;

    blaz  := CompareText( GetFileExt( _path ), '.laz' ) = 0 ;
    if blaz then begin
      if not assigned( lasFile ) then
        lasFile := TGIS_FileLAS.Create( _path ) ;
      try
        if not lasFile.Open then
          Result := False ;
      finally
        FreeObject( lasFile ) ;
      end ;
    end ;

  end ;

  { Perform initialization section.
  }
  class procedure Unit_GisLayerLAS.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-LAS', 'LiDAR LAS (ASPRS)', TGIS_LayerLAS, '.las;.laz',
                   TGIS_RegisteredLayerType.Vector,
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                    False
                  ) ;
  end ;

{$IFNDEF OXYGENE}
initialization
    Unit_GisLayerLAS.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.

