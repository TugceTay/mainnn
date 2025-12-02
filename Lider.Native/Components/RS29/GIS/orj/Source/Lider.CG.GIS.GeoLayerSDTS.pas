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
  Encapsulation of SDTS layer.

  TVP - Topological Vector Profile
  RPE - Raster Profile and Extensions
}

{$IFDEF DCC}
  unit GisLayerSDTS ;
  {$HPPEMIT '#pragma link "GisLayerSDTS"'}
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
    System.Classes,
    System.SysUtils,
    System.Types,

    GisTypes,
    GisFunctions,
    GisResource,
    GisLayerVector,    
    GisLayerPixel,
    GisFileSDTS,
    GisFile8211 ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerSDTS = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  /// <summary>
  ///   Encapsulation of SDTS TVP layer.
  /// </summary>
  TGIS_LayerSDTS_TVP = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    protected // properties internal values
      /// <summary>
      ///   SDTS reader.
      /// </summary>     
      oTransfer : TGIS_SDTSTransfer ;
      /// <summary>
      ///   SDTS index reader.
      /// </summary>       
      oReader   : TGIS_SDTSIndexedReader ;
    private
      /// <summary>
      ///   Sets the projection.
      /// </summary>    
      procedure setProjection         ;
      /// <summary>
      ///   Builds the layer.
      /// </summary> 
      /// <param name="_layer">
      ///   layer ID
      /// </param>
      procedure buildLayer            ( const _layer    : Integer
                                      ) ;

      /// <summary>
      ///   Add main attributes to the layer.
      /// </summary>
      /// <param name="_reader">
      ///   attribute reader
      /// </param>
      procedure addPrimaryAttrSchema  ( const _reader   : TGIS_SDTSAttrReader
                                      ) ;
      /// <summary>
      ///   Add attributes to a shape.
      /// </summary>
      /// <param name="_reader">
      ///   attribute reader
      /// </param>
      /// <param name="_feature">
      ///   SDTS feature
      /// </param>
      /// <param name="_shape">
      ///   shape
      /// </param>                          
      procedure writeAttrRecordToShape( const _reader   : TGIS_SDTSAttrRecord  ;
                                        const _feature  : TGIS_SDTSFeature     ;
                                        const _shape    : TGIS_Shape
                                       ) ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      /// <inheritdoc/>  
      procedure setUp                  ; override;
    protected
      /// <inheritdoc/>
      procedure doDestroy              ; override;
    public
      /// <inheritdoc/>
      constructor Create               ; override;

      /// <inheritdoc/>
      function  PreRecognize           ( const _path     : String ;
                                         var   _new_path : String
                                       ) : Boolean ; override;
  end ;

  /// <summary>
  ///   Encapsulation of SDTS RPE layer.
  /// </summary>
  TGIS_LayerSDTS_RPE = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerPixel )
    protected // properties internal values
      /// <summary>
      ///   SDTS reader.
      /// </summary>     
      oTransfer : TGIS_SDTSTransfer ;
      /// <summary>
      ///   SDTS raster reader.
      /// </summary>     
      poRL      : TGIS_SDTSRasterReader ;
      /// <summary>
      ///   Size of data.
      /// </summary>     
      dataSize  : Integer ;
    private
      /// <summary>
      ///   Sets the projection.
      /// </summary>        
      procedure setProjection          ;
      /// <summary>
      ///   Builds the layer.
      /// </summary> 
      /// <param name="_layer">
      ///   layer ID
      /// </param>      
      procedure buildRaster            ( const _layer : Integer
                                       ) ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF} // various protected routines
      /// <inheritdoc/>  
      procedure setUp                  ; override;

        /// <inheritdoc/>  
        function  getLine              ( const _buffer   : TBytes              ;
                                         const _offset   : Integer             ;
                                         const _linenr   : Integer             ;
                                         const _start    : Integer             ;
                                         const _bytes    : Integer
                                       ) : Integer; override;

      /// <inheritdoc/>  
      function  getNativeValue         ( const _pt       : TPoint              ;
                                         const _ar       : TGIS_DoubleArray
                                       ) : Boolean ; override;

      /// <inheritdoc/>  
      function  getNativeLine          ( const _buffer   : TGIS_SingleArray    ;
                                         const _linenr   : Integer             ;
                                         const _startIdx : Integer             ;
                                         const _count    : Integer
                                       ) : Integer ; override;
    protected
      /// <inheritdoc/>  
      procedure doDestroy              ; override;
    public
      /// <inheritdoc/>  
      constructor Create               ; override;

      /// <inheritdoc/>  
      function  PreRecognize           ( const _path     : String              ;
                                         var _new_path   : String
                                       ) : Boolean ; override;
  end ;
  

//##############################################################################
implementation

{$IFDEF DCC}
uses
  GisRtl,
  GisRegistredLayers,
  GisCsBase,
  GisTypesUI,
  GisClasses,
  GisInternals,
  GisCsSystems,
  GisCsProjections ;
{$ENDIF}

//==============================================================================
//  TGIS_LayerSDTS_TVP
//==============================================================================

  constructor TGIS_LayerSDTS_TVP.Create;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.InMemory
                           ] ;
  end ;


  procedure TGIS_LayerSDTS_TVP.doDestroy;
  begin

    inherited ;
  end ;


  procedure TGIS_LayerSDTS_TVP.setProjection ;
  var
    poXREF      : TGIS_SDTS_XREF ;
    datum_map   : TGIS_CSAbstract ;
    unit_map    : TGIS_CSAbstract ;
    proj_map    : TGIS_CSAbstract ;
    proj_param  : TGIS_CSProjParameters ;
  begin
    poXREF := oTransfer.XREF ;

    if poXREF.sDatum = 'NAS' then
      datum_map := CSGeographicCoordinateSystemList.ByEPSG( 4609 )
    else if poXREF.sDatum = 'NAX' then
      datum_map := CSGeographicCoordinateSystemList.ByEPSG( 4269 )
    else if poXREF.sDatum = 'WGC' then
      datum_map := CSGeographicCoordinateSystemList.ByEPSG( GIS_EPSG_WGS72 )
    else if poXREF.sDatum = 'WGE' then
      datum_map := CSGeographicCoordinateSystemList.ByEPSG( GIS_EPSG_WGS84 )
    else
      datum_map := CSGeographicCoordinateSystemList.ByEPSG( GIS_EPSG_WGS84 ) ;

    if poXREF.sSystemName = 'UTM' then begin
        // UTM
        proj_map   := CSProjList.ByEPSG( CSPROJ_Transverse_Mercator ) ;
        proj_param := CSProjectedCoordinateSystemList.DefaultParamsForUTM(
                        poXREF.nZone
                      ) ;
        unit_map := CSUnitsList.ByEPSG( 9001 ) ; // m
        CS := CSProjectedCoordinateSystemList.Prepare( -1, 'UTM',
                                                       datum_map.EPSG,
                                                       unit_map.EPSG,
                                                       proj_map.EPSG,
                                                       proj_param )
    end
    else if poXREF.sSystemName = 'GEO' then
      CS := CSGeographicCoordinateSystemList.ByEPSG( datum_map.EPSG )
    else
      CS := CSUnknownCoordinateSystem ;
  end ;


  procedure TGIS_LayerSDTS_TVP.addPrimaryAttrSchema(
    const _reader : TGIS_SDTSAttrReader
  ) ;
  var
    sf       : Integer  ;
    width    : Integer  ;

    ofld     : TGIS_DDFFieldDefn    ;
    osubfld  : TGIS_DDFSubfieldDefn ;
    sfldname : String               ;
    oattr    : TGIS_SDTSAttrRecord  ;
  begin
    if ( _reader <> nil ) then begin
      _reader.Rewind ;

      oattr := TGIS_SDTSAttrRecord( _reader.GetNextFeature ) ;
      if not assigned( oattr ) then
        exit ;

      try
        ofld := oattr.poATTR.FieldDefn ;
        if assigned( ofld  ) then begin
  
          for sf := 0 to ofld.SubfieldCount-1 do begin
            osubfld := ofld.GetSubfield( sf );
            width   := osubfld.Width ;
            if width = 0 then
              width := 1 ;

            sfldname := osubfld.Name ;

            if FindField( sfldname ) <> -1 then
              continue ;

            case osubfld.DataType of
              TGIS_DDFDataType.String :
                AddFieldInternal( sfldname, TGIS_FieldType.String, width, 0 );
              TGIS_DDFDataType.Int    :
                AddFieldInternal( sfldname, TGIS_FieldType.Number, width, 0 );
              TGIS_DDFDataType.Float  :
                AddFieldInternal( sfldname, TGIS_FieldType.Float , 10   , 4 );
            end ;
          end ;
        end ;
      finally
        FreeObject( oattr ) ;
      end ;
    end ;
  end ;


  procedure TGIS_LayerSDTS_TVP.buildLayer(
    const _layer : Integer
  ) ;
  var
    i             : Integer ;
    oattr_reader  : TGIS_SDTSAttrReader ;
    oattr         : TGIS_SDTSAttrRecord ;
    ofeature      : TGIS_SDTSFeature ;
    shp           : TGIS_Shape ;
    opoint        : TGIS_SDTSRawPoint ;
    oline         : TGIS_SDTSRawLine ;
    opoly         : TGIS_SDTSRawPolygon ;
    lst           : TStrings ;
    vrtx          : Integer ;
    rng           : Integer ;
  begin
    shp          := nil ;
    oattr_reader := nil ;

    oReader := oTransfer.GetLayerIndexedReader( _layer ) ;
    if not assigned( oReader ) then
      exit ;

    oReader.Rewind ;
    lst := oReader.ScanModuleReferences ;
    try
      if lst.Count > 0 then begin
        for i := 0 to lst.Count - 1 do begin
          oattr_reader := TGIS_SDTSAttrReader(
                            oTransfer.GetLayerIndexedReader(
                              oTransfer.FindLayer( lst[i] )
                            )
                          ) ;
          addPrimaryAttrSchema( oattr_reader ) ;

          oattr_reader.Rewind ;
        end ;
      end
      else
        oattr_reader := nil ;
    finally
      FreeObject( lst ) ;
    end ;

    if oTransfer.GetLayerType( _layer ) = TGIS_SDTSLayerType.Poly then
      TGIS_SDTSPolygonReader( oReader ).AssembleRings( oTransfer ) ;

    ofeature := oReader.GetNextFeature ;

    while assigned( ofeature ) do begin

      try
        case oTransfer.GetLayerType( _layer ) of
          TGIS_SDTSLayerType.Point :
            begin
              opoint := TGIS_SDTSRawPoint( ofeature ) ;
              shp := CreateShape( TGIS_ShapeType.Point, TGIS_DimensionType.XYZ ) ;
              shp.Lock( TGIS_Lock.Projection ) ;
              shp.AddPart ;
              shp.AddPoint3D( GisPoint3D( opoint.dfX[0],
                                          opoint.dfY[0],
                                          opoint.dfZ[0]
                                        )
                            ) ;
              shp.Unlock ;
            end ;
          TGIS_SDTSLayerType.Line  :
            begin
              oline  := TGIS_SDTSRawLine( ofeature ) ;
              shp := CreateShape( TGIS_ShapeType.Arc, TGIS_DimensionType.XYZ ) ;
              shp.Lock( TGIS_Lock.Projection ) ;
              shp.AddPart ;
              for i := 0 to oline.nVertices -1 do
                shp.AddPoint3D( GisPoint3D( oline.padfX[i],
                                            oline.padfY[i],
                                            oline.padfZ[i]
                                          )
                              ) ;
              shp.Unlock ;
             end ;
          TGIS_SDTSLayerType.Poly :
             begin
               opoly := TGIS_SDTSRawPolygon( ofeature ) ;
               shp := CreateShape( TGIS_ShapeType.Polygon, TGIS_DimensionType.XYZ ) ;
               for rng := 0 to opoly.nRings -1 do begin
                 shp.Lock( TGIS_Lock.Projection ) ;
                 shp.AddPart ;

                 if rng = ( opoly.nRings - 1 ) then
                   vrtx := opoly.nVertices - opoly.arRingStart[rng]
                 else
                   vrtx := opoly.arRingStart[rng + 1] -
                           opoly.arRingStart[rng    ] ;

                 for i := 0 to vrtx - 1 do
                   shp.AddPoint3D(
                     GisPoint3D( opoly.arX[ opoly.arRingStart[ rng ] + i ],
                                 opoly.arY[ opoly.arRingStart[ rng ] + i ],
                                 opoly.arZ[ opoly.arRingStart[ rng ] + i ]
                               )
                   ) ;
                 end ;
                 shp.Unlock ;
               end ;
        end ;

        if assigned( oattr_reader ) then begin
          oattr := TGIS_SDTSAttrRecord( oattr_reader.GetNextFeature ) ;
          try
            writeAttrRecordToShape( oattr, ofeature, shp ) ;
          finally
            FreeObject( oattr ) ;
          end ;
        end ;
      finally
        if ( not oReader.IsIndexed ) then
          FreeObject( ofeature ) ;
        ofeature := oReader.GetNextFeature ;
        shp := nil ;
      end ;
    end ;
  end ;


  procedure TGIS_LayerSDTS_TVP.writeAttrRecordToShape(
    const _reader  : TGIS_SDTSAttrRecord ;
    const _feature : TGIS_SDTSFeature    ;
    const _shape   : TGIS_Shape
   );
  var
    iAttrRecord   : Integer ;
    ardata        : TBytes  ;
    ofld          : TGIS_DDFFieldDefn    ;
    osubfld       : TGIS_DDFSubfieldDefn ;
    oattr         : TGIS_DDFField        ;
    isv           : Integer ;
    isf           : Integer ;
    svalue        : String  ;
    dvalue        : Double  ;
    nvalue        : Integer ;
    ntmp1         : Integer ;
    ntmp2         : Integer ;
  begin
    if ( _shape = nil ) or ( _feature = nil ) or ( _reader = nil ) then exit ;

    for iAttrRecord := 0 to _feature.nAttributes-1 do begin
      oattr := _reader.poATTR ;
      if not assigned( oattr ) then
        continue ;

      ofld := oattr.FieldDefn ;
      isv := 0 ;
      for isf := 0 to ofld.SubfieldCount-1 do begin

        osubfld := ofld.GetSubfield( isf );

        ardata := Copy( _reader.poWholeRecord.Data,
                        oattr.GetSubfieldData( _reader.poWholeRecord, osubfld, ntmp1, isv ),
                        length(_reader.poWholeRecord.Data)
                      ) ;
        case osubfld.DataType of
          TGIS_DDFDataType.String :
            begin
              svalue := oReader.DDFModule.DDFScanString(
                          osubfld.GetStringData( ardata, ntmp1, ntmp2 ),
                          0,
                          MaxInt
                         ) ;
              _shape.SetField( osubfld.Name, svalue ) ;
            end;
          TGIS_DDFDataType.Float  :
            begin
              dvalue := osubfld.GetFloatData( ardata, ntmp1, ntmp2 ) ;
              _shape.SetField( osubfld.Name, dvalue ) ;
            end;
          TGIS_DDFDataType.Int    :
            begin
              nvalue := osubfld.GetIntData( ardata, ntmp1, ntmp2 );
              _shape.SetField( osubfld.Name, nvalue ) ;
            end ;
        end ;
      end ;
    end ;
  end ;


  procedure TGIS_LayerSDTS_TVP.setUp ;
  var
    i : Integer ;
  begin
    try
      inherited ;

      oTransfer := TGIS_SDTSTransfer.Create ;
      Lock ;
      try
        if oTransfer.Open( Path ) then begin

          setProjection ;

          for i := 0 to oTransfer.LayerCount-1 do begin
            // ignore raster and attributes
            if oTransfer.GetLayerType( i ) = TGIS_SDTSLayerType.Raster then Continue ;

            buildLayer( i ) ;
          end ;
        end ;
      finally
        Unlock ;
        FreeObject( oTransfer ) ;
      end ;

      FFileInfo := 'SDTS (The Spatial Data Transfer Standard) Vector Layer' ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;
  

  function TGIS_LayerSDTS_TVP.PreRecognize(
    const _path     : String ;
    var   _new_path : String
  ) : Boolean ;
  var
    i     : Integer ;
    found : Boolean ;
  begin
    found := False ;

    oTransfer := TGIS_SDTSTransfer.Create ;
    try
      if oTransfer.Open( _path ) then begin
        for i := 0 to oTransfer.LayerCount-1 do begin
          if oTransfer.GetLayerType( i ) in [ TGIS_SDTSLayerType.Point, TGIS_SDTSLayerType.Line, TGIS_SDTSLayerType.Attr, TGIS_SDTSLayerType.Poly ] then
            found := True ;
        end
      end;
    finally
      FreeObject( oTransfer ) ;
    end;

    Result := inherited PreRecognize( _path, _new_path ) and found ;
  end ;


//==============================================================================
// TGIS_LayerSDTS_RPE
//==============================================================================

  constructor TGIS_LayerSDTS_RPE.Create;
  begin
    inherited;
    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.InMemory
                           ]  ;
    FIsNativeGridImage := True ;
  end ;


  procedure TGIS_LayerSDTS_RPE.doDestroy ;
  begin
    if oTransfer <> nil then
      FreeObject( oTransfer ) ;
    if poRL <> nil then
      FreeObject( poRL ) ;

    Dormant ;

    inherited ;
  end ;


  procedure TGIS_LayerSDTS_RPE.buildRaster(
    const _layer : Integer
  ) ;
  var
    ext  : TGIS_Extent {$IFDEF GIS_NORECORDS} = TGIS_Extent.create {$ENDIF};
  begin
    try
      poRL := oTransfer.GetLayerRasterReader( _layer ) ;

      if ( poRL = nil ) then exit ;

      FBitHeight    := poRL.YSize ;
      FBitWidth     := poRL.XSize ;
      poRL.GetScale( scaleX, scaleY ) ;
      FMinZ         := poRL.Min;
      FMaxZ         := poRL.Max;
      FNoDataValue  := poRL.Fill;
      dataSize      := poRL.GetRasterType ;
      Params.Pixel.GridNoValue := FNoDataValue ;

      poRL.GetExtent( ext.XMin, ext.YMin, ext.XMax, ext.YMax ) ;

      Extent := _TGIS_Extent(ext) ;

      IsGridImage := True ;
      FAntialias := True ;
      realBitCount  := 24;
      realLineWidth := FBitWidth ;
      intLineWidth  := realLineWidth ;

      // Transparency
      redTransp[0]    := BASE_TRANSPARENT_FLAG ;
      greenTransp[0]  := BASE_TRANSPARENT_FLAG ;
      blueTransp[0]   := BASE_TRANSPARENT_FLAG ;

      setProjection ;

      inherited setUp ;

      if SafeFileExists( Path ) then
        FAge := GisFileAge( Path ) ;

      FFileInfo := Format( 'SDTS (The Spatial Data Transfer Standard ) DEM Layer'
                           +#13#10 +
                           '%d x %d',
                           [FBitWidth, FBitHeight]
                         ) ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0) ;
    end ;

  end ;


  procedure TGIS_LayerSDTS_RPE.setProjection ;
  var
    oxref       : TGIS_SDTS_XREF  ;
    datum_map   : TGIS_CSAbstract ;
    unit_map    : TGIS_CSAbstract ;
    proj_map    : TGIS_CSAbstract ;
    proj_param  : TGIS_CSProjParameters ;
  begin
    oxref := oTransfer.XREF ;

    if oxref.sDatum = 'NAS' then
      datum_map := CSGeographicCoordinateSystemList.ByEPSG( 4267 )
    else if oxref.sDatum = 'NAX' then
      datum_map := CSGeographicCoordinateSystemList.ByEPSG( 4269 )
    else if oxref.sDatum = 'WGC' then
      datum_map := CSGeographicCoordinateSystemList.ByEPSG( GIS_EPSG_WGS72 )
    else if oxref.sDatum = 'WGE' then
      datum_map := CSGeographicCoordinateSystemList.ByEPSG( GIS_EPSG_WGS84 )
    else
      datum_map := CSGeographicCoordinateSystemList.ByEPSG( GIS_EPSG_WGS84 ) ;

    if oxref.sSystemName = 'UTM' then begin
        // UTM
        proj_map   := CSProjList.ByEPSG( CSPROJ_Universal_Transverse_Mercator ) ;
        proj_param := CSProjectedCoordinateSystemList.DefaultParams(
                        CSPROJ_Universal_Transverse_Mercator
                      ) ;
        unit_map := CSUnitsList.ByEPSG( 9001 ) ; // m
        proj_param.Zone := oxref.nZone ;
        CS := CSProjectedCoordinateSystemList.Prepare( -1,
                                                       Format('UTM%d',[oxref.nZone]),
                                                       datum_map.EPSG,
                                                       unit_map.EPSG,
                                                       proj_map.EPSG,
                                                       proj_param )
    end
    else if oxref.sSystemName = 'GEO' then
      CS := CSGeographicCoordinateSystemList.ByEPSG( datum_map.EPSG )
    else
      CS := CSUnknownCoordinateSystem ;
  end ;


  procedure TGIS_LayerSDTS_RPE.setUp  ;
  var
    i : Integer ;
  begin
    inherited ;

    oTransfer := TGIS_SDTSTransfer.Create ;

    if oTransfer.Open( Path ) then begin

      for i := 0 to oTransfer.LayerCount-1 do begin
        // ignore raster and attributes
        if oTransfer.GetLayerType( i ) <> TGIS_SDTSLayerType.Raster then Continue ;

        buildRaster( i ) ;
      end
    end ;

    if SafeFileExists( Path ) then
      FAge := GisFileAge( Path ) ;
  end ;


  function TGIS_LayerSDTS_RPE.getLine(
    const _buffer : TBytes  ;
    const _offset : Integer ;
    const _linenr : Integer ;
    const _start  : Integer ;
    const _bytes  : Integer
  ) : Integer ;
  var
    i       : Integer;
    start   : Integer ;
    pixels  : Integer ;
    offset  : Integer ;
    buf     : TBytes ;
    {$IFDEF OXYGENE}
      pvals   : SmallInt ;
      pos     : Integer ;
      psvals  : Single ;
    {$ELSE}
      pvals   : PSmallInt ;
      psvals  : PSingle ;
     {$ENDIF}
    ccolor  : TGIS_Color ;
  begin
    try
      Result   := _bytes ;
      pixels   := _bytes div 3 ;
      start    := _start div 3 ;
      {$IFDEF OXYGENE}
        pvals  := 0 ;
        psvals := 0 ;
      {$ENDIF}

      if _linenr >= FBitHeight then
        exit ;

      offset := 0 ;

      poRL.GetBlock( start, _linenr, buf ) ;
      if dataSize = 2 then begin
        {$IFDEF OXYGENE}
         pos := start*dataSize ;
        {$ELSE}
         pvals := PSmallInt( @buf[start*dataSize]) ;
        {$ENDIF}

        for i := start to start +pixels -2 do begin
         {$IFDEF OXYGENE}
          pvals := BitConverter.ToInt16( buf, pos ) ;
          if pvals = FNoDataValue then begin
            _buffer[ _offset +offset   ] := colorNoData.R ;
            _buffer[ _offset +offset+1 ] := colorNoData.G ;
            _buffer[ _offset +offset+2 ] := colorNoData.B ;
            makeTransparent := True ;
          end
          else begin
            ccolor := GetColorRamp( pvals ) ;
            _buffer[ _offset +offset    ] := ccolor.R ;
            _buffer[ _offset +offset + 1] := ccolor.G ;
            _buffer[ _offset +offset + 2] := ccolor.B ;
          end ;
          inc( pos, dataSize ) ;
         {$ELSE}
          if pvals^ = FNoDataValue then begin
            PInteger( NativeInt(_buffer) +offset )^ := colorNoData.ARGB ;
            makeTransparent := True ;
          end
          else
            PInteger( NativeInt(_buffer) +offset )^ := GetColorRamp( pvals^ ).ARGB ;

          Inc( pvals ) ;
         {$ENDIF}
          offset := offset +3 ;
        end ;

        {$IFDEF OXYGENE}
         // for last triple
         if pvals = FNoDataValue then begin
           makeTransparent := True ;
           ccolor := colorNoData ;
         end
         else
           ccolor := GetColorRamp( pvals ) ;
        {$ELSE}
         // for last triple
         if pvals^ = FNoDataValue then begin
           makeTransparent := True ;
           ccolor := colorNoData ;
         end
         else
           ccolor := GetColorRamp( pvals^ ) ;
        {$ENDIF}
      end
      else begin
        {$IFDEF OXYGENE}
         pos := start*dataSize ;
        {$ELSE}
         psvals := PSingle( @buf[start*dataSize]) ;
        {$ENDIF}

        for i := start to start +pixels -2 do begin
         {$IFDEF OXYGENE}
           psvals := BitConverter.ToSingle( buf, pos ) ;
           if psvals = FNoDataValue then begin
             _buffer[ _offset +offset   ] := colorNoData.R ;
             _buffer[ _offset +offset+1 ] := colorNoData.G ;
             _buffer[ _offset +offset+2 ] := colorNoData.B ;
             makeTransparent := True ;
           end
           else begin
             ccolor := GetColorRamp( psvals ) ;
             _buffer[ _offset +offset    ] := ccolor.R ;
             _buffer[ _offset +offset + 1] := ccolor.G ;
             _buffer[ _offset +offset + 2] := ccolor.B ;
           end ;
           inc( pos, dataSize ) ;
         {$ELSE}
           if psvals^ = FNoDataValue then begin
             PInteger( NativeInt(_buffer) +offset )^ := colorNoData.ARGB ;
             makeTransparent := True ;
           end
           else
             PInteger( NativeInt(_buffer) +offset )^ := GetColorRamp( psvals^ ).ARGB ;

           Inc( psvals ) ;
         {$ENDIF}
          offset := offset +3 ;
        end ;

        {$IFDEF OXYGENE}
         // for last triple
         if psvals = FNoDataValue then begin
           makeTransparent := True ;
           ccolor := colorNoData ;
         end
         else
           ccolor := GetColorRamp( psvals ) ;
        {$ELSE}
        // for last triple
        if psvals^ = FNoDataValue then begin
          makeTransparent := True ;
          ccolor := colorNoData ;
        end
        else
          ccolor := GetColorRamp( psvals^ ) ;
        {$ENDIF}
      end;

      _buffer[_offset + offset    ] := ccolor.B ;
      _buffer[_offset + offset + 1] := ccolor.G ;
      _buffer[_offset + offset + 2] := ccolor.R ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;


  function TGIS_LayerSDTS_RPE.getNativeLine(
    const _buffer   : TGIS_SingleArray ;
    const _linenr   : Integer          ;
    const _startIdx : Integer          ;
    const _count    : Integer
  ) : Integer ;
  var
    i       : Integer;
    buf     : TBytes ;
    {$IFDEF OXYGENE}
     pos, off: Integer ;
    {$ELSE}
     pvals   : PSmallInt ;
     psvals  : PSingle ;
     sbuffer : PSingle ;
    {$ENDIF}
  begin
    try
      {$IFDEF OXYGENE}
       off := 0 ;
      {$ELSE}
       sbuffer := PSingle( _buffer ) ;
      {$ENDIF}

      poRL.GetBlock( _startIdx, _linenr, buf ) ;

      if dataSize = 2 then begin
      {$IFDEF OXYGENE}
        pos := _startIdx*dataSize ;
      {$ELSE}
        pvals := PSmallInt( @buf[_startIdx*dataSize]) ;
      {$ENDIF}

        for i := _startIdx to _startIdx + _count -1 do begin
         {$IFDEF OXYGENE}
           _buffer[ off ] := buf[pos] + (buf[pos+1] shl 8) ;
           inc( pos, dataSize ) ;
           inc( off ) ;
         {$ELSE}
           PSingle( sbuffer )^ := pvals^ ;
           Inc( pvals ) ;
           Inc( sbuffer ) ;
         {$ENDIF}
        end ;
      end
      else begin
       {$IFDEF OXYGENE}
        pos := _startIdx*dataSize ;
       {$ELSE}
        psvals := PSingle( @buf[_startIdx*dataSize]) ;
       {$ENDIF}

        for i := _startIdx to _startIdx + _count -1 do begin
        {$IFDEF OXYGENE}
           _buffer[ off ] := BitConverter.ToSingle( buf, pos ) ;
           inc( pos, dataSize ) ;
           inc( off ) ;
        {$ELSE}
          PSingle( sbuffer )^ := psvals^ ;
          Inc( psvals ) ;
          Inc( sbuffer ) ;
        {$ENDIF}
        end ;
      end;
      Result := _count ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;
  

  function TGIS_LayerSDTS_RPE.getNativeValue(
    const _pt : TPoint  ;
    const _ar : TGIS_DoubleArray
  ) : Boolean ;
  var
    buf : TBytes ;
    idx : Integer ;
  begin
    try
      Result := True ;

      poRL.GetBlock( _pt.X, _pt.Y, buf ) ;
      idx := _pt.X * dataSize ;
      if dataSize = 2 then
        _ar[ 0 ] := buf[idx] + (buf[idx+1] shl 8)
      else
        {$IFDEF OXYGENE}
         _ar[ 0 ] := BitConverter.ToSingle( buf, idx ) ;
        {$ELSE}
         _ar[ 0 ] := PSingle( @buf[idx] )^ ;
        {$ENDIF}
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;


  function TGIS_LayerSDTS_RPE.PreRecognize(
    const _path     : String ;
    var   _new_path : String
  ) : Boolean ;
  var
    iLayer : Integer ;
    bFound : Boolean ;
  begin
    bFound := False ;

    oTransfer := TGIS_SDTSTransfer.Create ;
    try
      if oTransfer.Open( _path ) then begin
        for iLayer := 0 to oTransfer.LayerCount-1 do begin
          if oTransfer.GetLayerType( iLayer ) = TGIS_SDTSLayerType.Raster then
            bFound := True ;
        end
      end;
    finally
      FreeObject( oTransfer ) ;
    end;

    Result := inherited PreRecognize( _path, _new_path ) and bFound ;
  end ;
  
  
//==============================================================================
// Unit_GisLayerSDTS
//==============================================================================  

  class procedure Unit_GisLayerSDTS.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-SDTS_TVP', 'SDTS Topological Vector Profile',
                   TGIS_LayerSDTS_TVP, '.ddf',
                   TGIS_RegisteredLayerType.Vector,
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                   False
                 ) ;
    RegisterLayer( 'DK-SDTS_RPE', 'SDTS Raster Profile and Extensions',
                   TGIS_LayerSDTS_RPE, '.ddf',
                   TGIS_RegisteredLayerType.Pixel,
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                   False
                 ) ;
  end ;


//==============================================================================
// initialization/finalization
//==============================================================================

{$IFDEF DCC}
  initialization
    Unit_GisLayerSDTS.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.
