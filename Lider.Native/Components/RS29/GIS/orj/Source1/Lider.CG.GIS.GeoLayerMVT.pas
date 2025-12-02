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
  Encapsulation of Mapbox Vector Tiles format support.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerMVT ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerMVT"'}
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
    TatukGIS.RTL,
    TatukGIS.NDK.Common ;
{$ENDIF}
{$IFDEF DCC}
uses
  {$IFDEF DEBUG_WEBTILES}
    VCL.Graphics,
  {$ENDIF}
  System.Classes,
  System.SysUtils,
  System.Variants,
  System.Generics.Collections,
  System.Generics.Defaults,
  Lider.CG.GIS.GeoLayerCompound,
  Lider.CG.GIS.GeoFileGPB,
  Lider.CG.GIS.GeoLayerPixel,
  Lider.CG.GIS.GeoLayerVector,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoLayer,
  Lider.CG.GIS.GeoClasses,
  Lider.CG.GIS.GeoStreams ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  GisLayerMVT = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  {#gendoc:hide}
  T_MVTValueType = (
      vtNone,
      vtString,
      vtFloat,
      vtDouble,
      vtInt,
      vtUInt,
      vtSInt,
      vtBool,
      vtStringS
  ) ;

  {#gendoc:hide}
  T_MVTValue = record
    VType   : T_MVTValueType ;
    VValue  : Variant ;
  end ;

  {#gendoc:hide}
  T_MVTGeomType = (
    Unknown    = 0,
    Point      = 1,
    LineString = 2,
    Polygon    = 3
  ) ;

  {#gendoc:hide}
  T_MVTFeature = class
    public
      Uid      : Int64 ;
      GType    : T_MVTGeomType ;
      KeyVals  : TList<Cardinal> ;
      Geometry : TList<Cardinal> ;
    public
      constructor Create ;
      {$IFNDEF OXYGENE}
      destructor  Destroy ; override;
      {$ENDIF}

      procedure AddKeyVal(
        const _key : Cardinal ;
        const _val : Cardinal
      ) ;
      procedure AddGeometry(
        const _geo : Cardinal
      ) ;
      procedure SetGType(
        const _gtype : Cardinal
      ) ;
  end ;

  {#gendoc:hide}
  T_MVTLayer = class
    public
      Name      : String ;
      Version   : Cardinal ;
      Extent    : Cardinal ;
      Features  : TList<T_MVTFeature> ;
      Values    : TList<T_MVTValue> ;
      Keys      : TList<String> ;
      Order     : Integer ;
    public
      constructor Create ;
      {$IFNDEF OXYGENE}
      destructor  Destroy ; override;
      {$ENDIF}

      procedure AddKey(
        const _key : String
      ) ;
      procedure AddValue(
        const _vt  : T_MVTValueType ;
        const _val : Variant
      ) ;
      procedure AddFeature(
        const _fea : T_MVTFeature
      ) ;
  end ;

  /// <summary>
  ///   Encapsulation of MVT layer.
  /// </summary>
  TGIS_LayerMVT = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerCompoundVector )
    private
      const
        VT_LAYER                 = 3 ;
        VT_LAYER_NAME            = 1 ;
        VT_LAYER_FEATURES        = 2 ;
        VT_LAYER_KEYS            = 3 ;
        VT_LAYER_VALUES          = 4 ;
        VT_LAYER_EXTENT          = 5 ;
        VT_LAYER_VERSION         = 15 ;

        VT_VALUE_STRING          = 1 ;
        VT_VALUE_FLOAT           = 2 ;
        VT_VALUE_DOUBLE          = 3 ;
        VT_VALUE_INT             = 4 ;
        VT_VALUE_UINT            = 5 ;
        VT_VALUE_SINT            = 6 ;
        VT_VALUE_BOOL            = 7 ;

        VT_FEATURE_ID            = 1 ;
        VT_FEATURE_TAGS          = 2 ;
        VT_FEATURE_TYPE          = 3 ;
        VT_FEATURE_GEOMETRY      = 4 ;

        VT_GEOM_TYPE_UNKNOWN     = 0 ;
        VT_GEOM_TYPE_POINT       = 1 ;
        VT_GEOM_TYPE_LINESTRING  = 2 ;
        VT_GEOM_TYPE_POLYGON     = 3 ;

        VT_CMD_MOVETO            = 1 ;
        VT_CMD_LINETO            = 2 ;
        VT_CMD_CLOSEPATH         = 7 ;

        V_TDEFAULT_EXTENT        = 4096 ;
    private
      FGeoreferenced   : Boolean ;
      FLayerFilter     : String ;
      lstLayers        : TList<T_MVTLayer> ;
      arData           : TBytes ;
      iDataSize        : Integer ;
      iLevel           : Integer ;
      dctLayersOrder   : TDictionary<String,Integer> ;
      oStyler          : TObject ;
    private
      procedure loadData ;
      procedure readTile ;
      procedure parseTile ;

      procedure addLayer(
        const _layer : T_MVTLayer
      ) ;
      procedure readLayer(
        const _layer : T_MVTLayer ;
        const _rdr   : TGIS_GPBReader ;
        const _limit : Integer
      ) ;
      procedure readFeature(
        const _feature : T_MVTFeature ;
        const _rdr     : TGIS_GPBReader ;
        const _limit   : Integer
      ) ;
      procedure readValues(
        const _layer : T_MVTLayer ;
        const _rdr   : TGIS_GPBReader ;
        const _limit : Integer
      ) ;
      function getCmdId(
        const _cmd : Cardinal
      ) : Cardinal ; {$IFDEF GIS_INLINE} inline ; {$ENDIF}
      function getCmdCount(
        const _cmd : Cardinal
      ) : Cardinal ; {$IFDEF GIS_INLINE} inline ; {$ENDIF}
      procedure getPtg(
        const _layer : T_MVTLayer ;
        const _nx    : Integer ;
        const _ny    : Integer ;
          var _ptg   : TGIS_Point
      ) ;
      {$IFDEF OXYGENE}
      procedure doPaintShape( _sender : Object; _e : TGIS_ShapeEventArgs ) ;
      {$ELSE}
      procedure doPaintShape( _sender : TObject; _shape : TGIS_Shape );
      {$ENDIF}

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // for internal use of TGIS_Viewer

      /// <inheritdoc/>
      procedure setUp         ; override;
    protected
      // destructor

      /// <inheritdoc/>
      procedure doDestroy ; override;
    public
      // constructors

      /// <inheritdoc/>
      constructor Create ; override;

      /// <inheritdoc/>
      function  PreRecognize  ( const _path        : String           ;
                                var   _new_path    : String
                              ) : Boolean ; override;

    public
      /// <summary>
      ///   Filter a dataset by layer name.
      /// </summary>
      property LayerFilter : String     read  FLayerFilter
                                        write FLayerFilter ;

      /// <summary>
      ///   If True, geometry coordinates will be based on WebMercator (EPSG:3857).
      /// </summary>
      property Georeferenced : Boolean  read  FGeoreferenced
                                        write FGeoreferenced ;
      {$IFDEF GIS_TEST}
      property Level : Integer          read  iLevel
                                        write iLevel ;
      {$ENDIF}
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Encapsulation of tiled MVT pixel layer.
  /// </summary>
  /// <remarks>
  ///   <note type="caution">
  ///     Only for internal use of TatukGIS.
  ///   </note>
  /// </remarks>
  TGIS_LayerMVTPixel = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerPixel )
    private
      iLevel  : Integer ;
      oStyler : TObject ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      /// <inheritdoc/>
      procedure setUp ; override;
    protected
      /// <inheritdoc/>
      procedure doDestroy ; override;
    public
      /// <summary>
      ///   Class constructor
      /// </summary>
      /// <param name="_level">
      ///   tile level which object will represent
      /// </param>
      constructor Create      ( const _level    : Integer
                              ) ; overload;
      /// <inheritdoc/>
      function  GetRawBitmap  ( const _extent   : TGIS_Extent ;
                                const _bitmap   : TGIS_Pixels ;
                                const _width    : Integer ;
                                const _height   : Integer
                              ) : Boolean ; override;
      /// <summary>
      ///   Set renderer size for vector tile and prepare a bitmap
      /// </summary>
      /// <param name="_size">
      ///   size of rendered bitmap in pixels
      /// </param>
      /// <param name="_styler">
      ///   styler used to rendere vector tile
      /// </param>
      procedure RenderSize    ( const _size     : Integer ;
                                const _styler   : TObject
                              ) ;
  end ;


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Math,
    Lider.CG.GIS.GeoRegistredLayers,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoParams,
    Lider.CG.GIS.GeoSymbol,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoMVTStyler,
    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.GeoCompression ;
{$ENDIF}

//=============================================================================
// T_MVTLayer
//=============================================================================

  constructor T_MVTLayer.Create ;
  begin
    inherited ;

    Name      := '' ;
    Version   := 0 ;
    Extent    := 4096 ;
    Features  := TList<T_MVTFeature>.Create ;
    Values    := TList<T_MVTValue>.Create ;
    Keys      := TList<String>.Create ;
  end ;

  {$IFNDEF OXYGENE}
  destructor T_MVTLayer.Destroy ;
  var
    fea : T_MVTFeature ;
  begin
    for fea in Features do
      FreeObjectNotNil( fea ) ;
    FreeObject( Features ) ;
    FreeObject( Values   ) ;
    FreeObject( Keys     ) ;

    inherited ;
  end ;
  {$ENDIF}

  procedure T_MVTLayer.AddValue(
    const _vt  : T_MVTValueType ;
    const _val : Variant
  ) ;
  var
    val : T_MVTValue ;
  begin
    val.VType  := _vt ;
    val.VValue := _val ;
    Values.Add( val ) ;
  end ;

  procedure T_MVTLayer.AddKey(
    const _key : String
  ) ;
  begin
    Keys.Add( _key ) ;
  end ;

  procedure T_MVTLayer.AddFeature(
    const _fea : T_MVTFeature
  ) ;
  begin
    Features.Add( _fea ) ;
  end ;

//=============================================================================
// T_MVTFeature
//=============================================================================

  constructor T_MVTFeature.Create ;
  begin
    inherited ;

    Uid      := 0 ;
    GType    := T_MVTGeomType.Unknown ;
    KeyVals  := TList<Cardinal>.Create ;
    Geometry := TList<Cardinal>.Create ;
  end ;

  {$IFNDEF OXYGENE}
  destructor T_MVTFeature.Destroy ;
  begin
    FreeObject( KeyVals  ) ;
    FreeObject( Geometry ) ;

    inherited ;
  end ;
  {$ENDIF}

  procedure T_MVTFeature.AddGeometry(
    const _geo: Cardinal
  ) ;
  begin
    Geometry.Add( _geo ) ;
  end ;

  procedure T_MVTFeature.AddKeyVal(
    const _key : Cardinal ;
    const _val : Cardinal
  ) ;
  begin
    KeyVals.Add( _key ) ;
    KeyVals.Add( _val ) ;
  end ;

  procedure T_MVTFeature.SetGType(
    const _gtype : Cardinal
  ) ;
  begin
    case _gtype of
      1 : GType := T_MVTGeomType.Point      ;
      2 : GType := T_MVTGeomType.LineString ;
      3 : GType := T_MVTGeomType.Polygon    ;
    else  GType := T_MVTGeomType.Unknown    ;
    end ;
  end ;

//=============================================================================
// TGIS_MVTTile
//=============================================================================

  constructor TGIS_LayerMVT.Create ;
  begin
    inherited ;

    FGeoreferenced := True ;
    FLayerFilter   := '' ;
    lstLayers      := TList<T_MVTLayer>.Create ;
    iLevel         := 0 ;
    dctLayersOrder := TDictionary<String,Integer>.Create ;
    oStyler        := nil ;
  end ;

  procedure TGIS_LayerMVT.doDestroy ;
  {$IFDEF DCC}
  var
    layer : T_MVTLayer ;
  {$ENDIF}
  begin
    for layer in lstLayers do
      FreeObjectNotNil( layer ) ;

    FreeObject( lstLayers ) ;
    FreeObject( dctLayersOrder ) ;

    inherited ;
  end ;

  procedure TGIS_LayerMVT.setUp ;
  begin
    inherited;

    if FGeoreferenced then begin
      // Web Mercator
      SetCSByEPSG( 3857 ) ;
    end ;

    loadData  ;
    readTile  ;
    parseTile ;

    ReadConfig ;

    RecalcProjectedExtent ;
  end ;

  procedure TGIS_LayerMVT.addLayer(
    const _layer : T_MVTLayer
  ) ;
  begin
    lstLayers.Add( _layer ) ;
  end ;

  function TGIS_LayerMVT.getCmdId(
    const _cmd : Cardinal
  ) : Cardinal ;
  begin
    Result := _cmd and 7 ;
  end ;

  function TGIS_LayerMVT.getCmdCount(
    const _cmd : Cardinal
  ) : Cardinal ;
  begin
    Result := _cmd shr 3 ;
  end ;

  procedure TGIS_LayerMVT.getPtg(
    const _layer : T_MVTLayer ;
    const _nx    : Integer ;
    const _ny    : Integer ;
      var _ptg   : TGIS_Point
  ) ;
  var
    dext : Double ;
  begin
    {$IFDEF GIS_NORECORDS}
      _ptg := new TGIS_Point ;
    {$ENDIF}

    dext := _layer.Extent ;
    if FGeoreferenced then begin
      _ptg.X := Extent.XMin + _nx * (Extent.XMax - Extent.XMin) / dext ;
      _ptg.Y := Extent.YMax - _ny * (Extent.YMax - Extent.YMin) / dext ;
    end
    else begin
      _ptg.X := _nx ;
      _ptg.Y := dext - _ny ;
    end ;
  end ;

  procedure TGIS_LayerMVT.loadData ;
  var
    fs        : TGIS_BufferedFileStream ;
    oStream   : TStream ;
    r         : TGIS_HttpResponse ;

    procedure checkData( const _stm : TStream ) ;
    begin
      SetLength( arData, 2 ) ;
      _stm.Read( arData, 2 ) ;

      if ( arData[0] = $1F ) and ( arData[1] = $8B ) then begin
        oStream := DecompressGZipStream( _stm ) ;
        try
          oStream.Position := 0 ;
          SetLength( arData, oStream.Size ) ;
          oStream.Read( arData, oStream.Size ) ;
        finally
          FreeObject( oStream ) ;
        end ;
      end
      else if ( arData[0] = $78 ) and ( arData[1] = $9C ) then begin
        oStream := DecompressDeflateStream( _stm ) ;
        try
          oStream.Position := 0 ;
          SetLength( arData, oStream.Size ) ;
          oStream.Read( arData, oStream.Size ) ;
        finally
          FreeObject( oStream ) ;
        end ;
      end
      else begin
        SetLength( arData, _stm.Size ) ;
        _stm.Position := 0 ;
        _stm.Read( arData, _stm.Size ) ;
      end ;
    end ;

  begin
    arData := nil ;
    iDataSize := 0 ;

    if assigned( Stream ) then begin
      Stream.Position := 0 ;
      checkData( Stream ) ;
    end
    else begin
      if ( Pos( 'http:', Path  ) = StringFirst ) or
         ( Pos( 'https:', Path ) = StringFirst ) then begin

        r.Stream := nil ;
        try
          r := TGIS_WebUtils.HttpFetch( Path, nil, nil, True, 40000,
                                        GetDefaultUserAgent( 'ttkWP' ), '', '', ''
                                      ) ;
          if r.Status = GIS_HTTP_OK then begin
            checkData( r.Stream ) ;
          end ;
        finally
          FreeObject( r.Stream ) ;
        end ;
      end
      else begin
        if SafeFileExists( Path ) then begin
          fs := TGIS_BufferedFileStream.Create( Path, TGIS_StreamMode.Read ) ;
          try
            checkData( fs ) ;
          finally
            FreeObject( fs ) ;
          end ;
        end ;
      end ;
    end ;

    iDataSize := length( arData ) ;
  end ;

  procedure TGIS_LayerMVT.readTile ;
  var
    gpb       : TGIS_GPBReader ;
    key       : Integer ;
    layersize : Integer ;
    lv        : T_MVTLayer ;
  begin
    gpb := TGIS_GPBReader.Create( arData, iDataSize ) ;
    try
      while ( gpb.Position < iDataSize ) do begin

        key := gpb.ReadFieldKey ;

        if ( key = gpb.MakeKey( VT_LAYER, gpb.WT_DATA ) ) then begin
          layersize := gpb.ReadSize ;

          lv := T_MVTLayer.Create ;
          readLayer( lv, gpb, gpb.Position+layersize ) ;
          if Not IsStringEmpty( FLayerFilter ) and ( lv.Name <> FLayerFilter ) then
            FreeObject( lv )
          else
            addLayer( lv ) ;
        end
        else
          gpb.SkipUnknownField( key, False ) ;
      end ;
    finally
      FreeObject( gpb ) ;
    end ;
  end ;

  procedure TGIS_LayerMVT.readLayer(
    const _layer : T_MVTLayer ;
    const _rdr   : TGIS_GPBReader   ;
    const _limit : Integer
  ) ;
  var
    key     : Integer  ;
    feasize : Cardinal ;
    valsize : Cardinal ;
    iext    : Cardinal ;
    iver    : Cardinal ;
    kname   : String   ;
    fea     : T_MVTFeature ;
  begin
    while ( _rdr.Position < _limit ) do begin

      key := _rdr.ReadFieldKey ;

      if ( key = _rdr.MakeKey( VT_LAYER_NAME, _rdr.WT_DATA ) ) then begin
        _layer.Name := _rdr.ReadText ;
      end
      else if ( key = _rdr.MakeKey( VT_LAYER_FEATURES, _rdr.WT_DATA ) ) then begin
        feasize := _rdr.ReadSize ;
        fea     := T_MVTFeature.Create ;
        _layer.AddFeature( fea ) ;
        readFeature( fea, _rdr, _rdr.Position+feasize ) ;
      end
      else if ( key = _rdr.MakeKey( VT_LAYER_KEYS, _rdr.WT_DATA ) ) then begin
        kname := _rdr.ReadText ;
        _layer.AddKey( kname ) ;
      end
      else if ( key = _rdr.MakeKey( VT_LAYER_VALUES, _rdr.WT_DATA ) ) then begin
        valsize := _rdr.ReadSize ;
        readValues( _layer, _rdr, _rdr.Position+valsize ) ;
      end
      else if ( key = _rdr.MakeKey( VT_LAYER_EXTENT, _rdr.WT_VARINT ) ) then begin
        iext := _rdr.readVarUInt32 ;
        _layer.Extent := iext ;
      end
      else if ( key = _rdr.MakeKey( VT_LAYER_VERSION, _rdr.WT_VARINT ) ) then begin
        iver := _rdr.readVarUInt32 ;
        _layer.Version := iver ;
      end
      else
        _rdr.SkipUnknownField( key, False ) ;
    end ;

  end ;

  procedure TGIS_LayerMVT.readFeature(
    const _feature : T_MVTFeature ;
    const _rdr     : TGIS_GPBReader   ;
    const _limit   : Integer
  ) ;
  var
    key     : Integer ;
    nid     : Int64 ;
    tagsize : Cardinal ;
    tlimit  : Cardinal ;
    vkey    : Cardinal ;
    vval    : Cardinal ;
    ntype   : Cardinal ;
    ngeom   : Cardinal ;
    gsize   : Cardinal ;
    glimit  : Cardinal ;
  begin
    while ( _rdr.Position < _limit ) do begin

      key := _rdr.ReadFieldKey ;

      if ( key = _rdr.MakeKey( VT_FEATURE_ID, _rdr.WT_VARINT ) ) then begin
        nid := Int64(_rdr.readVarUInt64) ;
        _feature.Uid := nid ;
      end
      else if ( key = _rdr.MakeKey( VT_FEATURE_TAGS, _rdr.WT_DATA ) ) then begin
        tagsize := _rdr.ReadSize ;
        tlimit := _rdr.Position + tagsize ;
        while _rdr.Position < tlimit do begin
          vkey := _rdr.readVarUInt32 ;
          vval := _rdr.readVarUInt32 ;
          _feature.AddKeyVal( vkey, vval ) ;
        end ;
      end
      else if ( key = _rdr.MakeKey( VT_FEATURE_TYPE, _rdr.WT_VARINT ) ) then begin
        ntype := _rdr.readVarUInt32 ;
        _feature.SetGType( ntype ) ;
      end
      else if ( key = _rdr.MakeKey( VT_FEATURE_GEOMETRY, _rdr.WT_DATA ) ) then begin
        gsize := _rdr.ReadSize ;
        glimit := _rdr.Position + gsize ;
        while _rdr.Position < glimit do begin
          ngeom := _rdr.readVarUInt32 ;
          _feature.AddGeometry( ngeom ) ;
        end ;
      end
      else
        _rdr.SkipUnknownField( key, False ) ;
    end ;

  end ;

  procedure TGIS_LayerMVT.readValues(
    const _layer : T_MVTLayer ;
    const _rdr   : TGIS_GPBReader   ;
    const _limit : Integer
  ) ;
  var
    key  : Integer ;
    buf  : TBytes ;
    dval : Double ;
    fval : Single ;
    ival : Int64 ;
    uval : UInt64 ;
    sval : Cardinal ;
  begin
    while ( _rdr.Position < _limit ) do begin

      key := _rdr.ReadFieldKey ;

      if ( key = _rdr.MakeKey( VT_VALUE_STRING, _rdr.WT_DATA ) ) then begin
        _layer.AddValue( T_MVTValueType.vtString, _rdr.ReadText ) ;
      end
      else if ( key = _rdr.MakeKey( VT_VALUE_FLOAT, _rdr.WT_32BIT ) ) then begin
        SetLength( buf, 4 ) ;
        _rdr.readBuffer( buf, 0, 4 ) ;
        fval := 0 ;
        {$IFDEF MANAGED}
          fval := BitConverter.ToSingle( buf, 0 ) ;
        {$ELSE}
          System.Move( buf[0], fval, 4 ) ;
        {$ENDIF}
        _layer.AddValue( T_MVTValueType.vtFloat, fval ) ;
      end
      else if ( key = _rdr.MakeKey( VT_VALUE_DOUBLE, _rdr.WT_64BIT ) ) then begin
        SetLength( buf, 8 ) ;
        _rdr.readBuffer( buf, 0, 8 ) ;
        dval := 0 ;
        {$IFDEF MANAGED}
          dval := BitConverter.ToDouble( buf, 0 ) ;
        {$ELSE}
          System.Move( buf[0], dval, 8 ) ;
        {$ENDIF}
        _layer.AddValue( T_MVTValueType.vtDouble, dval ) ;
      end
      else if ( key = _rdr.MakeKey( VT_VALUE_INT, _rdr.WT_VARINT ) ) then begin
        ival := _rdr.readVarInt64 ;
        _layer.AddValue( T_MVTValueType.vtInt, ival ) ;
      end
      else if ( key = _rdr.MakeKey( VT_VALUE_UINT, _rdr.WT_VARINT ) ) then begin
        uval := _rdr.readVarUInt64 ;
        _layer.AddValue( T_MVTValueType.vtUInt, uval ) ;
      end
      else if ( key = _rdr.MakeKey( VT_VALUE_SINT, _rdr.WT_VARINT ) ) then begin
        ival :=_rdr.readVarSInt64 ;
        _layer.AddValue( T_MVTValueType.vtSInt, ival ) ;
      end
      else if ( key = _rdr.MakeKey( VT_VALUE_BOOL, _rdr.WT_VARINT ) ) then begin
        sval :=_rdr.readVarUInt32 ;
        _layer.AddValue( T_MVTValueType.vtBool, sval ) ;
      end
      else
        _rdr.SkipUnknownField( key, False ) ;
    end ;
  end ;

  {$IFDEF OXYGENE}
    procedure TGIS_LayerMVT.doPaintShape( _sender : Object; _e : TGIS_ShapeEventArgs ) ;
  {$ELSE}
    procedure TGIS_LayerMVT.doPaintShape( _sender : TObject; _shape : TGIS_Shape );
  {$ENDIF}
  var
    str : String ;
    sym : TGIS_SymbolAbstract ;
    obj : TGIS_MVTCustomSymbol ;
    res : Variant ;
    w   : Single ;
    sw  : Single ;
    shp : TGIS_Shape ;
    c   : TGIS_Color ;
    b   : Byte ;

    param_marker  : TGIS_ParamsMarker ;
    param_Line    : TGIS_ParamsLine ;
    param_area    : TGIS_ParamsArea ;
    param_label   : TGIS_ParamsLabel ;
    lparam_marker : TGIS_ParamsMarker ;
    lparam_Line   : TGIS_ParamsLine ;
    lparam_area   : TGIS_ParamsArea ;
    lparam_label  : TGIS_ParamsLabel ;

    function getQValue( const _query : TGIS_MVTQuery; var _res : Variant ) : Boolean ;
    var
      vFld : String ;
    begin
      Result := False ;
      if assigned( _query ) then begin
        vFld := VarToString( shp.GetField( _query.Field ) ) ;
        if not _query.Cases.TryGetValue( vFld, _res ) then
          _res := _query.Cases[''] ;
        Result := True ;
      end ;
    end ;

  begin
    {$IFDEF OXYGENE}
      shp := _e.Shape ;
    {$ELSE}
      shp := _shape ;
    {$ENDIF}

    param_marker := shp.Params.Marker ;
    param_Line   := shp.Params.Line ;
    param_area   := shp.Params.Area ;
    param_label  := shp.Params.Labels ;

    lparam_marker := shp.Layer.Params.Marker ;
    lparam_Line   := shp.Layer.Params.Line ;
    lparam_area   := shp.Layer.Params.Area ;
    lparam_label  := shp.Layer.Params.Labels ;

    if assigned( lparam_marker.UserObject ) then begin
      obj := TGIS_MVTCustomSymbol( lparam_marker.UserObject ) ;
      str := GisExpandLabel( obj.SymbolName, shp ) ;
      sym := TGIS_MVTStyler(oStyler).GetSprite( str ) ;

      if assigned( sym ) then begin
        param_marker.Symbol := sym ;
        param_marker.SizeAsText := Format(
          '%s:%ddip', [GIS_PARAMTXT_TYPE_SIZE, RoundS( sym.NativeSize * obj.SymbolWidth )]
        ) ;
      end ;
      if getQValue( obj.QSymbolSize, res ) then begin
        w := VarToSingle( res ) ;
        if w <> 0 then
          sw := RoundS(param_marker.Symbol.NativeSize * w)
        else
          sw := RoundS(param_marker.Symbol.NativeSize) ;

        param_marker.SizeAsText := Format( '%s:%ddip', [GIS_PARAMTXT_TYPE_SIZE, sw] ) ;
      end ;

      if not IsStringEmpty( param_label.Value ) then begin
        param_label.OutlineColor := param_label.FontColor ;
        param_label.Alignment    := TGIS_LabelAlignment.Center ;
        param_label.Shield       := param_marker.Symbol ;

        if param_label.Color = param_label.FontColor then
          param_label.FontColor := param_label.OutlineColor ;
      end ;
    end ;

    if assigned( lparam_area.UserObject ) then begin
      obj := TGIS_MVTCustomSymbol( lparam_area.UserObject ) ;
      if getQValue( obj.QFillColor, res ) then begin
        param_area.Color := TGIS_Color.FromARGB( VarToUInt32(res) ) ;
        param_area.OutlineColor := param_area.Color ;
        param_area.OutlineWidth := 0 ;
      end ;

      if getQValue( obj.QFillOutlineColor, res ) then
        param_area.OutlineColor := TGIS_Color.FromARGB( VarToUInt32(res) ) ;

      if not getQValue( obj.QFillOpacity, res ) then
        res := obj.Transparency ;
      w := VarToSingle(res) ;
      if w <> 1 then begin
        c := param_area.Color ;
        b := Byte(RoundS(w*255)) ;
        param_area.Color := TGIS_Color.FromARGB( b, c.R, c.G, c.B ) ;
        c := param_area.OutlineColor ;
        param_area.OutlineColor := TGIS_Color.FromARGB( b, c.R, c.G, c.B ) ;
      end ;

      if getQValue( obj.QFillPattern, res ) then begin
        sym := TGIS_MVTStyler(oStyler).GetSprite( VarToString(res) ) ;
        param_area.Symbol    := sym ;
        param_area.SymbolGap := 0 ;
      end ;
    end ;

    if assigned( lparam_Line.UserObject ) then begin
      obj := TGIS_MVTCustomSymbol( lparam_Line.UserObject ) ;
      if getQValue( obj.QLineColor, res ) then begin
        param_Line.Color := TGIS_Color.FromARGB( VarToUInt32(res) ) ;
        param_Line.Width := -1 ;
        param_Line.OutlineWidth := -1 ;
        param_area.OutlineColor := param_Line.Color ;
      end ;

      if getQValue( obj.QLineWidth, res ) then begin
        w := VarToSingle( res ) ;
        if w < 0 then begin
          param_Line.Width        := RoundS(w) ;
          param_area.OutlineWidth := param_Line.Width ;
        end
        else begin
          param_Line.Width        := RoundS(w * 20) ;
          param_area.OutlineWidth := param_Line.Width ;
        end ;
      end ;

      if not getQValue( obj.QLineOpacity, res ) then
        res := obj.Transparency ;
      w := VarToSingle(res) ;
      if w <> 1 then begin
        c := param_Line.Color ;
        b := Byte(RoundS(w*255)) ;
        param_Line.Color        := TGIS_Color.FromARGB( b, c.R, c.G, c.B ) ;
        param_area.OutlineColor := param_Line.Color ;
      end ;
    end ;

    if assigned( lparam_label.UserObject ) then begin
      obj := TGIS_MVTCustomSymbol( lparam_label.UserObject ) ;

      if getQValue( obj.QTextColor, res ) then begin
        param_label.Color     := TGIS_Color.FromARGB( VarToUInt32(res) ) ;
        param_label.FontColor := param_label.Color ;
      end ;

      if getQValue( obj.QTextHaloColor, res ) then
        param_label.Color := TGIS_Color.FromARGB( VarToUInt32(res) ) ;

      if getQValue( obj.QTextSize, res ) then begin
        w := VarToSingle( res ) ;
        if w <> 0 then
          param_label.FontSizeAsText := Format( '%s:%ddip', [GIS_PARAMTXT_TYPE_SIZE, TruncS(w)] ) ;
      end ;
    end ;

    shp.Draw ;
  end;

  procedure TGIS_LayerMVT.parseTile ;
  var
    lv    : TGIS_LayerVector ;
    ll    : TGIS_LayerAbstract ;
    shp   : TGIS_Shape ;
    {$IFDEF DCC}
    la    : T_MVTLayer ;
    fea   : T_MVTFeature ;
    {$ENDIF}
    i, k  : Integer ;
    //vtype : T_MVTValueType ;
    fname : String ;
    kidx  : Cardinal ;
    vidx  : Cardinal ;
    cmd   : Cardinal ;
    nx    : Integer ;
    ny    : Integer ;
    dx    : Integer ;
    dy    : Integer ;
    ptg   : TGIS_Point ;
    cnt   : Integer ;
    ex    : TGIS_Extent ;
  begin
    ex := Extent ;

    if FGeoreferenced then
      if GisIsNoWorld( ex ) then
        ex := GisExtent( -20037508.342789244, -20037508.342789244,
                          20037508.342789244,  20037508.342789244
                       ) ;

    for la in lstLayers do begin
      lv := TGIS_LayerVector.Create ;
      lv.Name := la.Name ;
      lv.UseRTree := False ;

      if FGeoreferenced then
        lv.SetCSByEPSG( 3857 ) ;

      Add( lv ) ;
      Extent := ex ;
      lv.Path := Self.Name + '\' + lv.Name ;

      lv.AddFieldInternal( 'mvt_id', TGIS_FieldType.Number, 10, 0 ) ;
      lv.AddFieldInternal( 'layer', TGIS_FieldType.String, 1, 0 ) ;

      for i := 0 to la.Keys.Count-1 do begin
        fname := la.Keys[i] ;
        if fname = 'layer' then continue ;

        lv.AddFieldInternal( fname, TGIS_FieldType.String, 1, 0 ) ;

        // TO-DO prerecognize field types from values
        //vtype := la.Values[i].VType ;
        {
        case vtype of
          T_MVTValueType.vtString,
          T_MVTValueType.vtStringS:
            lv.AddFieldInternal( fname, TGIS_FieldType.String, 1, 0 ) ;
          T_MVTValueType.vtFloat  :
            lv.AddFieldInternal( fname, TGIS_FieldType.Float, 10, 4 ) ;
          T_MVTValueType.vtDouble :
            lv.AddFieldInternal( fname, TGIS_FieldType.Number, 10, 7 ) ;
          T_MVTValueType.vtInt    :
            lv.AddFieldInternal( fname, TGIS_FieldType.Number, 10, 0 ) ;
          T_MVTValueType.vtUInt   :
            lv.AddFieldInternal( fname, TGIS_FieldType.Number, 10, 0 ) ;
          T_MVTValueType.vtSInt   :
            lv.AddFieldInternal( fname, TGIS_FieldType.Number, 10, 0 ) ;
          T_MVTValueType.vtBool   :
            lv.AddFieldInternal( fname, TGIS_FieldType.Boolean, 1, 0 ) ;
        end ;
        }
      end ;

      for fea in la.Features do begin
        if fea.Geometry.Count = 0 then continue ;

        case fea.GType of
          T_MVTGeomType.Point       :
            begin
              cmd := fea.Geometry[0] ;
              if ( getCmdId(cmd) = VT_CMD_MOVETO ) and ( getCmdCount(cmd) > 1 ) then
                shp := lv.CreateShape( TGIS_ShapeType.MultiPoint )
              else
                shp := lv.CreateShape( TGIS_ShapeType.Point ) ;
              end ;
          T_MVTGeomType.LineString  :
            shp := lv.CreateShape( TGIS_ShapeType.Arc ) ;
          T_MVTGeomType.Polygon     :
            shp := lv.CreateShape( TGIS_ShapeType.Polygon )
        else
            shp := lv.CreateShape( TGIS_ShapeType.Point ) ;
        end ;

        shp.SetField( 'mvt_id', fea.Uid ) ;
        shp.SetField( 'layer', lv.Name ) ;
        i := 0 ;
        while i < fea.KeyVals.Count do begin
          kidx := fea.KeyVals[i] ;
          vidx := fea.KeyVals[i+1] ;
          if la.Keys[kidx] <> 'layer' then
            shp.SetField( la.Keys[kidx], la.Values[vidx].VValue ) ;
          i := i + 2 ;
        end ;

        if fea.GType = T_MVTGeomType.Point then begin
          shp.Lock( TGIS_Lock.Projection ) ;
          shp.AddPart ;
          try
            nx  := 0 ;
            ny  := 0 ;
            cmd := fea.Geometry[0] ;
            cnt := getCmdCount( cmd ) ;
            i   := 1 ;
            k   := 0 ;

            while (k < cnt) and ( i < fea.Geometry.Count ) do begin
              dx := fea.Geometry[i  ] ;
              dy := fea.Geometry[i+1] ;

              dx := ((dx shr 1) xor (-(dx and 1))) ;
              dy := ((dy shr 1) xor (-(dy and 1))) ;

              nx := nx + dx ;
              ny := ny + dy ;

              getPtg( la, nx, ny, ptg ) ;
              shp.AddPoint( ptg ) ;

              i := i + 2 ;
              k := k + 1 ;
            end ;
          finally
            shp.Unlock ;
          end ;
        end
        else if (fea.GType = T_MVTGeomType.LineString) or
                (fea.GType = T_MVTGeomType.Polygon) then
        begin
          shp.Lock( TGIS_Lock.Projection ) ;
          try
            i  := 0 ;
            nx := 0 ;
            ny := 0 ;
            cmd := fea.Geometry[0] ;
            cnt := getCmdCount( cmd ) ;

            while i <  fea.Geometry.Count do begin
              if getCmdId( fea.Geometry[i] ) = VT_CMD_MOVETO then begin
                shp.AddPart ;
                cnt := getCmdCount( fea.Geometry[i] ) ;
                i := i + 1 ;
              end
              else if getCmdId( fea.Geometry[i] ) = VT_CMD_LINETO then begin
                cnt := getCmdCount( fea.Geometry[i] ) ;
                i := i + 1 ;
              end
              else if getCmdId( fea.Geometry[i] ) = VT_CMD_CLOSEPATH then begin
                cnt := 0 ;
                i := i + 1 ;
              end;

              for k := 0 to cnt-1 do begin
                dx := fea.Geometry[i]   ;
                dy := fea.Geometry[i+1] ;

                dx := ((dx shr 1) xor (-(dx and 1))) ;
                dy := ((dy shr 1) xor (-(dy and 1))) ;

                nx := nx + dx ;
                ny := ny + dy ;

                getPtg( la, nx, ny, ptg ) ;
                shp.AddPoint( ptg ) ;
                i := i + 2 ;
              end ;
            end ;
          finally
            shp.Unlock ;
          end ;

        end;
      end ; // for
    end ; // for

    if assigned( oStyler ) then begin
      TGIS_MVTStyler(oStyler).SortLayers( SubLayers ) ;
      for ll in SubLayers do begin
        lv := TGIS_LayerVector(ll) ;
        lv.ParamsList.ClearAndSetDefaults ;
        TGIS_MVTStyler(oStyler).BuildParams( lv, iLevel ) ;
        {$IFDEF OXYGENE}
          lv.PaintShapeEvent += @doPaintShape ;
        {$ELSE}
          lv.PaintShapeEvent := doPaintShape ;
        {$ENDIF}
      end ;
    end ;
  end ;

  function TGIS_LayerMVT.PreRecognize(
    const _path     : String ;
      var _new_path : String
  ) : Boolean ;
  var
    oStm : TGIS_BufferedFileStream ;
    buf  : TBytes ;
    i    : Integer ;
  begin
    if SafeFileExists( _path ) then begin
      Result := True ;
      oStm := TGIS_BufferedFileStream.Create( _path, TGIS_StreamMode.Read ) ;
      try
        SetLength( buf, 1024 ) ;
        {$IFDEF OXYGENE}
         oStm.Read( buf, 1024 ) ;
        {$ELSE}
         oStm.Read( buf[ 0 ], 1024 ) ;
        {$ENDIF}
        i := 0 ;
        while i < 1023 do begin
          if ( chr( buf[ i ] )   = 'O' ) and
             ( chr( buf[ i+1 ] ) = 'S' ) and
             ( chr( buf[ i+2 ] ) = 'M' ) then begin
               Result := False ;
               break ;
             end ;
          inc( i ) ;
        end ;
      finally
        FreeObject( oStm ) ;
      end ;
    end
    else
      Result := True ;

    Result := inherited PreRecognize( _path, _new_path ) and Result ;
  end ;

//==============================================================================
// TGIS_LayerMVTPixel
//==============================================================================

  procedure TGIS_LayerMVTPixel.setUp;
  begin

  end ;

  procedure TGIS_LayerMVTPixel.doDestroy ;
  begin

    inherited ;
  end ;

  constructor TGIS_LayerMVTPixel.Create(
    const _level    : Integer
  ) ;
  begin
    inherited Create ;

    iLevel  := _level ;
    oStyler := nil ;
  end;

  function TGIS_LayerMVTPixel.GetRawBitmap(
    const _extent   : TGIS_Extent ;
    const _bitmap   : TGIS_Pixels ;
    const _width    : Integer     ;
    const _height   : Integer
   ) : Boolean ;
  var
    ix, iy : Integer ;
  begin
    ix := RoundS(( FExtent.XMax - FExtent.XMin ) / ( _extent.XMax - _extent.XMin ) * _width)  ;
    iy := RoundS(( FExtent.YMax - FExtent.YMin ) / ( _extent.YMax - _extent.YMin ) * _height) ;

    if ix = 0 then
      ix := 1 ;
    RenderSize( ix, oStyler ) ;

    Result := inherited GetRawBitmap( _extent, _bitmap, _width, _height ) ;
  end ;

  procedure TGIS_LayerMVTPixel.RenderSize(
    const _size   : Integer ;
    const _styler : TObject
  ) ;
  var
    bmp     : TGIS_Bitmap ;
    lv      : TGIS_LayerMVT ;

    procedure viewer_call ;
    var
      {$IFDEF DCC}[unsafe]{$ENDIF} ovwr : IGIS_Viewer ;
      cl : TGIS_Color ;
    begin
      ovwr := IGIS_Viewer( bmp.CreateViewer ) ;
      if assigned( oStyler ) then begin
        cl := TGIS_MVTStyler(oStyler).BackgroundColor ;
        //if cl <> TGIS_Color.None then
          ovwr.Color := cl ;
      end ;
      ovwr.CustomPPI := Viewer.Ref.PPI ;
      ovwr.RestrictedDrag := False ;
      ovwr.UseRTree := False ;
      ovwr.Lock;
      ovwr.Add( lv ) ;
      ovwr.VisibleExtent := Extent ;
      ovwr.Unlock ;
    end ;

  begin
    if (FBitWidth = _size) and (FBitHeight = _size) then
      exit ;

    FBitWidth       := _size ;
    FBitHeight      := _size ;
    FCellWidth      := FBitWidth ;
    FCellHeight     := FBitHeight ;

    baseCellWidth   := FBitWidth ;
    baseCellHeight  := FBitHeight ;

    FBandsCount     := 3 ;
    realBitCount    := 24 ;
    realLineWidth   := (BitWidth*realBitCount) div 8 ;
    intLineWidth    := realLineWidth ;
    oStyler         := _styler ;

    lv := TGIS_LayerMVT.Create ;
    lv.Extent       := Extent ;
    lv.Stream       := Stream ;
    lv.Path         := '' ;
    lv.iLevel       := iLevel ;
    lv.Params.Style := Params.Style ;
    lv.oStyler      := oStyler ;

    try
      inherited setUp ;
    except
      on e : Exception do
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_SERVER_ERROR ), e.Message, 0 ) ;
    end ;

    bmp := TGIS_Bitmap.Create( FBitWidth, FBitHeight ) ;
    try
      bmp.Premultiplied := True ;
      viewer_call ;

      {$IFDEF DEBUG_WEBTILES}
        with VCL.Graphics.TBitmap( bmp.NativeBitmap ).Canvas do begin
          Font.Color := clRed ;
          TextOut( 1, 1, Caption ) ;
          Brush.Color := clRed ;
          FrameRect( Rect( 0, 0, FBitWidth, FBitHeight ) ) ;
        end;
      {$ENDIF}

      bmp.LockPixels( oBitmap, False, FBitmapFormat, FBitmapLinesOrder ) ;
    finally
      FreeObject( bmp ) ;
    end ;
  end;

//==============================================================================
// Lider.CG.GIS.GeoLayerMVT
//==============================================================================

  class procedure GisLayerMVT.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-MVT', 'Mapbox Vector Tile Format',
                   TGIS_LayerMVT,
                   '.mvt;.pbf',
                   TGIS_RegisteredLayerType.Vector,
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                   True
                 ) ;
    {$IFDEF GIS_TEST}
    RegisterLayer( 'DK-MVT', 'Mapbox Vector Tile Format',
                   TGIS_LayerMVT,
                   '.tile',
                   TGIS_RegisteredLayerType.Vector,
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                   True
                 ) ;
    {$ENDIF}
  end ;


//==============================================================================
// initialization/finalization
//==============================================================================

{$IFDEF DCC}
initialization
    GisLayerMVT.SelfRegisterLayer() ;
{$ENDIF}

end.
