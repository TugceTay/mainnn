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
  Encapsulation of a SQL - ADO file access.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerSqlAdo ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerSqlAdo"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

{$IFDEF GIS_NOADO}
  {$Message Error 'Unit not supported - no ADO support' }
{$ENDIF}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL,
    ADODB ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Variants,
    {$IFDEF ADOINTUNIT}
      AdoInt,
    {$ELSE}
      Lider.CG.GIS.GeoAdoInt,
    {$ENDIF}

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoLayerSql,
    Lider.CG.GIS.GeoLayerVector ;
{$ENDIF}

type

{$IFNDEF GIS_NOADO}
  {#gendoc:hide}
  // Initialization section handler
  GisLayerSqlAdo = class
    public
      class procedure SelfRegisterLayer() ;
  end ;
{$ENDIF}

  /// <summary>
  ///   Layer that can read SQL file via ADO.
  /// </summary>
  TGIS_LayerSqlAdo = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerSqlAbstract )
    protected
        procedure fset_SharedConnection   ( const _obj : _Connection );
        function  fget_SharedConnection : _Connection ;
    private
        /// <summary>
        ///   Is record in Append mode.
        /// </summary>
        isAppendMode : Boolean ;
    private
      // for internal use of TGIS_Viewer

        /// <summary>
        ///   Export the Shape geometry into variant geometry. For PostgeSQL
        ///   ODBC driver purposes
        /// </summary>
        /// <param name="_shp">
        ///   exported Shape
        /// </param>
        function  geometry2Hex          ( const _shp        : TGIS_Shape
                                        ) : String ;
    protected

        /// <inheritdoc/>
        procedure sqlTablePost          ( const _id        : Integer
                                        ) ; override;

        /// <inheritdoc/>
        procedure sqlTableAppend        ( const _id        : Integer        ;
                                          const _table     : String
                                        ) ; override;

        /// <inheritdoc/>
        procedure sqlTableOpenWrite     ( const _id        : Integer        ;
                                          const _table     : String         ;
                                          const _uidCol    : String         ;
                                          const _uidVal    : TGIS_Uid
                                        ) ; override;

        /// <inheritdoc/>
        procedure sqlTableSetGeometry   ( const _id        : Integer        ;
                                          const _name      : String         ;
                                          const _shp       : TGIS_Shape
                                        ) ; override;

        /// <inheritdoc/>
        procedure sqlQueryGetGeometry   ( const _uid       : TGIS_Uid        ;
                                          const _shapetype : TGIS_ShapeType ;
                                          const _cursor    : Integer        ;
                                          const _dim       : TGIS_DimensionType
                                        ) ; override;

        /// <inheritdoc/>
        function  sqlQueryGetGEOUID     ( const _cursor    : Integer
                                        ): Variant ; override;

        /// <inheritdoc/>
        function  sqlQueryNameGEOUID    ( const _cursor    : Integer
                                        ): String  ; override;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

        /// <inheritdoc/>
        function  bindFieldInternal     ( const _name      : String         ;
                                          const _cursor    : Integer
                                        ) : Integer ; override;

        /// <inheritdoc/>
        function  getBindedFieldInternal( const _shape     : TObject        ;
                                          const _field     : Integer        ;
                                          const _cursor    : Integer
                                        ) : Variant ; override;
    protected
          /// <inheritdoc/>
         procedure doDestroy ; override;
    public
         /// <inheritdoc/>
         constructor Create  ; override;
         /// <inheritdoc/>
         procedure Build                ( const _path      : String         ;
                                          const _extent    : TGIS_Extent    ;
                                          const _type      : TGIS_ShapeType ;
                                          const _dim       : TGIS_DimensionType
                                        ) ; override;
         /// <inheritdoc/>
         function  PreRecognize         ( const _path      : String         ;
                                          var   _new_path  : String
                                        ) : Boolean ; override;
    public

      {#gendoc:hide:GENPDK} { TODO -cPDK : Verify }
      /// <summary>
      ///   If assigned then external assigned connection will be used.
      /// </summary>
      property SharedConnection : _Connection    read  fget_SharedConnection
                                                 write fset_SharedConnection ;
  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoDbAdo,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoRegistredLayers ;
{$ENDIF}

//=============================================================================
// TGIS_LayerSqlAdo
//=============================================================================

  constructor TGIS_LayerSqlAdo.Create ;
  begin
    inherited ;

    oGisDb := TGIS_DbAdo.Create ;

    canSkipField := True ;
  end ;

  procedure TGIS_LayerSqlAdo.doDestroy ;
  begin
    FreeObject( oGisDb ) ;

    inherited ;
  end ;

  procedure TGIS_LayerSqlAdo.fset_SharedConnection(
    const _obj : _Connection
  ) ;
  begin
    TGIS_DbAdo( oGisDb ).SharedConnection := _obj ;
  end ;

  function TGIS_LayerSqlAdo.fget_SharedConnection : _Connection;
  begin
    Result := TGIS_DbAdo( oGisDb ).SharedConnection ;
  end ;

  function TGIS_LayerSqlAdo.geometry2Hex(
    const _shp : TGIS_Shape
  ) : String ;
  var
    {$IFDEF CLR}
      obj : System.Object ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      arr : array of Byte ;
    {$ENDIF}
    store : OleVariant ;
    b     : String     ;
    i     : Integer    ;
  begin
    Result := '' ;
    try
      {$IFDEF CLR}
        _shp.ExportToVAR( obj ) ;
        store := OleVariant( obj ) ;
      {$ELSE}
        _shp.ExportToVAR( store ) ;
      {$ENDIF}

      for i:=0 to VarArrayHighBound( store, 1 ) do begin
        {$IFNDEF OXYGENE}
          b := IntToHex( Integer( store[i] ), 2 ) ;
        {$ELSE}
          arr := array of Byte( store ) ;
          b := IntToHex( Integer( arr[i] ), 2 ) ;
        {$ENDIF}
        Result := Result + b ;
      end ;

      store := Unassigned ;
    except
      Result := '' ;
      store := Unassigned ;
    end ;
  end ;

  procedure TGIS_LayerSqlAdo.sqlTableSetGeometry(
    const _id   : Integer ;
    const _name : String ;
    const _shp  : TGIS_Shape
  ) ;
  var
    store : OleVariant ;
    {$IFDEF CLR}
      obj : System.Object ;
    {$ENDIF}
  begin
    if oGisDb.UseTextParameters then begin
      oGisDb.sqlTableSetGeometry(
          _id,
          _name,
          Format( getCmdGEOMETRY_CAST, [ geometry2Hex( _shp ) ]),
          nil
      ) ;
    end
    else begin
      {$IFDEF CLR}
        _shp.ExportToVAR( obj ) ;
        store := OleVariant( obj ) ;
      {$ELSE}
        _shp.ExportToVAR( store ) ;
      {$ENDIF}
      oGisDb.sqlTableSetGeometry( _id, _name, store, nil );
      store := Unassigned ;
    end ;
  end ;

  procedure TGIS_LayerSqlAdo.sqlTablePost(
    const _id : Integer
  ) ;
  begin
    if oGisDb.UseTextParameters then begin
      if ( isAppendMode ) then
        if ( _id = 0 ) then
          oGisDb.sqlExec( prepareAppendCommandEx( TableGeometry,
                                                  oGisDb.sqlGetParams( _id ) ) )
        else
          oGisDb.sqlExec( prepareAppendCommandEx( TableFeatures,
                                                  oGisDb.sqlGetParams( _id ) ) )
      else
        if ( _id = 0 ) then
          oGisDb.sqlExec( prepareUpdateCommandEx( TableGeometry,
                                                  oGisDb.sqlGetParams( _id ) ) )
        else
          oGisDb.sqlExec( prepareUpdateCommandEx( TableFeatures,
                                                  oGisDb.sqlGetParams( _id ) ) ) ;
    end
    else begin
      oGisDb.sqlTablePost( _id ) ;
    end ;
  end ;

  procedure TGIS_LayerSqlAdo.sqlTableAppend(
    const _id    : Integer ;
    const _table : String
   ) ;
  begin
    isAppendMode := True ;

    if oGisDb.UseTextParameters then begin
      prepareParamsCommand( _table, oGisDb.sqlGetParams( _id ) ) ;
    end
    else begin
      oGisDb.sqlTableAppend( _id,
                             prepareSelectCommand( _table, prepareFilterUid(-1))
                            ) ;
    end ;
  end ;

  procedure TGIS_LayerSqlAdo.sqlTableOpenWrite(
    const _id     : Integer ;
    const _table  : String  ;
    const _uidCol : String  ;
    const _uidVal : TGIS_Uid
  ) ;
  var
    cmd : String ;
  begin
    isAppendMode := False ;

    if oGisDb.UseTextParameters then
      prepareParamsCommand( _table, oGisDb.sqlGetParams( _id ) )
    else begin
      cmd := prepareSelectCommand( _table, prepareFilterUid( _uidVal ) );
      oGisDb.sqlTableOpenWrite( _id, cmd ) ;
    end ;
  end ;

  procedure TGIS_LayerSqlAdo.sqlQueryGetGeometry(
     const _uid       : TGIS_Uid ;
     const _shapetype : TGIS_ShapeType ;
     const _cursor    : Integer ;
     const _dim       : TGIS_DimensionType
   ) ;
  var
    {$IFDEF CLR}
      ptrvar : TGIS_Bytes ;
    {$ELSE}
      ptrvar : Pointer    ;
    {$ENDIF}
    size     : Integer ;
  begin
    ptrvar := oGisDb.sqlQueryGetGeomPtr( getCmdGEOMETRY, '', _cursor, size ) ;
    try
      if not assigned( cursorSql[_cursor].currShape ) then
      case _shapetype of
        TGIS_ShapeType.Point :
           begin
             cursorSql[_cursor].currPoint.Recreate     (
                nil, ptrvar, True, _uid, self, _dim
             ) ;
             cursorSql[_cursor].currShape := cursorSql[_cursor].currPoint ;
           end ;
        TGIS_ShapeType.MultiPoint :
           begin
             cursorSql[_cursor].currMultipoint.Recreate(
                nil, ptrvar, True, _uid, self, _dim
             ) ;
             cursorSql[_cursor].currShape := cursorSql[_cursor].currMultipoint ;
           end ;
        TGIS_ShapeType.Arc :
           begin
             cursorSql[_cursor].currArc.Recreate       (
                nil, ptrvar, True, _uid, self, _dim
             ) ;
             cursorSql[_cursor].currShape := cursorSql[_cursor].currArc ;
           end ;
        TGIS_ShapeType.Polygon :
           begin
             cursorSql[_cursor].currPolygon.Recreate   (
                nil, ptrvar, True, _uid, self, _dim
             ) ;
             cursorSql[_cursor].currShape := cursorSql[_cursor].currPolygon ;
           end ;
        TGIS_ShapeType.MultiPatch :
           begin
             cursorSql[_cursor].currMultiPatch.Recreate   (
                nil, ptrvar, True, _uid, self, _dim
             ) ;
             cursorSql[_cursor].currShape := cursorSql[_cursor].currMultiPatch ;
           end ;
        else
           begin
             cursorSql[_cursor].currShape := nil ;
           end ;
      end ;
    finally
      oGisDb.sqlQueryUnPrepareGetGeom( _cursor ) ;
    end ;
  end ;

  function TGIS_LayerSqlAdo.sqlQueryNameGEOUID(
    const _cursor     : Integer
  ) : String ;
  begin
    if fixGEOUID < 0 then
      fixGEOUID := oGisDb.sqlQueryNameGEOUID( getCmdGEOUID( 0 ),
                                              getCmdGEOUID( 1 ), _cursor
                                             );
    Result := getCmdGEOUID( fixGEOUID ) ;
  end ;

  function TGIS_LayerSqlAdo.sqlQueryGetGEOUID(
    const _cursor : Integer
  ) : Variant ;
  begin
    Result := oGisDb.sqlQueryGetGEOUID( sqlQueryNameGEOUID(_cursor), _cursor ) ;
  end ;

  function TGIS_LayerSqlAdo.bindFieldInternal(
    const _name   : String ;
    const _cursor : Integer
  ) : Integer ;
  begin
    Result := oGisDb.sqlBindField( _name, _cursor ) ;
  end ;

  function TGIS_LayerSqlAdo.getBindedFieldInternal(
    const _shape  : TObject ;
    const _field  : Integer ;
    const _cursor : Integer
  ) : Variant ;
  var
    shp    : TGIS_Shape ;
    rebind : Boolean ;
  begin
    Result := Unassigned ;

    shp := TGIS_Shape( _shape ) ;

    if not assigned( shp ) then Exit ;

    if shp.IsEditable then begin
      // fetch corresponding record for in-memory shapes
      if macroFetchRecord( shp.Uid, _cursor ) then
        oGisDb.sqlGetBindedField( -1, True, _cursor ); // just rebind
    end ;

    rebind := ( not assigned( cursorSql[_cursor].currShape ) ) or
              ( cursorSql[_cursor].currShape.Uid <> shp.Uid ) ;
    if rebind then
      cursorSql[_cursor].currShape := GetShape( shp.Uid, _cursor ) ;

    Result := oGisDb.sqlGetBindedField( _field, rebind, _cursor ) ;
  end ;

  procedure TGIS_LayerSqlAdo.Build(
    const _path   : String ;
    const _extent : TGIS_Extent;
    const _type   : TGIS_ShapeType ;
    const _dim    : TGIS_DimensionType
  ) ;
  begin
    try
      oGisDb.sqlBuild( _path, _extent, _type, GIS_INI_LAYERSQL_NATIVE, Name );
    finally
      inherited;
    end ;
  end ;

  function TGIS_LayerSqlAdo.PreRecognize(
    const _path     : String ;
    var   _new_path : String
   ) : Boolean ;
  begin
    Result := oGisDb.PreRecognize(
                _path,
                GIS_INI_LAYERSQL_NATIVE,
                GIS_SQL_PROVIDER_ADO
              ) ;
  end ;

{$IFNDEF GIS_NOADO}
  { Perform initialization section.
  }
  class procedure GisLayerSqlAdo.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-TTKLS', GIS_SQL_LAYER_CONNECTOR, TGIS_LayerSqlAdo,
                   GIS_TTKLAYER_VECTOR_FILTER,
                   TGIS_RegisteredLayerType.Vector, TGIS_RegisteredFormatType.Server,
                   [ TGIS_RegisteredOperationType.Read,
                     TGIS_RegisteredOperationType.Write,
                     TGIS_RegisteredOperationType.&Create,
                     TGIS_RegisteredOperationType.Merge ],
                   True
                 ) ;
  end ;

  {$IFNDEF OXYGENE}
    initialization
      GisLayerSqlAdo.SelfRegisterLayer() ;
  {$ENDIF}
{$ENDIF}

//==================================== END =====================================
end.

