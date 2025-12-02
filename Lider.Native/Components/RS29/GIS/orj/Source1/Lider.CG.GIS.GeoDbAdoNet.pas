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
  Encapsulation of a SQL file access for ADO.NET.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoDbAdoNet ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoDbAdoNet"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    System.Data,
    System.IO,
    System.Data.Common,
    System.Data.SqlTypes,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    System.Classes,
    System.SysUtils,
    System.Variants,

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoDb,
    Lider.CG.GIS.GeoStreams ;
{$ENDIF}

type

  {$IFDEF CLR}
    { DB cursor. }
    T_CursorDbAdoNet {$IFDEF OXYGENE} nested in TGIS_DbAdoNet {$ENDIF} = record
      public

      /// <summary>
      ///   Is cursor in use.
      /// </summary>
      FCurInUse           : Boolean ;

      /// <summary>
      ///   Sql command handle.
      /// </summary>
      FSqlCommand         : DbCommand ;

      /// <summary>
      ///   Sql data reader handle.
      /// </summary>
      FSqlDataReader      : DbDataReader ;

      /// <summary>
      ///   Sql table Schema handle.
      /// </summary>
      FSqlSchemaTable     : DataTable ;

      /// <summary>
      ///   Sql data reader eof flag.
      /// </summary>
      FSqlDataReaderEof   : Boolean ;

      /// <summary>
      ///   Geometry field id.
      /// </summary>
      FSqlGeometryFieldId : Integer ;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Class that can read AdoNet.
  /// </summary>
  TGIS_DbAdoNet = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_DbAbstract )
    private
      {$IFDEF CLR}
        FSqlConnection  : DbConnection ;
        FSqlTransaction : DbTransaction ;
        FSqlCommands    : array [0..2] of DbCommand ;
        FCursorDB       : array of T_CursorDbAdoNet ;
        FErrorMessage   : String ;
      {$ELSE}
        FObjHdl         : TObject ;
        FDLLHandle      : THandle ;
        FIsInitialized  : Boolean ;
      {$ENDIF}
    private

         /// <summary>
         ///   Test is a provider is initialized.
         /// </summary>
         function  isInitialized : Boolean ;
      {$IFNDEF CLR}
         /// <summary>
         ///   Initialize library.
         /// </summary>
         /// <param name="_path">
         ///   library path
         /// </param>
         procedure initializeDLL( const _path : String ) ;

         /// <summary>
         ///   Finalize library.
         /// </summary>
         procedure finalizeDLL ;

         /// <summary>
         ///   Check operation status and raise error.
         /// </summary>
         procedure check( const _status : Integer ) ;
      {$ENDIF}

    protected
      procedure fset_GeometryType       ( const _type : TGIS_SubDataType
                                        ) ; override;
    public
    // for internal use

       /// <inheritdoc/>
       procedure sqlConnect             (  const _folder        : String  ;
                                           const _sqlParameters : {$IFDEF OXYGENE}
                                                                   TGIS_Strings
                                                                 {$ELSE}
                                                                   TStrings
                                                                 {$ENDIF}
                                        ) ; override;

       /// <inheritdoc/>
       procedure sqlDisconnect          ; override;

       /// <inheritdoc/>
       procedure sqlTransactUpdateStart ; override;

       /// <inheritdoc/>
       procedure sqlTransactUpdateCommit; override;

       /// <inheritdoc/>
       procedure sqlTransactRestructStart; override;

       /// <inheritdoc/>
       procedure sqlTransactRestructCommit; override;

       /// <inheritdoc/>
       procedure sqlTransactGlobalUpdateStart; override;

       /// <inheritdoc/>
       procedure sqlTransactGlobalUpdateCommit; override;

       /// <inheritdoc/>
       procedure sqlTransactRollback   ; override;

       /// <inheritdoc/>
       procedure sqlQueryClose         ( const _cursor        : Integer
                                       ); override;

       /// <inheritdoc/>
       procedure sqlTableClose         ( const _id            : Integer
                                       ) ; override;

       /// <inheritdoc/>
       procedure sqlTablePost          ( const _id            : Integer
                                       ) ; override;

       /// <inheritdoc/>
       procedure sqlTableExec          ( const _id            : Integer
                                       ) ; override;

       /// <inheritdoc/>
       procedure sqlTableOpenRead      ( const _id            : Integer ;
                                         const _query         : String
                                       ) ; override;

       /// <inheritdoc/>
       procedure sqlTableOpenWrite     ( const _id            : Integer ;
                                         const _query         : String
                                       ) ; override;

       /// <inheritdoc/>
       procedure sqlTableAppend        ( const _id            : Integer ;
                                         const _query         : String
                                       ) ; override;

       /// <inheritdoc/>
       function  sqlTablePrepared      ( const _id            : Integer
                                       ) : Boolean ; override;

       /// <inheritdoc/>
       procedure sqlQueryOpen          ( const _query         : String ;
                                         const _cursor        : Integer
                                       ) ; override;

       /// <inheritdoc/>
       procedure sqlQueryReset         ( const _cursor        : Integer
                                       ) ; override;

       /// <inheritdoc/>
       procedure sqlExec               ( const _command       : String
                                       ) ; override;

       /// <inheritdoc/>
       procedure sqlQueryMoveFirst     ( const _cursor        : Integer
                                       ); override;

       /// <inheritdoc/>
       procedure sqlQueryMoveNext      ( const _cursor        : Integer
                                       ); override;

       /// <inheritdoc/>
       function  sqlQueryEof           ( const _cursor        : Integer
                                       ) : Boolean ; override;

       /// <inheritdoc/>
       function  sqlTableEof           ( const _id            : Integer
                                       ) : Boolean ; override;

       /// <inheritdoc/>
       function  sqlQueryGetFieldIndex ( const _name          : String ;
                                         const _cursor        : Integer
                                       ) : Integer ; override;

       /// <inheritdoc/>
       function  sqlQueryGetField      ( const _name          : String ;
                                         const _cursor        : Integer
                                       ) : Variant ; override;

       /// <inheritdoc/>
       function  sqlQueryGetFieldById  ( const _id            : Integer ;
                                         const _cursor        : Integer
                                       ) : Variant ; override;

       /// <inheritdoc/>
       function  sqlTableGetField      ( const _id            : Integer ;
                                         const _name          : String
                                       ) : Variant ; override;

       /// <inheritdoc/>
       function  sqlTableGetParam      ( const _id            : Integer ;
                                         const _name          : String
                                       ) : Variant ; override;

       /// <inheritdoc/>
       procedure sqlTableSetField      ( const _id            : Integer ;
                                         const _name          : String ;
                                         const _val           : Variant ;
                                         const _defSize       : Integer
                                       ) ; override;

       /// <inheritdoc/>
       function  sqlGetBindedField     ( const _field         : Integer ;
                                         const _rebindFields  : Boolean ;
                                         const _cursor        : Integer
                                       ) : Variant ; override;

       /// <inheritdoc/>
       procedure sqlQueryStructure     ( const _tableName     : String ;
                                         const _uidName       : String ;
                                         const _addFieldEvent :
                                                        TGIS_LayerAddFieldEvent
                                       ) ; override;

       /// <inheritdoc/>
       function  sqlQueryNameGEOUID    ( const _field         : String ;
                                         const _fieldEx       : String ;
                                         const _cursor        : Integer
                                       ) : Integer ; override;

       /// <inheritdoc/>
       function  sqlQueryGetGEOUID     ( const _field         : String ;
                                         const _cursor        : Integer
                                       ) : Variant ; overload; override;

       /// <inheritdoc/>
       function  sqlQueryGetGEOUID     ( const _index         : Integer ;
                                         const _cursor        : Integer
                                       ) : Variant ; overload; override;

       /// <inheritdoc/>
       function  sqlQueryGetSHAPETYPE  ( const _field         : String ;
                                         const _cursor        : Integer
                                       ) : Variant ; override;

       /// <inheritdoc/>
       function  sqlQueryGetXMIN       ( const _field         : String ;
                                         const _cursor        : Integer
                                       ) : Variant ; override;

       /// <inheritdoc/>
       function  sqlQueryGetYMIN       ( const _field         : String ;
                                         const _cursor        : Integer
                                       ) : Variant ; override;

       /// <inheritdoc/>
       function  sqlQueryGetGeomVAR    ( const _field         : String ;
                                         const _fieldEx       : String ;
                                         const _cursor        : Integer ;
                                         const _forceValue    : Boolean = False
                                       ) :  OleVariant; override;

       /// <inheritdoc/>
       function  sqlQueryGetBlob       ( const _name          : String ;
                                         const _cursor        : Integer
                                       ) : TStream ; override;
       {$IFDEF CLR}
         /// <inheritdoc/>
        function  sqlQueryGetGeomPtr   ( const _field         : String ;
                                         const _cursor        : Integer;
                                           var _size          : Integer
                                       ) : TBytes ; override;

         /// <inheritdoc/>
         function  sqlQueryGetGeomPtr  ( const _field         : String ;
                                         const _fieldEx       : String ;
                                         const _cursor        : Integer;
                                           var _size          : Integer
                                       ) : TGIS_Bytes ; override;
       {$ELSE}

       /// <inheritdoc/>
       function  sqlQueryGetGeomPtr    ( const _field         : String ;
                                         const _fieldEx       : String ;
                                         const _cursor        : Integer ;
                                           var _size          : Integer
                                       ) : Pointer ; override;
       {$ENDIF}

       /// <inheritdoc/>
       function  sqlQueryGetGeomObj    ( const _field         : String ;
                                         const _cursor        : Integer
                                       ) : TObject ; override;

       /// <inheritdoc/>
       procedure sqlTableSetGeometry   ( const _id            : Integer ;
                                         const _name          : String ;
                                         const _data          : OleVariant ;
                                         const _blob          : TGIS_MemoryStream
                                       ) ; overload; override;

       /// <inheritdoc/>
       procedure sqlTableSetGeometry   ( const _id            : Integer ;
                                         const _name          : String ;
                                         const _data          : TObject
                                       ) ; overload; override;

       /// <inheritdoc/>
       procedure sqlTableSetBlob       (  const _id    : Integer ;
                                          const _name  : String ;
                                          const _data  : OleVariant ;
                                          const _blob  : TGIS_MemoryStream
                                       ) ; override;

       /// <inheritdoc/>
       procedure sqlQueryUnPrepareGetGeom( const _cursor        : Integer
                                         ) ; override;

       /// <inheritdoc/>
       function  sqlBindField          ( const _field         : String ;
                                         const _cursor        : Integer
                                       ) : Integer ; override;

       /// <inheritdoc/>
       procedure sqlUpdateStart        ( const _id            : Integer ;
                                         const _table         : String
                                       ) ; override;

       /// <inheritdoc/>
       function sqlGetParams           ( const _idx           : Integer
                                       ) : {$IFDEF OXYGENE}
                                             TGIS_StringList ;
                                           {$ELSE}
                                             TStringList ;
                                           {$ENDIF}
                                       override;

       /// <inheritdoc/>
       procedure sqlTableCreateParam   ( const _id            : Integer ;
                                         const _name          : String ;
                                         const _type          : TGIS_DataType ;
                                         const _subtype       : TGIS_SubDataType ;
                                         const _size          : Integer
                                       ) ; override;

       /// <inheritdoc/>
       procedure sqlBuild              ( const _path          : String ;
                                         const _extent        : TGIS_Extent;
                                         const _type          : TGIS_ShapeType ;
                                         const _storage       : String ;
                                         const _layerName     : String
                                       ) ; override ;

       /// <inheritdoc/>
       function sqlQueryGeometryIsText ( const _cursor        : Integer
                                       ): Boolean ; override;

       /// <inheritdoc/>
       procedure cursorOpen            ( const _cursor       : Integer
                                       ) ; override;

       /// <inheritdoc/>
       procedure cursorClose           ( const _cursor       : Integer
                                       ) ; override;

       /// <inheritdoc/>
       function  getLastCursor         : Integer ; override;

    protected

      /// <inheritdoc/>
      procedure doDestroy ; override;
    public

       /// <inheritdoc/>
       constructor Create  ; override;

       /// <inheritdoc/>
       function  InitializeProvider : Boolean ; override;

       /// <inheritdoc/>
       procedure FinalizeProvider ; override;

       /// <inheritdoc/>
       function  GetLastErrorMessage : String ; override;
    end;

//##############################################################################
implementation

{$IFDEF OXYGENE}
  uses
    {$IFDEF ADONET_FULL}
    System.Data.SqlClient,
    {$ENDIF}
    System.Reflection ;
{$ELSE}
  uses
    System.Math,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoLogger,
    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoUtils,
    Lider.CG.GIS.GeoConfig ;
{$ENDIF}

{$IFNDEF CLR}
  //=============================================================================
  // T_Wrapper
  //=============================================================================

  const
    ADONETDLLNAME = 'ttkADONET.dll' ;

    ADONET_OK        =  0 ;
    ADONET_ERROR     =  1 ;
    ADONET_WARNING   =  2 ;

  var
    {$IFNDEF CLR}
      _adonet_initialize : function(var _handle : TObject) : Integer ; stdcall ;

      _adonet_finalize : function(_handle : TObject) : Integer ; stdcall ;

      _adonet_version : function(var _version : Double) : Integer ; stdcall ;

      _adonet_errmsg : function(_handle : TObject ; var _error : PAnsiChar)
         : Integer ; stdcall ;

      _adonet_cursor_open : function(_handle : TObject ; _cursor : Integer)
         : Integer ; stdcall ;

      _adonet_cursor_close : function(_handle : TObject ; _cursor : Integer)
         : Integer ; stdcall ;

      _adonet_get_last_cursor : function(_handle : TObject; var _res : Integer )
         : Integer ; stdcall ;

      _adonet_sql_connect : function(_handle : TObject ; _connectStr : PAnsiChar ;
        _provider : PAnsiChar) : Integer ; stdcall ;

      _adonet_sql_disconnect : function(_handle : TObject) : Integer ; stdcall ;

      _adonet_sql_transact_start : function(_handle : TObject) : Integer ; stdcall ;

      _adonet_sql_transact_commit : function(_handle : TObject) : Integer ; stdcall ;

      _adonet_sql_transact_rollback : function(_handle : TObject) : Integer ; stdcall ;

      _adonet_sql_query_open : function(_handle : TObject ; _query : PAnsiChar ;
        _cursor : Integer) : Integer ; stdcall ;

      _adonet_sql_query_close : function(_handle : TObject ; _cursor : Integer)
         : Integer ; stdcall ;

      _adonet_sql_query_move_first : function(_handle : TObject ; _cursor : Integer)
         : Integer ; stdcall ;

      _adonet_sql_query_move_next : function(_handle : TObject ; _cursor : Integer)
         : Integer ; stdcall ;

      _adonet_sql_query_eof : function(_handle : TObject ; var _eof : Integer ;
        _cursor : Integer) : Integer ; stdcall ;

      _adonet_sql_query_value : function(_handle : TObject ; _name : PAnsiChar ;
        var _val : OleVariant ; _cursor : Integer ) : Integer ; stdcall ;

      _adonet_sql_query_value_string : function(_handle : TObject ; _id : Integer ;
        var _val : OleVariant ; _cursor : Integer ) : Integer ; stdcall ;

      _adonet_sql_query_value_int64 : function(_handle : TObject ; _id : Integer ;
        var _val : OleVariant ; _cursor : Integer ) : Integer ; stdcall ;

      _adonet_sql_query_value_double : function(_handle : TObject ; _id : Integer ;
        var _val : OleVariant ; _cursor : Integer ) : Integer ; stdcall ;

      _adonet_sql_query_value_date : function(_handle : TObject ; _id : Integer ;
        var _val : OleVariant ; _cursor : Integer ) : Integer ; stdcall ;

      _adonet_sql_query_value_blob : function(_handle : TObject ; _name : PAnsiChar ;
        var _size : Integer ; var _val : IntPtr ; _cursor : Integer
      ) : Integer ; stdcall ;

      _adonet_sql_query_value_by_id : function(_handle : TObject ; _id : Integer ;
        var _val : OleVariant ; _cursor : Integer ) : Integer ; stdcall ;

      _adonet_sql_query_field_id : function(_handle : TObject ; _name : PAnsiChar ;
        var _id : Integer ; _cursor : Integer) : Integer stdcall ;

      _adonet_sql_query_fields_count : function(_handle : TObject ;
        var _count : Integer ; _cursor : Integer) : Integer ; stdcall ;

      _adonet_sql_query_field_info : function(_handle : TObject ; _index : Integer ;
        var _name : PAnsiChar ; var _type : Integer ;
        var _size : Integer ; var _prec : Integer ; var _scale  : Integer ;
        _cursor : Integer ) : Integer ; stdcall ;

      _adonet_sql_exec : function(_handle : TObject ; _command : PAnsiChar)
         : Integer ; stdcall ;

      _adonet_sql_table_prepare : function(_handle : TObject ; _id : Integer ;
        _query : PAnsiChar) : Integer ; stdcall ;

      _adonet_sql_table_prepared : function(_handle : TObject ; _id : Integer ;
          var _prepared : Integer ) : Integer ; stdcall ;

      _adonet_sql_table_close : function(_handle : TObject ; _id : Integer
          ) : Integer ; stdcall ;

      _adonet_sql_table_create_param : function(_handle : TObject ; _id : Integer ;
        _name : PAnsiChar ; _type : Integer ; _subtype : Integer ; _size : Integer)
         : Integer ; stdcall ;

      _adonet_sql_table_set_param_string : function(_handle : TObject ; _id : Integer ;
        _name : PAnsiChar ; _value : Variant) : Integer ; stdcall ;

      _adonet_sql_table_set_param_null : function(_handle : TObject ; _id : Integer ;
        _name : PAnsiChar ) : Integer ; stdcall ;

      _adonet_sql_table_set_param_int : function(_handle : TObject ; _id : Integer ;
        _name : PAnsiChar ; _value : Integer) : Integer ; stdcall ;

      _adonet_sql_table_set_param_int64 : function(_handle : TObject ; _id : Integer ;
        _name : PAnsiChar ; _value : Int64) : Integer ; stdcall ;

      _adonet_sql_table_set_param_double : function(_handle : TObject ; _id : Integer ;
        _name : PAnsiChar ; _value : Double) : Integer ; stdcall ;

      _adonet_sql_table_set_param_value : function(_handle : TObject ; _id : Integer ;
        _name : PAnsiChar ; _value : Variant) : Integer ; stdcall ;

      _adonet_sql_table_set_param_blob : function(_handle : TObject ; _id : Integer ;
        _name : PAnsiChar ; _value : Variant) : Integer ; stdcall ;

      _adonet_sql_table_set_param_geometry : function(_handle : TObject ;
        _id : Integer ; _name : PAnsiChar ; _buffer : Variant) : Integer ; stdcall ;

      _adonet_sql_table_post : function(_handle : TObject ; _id : Integer)
         : Integer ; stdcall ;

      _adonet_set_geometry_type : function(_handle : TObject ; _type : Integer)
         : Integer ; stdcall ;
    {$ENDIF}

{$ENDIF}

//=============================================================================
// TGIS_SqlAdoNet
//=============================================================================

  constructor TGIS_DbAdoNet.Create ;
  {$IFDEF CLR}
    var
      i : Integer ;
  {$ENDIF}
  begin
    inherited ;

    FParameterPrefix := '@p' ;

    {$IFDEF CLR}
      FSqlConnection := nil ;

      for i := 0 to BUILTIN_CURSORS -1 do
        cursorOpen( i ) ;
    {$ELSE}
      FObjHdl := nil ;
    {$ENDIF}
    FiCodePage := 65001 ;
  end ;

  procedure TGIS_DbAdoNet.doDestroy ;
  var
    i : Integer ;
  begin
    {$IFDEF CLR}
      sqlDisconnect ;
      for i := 0 to BUILTIN_CURSORS - 1 do
        cursorClose( i );
    {$ELSE}
      if isInitialized then begin
        sqlDisconnect ;
        for i := 0 to BUILTIN_CURSORS - 1 do
          cursorClose( i );
      end ;
      FinalizeProvider ;

      FObjHdl := nil ;
    {$ENDIF}

    inherited ;
  end ;

  procedure TGIS_DbAdoNet.fset_GeometryType(
    const _type : TGIS_SubDataType
  ) ;
  begin
    {$IFNDEF CLR}
      if isInitialized then
        check( _adonet_set_geometry_type( FObjHdl, Integer(_type) ) ) ;
    {$ENDIF}

    inherited ;
  end ;

 {$IFNDEF CLR}
  procedure TGIS_DbAdoNet.initializeDLL(
    const _path : String
  ) ;

    procedure mapFnc(  var _var    : Pointer;
                       const _name : String
                     ) ;
    begin
      _var := GetProcAddress( FDLLHandle, PChar( _name ) ) ;
      Assert( _var <> nil ) ;
    end;

  begin
    FIsInitialized := False ;
    FDLLHandle     := LoadLibraryWithinHinstance( _path ) ;

    if FDLLHandle = 0 then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEMAPPING ),
                                   _path + #13#10 + SystemErrorMessage,
                                   0
                                  ) ;

    mapFnc(@_adonet_initialize,                  'adonet_initialize'                  ) ;
    mapFnc(@_adonet_finalize,                    'adonet_finalize'                    ) ;
    mapFnc(@_adonet_version,                     'adonet_version'                     ) ;
    mapFnc(@_adonet_errmsg,                      'adonet_errmsg'                      ) ;
    mapFnc(@_adonet_cursor_open,                 'adonet_cursor_open'                 ) ;
    mapFnc(@_adonet_cursor_close,                'adonet_cursor_close'                ) ;
    mapFnc(@_adonet_get_last_cursor,             'adonet_get_last_cursor'             ) ;
    mapFnc(@_adonet_sql_connect,                 'adonet_sql_connect'                 ) ;
    mapFnc(@_adonet_sql_disconnect,              'adonet_sql_disconnect'              ) ;
    mapFnc(@_adonet_sql_transact_start,          'adonet_sql_transact_start'          ) ;
    mapFnc(@_adonet_sql_transact_commit,         'adonet_sql_transact_commit'         ) ;
    mapFnc(@_adonet_sql_transact_rollback,       'adonet_sql_transact_rollback'       ) ;
    mapFnc(@_adonet_sql_query_open,              'adonet_sql_query_open'              ) ;
    mapFnc(@_adonet_sql_query_close,             'adonet_sql_query_close'             ) ;
    mapFnc(@_adonet_sql_query_move_first,        'adonet_sql_query_move_first'        ) ;
    mapFnc(@_adonet_sql_query_move_next,         'adonet_sql_query_move_next'         ) ;
    mapFnc(@_adonet_sql_query_eof,               'adonet_sql_query_eof'               ) ;
    mapFnc(@_adonet_sql_query_value,             'adonet_sql_query_value'             ) ;
    mapFnc(@_adonet_sql_query_value_string,      'adonet_sql_query_value_string'      ) ;
    mapFnc(@_adonet_sql_query_value_int64,       'adonet_sql_query_value_int64'       ) ;
    mapFnc(@_adonet_sql_query_value_double,      'adonet_sql_query_value_double'      ) ;
    mapFnc(@_adonet_sql_query_value_date,        'adonet_sql_query_value_date'        ) ;
    mapFnc(@_adonet_sql_query_value_blob,        'adonet_sql_query_value_blob'        ) ;
    mapFnc(@_adonet_sql_query_value_by_id,       'adonet_sql_query_value_by_id'       ) ;
    mapFnc(@_adonet_sql_query_field_id,          'adonet_sql_query_field_id'          ) ;
    mapFnc(@_adonet_sql_query_fields_count,      'adonet_sql_query_fields_count'      ) ;
    mapFnc(@_adonet_sql_query_field_info,        'adonet_sql_query_field_info'        ) ;
    mapFnc(@_adonet_sql_exec,                    'adonet_sql_exec'                    ) ;
    mapFnc(@_adonet_sql_table_prepare,           'adonet_sql_table_prepare'           ) ;
    mapFnc(@_adonet_sql_table_prepared,          'adonet_sql_table_prepared'          ) ;
    mapFnc(@_adonet_sql_table_close,             'adonet_sql_table_close'             ) ;
    mapFnc(@_adonet_sql_table_create_param,      'adonet_sql_table_create_param'      ) ;
    mapFnc(@_adonet_sql_table_set_param_string,  'adonet_sql_table_set_param_string'  ) ;
    mapFnc(@_adonet_sql_table_set_param_null,    'adonet_sql_table_set_param_null'    ) ;
    mapFnc(@_adonet_sql_table_set_param_value,   'adonet_sql_table_set_param_value'   ) ;
    mapFnc(@_adonet_sql_table_set_param_int,     'adonet_sql_table_set_param_int'     ) ;
    mapFnc(@_adonet_sql_table_set_param_int64,   'adonet_sql_table_set_param_int64'   ) ;
    mapFnc(@_adonet_sql_table_set_param_double,  'adonet_sql_table_set_param_double'  ) ;
    mapFnc(@_adonet_sql_table_set_param_blob,    'adonet_sql_table_set_param_blob'    ) ;
    mapFnc(@_adonet_sql_table_set_param_geometry,'adonet_sql_table_set_param_geometry') ;
    mapFnc(@_adonet_sql_table_post,              'adonet_sql_table_post'              ) ;
    mapFnc(@_adonet_set_geometry_type,           'adonet_set_geometry_type'           ) ;

    FIsInitialized := True ;
  end ;

  procedure TGIS_DbAdoNet.finalizeDLL ;
  begin
    if FDLLHandle <> 0 then begin
      FreeLibrary( FDLLHandle ) ;
      FDLLHandle := 0 ;
      FIsInitialized := False ;
    end ;
  end ;

  procedure TGIS_DbAdoNet.check(
    const _status : Integer
  ) ;
  begin
    if _status = ADONET_OK then exit ;
    if _status = ADONET_WARNING then
      TGIS_Logger.AsWarning( GIS_RS_ERR_SERVER_ERROR,
                             GetLastErrorMessage
                            ) ;

    raise EGIS_Exception.Create( GIS_RS_ERR_SERVER_ERROR,
                                 GetLastErrorMessage,
                                 0
                                ) ;
  end ;

 {$ENDIF} // CLR

  function TGIS_DbAdoNet.isInitialized : Boolean ;
  begin
    {$IFDEF CLR}
      Result := True ;
    {$ELSE}
      Result := FIsInitialized ;
    {$ENDIF}
  end ;

  function TGIS_DbAdoNet.InitializeProvider : Boolean ;
  {$IFNDEF CLR}
    var
      ver : Double ;
      i   : Integer ;
  {$ENDIF}
  begin
    {$IFNDEF CLR}
      initializeDLL( ADONETDLLNAME ) ;

      check( _adonet_initialize( FObjHdl ) ) ;
      ver := 0 ;
      check( _adonet_version( ver ) ) ;

      if ver < 8 then
        raise EGIS_Exception.Create( GIS_RS_ERR_FILEMAPPING, ADONETDLLNAME, 35 ) ;

      for i := 0 to BUILTIN_CURSORS -1 do
        cursorOpen( i ) ;
    {$ENDIF}

    Result := inherited InitializeProvider ;
  end ;

  procedure TGIS_DbAdoNet.FinalizeProvider ;
  begin
    {$IFNDEF CLR}
      if isInitialized then begin
        check( _adonet_finalize( FObjHdl ) ) ;
        finalizeDLL ;
      end ;
    {$ENDIF}

    inherited ;
  end ;

  function TGIS_DbAdoNet.GetLastErrorMessage : String ;
  {$IFNDEF CLR}
    var
      ptmp : PAnsiChar ;
  {$ENDIF}
  begin
    {$IFDEF CLR}
      Result := FErrorMessage ;
    {$ELSE}
      ptmp := nil ;
      if isInitialized then
        check( _adonet_errmsg( FObjHdl, ptmp ) ) ;

      Result := String( ptmp ) ;
    {$ENDIF}
  end;

  function TGIS_DbAdoNet.sqlGetParams(
    const _idx : Integer
  ) : {$IFDEF OXYGENE}
        TGIS_StringList ;
      {$ELSE}
        TStringList ;
      {$ENDIF}
  begin
    Result := nil ;
  end ;

  procedure TGIS_DbAdoNet.sqlTableCreateParam(
    const _id       : Integer ;
    const _name     : String ;
    const _type     : TGIS_DataType ;
    const _subtype  : TGIS_SubDataType ;
    const _size     : Integer
  ) ;
  {$IFDEF CLR}
    var
      dbparam : DbParameter ;
      tname   : String ;
      ptype   : &Type ;
      pinfo   : PropertyInfo ;
  {$ENDIF}
  begin
    {$IFDEF CLR}
      if not assigned( FSqlCommands[_id] ) then exit ;

      dbparam := FSqlCommands[_id].CreateParameter ;
      case _type of
        TGIS_DataType.String   : dbparam.DbType := DbType.String ;
        TGIS_DataType.SmallInt : dbparam.DbType := DbType.Int16 ;
        TGIS_DataType.Integer  : dbparam.DbType := DbType.Int32 ;
        TGIS_DataType.LargeInt : dbparam.DbType := DbType.Decimal ;
        TGIS_DataType.Float    : dbparam.DbType := DbType.Double ;
        TGIS_DataType.Boolean  : dbparam.DbType := DbType.Boolean ;
        TGIS_DataType.DateTime : dbparam.DbType := DbType.DateTime ;
        TGIS_DataType.Object   :
          case _subtype of
            TGIS_SubDataType.SqlGeography :
              begin
                {$IFNDEF MONO}
                {$IFDEF ADONET_FULL}
                  if dbparam is SqlParameter then
                    SqlParameter(dbparam).UdtTypeName := 'geography' ;
                {$ELSE}
                //?
                {$ENDIF}
                {$ENDIF}
              end ;
            TGIS_SubDataType.SqlGeometry  :
              begin
                {$IFNDEF MONO}
                {$IFDEF ADONET_FULL}
                  if dbparam is SqlParameter then
                    SqlParameter(dbparam).UdtTypeName := 'geometry' ;
                {$ELSE}
                //?
                {$ENDIF}
                {$ENDIF}
              end ;
          end; // case
        TGIS_DataType.Memo     :
          begin
            dbparam.DbType := DbType.String ;
         end ;
        TGIS_DataType.Blob     :
          begin
            dbparam.DbType := DbType.Binary ;
            ptype := dbparam.GetType();
            if assigned( ptype ) then begin
              tname := ptype.ToString();
              if tname = 'System.Data.SqlServerCe.SqlCeParameter' then begin
                pinfo := ptype.GetProperty('SqlDbType') ;
                if assigned( pinfo ) then
                  pinfo.SetValue( dbparam, SqlDbType.Image, nil ) ;
              end
              else if tname = 'System.Data.OracleClient.OracleParameter' then begin
                pinfo := ptype.GetProperty('OracleType') ;
                if assigned( pinfo ) then begin
                  ptype := pinfo.PropertyType;
                  pinfo.SetValue( dbparam, System.Enum.Parse(ptype,'Blob'), nil ) ;
                end ;
              end ;
            end ;
          end ;
        TGIS_DataType.Unknown  : ;
      end ; // case

      dbparam.ParameterName := _name ;
      dbparam.Value         := DBNull.Value ;

      FSqlCommands[_id].Parameters.Add( dbparam ) ;
    {$ELSE}
      check( _adonet_sql_table_create_param(
              FObjHdl,
               _id,
               PAnsiChar( AnsiString( _name ) ),
               Integer( _type ),
               Integer( _subtype ),
               _size
              )
            ) ;
    {$ENDIF}
  end ;

  procedure TGIS_DbAdoNet.sqlConnect(
    const _folder        : String ;
    const _sqlParameters : {$IFDEF OXYGENE}
                             TGIS_Strings
                           {$ELSE}
                             TStrings
                           {$ENDIF}
  ) ;
  var
    cstr : String ;
    {$IFNDEF CLR}
      prov : String ;
    {$ENDIF}
  begin
    if not isInitialized then exit ;

    cstr := _sqlParameters.Values[ GIS_INI_LAYERSQL_CONNECTOR_ADONET ] ;

    cstr := sqlPathAbsolute( _folder, cstr, ';', 'Data Source', '=' ) ;

    {$IFDEF CLR}
      FSqlConnection := SharedConnections.OpenADONET(
                          cstr,
                          _sqlParameters.Values[ GIS_INI_LAYERSQL_CONNECTOR_PROVIDER ],
                          _sqlParameters
                         ) ;
    {$ELSE}
      prov := _sqlParameters.Values[ GIS_INI_LAYERSQL_CONNECTOR_PROVIDER ] ;

      check( _adonet_sql_connect(
                FObjHdl,
                PAnsiChar( AnsiString( cstr ) ) ,
                PAnsiChar( AnsiString( prov ) )
              )
            ) ;
    {$ENDIF}
    if IsOracle or IsPostgreSql then
      FParameterPrefix := ':' ;
  end ;

  procedure TGIS_DbAdoNet.sqlDisconnect ;
  begin
    if not isInitialized then exit ;
    {$IFDEF CLR}
      SharedConnections.CloseADONET( FSqlConnection ) ;
    {$ELSE}
      check( _adonet_sql_disconnect( FObjHdl ) ) ;
    {$ENDIF}
  end ;

  procedure TGIS_DbAdoNet.sqlTransactUpdateStart ;
  begin
    if not isInitialized then exit ;
    if options.UpdateTransact then
    {$IFDEF CLR}
      if assigned( FSqlConnection ) then
        FSqlTransaction := FSqlConnection.BeginTransaction( IsolationLevel.ReadCommitted ) ;
    {$ELSE}
      check( _adonet_sql_transact_start( FObjHdl ) )
    {$ENDIF}
  end ;

  procedure TGIS_DbAdoNet.sqlTransactUpdateCommit ;
  begin
    if not isInitialized then exit ;
    if options.UpdateTransact then
    {$IFDEF CLR}
      if assigned( FSqlTransaction ) then
        FSqlTransaction.Commit ;
    {$ELSE}
      check( _adonet_sql_transact_commit( FObjHdl ) )
    {$ENDIF}
  end ;

  procedure TGIS_DbAdoNet.sqlTransactRestructStart ;
  begin
    if not isInitialized then exit ;
    if options.RestructTransact then
    {$IFDEF CLR}
      if assigned( FSqlConnection ) then
        FSqlTransaction := FSqlConnection.BeginTransaction( IsolationLevel.ReadCommitted ) ;
    {$ELSE}
      check( _adonet_sql_transact_start( FObjHdl ) )
    {$ENDIF}
  end ;

  procedure TGIS_DbAdoNet.sqlTransactRestructCommit ;
  begin
    if not isInitialized then exit ;
    if options.RestructTransact then
    {$IFDEF CLR}
      if assigned( FSqlTransaction ) then
        FSqlTransaction.Commit ;
    {$ELSE}
      check( _adonet_sql_transact_commit( FObjHdl ) )
    {$ENDIF}
  end ;

  procedure TGIS_DbAdoNet.sqlTransactGlobalUpdateStart ;
  begin
    if not isInitialized then exit ;
    if options.GlobalUpdateTransact then
    {$IFDEF CLR}
      if assigned( FSqlConnection ) then
        FSqlTransaction := FSqlConnection.BeginTransaction( IsolationLevel.ReadCommitted ) ;
    {$ELSE}
      check( _adonet_sql_transact_start( FObjHdl ) )
    {$ENDIF}
  end ;

  procedure TGIS_DbAdoNet.sqlTransactGlobalUpdateCommit ;
  begin
    if not isInitialized then exit ;
    if options.GlobalUpdateTransact then
    {$IFDEF CLR}
      if assigned( FSqlTransaction ) then
        FSqlTransaction.Commit ;
    {$ELSE}
      check( _adonet_sql_transact_commit( FObjHdl ) )
    {$ENDIF}
  end ;

  procedure TGIS_DbAdoNet.sqlTransactRollback ;
  begin
    if not isInitialized then exit ;
    {$IFDEF CLR}
      if assigned( FSqlTransaction ) then
        FSqlTransaction.Rollback ;
    {$ELSE}
      check( _adonet_sql_transact_rollback( FObjHdl ) )
    {$ENDIF}
  end ;

  procedure TGIS_DbAdoNet.sqlQueryOpen(
    const _query   : String ;
    const _cursor  : Integer
  ) ;
  var
    tmp : String ;
  begin
    if not isInitialized then exit ;
    tmp := _query ;
    sqlQueryClose( _cursor ) ;

    if assigned( FOnSQLExecute ) then
      FOnSQLExecute( tmp ) ;
    {$IFDEF CLR}
      FCursorDB[_cursor].FSqlCommand := FSqlConnection.CreateCommand ;
      FCursorDB[_cursor].FSqlCommand.CommandType := CommandType.Text ;
      FCursorDB[_cursor].FSqlCommand.CommandText := tmp ;
      FCursorDB[_cursor].FSqlCommand.Transaction := FSqlTransaction ;

      if FCursorDB[_cursor].FSqlCommand.Connection.State <> ConnectionState.Open then
        FSqlConnection.Open ;

      FCursorDB[_cursor].FSqlCommand.CommandTimeout := Max( FSqlConnection.ConnectionTimeout,
                                                            FCursorDB[_cursor].FSqlCommand.CommandTimeout
                                                          ) ;
      sqlQueryMoveFirst( _cursor ) ;
    {$ELSE}
      check( _adonet_sql_query_open(
                FObjHdl,
                PAnsiChar( AnsiString( tmp ) ),
                _cursor
              )
            ) ;
    {$ENDIF}
  end ;

  procedure TGIS_DbAdoNet.sqlQueryReset(
    const _cursor  : Integer
  ) ;
  begin

  end ;

  procedure TGIS_DbAdoNet.sqlQueryClose(
    const _cursor : Integer
  ) ;
  begin
    if not isInitialized then exit ;
    {$IFDEF CLR}
      FCursorDB[_cursor].FSqlDataReaderEof := True ;
      if assigned( FCursorDB[_cursor].FSqlDataReader ) then begin
        FCursorDB[_cursor].FSqlDataReader.Close ;
        FreeObject( FCursorDB[_cursor].FSqlDataReader ) ;
      end ;
      if assigned( FCursorDB[_cursor].FSqlCommand ) then
        FreeObject( FCursorDB[_cursor].FSqlCommand ) ;

      FCursorDB[_cursor].FSqlSchemaTable := nil ;
      FCursorDB[_cursor].FSqlGeometryFieldId := 0 ;
    {$ELSE}
      check( _adonet_sql_query_close( FObjHdl, _cursor ) ) ;
    {$ENDIF}
  end ;

  procedure TGIS_DbAdoNet.sqlTableClose(
    const _id : Integer
  ) ;
  begin
    if not isInitialized then exit ;
    {$IFDEF CLR}
      if assigned( FSqlCommands[_id] ) then
        FreeObject( FSqlCommands[_id] ) ;
    {$ELSE}
      check( _adonet_sql_table_close( FObjHdl, _id ) ) ;
    {$ENDIF}
  end ;

  procedure TGIS_DbAdoNet.sqlTablePost(
    const _id : Integer
  ) ;
  begin
    {$IFDEF CLR}
      if assigned( FSqlCommands[_id] ) then begin
        if assigned( FOnSQLExecute ) then
          FOnSQLExecute( ' Post table ' + IntToStr( _id ) ) ;

        if FSqlCommands[_id].Connection.State <> ConnectionState.Open then
          FSqlConnection.Open ;

        FSqlCommands[_id].ExecuteNonQuery ;
      end ;
    {$ELSE}
      check( _adonet_sql_table_post( FObjHdl, _id ) ) ;
    {$ENDIF}

    if not InBatchMode then
      sqlTableClose( _id ) ;
  end ;

  procedure TGIS_DbAdoNet.sqlTableExec(
    const _id : Integer
  ) ;
  begin
    {$IFDEF CLR}
      if assigned( FSqlCommands[_id] ) then begin
        if assigned( FOnSQLExecute ) then
          FOnSQLExecute( ' Post table ' + IntToStr( _id ) ) ;

        if FSqlCommands[_id].Connection.State <> ConnectionState.Open then
          FSqlConnection.Open ;

        FSqlCommands[_id].ExecuteNonQuery ;
      end ;
    {$ELSE}
      check( _adonet_sql_table_post( FObjHdl, _id ) ) ;
    {$ENDIF}
  end ;

  procedure TGIS_DbAdoNet.sqlTableOpenRead(
    const _id     : Integer ;
    const _query  : String
  ) ;
  begin

  end ;

  procedure TGIS_DbAdoNet.sqlTableAppend(
    const _id       : Integer ;
    const _query    : String
  ) ;
  begin
    {$IFDEF CLR}
      if assigned( FOnSQLExecute ) then
        FOnSQLExecute( _query ) ;

      if FSqlCommands[_id] = nil then begin
        FSqlCommands[_id] := FSqlConnection.CreateCommand ;
        FSqlCommands[_id].Transaction := FSqlTransaction ;
        FSqlCommands[_id].Parameters.Clear ;
        FSqlCommands[_id].CommandType := CommandType.Text ;
        FSqlCommands[_id].CommandText := _query ;
      end ;
    {$ELSE}
      check( _adonet_sql_table_prepare( FObjHdl, _id, PAnsiChar(AnsiString(_query)) ) ) ;
    {$ENDIF}
  end ;

  function TGIS_DbAdoNet.sqlTablePrepared(
    const _id  : Integer
  ) : Boolean ;
  {$IFNDEF CLR}
  var
    val : Integer ;
  {$ENDIF}
  begin
    {$IFDEF CLR}
      Result := FSqlCommands[_id] <> nil ;
    {$ELSE}
      check( _adonet_sql_table_prepared( FObjHdl, _id, val ) ) ;
      Result := val = 1 ;
    {$ENDIF}
  end ;

  procedure TGIS_DbAdoNet.sqlTableOpenWrite(
    const _id     : Integer ;
    const _query  : String
   ) ;
  begin
    {$IFDEF CLR}
      if assigned( FOnSQLExecute ) then
        FOnSQLExecute( _query ) ;

      if FSqlCommands[_id] = nil then begin
        FSqlCommands[_id] := FSqlConnection.CreateCommand ;
        FSqlCommands[_id].Transaction := FSqlTransaction ;
        FSqlCommands[_id].Parameters.Clear ;
        FSqlCommands[_id].CommandType := CommandType.Text ;
        FSqlCommands[_id].CommandText := _query ;
      end
      else begin
        FSqlCommands[_id].Parameters.Clear ;
        FSqlCommands[_id].CommandType := CommandType.Text ;
        FSqlCommands[_id].CommandText := _query ;
      end ;
    {$ELSE}
      check( _adonet_sql_table_prepare( FObjHdl, _id, PAnsiChar(AnsiString(_query)) ) ) ;
    {$ENDIF}
  end ;

  procedure TGIS_DbAdoNet.sqlExec(
    const _command : String
  ) ;
  {$IFDEF CLR}
  var
    cmd : DbCommand ;
  {$ENDIF}
  begin
    if not isInitialized then exit ;
    if IsStringEmpty( _command ) then exit ;

    if assigned( FOnSQLExecute ) then
      FOnSQLExecute( _command ) ;

    {$IFDEF CLR}
      cmd := FSqlConnection.CreateCommand ;
      cmd.Transaction := FSqlTransaction ;
      cmd.CommandText    := _command ;
      cmd.CommandTimeout := cmd.CommandTimeout * 10 ;
      try
        cmd.ExecuteNonQuery ;
      finally
        FreeObject( cmd ) ;
      end ;
    {$ELSE}
      check( _adonet_sql_exec(
                FObjHdl,
                PAnsiChar( AnsiString( _command ) )
              )
            ) ;
    {$ENDIF}
  end ;

  procedure TGIS_DbAdoNet.sqlQueryMoveFirst(
    const _cursor : Integer
  ) ;
  begin
    if isInitialized then
    {$IFDEF CLR}
      if ( FCursorDB[_cursor].FSqlDataReader = nil ) then begin
        FCursorDB[_cursor].FSqlDataReader := FCursorDB[_cursor].FSqlCommand.ExecuteReader ;
        sqlQueryMoveNext( _cursor ) ;
      end ;
    {$ELSE}
       check( _adonet_sql_query_move_first( FObjHdl, _cursor ) ) ;
    {$ENDIF}
  end ;

  procedure TGIS_DbAdoNet.sqlQueryMoveNext(
    const _cursor : Integer
  ) ;
  begin
    if isInitialized then
    {$IFDEF CLR}
      if assigned( FCursorDB[_cursor].FSqlDataReader ) then
        FCursorDB[_cursor].FSqlDataReaderEof := not FCursorDB[_cursor].FSqlDataReader.Read
      else
        FCursorDB[_cursor].FSqlDataReaderEof := True ;
    {$ELSE}
      check( _adonet_sql_query_move_next( FObjHdl, _cursor ) ) ;
    {$ENDIF}
  end ;

  function  TGIS_DbAdoNet.sqlQueryEof(
    const _cursor : Integer
  ) : Boolean ;
  {$IFNDEF CLR}
  var
    eof : Integer ;
  {$ENDIF}
  begin
    {$IFDEF CLR}
      Result := FCursorDB[_cursor].FSqlDataReaderEof or
                not assigned( FCursorDB[_cursor].FSqlDataReader ) ;
    {$ELSE}
    if isInitialized then
      check( _adonet_sql_query_eof( FObjHdl, eof, _cursor ) ) ;
      Result := eof = 1 ;
    {$ENDIF}
  end ;

  function TGIS_DbAdoNet.sqlTableEof(
    const _id : Integer
  ) : Boolean ;
  begin
    Result := True ;
  end ;

  function TGIS_DbAdoNet.sqlQueryGetFieldIndex(
    const _name    : String ;
    const _cursor  : Integer
   ) : Integer ;
  {$IFDEF CLR}
   var
     i : Integer ;
  {$ENDIF}
  begin
    {$IFDEF CLR}
      if assigned( FCursorDB[_cursor].FSqlDataReader ) then begin
        if not assigned( FCursorDB[_cursor].FSqlSchemaTable ) then
          FCursorDB[_cursor].FSqlSchemaTable := FCursorDB[_cursor].FSqlDataReader.GetSchemaTable ;
          Result := -1 ;
          for i := 0 to FCursorDB[_cursor].FSqlDataReader.FieldCount-1 do
            if CompareText( FCursorDB[_cursor].FSqlSchemaTable.Rows[i].Item['ColumnName'].ToString, _name) = 0 then begin
              Result := i ;
              break ;
            end;
      end
      else
        Result := -1 ;
    {$ELSE}
      if isInitialized then
         check( _adonet_sql_query_field_id(
                  FObjHdl,
                  PAnsiChar( AnsiString( _name ) ),
                  Result,
                  _cursor
                 )
               ) ;
    {$ENDIF}
  end ;

  function TGIS_DbAdoNet.sqlQueryGetField(
    const _name   : String ;
    const _cursor : Integer
   ) : Variant ;
  begin
    {$IFDEF CLR}
      try
        {$IFDEF OXYGENE}
          if not FCursorDB[_cursor].FSqlDataReaderEof then
            Result := FCursorDB[_cursor].FSqlDataReader[ _name ]
          else
            Result := Unassigned ;
        {$ELSE}
          Result := Variant(FCursorDB[_cursor].FSqlDataReader[ _name ]) ;
        {$ENDIF}
      except
        Result := Unassigned ;
      end ;
    {$ELSE}
    if isInitialized then
      Result := sqlQueryGetFieldById( sqlQueryGetFieldIndex( _name, _cursor ),
                                      _cursor
                                     ) ;
    {$ENDIF}
  end ;

  function TGIS_DbAdoNet.sqlQueryGetFieldById(
    const _id     : Integer ;
    const _cursor : Integer
  ) : Variant ;
  {$IFNDEF CLR}
  var
    val   : OleVariant ;
    tp    : Integer ;
    size  : Integer ;
    prec  : Integer ;
    scale : Integer ;
    pname : PAnsiChar ;
  {$ENDIF}
  begin
    Result := Unassigned ;
    if _id < 0 then exit ;
    {$IFDEF CLR}
      try
        {$IFDEF OXYGENE}
          if not FCursorDB[_cursor].FSqlDataReaderEof then
            Result := FCursorDB[_cursor].FSqlDataReader[ _id ]
          else
            Result := Unassigned ;
        {$ELSE}
          Result := Variant(FCursorDB[_cursor].FSqlDataReader[ _id ]) ;
        {$ENDIF}
      except
        Result := Unassigned ;
      end ;
    {$ELSE}
    if isInitialized then begin
      pname := nil ;
      check( _adonet_sql_query_field_info( FObjHdl, _id, pname, tp, size, prec, scale, _cursor ) ) ;
      case TGIS_DataType(tp) of
        TGIS_DataType.String :
          check( _adonet_sql_query_value_string(
                    FObjHdl,
                    _id,
                    val,
                    _cursor
                  )
                ) ;
        TGIS_DataType.SmallInt,
        TGIS_DataType.Integer,
        TGIS_DataType.LargeInt :
          check( _adonet_sql_query_value_int64(
                    FObjHdl,
                    _id,
                    val,
                    _cursor
                  )
                ) ;
        TGIS_DataType.Float :
          check( _adonet_sql_query_value_double(
                    FObjHdl,
                    _id,
                    val,
                    _cursor
                  )
                ) ;
        TGIS_DataType.DateTime :
          check( _adonet_sql_query_value_date(
                    FObjHdl,
                    _id,
                    val,
                    _cursor
                  )
                ) ;
        TGIS_DataType.Boolean :
          check( _adonet_sql_query_value_int64(
                    FObjHdl,
                    _id,
                    val,
                    _cursor
                  )
                )
        else
          check( _adonet_sql_query_value_by_id(
                    FObjHdl,
                    _id,
                    val,
                    _cursor
                  )
                ) ;
        end ;

        Result := val ;
      end ;
    {$ENDIF}
  end ;

  function TGIS_DbAdoNet.sqlTableGetField(
    const _id   : Integer ;
    const _name : String
   ) : Variant ;
  begin
    Result := Unassigned ;
  end ;

  function TGIS_DbAdoNet.sqlTableGetParam(
    const _id   : Integer ;
    const _name : String
   ) : Variant ;
  begin
    Result := Unassigned ;
  end ;

  function TGIS_DbAdoNet.sqlBindField(
    const _field  : String ;
    const _cursor : Integer
   ) : Integer ;
  begin
    Result := sqlQueryGetFieldIndex( _field, _cursor ) ;
  end ;

  procedure TGIS_DbAdoNet.sqlTableSetField(
    const _id           : Integer ;
    const _name         : String ;
    const _val          : Variant ;
    const _defSize      : Integer
   ) ;
   var
   {$IFNDEF CLR}
     y,m,d : Word ;
     data  : OleVariant ;
   {$ENDIF}
     pname : String ;
  begin
    {$IFDEF CLR}
      if IsStringEmpty( _name ) or ( FSqlCommands[_id] = nil ) then exit ;

      if FSqlCommands[_id].Parameters.IndexOf(ParameterPrefix +_name)>=0 then
        pname := ParameterPrefix +_name
      else
        pname := _name ;
      if VarIsNull( _val ) or VarIsEmpty( _val ) then
        FSqlCommands[_id].Parameters[ pname ].Value := DBNull.Value
      else
        FSqlCommands[_id].Parameters[ pname ].Value := _val ;
    {$ELSE}
      pname := ParameterPrefix +_name ;
      case GetVariantType( _val ) of
        TGIS_VariantType.AnsiString  :
          begin
            data := TEncoding.UTF8.GetBytes( ConvertVar2WStrCP( _val, 65001 ) ) ;
            check( _adonet_sql_table_set_param_string(
                     FObjHdl,
                     _id,
                     PAnsiChar( AnsiString( pname ) ),
                     data
                    )
                  ) ;
          end ;
        TGIS_VariantType.WideString  :
          begin
            data := TEncoding.UTF8.GetBytes( VarToString(_val) ) ;
            check( _adonet_sql_table_set_param_string(
                     FObjHdl,
                     _id,
                     PAnsiChar( AnsiString( pname ) ),
                     data
                    )
                  ) ;
          end;
        TGIS_VariantType.Boolean     :
          if VarToBoolean( _val ) then
            check( _adonet_sql_table_set_param_int(
                     FObjHdl,
                     _id,
                     PAnsiChar( AnsiString( pname ) ),
                     1
                    )
                  )
          else
            check( _adonet_sql_table_set_param_int(
                     FObjHdl,
                     _id,
                     PAnsiChar( AnsiString( pname ) ),
                     0
                    )
                  ) ;
        TGIS_VariantType.Int,
        TGIS_VariantType.UInt        :
            check( _adonet_sql_table_set_param_int(
                     FObjHdl,
                     _id,
                     PAnsiChar( AnsiString( pname ) ),
                     VarToInt32(_val)
                    )
                  ) ;
        TGIS_VariantType.Int64,
        TGIS_VariantType.UInt64      :
            check( _adonet_sql_table_set_param_int64(
                     FObjHdl,
                     _id,
                     PAnsiChar( AnsiString( pname ) ),
                     VarToInt64(_val)
                    )
                  ) ;
        TGIS_VariantType.Float       :
            check( _adonet_sql_table_set_param_double(
                     FObjHdl,
                     _id,
                     PAnsiChar( AnsiString( pname ) ),
                     VarToDouble(_val)
                    )
                  ) ;
        TGIS_VariantType.DateTime    :
          begin
            DecodeDate( VarToDateTime( _val ), y, m, d ) ;
            data := TEncoding.UTF8.GetBytes( FormatDateTime('yyyy-MM-dd',
                                                            EncodeDate(y,m,d)
                                                           )
                                           ) ;
            check( _adonet_sql_table_set_param_string(
                     FObjHdl,
                     _id,
                     PAnsiChar( AnsiString( pname ) ),
                     data
                    )
                  ) ;
          end ;
        TGIS_VariantType.Nothing     :
            check( _adonet_sql_table_set_param_null(
                     FObjHdl,
                     _id,
                     PAnsiChar( AnsiString( pname ) )
                    )
                  ) ;
      end ;
    {$ENDIF}
  end ;

  function TGIS_DbAdoNet.sqlGetBindedField(
    const _field         : Integer ;
    const _rebindFields  : Boolean ;
    const _cursor        : Integer
   ) : Variant ;
  begin
    Result := sqlQueryGetFieldById( _field, _cursor ) ;
  end ;

  procedure TGIS_DbAdoNet.sqlQueryStructure(
    const _tableName     : String ;
    const _uidName       : String ;
    const _addFieldEvent : TGIS_LayerAddFieldEvent
   ) ;
  var
    i, cnt  : Integer ;
    fname   : String ;
    fsize   : Integer;
    fprec   : Integer ;
    fscale  : Integer ;
  {$IFDEF CLR}
    ftype   : TGIS_DataType ;
    stype   : System.Type ;
  {$ELSE}
    tp      : Integer ;
    pname   : PAnsiChar ;
  {$ENDIF}
  begin
    {$IFDEF CLR}
      if assigned( FCursorDB[0].FSqlDataReader ) then
        cnt := FCursorDB[0].FSqlDataReader.FieldCount
      else
        cnt := 0 ;

      for i := 0 to cnt - 1 do begin
        if not assigned( FCursorDB[0].FSqlSchemaTable ) then
          FCursorDB[0].FSqlSchemaTable := FCursorDB[0].FSqlDataReader.GetSchemaTable ;

        fname  := FCursorDB[0].FSqlSchemaTable.Rows[i].Item['ColumnName'].ToString ;
        fsize  := StrToInt( FCursorDB[0].FSqlSchemaTable.Rows[i].Item['ColumnSize'].ToString ) ;
        fprec  := StrToIntDef( FCursorDB[0].FSqlSchemaTable.Rows[i].Item['NumericPrecision'].ToString, 0 ) ;
        fscale := StrToIntDef( FCursorDB[0].FSqlSchemaTable.Rows[i].Item['NumericScale'].ToString, 0 ) ;
        if fscale = 255 then
          fscale := 0 ; // predefined types like int, bigint

        stype := FCursorDB[0].FSqlDataReader.GetFieldType( i ) ;
        case System.Type.GetTypeCode( stype ) of
          System.TypeCode.String,
          System.TypeCode.Char    : ftype := TGIS_DataType.String ;
          System.TypeCode.Byte,
          System.TypeCode.SByte,
          System.TypeCode.Int16,
          System.TypeCode.UInt16  : ftype := TGIS_DataType.SmallInt ;
          System.TypeCode.Decimal,
          System.TypeCode.Int32,
          System.TypeCode.UInt32  : ftype := TGIS_DataType.Integer ;
          System.TypeCode.Int64,
          System.TypeCode.UInt64  : ftype := TGIS_DataType.LargeInt ;
          System.TypeCode.Double,
          System.TypeCode.Single  : ftype := TGIS_DataType.Float ;
          System.TypeCode.Boolean : ftype := TGIS_DataType.Boolean ;
          System.TypeCode.DateTime: ftype := TGIS_DataType.DateTime ;
          System.TypeCode.Object  : ftype := TGIS_DataType.Object
        else
          ftype := TGIS_DataType.Unknown ;
        end ; // case

         if (ftype = TGIS_DataType.Unknown) and assigned(stype) then begin
            if stype.Equals(typeOf(TBytes)) or stype.Equals(typeOf(SqlBinary)) then
              ftype := TGIS_DataType.Blob
            else if stype.Equals(typeOf(SqlGuid)) then
              ftype := TGIS_DataType.Unknown
            else if stype.Equals(typeOf(SqlBoolean)) then
              ftype := TGIS_DataType.Boolean
            else if stype.Equals(typeOf(SqlDateTime)) then
              ftype := TGIS_DataType.DateTime
            else if stype.Equals(typeOf(SqlDecimal)) then
              ftype := TGIS_DataType.Integer
            else if stype.Equals(typeOf(SqlDouble)) or  stype.Equals(typeOf(SqlSingle)) then
              ftype := TGIS_DataType.Float
            else if stype.Equals(typeOf(SqlInt16)) or stype.Equals(typeOf(SqlByte)) then
              ftype := TGIS_DataType.SmallInt
            else if stype.Equals(typeOf(SqlInt32)) then
              ftype := TGIS_DataType.Integer
            else if stype.Equals(typeOf(SqlInt64)) then
              ftype := TGIS_DataType.LargeInt
            else if stype.Equals(typeOf(SqlMoney)) then
              ftype := TGIS_DataType.Float
            else if stype.Equals(typeOf(SqlString)) then
              ftype := TGIS_DataType.String
          end ;

          if ( ftype = TGIS_DataType.String ) and ( fsize >= 2147483647 ) then
            fsize := GIS_SQL_MEMO_SIZE ;

          if ( ftype = TGIS_DataType.Float ) and ( fscale = 0 ) then
            fscale := 6 ;

          case ftype of
            TGIS_DataType.String    : _addFieldEvent( _uidName, fname,
                                                      TGIS_FieldType.String,
                                                      fsize, 0 ) ;
            TGIS_DataType.Integer,
            TGIS_DataType.SmallInt,
            TGIS_DataType.LargeInt  : _addFieldEvent( _uidName, fname,
                                                      TGIS_FieldType.Number,
                                                      fprec, fscale ) ;
            TGIS_DataType.Float     : _addFieldEvent( _uidName, fname,
                                                      TGIS_FieldType.Float,
                                                      fprec, fscale ) ;
            TGIS_DataType.Boolean   : _addFieldEvent( _uidName, fname,
                                                      TGIS_FieldType.Boolean,
                                                      0, 0 ) ;
            TGIS_DataType.DateTime  : _addFieldEvent( _uidName, fname,
                                                      TGIS_FieldType.Date,
                                                      0, 0 ) ;
          end ;
      end ;
    {$ELSE}
      if not isInitialized then exit ;

      pname := nil ;
      _adonet_sql_query_fields_count( FObjHdl, cnt, 0 ) ;

      for i := 0 to cnt - 1 do begin
        check( _adonet_sql_query_field_info( FObjHdl, i, pname, tp, fsize, fprec, fscale, 0 ) ) ;

        if ( TGIS_DataType( tp ) = TGIS_DataType.Float ) and ( fscale = 0 ) then
          fscale := 6 ;

        if pname <> nil then begin
          fname := String( pname ) ;
          case TGIS_DataType( tp ) of
            TGIS_DataType.String    : _addFieldEvent( _uidName, fname,
                                                      TGIS_FieldType.String,
                                                      fsize, 0 ) ;
            TGIS_DataType.Integer,
            TGIS_DataType.SmallInt,
            TGIS_DataType.LargeInt  : _addFieldEvent( _uidName, fname,
                                                      TGIS_FieldType.Number,
                                                      fprec, fscale ) ;
            TGIS_DataType.Float     : _addFieldEvent( _uidName, fname,
                                                      TGIS_FieldType.Number,
                                                      fprec, fscale ) ;
            TGIS_DataType.Boolean   : _addFieldEvent( _uidName, fname,
                                                      TGIS_FieldType.Boolean,
                                                      0, 0 ) ;
            TGIS_DataType.DateTime  : _addFieldEvent( _uidName, fname,
                                                      TGIS_FieldType.Date,
                                                      0, 0 ) ;
          end ;
        end ;
      end ;
    {$ENDIF}
  end ;

  function TGIS_DbAdoNet.sqlQueryNameGEOUID(
    const _field   : String ;
    const _fieldEx : String ;
    const _cursor  : Integer
   ) : Integer ;
   var
     idx : Integer ;
  begin
    idx := sqlQueryGetFieldIndex( _field, _cursor ) ;
    if idx < 0 then
      Result := 1
    else
      Result := 0 ;
  end ;

  function TGIS_DbAdoNet.sqlQueryGetGEOUID(
    const _field   : String ;
    const _cursor  : Integer
   ) : Variant ;
  {$IFNDEF CLR}
    var
      val : OleVariant ;
  {$ENDIF}
  begin
    {$IFDEF CLR}
      try
        Result := sqlQueryGetField( _field, _cursor ) ;
      except
        Result := -1 ;
      end ;
    {$ELSE}
    if isInitialized then
      check( _adonet_sql_query_value(
              FObjHdl,
              PAnsiChar( AnsiString( _field ) ),
              val,
              _cursor
             )
            ) ;
      Result := val ;
    {$ENDIF}
  end ;

  function TGIS_DbAdoNet.sqlQueryGetGEOUID(
    const _index   : Integer ;
    const _cursor  : Integer
   ) : Variant ;
  {$IFNDEF CLR}
  var
    val : OleVariant ;
  {$ENDIF}
  begin
    {$IFDEF CLR}
      try
        Result := sqlQueryGetFieldById( _index, _cursor ) ;
      except
        Result := -1 ;
      end ;
    {$ELSE}
    if isInitialized then
      check( _adonet_sql_query_value_by_id(
                FObjHdl,
                _index,
                val,
                _cursor
              )
            ) ;
      Result := val ;
    {$ENDIF}
  end ;

  function TGIS_DbAdoNet.sqlQueryGetSHAPETYPE(
    const _field  : String ;
    const _cursor : Integer
   ) : Variant ;
  {$IFNDEF CLR}
    var
      val : OleVariant ;
  {$ENDIF}
  begin
    {$IFDEF CLR}
      try
        Result := sqlQueryGetField( _field, _cursor ) ;
      except
        Result := -1 ;
      end ;
    {$ELSE}
    if isInitialized then
      check( _adonet_sql_query_value(
              FObjHdl,
              PAnsiChar( AnsiString( _field ) ),
              val,
              _cursor
             )
            ) ;
      Result := val ;
    {$ENDIF}
  end ;

  function TGIS_DbAdoNet.sqlQueryGetXMIN(
    const _field   : String ;
    const _cursor  : Integer
   ) : Variant ;
  {$IFNDEF CLR}
    var
      val : OleVariant ;
  {$ENDIF}
  begin
    {$IFDEF CLR}
      try
        Result := sqlQueryGetField( _field, _cursor ) ;
      except
        Result := -1 ;
      end ;
    {$ELSE}
    if isInitialized then
      check( _adonet_sql_query_value(
              FObjHdl,
              PAnsiChar( AnsiString( _field ) ),
              val,
              _cursor
             )
            ) ;
      Result := val ;
    {$ENDIF}
  end ;

  function TGIS_DbAdoNet.sqlQueryGetYMIN(
    const _field   : String ;
    const _cursor  : Integer
   ) : Variant ;
  {$IFNDEF CLR}
    var
      val : OleVariant ;
  {$ENDIF}
  begin
    {$IFDEF CLR}
      try
        Result := sqlQueryGetField( _field, _cursor ) ;
      except
        Result := -1 ;
      end ;
    {$ELSE}
    if isInitialized then
      check( _adonet_sql_query_value(
              FObjHdl,
              PAnsiChar( AnsiString( _field ) ),
              val,
              _cursor
             )
            ) ;
      Result := val ;
    {$ENDIF}
  end ;

  function TGIS_DbAdoNet.sqlQueryGetGeomVAR(
    const _field       : String ;
    const _fieldEx     : String ;
    const _cursor      : Integer;
    const _forceValue  : Boolean = False
   ) : OleVariant ;
  begin
    if IsStringEmpty( _fieldEx ) then
      Result := sqlQueryGetField( _field, _cursor )
    else
      Result := sqlQueryGetField( _fieldEx, _cursor )
  end;

  {$IFDEF CLR}
    function TGIS_DbAdoNet.sqlQueryGetGeomPtr(
      const _field    : String ;
      const _fieldEx  : String ;
      const _cursor   : Integer;
        var _size     : Integer
     ) : TGIS_Bytes ;
    var
      buf : TBytes ;
    begin
      buf := TBytes( sqlQueryGetField( _field, _cursor ) ) ;
      Result := TGIS_Bytes.Create( buf, 0 ) ;
      _size  := length( buf ) ;
    end ;

    function TGIS_DbAdoNet.sqlQueryGetGeomPtr(
      const _field    : String ;
      const _cursor   : Integer;
        var _size     : Integer
     ) : TBytes ;
  {$ELSE}
    function TGIS_DbAdoNet.sqlQueryGetGeomPtr(
      const _field    : String ;
      const _fieldEx  : String ;
      const _cursor   : Integer ;
        var _size     : Integer
    ) : Pointer ;
  {$ENDIF}
    {$IFNDEF CLR}
    var
      ptr   : Pointer ;
      size  : Integer ;
    {$ENDIF}
  begin
    Result := nil ;
    {$IFDEF CLR}
      if FCursorDB[_cursor].FSqlGeometryFieldId = 0 then
        FCursorDB[_cursor].FSqlGeometryFieldId := sqlQueryGetFieldIndex( _field, _cursor ) ;

      if (GeometryType = TGIS_SubDataType.SqlGeometry) or (GeometryType = TGIS_SubDataType.SqlGeography) then
        {$IFDEF ADONET_FULL}
        Result := (FCursorDB[_cursor].FSqlDataReader as SqlDataReader).GetSqlBytes(FCursorDB[_cursor].FSqlGeometryFieldId).Buffer
        {$ELSE}
         Result := nil //?
        {$ENDIF}
      else if (GeometryType = TGIS_SubDataType.Native ) then
        Result := TBytes(sqlQueryGetFieldById( FCursorDB[_cursor].FSqlGeometryFieldId, _cursor ))
      else begin
        Result := nil ;
        _size  := 0 ;
        exit ;
      end ;

      _size := length( Result ) ;
    {$ELSE}
      ptr := nil ;
      if isInitialized then
        check( _adonet_sql_query_value_blob(
                  FObjHdl,
                  PAnsiChar( AnsiString( _field ) ),
                  size,
                  {$IFDEF LEVEL_XE2_RTL} IntPtr(ptr) {$ELSE} ptr {$ENDIF},
                  _cursor
                 )
               ) ;
       Result := ptr ;
       _size  := size ;
    {$ENDIF}
  end ;

  procedure TGIS_DbAdoNet.sqlQueryUnPrepareGetGeom(
    const _cursor        : Integer
  ) ;
  begin
    // for safe inheritance
  end ;

  function TGIS_DbAdoNet.sqlQueryGetBlob(
    const _name    : String ;
    const _cursor  : Integer
  ) : TStream ;
  var
    buf : TBytes ;
  begin
    buf := TBytes( sqlQueryGetField( _name, _cursor ) ) ;

    Result := TGIS_MemoryStream.Create ;
    {$IFDEF CLR}
     Result.WriteBuffer( buf, length( buf ) ) ;
    {$ELSE}
     Result.WriteBuffer( buf[0], Length( buf ) ) ;
    {$ENDIF}
    Result.Position := 0 ;
  end ;

  function TGIS_DbAdoNet.sqlQueryGetGeomObj(
    const _field   : String ;
    const _cursor  : Integer
  ) : TObject ;
  begin
    {$IFDEF CLR}
      if FCursorDB[_cursor].FSqlGeometryFieldId = 0 then
         FCursorDB[_cursor].FSqlGeometryFieldId := sqlQueryGetFieldIndex( _field, _cursor ) ;

      Result := sqlQueryGetFieldById( FCursorDB[_cursor].FSqlGeometryFieldId, _cursor ) ;
    {$ELSE}
      Result := nil ;
    {$ENDIF}
  end ;

  procedure TGIS_DbAdoNet.sqlTableSetGeometry(
    const _id    : Integer ;
    const _name  : String ;
    const _data  : OleVariant ;
    const _blob  : TGIS_MemoryStream
   ) ;
  {$IFDEF CLR}
    var
      pname : String ;
  {$ENDIF}
  begin
    {$IFDEF CLR}
      if IsStringEmpty( _name ) or ( FSqlCommands[_id] = nil ) then exit ;

      if FSqlCommands[_id].Parameters.IndexOf(ParameterPrefix +_name)>=0 then
        pname := ParameterPrefix +_name
      else
        pname := _name ;
      try
        {$IFDEF OXYGENE}
          FSqlCommands[_id].Parameters[ pname ].Size  := Array( _data ).Length ;
        {$ELSE}
          FSqlCommands[_id].Parameters[ pname ].Size  := length( TBytes(_data) ) ;
        {$ENDIF}
        FSqlCommands[_id].Parameters[ pname ].Value := _data ;
      except
        on e : Exception do begin
          FErrorMessage := e.Message ;
          FSqlCommands[_id].Parameters[ ParameterPrefix +_name ].Value := DBNull.Value ;
        end ;
      end ;
    {$ELSE}
      check( _adonet_sql_table_set_param_blob(
                FObjHdl,
                _id,
                PAnsiChar( AnsiString( ParameterPrefix +_name ) ),
                _data
              )
            )
    {$ENDIF}
  end ;

  procedure TGIS_DbAdoNet.sqlTableSetGeometry(
    const _id    : Integer ;
    const _name  : String ;
    const _data  : TObject
  ) ;
  begin
    {$IFDEF CLR}
      if IsStringEmpty( _name ) or ( FSqlCommands[_id] = nil ) then exit ;

      try
        FSqlCommands[_id].Parameters[ ParameterPrefix +_name ].Value := _data ;
      except
        on e : Exception do begin
          FErrorMessage := e.Message ;
          {$IFNDEF MONO}
            FSqlCommands[_id].Parameters[ ParameterPrefix +_name ].Value := DBNull.Value ;
          {$ENDIF}
        end ;
      end ;
    {$ELSE}
    {$ENDIF}
  end ;

  procedure TGIS_DbAdoNet.sqlTableSetBlob(
    const _id    : Integer ;
    const _name  : String ;
    const _data  : OleVariant ;
    const _blob  : TGIS_MemoryStream
   ) ;
  {$IFDEF CLR}
  var
    pname : String ;
  {$ENDIF}
  begin
    {$IFDEF CLR}
      if IsStringEmpty( _name ) or ( FSqlCommands[_id] = nil ) then exit ;

      if FSqlCommands[_id].Parameters.IndexOf(ParameterPrefix +_name)>=0 then
        pname := ParameterPrefix +_name
      else
        pname := _name ;
      try
        {$IFDEF OXYGENE}
          FSqlCommands[_id].Parameters[ pname ].Size  := Array( _data ).Length ;
        {$ELSE}
          FSqlCommands[_id].Parameters[ pname ].Size  := length( TBytes(_data) ) ;
        {$ENDIF}
        FSqlCommands[_id].Parameters[ pname ].Value := _data ;
      except
        on e : Exception do begin
          FErrorMessage := e.Message ;
          FSqlCommands[_id].Parameters[ ParameterPrefix +_name ].Value := DBNull.Value ;
        end ;
      end ;
    {$ELSE}
      check( _adonet_sql_table_set_param_blob(
                FObjHdl,
                _id,
                PAnsiChar( AnsiString( ParameterPrefix +_name ) ),
                _data
             )
           )
    {$ENDIF}
  end ;

  procedure TGIS_DbAdoNet.sqlUpdateStart(
    const _id     : Integer ;
    const _table  : String
  ) ;
  begin
    // for safe inheritance
  end ;

  function TGIS_DbAdoNet.sqlQueryGeometryIsText(
    const _cursor : Integer
  ) : Boolean ;
  begin
    Result := False ;
  end ;

  procedure TGIS_DbAdoNet.sqlBuild(
    const _path     : String ;
    const _extent   : TGIS_Extent;
    const _type     : TGIS_ShapeType ;
    const _storage  : String ;
    const _layerName: String
  ) ;
  var
    layername     : String   ;
    canonicalname : String   ;
    ttklspath     : String   ;
    provider      : String   ;
    cfg           : TGIS_Config ;
  begin
    if IsStringEmpty( _path ) then exit ;

    if IsEmbeddedSQLPath( _path ) then exit ;

    ttklspath := GetPathAbsolute( '', _path ) ;
    if SafeFileExists( ttklspath ) then exit ;

    layername := _layerName ;
    if IsStringEmpty( layername ) then
      layername   := GetFileName( GetPathNoExt( _path ) ) ;

    canonicalname := TGIS_Utils.GisCanonicalSQLName( layername ) ;
    {$IFDEF MSWINDOWS}
    provider      := CreateMSJET( GetFilePath( ttklspath ) + '\' +
                                  GIS_INI_LAYERSQL_DEFAULT_DATABASE
                                 ) ;
    {$ENDIF}
    cfg := TGIS_ConfigFactory.CreateConfig( nil, _path ) ;
    try
      if cfg.ConfigFormat = TGIS_ConfigFormat.Ini then
        cfg.Section := GIS_INI_LAYER_HEADER
      else
        cfg.Section := GIS_INI_LAYERSQL_CONNECTOR ;
      cfg.WriteString( GIS_INI_LAYERSQL_STORAGE,
                       _storage,
                       ''
                     ) ;
      cfg.WriteString( GIS_INI_LAYERSQL_LAYER,
                       canonicalname,
                       ''
                     ) ;
      cfg.WriteString( GIS_INI_LAYERSQL_DIALECT,
                       GIS_SQL_DIALECT_NAME_MSJET,
                       ''
                     ) ;
      cfg.WriteString( GIS_INI_LAYERSQL_CONNECTOR_ADONET,
                       provider,
                       ''
                     ) ;
      cfg.WriteString( GIS_INI_LAYERSQL_CONNECTOR_PROVIDER,
                       'System.Data.OleDb',
                       ''
                     ) ;

    finally
      cfg.Save ;
      FreeObject( cfg ) ;
    end ;

  end ;

  procedure TGIS_DbAdoNet.cursorOpen(
    const _cursor : Integer
  ) ;
  begin
    if isInitialized then
    {$IFDEF CLR}
      if _cursor >= length( FCursorDB )  then
        SetLength( FCursorDB, _cursor + 1 ) ;
      FCursorDB[ _cursor ].FCurInUse    := True ;
      FCursorDB[ _cursor ].FSqlCommand := nil ;
    {$ELSE}
      check( _adonet_cursor_open( FObjHdl, _cursor ) ) ;
    {$ENDIF}
  end ;

  procedure TGIS_DbAdoNet.cursorClose(
    const _cursor : Integer
  ) ;
  {$IFDEF CLR}
    var
      i : Integer ;
  {$ENDIF}
  begin
    {$IFDEF CLR}
      sqlQueryClose( _cursor ) ;

      FCursorDB[_cursor].FCurInUse := False ;
      FreeObject( FCursorDB[_cursor].FSqlCommand ) ;

      // truncate cursorState at the tail;
      for i := length( FCursorDB ) - 1 downto 0 do begin
        if not FCursorDB[i].FCurInUse then begin
          SetLength( FCursorDB, i ) ;
        end
        else
          break ;
      end ;
    {$ELSE}
      if isInitialized then
        check( _adonet_cursor_close( FObjHdl, _cursor ) ) ;
    {$ENDIF}
  end ;

  function TGIS_DbAdoNet.getLastCursor : Integer ;
  begin
    {$IFDEF CLR}
      Result := length( FCursorDB ) - 1 ;
    {$ELSE}
      if isInitialized then
        check( _adonet_get_last_cursor( FObjHdl, Result ) ) ;
    {$ENDIF}
  end ;

{==================================== END =====================================}
end.
