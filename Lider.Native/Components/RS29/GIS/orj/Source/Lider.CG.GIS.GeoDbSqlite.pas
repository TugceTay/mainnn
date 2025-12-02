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
  Encapsulation of a SQL file access for SQLite.
}

{$IFDEF DCC}
  unit GisDbSqlite ;
  {$HPPEMIT '#pragma link "GisDbSqlite"'}
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

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    System.Security,
    System.Runtime.InteropServices,
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    System.Classes,
    System.SysUtils,
    System.Variants,

    GisTypes,
    GisStreams,
    GisDb ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type
  {$IFNDEF CLR}
    {#gendoc:hide}
    tsqlite3 = Pointer ;
  {$ELSE}
    {#gendoc:hide}
    tsqlite3 = IntPtr ;
  {$ENDIF}

{$IFDEF OXYGENE}
  T_cursorDB_Sqlite nested in TGIS_DbSqlite = public record
    {$IFNDEF GENDOC}
    type
      T_fStmtCT_Sqlite = public record
        public
          dType : Integer ;
          dName : String  ;
          dSize : Integer ;
          dPrec : Integer ;
      end ;
    {$ENDIF}
    public
      /// <summary>
      ///   Is cursor in use.
      /// </summary>

      curInUse : Boolean ;
      /// <summary>
      ///   Query object.
      /// </summary>

      fStmt    : tsqlite3 ;
      /// <summary>
      ///   Query result code.
      /// </summary>

      fStmtRC  : Integer ;
      /// <summary>
      ///   Query data types.
      /// </summary>

      fStmtCT  : array of T_fStmtCT_Sqlite ;
      /// <summary>
      ///   Query column names.
      /// </summary>

      fStmtCN  : TStringList ;
      /// <summary>
      ///   Stored previous select query.
      /// </summary>
      oldQuery : String ;

      /// <summary>
      ///   Binded fields.
      /// </summary>
      fColUid     : Integer ;

      /// <summary>
      ///   Binded fields.
      /// </summary>
      fColGeom    : Integer ;

      /// <summary>
      ///   Binded fields.
      /// </summary>
      fColShpType : Integer ;

      /// <summary>
      ///   Binded fields.
      /// </summary>
      fColXmin    : Integer ;

      /// <summary>
      ///   Binded fields.
      /// </summary>
      fColYmin    : Integer ;
  end ;
{$ENDIF}

  /// <summary>
  ///   Class that can read Sqlite.
  /// </summary>
  TGIS_DbSqlite = {$IFDEF OXYGENE} public {$ENDIF}  class( TGIS_DbAbstract )
    private

      /// <summary>
      ///   Database handle.
      /// </summary>
      FDB               : tsqlite3 ;

      /// <summary>
      ///   Sqlite connection.
      /// </summary>
      FSqliteConnection : TObject ;

     {$IFDEF OXYGENE}
       cursorDB : array of T_cursorDB_Sqlite ;
     {$ELSE}

       /// <summary>
       ///   Cursor.
       /// </summary>
       cursorDB : array of record
         curInUse : Boolean  ; // Is cursor in use.
         fStmt    : tsqlite3 ; // Query object
         fStmtRC  : Integer  ; // Query result code
         fStmtCT  : array of record // Query data types
           dType : Integer ;
           dName : String  ;
           dSize : Integer ;
           dPrec : Integer ;
         end ;
         fStmtCN  : TStringList ; // Query column names.
         oldQuery : String   ; // Stored previous select query.

         // Binded fields.
         fColUid     : Integer ;
         fColGeom    : Integer ;
         fColShpType : Integer ;
         fColXmin    : Integer ;
         fColYmin    : Integer ;
       end ;
     {$ENDIF}

      /// <summary>
      ///   Statement array.
      /// </summary>
      FStmts          : array of tsqlite3 ;

      FDLLHandle      : THandle ;
      FIsInitialized  : Boolean ;
      FFunctions      : TGIS_ObjectList ;
      FInMemory       : Boolean ;
    private

      /// <summary>
      ///   Initialize library.
      /// </summary>
      procedure initializeDLL ;

      /// <summary>
      ///   Finalize library.
      /// </summary>
      procedure finalizeDLL ;

      /// <summary>
      ///   Check status code.
      /// </summary>
      /// <param name="_code">
      ///   status code
      /// </param>
      /// <param name="_msg">
      ///   info message
      /// </param>
      procedure check        ( const _code : Integer ;
                               const _msg  : String
                             ) ;

      /// <summary>
      ///   Get column info.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <param name="_index">
      ///   field index
      /// </param>
      /// <param name="_name">
      ///   field name
      /// </param>
      /// <param name="_dType">
      ///   field data type
      /// </param>
      /// <param name="_dName">
      ///   field data name
      /// </param>
      /// <param name="_dSize">
      ///   field data size
      /// </param>
      /// <param name="_dPrec">
      ///   field data precision
      /// </param>
      procedure getColumnInfo( const _cursor : Integer ;
                               const _index  : Integer ;
                                 var _name   : String ;
                                 var _dType  : Integer ;
                                 var _dName  : String  ;
                                 var _dSize  : Integer ;
                                 var _dPrec  : Integer
                               ) ;

      /// <summary>
      ///   Collect a query structure.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      procedure getStructure ( const _cursor : Integer
                             ) ;

      /// <summary>
      ///   Allocate table of statements.
      /// </summary>
      /// <param name="_id">
      ///   statement id
      /// </param>
      procedure allocStmts   ( const _id : Integer
                              ) ;
    public
      /// <inheritdoc/>
      procedure sqlConnect            ( const _folder        : String   ;
                                        const _sqlParameters : {$IFDEF OXYGENE}
                                                                 TGIS_Strings
                                                               {$ELSE}
                                                                 TStrings
                                                               {$ENDIF}
                                      ) ; override;

      /// <inheritdoc/>
      procedure sqlDisconnect                 ; override;

      /// <inheritdoc/>
      procedure sqlTransactUpdateStart        ; override;

      /// <inheritdoc/>
      procedure sqlTransactUpdateCommit       ; override;

      /// <inheritdoc/>
      procedure sqlTransactGlobalUpdateStart  ; override;

      /// <inheritdoc/>
      procedure sqlTransactGlobalUpdateCommit ; override;

      /// <inheritdoc/>
      procedure sqlTransactRestructStart      ; override;

      /// <inheritdoc/>
      procedure sqlTransactRestructCommit     ; override;

      /// <inheritdoc/>
      procedure sqlTransactRollback           ; override;

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
                                      ): Integer ; override;

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
      {$IFDEF MANAGED}
        /// <inheritdoc/>
        function  sqlQueryGetGeomPtr  ( const _field         : String ;
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
        function  sqlQueryGetGeomPtr  ( const _field         : String ;
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
      function  sqlGetParams          ( const _idx           : Integer
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
                                      ) ; override;

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

      /// <inheritdoc/>
      procedure sqlCreateFunction     ( const _name  : String ;
                                        const _nArgs : Integer ;
                                        const _fnc   : TGIS_DbFunction
                                       ) ; override;
      /// <inheritdoc/>
      function sqlLastInsertId        : TGIS_Uid ; override;
    protected

      /// <inheritdoc/>
      procedure doDestroy ; override;

    public

      /// <inheritdoc/>
      constructor Create  ; override;

      /// <inheritdoc/>
      function  InitializeProvider  : Boolean ; override;

      /// <inheritdoc/>
      procedure FinalizeProvider    ; override;

      /// <inheritdoc/>
      function  GetLastErrorMessage : String ; override;

    end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisRtl,
    GisInterfaces,
    GisInternals,
    GisClasses,
    GisResource,
    GisConfig,
    GisUtils,
    GisSharedConnections ;
{$ENDIF}

//-----------------------------------------------------------------------------
// T_Wrapper
//-----------------------------------------------------------------------------

const
{$IFDEF DCC}
  {$IFDEF MSWINDOWS_OS}
    // use custom DLL
    {$DEFINE DEF_CDECL}
    {$DEFINE DEF_DYNAMICLINK}
    SQLITE_LIB_NAME      = 'sqlite3.dll';
    SQLITE_LIB_NAME_ALT1 = '' ;
    SQLITE_LIB_NAME_ALT2 = '' ;
  {$ENDIF}
{$ENDIF}

{$IFDEF CLR}
  // use custom DLL
  SQLITE_LIB_NAME      = 'sqlite3.dll';
  SQLITE_LIB_NAME_ALT1 = '' ;
  SQLITE_LIB_NAME_ALT2 = '' ;
{$ENDIF}

{$IFDEF IOS_DEVICE}
  // use built in
  SQLITE_LIB_NAME      = 'libttksqlite3.a';
  SQLITE_LIB_NAME_ALT1 = '' ;
  SQLITE_LIB_NAME_ALT2 = '' ;
  {$DEFINE DEF_STATICLINK}
{$ENDIF}

{$IFDEF IOS_SIMULATOR}
  // use built in
  {$DEFINE DEF_DYNAMICLINK}
  SQLITE_LIB_NAME      = 'libsqlite3.dylib';
  SQLITE_LIB_NAME_ALT1 = '' ;
  SQLITE_LIB_NAME_ALT2 = '' ;
{$ENDIF}

{$IFDEF ANDROID_OS}
  // use custom DLL
  {$DEFINE DEF_DYNAMICLINK}
  SQLITE_LIB_NAME      = 'libttksqlite3.so';
  SQLITE_LIB_NAME_ALT1 = '' ;
  SQLITE_LIB_NAME_ALT2 = '' ;
{$ENDIF}

{$IFDEF MACOSX_OS}
  // use built in
  {$DEFINE DEF_CDECL}
  {$DEFINE DEF_DYNAMICLINK}
  SQLITE_LIB_NAME      = 'libttksqlite3.dylib';
  SQLITE_LIB_NAME_ALT1 = 'libsqlite3.dylib' ;
  SQLITE_LIB_NAME_ALT2 = '' ;
{$ENDIF}

{$IFDEF LINUX_OS}
  // use built in
  {$DEFINE DEF_CDECL}
  {$DEFINE DEF_DYNAMICLINK}
  SQLITE_LIB_NAME      = 'libttksqlite3.so' ;
  SQLITE_LIB_NAME_ALT1 = 'libsqlite3.so.0' ;
  SQLITE_LIB_NAME_ALT2 = 'libsqlite3.so' ;
{$ENDIF}

{$IFDEF UNKNOWN_OS}
  {$IFNDEF CLR}
    {$DEFINE DEF_CDECL}
    {$DEFINE DEF_DYNAMICLINK}
    SQLITE_LIB_NAME = 'sqlite3.dll';
    SQLITE_LIB_NAME_ALT1 = '' ;
    SQLITE_LIB_NAME_ALT2 = '' ;
  {$ENDIF}
{$ENDIF}


  // Result Codes
  SQLITE_OK           = 0;   // Successful result
  // beginning-of-error-codes
  SQLITE_ERROR        = 1;   // SQL error or missing database
  SQLITE_INTERNAL     = 2;   // Internal logic error in SQLite
  SQLITE_PERM         = 3;   // Access permission denied
  SQLITE_ABORT        = 4;   // Callback routine requested an abort
  SQLITE_BUSY         = 5;   // The database file is locked
  SQLITE_LOCKED       = 6;   // A table in the database is locked
  SQLITE_NOMEM        = 7;   // A malloc() failed
  SQLITE_READONLY     = 8;   // Attempt to write a readonly database
  SQLITE_INTERRUPT    = 9;   // Operation terminated by sqlite3_interrupt()
  SQLITE_IOERR       = 10;   // Some kind of disk I/O error occurred
  SQLITE_CORRUPT     = 11;   // The database disk image is malformed
  SQLITE_NOTFOUND    = 12;   // NOT USED. Table or record not found
  SQLITE_FULL        = 13;   // Insertion failed because database is full
  SQLITE_CANTOPEN    = 14;   // Unable to open the database file
  SQLITE_PROTOCOL    = 15;   // NOT USED. Database lock protocol error
  SQLITE_EMPTY       = 16;   // Database is empty
  SQLITE_SCHEMA      = 17;   // The database schema changed
  SQLITE_TOOBIG      = 18;   // String or BLOB exceeds size limit
  SQLITE_CONSTRAINT  = 19;   // Abort due to constraint violation
  SQLITE_MISMATCH    = 20;   // Data type mismatch
  SQLITE_MISUSE      = 21;   // Library used incorrectly
  SQLITE_NOLFS       = 22;   // Uses OS features not supported on host
  SQLITE_AUTH        = 23;   // Authorization denied
  SQLITE_FORMAT      = 24;   // Auxiliary database format error
  SQLITE_RANGE       = 25;   // 2nd parameter to sqlite3_bind out of range
  SQLITE_NOTADB      = 26;   // File opened that is not a database file
  SQLITE_ROW         = 100;  // sqlite3_step() has another row ready
  SQLITE_DONE        = 101;  // sqlite3_step() has finished executing

  // Fundamental Data types
  SQLITE_DATA_INTEGER  = 1;
  SQLITE_DATA_FLOAT    = 2;
  SQLITE_DATA_TEXT     = 3;
  SQLITE_DATA_BLOB     = 4;
  SQLITE_DATA_NULL     = 5;
  SQLITE_DATA_DATE     = 6;
  SQLITE_DATA_BOOL     = 7;

  SQLITE_OPEN_READONLY     = $00000001 ;
  SQLITE_OPEN_READWRITE    = $00000002 ;
  SQLITE_OPEN_CREATE       = $00000004 ;
  SQLITE_OPEN_NOMUTEX      = $00008000 ;
  SQLITE_OPEN_FULLMUTEX    = $00010000 ;
  SQLITE_OPEN_SHAREDCACHE  = $00020000 ;
  SQLITE_OPEN_PRIVATECACHE = $00040000 ;

type
  {$IFDEF CLR}
    Tsqlite3_destroy = tsqlite3 ;
  {$ELSE}
   Tsqlite3_destroy = procedure( _value : tsqlite3 ) ; cdecl ;
  {$ENDIF}

  {$IFDEF CLR}
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
  {$ENDIF}
  Tsqlite3_simple_callback = function  (       _add   : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  {$IFDEF CLR}
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
  {$ENDIF}
  Tsqlite3_func_callback   = procedure (       _ctx   : tsqlite3 ;
                                               _nargs : Integer  ;
                                               _args  : tsqlite3
                                       ) ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  {$IFDEF CLR}
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
  {$ENDIF}
  Tsqlite3_step_callback   = procedure (       _ctx   : tsqlite3 ;
                                               _nargs : Integer  ;
                                               _args  : tsqlite3
                                       ) ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  {$IFDEF CLR}
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
  {$ENDIF}
  Tsqlite3_final_callback  = procedure (       _ctx   : tsqlite3
                                       ) ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  {$IFDEF CLR}
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
  {$ENDIF}
  Tsqlite3_user_data       = function  (       _ctx   : tsqlite3
                                       ) : tsqlite3 ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
var
  SQLITE_TRANSIENT    : Tsqlite3_destroy ;
  Gsqlite3_user_data  : Tsqlite3_user_data = nil ;

{$IFDEF CLR}
  var
    sqlite3_open_v2Proc              : tsqlite3 ;
    sqlite3_closeProc                : tsqlite3 ;
    sqlite3_errmsgProc               : tsqlite3 ;
    sqlite3_prepareProc              : tsqlite3 ;
    sqlite3_stepProc                 : tsqlite3 ;
    sqlite3_finalizeProc             : tsqlite3 ;
    sqlite3_resetProc                : tsqlite3 ;
    sqlite3_column_countProc         : tsqlite3 ;
    sqlite3_column_nameProc          : tsqlite3 ;
    sqlite3_column_decltypeProc      : tsqlite3 ;
    sqlite3_column_typeProc          : tsqlite3 ;
    sqlite3_column_blobProc          : tsqlite3 ;
    sqlite3_column_bytesProc         : tsqlite3 ;
    sqlite3_column_doubleProc        : tsqlite3 ;
    sqlite3_column_textProc          : tsqlite3 ;
    sqlite3_column_intProc           : tsqlite3 ;
    sqlite3_column_int64Proc         : tsqlite3 ;
    sqlite3_execProc                 : tsqlite3 ;
    sqlite3_clear_bindingsProc       : tsqlite3 ;
    sqlite3_bind_parameter_countProc : tsqlite3 ;
    sqlite3_bind_parameter_indexProc : tsqlite3 ;
    sqlite3_bind_parameter_nameProc  : tsqlite3 ;
    sqlite3_bind_blobProc            : tsqlite3 ;
    sqlite3_bind_doubleProc          : tsqlite3 ;
    sqlite3_bind_intProc             : tsqlite3 ;
    sqlite3_bind_int64Proc           : tsqlite3 ;
    sqlite3_bind_nullProc            : tsqlite3 ;
    sqlite3_bind_textProc            : tsqlite3 ;
    sqlite3_bind_valueProc           : tsqlite3 ;
    sqlite3_progress_handlerProc     : tsqlite3 ;
    sqlite3_create_functionProc      : tsqlite3 ;
    sqlite3_user_dataProc            : tsqlite3 ;
    sqlite3_result_blobProc          : tsqlite3 ;
    sqlite3_result_doubleProc        : tsqlite3 ;
    sqlite3_result_intProc           : tsqlite3 ;
    sqlite3_result_int64Proc         : tsqlite3 ;
    sqlite3_result_nullProc          : tsqlite3 ;
    sqlite3_result_textProc          : tsqlite3 ;
    sqlite3_result_valueProc         : tsqlite3 ;
    sqlite3_result_zeroblobProc      : tsqlite3 ;
    sqlite3_value_blobProc           : tsqlite3 ;
    sqlite3_value_bytesProc          : tsqlite3 ;
    sqlite3_value_doubleProc         : tsqlite3 ;
    sqlite3_value_intProc            : tsqlite3 ;
    sqlite3_value_int64Proc          : tsqlite3 ;
    sqlite3_value_textProc           : tsqlite3 ;
    sqlite3_value_typeProc           : tsqlite3 ;
    sqlite3_value_numeric_typeProc   : tsqlite3 ;
    sqlite3_last_insert_rowidProc    : tsqlite3 ;
{$ENDIF}

{$IFDEF CLR}
  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_open_v2'
             )
  ]
  function sqlite3_open_v2             (       _filename  : TBytes   ;
                                         var   _pDb       : tsqlite3 ;
                                               _flags     : Integer  ;
                                               _vfs       : tsqlite3
                                       ) : Integer ;
                                       external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_close'
             )
  ]
  function sqlite3_close                (      _db        : tsqlite3
                                        ) : Integer ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_errmsg'
             )
  ]
  function sqlite3_errmsg               (      _db        : tsqlite3
                                        ) : tsqlite3 ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_prepare_v2'
             )
  ]
  function sqlite3_prepare              (      _db        : tsqlite3 ;
                                               _sql       : TBytes   ;
                                               _size      : Integer  ;
                                          var  _hStmt     : tsqlite3 ;
                                          var  _pzTail    : tsqlite3
                                        ) : Integer ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_step'
             )
  ]
  function sqlite3_step                 (      _hStmt     : tsqlite3
                                        ) : Integer ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_finalize'
             )
  ]
  function sqlite3_finalize             (      _hStmt     : tsqlite3
                                        ) : Integer ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_reset'
             )
  ]
  function sqlite3_reset                (      _hStmt     : tsqlite3
                                        ) : Integer ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_column_count'
             )
  ]
  function sqlite3_column_count         (      _hStmt     : tsqlite3
                                        ) : Integer ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_column_name'
             )
  ]
  function sqlite3_column_name          (      _hStmt     : tsqlite3 ;
                                               _colNum    : Integer
                                        ) : tsqlite3 ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_column_decltype'
             )
  ]
  function sqlite3_column_decltype      (      _hStmt     : tsqlite3;
                                               _colNum    : Integer
                                        ) : tsqlite3 ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_column_type'
             )
  ]
  function sqlite3_column_type          (      _hStmt     : tsqlite3;
                                               _colNum    : Integer
                                        ) : Integer ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_column_blob'
             )
  ]
  function sqlite3_column_blob          (      _hStmt     : tsqlite3 ;
                                               _colNum    : Integer
                                        ) : tsqlite3 ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_column_bytes'
             )
  ]
  function sqlite3_column_bytes         (      _hStmt     : tsqlite3 ;
                                               _colNum    : Integer
                                        ) : Integer ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_column_double'
             )
  ]
  function sqlite3_column_double        (      _hStmt     : tsqlite3 ;
                                               _colNum    : Integer
                                        ) : Double ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_column_text'
             )
  ]
  function sqlite3_column_text          (      _hStmt     : tsqlite3 ;
                                               _colNum    : Integer
                                        ) : tsqlite3 ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_column_int'
             )
  ]
  function sqlite3_column_int           (      _hStmt     : tsqlite3 ;
                                               _colNum    : Integer
                                        ) : Integer ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_column_int64'
             )
  ]
  function sqlite3_column_int64         (      _hStmt     : tsqlite3 ;
                                               _colNum    : Integer
                                        ) : Int64 ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_exec'
             )
  ]
  function sqlite3_exec                 (      _db        : tsqlite3 ;
                                               _sql       : TBytes   ;
                                               _callback  : tsqlite3 ;
                                               _userData  : tsqlite3 ;
                                          var  _erMsg     : tsqlite3
                                        ) : Integer ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_clear_bindings'
             )
  ]
  function sqlite3_clear_bindings       (      _hStmt     : tsqlite3
                                        ) : Integer ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_bind_parameter_count'
             )
  ]
  function sqlite3_bind_parameter_count (      _hStmt     : tsqlite3
                                        ) : Integer ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_bind_parameter_index'
             )
  ]
  function sqlite3_bind_parameter_index (      _hStmt     : tsqlite3 ;
                                               _zName     : TBytes
                                        ) : Integer ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_bind_parameter_name'
             )
  ]
  function sqlite3_bind_parameter_name  (      _hStmt     : tsqlite3 ;
                                               _index     : Integer
                                        ) : tsqlite3 ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_bind_blob'
             )
  ]
  function sqlite3_bind_blob            (      _hStmt     : tsqlite3 ;
                                               _index     : Integer  ;
                                               _value     : TBytes   ;
                                               _nBytes    : Integer  ;
                                               _destr     : Tsqlite3_destroy
                                        ) : Integer ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_bind_double'
             )
  ]
  function sqlite3_bind_double          (      _hStmt     : tsqlite3 ;
                                               _index     : Integer  ;
                                               _value     : Double
                                        ) : Integer ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_bind_int'
             )
  ]
  function sqlite3_bind_int             (      _hStmt     : tsqlite3 ;
                                               _index     : Integer  ;
                                               _value     : Integer
                                        ) : Integer ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_bind_int64'
             )
  ]
  function sqlite3_bind_int64           (      _hStmt     : tsqlite3 ;
                                               _index     : Integer  ;
                                               _value     : Int64
                                        ) : Integer ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_bind_null'
             )
  ]
  function sqlite3_bind_null            (      _hStmt     : tsqlite3 ;
                                               _index     : Integer
                                        ) : Integer ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_bind_text'
             )
  ]
  function sqlite3_bind_text            (      _hStmt     : tsqlite3 ;
                                               _index     : Integer  ;
                                               _value     : TBytes   ;
                                               _nBytes    : Integer  ;
                                               _destr     : Tsqlite3_destroy
                                        ) : Integer ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_bind_value'
             )
  ]
  function sqlite3_bind_value           (      _hStmt     : tsqlite3 ;
                                               _index     : Integer  ;
                                               _value     : tsqlite3
                                        ) : Integer ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_progress_handler'
             )
  ]
  procedure sqlite3_progress_handler    (      _db       : tsqlite3 ;
                                               _frq      : Integer  ;
                                               _callback : Tsqlite3_simple_callback ;
                                               _app      : tsqlite3
                                        ) ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_create_function'
             )
  ]
  function sqlite3_create_function      (      _db       : tsqlite3 ;
                                               _fName    : TBytes   ;
                                               _nArg     : Integer  ;
                                               _eTextRep : Integer  ;
                                               _pApp     : tsqlite3 ;
                                               _xFunc    : Tsqlite3_func_callback ;
                                               _xStep    : Tsqlite3_step_callback ;
                                               _xFinal   : Tsqlite3_final_callback
                                        ) : Integer ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_user_data'
             )
  ]
  function sqlite3_user_data            (      _db        : tsqlite3
                                        ) : tsqlite3 ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_result_blob'
             )
  ]
  procedure sqlite3_result_blob         (      _db       : tsqlite3 ;
                                               _value    : tsqlite3 ;
                                               _nBytes   : Integer  ;
                                               _destr    : Tsqlite3_destroy
                                        ) ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_result_double'
             )
  ]
  procedure sqlite3_result_double       (      _db       : tsqlite3 ;
                                               _value    : Double
                                        ) ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_result_int'
             )
  ]
  procedure sqlite3_result_int          (      _db       : tsqlite3 ;
                                               _value    : Integer
                                        ) ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_result_int64'
             )
  ]
  procedure sqlite3_result_int64        (      _db       : tsqlite3 ;
                                               _value    : Int64
                                        ) ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_result_null'
             )
  ]
  procedure  sqlite3_result_null        (      _db       : tsqlite3
                                        ) ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_result_text'
             )
  ]
  procedure sqlite3_result_text         (      _db       : tsqlite3 ;
                                               _value    : TBytes   ;
                                               _nBytes   : Integer  ;
                                               _destr    : Tsqlite3_destroy
                                        ) ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_result_value'
             )
  ]
  procedure sqlite3_result_value        (      _db       : tsqlite3 ;
                                               _value    : tsqlite3
                                        ) ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_result_zeroblob'
             )
  ]
  procedure sqlite3_result_zeroblob     (      _db       : tsqlite3 ;
                                               _n        : Integer
                                        ) ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_value_blob'
             )
  ]
  function sqlite3_value_blob           (      _value    : tsqlite3
                                        ) : tsqlite3 ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_value_bytes'
             )
  ]
  function sqlite3_value_bytes          (      _value    : tsqlite3
                                        ) : Integer ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_value_double'
             )
  ]
  function sqlite3_value_double         (      _value    : tsqlite3
                                        ) : Double ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_value_int'
             )
  ]
  function sqlite3_value_int            (      _value    : tsqlite3
                                        ) : Integer ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_value_int64'
             )
  ]
  function sqlite3_value_int64          (      _value    : tsqlite3
                                        ) : Int64 ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_value_text'
             )
  ]
  function sqlite3_value_text           (      _value    : tsqlite3
                                        ) : tsqlite3 ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_value_type'
             )
  ]
  function sqlite3_value_type           (      _value    : tsqlite3
                                        ) : Integer; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_value_numeric_type'
             )
  ]
  function sqlite3_value_numeric_type   (      _value    : tsqlite3
                                        ) : Integer ;
                                        external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( SQLITE_LIB_NAME, CallingConvention = CallingConvention.Cdecl,
               CharSet = CharSet.Ansi, EntryPoint = 'sqlite3_last_insert_rowid'
             )
  ]
  function sqlite3_last_insert_rowid    (      _db       : tsqlite3
                                        ) : Int64 ;
                                        external ;
{$ENDIF}

{$IFDEF DEF_DYNAMICLINK}
  var

  sqlite3_open_v2 : function           (       _filename  : tsqlite3 ;
                                         out   _pDb       : tsqlite3 ;
                                               _flags     : Integer  ;
                                               _vfs       : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_close : function             (       _db        : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_errmsg : function            (       _db        : tsqlite3
                                       ) : tsqlite3 ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_prepare : function           (       _db        : tsqlite3 ;
                                               _sql       : tsqlite3 ;
                                               _size      : Integer  ;
                                         var   _hStmt     : tsqlite3 ;
                                         var   _pzTail    : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_step : function              (       _hStmt     : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_finalize : function          (       _hStmt     : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_reset : function             (       _hStmt     : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_column_count : function      (       _hStmt     : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_column_name : function       (       _hStmt     : tsqlite3 ;
                                               _colNum    : Integer
                                       ) : tsqlite3 ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_column_decltype : function   (       _hStmt     : tsqlite3;
                                               _colNum    : Integer
                                       ) : tsqlite3  ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_column_type : function       (       _hStmt     : tsqlite3;
                                               _colNum    : Integer
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_column_blob : function       (       _hStmt     : tsqlite3 ;
                                               _colNum    : Integer
                                       ) : tsqlite3 ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_column_bytes : function      (       _hStmt     : tsqlite3 ;
                                               _colNum    : Integer
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_column_double : function     (       _hStmt     : tsqlite3 ;
                                               _colNum    : Integer
                                       ) : Double ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_column_text : function       (       _hStmt     : tsqlite3 ;
                                               _colNum    : Integer
                                       ) : tsqlite3 ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_column_int : function        (       _hStmt     : tsqlite3 ;
                                               _colNum    : Integer
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_column_int64 : function      (       _hStmt     : tsqlite3 ;
                                               _colNum    : Integer
                                       ) : Int64 ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_exec : function              (       _db        : tsqlite3 ;
                                               _sql       : tsqlite3 ;
                                               _callback  : tsqlite3 ;
                                               _userData  : tsqlite3 ;
                                         var   _erMsg     : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_clear_bindings : function    (       _hStmt     : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_bind_parameter_count : function
                                       (       _hStmt     : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_bind_parameter_index : function
                                       (       _hStmt     : tsqlite3 ;
                                               _zName     : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_bind_parameter_name : function
                                       (       _hStmt     : tsqlite3 ;
                                               _index     : Integer
                                       ) : tsqlite3 ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_bind_blob : function         (       _hStmt     : tsqlite3 ;
                                               _index     : Integer  ;
                                               _value     : tsqlite3 ;
                                               _nBytes    : Integer  ;
                                               _destr     : Tsqlite3_destroy
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_bind_double : function       (       _hStmt     : tsqlite3 ;
                                               _index     : Integer  ;
                                               _value     : Double
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_bind_int : function          (       _hStmt     : tsqlite3 ;
                                               _index     : Integer  ;
                                               _value     : Integer
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_bind_int64 : function        (       _hStmt     : tsqlite3 ;
                                               _index     : Integer  ;
                                               _value     : Int64
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_bind_null : function         (       _hStmt     : tsqlite3 ;
                                               _index     : Integer
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_bind_text : function         (       _hStmt     : tsqlite3 ;
                                               _index     : Integer  ;
                                               _value     : tsqlite3 ;
                                               _nBytes    : Integer  ;
                                               _destr     : Tsqlite3_destroy
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_bind_value : function        (       _hStmt     : tsqlite3 ;
                                               _index     : Integer  ;
                                               _value     : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_progress_handler : procedure (       _db       : tsqlite3 ;
                                               _frq      : Integer  ;
                                               _callback : Tsqlite3_simple_callback ;
                                               _app      : tsqlite3
                                       ) ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_create_function : function   (       _db       : tsqlite3 ;
                                               _fName    : tsqlite3 ;
                                               _nArg     : Integer  ;
                                               _eTextRep : Integer  ;
                                               _pApp     : tsqlite3 ;
                                               _xFunc    : Tsqlite3_func_callback ;
                                               _xStep    : Tsqlite3_step_callback ;
                                               _xFinal   : Tsqlite3_final_callback
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_user_data : function         (       _db       : tsqlite3
                                       ) : tsqlite3 ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

   sqlite3_result_blob : procedure     (       _db       : tsqlite3 ;
                                               _value    : tsqlite3 ;
                                               _nBytes   : Integer  ;
                                               _destr    : Tsqlite3_destroy
                                       ) ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

   sqlite3_result_double : procedure   (       _db       : tsqlite3 ;
                                              _value    : Double
                                       ) ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

   sqlite3_result_int : procedure      (       _db       : tsqlite3 ;
                                               _value    : Integer
                                       ) ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

   sqlite3_result_int64 : procedure    (       _db       : tsqlite3 ;
                                               _value    : Int64
                                       ) ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_result_null : procedure      (       _db       : tsqlite3
                                       ) ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

   sqlite3_result_text : procedure     (       _db       : tsqlite3 ;
                                               _value    : tsqlite3 ;
                                               _nBytes   : Integer  ;
                                               _destr    : Tsqlite3_destroy
                                       ) ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

   sqlite3_result_value : procedure    (       _db       : tsqlite3 ;
                                               _value    : tsqlite3
                                       ) ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

   sqlite3_result_zeroblob : procedure (       _db       : tsqlite3 ;
                                               _n        : Integer
                                       ) ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_value_blob : function        (       _value    : tsqlite3
                                       ) : tsqlite3 ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_value_bytes : function       (       _value    : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_value_double : function      (       _value    : tsqlite3
                                       ) : Double ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_value_int : function         (       _value    : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_value_int64 : function       (       _value    : tsqlite3
                                       ) : Int64 ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_value_text : function        (       _value    : tsqlite3
                                       ) : tsqlite3 ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_value_type : function        (       _value    : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_value_numeric_type : function(       _value    : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}

  sqlite3_last_insert_rowid : function (       _value    : tsqlite3
                                       ) : Int64 ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
{$ENDIF}

{$IFDEF DEF_STATICLINK}
  function sqlite3_open_v2             (       _filename  : tsqlite3 ;
                                         out   _pDb       : tsqlite3 ;
                                               _flags     : Integer  ;
                                               _vfs       : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_close               (       _db        : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_errmsg              (       _db        : tsqlite3
                                       ) : tsqlite3 ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_prepare             (       _db        : tsqlite3 ;
                                               _sql       : tsqlite3 ;
                                               _size      : Integer  ;
                                         var   _hStmt     : tsqlite3 ;
                                         var   _pzTail    : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_step                (       _hStmt     : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_finalize            (       _hStmt     : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_reset               (       _hStmt     : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_column_count        (       _hStmt     : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_column_name         (       _hStmt     : tsqlite3 ;
                                               _colNum    : Integer
                                       ) : tsqlite3 ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_column_decltype     (       _hStmt     : tsqlite3;
                                               _colNum    : Integer
                                       ) : tsqlite3  ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_column_type         (       _hStmt     : tsqlite3;
                                               _colNum    : Integer
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_column_blob         (       _hStmt     : tsqlite3 ;
                                               _colNum    : Integer
                                       ) : tsqlite3 ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_column_bytes        (       _hStmt     : tsqlite3 ;
                                               _colNum    : Integer
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_column_double       (       _hStmt     : tsqlite3 ;
                                               _colNum    : Integer
                                       ) : Double ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_column_text         (       _hStmt     : tsqlite3 ;
                                               _colNum    : Integer
                                       ) : tsqlite3 ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_column_int          (       _hStmt     : tsqlite3 ;
                                               _colNum    : Integer
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_column_int64        (       _hStmt     : tsqlite3 ;
                                               _colNum    : Integer
                                       ) : Int64 ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_exec                (       _db        : tsqlite3 ;
                                               _sql       : tsqlite3 ;
                                               _callback  : tsqlite3 ;
                                               _userData  : tsqlite3 ;
                                         var   _erMsg     : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_clear_bindings      (       _hStmt     : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_bind_parameter_count(       _hStmt     : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_bind_parameter_index(       _hStmt     : tsqlite3 ;
                                               _zName     : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_bind_parameter_name (       _hStmt     : tsqlite3 ;
                                               _index     : Integer
                                       ) : tsqlite3 ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_bind_blob           (       _hStmt     : tsqlite3 ;
                                               _index     : Integer  ;
                                               _value     : tsqlite3 ;
                                               _nBytes    : Integer  ;
                                               _destr     : Tsqlite3_destroy
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_bind_double         (       _hStmt     : tsqlite3 ;
                                               _index     : Integer  ;
                                               _value     : Double
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_bind_int            (       _hStmt     : tsqlite3 ;
                                               _index     : Integer  ;
                                               _value     : Integer
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_bind_int64          (       _hStmt     : tsqlite3 ;
                                               _index     : Integer  ;
                                               _value     : Int64
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_bind_null           (       _hStmt     : tsqlite3 ;
                                               _index     : Integer
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_bind_text           (       _hStmt     : tsqlite3 ;
                                               _index     : Integer  ;
                                               _value     : tsqlite3 ;
                                               _nBytes    : Integer  ;
                                               _destr     : Tsqlite3_destroy
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_bind_value          (       _hStmt     : tsqlite3 ;
                                               _index     : Integer  ;
                                               _value     : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  procedure sqlite3_progress_handler   (       _db       : tsqlite3 ;
                                               _frq      : Integer  ;
                                               _callback : Tsqlite3_simple_callback ;
                                               _app      : tsqlite3
                                       ) ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_create_function     (       _db       : tsqlite3 ;
                                               _fName    : tsqlite3 ;
                                               _nArg     : Integer  ;
                                               _eTextRep : Integer  ;
                                               _pApp     : tsqlite3 ;
                                               _xFunc    : Tsqlite3_func_callback ;
                                               _xStep    : Tsqlite3_step_callback ;
                                               _xFinal   : Tsqlite3_final_callback
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_user_data           (       _db       : tsqlite3
                                       ) : tsqlite3 ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  procedure sqlite3_result_blob        (       _db       : tsqlite3 ;
                                               _value    : tsqlite3 ;
                                               _nBytes   : Integer  ;
                                               _destr    : Tsqlite3_destroy
                                       ) ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  procedure sqlite3_result_double      (       _db       : tsqlite3 ;
                                              _value    : Double
                                       ) ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  procedure sqlite3_result_int         (       _db       : tsqlite3 ;
                                               _value    : Integer
                                       ) ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  procedure sqlite3_result_int64       (       _db       : tsqlite3 ;
                                               _value    : Int64
                                       ) ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  procedure sqlite3_result_null        (       _db       : tsqlite3
                                       ) ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  procedure sqlite3_result_text        (       _db       : tsqlite3 ;
                                               _value    : tsqlite3 ;
                                               _nBytes   : Integer  ;
                                               _destr    : Tsqlite3_destroy
                                       ) ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  procedure sqlite3_result_value       (       _db       : tsqlite3 ;
                                               _value    : tsqlite3
                                       ) ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  procedure sqlite3_result_zeroblob    (       _db       : tsqlite3 ;
                                               _n        : Integer
                                       ) ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_value_blob          (       _value    : tsqlite3
                                       ) : tsqlite3 ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_value_bytes         (       _value    : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_value_double        (       _value    : tsqlite3
                                       ) : Double ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_value_int           (       _value    : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_value_int64         (       _value    : tsqlite3
                                       ) : Int64 ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_value_text          (       _value    : tsqlite3
                                       ) : tsqlite3 ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_value_type          (       _value    : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_value_numeric_type  (       _value    : tsqlite3
                                       ) : Integer ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;

  function sqlite3_last_insert_rowid   (       _value    : tsqlite3
                                       ) : Int64 ;
                                       {$IFDEF DEF_CDECL}
                                         cdecl ;
                                       {$ENDIF}
                                       external SQLITE_LIB_NAME ;
{$ENDIF}

type

  // Sqlite connection.
  TSqliteConnection = class( TGIS_AnyConnection )
    private
      FDB : tsqlite3 ;
    private
      // Check status code.
      // _code status code
      // _msg  info message
      procedure check        ( const _code : Integer ;
                               const _msg  : String
                             ) ;
    protected
      // Destructor.
      procedure doDestroy ; override;
    public
      // Constructor.
      constructor Create ; override;

      // Open connection.
      procedure Open  ; override;

      // Close connection.
      procedure Close ; override;
    public
      property DB : tsqlite3 read FDB ;
  end ;

    TGIS_SqliteFunction = class
      public
        CallbackFnc : Tsqlite3_func_callback ;
        InvokeFnc   : TGIS_DbFunction ;
        NArg        : Integer ;
        Args        : array of Variant ;
        {$IFDEF CLR}
        procedure DbCreateFunction( _ctx    : tsqlite3 ;
                                    _nArgs  : Integer ;
                                    _args   : tsqlite3
                                   ) ;
        {$ENDIF}
    end ;

  // Convert pointer to string.
  // _ptr    pointer to string buffer
  // return  string
  function convertString(
    _ptr : tsqlite3
  ) : String ;
  {$IFDEF CLR}
    var
      buf : array of Byte ;
      i   : Integer ;
  {$ENDIF}
  begin
    {$IFDEF CLR}
      if (_ptr = IntPtr.Zero) then begin
        Result := '' ;
        exit ;
      end ;
      i := 0 ;
      while Marshal.ReadByte( IntPtr(Int(_ptr)+i) ) <> 0 do
        inc(i);
      if (i = 0) then begin
        Result := '' ;
        exit ;
      end ;
      buf := new Byte[i] ;
      Marshal.Copy( _ptr, buf, 0, i ) ;
      Result := TEncoding.UTF8.GetString( buf, 0, i ) ;
      {$IFDEF OXYGENE}
        if not assigned( Result ) then
          Result := '' ;
      {$ENDIF}
    {$ELSE}
      Result := String( UTF8ToString(MarshaledAString( _ptr )) ) ;
    {$ENDIF}
  end ;

//-----------------------------------------------------------------------------
// TGIS_SqlSqlite
//-----------------------------------------------------------------------------

  constructor TGIS_DbSqlite.Create ;
  var
    i : Integer ;
  begin
    inherited ;

    {$IFDEF CLR}
      SQLITE_TRANSIENT  := IntPtr.Create(-1) ;
    {$ELSE}
      SQLITE_TRANSIENT  := Pointer(-1) ;
    {$ENDIF}

    {$IFDEF LEVEL_XE2_RTL}
      FDB := tsqlite3(0) ;
    {$ELSE}
      FDB := nil ;
    {$ENDIF}
    FSqliteConnection := nil ;
    SetLength( FStmts, 2 ) ;
    FStmts[0] := {$IFDEF LEVEL_XE2_RTL} tsqlite3(0) {$ELSE} nil {$ENDIF} ;
    FStmts[1] := {$IFDEF LEVEL_XE2_RTL} tsqlite3(0) {$ELSE} nil {$ENDIF} ;

    FFunctions := TGIS_ObjectList.Create( True ) ;
    FInMemory  := False ;

    for i := 0 to BUILTIN_CURSORS - 1 do
      cursorOpen( i ) ;
  end ;

  procedure TGIS_DbSqlite.doDestroy ;
  var
    i : Integer ;
  begin
    FreeObject( FFunctions ) ;

    for i := length( cursorDB ) - 1 downto 0 do
      cursorClose( i ) ;

    FInMemory := False ;
    sqlDisconnect ;

    {$IFDEF LEVEL_XE2_RTL}
      FDB := tsqlite3(0) ;
    {$ELSE}
      FDB := nil ;
    {$ENDIF}
    FinalizeProvider ;

    inherited ;
  end ;

  procedure TGIS_DbSqlite.allocStmts(
    const _id : Integer
   ) ;
  begin
    if _id >= length( FStmts ) then begin
      SetLength( FStmts, _id+1 ) ;
      FStmts[_id] := {$IFDEF LEVEL_XE2_RTL} tsqlite3(0) {$ELSE} nil {$ENDIF} ;
    end ;
  end ;

  procedure TGIS_DbSqlite.initializeDLL ;

    {$IFDEF CLR}
      procedure mapFnc( var _var    : IntPtr ;
                        const _name : String
                       ) ;
      begin
        _var := GetProcAddress( FDLLHandle, _name ) ;
        assert( _var <> nil ) ;
      end ;
    {$ELSE}
      procedure mapFnc(  var _var    : Pointer;
                         const _name : String
                       ) ;
      begin
        _var := GetProcAddress( FDLLHandle, PChar( _name ) ) ;
        assert( _var <> nil ) ;
      end ;
    {$ENDIF}

  begin
    {$IFDEF DEF_STATICLINK}
      FIsInitialized := True ;
    {$ELSE}
      FIsInitialized := False ;
      FDLLHandle     := LoadLibraryWithinHinstance( SQLITE_LIB_NAME ) ;

      if ( FDLLHandle = 0 )
         and
         not IsStringEmpty( SQLITE_LIB_NAME_ALT1 )
      then
        FDLLHandle := LoadLibraryWithinHinstance( SQLITE_LIB_NAME_ALT1 ) ;

      if ( FDLLHandle = 0 )
         and
         not IsStringEmpty( SQLITE_LIB_NAME_ALT2 )
      then
        FDLLHandle := LoadLibraryWithinHinstance( SQLITE_LIB_NAME_ALT2 ) ;

      if FDLLHandle = 0 then begin
        raise EGIS_Exception.Create(
                _rsrc( GIS_RS_ERR_FILEMAPPING ),
                SQLITE_LIB_NAME + #13#10 + SystemErrorMessage,
                0
              )
      end
      else begin
        FIsInitialized := True ;

        {$IFDEF CLR}
          // LoadLibrary is performed also for .NET DLLImport to
          //  - enforce loading form specific location
          //  - mapFnc is not really required but it can be tested if library is
          //    loaded properly (proper set of API is supported
          mapFnc( sqlite3_open_v2Proc,             'sqlite3_open_v2'             );
          mapFnc( sqlite3_closeProc,               'sqlite3_close'               );
          mapFnc( sqlite3_errmsgProc,              'sqlite3_errmsg'              );
          mapFnc( sqlite3_prepareProc,             'sqlite3_prepare_v2'          );
          mapFnc( sqlite3_stepProc,                'sqlite3_step'                );
          mapFnc( sqlite3_finalizeProc,            'sqlite3_finalize'            );
          mapFnc( sqlite3_resetProc,               'sqlite3_reset'               );
          mapFnc( sqlite3_execProc,                'sqlite3_exec'                );
          mapFnc( sqlite3_column_countProc,        'sqlite3_column_count'        );
          mapFnc( sqlite3_column_nameProc,         'sqlite3_column_name'         );
          mapFnc( sqlite3_column_decltypeProc,     'sqlite3_column_decltype'     );
          mapFnc( sqlite3_column_typeProc,         'sqlite3_column_type'         );
          mapFnc( sqlite3_column_blobProc,         'sqlite3_column_blob'         );
          mapFnc( sqlite3_column_bytesProc,        'sqlite3_column_bytes'        );
          mapFnc( sqlite3_column_doubleProc,       'sqlite3_column_double'       );
          mapFnc( sqlite3_column_textProc,         'sqlite3_column_text'         );
          mapFnc( sqlite3_column_intProc,          'sqlite3_column_int'          );
          mapFnc( sqlite3_column_int64Proc,        'sqlite3_column_int64'        );
          mapFnc( sqlite3_clear_bindingsProc,      'sqlite3_clear_bindings'      );
          mapFnc( sqlite3_bind_parameter_countProc,'sqlite3_bind_parameter_count');
          mapFnc( sqlite3_bind_parameter_indexProc,'sqlite3_bind_parameter_index');
          mapFnc( sqlite3_bind_parameter_nameProc, 'sqlite3_bind_parameter_name' );
          mapFnc( sqlite3_bind_blobProc,           'sqlite3_bind_blob'           );
          mapFnc( sqlite3_bind_doubleProc,         'sqlite3_bind_double'         );
          mapFnc( sqlite3_bind_intProc,            'sqlite3_bind_int'            );
          mapFnc( sqlite3_bind_int64Proc,          'sqlite3_bind_int64'          );
          mapFnc( sqlite3_bind_nullProc,           'sqlite3_bind_null'           );
          mapFnc( sqlite3_bind_textProc,           'sqlite3_bind_text'           );
          mapFnc( sqlite3_bind_valueProc,          'sqlite3_bind_value'          );
          mapFnc( sqlite3_progress_handlerProc,    'sqlite3_progress_handler'    );
          mapFnc( sqlite3_create_functionProc,     'sqlite3_create_function'     );
          mapFnc( sqlite3_user_dataProc,           'sqlite3_user_data'           );
          mapFnc( sqlite3_result_blobProc,         'sqlite3_result_blob'         );
          mapFnc( sqlite3_result_doubleProc,       'sqlite3_result_double'       );
          mapFnc( sqlite3_result_intProc,          'sqlite3_result_int'          );
          mapFnc( sqlite3_result_int64Proc,        'sqlite3_result_int64'        );
          mapFnc( sqlite3_result_nullProc,         'sqlite3_result_null'         );
          mapFnc( sqlite3_result_textProc,         'sqlite3_result_text'         );
          mapFnc( sqlite3_result_valueProc,        'sqlite3_result_value'        );
          mapFnc( sqlite3_result_zeroblobProc,     'sqlite3_result_zeroblob'     );
          mapFnc( sqlite3_value_blobProc,          'sqlite3_value_blob'          );
          mapFnc( sqlite3_value_bytesProc,         'sqlite3_value_bytes'         );
          mapFnc( sqlite3_value_doubleProc,        'sqlite3_value_double'        );
          mapFnc( sqlite3_value_intProc,           'sqlite3_value_int'           );
          mapFnc( sqlite3_value_int64Proc,         'sqlite3_value_int64'         );
          mapFnc( sqlite3_value_textProc,          'sqlite3_value_text'          );
          mapFnc( sqlite3_value_typeProc,          'sqlite3_value_type'          );
          mapFnc( sqlite3_value_numeric_typeProc,  'sqlite3_value_numeric_type'  );
          mapFnc( sqlite3_last_insert_rowidProc,   'sqlite3_last_insert_rowid'   );
        {$ELSE}
          mapFnc( @sqlite3_open_v2,                'sqlite3_open_v2'             );
          mapFnc( @sqlite3_close,                  'sqlite3_close'               );
          mapFnc( @sqlite3_errmsg,                 'sqlite3_errmsg'              );
          mapFnc( @sqlite3_prepare,                'sqlite3_prepare_v2'          );
          mapFnc( @sqlite3_step,                   'sqlite3_step'                );
          mapFnc( @sqlite3_finalize,               'sqlite3_finalize'            );
          mapFnc( @sqlite3_reset,                  'sqlite3_reset'               );
          mapFnc( @sqlite3_exec,                   'sqlite3_exec'                );
          mapFnc( @sqlite3_column_count,           'sqlite3_column_count'        );
          mapFnc( @sqlite3_column_name,            'sqlite3_column_name'         );
          mapFnc( @sqlite3_column_decltype,        'sqlite3_column_decltype'     );
          mapFnc( @sqlite3_column_type,            'sqlite3_column_type'         );
          mapFnc( @sqlite3_column_blob,            'sqlite3_column_blob'         );
          mapFnc( @sqlite3_column_bytes,           'sqlite3_column_bytes'        );
          mapFnc( @sqlite3_column_double,          'sqlite3_column_double'       );
          mapFnc( @sqlite3_column_text,            'sqlite3_column_text'         );
          mapFnc( @sqlite3_column_int,             'sqlite3_column_int'          );
          mapFnc( @sqlite3_column_int64,           'sqlite3_column_int64'        );
          mapFnc( @sqlite3_clear_bindings,         'sqlite3_clear_bindings'      );
          mapFnc( @sqlite3_bind_parameter_count,   'sqlite3_bind_parameter_count');
          mapFnc( @sqlite3_bind_parameter_index,   'sqlite3_bind_parameter_index');
          mapFnc( @sqlite3_bind_parameter_name,    'sqlite3_bind_parameter_name' );
          mapFnc( @sqlite3_bind_blob,              'sqlite3_bind_blob'           );
          mapFnc( @sqlite3_bind_double,            'sqlite3_bind_double'         );
          mapFnc( @sqlite3_bind_int,               'sqlite3_bind_int'            );
          mapFnc( @sqlite3_bind_int64,             'sqlite3_bind_int64'          );
          mapFnc( @sqlite3_bind_null,              'sqlite3_bind_null'           );
          mapFnc( @sqlite3_bind_text,              'sqlite3_bind_text'           );
          mapFnc( @sqlite3_bind_value,             'sqlite3_bind_value'          );
          mapFnc( @sqlite3_progress_handler,       'sqlite3_progress_handler'    );
          mapFnc( @sqlite3_create_function,        'sqlite3_create_function'     );
          mapFnc( @sqlite3_user_data,              'sqlite3_user_data'           );
          mapFnc( @sqlite3_result_blob,            'sqlite3_result_blob'         );
          mapFnc( @sqlite3_result_double,          'sqlite3_result_double'       );
          mapFnc( @sqlite3_result_int,             'sqlite3_result_int'          );
          mapFnc( @sqlite3_result_int64,           'sqlite3_result_int64'        );
          mapFnc( @sqlite3_result_null,            'sqlite3_result_null'         );
          mapFnc( @sqlite3_result_text,            'sqlite3_result_text'         );
          mapFnc( @sqlite3_result_value,           'sqlite3_result_value'        );
          mapFnc( @sqlite3_result_zeroblob,        'sqlite3_result_zeroblob'     );
          mapFnc( @sqlite3_value_blob,             'sqlite3_value_blob'          );
          mapFnc( @sqlite3_value_bytes,            'sqlite3_value_bytes'         );
          mapFnc( @sqlite3_value_double,           'sqlite3_value_double'        );
          mapFnc( @sqlite3_value_int,              'sqlite3_value_int'           );
          mapFnc( @sqlite3_value_int64,            'sqlite3_value_int64'         );
          mapFnc( @sqlite3_value_text,             'sqlite3_value_text'          );
          mapFnc( @sqlite3_value_type,             'sqlite3_value_type'          );
          mapFnc( @sqlite3_value_numeric_type,     'sqlite3_value_numeric_type'  );
          mapFnc( @sqlite3_last_insert_rowid,      'sqlite3_last_insert_rowid'   );
        {$ENDIF}
      end ;
    {$ENDIF}

    if FIsInitialized then begin
      {$IFDEF CLR}
        Gsqlite3_user_data := @sqlite3_user_data ;
      {$ELSE}
        @Gsqlite3_user_data := @sqlite3_user_data ;
      {$ENDIF}
    end ;
  end ;

  procedure TGIS_DbSqlite.finalizeDLL ;
  begin
    {$IFDEF DEF_DYNAMICLINK}
      if FDLLHandle <> 0 then begin
        FreeLibrary( FDLLHandle ) ;
        FDLLHandle := 0 ;
      end ;
    {$ENDIF}
    FIsInitialized := False ;
  end ;

  procedure TGIS_DbSqlite.check(
    const _code : Integer ;
    const _msg  : String
  ) ;
  var
    err : String ;
  begin
    if _code = SQLITE_OK then exit ;

    err := GetLastErrorMessage ;
    if IsStringEmpty( err ) then
      err := _msg ;

    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_SERVER_ERROR ), err, 0 ) ;
  end ;

  function TGIS_DbSqlite.InitializeProvider : Boolean ;
  begin
    initializeDLL ;

    Result := inherited InitializeProvider ;
  end ;

  procedure TGIS_DbSqlite.FinalizeProvider ;
  begin
    if FIsInitialized then
      finalizeDLL ;

    inherited ;
  end ;

  function TGIS_DbSqlite.GetLastErrorMessage : String ;
  var
    msg : tsqlite3 ;
  begin
    Result := '' ;

    {$IFDEF LEVEL_XE2_RTL}
      if FDB <> tsqlite3(0)   then begin
    {$ELSE}
      if AssignedPtr( FDB ) then begin
    {$ENDIF}
      msg    := sqlite3_errmsg( FDB ) ;
      Result := convertString( msg ) ;
    end ;
  end ;


  function TGIS_DbSqlite.sqlGetParams(
   const _idx : Integer
  ) : {$IFDEF OXYGENE}
        TGIS_StringList ;
      {$ELSE}
        TStringList ;
      {$ENDIF}
  begin
    Result := nil ;
  end ;

  procedure TGIS_DbSqlite.sqlTableCreateParam(
    const _id       : Integer ;
    const _name     : String ;
    const _type     : TGIS_DataType ;
    const _subtype  : TGIS_SubDataType ;
    const _size     : Integer
  ) ;
  begin
    // for safe inheritance
  end ;

  procedure TGIS_DbSqlite.sqlConnect(
    const _folder        : String ;
    const _sqlParameters : {$IFDEF OXYGENE}
                             TGIS_Strings
                           {$ELSE}
                             TStrings
                           {$ENDIF}
  ) ;
  var
    cstr : String ;
    pass : String ;
    i    : Integer;
  begin
    {$IFDEF LEVEL_XE2_RTL}
      FDB := tsqlite3(0) ;
    {$ELSE}
      FDB := nil ;
    {$ENDIF}
    cstr := _sqlParameters.Values[ GIS_INI_LAYERSQL_CONNECTOR_SQLITE ] ;
    FInMemory := Pos( ':memory:', cstr ) >= StringFirst  ;

    if FInMemory then
      cstr := _sqlParameters.Values[ GIS_INI_LAYERSQL_CONNECTOR_SQLITE ]
    else
      cstr := GetPathAbsolute(
                _folder,
                _sqlParameters.Values[ GIS_INI_LAYERSQL_CONNECTOR_SQLITE ]
              ) ;

    if not assigned( FSqliteConnection ) then
      FSqliteConnection := SharedConnections.OpenAny(
                              _sqlParameters.Values[ GIS_INI_LAYERSQL_USER     ],
                              _sqlParameters.Values[ GIS_INI_LAYERSQL_PASSWORD ],
                              cstr,
                              _sqlParameters.Values[ GIS_INI_LAYERSQL_SCHEMA   ],
                              _sqlParameters,
                              TSqliteConnection
                            ) ;

    if assigned( FSqliteConnection ) then
      FDB := TSqliteConnection( FSqliteConnection ).DB ;

    // pass encryption key, must be first command
    pass := _sqlParameters.Values[ GIS_INI_LAYERSQL_PASSWORD ] ;
    if not IsStringEmpty( pass ) then
      sqlExec( 'PRAGMA key = ' + QuotedStr( pass ) )
    else begin
      pass := _sqlParameters.Values[ 'PRAGMA key' ] ;
      if not IsStringEmpty( pass ) then
        sqlExec( 'PRAGMA key = ' + QuotedStr( pass ) ) ;
    end ;

    for i := 0 to _sqlParameters.Count - 1 do
      if Pos( 'PRAGMA', _sqlParameters[i] ) = StringFirst then
        sqlExec( _sqlParameters[i] ) ;
  end ;

  procedure TGIS_DbSqlite.sqlDisconnect ;
  begin
    if FInMemory then exit ;

    {$IFDEF LEVEL_XE2_RTL}
      if FDB <> tsqlite3(0)   then begin
    {$ELSE}
      if AssignedPtr( FDB ) then begin
    {$ENDIF}
      SharedConnections.CloseAny( TSqliteConnection( FSqliteConnection ) ) ;
      FSqliteConnection := nil ;
      {$IFDEF LEVEL_XE2_RTL}
        FDB := tsqlite3(0) ;
      {$ELSE}
        FDB := nil ;
      {$ENDIF}
    end ;
  end ;

  procedure TGIS_DbSqlite.sqlTransactUpdateStart ;
  begin
    if options.UpdateTransact then
      sqlExec( 'BEGIN IMMEDIATE;' ) ;
  end ;

  procedure TGIS_DbSqlite.sqlTransactUpdateCommit ;
  begin
    if options.UpdateTransact then
      sqlExec( 'COMMIT;' ) ;
  end ;

  procedure TGIS_DbSqlite.sqlTransactGlobalUpdateStart ;
  begin
    if options.GlobalUpdateTransact then
      sqlExec( 'BEGIN IMMEDIATE;' ) ;
  end ;

  procedure TGIS_DbSqlite.sqlTransactGlobalUpdateCommit ;
  begin
    if options.GlobalUpdateTransact then
      sqlExec( 'COMMIT;' ) ;
  end ;

  procedure TGIS_DbSqlite.sqlTransactRestructStart ;
  begin
    if options.RestructTransact then
      sqlExec( 'BEGIN IMMEDIATE;' ) ;
  end ;

  procedure TGIS_DbSqlite.sqlTransactRestructCommit ;
  begin
    if options.RestructTransact then
      sqlExec( 'COMMIT;' ) ;
  end ;

  procedure TGIS_DbSqlite.sqlTransactRollback ;
  begin
    sqlExec( 'ROLLBACK;' ) ;
  end ;

  procedure TGIS_DbSqlite.sqlQueryOpen(
    const _query   : String ;
    const _cursor  : Integer
  ) ;
  var
    cmdTail : tsqlite3 ;
    {$IFDEF GIS_DEBUG}
      t1  : Cardinal ;
    {$ENDIF}
    {$IFDEF CLR}
    cmd : TBytes ;
    {$ELSE}
    cmd : tsqlite3 ;
    {$ENDIF}
  begin
    {$IFDEF LEVEL_XE2_RTL}
      cmdTail := tsqlite3(0) ;
    {$ELSE}
      cmdTail := nil ;
    {$ENDIF}

    {$IFDEF LEVEL_XE2_RTL}
      if ( cursorDB[_cursor].fStmt <> tsqlite3(0) ) and
    {$ELSE}
      if AssignedPtr( cursorDB[_cursor].fStmt )   and
    {$ENDIF}
         ( cursorDB[_cursor].fStmtRC <> SQLITE_OK ) then begin
      if FReuseQuery and (cursorDB[_cursor].oldQuery = _query) then begin
        try
          sqlQueryMoveFirst( _cursor ) ;
          exit ;
        except
          sqlQueryClose( _cursor ) ;
        end
      end
      else
        sqlQueryClose( _cursor ) ;
    end ;

    cursorDB[_cursor].fStmtRC := SQLITE_OK ;

    if assigned( FOnSQLExecute ) then
      FOnSQLExecute( _query );

    {$IFDEF GIS_DEBUG}
    t1 := GetTickCount ;
    {$ENDIF}

    {$IFDEF DCC}
      cmd := MarshaledAString(UTF8Encode(_query)) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( _query ) ;
    {$ENDIF}
    check( sqlite3_prepare( FDB,
                             cmd,
                             -1,
                             cursorDB[_cursor].fStmt,
                             cmdTail
                           ),
           'Failed to prepare query'
         ) ;

    cursorDB[_cursor].fStmtRC := sqlite3_step( cursorDB[_cursor].fStmt ) ;

    {$IFDEF GIS_DEBUG}
    if Assigned( FOnSQLExecute ) then
      FOnSQLExecute( ' Execute time : ' +FloatToStr( ( GetTickCount - t1 ) / 1000 ) + ' s' );
    {$ENDIF}

    if ( cursorDB[_cursor].fStmtRC <> SQLITE_DONE ) and
       ( cursorDB[_cursor].fStmtRC <> SQLITE_ROW  ) then begin
      try
        check( cursorDB[_cursor].fStmtRC, 'Failed to open query' );
      finally
        sqlQueryClose( _cursor ) ;
      end ;
    end ;

    cursorDB[_cursor].oldQuery := _query ;

    getStructure( _cursor ) ;
  end ;

  procedure TGIS_DbSqlite.sqlQueryReset(
    const _cursor  : Integer
  ) ;
  begin
    cursorDB[_cursor].oldQuery := '' ;
  end ;

  procedure TGIS_DbSqlite.sqlQueryClose(
    const _cursor : Integer
  ) ;
  begin
    {$IFDEF LEVEL_XE2_RTL}
      if ( cursorDB[_cursor].fStmt <> tsqlite3(0) ) and
    {$ELSE}
      if AssignedPtr( cursorDB[_cursor].fStmt )   and
    {$ENDIF}
         ( cursorDB[_cursor].fStmtRC <> SQLITE_OK ) then begin
      sqlite3_finalize( cursorDB[_cursor].fStmt ) ;
      {$IFDEF LEVEL_XE2_RTL}
        cursorDB[_cursor].fStmt := tsqlite3(0) ;
      {$ELSE}
        cursorDB[_cursor].fStmt := nil ;
      {$ENDIF}
      cursorDB[_cursor].fStmtRC := SQLITE_OK ;
    end ;
  end ;

  procedure TGIS_DbSqlite.sqlTableClose(
    const _id : Integer
  ) ;
  begin
    if _id < length( FStmts ) then begin
      {$IFDEF LEVEL_XE2_RTL}
        if FStmts[ _id ] <> tsqlite3(0)   then begin
      {$ELSE}
        if AssignedPtr( FStmts[ _id ] ) then begin
      {$ENDIF}
        sqlite3_finalize( FStmts[ _id ] ) ;
        {$IFDEF LEVEL_XE2_RTL}
          FStmts[ _id ] := tsqlite3(0) ;
        {$ELSE}
          FStmts[ _id ] := nil ;
        {$ENDIF}
      end ;
    end ;
  end ;

  procedure TGIS_DbSqlite.sqlTablePost(
    const _id : Integer
  ) ;
  var
    rc : Integer ;
  begin
    if assigned( FOnSQLExecute ) then
      FOnSQLExecute( ' Post table ' + IntToStr( _id ) ) ;

    rc := sqlite3_step( FStmts[ _id ] ) ;

    if ( rc <> SQLITE_DONE ) and ( rc <> SQLITE_ROW  ) then begin
      try
        check( rc, 'Failed to post table' );
      finally
        {$IFDEF LEVEL_XE2_RTL}
          if FStmts[ _id ] <> tsqlite3(0)   then begin
        {$ELSE}
          if AssignedPtr( FStmts[ _id ] ) then begin
        {$ENDIF}
          sqlite3_finalize( FStmts[ _id ] ) ;
          {$IFDEF LEVEL_XE2_RTL}
            FStmts[ _id ] := tsqlite3(0) ;
          {$ELSE}
            FStmts[ _id ] := nil ;
          {$ENDIF}
        end ;
      end ;
    end ;

    if not InBatchMode then
      sqlTableClose( _id ) ;
  end ;

  procedure TGIS_DbSqlite.sqlTableExec(
    const _id : Integer
  ) ;
  var
    rc : Integer ;
  begin
    if assigned( FOnSQLExecute ) then
      FOnSQLExecute( ' Post table ' + IntToStr( _id ) ) ;

    rc := sqlite3_step( FStmts[ _id ] ) ;

    if ( rc <> SQLITE_DONE ) and ( rc <> SQLITE_ROW  ) then begin
      try
        check( rc, 'Failed to post table' );
      finally
        {$IFDEF LEVEL_XE2_RTL}
          if FStmts[ _id ] <> tsqlite3(0)   then begin
        {$ELSE}
          if AssignedPtr( FStmts[ _id ] ) then begin
        {$ENDIF}
          sqlite3_finalize( FStmts[ _id ] ) ;
          {$IFDEF LEVEL_XE2_RTL}
            FStmts[ _id ] := tsqlite3(0) ;
          {$ELSE}
            FStmts[ _id ] := nil ;
          {$ENDIF}
        end ;
      end ;
    end ;

  end ;

  procedure TGIS_DbSqlite.sqlTableOpenRead(
    const _id     : Integer ;
    const _query  : String
  ) ;
  begin
   //
  end ;

  procedure TGIS_DbSqlite.sqlTableAppend(
    const _id       : Integer ;
    const _query    : String
  ) ;
  var
    cmdTail : tsqlite3 ;
    {$IFDEF CLR}
    cmd : TBytes ;
    {$ELSE}
    cmd : tsqlite3 ;
    {$ENDIF}
  begin
    if assigned( FOnSQLExecute ) then
      FOnSQLExecute( _query );

    {$IFDEF LEVEL_XE2_RTL}
      cmdTail := tsqlite3(0) ;
    {$ELSE}
      cmdTail := nil ;
    {$ENDIF}
    allocStmts( _id ) ;
    {$IFDEF LEVEL_XE2_RTL}
      if FStmts[_id] = tsqlite3(0) then begin
    {$ELSE}
      if FStmts[_id] = nil then begin
    {$ENDIF}
      {$IFNDEF OXYGENE}
        cmd := MarshaledAString(UTF8Encode(_query) ) ;
      {$ELSE}
        cmd := TEncoding.UTF8.GetBytes( _query ) ;
      {$ENDIF}
      check( sqlite3_prepare( FDB,
                               cmd,
                               -1,
                               FStmts[_id],
                               cmdTail
                             ),
             'Failed to prepare table for append'
           ) ;
    end
    else begin
      check( sqlite3_reset( FStmts[ _id ] ), '' ) ;
      check( sqlite3_clear_bindings( FStmts[ _id ] ), '' ) ;
    end ;
  end ;

  function TGIS_DbSqlite.sqlTablePrepared(
    const _id  : Integer
  ) : Boolean ;
  begin
    {$IFDEF LEVEL_XE2_RTL}
      Result := FStmts[_id] <> tsqlite3(0) ;
    {$ELSE}
      Result := FStmts[_id] <> nil ;
    {$ENDIF}
  end ;

  procedure TGIS_DbSqlite.sqlTableOpenWrite(
    const _id     : Integer ;
    const _query  : String
   ) ;
  var
    cmdTail : tsqlite3 ;
    {$IFDEF CLR}
    cmd : TBytes ;
    {$ELSE}
    cmd : tsqlite3 ;
    {$ENDIF}
  begin
    if assigned( FOnSQLExecute ) then
      FOnSQLExecute( _query );

    {$IFDEF LEVEL_XE2_RTL}
      cmdTail := tsqlite3(0) ;
    {$ELSE}
      cmdTail := nil ;
    {$ENDIF}
    allocStmts( _id ) ;
    {$IFDEF LEVEL_XE2_RTL}
      if FStmts[_id] = tsqlite3(0) then begin
    {$ELSE}
      if FStmts[_id] = nil then begin
    {$ENDIF}
      {$IFNDEF OXYGENE}
        cmd := MarshaledAString(UTF8Encode(_query) ) ;
      {$ELSE}
        cmd := TEncoding.UTF8.GetBytes( _query ) ;
      {$ENDIF}
      check( sqlite3_prepare( FDB,
                               cmd,
                               -1,
                               FStmts[_id],
                               cmdTail
                              ),
             'Failed to prepare table for update'
            ) ;
    end
    else begin
      check( sqlite3_reset( FStmts[ _id ] ), '' ) ;
      check( sqlite3_clear_bindings( FStmts[ _id ] ), '' ) ;
    end ;
  end ;

  procedure TGIS_DbSqlite.sqlExec(
    const _command : String
  ) ;
  var
    erMsg : tsqlite3 ;
    {$IFDEF CLR}
    cmd : TBytes ;
    {$ELSE}
    cmd : tsqlite3 ;
    {$ENDIF}
  begin
    if assigned( FOnSQLExecute ) then
      FOnSQLExecute( _command );

    {$IFNDEF OXYGENE}
      cmd := MarshaledAString(UTF8Encode(_command) ) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( _command ) ;
    {$ENDIF}

    check( sqlite3_exec( FDB,
                          cmd,
                          {$IFDEF LEVEL_XE2_RTL}
                            tsqlite3(0),
                            tsqlite3(0),
                          {$ELSE}
                            nil,
                            nil,
                          {$ENDIF}
                          erMsg
                         ),
           'Failed to exec command'
         ) ;
  end ;

  procedure TGIS_DbSqlite.sqlQueryMoveFirst(
    const _cursor : Integer
  ) ;
  begin
    {$IFDEF LEVEL_XE2_RTL}
      if cursorDB[_cursor].fStmt <> tsqlite3(0)   then begin
    {$ELSE}
      if AssignedPtr( cursorDB[_cursor].fStmt ) then begin
    {$ENDIF}
      check( sqlite3_reset( cursorDB[_cursor].fStmt ), 'Failed to reset query' ) ;
      cursorDB[_cursor].fStmtRC := sqlite3_step( cursorDB[_cursor].fStmt ) ;
    end ;
  end ;

  procedure TGIS_DbSqlite.sqlQueryMoveNext(
    const _cursor : Integer
  ) ;
  begin
    {$IFDEF LEVEL_XE2_RTL}
      if cursorDB[_cursor].fStmt <> tsqlite3(0)   then
    {$ELSE}
      if AssignedPtr( cursorDB[_cursor].fStmt ) then
    {$ENDIF}
      cursorDB[_cursor].fStmtRC := sqlite3_step( cursorDB[_cursor].fStmt ) ;
  end ;

  function  TGIS_DbSqlite.sqlQueryEof(
    const _cursor : Integer
  ) : Boolean ;
  begin
    Result := ( cursorDB[_cursor].fStmtRC = SQLITE_DONE ) or
              ( cursorDB[_cursor].fStmtRC = SQLITE_OK   )
  end ;

  function TGIS_DbSqlite.sqlTableEof(
    const _id : Integer
  ) : Boolean ;
  begin
    Result := True ;
  end ;

  function TGIS_DbSqlite.sqlQueryGetFieldIndex(
    const _name    : String ;
    const _cursor  : Integer
   ) : Integer ;
  begin
    Result := cursorDB[_cursor].fStmtCN.IndexOf( _name ) ;
  end ;

  function TGIS_DbSqlite.sqlQueryGetField(
    const _name   : String ;
    const _cursor : Integer
   ) : Variant ;
  var
    id : Integer ;
  begin
    id := cursorDB[_cursor].fStmtCN.IndexOf( _name ) ;
    if id > -1 then
      Result := sqlQueryGetFieldById( id, _cursor )
    else
      Result := Unassigned ;
  end ;

  function TGIS_DbSqlite.sqlQueryGetFieldById(
    const _id     : Integer ;
    const _cursor : Integer
  ) : Variant ;
  var
    size  : Integer ;
    ptvar : tsqlite3 ;
    dts   : String ;
    dt    : TDateTime ;
    v     : Variant ;
  begin
    {$IFDEF LEVEL_XE2_RTL}
      assert( cursorDB[_cursor].fStmt <> tsqlite3(0), 'Query not opened' ) ;
    {$ELSE}
      assert( cursorDB[_cursor].fStmt <> nil,       'Query not opened' ) ;
    {$ENDIF}
    assert( not sqlQueryEof( _cursor ), 'Query Eof reached'   ) ;

    Result := Unassigned ;
    if (_id >= cursorDB[_cursor].fStmtCN.Count) or (_id < 0) then exit ;

    if (cursorDB[_cursor].fStmtCT[_id].dType <> SQLITE_DATA_TEXT) and
       (sqlite3_column_type(cursorDB[_cursor].fStmt,_id) = SQLITE_DATA_NULL) then
    begin
      Result := NullVar ;
      exit ;
    end ;

    try
      case cursorDB[_cursor].fStmtCT[_id].dType of
        SQLITE_DATA_INTEGER :
          Result := sqlite3_column_int64( cursorDB[_cursor].fStmt, _id ) ;
        SQLITE_DATA_FLOAT   :
          Result := sqlite3_column_double( cursorDB[_cursor].fStmt, _id ) ;
        SQLITE_DATA_TEXT    :
          Result := convertString( sqlite3_column_text( cursorDB[_cursor].fStmt, _id ) ) ;
        SQLITE_DATA_BLOB    : begin
          size   := sqlite3_column_bytes( cursorDB[_cursor].fStmt, _id ) ;
          ptvar  := sqlite3_column_blob( cursorDB[_cursor].fStmt, _id );

          if (size > 0) and ( ptvar <> nil ) then begin
            Result := VarArrayCreate( [0,size-1], varByte ) ;
            VarFromPtr( Result, IntPtr(ptvar), size ) ;
          end ;
        end ;
        SQLITE_DATA_NULL    :
          Result := Unassigned ;
        SQLITE_DATA_BOOL    : begin
          dts := convertString( sqlite3_column_text( cursorDB[_cursor].fStmt, _id ) ) ;
          Result := VarToBoolean(dts) ;
        end ;
        SQLITE_DATA_DATE    : begin
          dts := convertString( sqlite3_column_text( cursorDB[_cursor].fStmt, _id ) ) ;
          if IsStringEmpty(dts) then
            Result := Unassigned
          else begin
          {$IFDEF CLR}
            if DateTime.TryParse( dts, dt ) then
          {$ELSE}
            if TryStrToDateTime( dts, dt ) then
          {$ENDIF}
              Result := dt  // date as text
          else begin
            v := dts ;    // date as double
            Result := VarAsType( v, varDate ) ;
          end ;
        end
        end
        else
          Result := Unassigned ;
      end ;
    except
      Result := Unassigned ;
    end ;
  end ;

  function TGIS_DbSqlite.sqlTableGetField(
    const _id   : Integer ;
    const _name : String
   ) : Variant ;
  begin
    Result := Unassigned ;
  end ;

  function TGIS_DbSqlite.sqlTableGetParam(
    const _id   : Integer ;
    const _name : String
   ) : Variant ;
  begin
    Result := Unassigned ;
  end ;

  function TGIS_DbSqlite.sqlBindField(
    const _field  : String ;
    const _cursor : Integer
   ) : Integer ;
  begin
    Result := sqlQueryGetFieldIndex( _field, _cursor ) ;
  end ;

  procedure TGIS_DbSqlite.sqlTableSetField(
    const _id           : Integer ;
    const _name         : String ;
    const _val          : Variant ;
    const _defSize      : Integer
   ) ;
  var
    idx     : Integer ;
    y,m,d   : Word ;
    h,min,
    s, ms   : Word ;

    {$IFDEF CLR}
      cmd : TBytes ;
    {$ELSE}
      cmd : tsqlite3 ;
    {$ENDIF}
    dfmt  : String ;
  begin
    {$IFDEF LEVEL_XE2_RTL}
      if IsStringEmpty( _name ) or ( FStmts[_id] = tsqlite3(0) ) then exit ;
    {$ELSE}
      if IsStringEmpty( _name ) or ( FStmts[_id] = nil       ) then exit ;
    {$ENDIF}

    {$IFNDEF OXYGENE}
      cmd := MarshaledAString(UTF8Encode(':' + _name) ) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( ':' + _name ) ;
    {$ENDIF}
    idx := sqlite3_bind_parameter_index( FStmts[_id],
                                      cmd
                                     ) ;
    if idx < 1 then exit ;

    case GetVariantType( _val ) of
      TGIS_VariantType.AnsiString  :
        begin
          {$IFNDEF OXYGENE}
            cmd := MarshaledAString(UTF8Encode(VarToString( _val )) ) ;
          {$ELSE}
            cmd := TEncoding.UTF8.GetBytes( VarToString( _val ) ) ;
          {$ENDIF}
          check( sqlite3_bind_text(
                   FStmts[_id],
                   idx,
                   cmd,
                   -1,
                   SQLITE_TRANSIENT
                 ), '' ) ;
        end ;
      TGIS_VariantType.WideString  :
        begin
          {$IFNDEF OXYGENE}
            cmd := MarshaledAString(UTF8Encode(VarToString( _val )) ) ;
          {$ELSE}
            cmd := TEncoding.UTF8.GetBytes( VarToString( _val ) ) ;
          {$ENDIF}
          check( sqlite3_bind_text(
                   FStmts[_id],
                   idx,
                   cmd,
                   -1,
                   SQLITE_TRANSIENT
                 ), '' ) ;
        end ;
      TGIS_VariantType.Boolean     :
        if VarToBoolean( _val ) then
          check( sqlite3_bind_int( FStmts[_id], idx, 1 ), '' )
        else
          check( sqlite3_bind_int( FStmts[_id], idx, 0 ), '' ) ;
      TGIS_VariantType.Int,
      TGIS_VariantType.UInt        :
        check( sqlite3_bind_int( FStmts[_id], idx, VarToInt32( _val ) ), '' ) ;
      TGIS_VariantType.Int64,
      TGIS_VariantType.UInt64      :
        check( sqlite3_bind_int64( FStmts[_id], idx, VarToInt64( _val ) ), '' ) ;
      TGIS_VariantType.Float       :
        check( sqlite3_bind_double( FStmts[_id], idx, VarToDouble( _val ) ), '' ) ;
      TGIS_VariantType.DateTime    :
        begin
          DecodeDate( VarToDateTime( _val ), y, m, d ) ;
          DecodeTime( VarToDateTime( _val ), h, min, s, ms ) ;

          {$IFDEF OXYGENE}
          if length( VarToDateTime( _val ).ToString ) > 10 then
          {$ELSE}
          if length( _val ) > 10 then
          {$ENDIF}
            dfmt := 'yyyy-MM-dd hh:nn:ss'
          else
            dfmt := 'yyyy-MM-dd' ;

          {$IFNDEF OXYGENE}
            cmd := MarshaledAString(UTF8Encode(FormatDateTime(dfmt,
                                                              EncodeDate(y,m,d)+EncodeTime(h,min,s,ms)
                                                              )
                                               )
                                    ) ;
          {$ELSE}
            cmd := TEncoding.UTF8.GetBytes( FormatDateTime(dfmt,
                                          EncodeDate(y,m,d).AddHours(h).AddMinutes(min).AddSeconds(s)
                                          )
                          ) ;
          {$ENDIF}
          check( sqlite3_bind_text(
                   FStmts[_id],
                   idx,
                   cmd,
                   -1,
                   SQLITE_TRANSIENT
                 ), '' ) ;
        end ;
      TGIS_VariantType.Nothing     : check( sqlite3_bind_null( FStmts[_id], idx ), '' ) ;
    end ;
  end ;

  function TGIS_DbSqlite.sqlGetBindedField(
    const _field         : Integer ;
    const _rebindFields  : Boolean ;
    const _cursor        : Integer
   ) : Variant ;
  begin
    Result := sqlQueryGetFieldById( _field, _cursor ) ;
  end ;

  procedure TGIS_DbSqlite.getStructure(
    const _cursor : Integer
  ) ;
  var
    i, nCols  : Integer ;
    fname     : String ;
  begin
    nCols := sqlite3_column_count( cursorDB[_cursor].fStmt );

    SetLength( cursorDB[_cursor].fStmtCT, nCols ) ;
    cursorDB[_cursor].fStmtCN.Clear ;

    for i := 0 to nCols - 1 do begin
      getColumnInfo( _cursor, i, fname, cursorDB[_cursor].fStmtCT[ i ].dType,
                     cursorDB[_cursor].fStmtCT[ i ].dName,
                     cursorDB[_cursor].fStmtCT[ i ].dSize,
                     cursorDB[_cursor].fStmtCT[ i ].dPrec
                    ) ;
      cursorDB[_cursor].fStmtCN.Add( fname ) ;
    end ;

    cursorDB[_cursor].fColUid     := -1 ;
    cursorDB[_cursor].fColGeom    := -1 ;
    cursorDB[_cursor].fColShpType := -1 ;
    cursorDB[_cursor].fColXmin    := -1 ;
    cursorDB[_cursor].fColYmin    := -1 ;
  end ;

  procedure TGIS_DbSqlite.getColumnInfo(
    const _cursor : Integer ;
    const _index  : Integer ;
      var _name   : String ;
      var _dType  : Integer ;
      var _dName  : String  ;
      var _dSize  : Integer ;
      var _dPrec  : Integer
  ) ;
    var
      i1,
      i2    : Integer ;
      sArgs,
      sMod,
      btName: String ;

      procedure setPrecScale( const _defPrec, _defScale : Integer ) ;
      var
        sPrec,
        sScale : String ;
        i      : Integer ;
      begin
        i := Pos(',', sArgs) ;
        if i = StringFirst - 1 then
          sPrec := sArgs
        else begin
          sPrec  := Copy(sArgs, StringFirst, i - StringFirst) ;
          sScale := Copy(sArgs, i + 1, length(sArgs)) ;
        end ;
        _dSize := StrToIntDef(sPrec, _defPrec) ;
        _dPrec := StrToIntDef(sScale, _defScale) ;
      end ;

      procedure setLen( const _defLen : Integer ) ;
      begin
        _dSize := StrToIntDef( sArgs, _defLen ) ;
      end ;

  begin
    _dSize  := 0 ;
    _dPrec  := 0 ;

    {$IFDEF LEVEL_XE2_RTL}
      if cursorDB[_cursor].fStmt <> tsqlite3(0) then begin
    {$ELSE}
      if AssignedPtr( cursorDB[_cursor].fStmt ) then begin
    {$ENDIF}
      _name  := convertString( sqlite3_column_name( cursorDB[_cursor].fStmt, _index ) ) ;
      _dType := sqlite3_column_type( cursorDB[_cursor].fStmt, _index ) ;
      _dName := convertString( sqlite3_column_decltype( cursorDB[_cursor].fStmt, _index ) ) ;

      i1 := Pos('(', _dName);
      i2 := Pos(')', _dName);
      if i1 = StringFirst - 1 then begin
        btName := Trim( UpperCase( _dName ) ) ;
        sArgs := '';
      end
      else begin
        btName := Trim( UpperCase( Copy( _dName, StringFirst, i1 - StringFirst ) ) ) ;
        sArgs := Trim(Copy(_dName, i1 + 1, i2 - i1 - 1));
      end ;

      i1 := Pos(' ', btName);
      if i1 = StringFirst - 1 then
        sMod := ''
      else begin
        sMod := Trim(Copy(btName, i1 + 1, length(btName)));
        btName := Copy(btName, StringFirst, i1 - StringFirst)
      end ;

      if (btName = 'BIT') or (btName = 'BOOL') or (btName = 'BOOLEAN') or
         (btName = 'LOGICAL') or (btName = 'YESNO') then begin
        _dType := SQLITE_DATA_BOOL ;
      end
      else if (btName = 'TINYINT') or (btName = 'SHORTINT') or
              (btName = 'INT8') or (btName = 'BYTE') or
              (btName = 'UINT8') or (btName = 'SMALLINT') or
              (btName = 'INT16') or (btName = 'WORD') or
              (btName = 'UINT16') or (btName = 'YEAR')
              then begin
        _dType := SQLITE_DATA_INTEGER ;
        _dSize := 3 ;
      end
      else if (btName = 'SMALLINT') or
              (btName = 'INT16') or (btName = 'WORD') or
              (btName = 'UINT16') or (btName = 'YEAR')
              then begin
        _dType := SQLITE_DATA_INTEGER ;
        _dSize := 5 ;
      end
      else if (btName = 'INTEGER') or (btName = 'MEDIUMINT') or
              (btName = 'INT') or (btName = 'INT32') or
              (btName = 'LONGWORD') or (btName = 'UINT32') then begin
        _dType := SQLITE_DATA_INTEGER ;
        _dSize := 10 ;
      end
      else if (btName = 'BIGINT') or (btName = 'INT64') or
              (btName = 'COUNTER') or (btName = 'AUTOINCREMENT') or
              (btName = 'IDENTITY') or
              (btName = 'LONGLONGWORD') or (btName = 'UINT64') then begin
        _dType := SQLITE_DATA_INTEGER ;
        _dSize := 20 ;
      end
      else if (btName = 'FLOAT') or (btName = 'REAL') or
              ((btName = 'DOUBLE') or (btName = 'SINGLE')) and
              (IsStringEmpty(sMod) or (sMod = 'PRECISION')) then begin
        setPrecScale(20, 8);
        _dType := SQLITE_DATA_FLOAT ;
      end
      else if (btName = 'DECIMAL') or (btName = 'DEC') or
              (btName = 'NUMERIC') or (btName = 'NUMBER') then begin
          setPrecScale(10, 0);
          _dType := SQLITE_DATA_INTEGER ;
      end
      else if (btName = 'MONEY') or (btName = 'SMALLMONEY') or
              (btName = 'CURRENCY') or (btName = 'FINANCIAL') then begin
        setPrecScale(19, 4);
          _dType := SQLITE_DATA_INTEGER ;
      end
      else if (btName = 'DATE') or (btName = 'SMALLDATE') or
              (btName = 'DATETIME') or (btName = 'SMALLDATETIME') or
              (btName = 'TIMESTAMP') or (btName = 'TIME') then begin
        _dType := SQLITE_DATA_DATE ;
        _dSize := 10 ;
      end
      else if ((btName = 'CHAR') or (btName = 'CHARACTER')) and IsStringEmpty(sMod) then begin
        setLen(255);
        _dType := SQLITE_DATA_TEXT ;
      end
      else if (btName = 'VARCHAR') or (btName = 'VARCHAR2') or (btName = 'TYNITEXT') or
              ((btName = 'CHARACTER') or (btName = 'CHAR')) and (sMod = 'VARYING') then begin
        setLen(255);
        _dType := SQLITE_DATA_TEXT
      end
      else if (btName = 'NCHAR') or (btName = 'NATIONAL') and (
              (sMod = 'CHAR') or (sMod = 'CHARACTER')) then begin
        setLen(1);
        _dType := SQLITE_DATA_TEXT
      end
      else if (btName = 'NVARCHAR') or (btName = 'NVARCHAR2') or
              (btName = 'NATIONAL') and (
                (sMod = 'CHAR VARYING') or (sMod = 'CHARACTER VARYING') or
                (sMod = 'VARYING CHAR') or (sMod = 'VARYING CHARACTER')) or
              (btName = 'STRING') then begin
        setLen(255);
        _dType := SQLITE_DATA_TEXT
      end
      else if (btName = 'RAW') or (btName = 'TYNIBLOB') or (btName = 'VARBINARY') or
              (btName = 'BINARY') and (IsStringEmpty(sMod) or (sMod = 'VARYING')) then begin
        setLen(GIS_SQL_MEMO_SIZE);
        _dType := SQLITE_DATA_TEXT
      end
      else if (btName = 'BLOB') or (btName = 'MEDIUMBLOB') or (btName = 'IMAGE') or
              (btName = 'LONGBLOB') or (btName = 'LONG') and
              ((sMod = 'BINARY') or (sMod = 'RAW')) or
              (btName = 'LONGVARBINARY') or (btName = 'GENERAL') or
              (btName = 'OLEOBJECT') or (btName = 'TINYBLOB') then begin
        _dType := SQLITE_DATA_BLOB
      end
      else if (btName = 'TEXT') or (btName = 'MEDIUMTEXT') or (btName = 'LONGTEXT') or
              (btName = 'CLOB') or (btName = 'MEMO') or (btName = 'NOTE') or
              (btName = 'LONG') and (IsStringEmpty(sMod) or (sMod = 'TEXT')) or
              (btName = 'LONGCHAR') or (btName = 'LONGVARCHAR') or
              (btName = 'TINYTEXT') then begin
        _dType := SQLITE_DATA_TEXT ;
        _dSize := GIS_SQL_MEMO_SIZE ;
      end
      else if (btName = 'NTEXT') or (btName = 'WTEXT') or
              (btName = 'NCLOB') or (btName = 'NMEMO') or
              (btName = 'LONG') and ((sMod = 'NTEXT') or (sMod = 'WTEXT')) or
              (btName = 'NATIONAL') and (sMod = 'TEXT') or
              (btName = 'LONGWCHAR') or (btName = 'LONGWVARCHAR') or
              (btName = 'HTML') then begin
        _dType := SQLITE_DATA_TEXT ;
        _dSize := GIS_SQL_MEMO_SIZE ;
      end
      else if (btName = 'XMLDATA') or (btName = 'XMLTYPE') or
              (btName = 'XML') then begin
        _dType := SQLITE_DATA_TEXT ;
        _dSize := GIS_SQL_MEMO_SIZE ;
      end
    end ;

  end ;

  procedure TGIS_DbSqlite.sqlQueryStructure(
    const _tableName     : String ;
    const _uidName       : String ;
    const _addFieldEvent : TGIS_LayerAddFieldEvent
   ) ;
  var
    i : Integer ;
  begin
    {$IFDEF LEVEL_XE2_RTL}
      assert( cursorDB[0].fStmt <> tsqlite3(0), 'Query not opened' ) ;
    {$ELSE}
      assert( cursorDB[0].fStmt <> nil,       'Query not opened' ) ;
    {$ENDIF}

    for i := 0 to cursorDB[0].fStmtCN.Count - 1 do begin
      if not assigned( _addFieldEvent ) then continue ;

      case cursorDB[0].fStmtCT[i].dType of
        SQLITE_DATA_INTEGER :
          _addFieldEvent( _uidName, cursorDB[0].fStmtCN[i],
                          TGIS_FieldType.Number,
                          cursorDB[0].fStmtCT[i].dSize,
                          cursorDB[0].fStmtCT[i].dPrec
                        ) ;
        SQLITE_DATA_FLOAT   :
          _addFieldEvent( _uidName, cursorDB[0].fStmtCN[i],
                          TGIS_FieldType.Float,
                          cursorDB[0].fStmtCT[i].dSize,
                          cursorDB[0].fStmtCT[i].dPrec
                        ) ;
        SQLITE_DATA_TEXT    :
          _addFieldEvent( _uidName, cursorDB[0].fStmtCN[i],
                          TGIS_FieldType.String,
                          cursorDB[0].fStmtCT[i].dSize,
                          cursorDB[0].fStmtCT[i].dPrec
                        ) ;
        SQLITE_DATA_DATE    :
          _addFieldEvent( _uidName, cursorDB[0].fStmtCN[i],
                          TGIS_FieldType.Date,
                          cursorDB[0].fStmtCT[i].dSize,
                          cursorDB[0].fStmtCT[i].dPrec
                        ) ;
        SQLITE_DATA_BOOL    :
          _addFieldEvent( _uidName, cursorDB[0].fStmtCN[i],
                          TGIS_FieldType.Boolean,
                          cursorDB[0].fStmtCT[i].dSize,
                          cursorDB[0].fStmtCT[i].dPrec
                        ) ;
      end ;
    end ;
  end ;

  function TGIS_DbSqlite.sqlQueryNameGEOUID(
    const _field   : String ;
    const _fieldEx : String ;
    const _cursor  : Integer
   ) : Integer ;
  var
    idx : Integer ;
  begin
    idx := cursorDB[_cursor].fStmtCN.IndexOf( _field ) ;
    if idx < 0 then
      Result := 1
    else
      Result := 0 ;
  end ;

  function TGIS_DbSqlite.sqlQueryGetGEOUID(
    const _field   : String ;
    const _cursor  : Integer
   ) : Variant ;
  begin
    if cursorDB[_cursor].fColUid < 0 then
      cursorDB[_cursor].fColUid := cursorDB[_cursor].fStmtCN.IndexOf( _field ) ;

    Result := sqlite3_column_int64( cursorDB[_cursor].fStmt,
                                     cursorDB[_cursor].fColUid
                                   ) ;
  end ;

  function TGIS_DbSqlite.sqlQueryGetGEOUID(
    const _index   : Integer ;
    const _cursor  : Integer
   ) : Variant ;
  begin
    Result := sqlite3_column_int64( cursorDB[_cursor].fStmt, _index ) ;
  end ;

  function TGIS_DbSqlite.sqlQueryGetSHAPETYPE(
    const _field  : String ;
    const _cursor : Integer
   ) : Variant ;
  begin
    if cursorDB[_cursor].fColShpType < 0 then
      cursorDB[_cursor].fColShpType := cursorDB[_cursor].fStmtCN.IndexOf( _field ) ;

    Result := sqlite3_column_int64( cursorDB[_cursor].fStmt,
                                     cursorDB[_cursor].fColShpType
                                   ) ;
  end ;

  function TGIS_DbSqlite.sqlQueryGetXMIN(
    const _field   : String ;
    const _cursor  : Integer
   ) : Variant ;
  begin
    if cursorDB[_cursor].fColXmin < 0 then
      cursorDB[_cursor].fColXmin := cursorDB[_cursor].fStmtCN.IndexOf( _field ) ;

    Result := sqlite3_column_double( cursorDB[_cursor].fStmt,
                                      cursorDB[_cursor].fColXmin
                                    ) ;
  end ;

  function TGIS_DbSqlite.sqlQueryGetYMIN(
    const _field   : String ;
    const _cursor  : Integer
   ) : Variant ;
  begin
    if cursorDB[_cursor].fColYmin < 0 then
      cursorDB[_cursor].fColYmin := cursorDB[_cursor].fStmtCN.IndexOf( _field ) ;

    Result := sqlite3_column_double( cursorDB[_cursor].fStmt,
                                      cursorDB[_cursor].fColYmin
                                    ) ;
  end ;

  function TGIS_DbSqlite.sqlQueryGetGeomVAR(
    const _field       : String ;
    const _fieldEx     : String ;
    const _cursor      : Integer;
    const _forceValue  : Boolean = False
   ) : OleVariant ;
  var
    size  : Integer ;
    ptvar : tsqlite3 ;
  begin
    if cursorDB[_cursor].fColGeom < 0 then begin
      if IsStringEmpty( _fieldEx ) then
        cursorDB[_cursor].fColGeom := cursorDB[_cursor].fStmtCN.IndexOf( _field )
      else
        cursorDB[_cursor].fColGeom := cursorDB[_cursor].fStmtCN.IndexOf( _fieldEx ) ;
    end ;

    size  := sqlite3_column_bytes( cursorDB[_cursor].fStmt,
                                    cursorDB[_cursor].fColGeom
                                   ) ;
    ptvar := sqlite3_column_blob( cursorDB[_cursor].fStmt,
                                   cursorDB[_cursor].fColGeom
                                  ) ;
    if size > 0 then begin
      Result := VarArrayCreate( [0,size+1], varByte ) ;
      {$IFNDEF CLR}
        if assigned( ptvar ) then
      {$ELSE}
        if (ptvar <> IntPtr.Zero) then
      {$ENDIF}
          VarFromPtr( Result, IntPtr(ptvar), size+1 ) ;
    end
    else
      Result := NullVar ;
  end ;

  {$IFDEF MANAGED}
    function TGIS_DbSqlite.sqlQueryGetGeomPtr(
      const _field    : String ;
      const _fieldEx  : String ;
      const _cursor   : Integer;
        var _size     : Integer
     ) : TGIS_Bytes ;
    var
      ptr : tsqlite3 ;
      buf : TBytes ;
      size: Integer ;
    begin
      if cursorDB[_cursor].fColGeom < 0 then begin
        if IsStringEmpty( _fieldEx ) then
          cursorDB[_cursor].fColGeom := cursorDB[_cursor].fStmtCN.IndexOf( _field )
        else
          cursorDB[_cursor].fColGeom := cursorDB[_cursor].fStmtCN.IndexOf( _fieldEx ) ;
      end ;

      ptr := sqlite3_column_blob( cursorDB[_cursor].fStmt,
                                   cursorDB[_cursor].fColGeom
                                  ) ;
      size := sqlite3_column_bytes( cursorDB[_cursor].fStmt,
                                     cursorDB[_cursor].fColGeom
                                    ) ;
      SetLength( buf, size ) ;
      {$IFNDEF CLR}
        if (size > 0) and assigned( ptr ) then
      {$ELSE}
        if (size > 0) and (ptr <> IntPtr.Zero) then
      {$ENDIF}
      begin
          Marshal.Copy( ptr, buf, 0, size ) ;
          Result := TGIS_Bytes.Create( buf, 0 ) ;
          _size  := size ;
       end
       else
         Result := nil ;
    end ;

    function TGIS_DbSqlite.sqlQueryGetGeomPtr(
      const _field    : String ;
      const _cursor   : Integer;
        var _size     : Integer
     ) : TBytes ;
    var
      ptr  : tsqlite3 ;
      size : Integer ;
    begin
      if cursorDB[_cursor].fColGeom < 0 then
        cursorDB[_cursor].fColGeom := cursorDB[_cursor].fStmtCN.IndexOf( _field ) ;

      ptr := sqlite3_column_blob( cursorDB[_cursor].fStmt,
                                   cursorDB[_cursor].fColGeom
                                  ) ;
      size := sqlite3_column_bytes( cursorDB[_cursor].fStmt,
                                     cursorDB[_cursor].fColGeom
                                    ) ;

      SetLength( Result, size ) ;
      {$IFNDEF CLR}
        if (size > 0) and assigned( ptr ) then
      {$ELSE}
        if (size > 0) and (ptr <> IntPtr.Zero) then
      {$ENDIF}
      begin
        Marshal.Copy( ptr, Result, 0, size ) ;
      end ;
      _size  := size ;
    end ;

  {$ELSE}

    function TGIS_DbSqlite.sqlQueryGetGeomPtr(
      const _field    : String ;
      const _fieldEx  : String ;
      const _cursor   : Integer ;
        var _size     : Integer
    ) : Pointer ;
    begin
      if cursorDB[_cursor].fColGeom < 0 then begin
        if IsStringEmpty( _fieldEx ) then
          cursorDB[_cursor].fColGeom := cursorDB[_cursor].fStmtCN.IndexOf( _field )
        else
          cursorDB[_cursor].fColGeom := cursorDB[_cursor].fStmtCN.IndexOf( _fieldEx ) ;
      end ;
      {$IFDEF LEVEL_XE2_RTL}
        Result := Pointer( sqlite3_column_blob( cursorDB[_cursor].fStmt,
                                                 cursorDB[_cursor].fColGeom
                                               ) ) ;
      {$ELSE}
        Result := sqlite3_column_blob( cursorDB[_cursor].fStmt,
                                        cursorDB[_cursor].fColGeom
                                       ) ;
      {$ENDIF}
      _size := sqlite3_column_bytes( cursorDB[_cursor].fStmt,
                                      cursorDB[_cursor].fColGeom
                                     ) ;
    end ;
  {$ENDIF}

  procedure TGIS_DbSqlite.sqlQueryUnPrepareGetGeom(
    const _cursor        : Integer
  ) ;
  begin
    // for safe inheritance
  end ;

  function TGIS_DbSqlite.sqlQueryGetBlob(
    const _name    : String  ;
    const _cursor  : Integer
  ) : TStream ;
  var
    size  : Integer ;
    ptvar : tsqlite3 ;
    id    : Integer ;
    {$IFDEF CLR}
      buf : TBytes ;
    {$ENDIF}
  begin
    id     := cursorDB[_cursor].fStmtCN.IndexOf( _name ) ;
    size   := sqlite3_column_bytes( cursorDB[_cursor].fStmt, id ) ;
    ptvar  := sqlite3_column_blob( cursorDB[_cursor].fStmt, id ) ;

    Result := TGIS_MemoryStream.Create ;
    {$IFDEF CLR}
      SetLength( buf, size ) ;
      Marshal.Copy( ptvar, buf, 0, size ) ;
      Result.WriteBuffer( buf, size ) ;
    {$ELSE}
      {$IFDEF LEVEL_XE2_RTL}
        Result.WriteBuffer( Pointer(ptvar)^, size ) ;
      {$ELSE}
        Result.WriteBuffer( ptvar^,          size ) ;
      {$ENDIF}
    {$ENDIF}
    Result.Position := 0 ;
  end ;

  function TGIS_DbSqlite.sqlQueryGetGeomObj(
    const _field   : String ;
    const _cursor  : Integer
  ) : TObject ;
  begin
    Result := nil ;
  end ;

  procedure TGIS_DbSqlite.sqlTableSetGeometry(
    const _id    : Integer ;
    const _name  : String ;
    const _data  : OleVariant ;
    const _blob  : TGIS_MemoryStream
   ) ;
  var
    idx,
    cnt : Integer ;
    {$IFDEF CLR}
      buf : TBytes ;
    {$ENDIF}
    {$IFDEF CLR}
      cmd : TBytes ;
    {$ELSE}
    cmd : tsqlite3 ;
    ptr : tsqlite3 ;
    {$ENDIF}
  begin
    {$IFNDEF OXYGENE}
      cmd := MarshaledAString(UTF8Encode(':' + _name) ) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( ':' + _name ) ;
    {$ENDIF}
    idx := sqlite3_bind_parameter_index( FStmts[_id],
                                      cmd
                                     ) ;
    if not assigned( _blob ) then begin
      if VarIsEmpty( _data ) or VarIsNull( _data ) then
        check( sqlite3_bind_null( FStmts[_id], idx ), '' )
      else begin
        cnt := VarArrayHighBound( _data, 1 )+1 ;
        {$IFDEF CLR}
          buf := TBytes( TObject( _data ) ) ;
          check( sqlite3_bind_blob( FStmts[_id], idx, buf, cnt, SQLITE_TRANSIENT ),'' ) ;
        {$ELSE}
          {$IFDEF LEVEL_XE2_RTL}
            ptr := tsqlite3(VarArrayLock( _data )) ;
          {$ELSE}
            ptr := VarArrayLock( _data ) ;
          {$ENDIF}
          try
            check( sqlite3_bind_blob( FStmts[_id], idx, ptr, cnt, SQLITE_TRANSIENT ),'' ) ;
          finally
            VarArrayUnlock( _data ) ;
          end ;
        {$ENDIF}
      end ;
    end
    else begin
      {$IFDEF CLR}
        check( sqlite3_bind_blob( FStmts[_id], idx, _blob.Memory, _blob.Size, SQLITE_TRANSIENT ),'' ) ;
      {$ELSE}
        {$IFDEF LEVEL_XE2_RTL}
          ptr := tsqlite3(_blob.Memory) ;
        {$ELSE}
          ptr := _blob.Memory ;
        {$ENDIF}
        check( sqlite3_bind_blob( FStmts[_id], idx, ptr, _blob.Size, SQLITE_TRANSIENT ),'' ) ;
      {$ENDIF}
    end ;
  end ;

  procedure TGIS_DbSqlite.sqlTableSetBlob(
    const _id    : Integer ;
    const _name  : String ;
    const _data  : OleVariant ;
    const _blob  : TGIS_MemoryStream
  ) ;
  begin
    sqlTableSetGeometry( _id, _name, _data, _blob ) ;
  end ;

  procedure TGIS_DbSqlite.sqlTableSetGeometry(
    const _id    : Integer ;
    const _name  : String ;
    const _data  : TObject
  ) ;
  begin
    // for safe inheritance
  end ;

  procedure TGIS_DbSqlite.sqlUpdateStart(
    const _id     : Integer ;
    const _table  : String
  ) ;
  begin
    // for safe inheritance
  end ;

  function TGIS_DbSqlite.sqlQueryGeometryIsText(
    const _cursor : Integer
  ) : Boolean ;
  begin
    Result := False ;
  end ;

  procedure TGIS_DbSqlite.sqlBuild(
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
    {$IFDEF CLR}
      cmd : TBytes ;
    {$ELSE}
      cmd : tsqlite3 ;
    {$ENDIF}
  begin
    if IsEmbeddedSQLPath( _path ) then exit ;

    ttklspath := GetPathAbsolute( '', _path ) ;
    if SafeFileExists( ttklspath ) then exit ;

    if IsStringEmpty( ttklspath ) then exit ;

    layername := _layerName ;
    if IsStringEmpty( layername ) then
      layername := GetFileName( GetPathNoExt( _path ) ) ;

    canonicalname := TGIS_Utils.GisCanonicalSQLName( layername ) ;
    provider := GetFilePath( ttklspath ) +
                GIS_INI_LAYERSQL_DEFAULT_DATABASE_SQLITE ;

    try
      {$IFDEF LEVEL_XE2_RTL}
        FDB := tsqlite3(0) ;
      {$ELSE}
        FDB := nil ;
      {$ENDIF}
      InitializeProvider ;

      {$IFNDEF OXYGENE}
        cmd := MarshaledAString(UTF8Encode(provider) ) ;
      {$ELSE}
        cmd := TEncoding.UTF8.GetBytes( provider ) ;
      {$ENDIF}

      check( sqlite3_open_v2( cmd,
                            FDB,
                            SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE,
                            {$IFDEF LEVEL_XE2_RTL} tsqlite3(0) {$ELSE} nil {$ENDIF}
                           ),
             Format( 'failed to create database %s', [ provider ] )
            ) ;
    finally
      {$IFDEF LEVEL_XE2_RTL}
        if FDB <> tsqlite3(0)   then begin
      {$ELSE}
        if AssignedPtr( FDB ) then begin
      {$ENDIF}
        check( sqlite3_close( FDB ), 'failed to close database' ) ;
        {$IFDEF LEVEL_XE2_RTL}
          FDB := tsqlite3(0) ;
        {$ELSE}
          FDB := nil ;
        {$ENDIF}
      end ;
    end ;

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
                       GIS_SQL_DIALECT_NAME_SQLITE,
                       ''
                     ) ;
      cfg.WriteString( GIS_INI_LAYERSQL_CONNECTOR_SQLITE,
                       provider,
                       ''
                     ) ;
      cfg.WriteString( GIS_SQL_PARAMS_ENGINEOPTIONS,
                       '16',
                       ''
                     ) ;
    finally
      cfg.Save ;
      FreeObject( cfg ) ;
    end ;
  end ;

  procedure TGIS_DbSqlite.cursorOpen(
    const _cursor : Integer
  ) ;
  begin
    if _cursor >= length( cursorDB )  then
      SetLength( cursorDB, _cursor + 1 ) ;
    cursorDB[ _cursor ].curInUse := True ;
    {$IFDEF LEVEL_XE2_RTL}
      cursorDB[ _cursor ].fStmt  := tsqlite3(0) ;
    {$ELSE}
      cursorDB[ _cursor ].fStmt  := nil ;
    {$ENDIF}
    cursorDB[ _cursor ].fStmtCN  := TStringList.Create ;
  end ;

  procedure TGIS_DbSqlite.cursorClose(
    const _cursor : Integer
  ) ;
  var
    i : Integer ;
  begin
    sqlQueryClose( _cursor ) ;

    cursorDB[_cursor].curInUse := False ;
    FreeObject( cursorDB[_cursor].fStmtCN ) ;

    // truncate cursorState at the tail;
    for i := length( cursorDB ) - 1 downto 0 do begin
      if not cursorDB[i].curInUse then begin
        SetLength( cursorDB, i ) ;
      end
      else
        break ;
    end ;
  end ;

  function TGIS_DbSqlite.getLastCursor : Integer ;
  begin
    Result := length( cursorDB ) - 1 ;
  end ;

  {$IFDEF CLR}
    procedure TGIS_SqliteFunction.DbCreateFunction(
  {$ELSE}
    procedure DbCreateFunction(
  {$ENDIF}
    _ctx    : tsqlite3 ;
    _nArgs  : Integer ;
    _args   : tsqlite3
  ) ;
  {$IFDEF DEF_CDECL}
    cdecl;
  {$ENDIF}
  var
    val   : Variant ;
    dtype : Integer ;
    size  : Integer ;
    ptvar : tsqlite3 ;
    valptr : tsqlite3 ;
    dts   : String ;
    dt    : TDateTime ;
    v     : Variant ;
    aargs : array of Variant ;
    i     : Integer ;
    {$IFDEF CLR}
    cmd : TBytes ;
    {$ELSE}
    cmd : tsqlite3 ;
    {$ENDIF}
  begin
    SetLength( aargs, _nArgs ) ;
    for i := 0 to _nArgs-1 do begin
      {$IFDEF OXYGENE}
        valptr := Marshal.ReadIntPtr( _args, i*sizeOf(tsqlite3) ) ;
      {$ELSE}
        valptr := tsqlite3(tsqlite3(NativeInt(_args) + i*sizeof(tsqlite3) )^ ) ;
      {$ENDIF}
      dtype := sqlite3_value_type( valptr ) ;

      case dtype of
        SQLITE_DATA_INTEGER :
          val := sqlite3_value_int64( valptr ) ;
        SQLITE_DATA_FLOAT   :
          val := sqlite3_value_double( valptr ) ;
        SQLITE_DATA_TEXT    :
          val := convertString( sqlite3_value_text( valptr ) ) ;
        SQLITE_DATA_BLOB    :
          begin
            size := sqlite3_value_bytes( valptr ) ;
            ptvar := sqlite3_value_blob(  valptr ) ;
            if (size > 0) and ( ptvar <> nil ) then begin
              val := VarArrayCreate( [0,size+1], varByte ) ;
              VarFromPtr( val, IntPtr(ptvar), size+1 ) ;
            end
            else
              val := Unassigned ;
          end ;
        SQLITE_DATA_NULL    :
          val := varNull ;
        SQLITE_DATA_DATE    :
          begin
            dts := convertString( sqlite3_value_text( valptr ) ) ;
            if IsStringEmpty(dts) then
              val := Unassigned
            else begin
              {$IFDEF CLR}
                if DateTime.TryParse( dts, dt ) then
              {$ELSE}
                if TryStrToDateTime( dts, dt ) then
              {$ENDIF}
                  val := dt  // date as text
                else begin
                  v := dts ;    // date as double
                  val := VarAsType( v, varDate ) ;
                end ;
            end
          end ;
        SQLITE_DATA_BOOL    :
          begin
            dts := convertString( sqlite3_value_text( valptr ) ) ;
            val := VarToBoolean(dts) ;
          end;
      end;
      aargs[i] := val ;
    end ;

    {$IFDEF OXYGENE}
      v := InvokeFnc( _nArgs, aargs ) ;
    {$ELSE}
      v := TGIS_SqliteFunction(Gsqlite3_user_data(_ctx)).InvokeFnc( _nArgs, aargs ) ;
    {$ENDIF}

    case GetVariantType( v ) of
      TGIS_VariantType.AnsiString,
      TGIS_VariantType.WideString  :
        begin
          {$IFNDEF OXYGENE}
            cmd := MarshaledAString(UTF8Encode(VarToString(v)) ) ;
          {$ELSE}
            cmd := TEncoding.UTF8.GetBytes( VarToString(v) ) ;
          {$ENDIF}
          sqlite3_result_text( _ctx, cmd, -1, SQLITE_TRANSIENT ) ;
        end ;
      TGIS_VariantType.Boolean     :
        if VarToBoolean( v ) then
          sqlite3_result_int( _ctx, 1 )
        else
          sqlite3_result_int( _ctx, 0 ) ;
      TGIS_VariantType.Int,
      TGIS_VariantType.UInt        :
        sqlite3_result_int( _ctx, VarToInt32(v) ) ;
      TGIS_VariantType.Int64,
      TGIS_VariantType.UInt64      :
        sqlite3_result_int64( _ctx, VarToInt64(v) ) ;
      TGIS_VariantType.Float       :
        sqlite3_result_double( _ctx, VarToDouble(v) ) ;
      TGIS_VariantType.DateTime    :
        begin
          {$IFNDEF OXYGENE}
            cmd := MarshaledAString(UTF8Encode(VarToString(v)) ) ;
          {$ELSE}
            cmd := TEncoding.UTF8.GetBytes( VarToString(v) ) ;
          {$ENDIF}
          sqlite3_result_text( _ctx, cmd, -1, SQLITE_TRANSIENT ) ;
        end ;
      TGIS_VariantType.Nothing     :
        sqlite3_result_null( _ctx ) ;
    end ;
  end;

  procedure TGIS_DbSqlite.sqlCreateFunction(
    const _name  : String ;
    const _nArgs : Integer ;
    const _fnc   : TGIS_DbFunction
  ) ;
  var
    ofnc : TGIS_SqliteFunction ;
    {$IFDEF CLR}
    cmd : TBytes ;
    {$ELSE}
    cmd : tsqlite3 ;
    {$ENDIF}
  begin
    ofnc := TGIS_SqliteFunction.Create ;
    ofnc.InvokeFnc   := _fnc ;
    ofnc.NArg        := _nArgs ;
    {$IFDEF CLR}
    ofnc.CallbackFnc := new Tsqlite3_func_callback( ofnc.DbCreateFunction ) ;
    {$ELSE}
    ofnc.CallbackFnc := DbCreateFunction ;
    {$ENDIF}
    FFunctions.Add( ofnc ) ;

    {$IFNDEF OXYGENE}
      cmd := MarshaledAString(UTF8Encode(_name) ) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( _name ) ;
    {$ENDIF}


    {$IFDEF OXYGENE}
      check( sqlite3_create_function(
               FDB, cmd, _nArgs, 1,  nil, ofnc.CallbackFnc, nil, nil
             ),
             'Failed to create a function ' + _name
            ) ;
    {$ELSE}
      check( sqlite3_create_function(
               FDB, cmd, _nArgs, 1, tsqlite3(ofnc), ofnc.CallbackFnc, nil, nil
             ),
             'Failed to create a function ' + _name
            ) ;
    {$ENDIF}
  end ;

  function TGIS_DbSqlite.sqlLastInsertId : TGIS_Uid ;
  begin
    Result := sqlite3_last_insert_rowid( FDB ) ;
  end ;


//-----------------------------------------------------------------------------
// TSqliteConnection
//-----------------------------------------------------------------------------

  constructor TSqliteConnection.Create ;
  begin
    inherited ;

    {$IFDEF LEVEL_XE2_RTL}
      FDB := tsqlite3(0) ;
    {$ELSE}
      FDB := nil ;
    {$ENDIF}
  end ;

  procedure TSqliteConnection.doDestroy ;
  begin
    Close ;
    {$IFDEF LEVEL_XE2_RTL}
      FDB := tsqlite3(0) ;
    {$ELSE}
      FDB := nil ;
    {$ENDIF}

    inherited ;
  end ;

  procedure TSqliteConnection.check(
    const _code : Integer ;
    const _msg  : String
  ) ;
  var
    err : String ;
    msg : tsqlite3 ;
  begin
    if _code = SQLITE_OK then exit ;

    {$IFDEF LEVEL_XE2_RTL}
      if FDB <> tsqlite3(0)   then begin
    {$ELSE}
      if AssignedPtr( FDB ) then begin
    {$ENDIF}
      msg := sqlite3_errmsg( FDB ) ;
      err := convertString( msg ) ;
    end ;

    if IsStringEmpty( err ) then
      err := _msg ;

    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERSQLERROR ), err, 0 ) ;
  end ;

  procedure TSqliteConnection.Open ;
  var
    isReadOnly : Boolean ;
    flags      : Integer ;
    {$IFDEF CLR}
    cmd : TBytes ;
    {$ELSE}
    cmd : tsqlite3 ;
    {$ENDIF}
  begin
    isReadOnly := CompareText( Params.Values[ GIS_INI_LAYERSQL_READONLY ],
                               GIS_INI_PARAM_BOOLEAN_TRUE
                              ) = 0 ;
    if SafeFileExists( Server ) then
      if isReadOnly then
        flags := SQLITE_OPEN_READONLY or SQLITE_OPEN_FULLMUTEX
      else
        flags := SQLITE_OPEN_READWRITE or SQLITE_OPEN_FULLMUTEX
    else
      flags := SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE ;

    {$IFNDEF OXYGENE}
      cmd := MarshaledAString(UTF8Encode(Server) ) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( Server ) ;
    {$ENDIF}

    check( sqlite3_open_v2( cmd, FDB, flags,
                          {$IFDEF LEVEL_XE2_RTL} tsqlite3(0) {$ELSE} nil {$ENDIF} ),
           Format( 'failed to open database %s', [ Server ] )
         ) ;
  end ;

  procedure TSqliteConnection.Close ;
  begin
    {$IFDEF LEVEL_XE2_RTL}
      if FDB <> tsqlite3(0)   then begin
    {$ELSE}
      if AssignedPtr( FDB ) then begin
    {$ENDIF}
      check( sqlite3_close( FDB ), 'failed to close database' ) ;
      {$IFDEF LEVEL_XE2_RTL}
        FDB := tsqlite3(0) ;
      {$ELSE}
        FDB := nil ;
      {$ENDIF}
    end ;
  end ;

{==================================== END =====================================}
end.
