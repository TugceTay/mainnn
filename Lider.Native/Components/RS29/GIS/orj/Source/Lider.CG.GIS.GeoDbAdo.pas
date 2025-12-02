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
  Encapsulation of a SQL file access.

  Currently support: MS Jet (Access), MS SQL Server, Interbase, Firebird,
  MySQL, DB2, Sybase, PostgreSql, BlackFishSql, Informix, Oracle, Advantage,
  Sqlite and SapDB dialects.
}

{$IFDEF DCC}
  unit GisDbAdo ;
  {$HPPEMIT '#pragma link "GisDbAdo"'}
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

{$IFDEF DCC}
  uses
    System.Classes,
    System.SysUtils,
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    System.Variants,
    {$IFDEF ADOINTUNIT}
      AdoInt,
    {$ELSE}
      GisAdoInt,
    {$ENDIF}

    GisRtl,
    GisTypes,
    GisStreams,
    GisDb ;
{$ENDIF}
{$IFDEF CLR}
  uses
    System.Reflection,
    TatukGIS.RTL,
    ADODB ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {$IFDEF OXYGENE}
    T_cursorDB nested in TGIS_DbAdo = public record
      {$IFNDEF GENDOC}
      type
        {#gendoc:hide}
        T_bindedFields = public record
          oField : Field  ;
          sName  : String ;
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
        myQuery       : _Recordset ;

        /// <summary>
        ///   Binded fields.
        /// </summary>
        bindedFields  : array of T_bindedFields ;

        /// <summary>
        ///   Stored previous select query for optimization purposes.
        /// </summary>
        oldQuery      : WideString ;

        /// <summary>
        ///   Binded GEO.UID
        /// </summary>
        fldUID        : Field ;

        /// <summary>
        ///   Binded SHAPETYPE
        /// </summary>
        fldSHAPETYPE  : Field ;

        /// <summary>
        ///   Binded XMIN
        /// </summary>
        fldXMIN       : Field ;

        /// <summary>
        ///   Binded YMIN
        /// </summary>
        fldYMIN       : Field ;

        /// <summary>
        ///   Binded GEOMETRY
        /// </summary>
        fldGEOMETRY   : Field ;

        /// <summary>
        ///   geometry buffer
        /// </summary>
        varStore      : OleVariant  ;

        /// <summary>
        ///   geometry size
        /// </summary>
        varSize       : Integer  ;

        /// <summary>
        ///   geometry pointer
        /// </summary>
        ptrvar        : TGIS_Bytes ;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Class that can read DB using ADO.
  /// </summary>
  TGIS_DbAdo = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_DbAbstract )
    private
      /// <summary>
      ///   ADO shared connection object.
      /// </summary>
      FSharedConnection : _Connection ;
    private

      /// <summary>
      ///   ADO connection object.
      /// </summary>
      myConnection  : _Connection ;
      {$IFDEF OXYGENE}
        cursorDB : array of T_cursorDB ;
      {$ELSE}
        cursorDB : array of record
          curInUse      : Boolean    ; // Is cursor in use. }
          myQuery       : _Recordset ; // Query object. }
          bindedFields  : Array of record // Binded fields. }
            oField : Field  ;
            sName  : String ;
          end ;
          oldQuery      : WideString ; // Stored previous select query for
                                       // optimization purposes.
          fldUID        : Field      ; // Binded GEO.UID }
          fldSHAPETYPE  : Field      ; // Binded SHAPETYPE }
          fldXMIN       : Field      ; // Binded XMIN }
          fldYMIN       : Field      ; // Binded YMIN }
          fldGEOMETRY   : Field      ; // Binded GEOMETRY }
          varStore      : OleVariant  ;
          varSize       : Integer  ;
        end;
      {$ENDIF}

        /// <summary>
        ///   Query object.
        /// </summary>
        myCommand     : _Command ;

        /// <summary>
        ///   Table object.
        /// </summary>
        myTable       : Array[0..2] of _Recordset ;

        /// <summary>
        ///   Params object.
        /// </summary>
        myParams      : Array[0..2] of TStringList ;

    private
    // for internal use of TGIS_Viewer

      /// <summary>
      ///   Makes string quoted for parametrized update situation.
      /// </summary>
      /// <param name="_txt">
      ///   string to be quoted
      /// </param>
      /// <param name="_size">
      ///   maximum size of string value; if -1 then no limits
      /// </param>
      function  safeString            ( const _txt           : String  ;
                                        {$IFNDEF OXYGENE}
                                          const _size        : Integer  = -1
                                        {$ELSE}
                                          const _size        : Integer := -1
                                        {$ENDIF}
                                      ) : String ; {$IFNDEF OXYGENE} overload; {$ENDIF}
    public
    // for internal use
      /// <inheritdoc/>
      procedure sqlConnect            ( const _folder        : String   ;
                                        const _sqlParameters : {$IFDEF OXYGENE}
                                                                 TGIS_Strings
                                                               {$ELSE}
                                                                 TStrings
                                                               {$ENDIF}
                                      ) ; override;

      /// <inheritdoc/>
      procedure sqlDisconnect         ; override;

      /// <inheritdoc/>
      procedure sqlTransactUpdateStart; override;

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
      procedure sqlTransactRollback      ; override;

      /// <inheritdoc/>
      procedure sqlQueryClose         ( const _cursor  : Integer ); override;

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
                                      ): Boolean ; override;

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
                                       const _name           : String ;
                                       const _val            : Variant ;
                                       const _defSize        : Integer
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
                                      ) : Variant ; override;

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
                                        {$IFNDEF OXYGENE}
                                          const _forceValue    : Boolean  = False
                                        {$ELSE}
                                          const _forceValue    : Boolean := False
                                        {$ENDIF}
                                      ) : OleVariant ; override;

      /// <inheritdoc/>
      function  sqlQueryGetBlob       ( const _name          : String ;
                                        const _cursor        : Integer
                                      ) : TStream ; override;
      {$IFDEF CLR}

      /// <inheritdoc/>
      function  sqlQueryGetGeomPtr    ( const _field         : String ;
                                        const _fieldEx       : String ;
                                        const _cursor        : Integer;
                                          var _size          : Integer
                                      ) : TGIS_Bytes ; override;

      /// <inheritdoc/>
      function  sqlQueryGetGeomPtr    ( const _field         : String ;
                                        const _cursor        : Integer;
                                          var _size          : Integer
                                      ) : TBytes ; override;
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
      procedure sqlBuild              ( const _path          : String ;
                                        const _extent        : TGIS_Extent;
                                        const _type          : TGIS_ShapeType ;
                                        const _storage       : String ;
                                        const _layerName     : String
                                      ) ; override;

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
      function sqlQueryGeometryIsText ( const _cursor        : Integer
                                      ) : Boolean ; override;

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

      {#gendoc:hide:GENPDK} { TODO -cPDK : Verify }
      /// <summary>
      ///   If assigned then external assigned connection will be used.
      /// </summary>
      property SharedConnection : _Connection    read  FSharedConnection
                                                 write FSharedConnection ;
    end;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisInterfaces,
    GisFunctions,
    GisInternals,
    GisResource,
    GisConfig,
    GisUtils,
    GisSharedConnections,
    GisClasses ;
{$ENDIF}

{$IFDEF OXYGENE}
//CursorLocationEnum
  const adUseServer = ADODB.CursorLocationEnum.adUseServer ;
  const adUseClient = ADODB.CursorLocationEnum.adUseClient ;

//CursorTypeEnum
  const adOpenStatic  = ADODB.CursorTypeEnum.adOpenStatic  ;
  const adOpenDynamic = ADODB.CursorTypeEnum.adOpenDynamic ;

//LockTypeEnum
  const adLockOptimistic = ADODB.LockTypeEnum.adLockOptimistic ;
  const adLockReadOnly   = ADODB.LockTypeEnum.adLockReadOnly   ;

//CommandTypeEnum
  const adCmdText = ADODB.CommandTypeEnum.adCmdText ;

//ParameterDirectionEnum
  const adParamInput = ADODB.ParameterDirectionEnum.adParamInput ;

{$ENDIF}

//=============================================================================
// TGIS_SqlAdo
//=============================================================================

  constructor TGIS_DbAdo.Create ;
  var
    i : Integer ;
  begin
    inherited ;

    myConnection := nil ;

    for i := low( myTable ) to high( myTable ) do begin
      {$IFDEF CLR}
        {$IFNDEF OXYGENE}
          myTable[ i ] := CoRecordset._Create ;
        {$ELSE}
          myTable[ i ] := new RecordsetClass ;
        {$ENDIF}
      {$ELSE}
        myTable[ i ] := CoRecordset.Create ;
      {$ENDIF}
    end ;

    for i := low( myParams ) to high( myParams ) do
      myParams[ i ] := TStringList.Create ;

    {$IFDEF CLR}
      {$IFNDEF OXYGENE}
        myCommand := CoCommand._Create ;
      {$ELSE}
        myCommand := new CommandClass ;
      {$ENDIF}
    {$ELSE}
      myCommand := CoCommand.Create ;
    {$ENDIF}

    for i := 0 to BUILTIN_CURSORS - 1 do
      cursorOpen( i );
  end ;

  procedure TGIS_DbAdo.doDestroy ;
  var
    i : Integer ;
  begin
    sqlDisconnect ;

    for i := low( myParams ) to high( myParams ) do
      FreeObject( myParams[ i ] ) ;

    for i := 0 to BUILTIN_CURSORS - 1 do
      cursorClose( i );

    inherited ;
  end ;

  function TGIS_DbAdo.sqlGetParams(
    const _idx : Integer
  ) : {$IFDEF OXYGENE}
        TGIS_StringList ;
      {$ELSE}
        TStringList ;
      {$ENDIF}
  begin
    if _idx in [0..2] then
      Result := myParams[ _idx ]
    else
      Result := nil;
  end;

  function TGIS_DbAdo.safeString(
    const _txt  : String  ;
    {$IFNDEF OXYGENE}
      const _size : Integer  = -1
    {$ELSE}
      const _size : Integer := -1
    {$ENDIF}
   ) : String ;
  begin
    if _size >= 0 then begin
      if UseTextParameters then begin
        {$IFDEF OXYGENE}
        Result := StringReplace( _txt, '''', '''''', [ TReplaceFlag.rfReplaceAll, TReplaceFlag.rfIgnoreCase] ) ;
        {$ELSE}
        Result := StringReplace( _txt, '''', '''''', [rfReplaceAll, rfIgnoreCase] ) ;
        {$ENDIF}
        Result := '''' + Copy( Result, StringFirst, length(Result) ) + '''' ;
      end
      else
        Result := Copy( _txt, StringFirst, _size ) ;
    end
    else begin
      if UseTextParameters then begin
        {$IFDEF OXYGENE}
        Result := StringReplace( _txt, '''', '''''', [TReplaceFlag.rfReplaceAll, TReplaceFlag.rfIgnoreCase] ) ;
        {$ELSE}
        Result := StringReplace( _txt, '''', '''''', [rfReplaceAll, rfIgnoreCase] ) ;
        {$ENDIF}
        Result := '''' + Result + '''' ;
      end
      else
        Result := _txt ;
    end ;
  end ;

  procedure TGIS_DbAdo.sqlConnect(
    const _folder        : String ;
    const _sqlParameters : {$IFDEF OXYGENE}
                             TGIS_Strings
                           {$ELSE}
                             TStrings
                           {$ENDIF}
  ) ;
  var
    cstr : String ;
  begin
    if not assigned( SharedConnection ) then begin
      sqlDisconnect  ;

      if GisEnvironmentInfo.Is64 then begin
        cstr := _sqlParameters.Values[ GIS_INI_LAYERSQL_CONNECTOR_ADO64 ] ;
        if IsStringEmpty( cstr ) then
          cstr := _sqlParameters.Values[ GIS_INI_LAYERSQL_CONNECTOR_ADO ] ;
      end
      else
        cstr := _sqlParameters.Values[ GIS_INI_LAYERSQL_CONNECTOR_ADO ] ;

      cstr := sqlPathAbsolute( _folder, cstr, ';', 'Data Source', '=' ) ;

      myConnection := SharedConnections.OpenADO( cstr, '', '', 0, _sqlParameters ) ;

      if options.ServerCursor then
        myConnection.CursorLocation := adUseServer
      else
        myConnection.CursorLocation := adUseClient
    end ;
  end ;

  procedure TGIS_DbAdo.sqlTableCreateParam(
    const _id       : Integer ;
    const _name     : String ;
    const _type     : TGIS_DataType ;
    const _subtype  : TGIS_SubDataType ;
    const _size     : Integer
  ) ;
  begin
    // for safe inheritance
  end ;

  procedure TGIS_DbAdo.sqlDisconnect ;
  var
    i : Integer ;
  begin
    for i := length( cursorDB ) - 1 downto 0 do
      sqlQueryClose( i ) ;

    for i:= low( myTable) to high( myTable ) do
      sqlTableClose( i ) ;

    if not assigned( SharedConnection ) then begin
      SharedConnections.CloseADO( myConnection ) ;
      myConnection := nil ;
    end ;
  end ;

  procedure TGIS_DbAdo.sqlTransactUpdateStart ;
  begin
    if options.UpdateTransact then
      if not assigned( SharedConnection ) then
        myConnection.BeginTrans ;
  end ;

  procedure TGIS_DbAdo.sqlTransactUpdateCommit ;
  begin
    if options.UpdateTransact then
      if not assigned( SharedConnection ) then
        myConnection.CommitTrans ;
  end ;

  procedure TGIS_DbAdo.sqlTransactRestructStart ;
  begin
    if options.RestructTransact then
      if not assigned( SharedConnection ) then
        myConnection.BeginTrans ;
  end ;

  procedure TGIS_DbAdo.sqlTransactRestructCommit ;
  begin
    if options.RestructTransact then
      if not assigned( SharedConnection ) then
        myConnection.CommitTrans ;
  end ;

  procedure TGIS_DbAdo.sqlTransactGlobalUpdateStart ;
  begin
    if options.GlobalUpdateTransact then
      if not assigned( SharedConnection ) then
        myConnection.BeginTrans ;
  end ;

  procedure TGIS_DbAdo.sqlTransactGlobalUpdateCommit ;
  begin
    if options.GlobalUpdateTransact then
      if not assigned( SharedConnection ) then
        myConnection.CommitTrans ;
  end ;

  procedure TGIS_DbAdo.sqlTransactRollback ;
  begin
    if not assigned( SharedConnection ) then
        myConnection.RollbackTrans ;
  end ;

  procedure TGIS_DbAdo.sqlQueryOpen(
    const _query   : String ;
    const _cursor  : Integer
   ) ;
  var
    tmp : WideString ;
    {$IFDEF GIS_DEBUG}
    t1  : Cardinal ;
    {$ENDIF}
  begin
    tmp := _query ;

    if cursorDB[_cursor].myQuery.State <> 0 then begin
      if FReuseQuery and (cursorDB[_cursor].oldQuery = tmp) then
        try
          if not cursorDB[_cursor].myQuery.BOF then
            cursorDB[_cursor].myQuery.MoveFirst
        except
          sqlQueryClose( _cursor ) ;
        end
      else
        sqlQueryClose( _cursor ) ;
    end ;

    if cursorDB[_cursor].myQuery.State = 0 then begin
      if assigned( FOnSQLExecute ) then
        FOnSQLExecute( tmp );

      {$IFDEF GIS_DEBUG}
      t1 := GetTickCount ;
      {$ENDIF}
      if not assigned( SharedConnection ) then
        cursorDB[_cursor].myQuery.Open(
          tmp, myConnection, adOpenStatic, adLockReadOnly, 0
        )
      else
        cursorDB[_cursor].myQuery.Open(
          tmp, SharedConnection, adOpenStatic, adLockReadOnly, 0
        ) ;

      {$IFDEF GIS_DEBUG}
      if assigned( FOnSQLExecute ) then
        FOnSQLExecute( ' Execute time : ' +FloatToStr( ( GetTickCount - t1 ) / 1000 ) + ' s' )
      {$ENDIF}
    end;

    cursorDB[_cursor].oldQuery := tmp ;

    cursorDB[_cursor].fldUID       := nil ;
    cursorDB[_cursor].fldSHAPETYPE := nil ;
    cursorDB[_cursor].fldXMIN      := nil ;
    cursorDB[_cursor].fldYMIN      := nil ;
    cursorDB[_cursor].fldGEOMETRY  := nil ;
  end ;

  procedure TGIS_DbAdo.sqlQueryReset(
    const _cursor  : Integer
  ) ;
  begin
    cursorDB[_cursor].oldQuery := '' ;
  end ;

  procedure TGIS_DbAdo.sqlQueryClose(
    const _cursor  : Integer
  ) ;
  begin
    if assigned( cursorDB[_cursor].myQuery ) then begin
      if cursorDB[_cursor].myQuery.State <> 0 then
        cursorDB[_cursor].myQuery.Close ;
    end ;

    {$IFDEF CLR}
      {$IFNDEF OXYGENE}
        cursorDB[_cursor].myQuery := CoRecordset._Create ;
      {$ELSE}
        cursorDB[_cursor].myQuery := new RecordsetClass ;
      {$ENDIF}
    {$ELSE}
      cursorDB[_cursor].myQuery := CoRecordset.Create ;
    {$ENDIF}
  end;

  procedure TGIS_DbAdo.sqlTableClose(
    const _id : Integer
  ) ;
  begin
    if assigned( myTable[ _id ] ) then begin
      if myTable[ _id ].State <> 0 then begin
        if ( not myTable[ _id ].BOF ) and ( not myTable[ _id ].EOF ) then
          myTable[ _id ].CancelUpdate ;
        myTable[ _id ].Close ;
      end ;
    end ;

    {$IFDEF CLR}
      {$IFNDEF OXYGENE}
        myTable[ _id ] := CoRecordset._Create ;
      {$ELSE}
        myTable[ _id ] := new RecordsetClass ;
      {$ENDIF}
    {$ELSE}
      myTable[ _id ] := CoRecordset.Create ;
    {$ENDIF}
  end ;

  procedure TGIS_DbAdo.sqlTablePost(
    const _id : Integer
  ) ;
  begin
    assert( myTable[ _id ].State <> 0, 'Table was not opened' ) ;

    if assigned( FOnSQLExecute ) then
      FOnSQLExecute( ' Post table ' + IntToStr( _id ) ) ;

    {$IFNDEF CLR}
      myTable[ _id  ].Update( EmptyParam, EmptyParam ) ;
    {$ELSE}
      myTable[ _id  ].Update( Missing.Value, Missing.Value ) ;
    {$ENDIF}
  end ;

  procedure TGIS_DbAdo.sqlTableExec(
    const _id : Integer
  ) ;
  begin
    assert( myTable[ _id ].State <> 0, 'Table was not opened' ) ;

    if assigned( FOnSQLExecute ) then
      FOnSQLExecute( ' Post table ' + IntToStr( _id ) ) ;

    {$IFNDEF CLR}
      myTable[ _id  ].Update( EmptyParam, EmptyParam ) ;
    {$ELSE}
      myTable[ _id  ].Update( Missing.Value, Missing.Value ) ;
    {$ENDIF}
  end ;

  procedure TGIS_DbAdo.sqlTableOpenRead(
    const _id     : Integer ;
    const _query  : String
   ) ;
  begin
    assert( myTable[ _id ].State = 0, 'Table already opened' ) ;

    if assigned( FOnSQLExecute ) then
      FOnSQLExecute( _query );

    if assigned(SharedConnection) then
      myTable[ _id ].Open( _query, SharedConnection, adOpenStatic, adLockReadOnly, 0 )
    else
      myTable[ _id ].Open( _query, myConnection, adOpenStatic, adLockReadOnly, 0 );
  end ;

  procedure TGIS_DbAdo.sqlTableAppend(
    const _id       : Integer ;
    const _query    : String
   ) ;
  var
    {$IFNDEF OXYGENE}
      cursor_type : Integer ;
    {$ELSE}
      cursor_type : CursorTypeEnum ;
    {$ENDIF}
  begin
    if myTable[ _id ].State = 0 then begin
      if IsMySql then cursor_type := adOpenStatic
                 else cursor_type := adOpenDynamic ;

      if assigned( FOnSQLExecute ) then
        FOnSQLExecute( _query );

      if assigned(SharedConnection) then
        myTable[ _id ].Open( _query, SharedConnection,
                             cursor_type, adLockOptimistic, 0 )
      else
        myTable[ _id ].Open( _query, myConnection,
                             cursor_type, adLockOptimistic, 0 )
    end
    else begin
      if options.Requery then
        myTable[ _id ].Requery( -1 ) ;
    end ;
    {$IFNDEF OXYGENE}
      myTable[ _id ].AddNew( EmptyParam, EmptyParam ) ;
    {$ELSE}
      myTable[ _id ].AddNew( Missing.Value, Missing.Value ) ;
    {$ENDIF}
  end ;

  function TGIS_DbAdo.sqlTablePrepared(
    const _id  : Integer
  ) : Boolean ;
  begin
    Result := myTable[ _id ].State <> 0 ;
  end ;

  procedure TGIS_DbAdo.sqlTableOpenWrite(
    const _id      : Integer ;
    const _query   : String
  ) ;
  var
    {$IFNDEF OXYGENE}
      cursor_type : Integer ;
    {$ELSE}
      cursor_type : CursorTypeEnum ;
    {$ENDIF}
  begin
    sqlTableClose( _id ) ;

    if IsMySql then cursor_type := adOpenStatic
               else cursor_type := adOpenDynamic ;

    if assigned( FOnSQLExecute ) then
      FOnSQLExecute( _query );

    if assigned(SharedConnection) then
      myTable[ _id ].Open( _query, SharedConnection,
                           cursor_type, adLockOptimistic, 0 )
    else
      myTable[ _id ].Open( _query, myConnection,
                           cursor_type, adLockOptimistic, 0 );
  end ;

  procedure TGIS_DbAdo.sqlExec(
    const _command : String
  ) ;
  var
    {$IFDEF CLR}
      {$IFNDEF OXYGENE}
        res : Variants.OleVariant ;
      {$ELSE}
        res : OleVariant ;
      {$ENDIF}
    {$ELSE}
      res : System.OleVariant ;
    {$ENDIF}
  begin
    if IsStringEmpty( _command ) then exit ;

    if assigned( FOnSQLExecute ) then
      FOnSQLExecute( _command );

    if assigned(SharedConnection) then
      SharedConnection.Execute( _command, res, 0 )
    else
      myConnection.Execute( _command, res, 0 ) ;
  end ;

  procedure TGIS_DbAdo.sqlQueryMoveFirst(
    const _cursor : Integer
  ) ;
  begin
    assert( cursorDB[_cursor].myQuery.State <> 0, 'Query not opened' ) ;

    cursorDB[_cursor].myQuery.MoveFirst ;
  end ;

  procedure TGIS_DbAdo.sqlQueryMoveNext(
    const _cursor : Integer
  ) ;
  begin
    assert( cursorDB[_cursor].myQuery.State <> 0, 'Query not opened' ) ;

    cursorDB[_cursor].myQuery.MoveNext ;
  end ;

  function  TGIS_DbAdo.sqlQueryEof(
    const _cursor : Integer
  ) : Boolean ;
  begin
    Result := cursorDB[_cursor].myQuery.State = 0 ;
    if Result then exit ;

    Result := cursorDB[_cursor].myQuery.EOF ;
  end ;

  function TGIS_DbAdo.sqlTableEof(
    const _id : Integer
  ) : Boolean ;
  begin
    Result := not ( myTable[ _id ].State <> 0 ) ;
    if Result then exit ;

    Result := myTable[ _id ].EOF ;
    if Result then
      sqlTableClose( _id ) ;
  end ;

  function  TGIS_DbAdo.sqlQueryGetFieldIndex(
    const _name   : String ;
    const _cursor : Integer
   ) : Integer ;
  var
    i : Integer ;
  begin
    Result := -1 ;

    for i := 0 to cursorDB[_cursor].myQuery.Fields.Count - 1 do begin
      if CompareText( cursorDB[_cursor].myQuery.Fields[ i ].Name, _name ) = 0 then
      begin
        Result := i ;
        break ;
      end ;
    end ;
  end;

  function TGIS_DbAdo.sqlQueryGetField(
    const _name   : String ;
    const _cursor : Integer
   ) : Variant ;
  begin
    assert( cursorDB[_cursor].myQuery.State <> 0, 'Query not opened' ) ;
    assert( not sqlQueryEof( _cursor ), 'Query Eof reached' ) ;

    try
      Result := ConvertADOFieldCP( cursorDB[_cursor].myQuery.Fields.Item[ _name ],
                                   FiCodePage ) ;
    except
      Result := Unassigned ;
    end ;
  end ;

  function TGIS_DbAdo.sqlQueryGetFieldById(
    const _id     : Integer ;
    const _cursor : Integer
   ) : Variant ;
  begin
    assert( cursorDB[_cursor].myQuery.State <> 0, 'Query not opened' ) ;
    assert( not sqlQueryEof( _cursor ), 'Query Eof reached'   ) ;

    try
      Result := ConvertADOFieldCP( cursorDB[_cursor].myQuery.Fields.Item[ _id ],
                                   FiCodePage ) ;
    except
      Result := Unassigned ;
    end ;
  end ;

  function TGIS_DbAdo.sqlTableGetField(
    const _id   : Integer ;
    const _name : String
   ) : Variant ;
  var
    tmp : Field ;
  begin
    assert( myTable[ _id ].State <> 0, 'Table not opened' ) ;
    assert( not sqlTableEof( _id ), 'Table Eof reached'   ) ;

    tmp := myTable[ _id ].Fields.Item[ _name ] ;
    {$IFNDEF OXYGENE}
      if tmp.Type_ = adNumeric then begin
    {$ELSE}
      if tmp.Type  = adNumeric then begin
    {$ENDIF}
      if tmp.NumericScale = 0 then
        try
          Result := Integer( ConvertVar( tmp.Value ) )
        except
          Result := Double( ConvertVar( tmp.Value ) )
        end
      else
        Result := Double( ConvertVar( tmp.Value ) )
    end
    {$IFNDEF OXYGENE}
      else if tmp.Type_ = adInteger then begin
    {$ELSE}
      else if tmp.Type  = adInteger then begin
    {$ENDIF}
      Result := ConvertVar( tmp.Value ) ;
    end
    else
      Result := ConvertVar( tmp.Value ) ;
  end ;

  function TGIS_DbAdo.sqlTableGetParam(
    const _id   : Integer ;
    const _name : String
   ) : Variant ;
  begin
    Result := Unassigned ;
  end;

  function TGIS_DbAdo.sqlBindField(
    const _field  : String ;
    const _cursor : Integer
   ) : Integer ;
  var
    i   : Integer ;
    fld : Field   ;
  begin
    assert( cursorDB[_cursor].myQuery.State <> 0, 'Query not opened' ) ;

    Result := -1 ;

    for i := 0 to cursorDB[_cursor].myQuery.Fields.Count -1 do
      if CompareText( cursorDB[_cursor].myQuery.Fields[ i ].Name, _field ) = 0 then begin
        Result := i ;
        break ;
      end ;

    if Result >= 0 then
      fld := cursorDB[_cursor].myQuery.Fields[ _field ]
    else
      fld := nil ;
    if assigned( fld ) then begin
      if high( cursorDB[_cursor].bindedFields ) < Result + 1 then
        SetLength( cursorDB[_cursor].bindedFields, Result + 1 ) ;
      cursorDB[_cursor].bindedFields[ Result ].oField := fld      ;
      cursorDB[_cursor].bindedFields[ Result ].sName  := fld.Name ;
    end ;
  end ;

  procedure TGIS_DbAdo.sqlTableSetField(
    const _id           : Integer ;
    const _name         : String ;
    const _val          : Variant ;
    const _defSize      : Integer
   ) ;
  var
    defsize : Integer ;
    y,m,d   : Word ;

    // MyODBC has a bug updating layer with more then 14 precision numbers
    function dbl14( _v : Double ) : Double ;
    begin
      if IsMySql then
        Result := DotStrToFloat( Format( '%.14g', [ _v ] ) )
      else
        Result := _v ;
    end ;

  begin
    if IsStringEmpty( _name ) then exit;

    if ( not UseTextParameters ) then begin
      assert( myTable[ _id ].State <> 0, 'Table not opened' ) ;
      assert( not sqlTableEof( _id ), 'Table Eof reached'   ) ;
    end ;

    if ( UseTextParameters ) then begin
      defsize := -1 ;
      if _id = 1 then begin
        defsize := _defSize ;
      end ;
    end
    else
      defsize := myTable[ _id ].Fields.Item[ _name ].DefinedSize ;

    case GetVariantType( _val ) of
      TGIS_VariantType.AnsiString  :
        begin
          if ( UseTextParameters ) then
            myParams[ _id ].Values[ _name ] :=
              {$IFDEF CLR}
                safeString( VarToString( _val ), defsize )
              {$ELSE}
                safeString( ConvertVar2WStrCP( _val, 65001 ), defsize )
              {$ENDIF}
          else begin
            {$IFDEF CLR}
              myTable[ _id ].Fields.Item[ _name ].Value :=
                                     ConvertVar( safeString( VarToString( _val ), defsize ) ) ;
            {$ELSE}
               myTable[ _id ].Fields.Item[ _name ].Value :=
                safeString( ConvertVar2WStrCP( safeString( _val, defsize ),
                                               65001 ), defsize
                          ) ;
            {$ENDIF}
          end ;
        end ;

      TGIS_VariantType.WideString  :
        begin
          if ( UseTextParameters ) then
            myParams[ _id ].Values[ _name ] :=
              safeString( VarToString( _val ), defsize )
          else
            myTable[ _id ].Fields.Item[ _name ].Value :=
              {$IFDEF CLR}
                ConvertVar( safeString( VarToString( _val ), defsize ) ) ;
              {$ELSE}
                safeString( _val, defsize ) ;
              {$ENDIF}
        end ;
      TGIS_VariantType.Nothing :
        try
          if ( UseTextParameters ) then
            myParams[ _id ].Values[ _name ] := 'null'
          else begin
            {$IFDEF CLR}
              myTable[ _id ].Fields.Item[ _name ].Value := ConvertVar( NullVar ) ;
            {$ELSE}
              myTable[ _id ].Fields.Item[ _name ].Value := NullVar ;
            {$ENDIF}
          end ;
        except
          if ( UseTextParameters ) then
            myParams[ _id ].Values[ _name ] := safeString( '' )
          else begin
            {$IFDEF CLR}
              myTable[ _id ].Fields.Item[ _name ].Value :=
                                                ConvertVar( safeString( '' ) ) ;
            {$ELSE}
               myTable[ _id ].Fields.Item[ _name ].Value := safeString( '' ) ;
            {$ENDIF}
          end ;
        end ;

      TGIS_VariantType.Boolean :
        begin
          if not UseTextParameters and
            {$IFNDEF OXYGENE}
              ( myTable[ _id ].Fields.Item[ _name ].Type_ = adBoolean ) then begin
            {$ELSE}
              ( myTable[ _id ].Fields.Item[ _name ].Type  = adBoolean ) then begin
            {$ENDIF}
            if ( UseTextParameters ) then
              myParams[ _id ].Values[ _name ] :=
                                safeString( BoolToStr( VarToBoolean( _val ), True ) )
            else begin
              {$IFDEF CLR}
                myTable[ _id ].Fields.Item[ _name ].Value := ConvertVar( _val ) ;
              {$ELSE}
                myTable[ _id ].Fields.Item[ _name ].Value := _val ;
              {$ENDIF}
            end ;
          end
          else begin
            if VarToBoolean( _val ) then begin
              if ( UseTextParameters ) then
                myParams[ _id ].Values[ _name ] := safeString( 'Y' )
              else begin
                {$IFDEF CLR}
                  myTable[ _id ].Fields.Item[ _name ].Value :=
                                                ConvertVar( safeString( 'Y' ) )
                {$ELSE}
                  myTable[ _id ].Fields.Item[ _name ].Value := 'Y' ;
                {$ENDIF}
              end ;
            end
            else begin
              if ( UseTextParameters ) then
                myParams[ _id ].Values[ _name ] := safeString( 'N' )
              else begin
                {$IFDEF CLR}
                  myTable[ _id ].Fields.Item[ _name ].Value :=
                                              ConvertVar( safeString( 'N' ) ) ;
                {$ELSE}
                  myTable[ _id ].Fields.Item[ _name ].Value := 'N' ;
                {$ENDIF}
              end ;
            end ;
          end ;
        end ;

      TGIS_VariantType.Float :
        begin
          if ( UseTextParameters ) then
            myParams [ _id ].Values[ _name ] := DotFloatToStr( VarToDouble( _val ) )
          else begin
            {$IFDEF CLR}
              myTable[ _id ].Fields.Item[ _name ].Value :=
                                                  ConvertVar( dbl14( VarToDouble( _val ) ) ) ;
            {$ELSE}
              // strange but WINDOWS 2000 has errors
              myTable[ _id ].Fields.Item[ _name ].Value := Variant( _val ) ;
            {$ENDIF}
          end ;
        end ;
      TGIS_VariantType.DateTime :
        begin
          if ( UseTextParameters ) then begin
             DecodeDate( VarToDateTime( _val ), y, m, d ) ;
             myParams [ _id ].Values[ _name ] := safeString( FormatDateTime( 'yyyy-MM-dd',
                                                                 EncodeDate( y, m, d )
                                                                ) ) ;
          end
          else begin
            {$IFDEF CLR}
              myTable[ _id ].Fields.Item[ _name ].Value := ConvertVar( _val ) ;
            {$ELSE}
              // strange but WINDOWS 2000 has errors
              myTable[ _id ].Fields.Item[ _name ].Value := Variant( _val ) ;
            {$ENDIF}
          end ;
        end
      else
        begin
          if ( UseTextParameters ) then
            myParams [ _id ].Values[ _name ] := VarToString( _val )
          else begin
            {$IFDEF CLR}
              myTable[ _id ].Fields.Item[ _name ].Value := ConvertVar( _val ) ;
            {$ELSE}
              // strange but WINDOWS 2000 has errors
              myTable[ _id ].Fields.Item[ _name ].Value := Variant( _val ) ;
            {$ENDIF}
          end ;
        end ;
    end ;
  end ;

  function TGIS_DbAdo.sqlGetBindedField(
    const _field         : Integer ;
    const _rebindFields  : Boolean ;
    const _cursor        : Integer
   ) : Variant ;
  var
    fld : Field ;

    procedure rebind_fields ;
    var
      i  : Integer ;
      nm : String ;
    begin
      for i := low( cursorDB[_cursor].bindedFields ) to
              high( cursorDB[_cursor].bindedFields ) do begin
        nm := cursorDB[_cursor].bindedFields[i].sName ;
        if not IsStringEmpty( nm ) then
          cursorDB[_cursor].bindedFields[i].oField :=
                                    cursorDB[_cursor].myQuery.Fields.Item[ nm ];
      end ;
    end ;

    function hasRecords : Boolean ;
    begin
      Result := cursorDB[_cursor].myQuery.RecordCount > 0 ;
    end;

  begin
    Result := Unassigned ;

    if _rebindFields then
      rebind_fields ;

    assert( cursorDB[_cursor].myQuery.State <> 0, 'Query not opened' ) ;

    if ( _field < 0 ) or ( _field > high( cursorDB[_cursor].bindedFields ) ) then exit ;

    fld := cursorDB[_cursor].bindedFields[ _field ].oField ;

    if assigned( fld ) and hasRecords then
      Result := ConvertADOFieldCP( fld, FiCodePage )
    else
      Result := Unassigned ;
  end ;

  procedure TGIS_DbAdo.sqlQueryStructure(
    const _tableName     : String ;
    const _uidName       : String ;
    const _addFieldEvent : TGIS_LayerAddFieldEvent
   ) ;
  var
    i  : Integer ;
    fn : String  ;
  begin
    assert( cursorDB[0].myQuery.State <> 0, 'Query not opened'               ) ;
    assert( assigned( _addFieldEvent ), 'Layer add field event not assigned' ) ;

    for i := 0 to cursorDB[0].myQuery.Fields.Count - 1 do begin

      fn := cursorDB[0].myQuery.Fields.Item[ i ].Name ;
      {$IFNDEF OXYGENE}
        case cursorDB[0].myQuery.Fields.Item[ i ].Type_ of
      {$ELSE}
        case cursorDB[0].myQuery.Fields.Item[ i ].Type  of
      {$ENDIF}
        adBoolean          : _addFieldEvent( _uidName,fn,
                                             TGIS_FieldType.Boolean, 0,0 );
        adDate,
        adDBDate,
        adDBTime,
        adDBTimeStamp      : _addFieldEvent( _uidName,fn,
                                             TGIS_FieldType.Date, 0,0    );
        adTinyInt,
        adUnsignedTinyInt  : _addFieldEvent( _uidName,fn,
                                             TGIS_FieldType.Number, 3,0  );
        adSmallInt,
        adUnsignedSmallInt : _addFieldEvent( _uidName,fn,
                                             TGIS_FieldType.Number, 5,0  );
        adInteger,
        adUnsignedInt      : _addFieldEvent( _uidName,fn,
                                             TGIS_FieldType.Number, 10,0 );
        adBigInt,
        adUnsignedBigInt   :{$IFDEF ADOINTUNIT}
                             _addFieldEvent( _uidName,fn,
                                             TGIS_FieldType.Number, 20,0 );
                            {$ELSE}
                             _addFieldEvent( _uidName,fn,
                                             TGIS_FieldType.Float, 0,0   );
                            {$ENDIF}
        adSingle,
        adDouble,
        adCurrency,
        adDecimal,
        adNumeric,
        adVarNumeric       : _addFieldEvent( _uidName,fn,
                                             TGIS_FieldType.Float, 0,0   );
        adChar,
        adWChar,
        adVarChar,
        adVarWChar,
        adGUID,
        adBSTR             : _addFieldEvent( _uidName,fn,
                                             TGIS_FieldType.String,
                            cursorDB[0].myQuery.Fields.Item[i].DefinedSize,0  );
        adLongVarChar,
        adLongVarWChar     : _addFieldEvent( _uidName,fn,
                                             TGIS_FieldType.String,
                                             GIS_SQL_MEMO_SIZE, 0 );
      end ;
    end ;

  end ;

  function TGIS_DbAdo.sqlQueryNameGEOUID(
    const _field   : String ;
    const _fieldEx : String ;
    const _cursor  : Integer
   ) : Integer ;
  var
    i    : Integer ;
    fld  : Field   ;
    tmp  : String  ;
  begin
    assert( cursorDB[_cursor].myQuery.State <> 0, 'Query not opened' ) ;
    assert( not sqlQueryEof( _cursor ), 'Query Eof reached'   ) ;

    // strange code - but we are trying to resolve if database is returning
    // in a column name GEO.UID or UID. Mainly for MSJET ADO vs ODBC access
    for i := 0 to cursorDB[_cursor].myQuery.Fields.Count - 1 do begin
      tmp := cursorDB[_cursor].myQuery.Fields.Item[ i ].Name ;
      if CompareText( tmp, _field ) = 0 then begin
        fld := cursorDB[_cursor].myQuery.Fields.Item[ i ] ;
        break ;
      end
    end ;

    if assigned( fld ) then
      Result := 0
    else begin
      fld    := cursorDB[_cursor].myQuery.Fields.Item[ _fieldEx ] ;
      Result := 1 ;
    end ;
  end ;

  function TGIS_DbAdo.sqlQueryGetGEOUID(
    const _field   : String ;
    const _cursor  : Integer
  ) : Variant ;
  begin
    try
      if not assigned( cursorDB[_cursor].fldUID ) then
        cursorDB[_cursor].fldUID := cursorDB[_cursor].myQuery.Fields.Item[ _field ] ;
      Result := ConvertADOFieldCP( cursorDB[_cursor].fldUID, FiCodePage ) ;
    except
      Result := -1 ;
    end;
  end ;

  function TGIS_DbAdo.sqlQueryGetGEOUID(
    const _index   : Integer ;
    const _cursor  : Integer
  ) : Variant ;
  begin
    try
      if not assigned( cursorDB[_cursor].fldUID ) then
        cursorDB[_cursor].fldUID := cursorDB[_cursor].myQuery.Fields.Item[ _index ] ;
      Result := ConvertADOFieldCP( cursorDB[_cursor].fldUID, FiCodePage ) ;
    except
      Result := -1 ;
    end;
  end ;

  function TGIS_DbAdo.sqlQueryGetSHAPETYPE(
    const _field  : String ;
    const _cursor : Integer
   ) : Variant ;
  begin
    if not assigned( cursorDB[_cursor].fldSHAPETYPE ) then
      cursorDB[_cursor].fldSHAPETYPE := cursorDB[_cursor].myQuery.Fields.Item[ _field ] ;
    Result := ConvertVar( cursorDB[_cursor].fldSHAPETYPE.Value ) ;
  end ;

  function TGIS_DbAdo.sqlQueryGetXMIN(
    const _field   : String ;
    const _cursor  : Integer
   ) : Variant ;
  begin
    if not assigned( cursorDB[_cursor].fldXMIN ) then
      cursorDB[_cursor].fldXMIN := cursorDB[_cursor].myQuery.Fields.Item[ _field ] ;
    Result := ConvertVar( cursorDB[_cursor].fldXMIN.Value ) ;
  end ;

  function TGIS_DbAdo.sqlQueryGetYMIN(
    const _field   : String ;
    const _cursor  : Integer
   ) : Variant ;
  begin
    if not assigned( cursorDB[_cursor].fldYMIN ) then
      cursorDB[_cursor].fldYMIN := cursorDB[_cursor].myQuery.Fields.Item[ _field ] ;
    Result := ConvertVar( cursorDB[_cursor].fldYMIN.Value ) ;
  end ;

  function TGIS_DbAdo.sqlQueryGetGeomVAR(
    const _field       : String ;
    const _fieldEx     : String ;
    const _cursor      : Integer;
    {$IFNDEF OXYGENE}
      const _forceValue  : Boolean  = False
    {$ELSE}
      const _forceValue  : Boolean := False
    {$ENDIF}
   ) : OleVariant ;
  var
    size  : Integer ;
    i     : Integer ;
  begin
    if not assigned( cursorDB[_cursor].fldGEOMETRY ) then begin
      if IsStringEmpty( _fieldEx ) then
        cursorDB[_cursor].fldGEOMETRY := cursorDB[_cursor].myQuery.Fields.Item[ _field ]
      else begin
        try
          for i := 0 to cursorDB[_cursor].myQuery.Fields.Count - 1 do begin
            if CompareText( cursorDB[_cursor].myQuery.Fields.Item[ i ].Name, _field ) = 0 then
            begin
              cursorDB[_cursor].fldGEOMETRY := cursorDB[_cursor].myQuery.Fields.Item[ i ] ;
              break ;
            end
          end ;
          if not assigned( cursorDB[_cursor].fldGEOMETRY ) then Abort ;
        except
          cursorDB[_cursor].fldGEOMETRY := cursorDB[_cursor].myQuery.Fields.Item[ _fieldEx ] ;
        end ;
      end;
    end;

    if IsMySql or _forceValue then begin
      cursorDB[ _cursor ].varStore := ConvertVar( cursorDB[_cursor].fldGEOMETRY.Value ) ;
    end
    else begin
      size := cursorDB[_cursor].fldGEOMETRY.ActualSize ;
      if size > 0  then
        cursorDB[ _cursor ].varStore := cursorDB[_cursor].fldGEOMETRY.GetChunk( size )
      else
        cursorDB[ _cursor ].varStore := NullVar ;
    end;

    Result := cursorDB[ _cursor ].varStore;
  end;

  {$IFDEF CLR}

  function TGIS_DbAdo.sqlQueryGetGeomPtr(
    const _field    : String ;
    const _cursor   : Integer;
      var _size     : Integer
   ) : TBytes ;
  begin
    Result := nil ;

    sqlQueryGetGeomVAR( _field, '', _cursor ) ;

    if VarIsNull( cursorDB[ _cursor ].varStore ) then exit ;

    Result := TBytes( TObject( cursorDB[ _cursor ].varStore ) ) ;
    _size  := length( Result ) ;
  end ;

  function TGIS_DbAdo.sqlQueryGetGeomPtr(
    const _field    : String ;
    const _fieldEx  : String ;
    const _cursor   : Integer;
      var _size     : Integer
   ) : TGIS_Bytes ;
  {$ELSE}

  function TGIS_DbAdo.sqlQueryGetGeomPtr(
    const _field    : String ;
    const _fieldEx  : String ;
    const _cursor   : Integer ;
      var _size     : Integer
   ) : Pointer ;
  {$ENDIF}
  var
    {$IFDEF CLR}
      vr     : TBytes     ;
    {$ELSE}
      ptrvar : Pointer    ;
    {$ENDIF}
  begin
    Result := nil ;

    sqlQueryGetGeomVAR( _field, _fieldEx, _cursor ) ;

    if VarIsNull( cursorDB[ _cursor ].varStore ) then exit ;

    {$IFDEF CLR}
      vr     := TBytes( TObject( cursorDB[ _cursor ].varStore ) ) ;
      cursorDB[ _cursor ].ptrvar := TGIS_Bytes.Create( vr, 0 ) ;
      _size  := length( vr ) ;
      Result := cursorDB[ _cursor ].ptrvar ;
    {$ELSE}
      ptrvar := VarArrayLock( cursorDB[ _cursor ].varStore ) ;
      _size  := VarArrayHighBound( cursorDB[ _cursor ].varStore, 1 ) ;
      Result := ptrvar ;
    {$ENDIF}

  end ;

  procedure TGIS_DbAdo.sqlQueryUnPrepareGetGeom(
    const _cursor        : Integer
  ) ;
  begin
    {$IFDEF CLR}
      if assigned( cursorDB[ _cursor ].ptrvar ) then
        cursorDB[ _cursor ].ptrvar.Reset ;
      FreeObject( cursorDB[ _cursor ].ptrvar ) ;
    {$ELSE}
      if VarIsNull( cursorDB[ _cursor ].varStore ) then exit ;
      VarArrayUnLock( cursorDB[ _cursor ].varStore ) ;
    {$ENDIF}
  end ;

  function TGIS_DbAdo.sqlQueryGetBlob(
    const _name          : String ;
    const _cursor        : Integer
  ) : TStream ;
  var
    {$IFDEF CLR}
      vptr : TBytes  ;
    {$ELSE}
      vptr : Pointer ;
    {$ENDIF}
    store  : Variant   ;
    sz     : Integer   ;
    fld    : Field     ;
  begin
    Result := TMemoryStream.Create ;

    fld := cursorDB[_cursor].myQuery.Fields.Item[ _name ] ;

    if IsMySql then begin
      store := ConvertVar( fld.Value ) ;
      sz    := VarArrayHighBound( store, 1 ) ;
    end
    else begin
      sz := fld.ActualSize ;
      if sz > 0 then
        store := fld.GetChunk( sz )
      else
        store := NullVar ;
    end ;

    if VarIsNull( store ) then exit ;

    {$IFDEF CLR}
      vptr := TBytes( TObject( store ) ) ;
    {$ELSE}
      vptr := VarArrayLock( store ) ;
    {$ENDIF}
    try
      {$IFDEF CLR}
        Result.WriteBuffer( vptr, sz ) ;
      {$ELSE}
        Result.WriteBuffer( vptr^, sz ) ;
      {$ENDIF}
      Result.Position := 0 ;
    finally
      {$IFDEF CLR}
      {$ELSE}
        VarArrayUnLock( store ) ;
      {$ENDIF}
    end ;
  end ;

  function TGIS_DbAdo.sqlQueryGetGeomObj(
    const _field   : String ;
    const _cursor  : Integer
  ) : TObject ;
  begin
    Result := nil ;
  end ;

  procedure TGIS_DbAdo.sqlTableSetGeometry(
    const _id    : Integer ;
    const _name  : String ;
    const _data  : OleVariant  ;
    const _blob  : TGIS_MemoryStream
   ) ;
  begin
    if ( UseTextParameters ) then
      myParams [_id ].Values[ _name ] := VarToString( _data )
    else
      myTable[ _id ].Fields.Item[ _name ].AppendChunk( _data ) ;
  end;

  procedure TGIS_DbAdo.sqlTableSetGeometry(
    const _id    : Integer ;
    const _name  : String ;
    const _data  : TObject
  ) ;
  begin
    // for safe inheritance
  end ;

  procedure TGIS_DbAdo.sqlTableSetBlob(
    const _id    : Integer ;
    const _name  : String ;
    const _data  : OleVariant ;
    const _blob  : TGIS_MemoryStream
  ) ;
  begin
    sqlTableSetGeometry( _id, _name, _data, _blob ) ;
  end ;

  procedure TGIS_DbAdo.sqlUpdateStart(
    const _id     : Integer ;
    const _table  : String
   ) ;
  begin
    // for safe inheritance
  end ;

  function TGIS_DbAdo.sqlQueryGeometryIsText(
    const _cursor : Integer
  ) : Boolean ;
  begin
    if assigned( cursorDB[_cursor].fldGEOMETRY ) then
      {$IFNDEF OXYGENE}
        Result := ( cursorDB[_cursor].fldGEOMETRY.Type_ = adVarWChar     ) or
                  ( cursorDB[_cursor].fldGEOMETRY.Type_ = adLongVarWChar ) or
                  ( cursorDB[_cursor].fldGEOMETRY.Type_ = adVarChar      ) or
                  ( cursorDB[_cursor].fldGEOMETRY.Type_ = adLongVarChar  )
      {$ELSE}
        Result := ( cursorDB[_cursor].fldGEOMETRY.Type  = adVarWChar     ) or
                  ( cursorDB[_cursor].fldGEOMETRY.Type  = adLongVarWChar ) or
                  ( cursorDB[_cursor].fldGEOMETRY.Type  = adVarChar      ) or
                  ( cursorDB[_cursor].fldGEOMETRY.Type  = adLongVarChar  )
      {$ENDIF}
    else
      Result := False ;
  end ;

  procedure TGIS_DbAdo.sqlBuild(
    const _path      : String ;
    const _extent    : TGIS_Extent;
    const _type      : TGIS_ShapeType ;
    const _storage   : String ;
    const _layerName : String
  ) ;
  var
    layername     : String   ;
    canonicalname : String   ;
    ttklspath     : String   ;
    provider      : String   ;
    cfg           : TGIS_Config ;
  begin
    if GetSQLParamFromPath( _path, GIS_INI_LAYERSQL_LAYER, layername
                           ) = GIS_SQL_PROVIDER_ADO then begin
      // only new TTKPS will issue MDB file creation
      if not IsStringEmpty( layername ) then exit ;

      ttklspath := GetPathAbsolute( '', _path ) ;
      if SafeFileExists( ttklspath ) then exit ;

      layername := _layerName ;
      if IsStringEmpty( layername ) then
        layername   := GetFileName( GetPathNoExt( _path ) ) ;

      canonicalname := GisCanonicalSQLName( layername ) ;
      provider      := CreateMSJET( GetFilePath( ttklspath ) + '\' +
                                    GIS_INI_LAYERSQL_DEFAULT_DATABASE
                                   ) ;

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
        cfg.WriteString( GIS_INI_LAYERSQL_CONNECTOR_ADO,
                         provider,
                         ''
                       ) ;
        cfg.WriteString( GIS_INI_LAYERSQL_CONNECTOR_ADO64,
                         provider,
                         ''
                       ) ;
      finally
        cfg.Save ;
        FreeObject( cfg ) ;
      end ;
    end ;
  end ;

  procedure TGIS_DbAdo.cursorOpen(
    const _cursor : Integer
  ) ;
  begin
    if _cursor >= length( cursorDB )  then
      SetLength( cursorDB, _cursor + 1 ) ;
    cursorDB[ _cursor ].curInUse := True ;
    {$IFDEF CLR}
      {$IFNDEF OXYGENE}
        cursorDB[ _cursor ].myQuery := CoRecordset._Create ;
      {$ELSE}
        cursorDB[ _cursor ].myQuery := new RecordsetClass ;
      {$ENDIF}
    {$ELSE}
      cursorDB[ _cursor ].myQuery := CoRecordset.Create ;
    {$ENDIF}
    cursorDB[ _cursor ].varSize  := 1024 ;
    cursorDB[ _cursor ].varStore := VarArrayCreate( [ 0, cursorDB[ _cursor ].varSize ], varByte ) ;
  end ;

  procedure TGIS_DbAdo.cursorClose(
    const _cursor : Integer
  ) ;
  var
    i : Integer ;
  begin
    cursorDB[_cursor].curInUse := False ;

    // truncate cursorState at the tail;
    for i := length( cursorDB ) - 1 downto 0 do begin
      if not cursorDB[i].curInUse then begin
        SetLength( cursorDB, i ) ;
      end
      else
        break ;
    end ;
  end ;

  function TGIS_DbAdo.getLastCursor : Integer ;
  begin
    Result := length( cursorDB ) - 1 ;
  end ;

{==================================== END =====================================}
end.

