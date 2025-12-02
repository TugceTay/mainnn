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
  Encapsulation of a SQL data access via FireDAC.
}

{$IFDEF DCC}
  unit GisDbFireDac ;
  {$HPPEMIT '#pragma link "GisDbFireDac"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}

{$INCLUDE GisInclude.inc}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,

  {$IFNDEF GIS_NODB}
    FireDAC.Comp.Client,
    FireDAC.Comp.DataSet,
    FireDAC.Comp.UI,
    FireDAC.DApt,
    FireDAC.DApt.Intf,
    FireDAC.DatS,
    FireDAC.Phys,
    FireDAC.Phys.Intf,
    FireDAC.Stan.Async,
    FireDAC.Stan.Def,
    FireDAC.Stan.Error,
    FireDAC.Stan.ExprFuncs,
    FireDAC.Stan.Intf,
    FireDAC.Stan.Option,
    FireDAC.Stan.Param,
    FireDAC.Stan.Pool,
    FireDAC.UI.Intf,
    Data.DB,
  {$ENDIF}

  GisTypes,
  GisStreams,
  GisDb ;

type

  /// <summary>
  ///   Class that can read FireDac.
  /// </summary>
  TGIS_DbFireDac = class( TGIS_DbAbstract )
    private

        /// <summary>
        ///   FireDac shared connection object.
        /// </summary>
        FSharedConnection : TFDConnection ;
    private

        /// <summary>
        ///   FireDac connection object.
        /// </summary>
        myConnection      : TFDConnection ;

        cursorDB : array of record
          curInUse          : Boolean ;    // Is cursor in use.
          myQuery           : TFDQuery ;  // Query object.
          bindedFields      : Array of record
            oField : TField ;
            sName  : String ;
          end ;

          // Stored previous select query for optimization purposes.
          oldQuery          : String ;
          fldUID            : TField ;     // Binded GEO.UID
          fldSHAPETYPE      : TField ;     // Binded SHAPETYPE
          fldXMIN           : TField ;     // Binded XMIN
          fldYMIN           : TField ;     // Binded YMIN
          fldGEOMETRY       : TField ;     // Binded GEOMETRY

          /// <summary>
          ///   Pointer treated as "mapped" for shape geometry.
          /// </summary>
         {$IFDEF CLR}
           currPointer     : TBytes  ;
         {$ELSE}
           currPointer     : Pointer ;
         {$ENDIF}

          varStore          : OleVariant  ;
          usedVar           : Boolean ;
        end;

        /// <summary>
        ///   Table object.
        /// </summary>
        myTable           : array [0..2] of TFDQuery ;

        /// <summary>
        ///   Params object.
        /// </summary>
        myParams          : array [0..2] of TParams ;

        /// <summary>
        ///   Transaction object.
        /// </summary>
        myTransaction     : TFDTransaction ;

    private
    // for internal use of TGIS_Viewer

       /// <summary>
       ///   Makes string quated for parametrized update situation.
       /// </summary>
       /// <param name="_txt">
       ///   string to be quated
       /// </param>
       /// <param name="_size">
       ///   maximum size of string value; if -1 then no limits
       /// </param>
       function  safeString            ( const _txt           : String  ;
                                         const _size          : Integer  = -1
                                       ) : String ;
    protected

       /// <summary>
       ///   Update table params.
       /// </summary>
       /// <param name="_idx">
       ///   table index
       /// </param>
       procedure sqlTableUpdateParams  ( const _idx           : Integer ) ;
    public
    // for internal use

       /// <inheritdoc/>
       procedure sqlConnect            ( const _folder        : String ;
                                         const _sqlParameters : TStrings
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
                                         const _forceValue    : Boolean = False
                                       ) :  OleVariant; override;

       /// <inheritdoc/>
       function  sqlQueryGetBlob       ( const _name          : String ;
                                         const _cursor        : Integer
                                       ) : TStream ; override;
       {$IFDEF CLR}
         /// <inheritdoc/>
         function  sqlQueryGetGeomPtr  ( const _field         : String ;
                                         const _fieldEx       : String ;
                                         const _cursor        : Integer
                                        ) : TGIS_Bytes ; override;

         /// <inheritdoc/>
         function  sqlQueryGetGeomPtr  ( const _field         : String ;
                                         const _cursor        : Integer
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
       procedure sqlUpdateStart        ( const _id            : Integer ;
                                         const _table         : String
                                       ) ; override;

       /// <inheritdoc/>
       function sqlGetParams           ( const _idx           : Integer
                                       ) : TStringList ; override;

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
      function  getLastCursor          : Integer ; override;

    public

       /// <inheritdoc/>
       constructor Create  ; override;

       /// <inheritdoc/>
       destructor  Destroy ; override;

      {#gendoc:hide:GENPDK} { TODO -cPDK : Verify }
      /// <summary>
      ///   If assigned then external assigned connection will be used.
      /// </summary>
      property SharedConnection : TFDConnection read  FSharedConnection
                                                 write FSharedConnection ;
    end;

//##############################################################################
implementation

uses
  GisRtl,
  GisInternals,
  GisResource,
  GisSharedConnections;

//=============================================================================
// TGIS_SqlFireDac
//=============================================================================

  constructor TGIS_DbFireDac.Create ;
  var
    i : Integer ;
  begin
    inherited ;

    myConnection := nil ;

    FDManager.ResourceOptions.SilentMode := True;

    for i := Low( myTable ) to High( myTable ) do
      myTable[ i ] := TFDQuery.Create( nil ) ;

    for i := Low( myParams ) to High( myParams ) do
      myParams[ i ] := TParams.Create ;

    for i := 0 to BUILTIN_CURSORS - 1 do
      cursorOpen( i ) ;
  end ;

  destructor  TGIS_DbFireDac.Destroy ;
  var
    i : Integer ;
  begin
    sqlDisconnect ;

    for i := Low( myTable) to High( myTable ) do
      FreeObject( myTable[ i ] ) ;

    for i := Low( myParams ) to High( myParams ) do
      FreeObject( myParams[ i ] ) ;

    for i := 0 to BUILTIN_CURSORS - 1 do
      cursorClose( i );

    inherited ;
  end ;

  function TGIS_DbFireDac.sqlGetParams(
    const _idx : Integer
  ) : TStringList ;
  begin
    Result := nil ;
  end ;

  function TGIS_DbFireDac.safeString(
    const _txt  : String  ;
    const _size : Integer
   ) : String ;
  begin
    if IsStringEmpty( _txt ) then begin
      Result := _txt ;
      exit ;
    end ;

    if _size >= 0 then begin
      if UseTextParameters then begin
        Result := StringReplace( _txt, '''', '''''', [rfReplaceAll, rfIgnoreCase] ) ;
        Result := '' + Copy( Result, StringFirst, Length(Result) ) + '' ;
      end
      else
        Result := Copy( _txt, StringFirst, _size ) ;
    end
    else begin
      if UseTextParameters then begin
        Result := StringReplace( _txt, '''', '''''', [rfReplaceAll, rfIgnoreCase] ) ;
        Result := '' + Result + '' ;
      end
      else
        Result := _txt ;
    end ;
  end ;

  procedure TGIS_DbFireDac.sqlConnect(
    const _folder        : String ;
    const _sqlParameters : TStrings
  ) ;
  var
    cstr    : String      ;
    cprompt : String      ;
    lst     : TStringList ;
  begin
    if not Assigned( SharedConnection ) then begin
      if not Assigned( myConnection ) or
        ( Assigned( myConnection ) and ( not myConnection.Connected ) ) then
      begin
        cprompt := _sqlParameters.Values['LoginPrompt'] ;

        lst := TStringList.Create ;
        try
          lst.Assign( _sqlParameters ) ;
          lst.Values[ GIS_INI_LAYERSQL_LAYER ] := '' ;

          cstr := lst.Values[ 'Database' ] ;
          if not IsStringEmpty( cstr ) then begin
            lst.Values[ 'Database' ] := sqlPathAbsolute( _folder, cstr ) ;
          end;

          myConnection := SharedConnections.OpenFireDac(
                            _sqlParameters.Values['DriverID'   ],
                            ( cprompt = '1' ) or ( Uppercase( cprompt ) = 'YES' ),
                            lst
                          ) ;
        finally
          FreeObject( lst ) ;
        end ;
      end ;

      sqlQueryClose(0) ;
      sqlQueryClose(1) ;
      sqlTableClose( 0 ) ;
      sqlTableClose( 1 ) ;
      sqlTableClose( 2 ) ;
    end
    else if cursorDB[0].myQuery.Connection <> SharedConnection  then begin
      cursorDB[0].myQuery.Connection := SharedConnection ;
      cursorDB[1].myQuery.Connection := SharedConnection ;
      myTable[ 0 ].Connection := SharedConnection ;
      myTable[ 1 ].Connection := SharedConnection ;
      myTable[ 2 ].Connection := SharedConnection ;
    end ;
  end ;

  procedure TGIS_DbFireDac.sqlTableCreateParam(
    const _id       : Integer ;
    const _name     : String ;
    const _type     : TGIS_DataType ;
    const _subtype  : TGIS_SubDataType ;
    const _size     : Integer
  ) ;
  begin
    // for safe inheritance
  end ;

  procedure TGIS_DbFireDac.sqlDisconnect ;
  var
    i : Integer ;
  begin
    sqlQueryClose(0) ;
    cursorDB[0].myQuery.Connection := nil ;
    cursorDB[1].myQuery.Connection := nil ;

    for i := Low( myTable) to High( myTable ) do begin
      sqlTableClose( i ) ;
      myTable[ i ].Connection := nil ;
    end ;

    if not Assigned( SharedConnection ) then begin
      SharedConnections.CloseFireDac( myConnection ) ;
      myConnection := nil ;
    end ;
  end ;

  procedure TGIS_DbFireDac.sqlTransactUpdateStart ;
  begin
    if options.UpdateTransact then
      if not Assigned( SharedConnection ) then begin
        myConnection.Transaction := myTransaction ;
        myConnection.StartTransaction;
      end;
  end ;

  procedure TGIS_DbFireDac.sqlTransactUpdateCommit ;
  begin
    if options.UpdateTransact then
      if not Assigned( SharedConnection ) then
        myConnection.Commit ;
  end ;

  procedure TGIS_DbFireDac.sqlTransactRestructStart ;
  begin
    if options.RestructTransact then
      if not Assigned( SharedConnection ) then begin
        myConnection.Transaction := myTransaction ;
        myConnection.StartTransaction;
      end;
  end ;

  procedure TGIS_DbFireDac.sqlTransactRestructCommit ;
  begin
    if options.RestructTransact then
      if not Assigned( SharedConnection ) then
        myConnection.Commit ;
  end ;

  procedure TGIS_DbFireDac.sqlTransactGlobalUpdateStart ;
  begin
    if options.GlobalUpdateTransact then
      if not Assigned( SharedConnection ) then begin
        myConnection.Transaction := myTransaction ;
        myConnection.StartTransaction;
      end;
  end ;

  procedure TGIS_DbFireDac.sqlTransactGlobalUpdateCommit ;
  begin
    if options.GlobalUpdateTransact then
      if not Assigned( SharedConnection ) then
        myConnection.Commit ;
  end ;

  procedure TGIS_DbFireDac.sqlTransactRollback ;
  begin
    if not Assigned( SharedConnection ) then
      myConnection.Rollback ;
  end ;

  procedure TGIS_DbFireDac.sqlQueryOpen(
    const _query   : String ;
    const _cursor  : Integer
  ) ;
  var
    tmp : String ;
    {$IFDEF GIS_DEBUG}
    t1  : Cardinal ;
    {$ENDIF}
  begin
    tmp := _query ;

    if cursorDB[_cursor].myQuery.Active then begin
      if FReuseQuery and (cursorDB[_cursor].oldQuery = tmp) then
        try
          if not cursorDB[_cursor].myQuery.Bof then
            cursorDB[_cursor].myQuery.First
        except
          sqlQueryClose( _cursor ) ;
        end
      else
        sqlQueryClose( _cursor ) ;
    end ;

    if not cursorDB[_cursor].myQuery.Active then begin
      if Assigned( FOnSQLExecute ) then
        FOnSQLExecute( tmp );

      cursorDB[_cursor].myQuery.SQL.Text := tmp ;

      {$IFDEF GIS_DEBUG}
      t1 := GetTickCount ;
      {$ENDIF}

      cursorDB[_cursor].myQuery.Open ;

      {$IFDEF GIS_DEBUG}
      if Assigned( FOnSQLExecute ) then
        FOnSQLExecute( ' Execute time : ' +FloatToStr( ( GetTickCount - t1 ) / 1000 ) + ' s' )
      {$ENDIF}
    end ;

    cursorDB[_cursor].oldQuery := tmp ;

    cursorDB[_cursor].fldUID       := nil ;
    cursorDB[_cursor].fldSHAPETYPE := nil ;
    cursorDB[_cursor].fldXMIN      := nil ;
    cursorDB[_cursor].fldYMIN      := nil ;
    cursorDB[_cursor].fldGEOMETRY  := nil ;
  end ;

  procedure TGIS_DbFireDac.sqlQueryReset(
    const _cursor  : Integer
  ) ;
  begin
    cursorDB[_cursor].oldQuery := '' ;
  end ;

  procedure TGIS_DbFireDac.sqlQueryClose(
    const _cursor : Integer
  ) ;
  begin
    if Assigned( cursorDB[_cursor].myQuery ) then begin
      if cursorDB[_cursor].myQuery.Active then
        cursorDB[_cursor].myQuery.Close  ;
      cursorDB[_cursor].myQuery.Free   ;
    end ;

    cursorDB[_cursor].myQuery := TFDQuery.Create( nil ) ;

    if not Assigned( SharedConnection ) then
      cursorDB[_cursor].myQuery.Connection := myConnection
    else
      cursorDB[_cursor].myQuery.Connection := SharedConnection ;
  end;

  procedure TGIS_DbFireDac.sqlTableClose(
    const _id : Integer
  ) ;
  begin
    if Assigned( myTable[ _id ] ) then begin
      if myTable[ _id ].Active then
        myTable[ _id ].Close ;
      myTable[ _id ].Free   ;
    end ;

    myTable[ _id ] := TFDQuery.Create( nil ) ;

    if not Assigned( SharedConnection ) then
      myTable[ _id ].Connection := myConnection
    else
      myTable[ _id ].Connection := SharedConnection ;
  end ;

  procedure TGIS_DbFireDac.sqlTablePost(
    const _id : Integer
  ) ;
  begin
    if Assigned( FOnSQLExecute ) then
      FOnSQLExecute( ' Post table ' + IntToStr( _id ) ) ;

    myTable[ _id ].ExecSQL ;
  end ;

  procedure TGIS_DbFireDac.sqlTableExec(
    const _id : Integer
  ) ;
  begin
    if Assigned( FOnSQLExecute ) then
      FOnSQLExecute( ' Post table ' + IntToStr( _id ) ) ;

    myTable[ _id ].ExecSQL ;
  end ;

  procedure TGIS_DbFireDac.sqlTableOpenRead(
    const _id     : Integer ;
    const _query  : String
  ) ;
  begin
    Assert( Assigned(SharedConnection) or myConnection.Connected,
            'Database not connected'
          ) ;
    Assert( not myTable[ _id ].Active, 'Table already opened' ) ;

    if Assigned( FOnSQLExecute ) then
      FOnSQLExecute( _query );

    myTable[ _id ].SQL.Text := _query ;
    myTable[ _id ].Open ;
  end ;

  procedure TGIS_DbFireDac.sqlTableAppend(
    const _id       : Integer ;
    const _query    : String
  ) ;
  var
    i   : Integer ;
    s   : String  ;
    prm : TParam ;
  begin
    if not IsStringEmpty( _query ) then begin
      s := _query + #13#10;
      if myTable[ _id ].SQL.Text <> s then begin
        myTable[ _id ].Params.Clear ;
        myTable[ _id ].SQL.Text := s ;
      end ;

      if Assigned( FOnSQLExecute ) then
        FOnSQLExecute( _query );
    end ;

    if myParams[ _id ].Count > 0 then begin
      for i := 0 to myTable[ _id ].Params.Count - 1 do begin
        try
          prm := myParams[ _id ].FindParam( myTable[ _id ].Params[ i ].Name ) ;
          if assigned( prm ) then
            myTable[ _id ].Params[i].Assign( prm );
        except

        end;
        myTable[ _id ].Params[ i ].Clear ;
      end ;
    end
    else
      for i := 0 to myTable[ _id ].Params.Count - 1 do
        myTable[ _id ].Params[ i ].Clear ;
  end ;

  function TGIS_DbFireDac.sqlTablePrepared(
    const _id  : Integer
  ) : Boolean ;
  begin
    Result := not IsStringEmpty( myTable[ _id ].SQL.Text ) ;
  end ;

  procedure TGIS_DbFireDac.sqlTableOpenWrite(
    const _id     : Integer ;
    const _query  : String
  ) ;
  var
    i : Integer ;
    s : String  ;
  begin
    Assert( Assigned(SharedConnection) or myConnection.Connected,
            'Database not connected'
          ) ;
    Assert( not myTable[ _id ].Active, 'Table already opened' ) ;

    s := _query + #13#10;

    if myTable[ _id ].SQL.Text <> s then begin
      myTable[ _id ].Params.Clear ;
      myTable[ _id ].SQL.Text := s ;
    end ;

    if Assigned( FOnSQLExecute ) then
      FOnSQLExecute( _query );

    if myParams[ _id ].Count > 0 then begin
      for i := 0 to myTable[ _id ].Params.Count - 1 do begin
        myTable[ _id ].Params[ i ].Assign(
          myParams[ _id ].ParamByName( myTable[ _id ].Params[ i ].Name )
        );
        myTable[ _id ].Params[ i ].Clear ;
      end ;
    end;
  end ;

  procedure TGIS_DbFireDac.sqlExec(
    const _command : String
  ) ;
  begin
    if IsStringEmpty( _command ) then exit ;

    if Assigned( FOnSQLExecute ) then
      FOnSQLExecute( _command );

    if Assigned(SharedConnection) then
      SharedConnection.ExecSQL( _command )
    else begin
      Assert( myConnection.Connected, 'Database not connected' ) ;
      if Pos( 'SELECT', UpperCase( _command )) >= StringFirst then
        myConnection.ExecSQLScalar( _command )
      else
        myConnection.ExecSQL( _command ) ;
    end ;
  end ;

  procedure TGIS_DbFireDac.sqlQueryMoveFirst(
    const _cursor : Integer
  ) ;
  begin
    Assert( cursorDB[_cursor].myQuery.Active, 'Query not connected' ) ;

    cursorDB[_cursor].myQuery.First ;
  end ;

  procedure TGIS_DbFireDac.sqlQueryMoveNext(
    const _cursor : Integer
  ) ;
  begin
    Assert( cursorDB[_cursor].myQuery.Active, 'Query not connected' ) ;

    cursorDB[_cursor].myQuery.Next ;
  end ;

  function  TGIS_DbFireDac.sqlQueryEof(
    const _cursor : Integer
  ) : Boolean ;
  begin
    Result := not cursorDB[_cursor].myQuery.Active ;
    if Result then exit ;

    Result := cursorDB[_cursor].myQuery.Eof ;
  end ;

  function TGIS_DbFireDac.sqlTableEof(
    const _id : Integer
  ) : Boolean ;
  begin
    Result := not myTable[ _id ].Active ;
    if Result then exit ;

    Result := myTable[ _id ].Eof ;
    if Result then
      sqlTableClose( 0 ) ;
  end ;

  function TGIS_DbFireDac.sqlQueryGetFieldIndex(
    const _name    : String ;
    const _cursor  : Integer
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
  end ;

  function TGIS_DbFireDac.sqlQueryGetField(
    const _name   : String ;
    const _cursor : Integer
   ) : Variant ;
  begin
    Assert( cursorDB[_cursor].myQuery.Active , 'Query not opened' ) ;
    Assert( not sqlQueryEof( _cursor ), 'Query Eof reached' ) ;

    try
      Result := ConvertDBFieldCP( cursorDB[_cursor].myQuery.FieldByName( _name ),
                                  FiCodePage
                                ) ;
    except
      Result := Unassigned ;
    end ;
  end ;

  function TGIS_DbFireDac.sqlQueryGetFieldById(
    const _id     : Integer ;
    const _cursor : Integer
   ) : Variant ;
  begin
    Assert( cursorDB[_cursor].myQuery.Active, 'Query not opened' ) ;
    Assert( not sqlQueryEof( _cursor ), 'Query Eof reached' ) ;

    try
      Result := ConvertDBFieldCP( cursorDB[_cursor].myQuery.Fields[ _id ],
                                  FiCodePage
                                ) ;
    except
      Result := Unassigned ;
    end ;
  end ;

  function TGIS_DbFireDac.sqlTableGetField(
    const _id   : Integer ;
    const _name : String
   ) : Variant ;
  begin
    Assert( myTable[ _id ].Active, 'Table not opened' ) ;
    Assert( not sqlTableEof( _id ), 'Table Eof reached' ) ;

    Result := myTable[ _id ].FieldValues[ _name ] ;
  end ;

  function TGIS_DbFireDac.sqlTableGetParam(
    const _id   : Integer ;
    const _name : String
   ) : Variant ;
  begin
    Result := Unassigned ;
  end ;

  function TGIS_DbFireDac.sqlBindField(
    const _field  : String ;
    const _cursor : Integer
   ) : Integer ;
  var
    i   : Integer ;
    fld : TField  ;
  begin
    Assert( cursorDB[_cursor].myQuery.Active, 'Query not opened' ) ;

    Result := -1 ;

    for i := 0 to cursorDB[_cursor].myQuery.Fields.Count - 1 do
      if CompareText( cursorDB[_cursor].myQuery.Fields[ i ].FieldName, _field ) = 0 then begin
        Result := i ;
        break ;
      end ;

    if Result >= 0 then
      fld := cursorDB[_cursor].myQuery.FieldByName( _field )
    else
      fld := nil ;

    if Assigned( fld ) then begin
      if High( cursorDB[_cursor].bindedFields ) < Result + 1 then
        SetLength( cursorDB[_cursor].bindedFields, Result + 1 ) ;
      cursorDB[_cursor].bindedFields[ Result ].oField := fld      ;
      cursorDB[_cursor].bindedFields[ Result ].sName  := fld.Name ;
    end ;
  end ;

  procedure TGIS_DbFireDac.sqlTableSetField(
    const _id           : Integer ;
    const _name         : String ;
    const _val          : Variant ;
    const _defSize      : Integer
  ) ;
  var
    defsize : Integer ;
    param   : TFDParam  ;

    // MyODBC has a bug updating layer with more then 14 precision numbers
    function dbl14( _v : Double ) : Double ;
    begin
      if isMySql then
        Result := DotStrToFloat( Format( '%.14g', [ _v ] ) )
      else
        Result := _v ;
    end ;

  begin
    if IsStringEmpty( _name ) then exit;

    param := myTable[ _id ].Params.FindParam( _name ) ;
    if not assigned( param ) then exit ;

    case GetVariantType( _val ) of
      TGIS_VariantType.Boolean :
        begin
          try
            if param.DataType = ftBoolean then
              param.Value := _val
            else
              if _val then begin
                if ( UseTextParameters ) then
                  param.Value := safeString( 'Y' )
                else
                param.Value := 'Y'
              end
              else begin
                if ( UseTextParameters ) then
                  param.Value := safeString( 'N' )
              else
                param.Value := 'N' ;
              end ;
          except
            if _val then begin
              if ( UseTextParameters ) then
                param.Value := safeString( 'Y' )
              else
              param.Value := 'Y'
            end
            else begin
              if ( UseTextParameters ) then
                param.Value := safeString( 'N' )
            else
              param.Value := 'N' ;
          end ;
        end ;
        end ;

      TGIS_VariantType.Float :
        begin
          param.Value := dbl14( _val ) ;
        end ;

      TGIS_VariantType.AnsiString,
      TGIS_VariantType.WideString :
        begin
          if param.DataType = ftOraClob then begin
            param.DataType  := ftString;
            param.Value     := Copy( _val, 1, GIS_SQL_ORACLOB_SIZE ) ;
          end
          else if param.DataType = ftBlob then begin
            defsize     := Length(_val) ;
            param.Value := Copy( _val, 1, defsize ) ;
          end
          else if param.DataType <> ftMemo then begin
            if ( UseTextParameters ) then begin
              defsize     := Length(_val) ;
              param.Value := safeString( _val, defsize ) ;
            end
            else begin
            defsize     := param.Size ;
            param.Value := Copy( _val, 1, defsize ) ;
            end ;
          end
          else begin
            if ( UseTextParameters ) then
              //param.Value := safeString( _val, param.Size )
              param.Value := safeString( _val )
            else
            param.Value := _val ;
          end ;
        end ;
      else
        param.Value := _val ;
    end ;
  end ;

  function TGIS_DbFireDac.sqlGetBindedField(
    const _field         : Integer ;
    const _rebindFields  : Boolean ;
    const _cursor        : Integer
   ) : Variant ;
  var
    fld : TField     ;

    procedure rebind_fields ;
    var
      i : Integer ;
    begin
      for i := Low( cursorDB[_cursor].bindedFields ) to High( cursorDB[_cursor].bindedFields ) do begin
        if not IsStringEmpty( cursorDB[_cursor].bindedFields[i].sName ) then
          cursorDB[_cursor].bindedFields[i].oField :=
              cursorDB[_cursor].myQuery.FieldByName(
                  cursorDB[_cursor].bindedFields[i].sName
              ) ;
      end ;
    end ;

    function hasRecords : Boolean ;
    begin
      try
        Result := not ( cursorDB[_cursor].myQuery.Bof and
                        cursorDB[_cursor].myQuery.Eof ) ;
      except
        Result := False ;
      end;
    end;

  begin
    Result := unassigned ;

    if _rebindFields then
      rebind_fields ;

    Assert( cursorDB[_cursor].myQuery.Active, 'Query not opened' ) ;

    if (_field < 0) or (_field > High(cursorDB[_cursor].bindedFields)) then exit ;

    fld := cursorDB[_cursor].bindedFields[ _field ].oField ;

    if Assigned( fld ) and hasRecords then
      Result := ConvertDBFieldCP( fld, FiCodePage ) ;
  end ;

  procedure TGIS_DbFireDac.sqlQueryStructure(
    const _tableName     : String ;
    const _uidName       : String ;
    const _addFieldEvent : TGIS_LayerAddFieldEvent
  ) ;
  var
    i     : Integer ;
    fname : String ;
  begin
    Assert( cursorDB[0].myQuery.Active, 'Query not opened' ) ;

    for i := 0 to cursorDB[0].myQuery.FieldList.Count - 1 do begin
      fname := cursorDB[0].myQuery.FieldList.Fields[ i ].FieldName ;
      case cursorDB[0].myQuery.FieldList.Fields[ i ].DataType of
        ftBoolean     : _addFieldEvent( _uidname, fname,
                                        TGIS_FieldType.Boolean, 0, 0 ) ;
        ftTime,
        ftDate,
        ftDateTime,
        ftTimeStamp   : _addFieldEvent( _uidname, fname,
                                        TGIS_FieldType.Date, 0, 0    ) ;
        ftWord ,
        ftSmallint,
        ftInteger,
        ftLargeint,
        ftAutoInc     : _addFieldEvent( _uidname, fname,
                                        TGIS_FieldType.Number, 18, 0 ) ;
        ftFloat,
        ftCurrency,
        ftBCD,
        ftFMTBcd      : _addFieldEvent( _uidname, fname,
                                        TGIS_FieldType.Number, 20, 8 ) ;
        ftString,
        ftWideString,
        ftFixedChar   : _addFieldEvent( _uidname, fname,
                                        TGIS_FieldType.String,
                           cursorDB[0].myQuery.FieldList.Fields[ i ].Size, 0 ) ;
        ftMemo,
        ftFmtMemo,
        ftOraClob     : _addFieldEvent( _uidname, fname,
                                        TGIS_FieldType.String,
                                        GIS_SQL_MEMO_SIZE, 0 ) ;
      end ;
    end ;
  end ;

  function TGIS_DbFireDac.sqlQueryNameGEOUID(
    const _field   : String ;
    const _fieldEx : String ;
    const _cursor  : Integer
   ) : Integer ;
  var
    fld  : TField  ;
  begin
    Assert( cursorDB[_cursor].myQuery.Active , 'Query not opened'  ) ;
    Assert( not sqlQueryEof( _cursor ), 'Query Eof reached' ) ;

    Result := 0 ;
    // strange code - but we are trying to resolve if database is returning
    // in a column name GEO.UID or UID. Mainly for MSJET ADO vs ODBC access
    fld := cursorDB[_cursor].myQuery.FindField( _field ) ;
    if Assigned( fld ) then begin
      Result := 0 ;
    end
    else begin
      fld  := cursorDB[_cursor].myQuery.FindField( _fieldEx ) ;
      if Assigned( fld ) then begin
        Result := 1 ;
      end ;
    end ;
  end ;

  function TGIS_DbFireDac.sqlQueryGetGEOUID(
    const _field   : String ;
    const _cursor  : Integer
  ) : Variant ;
  begin
    try
      if not Assigned( cursorDB[_cursor].fldUID ) then
        cursorDB[_cursor].fldUID := cursorDB[_cursor].myQuery.FieldByName( _field ) ;
      Result := cursorDB[_cursor].fldUID.Value ;
    except
      Result := -1 ;
    end;
  end ;

  function TGIS_DbFireDac.sqlQueryGetGEOUID(
    const _index   : Integer ;
    const _cursor  : Integer
  ) : Variant ;
  begin
    try
      if not Assigned( cursorDB[_cursor].fldUID ) then
        cursorDB[_cursor].fldUID := cursorDB[_cursor].myQuery.Fields[ _index ] ;
      Result := cursorDB[_cursor].fldUID.Value ;
    except
      Result := -1 ;
    end;
  end ;

  function TGIS_DbFireDac.sqlQueryGetSHAPETYPE(
    const _field  : String ;
    const _cursor : Integer
   ) : Variant ;
  begin
    if not Assigned( cursorDB[_cursor].fldSHAPETYPE ) then
      cursorDB[_cursor].fldSHAPETYPE := cursorDB[_cursor].myQuery.FieldByName(_field) ;
    Result := cursorDB[_cursor].fldSHAPETYPE.Value ;
  end ;

  function TGIS_DbFireDac.sqlQueryGetXMIN(
    const _field   : String ;
    const _cursor  : Integer
  ) : Variant ;
  begin
    if not Assigned( cursorDB[_cursor].fldXMIN ) then
      cursorDB[_cursor].fldXMIN := cursorDB[_cursor].myQuery.FieldByName(_field) ;
    Result := cursorDB[_cursor].fldXMIN.Value ;
  end ;

  function TGIS_DbFireDac.sqlQueryGetYMIN(
    const _field   : String ;
    const _cursor  : Integer
   ) : Variant ;
  begin
    if not Assigned( cursorDB[_cursor].fldYMIN ) then
      cursorDB[_cursor].fldYMIN := cursorDB[_cursor].myQuery.FieldByName( _field ) ;
    Result := cursorDB[_cursor].fldYMIN.Value ;
  end ;

  function TGIS_DbFireDac.sqlQueryGetGeomVAR(
    const _field       : String ;
    const _fieldEx     : String ;
    const _cursor      : Integer;
    const _forceValue  : Boolean = False
   ) : OleVariant ;
  var
    {$IFDEF CLR}
      vr     : TBytes     ;
    {$ELSE}
      ptrvar : Pointer    ;
    {$ENDIF}
    size     : Integer    ;
    blob     : TStream ;
  begin
    Result  := NullVar ;
    cursorDB[_cursor].usedVar := True ;

    if not Assigned( cursorDB[_cursor].fldGEOMETRY ) then begin
      if IsStringEmpty( _fieldEx ) then
        cursorDB[_cursor].fldGEOMETRY := cursorDB[_cursor].myQuery.FindField(_field)
      else
        cursorDB[_cursor].fldGEOMETRY := cursorDB[_cursor].myQuery.FindField(_fieldEx) ;
    end;

    // blob used because a number of databases will report a proper size of
    // blob after reading the blob
    blob := cursorDB[_cursor].myQuery.CreateBlobStream(
              cursorDB[_cursor].fldGEOMETRY, bmRead
            ) ;
    try
      size := blob.Size ;
      if size > 0 then begin
        {$IFDEF CLR}
          SetLength( vr, size ) ;
          blob.ReadBuffer( vr, size ) ;
          cursorDB[_cursor].varStore := vr ;
          Result := cursorDB[_cursor].varStore;
        {$ELSE}
          VarArrayRedim( cursorDB[_cursor].varStore, size ) ;
          ptrvar := VarArrayLock( cursorDB[_cursor].varStore ) ;
          try
            blob.ReadBuffer( ptrvar^, size ) ;
          finally
            VarArrayUnlock( cursorDB[_cursor].varStore );
            Result := cursorDB[_cursor].varStore;
          end;
        {$ENDIF}
      end ;
    finally
      FreeObject( blob ) ;
    end ;
  end ;

  {$IFDEF CLR}
  function TGIS_DbFireDac.sqlQueryGetGeomPtr(
    const _field    : String ;
    const _cursor   : Integer
      var _size     : Integer
   ) : TBytes ;
  begin
    Result := nil ;

    sqlQueryGetGeomVAR( _field, '', _cursor ) ;

    if VarIsNull( varStore ) then exit ;

    Result := TBytes( TObject( varStore ) ) ;
  end ;

  function TGIS_DbFireDac.sqlQueryGetGeomPtr(
    const _field    : String ;
    const _fieldEx  : String ;
    const _cursor   : Integer
      var _size     : Integer
   ) : TGIS_Bytes ;
  {$ELSE}

  function TGIS_DbFireDac.sqlQueryGetGeomPtr(
    const _field    : String ;
    const _fieldEx  : String ;
    const _cursor   : Integer ;
      var _size     : Integer
   ) : Pointer ;
  {$ENDIF}
  var
    size     : Integer ;
    blob     : TStream ;
  begin
    cursorDB[_cursor].usedVar := False ;

    if not Assigned( cursorDB[_cursor].fldGEOMETRY ) then begin
      if IsStringEmpty( _fieldEx ) then
        cursorDB[_cursor].fldGEOMETRY := cursorDB[_cursor].myQuery.FindField(_field)
      else
        cursorDB[_cursor].fldGEOMETRY := cursorDB[_cursor].myQuery.FindField(_fieldEx) ;
    end ;
    // blob used because a number of databases will report a proper size of
    // blob after reading the blob
    blob := cursorDB[_cursor].myQuery.CreateBlobStream(
              cursorDB[_cursor].fldGEOMETRY, bmRead
            ) ;
    try
      size := blob.Size ;
      if size > 0 then begin
       {$IFDEF CLR}
         SetLength( cursorDB[_cursor].currPointer, size ) ;
         blob.ReadBuffer( cursorDB[_cursor].currPointer, size ) ;
         Result := TGIS_Bytes.Create( cursorDB[_cursor].currPointer, 0 );
       {$ELSE}
         ReallocMem( cursorDB[_cursor].currPointer, size ) ;
         blob.ReadBuffer( cursorDB[_cursor].currPointer^, size ) ;
         Result := cursorDB[_cursor].currPointer;
       {$ENDIF}
      end
      else
        Result := nil ;
      _size := size ;
    finally
      FreeObject( blob ) ;
    end ;
  end ;

  procedure TGIS_DbFireDac.sqlQueryUnPrepareGetGeom(
    const _cursor        : Integer
  ) ;
  begin
    // for safe inheritance
  end ;

  function TGIS_DbFireDac.sqlQueryGetBlob(
    const _name    : String  ;
    const _cursor  : Integer
   ) : TStream ;
  var
    blob : TStream ;
    fld  : TField ;
  begin
    fld    := cursorDB[_cursor].myQuery.FindField( _name ) ;
    blob   := cursorDB[_cursor].myQuery.CreateBlobStream( fld, bmRead ) ;
    Result := blob ;
  end ;

  function TGIS_DbFireDac.sqlQueryGetGeomObj(
    const _field   : String ;
    const _cursor  : Integer
  ) : TObject ;
  begin
    Result := nil ;
  end ;

  procedure TGIS_DbFireDac.sqlTableSetGeometry(
    const _id    : Integer ;
    const _name  : String ;
    const _data  : OleVariant ;
    const _blob  : TGIS_MemoryStream
   ) ;
  var
    blob    : TStream ;
    {$IFDEF CLR}
      vr    : TBytes  ;
    {$ELSE}
      sptr  : Pointer ;
    {$ENDIF}
    ssize   : Integer ;
  begin
    if not Assigned( _blob ) then begin
      blob := TMemoryStream.Create ;
      try
        {$IFDEF CLR}
          vr := TBytes( TObject( _data ) ) ;
          ssize := VarArrayHighBound( _data, 1 ) + 1;
          blob.WriteBuffer( vr, ssize ) ;
        {$ELSE}
          sptr := VarArrayLock( _data ) ;
          ssize := VarArrayHighBound( _data, 1 ) + 1;
          blob.WriteBuffer( sptr^, ssize ) ;
          VarArrayUnlock( _data ) ;
        {$ENDIF}
        blob.Position := 0 ;

        if isOracle then
          myTable[ _id ].Params.FindParam(_name).LoadFromStream( blob, ftOraBlob )
        else
          myTable[ _id ].Params.FindParam(_name).LoadFromStream( blob, ftBlob ) ;
      finally
        FreeObject( blob ) ;
      end ;
    end
    else begin
      blob := _blob ;
      if isOracle then
        myTable[ _id ].Params.FindParam(_name).LoadFromStream( blob, ftOraBlob )
      else
        myTable[ _id ].Params.FindParam(_name).LoadFromStream( blob, ftBlob ) ;
    end;

  end;

  procedure TGIS_DbFireDac.sqlTableSetBlob(
    const _id    : Integer ;
    const _name  : String ;
    const _data  : OleVariant ;
    const _blob  : TGIS_MemoryStream
  ) ;
  begin
    sqlTableSetGeometry( _id, _name, _data, _blob ) ;
  end ;

  procedure TGIS_DbFireDac.sqlTableSetGeometry(
    const _id    : Integer ;
    const _name  : String ;
    const _data  : TObject
  ) ;
  begin
    // for safe inheritance
  end ;

  procedure TGIS_DbFireDac.sqlTableUpdateParams(
    const _idx : Integer
  ) ;
  var
    i   : Integer ;
    prm : TParam  ;
  begin
    myParams[ _idx ].Clear ;

    for i := 0 to myTable[ _idx ].Fields.Count - 1 do begin
      if ( myTable[ _idx ].Fields[ i ].DataType = ftBCD    ) or
         ( myTable[ _idx ].Fields[ i ].DataType = ftFMTBcd )
      then begin
        prm := TParam( myParams[ _idx ].Add ) ;
        prm.Name          := myTable[ _idx ].Fields[ i ].FieldName ;
        prm.DataType      := myTable[ _idx ].Fields[ i ].DataType ;
        prm.Size          := myTable[ _idx ].Fields[ i ].DataSize;
        prm.NumericScale  := myTable[ _idx ].Fields[ i ].Size;
        prm.Bound         := True;
        prm.Clear;
      end
      else
        TParam(myParams[ _idx ].Add).AssignField( myTable[_idx].Fields[ i ] ) ;
    end ;
  end ;

  procedure TGIS_DbFireDac.sqlUpdateStart(
    const _id     : Integer ;
    const _table  : String
  ) ;
  begin
    if not IsStringEmpty( _table ) then begin
      sqlTableOpenRead( _id, _table ) ;
      try
        sqlTableUpdateParams( _id ) ;
      finally
        sqlTableClose( _id ) ;
      end ;
    end ;
  end ;

  function TGIS_DbFireDac.sqlQueryGeometryIsText(
    const _cursor : Integer
  ) : Boolean ;
  begin
    if Assigned( cursorDB[_cursor].fldGEOMETRY ) then
      Result := ( cursorDB[_cursor].fldGEOMETRY.DataType = ftString     ) or
                ( cursorDB[_cursor].fldGEOMETRY.DataType = ftWideString ) or
                ( cursorDB[_cursor].fldGEOMETRY.DataType = ftFixedChar  )
    else
      Result := False ;
  end ;

  procedure TGIS_DbFireDac.sqlBuild(
    const _path     : String ;
    const _extent   : TGIS_Extent;
    const _type     : TGIS_ShapeType ;
    const _storage  : String ;
    const _layerName: String
  ) ;
  begin
    // for safe inheritance
  end ;

  procedure TGIS_DbFireDac.cursorOpen(
    const _cursor : Integer
  ) ;
  begin
    if _cursor >= length( cursorDB )  then
      SetLength( cursorDB, _cursor + 1 ) ;
    cursorDB[ _cursor ].curInUse := True ;
    cursorDB[ _cursor ].myQuery := TFDQuery.Create( nil ) ;
    if not Assigned( SharedConnection ) then
      cursorDB[_cursor].myQuery.Connection := myConnection
    else
      cursorDB[_cursor].myQuery.Connection := SharedConnection ;

    cursorDB[_cursor].varStore := VarArrayCreate( [ 0, 1024 ], varByte ) ;
  end ;

  procedure TGIS_DbFireDac.cursorClose(
    const _cursor : Integer
  ) ;
  var
    i : Integer ;
  begin
    cursorDB[_cursor].curInUse := False ;
    cursorDB[_cursor].myQuery.Free ;

    if Assigned( cursorDB[_cursor].currPointer ) then
    {$IFDEF CLR}
      begin
        SetLength( cursorDB[_cursor].currPointer, 0 ) ;
        FreeObject( cursorDB[_cursor].currPointer ) ;
      end ;
    {$ELSE}
      FreeMem( cursorDB[_cursor].currPointer ) ;
    {$ENDIF}

    // truncate cursorState at the tail;
    for i := Length( cursorDB ) - 1 downto 0 do begin
      if not cursorDB[i].curInUse then begin
        SetLength( cursorDB, i ) ;
      end
      else
        break ;
    end ;
  end ;

  function TGIS_DbFireDac.getLastCursor : Integer ;
  begin
    Result := length( cursorDB ) - 1 ;
  end ;

{==================================== END =====================================}
end.


