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
  Encapsulation of SQL Pixel layer.
}

{$IFDEF DCC}
  unit GisLayerPixelSql ;
  {$HPPEMIT '#pragma link "GisLayerPixelSql"'}
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
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,

    GisLayerPixel,
    GisDb ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  /// <summary>
  ///   SQL-based pixel layer class encapsulation.
  /// </summary>
  TGIS_LayerPixelSqlAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF}
                               class( TGIS_LayerPixel )

    protected // properties events
      /// <summary>
      ///   Event to be fired before dialect change.
      /// </summary>
      FOnBeforeDialectChange :
        {$IFDEF CLR}
          EventHandler ;
        {$ELSE}
          TNotifyEvent ;
        {$ENDIF}

      /// <summary>
      ///   Event to be fired after dialect change.
      /// </summary>
      FOnAfterDialectChange   :
        {$IFDEF CLR}
          EventHandler ;
        {$ELSE}
          TNotifyEvent ;
        {$ENDIF}

      procedure fset_SQLExecuteEvent  ( const _event : TGetStrProc
                                      ) ;

      function  fget_SQLExecuteEvent  : TGetStrProc ;
      function  fget_SQLParameter       ( const _name   : String
                                      ) : String ;
      procedure fset_SQLParameter       ( const _name   : String;
                                        const _value  : String
                                      ) ;

      /// <summary>
      ///   Process tokens in a SQLParameters property.
      /// </summary>
      /// <param name="_token">
      ///   password token
      /// </param>
      /// <returns>
      ///   token
      /// </returns>      
      function  passwordCallBack      ( const _token  : String
                                      ) : String ;

      /// <summary>
      ///   Return a SQL command associated with a given identifier.
      /// </summary>
      /// <param name="_id">
      ///   command identifier
      /// </param>
      /// <returns>
      ///   SQL command
      /// </returns>
      function  getCmd                ( const _id     : Integer
                                      ) : String ;

      /// <summary>
      ///   Initialize command list process.
      /// </summary>
      /// <param name="_cnt">
      ///   number of commands
      /// </param>
      procedure initializeCommandList( const _cnt     : Integer ) ;

      /// <summary>
      ///   Finalize command list process.
      /// </summary>
      procedure finalizeCommandList ;

      /// <summary>
      ///   Initialize connect process.
      /// </summary>
      /// <param name="_prepare">
      ///   if True, command list will be prepared
      /// </param>
      procedure initializeConnect    ( const _prepare : Boolean
                                      ) ;

      function  fget_Table : String ; virtual;

      /// <summary>
      ///   Prepare command list.
      /// </summary>
      procedure prepareCommandList ; virtual;

      /// <summary>
      ///   Prepare additional parameters list.
      /// </summary>
      procedure prepareParametersExList ; virtual;
    // properties internal values
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      /// <summary>
      ///   Connection parameters.
      /// </summary>
      FSQLParameters  : TStringList ;
    protected 

      /// <summary>
      ///   Table name.
      /// </summary>
      FCurrTable      : String ;

      /// <summary>
      ///   SQL dialect (list of tokens) attached to the layer.
      /// </summary>
      FSQLDialectList : TStringList ;

      /// <summary>
      ///   List of sql commands.
      /// </summary>
      FSQLCommands    : TStringList ;

      /// <summary>
      ///   List of additional connection parameters.
      /// </summary>
      FSQLParametersEx : TStringList ;

      /// <summary>
      ///   CodePage stored for optimization purposes.
      /// </summary>
      iCodePage       : Integer ;

      /// <summary>
      ///   JoinCodePage stored for optimization purposes.
      /// </summary>
      iJoinCodePage   : Integer ;

      /// <summary>
      ///   Database supporting class handle.
      /// </summary>
      oGisDb          : TGIS_DbAbstract ;
    protected
    // destructor

      /// <summary>
      ///   Destroy a layer instance.
      /// </summary>
      procedure doDestroy ; override;
    public
    // constructors

      /// <summary>
      ///   Create a layer instance.
      /// </summary>
      /// <remarks>
      ///   See TGIS_LayerVector.Create for details and example.
      /// </remarks>
      constructor Create ; override;
    public

      /// <summary>
      ///   Execute custom SQL statement.
      /// </summary>
      /// <param name="_sql">
      ///   SQL String
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_SQLQUERY
      /// </exception>
      /// <returns>
      ///   True if succeeded
      /// </returns>
      function  ExecuteSQL ( const _sql : String
                           ) : Boolean ; virtual;

      /// <summary>
      ///   Reset the layer dataset to ensure the current database context
      ///   upon next drawing (e.g to notify changes when a record was deleted
      ///   manually in the table).
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Call it before the viewer update.
      ///    </note>
      /// </remarks>
      procedure ResetDataset ; virtual;
    public // properties

      /// <summary>
      ///   Connection parameters.
      /// </summary>
      /// <param name="_name">
      ///   parameter name
      /// </param>
      property SQLParameter[ const _name : String ] : String
                             read  fget_SQLParameter
                             write fset_SQLParameter ;
      /// <summary>
      ///   List of additional connection parameters.
      /// </summary>
      property SQLParametersEx :
        {$IFDEF OXYGENE}
          TGIS_StringList read FSQLParametersEx ;
        {$ELSE}
          TStringList     read FSQLParametersEx ;
        {$ENDIF}

      /// <summary>
      ///   Dialect list in a form "token=replacement". Will be changed
      ///   after each change of SQLDialect property.
      /// </summary>
      property SQLDialectList :
        {$IFDEF OXYGENE}
          TGIS_StringList  read  FSQLDialectList ;
        {$ELSE}
          TStringList      read  FSQLDialectList ;
        {$ENDIF}

      /// <summary>
      ///   SQL Commands used for database operations.
      /// </summary>
      property SQLCommands    :
        {$IFDEF OXYGENE}
          TGIS_StringList  read  FSQLCommands ;
        {$ELSE}
          TStringList      read  FSQLCommands ;
        {$ENDIF}

      /// <summary>
      ///   SQL table.
      /// </summary>
      property Table          : String read  fget_Table
                                       write FCurrTable ;
    published // events
        /// <event/>
        /// <summary>
        ///   Will be fired upon any sql execution to trace sql statements.
        /// </summary>
        property SQLExecuteEvent : TGetStrProc        read  fget_SQLExecuteEvent
                                                      write fset_SQLExecuteEvent ;
      {$IFDEF CLR}
      
          /// <event/>
          /// <summary>
          ///   Will be fired before every SQLDialect change. By changing
          ///   SQLDialectList inside handler for this event you will be able
          ///   to modify dialect dynamically.
          /// </summary>
          event   BeforeDialectChangeEvent : EventHandler
                                              delegate FOnBeforeDialectChange ;
      {$ELSE}
      
          /// <event/>
          /// <summary>
          ///   Will be fired before every SQLDialect change. By changing
          ///   SQLDialectList inside handler for this event you will be able
          ///   to modify dialect dynamically.
          /// </summary>
          property BeforeDialectChangeEvent : TNotifyEvent
                                              read  FOnBeforeDialectChange
                                              write FOnBeforeDialectChange ;
      {$ENDIF}
      {$IFDEF CLR}
          
          /// <event/>
          /// <summary>
          ///   Will be fired after SQLDialect change. By changing
          ///   SQLCommandList inside handler for this event you will be able
          ///   to modify commands dynamically.
          /// </summary>
          event   AfterDialectChangeEvent : EventHandler
                                            delegate FOnAfterDialectChange ;
      {$ELSE}
      
          /// <event/>
          /// <summary>
          ///   Will be fired after SQLDialect change. By changing
          ///   SQLCommandList inside handler for this event you will be able
          ///   to modify commands dynamically.
          /// </summary>
          property AfterDialectChangeEvent  : TNotifyEvent
                                              read  FOnAfterDialectChange
                                              write FOnAfterDialectChange ;
      {$ENDIF}
  end ;


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SysUtils,

    GisRtl,
    GisTypes,
    GisFunctions,
    GisInternals,
    GisClasses,
    GisResource,
    GisParams ;
{$ENDIF}

//=============================================================================
// TGIS_LayerPixelSqlAbstract
//=============================================================================

  constructor TGIS_LayerPixelSqlAbstract.Create ;
  begin
    inherited ;

    FSQLParameters   := TStringList.Create ;
    FSQLDialectList  := TStringList.Create ;
    FSQLCommands     := TStringList.Create ;
    FSQLParametersEx := TStringList.Create ;
  end ;

  procedure TGIS_LayerPixelSqlAbstract.doDestroy ;
  begin
    FreeObject( FSQLParameters   ) ;
    FreeObject( FSQLDialectList  ) ;
    FreeObject( FSQLCommands     ) ;
    FreeObject( FSQLParametersEx ) ;

    inherited ;
  end ;

  procedure TGIS_LayerPixelSqlAbstract.fset_SQLExecuteEvent(
    const _event : TGetStrProc
  ) ;
  begin
    if assigned( oGisDb ) then
      oGisDb.SQLExecuteEvent := _event
  end ;

  function TGIS_LayerPixelSqlAbstract.fget_SQLExecuteEvent
   : TGetStrProc ;
  begin
    if assigned( oGisDb ) then
      Result := oGisDb.SQLExecuteEvent
    else
      Result := nil ;
  end ;

  function TGIS_LayerPixelSqlAbstract.fget_SQLParameter(
    const _name : String
  ) : String ;
  begin
    Result := FSQLParameters.Values[ _name ] ;
  end ;

  function TGIS_LayerPixelSqlAbstract.fget_Table : String ;
  begin
    Result := FCurrTable ;
  end ;

  procedure TGIS_LayerPixelSqlAbstract.fset_SQLParameter(
    const _name  : String ;
    const _value : String
  ) ;
  begin
    FSQLParameters.Values[ _name ] := _value ;
  end ;

  function TGIS_LayerPixelSqlAbstract.passwordCallBack(
    const _token : String
  ) : String ;
  begin
    Result := GisPasswordList.Get( Name, _token ) ;
    if IsStringEmpty( Result ) then begin
      if assigned( FOnPassword ) then
        {$IFDEF OXYGENE}
          Result := FOnPassword( Self,
                                 TGIS_TemplateProducerEventArgs.Create( _token )
                               )
        {$ELSE}
          Result := PasswordEvent( Self, _token )
        {$ENDIF}
      else
        Result := _token ;
      GisPasswordList.Add( Name, _token, Result ) ;
    end ;
  end ;

  function TGIS_LayerPixelSqlAbstract.getCmd(
    const _id : Integer
  ) : String ;
  begin
    assert( _id < FSQLCommands.Count, 'no such command' ) ;
    Result := FSQLCommands[ _id ] ;
  end ;

  function TGIS_LayerPixelSqlAbstract.ExecuteSQL(
    const _sql : String
  ) : Boolean ;
  begin
    try
      oGisDb.sqlExec( _sql ) ;
    except
      on e : EGIS_Exception do
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_SQLQUERY ), e.Message, 0 ) ;
    end;
    Result := True ;
  end ;

  procedure TGIS_LayerPixelSqlAbstract.ResetDataset ;
  begin
    oGisDb.sqlQueryReset(0) ;
  end ;

  procedure TGIS_LayerPixelSqlAbstract.initializeCommandList(
    const _cnt : Integer
  ) ;
  var
    i : Integer ;
  begin
    if assigned( FOnBeforeDialectChange ) then
      {$IFDEF CLR}
        FOnBeforeDialectChange( Self, EventArgs.Create ) ;
      {$ELSE}
        BeforeDialectChangeEvent( Self ) ;
      {$ENDIF}

    FSQLCommands.Clear ;
    for i := 0 to _cnt do
      FSQLCommands.Add( '' ) ;
  end ;

  procedure TGIS_LayerPixelSqlAbstract.finalizeCommandList ;
  var
    lst : TStringList ;
    i   : Integer ;
  begin
    lst := TStringList.Create ;
    try
      lst.AddStrings( FSQLParameters  ) ;
      lst.AddStrings( FSQLDialectList ) ;

      for i := 0 to FSQLCommands.Count - 1 do begin
        FSQLCommands[i] := TemplateProducer( FSQLCommands[i], lst, nil, False ) ;
      end ;
    finally
      FreeObject( lst ) ;
    end ;

    if assigned( FOnAfterDialectChange ) then
      {$IFDEF CLR}
        FOnAfterDialectChange( Self, EventArgs.Create ) ;
      {$ELSE}
        AfterDialectChangeEvent( Self ) ;
      {$ENDIF}
  end ;

  procedure TGIS_LayerPixelSqlAbstract.prepareParametersExList ;
  begin
    FSQLParametersEx.Clear ;
  end ;

  procedure TGIS_LayerPixelSqlAbstract.initializeConnect(
    const _prepare : Boolean
  ) ;
  var
    i           : Integer     ;
    lst         : TStringList ;
    tmp         : String      ;
    isReadOnly  : Boolean ;
  begin
    oGisDb.InitializeProvider ;

    lst := TStringList.Create ;
    try
      ReadSQLParamsFromPath( Path, lst  ) ;

      for i := 0 to lst.Count - 1 do begin
        tmp := lst.Names[ i ] ;
        if IsStringEmpty( FSQLParameters.Values[ tmp ] ) then
          FSQLParameters.Values[ tmp ] := lst.Values[ tmp ] ;
      end ;
    finally
      FreeObject( lst ) ;
    end ;

    // resolve any ID/Password tokens
    {$IFNDEF OXYGENE}
      FSQLParameters.Text  := TemplateProducer( FSQLParameters.Text, nil,
                                                 passwordCallBack, False
                                              ) ;
    {$ELSE}
      FSQLParameters.Text  := TemplateProducer( FSQLParameters.Text, nil,
                                                @passwordCallBack, False
                                              ) ;
    {$ENDIF}
    FSQLDialectList.Text := GetSQLDialect(
                               UpperCase(
                                  FSQLParameters.Values[
                                    GIS_INI_LAYERSQL_DIALECT
                                  ]
                               )
                             ) ;
    FCurrTable     := FSQLParameters.Values[ GIS_INI_LAYERSQL_LAYER    ] ;
    isReadOnly     := ParamBoolean(
                        FSQLParameters.Values[ GIS_INI_LAYERSQL_READONLY ],
                        False
                      ) ;
    if isReadOnly then
      FSubType := FSubType - [ TGIS_LayerSubType.Exportable ] ;

    if FSQLParameters.Values[GIS_INI_LAYERSQL_CONNECTOR_FIREDAC] = 'Ora' then begin
      FSQLDialectList.Values[':WKB_GEOMETRY'] := '' ;
      FSQLDialectList.Values[':GEOMETRY'] := '' ;
      FSQLDialectList.Values[':CELL'] := '' ;
    end ;

    if _prepare then begin

      if FSQLCommands.Count = 0 then
        prepareCommandList ;

    end;
    oGisDb.sqlInitialize( FSQLParameters, FSQLDialectList );

  end ;

  procedure TGIS_LayerPixelSqlAbstract.prepareCommandList ;
  begin

  end ;

//==================================== END =====================================
end.

