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
  List of Shared connections. For internal use.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoSharedConnections ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoSharedConnections"'}
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
    TatukGIS.RTL,
    {$IFNDEF GIS_NOADO}
      ADODB,
    {$ENDIF}
    System.Data.Common ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.SysUtils,
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    System.SyncObjs,
    {$IFNDEF GIS_NOADO}
      {$IFDEF ADOINTUNIT}
        AdoInt,
      {$ELSE}
        Lider.CG.GIS.GeoAdoInt,
      {$ENDIF}
    {$ENDIF}
    {$IFNDEF GIS_NODB}
      Data.SqlExpr,
      Data.DB,
      {$IFDEF LEVEL_XE5_RTL}
        FireDAC.Comp.Client,
      {$ENDIF}
    {$ENDIF}

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypes ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    java.sql,
    tatukgis.rtl ;
{$ENDIF}

type

  /// <summary>
  ///   Abstract class for internal use only.
  /// </summary>
  /// <remarks>
  ///   Used to provide a shared connection for any database. Other classes
  ///   should inherit from this class and provide required methods.
  /// </remarks>
  TGIS_AnyConnection = {$IFDEF OXYGENE} public abstract {$ENDIF}
                       class( TGIS_ObjectDisposable )

    {$IFDEF OXYGENE} unit or protected {$ELSE} protected {$ENDIF}
      /// <summary>
      ///   Server name.
      /// </summary>
      Server   : String ;
      /// <summary>
      ///   User name.
      /// </summary>
      User     : String ;
      /// <summary>
      ///   Password.
      /// </summary>
      Password : String ;
      /// <summary>
      ///   Schema definition.
      /// </summary>
      Schema   : String ;
      {$IFDEF VCL7}
        /// <summary>
        ///   Parameters list.
        /// </summary>
        Params  : TStrings ;
      {$ELSE}
        /// <summary>
        ///   Parameters list.
        /// </summary>
        &Params : TStrings ;
      {$ENDIF}
    protected
      procedure doDestroy ; override;

    public

      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ; virtual;

      /// <summary>
      ///   Opens the connection.
      /// </summary>
      procedure Open ; virtual; abstract;

      /// <summary>
      ///   Closes the connection.
      /// </summary>
      procedure Close ; virtual; abstract;
  end ;

  TGIS_AnyConnectionAbstract = class of TGIS_AnyConnection ;

  /// <summary>
  ///   Class for internal use only.
  /// </summary>
  /// <remarks>
  ///   Used to provide a shared connection pool for ADO and DBX connections.
  /// </remarks>
  TGIS_SharedConnections = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectList
                                                                 {$IFDEF OXYGENE}
                                                                   , IDisposable
                                                                 {$ENDIF}
                                                               )
    {$IFDEF OXYGENE}

      protected

        /// <summary>
        ///   Destroy an instance.
        /// </summary>
      procedure Dispose ; override;
    {$ELSE}

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      destructor Destroy ; override;
    {$ENDIF}

    public

      {$IFNDEF GIS_NOADO}

        /// <summary>
        ///   Open ADO connection.
        /// </summary>
        /// <param name="_connect_string">
        ///   ADO connection.Open parameter
        /// </param>
        /// <param name="_user_name">
        ///   ADO connection.Open parameter
        /// </param>
        /// <param name="_password">
        ///   ADO connection.Open parameter
        /// </param>
        /// <param name="_options">
        ///   ADO connection.Open parameter
        /// </param>
        /// <param name="_params">
        ///   ADO connection.Open parameter
        /// </param>
        /// <returns>
        ///   prepared connection
        /// </returns>
        function  OpenADO ( const _connect_string  : String     ;
                            const _user_name       : String     ;
                            const _password        : String     ;
                            const _options         : Integer    ;
                            const _params          : {$IFDEF OXYGENE}
                                                       TGIS_Strings
                                                     {$ELSE}
                                                       TStrings
                                                     {$ENDIF}
                          ) : _Connection ;

        /// <summary>
        ///   Close ADO connection.
        /// </summary>
        /// <param name="_conn">
        ///   connection to be closed; if is referenced more than once the
        ///   reference will be decremented; if it is referenced 0 times then
        ///   connection will be closed
        /// </param>
        procedure CloseADO( const _conn            : _Connection
                          ) ;
      {$ENDIF}
      {$IFNDEF GIS_NODB}

        /// <summary>
        ///   Open DBX connection.
        /// </summary>
        /// <param name="_driver_name">
        ///   DBX connection.Open parameter
        /// </param>
        /// <param name="_get_driver_func">
        ///   DBX connection.Open parameter
        /// </param>
        /// <param name="_vendor_lib">
        ///   DBX connection.Open parameter
        /// </param>
        /// <param name="_library_name">
        ///   DBX connection.Open parameter
        /// </param>
        /// <param name="_login_prompt">
        ///   DBX connection.Open parameter
        /// </param>
        /// <param name="_params">
        ///   DBX connection.Open parameter
        /// </param>
        /// <returns>
        ///   reference to existing connection or fresh new connection
        /// </returns>
        function OpenDBX( const _driver_name     : String     ;
                          const _get_driver_func : String     ;
                          const _vendor_lib      : String     ;
                          const _library_name    : String     ;
                          const _login_prompt    : Boolean    ;
                          const _params          : {$IFDEF OXYGENE}
                                                     TGIS_Strings
                                                   {$ELSE}
                                                     TStrings
                                                   {$ENDIF}
                        ) : TSQLConnection ;

        /// <summary>
        ///   Close DBX connection.
        /// </summary>
        /// <param name="_conn">
        ///   connection to be closed; if is referenced more then once the
        ///   reference will be decremented; if it is referenced 0 times then
        ///   connection will be closed
        /// </param>
        procedure CloseDBX( const _conn            : TSQLConnection
                          ) ;

        {$IFDEF LEVEL_XE5_RTL}
        /// <summary>
        ///   Open FireDAC connection.
        /// </summary>
        /// <param name="_driverID">
        ///   FireDAC connection.Open parameter
        /// </param>
        /// <param name="_login_prompt">
        ///   FireDAC connection.Open parameter
        /// </param>
        /// <param name="_params">
        ///   FireDAC connection.Open parameter
        /// </param>
        /// <returns>
        ///   reference to existing connection or fresh new connection
        /// </returns>
        function OpenFireDAC( const _driverID        : String     ;
                              const _login_prompt    : Boolean    ;
                              const _params          : {$IFDEF OXYGENE}
                                                         TGIS_Strings
                                                       {$ELSE}
                                                         TStrings
                                                       {$ENDIF}
                            ) : TFDConnection ;

        /// <summary>
        ///   Close FireDAC connection.
        /// </summary>
        /// <param name="_conn">
        ///   connection to be closed; if is referenced more then once the
        ///   reference will be decremented; if it is referenced 0 times then
        ///   connection will be closed
        /// </param>
        procedure CloseFireDAC( const _conn          : TFDConnection
                              ) ;
      {$ENDIF}
      {$ENDIF}

      /// <summary>
      ///   Open any connection type like OCI, LibPQ, Sqlite, FGDB.
      /// </summary>
      /// <param name="_user_name">
      ///   user name
      /// </param>
      /// <param name="_password">
      ///   user password
      /// </param>
      /// <param name="_database">
      ///   database
      /// </param>
      /// <param name="_schema">
      ///   schema name
      /// </param>
      /// <param name="_params">
      ///   additional connection parameters
      /// </param>
      /// <param name="_class">
      ///   connection class reference
      /// </param>
      /// <returns>
      ///   new or existing connection handle
      /// </returns>
      function OpenAny( const _user_name       : String     ;
                        const _password        : String     ;
                        const _database        : String     ;
                        const _schema          : String     ;
                        const _params          : {$IFDEF OXYGENE}
                                                   TGIS_Strings ;
                                                 {$ELSE}
                                                   TStrings ;
                                                 {$ENDIF}
                        const _class           : TGIS_AnyConnectionAbstract
                      ) : TGIS_AnyConnection ;

      /// <summary>
      ///   Close any connection.
      /// </summary>
      /// <param name="_conn">
      ///   connection handle
      /// </param>
      procedure CloseAny( const _conn            : TGIS_AnyConnection
                        ) ;

      {$IFDEF CLR}
        /// <summary>
        ///   Open ADO .NET connection.
        /// </summary>
        /// <param name="_connectString">
        ///   database connect string
        /// </param>
        /// <param name="_provider">
        ///   .NET Framework data provider (InvariantName from DbProviderFactories)
        /// </param>
        /// <param name="_params">
        ///   additional connection parameters
        /// </param>
        /// <returns>
        ///   new or existing connection handle
        /// </returns>
        function OpenADONET( const _connectString  : String     ;
                             const _provider       : String     ;
                             const _params         : TGIS_Strings
                           ) : DbConnection ;

        /// <summary>
        ///   Close ADO .NET connection.
        /// </summary>
        /// <param name="_conn">
        ///   connection handle
        /// </param>
        procedure CloseADONET( const _conn           : DbConnection
                             )  ;
      {$ENDIF}
      {$IFDEF JAVA}

        /// <summary>
        ///   Open JDBC connection.
        /// </summary>
        /// <param name="_connectString">
        ///   database connect string in jdbc format
        /// </param>
        /// <param name="_driver">
        ///   JDBC driver class name to register
        /// </param>
        /// <param name="_userName">
        ///   JDBC getConnection parameter
        /// </param>
        /// <param name="_password">
        ///   JDBC getConnection parameter
        /// </param>
        /// <param name="_params">
        ///   additional connection parameters
        /// </param>
        /// <returns>
        ///   new or existing connection handle
        /// </returns>
        function OpenJDBC   ( const _connectString  : String     ;
                              const _driver         : String     ;
                              const _userName       : String     ;
                              const _password       : String     ;
                              const _params         : TGIS_Strings
                            ) : Connection ;

        /// <summary>
        ///   Close any connection.
        /// </summary>
        /// <param name="_conn">
        ///   connection handle
        /// </param>
        procedure CloseJDBC ( const _conn           : Connection
                            )  ;
      {$ENDIF}
  end ;


  /// <summary>
  ///   Shared connections.
  /// </summary>
  /// <returns>
  ///   handle to shared connections object
  /// </returns>
  function SharedConnections : TGIS_SharedConnections ;


//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoLogger,
    Lider.CG.GIS.GeoParams,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoClasses ;
{$ENDIF}

const
  GIS_CONNECTION_JET_OLEDB  = 'Microsoft.Jet.OLEDB' ;
  GIS_CONNECTION_DATASOURCE = 'Data Source' ;

var
  {$IFNDEF OXYGENE}
    oSharedConnections  : TGIS_SharedConnections ;
    oCrtSctShrdCnctns   : TCriticalSection       ;
  {$ELSE}
    oSharedConnections  : TGIS_SharedConnections := nil ;
    oCrtSctShrdCnctns   : TCriticalSection       := nil ;
  {$ENDIF}
  {$IFDEF ANDROID}
    oSCUponDestroy : Boolean ;
  {$ENDIF}

  function SharedConnections : TGIS_SharedConnections ;
  begin
    if not assigned( oSharedConnections ) then
      oSharedConnections := TGIS_SharedConnections.Create ;
    Result := oSharedConnections ;
  end ;

  function CrtSctShrdCnctns : TCriticalSection ;
  begin
    if not assigned( oCrtSctShrdCnctns ) then
      oCrtSctShrdCnctns := TCriticalSection.Create ;
    Result := oCrtSctShrdCnctns ;
  end ;

type
  // Referenced counted connection.
  T_sharedConnection = class( TGIS_ObjectDisposable )
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      sName     : String ;
      iRef      : Integer ;
      {$IFNDEF GIS_NOADO}
        {$IFDEF DCC}
          oADO    : _Connection ;
        {$ELSE}
          oADO    : TObject ;
        {$ENDIF}
      {$ENDIF}
      {$IFNDEF GIS_NODB}
        oDBX      : TSQLConnection ;
        {$IFDEF LEVEL_XE5_RTL}
          oFIREDAC  : TFDConnection ;
        {$ENDIF}
      {$ENDIF}
        oAny    : TGIS_AnyConnection ;
      {$IFDEF CLR}
        oADONET : DbConnection ;
      {$ENDIF}
      {$IFDEF JAVA}
        oJDBC : Connection ;
      {$ENDIF}
    protected
      procedure doDestroy ; override;
    public
      constructor Create;
  end ;

  constructor T_sharedConnection.Create ;
  begin
   {$IFNDEF GIS_NOADO}
     oADO       := nil ;
   {$ENDIF}
    {$IFNDEF GIS_NODB}
      oDBX      := nil ;
      {$IFDEF LEVEL_XE5_RTL}
        oFIREDAC  := nil ;
      {$ENDIF}
    {$ENDIF}
      oAny      := nil ;
    {$IFDEF CLR}
      oADONET   := nil ;
    {$ENDIF}
  end ;


//==============================================================================
// T_sharedConnection
//==============================================================================

  {$IFNDEF GIS_NOADO}
    procedure closeADO(
      {$IFDEF DCC}
      oADO : _Connection
    {$ELSE}
        oADO : TObject
      {$ENDIF}
    ) ;
    begin
      if _Connection(oADO).State <> 0 then
        _Connection(oADO).Close ;
      oADO := nil ;
    end;
  {$ENDIF}

  // Destroy an instance.
  procedure T_sharedConnection.doDestroy ;
  begin
    {$IFDEF ANDROID}
      if oSCUponDestroy then exit ;
    {$ENDIF}

    {$IFNDEF GIS_NOADO}
      if assigned( oADO ) then begin
        closeADO( oADO ) ;
        oADO := nil ;
      end ;
    {$ENDIF}
    {$IFNDEF GIS_NODB}
      if assigned( oDBX ) then begin
        oDBX.Close ;
        FreeObject( oDBX ) ;
      end ;
      {$IFDEF LEVEL_XE5_RTL}
        if assigned( oFIREDAC ) then begin
          oFIREDAC.Close ;
          FreeObject( oFIREDAC ) ;
        end ;
      {$ENDIF}
    {$ENDIF}
    if assigned( oAny ) then begin
      oAny.Close ;
      FreeObject( oAny ) ;
    end ;
    {$IFDEF CLR}
      if assigned( oADONET ) then begin
        oADONET.Close ;
        FreeObject( oADONET ) ;
      end ;
    {$ENDIF}
    {$IFDEF JAVA}
      if assigned( oJDBC ) then begin
        try
          oJDBC.close ;
        except
        end;
        FreeObject( oJDBC ) ;
      end ;
    {$ENDIF}

    inherited ;
  end ;

//==============================================================================
// TGIS_SharedConnections
//==============================================================================

  {$IFDEF OXYGENE}
    procedure TGIS_SharedConnections.Dispose ;
  {$ELSE}
    destructor TGIS_SharedConnections.Destroy ;
  {$ENDIF}
  begin
    {$IFDEF CLR}
      GC.SuppressFinalize( self ) ;
    {$ELSE}
      inherited ;
    {$ENDIF}
  end ;

{$IFNDEF GIS_NOADO}

  {$IFDEF OXYGENE}
    function TGIS_SharedConnections.OpenADO(
      const _connect_string : String ;
      const _user_name      : String ;
      const _password       : String ;
      const _options        : Integer ;
      const _params         : TGIS_Strings
  {$ELSE}
    function TGIS_SharedConnections.OpenADO(
      const _connect_string : String ;
      const _user_name      : String ;
      const _password       : String ;
      const _options        : Integer ;
      const _params         : TStrings
  {$ENDIF}
  ) : _Connection ;
  var
    i    : Integer            ;
    obj  : T_sharedConnection ;
    str  : String             ;
    fnd  : Boolean            ;
    fnc  : Boolean            ;
  begin
    CrtSctShrdCnctns.Enter ;
    try
      Result := nil ;

      str := UpperCase( _connect_string ) + ';' +
             _params.Values[ GIS_INI_LAYERSQL_CONNECTION_POOL_ID ] ;
      fnc := ParamBoolean( _params.Values[GIS_INI_LAYERSQL_FORCE_NEW_CONNECTION], False ) ;
      fnd := False ;
      for i:=0 to Count -1 do begin
        {$IFDEF CLR}
          obj := T_sharedConnection( Items[i] ) ;
        {$ELSE}
          obj := T_sharedConnection( Items[i] ) ;
        {$ENDIF}
        assert( assigned( obj ) ) ;
        if obj.sName = str then begin
          if not fnc then begin
            inc( obj.iRef ) ;
            fnd := True  ;
          end ;
          Result := _Connection(obj.oADO) ;
          break ;
        end ;
      end ;

      if not fnd then begin
        obj := T_sharedConnection.Create ;
        obj.sName := str ;
        obj.iRef  := 1   ;

        {$IFDEF CLR}
          {$IFNDEF OXYGENE}
            obj.oADO := CoConnection._Create ;
          {$ELSE}
            obj.oADO := new ConnectionClass ;
          {$ENDIF}
        {$ELSE}
          obj.oADO := CoConnection.Create ;
        {$ENDIF}
        try
          _Connection(obj.oADO).Open( _connect_string, _user_name, _password, _options )  ;
          Result := _Connection(obj.oADO) ;
          Add( obj ) ;
        except
          Result := nil ;
          FreeObject( obj ) ;
          TGIS_Logger.AsTrace( 'SharedConnections.OpenADO', _connect_string ) ;
          raise ;
        end ;
      end ;

    finally
      CrtSctShrdCnctns.Leave ;
    end ;
  end ;
{$ENDIF}

{$IFNDEF GIS_NOADO}

  procedure TGIS_SharedConnections.CloseADO( const _conn : _Connection ) ;
  var
    i   : Integer            ;
    obj : T_sharedConnection ;
  begin
    if not assigned( oCrtSctShrdCnctns ) then exit ;

    CrtSctShrdCnctns.Enter ;
    try

      if not assigned( _conn ) then exit ;

      for i:=0 to Count -1 do begin
        {$IFDEF CLR}
          obj := T_sharedConnection( Items[i] ) ;
        {$ELSE}
          obj := T_sharedConnection( Items[i] ) ;
        {$ENDIF}
        assert( assigned( obj ) ) ;
        if obj.oADO = _conn then begin
          dec( obj.iRef ) ;
          if obj.iRef <= 0 then begin
            {$IFDEF OXYGENE}
              FreeObject( obj ) ;
            {$ENDIF}
            Delete( i ) ;
          end ;
          break ;
        end ;
      end ;

    finally
      CrtSctShrdCnctns.Leave ;
    end ;
  end ;
{$ENDIF}

{$IFNDEF GIS_NODB}
  {$IFDEF OXYGENE}
    function TGIS_SharedConnections.OpenDBX(
      const _driver_name     : String ;
      const _get_driver_func : String ;
      const _vendor_lib      : String ;
      const _library_name    : String ;
      const _login_prompt    : Boolean ;
      const _params          : TGIS_Strings
    ) : TSQLConnection ;
  {$ELSE}
    function TGIS_SharedConnections.OpenDBX(
      const _driver_name     : String ;
      const _get_driver_func : String ;
      const _vendor_lib      : String ;
      const _library_name    : String ;
      const _login_prompt    : Boolean ;
      const _params          : TStrings
    ) : TSQLConnection ;
  {$ENDIF}
  var
    i    : Integer            ;
    obj  : T_sharedConnection ;
    str  : String             ;
    fnd  : Boolean            ;
    fnc  : Boolean            ;
  begin
    CrtSctShrdCnctns.Enter ;
    try
      Result := nil ;

      str := Uppercase( Trim( _driver_name       ) + ';' +
                        Trim( _get_driver_func   ) + ';' +
                        Trim( _vendor_lib        ) + ';' +
                        Trim( _library_name      ) + ';' +
                        BoolToStr( _login_prompt ) + ';' +
                        Trim( _params.Text       ) + ';' +
                        _params.Values[ GIS_INI_LAYERSQL_CONNECTION_POOL_ID ]
                      ) ;
      fnc := ParamBoolean( _params.Values[GIS_INI_LAYERSQL_FORCE_NEW_CONNECTION], False ) ;
      fnd := False ;
      for i:=0 to Count -1 do begin
        {$IFDEF CLR}
          obj := T_SharedConnection( Items[i] ) ;
        {$ELSE}
          obj := T_sharedConnection( Items[i] ) ;
        {$ENDIF}
        Assert( assigned( obj ) ) ;
        if obj.sName = str then begin
          if not fnc then begin
            Inc( obj.iRef ) ;
            fnd := True  ;
          end ;
          Result := obj.oDBX ;
          break ;
        end ;
      end ;

      if not fnd then begin
        obj := T_sharedConnection.Create ;
        obj.sName := str ;
        obj.iRef  := 1   ;

        obj.oDBX := TSQLConnection.Create( nil );
        try
          obj.oDBX.DriverName    := _driver_name     ;
          obj.oDBX.GetDriverFunc := _get_driver_func ;
          obj.oDBX.VendorLib     := _vendor_lib      ;
          obj.oDBX.LibraryName   := _library_name    ;
          obj.oDBX.LoginPrompt   := _login_prompt    ;
          for i:=0 to _params.Count - 1 do begin
            obj.oDBX.Params.Values[ _params.Names[i] ] :=
              _params.Values[ _params.Names[i] ] ;
          end ;
          obj.oDBX.Open ;
          Result := obj.oDBX ;
          Add( obj ) ;
        except
          Result := nil ;
          FreeObject( obj ) ;
          raise ;
        end ;
      end ;

    finally
      CrtSctShrdCnctns.Leave ;
    end ;
  end ;
{$ENDIF}

{$IFNDEF GIS_NODB}

  procedure TGIS_SharedConnections.CloseDBX( const _conn : TSQLConnection ) ;
  var
    i   : Integer            ;
    obj : T_sharedConnection ;
  begin
    if not assigned( oCrtSctShrdCnctns ) then exit ;

    CrtSctShrdCnctns.Enter ;
    try
      if not assigned( _conn ) then exit ;

      for i:=0 to Count -1 do begin
        {$IFDEF CLR}
          obj := T_SharedConnection( Items[i] ) ;
        {$ELSE}
          obj := T_sharedConnection( Items[i] ) ;
        {$ENDIF}
        Assert( assigned( obj ) ) ;
        if obj.oDBX = _conn then begin
          Dec( obj.iRef ) ;
          if obj.iRef <= 0 then begin
            Delete( i ) ;
          end ;
          break ;
        end ;
      end ;
    finally
      CrtSctShrdCnctns.Leave ;
    end ;
  end ;
{$ENDIF}

{$IFNDEF GIS_NODB}
{$IFDEF LEVEL_XE5_RTL}
  {$IFDEF OXYGENE}
    function TGIS_SharedConnections.OpenFireDAC(
      const _driverID     : String  ;
      const _login_prompt : Boolean ;
      const _params       : TGIS_Strings
    ) : TFDConnection ;
  {$ELSE}
    function TGIS_SharedConnections.OpenFireDAC(
      const _driverID     : String  ;
      const _login_prompt : Boolean ;
      const _params       : TStrings
    ) : TFDConnection ;
  {$ENDIF}
  var
    i    : Integer            ;
    obj  : T_sharedConnection ;
    str  : String             ;
    fnd  : Boolean            ;
  begin
    CrtSctShrdCnctns.Enter ;
    try

      Result := nil ;

      str := Uppercase( Trim( _driverID          ) + ';' +
                        BoolToStr( _login_prompt ) + ';' +
                        Trim( _params.Text       )
                      ) ;

      fnd := False ;
      for i:=0 to Count -1 do begin
        {$IFDEF CLR}
          obj := T_SharedConnection( Items[i] ) ;
        {$ELSE}
          obj := T_sharedConnection( Items[i] ) ;
        {$ENDIF}
        Assert( assigned( obj ) ) ;
        if obj.sName = str then begin
          Inc( obj.iRef ) ;
          fnd    := True  ;
          Result := obj.oFireDAC ;
          break ;
        end ;
      end ;

      if not fnd then begin
        obj := T_sharedConnection.Create ;
        obj.sName := str ;
        obj.iRef  := 1   ;

        obj.oFireDAC := TFDConnection.Create( nil );
        try
          obj.oFireDAC.DriverName    := _driverID     ;
          obj.oFireDAC.LoginPrompt   := _login_prompt    ;
          for i:=0 to _params.Count - 1 do begin
            obj.oFireDAC.Params.Values[ _params.Names[i] ] :=
              _params.Values[ _params.Names[i] ] ;
          end ;
          obj.oFireDAC.Open ;
          Result := obj.oFireDAC ;
          Add( obj ) ;
        except
          Result := nil ;
          FreeObject( obj ) ;
          raise ;
        end ;
      end ;

    finally
      CrtSctShrdCnctns.Leave ;
    end ;
  end ;
{$ENDIF}
{$ENDIF}

{$IFNDEF GIS_NODB}
{$IFDEF LEVEL_XE5_RTL}
  procedure TGIS_SharedConnections.CloseFireDAC( const _conn : TFDConnection ) ;
  var
    i   : Integer            ;
    obj : T_sharedConnection ;
  begin
    if not assigned( oCrtSctShrdCnctns ) then exit ;

    CrtSctShrdCnctns.Enter ;
    try
      if not assigned( _conn ) then exit ;

      for i:=0 to Count -1 do begin
        {$IFDEF CLR}
          obj := T_SharedConnection( Items[i] ) ;
        {$ELSE}
          obj := T_sharedConnection( Items[i] ) ;
        {$ENDIF}
        Assert( assigned( obj ) ) ;
        if obj.oFireDAC = _conn then begin
          Dec( obj.iRef ) ;
          if obj.iRef <= 0 then begin
            Delete( i ) ;
          end ;
          break ;
        end ;
      end ;
    finally
      CrtSctShrdCnctns.Leave ;
    end ;
  end ;
{$ENDIF}
{$ENDIF}

  {$IFDEF OXYGENE}
    function TGIS_SharedConnections.OpenAny(
      const _user_name : String ;
      const _password  : String ;
      const _database  : String ;
      const _schema    : String ;
      const _params    : TGIS_Strings ;
      const _class     : TGIS_AnyConnectionAbstract
    ) : TGIS_AnyConnection ;
  {$ELSE}

    function TGIS_SharedConnections.OpenAny(
      const _user_name : String ;
      const _password  : String ;
      const _database  : String ;
      const _schema    : String ;
      const _params    : TStrings ;
      const _class     : TGIS_AnyConnectionAbstract
    ) : TGIS_AnyConnection ;
  {$ENDIF}
  var
    i    : Integer            ;
    obj  : T_sharedConnection ;
    str  : String             ;
    fnd  : Boolean            ;
    fnc  : Boolean            ;
  begin
    CrtSctShrdCnctns.Enter ;
    try
      Result := nil ;

      str := UpperCase( Trim( _user_name ) + ';' +
                        Trim( _password  ) + ';' +
                        Trim( _database  ) + ';' +
                        Trim( _schema    ) + ';' +
                        _params.Values[ GIS_INI_LAYERSQL_CONNECTION_POOL_ID ]
                      ) ;
      fnc := ParamBoolean( _params.Values[GIS_INI_LAYERSQL_FORCE_NEW_CONNECTION], False ) ;

      fnd := False ;
      for i := 0 to Count -1 do begin
        {$IFDEF CLR}
          obj := T_sharedConnection( Items[i] ) ;
        {$ELSE}
          obj := T_sharedConnection( Items[i] ) ;
        {$ENDIF}
        assert( assigned( obj ) ) ;
        if obj.sName = str then begin
          if not fnc then begin
            inc( obj.iRef ) ;
            fnd := True  ;
          end ;
          Result := obj.oAny ;
          break ;
        end ;
      end ;

      if not fnd then begin
        obj := T_sharedConnection.Create ;
        obj.sName := str ;
        obj.iRef  := 1   ;
        {$IFNDEF OXYGENE}
         obj.oAny           := _class.Create ;
        {$ELSE}
         obj.oAny           := _class.new ;
        {$ENDIF}
        obj.oAny.User      := _user_name ;
        obj.oAny.Password  := _password ;
        obj.oAny.Server    := _database ;
        obj.oAny.Schema    := _schema ;

        for i := 0 to _params.Count - 1 do begin
          obj.oAny.Params.Values[ _params.Names[i] ] :=
            _params.Values[ _params.Names[i] ] ;
        end ;

        try
          obj.oAny.Open ;
          Result := obj.oAny ;
          Add( obj ) ;
        except
          Result := nil ;
          FreeObject( obj ) ;
          raise ;
        end ;
      end ;
    finally
      CrtSctShrdCnctns.Leave ;
    end ;
  end ;

  procedure TGIS_SharedConnections.CloseAny(
    const _conn : TGIS_AnyConnection
  ) ;
  var
    i   : Integer            ;
    obj : T_sharedConnection ;
  begin
    if not assigned( oCrtSctShrdCnctns ) then exit ;

    CrtSctShrdCnctns.Enter ;
    try
      if not assigned( _conn ) then exit ;

      for i := 0 to Count-1 do begin
        {$IFDEF CLR}
          obj := T_sharedConnection( Items[i] ) ;
        {$ELSE}
          obj := T_sharedConnection( Items[i] ) ;
        {$ENDIF}
        assert( assigned( obj ) ) ;
        if obj.oAny = _conn then begin
          dec( obj.iRef ) ;
          if obj.iRef <= 0 then begin
            {$IFDEF OXYGENE}
              FreeObject( obj ) ;
            {$ENDIF}
            Delete( i ) ;
          end ;
          break ;
        end ;
      end ;
    finally
      CrtSctShrdCnctns.Leave ;
    end ;
  end ;

  {$IFDEF CLR}
    function TGIS_SharedConnections.OpenADONET(
      const _connectString : String ;
      const _provider      : String ;
      const _params        : TGIS_Strings
    ) : DbConnection ;
    var
      i    : Integer            ;
      obj  : T_sharedConnection ;
      str  : String             ;
      fnd  : Boolean            ;
      fnc  : Boolean            ;

      function createConnection : DbConnection ;
      var
        provider : DbProviderFactory ;
      begin
        Result := nil ;
        try
          {$IFDEF ADONET_FULL}
            provider := DbProviderFactories.GetFactory( _provider ) ;
          {$ELSE}
            provider := DbProviderFactories.GetFactory( _provider ) ;
          {$ENDIF}
          if provider <> nil then
            Result := provider.CreateConnection ;

          if Result <> nil then
            Result.ConnectionString := _connectString
        except
          on e : Exception do
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERSQLERROR ), e.Message, 0 ) ;
        end ;
      end ;

    begin
      CrtSctShrdCnctns.Enter ;
      try
        Result := nil ;

        str := UpperCase( Trim( _connectString ) + ';' +
                          Trim( _provider      ) + ';' +
                          _params.Values[ GIS_INI_LAYERSQL_CONNECTION_POOL_ID ] + ';'
                        ) ;
        fnc := ParamBoolean( _params.Values[GIS_INI_LAYERSQL_FORCE_NEW_CONNECTION], False ) ;
        fnd := False ;
        for i := 0 to Count -1 do begin
          obj := T_sharedConnection( Items[i] ) ;
          assert( assigned( obj ) ) ;
          if obj.sName = str then begin
            if not fnc then begin
              inc( obj.iRef ) ;
              fnd := True  ;
            end ;
            Result := obj.oADONET ;
            break ;
          end ;
        end ;

        if not fnd then begin
          obj := T_sharedConnection.Create ;
          obj.sName   := str ;
          obj.iRef    := 1   ;
          try
            obj.oADONET := createConnection ;
            if assigned( obj.oADONET ) then
              obj.oADONET.Open ;
            Result := obj.oADONET ;
            Add( obj ) ;
          except
            Result := nil ;
            FreeObject( obj ) ;
            TGIS_Logger.AsTrace( 'SharedConnections.OpenADONET', _connectString ) ;
            raise ;
          end ;
        end ;
      finally
        CrtSctShrdCnctns.Leave ;
      end ;
    end ;
  {$ENDIF}

  {$IFDEF CLR}
    procedure TGIS_SharedConnections.CloseADONET(
      const _conn : DbConnection
    ) ;
    var
      i   : Integer            ;
      obj : T_sharedConnection ;
    begin
      if not assigned( oCrtSctShrdCnctns ) then exit ;

      CrtSctShrdCnctns.Enter ;
      try
        if not assigned( _conn ) then exit ;

        for i := 0 to Count-1 do begin
          obj := T_sharedConnection( Items[i] ) ;
          assert( assigned( obj ) ) ;
          if obj.oADONET = _conn then begin
            dec( obj.iRef ) ;
            if obj.iRef <= 0 then begin
              {$IFDEF OXYGENE}
                FreeObject( obj ) ;
              {$ENDIF}
              Delete( i ) ;
            end ;
            break ;
          end ;
        end ;
      finally
        CrtSctShrdCnctns.Leave ;
      end ;
    end ;
  {$ENDIF}

  {$IFDEF JAVA}
    function TGIS_SharedConnections.OpenJDBC(
      const _connectString : String ;
      const _driver        : String ;
      const _userName      : String ;
      const _password      : String ;
      const _params        : TGIS_Strings
    ) : Connection ;
    var
      i    : Integer            ;
      obj  : T_sharedConnection ;
      str  : String             ;
      fnd  : Boolean            ;
      fnc  : Boolean            ;

      function createConnection : Connection ;
      begin
        Result := nil ;
        try
          // load and register the driver first
          &Class.forName( _driver ) ;
          { TODO -cReview : add rewrite_mdb and expand to absolute path with replacing ,with, }
          // add setTransactionIsolation
          Result := DriverManager.getConnection( _connectString, _userName, _password ) ;
        except
          on e : Exception do
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERSQLERROR ), e.Message, 0 ) ;
        end ;
      end ;

    begin
      CrtSctShrdCnctns.Enter ;
      try
        Result := nil ;

        str := UpperCase( Trim( _connectString ) + ';' +
                          Trim( _driver        ) + ';' +
                          _params.Values[ GIS_INI_LAYERSQL_CONNECTION_POOL_ID ] + ';'
                        ) ;
        fnd := False ;
        fnc := ParamBoolean( _params.Values[GIS_INI_LAYERSQL_FORCE_NEW_CONNECTION], False ) ;
        for i := 0 to Count -1 do begin
          obj := T_sharedConnection( Items[i] ) ;
          assert( assigned( obj ) ) ;
          if obj.sName = str then begin
            if not fnc then begin
              inc( obj.iRef ) ;
              fnd := True  ;
            end ;
            Result := obj.oJDBC ;
            break ;
          end ;
        end ;

        if not fnd then begin
          obj := T_sharedConnection.Create ;
          obj.sName   := str ;
          obj.iRef    := 1   ;
          try
            obj.oJDBC := createConnection ;
            Result := obj.oJDBC ;
            Add( obj ) ;
          except
            Result := nil ;
            FreeObject( obj ) ;
            raise ;
          end ;
        end ;
      finally
        CrtSctShrdCnctns.Leave ;
      end ;
    end ;

    procedure TGIS_SharedConnections.CloseJDBC(
      const _conn : Connection
    )  ;
    var
      i   : Integer            ;
      obj : T_sharedConnection ;
    begin
      if not assigned( oCrtSctShrdCnctns ) then exit ;

      CrtSctShrdCnctns.Enter ;
      try
        if not assigned( _conn ) then exit ;

        for i := 0 to Count-1 do begin
          obj := T_sharedConnection( Items[i] ) ;
          assert( assigned( obj ) ) ;
          if obj.oJDBC = _conn then begin
            dec( obj.iRef ) ;
            if obj.iRef <= 0 then begin
              FreeObject( obj ) ;
              Delete( i ) ;
            end ;
            break ;
          end ;
        end ;
      finally
        CrtSctShrdCnctns.Leave ;
      end ;
    end ;
  {$ENDIF}

//==============================================================================
// TGIS_AnyConnection
//==============================================================================

  constructor TGIS_AnyConnection.Create ;
  begin
    inherited ;

    Params := TStringList.Create ;
  end ;

  procedure TGIS_AnyConnection.doDestroy ;
  begin
    FreeObject( Params ) ;

    inherited ;
  end ;

{$IFNDEF OXYGENE}
  initialization
    oSharedConnections := nil ;
    oCrtSctShrdCnctns  := nil ;
    {$IFDEF ANDROID}
      oSCUponDestroy := False ;
    {$ENDIF}
  finalization
    {$IFDEF ANDROID}
      oSCUponDestroy := True ;
    {$ENDIF}
    if assigned( oSharedConnections ) then begin
                                        oSharedConnections.Free   ;
                                        oSharedConnections := nil ;
                                      end ;
    if assigned( oCrtSctShrdCnctns  ) then begin
                                        oCrtSctShrdCnctns.Free   ;
                                        oCrtSctShrdCnctns := nil ;
                                      end ;
{$ENDIF}


//==================================== END =====================================
end.
