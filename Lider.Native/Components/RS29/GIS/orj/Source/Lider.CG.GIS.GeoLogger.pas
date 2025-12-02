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
  Logger class.
}
{$IFDEF DCC}
  unit GisLogger;
  {$HPPEMIT '#pragma link "GisLogger"}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}
{$IFDEF ISLAND}
  namespace TatukGIS ;
{$ENDIF}

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL,
    System.IO ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}

    GisRtl ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    remobjects.elements.rtl.*,
    TatukGIS.RTL ;
{$ENDIF}

type

  /// <summary>
  ///   Type of logged event
  /// </summary>
  TGIS_LoggerType = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   log as an error
    /// </summary>
    Error,
    /// <summary>
    ///   log as a warning
    /// </summary>
    Warning,
    /// <summary>
    ///   log as a simple message
    /// </summary>
    &Message,
    /// <summary>
    ///   log as a trace
    /// </summary>
    Trace
  ) ;

  /// <summary>
  ///   Class responsible for logging. To be used for tracking program
  ///   execution flow upon debugging and in runtime environment.
  /// </summary>
  TGIS_Logger = {$IFDEF OXYGENE} public {$ENDIF} class
    private
      class function  fget_Active : Boolean ; static ;
      class procedure fset_Active ( const _value  : Boolean ) ; static ;
      class function  fget_Debug  : Boolean ; static ;
      class function  fget_Warnings : String ; static ;
      class function  fget_Errors   : String ; static ;
      class function  fget_Path   : String ; static ;
      class procedure fset_Path   ( const _value  : String ) ; static ;
    public

      /// <summary>
      ///   Log an error.
      /// </summary>
      /// <param name="_title">
      ///   title of event
      /// </param>
      /// <param name="_desc">
      ///   additional description
      /// </param>
      class procedure AsError  ( const _title : String ;
                                 const _desc  : String
                               ) ;

      /// <summary>
      ///   Log a warning.
      /// </summary>
      /// <param name="_title">
      ///   title of event
      /// </param>
      /// <param name="_desc">
      ///   additional description
      /// </param>
      class procedure AsWarning( const _title : String ;
                                 const _desc  : String
                               ) ;

      /// <summary>
      ///   Log a message.
      /// </summary>
      /// <param name="_title">
      ///   title of event
      /// </param>
      /// <param name="_desc">
      ///   additional description
      /// </param>
      class procedure AsMessage( const _title : String ;
                                 const _desc  : String
                               ) ;

      /// <summary>
      ///   Log a trace.
      /// </summary>
      /// <param name="_title">
      ///   title of event
      /// </param>
      /// <param name="_desc">
      ///   additional description
      /// </param>
      class procedure AsTrace  ( const _title : String ;
                                 const _desc  : String
                               ) ;

      /// <summary>
      ///   Get all warnings as text.
      /// </summary>
      class property  Warnings   : String read fget_Warnings ;

      /// <summary>
      ///   Get all errors as text.
      /// </summary>
      class property  Errors     : String read fget_Errors ;

      /// <summary>
      ///   True if logger is active.
      /// </summary>
      class property  Active : Boolean read fget_Active write fset_Active ;

      /// <summary>
      ///   Path for logger file.
      /// </summary>
      class property  Path : String read fget_Path write fset_Path ;

      /// <summary>
      ///   True if app is running in a debug mode.
      /// </summary>
      class property  Debug  : Boolean read fget_Debug ;
  end ;


const
   {#gendoc:hide}
   METADATA_LOGGER_PATH = 'TGIS_Logger.Path';

implementation

{$IFDEF DCC}
uses
  System.SyncObjs,
  GisFunctions,
  GisClasses ;
{$ENDIF}

type
  T_Logger  = class( TGIS_Object )
    private
      FPath       : String ;
      FStartTime  : Int64 ;
      FActive     : Boolean ;
    {$IFDEF OXYGENE} unit {$ELSE} private {$ENDIF}
      FWarnings   : TStringList ;
      FErrros     : TStringList ;
    private
      function  fget_Active     : Boolean ;
      procedure fset_Active     ( const _value  : Boolean
                                ) ;
      function  fget_Debug      : Boolean ;
      function  fget_IsConsole  : Boolean ;
    protected
      procedure  doDestroy      ; override;
    public
      constructor Create ;
      procedure Log         ( const _type  : TGIS_LoggerType ;
                              const _title : String ;
                              const _desc  : String
                            ) ;
    public
      property  Active      : Boolean read  fget_Active
                                      write fset_Active ;
      property  Path        : String  read  FPath
                                      write FPath ;
      property  Debug       : Boolean read  fget_Debug ;
      property  IsConsole   : Boolean read  fget_IsConsole ;
  end ;

var
  GisLoggerObj : T_Logger ;


constructor T_Logger.Create ;
begin
  FPath       := '' ;
  FStartTime  := GetTickCount  ;
  FActive     := False ;

  FWarnings   := TStringList.Create ;
  FErrros     := TStringList.Create ;

  Log( TGIS_LoggerType.Trace, 'Logger registered', '' ) ;
end ;

procedure T_Logger.doDestroy ;
begin
  FreeObject( FWarnings   ) ;
  FreeObject( FErrros     ) ;

  inherited;
end;

function T_Logger.fget_Active : Boolean ;
begin
  Result := FActive ;
end ;

procedure T_Logger.fset_Active(
  const _value : Boolean
) ;
begin
  FActive := _value ;
  if not FActive then begin
    FWarnings.Clear ;
    FErrros.Clear ;
  end ;
end ;

function T_Logger.fget_Debug : Boolean ;
begin
  {$IFDEF DCC}
    {$WARN SYMBOL_PLATFORM OFF}
      Result := DebugHook <> 0 ;
      if Result then Exit ;
    {$WARN SYMBOL_PLATFORM ON}
  {$ENDIF}
  {$IFDEF CLR}
    Result := System.Diagnostics.Debugger.IsAttached ;
  {$ENDIF}
  {$IFDEF JAVA}
    {$IFNDEF ANDROID}
      Result := java.lang.management.ManagementFactory
                  .getRuntimeMXBean().getInputArguments().toString()
                  .indexOf( 'jdwp' ) > 0 ;
    {$else}
      //?Result := BuildConfig.DEBUG ;
      Result := False ;
    {$ENDIF}
  {$ENDIF}
end ;

function T_Logger.fget_IsConsole : Boolean ;
{$IFDEF DCC}
  var
    stdout : THandle ;
{$ENDIF}
begin
  {$IFDEF DCC}
    {$IFDEF MSWINDOWS}
      stdout := GetStdHandle( STD_OUTPUT_HANDLE ) ;
      Win32Check( stdout <> INVALID_HANDLE_VALUE ) ;
      Result := stdout <> 0;
    {$ELSE}
      Result := False ;
    {$ENDIF}
  {$ENDIF}
  {$IFDEF CLR}
    Result := False ;
  {$ENDIF}
  {$IFDEF JAVA}
    Result := False ;
  {$ENDIF}
end ;

procedure T_Logger.Log(
  const _type  : TGIS_LoggerType;
  const _title : String ;
  const _desc  : String
) ;
var
  msg_type : String ;
  msg      : String ;

  function prepare_text : String ;
  begin
    Result := Format(
                '%s %s %s (%.3f s)',
                [ msg_type,
                  _title,
                  _desc,
                  ( GetTickCount - FStartTime ) / 1000
                ]
              ) ;
  end ;

  function prepare_simple_text : String ;
  begin
    Result := Format(
                '%s %s %s',
                [ msg_type, _title + #10#13, _desc ]
              ) ;
  end ;

  procedure log_to_file(
    const _path  : String ;
    const _msg   : String
  ) ;
  var
    str    : String   ;
    {$IFDEF DCC}
      flog  : TextFile ;
    {$ENDIF}
    {$IFDEF JAVA}
      fapp  : Boolean ;
    {$ENDIF}
    thd : TGIS_ThreadClass ;
  begin
    thd := TGIS_ThreadClass.Create ;
    thd.LockThread ;
    try
      str := Format( '%s %s', [ FormatDateTime( 'yy-MM-dd hh:nn:ss', Now ),
                                _msg
                              ]
                   ) ;
      {$IFDEF DCC}
        AssignFile( flog, _path ) ;
        try
          try
            if FileExists( _path ) then
              Append( flog )
            else begin
              Rewrite( flog ) ;
            end ;
            Writeln( flog, str ) ;
          except
          end ;
        finally
          try
            CloseFile( flog ) ;
          except
          end ;
        end ;
      {$ENDIF}
      {$IFDEF CLR}
        try
          if FileExists( _path ) then begin
            using flog := File.AppendText( _path ) do begin
              flog.WriteLine( str ) ;
            end ;
          end
          else begin
            using flog := new StreamWriter( _path ) do begin
              flog.WriteLine( str ) ;
            end ;
          end ;
        except
        end ;
      {$ENDIF}
      {$IFDEF JAVA}
        try
          fapp := FileExists( _path ) ;
          using flog := new java.io.FileWriter( _path, fapp ) do begin
            flog.write( str ) ;
            flog.write( System.lineSeparator ) ;
            flog.flush ;
            flog.close ;
          end ;
        except
        end ;        
      {$ENDIF}
    finally
      thd.UnlockThread ;
      FreeObject( thd ) ;
    end ;
  end ;

begin
  case _type of
    TGIS_LoggerType.Error    :
      begin
        msg_type := 'Error:' ;
        msg := prepare_simple_text ;
        FErrros.Add( msg ) ;
      end ;
    TGIS_LoggerType.Warning  :
      begin
        msg_type := 'Warning:' ;
        msg := prepare_simple_text ;
        FWarnings.Add( msg ) ;
      end ;
    TGIS_LoggerType.Message :
      begin
        msg_type := '' ;
        msg := prepare_simple_text ;
      end ;
    TGIS_LoggerType.Trace    :
      begin
        msg_type := 'Trace:' ;
      end ;
    else
      begin
        msg := '' ;
      end ;
  end ;

  msg := prepare_text ;

  if IsStringEmpty( FPath ) and GisMetadataAssigned then
    FPath := GisMetadataAsString( METADATA_LOGGER_PATH, ''  ) ;
  if not IsStringEmpty( FPath ) then
    log_to_file( FPath, msg ) ;

  if ( not Active ) and ( not Debug ) then exit ;

  {$IFDEF MSWINDOWS}
    {$IFNDEF CLR}
      OutputDebugString( PChar(msg) ) ;
      if IsConsole then
        Writeln( msg ) ;
    {$ELSE}
      System.Diagnostics.Debug.WriteLine( msg ) ;
    {$ENDIF}
  {$ELSE}
    { TODO : Support for mobile }
  {$ENDIF}
end ;

class procedure TGIS_Logger.AsError(
  const _title : String ;
  const _desc  : String
) ;
begin
  if not assigned( GisLoggerObj ) then
    GisLoggerObj := T_Logger.Create ;

  GisLoggerObj.Log( TGIS_LoggerType.Error, _title, _desc ) ;
end ;

class procedure TGIS_Logger.AsMessage(
  const _title : String ;
  const _desc  : String
) ;
begin
  if not assigned( GisLoggerObj ) then
    GisLoggerObj := T_Logger.Create ;

  GisLoggerObj.Log( TGIS_LoggerType.Message, _title, _desc ) ;
end ;

class procedure TGIS_Logger.AsWarning(
  const _title : String ;
  const _desc  : String
) ;
begin
  if not assigned( GisLoggerObj ) then
    GisLoggerObj := T_Logger.Create ;

  GisLoggerObj.Log( TGIS_LoggerType.Warning, _title, _desc ) ;
end ;

class procedure TGIS_Logger.AsTrace(
  const _title : String ;
  const _desc  : String
) ;
begin
  if not assigned( GisLoggerObj ) then
    GisLoggerObj := T_Logger.Create ;

  GisLoggerObj.Log( TGIS_LoggerType.Trace, _title, _desc ) ;
end ;


class function TGIS_Logger.fget_Active : Boolean ;
begin
  if not assigned( GisLoggerObj ) then
    GisLoggerObj := T_Logger.Create ;

  Result := GisLoggerObj.Active ;
end ;

class function TGIS_Logger.fget_Debug : Boolean ;
begin
  if not assigned( GisLoggerObj ) then
    GisLoggerObj := T_Logger.Create ;

  Result := GisLoggerObj.Debug ;
end ;

class procedure TGIS_Logger.fset_Active(
  const _value : Boolean
) ;
begin
  if not assigned( GisLoggerObj ) then
    GisLoggerObj := T_Logger.Create ;

  GisLoggerObj.Active := _value ;
end ;

class function TGIS_Logger.fget_Warnings : String ;
begin
  if not assigned( GisLoggerObj ) then
    GisLoggerObj := T_Logger.Create ;

  Result := GisLoggerObj.FWarnings.Text ;
end ;

class function TGIS_Logger.fget_Errors : String ;
begin
  if not assigned( GisLoggerObj ) then
    GisLoggerObj := T_Logger.Create ;

  Result := GisLoggerObj.FErrros.Text ;
end ;

class function TGIS_Logger.fget_Path : String ;
begin
  if not assigned( GisLoggerObj ) then
    GisLoggerObj := T_Logger.Create ;

  Result := GisLoggerObj.Path ;
end ;

class procedure TGIS_Logger.fset_Path(
  const _value : String
) ;
var
  s_path : String ;
begin
  if not assigned( GisLoggerObj ) then begin
    GisLoggerObj := T_Logger.Create ;
    s_path := GisLoggerObj.Path ;
    GisLoggerObj.Path := _value ;
    if s_path <> _value then
      TGIS_Logger.AsTrace( 'Logger registered', '' ) ;
  end else
    GisLoggerObj.Path := _value ;
end ;

{$IFDEF DCC}
  initialization

  finalization
    TGIS_Logger.AsTrace( 'Logger unregistered', '' ) ;
    FreeObject( GisLoggerObj ) ;
{$ENDIF}


end.

