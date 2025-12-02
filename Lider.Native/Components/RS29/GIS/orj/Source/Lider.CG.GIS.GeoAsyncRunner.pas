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
  Async runner.
}

unit GisAsyncRunner;


interface

uses
   System.Classes
  ,System.SysUtils
  ,GisRtl;

type
  /// <struct>
  ///   Status of async operation
  ///  </summary>
  TGIS_AsyncState = (
    /// <struct>
    ///   Async is scheduled but nop results yet.
    ///  </summary>
    Future,

    /// <struct>
    ///   Async has been successufully executed.
    ///  </summary>
    Executed,

    /// <struct>
    ///   Async has been canceld.
    ///  </summary>
    Canceled
  );

  /// <struct>
  ///   Async data to be passed to runner and callback
  ///  </summary>
  TGIS_AsyncData = record
    private
      Owner : TObject ;
    public
      /// <struct>
      ///   Unique asyc operation identifier
      /// </summary>
      UUID : TGUID ;

      /// <struct>
      ///   Optional data to be passed. It is user responsibility to free thes
      ///   object upon callback. If callbeack is not executed object will be
      ///   freed internally.
      /// </summary>
      ManagedData : TObject ;

      /// <struct>
      ///   Optional data to be passed. This pointer remain unmanaged and should not be freed.
      /// </summary>
      UnmanagedData : TObject ;

      /// <struct>
      ///   Status of operation.
      /// </summary>
      State : TGIS_AsyncState ;
    public
      /// <struct>
      ///   Send caneclation signal to the async runner. Operation will be
      ///   terminated ASAP anm=nd callback will be never send.
      /// </summary>
      procedure Cancel;
  end;

  /// <struct>
  ///   Prototype for runner nad callbec procedures.
  ///  </summary>
  TGIS_AsyncProc = procedure( var _async : TGIS_AsyncData ) of object;


  /// <struct>
  ///   Async runner helper calss.
  /// </summary>
  TGIS_AsyncRunner = class
    private
      FAsync : TObject;
    public

      /// <summary>
      ///   Schedule a new async operation.
      /// <summary>
      /// <param name="_sender">
      ///   Object that calls async. Is a good habit taht callback procedure
      ///   is a part of the object: by calling TGIS_AsyncRunner.CamcllAll(self)
      ///   upon obect destruction is garanteed that callback will not b send
      ///   to an object that does not exits anymore.
      /// </param>
      /// <param name="_runner">
      ///   Aync runner procedure. Procedure that does actuall work,
      /// </param>
      /// <param name="_callback">
      ///   Optional caallback procedue to be executed after async runner
      //   finish its work.
      /// </param>
      /// <param name="_data">
      ///   Optional object with data with information required by runner or
      ///   passed to callback. It is programmer rpsonsibility to free this
      ///   object upon callback. If callbeck was nil then object will be
      //    destroyed autmatically.
      /// </param>
      /// <returns>
      ///   Async object descriptor.
      /// </returns>
      class function  Run      ( const _sender    : TObject         ;
                                 const _runner    : TGIS_AsyncProc  ;
                                 const _callback  : TGIS_AsyncProc  ;
                                 const _manageddata : TObject;
                                 const _unmanageddata  : TObject
                               ) : TGIS_AsyncData ;
      /// <summary>
      ///   Cancell all async operation started within the object.
      /// <summary>
      /// <param name="_sender">
      ///   Sender object (owner of callback procedure) wich defines wich
      ///   pending async operations should be cancelled.
      /// </param>
      class procedure CancelAll( const _sender    : TObject );
   end;

//##############################################################################
implementation

uses
  System.Generics.Collections
  ,GisClasses
  ;


type
  T_AsyncListener = class;

  T_AsyncObject = class
    private
      Sender    : TObject;
      AsyncData : TGIS_AsyncData;
      Runner    : TGIS_AsyncProc ;
      Callback  : TGIS_AsyncProc ;
      Listener  : T_AsyncListener ;
    public
      procedure Cancel ;
  end;


  /// <summary>
  ///   Object to for centrialize async call management.
  /// </summary>
  T_AsyncListener = class( TGIS_ObjectDisposable )
    private
      dctRunners : TDictionary<TGUID, T_AsyncObject>;
    public
      constructor Create;
    protected
      procedure doDestroy ; override ;
    public
      // to be called after asyn run
      function  Callback ( const _async : T_AsyncObject ) : Boolean ;
      // dete aync object
//      procedure Delete   ( const _async : T_AsyncObject );
      procedure CancelAll( const _sender: TObject       );
  end ;

  T_AsyncThread = class(TThread)
    private
      FAsync : TObject;
    protected
      procedure Execute; override;
  end;


var
  oAsyncListener : T_AsyncListener = nil;
  oThd : TGIS_ThreadClass ;

{$REGION 'TGIS_AsyncData'}

procedure TGIS_AsyncData.Cancel;
begin
  T_AsyncObject(Owner).Cancel ;
end;

{$ENDREGION 'TGIS_AsyncData'}

{$REGION 'TGIS_AsyncRunner'}

class function TGIS_AsyncRunner.Run(
  const _sender   : TObject;
  const _runner   : TGIS_AsyncProc;
  const _callback : TGIS_AsyncProc;
  const _manageddata : TObject;
  const _unmanageddata : TObject
) : TGIS_AsyncData;
var
  th : T_AsyncThread ;
  ao : T_AsyncObject ;
begin
  th := T_AsyncThread.Create( True ) ;
  th.FreeOnTerminate := True ;
  ao := T_AsyncObject.Create ;
  ao.Sender := _sender ;
  ao.Runner := _runner ;
  ao.Callback := _callback ;

  if not Assigned( oAsyncListener ) then
    oAsyncListener := T_AsyncListener.Create ;

  ao.Listener := oAsyncListener ;
  ao.AsyncData.Owner := ao;
  ao.AsyncData.ManagedData := _manageddata ;
  ao.AsyncData.UnmanagedData := _unmanageddata ;
  ao.AsyncData.UUID := TGUID.NewGuid ; ;
  ao.AsyncData.State := TGIS_AsyncState.Future ;

  Result := ao.AsyncData;

  th.FAsync := ao ;

  oThd.LockThread ;
  oAsyncListener.dctRunners.Add( Result.UUID, ao );
  oThd.UnlockThread ;

  th.Start ;
end;

class procedure TGIS_AsyncRunner.CancelAll(const _sender: TObject);
begin
  oAsyncListener.CancelAll( _sender ) ;
end;

{$ENDREGION 'TGIS_AsyncRunner'}

{$REGION 'T_AsyncThread'}

procedure T_AsyncThread.Execute;
var
  oasync   : T_AsyncObject ;
  bcalled  : Boolean ;
begin
  inherited;
  oasync := T_AsyncObject( FAsync ) ;
  oasync.Runner( oasync.AsyncData );

  oasync.AsyncData.State := TGIS_AsyncState.Executed;

  if Assigned( oasync.Listener ) then
    bcalled := oasync.Listener.Callback( oasync )
  else
    bcalled := False ;

  if not bcalled then // not called so maybe not freed
    FreeObject( oasync.AsyncData.ManagedData ) ;
end;

{$ENDREGION 'T_AsyncThread'}

{$REGION 'T_AsyncListener'}

constructor T_AsyncListener.Create;
begin
  dctRunners := TDictionary<TGUID, T_AsyncObject>.Create;
end;

procedure T_AsyncListener.doDestroy;
var
  elm : TPair<TGUID, T_AsyncObject>;
begin
  oThd.LockThread ;
  for elm in dctRunners do begin
    elm.Value.Cancel ;
  end;
  oThd.UnlockThread ;

  FreeObject( dctRunners );
end;

//procedure T_AsyncListener.Delete(
//  const _async: T_AsyncObject
//) ;
//var
//  oasync : T_AsyncObject ;
//begin
//  if dctRunners.TryGetValue( _async.AsyncData.UUID, oasync ) then
//    dctRunners.Remove(_async.AsyncData.UUID)
//end;

procedure T_AsyncListener.CancelAll(
  const _sender: TObject
);
var
  elm : TPair<TGUID, T_AsyncObject>;
begin
  oThd.LockThread ;
  for elm in dctRunners do begin
    if elm.Value.Sender = _sender then
      elm.Value.Cancel ;
  end;
  oThd.UnlockThread ;

end;


function T_AsyncListener.Callback(
  const _async: T_AsyncObject
) : Boolean ;
var
  oasync : T_AsyncObject ;
  bfnd : Boolean ;
begin
  Result := False ;

  oThd.LockThread ;
  bfnd := dctRunners.TryGetValue( _async.AsyncData.UUID, oasync ) ;
  oThd.UnlockThread;

  if bfnd then begin
    if Assigned( oasync.Callback ) then begin
      oasync.Callback(oasync.AsyncData);
      Result := true ;
    end;
    oThd.LockThread ;
    dctRunners.Remove(oasync.AsyncData.UUID);
    oThd.UnlockThread;
    oasync.Free ;
  end;
end;

{$ENDREGION 'TGIS_AsyncListener'}

{$REGION 'T_AsyncObject'}

procedure T_AsyncObject.Cancel;
begin
  Listener := nil ;
end;

{$ENDREGION 'T_AsyncObject'}


initialization
  oThd := TGIS_ThreadClass.Create ;

finalization
  FreeObject( oAsyncListener ) ;
  FreeObject( oThd ) ;

//==================================== END =====================================
end.

