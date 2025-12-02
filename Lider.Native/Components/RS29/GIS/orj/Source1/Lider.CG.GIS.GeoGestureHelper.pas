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
  Generic class to help screen manipulation on touch screens.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoGestureHelper;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoGestureHelper"}
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

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL,
    TatukGIS.NDK;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,
    System.SysUtils,
    System.Classes,
    System.Math,
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypesUI ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type
  TGIS_GestureHelper = class ;

  /// <summary>
  ///   Generic gesture/touch callback procedure.
  /// </summary>
  /// <param name="_sender">
  ///   sender
  /// </param>
  TGIS_GestureHelperSimpleEvent = {$IFDEF OXYGENE} public {$ENDIF} procedure(
     _sender  : TObject
  ) of object ;


  /// <summary>
  ///   Windows helper for operation on touch screens.
  /// </summary>
  TGIS_GestureHelper = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
    private
      {$IFDEF DCC} [weak] {$ENDIF}
      oComponent    : TComponent ;
      oTapLongTimer : TGIS_Timer ;
      fnRaiseTap    : TGIS_GestureHelperSimpleEvent ;
    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      /// <param name="_component">
      ///   owner component
      /// </param>
      /// <param name="_raiseTap">
      ///   event to be raised on tap
      /// </param>
      constructor Create ( const _component : TComponent ;
                           const _raiseTap  : TGIS_GestureHelperSimpleEvent
                         ) ; overload;
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      /// <param name="_component">
      ///   owner component
      /// </param>
      constructor Create ( const _component : TComponent
                         ) ; overload;
    protected
      procedure doDestroy ; override;
    private
      /// <summary>
      ///   Perform mouse Tap.
      /// </summary>
      procedure doTapLong          (       _sender : TObject
                                   ) ;

    public
      /// <summary>
      ///   Keyboard state; reports pressed keys.
      /// </summary>
      GestureState                 : TGIS_GestureState ;
    public
      /// <summary>
      ///   Gesture/touch equivalent of the OnMouseDown event.
      /// </summary>
      /// <param name="_shift">
      ///   True if the Shift key is pressed
      /// </param>
      /// <param name="_alt">
      ///   True if the Alt key is pressed
      /// </param>
      /// <param name="_ctrl">
      ///   True if the Ctrl key is pressed
      /// </param>
      /// <param name="_left">
      ///   True if the left mouse button is pressed
      /// </param>
      /// <param name="_right">
      ///   True if the right mouse button is pressed
      /// </param>
      /// <param name="_middle">
      ///   True if the middle mouse button is pressed
      /// </param>
      /// <param name="_x">
      ///   X coordinate
      /// </param>
      /// <param name="_y">
      ///   Y coordinate
      /// </param>
      procedure GestureMouseDown   ( const _shift  : Boolean ;
                                     const _alt    : Boolean ;
                                     const _ctrl   : Boolean ;
                                     const _left   : Boolean ;
                                     const _right  : Boolean ;
                                     const _middle : Boolean ;
                                     const _x      : Double  ;
                                     const _y      : Double
                                   ) ;
      /// <summary>
      ///   Gesture/touch equivalent of the OnMouseUp event.
      /// </summary>
      /// <param name="_shift">
      ///   True if the Shift key is pressed
      /// </param>
      /// <param name="_alt">
      ///   True if the Alt key is pressed
      /// </param>
      /// <param name="_ctrl">
      ///   True if the Ctrl key is pressed
      /// </param>
      /// <param name="_left">
      ///   True if the left mouse button is pressed
      /// </param>
      /// <param name="_right">
      ///   True if the right mouse button is pressed
      /// </param>
      /// <param name="_middle">
      ///   True if the middle mouse button is pressed
      /// </param>
      /// <param name="_x">
      ///   X coordinate
      /// </param>
      /// <param name="_y">
      ///   Y coordinate
      /// </param>
      procedure GestureMouseUp     ( const _shift  : Boolean ;
                                     const _alt    : Boolean ;
                                     const _ctrl   : Boolean ;
                                     const _left   : Boolean ;
                                     const _right  : Boolean ;
                                     const _middle : Boolean ;
                                     const _x      : Double  ;
                                     const _y      : Double
                                   ) ;
      /// <summary>
      ///   Gesture/touch equivalent of the OnMouseMove event.
      /// </summary>
      /// <param name="_shift">
      ///   True if the Shift key is pressed
      /// </param>
      /// <param name="_alt">
      ///   True if the Alt key is pressed
      /// </param>
      /// <param name="_ctrl">
      ///   True if the Ctrl key is pressed
      /// </param>
      /// <param name="_left">
      ///   True if the left mouse button is pressed
      /// </param>
      /// <param name="_right">
      ///   True if the right mouse button is pressed
      /// </param>
      /// <param name="_middle">
      ///   True if the middle mouse button is pressed
      /// </param>
      /// <param name="_x">
      ///   X coordinate
      /// </param>
      /// <param name="_y">
      ///   Y coordinate
      /// </param>
      procedure GestureMouseMove   ( const _shift  : Boolean ;
                                     const _alt    : Boolean ;
                                     const _ctrl   : Boolean ;
                                     const _left   : Boolean ;
                                     const _right  : Boolean ;
                                     const _middle : Boolean ;
                                     const _x      : Double  ;
                                     const _y      : Double
                                   ) ;
      /// <summary>
      ///   Gesture/touch equivalent of the OnMouseMove event.
      ///   In addition, the method calls the long tap timer handler
      ///   if the long tap interval has already expired
      ///   and the handler was not called spontaneously.
      /// </summary>
      /// <param name="_shift">
      ///   True if the Shift key is pressed
      /// </param>
      /// <param name="_alt">
      ///   True if the Alt key is pressed
      /// </param>
      /// <param name="_ctrl">
      ///   True if the Ctrl key is pressed
      /// </param>
      /// <param name="_left">
      ///   True if the left mouse button is pressed
      /// </param>
      /// <param name="_right">
      ///   True if the right mouse button is pressed
      /// </param>
      /// <param name="_middle">
      ///   True if the middle mouse button is pressed
      /// </param>
      /// <param name="_x">
      ///   X coordinate
      /// </param>
      /// <param name="_y">
      ///   Y coordinate
      /// </param>
      procedure GestureMouseMoveEx ( const _shift  : Boolean ;
                                     const _alt    : Boolean ;
                                     const _ctrl   : Boolean ;
                                     const _left   : Boolean ;
                                     const _right  : Boolean ;
                                     const _middle : Boolean ;
                                     const _x      : Double  ;
                                     const _y      : Double
                                   ) ;
      /// <summary>
      ///   Cancels the long tap event.
      /// </summary>
      procedure GestureCancelLongTap ;

      {$IFDEF CLR}
        /// <summary>
        ///   Gesture/touch equivalent of the OnDoubleClick event.
        /// </summary>
        procedure GestureDoubleClick ;
      {$ENDIF}

      {$IFNDEF CLR}
        /// <summary>
        ///   Indicates a beginning of a gesture/touch event.
        /// </summary>
        /// <returns>
        ///   True if there are gesture events in progress
        /// </returns>
        function  GestureBegin     : Boolean ;
        /// <summary>
        ///   Indicates an end of a gesture/touch event.
        /// </summary>
        /// <param name="_clear">
        ///   True to end all the gesture events in progress
        /// </param>
        /// <returns>
        ///   True if there are gesture events in progress
        /// </returns>
        function  GestureEnd       ( const _clear  : Boolean = True
                                   ) : Boolean ;
        /// <summary>
        ///   Checks if there are gesture events in progress.
        /// </summary>
        /// <returns>
        ///   True if there are gesture events in progress
        /// </returns>
        function  GestureActive    : Boolean ;
      {$ENDIF}
      /// <summary>
      ///   Checks if the gesture/touch event in progress
      ///   indicates displacement.
      /// </summary>
      /// <returns>
      ///   True if the event does not indicate displacement
      /// </returns>
      function  GestureNoMovement  : Boolean ;
      /// <summary>
      ///   Checks if the displacement defined by the gesture
      ///   starting point and the supplied coordinates is the
      ///   largest during the gesture event; if so records it.
      /// </summary>
      /// <param name="_x">
      ///   X coordinate
      /// </param>
      /// <param name="_y">
      ///   Y coordinate
      /// </param>
      procedure GestureRecordMovement(
                                     const _x      : Double  ;
                                     const _y      : Double
                                   ) ;
  end ;


//##############################################################################
implementation

//==============================================================================
// TGIS_GestureHelper
//==============================================================================

  constructor TGIS_GestureHelper.Create(
    const _component : TComponent
  ) ;
  var
    tmp : TGIS_GestureHelperSimpleEvent ;
  begin
    tmp := nil ;
    Create( _component, tmp ) ;
  end ;


  constructor TGIS_GestureHelper.Create(
    const _component   : TComponent ;
    const _raiseTap : TGIS_GestureHelperSimpleEvent
  ) ;
  begin
    oComponent := _component ;
    fnRaiseTap := _raiseTap ;

    oTapLongTimer   := TGIS_Timer.Create ;
    oTapLongTimer.Interval := GIS_GESTURE_TAPLONG_THRESHOLD ;
    {$IFDEF OXYGENE}
      oTapLongTimer.OnTimer += doTapLong ;
    {$ELSE}
      oTapLongTimer.OnTimer := doTapLong ;
    {$ENDIF}

    oTapLongTimer.Enabled := False ;
  end ;


  procedure TGIS_GestureHelper.doDestroy ;
  begin
    FreeObject( oTapLongTimer ) ;

    inherited ;
  end ;

  procedure TGIS_GestureHelper.doTapLong(
    _sender : TObject
  ) ;
  begin
    oTapLongTimer.Enabled := False ;
    GestureState.DownCount := 3 ;
    if assigned( fnRaiseTap ) then
      fnRaiseTap( _sender ) ;
  end ;


  procedure TGIS_GestureHelper.GestureMouseDown(
    const _shift  : Boolean ;
    const _alt    : Boolean ;
    const _ctrl   : Boolean ;
    const _left   : Boolean ;
    const _right  : Boolean ;
    const _middle : Boolean ;
    const _x      : Double  ;
    const _y      : Double
  ) ;

    procedure reset_mouse_state ;
    begin
      GestureState.Shift     := _shift ;
      GestureState.Alt       := _alt ;
      GestureState.Ctrl      := _ctrl ;
      GestureState.Left      := _left ;
      GestureState.Right     := _right ;
      GestureState.Middle    := _middle ;
      GestureState.DownX     := _x ;
      GestureState.DownY     := _y ;
      GestureState.DownTime  := GetTickCount ;
      GestureState.DownCount := 1 ;
      GestureState.MoveDelta := 0 ;
      oTapLongTimer.Enabled := True ;
    end ;
  begin
    if GetTickCount - GestureState.DownTime > GIS_GESTURE_TAPLONG_THRESHOLD then
      GestureState.DownCount := 0 ;

    if GestureState.DownCount = 0 then begin
      reset_mouse_state ;
    end
    else begin
      GestureRecordMovement( _x, _y  );
      if GestureNoMovement                and
        ( GestureState.Shift  = _shift  ) and
        ( GestureState.Alt    = _alt    ) and
        ( GestureState.Ctrl   = _ctrl   ) and
        ( GestureState.Left   = _left   ) and
        ( GestureState.Right  = _right  ) and
        ( GestureState.Middle = _middle )
      then begin
        // accepted as double click
        GestureState.DownCount := 2 ;
      end
      else begin
        // treat as a new sequence
        reset_mouse_state ;
      end ;
    end ;
  end ;


  procedure TGIS_GestureHelper.GestureMouseUp(
    const _shift  : Boolean ;
    const _alt    : Boolean ;
    const _ctrl   : Boolean ;
    const _left   : Boolean ;
    const _right  : Boolean ;
    const _middle : Boolean ;
    const _x      : Double  ;
    const _y      : Double
  ) ;
  begin
    GestureCancelLongTap ;

    GestureRecordMovement( _x, _y ) ;

    case GestureState.DownCount of
      1  : begin
             if GestureNoMovement then begin
               if assigned( fnRaiseTap ) then
                 fnRaiseTap( self );
             end ;
           end ;
      2  : begin
             if GestureNoMovement then begin
               if GetTickCount - GestureState.DownTime
                  <
                  GIS_GESTURE_TAPLONG_THRESHOLD
               then begin
                 // double click
                 if assigned( fnRaiseTap ) then
                   fnRaiseTap( self );
               end ;
             end ;
             GestureState.DownCount := 0 ;  // reset mouse state
           end ;
      else begin
             GestureState.DownCount := 0 ; // reset mouse state
           end ;
    end ;
  end ;


  procedure TGIS_GestureHelper.GestureMouseMove(
    const _shift  : Boolean ;
    const _alt    : Boolean ;
    const _ctrl   : Boolean ;
    const _left   : Boolean ;
    const _right  : Boolean ;
    const _middle : Boolean ;
    const _x      : Double  ;
    const _y      : Double
  ) ;
  begin
    GestureRecordMovement( _x, _y ) ;
    if not GestureNoMovement then
      GestureCancelLongTap ;
  end ;


  procedure TGIS_GestureHelper.GestureMouseMoveEx(
    const _shift  : Boolean ;
    const _alt    : Boolean ;
    const _ctrl   : Boolean ;
    const _left   : Boolean ;
    const _right  : Boolean ;
    const _middle : Boolean ;
    const _x      : Double  ;
    const _y      : Double
  ) ;
  begin
    GestureRecordMovement( _x, _y ) ;
    if not GestureNoMovement then
      GestureCancelLongTap ;
    if oTapLongTimer.Enabled and
       ( GetTickCount - GestureState.DownTime > GIS_GESTURE_TAPLONG_THRESHOLD ) then
      doTapLong( nil ) ;
  end ;


  procedure TGIS_GestureHelper.GestureCancelLongTap ;
  begin
    oTapLongTimer.Enabled := False ;
  end ;

  {$IFDEF CLR}
    procedure TGIS_GestureHelper.GestureDoubleClick ;
    begin
      GestureState.DownCount := 2 ;
    end;
  {$ENDIF}

{$IFNDEF CLR}
  function TGIS_GestureHelper.GestureBegin : Boolean ;
  begin
    GestureState.GestureCnt := GestureState.GestureCnt + 1 ;
    Result := GestureState.GestureCnt > 0 ;
  end ;


  function TGIS_GestureHelper.GestureEnd(
    const _clear : Boolean = True
  ) : Boolean ;
  begin
    if _clear then
      GestureState.GestureCnt := 0
    else begin
      GestureState.GestureCnt := GestureState.GestureCnt - 1 ;
      if GestureState.GestureCnt < 0 then
        GestureState.GestureCnt := 0 ;
    end ;
    Result := GestureState.GestureCnt > 0 ;
  end ;


  function TGIS_GestureHelper.GestureActive : Boolean ;
  begin
    Result := GestureState.GestureCnt > 0 ;
  end ;
{$ENDIF}


  function TGIS_GestureHelper.GestureNoMovement : Boolean ;
  begin
    Result := GestureState.MoveDelta < GIS_GESTURE_MOVEMENT_THRESHOLD ;
  end ;


  procedure TGIS_GestureHelper.GestureRecordMovement(
    const _x      : Double  ;
    const _y      : Double
  ) ;
  begin
    GestureState.MoveDelta := Max( GestureState.MoveDelta,
                                   Sqrt( Sqr( GestureState.DownX - _x ) +
                                         Sqr( GestureState.DownY - _y )
                                       )
                                 ) ;
  end ;


//==================================== END =====================================
end.
