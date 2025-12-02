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
  Support for GPS devices.
}

unit Lider.CG.GIS.VCL.GeoGps ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.VCL.GeoGps"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.Math,
  VCL.Graphics,
  VCL.Forms,
  VCL.Controls,
  VCL.StdCtrls,
  VCL.ExtCtrls,

  {$IFDEF GIS_XDK}
    XDK.Core,
  {$ENDIF}

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoFunctions ;

type
  /// <summary>
  ///   OnNmea event.
  /// </summary>
  /// <param name="_sender">
  ///   object that sent the event
  /// </param>
  /// <param name="_name">
  ///   name of an NMEA message (GPGSA, GPGGA, etc.)
  /// </param>
  /// <param name="_items">
  ///   list of items (0 is name)
  /// </param>
  /// <param name="_parsed">
  ///   If False, then message was not used in any way by the component.
  /// </param>
  TGIS_GpsNmeaEvent = procedure (
    _sender : TObject  ;
    _name   : String   ;
    _items  : TStrings ;
    _parsed : Boolean
  ) of object ;

  /// <summary>
  ///   OnNmeaMessage event.
  /// </summary>
  /// <param name="_sender">
  ///   object that sent the event
  /// </param>
  /// <param name="_message">
  ///   single, uninterpreted line of data as received by
  /// </param>
  TGIS_GpsNmeaMessageEvent = procedure (
    _sender  : TObject  ;
    _message : String
  ) of object ;

  /// <summary>
  ///   <para>
  ///     GPS NMEA devices support.
  ///   </para>
  ///   <para>
  ///     Following NMEA messages are interpreted: - GSA for obtaining
  ///     visible satellites count and precision - GGA for obtaining altitude
  ///     - RMC for obtaining position and speed and course
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    All data acquired from the GPS NMEA device are related  to the WGS84
  ///    ellipsoid.
  ///    </note>
  /// </remarks>
  [ComponentPlatformsAttribute( pfidWindows )]
  TGIS_GpsNmea = class( TCustomPanel )
    private // screen objects
      lblTime: TLabel;
      lblTimeVal: TLabel;
      lblLong: TLabel;
      lblLongVal: TLabel;
      lblLat: TLabel;
      lblLatVal: TLabel;
      lblAlt: TLabel;
      lblAltVal: TLabel;
      lblSpeed: TLabel;
      lblSpeedVal: TLabel;
      lblCourse: TLabel;
      lblCourseVal: TLabel;
      lblSat: TLabel;
      lblSatVal: TLabel;
      lblPrec: TLabel;
      lblPrecVal: TLabel;
      tmrCheck : TTimer ;
      tmrLocation : TTimer ;

    private
      timeVal   : String ;
      longVal   : String ;
      latVal    : String ;
      altVal    : String ;
      speedVal  : String ;
      courseVal : String ;
      satVal    : String ;
      precVal   : String ;

    private // property internal values ;
      FTimeOut : Integer ;
      FPositionTime : TDateTime ;
      FPositionPrec : Double ;
      FLongitude : Double ;
      FLatitude : Double ;
      FSpeed : Double ;
      FCourse : Double ;
      FSatellites : Integer ;
      FAltitude : Double ;

      FOnPosition    : TNotifyEvent ;
      FOnNmea        : TGIS_GpsNmeaEvent ;
      FOnNmeaMessage : TGIS_GpsNmeaMessageEvent ;

    protected // property access routines

      function  fget_Active   : Boolean ;
      procedure fset_Active   ( const _val : Boolean
                              ) ;
      function  fget_Com      : Integer ;
      procedure fset_Com      ( const _val : Integer
                              ) ;
      function  fget_BaudRate : Integer ;
      procedure fset_BaudRate ( const _val : Integer
                              ) ;
      function fget_TimeVal   : String ;
      function fget_LongVal   : String ;
      function fget_LatVal    : String ;
      function fget_AltVal    : String ;
      function fget_SpeedVal  : String ;
      function fget_CourseVal : String ;
      function fget_SatVal    : String ;
      function fget_PrecVal   : String ;

    private // other private variables
      bLocationService : Boolean ;
      sensorId    : Integer ;
      sensorObj   : TObject ;
      comPort     : Pointer ;
      nmeaLine    : TStringList ;
      nmeaRaw     : TStringBuilder ;
      nmeaTmp     : TStringBuilder ;

      baseObj     : TGIS_Object ;
      readTime    : TDateTime ;
      uponDestroy : Boolean ;

      supportedGPGGA : Boolean ;

    private // private methods

      /// <summary>
      ///   Process a single character from the serial port.
      /// </summary>
      /// <param name="_c">
      ///   character to be processed
      /// </param>
      procedure processChar  ( const _c : Char ) ;
      procedure updateControl;

      /// <summary>
      ///   Process a single line from the serial port.
      /// </summary>
      procedure processLine  ;

      /// <summary>
      ///   Set all labels into green state (proper GPS communication)
      /// </summary>
      procedure setGreen     ;

      /// <summary>
      ///   Set all labels into red state (temporary GPS communication problems)
      /// </summary>
      procedure setRed       ;

      /// <summary>
      ///   Set all labels into unknown state (GPS is disconnected)
      /// </summary>
      procedure setUnknown   ;

    protected
      /// <summary>
      ///   Event handler for receiving characters from the serial port.
      /// </summary>
      /// <param name="_sender">
      ///   who rises the event
      /// </param>
      /// <param name="_count">
      ///   number of characters
      /// </param>
      procedure rxChar    ( _sender : TObject ;
                            _count  : Integer
                          ) ;

      /// <summary>
      ///   Event handler for 'is living' check.
      /// </summary>
      /// <param name="_sender">
      ///   who rises the event
      /// </param>
      procedure checkTime ( _sender : TObject
                          ) ;

      /// <summary>
      ///   Event handler for locations services check.
      /// </summary>
      /// <param name="_sender">
      ///   who rises the event
      /// </param>
      procedure checkLocation(
                            _sender : TObject
                          ) ;
    private
      procedure doCreate  ;
      procedure doDestroy ;

    public

      /// <summary>
      ///   Create component.
      /// </summary>
      /// <param name="_owner">
      ///   parent control
      /// </param>
      constructor Create( _owner : TComponent ) ; override;

      /// <summary>
      ///   Destroy component.
      /// </summary>
      destructor Destroy ; override;

    protected

      /// <summary>
      ///   Panel painting.
      /// </summary>
      procedure Paint   ; override;

    public

      /// <summary>
      ///   Interpret a single NMEA message.
      /// </summary>
      /// <param name="_message">
      ///   NMEA message
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Message should be in standard NMEA format like
      ///    '$GPGSA,A,3,19,08,27,28,17,13,,,,,,,3.0,2.2,2.0*38'
      ///    </note>
      /// </remarks>
      procedure ParseMessage( const _message : String ) ;

      /// <summary>
      ///   Interpret a text as a raw NMEA stream.
      /// </summary>
      /// <param name="_text">
      ///   NMEA stream
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Text should be in standard NMEA format like
      ///    '$GPGSA,A,3,19,08,27,28,17,13,,,,,,,3.0,2.2,2.0*38'
      ///    </note>
      ///   This method can be use used to parse NMES stream received form
      ///   source other then COM port.
      /// </remarks>
      procedure ParseText   ( const _text    : String ) ;

    public
      /// <summary>
      ///   Activity state. If True, GPS device will be connected.
      /// </summary>
      property Active : Boolean read fget_Active write fset_Active ;

      /// <summary>
      ///   Current Longitude (in radians).
      /// </summary>
      property Longitude : Double read FLongitude ;

      /// <summary>
      ///   Current Latitude (in radians).
      /// </summary>
      property Latitude : Double read FLatitude ;

      /// <summary>
      ///   Current Speed (in km/h).
      /// </summary>
      property Speed : Double read FSpeed ;

      /// <summary>
      ///   Current Course (in radians).
      /// </summary>
      property Course : Double read FCourse ;

      /// <summary>
      ///   Current Altitude (in meters).
      /// </summary>
      property Altitude : Double read FAltitude ;

      /// <summary>
      ///   Number of satellites that are currently visible.
      /// </summary>
      property Satellites : Integer read FSatellites ;

      /// <summary>
      ///   Text representation of time value.
      /// </summary>
      property TimeValTxt   : String read fget_TimeVal   ;

      /// <summary>
      ///   Text representation of longitude value.
      /// </summary>
      property LongValTxt   : String read fget_LongVal   ;

      /// <summary>
      ///   Text representation of latitude value.
      /// </summary>
      property LatValTxt    : String read fget_LatVal    ;

      /// <summary>
      ///   Text representation of altitude value.
      /// </summary>
      property AltValTxt    : String read fget_AltVal    ;

      /// <summary>
      ///   Text representation of speed value.
      /// </summary>
      property SpeedValTxt  : String read fget_SpeedVal  ;

      /// <summary>
      ///   Text representation of course value.
      /// </summary>
      property CourseValTxt : String read fget_CourseVal ;

      /// <summary>
      ///   Text representation of satellite number value.
      /// </summary>
      property SatValTxt    : String read fget_SatVal    ;

      /// <summary>
      ///   Text representation of precision value.
      /// </summary>
      property PrecValTxt   : String read fget_PrecVal   ;

    published

        /// <summary>
        ///   Timeout, after which the GPS device will be treated as
        ///   non-receiving.
        /// </summary>
        property Timeout : Integer read FTimeOut write FTimeOut
                                                 default 1000 ;

        /// <summary>
        ///   Serial port number. By providing 0 a platform location services
        ///   will be enabled.
        /// </summary>
        property Com : Integer read fget_Com write fset_Com ;

        /// <summary>
        ///   Serial port baud rate. Should one of the value: 110, 300, 600,
        ///   1200, 2400, 4800, 9600, 14400, 19200, 38400, 56000, 57600,
        ///   115200, 128000, 256000
        /// </summary>
        property BaudRate : Integer read fget_BaudRate write fset_BaudRate ;

        /// <summary>
        ///   Time of last position reading received from the GPS device as
        ///   Coordinated Universal Time (UTC).
        /// </summary>
        property PositionTime : TDateTime read FPositionTime ;

        /// <summary>
        ///   Precision of position readings (in meters).
        /// </summary>
        property PositionPrec  : Double read FPositionPrec ;

    published // events

        /// <event/>
        /// <summary>
        ///   Event fired on each position messages received from the GPS
        ///   device.
        /// </summary>
        property PositionEvent : TNotifyEvent
                                 read  FOnPosition
                                 write FOnPosition ;

        /// <event/>
        /// <summary>
        ///   Event fired on each messages received from the GPS device.
        ///   Provides message parsed into individual elements.
        /// </summary>
        property NmeaEvent     : TGIS_GpsNmeaEvent
                                 read FOnNmea
                                 write FOnNmea ;

        /// <event/>
        /// <summary>
        ///   Event fired on each messages received from the GPS device.
        ///   Provides single, uninterpreted line of data as received by by
        ///   the the component.
        /// </summary>
        property NmeaMessageEvent : TGIS_GpsNmeaMessageEvent
                                 read FOnNmeaMessage
                                 write FOnNmeaMessage ;
      public

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property DockManager;

      published

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property Align;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property Alignment;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property Anchors;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property AutoSize;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property BevelInner;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property BevelOuter;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property BevelWidth;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property BiDiMode;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property BorderWidth;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property BorderStyle;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property Color;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property Constraints;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property Ctl3D;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property UseDockManager default True;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property DockSite;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property DragCursor;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property DragKind;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property DragMode;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property Enabled;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property FullRepaint;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property Font;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property Locked;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property ParentBiDiMode;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property ParentColor;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property ParentCtl3D;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property ParentFont;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property ParentShowHint;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property PopupMenu;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property ShowHint;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property TabOrder;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property TabStop;

        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property Visible;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property OnCanResize;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property OnClick;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property OnConstrainedResize;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property OnDockDrop;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property OnDockOver;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property OnDblClick;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property OnDragDrop;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property OnDragOver;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property OnEndDock;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property OnEndDrag;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property OnEnter;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property OnExit;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property OnGetSiteInfo;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property OnMouseDown;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property OnMouseMove;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property OnMouseUp;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property OnResize;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property OnStartDock;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property OnStartDrag;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property OnUnDock;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property OnMouseWheel ;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomPanel in Delphi help.
        /// </summary>
        property OnMouseWheelUp ;

    {$IFDEF GIS_XDK}
      public
        {#gendoc:hide:GENXDK}
        XDK : TGIS_ControlXDK ;
    {$ENDIF}

  end;

  {#gendoc:hide}
  procedure Register;

//##############################################################################
implementation

{$R GisGps_16x16.RES}

uses
  System.DateUtils,
  System.Sensors,
  Lider.CG.GIS.GeoResource ;

type
  T_comPortRXCharEvent   = procedure(
    Sender  : TObject ;
    Count   : Integer
  ) of object;

  T_comPort = class ( TThread )
    private
      FBaudRate   : Integer    ;
      FTimeOut    : Integer    ;
      FCom          : Integer    ;
      FOnRXChar     : T_comPortRXCharEvent ;

    private
      comHandle   : THandle ;
      breakEvent  : THandle ;
      uponDestroy : Boolean ;
      stopped     : Boolean ;

    private
      function  fget_Active      : Boolean ;
      procedure fset_Active      ( const _value : Boolean
                                 ) ;
      procedure fset_Com         ( const _value : Integer
                                 ) ;
      procedure fset_BaudRate    ( const _value : Integer
                                 ) ;
    protected
      // Standard override of Execute method.
      procedure Execute       ; override;

      // Event raising method. TO be called from Synchronize().
      procedure RaiseEvent    ;

    public
      // Create object.
      constructor Create     ;

      // Destroy object.
      destructor  Destroy    ; override;

    public
      // Read character to the buffer.
      // _buffer buffer to be read-in
      // _count  number of characters to be read-in
      // return readed characters
      function  Read  ( var _buffer; _count: DWORD
                      ) : DWORD ;

    public // properties
      property Active   : Boolean read fget_Active
                                  write fset_Active ;
      property Com      : Integer read  FCom
                                  write fset_Com ;
      property BaudRate : Integer read FBaudRate
                                  write fset_BaudRate ;
      property TimeOut  : Integer read FTimeOut
                                  write FTimeOut ;
    public    // events
      property RXCharEvent : T_comPortRXCharEvent read FOnRXChar write FOnRXChar ;
  end ;


//==============================================================================
// T_comPort component
//==============================================================================

  constructor T_comPort.Create ;
  begin
    inherited Create( False ) ;

    FCom := 1 ;
    FBaudRate  := 4800 ;
    comHandle := INVALID_HANDLE_VALUE ;
    breakEvent := CreateEvent( nil, True, False, nil ) ;
    uponDestroy := False ;
  end ;

  destructor T_comPort.Destroy ;
  begin
    uponDestroy := True ;
    SetEvent( breakEvent );
    inherited ;
  end ;

  function T_comPort.fget_Active : Boolean ;
  begin
    Result := comHandle <> INVALID_HANDLE_VALUE ;
  end ;

  procedure T_comPort.fset_Active(
    const _value : Boolean
  ) ;
  var
    dcb  : TDCB ;
    i    : Integer ;
    scom : String ;
  begin
    for i:= 0 to 0 do begin
      if Active = _value then exit ;

      try
        if not _value then abort ;
        try
          if comHandle <> INVALID_HANDLE_VALUE then
            CloseHandle( comHandle ) ;
        except
        end ;

        comHandle := INVALID_HANDLE_VALUE ;

        scom := Format( '\\.\COM%d', [FCom] ) ;

        comHandle := CreateFile( PChar( scom ),
                                 GENERIC_READ or GENERIC_WRITE,
                                 0, nil,
                                 OPEN_EXISTING, 0,
                                 0
                               ) ;

        if comHandle = INVALID_HANDLE_VALUE then abort ;

        GetCommState( comHandle, dcb ) ;

        dcb.Flags    := $00000001  ;
        dcb.Parity   := NOPARITY   ;
        dcb.StopBits := ONESTOPBIT ;
        dcb.BaudRate := BaudRate   ;
        dcb.ByteSize := 8          ;

        if not SetCommMask( comHandle, EV_RXCHAR ) then abort ;
        if not SetCommState( comHandle, dcb )      then abort ;
      except
        try
          if comHandle <> INVALID_HANDLE_VALUE then
            CloseHandle( comHandle ) ;
        except
        end ;
        comHandle := INVALID_HANDLE_VALUE ;
      end ;

      Sleep( 50 ) ;
    end ;
  end ;

  procedure T_comPort.fset_Com(
    const _value : Integer
  ) ;
  var
    old_active : Boolean ;
  begin
    old_active := Active ;
    Active     := False  ;

    if ( _value < 0 ) or ( _value > 500 ) then exit ;

    FCom := _value ;

    Active := old_active ;
  end ;

  procedure T_comPort.fset_BaudRate(
    const _value : Integer
  ) ;
  var
    old_active : Boolean ;
    br : Integer ;
  begin
    old_active := Active ;
    Active     := False  ;

    br := 0 ;
    if _value >= ( 0                       )       then br := CBR_110    ;
    if _value >= ( CBR_300    + CBR_110    ) div 2 then br := CBR_300    ;
    if _value >= ( CBR_600    + CBR_300    ) div 2 then br := CBR_600    ;
    if _value >= ( CBR_1200   + CBR_600    ) div 2 then br := CBR_1200   ;
    if _value >= ( CBR_2400   + CBR_1200   ) div 2 then br := CBR_2400   ;
    if _value >= ( CBR_4800   + CBR_2400   ) div 2 then br := CBR_4800   ;
    if _value >= ( CBR_9600   + CBR_4800   ) div 2 then br := CBR_9600   ;
    if _value >= ( CBR_14400  + CBR_9600   ) div 2 then br := CBR_14400  ;
    if _value >= ( CBR_19200  + CBR_14400  ) div 2 then br := CBR_19200  ;
    if _value >= ( CBR_38400  + CBR_19200  ) div 2 then br := CBR_38400  ;
    if _value >= ( CBR_56000  + CBR_38400  ) div 2 then br := CBR_56000  ;
    if _value >= ( CBR_57600  + CBR_56000  ) div 2 then br := CBR_57600  ;
    if _value >= ( CBR_115200 + CBR_57600  ) div 2 then br := CBR_115200 ;
    if _value >= ( CBR_128000 + CBR_115200 ) div 2 then br := CBR_128000 ;
    if _value >= ( CBR_256000 + CBR_128000 ) div 2 then br := CBR_256000 ;

    FBaudRate := br ;
    Active    := old_active ;
  end ;


  procedure T_ComPort.RaiseEvent ;
  var
    stat : TComStat ;
    err  : DWORD    ;
  begin
    if not ClearCommError( comHandle, err, @stat) then begin
      Active := False ;
      exit ;
    end ;

    if Assigned( RXCharEvent ) then RXCharEvent( Self, stat.cbInQue ) ;
  end ;

  procedure T_comPort.Execute ;
  var
    events   : array[0..1] of THandle;
    overlap  : TOverlapped ;
  begin
    FillChar( overlap, SizeOf( overlap), 0 );
    overlap.hEvent := CreateEvent( nil, True, True, nil ) ;
    events[0] := breakEvent ;
    events[1] := overlap.hEvent ;

    while True do begin
      if Terminated then break ;
      Sleep( 200 ) ;
      if comHandle = INVALID_HANDLE_VALUE then begin
        Sleep( 500 ) ;
        if uponDestroy then break ;
      end ;

      RaiseEvent;
    end ;

    PurgeComm( comHandle,
               PURGE_TXABORT or PURGE_RXABORT or PURGE_TXCLEAR or PURGE_RXCLEAR
             ) ;
    CloseHandle( overlap.hEvent ) ;
    CloseHandle( breakEvent     ) ;

    stopped := True ;
  end ;

  function T_comPort.Read( var _buffer; _count: DWORD ): DWORD ;
  var
    res      : Boolean     ;
    transfer : DWORD       ;
  begin
    res := ReadFile( comHandle, _buffer, _count, transfer, nil ) ;

    if not res then begin
      Result := 0 ;
      exit ;
    end ;

    if transfer = 0 then begin
      // stupid, but on some USB drivers returned transfer is always 0
      // but because we know, that read succeed we can assume that
      // transfer is equal requested character count
      transfer := _count ;
    end ;

    Result := transfer;
  end ;


//==============================================================================
// TGIS_GpsNmea component
//==============================================================================

  procedure TGIS_GpsNmea.doCreate ;
  begin
    baseObj := TGIS_Object.Create ;

    Width := 100 ;
    Height := 100 ;

    Caption   := ' ' ;

    timeVal   := '' ;
    longVal   := '' ;
    latVal    := '' ;
    altVal    := '' ;
    speedVal  := '' ;
    precVal   := '' ;
    courseVal := '' ;
    satVal    := '' ;

    lblTime      := TLabel.Create( self ) ;
    lblTimeVal   := TLabel.Create( self ) ;
    lblLong      := TLabel.Create( self ) ;
    lblLongVal   := TLabel.Create( self ) ;
    lblLat       := TLabel.Create( self ) ;
    lblLatVal    := TLabel.Create( self ) ;
    lblPrec      := TLabel.Create( self ) ;
    lblPrecVal   := TLabel.Create( self ) ;
    lblAlt       := TLabel.Create( self ) ;
    lblAltVal    := TLabel.Create( self ) ;
    lblSpeed     := TLabel.Create( self ) ;
    lblSpeedVal  := TLabel.Create( self ) ;
    lblCourse    := TLabel.Create( self ) ;
    lblCourseVal := TLabel.Create( self ) ;
    lblSat       := TLabel.Create( self ) ;
    lblSatVal    := TLabel.Create( self ) ;

    InsertControl( lblTimeVal   ) ;
    InsertControl( lblTime      ) ;
    InsertControl( lblLong      ) ;
    InsertControl( lblLongVal   ) ;
    InsertControl( lblLat       ) ;
    InsertControl( lblLatVal    ) ;
    InsertControl( lblPrec      ) ;
    InsertControl( lblPrecVal   ) ;
    InsertControl( lblAlt       ) ;
    InsertControl( lblAltVal    ) ;
    InsertControl( lblSpeed     ) ;
    InsertControl( lblSpeedVal  ) ;
    InsertControl( lblCourse    ) ;
    InsertControl( lblCourseVal ) ;
    InsertControl( lblSat       ) ;
    InsertControl( lblSatVal    ) ;

    lblTime.AutoSize      := True ;
    lblTimeVal.AutoSize   := True ;
    lblLong.AutoSize      := True ;
    lblLongVal.AutoSize   := True ;
    lblLat.AutoSize       := True ;
    lblLatVal.AutoSize    := True ;
    lblPrec.AutoSize      := True ;
    lblPrecVal.AutoSize   := True ;
    lblAlt.AutoSize       := True ;
    lblAltVal.AutoSize    := True ;
    lblSpeed.AutoSize     := True ;
    lblSpeedVal.AutoSize  := True ;
    lblCourse.AutoSize    := True ;
    lblCourseVal.AutoSize := True ;
    lblSat.AutoSize       := True ;
    lblSatVal.AutoSize    := True ;

    setUnknown ;

    lblCourseVal.Left := 10 ;
    lblCourseVal.Top := 10 ;

    tmrCheck := TTimer.Create( self ) ;
    tmrCheck.OnTimer := checkTime ;
    tmrCheck.Interval := 3000 ;
    tmrCheck.Enabled := True ;

    tmrLocation := TTimer.Create( self ) ;
    tmrLocation.OnTimer := checkLocation ;
    tmrLocation.Interval := 1000 ;
    tmrLocation.Enabled := False ;

    nmeaLine := TStringList.Create ;

    nmeaRaw  := TStringBuilder.Create ;
    nmeaTmp  := TStringBuilder.Create ;

    FTimeOut         := 1000  ;
    FPositionTime    := 0 ;
    readTime         := 0 ;
    FPositionPrec    := 0 ;
    FLongitude       := 0 ;
    FLatitude        := 0 ;
    FAltitude        := 0 ;
    FSatellites      := 0 ;

    comPort  := T_comPort.Create ;
    T_comPort( comPort ).BaudRate := 4800 ;
    T_comPort( comPort ).RXCharEvent := rxChar ;

    bLocationService := False ;
    sensorId  := -1 ;
    sensorObj := nil ;

    uponDestroy := False ;
  end;

  constructor TGIS_GpsNmea.Create( _owner : TComponent ) ;
  begin
    inherited ;
    doCreate ;
  end ;

  procedure TGIS_GpsNmea.doDestroy ;
  begin
    uponDestroy := True ;
    Active := False ;

    T_comPort( comPort ).RXCharEvent := nil ;
    FreeObjectNotNil( T_comPort( comPort ) ) ;

    FreeObject( nmeaLine     ) ;
    FreeObject( nmeaRaw      ) ;
    FreeObject( nmeaTmp      ) ;

    tmrCheck.OnTimer := nil ;
    FreeObject( tmrCheck     ) ;

    tmrLocation.OnTimer := nil ;
    FreeObject( tmrLocation  ) ;

    FreeObject( lblTime      ) ;
    FreeObject( lblTimeVal   ) ;
    FreeObject( lblLong      ) ;
    FreeObject( lblLongVal   ) ;
    FreeObject( lblLat       ) ;
    FreeObject( lblLatVal    ) ;
    FreeObject( lblPrec      ) ;
    FreeObject( lblPrecVal   ) ;
    FreeObject( lblAlt       ) ;
    FreeObject( lblAltVal    ) ;
    FreeObject( lblSpeed     ) ;
    FreeObject( lblSpeedVal  ) ;
    FreeObject( lblCourse    ) ;
    FreeObject( lblCourseVal ) ;
    FreeObject( lblSat       ) ;
    FreeObject( lblSatVal    ) ;

    FreeObject( baseObj      ) ;

  end;

  destructor TGIS_GpsNmea.Destroy ;
  begin
    if not uponDestroy then
      doDestroy;
    inherited ;
  end ;

  function TGIS_GpsNmea.fget_Active : Boolean ;
  begin
    if bLocationService then
      Result := tmrLocation.Enabled
    else
      Result := T_comPort( comPort ).Active ;
  end ;

  procedure TGIS_GpsNmea.fset_Active( const _val : Boolean ) ;
  begin
    if _val = Active then exit ;

    if bLocationService then begin
      if _val = Active then exit ;

      TSensorManager.Current.Active := _val ;

      sensorId := -1 ;
      sensorObj := nil ;
      tmrLocation.Enabled := _val ;
    end
    else begin
      T_comPort( comPort ).TimeOut := FTimeOut ;

      if T_comPort( comPort ).Active = _val then exit ;

      T_comPort( comPort ).Active := _val ;
    end;

    supportedGPGGA := False ;
    setUnknown ;

    if Active then
      readTime := Now
    else
      readTime := 0 ;
  end ;

  function TGIS_GpsNmea.fget_Com : Integer ;
  begin
    if bLocationService then
      Result := 0
    else
      Result := T_comPort(comPort).Com ;
  end ;

  procedure TGIS_GpsNmea.fset_Com( const _val : Integer ) ;
  var
    old_active : Boolean ;
  begin
    old_active := Active ;

    Active := False ;

    if _val = 0 then begin
      bLocationService := True
    end
    else begin
      bLocationService := False ;
      T_comPort(comPort).Com := _val ;
    end;

    Active := old_active ;
  end ;

  function TGIS_GpsNmea.fget_BaudRate : Integer ;
  begin
    if bLocationService then
      Result := 0
    else
      Result := T_comPort(comPort).BaudRate ;
  end ;

  procedure TGIS_GpsNmea.fset_BaudRate( const _val : Integer ) ;
  begin
    if bLocationService then
      // do nothing
    else
      T_comPort(comPort).BaudRate := _val ;
  end ;

  function TGIS_GpsNmea.fget_TimeVal : String ;
  begin
    Result := timeVal ;
  end;

  function TGIS_GpsNmea.fget_LongVal : String ;
  begin
    Result := longVal ;
  end;

  function TGIS_GpsNmea.fget_LatVal : String ;
  begin
    Result := latVal ;
  end;

  function TGIS_GpsNmea.fget_AltVal : String ;
  begin
    Result := altVal ;
  end;

  function TGIS_GpsNmea.fget_SpeedVal : String ;
  begin
    Result := speedVal ;
  end;

  function TGIS_GpsNmea.fget_CourseVal : String ;
  begin
    Result := courseVal ;
  end;

  function TGIS_GpsNmea.fget_SatVal : String ;
  begin
    Result := satVal ;
  end;

  function TGIS_GpsNmea.fget_PrecVal : String ;
  begin
    Result := precVal ;
  end;

  procedure TGIS_GpsNmea.processChar( const _c : Char ) ;
  begin
    case _c of
      #10,
      #13 : begin
              if nmeaLine.Count > 0 then
                processLine ;
              exit ;
            end ;
      '*',
      ',' : begin
              if nmeaLine.Count = 0 then
              begin
                nmeaLine.Add('');
                if nmeaTmp.Length > 0 then begin
                  FreeObject( nmeaTmp ) ;
                  nmeaTmp := TStringBuilder.Create;
                end;
              end ;
              nmeaRaw.Append( _c ) ;
              nmeaLine[nmeaLine.Count-1] := nmeaTmp.ToString ;
              nmeaLine.Add('');
              if nmeaTmp.Length > 0 then begin
                FreeObject( nmeaTmp ) ;
                nmeaTmp := TStringBuilder.Create;
              end;
            end ;
      else  begin
              nmeaRaw.Append( _c ) ;
              if nmeaLine.Count = 0 then
              begin
                nmeaLine.Add('');
                if nmeaTmp.Length > 0 then begin
                  FreeObject( nmeaTmp ) ;
                  nmeaTmp := TStringBuilder.Create;
                end;
              end ;
              nmeaTmp.Append( _c ) ;
             end;
    end ;

    if nmeaLine.Count > 0 then begin
      if Length( String( nmeaLine[ nmeaLine.Count-1] ) ) > 9999 then
        nmeaLine.Clear ;
      if nmeaLine.Count > 99 then
        nmeaLine.Clear ;
    end ;

  end ;

  procedure TGIS_GpsNmea.updateControl ;
  begin
    lblSatVal.Caption  := satVal ;
    lblPrecVal.Caption  := precVal ;
    lblTimeVal.Caption  := timeVal ;
    lblLatVal.Caption  := latVal ;
    lblLongVal.Caption  := longVal ;
    lblAltVal.Caption  := altVal ;
    lblSpeedVal.Caption  := speedVal ;
    lblCourseVal.Caption  := courseVal ;
  end ;

  procedure TGIS_GpsNmea.processLine ;
  var
    k       : Integer ;
    sdeg    : String  ;
    smin    : String  ;
    sname   : String  ;
    parsed  : Boolean ;
    stmp    : String  ;
    hour    : Word    ;
    min     : Word    ;
    sec     : Word    ;
    year    : Word    ;
    month   : Word    ;
    day     : Word    ;

    procedure update_control ;
    begin
      updateControl ;
    end ;

    function compare_sentence(
      const _sname : String ;
      const _id    : String
    ) : Boolean ;
    var
      i : Integer ;
      l : Integer ;
    begin
      Result := False ;

      l := StringLast( _sname ) ;
      if l <> StringLast( _id ) then
        exit ;

      for i := StringFirst to l do begin
        if _id[i] = '-' then
          continue ;

        if _sname[i] <> _id[i] then
          exit ;
      end ;

      Result := True ;
    end;

  begin
    try
      try
        if nmeaLine.Count < 1 then exit ;

        sname  := String(nmeaLine[0]) ;

        parsed := False       ;

        if Assigned( FOnNmeaMessage ) then
          NmeaMessageEvent( Self, nmeaRaw.ToString ) ;

        if      compare_sentence( sname, '$--GSA' ) then begin
                  if nmeaLine.Count < 16 then exit ;

                  k := 0 ;
                  if not IsStringEmpty( String(nmeaLine[ 3]) ) then Inc( k )  ;
                  if not IsStringEmpty( String(nmeaLine[ 4]) ) then Inc( k )  ;
                  if not IsStringEmpty( String(nmeaLine[ 5]) ) then Inc( k )  ;
                  if not IsStringEmpty( String(nmeaLine[ 6]) ) then Inc( k )  ;
                  if not IsStringEmpty( String(nmeaLine[ 7]) ) then Inc( k )  ;
                  if not IsStringEmpty( String(nmeaLine[ 8]) ) then Inc( k )  ;
                  if not IsStringEmpty( String(nmeaLine[ 9]) ) then Inc( k )  ;
                  if not IsStringEmpty( String(nmeaLine[10]) ) then Inc( k )  ;
                  if not IsStringEmpty( String(nmeaLine[11]) ) then Inc( k )  ;
                  if not IsStringEmpty( String(nmeaLine[12]) ) then Inc( k )  ;
                  if not IsStringEmpty( String(nmeaLine[13]) ) then Inc( k )  ;
                  if not IsStringEmpty( String(nmeaLine[14]) ) then Inc( k )  ;
                  FSatellites := k ;

                  satVal := Format( '%10d', [ FSatellites ] ) ;

                  if not supportedGPGGA then begin
                    try
                      stmp := String(nmeaLine[15]) ;
                      if IsStringEmpty( stmp ) then
                        Abort ;
                      FPositionPrec := DotStrToFloat( stmp ) ;

                      precVal := Format( '%11.1f m', [ PositionPrec ] ) ;
                    except
                      FPositionPrec  := 99 ;

                      precVal := GIS_RS_GPS_UNDEFINED ;
                    end ;
                  end ;

                  parsed := True ;
                end
        else if compare_sentence( sname, '$--GGA' ) then begin
                  if nmeaLine.Count < 10 then exit ;

                  supportedGPGGA := True ;

                  try
                    stmp := String(nmeaLine[1]) ;
                    if IsStringEmpty( stmp ) then
                      Abort;
                    hour := StrToInt( Copy( stmp, StringFirst    , 2 ) ) ;
                    min  := StrToInt( Copy( stmp, StringFirst + 2, 2 ) ) ;
                    sec  := StrToInt( Copy( stmp, StringFirst + 4, 2 ) ) ;

                    DecodeDate( Now, year, month, day );

                    FPositionTime := EncodeDate( year, month, day ) +
                                     EncodeTime( hour, min  , sec,  0 ) ;

                    timeVal := DateTimeToStr( PositionTime ) ;
                  except
                    FPositionTime  := 0 ;
                    timeVal := GIS_RS_GPS_UNDEFINED ;
                  end;
                  readTime := Now ;

                  try
                    stmp := String(nmeaLine[2]);
                    if IsStringEmpty( stmp ) then
                      Abort;
                    k := Pos( '.', stmp ) ;
                    sdeg := Copy( stmp, StringFirst, k - 2 - StringFirst ) ;
                    smin := Copy( stmp, k - 2, 255 ) ;
                    FLatitude := ( DotStrToFloat( sdeg ) * Pi / 180 ) +
                                 ( DotStrToFloat( smin ) * Pi / 180 / 60 ) ;
                    if String(nmeaLine[3]) = 'S' then FLatitude := - FLatitude ;

                    latVal := GisLatitudeToStr( Latitude ) ;
                  except
                    FLatitude := 0 ;
                    latVal := GIS_RS_GPS_UNDEFINED ;
                  end;

                  try
                    stmp := String(nmeaLine[4]);
                    if IsStringEmpty( stmp ) then
                      Abort;
                    k := Pos( '.', stmp ) ;
                    sdeg := Copy( stmp, StringFirst, k - 2 - StringFirst ) ;
                    smin := Copy( stmp, k - 2, 255 ) ;
                    FLongitude := ( DotStrToFloat( sdeg ) * Pi / 180 ) +
                                  ( DotStrToFloat( smin ) * Pi / 180 / 60 ) ;
                    if String(nmeaLine[5]) = 'W' then FLongitude := - FLongitude ;

                    longVal := GisLongitudeToStr( Longitude ) ;
                  except
                    FLongitude := 0 ;

                    longVal := GIS_RS_GPS_UNDEFINED ;
                  end;

                  try
                    stmp := String(nmeaLine[7]);
                    if IsStringEmpty( stmp ) then
                      Abort;
                    FSatellites := StrToInt( stmp ) ;
                    satVal  := Format( '%10d', [ FSatellites ] ) ;
                  except
                    FSatellites  := 0 ;

                    satVal := GIS_RS_GPS_UNDEFINED ;
                  end ;

                  try
                    stmp := String(nmeaLine[8]);
                    if IsStringEmpty( stmp ) then
                      Abort;
                    FPositionPrec := DotStrToFloat( stmp ) ;

                    precVal := Format( '%11.1f m', [ PositionPrec ] ) ;
                  except
                    FPositionPrec  := 99 ;

                    precVal := GIS_RS_GPS_UNDEFINED ;
                  end ;

                  try
                    stmp := String(nmeaLine[9]);
                    if IsStringEmpty( stmp ) then
                      Abort;
                    FAltitude := DotStrToFloat( stmp ) ;

                    altVal := Format( '%11.1f m',[ FAltitude ] ) ;
                  except
                    FAltitude := 0 ;

                    altVal := GIS_RS_GPS_UNDEFINED ;
                  end;

                  if Assigned( FOnPosition ) then
                    PositionEvent( self ) ;

                  parsed := True ;
                end
        else if compare_sentence( sname, '$--RMC' ) then begin
                  if nmeaLine.Count < 7 then exit ;

                  try
                    stmp := String(nmeaLine[7]) ;
                    if IsStringEmpty( stmp ) then
                      Abort ;
                    FSpeed  := DotStrToFloat( stmp ) * 1.852 ;

                    speedVal := Format( '%11.1f km/h', [ Speed ] );
                  except
                    FSpeed := -1 ;

                    speedVal := GIS_RS_GPS_UNDEFINED ;
                  end;

                  try
                    stmp := String(nmeaLine[8]) ;
                    if IsStringEmpty( stmp ) then
                      Abort ;
                    FCourse := DotStrToFloat( stmp ) ;

                    if Course = 360 then
                      Abort ;

                    courseVal := Format( '%3.0f°', [ Course ] ) ;
                  except
                    FCourse := 360 ;

                    courseVal := GIS_RS_GPS_UNDEFINED ;
                  end;

                  if not supportedGPGGA then begin
                    try
                      stmp := String(nmeaLine[1]) ;
                      if IsStringEmpty( stmp ) then
                        Abort ;
                      hour := StrToInt( Copy( stmp, StringFirst    , 2 ) ) ;
                      min  := StrToInt( Copy( stmp, StringFirst + 2, 2 ) ) ;
                      sec  := StrToInt( Copy( stmp, StringFirst + 4, 2 ) ) ;

                      stmp  := String(nmeaLine[9]) ;
                      if IsStringEmpty( stmp ) then
                        Abort ;
                      day   := StrToInt( Copy( stmp, StringFirst    , 2 ) ) ;
                      month := StrToInt( Copy( stmp, StringFirst + 2, 2 ) ) ;
                      year  := StrToInt( Copy( stmp, StringFirst + 4, 2 ) ) ;
                      if year < 80 then year := year + 2000
                                   else year := year + 1900 ;

                      FPositionTime := EncodeDate( year, month, day ) +
                                       EncodeTime( hour, min  , sec,  0 ) ;

                      timeVal := DateTimeToStr( PositionTime ) ;
                    except
                      FPositionTime  := 0 ;
                      timeVal := GIS_RS_GPS_UNDEFINED ;
                    end;
                    readTime := Now ;

                    try
                      stmp  := String(nmeaLine[3]) ;
                      if IsStringEmpty( stmp ) then
                        Abort ;
                      k := Pos( '.', stmp ) ;
                      sdeg := Copy( stmp, StringFirst, k - 2 - StringFirst ) ;
                      smin := Copy( stmp, k - 2, 255 ) ;
                      FLatitude := ( DotStrToFloat( sdeg ) * Pi / 180 ) +
                                   ( DotStrToFloat( smin ) * Pi / 180 / 60 ) ;
                      if String(nmeaLine[4]) = 'S' then FLatitude := - FLatitude ;

                      latVal := GisLatitudeToStr( Latitude ) ;
                    except
                      FLatitude := 0 ;
                      latVal := GIS_RS_GPS_UNDEFINED ;
                    end;

                    try
                      stmp  := String(nmeaLine[5]) ;
                      if IsStringEmpty( stmp ) then
                        Abort ;
                      k := Pos( '.', stmp ) ;
                      sdeg := Copy( stmp, StringFirst, k - 2 - StringFirst ) ;
                      smin := Copy( stmp, k - 2, 255 ) ;
                      FLongitude := ( DotStrToFloat( sdeg ) * Pi / 180 ) +
                                    ( DotStrToFloat( smin ) * Pi / 180 / 60 ) ;
                      if String(nmeaLine[6]) = 'W' then FLongitude := - FLongitude ;

                      longVal := GisLongitudeToStr( Longitude ) ;
                    except
                      FLongitude := 0 ;
                      longVal := GIS_RS_GPS_UNDEFINED ;
                    end;
                  end ;

                  if Assigned( FOnPosition ) then
                    PositionEvent( self ) ;

                  parsed := True ;
                end ;

        update_control ;

        if Assigned( FOnNmea ) then
           NmeaEvent ( Self, sname, nmeaLine, parsed ) ;
      except
      end ;
    finally
      nmeaLine.Clear ;
      nmeaRaw.Length := 0 ;
    end ;
  end ;

  procedure TGIS_GpsNmea.setGreen ;
  begin
    lblTimeVal.Font.Color   := clGreen     ;
    lblLongVal.Font.Color   := clGreen     ;
    lblLatVal.Font.Color    := clGreen     ;
    lblPrecVal.Font.Color   := clGreen     ;
    lblSpeedVal.Font.Color  := clGreen     ;
    lblCourseVal.Font.Color := clGreen     ;
    lblAltVal.Font.Color    := clGreen     ;
    lblSatVal.Font.Color    := clGreen     ;
  end;

  procedure TGIS_GpsNmea.setRed ;
  begin
    lblTimeVal.Font.Color   := clRed       ;
    lblLongVal.Font.Color   := clRed       ;
    lblLatVal.Font.Color    := clRed       ;
    lblPrecVal.Font.Color   := clRed       ;
    lblSpeedVal.Font.Color  := clRed       ;
    lblCourseVal.Font.Color := clRed       ;
    lblAltVal.Font.Color    := clRed       ;
    lblSatVal.Font.Color    := clRed       ;
  end;

  procedure TGIS_GpsNmea.setUnknown ;
  begin
    lblTimeVal.Font.Color   := clGray       ;
    lblLongVal.Font.Color   := clGray       ;
    lblLatVal.Font.Color    := clGray       ;
    lblPrecVal.Font.Color   := clGray       ;
    lblAltVal.Font.Color    := clGray       ;
    lblSpeedVal.Font.Color  := clGray       ;
    lblCourseVal.Font.Color := clGray       ;
    lblSatVal.Font.Color    := clGray       ;

    lblTimeVal.Caption      := GIS_RS_GPS_UNDEFINED ;
    lblLongVal.Caption      := GIS_RS_GPS_UNDEFINED ;
    lblLatVal.Caption       := GIS_RS_GPS_UNDEFINED ;
    lblPrecVal.Caption      := GIS_RS_GPS_UNDEFINED ;
    lblAltVal.Caption       := GIS_RS_GPS_UNDEFINED ;
    lblSpeedVal.Caption     := GIS_RS_GPS_UNDEFINED ;
    lblCourseVal.Caption    := GIS_RS_GPS_UNDEFINED ;
    lblSatVal.Caption       := GIS_RS_GPS_UNDEFINED ;
  end;

  procedure TGIS_GpsNmea.rxChar(
    _sender : TObject ;
    _count  : Integer
  ) ;
  var
    i : Integer ;
    c : Byte ;
  begin
    for i := 1 to _count do begin
      if T_comPort( comPort ).Read( c, 1 ) = 1 then begin
        processChar( Char( c ) ) ;
      end ;
    end ;
  end ;

  procedure TGIS_GpsNmea.checkTime( _sender: TObject );
  begin
//    if not Active then begin
//      setUnknown ;
//    end
//    else begin
      if      ( Now - readTime ) < 1/24/60/60* 5 then setGreen
      else if ( Now - readTime ) < 1/24/60/60*30 then setRed
      else                                            setUnknown ;
//    end ;
  end ;

  procedure TGIS_GpsNmea.checkLocation( _sender: TObject );
  var
    ls : TCustomLocationSensor;
    lsprop : TCustomLocationSensor.TProperty;
    dlat   : Double ;
    slat   : String ;
    clat   : String ;
    dlon   : Double ;
    slon   : String ;
    clon   : String ;
    dalt   : Double ;
    salt   : String ;
    dhead  : Double ;
    shead  : String ;
    dspeed : Double ;
    sspeed : String ;
    dprec  : Double ;
    sprec  : String ;
    sdat   : String ;
    utc    : TDateTime ;
    stime  : String ;
    sdate  : String ;
    deg,
    min,
    sec,
    frac   : Integer ;
    sign   : TValueSign ;
    iminsec: Integer ;

    function try_other_sensor : TCustomSensor ;
    var
      sensors : TSensorArray ;
    begin
      Result := nil ;

      sensors := TSensorManager.Current.GetSensorsByCategory(TSensorCategory.Location);

      if Assigned( sensors ) then begin
        sensorId := ( sensorId + 1 ) mod ( Length( sensors ) ) ;
        Result := sensors[ sensorId ] ;
      end;
    end ;
  begin
    try
      if not Assigned( sensorObj ) then
        sensorObj := try_other_sensor ;

      ls := TCustomLocationSensor(sensorObj);

      if not Assigned( ls ) then
        exit ;

      if ls.Started and
         ( ls.State in [ TSensorState.NoData,
                         TSensorState.AccessDenied,
                         TSensorState.Error
                       ]
         )
      then begin
        sensorObj := nil ;
        exit ;
      end ;

      if not ls.Started then begin
        ls.Start;
        ls.LocationChange := TLocationChangeType.lctSmall ;
      end ;

      dlat   := Nan ;
      dlon   := Nan ;
      dprec  := Nan ;
      dalt   := Nan ;
      dspeed := Nan ;
      dhead  := Nan ;

      for lsprop in ls.AvailableProperties do
      begin
        case lsprop of
          TCustomLocationSensor.TProperty.Latitude:
            dlat   := ls.Latitude ;
          TCustomLocationSensor.TProperty.Longitude:
            dlon   := ls.Longitude ;
          TCustomLocationSensor.TProperty.ErrorRadius:
            dprec  := ls.ErrorRadius;
          TCustomLocationSensor.TProperty.Altitude:
            dalt   := ls.Altitude;
          TCustomLocationSensor.TProperty.Speed:
            dspeed := ls.Speed ;
          TCustomLocationSensor.TProperty.TrueHeading:
            dhead  := ls.TrueHeading;
        end;
      end;

      if IsNan( dlat   ) then exit ;
      if IsNan( dlon   ) then exit ;
      if IsNan( dprec  ) then dprec  := 0 ;

      GisDecodeLatitude( DegToRad( dlat ), deg, min, sec, frac, sign, 2  );
      iminsec := Trunc( ( sec + frac / 100 ) * 10000 / 60 ) ;
      slat := Format( '%d%.2d.%.4d', [deg,min,iminsec] ) ;
      if sign > 0 then clat := 'N'
                  else clat := 'S' ;

      GisDecodeLongitude( DegToRad( dlon ), deg, min, sec, frac, sign, 2  );
      iminsec := Trunc( ( sec + frac / 100 ) * 10000 / 60 ) ;
      slon := Format( '%d%.2d.%.4d', [deg,min,iminsec] ) ;
      if sign > 0 then clon := 'E'
                  else clon := 'W' ;

      sprec  := DotFloatToStr( RoundTo( dprec, -1 ) ) ;

      if IsNan( dalt ) then
        salt   := ''
      else
        salt   := DotFloatToStr( RoundTo( dalt, -1 ) ) ;

      if IsNan( dhead ) then
        shead  := ''
      else
        shead  := DotFloatToStr( RoundTo( dhead , -1 ) ) ;

      if IsNan( dspeed ) then
        sspeed := ''
      else
        sspeed := DotFloatToStr( RoundTo( dspeed / 1.852, -1 ) ) ;

      utc := TTimeZone.Local.ToUniversalTime(Now);

      stime := FormatDateTime( 'HHnnss', utc) ;
      sdate := FormatDateTime( 'DDMMYY', utc) ;

      ParseMessage(
        Format( '$GPRMC,%s,A,%s,%s,%s,%s,%s,%s,%s,0,',
                [ stime, slat,clat,slon,clon,sspeed,shead, sdate ]
              )
      ) ;

      ParseMessage(
        Format( '$GPGGA,%s,%s,%s,%s,%s,0,,%s,%s,,,',
                [ stime, slat,clat,slon,clon,sprec,salt ]
              )
      ) ;

    except
      sensorObj := nil ;
    end ;
  end;

  procedure TGIS_GpsNmea.Paint ;
  var
    dx : Integer ;
    dy : Integer ;
    w  : Integer ;
  begin
    inherited ;

    lblTime.Caption     := GIS_RS_GPS_TIME       ;
    lblLong.Caption     := GIS_RS_GPS_LONGITUDE  ;
    lblLat.Caption      := GIS_RS_GPS_LATITUDE   ;
    lblPrec.Caption     := GIS_RS_GPS_PRECISION  ;
    lblAlt.Caption      := GIS_RS_GPS_ALTITUDE   ;
    lblSpeed.Caption    := GIS_RS_GPS_SPEED      ;
    lblCourse.Caption   := GIS_RS_GPS_COURSE     ;
    lblSat.Caption      := GIS_RS_GPS_SATELLITES ;

    dx := 10 ;

    lblTime.Left   := dx ;
    lblLong.Left   := dx ;
    lblLat.Left    := dx ;
    lblPrec.Left   := dx ;
    lblAlt.Left    := dx ;
    lblSpeed.Left  := dx ;
    lblCourse.Left := dx ;
    lblSat.Left    := dx ;

    w := 0 ;
    w := Max( w, lblTime.Width   ) ;
    w := Max( w, lblLong.Width   ) ;
    w := Max( w, lblLat.Width    ) ;
    w := Max( w, lblPrec.Width   ) ;
    w := Max( w, lblAlt.Width    ) ;
    w := Max( w, lblSpeed.Width  ) ;
    w := Max( w, lblCourse.Width ) ;
    w := Max( w, lblSat.Width    ) ;

    dx := 25 ;

    lblTimeVal.Left   := w + dx ;
    lblLongVal.Left   := w + dx ;
    lblLatVal.Left    := w + dx ;
    lblPrecVal.Left   := w + dx ;
    lblAltVal.Left    := w + dx ;
    lblSpeedVal.Left  := w + dx ;
    lblCourseVal.Left := w + dx ;
    lblSatVal.Left    := w + dx ;

    dy := 5 ;

    lblTime.Top    := 10 ;
    lblLong.Top    := lblTime.Top   + lblTime.Height   + dy ;
    lblLat.Top     := lblLong.Top   + lblLong.Height   + dy ;
    lblPrec.Top    := lblLat.Top    + lblLat.Height    + dy ;
    lblAlt.Top     := lblPrec.Top   + lblPrec.Height   + dy ;
    lblSpeed.Top   := lblAlt.Top    + lblAlt.Height    + dy ;
    lblCourse.Top  := lblSpeed.Top  + lblSpeed.Height  + dy ;
    lblSat.Top     := lblCourse.Top + lblCourse.Height + dy ;

    lblTimeVal.Top   := lblTime.Top   ;
    lblLongVal.Top   := lblLong.Top   ;
    lblLatVal.Top    := lblLat.Top    ;
    lblPrecVal.Top   := lblPrec.Top   ;
    lblAltVal.Top    := lblAlt.Top    ;
    lblSpeedVal.Top  := lblSpeed.Top  ;
    lblCourseVal.Top := lblCourse.Top ;
    lblSatVal.Top    := lblSat.Top    ;

  end ;

  procedure TGIS_GpsNmea.ParseMessage( const _message : String ) ;
  var
    i : Integer  ;
  begin
    for i:=StringFirst to StringLast( _message ) do begin
      if _message[i] = #13 then break ;
      processChar( _message[i] ) ;
    end ;

    processChar( #13 ) ;

  end;

  procedure TGIS_GpsNmea.ParseText( const _text : String ) ;
  var
    i : Integer  ;
  begin
    for i:=StringFirst to StringLast( _text ) do begin
      processChar( _text[i] ) ;
      Application.ProcessMessages ;
    end ;
  end;

  procedure Register;
  begin
    RegisterComponents('TatukGIS', [TGIS_GpsNmea]);
  end ;

//==================================== END =====================================
end.

