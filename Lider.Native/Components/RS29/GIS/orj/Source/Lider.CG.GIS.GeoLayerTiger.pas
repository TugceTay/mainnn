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
  Encapsulation of a TIGER/Line files access.

  Only Record Type 1-, Record Type 2-, Record Type Z-
  and partially Record Type C data are read.
}

{$IFDEF DCC}
  unit GisLayerTiger ;
  {$HPPEMIT '#pragma link "GisLayerTiger"'}
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
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,

    GisTypes,
    
    GisLayerVector ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerTiger = class
    public
      class procedure SelfRegisterLayer() ;
  end;

  /// <summary>
  ///   Layer that can read TIGER/Line 2000 (or later) files.
  /// </summary>
  TGIS_LayerTiger = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // for internal use of TGIS_Viewer

         /// <inheritdoc/>
         procedure setUp ; override;
    protected
      // destructor

         /// <inheritdoc/>
         procedure doDestroy ; override;
    public
      // constructors

         /// <inheritdoc/>
         constructor Create ; override;
    public
        /// <summary>
        ///   Layer shape type.
        /// </summary>
        ShapeType : TGIS_ShapeType  ;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Generics.Collections,
    
    GisRtl,
    GisClasses,
    GisFunctions,
    GisInternals,
    GisResource,
    GisRegistredLayers,
    GisCsFactory,
    GisStreams ;
{$ENDIF}

const
  // E00 file strings
     TGR_RT1          = 'RT1'  ;
     TGR_RT2          = 'RT2'  ;
     TGR_RT3          = 'RT3'  ;
     TGR_RT4          = 'RT4'  ;
     TGR_RT5          = 'RT5'  ;
     TGR_RT6          = 'RT6'  ;
     TGR_RT7          = 'RT7'  ;
     TGR_RT8          = 'RT8'  ;
     TGR_RT9          = 'RT9'  ;
     TGR_RTC          = 'RTC'  ;
     TGR_RTZ          = 'RTZ'  ;

type

  // Descriptive type of supported TIGER/Line release versions.
  T_listReleaseVersion = (
    RedistrictingCensus2000,
    Census2000,
    UACensus2000,
    Census2002,
    Census2000CD108th,
    Census2003
  ) ;

  T_record = class
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF} // property internal values
      FReleaseVersion : T_listReleaseVersion
                        {$IFDEF JAVA}
                          := T_listReleaseVersion.RedistrictingCensus2000
                        {$ENDIF}  ;
    protected  // property access routines
      procedure fset_ReleaseVersion ( const _ver : String ) ;

    protected
      currLine : String ;
      currPos  : Integer ;
    protected
      function  readChar      : Char ;
      function  readInteger   ( const _size : Integer
                              ) : Integer ;
      function  readString    ( const _size : Integer
                              ) : String  ;
      function  readBoolean   : Boolean  ;
      function  readLongitude : Double ;
      function  readLatitude  : Double ;

      procedure readLine      ( const _file : TGIS_Stream
                              ) ; virtual;
      procedure setShape      ( const _shp  : TGIS_Shape
                              ) ; virtual;
    public

      /// <summary>
      ///   TIGER / Line file version.
      /// </summary>
      property ReleaseVersion : String write fset_ReleaseVersion ;
  end ;

  T_recordRT1 = class( T_record )
    public
      // Record type.
      RT : Char ;

      // Version number.
      VERSION : Integer ;

      // TIGER/Line ID, Permanent Record Number.
      TLID : String ;

      // Single-Side Complete Chain Code.
      SIDE1 : Integer ;

      // Linear Segment Source Code.
      SOURCE : Char ;

      // Feature Direction Prefix.
      FEDIRP : String ;

      // Feature Name.
      FENAME : String ;

      // Feature Type.
      FETYPE : String ;

      // Feature Direction Suffix.
      FEDIRS : String ;

      // Census Feature Class Code.
      CFCC : String ;

      // Start Address, Left.
      FRADDL : String ;

      // End Address, Left.
      TOADDL : String ;

      // Start Address, Right.
      FRADDR : String ;

      // End Address, Right.
      TOADDR : String ;

      // Start Imputed Address Flag, Left.
      FRIADDL : Boolean ;

      // End Imputed Address Flag, Left.
      TOIADDL : Boolean ;

      // Start Imputed Address Flag, Right.
      FRIADDR : Boolean ;

      // End Imputed Address Flag, Right.
      TOIADDR : Boolean ;

      // ZIP Code, Left.
      ZIPL : String ;

      // ZIP Code, Right.
      ZIPR : String ;

      // FIPS 55 Code (American Indian/Alaska Native Area/Havaiian Home Land)
      // 2000 Left - from 2002 Version unused.
      AIANHHL : LongInt ;

      // FIPS 55 Code (American Indian/Alaska Native Area/Havaiian Home Land)
      // 2000 Left - used from 2002 Version.
      AIANHHFPL : LongInt ;

      // FIPS 55 Code (American Indian/Alaska Native Area/Havaiian Home Land)
      // 2000 Right - from Census2003 unused.
      AIANHHR : LongInt ;

      // FIPS 55 Code (American Indian/Alaska Native Area/Havaiian Home Land)
      // 2000 Right - used from Census2003.
      AIANHHFPR : LongInt ;

      // American Indian/Havaiian Home Land Trust Land Indicator, 2000 Left.
      AIHHTLIL : Char ;

      // American Indian/Havaiian Home Land Trust Land Indicator, 2000 Right.
      AIHHTLIR : Char ;

      // Census Use 1.
      CENSUS1 : Char ;

      // Census Use 2.
      CENSUS2 : Char ;

      // FIPS State Code, 2000 Left.
      STATEL : Integer ;

      // FIPS State Code, 2000 Right.
      STATER : Integer ;

      // FIPS County Code, 2000 Left.
      COUNTYL : Integer ;

      // FIPS County Code, 2000 Right.
      COUNTYR : Integer ;

      // FIPS 55 Code (County Subdivision), 2000 Left.
      COUSUBL : String ;

      // FIPS 55 Code (County Subdivision), 2000 Right.
      COUSUBR : String ;

      // FIPS 55 Code (Subbarrio), 2000 Left.
      SUBMCDL : LongInt ;

      // FIPS 55 Code (Subbarrio), 2000 Right.
      SUBMCDR : LongInt ;

      // FIPS 55 Code (Place/CDP), 2000 Left.
      PLACEL : String ;

      // FIPS 55 Code (Place/CDP), 2000 Right.
      PLACER : String ;

      // Census Tract Code, 2000 Left.
      TRACTL : LongInt ;

      // Census Tract Code, 2000 Right.
      TRACTR : LongInt ;

      // Census Block Number, 2000 Left.
      BLOCKL : String ;

      // Census Block Number, 2000 Right.
      BLOCKR : String ;

      // Start Longitude.
      FRLONG : Double ;

      // Start Latitude.
      FRLAT : Double ;

      // End Longitude.
      TOLONG : Double ;

      // End Latitude.
      TOLAT : Double ;

      // Name of Geographic Area, Left.
      NAMEL : String ;

      // Name of Geographic Area, Right.
      NAMER : String ;

      // ZIP+4 Code, Left.
      ZIP4L : String ;

      // ZIP+4 Code, Right.
      ZIP4R : String ;

    {$IFDEF OXYGENE} unit or protected {$ELSE} protected {$ENDIF}
      procedure readLine      ( const _file : TGIS_Stream
                              ) ; override;
      procedure setShape      ( const _shp  : TGIS_Shape
                              ) ; override;
      procedure prepareStructure( const _ll : TGIS_LayerVector ) ;
  end ;

  T_recordRT2 = class( T_record )
    public

      // Record type.
      RT : Char ;

      // Version number.
      VERSION : Integer ;

      // TIGER/Line ID, Permanent Record Number.
      TLID : String ;

      // Record Sequence Number.
      RTSQ : Integer ;

      // Point 1, Longitude.
      LONG1 : Double ;

      // Point 1, Latitude.
      LAT1 : Double ;

      // Point 2, Longitude.
      LONG2 : Double ;

      // Point 2, Latitude.
      LAT2 : Double ;

      // Point 3, Longitude.
      LONG3 : Double ;

      // LAT3
      LAT3 : Double ;

      //  Point 4, Longitude.
      LONG4 : Double ;

      // Point 4, Latitude.
      LAT4 : Double ;

      // Point 5, Longitude.
      LONG5 : Double ;

      // Point 5, Latitude.
      LAT5 : Double ;

      // Point 6, Longitude.
      LONG6 : Double ;

      // Point 6, Latitude.
      LAT6 : Double ;

      // Point 7, Longitude.
      LONG7 : Double ;

      // Point 7, Latitude.
      LAT7 : Double ;

      //  Point 8, Longitude.
      LONG8 : Double ;

      // Point 8, Longitude.
      LAT8 : Double ;

      // Point 9, Longitude.
      LONG9 : Double ;

      // Point 9, Latitude.
      LAT9 : Double ;

      // Point 10, Longitude.
      LONG10 : Double ;

      // Point 10, Latitude.
      LAT10 : Double ;

    {$IFDEF OXYGENE} unit or protected {$ELSE} protected {$ENDIF}
      procedure readLine      ( const _file : TGIS_Stream
                              ) ; override;
      procedure setShape      ( const _shp  : TGIS_Shape
                              ) ; override;
  end ;

  T_recordRT3 = class( T_record )
  end ;

  T_recordRT4 = class( T_record )
  end ;

  T_recordRT5 = class( T_record )
  end ;

  T_recordRT6 = class( T_record )
  end ;

  T_recordRT7 = class( T_record )
  end ;

  T_recordRT8 = class( T_record )
  end ;

  T_recordRT9 = class( T_record )
  end ;

  T_recordRTI = class( T_record )
    public

      // Record Type.
      RT : Char ;

      // Version number.
      VERSION : Integer ;

      // TIGER/Line ID, Permanent Record Number.
      TLID : String ;

      // File Code.
      FILECODE : Integer ;

      // Record Type of Link.
      RTLINK : String ;

      // Census File Identification Code, Left.
      CENIDL : String ;

      // Polygon Identification Code, Left.
      POLYIDL : LongInt ;

      // Census File Identification Code, Right.
      CENIDR : String ;

      // Polygon Identification Code, Right.
      POLYIDR : LongInt ;

      // Filler (to make even character count).
      FILLER : Char ;

    protected
      procedure readLine      ( const _file : TGIS_Stream
                              ) ; override;
      procedure setShape      ( const _shp  : TGIS_Shape
                              ) ; override;
  end ;

  T_recordRTC = class( T_record )
    public

      // Record Type.
      RT : Char ;

      // Version number.
      VERSION : Integer ;

      // FIPS State Code.
      STATE : String ;

      // FIPS County Code.
      COUNTY : String ;

      // FIPS Code, Name, and/or Attribute Data Applicable Year.
      DATAYR : String ;

      // FIPS PUB 55-3 Code.
      FIPS : String ;

      // FIPS 55 Class Code.
      FIPSCC : String ;

      // Place Description Code.
      PLACEDC : Char ;

      // Legal/Statistical Area Description Code.
      LSADC : String ;

      // Entity Type Code.
      ENTITY : Char ;


      // Metropolitan Area Code.
      MA : String ;

      // School District Code.
      SD : String ;

      // Census American Indian/Alaska Native Area/Hawaiian Home Land Code - used from 2002 Version.
      AIANHH : String ;

      // Census Voting District Code/Census Tract Code.
      VTDTRACT : String ;

      // Census Urban Area Code/Urban Growth Area Code.
      UAUGA : String ;

      // Census American Indian Tribal Subdivision Code.
      AITSCE : String ;

      // Combined Statistical Area/Combined New England City and Town Area Code - used from 2003 Version.
      CSACNECTA : String ;

      // Metropolitan Statistical Area/Micropolitan Statistical Area/
      // New England City and Town Area/Metropolitan Division/
      // New England City and Town Area Division Code - used from 2003 Version.
      CBSANECTA : String ;

      // Commercial Region Code, Economic Census - used from 2003 Version.
      COMMREG : Char ;

      // Reserved Space C2 - used from 2002 Version.
      RSC2 : String ;

      // Name of Geographic Area.
      NAME : String ;

    {$IFDEF OXYGENE} unit or protected {$ELSE} protected {$ENDIF}
      procedure readLine      ( const _file : TGIS_Stream
                              ) ; override;
      procedure setShape      ( const _shp  : TGIS_Shape
                              ) ; override;
  end ;

  T_recordRTZ = class( T_record )
    public

      // Record Type.
      RT : Char ;

      // Version Number.
      VERSION : Integer ;

      // TIGER/Line ID, Permanent 1-Cell Number.
      TLID : String ;

      // Record Sequence Number.
      RTSQ : Integer ;

      // 4 Postal Add-On Code, Left.
      ZIP4L : Integer ;

      // 4 Postal Add-On Code, Right.
      ZIP4R : Integer ;

    {$IFDEF OXYGENE} unit or protected {$ELSE} protected {$ENDIF}
      procedure readLine      ( const _file : TGIS_Stream
                              ) ; override;
      procedure setShape      ( const _shp  : TGIS_Shape
                              ) ; override;
  end ;

//=============================================================================
// TGIS_LayerTiger
//=============================================================================

  constructor TGIS_LayerTiger.Create ;
  begin
    inherited ;
    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.InMemory
                           ] ;
    ShapeType := TGIS_ShapeType.Point ;

    FCS := TGIS_CSFactory.ByEPSG( GIS_EPSG_WGS84 ) ;
  end ;

  procedure TGIS_LayerTiger.doDestroy ;
  begin
    inherited ;
  end ;

  procedure TGIS_LayerTiger.setUp ;
  var
    fl       : TGIS_BufferedFileStream ;
    fname    : String ;
    {$IFDEF OXYGENE}
    {$ELSE}
      unused : Char ;
    {$ENDIF}
    verStr   : String ;
    rt1      : T_recordRT1 ;
    rt2      : T_recordRT2 ;
    rtC      : T_recordRTC ;
    rtCTmp   : T_recordRTC ;
    rtCold   : T_recordRTC ;
    rtZ      : T_recordRTZ ;
    rtZTmp   : T_recordRTZ ;
    oldDataYr : String ;
    shp      : TGIS_Shape ;
    ptg1     : TGIS_Point ;
    ptg2     : TGIS_Point ;
    size_all : Integer ;
    size_cur : Integer ;
    abort    : Boolean ;
    flpath   : String  ;
    fllineno : Integer ;

    listRT1 : TDictionary<String,TGIS_Shape> ;
    listRTC : TDictionary<String,T_recordRTC> ;
    listRTZ : TDictionary<String,T_recordRTZ> ;

  {$IFNDEF OXYGENE}
    itmRTC : TPair<String,T_recordRTC> ;
    itmRTZ : TPair<String,T_recordRTZ> ;
  {$ENDIF}

    procedure find_zip4 ( const _tlid : String ;
                          var _left   : String ;
                          var _right  : String ) ;
    var
      rtZ1 : T_recordRTZ ;
    begin
      if listRTZ.TryGetValue( _tlid, rtZ1 ) then begin
        if rtZ1.ZIP4L <> 0 then
          _left := IntToStr ( rtZ1.ZIP4L ) ;
        if rtZ1.ZIP4R <> 0 then
          _right := IntToStr ( rtZ1.ZIP4R ) ;
      end ;
    end ;

    function find_name( const _cousub : String ) : String ;
    var
      rtc   : T_recordRTC ;
    begin
      if listRTC.TryGetValue( _cousub, rtc ) then
        Result := rtc.NAME
      else
        Result := '' ;
    end ;

    function find_shape( const _tlid : String ) : TGIS_Shape ;
    begin
      if listRT1.TryGetValue( _tlid, Result ) then
      else
        Result := nil ;
    end ;

  {$IFNDEF OXYGENE}
    {$HINTS OFF} // we know that few variables were not used
  {$ENDIF}
  begin
    inherited ;
    abort := False ;

    fllineno := 0 ;

    size_all := 0 ;
    size_cur := 0 ;

    fname := GetPathNoExt( Path )+ '.rt1' ;
    if SafeFileExists( fname ) then begin
      fl := TGIS_BufferedFileStream.Create( fname, TGIS_StreamMode.Read ) ;
      size_all := size_all + fl.Size ;
      fl.Position := fl.Position + 1 ;
      fl.ReadAsciiString( verStr, 4 ) ;
      FreeObject( fl ) ;
    end
    else
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEMAPPING ), fname, GetLastError ) ;

    fname := GetPathNoExt( Path )+ '.rt2' ;
    if SafeFileExists( fname ) then begin
      fl := TGIS_BufferedFileStream.Create( fname, TGIS_StreamMode.Read ) ;
      size_all := size_all + fl.Size ;
      FreeObject( fl ) ;
    end
    else
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEMAPPING ), fname, GetLastError ) ;

    fname := GetPathNoExt( Path )+ '.rtc' ;
    if SafeFileExists( fname ) then begin
      fl := TGIS_BufferedFileStream.Create( fname, TGIS_StreamMode.Read ) ;
      size_all := size_all + fl.Size ;
      FreeObject( fl ) ;
    end
    else
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEMAPPING ), fname, GetLastError ) ;

    fname := GetPathNoExt( Path )+ '.rtz' ;
    if SafeFileExists( fname ) then begin
      fl := TGIS_BufferedFileStream.Create( fname, TGIS_StreamMode.Read ) ;
      size_all := size_all + fl.Size ;
      FreeObject( fl ) ;
    end
    else
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEMAPPING ), fname, GetLastError ) ;

    RaiseBusyPrepare( Self, Format( _rsrc( GIS_RS_BUSY_READ ), [Name] ) ) ;
    Lock ;
    try

      try
        listRT1 := TDictionary<String,TGIS_Shape>.Create ;
        listRTC := TDictionary<String,T_recordRTC>.Create ;
        listRTZ := TDictionary<String,T_recordRTZ>.Create ;
        try
          rtC := T_recordRTC.Create ;
          flpath := GetPathNoExt( Path )+ '.rtc' ;
          fl := TGIS_BufferedFileStream.Create( flpath, TGIS_StreamMode.Read ) ;
          fllineno := 0 ;
          try
            rtC.ReleaseVersion := verStr ;

            while not fl.Eof do begin
              rtC.readLine( fl ) ;
              inc( fllineno ) ;

              if ( not IsStringEmpty( rtC.FIPS ) ) and
                 ( rtC.ENTITY = 'P' ) then begin
                rtCTmp := T_recordRTC.Create ;
                rtCTmp.DATAYR := rtC.DATAYR ;
                rtCTmp.FIPS   := rtC.FIPS ;
                rtCTmp.NAME   := rtC.NAME ;
                if not listRTC.TryGetValue ( rtC.FIPS, rtCold ) then
                  listRTC.Add( rtC.FIPS, rtCTmp )
                else begin
                  oldDataYr := rtCold.DATAYR ;
                  if ( IsStringEmpty( rtCTmp.DATAYR ) ) or
                     ( oldDataYr < rtCTmp.DATAYR ) then begin
                    // something better
                    FreeObjectNotNil( rtCold ) ;
                    listRTC.AddOrSetValue(rtC.FIPS, rtCTmp ) ;
                  end else
                    FreeObject( rtCTmp ) ;
                end ;
              end ;

              if fllineno mod GIS_PROGRESS_TRESHOLD = 1 then begin
                abort := RaiseBusyShake( Self,
                                        fl.Position + size_cur,
                                        size_all
                                       ) ;
                if abort then exit ;
              end ;
            end ;

          finally
            size_cur := size_cur + fl.Size;
            FreeObject( fl  );
            FreeObject( rtC );
          end ;

          rtZ := T_recordRTZ.Create ;
          flpath := GetPathNoExt( Path )+ '.rtz' ;
          fl := TGIS_BufferedFileStream.Create( flpath, TGIS_StreamMode.Read ) ;
          fllineno := 0 ;
          try
            rtZ.ReleaseVersion := verStr ;

            while not fl.Eof do begin
              rtZ.readLine( fl ) ;
              inc( fllineno ) ;

              rtZTmp := T_recordRTZ.Create ;
              rtZTmp.TLID := rtZ.TLID ;
              rtZTmp.ZIP4L := rtZ.ZIP4L ;
              rtZTmp.ZIP4R := rtZ.ZIP4R ;
              if not listRTZ.ContainsKey( rtZ.TLID ) then
                listRTZ.Add( rtZ.TLID, rtZTmp )
              else
                FreeObject( rtZTmp ) ;

              if fllineno mod GIS_PROGRESS_TRESHOLD = 1 then begin
                abort := RaiseBusyShake( Self,
                                        fl.Position + size_cur,
                                        size_all
                                      ) ;
                if abort then exit ;
              end ;
            end ;

          finally
            size_cur := size_cur + fl.Size ;
            FreeObject( fl  ) ;
            FreeObject( rtZ ) ;
          end ;

          rt1 := T_recordRT1.Create ;
          flpath := GetPathNoExt( Path )+ '.rt1' ;
          fl := TGIS_BufferedFileStream.Create( flpath, TGIS_StreamMode.Read ) ;
          fllineno := 0 ;
          try
            rt1.ReleaseVersion := verStr ;
            rt1.prepareStructure( Self ) ;

            while not fl.Eof do begin
              rt1.readLine( fl ) ;
              inc( fllineno ) ;

              rt1.NAMEL := find_name ( rt1.PLACEL ) ;
              rt1.NAMER := find_name ( rt1.PLACER ) ;
              find_zip4 ( rt1.TLID, rt1.ZIP4L, rt1.ZIP4R ) ;

              shp := CreateShape( TGIS_ShapeType.Arc ) ;
              shp.Lock( TGIS_Lock.Projection ) ;
                shp.AddPart ;
                rt1.setShape( shp ) ;
              shp.Unlock ;

              listRT1.Add( rt1.TLID, shp ) ;

              if fllineno mod GIS_PROGRESS_TRESHOLD = 1 then begin
                abort := RaiseBusyShake( Self,
                                        fl.Position + size_cur,
                                        size_all
                                      ) ;
                if abort then exit ;
              end ;
            end ;
          finally
            size_cur := size_cur + fl.Size ;
            FreeObject( fl  ) ;
            FreeObject( rt1 ) ;
          end ;

          rt2 := T_recordRT2.Create ;
          flpath := GetPathNoExt( Path )+ '.rt2' ;
          fl := TGIS_BufferedFileStream.Create( flpath, TGIS_StreamMode.Read ) ;
          fllineno := 0 ;
          shp := nil ;
          try
            while not fl.Eof do begin
              rt2.readLine( fl ) ;
              inc( fllineno ) ;
              if rt2.RTSQ = 1 then begin
                if shp <> nil then begin
                  shp.AddPoint( ptg2 ) ;
                  shp.Unlock ;
                end ;

                shp := find_shape( rt2.TLID ) ;
                ptg1 := shp.GetPoint( 0, 0 ) ;
                ptg2 := shp.GetPoint( 0, 1 ) ;
                shp.Lock( TGIS_Lock.Projection ) ;

                shp.Reset ;
                shp.AddPart ;
                shp.AddPoint( ptg1 ) ;

                if fllineno mod GIS_PROGRESS_TRESHOLD = 1 then begin
                  abort := RaiseBusyShake( Self,
                                          fl.Position + size_cur,
                                          size_all
                                        ) ;
                  if abort then exit ;
                end ;
              end ;
              assert( shp <> nil ) ;
              rt2.setShape( shp ) ;
            end ;
          finally
            if shp <> nil then begin
              // adds the ptg2 from the last shape
              shp.AddPoint( ptg2 ) ;
              shp.Unlock ;
            end ;
            FreeObject( fl  ) ;
            FreeObject( rt2 ) ;
          end ;

        finally
          FIsModified := False ;

          // Free objects
            FreeObject( listRT1 ) ;
            {$IFNDEF NEXTGEN}
            for itmRTC in listRTC do
              FreeObjectNotNil( itmRTC.Value );
            {$ENDIF}

            FreeObject( listRTC ) ;

            {$IFNDEF NEXTGEN}
            for itmRTZ in listRTZ do
              FreeObjectNotNil( itmRTZ.Value );
            {$ENDIF}

            FreeObject( listRTZ ) ;
        end ;

        if SafeFileExists( Path ) then
          FAge := GisFileAge( Path ) ;

        FFileInfo := 'Census 2000 TIGER/Line (RT1)' ;
      except
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ), flpath, fllineno ) ;
      end ;

    finally
      Unlock ;
      RaiseBusyRelease( Self ) ;
    end
  end ;
  {$IFNDEF OXYGENE}
    {$HINTS ON}
  {$ENDIF}

//=============================================================================
// T_record
//=============================================================================

  procedure T_record.fset_ReleaseVersion ( const _ver : String ) ;
  var
    verInt : Integer ;
  begin
    verInt := StrToInt( _ver[StringFirst + 2] +
                        _ver[StringFirst + 3] +
                        _ver[StringFirst    ] +
                        _ver[StringFirst + 1]
                      ) ;

    if ( verInt > 306 ) then
      // assume later then 0603
      FReleaseVersion := T_listReleaseVersion.Census2003
    else if ( ( verInt >= 210 ) and ( verInt <= 301 ) ) or
            ( ( verInt >= 305 ) and ( verInt <= 306 ) ) then
      // 1002 to 0103 — TIGER/Line Files, 2002
      // 0503 to 0603
      FReleaseVersion := T_listReleaseVersion.Census2002
    else if ( verInt >= 302 ) and ( verInt <= 303 ) then
      // 0203 to 0303 — TIGER/Line Files, 108th CD Census 2000
      FReleaseVersion := T_listReleaseVersion.Census2000CD108th
    else if ( verInt >= 203 ) and ( verInt <= 205 ) then
      // 0302 to 0502 — TIGER/Line Files, UA Census 2000
      FReleaseVersion := T_listReleaseVersion.UACensus2000
    else if ( verInt >= 103 ) and ( verInt <= 108 ) then
      // 0301 to 0801 — TIGER/Line Files, Census 2000
      FReleaseVersion := T_listReleaseVersion.Census2000
    else if ( verInt >= 10 ) and ( verInt <= 11 ) then
      // 1000 to 1100 — TIGER/Line Files, Redistricting Census 2000
      FReleaseVersion := T_listReleaseVersion.RedistrictingCensus2000
    else //unsupported version
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ),
                                   'unsupported TIGER file version ' + _ver,
                                   0 ) ;
  end;

  function T_record.readChar : Char ;
  begin
    Result := Copy( currLine, currPos, 1 )[StringFirst] ;
    inc( currPos ) ;
  end ;

  function T_record.readInteger( const _size : Integer ) : Integer ;
  var
    tmp : String ;
  begin
    tmp := Copy( currLine, currPos, _size ) ;
    inc( currPos, _size ) ;
    tmp := Trim( tmp ) ;
    if IsStringEmpty( tmp ) then tmp := '0' ;
    Result := StrToInt( tmp ) ;
  end ;

  function T_record.readString( const _size : Integer ) : String  ;
  begin
    Result := Trim( Copy( currLine, currPos, _size ) ) ;
    inc( currPos, _size ) ;
  end ;

  function T_record.readBoolean : Boolean  ;
  var
    tmp : String ;
  begin
    tmp := Copy( currLine, currPos, 1 ) ;
    inc( currPos, 1 ) ;
    if tmp = ' ' then tmp := '0' ;
    Result := Boolean( StrToInt( tmp ) ) ;
  end ;

  function T_record.readLongitude : Double ;
  var
    tmp : String ;
  begin
    tmp := Copy( currLine, currPos, 10 ) ;
    inc( currPos, 10 ) ;
    Result := DotStrToFloat( tmp ) * 1e-06 ;
  end ;

  function T_record.readLatitude : Double ;
  var
    tmp : String ;
  begin
    tmp := Copy( currLine, currPos, 9 ) ;
    inc( currPos, 9 ) ;
    Result := DotStrToFloat( tmp ) * 1e-06 ;
  end ;

  procedure T_record.readLine( const _file : TGIS_Stream ) ;
  begin
    currLine := _file.ReadLine ;
    currPos := StringFirst ;
  end ;

  procedure T_record.setShape( const _shp : TGIS_Shape ) ;
  begin
  end ;

//=============================================================================
// T_recordRT1
//=============================================================================

  procedure T_recordRT1.readLine( const _file : TGIS_Stream ) ;
  begin
    inherited readLine( _file ) ;

    case FReleaseVersion of

      T_listReleaseVersion.Census2003               ,
      T_listReleaseVersion.Census2002               :
        begin
          RT        := readChar          ;
          VERSION   := readInteger(  4 ) ;
          TLID      := readString ( 10 ) ;
          SIDE1     := readInteger(  1 ) ;
          SOURCE    := readChar          ;
          FEDIRP    := readString (  2 ) ;
          FENAME    := readString ( 30 ) ;
          FETYPE    := readString (  4 ) ;
          FEDIRS    := readString (  2 ) ;
          CFCC      := readString (  3 ) ;
          FRADDL    := readString ( 11 ) ;
          TOADDL    := readString ( 11 ) ;
          FRADDR    := readString ( 11 ) ;
          TOADDR    := readString ( 11 ) ;
          FRIADDL   := readBoolean       ;
          TOIADDL   := readBoolean       ;
          FRIADDR   := readBoolean       ;
          TOIADDR   := readBoolean       ;
          ZIPL      := readString (  5 ) ;
          ZIPR      := readString (  5 ) ;
          AIANHHFPL := readInteger(  5 ) ;
          AIANHHFPR := readInteger(  5 ) ;
          AIHHTLIL  := readChar          ;
          AIHHTLIR  := readChar          ;
          CENSUS1   := readChar          ;
          CENSUS2   := readChar          ;
          STATEL    := readInteger(  2 ) ;
          STATER    := readInteger(  2 ) ;
          COUNTYL   := readInteger(  3 ) ;
          COUNTYR   := readInteger(  3 ) ;
          COUSUBL   := readString (  5 ) ;
          COUSUBR   := readString (  5 ) ;
          SUBMCDL   := readInteger(  5 ) ;
          SUBMCDR   := readInteger(  5 ) ;
          PLACEL    := readString (  5 ) ;
          PLACER    := readString (  5 ) ;
          TRACTL    := readInteger(  6 ) ;
          TRACTR    := readInteger(  6 ) ;
          BLOCKL    := readString (  4 ) ;
          BLOCKR    := readString (  4 ) ;
          FRLONG    := readLongitude     ;
          FRLAT     := readLatitude      ;
          TOLONG    := readLongitude     ;
          TOLAT     := readLatitude      ;
          NAMEL     := ''                ;
          NAMER     := ''                ;
          ZIP4L     := ''                ;
          ZIP4R     := ''                ;
        end;
      T_listReleaseVersion.Census2000CD108th        ,
      T_listReleaseVersion.UACensus2000             ,
      T_listReleaseVersion.Census2000               ,
      T_listReleaseVersion.RedistrictingCensus2000  :
        begin
          RT        := readChar          ;
          VERSION   := readInteger(  4 ) ;
          TLID      := readString ( 10 ) ;
          SIDE1     := readInteger(  1 ) ;
          SOURCE    := readChar          ;
          FEDIRP    := readString (  2 ) ;
          FENAME    := readString ( 30 ) ;
          FETYPE    := readString (  4 ) ;
          FEDIRS    := readString (  2 ) ;
          CFCC      := readString (  3 ) ;
          FRADDL    := readString ( 11 ) ;
          TOADDL    := readString ( 11 ) ;
          FRADDR    := readString ( 11 ) ;
          TOADDR    := readString ( 11 ) ;
          FRIADDL   := readBoolean       ;
          TOIADDL   := readBoolean       ;
          FRIADDR   := readBoolean       ;
          TOIADDR   := readBoolean       ;
          ZIPL      := readString (  5 ) ;
          ZIPR      := readString (  5 ) ;
          AIANHHL   := readInteger(  5 ) ;
          AIANHHR   := readInteger(  5 ) ;
          AIHHTLIL  := readChar          ;
          AIHHTLIR  := readChar          ;
          CENSUS1   := readChar          ;
          CENSUS2   := readChar          ;
          STATEL    := readInteger(  2 ) ;
          STATER    := readInteger(  2 ) ;
          COUNTYL   := readInteger(  3 ) ;
          COUNTYR   := readInteger(  3 ) ;
          COUSUBL   := readString (  5 ) ;
          COUSUBR   := readString (  5 ) ;
          SUBMCDL   := readInteger(  5 ) ;
          SUBMCDR   := readInteger(  5 ) ;
          PLACEL    := readString (  5 ) ;
          PLACER    := readString (  5 ) ;
          TRACTL    := readInteger(  6 ) ;
          TRACTR    := readInteger(  6 ) ;
          BLOCKL    := readString (  4 ) ;
          BLOCKR    := readString (  4 ) ;
          FRLONG    := readLongitude     ;
          FRLAT     := readLatitude      ;
          TOLONG    := readLongitude     ;
          TOLAT     := readLatitude      ;
          NAMEL     := ''                ;
          NAMER     := ''                ;
          ZIP4L     := ''                ;
          ZIP4R     := ''                ;
        end;
    end;

  end ;

  procedure T_recordRT1.setShape( const _shp  : TGIS_Shape ) ;
  begin

    case FReleaseVersion of

      T_listReleaseVersion.Census2003 ,
      T_listReleaseVersion.Census2002               :
        begin
          _shp.SetField( 'TLID'      , TLID     ) ;
          _shp.SetField( 'SIDE1'     , SIDE1    ) ;
          _shp.SetField( 'SOURCE'    , SOURCE   ) ;
          _shp.SetField( 'FEDIRP'    , FEDIRP   ) ;
          _shp.SetField( 'FENAME'    , FENAME   ) ;
          _shp.SetField( 'FETYPE'    , FETYPE   ) ;
          _shp.SetField( 'FEDIRS'    , FEDIRS   ) ;
          _shp.SetField( 'CFCC'      , CFCC     ) ;
          _shp.SetField( 'FRADDL'    , FRADDL   ) ;
          _shp.SetField( 'TOADDL'    , TOADDL   ) ;
          _shp.SetField( 'FRADDR'    , FRADDR   ) ;
          _shp.SetField( 'TOADDR'    , TOADDR   ) ;
          _shp.SetField( 'FRIADDL'   , FRIADDL  ) ;
          _shp.SetField( 'TOIADDL'   , TOIADDL  ) ;
          _shp.SetField( 'FRIADDR'   , FRIADDR  ) ;
          _shp.SetField( 'TOIADDR'   , TOIADDR  ) ;
          _shp.SetField( 'ZIPL'      , ZIPL     ) ;
          _shp.SetField( 'ZIPR'      , ZIPR     ) ;
          _shp.SetField( 'AIANHHFPL' , AIANHHFPL  ) ;
          _shp.SetField( 'AIANHHFPR' , AIANHHFPR  ) ;
          _shp.SetField( 'AIHHTLIL'  , AIHHTLIL ) ;
          _shp.SetField( 'AIHHTLIR'  , AIHHTLIR ) ;
          _shp.SetField( 'CENSUS1'   , CENSUS1  ) ;
          _shp.SetField( 'CENSUS2'   , CENSUS2  ) ;
          _shp.SetField( 'STATEL'    , STATEL   ) ;
          _shp.SetField( 'STATER'    , STATER   ) ;
          _shp.SetField( 'COUNTYL'   , COUNTYL  ) ;
          _shp.SetField( 'COUNTYR'   , COUNTYR  ) ;
          _shp.SetField( 'COUSUBL'   , COUSUBL  ) ;
          _shp.SetField( 'COUSUBR'   , COUSUBR  ) ;
          _shp.SetField( 'SUBMCDL'   , SUBMCDL  ) ;
          _shp.SetField( 'SUBMCDR'   , SUBMCDR  ) ;
          _shp.SetField( 'PLACEL'    , PLACEL   ) ;
          _shp.SetField( 'PLACER'    , PLACER   ) ;
          _shp.SetField( 'TRACTL'    , TRACTL   ) ;
          _shp.SetField( 'TRACTR'    , TRACTR   ) ;
          _shp.SetField( 'BLOCKL'    , BLOCKL   ) ;
          _shp.SetField( 'BLOCKR'    , BLOCKR   ) ;
          _shp.SetField( 'NAMEL'     , NAMEL    ) ;
          _shp.SetField( 'NAMER'     , NAMER    ) ;
          _shp.SetField( 'ZIP4L'     , ZIP4L    ) ;
          _shp.SetField( 'ZIP4R'     , ZIP4R    ) ;

          _shp.AddPoint( GisPoint( FRLONG, FRLAT ) ) ;
          _shp.AddPoint( GisPoint( TOLONG, TOLAT ) ) ;

        end;
      T_listReleaseVersion.Census2000CD108th ,
      T_listReleaseVersion.UACensus2000      ,
      T_listReleaseVersion.Census2000        ,
      T_listReleaseVersion.RedistrictingCensus2000  :
        begin
          _shp.SetField( 'TLID'      , TLID     ) ;
          _shp.SetField( 'SIDE1'     , SIDE1    ) ;
          _shp.SetField( 'SOURCE'    , SOURCE   ) ;
          _shp.SetField( 'FEDIRP'    , FEDIRP   ) ;
          _shp.SetField( 'FENAME'    , FENAME   ) ;
          _shp.SetField( 'FETYPE'    , FETYPE   ) ;
          _shp.SetField( 'FEDIRS'    , FEDIRS   ) ;
          _shp.SetField( 'CFCC'      , CFCC     ) ;
          _shp.SetField( 'FRADDL'    , FRADDL   ) ;
          _shp.SetField( 'TOADDL'    , TOADDL   ) ;
          _shp.SetField( 'FRADDR'    , FRADDR   ) ;
          _shp.SetField( 'TOADDR'    , TOADDR   ) ;
          _shp.SetField( 'FRIADDL'   , FRIADDL  ) ;
          _shp.SetField( 'TOIADDL'   , TOIADDL  ) ;
          _shp.SetField( 'FRIADDR'   , FRIADDR  ) ;
          _shp.SetField( 'TOIADDR'   , TOIADDR  ) ;
          _shp.SetField( 'ZIPL'      , ZIPL     ) ;
          _shp.SetField( 'ZIPR'      , ZIPR     ) ;
          _shp.SetField( 'AIANHHL'   , AIANHHL  ) ;
          _shp.SetField( 'AIANHHR'   , AIANHHR  ) ;
          _shp.SetField( 'AIHHTLIL'  , AIHHTLIL ) ;
          _shp.SetField( 'AIHHTLIR'  , AIHHTLIR ) ;
          _shp.SetField( 'CENSUS1'   , CENSUS1  ) ;
          _shp.SetField( 'CENSUS2'   , CENSUS2  ) ;
          _shp.SetField( 'STATEL'    , STATEL   ) ;
          _shp.SetField( 'STATER'    , STATER   ) ;
          _shp.SetField( 'COUNTYL'   , COUNTYL  ) ;
          _shp.SetField( 'COUNTYR'   , COUNTYR  ) ;
          _shp.SetField( 'COUSUBL'   , COUSUBL  ) ;
          _shp.SetField( 'COUSUBR'   , COUSUBR  ) ;
          _shp.SetField( 'SUBMCDL'   , SUBMCDL  ) ;
          _shp.SetField( 'SUBMCDR'   , SUBMCDR  ) ;
          _shp.SetField( 'PLACEL'    , PLACEL   ) ;
          _shp.SetField( 'PLACER'    , PLACER   ) ;
          _shp.SetField( 'TRACTL'    , TRACTL   ) ;
          _shp.SetField( 'TRACTR'    , TRACTR   ) ;
          _shp.SetField( 'BLOCKL'    , BLOCKL   ) ;
          _shp.SetField( 'BLOCKR'    , BLOCKR   ) ;
          _shp.SetField( 'NAMEL'     , NAMEL    ) ;
          _shp.SetField( 'NAMER'     , NAMER    ) ;
          _shp.SetField( 'ZIP4L'     , ZIP4L    ) ;
          _shp.SetField( 'ZIP4R'     , ZIP4R    ) ;

          _shp.AddPoint( GisPoint( FRLONG, FRLAT ) ) ;
          _shp.AddPoint( GisPoint( TOLONG, TOLAT ) ) ;
        end;
    end;

  end ;

  procedure T_recordRT1.prepareStructure( const _ll : TGIS_LayerVector ) ;
  begin

    case FReleaseVersion of

      T_listReleaseVersion.Census2003 ,
      T_listReleaseVersion.Census2002 :
        begin
          _ll.AddField( 'TLID'      , TGIS_FieldType.String  , 10,0 ) ;
          _ll.AddField( 'SIDE1'     , TGIS_FieldType.Number  ,  1,0 ) ;
          _ll.AddField( 'SOURCE'    , TGIS_FieldType.String  ,  1,0 ) ;
          _ll.AddField( 'FEDIRP'    , TGIS_FieldType.String  ,  1,0 ) ;
          _ll.AddField( 'FENAME'    , TGIS_FieldType.String  , 30,0 ) ;
          _ll.AddField( 'FETYPE'    , TGIS_FieldType.String  ,  4,0 ) ;
          _ll.AddField( 'FEDIRS'    , TGIS_FieldType.String  ,  2,0 ) ;
          _ll.AddField( 'CFCC'      , TGIS_FieldType.String  ,  3,0 ) ;
          _ll.AddField( 'FRADDL'    , TGIS_FieldType.String  , 11,0 ) ;
          _ll.AddField( 'TOADDL'    , TGIS_FieldType.String  , 11,0 ) ;
          _ll.AddField( 'FRADDR'    , TGIS_FieldType.String  , 11,0 ) ;
          _ll.AddField( 'TOADDR'    , TGIS_FieldType.String  , 11,0 ) ;
          _ll.AddField( 'FRIADDL'   , TGIS_FieldType.Boolean ,  1,0 ) ;
          _ll.AddField( 'TOIADDL'   , TGIS_FieldType.Boolean ,  1,0 ) ;
          _ll.AddField( 'FRIADDR'   , TGIS_FieldType.Boolean ,  1,0 ) ;
          _ll.AddField( 'TOIADDR'   , TGIS_FieldType.Boolean ,  1,0 ) ;
          _ll.AddField( 'ZIPL'      , TGIS_FieldType.String  ,  5,0 ) ;
          _ll.AddField( 'ZIPR'      , TGIS_FieldType.String  ,  5,0 ) ;
          _ll.AddField( 'AIANHHFPL' , TGIS_FieldType.Number  ,  5,0 ) ;
          _ll.AddField( 'AIANHHFPR' , TGIS_FieldType.Number  ,  5,0 ) ;
          _ll.AddField( 'AIHHTLIL'  , TGIS_FieldType.String  ,  1,0 ) ;
          _ll.AddField( 'AIHHTLIR'  , TGIS_FieldType.String  ,  1,0 ) ;
          _ll.AddField( 'CENSUS1'   , TGIS_FieldType.String  ,  1,0 ) ;
          _ll.AddField( 'CENSUS2'   , TGIS_FieldType.String  ,  1,0 ) ;
          _ll.AddField( 'STATEL'    , TGIS_FieldType.Number  ,  2,0 ) ;
          _ll.AddField( 'STATER'    , TGIS_FieldType.Number  ,  2,0 ) ;
          _ll.AddField( 'COUNTYL'   , TGIS_FieldType.Number  ,  3,0 ) ;
          _ll.AddField( 'COUNTYR'   , TGIS_FieldType.Number  ,  3,0 ) ;
          _ll.AddField( 'COUSUBL'   , TGIS_FieldType.String  ,  5,0 ) ;
          _ll.AddField( 'COUSUBR'   , TGIS_FieldType.String  ,  5,0 ) ;
          _ll.AddField( 'SUBMCDL'   , TGIS_FieldType.Number  ,  5,0 ) ;
          _ll.AddField( 'SUBMCDR'   , TGIS_FieldType.Number  ,  5,0 ) ;
          _ll.AddField( 'PLACEL'    , TGIS_FieldType.String  ,  5,0 ) ;
          _ll.AddField( 'PLACER'    , TGIS_FieldType.String  ,  5,0 ) ;
          _ll.AddField( 'TRACTL'    , TGIS_FieldType.Number  ,  6,0 ) ;
          _ll.AddField( 'TRACTR'    , TGIS_FieldType.Number  ,  6,0 ) ;
          _ll.AddField( 'BLOCKL'    , TGIS_FieldType.String  ,  4,0 ) ;
          _ll.AddField( 'BLOCKR'    , TGIS_FieldType.String  ,  4,0 ) ;
          _ll.AddField( 'NAMEL'     , TGIS_FieldType.String  , 60,0 ) ;
          _ll.AddField( 'NAMER'     , TGIS_FieldType.String  , 60,0 ) ;
          _ll.AddField( 'ZIP4L'     , TGIS_FieldType.String  ,  4,0 ) ;
          _ll.AddField( 'ZIP4R'     , TGIS_FieldType.String  ,  4,0 ) ;
        end;
      T_listReleaseVersion.Census2000CD108th        ,
      T_listReleaseVersion.UACensus2000             ,
      T_listReleaseVersion.Census2000               ,
      T_listReleaseVersion.RedistrictingCensus2000  :
        begin
        _ll.AddField( 'TLID'      , TGIS_FieldType.String  , 10,0 ) ;
        _ll.AddField( 'SIDE1'     , TGIS_FieldType.Number  ,  1,0 ) ;
        _ll.AddField( 'SOURCE'    , TGIS_FieldType.String  ,  1,0 ) ;
        _ll.AddField( 'FEDIRP'    , TGIS_FieldType.String  ,  1,0 ) ;
        _ll.AddField( 'FENAME'    , TGIS_FieldType.String  , 30,0 ) ;
        _ll.AddField( 'FETYPE'    , TGIS_FieldType.String  ,  4,0 ) ;
        _ll.AddField( 'FEDIRS'    , TGIS_FieldType.String  ,  2,0 ) ;
        _ll.AddField( 'CFCC'      , TGIS_FieldType.String  ,  3,0 ) ;
        _ll.AddField( 'FRADDL'    , TGIS_FieldType.String  , 11,0 ) ;
        _ll.AddField( 'TOADDL'    , TGIS_FieldType.String  , 11,0 ) ;
        _ll.AddField( 'FRADDR'    , TGIS_FieldType.String  , 11,0 ) ;
        _ll.AddField( 'TOADDR'    , TGIS_FieldType.String  , 11,0 ) ;
        _ll.AddField( 'FRIADDL'   , TGIS_FieldType.Boolean ,  1,0 ) ;
        _ll.AddField( 'TOIADDL'   , TGIS_FieldType.Boolean ,  1,0 ) ;
        _ll.AddField( 'FRIADDR'   , TGIS_FieldType.Boolean ,  1,0 ) ;
        _ll.AddField( 'TOIADDR'   , TGIS_FieldType.Boolean ,  1,0 ) ;
        _ll.AddField( 'ZIPL'      , TGIS_FieldType.String  ,  5,0 ) ;
        _ll.AddField( 'ZIPR'      , TGIS_FieldType.String  ,  5,0 ) ;
        _ll.AddField( 'AIANHHL'   , TGIS_FieldType.Number  ,  5,0 ) ;
        _ll.AddField( 'AIANHHR'   , TGIS_FieldType.Number  ,  5,0 ) ;
        _ll.AddField( 'AIHHTLIL'  , TGIS_FieldType.String  ,  1,0 ) ;
        _ll.AddField( 'AIHHTLIR'  , TGIS_FieldType.String  ,  1,0 ) ;
        _ll.AddField( 'CENSUS1'   , TGIS_FieldType.String  ,  1,0 ) ;
        _ll.AddField( 'CENSUS2'   , TGIS_FieldType.String  ,  1,0 ) ;
        _ll.AddField( 'STATEL'    , TGIS_FieldType.Number  ,  2,0 ) ;
        _ll.AddField( 'STATER'    , TGIS_FieldType.Number  ,  2,0 ) ;
        _ll.AddField( 'COUNTYL'   , TGIS_FieldType.Number  ,  3,0 ) ;
        _ll.AddField( 'COUNTYR'   , TGIS_FieldType.Number  ,  3,0 ) ;
        _ll.AddField( 'COUSUBL'   , TGIS_FieldType.String  ,  5,0 ) ;
        _ll.AddField( 'COUSUBR'   , TGIS_FieldType.String  ,  5,0 ) ;
        _ll.AddField( 'SUBMCDL'   , TGIS_FieldType.Number  ,  5,0 ) ;
        _ll.AddField( 'SUBMCDR'   , TGIS_FieldType.Number  ,  5,0 ) ;
        _ll.AddField( 'PLACEL'    , TGIS_FieldType.Number  ,  5,0 ) ;
        _ll.AddField( 'PLACER'    , TGIS_FieldType.Number  ,  5,0 ) ;
        _ll.AddField( 'TRACTL'    , TGIS_FieldType.Number  ,  6,0 ) ;
        _ll.AddField( 'TRACTR'    , TGIS_FieldType.Number  ,  6,0 ) ;
        _ll.AddField( 'BLOCKL'    , TGIS_FieldType.String  ,  4,0 ) ;
        _ll.AddField( 'BLOCKR'    , TGIS_FieldType.String  ,  4,0 ) ;
        _ll.AddField( 'NAMEL'     , TGIS_FieldType.String  , 60,0 ) ;
        _ll.AddField( 'NAMER'     , TGIS_FieldType.String  , 60,0 ) ;
        _ll.AddField( 'ZIP4L'     , TGIS_FieldType.String  ,  4,0 ) ;
        _ll.AddField( 'ZIP4R'     , TGIS_FieldType.String  ,  4,0 ) ;
      end;
    end;

  end ;

//=============================================================================
// T_recordRT2
//============================================================================

  procedure T_recordRT2.readLine( const _file : TGIS_Stream ) ;
  begin
    inherited readLine( _file ) ;

    case FReleaseVersion of

      T_listReleaseVersion.Census2003               ,
      T_listReleaseVersion.Census2002               ,
      T_listReleaseVersion.Census2000CD108th        ,
      T_listReleaseVersion.UACensus2000             ,
      T_listReleaseVersion.Census2000               ,
      T_listReleaseVersion.RedistrictingCensus2000  :
        begin
          RT       := readChar          ;
          VERSION  := readInteger(  4 ) ;
          TLID     := readString ( 10 ) ;
          RTSQ     := readInteger(  3 ) ;
          LONG1    := readLongitude     ;
          LAT1     := readLatitude      ;
          LONG2    := readLongitude     ;
          LAT2     := readLatitude      ;
          LONG3    := readLongitude     ;
          LAT3     := readLatitude      ;
          LONG4    := readLongitude     ;
          LAT4     := readLatitude      ;
          LONG5    := readLongitude     ;
          LAT5     := readLatitude      ;
          LONG6    := readLongitude     ;
          LAT6     := readLatitude      ;
          LONG7    := readLongitude     ;
          LAT7     := readLatitude      ;
          LONG8    := readLongitude     ;
          LAT8     := readLatitude      ;
          LONG9    := readLongitude     ;
          LAT9     := readLatitude      ;
          LONG10   := readLongitude     ;
          LAT10    := readLatitude      ;
        end ;
    end;
  end;

  procedure T_recordRT2.setShape( const _shp  : TGIS_Shape ) ;
  var
    ptg : TGIS_Point ;
  begin
    ptg := GisPoint( LONG1, LAT1 ) ;
    if ptg.X = 0 then exit ;
    _shp.AddPoint( ptg ) ;

    ptg := GisPoint( LONG2, LAT2 ) ;
    if ptg.X = 0 then exit ;
    _shp.AddPoint( ptg ) ;

    ptg := GisPoint( LONG3, LAT3 ) ;
    if ptg.X = 0 then exit ;
    _shp.AddPoint( ptg ) ;

    ptg := GisPoint( LONG4, LAT4 ) ;
    if ptg.X = 0 then exit ;
    _shp.AddPoint( ptg ) ;

    ptg := GisPoint( LONG5, LAT5 ) ;
    if ptg.X = 0 then exit ;
    _shp.AddPoint( ptg ) ;

    ptg := GisPoint( LONG6, LAT6 ) ;
    if ptg.X = 0 then exit ;
    _shp.AddPoint( ptg ) ;

    ptg := GisPoint( LONG7, LAT7 ) ;
    if ptg.X = 0 then exit ;
    _shp.AddPoint( ptg ) ;

    ptg := GisPoint( LONG8, LAT8 ) ;
    if ptg.X = 0 then exit ;
    _shp.AddPoint( ptg ) ;

    ptg := GisPoint( LONG9, LAT9 ) ;
    if ptg.X = 0 then exit ;
    _shp.AddPoint( ptg ) ;

    ptg := GisPoint( LONG10, LAT10 ) ;
    if ptg.X = 0 then exit ;
    _shp.AddPoint( ptg ) ;
  end ;

//=============================================================================
// T_recordRTI
//============================================================================

  procedure T_recordRTI.readLine( const _file : TGIS_Stream ) ;
  begin
    inherited readLine( _file ) ;

    RT       := readChar          ;
    VERSION  := readInteger(  4 ) ;
    TLID     := readString ( 10 ) ;
    FILECODE := readInteger(  4 ) ;
    RTLINK   := readChar          ;
    CENIDL   := readString (  5 ) ;
    POLYIDL  := readInteger( 10 ) ;
    CENIDR   := readString (  5 ) ;
    POLYIDR  := readInteger( 10 ) ;
    FILLER   := readChar          ;

  end ;

  procedure T_recordRTI.setShape( const _shp  : TGIS_Shape ) ;
  begin
  end ;

//=============================================================================
// T_recordRTC
//============================================================================

  procedure T_recordRTC.readLine( const _file : TGIS_Stream ) ;
  begin
    inherited readLine( _file ) ;

    case FReleaseVersion of

      T_listReleaseVersion.Census2003               :
        begin
          RT        := readChar          ;
          VERSION   := readInteger(  4 ) ;
          STATE     := readString (  2 ) ;
          COUNTY    := readString (  3 ) ;
          DATAYR    := readString (  4 ) ;
          FIPS      := readString (  5 ) ;
          FIPSCC    := readString (  2 ) ;
          PLACEDC   := readChar          ;
          LSADC     := readString (  2 ) ;
          ENTITY    := readChar          ;
          MA        := readString (  4 ) ;
          SD        := readString (  5 ) ;
          AIANHH    := readString (  4 ) ;
          VTDTRACT  := readString (  6 ) ;
          UAUGA     := readString (  5 ) ;
          AITSCE    := readString (  3 ) ;
          CSACNECTA := readString (  3 ) ;
          CBSANECTA := readString (  5 ) ;
          COMMREG   := readChar          ;
          RSC2      := readChar          ;
          NAME      := readString ( 60 ) ;
        end;
      T_listReleaseVersion.Census2002               :
        begin
          RT        := readChar          ;
          VERSION   := readInteger(  4 ) ;
          STATE     := readString (  2 ) ;
          COUNTY    := readString (  3 ) ;
          DATAYR    := readString (  4 ) ;
          FIPS      := readString (  5 ) ;
          FIPSCC    := readString (  2 ) ;
          PLACEDC   := readChar          ;
          LSADC     := readString (  2 ) ;
          ENTITY    := readChar          ;
          MA        := readString (  4 ) ;
          SD        := readString (  5 ) ;
          AIANHH    := readString (  4 ) ;
          VTDTRACT  := readString (  6 ) ;
          UAUGA     := readString (  5 ) ;
          AITSCE    := readString (  3 ) ;
          CSACNECTA := readString (  3 ) ;
          CBSANECTA := readString (  5 ) ;
          COMMREG   := readChar          ;
          RSC2      := readChar          ;
          NAME      := readString ( 60 ) ;
        end;
      T_listReleaseVersion.Census2000CD108th        :
        begin
        end;
      T_listReleaseVersion.UACensus2000             :
        begin
        end;
      T_listReleaseVersion.Census2000               :
        begin
          RT        := readChar          ;
          VERSION   := readInteger(  4 ) ;
          STATE     := readString (  2 ) ;
          COUNTY    := readString (  3 ) ;
          DATAYR    := readString (  4 ) ;
          FIPS      := readString (  5 ) ;
          FIPSCC    := readString (  2 ) ;
          PLACEDC   := readChar          ;
          LSADC     := readString (  2 ) ;
          ENTITY    := readChar          ;
          MA        := readString (  4 ) ;
          SD        := readString (  5 ) ;
          AIANHH    := readString (  4 ) ;
          VTDTRACT  := readString (  6 ) ;
          UAUGA     := readString (  5 ) ;
          AITSCE    := readString (  3 ) ;
          NAME      := readString ( 60 ) ;
        end;
      T_listReleaseVersion.RedistrictingCensus2000  :
        begin
        end;
    end;

  end ;

  procedure T_recordRTC.setShape( const _shp  : TGIS_Shape ) ;
  begin
  end ;

//=============================================================================
// T_recordRTZ
//============================================================================

  procedure T_recordRTZ.readLine( const _file : TGIS_Stream ) ;
  begin
    inherited readLine( _file ) ;

    RT        := readChar           ;
    VERSION   := readInteger (  4 ) ;
    TLID      := readString  ( 10 ) ;
    RTSQ      := readInteger (  3 ) ;
    ZIP4L     := readInteger (  4 ) ;
    ZIP4R     := readInteger (  4 ) ;
  end ;

  procedure T_recordRTZ.setShape( const _shp  : TGIS_Shape ) ;
  begin
    // do nothing
  end ;

  // Perform initialization section.
  class procedure Unit_GisLayerTiger.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-TGR', 'Census 2000 TIGER/Line', TGIS_LayerTiger, '.rt1',
                   TGIS_RegisteredLayerType.Vector, TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                   False
                 ) ;
  end;

{$IFNDEF OXYGENE}
  initialization
    Unit_GisLayerTiger.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.

