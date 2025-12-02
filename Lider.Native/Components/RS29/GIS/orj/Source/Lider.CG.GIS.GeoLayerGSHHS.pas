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
  Encapsulation of Global Self-consistant Hierarchical High-resolution Shorelines
  (GSHHS) layer.
}

{$IFDEF DCC}
  unit GisLayerGSHHS ;
  {$HPPEMIT '#pragma link "GisLayerGSHHS"'}
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
    System.SysUtils,
    System.Variants,

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
  Unit_GisLayerGSHHS = class
    public
      class procedure SelfRegisterLayer() ;
  end;

  /// <summary>
  ///   Layer which can read a GSHHS file.
  /// </summary>
  TGIS_LayerGSHHS = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    private
        FLevel  : Integer ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
         /// <inheritdoc/>
         procedure setUp      ; override;
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
        ///   Data level to read. Can be 1 land, 2 lake, 3 island in lake, 4
        ///   pond in island in lake.
        /// </summary>
        property Level : Integer read FLevel write FLevel ;
  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    {$IFDEF WINFORMS}
      System.Drawing,
    {$ENDIF}
    GisRtl,
    GisInternals,
    GisStreams,
    GisResource,
    GisTypes,
    GisClasses,
    GisRegistredLayers ;
{$ENDIF}

type

  //  Global Self-consistant Hierarchical High-resolution Shorelines
  T_GSHHS = record
    id        : LongInt ; // Unique polygon id number, starting at 0
    n         : LongInt ; // Number of points in this polygon
    flag      : LongInt ; // level + version << 8 + greenwich << 16 + source << 24
    // low byte:  level = flag & 255: Values: 1 land, 2 lake, 3 island_in_lake, 4 pond_in_island_in_lake
    // 2nd byte:  version = (flag >> 8) & 255: Values: Should be 4 for GSHHS version 1.4
    // 3rd byte:  greenwich = (flag >> 16) & 255: Values: Greenwich is 1 if Greenwich is crossed
    // 4th byte:  source = (flag >> 24) & 255: Values: 0 = CIA WDBII, 1 = WVS
    west,
    east,
    south,
    north     : LongInt  ; // min/max extent in micro-degrees
    area      : LongInt  ; // Area of polygon in 1/10 km^2
    area_full : LongInt  ;
    container : LongInt  ;
    ancestor  : LongInt  ;
  end ;

  // point representation
  T_POINT_GSHHS = record
    x : LongInt ;
    y : LongInt ;
  end ;

const
  {$IFDEF OXYGENE}
    SIZEOFT_POINT_GSHHS = sizeOf( LongInt ) + sizeOf( LongInt ) ;
  {$ELSE}
    SIZEOFT_POINT_GSHHS = sizeOf( T_POINT_GSHHS ) ;
  {$ENDIF}

//=============================================================================
// TGIS_LayerGSHHS
//=============================================================================

  constructor TGIS_LayerGSHHS.Create ;
  begin
    inherited ;
    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.InMemory
                           ]  ;
    FSupportedShapes := [ TGIS_ShapeType.Arc, TGIS_ShapeType.Polygon ] ;

    FLevel := 5 ;
  end ;

  procedure TGIS_LayerGSHHS.doDestroy ;
  begin
    inherited ;
  end ;

  procedure TGIS_LayerGSHHS.setUp ;
  var
    GSHHSRimFile  : TGIS_BufferedFileStream ;
    max_east      : Integer ;
    flip          : Boolean ;
    version       : Integer ;
    dlevel        : Integer ;
    greenwich     : Integer ;
    src           : Integer ;
    river         : Integer ;
    w, e, s, n, area : Double ;

    function swap32( i4 : LongInt ) : LongInt;
    begin
      Result := ((i4) shr 24) + (((i4) shr 8) and 65280) +
                (((i4) and 65280) shl 8) + (((i4) and 255) shl 24)
    end;

    procedure headerSwap( var hdr : T_GSHHS ) ;
    begin
      hdr.id        := swap32( hdr.id     ) ;
      hdr.n         := swap32( hdr.n      ) ;
      hdr.west      := swap32( hdr.west   ) ;
      hdr.east      := swap32( hdr.east   ) ;
      hdr.north     := swap32( hdr.north  ) ;
      hdr.south     := swap32( hdr.south  ) ;
      hdr.area      := swap32( hdr.area   ) ;
      hdr.flag      := swap32( hdr.flag   ) ;
    end;

    procedure pointSwap( var xy : T_POINT_GSHHS ) ;
    begin
      xy.x := swap32( xy.x ) ;
      xy.y := swap32( xy.y ) ;
    end;

    procedure readShape( const _gshhs : T_GSHHS ) ;
    var
      i   : Integer       ;
      shp : TGIS_Shape    ;
      ptg : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF}    ;
      pt  : T_POINT_GSHHS ;
      {$IFDEF OXYGENE}
        pt_x : LongInt ;
        pt_y : LongInt ;
      {$ENDIF}
    begin
      try
        if ( area <> 0 ) then
          shp := CreateShape( TGIS_ShapeType.Polygon )
        else
          shp := CreateShape( TGIS_ShapeType.Arc ) ;

        shp.AddPart ;
        shp.Lock( TGIS_Lock.Internal ) ;
        try
          for i := 0 to _gshhs.n - 1 do begin

            {$IFNDEF OXYGENE}
              GSHHSRimFile.Read( pt, sizeOf( pt ) ) ;
            {$ELSE}
              GSHHSRimFile.ReadInteger( pt_x, 4 ) ;
              GSHHSRimFile.ReadInteger( pt_y, 4 ) ;
              pt.x := pt_x ;
              pt.y := pt_y ;
            {$ENDIF}

            if flip then
              pointSwap( pt ) ;

            if (( greenwich <> 0 ) and ( pt.x > max_east )) or ( _gshhs.west > 180000000 ) then
              ptg.X := pt.x * 1.0e-6 - 360.0
            else
              ptg.X := pt.x * 1.0e-6 ;

            ptg.Y := pt.y * 1.0e-6 ;

            shp.AddPoint( ptg ) ;
          end;
          shp.SetField( 'GSHHS_LEVEL', dlevel ) ;
        finally
          shp.Unlock ;
        end;
      except
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ), Path, 0 ) ;
      end ;
      max_east := 180000000 ;
    end ;

    procedure skipShape( _hdr : T_GSHHS ) ;
    begin
      GSHHSRimFile.Position := GSHHSRimFile.Position + _hdr.n * SIZEOFT_POINT_GSHHS ;
    end;

    procedure read_layer ;
    var
      abort   : Boolean ;
      GSHHS   : T_GSHHS ;
    begin
      GSHHSRimFile := TGIS_BufferedFileStream.Create( Path, TGIS_StreamMode.Read ) ;

      try
        max_east  := 270000000 ;
        flip      := UpperCase( GetFileExt( Path ) ) = '.B' ;

        while not GSHHSRimFile.Eof do begin

          {$IFNDEF OXYGENE}
            GSHHSRimFile.Read( GSHHS, sizeOf( GSHHS ) ) ;
          {$ELSE}
            GSHHSRimFile.ReadInteger( GSHHS.id       , 4 ) ;
            GSHHSRimFile.ReadInteger( GSHHS.n        , 4 ) ;
            GSHHSRimFile.ReadInteger( GSHHS.flag     , 4 ) ;
            GSHHSRimFile.ReadInteger( GSHHS.west     , 4 ) ;
            GSHHSRimFile.ReadInteger( GSHHS.east     , 4 ) ;
            GSHHSRimFile.ReadInteger( GSHHS.south    , 4 ) ;
            GSHHSRimFile.ReadInteger( GSHHS.north    , 4 ) ;
            GSHHSRimFile.ReadInteger( GSHHS.area     , 4 ) ;
            GSHHSRimFile.ReadInteger( GSHHS.area_full, 4 ) ;
            GSHHSRimFile.ReadInteger( GSHHS.container, 4 ) ;
            GSHHSRimFile.ReadInteger( GSHHS.ancestor , 4 ) ;
          {$ENDIF}

          version := (GSHHS.flag shr 8) and 255 ;
          flip    := (version <> 7);  // Take as sign that byte-swabbing is needed

          abort := RaiseBusyShake( Self,
                            GSHHSRimFile.Position div 2 + GSHHSRimFile.Size div 2,
                            GSHHSRimFile.Size
                          ) ;
          if abort then break ;

          if flip then
            headerSwap( GSHHS );

          dlevel    := (GSHHS.flag and 255) ;
          version   := (GSHHS.flag shr 8)  and 255 ;

          if (version <> 7) then
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEBADFORMAT ), Path, 0 ) ;

          greenwich := (GSHHS.flag shr 16) and 1 ;
          src       := (GSHHS.flag shr 24) and 1 ;
          river     := (GSHHS.flag shr 25) and 1 ;
          w         := GSHHS.west  * 1.0e-6 ;  // Convert from microdegrees to degrees
          e         := GSHHS.east  * 1.0e-6 ;
          s         := GSHHS.south * 1.0e-6 ;
          n         := GSHHS.north * 1.0e-6 ;
          area      := 0.1 * GSHHS.area ;      // Now im km^2

          if dlevel <= FLevel then
            readShape( GSHHS )
          else
            skipShape( GSHHS ) ;
        end ;
      finally
        FIsModified := False ;

        FreeObject( GSHHSRimFile ) ;
      end ;
    end;

  begin
    inherited ;

    if not IsStringEmpty( Path ) then begin
      RaiseBusyPrepare( Self, Format( _rsrc( GIS_RS_BUSY_READ ), [Name] ) ) ;
      Lock ;
      try
        AddFieldInternal( 'GSHHS_LEVEL', TGIS_FieldType.Number, 1, 0 ) ;

        read_layer ;
      finally
        Unlock ;
        RaiseBusyRelease( Self ) ;
      end;
    end ;

    if SafeFileExists( Path ) then
      FAge := GisFileAge( Path ) ;

    FFileInfo := 'Global Self-consistant Hierarchical High-resolution'+
                 ' Shorelines (GSHHS)' ;
  end ;

  { Perform initialization section.
  }
  class procedure Unit_GisLayerGSHHS.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-GSHHS', 'Global Self-consistant Hierarchical High-resolution'+
                   ' Shorelines (GSHHS)',
                   TGIS_LayerGSHHS, '.rim;.b',
                   TGIS_RegisteredLayerType.Vector,
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                   False
                  ) ;
  end ;

{$IFNDEF OXYGENE}
  initialization
    Unit_GisLayerGSHHS.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.

