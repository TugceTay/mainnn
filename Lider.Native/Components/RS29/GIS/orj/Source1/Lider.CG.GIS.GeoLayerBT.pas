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
  Encapsulation of Binary Terrian Grid layer.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerBT ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerBT"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    System.Types,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoLayerPixel ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl,
    tatukgis.rtl.xml ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  GisLayerBT = class
    public
      class procedure SelfRegisterLayer() ;
  end;

  /// <summary>
  ///   Encapsulation of Binary Terrian Grid layer.
  /// </summary>
  TGIS_LayerBT = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerPixel )

    private // various private variables

      /// <summary>
      ///   Data size in bytes.
      /// </summary>
      dataSize    : Word ;

      sLineBuffer : Array of SmallInt ;

      /// <summary>
      ///   Zoom in lastreading.
      /// </summary>
      lastZoom    : Double ;

      /// <summary>
      ///   Last used in setZoom function.
      /// </summary>
      actualZoom  : Double ;
    //From Grid

      /// <summary>
      ///   line buffer (line of singles).
      /// </summary>
      lineBuffer  : array of Single ;

      /// <summary>
      ///   Number of lines present in lineBuffer.
      /// </summary>
      lineInBuffer: Integer ;

    // various protected procedures
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <inheritdoc/>
      procedure setUp          ; override;

      /// <inheritdoc/>
      function  setFileScale   ( const _dwidth : Double ;
                                 const _swidth : Double ) : Double ; override;

      /// <inheritdoc/>
      function  getLine      ( const _buffer : TBytes  ;
                               const _offset : Integer ;
                               const _linenr : Integer ;
                               const _start  : Integer ;
                               const _bytes  : Integer
                             ) : Integer; override  ;

      /// <inheritdoc/>
      function  getNativeValue ( const _pt     : TPoint  ;
                                 const _ar     : TGIS_DoubleArray
                               ) : Boolean ; override;

      /// <inheritdoc/>
      function  getNativeLine ( const   _buffer   : TGIS_SingleArray ;
                                const _linenr   : Integer          ;
                                const _startIdx : Integer          ;
                                const _count    : Integer
                               ) : Integer ; override;

      /// <inheritdoc/>
      procedure prepareMinMaxZ ( const _zoom : Double = -1
                               ) ; override ;

    protected

      procedure doDestroy ; override;

    public
      // constructors

      /// <inheritdoc/>
      constructor Create  ; override;
  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoStreams,
    Lider.CG.GIS.GeoRegistredLayers,
    Lider.CG.GIS.GeoCsBase,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoCsProjections ;
{$ENDIF}

const
  BT_HEADER_SIZE = 256 ;
type
  { BT Library interface structure types.
  }
  T_BTHeader = record
    btMarker    : Array of Byte ;            //"binterr1.3" for BT 1.3
    columns     : Integer ;
    rows        : Integer ;
    dataSize    : Word ;    //Bytes per elevation grid point, either 2 or 4
    isFlPoint   : Word ;    //If 1, the data consists of single values (float)
    eHUnits     : Word ;
    utmZone     : SmallInt ;
    datum       : SmallInt ;
    xLeft       : Double ;
    xRight      : Double ;
    yBottom     : Double ;
    yTop        : Double ;
    extProj     : Word ;    // If 1, projection specified  in .prj file
    vScale      : Single ;
    {$IFDEF OXYGENE}
      {$IFDEF OXYGENE}
        function Read( _stream : TGIS_BaseStream ) : Integer ;
      {$ELSE}
        function Read( _stream : TStream     ) : Integer ;
      {$ENDIF}
    {$ENDIF}
  end ;

{$IFDEF OXYGENE}
//==============================================================================
// T_BTHeader
//==============================================================================

  {$IFDEF OXYGENE}
    function T_BTHeader.Read( _stream : TGIS_BaseStream ) : Integer ;
  {$ELSE}
    function T_BTHeader.Read( _stream : TStream ) : Integer ;
  {$ENDIF}
  begin
    {$IFDEF OXYGENE}
      if not assigned( btMarker ) then
        SetLength( btMarker, 10 ) ;
    {$ENDIF}
    Result := _stream.Read( btMarker,  10 ) +
              _stream.ReadInteger( columns,   4  ) +
              _stream.ReadInteger( rows,      4  ) +
              _stream.ReadWord( dataSize,  2  ) +
              _stream.ReadWord( isFlPoint, 2  ) +
              _stream.ReadWord( eHUnits,   2  ) +
              _stream.ReadSmallInt( utmZone,   2  ) +
              _stream.ReadSmallInt( datum,     2  ) +
              _stream.ReadDouble( xLeft,     8  ) +
              _stream.ReadDouble( xRight,    8  ) +
              _stream.ReadDouble( yBottom,   8  ) +
              _stream.ReadDouble( yTop,      8  ) +
              _stream.ReadWord( extProj,   2  ) +
              _stream.ReadSingle( vScale,    4  ) ;
  end ;
{$ENDIF}

//==============================================================================
// TGIS_LayerFLT
//==============================================================================

  constructor TGIS_LayerBT.Create ;
  begin
    inherited ;
    FSubType := FSubType + [TGIS_LayerSubType.Persistent] ;
    FIsGridImage   := True    ;
    FNoDataValue   := -9999   ;
    Params.Pixel.GridNoValue := FNoDataValue ;
    FIsNativeGridImage := True ;
    FBandsCount := 1 ;
  end ;

  procedure TGIS_LayerBT.doDestroy ;
  begin
    Dormant ;

    inherited ;
  end ;

  procedure TGIS_LayerBT.setUp ;
  var
    ext             : TGIS_Extent ;
    hdr             : T_BTHeader ;

    procedure setProjection ;
    var
      units    : TGIS_CSAbstract       ;
      params   : TGIS_CSProjParameters ;
      datum    : TGIS_CSDatum          ;
      gcs      : TGIS_CSGeographicCoordinateSystem ;
      pcs_name : String  ;
      datum_id : Integer ;
      prj_path : String  ;
    begin
      prj_path := GetPathNoExt(Path) + GIS_PRJ_EXT ;

      if ( hdr.extProj = 1 ) and SafeFileExists( prj_path ) then begin
        // external projection in prj file
        self.SetCSByWKTFile( prj_path ) ;
      end
      else begin
        // Horizontal datum
        case hdr.datum of
          0  : datum_id := 6201           ; // ADINDAN
          1  : datum_id := 6209           ; // ARC1950
          2  : datum_id := 6210           ; // ARC1960
          3  : datum_id := 6202           ; // AUSTRALIAN_GEODETIC_1966
          4  : datum_id := 6203           ; // AUSTRALIAN_GEODETIC_1984
          5  : datum_id := 6715           ; // CAMP_AREA_ASTRO
          6  : datum_id := 6222           ; // CAPE
          7  : datum_id := 6230           ; // EUROPEAN_DATUM_1950
          8  : datum_id := 6668           ; // EUROPEAN_DATUM_1979
          9  : datum_id := 6631           ; // GEODETIC_DATUM_1949
          10 : datum_id := 6738           ; // HONG_KONG_1963
          11 : datum_id := 6236           ; // HU_TZU_SHAN
          12 : datum_id := 6240           ; // INDIAN
          13 : datum_id := 6267           ; // NAD27
          14 : datum_id := 6269           ; // NAD83
          15 : datum_id := 6135           ; // OLD_HAWAIIAN_MEAN
          16 : datum_id := 106206         ; // OMAN
          17 : datum_id := 6277           ; // ORDNANCE_SURVEY_1936
          18 : datum_id := 6139           ; // PUERTO_RICO
          19 : datum_id := 6178           ; // PULKOVO_1942
          20 : datum_id := 6248           ; // PROVISIONAL_S_AMERICAN_1956
          21 : datum_id := 6301           ; // TOKYO
          22 : datum_id := 6322           ; // WGS_72
          23 : datum_id := 6326           ; // WGS_84
          else datum_id := 6326           ;
        end;

        datum := CSDatumList.ByEPSG( datum_id ) ;

        if not assigned( datum ) then
          exit ;

        gcs := CSGeographicCoordinateSystemList.Prepare(
                 -1,
                 'GCS_' + datum.WKT,
                 datum.EPSG,
                 8901, // Greenwich
                 9122  // degree
               ) ;

        if not assigned( gcs ) then
          exit ;

        if ( hdr.eHUnits > 0 ) then begin // UTM
          params := CSProjectedCoordinateSystemList.DefaultParamsForUTM(
                      hdr.utmZone
                    ) ;

          case hdr.eHUnits of
            1 :  units := CSUnitsList.ByEPSG( 9001 ) ; // m
            2 :  units := CSUnitsList.ByEPSG( 9002 ) ; // Feet international
            3 :  units := CSUnitsList.ByEPSG( 9003 ) ; // Feet U.S. survey foot
            else units := CSUnitsList.ByEPSG( 9001 ) ; // m
          end;

          if hdr.utmZone > 0 then
            pcs_name := Format( 'UTM Zone %dN', [ hdr.utmZone ] )
          else
            pcs_name := Format( 'UTM Zone %dS', [ hdr.utmZone ] ) ;

          if assigned( gcs   ) and
             assigned( units ) then
          begin
            CS := CSProjectedCoordinateSystemList.Prepare(
                    -1,
                    pcs_name,
                    gcs.EPSG,
                    units.EPSG,
                    CSPROJ_Transverse_Mercator,
                    params
                  ) ;
          end
          else
            CS := CSUnknownCoordinateSystem ;
        end
        else
          CS := gcs
      end;
    end;

  begin
    ext := GisExtent( 0,0,0,0 ) ;
    try
      realBitCount := 24 ;
      colorsNo := 0 ;

      FAntialias := True ;
      // open binary file
      fileStream := openBufferedFileStream( Path ) ;

      {$IFDEF OXYGENE}
        hdr.Read( fileStream ) ;
      {$ELSE}
        if not assigned( hdr.btMarker ) then
          SetLength( hdr.btMarker, 10 ) ;
        fileStream.Read(hdr.btMarker[0], 10) ;
        fileStream.Read(hdr.columns, sizeOf(hdr.columns)) ;
        fileStream.Read(hdr.rows, sizeOf(hdr.rows)) ;
        fileStream.Read(hdr.dataSize, sizeOf(hdr.dataSize)) ;
        fileStream.Read(hdr.isFlPoint, sizeOf(hdr.isFlPoint)) ;
        fileStream.Read(hdr.eHUnits, sizeOf(hdr.eHUnits)) ;
        fileStream.Read(hdr.utmZone, sizeOf(hdr.utmZone)) ;
        fileStream.Read(hdr.datum, sizeOf(hdr.datum)) ;
        fileStream.Read(hdr.xLeft, sizeOf(hdr.xLeft)) ;
        fileStream.Read(hdr.xRight, sizeOf(hdr.xRight)) ;
        fileStream.Read(hdr.yBottom, sizeOf(hdr.yBottom)) ;
        fileStream.Read(hdr.yTop, sizeOf(hdr.yTop)) ;
        fileStream.Read(hdr.extProj, sizeOf(hdr.extProj)) ;
        fileStream.Read(hdr.vScale, sizeOf(hdr.vScale)) ;
      {$ENDIF}

      FBitWidth  := hdr.columns ;
      FBitHeight := hdr.rows ;
      dataSize := hdr.dataSize ;

      realLineWidth := ( FBitWidth*realBitCount +7 ) div 8 ;
      intLineWidth := FBitWidth * 3 ;

      if dataSize = 2 then
        SetLength( sLineBuffer, FBitWidth )
      else
        SetLength( lineBuffer, FBitWidth ) ;

      lineInBuffer := -1 ;
      actualZoom := 1 ;
      lastZoom := 1 ;


      scaleX := (hdr.xRight -hdr.xLeft)/FBitWidth  ;
      scaleY := (hdr.yBottom -hdr.yTop)/FBitHeight ;

      if hdr.vScale <> 0 then
        scaleY := scaleY*hdr.vScale ;

      ext.XMin := hdr.xLeft   ;
      ext.YMin := hdr.yBottom ;
      ext.XMax := hdr.xRight  ;
      ext.YMax := hdr.yTop    ;

      Extent := ext ;

      setProjection ;

      if FMaxZ <= FMinZ then begin
        FMaxZ   := -GIS_MAX_SINGLE  ;
        FMinZ   :=  GIS_MAX_SINGLE  ;
        prepareMinMaxZ( 1 ) ;
      end;

      Extent3D := GisExtent3D( Extent.XMin, Extent.YMin, FMinZ,
                               Extent.XMax, Extent.YMax, FMaxZ
                              ) ;
      inherited ;

      if SafeFileExists( Path ) then
        FAge := GisFileAge( Path ) ;

      FFileInfo := Format( 'Binary Terrian Grid Format(BT)' +#13#10 +
                           '%d x %d; ',
                           [ FBitWidth, FBitHeight ]
                         ) ;
      if dataSize = 2 then
        FFileInfo := FFileInfo + 'integers'
      else
        FFileInfo := FFileInfo + 'floats' ;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0) ;
    end ;
  end;

  function  TGIS_LayerBT.setFileScale( const _dwidth : Double ;
                                       const _swidth : Double ) : Double ;
  begin
    if _swidth <> 0 then
      Result := _dwidth / _swidth
    else
      Result := 1 ;

    if (Result > 1) OR (Result = 0) then Result := 1 ;
    actualZoom := Result ;

  end ;

  procedure TGIS_LayerBT.prepareMinMaxZ( const _zoom : Double = -1 ) ;
  var
    i, k    : Integer ;
    zoom    : Double ;
    bytes   : Int64 ;
    columns : Double ;
    fact : Double ;
    fbuff : Array of Single ;
    sbuff : Array of SmallInt ;
    {$IFDEF OXYGENE}
      j   : Integer ;
    {$ENDIF}
    offset : Int64 ;
  const
    MAX_COLUMNS = 200 ;
  begin
    if dataSize = 2 then begin
      SetLength(sbuff, FBitHeight) ;
      bytes := FBitHeight * sizeOf(SmallInt) ;
    end
    else begin
      SetLength(fbuff, FBitHeight) ;
      bytes := FBitHeight * sizeOf(Single) ;
    end ;

    if (_zoom > 0) AND (_zoom <= 1) then begin
      columns := RoundS(FBitWidth * _zoom) ;
      if columns = 0 then
        columns := 1 ;
    end
    else
    begin
      if MAX_COLUMNS > FBitWidth then
        columns := FBitWidth
      else
        columns := MAX_COLUMNS ;
    end ;

    zoom := columns/FBitWidth ;
    fact := zoom/2 ;

    for i := 0 to FBitWidth -1 do begin
      fact := fact + zoom ;
      if fact >= 1 then begin
        fact := fact - 1 ;
        offset := bytes * i + BT_HEADER_SIZE  ;
        fileStream.Seek( offset, soBeginning ) ;
        if dataSize = 2 then
          {$IFDEF OXYGENE}
            for j := 0 to FBitHeight-1 do
              fileStream.ReadSmallInt( sbuff[j], 2 )
          {$ELSE}
            fileStream.Read( sbuff[0], bytes )
          {$ENDIF}
        else
          {$IFDEF OXYGENE}
            for j := 0 to FBitHeight-1 do
              fileStream.ReadSingle( fbuff[j], 4 ) ;
          {$ELSE}
            fileStream.Read( fbuff[0], bytes ) ;
          {$ENDIF}
        for k := 0 to FBitHeight -1 do begin
          if dataSize = 2 then
          begin
            if sbuff[k] < FMinZ then
              FMinZ := sbuff[k] ;
            if sbuff[k] > FMaxZ then
              FMaxZ := sbuff[k] ;
          end
          else
          begin
            if fbuff[k] < FMinZ then
              FMinZ := fbuff[k] ;
            if fbuff[k] > FMaxZ then
              FMaxZ := fbuff[k] ;
          end ;
        end ;
      end ;
    end ;

    if dataSize = 2 then
      SetLength(sbuff, 0)
    else
      SetLength(fbuff, 0) ;
  end ;

  function TGIS_LayerBT.getLine( const _buffer : TBytes  ;
                                 const _offset : Integer ;
                                 const _linenr : Integer ;
                                 const _start  : Integer ;
                                 const _bytes  : Integer
                               ) : Integer ;
  var
    i, k    : Integer;
    start  : Integer ;
    line : Integer ;
    pixels : Integer ;
    offset : Integer ;
    ccolor : TGIS_Color ;
    zstart : Double ;
  begin
    try

      pixels := _bytes div 3 ;
      line   := FBitHeight - RoundS(_linenr/actualZoom) -1 ;
      if line < 0 then
        line := 0 ;
      Result := _bytes ;
      start := RoundS((_start div 3) / actualZoom);

      if (lineInBuffer <> line) OR (lastZoom <> actualZoom)  then begin
        k := 0 ;
        zstart := actualZoom/2 ;
        if dataSize = 4 then begin
          offset := (line +start*FBitHeight)*sizeOf(Single) + BT_HEADER_SIZE;
          for i := 0 to FBitWidth -1 do begin
            zstart := zstart + actualZoom ;
            if zstart >= 1 then begin
              fileStream.Seek( offset, soBeginning ) ;
              fileStream.ReadSingle( lineBuffer[k], sizeOf(Single) ) ;
              zstart := zstart -1 ;
              inc(k) ;
              if k = pixels then
                break ;
            end ;
            offset := offset +FBitHeight*sizeOf(Single) ;
          end ;
        end
        else begin
          offset := (line +start*FBitHeight)*sizeOf(SmallInt) + BT_HEADER_SIZE;
          for i := 0 to FBitWidth -1 do begin
            zstart := zstart + actualZoom ;
            if zstart >= 1 then begin
              fileStream.Seek( offset, soBeginning ) ;
              fileStream.ReadSmallInt( sLineBuffer[k], sizeOf(SmallInt) ) ;
              zstart := zstart -1 ;
              inc(k) ;
              if k = pixels then
                break ;
            end ;
            offset := offset +FBitHeight*sizeOf(SmallInt) ;
          end ;
        end ;

        lastZoom := actualZoom ;
        lineInBuffer := line ;
      end ;

      offset := 0 ;
      for i := 0 to pixels -2 do begin
        if dataSize = 2 then
          begin
            ccolor := GetColorRamp( sLineBuffer[i] ) ;
            _buffer[_offset + offset    ] := ccolor.B ;
            _buffer[_offset + offset + 1] := ccolor.G ;
            _buffer[_offset + offset + 2] := ccolor.R ;
          end
        else
        begin
          ccolor := GetColorRamp( lineBuffer[i] ) ;
         _buffer[_offset + offset    ] := ccolor.B ;
          _buffer[_offset + offset + 1] := ccolor.G ;
          _buffer[_offset + offset + 2] := ccolor.R ;
         end ;
      end ;

      // last triple

      if dataSize = 2 then
        ccolor := GetColorRamp( sLineBuffer[pixels -1] )
      else
        ccolor := GetColorRamp( lineBuffer[pixels -1] ) ;


      _buffer[_offset + offset    ] := ccolor.B ;
      _buffer[_offset + offset + 1] := ccolor.G ;
      _buffer[_offset + offset + 2] := ccolor.R ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  function  TGIS_LayerBT.getNativeValue( const _pt : TPoint  ;
                                         const _ar : TGIS_DoubleArray
                                       ) : Boolean ;

  var
    offset : Integer ;
    sval   : SmallInt ;
    fval   : Single ;
  begin
    try
      Result := True ;

      if dataSize = 2 then begin
        offset := ((_pt.X +1)*FBitHeight -_pt.Y)*sizeOf(SmallInt) +BT_HEADER_SIZE ;
        fileStream.Seek( offset, soBeginning ) ;
        {$IFDEF OXYGENE}
          fileStream.ReadSmallInt( sval, sizeOf(SmallInt) ) ;
        {$ELSE}
          fileStream.Read( (Addr(sval))^, sizeOf(SmallInt)) ;
        {$ENDIF}
        _ar[0] := sval ;
      end
      else begin
        offset := ((_pt.X +1)*FBitHeight -_pt.Y)*sizeOf(Single) +BT_HEADER_SIZE;
        fileStream.Seek( offset, soBeginning ) ;
        {$IFDEF OXYGENE}
          fileStream.ReadSingle( fval, sizeOf(Single) ) ;
        {$ELSE}
          fileStream.Read( (Addr(fval))^, sizeOf(Single));
        {$ENDIF}
        _ar[0] := fval ;
      end ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  function TGIS_LayerBT.getNativeLine( const _buffer   : TGIS_SingleArray ;
                                       const _linenr   : Integer          ;
                                       const _startIdx : Integer          ;
                                       const _count    : Integer
                                     ) : Integer ;
  var
    i, k    : Integer;
    start  : Integer ;
    line : Integer ;
    pixels : Integer ;
    offset : Int64 ;
    zstart : Double ;
  begin
    try
      pixels := _count ;

      line   := FBitHeight - RoundS(_linenr/actualZoom) -1 ;
      if line < 0 then
        line := 0 ;
      if not assigned(lineBuffer) then
        SetLength(lineBuffer, FBitWidth) ;

      Result := pixels ;
      start := RoundS(_startIdx/actualZoom) ;

      if (lineInBuffer <> line) OR (lastZoom <> actualZoom)  then begin
        k := 0 ;
        zstart := actualZoom/2 ;
        if dataSize = 4 then begin
          offset := (line +start*FBitHeight)*sizeOf(Single) + BT_HEADER_SIZE;
          for i := 0 to FBitWidth -1 do begin
            zstart := zstart + actualZoom ;
            if zstart >= 1 then begin
              fileStream.Seek( offset, soBeginning ) ;
              fileStream.ReadSingle( lineBuffer[k], sizeOf(Single) ) ;
              zstart := zstart -1 ;
              inc(k) ;
              if k = pixels then
                break ;
            end ;
            offset := offset +FBitHeight*sizeOf(Single) ;
          end ;
        end
        else begin
          offset := (line +start*FBitHeight)*sizeOf(SmallInt) + BT_HEADER_SIZE;
          for i := 0 to FBitWidth -1 do begin
            zstart := zstart + actualZoom ;
            if zstart >= 1 then begin
              fileStream.Seek( offset, soBeginning ) ;
              fileStream.ReadSmallInt( sLineBuffer[k], sizeOf(SmallInt) ) ;
              zstart := zstart -1 ;
              inc(k) ;
              if k = pixels then
                break ;
            end ;
            offset := offset +FBitHeight*sizeOf(SmallInt) ;
          end ;

          for k := 0 to pixels -1 do
            lineBuffer[k] :=  sLineBuffer[k] ;

        end ;

        lastZoom := actualZoom ;
        lineInBuffer := line ;
      end ;

      for k := 0 to pixels -1 do
        _buffer[k] :=  lineBuffer[k] ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  { Perform initialization section.
  }
  class procedure GisLayerBT.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-BT', 'Binary Terrian Grid', TGIS_LayerBT, '.bt',
                   TGIS_RegisteredLayerType.Grid, TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                    True
                 ) ;
  end ;

{$IFNDEF OXYGENE}
  initialization
    GisLayerBT.SelfRegisterLayer() ;
{$ENDIF}

{==================================== END =====================================}
end.

