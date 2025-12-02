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
  Encapsulation of Digital Terrain Elevation Data layer levels 0, 1 and 2
  (*.dt0, *.dt1, *.dt2).
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerDTED ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerDTED"'}
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
    System.Math,
    System.Types,
    Lider.CG.GIS.GeoTypes,
    
    Lider.CG.GIS.GeoLayerPixel ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  GisLayerDTED = class
    public
      class procedure SelfRegisterLayer() ;
  end;

  /// <summary>
  ///   Encapsulation of Binary Terrian Grid layer.
  /// </summary>
  TGIS_LayerDTED = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerPixel )

    private // various private variables

      /// <summary>
      ///   Suitable (for level 0, 1 or 2) value from DATA_RECORD_LENGTH array
      /// </summary>
      dataRecordSize : Integer ;

      /// <summary>
      ///   Local buffer
      /// </summary>
      lineBuffer : Array of SmallInt ;

      workBuff : Array of SmallInt ;

      /// <summary>
      ///   Zoom in lastreading
      /// </summary>
      lastZoom : Double ;

      /// <summary>
      ///   Last used in setZoom function
      /// </summary>
      actualZoom : Double ;

      /// <summary>
      ///   Last set view rectangle
      /// </summary>
      lastviewrect  : TRect    ;

      /// <summary>
      ///   Strip buffer
      /// </summary>
      viewBuffer : Array of SmallInt ;
//From Grid

      /// <summary>
      ///   Number of lines present in lineBuffer.
      /// </summary>
      lineInBuffer  : Integer ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF} // various protected routines

      /// <summary>
      ///   Convert from big endian (signed) to little endian format.
      /// </summary>
      /// <param name="_length">
      ///   line length
      /// </param>      
      procedure convertLineToLE( const _length : Integer )  ;

      /// <inheritdoc/>
      procedure setUp          ; override;

      /// <inheritdoc/>
      function  setFileScale   ( const _dwidth : Double ;
                                 const _swidth : Double ) : Double ; override;

      /// <inheritdoc/>
      procedure setFileView ( const _viewRect    : TRect) ; override;

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
      function  getNativeLine  ( const _buffer   : TGIS_SingleArray ;
                                 const _linenr   : Integer          ;
                                 const _startIdx : Integer          ;
                                 const _count    : Integer
                               ) : Integer ; override;

      /// <inheritdoc/>
      procedure prepareMinMaxZ ( const _zoom : Double = -1
                               ) ; override ;
    protected
      // destructor

      /// <inheritdoc/>
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
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoStreams,
    Lider.CG.GIS.GeoRegistredLayers,
    Lider.CG.GIS.GeoCsSystems ;
{$ENDIF}

const
  FIRST_REC_OFFSET =  3428 ;
  CHECKSUM_SIZE    =  4 ;
  MIN_SHORT        = -32766 ;
  MAX_SHORT        =  32767 ;
  NO_VALUE         = -32767 ;
  EXT_NO_VALUE     = -32500 ;
  SIZE_OF_DTADESCRIPTION = 8 ;

type
  { DTED Library interface structure types.
  }

  T_UserHeaderLabel = record
      rsUHL         : Array [0..2]  of Byte ; //'UHL' - recognition sentinel
      nUsed0        : Byte ;
      longitudeWS   : Array [0..7]  of Byte ;
      latitudeWS    : Array [0..7]  of Byte ;
      lonInterval   : Array [0..3]  of Byte ;
      latInterval   : Array [0..3]  of Byte ;
      absVertAccur  : Array [0..3]  of Byte ;
      securCode     : Array [0..2]  of Byte ;
      uniqueRef     : Array [0..11] of Byte ;
      noLonLines    : Array [0..3]  of Byte ;
      noLatPoints   : Array [0..3]  of Byte ;
      multAccuracy  : Byte ; // '0' Single; '1' Multiple
      nUsed1        : Array [0..23] of Byte ;
    {$IFDEF OXYGENE}
      function Read ( _stream : TGIS_BaseStream ) : Integer ;
    {$ENDIF}
  end ;
  {$IFDEF OXYGENE}
  {$ELSE}
    PT_UserHeaderLabel = ^T_UserHeaderLabel ;
  {$ENDIF}

  T_DataSetIdentification = record
      rsDSI          : Array [0..2]  of Byte ; // 'DSI' - rocognition sentinel
      securClassCode  : Byte ; // 'S' secret; 'C' confidentional
                             // 'U' unclassified; 'R' restricted
      nUsed0          : Array [0..1]  of Byte ;
      securDesc       : Array [0..26] of Byte ; // Free text or blank filled
      nUsed1          : Array [0..25] of Byte ; // Blank filled
      seriesDesig     : Array [0..4]  of Byte ; //'DTED0'
      uniqueRefNo     : Array [0..14] of Byte ; //Free text or zero filled
      nUsed2          : Array [0..7]  of Byte ;
      dataEditionNo   : Array [0..1]  of Byte ;
      mergeVersion    : Byte ;
      maintenanceDate : Array [0..3]  of Byte ; // YYMM
      mergeDate       : Array [0..3]  of Byte ; // YYMM
      mDCode          : Array [0..3]  of Byte ; // '0000' or 'ANNN'
      productCode     : Array [0..7]  of Byte ; // CCAAABBB - country free code
      nUsed3          : Array [0..15] of Byte ;
      productSpec     : Array [0..8]  of Byte ; // Alfanumeric field
      amendmentNo     : Byte ;
      changeNo        : Byte ;
      productSpecDate : Array [0..3]  of Byte ; // YYMM
      vrticalDatum    : Array [0..2]  of Byte ; //'MSL' or 'E96'
      horizDatumCode  : Array [0..4]  of Byte ; //'WSG84'
      digitSystem     : Array [0..9]  of Byte ; // Free text
      compilDate      : Array [0..3]  of Byte ; // YYMM
      nUsed33         : Array [0..21] of Byte ;
      latOfOrig10     : Array [0..8]  of Byte ; // DDMMSS.SH  for val < 10
                                              // ; H hemisfere
      lonOfOrig100    : Array [0..9]  of Byte ; // DDDMMSS.SH  for val < 100
                                              // ; H hemisfere
      latOfSW10       : Array [0..6]  of Byte ; // DDMMSS.SH  for val < 10
                                              // ; H hemisfere
      lonOfSW100      : Array [0..7]  of Byte ; // DDDMMSS.SH  for val < 100
                                              // ; H hemisfere
      latOfNW10       : Array [0..6]  of Byte ; // DDMMSS.SH  for val < 10
                                              // ; H hemisfere
      lonOfNW100      : Array [0..7]  of Byte ; // DDDMMSS.SH  for val < 100
                                              // ; H hemisfere
      latOfNE10       : Array [0..6]  of Byte ; // DDMMSS.SH  for val < 10
                                              // ; H hemisfere
      lonOfNE100      : Array [0..7]  of Byte ; // DDDMMSS.SH  for val < 100
                                              // ; H hemisfere
      latOfSE10       : Array [0..6]  of Byte ; // DDMMSS.SH  for val < 10
                                              // ; H hemisfere
      lonOfSE100      : Array [0..7]  of Byte ; // DDDMMSS.SH  for val < 100
                                              // ; H hemisfere
      cwOrientAngle   : Array [0..8]  of Byte ;
      latInterval     : Array [0..3]  of Byte ; // in tenths of seconds
      lonInterval     : Array [0..3]  of Byte ; // in tenths of seconds
      latLinesNo      : Array [0..3]  of Byte ; // rows count e.g. 1201 for DTED1
      lonLinesNo      : Array [0..3]  of Byte ; // columns count
      partialCellInd  : Array [0..1]  of Byte ; //'00' full one-degree or 01-99 %
      nUsed4          : Array [0..100] of Byte ;
      nUsed5          : Array [0..99] of Byte ;
      nUsed6          : Array [0..155] of Byte ;
    {$IFDEF OXYGENE}
      function Read ( _stream : TGIS_BaseStream ) : Integer ;
    {$ENDIF}
  end ;
  {$IFDEF OXYGENE}
  {$ELSE}
    PT_DataSetIdentification = ^T_DataSetIdentification ;
  {$ENDIF}

{$IFDEF OXYGENE}
//==============================================================================
// T_UserHeaderLabel
//==============================================================================

  {$IFDEF OXYGENE}
    function T_UserHeaderLabel.Read ( _stream : TGIS_BaseStream ) : Integer ;
  {$ELSE}
    function T_UserHeaderLabel.Read ( _stream : TStream ) : Integer ;
  {$ENDIF}
  begin
    {$IFDEF OXYGENE}
      rsUHL        := new Byte[ 3] ;
      longitudeWS  := new Byte[ 8] ;
      latitudeWS   := new Byte[ 8] ;
      lonInterval  := new Byte[ 4] ;
      latInterval  := new Byte[ 4] ;
      absVertAccur := new Byte[ 4] ;
      securCode    := new Byte[ 3] ;
      uniqueRef    := new Byte[12] ;
      noLonLines   := new Byte[ 4] ;
      noLatPoints  := new Byte[ 4] ;
      nUsed1       := new Byte[24] ;
    {$ENDIF}

    Result := _stream.Read( rsUHL,        0, 3  ) +
              _stream.ReadByte( nUsed0,          1  ) +
              _stream.Read( longitudeWS,  0, 8  ) +
              _stream.Read( latitudeWS,   0, 8  ) +
              _stream.Read( lonInterval,  0, 4  ) +
              _stream.Read( latInterval,  0, 4  ) +
              _stream.Read( absVertAccur, 0, 4  ) +
              _stream.Read( securCode,    0, 3  ) +
              _stream.Read( uniqueRef,    0, 12 ) +
              _stream.Read( noLonLines,   0, 4  ) +
              _stream.Read( noLatPoints,  0, 4  ) +
              _stream.ReadByte( multAccuracy, 1     ) +
              _stream.Read( nUsed1,       0, 24 ) ;
  end ;

//==============================================================================
// T_DataSetIdentification
//==============================================================================

  {$IFDEF OXYGENE}
    function T_DataSetIdentification.Read ( _stream : TGIS_BaseStream ) : Integer ;
  {$ELSE}
    function T_DataSetIdentification.Read ( _stream : TStream ) : Integer ;
  {$ENDIF}
  begin
    {$IFDEF OXYGENE}
      rsDSI           := new Byte[  3] ;
      nUsed0          := new Byte[  2] ;
      securDesc       := new Byte[ 27] ;
      nUsed1          := new Byte[ 26] ;
      seriesDesig     := new Byte[  5] ;
      uniqueRefNo     := new Byte[ 15] ;
      nUsed2          := new Byte[  8] ;
      dataEditionNo   := new Byte[  2] ;
      maintenanceDate := new Byte[  4] ;
      mergeDate       := new Byte[  4] ;
      mDCode          := new Byte[  4] ;
      productCode     := new Byte[  8] ;
      nUsed3          := new Byte[ 16] ;
      productSpec     := new Byte[  9] ;
      productSpecDate := new Byte[  4] ;
      vrticalDatum    := new Byte[  3] ;
      horizDatumCode  := new Byte[  5] ;
      digitSystem     := new Byte[ 10] ;
      compilDate      := new Byte[  4] ;
      nUsed33         := new Byte[ 22] ;
      latOfOrig10     := new Byte[  9] ;
      lonOfOrig100    := new Byte[  9] ;
      latOfSW10       := new Byte[  7] ;
      lonOfSW100      := new Byte[  8] ;
      latOfNW10       := new Byte[  7] ;
      lonOfNW100      := new Byte[  8] ;
      latOfNE10       := new Byte[  7] ;
      lonOfNE100      := new Byte[  8] ;
      latOfSE10       := new Byte[  7] ;
      lonOfSE100      := new Byte[  8] ;
      cwOrientAngle   := new Byte[  9] ;
      latInterval     := new Byte[  4] ;
      lonInterval     := new Byte[  4] ;
      latLinesNo      := new Byte[  4] ;
      lonLinesNo      := new Byte[  4] ;
      partialCellInd  := new Byte[  2] ;
      nUsed4          := new Byte[101] ;
      nUsed5          := new Byte[100] ;
      nUsed6          := new Byte[156] ;
    {$ENDIF}

    Result := _stream.Read( rsDSI,           0, 3   ) +
              _stream.ReadByte( securClassCode,     1   ) +
              _stream.Read( nUsed0,          0, 2   ) +
              _stream.Read( securDesc,       0, 27  ) +
              _stream.Read( nUsed1,          0, 26  ) +
              _stream.Read( seriesDesig,     0, 5   ) +
              _stream.Read( uniqueRefNo,     0, 15  ) +
              _stream.Read( nUsed2,          0, 8   ) +
              _stream.Read( dataEditionNo,   0, 2   ) +
              _stream.ReadByte( mergeVersion,    1      ) +
              _stream.Read( maintenanceDate, 0, 4   ) +
              _stream.Read( mergeDate,       0, 4   ) +
              _stream.Read( mDCode,          0, 4   ) +
              _stream.Read( productCode,     0, 8   ) +
              _stream.Read( nUsed3,          0, 16  ) +
              _stream.Read( productSpec,     0, 9   ) +
              _stream.ReadByte( amendmentNo,     1      ) +
              _stream.ReadByte( changeNo,        1      ) +
              _stream.Read( productSpecDate, 0, 4   ) +
              _stream.Read( vrticalDatum,    0, 3   ) +
              _stream.Read( horizDatumCode,  0, 5   ) +
              _stream.Read( digitSystem,     0, 10  ) +
              _stream.Read( compilDate,      0, 4   ) +
              _stream.Read( nUsed33,         0, 22  ) +
              _stream.Read( latOfOrig10,     0, 9   ) +
              _stream.Read( lonOfOrig100,    0, 9   ) +
              _stream.Read( latOfSW10,       0, 7   ) +
              _stream.Read( lonOfSW100,      0, 8   ) +
              _stream.Read( latOfNW10,       0, 7   ) +
              _stream.Read( lonOfNW100,      0, 8   ) +
              _stream.Read( latOfNE10,       0, 7   ) +
              _stream.Read( lonOfNE100,      0, 8   ) +
              _stream.Read( latOfSE10,       0, 7   ) +
              _stream.Read( lonOfSE100,      0, 8   ) +
              _stream.Read( cwOrientAngle,   0, 9   ) +
              _stream.Read( latInterval,     0, 4   ) +
              _stream.Read( lonInterval,     0, 4   ) +
              _stream.Read( latLinesNo,      0, 4   ) +
              _stream.Read( lonLinesNo,      0, 4   ) +
              _stream.Read( partialCellInd,  0, 2   ) +
              _stream.Read( nUsed4,          0, 101 ) +
              _stream.Read( nUsed5,          0, 100 ) +
              _stream.Read( nUsed6,          0, 156 )
  end ;
{$ENDIF}

//==============================================================================
// TGIS_LayerDTED
//==============================================================================

  constructor TGIS_LayerDTED.Create ;
  begin
    inherited ;
    FSubType := FSubType + [TGIS_LayerSubType.Persistent] ;
    FIsGridImage   := True;
    FIsNativeGridImage := True ;
    FBandsCount := 1 ;
  end ;

  procedure TGIS_LayerDTED.doDestroy ;
  begin

    inherited ;
  end ;

  procedure TGIS_LayerDTED.convertLineToLE(const _length : Integer) ;
  var
    i : Integer ;
    {$IFDEF OXYGENE}
    {$ELSE}
      b : Byte    ;
      p : Pointer ;
    {$ENDIF}
  begin
    for i:= 0 to _length - 1 do begin
      {$IFDEF OXYGENE}
        lineBuffer[i] := SwapSmallint( lineBuffer[i] ) ;
      {$ELSE}
        p := @lineBuffer[i] ;
        b                        := PByte( NativeInt(p) + 0 )^ ;
        PByte( NativeInt(p) + 0 )^ := PByte( NativeInt(p) + 1 )^ ;
        PByte( NativeInt(p) + 1 )^ := b ;
      {$ENDIF}
    end ;
  end ;

  procedure TGIS_LayerDTED.setUp ;
  var
    ext             : TGIS_Extent ;
    hdr             : T_UserHeaderLabel ;
    dsi             : T_DataSetIdentification ;
    sgn             : TValueSign ;
    ddd, mm, ss     : Double ;

      stmp          : Array [0..2] of Byte ;
    x_interval,
    y_interval      : Double ;
    i               : Integer ;
  begin
    Params.Pixel.GridNoValue := NO_VALUE ;
    FNoDataValue := Params.Pixel.GridNoValue ;

    ext := GisExtent( 0,0,0,0 ) ;
    try

      scaleX := 0 ;
      scaleY := 0 ;
      Extent := ext;

      realBitCount := 24 ;
      colorsNo := 0 ;
      actualZoom := 1 ;
      lastZoom := 1 ;

      // open binary file
      fileStream := openBufferedFileStream( Path ) ;

      {$IFDEF OXYGENE}
        hdr.Read( fileStream ) ;
      {$ELSE}
        fileStream.Read(hdr, sizeOf(hdr)) ;
      {$ENDIF}
        if ConvertAnsiString( hdr.rsUHL ) <> 'UHL' then
          Abort ;

      {$IFDEF OXYGENE}
        dsi.Read( fileStream ) ;
      {$ELSE}
        fileStream.Read(dsi, sizeOf(dsi)) ;
      {$ENDIF}
        if ConvertAnsiString( dsi.rsDSI ) <> 'DSI' then
          Abort ;

      FBitWidth  := RoundS( DotStrToFloat( ConvertAnsiString(hdr.noLonLines ) ) ) ;
      FBitHeight := RoundS( DotStrToFloat( ConvertAnsiString(hdr.noLatPoints) ) ) ;

      FAntialias := True ;
      dataRecordSize := SIZE_OF_DTADESCRIPTION + FBitHeight*sizeOf(SmallInt) +
                        CHECKSUM_SIZE ;

      realLineWidth := ( FBitWidth*realBitCount +7 ) div 8 ;
      intLineWidth := FBitWidth * 3 ;

      SetLength( lineBuffer, FBitWidth ) ;

      lineInBuffer := -1 ;
      // coordinate of WS point
      // longitude
      for i := 0 to 2 do
        stmp[i] := hdr.longitudeWS[i] ;
      ddd := DotStrToFloat( ConvertAnsiString( stmp ) ) ;

      stmp[0] := Byte( '0' ) ;
      for i := 0 to 1 do
            stmp[i+1] := hdr.longitudeWS[i+3] ;
      mm := DotStrToFloat( ConvertAnsiString( stmp ) ) ;

      for i := 0 to 1 do
        stmp[i+1] := hdr.longitudeWS[i+5] ;
      ss := DotStrToFloat( ConvertAnsiString( stmp ) ) ;

      if hdr.longitudeWS[7] = Byte('W') then
        sgn := -1
      else
        sgn := 1 ;

      ext.XMin := GisEncodeLongitude(ddd, mm, ss, sgn) ;

      // latitude
      for i := 0 to 2 do
        stmp[i] := hdr.latitudeWS[i] ;
      ddd := DotStrToFloat( ConvertAnsiString( stmp ) ) ;

      stmp[0] := Byte( '0' ) ;
      for i := 0 to 1 do
        stmp[i+1] := hdr.latitudeWS[i+3] ;
      mm := DotStrToFloat( ConvertAnsiString( stmp ) ) ;

      for i := 0 to 1 do
        stmp[i+1] := hdr.latitudeWS[i+5] ;
      ss := DotStrToFloat( ConvertAnsiString( stmp ) ) ;

      if hdr.latitudeWS[7] = Byte('S') then
        sgn := -1
      else
        sgn := 1 ;

      ext.YMin := GisEncodeLatitude(ddd, mm, ss, sgn) ;

      ss := DotStrToFloat(ConvertAnsiString(hdr.lonInterval)) / 10 ;
      x_interval := GisEncodeLongitude(0, 0, ss) ;

      ss := DotStrToFloat(ConvertAnsiString(hdr.latInterval)) / 10 ;
      y_interval := GisEncodeLatitude(0, 0, ss) ;

      scaleX := x_interval  ;
      scaleY := -y_interval ;

      ext.XMax := ext.XMin + FBitWidth  * scaleX ;
      ext.YMax := ext.YMin - FBitHeight * scaleY ;

      ext.XMin := RadToDeg( ext.XMin ) ;
      ext.YMin := RadToDeg( ext.YMin ) ;
      ext.XMax := RadToDeg( ext.XMax ) ;
      ext.YMax := RadToDeg( ext.YMax ) ;

      Extent := ext ;

      if ConvertAnsiString( dsi.horizDatumCode ) = 'WGS84' then
        CS := CSGeographicCoordinateSystemList.ByEPSG( GIS_EPSG_WGS84 )
      else
      if ConvertAnsiString( dsi.horizDatumCode ) = 'WGS72' then
        CS := CSGeographicCoordinateSystemList.ByEPSG( GIS_EPSG_WGS72 ) ;

      if FMaxZ = FMinZ then begin
        FMaxZ   := MIN_SHORT  ;
        FMinZ   := MAX_SHORT  ;
        prepareMinMaxZ(1.0) ;
      end;

      Extent3D := GisExtent3D( Extent.XMin, Extent.YMin, FMinZ,
                               Extent.XMax, Extent.YMax, FMaxZ
                              ) ;

      inherited ;

      if SafeFileExists( Path ) then
        FAge := GisFileAge( Path ) ;

      FFileInfo := Format(
        'DTED - Digital Terrain Elevation Data %d x %d, 2 byte integers',
                          [ FBitWidth, FBitHeight ]
                         ) ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0) ;
    end ;
  end;

  function  TGIS_LayerDTED.setFileScale( const _dwidth : Double ;
                                         const _swidth : Double ) : Double ;
  begin
    if _swidth <> 0 then
      Result := _dwidth / _swidth
    else
      Result := 1 ;

    if (Result > 1) OR (Result = 0) then Result := 1 ;
    if Abs(actualZoom - Result) > (1/(2*FBitWidth)) then
      actualZoom := Result
    else
      Result := actualZoom ;

  end ;

  procedure TGIS_LayerDTED.setFileView( const _viewRect : TRect ) ;
  var
    t, b : Integer ;
    h, zh : Integer ;
    work_size : Integer ;
    vwb_size : Integer ;
    offset : Int64 ;
    i, k, dk, sidx, bsidx : Integer;
    rows, columns : Integer ;
    column_start : Integer ;
    {$IFDEF OXYGENE}
    {$ELSE}
      ppd, pps : Pointer ;
    {$ENDIF}
    drn : Integer ;
 begin
    if (lastviewrect.Top    <= _viewRect.Top)    and
       (lastviewrect.Bottom >= _viewRect.Bottom) and
       (lastviewrect.Left   <= _viewRect.Left)   and
       (lastviewrect.Right  >= _viewRect.Right)  and
       (lastZoom = actualZoom )
    then
      exit ;
    t := _viewRect.Top ;
    h := (_viewRect.Bottom -_viewRect.Top +2) ;
    zh := RoundS((FBitHeight + actualZoom -1) / actualZoom) ;
    b := t +h ;
    if b >= zh then
      b := zh -1 ;
    lastviewrect := Rect(_viewRect.Left, t, _viewRect.Right, b) ;

    column_start := RoundS(_viewRect.Left/actualZoom) ;

    work_size := dataRecordSize * FBitWidth ;
    SetLength(workBuff, work_size) ;

    columns := lastviewrect.Right -lastviewrect.Left +1 ;
    rows := lastviewrect.Bottom -lastviewrect.Top +1 ;
    vwb_size := columns * rows ;

    SetLength(viewBuffer, vwb_size) ;

    offset := FIRST_REC_OFFSET + SIZE_OF_DTADESCRIPTION ;
    drn := dataRecordSize div sizeOf(SmallInt) ;
    {$IFDEF OXYGENE}
      fileStream.Seek( offset, soBeginning ) ;
      for i := 0 to (FBitWidth*drn) -1 do
        fileStream.ReadSmallInt( workBuff[i], sizeOf(Short) ) ;
    {$ELSE}
      fileStream.Seek( offset, 0 ) ;
      fileStream.Read( workBuff[0], work_size -SIZE_OF_DTADESCRIPTION) ;
    {$ENDIF}
    for i := 0 to rows -1 do begin
      dk := i*columns ;
      bsidx := (FBitHeight -RoundS((t+i)/actualZoom) -1) +drn*column_start ;
      if bsidx < 0 then
        bsidx := 0 ;

      for k := 0 to columns -1 do begin
         sidx := bsidx +RoundS(k/actualZoom)*drn ;
         if sidx >= work_size then
           continue ;

        {$IFDEF OXYGENE}
          viewBuffer[k +dk] := SwapSmallint(workBuff[sidx]) ;
        {$ELSE}
          ppd := @viewBuffer[k +dk] ;
          pps := @workBuff[sidx] ;
          PByte( NativeInt(ppd) + 0 )^ := PByte( NativeInt(pps) + 1 )^ ;
          PByte( NativeInt(ppd) + 1 )^ := PByte( NativeInt(pps) + 0 )^ ;
        {$ENDIF}
      end;
    end;

    SetLength(workBuff, 0) ;
    lastZoom := actualZoom ;
  end;

  procedure TGIS_LayerDTED.prepareMinMaxZ(const _zoom : Double = -1.0) ;
  var
    i, k    : Integer ;
    {$IFDEF OXYGENE}
      j     : Integer ;
    {$ENDIF}
    zoom    : Double ;
    {$IFDEF OXYGENE}
    {$ELSE}
      bytes : Integer ;
    {$ENDIF}
    columns : Double ;
    fact : Double ;
  const
    MAX_COLUMNS = 200 ;
  begin

    if FBitHeight > FBitWidth then
      SetLength(lineBuffer, FBitHeight) ;

    {$IFDEF OXYGENE}
    {$ELSE}
      bytes := FBitHeight * sizeOf(SmallInt) ;
    {$ENDIF}

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

        fileStream.Seek( Int64(i)*dataRecordSize + FIRST_REC_OFFSET
                         + SIZE_OF_DTADESCRIPTION, soBeginning ) ;

        {$IFDEF OXYGENE}
          for j := 0 to FBitHeight-1 do
            fileStream.ReadSmallInt( lineBuffer[j], sizeOf(Short) ) ;
        {$ELSE}
          fileStream.Read( lineBuffer[0], bytes ) ;
        {$ENDIF}
        convertLineToLE(FBitHeight) ;

        for k := 0 to FBitHeight -1 do begin
          if lineBuffer[k] > EXT_NO_VALUE then begin
            if lineBuffer[k] < FMinZ then
              FMinZ := lineBuffer[k] ;
            if lineBuffer[k] > FMaxZ then
              FMaxZ := lineBuffer[k] ;
          end
          else
            lineBuffer[k] := lineBuffer[k] ;
        end ;
      end ;
    end ;

    if FBitHeight > FBitWidth then
      SetLength(lineBuffer, FBitWidth) ;

  end ;

  function TGIS_LayerDTED.getLine( const _buffer : TBytes  ;
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
    offset : Int64 ;
    ccolor : TGIS_Color ;
    zstart : Double ;
  begin
    try

      pixels := _bytes div 3 ;

      Result := _bytes ;
      start := (_start div 3) ;

      if (length(viewBuffer) > 0) and (actualZoom = lastZoom) then begin
        line := _linenr ;
        if line > lastviewrect.Bottom then
          line := lastviewrect.Bottom ;
        start := start -lastviewrect.Left
                 +(line -lastviewrect.Top)*(lastviewrect.Right -lastviewrect.Left +1) ;
        offset := 0 ;
        for i := 0 to pixels -2 do begin
          ccolor := GetColorRamp( viewBuffer[start +i] ) ;
          _buffer[_offset + offset    ] := ccolor.B ;
          _buffer[_offset + offset + 1] := ccolor.G ;
          _buffer[_offset + offset + 2] := ccolor.R ;
          offset := offset +3 ;
        end;
        // last triple
        ccolor := GetColorRamp( viewBuffer[start +pixels -1] ) ;
        _buffer[_offset + offset    ] := ccolor.B ;
        _buffer[_offset + offset + 1] := ccolor.G ;
        _buffer[_offset + offset + 2] := ccolor.R ;
        exit ;
      end;

      line   := FBitHeight - RoundS(_linenr/actualZoom) -1;
      if line < 0 then
        line := 0 ;

      if (lineInBuffer <> line) OR (lastZoom <> actualZoom)  then begin
        zstart := actualZoom/2 ;
        offset := Int64(line)*sizeOf(SmallInt) + FIRST_REC_OFFSET
                  + SIZE_OF_DTADESCRIPTION ;
        k := 0 ;
        for i := 0 to FBitWidth -1 do begin
          zstart := zstart + actualZoom ;
          if zstart >= 1 then begin
            {$IFDEF OXYGENE}
              fileStream.Seek( offset, soBeginning ) ;
            {$ELSE}
              fileStream.Seek( offset, 0 ) ;
            {$ENDIF}
            fileStream.ReadSmallInt( lineBuffer[k], sizeOf(SmallInt) ) ;
            zstart := zstart -1 ;
            inc(k) ;
          end ;
          offset := offset +dataRecordSize ;
        end ;
        lineInBuffer := line ;
        lastZoom := actualZoom ;
        convertLineToLE(k) ;
      end ;

      offset := 0 ;
      for i := start to start +pixels -2 do begin
        ccolor := GetColorRamp( lineBuffer[i] ) ;
        _buffer[_offset + offset    ] := ccolor.B ;
        _buffer[_offset + offset + 1] := ccolor.G ;
        _buffer[_offset + offset + 2] := ccolor.R ;
      offset := offset +3 ;
      end ;

      // last triple

      ccolor := GetColorRamp( lineBuffer[start +pixels -1] ) ;
      _buffer[_offset + offset    ] := ccolor.B ;
      _buffer[_offset + offset + 1] := ccolor.G ;
      _buffer[_offset + offset + 2] := ccolor.R ;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  function  TGIS_LayerDTED.getNativeValue( const _pt : TPoint  ;
                                           const _ar : TGIS_DoubleArray
                                       ) : Boolean ;

  var
    offset : Int64 ;
    line   : Integer ;
    sval     : SmallInt ;
    aval     : SmallInt ;
    {$IFDEF OXYGENE}
    {$ELSE}
      pb     : PByte ;
    {$ENDIF}
    is_minus : Boolean ;
  begin
    try
      Result := True ;

      if (length(viewBuffer) > 0) and (actualZoom = lastZoom) then begin
        line := _pt.Y ;
        if line > lastviewrect.Bottom then
          line := lastviewrect.Bottom ;
        offset := Int64(_pt.X) -lastviewrect.Left
                 +Int64(line -lastviewrect.Top)*(lastviewrect.Right -lastviewrect.Left +1) ;
        sval := viewBuffer[offset] ;
       _ar[0]  := sval
      end
      else begin
        offset := Int64(FBitHeight -_pt.Y -1)*sizeOf(SmallInt) + FIRST_REC_OFFSET
                  + SIZE_OF_DTADESCRIPTION +_pt.X*dataRecordSize ;

        {$IFDEF OXYGENE}
          fileStream.Seek( offset, soBeginning ) ;
        {$ELSE}
          fileStream.Seek( offset, 0 ) ;
        {$ENDIF}
        fileStream.ReadSmallInt( sval, sizeOf(SmallInt)) ;

        {$IFDEF OXYGENE}
        {$ELSE}
          pb := Addr(sval) ;
        {$ENDIF}

        {$IFDEF OXYGENE}
          if (Byte(sval) AND $A0) <> 0 then begin
        {$ELSE}
          if (pb^ AND $A0) <> 0 then begin
        {$ENDIF}
          {$IFDEF OXYGENE}
            aval := (SmallInt(Byte(sval) AND $7F)) SHL 8 ;
          {$ELSE}
            aval := (SmallInt(pb^ AND $7F)) SHL 8 ;
          {$ENDIF}
          is_minus := True ;
        end
        else begin
          {$IFDEF OXYGENE}
            aval := SmallInt(Byte(sval)) SHL 8;
          {$ELSE}
            aval := SmallInt(pb^) SHL 8;
          {$ENDIF}
          is_minus := False ;
        end ;
        {$IFDEF OXYGENE}
        {$ELSE}
          Inc(pb) ;
        {$ENDIF}

        {$IFDEF OXYGENE}
          aval := aval OR Byte(sval shr 8) ;
        {$ELSE}
          aval := aval OR pb^ ;
        {$ENDIF}

        if is_minus then
          _ar[0]  := -aval
        else
          _ar[0]  := aval ;
      end;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  function TGIS_LayerDTED.getNativeLine( const _buffer   : TGIS_SingleArray ;
                                         const _linenr   : Integer          ;
                                         const _startIdx : Integer          ;
                                         const _count    : Integer
                                       ) : Integer ;
  var
    i, k    : Integer;
    line : Integer ;
    offset : Int64 ;
    start : Integer ;
    zstart : Double ;
    stidx : Integer ;
    count : Integer ;
  begin
    try
      Result := _count ;
      stidx := _startIdx ;
      count := _count ;

      if (length(viewBuffer) > 0) and (actualZoom = lastZoom) then begin
        line := _linenr ;
        if line > lastviewrect.Bottom then
          line := lastviewrect.Bottom ;
        if (stidx +_count) > lastviewrect.Right then begin
          stidx := lastviewrect.Right -count ; ;
          if stidx < 0 then begin
            count := count +stidx ;
            stidx := 0 ;
            if count <= 0 then
              exit ;
          end;
        end ;
        start := stidx -lastviewrect.Left
                 +(line -lastviewrect.Top)*(lastviewrect.Right -lastviewrect.Left +1) ;
        if start < 0 then
          start := 0 ;

        for i := 0 to count -1 do begin
          _buffer[i] := viewBuffer[i + start] ;
        end;
        exit ;
      end;

      line   := FBitHeight - RoundS(_linenr/actualZoom) -1;
      if line < 0 then
        line := 0 ;

      if (lineInBuffer <> line) OR (lastZoom <> actualZoom)  then begin
        zstart := actualZoom/2 ;
        offset :=Int64(line)*sizeOf(SmallInt) + FIRST_REC_OFFSET
                  + SIZE_OF_DTADESCRIPTION ;
        k := 0 ;
        for i := 0 to FBitWidth -1 do begin
          zstart := zstart + actualZoom ;
          if zstart >= 1 then begin
            {$IFDEF OXYGENE}
              fileStream.Seek( offset, soBeginning ) ;
            {$ELSE}
              fileStream.Seek( offset, 0 ) ;
            {$ENDIF}
            fileStream.ReadSmallInt( lineBuffer[k], sizeOf(SmallInt) ) ;
            zstart := zstart -1 ;
            inc(k) ;
          end ;
          offset := offset +dataRecordSize ;
        end ;
        lineInBuffer := line ;
        lastZoom := actualZoom ;
        convertLineToLE(k) ;
      end ;

      for i := 0 to _count - 1 do
        _buffer[i] := lineBuffer[i + _startIdx] ;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  { Perform initialization section.
  }
  class procedure GisLayerDTED.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-DTED', 'Digital Terrain Elevation Data', TGIS_LayerDTED,
                   '.dt0;.dt1;.dt2',
                   TGIS_RegisteredLayerType.Grid, TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                   True
                 ) ;
  end;

{$IFNDEF OXYGENE}
  initialization
    GisLayerDTED.SelfRegisterLayer() ;
{$ENDIF}

{==================================== END =====================================}
end.

