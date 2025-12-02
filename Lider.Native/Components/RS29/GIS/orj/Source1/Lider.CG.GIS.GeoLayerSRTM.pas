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
  Encapsulation of SRTM HGT Grid layer.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerSRTM ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerSRTM"'}
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
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.SysUtils,
    System.Types,

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoResource,
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
  GisLayerSRTM = class
    public
      class procedure SelfRegisterLayer() ;
  end;

  /// <summary>
  ///   Encapsulation of an Arcinfo Float(Binary) Grid layer.
  /// </summary>
  TGIS_LayerSRTM = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerPixel )
    private // various private variables

      /// <summary>
      ///   line buffer.
      /// </summary>
      lineBuffer    : array of SmallInt ;

      /// <summary>
      ///   Number of lines present in lineBuffer.
      /// </summary>
      lineInBuffer  : Integer ;

      /// <summary>
      ///   Must set min and max Z value
      /// </summary>
      mustPrepareMinMaxZ : Boolean ;
    private

      /// <summary>
      ///   Convert from big endian to little endian format.
      /// </summary>
      procedure convertLineToLE  ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // various protected routines

      /// <inheritdoc/>
      procedure setUp          ; override;


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
      procedure prepareMinMaxZ ( const _zoom : Double = -1
                               ) ; override ;
      /// <inheritdoc/>
      function  getNativeLine  ( const _buffer   : TGIS_SingleArray ;
                                 const _linenr   : Integer          ;
                                 const _startIdx : Integer          ;
                                 const _count    : Integer
                               ) : Integer ; override;
    public
      // constructors

      /// <inheritdoc/>
      constructor Create  ; override;
  end ;


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoRegistredLayers,
    Lider.CG.GIS.GeoStreams ;
{$ENDIF}

//==============================================================================
// TGIS_LayerSRTM
//==============================================================================

  constructor TGIS_LayerSRTM.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent ] ;
  end ;

  procedure TGIS_LayerSRTM.setUp ;
  var
    fname     : String ;
    swlat     : Integer ;
    swlon     : Integer ;
    numpixels : Integer ;
    ext       : TGIS_Extent {$IFDEF GIS_NORECORDS} = TGIS_Extent.Create {$ENDIF};
  begin
    try
      fname := GetFileName( Path ) ;

      // decode extent by file name NxxWxxx.ght
      swlat := StrToInt( Copy( fname, StringFirst+1, 2 ) ) ;
      swlon := StrToInt( Copy( fname, StringFirst+4, 3 ) ) ;

      if UpCase( fname[StringFirst] ) = 'N' then
        // southWestLat = southWestLat
      else if UpCase( fname[StringFirst] ) = 'S' then
        swlat := swlat * -1
      else
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEBADFORMAT ), Path, 1 ) ;

      if UpCase( fname[StringFirst+3] ) = 'E' then
        // southWestLon = southWestLon
      else if UpCase( fname[StringFirst+3] ) = 'W' then
        swlon := swlon * -1
      else
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEBADFORMAT ), Path, 2 ) ;

      // open binary file
      fileStream := TGIS_FileStream.Create( Path, 
                                            fmOpenRead or 
                                            fmShareDenyWrite
                                          ) ;

      if fileStream.Size = 25934402 then
        numpixels := 3601
      else if fileStream.Size = 2884802 then
        numpixels := 1201
      else
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEBADFORMAT ), Path, 3 ) ;

      SetLength( lineBuffer, numpixels ) ;

      FBitWidth           := numpixels ;
      FBitHeight          := numpixels ;
      FBandsCount         := 1 ;
      FIsGridImage        := True ;
      FIsNativeGridImage  := True ;
      FBandsCount         := 1 ;
      FAntialias := True ;
      Params.Pixel.GridNoValue := -32768 ;
      FNoDataValue := Params.Pixel.GridNoValue ;

      redTransp[0]    := BASE_TRANSPARENT_FLAG ;
      greenTransp[0]  := BASE_TRANSPARENT_FLAG ;
      blueTransp[0]   := BASE_TRANSPARENT_FLAG ;

      ext.XMin := swlon - 0.5 / (numpixels - 1) ;
      ext.YMax := swlat + 1 + 0.5 / (numpixels - 1) ;
      scaleX   := 1.0 / (numpixels-1) ;
      scaleY   := -1.0 / (numpixels-1) ;
      if scaleY > 0 then
        scaleY := -scaleY ;

      if scaleX <> 0 then
        ext.XMax := ext.XMin + scaleX * numpixels
      else
        ext.XMax := ext.XMin + numpixels ;

      if scaleY <> 0 then
        ext.YMin := ext.YMax + scaleY * numpixels
      else
        ext.YMin := ext.YMax + numpixels ;

      Extent := _TGIS_Extent(ext) ;
      SetCSByEPSG( GIS_EPSG_WGS84 ) ;

      realBitCount := 24 ;
      colorsNo := 0 ;

      realLineWidth := ( FBitWidth*realBitCount +7 ) div 8 ;
      intLineWidth  := FBitWidth * 3 ;

      lineInBuffer := -1 ;
      if FMinZ >= FMaxZ then begin
        FMinZ   :=  GIS_MAX_SINGLE  ;
        FMaxZ   := -GIS_MAX_SINGLE  ;
        mustPrepareMinMaxZ := True ;
      end
      else
        mustPrepareMinMaxZ := False ;

      Extent3D := GisExtent3D( Extent.XMin, Extent.YMin, FMinZ,
                               Extent.XMax, Extent.YMax, FMaxZ
                              ) ;

      inherited ;

      if SafeFileExists( Path ) then
        FAge := GisFileAge( Path ) ;

      FFileInfo := Format( 'SRTM Grid Format (HGT)' + #13#10 +
                           '%d x %d',
                           [ FBitWidth, FBitHeight ]
                         ) ;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0) ;
    end ;
  end;

  procedure TGIS_LayerSRTM.prepareMinMaxZ(
    const _zoom : Double = -1
  ) ;
  var
    i, k    : Integer ;
    {$IFDEF OXYGENE}
      j     : Integer ;
    {$ENDIF}
    zoom    : Double ;
    bytes   : Integer ;
    lines   : Double ;
    fact    : Double ;
  const
    maxlines = 300 ;
  begin
    mustPrepareMinMaxZ := False ;

    bytes := FBitWidth * sizeOf(SmallInt) ;
    if (_zoom > 0) AND (_zoom <= 1) then begin
      lines := RoundS( FBitHeight * _zoom ) ;
      if lines = 0 then
        lines := 1 ;
    end
    else begin
      if maxlines > FBitHeight then
        lines := FBitHeight
      else
        lines := maxlines ;
    end ;

    zoom := lines/FBitHeight ;
    fact := zoom/2 ;

    for i := 0 to FBitHeight -1 do begin
      fact := fact + zoom ;
      if fact >= 1 then begin
        fact := fact - 1 ;
        {$IFDEF OXYGENE}
          fileStream.Seek( bytes * i, soBeginning ) ;
        {$ELSE}
          fileStream.Seek( bytes * i, 0 ) ;
        {$ENDIF}
        {$IFDEF OXYGENE}
          for j := 0 to FBitWidth-1 do
            fileStream.ReadSmallInt( lineBuffer[j], sizeOf(SmallInt) ) ;
        {$ELSE}
          fileStream.Read( lineBuffer[0], bytes ) ;
        {$ENDIF}

        convertLineToLE ;
        for k := 0 to FBitWidth -1 do begin
          if lineBuffer[k] <> NoDataValue then begin
            if lineBuffer[k] < FMinZ then
              FMinZ := lineBuffer[k] ;
            if lineBuffer[k] > FMaxZ then
              FMaxZ := lineBuffer[k] ;
          end ;
        end ;
      end ;
    end ;
    FExtent3D.ZMin := FMinZ ;
    FExtent3D.ZMax := FMaxZ ;

  end ;

  procedure TGIS_LayerSRTM.convertLineToLE  ;
  var
    i : Integer ;
    {$IFDEF OXYGENE}
    {$ELSE}
      dwval : DWord ;
    {$ENDIF}
  begin
    for i := 0 to FBitWidth-1 do begin
      {$IFDEF OXYGENE}
        lineBuffer[i] := SwapSmallint( lineBuffer[i] ) ;
      {$ELSE}
        dwval := SwapShort2LongInt( PDWord( Addr( lineBuffer[i] ) )^ ) ;
        lineBuffer[i] := PSmallInt( Addr(dwval) )^ ;
      {$ENDIF}
    end ;
  end ;

  function TGIS_LayerSRTM.getLine(
   const _buffer : TBytes  ;
   const _offset : Integer ;
   const _linenr : Integer ;
   const _start  : Integer ;
   const _bytes  : Integer
 ) : Integer ;
  var
    i    : Integer;
    {$IFDEF OXYGENE}
      j  : Integer ;
    {$ENDIF}
    start  : Integer ;
    line   : Integer ;
    pixels : Integer ;
    offset : Integer ;
    ccolor : TGIS_Color ;
  begin
    try
      pixels := _bytes div 3 ;
      line   := _linenr ;
      Result := _bytes ;

      if lineInBuffer <> line then begin
        if mustPrepareMinMaxZ then
          prepareMinMaxZ ;

        offset := (line*baseCellWidth*sizeOf(SmallInt)) ;
        {$IFDEF OXYGENE}
          fileStream.Seek( offset, soBeginning ) ;
        {$ELSE}
          fileStream.Seek( offset, 0 ) ;
        {$ENDIF}
        {$IFDEF OXYGENE}
          for j := 0 to baseCellWidth-1 do
            fileStream.ReadSmallInt( lineBuffer[j], sizeOf(SmallInt) ) ;
        {$ELSE}
          fileStream.Read( ( Addr( lineBuffer[0]))^, baseCellWidth * sizeOf(SmallInt) ) ;
        {$ENDIF}
        convertLineToLE ;
        lineInBuffer := line ;
      end ;

      start  := (_start div 3) ;
      offset := 0 ;
      pixels := pixels +start ;

      for i := start to pixels -2 do begin
        if lineBuffer[i] = NoDataValue then begin
          ccolor := colorNoData ;
          _buffer[_offset + offset    ] := ccolor.B ;
          _buffer[_offset + offset + 1] := ccolor.G ;
          _buffer[_offset + offset + 2] := ccolor.R ;
          makeTransparent := True ;
        end
        else begin
          ccolor := GetColorRamp( lineBuffer[i] ) ;
          _buffer[_offset + offset    ] := ccolor.B ;
          _buffer[_offset + offset + 1] := ccolor.G ;
          _buffer[_offset + offset + 2] := ccolor.R ;
        end ;
        offset := offset +3 ;
      end ;

      // last triple
      if lineBuffer[pixels -1] = NoDataValue then begin
        ccolor := colorNoData ;
        makeTransparent := True ;
      end
      else
        ccolor := GetColorRamp( lineBuffer[pixels -1] ) ;

      _buffer[_offset + offset    ] := ccolor.B ;
      _buffer[_offset + offset + 1] := ccolor.G ;
      _buffer[_offset + offset + 2] := ccolor.R ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  function  TGIS_LayerSRTM.getNativeValue(
    const _pt : TPoint  ;
    const _ar : TGIS_DoubleArray
   ) : Boolean ;
  var
    start  : Integer ;
    offset : Integer ;
    sval   : SmallInt ;
    {$IFDEF OXYGENE}
    {$ELSE}
      dwval  : DWord ;
    {$ENDIF}
  begin
    Result := True ;
    try
      start  := _pt.X*2 ;
      offset := (_pt.Y*baseCellWidth*sizeOf(SmallInt)) +start ;
      {$IFDEF OXYGENE}
        fileStream.Seek( offset, soBeginning ) ;
      {$ELSE}
        fileStream.Seek( offset, 0 ) ;
      {$ENDIF}
      {$IFDEF OXYGENE}
        fileStream.ReadSmallInt( sval, sizeOf(SmallInt) ) ;
      {$ELSE}
        fileStream.Read( (Addr(sval))^, sizeOf(SmallInt));
      {$ENDIF}

      {$IFDEF OXYGENE}
        _ar[0] := SwapSmallint( sval ) ;
      {$ELSE}
        dwval := SwapShort2LongInt(PDWord(Addr(sval))^) ;
        _ar[0] := PSmallInt(Addr(dwval))^ ;
      {$ENDIF}

      if _ar[0] = NoDataValue then
        Result := False ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  function TGIS_LayerSRTM.getNativeLine(
    const _buffer   : TGIS_SingleArray ;
    const _linenr   : Integer          ;
    const _startIdx : Integer          ;
    const _count    : Integer
  ) : Integer ;
  var
    offset : Integer ;
    i      : Integer ;
  begin
    try
      Result := 0 ;

      if (_linenr < 0) or (_linenr > FBitHeight) then exit ;

      if lineInBuffer <> _linenr then begin
        if mustPrepareMinMaxZ then
          prepareMinMaxZ ;

        offset := (_linenr*baseCellWidth*sizeOf(SmallInt)) ;
        {$IFDEF OXYGENE}
          fileStream.Seek( offset, soBeginning ) ;
        {$ELSE}
          fileStream.Seek( offset, 0 ) ;
        {$ENDIF}
        {$IFDEF OXYGENE}
          for i := 0 to baseCellWidth-1 do
            fileStream.ReadSmallInt( lineBuffer[i], sizeOf(SmallInt) ) ;
        {$ELSE}
          fileStream.Read( ( Addr(lineBuffer[0]))^, baseCellWidth * sizeOf(SmallInt) ) ;
        {$ENDIF}
        convertLineToLE ;
        for i := 0 to _count-1 do
          _buffer[i] := lineBuffer[_startIdx +i] ;
        lineInBuffer := _linenr ;
      end ;

      Result := _count ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;


//==============================================================================
// Lider.CG.GIS.GeoLayerSRTM
//==============================================================================

  class procedure GisLayerSRTM.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-SRTM', 'SRTM HGT Grid', TGIS_LayerSRTM, '.hgt',
                   TGIS_RegisteredLayerType.Grid, TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                   False
                 ) ;
  end ;

//==============================================================================
// initialization/finalization
//==============================================================================

{$IFDEF DCC}
  initialization
    GisLayerSRTM.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.

