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
  Encapsulation of an Arcinfo internal Grid layer (integer values only).
}

{$IFDEF DCC}
  unit GisLayerADF ;
  {$HPPEMIT '#pragma link "GisLayerADF"'}
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
    System.Classes,
    System.SysUtils,
    System.Types,

    GisLayerPixel,
    GisTypes,
    GisStreams;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerADF = class
    public
      class procedure SelfRegisterLayer() ;
  end;


  /// <summary>
  ///   Encapsulation of an Arcinfo Float(Binary) Grid layer.
  /// </summary>
  TGIS_LayerADF = {$IFDEF OXYGENE} public {$ENDIF}  class( TGIS_LayerPixel )

    private // various private variables

        /// <summary>
        ///   Index file name.
        /// </summary>
        idxFileName : String ;

        /// <summary>
        ///   Data files directory
        /// </summary>
        fdir        : String ;

        /// <summary>
        ///   Array with short names all data files
        /// </summary>
        filesShortNames : Array of String ;

        /// <summary>
        ///   Parts number divided in files
        /// </summary>
        baseFilesNumber : Integer ;

        /// <summary>
        ///   Actual part idx
        /// </summary>
        actualFilesIdx  : Integer ;

        idxStream : TGIS_HandleStream ;

        tileWidth : Integer ;

        /// <summary>
        ///   Tiles Length
        /// </summary>
        tileLength : Integer ;

        /// <summary>
        ///   Tile columns number
        /// </summary>
        tilesColumns : Integer ;

        /// <summary>
        ///   Tiles rows number
        /// </summary>
        tilesRows : Integer ;

        /// <summary>
        ///   In memory list of image tiles infos in Grid file.
        /// </summary>
        tilesInfoList : TGIS_ObjectList ;

        /// <summary>
        ///
        /// </summary>
        dataType  : DWORD     ;

        /// <summary>
        ///   Number of line present in lineBuffer.
        /// </summary>
        lineInBuffer  : Integer ;

        /// <summary>
        ///   Last pixel X - used by locate
        /// </summary>
        lpixelX : Integer ;

        /// <summary>
        ///   Last pixel Y - used by locate
        /// </summary>
        lpixelY : Integer ;

        /// <summary>
        ///   line buffer (line of singles).
        /// </summary>
        lineBuffer : array of Single ;

        /// <summary>
        ///   Byte order from most significant to least significant. If True
        ///   then msbfirst. If false then lsbfirst.
        /// </summary>
        bigEndian : Boolean ;

        /// <summary>
        ///   Buffer with NoDataValue flag
        /// </summary>
        noDataInBuffer : Boolean ;

    private

      {$IFDEF OXYGENE}

        /// <summary>
        ///   Reading data procedure.
        /// </summary>
        procedure getConstLine      ( const _buffer : TGIS_SingleArray ;
                                      const _offset : Integer ;
                                      const _linenr : Integer ;
                                      const _obj    : TObject
                                    ) ;
      {$ELSE}

        /// <summary>
        ///   Reading data procedure.
        /// </summary>
        procedure getConstLine      ( const _buffer : PSingle ;
                                      const _linenr : Integer ;
                                      const _obj    : TObject
                                    ) ;
      {$ENDIF}
      {$IFDEF OXYGENE}

        /// <summary>
        ///   Reading data procedure.
        /// </summary>
        procedure get1BitLine       ( const _buffer : TGIS_SingleArray ;
                                      const _offset : Integer ;
                                      const _linenr : Integer ;
                                      const _obj    : TObject
                                    ) ;
      {$ELSE}

        /// <summary>
        ///   Reading data procedure.
        /// </summary>
        procedure get1BitLine       ( const _buffer : PSingle   ;
                                      const _linenr : Integer ;
                                      const _obj    : TObject
                                    ) ;
      {$ENDIF}
      {$IFDEF OXYGENE}

        /// <summary>
        ///   Reading data procedure.
        /// </summary>
        procedure get4BitLine       ( const _buffer : TGIS_SingleArray ;
                                      const _offset : Integer ;
                                      const _linenr : Integer ;
                                      const _obj    : TObject
                                    ) ;
      {$ELSE}

        /// <summary>
        ///   Reading data procedure.
        /// </summary>
        procedure get4BitLine       ( const _buffer : PSingle   ;
                                      const _linenr : Integer ;
                                      const _obj    : TObject
                                    ) ;
      {$ENDIF}
      {$IFDEF OXYGENE}

        /// <summary>
        ///   Reading data procedure.
        /// </summary>
        procedure get8BitLine       ( const _buffer : TGIS_SingleArray ;
                                      const _offset : Integer ;
                                      const _linenr : Integer ;
                                      const _obj    : TObject
                                    ) ;
      {$ELSE}

        /// <summary>
        ///   Reading data procedure.
        /// </summary>
        procedure get8BitLine       ( const _buffer : PSingle   ;
                                      const _linenr : Integer ;
                                      const _obj    : TObject
                                    ) ;
      {$ENDIF}
      {$IFDEF OXYGENE}

        /// <summary>
        ///   Reading data procedure.
        /// </summary>
        procedure get16BitLine      ( const _buffer : TGIS_SingleArray ;
                                      const _offset : Integer ;
                                      const _linenr : Integer ;
                                      const _obj    : TObject
                                    ) ;
      {$ELSE}

        /// <summary>
        ///   Reading data procedure.
        /// </summary>
        procedure get16BitLine      ( const _buffer : PSingle   ;
                                      const _linenr : Integer ;
                                      const _obj    : TObject
                                    ) ;
      {$ENDIF}
      {$IFDEF OXYGENE}

        /// <summary>
        ///   Reading data procedure.
        /// </summary>
        procedure getFloatLine      ( const _buffer : TGIS_SingleArray ;
                                      const _offset : Integer ;
                                      const _linenr : Integer ;
                                      const _obj    : TObject
                                    ) ;
      {$ELSE}

        /// <summary>
        ///   Reading data procedure.
        /// </summary>
        procedure getFloatLine      ( const _buffer : PSingle   ;
                                      const _linenr : Integer ;
                                      const _obj    : TObject
                                    ) ;
      {$ENDIF}
      {$IFDEF OXYGENE}

        /// <summary>
        ///   Reading data procedure.
        /// </summary>
        procedure getLiteralRunsLineCF
                                    ( const _buffer : TGIS_SingleArray ;
                                      const _offset : Integer ;
                                      const _linenr : Integer ;
                                      const _obj    : TObject
                                    ) ;
      {$ELSE}

        /// <summary>
        ///   Reading data procedure.
        /// </summary>
        procedure getLiteralRunsLineCF
                                    ( const _buffer : PSingle   ;
                                      const _linenr : Integer ;
                                      const _obj    : TObject
                                    ) ;
      {$ENDIF}
      {$IFDEF OXYGENE}

        /// <summary>
        ///   Reading data procedure.
        /// </summary>
        procedure getLiteralRunsLineD7
                                    ( const _buffer : TGIS_SingleArray ;
                                      const _offset : Integer ;
                                      const _linenr : Integer ;
                                      const _obj    : TObject
                                    ) ;
      {$ELSE}

        /// <summary>
        ///   Reading data procedure.
        /// </summary>
        procedure getLiteralRunsLineD7
                                    ( const _buffer : PSingle   ;
                                      const _linenr : Integer ;
                                      const _obj    : TObject
                                    ) ;
      {$ENDIF}
      {$IFDEF OXYGENE}
        procedure getLiteralRunsLineDF
                                    ( const _buffer : TGIS_SingleArray ;
                                      const _offset : Integer ;
                                      const _linenr : Integer ;
                                      const _obj    : TObject
                                    ) ;
      {$ELSE}

        /// <summary>
        ///   Reading data procedure.
        /// </summary>
        procedure getLiteralRunsLineDF
                                    ( const _buffer : PSingle   ;
                                      const _linenr : Integer ;
                                      const _obj    : TObject
                                    ) ;
      {$ENDIF}
      {$IFDEF OXYGENE}

        /// <summary>
        ///   Reading data procedure.
        /// </summary>
        procedure getLengthEcodedLine
                                    ( const _buffer : TGIS_SingleArray ;
                                      const _offset : Integer ;
                                      const _linenr : Integer ;
                                      const _obj    : TObject
                                    ) ;
      {$ELSE}

        /// <summary>
        ///   Reading data procedure.
        /// </summary>
        procedure getLengthEcodedLine
                                    ( const _buffer : PSingle   ;
                                      const _linenr : Integer ;
                                      const _obj    : TObject
                                    ) ;
      {$ENDIF}
      {$IFDEF OXYGENE}

        /// <summary>
        ///   Reading data procedure.
        /// </summary>
        procedure get1BitCCITTRLELine
                                    ( const _buffer : TGIS_SingleArray ;
                                      const _offset : Integer ;
                                      const _linenr : Integer ;
                                      const _obj    : TObject
                                    ) ;
      {$ELSE}

        /// <summary>
        ///   Reading data procedure.
        /// </summary>
        procedure get1BitCCITTRLELine
                                    ( const _buffer : PSingle   ;
                                      const _linenr : Integer ;
                                      const _obj    : TObject
                                    ) ;
      {$ENDIF}

      /// <summary>
      ///   Reading an image line.
      /// </summary>
      /// <param name="_linenr">
      ///   line number
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FILEREAD
      /// </exception>
      procedure lineToBuffer        ( const _linenr : Integer ;
                                      const _pixsta : Integer ;
                                      const _pixels : Integer
                                    ) ;

    // various protected routine
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <summary>
      ///   Inits CCITT RLE file decoder.
      /// </summary>
      /// <param name="_tileIdx">
      ///   tile index
      /// </param>      
      procedure initDecoder    ( const _tileIdx : Integer
                               ) ;

      /// <summary>
      ///   Inits TileInfo structure from tilesInfoList. List has tilesColumns
      ///   objects.
      /// </summary>
      /// <param name="_tileIdx">
      ///   tile index
      /// </param>      
      procedure initTileInfo   ( const _tileIdx : Integer
                               ) ;

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
      function  getNativeLine    ( const _buffer   : TGIS_SingleArray ;
                                   const _linenr   : Integer          ;
                                   const _startIdx : Integer          ;
                                   const _count    : Integer
                                 ) : Integer ; override;

      /// <inheritdoc/>
      procedure prepareMinMaxZ ( const _zoom : Double = -1
                               ) ; override ;

    protected

      procedure doDestroy         ; override;

    public // API
      // constructors

      /// <inheritdoc/>
      constructor Create          ; override;

    public // various public routines

      /// <inheritdoc/>
      procedure Alive             ; override;

      /// <inheritdoc/>
      function  DormantGain       : Integer ; override;

      /// <inheritdoc/>
      procedure Dormant           ; override;
  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    GisRegistredLayers,
    GisFunctions,
    GisInternals,
    GisTypesUI,
    GisCsEsri,
    GisClasses,
    GisRtl,
    GisFileTIFF,
    GisResource
      {$IFDEF OXYGENE}
        ,System.IO
      {$ENDIF} ;
{$ENDIF}

const
  DATA_FILE_NAME        = 'w001001.adf'   ;
  INDEX_FILE_NAME       = 'w001001x.adf'  ;
  BOUNDS_FILE_NAME      = 'dblbnd.adf'    ;
  STATISTICS_FILE_NAME  = 'sta.adf'       ;
  HEADER_FILE_NAME      = 'hdr.adf'       ;
  PROJECTION_FILE_NAME  = 'prj.adf'       ;

  ESRI_GRID_FLOAT_NO_DATA = -340282346638528859811704183484516925440.0 ;
  ESRI_GRID_NO_DATA       = -2147483647 ;
  ESRI_INTEGER_DATA       = 1 ;
  ESRI_FLOAT_DATA         = 2 ;

type
  {$IFDEF OXYGENE}
    T_GetTileLineFunc = procedure( const _buffer : TGIS_SingleArray ;
                                   const _offset : Integer ;
                                   const _linenr : Integer ;
                                   const _obj    : TObject
                                 ) of object ;
  {$ELSE}
    T_GetTileLineFunc = procedure( const _buffer : PSingle ;
                                   const _linenr : Integer ;
                                   const _obj    : TObject
                                 ) of object ;
  {$ENDIF}

  { Single tile information.
  }
  T_TileInfo = class
    minVal          : Integer                   ;
    tileIdx         : Integer                   ;
    tileDecoder     : TGIS_FileTIFFDecoder      ;
    listStripInfo   : {$IFDEF OXYGENE} array of TGIS_FileTIFF_StripInfo64 
                      {$ELSE} TArray<TGIS_FileTIFF_StripInfo64>
                      {$ENDIF}  ; //only 1 strip
    localWidth      : Integer                   ;
    dataType        : Byte                      ;
    initialized     : Boolean                   ;
  public
    GetTileLineFunc : T_GetTileLineFunc ;

    constructor Create ;
  end ;

{$IFDEF OXYGENE}
  const
{$ELSE}
  var
{$ENDIF}
  lt_types : Array [0..15] of Byte = ($33, $33, $33, $33, $33, $33, $33, $33,
                                      $33, $33, $33, $33, $33, $33, $33, $33 ) ;

var
  lt_types_count : Array [0..15] of Integer ;

  { T_TileInfo }

  constructor T_TileInfo.Create;
  begin
    inherited ;

    SetLength( listStripInfo, 1 ) ;
    {$IFDEF JAVA}
      listStripInfo[0] := new TGIS_FileTIFF_StripInfo64 ;
    {$ENDIF}
  end ;

//==============================================================================
// TGIS_LayerADF
//==============================================================================

  constructor TGIS_LayerADF.Create ;
  begin
    inherited ;
    FSubType := FSubType + [TGIS_LayerSubType.Persistent] ;
    FIsGridImage   := True;
    FIsNativeGridImage := True ;
    FBandsCount := 1 ;
    bigEndian := False ;
  end ;

  procedure TGIS_LayerADF.doDestroy ;
  begin
    Dormant ;

    inherited ;
  end ;

  {$IFDEF OXYGENE}

    procedure TGIS_LayerADF.getConstLine( const _buffer : TGIS_SingleArray ;
                                          const _offset : Integer ;
                                          const _linenr : Integer ;
                                          const _obj    : TObject
                                        ) ;
  {$ELSE}

    procedure TGIS_LayerADF.getConstLine( const _buffer : PSingle   ;
                                          const _linenr : Integer ;
                                          const _obj    : TObject
                                        ) ;
  {$ENDIF}
  var
    {$IFDEF OXYGENE}
      sbuffer  : array of Single ;
      sbufferx : Integer ;
    {$ELSE}
      sbuffer : PSingle ;
    {$ENDIF}
    i : Integer ;
  begin
    sbuffer := _buffer ;
    {$IFDEF OXYGENE}
      sbufferx := _offset ;
    {$ENDIF}
    for i := 1 to tileWidth do begin
      {$IFDEF OXYGENE}
        sbuffer[sbufferx] := T_TileInfo(_obj).minVal ;
        inc(sbufferx) ;
      {$ELSE}
        sbuffer^ := T_TileInfo(_obj).minVal ;
        inc(sbuffer) ;
      {$ENDIF}
    end ;
  end ;

  {$IFDEF OXYGENE}

    procedure TGIS_LayerADF.get1BitLine( const _buffer : TGIS_SingleArray ;
                                         const _offset : Integer ;
                                         const _linenr : Integer ;
                                         const _obj    : TObject
                                       ) ;
  {$ELSE}

    procedure TGIS_LayerADF.get1BitLine( const _buffer : PSingle   ;
                                         const _linenr : Integer ;
                                         const _obj    : TObject
                                       ) ;
  {$ENDIF}
  var
    {$IFDEF OXYGENE}
      sbuffer   : array of Single ;
      sbufferx  : Integer ;
      srcptr    : TBytes  ;
      srcptrx   : Integer ;
    {$ELSE}
      sbuffer   : PSingle ;
      srcptr    : PByte ;
    {$ENDIF}
    i         : Integer ;
    bytes     : Integer ;
    pixels    : Integer ;
    bdata     : Array of Byte ;
    mask      : Byte ;
  begin
    pixels := T_TileInfo(_obj).localWidth ;
    bytes := (pixels +7) div 8 ;

    SetLength(bdata, bytes) ;
    fileStream.Seek( T_TileInfo(_obj).listStripInfo[0].StripOffsets +
                     DWORD(_linenr)*DWORD(bytes),
                     soBeginning ) ;
    {$IFDEF OXYGENE}
      fileStream.Read( bdata, bytes ) ;
    {$ELSE}
      fileStream.Read(bdata[0], bytes) ;
    {$ENDIF}

    {$IFDEF OXYGENE}
      srcptr  := bdata ;
      srcptrx := 0 ;
    {$ELSE}
      srcptr := Addr(bdata[0]) ;
    {$ENDIF}
    sbuffer := _buffer ;
    {$IFDEF OXYGENE}
      sbufferx := _offset ;
    {$ENDIF}
    mask := $80 ;
    for i := 0 to pixels -1 do begin
      {$IFDEF OXYGENE}
        if (srcptr[srcptrx] AND mask) = mask then
      {$ELSE}
        if (srcptr^ AND mask) = mask then
      {$ENDIF}
        {$IFDEF OXYGENE}
          sbuffer[sbufferx] := T_TileInfo(_obj).minVal +1
        {$ELSE}
          sbuffer^ := T_TileInfo(_obj).minVal +1
        {$ENDIF}
      else
        {$IFDEF OXYGENE}
          sbuffer[sbufferx] := T_TileInfo(_obj).minVal ;
        {$ELSE}
          sbuffer^ := T_TileInfo(_obj).minVal ;
        {$ENDIF}
      {$IFDEF OXYGENE}
        inc(sbufferx) ;
      {$ELSE}
        inc(sbuffer) ;
      {$ENDIF}
      mask := mask SHR 1 ;
      if mask = 0 then begin
        {$IFDEF OXYGENE}
          inc(srcptrx) ;
        {$ELSE}
          inc(srcptr) ;
        {$ENDIF}
        mask := $80 ;
      end ;
    end ;

    SetLength(bdata, 0) ;
  end ;

  {$IFDEF OXYGENE}

    procedure TGIS_LayerADF.get4BitLine( const _buffer : TGIS_SingleArray ;
                                         const _offset : Integer ;
                                         const _linenr : Integer ;
                                         const _obj    : TObject
                                       ) ;
  {$ELSE}

    procedure TGIS_LayerADF.get4BitLine( const _buffer : PSingle   ;
                                         const _linenr : Integer ;
                                         const _obj    : TObject
                                       ) ;
  {$ENDIF}
  var
    {$IFDEF OXYGENE}
      sbuffer  : TGIS_SingleArray ;
      sbufferx : Integer ;
    {$ELSE}
      sbuffer : PSingle ;
    {$ENDIF}
    i         : Integer ;
    bytes     : Integer ;
    pixels    : Integer ;
    bdata     : Array of Byte ;
  begin
    sbuffer := _buffer ;
    {$IFDEF OXYGENE}
      sbufferx := _offset ;
    {$ENDIF}
    pixels := T_TileInfo(_obj).localWidth ;
    bytes := (pixels +1) div 2 ;

    SetLength(bdata, bytes) ;
    
    fileStream.Seek( T_TileInfo(_obj).listStripInfo[0].StripOffsets +
                     DWORD(_linenr)*DWORD(bytes),
                     soBeginning ) ;
    {$IFDEF OXYGENE}
      fileStream.Read( bdata, bytes ) ;
    {$ELSE}
      fileStream.Read(bdata[0], bytes) ;
    {$ENDIF}

    for i := 0 to pixels -1 do begin
      {$IFDEF OXYGENE}
        sbuffer[sbufferx] := T_TileInfo(_obj).minVal +
                     (bdata[i div 2] SHR ((i AND $01) SHL 2 )) AND $0F ;
        inc(sbufferx) ;
      {$ELSE}
        sbuffer^ := T_TileInfo(_obj).minVal +
                     (bdata[i div 2] SHR ((i AND $01) SHL 2 )) AND $0F ;
        inc(sbuffer) ;
      {$ENDIF}
    end ;

    SetLength(bdata, 0) ;
  end ;

  {$IFDEF OXYGENE}

    procedure TGIS_LayerADF.get8BitLine( const _buffer : TGIS_SingleArray ;
                                         const _offset : Integer ;
                                         const _linenr : Integer ;
                                         const _obj    : TObject
                                       ) ;
  {$ELSE}

    procedure TGIS_LayerADF.get8BitLine( const _buffer : PSingle   ;
                                         const _linenr : Integer ;
                                         const _obj    : TObject
                                       ) ;
  {$ENDIF}
  var
    {$IFDEF OXYGENE}
      sbuffer  : TGIS_SingleArray ;
      sbufferx : Integer ;
    {$ELSE}
      sbuffer : PSingle ;
    {$ENDIF}
    i         : Integer ;
    pixels    : Integer ;
    bdata     : Array of Byte ;
  begin
    sbuffer := _buffer ;
    {$IFDEF OXYGENE}
      sbufferx := _offset ;
    {$ENDIF}
    pixels := T_TileInfo(_obj).localWidth ;

    SetLength(bdata, pixels) ;

    fileStream.Seek( T_TileInfo(_obj).listStripInfo[0].StripOffsets +
                     DWORD(_linenr)*DWORD(pixels),
                     soBeginning ) ;
    {$IFDEF OXYGENE}
      fileStream.Read( bdata, pixels ) ;
    {$ELSE}
      fileStream.Read(bdata[0], pixels) ;
    {$ENDIF}

    for i := 0 to pixels -1 do begin
      {$IFDEF OXYGENE}
        sbuffer[sbufferx] := T_TileInfo(_obj).minVal + bdata[i ] ;
        inc(sbufferx) ;
      {$ELSE}
        sbuffer^ := T_TileInfo(_obj).minVal + bdata[i ] ;
        inc(sbuffer) ;
      {$ENDIF}
    end ;

    SetLength(bdata, 0) ;
  end ;

  {$IFDEF OXYGENE}

    procedure TGIS_LayerADF.get16BitLine( const _buffer : TGIS_SingleArray ;
                                          const _offset : Integer ;
                                          const _linenr : Integer ;
                                          const _obj    : TObject
                                        ) ;
  {$ELSE}

    procedure TGIS_LayerADF.get16BitLine( const _buffer : PSingle ;
                                          const _linenr : Integer ;
                                          const _obj    : TObject
                                        ) ;
  {$ENDIF}
  var
    {$IFDEF OXYGENE}
      sbuffer  : TGIS_SingleArray ;
      sbufferx : Integer ;
    {$ELSE}
      sbuffer : PSingle ;
    {$ENDIF}
    i         : Integer ;
    pixels    : Integer ;
    bytes     : Integer ;
    bdata     : Array of Byte ;
  begin
    sbuffer := _buffer ;
    {$IFDEF OXYGENE}
      sbufferx := _offset ;
    {$ENDIF}
    pixels := T_TileInfo(_obj).localWidth ;
    bytes := pixels * 2 ;

    SetLength(bdata, bytes) ;

    fileStream.Seek( T_TileInfo(_obj).listStripInfo[0].StripOffsets +
                     DWORD(_linenr)*DWORD(bytes),
                     soBeginning ) ;
    {$IFDEF OXYGENE}
      fileStream.Read ( bdata, bytes ) ;
    {$ELSE}
      fileStream.Read(bdata[0], bytes) ;
    {$ENDIF}

    for i := 0 to pixels -1 do begin
      {$IFDEF OXYGENE}
        sbuffer[sbufferx] := T_TileInfo(_obj).minVal + (Integer(bdata[i * 2]) SHL 8)
                      + bdata[i * 2 +1];
        inc(sbufferx) ;
      {$ELSE}
        sbuffer^ := T_TileInfo(_obj).minVal + (Integer(bdata[i * 2]) SHL 8)
                      + bdata[i * 2 +1];
        inc(sbuffer) ;
      {$ENDIF}
    end ;

    SetLength(bdata, 0) ;
  end ;

  {$IFDEF OXYGENE}

    procedure TGIS_LayerADF.getFloatLine( const _buffer : TGIS_SingleArray ;
                                          const _offset : Integer ;
                                          const _linenr : Integer ;
                                          const _obj    : TObject
                                        ) ;
  {$ELSE}

    procedure TGIS_LayerADF.getFloatLine( const _buffer : PSingle ;
                                          const _linenr : Integer ;
                                          const _obj    : TObject
                                        ) ;
  {$ENDIF}
  var
    {$IFDEF OXYGENE}
      sbuffer   : TGIS_SingleArray ;
      sbufferx  : Integer ;
    {$ELSE}
      sbuffer   : PSingle ;
    {$ENDIF}
    i           : Integer ;
    pixels      : Integer ;
    bytes       : Integer ;
    bdata       : Array of Byte ;
    {$IFNDEF OXYGENE}
      pbytein   : PByte ;
      pbyteout  : PByte ;
    {$ENDIF}
  begin
    sbuffer := _buffer ;
    {$IFDEF OXYGENE}
      sbufferx := _offset ;
    {$ENDIF}
    pixels := T_TileInfo(_obj).localWidth ;
    bytes := pixels * 4 ;
    if (T_TileInfo(_obj).listStripInfo[0].StripByteCounts = 0) or
       (T_TileInfo(_obj).listStripInfo[0].StripByteCounts > (4*Sqr(tileWidth)))
    then begin
      for i := 0 to pixels -1 do begin
      {$IFDEF OXYGENE}
        sbuffer[sbufferx+i] := FNoDataValue ;
      {$ELSE}
        sbuffer^ := FNoDataValue ;
        inc(sbuffer) ;
      {$ENDIF}
      end;
      exit ;
    end;
    SetLength(bdata, bytes) ;

    fileStream.Seek( T_TileInfo(_obj).listStripInfo[0].StripOffsets +
                     DWORD(_linenr)*DWORD(tileWidth*4),
                     soBeginning ) ;
    {$IFDEF OXYGENE}
      fileStream.Read( bdata, bytes ) ;
    {$ELSE}
      fileStream.Read(bdata[0], bytes) ;
    {$ENDIF}

    {$IFNDEF OXYGENE}
      pbyteout := PByte(sbuffer) ;
    {$ENDIF}
    for i := 0 to pixels -1 do begin
      {$IFDEF OXYGENE}
        sbuffer[sbufferx+i] := SwapSingle( bdata, 4*i ) ;
      {$ELSE}
        pbytein := PByte(Addr(bdata[4*i +3])) ;
        pbyteout^ := pbytein^ ;
        inc(pbyteout) ;
        Dec(pbytein) ;
        pbyteout^ := pbytein^ ;
        inc(pbyteout) ;
        Dec(pbytein) ;
        pbyteout^ := pbytein^ ;
        inc(pbyteout) ;
        Dec(pbytein) ;
        pbyteout^ := pbytein^ ;
        inc(pbyteout) ;
      {$ENDIF}
    end ;

    SetLength(bdata, 0) ;
  end ;

  {$IFDEF OXYGENE}

    procedure TGIS_LayerADF.getLiteralRunsLineCF( const _buffer : TGIS_SingleArray ;
                                                  const _offset : Integer ;
                                                  const _linenr : Integer ;
                                                  const _obj    : TObject
                                                ) ;
  {$ELSE}

    procedure TGIS_LayerADF.getLiteralRunsLineCF( const _buffer : PSingle   ;
                                                  const _linenr : Integer ;
                                                  const _obj    : TObject
                                                ) ;
  {$ENDIF}
  var
    {$IFDEF OXYGENE}
      sbuffer   : TGIS_SingleArray ;
      sbufferx  : Integer ;
    {$ELSE}
      sbuffer   : PSingle ;
    {$ENDIF}
    i         : Integer ;
    pixels    : Integer ;
    bytes     : Integer ;
    bdata     : Array of Byte ;
    offs      : Integer ;
    dataoffs  : Integer ;
    pixelcount: Integer ;
    pixelno   : Integer ;
    wval  : Word ;
    linenr : Integer ;
    imax  : Integer ;
    is_data : Boolean ;
  begin

    sbuffer := _buffer ;
    {$IFDEF OXYGENE}
      sbufferx := _offset ;
    {$ENDIF}
    bytes := T_TileInfo(_obj).listStripInfo[0].StripByteCounts ;

    SetLength(bdata, bytes) ;
    fileStream.Seek( T_TileInfo(_obj).listStripInfo[0].StripOffsets,
                     soBeginning ) ;
    {$IFDEF OXYGENE}
      fileStream.Read( bdata, bytes ) ;
    {$ELSE}
      fileStream.Read(bdata[0], bytes) ;
    {$ENDIF}

    pixels := T_TileInfo(_obj).localWidth ;

    linenr := 0 ;
    offs := 0 ;
    dataoffs := offs ;

    imax := 0 ;
    repeat
      if bdata[offs] > 127 then begin
        pixelcount :=  256 - bdata[offs] ;
        inc(offs) ;
        is_data := False ;
      end
      else begin // <= 127
        pixelcount :=  bdata[offs];
        inc(offs) ;
        dataoffs := offs ;
        inc(offs, 2*pixelcount) ;
        is_data := True ;
      end ;

      if pixelcount > 0 then begin
        pixelno := pixelcount ;

        inc(imax, pixelno) ;
        repeat
          if _linenr = linenr then begin
            if imax > pixels  then
              imax := pixels ;

            if not is_data then begin
              for i := 1 to imax  do begin
                {$IFDEF OXYGENE}
                  sbuffer[sbufferx] := FNoDataValue ;
                  inc(sbufferx);
                {$ELSE}
                  sbuffer^ := FNoDataValue ;
                  inc(sbuffer);
                {$ENDIF}
              end ;
            end
            else begin
              for i := 1 to imax  do begin
                wval := Word(bdata[dataoffs]) ;
                inc(dataoffs) ;
                wval := wval SHL 8 ;
                wval := wval OR bdata[dataoffs] ;
                inc(dataoffs) ;
                {$IFDEF OXYGENE}
                  sbuffer[sbufferx] := wval +T_TileInfo(_obj).minVal ;;
                  inc(sbufferx);
                {$ELSE}
                  sbuffer^ := wval +T_TileInfo(_obj).minVal ;;
                  inc(sbuffer);
                {$ENDIF}
              end ;
            end ;
            pixelno := 0 ;
            pixels := pixels -imax ;
            imax := 0 ;
          end
          else begin
            if imax >= tileWidth then begin
              inc(linenr) ;
              dec(imax, tileWidth) ;
              pixelno := imax ;
              inc(dataoffs, 2*(pixelcount -pixelno)) ;
            end
            else
              pixelno := 0 ;
          end ;
        until pixelno <= 0 ;
      end ;
      if (pixels <= 0) then begin
        break ;
      end ;
      if offs >= bytes then begin
        break ;
      end ;
    until False;
    SetLength(bdata, 0) ;
  end ;

  {$IFDEF OXYGENE}

    procedure TGIS_LayerADF.getLiteralRunsLineD7( const _buffer : TGIS_SingleArray ;
                                                  const _offset : Integer ;
                                                  const _linenr : Integer ;
                                                  const _obj    : TObject
                                                ) ;
  {$ELSE}

    procedure TGIS_LayerADF.getLiteralRunsLineD7( const _buffer : PSingle   ;
                                                  const _linenr : Integer ;
                                                  const _obj    : TObject
                                                ) ;
  {$ENDIF}
  var
    {$IFDEF OXYGENE}
      sbuffer   : TGIS_SingleArray ;
      sbufferx  : Integer ;
    {$ELSE}
      sbuffer   : PSingle ;
    {$ENDIF}
    i         : Integer ;
    pixels    : Integer ;
    bytes     : Integer ;
    bdata     : Array of Byte ;
    offs      : Integer ;
    dataoffs  : Integer ;
    pixelcount: Integer ;
    pixelno   : Integer ;
    dataSize  : Integer ;
    linenr : Integer ;
    imax  : Integer ;
    is_data : Boolean ;
  begin

    dataSize := 1 ;

    sbuffer := _buffer ;
    {$IFDEF OXYGENE}
      sbufferx := _offset ;
    {$ENDIF}
    bytes := T_TileInfo(_obj).listStripInfo[0].StripByteCounts ;

    SetLength(bdata, bytes) ;
    fileStream.Seek( T_TileInfo(_obj).listStripInfo[0].StripOffsets, soBeginning ) ;
    {$IFDEF OXYGENE}
      fileStream.Read( bdata, bytes ) ;
    {$ELSE}
      fileStream.Read(bdata[0], bytes) ;
    {$ENDIF}

    pixels := T_TileInfo(_obj).localWidth ;

    linenr := 0 ;
    offs := 0 ;
    dataoffs := offs ;

    imax := 0 ;
    repeat

      if bdata[offs] > 127 then begin
        pixelcount :=  256 - bdata[offs] ;
        inc(offs) ;
        is_data := False ;
      end
      else begin // <= 127
        pixelcount :=  bdata[offs];
        inc(offs) ;
        dataoffs := offs ;
        inc(offs, pixelcount) ;
        is_data := True ;
      end ;

      if pixelcount > 0 then begin
        pixelno := pixelcount ;

        inc(imax, pixelno) ;
        repeat
          if _linenr = linenr then begin
            if imax > pixels  then
              imax := pixels ;

            if not is_data then begin
              for i := 1 to imax  do begin
                {$IFDEF OXYGENE}
                  sbuffer[sbufferx] := FNoDataValue ;
                  inc(sbufferx);
                {$ELSE}
                  sbuffer^ := FNoDataValue ;
                  inc(sbuffer);
                {$ENDIF}
              end ;
            end
            else begin
              for i := 1 to imax  do begin
                {$IFDEF OXYGENE}
                  sbuffer[sbufferx] := bdata[dataoffs] +T_TileInfo(_obj).minVal ;
                  inc(sbufferx);
                {$ELSE}
                  sbuffer^ := bdata[dataoffs] +T_TileInfo(_obj).minVal ;
                  inc(sbuffer);
                {$ENDIF}
                inc(dataoffs) ;
              end ;
            end ;
            pixelno := 0 ;
            pixels := pixels -imax ;
            imax := 0 ;
          end
          else begin
            if imax >= tileWidth then begin
              inc(linenr) ;
              dec(imax, tileWidth) ;
              pixelno := imax ;
              inc(dataoffs, (pixelcount -pixelno)*dataSize) ;
            end
            else
              pixelno := 0 ;
          end ;
        until pixelno <= 0 ;
      end ;
      if (pixels <= 0) then begin
        break ;
      end ;
      if offs >= bytes then begin
        break ;
      end ;
    until False;
    SetLength(bdata, 0) ;
  end ;

  {$IFDEF OXYGENE}

    procedure TGIS_LayerADF.getLiteralRunsLineDF( const _buffer : TGIS_SingleArray ;
                                                  const _offset : Integer ;
                                                  const _linenr : Integer ;
                                                  const _obj    : TObject
                                                ) ;
  {$ELSE}

    procedure TGIS_LayerADF.getLiteralRunsLineDF( const _buffer : PSingle   ;
                                                  const _linenr : Integer ;
                                                  const _obj    : TObject
                                                ) ;
  {$ENDIF}
  var
    {$IFDEF OXYGENE}
      sbuffer   : TGIS_SingleArray ;
      sbufferx  : Integer ;
    {$ELSE}
      sbuffer   : PSingle ;
    {$ENDIF}
    i         : Integer ;
    pixels    : Integer ;
    bytes     : Integer ;
    bdata     : Array of Byte ;
    offs      : Integer ;
    pixelcount: Integer ;
    pixelno   : Integer ;
    sval      : Single ;
    linenr    : Integer ;
    imax      : Integer ;
  begin

    sbuffer := _buffer ;
    {$IFDEF OXYGENE}
      sbufferx := _offset ;
    {$ENDIF}
    bytes := T_TileInfo(_obj).listStripInfo[0].StripByteCounts ;

    SetLength(bdata, bytes) ;
    fileStream.Seek( T_TileInfo(_obj).listStripInfo[0].StripOffsets, soBeginning ) ;
    {$IFDEF OXYGENE}
      fileStream.Read( bdata, bytes ) ;
    {$ELSE}
      fileStream.Read(bdata[0], bytes) ;
    {$ENDIF}

    pixels := T_TileInfo(_obj).localWidth ;

    linenr := 0 ;
    offs := 0 ;
    imax := 0 ;
    repeat

      if bdata[offs] > 127 then begin
        pixelcount :=  256 - bdata[offs] ;
        sval := FNoDataValue ;
      end
      else begin // <= 127
        pixelcount :=  bdata[offs] ;
        sval := T_TileInfo(_obj).minVal ;
      end ;
      inc(offs) ;

      if pixelcount > 0 then begin
        pixelno := pixelcount ;

        inc(imax, pixelno) ;
        repeat
          if _linenr = linenr then begin
            if imax > pixels  then
              imax := pixels ;

            for i := 1 to imax  do begin
              {$IFDEF OXYGENE}
                sbuffer[sbufferx] := sval ;
                inc(sbufferx);
              {$ELSE}
                sbuffer^ := sval ;
                inc(sbuffer);
              {$ENDIF}
            end ;
            pixelno := 0 ;
            pixels := pixels -imax ;
            imax := 0 ;
          end
          else begin
            if imax >= tileWidth then begin
              inc(linenr) ;
              dec(imax, tileWidth) ;
              pixelno := imax ;
            end
            else
              pixelno := 0 ;
          end ;
        until pixelno <= 0 ;
      end ;
      if (pixels <= 0) then begin
        break ;
      end ;
      if offs >= bytes then begin
        break ;
      end ;
    until False;
    SetLength(bdata, 0) ;
  end ;

  {$IFDEF OXYGENE}

    procedure TGIS_LayerADF.getLengthEcodedLine( const _buffer : TGIS_SingleArray ;
                                                 const _offset : Integer ;
                                                 const _linenr : Integer ;
                                                 const _obj    : TObject
                                               ) ;
  {$ELSE}

    procedure TGIS_LayerADF.getLengthEcodedLine( const _buffer : PSingle   ;
                                                 const _linenr : Integer ;
                                                 const _obj    : TObject
                                               ) ;
  {$ENDIF}
  var
    {$IFDEF OXYGENE}
      sbuffer   : TGIS_SingleArray ;
      sbufferx  : Integer ;
    {$ELSE}
      sbuffer   : PSingle ;
    {$ENDIF}
    i         : Integer ;
    pixels    : Integer ;
    bytes     : Integer ;
    bdata     : Array of Byte ;
    offs      : Integer ;
    pixelcount: Integer ;
    pixelno   : Integer ;
    dataSize  : Integer ;
    ival      : Integer ;
    sival     : Integer ;
    ival64    : Int64 ;
    linenr    : Integer ;
    imax      : Integer ;
  begin

    case T_TileInfo(_obj).dataType of
      $E0 : dataSize := 4 ;
      $F0 : dataSize := 2 ;
    else
      dataSize := 1;
    end ;

    sbuffer := _buffer ;
    {$IFDEF OXYGENE}
      sbufferx := _offset ;
    {$ENDIF}
    pixels := T_TileInfo(_obj).localWidth ;

    bytes := T_TileInfo(_obj).listStripInfo[0].StripByteCounts ;

    SetLength(bdata, bytes) ;
    fileStream.Seek( T_TileInfo(_obj).listStripInfo[0].StripOffsets,
                     soBeginning ) ;
    {$IFDEF OXYGENE}
      fileStream.Read( bdata, bytes ) ;
    {$ELSE}
      fileStream.Read(bdata[0], bytes) ;
    {$ENDIF}

    offs   := 0 ;
    linenr := 0 ;
    imax   := 0 ;

    repeat
      pixelcount := bdata[offs] ;
      inc(offs) ;
      if offs >= bytes then
        offs := 0 ;
      sival := SmallInt(bdata[offs]) ;
      inc(offs) ;
      if dataSize >= 2 then begin
        sival := sival SHL 8 ;
        sival := SmallInt(sival OR bdata[offs]) ;
        inc(offs) ;
        if dataSize = 4 then begin
          ival := (sival SHL 16) OR (SmallInt(bdata[offs]) SHL 8)
                  OR bdata[offs +1] ;
          inc(offs, 2) ;
        end
        else
          ival := Integer(sival) ;
      end
      else
        ival := Integer(sival) ;

      if pixelcount > 0 then begin
        pixelno := pixelcount ;

        inc(imax, pixelno) ;
        repeat
          if _linenr = linenr then begin
            ival64 := Int64(ival) +Int64(T_TileInfo(_obj).minVal) ;
            ival := Integer(ival64) ;
            if imax > pixels  then
              imax := pixels ;
            for i := 1 to imax  do begin
              {$IFDEF OXYGENE}
                sbuffer[sbufferx] := ival ;
                inc(sbufferx);
              {$ELSE}
                sbuffer^ := ival ;
                inc(sbuffer);
              {$ENDIF}
            end ;
            pixelno := 0 ;
            pixels := pixels -imax ;
            imax := 0 ;
          end
          else begin
            if imax >= tileWidth then begin
              inc(linenr) ;
              dec(imax, tileWidth) ;
              pixelno := imax ;
            end
            else
              pixelno := 0 ;
          end ;
        until pixelno = 0 ;
      end ;
      if (pixels = 0) then begin
        break ;
      end ;
      if offs >= bytes then begin
        break ;
      end ;
    until False;
    SetLength(bdata, 0) ;
  end ;

  {$IFDEF OXYGENE}

    procedure TGIS_LayerADF.get1BitCCITTRLELine( const _buffer : TGIS_SingleArray ;
                                                 const _offset : Integer ;
                                                 const _linenr : Integer ;
                                                 const _obj    : TObject
                                               ) ;
  {$ELSE}

    procedure TGIS_LayerADF.get1BitCCITTRLELine( const _buffer : PSingle ;
                                                 const _linenr : Integer ;
                                                 const _obj    : TObject
                                               ) ;
  {$ENDIF}
  var
    {$IFDEF OXYGENE}
      sbuffer  : TGIS_SingleArray ;
      sbufferx : Integer ;
      srcptr   : TBytes  ;
      srcptrx  : Integer ;
    {$ELSE}
      sbuffer : PSingle ;
      srcptr  : PByte ;
    {$ENDIF}
    i : Integer ;
    ti : T_TileInfo ;
    mask : Byte ;
  begin
    ti := T_TileInfo(_obj) ;
    if not assigned(ti.tileDecoder) then
      exit ;
    ti.tileDecoder.HuffmanRLEDecodeLine(_linenr) ;

    {$IFDEF OXYGENE}
      srcptrx := 0 ;
      srcptr := ti.tileDecoder.DecodeState.LineBuffer;
    {$ELSE}
      srcptr := @ti.tileDecoder.DecodeState.LineBuffer[0];
    {$ENDIF}
    sbuffer := _buffer ;
    {$IFDEF OXYGENE}
      sbufferx := _offset ;
    {$ENDIF}
    mask := $80 ;
    for i := 0 to tileWidth -1 do begin
      {$IFDEF OXYGENE}
        if (srcptr[srcptrx] AND mask) = mask then
      {$ELSE}
        if (srcptr^ AND mask) = mask then
      {$ENDIF}
        {$IFDEF OXYGENE}
          sbuffer[sbufferx] := ti.minVal +1
        {$ELSE}
          sbuffer^ := ti.minVal +1
        {$ENDIF}
      else
        {$IFDEF OXYGENE}
          sbuffer[sbufferx] := ti.minVal ;
        {$ELSE}
          sbuffer^ := ti.minVal ;
        {$ENDIF}
      {$IFDEF OXYGENE}
        inc(sbufferx) ;
      {$ELSE}
        inc(sbuffer) ;
      {$ENDIF}
      mask := mask SHR 1 ;
      if mask = 0 then begin
        {$IFDEF OXYGENE}
          inc(srcptrx) ;
        {$ELSE}
          inc(srcptr) ;
        {$ENDIF}
        mask := $80 ;
      end ;
    end ;
  end ;

  procedure TGIS_LayerADF.lineToBuffer(const _linenr : Integer ;
                                       const _pixsta : Integer  ;
                                       const _pixels : Integer  ) ;
  var
    i    : Integer;
    line : Integer ;
    tidxstart, tidxstop : Integer ;
    tileline : Integer ;
    lidx : Integer ;
    tidx : Integer ;
    partno : Integer ;
    linesinpart : Integer ;
    fname : String ;
    stshift : Integer ;

    function change_file : Boolean ;
    var
      k : Integer ;
    begin
     {$IFDEF OXYGENE}
      if filesShortNames[partno] <> nil then begin
     {$ELSE}
      if filesShortNames[partno] <> '' then begin
     {$ENDIF}
        FreeObject(fileStream) ;
        FreeObject(idxStream) ;
        fname := fdir + filesShortNames[partno] + '.adf' ;
        idxFileName := fdir + filesShortNames[partno] + 'x.adf' ;
        fileStream    := openBufferedFileStream( fname ) ;  
        idxStream     := openBufferedFileStream( idxFileName ) ;  
        actualFilesIdx := partno ;
        Result := True ;
      end
      else begin
        if not noDataInBuffer then begin
          for k := 0 to length(lineBuffer) -1 do
            lineBuffer[k] := FNoDataValue ;
          noDataInBuffer := True ;
        end;
        Result := False ;
      end;
    end;
  begin
    try

      line   := _linenr ;

      if lineInBuffer <> line then begin
        if lineInBuffer = -1 then begin
          if (_pixels = 1) and (lpixelX = _pixsta) and (lpixelY = _linenr) then
            exit ;
        end;
        if baseFilesNumber <= 1 then begin
          tidxstart := (line div tileLength)*tilesColumns ;
          tidxstop := tidxstart + ((FBitWidth +tileWidth -1) div tileWidth) - 1 ;

          stshift := (_pixsta div tileWidth ) ;
          tidxstart := tidxstart + stshift ;
          tidxstop := tidxstop - ((FBitWidth -_pixsta -_pixels) div tileWidth) ;
        end
        else begin
          linesinpart := tilesRows*tileLength ;
          partno := line div linesinpart ;

          if partno <> actualFilesIdx then begin
            if not change_file then begin
              exit ;
            end;
          end;
          line := line -(partno*linesinpart) ;

          tidxstart := (line div tileLength)*tilesColumns ;
          tidxstop := tidxstart + ((FBitWidth +tileWidth -1) div tileWidth) - 1 ;
          stshift := (_pixsta div tileWidth ) ;

          tidxstart := tidxstart + stshift ;
          tidxstop := tidxstop - ((FBitWidth -_pixsta -_pixels) div tileWidth) ;
        end;

        lidx := tileWidth * stshift ;
        tidx := stshift ;
        tileline := line mod tileLength ;
        for i := tidxstart to tidxstop do begin
          initTileInfo(i) ;
          {$IFDEF OXYGENE}
            T_TileInfo(tilesInfoList.Items[tidx]).GetTileLineFunc
                                                  (lineBuffer,
                                                   lidx,
                                                   tileline,
                                                   tilesInfoList.Items[tidx]
                                                  ) ;
          {$ELSE}
            T_TileInfo(tilesInfoList.Items[tidx]).GetTileLineFunc
                                                  (Addr(lineBuffer[lidx]),
                                                   tileline,
                                                   tilesInfoList.Items[tidx]
                                                  ) ;
          {$ENDIF}
          inc(tidx) ;
          inc(lidx, tileWidth) ;
        end ;

        if _pixels = FBitWidth then
          lineInBuffer := _linenr
        else begin
          lineInBuffer := -1 ;
          lpixelX := _pixsta ;
          lpixelY := _linenr ;
        end;

        noDataInBuffer := False ;
      end ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  procedure TGIS_LayerADF.initTileInfo(const _tileIdx : Integer) ;
  var
    ti         : T_TileInfo ;
    beshortval : SmallInt ;
    beintval   : Cardinal ;
    beint64val : Cardinal ;
    minValSize : Byte ;
    act_width  : Integer ;
    idx        : Integer ;
    i : Integer ;

    {$IFNDEF OXYGENE}
      function swap_short( _pb : PByte) : SmallInt ;
      var
        k : Integer ;
        d : Integer ;
      begin
        d := 0 ;
        for k := 1 to 2 do begin
          d := d OR _pb^ ;
          if k = 2 then
            break ;
          d := d SHL 8 ;
          inc(_pb) ;
        end ;
        Result := SmallInt(d) ;
      end ;

      function swap_integer( _pb : PByte) : Integer ;
      var
        k : Integer ;
      begin
        Result := 0 ;
        for k := 1 to 4 do begin
          Result := Result OR _pb^ ;
          if k = 4 then
            break ;
          Result := Result SHL 8 ;
          inc(_pb) ;
        end ;
      end ;
    {$ENDIF}

  begin
    idx :=  _tileIdx mod tilesColumns ;
    ti := T_TileInfo( tilesInfoList[idx] ) ;
    if ti.initialized AND ( ti.tileIdx = _tileIdx ) then
      exit ;
    ti.tileIdx := _tileIdx ;
    act_width := (idx +1)*tileWidth ;
    if FBitWidth < act_width then
      ti.localWidth := tileWidth -(act_width -FBitWidth)
    else
      ti.localWidth := tileWidth ;

    idxStream.Seek( 100 + _tileIdx*8, soBeginning ) ;
    idxStream.ReadCardinal(beintval, sizeOf(beintval)) ;
    ti.listStripInfo[0].StripOffsets := SwapLongInt(beintval)*2 ;

    idxStream.ReadCardinal(beintval, sizeOf(beintval)) ;
    ti.listStripInfo[0].StripByteCounts := SwapLongInt(beintval)*2 ;

    fileStream.Seek( ti.listStripInfo[0].StripOffsets, soBeginning ) ;
    fileStream.ReadSmallInt( beshortval, sizeOf( SmallInt )) ;
    {$IFDEF OXYGENE}
      beshortval := SwapSmallint( beshortval ) ;
    {$ELSE}
      beshortval := swap_short( Addr(beshortval)) ;
    {$ENDIF}

    fileStream.Seek( ti.listStripInfo[0].StripOffsets + 2, soBeginning ) ;
    fileStream.ReadByte(ti.dataType, sizeOf(ti.dataType)) ;

    if dataType = ESRI_INTEGER_DATA then begin
      case ti.dataType of
        {$IFDEF OXYGENE}
          $00: ti.GetTileLineFunc := @getConstLine ;
          $01: ti.GetTileLineFunc := @get1BitLine  ;
          $04: ti.GetTileLineFunc := @get4BitLine  ;
          $08: ti.GetTileLineFunc := @get8BitLine  ;
          $10: ti.GetTileLineFunc := @get16BitLine ;
          $CF: ti.GetTileLineFunc := @getLiteralRunsLineCF ;
          $D7: ti.GetTileLineFunc := @getLiteralRunsLineD7  ;
          $DF: ti.GetTileLineFunc := @getLiteralRunsLineDF  ;
          $E0,
          $F0,
          $F8,
          $FC: ti.GetTileLineFunc := @getLengthEcodedLine ;
          $FF: ti.GetTileLineFunc := @get1BitCCITTRLELine ;
        {$ELSE}
          $00: ti.GetTileLineFunc := getConstLine ;
          $01: ti.GetTileLineFunc := get1BitLine  ;
          $04: ti.GetTileLineFunc := get4BitLine  ;
          $08: ti.GetTileLineFunc := get8BitLine  ;
          $10: ti.GetTileLineFunc := get16BitLine ;
          $CF: ti.GetTileLineFunc := getLiteralRunsLineCF ;
          $D7: ti.GetTileLineFunc := getLiteralRunsLineD7  ;
          $DF: ti.GetTileLineFunc := getLiteralRunsLineDF  ;
          $E0,
          $F0,
          $F8,
          $FC: ti.GetTileLineFunc := getLengthEcodedLine ;
          $FF: ti.GetTileLineFunc := get1BitCCITTRLELine ;
        {$ENDIF}
      else
        begin
          {$IFDEF OXYGENE}
            ti.GetTileLineFunc := @getConstLine       ;
          {$ELSE}
            ti.GetTileLineFunc := getConstLine        ;
          {$ENDIF}
          ti.dataType := 0 ;
        end ;
      end;

      if ((_tileIdx mod tilesColumns) = (tilesColumns -1))        //last column
          OR
         (((_tileIdx +1) div tilesColumns) >= (FBitHeight div 4)  -2) then //last tile
      begin
        for i := 0 to length(lt_types) -1 do begin
          if (lt_types[i] = $33) OR (ti.dataType = lt_types[i]) then
            break ;
        end ;
        if lt_types[i] = $33 then begin
          lt_types[i] := ti.dataType ;
          lt_types_count[i] := 1 ;
        end
        else
          inc(lt_types_count[i]) ;

      end ;

      fileStream.ReadByte( minValSize, sizeOf( minValSize )) ;
      if minValSize = 127 then
        minValSize := 4 ;
      case minValSize of
        0:  ti.minVal := 0 ;
        1:
          begin
            ti.minVal := 0 ;
            fileStream.ReadInteger( ti.minVal, sizeOf( Byte )) ;
          end ;
        2:
          begin
            fileStream.ReadSmallInt( beshortval, sizeOf( SmallInt )) ;
            {$IFDEF OXYGENE}
              ti.minVal := SwapSmallint( beshortval ) ;
            {$ELSE}
              ti.minVal := swap_short( Addr(beshortval)) ;
            {$ENDIF}
          end ;
        4:
          begin
            fileStream.ReadCardinal( beintval, sizeOf( Cardinal )) ;
            {$IFDEF OXYGENE}
              ti.minVal := LongInt( SwapLongInt( beintval ) ) ;
            {$ELSE}
              ti.minVal := swap_integer( Addr(beintval)) ;
            {$ENDIF}
          end ;
        8:
          begin
            fileStream.ReadCardinal( beint64val, sizeOf( Cardinal )) ;
            {$IFDEF OXYGENE}
              ti.minVal := LongInt( SwapLongInt( beint64val ) ) ;
            {$ELSE}
              ti.minVal := swap_integer( Addr(beint64val)) ;
            {$ENDIF}
          end ;
      else
        minValSize := 4 ;
      end;

      ti.listStripInfo[0].StripByteCounts := ti.listStripInfo[0].StripByteCounts
                                        - 2 - minValSize ;
      ti.listStripInfo[0].StripOffsets := ti.listStripInfo[0].StripOffsets
                                        + 4 + minValSize ;
      ti.initialized := True ;
      if (ti.dataType = $FF) and (not ti.initialized) then
        initDecoder( _tileIdx ) ;
      ti.initialized := True ;
    end
    else begin
      {$IFDEF OXYGENE}
        ti.GetTileLineFunc := @getFloatLine ;
      {$ELSE}
        ti.GetTileLineFunc := getFloatLine ;
      {$ENDIF}

      ti.listStripInfo[0].StripOffsets := ti.listStripInfo[0].StripOffsets + 2 ;
      ti.initialized := True ;
    end ;

  end ;

  procedure TGIS_LayerADF.initDecoder(const _tileIdx : Integer) ;
  var
    ti           : T_TileInfo      ;
    decodeState  : TGIS_FileTIFFDecodeState  ;
  begin

    ti := T_TileInfo( tilesInfoList[ _tileIdx mod tilesColumns ] ) ;
    if assigned( ti.tileDecoder ) then
      FreeObject( ti.tileDecoder ) ;

    ti.tileDecoder := TGIS_FileTIFFDecoder.Create ;
    decodeState.Compression := TGIS_CompressionType.CCITTHuffmanRLE ;

    decodeState.TiffStream := TGIS_HandleStream(fileStream) ;

    decodeState.StripList := ti.listStripInfo ;

    decodeState.RowsPerStrip := tileLength ;
    decodeState.RowBytes := (tileWidth  +7) div 8 ;
    decodeState.RowPixels := tileWidth ;

    decodeState.TotalRowsNo := tileLength ;
    decodeState.CurStrip := 0 ;
    decodeState.CurStripLine := -1 ;

    ti.tileDecoder.DecodeState := decodeState ;
    ti.tileDecoder.SetUp ;
  end ;

  procedure TGIS_LayerADF.setUp ;
  var
    ext             : TGIS_Extent ;
    lext            : TGIS_Extent {$IFDEF GIS_NORECORDS} := new TGIS_Extent {$ENDIF} ;
    fext            : String      ;
    fname           : String      ;
    dtype           : DWORD       ;
    beint           : DWORD       ;
    i               : Integer     ;
    tinfo           : T_TileInfo  ;

    procedure check_set_names ;
    var
      m : Integer ;
      fsn : String ;
    begin
      SetLength(filesShortNames, baseFilesNumber) ;

      filesShortNames[0] := 'w001001' ;
      fname := fdir + 'w001000.adf' ;
      if SafeFileExists( fname ) then begin
        fname := fdir + 'w001000x.adf' ;
        if SafeFileExists( fname ) then begin
          filesShortNames[1] := 'w001000' ;
        end;
      end;
      fsn := 'z00100' ;
      for m := 2 to baseFilesNumber -1 do begin
        fname := fdir + fsn +IntToStr(m -1) +'.adf' ;
        if SafeFileExists( fname ) then begin
          fname := fdir + fsn +IntToStr(m -1)+ 'x' +'.adf' ;
          if SafeFileExists( fname ) then begin
            filesShortNames[m] := fsn +IntToStr(m -1) ;
          end;
        end;
      end;
    end;

  begin
    ext := GisExtent( 0,0,0,0 ) ;
    try

      fext := GetFileExt(Path) ;
      if fext = '.adf' then begin
        fdir := GetFileDir( Path ) ;
        if not IsStringEmpty( fdir ) and
           ( fdir[StringLast(fdir)] <> GisEnvironmentInfo.DirSep ) then
          fdir := fdir + GisEnvironmentInfo.DirSep ;
        Path := fdir + 'w001001.adf' ;
      end ;

      fname := fdir + HEADER_FILE_NAME ;
      if SafeFileExists( fname ) then begin
        try
          fileStream := openBufferedFileStream( fname ) ;  
          fileStream.Seek( 16, soBeginning ) ;
          fileStream.ReadCardinal(dtype, sizeOf(dtype)) ;
          dataType := SwapLongInt(dtype) ;

          if dataType = ESRI_INTEGER_DATA then
            Params.Pixel.GridNoValue := ESRI_GRID_NO_DATA
          else
          if dataType = ESRI_FLOAT_DATA then
            Params.Pixel.GridNoValue := ESRI_GRID_FLOAT_NO_DATA
          else
            Abort ;  //Unsupported format

          FNoDataValue := Params.Pixel.GridNoValue ;

          // Transparency
          redTransp[0] := BASE_TRANSPARENT_FLAG ;
          greenTransp[0] := BASE_TRANSPARENT_FLAG ;
          blueTransp[0] := BASE_TRANSPARENT_FLAG ;

          fileStream.Seek( 256, soBeginning ) ;

          scaleX :=  readDoubleBE ;
          scaleY := -readDoubleBE ;

          fileStream.Seek( 288, soBeginning ) ;
          fileStream.ReadCardinal(beint, sizeOf(beint)) ;
          tilesColumns := LongInt( SwapLongInt(beint) ) ;
          fileStream.ReadCardinal(beint, sizeOf(beint)) ;
          tilesRows    := LongInt( SwapLongInt(beint) ) ;

          fileStream.ReadCardinal(beint, sizeOf(beint)) ;
          tileWidth := LongInt( SwapLongInt(beint) ) ;

          fileStream.ReadCardinal(beint, sizeOf(beint)) ;
          fileStream.ReadCardinal(beint, sizeOf(beint)) ;

          tileLength := LongInt( SwapLongInt(beint) ) ;

        finally
          FreeObject( fileStream ) ;
        end ;
      end
      else
        Extent := ext;

      fname := fdir + STATISTICS_FILE_NAME ;
      if SafeFileExists( fname ) then begin
        try

          fileStream := openBufferedFileStream( fname ) ;  
          if FMinZ >= FMaxZ then begin
            FMinZ := readDoubleBE ;
            FMaxZ := readDoubleBE ;
          end ;
        finally
          FreeObject( fileStream ) ;
        end ;
      end
      else
        Extent := ext;

      fname := fdir + BOUNDS_FILE_NAME ;
      if SafeFileExists( fname ) then begin
        try
          try

            fileStream := openBufferedFileStream( fname ) ;  
            lext.XMin := readDoubleBE ;
            lext.YMin := readDoubleBE ;
            lext.XMax := readDoubleBE ;
            lext.YMax := readDoubleBE ;

            Extent := lext ;

          finally
            FreeObject(fileStream) ;
          end ;
        except
          scaleX := 0 ;
          scaleY := 0 ;
          Extent := ext;
        end ;
      end
      else
        Extent := ext;

      fname := fdir + DATA_FILE_NAME ;
      idxFileName := fdir + INDEX_FILE_NAME ;
      if SafeFileExists( fname ) AND SafeFileExists( idxFileName )then begin
        try

          fileStream    := openBufferedFileStream( fname ) ;  
          idxStream     := openBufferedFileStream( idxFileName ) ;  
          tilesInfoList := TGIS_ObjectList.Create( False ) ;

          for i := 0 to tilesColumns -1 do begin
            tinfo := T_TileInfo.Create ;
            tilesInfoList.Add( tinfo ) ;
          end ;
        except
          Abort ;
        end ;
      end
      else
        Extent := ext;

      realBitCount := 24 ;
      colorsNo := 0 ;

      FAntialias := True ;
      if ( scaleX = 0 ) or ( scaleY = 0 ) then
        Abort ;

      FBitHeight := RoundS((FExtent.YMax -FExtent.YMin) / scaleX) ;
      FBitWidth := RoundS((FExtent.XMax -FExtent.XMin) / -scaleY) ;

      i :=  ((FBitHeight + tileLength -1) div  tileLength) ;
      if i > tilesRows then begin
        baseFilesNumber := (i +tilesRows -1) div tilesRows ;
        check_set_names ;
      end
      else begin
        baseFilesNumber := 1 ;
      end;
      actualFilesIdx := 0 ;

      realLineWidth := ( FBitWidth*realBitCount +7 ) div 8 ;
      intLineWidth := FBitWidth * 3 ;

      SetLength( lineBuffer, tilesColumns*tileWidth ) ;

      lineInBuffer := -1 ;
      lpixelX := -1 ;
      lpixelY := -1 ;

      if (FMaxZ = 0) AND (FMinZ = 0) then begin
        FMaxZ   :=  -GIS_MAX_SINGLE  ;
        FMinZ   :=   GIS_MAX_SINGLE  ;
        prepareMinMaxZ ;
      end ;

      Extent3D := GisExtent3D( Extent.XMin, Extent.YMin, FMinZ,
                               Extent.XMax, Extent.YMax, FMaxZ
                              ) ;

      // open binary file

      inherited ;

      CS := TGIS_CSFactoryEsri.BuildCsFromFile( fdir + PROJECTION_FILE_NAME ) ;

      if SafeFileExists( Path ) then
        FAge := GisFileAge( Path ) ;

      FFileInfo := Format( 'Arc/Info Binary Grid (ADF)' + #13#10 +
                           '%d x %d; ',
                           [ FBitWidth, FBitHeight ]
                         ) ;
      if dataType = ESRI_INTEGER_DATA then
        FFileInfo := FFileInfo + 'integers'
      else
        FFileInfo := FFileInfo + 'floats' ;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0) ;
    end ;
  end;

  procedure TGIS_LayerADF.prepareMinMaxZ( const _zoom : Double = -1 ) ;
  var
    i, k : Integer ;
    zoom : Double ;
    lines : Integer ;
    lno : Integer ;
    lb : TGIS_SingleArray ;
  const
    MAX_LINES = 200 ;

  begin

    if (_zoom > 0) AND (_zoom <= 1) then begin
      lines := RoundS(FBitHeight * _zoom) ;
      if lines = 0 then
        lines := 1 ;
    end
    else
    begin
      if MAX_LINES > FBitHeight then
        lines := FBitHeight
      else
        lines := MAX_LINES ;
    end ;

    zoom := lines/FBitHeight ;

    SetLength(lb, FBitWidth) ;

    for i := 0 to lines -1 do begin
      lno := RoundS(i/zoom) ;
      getNativeLine(lb, lno, 0, FBitWidth) ;
      for k := 0 to FBitWidth -1 do begin
        if lb[k] <> FNoDataValue then
        begin
          if lb[k] < FMinZ then
            FMinZ := lb[k] ;
          if lb[k] > FMaxZ then
            FMaxZ := lb[k] ;
        end ;
      end ;
    end;
    SetLength(lb, 0) ;
  end ;

  function TGIS_LayerADF.getLine( const _buffer : TBytes  ;
                                  const _offset : Integer ;
                                  const _linenr : Integer ;
                                  const _start  : Integer ;
                                  const _bytes  : Integer
                                ) : Integer ;
  var
    i    : Integer;
    start  : Integer ;
    line : Integer ;
    pixels : Integer ;
    offset : Integer ;
    ccolor : TGIS_Color ;
  begin

    pixels := _bytes div 3 ;
    line   := _linenr ;
    Result := _bytes ;

    start := (_start div 3) ;

    if lineInBuffer <> line then
      lineToBuffer( line, start, pixels ) ;

    offset := 0 ;
    pixels := pixels +start ;
    for i := start to pixels -2 do begin
      if lineBuffer[i] <= FNoDataValue then begin
        ccolor := colorNoData ;
        _buffer[_offset + offset    ] := ccolor.B ;
        _buffer[_offset + offset + 1] := ccolor.G ;
        _buffer[_offset + offset + 2] := ccolor.R ;
        makeTransparent := True ;
      end
      else
      begin
        ccolor := GetColorRamp( lineBuffer[i] ) ;
        _buffer[_offset + offset    ] := ccolor.B ;
        _buffer[_offset + offset + 1] := ccolor.G ;
        _buffer[_offset + offset + 2] := ccolor.R ;
      end;
      offset := offset +3 ;
    end ;

    // last triple
    if lineBuffer[pixels -1] <= FNoDataValue then begin
      ccolor := colorNoData ;
      makeTransparent := True ;
    end
    else
      ccolor := GetColorRamp( lineBuffer[pixels -1] ) ;

    _buffer[_offset + offset    ] := ccolor.B ;
    _buffer[_offset + offset + 1] := ccolor.G ;
    _buffer[_offset + offset + 2] := ccolor.R ;
  end ;

  function TGIS_LayerADF.getNativeValue( const _pt : TPoint  ;
                                         const _ar : TGIS_DoubleArray
                                       ) : Boolean ;
  begin
    Result := True ;

    if lineInBuffer <> _pt.Y then
      lineToBuffer(_pt.Y, _pt.X, 1) ;

    _ar[0] := lineBuffer[_pt.X] ;
  end ;

  function TGIS_LayerADF.getNativeLine( const _buffer   : TGIS_SingleArray ;
                                        const _linenr   : Integer          ;
                                        const _startIdx : Integer          ;
                                        const _count    : Integer
                                      ) : Integer ;
  var
    i : Integer ;
  begin

    Result := 0;
    if (_linenr < 0) or (_linenr > FBitHeight) then
      exit ;
    Result := _count ;

    if lineInBuffer <> _linenr then
      lineToBuffer(_linenr, _startIdx, _count) ;

    for i := 0 to _count -1 do
      _buffer[i] := lineBuffer[i +_startIdx] ;

  end ;

  procedure TGIS_LayerADF.Alive ;
  var
    i               : Integer     ;
    tinfo           : T_TileInfo  ;
  begin
    if Params.Pixel.GridNoValue = GIS_GRID_NOVALUE then begin//Old default value
      if dataType = ESRI_INTEGER_DATA then
        Params.Pixel.GridNoValue := ESRI_GRID_NO_DATA
      else
        Params.Pixel.GridNoValue := ESRI_GRID_FLOAT_NO_DATA ;

      FNoDataValue := Params.Pixel.GridNoValue ;
    end ;

    if assigned( fileStream ) then exit ;

    fileStream := openBufferedFileStream( Path ) ;  
    idxStream  := openBufferedFileStream( idxFileName ) ;  

    tilesInfoList := TGIS_ObjectList.Create( False ) ;

    for i := 0 to tilesColumns -1 do begin
      tinfo := T_TileInfo.Create ;
      tilesInfoList.Add( tinfo ) ;
    end ;

  end ;

  function TGIS_LayerADF.DormantGain
    : Integer ;
  begin
    case DormantMode of
      TGIS_LayerDormantMode.Off :
        Result := 0;
      else
        Result := 5 ;
    end ;
  end ;

  procedure TGIS_LayerADF.Dormant ;
  var
    i     : Integer ;
    tinfo : T_TileInfo ;
  begin
    if DormantMode = TGIS_LayerDormantMode.Off then
      exit ;
      
    if not assigned(fileStream) then exit ;
    if not assigned(tilesInfoList) then exit ;

    for i := tilesColumns -1 downto 0 do begin
      tinfo := T_TileInfo(tilesInfoList.Items[i]) ;
      if assigned(tinfo.tileDecoder) then begin
        FreeObject(tinfo.tileDecoder) ;
      end ;
      FreeObject( tinfo ) ;
    end ;

    FreeObject(tilesInfoList) ;

    FreeObject(idxStream) ;
    FreeObject(fileStream) ;
  end ;

  { Perform initialization section.
  }
  class procedure Unit_GisLayerADF.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-ADF', 'Arcinfo Binary Grid', TGIS_LayerADF, '.adf',
                   TGIS_RegisteredLayerType.Grid, TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                    True
                  );
  end ;

{$IFNDEF OXYGENE}

initialization
    Unit_GisLayerADF.SelfRegisterLayer() ;
{$ENDIF}

{==================================== END =====================================}
end.

