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
  Internal part of TIFF-file implementation.
  Supports PackBit and Group4 and LZW decompression.

  This unit was partially based on tiff-v3.5.5:

  - Copyright (c) 1988-1997 Sam Leffler
  - Copyright (c) 1991-1997 Silicon Graphics, inc.

  Permission to use, copy, modify, distribute, and sell this software and
  its documentation for any purpose is hereby granted without fee, provided
  that (i) the above copyright notices and this permission notice appear in
  all copies of the software and related documentation, and (ii) the names of
  Sam Leffler and Silicon Graphics may not be used in any advertising or
  publicity relating to the software without the specific, prior written
  permission of Sam Leffler and Silicon Graphics.

  THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND,
  EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
  WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.

  IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
  ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
  OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
  WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF
  LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
  OF THIS SOFTWARE.

  Sublicensing of this unit is a subject of TatukGIS Developer
  Kernel License.
}

{$IFDEF DCC}
  unit GisFileTIFF ;
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
    System.Runtime.InteropServices,
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.SysUtils,
    System.Math,
    System.ZLib,
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    GisRtl,
    GisTypes,
    GisTypesUI,
    GisInternals,
    GisStreams,
    GisFilePixel,
    GisFileJPEG,
    GisCsSystems ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}
{$IFDEF ISLAND}
uses
  TatukGIS.RTL ;
{$ENDIF}

const

  /// <summary>
  ///   LZW compression constant used to define string table array size.
  /// </summary>
  MAXPREALLOC=8 ;

  /// <summary>
  ///   Default TIFF no data value.
  /// </summary>
  TIFF_GRID_NOVALUE = -32767 ;


  /// <summary>
  ///   LZW compression constant used to define hash and code arrays size.
  /// </summary>
  HSIZE = 119001 ;

  /// <summary>
  ///   Extension of world file (extension definition file foe pixel layer).
  /// </summary>
  WORLD_FILE_EXT_TIF = '.tfw' ;

  /// <summary>
  ///   Extension of world file (extension file for pixel layer). Alternative
  ///   naming.
  /// </summary>
  WORLD_FILE_EXT_TIF2 = '.tifw' ;

  /// <summary>
  ///   Extension of world file (extension file for pixel layer). Alternative
  ///   naming.
  /// </summary>
  WORLD_FILE_EXT_TIF3 = '.tiffw' ;

  /// <summary>
  ///   Byte Order II: from least significant Byte to the most significant
  ///   Byte e.g. Intel.
  /// </summary>
  TIFF_LITTLEENDIAN = $4949 ;

  /// <summary>
  ///   Byte Order MM: from most significant Byte to the least significant
  ///   Byte e.g. Motorola.
  /// </summary>
  TIFF_BIGENDIAN = $4D4D ;

  /// <summary>
  ///   identifies TIFF file
  /// </summary>
  TIFF_CONFIRM = $002A ;

  /// <summary>
  ///   identifies TIFF file
  /// </summary>
  TIFF_BIGTIFFCONFIRM = $002B ;

// TIFF Tags
  {#GENDOC:HIDE}
  TIFF_TAG_SUBFILETYPE               = 254 ;
  {#GENDOC:HIDE}
  TIFF_TAG_IMAGEWIDTH                = 256 ;
  {#GENDOC:HIDE}
  TIFF_TAG_IMAGELENGTH               = 257 ;
  {#GENDOC:HIDE}
  TIFF_TAG_BITSPERSAMPLE             = 258 ;
  {#GENDOC:HIDE}
  TIFF_TAG_COMPRESSION               = 259 ;
  {#GENDOC:HIDE}
  TIFF_TAG_PHOTOMETRIC               = 262 ;
  {#GENDOC:HIDE}
  TIFF_TAG_FILLORDER                 = $010A ;
  {#GENDOC:HIDE}
  TIFF_TAG_MAKE                      = $010F ; //271
  {#GENDOC:HIDE}
  TIFF_TAG_MODEL                     = $0110 ; //272
  {#GENDOC:HIDE}
  TIFF_TAG_STRIPOFFSETS              = $0111 ;
  {#GENDOC:HIDE}
  TIFF_TAG_ORIENTATION               = $0112 ;
  {#GENDOC:HIDE}
  TIFF_TAG_SAMPLESPERPIXELS          = $0115 ;
  {#GENDOC:HIDE}
  TIFF_TAG_ROWSPERSTRIP              = $0116 ;
  {#GENDOC:HIDE}
  TIFF_TAG_STRIPBYTECOUNTS           = $0117 ;
  {#GENDOC:HIDE}
  TIFF_TAG_MINSAMPLEVAL              = $0118 ;
  {#GENDOC:HIDE}
  TIFF_TAG_MAXSAMPLEVAL              = $0119 ;
  {#GENDOC:HIDE}
  TIFF_TAG_XRESOLUTION               = $011A ;
  {#GENDOC:HIDE}
  TIFF_TAG_YRESOLUTION               = $011B ;
  {#GENDOC:HIDE}
  TIFF_TAG_PLANARCONFIGURATION       = $011c ;
  {#GENDOC:HIDE}
  TIFF_TAG_T4OPTIONS                 = $0124 ;
  {#GENDOC:HIDE}
  TIFF_TAG_T6OPTIONS                 = $0125 ;
  {#GENDOC:HIDE}
  TIFF_TAG_RESOLUTIONUNIT            = $0128 ;
  {#GENDOC:HIDE}
  TIFF_TAG_SOFTWARE                  = $0131 ;
  {#GENDOC:HIDE}
  TIFF_TAG_PREDICTOR                 = $013D ;
  {#GENDOC:HIDE}
  TIFF_TAG_COLORMAPOFFSET            = $0140 ;
  {#GENDOC:HIDE}
  TIFF_TAG_SAMPLEFORMAT              = $0153 ;
  {#GENDOC:HIDE}
  TIFF_TAG_MINSAMPLEVALUE            = $0154 ;
  {#GENDOC:HIDE}
  TIFF_TAG_MAXSAMPLEVALUE            = $0155 ;
  {#GENDOC:HIDE}
  TIFF_TAG_JPEGTABLES                = $015B ;

  {#GENDOC:HIDE}
  TIFF_TAG_JPEGPROC                  = $0200 ;
  {#GENDOC:HIDE}
  TIFF_TAG_JPEGQTABLES               = $0207 ;
  {#GENDOC:HIDE}
  TIFF_TAG_JPEGDCTABLES              = $0208 ;
  {#GENDOC:HIDE}
  TIFF_TAG_JPEGACTABLES              = $0209 ;

  {#GENDOC:HIDE}
  TIFF_TAG_YCbCrSUBSAMPLING          = $0212 ;

  {#GENDOC:HIDE}
  TIFF_COMPRESSION_NONE              = 1     ;
  {#GENDOC:HIDE}
  TIFF_COMPRESSION_CCITTRLE          = 2     ;
  {#GENDOC:HIDE}
  TIFF_COMPRESSION_CCITTFAX3         = 3     ;
  {#GENDOC:HIDE}
  TIFF_COMPRESSION_CCITTFAX4         = 4     ;
  {#GENDOC:HIDE}
  TIFF_COMPRESSION_LZW               = 5     ;
  {#GENDOC:HIDE}
  TIFF_COMPRESSION_JPEG              = 7     ;
  {#GENDOC:HIDE}
  TIFF_COMPRESSION__ADOBE_DEFLATE    = 8     ;
  {#GENDOC:HIDE}
  TIFF_COMPRESSION__DEFLATE          = 32946 ;
  {#GENDOC:HIDE}
  TIFF_COMPRESSION_PACKBITS          = 32773 ;

  {#GENDOC:HIDE}
  TIFF_TAG_TILEWIDTH                 = $0142 ;
  {#GENDOC:HIDE}
  TIFF_TAG_TILELENGTH                = $0143 ;
  {#GENDOC:HIDE}
  TIFF_TAG_TILEOFFSETS               = $0144 ;
  {#GENDOC:HIDE}
  TIFF_TAG_TILEBYTECOUNT             = $0145 ;

  {#GENDOC:HIDE}
  TIFF_TAG_EXTRASAMPLES              = $0152 ;

  {#GENDOC:HIDE}
  GEOTIFF_TAG_MODELPIXELSCALE        = 33550 ;
  {#GENDOC:HIDE}
  GEOTIFF_TAG_MODELTIEPOINT          = 33922 ;
  {#GENDOC:HIDE}
  GEOTIFF_TAG_MODELTRANSFORMATION    = 34264 ;
  {#GENDOC:HIDE}
  GEOTIFF_TAG_GEOKEYDIRECTORY        = 34735 ;
  {#GENDOC:HIDE}
  GEOTIFF_TAG_GEODOUBLEPARAMS        = 34736 ;
  {#GENDOC:HIDE}
  GEOTIFF_TAG_GEOASCIIPARAMS         = 34737 ;
  {#GENDOC:HIDE}
  GEOTIFF_TAG_GDALNODATA             = 42113 ;

//Coordinate Transformation Codes
  {#GENDOC:HIDE}
  CT_TRANSVERSEMERCATOR                   = 1 ;
  {#GENDOC:HIDE}
  CT_TRANSVERSEMERCATOR_MODIFIED_ALASKA   = 2 ;
  {#GENDOC:HIDE}
  CT_OBLIQUEMERCATOR                      = 3 ;
  {#GENDOC:HIDE}
  CT_OBLIQUEMERCATOR_LABORDE              = 4 ;
  {#GENDOC:HIDE}
  CT_OBLIQUEMERCATOR_ROSENMUND            = 5 ;
  {#GENDOC:HIDE}
  CT_OBLIQUEMERCATOR_SPHERICAL            = 6 ;
  {#GENDOC:HIDE}
  CT_MERCATOR                             = 7 ;
  {#GENDOC:HIDE}
  CT_LAMBERTCONFCONIC_2SP                 = 8 ;
  {#GENDOC:HIDE}
  CT_LAMBERTCONFCONIC_1SP                 = 9 ;
  {#GENDOC:HIDE}
  CT_LAMBERTAZIMEQUALAREA                 = 10 ;
  {#GENDOC:HIDE}
  CT_ALBERSEQUERAREA                      = 11 ;
  {#GENDOC:HIDE}
  CT_AZIMUTHALEQUIDISTANT                 = 12 ;
  {#GENDOC:HIDE}
  CT_EQUIDISTANTCONIC                     = 13 ;
  {#GENDOC:HIDE}
  CT_STEREOGRAPHIC                        = 14 ;
  {#GENDOC:HIDE}
  CT_POLARSTEREOGRAPHIC                   = 15 ;
  {#GENDOC:HIDE}
  CT_OBLIQUESTEREOGRAPHIC                 = 16 ;
  {#GENDOC:HIDE}
  CT_EQUIRECTANGULAR                      = 17 ;
  {#GENDOC:HIDE}
  CT_CASSINISOLDNER                       = 18 ;
  {#GENDOC:HIDE}
  CT_GNOMONIC                             = 19 ;
  {#GENDOC:HIDE}
  CT_MILLERCYLINDRICAL                    = 20 ;
  {#GENDOC:HIDE}
  CT_ORTHOGRAPHIC                         = 21 ;
  {#GENDOC:HIDE}
  CT_POLYCONIC                            = 22 ;
  {#GENDOC:HIDE}
  CT_ROBINSON                             = 23 ;
  {#GENDOC:HIDE}
  CT_SINUSOIDAL                           = 24 ;
  {#GENDOC:HIDE}
  CT_VANDERGRINTEN                        = 25 ;
  {#GENDOC:HIDE}
  CT_NEWZEALANDMAPGRID                    = 26 ;
  {#GENDOC:HIDE}
  CT_TRANSVMERCATOR_SOUTHORIENTED         = 27 ;

type

//==============================================================================
//  TIFF decoder declaration
//==============================================================================

   /// <summary>
   ///   JPEG data buffer type
   /// </summary>
  TGIS_JPEGTable = array of Byte ;

   /// <summary>
   ///   Image File Directory element (IFD)
   /// </summary>
  TGIS_FileTIFF_Tag = {$IFDEF OXYGENE} public {$ENDIF} packed record
    {#GENDOC:HIDE}
    Tag     : Word  ;
    {#GENDOC:HIDE}
    VarType : Word  ;
    {#GENDOC:HIDE}
    Count   : Cardinal ;
    {#GENDOC:HIDE}
    Value   : Cardinal ;
  end ;

   /// <summary>
   ///   BigTIFF Image File Directory element (IFD)
   /// </summary>
  {$IFDEF CLR}
    [StructLayout(LayoutKind.Sequential, Pack=1)]
  {$ENDIF}
  TGIS_FileTIFF_Tag64 = {$IFDEF OXYGENE} public {$ENDIF} packed record
    {#GENDOC:HIDE}
    Tag     : Word  ;
    {#GENDOC:HIDE}
    VarType : Word  ;
    {#GENDOC:HIDE}
    Count   : Int64 ;
    {#GENDOC:HIDE}
    Value   : Int64 ;
  end ;

  // GeoTiff Directory Tag
  TGIS_FileTIFF_TagGeo = {$IFDEF OXYGENE} public {$ENDIF} packed record
    {#GENDOC:HIDE}
    KeyID           : Word ;
    {#GENDOC:HIDE}
    TIFFTagLocation : Word ;
    {#GENDOC:HIDE}
    Count           : Cardinal ;
    {#GENDOC:HIDE}
    Value_Offset    : Cardinal ;
  end ;

  // GeoTiff GeoKey
  TGIS_FileTIFF_GeoKey = {$IFDEF OXYGENE} public {$ENDIF} packed record
    {#GENDOC:HIDE}
    KeyID           : Word ;
    {#GENDOC:HIDE}
    KeyType         : Word ;
    {#GENDOC:HIDE}
    Count           : Word ;
    {#GENDOC:HIDE}
    Value_Index     : Word ;
  end ;

  // StripOffsets and StripByteCounts Information List.
  TGIS_FileTIFF_StripInfo64 = packed record
    {#GENDOC:HIDE}
    StripOffsets    : Int64 ;
    {#GENDOC:HIDE}
    StripByteCounts : Int64 ;
  end ;

  // StripOffsets and StripByteCounts Information List.
  TGIS_FileTIFF_StripInfo32 = packed record
    {#GENDOC:HIDE}
    StripOffsets    : Cardinal ;
    {#GENDOC:HIDE}
    StripByteCounts : Cardinal ;
  end ;

  // Information of lineOffset and its validity  for PackBits.
  TGIS_FileTIFF_PackLineEntry = packed record
    {#GENDOC:HIDE}
    LineOffset   : Cardinal ;
    {#GENDOC:HIDE}
    StripNo      : Cardinal ;
  end ;

  /// <summary>
  ///   Actual state of decoding. For Internal use of TGIS_LayerTIFF.
  /// </summary>
  TGIS_FileTIFFDecodeState = packed record
    /// <summary>
    ///   Chosen compression type.
    /// </summary>
    Compression   : TGIS_CompressionType    ;
    /// <summary>
    ///   Image/Tile line size in bytes.
    /// </summary>
    RowBytes      : LongInt                ;
    /// <summary>
    ///   Image/Tile line size in pixels.
    /// </summary>
    RowPixels     : LongWord                ;
    /// <summary>
    ///   Data size.
    /// </summary>
    Data          : LongWord                ;
    /// <summary>
    ///   Decoded line in LineBuffer.
    /// </summary>
    CurStripLine  : Integer                 ;
    /// <summary>
    ///   Strip/tile in use.
    /// </summary>
    CurStrip      : LongWord                ;


    /// <summary>
    ///   List of strips for not tiled image.
    /// </summary>
    StripList     : {$IFDEF OXYGENE} array of TGIS_FileTIFF_StripInfo64
                    {$ELSE} TArray<TGIS_FileTIFF_StripInfo64>
                    {$ENDIF} ;
    /// <summary>
    ///   Number of line in strip or tile.
    /// </summary>
    RowsPerStrip  : LongWord                ;
    /// <summary>
    ///   Number of strips or tiles rows.
    /// </summary>
    TotalRowsNo   : Integer                 ;
    /// <summary>
    ///   JPEG data for decoding if any.
    /// </summary>
    jpegTables    : TGIS_JPEGTable          ;
    /// <summary>
    ///   Chosen page.
    /// </summary>
    CurrentPage   : Integer                 ;
    /// <summary>
    ///   Decoded line buffer.
    /// </summary>
    LineBuffer    : TBytes                  ;
    /// <summary>
    ///   Table used for pack bits compression,
    ///   needs ResetPackLineTable call on begin
    /// </summary>
    PackLineTable : array of TGIS_FileTIFF_PackLineEntry ;
    /// <summary>
    ///   Start in compressed data buffer
    /// </summary>
    StartOffset   : Int64                   ;
    /// <summary>
    ///   Current position in compressed data buffer
    /// </summary>
    Position      : Int64                   ;
    /// <summary>
    ///   CCITT type compression parameter
    /// </summary>
    FillOrder     : Word                    ;
    /// <summary>
    ///   Differencing used flag
    /// </summary>
    HDifferencing : Boolean                 ; //for LZW, ZLIB compression
    /// <summary>
    ///   Ntive values are required
    /// </summary>
    NativeRequired : Boolean                 ; //for 12 bits jpeg compression
    /// <summary>
    ///   Number of defined bands
    /// </summary>
    BandsNo : Integer                 ;

    /// <summary>
    ///   Differencing floating point used flag
    /// </summary>
    HFDifferencing : Boolean                 ; //for LZW  compression

    /// <summary>
    ///   Compressed data stream
    /// </summary>
    TiffStream    : TGIS_HandleStream       ;

    /// <summary>
    ///   Compressed data stream buffer data
    /// </summary>
    ReadBuffer    : TBytes                  ;

    /// <summary>
    ///   Compressed data stream buffer data
    /// </summary>
    ReadBufferOff : Integer                 ;

    /// <summary>
    ///   Compressed data stream buffer data
    /// </summary>
    ReadBufferSize: LongWord                ;

    /// <summary>
    ///   Compressed data stream buffer data
    /// </summary>
    ReadBufferBC  : LongWord                ;

    /// <summary>
    ///   Compressed data stream buffer data
    /// </summary>
    InBufferStart : Int64                  ;

    /// <summary>
    ///   Compressed data stream buffer data
    /// </summary>
    InBufferBytes : LongWord                ;

    // Part for internal use only
    {#GENDOC:HIDE}
    MustRead      : Boolean                 ;
    {#GENDOC:HIDE}
    Bit           : Integer                 ;
    {#GENDOC:HIDE}
    RefRuns       : TGIS_CardinalArray      ;
    {#GENDOC:HIDE}
    CurRuns       : TGIS_CardinalArray      ;
    {#GENDOC:HIDE}
    TiffRawCB     : TBytes                  ;
    {#GENDOC:HIDE}
    TiffRawCBOff  : Int64                   ;
    {#GENDOC:HIDE}
    TiffRawCC     : Int64                   ;
  end ;

  /// <summary>
  ///    LZW string table
  /// </summary>
  TGIS_FileTIFFSItem = record
    {#GENDOC:HIDE}
    Data     : TBytes ;
    {#GENDOC:HIDE}
    Dim      : Integer;
    {#GENDOC:HIDE}
    PreAlloc : TBytes; // preallocated bytes
  end ;

  /// <summary>
  ///   LZW compressor class
  /// </summary>
  TGIS_FileTIFFLZWCompRecord = class
    inPos         : Integer;
    outBuff       : TBytes ;
    countDown     : Integer ;
    inData        : TBytes ;
    initBits      : Integer ;
    nBits         : Integer ;
    maxCode       : Integer ;
    clearCode     : Integer ;
    eofCode       : Integer ;
    freeEnt       : Integer ;
    clearFlag     : Integer ;
    aCount        : Integer ;
    hTab          : Array [0..HSIZE-1] of Integer ;
    codeTab       : Array [0..HSIZE-1] of Word ;
    currAccum     : Cardinal ;
    currBits      : Integer;
    accum         : Array [0..255] of Byte ;
    gInitBits     : Integer ;
    fCode         : Integer ;
    i             : Integer ;
    c             : Integer ;
    ent           : Integer ;
    disp          : Integer ;
    hSizeReg      : Integer ;
    hShift        : Integer;
    tileWidth     : Integer ;
    tileLength    : Integer ;
    numberOfTiles : Integer ;
    tileRows      : Integer ;
    tileColumns   : Integer ;
    currTile      : Integer ;
    tileOffsets   : array of Cardinal ;
    tileBytes     : array of Cardinal ;
    finishTile    : Boolean ;
  end ;

  /// <summary>
  ///   Encapsulation of TIFF decoding. For Internal use of TGIS_LayerTIFF.
  /// </summary>
  TGIS_FileTIFFDecoder = class( TGIS_ObjectDisposable )
    private
      tabEntX   : Integer   ;
      a0        : Integer   ;
      lastx     : Integer   ;
      bitACC    : LongWord  ;
      bitsAvail : Integer   ;
      runLength : Integer   ;
      cpbuf     : TBytes    ;
      cpoff     : Integer   ;
      epbuf     : TBytes    ;
      epoff     : Integer   ;
      pa        : TGIS_CardinalArray ;
      pax       : Integer     ;
      thisRun   : TGIS_CardinalArray ;
      b1        : Integer   ;
      pb        : TGIS_CardinalArray ;
      pbx       : Integer   ;
      revTab    : Array [0..255] of Byte ;
      tds       : TGIS_FileTIFFDecodeState ;
      fDecomp   : TBytes ;
      fComp     : TBytes  ;
      fCompSize : Integer  ;
      fCompOff  : Integer  ;
      fLineSize : Integer;
      fNextCode : Integer;
      fDimCode  : Integer;
      lzwOldCode: Integer;
      fWPos     : Integer;
      STableSize: Integer;  //
      STable : array of TGIS_FileTIFFSItem;
      lzwAborting:Boolean;

      jpgStream    : TMemoryStream ;
      zlibStream   : TMemoryStream ;
      zDecompStream : TZDecompressionStream ;
      jpegZoom      : Integer ;
      jpegDecoder   : TGIS_JPEGDecoder ;

    private

      /// <summary>
      ///   Read a current position in TIFF file.
      /// </summary>
      function  readOffset        : Int64  ;

      /// <summary>
      ///   Decode single line in CCIT Huffman RLE encoded file.
      /// </summary>
      function  expandLine1d      : Boolean ;

      /// <summary>
      ///   Decode single line in CCIT Group4 encoded file.
      /// </summary>
      function  expandLine2d      : Boolean ;

      /// <summary>
      ///   For internal use of expandLine2d.
      /// </summary>
      procedure verify_b1         ;

      /// <summary>
      ///   For internal use of expandLine2d.
      /// </summary>
      procedure setVal            (_x : Integer) ;

      /// <summary>
      ///   For internal use of expandLine2d.
      /// </summary>
      procedure cleanupRuns       ;

      /// <summary>
      ///   Perform a low level decoding (bit filling) for CCIT Group4 files.
      /// </summary>
      procedure tiffGroup4FillRuns( var   _buf   : TBytes ;
                                    var   _runs  : TGIS_CardinalArray ;
                                    var   _erun  : TGIS_CardinalArray ;
                                    var   _erunx : Integer ;
                                    const _lastx : LongWord
                                  ) ;

      /// <summary>
      ///   Free table memory
      /// </summary>
      procedure freeTable ;

    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure   doDestroy             ; override;

    public

      /// <summary>
      ///   Memory allocation.
      /// </summary>
      procedure   SetUp                 ;

      /// <summary>
      ///   Cleaning table.
      /// </summary>
      procedure   ResetPackLineTable    ;

      /// <summary>
      ///   Process decoding of CCCIT Huffman RLE encoded file.
      /// </summary>
      /// <returns>
      ///   always 1
      /// </returns>
      /// <param name="_linenr">
      ///   image line number
      /// </param>
      /// <param name="_tilenr">
      ///   tile column number
      /// </param>
      function    HuffmanRLEDecodeLine  (  const _linenr : Integer ;
                                           const _tilenr : Integer = -1
                                        ) : Integer ;

      /// <summary>
      ///   Process decoding of CCCIT FAX Group 3 encoded file.
      /// </summary>
      /// <returns>
      ///   always 1
      /// </returns>
      /// <param name="_linenr">
      ///   image line number
      /// </param>
      /// <param name="_tilenr">
      ///   tile column number
      /// </param>
      function    Group3DecodeLine      (  const _linenr : Integer ;
                                           const _tilenr : Integer = -1
                                        ) : Integer ;

      /// <summary>
      ///   Process decoding of CCCIT FAX Group 4 encoded file.
      /// </summary>
      /// <returns>
      ///   always 1
      /// </returns>
      /// <param name="_linenr">
      ///   image line number
      /// </param>
      /// <param name="_tilenr">
      ///   tile column number
      /// </param>
      function    Group4DecodeLine      (  const _linenr : Integer ;
                                           const _tilenr : Integer = -1
                                        ) : Integer ;

      /// <summary>
      ///   Process decoding of LZW (Lempel-Ziff-Welch) encoded file.
      /// </summary>
      /// <returns>
      ///   read number of bytes
      /// </returns>
      /// <param name="_linenr">
      ///   image line number
      /// </param>
      /// <param name="_tilenr">
      ///   tile column number
      /// </param>
      /// <returns>
      ///   uncompressed data length (in bytes)
      /// </returns>
      function    LZWDecodeLine         (  const _linenr : Integer ;
                                           const _tilenr : Integer = -1
                                        ) : Integer ;

      /// <summary>
      ///   Process zlib supported decoding.
      /// </summary>
      /// <returns>
      ///   read nubber of bytes
      /// </returns>
      /// <param name="_linenr">
      ///   image line number
      /// </param>
      /// <param name="_tilenr">
      ///   tile column number
      /// </param>
      function    ZLIBDecodeLine        (  const _linenr : Integer ;
                                           const _tilenr : Integer = -1
                                        ) : Integer ;

      /// <summary>
      ///   Process decoding of Pack Bits encoded file.
      /// </summary>
      /// <returns>
      ///   read nubber of bytes
      /// </returns>
      /// <param name="_linenr">
      ///   image line number
      /// </param>
      /// <param name="_tilenr">
      ///   tile column number
      /// </param>
      function    PackBitsDecodeLine    (  const _linenr : Integer ;
                                           const _tilenr : Integer = -1
                                        ) : Integer ;

      /// <summary>
      ///   Process decoding of JPEG encoded file.
      /// </summary>
      /// <returns>
      ///   always 1
      /// </returns>
      /// <param name="_linenr">
      ///   image line number
      /// </param>
      /// <param name="_tilenr">
      ///   tile column number
      /// </param>
      function    JpegDecodeLine        (  const _linenr : Integer ;
                                           const _tilenr : Integer = -1
                                        ) : Integer ;

      /// <summary>
      ///   Scale setting.
      /// </summary>
      /// <returns>
      ///   actual image scale from 1 to 8
      /// </returns>
      /// <param name="_scale">
      ///   scale needed
      /// </param>
      function    JpegScale             (  const _scale    : Integer ) : Integer ;

      /// <summary>
      ///   JPEG encoded file type.
      /// </summary>
      /// <param name="_componentsJpeg">
      ///   actual type: YBR, BGR, RGB or GRAYSCAL
      /// </param>
      procedure   ComponentsJpeg        (  const _componentsJpeg : Integer ) ;

      /// <summary>
      ///   JPEG needs more than 8bits per band.
      /// </summary>
      /// <param name="_nativerq">
      ///   If False bands values are 8 bits long
      /// </param>
      procedure   NativeRequested      (  const _nativerq : Boolean ) ;
    public
      /// <summary>
      ///   TIFF decoder current state.
      /// </summary>
      property  DecodeState : TGIS_FileTIFFDecodeState read tds write tds ;
  end ;

type

//==============================================================================
//  TIFF file declaration
//==============================================================================

  /// <summary>
  ///   The Class which encapsulates the writing of a bitmap as a TIFF file. A
  ///   file of any size can be written.
  /// </summary>
  TGIS_FileTIFF = class( TGIS_FilePixel )
    private // property internal values
      compression         : TGIS_CompressionType  ;
      grayScale           : Boolean ;
      writeMode           : Boolean ;
      offsetStrip         : Cardinal ;
      offsetXRes          : Cardinal ;
      offsetYRes          : Cardinal ;
      offsetSoftware      : Cardinal ;
      offsetDir           : Cardinal ;
      offsetBitsPerSample : Cardinal ;
      offsetNoData        : Cardinal ;
      writeExt            : TGIS_Extent ;
      paletteEntry        : array of TGIS_Color ;
      noEntries           : Integer ;
      isPalette           : Boolean ;
      isTiled             : Boolean ;
      woutBuff            : TBytes  ;
      convBytes           : TBytes  ;
    protected

       /// <summary>
       ///   Bitmap file stram.
       /// </summary>
       fileStream : TGIS_FileStream;

       /// <summary>
       ///   Length of line in bytes.
       /// </summary>
       byteLineWidth  : Integer ;

       /// <summary>
       ///   Length of tile/strip in bytes.
       /// </summary>
       byteTileWidth  : Integer ;

       /// <summary>
       ///   Effective length of right tile in bytes.
       /// </summary>
       byteRTileWidth  : Integer ;

       /// <summary>
       ///   LZW compression record
       /// </summary>
       lzwr  : TGIS_FileTIFFLZWCompRecord ;

       /// <summary>
       ///   Coordinate system
       /// </summary>
       cs    : TGIS_CSCoordinateSystem ;
    private

      /// <summary>
      ///   Data LZW encoding.
      /// </summary>
      /// <param name="_indata">
      ///   decompressed data
      /// </param>
      /// <param name="_inputlen">
      ///   indata length (in bytes)
      /// </param>
      /// <param name="_op">
      ///   a reference variable 0 - start of compression 1 - continue encoding
      ///   2 - finish
      /// </param>
      function  compressLZW        ( _indata   : TBytes  ;
                                     _inStart  : Integer ;
                                     _inputlen : Integer ;
                                     _op       : Integer
                                   ) : Integer ;
    protected

      /// <inheritdoc/>
      function  writeLine          ( const _buffer : TBytes  ;
                                     const _idx : Integer ;
                                     const _linenr : Integer ;
                                     const _start  : Integer ;
                                     const _bytes : Integer
                                   ) : Integer; override;

      /// <inheritdoc/>
      procedure prepareCapabilities  ; override;

    protected

      /// <inheritdoc/>
      procedure doDestroy            ; override;

    public

      /// <inheritdoc/>
      constructor Create             ; overload; override;

      /// <inheritdoc/>
      constructor Create             ( const _path        : String      ;
                                       const _ext         : TGIS_Extent ;
                                       const _width       : Integer     ;
                                       const _height      : Integer     ;
                                       const _subformat   : TGIS_LayerPixelSubFormat     ;
                                       const _ppi         : Integer     ;
                                       const _cs          : TGIS_CSCoordinateSystem
                                     ) ; overload; override;

      /// <inheritdoc/>
      procedure   Write              ( const _x       : Integer ;
                                       const _y       : Integer ;
                                       const _pixels  : TGIS_Pixels ;
                                       const _pformat : TGIS_PixelFormat ;
                                       const _width   : Integer ;
                                       const _height  : Integer
                                     ) ; override;
      /// <inheritdoc/>
      procedure   WriteGrid           ( const _x      : Integer ;
                                        const _y      : Integer ;
                                        const _grd    : TGIS_GridArray
                                      ) ; override;
  end ;

  /// <summary>
  ///   reads TGIS_FileTIFF_Tag structure from stream
  /// </summary>
  /// <returns>
  ///    number of read bytes
  /// </returns>
  /// <param name="_stream">
  ///    input stram
  /// </param>
  /// <param name="_ft">
  ///    read structure
  /// </param>
  function  ReadFT  ( _stream : TGIS_BaseStream; var _ft : TGIS_FileTIFF_Tag) : Integer ;

  /// <summary>
  ///   reads TGIS_FileTIFF_Tag64 structure from stream
  /// </summary>
  /// <returns>
  ///    number of read bytes
  /// </returns>
  /// <param name="_stream">
  ///    input stram
  /// </param>
  /// <param name="_ft">
  ///    read structure
  /// </param>
  function  ReadFT64( _stream : TGIS_BaseStream; var _ft : TGIS_FileTIFF_Tag64) : Integer ;

  /// <summary>
  ///   writes TGIS_FileTIFF_Tag structure to stream
  /// </summary>
  /// <returns>
  ///    number of written bytes
  /// </returns>
  /// <param name="_stream">
  ///    output stram
  /// </param>
  /// <param name="_ft">
  ///    structure to write
  /// </param>
  function  WriteFT ( _stream : TGIS_BaseStream; var _ft : TGIS_FileTIFF_Tag ) : Integer ;

  /// <summary>
  ///   reads TGIS_FileTIFF_TagGeo structure from stream
  /// </summary>
  /// <returns>
  ///    number of read bytes
  /// </returns>
  /// <param name="_stream">
  ///    input stram
  /// </param>
  /// <param name="_ftg">
  ///    read structure
  /// </param>
  function  ReadFTG ( _stream : TGIS_BaseStream; var _ftg : TGIS_FileTIFF_TagGeo) : Integer ;

  /// <summary>
  ///   writes TGIS_FileTIFF_TagGeo structure to stream
  /// </summary>
  /// <returns>
  ///    number of written bytes
  /// </returns>
  /// <param name="_stream">
  ///    output stram
  /// </param>
  /// <param name="_ftg">
  ///    structure to write
  /// </param>
  function  WriteFTG( _stream : TGIS_BaseStream; var _ftg : TGIS_FileTIFF_TagGeo ) : Integer ;

  /// <summary>
  ///   reads TGIS_FileTIFF_GeoKey structure from stream
  /// </summary>
  /// <returns>
  ///    number of read bytes
  /// </returns>
  /// <param name="_stream">
  ///    input stram
  /// </param>
  /// <param name="_fgk">
  ///    read structure
  /// </param>
  function  ReadFGK ( _stream : TGIS_BaseStream; var _fgk : TGIS_FileTIFF_GeoKey) : Integer ;

  /// <summary>
  ///   writes TGIS_FileTIFF_GeoKey structure to stream
  /// </summary>
  /// <returns>
  ///    nubber of written bytes
  /// </returns>
  /// <param name="_stream">
  ///    output stram
  /// </param>
  /// <param name="_fgk">
  ///    structure to write
  /// </param>
  function  WriteFGK( _stream : TGIS_BaseStream; var _fgk : TGIS_FileTIFF_GeoKey ) : Integer ;

//##############################################################################
implementation

{$IFDEF DCC}
{$A-}
{$ENDIF}

{$IFDEF DCC}
  uses
    GisResource,
    GisClasses ;
{$ENDIF}

const
    // LZW support. Preallocated Byte (min 3) (great is more quick).
    // LZW compression constants.
    XEOF = -1 ;
    MAXBITS = 12 ;
    BITS = 12 ;
    MAXMAXCODE = 1 shl BITS -1 ;

type
  // TIFF tags container class.
  T_ArrayTIFF = class
    public
      TIFF_Tags  : array [0..28] of TGIS_FileTIFF_Tag ;
    private
      procedure fill_TIFF_Tags  ;
    public
      constructor Create ;
  end ;

var

  XResNumerator   : Integer ;
  XResDenominator : Integer ;

const
    BitsPerSample   : array[0..2] of Word = ( $0008, $0008, $0008 );
    BitsPerSample32 : array[0..3] of Word = ( $0008, $0008, $0008, $0008 );
    BASE_TAGS_NO   = 12 ;

  // LZW encoding and decoding support
    NoLZWCode = 4096;
    EOICODE=257;
    CLEARCODE=256;

  // CCITT encoding support
    EOL       = $001 ; // EOL code value - 0000 0000 0000 1

    S_NULL    = 0    ;
    S_PASS    = 1    ;
    S_HORIZ   = 2    ;
    S_V0      = 3    ;
    S_VR      = 4    ;
    S_VL      = 5    ;
    S_EXT     = 6    ;
    S_TERMW   = 7    ;
    S_TERMB   = 8    ;
    S_MAKEUPW = 9    ;
    S_MAKEUPB = 10   ;
    S_MAKEUP  = 11   ;
    S_EOL     = 12   ;

{$IFNDEF JAVA}
  T_TIFFFileHeader : array [0..7] of Byte = (
    $49, $49, $2a, $00, $08, $00, $00, $00  );

  T_TIFFBitRevTable : array [0..255] of Byte = (
    $00, $80, $40, $c0, $20, $a0, $60, $e0,
    $10, $90, $50, $d0, $30, $b0, $70, $f0,
    $08, $88, $48, $c8, $28, $a8, $68, $e8,
    $18, $98, $58, $d8, $38, $b8, $78, $f8,
    $04, $84, $44, $c4, $24, $a4, $64, $e4,
    $14, $94, $54, $d4, $34, $b4, $74, $f4,
    $0c, $8c, $4c, $cc, $2c, $ac, $6c, $ec,
    $1c, $9c, $5c, $dc, $3c, $bc, $7c, $fc,
    $02, $82, $42, $c2, $22, $a2, $62, $e2,
    $12, $92, $52, $d2, $32, $b2, $72, $f2,
    $0a, $8a, $4a, $ca, $2a, $aa, $6a, $ea,
    $1a, $9a, $5a, $da, $3a, $ba, $7a, $fa,
    $06, $86, $46, $c6, $26, $a6, $66, $e6,
    $16, $96, $56, $d6, $36, $b6, $76, $f6,
    $0e, $8e, $4e, $ce, $2e, $ae, $6e, $ee,
    $1e, $9e, $5e, $de, $3e, $be, $7e, $fe,
    $01, $81, $41, $c1, $21, $a1, $61, $e1,
    $11, $91, $51, $d1, $31, $b1, $71, $f1,
    $09, $89, $49, $c9, $29, $a9, $69, $e9,
    $19, $99, $59, $d9, $39, $b9, $79, $f9,
    $05, $85, $45, $c5, $25, $a5, $65, $e5,
    $15, $95, $55, $d5, $35, $b5, $75, $f5,
    $0d, $8d, $4d, $cd, $2d, $ad, $6d, $ed,
    $1d, $9d, $5d, $dd, $3d, $bd, $7d, $fd,
    $03, $83, $43, $c3, $23, $a3, $63, $e3,
    $13, $93, $53, $d3, $33, $b3, $73, $f3,
    $0b, $8b, $4b, $cb, $2b, $ab, $6b, $eb,
    $1b, $9b, $5b, $db, $3b, $bb, $7b, $fb,
    $07, $87, $47, $c7, $27, $a7, $67, $e7,
    $17, $97, $57, $d7, $37, $b7, $77, $f7,
    $0f, $8f, $4f, $cf, $2f, $af, $6f, $ef,
    $1f, $9f, $5f, $df, $3f, $bf, $7f, $ff
  ) ;

  T_TIFFGroup4MainTableState : array [0..127] of Byte = (
    12 ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,1  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,
    5  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,1  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,
    5  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,1  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,
    4  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,1  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,
    6  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,1  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,
    5  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,1  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,
    4  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,1  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,
    4  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,1  ,3  ,5  ,3  ,2  ,3  ,4  ,3
  ) ;

  T_TIFFGroup4MainTableWidth : array [0..127] of Byte = (
    7  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,4  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,
    6  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,4  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,
    7  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,4  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,
    6  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,4  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,
    7  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,4  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,
    6  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,4  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,
    7  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,4  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,
    6  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,4  ,1  ,3  ,1  ,3  ,1  ,3  ,1
  ) ;

  T_TIFFGroup4MainTableParam : array [0..127] of Word = (
    0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,
    2  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,
    3  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,
    2  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,
    0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,
    2  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,
    3  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,
    2  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0
  ) ;

  T_TIFFGroup4WhiteTableState : array [0..4095] of Byte  = (
    12 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    12 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
    7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7
  ) ;

  T_TIFFGroup4WhiteTableWidth : array [0..4095] of Byte = (
    11 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    11 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    11 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    12 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    12 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    12 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    11 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    12 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    12 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    11 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    11 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    11 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    12 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    12 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    12 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    11 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    12 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    12 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
    7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
    8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
    8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
    7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
    6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4
  ) ;

  T_TIFFGroup4WhiteTableParam : array [0..4095] of Word = (
    0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
    45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
    13  ,3   ,1472,5   ,43  ,17  ,1216,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
    47  ,3   ,59  ,5   ,41  ,16  ,960 ,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,704 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    1792,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,40  ,16  ,832 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
    46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
    13  ,3   ,1600,5   ,44  ,17  ,1344,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
    48  ,3   ,60  ,5   ,42  ,16  ,1088,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
    45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
    13  ,3   ,1536,5   ,43  ,17  ,1280,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
    47  ,3   ,59  ,5   ,41  ,16  ,1024,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,768 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    1856,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,40  ,16  ,896 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
    46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
    13  ,3   ,1728,5   ,44  ,17  ,1408,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
    48  ,3   ,60  ,5   ,42  ,16  ,1152,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
    45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
    13  ,3   ,1472,5   ,43  ,17  ,1216,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
    47  ,3   ,59  ,5   ,41  ,16  ,960 ,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,704 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    2112,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,40  ,16  ,832 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
    46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
    13  ,3   ,1600,5   ,44  ,17  ,1344,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
    48  ,3   ,60  ,5   ,42  ,16  ,1088,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
    45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
    13  ,3   ,1536,5   ,43  ,17  ,1280,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
    47  ,3   ,59  ,5   ,41  ,16  ,1024,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,768 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    2368,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,40  ,16  ,896 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
    46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
    13  ,3   ,1728,5   ,44  ,17  ,1408,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
    48  ,3   ,60  ,5   ,42  ,16  ,1152,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
    45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
    13  ,3   ,1472,5   ,43  ,17  ,1216,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
    47  ,3   ,59  ,5   ,41  ,16  ,960 ,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,704 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    1984,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,40  ,16  ,832 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
    46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
    13  ,3   ,1600,5   ,44  ,17  ,1344,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
    48  ,3   ,60  ,5   ,42  ,16  ,1088,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
    45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
    13  ,3   ,1536,5   ,43  ,17  ,1280,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
    47  ,3   ,59  ,5   ,41  ,16  ,1024,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,768 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    1920,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,40  ,16  ,896 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
    46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
    13  ,3   ,1728,5   ,44  ,17  ,1408,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
    48  ,3   ,60  ,5   ,42  ,16  ,1152,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
    45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
    13  ,3   ,1472,5   ,43  ,17  ,1216,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
    47  ,3   ,59  ,5   ,41  ,16  ,960 ,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,704 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    2240,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,40  ,16  ,832 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
    46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
    13  ,3   ,1600,5   ,44  ,17  ,1344,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
    48  ,3   ,60  ,5   ,42  ,16  ,1088,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
    45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
    13  ,3   ,1536,5   ,43  ,17  ,1280,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
    47  ,3   ,59  ,5   ,41  ,16  ,1024,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,768 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    2496,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,40  ,16  ,896 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
    46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
    13  ,3   ,1728,5   ,44  ,17  ,1408,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
    48  ,3   ,60  ,5   ,42  ,16  ,1152,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
    45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
    13  ,3   ,1472,5   ,43  ,17  ,1216,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
    47  ,3   ,59  ,5   ,41  ,16  ,960 ,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,704 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    1792,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,40  ,16  ,832 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
    46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
    13  ,3   ,1600,5   ,44  ,17  ,1344,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
    48  ,3   ,60  ,5   ,42  ,16  ,1088,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
    45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
    13  ,3   ,1536,5   ,43  ,17  ,1280,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
    47  ,3   ,59  ,5   ,41  ,16  ,1024,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,768 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    1856,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,40  ,16  ,896 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
    46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
    13  ,3   ,1728,5   ,44  ,17  ,1408,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
    48  ,3   ,60  ,5   ,42  ,16  ,1152,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
    45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
    13  ,3   ,1472,5   ,43  ,17  ,1216,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
    47  ,3   ,59  ,5   ,41  ,16  ,960 ,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,704 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    2176,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,40  ,16  ,832 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
    46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
    13  ,3   ,1600,5   ,44  ,17  ,1344,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
    48  ,3   ,60  ,5   ,42  ,16  ,1088,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
    45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
    13  ,3   ,1536,5   ,43  ,17  ,1280,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
    47  ,3   ,59  ,5   ,41  ,16  ,1024,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,768 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    2432,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,40  ,16  ,896 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
    46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
    13  ,3   ,1728,5   ,44  ,17  ,1408,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
    48  ,3   ,60  ,5   ,42  ,16  ,1152,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
    45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
    13  ,3   ,1472,5   ,43  ,17  ,1216,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
    47  ,3   ,59  ,5   ,41  ,16  ,960 ,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,704 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    2048,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,40  ,16  ,832 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
    46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
    13  ,3   ,1600,5   ,44  ,17  ,1344,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
    48  ,3   ,60  ,5   ,42  ,16  ,1088,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
    45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
    13  ,3   ,1536,5   ,43  ,17  ,1280,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
    47  ,3   ,59  ,5   ,41  ,16  ,1024,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,768 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    1920,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,40  ,16  ,896 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
    46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
    13  ,3   ,1728,5   ,44  ,17  ,1408,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
    48  ,3   ,60  ,5   ,42  ,16  ,1152,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
    45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
    13  ,3   ,1472,5   ,43  ,17  ,1216,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
    47  ,3   ,59  ,5   ,41  ,16  ,960 ,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,704 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    2304,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,40  ,16  ,832 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
    46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
    13  ,3   ,1600,5   ,44  ,17  ,1344,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
    48  ,3   ,60  ,5   ,42  ,16  ,1088,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
    45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
    13  ,3   ,1536,5   ,43  ,17  ,1280,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
    47  ,3   ,59  ,5   ,41  ,16  ,1024,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,768 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    2560,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
    23  ,3   ,27  ,5   ,40  ,16  ,896 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
    46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
    13  ,3   ,1728,5   ,44  ,17  ,1408,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
    30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
    48  ,3   ,60  ,5   ,42  ,16  ,1152,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
    22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
    13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7
  ) ;

  T_TIFFGroup4BlackTableState : array [0..8191] of Byte = (
    12 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    12 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    12 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    12 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
    8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8
  ) ;

  T_TIFFGroup4BlackTableWidth : array [0..8191] of Byte = (
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
    7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2
  ) ;

  T_TIFFGroup4BlackTableParam : array [0..8191] of Word = (
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    18  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    17  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1792,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    23  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    20  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    25  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    128 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    56  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    30  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1856,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    57  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    21  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    54  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    52  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    48  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    2112,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    44  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    36  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    384 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    28  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    60  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    40  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    2368,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    16  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    64  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    18  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    17  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1984,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    50  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    34  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1664,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    26  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1408,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    32  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1920,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    61  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    42  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1024,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    768 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    62  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    2240,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    46  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    38  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    512 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    19  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    24  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    22  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    2496,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    16  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    64  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    18  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    17  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1792,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    23  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    20  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    25  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    192 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1280,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    31  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1856,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    58  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    21  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    896 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    640 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    49  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    2176,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    45  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    37  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    448 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    29  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1536,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    41  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    2432,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    16  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    64  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    18  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    17  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    2048,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    51  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    35  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    320 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    27  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    59  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    33  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1920,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    256 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    43  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1152,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    55  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    63  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    2304,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    47  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    39  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    53  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    19  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    24  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    22  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    2560,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    16  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    64  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    18  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    17  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1792,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    23  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    20  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    25  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    128 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    56  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    30  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1856,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    57  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    21  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    54  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    52  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    48  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    2112,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    44  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    36  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    384 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    28  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    60  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    40  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    2368,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    16  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    64  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    18  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    17  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1984,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    50  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    34  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1728,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    26  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1472,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    32  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1920,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    61  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    42  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1088,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    832 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    62  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    2240,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    46  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    38  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    576 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    19  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    24  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    22  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    2496,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    16  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    64  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    18  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    17  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1792,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    23  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    20  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    25  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    192 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1344,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    31  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1856,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    58  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    21  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    960 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    704 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    49  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    2176,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    45  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    37  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    448 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    29  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1600,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    41  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    2432,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    16  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    64  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    18  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    17  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    2048,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    51  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    35  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    320 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    27  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    59  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    33  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1920,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    256 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    43  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    1216,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    55  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    63  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    2304,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    47  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    39  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    53  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    19  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    24  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    22  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    2560,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    16  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    64  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
    12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2
  ) ;
{$ELSE}
type
    T_ArraysTIFF = static class
    public
      T_TIFFFileHeader            : array [0..7]    of Byte ;
      T_TIFFBitRevTable           : array [0..255]  of Byte ;
      T_TIFFGroup4MainTableState  : array [0..127]  of Byte ;
      T_TIFFGroup4MainTableWidth  : array [0..127]  of Byte ;
      T_TIFFGroup4MainTableParam  : array [0..127]  of Word ;
      T_TIFFGroup4WhiteTableState : array [0..4095] of Byte ;
      T_TIFFGroup4WhiteTableWidth : array [0..4095] of Byte ;
      T_TIFFGroup4WhiteTableParam : array [0..4095] of Word ;
      T_TIFFGroup4BlackTableState : array [0..8191] of Byte ;
      T_TIFFGroup4BlackTableWidth : array [0..8191] of Byte ;
      T_TIFFGroup4BlackTableParam : array [0..8191] of Word ;
  public
    class constructor Create ;
  private
    class procedure fill_T_TIFFFileHeader ;
    class procedure fill_T_TIFFBitRevTable ;
    class procedure fill_T_TIFFGroup4MainTableState ;
    class procedure fill_T_TIFFGroup4MainTableWidth ;
    class procedure fill_T_TIFFGroup4MainTableParam ;
    class procedure fill_T_TIFFGroup4WhiteTableState ;
    class procedure fill_T_TIFFGroup4WhiteTableWidth ;
    class procedure fill_T_TIFFGroup4WhiteTableParam ;
    class procedure fill_T_TIFFGroup4BlackTableState ;
    class procedure fill_T_TIFFGroup4BlackTableWidth ;
    class procedure fill_T_TIFFGroup4BlackTableParam ;
  end ;
{$ENDIF}
//==============================================================================
//  T_ArraysTIFF
//==============================================================================
  {$IFDEF JAVA}
    class constructor T_ArraysTIFF.Create ;
    begin
      fill_T_TIFFFileHeader ;
      fill_T_TIFFBitRevTable ;
      fill_T_TIFFGroup4MainTableState ;
      fill_T_TIFFGroup4MainTableWidth ;
      fill_T_TIFFGroup4MainTableParam ;
      fill_T_TIFFGroup4WhiteTableState ;
      fill_T_TIFFGroup4WhiteTableWidth ;
      fill_T_TIFFGroup4WhiteTableParam ;
      fill_T_TIFFGroup4BlackTableState ;
      fill_T_TIFFGroup4BlackTableWidth ;
      fill_T_TIFFGroup4BlackTableParam ;
    end ;


    class procedure T_ArraysTIFF.fill_T_TIFFFileHeader ;
    begin
      T_TIFFFileHeader :=[
        $49, $49, $2a, $00, $08, $00, $00, $00 ] ;

    end ;

    class procedure T_ArraysTIFF.fill_T_TIFFBitRevTable ;
    begin
      T_TIFFBitRevTable := [
        $00, $80, $40, $c0, $20, $a0, $60, $e0,
        $10, $90, $50, $d0, $30, $b0, $70, $f0,
        $08, $88, $48, $c8, $28, $a8, $68, $e8,
        $18, $98, $58, $d8, $38, $b8, $78, $f8,
        $04, $84, $44, $c4, $24, $a4, $64, $e4,
        $14, $94, $54, $d4, $34, $b4, $74, $f4,
        $0c, $8c, $4c, $cc, $2c, $ac, $6c, $ec,
        $1c, $9c, $5c, $dc, $3c, $bc, $7c, $fc,
        $02, $82, $42, $c2, $22, $a2, $62, $e2,
        $12, $92, $52, $d2, $32, $b2, $72, $f2,
        $0a, $8a, $4a, $ca, $2a, $aa, $6a, $ea,
        $1a, $9a, $5a, $da, $3a, $ba, $7a, $fa,
        $06, $86, $46, $c6, $26, $a6, $66, $e6,
        $16, $96, $56, $d6, $36, $b6, $76, $f6,
        $0e, $8e, $4e, $ce, $2e, $ae, $6e, $ee,
        $1e, $9e, $5e, $de, $3e, $be, $7e, $fe,
        $01, $81, $41, $c1, $21, $a1, $61, $e1,
        $11, $91, $51, $d1, $31, $b1, $71, $f1,
        $09, $89, $49, $c9, $29, $a9, $69, $e9,
        $19, $99, $59, $d9, $39, $b9, $79, $f9,
        $05, $85, $45, $c5, $25, $a5, $65, $e5,
        $15, $95, $55, $d5, $35, $b5, $75, $f5,
        $0d, $8d, $4d, $cd, $2d, $ad, $6d, $ed,
        $1d, $9d, $5d, $dd, $3d, $bd, $7d, $fd,
        $03, $83, $43, $c3, $23, $a3, $63, $e3,
        $13, $93, $53, $d3, $33, $b3, $73, $f3,
        $0b, $8b, $4b, $cb, $2b, $ab, $6b, $eb,
        $1b, $9b, $5b, $db, $3b, $bb, $7b, $fb,
        $07, $87, $47, $c7, $27, $a7, $67, $e7,
        $17, $97, $57, $d7, $37, $b7, $77, $f7,
        $0f, $8f, $4f, $cf, $2f, $af, $6f, $ef,
        $1f, $9f, $5f, $df, $3f, $bf, $7f, $ff
      ] ;
    end ;

    class procedure T_ArraysTIFF.fill_T_TIFFGroup4MainTableState ;
    begin
      T_TIFFGroup4MainTableState := [
        12 ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,1  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,
        5  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,1  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,
        5  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,1  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,
        4  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,1  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,
        6  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,1  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,
        5  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,1  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,
        4  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,1  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,
        4  ,3  ,5  ,3  ,2  ,3  ,4  ,3  ,1  ,3  ,5  ,3  ,2  ,3  ,4  ,3
      ];
    end ;

    class procedure T_ArraysTIFF.fill_T_TIFFGroup4MainTableWidth ;
    begin
      T_TIFFGroup4MainTableWidth := [
        7  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,4  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,
        6  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,4  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,
        7  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,4  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,
        6  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,4  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,
        7  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,4  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,
        6  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,4  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,
        7  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,4  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,
        6  ,1  ,3  ,1  ,3  ,1  ,3  ,1  ,4  ,1  ,3  ,1  ,3  ,1  ,3  ,1
      ] ;
    end ;

    class procedure T_ArraysTIFF.fill_T_TIFFGroup4MainTableParam ;
    begin
      T_TIFFGroup4MainTableParam := [
        0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,
        2  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,
        3  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,
        2  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,
        0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,
        2  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,
        3  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,
        2  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0  ,0  ,0  ,1  ,0
      ] ;
    end ;

    class procedure T_ArraysTIFF.fill_T_TIFFGroup4WhiteTableState ;
    begin
      T_TIFFGroup4WhiteTableState := [
        12 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        12 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        0  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        11 ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,9  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,9  ,7  ,7  ,7  ,
        7  ,7  ,7  ,7  ,7  ,7  ,9  ,7  ,7  ,7  ,9  ,9  ,7  ,7  ,7  ,7
       ];
    end ;

    class procedure T_ArraysTIFF.fill_T_TIFFGroup4WhiteTableWidth ;
    begin
      T_TIFFGroup4WhiteTableWidth := [
        11 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        11 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        11 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        12 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        12 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        12 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        11 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        12 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        12 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        11 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        11 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        11 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        12 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        12 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        12 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        11 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        12 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        0  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,8  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,9  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        12 ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,7  ,5  ,7  ,6  ,7  ,4  ,4  ,4  ,
        7  ,4  ,7  ,4  ,8  ,6  ,9  ,4  ,7  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,8  ,5  ,8  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,9  ,4  ,8  ,6  ,9  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4  ,
        8  ,4  ,5  ,4  ,6  ,5  ,6  ,4  ,8  ,5  ,8  ,6  ,8  ,4  ,4  ,4  ,
        8  ,4  ,8  ,4  ,8  ,6  ,9  ,4  ,8  ,5  ,8  ,5  ,5  ,4  ,4  ,4  ,
        7  ,4  ,5  ,4  ,7  ,5  ,8  ,4  ,8  ,5  ,7  ,6  ,8  ,4  ,4  ,4  ,
        6  ,4  ,7  ,4  ,7  ,6  ,7  ,4  ,6  ,5  ,6  ,5  ,5  ,4  ,4  ,4
      ] ;
    end ;

    class procedure T_ArraysTIFF.fill_T_TIFFGroup4WhiteTableParam ;
    begin
      T_TIFFGroup4WhiteTableParam := [
        0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
        45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
        13  ,3   ,1472,5   ,43  ,17  ,1216,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
        47  ,3   ,59  ,5   ,41  ,16  ,960 ,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,704 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        1792,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,40  ,16  ,832 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
        46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
        13  ,3   ,1600,5   ,44  ,17  ,1344,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
        48  ,3   ,60  ,5   ,42  ,16  ,1088,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
        45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
        13  ,3   ,1536,5   ,43  ,17  ,1280,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
        47  ,3   ,59  ,5   ,41  ,16  ,1024,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,768 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        1856,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,40  ,16  ,896 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
        46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
        13  ,3   ,1728,5   ,44  ,17  ,1408,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
        48  ,3   ,60  ,5   ,42  ,16  ,1152,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
        45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
        13  ,3   ,1472,5   ,43  ,17  ,1216,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
        47  ,3   ,59  ,5   ,41  ,16  ,960 ,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,704 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        2112,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,40  ,16  ,832 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
        46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
        13  ,3   ,1600,5   ,44  ,17  ,1344,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
        48  ,3   ,60  ,5   ,42  ,16  ,1088,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
        45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
        13  ,3   ,1536,5   ,43  ,17  ,1280,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
        47  ,3   ,59  ,5   ,41  ,16  ,1024,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,768 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        2368,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,40  ,16  ,896 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
        46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
        13  ,3   ,1728,5   ,44  ,17  ,1408,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
        48  ,3   ,60  ,5   ,42  ,16  ,1152,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
        45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
        13  ,3   ,1472,5   ,43  ,17  ,1216,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
        47  ,3   ,59  ,5   ,41  ,16  ,960 ,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,704 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        1984,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,40  ,16  ,832 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
        46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
        13  ,3   ,1600,5   ,44  ,17  ,1344,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
        48  ,3   ,60  ,5   ,42  ,16  ,1088,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
        45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
        13  ,3   ,1536,5   ,43  ,17  ,1280,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
        47  ,3   ,59  ,5   ,41  ,16  ,1024,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,768 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        1920,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,40  ,16  ,896 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
        46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
        13  ,3   ,1728,5   ,44  ,17  ,1408,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
        48  ,3   ,60  ,5   ,42  ,16  ,1152,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
        45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
        13  ,3   ,1472,5   ,43  ,17  ,1216,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
        47  ,3   ,59  ,5   ,41  ,16  ,960 ,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,704 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        2240,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,40  ,16  ,832 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
        46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
        13  ,3   ,1600,5   ,44  ,17  ,1344,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
        48  ,3   ,60  ,5   ,42  ,16  ,1088,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
        45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
        13  ,3   ,1536,5   ,43  ,17  ,1280,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
        47  ,3   ,59  ,5   ,41  ,16  ,1024,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,768 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        2496,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,40  ,16  ,896 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
        46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
        13  ,3   ,1728,5   ,44  ,17  ,1408,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
        48  ,3   ,60  ,5   ,42  ,16  ,1152,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
        45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
        13  ,3   ,1472,5   ,43  ,17  ,1216,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
        47  ,3   ,59  ,5   ,41  ,16  ,960 ,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,704 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        1792,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,40  ,16  ,832 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
        46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
        13  ,3   ,1600,5   ,44  ,17  ,1344,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
        48  ,3   ,60  ,5   ,42  ,16  ,1088,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
        45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
        13  ,3   ,1536,5   ,43  ,17  ,1280,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
        47  ,3   ,59  ,5   ,41  ,16  ,1024,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,768 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        1856,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,40  ,16  ,896 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
        46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
        13  ,3   ,1728,5   ,44  ,17  ,1408,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
        48  ,3   ,60  ,5   ,42  ,16  ,1152,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
        45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
        13  ,3   ,1472,5   ,43  ,17  ,1216,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
        47  ,3   ,59  ,5   ,41  ,16  ,960 ,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,704 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        2176,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,40  ,16  ,832 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
        46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
        13  ,3   ,1600,5   ,44  ,17  ,1344,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
        48  ,3   ,60  ,5   ,42  ,16  ,1088,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
        45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
        13  ,3   ,1536,5   ,43  ,17  ,1280,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
        47  ,3   ,59  ,5   ,41  ,16  ,1024,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,768 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        2432,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,40  ,16  ,896 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
        46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
        13  ,3   ,1728,5   ,44  ,17  ,1408,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
        48  ,3   ,60  ,5   ,42  ,16  ,1152,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
        45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
        13  ,3   ,1472,5   ,43  ,17  ,1216,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
        47  ,3   ,59  ,5   ,41  ,16  ,960 ,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,704 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        2048,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,40  ,16  ,832 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
        46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
        13  ,3   ,1600,5   ,44  ,17  ,1344,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
        48  ,3   ,60  ,5   ,42  ,16  ,1088,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
        45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
        13  ,3   ,1536,5   ,43  ,17  ,1280,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
        47  ,3   ,59  ,5   ,41  ,16  ,1024,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,768 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        1920,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,40  ,16  ,896 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
        46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
        13  ,3   ,1728,5   ,44  ,17  ,1408,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
        48  ,3   ,60  ,5   ,42  ,16  ,1152,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
        45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
        13  ,3   ,1472,5   ,43  ,17  ,1216,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
        47  ,3   ,59  ,5   ,41  ,16  ,960 ,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,704 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        2304,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,40  ,16  ,832 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
        46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
        13  ,3   ,1600,5   ,44  ,17  ,1344,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
        48  ,3   ,60  ,5   ,42  ,16  ,1088,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        0   ,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,39  ,16  ,576 ,6   ,19  ,8   ,55  ,64  ,10  ,4   ,2   ,7   ,
        45  ,3   ,11  ,5   ,53  ,9   ,448 ,6   ,35  ,128 ,51  ,15  ,63  ,4   ,2   ,7   ,
        13  ,3   ,1536,5   ,43  ,17  ,1280,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        29  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,33  ,128 ,49  ,14  ,61  ,4   ,2   ,7   ,
        47  ,3   ,59  ,5   ,41  ,16  ,1024,6   ,31  ,8   ,57  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,768 ,6   ,37  ,128 ,25  ,15  ,320 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        2560,3   ,11  ,5   ,12  ,9   ,1664,6   ,20  ,128 ,24  ,14  ,28  ,4   ,2   ,7   ,
        23  ,3   ,27  ,5   ,40  ,16  ,896 ,6   ,19  ,8   ,56  ,64  ,10  ,4   ,2   ,7   ,
        46  ,3   ,11  ,5   ,54  ,9   ,512 ,6   ,36  ,128 ,52  ,15  ,0   ,4   ,2   ,7   ,
        13  ,3   ,1728,5   ,44  ,17  ,1408,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7   ,
        30  ,3   ,11  ,5   ,12  ,9   ,1664,6   ,34  ,128 ,50  ,14  ,62  ,4   ,2   ,7   ,
        48  ,3   ,60  ,5   ,42  ,16  ,1152,6   ,32  ,8   ,58  ,64  ,10  ,4   ,2   ,7   ,
        22  ,3   ,11  ,5   ,26  ,9   ,640 ,6   ,38  ,128 ,25  ,15  ,384 ,4   ,2   ,7   ,
        13  ,3   ,18  ,5   ,21  ,17  ,256 ,6   ,1   ,8   ,192 ,64  ,10  ,4   ,2   ,7
      ] ;
    end ;

    class procedure T_ArraysTIFF.fill_T_TIFFGroup4BlackTableState ;
    begin
     T_TIFFGroup4BlackTableState := [
        12 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        12 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        12 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        12 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        0  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        11 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        10 ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,
        8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8  ,8
      ] ;
    end ;

    class procedure T_ArraysTIFF.fill_T_TIFFGroup4BlackTableWidth ;
    begin
      T_TIFFGroup4BlackTableWidth := [
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        13 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        9  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        0  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        11 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        12 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        10 ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        8  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,6  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,
        7  ,2  ,3  ,2  ,4  ,2  ,3  ,2  ,5  ,2  ,3  ,2  ,4  ,2  ,3  ,2
      ] ;
    end ;

    class procedure T_ArraysTIFF.fill_T_TIFFGroup4BlackTableParam ;
    begin
      T_TIFFGroup4BlackTableParam := [
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        18  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        17  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1792,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        23  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        20  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        25  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        128 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        56  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        30  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1856,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        57  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        21  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        54  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        52  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        48  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        2112,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        44  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        36  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        384 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        28  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        60  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        40  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        2368,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        16  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        64  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        18  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        17  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1984,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        50  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        34  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1664,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        26  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1408,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        32  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1920,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        61  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        42  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1024,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        768 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        62  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        2240,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        46  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        38  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        512 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        19  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        24  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        22  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        2496,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        16  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        64  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        18  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        17  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1792,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        23  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        20  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        25  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        192 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1280,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        31  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1856,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        58  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        21  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        896 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        640 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        49  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        2176,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        45  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        37  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        448 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        29  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1536,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        41  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        2432,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        16  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        64  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        18  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        17  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        2048,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        51  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        35  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        320 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        27  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        59  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        33  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1920,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        256 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        43  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1152,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        55  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        63  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        2304,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        47  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        39  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        53  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        19  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        24  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        22  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        2560,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        16  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        64  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        18  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        17  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1792,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        23  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        20  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        25  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        128 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        56  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        30  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1856,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        57  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        21  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        54  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        52  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        48  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        2112,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        44  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        36  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        384 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        28  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        60  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        40  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        2368,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        16  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        64  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        18  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        17  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1984,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        50  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        34  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1728,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        26  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1472,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        32  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1920,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        61  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        42  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1088,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        832 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        62  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        2240,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        46  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        38  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        576 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        19  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        24  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        22  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        2496,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        16  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        64  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        18  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        17  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1792,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        23  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        20  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        25  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        192 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1344,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        31  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1856,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        58  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        21  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        960 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        704 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        49  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        2176,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        45  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        37  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        448 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        29  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1600,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        41  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        2432,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        16  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        64  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        18  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        17  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        2048,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        51  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        35  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        320 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        27  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        59  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        33  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1920,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        256 ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        43  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        1216,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        15  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        55  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        63  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        2304,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        47  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        39  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        53  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        13  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        19  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        24  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        22  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        2560,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        10  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        16  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        0   ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        64  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,9   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        11  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        14  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,8   ,3   ,1   ,2   ,5   ,3   ,4   ,2   ,
        12  ,3   ,1   ,2   ,6   ,3   ,4   ,2   ,7   ,3   ,1   ,2   ,5   ,3   ,4   ,2
      ] ;
    end ;
  {$ENDIF}

//==============================================================================
//  T_ArraysTIFF
//==============================================================================
  constructor T_ArrayTIFF.Create ;
  begin
    inherited Create;

    fill_TIFF_Tags ;
  end ;

  procedure T_ArrayTIFF.fill_TIFF_Tags ;
  begin
    TIFF_Tags[ 0].Tag := TIFF_TAG_SUBFILETYPE         ; TIFF_Tags[ 0].VarType := $0004 ; TIFF_Tags[ 0].Count := $00000001 ;
                                                        TIFF_Tags[ 0].Value := $00000000 ;
    TIFF_Tags[ 1].Tag := TIFF_TAG_IMAGEWIDTH          ; TIFF_Tags[ 1].VarType := $0004 ; TIFF_Tags[ 1].Count := $00000001 ;
                                                        TIFF_Tags[ 1].Value := $00000000 ;
    TIFF_Tags[ 2].Tag := TIFF_TAG_IMAGELENGTH         ; TIFF_Tags[ 2].VarType := $0004 ; TIFF_Tags[ 2].Count := $00000001 ;
                                                        TIFF_Tags[ 2].Value := $00000000 ;
    TIFF_Tags[ 3].Tag := TIFF_TAG_BITSPERSAMPLE       ; TIFF_Tags[ 3].VarType := $0003 ; TIFF_Tags[ 3].Count := $00000003 ;
                                                        TIFF_Tags[ 3].Value := $00000000 ;
    TIFF_Tags[ 4].Tag := TIFF_TAG_COMPRESSION         ; TIFF_Tags[ 4].VarType := $0003 ; TIFF_Tags[ 4].Count := $00000001 ;
                                                        TIFF_Tags[ 4].Value := $00000001 ;
    TIFF_Tags[ 5].Tag := TIFF_TAG_PHOTOMETRIC         ; TIFF_Tags[ 5].VarType := $0003 ; TIFF_Tags[ 5].Count := $00000001 ;
                                                        TIFF_Tags[ 5].Value := $00000002 ;
    TIFF_Tags[ 6].Tag := TIFF_TAG_SAMPLESPERPIXELS    ; TIFF_Tags[ 6].VarType := $0003 ; TIFF_Tags[ 6].Count := $00000001 ;
                                                        TIFF_Tags[ 6].Value := $00000003 ;
    TIFF_Tags[ 7].Tag := TIFF_TAG_XRESOLUTION         ; TIFF_Tags[ 7].VarType := $0005 ; TIFF_Tags[ 7].Count := $00000001 ;
                                                        TIFF_Tags[ 7].Value := $00000000 ;
    TIFF_Tags[ 8].Tag := TIFF_TAG_YRESOLUTION         ; TIFF_Tags[ 8].VarType := $0005 ; TIFF_Tags[ 8].Count := $00000001 ;
                                                        TIFF_Tags[ 8].Value := $00000000 ;
    TIFF_Tags[ 9].Tag := TIFF_TAG_PLANARCONFIGURATION ; TIFF_Tags[ 9].VarType := $0003 ; TIFF_Tags[ 9].Count := $00000001 ;
                                                        TIFF_Tags[ 9].Value := $00000001 ;
    TIFF_Tags[10].Tag := TIFF_TAG_RESOLUTIONUNIT      ; TIFF_Tags[10].VarType := $0003 ; TIFF_Tags[10].Count := $00000001 ;
                                                        TIFF_Tags[10].Value := $00000002 ;
    TIFF_Tags[11].Tag := TIFF_TAG_SOFTWARE            ; TIFF_Tags[11].VarType := $0002 ; TIFF_Tags[11].Count := $00000001 ;
                                                        TIFF_Tags[11].Value := $00000002 ;
    TIFF_Tags[12].Tag := GEOTIFF_TAG_GEOKEYDIRECTORY  ; TIFF_Tags[12].VarType := $0003 ; TIFF_Tags[12].Count := $00000004 ;
                                                        TIFF_Tags[12].Value := $00000000 ;
    TIFF_Tags[13].Tag := GEOTIFF_TAG_MODELTIEPOINT    ; TIFF_Tags[13].VarType := $000C ; TIFF_Tags[13].Count := $00000006 ;
                                                        TIFF_Tags[13].Value := $00000000 ;
    TIFF_Tags[14].Tag := GEOTIFF_TAG_MODELPIXELSCALE  ; TIFF_Tags[14].VarType := $000C ; TIFF_Tags[14].Count := $00000003 ;
                                                        TIFF_Tags[14].Value := $00000000 ;
    TIFF_Tags[15].Tag := TIFF_TAG_ROWSPERSTRIP        ; TIFF_Tags[15].VarType := $0004 ; TIFF_Tags[15].Count := $00000001 ;
                                                        TIFF_Tags[15].Value := $00000000 ;
    TIFF_Tags[16].Tag := TIFF_TAG_STRIPBYTECOUNTS     ; TIFF_Tags[16].VarType := $0004 ; TIFF_Tags[16].Count := $00000001 ;
                                                        TIFF_Tags[16].Value := $00000000 ;
    TIFF_Tags[17].Tag := TIFF_TAG_STRIPOFFSETS        ; TIFF_Tags[17].VarType := $0004 ; TIFF_Tags[17].Count := $00000001 ;
                                                        TIFF_Tags[17].Value := $00000000 ;
    TIFF_Tags[18].Tag := TIFF_TAG_TILEWIDTH           ; TIFF_Tags[18].VarType := $0003 ; TIFF_Tags[18].Count := $00000001 ;
                                                        TIFF_Tags[18].Value := $00000000 ;
    TIFF_Tags[19].Tag := TIFF_TAG_TILELENGTH          ; TIFF_Tags[19].VarType := $0003 ; TIFF_Tags[19].Count := $00000001 ;
                                                        TIFF_Tags[19].Value := $00000003 ;
    TIFF_Tags[20].Tag := TIFF_TAG_TILEOFFSETS         ; TIFF_Tags[20].VarType := $0004 ; TIFF_Tags[20].Count := $00000001 ;
                                                        TIFF_Tags[20].Value := $00000000 ;
    TIFF_Tags[21].Tag := TIFF_TAG_TILEBYTECOUNT       ; TIFF_Tags[21].VarType := $0004 ; TIFF_Tags[21].Count := $00000001 ;
                                                        TIFF_Tags[21].Value := $00000000 ;
    TIFF_Tags[22].Tag := TIFF_TAG_PREDICTOR           ; TIFF_Tags[22].VarType := $0003 ; TIFF_Tags[22].Count := $00000001 ;
                                                        TIFF_Tags[22].Value := $00000002 ;
    TIFF_Tags[23].Tag := TIFF_TAG_COLORMAPOFFSET      ; TIFF_Tags[23].VarType := $0003 ; TIFF_Tags[23].Count := $00000000 ;
                                                        TIFF_Tags[23].Value := $00000000 ;
    TIFF_Tags[24].Tag := GEOTIFF_TAG_GEODOUBLEPARAMS  ; TIFF_Tags[24].VarType := $000C ; TIFF_Tags[23].Count := $00000000 ;
                                                        TIFF_Tags[24].Value := $00000000 ;
    TIFF_Tags[25].Tag := GEOTIFF_TAG_GEOASCIIPARAMS   ; TIFF_Tags[25].VarType := $0002 ; TIFF_Tags[25].Count := $00000000 ;
                                                        TIFF_Tags[25].Value := $00000000 ;
    TIFF_Tags[26].Tag := TIFF_TAG_EXTRASAMPLES        ; TIFF_Tags[26].VarType := $0003 ; TIFF_Tags[26].Count := $00000001 ;
                                                        TIFF_Tags[26].Value := $00000000 ;
    TIFF_Tags[27].Tag := TIFF_TAG_SAMPLEFORMAT        ; TIFF_Tags[27].VarType := $0003 ; TIFF_Tags[27].Count := $00000001 ;
                                                        TIFF_Tags[27].Value := $00000002 ;
    TIFF_Tags[28].Tag := GEOTIFF_TAG_GDALNODATA       ; TIFF_Tags[28].VarType := $0002 ; TIFF_Tags[28].Count := $00000007 ;
                                                        TIFF_Tags[28].Value := $00000002 ;
  end ;

//==============================================================================
// TGIS_FileTIFF_Tag
//==============================================================================

  function ReadFT(
        _stream : TGIS_BaseStream ;
    var _ft     : TGIS_FileTIFF_Tag
  ) : Integer ;
  begin
    {$IFDEF GIS_NORECORDS}
      _ft := new TGIS_FileTIFF_Tag ;
    {$ENDIF}
    Result := _stream.ReadWord    ( _ft.Tag,     2 ) +
              _stream.ReadWord    ( _ft.VarType, 2 ) +
              _stream.ReadCardinal( _ft.Count,   4 ) +
              _stream.ReadCardinal( _ft.Value,   4 ) ;
  end ;

  function ReadFT64(
        _stream : TGIS_BaseStream ;
    var _ft     : TGIS_FileTIFF_Tag64
  ) : Integer ;
  begin
    {$IFDEF GIS_NORECORDS}
      _ft := new TGIS_FileTIFF_Tag64 ;
    {$ENDIF}
    Result := _stream.ReadWord ( _ft.Tag,     2 ) +
              _stream.ReadWord ( _ft.VarType, 2 ) +
              _stream.ReadInt64( _ft.Count,   8 ) +
              _stream.ReadInt64( _ft.Value,   8 ) ;
  end ;

  function WriteFT(
        _stream : TGIS_BaseStream ;
    var _ft     : TGIS_FileTIFF_Tag
  ) : Integer ;
  begin
    Result := _stream.WriteWord    ( _ft.Tag,     2 ) +
              _stream.WriteWord    ( _ft.VarType, 2 ) +
              _stream.WriteCardinal( _ft.Count,   4 ) +
              _stream.WriteCardinal( _ft.Value,   4 ) ;
  end ;

//==============================================================================
// TGIS_FileTIFF_TagGeo
//==============================================================================

  function ReadFTG(
        _stream : TGIS_BaseStream ;
    var _ftg    : TGIS_FileTIFF_TagGeo
  ) : Integer ;
  begin
    Result := _stream.ReadWord    ( _ftg.KeyID,           2 ) +
              _stream.ReadWord    ( _ftg.TIFFTagLocation, 2 ) +
              _stream.ReadCardinal( _ftg.Count,           4 ) +
              _stream.ReadCardinal( _ftg.Value_Offset,    4 ) ;
  end ;

  function WriteFTG(
        _stream : TGIS_BaseStream ;
    var _ftg    : TGIS_FileTIFF_TagGeo
  ) : Integer ;
  begin
    Result := _stream.WriteWord    ( _ftg.KeyID,           2 ) +
              _stream.WriteWord    ( _ftg.TIFFTagLocation, 2 ) +
              _stream.WriteCardinal( _ftg.Count,           4 ) +
              _stream.WriteCardinal( _ftg.Value_Offset,    4 ) ;
  end ;

//==============================================================================
// TGIS_FileTIFF_GeoKey
//==============================================================================

  function ReadFGK(
        _stream : TGIS_BaseStream ;
    var _fgk    : TGIS_FileTIFF_GeoKey
  ) : Integer ;
  begin
    Result := _stream.ReadWord( _fgk.KeyID,          2 ) +
              _stream.ReadWord( _fgk.KeyType,        2 ) +
              _stream.ReadWord( _fgk.Count,          2 ) +
              _stream.ReadWord( _fgk.Value_Index,    2 ) ;
  end ;

  function WriteFGK(
        _stream : TGIS_BaseStream ;
    var _fgk    : TGIS_FileTIFF_GeoKey
  ) : Integer ;
  begin
    Result := _stream.WriteWord( _fgk.KeyID,          2 ) +
              _stream.WriteWord( _fgk.KeyType,        2 ) +
              _stream.WriteWord( _fgk.Count,          2 ) +
              _stream.WriteWord( _fgk.Value_Index,    2 ) ;
  end ;

//=============================================================================
// TGIS_FileTIFFDecoder
//=============================================================================

  procedure TGIS_FileTIFFDecoder.doDestroy ;
  begin

    tds.LineBuffer := nil ;
    {$IFDEF OXYGENE}
      if assigned( tds.ReadBuffer ) then
          tds.ReadBuffer := nil ;
    {$ENDIF}
    {$IFNDEF JAVA}
      tds.ReadBuffer := nil ;
    {$ENDIF}
    if tds.CurRuns <> nil then
      tds.CurRuns := nil ;
    if tds.RefRuns <> nil then
      tds.RefRuns := nil ;
    if assigned(tds.PackLineTable) then
      tds.PackLineTable := nil ;
    if tds.Compression = TGIS_CompressionType.LZW then
      freeTable ;
    if tds.Compression = TGIS_CompressionType.JPEG then begin
      FreeObject(jpegDecoder) ;
      FreeObject(jpgStream) ;
    end ;
    if tds.Compression = TGIS_CompressionType.ZLIB then begin
      FreeObject(zDecompStream) ;
      FreeObject(zlibStream) ;
    end ;

    inherited ;

  end ;

  procedure TGIS_FileTIFFDecoder.ResetPackLineTable ;
  var
    idx : Integer ;
  begin
    for idx := 0 to tds.TotalRowsNo do begin
      {$IFDEF JAVA}
        tds.PackLineTable[idx] := new TGIS_FileTIFF_PackLineEntry ;
      {$ENDIF}
      tds.PackLineTable[idx].LineOffset := 0 ;
      tds.PackLineTable[idx].StripNo := 0 ;
    end ;
  end ;

  procedure TGIS_FileTIFFDecoder.SetUp ;
  var
    i : Integer ;
    tblen : Integer ;
    tbidx : Integer ;
    bufflen : Integer ;
  begin
    if tds.Compression = TGIS_CompressionType.CCITTHuffmanRLE then begin
      SetLength( tds.RefRuns, Integer((tds.RowPixels+2) * 4) ) ;
      SetLength( tds.CurRuns, Integer((tds.RowPixels+2) * 4) ) ;
      if tds.FillOrder = 2 then
        for i := 0 to 255 do begin
          revTab[i] := Byte(i) ;
        end
      else begin
        for i := 0 to 255 do
          revTab[i] := {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFBitRevTable[i] ;
      end ;
    end
    else
    if tds.Compression = TGIS_CompressionType.CCITTFaxGroup3 then begin
      SetLength( tds.RefRuns, Integer((tds.RowPixels+2) * 4) ) ;
      SetLength( tds.CurRuns, Integer((tds.RowPixels+2) * 4) ) ;
      if tds.FillOrder = 2 then
        for i := 0 to 255 do begin
          revTab[i] := Byte(i) ;
        end
      else begin
        for i := 0 to 255 do
          revTab[i] := {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFBitRevTable[i] ;
      end ;
    end
    else
    if tds.Compression = TGIS_CompressionType.CCITTFaxGroup4 then begin
      SetLength( tds.RefRuns, Integer((tds.RowPixels+2) * 4) ) ;
      SetLength( tds.CurRuns, Integer((tds.RowPixels+2) * 4) ) ;
      if tds.FillOrder = 2 then
        for i := 0 to 255 do begin
          revTab[i] := Byte(i) ;
        end
      else begin
        for i := 0 to 255 do
          revTab[i] := {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFBitRevTable[i] ;
      end ;
    end
    else
    if tds.Compression = TGIS_CompressionType.LZW then begin
      fLineSize := tds.RowBytes ;
      fNextCode := 0;
      lzwOldCode := NoLZWCode ;
      fWPos := 0 ;
      lzwAborting := False ;
    end
    else
    if tds.Compression = TGIS_CompressionType.PackBits then begin
      SetLength(tds.PackLineTable, (tds.TotalRowsNo +1) );
      ResetPackLineTable ;
    end
    else
    if tds.Compression = TGIS_CompressionType.JPEG then begin
      jpgStream := TMemoryStream.Create ;

      if assigned(tds.jpegTables) then begin
         tblen := length(tds.jpegTables) ;
         tbidx := 0 ;
         jpgStream.Seek(0, soBeginning) ;

        {$IFDEF OXYGENE}
          jpgStream.Write(tds.jpegTables, tbidx, tblen -2) ;
        {$ELSE}
          jpgStream.Write(tds.jpegTables[tbidx], tblen -2) ;
        {$ENDIF}
      end ;
      jpegDecoder := TGIS_JPEGDecoder.Create ;
      jpegDecoder.JpegInTiff := True ;

      if tds.RowBytes = tds.RowPixels then
        jpegDecoder.DecodeMode := JPEG_GRAYSCAL
      else
      if tds.RowBytes = 4 * tds.RowPixels then
        jpegDecoder.DecodeMode := JPEG_ARGB
      else
        jpegDecoder.DecodeMode := JPEG_BGR ;
    end
    else
    if tds.Compression = TGIS_CompressionType.ZLIB then begin
      zlibStream := TMemoryStream.Create ;
    end ;

    SetLength( tds.ReadBuffer, tds.ReadBufferSize );

    if tds.Compression = TGIS_CompressionType.LZW then
      bufflen := tds.RowBytes*50
    else
    if tds.Compression = TGIS_CompressionType.ZLIB then
      bufflen := tds.RowBytes*50
    else
      bufflen := 2*((tds.RowBytes +3) div 4)*4 ;

    SetLength( tds.LineBuffer, bufflen )
  end ;

  procedure TGIS_FileTIFFDecoder.tiffGroup4FillRuns(
    var   _buf   : TBytes ;
    var   _runs  : TGIS_CardinalArray ;
    var   _erun  : TGIS_CardinalArray ;
    var   _erunx : Integer ;
    const _lastx : LongWord
  ) ;
  var
    runsx : Integer ;
    cpb : TBytes   ;
    cpx : Integer  ;
    i   : Integer  ;
    cpt : Byte     ;
    x,
    bx,
    run : LongWord ;
    n   : Integer  ;
  const
    _fillmasks : array [0..8] of Byte =
       ( $00, $80, $c0, $e0, $f0, $f8, $fc, $fe, $ff );
  begin
    runsx := 0 ;
    if ( _erunx and 1) = 1 then begin
      _erun[_erunx] := 0 ;
      _erunx := _erunx + 1 ;
    end ;

    x   := 0 ;
    cpb := _buf ;

    while runsx < _erunx do begin
      run := _runs[runsx];
      if (Int64(x)+Int64(run) > Int64(_lastx)) or
         (Int64(run) > Int64(_lastx)) then begin
        _runs[runsx] := LongWord(_lastx -x) ;
        run := _runs[runsx] ;
      end ;
      if run <> 0 then begin
        cpb := _buf ;
        cpx := Integer(x shr 3) ;
        bx := x and $07 ;

        if run > 8-bx then begin
          if bx <> 0 then begin      // align to Byte boundary
            cpt := cpb[cpx] and ($ff shl (8 -bx)) ;
            cpb[cpx] := cpt ;
            cpx := cpx + 1 ;
            run := run -(8-bx);
          end ;
          n := run shr 3 ;
          if n > 0  then begin  // multiple bytes to fill
            for i := 0 to n-1 do
              cpb[cpx+i] := $00 ;
            cpx := cpx + n ;
            run := run and $07;
          end ;
          if run <> 0 then
            cpb[cpx] := cpb[cpx] and ($ff shr run) ;
        end
        else
          cpb[cpx] := cpb[cpx] and (not(_fillmasks[run] shr bx)) ;

        x := x + _runs[runsx] ;
      end ;

      run := _runs[runsx+1];

      if ((Int64(x) +Int64(run)) > Int64(_lastx)) or
          (Int64(run) > Int64(_lastx)) then begin
        _runs[runsx+1] := _lastx -x ;
        run := _runs[runsx+1] ;
      end ;

      if run <> 0 then begin
        cpb := _buf ;
        cpx := Integer(x shr 3) ;
        bx := x and $07;

        if (run > 8-bx) then begin
          if bx <> 0 then begin      // align to Byte boundary
            cpb[cpx] := cpb[cpx] or Byte($ff shr bx) ;
            cpx := cpx + 1 ;
            run := run -(8 -bx) ;
          end ;
          n := run shr 3 ;
          if( n > 0 ) then begin  // multiple bytes to fill
            for i := 0 to n-1 do
              cpb[cpx+i] := $ff ;
            cpx := cpx + n ;
            run := run and $07;
          end ;
          if run <> 0 then
            cpb[cpx] := cpb[cpx] or Byte(($ff00 shr run)) ;
        end
        else
          cpb[cpx] := cpb[cpx] or (_fillmasks[run] shr bx) ;
        x := x + _runs[runsx+1] ;
      end ;
      runsx := runsx + 2 ;
    end ;
  end ;

  function  TGIS_FileTIFFDecoder.readOffset        : Int64 ;
  var
    tmp : TBytes ;
  begin
    tmp := nil ;
    if tds.MustRead then begin
      if (tds.InBufferBytes = 0) or
         (tds.StartOffset < tds.InBufferStart) or
         ((tds.StartOffset + LongWord(tds.TiffRawCC)) > (tds.InBufferStart +
                                               tds.InBufferBytes))
      then begin
        tds.TiffStream.Position := tds.StartOffset ;
        if tds.ReadBufferSize < Cardinal(tds.TiffRawCC) then begin
          tds.ReadBufferSize := tds.TiffRawCC ;
          tds.ReadBuffer := nil ;
          SetLength(tds.ReadBuffer, tds.ReadBufferSize );
        end ;
        {$IFDEF OXYGENE}
          SetLength( tmp, tds.ReadBufferSize ) ;
          try
            tds.TiffStream.Read( tds.ReadBuffer, tds.TiffRawCC ) ;
             { TODO : Verify code }
//?             needed?
//              {$IFDEF CLR}
//                Marshal.Copy( tmp, 0, tds.ReadBuffer, tds.TiffRawCC ) ;
//              {$ENDIF}
//              {$IFDEF JAVA}
//                for counter := 0 to Length(tds.ReadBuffer) - 1 do
//                  tds.ReadBuffer[counter] := tmp[counter];
//              {$ENDIF}
          finally
            tmp := nil ;
          end ;
        {$ELSE}
          tds.TiffStream.Read( tds.ReadBuffer[0], tds.TiffRawCC ) ;
        {$ENDIF}
          tds.InBufferStart := tds.StartOffset ;
          tds.InBufferBytes := tds.TiffRawCC  ;
        end ;
      end ;
      Result := tds.Position  -Integer(tds.InBufferStart) ;
  end ;

  function TGIS_FileTIFFDecoder.expandLine1d : Boolean ;
  var
    tparam : Word ;
  begin
    Result := false ;

    while true do begin
      while true do begin
        if bitsAvail < 12 then begin
          if NativeInt(cpoff) >= NativeInt(epoff) then begin
            if bitsAvail = 0 then begin
              Result := true ;
              cleanupRuns ;
              exit ;
            end ;
            bitsAvail := 12 ;
          end
          else begin
            if cpoff < Integer(tds.ReadBufferSize) then begin
              bitACC := bitACC or
                     (LongWord(revTab[cpbuf[cpoff]]) shl bitsAvail) ;
              inc(cpoff) ;
              bitsAvail := bitsAvail +8 ;
              if bitsAvail < 12 then begin
                if NativeInt(cpoff) >= NativeInt(epoff) then
                  bitsAvail := 12
                else begin
                  bitACC := bitACC or
                            (LongWord(revTab[cpbuf[cpoff]]) shl bitsAvail);
                  inc(cpoff) ;
                  bitsAvail := bitsAvail +8 ;
                end ;
              end ;
            end ;
          end;
        end ;
        tabEntX := bitACC and ((1 shl 12) -1) ;
        tparam := {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4WhiteTableParam[tabEntX] ;

        bitsAvail := bitsAvail - {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4WhiteTableWidth[tabEntX] ;
        bitACC := bitACC shr {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4WhiteTableWidth[tabEntX] ;

        case {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4WhiteTableState[tabEntX] of
          S_TERMW:
            begin
              setVal( tparam ) ;
              break ;
            end ;
          S_MAKEUPW, S_MAKEUP:
            begin
              a0 := a0 +Integer(tparam) ;
              runLength := runLength + Integer( tparam ) ;
            end ;
          else begin
            setVal( tparam ) ;
            Result := true ;
            cleanupRuns ;
            exit ;
          end ;
        end ;
      end ;
      if a0 >= lastx then
        break ;
      while true do begin
        if bitsAvail < 13 then begin
          if NativeInt(cpoff) >= NativeInt(epoff) then begin
            if bitsAvail = 0 then begin
              Result := true ;
              cleanupRuns ;
              exit ;
            end ;
            bitsAvail := 13 ;
          end
          else begin
            bitACC := bitACC or
                      (LongWord(revTab[cpbuf[cpoff]]) shl bitsAvail) ;
            inc(cpoff) ;
            bitsAvail := bitsAvail +8 ;
            if bitsAvail < 13 then begin
              if NativeInt(cpoff) >= NativeInt(epoff) then
                bitsAvail := 13
              else begin
                bitACC := bitACC or
                          (LongWord(revTab[cpbuf[cpoff]]) shl bitsAvail);
                inc(cpoff) ;
                bitsAvail := bitsAvail +8 ;
              end ;
            end ;
          end ;
        end ;

        tabEntX := bitACC and ((1 shl 13) -1) ;
        tparam := {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4BlackTableParam[tabEntX] ;

        bitsAvail := bitsAvail -{$IFDEF JAVA}T_ArraysTIFF.{$ENDIF} T_TIFFGroup4BlackTableWidth[tabEntX] ;
        bitACC := bitACC shr {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4BlackTableWidth[tabEntX] ;

        case {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4BlackTableState[tabEntX] of
          S_TERMB:
            begin
              setVal(  tparam ) ;
              break ;
            end ;
          S_MAKEUPB, S_MAKEUP:
            begin
              a0 := a0 +Integer( tparam ) ;
              runLength := runLength + Integer( tparam ) ;
            end ;
          else begin
            setVal( tparam ) ;
            Result := true ;
            cleanupRuns ;
            exit ;
          end ;
        end ;
      end ;
      if a0 >= lastx then
        break ;
    end ;
    cleanupRuns ;
  end ;

  function TGIS_FileTIFFDecoder.expandLine2d : Boolean ;
  const
    fill_arr : array [0..255] of Byte =
        ( $00, $80, $40, $C0, $20, $A0, $60, $E0,
          $10, $90, $50, $D0, $30, $B0, $70, $F0,
          $08, $88, $48, $C8, $28, $A8, $68, $E8,
          $18, $98, $58, $D8, $38, $B8, $78, $F8,
          $04, $84, $44, $C4, $24, $A4, $64, $E4,
          $14, $94, $54, $D4, $34, $B4, $74, $F4,
          $0C, $8C, $4C, $CC, $2C, $AC, $6C, $EC,
          $1C, $9C, $5C, $DC, $3C, $BC, $7C, $FC,
          $02, $82, $42, $C2, $22, $A2, $62, $E2,
          $12, $92, $52, $D2, $32, $B2, $72, $F2,
          $0A, $8A, $4A, $CA, $2A, $AA, $6A, $EA,
          $1A, $9A, $5A, $DA, $3A, $BA, $7A, $FA,
          $06, $86, $46, $C6, $26, $A6, $66, $E6,
          $16, $96, $56, $D6, $36, $B6, $76, $F6,
          $0E, $8E, $4E, $CE, $2E, $AE, $6E, $EE,
          $1E, $9E, $5E, $DE, $3E, $BE, $7E, $FE,
          $01, $81, $41, $C1, $21, $A1, $61, $E1,
          $11, $91, $51, $D1, $31, $B1, $71, $F1,
          $09, $89, $49, $C9, $29, $A9, $69, $E9,
          $19, $99, $59, $D9, $39, $B9, $79, $F9,
          $05, $85, $45, $C5, $25, $A5, $65, $E5,
          $15, $95, $55, $D5, $35, $B5, $75, $F5,
          $0D, $8D, $4D, $CD, $2D, $AD, $6D, $ED,
          $1D, $9D, $5D, $DD, $3D, $BD, $7D, $FD,
          $03, $83, $43, $C3, $23, $A3, $63, $E3,
          $13, $93, $53, $D3, $33, $B3, $73, $F3,
          $0B, $8B, $4B, $CB, $2B, $AB, $6B, $EB,
          $1B, $9B, $5B, $DB, $3B, $BB, $7B, $FB,
          $07, $87, $47, $C7, $27, $A7, $67, $E7,
          $17, $97, $57, $D7, $37, $B7, $77, $F7,
          $0F, $8F, $4F, $CF, $2F, $AF, $6F, $EF,
          $1F, $9F, $5F, $DF, $3F, $BF, $7F, $FF
        ) ;
  var
    tparam : Word ;

  begin
    Result := false ;

    while a0 < lastx do begin
      if bitsAvail < 7 then begin
        if NativeInt(cpoff) >= NativeInt(epoff) then begin
          if bitsAvail = 0 then begin
            cleanupRuns ;
            Result := true ;
            exit ;
          end ;
          bitsAvail := 7 ;
        end
        else begin
          bitACC := bitACC or
                    (LongWord(revTab[cpbuf[cpoff]]) shl bitsAvail) ;
          inc(cpoff) ;
          bitsAvail := bitsAvail +8 ;
        end ;
      end ;

      tabEntX := bitACC and ((1 shl 7) -1) ;
      tparam := {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4MainTableParam[tabEntX] ;

      bitsAvail := bitsAvail - {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4MainTableWidth[tabEntX] ;
      bitACC := bitACC shr {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4MainTableWidth[tabEntX] ;
      case {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4MainTableState[tabEntX] of
        S_PASS :
            begin
              verify_b1 ;
              b1 := b1 + Integer(pb[pbx]) ;
              pbx := pbx + 1 ;
              runLength := runLength +Integer(b1) -Integer(a0);
              a0 := b1 ;
              b1 := b1 + Integer(pb[pbx]) ;
              pbx := pbx + 1 ;
            end ;
        S_HORIZ :
            begin
              {$IFDEF OXYGENE}
                assert( ( pa = thisRun ) ) ;
              {$ENDIF}
              if (pax and 1) <> 0
              then begin
                while true do begin  //black
                  if bitsAvail < 13 then begin
                    if NativeInt(cpoff) >= NativeInt(epoff) then begin
                      if bitsAvail = 0 then
                      begin
                        cleanupRuns ;
                        Result := true ;
                        exit ;
                      end ;
                      bitsAvail := 13 ;
                    end
                    else begin
                      bitACC := bitACC or
                                (LongWord(revTab[cpbuf[cpoff]]) shl bitsAvail) ;
                       inc(cpoff) ;
                      bitsAvail := bitsAvail +8 ;
                      if bitsAvail < 13 then begin
                        if NativeInt(cpoff) >= NativeInt(epoff) then
                          bitsAvail := 13
                        else begin
                          bitACC := bitACC or
                                    (LongWord(revTab[cpbuf[cpoff]]) shl bitsAvail);
                          inc(cpoff) ;
                          bitsAvail := bitsAvail +8 ;
                        end ;
                      end ;
                    end ;
                  end ;
                  tabEntX := bitACC and ((1 shl 13) -1) ;
                  tparam := {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4BlackTableParam[tabEntX] ;

                  bitsAvail := bitsAvail - {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4BlackTableWidth[tabEntX] ;
                  bitACC := bitACC shr {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4BlackTableWidth[tabEntX] ;

                  case {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4BlackTableState[tabEntX] of
                    S_TERMB :
                      begin
                        setVal( tparam ) ;
                        break ;
                      end ;
                    S_MAKEUPB,
                    S_MAKEUP :
                      begin
                        a0 := a0 +Integer( tparam ) ;
                        runLength := runLength + Integer( tparam ) ;
                      end ;
                    else
                    begin
                      cleanupRuns ;
                      exit ;
                    end ;
                  end ;
                end ;
                while true do begin  //white
                  if bitsAvail < 12 then begin
                    if NativeInt(cpoff) >= NativeInt(epoff) then begin
                      if bitsAvail = 0 then
                      begin
                        cleanupRuns ;
                        Result := true ;
                        exit ;
                      end ;
                      bitsAvail := 12 ;
                    end
                    else begin
                      bitACC := bitACC or
                                (LongWord(revTab[cpbuf[cpoff]]) shl bitsAvail) ;
                      inc(cpoff) ;
                      bitsAvail := bitsAvail +8 ;
                      if bitsAvail < 12 then begin
                        if NativeInt(cpoff) >= NativeInt(epoff) then
                          bitsAvail := 12
                        else begin
                          bitACC := bitACC or
                                   (LongWord(revTab[cpbuf[cpoff]]) shl bitsAvail) ;
                          inc(cpoff) ;
                          bitsAvail := bitsAvail +8 ;
                        end ;
                      end ;
                    end ;
                  end ;
                  tabEntX := bitACC and ((1 shl 12) -1) ;
                  tparam := {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4WhiteTableParam[tabEntX] ;

                  bitsAvail := bitsAvail - {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4WhiteTableWidth[tabEntX] ;
                  bitACC := bitACC shr {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4WhiteTableWidth[tabEntX] ;

                  case {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4WhiteTableState[tabEntX] of
                    S_TERMW :
                      begin
                        setVal( tparam ) ;
                        break ;
                      end ;
                    S_MAKEUPW,
                    S_MAKEUP :
                      begin
                        a0 := a0 +Integer(tparam) ;
                        runLength := runLength + Integer( tparam ) ;
                      end ;
                    else
                    begin
                      cleanupRuns ;
                      exit ;
                    end ;
                  end ;
                end ;
              end
              else begin

                while true do begin //white

                  if bitsAvail < 12 then begin
                    if NativeInt(cpoff) >= NativeInt(epoff) then begin
                      if bitsAvail = 0 then
                      begin
                        cleanupRuns ;
                        Result := true ;
                        exit ;
                      end ;
                      bitsAvail := 12 ;
                    end
                    else begin
                      bitACC := bitACC or
                                (LongWord(revTab[cpbuf[cpoff]]) shl bitsAvail) ;
                      inc(cpoff) ;
                      bitsAvail := bitsAvail +8 ;
                      if bitsAvail < 12 then begin
                        if NativeInt(cpoff) >= NativeInt(epoff) then
                          bitsAvail := 12
                        else begin
                          bitACC := bitACC or
                                    (LongWord(revTab[cpbuf[cpoff]]) shl bitsAvail);
                          inc(cpoff) ;
                          bitsAvail := bitsAvail +8 ;
                        end ;
                      end ;
                    end ;
                  end ;
                  tabEntX := bitACC and ((1 shl 12) -1) ;
                  tparam := {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4WhiteTableParam[tabEntX] ;

                  bitsAvail := bitsAvail - {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4WhiteTableWidth[tabEntX] ;
                  bitACC := bitACC shr {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4WhiteTableWidth[tabEntX] ;

                  case {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4WhiteTableState[tabEntX] of
                    S_TERMW :
                      begin
                        setVal( tparam ) ;
                        break ;
                      end ;
                    S_MAKEUPW,
                    S_MAKEUP :
                      begin
                        a0 := a0 +Integer(tparam) ;
                        runLength := runLength + Integer( tparam ) ;
                      end ;
                    else
                    begin
                      cleanupRuns ;
                      exit ;
                    end ;
                  end ;
                end ;
                while true do begin

                  if bitsAvail < 13 then begin
                    if NativeInt(cpoff) >= NativeInt(epoff) then begin
                      if bitsAvail = 0 then
                      begin
                        cleanupRuns ;
                        Result := true ;
                        exit ;
                      end ;
                      bitsAvail := 13 ;
                    end
                    else begin
                      bitACC := bitACC or
                               (LongWord(revTab[cpbuf[cpoff]]) shl bitsAvail) ;
                      inc(cpoff) ;
                      bitsAvail := bitsAvail +8 ;
                      if bitsAvail < 13 then begin
                        if NativeInt(cpoff) >= NativeInt(epoff) then
                          bitsAvail := 13
                        else begin
                          bitACC := bitACC or
                                    (LongWord(revTab[cpbuf[cpoff]]) shl bitsAvail);
                           inc(cpoff) ;
                          bitsAvail := bitsAvail +8 ;
                        end ;
                      end ;
                    end ;
                  end ;
                  tabEntX := bitACC and ((1 shl 13) -1) ;
                  tparam := {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4BlackTableParam[tabEntX] ;

                  bitsAvail := bitsAvail - {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4BlackTableWidth[tabEntX] ;
                  bitACC := bitACC shr {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4BlackTableWidth[tabEntX] ;

                  case {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4BlackTableState[tabEntX] of
                    S_TERMB :
                      begin
                        setVal( tparam ) ;
                        break ;
                      end ;
                    S_MAKEUPB,
                    S_MAKEUP :
                      begin
                        a0 := a0 +Integer( tparam ) ;
                        runLength := runLength + Integer( tparam ) ;
                      end ;
                    else
                    begin
                      cleanupRuns ;
                      exit ;
                    end ;
                  end ;
                end ;
              end ;
              verify_b1 ;
            end ;
        S_V0 :
            begin
              verify_b1;
              setVal(b1 -a0);
              b1 := b1 + Integer(pb[pbx]) ;
              pbx := pbx + 1 ;
            end ;
        S_VR :
            begin
              verify_b1 ;
              setVal( b1 - a0 + tparam ) ;
              b1 := b1 + Integer(pb[pbx]) ;
              pbx := pbx + 1 ;
            end ;
        S_VL :
            begin
              verify_b1 ;
              setVal( b1 - a0 - tparam ) ;
              if pbx > 0 then
                pbx := pbx - 1 ;
              b1 := b1 - Integer(pb[pbx]) ;
            end ;
        S_EXT :
            begin
              pa[pax] := Cardinal(lastx -a0) ;
              pax := pax + 1 ;
              cleanupRuns ;
              exit ;
            end ;
        S_EOL :
            begin
              pa[pax] := Cardinal(lastx -a0) ;
              pax := pax + 1 ;

              if bitsAvail < 5 then begin
                if NativeInt(cpoff) >= NativeInt(epoff) then begin
                  if bitsAvail = 0 then
                  begin
                    cleanupRuns ;
                    Result := true ;
                    exit ;
                  end ;
                  bitsAvail := 5 ;
                end
                else begin
                  bitACC := bitACC or
                            (LongWord(revTab[cpbuf[cpoff]]) shl bitsAvail) ;
                  inc(cpoff) ;
                  bitsAvail := bitsAvail +8 ;
                end ;
              end ;
              cleanupRuns ;
              Result := true ;
              exit ;
            end ;
        else
        begin
          cleanupRuns ;
          exit ;
        end ;
      end ;
    end ;

    if runLength <> 0 then begin
      if (runLength +a0) < lastx then begin
        if bitsAvail < 1 then begin
          if NativeInt(cpoff) >= NativeInt(epoff) then begin
            if bitsAvail = 0 then
            begin
              cleanupRuns ;
              Result := true ;
              exit ;
            end ;
            bitsAvail := 1 ;
          end
          else begin
            bitACC := bitACC or
                     (LongWord(revTab[cpbuf[cpoff]]) shl bitsAvail) ;
            inc(cpoff) ;
            bitsAvail := bitsAvail +8 ;
          end ;
        end ;

        if bitACC and ((1 shl 1) -1) = 0 then
        begin
          cleanupRuns ;
          exit ;
        end ;
        dec(bitsAvail) ; ;
        bitACC := bitACC shr 1 ;
      end ;
      setVal(0);
    end ;
    cleanupRuns ;
  end ;

  procedure TGIS_FileTIFFDecoder.verify_b1 ;
  begin
    if pax <> 0 then
      while (b1 <= a0) and (b1 < lastx) do begin
        b1 := b1 + Integer(pb[pbx]) + Integer(pb[pbx+1]) ;
        pbx := pbx + 2 ;
      end ;
  end ;

  procedure TGIS_FileTIFFDecoder.setVal(_x : Integer) ;
  begin
    pa[pax] := Cardinal(runLength +_x) ;
    pax := pax + 1 ;
    a0 := a0 +_x ;
    runLength := 0 ;
  end ;

  procedure TGIS_FileTIFFDecoder.cleanupRuns ;
  begin
    if runLength <> 0 then
      setVal(0) ;
    if a0 <> lastx then begin
      while (a0 > lastx) and (pax > 0) do begin
        pax := pax - 1 ;
        a0 := a0 - Integer(pa[pax]);
      end ;
      if a0 < lastx then begin
        if a0 < 0 then
          a0 := 0 ;
        if (pax and 1) <> 0 then
          setVal(0) ;
        setVal(lastx -a0) ;
      end
      else
      if a0 > lastx then begin
        setVal(lastx) ;
        setVal(0) ;
      end ;
    end ;
  end ;

  function  TGIS_FileTIFFDecoder.HuffmanRLEDecodeLine(  const _linenr : Integer ;
                                                        const _tilenr : Integer = -1
                                                     ) : Integer ;
  var
    stripno    : LongWord ;
    stripline  : Integer ;
    line_valid : Boolean ;
    must_preset: Boolean ;
    n          : Integer ;
  begin
    Result := 1 ;

    line_valid := false ;
    tds.MustRead := true ;
    must_preset := true ;

    if _tilenr = -1 then begin
      stripno := LongWord(_linenr) div tds.RowsPerStrip ;
      stripline := _linenr mod Integer(tds.RowsPerStrip) ;
    end
    else begin
      stripno := _tilenr ;
      stripline := _linenr ;
    end ;

    if tds.StripList[stripno].StripOffsets = tds.StartOffset then begin
      tds.MustRead := false ;
      if tds.CurStripLine <= stripline then begin
        must_preset := false ;
        if tds.CurStripLine = stripline then
          line_valid := true ;
      end ;
    end ;

    if tds.MustRead then begin
      tds.CurStrip := stripno ;
      tds.StartOffset := tds.StripList[stripno].StripOffsets ;
      tds.TiffRawCC   := tds.StripList[stripno].StripByteCounts ;
    end ;

    if must_preset then begin
      tds.CurStripLine := -1 ;
      tds.Bit := 0; // force initial read
      tds.Data := 0;
      tds.RefRuns[0] := tds.RowPixels ;
      for n := 1 to length(tds.RefRuns) - 1 do
        tds.RefRuns[n] := 0 ;

      bitACC := tds.Data ;
      bitsAvail := tds.Bit ;
      tds.Position := Integer(tds.StartOffset) ;
      cpoff := readOffset ;
      cpbuf := tds.ReadBuffer ;
      epbuf := cpbuf ;
      epoff := cpoff +tds.TiffRawCC ;
      lastx := tds.RowPixels ;
    end ;

    thisRun := TGIS_CardinalArray(tds.CurRuns) ;
    while not line_valid do begin
      a0 := 0 ;
      runLength := 0 ;
      pa := TGIS_CardinalArray(thisRun) ;
      pax := 0 ;

      pb := TGIS_CardinalArray(tds.RefRuns) ;
      pbx := 0 ;

      expandLine1d ;

      inc(tds.CurStripLine) ;
      if tds.CurStripLine = stripline then begin
        line_valid := true ;
        tiffGroup4FillRuns( tds.LineBuffer, thisRun, pa, pax, lastx ) ;
      end ;
      n := bitsAvail - (bitsAvail and (not Integer(7))) ;
      bitsAvail := bitsAvail -n ;
      bitACC := bitACC shr n ;

    end ;

  end ;

  function  TGIS_FileTIFFDecoder.Group3DecodeLine (  const _linenr : Integer ;
                                                     const _tilenr : Integer = -1
                                                  ) : Integer ;
  var
    stripno    : LongWord ;
    stripline  : Integer ;
    line_valid : Boolean ;
    must_preset: Boolean ;
    n          : Integer ;
  begin

    Result := 1 ;

    line_valid := false ;
    tds.MustRead := true ;
    must_preset := true ;

    if _tilenr = -1 then begin
      stripno := LongWord(_linenr) div tds.RowsPerStrip ;
      stripline := _linenr mod Integer(tds.RowsPerStrip) ;
    end
    else begin
      stripno := _tilenr ;
      stripline := _linenr ;
    end ;

    if tds.StripList[stripno].StripOffsets = tds.StartOffset then begin
      tds.MustRead := false ;
      if tds.CurStripLine <= stripline then begin
        must_preset := false ;
        if tds.CurStripLine = stripline then
          line_valid := true ;
      end ;
    end ;

    if tds.MustRead then begin
      tds.CurStrip := stripno ;
      tds.StartOffset := tds.StripList[stripno].StripOffsets ;
      tds.TiffRawCC   := tds.StripList[stripno].StripByteCounts ;
    end ;

    if must_preset then begin
      tds.CurStripLine := -1 ;
      tds.Bit := 0; // force initial read
      tds.Data := 0;
      tds.RefRuns[0] := tds.RowPixels ;
      for n := 1 to length(tds.RefRuns) - 1 do
        tds.RefRuns[n] := 0 ;

      bitACC := tds.Data ;
      bitsAvail := tds.Bit ;
      tds.Position := Integer(tds.StartOffset) ;
      cpoff := readOffset ;
      cpbuf := tds.ReadBuffer ;
      epbuf := cpbuf ;
      epoff := cpoff +tds.TiffRawCC ;
      lastx := tds.RowPixels ;
    end ;

    while not line_valid do begin
      a0 := 0 ;
      runLength := 0 ;
      thisRun := TGIS_CardinalArray(tds.CurRuns) ;
      pa := TGIS_CardinalArray(thisRun) ;
      pax := 0 ;
      bitACC := bitACC or (LongWord(revTab[cpbuf[cpoff]]) shl bitsAvail) ;
      inc(cpoff) ;
      bitsAvail := bitsAvail +8 ;
      if bitsAvail < 12 then begin
        if NativeInt(cpoff) >= NativeInt(epoff) then
          bitsAvail := 12
        else begin
          bitACC := bitACC or
                    (LongWord(revTab[cpbuf[cpoff]]) shl bitsAvail);
          inc(cpoff) ;
          bitsAvail := bitsAvail +8 ;
        end ;
      end ;
      tabEntX := bitACC and ((1 shl 12) -1) ;
      bitsAvail := bitsAvail - {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4WhiteTableWidth[tabEntX] ;
      bitACC := bitACC shr {$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFGroup4WhiteTableWidth[tabEntX] ;

      bitsAvail := bitsAvail -1 ;
      bitACC := bitACC shr 1 ;

      pb := TGIS_CardinalArray(tds.RefRuns) ;
      pbx := 0 ;
      b1 := Integer(pb[pbx]) ;
      pbx := pbx + 1 ;

      expandLine1d ;

      inc(tds.CurStripLine) ;
      if tds.CurStripLine = stripline then begin
        line_valid := true ;
        tiffGroup4FillRuns( tds.LineBuffer, thisRun, pa, pax, lastx ) ;
      end ;
    end ;

  end ;

  function  TGIS_FileTIFFDecoder.Group4DecodeLine (  const _linenr : Integer ;
                                                     const _tilenr : Integer = -1
                                                  ) : Integer ;
  var
    tmp        : TGIS_CardinalArray ;
    stripno    : LongWord ;
    stripline  : Integer ;
    line_valid : Boolean ;
    must_preset: Boolean ;
    n          : Integer ;
  begin

    Result := 1 ;

    line_valid := false ;
    tds.MustRead := true ;
    must_preset := true ;
    tmp := nil ;

    if _tilenr = -1 then begin
      stripno := LongWord(_linenr) div tds.RowsPerStrip ;
      stripline := _linenr mod Integer(tds.RowsPerStrip) ;
    end
    else begin
      stripno := _tilenr ;
      stripline := _linenr ;
    end ;

    if tds.StripList[stripno].StripOffsets = tds.StartOffset then begin
      tds.MustRead := false ;
      if tds.CurStripLine <= stripline then begin
        must_preset := false ;
        if tds.CurStripLine = stripline then
          line_valid := true ;
      end ;
    end ;

    if tds.MustRead then begin
      tds.CurStrip := stripno ;
      tds.StartOffset := tds.StripList[stripno].StripOffsets ;
      tds.TiffRawCC   := tds.StripList[stripno].StripByteCounts ;
    end ;

    if must_preset then begin
      tds.CurStripLine := -1 ;
      tds.Bit := 0; // force initial read
      tds.Data := 0;
      tds.RefRuns[0] := tds.RowPixels ;
      for n := 1 to length(tds.RefRuns) - 1 do
        tds.RefRuns[n] := 0 ;

      bitACC := tds.Data ;
      bitsAvail := tds.Bit ;
      tds.Position := Integer(tds.StartOffset) ;
      cpoff := readOffset ;
      cpbuf := tds.ReadBuffer ;
      epbuf := cpbuf ;
      epoff := cpoff +tds.TiffRawCC ;
      lastx := tds.RowPixels ;
    end ;

    while not line_valid do begin
      a0 := 0 ;
      runLength := 0 ;
      thisRun := TGIS_CardinalArray(tds.CurRuns) ;
      pa := TGIS_CardinalArray(thisRun) ;
      pax := 0 ;
      pb := TGIS_CardinalArray(tds.RefRuns) ;
      pbx := 0 ;
      b1 := Integer(pb[pbx]) ;
      pbx := pbx + 1 ;

      if not expandLine2d then begin
        setVal(0);
        tmp := TGIS_CardinalArray(tds.CurRuns) ;
        tds.CurRuns := TGIS_CardinalArray(tds.RefRuns) ;
        tds.RefRuns := TGIS_CardinalArray(tmp) ;
      end ;

      inc(tds.CurStripLine) ;
      if tds.CurStripLine = stripline then begin
        line_valid := true ;
        tiffGroup4FillRuns( tds.LineBuffer, thisRun, pa, pax, lastx ) ;
      end ;
    end ;

  end ;

  procedure TGIS_FileTIFFDecoder.freeTable ;
  var
    q:Integer;
  begin
    {$IFDEF OXYGENE}
      if not assigned( STable ) then exit ;
    {$ENDIF}
    for q:=256 to STableSize-1 do
      if STable[q].Dim>MAXPREALLOC then begin
        STable[q].Data := nil ;
      end ;
    STableSize := 0 ;
  end ;

  function TGIS_FileTIFFDecoder.LZWDecodeLine( const _linenr : Integer ;
                                               const _tilenr : Integer = -1
                                             ) : Integer ;
  var
    i, ss  : Integer ;
    offset : Int64   ;
    loop1  : Integer ;
    red    : Byte    ;
    green  : Byte    ;
    blue   : Byte    ;
    b4     : Byte    ;
    b5     : Byte    ;
    stripno     : LongWord ;
    stripline   : Integer ;
    start_line  : Integer ;
    line_valid  : Boolean ;
    line_pre_valid  : Boolean ;
    line_writed : Boolean ;

    code1: Cardinal;
    old_code : Cardinal ;
    wbytes      : TBytes ;
    k, bidx     : Integer ;
    ival        : Int64 ;
    nval64      : Int64 ;
    pixcount    : Integer ;


  procedure freeTable ;
  var
    q:Integer;
  begin
    {$IFDEF OXYGENE}
      if not assigned( STable ) then exit ;
    {$ENDIF}
    for q:=256 to STableSize-1 do
      if STable[q].Dim>MAXPREALLOC then begin
        STable[q].Data := nil ;
      end ;
    STableSize := 0 ;
  end ;


    procedure initializeTable ;
    var
      i : Integer ;
    begin
//      freeTable ; // free table if allocated
      if not assigned( STable ) then begin
        SetLength( STable, 4114 ) ;
        for i := 0 to 4113 do begin
          {$IFDEF GIS_NORECORDS}
          STable[i] := new TGIS_FileTIFFSItem ;
          {$ENDIF}
          SetLength( STable[i].PreAlloc, MAXPREALLOC ) ;
        end ;
      end ;
      STableSize:=258;
      fDimCode:=9;
    end ;


    // return next code from fComp (based on fNextCode and fDimCode)
    // Note: fDimCode is from 9 to 12
    procedure get_next_code ;
    var
      posb : Integer;
      mr   : Integer ;
    begin
      posb := (fNextCode shr 3);  // position of initial Byte (divide per 8)
      mr := fNextCode mod 8 ;
      if posb <= fCompSize-3  then
        code1 := Cardinal(( fComp[posb] and (Word($FF) shr mr)  shl 16) +
                          (( fComp[posb+1]) shl 8) +
                          (( fComp[posb+2]) shl  0 ))
      else
        code1 := 0 ;

      code1 := code1 shr (24-fDimCode -mr);

      inc(fNextCode, fDimCode);
      offset := posb ;
    end ;

    // CreateString + PutString + DestroyString / optimized
    procedure put_code ;
    begin
      if lzwAborting then exit ;

      if code1 >= Cardinal(STableSize) then begin
        lzwAborting := True ;
        exit ;
      end ;

      if code1 < 256 then begin
        fDecomp[fWPos] := Byte( code1 ) ;
        inc(fWPos) ;
      end
      else begin
        if line_valid or line_pre_valid then
        {$IFDEF OXYGENE}
          {$IFDEF CLR}
            System.Buffer.BlockCopy( STable[code1].Data, 0, fDecomp, fWPos, STable[code1].Dim ) ;
          {$ENDIF}
          {$IFDEF JAVA}
            System.arraycopy(STable[code1].Data, 0, fDecomp, fWPos, STable[code1].Dim);
          {$ENDIF}
        {$ELSE}
          {$IFDEF MSWINDOWS}
            CopyMemory(@(fDecomp[fWPos]),STable[code1].Data,STable[code1].Dim);
          {$ENDIF}
        {$ENDIF}
        inc( fWPos, STable[code1].Dim ) ;
      end ;
    end ;

    // CreateString + PutString + DestroyString / optimized
    // CreateString + PutString + DestroyString / optimized
    procedure put_codeST ;
    var
      st : Integer ;
    begin
      if lzwAborting then exit ;

      st := STableSize -1 ;
        if line_valid or line_pre_valid then
      {$IFDEF OXYGENE}
        {$IFDEF CLR}
          System.Buffer.BlockCopy( STable[st].Data, 0, fDecomp, fWPos, STable[st].Dim ) ;
        {$ENDIF}
        {$IFDEF JAVA}
          System.arraycopy(STable[st].Data, 0, fDecomp, fWPos, STable[st].Dim);
        {$ENDIF}
      {$ELSE}
        {$IFDEF MSWINDOWS}
          CopyMemory(@(fDecomp[fWPos]),STable[st].Data,STable[st].Dim);
        {$ENDIF}
      {$ENDIF}
      inc( fWPos, STable[st].Dim ) ;
    end ;


    // Adds to table OldCode + the first char in Code
    procedure add_concat_to_table ;
    var
      sz : Integer;
    begin
      if lzwAborting then exit;

      if old_code<256 then begin
        sz := 1 ;
        STable[STableSize].Dim := 2 ;
        STable[STableSize].Data := STable[STableSize].PreAlloc ;
        STable[STableSize].Data[0] := Byte(old_code) ;
      end
      else begin
        if old_code >= Cardinal(STableSize) then begin
          lzwAborting:=True;
          exit;
        end ;

        sz := STable[old_code].Dim ;
        STable[STableSize].Dim := sz+1 ;

        if STable[STableSize].Dim>MAXPREALLOC then begin
          SetLength( STable[STableSize].Data, STable[STableSize].Dim ) ;
        end
        else
          STable[STableSize].Data := STable[STableSize].PreAlloc ;
        {$IFDEF OXYGENE}
          {$IFDEF CLR}
            System.Buffer.BlockCopy( STable[old_code].Data, 0, STable[STableSize].Data, 0, sz ) ;
          {$ENDIF}
          {$IFDEF JAVA}
            System.arraycopy( STable[old_code].Data, 0, STable[STableSize].Data, 0, sz );
          {$ENDIF}
        {$ELSE}
          {$IFDEF MSWINDOWS}
            CopyMemory(STable[STableSize].Data,STable[old_code].Data,sz);
          {$ENDIF}
        {$ENDIF}
      end ;

      if code1 < 256 then
        {$IFDEF OXYGENE}
          STable[STableSize].Data[sz] := Byte(code1)
        {$ELSE}
          pbytearray(STable[STableSize].Data)^[sz]:= Byte(code1)
        {$ENDIF}
      else begin
        if code1 >= Cardinal(STableSize) then begin
          lzwAborting := True ;
          exit ;
        end ;
        STable[STableSize].Data[sz] := Byte( STable[code1].Data[0] ) ; // first char
      end ;

      inc( STableSize ) ;
      case STableSize of
        511  : fDimCode := 10 ;
        1023 : fDimCode := 11 ;
        2047 : fDimCode := 12 ;
      end ;

      if STableSize > high(STable) then begin
        lzwAborting := True ;
        exit ;
      end ;
    end ;

    // Adds to table OldCode + the first char in Code
    procedure add_concat_to_table2 ;
    var
      sz : Integer;
    begin
      if lzwAborting then exit;

      if old_code<256 then begin
        sz := 1 ;
        STable[STableSize].Dim := 2 ;
        STable[STableSize].Data := STable[STableSize].PreAlloc ;
        STable[STableSize].Data[0] := Byte(old_code) ;
      end
      else begin
        if old_code >= Cardinal(STableSize) then begin
          lzwAborting:=True;
          exit;
        end ;

        sz := STable[old_code].Dim ;
        STable[STableSize].Dim := sz+1 ;

        if STable[STableSize].Dim>MAXPREALLOC then begin
          SetLength( STable[STableSize].Data, STable[STableSize].Dim ) ;
        end
        else
          STable[STableSize].Data := STable[STableSize].PreAlloc ;
        {$IFDEF OXYGENE}
          {$IFDEF CLR}
            System.Buffer.BlockCopy( STable[old_code].Data, 0, STable[STableSize].Data, 0, sz ) ;
          {$ENDIF}
          {$IFDEF JAVA}
            System.arraycopy( STable[old_code].Data, 0, STable[STableSize].Data, 0, sz );
          {$ENDIF}
        {$ELSE}
          {$IFDEF MSWINDOWS}
            CopyMemory(STable[STableSize].Data,STable[old_code].Data,sz);
          {$ENDIF}
        {$ENDIF}
      end ;

      if old_code < 256 then
        {$IFDEF OXYGENE}
          STable[STableSize].Data[sz] := Byte(old_code)
        {$ELSE}
          pbytearray(STable[STableSize].Data)^[sz]:= Byte(old_code)
        {$ENDIF}
      else begin
        if old_code >= Cardinal(STableSize) then begin
          lzwAborting := True ;
          exit ;
        end ;
        STable[STableSize].Data[sz] := Byte( STable[old_code].Data[0] ) ; // first char
      end ;

      inc( STableSize ) ;
      case STableSize of
        511  : fDimCode := 10 ;
        1023 : fDimCode := 11 ;
        2047 : fDimCode := 12 ;
      end ;

      if STableSize > high(STable) then begin
        lzwAborting := True ;
        exit ;
      end ;
    end ;


  begin

    line_valid := false ;
    line_pre_valid := false ;
    tds.MustRead := true ;
    Result := fLineSize ;

    if _tilenr = -1 then begin
      stripno := LongWord(_linenr) div tds.RowsPerStrip ;
      stripline := _linenr mod Integer(tds.RowsPerStrip) ;
    end
    else begin
      stripno := _tilenr ;
      stripline := _linenr ;
    end ;

    if tds.CurStrip = stripno then begin
      if tds.CurStripLine = stripline then
        exit ;
      offset := tds.StripList[stripno].StripOffsets ;
      tds.StartOffset := offset ;
      if _tilenr <> -1 then begin
        start_line := 0 ;
      end
      else
        start_line := stripno * tds.RowsPerStrip ;
      ss := (start_line +tds.CurStripLine +1) ;
      if _linenr >= ss then
        start_line := ss ;
    end
    else begin
      offset := tds.StripList[stripno].StripOffsets ;
      tds.StartOffset := offset ;
      if _tilenr <> -1 then
        start_line := 0
      else
        start_line := stripno * tds.RowsPerStrip ;
    end ;

    tds.TiffRawCC := tds.StripList[stripno].StripByteCounts ;

    if (start_line = _linenr) then
      line_valid := true
    else
    if (start_line = _linenr -1) then
      line_pre_valid := true ;

    old_code := lzwOldCode ;
    if (Word(start_line) mod tds.RowsPerStrip) = 0 then begin
      tds.Position := offset ;
      tds.TiffRawCBOff := readOffset ;
      tds.TiffRawCB := tds.ReadBuffer ;
      fNextCode := 0;
      initializeTable ;
      old_code := NoLZWCode ;
      fWPos := 0 ;
    end ;

    lzwAborting := False ;

    line_writed := false ;
    fComp := tds.TiffRawCB ;
    fCompSize := length( fComp ) ;
    fDecomp := TBytes(tds.LineBuffer) ;

    while not line_writed do begin

      if fWPos > fLineSize then begin
        // copy the rest of previous row
        for i := 0 to fWPos-fLineSize-1 do
          fDecomp[i] := fDecomp[fLineSize+i] ;
        fWPos:=fWPos-fLineSize;
      end
      else begin
        fWPos:=0;
      end ;

      while fWPos < fLineSize do begin
        get_next_code ; //code1
        if old_code = NoLZWCode then
          old_code := code1 ;

        if code1 = CLEARCODE then begin
          initializeTable ;
          get_next_code ; //code1
          if code1=EOICODE then
            break ;
          put_code ;
          old_code := code1 ;
        end
        else
          if code1 = EOICODE then
            break
          else begin
            if code1 < Cardinal(STableSize) then begin
              put_code ;
              add_concat_to_table ;
              old_code := code1 ;
            end
            else begin
              add_concat_to_table2 ;
              put_codeST ;
              old_code := code1 ;
           end ;
        end ;
        if lzwAborting then begin
          result := 0;
          exit;
        end ;
      end ;

      inc(start_line) ;
      if line_valid then
        line_writed := true
      else begin
        if start_line = _linenr then
          line_valid := true
        else
        if (start_line = _linenr -1) then
          line_pre_valid := true ;
      end ;
    end ;

    lzwOldCode := old_code ;
    tds.CurStripLine := stripline ;
    tds.CurStrip := stripno ;

    if tds.RowBytes = 3*tds.RowPixels then begin //24 bits per pixel
      offset := 0 ;
      if tds.HDifferencing then begin //horizontal differencing
        loop1 := tds.RowPixels  ;
        offset := 0 ;

        for i := 1 to loop1 -1 do begin
          red   := tds.LineBuffer[offset  ] ;
          green := tds.LineBuffer[offset+1] ;
          blue  := tds.LineBuffer[offset+2] ;

          offset := offset +3 ;

          tds.LineBuffer[offset  ] :=
              Byte(Integer(tds.LineBuffer[offset  ]) + red) ;

          tds.LineBuffer[offset+1] :=
              Byte(Integer(tds.LineBuffer[offset+1]) + green) ;

          tds.LineBuffer[offset+2] :=
              Byte(Integer(tds.LineBuffer[offset+2]) + blue) ;
        end ;
      end ;
    end
    else if tds.RowBytes = 2*tds.RowPixels then begin //16 bits per pixel
      if tds.HDifferencing then begin //horizontal differencing
        loop1 := tds.RowPixels  ;
        offset := 0 ;

        for i := 1 to loop1 -1 do begin
          red   := tds.LineBuffer[offset  ] ;
          green := tds.LineBuffer[offset+1] ;

          offset := offset +2 ;

          tds.LineBuffer[offset  ] :=
              Byte(Integer(tds.LineBuffer[offset  ]) + red) ;

          tds.LineBuffer[offset+1] :=
              Byte(Integer(tds.LineBuffer[offset+1]) + green) ;
        end ;
      end ;
    end
    else
    if tds.RowBytes = 4*tds.RowPixels then begin //32 bits per pixel
      offset := 0 ;

      if tds.HDifferencing then begin //horizontal differencing
        if tds.BandsNo = 1 then begin
          pixcount := tds.RowPixels ;
          nval64 := 0 ;
          bidx := 0 ;
          for k := 0 to pixcount - 1 do begin
            ival := tds.LineBuffer[bidx +3] ;
            for i := 0 to 2 do
              ival := (ival shl 8) or tds.LineBuffer[bidx +2 -i] ;
            nval64 := nval64 +ival ;
            ival := nval64 ;
            for i := 0 to 3 do begin
              tds.LineBuffer[bidx +i] := Byte(ival) ;
              ival := ival shr 8 ;
            end;
            inc(bidx, 4) ;
          end ;
          exit ;
        end ;


        loop1 := tds.RowPixels  ;
        offset := 0 ;

        for i := 1 to loop1 -1 do begin
          red   := tds.LineBuffer[offset  ] ;
          green := tds.LineBuffer[offset+1] ;
          blue  := tds.LineBuffer[offset+2] ;
          b4    := tds.LineBuffer[offset+3] ;

          offset := offset +4 ;

          tds.LineBuffer[offset  ] :=
              Byte(Integer(tds.LineBuffer[offset  ]) + red) ;

          tds.LineBuffer[offset+1] :=
              Byte(Integer(tds.LineBuffer[offset+1]) + green) ;

          tds.LineBuffer[offset+2] :=
              Byte(Integer(tds.LineBuffer[offset+2]) + blue) ;

          tds.LineBuffer[offset+3] :=
              Byte(Integer(tds.LineBuffer[offset+3]) + b4) ;
        end ;
      end
      else if tds.HFDifferencing then begin //float horizontal differencing
        if tds.BandsNo = 1 then begin
          loop1 := tds.RowPixels ;
          SetLength(wbytes, tds.RowBytes) ;
          wbytes[0] := tds.LineBuffer[0] ;
          for k := 1 to loop1*4 - 1 do begin
            wbytes[k] := Byte(Integer(tds.LineBuffer[k])  +wbytes[k -1]) ;
          end;
          for k := 0 to loop1 - 1 do begin
            for i := 0 to 3 do
              tds.LineBuffer[4*k +i] := wbytes[(4 -i -1)*loop1 +k ] ;
          end ;
          SetLength(wbytes, 0) ;
          exit ;
        end ;
      end;
    end
    else
    if tds.RowBytes = 5*tds.RowPixels then begin //40 bits per pixel
      offset := 0 ;

      if tds.HDifferencing then begin //horizontal differencing
        loop1 := tds.RowPixels  ;
        offset := 0 ;

        for i := 1 to loop1 -1 do begin
          red   := tds.LineBuffer[offset  ] ;
          green := tds.LineBuffer[offset+1] ;
          blue  := tds.LineBuffer[offset+2] ;
          b4    := tds.LineBuffer[offset+3] ;
          b5    := tds.LineBuffer[offset+4] ;
          offset := offset +5 ;

          tds.LineBuffer[offset  ] :=
              Byte(Integer(tds.LineBuffer[offset  ]) + red) ;

          tds.LineBuffer[offset+1] :=
              Byte(Integer(tds.LineBuffer[offset+1]) + green) ;

          tds.LineBuffer[offset+2] :=
              Byte(Integer(tds.LineBuffer[offset+2]) + blue) ;

          tds.LineBuffer[offset+3] :=
              Byte(Integer(tds.LineBuffer[offset+3]) + b4) ;
          tds.LineBuffer[offset+4] :=
              Byte(Integer(tds.LineBuffer[offset+4]) + b5) ;
        end ;
      end ;
    end
    else begin
      if tds.HDifferencing then begin //horizontal differencing
        loop1 := tds.RowBytes  ;
        offset := 0 ;

        for i := 1 to loop1 -1 do begin
          red   := tds.LineBuffer[offset  ] ;

          offset := offset +1 ;

          tds.LineBuffer[offset  ] :=
              Byte(Integer(tds.LineBuffer[offset  ]) + red) ;
        end ;
      end ;
    end ;

  end ;

  function TGIS_FileTIFFDecoder.PackBitsDecodeLine( const _linenr : Integer ;
                                                    const _tilenr : Integer = -1
                                                  ) : Integer ;
  var
    i, n,
    ss,
    b      : Integer ;
    bpbuf  : TBytes   ;
    bpoff  : Integer  ;
    buf    : TBytes   ;
    bufx   : Integer  ;
    bytes  : LongWord ;
    offset : Cardinal  ;
    stripno     : LongWord ;
    stripline   : Integer ;
    start_line  : Integer ;
    line_valid  : Boolean ;
    line_writed : Boolean ;
  begin

    Result := 1 ;
    line_valid := false ;
    tds.MustRead := true ;
    buf := TBytes(tds.LineBuffer) ;
    bufx := 0 ;

    if _tilenr = -1 then begin
      stripno := LongWord(_linenr) div tds.RowsPerStrip ;
      stripline := _linenr mod Integer(tds.RowsPerStrip) ;
    end
    else begin
      stripno := _tilenr ;
      stripline := _linenr ;
    end ;

    if tds.CurStrip = stripno then begin
      if tds.CurStripLine = stripline then
        exit ;

      if tds.PackLineTable[_linenr].LineOffset <> 0 then
        tds.MustRead := false ;
    end ;

    tds.TiffRawCC := tds.StripList[stripno].StripByteCounts ;

    if (tds.PackLineTable[_linenr].LineOffset = 0) or
       (tds.PackLineTable[_linenr].StripNo <> stripno)  then
    begin
      offset := tds.StripList[stripno].StripOffsets ;
      tds.StartOffset := offset ;
      if _tilenr <> -1 then
        start_line := 0
      else
        start_line := stripno * tds.RowsPerStrip ;
      ss := (start_line +tds.CurStripLine +1) ;
      if (tds.CurStripLine >= 0) and (tds.CurStrip = stripno) and
         (tds.PackLineTable[ss].LineOffset <> 0)
      then begin
        if tds.CurStripLine <= _linenr then begin
          start_line := ss ;
          offset := tds.PackLineTable[start_line].LineOffset ;
        end;
      end ;

      if start_line = _linenr then
        line_valid := true ;

      tds.PackLineTable[start_line].LineOffset := offset ;
      tds.PackLineTable[start_line].StripNo :=  stripno;
      tds.Position := offset ;
      tds.TiffRawCBOff := readOffset ;
      tds.TiffRawCB := tds.ReadBuffer ;
    end
    else begin
      start_line := _linenr ;
      line_valid := true ;
      offset := tds.PackLineTable[_linenr].LineOffset ;
      tds.StartOffset := tds.StripList[stripno].StripOffsets ;
      tds.Position := offset ;
      tds.TiffRawCBOff := readOffset ;
      tds.TiffRawCB := tds.ReadBuffer ;
    end ;

    bpbuf := tds.TiffRawCB ;
    bpoff := tds.TiffRawCBOff ;

    bytes := 0 ;
    line_writed := false ;

    while not line_writed do begin

      n  := bpbuf[bpoff];
      inc(bpoff) ;
      inc(offset) ;
      if n >= 128 then n := n -256 ;

      if n < 0 then begin
        if n = -128 then continue ;

        n := 1 -n ;
        bytes := bytes +LongWord(n) ;
        b := bpbuf[bpoff] ;
      inc(bpoff) ;
        inc(offset) ;
        if not line_valid then begin
          if (bytes > tds.RowBytes) then begin
            if (start_line +1) = _linenr then begin
              line_valid := true ;
            end ;
            n := bytes - tds.RowBytes ;
            bytes := n ;
            inc(start_line) ;
          end ;
        end
        else
        if (bytes > tds.RowBytes) then
          n := n -(Integer(bytes) - Integer(tds.RowBytes)) ;
        if line_valid then begin
          for i := 0 to n-1 do
            buf[bufx+i] := Byte(b) and $FF ;
          bufx := bufx + n ;
        end ;
      end
      else begin
        inc(n) ;
        bytes := bytes + LongWord(n) ;

        if not line_valid then begin
          if (bytes > tds.RowBytes) then begin
            if (start_line +1) = _linenr then begin
              line_valid := true ;
            end ;
            n := bytes - tds.RowBytes ;
            bytes := n ;
            inc(start_line) ;
          end ;
        end else
        if (bytes > tds.RowBytes) then
          n := n -(Integer(bytes) - Integer(tds.RowBytes)) ;

        if line_valid then begin
          GisCopyMemory(bpbuf, bpoff, buf, bufx, n) ;
          bufx := bufx + n ;
        end ;
        bpoff := bpoff +n ;
        offset := offset +Cardinal(n) ; ;
      end ;

      if bytes = tds.RowBytes then begin
        inc(start_line) ;
        tds.PackLineTable[start_line].LineOffset := offset ;
        tds.PackLineTable[start_line].StripNo :=  stripno;

        if line_valid then
          line_writed := true
        else begin
          bytes := 0 ;
          if start_line = _linenr then
            line_valid := true ;
        end ;
      end
      else
      if bytes > tds.RowBytes then begin
        line_writed := true ;
      end ;
    end ;

    tds.CurStripLine := stripline ;
    tds.CurStrip := stripno ;

  end ;

  function TGIS_FileTIFFDecoder.ZLIBDecodeLine( const _linenr : Integer ;
                                                    const _tilenr : Integer = -1
                                                  ) : Integer ;
  var
    stripno     : LongWord ;
    stripline   : Integer ;
    dval        : Array [0..3] of SmallInt ;
    ival        : Int64   ;
    pixcount, i : Integer ;
    k, bidx     : Integer ;
    nval        : Integer ;
    nval64      : Int64   ;
    offset      : Cardinal ;
    loop1       : Integer ;
    red         : Byte    ;
    green       : Byte    ;
    blue        : Byte    ;
    b4          : Byte    ;
    wbytes      : TBytes  ;
  begin
    Result := 1 ;
    tds.MustRead := true ;

    if _tilenr = -1 then begin
      stripno := LongWord(_linenr) div tds.RowsPerStrip ;
      stripline := _linenr mod Integer(tds.RowsPerStrip) ;
    end
    else begin
      stripno := _tilenr ;
      stripline := _linenr ;
    end ;

    if tds.CurStrip = stripno then begin
      if tds.CurStripLine = stripline then
        exit ;
      if tds.CurStripLine <> - 1 then
        if tds.CurStripLine < stripline then
          tds.MustRead := false ;
    end ;

    if tds.MustRead then begin
      zlibStream.Seek(0, soBeginning) ;

      if assigned(zDecompStream) then
        FreeObject(zDecompStream) ;

      tds.TiffStream.Position := tds.StripList[stripno].StripOffsets ;

      zlibStream.Position := 0 ;
      zlibStream.CopyFrom(tds.TiffStream, tds.StripList[stripno].StripByteCounts) ;

      zlibStream.Seek(0, soBeginning) ;
      zDecompStream :=  TZDecompressionStream.Create(zlibStream);
      zDecompStream.Position := 0 ;
      tds.CurStripLine := -1 ;
      tds.CurStrip := stripno ;
    end
    else begin
      if tds.CurStripLine > stripline then begin
        zDecompStream.Position := 0 ;
        tds.CurStripLine := -1 ;
      end ;
    end ;

    repeat
      {$IFDEF OXYGENE}
        zDecompStream.Read(tds.LineBuffer, tds.RowBytes) ;
      {$ELSE}
      try
        zDecompStream.Read(tds.LineBuffer[0], tds.RowBytes)  ;
      except
        zDecompStream.Read(tds.LineBuffer[0], tds.RowBytes)  ;
      end ;
      {$ENDIF}
      inc(tds.CurStripLine) ;
    until tds.CurStripLine = stripline ;

    if tds.HDifferencing then begin
      if tds.RowBytes = 4*tds.RowPixels then begin //32 bits per pixel
        if tds.BandsNo = 1 then begin
          pixcount := tds.RowPixels ;
          nval64 := 0 ;
          bidx := 0 ;
          for k := 0 to pixcount - 1 do begin
            ival := tds.LineBuffer[bidx +3] ;
            for i := 0 to 2 do
              ival := (ival shl 8) or tds.LineBuffer[bidx +2 -i] ;
            nval64 := nval64 +ival ;
            ival := nval64 ;
            for i := 0 to 3 do begin
              tds.LineBuffer[bidx +i] := Byte(ival) ;
              ival := ival shr 8 ;
            end;
            inc(bidx, 4) ;
          end ;
          exit ;
        end ;

        loop1 := tds.RowPixels  ;
        offset := 0 ;
        for i := 1 to loop1 -1 do begin
          red   := tds.LineBuffer[offset  ] ;
          green := tds.LineBuffer[offset+1] ;
          blue  := tds.LineBuffer[offset+2] ;
          b4    := tds.LineBuffer[offset+3] ;

          offset := offset +4 ;

          tds.LineBuffer[offset  ] :=
              Byte(Integer(tds.LineBuffer[offset  ]) + red) ;

          tds.LineBuffer[offset+1] :=
              Byte(Integer(tds.LineBuffer[offset+1]) + green) ;

          tds.LineBuffer[offset+2] :=
              Byte(Integer(tds.LineBuffer[offset+2]) + blue) ;
          tds.LineBuffer[offset+3] :=
              Byte(Integer(tds.LineBuffer[offset+3]) + b4) ;
        end ;
      end
      else
      if tds.RowBytes = 2*tds.RowPixels then begin //16 bits per pixel
        if tds.BandsNo = 1 then begin
          pixcount := tds.RowPixels ;
          nval64 := 0 ;
          bidx := 0 ;
          for k := 0 to pixcount - 1 do begin
            ival := tds.LineBuffer[bidx +1] ;
            ival := (ival shl 8) or tds.LineBuffer[bidx] ;
            nval64 := nval64 +ival ;
            tds.LineBuffer[bidx] := Byte(nval64) ;
            tds.LineBuffer[bidx +1] := Byte(nval64 shr 8) ;
            inc(bidx, 2) ;
          end ;
          exit ;
        end ;
      end
      else
      if tds.RowBytes = 3*tds.RowPixels then begin //24 bits per pixel
        offset := 0 ;
        loop1 := tds.RowPixels  ;


        for i := 1 to loop1 -1 do begin
          red   := tds.LineBuffer[offset +0] ;
          green := tds.LineBuffer[offset +1] ;
          blue  := tds.LineBuffer[offset +2] ;
          offset := offset +3 ;

          tds.LineBuffer[offset  ] :=
              Byte(Integer(tds.LineBuffer[offset  ]) + red) ;

          tds.LineBuffer[offset+1] :=
              Byte(Integer(tds.LineBuffer[offset+1]) + green) ;

          tds.LineBuffer[offset+2] :=
              Byte(Integer(tds.LineBuffer[offset+2]) + blue) ;

        end ;
      end
      else
      if tds.RowBytes = tds.RowPixels then begin //8 bits per pixel
        offset := 0 ;
        loop1 := tds.RowPixels  ;

        red := tds.LineBuffer[0] ;
        for i := 1 to loop1 -1 do begin
          red := Byte(red +Integer(tds.LineBuffer[i])) ;
          tds.LineBuffer[i] := red ;
        end ;
      end
      else
      if tds.RowBytes = 8*tds.RowPixels then begin //64 bits per pixel
        pixcount := tds.RowPixels ;
        dval[0] := 0 ;
        dval[1] := 0 ;
        dval[2] := 0 ;
        dval[3] := 0 ;
        bidx := 0 ;
        for k := 0 to pixcount - 1 do begin
          for i := 0 to 3 do begin
            nval := (Integer(Word(tds.LineBuffer[bidx+1])) shl 8) ;
            nval :=  nval or tds.LineBuffer[bidx] ;
            nval := nval +dval[i] ;
            dval[i] := SmallInt(nval) ;
            tds.LineBuffer[bidx+1] := Byte(nval shr 8  ) ;
            tds.LineBuffer[bidx]   := Byte(nval and $FF) ;
            inc(bidx, 2) ;
          end ;
        end ;
      end
      else
      begin //and 48bpp
        pixcount := tds.RowPixels ;
        dval[0] := 0 ;
        dval[1] := 0 ;
        dval[2] := 0 ;
        bidx := 0 ;
        for k := 0 to pixcount - 1 do begin
          for i := 0 to 2 do begin
            nval := (Integer(Word(tds.LineBuffer[bidx+1])) shl 8) ;
            nval :=  nval or tds.LineBuffer[bidx] ;
            nval := nval +dval[i] ;
            dval[i] := SmallInt(nval) ;
            tds.LineBuffer[bidx+1] := Byte(nval shr 8  ) ;
            tds.LineBuffer[bidx]   := Byte(nval and $FF) ;
            inc(bidx, 2) ;
          end ;
        end ;
      end ;
    end
    else
    if tds.HFDifferencing then begin // Floating Point
      if tds.RowBytes = 4*tds.RowPixels then begin //32 bits per pixel
        if tds.BandsNo = 1 then begin
          pixcount := tds.RowPixels ;
          SetLength(wbytes, tds.RowBytes) ;
          wbytes[0] := tds.LineBuffer[0] ;
          for k := 1 to pixcount*4 - 1 do begin
            wbytes[k] := Byte(Integer(tds.LineBuffer[k])  +wbytes[k -1]) ;
          end;
          for k := 0 to pixcount - 1 do begin
            for i := 0 to 3 do
              tds.LineBuffer[4*k +i] := wbytes[(4 -i -1)*pixcount +k ] ;
          end ;
          SetLength(wbytes, 0) ;
        end ;
      end;
    end;
  end ;

  function    TGIS_FileTIFFDecoder.JpegScale (  const _scale  : Integer )
                                             : Integer ;
  begin
    if jpegDecoder.Scale <> _scale then
      jpegDecoder.Scale := _scale ;
    Result := jpegDecoder.Scale ;
    jpegZoom := Result ;
  end ;

  procedure   TGIS_FileTIFFDecoder.ComponentsJpeg (  const _componentsJpeg  : Integer ) ;
  begin
    jpegDecoder.ComponentsJpeg := _componentsJpeg ;
  end ;

 procedure   TGIS_FileTIFFDecoder.NativeRequested      (  const _nativerq : Boolean ) ;
  begin
    jpegDecoder.NativeRequested := _nativerq ;
  end ;


  function TGIS_FileTIFFDecoder.JpegDecodeLine( const _linenr : Integer ;
                                                const _tilenr : Integer = -1
                                               ) : Integer ;
  var
    loop1     : Integer ;
    stripno   : LongWord ;
    stripline : Integer ;
    tblen     : Integer ;
    scale     : Integer ;
    res       : Integer ;
  begin
    Result := 1 ;
    tds.MustRead := true ;

    if _tilenr = -1 then begin
      stripno := LongWord(_linenr) div tds.RowsPerStrip ;
      stripline := _linenr mod Integer(tds.RowsPerStrip) ;
    end
    else begin
      stripno := _tilenr ;
      stripline := _linenr ;
    end ;

    if tds.CurStrip = stripno then begin
      if tds.CurStripLine = stripline then
        exit ;
      if tds.CurStripLine <> - 1 then
        tds.MustRead := false ;
    end ;

    if tds.MustRead then begin
      if assigned(tds.jpegTables) then begin
        tblen := length(tds.jpegTables)  ;

        jpgStream.Seek(tblen -2, soBeginning) ;
        if tds.StripList[stripno].StripByteCounts = 0 then
          tds.StripList[stripno].StripByteCounts := 512*512*4 ;
        tds.TiffStream.Position := tds.StripList[stripno].StripOffsets +2 ;
        jpgStream.CopyFrom(tds.TiffStream, tds.StripList[stripno].StripByteCounts -2) ;
      end
      else begin
        jpgStream.Seek(0, soBeginning) ;
        tds.TiffStream.Position := tds.StripList[stripno].StripOffsets ;
        jpgStream.CopyFrom(tds.TiffStream, tds.StripList[stripno].StripByteCounts) ;
      end ;
      jpgStream.Seek(0, soBeginning) ;

      jpegDecoder.LoadFromStream(jpgStream);
      if jpegDecoder.JpegInitialized and assigned(tds.jpegTables) and
         (_tilenr = Integer(tds.CurStrip) )
      then begin
        jpegDecoder.Reinitialize(TGIS_LayerDormantMode.Standard ) ;
      end
      else begin
        res := jpegDecoder.Initialize( TGIS_LayerDormantMode.Standard ) ;
        if (res <> 0) and (res <> 83) then
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPIXELFORMAT ), '', res) ;
      end ;
      jpegDecoder.Scale := jpegZoom ;
    end ;

    tds.CurStripLine := stripline ;
    tds.CurStrip := stripno ;
    if tds.RowBytes = tds.RowPixels then
      loop1 := tds.RowBytes
    else
    if tds.RowBytes = 4 * tds.RowPixels then
      loop1 := tds.RowBytes shr 2
    else
    if tds.RowBytes = 3 * tds.RowPixels then begin
      if tds.NativeRequired then
        loop1 := 2 * tds.RowBytes
      else
        loop1 := tds.RowBytes div 3 ;
    end
    else
      loop1 := tds.RowBytes ;
    scale := jpegDecoder.Scale ;
    if scale > 1 then
      loop1 := (loop1 +scale -1) div scale ;

    jpegDecoder.DecodeLine(tds.LineBuffer, 0, stripline, 0, loop1) ;
  end ;

//=============================================================================
// TGIS_FileTIFF
//=============================================================================

  function TGIS_FileTIFF.compressLZW( _indata   : TBytes  ;
                                      _inStart  : Integer ;
                                      _inputlen : Integer ;
                                      _op       : Integer
                                    ) : Integer ;
  var
    res : Integer ;

    function next_pixel : Integer;
    begin
      if( lzwr.countDown = 0 ) then begin
        Result := XEOF ;
        exit ;
      end ;
      dec(lzwr.countDown) ;
      Result := lzwr.inData[lzwr.inPos] ;
      inc( lzwr.inPos ) ;
    end ;

    procedure cl_hash(_hsize : Integer) ;
    var
      htab_p : Integer ;
      i,m1:Integer;
    begin
      htab_p := 0 ;
      inc(htab_p, _hsize);
      m1 := -1;
      i := _hsize - 16;
      repeat
        lzwr.hTab[htab_p-16] := m1;
        lzwr.hTab[htab_p-15] := m1;
        lzwr.hTab[htab_p-14] := m1;
        lzwr.hTab[htab_p-13] := m1;
        lzwr.hTab[htab_p-12] := m1;
        lzwr.hTab[htab_p-11] := m1;
        lzwr.hTab[htab_p-10] := m1;
        lzwr.hTab[htab_p- 9] := m1;
        lzwr.hTab[htab_p- 8] := m1;
        lzwr.hTab[htab_p- 7] := m1;
        lzwr.hTab[htab_p- 6] := m1;
        lzwr.hTab[htab_p- 5] := m1;
        lzwr.hTab[htab_p- 4] := m1;
        lzwr.hTab[htab_p- 3] := m1;
        lzwr.hTab[htab_p- 2] := m1;
        lzwr.hTab[htab_p- 1] := m1;
        dec(htab_p,16);
        dec(i,16);
      until not (i >= 0);
      inc(i,16);
      while i>0 do begin
        dec(htab_p);
        lzwr.hTab[htab_p] := m1;
       dec(i);
      end ;
    end ;

    procedure flush_char;
    var
      i : Integer ;
    begin
        if( lzwr.aCount > 0 ) then begin
            for i := 0 to lzwr.aCount -1 do begin
              lzwr.outBuff[res] := lzwr.accum[i] ;
              inc(res) ;
            end ;
            lzwr.aCount := 0;
        end ;
    end ;

    procedure char_out( _c : Integer );
    begin
      lzwr.accum[ lzwr.aCount ] := Byte(_c) ;
      inc( lzwr.aCount ) ;
      if ( lzwr.aCount >= 254 ) then
        flush_char ;
    end ;

    procedure do_output(_code : Integer );
    const
      masks:array [0..16] of Integer= ( $0000, $8000, $C000, $E000, $F000,
                                        $F800, $FC00, $FE00, $FF00, $FF80,
                                        $FFC0, $FFE0, $FFF0, $FFF8, $FFFC,
                                        $FFFE, $FFFF );
    begin
      lzwr.currAccum := lzwr.currAccum and
                        (((1 shl lzwr.currBits)-1) shl (32-lzwr.currBits)) ;
      if( lzwr.currBits > 0 ) then
        lzwr.currAccum := lzwr.currAccum or
                            Cardinal(_code shl (32-lzwr.nBits-lzwr.currBits))
      else
        lzwr.currAccum := Cardinal(_code shl Cardinal(32-lzwr.nBits)) ;

      inc(lzwr.currBits, lzwr.nBits) ;
      while( lzwr.currBits >= 8 ) do begin
        char_out( Cardinal(lzwr.currAccum and $ff000000) shr 24 );
        lzwr.currAccum := lzwr.currAccum shl 8;
        dec( lzwr.currBits, 8 ) ;
      end ;
      if ( lzwr.freeEnt > lzwr.maxCode-1) or (lzwr.clearFlag<>0 ) then begin
        if ( lzwr.clearFlag <> 0 ) then begin
          lzwr.nBits := lzwr.gInitBits ;
          lzwr.maxCode := 1 shl lzwr.nBits -1 ;
          lzwr.clearFlag := 0 ;
        end
        else begin
          inc( lzwr.nBits ) ;
          if ( lzwr.nBits = MAXBITS ) then
            lzwr.maxCode := MAXMAXCODE
          else
            lzwr.maxCode := 1 shl lzwr.nBits - 1 ;
        end ;
      end ;
      if( _code = lzwr.eofCode ) then begin
        while( lzwr.currBits > 0 ) do begin
          char_out( Cardinal( lzwr.currAccum and $ff000000 ) shr 24 ) ;
          lzwr.currAccum := lzwr.currAccum shl 8;
          dec( lzwr.currBits, 8 ) ;
        end ;
        flush_char ;
      end ;
    end ;

    procedure cl_block ;
    begin
      cl_hash ( Integer(HSIZE) );
      lzwr.freeEnt := lzwr.clearCode + 2;
      lzwr.clearFlag := 1;
      do_output( lzwr.clearCode ) ;
    end ;

    procedure lzw_compress( _op : Integer );
    begin
      if _op=0 then begin
        // initialize
        lzwr.gInitBits := lzwr.initBits;
        lzwr.clearFlag := 0;
        lzwr.nBits := lzwr.initBits;
        lzwr.maxCode := 1 shl lzwr.nBits -1;
        lzwr.clearCode := (1 shl (lzwr.initBits - 1));
        lzwr.eofCode := lzwr.clearCode + 1;
        lzwr.freeEnt := lzwr.clearCode + 2;
        lzwr.aCount := 0;
        lzwr.ent := next_pixel ;
        lzwr.hShift := 0;
        lzwr.fCode := HSIZE ;
         while lzwr.fCode < 65536 do begin
          inc( lzwr.hShift ) ;
          lzwr.fCode := lzwr.fCode*2;
        end ;
        lzwr.hShift := 8 - lzwr.hShift;
        lzwr.hSizeReg := HSIZE ;
        cl_hash( lzwr.hSizeReg ) ;
        do_output( lzwr.clearCode ) ;
      end ;
      if (_op=0) or (_op=1) then begin
        // encoding
        while(true) do begin
          lzwr.c := next_pixel ;
          if lzwr.c = XEOF then
            break;
          lzwr.fCode := Integer( ((Integer(lzwr.c) shl MAXBITS) + lzwr.ent));
          lzwr.i := ((Integer(lzwr.c) shl lzwr.hShift) xor lzwr.ent);

          if ( lzwr.hTab[lzwr.i] = lzwr.fCode ) then begin
            lzwr.ent := lzwr.codeTab[lzwr.i];
            continue;
          end  ;

          if ( Integer(lzwr.hTab[lzwr.i]) >= 0 ) then begin
            lzwr.disp := lzwr.hSizeReg - lzwr.i;
            if ( lzwr.i = 0 ) then
              lzwr.disp := 1;
            repeat
              dec( lzwr.i, lzwr.disp ) ;
              if ( lzwr.i < 0 ) then
                inc( lzwr.i, lzwr.hSizeReg ) ;

              if ( lzwr.hTab[lzwr.i] = lzwr.fCode ) then
                break ;
            until ( Integer(lzwr.hTab[lzwr.i]) <= 0 ) ;

            if ( lzwr.hTab[lzwr.i] = lzwr.fCode ) then begin
              lzwr.ent := lzwr.codeTab[lzwr.i];
              continue;
            end ;
          end ;
          do_output ( Integer(lzwr.ent) );
          lzwr.ent := lzwr.c;
          if ( lzwr.freeEnt < MAXMAXCODE-1 ) then begin
            lzwr.codeTab[lzwr.i] := lzwr.freeEnt;
            inc(lzwr.freeEnt);
            lzwr.hTab[lzwr.i]:= lzwr.fCode;
          end
          else
            cl_block ;

        end ;
      end
      else if _op=2 then begin
        // finish
        do_output( lzwr.ent ) ;
        do_output( lzwr.eofCode ) ;
      end ;
    end ;

  begin
    res    := 0 ;
    try
      case _op of
        0 :
          begin
          // initialize/encode
            lzwr.inData := _indata ;
            if not assigned(lzwr.outBuff) then
              SetLength( lzwr.outBuff, 15*_inputlen ) ;
            lzwr.currAccum := 0 ;
            lzwr.currBits := 0 ;
            lzwr.countDown := _inputlen ;
            lzwr.freeEnt :=0 ;
            lzwr.inPos := _inStart ;
            lzwr.initBits := 8+1 ;
            lzw_compress(0) ;
          end ;
        2 :
          // finish
          lzw_compress(_op) ;
        else begin
          // continue encoding
          lzwr.countDown := _inputlen ;
          lzwr.inData := _indata ;
          lzwr.inPos := _inStart ;
          lzw_compress(1) ;
        end ;
      end ;
    finally
      Result := res ;
    end ;
  end ;

  procedure TGIS_FileTIFF.WriteGrid(
    const _x   : Integer ;
    const _y   : Integer ;
    const _grd : TGIS_GridArray
  ) ;
  var
    row       : Integer ;
    inptr   : TBytes  ;
    grdwidth  : Integer ;
    grdheight : Integer ;
    ridx  : Integer ;


    procedure convert32single_to_32bits ;
    var
      ii, kk, ss : Integer ;
      {$IFDEF OXYGENE}
      {$ELSE}
        pixptr : PInteger ;
      {$ENDIF}
    begin
      for ii := 0 to grdheight - 1 do begin
        ss := 4*grdwidth*ii ;
        for kk := 0 to grdwidth - 1 do begin
          {$IFDEF OXYGENE}
             GisCopyMemory(  BitConverter.GetBytes(_grd[ii][kk]),
                               0, convBytes, ss +4*kk, 4 ) ;
          {$ELSE}
            pixptr := PInteger(@_grd[ii][kk]) ;
            convBytes[ss +4*kk +0] := PByte(pixptr)^ ;
            convBytes[ss +4*kk +1] := PByte(IntPtr(pixptr) +1)^ ;
            convBytes[ss +4*kk +2] := PByte(IntPtr(pixptr) +2)^ ;
            convBytes[ss +4*kk +3] := PByte(IntPtr(pixptr) +3)^ ;
          {$ENDIF}
        end ;
      end ;
    end ;


  begin
    grdheight := length(_grd) ;
    grdwidth  := length(_grd[0]) ;

    if not assigned(convBytes) then
      SetLength(convBytes,  4*grdwidth * grdheight ) ;
    convert32single_to_32bits ;


    if byteTileWidth = 0 then begin
      byteTileWidth := 4*grdwidth  ;

      if compression = TGIS_CompressionType.LZW then begin
        lzwr := TGIS_FileTIFFLZWCompRecord.Create ;
        lzwr.currTile := 0 ;
        lzwr.outBuff := nil ;
        woutBuff := nil ;

        isTiled := True ;

        lzwr.tileWidth :=  grdwidth  ;
        lzwr.tileLength := grdheight ;
        lzwr.tileRows :=  (Height +grdheight -1) div grdheight ;
        lzwr.tileColumns := (Width +grdwidth -1) div grdwidth ;

        if (lzwr.tileRows = 1) and (lzwr.tileColumns > 1) then
          exit ;

        lzwr.numberOfTiles := lzwr.tileRows * lzwr.tileColumns ;
        SetLength( lzwr.tileOffsets, lzwr.numberOfTiles ) ;
        SetLength( lzwr.tileBytes, lzwr.numberOfTiles) ;
      end ;
    end ;

    if (compression = TGIS_CompressionType.None) or
       {$IFDEF OXYGENE}
         ((lzwr <> nil) and ((lzwr.tileRows = 1) or (lzwr.tileColumns = 1)))then begin
       {$ELSE}
         (((lzwr.tileRows = 1) or (lzwr.tileColumns = 1)))then begin
       {$ENDIF}
      if ( grdwidth  + _x ) > FWidth  then grdwidth  := FWidth  - _x ;
      if ( grdheight + _y ) > FHeight then grdheight := FHeight - _y ;
    end ;

   byteRTileWidth := 4*grdwidth  ;

    if assigned( lzwr ) then
      lzwr.finishTile := False ;

    inptr := convBytes ;
    for row := 0 to grdheight -1 do begin
      ridx := row*byteTileWidth ;
      if row = (grdheight -1) then begin
        if assigned( lzwr ) then
         lzwr.finishTile := True ;
      end ;
      writeLine(inptr, ridx,  _y +row, _x, byteTileWidth );
    end ;
  end ;



  function TGIS_FileTIFF.writeLine( const _buffer : TBytes  ;
                                    const _idx    : Integer ;
                                    const _linenr : Integer ;
                                    const _start  : Integer ;
                                    const _bytes : Integer
                                  ) : Integer;
  var
    offset    : Int64 ;
    wbuff     : TBytes  ;
    bytes     : Integer ;
    tile_line : Integer ;
    start     : Integer ;

    procedure rgb2bgr ;
    var
      loops, woffset, i : Integer ;
      red : Byte ;
    begin
      woffset := _idx ;
      loops := byteRTileWidth div 3;
      for i := 1 to loops do begin
        red := _buffer[woffset  ] ;
        _buffer[woffset  ] := _buffer[woffset+2] ;
        _buffer[woffset+2] := red ;
        woffset := woffset + 3 ;
      end ;
    end ;

    procedure do_HDifferencing ;
    var
      loop1, off, i : Integer ;
      red, green, blue : Integer ;

    begin //horizontal differencing
        loop1 := (_bytes div 3)-1;
        off   := _idx + _bytes -3;
        for i := 1 to loop1 do begin

          red :=   _buffer[off-3] ;
          green := _buffer[off-2] ;
          blue :=  _buffer[off-1] ;

          _buffer[off  ] := Byte(Integer(_buffer[off  ]) - red) ;
          _buffer[off+1] := Byte(Integer(_buffer[off+1]) - green) ;
          _buffer[off+2] := Byte(Integer(_buffer[off+2]) - blue) ;
          off := off -3 ;

        end ;
      end ;

    procedure do_HDifferencing32 ;
    var
      loop1, off, i : Integer ;
      red, green, blue, alpha : Integer ;

    begin //horizontal differencing
        loop1 := (_bytes shr 2)-1;
        off   := _idx + _bytes -4;
        for i := 1 to loop1 do begin

          alpha :=   _buffer[off-4] ;
          red   :=   _buffer[off-3] ;
          green :=   _buffer[off-2] ;
          blue  :=   _buffer[off-1] ;

          _buffer[off  ] := Byte(Integer(_buffer[off  ]) - alpha  ) ;
          _buffer[off+1] := Byte(Integer(_buffer[off+1]) - red) ;
          _buffer[off+2] := Byte(Integer(_buffer[off+2]) - green ) ;
          _buffer[off+3] := Byte(Integer(_buffer[off+3]) - blue) ;
          off := off -4 ;

        end ;
      end ;

  begin
    Result := 0 ;
    start := 0 ;
    try
        case FPixelFormat of
          TGIS_PixelFormat.Custom :
            begin
              start := 4*_start  ;
            end ;
          TGIS_PixelFormat.ARGB :
            begin
              start := 4*_start  ;
              if compression = TGIS_CompressionType.LZW then
                do_HDifferencing32 ;
            end ;
          TGIS_PixelFormat.RGB :
            begin
              start := 3*_start  ;
              if compression = TGIS_CompressionType.LZW then
                do_HDifferencing ;
            end ;
            TGIS_PixelFormat.Bit8  :
              start := _start  ;
           TGIS_PixelFormat.Bit4  :
              start := (_start + 1) div 2  ;
            TGIS_PixelFormat.Bit1  :
              start := (_start + 7) div 8 ;
        end ;

        if compression = TGIS_CompressionType.LZW then begin
          tile_line := _linenr mod lzwr.tileLength ;
          if tile_line = 0  then begin
            bytes := compressLZW(_buffer, _idx, byteRTileWidth, 0) ;

            lzwr.tileOffsets[lzwr.currTile] := fileStream.Position ;
            lzwr.tileBytes[lzwr.currTile] := bytes ;
          end
          else begin
            bytes := compressLZW(_buffer, _idx, byteRTileWidth, 1) ;
            lzwr.tileBytes[lzwr.currTile] := Cardinal(bytes) +
                                             Cardinal(lzwr.tileBytes[lzwr.currTile]) ;
          end ;

          wbuff := TBytes(lzwr.outBuff) ;

          if lzwr.finishTile then begin
            if bytes > 0 then
              {$IFDEF OXYGENE}
                fileStream.Write( lzwr.outBuff, bytes ) ;
              {$ELSE}
                fileStream.Write( lzwr.outBuff[0], bytes ) ;
              {$ENDIF}

            bytes := compressLZW(nil, 0, 0, 2) ; //Finish compression
            if bytes > 0 then begin
              {$IFDEF OXYGENE}
                fileStream.Write( lzwr.outBuff, bytes ) ;
              {$ELSE}
                fileStream.Write( lzwr.outBuff[0], bytes ) ;
              {$ENDIF}
              lzwr.tileBytes[lzwr.currTile] := Cardinal(bytes) +
                                          Cardinal(lzwr.tileBytes[lzwr.currTile]) ;
              bytes := 0 ;
              inc( lzwr.currTile ) ;
            end ;
          end ;
          offset := fileStream.Position ;
        end
        else begin
          bytes := byteRTileWidth ;
          offset := Int64( Int64(offsetStrip) + start + Int64(_linenr)*byteLineWidth ) ;
          wbuff := _buffer ;
        end ;

        fileStream.Position := offset ;
        if bytes > 0 then begin
          if compression = TGIS_CompressionType.LZW then begin
            {$IFDEF OXYGENE}
              fileStream.Write( wbuff,  bytes ) ;
              Result := bytes ;
            {$ELSE}
              Result := fileStream.Write( wbuff[0],  bytes )
            {$ENDIF}
          end else begin
            {$IFDEF OXYGENE}
              fileStream.Write( wbuff, _idx,  bytes ) ;
              Result := bytes ;
            {$ELSE}
              Result := fileStream.Write( wbuff[_idx],  bytes ) ;
            {$ENDIF}
          end ;
        end ;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0) ;
    end ;
  end ;

  procedure TGIS_FileTIFF.prepareCapabilities ;
  begin
    inherited ;

    Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                         TGIS_PixelFormat.RGB, False, TGIS_PixelSubFormat.None,
                         TGIS_CompressionType.None,
                        0
                      )
                    ) ;
    Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                         TGIS_PixelFormat.ARGB, False, TGIS_PixelSubFormat.None,
                         TGIS_CompressionType.None,
                        0
                      )
                    ) ;
    Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                         TGIS_PixelFormat.Custom, False, TGIS_PixelSubFormat.GRID,
                         TGIS_CompressionType.None,
                        0
                      )
                    ) ;
    Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                        TGIS_PixelFormat.Bit8, False, TGIS_PixelSubFormat.None,
                        TGIS_CompressionType.None,
                        0
                      )
                    ) ;

    Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                        TGIS_PixelFormat.Bit8, True, TGIS_PixelSubFormat.None,
                        TGIS_CompressionType.None,
                        0
                      )
                    ) ;

    Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                        TGIS_PixelFormat.Bit4, False, TGIS_PixelSubFormat.None,
                        TGIS_CompressionType.None,
                        0
                      )
                    ) ;
    Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                        TGIS_PixelFormat.Bit1, False, TGIS_PixelSubFormat.None,
                        TGIS_CompressionType.None,
                        0
                      )
                    ) ;
    {$IFNDEF GIS_NOLZWWRITE}
      Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                           TGIS_PixelFormat.RGB, False,
                           TGIS_PixelSubFormat.None, TGIS_CompressionType.LZW,
                        0
                        )
                      ) ;
      Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                           TGIS_PixelFormat.ARGB, False,
                           TGIS_PixelSubFormat.None, TGIS_CompressionType.LZW,
                        0
                        )
                      ) ;
      Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                         TGIS_PixelFormat.Custom, False, TGIS_PixelSubFormat.GRID,
                         TGIS_CompressionType.LZW,
                        0
                      )
                    ) ;
      Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                           TGIS_PixelFormat.Bit8 , False,
                           TGIS_PixelSubFormat.None, TGIS_CompressionType.LZW,
                        0
                        )
                      ) ;
      Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                           TGIS_PixelFormat.Bit8 , True,
                           TGIS_PixelSubFormat.None, TGIS_CompressionType.LZW,
                        0
                        )
                      ) ;
      Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                           TGIS_PixelFormat.Bit4 , False,
                           TGIS_PixelSubFormat.None, TGIS_CompressionType.LZW,
                        0
                        )
                      ) ;
      Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                           TGIS_PixelFormat.Bit1 , False,
                           TGIS_PixelSubFormat.None, TGIS_CompressionType.LZW,
                        0
                        )
                      ) ;
    {$ENDIF}
  end ;

  constructor TGIS_FileTIFF.Create ;
  begin
    inherited ;
  end ;

  constructor TGIS_FileTIFF.Create(
                              const _path        : String      ;
                              const _ext         : TGIS_Extent ;
                              const _width       : Integer     ;
                              const _height      : Integer     ;
                              const _subformat   : TGIS_LayerPixelSubFormat     ;
                              const _ppi         : Integer     ;
                              const _cs          : TGIS_CSCoordinateSystem
                            ) ;
  var
  {$IFDEF OXYGENE}
    i   : Integer ;
  {$ELSE}
    buf : TBytes ;
  {$ENDIF}
  begin
    inherited Create( _path, _ext, _width, _height,
                      _subformat, _ppi, _cs
                    ) ;

    compression := _subformat.Compression ;
    grayScale   := _subformat.GrayScale ;

    cs := _cs ;
    writeMode := True ;
    writeExt  := _ext ;
    XResDenominator := 10 ;
    XResNumerator := PPI * XResDenominator ;

    case PixelFormat of
      TGIS_PixelFormat.ARGB  :
        begin
          byteLineWidth := 4*_width  ;
        end ;
      TGIS_PixelFormat.RGB  :
        begin
          byteLineWidth := 3*_width  ;
        end ;
      TGIS_PixelFormat.Custom  :
        begin
          byteLineWidth := 4*_width  ;
        end ;
      TGIS_PixelFormat.Bit8  :
        begin
          byteLineWidth := _width  ;
          if not assigned(paletteEntry) then begin
            SetLength(paletteEntry, 256) ;
            noEntries := 0 ;
          end;
      end ;
      TGIS_PixelFormat.Bit4  :
        begin
          byteLineWidth := (_width + 1) div 2  ;
        end ;
      TGIS_PixelFormat.Bit1  :
        begin
          byteLineWidth := (_width + 7) div 8 ;
        end ;
      else
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPIXELFORMAT ), '', 0) ;
    end ;

    writeWorldFile( _ext, WORLD_FILE_EXT_TIF, _cs ) ;

    isPalette := False ;

    try
      fileStream := TGIS_FileStream.Create(_path, fmCreate) ;
      {$IFDEF OXYGENE}
        fileStream.Write({$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFFileHeader, length({$IFDEF JAVA}T_ArraysTIFF.{$ENDIF}T_TIFFFileHeader) * sizeOf(Byte));
      {$ELSE}
        fileStream.Write(T_TIFFFileHeader, sizeOf(T_TIFFFileHeader));
      {$ENDIF}

      offsetXRes := fileStream.Position ;
      fileStream.WriteInteger(XResNumerator, sizeOf(XResNumerator));
      fileStream.WriteInteger(XResDenominator, sizeOf(XResDenominator));

      offsetYRes := fileStream.Position ; //YResolution = XResolution
      fileStream.WriteInteger(XResNumerator, sizeOf(XResNumerator));
      fileStream.WriteInteger(XResDenominator, sizeOf(XResDenominator));

      offsetBitsPerSample := fileStream.Position ;

      if FPixelFormat = TGIS_PixelFormat.ARGB then begin
        {$IFDEF OXYGENE}
          for i := 0 to length(BitsPerSample32)-1 do
            fileStream.WriteWord( BitsPerSample32[i], 2 ) ;
        {$ELSE}
          fileStream.Write(BitsPerSample32, sizeOf(BitsPerSample32));
        {$ENDIF}
      end
      else
      if FPixelFormat = TGIS_PixelFormat.RGB then begin
        {$IFDEF OXYGENE}
          for i := 0 to length(BitsPerSample)-1 do
            fileStream.WriteWord( BitsPerSample[i], 2 ) ;
        {$ELSE}
          fileStream.Write(BitsPerSample, sizeOf(BitsPerSample));
        {$ENDIF}
      end ;

      if PixelFormat =  TGIS_PixelFormat.Custom  then begin
        offsetNoData := fileStream.Position ;
        {$IFDEF OXYGENE}
          fileStream.Write( BytesOf(IntToStr(GIS_GRID_NOVALUE)),
                            length( IntToStr(GIS_GRID_NOVALUE) ) ) ;
        {$ELSE}
          buf := ConvertAnsiString( IntToStr(GIS_GRID_NOVALUE) ) ;
          fileStream.Write(buf, length( buf ) ) ;
          buf[0] := 0 ;
          fileStream.Write(buf, 1 ) ;
        {$ENDIF}
      end;

      offsetSoftware := fileStream.Position ;
      {$IFDEF OXYGENE}
        fileStream.Write( BytesOf(GIS_INI_GENERAL_HEADER), length( GIS_INI_GENERAL_HEADER ) ) ;
      {$ELSE}
        buf := ConvertAnsiString( GIS_INI_GENERAL_HEADER ) ;
        fileStream.Write(buf, length( buf ) ) ;
        buf[0] := 0 ;
        fileStream.Write(buf, 1 ) ;
      {$ENDIF}

      offsetStrip := fileStream.Position ;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ), Path, 0) ;
    end ;
  end ;

  procedure TGIS_FileTIFF.doDestroy ;
  var
    ptag       : Integer              ;
    i          : Integer              ;
    geo_key    : TGIS_FileTIFF_GeoKey ;
    dtmp       : Double               ;
    xres, yres : Double               ;
    nullString : Integer              ;
    tagsarr    : array [0..26] of Integer ;
    tagsarrIdx : Word ;
    d_params   : array [0..22] of Double ;
    d_count    : Integer ;
    s_count    : Integer ;
    a_count    : Integer ;
    l_d_count  : Integer ;
    txt        : String ;
    tiff_ta    : T_ArrayTIFF ;
    add_predictor : Boolean ;

    function find_tag( const _tag : Word ) : Integer ;
    var
      ii6 : Integer ;
    begin
      Result := -1  ;
      for ii6 := low( tiff_ta.TIFF_Tags ) to high( tiff_ta.TIFF_Tags ) do
        {$IFDEF JAVA}
          if Integer(tiff_ta.TIFF_Tags[ii6].Tag) = Integer(_tag) then begin
        {$ELSE}
          if tiff_ta.TIFF_Tags[ii6].Tag = _tag then begin
        {$ENDIF}
          Result := ii6 ;
          exit ;
        end ;
    end ;

    procedure sort_tagsarr ;
    var
      ii : Integer ;
      t  : Integer ;
      sorted : Boolean ;
    begin
      repeat
        sorted := True ;
        for ii := 0 to tagsarrIdx -2 do begin
          if tiff_ta.TIFF_Tags[(tagsarr[ii])].Tag >
             tiff_ta.TIFF_Tags[(tagsarr[ii +1])].Tag
          then begin
            t := tagsarr[ii] ;
            tagsarr[ii] := tagsarr[ii +1] ;
            tagsarr[ii +1] := t ;
            sorted := False ;
          end ;
        end ;
      until sorted ;
    end ;

    procedure writeIFD ;
    var
      ii1 : Integer ;
    begin
      offsetDir := fileStream.Position ;
      fileStream.WriteWord( tagsarrIdx, sizeOf(tagsarrIdx)) ;

      for ii1 := 0 to tagsarrIdx -1 do
        {$IFDEF OXYGENE}
          WriteFT( fileStream, tiff_ta.TIFF_Tags[(tagsarr[ii1])] ) ;
        {$ELSE}
          fileStream.Write( tiff_ta.TIFF_Tags[(tagsarr[ii1])],
                            sizeOf(TGIS_FileTIFF_Tag)
                          ) ;
        {$ENDIF}
    end ;

    procedure prepare_and_write_palette(const _colorno : Integer) ;
    var
      colorTableRed   : array [0..256] of Word ;
      colorTableGreen : array [0..256] of Word ;
      colorTableBlue  : array [0..256] of Word ;
      ii2 : Integer ;
    begin
      for ii2 := 0 to _colorno - 1 do begin
        colorTableRed[  ii2] := paletteEntry[ii2].R * 256 ;
        colorTableGreen[ii2] := paletteEntry[ii2].G * 256 ;
        colorTableBlue[ ii2] := paletteEntry[ii2].B * 256 ;
      end ;
      ii2 := find_tag(TIFF_TAG_COLORMAPOFFSET) ;
      tiff_ta.TIFF_Tags[ii2].Count := 3 * _colorno ;
      tiff_ta.TIFF_Tags[ii2].Value := fileStream.Position ;

      {$IFDEF OXYGENE}
        for ii2 := 0 to _colorno-1 do
          fileStream.WriteWord( colorTableRed[ii2], 2 ) ;
        for ii2 := 0 to _colorno-1 do
          fileStream.WriteWord( colorTableGreen[ii2], 2 ) ;
        for ii2 := 0 to _colorno-1 do
          fileStream.WriteWord( colorTableBlue[ii2], 2 ) ;
      {$ELSE}
        fileStream.Write( colorTableRed,   sizeOf( Word ) * _colorno ) ;
        fileStream.Write( colorTableGreen, sizeOf( Word ) * _colorno ) ;
        fileStream.Write( colorTableBlue,  sizeOf( Word ) * _colorno ) ;
      {$ENDIF}

    end ;

    procedure write_d_params ;
    var
      m3 : Integer ;
      d3 : Double ;
    begin
      for m3 := 0 to d_count - 1 do begin
        d3 := d_params[m3] ;
        fileStream.WriteDouble( d3, 8 ) ;
      end ;
    end ;

    procedure write_a_params ;
    var
      m1 : Integer ;
      b : Byte ;
    begin
      for m1 := StringFirst to StringLast(txt) do begin
        b := Byte(txt[m1]) ;
        fileStream.WriteByte( b, 1 ) ;
      end ;
    end ;

    function check_d_params : Integer ;
    var
      pcs       : TGIS_CSProjectedCoordinateSystem  ;
    begin

      Result := 0 ;
      a_count := 0 ;
      if cs is TGIS_CSProjectedCoordinateSystem then begin
        pcs := cs as TGIS_CSProjectedCoordinateSystem ;
        s_count := 10 ;
        txt := pcs.WKT ;
        a_count := length(txt) ;
        if a_count > 0 then
          inc(s_count) ;

        if not IsNan(pcs.Projection.Parameters.StandardParallel_1) then
          inc(Result) ;

        if not IsNan(pcs.Projection.Parameters.StandardParallel_2) then
          inc(Result) ;

        if not IsNan(pcs.Projection.Parameters.CentralMeridian) then
          inc(Result) ;

        if not IsNan(pcs.Projection.Parameters.LatitudeOfOrigin) then
          inc(Result) ;

        if not IsNan(pcs.Projection.Parameters.FalseEasting) then
          inc(Result) ;

        if not IsNan(pcs.Projection.Parameters.FalseNorthing) then
          inc(Result) ;

        if not IsNan(pcs.Projection.Parameters.CentralMeridian) then
          inc(Result) ;

        if not IsNan(pcs.Projection.Parameters.LatitudeOfOrigin) then
          inc(Result) ;

        if not IsNan(pcs.Projection.Parameters.ScaleFactor) then
          inc(Result) ;

        if not IsNan(pcs.Projection.Parameters.Azimuth) then
          inc(Result) ;

      end
      else if cs is TGIS_CSGeographicCoordinateSystem then begin
        s_count := 1 ;
      end
      else
        s_count := 0 ;
    end ;

    procedure write_proj_params ;
    var
      gcs       : TGIS_CSGeographicCoordinateSystem ;
      pcs       : TGIS_CSProjectedCoordinateSystem  ;
      gcs_epsg  : Integer ;
      pcs_epsg  : Integer ;
      prj_epsg  : Integer ;
      unt_epsg  : Integer ;
    begin
      d_count := 0 ;
      if cs is TGIS_CSProjectedCoordinateSystem then begin
        s_count := 10 ;
        pcs := cs as TGIS_CSProjectedCoordinateSystem ;
        gcs := pcs.Geocs ;

        pcs_epsg := pcs.EPSG ;
        prj_epsg := pcs.Projection.EPSG ;
        unt_epsg := pcs.Units.EPSG ;

        geo_key.KeyID       := 1024 ; // keyGTModelType
        geo_key.KeyType     := 0 ;
        geo_key.Count       := 1 ;
        geo_key.Value_Index := 1 ;
        {$IFDEF OXYGENE}
          WriteFGK( fileStream, geo_key ) ;
        {$ELSE}
          fileStream.Write( geo_key, sizeOf( geo_key ) ) ;
        {$ENDIF}

        geo_key.KeyID       := 1025 ; // keyGTRasterType
        geo_key.KeyType     := 0 ;
        geo_key.Count       := 1 ;
        geo_key.Value_Index := 1 ;
        {$IFDEF OXYGENE}
          WriteFGK( fileStream, geo_key ) ;
        {$ELSE}
          fileStream.Write( geo_key, sizeOf( geo_key ) ) ;
        {$ENDIF}

        geo_key.KeyID       := 2048 ; // keyGeographicType
        geo_key.KeyType     := 0 ;
        geo_key.Count       := 1 ;
        if (pcs_epsg > 0) and (pcs_epsg < 65535) then
          geo_key.Value_Index := Word(gcs.EPSG)
        else
          geo_key.Value_Index := 32767 ;

        {$IFDEF OXYGENE}
          WriteFGK( fileStream, geo_key ) ;
        {$ELSE}
          fileStream.Write( geo_key, sizeOf( geo_key ) ) ;
        {$ENDIF}

        geo_key.KeyID       := 2050 ; // keyGeogGeodeticDatum
        geo_key.KeyType     := 0 ;
        geo_key.Count       := 1 ;
        if (pcs_epsg > 0) and (pcs_epsg < 65535) then
          geo_key.Value_Index := Word(gcs.Datum.EPSG)
        else
          geo_key.Value_Index := 32767 ;
        {$IFDEF OXYGENE}
          WriteFGK( fileStream, geo_key ) ;
        {$ELSE}
          fileStream.Write( geo_key, sizeOf( geo_key ) ) ;
        {$ENDIF}

        geo_key.KeyID       := 2054 ; // keyGeogAngularUnits
        geo_key.KeyType     := 0 ;
        geo_key.Count       := 1 ;
        geo_key.Value_Index := 9102 ;

        {$IFDEF OXYGENE}
          WriteFGK( fileStream, geo_key ) ;
        {$ELSE}
          fileStream.Write( geo_key, sizeOf( geo_key ) ) ;
        {$ENDIF}

        geo_key.KeyID       := 2056 ; // keyGeogEllipsoid
        geo_key.KeyType     := 0 ;
        geo_key.Count       := 1 ;
        geo_key.Value_Index := Word(gcs.Datum.Ellipsoid.EPSG) ;
        {$IFDEF OXYGENE}
          WriteFGK( fileStream, geo_key ) ;
        {$ELSE}
          fileStream.Write( geo_key, sizeOf( geo_key ) ) ;
        {$ENDIF}

        geo_key.KeyID       := 3072 ; // keyProjectedCSType
        geo_key.KeyType     := 0 ;
        geo_key.Count       := 1 ;
        if (pcs_epsg > 0) and (pcs_epsg < 65535) then
          geo_key.Value_Index := Word(pcs_epsg)
        else
          geo_key.Value_Index := 32767 ; //User defined

        {$IFDEF OXYGENE}
          WriteFGK( fileStream, geo_key ) ;
        {$ELSE}
          fileStream.Write( geo_key, sizeOf( geo_key ) ) ;
        {$ENDIF}

        if a_count > 0 then begin
          geo_key.KeyID       := 3073 ;  // keyProjectedCSType
          geo_key.KeyType     := 34737 ; // ASCII params
          geo_key.Count       := a_count +1 ;
          geo_key.Value_Index := 0 ; //User defined

        {$IFDEF OXYGENE}
          WriteFGK( fileStream, geo_key ) ;
        {$ELSE}
          fileStream.Write( geo_key, sizeOf( geo_key ) ) ;
        {$ENDIF}

        end ;

        geo_key.KeyID       := 3074 ; // keyProjected
        geo_key.KeyType     := 0 ;
        geo_key.Count       := 1 ;
        geo_key.Value_Index := 32767 ;
        {$IFDEF OXYGENE}
          WriteFGK( fileStream, geo_key ) ;
        {$ELSE}
          fileStream.Write( geo_key, sizeOf( geo_key ) ) ;
        {$ENDIF}

        geo_key.KeyID       := 3075 ; // keyProjCoordTrans
        geo_key.KeyType     := 0 ;
        geo_key.Count       := 1 ;

        case prj_epsg of
          9801 :     // CSPROJ_Lambert_Conformal_Conic_1SP
            geo_key.Value_Index := 9 ;
          9802 :      // CSPROJ_Lambert_Conformal_Conic_2SP
            geo_key.Value_Index := 8 ;
          9803 :      // CSPROJ_Lambert_Conformal_Conic_2SP_Belgium
            geo_key.Value_Index := 8 ;        //????
          9804 :      // CSPROJ_Mercator_1SP
            geo_key.Value_Index := 7 ;        //????
          9805 :      // CSPROJ_Mercator
            geo_key.Value_Index := 7 ;
          9806 :      // CSPROJ_Cassini_Soldner
            geo_key.Value_Index := 18 ;
          9807 :      // CSPROJ_Transverse_Mercator
            geo_key.Value_Index := 1 ;
          9808 :      // CSPROJ_Transverse_Mercator_South_Oriented
            geo_key.Value_Index := 27 ;
          9809 :      // CSPROJ_Oblique_Stereographic
            geo_key.Value_Index := 16 ;
          9810 :      // CSPROJ_Polar_Stereographic
            geo_key.Value_Index := 15 ;
          9811 :      // CSPROJ_New_Zealand_Map_Grid
            geo_key.Value_Index := 26 ;
          9812 :      // CSPROJ_Hotine_Oblique_Mercator_Azimuth_Natural_Origin
            geo_key.Value_Index := 3 ;
          9814 :      // CSPROJ_Swiss_Oblique_Mercator
            geo_key.Value_Index := 3 ;
          9815 :      // CSPROJ_Hotine_Oblique_Mercator_Azimuth_Center
            geo_key.Value_Index := 3 ;
          9818 :      // CSPROJ_Polyconic
            geo_key.Value_Index := 22 ;
          9817 :      // CSPROJ_Lambert_Near_Conformal_Conic
            geo_key.Value_Index := 9 ;
          9819 :      // CSPROJ_Krovak
            geo_key.Value_Index := 0 ;
          9820 :      // CSPROJ_Azimuthal_Equal_Area
            geo_key.Value_Index := 11 ;
          9821 :      // CSPROJ_Lambert_Azimuthal_Equal_Area
            geo_key.Value_Index := 10 ;
          9822 :      // CSPROJ_Albers
            geo_key.Value_Index := 11 ;
          9823 :      // CSPROJ_Plate_Carree
            geo_key.Value_Index := 0 ;
          9829 :      // CSPROJ_Polar_Stereographic_B
            geo_key.Value_Index := 15 ;
          9830 :      // CSPROJ_Polar_Stereographic_C
            geo_key.Value_Index := 15 ;
          9841 :      // CSPROJ_Mercator_1SP_Spherical
            geo_key.Value_Index := 7 ;
        end ;

        {$IFDEF OXYGENE}
          WriteFGK( fileStream, geo_key ) ;
        {$ELSE}
          fileStream.Write( geo_key, sizeOf( geo_key ) ) ;
        {$ENDIF}

        geo_key.KeyID       := 3076 ; // keyProjLinearUnits
        geo_key.KeyType     := 0 ;
        geo_key.Count       := 1 ;
        geo_key.Value_Index := unt_epsg ;
        {$IFDEF OXYGENE}
          WriteFGK( fileStream, geo_key ) ;
        {$ELSE}
          fileStream.Write( geo_key, sizeOf( geo_key ) ) ;
        {$ENDIF}

//Double params BEGIN
        if not IsNan(pcs.Projection.Parameters.StandardParallel_1) then begin
          geo_key.KeyID       := 3078 ; // keyProjStdParallel1
          geo_key.KeyType     := 34736 ;
          geo_key.Count       := 1 ;
          geo_key.Value_Index := Word(d_count) ;
          {$IFDEF OXYGENE}
            WriteFGK( fileStream, geo_key ) ;
          {$ELSE}
            fileStream.Write( geo_key, sizeOf( geo_key ) ) ;
          {$ENDIF}
         d_params[d_count] :=
            RadToDeg(pcs.Projection.Parameters.StandardParallel_1) ;
          inc(d_count) ;
        end ;

        if not IsNan(pcs.Projection.Parameters.StandardParallel_2) then begin
          geo_key.KeyID       := 3079 ; // keyProjStdParallel2
          geo_key.KeyType     := 34736 ;
          geo_key.Count       := 1 ;
          geo_key.Value_Index := Word(d_count) ;

          {$IFDEF OXYGENE}
            WriteFGK( fileStream, geo_key ) ;
          {$ELSE}
            fileStream.Write( geo_key, sizeOf( geo_key ) ) ;
          {$ENDIF}
         d_params[d_count] :=
              RadToDeg(pcs.Projection.Parameters.StandardParallel_2) ;
          inc(d_count) ;
        end ;

        if not IsNan(pcs.Projection.Parameters.CentralMeridian) then begin
          geo_key.KeyID       := 3080 ; // keyProjNatOriginLong
          geo_key.KeyType     := 34736 ;
          geo_key.Count       := 1 ;
          geo_key.Value_Index := Word(d_count) ;
          {$IFDEF OXYGENE}
            WriteFGK( fileStream, geo_key ) ;
          {$ELSE}
            fileStream.Write( geo_key, sizeOf( geo_key ) ) ;
          {$ENDIF}
          d_params[d_count] :=
            RadToDeg(pcs.Projection.Parameters.CentralMeridian) ;
          inc(d_count) ;
        end ;

        if not IsNan(pcs.Projection.Parameters.LatitudeOfOrigin) then begin
          geo_key.KeyID       := 3081 ; // keyProjNatOriginLat
          geo_key.KeyType     := 34736 ;
          geo_key.Count       := 1 ;
          geo_key.Value_Index := Word(d_count) ;
          {$IFDEF OXYGENE}
            WriteFGK( fileStream, geo_key ) ;
          {$ELSE}
            fileStream.Write( geo_key, sizeOf( geo_key ) ) ;
          {$ENDIF}
          d_params[d_count] :=
            RadToDeg(pcs.Projection.Parameters.LatitudeOfOrigin) ;
          inc(d_count) ;
        end ;

        if not IsNan(pcs.Projection.Parameters.FalseEasting) then begin
          geo_key.KeyID       := 3082 ; // keyProjFalseEasting
          geo_key.KeyType     := 34736 ;
          geo_key.Count       := 1 ;
          geo_key.Value_Index := Word(d_count) ;
          {$IFDEF OXYGENE}
            WriteFGK( fileStream, geo_key ) ;
          {$ELSE}
            fileStream.Write( geo_key, sizeOf( geo_key ) ) ;
          {$ENDIF}
          d_params[d_count] := pcs.Projection.Parameters.FalseEasting ;
          inc(d_count) ;
        end ;

        if not IsNan(pcs.Projection.Parameters.FalseNorthing) then begin
          geo_key.KeyID       := 3083 ; // keyProjFalseNorthing
          geo_key.KeyType     := 34736 ;
          geo_key.Count       := 1 ;
          geo_key.Value_Index := Word(d_count) ;
          {$IFDEF OXYGENE}
            WriteFGK( fileStream, geo_key ) ;
          {$ELSE}
            fileStream.Write( geo_key, sizeOf( geo_key ) ) ;
          {$ENDIF}
          d_params[d_count] := pcs.Projection.Parameters.FalseNorthing ;
          inc(d_count) ;
        end ;

        if not IsNan(pcs.Projection.Parameters.CentralMeridian) then begin
          geo_key.KeyID       := 3084 ; // keyProjFalseOriginLong
          geo_key.KeyType     := 34736 ;
          geo_key.Count       := 1 ;
          geo_key.Value_Index := Word(d_count) ;

          {$IFDEF OXYGENE}
            WriteFGK( fileStream, geo_key ) ;
          {$ELSE}
            fileStream.Write( geo_key, sizeOf( geo_key ) ) ;
          {$ENDIF}
         d_params[d_count] :=
            RadToDeg(pcs.Projection.Parameters.CentralMeridian) ;
          inc(d_count) ;
        end ;

        if not IsNan(pcs.Projection.Parameters.LatitudeOfOrigin) then begin
          geo_key.KeyID       := 3085 ; // keyProjFalseOriginLat
          geo_key.KeyType     := 34736 ;
          geo_key.Count       := 1 ;
          geo_key.Value_Index := Word(d_count) ;

          {$IFDEF OXYGENE}
            WriteFGK( fileStream, geo_key ) ;
          {$ELSE}
            fileStream.Write( geo_key, sizeOf( geo_key ) ) ;
          {$ENDIF}
          d_params[d_count] :=
            RadToDeg(pcs.Projection.Parameters.LatitudeOfOrigin) ;
          inc(d_count) ;
        end ;

        if not IsNan(pcs.Projection.Parameters.ScaleFactor) then begin
          geo_key.KeyID       := 3092 ; // keyProjScaleAtNatOrigin
          geo_key.KeyType     := 34736 ;
          geo_key.Count       := 1 ;
          geo_key.Value_Index := Word(d_count) ;
          {$IFDEF OXYGENE}
            WriteFGK( fileStream, geo_key ) ;
          {$ELSE}
            fileStream.Write( geo_key, sizeOf( geo_key ) ) ;
          {$ENDIF}
          d_params[d_count] := pcs.Projection.Parameters.ScaleFactor ;
          inc(d_count) ;
        end ;

        if not IsNan(pcs.Projection.Parameters.Azimuth) then begin
          geo_key.KeyID       := 3094 ; // keyProjAzimuthAngle
          geo_key.KeyType     := 34736 ;
          geo_key.Count       := 1 ;
          geo_key.Value_Index := Word(d_count) ;

          {$IFDEF OXYGENE}
            WriteFGK( fileStream, geo_key ) ;
          {$ELSE}
            fileStream.Write( geo_key, sizeOf( geo_key ) ) ;
          {$ENDIF}
          d_params[d_count] := 0 ;
            RadToDeg(pcs.Projection.Parameters.Azimuth) ;
          inc(d_count) ;
        end ;
//Double params END

      end
      else if cs is TGIS_CSGeographicCoordinateSystem then begin
        gcs := cs as TGIS_CSGeographicCoordinateSystem ;
        gcs_epsg := gcs.EPSG ;

        geo_key.KeyID       := 2048 ; // keyGeographicType
        geo_key.KeyType     := 1 ;
        geo_key.Count       := 1 ;
        geo_key.Value_Index := gcs_epsg ;

        {$IFDEF OXYGENE}
          WriteFGK( fileStream, geo_key ) ;
        {$ELSE}
          fileStream.Write( geo_key, sizeOf( geo_key ) ) ;
        {$ENDIF}

      end ;
    end ;

  begin
    try
      add_predictor := True ;
      tiff_ta := nil ;
      if writeMode then begin
        tiff_ta := T_ArrayTIFF.Create ;
        tagsarrIdx := 0 ;
        for i := 0 to BASE_TAGS_NO -1 do
         tagsarr[i] := i ;

        tagsarrIdx := BASE_TAGS_NO ;

        ptag := find_tag( TIFF_TAG_IMAGEWIDTH ) ;
        tiff_ta.TIFF_Tags[ptag].Value := Width ;

        ptag := find_tag( TIFF_TAG_IMAGELENGTH ) ;
        tiff_ta.TIFF_Tags[ptag].Value := Height ;

        case FPixelFormat of
      TGIS_PixelFormat.ARGB  :
            begin
              ptag := find_tag(TIFF_TAG_BITSPERSAMPLE ) ;
              tiff_ta.TIFF_Tags[ptag].Count := 4 ;
              tiff_ta.TIFF_Tags[ptag].Value := offsetBitsPerSample ;
              ptag := find_tag( TIFF_TAG_PHOTOMETRIC ) ;
              tiff_ta.TIFF_Tags[ptag].Value := 2 ;
              ptag := find_tag( TIFF_TAG_SAMPLESPERPIXELS ) ;
              tiff_ta.TIFF_Tags[ptag].Value := 4 ;
              ptag := find_tag( TIFF_TAG_EXTRASAMPLES ) ;
              tagsarr[tagsarrIdx] := ptag ;
              inc(tagsarrIdx) ;
          end ;
      TGIS_PixelFormat.RGB  :
            begin
              ptag := find_tag(TIFF_TAG_BITSPERSAMPLE ) ;
              tiff_ta.TIFF_Tags[ptag].Count := 3 ;
              tiff_ta.TIFF_Tags[ptag].Value := offsetBitsPerSample ;
              ptag := find_tag( TIFF_TAG_PHOTOMETRIC ) ;
              tiff_ta.TIFF_Tags[ptag].Value := 2 ;
              ptag := find_tag( TIFF_TAG_SAMPLESPERPIXELS ) ;
              tiff_ta.TIFF_Tags[ptag].Value := 3 ;
            end ;
      TGIS_PixelFormat.Custom  :
            begin
              ptag := find_tag(TIFF_TAG_BITSPERSAMPLE ) ;
              tiff_ta.TIFF_Tags[ptag].Count := 1 ;
              tiff_ta.TIFF_Tags[ptag].Value := 32 ;
              ptag := find_tag( TIFF_TAG_PHOTOMETRIC ) ;
              tiff_ta.TIFF_Tags[ptag].Value := 1 ;
              ptag := find_tag( TIFF_TAG_SAMPLESPERPIXELS ) ;
              tiff_ta.TIFF_Tags[ptag].Value := 1 ;
              ptag := find_tag( TIFF_TAG_PREDICTOR ) ;
              tiff_ta.TIFF_Tags[ptag].Value := 1 ;
              ptag := find_tag( TIFF_TAG_SAMPLEFORMAT ) ;
              tiff_ta.TIFF_Tags[ptag].Value := 3 ;
              tagsarr[tagsarrIdx] := ptag ;
              inc(tagsarrIdx) ;
              ptag := find_tag(GEOTIFF_TAG_GDALNODATA ) ;
              tiff_ta.TIFF_Tags[ptag].Value := offsetNoData ;
              tagsarr[tagsarrIdx] := ptag ;
              inc(tagsarrIdx) ;
          end ;
        TGIS_PixelFormat.Bit8  :
              begin
                add_predictor := False ;
                ptag := find_tag( TIFF_TAG_BITSPERSAMPLE ) ;
                tiff_ta.TIFF_Tags[ptag].Count := 1 ;
                tiff_ta.TIFF_Tags[ptag].Value := 8 ;
                ptag := find_tag( TIFF_TAG_PHOTOMETRIC ) ;
                if grayScale then
                  tiff_ta.TIFF_Tags[ptag].Value := 1
                else
                  tiff_ta.TIFF_Tags[ptag].Value := 3 ;
                ptag := find_tag( TIFF_TAG_SAMPLESPERPIXELS ) ;
                tiff_ta.TIFF_Tags[ptag].Value := 1 ;
                if not grayScale then
                  prepare_and_write_palette(256) ;
              end ;
        TGIS_PixelFormat.Bit4  :
              begin
                add_predictor := False ;
                ptag := find_tag( TIFF_TAG_BITSPERSAMPLE ) ;
                tiff_ta.TIFF_Tags[ptag].Count := 1 ;
                tiff_ta.TIFF_Tags[ptag].Value := 4 ;
                ptag := find_tag( TIFF_TAG_PHOTOMETRIC ) ;
                tiff_ta.TIFF_Tags[ptag].Value := 3 ;
                ptag := find_tag( TIFF_TAG_SAMPLESPERPIXELS ) ;
                tiff_ta.TIFF_Tags[ptag].Value := 1 ;
                prepare_and_write_palette(16) ;
              end ;
        TGIS_PixelFormat.Bit1  :
              begin
                add_predictor := False ;
                ptag := find_tag( TIFF_TAG_BITSPERSAMPLE ) ;
                tiff_ta.TIFF_Tags[ptag].Count := 1 ;
                tiff_ta.TIFF_Tags[ptag].Value := 1 ;
                ptag := find_tag( TIFF_TAG_PHOTOMETRIC ) ;
                tiff_ta.TIFF_Tags[ptag].Value := 1 ;
                ptag := find_tag( TIFF_TAG_SAMPLESPERPIXELS ) ;
                tiff_ta.TIFF_Tags[ptag].Value := 1 ;
                prepare_and_write_palette(2) ;
              end ;
        end ;

        if compression <> TGIS_CompressionType.None then begin
          ptag := find_tag( TIFF_TAG_COMPRESSION ) ;
          tiff_ta.TIFF_Tags[ptag].Value := TIFF_COMPRESSION_LZW ;

          if add_predictor then begin
            tagsarr[tagsarrIdx] := find_tag(TIFF_TAG_PREDICTOR) ; ;
            inc(tagsarrIdx) ;
          end ;

          if isTiled and (lzwr.numberOfTiles > 1) then begin
            ptag := find_tag( TIFF_TAG_TILEWIDTH ) ;
            tagsarr[tagsarrIdx] := ptag ;
            inc(tagsarrIdx) ;
            tiff_ta.TIFF_Tags[ptag].Value := lzwr.tileWidth ;

            ptag := find_tag( TIFF_TAG_TILELENGTH ) ;
            tagsarr[tagsarrIdx] := ptag ;
            inc(tagsarrIdx) ;
            tiff_ta.TIFF_Tags[ptag].Value := lzwr.tileLength ;

            ptag := find_tag(TIFF_TAG_TILEOFFSETS ) ;
            tagsarr[tagsarrIdx] := ptag ;
            inc(tagsarrIdx) ;
            tiff_ta.TIFF_Tags[ptag].Value := fileStream.Position ;
            tiff_ta.TIFF_Tags[ptag].Count := lzwr.numberOfTiles ;

            {$IFDEF OXYGENE}
              for i := 0 to length(lzwr.tileOffsets)-1 do
                fileStream.WriteCardinal( lzwr.tileOffsets[i], 4 ) ;
            {$ELSE}
              fileStream.Write( lzwr.tileOffsets[0],
                              sizeOf(Integer)*length(lzwr.tileOffsets) )  ;
            {$ENDIF}

            ptag := find_tag(TIFF_TAG_TILEBYTECOUNT ) ;
            tagsarr[tagsarrIdx] := ptag ;
            inc(tagsarrIdx) ;
            tiff_ta.TIFF_Tags[ptag].Value := fileStream.Position ;
            tiff_ta.TIFF_Tags[ptag].Count := lzwr.numberOfTiles ;

            {$IFDEF OXYGENE}
              for i := 0 to length(lzwr.tileBytes)-1 do
                fileStream.WriteCardinal( lzwr.tileBytes[i], 4 ) ;
            {$ELSE}
              fileStream.Write( lzwr.tileBytes[0],
                            sizeOf(Integer)*length(lzwr.tileBytes) )  ;
            {$ENDIF}
          end //stripped
          else begin
            ptag := find_tag( TIFF_TAG_ROWSPERSTRIP ) ;
            tagsarr[tagsarrIdx] := ptag ;
            inc(tagsarrIdx) ;
            tiff_ta.TIFF_Tags[ptag].Value := Height ;

            ptag := find_tag( TIFF_TAG_STRIPBYTECOUNTS ) ;
            tagsarr[tagsarrIdx] := ptag ;
            inc(tagsarrIdx) ;
            tiff_ta.TIFF_Tags[ptag].Value := lzwr.tileBytes[0] ;

            ptag := find_tag( TIFF_TAG_STRIPOFFSETS ) ;
            tagsarr[tagsarrIdx] := ptag ;
            inc(tagsarrIdx) ;
            tiff_ta.TIFF_Tags[ptag].Value := offsetStrip ;
          end ;
        end
        else begin
          ptag := find_tag( TIFF_TAG_ROWSPERSTRIP ) ;
          tagsarr[tagsarrIdx] := ptag ;
          inc(tagsarrIdx) ;
          tiff_ta.TIFF_Tags[ptag].Value := Height ;

          ptag := find_tag( TIFF_TAG_STRIPBYTECOUNTS ) ;
          tagsarr[tagsarrIdx] := ptag ;
          inc(tagsarrIdx) ;
          tiff_ta.TIFF_Tags[ptag].Value := Cardinal(byteLineWidth) * Cardinal(Height) ;

          ptag := find_tag( TIFF_TAG_STRIPOFFSETS ) ;
          tagsarr[tagsarrIdx] := ptag ;
          inc(tagsarrIdx) ;
          tiff_ta.TIFF_Tags[ptag].Value := offsetStrip ;
        end ;

        ptag := find_tag( TIFF_TAG_XRESOLUTION ) ;
        tiff_ta.TIFF_Tags[ptag].Value := offsetXRes ;

        ptag := find_tag( TIFF_TAG_YRESOLUTION ) ;
        tiff_ta.TIFF_Tags[ptag].Value := offsetYRes ;

        ptag := find_tag( TIFF_TAG_SOFTWARE ) ;
        tiff_ta.TIFF_Tags[ptag].Value := offsetSoftware ;

        if isPalette and (not grayScale) then begin
            tagsarr[tagsarrIdx] := find_tag(TIFF_TAG_COLORMAPOFFSET) ;
            inc(tagsarrIdx) ;
        end ;

      // write GEOTIFF_TAG_GEOKEYDIRECTORY content
        l_d_count := check_d_params ;
        ptag := find_tag( GEOTIFF_TAG_GEOKEYDIRECTORY ) ;
        tiff_ta.TIFF_Tags[ptag].Count := (1 +s_count  +l_d_count)*4 ;
        tiff_ta.TIFF_Tags[ptag].Value := fileStream.Position ;
        tagsarr[tagsarrIdx] := ptag ;
        inc(tagsarrIdx) ;

      //Geokeys

        geo_key.KeyID       := 1 ;
        geo_key.KeyType     := 1 ;
        geo_key.Count       := 0 ;
        geo_key.Value_Index := s_count +l_d_count;

        {$IFDEF OXYGENE}
          WriteFGK( fileStream, geo_key ) ;
        {$ELSE}
          fileStream.Write( geo_key, sizeOf( geo_key ) ) ;
        {$ENDIF}
      // Projection params
        if assigned(cs) then begin
          write_proj_params ;
          if d_count > 0 then begin
            ptag := find_tag( GEOTIFF_TAG_GEODOUBLEPARAMS ) ;
            tiff_ta.TIFF_Tags[ptag].Count := d_count ;
            tiff_ta.TIFF_Tags[ptag].Value := fileStream.Position ;
            tagsarr[tagsarrIdx] := ptag ;
            inc(tagsarrIdx) ;
            write_d_params ;
          end ;
          if a_count > 0 then begin
            ptag := find_tag( GEOTIFF_TAG_GEOASCIIPARAMS ) ;
            tiff_ta.TIFF_Tags[ptag].Count := a_count ;
            tiff_ta.TIFF_Tags[ptag].Value := fileStream.Position ;
            tagsarr[tagsarrIdx] := ptag ;
            inc(tagsarrIdx) ;
            write_a_params ;
          end ;
        end ;

      // write GEOTIFF_TAG_MODELTIEPOINT content
        ptag := find_tag( GEOTIFF_TAG_MODELTIEPOINT ) ;
        tiff_ta.TIFF_Tags[ptag].Value := fileStream.Position ;
        tagsarr[tagsarrIdx] := ptag ;
        inc(tagsarrIdx) ;

        xres := ( writeExt.XMax - writeExt.XMin ) / Width ;
        yres := ( writeExt.YMax - writeExt.YMin ) / Height ;

        dtmp := 0 ;
        fileStream.WriteDouble( dtmp, sizeOf( dtmp ) ) ; // i
        fileStream.WriteDouble( dtmp, sizeOf( dtmp ) ) ; // j
        fileStream.WriteDouble( dtmp, sizeOf( dtmp ) ) ; // k
        dtmp := writeExt.XMin ;
        fileStream.WriteDouble( dtmp, sizeOf( dtmp ) ) ; // x
        dtmp := writeExt.YMax ;
        fileStream.WriteDouble( dtmp, sizeOf( dtmp ) ) ; // y
        dtmp := 0 ;
        fileStream.WriteDouble( dtmp, sizeOf( dtmp ) ) ; // z

      // write GEOTIFF_TAG_MODELPIXELSCALE content
        ptag := find_tag( GEOTIFF_TAG_MODELPIXELSCALE ) ;
        tiff_ta.TIFF_Tags[ptag].Value := fileStream.Position ;
        tagsarr[tagsarrIdx] := ptag ;
        inc(tagsarrIdx) ;

        fileStream.WriteDouble( xres, sizeOf( xres ) ) ;
        fileStream.WriteDouble( yres, sizeOf( yres ) ) ;

        sort_tagsarr ;
        writeIFD ;

        nullString := 0 ;
        fileStream.WriteInteger(nullString, sizeOf(nullString));

        fileStream.Seek( 4, soBeginning ) ;
        fileStream.WriteCardinal(offsetDir, sizeOf(offsetDir)) ;
      end ;
    finally
      FreeObject(tiff_ta) ;
      FreeObject( fileStream ) ;
      if assigned(convBytes) then
        convBytes := nil ;

      if assigned(woutBuff) then
        woutBuff := nil ;

      if assigned(lzwr) then begin
        lzwr.tileOffsets := nil ;
        lzwr.tileBytes := nil ;
        if assigned(lzwr.outBuff) then
          lzwr.outBuff := nil ;

        FreeObject( lzwr ) ;
      end ;

      if isPalette then
        paletteEntry := nil ;

      FreeObject(tiff_ta) ;

      inherited ;
    end ;
  end ;


  procedure TGIS_FileTIFF.Write(const _x, _y : Integer;
                                const _pixels  : TGIS_Pixels ;
                                const _pformat : TGIS_PixelFormat ;
                                const _width   : Integer ;
                                const _height  : Integer
  ) ;
  var
    row       : Integer ;
//    ex_bmp, exconvBmp : TGIS_Pixels ;
    palettesize : Integer ;
    lr, av : Integer ;
    inptr   : TBytes  ;
    bmpwidth  : Integer ;
    bmpheight : Integer ;
    ridx  : Integer ;

    function find_in_palette(const _pixel : Integer) : Integer ;
    var
      i : Integer ;
      maxdif  : Integer ;
      dif  : Integer ;
      ld1, ld2, ld3  : Integer ;
      red, green, blue : Byte ;
    begin
      blue  := (_pixel       ) and $FF ;
      if grayScale then begin
        Result := blue ;
        lr := blue ;
        exit ;
      end;
      green := (_pixel shr 08) and $FF ;
      red   := (_pixel shr 16) and $FF;
      Result := 0 ;
      if noEntries = 0 then begin
        inc(noEntries) ;
        paletteEntry[0] := TGIS_Color.FromRGB(red,green,blue) ;
        exit ;
      end;
      maxdif := 48 ;

      for i := noEntries - 1 downto 0 do begin
        ld1 := Abs(Integer(paletteEntry[i].R) -red  ) ;
        ld2 := Abs(Integer(paletteEntry[i].G) -green) ;
        ld3 := Abs(Integer(paletteEntry[i].B) -blue ) ;
        if ld1 > ld2 then
          dif := ld1
        else
          dif := ld2 ;
        if ld3 > dif then
          dif := ld3 ;
        if dif < maxdif then begin
          maxdif := dif ;
          Result := i ;
        end ;
        if( maxdif = 0)  then
          break ;
      end ;

      if maxdif > 0 then begin
        if noEntries < palettesize then begin
          Result := noEntries ;
          paletteEntry[noEntries] := TGIS_Color.FromRGB(red,green,blue) ;
          inc(noEntries) ;
        end;
      end;
      lr := Result ;
    end ;

    procedure convert32_to_24bits ;
    var
      ii, kk, ss : Integer ;
      pixel : Integer ;
      idxpix : Integer ;
    begin
      for ii := 0 to _height - 1 do begin
        ss := 3*ii*_width ;
        idxpix := ii*_width ;
        for kk := 0 to _width - 1 do begin
           pixel := _pixels[idxpix +kk] ;
           convBytes[ss +3*kk +0] := Byte((pixel shr 16) and $FF) ;
           convBytes[ss +3*kk +1] := Byte((pixel shr 08) and $FF) ;
           convBytes[ss +3*kk +2] := Byte((pixel shr 00) and $FF) ;
        end ;
      end ;
    end ;

    procedure convert32_to_32bits ;
    var
      ii, kk, ss : Integer ;
      pixel : Integer ;
      idxpix : Integer ;
    begin
      for ii := 0 to _height - 1 do begin
        ss := 4*ii*_width ;
        idxpix := ii*_width ;
        for kk := 0 to _width - 1 do begin
           pixel := _pixels[idxpix +kk] ;
           convBytes[ss +4*kk +0] := Byte((pixel shr 16) and $FF) ;
           convBytes[ss +4*kk +1] := Byte((pixel shr 08) and $FF) ;
           convBytes[ss +4*kk +2] := Byte((pixel shr 00) and $FF) ;
           convBytes[ss +4*kk +3] := Byte((pixel shr 24) and $FF) ;
        end ;
      end ;
    end ;


    procedure convert32_to_8bits  ;
    var
      ii, kk, ss : Integer ;
      pixel : Integer ;
    begin
       palettesize := 256 ;
       av := palettesize div 2 ;
       lr := 0 ;

      for ii := 0 to _height - 1 do begin
        ss := ii*_width ;
        for kk := 0 to _width - 1 do begin
           pixel := _pixels[ss +kk] ;
           convBytes[ss +kk ] := Byte(find_in_palette(pixel)) ;
        end ;
      end ;
    end ;

    procedure convert32_to_4bits;
    var
      ww : Integer ;
      ii, kk, mm, ssi, sso : Integer ;
      pixel  : Integer ;
    begin
      palettesize := 16 ;
      av := palettesize div 2 ;
      lr := 0 ;
      if not isPalette then begin
        SetLength(paletteEntry, 16) ;
        isPalette := True ;
      end ;
      ww := 0 ;
      for ii := 0 to _height - 1 do begin
        sso := ii*((_width +1) div 2);
        ssi := ii*_width ;
        mm := 0 ;
        for kk := 0 to _width - 1 do begin
           pixel := _pixels[ssi +kk] ;
          if (kk and 1) = 0 then begin
            ww := (find_in_palette(pixel) shl 4)  ;
          end
          else begin
            ww := ww or find_in_palette(pixel) ;
            convBytes[sso +mm ] := Byte(ww) ;
            inc(mm) ;
            end ;
          end ;
        end ;
     end ;

     procedure convert32_to_1bit  ;
     var
       ww : Integer ;
       ii, kk, mk, mm, ssi, sso : Integer ;
       wlb : Boolean ;
       pixel : Integer ;
       pix     : Integer ;
     begin
       palettesize := 2 ;
       av := palettesize ;
       lr := 0 ;
       if not isPalette then begin
         SetLength(paletteEntry, 2) ;
         paletteEntry[0] := TGIS_Color.FromRGB(0, 0, 0) ;
         paletteEntry[1] := TGIS_Color.FromRGB($FF, $FF, $FF) ;
         noEntries := 2 ;
         isPalette := True ;
       end ;

       if (_width mod 8) = 0 then
          wlb := False
       else
         wlb := True ;

       for ii := 0 to _height - 1 do begin
         ww := 0 ;
         mm := 0 ;
         sso := ii*((_width +7) div 8);
         ssi := ii*_width ;
         for kk := 0 to _width - 1 do begin
           mk := kk and $07 ;
           pixel := _pixels[ssi +kk] ;
           pix := find_in_palette(pixel) ;
           case mk of
             0:
               if pix = 1 then
                 ww := ww or 128 ;
             1:
               if pix = 1 then
                 ww := ww or 64 ;
             2:
               if pix = 1 then
                 ww := ww or 32 ;
             3:
               if pix = 1 then
                 ww := ww or 16 ;
             4:
               if pix = 1 then
                 ww := ww or 8 ;
             5:
               if pix = 1 then
                 ww := ww or 4 ;
             6:
               if pix = 1 then
                 ww := ww or 2 ;
             else //7
             begin
               if pix = 1 then
                 ww := ww or 1 ;
               convBytes[sso +mm ] := Byte(ww) ;
               inc(mm) ;
               ww := 0 ;
             end ;
          end ;
        end ;
      end ;
    end ;

  begin
    case FPixelFormat of
        TGIS_PixelFormat.ARGB :
          begin
            if not assigned(convBytes) then begin
              SetLength(convBytes,  4*_width * _height ) ;
            end ;
            convert32_to_32bits
          end ;
        TGIS_PixelFormat.RGB :
          begin
            if not assigned(convBytes) then begin
              SetLength(convBytes,  3*_width * _height ) ;
            end ;
            convert32_to_24bits
          end ;
        TGIS_PixelFormat.Bit8  :
          begin
            if not assigned(convBytes) then begin
              SetLength(convBytes,  _width * _height ) ;
            end ;
            convert32_to_8bits ;
          end ;
        TGIS_PixelFormat.Bit4  :
          begin
            if not assigned(convBytes) then begin
              SetLength(convBytes,  ((_width +1) div 2) * _height ) ;
            end ;
            convert32_to_4bits ;
          end ;
        TGIS_PixelFormat.Bit1  :
          begin
            if not assigned(convBytes) then begin
              SetLength(convBytes,  ((_width +7) div 8) * _height ) ;
            end ;
            convert32_to_1bit ;

          end ;
        else raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPIXELFORMAT ), '', 0) ;
      end ;

    bmpheight := _height ;
    bmpwidth  := _width  ;

    if byteTileWidth = 0 then begin
        case FPixelFormat of
        TGIS_PixelFormat.ARGB  :
          byteTileWidth := 4*_width  ;
        TGIS_PixelFormat.RGB  :
          byteTileWidth := 3*_width  ;
        TGIS_PixelFormat.Bit8 : byteTileWidth := _width  ;
        TGIS_PixelFormat.Bit4 : byteTileWidth := (_width + 1) div 2  ;
        TGIS_PixelFormat.Bit1 : byteTileWidth := (_width + 7) div 8 ;
       else raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPIXELFORMAT ), '', 0) ;
      end ;

        if FPixelFormat = TGIS_PixelFormat.Bit8 then  begin
          if not isPalette then begin
            SetLength(paletteEntry, 256) ;
            isPalette := True ;
          end ;
        end
        else
       if FPixelFormat = TGIS_PixelFormat.Bit4 then  begin
          if not isPalette then begin
            SetLength(paletteEntry, 16) ;
            isPalette := True ;
          end ;
        end ;

      if compression = TGIS_CompressionType.LZW then begin
        lzwr := TGIS_FileTIFFLZWCompRecord.Create ;
        lzwr.currTile := 0 ;
        lzwr.outBuff := nil ;
        woutBuff := nil ;

        isTiled := True ;

        lzwr.tileWidth := _width  ;
        lzwr.tileLength := _height ;
        lzwr.tileRows :=  (Height +_height -1) div _height ;
        lzwr.tileColumns := (Width +_width -1) div _width ;

        if (lzwr.tileRows = 1) and (lzwr.tileColumns > 1) then begin

          exit ;
        end ;
        lzwr.numberOfTiles := lzwr.tileRows * lzwr.tileColumns ;
        SetLength( lzwr.tileOffsets, lzwr.numberOfTiles ) ;
        SetLength( lzwr.tileBytes, lzwr.numberOfTiles) ;
      end ;
    end ;

    if (compression = TGIS_CompressionType.None) or
       {$IFDEF OXYGENE}
         ((lzwr <> nil) and ((lzwr.tileRows = 1) or (lzwr.tileColumns = 1)))then begin
       {$ELSE}
         (((lzwr.tileRows = 1) or (lzwr.tileColumns = 1)))then begin
       {$ENDIF}
      if ( bmpwidth  + _x ) > FWidth  then bmpwidth  := FWidth  - _x ;
      if ( bmpheight + _y ) > FHeight then bmpheight := FHeight - _y ;
    end ;

    case FPixelFormat of
      TGIS_PixelFormat.ARGB  :
        byteRTileWidth := 4*bmpwidth  ;
      TGIS_PixelFormat.RGB  :
        byteRTileWidth := 3*bmpwidth  ;
      TGIS_PixelFormat.Bit8 : byteRTileWidth := bmpwidth  ;
      TGIS_PixelFormat.Bit4 : byteRTileWidth := (bmpwidth + 1) div 2  ;
      TGIS_PixelFormat.Bit1 : byteRTileWidth := (bmpwidth + 7) div 8 ;
      else raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPIXELFORMAT ), '', 0) ;
    end ;

    if assigned( lzwr ) then
      lzwr.finishTile := False ;

    inptr := convBytes ;
    for row := 0 to bmpheight -1 do begin
      ridx := row*byteTileWidth ;
      if row = (bmpheight -1) then begin
        if assigned( lzwr ) then
         lzwr.finishTile := True ;
      end ;
      writeLine(inptr, ridx,  _y +row, _x, byteTileWidth );
  end ;
end ;

//==================================== END =====================================
end.
