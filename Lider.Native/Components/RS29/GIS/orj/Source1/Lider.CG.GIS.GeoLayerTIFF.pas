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
  Encapsulation of a TIFF Layer.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerTIFF ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerTIFF"'}
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

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,
    System.Classes,
    System.SysUtils,
    System.Math,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoFileTIFF,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoStreams,
    Lider.CG.GIS.GeoCsFactory,
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
  GisLayerTIFF = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  {#gendoc:hide}
  // Image data type
  TGIS_BandDataType   = ( Unknown, dtU8, dtS8, dtU16, dtS16, dtU32, dtS32,
                            dtF32, dtF64 ) ;

  /// <summary>
  ///   Encapsulation of a TIFF layer.
  /// </summary>
  TGIS_LayerTIFF = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerPixel )

    private // various private variables
      /// <summary>
      ///   Number of bands in file.
      /// </summary>
      FBandsCount2 : Integer ;

      /// <summary>
      ///   Header endian description.
      /// </summary>
      headerEndian : Word ;

      /// <summary>
      ///   Header sentinel value.
      /// </summary>
      headerConfirm : Word ;

      /// <summary>
      ///   Header Image File Directory value..
      /// </summary>
      headerFirstIFD : Cardinal ;

      /// <summary>
      ///   Header BigTIFF Image File Directory value..
      /// </summary>
      headerFirstIFD64 : Int64 ;

      /// <summary>
      ///   Header BigTIFF Image offset size (in bytes = 8).
      /// </summary>
      headerBigTIFFOffsetsSize : Word ;

      /// <summary>
      ///   Header BigTIFF Image offset size ( = 0).
      /// </summary>
      headerBigTIFFConstant : Word ;

      /// <summary>
      ///   Image validation.
      /// </summary>
      isValid : Boolean;

      /// <summary>
      ///   CMYK image
      /// </summary>
      isCMYK : Boolean ;

      /// <summary>
      ///   CCITT group 4 fax compression, CCITT Huffman RLE, LZW, JPEG or
      ///   PackBits is used
      /// </summary>
      isCompressed : Boolean ;

      /// <summary>
      ///   LZW, ZLIB compression with horizontal differencing.
      /// </summary>
      hDifferencing : Boolean ;

      /// <summary>
      ///   LZW, ZLIB compression with floating point horizontal differencing.
      /// </summary>
      hFDifferencing : Boolean ;

      /// <summary>
      ///   TIFF file data with sign.
      /// </summary>
      floatDataType : Boolean ;

      /// <summary>
      ///   Used in CCITT compression (see FillOrder Tag = 266 (10A.H) in
      ///   TIFF6.0 specification.
      /// </summary>
      fillOrder : Word ;

      /// <summary>
      ///   Compression type
      /// </summary>
      compressionType : TGIS_CompressionType ;

      /// <summary>
      ///   Number of tags in Tiff file
      /// </summary>
      countTag : Word;

      /// <summary>
      ///   No data color is defined.
      /// </summary>
      valueNoDataGDAL : Integer ;

      /// <summary>
      ///   Image with extra samples if &gt; 0 (if alphaAssociated then =2, 1
      ///   for transparency.
      /// </summary>
      extraSamples   : Integer ;

      /// <summary>
      ///   Last used in setZoom function
      /// </summary>
      actualZoom : Double ;

      /// <summary>
      ///   Width of a layer in pixels - for actual page.
      /// </summary>
      actualBitWidth : Integer ;

      /// <summary>
      ///   Width of a layer in pixels - for actual page.
      /// </summary>
      actualBitHeight : Integer ;

      /// <summary>
      ///   Number of subviwes.
      /// </summary>
      subViewsCount : Integer ;

      /// <summary>
      ///   Validation table
      /// </summary>
      validPagesTable : Array of Boolean ;

      /// <summary>
      ///   Scales of pages.
      /// </summary>
      pagesScales : Array of Double ;

      /// <summary>
      ///   Numbers of pages associates with pagesScales array.
      /// </summary>
      associatesPagesNo : Array of Word ;

      /// <summary>
      ///   Associates offsets.
      /// </summary>
      associatesIFDOffset64 : Array of Int64 ;

      /// <summary>
      ///   Parameters setting mode
      /// </summary>
      inSetUp : Boolean ;

      /// <summary>
      ///   List of Tiff tags.  ? listTag : Array of TGIS_FileTIFF_Tag ;
      /// </summary>
      listTag64 : Array of TGIS_FileTIFF_Tag64 ;

      /// <summary>
      ///   In memory list of image strips in Tiff file.
      /// </summary>
      listStripInfo64 : {$IFDEF OXYGENE} array of TGIS_FileTIFF_StripInfo64
                        {$ELSE} TArray<TGIS_FileTIFF_StripInfo64>
                        {$ENDIF} ;

      /// <summary>
      ///   Number of image strips in Tiff file.
      /// </summary>
      countStripInfo : Integer ;

      /// <summary>
      ///   Bitmap overlay file stream (with sub views).
      /// </summary>
      fileStreamOvr : TGIS_HandleStream ;

      /// <summary>
      ///   For overlay file True when BigTIFF detected
      /// </summary>
      isBigTIFFOvr : Boolean ;

      /// <summary>
      ///   True when BigTIFF detected for main TIFF file
      /// </summary>
      isBigTIFFMain : Boolean ;

      /// <summary>
      ///   Bitmap temporary unused stream.
      /// </summary>
      fileStreamMain : TGIS_HandleStream ;

      /// <summary>
      ///   Number of first overlad page.
      /// </summary>
      ovrFirstPage : Integer ;

      /// <summary>
      ///   Number of rows in each strip in Tiff file.
      /// </summary>
      rowsPerStrip : Integer;

      /// <summary>
      ///   Decoder for compressed TIFF
      /// </summary>
      fileDecoder : Array of TGIS_FileTIFFDecoder ;

      /// <summary>
      ///   Decoder for compressed TIFF
      /// </summary>
      decodeState : Array of TGIS_FileTIFFDecodeState ;

      /// <summary>
      ///   Image organize into tiles
      /// </summary>
      isTiffTiled : Boolean ;

      /// <summary>
      ///   Tile width
      /// </summary>
      tileWidth : Integer ;

      /// <summary>
      ///   Photometric interpretation
      /// </summary>
      phti : Integer ;

      /// <summary>
      /// If extra setting for jpeg compression is needed
      /// </summary>
      callJpegProc : Boolean ;

      /// <summary>
      ///   Tiles length
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
      ///   Tile offsets
      /// </summary>
      tileOffsets : Int64 ;

      /// <summary>
      ///   Tiles number
      /// </summary>
      tilesPerImage : Integer ;

      /// <summary>
      ///   Bits per pixel
      /// </summary>
      bitscountinfo    : Integer ;

      /// <summary>
      ///   JPEG quantization and (or) Huffman tables
      /// </summary>
      jpegTables : TGIS_JPEGTable ;

      /// <summary>
      ///   YCbCr, BGR or RGB compressed
      /// </summary>
      componentsJpeg : Integer ;

      /// <summary>
      ///   Zoom for jpeg decoder
      /// </summary>
      jpegZoom : Integer ;

      /// <summary>
      ///   Horizontal subsampling
      /// </summary>
      subSampleHorizYCbCr : Word ;

      /// <summary>
      ///   Horizontal subsampling
      /// </summary>
      subSampleVertYCbCr  :  Word ;

      /// <summary>
      ///   True when BigTIFF detected
      /// </summary>
      isBigTIFF   : Boolean ;

      /// <summary>
      ///  Number of bytes perpixel for grid images
      /// </summary>
      pixelBytesNo : Integer ;

      /// <summary>
      ///  Type of data in band
      /// </summary>
      bandDataType : TGIS_BandDataType ;

      /// <summary>
      ///   line buffer (line of shorts).
      /// </summary>
      lineShBuffer : array of SmallInt ;

      /// <summary>
      ///   line buffer (line of integer).
      /// </summary>
      lineIntBuffer : array of Integer ;

      /// <summary>
      ///   line buffer (line of words).
      /// </summary>
      lineWBuffer    : array of Word  ;

      /// <summary>
      ///   Not a grid file but served like a grid
      /// </summary>
      likeGrid : Boolean ;

      /// <summary>
      ///   Signed 16 bits grid
      /// </summary>
      signed16Bits : Boolean ;

      /// <summary>
      ///   Signed 32 bits integer grid
      /// </summary>
      signed32Bits : Boolean ;

      /// <summary>
      ///   line buffer (line of singles).
      /// </summary>
      lineDBuffer : array of Double ;

      /// <summary>
      ///   Number of lines present in lineBuffer.
      /// </summary>
      lineInBuffer  : Integer ;

      /// <summary>
      ///   Must set min and max Z value
      /// </summary>
      mustPrepareMinMaxZ : Boolean ;

      /// <summary>
      ///   Image orientation
      /// </summary>
      imageOrientation : SmallInt ;

    protected // properties internal values

      /// <summary>
      ///   Additional textual information about GeoTIFF.
      /// </summary>
      FFileInfoGeoTIFF : String ;

      /// <summary>
      ///   Information about used Model and Maker of GeoTIFF.
      /// </summary>
      FMakerModel : String ;

      /// <summary>
      ///   Name and version number of the software package(s) used to
      ///   create the image
      /// </summary>
      FSoftware : String ;

    private // various private routines

      /// <summary>
      ///   Internal use only.
      ///   Reading image line.
      /// </summary>
      procedure readColTabFromFile( const _counter : Integer ;
                                    const _fileOffset : Int64
                                  ) ;

      /// <summary>
      ///   Reading an image file directory element.
      /// </summary>
      /// <param name="_fileOffset">
      ///   offset to IFD in the file
      /// </param>
      procedure loadIFD           ( const _fileOffset : Int64) ;

      /// <summary>
      ///   Inits TIFF file decoder.
      /// </summary>
      procedure initDecoder ;

      /// <summary>
      ///   Inits TIFF jpegTable (.
      /// </summary>
      procedure jpegProc ;

      /// <summary>
      ///   Reading an image file directory element.
      /// </summary>
      /// <param name="_tag">
      ///   tag identifier
      /// </param>
      /// <returns>
      ///   tag pointer; -1 if tag does not exits
      /// </returns>
      function  getTagTiff        ( const _tag  : Word    ) : Integer ;

      /// <summary>
      ///   Sets initial bands description
      /// </summary>
      procedure  setTiffBandsDefinition  ;

      /// <summary>
      ///   Sets user rdefined bands description
      /// </summary>
      procedure  forceTiffBandsDefinition  ;

      /// <summary>
      ///   Sets layer params from tiff tags
      /// </summary>
      /// <returns>
      ///   False when wrong parameter is detected
      /// </returns>
      function interpretTiffTags : Boolean ;

      /// <summary>
      ///   Setting image width or actual wdth only for subview (scaled image)
      /// </summary>
      /// <param name="_tidx">
      ///   Index to tags table
      /// </param>
      procedure  setImageWidth      ( const _tidx : Integer ) ;

      /// <summary>
      ///   Setting image height or actual height only for subview (scaled image)
      /// </summary>
      /// <param name="_tidx">
      ///   Index to tags table
      /// </param>
      procedure  setImageHeight      ( const _tidx : Integer ) ;

      /// <summary>
      ///   Setting compression type
      /// </summary>
      /// <param name="_tidx">
      ///   Index to tags table
      /// </param>
      procedure  setCompression      ( const _tidx : Integer ) ;

      /// <summary>
      ///   Setting total count bits perpixel
      /// </summary>
      /// <param name="_tidx">
      ///   Index to tags table
      /// </param>
      procedure  setBPP       ( const _tidx : Integer ) ;

      /// <summary>
      ///   Setting
      /// </summary>
      /// <param name="_tidx">
      ///   Index to tags table
      /// </param>
      procedure  setPhotometric      ( const _tidx : Integer ) ;


      /// <summary>
      ///   Setting
      /// </summary>
      /// <param name="_tidx">
      ///   Index to tags table
      /// </param>
      procedure  setMinValue      ( const _tidx : Integer ) ;

      /// <summary>
      ///   Setting
      /// </summary>
      /// <param name="_tidx">
      ///   Index to tags table
      /// </param>
      procedure  setMaxValue      ( const _tidx : Integer ) ;

      /// <summary>
      ///   Setting
      /// </summary>
      /// <param name="_tidx">
      ///   Index to tags table
      /// </param>
      procedure  setExtraSample      ( const _tidx : Integer ) ;

      /// <summary>
      ///   Setting total count bits perpixel
      /// </summary>
      /// <param name="_tidx">
      ///   Index to tags table
      /// </param>
      procedure  setSampleFormat  ( const _tidx : Integer ) ;

      /// <summary>
      ///   Setting
      /// </summary>
      /// <param name="_tidx">
      ///   Index to tags table
      /// </param>
      procedure  setJPEGTables      ( const _tidx : Integer ) ;

      /// <summary>
      ///   Setting
      /// </summary>
      /// <param name="_tidx">
      ///   Index to tags table
      /// </param>
      procedure  setJPEGSubSampling    ( const _tidx : Integer ) ;

      /// <summary>
      ///   Setting nodata params (value or color)
      /// </summary>
      /// <param name="_tidx">
      ///   Index to tags table
      /// </param>
      procedure  setNoDataParams   ( const _tidx : Integer ) ;

      /// <summary>
      ///   Reading an image file directory ascii element.
      /// </summary>
      /// <param name="_ptag">
      ///   tag pointer as obtained from getTagTiff
      /// </param>
      /// <returns>
      ///   string repreaentation of asciii tag
      /// </returns>
      function  getTagAscii       ( const _ptag : Integer ) : String ;

      /// <summary>
      ///   Find optimal divider to converting 48 bits per pixel data to 24bits
      ///   data.
      /// </summary>
      function  getWordDivider    : Double ;

      /// <summary>
      ///   Find optimal divider to converting 48 bits per pixel data to 24bits
      ///   data.
      /// </summary>
      function  getWordShift    : Integer ;

      /// <summary>
      ///   Check needed shift for data bands greter than 8 bits
      /// </summary>
      procedure  checkWordShift ;

      /// <summary>
      ///   Check needed shift for data bands greter than 8 bits
      /// </summary>
      procedure  applyGDALTransparency
                                   ( const _buffer    : TGIS_Pixels ;
                                     const _offset : Integer ;
                                     const _pixCount  : Integer
                                   ) ;

      /// <summary>
      ///   Reading image line.
      /// </summary>
      /// <param name="_buffer">
      ///   pointer
      /// </param>
      /// <param name="_offset">
      ///   line start offset
      /// </param>
      /// <param name="_linenr">
      ///   line number
      /// </param>
      /// <param name="_start">
      ///   left margin (bytes to skip)
      /// </param>
      /// <param name="_bytes">
      ///   bytes count
      /// </param>
      /// <param name="_tilenr">
      ///   tile number
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FILEREAD
      /// </exception>
      function  readCompressedLineOnFly(
                                    const _buffer : TBytes  ;
                                    const _offset : Integer ;
                                    const _linenr : Integer ;
                                    const _start  : Integer ;
                                    const _bytes  : Integer ;
                                    const _tilenr : Integer = -1
                                  ) : Integer;

      /// <summary>
      ///   Makes a general layer setup for given page.
      /// </summary>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FILEREAD
      /// </exception>
      procedure choosePage        ( const _page : Integer = 1 ) ;

      /// <summary>
      ///   Frees memory related with current page.
      /// </summary>
      procedure closeCurrentPage  ;

      /// <summary>
      ///   Find number of pages in image file.
      /// </summary>
      /// <param name="_stream">
      ///   Actual image file (possible using ovr file)
      /// </param>
      function  getPagesCount    (const _stream : TGIS_HandleStream
                                  )   : Integer ;

      /// <summary>
      ///   Inits TIFF file decoder.
      /// </summary>
      function  getPagesScales    : Boolean ;

    // various protected routines
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <inheritdoc/>
      procedure prepareAltitudeMapTable ; override;

      /// <inheritdoc/>
      function  setFileScale      ( const _dwidth : Double ;
                                    const _swidth : Double
                                  ) : Double ; override;
      /// <inheritdoc/>
      function  importPixelData   ( const _layer       : TGIS_LayerPixel
                                  ) : Boolean ; override;

      /// <inheritdoc/>
      procedure setPageInternal   ( const _value : Integer
                                  ) ;

      /// <summary>
      ///   Checks if TIFF image or Big TIFF image.
      /// </summary>
      /// <param name="_stream">
      ///   Actual image file (possible using ovr file)
      /// </param>
      procedure checkTiff         (const _stream : TGIS_HandleStream
                                  )  ;

      /// <inheritdoc/>
      procedure setUp             ; override;

      /// <inheritdoc/>
      procedure setupParams       ; override;

      /// <inheritdoc/>
      procedure setPage        ( const _value : Integer
                               ) ; override;

      /// <summary>
      ///   reads GEO-TIFF image parameters
      /// </summary>
      procedure readGeoTiffCS     ;

      /// <summary>
      ///   Convert from big endian to little endian format.
      /// </summary>
      /// <param name="_buffer">
      ///   Bytes buffer
      /// </param>
      /// <param name="_varsize">
      ///   size in bytes of portion to convert (2, 4 or 8)
      /// </param>
      procedure convertLineToLE  ( const _buffer  : TBytes  ;
                                   const _varsize : Word
                                 ) ;

      /// <inheritdoc/>
      function  getLinePixels     ( const _buffer   : TGIS_Pixels  ;
                                    const _offset   : Integer ;
                                    const _linenr   : Integer ;
                                    const _pixStart : Integer ;
                                    const _pixCount : Integer
                                  ) : Integer; override;


      /// <inheritdoc/>
      function  getLine           ( const _buffer : TBytes  ;
                                    const _offset : Integer ;
                                    const _linenr : Integer ;
                                    const _start  : Integer ;
                                    const _bytes  : Integer
                                  ) : Integer; override;

      /// <inheritdoc/>
      function  getLineBits      ( const _buffer   : TBytes  ;
                                   const _offset   : Integer ;
                                   const _linenr   : Integer ;
                                   const _pixStart : Integer ;
                                   const _pixCount : Integer
                                 ) : Integer ; override;

      /// <summary>
      ///   Internal use only. For reading an image line data when
      ///   isPlanar1 = Fale.
      /// </summary>
      /// <returns>
      ///   number of read bytes
      /// </returns>
      /// <param name="_buffer">
      ///   bytes array
      /// </param>
      /// <param name="_offset">
      ///   start in _buffer
      /// </param>
      /// <param name="_linenr">
      ///   line number
      /// </param>
      /// <param name="_pixStart">
      ///   left margin (pixels to skip)
      /// </param>
      /// <param name="_pixCount">
      ///   number of image pixels to read
      /// </param>
      function  getLineBits2     ( const _buffer   : TBytes  ;
                                   const _offset   : Integer ;
                                   const _linenr   : Integer ;
                                   const _pixStart : Integer ;
                                   const _pixCount : Integer
                                 ) : Integer ;


      /// <inheritdoc/>
      function  convert1BtoPixels ( const _buffSrc    : TBytes  ;
                                    const _srcOffset  : Integer ;
                                    const _buffDst    : TGIS_Pixels ;
                                    const _dstOffset  : Integer ;
                                    const _pixStart   : Integer ;
                                    const _pixCount   : Integer
                                  ) : Integer ;
      /// <inheritdoc/>
      function  convert4BtoPixels ( const _buffSrc  : TBytes  ;
                                    const _srcOffset : Integer ;
                                    const _buffDst   : TGIS_Pixels ;
                                    const _dstOffset : Integer ;
                                    const _pixStart   : Integer ;
                                    const _pixCount  : Integer
                                  ) : Integer ;

      /// <inheritdoc/>
      function  convert12BtoPixels  ( const _buffSrc  : TBytes  ;
                                      const _srcOffset : Integer ;
                                      const _buffDst   : TGIS_Pixels ;
                                      const _dstOffset : Integer ;
                                      const _pixStart   : Integer ;
                                      const _pixCount  : Integer
                                    ) : Integer ;

      /// <inheritdoc/>
      function  convert36BtoPixels  ( const _buffSrc  : TBytes  ;
                                      const _srcOffset : Integer ;
                                      const _buffDst   : TGIS_Pixels ;
                                      const _dstOffset : Integer ;
                                      const _pixStart   : Integer ;
                                      const _pixCount  : Integer
                                    ) : Integer ;

      /// <inheritdoc/>
      function  convert8BtoPixels  ( const _buffSrc  : TBytes  ;
                                     const _srcOffset : Integer ;
                                     const _buffDst   : TGIS_Pixels ;
                                     const _dstOffset : Integer ;
                                     const _pixStart   : Integer ;
                                     const _pixCount  : Integer
                                   ) : Integer ;
      /// <inheritdoc/>
      function  convert16BtoPixels ( const _buffSrc  : TBytes  ;
                                     const _srcOffset : Integer ;
                                     const _buffDst   : TGIS_Pixels ;
                                     const _dstOffset : Integer ;
                                     const _pixStart   : Integer ;
                                     const _pixCount  : Integer
                                  ) : Integer ;

      /// <inheritdoc/>
      function  convertN16BtoPixels ( const _buffSrc  : TBytes  ;
                                      const _srcOffset : Integer ;
                                      const _buffDst   : TGIS_Pixels ;
                                      const _dstOffset : Integer ;
                                      const _pixStart   : Integer ;
                                      const _pixCount  : Integer
                                    ) : Integer ;

      /// <inheritdoc/>
      function  convertComplextoPixels
                                   ( const _buffSrc  : TBytes  ;
                                     const _srcOffset : Integer ;
                                     const _buffDst   : TGIS_Pixels ;
                                     const _dstOffset : Integer ;
                                     const _pixStart   : Integer ;
                                     const _pixCount  : Integer
                                   ) : Integer ;

      /// <inheritdoc/>
      function  convertBytesToPixels
                                   ( const _buffSrc  : TBytes  ;
                                     const _srcOffset : Integer ;
                                     const _buffDst   : TGIS_Pixels ;
                                     const _dstOffset : Integer ;
                                     const _pixStart   : Integer ;
                                     const _pixCount  : Integer
                                   ) : Integer ;

      /// <inheritdoc/>
      function  convertBitsStringToBytes
                                   ( const _buffSrc   : TBytes  ;
                                     const _srcOffset : Integer ;
                                     const _buffDst   : TBytes ;
                                     const _dstOffset : Integer ;
                                     const _pixStart   : Integer ;
                                     const _pixCount  : Integer
                                  ) : Integer ;


      /// <inheritdoc/>
      function  convertFloatToPixels
                                   ( const _buffDst   : TGIS_Pixels ;
                                     const _dstOffset : Integer ;
                                     const _pixLine   : Integer ;
                                     const _pixStart   : Integer ;
                                     const _pixCount  : Integer
                                   ) : Integer ;



      /// <inheritdoc/>
      function  convert24BtoPixels ( const _buffSrc  : TBytes  ;
                                     const _srcOffset : Integer ;
                                     const _buffDst   : TGIS_Pixels ;
                                     const _dstOffset : Integer ;
                                     const _pixStart   : Integer ;
                                     const _pixCount  : Integer
                                  ) : Integer ;


      /// <inheritdoc/>
      function  convert32BtoPixels ( const _buffSrc  : TBytes  ;
                                     const _srcOffset : Integer ;
                                     const _buffDst   : TGIS_Pixels ;
                                     const _dstOffset : Integer ;
                                     const _pixStart   : Integer ;
                                     const _pixCount  : Integer
                                  ) : Integer ;



      /// <inheritdoc/>
      function  getAlphaLine      ( const _buffer : TBytes  ;
                                    const _offset : Integer ;
                                    const _linenr : Integer ;
                                    const _start  : Integer ;
                                    const _bytes  : Integer
                                  ) : Integer; override;

      /// <summary>
      ///   Reading an image line.
      /// </summary>
      /// <returns>
      ///   number of read bytes
      /// </returns>
      /// <param name="_buffer">
      ///   pointer
      /// </param>
      /// <param name="_offset">
      ///   line offset
      /// </param>
      /// <param name="_linenr">
      ///   line number
      /// </param>
      /// <param name="_start">
      ///   left margin (bytes to skip)
      /// </param>
      /// <param name="_bytes">
      ///   bytes count
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FILEREAD
      /// </exception>
      function  getLine2          ( const _buffer : TBytes  ;
                                    const _offset : Integer ;
                                    const _linenr : Integer ;
                                    const _start  : Integer ;
                                    const _bytes  : Integer
                                  ) : Integer;


      /// <summary>
      ///   Reading an image line bytes for planar 2 configuration.
      /// </summary>
      /// <returns>
      ///   number of read bytes
      /// </returns>
      /// <param name="_buffer">
      ///   pointer
      /// </param>
      /// <param name="_linenr">
      ///   line number
      /// </param>
      /// <param name="_startIdx">
      ///   left margin (pixel to skip)
      /// </param>
      /// <param name="_count">
      ///   pixels count
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FILEREAD
      /// </exception>
      function  getNativeLine2Bytes
                                 ( const _buffer   : TBytes  ;
                                   const _linenr : Integer ;
                                   const _startIdx : Integer ;
                                   const _count    : Integer
                                 ) : Integer;


      /// <summary>
      ///   Reading an image line.
      /// </summary>
      /// <returns>
      ///   number of read bytes
      /// </returns>
      /// <param name="_buffer">
      ///   pointer
      /// </param>
      /// <param name="_offset">
      ///   line offset
      /// </param>
      /// <param name="_linenr">
      ///   line number
      /// </param>
      /// <param name="_start">
      ///   left margin (bytes to skip)
      /// </param>
      /// <param name="_bytes">
      ///   bytes count
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FILEREAD
      /// </exception>
      function  getGridAsARGBLine ( const _buffer : TBytes  ;
                                    const _offset : Integer ;
                                    const _linenr : Integer ;
                                    const _start  : Integer ;
                                    const _bytes  : Integer
                                  ) : Integer;

      /// <inheritdoc/>
      procedure prepareMinMaxZ    ( const _zoom : Double = -1
                                  ) ; override ;

      /// <inheritdoc/>
      function  getNativeValue    ( const _pt : TPoint  ;
                                    const _ar : TGIS_DoubleArray
                                  ) : Boolean ; override;

      /// <inheritdoc/>
      function  getNativeLine     ( const _buffer   : TGIS_SingleArray ;
                                    const _linenr   : Integer          ;
                                    const _startIdx : Integer          ;
                                    const _count    : Integer
                                  ) : Integer ; override;

      /// <summary>
      ///   Internal use only.
      /// </summary>
      /// <returns>
      ///   number of read bytes
      /// </returns>
      /// <param name="_buffer">
      ///   predefined buffer of singles
      /// </param>
      /// <param name="_linenr">
      ///   number of needed grid line
      /// </param>
      /// <param name="_startIdx">
      ///   left margin (pixels to skip)
      /// </param>
      /// <param name="_count">
      ///   pixel count
      /// </param>
      function  getNativeLine2    ( const _buffer   : TGIS_SingleArray ;
                                    const _linenr   : Integer          ;
                                    const _startIdx : Integer          ;
                                    const _count    : Integer
                                  ) : Integer ;


      /// <inheritdoc/>
      procedure setUpInternal     ; override;

     /// <summary>
     ///   Reading 48 bits per pixel line or grid 16, 32-bits.
     /// </summary>
     /// <returns>
     ///   number of read bytes
     /// </returns>
     /// <param name="_buffer">
     ///   predefined buffer of singles
     /// </param>
     /// <param name="_linenr">
     ///   number of needed grid line
     /// </param>
     /// <param name="_startIdx">
     ///   left margin (pixels to skip)
     /// </param>
     /// <param name="_count">
     ///   pixel count
     /// </param>
     function  getNativeLine48  (   const   _buffer   : TGIS_SingleArray ;
                                    const _linenr   : Integer          ;
                                    const _startIdx : Integer          ;
                                    const _count    : Integer
                                  ) : Integer ;

     /// <inheritdoc/>
     function fget_Page       : Integer ; override;

     /// <inheritdoc/>
     function  fget_MinHeight      : Single ; override;

     /// <inheritdoc/>
     function  fget_MaxHeight      : Single ; override;

     /// <inheritdoc/>
     function  fget_Capabilities   : TGIS_LayerPixelSubFormatList ; override;

    protected

      /// <inheritdoc/>
      procedure doDestroy         ; override;


    public // API

      /// <inheritdoc/>
      constructor Create          ; override;

      /// <inheritdoc/>
      procedure ReOpen            ; override;

      /// <inheritdoc/>
      procedure Build          (  const _path      : String         ;
                                  const _grid      : Boolean        ;
                                  const _cs        : TGIS_CSCoordinateSystem  ;
                                  const _ext       : TGIS_Extent    ;
                                  const _width     : Integer        ;
                                  const _height    : Integer ;
                                  const _subformat : TGIS_LayerPixelSubFormat
                                ) ; override;

      /// <inheritdoc/>
      procedure SaveData   ; override;

      /// <inheritdoc/>
      function    PreRecognize     ( const _path     : String ;
                                       var _new_path : String
                                   ) : Boolean ; override;
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

{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoParams,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoStatistics,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoFileJPEG,
    Lider.CG.GIS.GeoRegistredLayers,
    Lider.CG.GIS.GeoCsBase,
    Lider.CG.GIS.GeoCsProjections ;
{$ENDIF}

{$IFNDEF OXYGENE}
type
    PDWord = ^DWord ;
{$ENDIF}

//==============================================================================
// TGIS_LayerTIFF
//==============================================================================

  constructor TGIS_LayerTIFF.Create ;
  begin
    inherited ;
    wordDivider := 1.0 ;
    jpegZoom := 1 ;
    isValid := False ;
    FSubType := FSubType +  [ TGIS_LayerSubType.Persistent,
                              TGIS_LayerSubType.Exportable ] ;
  end ;

  /// <inheritdoc/>
  procedure TGIS_LayerTIFF.ReOpen ;
  begin
    jpegTables            := nil   ;
    associatesIFDOffset64 := nil   ;
    validPagesTable       := nil   ;
    pagesScales           := nil   ;
    associatesPagesNo     := nil   ;
    listTag64             := nil   ;
    FIsNativeGridImage    := False ;

    inherited ;
  end;

  procedure TGIS_LayerTIFF.doDestroy ;
  begin
    Dormant ;
    if assigned(fileStreamOvr) then begin
      FreeObject(fileStreamOvr) ;
      fileStream := fileStreamOvr ;
    end;
    if assigned(fileStreamMain) then begin
      FreeObject(fileStreamMain) ;
      fileStream := fileStreamMain ;
    end;
    if assigned(fileStream) then
      FreeObject(fileStream) ;

    jpegTables            := nil ;
    associatesIFDOffset64 := nil ;
    validPagesTable       := nil ;
    pagesScales           := nil ;
    associatesPagesNo     := nil ;

    inherited ;
  end ;

  procedure TGIS_LayerTIFF.Build(
    const _path      : String         ;
    const _grid      : Boolean        ;
    const _cs        : TGIS_CSCoordinateSystem  ;
    const _ext       : TGIS_Extent    ;
    const _width     : Integer        ;
    const _height    : Integer ;
    const _subformat : TGIS_LayerPixelSubFormat
  ) ;
  begin
    if SafeFileExists( _path ) then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEEXIST ), _path, 0 ) ;
    end ;

    inherited Build(_path, _grid, _cs, _ext, _width, _height, _subformat) ;
    isPartialTransparent := True ;
    isPlanar1 := True ;

    if (actualBitHeight = 0) and (actualBitWidth = 0) then begin
      actualBitHeight := FBitHeight ;
      actualBitWidth  := FBitWidth ;
    end;

    FIsOpened := True ;
  end ;

  procedure TGIS_LayerTIFF.SaveData ;
  var
    i, k      : Integer ;
    cw, ch    : Integer ;
    lw, lh    : Integer ;
    maxc,maxr : Integer ;
    f         : TGIS_FileTIFF ;
    pix       : TGIS_Pixels ;
    grid      : TGIS_GridArray ;
    ext       : TGIS_Extent {$IFDEF GIS_NORECORDS} = new TGIS_Extent{$ENDIF} ;
    sx, sy    : Double ;
    pixformat : TGIS_PixelFormat ;
    istr      : Boolean ;
    trcolor   : Integer ;
    lsf       : TGIS_LayerPixelSubFormat ;
    comp      : TGIS_CompressionType ;
    pf        : TGIS_PixelFormat ;
    tmpname   : String ;
  const
    MAX_CELL_WH = 512 ;
    procedure set_transparent ;
      var
        ii : Integer ;
    begin
      for ii := low(pix) to high(pix) do
        if (pix[ii] and $00FFFFFF) = trcolor then
          pix[ii] := 0 ;
    end;
  begin
    if IsStringEmpty( Path ) then exit ;

    if not MustSave then exit ;
    if not isBuilt  then begin

      if bitsPerPixel <= 8 then
        pf := TGIS_PixelFormat.Bit8
      else
      if bitsPerPixel > 24 then
        pf := TGIS_PixelFormat.RGB
      else
        pf := TGIS_PixelFormat.ARGB ;

      if compressionType <> TGIS_CompressionType.None then
        comp := TGIS_CompressionType.LZW
      else
        comp := TGIS_CompressionType.None ;

      if FIsNativeGridImage then
        lsf := TGIS_LayerPixelSubFormat.Create( TGIS_PixelFormat.Custom, False, TGIS_PixelSubFormat.GRID, comp, 1 )
      else
        lsf := TGIS_LayerPixelSubFormat.Create( TGIS_PixelFormat.RGB, False, TGIS_PixelSubFormat.None, comp, 1 ) ;

        tmpname := GetFilePath(Path) +'2' +GetFileName(Path) ;
       f := TGIS_FileTIFF.Create(
            tmpname,
            FExtent ,
            FBitWidth,
            FBitHeight,
            lsf,
            96,
            CS
          ) ;
    end
    else begin

      tmpname := '' ;
      f := TGIS_FileTIFF.Create(
            Path,
            FExtent ,
            FBitWidth,
            FBitHeight,
            FSubFormat,
            96,
            CS
          ) ;
    end;
    try
      if not assigned(f) then exit ;

      if (FBitHeight <= MAX_CELL_WH) or (FBitWidth <= MAX_CELL_WH) then
        ch := FBitHeight
      else
        ch := MAX_CELL_WH ;

      if (FBitWidth <= MAX_CELL_WH) or (FBitHeight <= MAX_CELL_WH) then
        cw := FBitWidth
      else
        cw := MAX_CELL_WH ;

      maxc := FBitWidth div cw ;
      lw := FBitWidth mod cw ;
      if lw > 0 then begin
        lw := 0 ;
        inc(maxc) ;
      end;
      maxr := FBitHeight div ch ;
      lh := FBitHeight mod ch ;
      if lh > 0 then begin
        inc(maxr) ;
      end;

      sx := (FExtent.XMax -FExtent.XMin)/FBitWidth  ;
      sy := (FExtent.YMax -FExtent.YMin)/FBitHeight ;

      if maxc = 0 then begin
        maxc := 1 ;
        cw := lw ;
      end;
      try
         if f.PixelFormat = TGIS_PixelFormat.Custom then begin
          {$IFDEF OXYGENE}
            grid := new array of Single[ch] ;
            var ii : Integer ;
            for ii := 0 to ch-1 do
              grid[ii] := new Single[cw] ;
          {$ELSE}
            SetLength( grid, ch, cw ) ;
          {$ENDIF}

          for i := 0 to maxr -1 do begin
            ext.YMax := FExtent.YMax -i*(ch*sy) ;
            ext.YMin := ext.YMax -ch*sy ;
            for k := 0 to maxc -1 do begin
              ext.XMin := FExtent.XMin +k*(cw*sx) ;
              ext.XMax := ext.XMin +cw*sx ;
              setNoDataTable( grid ) ;
              getGridData(ext, grid) ;
              f.WriteGrid(k*cw, i*ch, grid );
            end;
          end ;
        end
        else begin
          pixformat := f.PixelFormat ;
          istr := False ;
          if pixformat = TGIS_PixelFormat.ARGB then begin
            trcolor := Integer(NoDataColor.ARGB) and $00FFFFFF ;
            if trcolor <> 0 then
              istr := True
            else
            if (Integer(NoDataColor.ARGB) and Integer($FF000000)) <> 0 then
              istr := True ;
          end ;


          SetLength(pix, cw*ch) ;
          for i := 0 to maxr -1 do begin
            ext.YMax := FExtent.YMax -i*(ch*sy) ;
            ext.YMin := ext.YMax -ch*sy ;
            for k := 0 to maxc -1 do begin
              ext.XMin := FExtent.XMin +k*(cw*sx) ;
              ext.XMax := ext.XMin +cw*sx ;
              setBmpTransparent( pix ) ;
              getBitmapData(ext, pix, cw, ch) ;
              if istr then
                set_transparent ;
              f.Write(k*cw, i*ch, pix, pixformat, cw, ch );
            end;
          end ;
        end;
      finally
         if f.PixelFormat = TGIS_PixelFormat.Custom then
          grid := nil
        else
          pix := nil ;
      end ;
    finally
      FreeObject(f) ;
      if tmpname <> '' then
        replaceWorkingFiles(tmpname) ;
    end;
  end;

  function TGIS_LayerTIFF.PreRecognize(
    const _path     : String ;
      var _new_path : String
  ) : Boolean ;
  begin
    Result := inherited PreRecognize( _path, _new_path ) ;
    if not SafeFileExists(_path) then
      exit ;

    fileStream := openBufferedFileStream( _path ) ;
    if not assigned(fileStream) then begin
      Result := False ;
      exit ;
    end;

    checkTiff(fileStream) ;
   // Header is a valid Tiff Header
    if not isValid then begin
      Result := False ;
      exit ;
    end ;

    if bigEndian then
      headerFirstIFD := SwapLongInt( headerFirstIFD ) ;

    FPagesCount := getPagesCount(fileStream) ;
    loadIFD( associatesIFDOffset64[0] ) ;
    interpretTiffTags ;
    Result := Result and isValid ;
    Dormant ;
    jpegTables            := nil ;
    associatesIFDOffset64 := nil ;
    validPagesTable       := nil ;
    pagesScales           := nil ;
    associatesPagesNo     := nil ;

  end ;



  function TGIS_LayerTIFF.importPixelData   ( const _layer : TGIS_LayerPixel
                                            ) : Boolean ;
  var
    i, k      : Integer ;
    cw, ch    : Integer ;
    lw, lh    : Integer ;
    maxc,maxr : Integer ;
    f         : TGIS_FileTIFF ;
    pix       : TGIS_Pixels ;
    grid      : TGIS_GridArray ;
    ext       : TGIS_Extent {$IFDEF GIS_NORECORDS} = new TGIS_Extent{$ENDIF} ;
    sx, sy    : Double ;
    pixformat : TGIS_PixelFormat ;
    abrt      : Boolean ;
    cnt       : Integer ;
    pos       : Integer ;
  const
    MAX_CELL_WH = 1024 ;
  begin
    Result := False ;
    if IsStringEmpty( Path ) then exit ;

    f := TGIS_FileTIFF.Create(
            Path,
            FExtent ,
            FBitWidth,
            FBitHeight,
            FSubFormat,
            96,
            CS
          ) ;

    if assigned( FOnBusy ) then
      {$IFDEF OXYGENE}
        FOnBusy( self, TGIS_BusyEventArgs.Create( -1, -1, abrt ) ) ;
      {$ELSE}
        FOnBusy( self, -1, -1, abrt ) ;
      {$ENDIF}

    try
      if FBitHeight <= MAX_CELL_WH then
        ch := FBitHeight
      else
        ch := MAX_CELL_WH ;

      if FBitWidth <= MAX_CELL_WH then
        cw := FBitWidth
      else
        cw := MAX_CELL_WH ;

      maxc := FBitWidth div cw ;
      lw := FBitWidth mod cw ;
      if lw > 0 then begin
        lw := 0 ;
        inc(maxc) ;
      end;
      maxr := FBitHeight div ch ;
      lh := FBitHeight mod ch ;
      if lh > 0 then begin
        inc(maxr) ;
      end;

      sx := (FExtent.XMax -FExtent.XMin)/FBitWidth  ;
      sy := (FExtent.YMax -FExtent.YMin)/FBitHeight ;

      if maxc = 0 then begin
        maxc := 1 ;
        cw := lw ;
      end;
      try
        cnt := maxr * maxc ;
        pos := 0 ;
         if f.PixelFormat = TGIS_PixelFormat.Custom then begin
          {$IFDEF OXYGENE}
            grid := new array of Single[ch] ;
            var ii : Integer ;
            for ii := 0 to ch-1 do
              grid[ii] := new Single[cw] ;
          {$ELSE}
            SetLength( grid, ch, cw ) ;
          {$ENDIF}

          for i := 0 to maxr -1 do begin
            ext.YMax := FExtent.YMax -i*(ch*sy) ;
            ext.YMin := ext.YMax -ch*sy ;
            for k := 0 to maxc -1 do begin
              ext.XMin := FExtent.XMin +k*(cw*sx) ;
              ext.XMax := ext.XMin +cw*sx ;
              setNoDataTable( grid ) ;
              _layer.GetGrid(ext, grid) ;
              f.WriteGrid(k*cw, i*ch, grid );
              if assigned( FOnBusy ) then
                {$IFDEF OXYGENE}
                  FOnBusy( self, TGIS_BusyEventArgs.Create( pos, cnt, abrt ) ) ;
                {$ELSE}
                  FOnBusy( self, pos, cnt, abrt ) ;
                {$ENDIF}
              inc( pos ) ;
            end;
          end ;
        end
        else begin
          pixformat := f.PixelFormat ;
          SetLength(pix, cw*ch) ;
          for i := 0 to maxr -1 do begin
            ext.YMax := FExtent.YMax -i*(ch*sy) ;
            ext.YMin := ext.YMax -ch*sy ;
            for k := 0 to maxc -1 do begin
              ext.XMin := FExtent.XMin +k*(cw*sx) ;
              ext.XMax := ext.XMin +cw*sx ;
              setBmpTransparent( pix ) ;
              _layer.GetBitmap(ext, pix, cw, ch) ;
              f.Write(k*cw, i*ch, pix, pixformat, cw, ch );
              if assigned( FOnBusy ) then
                {$IFDEF OXYGENE}
                  FOnBusy( self, TGIS_BusyEventArgs.Create( pos, cnt, abrt ) ) ;
                {$ELSE}
                  FOnBusy( self, pos, cnt, abrt ) ;
                {$ENDIF}
              inc( pos ) ;
            end;
          end ;
        end;
      finally
         if f.PixelFormat = TGIS_PixelFormat.Custom then
          grid := nil
        else
          pix := nil ;
      end ;
    finally
      FreeObject(f) ;

      if assigned( FOnBusy ) then
        {$IFDEF OXYGENE}
          FOnBusy( self, TGIS_BusyEventArgs.Create( -1, -1, abrt ) ) ;
        {$ELSE}
          FOnBusy( self, -1, -1, abrt ) ;
        {$ENDIF}
    end;
  end;



  procedure TGIS_LayerTIFF.readColTabFromFile(
    const _counter    : Integer ;
    const _fileOffset : Int64
  ) ;
  var
    i, c, c2, c_1   : Integer ;
    colorTableRed   : array [0..255] of Word ;
    colorTableGreen : array [0..255] of Word ;
    colorTableBlue  : array [0..255] of Word ;

    function getRGB(const _b, _g, _r : Word) : Cardinal ;
    begin
      Result := (_b and $000000FF) or ((_g shl 8)and $0000FF00) or
                ((_r shl 16) and $00FF0000) ;
    end;
  begin
    fileStream.Position := _fileOffset ;
    c := _counter div 3 ;

    if c < 1 then exit;
    c2 := c div 2;
    c_1 := c -1 ;
    if c_1 >= length(colorTableRed) then begin
      c_1 := length(colorTableRed) -1 ;
      c2 := (c_1 +1) div 2 ;
    end ;


     // read Color Table from file
    isGrayScaleImage := True ;
    for i := 0 to c_1 do begin
      fileStream.ReadWord( colorTableRed[i],   2 ) ;
    end ;
    for i := 0 to c_1 do begin
      fileStream.ReadWord( colorTableGreen[i], 2 ) ;
      if colorTableGreen[i] <> colorTableRed[i] then
        isGrayScaleImage := False ;
    end;
    for i := 0 to c_1 do begin
      fileStream.ReadWord( colorTableBlue[i],  2 ) ;
      if colorTableBlue[i] <> colorTableRed[i] then
        isGrayScaleImage := False ;
    end;

    if bigEndian then begin
      for i:=0 to c_1 do begin
        //reversed order!!!!!!!!!!!
        {$IFDEF GIS_NORECORDS}
          bitPalette[i] := new TGIS_Color ;
        {$ENDIF}
       bitPalette[i].ARGB := getRGB( colorTableBlue [i] and $FF,
                                     colorTableGreen[i] and $FF,
                                     colorTableRed  [i] and $FF
                                    ) ;
      end ;
    end
    else begin
      if ((colorTableBlue [0]   <> 0) and (colorTableBlue [0]   > 255)) or
         ((colorTableBlue [1]   <> 0) and (colorTableBlue [1]   > 255)) or
         ((colorTableBlue [2]   <> 0) and (colorTableBlue [2]   > 255)) or
         ((colorTableBlue [c_1] <> 0) and (colorTableBlue [c_1] > 255)) or
         ((colorTableBlue [c2 ] <> 0) and (colorTableBlue [c2 ] > 255))
      then begin

        for i:=0 to c_1 do begin
          {$IFDEF GIS_NORECORDS}
            bitPalette[i] := new TGIS_Color ;
          {$ENDIF}
          bitPalette[i].ARGB := getRGB( colorTableBlue [i] div 256,
                                        colorTableGreen[i] div 256,
                                        colorTableRed  [i] div 256
                                      ) ;
        end ;
      end
      else begin
        for i:=0 to c_1 do begin
          {$IFDEF GIS_NORECORDS}
            bitPalette[i] := new TGIS_Color ;
          {$ENDIF}
          bitPalette[i].ARGB := getRGB( colorTableBlue [i] ,
                                        colorTableGreen[i] ,
                                        colorTableRed  [i]
                                      ) ;
        end ;

      end;
    end ;

  end ;

  procedure TGIS_LayerTIFF.loadIFD(
    const _fileOffset : Int64
  ) ;
  var
    i     : Integer ;
    ptagc,
    ptago  : Integer ;
    ptagot : Integer ;
    countTag64 : Int64 ;
    listTag32 : Array of TGIS_FileTIFF_Tag ;
    listStripInfo32 : Array of TGIS_FileTIFF_StripInfo32 ;
    shortUsed : Boolean ;
    shVal : Word ;
    sbc   : Cardinal ;
  begin

    Alive ;
    shortUsed := False ;
    fileStream.Position := _fileOffset ;
    if isBigTIFF then begin
      fileStream.ReadInt64( countTag64, 8 ) ;
      countTag := countTag64 ;
    end
    else
      fileStream.ReadWord( countTag, 2 ) ;
    if bigEndian then begin
        countTag := Swap( countTag ) ;
    end ;

    if countTag > 0 then begin
      // get memory for all IFD entries
      SetLength( listTag64, countTag );
      SetLength( listTag32, countTag );
      if isBigTIFF then begin
        // read all ifd entries
         for i := 0 to countTag-1 do
           ReadFT64( fileStream, listTag64[i]) ;
      end
      else begin
        // read all ifd entries
        for i := 0 to countTag-1 do
          ReadFT( fileStream, listTag32[i]) ;

      // swap Bytes
        if bigEndian then begin
          for i := 0 to countTag-1 do begin
            listTag32[i].Tag := Swap(listTag32[i].Tag);
            listTag32[i].VarType := Swap(listTag32[i].VarType);
            listTag32[i].Count   := SwapLongInt(listTag32[i].Count);
            if ( listTag32[i].VarType = 3 ) and ( listTag32[i].Count < 3 ) then // SHORT
              listTag32[i].Value := SwapShort2LongInt( listTag32[i].Value )
            else
              listTag32[i].Value := SwapLongInt( listTag32[i].Value ) ;
          end ;
        end ;
        for i := 0 to countTag-1 do begin             // listTag32[19]
          {$IFDEF GIS_NORECORDS}
            listTag64[i] := new TGIS_FileTIFF_Tag64 ;
          {$ENDIF}
          listTag64[i].Tag     := listTag32[i].Tag ;
          listTag64[i].VarType := listTag32[i].VarType ;
          listTag64[i].Count   := listTag32[i].Count ;
          listTag64[i].Value   := listTag32[i].Value ;
        end;
        SetLength( listTag32, 0 );
      end ;

      ptagc := getTagTiff( TIFF_TAG_COMPRESSION ) ;
      if ptagc <> -1 then begin
        case listTag64[ptagc].Value of
          TIFF_COMPRESSION_NONE :
            begin
              isCompressed    := False ;
              compressionType := TGIS_CompressionType.None ;
            end ;
          TIFF_COMPRESSION_CCITTRLE :
            begin
              isCompressed    := True ;
              compressionType := TGIS_CompressionType.CCITTHuffmanRLE ;
              ptagc := getTagTiff( TIFF_TAG_FILLORDER ) ;
              if ptagc <> -1 then
                fillOrder := listTag64[ptagc].Value ;
            end ;
          TIFF_COMPRESSION_CCITTFAX3 :
            begin
              isCompressed    := True ;
              compressionType := TGIS_CompressionType.CCITTFaxGroup3;
              ptagc := getTagTiff( TIFF_TAG_FILLORDER ) ;
              if ptagc <> -1 then
                fillOrder := listTag64[ptagc].Value ;
              ptagc := getTagTiff( TIFF_TAG_T6OPTIONS ) ;
              if ptagc <> -1 then begin
                if(listTag64[ptagc].Value AND $02) <> 0 then // if bit 1 is set
                  isCompressed := False ;    // uncompressed mode is allowed
              end ;
            end ;
          TIFF_COMPRESSION_CCITTFAX4 :
            begin
              isCompressed    := True ;
              compressionType := TGIS_CompressionType.CCITTFaxGroup4 ;
              ptagc := getTagTiff( TIFF_TAG_FILLORDER ) ;
              if ptagc <> -1 then
                fillOrder := listTag64[ptagc].Value ;
              ptagc := getTagTiff( TIFF_TAG_T6OPTIONS ) ;
              if ptagc <> -1 then begin
                if(listTag64[ptagc].Value AND $02) <> 0 then // if bit 1 is set
                  isCompressed := False ;    // uncompressed mode is allowed
              end ;
            end ;
  {$IFNDEF GIS_NOLZWREAD}
          TIFF_COMPRESSION_LZW :
            begin
              isCompressed    := True ;
              compressionType := TGIS_CompressionType.LZW ;
              hDifferencing   := False ;
              hFDifferencing  := False ;
              ptagc := getTagTiff( TIFF_TAG_PREDICTOR ) ;
              if ptagc <> -1 then begin
                if listTag64[ptagc].Value = 2 then begin
                  hDifferencing := True ;
                end else
                if listTag64[ptagc].Value = 3 then
                  hFDifferencing  := True ;
              end;
            end ;
  {$ENDIF}
          TIFF_COMPRESSION_PACKBITS :
            begin
              isCompressed    := True ;
              compressionType := TGIS_CompressionType.PackBits ;
            end ;
          6, TIFF_COMPRESSION_JPEG : //6 - JPEG old style
            begin
              isCompressed    := True ;
              compressionType := TGIS_CompressionType.JPEG ;
            end ;
          TIFF_COMPRESSION__ADOBE_DEFLATE,
          TIFF_COMPRESSION__DEFLATE :
            begin
              isCompressed    := True ;
              compressionType := TGIS_CompressionType.ZLIB ;
              hDifferencing   := False ;
              hFDifferencing  := False ;
              ptagc := getTagTiff( TIFF_TAG_PREDICTOR ) ;
              if ptagc <> -1 then begin
                if listTag64[ptagc].Value = 2 then begin
                  hDifferencing := True ;
                end else
                if listTag64[ptagc].Value = 3 then
                  hFDifferencing  := True ;
              end;
            end ;
          else begin
            isValid := False ;
            exit ;
          end ;
        end ;
      end ;

      ptago := getTagTiff( TIFF_TAG_STRIPOFFSETS    ) ;
      ptagc := getTagTiff( TIFF_TAG_STRIPBYTECOUNTS ) ;
      ptagot := getTagTiff( TIFF_TAG_TILEOFFSETS ) ;

      if ptagc = -1 then
        ptagc := ptago ;

      if ptagot =  -1 then begin
        ptagot := getTagTiff( TIFF_TAG_IMAGELENGTH ) ;
        actualBitHeight := listTag64[ptagot].Value;
        ptagot := getTagTiff( TIFF_TAG_TILEWIDTH ) ;
        if ptagot <> -1 then begin
          tileWidth := listTag64[ptagot].Value ;
          if actualBitHeight <> tileWidth then begin
            ptagot := ptago ;
          end;
        end;
      end ;


      if ptagot <> -1 then begin
          //set but not read
        tileOffsets := listTag64[ptagot].Value ;

        isTiffTiled := True ;
        isValid := True ;
      end else
      if listTag64[ptagc].Count = listTag64[ptago].Count then begin
        isTiffTiled := False ;

        countStripInfo := listTag64[ptagc].Count ;
        if listTag64[ptagc].VarType = 3 then
          shortUsed := True ;

        SetLength( listStripInfo64, countStripInfo ) ;
        {$IFDEF GIS_NORECORDS}
          for i := 0 to countStripInfo - 1 do
            listStripInfo64[i] := new TGIS_FileTIFF_StripInfo64 ;
        {$ENDIF}
        // Read StripOffsets and StripByteCounts into Information List
        if countStripInfo = 1 then begin
          listStripInfo64[0].StripByteCounts := listTag64[ptagc].Value ;
          listStripInfo64[0].StripOffsets := listTag64[ptago].Value ;
        end
        else begin
          fileStream.Position := listTag64[ptagc].Value ;
          if isBigTIFF then begin
            // Strip Byte Counts
            if listTag64[ptagc].VarType = 4 then begin
              for i := 0 to countStripInfo - 1 do begin
                fileStream.ReadCardinal( sbc, 4 ) ;
                listStripInfo64[i].StripByteCounts := sbc  ;
              end ;
            end
            else begin
              for i := 0 to countStripInfo - 1 do begin
                fileStream.ReadInt64( listStripInfo64[i].StripByteCounts, 8 ) ;
              end;
            end;

            fileStream.Position := listTag64[ptago].Value ;

            // Strip Offsets
            for i := 0 to countStripInfo -1 do begin
              fileStream.ReadInt64(listStripInfo64[i].StripOffsets, 8);
            end ;
          end
          else begin
            SetLength( listStripInfo32, countStripInfo ) ;
            {$IFDEF GIS_NORECORDS}
              for i := 0 to countStripInfo - 1 do
                listStripInfo32[i] := new TGIS_FileTIFF_StripInfo32 ;
            {$ENDIF}
            // Strip Byte Counts
            if shortUsed then begin
              for i := 0 to countStripInfo - 1 do begin
                fileStream.ReadWord( shVal, 2 ) ;
                listStripInfo32[i].StripByteCounts := Integer(shVal) ;
              end ;
            end
            else begin
              for i := 0 to countStripInfo - 1 do begin
                fileStream.ReadCardinal( listStripInfo32[i].StripByteCounts, 4 ) ;
              end ;
            end;

            fileStream.Position := listTag64[ptago].Value ;
            if listTag64[ptago].VarType = 3 then
              shortUsed := True
            else
              shortUsed := False ;

            // Strip Offsets
            if shortUsed then begin
              for i := 0 to countStripInfo -1 do begin
                fileStream.ReadWord( shVal, 2 ) ;
                listStripInfo32[i].StripOffsets := Cardinal(shVal);
              end;
            end
            else begin
              for i := 0 to countStripInfo -1 do begin
                fileStream.ReadCardinal(listStripInfo32[i].StripOffsets, 4);
              end ;
            end;
            // swap bytes
            if bigEndian then begin
              for i := 0 to countStripInfo -1 do begin
                listStripInfo32[i].StripByteCounts := SwapLongInt(listStripInfo32[i].StripByteCounts) ;
                listStripInfo32[i].StripOffsets    := SwapLongInt(listStripInfo32[i].StripOffsets) ;
               end;
            end ;
            for i := 0 to countStripInfo -1 do begin
              listStripInfo64[i].StripByteCounts := listStripInfo32[i].StripByteCounts ;
              listStripInfo64[i].StripOffsets    := listStripInfo32[i].StripOffsets ;
            end;
            SetLength( listStripInfo32, 0 ) ;
          end ;
        end;
        isValid := True ;
      end
      else
        isValid := False ;
    end
    else
      isValid := False ;
  end;

  function TGIS_LayerTIFF.getPagesCount(const _stream : TGIS_HandleStream )
    : Integer ;
  var
    count_tag : Word ;
    next_off : Cardinal ;
    next_off64 : Int64 ;
    filesize : Int64 ;
    count_tag64 : Int64 ;
    i : Integer ;
    tmp64 : Array [0..199] of Int64 ;
    curl : Integer ;
  begin
    if isBigTIFF then begin
      next_off64 := headerFirstIFD64 ;
      tmp64[0] := next_off64 ;
    end
    else begin
      next_off := headerFirstIFD ;
      tmp64[0] := next_off ;
    end;
    filesize := _stream.Size ;
    Result := 1 ;

    while True do begin
      if isBigTIFF then begin
        _stream.Position := next_off64 ;
        _stream.ReadInt64( count_tag64, 8 ) ;
        count_tag := count_tag64 ;
      end
      else begin
        _stream.Position := next_off ;
        _stream.ReadWord( count_tag, 2 ) ;
      end;
      if bigEndian then begin
          count_tag := Swap( count_tag ) ;
      end ;
      if isBigTIFF then begin
        _stream.Position := next_off64 + Int64(count_tag*{$IFDEF JAVA}24{$ELSE}sizeOf(TGIS_FileTIFF_Tag64){$ENDIF} +8) ; //?
        _stream.ReadInt64( next_off64, 8 ) ;
      end
      else begin
        _stream.Position := next_off + Cardinal(count_tag*{$IFDEF JAVA}12{$ELSE}sizeOf(TGIS_FileTIFF_Tag){$ENDIF} +2 ) ;
        _stream.ReadCardinal( next_off, 4 ) ;
      end;
      if bigEndian then begin
          next_off := SwapLongInt( next_off ) ;
      end ;
      if isBigTIFF then begin

        if (next_off64 <= 0) OR (next_off64 > filesize) then begin
          curl := length(associatesIFDOffset64) ;
          SetLength(associatesIFDOffset64, Result +curl) ;
          for i := 0 to Result -1 do
            associatesIFDOffset64[i +curl] := tmp64[i] ;
          break ;
        end ;

        tmp64[Result] := next_off64 ;
        inc(Result) ;
      end
      else begin
        if (next_off <= 0) OR (next_off > filesize) then begin
          curl := length(associatesIFDOffset64) ;
          SetLength(associatesIFDOffset64, Result +curl) ;
          for i := 0 to Result -1 do
            associatesIFDOffset64[i +curl] := tmp64[i] ;
          break ;
        end ;

        tmp64[Result] := next_off ;
        inc(Result) ;
      end;

    end ;

    if ovrFirstPage = 0 then begin
      if FileExists(Path+'.ovr') then begin
        ovrFirstPage  := Result + 1;
        fileStreamOvr := openBufferedFileStream( Path +'.ovr' ) ;
        isBigTIFFMain := isBigTIFF ;
        checkTiff(fileStreamOvr) ;
        if isValid then begin
          fileStreamMain := _stream ;
          isBigTIFFOvr := isBigTIFF ;
          Result := Result +getPagesCount(fileStreamOvr) ;
          isBigTIFF := isBigTIFFMain ;
        end
        else begin
          isValid := True ;
          ovrFirstPage := 0 ;
          isBigTIFF := isBigTIFFMain ;
          FreeObject(fileStreamOvr) ;
        end;
      end;
    end;


  end ;

  function TGIS_LayerTIFF.getPagesScales
    : Boolean ;
  var
    i, n : Integer ;
    w, h : Integer ;
    sorted : Boolean ;
    count : Integer ;
    s : Double ;
    rw : Integer ;
  begin
    SetLength(pagesScales, FPagesCount) ;
    SetLength(associatesPagesNo, FPagesCount) ;
    SetLength(validPagesTable, FPagesCount) ;
    rw := FBitWidth ;
    pagesScales[0] := 1 ;
    associatesPagesNo[0] := 1 ;
    validPagesTable[0] := True ;
    w := 0 ;
    h := 0 ;
    for i := 1 to FPagesCount -1 do begin
      setPageInternal(i +1) ;
      pagesScales[i] := (1.0*actualBitWidth)/rw ;
      associatesPagesNo[i] := i +1 ;
      if (actualBitWidth > FBitWidth) and (actualBitWidth > w)then begin
        w := actualBitWidth ;
        h := actualBitHeight ;
      end ;
    end ;
    actualZoom := pagesScales[FPagesCount -1] ;

    repeat
      sorted := True ;
      for i := 0 to FPagesCount -2 do begin
        if pagesScales[i] < pagesScales[i +1] then begin
          s := pagesScales[i] ;
          pagesScales[i] := pagesScales[i +1] ;
          pagesScales[i +1] := s ;
          n := associatesPagesNo[i] ;
          associatesPagesNo[i] := associatesPagesNo[i +1] ;
          associatesPagesNo[i +1] := n ;
          sorted := False ;
          break ;
        end ;
      end ;
    until sorted ;

    if w > 0 then begin
      s := pagesScales[0] ;
      pagesScales[0] := 1 ;
      for i := 1 to FPagesCount -1 do
        pagesScales[i] := pagesScales[i]/s ;
      FBitHeight := h ;
      FBitWidth  := w ;
    end ;

    count := FPagesCount ;
    for i := 1 to FPagesCount -1 do begin
      if pagesScales[i -1] = pagesScales[i] then
         dec(count) ;
    end ;
    if count < FPagesCount div 2 then
      Result := False
    else
      Result := True ;
  end ;

  procedure TGIS_LayerTIFF.initDecoder ;
  var
    decidx     : Integer ;
    count, i   : Integer ;
    dec        : TGIS_FileTIFFDecoder ;
    decno      : Integer ;
  begin

    count := length( fileDecoder ) ;
    if isPlanar1 then
      decno := tilesColumns
    else
      decno := tilesColumns*FBandsCount ;

    if count = decno then begin
      if decodeState[0].CurrentPage = FCurrentPage then begin
        if assigned(decodeState[0].StripList) then
          exit ;
      end;
    end;

    if  count > 0 then begin
      for i := high(fileDecoder) downto low(fileDecoder) do
      begin
        dec := fileDecoder[i] ;
        FreeObject( dec ) ;
      end ;
      SetLength(fileDecoder, 0) ;
    end ;

    SetLength(fileDecoder, decno) ;
    SetLength(decodeState, decno) ;

    for decidx := low(fileDecoder) to high(fileDecoder) do begin

      fileDecoder[decidx] := TGIS_FileTIFFDecoder.Create ;
      {$IFDEF GIS_NORECORDS}
      decodeState[decidx] := new TGIS_FileTIFFDecodeState ;
      {$ENDIF}
      decodeState[decidx].Compression := compressionType ;
      //set but not read
      decodeState[decidx].CurrentPage := FCurrentPage ;

      if not assigned(fileStream) then
        fileStream := openBufferedFileStream( Path ) ;

      decodeState[decidx].TiffStream := fileStream ;
      if assigned(listStripInfo64) then  begin
        decodeState[decidx].ReadBufferSize := listStripInfo64[decidx].StripByteCounts ;
      end ;

      //set but not read
      decodeState[decidx].ReadBufferBC := 0 ;

      if assigned(listStripInfo64) then
        decodeState[decidx].StripList := listStripInfo64
      else
        decodeState[decidx].StripList := nil ;

      if isTiffTiled then begin
        decodeState[decidx].RowsPerStrip := tileLength ;

        case bitsPerPixel of
          48 :
            begin
              if isPlanar1 then
                decodeState[decidx].RowBytes := tileWidth*6
              else
                decodeState[decidx].RowBytes := tileWidth*2 ;
            end;
          32 :
            begin
              if isPlanar1 then
                decodeState[decidx].RowBytes := tileWidth*4
              else
                decodeState[decidx].RowBytes := tileWidth ;
            end;
          24 :
            begin
              if isPlanar1 then
                decodeState[decidx].RowBytes := tileWidth*3
              else
                decodeState[decidx].RowBytes := tileWidth ;
            end;
          16 :
            decodeState[decidx].RowBytes := tileWidth*2 ;
           8 :
            decodeState[decidx].RowBytes := tileWidth ;
           4 :
            decodeState[decidx].RowBytes := (tileWidth +1) div 2;
           2 :
            decodeState[decidx].RowBytes := (tileWidth +1) div 4;
           1 :
            decodeState[decidx].RowBytes := (tileWidth  +7) div 8 ;
        else
          decodeState[decidx].RowBytes := tileWidth*((bitsPerPixel +7) div 8);
        end ;
        decodeState[decidx].RowPixels := tileWidth ;
      end
      else begin
        decodeState[decidx].RowsPerStrip := rowsPerStrip ;
        if isPlanar1 then begin
          if FIsNativeGridImage then
              decodeState[decidx].RowBytes := actualBitWidth*pixelBytesNo
          else
          if (compressionType = TGIS_CompressionType.JPEG) and (bitsPerPixel = 36) then
            decodeState[decidx].RowBytes := actualBitWidth*pixelBytesNo
          else begin
            decodeState[decidx].RowBytes := (actualBitWidth * bitsPerPixel +7) div 8
          end;
        end
        else begin
          if FIsNativeGridImage then begin
            if bitscountinfo = 32 then
              decodeState[decidx].RowBytes := tileWidth*pixelBytesNo
            else
              decodeState[decidx].RowBytes := (realLineWidth div 3)*
                                              ((bitscountinfo +7) div 8) ;
          end
          else begin
            if bitsPerPixel = 8 then
              decodeState[decidx].RowBytes := realLineWidth
            else
              decodeState[decidx].RowBytes := realLineWidth div FBandsCount2 ;
          end;
        end;
        decodeState[decidx].RowPixels := actualBitWidth ;
      end ;

      if extraSamples > 0 then begin
        if FBandsCount > 3 then begin
          if extraSamples = 4 then //8-bytes per pixel data
            extraSamples := 5 ;
        end ;
      end ;

      if isPlanar1 then
        decodeState[decidx].TotalRowsNo := actualBitHeight
      else
        decodeState[decidx].TotalRowsNo :=
          ((actualBitHeight +rowsPerStrip -1) div rowsPerStrip)
          *rowsPerStrip*FBandsCount ;
      decodeState[decidx].CurStrip := 0 ;
      decodeState[decidx].CurStripLine := -1 ;

      decodeState[decidx].HDifferencing := hDifferencing ;
      decodeState[decidx].HFDifferencing := hFDifferencing ;
      decodeState[decidx].BandsNo := FBandsCount2 ;
      decodeState[decidx].FillOrder     := fillOrder ;
      decodeState[decidx].jpegTables    := jpegTables ;

      fileDecoder[decidx].DecodeState := decodeState[decidx] ;
      fileDecoder[decidx].SetUp ;

      decodeState[decidx] := fileDecoder[decidx].DecodeState;
      decodeState[decidx].StartOffset := 0 ;

      if decodeState[decidx].Compression = TGIS_CompressionType.PackBits then
        fileDecoder[decidx].ResetPackLineTable ;

    end ;
  end ;

  procedure TGIS_LayerTIFF.jpegProc ;
  var
    i, j, len   : Integer ;
    q, dc, ac   : Integer ;
    ss          : Integer ;
    q_size,
    ac_dc_size     : Integer ;
    jpt         : TGIS_JPEGTable ;
    q_tab,
    ac_dc_tab   : TGIS_JPEGTable ;
    codes_tab   : TGIS_JPEGTable ;
    codes_num   : Integer ;
    hdr_len     : Word ;
    off         : array [0..3] of Cardinal ;
    {$IFDEF OXYGENE}
      offw      : Cardinal ;
    {$ENDIF}
    ss_tab      : array [0..3] of Byte ;
    ssv, ssh    : Word ;
  const
    FF_IDENT  = $FF ;
    DQT_M     = $DB ;
    DHT_M     = $C4 ;
    SOI_M     = $D8 ;
    SOS_M     = $DA ;
    SOF_M     = $C0 ;
    HT_AC     = $10 ;
    HT_DC     = $00 ;
    procedure m_ff ;
    begin
      jpt[len] := FF_IDENT ;
      inc(len) ;
    end;
    procedure put_word(const _w : Word) ;
    begin
      jpt[len] := Byte(_w shr 8) ;
      inc(len) ;
      jpt[len] := Byte(_w) ;
      inc(len) ;
    end;
    function get_codes_num : Integer ;
    var
      z : Integer ;
    begin
      Result := 0 ;
      for z := 0 to  16 - 1 do
        Result := Result +ac_dc_tab[z] ;
    end;
  begin
    q := getTagTiff( TIFF_TAG_JPEGQTABLES ) ;
    if q < 0 then
      exit ;
    ac := getTagTiff( TIFF_TAG_JPEGACTABLES ) ;
    if ac < 0 then
      exit ;
    dc := getTagTiff( TIFF_TAG_JPEGDCTABLES ) ;
    if dc < 0 then
      exit ;
    ss := getTagTiff( TIFF_TAG_YCbCrSUBSAMPLING ) ;
    if ss > 0 then begin
      ssh := listTag64[ss].Value and $FFFF ;
      ssv := Word(listTag64[ss].Value shr 16) ;
      if (ssh = 2) and (ssv =2) then begin
        ss_tab[0] := $22 ;
        ss_tab[1] := $11 ;
        ss_tab[2] := $11 ;
      end;
    end
    else begin
      ss_tab[0] := $22 ;
      ss_tab[1] := $11 ;
      ss_tab[2] := $11 ;
    end;

    q_size := 64 ;
    ac_dc_size := 16 ;
    SetLength(q_tab , q_size) ;
    SetLength(ac_dc_tab, ac_dc_size) ;
    SetLength(codes_tab, 256) ;
    SetLength(jpt, 1000) ;

    fileStream.Position := listTag64[q].Value ;
    {$IFDEF OXYGENE}
      for i := 0 to listTag64[q].Count - 1 do begin
        fileStream.ReadCardinal(offw, 4);
        off[i] := offw ;
      end;
    {$ELSE}
      fileStream.Read(off[0], 4*listTag64[q ].Count);
    {$ENDIF}

    len := 0 ;
    m_ff ;
    jpt[len] := SOI_M ;
    inc(len) ;
    hdr_len := 64 + 1 +2;

    for i := 0 to listTag64[q ].Count - 1 do begin
      if bigEndian then
        off[i] := SwapLongInt(off[i]) ;
      fileStream.Position := off[i] ;
      {$IFDEF OXYGENE}
        fileStream.Read(q_tab, 64);
      {$ELSE}
        fileStream.Read(q_tab[0], 64);
     {$ENDIF}

      m_ff ;
      jpt[len] := DQT_M ;
      inc(len) ;
      put_word(hdr_len) ;
      jpt[len] := Byte(i) ;
      inc(len) ;
      for j := 0 to 63 do begin
        jpt[len] := q_tab[j] ;
        inc(len) ;
      end;
    end;

    //SOF
    m_ff ;
    jpt[len] := SOF_M ;
    inc(len) ;
    hdr_len := 8 +listTag64[q ].Count*3 ;
    put_word(hdr_len) ;
    jpt[len] := 8 ;   //precision
    inc(len) ;
    put_word(tileLength) ;
    put_word(tileWidth) ;
    jpt[len] := 3 ;   //number of componenent
    inc(len) ;
    for i := 0 to 2 do begin
      jpt[len] := Byte(i +1) ;   //component id
      inc(len) ;
      jpt[len] := ss_tab[i] ;    //sampling factors
      inc(len) ;
      jpt[len] := Byte(i) ;   //quantization table number
      inc(len) ;
    end;

    //AC huffman tables
    fileStream.Position := listTag64[ac].Value ;
    {$IFDEF OXYGENE}
      for i := 0 to listTag64[ac].Count - 1 do begin
        fileStream.ReadCardinal(offw, 4);
        off[i] := offw ;
      end;
    {$ELSE}
      fileStream.Read(off[0], 4*listTag64[ac].Count);
    {$ENDIF}

    for i := 0 to listTag64[ac ].Count - 1 do begin
      if bigEndian then
        off[i] := SwapLongInt(off[i]) ;
      fileStream.Position := off[i] ;
      {$IFDEF OXYGENE}
        fileStream.Read(ac_dc_tab, ac_dc_size);
      {$ELSE}
        fileStream.Read(ac_dc_tab[0], ac_dc_size);
      {$ENDIF}

      codes_num := get_codes_num ;
      {$IFDEF OXYGENE}
        fileStream.Read(codes_tab, codes_num);
      {$ELSE}
        fileStream.Read(codes_tab[0], codes_num);
      {$ENDIF}

      m_ff ;
      jpt[len] := DHT_M ;
      inc(len) ;
      hdr_len := 2+1+16+codes_num ;
      put_word(hdr_len) ;
      jpt[len] := Byte(HT_AC or i ) ;
      inc(len) ;
      for j := 0 to 16 -1 do begin
        jpt[len] := ac_dc_tab[j] ;
        inc(len) ;
      end;
      for j := 0 to codes_num -1 do begin
        jpt[len] := codes_tab[j] ;
        inc(len) ;
      end;
    end;

    //DC huffman tables
    fileStream.Position := listTag64[dc].Value ;
    {$IFDEF OXYGENE}
      for i := 0 to listTag64[dc].Count - 1 do begin
        fileStream.ReadCardinal(offw, 4);
        off[i] := offw ;
      end;
    {$ELSE}
      fileStream.Read(off[0], 4*listTag64[dc].Count);
    {$ENDIF}

    for i := 0 to listTag64[dc ].Count - 1 do begin
      if bigEndian then
        off[i] := SwapLongInt(off[i]) ;
      fileStream.Position := off[i] ;
      {$IFDEF OXYGENE}
        fileStream.Read(ac_dc_tab, ac_dc_size);
      {$ELSE}
        fileStream.Read(ac_dc_tab[0], ac_dc_size);
      {$ENDIF}

      codes_num := get_codes_num ;
      {$IFDEF OXYGENE}
        fileStream.Read(codes_tab, codes_num);
      {$ELSE}
        fileStream.Read(codes_tab[0], codes_num);
     {$ENDIF}

      m_ff ;
      jpt[len] := DHT_M ;
      inc(len) ;
      hdr_len := 2+1+16+codes_num ;
     put_word(hdr_len) ;
      jpt[len] := Byte(HT_DC or i ) ;
      inc(len) ;
      for j := 0 to 16 -1 do begin
        jpt[len] := ac_dc_tab[j] ;
       inc(len) ;
      end;
      for j := 0 to codes_num -1 do begin
        jpt[len] := codes_tab[j] ;
        inc(len) ;
      end;
    end;

    //SOS
    m_ff ;
    jpt[len] := SOS_M ;
    inc(len) ;
    hdr_len := 6 +listTag64[q ].Count*2 ;
    put_word(hdr_len) ;
    jpt[len] := 3 ;   //number of componenent
    inc(len) ;
    //component 1
    jpt[len] := 01 ;
    inc(len) ;
    jpt[len] := 0 ;
    inc(len) ;
    //component 2
    jpt[len] := 02 ;
    inc(len) ;
    jpt[len] := $11 ;
    inc(len) ;
    //component 3
    jpt[len] := 03 ;
    inc(len) ;
    jpt[len] := $11 ;
    inc(len) ;

    jpt[len] := 0 ;
    inc(len) ;
    jpt[len] := $3F ;
    inc(len) ;

    put_word(0) ;  // 3 + 2 bytes to ignore
    put_word(0) ;
    jpt[len] := 0 ;
    inc(len) ; // 3 bytes to ignore
    SetLength(jpegTables, len) ;
    for i := 0 to len -1 do
      jpegTables[i] := jpt[i] ;
    SetLength(codes_tab, 256) ;
    SetLength(jpt, 0) ;
    SetLength(ac_dc_tab, 0) ;
    SetLength(q_tab , 0) ;
  end;

  function TGIS_LayerTIFF.getTagTiff(
    const _tag : Word
  ) : Integer ;
  var
    i  : Integer ;
  begin
    Result := -1  ;
    for i := 0 to countTag -1 do begin
      {$IFDEF JAVA}
        if Integer(listTag64[i].Tag) = Integer(_tag) then begin
      {$ELSE}
        if listTag64[i].Tag = _tag then begin
      {$ENDIF}
          Result := i ;
          break ;
        end ;
    end ;
  end ;

  procedure TGIS_LayerTIFF.setTiffBandsDefinition ;
  var
    i : Integer ;
    cont : array [0..3] of TGIS_PixelBandContent ;
    cod : TGIS_PixelBandCoding ;
    no : Integer ;
    pbs : TGIS_PixelBandSet ;
    b1, b2, b3, b4 : Cardinal ;
  begin
    if FBandsCount2 < 3 then begin
      if FIsNativeGridImage then begin
        cont[bandsMap[0]] := TGIS_PixelBandContent.Dem ;
        pbs := TGIS_PixelBandSet.Dem ;
      end
      else begin
        cont[bandsMap[0]] := TGIS_PixelBandContent.Photo ;
        if swapBW then
          pbs := TGIS_PixelBandSet.Negative
        else
        if isGrayScaleImage then
          pbs := TGIS_PixelBandSet.Greyscale
        else begin
          if bitsPerPixel <= 8 then
            pbs := TGIS_PixelBandSet.Rgb
          else
            pbs := TGIS_PixelBandSet.Greyscale
        end;
        if bitsPerBand[0] = 0 then
          bitsPerBand[0] := bitsPerPixel ;
      end;
      b1 := 1 ;
      if  FBandsCount2 = 2 then begin
        b2 := 2 ;
        if alphaAssociated then
          cont[bandsMap[1]] := TGIS_PixelBandContent.Alpha
        else
          cont[bandsMap[1]] := TGIS_PixelBandContent.Photo ;
      end
      else
        b2 := 0 ;
      b3 := 0 ;
      b4 := 0 ;

    end
    else begin
      cont[bandsMap[0]] := TGIS_PixelBandContent.Red ; //or Red...
      cont[bandsMap[1]] := TGIS_PixelBandContent.Green ;
      cont[bandsMap[2]] := TGIS_PixelBandContent.Blue ; //or Blue...
      b1 := 1 ;
      b2 := 2 ;
      b3 := 3 ;
      b4 := 0 ;
      if FBandsCount2 > 3 then begin
        if alphaAssociated then begin
          cont[bandsMap[3]] := TGIS_PixelBandContent.Alpha ;
          pbs := TGIS_PixelBandSet.Argb ;
        end
        else begin
          cont[bandsMap[3]] := TGIS_PixelBandContent.Photo ;
          pbs := TGIS_PixelBandSet.Rgb ;
        end;
      end
      else begin
        b4 := 0 ;
        pbs := TGIS_PixelBandSet.Rgb ;
      end;
    end ;

    case bandDataType of
      TGIS_BandDataType.dtU8:
        begin
          if bitsPerPixel <= 8 then
            cod := TGIS_PixelBandCoding.Palette
          else
            cod := TGIS_PixelBandCoding.Cardinal ;
        end ;
      TGIS_BandDataType.dtU16,
      TGIS_BandDataType.dtU32 :
        begin
          cod := TGIS_PixelBandCoding.Cardinal ;
        end ;
      TGIS_BandDataType.dtS8,
      TGIS_BandDataType.dtS32,
      TGIS_BandDataType.dtS16 :
        begin
          cod := TGIS_PixelBandCoding.Integer ;
        end ;
      TGIS_BandDataType.dtF32,
      TGIS_BandDataType.dtF64 :
        begin
          cod := TGIS_PixelBandCoding.Float ;
        end
    else begin
      if bitsPerPixel <= 8 then begin
        cod := TGIS_PixelBandCoding.Palette ;
        bandDataType := TGIS_BandDataType.dtU8 ;
      end
      else
        cod := TGIS_PixelBandCoding.Cardinal ;
      end;
    end;

    bandsDefinition.ClearBands ;
    bandsDefinition.SetBandSet(FMakerModel +'.' +FSoftware, pbs, b1, b2, b3, b4);
    no := FBandsCount2 -1 ;
    if no > 3 then
      no := 3 ;
    for i := 0 to no do begin
      bandsDefinition.AddBand(cont[i], cod, bitsSkipLeft[i],
                              bitsPerBand[i]-bitsSkipLeft[i] -bitsSkipRight[i],
                                            bitsSkipRight[i] );


    end;
  end;

  procedure TGIS_LayerTIFF.forceTiffBandsDefinition ;
  var
    idx : Integer ;
    {$IFNDEF OXYGENE}
    bnd : TGIS_PixelBand ;
    {$ENDIF}
  begin
    bandsDefinition.ClearBands ;
    bandsDefinition.FromString(FForcedBandsDefinition) ;
    for bnd in bandsDefinition.Bands do begin
      idx := bandsDefinition.Bands.IndexOf(bnd) ;
      case bnd.Content of
        TGIS_PixelBandContent.Alpha : begin
                                        bandsMap[3] :=  idx ;
                                        alphaAssociated := True ;
                                        isPartialTransparent := True ;
                                      end;
        TGIS_PixelBandContent.Red   : bandsMap[0] :=  idx ;
        TGIS_PixelBandContent.Green : bandsMap[1] :=  idx ;
        TGIS_PixelBandContent.Blue  : bandsMap[2] :=  idx ;
        TGIS_PixelBandContent.Dem   : FGridBand   :=  idx +1 ;
      end ;

      assert( bnd.SkipBefore <=64 ) ;
      bitsSkipLeft[idx] := bnd.SkipBefore ;
      assert( bnd.ValidBits  <=64 ) ;
      assert( bnd.SkipAfter  <=64 ) ;
//?
      if bnd.SkipBefore +bnd.ValidBits +bnd.SkipAfter = bitsPerBand[idx] then
        bitsSkipRight[idx] := bnd.SkipAfter
      else
        bitsSkipRight[idx] := bitsPerBand[idx] -bnd.ValidBits - bnd.SkipBefore ;

      case bnd.Coding of
        TGIS_PixelBandCoding.Palette  : ;
        TGIS_PixelBandCoding.Integer  :  ;
        TGIS_PixelBandCoding.Cardinal :  ;
        TGIS_PixelBandCoding.Float    :  ;
        else begin
          assert( False ) ;
        end;
      end;
    end;
  end;

  function TGIS_LayerTIFF.interpretTiffTags : Boolean ;
  var
    i  : Integer ;
  begin
    Result := True  ;
    for i := 0 to countTag -1 do begin
      if not isValid then
        exit ;
      case Integer(listTag64[i].Tag) of
        TIFF_TAG_IMAGEWIDTH       : setImageWidth(i) ;
        TIFF_TAG_IMAGELENGTH      : setImageHeight(i) ;
        TIFF_TAG_BITSPERSAMPLE    : setBPP(i) ;
        TIFF_TAG_COMPRESSION      : setCompression(i) ;
        TIFF_TAG_PHOTOMETRIC      : setPhotometric(i) ;
        TIFF_TAG_MINSAMPLEVAL     : setMinValue(i) ;
        TIFF_TAG_MAXSAMPLEVAL     : setMaxValue(i) ;
        TIFF_TAG_EXTRASAMPLES     : setExtraSample(i) ;
        TIFF_TAG_SAMPLEFORMAT     : setSampleFormat(i) ;
        TIFF_TAG_JPEGTABLES       : setJPEGTables(i) ;
        TIFF_TAG_JPEGPROC         : if  listTag64[i].Value = 1 then
                                      callJpegProc := True ;
        TIFF_TAG_YCbCrSUBSAMPLING : setJPEGSubSampling(i) ;
        GEOTIFF_TAG_GDALNODATA  : setNoDataParams(i) ;
      else
        continue ;
      end ;
    end ;
  end ;

  procedure TGIS_LayerTIFF.setSampleFormat( const _tidx : Integer ) ;
  var
    i : Integer ;
    ssv : Array [0..19] of Word ;
    vv  : Cardinal ;
    cc  : Word ;
  begin
    cc := Word(listTag64[_tidx].Count) ;
    if cc  = 1 then begin
      if listTag64[_tidx].Value = 1 then begin
        if  bitsPerBand[0] = 32 then
          bandDataType :=  TGIS_BandDataType.dtU32
        else
        if  bitsPerBand[0] = 16 then begin
          bandDataType :=  TGIS_BandDataType.dtU16 ;
        end
        else
        if  bitsPerBand[0] = 8 then begin
          bandDataType :=  TGIS_BandDataType.dtU8 ;
          floatDataType := False ;
        end;
      end
      else
      if listTag64[_tidx].Value = 2 then begin
        if (bitsPerBand[0] = 32) or (bitscountinfo = 32) then
          signed32Bits := true
        else
        if (bitsPerBand[0] = 8) then begin
          bandDataType :=  TGIS_BandDataType.dtU8 ;
          floatDataType := False ;
        end
        else
          signed16Bits := True ;
      end
      else
      if listTag64[_tidx].Value = 3 then begin
        if bitsPerBand[0] = 32 then
          bandDataType :=  TGIS_BandDataType.dtF32 ;
      end ;
      if (listTag64[_tidx].Value > 1) or (Params.Pixel.GridBand>0) then begin
        if bitsPerBand[0] > 8 then begin
          if subViewsCount <= 1 then
            FIsGridImage := True ;
          likeGrid := True ;
          FNoDataValue := Params.Pixel.GridNoValue ;
          FAntialias := True ;
        end;
      end
    end
    else begin
      vv := Cardinal(listTag64[_tidx].Value ) ;
      if cc >= 3 then
        fileStream.Position := vv ;
      for i := 0 to cc -1 do begin

        if cc >= 3 then begin
          {$IFDEF OXYGENE}
            fileStream.ReadWord( ssv[i], 2 );
          {$ELSE}
            fileStream.Read(ssv[i], 2);
          {$ENDIF}
        end
        else begin
          ssv[i] := Word(vv and $FF) ;
          vv := vv shr 16 ;
        end;
        if bigEndian then
            ssv[i] := Swap(  ssv[i] ) ;
      end ;
      if ssv[0] = 3 then begin
        if bitsPerBand[0] = 32 then
          bandDataType :=  TGIS_BandDataType.dtF32
        else
        if bitsPerBand[0] = 64 then
            bandDataType :=  TGIS_BandDataType.dtF64 ;
        if (FBandsCount2 >= 3) and (FBandsCount2 <= 6) then begin
          floatDataType := True ;
        end
        else begin
          likeGrid := True ;
          FIsGridImage := True ;
        end;
      end
      else
      if ssv[0] = 2 then begin
        if  bitsPerBand[0] = 32 then
          bandDataType :=  TGIS_BandDataType.dtS32
        else
        if  bitsPerBand[0] = 16 then
          bandDataType :=  TGIS_BandDataType.dtS16
        else
        if  bitsPerBand[0] = 8 then
          bandDataType :=  TGIS_BandDataType.dtS8 ;
        floatDataType := False ;
      end
      else
      if ssv[0] = 1 then begin
        if  bitsPerBand[0] = 32 then
          bandDataType :=  TGIS_BandDataType.dtU32
        else
        if  bitsPerBand[0] = 16 then begin
          bandDataType :=  TGIS_BandDataType.dtU16 ;
        end
        else
        if  bitsPerBand[0] = 8 then begin
          bandDataType :=  TGIS_BandDataType.dtU8 ;
          floatDataType := False ;
        end;
      end
   end;
  end;

  procedure TGIS_LayerTIFF.setImageWidth( const _tidx : Integer ) ;
  begin
    actualBitWidth :=  Integer(listTag64[_tidx].Value) ;
    if FBitWidth < actualBitWidth then begin
      if FPagesCount > 1 then
        Extent := GisExtent(0, 0, 0, 0) ;
      FBitWidth := actualBitWidth  ;
    end ;

  end;

  procedure TGIS_LayerTIFF.setImageHeight( const _tidx : Integer ) ;
  begin
    actualBitHeight :=  Integer(listTag64[_tidx].Value) ;
    if FBitHeight < actualBitHeight then begin
      if FPagesCount > 1 then
        Extent := GisExtent(0, 0, 0, 0) ;
      FBitHeight := actualBitHeight ;
    end ;

  end;

  procedure TGIS_LayerTIFF.setCompression( const _tidx : Integer ) ;
    var
      lidx : Integer ;
    procedure check_fill_order  ;
    begin
      lidx := getTagTiff( TIFF_TAG_FILLORDER ) ;
      if lidx <> -1 then
        fillOrder := listTag64[lidx].Value ;
    end;
    procedure check_T6opcion  ;
    begin
      lidx := getTagTiff( TIFF_TAG_T6OPTIONS ) ;
      if lidx <> -1 then begin
        if(listTag64[lidx].Value AND $02) <> 0 then // if bit 1 is set
          isCompressed := False ;    // uncompressed mode is allowed
      end ;
    end;
    procedure check_predictor  ;
    begin
      hDifferencing  := False ;
      hFDifferencing := False ;
      lidx := getTagTiff( TIFF_TAG_PREDICTOR ) ;
      if lidx <> -1 then begin
        if listTag64[lidx].Value = 2 then
          hDifferencing := True
        else
        if listTag64[lidx].Value = 3 then begin
          hFDifferencing  := True ;
        end;
      end;
    end;
  begin

    case listTag64[_tidx].Value of
      TIFF_COMPRESSION_NONE :
        begin
          isCompressed    := False ;
          compressionType := TGIS_CompressionType.None ;
        end ;
      TIFF_COMPRESSION_CCITTRLE :
        begin
          isCompressed    := True ;
          compressionType := TGIS_CompressionType.CCITTHuffmanRLE ;
          check_fill_order ;
        end ;
      TIFF_COMPRESSION_CCITTFAX3 :
        begin
          isCompressed    := True ;
          compressionType := TGIS_CompressionType.CCITTFaxGroup3;
          check_fill_order ;
          check_T6opcion ;
        end ;
      TIFF_COMPRESSION_CCITTFAX4 :
        begin
          isCompressed    := True ;
          compressionType := TGIS_CompressionType.CCITTFaxGroup4 ;
          check_fill_order ;
          check_T6opcion ;
        end ;
    {$IFNDEF GIS_NOLZWREAD}
      TIFF_COMPRESSION_LZW :
        begin
          isCompressed    := True ;
          compressionType := TGIS_CompressionType.LZW ;
          check_predictor ;
        end ;
    {$ENDIF}
      TIFF_COMPRESSION_PACKBITS :
        begin
          isCompressed    := True ;
          compressionType := TGIS_CompressionType.PackBits ;
        end ;
      6, TIFF_COMPRESSION_JPEG : //6 - JPEG old style
        begin
          if (bitsPerBand[0] <> 8) and  (bitsPerBand[0] <> 12 ) and  (bitsPerBand[0] <> 16 ) then
            isValid := False ;
          isCompressed    := True ;
          compressionType := TGIS_CompressionType.JPEG ;
        end ;
      TIFF_COMPRESSION__ADOBE_DEFLATE,
      TIFF_COMPRESSION__DEFLATE :
        begin
          isCompressed    := True ;
          compressionType := TGIS_CompressionType.ZLIB ;
          check_predictor ;
        end ;
      else begin
        isValid := False ;
        exit ;
      end ;
    end;

  end;

  procedure TGIS_LayerTIFF.setBPP( const _tidx : Integer ) ;
  var
    i : Integer ;
    bppc : Int64 ;
    {$IFDEF OXYGENE}
      bppw : Word ;
    {$ENDIF}
    bc : Integer ;
  begin
    FBandsCount2 :=  Integer(listTag64[_tidx].Count) ;
    bitsPerPixel := 0 ;
    bytesPerPixel := 0 ;

    bppc:= Int64(listTag64[_tidx].Value) ;
    if (FBandsCount2 = 1) and ((bppc = 4) or (bppc = 8) or (bppc = 16)) then begin
      for i := 0 to FBandsCount2 -1 do begin
        bitsPerBand[i] := bppc ;
        bitsPerPixel := bitsPerPixel + Integer(bitsPerBand[i]) ;
        bytesPerBand[i] :=  (bitsPerBand[i] +7) div 8 ;
        bytesPerPixel := bytesPerPixel + Integer(bytesPerBand[i]) ;
      end ;
      if ( bitsPerBand[0] mod 8) <> 0 then
        isBitsString := True ;
      exit ;
    end;

    if isBigTIFF then
      bc := 4
    else
      bc := 2 ;


   if (FBandsCount2 <= bc) and ( Word(listTag64[_tidx].VarType) = 3)  then begin
      for i := 0 to FBandsCount2 -1 do begin
        bitsPerBand[i] := $FF and bppc ;
        bytesPerBand[i] :=  (bitsPerBand[i] +7) div 8 ;
        bppc := bppc shr 16 ;
        bytesPerPixel := bytesPerPixel +Integer(bytesPerBand[i]) ;
        bitsPerPixel := bitsPerPixel +Integer(bitsPerBand[i]) ;
      end;
      if ( bitsPerBand[0] mod 8) <> 0 then
        isBitsString := True ;
      exit ;
    end ;

    if FBandsCount2 > 1 then begin
      fileStream.Position := listTag64[_tidx].Value ;
      for i := 0 to FBandsCount2 -1 do begin
      {$IFDEF OXYGENE}
        fileStream.ReadWord(bppw, 2 );
        bitsPerBand[i] := bppw ;
      {$ELSE}
        fileStream.Read(bitsPerBand[i], 2);
      {$ENDIF}
        if bigEndian then
          bitsPerBand[i] := Swap(  bitsPerBand[i] ) ;
        bitsPerPixel := bitsPerPixel + Integer(bitsPerBand[i]) ;
        bytesPerBand[i] :=  (bitsPerBand[i] +7) div 8 ;
        if ( bitsPerBand[i] mod 8) <> 0 then
          isBitsString := True ;
        bytesPerPixel := bytesPerPixel + Integer(bytesPerBand[i]) ;
      end ;
    end
    else begin
      if listTag64[_tidx].Value > 100  then begin
        fileStream.Position := listTag64[_tidx].Value ;
        {$IFDEF OXYGENE}
          fileStream.ReadWord( bppw, 2 );
          bitsPerBand[0] := bppw ;
        {$ELSE}
          fileStream.Read(bitsPerBand[0], 2);
        {$ENDIF}
      end else
      begin
        bitsPerBand[0] := listTag64[_tidx].Value ;
        bitsPerPixel := bitsPerBand[0] ;
      end ;
      bytesPerBand[0] :=  (bitsPerBand[0] +7) div 8 ;
      bytesPerPixel :=  bytesPerBand[0] ;
      if ( bitsPerBand[0] mod 8) <> 0 then
        isBitsString := True ;
    end;
  end;


  procedure TGIS_LayerTIFF.setPhotometric( const _tidx : Integer ) ;
  begin
    phti := listTag64[_tidx].Value ;
  end;

  procedure TGIS_LayerTIFF.setMinValue( const _tidx : Integer ) ;
  var
    tv : Word ;
  begin
    if FIsNativeGridImage then begin
      if signed16Bits then begin
        FMinZ := SmallInt(listTag64[_tidx].Value) ;
      end
      else begin
        if bigEndian then
          tv := Word(SwapLongInt(listTag64[_tidx].Value))
        else
          tv := Word(listTag64[_tidx].Value) ;
        if tv > $8000 then begin
          signed16Bits := True ;
          FMinZ := SmallInt(listTag64[_tidx].Value) ;
          bandDataType := TGIS_BandDataType.dtS16 ;
        end
        else
          FMinZ := tv ;
      end;
    end;
  end;

  procedure TGIS_LayerTIFF.setMaxValue( const _tidx : Integer ) ;
  begin
    if FIsNativeGridImage then begin
      if signed16Bits then begin
        FMaxZ := SmallInt(listTag64[_tidx].Value) ;
      end
      else begin
        FMaxZ := listTag64[_tidx].Value ;
      end;
    end;
  end;

  procedure TGIS_LayerTIFF.setJPEGTables( const _tidx : Integer ) ;
  var
    i, tabl : Integer ;
  begin
    if compressionType <> TGIS_CompressionType.JPEG then
      exit ;
    tabl := listTag64[_tidx].Count ;
    SetLength(jpegTables, tabl ) ;
    fileStream.Position := listTag64[_tidx].Value ;

    {$IFDEF OXYGENE}
      fileStream.Read(jpegTables, listTag64[_tidx].Count);
    {$ELSE}
      fileStream.Read(jpegTables[0], listTag64[_tidx].Count);
    {$ENDIF}

    for i := 0 to tabl -2 do begin
      if (jpegTables[i] = $FF) and
         (jpegTables[i +1] = $D9) then
      begin
        if i <> tabl -2 then
           SetLength(jpegTables, i + 2 ) ;
        break ;
      end ;
    end ;

  end;

  procedure TGIS_LayerTIFF.setJPEGSubSampling( const _tidx : Integer ) ;
  begin
    subSampleHorizYCbCr := Word(listTag64[_tidx].Value shr 16);
    subSampleVertYCbCr := Word(listTag64[_tidx].Value) ;
     if phti = 2 then begin
       if componentsJpeg = JPEG_YBR then begin
         componentsJpeg := JPEG_BGR ;
       end;
     end
     else begin
       componentsJpeg := JPEG_YBR ;
     end;
  end;

  procedure TGIS_LayerTIFF.setExtraSample( const _tidx : Integer ) ;
  var
    val : Word ;
  begin
    val := 5 ;
    if listTag64[_tidx].Value > 10  then begin
      fileStream.Position := listTag64[_tidx].Value ;
      {$IFDEF OXYGENE}
        fileStream.ReadWord( val, 2 );
      {$ELSE}
          fileStream.Read(val, 2);
      {$ENDIF}
    end else
     val := listTag64[_tidx].Value ;
   if (val <= 2) and ( (bitsPerBand[0] = 8) or (bitsPerBand[0] = 16))  and
                                               ((FBandsCount2 = 4) or
                                               (FBandsCount2 = 2))then begin
     alphaAssociated := True ;
     defaultPartialTransparent := True ;
     isPartialTransparent := True ;
   end
   else
    alphaAssociated := False ;
  end;

  procedure TGIS_LayerTIFF.setNoDataParams( const _tidx : Integer ) ;
  var
    txt : String ;
    workdouble : Double ;
  begin
    if Params.Pixel.GridNoValue <> GIS_GRID_NOVALUE then
      exit ;
    txt := getTagAscii( _tidx ) ;
    try
      if not IsStringEmpty( txt ) then
        workdouble := DotStrToFloat(txt)
      else
        exit ;
      if txt = '-' then  begin
        exit ;
      end;
      if workdouble > GIS_MAX_SINGLE then
        Params.Pixel.GridNoValue := GIS_MAX_SINGLE
      else
      if workdouble < -GIS_MAX_SINGLE then
        Params.Pixel.GridNoValue := -GIS_MAX_SINGLE
      else
        Params.Pixel.GridNoValue := workdouble ;
      FNoDataValue :=  Params.Pixel.GridNoValue ;
      if (workdouble >= 0) and (workdouble <= Cardinal($FFFFFFFF))  then
      begin
        valueNoDataGDAL := Cardinal(TruncS(workdouble)) ;
        isColorNoDataGDAL := True ;
      end;
    except
      // do nothing
    end;
  end;

  function TGIS_LayerTIFF.getTagAscii(
    const _ptag : Integer
  ) : String ;
  var
    buf : TBytes ;
  begin
    if listTag64[_ptag].Count <= 4 then begin
      SetLength( buf, listTag64[_ptag].Count ) ;
      if listTag64[_ptag].Count > 0 then
        buf[0] := ( listTag64[_ptag].Value and $000000FF ) ;
      if listTag64[_ptag].Count > 1 then
        buf[1] := ( listTag64[_ptag].Value and $0000FF00 ) shr 8;
      if listTag64[_ptag].Count > 2 then
        buf[2] := ( listTag64[_ptag].Value and $00FF0000 ) shr 16 ;
      if listTag64[_ptag].Count > 3 then
        buf[3] := ( listTag64[_ptag].Value and $FF000000 ) shr 24 ;

      Result := ConvertAnsiString( buf ) ;
    end
    else begin
      fileStream.Position := listTag64[_ptag].Value ;
      SetLength( buf, listTag64[_ptag].Count ) ;
      {$IFDEF OXYGENE}
        fileStream.Read( buf   , high( buf ) + 1  ) ;
      {$ELSE}
        fileStream.Read( buf[0], high( buf ) + 1  ) ;
      {$ENDIF}
      Result := ConvertAnsiString( buf ) ;
    end;
  end ;

  function TGIS_LayerTIFF.getWordDivider
    : Double ;
  var
    linenr, sty : Integer ;
    words  : Integer ;
    single_buff : TGIS_SingleArray ;
    idx  : Integer ;
    maxVal, minVal  :  Double ;
    count : Integer ;
  const
    MAX_LINES = 200 ;
  begin
    maxVal := -GIS_MAX_SINGLE ;
    minVal :=  GIS_MAX_SINGLE ;


    if MAX_LINES > actualBitHeight then
      sty :=1
    else
      sty := actualBitHeight div MAX_LINES ;

    linenr:= 0 ;

    words := FBandsCount2 * actualBitWidth*Integer(bytesPerBand[0]) ;
    SetLength(single_buff, words ) ;
    if FBandsCount2 >= 3 then
      count := 3*actualBitWidth
    else
      count := actualBitWidth ;


    while linenr < actualBitHeight  do begin

      getNativeLine(single_buff, linenr, 0, actualBitWidth) ;

      for idx := 0 to count -1  do begin
        if single_buff[idx] <> FNoDataValue then begin
          if single_buff[idx] > maxVal then
            maxVal := single_buff[idx]
          else
          if single_buff[idx] < minVal then
            minVal := single_buff[idx] ;
        end ;
      end;
      inc(linenr, sty) ;
    end;

    Result := maxVal/255.0 ;
    if Result = 0 then begin
      if minVal < 0 then begin
        Result := minVal/255.0 ;
      end
      else
        Result := 1.01 ;
    end;

    SetLength(single_buff, 0 );
  end ;

  function TGIS_LayerTIFF.getWordShift
    : Integer ;
  var
    linenr : Integer ;
    words  : Integer ;
    idx  : Integer ;
    didx : Integer ; //buffer step
    cmp_car  :  Cardinal ;
    cmp_idx  :  Integer ;
    dl : Integer ;
    wbuff   : TBytes ;
    wval : Word ;
    awval : Word ;
    bnc : Integer ;
    wcount : Integer ;
  const
    MAX_LINES = 200 ;
  begin

    if (bitsPerBand[0] > 8) and (bitsPerBand[0] < 16) then begin
      Result := 16 -bitsPerBand[0] ;
      exit ;
    end;
    cmp_car := 0 ;
    Result := 0 ;

    Alive ;
    if actualBitHeight <= MAX_LINES then
      dl := 1
    else
      dl := actualBitHeight div MAX_LINES ;

    SetLength(wbuff, actualBitWidth*2*FBandsCount) ;
    if bigEndian then
      cmp_idx := 0
    else begin
      cmp_idx := 1 ;
    end;

    linenr := 0 ;
    if (FBandsCount2 = 2) and alphaAssociated then begin
      didx := 4 ;
      bnc := 1 ;
    end
    else begin
      didx := 2 ;
      bnc := FBandsCount ;
    end;
    wcount := 0 ;
    while linenr < actualBitHeight  do begin
       if alphaAssociated then begin
          inc(wcount) ;
          if wcount = FBandsCount then begin
            inc(idx, didx) ;
            inc(linenr, dl) ;
            wcount := 0 ;
            continue ;
          end;
       end;

      if isPlanar1 then
        getLineBits(wbuff, 0, linenr, 0, actualBitWidth)
      else
        getLineBits2(wbuff, 0, linenr, 0, actualBitWidth) ;
      idx := cmp_idx ;
      for words := 0 to actualBitWidth*bnc -1  do begin
        if idx >= actualBitWidth*bnc then
          break ;

        if isColorNoDataGDAL and (not alphaAssociated) then begin
          if bigEndian then
            wval := (Word(wbuff[idx]) shl 8) + Word(wbuff[idx +1])
          else
            wval := (Word(wbuff[idx -1]) shl 8) + Word(wbuff[idx]) ;
          if wval = valueNoDataGDAL then begin
            inc(idx, didx) ;
            continue ;
          end;
        end;

        if alphaAssociated then begin
          if words < (actualBitWidth -1)*bnc -1 then begin
            if ((words) div bnc) = 0 then begin
              if bigEndian then
                awval := (Word(wbuff[idx +bnc*2]) shl 8) + Word(wbuff[idx +1])
              else
                awval := (Word(wbuff[idx +(bnc -1)*2 -1]) shl 8) + Word(wbuff[idx +(bnc -1)*2]) ;

              if awval = 0 then begin
                inc(idx, didx*bnc) ;
                continue ;
              end;
            end;
          end;

          if ((words +1) mod bnc) <> 0 then begin
            if  cmp_car < Cardinal(wbuff[idx]) then
               cmp_car := Cardinal(wbuff[idx]) ;
          end;
        end ;

        if alphaAssociated then begin
          if ((words +1) mod bnc) <> 0 then begin
            if  cmp_car < Cardinal(wbuff[idx]) then
               cmp_car := Cardinal(wbuff[idx]) ;
          end;
        end
        else begin
          if  cmp_car < Cardinal(wbuff[idx]) then
             cmp_car := Cardinal(wbuff[idx]) ;
        end;

        inc(idx, didx) ;
      end ;
      inc(linenr, dl) ;
    end ;
    wval := 0 ;
    while wval < cmp_car do begin
      wval := wval shl 1 ;
      wval := wval or $01 ;
      inc(Result) ;
    end;

    SetLength(wbuff, 0 );
  end ;


  function TGIS_LayerTIFF.readCompressedLineOnFly(
    const _buffer : TBytes  ;
    const _offset : Integer ;
    const   _linenr : Integer ;
    const   _start  : Integer ;
    const   _bytes  : Integer ;
    const   _tilenr : Integer = -1
  ) : Integer ;
  var
    idx       : Integer ;
    srcptr    : TBytes  ;
    decno     : Integer ;
    start     : Integer ;
    bandtiles : Integer ;
    hws       : Integer ;
    tilenr    : Integer ;
    linenr   : Integer ;
  begin

    tilenr := _tilenr ;
    linenr := _linenr ;
    if _tilenr = -1 then begin
      decno := 0 ;
      if not isPlanar1 then begin
        hws := ((actualBitHeight +rowsPerStrip -1) div rowsPerStrip)*rowsPerStrip ;
        decno := _linenr div hws ;
        tilenr := _linenr div rowsPerStrip ;
        linenr := _linenr mod rowsPerStrip ;
      end;
    end
    else begin
      decno := _tilenr mod tilesColumns ;
      if not isPlanar1 then begin
        bandtiles := tilesColumns*tilesRows ;
        decno := decno +((_tilenr div bandtiles)*tilesColumns) ;
      end;
    end;

    if length(fileDecoder) = 0 then
     initDecoder ;

    if compressionType = TGIS_CompressionType.CCITTFaxGroup4 then
      fileDecoder[decno].Group4DecodeLine(_linenr, _tilenr)
    else
    if compressionType = TGIS_CompressionType.CCITTFaxGroup3 then
      fileDecoder[decno].Group3DecodeLine(_linenr, _tilenr)
    else
    if compressionType = TGIS_CompressionType.CCITTHuffmanRLE then
      fileDecoder[decno].HuffmanRLEDecodeLine(_linenr, _tilenr)
    else
    if compressionType = TGIS_CompressionType.LZW then
      fileDecoder[decno].LZWDecodeLine(linenr, tilenr)
    else
    if compressionType = TGIS_CompressionType.JPEG then begin
      fileDecoder[decno].JpegScale(jpegZoom ) ;
      fileDecoder[decno].ComponentsJpeg(componentsJpeg) ;
      fileDecoder[decno].NativeRequested(FIsGridImage) ;
      fileDecoder[decno].JpegDecodeLine(_linenr, _tilenr) ;
      fileDecoder[decno].NativeRequested(False) ;
    end
    else
    if compressionType = TGIS_CompressionType.ZLIB then
      fileDecoder[decno].ZLIBDecodeLine(_linenr, _tilenr)
    else
      fileDecoder[decno].PackBitsDecodeLine(linenr, tilenr) ;

    start := _start ;

    srcptr := fileDecoder[decno].DecodeState.LineBuffer ;
    for idx := 0 to _bytes -1 do
      _buffer[_offset+idx] := srcptr[start+idx] ;

    Result := _bytes ;
  end ;

  procedure TGIS_LayerTIFF.prepareAltitudeMapTable ;
  begin
    if mustPrepareMinMaxZ or (FMinZ >= FMaxZ) then
      prepareMinMaxZ ;
    inherited ;
  end;

  function TGIS_LayerTIFF.setFileScale(
    const _dwidth : Double ;
    const _swidth : Double
  ) : Double ;
  var
    nzoom : Double ;
    npage : Integer ;
    i     : Integer ;
  begin


    if  subViewsCount > 1 then begin
      npage := FCurrentPage ;
      if _swidth <> 0 then begin
        nzoom := _dwidth / _swidth ;

        if not assigned(pagesScales) then begin
          Result := 1 ;
          exit ;
        end;

        if (nzoom > 0) and (nzoom < 1) then begin
          for i := 0 to subViewsCount -2 do
            if pagesScales[i +1] < nzoom then break ;
          while (not validPagesTable[i]) or ( not validPagesTable[associatesPagesNo[i] -1]) do begin
            if i = 0 then
              break ;
            dec(i) ;
          end;

          npage := associatesPagesNo[i] ;
          actualZoom := pagesScales[i] ;
        end
        else begin
          if actualZoom <> 1 then begin
            actualZoom := 1 ;
            npage := 1 ;
          end ;
        end ;
      end ;
      if FCurrentPage <> npage then begin
        setPageInternal(npage) ;
      end ;
    end
    else begin
      if compressionType = TGIS_CompressionType.JPEG then begin
        if not assigned(fileDecoder) then
          initDecoder ;
        if _swidth > _dwidth then
          jpegZoom := fileDecoder[low(fileDecoder)].JpegScale(TruncS(_swidth/_dwidth))
        else
          jpegZoom := fileDecoder[low(fileDecoder)].JpegScale(1) ;

        actualZoom := 1.0/jpegZoom ;
      end
      else begin
        jpegZoom := 1 ;
        actualZoom := jpegZoom ;
      end;
    end;
    Result := actualZoom ;
  end ;

  procedure TGIS_LayerTIFF.checkTiff(const _stream : TGIS_HandleStream)  ;
  begin
    _stream.ReadWord( headerEndian  , 2 ) ;
    _stream.ReadWord( headerConfirm , 2 ) ;

    if headerEndian = TIFF_LITTLEENDIAN then begin
      bigEndian := False ;
      isValid   := True  ;
    end
    else begin
      if headerEndian = TIFF_BIGENDIAN then  begin
        bigEndian := True ;
        isValid   := True ;
        headerConfirm := Swap(headerConfirm) ;
      end
      else
        isValid   := False ;
    end ;

    if isValid then begin
      if headerConfirm = TIFF_CONFIRM then
        isBigTIFF := False
      else
      if headerConfirm = TIFF_BIGTIFFCONFIRM then
        isBigTIFF := True
      else
        isBigTIFF := False ;

      if isBigTIFF then begin
        _stream.ReadWord ( headerBigTIFFOffsetsSize, 2 ) ;
        _stream.ReadWord ( headerBigTIFFConstant   , 2 ) ;
        _stream.ReadInt64( headerFirstIFD64        , 8 ) ;
      end
      else
        _stream.ReadCardinal( headerFirstIFD, 4 ) ;
    end;

  end;

  procedure TGIS_LayerTIFF.setUp ;
  var
    i : Integer ;
  begin
    if isBuilt then
      exit ;
    // read bitmap parameters

    if assigned( Stream ) then begin
      Stream.Position := 0 ;
      fileStream := TGIS_MemoryStream.Create ;
      fileStream.CopyFrom( Stream, Stream.Size ) ;
      fileStream.Position := 0 ;
    end
    else begin
      if not assigned(fileStream) then
        fileStream := openBufferedFileStream( Path ) ;
      fileStream.Position := 0 ;
    end;

    checkTiff(fileStream) ;
   // Header is a valid Tiff Header
    if isValid then begin
      FIsGridImage := False ;
      likeGrid     := False ;
      signed16Bits := False ;
      signed32Bits := False ;
      FBandsCount  := 3 ;
      if bigEndian then begin
        headerFirstIFD := SwapLongInt( headerFirstIFD ) ;
      end ;
      if (headerConfirm <> TIFF_CONFIRM) and
         (headerConfirm <> TIFF_BIGTIFFCONFIRM) then
        isValid := False ;
    end ;

    FPagesCount := getPagesCount(fileStream) ;
    subViewsCount := FPagesCount ;
    actualZoom := 1 ;
    FCurrentPage := 1 ;

    choosePage( FCurrentPage ) ;
    if not isValid then
      exit ;
    inSetUp := True ;

    if FPagesCount > 1 then begin
      if getPagesScales then begin//One image in different scales
        FPagesCount := 1 ;
        setPage(1) ;
      end
      else begin
        for i := 1 to FPagesCount - 1 do
          if not validPagesTable[i] then
            dec(FPagesCount) ;
        subViewsCount := 1 ;
        setPage(1) ;
        FCurrentPage := 1 ;
      end ;
    end ;

    inSetUp := False ;

    if SafeFileExists( Path ) then
      FAge := GisFileAge( Path ) ;
    if IsNativeGridImage then
      FSubFormat := TGIS_LayerPixelSubFormat.Create( TGIS_PixelFormat.Custom,
                    False, TGIS_PixelSubFormat.GRID, TGIS_CompressionType.None, 1) ;

  end;

  procedure TGIS_LayerTIFF.setupParams ;
  begin
    // Setting page
    if FPagesCount > 1 then begin
      if (Params.Pixel.Page > 0) and (Params.Pixel.Page <= FPagesCount) then
        if Params.Pixel.Page <> FCurrentPage then begin
          setPage( Params.Pixel.Page) ;
        end;
    end ;

    inherited ;

    if isPartialTransparent then
      alphaAssociated := True ;

    if bandsDefinition.Bands.Count = 0 then
      setTiffBandsDefinition ;

    if FForcedBandsDefinition = '' then
      resolveBandDefintion ;

    if FForcedBandsDefinition <> '' then
      forceTiffBandsDefinition ;

  end ;

  function TGIS_LayerTIFF.fget_Capabilities : TGIS_LayerPixelSubFormatList ;
  var
    f : TGIS_FileTIFF ;
    {$IFDEF DCC}
      c : TGIS_LayerPixelSubFormat ;
    {$ENDIF}
  begin
    Result := inherited ;

    f := TGIS_FileTIFF.Create ;
    try
      Result.Clear ;

      for c in f.Capabilities do
        Result.Add( c.CreateCopy ) ;
    finally
      FreeObject( f ) ;
    end ;
  end ;

  function TGIS_LayerTIFF.fget_Page : Integer ;
  begin
    if FCurrentPage <> self.Params.Pixel.Page then
      setPage(self.Params.Pixel.Page) ;
    Result :=  self.Params.Pixel.Page ;
  end ;

  function TGIS_LayerTIFF.fget_MinHeight : Single ;
  begin
    if mustPrepareMinMaxZ then
      prepareMinMaxZ ;
    Result :=  FMinZ ;
  end ;

  function TGIS_LayerTIFF.fget_MaxHeight : Single ;
  begin
    if mustPrepareMinMaxZ then
      prepareMinMaxZ ;
    Result :=  FMaxZ ;
  end ;

  procedure TGIS_LayerTIFF.setPage(
    const _value : Integer
  ) ;
  begin
    if (_value > FPagesCount) OR (_value < 1) then
      exit ;
    if self.Params.Pixel.Page <> FCurrentPage then begin
      closeCurrentPage ;
      FBitWidth  := 0 ;
      FBitHeight := 0 ;
      FProjectedExtentRotationAngle := 0 ;
      if FPagesCount <> 1 then // different images
        RevertAll ;
      if FIsNativeGridImage then begin
        if FPagesCount > 1 then begin
          mustPrepareMinMaxZ := True ;
          FMaxZ   := -GIS_MAX_SINGLE  ;
          FMinZ   :=  GIS_MAX_SINGLE  ;
        end;
      end ;
      likeGrid := False ;
      FIsGridImage := False ;
      FIsNativeGridImage := False ;
      choosePage( _value ) ;
      FCellWidth := FBitWidth ;
      baseCellWidth := FBitWidth ;
      FCellHeight := FBitHeight ;
      baseCellHeight := FBitHeight ;
      FCurrentPage := _value ;
      RecalcProjectedExtent ;
    end;
  end ;

  procedure TGIS_LayerTIFF.setPageInternal(
    const _value : Integer
  ) ;
  begin
    closeCurrentPage ;
    FProjectedExtentRotationAngle := 0 ;

    FProjectedExtentViewerEPSG    := -1 ;  // enforce projection recalc
    FProjectedExtentEPSG          := -1 ;

    choosePage( _value ) ;
    FCurrentPage := _value ;
    RecalcProjectedExtent ;
  end ;

  {$IFNDEF OXYGENE}
    {$HINTS OFF} // we know that few variables were not used
  {$ENDIF}
  procedure TGIS_LayerTIFF.readGeoTiffCS ;
  var
    k    : Integer ;
    ptag : Integer ;
    txt  : String  ;

    lun  : TGIS_CSUnits ;
    aun  : TGIS_CSUnits ;
    flat : Double ;
    elp  : TGIS_CSEllipsoid ;
    pmd  : TGIS_CSPrimeMeridian ;
    dat  : TGIS_CSDatum ;
    gcs  : TGIS_CSGeographicCoordinateSystem ;
    pcs  : TGIS_CSProjectedCoordinateSystem  ;

    geokeys      : array of Cardinal  ;
    tiepoint     : array of Double ;
    trnsmatrix   : array of Double ;
    doubleparams : array of Double ;
    scale        : array of Double ;

    key : Cardinal ;

    keyGTModelType              : Integer ;
    keyGTRasterType             : Integer ;
    keyGTCitation               : String  ;
    keyGeographicType           : Integer ;
    keyGeogCitation             : String  ;
    keyGeogGeodeticDatum        : Integer ;
    keyGeogPrimeMeridian        : Integer ;
    keyGeogPrimeMeridianLong    : Double  ;
    keyGeogLinearUnits          : Integer ;
    keyGeogLinearUnitSize       : Double  ;
    keyGeogAngularUnits         : Integer ;
    keyGeogAngularUnitSize      : Double  ;
    keyGeogEllipsoid            : Integer ;
    keyGeogSemiMajorAxis        : Double  ;
    keyGeogSemiMinorAxis        : Double  ;
    keyGeogInvFlattering        : Double  ;
    keyGeogAzimuth              : Integer ;
    keyProjectedCSType          : Integer ;
    keyPCSCitation              : String  ;
    keyProjected                : Integer ;
    keyProjCoordTrans           : Integer ;
    keyProjLinearUnits          : Integer ;
    keyProjLinearUnitSize       : Double  ;
    keyProjStdParallel1         : Double  ;
    keyProjStdParallel2         : Double  ;
    keyProjNatOriginLong        : Double  ;
    keyProjNatOriginLat         : Double  ;
    keyProjFalseEasting         : Double  ;
    keyProjFalseNorthing        : Double  ;
    keyProjFalseOriginLong      : Double  ;
    keyProjFalseOriginLat       : Double  ;
    keyProjFalseOriginEasting   : Double  ;
    keyProjFalseOriginNorthing  : Double  ;
    keyProjCenterLong           : Double  ;
    keyProjCenterLat            : Double  ;
    keyProjCenterEasting        : Double  ;
    keyProjCenterNorthing       : Double  ;
    keyProjScaleAtNatOrigin     : Double  ;
    keyProjScaleAtCenter        : Double  ;
    keyProjAzimuthAngle         : Double  ;
    keyProjStraightVertPoleLong : Double  ;
    keyVerticalCSType           : SmallInt;
    keyVerticalCitation         : String  ;
    keyVerticalDatum            : SmallInt;
    keyVerticalUnits            : SmallInt;

    is_lun                      : Boolean ; // exact angular units by TAG
    is_aun                      : Boolean ; // exact linear units by TAG

    //set but not read
    is_elp                      : Boolean ; // exact ellipsoid by TAG
    is_pmd                      : Boolean ; // exact prime meridian by TAG
    is_dat                      : Boolean ; // exact datum by TAG
    is_gcs                      : Boolean ; // exact gcs by TAG
  const
    USER_DEFINED = 32767 ;

    function read_word
      : Word ;
    begin
      fileStream.ReadWord( Result, sizeOf(Result) ) ;
      if bigEndian then begin
          Result := Swap( Result ) ;
      end ;
    end ;

    function read_double
      : Double ;
    begin
      if bigEndian then
        Result := readDoubleBE
      else
        Result := readDoubleLE ;
    end ;

    function key_double
      : Double ;
    begin
      Result := doubleparams[ geokeys[ k + 3 ] ] ;
    end;

    function key_short
      : Integer ;
    begin
      Result := geokeys[ k + 3 ] ;
    end;

    function key_string
      : String ;
    begin
      Result := Copy( txt, geokeys[ k + 3 ] + StringFirst, geokeys[ k + 2 ] - 1 ) ;
    end;

    procedure add_info_double(
      const _name : String ;
      const _val  : Double
    ) ;
    begin
      FFileInfoGeoTIFF := FFileInfoGeoTIFF +#13#10 +
                          _name + ' = ' + DotFloatToStr( _val ) ;
    end;

    procedure add_info_int(
      const _name : String ;
      const _val  : Integer
    ) ;
    begin
      FFileInfoGeoTIFF := FFileInfoGeoTIFF +#13#10 +
                          _name + ' = ' + IntToStr( _val ) ;
    end;

    procedure add_info_string(
      const _name : String ;
      const _val  : String
    ) ;
    var
      tx : String ;
    begin
      {$IFDEF OXYGENE}
        tx := StringReplace( _val, #13, ' ', [TReplaceFlag.rfReplaceAll] ) ;
        tx := StringReplace( tx,   #10, '',  [TReplaceFlag.rfReplaceAll] ) ;
      {$ELSE}
        tx := StringReplace( _val, #13, ' ', [rfReplaceAll] ) ;
        tx := StringReplace( tx,   #10, '',  [rfReplaceAll] ) ;
      {$ENDIF}

      FFileInfoGeoTIFF := FFileInfoGeoTIFF +#13#10 +
                          _name + ' = ' + tx ;
    end;

    procedure add_info(
      const _name : String ;
      const _ar   : array of Double
    ) ;
    var
      i : Integer ;
    begin
      FFileInfoGeoTIFF := FFileInfoGeoTIFF +#13#10 +
                          _name + ' =' ;

      for i := low( _ar ) to high( _ar ) do begin
         FFileInfoGeoTIFF := FFileInfoGeoTIFF + ' ' + DotFloatToStr( _ar[i] ) ;
      end;
    end;

    function defval(
      const _default : Double ;
      const _value1  : Double ;
      const _value2  : Double = NaN ;
      const _value3  : Double = NaN
    ) : Double ;
    begin
      if IsNan( _value1 ) then begin
        if IsNan( _value2 ) then begin
          if IsNan( _value3 ) then begin
            Result := _default ;
          end
          else begin
            Result := _value3 ;
          end ;
        end
        else begin
          Result := _value2 ;
        end
      end
      else begin
        Result := _value1 ;
      end;
    end;

    function defdeg(
      const _default : Double ;
      const _value1  : Double ;
      const _value2  : Double = NaN ;
      const _value3  : Double = NaN
    ) : Double ;
    begin
      Result := DegToRad( defval( _default, _value1, _value2, _value3 ) ) ;
    end;

    function extract_ESRI_PE_String(
      const _txt : String
    ) : String ;
    var
      k   : Integer ;
      fnd : Boolean ;
      sleft  : TStringBuilder ;
      sright : TStringBuilder ;
    begin
      Result := '' ;

      sleft  :=  TStringBuilder.Create ;
      sright :=  TStringBuilder.Create ;
      try
        fnd := False ;

        for k := StringFirst to StringLast( _txt ) do begin
          if _txt[k] = '=' then begin
            if not fnd then begin
              fnd := True ;
              continue ;
            end
            else begin
              // unexpected
              sright.Clear ;
              break ;
            end;
          end;

          if not fnd then sleft.Append( _txt[k] )
                     else sright.Append( _txt[k] )

        end;

        if Trim( sleft.ToString ) = 'ESRI PE String' then
          Result := sright.ToString ;
      finally
        FreeObject( sleft  ) ;
        FreeObject( sright ) ;
      end;
    end;

    function pcs_by_citation(
      const _txt : String
    ) : TGIS_CSProjectedCoordinateSystem ;
    var
      wkt : String ;
      cs  : TGIS_CSCoordinateSystem ;
    begin
      Result := nil ;

      cs := TGIS_CSFactory.ByWKT( _txt ) ;
      if cs is TGIS_CSProjectedCoordinateSystem then begin
        Result := TGIS_CSProjectedCoordinateSystem( cs ) ;
        exit ;
      end;

      wkt := extract_ESRI_PE_String( _txt ) ;
      cs := TGIS_CSFactory.ByWKT( wkt ) ;
      if cs is TGIS_CSProjectedCoordinateSystem then begin
        Result := TGIS_CSProjectedCoordinateSystem( cs ) ;
        exit ;
      end;
    end;

    function setup_projection(
      const _gcs : TGIS_CSGeographicCoordinateSystem ;
      const _unt : TGIS_CSUnits
    ) : TGIS_CSProjectedCoordinateSystem ;
    var
      prm : TGIS_CSProjParameters ;
      prj : Integer ;
      tx  : String ;

      procedure setup_projection_ex ;
      var
        cs : TGIS_CSProjectedCoordinateSystem ;
        cd : Integer ;
      begin
        case keyProjected of
          10101 : cd := 26729 ;
          10102 : cd := 26730 ;
          10131 : cd := 26929 ;
          10132 : cd := 26930 ;
          10201 : cd := 26748 ;
          10202 : cd := 26749 ;
          10203 : cd := 26750 ;
          10231 : cd := 26948 ;
          10232 : cd := 26949 ;
          10233 : cd := 26950 ;
          10301 : cd := 26751 ;
          10302 : cd := 26752 ;
          10331 : cd := 26951 ;
          10332 : cd := 26952 ;
          10401 : cd := 26741 ;
          10402 : cd := 26742 ;
          10403 : cd := 26743 ;
          10404 : cd := 26744 ;
          10405 : cd := 26745 ;
          10406 : cd := 26746 ;
          10407 : cd := 26747 ;
          10431 : cd := 26941 ;
          10432 : cd := 26942 ;
          10433 : cd := 26943 ;
          10434 : cd := 26944 ;
          10435 : cd := 26945 ;
          10436 : cd := 26946 ;
          10501 : cd := 26753 ;
          10502 : cd := 26754 ;
          10503 : cd := 26755 ;
          10531 : cd := 26953 ;
          10532 : cd := 26954 ;
          10533 : cd := 26955 ;
          10600 : cd := 26756 ;
          10630 : cd := 26956 ;
          10700 : cd := 26757 ;
          10730 : cd := 26957 ;
          10901 : cd := 26758 ;
          10902 : cd := 26759 ;
          10903 : cd := 26760 ;
          10931 : cd := 26958 ;
          10932 : cd := 26959 ;
          10933 : cd := 26960 ;
          11001 : cd := 26766 ;
          11002 : cd := 26767 ;
          11031 : cd := 26966 ;
          11032 : cd := 26967 ;
          11101 : cd := 26768 ;
          11102 : cd := 26769 ;
          11103 : cd := 26770 ;
          11131 : cd := 26968 ;
          11132 : cd := 26969 ;
          11133 : cd := 26970 ;
          11201 : cd := 26771 ;
          11202 : cd := 26772 ;
          11231 : cd := 26971 ;
          11232 : cd := 26972 ;
          11301 : cd := 26773 ;
          11302 : cd := 26774 ;
          11331 : cd := 26973 ;
          11332 : cd := 26974 ;
          11401 : cd := 26775 ;
          11402 : cd := 26776 ;
          11431 : cd := 26975 ;
          11432 : cd := 26976 ;
          11501 : cd := 26777 ;
          11502 : cd := 26778 ;
          11531 : cd := 26977 ;
          11532 : cd := 26978 ;
          11601 : cd := 26779 ;
          11602 : cd := 26780 ;
          11631 : cd := 26979 ;
          11632 : cd := 26980 ;
          11701 : cd := 26781 ;
          11702 : cd := 26782 ;
          11703 : cd := 32099 ;
          11731 : cd := 26981 ;
          11732 : cd := 26982 ;
          11733 : cd := 32199 ;
          11801 : cd := 26783 ;
          11802 : cd := 26784 ;
          11831 : cd := 26983 ;
          11832 : cd := 26984 ;
          11900 : cd := 26785 ;
          11930 : cd := 26985 ;
          12001 : cd := 26786 ;
          12002 : cd := 26787 ;
          12031 : cd := 26986 ;
          12032 : cd := 26987 ;
          12101 : cd := 26801 ;
          12102 : cd := 26802 ;
          12103 : cd := 26803 ;
          12111 : cd := 26811 ;
          12112 : cd := 26812 ;
          12113 : cd := 26813 ;
          12141 : cd := 26988 ;
          12142 : cd := 26989 ;
          12143 : cd := 26990 ;
          12201 : cd := 26791 ;
          12202 : cd := 26792 ;
          12203 : cd := 26793 ;
          12231 : cd := 26991 ;
          12232 : cd := 26992 ;
          12233 : cd := 26993 ;
          12301 : cd := 26794 ;
          12302 : cd := 26795 ;
          12331 : cd := 26994 ;
          12332 : cd := 26995 ;
          12401 : cd := 26796 ;
          12402 : cd := 26797 ;
          12403 : cd := 26798 ;
          12431 : cd := 26996 ;
          12432 : cd := 26997 ;
          12433 : cd := 26998 ;
          12501 : cd := 32001 ;
          12502 : cd := 32002 ;
          12503 : cd := 32003 ;
          12530 : cd := 32100 ;
          12601 : cd := 32005 ;
          12602 : cd := 32006 ;
          12630 : cd := 32104 ;
          12701 : cd := 32007 ;
          12702 : cd := 32008 ;
          12703 : cd := 32009 ;
          12731 : cd := 32107 ;
          12732 : cd := 32108 ;
          12733 : cd := 32109 ;
          12800 : cd := 32010 ;
          12830 : cd := 32110 ;
          12900 : cd := 32011 ;
          12930 : cd := 32111 ;
          13001 : cd := 32012 ;
          13002 : cd := 32013 ;
          13003 : cd := 32014 ;
          13031 : cd := 32112 ;
          13032 : cd := 32113 ;
          13033 : cd := 32114 ;
          13101 : cd := 32015 ;
          13102 : cd := 32016 ;
          13103 : cd := 32017 ;
          13104 : cd := 32018 ;
          13131 : cd := 32115 ;
          13132 : cd := 32116 ;
          13133 : cd := 32117 ;
          13134 : cd := 32118 ;
          13200 : cd := 32019 ;
          13230 : cd := 32119 ;
          13301 : cd := 32020 ;
          13302 : cd := 32021 ;
          13331 : cd := 32120 ;
          13332 : cd := 32121 ;
          13401 : cd := 32022 ;
          13402 : cd := 32023 ;
          13431 : cd := 32122 ;
          13432 : cd := 32123 ;
          13501 : cd := 32024 ;
          13502 : cd := 32025 ;
          13531 : cd := 32124 ;
          13532 : cd := 32125 ;
          13601 : cd := 32026 ;
          13602 : cd := 32027 ;
          13631 : cd := 32126 ;
          13632 : cd := 32127 ;
          13701 : cd := 32028 ;
          13702 : cd := 32029 ;
          13731 : cd := 32128 ;
          13732 : cd := 32129 ;
          13800 : cd := 32030 ;
          13830 : cd := 32130 ;
          13901 : cd := 32031 ;
          13902 : cd := 32033 ;
          13930 : cd := 32133 ;
          14001 : cd := 32034 ;
          14002 : cd := 32035 ;
          14031 : cd := 32134 ;
          14032 : cd := 32135 ;
          14100 : cd := 32036 ;
          14130 : cd := 32136 ;
          14201 : cd := 32037 ;
          14202 : cd := 32038 ;
          14203 : cd := 32039 ;
          14204 : cd := 32040 ;
          14205 : cd := 32041 ;
          14231 : cd := 32137 ;
          14232 : cd := 32138 ;
          14233 : cd := 32139 ;
          14234 : cd := 32140 ;
          14235 : cd := 32141 ;
          14301 : cd := 32042 ;
          14302 : cd := 32043 ;
          14303 : cd := 32044 ;
          14331 : cd := 32142 ;
          14332 : cd := 32143 ;
          14333 : cd := 32144 ;
          14400 : cd := 32045 ;
          14430 : cd := 32145 ;
          14501 : cd := 32046 ;
          14502 : cd := 32047 ;
          14531 : cd := 32146 ;
          14532 : cd := 32147 ;
          14601 : cd := 32048 ;
          14602 : cd := 32049 ;
          14631 : cd := 32148 ;
          14632 : cd := 32149 ;
          14701 : cd := 32050 ;
          14702 : cd := 32051 ;
          14731 : cd := 32150 ;
          14732 : cd := 32151 ;
          14801 : cd := 32052 ;
          14802 : cd := 32053 ;
          14803 : cd := 32054 ;
          14831 : cd := 32152 ;
          14832 : cd := 32153 ;
          14833 : cd := 32154 ;
          14901 : cd := 32055 ;
          14902 : cd := 32056 ;
          14903 : cd := 32057 ;
          14904 : cd := 32058 ;
          14931 : cd := 32155 ;
          14932 : cd := 32156 ;
          14933 : cd := 32157 ;
          14934 : cd := 32158 ;
          15001 : cd := 26731 ;
          15002 : cd := 26732 ;
          15003 : cd := 26733 ;
          15004 : cd := 26734 ;
          15005 : cd := 26735 ;
          15006 : cd := 26736 ;
          15007 : cd := 26737 ;
          15008 : cd := 26738 ;
          15009 : cd := 26739 ;
          15010 : cd := 26740 ;
          15031 : cd := 26931 ;
          15032 : cd := 26932 ;
          15033 : cd := 26933 ;
          15034 : cd := 26934 ;
          15035 : cd := 26935 ;
          15036 : cd := 26936 ;
          15037 : cd := 26937 ;
          15038 : cd := 26938 ;
          15039 : cd := 26939 ;
          15040 : cd := 26940 ;
          15131 : cd := 26961 ;
          15132 : cd := 26962 ;
          15133 : cd := 26963 ;
          15134 : cd := 26964 ;
          15135 : cd := 26965 ;
          15201 : cd := 3991  ;
          15202 : cd := 3992  ;
          15230 : cd := 32161 ;
          15302 : cd := 2204  ;
          15303 : cd := 2205  ;
          15914 : cd := 32074 ;
          15915 : cd := 32075 ;
          15916 : cd := 32076 ;
          15917 : cd := 32077 ;
          16001 : cd := 32601 ;
          16002 : cd := 32602 ;
          16003 : cd := 32603 ;
          16004 : cd := 32604 ;
          16005 : cd := 32605 ;
          16006 : cd := 32606 ;
          16007 : cd := 32607 ;
          16008 : cd := 32608 ;
          16009 : cd := 32609 ;
          16010 : cd := 32610 ;
          16011 : cd := 32611 ;
          16012 : cd := 32612 ;
          16013 : cd := 32613 ;
          16014 : cd := 32614 ;
          16015 : cd := 32615 ;
          16016 : cd := 32616 ;
          16017 : cd := 32617 ;
          16018 : cd := 32618 ;
          16019 : cd := 32619 ;
          16020 : cd := 32620 ;
          16021 : cd := 32621 ;
          16022 : cd := 32622 ;
          16023 : cd := 32623 ;
          16024 : cd := 32624 ;
          16025 : cd := 32625 ;
          16026 : cd := 32626 ;
          16027 : cd := 32627 ;
          16028 : cd := 32628 ;
          16029 : cd := 32629 ;
          16030 : cd := 32630 ;
          16031 : cd := 32631 ;
          16032 : cd := 32632 ;
          16033 : cd := 32633 ;
          16034 : cd := 32634 ;
          16035 : cd := 32635 ;
          16036 : cd := 32636 ;
          16037 : cd := 32637 ;
          16038 : cd := 32638 ;
          16039 : cd := 32639 ;
          16040 : cd := 32640 ;
          16041 : cd := 32641 ;
          16042 : cd := 32642 ;
          16043 : cd := 32643 ;
          16044 : cd := 32644 ;
          16045 : cd := 32645 ;
          16046 : cd := 32646 ;
          16047 : cd := 32647 ;
          16048 : cd := 32648 ;
          16049 : cd := 32649 ;
          16050 : cd := 32650 ;
          16051 : cd := 32651 ;
          16052 : cd := 32652 ;
          16053 : cd := 32653 ;
          16054 : cd := 32654 ;
          16055 : cd := 32655 ;
          16056 : cd := 32656 ;
          16057 : cd := 32657 ;
          16058 : cd := 32658 ;
          16059 : cd := 32659 ;
          16060 : cd := 32660 ;
          16101 : cd := 32701 ;
          16102 : cd := 32702 ;
          16103 : cd := 32703 ;
          16104 : cd := 32704 ;
          16105 : cd := 32705 ;
          16106 : cd := 32706 ;
          16107 : cd := 32707 ;
          16108 : cd := 32708 ;
          16109 : cd := 32709 ;
          16110 : cd := 32710 ;
          16111 : cd := 32711 ;
          16112 : cd := 32712 ;
          16113 : cd := 32713 ;
          16114 : cd := 32714 ;
          16115 : cd := 32715 ;
          16116 : cd := 32716 ;
          16117 : cd := 32717 ;
          16118 : cd := 32718 ;
          16119 : cd := 32719 ;
          16120 : cd := 32720 ;
          16121 : cd := 32721 ;
          16122 : cd := 32722 ;
          16123 : cd := 32723 ;
          16124 : cd := 32724 ;
          16125 : cd := 32725 ;
          16126 : cd := 32726 ;
          16127 : cd := 32727 ;
          16128 : cd := 32728 ;
          16129 : cd := 32729 ;
          16130 : cd := 32730 ;
          16131 : cd := 32731 ;
          16132 : cd := 32732 ;
          16133 : cd := 32733 ;
          16134 : cd := 32734 ;
          16135 : cd := 32735 ;
          16136 : cd := 32736 ;
          16137 : cd := 32737 ;
          16138 : cd := 32738 ;
          16139 : cd := 32739 ;
          16140 : cd := 32740 ;
          16141 : cd := 32741 ;
          16142 : cd := 32742 ;
          16143 : cd := 32743 ;
          16144 : cd := 32744 ;
          16145 : cd := 32745 ;
          16146 : cd := 32746 ;
          16147 : cd := 32747 ;
          16148 : cd := 32748 ;
          16149 : cd := 32749 ;
          16150 : cd := 32750 ;
          16151 : cd := 32751 ;
          16152 : cd := 32752 ;
          16153 : cd := 32753 ;
          16154 : cd := 32754 ;
          16155 : cd := 32755 ;
          16156 : cd := 32756 ;
          16157 : cd := 32757 ;
          16158 : cd := 32758 ;
          16159 : cd := 32759 ;
          16160 : cd := 32760 ;
          16202 : cd := 28402 ;
          16203 : cd := 28403 ;
          16204 : cd := 20004 ;
          16205 : cd := 20005 ;
          17348 : cd := 28348 ;
          17349 : cd := 28349 ;
          17350 : cd := 28350 ;
          17351 : cd := 28351 ;
          17352 : cd := 28352 ;
          17353 : cd := 28353 ;
          17354 : cd := 28354 ;
          17355 : cd := 28355 ;
          17356 : cd := 28356 ;
          17357 : cd := 28357 ;
          17358 : cd := 28358 ;
          17448 : cd := 20248 ;
          17449 : cd := 20249 ;
          17450 : cd := 20250 ;
          17451 : cd := 20251 ;
          17452 : cd := 20252 ;
          17453 : cd := 20253 ;
          17454 : cd := 20254 ;
          17455 : cd := 20255 ;
          17456 : cd := 20256 ;
          17457 : cd := 20257 ;
          17458 : cd := 20258 ;
          18031 : cd := 22171 ;
          18032 : cd := 22172 ;
          18033 : cd := 22173 ;
          18034 : cd := 22174 ;
          18035 : cd := 22175 ;
          18036 : cd := 22176 ;
          18037 : cd := 22197 ;
          18051 : cd := 21891 ;
          18052 : cd := 21892 ;
          18053 : cd := 21893 ;
          18054 : cd := 21894 ;
          18072 : cd := 22992 ;
          18073 : cd := 22993 ;
          18074 : cd := 22994 ;
          18141 : cd := 27291 ;
          18142 : cd := 27292 ;
          19900 : cd := 20499 ;
          19905 : cd := 21100 ;
          19926 : cd := 31700 ;
          else    cd :=     0 ;
        end ;

        cs := CSProjectedCoordinateSystemList.ByEPSG( cd ) ;

        if assigned( cs ) then begin
          prj := cs.Projection.EPSG ;
          prm := cs.ProjectionParams.All ;
        end
        else begin
          prj := -1 ;
        end ;
      end ;

    begin
      Result := nil ;

      prj := -1 ;

      case keyProjCoordTrans of
         1 : // CT_TransverseMercator
           begin
             prj := CSPROJ_Transverse_Mercator ;
             tx  := 'CT_Transverse_Mercator' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjNatOriginLong     ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjNatOriginLat      ) ;
             prm.ScaleFactor        := defval( 0, keyProjScaleAtNatOrigin  ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end;
         2 : // CT_TransvMercator_Modified_Alaska,
           begin
             prj := -1 ;//?
             tx  := 'CT_TransvMercator_Modified_Alaska' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defval( 0, keyProjNatOriginLong     ) ;
             prm.LatitudeOfOrigin   := defval( 0, keyProjNatOriginLat      ) ;
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin  ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;

             assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
           end;
         3 : // CT_ObliqueMercator,
           begin
             prj := -1 ; //?9815 ; //?
             tx  := 'CT_ObliqueMercator' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjCenterLong        ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjCenterLat         ) ;
             prm.Azimuth            := defdeg( 0, keyProjAzimuthAngle      ) ;
             //?RectifiedGridAngle recitified_grid_angle Angular
             prm.ScaleFactor        := defval( 1, keyProjScaleAtCenter     ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end;
         4 : // CT_ObliqueMercator_Laborde,
           begin
             prj := -1 ; //?
             tx  := 'CT_ObliqueMercator_Laborde' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjCenterLong        ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjCenterLat         ) ;
             prm.Azimuth            := defdeg( 0, keyProjAzimuthAngle      ) ;
             //?RectifiedGridAngle recitified_grid_angle Angular (90)
             prm.ScaleFactor        := defval( 1, keyProjScaleAtCenter     ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end;
         5 : // CT_ObliqueMercator_Rosenmund,
           begin
             prj := -1 ; //?
             tx  := 'CT_ObliqueMercator_Rosenmund' ;
             // not implemented
             end;
         6 : // CT_ObliqueMercator_Spherical,
           begin
           assert( False ) ;
             prj := -1 ;
             // not implemented
           end;
         7 : // CT_Mercator,
           begin
             if IsNan( keyProjCenterLat    ) and
                IsNan( keyProjNatOriginLat ) and
                IsNan( keyProjStdParallel1 )
             then begin
               prj := CSPROJ_Mercator_1SP ;
               tx  := 'CT_Mercator' ;
             end
             else begin
               prj := CSPROJ_Mercator_2SP ;
               tx  := 'CT_Mercator_SP2' ;
             end ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;

             prm.CentralMeridian    := defdeg( 0,
                                               keyProjCenterLong      ,
                                               keyProjNatOriginLong
                                             ) ;
             prm.StandardParallel_1 := defdeg( 0,
                                               keyProjCenterLat       ,
                                               keyProjNatOriginLat    ,
                                               keyProjStdParallel1
                                             ) ;
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin  ) ;
             prm.FalseEasting       := defval( 0,
                                               keyProjCenterEasting   ,
                                               keyProjFalseEasting
                                             ) ;
             prm.FalseNorthing      := defval( 0,
                                               keyProjCenterNorthing  ,
                                               keyProjFalseNorthing
                                             ) ;
           end;
         8 : // CT_LambertConfConic_2SP,
           begin
             prj := CSPROJ_Lambert_Conformal_Conic_2SP ;
             tx  := 'CT_LambertConfConic_2SP' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0,
                                               keyProjFalseOriginLong ,
                                               keyProjNatOriginLong
                                             ) ;
             prm.LatitudeOfOrigin   := defdeg( 0,
                                               keyProjFalseOriginLat  ,
                                               keyProjNatOriginLat
                                             ) ;
             prm.StandardParallel_1 := defdeg( 0, keyProjStdParallel1      ) ;
             prm.StandardParallel_2 := defdeg( 0, keyProjStdParallel2      ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end;
         9 : // CT_LambertConfConic_1SP
           begin
             prj := CSPROJ_Lambert_Conformal_Conic_1SP ;
             tx  := 'CT_LambertConfConic_1SP' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defval( 0, keyProjNatOriginLong     ) ;
             prm.LatitudeOfOrigin   := defval( 0, keyProjNatOriginLat      ) ;
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin  ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end;
         10 : // CT_LambertAzimEqualArea,
           begin
             prj := CSPROJ_Lambert_Azimuthal_Equal_Area ;
             tx  := 'CT_LambertAzimEqualArea' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0,
                                               keyProjCenterLong      ,
                                               keyProjNatOriginLong
                                             ) ;
             prm.LatitudeOfOrigin   := defdeg( 0,
                                               keyProjCenterLat       ,
                                               keyProjNatOriginLat
                                             ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end;
         11 : // CT_AlbersEqualArea,
           begin
             prj := CSPROJ_Albers ;
             tx  := 'CT_AlbersEqualArea' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjNatOriginLong     ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjNatOriginLat      ) ;
             prm.StandardParallel_1 := defdeg( 29.5, keyProjStdParallel1   ) ;
             prm.StandardParallel_2 := defdeg( 45.4, keyProjStdParallel2   ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end;
         12 : // CT_AzimuthalEquidistant,
           begin
             prj := CSPROJ_Azimuthal_Equidistance ;
             tx  := 'CT_AzimuthalEquidistant' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.LongitudeOfCenter  := defdeg( 0, keyProjCenterLong        ) ;
             prm.LatitudeOfCenter   := defdeg( 0, keyProjCenterLat         ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end;
         13 : // CT_EquidistantConic,
           begin
             prj := CSPROJ_Equidistant_Conic ;
             tx  := 'CT_EquidistantConic' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.StandardParallel_1 := defdeg( 20.0, keyProjStdParallel1   ) ;
             prm.StandardParallel_2 := defdeg( 60.0, keyProjStdParallel2   ) ;
             prm.LongitudeOfCenter  := defdeg( 0, keyProjCenterLong        ) ;
             prm.LatitudeOfCenter   := defdeg( 0, keyProjCenterLat         ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end;
         14 : // CT_Stereographic,
           begin
             prj := CSPROJ_Double_Stereographic ;
             tx  := 'CT_Stereographic' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0,
                                               keyProjCenterLong      ,
                                               keyProjNatOriginLong
                                             ) ;
             prm.LatitudeOfOrigin   := defdeg( 0,
                                               keyProjCenterLat       ,
                                               keyProjNatOriginLat
                                             ) ;
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin  ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end;
         15 : // CT_PolarStereographic,
           begin
             prj := CSPROJ_Polar_Stereographic ;
             tx  := 'CT_PolarStereographic' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0,
                                               keyProjStraightVertPoleLong,
                                               keyProjNatOriginLong
                                             ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjNatOriginLat      ) ;
             prm.StandardParallel_1 := defdeg( 0,
                                               keyProjCenterLat       ,
                                               keyProjNatOriginLat
                                             ) ;
             prm.StandardParallel_1 := defdeg( 0, prm.StandardParallel_1   ) ;
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin  ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end;
         16 : // CT_ObliqueStereographic,
           begin
             prj := 9809 ;
             tx  := 'CT_ObliqueStereographic' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjNatOriginLong     ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjNatOriginLat      ) ;
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin  ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
          end;
         17 : // CT_Equirectangular,
           begin
             prj := CSPROJ_Plate_Carree ;
             tx  := 'CT_Equirectangular' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjCenterLong        ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjCenterLat         ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
             prm.StandardParallel_1 := defdeg( 0, keyProjStdParallel1      ) ;
           end;
         18 : // CT_CassiniSoldner,
           begin
             prj := CSPROJ_Cassini_Soldner ;
             tx  := 'CT_CassiniSoldner' ;

             prm.CentralMeridian    := defdeg( 0, keyProjNatOriginLong     ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjNatOriginLat      ) ;
             prm.ScaleFactor        := defval( 0, keyProjScaleAtNatOrigin  ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end;
         19 : // CT_Gnomonic,
           begin
             prj := CSPROJ_Gnomic ;
             tx  := 'CT_Gnomic' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjCenterLong        ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjCenterLat         ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end;
         20 : // CT_MillerCylindrical,
           begin
             prj := CSPROJ_Miller_Cylindrical ;
             tx  := 'CT_MillerCylindrical' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0,
                                               keyProjCenterLong      ,
                                               keyProjNatOriginLong
                                             ) ;
             prm.LatitudeOfOrigin   := defdeg( 0,
                                               keyProjCenterLat       ,
                                               keyProjNatOriginLat
                                             ) ;
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin  ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end;
         21 : // CT_Orthographic,
           begin
             prj := CSPROJ_Orthographic ;
             tx  := 'CT_Orthographic' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjCenterLong        ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjCenterLat         ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end;
         22 : // CT_Polyconic,
           begin
             prj := CSPROJ_Polyconic ;
             tx  := 'CT_Polyconic' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjNatOriginLong     ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjNatOriginLat      ) ;
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin  ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end;
         23 : // CT_Robinson,
           begin
             prj := CSPROJ_Robinson ;
             tx  := 'CT_Robinson' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0,
                                               keyProjCenterLong      ,
                                               keyProjNatOriginLong
                                             ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end;
         24 : // CT_Sinusoidal,
           begin
             prj := CSPROJ_Sinusoidal ;
             tx  := 'CT_Sinusoidal' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0,
                                               keyProjCenterLong      ,
                                               keyProjNatOriginLong
                                             ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end;
         25 : // CT_VanDerGrinten,
           begin
             prj := CSPROJ_van_der_Grinten_I ;
             tx  := 'CT_VanDerGrinten' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0,
                                               keyProjCenterLong      ,
                                               keyProjNatOriginLong
                                             ) ;
             prm.CentralMeridian    := defdeg( 0, prm.CentralMeridian      ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end;
         26 : // CT_NewZealandMapGrid,
           begin
             prj := CSPROJ_New_Zealand_Map_Grid ;
             tx  := 'CT_NewZealandMapGrid' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 173.0, keyProjNatOriginLong   ) ;
             prm.LatitudeOfOrigin   := defdeg( -41.0, keyProjNatOriginLat    ) ;
             prm.FalseEasting       := defval( 2510000, keyProjFalseEasting  ) ;
             prm.FalseNorthing      := defval( 6023150, keyProjFalseNorthing ) ;
           end;
         27 : // CT_TransvMercator_SouthOrientated,
           begin
             prj := CSPROJ_Oblique_Stereographic ;
             tx  := 'CT_TransvMercator_SouthOrientated' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defval( 0, keyProjNatOriginLong     ) ;
             prm.LatitudeOfOrigin   := defval( 0, keyProjNatOriginLat      ) ;
             prm.LatitudeOfOrigin   := defval( 0, keyProjNatOriginLat      ) ;
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin  ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end;
         28 : // CT_CylindricalEqualArea,
           begin
             prj := CSPROJ_Cylindrical_Equal_Area ;
             tx  := 'CT_CylindricalEqualArea' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjNatOriginLong     ) ;
             prm.StandardParallel_1 := defdeg( 0, keyProjStdParallel1      ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end;
         -1  : // Undefined
           begin
             setup_projection_ex ;
           end ;
         USER_DEFINED : // User defined
           begin
             setup_projection_ex ;
           end
         else
           begin
             assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
             exit ;
           end;
      end ;

      if prj < 0 then exit ;

      if (length( keyPCSCitation ) > 5) and (length( keyPCSCitation ) < 100) then
      begin
        // some significant citation
        {$IFDEF OXYGENE}
          tx := StringReplace( keyPCSCitation, #13, ' ', [TReplaceFlag.rfReplaceAll] ) ;
          tx := StringReplace( tx, #10, '', [TReplaceFlag.rfReplaceAll] ) ;
        {$ELSE}
          tx := StringReplace( keyPCSCitation, #13, ' ', [rfReplaceAll] ) ;
          tx := StringReplace( tx, #10, '', [rfReplaceAll] ) ;
        {$ENDIF}
      end
      else
        tx := tx + '_' + _gcs.WKT ;

      Result := CSProjectedCoordinateSystemList.Prepare(
                  -1, tx, _gcs.EPSG, _unt.EPSG, prj, prm
                ) ;

    end;

  begin
    ptag := getTagTiff( GEOTIFF_TAG_GEOKEYDIRECTORY ) ;
    if ptag <> -1  then begin
      FFileInfoGeoTIFF := #13#10#13#10 + 'GEOTIFF:' ;

      fileStream.Position := listTag64[ptag].Value ;
      SetLength( geokeys, listTag64[ptag].Count ) ;
      for k:= low( geokeys ) to high( geokeys ) do begin
         geokeys[k] := read_word ;
      end ;
    end ;

    ptag := getTagTiff( GEOTIFF_TAG_MODELTIEPOINT ) ;
    if ptag <> -1 then begin
      fileStream.Position := listTag64[ptag].Value ;
      SetLength( tiepoint, listTag64[ptag].Count ) ;
      for k:= low( tiepoint ) to high( tiepoint ) do begin
         tiepoint[k] := read_double ;
      end ;
      add_info( 'ModelTiePointTag', tiepoint );
    end ;

    ptag := getTagTiff( GEOTIFF_TAG_MODELTRANSFORMATION ) ;
    if ptag <> -1 then begin
      fileStream.Position := listTag64[ptag].Value ;
      SetLength( trnsmatrix, listTag64[ptag].Count ) ;
      for k:= low( trnsmatrix ) to high( trnsmatrix ) do begin
         trnsmatrix[k] := read_double ;
      end ;
      add_info( 'ModelTransformationTag', trnsmatrix );
    end ;

    ptag := getTagTiff( GEOTIFF_TAG_MODELPIXELSCALE ) ;
    if ptag <> -1 then begin
      fileStream.Position := listTag64[ptag].Value ;
      SetLength( scale, listTag64[ptag].Count ) ;
      for k:= low( scale ) to high( scale ) do begin
         scale[k] := read_double ;
      end ;
      add_info( 'ModelPixelScaleTag', scale );
    end ;

    ptag := getTagTiff( GEOTIFF_TAG_GEOASCIIPARAMS ) ;
    if ptag <> -1  then begin
      txt := getTagAscii( ptag ) ;
    end ;

    ptag := getTagTiff( GEOTIFF_TAG_GEODOUBLEPARAMS ) ;
    if ptag <> -1  then begin
      fileStream.Position := listTag64[ptag].Value ;
      SetLength( doubleparams, listTag64[ptag].Count ) ;
      for k:= low( doubleparams ) to high( doubleparams ) do begin
         doubleparams[k] := read_double ;
      end ;
    end ;

    if high( geokeys ) < 4 then exit ;

    keyGTModelType                 := -1   ;
    keyGTRasterType                := -1   ;
    keyGTCitation                  := ''   ;
    keyGeographicType              := -1   ;
    keyGeogCitation                := ''   ;
    keyGeogGeodeticDatum           := -1   ;
    keyGeogPrimeMeridian           := 8901 ;
    keyGeogPrimeMeridianLong       := NaN  ;
    keyGeogLinearUnits             := 9001 ;
    keyGeogLinearUnitSize          := NaN  ;
    keyGeogAngularUnits            := 9102 ;
    keyGeogAngularUnitSize         := NaN  ;
    keyGeogEllipsoid               := -1   ;
    keyGeogSemiMajorAxis           := NaN  ;
    keyGeogSemiMinorAxis           := NaN  ;
    keyGeogInvFlattering           := NaN  ;
    keyGeogAzimuth                 := -1   ;
    keyProjectedCSType             := -1   ;
    keyPCSCitation                 := ''   ;
    keyProjected                   := -1   ;
    keyProjCoordTrans              := -1   ;
    keyProjLinearUnits             := 9001 ;
    keyProjLinearUnitSize          := NaN  ;
    keyProjStdParallel1            := NaN  ;
    keyProjStdParallel2            := NaN  ;
    keyProjNatOriginLong           := NaN  ;
    keyProjNatOriginLat            := NaN  ;
    keyProjFalseEasting            := NaN  ;
    keyProjFalseNorthing           := NaN  ;
    keyProjFalseOriginLong         := NaN  ;
    keyProjFalseOriginLat          := NaN  ;
    keyProjFalseOriginEasting      := NaN  ;
    keyProjFalseOriginNorthing     := NaN  ;
    keyProjCenterLong              := NaN  ;
    keyProjCenterLat               := NaN  ;
    keyProjCenterEasting           := NaN  ;
    keyProjCenterNorthing          := NaN  ;
    keyProjScaleAtNatOrigin        := NaN  ;
    keyProjScaleAtCenter           := NaN  ;
    keyProjAzimuthAngle            := NaN  ;
    keyProjStraightVertPoleLong    := NaN  ;
    keyVerticalCSType              := -1   ;
    keyVerticalCitation            := ''   ;
    keyVerticalDatum               := -1   ;
    keyVerticalUnits               := -1   ;

    // interpret table ;
    k := 4 ;

    while k <= high( geokeys ) do begin
      key := geokeys[ k ] ;
      case key of
        0    : // Empty key
          begin
          end ;
        1024 : // GTModelType
          begin
            keyGTModelType := key_short ;
            add_info_int( 'GTModelTypeGeoKey', keyGTModelType ) ;
          end ;
        1025 : // GTRasterType
          begin
            keyGTRasterType := key_short ;
            add_info_int( 'GTRasterTypeGeoKey', keyGTRasterType ) ;
          end ;
        1026 : // GTCitation
          begin
            keyGTCitation := key_string ;
            add_info_string( 'GTCitationGeoKey', keyGTCitation ) ;
          end ;
        2048 : // GeographicType
          begin
            keyGeographicType := key_short ;
            add_info_int( 'GeographicTypeGeoKey', keyGeographicType ) ;
          end;
        2049 : // GeogCitation
          begin
            keyGeogCitation := key_string ;
            add_info_string( 'GeogCitationGeoKey', keyGeogCitation ) ;
          end ;
        2050 : // GeogGeodeticDatum
          begin
            keyGeogGeodeticDatum := key_short ;
            add_info_int( 'GeogGeodeticDatumGeoKey', keyGeogGeodeticDatum ) ;
          end ;
        2051 : // GeogPrimeMeridian
          begin
            keyGeogPrimeMeridian := key_short ;
            add_info_int( 'GeogPrimeMeridianKey', keyGeogPrimeMeridian) ;
          end ;
        2061 : // GeogPrimeMeridianLong
          begin
            keyGeogPrimeMeridianLong := key_double ;
            add_info_double( 'GeogPrimeMeridianLongGeoKey', keyGeogPrimeMeridianLong ) ;
          end ;
        2052 : // GeogLinearUnits
          begin
            keyGeogLinearUnits := key_short ;
            add_info_int( 'GeogLinearUnitsGeoKey', keyGeogLinearUnits ) ;
          end ;
        2053 : // GeogLinearUnitSize
          begin
            keyGeogLinearUnitSize := key_double ;
            add_info_double( 'GeogLinearUnitSizeGeoKey', keyGeogLinearUnitSize ) ;
          end ;
        2054 : // GeogAngularUnits
          begin
            keyGeogAngularUnits := key_short ;
            add_info_int( 'GeogAngularUnitsGeoKey', keyGeogAngularUnits ) ;
          end ;
        2055 : // GeogAngularUnitSize
          begin
            keyGeogAngularUnitSize := key_double ;
            add_info_double( 'GeogAngularUnitSizeKey', keyGeogAngularUnitSize ) ;
          end ;
        2056 : // GeogEllipsoid
          begin
            keyGeogEllipsoid := key_short ;
            add_info_int( 'GeogEllipsoidGeoKey', keyGeogEllipsoid ) ;
          end ;
        2057 : // GeogSemiMajorAxis
          begin
            keyGeogSemiMajorAxis := key_double ;
            add_info_double( 'GeogSemiMajorAxisGeoKey', keyGeogSemiMajorAxis ) ;
          end ;
        2058 : // GeogSemiMinorAxis
          begin
            keyGeogSemiMinorAxis := key_double ;
            add_info_double( 'GeogSemiMinorAxisGeoKey', keyGeogSemiMinorAxis ) ;
          end ;
        2059 : // GeogInvFlattering
          begin
            keyGeogInvFlattering := key_double ;
            add_info_double( 'GeogInvFlatteringGeoKey', keyGeogInvFlattering ) ;
          end ;
        2060 : // GeogAzimuth
          begin
            keyGeogAzimuth := key_short ;
            add_info_int( 'GeogAzimuthGeoKey', keyGeogAzimuth ) ;
          end ;
        3072 : // ProjectedCSType
          begin
            keyProjectedCSType := key_short ;
            add_info_int( 'ProjectedCSTypeGeoKey', keyProjectedCSType ) ;
          end ;
        3073 : // PCSCitation
          begin
            keyPCSCitation := key_string ;
            add_info_string( 'PCSCitationGeoKey', keyPCSCitation ) ;
          end ;
        3074 : // Projected
          begin
            keyProjected := key_short ;
            add_info_int( 'ProjectedGeoKey', keyProjected ) ;
          end ;
        3075 : // ProjCoordTrans
          begin
            keyProjCoordTrans := key_short ;
            add_info_int( 'ProjCoordTransGeoKey', keyProjCoordTrans ) ;
            if keyProjCoordTrans = USER_DEFINED then
              exit ;
          end ;
        3076 : // ProjLinearUnits
          begin
            keyProjLinearUnits := key_short ;
            add_info_int( 'ProjLinearUnitsGeoKey', keyProjLinearUnits ) ;
          end ;
        3077 : // ProjLinearUnitSize
          begin
            keyProjLinearUnitSize := key_double ;
           add_info_double( 'ProjLinearUnitSizeGeoKey', keyProjLinearUnitSize ) ;
          end ;
        3078 : // ProjStdParallel1
          begin
            keyProjStdParallel1 := key_double ;
            add_info_double( 'ProjStdParallel1GeoKey', keyProjStdParallel1 ) ;
          end ;
        3079 : // ProjStdParallel2
          begin
            keyProjStdParallel2 := key_double ;
            add_info_double( 'ProjStdParallel2GeoKey', keyProjStdParallel2 ) ;
          end ;
        3080 : // ProjNatOriginLong
          begin
            keyProjNatOriginLong := key_double ;
            add_info_double( 'ProjNatOriginLongGeoKey', keyProjNatOriginLong ) ;
          end ;
        3081 : // ProjNatOriginLat
          begin
            keyProjNatOriginLat := key_double ;
            add_info_double( 'ProjNatOriginLatGeoKey', keyProjNatOriginLat ) ;
          end ;
        3082 : // ProjFalseEasing
          begin
            keyProjFalseEasting := key_double ;
            add_info_double( 'ProjFalseEastingGeoKey', keyProjFalseEasting ) ;
          end ;
        3083 : // ProjFalseNorthing
          begin
            keyProjFalseNorthing := key_double ;
            add_info_double( 'ProjFalseNorthingGeoKey', keyProjFalseNorthing ) ;
          end ;
        3084 : // ProjFalseOriginLong
          begin
            keyProjFalseOriginLong := key_double ;
            add_info_double( 'ProjFalseOriginLongGeoKey', keyProjFalseOriginLong ) ;
          end ;
        3085 : // ProjFalseOriginLat
          begin
            keyProjFalseOriginLat := key_double ;
            add_info_double( 'GeogProjFalseOriginLat', keyProjFalseOriginLat ) ;
          end ;
        3086 : // ProjFalseOriginEasting
          begin
            keyProjFalseOriginEasting := read_double ;
            add_info_double( 'ProjFalseOriginEastingGeoKey', keyProjFalseOriginEasting ) ;
          end ;
        3087 : // ProjFalseOriginNorthing
          begin
            keyProjFalseOriginNorthing := read_double ;
            add_info_double( 'ProjFalseOriginNorthingGeoKey', keyProjFalseOriginNorthing ) ;
          end ;
        3088 : // ProjCenterLong
          begin
            keyProjCenterLong := key_double ;
            add_info_double( 'ProjCenterLongGeoKey', keyProjCenterLong ) ;
          end ;
        3089 : // ProjCenterLat
          begin
            keyProjCenterLat := key_double ;
            add_info_double( 'ProjCenterLatGeoKey', keyProjCenterLat ) ;
          end ;
        3090 : // ProjCenterEasting
          begin
            keyProjCenterEasting := key_double ;
            add_info_double( 'ProjCenterEastingGeoKey', keyProjCenterEasting ) ;
          end ;
        3091 : // ProjCenterNorthing
          begin
            keyProjCenterNorthing := key_double ;
            add_info_double( 'ProjProjCenterNorthingGeoKey', keyProjCenterNorthing ) ;
          end ;
        3092 : // ProjScaleAtNatOrigin
          begin
            keyProjScaleAtNatOrigin := key_double ;
            add_info_double( 'ProjScaleAtNatOriginGeoKey', keyProjScaleAtNatOrigin ) ;
          end ;
        3093 : // ProjScaleAtCenter
          begin
            keyProjScaleAtCenter := key_double ;
            add_info_double( 'ProjScaleAtCenterGeoKey', keyProjScaleAtCenter ) ;
          end ;
        3094 : // ProjAzimuthAngle
          begin
            keyProjAzimuthAngle := key_double ;
            add_info_double( 'ProjAzimuthAngleGeoKey', keyProjAzimuthAngle ) ;
          end ;
        3095 : // ProjStraightVertPoleLong
          begin
            keyProjStraightVertPoleLong := key_double ;
            add_info_double( 'ProjStraightVertPoleLongGeoKey', keyProjStraightVertPoleLong ) ;
          end ;
        4096 : // VerticalCSType
          begin
            keyVerticalCSType := key_short ;
            add_info_int( 'VerticalCSTypeGeoKey', keyVerticalCSType ) ;
          end ;
        4097 : // VerticalCitation
          begin
            keyVerticalCitation := key_string ;
            add_info_string( 'VerticalCitationGeoKey', keyVerticalCitation ) ;
          end ;
        4098 : // VerticalDatum
          begin
            keyVerticalDatum := key_short ;
            add_info_int( 'VerticalDatumGeoKey', keyVerticalDatum ) ;
          end ;
        4099 : // VerticalUnits
          begin
            keyVerticalUnits := key_short ;
            add_info_int( 'GeogVerticalUnits', keyVerticalUnits ) ;
          end ;
        else
          begin
            add_info_int( 'Unknown', key ) ;
          end ;
      end ;
      inc( k, 4 ) ;
    end ;

    is_aun := False ;
    is_lun := False ;

    //set but not read
    is_elp := False ;

    is_pmd := False ;
    is_dat := False ;
    is_gcs := False ;

    // construct angular units
       aun := nil ;
       if ( keyGeogAngularUnits > 0            ) and
          ( keyGeogAngularUnits < USER_DEFINED )
       then begin
         aun := CSUnitsList.ByEPSG( keyGeogAngularUnits ) ;
         is_aun := True ;
       end
       else begin
         if not IsNan( keyGeogAngularUnitSize ) then begin
           aun := CSUnitsList.Prepare(
                    -1, 'Custom_Angular', '',
                    TGIS_CSUnitsType.Angular, keyGeogAngularUnitSize
                  )
         end;
       end ;



    // construct linear units
       lun := nil ;
       if ( keyProjLinearUnits > 0            ) and
          ( keyProjLinearUnits < USER_DEFINED )
       then begin
         lun := CSUnitsList.ByEPSG( keyProjLinearUnits ) ;
         is_lun := True ;
       end
       else begin
         if not IsNan( keyProjLinearUnitSize ) then begin
           lun := CSUnitsList.Prepare(
                    -1, 'Custom_Linear', '',
                    TGIS_CSUnitsType.Linear, keyProjLinearUnitSize
                  )
         end;
       end ;

    // construct ellipsoid
      elp := nil ;
      if ( keyGeogEllipsoid > 0            ) and
         ( keyGeogEllipsoid < USER_DEFINED )
      then begin
        elp := CSEllipsoidList.ByEPSG( keyGeogEllipsoid ) ;

        //set but not read
        is_elp := True ;
      end
      else begin
        if not IsNan( keyGeogSemiMajorAxis ) then begin
          flat := 0 ;

          if not IsNan( keyGeogSemiMinorAxis ) and
             ( ( keyGeogSemiMajorAxis - keyGeogSemiMinorAxis ) > 0 ) then
          begin
            flat := keyGeogSemiMajorAxis /
                    ( keyGeogSemiMajorAxis - keyGeogSemiMinorAxis ) ;
          end
          else
          if not IsNan( keyGeogInvFlattering ) then
          begin
            flat := keyGeogInvFlattering ;
          end ;

          elp := CSEllipsoidList.Prepare(
                   -1, 'Custom_Ellipsoid', keyGeogSemiMajorAxis, flat )
        end ;
      end ;

    // construct primem
       pmd := nil ;
       if ( keyGeogPrimeMeridian > 0            ) and
          ( keyGeogPrimeMeridian < USER_DEFINED )
       then begin
         pmd := CSPrimeMeridianList.ByEPSG( keyGeogPrimeMeridian ) ;
         is_pmd := True ;
       end
       else begin
         if not IsNan( keyGeogPrimeMeridianLong ) then begin
           pmd := CSPrimeMeridianList.Prepare(
                    -1, 'Custom_Primem', DegToRad( keyGeogPrimeMeridianLong )
                  ) ;
         end;
       end ;

    // construct datum
       dat := nil ;
       if ( keyGeogGeodeticDatum > 0            ) and
          ( keyGeogGeodeticDatum < USER_DEFINED )
       then begin
         dat := CSDatumList.ByEPSG( keyGeogGeodeticDatum ) ;
         is_dat := True ;
       end
       else begin
         if assigned( elp ) then begin
           dat := CSDatumList.Prepare(
                    -1, 'Based_on_' + elp.WKT, elp.EPSG, 0
                  ) ;
         end;
       end ;

    // construct GCS
       gcs := nil ;
       if ( keyGeographicType > 0            ) and
          ( keyGeographicType < USER_DEFINED )
       then begin
         gcs := CSGeographicCoordinateSystemList.ByEPSG( keyGeographicType ) ;
         is_gcs := True ;
       end ;

       if assigned( dat ) and
          assigned( pmd ) and
          assigned( aun ) then
       begin
         if assigned( gcs ) and
            (
              ( ( gcs.Datum.EPSG         <> dat.EPSG ) and is_dat ) or
              ( ( gcs.PrimeMeridian.EPSG <> pmd.EPSG ) and is_pmd ) or
              ( ( gcs.Units.EPSG         <> aun.EPSG ) and is_aun )
            )
         then
           gcs := nil ;

         if not assigned( gcs ) then
           gcs := CSGeographicCoordinateSystemList.Prepare(
                    -1, 'Custom_GCS_' + dat.WKT , dat.EPSG, pmd.EPSG, aun.EPSG
                  )
       end ;

    // construct PCS
       pcs := nil ;
       if ( keyProjectedCSType > 0            ) and
          ( keyProjectedCSType < USER_DEFINED )
       then
         pcs := CSProjectedCoordinateSystemList.ByEPSG( keyProjectedCSType ) ;

       if assigned( pcs ) and assigned( gcs ) then begin
         if ( pcs.Geocs.EPSG <> gcs.EPSG ) and is_gcs then begin
           pcs := CSProjectedCoordinateSystemList.Prepare(
                    -1, 'Custom_PCS_' + pcs.WKT ,
                    gcs.EPSG,
                    pcs.Units.EPSG,
                    pcs.Projection.EPSG,
                    pcs.Projection.Parameters
                  )
         end;
       end;

       if assigned( pcs ) and assigned( lun ) then begin
         if ( pcs.Units.EPSG <> lun.EPSG ) and is_lun then begin
           pcs := CSProjectedCoordinateSystemList.Prepare(
                    -1, 'Custom_PCS_' + pcs.WKT ,
                    pcs.Geocs.EPSG,
                    lun.EPSG,
                    pcs.Projection.EPSG,
                    pcs.Projection.Parameters
                  )
         end;
       end;

       if not assigned( pcs ) then begin
         if assigned( gcs ) and assigned( lun ) then begin
           pcs := setup_projection( gcs, lun ) ;
         end ;
       end ;

       if not assigned( pcs ) then begin
         if keyProjectedCSType = USER_DEFINED then begin
           pcs := pcs_by_citation( keyPCSCitation ) ;
         end ;
       end ;


    // final assignments
      case keyGTModelType  of
        1 :
          begin
            if self.CS.EPSG = 0 then begin
              if assigned( pcs ) then
                self.CS := pcs
              else
                self.CS := CSUnknownCoordinateSystem ;
            end ;
          end ;
        2 :
          begin
            if self.CS.EPSG = 0 then begin
              if assigned( gcs ) then
                self.CS := gcs
              else
                self.CS := CSUnknownCoordinateSystem ;
            end ;
          end ;
        USER_DEFINED :
          begin
            if self.CS.EPSG = 0 then begin
              if assigned( pcs ) then
                self.CS := pcs
              else
              if assigned( gcs ) then
                self.CS := gcs
              else
                self.CS := CSUnknownCoordinateSystem ;
            end ;
          end ;
        else
          begin
            if self.CS.EPSG = 0 then begin
              self.CS := CSUnknownCoordinateSystem ;
            end ;
          end ;
      end;
  end;

  {$IFNDEF OXYGENE}
    {$HINTS ON}
  {$ENDIF}

  procedure TGIS_LayerTIFF.choosePage(
    const _page : Integer = 1
  ) ;
  var
    i, j              : Integer ;
    ptag              : Integer ;
    tiepoint          : Array of Double ;
    geokeys           : Array of Array of Word ;
    doubleparams      : Array of Double ;
    scale_x           : Double ;
    scale_y           : Double ;
    //scale_z           : Double ;
    ext               : TGIS_Extent {$IFDEF GIS_NORECORDS} = new TGIS_Extent{$ENDIF} ;
    maxv              : Array [0..2] of Word ;
    listStripInfo32   : Array of TGIS_FileTIFF_StripInfo32 ;
    {$IFDEF OXYGENE}
      to_conv : Array [0..3] of Byte ;
    {$ENDIF}
    ival : Cardinal ;
  const
    SOFTWARE_LEN =  50 ;
  begin
    try
      if assigned(fileStreamOvr) then begin
        if _page < ovrFirstPage then begin
          if fileStream <> fileStreamMain then
            fileStream := fileStreamMain ;
          isBigTIFF := isBigTIFFMain ;
        end
        else begin
          if fileStream <> fileStreamOvr then
            fileStream := fileStreamOvr ;
          isBigTIFF := isBigTIFFOvr ;
        end;
      end;

      isBitsString := False ;
      floatDataType := False ;
      wordShift := -1 ;
      pixelBytesNo := 3 ;
      bandDataType :=  TGIS_BandDataType.Unknown ;
      FCurrentPage := _page ;

      // Header is a valid Tiff Header
      loadIFD( associatesIFDOffset64[_page -1] ) ;
      if not isValid then
        exit ;

      swapBW := False ;
      callJpegProc := False ;
      bitscountinfo := 0 ;

      interpretTiffTags ;

      if _page > 1 then begin
        if phti = 4  then
          validPagesTable[_page -1] := False
        else
          validPagesTable[_page -1] := True ;
      end;

      if isValid then begin
           FMakerModel := '' ;
           // Maker
           ptag := getTagTiff( TIFF_TAG_MAKE ) ;
           if ptag <> -1 then
             FMakerModel := getTagAscii( ptag ) ;

           ptag := getTagTiff( TIFF_TAG_MODEL ) ;
           if ptag <> -1 then
             FMakerModel := FMakerModel + ' ' + getTagAscii( ptag ) ;

           if isCompressed then begin
             if compressionType = TGIS_CompressionType.JPEG then begin
              if FBandsCount2 = 4 then begin
                componentsJpeg := JPEG_ARGB ;
              end
              else begin

                if phti = 2 then begin
                  if componentsJpeg = JPEG_YBR then
                    componentsJpeg := JPEG_BGR  ;
                end
                else begin
                  componentsJpeg := JPEG_YBR ;
                end;
              end;


             end ;
           end ;

           if phti = 5 then begin
             isCMYK := True ;
           end
           else
           if (phti = 0) and (bitsPerPixel = 1) then
             swapBW := True
           else
           if phti < 0 then begin
              isValid := False ;
              Abort ;
           end ;

           if bitsPerPixel = 0 then begin
             if phti = 1 then
              bitsPerPixel := 1
             else
             if phti = 0 then begin
               bitsPerPixel := 1 ;
               swapBW := True
             end;
           end;
      end ;

      FBandsCount := FBandsCount2 ;

      if alphaAssociated then begin
        if  bitsPerPixel = 8  then begin
          defaultPartialTransparent := True ;
          isPartialTransparent := True ;
          forceCachedMode := True ;
          extraSamples := 1 ;
        end
        else
        if FBandsCount = 3 then
          inc(FBandsCount) ;
      end;

      case bitsPerPixel of
         1, 2, 4, 8 : colorsNo := 1 shl bitsPerPixel
         else      colorsNo := 0
      end ;

      realLineWidth := ( actualBitWidth*bitsPerPixel +7 ) div 8 ;

      ptag := getTagTiff( TIFF_TAG_PLANARCONFIGURATION ) ;
      if ptag <> -1 then begin
        if (listTag64[ptag].Value = 2) and (FBandsCount > 1)then
          isPlanar1 := False
        else
          isPlanar1 := True ;
      end
      else
        isPlanar1 := True ;

      if isTiffTiled then begin
      // Tiled image
        ptag := getTagTiff( TIFF_TAG_TILEWIDTH ) ;
        if ptag <> -1 then
          tileWidth := listTag64[ptag].Value
        else begin
          tileWidth := actualBitWidth ;
          isTiffTiled := False ;
        end;


        ptag := getTagTiff( TIFF_TAG_TILELENGTH ) ;
        if ptag <> -1 then begin
          tileLength := listTag64[ptag].Value ;
          rowsPerStrip := tileLength ;
        end
        else begin
          ptag := getTagTiff( TIFF_TAG_ROWSPERSTRIP ) ;
          if ptag <> -1 then begin
            rowsPerStrip := Integer(listTag64[ptag].Value) ;
            if (rowsPerStrip <= 0) or (rowsPerStrip > actualBitHeight) then
              rowsPerStrip := actualBitHeight ;
          end
          else
            rowsPerStrip := actualBitHeight ;
          tileLength := rowsPerStrip ;
        end;

        rowsPerStrip := tileLength ;
        ptag := getTagTiff( TIFF_TAG_TILEOFFSETS ) ;
        if ptag = -1 then
          ptag := getTagTiff( TIFF_TAG_STRIPOFFSETS    ) ;

        tilesColumns := (actualBitWidth + tileWidth -1) div tileWidth ;
        tilesRows := (actualBitHeight +tileLength -1) div tileLength ;
        tilesPerImage := tilesColumns * tilesRows ;

        if not isPlanar1 then
          tilesPerImage := FBandsCount*tilesPerImage ;

        SetLength( listStripInfo64, tilesPerImage ) ;
        SetLength( listStripInfo32, tilesPerImage ) ;
        {$IFDEF GIS_NORECORDS}
          for i := 0 to tilesPerImage - 1 do begin
            listStripInfo32[i] := new TGIS_FileTIFF_StripInfo32 ;
            listStripInfo64[i] := new TGIS_FileTIFF_StripInfo64 ;
          end ;
        {$ENDIF}
        countStripInfo := tilesPerImage ;

        fileStream.Position := listTag64[ptag].Value ;

        if isBigTIFF then begin

          // Strip Offsets
          if countStripInfo > 1 then begin
            for i := 0 to countStripInfo -1 do begin
              fileStream.ReadInt64( listStripInfo64[i].StripOffsets, 8 ) ;
            end ;
          end
          else begin
            listStripInfo64[0].StripOffsets := listTag64[ptag].Value ;
          end ;
        end
        else begin
          // Strip Offsets
          if countStripInfo > 1 then begin
            for i := 0 to countStripInfo -1 do begin
              fileStream.ReadCardinal( listStripInfo32[i].StripOffsets, 4 ) ;
            end ;
            if bigEndian then begin
              for i := 0 to countStripInfo -1 do
                  listStripInfo32[i].StripOffsets :=
                      SwapLongInt(listStripInfo32[i].StripOffsets);
            end;
          end
          else begin
            listStripInfo32[0].StripOffsets := listTag64[ptag].Value ;
          end ;
        end ;

        ptag := getTagTiff( TIFF_TAG_TILEBYTECOUNT ) ;

        if isBigTIFF then begin

          if ptag <> -1  then begin
            fileStream.Position := listTag64[ptag].Value ;

            // Strip (Tile) Byte Counts
            if countStripInfo > 1 then begin
              fileStream.ReadInt64( listStripInfo64[0].StripByteCounts, 8 ) ;
              if listStripInfo64[0].StripByteCounts > (tileWidth*tileLength*4) then
              begin
                fileStream.Position := listTag64[ptag].Value ;

                for i := 0 to countStripInfo -1 do begin
                  listStripInfo64[i].StripByteCounts := 0 ;
                  ival := 0 ;
                  fileStream.ReadCardinal( ival, 4 ) ;
                  listStripInfo64[i].StripByteCounts := ival ;
                end ;
              end
              else begin
                for i := 1 to countStripInfo -1 do begin
                  fileStream.ReadInt64( listStripInfo64[i].StripByteCounts, 8 ) ;
                end ;
              end;
            end
            else begin
              listStripInfo64[0].StripByteCounts := listTag64[ptag].Value ;
            end ;
          end
          else begin
          // Strip (Tile) Byte Counts
            for i := 0 to countStripInfo - 1 do begin
             listStripInfo64[i].StripByteCounts :=  3*tileLength * tileWidth ;
            end ;
          end;
        end
        else begin
          if ptag <> -1  then begin
            fileStream.Position := listTag64[ptag].Value ;

            // Strip (Tile) Byte Counts
            if countStripInfo > 1 then begin
              for i := 0 to countStripInfo -1 do begin
                fileStream.ReadCardinal( listStripInfo32[i].StripByteCounts, 4 ) ;
              end ;

              if bigEndian then begin
                for i := 0 to countStripInfo -1 do begin
                    listStripInfo32[i].StripByteCounts :=
                      SwapLongInt(listStripInfo32[i].StripByteCounts);
                end;
              end;
            end
            else begin
              listStripInfo32[0].StripByteCounts := listTag64[ptag].Value ;
            end ;
          end
          else begin
          // Strip (Tile) Byte Counts
            for i := 0 to countStripInfo - 1 do begin
             listStripInfo32[i].StripByteCounts :=  3*tileLength * tileWidth ;
            end ;
          end;
          for i := 0 to countStripInfo - 1 do begin
            listStripInfo64[i].StripOffsets := listStripInfo32[i].StripOffsets ;
            listStripInfo64[i].StripByteCounts := listStripInfo32[i].StripByteCounts ;
          end;
        end;
        SetLength( listStripInfo32, 0 ) ;
      end
      else begin
        tilesColumns := 1 ;
        tilesRows := 1 ;
        ptag := getTagTiff( TIFF_TAG_ROWSPERSTRIP ) ;
        if ptag <> -1 then begin
          rowsPerStrip := Integer(listTag64[ptag].Value) ;
          if (rowsPerStrip <= 0) or (rowsPerStrip > actualBitHeight) then
            rowsPerStrip := actualBitHeight ;
        end
        else
          rowsPerStrip := actualBitHeight ;
      end ;

      // fetch the palette
      ptag := getTagTiff( TIFF_TAG_COLORMAPOFFSET ) ;
      if ptag <> -1 then begin
        // read Color Table Offset
        if not FIsNativeGridImage then
          readColTabFromFile( listTag64[ptag].Count, listTag64[ptag].Value ) ;
      end
      else begin
        if FBandsCount2 < 3 then begin
          // grayscale image
          isGrayScaleImage := True ;
          if colorsNo = 0 then
            colorsNo := 256 ;
          setBitmapPalette ;
          if (bitsPerPixel = 1) AND swapBW then
            inversePalette ;
        end;
      end ;

      if (bitsPerPixel = 1) OR (bitsPerPixel = 4)
          OR (bitsPerPixel = 8) then begin
        intLineWidth := actualBitWidth * 3 ;
        colorsNo := 0 ;
      end
      else begin
        intLineWidth := realLineWidth ;
      end ;

      if isCompressed then
        initDecoder ;

      //  Image orientation
      ptag := getTagTiff( TIFF_TAG_ORIENTATION ) ;
      if ptag <> -1 then begin
        imageOrientation := SmallInt(listTag64[ptag].Value) ;
      end
      else
        imageOrientation := 1 ;

      // read a GeoTiff projection
      readGeoTiffCS ;

      // read a word file
      if ( scaleX = 0 ) and ( scaleY = 0 ) then begin
        setWorldFile( WORLD_FILE_EXT_TIF ) ;

        // read a word file (alternative)
        if GisIsSameExtent( Extent, GisExtent( 0, 0, 0, 0 ) ) then
           setWorldFile( WORLD_FILE_EXT_TIF2 ) ;

        // read a word file (alternative)
        if GisIsSameExtent( Extent, GisExtent( 0, 0, 0, 0 ) ) then
           setWorldFile( WORLD_FILE_EXT_TIF3 ) ;
      end ;

      if is48BitsPerPixel then begin
        if isPlanar1 then begin

          ptag := getTagTiff( TIFF_TAG_MAXSAMPLEVAL ) ;
          if ptag <> -1  then begin
            wordDivider := 1 ;
            fileStream.Position := listTag64[ptag].Value ;
            {$IFDEF OXYGENE}
              fileStream.ReadWord( maxv[0], 2 );
              fileStream.ReadWord( maxv[1], 2 );
              fileStream.ReadWord( maxv[2], 2 );
            {$ELSE}
              fileStream.Read(maxv[0], sizeOf(maxv));
            {$ENDIF}
            if bigEndian then begin
                maxv[0] := Swap( maxv[0] ) ;
                maxv[1] := Swap( maxv[1] ) ;
                maxv[2] := Swap( maxv[2] ) ;
            end;
            maxv[0] := Max(maxv[0], maxv[1]) ;
            maxv[0] := Max(maxv[0], maxv[2]) ;
            wordDivider := (maxv[0] + 254) div 255 ;

            FMaxZ := maxv[0] ;
            FMinZ := 0 ;
          end ;
        end
        else begin
          ptag := getTagTiff( TIFF_TAG_MAXSAMPLEVAL ) ;
          if ptag <> -1  then begin
            fileStream.Position := listTag64[ptag].Value ;
            {$IFDEF OXYGENE}
              fileStream.ReadWord( maxv[0], 2 );
              fileStream.ReadWord( maxv[1], 2 );
              fileStream.ReadWord( maxv[2], 2 );
            {$ELSE}
              fileStream.Read(maxv[0], sizeOf(maxv));
            {$ENDIF}
            if bigEndian then begin
                maxv[0] := Swap( maxv[0] ) ;
                maxv[1] := Swap( maxv[1] ) ;
                maxv[2] := Swap( maxv[2] ) ;
            end;
            maxv[0] := Max(maxv[0], maxv[1]) ;
            maxv[0] := Max(maxv[0], maxv[2]) ;
            wordDivider := (maxv[0] + 254) div 255 ;

            FMaxZ := maxv[0] ;
            FMinZ := 0 ;
          end ;
        end;
      end;

      // setup GEOTIFF
      if ( scaleX = 0 ) and ( scaleY = 0 ) then begin
        ptag := getTagTiff( GEOTIFF_TAG_GEOKEYDIRECTORY ) ;
        if ptag <> -1  then begin // GEOTIFF
          SetLength( geokeys, listTag64[ptag].Count div 4, 4 ) ;
          fileStream.Position := listTag64[ptag].Value ;

          for i:=0 to listTag64[ptag].Count div 4 - 1 do begin
            for j:=0 to 4 - 1 do begin
              {$IFDEF OXYGENE}
                fileStream.ReadWord(geokeys[i][j], sizeOf(Word)) ;
              {$ELSE}
                fileStream.Read(geokeys[i, j], sizeOf(Word)) ;
              {$ENDIF}
            end;
          end ;

          if bigEndian then begin
            for i:=0 to listTag64[ptag].Count div 4 - 1 do begin
              for j:=0 to 4 - 1 do begin
                  {$IFDEF OXYGENE}
                    geokeys[i][j] := Swap(geokeys[i][j]) ;
                  {$ELSE}
                    geokeys[i, j] := Swap(geokeys[i, j]) ;
                  {$ENDIF}
              end;
            end ;
          end ;
        end ;

        scale_x := 0 ;
        scale_y := 0 ;
        ptag := getTagTiff( GEOTIFF_TAG_MODELTIEPOINT ) ;
        if ptag <> -1 then begin
          SetLength( tiepoint, listTag64[ptag].Count ) ;
          fileStream.Position := listTag64[ptag].Value ;

          for i:=0 to listTag64[ptag].Count - 1 do begin
            if bigEndian then
              tiepoint[ i ] := readDoubleBE
            else
              tiepoint[ i ] := readDoubleLE ;
          end ;
        end ;
        ptag := getTagTiff( GEOTIFF_TAG_MODELPIXELSCALE ) ;
        if ptag <> -1  then begin
          fileStream.Position := listTag64[ptag].Value ;
          if bigEndian then begin
            scale_x := readDoubleBE ;
            scale_y := - readDoubleBE ;
            if listTag64[ptag].Count = 3 then
              //scale_z := readDoubleBE ;
              readDoubleBE ;
          end
          else begin
            scale_x := readDoubleLE ;
            scale_y := - readDoubleLE ;
            if listTag64[ptag].Count = 3 then
              //scale_z := readDoubleLE ;
              readDoubleLE ;
          end ;
        end ;

        ptag := getTagTiff( GEOTIFF_TAG_GEODOUBLEPARAMS ) ;
        if ptag <> -1  then begin
          fileStream.Position := listTag64[ptag].Value ;
          SetLength( doubleparams, listTag64[ptag].Count ) ;

          for i:=0 to listTag64[ptag].Count - 1 do begin
            if bigEndian then
              doubleparams[ i ] := readDoubleBE
            else
              doubleparams[ i ] := readDoubleLE ;
          end ;
        end ;

        if ( scale_x <> 0 ) and ( scale_y <> 0 ) and ( high( tiepoint ) >= 5 )
        then  begin
          ext.XMin := tiepoint[3] - tiepoint[0] * scale_x ;
          ext.XMax := ext.XMin +scale_x*FBitWidth ;
          ext.YMax := tiepoint[4] - tiepoint[1] * scale_y ;
          ext.YMin := ext.YMax +scale_y*FBitHeight ;

          Extent := _TGIS_Extent(ext) ;
          if scaleX = 0 then
            scaleX := scale_x ;
          if scaleY = 0 then
            scaleY := scale_y ;
        end
        else begin
          ptag := getTagTiff( GEOTIFF_TAG_MODELTRANSFORMATION ) ;
          if ptag <> -1 then begin
            fileStream.Position := listTag64[ptag].Value ;
            SetLength( doubleparams, listTag64[ptag].Count ) ;

            for i:=0 to listTag64[ptag].Count - 1 do begin
              if bigEndian then
                doubleparams[ i ] := readDoubleBE
              else
                doubleparams[ i ] := readDoubleLE ;
            end ;

            scaleX := doubleparams[0] ;
            if scaleX < 0 then begin
              scaleXFactor := -1 ;
              scaleX := -scaleX ;
              baseRotation := True ;
            end ;
            scaleY := doubleparams[5] ;
            if scaleY > 0 then begin
              scaleYFactor := -1 ;
              scaleY := -scaleY ;
              baseRotation := True ;
            end ;

            if scaleX > 0 then begin
              FExtent.XMin := doubleparams[3] ;
              FExtent.XMax := FExtent.XMin +scaleX*FBitWidth ;
            end
            else begin
              FExtent.XMin := doubleparams[3] ;
              FExtent.XMax := FExtent.XMin -scaleX*FBitWidth ;
            end;

            if scaleY > 0 then begin
              FExtent.YMax := doubleparams[7] ;
              FExtent.YMin := FExtent.YMax -scaleY*FBitHeight ;
            end
            else begin
              FExtent.YMax := doubleparams[7] ;
              FExtent.YMin := FExtent.YMax +scaleY*FBitHeight ;
            end;

            if (doubleparams[1] <> 0) or (doubleparams[4] <> 0) then begin
              if scaleX = 0 then
                scaleX := 1 ;
              if scaleY = 0 then
                scaleY := -1 ;
              yRotDivSc := doubleparams[1]/scaleX ;
              xRotDivSc := doubleparams[4]/scaleY ;
              scxF_yRotDivScxRotDivSMcsyF := 1 - yRotDivSc * xRotDivSc ;
              scyF_yRotDivScxRotDivSMcsxF := scxF_yRotDivScxRotDivSMcsyF ;
              if (scyF_yRotDivScxRotDivSMcsxF <> 0) and (scyF_yRotDivScxRotDivSMcsxF <> 1) then
                baseRotation := True
              else
              if doubleparams[0] = 0 then begin
                if doubleparams[1] > 0 then begin
                  xRotDivSc := -1 ;
                  yRotDivSc := -1 ;
                end
                else begin
                  xRotDivSc := 1 ;
                  yRotDivSc := 1 ;
                end;

                baseRotation := True ;
              end;
            end else
            if baseRotation then begin
              scaleYFactor := -1 ;
              scxF_yRotDivScxRotDivSMcsyF := scaleXFactor - yRotDivSc*xRotDivSc*scaleYFactor ;
              scyF_yRotDivScxRotDivSMcsxF := scaleYFactor - yRotDivSc*xRotDivSc*scaleXFactor ;
            end;

          end;
        end ;
      end ;
      if callJpegProc then
       jpegProc ;

      if assigned( fileStream ) then begin
        SetLengthStr(FFileInfo, 0) ;

        if is48BitsPerPixel then
          bitscountinfo := 48
        else
        if bitscountinfo = 0 then
          bitscountinfo := bitsPerPixel ;

        if length(tiepoint) > 0 then
          FFileInfo := Format( 'GeoTIFF - Tag Image File Format (TIF)' +#13#10 +
                               '%d x %d; %d bit ',
                               [ FBitWidth, FBitHeight, bitscountinfo ]
                             )
        else
          FFileInfo := Format( 'TIFF - Tag Image File Format (TIF)' + #13#10 +
                               '%d x %d; %d bit',
                               [ FBitWidth, FBitHeight, bitscountinfo ] ) ;
        if isGrayScaleImage then
          FFileInfo := FFileInfo +'; Grayscale' ;

        if extraSamples > 0 then
          FFileInfo := FFileInfo + Format('; %d byte(s) Alpha Associated',
                                    [ extraSamples ] ) ;

        FFileInfo := FFileInfo +'; Compression: ' ;
        case compressionType of
          TGIS_CompressionType.None :
            FFileInfo := FFileInfo +'None' ;
          TGIS_CompressionType.CCITTHuffmanRLE :
            FFileInfo := FFileInfo +'CCITT Huffman RLE' ;
          TGIS_CompressionType.CCITTFaxGroup3 :
            FFileInfo := FFileInfo +'CCITT Fax Group 3' ;
          TGIS_CompressionType.CCITTFaxGroup4 :
            FFileInfo := FFileInfo +'CCITT Fax Group 4' ;
          TGIS_CompressionType.JPEG :
            FFileInfo := FFileInfo +'JPEG' ;
          TGIS_CompressionType.ZLIB :
            FFileInfo := FFileInfo +'ZLIB' ;
          {$IFNDEF GIS_NOLZWREAD}
            TGIS_CompressionType.LZW :
              FFileInfo := FFileInfo +'LZW' ;
          {$ENDIF}
            TGIS_CompressionType.PackBits :
              FFileInfo := FFileInfo +'Packbits' ;
          else
            FFileInfo := FFileInfo +'Unknown' ;
        end ;

        FFileInfo := FFileInfo + Format( '; %d/%d Page',
                                         [ _page, FPagesCount ]
                                       ) ;

        ptag := getTagTiff( TIFF_TAG_SOFTWARE ) ;
        if ptag <> -1  then begin
          FSoftware := getTagAscii( ptag ) ;

          if length(FSoftware) > 0 then
            FFileInfo := FFileInfo + '; Software: ' + FSoftware ;
        end ;

        if not IsStringEmpty( FMakerModel ) then
          FFileInfo := FFileInfo + '; Maker and Model: ' + FMakerModel ;

        FFileInfo := FFileInfo + FFileInfoGeoTIFF ;
      end ;
      if bandDataType = TGIS_BandDataType.Unknown then begin
        if bitsPerBand[0] > 8 then begin
          if Params.Pixel.GridBand > 0 then begin
            likeGrid := True ;
            FNoDataValue := Params.Pixel.GridNoValue ;
          end;
          if FNoDataValue < 0 then
            bandDataType := TGIS_BandDataType.dtS16
          else
            bandDataType := TGIS_BandDataType.dtU16 ;
        end
        else
        if bitsPerBand[0] = 8 then
          bandDataType := TGIS_BandDataType.dtU8 ;
      end;


      if likeGrid then begin
        if FNoDataValue <> Params.Pixel.GridNoValue then
          FNoDataValue := Params.Pixel.GridNoValue
        else begin
          if not isColorNoDataGDAL then begin
            if FNoDataValue = GIS_GRID_NOVALUE then begin
              Params.Pixel.GridNoValue := TIFF_GRID_NOVALUE ;
              FNoDataValue := TIFF_GRID_NOVALUE ;
            end;
          end;
        end ;
        if bitscountinfo = 32 then begin
          pixelBytesNo := 4 ;
          if signed32Bits then begin
            bandDataType :=  TGIS_BandDataType.dtS32 ;
            SetLength( lineIntBuffer, FBitWidth ) ;
          end
          else begin
            bandDataType :=  TGIS_BandDataType.dtF32 ;
          end;
        end
        else
        if bitscountinfo = 64 then begin
          SetLength( lineDBuffer, FBitWidth ) ;
          bandDataType :=  TGIS_BandDataType.dtF64 ;
          pixelBytesNo := 8 ;
        end
        else
        begin
          if signed16Bits then begin
            SetLength( lineShBuffer, FBitWidth ) ;
            bandDataType :=  TGIS_BandDataType.dtS16 ;
            pixelBytesNo := 2 ;
          end
          else
          if signed32Bits then begin
            SetLength( lineIntBuffer, FBitWidth ) ;
            bandDataType :=  TGIS_BandDataType.dtS32 ;
            pixelBytesNo := 4 ;
          end
          else begin
            if bandDataType = TGIS_BandDataType.Unknown then begin
              if bitsPerBand[0] <= 8 then begin
                bandDataType :=  TGIS_BandDataType.dtU8 ;
                pixelBytesNo := 1 ;
              end
              else begin
                SetLength( lineWBuffer, FBitWidth ) ;
                bandDataType :=  TGIS_BandDataType.dtU16 ;
                pixelBytesNo := 2 ;
              end;
            end ;
          end;
        end;
        lineInBuffer := -1 ;

        FIsNativeGridImage := True ;
        FIsGridImage := True ;

        if (FMinZ = 0) and (FMaxZ = 0) then begin
          ptag := getTagTiff( TIFF_TAG_MINSAMPLEVALUE ) ;
          if ptag <> -1  then begin
            if (bandDataType = TGIS_BandDataType.dtF32) or
               (bandDataType = TGIS_BandDataType.dtF64)
            then begin
            {$IFDEF OXYGENE}
              to_conv[0] :=  listTag64[ptag].Value and $FF ;
              to_conv[1] := (listTag64[ptag].Value and $FF00) shr 8 ;
              to_conv[2] := (listTag64[ptag].Value and $FF0000) shr 16 ;
              to_conv[3] := (listTag64[ptag].Value and $FF000000) shr 24 ;
              FMinZ := BitConverter.ToSingle( to_conv, 0) ;
            {$ELSE}
              FMinZ := PSingle(@listTag64[ptag].Value)^ ;
            {$ENDIF}
            end;
          end;

          ptag := getTagTiff( TIFF_TAG_MAXSAMPLEVALUE ) ;
          if ptag <> -1  then begin
            if bandDataType = TGIS_BandDataType.dtF32 then begin
            {$IFDEF OXYGENE}
              to_conv[0] :=  listTag64[ptag].Value and $FF ;
              to_conv[1] := (listTag64[ptag].Value and $FF00) shr 8 ;
              to_conv[2] := (listTag64[ptag].Value and $FF0000) shr 16 ;
              to_conv[3] := (listTag64[ptag].Value and $FF000000) shr 24 ;
              FMaxZ := BitConverter.ToSingle( to_conv, 0) ;
            {$ELSE}
              FMaxZ := PSingle(@listTag64[ptag].Value)^ ;
            {$ENDIF}
            end;
          end;

          if FMinZ >= FMaxZ then begin
            FMaxZ   := -GIS_MAX_SINGLE  ;
            FMinZ   :=  GIS_MAX_SINGLE  ;
            mustPrepareMinMaxZ := True ;
          end;
          Extent3D := GisExtent3D( Extent.XMin, Extent.YMin, FMinZ,
                                   Extent.XMax, Extent.YMax, FMaxZ
                                  ) ;
        end;

      // Transparency
        redTransp[0]    := BASE_TRANSPARENT_FLAG ;
        greenTransp[0]  := BASE_TRANSPARENT_FLAG ;
        blueTransp[0]   := BASE_TRANSPARENT_FLAG ;
      end
      else
      if bandDataType = TGIS_BandDataType.Unknown then begin
        case bitsPerBand[0] of
          8  :
            bandDataType :=  TGIS_BandDataType.dtU8 ;
          16 :
            bandDataType :=  TGIS_BandDataType.dtU16 ;
          32 :
            bandDataType :=  TGIS_BandDataType.dtU32 ;
        end;
      end;

      // force rereading layer new page
      inherited setUp ;
//        Alive ;
      if bandDataType =  TGIS_BandDataType.dtU16  then begin
        is48BitsPerPixel := True ;

        if (Params.Pixel.RedMapZones.Count   > 0) or
           (Params.Pixel.GreenMapZones.Count > 0) or
           (Params.Pixel.BlueMapZones.Count  > 0)
        then
          wordDivider := getWordDivider ;
      end;
      if realBitCount = 0 then begin
        realBitCount := bitsPerPixel ;
        if realBitCount > 24 then
          realBitCount := 24 ;
      end ;

      if bandsDefinition.Bands.Count = 0 then
        setTiffBandsDefinition ;
    except
        raise EGIS_Exception.Create( '0997 Unsupported TIFF format', Path, 0) ;
    end ;

  end;

  procedure TGIS_LayerTIFF.closeCurrentPage ;
  var
    i : Integer ;
    l : Integer ;
  begin
    l := length( fileDecoder ) ;
    if l > 0 then begin
      for i := high(fileDecoder) downto low(fileDecoder) do
        FreeObject( fileDecoder[i] ) ;
      fileDecoder := nil ;
    end ;

    listTag64       := nil ;
    listStripInfo64 := nil ;
    if subViewsCount = 1 then begin
      scaleX := 0 ;
      scaleY := 0 ;
    end ;
    FCellWidth  := 0 ;
    FCellHeight := 0 ;
  end ;

  procedure TGIS_LayerTIFF.convertLineToLE (
    const _buffer   : TBytes  ;
    const _varsize  : Word
  ) ;
  var
    i, k : Integer ;
    count : Integer ;
    wb : Byte ;
    idxl : Integer ;
    idxr : Integer ;
    sidx : Integer ;
  begin
    count := length(_buffer) div _varsize ;
    sidx := (_varsize div 2) -1 ;
    for i := 0 to count -1 do begin
      for k:= 0 to sidx do begin
        idxl := i*_varsize +k ;
        idxr := (i +1)*_varsize -k -1;
        wb := _buffer[idxl] ;
        _buffer[idxl] :=  _buffer[idxr] ;
        _buffer[idxr] := wb ;
      end;
    end ;
  end ;

  function TGIS_LayerTIFF.getLine(
    const _buffer : TBytes  ;
    const _offset : Integer ;
    const _linenr : Integer ;
    const _start  : Integer ;
    const _bytes  : Integer
  ) : Integer ;
  var
    i : Integer ;
    buf : TGIS_Pixels ;
    pix_start : Integer ;
    pix_count : Integer ;
    rgb_off, a_off : Integer ;
    ot : Boolean ;
  const
    A_MASK = $FF000000 ;
    R_MASK = $00FF0000 ;
    G_MASK = $0000FF00 ;
    B_MASK = $000000FF ;
  begin
    if not isPlanar1 then begin
      if (FBandsCount2 = 4) and (bitsPerBand[0] = 16) then
        is48BitsPerPixel := True ;
      Result := getLine2( _buffer, _offset, _linenr, _start, _bytes ) ;
      exit ;
    end;

    pix_count := _bytes div 3 ;
    pix_start := _start div 3 ;
    SetLength(buf, pix_count ) ;
    if not assigned(lineAlphaBuffer) then
      SetLength(lineAlphaBuffer, FBitWidth) ;
    ot := isPartialTransparent ;
    isPartialTransparent := true ;



    Result := getLinePixels( buf, 0, _linenr, pix_start, pix_count) * 3 ;
    rgb_off := _offset ;
    a_off := 0 ;
    for i := 0 to  pix_count -1 do begin
      lineAlphaBuffer[a_off] := Byte((buf[i] and A_MASK) shr 24) ;
      inc(a_off) ;
      _buffer[rgb_off] := Byte((buf[i] and R_MASK) shr 16) ;
      inc(rgb_off) ;
      _buffer[rgb_off] := Byte((buf[i] and G_MASK) shr  8) ;
      inc(rgb_off) ;
      _buffer[rgb_off] := Byte((buf[i] and B_MASK)       ) ;
      inc(rgb_off) ;
    end;
    isPartialTransparent := ot ;
  end;


  function TGIS_LayerTIFF.getLinePixels(
    const _buffer   : TGIS_Pixels  ;
    const _offset   : Integer ;
    const _linenr   : Integer ;
    const _pixStart : Integer ;
    const _pixCount : Integer
  ) : Integer ;
  var
    buf : TBytes ;
    i,k : Integer ;
    cc  : Integer ;
  begin
    if isBuilt then begin
      Result := inherited getLinePixels(_buffer, _offset, _linenr, _pixStart,
                                        _pixCount) ;
      exit ;
    end ;


    if floatDataType then begin
      Result := convertFloatToPixels(_buffer, _offset,
                                     _linenr, _pixStart, _pixCount) ;
      exit ;
    end
    else
    begin
      if bytesPerPixel = 0 then begin
        bytesPerPixel := 1 ;
        bitsPerBand[0] := bytesPerPixel ;
        FBandsCount2 := bytesPerPixel ;
        FBandsCount  := FBandsCount2 ;
        bytesPerBand[0] := bytesPerPixel ;
      end;
      SetLength(buf, _pixCount*bytesPerPixel) ;

      if isPlanar1 then begin
        if (bitsPerBand[0] = 12) and isCompressed then begin

          Result := (getLineBits(buf, 0, _linenr, _pixStart, _pixCount)) div 3 ;
          k := 0 ;
          for i := 0 to _pixCount -1 do begin
            if bandsMap[0] >= 0 then
              cc := (Integer(buf[k +bandsMap[0]]) shl 16)
            else
              cc := 0 ;
            if bandsMap[1] >= 0 then
              cc := cc or (Integer(buf[k +bandsMap[1]]) shl 8) ;

            if bandsMap[2] >= 0 then
              cc := cc or (Integer(buf[k +bandsMap[2]]) shl 0) ;

            _buffer[i] := cc or Integer($FF000000) ;
            inc(k, 3) ;
          end;
          exit ;
        end;
        getLineBits(buf, 0, _linenr, _pixStart, _pixCount) ;
      end
      else begin
        getLineBits2(buf, 0, _linenr, _pixStart, _pixCount) ;
      end;

      if (bitsPerBand[0] > 8) and (not FIsNativeGridImage) and (wordShift <= 0) then
        checkWordShift ;
      Result := convertBitsToPixels(buf, 0, _buffer, _offset,
                                    _pixStart, _pixCount) ;
      if isColorNoDataGDAL then
        applyGDALTransparency(_buffer, _offset, _pixCount) ;

    end;
  end;

  function  TGIS_LayerTIFF.convert1BtoPixels (
    const _buffSrc    : TBytes  ;
    const _srcOffset  : Integer ;
    const _buffDst    : TGIS_Pixels ;
    const _dstOffset  : Integer ;
    const _pixStart   : Integer ;
    const _pixCount   : Integer
  ) : Integer ;
  var
    i, k : Integer ;
    lmargin : Integer ;
    pal_idx  : Integer ;
    pixmask  : Byte ;
  const
    B_MASK = $80 ;
  begin
    k := _srcOffset ;
    lmargin := _pixStart mod 8 ;
    for i := _dstOffset to _pixCount +_dstOffset -1 do begin

      pixmask := Byte(Integer(B_MASK) shr lmargin) ;
      if _buffSrc[k] and pixmask > 0 then
        pal_idx := 1
      else
        pal_idx := 0 ;

      //Tiff has BGR
      _buffDst[i] := Integer(bitPalette[pal_idx].B)  or
                    (Integer(bitPalette[pal_idx].G ) shl 8 ) or
                    (Integer(bitPalette[pal_idx].R ) shl 16) or
                    Integer($FF000000) ;
      inc(lmargin) ;
      if lmargin >= 8 then begin
        inc(k) ;
        lmargin := 0 ;
      end;
    end;
    Result := _pixCount ;
  end;

  function  TGIS_LayerTIFF.convert4BtoPixels (
    const _buffSrc    : TBytes  ;
    const _srcOffset  : Integer ;
    const _buffDst    : TGIS_Pixels ;
    const _dstOffset  : Integer ;
    const _pixStart   : Integer ;
    const _pixCount   : Integer
  ) : Integer ;
  var
    i, k : Integer ;
    lmargin : Integer ;
    pal_idx  : Integer ;
  const
    B_MASK = $0F ;
  begin
    k := _srcOffset ;
    lmargin := _pixStart mod 2 ;
    for i := _dstOffset to _pixCount +_dstOffset -1 do begin

      if lmargin > 0 then
         pal_idx := Integer(_buffSrc[k] and B_MASK)
      else
         pal_idx := (Integer(_buffSrc[k]) shr 4) and Integer(B_MASK) ;

      _buffDst[i] := Integer(bitPalette[pal_idx].B)  or
                    (Integer(bitPalette[pal_idx].G ) shl 8 ) or
                    (Integer(bitPalette[pal_idx].R ) shl 16) or
                    Integer($FF000000) ;
      inc(lmargin) ;
      if lmargin >= 2 then begin
        inc(k) ;
        lmargin := 0 ;
      end;
    end;
    Result := _pixCount ;
  end;

  procedure TGIS_LayerTIFF.setUpInternal ;
  begin
    workSrcRect := Rect( 0, 0,
                         FCellWidth  - 1,
                         FCellHeight - 1
                       ) ;
  end ;

  function  TGIS_LayerTIFF.convert12BtoPixels (
    const _buffSrc    : TBytes  ;
    const _srcOffset  : Integer ;
    const _buffDst    : TGIS_Pixels ;
    const _dstOffset  : Integer ;
    const _pixStart   : Integer ;
    const _pixCount   : Integer
  ) : Integer ;
  var
    i, didx      : Integer ;
    lmargin      : Integer ;
    start_margin : Integer ;
    bits         : Integer ;
    c            : Integer ;
    val1         : Byte ;
    val2         : Byte ;
    val3         : Byte ;
  begin
    lmargin := _pixStart mod 2 ;
    start_margin := lmargin ;
    didx := _dstOffset ;
    bits := (_pixStart mod 2)*4 ;

    for i := start_margin to _pixCount +start_margin -1 do begin

      if lmargin = 0 then
        val1 := (_buffSrc[bits div 8] shr 4)
      else
        val1 := _buffSrc[bits div 8] and $0F ;


      inc(lmargin) ;
      lmargin := lmargin and 1 ;
      inc( bits, 4 ) ;

      if lmargin = 0 then
        val2 := (_buffSrc[bits div 8] shr 4)
      else
        val2 := _buffSrc[bits div 8] and $0F ;

      inc(lmargin) ;
      lmargin := lmargin and 1 ;
      inc( bits, 4 ) ;
      if lmargin = 0 then
        val3 := (_buffSrc[bits div 8] shr 4)
      else
        val3 := _buffSrc[bits div 8] and $0F ;

      inc(lmargin) ;
      lmargin := lmargin and 1 ;
      inc( bits, 4 ) ;

      c := (val1 shl 8) + (val2 shl 4) + val3 ;
      c := c * 256 div 4096 ;

      _buffDst[didx] := Integer(c and $FF)  or
                       (Integer(c and $FF) shl 8 ) or
                       (Integer(c and $FF ) shl 16) or
                       Integer($FF000000) ;
      inc(didx) ;
    end;
    Result := _pixCount ;
  end;

  function  TGIS_LayerTIFF.convert36BtoPixels (
    const _buffSrc    : TBytes  ;
    const _srcOffset  : Integer ;
    const _buffDst    : TGIS_Pixels ;
    const _dstOffset  : Integer ;
    const _pixStart   : Integer ;
    const _pixCount   : Integer
  ) : Integer ;
  var
    i, n, didx   : Integer ;
    lmargin      : Integer ;
    start_margin : Integer ;
    bits         : Integer ;
    ac           : Array [0..2] of  Integer ;
    val1         : Byte ;
    val2         : Byte ;
    val3         : Byte ;
  begin
    lmargin := _pixStart mod 2 ;
    start_margin := lmargin ;
    didx := _dstOffset ;
    bits := (_pixStart mod 2)*4 ;

    for i := start_margin to _pixCount +start_margin -1 do begin
      for n := 0 to 2 do begin

        if lmargin = 0 then
          val1 := (_buffSrc[bits div 8] shr 4)
        else
          val1 := _buffSrc[bits div 8] and $0F ;


        inc(lmargin) ;
        lmargin := lmargin and 1 ;
        inc( bits, 4 ) ;

        if lmargin = 0 then
          val2 := (_buffSrc[bits div 8] shr 4)
        else
          val2 := _buffSrc[bits div 8] and $0F ;

        inc(lmargin) ;
        lmargin := lmargin and 1 ;
        inc( bits, 4 ) ;
        if lmargin = 0 then
          val3 := (_buffSrc[bits div 8] shr 4)
        else
          val3 := _buffSrc[bits div 8] and $0F ;

        inc(lmargin) ;
        lmargin := lmargin and 1 ;
        inc( bits, 4 ) ;

        ac[n] := (val1 shl 8) + (val2 shl 4) + val3 ;
        ac[n] := ac[n] * 256 div 4096 ;
      end;

      _buffDst[didx] := ac[bandsMap[2]]  or
                       (ac[bandsMap[1]] shl 8 ) or
                       (ac[bandsMap[0]] shl 16) or
                       Integer($FF000000) ;
      inc(didx) ;
    end;
    Result := _pixCount ;
  end;

  function  TGIS_LayerTIFF.convert8BtoPixels (
    const _buffSrc   : TBytes  ;
    const _srcOffset : Integer ;
    const _buffDst   : TGIS_Pixels ;
    const _dstOffset : Integer ;
    const _pixStart  : Integer ;
    const _pixCount  : Integer
  ) : Integer ;
  var
    i, k : Integer ;
    idx : Integer ;
  begin
    k := _srcOffset ;
    for i := _dstOffset to _pixCount +_dstOffset -1 do begin
      //Tiff has BGR
      idx := _buffSrc[k] ;
      _buffDst[i] := Integer(bitPalette[idx].B)  or
                    (Integer(bitPalette[idx].G ) shl 8 ) or
                    (Integer(bitPalette[idx].R ) shl 16) or
                    Integer($FF000000) ;
      inc(k) ;
    end;
    Result := _pixCount ;
  end;

  function  TGIS_LayerTIFF.convert16BtoPixels (
    const _buffSrc   : TBytes  ;
    const _srcOffset : Integer ;
    const _buffDst   : TGIS_Pixels ;
    const _dstOffset : Integer ;
    const _pixStart  : Integer ;
    const _pixCount  : Integer
  ) : Integer ;
  var
    i, k : Integer ;
  begin
    k := _srcOffset ;
    for i := _dstOffset to _pixCount +_dstOffset -1 do begin
      //Tiff has BGR
      _buffDst[i] := (Integer(_buffSrc[k   ])       ) or
                     (Integer(_buffSrc[k +1]) shl  8) or
                     (Integer(_buffSrc[k +2]) shl 16) or
                     (Integer($FF000000)) ;
      inc(k, 3) ;
    end;
    Result := _pixCount ;
  end;

  function  TGIS_LayerTIFF.convertN16BtoPixels (
    const _buffSrc   : TBytes  ;
    const _srcOffset : Integer ;
    const _buffDst   : TGIS_Pixels ;
    const _dstOffset : Integer ;
    const _pixStart  : Integer ;
    const _pixCount  : Integer
  ) : Integer ;
  var
    i, k : Integer ;
    b1, b2, b3 : Integer ;
    wd : Integer ;
  begin
    k := _srcOffset ;
    wd := TruncS(wordDivider) ;
    if bigEndian then begin
      for i := _dstOffset to _pixCount +_dstOffset -1 do begin
      //Tiff has RGB
        b1 :=  TruncS((Integer(_buffSrc[k +bandsMap[0]*2 +1]) +
                      (Integer(_buffSrc[k +bandsMap[0]*2   ]) shl 8)) / wd);
        if b1 > 255 then
          b1 := 255 ;
        b2 :=  TruncS((Integer(_buffSrc[k +bandsMap[1]*2 +1]) +
                      (Integer(_buffSrc[k +bandsMap[1]*2   ]) shl 8)) / wd);
        if b2 > 255 then
          b2 := 255 ;
        b3 :=  TruncS((Integer(_buffSrc[k +bandsMap[2]*2 +1]) +
                      (Integer(_buffSrc[k +bandsMap[2]*2   ]) shl 8)) / wd);
        if b3 > 255 then
          b3 := 255 ;
        _buffDst[i] := (b3       ) or
                       (b2 shl  8) or
                       (b1 shl 16) ;
        if _buffDst[i] <> 0 then
           _buffDst[i] :=  _buffDst[i] or (Integer($FF000000)) ;
        inc(k, 2*FBandsCount) ;
      end;
    end
    else begin
      for i := _dstOffset to _pixCount +_dstOffset -1 do begin
      //Tiff has BGR
        b1 :=  TruncS((Integer(_buffSrc[k +bandsMap[0]*2   ]) +
                      (Integer(_buffSrc[k +bandsMap[0]*2 +1]) shl 8)) / wd);
        if b1 > 255 then
          b1 := 255 ;
        b2 :=  TruncS((Integer(_buffSrc[k +bandsMap[1]*2   ]) +
                      (Integer(_buffSrc[k +bandsMap[1]*2 +1]) shl 8)) / wd);
        if b2 > 255 then
          b2 := 255 ;
        b3 :=  TruncS((Integer(_buffSrc[k +bandsMap[2]*2   ]) +
                      (Integer(_buffSrc[k +bandsMap[2]*2 +1]) shl 8)) / wd);
        if b3 > 255 then
          b3 := 255 ;
        _buffDst[i] := (b1       ) or
                       (b2 shl  8) or
                       (b3 shl 16) ;
        if _buffDst[i] <> 0 then
           _buffDst[i] :=  _buffDst[i] or (Integer($FF000000)) ;
        inc(k, 2*FBandsCount) ;
      end;
    end;
    Result := _pixCount ;
  end;

  function  TGIS_LayerTIFF.convertComplextoPixels (
    const _buffSrc    : TBytes  ;
    const _srcOffset  : Integer ;
    const _buffDst    : TGIS_Pixels ;
    const _dstOffset  : Integer ;
    const _pixStart   : Integer ;
    const _pixCount   : Integer
  ) : Integer ;
  var
    i, k, n      : Integer ;
    sidx, didx   : Integer ;
    lmargin      : Integer ;
    flmargin     : Integer ;
    blstart      : Array [0..3] of Integer ;
    bitsno       : Integer ;
    abpp         : Array [0..3] of Integer ; //actual bits pier pixel in band
    rval         : Array [0..3] of Integer ;
  const
    B_MASK = $FF ;
  begin
    if FBandsCount < 3 then begin
      bandsMap[1] := bandsMap[0] ;
      bandsMap[2] := bandsMap[0] ;
    end;

    for i := 0 to 2 do begin
      abpp[i] := bitsPerBand[bandsMap[i]] ;
      blstart[i] := 0 ;
      for k := 0 to bandsMap[i] -1 do
        blstart[i] := blstart[i] +Integer(bitsPerBand[k]) ;
    end;
    flmargin := ( _pixStart  * bitsPerPixel ) mod 8 ;
    didx := 0 ;
    for i := 0 to _pixCount -1 do begin
      for n := 0 to 2 do begin
        bitsno := abpp[n] ;
        sidx := _srcOffset +( (i * bitsPerPixel +blstart[n] +flmargin) div 8) ;
        lmargin := ( (_pixStart +i) * bitsPerPixel +blstart[n]) mod 8 ;
        bitsno := bitsno -8 +lmargin ;
        if lmargin > 0 then begin
          rval[n] := (Integer(_buffSrc[sidx]) and (Integer(B_MASK) shr lmargin))
        end
        else begin
          rval[n] := Integer(_buffSrc[sidx]) ;
        end;

        if bitsno < 0 then begin
          rval[n] := rval[n] shr -bitsno ;
        end else
        if bitsno > 0 then begin
          inc(sidx) ;
          rval[n] := rval[n] shl bitsno ;
          bitsno := bitsno -8 ;
          if bitsno < 0 then begin
            rval[n] := rval[n] or ( Integer(_buffSrc[sidx]) shr -bitsno) ;
          end
          else
            rval[n] := rval[n] or  Integer(_buffSrc[sidx]) ;
        end;

        if  bitsPerBand[bandsMap[n]] > 8 then
          if wordDivider > 0 then begin
            rval[n] := TruncS(rval[n]/wordDivider) ;
            if rval[n] > 255 then
              rval[n] := 255 ;
          end
          else
            rval[n] := rval[n] shr (bitsPerBand[bandsMap[n]] -8) ;
      end;

      _buffDst[didx] :=  rval[2] or
                       ( rval[1] shl 8 ) or
                       ( rval[0] shl 16) or
                       Integer($FF000000) ;
      inc(didx) ;
    end;
    Result := _pixCount ;
  end;

  procedure TGIS_LayerTIFF.applyGDALTransparency(
    const _buffer    : TGIS_Pixels ;
    const _offset    : Integer ;
    const _pixCount  : Integer
  ) ;
  var
    i       : Integer ;
    wcmpR   : Word ;
    wcmpG   : Word ;
    wcmpB   : Word ;
    bcheck  : Boolean ;
  begin
    bcheck := False ;
    wcmpR := 0 ;
    wcmpG := 0 ;
    wcmpB := 0 ;

    if is48BitsPerPixel then begin
      wcmpR := Word((valueNoDataGDAL shr wordShift) and $FF) ;
      wcmpG := wcmpR ;
      wcmpB := wcmpR ;
      bcheck := True ;
    end
    else begin
      if ( colorsNo > 0 ) and ( colorsNo < 256 ) and
         ( valueNoDataGDAL >=0 ) and ( valueNoDataGDAL < 256 ) then
      begin
        wcmpR := Word(bitPalette[valueNoDataGDAL].R ) and $FF ;
        wcmpG := Word(bitPalette[valueNoDataGDAL].G ) and $FF ;
        wcmpB := Word(bitPalette[valueNoDataGDAL].B ) and $FF ;
        bcheck := True ;
      end
      else begin
        if ( valueNoDataGDAL >=0 ) and ( valueNoDataGDAL < 256 ) then begin
          wcmpR := Word(valueNoDataGDAL and $FF) ;
          wcmpG := wcmpR ;
          wcmpB := wcmpR ;
          bcheck := True ;
        end ;
      end ;
    end ;

    if bcheck then begin
      for i := _offset to _offset + _pixCount -1 do
        if (( _buffer[i] and $FF)         = wcmpB) and
           (((_buffer[i] shr 08) and $FF) = wcmpG) and
           (((_buffer[i] shr 16) and $FF) = wcmpR)
        then
          _buffer[i] := _buffer[i] and $00FFFFFF ;
    end ;
  end;

  procedure TGIS_LayerTIFF.checkWordShift ;
  var
    valbits   : Cardinal ;
    mpn    : Cardinal ;
  begin
    if wordShift = -1 then begin
      valbits := bitsPerBand[0] -bitsSkipLeft[0] ;
      mpn := valbits mod 8 ;
      if mpn = 0 then begin
        if (bitsSkipLeft[0] = 0) and (FBandsCount2 = 1) then
          wordShift := getWordShift
        else begin
          if bitsSkipLeft[0] <> 0 then
            wordShift := mpn +bitsSkipLeft[0]
          else
            wordShift := getWordShift ;
        end;
      end
      else begin
        mpn := 8 -mpn ;
        wordShift := mpn +bitsSkipLeft[0] ;
      end;
    end
    else begin
      if (bitsSkipLeft[0] <> 0) or (bitsSkipRight[0] <> 0) then
        if wordShift <> (8 - Integer(bitsSkipLeft[0])) then
           wordShift := (8 - bitsSkipLeft[0]) ;
    end;
  end;

  function  TGIS_LayerTIFF.convertBytesToPixels (
    const _buffSrc    : TBytes  ;
    const _srcOffset  : Integer ;
    const _buffDst    : TGIS_Pixels ;
    const _dstOffset  : Integer ;
    const _pixStart   : Integer ;
    const _pixCount   : Integer
  ) : Integer ;
  var
    i : Integer ;
    sidx, didx : Integer ;
    val1         : Integer ;
    val2         : Integer ;
    val3         : Integer ;
    band_bytes_start       : Array [0..19] of Integer ; //actual bits pier pixel in band
    pal_idx      : Integer ;
    lidx         : Integer ;
    bidx         : Integer ;
    cval         : Cardinal ;
    cbits        : Integer ;
    nshl         : Integer ;

  begin
    if FBandsCount2 < 3 then begin
      bandsMap[1] := bandsMap[0] ;
      bandsMap[2] := bandsMap[0] ;
    end;

    bidx := 0 ;
    for i := 0 to FBandsCount2 -1 do begin
      band_bytes_start[i] := bidx ;
      bidx := bidx +Integer(bytesPerBand[i]) ;
    end;

   if wordDivider = 1 then begin
     if bitsPerBand[0] = 12 then
       wordDivider := 16.0
      else
     if bitsPerBand[0] >= 16 then
       wordDivider := getWordDivider ;
   end;

    sidx := _srcOffset ;
    didx := _dstOffset ;
//?    maxsidx := length(_buffSrc) -1 ;
    for i := 0 to _pixCount -1 do begin

      lidx := sidx +band_bytes_start[bandsMap[0]] ;

      if bitsPerPixel >  8 then begin

        cbits := bitsPerBand[bandsMap[0]] ;
        cval := Cardinal(_buffSrc[lidx]) ;
        if cbits > 8 then begin
          nshl := 8 ;
          dec(cbits, 8) ;
          repeat
            inc(lidx) ;
            cval := cval or (Cardinal(_buffSrc[lidx]) shl nshl)  ;
            dec(cbits, 8) ;
            inc(nshl, 8) ;
          until cbits <= 0 ;
          val1 := TruncS(cval / wordDivider) ;
          if val1 > 255 then
            val1 := 255 ;
        end
        else
          val1 := cval ;

        lidx := sidx +band_bytes_start[bandsMap[1]] ;
        cbits := bitsPerBand[bandsMap[1]] ;
        cval := Cardinal(_buffSrc[lidx]) ;
        if cbits > 8 then begin
          nshl := 8 ;
          dec(cbits, 8) ;
          repeat
            inc(lidx) ;
            cval := cval or (Cardinal(_buffSrc[lidx]) shl nshl)  ;
            dec(cbits, 8) ;
            inc(nshl, 8) ;
          until cbits <= 0 ;
          val2 := TruncS(cval / wordDivider) ;
          if val2 > 255 then
            val2 := 255 ;
        end
        else
          val2 := cval ;

        lidx := sidx +band_bytes_start[bandsMap[2]] ;
        cbits := bitsPerBand[bandsMap[2]] ;
        cval := Cardinal(_buffSrc[lidx]) ;
        if cbits > 8 then begin
          nshl := 8 ;
            dec(cbits, 8) ;
          repeat
            inc(lidx) ;
            cval := cval or (Cardinal(_buffSrc[lidx]) shl nshl)  ;
            dec(cbits, 8) ;
            inc(nshl, 8) ;
          until cbits <= 0 ;
          val3 := TruncS(cval / wordDivider) ;
          if val3 > 255 then
            val3 := 255 ;
        end
        else
          val3 := cval ;
        _buffDst[didx] :=  val3 or
                         ( val2 shl 8 ) or
                         ( val1 shl 16) or
                         Integer($FF000000) ;
      end
      else begin
        pal_idx := _buffSrc[lidx] ;
        _buffDst[didx] := Integer(bitPalette[pal_idx].B)  or
                          (Integer(bitPalette[pal_idx].G ) shl 8 ) or
                          (Integer(bitPalette[pal_idx].R ) shl 16) or
                          Integer($FF000000) ;
      end;

      inc(didx) ;
      inc(sidx, bytesPerPixel) ;
    end;
    Result := _pixCount ;
  end;

  function  TGIS_LayerTIFF.convertBitsStringToBytes (
    const _buffSrc    : TBytes  ;
    const _srcOffset  : Integer ;
    const _buffDst    : TBytes ;
    const _dstOffset  : Integer ;
    const _pixStart   : Integer ;
    const _pixCount   : Integer
  ) : Integer ;
  var
    i, n : Integer ;
    sidx, didx  : Integer ;
    flmargin : Integer ;
    wbits  : Integer ;
    lmask   : Integer ;
    testbyte     : Byte  ;
    band_bytes_start       : Array [0..19] of Integer ; //actual bytes pier pixel per band
    bval         : Array [0..19] of Byte ;
    abitsno      : Integer ;
    workbyte     : Integer ;
    bidx         : Integer ;
    hibits       : Integer ;
    bpos         : Integer ;
    workbyte2    : Byte ;
    lidx         : Integer ;
  const
    L_MASK = $FF ;
  begin

    if FBandsCount2 < 3 then begin
      bandsMap[1] := bandsMap[0] ;
      bandsMap[2] := bandsMap[0] ;
    end;
    bidx := 0 ;
    for i := 0 to FBandsCount2 -1 do begin
      band_bytes_start[i] := bidx ;
      bidx := bidx +Integer(bytesPerBand[i]) ;
    end;


    sidx := _srcOffset ;
    didx := _dstOffset ;
    flmargin := ( _pixStart  * bitsPerPixel ) mod 8 ;
    bpos := flmargin ;
    wbits := 0 ;
    testbyte := 0 ;
    for i := 0 to _pixCount -1 do begin
      bidx := 0 ;


      for n := 0 to FBandsCount2 -1 do begin
        abitsno := bitsPerBand[n] ;
        hibits := abitsno mod 8 ;
        if hibits = 0 then begin
          hibits := 8 ;
        end ;
        if bpos = 0 then
          lmask := Byte(L_MASK)
        else
          lmask := Byte(L_MASK) shr bpos ;

        workbyte2 := 0 ;
        repeat
          if wbits = 0 then begin
            wbits := 8 -flmargin ;
            testbyte :=  _buffSrc[sidx] ;
            inc(sidx) ;
            flmargin := 0 ;
            if hibits = 8 then
              lmask := Byte(L_MASK) ;
          end;
          workbyte := testbyte and lmask ;
          if hibits <= wbits then begin
            if hibits <  wbits then begin
              dec(wbits, hibits) ;
              workbyte := workbyte shr wbits ;
              lmask := lmask shr hibits ;
              inc(bpos, hibits) ;
              dec(abitsno, hibits) ;
            end
            else begin
              dec(abitsno, wbits) ;
              wbits := 0 ;
              bpos  := 0 ;
            end;
            bval[bidx] := workbyte or workbyte2 ;
            inc(bidx) ;
            hibits := 8 ;
          end
          else begin
            workbyte2 := workbyte shl (8 -wbits) ;
            abitsno := abitsno -wbits ;
            hibits := abitsno mod 8 ;
            lmask := Byte(L_MASK) ;
            wbits := 0 ;
            bpos  := 0 ;
          end;
        until abitsno <= 0 ;
      end ;

      for n := 0 to FBandsCount2 -1 do begin
        lidx := band_bytes_start[bandsMap[n]] ;
        if bitsPerPixel >  8 then begin
          if isBitsString then begin
            _buffDst[didx] := bval[lidx +1] ;
            inc(didx) ;
            _buffDst[didx] := bval[lidx] ;
            inc(didx) ;
          end
          else begin
            _buffDst[didx] := bval[lidx] ;
            inc(didx) ;
            _buffDst[didx] := bval[lidx +1] ;
            inc(didx) ;
          end;
        end
        else begin
          _buffDst[didx] := bval[lidx] ;
          inc(didx) ;
        end;

      end;
    end;
    Result := _pixCount ;
  end;

  function  TGIS_LayerTIFF.convertFloatToPixels (
    const _buffDst    : TGIS_Pixels ;
    const _dstOffset  : Integer ;
    const _pixLine    : Integer ;
    const _pixStart   : Integer ;
    const _pixCount   : Integer
  ) : Integer ;
  var
    i : Integer ;
    didx : Integer ;
    val1         : Integer ;
    val2         : Integer ;
    val3         : Integer ;
    bidx         : Integer ;
    sa          : TGIS_SingleArray ;
  begin

    if FBandsCount < 3 then begin
      bandsMap[1] := bandsMap[0] ;
      bandsMap[2] := bandsMap[0] ;
    end;

   if wordDivider = 1 then
     wordDivider := getWordDivider ;

    didx := _dstOffset ;

    SetLength(sa, _pixCount*FBandsCount2) ;

    getNativeLine(sa, _pixLine, _pixStart, _pixCount) ;

    for i := 0 to _pixCount -1 do begin
      if isPlanar1 then
        bidx := i * FBandsCount2
      else
        bidx := i * 3 ;
      if (sa[bidx +bandsMap[0]] <> FNoDataValue) or
         (sa[bidx +bandsMap[1]] <> FNoDataValue) or
         (sa[bidx +bandsMap[2]] <> FNoDataValue)
      then begin
        if unusedRed then
          val1 := 0
        else begin
          val1 := Integer(RoundS(sa[bidx +bandsMap[0]] / wordDivider)) ;
          if val1 > 255 then
            val1 := 255 ;
        end;
        if unusedGreen then
          val2 := 0
        else begin
          val2 := Integer(RoundS(sa[bidx +bandsMap[1]] / wordDivider)) ;
          if val2 > 255 then
            val2 := 255 ;
        end;
        if unusedBlue then
          val3 := 0
        else begin
          val3 := Integer(RoundS(sa[bidx +bandsMap[2]] / wordDivider)) ;
          if val3 > 255 then
            val3 := 255 ;
        end;

        _buffDst[didx] :=  val3 or
                         ( val2 shl 8 ) or
                         ( val1 shl 16) or
                         Integer($FF000000) ;
      end
      else
        _buffDst[didx] := 0 ;

     inc(didx) ;
    end;
    Result := _pixCount ;
  end;

  function  TGIS_LayerTIFF.convert24BtoPixels (
    const _buffSrc   : TBytes  ;
    const _srcOffset : Integer ;
    const _buffDst   : TGIS_Pixels ;
    const _dstOffset : Integer ;
    const _pixStart  : Integer ;
    const _pixCount  : Integer
  ) : Integer ;
  var
    i, k : Integer ;
  begin
    k := _srcOffset ;
    for i := _dstOffset to _pixCount +_dstOffset -1 do begin
      //Tiff has BGR
      _buffDst[i] := (Integer(_buffSrc[k +2])       ) or
                     (Integer(_buffSrc[k +1]) shl  8) or
                     (Integer(_buffSrc[k   ]) shl 16) or
                     (Integer($FF000000)) ;
      inc(k, 3) ;
    end;
    Result := _pixCount ;
  end;

  function  TGIS_LayerTIFF.convert32BtoPixels (
    const _buffSrc   : TBytes  ;
    const _srcOffset : Integer ;
    const _buffDst   : TGIS_Pixels ;
    const _dstOffset : Integer ;
    const _pixStart  : Integer ;
    const _pixCount  : Integer
  ) : Integer ;
  var
    i, k : Integer ;
  begin
    k := _srcOffset ;
    for i := _dstOffset to _pixCount +_dstOffset -1 do begin
      //Tiff has BGR
      _buffDst[i] := (Integer(_buffSrc[k +2])       ) or
                     (Integer(_buffSrc[k +1]) shl  8) or
                     (Integer(_buffSrc[k   ]) shl 16) or
                     (Integer(_buffSrc[k +3]) shl 24) ;
      inc(k, 4) ;
    end;
    Result := _pixCount ;
  end;

  function TGIS_LayerTIFF.getLineBits(
    const _buffer   : TBytes  ;
    const _offset   : Integer ;
    const _linenr   : Integer ;
    const _pixStart : Integer ;
    const _pixCount : Integer
  ) : Integer ;
  var
    offset     : Int64 ;
    stripno,
    stripline  : Integer ;
    psi      : TGIS_FileTIFF_StripInfo64 ;
    cols   : Integer ;
    rcol, lcol : Integer ;
    buffer   : TBytes  ;
    buffoff  : Integer ;
    colidx     : Integer ;
    tileWidthZoom : Integer ;
    line : Integer ;
    firsttile  : Integer ;
    wbuffer : TBytes  ;
    woffset   : Integer ;
    tileWidthBytes : Integer ;
    tilebytes      : Integer ;
    tileStart      : Integer ;
    bytestart      : Integer ;
    bytestop       : Integer ;
    bytes          : Integer ;
    readbytes      : Integer ;
  begin

    line   := _linenr * jpegZoom ;
    if line >= baseCellHeight then
      line := baseCellHeight -1 ;

    bytestart  := ( _pixStart * bitsPerPixel) div 8 ;
    bytestop   := ((_pixStart + _pixCount -1) * bitsPerPixel) div 8 ;
    bytes      := bytestop -bytestart +((bitsPerPixel +7) div 8);


    wbuffer := _buffer ;
    woffset := _offset ;

    if isCompressed AND (not isTiffTiled) then begin
      if bitsPerBand[0] = 12 then begin
        if _pixCount <= FBitWidth then begin
          if decodeState[0].NativeRequired then begin
            bytestart := _pixStart*6 ;
            bytes     := _pixCount*6 ;
          end
          else begin
            bytestart :=  _pixStart * 3 ;
            bytes     := _pixCount * 3 ;
          end;
        end
        else begin
          bytestart := _pixStart ;
          bytes     := _pixCount ;
        end;
      end;
      bytes := readCompressedLineOnFly(wbuffer, woffset, line, bytestart, bytes);
      Result := bytes ;
      exit ;
    end ;

    try

      if line >= actualBitHeight then
        line := actualBitHeight -1 ;

      lcol := 0 ;
      stripline := line ;
      firsttile := line ;

      if isTiffTiled then begin

        tileWidthZoom := tileWidth div jpegZoom ;
        if bitsPerBand[0] = 12 then begin
          if _pixCount <= FBitWidth then begin
            if decodeState[0].NativeRequired then begin
              bytestart := _pixStart*6 ;
              bytes     := _pixCount*6 ;
              tileWidthBytes := tileWidthZoom*6 ;
            end
            else begin
              bytestart :=  _pixStart * 3 ;
              bytes     := _pixCount * 3 ;
              tileWidthBytes := tileWidthZoom*3 ;
          end;
          end
          else begin
            bytestart := _pixStart ;
            bytes     := _pixCount ;
            tileWidthBytes := tileWidthZoom ;
          end;
        end else begin
          tileWidthBytes := ((tileWidthZoom*bitsPerPixel) +7) div 8 ;
        end;

        rcol := (bytestart +bytes -1)  div tileWidthBytes ;

        lcol := bytestart  div tileWidthBytes ;

        cols := rcol -lcol +1;

        stripline := line mod tileLength ;
        tileStart := bytestart mod tileWidthBytes ;

        if imageOrientation = 4 then
          firsttile := ((FBitHeight +tileLength -1) div tileLength) -
                        (line div tileLength) -1
        else
          firsttile := line div tileLength ;

        firsttile := firsttile*tilesColumns ;
      end
      else begin
        cols := 1 ;
        tileWidthZoom := actualBitWidth div jpegZoom ;
        tileWidthBytes := (tileWidthZoom *bitsPerPixel +7) div 8 ;
        tileStart := bytestart mod tileWidthBytes ;
      end ;

      Result := 0 ;

      for colidx := 0 to cols -1 do
      begin

        if isTiffTiled then begin
          stripno   := firsttile + lcol +colidx ;
          if stripno >= countStripInfo then
            stripno := countStripInfo - 1 ;
          psi := listStripInfo64[stripno] ;

          offset := Int64(psi.StripOffsets)
                    + Int64(tileWidthBytes * stripline)
                    + Int64(tileStart) ;
          buffer := wbuffer ;
          buffoff := woffset + Result ;
          if colidx = cols -1 then begin
            tilebytes := bytes -Result
          end
          else
          if colidx = 0 then
            tilebytes := tileWidthBytes -tileStart
          else
            tilebytes := tileWidthBytes ;
        end
        else begin
          stripno   := line div rowsPerStrip ;
          stripline := (line mod rowsPerStrip) div jpegZoom ;
          buffoff := woffset ;

          psi := listStripInfo64[stripno] ;
          offset := Int64(psi.StripOffsets)
                    + (Int64(tileWidthBytes) * Int64(stripline))
                    + Int64(bytestart) ;
          tilebytes := bytes ;
        end ;
        if isCompressed then begin
          readbytes := readCompressedLineOnFly(buffer, buffoff, stripline, tileStart, tilebytes,
                                            stripno);
        end
        else begin
          fileStream.Position := offset ;
          {$IFDEF OXYGENE}
            readbytes := fileStream.Read( wbuffer, buffoff, tilebytes ) ;
          {$ELSE}
            readbytes := fileStream.Read( wbuffer[buffoff], tilebytes ) ;
          {$ENDIF}
        end ;
        tileStart := 0 ;
        Result := Result + readbytes ;
      end ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end;

  function TGIS_LayerTIFF.getAlphaLine(
    const _buffer : TBytes  ;
    const _offset : Integer ;
    const _linenr : Integer ;
    const _start  : Integer ;
    const _bytes  : Integer
  ) : Integer;
  var
    i : Integer ;
    offset : Integer ;
  begin
    if (FBandsCount2 <= 3) and (not isColorNoDataGDAL) and
       (not isPartialTransparent) then
    begin
      Result := inherited getAlphaLine(_buffer, _offset, _linenr, _start, _bytes) ;
      exit ;
    end;
    if assigned(lineAlphaBuffer) then
      for i := 0 to _bytes - 1 do begin
        _buffer[i +_offset] := lineAlphaBuffer[i] ;
      end
    else begin
      if not assigned(oBitmap) then begin
        Result := inherited getAlphaLine(_buffer, _offset, _linenr,
                                         _start, _bytes) ;
        exit ;
      end
      else begin
        offset := _linenr*actualBitWidth +_start;
        for i := 0 to _bytes - 1 do begin
          _buffer[i +_offset] := Byte((oBitmap[offset] shr 24)) ;
          inc(offset)
        end;
      end;
    end;

    Result := _bytes ;
  end;

  function TGIS_LayerTIFF.getLine2(
    const _buffer : TBytes  ;
    const _offset : Integer ;
    const _linenr : Integer ;
    const _start  : Integer ;
    const _bytes  : Integer
  ) : Integer ;
  var
    rgbBuffer : Array of Array of Byte ;
    pixels : Integer ;
    lpixel : Integer ;
    lcol, rcol : Integer ;
    cols : Integer ;
    stripline : Integer ;
    start, lstart : Integer ;
    firsttile : Integer ;
    rgb_idx : Integer ;
    line : Integer ;
    colidx : Integer ;
    stripno : Integer ;
    psi : TGIS_FileTIFF_StripInfo64 ;
    offset : Int64 ;
    soffset : Integer ;
    bytes : Integer ;
    readin : Integer ;
    idx : Integer ;
    stripwidth : Integer ;
    pixsize : Integer ;
    bandtiles : Integer ;
    hws : Integer ;
    pixelsex : Integer ;
    lpixelex : Integer ;
    stripwidthex : Integer ;
    idxex : Integer ;
    wval : Integer ;
  begin

    pixels := _bytes div 3 ;
    lpixel := _start div 3 ;
    if is48BitsPerPixel then begin
      pixsize := 2 ;
      pixelsex := 2*pixels ;
      lpixelex := 2*lpixel ;
    end
    else begin
      pixsize := 1 ;
      pixelsex := pixels ;
      lpixelex := lpixel ;
    end;
    SetLength(rgbBuffer, 3, pixelsex) ;

    if isCompressed AND (not isTiffTiled) then begin
      line := _linenr * jpegZoom ;
      hws := ((actualBitHeight +rowsPerStrip -1) div rowsPerStrip)*rowsPerStrip ;
      readCompressedLineOnFly(TBytes(rgbBuffer[0]), 0, line, lpixel, pixels);

      inc(line, hws) ;
      readCompressedLineOnFly(TBytes(rgbBuffer[1]), 0, line, lpixel, pixels);

      inc(line, hws) ;
      readCompressedLineOnFly(TBytes(rgbBuffer[2]), 0, line, lpixel, pixels);
    end
    else begin
      line := _linenr * jpegZoom ;

      stripwidth := actualBitWidth ;
      if is48BitsPerPixel then
        stripwidthex := 2*stripwidth
      else
        stripwidthex := stripwidth ;

      bandtiles := tilesColumns*tilesRows ;

      if isTiffTiled then begin
        lcol := lpixel div tileWidth ;
        rcol := (lpixel + pixels -1) div tileWidth ;
        cols := rcol -lcol +1 ;

        stripline := line mod tileLength ;
        start :=  lpixel mod tileWidth ;
        firsttile := line div tileLength ;

        firsttile := firsttile*tilesColumns ;

      end
      else begin
        firsttile := 0 ;
        stripline := 0 ;
        start := 0 ;
        lcol := 0 ;
        cols := 1 ;
      end ;

      for rgb_idx := 0 to 2 do begin
        lstart := start ;
        readin := 0 ;
        for colidx := 0 to cols -1 do begin

          if isTiffTiled then begin
            stripno   := firsttile + lcol +colidx +bandsMap[rgb_idx]*bandtiles ;

            psi := listStripInfo64[stripno] ;

            offset := Int64(psi.StripOffsets) +
                      Int64(tileWidth * stripline) + Int64(lstart) ;

            if colidx = cols -1 then begin
              bytes := pixels -readin
            end
            else
            if colidx = 0 then
              bytes := tileWidth -lstart
            else
              bytes := tileWidth ;

          end
          else begin

            stripno   := bandsMap[rgb_idx]*((actualBitHeight +rowsPerStrip -1 )div rowsPerStrip)
                         +(line div rowsPerStrip);
            stripline := line mod rowsPerStrip ;

            psi := listStripInfo64[stripno];

            offset := Int64(psi.StripOffsets)
                    + Int64(stripwidthex*stripline) + Int64(lpixelex) ;
            bytes  := pixelsex ;
          end ;

          fileStream.Position := offset  ;
          {$IFDEF OXYGENE}
            readin := readin +fileStream.Read( rgbBuffer[rgb_idx], readin, bytes ) ;
          {$ELSE}
            readin := readin +fileStream.Read( rgbBuffer[rgb_idx, readin], bytes ) ;
          {$ENDIF}

          lstart := 0 ;
        end ;
      end ;
    end ;

    soffset := 0 ;
    if is48BitsPerPixel then begin
      if wordDivider = 1 then
        wordDivider := 4 ;

      idxex := 0 ;
      for idx := 0 to pixels -1 do begin

        if bigEndian then
          {$IFDEF OXYGENE}
            wval := (Word(rgbBuffer[2][idxex]) shl 8) or rgbBuffer[2][idxex +1]
          {$ELSE}
            wval := (Word(rgbBuffer[2, idxex]) shl 8) or rgbBuffer[2, idxex +1]
          {$ENDIF}
        else
          {$IFDEF OXYGENE}
            wval := (Word(rgbBuffer[2][idxex +1]) shl 8) or rgbBuffer[2][idxex] ;
          {$ELSE}
            wval := (Word(rgbBuffer[2, idxex +1]) shl 8) or rgbBuffer[2, idxex] ;
          {$ENDIF}

        if wval < 0 then
          wval := 0
        else
          wval := TruncS(wval / wordDivider) ;
        if wval >= 255 then
        {$IFDEF OXYGENE}
          _buffer[_offset+soffset] := 255
        else
          _buffer[_offset+soffset] := Byte(wval) ;
        {$ELSE}
          PByte(NativeInt(_buffer) + soffset)^ := 255
        else
          PByte(NativeInt(_buffer) + soffset)^ := Byte(wval) ;
        {$ENDIF}
        inc(soffset) ;
        if bigEndian then
          {$IFDEF OXYGENE}
            wval := (Word(rgbBuffer[1][idxex]) shl 8) or rgbBuffer[1][idxex +1]
          {$ELSE}
            wval := (Word(rgbBuffer[1, idxex]) shl 8) or rgbBuffer[1, idxex +1]
          {$ENDIF}
        else
          {$IFDEF OXYGENE}
            wval := (Word(rgbBuffer[1][idxex +1]) shl 8) or rgbBuffer[1][idxex] ;
          {$ELSE}
            wval := (Word(rgbBuffer[1, idxex +1]) shl 8) or rgbBuffer[1, idxex] ;
          {$ENDIF}


        if wval < 0 then
          wval := 0
        else
          wval := TruncS(wval/ wordDivider) ;

        if wval >= 255 then
        {$IFDEF OXYGENE}
          _buffer[_offset+soffset] := 255
        else
          _buffer[_offset+soffset] := Byte(wval) ;
        {$ELSE}
          PByte(NativeInt(_buffer) + soffset)^ := 255
        else
          PByte(NativeInt(_buffer) + soffset)^ := Byte(wval) ;
        {$ENDIF}
        inc(soffset) ;
        if bigEndian then
          {$IFDEF OXYGENE}
            wval := (Word(rgbBuffer[0][idxex]) shl 8) or rgbBuffer[0][idxex +1]
          {$ELSE}
            wval := (Word(rgbBuffer[0, idxex]) shl 8) or rgbBuffer[0, idxex +1]
          {$ENDIF}
        else
          {$IFDEF OXYGENE}
            wval := (Word(rgbBuffer[0][idxex +1]) shl 8) or rgbBuffer[0][idxex] ;
          {$ELSE}
            wval := (Word(rgbBuffer[0, idxex +1]) shl 8) or rgbBuffer[0, idxex] ;
          {$ENDIF}
        if wval < 0 then
          wval := 0
        else
          wval := TruncS(wval/ wordDivider) ;

        if wval >= 255 then
        {$IFDEF OXYGENE}
          _buffer[_offset+soffset] := 255
        else
          _buffer[_offset+soffset] := Byte(wval) ;
        {$ELSE}
          PByte(NativeInt(_buffer) + soffset)^ := 255
        else
          PByte(NativeInt(_buffer) + soffset)^ := Byte(wval) ;
        {$ENDIF}
        inc(soffset) ;
        inc(idxex, pixsize) ;
      end ;
    end
    else begin
      for idx := 0 to pixels -1 do begin
        {$IFDEF OXYGENE}
          _buffer[_offset+soffset] := rgbBuffer[2][idx] ;
        {$ELSE}
          PByte(NativeInt(_buffer) + soffset)^ := rgbBuffer[2, idx] ;
        {$ENDIF}
        inc(soffset) ;
        {$IFDEF OXYGENE}
          _buffer[_offset+soffset] := rgbBuffer[1][idx] ;
        {$ELSE}
          PByte(NativeInt(_buffer) + soffset)^ := rgbBuffer[1, idx] ;
        {$ENDIF}
        inc(soffset) ;
        {$IFDEF OXYGENE}
          _buffer[_offset+soffset] := rgbBuffer[0][idx] ;
        {$ELSE}
          PByte(NativeInt(_buffer) + soffset)^ := rgbBuffer[0, idx] ;
        {$ENDIF}
        inc(soffset) ;
      end ;
    end;
    Result := _bytes ;
  end ;

  function TGIS_LayerTIFF.getLineBits2(
    const _buffer   : TBytes  ;
    const _offset   : Integer ;
    const _linenr   : Integer ;
    const _pixStart : Integer ;
    const _pixCount : Integer
  ) : Integer ;
  var
    rgbBuffer : Array of TBytes ;
    pixels, i : Integer ;
    lpixel : Integer ;
    lcol, rcol : Integer ;
    cols : Integer ;
    stripline : Integer ;
    start, lstart : Integer ;
    firsttile : Integer ;
    rgb_idx : Integer ;
    line : Integer ;
    colidx : Integer ;
    stripno : Integer ;
    psi : TGIS_FileTIFF_StripInfo64 ;
    offset : Int64 ;
    outidx : Integer ;
    rgbidx : Integer ;
    bytes  : Integer ;
    readin : Integer ;
    idx : Integer ;
    stripwidth : Integer ;
    pixsize : Integer ;
    bandtiles : Integer ;
    hws : Integer ;
    pixelsex : Integer ;
    lpixelex : Integer ;
    stripwidthex : Integer ;
  begin

    pixels := _pixCount ;
    lpixel := _pixStart ;
    if is48BitsPerPixel then begin
      pixsize := 2 ;
      pixelsex := 2*pixels ;
      lpixelex := 2*lpixel ;
    end
    else begin
      pixsize := 1 ;
      pixelsex := pixels ;
      lpixelex := lpixel ;
    end;
    SetLength(rgbBuffer, FBandsCount2, pixelsex) ;

    if isCompressed AND (not isTiffTiled) then begin
      line := _linenr * jpegZoom ;
      hws := ((actualBitHeight +rowsPerStrip -1) div rowsPerStrip)*rowsPerStrip ;
      for i := 0 to FBandsCount2 -1 do begin
        readCompressedLineOnFly(TBytes(rgbBuffer[i]), 0, line, lpixel, pixels);
        inc(line, hws) ;
      end;
    end
    else begin
      line := _linenr * jpegZoom ;

      stripwidth := actualBitWidth ;
      if is48BitsPerPixel then
        stripwidthex := 2*stripwidth
      else
        stripwidthex := stripwidth ;

      bandtiles := tilesColumns*tilesRows ;

      if isTiffTiled then begin
        lcol := lpixel div tileWidth ;
        rcol := (lpixel + pixels -1) div tileWidth ;
        cols := rcol -lcol +1 ;

        stripline := line mod tileLength ;
        start :=  lpixel mod tileWidth ;
        firsttile := line div tileLength ;
        firsttile := firsttile*tilesColumns ;
      end
      else begin
        firsttile := 0 ;
        stripline := 0 ;
        start := 0 ;
        lcol := 0 ;
        cols := 1 ;
      end ;

      for rgb_idx := 0 to FBandsCount2 -1 do begin
        lstart := start*pixsize ;
        readin := 0 ;
        for colidx := 0 to cols -1 do begin

          if isTiffTiled then begin
            stripno   := firsttile + lcol +colidx +rgb_idx*bandtiles ;

            psi := listStripInfo64[stripno] ;

            offset := Int64(psi.StripOffsets) +
                      Int64(tileWidth * stripline)*pixsize + Int64(lstart) ;

            if colidx = cols -1 then begin
              bytes := (pixels*pixsize) -readin
            end
            else
            if colidx = 0 then
              bytes := tileWidth* pixsize -lstart
            else
              bytes := tileWidth * pixsize ;

          end
          else begin

            stripno   := rgb_idx*((actualBitHeight +rowsPerStrip -1 )div rowsPerStrip)
                         +(line div rowsPerStrip);
            stripline := line mod rowsPerStrip ;

            psi := listStripInfo64[stripno];

            offset := Int64(psi.StripOffsets)
                    + Int64(stripwidthex*stripline) + Int64(lpixelex) ;
            bytes  := pixelsex ;
          end ;

          if isCompressed then begin
            readin :=  readin +readCompressedLineOnFly( rgbBuffer[rgb_idx], readin, stripline, lstart, bytes,
                                            stripno) ;
          end
          else begin
            fileStream.Position := offset  ;
            {$IFDEF OXYGENE}
              readin := readin +fileStream.Read( rgbBuffer[rgb_idx], readin, bytes ) ;
            {$ELSE}
              readin := readin +fileStream.Read( rgbBuffer[rgb_idx, readin], bytes ) ;
            {$ENDIF}

          end;
          lstart := 0 ;
        end ;
      end ;
    end ;

    outidx := 0 ;
    rgbidx := 0 ;

    for idx := 0 to pixels -1 do begin
      for i := 0 to FBandsCount2 -1 do begin
        _buffer[outidx] := rgbBuffer[i][rgbidx] ;
        inc(outidx) ;
        if is48BitsPerPixel then begin
          _buffer[outidx] := rgbBuffer[i][rgbidx +1] ;
          inc(outidx) ;
        end;
      end;
      inc(rgbidx, pixsize) ;
    end;
    Result := FBandsCount2*pixelsex ;
  end ;

  function TGIS_LayerTIFF.getNativeLine2Bytes(
    const _buffer   : TBytes  ;
    const _linenr   : Integer ;
    const _startIdx : Integer ;
    const _count    : Integer
  ) : Integer;
  var
    rgbBuffer : Array of Array of Byte ;
    pixels : Integer ;
    lpixel : Integer ;
    lcol, rcol : Integer ;
    cols : Integer ;
    stripline : Integer ;
    start, lstart : Integer ;
    firsttile : Integer ;
    rgb_idx : Integer ;
    line : Integer ;
    colidx : Integer ;
    stripno : Integer ;
    psi : TGIS_FileTIFF_StripInfo64 ;
    offset : Int64 ;
    soffset : Integer ;
    bytes : Integer ;
    readin : Integer ;
    idx : Integer ;
    stripwidth : Integer ;
    pixsize : Integer ;
    bandtiles : Integer ;
    hws : Integer ;
    pixelsex : Integer ;
    lpixelex : Integer ;
    stripwidthex : Integer ;
    idxex : Integer ;
  begin

    pixels :=_count ;
    lpixel := _startIdx ;
    if is48BitsPerPixel then begin
      pixsize := 2 ;
      pixelsex := 2*pixels ;
      lpixelex := 2*lpixel ;
    end
    else begin
      pixsize := 1 ;
      pixelsex := pixels ;
      lpixelex := lpixel ;
    end;
    SetLength(rgbBuffer, 3, pixelsex) ;

    if isCompressed AND (not isTiffTiled) then begin
      line := _linenr * jpegZoom ;
      hws := ((actualBitHeight +rowsPerStrip -1) div rowsPerStrip)*rowsPerStrip ;
      readCompressedLineOnFly(TBytes(rgbBuffer[0]), 0, line, lpixel, pixels);

      inc(line, hws) ;
      readCompressedLineOnFly(TBytes(rgbBuffer[1]), 0, line, lpixel, pixels);

      inc(line, hws) ;
      readCompressedLineOnFly(TBytes(rgbBuffer[2]), 0, line, lpixel, pixels);
    end
    else begin
      line := _linenr * jpegZoom ;

      stripwidth := actualBitWidth ;
      if is48BitsPerPixel then
        stripwidthex := 2*stripwidth
      else
        stripwidthex := stripwidth ;

      bandtiles := tilesColumns*tilesRows ;

      if isTiffTiled then begin
        lcol := lpixel div tileWidth ;
        rcol := (lpixel + pixels -1) div tileWidth ;
        cols := rcol -lcol +1 ;

        stripline := line mod tileLength ;
        start :=  lpixel mod tileWidth ;
        firsttile := line div tileLength ;

        firsttile := firsttile*tilesColumns ;

      end
      else begin
        firsttile := 0 ;
        stripline := 0 ;
        start := 0 ;
        lcol := 0 ;
        cols := 1 ;
      end ;

      for rgb_idx := 0 to 2 do begin
        lstart := start ;
        readin := 0 ;
        for colidx := 0 to cols -1 do begin

          if isTiffTiled then begin
            stripno   := firsttile + lcol +colidx +bandsMap[rgb_idx]*bandtiles ;

            psi := listStripInfo64[stripno] ;

            offset := Int64(psi.StripOffsets) +
                      Int64(tileWidth * stripline) + Int64(lstart) ;

            if colidx = cols -1 then begin
              bytes := pixels -readin
            end
            else
            if colidx = 0 then
              bytes := tileWidth -lstart
            else
              bytes := tileWidth ;

          end
          else begin

            stripno   := bandsMap[rgb_idx]*((actualBitHeight +rowsPerStrip -1 )div rowsPerStrip)
                         +(line div rowsPerStrip);
            stripline := line mod rowsPerStrip ;

            psi := listStripInfo64[stripno];

            offset := Int64(psi.StripOffsets)
                    + Int64(stripwidthex*stripline) + Int64(lpixelex) ;
            bytes  := pixelsex ;
          end ;

          fileStream.Position := offset  ;
          {$IFDEF OXYGENE}
            readin := readin +fileStream.Read( rgbBuffer[rgb_idx], readin, bytes ) ;
          {$ELSE}
            readin := readin +fileStream.Read( rgbBuffer[rgb_idx, readin], bytes ) ;
          {$ENDIF}

          lstart := 0 ;
        end ;
      end ;
    end ;

    soffset := 0 ;
    if is48BitsPerPixel then begin

      idxex := 0 ;
      for idx := 0 to pixels -1 do begin

        {$IFDEF OXYGENE}
          _buffer[soffset] := rgbBuffer[0][idxex] ;
          inc(soffset) ;
          _buffer[soffset] := rgbBuffer[0][idxex +1] ;
          inc(soffset) ;
       {$ELSE}
          _buffer[soffset] := rgbBuffer[0, idxex] ;
          inc(soffset) ;
          _buffer[soffset] := rgbBuffer[0, idxex +1] ;
          inc(soffset) ;
       {$ENDIF}

        {$IFDEF OXYGENE}
          _buffer[soffset] := rgbBuffer[1][idxex] ;
          inc(soffset) ;
          _buffer[soffset] := rgbBuffer[1][idxex +1] ;
          inc(soffset) ;
       {$ELSE}
          _buffer[soffset] := rgbBuffer[1, idxex] ;
          inc(soffset) ;
          _buffer[soffset] := rgbBuffer[1, idxex +1] ;
          inc(soffset) ;
       {$ENDIF}

        {$IFDEF OXYGENE}
          _buffer[soffset] := rgbBuffer[2][idxex] ;
          inc(soffset) ;
          _buffer[soffset] := rgbBuffer[2][idxex +1] ;
          inc(soffset) ;
       {$ELSE}
          _buffer[soffset] := rgbBuffer[2, idxex] ;
          inc(soffset) ;
          _buffer[soffset] := rgbBuffer[0, idxex +1] ;
          inc(soffset) ;
       {$ENDIF}
        inc(idxex, pixsize) ;
      end ;
    end
    else begin
      for idx := 0 to pixels -1 do begin
        {$IFDEF OXYGENE}
          _buffer[soffset] := rgbBuffer[0][idx] ;
        {$ELSE}
          _buffer[soffset]  := rgbBuffer[0, idx] ;
        {$ENDIF}
        inc(soffset) ;
        {$IFDEF OXYGENE}
          _buffer[soffset] := rgbBuffer[1][idx] ;
        {$ELSE}
          _buffer[soffset]  := rgbBuffer[1, idx] ;
        {$ENDIF}
        inc(soffset) ;
        {$IFDEF OXYGENE}
          _buffer[soffset] := rgbBuffer[2][idx] ;
        {$ELSE}
          _buffer[soffset]  := rgbBuffer[2, idx] ;
        {$ENDIF}
        inc(soffset) ;
      end ;
    end;
    Result := _count ;
  end ;

  function TGIS_LayerTIFF.getGridAsARGBLine(
    const _buffer : TBytes  ;
    const _offset : Integer ;
    const _linenr : Integer ;
    const _start  : Integer ;
    const _bytes  : Integer
  ) : Integer ;
  var
    i       : Integer ;
    start   : Integer ;
    line    : Integer ;
    pixels  : Integer ;
    soffset : Integer ;
    ccolor  : TGIS_Color ;
    {$IFDEF OXYGENE}
      c     : TGIS_Color ;
    {$ENDIF}
    lpixel  : Integer ;
    arr     : TGIS_SingleArray ;
  begin
    try

      pixels := _bytes div 3 ;
      line   := _linenr ;
      Result := _bytes ;

      lpixel := _start div 3 ;

      start := 0 ;
      if lineInBuffer <> line then begin
        if mustPrepareMinMaxZ then begin
          prepareMinMaxZ ;
        end;
        SetLength(arr, pixels) ;
        getNativeLine(arr, line, lpixel, pixels) ;
      end;

      soffset := 0 ;
      pixels := pixels +start ;

      if assigned(arr) then begin // Single values
        for i := start to pixels -2 do begin
          if arr[i] = FNoDataValue then begin
            {$IFDEF OXYGENE}
              c := colorNoData ;
              _buffer[_offset+soffset  ] := c.R ;
              _buffer[_offset+soffset+1] := c.G ;
              _buffer[_offset+soffset+2] := c.B ;
            {$ELSE}
              PInteger( NativeInt(_buffer) +soffset )^ := colorNoData.ARGB ;
            {$ENDIF}
            makeTransparent := True ;
          end
          else begin
            {$IFDEF OXYGENE}
              c := GetColorRamp(arr[i]) ;
              _buffer[_offset+soffset  ] := c.R ;
              _buffer[_offset+soffset+1] := c.G ;
              _buffer[_offset+soffset+2] := c.B ;
            {$ELSE}
              PInteger( NativeInt(_buffer) +soffset )^ := GetColorRamp(arr[i]).ARGB ;
            {$ENDIF}
          end ;
          soffset := soffset +3 ;
        end ;

        // last triple
        if arr[pixels -1] = FNoDataValue then begin
          ccolor := colorNoData ;
          makeTransparent := True ;
        end
        else
          ccolor := GetColorRamp( arr[pixels -1] ) ;

        {$IFDEF OXYGENE}
          _buffer[_offset+soffset]   := Byte(ccolor.ARGB and $FF) ;
        {$ELSE}
          PByte( NativeInt(_buffer) +soffset )^   := Byte(ccolor.ARGB and $FF) ;
        {$ENDIF}
        ccolor.ARGB := ccolor.ARGB shr 8 ;
        {$IFDEF OXYGENE}
          _buffer[_offset+soffset+1] := Byte(ccolor.ARGB and $FF) ;
        {$ELSE}
          PByte( NativeInt(_buffer) +soffset +1)^ := Byte(ccolor.ARGB and $FF) ;
        {$ENDIF}
        ccolor.ARGB := ccolor.ARGB shr 8 ;
        {$IFDEF OXYGENE}
          _buffer[_offset+soffset +2] := Byte(ccolor.ARGB and $FF) ;
        {$ELSE}
          PByte( NativeInt(_buffer) +soffset +2)^ := Byte(ccolor.ARGB and $FF) ;
        {$ENDIF}
      end
      else begin//Short
        if signed16Bits then begin

          for i := start to pixels -2 do begin
            if lineShBuffer[i] <= FNoDataValue then begin
              ccolor := colorNoData ;
              makeTransparent := True ;
            end
            else
              ccolor:= GetColorRamp(lineShBuffer[i]) ;

            {$IFDEF OXYGENE}
              _buffer[_offset+soffset  ] := ccolor.R ;
              _buffer[_offset+soffset+1] := ccolor.G ;
              _buffer[_offset+soffset+2] := ccolor.B ;
            {$ELSE}
              PInteger( NativeInt(_buffer) +soffset )^ := Integer(ccolor) ;
            {$ENDIF}
            soffset := soffset +3 ;
          end ;

          // last triple
          if lineShBuffer[pixels -1] = FNoDataValue then begin
            ccolor := colorNoData ;
            makeTransparent := True ;
          end
          else
            ccolor := GetColorRamp( lineShBuffer[pixels -1] ) ;

          {$IFDEF OXYGENE}
            _buffer[_offset+soffset  ]   := Byte(ccolor.ARGB and $FF) ;
          {$ELSE}
            PByte( NativeInt(_buffer) +soffset )^   := Byte(ccolor.ARGB and $FF) ;
          {$ENDIF}
          ccolor.ARGB := ccolor.ARGB shr 8 ;
          {$IFDEF OXYGENE}
            _buffer[_offset+soffset+1] := Byte(ccolor.ARGB and $FF) ;
          {$ELSE}
            PByte( NativeInt(_buffer) +soffset +1)^ := Byte(ccolor.ARGB and $FF) ;
          {$ENDIF}
          ccolor.ARGB := ccolor.ARGB shr 8 ;
          {$IFDEF OXYGENE}
            _buffer[_offset+soffset+2] := Byte(ccolor.ARGB and $FF) ;
          {$ELSE}
            PByte( NativeInt(_buffer) +soffset +2)^ := Byte(ccolor.ARGB and $FF) ;
          {$ENDIF}
        end
        else
        if signed32Bits then begin

          for i := start to pixels -2 do begin
            if lineIntBuffer[i] <= FNoDataValue then begin
              ccolor := colorNoData ;
              makeTransparent := True ;
            end
            else
              ccolor:= GetColorRamp(lineIntBuffer[i]) ;

            {$IFDEF OXYGENE}
              _buffer[_offset+soffset  ] := ccolor.R ;
              _buffer[_offset+soffset+1] := ccolor.G ;
              _buffer[_offset+soffset+2] := ccolor.B ;
            {$ELSE}
              PInteger( NativeInt(_buffer) +soffset )^ := Integer(ccolor) ;
            {$ENDIF}
            soffset := soffset +3 ;
          end ;

          // last triple
          if lineIntBuffer[pixels -1] <= FNoDataValue then begin
            ccolor := colorNoData ;
            makeTransparent := True ;
          end
          else
            ccolor := GetColorRamp( lineIntBuffer[pixels -1] ) ;

          {$IFDEF OXYGENE}
            _buffer[_offset+soffset  ]   := Byte(ccolor.ARGB and $FF) ;
          {$ELSE}
            PByte( NativeInt(_buffer) +soffset )^   := Byte(ccolor.ARGB and $FF) ;
          {$ENDIF}
          ccolor.ARGB := ccolor.ARGB shr 8 ;
          {$IFDEF OXYGENE}
            _buffer[_offset+soffset+1] := Byte(ccolor.ARGB and $FF) ;
          {$ELSE}
            PByte( NativeInt(_buffer) +soffset +1)^ := Byte(ccolor.ARGB and $FF) ;
          {$ENDIF}
          ccolor.ARGB := ccolor.ARGB shr 8 ;
          {$IFDEF OXYGENE}
            _buffer[_offset+soffset+2] := Byte(ccolor.ARGB and $FF) ;
          {$ELSE}
            PByte( NativeInt(_buffer) +soffset +2)^ := Byte(ccolor.ARGB and $FF) ;
          {$ENDIF}
        end
        else begin //Word
          for i := start to pixels -2 do begin
            if lineWBuffer[i] = FNoDataValue then begin
              ccolor := colorNoData ;
              makeTransparent := True ;
            end
            else
              ccolor:= GetColorRamp(lineWBuffer[i]) ;

            {$IFDEF OXYGENE}
              _buffer[_offset+soffset  ] := ccolor.R ;
              _buffer[_offset+soffset+1] := ccolor.G ;
              _buffer[_offset+soffset+2] := ccolor.B ;
            {$ELSE}
              PInteger( NativeInt(_buffer) +soffset )^ := Integer(ccolor) ;
            {$ENDIF}
              soffset := soffset +3 ;
          end ;

          // last triple
          if lineWBuffer[pixels -1] = FNoDataValue then begin
            ccolor := colorNoData ;
            makeTransparent := True ;
          end
          else
            ccolor := GetColorRamp( lineWBuffer[pixels -1] ) ;

          {$IFDEF OXYGENE}
            _buffer[_offset+soffset  ]   := Byte(ccolor.ARGB and $FF) ;
          {$ELSE}
            PByte( NativeInt(_buffer) +soffset )^   := Byte(ccolor.ARGB and $FF) ;
          {$ENDIF}
          ccolor.ARGB := ccolor.ARGB shr 8 ;
          {$IFDEF OXYGENE}
            _buffer[_offset+soffset+1] := Byte(ccolor.ARGB and $FF) ;
          {$ELSE}
            PByte( NativeInt(_buffer) +soffset +1)^ := Byte(ccolor.ARGB and $FF) ;
          {$ENDIF}
          ccolor.ARGB := ccolor.ARGB shr 8 ;
          {$IFDEF OXYGENE}
            _buffer[_offset+soffset+2] := Byte(ccolor.ARGB and $FF) ;
          {$ELSE}
            PByte( NativeInt(_buffer) +soffset +2)^ := Byte(ccolor.ARGB and $FF) ;
          {$ENDIF}
        end ;

      end ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  procedure TGIS_LayerTIFF.prepareMinMaxZ(
    const _zoom : Double = -1
  ) ;
  var
    i, k,
    l_no  : Integer ;
    zoom  : Double ;
    lines : Integer ;
    arr   : TGIS_SingleArray ;
    is_graygrid   : Boolean ;
    available_res : TGIS_ListOfStrings ;
    stats_res     : TGIS_StatisticsLayerResult ;
  const
    MAX_LINES  = 900 ;
    WORD_MAX_Z = 32760 ;
  begin
    FMinZ :=  GIS_MAX_SINGLE ;
    FMaxZ := -GIS_MAX_SINGLE ;
    mustPrepareMinMaxZ := False ;

    // try statistics first
    available_res := Statistics.AvailableResults ;
    if available_res.Count > 0 then begin
      stats_res := Statistics.Get( available_res[0] ) ;
      if not IsNan(stats_res.Min.Value) then
        FMinZ := stats_res.Min.Value ;
      if not IsNan(stats_res.Max.Value) then
        FMaxZ := stats_res.Max.Value ;

      if (FMinZ <> GIS_MAX_SINGLE) and (FMaxZ <> -GIS_MAX_SINGLE) then begin
        FExtent3D.ZMin := FMinZ ;
        FExtent3D.ZMax := FMaxZ ;
        exit ;
      end ;
    end ;

    Alive ;

    if (_zoom > 0) AND (_zoom <= 1) then begin
      lines := RoundS(actualBitHeight * _zoom) ;
      if lines = 0 then
        lines := 1 ;
    end
    else
    begin
      if MAX_LINES > actualBitHeight then
        lines := actualBitHeight
      else
        lines := MAX_LINES ;
    end ;

    if isCompressed then begin
      if length(fileDecoder) = 0 then
        initDecoder ;
    end;

    zoom := (1.0 *actualBitHeight) /lines ;
    is_graygrid := False ;
    if ((not FIsNativeGridImage) and  rgbAsGrid  and (FGridBand = 0)) or
       ((FGridBand = 0) and (FBandsCount > 1))
    then
      is_graygrid := True ;


    SetLength(arr, actualBitWidth ) ;
    for i := 0 to lines -1 do begin

      l_no := TruncS(i*zoom) ;
      if is_graygrid then
        getNativeLineGrayGrid(arr, l_no, 0, actualBitWidth)
      else
        getNativeLine(arr, l_no, 0, actualBitWidth) ;

      for k := 0 to actualBitWidth -1 do begin
        if arr[k] <> FNoDataValue then
        begin
          if (arr[k] <= -GIS_MAX_SINGLE) or
             (arr[k] >=  GIS_MAX_SINGLE) then
          begin
            Params.Pixel.GridNoValue :=  arr[k] ;
            FNoDataValue := arr[k] ;
            continue ;
          end
          else
          if arr[k] = GIS_GRID_NOVALUE  then
          begin
            Params.Pixel.GridNoValue := arr[k] ;
            FNoDataValue := arr[k] ;
            continue ;
          end ;
          if arr[k] < FMinZ then
            FMinZ := arr[k]
          else
          if arr[k] > FMaxZ then
            FMaxZ := arr[k] ;
        end ;
      end ;
    end ;
    SetLength(arr, 0 ) ;
    FExtent3D.ZMin := FMinZ ;
    FExtent3D.ZMax := FMaxZ ;
  end ;

  function  TGIS_LayerTIFF.getNativeValue(
    const _pt : TPoint  ;
    const _ar : TGIS_DoubleArray
  ) : Boolean ;
  var
    offset     : Int64 ;
    stripno,
    stripline  : Integer ;
    word_arr   : Array [0..3] of Word ;
    buffer     : TGIS_SingleArray ;
    count      : Integer ;
    i          : Integer ;
  begin
    Result := True ;

    if FIsNativeGridImage then begin
      SetLength(buffer, FCellWidth) ;
      getNativeLine(buffer, _pt.Y, _pt.X, 1) ;
      _ar[0] := buffer[0] ;

      if _ar[0] = FNoDataValue then
        Result := False ;

      SetLength(buffer, 0 );
      exit ;
    end ;

    try

      if FIsGridImage then begin
        SetLength(buffer, FCellWidth) ;
        count := inherited getNativeLine( buffer, _pt.Y, 0, FCellWidth ) ;
        if count >= _pt.X then begin
          _ar[0] := buffer[_pt.X] ;
          if _ar[0] = FNoDataValue then
            Result := False ;
        end
        else
          Result := False ;
        SetLength(buffer, 0 );
      end
      else
      if is48BitsPerPixel then begin
        if not isCompressed then begin

          stripno   := _pt.Y div rowsPerStrip ;
          stripline := _pt.Y mod rowsPerStrip ;

          offset := listStripInfo64[stripno].StripOffsets +
                    DWORD(( 2*realLineWidth*stripline )
                            + FBandsCount*_pt.X*sizeOf(Word)) ;
          fileStream.Position := offset ;

          {$IFDEF OXYGENE}
            fileStream.ReadWord( word_arr[0], 2 ) ;
            fileStream.ReadWord( word_arr[1], 2 ) ;
            fileStream.ReadWord( word_arr[2], 2 ) ;
            if FBandsCount = 4 then
              fileStream.ReadWord( word_arr[3], 2 ) ;
          {$ELSE}
            if FBandsCount = 4 then
              fileStream.Read( word_arr[0], 8 )
            else
              fileStream.Read( word_arr[0], 6 ) ;
          {$ENDIF}
          for i := 0 to FBandsCount -1 do
            _ar[i] := word_arr[i] ; // In TIFF files BGR
        end
        else begin
          SetLength(buffer, FBandsCount) ;
          getNativeLine48(buffer, _pt.Y, _pt.X, 1) ;
          for i := 0 to FBandsCount -1 do
            _ar[i] := buffer[i] ; // In TIFF files BGR
          SetLength(buffer, 0 );
        end;
      end ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  function TGIS_LayerTIFF.getNativeLine(
    const _buffer   : TGIS_SingleArray ;
    const _linenr   : Integer          ;
    const _startIdx : Integer          ;
    const _count    : Integer
  ) : Integer ;
  var
    i           : Integer ;
    buffer      :  TGIS_SingleArray ;
    startIdx    : Integer ;
    pixcount    : Integer ;
    buf, wbuf   : TBytes ;
    bytescount  : Integer ;
    bidx        : Integer ;
    cpn         : Boolean ;
    vcount      : Integer ;
    dd          : Double ;
    bl : Integer ;
    procedure   set_native_req(const _rq : Boolean) ;
    var
      decno : Integer ;
    begin
      if _rq then
        for decno := low(fileDecoder) to high(fileDecoder) do begin
          decodeState[decno].NativeRequired := True ;
          fileDecoder[decno].DecodeState :=  decodeState[decno] ;
        end
        else
        for decno := low(fileDecoder) to high(fileDecoder) do begin
          decodeState[decno].NativeRequired := False ;
          fileDecoder[decno].DecodeState :=  decodeState[decno] ;
        end;
    end;
  begin
    if isBuilt then begin
      Result := inherited getNativeLine(_buffer, _linenr, _startIdx, _count) ;
      exit ;
    end;

    try
      bidx := 0 ;
      pixcount := _count ;
      vcount := FBandsCount2*pixcount ;
      startIdx := _startIdx ;
      if FIsGridImage and (FBandsCount2 > 1)then begin
        if length(_buffer) = _count*FBandsCount2 then begin
          cpn := False ;
          buffer   := _buffer ;
        end
        else begin
          cpn := True ;
          SetLength(buffer, vcount) ;
        end;
        bidx := Params.Pixel.GridBand -1 ;
        if bidx < 0 then
          bidx := 0 ;
        bytescount := _count*bytesPerPixel ;
      end
      else begin
        cpn := False ;
        buffer   := _buffer ;
        bytescount := _count*bytesPerPixel ;
      end;

      if isPlanar1 then begin
        SetLength(buf, bytescount) ;
        if (bitsPerBand[0] mod 8) <> 0 then begin
          bytescount := _count*bytesPerPixel ;
          SetLength(wbuf, _count*bytesPerPixel) ;
          if isCompressed and (bitsPerBand[0] = 12)then begin
            SetLength(buf, bytescount) ;
            set_native_req(True) ;
            Result := getLineBits(buf, 0, _linenr, startIdx, pixcount) div bytesPerPixel ;
            set_native_req(False) ;
          end
          else begin
            Result := getLineBits(buf, 0, _linenr, startIdx, pixcount) div bytesPerPixel ;
            convertBitsStringToBytes(buf, 0, wbuf, 0, _startIdx, pixcount) ;
            SetLength(buf, 0) ;
            buf := wbuf ;
          end;
        end
        else
          Result := getLineBits(buf, 0, _linenr, startIdx, pixcount) div bytesPerPixel ;
      end
      else begin
        if floatDataType then begin
          Result := getNativeLine2(_buffer, _linenr, startIdx, _count) ;
          exit ;
        end ;
        SetLength(buf, bytescount) ;
        Result := getNativeLine2Bytes(buf, _linenr, startIdx, _count) div bytesPerPixel ;
      end;

      case bandDataType of
         TGIS_BandDataType.dtU8 :
          begin
            for i := 0 to vcount -1 do
              buffer[i] := buf[i] ;
          end ;
         TGIS_BandDataType.dtU16 :
          begin
            if bigEndian then
              convertLineToLE(buf, 2) ;
            for i := 0 to vcount -1 do begin
            {$IFDEF OXYGENE}
               buffer[i] := BitConverter.ToUInt16(buf, 2*i) ;
            {$ELSE}
               buffer[i] := (PWord(@buf[2*i]))^ ;
            {$ENDIF}
            end;
          end ;
        TGIS_BandDataType.dtS16 :
          begin
            if bigEndian then
              convertLineToLE(buf, 2) ;
            for i := 0 to vcount -1 do begin
            {$IFDEF OXYGENE}
               buffer[i] := BitConverter.ToInt16(buf, 2*i) ;
            {$ELSE}
               buffer[i] := (PSmallInt(@buf[2*i]))^ ;
            {$ENDIF}
            end;
          end ;
        TGIS_BandDataType.dtU32 :
          begin
            if bigEndian then
              convertLineToLE(buf, 4) ;
            for i := 0 to vcount -1 do begin
            {$IFDEF OXYGENE}
               buffer[i] := BitConverter.ToUInt32(buf, 4*i) ;
            {$ELSE}
               buffer[i] := (PCardinal(@buf[4*i]))^ ;
            {$ENDIF}
            end;
          end ;
        TGIS_BandDataType.dtS32 :
          begin
            if bigEndian then
              convertLineToLE(buf, 4) ;
            for i := 0 to vcount -1 do begin
            {$IFDEF OXYGENE}
               buffer[i] := BitConverter.ToInt32(buf, 4*i) ;
            {$ELSE}
               buffer[i] := (PInteger(@buf[4*i]))^ ;
            {$ENDIF}
            end;
          end ;
        TGIS_BandDataType.dtF32 :
          begin
            if bigEndian then
              convertLineToLE(buf, 4) ;
            for i := 0 to vcount -1 do begin
            {$IFDEF OXYGENE}
               buffer[i] := BitConverter.ToSingle(buf, 4*i) ;
            {$ELSE}
               buffer[i] := (PSingle(@buf[4*i]))^ ;
            {$ENDIF}
             if IsNan(buffer[i]) then
               buffer[i] :=  FNoDataValue ;

            end;
          end ;
        TGIS_BandDataType.dtF64 :
          begin
            if bigEndian then
              convertLineToLE(buf, 8) ;
            for i := 0 to vcount -1 do begin
            {$IFDEF OXYGENE}
               dd := BitConverter.ToDouble(buf, 8*i) ;
            {$ELSE}
               dd := PDouble(@buf[8*i])^ ;
            {$ENDIF}
               if IsNan(buffer[i]) then
                 buffer[i] :=  FNoDataValue ;
               if (dd < -GIS_MAX_SINGLE) or (dd > GIS_MAX_SINGLE) then
                 buffer[i] := GIS_GRID_NOVALUE
               else
                 buffer[i] := dd ;
            end;
          end
      else
        exit ;
      end;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;

    if cpn then begin
      if Params.Pixel.GridBand > 0 then begin
        for i := 0 to _count -1 do
          _buffer[i] := buffer[i*FBandsCount2 + bidx] ;
      end
      else begin
        bl := length(_buffer) ;
        for i := 0 to bl -1 do
          _buffer[i] := buffer[i] ;
      end;
      SetLength(buffer, 0) ;
    end;

  end ;

  function TGIS_LayerTIFF.getNativeLine2(
    const _buffer   : TGIS_SingleArray ;
    const _linenr   : Integer          ;
    const _startIdx : Integer          ;
    const _count    : Integer
  ) : Integer ;
  var
    i           : Integer ;
    buffer      :  TGIS_SingleArray ;
    rgb_idx     : Integer ;
    pixels : Integer ;
    lpixel : Integer ;
    lcol, rcol : Integer ;
    cols : Integer ;
    stripline : Integer ;
    lstart : Integer ;
    firsttile : Integer ;
    line : Integer ;
    colidx : Integer ;
    stripno : Integer ;
    psi : TGIS_FileTIFF_StripInfo64 ;
    offset : Int64 ;
    bytes : Integer ;
    readin : Integer ;
    pixsize : Integer ;
    bandtiles : Integer ;
    bytesno : Integer ;
    byteBuffer : TBytes ;
    lbandsMap : Array [0..19] of Integer ;
    mbno : Integer ;
  begin
    if isBuilt then begin
      Result := inherited getNativeLine(_buffer, _linenr, _startIdx, _count) ;
      exit ;
    end;

    try
      pixsize := bytesPerBand[0] ;
      line := _linenr ;
      pixels := _count ;
      lpixel := _startIdx ;
      bytesno := pixsize*pixels ;

      if FIsGridImage and (FGridBand > 0) then begin
        mbno := 0 ;
        lbandsMap[0] := FGridBand -1 ;
      end
      else begin
        mbno := 2 ;
        for i := 0 to 2 do
          lbandsMap[i] := bandsMap[i] ;
      end;


      SetLength(buffer, pixels) ;
      SetLength(byteBuffer, bytesno) ;

      if isTiffTiled then begin
        lcol := lpixel div tileWidth ;
        rcol := (lpixel + pixels -1) div tileWidth ;
        cols := rcol -lcol +1 ;
        stripline := line mod tileLength ;
        firsttile := line div tileLength ;
        firsttile := firsttile*tilesColumns ;
        lpixel := lpixel -lcol*tileWidth ;
      end
      else begin
        firsttile := 0 ;
        stripline := 0 ;
        lcol := 0 ;
        cols := 1 ;
      end ;

      bandtiles := tilesColumns*tilesRows ;


      for rgb_idx := 0 to mbno do begin
        lstart := lpixel*pixsize ;
        readin := 0 ;
        for colidx := 0 to cols -1 do begin

          if isTiffTiled then begin
            stripno   := firsttile + lcol +colidx +lbandsMap[rgb_idx]*bandtiles ;
            if stripno >= countStripInfo then
              stripno := countStripInfo - 1 ;
            psi := listStripInfo64[stripno] ;

            offset := Int64(psi.StripOffsets) +
                      Int64(tileWidth * stripline *pixsize) + Int64(lstart) ;

            if colidx = cols -1 then begin
              bytes := pixsize*pixels -readin
            end
            else
            if colidx = 0 then
              bytes := pixsize*tileWidth -lstart
            else
              bytes := pixsize*tileWidth ;

          end
          else begin

            stripno   := lbandsMap[rgb_idx]*((actualBitHeight +rowsPerStrip -1 )div rowsPerStrip)
                         +(line div rowsPerStrip);
            stripline := line mod rowsPerStrip ;

            psi := listStripInfo64[stripno];
            offset := Int64(psi.StripOffsets) +
                      Int64(tileWidth * stripline *pixsize) + Int64(lstart) ;

            bytes  := bytesno ;
          end ;

          fileStream.Position := offset  ;
          {$IFDEF OXYGENE}
            readin := readin +fileStream.Read( byteBuffer, readin, bytes ) ;
          {$ELSE}
            readin := readin +fileStream.Read( byteBuffer[readin], bytes ) ;
          {$ENDIF}

          lstart := 0 ;
        end ;

        if bigEndian then
          convertLineToLE( byteBuffer, pixsize) ;

        case bandDataType of
          TGIS_BandDataType.dtF32 :
            begin
              for i := 0 to pixels -1 do begin
                {$IFDEF OXYGENE}
                   buffer[i] := BitConverter.ToSingle( byteBuffer, 4*i) ;
                {$ELSE}
                   buffer[i] := (PSingle(@ byteBuffer[4*i]))^ ;
                {$ENDIF}
                 if IsNan(buffer[i]) then
                   buffer[i] :=  FNoDataValue ;
              end ;
            end;
          TGIS_BandDataType.dtF64 :
            begin
              for i := 0 to pixels -1 do begin
              {$IFDEF OXYGENE}
                 buffer[i] := BitConverter.ToDouble( byteBuffer, 8*i) ;
              {$ELSE}
                 buffer[i] := (PDouble(@ byteBuffer[8*i]))^ ;
              {$ENDIF}
               if IsNan(buffer[i]) then
                 buffer[i] :=  FNoDataValue ;
              end;
            end
          else begin
            Result := pixels ;
            exit ;
          end;
        end;

        if FGridBand > 0 then begin
          for i := 0 to pixels -1 do
            _buffer[i] := buffer[i] ;
        end
        else begin
          for i := 0 to pixels -1 do
            _buffer[3*i +rgb_idx] := buffer[i] ;
        end;
      end ;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;

    Result := pixels ;
    SetLength(byteBuffer, 0) ;
    SetLength(buffer, 0) ;
  end ;


  procedure TGIS_LayerTIFF.Alive ;
  begin
    if IsStringEmpty( Path ) then exit ;
    if isBuilt then exit ;

    if ovrFirstPage > 1 then begin
      if not assigned( fileStreamMain ) then begin
        fileStreamMain := openBufferedFileStream( Path ) ;
      end;
      if not assigned( fileStreamOvr ) then
        fileStreamOvr := openBufferedFileStream( Path + '.ovr') ;
      if not assigned( fileStream) then begin
        if FCurrentPage <= ovrFirstPage then
          fileStream := fileStreamMain
        else
          fileStream := fileStreamOvr ;
      end;
    end
    else
    if not assigned( fileStream ) then begin
      fileStream := openBufferedFileStream( Path ) ;
    end ;

  end ;

  function TGIS_LayerTIFF.getNativeLine48(
    const _buffer   : TGIS_SingleArray ;
    const _linenr   : Integer          ;
    const _startIdx : Integer          ;
    const _count    : Integer
  ) : Integer ;
  var
    i, idx    : Integer ;
    line      : Integer ;
    pixels    : Integer ;
    lpixel    : Integer ;
    warr      : Array of Word ;
    stripno,
    stripcount,
    offset,
    bytes,
    stripline : Integer ;
    psi       : TGIS_FileTIFF_StripInfo64 ;
    pmul      : Integer ;
    bandidx   : Integer ;
    stripwidthex : Integer ;
    totbytes  : Integer ;
    totstart,
    idxstart  : Integer ;
    k, no     : Integer ;
    pixcount,
    apixcount : Integer ;
    buffer    : TBytes  ;
    buffoff   : Integer ;
    sbuffer   : TBytes  ;
    rbytes    : Integer ;
  begin
    try

      pixels := _count ;
      line   := _linenr ;
      Result := _count ;

      rbytes := FBandsCount*2*_count ;

      lpixel := _startIdx ;

      if FGridBand < 1 then
        bandidx := 0
      else
        bandidx := FGridBand -1 ;
      stripline := line mod rowsPerStrip ;

      if isPlanar1 then begin //RGB - configuration

        pmul := FBandsCount ;

        if not isCompressed then begin

          stripno   := line div rowsPerStrip ;

          psi := listStripInfo64[stripno] ;
          SetLength( warr, FBandsCount*_count ) ;
          offset := Int64(psi.StripOffsets)
                    + (Int64(realLineWidth*stripline) + Int64(lpixel+3))*2 ;
          fileStream.Position := offset ;
          {$IFDEF OXYGENE}
            for k := 0 to FBandsCount*_count-1 do
              fileStream.ReadWord( warr[k] ) ;
          {$ELSE}
            fileStream.Read( warr[0], rbytes ) ;
          {$ENDIF}
        end
        else begin
          totbytes  := _count*2*FBandsCount ;
          SetLength( sbuffer, totbytes) ;
          if not isTiffTiled then begin
            totstart  := _startIdx*2*FBandsCount ;
            Result := readCompressedLineOnFly(sbuffer, 0, line, totstart, totbytes);
            SetLength( warr, FBandsCount*_count ) ;

            i := 0 ;
            for k := 0 to (_count*FBandsCount) -1 do begin
              {$IFDEF OXYGENE}
                warr[k] := (Word(sbuffer[i +1]) shl 8) or sbuffer[i] ;
              {$ELSE}
                warr[k] := (Word(PByte(NativeInt(sbuffer) +i +1)^) shl 8) or
                            PByte(NativeInt(sbuffer) +i)^ ;
              {$ENDIF}
              inc(i, 2) ;
            end;
          end
          else begin
            stripno :=  line div tileLength ;
            stripno := stripno * tilesColumns ;
            idxstart := _startIdx mod tileWidth ;
            stripno := stripno +(_startIdx div tileWidth) ;
            stripcount := ((_startIdx +_count) div tileWidth) -
                          (_startIdx div tileWidth) +1 ;
            buffer := sbuffer ;
            SetLength( warr, FBandsCount*_count ) ;
            pixcount := _count ;
            if stripcount > 1 then
              apixcount := tileWidth -idxstart
            else
              apixcount := pixcount ;

            buffoff := 0 ;
            no := 0 ;
            totstart := idxstart*3 ;//24bpp like
            for i := 0 to stripcount -1 do begin
              bytes := apixcount*2*FBandsCount ;
              no := no +readCompressedLineOnFly(buffer, buffoff,
                                    stripline, totstart, bytes, stripno);
              buffoff := no;
              pixcount := pixcount -apixcount ;
              if pixcount > tileWidth then
                apixcount := tileWidth
              else
                apixcount := pixcount ;

              inc( stripno) ;
              totstart := 0 ;
            end;
            i := 0 ;
            for k := 0 to (_count*FBandsCount) -1 do begin
              {$IFDEF OXYGENE}
                warr[k] := (Word(sbuffer[i +1]) shl 8) or sbuffer[i] ;
              {$ELSE}
                warr[k] := (Word(PByte(NativeInt(sbuffer) +i +1)^) shl 8) or
                            PByte(NativeInt(sbuffer) +i)^ ;
              {$ENDIF}
              inc(i, 2) ;
            end;

          end;
          SetLength( sbuffer, 0) ;
        end;
      end
      else begin
        pmul := 1 ;

        stripno   := bandidx*((actualBitHeight +rowsPerStrip -1 )div rowsPerStrip)
                    +(line div rowsPerStrip);

        psi := listStripInfo64[stripno];

        stripwidthex := 2*(realLineWidth div 3) ;

        offset := Int64(psi.StripOffsets)
                  + Int64(stripwidthex*stripline) + Int64(lpixel*2) ;
        SetLength( warr, pixels ) ;
        fileStream.Position := offset ;
        {$IFDEF OXYGENE}
          for k := 0 to pixels-1 do
            fileStream.ReadWord( warr[k] ) ;
        {$ELSE}
          fileStream.Read( warr[0], 2*pixels ) ;
        {$ENDIF}

      end;

      for i := 0 to pixels - 1 do begin
        if bigEndian then
          for idx := 0 to FBandsCount -1 do
            _buffer[pmul*i +idx] := Swap(warr[pmul*i +idx])
        else
          for idx := 0 to FBandsCount -1 do
            _buffer[pmul*i +idx] := warr[pmul*i +idx] ;
      end ;
      SetLength( warr, 0 ) ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  function TGIS_LayerTIFF.DormantGain
    : Integer ;
  begin
    case DormantMode of
      TGIS_LayerDormantMode.Off :
        Result := 0;
      else
        Result := 10 ;
    end ;
  end ;

  procedure TGIS_LayerTIFF.Dormant ;
  var
    i     : Integer ;
    count : Integer ;
    dec   : TGIS_FileTIFFDecoder ;
  begin
    if IsStringEmpty( Path ) and (not assigned(fileStream)) then exit ;

    if DormantMode = TGIS_LayerDormantMode.Off then
      exit ;

    count := length( fileDecoder ) ;
    if  count > 0 then begin
      for i := high(fileDecoder) downto low(fileDecoder) do
      begin
        dec := fileDecoder[i] ;
        FreeObject( dec ) ;
      end ;
      fileDecoder := nil ;
    end ;
    inherited ;
  end ;

//==============================================================================
// Lider.CG.GIS.GeoLayerTIFF
//==============================================================================

  class procedure GisLayerTIFF.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-TIFF', 'Tag Image File Format',
                   TGIS_LayerTIFF, '.tif;.tiff',
                   TGIS_RegisteredLayerType.Pixel,
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read,
                     TGIS_RegisteredOperationType.Write,
                     TGIS_RegisteredOperationType.&Create
                   ],
                   True
                 ) ;
    RegisterLayer( 'DK-TIFF', 'Tag Image File Format',
                   TGIS_LayerTIFF, '.tif;.tiff',
                   TGIS_RegisteredLayerType.Grid,
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read,
                     TGIS_RegisteredOperationType.Write,
                     TGIS_RegisteredOperationType.&Create
                   ],
                   True
                 ) ;
  end ;

//==============================================================================
// initialization/finalization
//==============================================================================

{$IFDEF DCC}
  initialization
    GisLayerTIFF.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.
