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
  Procedures to write a JPEG file of any size (supported by computer memory).
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoFileJPEG ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoFileJPEG"'}
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
{$IFDEF ISLAND}
namespace TatukGIS ;
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}
interface

{$IFDEF CLR}
  uses
    System.Runtime.InteropServices,
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    System.Classes,
    System.SysUtils,
    System.Math,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoFilePixel,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoStreams ;
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
  ///   Indicates that the JPEG file contains YBR components.
  /// </summary>
  JPEG_YBR      = 0 ;

  /// <summary>
  ///   Indicates that the JPEG file contains BGR components.
  /// </summary>
  JPEG_BGR      = 1 ;

  /// <summary>
  ///   Indicates that the JPEG file contains RGB components.
  /// </summary>
  JPEG_RGB      = 2 ;

  /// <summary>
  ///   Indicates that the JPEG file contains a grayscale image.
  /// </summary>
  JPEG_GRAYSCAL = 3 ;

  /// <summary>
  ///   Indicates that the JPEG file contains a alpha band.
  /// </summary>
  JPEG_ARGB     = 4 ;

  /// <summary>
  ///   Indicates that the JPEG file contains CMYK components.
  /// </summary>
  JPEG_CMYK     = 5 ;

type

  /// <summary>
  ///   The Class which encapsulates the writing of a bitmap file. A file of
  ///   any size can be written.
  /// </summary>
  TGIS_FileJPEG = class( TGIS_FilePixel )
    private
      bmpObj    : TGIS_Bitmap    ;
      pixObj    : TGIS_Pixels    ;
      writeMode : Boolean    ;
      isReady   : Boolean    ;
    protected
      /// <summary>
      ///   JPEG file stram.
      /// </summary>
      jpgFileStream : TGIS_FileStream;
    protected

      /// <inheritdoc/>
      procedure prepareCapabilities ; override;

    protected

      /// <summary>
      ///   Destroys an instance.
      /// </summary>
      procedure  doDestroy          ; override;
    public

      /// <summary>
      ///   Create and open a new JPEG file.
      /// </summary>
      constructor Create            ; overload; override;

      /// <inheritdoc/>
      /// <param name="_subformat">
      ///   unused
      /// </param>
      /// <param name="_ppi">
      ///   unused
      /// </param>
      constructor Create            ( const _path        : String      ;
                                      const _ext         : TGIS_Extent ;
                                      const _width       : Integer     ;
                                      const _height      : Integer     ;
                                      const _subformat   : TGIS_LayerPixelSubFormat     ;
                                      const _ppi         : Integer     ;
                                      const _cs          : TGIS_CSCoordinateSystem
                                    ) ; overload; override;

      /// <inheritdoc/>
      procedure   Write             ( const _x       : Integer ;
                                      const _y       : Integer ;
                                      const _pixels  : TGIS_Pixels ;
                                      const _pformat : TGIS_PixelFormat ;
                                      const _width   : Integer ;
                                      const _height  : Integer
                                    ) ; override;
  end ;

  /// <summary>
  ///   The Class which encapsulates JPEG decoder. A file of any size can be
  ///   read.
  /// </summary>
  TGIS_JPEGDecoder = class( TGIS_ObjectDisposable )

    private
      odecoder : TObject ;

    // properties access routines
    {$IFDEF OXYGENE} unit or protected {$ELSE} protected {$ENDIF}
      procedure  fset_Scale( const _sc : Integer ) ;
      function   fget_Scale : Integer ;
      function   fget_Precision : Integer ;
      procedure  fset_JpegInTiff( const _it : Boolean ) ;
      procedure  fset_MemoryThreshold ( const _mt : Integer ) ;
      function   fget_MemoryThreshold : Integer ;
      function   fget_JpegInitialized : Boolean ;
      procedure  fset_ComponentsJpeg( const _componentsJpeg : Integer ) ;
      procedure  fset_decodeMode( const _mode : Integer ) ;
      procedure  fset_NativeRequsted ( const _nrq : Boolean ) ;
    protected

      /// <summary>
      ///   Destroy a decoder instance.
      /// </summary>
      procedure doDestroy     ; override;
    public

      /// <summary>
      ///   Create a decoder instance.
      /// </summary>
      constructor Create      ;

      /// <summary>
      ///   Initialize JPEG. Performs initial decoding.
      /// </summary>
      /// <param name="_mode">
      ///   dormant mode
      /// </param>
      /// <returns>
      ///   error code
      /// </returns>
      function  Initialize    ( const _mode : TGIS_LayerDormantMode )
                              : Integer ;

      /// <summary>
      ///   Initialize JPEG. Performs initial decoding.
      /// </summary>
      /// <param name="_mode">
      ///   dormant mode
      /// </param>
      /// <returns>
      ///   error code
      /// </returns>
      function  Reinitialize  ( const _mode : TGIS_LayerDormantMode )
                              : Integer ;

      /// <summary>
      ///   Returns JPEG width.
      /// </summary>
      /// <returns>
      ///   image width in pixels
      /// </returns>
      function  Width         : Integer ;

      /// <summary>
      ///   Returns JPEG height.
      /// </summary>
      /// <returns>
      ///   image height in pixels
      /// </returns>
      function  Height        : Integer ;

      /// <summary>
      ///   Returns JPEG subformat info.
      /// </summary>
      /// <returns>
      ///   text like RGB, YBR, CMYK
      /// </returns>
      function  SubFormatInfo : String ;

      /// <summary>
      ///   Reading an image line.
      /// </summary>
      /// <param name="_buffer">
      ///   bytes buffer
      /// </param>
      /// <param name="_buferIndex">
      ///   offset in buffer
      /// </param>
      /// <param name="_line">
      ///   line number
      /// </param>
      /// <param name="_lskip">
      ///   left margin (bytes to skip)
      /// </param>
      /// <param name="_pixels">
      ///   byte count
      /// </param>
      /// <returns>
      ///   buffer position
      /// </returns>
      function  DecodeLine  ( const _buffer     : TBytes  ;
                              const _buferIndex : Integer ;
                              const _line       : Integer ;
                              const _lskip      : Integer ;
                              const _pixels     : Integer
                            ) : Integer ;

      /// <summary>
      ///   Load JPEG from file.
      /// </summary>
      /// <param name="_path">
      ///   file path
      /// </param>
      procedure LoadFromFile  ( const _path   : String
                              ) ;

      /// <summary>
      ///   Load JPEG from provided stream. After loading stream can be freed.
      /// </summary>
      /// <param name="_strm">
      ///   stream with JPEG; stream should be positioned at the beginning of
      ///   the JPEG image
      /// </param>
      procedure LoadFromStream( const _strm   : TStream
                              ) ;

      /// <summary>
      ///   Load default Huffman and quantization tables for abbreviated JPEGs.
      /// </summary>
      /// <param name="_quality">
      ///   JPEG quality
      /// </param>
      procedure LoadDefaultTables( const _quality : Double ) ;

      /// <summary>
      ///   Makes a decoder non-dormant. Reallocate files, memory etc.
      /// </summary>
      procedure Alive         ;

      /// <summary>
      ///   Makes a decoder dormant. Reduce memory consumption etc.
      /// </summary>
      procedure Dormant       ;
    public  // properties

      /// <summary>
      ///   Scale.
      /// </summary>
      property Scale           : Integer read  fget_Scale write fset_Scale ;

      /// <summary>
      ///   Scale.
      /// </summary>
      property Precision       : Integer read  fget_Precision ;

      /// <summary>
      ///   Memory threshold.
      /// </summary>
      property MemoryThreshold : Integer read  fget_MemoryThreshold
                                         write fset_MemoryThreshold ;
      /// <summary>
      ///   JPEG components indicator.
      /// </summary>
      property ComponentsJpeg  : Integer write fset_ComponentsJpeg ;

      /// <summary>
      ///   JPEG components indicator.
      /// </summary>
      property NativeRequested  : Boolean write fset_NativeRequsted ;

      /// <summary>
      ///   JPEG in TIFF.
      /// </summary>
      property JpegInTiff  : Boolean write fset_JpegInTiff ;

      /// <summary>
      ///   Mode of decoding indicator.
      /// </summary>
      property DecodeMode      : Integer write fset_decodeMode ;
      /// <summary>
      ///   True if the JPEF file has been initialized.
      /// </summary>
      property JpegInitialized : Boolean read  fget_JpegInitialized ;
  end;


//##############################################################################
implementation
{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoClasses ;
{$ENDIF}


const
  { Extension of world file (extension file for pixel layer). }
    WORLD_FILE_EXT_JPG2 = '.jgw' ;
    DEFAULT_MEMORY_THRESHOLD_JPG = 12 ; //In millions

type
  ArrayOfInt16   = array of SmallInt ;
  ArrayOfInteger = array of Integer  ;
  ArrayOfShort   = array of ShortInt ;
  ArrayOfByte    = array of Byte     ;
  ArrayOfWord    = array of Word     ;

  T_stHuffman = class
    ucSizeLo : ArrayOfShort ;
    ucValLo  : ArrayOfByte  ;
    ucSizeHi : ArrayOfShort ;
    ucValHi  : ArrayOfByte  ;
    cVal     : ArrayOfByte  ;
    cSize    : ArrayOfShort ;
  end ;

  T_fnWrite         = procedure( const _offset : Integer
                               ) of object;

  T_fnIdct          = procedure( const _offset_mcu   : Integer
                               ) of object;

  T_fndecodeHuffman = function ( const _offset : Integer ;
                                 const _index  : Integer
                               ) : Integer of object;

  T_stdecoder = class
    markerNext        : Integer ;
    markerSOF         : Integer ;

    iHeight           : Integer ; // image height in pixels
    iWidth            : Integer ; // image width in pixels
    iWidthR           : Integer ; // image width in pixels (rounded up to compressed width)
    nComponentsFrame  : Integer ; // 1 for Grayscale; 3 for RGB; 4 CMYK
    arComponent       : array[0..3] of Byte    ; // component identifier
    arHorzSampling    : array[0..3] of Integer ; // horizontal sampling factor
    arVertSampling    : array[0..3] of Integer ; // vertical sampling factor
    nQuantTableSel    : array[0..3] of Byte    ;

    imageFormat       : Integer  ;
    nHorzMcu          : Integer  ;
    nVertMcu          : Integer  ;
    nMcuSize          : Integer  ;
    iInc1             : Integer  ;  //set but not read
    iInc2             : Integer  ;
    arRows            : Integer ;
    arCols            : Integer ;
    fnWrite           : T_fnWrite ;
    arFnWrite         : array[0..3] of T_fnWrite ;
    nComponentsScan   : Integer ;  //set but not read
    nCurrComponent    : Array[0..3] of Integer ;
    nEntropyDestSelDC : Array[0..3] of Byte    ;
    nEntropyDestSelAC : Array[0..3] of Byte    ;
    nSpectralSelStart : Byte    ;
    nSpectralSelEnd   : Byte    ;
    nApproxBitHi      : Byte    ;
    nApproxBitLo      : Byte    ;
    nRestartInterval  : Integer ;
    nRestartPending   : Integer ;
    nRestartNext      : Integer ;
    arQuantization    : array[0..3]      of ArrayOfInteger ; // quantization table
    {$IFDEF OXYGENE}
      {$IFDEF ISLAND}
        arHuffman     : array of array of T_stHuffman    ;
      {$ELSE}
        arHuffman     : array[0..1] of array[0..3] of T_stHuffman    ;
      {$ENDIF}
    {$ELSE}
      arHuffman       : array[0..1,0..3] of T_stHuffman    ;
    {$ENDIF}

    nBufferOffset     : Integer ; // offset within buffer
    nImageOffset      : Integer ; // offset within final image

    uCode             : Cardinal;
    nSize             : Integer ;
    arLdc             : Array[0..3] of Integer ;
    iEobrun           : Integer ;

    bDetectedSOF      : Boolean ;
    public
      constructor Create ;
  end ;

type
  {$IFDEF OXYGENE}
    T_arLdc = Array of Integer ;
  {$ELSE}
    T_arLdc = Array[0..3] of Integer ;
  {$ENDIF}

  T_jpegdecoder = class( TGIS_ObjectDisposable )

    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      stdecoder        : T_stdecoder                  ;
      FMemoryThreshold : Integer                      ;
      FNativeRequested : Boolean                      ;
      decodeIdct       : T_fnIdct                     ;
    protected
      FScale          : Integer                       ;
      FComponentsJpeg : Integer                       ;
      FComponentsApp  : Integer                       ;
      FdecodeMode     : Integer                       ;
      FJpegInTiff     : Boolean                       ;
    {$IFDEF OXYGENE} unit or protected {$ELSE} protected {$ENDIF}
      arBlock         : array [0..63] of Integer      ; // buffer of 8x8 block
      arMcu           : ArrayOfInteger                ; // buffer of dct
      arQuantization  : array[0..3] of ArrayOfInteger ;

      arImage         : Array Of Integer              ; // output image
      arImageSize     : Integer                       ;
      arImagePartSize : Integer                       ;

      arImagePrecision : Integer                      ;
      arImageMaxVal    : Integer                      ;
      arImageMedVal    : Integer                      ;

      arPartsScales   : array of Integer              ;
      bufferedPartsNo : Integer                       ;
      nPartOffset     : Integer                       ;
      arCoefficient   : ArrayOfInteger                ;

      bytesPerPixel   : Integer   ;
      bytesPerLine    : Integer   ;

      nPartNeeded     : Integer                       ;
      nPart           : Integer                       ;

      arOffset        : array of Cardinal             ;
      arLdc           : array of T_arLdc              ;
      arCode          : array of Cardinal             ;
      arSize          : array of Integer              ;
      arRstPending    : array of Integer              ;

      {$IFDEF OXYGENE}
        oStream       : TGIS_BaseStream               ; // image stream
      {$ELSE}
        oStream       : TStream                       ; // image stream
      {$ENDIF}

      stripHeight     : Integer                       ;
      stripNo         : Integer                       ;
      partsNo       : Integer                         ; //set but not read
      isProgressive   : Boolean                       ;
      rsInc           : Integer                       ;
      dormantMode     : TGIS_LayerDormantMode         ;
      definedDHT      : Boolean                       ;

    protected // properties access routines
      procedure  fset_Scale( const _sc : Integer ) ;
      procedure  fset_JpegInTiff( const _it : Boolean ) ;
      function   fget_JpegInitialized   : Boolean ;

    protected

      function  decodeImage            ( const _read_SOI : Boolean
                                       ) : Integer ;

      function  skipMarker             : Integer ;
      function  readMarkerNext         : Integer ;
      function  readMarkerSOI          : Integer ;
      function  readMarkerSOF          : Integer ;
      function  readMarkerDHT          : Integer ;
      function  readMarkerSOS          : Integer ;
      function  readMarkerDQT          : Integer ;
      function  readMarkerDRI          : Integer ;
      function  readMarkerRST          : Integer ;
      function  readMarkerAPP          : Integer ;

      function  readWord               : Word ;
      function  readByte               : Byte ;
      function  readHuffmanWord        : Word ;

      procedure write400_1             ( const _offset       : Integer
                                       ) ;
      procedure write400_2             ( const _offset       : Integer
                                       ) ;
      procedure write400_4             ( const _offset       : Integer
                                       ) ;
      procedure write400_8             ( const _offset       : Integer
                                       ) ;
      procedure write444_1             ( const _offset       : Integer
                                       ) ;
      procedure write444_2             ( const _offset       : Integer
                                       ) ;
      procedure write444_4             ( const _offset       : Integer
                                       ) ;
      procedure write444_8             ( const _offset       : Integer
                                       ) ;
      procedure write412_1             ( const _offset       : Integer
                                       ) ;
      procedure write412_2             ( const _offset       : Integer
                                       ) ;
      procedure write412_4             ( const _offset       : Integer
                                       ) ;
      procedure write412_8             ( const _offset       : Integer
                                       ) ;
      procedure write411_1             ( const _offset       : Integer
                                       ) ;
      procedure write411_2             ( const _offset       : Integer
                                       ) ;
      procedure write411_4             ( const _offset       : Integer
                                       ) ;
      procedure write411_8             ( const _offset       : Integer
                                       ) ;
      procedure write422_1             ( const _offset       : Integer
                                       ) ;
      procedure write422_2             ( const _offset       : Integer
                                       ) ;
      procedure write422_4             ( const _offset       : Integer
                                       ) ;
      procedure write422_8             ( const _offset       : Integer
                                       ) ;

      procedure skipHuffman            ( const _index        : Integer
                                       ) ;

      function  decodeHuffmanSequential( const _offset       : Integer ;
                                         const _index        : Integer
                                       ) : Integer ;

      function  decodeHuffmanFirstDC   ( const _offset       : Integer ;
                                         const _index        : Integer
                                       ) : Integer ;
      function  decodeHuffmanRefineDC  ( const _offset       : Integer ;
                                         const _index        : Integer
                                       ) : Integer ;
      function  decodeHuffmanFirstAC   ( const _offset       : Integer ;
                                         const _index        : Integer
                                       ) : Integer ;
      function  decodeHuffmanRefineAC  ( const _offset       : Integer ;
                                         const _index        : Integer
                                       ) : Integer ;

      procedure clearMcu               ( const _offset_mcu   : Integer ;
                                         const _size         : Integer
                                       ) ;
      procedure copyMcuToCoef          ( const _offset_mcu   : Integer ;
                                         const _offset_coef  : Integer ;
                                         const _size         : Integer
                                       ) ;
      procedure copyCoefToMcu          ( const _offset_coef  : Integer ;
                                         const _offset_mcu   : Integer ;
                                         const _size         : Integer
                                       ) ;
      procedure decodeZigzag           ( const _offset_mcu   : Integer ;
                                         const _quantization : ArrayOfInteger
                                       ) ;
      procedure decodeIdct_1           ( const _offset_mcu   : Integer
                                       ) ;
      procedure decodeIdct_2           ( const _offset_mcu   : Integer
                                       ) ;
      procedure decodeIdct_4           ( const _offset_mcu   : Integer
                                       ) ;
      procedure decodeIdct_8           ( const _offset_mcu   : Integer
                                       ) ;

      function  decodeNoninterleaved   : Integer ;
      function  decodeInterleaved      ( const _flag         : Boolean
                                       ) : Integer ;
      function  decodeInterleavedForced  : Integer ;
      procedure synchronizeBuffer      ;
      function  dodecode               : Integer ;
    {$IFDEF OXYGENE} unit or protected {$ELSE} protected {$ENDIF}
      function  decodeLine           ( const _buffer       : TBytes  ;
                                       const _buferIndex   : Integer ;
                                       const _line         : Integer ;
                                       const _lskip        : Integer ;
                                       const _pixels       : Integer
                                     ) : Integer ;
      function  doInitialize           ( const _mode : TGIS_LayerDormantMode )
                                       : Integer ; overload;
      function  doReinitialize         ( const _mode : TGIS_LayerDormantMode
                                       ) : Integer ; overload;
      procedure loadDefaultTables      ( const _quality : Double
                                       ) ;
      procedure doDormant              ;
      procedure doAlive                ;

      procedure fset_decodeMode         ( const _mode : Integer ) ;
    protected
      procedure   doDestroy            ; override;
    public
      constructor Create               ;

    public  // properties

      // Scale.
      property Scale           : Integer  read  FScale write fset_Scale ;
      property MemoryThreshold  : Integer read  FMemoryThreshold
                                          write FMemoryThreshold ;
      property NativeRequested  : Boolean read  FNativeRequested
                                          write FNativeRequested ;
      property ComponentsJpeg  : Integer  read  FComponentsJpeg
                                          write FComponentsJpeg ;
      property JpegInTiff      : Boolean  write fset_JpegInTiff ;
      property decodeMode      : Integer  write fset_decodeMode ;
      property JpegInitialized : Boolean  read  fget_JpegInitialized ;
  end;

type

  T_JPGCONST = class
  public
  const
    NBITS = 8;

    // formart
    FORMAT_400 = 0;
    FORMAT_411 = 1;
    FORMAT_422 = 2;
    FORMAT_444 = 3;

    // operation codes
    SUCCESS                                                             = 0;
    ERR_SOF_ALREADY_DETECTED                                            = 1;
    ERR_INVALID_SAMPLE_PRECISION                                        = 2;
    ERR_TWELVE_BIT_SAMPLE_PRECISION_NOT_SUPPORTED                       = 3;
    ERR_ZERO_NUMBER_OF_LINES_NOT_SUPPORTED                              = 4;
    ERR_LARGE_NUMBER_OF_LINES_NOT_SUPPORTED                             = 5;
    ERR_ZERO_SAMPLES_PER_LINE                                           = 6;
    ERR_LARGE_NUMBER_OF_SAMPLES_PER_LINE_NOT_SUPPORTED                  = 7;
    ERR_MARKER_SOI_ALREADY_DETECTED                                     = 8;
    ERR_INVALID_NUMBER_OF_IMAGE_COMPONENTS_IN_FRAME                     = 9;
    ERR_INVALID_FRAME_HEADER_LENGTH                                     = 10;
    ERR_INVALID_HORIZONTAL_SAMPLING_FACTOR                              = 11;
    ERR_INVALID_VERTICAL_SAMPLING_FACTOR                                = 12;
    ERR_INVALID_QUANTIZATION_TABLE_DESTINATION_SELECTOR                 = 13;
    ERR_UNSUPPORTED_IMAGE_FORMAT                                        = 14;
    ERR_SOF_NOT_DETECTED                                                = 15;
    ERR_INVALID_NUMBER_OF_COMPONENTS_IN_SCAN                            = 16;
    ERR_INVALID_SCAN_HEADER_LENGTH                                      = 17;
    ERR_INVALID_START_OF_SPECTRAL_SELECTION                             = 18;
    ERR_INVALID_END_OF_SPECTRAL_SELECTION                               = 19;
    ERR_INVALID_SUCCESSIVE_APPROXIMATION_BIT_POSITION_HIGH              = 20;
    ERR_INVALID_SUCCESSIVE_APPROXIMATION_BIT_POSITION_LOW               = 21;
    ERR_INVALID_QUANTIZATION_TABLE_ELEMENT_PRECISION                    = 22;
    ERR_INVALID_QUANTIZATION_TABLE_DESTINATION_IDENTIFIER               = 23;
    ERR_QUANTIZATION_TABLE_ELEMENT_ZERO                                 = 24;
    ERR_INVALID_QUANTIZATION_TABLE_DEFINITION_LENGTH                    = 25;
    ERR_INVALID_DEFINE_RESTART_INTERVAL_SEGMENT_LENGTH                  = 26;
    ERR_MARKER_SOF3_NOT_SUPPORTED                                       = 27;
    ERR_MARKER_SOF5_NOT_SUPPORTED                                       = 28;
    ERR_MARKER_SOF6_NOT_SUPPORTED                                       = 29;
    ERR_MARKER_SOF7_NOT_SUPPORTED                                       = 30;
    ERR_MARKER_JPG_NOT_SUPPORTED                                        = 31;
    ERR_MARKER_SOF9_NOT_SUPPORTED                                       = 32;
    ERR_MARKER_SOF10_NOT_SUPPORTED                                      = 33;
    ERR_MARKER_SOF11_NOT_SUPPORTED                                      = 34;
    ERR_MARKER_SOF13_NOT_SUPPORTED                                      = 35;
    ERR_MARKER_SOF14_NOT_SUPPORTED                                      = 36;
    ERR_MARKER_SOF15_NOT_SUPPORTED                                      = 37;
    ERR_MARKER_DAC_NOT_SUPPORTED                                        = 38;
    ERR_MARKER_DNL_NOT_SUPPORTED                                        = 39;
    ERR_MARKER_DRI_NOT_SUPPORTED                                        = 40;
    ERR_MARKER_DHP_NOT_SUPPORTED                                        = 41;
    ERR_MARKER_EXP_NOT_SUPPORTED                                        = 42;
    ERR_MARKER_JPG0_NOT_SUPPORTED                                       = 43;
    ERR_MARKER_JPG1_NOT_SUPPORTED                                       = 44;
    ERR_MARKER_JPG2_NOT_SUPPORTED                                       = 45;
    ERR_MARKER_JPG3_NOT_SUPPORTED                                       = 46;
    ERR_MARKER_JPG4_NOT_SUPPORTED                                       = 47;
    ERR_MARKER_JPG5_NOT_SUPPORTED                                       = 48;
    ERR_MARKER_JPG6_NOT_SUPPORTED                                       = 49;
    ERR_MARKER_JPG7_NOT_SUPPORTED                                       = 50;
    ERR_MARKER_JPG8_NOT_SUPPORTED                                       = 51;
    ERR_MARKER_JPG9_NOT_SUPPORTED                                       = 52;
    ERR_MARKER_JPG10_NOT_SUPPORTED                                      = 53;
    ERR_MARKER_JPG11_NOT_SUPPORTED                                      = 54;
    ERR_MARKER_JPG12_NOT_SUPPORTED                                      = 55;
    ERR_MARKER_JPG13_NOT_SUPPORTED                                      = 56;
    ERR_MARKER_TEM_NOT_SUPPORTED                                        = 57;
    ERR_MARKER_RES_NOT_SUPPORTED                                        = 58;
    ERR_INVALID_SOS_MARKER                                              = 59;
    ERR_INVALID_DC_ENTROPY_CODING_DESTINATION_SELECTOR                  = 60;
    ERR_INVALID_AC_ENTROPY_CODING_DESTINATION_SELECTOR                  = 61;
    ERR_INVALID_TABLE_CLASS                                             = 62;
    ERR_INVALID_NUMBER_OF_HUFFMAN_CODES                                 = 63;
    ERR_BAD_HUFFMAN_TABLE                                               = 64;
    ERR_INVALID_HUFFMAN_TABLE_DESTINATION_IDENTIFIER                    = 65;
    ERR_INVALID_COMPONENT_IDENTIFIER                                    = 66;
    ERR_INVALID_SCAN_COMPONENT_SELECTOR                                 = 67;
    ERR_SOI_NOT_DETECTED                                                = 68;
    ERR_INVALID_HUFFMAN_CODE                                            = 69;
    ERR_NON_INTERLEAVED_decODING_NOT_SUPPORTED                          = 70;
    ERR_INTERLEAVED_AC_SCAN                                             = 71;
    ERR_ONE_SAMPLE_PER_LINE_NOT_SUPPORTED                               = 72;
    ERR_CODING                                                          = 73;
    ERR_MARKER_RST0_DETECTED_OUTSIDE_SCAN                               = 74;
    ERR_MARKER_RST1_DETECTED_OUTSIDE_SCAN                               = 75;
    ERR_MARKER_RST2_DETECTED_OUTSIDE_SCAN                               = 76;
    ERR_MARKER_RST3_DETECTED_OUTSIDE_SCAN                               = 77;
    ERR_MARKER_RST4_DETECTED_OUTSIDE_SCAN                               = 78;
    ERR_MARKER_RST5_DETECTED_OUTSIDE_SCAN                               = 79;
    ERR_MARKER_RST6_DETECTED_OUTSIDE_SCAN                               = 80;
    ERR_MARKER_RST7_DETECTED_OUTSIDE_SCAN                               = 81;
    ERR_INVALID_RESTART_MARKER                                          = 82;
    ERR_INVALID_FILL_BYTE                                               = 83;
    ERR_SIXTEEN_BIT_QUANTIZATION_TABLE_ELEMENT_PRECISION_NOT_SUPPORTED  = 84;
    ERR_INSUFFICIENT_MEMORY                                             = 85;
    ERR_QUANTIZATION_TABLE                                              = 86;
    ERR_UNKNOWN                                                         = 87;

    // file markers
    SOF0  = $c0;
    SOF1  = $c1;
    SOF2  = $c2;
    SOF3  = $c3;
    SOF5  = $c5;
    SOF6  = $c6;
    SOF7  = $c7;
    JPG   = $c8;
    SOF9  = $c9;
    SOF10 = $ca;
    SOF11 = $cb;
    SOF13 = $cd;
    SOF14 = $ce;
    SOF15 = $cf;
    DHT   = $c4;
    DAC   = $cc;
    RST0  = $d0;
    RST1  = $d1;
    RST2  = $d2;
    RST3  = $d3;
    RST4  = $d4;
    RST5  = $d5;
    RST6  = $d6;
    RST7  = $d7;
    SOI   = $d8;
    EOI   = $d9;
    SOS   = $da;
    DQT   = $db;
    DNL   = $dc;
    DRI   = $dd;
    DHP   = $de;
    EXP_  = $df;
    APP0  = $e0;
    APP1  = $e1;
    APP2  = $e2;
    APP3  = $e3;
    APP4  = $e4;
    APP5  = $e5;
    APP6  = $e6;
    APP7  = $e7;
    APP8  = $e8;
    APP9  = $e9;
    APP10 = $ea;
    APP11 = $eb;
    APP12 = $ec;
    APP13 = $ed;
    APP14 = $ee;
    APP15 = $ef;
    JPG0  = $f0;
    JPG1  = $f1;
    JPG2  = $f2;
    JPG3  = $f3;
    JPG4  = $f4;
    JPG5  = $f5;
    JPG6  = $f6;
    JPG7  = $f7;
    JPG8  = $f8;
    JPG9  = $f9;
    JPG10 = $fa;
    JPG11 = $fb;
    JPG12 = $fc;
    JPG13 = $fd;
    COM_  = $fe;
    TEM   = $01;

    NEGS : array[0..15]  of Integer =
      {$IFDEF OXYGENE}
      [
           0,   -1,    -3,    -7,   -15,   -31,     -63,   -127, // +1
        -255, -511, -1023, -2047, -4095, -8191,  -15383, -32767
      ] ;
      {$ELSE}
      (
           0,   -1,    -3,    -7,   -15,   -31,     -63,   -127, // +1
        -255, -511, -1023, -2047, -4095, -8191,  -15383, -32767
      );
      {$ENDIF}

    {$IFDEF JAVA}
      {$IFDEF ELEMENTS100UP}
        FF_FLAG = $ff ;
      {$ELSE}
        FF_FLAG = SByte($ff) ;
      {$ENDIF}
    {$ELSE}
      FF_FLAG = $ff ;
    {$ENDIF}
  end ;

//==============================================================================
// TGIS_FileJPEG
//==============================================================================

  procedure TGIS_FileJPEG.prepareCapabilities ;
  begin
    inherited ;

    Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                         TGIS_PixelFormat.RGB,
                         False,
                         TGIS_PixelSubFormat.JPEG,
                         TGIS_CompressionType.JPEG,
                         90
                      )
                    ) ;

    {$IFDEF DCC}
    if not ( Assigned( BitmapHelper ) and BitmapHelper.ClassName.Contains('FMX') )
    then
    {$ENDIF}
    begin
      Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                            TGIS_PixelFormat.Bit8,
                            True,
                            TGIS_PixelSubFormat.JPEG,
                            TGIS_CompressionType.JPEG,
                            90
                         )
                       ) ;
    end;
  end ;


  constructor TGIS_FileJPEG.Create ;
  begin
    inherited ;
  end;

  constructor TGIS_FileJPEG.Create(
    const _path        : String      ;
    const _ext         : TGIS_Extent ;
    const _width       : Integer     ;
    const _height      : Integer     ;
    const _subformat   : TGIS_LayerPixelSubFormat ;
    const _ppi         : Integer     ;
    const _cs          : TGIS_CSCoordinateSystem
  ) ;
  begin
    inherited Create( _path, _ext, _width, _height,
                      _subformat, _ppi, _cs
                    ) ;

    writeMode := True ;
    isReady   := True ;

    FPixelFormat := TGIS_PixelFormat.RGB ;
    writeWorldFile( _ext, WORLD_FILE_EXT_JPG2, _cs ) ;

    bmpObj := TGIS_Bitmap.Create(_width, _height) ;
    pixObj := nil ;
  end ;

  procedure TGIS_FileJPEG.doDestroy ;
  var
    blob : TGIS_MemoryStream ;
    ls   : Int64 ;
    buf  : TBytes ;
  begin
    try
      if writeMode and isReady then begin
       jpgFileStream := TGIS_FileStream.Create( Path, fmCreate ) ;
       blob := TGIS_MemoryStream.Create ;
       bmpObj.UnlockPixels ;
       bmpObj.SaveToStream( blob, FSubFormat.PixelFormat, FSubFormat.Subformat, CompressionLevel ) ;
       blob.Position := 0 ;
       ls := blob.Size ;
       {$IFDEF DCC}
         SetLength( buf, ls ) ;
         Move( blob.Memory^, buf[0], ls ) ;
         jpgFileStream.Write( buf[0], ls) ;
       {$ELSE}
         buf := blob.Memory ;
         jpgFileStream.Write( buf, ls) ;
       {$ENDIF}
      end ;
    finally
      if writeMode and isReady then begin
        FreeObject( blob ) ;
        FreeObject( jpgFileStream ) ;
      end;
      FreeObject( bmpObj ) ;
      inherited ;
    end ;
  end ;

  { TODO -cReview : write for CLR }
  procedure TGIS_FileJPEG.Write( const _x       : Integer ;
                                 const _y       : Integer ;
                                 const _pixels  : TGIS_Pixels ;
                                 const _pformat : TGIS_PixelFormat ;
                                 const _width   : Integer ;
                                 const _height  : Integer
                               ) ;
  var
    w, h : Integer ;
    i : Integer ;
  begin
    if not assigned(pixObj) then begin
      try
        bmpObj.LockPixels(pixObj, True);
      except
        isReady := False ;
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_IMAGETOBIG ), '', 0) ;
      end;
    end;


    if (_x + _width) > Width then
      w := Width -_x
    else
      w := _width ;

    if (_y + _height) > Height then
      h := Height -_y
    else
      h := _height ;


    if (w = Width) and (w = _width) then // solid input and draw area
        GisCopyPixels ( _pixels, 0, pixObj, _y*w, w*h)
    else begin
        for i := 0 to h -1 do
          GisCopyPixels ( _pixels, i*_width, pixObj, (_y +i)*Width +_x, w) ;
    end;
  end ;

//==============================================================================
// T_stdecoder
//==============================================================================

  constructor T_stdecoder.Create ;
  begin
    inherited ;

    {$IFDEF OXYGENE}
      {$IFDEF ISLAND}
        SetLength( arHuffman, 2, 4 ) ;
      {$ELSE}
        arHuffman[0] := new T_stHuffman[4] ;
        arHuffman[1] := new T_stHuffman[4] ;
      {$ENDIF}
    {$ENDIF}
  end ;

//==============================================================================
// T_jpegdecoder
//==============================================================================

  // Create an instance.
  constructor T_jpegdecoder.Create;
  begin
    inherited ;

    stdecoder := T_stdecoder.Create ;

    SetLength( arMcu, 660 ) ;

    SetLength( arQuantization[0], 64 );
    SetLength( arQuantization[1], 64 );
    SetLength( arQuantization[2], 64 );
    SetLength( arQuantization[3], 64 );

    {$IFDEF OXYGENE}
      decodeIdct := @decodeIdct_1 ;
    {$ELSE}
      decodeIdct := decodeIdct_1 ;
    {$ENDIF}

    FdecodeMode     := JPEG_RGB ;
    FComponentsJpeg := JPEG_YBR ;
    FComponentsApp  := -1 ;

    nPart := -1 ;
  end;

  // Destroy an instance.
  procedure T_jpegdecoder.doDestroy;
  var
    i, j : Integer ;
  begin

    arPartsScales :=nil ;
    arImage := nil;
    FreeObject( oStream ) ;

    for i := 1 downto 0 do begin
      for j := 3 downto 0 do begin
        if assigned( T_stHuffman( stdecoder.arHuffman[i][j] )) then begin
          stdecoder.arHuffman[i][j].cSize    := nil ;
          stdecoder.arHuffman[i][j].cVal     := nil ;
          stdecoder.arHuffman[i][j].ucValHi  := nil ;
          stdecoder.arHuffman[i][j].ucSizeHi := nil ;
          stdecoder.arHuffman[i][j].ucValLo  := nil ;
          stdecoder.arHuffman[i][j].ucSizeLo := nil ;
          FreeObject( stdecoder.arHuffman[i][j] ) ;
        end ;
      end ;
    end ;
    FreeObject( stdecoder ) ;

    inherited ;
  end ;

 //  Scale setting
  procedure  T_jpegdecoder.fset_Scale(
    const _sc : Integer
  ) ;
  var
    sc : Integer ;
  begin

    if not assigned(arPartsScales) then
      doAlive ;

    if assigned(arPartsScales) then begin
      if isProgressive then begin
        if arPartsScales[0] = 0 then begin
          FScale := 1 ;
          {$IFDEF OXYGENE}
            decodeIdct := @decodeIdct_1 ;
          {$ELSE}
            decodeIdct :=  decodeIdct_1 ;
          {$ENDIF}
          stdecoder.fnWrite := stdecoder.arFnWrite[0] ;
          exit ;
        end;
      end;
      if _sc > 8 then
        sc := 8
      else begin
        sc := (_sc div 2)*2;
        if sc < 1 then
        sc := 1 ;
      end;
    end
    else
      sc := 1 ;

    case sc of
      8 :
        begin
          FScale := 8 ;
          if (stdecoder.imageFormat = T_JPGCONST.FORMAT_444 ) then
            {$IFDEF OXYGENE}
              decodeIdct := @decodeIdct_8
            {$ELSE}
              decodeIdct :=  decodeIdct_8
            {$ENDIF}
          else
            {$IFDEF OXYGENE}
              decodeIdct := @decodeIdct_4 ;
            {$ELSE}
              decodeIdct :=  decodeIdct_4 ;
            {$ENDIF}
          stdecoder.fnWrite     := stdecoder.arFnWrite[3] ;
        end ;
      4 :
        begin
          FScale := 4 ;
          if (stdecoder.imageFormat = T_JPGCONST.FORMAT_444 ) then
            {$IFDEF OXYGENE}
              decodeIdct := @decodeIdct_4
            {$ELSE}
              decodeIdct :=  decodeIdct_4
            {$ENDIF}
          else
            {$IFDEF OXYGENE}
              decodeIdct := @decodeIdct_2 ;
            {$ELSE}
              decodeIdct :=  decodeIdct_2 ;
            {$ENDIF}
          stdecoder.fnWrite     := stdecoder.arFnWrite[2] ;
        end ;
      2 :
        begin
          FScale := 2 ;
          if (stdecoder.imageFormat = T_JPGCONST.FORMAT_444 ) then
            {$IFDEF OXYGENE}
              decodeIdct := @decodeIdct_2
            {$ELSE}
              decodeIdct :=  decodeIdct_2
            {$ENDIF}
          else
            {$IFDEF OXYGENE}
              decodeIdct := @decodeIdct_1 ;
            {$ELSE}
              decodeIdct :=  decodeIdct_1 ;
            {$ENDIF}
          stdecoder.fnWrite     := stdecoder.arFnWrite[1] ;
        end ;
      else
        begin
          FScale := 1 ;
          {$IFDEF OXYGENE}
            decodeIdct := @decodeIdct_1 ;
          {$ELSE}
            decodeIdct :=  decodeIdct_1 ;
          {$ENDIF}
          stdecoder.fnWrite     := stdecoder.arFnWrite[0] ;
        end ;
    end ;
  end ;

   //  Scale setting
  procedure  T_jpegdecoder.fset_JpegInTiff(
    const _it : Boolean
  ) ;
  begin
    FJpegInTiff := _it ;
  end;


  function  T_jpegdecoder.fget_JpegInitialized
    : Boolean ;
  begin
    Result := False ;
    if assigned(stdecoder) then begin
      if assigned(arOffset)  then begin
        if  arOffset[0] <> 0 then
          Result := True ;
      end;
    end ;
  end;

  // Parse file marker and call respective functions.
  function T_jpegdecoder.decodeImage(
    const _read_SOI : Boolean
  ) : Integer ;
  var
    res        : Integer ;
    nextmarker : Integer ;
  begin
    Result := T_JPGCONST.ERR_UNKNOWN ;

    if _read_SOI then
      readMarkerSOI ;

    while True do begin
      res := readMarkerNext ;
      if res <> T_JPGCONST.SUCCESS then begin
        Result := res ;
        exit ;
      end;

      nextmarker := stdecoder.markerNext;
      case nextmarker of
        T_JPGCONST.SOF0,
        T_JPGCONST.SOF1,
        T_JPGCONST.SOF2   : begin
                   Result := readMarkerSOF ;
                   break ;
                 end;
        T_JPGCONST.SOF3   : begin
                   Result := T_JPGCONST.ERR_MARKER_SOF3_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.SOF5   : begin
                   Result := T_JPGCONST.ERR_MARKER_SOF5_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.SOF6   : begin
                   Result := T_JPGCONST.ERR_MARKER_SOF6_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.SOF7   : begin
                   Result := T_JPGCONST.ERR_MARKER_SOF7_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.JPG    : begin
                   Result := T_JPGCONST.ERR_MARKER_JPG_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.SOF9   : begin
                   Result := T_JPGCONST.ERR_MARKER_SOF9_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.SOF10  : begin
                   Result := T_JPGCONST.ERR_MARKER_SOF10_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.SOF11  : begin
                   Result := T_JPGCONST.ERR_MARKER_SOF11_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.SOF13  : begin
                   Result := T_JPGCONST.ERR_MARKER_SOF13_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.SOF14  : begin
                   Result := T_JPGCONST.ERR_MARKER_SOF14_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.SOF15  : begin
                   Result := T_JPGCONST.ERR_MARKER_SOF15_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.DHT    : begin
                  res := readMarkerDHT ;
                  if res <> T_JPGCONST.SUCCESS then begin
                     Result := res ;
                     break ;
                   end;
                   definedDHT := True ;
                 end;
        T_JPGCONST.DAC    : begin
                   Result := T_JPGCONST.ERR_MARKER_DAC_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.RST0   : begin
                   Result := T_JPGCONST.ERR_MARKER_RST0_DETECTED_OUTSIDE_SCAN ;
                   break ;
                 end ;
        T_JPGCONST.RST1   : begin
                   Result := T_JPGCONST.ERR_MARKER_RST1_DETECTED_OUTSIDE_SCAN ;
                   break ;
                 end ;
        T_JPGCONST.RST2   : begin
                   Result := T_JPGCONST.ERR_MARKER_RST2_DETECTED_OUTSIDE_SCAN ;
                   break ;
                 end ;
        T_JPGCONST.RST3   : begin
                   Result := T_JPGCONST.ERR_MARKER_RST3_DETECTED_OUTSIDE_SCAN ;
                   break ;
                 end ;
        T_JPGCONST.RST4   : begin
                   Result := T_JPGCONST.ERR_MARKER_RST4_DETECTED_OUTSIDE_SCAN ;
                   break ;
                 end ;
        T_JPGCONST.RST5   : begin
                   Result := T_JPGCONST.ERR_MARKER_RST5_DETECTED_OUTSIDE_SCAN ;
                   break ;
                 end ;
        T_JPGCONST.RST6   : begin
                   Result := T_JPGCONST.ERR_MARKER_RST6_DETECTED_OUTSIDE_SCAN ;
                   break ;
                 end ;
        T_JPGCONST.RST7   : begin
                   Result := T_JPGCONST.ERR_MARKER_RST7_DETECTED_OUTSIDE_SCAN ;
                   break ;
                 end ;
        T_JPGCONST.SOI    : begin
                   Result := T_JPGCONST.ERR_MARKER_SOI_ALREADY_DETECTED ;
                   break ;
                 end ;
        T_JPGCONST.EOI    : begin
                   Result := T_JPGCONST.SUCCESS ;
                   break ;
                 end ;
        T_JPGCONST.SOS    : begin
                   if not definedDHT then
                     loadDefaultTables(1.0) ;
                   res := readMarkerSOS ;
                   if ( res <> T_JPGCONST.SUCCESS ) or (not isProgressive) then begin
                     Result := res ;
                     break ;
                   end;
                 end;
        T_JPGCONST.DQT    : begin
                   res := readMarkerDQT ;
                   if res <> T_JPGCONST.SUCCESS then begin
                     Result := res;
                     break ;
                   end;
                 end;
        T_JPGCONST.DNL    : begin
                   Result := T_JPGCONST.ERR_MARKER_DNL_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.DRI    : begin
                   res := readMarkerDRI ;
                   if res <> T_JPGCONST.SUCCESS then begin
                     Result := res ;
                     break ;
                   end;
                 end;
        T_JPGCONST.DHP    : begin
                   Result := T_JPGCONST.ERR_MARKER_DHP_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.EXP_   : begin
                   Result := T_JPGCONST.ERR_MARKER_EXP_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.APP0,
        T_JPGCONST.APP1,
        T_JPGCONST.APP2,
        T_JPGCONST.APP3,
        T_JPGCONST.APP4,
        T_JPGCONST.APP5,
        T_JPGCONST.APP6,
        T_JPGCONST.APP7,
        T_JPGCONST.APP8,
        T_JPGCONST.APP9,
        T_JPGCONST.APP10,
        T_JPGCONST.APP11,
        T_JPGCONST.APP12,
        T_JPGCONST.APP13,
        T_JPGCONST.APP15,
        T_JPGCONST.COM_   : begin
                  res := skipMarker;
                  if res <> T_JPGCONST.SUCCESS then begin
                    Result := res;
                   break ;
                   end;
                 end;
        T_JPGCONST.APP14: begin
                  res := readMarkerAPP;
                  if res <> T_JPGCONST.SUCCESS then begin
                    Result := res;
                   break ;
                   end;
                 end;
        T_JPGCONST.JPG0   : begin
                   Result := T_JPGCONST.ERR_MARKER_JPG0_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.JPG1   : begin
                   Result := T_JPGCONST.ERR_MARKER_JPG1_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.JPG2   : begin
                   Result := T_JPGCONST.ERR_MARKER_JPG2_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.JPG3   : begin
                   Result := T_JPGCONST.ERR_MARKER_JPG3_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.JPG4   : begin
                   Result := T_JPGCONST.ERR_MARKER_JPG4_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.JPG5   : begin
                   Result := T_JPGCONST.ERR_MARKER_JPG5_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.JPG6   : begin
                   Result := T_JPGCONST.ERR_MARKER_JPG6_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.JPG7   : begin
                   Result := T_JPGCONST.ERR_MARKER_JPG7_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.JPG8   : begin
                   Result := T_JPGCONST.ERR_MARKER_JPG8_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.JPG9   : begin
                   Result := T_JPGCONST.ERR_MARKER_JPG9_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.JPG10  : begin
                   Result := T_JPGCONST.ERR_MARKER_JPG10_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.JPG11  : begin
                   Result := T_JPGCONST.ERR_MARKER_JPG11_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.JPG12  : begin
                   Result := T_JPGCONST.ERR_MARKER_JPG12_NOT_SUPPORTED ;
                   break ;
                 end ;
        T_JPGCONST.JPG13  : begin
                   Result := T_JPGCONST.ERR_MARKER_JPG13_NOT_SUPPORTED ;
                   break ;
                 end  ;
        T_JPGCONST.TEM    : begin
                   Result := T_JPGCONST.ERR_MARKER_TEM_NOT_SUPPORTED ;
                   break ;
                 end ;
        else     begin
                   Result := T_JPGCONST.ERR_MARKER_RES_NOT_SUPPORTED ;
                   break ;
                 end ;
      end;
    end ;
  end ;

  // Ignore marker data.
  function T_jpegdecoder.skipMarker
    : Integer ;
  var
    data_length : Word ;
  begin
    data_length := readWord - 2 ;
    oStream.Position := oStream.Position + data_length ;
    Result := T_JPGCONST.SUCCESS;
  end;

  // Read app marker data.
  function T_jpegdecoder.readMarkerAPP
    : Integer ;
  var
    data_length : Word ;
    buf         : TBytes ;
    transform   : Byte ;
  begin
    data_length := readWord - 2 ;
    SetLength( buf, data_length ) ;
    oStream.ReadBuffer( buf, data_length ) ;

    if (data_length >= 12) and
      (buf[0] = $41) and
      (buf[1] = $64) and
      (buf[2] = $6F) and
      (buf[3] = $62) and
      (buf[4] = $65) then
    begin
      // Found Adobe APP14 marker
      transform := buf[11] ;
      case transform of
        0 : FComponentsApp := JPEG_CMYK
      else  FComponentsApp := JPEG_YBR ;
      end
    end ;

    Result := T_JPGCONST.SUCCESS;
  end;

  // Featch next marker identifier.
  function T_jpegdecoder.readMarkerNext
    : Integer ;
  var
    btmp : Byte ;
  begin
    btmp := readByte and $FF;
    inc(rsInc) ;

    if btmp <> T_JPGCONST.FF_FLAG then begin
      Result := T_JPGCONST.ERR_INVALID_FILL_BYTE;
      exit ;
    end;

    while btmp = T_JPGCONST.FF_FLAG do begin
      btmp := readByte ;
      inc(rsInc) ;
    end ;

    stdecoder.markerNext := btmp;

    Result := T_JPGCONST.SUCCESS;
  end;

  // Start of image marker.
  function T_jpegdecoder.readMarkerSOI : Integer ;
  begin
    if readWord <> ( $FF00 or T_JPGCONST.SOI) then
      Result := T_JPGCONST.ERR_SOI_NOT_DETECTED
    else
      Result := T_JPGCONST.SUCCESS;
  end ;

  // Start of frame.
  function T_jpegdecoder.readMarkerSOF
    : Integer ;
  var
    i             : Integer ;
    btmp          : Byte    ;
    marker        : Integer ;
    header_length : Integer ;
    lines_no      : Integer ;
    components_no : Integer ;
    horz_sampling : Integer ;
    vert_sampling : Integer ;
    quant_tab_sel : Byte    ;
    horz_mcus     : Integer ;
    vert_mcus     : Integer ;
  begin
    if stdecoder.bDetectedSOF then begin
      Result := T_JPGCONST.ERR_SOF_ALREADY_DETECTED;
      exit ;
    end;

    stdecoder.bDetectedSOF := True;
    marker := stdecoder.markerNext ;
    stdecoder.markerSOF := marker ;

    if marker <> T_JPGCONST.SOF0 then
      isProgressive := True
    else
      isProgressive := False ;

    header_length := readWord ;
    arImagePrecision := readByte ;
    arImageMaxVal := 0 ;
    arImageMedVal := 128 ;

    if arImagePrecision = 8 then
      arImageMaxVal := Integer($FF)
    else                      ;
    if arImagePrecision = 12 then begin
      arImageMaxVal := Integer($FFF) ;
      arImageMedVal := 2048 ;
    end
    else
    begin
      if ( marker = T_JPGCONST.SOF0 ) and (arImageMaxVal = 0) then begin
        Result := T_JPGCONST.ERR_INVALID_SAMPLE_PRECISION;
        exit ;
      end;
    end ;

    lines_no := readWord ;
    stdecoder.iHeight := lines_no ;

    if lines_no = 0 then begin
      Result := T_JPGCONST.ERR_ZERO_NUMBER_OF_LINES_NOT_SUPPORTED;
      exit ;
    end;

    stdecoder.iWidth := Integer(readWord) ;
    if stdecoder.iWidth = 0 then begin
      Result := T_JPGCONST.ERR_ZERO_SAMPLES_PER_LINE;
      exit ;
    end;

   stdecoder.iWidthR := ((stdecoder.iWidth +7) div 8) * 8 ;

    components_no := readByte ;
    stdecoder.nComponentsFrame := components_no ;

    if ( components_no <> 1 ) and ( components_no <> 3 ) and ( components_no <> 4 )then begin
      Result := T_JPGCONST.ERR_INVALID_NUMBER_OF_IMAGE_COMPONENTS_IN_FRAME ;
      exit ;
    end;

    if header_length <> ( components_no*3 + 8 ) then begin
      Result := T_JPGCONST.ERR_INVALID_FRAME_HEADER_LENGTH ;
      exit ;
    end;

    for i := 0 to components_no - 1 do begin
      stdecoder.arComponent[i] := readByte ;

      btmp := readByte ;
      horz_sampling := btmp shr 4;
      stdecoder.arHorzSampling[i] := horz_sampling ;

      if ( horz_sampling = 0 ) or ( horz_sampling > 4 ) then
      begin
        Result := T_JPGCONST.ERR_INVALID_HORIZONTAL_SAMPLING_FACTOR ;
        exit ;
      end;

      vert_sampling := btmp and $f;
      stdecoder.arVertSampling[i] := vert_sampling ;

      if ( vert_sampling = 0 ) or ( vert_sampling > 4 ) then
      begin
        Result := T_JPGCONST.ERR_INVALID_VERTICAL_SAMPLING_FACTOR ;
        exit ;
      end;

      quant_tab_sel := readByte ;
      stdecoder.nQuantTableSel[i] := quant_tab_sel ;

      if quant_tab_sel > 3 then begin
        Result := T_JPGCONST.ERR_INVALID_QUANTIZATION_TABLE_DESTINATION_SELECTOR ;
        exit ;
      end;
    end ;

    horz_mcus := ( stdecoder.iWidth + 7 ) shr 3 ;
    vert_mcus := ( lines_no   + 7 ) shr 3 ;

    stdecoder.arRows := 8 ;
    stdecoder.arCols := 8 ;

    //set but not read
    stdecoder.iInc1 := 4;
    stdecoder.iInc2 := 4 * ( stdecoder.iWidthR - horz_mcus ) ;

    if components_no = 1 then begin
      if (stdecoder.arHorzSampling[0] = 1) and
         (stdecoder.arVertSampling[0] = 1) then
      begin
        stdecoder.imageFormat    := T_JPGCONST.FORMAT_400  ;
        stdecoder.nMcuSize       := 64          ;
        {$IFDEF OXYGENE}
          stdecoder.fnWrite      := @write400_1 ;
          stdecoder.arFnWrite[0] := @write400_1 ;
          stdecoder.arFnWrite[1] := @write400_2 ;
          stdecoder.arFnWrite[2] := @write400_4 ;
          stdecoder.arFnWrite[3] := @write400_8 ;
        {$ELSE}
          stdecoder.fnWrite      :=  write400_1 ;
          stdecoder.arFnWrite[0] :=  write400_1 ;
          stdecoder.arFnWrite[1] :=  write400_2 ;
          stdecoder.arFnWrite[2] :=  write400_4 ;
          stdecoder.arFnWrite[3] :=  write400_8 ;
        {$ENDIF}
        bytesPerPixel            := 2           ;
      end
      else begin
        Result := T_JPGCONST.ERR_UNSUPPORTED_IMAGE_FORMAT ;
        exit ;
      end;
    end
    else begin
      if ( ( stdecoder.arHorzSampling[1] = 1 ) and
           ( stdecoder.arVertSampling[1] = 1 ) and
           ( stdecoder.arHorzSampling[2] = 1 ) and
           ( stdecoder.arVertSampling[2] = 1 )
         ) then
      begin

        if (stdecoder.arHorzSampling[0] = 1 ) then begin
          if (stdecoder.arVertSampling[0] = 1 ) then begin
            stdecoder.imageFormat    := T_JPGCONST.FORMAT_444  ;
            stdecoder.nMcuSize       := 192         ;
            {$IFDEF OXYGENE}
              stdecoder.fnWrite      := @write444_1 ;
              stdecoder.arFnWrite[0] := @write444_1 ;
              stdecoder.arFnWrite[1] := @write444_2 ;
              stdecoder.arFnWrite[2] := @write444_4 ;
              stdecoder.arFnWrite[3] := @write444_8 ;
            {$ELSE}
              stdecoder.fnWrite      :=  write444_1 ;
              stdecoder.arFnWrite[0] :=  write444_1 ;
              stdecoder.arFnWrite[1] :=  write444_2 ;
              stdecoder.arFnWrite[2] :=  write444_4 ;
              stdecoder.arFnWrite[3] :=  write444_8 ;
            {$ENDIF}
            bytesPerPixel := 3 ;
            if components_no = 4 then begin
              stdecoder.nMcuSize := 256 ;
              if FComponentsApp >= 0 then begin
                FComponentsJpeg := FComponentsApp ;
                bytesPerPixel := 3 ;
              end
              else begin
                if FComponentsJpeg <> JPEG_ARGB then
                  FComponentsJpeg := JPEG_CMYK ;
                bytesPerPixel := 4 //4
              end;
            end ;
          end
          else
          if (stdecoder.arVertSampling[0] = 2 ) then begin
            stdecoder.imageFormat    := T_JPGCONST.FORMAT_422  ;
            stdecoder.nMcuSize       := 256         ;
            {$IFDEF OXYGENE}
              stdecoder.fnWrite      := @write412_1 ;
              stdecoder.arFnWrite[0] := @write412_1 ;
              stdecoder.arFnWrite[1] := @write412_2 ;
              stdecoder.arFnWrite[2] := @write412_4 ;
              stdecoder.arFnWrite[3] := @write412_8 ;
            {$ELSE}
              stdecoder.fnWrite      :=  write412_1 ;
              stdecoder.arFnWrite[0] :=  write412_1 ;
              stdecoder.arFnWrite[1] :=  write412_2 ;
              stdecoder.arFnWrite[2] :=  write412_4 ;
              stdecoder.arFnWrite[3] :=  write412_8 ;
            {$ENDIF}
            bytesPerPixel            := 2           ;
            vert_mcus := ( lines_no + 15 ) shr 4 ;
            stdecoder.arRows := 16 ;

          end
          else begin
            Result := T_JPGCONST.ERR_UNSUPPORTED_IMAGE_FORMAT ;
            exit ;
          end;
        end
        else if stdecoder.arHorzSampling[0] = 2 then
        begin
          horz_mcus           := (stdecoder.iWidth + 15) shr 4;
          //set but not read
          stdecoder.iInc1   := 8 ;
          stdecoder.arCols := 16;

          stdecoder.iWidthR := ((stdecoder.iWidth +15) div 16) * 16 ;

          if stdecoder.arVertSampling[0] = 1 then begin
            stdecoder.imageFormat    := T_JPGCONST.FORMAT_422  ;
            stdecoder.nMcuSize       := 256         ;
            {$IFDEF OXYGENE}
              stdecoder.fnWrite      := @write422_1 ;
              stdecoder.arFnWrite[0] := @write422_1 ;
              stdecoder.arFnWrite[1] := @write422_2 ;
              stdecoder.arFnWrite[2] := @write422_4 ;
              stdecoder.arFnWrite[3] := @write422_8 ;
            {$ELSE}
              stdecoder.fnWrite      :=  write422_1 ;
              stdecoder.arFnWrite[0] :=  write422_1 ;
              stdecoder.arFnWrite[1] :=  write422_2 ;
              stdecoder.arFnWrite[2] :=  write422_4 ;
              stdecoder.arFnWrite[3] :=  write422_8 ;
            {$ENDIF}
            bytesPerPixel            := 2           ;

            stdecoder.iInc2 :=  4 * (stdecoder.iWidthR - 2*horz_mcus );
          end
          else if stdecoder.arVertSampling[0] = 2 then begin
            stdecoder.imageFormat    := T_JPGCONST.FORMAT_411  ;
            stdecoder.nMcuSize       := 384         ;
            {$IFDEF OXYGENE}
              stdecoder.fnWrite      := @write411_1 ;
              stdecoder.arFnWrite[0] := @write411_1 ;
              stdecoder.arFnWrite[1] := @write411_2 ;
              stdecoder.arFnWrite[2] := @write411_4 ;
              stdecoder.arFnWrite[3] := @write411_8 ;
            {$ELSE}
              stdecoder.fnWrite      :=  write411_1 ;
              stdecoder.arFnWrite[0] :=  write411_1 ;
              stdecoder.arFnWrite[1] :=  write411_2 ;
              stdecoder.arFnWrite[2] :=  write411_4 ;
              stdecoder.arFnWrite[3] :=  write411_8 ;
            {$ENDIF}
            bytesPerPixel            := 2           ;

            vert_mcus := ( lines_no + 15 ) shr 4 ;

            stdecoder.iInc2 :=  8 *( stdecoder.iWidthR - horz_mcus);

            stdecoder.arRows := 16 ;
          end
        end
        else begin
          Result := T_JPGCONST.ERR_UNSUPPORTED_IMAGE_FORMAT ;
          exit ;
        end;
      end
      else begin
        Result := T_JPGCONST.ERR_UNSUPPORTED_IMAGE_FORMAT ;
        exit ;
      end;
    end ;

    stdecoder.nHorzMcu := horz_mcus ;
    stdecoder.nVertMcu := vert_mcus ;

    if not isProgressive then begin
      stripHeight := stdecoder.arRows ;
      stripNo     := ( stripHeight + lines_no - 1 ) div stripHeight ;
    end
    else begin
      stripNo     := 1        ;
      stripHeight := lines_no ;
    end ;

    //set but not read
    partsNo     := stripNo  ;

    if ( dormantMode = TGIS_LayerDormantMode.Agressive ) or
       ((Int64(stdecoder.iWidth) * stdecoder.iHeight) > (FMemoryThreshold *1e6)) then
      bufferedPartsNo := 1
    else
      bufferedPartsNo := stripNo ;

    bytesPerLine := stdecoder.iWidthR * bytesPerPixel ;

    arImagePartSize := (((stripHeight +stdecoder.arRows -1) div
                        stdecoder.arRows)*stdecoder.arRows)*
                        (((bytesPerLine + stdecoder.arCols -1) div
                          stdecoder.arCols)*stdecoder.arCols) ;
    arImageSize := bufferedPartsNo*arImagePartSize ;

    SetLength( arImage, arImageSize ) ;
    if not assigned(arImage) then begin
      bufferedPartsNo := 1 ;
      arImageSize := arImagePartSize ;
    end
    else
      arImage := nil ;

    stdecoder.nImageOffset := 0 ;

    if isProgressive then
      SetLength( arCoefficient, stdecoder.nVertMcu*stdecoder.nHorzMcu* stdecoder.nMcuSize ) ;

    SetLength( arOffset, stripNo ) ;
    SetLength( arLdc , stripNo ) ;
    {$IFDEF OXYGENE}
      for i := 0 to stripNo-1 do
        arLdc[i] := new T_arLdc( 4 ) ;
    {$ENDIF}
    SetLength( arCode  , stripNo ) ;
    SetLength( arSize  , stripNo ) ;
    SetLength( arRstPending , stripNo ) ;

    Result := T_JPGCONST.SUCCESS;
  end ;

  // Read Huffman tables.
  function T_jpegdecoder.readMarkerDHT
    : Integer ;
  var
    i, j, k, m  : Integer  ;
    index       : Word     ;
    code        : Word     ;
    bits        : ShortInt ;
    tmp         : Byte     ;
    table_class : Byte     ;
    huff_ident  : Byte     ;
    huff_size   : Integer     ;
    huff_st     : T_stHuffman ;
  begin

    huff_size := Cardinal(Integer(readWord) - 2) ;

    while huff_size > 0 do begin

      tmp := readByte ;
      table_class := Cardinal (Integer(tmp shr 4));

      if table_class > 1 then begin
        Result := T_JPGCONST.ERR_INVALID_TABLE_CLASS;
        exit ;
      end;

      huff_ident := Cardinal(Integer(tmp) and $f);

      if huff_ident > 3 then begin
        Result := T_JPGCONST.ERR_INVALID_HUFFMAN_TABLE_DESTINATION_IDENTIFIER;
        exit ;
      end;

      if assigned( stdecoder.arHuffman[table_class][huff_ident] ) then begin
        huff_st := stdecoder.arHuffman[table_class][huff_ident] ;
      end
      else begin
        huff_st := T_stHuffman.Create ;
        stdecoder.arHuffman[table_class][huff_ident] := huff_st ;
        SetLength( huff_st.ucSizeLo, $1000 ) ;
        SetLength( huff_st.ucValLo , $1000 ) ;
        SetLength( huff_st.ucSizeHi, $1000 ) ;
        SetLength( huff_st.ucValHi , $1000 ) ;
        SetLength( huff_st.cVal    , 500  ) ;
        SetLength( huff_st.cSize   , 17   ) ;
      end ;

      k := 0 ;
      for i := 1 to 16 do begin
        bits := readByte ;
        huff_st.cSize[i] := bits ;
        inc(k, bits) ;
      end ;

      dec( huff_size, 1 + 16 );

      if ( k > 256 ) or ( k > huff_size ) then begin
        Result := T_JPGCONST.ERR_INVALID_NUMBER_OF_HUFFMAN_CODES ;
        exit ;
      end;

      for i := 0 to k -1 do
        huff_st.cVal[i] := readByte ;

      code  := 0 ;
      k     := 0 ;
      index := 0 ;

      for i := 1 to 8 do begin
        bits := huff_st.cSize[i] ;
        for j := 1 to bits do begin
          for m := 0 to Word(1 shl (8 - i) )-1 do begin
            index := ( code shl (8 - i) ) or m ;
            huff_st.ucSizeHi[index] := Word( i );
            huff_st.ucValHi[index] := huff_st.cVal[k];
          end;
          inc( k    ) ;
          inc( code );
        end ;

        code := code shl 1;
      end ;

      for m := index + 1 to 255 do
        huff_st.ucSizeHi[m] := 0 ;

      for i := 9 to 16 do begin
        bits := huff_st.cSize[i] ;
        for j := 1 to bits do begin
          for m := 0 to Word(1 shl (16 - i) )-1 do begin
            index := (( code shl (16 - i) ) or m) and $FFF ;
            huff_st.ucSizeLo[index] := Word( i );
            huff_st.ucValLo[index] := huff_st.cVal[k];
          end;
          inc( k    ) ;
          inc( code );
        end ;

        if i < 16 then
          code := code shl 1;
      end ;

      dec( huff_size, k ) ;

    end ;

    Result := T_JPGCONST.SUCCESS;
  end;

  // Start of scan.
  function T_jpegdecoder.readMarkerSOS
    : Integer ;
  var
    i, j             : Integer ;
    components_frame : Integer ;
    components_scan  : Integer ;
    header_length    : Integer ;
    component_sel    : Byte    ;
    spect_start      : Byte    ;
    spect_end        : Byte    ;
    approx_bit_hi    : Byte    ;
    approx_bit_lo    : Byte    ;
    entropy_dc       : Byte    ;
    entropy_ac       : Byte    ;
    quant_tab_sel    : Byte    ;
    btmp             : Byte    ;
    res              : Integer ;
    marker_sof       : Integer ;
  begin
    if not stdecoder.bDetectedSOF then begin
      Result := T_JPGCONST.ERR_SOF_NOT_DETECTED ;
      exit ;
    end;

    marker_sof := stdecoder.markerSOF ;
    components_frame := stdecoder.nComponentsFrame ;

    header_length := readWord ;

    components_scan := readByte ;
    //set but not read
    stdecoder.nComponentsScan := components_scan ;

    if ( components_scan <> 1               ) and
       ( components_scan <> components_frame) then
    begin
      Result := T_JPGCONST.ERR_INVALID_NUMBER_OF_COMPONENTS_IN_SCAN;
      exit ;
    end;

    if header_length <> ( components_scan*2 + 6 )  then
    begin
      Result := T_JPGCONST.ERR_INVALID_SCAN_HEADER_LENGTH;
      exit ;
    end;

    for i := 0 to components_scan - 1 do begin

      component_sel := readByte ;

      for j := 0 to components_frame - 1 do begin
        if component_sel = stdecoder.arComponent[j] then
          break;

        if j = components_frame - 1 then begin
          Result := T_JPGCONST.ERR_INVALID_SCAN_COMPONENT_SELECTOR ;
          exit ;
        end;
      end ;

      stdecoder.nCurrComponent[i] := j ;

      btmp := readByte ;

      entropy_dc := btmp shr 4;
      stdecoder.nEntropyDestSelDC[i] := entropy_dc ;

      if ( ( marker_sof = T_JPGCONST.SOF0 ) and ( entropy_dc > 1 ) ) or
         ( entropy_dc > 3 )                               then
      begin
        Result := T_JPGCONST.ERR_INVALID_DC_ENTROPY_CODING_DESTINATION_SELECTOR ;
        exit ;
      end;

      entropy_ac := btmp and $f;
      stdecoder.nEntropyDestSelAC[i] := entropy_ac ;

      if ( ( marker_sof = T_JPGCONST.SOF0 ) and ( entropy_ac > 1 ) ) or
         ( entropy_ac > 3 )                               then
      begin
        Result := T_JPGCONST.ERR_INVALID_AC_ENTROPY_CODING_DESTINATION_SELECTOR ;
        exit ;
      end;

      quant_tab_sel := stdecoder.nQuantTableSel[j];
      if not assigned( stdecoder.arQuantization[quant_tab_sel] ) then begin
        Result := T_JPGCONST.ERR_QUANTIZATION_TABLE ;
        exit ;
      end;

    end ;

    spect_start := readByte  ;
    stdecoder.nSpectralSelStart := spect_start ;

    if ( ( marker_sof <> T_JPGCONST.SOF2 ) and ( spect_start <> 0 ) ) or
       ( spect_start > 63 )                                then
    begin
      Result :=  T_JPGCONST.ERR_INVALID_START_OF_SPECTRAL_SELECTION ;
      exit ;
    end;

    spect_end := readByte ;
    stdecoder.nSpectralSelEnd := spect_end ;

    if ( marker_sof <> T_JPGCONST.SOF2 ) and ( spect_end <> 63 ) or
       ( marker_sof =  T_JPGCONST.SOF2 ) and ( spect_start = 0 ) and ( spect_end  <> 0 ) or
       ( spect_start > spect_end )                    or
       ( spect_end   > 63        )                    then
    begin
      Result := T_JPGCONST.ERR_INVALID_END_OF_SPECTRAL_SELECTION;
      exit ;
    end;

    btmp := readByte ;

    approx_bit_hi := btmp shr 4;
    stdecoder.nApproxBitHi := approx_bit_hi ;

    if ( marker_sof <> T_JPGCONST.SOF2 ) and ( approx_bit_hi <> 0 ) or
       ( approx_bit_hi > 13 )                            then
    begin
      Result := T_JPGCONST.ERR_INVALID_SUCCESSIVE_APPROXIMATION_BIT_POSITION_HIGH ;
      exit ;
    end;

    approx_bit_lo := btmp and $f;
    stdecoder.nApproxBitLo :=  approx_bit_lo ;

    if ( marker_sof <> T_JPGCONST.SOF2 ) and ( approx_bit_lo <> 0 ) or
       ( approx_bit_lo > 13 )                            then
    begin
      Result := T_JPGCONST.ERR_INVALID_SUCCESSIVE_APPROXIMATION_BIT_POSITION_LOW ;
      exit ;
    end;

    stdecoder.arLdc[0] := 0 ;
    stdecoder.arLdc[1] := 0 ;
    stdecoder.arLdc[2] := 0 ;
    stdecoder.arLdc[3] := 0 ;
    stdecoder.nRestartNext := 0 ;

    if ( spect_start > 0 ) or ( components_scan <> components_frame ) then begin
      res := decodeNoninterleaved ;
      if res <> T_JPGCONST.SUCCESS then begin
        Result := res ;
        exit ;
      end;

      decodeInterleaved( False );
    end
    else begin
      if isProgressive then
        res := decodeInterleaved( True )
      else
        res := T_JPGCONST.SUCCESS ;
      if res <> T_JPGCONST.SUCCESS then begin
        Result := res ;
        exit ;
      end ;
    end ;

    if isProgressive then
      synchronizeBuffer ;

    Result := T_JPGCONST.SUCCESS ;
  end ;

  // Quantization tables.
  function T_jpegdecoder.readMarkerDQT
    : Integer ;
  var
    i     : Integer ;
    btmp  : Byte    ;
    len   : Integer    ;
    prec  : Byte    ;
    idx   : Byte    ;
  begin
    len := readWord - 2;

    while len >= 1 + 64 do begin
      btmp := readByte ;

      prec := btmp shr 4;

      if prec > 1 then begin
        Result := T_JPGCONST.ERR_INVALID_QUANTIZATION_TABLE_ELEMENT_PRECISION ;
        exit ;
      end;

      idx := btmp and $f;

      if idx > 3 then begin
        Result := T_JPGCONST.ERR_INVALID_QUANTIZATION_TABLE_DESTINATION_IDENTIFIER;
        exit ;
      end;

      stdecoder.arQuantization[idx] := arQuantization[idx];

      for i := 0 to 64-1 do begin
        if prec = 1 then
          arQuantization[idx][i] := readWord
        else
          arQuantization[idx][i] := readByte ;
        if arQuantization[idx][i] = 0 then begin
          Result := T_JPGCONST.ERR_QUANTIZATION_TABLE_ELEMENT_ZERO;
          exit ;
        end;
      end ;

      if prec = 1 then
        dec( len, (1 + 128) )
      else
        dec( len, (1 + 64) );
    end ;

    if len <> 0 then
      Result := T_JPGCONST.ERR_INVALID_QUANTIZATION_TABLE_DEFINITION_LENGTH
    else
      Result := T_JPGCONST.SUCCESS;
  end;

  // Restart interval.
  function T_jpegdecoder.readMarkerDRI
    : Integer ;
  var
    interval : Word ;
  begin
    interval := readWord ;

    if interval <> 4 then begin
      Result := T_JPGCONST.ERR_INVALID_DEFINE_RESTART_INTERVAL_SEGMENT_LENGTH ;
      exit ;
    end;

    stdecoder.nRestartPending  := readWord ;
    stdecoder.nRestartInterval := stdecoder.nRestartPending ;

    Result := T_JPGCONST.SUCCESS ;
  end;

  // Restart marker.
  function T_jpegdecoder.readMarkerRST : Integer ;
  var
    res : Integer ;
  begin
    synchronizeBuffer ;

    res := readMarkerNext ;
    if res <> T_JPGCONST.SUCCESS then begin
      Result := res ;
      exit ;
    end;

    inc( stdecoder.nRestartNext ) ;
    stdecoder.nRestartNext := stdecoder.nRestartNext and 7;

    stdecoder.nRestartPending := stdecoder.nRestartInterval ;

    stdecoder.arLdc[0] := 0;
    stdecoder.arLdc[1] := 0;
    stdecoder.arLdc[2] := 0;
    stdecoder.arLdc[3] := 0;

    Result := T_JPGCONST.SUCCESS;
  end;

  // Read word stored in big endian order.
  function T_jpegdecoder.readWord : Word ;
  begin
    {$IFDEF OXYGENE}
      oStream.ReadWord( Result, 2 );
    {$ELSE}
      oStream.Read( Result, 2 );
    {$ENDIF}
    Result := (Integer(Result) shr 8) or ( Integer(Result and $00ff ) shl 8) ;
  end;

  // Read byte form input stream.
  function T_jpegdecoder.readByte  : Byte ;
  begin
    {$IFDEF OXYGENE}
      oStream.ReadByte( Result ) ;
    {$ELSE}
      oStream.Read( Result, 1 ) ;
    {$ENDIF}
  end;

  // Read word from Huffman table stored in big endian order.
  function T_jpegdecoder.readHuffmanWord
    : Word;
  var
    m_hf_w : Word ;
    m_hf_b : Byte ;
  begin
    {$IFDEF OXYGENE}
      oStream.ReadWord( m_hf_w, 2 );
    {$ELSE}
      oStream.Read( m_hf_w, 2 );
    {$ENDIF}

    Result := (Int64(m_hf_w) shr 8) or ( (Int64(m_hf_w) and $00ff ) shl 8) ;
    if (m_hf_w and ($ff))  = ($ff) then begin
       m_hf_b := readByte ;
      Result := (Result and $FF00) or m_hf_b ;
    end;

    if (Result and ($ff)) = ($ff) then
      m_hf_b := readByte ;
  end;

  // write buffer in 4-0-0 format
  procedure T_jpegdecoder.write400_1(
    const _offset : Integer
  ) ;
  var
    i, j, k, l : Integer ;
    lin        : Integer  ;
  begin
    k := 0 ;
    l := _offset + nPartOffset ;
    for i := 1 to 8 do begin
      lin := l ;
      for j := 1 to 8 do begin
        arImage[l] := (arMcu[k]) ;
        inc( k ) ;
        inc( l ) ;
        arImage[l] := arImageMedVal ;
        inc( l ) ;
      end;
      l := lin +2*stdecoder.iWidthR ;
    end;
  end;

    // write buffer in 4-0-0 format
  procedure T_jpegdecoder.write400_2(
    const _offset : Integer
  ) ;
  var
    i, k, l : Integer ;
    lin        : Integer  ;
  begin
    k := 0 ;
    l := _offset + nPartOffset ;
    for i := 1 to 4 do begin
      lin := l ;

      arImage[l] := (arMcu[k]) ;
      inc( k, 2 ) ;
      inc( l ) ;
      arImage[l] := arImageMedVal ;
      inc( l, 2 ) ;
      arImage[l] := arImageMedVal ;
      inc( l ) ;

      arImage[l] := (arMcu[k]) ;
      inc( k, 2 ) ;
      inc( l ) ;
      arImage[l] := arImageMedVal ;
      inc( l, 2 ) ;
      arImage[l] := arImageMedVal ;
      inc( l ) ;

      arImage[l] := (arMcu[k]) ;
      inc( k, 2 ) ;
      inc( l ) ;
      arImage[l] := arImageMedVal ;
      inc( l, 2 ) ;
      arImage[l] := arImageMedVal ;
      inc( l ) ;

      arImage[l] := (arMcu[k]) ;
      inc( k, 10 ) ;
      inc( l ) ;
      arImage[l] := arImageMedVal ;
      inc( l, 2 ) ;
      arImage[l] := arImageMedVal ;

      l := lin +2*2*stdecoder.iWidthR ;
    end;
  end;
  // write buffer in 4-0-0 format
  procedure T_jpegdecoder.write400_4(
    const _offset : Integer
  ) ;
  var
    k, l : Integer ;
    lin        : Integer  ;
  begin
    k := 0 ;
    l := _offset + nPartOffset ;

    lin := l ;

    arImage[l] := (arMcu[k]) ;
    inc( k, 4 ) ;
    inc( l ) ;
    arImage[l] := arImageMedVal ;
    inc( l, 2 ) ;
    arImage[l] := arImageMedVal ;

    inc( l, 5 ) ;

    arImage[l] := (arMcu[k]) ;
    inc( k, 28 ) ;
    inc( l ) ;
    arImage[l] := arImageMedVal ;
    inc( l, 2 ) ;
    arImage[l] := arImageMedVal ;

    l := lin +4*2*stdecoder.iWidthR ;

    arImage[l] := (arMcu[k]) ;
    inc( k, 4 ) ;
    inc( l ) ;
    arImage[l] := arImageMedVal ;
    inc( l, 2 ) ;
    arImage[l] := arImageMedVal ;

    inc( l, 5 ) ;

    arImage[l] := (arMcu[k]) ;
    inc( l ) ;
    arImage[l] := arImageMedVal ;
    inc( l, 2 ) ;
    arImage[l] := arImageMedVal ;
  end;

  // write buffer in 4-0-0 format
  procedure T_jpegdecoder.write400_8(
    const _offset : Integer
  ) ;
  var
    l : Integer ;
  begin
    l := _offset + nPartOffset ;
    arImage[l] := (arMcu[0]) ;
    inc( l ) ;
    arImage[l] := arImageMedVal ;
    inc( l, 2 ) ;
    arImage[l] := arImageMedVal ;
  end;

  // Write buffer in 4-4-4 format.
  procedure T_jpegdecoder.write444_1(
    const _offset : Integer
  ) ;
  var
    i, j      : Integer  ;
    ptr       : Integer  ;
    ptrin     : Integer  ;
    y, cb, cr,k : Integer  ;
  begin
    y   := 0   ;
    cb  := 64  ;
    cr  := 128 ;
    k   := 192 ;
    ptr := _offset + nPartOffset ;

    for i := 1 to 8 do begin
      ptrin := ptr ;
      for j := 1 to 8 do begin
        arImage[ptr] := (arMcu[y]) ;
        inc( ptr ) ;
        inc( y   ) ;
        arImage[ptr] := (arMcu[cb]) ;
        inc( ptr ) ;
        inc( cb  ) ;
        arImage[ptr] := (arMcu[cr]) ;
        inc( ptr ) ;
        inc( cr  ) ;
        if bytesPerPixel = 4 then begin
          arImage[ptr] := (arMcu[k]) ;
          inc( ptr ) ;
          inc( k  ) ;
        end;
      end;
      ptr := ptrin +bytesPerPixel*stdecoder.iWidthR ;
    end;
  end;

    // Write buffer in 4-4-4 format.
  procedure T_jpegdecoder.write444_2(
    const _offset : Integer
  ) ;
  var
    i         : Integer  ;
    ptr       : Integer  ;
    ptrin     : Integer  ;
    y, cb, cr,k : SmallInt ;
  begin
    y   := 0   ;
    cb  := 64  ;
    cr  := 128 ;
    k   := 192 ;
    ptr := _offset + nPartOffset ;

    for i := 1 to 4 do begin
      ptrin := ptr ;

      arImage[ptr] := (arMcu[y]) ;
      inc( ptr ) ;
      arImage[ptr] := (arMcu[cb]) ;
      inc( ptr ) ;
      arImage[ptr] := (arMcu[cr]) ;
      if bytesPerPixel = 4 then begin
        inc( ptr ) ;
        arImage[ptr] := (arMcu[k]) ;
        inc( ptr, 5 ) ;
      end
      else
        inc( ptr, 4 ) ;
      inc( y, 2 ) ;
      inc( cb , 2 ) ;
      inc( cr, 2  ) ;
      inc( k, 2  ) ;

      arImage[ptr] := (arMcu[y]) ;
      inc( ptr ) ;
      arImage[ptr] := (arMcu[cb]) ;
      inc( ptr ) ;
      arImage[ptr] := (arMcu[cr]) ;
      if bytesPerPixel = 4 then begin
        inc( ptr ) ;
        arImage[ptr] := (arMcu[k]) ;
        inc( ptr, 5 ) ;
      end
      else
        inc( ptr, 4 ) ;
      inc( y, 2 ) ;
      inc( cb , 2 ) ;
      inc( cr, 2  ) ;
      inc( k, 2  ) ;

      arImage[ptr] := (arMcu[y]) ;
      inc( ptr ) ;
      arImage[ptr] := (arMcu[cb]) ;
      inc( ptr ) ;
      arImage[ptr] := (arMcu[cr]) ;
      if bytesPerPixel = 4 then begin
        inc( ptr ) ;
        arImage[ptr] := (arMcu[k]) ;
        inc( ptr, 5 ) ;
      end
      else
        inc( ptr, 4 ) ;
      inc( y, 2 ) ;
      inc( cb , 2 ) ;
      inc( cr, 2  ) ;
      inc( k, 2  ) ;

      arImage[ptr] := (arMcu[y]) ;
      inc( ptr ) ;
      arImage[ptr] := (arMcu[cb]) ;
      inc( ptr ) ;
      arImage[ptr] := (arMcu[cr]) ;
      if bytesPerPixel = 4 then begin
        inc( ptr ) ;
        arImage[ptr] := (arMcu[k]) ;
      end ;

      inc( y, 10 ) ;
      inc( cb , 10 ) ;
      inc( cr, 10  ) ;
      inc( k, 10  ) ;

      ptr := ptrin +2*bytesPerPixel*stdecoder.iWidthR ;
    end;
  end;

  // Write buffer in 4-4-4 format.
  procedure T_jpegdecoder.write444_4(
    const _offset : Integer
  ) ;
  var
    ptr       : Integer  ;
    ptrin     : Integer  ;
    y, cb, cr,k : SmallInt ;
  begin
    ptr := _offset + nPartOffset ;
    y   := 0   ;
    cb  := 64  ;
    cr  := 128 ;
    k   := 192 ;

    ptrin := ptr ;
    arImage[ptr] := (arMcu[y]) ;
    inc( ptr ) ;
    arImage[ptr] := (arMcu[cb]) ;
    inc( ptr ) ;
    arImage[ptr] := (arMcu[cr]) ;
    if bytesPerPixel = 4 then begin
      inc( ptr ) ;
      arImage[ptr] := (arMcu[k]) ;
    end;

    inc( y,  4 ) ;
    inc( cb, 4 ) ;
    inc( cr, 4 ) ;
    inc( k,  4 ) ;
    if bytesPerPixel = 4 then
      inc( ptr, 13 )
    else
      inc( ptr, 10 ) ;

    arImage[ptr] := (arMcu[y]) ;
    inc( ptr ) ;
    arImage[ptr] := (arMcu[cb]) ;
    inc( ptr ) ;
    arImage[ptr] := (arMcu[cr]) ;
    if bytesPerPixel = 4 then begin
      inc( ptr ) ;
      arImage[ptr] := (arMcu[k]) ;
    end ;
    inc( y,  28 ) ;
    inc( cb, 28 ) ;
    inc( cr, 28 ) ;
    inc( k, 28 ) ;
    ptr := ptrin +4*bytesPerPixel*stdecoder.iWidthR ;

    arImage[ptr] := (arMcu[y]) ;
    inc( ptr ) ;
    arImage[ptr] := (arMcu[cb]) ;
    inc( ptr ) ;
    arImage[ptr] := (arMcu[cr]) ;
    if bytesPerPixel = 4 then begin
      inc( ptr ) ;
      arImage[ptr] := (arMcu[k]) ;
    end;

    inc( y,  4 ) ;
    inc( cb, 4 ) ;
    inc( cr, 4 ) ;
    inc( k, 4 ) ;
    if bytesPerPixel = 4 then
      inc( ptr, 13 )
    else
      inc( ptr, 10 ) ;

    arImage[ptr] := (arMcu[y]) ;
    inc( ptr ) ;
    arImage[ptr] := (arMcu[cb]) ;
    inc( ptr ) ;
    arImage[ptr] := (arMcu[cr]) ;
    if bytesPerPixel = 4 then begin
      inc( ptr ) ;
      arImage[ptr] := (arMcu[k]) ;
    end;
  end;

  // Write buffer in 4-4-4 format.
  procedure T_jpegdecoder.write444_8(
    const _offset : Integer
  ) ;
  var
    ptr : Integer  ;
  begin
    ptr := _offset + nPartOffset ;

    arImage[ptr] := (arMcu[0]) ; //y
    inc( ptr ) ;
    arImage[ptr] := (arMcu[64]) ; //cb
    inc( ptr ) ;
    arImage[ptr] := (arMcu[128]) ;//cr
    if bytesPerPixel = 4 then begin
      inc( ptr ) ;
      arImage[ptr] := (arMcu[192]) ;//K
    end ;
  end;

  // Write buffer in 4-1-2 format.
  procedure T_jpegdecoder.write412_1(
    const _offset : Integer
  ) ;
  var
    i, j   : Integer ;
    ptr    : Integer ;
    y : Integer ;
    cb, cr : Integer ;
    ptrin     : Integer  ;
  begin
    y  := 0   ;
    cb  := 128 ;
    cr  := 192 ;
    ptr := _offset + nPartOffset  ;
    ptrin := ptr ;

    for i := 1 to 16 do begin
      for j := 1 to 4 do begin
        arImage[ptr] := (arMcu[y]) ;
        inc( ptr ) ;
        inc( y  ) ;
        arImage[ptr] := (arMcu[cb]) ;
        inc( ptr ) ;
        inc( cb  ) ;

        arImage[ptr] := (arMcu[y]) ;
        inc( ptr ) ;
        inc( y  ) ;
        arImage[ptr] := (arMcu[cr]) ;
        inc( ptr ) ;
        inc( cr ) ;
      end;

      ptr := ptrin +2*stdecoder.iWidthR ;
      ptrin := ptr ;
    end;
  end;

    // Write buffer in 4-1-2 format.
  procedure T_jpegdecoder.write412_2(
    const _offset : Integer
  ) ;
  var
    i, j   : Integer ;
    ptr    : Integer ;
    y : Integer ;
    cb, cr : Integer ;
    ptrin     : Integer  ;
  begin
    y  := 0   ;
    cb  := 128 ;
    cr  := 192 ;
    ptr := _offset + nPartOffset  ;
    ptrin := ptr ;

    for i := 1 to 16 do begin
      for j := 1 to 4 do begin
        arImage[ptr] := (arMcu[y]) ;
        inc( ptr ) ;
        inc( y  ) ;
        arImage[ptr] := (arMcu[cb]) ;
        inc( ptr ) ;
        inc( cb  ) ;

        arImage[ptr] := (arMcu[y]) ;
        inc( ptr ) ;
        inc( y  ) ;
        arImage[ptr] := (arMcu[cr]) ;
        inc( ptr ) ;
        inc( cr ) ;
      end;

      ptr := ptrin +2*stdecoder.iWidthR ;
      ptrin := ptr ;
    end;
  end;

  // Write buffer in 4-1-2 format.
  procedure T_jpegdecoder.write412_4(
    const _offset : Integer
  ) ;
  var
    i, j   : Integer ;
    ptr    : Integer ;
    y : Integer ;
    cb, cr : Integer ;
    ptrin     : Integer  ;
  begin
    y  := 0   ;
    cb  := 128 ;
    cr  := 192 ;
    ptr := _offset + nPartOffset  ;
    ptrin := ptr ;

    for i := 1 to 16 do begin
      for j := 1 to 4 do begin
        arImage[ptr] := (arMcu[y]) ;
        inc( ptr ) ;
        inc( y  ) ;
        arImage[ptr] := (arMcu[cb]) ;
        inc( ptr ) ;
        inc( cb  ) ;

        arImage[ptr] := (arMcu[y]) ;
        inc( ptr ) ;
        inc( y  ) ;
        arImage[ptr] := (arMcu[cr]) ;
        inc( ptr ) ;
        inc( cr ) ;
      end;

      ptr := ptrin +2*stdecoder.iWidthR ;
      ptrin := ptr ;
    end;
  end;

  // Write buffer in 4-1-2 format.
  procedure T_jpegdecoder.write412_8(
    const _offset : Integer
  ) ;
  var
    i, j   : Integer ;
    ptr    : Integer ;
    y : Integer ;
    cb, cr : Integer ;
    ptrin     : Integer  ;
  begin
    y  := 0   ;
    cb  := 128 ;
    cr  := 192 ;
    ptr := _offset + nPartOffset  ;
    ptrin := ptr ;

    for i := 1 to 16 do begin
      for j := 1 to 4 do begin
        arImage[ptr] := (arMcu[y]) ;
        inc( ptr ) ;
        inc( y  ) ;
        arImage[ptr] := (arMcu[cb]) ;
        inc( ptr ) ;
        inc( cb  ) ;

        arImage[ptr] := (arMcu[y]) ;
        inc( ptr ) ;
        inc( y  ) ;
        arImage[ptr] := (arMcu[cr]) ;
        inc( ptr ) ;
        inc( cr ) ;
      end;

      ptr := ptrin +2*stdecoder.iWidthR ;
      ptrin := ptr ;
    end;
  end;

  // Write buffer in 4-2-0 format.
  procedure T_jpegdecoder.write411_1(
    const _offset : Integer
  ) ;
  var
    i, j, k : Integer ;
    y1, y2,
    y3, y4  : Integer ;
    cb, cr  : Integer ;
    kin  : Integer ;
  begin
    k  := 2*_offset + nPartOffset  ;
    y1 := 0   ;
    y2 := 64  ;
    y3 := 128 ;
    y4 := 192 ;
    cb := 256 ;
    cr := 320 ;

    for i := 0 to 3 do begin
      kin := k ;
      for j := 0 to 3 do begin
        arImage[k] := (arMcu[y1]) ;
        inc( k  ) ;
        arImage[k] := (arMcu[cb]) ;
        inc( k  ) ;
        inc( cb ) ;
        inc( y1 ) ;

        arImage[k] := (arMcu[y1]) ;
        inc( k  ) ;
        arImage[k] := (arMcu[cr]) ;
        inc( k  ) ;
        inc( cr ) ;
        inc( y1 ) ;
      end ;

      for j := 0 to 3 do begin
        arImage[k] := (arMcu[y2]) ;
        inc( k  ) ;
        arImage[k] := (arMcu[cb]) ;
        inc( k  ) ;
        inc( cb ) ;
        inc( y2 ) ;

        arImage[k] := (arMcu[y2]) ;
        inc( k  ) ;
        arImage[k] := (arMcu[cr]) ;
        inc( k  ) ;
        inc( cr ) ;
        inc( y2 ) ;
      end ;

      dec( cb, 8 );
      dec( cr, 8 );
      k := kin +2*stdecoder.iWidthR ;
      kin := k ;

      for j := 0 to 3 do begin
        arImage[k] := (arMcu[y1]) ;
        inc( k  ) ;
        arImage[k] := (arMcu[cb]) ;
        inc( k  ) ;
        inc( cb ) ;
        inc( y1 ) ;

        arImage[k] := (arMcu[y1]) ;
        inc( k  ) ;
        arImage[k] := (arMcu[cr]) ;
        inc( k  ) ;
        inc( cr ) ;
        inc( y1 ) ;
      end;

      for j := 0 to 3 do begin
        arImage[k] := (arMcu[y2]) ;
        inc( k  ) ;
        arImage[k] := (arMcu[cb]) ;
        inc( k  ) ;
        inc( cb ) ;
        inc( y2 ) ;

        arImage[k] := (arMcu[y2]) ;
        inc( k  ) ;
        arImage[k] := (arMcu[cr]) ;
        inc( k  ) ;
        inc( cr ) ;
        inc( y2 ) ;
      end ;
      k := kin +2*stdecoder.iWidthR ;
    end ;

    for i := 0 to 3 do begin
      kin := k ;
      for j := 0 to 3 do begin
        arImage[k] := (arMcu[y3]) ;
        inc( k  ) ;
        arImage[k] := (arMcu[cb]) ;
        inc( k  ) ;
        inc( cb ) ;
        inc( y3 ) ;

        arImage[k] := (arMcu[y3]) ;
        inc( k  ) ;
        arImage[k] := (arMcu[cr]) ;
        inc( k  ) ;
        inc( cr ) ;
        inc( y3 ) ;
      end ;

      for j := 0 to 3 do begin
        arImage[k] := (arMcu[y4]) ;
        inc( k  ) ;
        arImage[k] := (arMcu[cb]) ;
        inc( k  ) ;
        inc( cb ) ;
        inc( y4 ) ;

        arImage[k] := (arMcu[y4]) ;
        inc( k  ) ;
        arImage[k] := (arMcu[cr]) ;
        inc( k  ) ;
        inc( cr ) ;
        inc( y4 ) ;
      end ;

      dec( cb, 8 );
      dec( cr, 8 );
      k := kin +2*stdecoder.iWidthR ;
      kin := k ;

      for j := 0 to 3 do begin
        arImage[k] := (arMcu[y3]) ;
        inc( k  ) ;
        arImage[k] := (arMcu[cb]) ;
        inc( k  ) ;
        inc( cb ) ;
        inc( y3 ) ;

        arImage[k] := (arMcu[y3]) ;
        inc( k  ) ;
        arImage[k] := (arMcu[cr]) ;
        inc( k  ) ;
        inc( cr ) ;
        inc( y3 ) ;
      end;

      for j := 0 to 3 do begin
        arImage[k] := (arMcu[y4]) ;
        inc( k  ) ;
        arImage[k] := (arMcu[cb]) ;
        inc( k  ) ;
        inc( cb ) ;
        inc( y4 ) ;

        arImage[k] := (arMcu[y4]) ;
        inc( k  ) ;
        arImage[k] := (arMcu[cr]) ;
        inc( k  ) ;
        inc( cr ) ;
        inc( y4 ) ;
      end ;

      k := kin +2*stdecoder.iWidthR ;
    end ;

  end ;

  procedure T_jpegdecoder.write411_2(
    const _offset : Integer
  ) ;
  var
    i, j, k : Integer ;
    y1, y2,
    y3, y4  : Integer ;
    cb, cr  : Integer ;
    kin  : Integer ;
  begin
    k  := 2*_offset + nPartOffset  ;
    y1 := 0   ;
    y2 := 64  ;
    y3 := 128 ;
    y4 := 192 ;
    cb := 256 ;
    cr := 320 ;

    for i := 0 to 3 do begin
      kin := k ;
      for j := 0 to 3 do begin
        arImage[k] := (arMcu[y1]) ;
        inc( k  ) ;
        arImage[k] := (arMcu[cb]) ;
        inc( k, 2 ) ;
        inc( cb ) ;
        inc( y1, 2 ) ;

        arImage[k] := (arMcu[cr]) ;
        inc( k  ) ;
        inc( cr ) ;
      end ;

      for j := 0 to 3 do begin
        arImage[k] := (arMcu[y2]) ;
        inc( k  ) ;
        arImage[k] := (arMcu[cb]) ;
        inc( k, 2  ) ;
        inc( cb ) ;
        inc( y2, 2 ) ;

        arImage[k] := (arMcu[cr]) ;
        inc( k  ) ;
        inc( cr ) ;
      end ;

      inc(y1, 8) ;
      inc(y2, 8) ;
      k := kin +2*2*stdecoder.iWidthR ;
    end ;

    for i := 0 to 3 do begin
      kin := k ;
      for j := 0 to 3 do begin
        arImage[k] := (arMcu[y3]) ;
        inc( k  ) ;
        arImage[k] := (arMcu[cb]) ;
        inc( k, 2 ) ;
        inc( cb ) ;
        inc( y3, 2 ) ;

        arImage[k] := (arMcu[cr]) ;
        inc( k  ) ;
        inc( cr ) ;
      end ;

      for j := 0 to 3 do begin
        arImage[k] := (arMcu[y4]) ;
        inc( k  ) ;
        arImage[k] := (arMcu[cb]) ;
        inc( k, 2  ) ;
        inc( cb ) ;
        inc( y4, 2 ) ;

        arImage[k] := (arMcu[cr]) ;
        inc( k  ) ;
        inc( cr ) ;
      end ;
      inc(y3, 8) ;
      inc(y4, 8) ;

      k := kin +2*2*stdecoder.iWidthR ;
    end ;

  end ;

  procedure T_jpegdecoder.write411_4(
    const _offset : Integer
  ) ;
  var
    i, k : Integer ;
    y1, y2,
    y3, y4  : Integer ;
    cb, cr  : Integer ;
    kin  : Integer ;
  begin
    k  := 2*_offset + nPartOffset  ;
    y1 := 0   ;
    y2 := 64  ;
    y3 := 128 ;
    y4 := 192 ;
    cb := 256 ;
    cr := 320 ;

    for i := 0 to 1 do begin
      kin := k ;

      arImage[k] := (arMcu[y1]) ;
      inc( k  ) ;
      arImage[k] := (arMcu[cb]) ;
      inc( k, 2 ) ;
      arImage[k] := (arMcu[cr]) ;
      inc( k, 5 ) ;
      inc( y1, 4 ) ;
      inc( cb, 2 ) ;
      inc( cr, 2 ) ;

      arImage[k] := (arMcu[y1]) ;
      inc( k  ) ;
      arImage[k] := (arMcu[cb]) ;
      inc( k, 2 ) ;
      arImage[k] := (arMcu[cr]) ;
      inc( k, 5 ) ;
      inc( cb, 2 ) ;
      inc( cr, 2 ) ;

      arImage[k] := (arMcu[y2]) ;
      inc( k  ) ;
      arImage[k] := (arMcu[cb]) ;
      inc( k, 2 ) ;
      arImage[k] := (arMcu[cr]) ;
      inc( k, 5 ) ;
      inc( y2, 4 ) ;
      inc( cb, 2 ) ;
      inc( cr, 2 ) ;

      arImage[k] := (arMcu[y2]) ;
      inc( k  ) ;
      arImage[k] := (arMcu[cb]) ;
      inc( k, 2 ) ;
      arImage[k] := (arMcu[cr]) ;

      inc(y1, 28) ;
      inc(y2, 28) ;
      inc(cb, 10) ;
      inc(cr, 10) ;

      k := kin +4*2*stdecoder.iWidthR ;
    end ;

    for i := 0 to 1 do begin
      kin := k ;

      arImage[k] := (arMcu[y3]) ;
      inc( k  ) ;
      arImage[k] := (arMcu[cb]) ;
      inc( k, 2 ) ;
      arImage[k] := (arMcu[cr]) ;
      inc( k, 5 ) ;
      inc( y3, 4 ) ;
      inc( cb, 2 ) ;
      inc( cr, 2 ) ;

      arImage[k] := (arMcu[y3]) ;
      inc( k  ) ;
      arImage[k] := (arMcu[cb]) ;
      inc( k, 2 ) ;
      arImage[k] := (arMcu[cr]) ;
      inc( k, 5 ) ;
      inc( cb, 2 ) ;
      inc( cr, 2 ) ;

      arImage[k] := (arMcu[y4]) ;
      inc( k  ) ;
      arImage[k] := (arMcu[cb]) ;
      inc( k, 2 ) ;
      arImage[k] := (arMcu[cr]) ;
      inc( k, 5 ) ;
      inc( y4, 4 ) ;
      inc( cb, 2 ) ;
      inc( cr, 2 ) ;

      arImage[k] := (arMcu[y2]) ;
      inc( k  ) ;
      arImage[k] := (arMcu[cb]) ;
      inc( k, 2 ) ;
      arImage[k] := (arMcu[cr]) ;

      inc(y3, 28) ;
      inc(y4, 28) ;
      inc(cb, 10) ;
      inc(cr, 10) ;

      k := kin +4*2*stdecoder.iWidthR ;
    end ;

  end ;

  procedure T_jpegdecoder.write411_8(
    const _offset : Integer
  ) ;
  var
    k : Integer ;
    cb, cr  : Integer ;
    kin  : Integer ;
  begin
    k  := 2*_offset + nPartOffset  ;
    cb := 256 ;
    cr := 320 ;

    kin := k ;
    arImage[k] := (arMcu[0]) ;
    inc( k  ) ;
    arImage[k] := (arMcu[cb]) ;
    inc( k, 2 ) ;
    arImage[k] := (arMcu[cr]) ;
    inc(cb, 4) ;
    inc(cr, 4) ;
    inc( k, 13 ) ;

    arImage[k] := (arMcu[64]) ;
    inc( k  ) ;
    arImage[k] := (arMcu[cb]) ;
    inc( k, 2 ) ;
    arImage[k] := (arMcu[cr]) ;
    inc(cb, 28) ;
    inc(cr, 28) ;

    k := kin +8*2*stdecoder.iWidthR ;

    arImage[k] := (arMcu[128]) ;
    inc( k  ) ;
    arImage[k] := (arMcu[cb]) ;
    inc( k, 2 ) ;
    arImage[k] := (arMcu[cr]) ;
    inc(cb, 4) ;
    inc(cr, 4) ;
    inc( k, 13 ) ;

    arImage[k] := (arMcu[192]) ;
    inc( k  ) ;
    arImage[k] := (arMcu[cb]) ;
    inc( k, 2 ) ;
    arImage[k] := (arMcu[cr]) ;

  end ;

  // Write buffer in 4-2-2 format.
  procedure T_jpegdecoder.write422_1(
    const _offset : Integer
  ) ;
  var
    i, j   : Integer ;
    ptr    : Integer ;
    y1, y2 : Integer ;
    cb, cr : Integer ;
    ptrin     : Integer  ;
  begin
    y1  := 0   ;
    y2  := 64  ;
    cb  := 128 ;
    cr  := 192 ;
    ptr := 2*_offset + nPartOffset  ;

    for i := 1 to 8 do begin
      ptrin := ptr ;
      for j := 1 to 4 do begin
        arImage[ptr] := (arMcu[y1]) ;
        inc( ptr ) ;
        inc( y1  ) ;
        arImage[ptr] := (arMcu[cb]) ;
        inc( ptr ) ;
        inc( cb  ) ;

        arImage[ptr] := (arMcu[y1]) ;
        inc( ptr ) ;
        inc( y1  ) ;
        arImage[ptr] := (arMcu[cr]) ;
        inc( ptr ) ;
        inc( cr  ) ;
      end;

      for j := 1 to 4 do begin
        arImage[ptr] := (arMcu[y2]) ;
        inc( ptr ) ;
        inc( y2  ) ;
        arImage[ptr] := (arMcu[cb]) ;
        inc( ptr ) ;
        inc( cb  ) ;

        arImage[ptr] := (arMcu[y2]) ;
        inc( ptr ) ;
        inc( y2  ) ;
        arImage[ptr] := (arMcu[cr]) ;
        inc( ptr ) ;
        inc( cr  ) ;
      end ;
      ptr := ptrin +2*stdecoder.iWidthR ;
    end ;
  end ;

  // Write buffer in 4-2-2 format.
  procedure T_jpegdecoder.write422_2(
    const _offset : Integer
  ) ;
  var
    i, j, k : Integer ;
    y1, y2  : Integer ;
    cb, cr  : Integer ;
    kin  : Integer ;
  begin
    k  := 2*_offset + nPartOffset  ;
    y1 := 0   ;
    y2 := 64  ;
    cb  := 128 ;
    cr  := 192 ;

    for i := 0 to 3 do begin
      kin := k ;
      for j := 0 to 3 do begin
        arImage[k] := (arMcu[y1]) ;
        inc( k  ) ;
        arImage[k] := (arMcu[cb]) ;
        inc( k, 2 ) ;
        inc( cb ) ;
        inc( y1, 2 ) ;

        arImage[k] := (arMcu[cr]) ;
        inc( k  ) ;
        inc( cr ) ;
      end ;

      for j := 0 to 3 do begin
        arImage[k] := (arMcu[y2]) ;
        inc( k  ) ;
        arImage[k] := (arMcu[cb]) ;
        inc( k, 2  ) ;
        inc( cb ) ;
        inc( y2, 2 ) ;

        arImage[k] := (arMcu[cr]) ;
        inc( k  ) ;
        inc( cr ) ;
      end ;

      inc(y1, 8) ;
      inc(y2, 8) ;
      k := kin +2*2*stdecoder.iWidthR ;
    end ;
  end ;
  // Write buffer in 4-2-2 format.
  procedure T_jpegdecoder.write422_4(
    const _offset : Integer
  ) ;
  var
    i, j   : Integer ;
    ptr    : Integer ;
    y1, y2 : Integer ;
    cb, cr : Integer ;
    ptrin     : Integer  ;
  begin
    y1  := 0   ;
    y2  := 64  ;
    cb  := 128 ;
    cr  := 192 ;
    ptr := 2*_offset + nPartOffset  ;

    for i := 1 to 8 do begin
      ptrin := ptr ;
      for j := 1 to 4 do begin
        arImage[ptr] := (arMcu[y1]) ;
        inc( ptr ) ;
        inc( y1  ) ;
        arImage[ptr] := (arMcu[cb]) ;
        inc( ptr ) ;
        inc( cb  ) ;

        arImage[ptr] := (arMcu[y1]) ;
        inc( ptr ) ;
        inc( y1  ) ;
        arImage[ptr] := (arMcu[cr]) ;
        inc( ptr ) ;
        inc( cr  ) ;
      end;

      for j := 1 to 4 do begin
        arImage[ptr] := (arMcu[y2]) ;
        inc( ptr ) ;
        inc( y2  ) ;
        arImage[ptr] := (arMcu[cb]) ;
        inc( ptr ) ;
        inc( cb  ) ;

        arImage[ptr] := (arMcu[y2]) ;
        inc( ptr ) ;
        inc( y2  ) ;
        arImage[ptr] := (arMcu[cr]) ;
        inc( ptr ) ;
        inc( cr  ) ;
      end ;
      ptr := ptrin +2*stdecoder.iWidthR ;
    end ;

  end ;
  // Write buffer in 4-2-2 format.
  procedure T_jpegdecoder.write422_8(
    const _offset : Integer
  ) ;
  var
    k    : Integer ;
    cb, cr : Integer ;
  begin
    cb  := 128 ;
    cr  := 192 ;
    k := 2*_offset + nPartOffset  ;

    arImage[k] := (arMcu[0]) ;
    inc( k  ) ;
    arImage[k] := (arMcu[cb]) ;
    inc( k, 2 ) ;
    arImage[k] := (arMcu[cr]) ;
    inc(cb, 4) ;
    inc(cr, 4) ;
    inc( k, 13 ) ;

    arImage[k] := (arMcu[64]) ;
    inc( k  ) ;
    arImage[k] := (arMcu[cb]) ;
    inc( k, 2 ) ;
    arImage[k] := (arMcu[cr]) ;
  end ;

  // Skip actual Huffman sequence. Store DC incrementally.
  procedure T_jpegdecoder.skipHuffman(
    const _index : Integer
  ) ;
  var
    ucode            : Cardinal    ;
    nsize            : Integer     ;
    hwc              : Cardinal    ;
    coefficient      : Integer     ;
    huffval          : ShortInt    ;
    huffsize         : ShortInt    ;
    run_length       : Byte        ;
    size             : Byte        ;
    entropy_dc       : Byte        ;
    entropy_ac       : Byte        ;
    huff_st          : T_stHuffman ;
    bnegs            : Boolean     ;
    dc               : Integer     ;
  begin
    entropy_dc := stdecoder.nEntropyDestSelDC[_index] ;
    entropy_ac := stdecoder.nEntropyDestSelAC[_index] ;

    huff_st := stdecoder.arHuffman[0][entropy_dc] ;

    ucode := stdecoder.uCode ;
    nsize := stdecoder.nSize ;

    if nsize < 16 then begin
      ucode := ucode or Cardinal( readHuffmanWord shl ( 16 - nsize ) ) ;
      inc( nsize, 16 ) ;
    end ;

    hwc := ucode shr (16 +8);
    huffsize := huff_st.ucSizeHi[hwc] ;
    if huffsize > 0 then begin
      huffval  := huff_st.ucValHi[hwc] ;
    end
    else begin
      hwc := (ucode shr 16) and $FFF;
      huffsize := huff_st.ucSizeLo[hwc] ;
      huffval  := huff_st.ucValLo[hwc] ;
    end;

    ucode := ucode shl huffsize ;
    dec( nsize, huffsize ) ;

    if nsize < huffval then begin
      ucode := ucode or Cardinal( readHuffmanWord shl ( 16 - nsize ) ) ;
      inc( nsize, 16 );
    end ;
    bnegs := ( ucode and $80000000 ) = 0 ;

    if huffval = 0 then
      dc := 0
    else
      dc := ucode shr ( 32 - huffval ) ;

    ucode := ucode shl huffval ;
    dec( nsize, huffval ) ;

    if bnegs then  begin
      inc( dc, T_JPGCONST.NEGS[huffval] ) ;
    end ;

    stdecoder.arLdc[_index] := ( dc + stdecoder.arLdc[_index] ) ;

    huff_st := stdecoder.arHuffman[1][entropy_ac] ;

    coefficient := 1 ;
    while coefficient < 64 do begin

      if nsize < 16 then begin
        ucode := ucode or Cardinal( readHuffmanWord shl ( 16 - nsize ) ) ;
        inc( nsize, 16 ) ;
      end ;

      hwc := ucode shr (16 +8);
      huffsize := huff_st.ucSizeHi[hwc] ;
      if huffsize > 0 then begin
        huffval  := huff_st.ucValHi[hwc] ;
      end
      else begin
        hwc := (ucode shr 16) and $FFF;
        huffsize := huff_st.ucSizeLo[hwc] ;
        huffval  := huff_st.ucValLo[hwc] ;
      end;

      ucode := ucode shl huffsize ;
      dec( nsize, huffsize ) ;

      run_length := huffval shr 4 ;
      size := huffval and 15 ;

      if size > 0 then begin

        if run_length > 0 then
          inc( coefficient, run_length ) ;

        if nsize < size then begin
          ucode := ucode or Cardinal( readHuffmanWord shl ( 16 - nsize ) ) ;
          inc( nsize, 16 ) ;
        end ;

        ucode := ucode shl size ;
        dec( nsize, size ) ;

      end
      else begin

        if run_length <> 15 then
          break ;

        inc( coefficient, 15 ) ;

      end ;
      inc( coefficient ) ;

    end ;

    stdecoder.uCode := ucode ;
    stdecoder.nSize := nsize ;

  end ;

  // decode subsequent huffman value.
  function T_jpegdecoder.decodeHuffmanSequential(
    const _offset : Integer ;
    const _index  : Integer
  ) : Integer ;
  var
    ucode            : Cardinal    ;
    nsize            : Integer     ;
    hwc              : Cardinal    ;
    coefficient      : Integer     ;
    huffval          : Byte        ;
    huffsize         : Byte        ;
    run_length       : Byte        ;
    size             : Byte        ;
    entropy_dc       : Byte        ;
    entropy_ac       : Byte        ;
    huff_st          : T_stHuffman ;
    bnegs            : Boolean     ;
    dc               : Integer     ;
    ac               : Integer     ;
  begin
    entropy_dc := stdecoder.nEntropyDestSelDC[_index] ;
    entropy_ac := stdecoder.nEntropyDestSelAC[_index] ;

    huff_st := stdecoder.arHuffman[0][entropy_dc] ;

    ucode := stdecoder.uCode ;
    nsize := stdecoder.nSize ;

    if nsize < 16 then begin
      ucode := ucode or Cardinal( readHuffmanWord shl ( 16 - nsize ) ) ;
      inc( nsize, 16 ) ;
    end ;

    hwc := ucode shr (16 +8);
    huffsize := huff_st.ucSizeHi[hwc] ;
    if huffsize > 0 then begin
      huffval  := huff_st.ucValHi[hwc] ;
    end
    else begin
      hwc := (ucode shr 16) and $FFF;
      huffsize := huff_st.ucSizeLo[hwc] ;
      huffval  := huff_st.ucValLo[hwc] ;
    end;

    ucode := ucode shl huffsize ;
    dec( nsize, huffsize ) ;

    if nsize < huffval then begin
      ucode := ucode or Cardinal( readHuffmanWord shl ( 16 - nsize ) ) ;
      inc( nsize, 16 ) ;
    end ;

    bnegs := ( ucode and $80000000 ) = 0 ;

    if huffval = 0 then
      dc := 0
    else
      dc := ucode shr ( 32 - huffval ) ;

    ucode := ucode shl huffval ;
    dec( nsize, huffval ) ;

    if bnegs then begin
      inc( dc, T_JPGCONST.NEGS[huffval] ) ;
    end ;

    arMcu[_offset] := dc + stdecoder.arLdc[_index] ;
    stdecoder.arLdc[_index] := arMcu[_offset] ;

    huff_st := stdecoder.arHuffman[1][entropy_ac] ;

    coefficient := _offset +1 ;
    while coefficient < _offset +64 do begin

      if nsize < 16 then begin
        ucode := ucode or Cardinal( readHuffmanWord shl ( 16 - nsize ) ) ;
        inc( nsize, 16 ) ;
      end ;

      hwc := ucode shr (16 +8);
      huffsize := huff_st.ucSizeHi[hwc] ;
      if huffsize > 0 then begin
        huffval  := huff_st.ucValHi[hwc] ;
      end
      else begin
        hwc := (ucode shr 16) and $FFF;
        huffsize := huff_st.ucSizeLo[hwc] ;
        huffval  := huff_st.ucValLo[hwc] ;
      end;

      ucode := ucode shl huffsize ;
      dec( nsize, huffsize ) ;

      run_length := huffval shr 4 ;
      size := huffval and 15 ;

      if size > 0 then begin

        if run_length > 0 then
          inc( coefficient, run_length ) ;

        if nsize < size then begin
          ucode := ucode or Cardinal( readHuffmanWord shl ( 16 - nsize ) ) ;
          inc( nsize, 16 ) ;
        end ;

        ac := ucode shr ( 32 - size );

        bnegs := ( ucode and $80000000 ) = 0 ;

        ucode := ucode shl size ;
        dec( nsize, size );

        if bnegs then
          inc( ac, T_JPGCONST.NEGS[size] ) ;

        arMcu[coefficient] := ac ;

      end
      else begin

        if run_length <> 15 then
          break;

        inc( coefficient, 15 ) ;
      end ;

      inc( coefficient ) ;

    end;

    stdecoder.uCode := ucode ;
    stdecoder.nSize := nsize ;

    Result := T_JPGCONST.SUCCESS ;
  end ;

  // decode first DC from Huffman.
  function T_jpegdecoder.decodeHuffmanFirstDC(
    const _offset : Integer ;
    const _index  : Integer
  ) : Integer ;
  var
    ucode            : Cardinal    ;
    nsize            : Integer     ;
    hwc              : Cardinal    ;
    huffval          : Byte        ;
    huffsize         : Byte        ;
    entropy_dc       : Byte        ;
    huff_st          : T_stHuffman ;
    dc               : Integer     ;
    ldc              : Integer     ;
    approx_bit_lo    : Byte        ;
  begin
    ucode := stdecoder.uCode ;
    nsize := stdecoder.nSize ;

    if nsize < 16 then begin
      ucode := ucode or Cardinal(Integer(readHuffmanWord) shl (16- nsize)) ;
      inc( nsize, 16 ) ;
    end ;

    entropy_dc := stdecoder.nEntropyDestSelDC[_index] ;

    huff_st := stdecoder.arHuffman[0][entropy_dc] ;

    hwc := ucode shr (16 +8);
    huffsize := huff_st.ucSizeHi[hwc] ;
    if huffsize > 0 then begin
      huffval  := huff_st.ucValHi[hwc] ;
    end
    else begin
      hwc := Cardinal(Integer(ucode) shr 16) and $FFF;
      huffsize := huff_st.ucSizeLo[hwc] ;
      huffval  := huff_st.ucValLo[hwc] ;
    end;

    ucode := Cardinal(Integer(ucode) shl huffsize) ;
    dec( nsize, huffsize );

    if nsize < huffval then begin
      ucode := ucode or Cardinal( Integer(readHuffmanWord) shl ( 16 - nsize ) ) ;
      inc( nsize, 16 ) ;
    end;

    if huffval = 0 then
      dc := 0
    else
      dc := Cardinal(Int64 (ucode) shr ( 32 - huffval )) ;

    ucode := Cardinal(Int64 (ucode) shl huffval) ;
    dec( nsize, huffval );

    if dc < ( 1 shl ( huffval - 1 ) ) then
      inc( dc, ( - 1 shl huffval ) + 1 ) ;

    ldc := stdecoder.arLdc[_index] ;
    inc( dc, ldc ) ;

    stdecoder.arLdc[_index] := dc ;

    approx_bit_lo := stdecoder.nApproxBitLo ;

    dc := dc shl approx_bit_lo ;

    arMcu[_offset] := dc ;

    stdecoder.uCode := ucode ;
    stdecoder.nSize := nsize ;

    Result := T_JPGCONST.SUCCESS;
  end;

  // decode refine DC from Huffman.
  function T_jpegdecoder.decodeHuffmanRefineDC(
    const _offset : Integer ;
    const _index  : Integer
  ) : Integer ;
  var
    nsize : Integer  ;
    ucode : Cardinal ;
  begin
    ucode := stdecoder.uCode;
    nsize := stdecoder.nSize;

    if nsize = 0 then begin
      ucode := Cardinal( readHuffmanWord shl 16 ) ;
      nsize := 16 ;
    end;

    arMcu[_offset] := arMcu[_offset] or
                      Integer( ( ucode shr 31 ) shl stdecoder.nApproxBitLo ) ;

    ucode := ucode shl 1 ;
    dec( nsize, 1 ) ;

    stdecoder.uCode := ucode ;
    stdecoder.nSize := nsize ;

    Result := T_JPGCONST.SUCCESS ;
  end;

  // decode first AC from Huffman.
  function T_jpegdecoder.decodeHuffmanFirstAC(
    const _offset : Integer ;
    const _index  : Integer
  ) : Integer ;
  var
    ucode            : Cardinal    ;
    nsize            : Integer     ;
    hwc              : Cardinal    ;
    coefficient      : Integer     ;
    huffval          : Byte        ;
    huffsize         : Byte        ;
    run_length       : Byte        ;
    size             : Byte        ;
    entropy_ac       : Byte        ;
    huff_st          : T_stHuffman ;
    ac               : Integer     ;
    spectral_start   : Byte        ;
    spectral_end     : Byte        ;
    approx_bit_lo    : Byte        ;
    eobrun           : Integer     ;
  begin
    eobrun := stdecoder.iEobrun;

    if eobrun > 0 then begin
      dec( eobrun ) ;
    end
    else begin
      entropy_ac := stdecoder.nEntropyDestSelAC[0] ;

      huff_st := stdecoder.arHuffman[1][entropy_ac] ;

      ucode := stdecoder.uCode ;
      nsize := stdecoder.nSize ;

      spectral_start := stdecoder.nSpectralSelStart ;
      spectral_end   := stdecoder.nSpectralSelEnd   ;
      approx_bit_lo  := stdecoder.nApproxBitLo      ;

      coefficient := spectral_start ;
      while coefficient <= spectral_end do begin
        if nsize < 16 then begin
          ucode := ucode or Cardinal( Integer(readHuffmanWord) shl ( 16 - nsize ) ) ;
          inc( nsize, 16 ) ;
        end;

        hwc := ucode shr (16 +8);
        huffsize := huff_st.ucSizeHi[hwc] ;
        if huffsize > 0 then begin
          huffval  := huff_st.ucValHi[hwc] ;
        end
        else begin
          hwc := (ucode shr 16) and $FFF;
          huffsize := huff_st.ucSizeLo[hwc] ;
          huffval  := huff_st.ucValLo[hwc] ;
        end;

        ucode := ucode shl huffsize ;
        dec( nsize, huffsize ) ;

        run_length := huffval shr 4  ;
        size       := huffval and 15 ;

        if size > 0 then begin
          inc( coefficient, run_length ) ;

          if nsize < size then begin
            ucode := ucode or Cardinal( Integer (readHuffmanWord) shl ( 16 - nsize ) ) ;
            inc( nsize, 16 ) ;
          end;

          ac := ucode shr ( 32 - size ) ;

          ucode := ucode shl size ;
          dec( nsize, size ) ;

          if ac < ( 1 shl ( size - 1 ) ) then
            inc( ac, ( - 1 shl size ) + 1 ) ;

          arMcu[_offset +coefficient] := ac shl approx_bit_lo ;
        end
        else begin
          if run_length = 15 then
            inc( coefficient, 15 )
          else begin
            eobrun := 1 shl run_length ;

            if run_length > 0 then begin
              if nsize < run_length then begin
                ucode := ucode or Cardinal( Integer (readHuffmanWord) shl ( 16 - nsize ) ) ;
                inc( nsize, 16 ) ;
              end;

              inc( eobrun, ucode shr ( 32 - run_length ) ) ;

              ucode := ucode shl run_length ;
              dec( nsize, run_length ) ;
            end;

            dec( eobrun ) ;

            break;
          end
        end;
        coefficient := coefficient + 1 ;
      end;

      stdecoder.uCode := ucode ;
      stdecoder.nSize := nsize ;
    end;

    stdecoder.iEobrun := eobrun ;

    Result := T_JPGCONST.SUCCESS ;
  end ;

  // decode refine DC from Huffman.
  function T_jpegdecoder.decodeHuffmanRefineAC(
    const _offset : Integer ;
    const _index  : Integer
  ) : Integer ;
  var
    ucode                                           : Cardinal ;
    nsize                                           : Integer  ;
    hwc                                             : Cardinal ;
    coefficient                                     : Integer  ;
    huffval          : Byte        ;
    huffsize         : Byte        ;
    run_length       : Byte        ;
    size             : Integer     ;
    entropy_ac       : Byte        ;
    huff_st          : T_stHuffman ;
    spectral_start   : Byte        ;
    spectral_end     : Byte        ;
    eobrun           : Integer     ;
    pl               : Integer     ;
    ml               : Integer     ;
  begin
    pl := 1 shl stdecoder.nApproxBitLo ;
    ml := (-1) shl stdecoder.nApproxBitLo ;

    entropy_ac := stdecoder.nEntropyDestSelAC[0];

    huff_st := stdecoder.arHuffman[1][entropy_ac];

    spectral_start := stdecoder.nSpectralSelStart;
    spectral_end   := stdecoder.nSpectralSelEnd;

    eobrun      := stdecoder.iEobrun;
    coefficient := spectral_start ;

    ucode := stdecoder.uCode;
    nsize := stdecoder.nSize;

    if eobrun = 0 then begin

      while coefficient <= spectral_end do begin

        if nsize < 16 then begin

          ucode := ucode or Cardinal( Int64( readHuffmanWord ) shl (16 - nsize));
          inc( nsize, 16 );
        end;

        hwc := ucode shr (16 +8);
        huffsize := huff_st.ucSizeHi[hwc] ;
        if huffsize > 0 then begin
          huffval  := huff_st.ucValHi[hwc] ;
        end
        else begin
          hwc := (ucode shr 16) and $FFF;
          huffsize := huff_st.ucSizeLo[hwc] ;
          huffval  := huff_st.ucValLo[hwc] ;
        end;

        ucode := ucode shl huffsize;
        dec( nsize, huffsize );

        run_length := huffval shr 4;
        size       := huffval and 15;

        if size > 0 then begin

          if (size <> 1) then begin
            Result := T_JPGCONST.ERR_INVALID_HUFFMAN_CODE;
            exit ;
          end;

          if (nsize = 0) then begin

            ucode := Cardinal( readHuffmanWord  shl 16 );
            nsize := 16;
          end;

          if (ucode shr 31) > 0 then
            size := pl
          else
            size := ml;

          ucode := ucode shl 1;
          dec( nsize, 1 );
        end
        else begin

          if (run_length <> 15) then begin

            eobrun := 1 shl run_length;

            if (run_length > 0) then begin

              if (nsize < run_length) then begin

                ucode := ucode or Cardinal( readHuffmanWord shl (16 - nsize) );
                inc( nsize, 16 );
              end;

              inc( eobrun, ucode shr (32 - run_length) );

              ucode := ucode shl run_length;
              dec( nsize, run_length );
            end;

            break;
          end;
        end;

        repeat

          if arMcu[_offset +coefficient] <> 0 then begin

            if nsize = 0 then begin

              ucode := Cardinal( readHuffmanWord shl 16 );
              nsize := 16;
            end;

            if ( ucode shr 31 ) > 0 then begin

              if ((arMcu[_offset +coefficient] and pl) = 0) then begin

                if arMcu[_offset +coefficient] >= 0 then
                  arMcu[_offset +coefficient] := arMcu[_offset +coefficient] + pl
                else
                  arMcu[_offset +coefficient] := arMcu[_offset +coefficient] + ml;
              end;
            end;

            ucode := ucode shl 1;
            dec( nsize, 1 );
          end
          else begin

            if run_length = 0 then
              break;

            dec( run_length );
          end;

          inc( coefficient );
        until not (coefficient <= spectral_end);

        if size > 0 then
          arMcu[_offset +coefficient] := size;

        inc( coefficient );
      end;
    end;

    if (eobrun > 0) then begin

      while coefficient <= spectral_end do begin

        if (arMcu[_offset +coefficient] <> 0) then begin

          if (nsize = 0) then begin

            ucode := Cardinal( readHuffmanWord shl 16 );
            nsize := 16;
          end;

          if (ucode shr 31) > 0 then begin

            if ((arMcu[_offset +coefficient] and pl) = 0) then begin

              if (arMcu[_offset +coefficient] >= 0) then
                arMcu[_offset +coefficient] := arMcu[_offset +coefficient] + pl
              else
                arMcu[_offset +coefficient] := arMcu[_offset +coefficient] + ml;
            end;
          end;

          ucode := ucode shl 1;
          dec( nsize, 1 );
        end;

        inc( coefficient );
      end;

      dec( eobrun );
    end;

    stdecoder.uCode := ucode;
    stdecoder.nSize := nsize;

    stdecoder.iEobrun := eobrun;

    Result := T_JPGCONST.SUCCESS;
  end;

  // Clear MCU table
  procedure T_jpegdecoder.clearMcu(
    const _offset_mcu : Integer ;
    const _size       : Integer
  );
  var
    i : Integer ;
  begin
    for i := _offset_mcu to _offset_mcu +_size-1 do
      arMcu[i] := 0 ;
  end;

  // Copy data form MCU to Coefficient table
  procedure T_jpegdecoder.copyMcuToCoef(
    const _offset_mcu  : Integer ;
    const _offset_coef : Integer ;
    const _size        : Integer
  ) ;
  var
    i, ci : Integer ;
  begin
    ci := _offset_coef ;
    for i := _offset_mcu to _offset_mcu + _size -1 do begin
      arCoefficient[ci] := arMcu[i] ;
      inc( ci ) ;
    end;
  end;

  // Copy data form Coefficient to MCU table
  procedure T_jpegdecoder.copyCoefToMcu(
    const _offset_coef : Integer ;
    const _offset_mcu  : Integer ;
    const _size        : Integer
  ) ;
  var
    i, ci :   Integer ;
  begin
    ci := _offset_coef ;
    for i := _offset_mcu to _offset_mcu + _size  - 1 do begin
      arMcu[i] := arCoefficient[ci];
      inc( ci ) ;
    end;
  end;

  // decode zigzag block
  procedure T_jpegdecoder.decodeZigzag(
    const _offset_mcu   : Integer        ;
    const _quantization : ArrayOfInteger
  ) ;
  begin

    arBlock[00] := arMcu[00 +_offset_mcu]*_quantization[00] ;
    arBlock[01] := arMcu[01 +_offset_mcu]*_quantization[01] ;
    arBlock[08] := arMcu[02 +_offset_mcu]*_quantization[02] ;
    arBlock[16] := arMcu[03 +_offset_mcu]*_quantization[03] ;
    arBlock[09] := arMcu[04 +_offset_mcu]*_quantization[04] ;
    arBlock[02] := arMcu[05 +_offset_mcu]*_quantization[05] ;
    arBlock[03] := arMcu[06 +_offset_mcu]*_quantization[06] ;
    arBlock[10] := arMcu[07 +_offset_mcu]*_quantization[07] ;

    arBlock[17] := arMcu[08 +_offset_mcu]*_quantization[08] ;
    arBlock[24] := arMcu[09 +_offset_mcu]*_quantization[09] ;
    arBlock[32] := arMcu[10 +_offset_mcu]*_quantization[10] ;
    arBlock[25] := arMcu[11 +_offset_mcu]*_quantization[11] ;
    arBlock[18] := arMcu[12 +_offset_mcu]*_quantization[12] ;
    arBlock[11] := arMcu[13 +_offset_mcu]*_quantization[13] ;
    arBlock[04] := arMcu[14 +_offset_mcu]*_quantization[14] ;
    arBlock[05] := arMcu[15 +_offset_mcu]*_quantization[15] ;

    arBlock[12] := arMcu[16 +_offset_mcu]*_quantization[16] ;
    arBlock[19] := arMcu[17 +_offset_mcu]*_quantization[17] ;
    arBlock[26] := arMcu[18 +_offset_mcu]*_quantization[18] ;
    arBlock[33] := arMcu[19 +_offset_mcu]*_quantization[19] ;
    arBlock[40] := arMcu[20 +_offset_mcu]*_quantization[20] ;
    arBlock[48] := arMcu[21 +_offset_mcu]*_quantization[21] ;
    arBlock[41] := arMcu[22 +_offset_mcu]*_quantization[22] ;
    arBlock[34] := arMcu[23 +_offset_mcu]*_quantization[23] ;

    arBlock[27] := arMcu[24 +_offset_mcu]*_quantization[24] ;
    arBlock[20] := arMcu[25 +_offset_mcu]*_quantization[25] ;
    arBlock[13] := arMcu[26 +_offset_mcu]*_quantization[26] ;
    arBlock[06] := arMcu[27 +_offset_mcu]*_quantization[27] ;
    arBlock[07] := arMcu[28 +_offset_mcu]*_quantization[28] ;
    arBlock[14] := arMcu[29 +_offset_mcu]*_quantization[29] ;
    arBlock[21] := arMcu[30 +_offset_mcu]*_quantization[30] ;
    arBlock[28] := arMcu[31 +_offset_mcu]*_quantization[31] ;

    arBlock[35] := arMcu[32 +_offset_mcu]*_quantization[32] ;
    arBlock[42] := arMcu[33 +_offset_mcu]*_quantization[33] ;
    arBlock[49] := arMcu[34 +_offset_mcu]*_quantization[34] ;
    arBlock[56] := arMcu[35 +_offset_mcu]*_quantization[35] ;
    arBlock[57] := arMcu[36 +_offset_mcu]*_quantization[36] ;
    arBlock[50] := arMcu[37 +_offset_mcu]*_quantization[37] ;
    arBlock[43] := arMcu[38 +_offset_mcu]*_quantization[38] ;
    arBlock[36] := arMcu[39 +_offset_mcu]*_quantization[39] ;

    arBlock[29] := arMcu[40 +_offset_mcu]*_quantization[40] ;
    arBlock[22] := arMcu[41 +_offset_mcu]*_quantization[41] ;
    arBlock[15] := arMcu[42 +_offset_mcu]*_quantization[42] ;
    arBlock[23] := arMcu[43 +_offset_mcu]*_quantization[43] ;
    arBlock[30] := arMcu[44 +_offset_mcu]*_quantization[44] ;
    arBlock[37] := arMcu[45 +_offset_mcu]*_quantization[45] ;
    arBlock[44] := arMcu[46 +_offset_mcu]*_quantization[46] ;
    arBlock[51] := arMcu[47 +_offset_mcu]*_quantization[47] ;

    arBlock[58] := arMcu[48 +_offset_mcu]*_quantization[48] ;
    arBlock[59] := arMcu[49 +_offset_mcu]*_quantization[49] ;
    arBlock[52] := arMcu[50 +_offset_mcu]*_quantization[50] ;
    arBlock[45] := arMcu[51 +_offset_mcu]*_quantization[51] ;
    arBlock[38] := arMcu[52 +_offset_mcu]*_quantization[52] ;
    arBlock[31] := arMcu[53 +_offset_mcu]*_quantization[53] ;
    arBlock[39] := arMcu[54 +_offset_mcu]*_quantization[54] ;
    arBlock[46] := arMcu[55 +_offset_mcu]*_quantization[55] ;

    arBlock[53] := arMcu[56 +_offset_mcu]*_quantization[56] ;
    arBlock[60] := arMcu[57 +_offset_mcu]*_quantization[57] ;
    arBlock[61] := arMcu[58 +_offset_mcu]*_quantization[58] ;
    arBlock[54] := arMcu[59 +_offset_mcu]*_quantization[59] ;
    arBlock[47] := arMcu[60 +_offset_mcu]*_quantization[60] ;
    arBlock[55] := arMcu[61 +_offset_mcu]*_quantization[61] ;
    arBlock[62] := arMcu[62 +_offset_mcu]*_quantization[62] ;
    arBlock[63] := arMcu[63 +_offset_mcu]*_quantization[63] ;

  end;

  // decode inverse DCT (Discrete Cosine Transform)
  // with 128 shift normalized to 0..255
  procedure T_jpegdecoder.decodeIdct_1(
    const _offset_mcu : Integer
  ) ;
  const
    ic1   = 251;
    is1   = 50;
    ic3   = 213;
    is3   = 142;
    ir2c6 = 277;
    ir2s6 = 669;
    ir2   = 181;
  var
    i, j, m    : Integer ;
    x0, x1, x2,
    x3, x4, x5,
    x6, x7, x8 : Integer ;
  begin
    j := 0 ;
    for i := 0 to 7 do begin

      x0 := arBlock[j+0] shl 9 ;
      x1 := arBlock[j+1] shl 7 ;
      x2 := arBlock[j+2]       ;
      x3 := arBlock[j+3] * ir2 ;
      x4 := arBlock[j+4] shl 9 ;
      x5 := arBlock[j+5] * ir2 ;
      x6 := arBlock[j+6]       ;
      x7 := arBlock[j+7] shl 7 ;

      if (x0 = 0) and
         (x1 = 0) and
         (x2 = 0) and
         (x3 = 0) and
         (x4 = 0) and
         (x5 = 0) and
         (x6 = 0) and
         (x7 = 0) then
      begin
        inc( j, 8 );
        continue ;
      end ;

      x8 := x7 + x1;
      dec( x1, x7 );

      x7 := x0 + x4;
      dec( x0, x4 );
      x4 := x1 + x5;
      dec( x1, x5 );
      x5 := x3 + x8;
      dec( x8, x3 );
      x3 := ir2c6 * (x2 + x6);
      x6 := x3 + (- ir2c6 - ir2s6) * x6;
      x2 := x3 + (- ir2c6 + ir2s6) * x2;

      x3 := x7 + x2;
      dec( x7, x2 );
      x2 := x0 + x6;
      dec( x0, x6 );

      x6 := ic3 * (x4 + x5);
      x5 := (x6 + (- ic3 - is3) * x5) div 64 ;
      x4 := (x6 + (- ic3 + is3) * x4) div 64 ;
      x6 := ic1 * (x1 + x8);
      x1 := ( x6 + (- ic1 - is1) * x1) div 64 ;
      x8 := ( x6 + (- ic1 + is1) * x8) div 64 ;

      inc( x7, 512 );
      inc( x2, 512 );
      inc( x0, 512 );
      inc( x3, 512 );

      arBlock[j+0] := (x3 + x4) div 1024;
      arBlock[j+1] := (x2 + x8) div 1024;
      arBlock[j+2] := (x0 + x1) div 1024;
      arBlock[j+3] := (x7 + x5) div 1024;
      arBlock[j+4] := (x7 - x5) div 1024;
      arBlock[j+5] := (x0 - x1) div 1024;
      arBlock[j+6] := (x2 - x8) div 1024;
      arBlock[j+7] := (x3 - x4) div 1024;
      inc( j, 8 );
    end ;

    j := 0 ;
    m := _offset_mcu ;

    for i := 0 to 7 do begin

      x0 := arBlock[j+ 0] shl 9 ;
      x1 := arBlock[j+ 8] shl 7 ;
      x2 := arBlock[j+16]       ;
      x3 := arBlock[j+24] * ir2 ;
      x4 := arBlock[j+32] shl 9 ;
      x5 := arBlock[j+40] * ir2 ;
      x6 := arBlock[j+48]       ;
      x7 := arBlock[j+56] shl 7 ;
     if (x0 = 0) and
         (x1 = 0) and
         (x2 = 0) and
         (x3 = 0) and
         (x4 = 0) and
         (x5 = 0) and
         (x6 = 0) and
         (x7 = 0) then
      begin
        arMcu[m+0]  := arImageMedVal ;
        arMcu[m+8]  := arImageMedVal ;
        arMcu[m+16] := arImageMedVal ;
        arMcu[m+24] := arImageMedVal ;
        arMcu[m+32] := arImageMedVal ;
        arMcu[m+40] := arImageMedVal ;
        arMcu[m+48] := arImageMedVal ;
        arMcu[m+56] := arImageMedVal ;

        inc( j ) ;
        inc( m ) ;
        continue ;
      end ;

      x8 := x7 + x1;
      dec( x1, x7 );

      x7 := x0 + x4;
      dec( x0, x4 );
      x4 := x1 + x5;
      dec( x1, x5 );
      x5 := x3 + x8;
      dec( x8, x3 );
      x3 := ir2c6 * (x2 + x6);
      x6 := x3 + (- ir2c6 - ir2s6) * x6;
      x2 := x3 + (- ir2c6 + ir2s6) * x2;

      x3 := x7 + x2;
      dec( x7, x2 );
      x2 := x0 + x6;
      dec( x0, x6 );

      x4 :=  x4 div 64 ;
      x5 :=  x5 div 64 ;
      x1 := x1 div 64 ;
      x8 := x8 div 64 ;

      x6 := ic3 * (x4 + x5);

      x5 := (x6 + (- ic3 - is3) * x5);
      x4 := (x6 + (- ic3 + is3) * x4);

      x6 := ic1 * (x1 + x8);

      x1 := (x6 + (- ic1 - is1) * x1);
      x8 := (x6 + (- ic1 + is1) * x8);

      inc( x7, 1024 );
      inc( x2, 1024 );
      inc( x0, 1024 );
      inc( x3, 1024 );

      arMcu[m+0] := ((x3 + x4) div 2048) + arImageMedVal ;
      if      arMcu[m+0] < 0   then  arMcu[m+0] := 0
      else
      if arMcu[m+0] > arImageMaxVal then
        arMcu[m+0] := arImageMaxVal ;

      arMcu[m+8] := ((x2 + x8) div 2048) + arImageMedVal ;
      if      arMcu[m+8] < 0   then arMcu[m+8] := 0
      else
      if arMcu[m+8] > arImageMaxVal then
        arMcu[m+8] := arImageMaxVal ;

      arMcu[m+16] := ((x0 + x1) div 2048) + arImageMedVal ;
      if      arMcu[m+16] < 0   then arMcu[m+16] := 0
      else
      if arMcu[m+16] >
        arImageMaxVal then arMcu[m+16] := arImageMaxVal ;

      arMcu[m+24] := ((x7 + x5) div 2048) + arImageMedVal ;
      if      arMcu[m+24] < 0   then arMcu[m+24] := 0
      else
      if arMcu[m+24] > arImageMaxVal then
        arMcu[m+24] := arImageMaxVal ;

      arMcu[m+32] := ((x7 - x5) div 2048) + arImageMedVal ;
      if      arMcu[m+32] < 0   then arMcu[m+32] := 0
      else
      if arMcu[m+32] > arImageMaxVal then
        arMcu[m+32] := arImageMaxVal ;

      arMcu[m+40] := ((x0 - x1) div 2048) + arImageMedVal ;
      if      arMcu[m+40] < 0   then arMcu[m+40] := 0
      else
      if arMcu[m+40] > arImageMaxVal then
        arMcu[m+40] := arImageMaxVal ;

      arMcu[m+48] := ((x2 - x8) div 2048) + arImageMedVal ;
      if      arMcu[m+48] < 0   then arMcu[m+48] := 0
      else
      if arMcu[m+48] > arImageMaxVal then
        arMcu[m+48] := arImageMaxVal ;

      arMcu[m+56] := ((x3 - x4) div 2048) + arImageMedVal ;
      if      arMcu[m+56] < 0   then arMcu[m+56] := 0
      else
      if arMcu[m+56] > arImageMaxVal then
        arMcu[m+56] := arImageMaxVal ;

      inc( j ) ;
      inc( m ) ;
    end ;

  end ;

  // decode inverse DCT (Discrete Cosine Transform)
  // with 128 shift normalized to 0..255
  procedure T_jpegdecoder.decodeIdct_2(
    const _offset_mcu : Integer
  ) ;
  const
    ic1   = 251;
    is1   = 50;
    ic3   = 213;
    is3   = 142;
    ir2c6 = 277;
    ir2s6 = 669;
    ir2   = 181;
  var
    i, j, m    : Integer ;
    x0, x1, x2,
    x3, x4, x5,
    x6, x7, x8 : Integer ;
  begin

    j := 0 ;
    for i := 0 to 7 do begin

      x0 := arBlock[j+0] shl 9 ;
      x1 := arBlock[j+1] shl 7 ;
      x2 := arBlock[j+2]       ;
      x3 := arBlock[j+3] * ir2 ;
      x4 := arBlock[j+4] shl 9 ;
      x5 := arBlock[j+5] * ir2 ;
      x6 := arBlock[j+6]       ;
      x7 := arBlock[j+7] shl 7 ;

      if (x0 = 0) and
         (x1 = 0) and
         (x2 = 0) and
         (x3 = 0) and
         (x4 = 0) and
         (x5 = 0) and
         (x6 = 0) and
         (x7 = 0) then
      begin
        inc( j, 8 );
        continue ;
      end ;

      x8 := x7 + x1;
      dec( x1, x7 );

      x7 := x0 + x4;
      dec( x0, x4 );
      x4 := x1 + x5;
      dec( x1, x5 );
      x5 := x3 + x8;
      dec( x8, x3 );
      x3 := ir2c6 * (x2 + x6);
      x6 := x3 + (- ir2c6 - ir2s6) * x6;
      x2 := x3 + (- ir2c6 + ir2s6) * x2;

      x3 := x7 + x2;
      dec( x7, x2 );
      x2 := x0 + x6;
      dec( x0, x6 );

      x6 := ic3 * (x4 + x5);
      x5 := (x6 + (- ic3 - is3) * x5) div 64 ;
      x4 := (x6 + (- ic3 + is3) * x4) div 64 ;
      x6 := ic1 * (x1 + x8);
      x1 := ( x6 + (- ic1 - is1) * x1) div 64 ;
      x8 := ( x6 + (- ic1 + is1) * x8) div 64 ;

      inc( x7, 512 );
      inc( x2, 512 );
      inc( x0, 512 );
      inc( x3, 512 );

      arBlock[j+0] := (x3 + x4) div 1024;
      arBlock[j+2] := (x0 + x1) div 1024;
      arBlock[j+4] := (x7 - x5) div 1024;
      arBlock[j+6] := (x2 - x8) div 1024;
      inc( j, 8 );
    end ;

    j := 0 ;
    m := _offset_mcu ;

    for i := 0 to 3 do begin

      x0 := arBlock[j+ 0] shl 9 ;
      x1 := arBlock[j+ 8] shl 7 ;
      x2 := arBlock[j+16]       ;
      x3 := arBlock[j+24] * ir2 ;
      x4 := arBlock[j+32] shl 9 ;
      x5 := arBlock[j+40] * ir2 ;
      x6 := arBlock[j+48]       ;
      x7 := arBlock[j+56] shl 7 ;

     if (x0 = 0) and
         (x1 = 0) and
         (x2 = 0) and
         (x3 = 0) and
         (x4 = 0) and
         (x5 = 0) and
         (x6 = 0) and
         (x7 = 0) then
      begin
        arMcu[m+0]  := arImageMedVal ;
        arMcu[m+16] := arImageMedVal ;
        arMcu[m+32] := arImageMedVal ;
        arMcu[m+48] := arImageMedVal ;

        arMcu[m+1]  := arImageMedVal ;
        arMcu[m+17] := arImageMedVal ;
        arMcu[m+33] := arImageMedVal ;
        arMcu[m+49] := arImageMedVal ;

        arMcu[m+8]  := arImageMedVal ;
        arMcu[m+24] := arImageMedVal ;
        arMcu[m+40] := arImageMedVal ;
        arMcu[m+56] := arImageMedVal ;

        arMcu[m+9]  := arImageMedVal ;
        arMcu[m+25] := arImageMedVal ;
        arMcu[m+41] := arImageMedVal ;
        arMcu[m+57] := arImageMedVal ;

        inc( j ,2) ;
        inc( m ,2) ;
        continue ;
      end ;

      x8 := x7 + x1;
      dec( x1, x7 );

      x7 := x0 + x4;
      dec( x0, x4 );
      x4 := x1 + x5;
      dec( x1, x5 );
      x5 := x3 + x8;
      dec( x8, x3 );
      x3 := ir2c6 * (x2 + x6);
      x6 := x3 + (- ir2c6 - ir2s6) * x6;
      x2 := x3 + (- ir2c6 + ir2s6) * x2;

      x3 := x7 + x2;
      dec( x7, x2 );
      x2 := x0 + x6;
      dec( x0, x6 );

      x4 :=  x4 div 64 ;
      x5 :=  x5 div 64 ;
      x1 := x1 div 64 ;
      x8 := x8 div 64 ;

      x6 := ic3 * (x4 + x5);

      x5 := (x6 + (- ic3 - is3) * x5);
      x4 := (x6 + (- ic3 + is3) * x4);

      x6 := ic1 * (x1 + x8);

      x1 := (x6 + (- ic1 - is1) * x1);
      x8 := (x6 + (- ic1 + is1) * x8);

      inc( x7, 1024 );
      inc( x2, 1024 );
      inc( x0, 1024 );
      inc( x3, 1024 );

      arMcu[m+0] := ((x3 + x4) div 2048) + arImageMedVal ;
      if      arMcu[m+0] < 0   then  arMcu[m+0] := 0
      else if arMcu[m+0] > arImageMaxVal then  arMcu[m+0] := arImageMaxVal ;

      arMcu[m+16] := ((x0 + x1) div 2048) + arImageMedVal ;
      if      arMcu[m+16] < 0   then arMcu[m+16] := 0
      else if arMcu[m+16] > arImageMaxVal then arMcu[m+16] := arImageMaxVal ;

      arMcu[m+32] := ((x7 - x5) div 2048) + arImageMedVal ;
      if      arMcu[m+32] < 0   then arMcu[m+32] := 0
      else if arMcu[m+32] > arImageMaxVal then arMcu[m+32] := arImageMaxVal ;

      arMcu[m+48] := ((x2 - x8) div 2048) + arImageMedVal ;
      if      arMcu[m+48] < 0   then arMcu[m+48] := 0
      else if arMcu[m+48] > arImageMaxVal then arMcu[m+48] := arImageMaxVal ;

      inc( j, 2 ) ;
      inc( m, 2 ) ;
    end ;

  end ;

  // decode inverse DCT (Discrete Cosine Transform)
  // with 128 shift normalized to 0..255
  procedure T_jpegdecoder.decodeIdct_4(
    const _offset_mcu : Integer
  ) ;
  const
    ic1   = 251;
    is1   = 50;
    ic3   = 213;
    is3   = 142;
    ir2c6 = 277;
    ir2s6 = 669;
    ir2   = 181;
  var
    i, j, m    : Integer ;
    x0, x1, x2,
    x3, x4, x5,
    x6, x7, x8 : Integer ;
  begin

    j := 0 ;
    for i := 0 to 7 do begin

      x0 := arBlock[j+0] shl 9 ;
      x1 := arBlock[j+1] shl 7 ;
      x2 := arBlock[j+2]       ;
      x3 := arBlock[j+3] * ir2 ;
      x4 := arBlock[j+4] shl 9 ;
      x5 := arBlock[j+5] * ir2 ;
      x6 := arBlock[j+6]       ;
      x7 := arBlock[j+7] shl 7 ;

      if (x0 = 0) and
         (x1 = 0) and
         (x2 = 0) and
         (x3 = 0) and
         (x4 = 0) and
         (x5 = 0) and
         (x6 = 0) and
         (x7 = 0) then
      begin
        inc( j, 8 );
        continue ;
      end ;

      x8 := x7 + x1;
      dec( x1, x7 );

      x7 := x0 + x4;
      x4 := x1 + x5;
      x5 := x3 + x8;
      x3 := ir2c6 * (x2 + x6);
      x2 := x3 + (- ir2c6 + ir2s6) * x2;

      x3 := x7 + x2;
      dec( x7, x2 );

      x6 := ic3 * (x4 + x5);
      x5 := (x6 + (- ic3 - is3) * x5) div 64 ;
      x4 := (x6 + (- ic3 + is3) * x4) div 64 ;

      inc( x7, 512 );
      inc( x3, 512 );

      arBlock[j+0] := (x3 + x4) div 1024;
      arBlock[j+4] := (x7 - x5) div 1024;
      inc( j, 8 );
    end ;

    j := 0 ;
    m := _offset_mcu ;

    for i := 0 to 1 do begin

      x0 := arBlock[j+ 0] shl 9 ;
      x1 := arBlock[j+ 8] shl 7 ;
      x2 := arBlock[j+16]       ;
      x3 := arBlock[j+24] * ir2 ;
      x4 := arBlock[j+32] shl 9 ;
      x5 := arBlock[j+40] * ir2 ;
      x6 := arBlock[j+48]       ;
      x7 := arBlock[j+56] shl 7 ;
      x8 := x7 + x1;
      dec( x1, x7 );

      x7 := x0 + x4;
      x4 := x1 + x5;
      x5 := x3 + x8;
      x3 := ir2c6 * (x2 + x6);
      x2 := x3 + (- ir2c6 + ir2s6) * x2;

      x3 := x7 + x2;
      dec( x7, x2 );

      x4 :=  x4 div 64 ;
      x5 :=  x5 div 64 ;

      x6 := ic3 * (x4 + x5);

      x5 := (x6 + (- ic3 - is3) * x5);
      x4 := (x6 + (- ic3 + is3) * x4);

      inc( x7, 1024 );
      inc( x3, 1024 );

      arMcu[m+0] := ((x3 + x4) div 2048) + arImageMedVal ;
      if      arMcu[m+0] < 0   then  arMcu[m+0] := 0
      else if arMcu[m+0] > arImageMaxVal then  arMcu[m+0] := arImageMaxVal ;

      arMcu[m+32] := ((x7 - x5) div 2048) + arImageMedVal ;
      if      arMcu[m+32] < 0   then arMcu[m+32] := 0
      else if arMcu[m+32] > arImageMaxVal then arMcu[m+32] := arImageMaxVal ;
      inc( j, 4 ) ;
      inc( m, 4 ) ;
    end ;

  end ;

  // decode inverse DCT (Discrete Cosine Transform)
  // with 128 shift normalized to 0..255
  procedure T_jpegdecoder.decodeIdct_8(
    const _offset_mcu : Integer
  ) ;
  const
    ic1   = 251;
    is1   = 50;
    ic3   = 213;
    is3   = 142;
    ir2c6 = 277;
    ir2s6 = 669;
    ir2   = 181;
  var
    i, j, m    : Integer ;
    x0, x1, x2,
    x3, x4, x5,
    x6, x7, x8 : Integer ;
  begin

    j := 0 ;
    for i := 0 to 7 do begin

      x0 := arBlock[j+0] shl 9 ;
      x1 := arBlock[j+1] shl 7 ;
      x2 := arBlock[j+2]       ;
      x3 := arBlock[j+3] * ir2 ;
      x4 := arBlock[j+4] shl 9 ;
      x5 := arBlock[j+5] * ir2 ;
      x6 := arBlock[j+6]       ;
      x7 := arBlock[j+7] shl 7 ;

      if (x0 = 0) and
         (x1 = 0) and
         (x2 = 0) and
         (x3 = 0) and
         (x4 = 0) and
         (x5 = 0) and
         (x6 = 0) and
         (x7 = 0) then
      begin
        inc( j, 8 );
        continue ;
      end ;

      x8 := x7 + x1;
      dec( x1, x7 );

      x7 := x0 + x4;
      x4 := x1 + x5;
      x5 := x3 + x8;
      x3 := ir2c6 * (x2 + x6);
      x2 := x3 + (- ir2c6 + ir2s6) * x2;

      x3 := x7 + x2;

      x6 := ic3 * (x4 + x5);
      x4 := (x6 + (- ic3 + is3) * x4) div 64 ;

      inc( x3, 512 );

      arBlock[j+0] := (x3 + x4) div 1024;
      inc( j, 8 );
    end ;

    j := 0 ;
    m := _offset_mcu ;

    x0 := arBlock[j+ 0] shl 9 ;
    x1 := arBlock[j+ 8] shl 7 ;
    x2 := arBlock[j+16]       ;
    x3 := arBlock[j+24] * ir2 ;
    x4 := arBlock[j+32] shl 9 ;
    x5 := arBlock[j+40] * ir2 ;
    x6 := arBlock[j+48]       ;
    x7 := arBlock[j+56] shl 7 ;

    x8 := x7 + x1;
    dec( x1, x7 );

    x7 := x0 + x4;
    x4 := x1 + x5;
    x5 := x3 + x8;
    x3 := ir2c6 * (x2 + x6);
    x2 := x3 + (- ir2c6 + ir2s6) * x2;

    x3 := x7 + x2;

    x4 :=  x4 div 64 ;
    x5 :=  x5 div 64 ;

    x6 := ic3 * (x4 + x5);

    x4 := (x6 + (- ic3 + is3) * x4);

    inc( x3, 1024 );

    arMcu[m] := ((x3 + x4) div 2048) + arImageMedVal ;
    if      arMcu[m] < 0   then  arMcu[m] := 0
    else if arMcu[m] > arImageMaxVal then  arMcu[m] := arImageMaxVal ;
  end ;

  // decode non interleaved frame
  function T_jpegdecoder.decodeNoninterleaved
    : Integer ;
  var
    i, j             : Integer ;
    components_frame : Integer ;
    approx_bit_hi    : Byte    ;
    marker_sof       : Integer ;
    offset_coef      : Integer ;
    offset_mcu       : Integer ;
    size_mcu         : Integer ;
    offset           : Integer ;
    lines            : Integer ;
    samples          : Integer ;
    vert_blocks      : Integer ;
    horz_blocks      : Integer ;
    horz_mcus        : Integer ;
    image_format     : Integer ;
    fn_decode         : T_fndecodeHuffman ;
    res              : Integer ;
  begin
    marker_sof    := stdecoder.markerSOF    ;
    approx_bit_hi := stdecoder.nApproxBitHi ;

    if marker_sof <> T_JPGCONST.SOF2 then
      {$IFDEF OXYGENE}
        fn_decode := @decodeHuffmanSequential
      {$ELSE}
        fn_decode :=  decodeHuffmanSequential
      {$ENDIF}
    else begin
      if approx_bit_hi = 0 then
        {$IFDEF OXYGENE}
          fn_decode := @decodeHuffmanFirstAC
        {$ELSE}
          fn_decode :=  decodeHuffmanFirstAC
        {$ENDIF}
      else
        {$IFDEF OXYGENE}
          fn_decode := @decodeHuffmanRefineAC ;
        {$ELSE}
          fn_decode :=  decodeHuffmanRefineAC ;
        {$ENDIF}

      stdecoder.iEobrun := 0;
    end ;

    components_frame := stdecoder.nCurrComponent[0] ;
    size_mcu := stdecoder.nMcuSize ;

    if      components_frame = 0 then offset := 0
    else if components_frame = 1 then offset := size_mcu - 128
    else                              offset := size_mcu -  64 ;

    lines       := stdecoder.iHeight ;
    samples     := stdecoder.iWidth  ;
    vert_blocks := ( lines   + 7 ) shr 3 ;
    horz_blocks := ( samples + 7 ) shr 3 ;

    image_format := stdecoder.imageFormat;

    if components_frame <> 0 then begin
      if      image_format = T_JPGCONST.FORMAT_411 then begin
                vert_blocks := ( lines   + 15 ) shr 4 ;
                horz_blocks := ( samples + 15 ) shr 4 ;
              end
      else if image_format = T_JPGCONST.FORMAT_422 then begin
                horz_blocks := ( samples + 15 ) shr 4 ;
              end ;
    end ;

    offset_coef := 0 ;
    offset_mcu  := 0 ;
    horz_mcus   := stdecoder.nHorzMcu ;

    for i := 0 to vert_blocks - 1 do begin

      for j := 0 to horz_blocks - 1 do begin

        if marker_sof = T_JPGCONST.SOF2 then begin
          inc( offset_coef, offset ) ;
          copyCoefToMcu( offset_coef, offset_mcu, 64 ) ;
          offset_coef := 0 ;
        end
        else begin
          clearMcu( offset_mcu, 64 ) ;
        end ;

        res := fn_decode( offset_mcu, 0 ) ;
        if res <> T_JPGCONST.SUCCESS then begin
          Result := res ;
          exit ;
        end;

        inc( offset_coef, offset ) ;
        copyMcuToCoef( offset_mcu, offset_coef, 64 ) ;
        offset_coef := 0 ;

        inc( offset, size_mcu ) ;

        if components_frame = 0 then begin
          if ( image_format = T_JPGCONST.FORMAT_411 ) or
             ( image_format = T_JPGCONST.FORMAT_422 ) then
          begin
            if ( j and 1 ) > 0 then dec( offset, 64 )
                               else dec( offset, ( size_mcu - 64 ) ) ;
          end ;
        end ;

      end ;

      if components_frame = 0 then begin
        if image_format = T_JPGCONST.FORMAT_411 then begin
          if (i and 1) > 0 then begin
            if ( horz_blocks and 1 ) > 0 then
              inc( offset, 320 ) ;

            dec( offset, 128 ) ;
          end
          else begin
            if ( horz_blocks and 1 ) > 0 then
              inc( offset, 320 ) ;

            dec( offset, 384 * horz_mcus ) ;
            inc( offset, 128 );
          end
        end
      end ;

    end ;

    Result := T_JPGCONST.SUCCESS ;
  end ;

  // decode interleaved frame
  function T_jpegdecoder.decodeInterleaved( const _flag : Boolean ) : Integer;
  var
    i, j, k, l, m    : Integer ;
    components_frame : Integer ;
    approx_bit_hi    : Byte    ;
    marker_sof       : Integer ;
    offset_coef      : Integer ;
    offset_mcu       : Integer ;
    offset_img       : Integer ;
    size_mcu         : Integer ;
    horz_mcus        : Integer ;
    vert_mcus        : Integer ;
    row_mcu          : Integer ;
    col_mcu          : Integer ;
    fn_decode        : T_fndecodeHuffman ;
    horz_factor      : Integer ;
    vert_factor      : Integer ;
    quant_tab_sel    : Byte    ;
    restart_interval : Integer ;
    res              : Integer ;
    increm1,
    increm2          : Integer ;
  begin
    marker_sof    := stdecoder.markerSOF    ;
    approx_bit_hi := stdecoder.nApproxBitHi ;

    if      marker_sof <> T_JPGCONST.SOF2 then
            {$IFDEF OXYGENE}
              fn_decode := @decodeHuffmanSequential
            {$ELSE}
              fn_decode :=  decodeHuffmanSequential
            {$ENDIF}
    else if approx_bit_hi = 0 then
            {$IFDEF OXYGENE}
              fn_decode := @decodeHuffmanFirstDC
            {$ELSE}
              fn_decode :=  decodeHuffmanFirstDC
            {$ENDIF}
    else
            {$IFDEF OXYGENE}
              fn_decode := @decodeHuffmanRefineDC ;
            {$ELSE}
              fn_decode :=  decodeHuffmanRefineDC ;
            {$ENDIF}

    horz_mcus        := stdecoder.nHorzMcu ;
    vert_mcus        := stdecoder.nVertMcu ;
    size_mcu         := stdecoder.nMcuSize ;
    components_frame := stdecoder.nComponentsFrame ;
    offset_coef      := 0 ;
    offset_mcu       := 0 ;

    stdecoder.nImageOffset := 0 ;
    offset_img := stdecoder.nImageOffset ;

    increm1 := bytesPerPixel*8 ;
    increm2 := 0 ;
    case stdecoder.imageFormat of
      T_JPGCONST.FORMAT_444 :
        increm2 := stdecoder.iInc2*2*bytesPerPixel ;
      T_JPGCONST.FORMAT_422 :
        increm2 := stdecoder.iInc2*2 ;
      T_JPGCONST.FORMAT_411 :
        increm2 := stdecoder.iInc2*2 ;
      T_JPGCONST.FORMAT_400 :
        increm2 := stdecoder.iInc2*2*2 ;
    end;

    for i := 1 to vert_mcus do begin
      if arOffset[0] = 0 then begin
        arOffset[0] := oStream.Position ;
        arLdc[0][0] := 0 ;
        arLdc[0][1] := 0 ;
        arLdc[0][2] := 0 ;
        arLdc[0][3] := 0 ;
        arCode[0]   := stdecoder.uCode ;
        arSize[0]   := stdecoder.nSize ;
      end ;

      for j := 1 to horz_mcus do begin

        if i = vert_mcus then row_mcu := 1
                         else row_mcu := 0 ;

        if j = horz_mcus then col_mcu := 1
                         else col_mcu := 0 ;

        if (_flag and (approx_bit_hi = 0)) then
          clearMcu( offset_mcu, size_mcu )
        else
          copyCoefToMcu( offset_coef, offset_mcu, size_mcu );

        for k := 0 to components_frame-1 do begin
          vert_factor := stdecoder.arVertSampling[k] ;
          horz_factor := stdecoder.arHorzSampling[k] ;

          quant_tab_sel := stdecoder.nQuantTableSel[k] ;

          for l := 0 to vert_factor - 1 do begin

            for m := 0 to horz_factor-1 do begin
              if _flag then begin
                res := fn_decode( offset_mcu, k ) ;
                if res <> T_JPGCONST.SUCCESS then begin
                  Result := res ;
                  exit ;
                end;

                if marker_sof = T_JPGCONST.SOF2 then
                  copyMcuToCoef( offset_mcu, offset_coef, 64 ) ;
              end ;

              decodeZigzag( offset_mcu,
                            stdecoder.arQuantization[quant_tab_sel]
                          ) ;
              decodeIdct( offset_mcu ) ;

              inc( offset_mcu , 64 ) ;
              inc( offset_coef, 64 ) ;

            end ;

          end ;

        end ;

        offset_mcu := 0 ;

        stdecoder.fnWrite( offset_img
                         ) ;

        dec( stdecoder.nRestartPending );

        restart_interval := stdecoder.nRestartInterval ;

        if not ( ( row_mcu > 0 ) and ( col_mcu > 0 ) ) then begin
          if restart_interval > 0 then begin
            if stdecoder.nRestartPending = 0 then begin
              res := readMarkerRST;
              if res <> T_JPGCONST.SUCCESS then begin
                Result := res;
                exit ;
              end;
            end;
          end;
        end ;
        inc( offset_img, increm1 ) ;

      end ;

      if not isProgressive then begin
        if nPart <> -1 then
          inc( nPart )
        else
          nPart := 0 ;
        break ;
      end ;
      inc( offset_img, increm2 );
    end ;

    nPart := 0 ;

    Result := T_JPGCONST.SUCCESS ;
  end ;

  // decode interleaved frame
  function T_jpegdecoder.decodeInterleavedForced : Integer;
  var
    i, j, k, l       : Integer ;
    components_frame : Integer ;
    offset_mcu       : Integer ;
    offset_img       : Integer ;
    size_mcu         : Integer ;
    horz_mcus        : Integer ;
    vert_mcus        : Integer ;
    row_mcu          : Integer ;
    col_mcu          : Integer ;
    horz_factor      : Integer ;
    vert_factor      : Integer ;
    quant_tab_sel    : Byte    ;
    restart_interval : Integer ;
    part             : Integer ;

    ucode            : Cardinal    ;
    nsize            : Integer     ;
    hwc              : Cardinal    ;
    coefficient      : Integer     ;
    huffval          : Byte        ;
    huffsize         : Byte        ;
    run_length       : Byte        ;
    size             : Byte        ;
    entropy_dc       : Byte        ;
    entropy_ac       : Byte        ;
    huff_st_dc       : T_stHuffman ;
    huff_st_ac       : T_stHuffman ;
    bnegs            : Boolean     ;
    dc               : Integer     ;
    ac               : Integer     ;

    hf_w             : Word        ;

    in_buff          : ArrayOfByte ;
    in_buff_size     : Integer     ;
    in_buff_pos      : Integer     ;

    in_position      : Integer ;

    inc_part         : Integer ;
    istart           : Integer ;
    img_size         : Integer ;

    last_read        : Boolean ;

    procedure readHuffmanWord_FF ;
    var
      s : Integer ;
    begin
      s := 0 ;
      if in_buff[in_buff_pos] = ($ff) then begin
        hf_w := Word(in_buff[in_buff_pos +2]) ;
        inc(s) ;
      end
      else
        hf_w := Word(in_buff[in_buff_pos +1]) ;

      hf_w := Word(hf_w or (Integer(in_buff[in_buff_pos]) shl 8)) ;

      if (hf_w and ($ff)) = ($ff) then
        inc(s) ;
      inc(in_buff_pos, 2 +s) ;
    end;

  begin

    horz_mcus        := stdecoder.nHorzMcu ;
    vert_mcus        := stdecoder.nVertMcu ;
    size_mcu         := stdecoder.nMcuSize ;
    components_frame := stdecoder.nComponentsFrame ;
    offset_mcu       := 0 ;

    stdecoder.nImageOffset := 0;
    offset_img := 0 ;

    last_read := False ;
    in_buff_size := 0 ;
    in_buff_pos := 0 ;
    img_size := Integer(oStream.Size) ;

    if vert_mcus = (nPartNeeded -1) then
      offset_mcu := 0 ;

    if arOffset[nPartNeeded] = 0 then begin
      if arOffset[0] = 0 then begin
        in_position  := oStream.Position ;
        arOffset[0] := in_position ;
        arLdc[0][0] := 0 ;
        arLdc[0][1] := 0 ;
        arLdc[0][2] := 0 ;
        arLdc[0][3] := 0 ;
        arCode[0]   := stdecoder.uCode ;
        arSize[0]   := stdecoder.nSize ;
        part := 0 ;
      end
      else begin
        part := nPartNeeded -1 ;
        while arOffset[part] = 0 do
          dec(part) ;

        in_position  := arOffset[part] ;
        oStream.Position := in_position ;

        inc_part := part +1 ;
        in_position  := arOffset[part] ;
        oStream.Position := in_position ;

        stdecoder.uCode  := arCode  [part] ;
        stdecoder.nSize  := arSize  [part] ;

        stdecoder.arLdc[0] := arLdc[part][0] ;
        stdecoder.arLdc[1] := arLdc[part][1] ;
        stdecoder.arLdc[2] := arLdc[part][2] ;
        stdecoder.arLdc[3] := arLdc[part][3] ;

        if nPartNeeded = 0 then
          stdecoder.nRestartPending := stdecoder.nRestartInterval
        else
        if arRstPending[nPartNeeded -1] <> 0 then
          stdecoder.nRestartPending := arRstPending[nPartNeeded -1] ;

        if inc_part = vert_mcus then
          in_buff_size := Cardinal(img_size) -arOffset[part]
        else
        if arOffset[inc_part] <> 0 then
          in_buff_size := arOffset[inc_part] -arOffset[part] ;
      end ;
    end
    else begin
      part  := nPartNeeded ;
      inc_part := part +1 ;
      in_position := arOffset[nPartNeeded] ;
      oStream.Position := in_position ;

      stdecoder.uCode  := arCode  [nPartNeeded] ;
      stdecoder.nSize  := arSize  [nPartNeeded] ;

      stdecoder.arLdc[0] := arLdc[nPartNeeded][0] ;
      stdecoder.arLdc[1] := arLdc[nPartNeeded][1] ;
      stdecoder.arLdc[2] := arLdc[nPartNeeded][2] ;
      stdecoder.arLdc[3] := arLdc[nPartNeeded][3] ;

      if nPartNeeded = 0 then
        stdecoder.nRestartPending := stdecoder.nRestartInterval
      else
      if arRstPending[nPartNeeded -1] <> 0 then
        stdecoder.nRestartPending := arRstPending[nPartNeeded -1] ;

      if inc_part = vert_mcus then
        in_buff_size := Cardinal(img_size) -arOffset[nPartNeeded]
      else
      if arOffset[inc_part] <> 0 then
        in_buff_size := arOffset[inc_part] -arOffset[part] ;
    end ;

    if in_buff_size = 0 then begin
      in_buff_size := components_frame * stdecoder.iWidthR * 16 ;
      if in_buff_size > img_size - in_position then begin
        in_buff_size := img_size - in_position ;
        last_read := True ;
      end ;
    end ;
    SetLength(in_buff, in_buff_size +16) ;
    {$IFDEF OXYGENE}
      oStream.Read( in_buff, in_buff_size );
    {$ELSE}
      oStream.Read( in_buff[0], in_buff_size );
    {$ENDIF}

    ucode := stdecoder.uCode ;
    nsize := stdecoder.nSize ;

    istart := part ;

    for i := istart to vert_mcus do begin
      if i > istart then begin

        if not last_read then begin
          in_position := in_position +in_buff_pos ;
          oStream.Position := in_position ;
          if in_buff_size > img_size - in_position then begin
            in_buff_size := img_size - in_position ;
            last_read := True ;
          end ;

          {$IFDEF OXYGENE}
            oStream.Read( in_buff, in_buff_size );
          {$ELSE}
            oStream.Read( in_buff[0], in_buff_size );
          {$ENDIF}
          in_buff_pos := 0 ;
        end ;
      end ;

      if arOffset[i] = 0 then begin
        inc(part) ;
        arOffset[i] :=  in_position +in_buff_pos ;

        arLdc[i][0] := stdecoder.arLdc[0] ;
        arLdc[i][1] := stdecoder.arLdc[1] ;
        arLdc[i][2] := stdecoder.arLdc[2] ;
        arLdc[i][3] := stdecoder.arLdc[3] ;

        arCode[i]   := ucode    ;
        arSize[i]   := nsize    ;
      end ;

      if (part +1) = vert_mcus then row_mcu := 1
                               else row_mcu := 0 ;

      for j := 1 to horz_mcus do begin
        if j = horz_mcus then col_mcu := 1
                         else col_mcu := 0 ;

        clearMcu( offset_mcu, size_mcu ) ;
        for k := 0 to components_frame-1 do begin

          vert_factor   := stdecoder.arVertSampling[k] ;
          horz_factor   := stdecoder.arHorzSampling[k] ;
          quant_tab_sel := stdecoder.nQuantTableSel[k] ;
//H b
          entropy_dc := stdecoder.nEntropyDestSelDC[k] ;
          entropy_ac := stdecoder.nEntropyDestSelAC[k] ;

          huff_st_dc := stdecoder.arHuffman[0][entropy_dc] ;
          huff_st_ac := stdecoder.arHuffman[1][entropy_ac] ;
//H e

          for l := 0 to ( vert_factor *horz_factor ) - 1 do begin

//decodeHuffmanSequential BEGIN
            if nsize < 16 then begin
              readHuffmanWord_FF ;
              ucode := ucode or ( Cardinal(hf_w) shl ( 16 - nsize ) ) ;
              inc( nsize, 16 ) ;
            end ;

            hwc := ucode shr (16 +8);
            huffsize := huff_st_dc.ucSizeHi[hwc] ;
            if huffsize > 0 then begin
              huffval  := huff_st_dc.ucValHi[hwc] ;
            end
            else begin
              hwc := (ucode shr 16) and $FFF;
              huffsize := huff_st_dc.ucSizeLo[hwc] ;
              huffval  := huff_st_dc.ucValLo[hwc] ;
            end;

            ucode := ucode shl huffsize ;
            dec( nsize, huffsize ) ;

            if nsize < huffval then begin
              readHuffmanWord_FF ;
              ucode := ucode or ( Cardinal(hf_w) shl ( 16 - nsize ) ) ;
              inc( nsize, 16 ) ;
            end ;

            bnegs := ( ucode and $80000000 ) = 0 ;

            if huffval = 0 then
              dc := 0
            else
              dc := ucode shr ( 32 - huffval ) ;

            ucode := ucode shl huffval ;
            dec( nsize, huffval ) ;

            if bnegs then begin
              inc( dc, T_JPGCONST.NEGS[huffval] ) ;
            end ;

            arMcu[offset_mcu] := dc + stdecoder.arLdc[k] ;
            stdecoder.arLdc[k] := arMcu[offset_mcu] ;

            coefficient := offset_mcu +1 ;
            while coefficient < offset_mcu +64 do begin

              if nsize < 16 then begin
                readHuffmanWord_FF ;
                ucode := ucode or ( Cardinal(hf_w) shl ( 16 - nsize ) ) ;
                inc( nsize, 16 ) ;
              end ;

              hwc := ucode shr (16 +8);
              huffsize := huff_st_ac.ucSizeHi[hwc] ;
              if huffsize > 0 then begin
                huffval  := huff_st_ac.ucValHi[hwc] ;
              end
              else begin
                hwc := (ucode shr 16) and $FFF;
                huffsize := huff_st_ac.ucSizeLo[hwc] ;
                huffval  := huff_st_ac.ucValLo[hwc] ;
              end;

              ucode := ucode shl huffsize ;
              dec( nsize, huffsize ) ;

              run_length := huffval shr 4 ;
              size := huffval and 15 ;

              if size > 0 then begin

                if run_length > 0 then
                  inc( coefficient, run_length ) ;

                if nsize < size then begin
                  readHuffmanWord_FF ;
                  ucode := ucode or ( Cardinal(hf_w) shl ( 16 - nsize ) ) ;
                  inc( nsize, 16 ) ;
                end ;

                ac := ucode shr ( 32 - size );

                bnegs := ( ucode and $80000000 ) = 0 ;

                ucode := ucode shl size ;
                dec( nsize, size );

                if bnegs then
                  inc( ac, T_JPGCONST.NEGS[size] ) ;

                arMcu[coefficient] := ac ;
              end
              else begin

                if run_length <> 15 then
                  break;

                inc( coefficient, 15 ) ;
              end ;

              inc( coefficient ) ;
            end;

//decodeHuffmanSequential END
            if part < nPartNeeded then begin
               continue ;
            end ;

            decodeZigzag( offset_mcu,
                          stdecoder.arQuantization[quant_tab_sel]
                        ) ;
            decodeIdct( offset_mcu );

            inc(offset_mcu , 64 ) ;
          end ;
        end ;

        if part >= nPartNeeded then begin
          offset_mcu := 0 ;
          stdecoder.fnWrite( offset_img ) ;
        end ;

        dec( stdecoder.nRestartPending );
        restart_interval := stdecoder.nRestartInterval ;

        if not ( ( row_mcu > 0 ) and ( col_mcu > 0 ) ) then begin
          if (restart_interval>0) then begin
            if stdecoder.nRestartPending = 0 then begin
              oStream.Position :=  in_position + in_buff_pos ;

              rsInc := 0 ;
              stdecoder.uCode := ucode ;
              stdecoder.nSize := nsize ;
              readMarkerRST ;
              if rsInc <> 0 then
                inc(in_buff_pos ,rsInc) ;
              ucode := 0 ;
              nsize := 0 ;

            end ;
          end ;
        end ;

        if part >= nPartNeeded then
          inc(offset_img, bytesPerPixel*8 ) ;
      end ;

      if i = nPartNeeded then begin
        nPart := nPartNeeded ;
        part := nPartNeeded + 1 ;

        if part < stripNo then begin
          if arOffset[part] = 0 then begin
            arOffset[part] :=  in_position + in_buff_pos ;

            arLdc[part][0] := stdecoder.arLdc[0] ;
            arLdc[part][1] := stdecoder.arLdc[1] ;
            arLdc[part][2] := stdecoder.arLdc[2] ;
            arLdc[part][3] := stdecoder.arLdc[3] ;

            arCode[part]   := ucode    ;
            arSize[part]   := nsize    ;
          end ;
        end ;
        break ;
      end ;

    end ;

    in_buff := nil ;
    stdecoder.uCode := ucode ;
    stdecoder.nSize := nsize ;
    arRstPending[nPartNeeded] := stdecoder.nRestartPending ;

    oStream.Position :=  in_position + in_buff_pos ;
    Result := T_JPGCONST.SUCCESS ;
  end ;

  // Synchronize buffer to decode Huffman tables
  procedure T_jpegdecoder.synchronizeBuffer ;
  var
    nsize : Integer ;
    ucode : Cardinal ;
  begin
    ucode := stdecoder.uCode;
    nsize := stdecoder.nSize;

    ucode := ucode shl ( nsize and $7 );
    nsize := nsize and $f8;

    if nsize <> 0 then
      ucode := ucode shr (32 - nsize)
    else
      ucode := 0;

    while nsize > 0 do begin

      if ((ucode and ($ff)) <> ($ff)) then begin
        oStream.Position := oStream.Position - 1 ;
        dec(rsInc) ;
      end
      else begin
        oStream.Position := oStream.Position - 1;
        oStream.Position := oStream.Position - 1;
        dec(rsInc, 2) ;
      end;

      ucode := ucode shr 8;
      dec( nsize, 8 );
    end ;
    stdecoder.uCode := 0;
    stdecoder.nSize := 0;
  end;

  // prepare single line for DK Draw
  function  T_jpegdecoder.decodeLine(
    const _buffer     : TBytes  ;
    const _buferIndex : Integer ;
    const _line       : Integer ;
    const _lskip      : Integer ;
    const _pixels     : Integer
  ) : Integer ;
  var
    j         : Integer ;
    C, M, Y, K: Integer ;
    Y1        : Integer ;
    Y2        : Integer ;
    CB, CR    : Integer ;
    R, G, B   : Integer ;
    maxVal    : Integer ;
    divider   : Integer ;
    offset    : Integer ;
    skip1th   : Boolean ;
    pos       : Integer ;
    left      : Integer ;
    z         : Integer ;
    part,
    rline,
    sline,
    line      : Integer ;
    lskip     : Integer ;
    sx        : Integer ;
    nr        : Integer ;
    pixels    : Integer ;
  begin
    if stripHeight <= 0 then begin
      j := low(_buffer) ;
      maxVal := length(_buffer) ;
      while _buffer[j] <> 0 do begin
        _buffer[j] := 0 ;
        inc(j) ;
        if j >= maxVal then
          break ;
      end;
      exit ;
    end;

    line  := _line ;
    part  := line div stripHeight ;
    sline := part * stripHeight ;
    rline := line - sline ;

    if part >= stripNo then begin
      part := stripNo -1 ;
      if stripHeight = 8 then begin
        case FScale of
          8 : rline := 0 ;
          4 : rline := 4 ;
          2 : rline := 6 ;
          else rline := 7 ;
        end;
      end
      else begin
        case FScale of
          8 : rline := 8 ;
          4 : rline := 12 ;
          2 : rline := 14 ;
          else rline := 15 ;
        end;
      end;
    end;

    if part <> nPart then begin

      if length(arPartsScales) = 0 then
        doAlive ;

      if bufferedPartsNo > 1 then begin
        if part > (length(arPartsScales) -1) then
          part := length(arPartsScales) -1 ;

        if arPartsScales[part] > 0 then begin
          if arPartsScales[part] <= FScale then begin
            nPartOffset := arImagePartSize*part ;
            nPart := part ;
          end;
        end;
      end
      else begin
        nPartOffset := 0 ;
      end;
    end
    else begin
      if bufferedPartsNo > 1 then begin
        if arPartsScales[part] > 0 then begin
            if arPartsScales[part] > FScale then begin
              inc(nPart) ;
            end;
        end ;
      end;
    end;

    if part <> nPart then begin
      if bufferedPartsNo > 1 then begin
        nPartOffset := arImagePartSize*part ;
        arPartsScales[part] := FScale ;
      end
      else
        nPartOffset := 0 ;
      nPartNeeded := part ;
      if nPart =  stripNo then begin
        doAlive ;
        if isProgressive then begin
          oStream.Position := arOffset[0] ;
          nPart := -1 ;
        end ;
      end ;
      if nPart = -1 then
        dodecode ;
      if not isProgressive then
        decodeInterleavedForced
    end ;

    if _pixels <> stdecoder.iWidth then begin
      if arImageMaxVal = 4095 then begin
        if _pixels > stdecoder.iWidth then
          pixels := stdecoder.iWidth
        else
          pixels := _pixels ;
        if FNativeRequested then begin
          divider := 1024 ;
          maxVal :=  4095 ;
        end
        else begin
          if length(_buffer) = pixels then begin
            pixels := pixels div 6 ;
            maxVal :=  4095 ;
            divider := 1024 ;
          end
          else begin
            maxVal :=  255 ;
            divider := 16384 ;
          end;
        end;
      end
      else begin
        if length(_buffer) = _pixels then
          pixels := _pixels div 3
        else
          pixels := _pixels ;
        divider := 1024 ;
        maxVal :=  255 ;
      end;
    end
    else begin
      pixels := _pixels ;
      if arImageMaxVal = 4095 then
        divider := 16384
      else
        divider := 1024 ;
      maxVal :=  255 ;
    end;

    if bytesPerPixel >= 3 then begin
      lskip  := FScale*_lskip ;
      offset := bytesPerPixel * ( rline * stdecoder.iWidthR  + lskip ) + nPartOffset ;
      pos    := _buferIndex ;

      if FComponentsJpeg = JPEG_YBR then begin

        for j := 1 to pixels  do begin
          Y  := Integer(arImage[offset +0]) shl 10 ;
          CB := Integer(arImage[offset +1])  - arImageMedVal ;
          CR := Integer(arImage[offset +2])  - arImageMedVal ;
          inc( offset, FScale*bytesPerPixel ) ;

          R := ( Y              + 1436 * CR ) div divider ;
          G := ( Y  -  352 * CB -  731 * CR ) div divider ;
          B := ( Y  + 1815 * CB             ) div divider ;

          if stdecoder.nComponentsFrame = 4 then begin
            z := Min( R, G ) ;
            z := Min( z, B ) ;
            z := z - z div 3 ;
            R := maxVal -(R + z) ;
            G := maxVal -(G + z) ;
            B := maxVal -(B + z) ;
          end ;

          if      R < 0   then R := 0
          else if R > maxVal then R := maxVal ;

          if      G < 0   then G := 0
          else if G > maxVal then G := maxVal ;

          if      B < 0   then B := 0
          else if B > maxVal then B := maxVal ;

          if FJpegInTiff then begin
              if maxVal = 255 then begin
                _buffer[pos] := R ;
                inc(pos) ;
                _buffer[pos] := G ;
                inc(pos) ;
                _buffer[pos] := B ;
                inc(pos) ;
              end
              else begin
                _buffer[pos] := R and $FF ;
                inc(pos) ;
                _buffer[pos] := ( R shr 8) and $FF ;
                inc(pos) ;
                _buffer[pos] := G and $FF ;
                inc(pos) ;
                _buffer[pos] := ( G shr 8) and $FF ;
                inc(pos) ;
                _buffer[pos] := B and $FF ;
                inc(pos) ;
                _buffer[pos] := ( B shr 8) and $FF ;
                inc(pos) ;
              end;
          end
          else begin
            _buffer[pos] := B ;
            inc(pos) ;
            _buffer[pos] := G ;
            inc(pos) ;
            _buffer[pos] := R ;
            inc(pos) ;
          end;
        end  ;
      end
      else begin
        for j := 1 to pixels  do begin

          if FComponentsJpeg = JPEG_RGB then begin
            R := Integer(arImage[offset +0]) ;
            G := Integer(arImage[offset +1]) ;
            B := Integer(arImage[offset +2]) ;
          end
          else if FComponentsJpeg = JPEG_CMYK then begin
            C := Integer(arImage[offset +0]) ;
            M := Integer(arImage[offset +1]) ;
            Y := Integer(arImage[offset +2]) ;
            K := Integer(arImage[offset +3]) ;

            R := TruncS( (C * K) / maxVal ) ;
            G := TruncS( (M * K) / maxVal ) ;
            B := TruncS( (Y * K) / maxVal ) ;

            if      R < 0   then R := 0
            else if R > maxVal then R := maxVal ;

            if      G < 0   then G := 0
            else if G > maxVal then G := maxVal ;

            if      B < 0   then B := 0
            else if B > maxVal then B := maxVal ;

          end
          else begin
            B := Integer(arImage[offset +0]) ;
            G := Integer(arImage[offset +1]) ;
            R := Integer(arImage[offset +2]) ;
          end;
          _buffer[pos] := B ;
          inc(pos) ;
          _buffer[pos] := G ;
          inc(pos) ;
          _buffer[pos] := R ;
          inc(pos) ;
          if FdecodeMode = JPEG_ARGB then begin
            if bytesPerPixel = 4 then
              _buffer[pos] := arImage[offset +3]
            else
              _buffer[pos] := $FF ;
            inc(pos) ;
          end;
          inc( offset, FScale*bytesPerPixel ) ;
        end  ;
      end;
    end
    else begin
      if FScale = 1 then begin

        lskip := _lskip ;
        left  := lskip ;

        if (left and 1) = 1 then begin
          dec(left) ;
          skip1th := True
        end
        else
          skip1th := False ;
        offset := 2 * ( rline * stdecoder.iWidthR  + left ) + nPartOffset ;

        pos := _buferIndex ;
        nr := 0 ;


        while true do begin
          Y1 := Integer(arImage[offset]) shl 10 ;
          inc( offset ) ;
          CB := Integer( arImage[offset] ) - arImageMedVal ;
          inc( offset ) ;

          Y2 :=  Integer(arImage[offset]) shl 10 ;
          inc( offset ) ;
          CR := Integer( arImage[offset] ) - arImageMedVal ;
          inc( offset ) ;

          if not skip1th then begin
            R := ( Y1              + 1436 * CR ) div divider ;
            G := ( Y1  -  352 * CB -  731 * CR ) div divider ;
            B := ( Y1  + 1815 * CB             ) div divider ;

            if      R < 0      then R := 0
            else if R > maxVal then R := maxVal ;

            if      G < 0      then G := 0
            else if G > maxVal then G := maxVal ;

            if      B < 0      then B := 0
            else if B > maxVal then B := maxVal ;

            if FdecodeMode = JPEG_GRAYSCAL then begin
              _buffer[pos] := B ;
              inc(pos) ;
            end
            else
            if FdecodeMode = JPEG_RGB then begin
              if maxVal = 255 then begin
                _buffer[pos] := B ;
                inc(pos) ;
                _buffer[pos] := G ;
                inc(pos) ;
                _buffer[pos] := R ;
                inc(pos) ;
              end
              else begin
                _buffer[pos] := B and $FF ;
                inc(pos) ;
                _buffer[pos] := ( B shr 8) and $FF ;
                inc(pos) ;
                _buffer[pos] := G and $FF ;
                inc(pos) ;
                _buffer[pos] := ( G shr 8) and $FF ;
                inc(pos) ;
                _buffer[pos] := R and $FF ;
                inc(pos) ;
                _buffer[pos] := ( R shr 8) and $FF ;
                inc(pos) ;
              end;


            end
            else
            if FdecodeMode = JPEG_BGR then begin
              if maxVal = 255 then begin
                _buffer[pos] := R ;
                inc(pos) ;
                _buffer[pos] := G ;
                inc(pos) ;
                _buffer[pos] := B ;
                inc(pos) ;
              end
              else begin
                _buffer[pos] := R and $FF ;
                inc(pos) ;
                _buffer[pos] := ( R shr 8) and $FF ;
                inc(pos) ;
                _buffer[pos] := G and $FF ;
                inc(pos) ;
                _buffer[pos] := ( G shr 8) and $FF ;
                inc(pos) ;
                _buffer[pos] := B and $FF ;
                inc(pos) ;
                _buffer[pos] := ( B shr 8) and $FF ;
                inc(pos) ;
              end;
            end  ;
            inc(nr) ;
          end
          else
            skip1th := False ;

          if nr >= pixels then
            break ;

          R := ( Y2              + 1436 * CR ) div divider ;
          G := ( Y2  -  352 * CB -  731 * CR ) div divider ;
          B := ( Y2  + 1815 * CB             ) div divider ;

          if      R < 0      then R := 0
          else if R > maxVal then R := maxVal ;

          if      G < 0      then G := 0
          else if G > maxVal then G := maxVal ;

          if      B < 0      then B := 0
          else if B > maxVal then B := maxVal ;

          if FdecodeMode = JPEG_GRAYSCAL then begin
            _buffer[pos] := B ;
            inc(pos) ;
          end
          else
          if FdecodeMode = JPEG_RGB then begin
            if maxVal = 255 then begin
              _buffer[pos] := B ;
              inc(pos) ;
              _buffer[pos] := G ;
              inc(pos) ;
              _buffer[pos] := R ;
              inc(pos) ;
            end
            else begin
              _buffer[pos] := B and $FF ;
              inc(pos) ;
              _buffer[pos] := ( B shr 8) and $FF ;
              inc(pos) ;
              _buffer[pos] := G and $FF ;
              inc(pos) ;
              _buffer[pos] := ( G shr 8) and $FF ;
              inc(pos) ;
              _buffer[pos] := R and $FF ;
              inc(pos) ;
              _buffer[pos] := ( R shr 8) and $FF ;
              inc(pos) ;
            end;
          end
          else
          if FdecodeMode = JPEG_BGR then begin
            if maxVal = 255 then begin
              _buffer[pos] := R ;
              inc(pos) ;
              _buffer[pos] := G ;
              inc(pos) ;
              _buffer[pos] := B ;
              inc(pos) ;
            end
            else begin
              _buffer[pos] := R and $FF ;
              inc(pos) ;
              _buffer[pos] := ( R shr 8) and $FF ;
              inc(pos) ;
              _buffer[pos] := G and $FF ;
              inc(pos) ;
              _buffer[pos] := ( G shr 8) and $FF ;
              inc(pos) ;
              _buffer[pos] := B and $FF ;
              inc(pos) ;
              _buffer[pos] := ( B shr 8) and $FF ;
              inc(pos) ;
            end;
          end  ;

          inc(nr) ;
          if nr >= pixels then
            break ;
        end  ;
      end
      else begin
        left   := FScale*_lskip ;
        offset := 2 * ( rline * stdecoder.iWidthR  + left ) + nPartOffset ;

        pos := _buferIndex ;
        nr := 0 ;
        sx := 2*FScale ;

        while true do begin
          Y1 :=  Integer(arImage[offset]) shl 10 ;
          CB := Integer( arImage[offset +1] ) - arImageMedVal ;
          CR := Integer( arImage[offset +3] ) - arImageMedVal ;
          inc(offset, sx) ;

          R := ( Y1              + 1436 * CR ) div divider ;
          G := ( Y1  -  352 * CB -  731 * CR ) div divider ;
          B := ( Y1  + 1815 * CB             ) div divider ;

          if      R < 0   then R := 0
          else if R > maxVal then R := maxVal ;

          if      G < 0   then G := 0
          else if G > maxVal then G := maxVal ;

          if      B < 0   then B := 0
          else if B > maxVal then B := maxVal ;

          if FdecodeMode = JPEG_GRAYSCAL then begin
            _buffer[pos] := B ;
            inc(pos) ;
          end
          else
          if FdecodeMode = JPEG_RGB then begin
            _buffer[pos] := B ;
            inc(pos) ;
            _buffer[pos] := G ;
            inc(pos) ;
            _buffer[pos] := R ;
            inc(pos) ;
          end
          else
          if FdecodeMode = JPEG_BGR then begin
            _buffer[pos] := R ;
            inc(pos) ;
            _buffer[pos] := G ;
            inc(pos) ;
            _buffer[pos] := B ;
            inc(pos) ;
          end  ;
          inc(nr) ;

          if nr >= pixels then
            break ;
        end;
      end;
    end;

    Result := pos ;

  end ;

  // Main decoder entry
  function T_jpegdecoder.dodecode : Integer ;
  var
    tmp : Word;
  begin
    if assigned( stdecoder ) then begin

      if nPart = -1 then begin
        if isProgressive then begin
          if arOffset[0] = 0 then
            arOffset[0] := oStream.Position ;
        end ;

        stdecoder.uCode            := 0;
        stdecoder.nSize            := 0;
        //set but not read
        stdecoder.nBufferOffset  := 0 ;
      end ;

      tmp := decodeImage(False);
    end
    else
      tmp := T_JPGCONST.ERR_INSUFFICIENT_MEMORY;

    Result := tmp;
  end ;

  // Initialize decoder. Perform initial decoding steps.
  function T_jpegdecoder.doInitialize(
    const _mode : TGIS_LayerDormantMode
  ) : Integer ;
  var
    i : Integer ;
  begin
    if assigned( stdecoder ) then begin
      dormantMode := _mode ;
      stdecoder.bDetectedSOF := False;
      stdecoder.nRestartInterval := 0;
      stdecoder.nRestartPending  := MaxInt;

      oStream.Position := 0;

      nPart := -1 ;
      if assigned(arOffset)  then begin
        if  arOffset[0] <> 0 then
          for i := 0 to length(arOffset) - 1 do
            arOffset[i] := 0 ;
        arPartsScales := nil ;
      end;
      Result := decodeImage(True) ;
    end
    else
      Result := T_JPGCONST.ERR_INSUFFICIENT_MEMORY ;
  end ;

  // Initialize decoder. Perform initial decoding steps.
  function T_jpegdecoder.doReinitialize(
    const _mode : TGIS_LayerDormantMode
  ) : Integer ;
  var
    i : Integer ;
    do_decoding : Boolean ;
  begin
    if assigned( stdecoder ) then begin
      do_decoding := True ;
      stdecoder.bDetectedSOF := False;
      oStream.Position := 0;
      nPart := -1 ;
      if assigned(arOffset)  then begin
        if  arOffset[0] <> 0 then begin
          do_decoding := False ;
          if arOffset[0] <> 320 then begin
            if  arOffset[0] > 700 then begin
              Result := doInitialize(_mode) ;
              exit ;
            end
            else
              oStream.Position := arOffset[0] ;
          end;
          stdecoder.bDetectedSOF := True;

          for i := 1 to length(arOffset) - 1 do
            arOffset[i] := 0 ;

          stdecoder.arLdc[0] := 0 ;
          stdecoder.arLdc[1] := 0 ;
          stdecoder.arLdc[2] := 0 ;
          stdecoder.arLdc[3] := 0 ;
          stdecoder.nRestartNext := 0 ;
        end;
      end;
      if assigned(arPartsScales) then
        for i := 0 to bufferedPartsNo - 1 do
          arPartsScales[i] := 0 ;

      oStream.Position := 0;
      if do_decoding then
        Result := decodeImage(True)
      else
        Result := T_JPGCONST.SUCCESS ;
    end
    else
      Result := T_JPGCONST.ERR_INSUFFICIENT_MEMORY ;
  end ;

  // Free memory to reduce memory consumption
  procedure T_jpegdecoder.doDormant ;
  begin

    if bufferedPartsNo > 1 then begin
      bufferedPartsNo := 1 ;
      arImageSize := arImagePartSize ;
    end;
    if nPart = -1 then
      exit ;
    if isProgressive then
      arCoefficient := nil ;

    arPartsScales := nil ;
    arImage := nil     ;
    nPart   := stripNo ;
  end ;

  // Restores huffman tables (after calls to doDormant)
  procedure  T_jpegdecoder.doAlive ;
  begin
    if assigned( arImage ) then
      exit ;
    if arImageSize <= 0 then
      exit ;

    if isProgressive then
      if arCoefficient = nil then
        SetLength( arCoefficient, stdecoder.nVertMcu*stdecoder.nHorzMcu* stdecoder.nMcuSize ) ;

    SetLength( arImage, arImageSize) ;
    SetLength( arPartsScales, bufferedPartsNo) ;
  end ;

  // Load default Huffman and quantization tables for abbreviated JPEGs.
  procedure T_jpegdecoder.loadDefaultTables( const _quality : Double ) ;
  const
    QTABLESIZE    = 64 ;

    K2Chrominance : array {$IFNDEF ISLAND}[0..QTABLESIZE-1]{$ENDIF} of Byte =
                    {$IFDEF OXYGENE} [ {$ELSE} ( {$ENDIF}
                      09,  09,  09,  12,  11,  12,  24,  13,
                      13,  24,  50,  33,  28,  33,  50,  50,
                      50,  50,  50,  50,  50,  50,  50,  50,
                      50,  50,  50,  50,  50,  50,  50,  50,
                      50,  50,  50,  50,  50,  50,  50,  50,
                      50,  50,  50,  50,  50,  50,  50,  50,
                      50,  50,  50,  50,  50,  50,  50,  50,
                      50,  50,  50,  50,  50,  50,  50,  50
                     {$IFDEF OXYGENE} ] {$ELSE} ) {$ENDIF} ;
    HUFF_MAX_LEN  = 17 ;
    HUFF_MAX_VALS = 256 ;

    StdDCChrominance : array [0..1] of array[0..HUFF_MAX_LEN-1] of Byte =
                       {$IFDEF OXYGENE}
                         [[0, 0, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0],
                          [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 0, 0, 0, 0, 0]
                         ] ;
                       {$ELSE}
                         ((0, 0, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0),
                          (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 0, 0, 0, 0, 0)
                         ) ;
                       {$ENDIF}

    StdACChrominanceLen : array[0..HUFF_MAX_LEN-1] of Byte =
                          {$IFDEF OXYGENE}
                            [0, 0, 2, 1, 2, 4, 4, 3, 4, 7, 5, 4, 4, 0, 1, 2, $77] ;
                          {$ELSE}
                            (0, 0, 2, 1, 2, 4, 4, 3, 4, 7, 5, 4, 4, 0, 1, 2, $77) ;
                          {$ENDIF}
    StdACChrominanceVal : array[0..HUFF_MAX_VALS-1] of Byte =
                          {$IFDEF OXYGENE} [ {$ELSE} ( {$ENDIF}
                            $00, $01, $02, $03, $11, $04, $05, $21,
                            $31, $06, $12, $41, $51, $07, $61, $71,
                            $13, $22, $32, $81, $08, $14, $42, $91,
                            $a1, $b1, $c1, $09, $23, $33, $52, $f0,
                            $15, $62, $72, $d1, $0a, $16, $24, $34,
                            $e1, $25, $f1, $17, $18, $19, $1a, $26,
                            $27, $28, $29, $2a, $35, $36, $37, $38,
                            $39, $3a, $43, $44, $45, $46, $47, $48,
                            $49, $4a, $53, $54, $55, $56, $57, $58,
                            $59, $5a, $63, $64, $65, $66, $67, $68,
                            $69, $6a, $73, $74, $75, $76, $77, $78,
                            $79, $7a, $82, $83, $84, $85, $86, $87,
                            $88, $89, $8a, $92, $93, $94, $95, $96,
                            $97, $98, $99, $9a, $a2, $a3, $a4, $a5,
                            $a6, $a7, $a8, $a9, $aa, $b2, $b3, $b4,
                            $b5, $b6, $b7, $b8, $b9, $ba, $c2, $c3,
                            $c4, $c5, $c6, $c7, $c8, $c9, $ca, $d2,
                            $d3, $d4, $d5, $d6, $d7, $d8, $d9, $da,
                            $e2, $e3, $e4, $e5, $e6, $e7, $e8, $e9,
                            $ea, $f2, $f3, $f4, $f5, $f6, $f7, $f8,
                            $f9, $fa, $00, $00, $00, $00, $00, $00,
                            $00, $00, $00, $00, $00, $00, $00, $00,
                            $00, $00, $00, $00, $00, $00, $00, $00,
                            $00, $00, $00, $00, $00, $00, $00, $00,
                            $00, $00, $00, $00, $00, $00, $00, $00,
                            $00, $00, $00, $00, $00, $00, $00, $00,
                            $00, $00, $00, $00, $00, $00, $00, $00,
                            $00, $00, $00, $00, $00, $00, $00, $00,
                            $00, $00, $00, $00, $00, $00, $00, $00,
                            $00, $00, $00, $00, $00, $00, $00, $00,
                            $00, $00, $00, $00, $00, $00, $00, $00,
                            $00, $00, $00, $00, $00, $00, $00, $00
                          {$IFDEF OXYGENE} ] {$ELSE} ) {$ENDIF} ;

    procedure getScaledInstance( const _scaleFactor   : Double  ;
                                 const _forceBaseline : Boolean ;
                                 const _inTable       : array of Byte ;
                                   var _outTable      : ArrayOfInteger
                                ) ;
    var
      val, i    : Integer ;
    begin
      for i := 0 to QTABLESIZE - 1 do begin
        val := FloorS(_inTable[i]*_scaleFactor +0.5) ;
        if val <= 0then
          val := 1
        else
        if val > 255 then
          val := 255 ;
        _outTable[i] := val ;
      end ;
    end ;

  var
    z, n, ml    : Integer ;
    huff_st     : T_stHuffman ;
    i, j, k, m  : Integer  ;
    index       : Word     ;
    code        : Word     ;
    bits        : ShortInt ;
  begin

    definedDHT := True ;

    stdecoder.arQuantization[0] := arQuantization[0] ;
    getScaledInstance( _quality, True, K2Chrominance, arQuantization[0] ) ;
    stdecoder.arQuantization[1] := arQuantization[1] ;
    getScaledInstance( _quality, True, K2Chrominance, arQuantization[1] ) ;
    stdecoder.arQuantization[2] := arQuantization[2] ;
    getScaledInstance( _quality, True, K2Chrominance, arQuantization[2] ) ;

    // DC Luminance
    huff_st := T_stHuffman.Create ;
    stdecoder.arHuffman[0][0] := huff_st ;
    SetLength( huff_st.cVal  , 500    ) ;
    SetLength( huff_st.cSize , 17     ) ;

    for z := 0 to HUFF_MAX_LEN-1 do begin
      huff_st.cSize[z] := StdDCChrominance[0][z] ;
      huff_st.cVal [z] := StdDCChrominance[1][z] ;
    end;

    // DC Chrominance
    huff_st := T_stHuffman.Create ;
    stdecoder.arHuffman[0][1] := huff_st ;
    SetLength( huff_st.cVal  , 500    ) ;
    SetLength( huff_st.cSize , 17     ) ;

    for z := 0 to HUFF_MAX_LEN-1 do begin
      huff_st.cSize[z] := StdDCChrominance[0][z] ;
      huff_st.cVal [z] := StdDCChrominance[1][z] ;
    end;

    // AC Luminance
    huff_st := T_stHuffman.Create ;
    stdecoder.arHuffman[1][0] := huff_st ;

    SetLength( huff_st.cVal  , 500    ) ;
    SetLength( huff_st.cSize , 17     ) ;

    for z := 0 to HUFF_MAX_LEN-1 do
      huff_st.cSize[z] := StdACChrominanceLen[z] ;

    for z := 0 to HUFF_MAX_VALS-1 do
      huff_st.cVal[z] := StdACChrominanceVal[z] ;

    // AC Chrominance
    huff_st := T_stHuffman.Create ;
    stdecoder.arHuffman[1][1] := huff_st ;
    SetLength( huff_st.cVal  , 500    ) ;
    SetLength( huff_st.cSize , 17     ) ;

    for z := 0 to HUFF_MAX_LEN-1 do
      huff_st.cSize[z] := StdACChrominanceLen[z] ;

    for z := 0 to HUFF_MAX_VALS-1 do
      huff_st.cVal[z] := StdACChrominanceVal[z] ;

    for n := 0 to 1 do begin
      for m := 0 to 3 do begin
        if assigned(  stdecoder.arHuffman[n][m] ) then begin

            huff_st := stdecoder.arHuffman[n][m] ;

            SetLength( huff_st.ucSizeLo, $1000 ) ;
            SetLength( huff_st.ucValLo , $1000 ) ;
            SetLength( huff_st.ucSizeHi, $1000 ) ;
            SetLength( huff_st.ucValHi , $1000 ) ;

            code := 0;
            k     := 0 ;
            index := 0 ;

            for i := 1 to 8 do begin
              bits := huff_st.cSize[i] ;
              for j := 1 to bits do begin
                for ml := 0 to Word(1 shl (8 - i) )-1 do begin
                  index := ( code shl (8 - i) ) or ml ;
                  huff_st.ucSizeHi[index] := Word( i );
                  huff_st.ucValHi[index] := huff_st.cVal[k];
                end;
                inc( k    ) ;
                inc( code );
              end ;

              code := code shl 1;
            end ;

            for ml := index + 1 to 255 do
              huff_st.ucSizeHi[ml] := 0 ;

            for i := 9 to 16 do begin
              bits := huff_st.cSize[i] ;
              for j := 1 to bits do begin
                for ml := 0 to Word(1 shl (16 - i) )-1 do begin
                  index := (( code shl (16 - i) ) or ml) and $FFF ;
                  huff_st.ucSizeLo[index] := Word( i );
                  huff_st.ucValLo[index] := huff_st.cVal[k];
                end;
                inc( k    ) ;
                inc( code );
              end ;

              if i < 16 then
                code := code shl 1;
            end ;
        end ;
      end ;
    end ;
  end;

  procedure T_jpegdecoder.fset_decodeMode(
    const _mode : Integer
  ) ;
  begin
    FdecodeMode := _mode ;
  end ;

//==============================================================================
// TGIS_JPEGDecoder
//==============================================================================

  constructor TGIS_JPEGDecoder.Create ;
  begin
    inherited ;
    odecoder := T_jpegdecoder.Create() ;
    T_jpegdecoder(odecoder).FMemoryThreshold :=  DEFAULT_MEMORY_THRESHOLD_JPG ;
    T_jpegdecoder(odecoder).FNativeRequested :=  False ;
  end;

  procedure TGIS_JPEGDecoder.doDestroy ;
  begin
    FreeObject( odecoder ) ;
    inherited ;
  end ;

  procedure TGIS_JPEGDecoder.LoadFromFile(
    const _path : String
  ) ;
  var
    decoder : T_jpegdecoder ;
  begin
    decoder := T_jpegdecoder( odecoder ) ;
    FreeObject( decoder.oStream ) ;
    decoder.oStream := TGIS_BufferedFileStream.Create( _path,
                                              TGIS_StreamMode.Read ) ;
  end;

  procedure TGIS_JPEGDecoder.LoadFromStream(
    const _strm  : TStream
  ) ;
  var
    decoder : T_jpegdecoder ;
  begin
    decoder := T_jpegdecoder( odecoder ) ;
    FreeObject( decoder.oStream ) ;
    decoder.oStream := TGIS_MemoryStream.Create ;
    decoder.oStream.CopyFrom( _strm, _strm.Size - _strm.Position ) ;
    decoder.oStream.Position := 0 ;
  end;

  procedure TGIS_JPEGDecoder.LoadDefaultTables( const _quality : Double ) ;
  begin
    T_jpegdecoder( odecoder ).loadDefaultTables( _quality ) ;
  end;

  function TGIS_JPEGDecoder.Initialize( const _mode : TGIS_LayerDormantMode )
                                      : Integer ;
  begin
    Result :=  T_jpegdecoder( odecoder ).doInitialize(_mode) ;
  end;

  function TGIS_JPEGDecoder.Reinitialize(const _mode : TGIS_LayerDormantMode )
                                      : Integer ;
  begin
    Result :=  T_jpegdecoder( odecoder ).doReinitialize(_mode ) ;
  end;

  //  Scale setting
  procedure  TGIS_JPEGDecoder.fset_Scale(
    const _sc : Integer
  ) ;
  begin
    if assigned(odecoder) then
      T_jpegdecoder( odecoder ).Scale := _sc ;
  end ;

   //  Scale getting
  function  TGIS_JPEGDecoder.fget_Scale
    : Integer ;
  begin
    if assigned(odecoder) then
      Result := T_jpegdecoder( odecoder ).Scale
    else
      Result := 1 ;
  end ;

   // Precision (bits per band) getting
  function  TGIS_JPEGDecoder.fget_Precision
    : Integer ;
  begin
    if assigned(odecoder) then
      Result := T_jpegdecoder( odecoder ).arImagePrecision
    else
      Result := 8 ;
  end ;


    //  JPEG in tiff decoding
  procedure  TGIS_JPEGDecoder.fset_JpegInTiff(
    const _it : Boolean
  ) ;
  begin
    if assigned(odecoder) then
      T_jpegdecoder( odecoder ).JpegInTiff := _it ;
  end ;


   //  Pixels (in millions) in memory limit
  procedure  TGIS_JPEGDecoder.fset_MemoryThreshold(
    const _mt : Integer
  ) ;
  begin
    if assigned(odecoder) then
      T_jpegdecoder( odecoder ).FMemoryThreshold := _mt ;
  end ;

   //  Pixels (in millions) in memory limit
  function  TGIS_JPEGDecoder.fget_MemoryThreshold
    : Integer ;
  begin
    if assigned(odecoder) then
      Result := T_jpegdecoder( odecoder ).FMemoryThreshold
    else
      Result := DEFAULT_MEMORY_THRESHOLD_JPG ;
  end ;

  // JPEG decoder initialization checking
  function  TGIS_JPEGDecoder.fget_JpegInitialized
    : Boolean ;
  begin
    if assigned(odecoder) then
      Result := T_jpegdecoder( odecoder ).JpegInitialized
    else
      Result := False ;
  end ;

  //  Pure RGB decoding setting
  procedure  TGIS_JPEGDecoder.fset_ComponentsJpeg(
    const _componentsJpeg : Integer
  ) ;
  begin
    if assigned(odecoder) then
      T_jpegdecoder( odecoder ).ComponentsJpeg := _componentsJpeg ;
  end ;

  // Set decode mode (RGB,GRAYSCALE).
  procedure TGIS_JPEGDecoder.fset_decodeMode(
    const _mode : Integer
  ) ;
  begin
    T_jpegdecoder( odecoder ).decodeMode := _mode ;
  end ;

  // Set for native values longer than 8 bits per band
  procedure  TGIS_JPEGDecoder.fset_NativeRequsted ( const _nrq : Boolean ) ;
  begin
    T_jpegdecoder( odecoder ).FNativeRequested := _nrq ;
  end ;

  function  TGIS_JPEGDecoder.Width
    : Integer ;
  begin
    Result := T_jpegdecoder( odecoder ).stdecoder.iWidth ;
  end;

  function TGIS_JPEGDecoder.Height
    : Integer ;
  begin
    Result := T_jpegdecoder( odecoder ).stdecoder.iHeight ;
  end;

  function TGIS_JPEGDecoder.SubFormatInfo : String ;
  begin
    if assigned(odecoder) then begin
      case T_jpegdecoder( odecoder ).ComponentsJpeg of
        JPEG_YBR      : Result := 'YBR' ;
        JPEG_BGR      : Result := 'BGR' ;
        JPEG_RGB      : Result := 'RGB' ;
        JPEG_GRAYSCAL : Result := 'Grayscale' ;
        JPEG_ARGB     : Result := 'ARGB' ;
        JPEG_CMYK     : Result := 'CMYK'
      end;
    end
    else
      Result := '' ;
  end ;

  function TGIS_JPEGDecoder.DecodeLine(
    const _buffer     : TBytes  ;
    const _buferIndex : Integer ;
    const _line       : Integer ;
    const _lskip      : Integer ;
    const _pixels     : Integer
  ) : Integer ;
  begin
    Result := T_jpegdecoder( odecoder ).decodeLine(
                _buffer,
                _buferIndex,
                _line,
                _lskip,
                _pixels
              ) ;
  end;

  procedure TGIS_JPEGDecoder.Alive ;
  begin
    T_jpegdecoder( odecoder ).doAlive ;
  end ;

  procedure TGIS_JPEGDecoder.Dormant ;
  begin
    T_jpegdecoder( odecoder ).doDormant ;
  end ;

//==================================== END =====================================
end.
