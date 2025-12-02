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
  Internal part of CADRG-file implementation.

  CADRG support was written by Tim Ranger for the Canadian Department of
  National Defence and is provided 'as-is' without warranty. DND assumes
  no liability for damages or financial loss incurred in the use of this
  software.
}

{$IFDEF DCC}
  unit GisFileCADRG ;
  {$HPPEMIT '#pragma link "GisFileCADRG"'}
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
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    System.Math,
    System.IoUtils,
    System.DateUtils,
    System.Types,
    System.Generics.Collections,
    System.Generics.Defaults,
    Data.DB,

    GisInternals,
    GisFunctions,
    GisTypes,
    GisTypesUI,
    GisStreams,
    GisClasses,
    GisRtl,
    GisResource;
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

var
  {#GENDOC:HIDE}
  PathCADRG : String ;

const
  // Fill color for very small scales
    {#GENDOC:HIDE}
    FILL_COLOR = $FFA52A2A ; //brown

  // NITF Constants
    {#GENDOC:HIDE}
    CADRG_NTIF2X_IDENTIFIER = 'NITF02.';
    {#GENDOC:HIDE}
    CADRG_NTIF20_IDENTIFIER = 'NITF02.00';
    {#GENDOC:HIDE}
    CADRG_NTIF21_IDENTIFIER = 'NITF02.10';

  // Table of Contents constants
    {#GENDOC:HIDE}
    CADRG_TOC_NOT_NTIF_OFFSET = 0;

  // This parser currently supports CADRG and CIB. Create some constants in case
  // we extend it to accommodate others later
    {#GENDOC:HIDE}
    CADRG_PRODUCT_DATA_TYPE_CADRG = 0 ;
    {#GENDOC:HIDE}
    CADRG_PRODUCT_DATA_TYPE_CIB   = 1 ;
    {#GENDOC:HIDE}
    CADRG_CADRG_TAG               = 'CADRG' ;
    {#GENDOC:HIDE}
    CADRG_CIB_TAG                 = 'CIB'   ;

  // Location constants
    {#GENDOC:HIDE}
    CADRG_LOC_HEADER_SECTION = 128;
    {#GENDOC:HIDE}
    CADRG_LOC_LOCATION_SECTION = 129;
    {#GENDOC:HIDE}
    CADRG_LOC_COVERAGE_SECTION = 130;
    {#GENDOC:HIDE}
    CADRG_LOC_COMPRESSION_SECTION = 131;
    {#GENDOC:HIDE}
    CADRG_LOC_COMPRESSION_LOOKUP_SUBSECTION = 132;
    {#GENDOC:HIDE}
    CADRG_LOC_COMPRESSION_PARAMETER_SUBSECTION = 133;
    {#GENDOC:HIDE}
    CADRG_LOC_COLORGRAY_SECTION_SUBHEADER = 134;
    {#GENDOC:HIDE}
    CADRG_LOC_COLORMAP_SUBSECTION = 135;
    {#GENDOC:HIDE}
    CADRG_LOC_IMAGE_DESCR_SUBHEADER = 136;
    {#GENDOC:HIDE}
    CADRG_LOC_IMAGE_DISPLAY_PARAM_SUBHEADER = 137;
    {#GENDOC:HIDE}
    CADRG_LOC_MASK_SUBSECTION = 138;
    {#GENDOC:HIDE}
    CADRG_LOC_COLOR_CONVERTER_SUBSECTION = 139;
    {#GENDOC:HIDE}
    CADRG_LOC_SPATIAL_DATA_SUBSECTION = 140;
    {#GENDOC:HIDE}
    CADRG_LOC_ATTRIBUTE_SECTION_SUBHEADER = 141;
    {#GENDOC:HIDE}
    CADRG_LOC_ATTRIBUTE_SUBSECTION = 142;
    {#GENDOC:HIDE}
    CADRG_LOC_EXPLICIT_AREAL_TABLE = 143;
    {#GENDOC:HIDE}
    CADRG_LOC_RELATED_IMAGE_SECTION_SUBHEADER = 144;
    {#GENDOC:HIDE}
    CADRG_LOC_RELATED_IMAGE_SUBSECTION = 145;
    {#GENDOC:HIDE}
    CADRG_LOC_REPLACE_UPDATE_SECTION_SUBHEADER = 146;
    {#GENDOC:HIDE}
    CADRG_LOC_REPLACE_UPDATE_TABLE = 147;
    {#GENDOC:HIDE}
    CADRG_LOC_BOUNDARY_SECTION_SUBHEADER = 148;
    {#GENDOC:HIDE}
    CADRG_LOC_BOUNDARY_RECTANGLE_TABLE = 149;
    {#GENDOC:HIDE}
    CADRG_LOC_FRAME_FILE_INDEX_SUBHEADER = 150;
    {#GENDOC:HIDE}
    CADRG_LOC_FRAME_FILE_INDEX_SUBSECTION = 151;
    {#GENDOC:HIDE}
    CADRG_LOC_COLOR_TABLE_SECTION_SUBHEADER = 152;
    {#GENDOC:HIDE}
    CADRG_LOC_COLOR_TABLE_INDEX_RECORD = 153;
    {#GENDOC:HIDE}
    CADRG_LOC_PRODUCT_DATA_TYPE = 481 ;
    {#GENDOC:HIDE}
    CADRG_TRANSPARENT_COLOR = $00010816  ;

  // Frame Constants
    {#GENDOC:HIDE}
    CADRG_FRAME_NOT_NTIF_OFFSET = 0;

    {#GENDOC:HIDE}
    CADRG_COLOR_TABLE_ENTRIES = 217;

  // Used to control frame cache size
    {#GENDOC:HIDE}
    CADRG_FRAME_CACHE_SIZE = 12 ;

    {#GENDOC:HIDE}
    CADRG_CACHE_ENTRY_AVAILABLE = 4294967295; // Highest value for FrameIndexID
                                              // is a free in cache indicator

    {$IFDEF OXYGENE}
      {#GENDOC:HIDE}
      CADRG_RGBMBITMAP_DIMSIZE = 1536 ;
      {#GENDOC:HIDE}
      CADRG_RGBMBITMAP_SIZE = CADRG_RGBMBITMAP_DIMSIZE * CADRG_RGBMBITMAP_DIMSIZE ;
    {$ENDIF}

type
  { Component Location Record
    Composite used to decode the location section
  }
  {#GENDOC:HIDE}
  TGIS_FileCADRG_ComponentLocationRec = packed record
    ID       : Word     ;
    Length   : Cardinal ;
    Location : Cardinal ;
  end ;

  { Boundary Location Record
    Used to decode location of Boundary records
    RecLen = 8
  }
  {#GENDOC:HIDE}
  TGIS_FileCADRG_BoundaryLocationRec = packed record
    TableOffset     : Cardinal ;
    NumberOfRecords : Word     ;
    RecordLength    : Word     ;
    {$IFDEF OXYGENE}
      function Read ( _stream : TGIS_Stream ) : Integer ;
    {$ENDIF}
  end ;

  { Frame Location Record
    Used to decode location of Frame records
    RecLen = 13
  }
  {#GENDOC:HIDE}
  TGIS_FileCADRG_FrameLocationRec = packed record
    HighestSecurityClassification   : Byte     ;
    TableOffset                     : Cardinal ;
    NumberOfFrameFileRecords        : Cardinal ;
    NumberOfpathNameRecords         : Word     ;
    FrameFileRecordLength           : Word     ;
    {$IFDEF OXYGENE}
      function Read ( _stream : TGIS_Stream ) : Integer ;
    {$ENDIF}
  end ;

  { Frame File Index Record
    RecLen = 33
  }
  {#GENDOC:HIDE}
  TGIS_FileCADRG_FrameFileIndexRec = {$IFDEF OXYGENE} public {$ENDIF} packed record
    BoundaryRectangleNumber           : Word ;
    FrameLocationRowNumber            : Word ;
    FrameLocationColumnNumber         : Word ;
    PathnameRecordOffset              : Cardinal ;
    {$IFDEF OXYGENE}
      FrameFileName                   : String ;
    {$ELSE}
      FrameFileName                   : array [1..12] of Byte ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      GeographicLocation            : String ;
    {$ELSE}
      GeographicLocation            : array [1.. 6] of Byte ;
    {$ENDIF}
    FrameFileSecurityClassification : Byte ;
    {$IFDEF OXYGENE}
      FrameFileSecurityCountryCode  : String ;
    {$ELSE}
      FrameFileSecurityCountryCode  : array [1.. 2] of Byte ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      FrameFileSecurityReleaseMarking : String ;
    {$ELSE}
      FrameFileSecurityReleaseMarking : array [1.. 2] of Byte ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      function Read ( _stream : TGIS_Stream ) : Integer ;
    {$ENDIF}
  end ;

  { Pathname Record
    Used to store path component for frame file index entries
  }
  {#GENDOC:HIDE}
  TGIS_FileCADRG_PathnameRec = packed record
    Offset : Cardinal ; // Int64;
    Length : Word     ;
    Path   : TBytes   ;
  end ;

  { Boundary Rectangle Record
    RecLen = 132
  }
  {#GENDOC:HIDE}
  TGIS_FileCADRG_BoundaryRectangleRec = {$IFDEF OXYGENE} public {$ENDIF} packed record
    {$IFDEF OXYGENE}
      ProductDataType         : array of Byte ;
    {$ELSE}
      ProductDataType         : array [1.. 5] of Byte ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      CompressionRatio        : array of Byte ;
    {$ELSE}
      CompressionRatio        : array [1.. 5] of Byte ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      ScaleOrResolution       : array of Byte ;
    {$ELSE}
      ScaleOrResolution       : array [1..12] of Byte ;
    {$ENDIF}
    Zone                      : Byte ;
    {$IFDEF OXYGENE}
      Producer                : array of Byte ;
    {$ELSE}
      Producer                : array [1.. 5] of Byte ;
    {$ENDIF}
    NWUpperLeftLatitude         : Double ;
    NWUpperLeftLongitude        : Double ;
    SWLowerLeftLatitude         : Double ;
    SWLowerLeftLongitude        : Double ;
    NEUpperRightLatitude        : Double ;
    NEUpperRightLongitude       : Double ;
    SELowerRightLatitude        : Double ;
    SELowerRightLongitude       : Double ;
    NSVerticalResolution        : Double ;
    EWHorizontalResolution      : Double ;
    LatitudeVerticalInterval    : Double ;
    LongitudeHorizontalInterval : Double ;
    NumberOfFramesNS            : Cardinal ;
    NumberOfFramesEW            : Cardinal ;
    {$IFDEF OXYGENE}
      function Read ( _stream : TGIS_Stream ) : Integer ;
    {$ENDIF}
  end ;

  {
    MapSeries elsement
  }
  {#GENDOC:HIDE}
  TGIS_FileCADRG_Map = {$IFDEF OXYGENE} public {$ENDIF} class
    public
      Scale         : Integer  ; // 5
      ProductName   : String   ; // 6
      ProductNameId : Integer  ; // = 0 - CADRG; 1 - CIB
      SeriesCode    : String   ; // 4
      SeriesCodeId  : Integer  ; // 4
      Enabled       : Boolean  ;
  end ;

  {
    MapSeries component
  }
  {#GENDOC:HIDE}
  TGIS_FileCADRG_MapSeries = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
  private // properties internal values

    /// <summary>
    ///   List of cadrg maps series.
    /// </summary>
    FItems : TGIS_ObjectList ;
  protected
    procedure doDestroy ; override;
  public // constructors
    constructor Create ;
  public // API functons
    function  GetSeriesCodeId(const _s : String) : Integer ;
    function  AddUniqueMap(const _mc : TGIS_FileCADRG_Map) : TGIS_FileCADRG_Map ;
    function  InSeries(const _mc : TGIS_FileCADRG_Map) : Boolean ;
    procedure AddUniqueMapSeries(const _mcs : TGIS_FileCADRG_MapSeries) ;
    procedure SynchronizeEnabled(const _mcs : TGIS_FileCADRG_MapSeries) ;
  public // properties
    property Items : TGIS_ObjectList read FItems ;
  end ;

  { Describes one frame data CADRG or CIB file .
  }
  {#GENDOC:HIDE}
  TGIS_FileCADRG_SimpleFrame = {$IFDEF OXYGENE} public {$ENDIF} class
    public
      Path                  : String  ;
      SeriesCode            : String  ;
      ProductName           : String  ;
      Scale                 : Integer ;
      SWLowerLeftLatitude   : Double  ;
      SWLowerLeftLongitude  : Double  ;
      NEUpperRightLatitude  : Double  ;
      NEUpperRightLongitude : Double  ;
      Available             : Boolean ;
  end ;

  {#GENDOC:HIDE}
  TGIS_FileCADRG_ReadFrameEvent = {$IFDEF OXYGENE} public {$ENDIF} procedure(
          _sender : TObject ;
    var   _frame  : TGIS_FileCADRG_SimpleFrame ;
    var   _eof    : Boolean
  ) of object ;

  { Boundary Rectangle Record for data base
  }
  {#GENDOC:HIDE}
  TGIS_FileCADRG_Sublayer = {$IFDEF OXYGENE} public {$ENDIF} class
    public
      SWLowerLeftLatitude         : Double ;
      SWLowerLeftLongitude        : Double ;
      NEUpperRightLatitude        : Double ;
      NEUpperRightLongitude       : Double ;
      NSVerticalResolution        : Double ;
      EWHorizontalResolution      : Double ;
      LatitudeVerticalInterval    : Double ;
      LongitudeHorizontalInterval : Double ;
      NumberOfFramesNS            : Integer ;
      NumberOfFramesEW            : Integer ;
      FrameStartIndex             : Integer ;
      FrameStopIndex              : Integer ;
      MapCADRG                    : TGIS_FileCADRG_Map ;
  end ;

  { Header Section record
  }
  {#GENDOC:HIDE}
  TGIS_FileCADRG_HeaderRec = packed record
    Endian : Byte ;
    HeaderSectionLength : Word ;
    {$IFDEF OXYGENE}
      FileName : array of Byte ;
    {$ELSE}
      FileName : array [1..12] of Byte ;
    {$ENDIF}
    NRU : Byte ;
    {$IFDEF OXYGENE}
      StandardNumber : array of Byte ;
    {$ELSE}
      StandardNumber : array [1..15] of Byte ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      StandardDate : array of Byte ;
    {$ELSE}
      StandardDate : array [1..8] of Byte ;
    {$ENDIF}
    SecurityClassification: Byte;
    {$IFDEF OXYGENE}
      SecurityCountryCode : array of Byte ;
    {$ELSE}
      SecurityCountryCode : array [1..2] of Byte ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      SecurityReleaseMarking : array of Byte ;
    {$ELSE}
      SecurityReleaseMarking : array [1..2] of Byte ;
    {$ENDIF}
    LocationPhysical : Cardinal ;
    {$IFDEF OXYGENE}
      function Read ( _stream : TGIS_Stream ) : Integer ;
    {$ENDIF}
  end ;

  { Frame file related records
    Coverage Section record
    RecLen=96
  }
  {$IFDEF OXYGENE}
    {#GENDOC:HIDE}
    TGIS_FileCADRG_CoverageRec = {$IFDEF GIS_PACKED} packed {$ENDIF} record
  {$ELSE}
    {#GENDOC:HIDE}
    TGIS_FileCADRG_CoverageRec = packed record
  {$ENDIF}
    NWUpperLeftLatitude        : Double ;
    NWUpperLeftLongitude       : Double ;
    SWLowerLeftLatitude        : Double ;
    SWLowerLeftLongitude       : Double ;
    NEUpperRightLatitude       : Double ;
    NEUpperRightLongitude      : Double ;
    SELowerRightLatitude       : Double ;
    SELowerRightLongitude      : Double ;
    NSVerticalResolution       : Double ;
    EWHorizontalResolution     : Double ;
    LatitudeVerticalInterval   : Double ;
    LongitudeHorizontalInterval: Double ;
    {$IFDEF OXYGENE}
      {$IFDEF OXYGENE}
        function Read ( _stream : TGIS_Stream ) : Integer ;
      {$ELSE}
        function Read ( _stream : TStream ) : Integer ;
      {$ENDIF}
    {$ENDIF}
  end ;

  { Compression Section Header record
    RecLen=6
  }
  {#GENDOC:HIDE}
  TGIS_FileCADRG_CompressionHeaderRec = packed record
    CompressionAlgorithmId                    : Word ;
    NumberOfCompressionLookupOffsetRecords    : Word ;
    NumberOfCompressionParameterOffsetRecords : Word ;
    {$IFDEF OXYGENE}
      {$IFDEF OXYGENE}
        function Read ( _stream : TGIS_Stream ) : Integer ;
      {$ELSE}
        function Read ( _stream : TStream ) : Integer ;
      {$ENDIF}
    {$ENDIF}
  end ;

  { Image Description Section Header record
    RecLen=28
  }
  {#GENDOC:HIDE}
  TGIS_FileCADRG_ImageDescriptionHeaderRec = packed record
    NumberOfSpectralGroups               : Word ;
    NumberOfSubframeTables               : Word ;
    NumberOfSpectralBandTables           : Word ;
    NumberOfSpectralBandLinesPerImageRow : Word ;
    NumberOfSubframesEastWestDirection   : Word ;
    NumberOfSubframesNorthSouthDirection : Word ;
    NumberOfOutputColumnsPerSubframe     : Cardinal ;
    NumberOfOutputRowsPerSubframe        : Cardinal ;
    SubframeMaskTableOffset              : Cardinal ;
    TransparencyMaskTableOffset          : Cardinal ;
    {$IFDEF OXYGENE}
      {$IFDEF OXYGENE}
        function Read ( _stream : TGIS_Stream ) : Integer ;
      {$ELSE}
        function Read ( _stream : TStream ) : Integer ;
      {$ENDIF}
    {$ENDIF}
  end ;

  { Image Display Parameters Section Header record
    RecLen=9
  }
  {#GENDOC:HIDE}
  TGIS_FileCADRG_ImageDisplayParametersHeaderRec = packed record
    NumberOfImageRows        : Cardinal ;
    NumberOfImageCodesPerRow : Cardinal ;
    ImageCodeBitLength       : Byte     ;
    {$IFDEF OXYGENE}
      {$IFDEF OXYGENE}
        function Read ( _stream : TGIS_Stream ) : Integer ;
      {$ELSE}
        function Read ( _stream : TStream ) : Integer ;
      {$ENDIF}
    {$ENDIF}
  end ;

  { Compression Lookup record
    RecLen=14
  }
  {#GENDOC:HIDE}
  TGIS_FileCADRG_CompressionLookupRecord = packed record
    LookupTableId         : Word     ;
    NumberOfLookupRecords : Cardinal ;
    NumberOfValues        : Word     ;
    LookupValueBitLength  : Word     ;
    LookupTableOffset     : Cardinal ;
    {$IFDEF OXYGENE}
      {$IFDEF OXYGENE}
        function Read ( _stream : TGIS_Stream ) : Integer ;
      {$ELSE}
        function Read ( _stream : TStream ) : Integer ;
      {$ENDIF}
    {$ENDIF}
  end ;

  { Color/Grayscale section header record
    RecLen=14
  }
  {#GENDOC:HIDE}
  TGIS_FileCADRG_ColorHeaderRec = packed record
    NumberOfColorOffsetRecords          : Byte ;
    NumberOfColorConverterOffsetRecords : Byte ;
    ExternalColorFileName             : array [0..11] of Byte ;
    {$IFDEF OXYGENE}
      function Read ( _stream : TGIS_Stream ) : Integer ;
    {$ENDIF}
  end ;

  { ColorMap section header record
    RecLen=6
  }
  {#GENDOC:HIDE}
  TGIS_FileCADRG_ColorMapHeaderRec = packed record
    ColorMapOffsetTableOffset : Cardinal ;
    ColorOffsetRecordLength   : Word     ;
    {$IFDEF OXYGENE}
      function Read ( _stream : TGIS_Stream ) : Integer ;
    {$ENDIF}
  end ;

  { ColorOffset record
    RecLen=17
  }
  {#GENDOC:HIDE}
  TGIS_FileCADRG_ColorOffsetRec = packed record
    ColorTableId          : Word     ;
    NumberOfColorRecords  : Cardinal ;
    ColorElementLength    : Byte     ;
    HistogramRecordLength : Word     ;
    ColorTableOffset      : Cardinal ;
    HistogramTableOffset  : Cardinal ;
    {$IFDEF OXYGENE}
      function Read ( _stream : TGIS_Stream ) : Integer ;
    {$ENDIF}
  end ;

  { Color Converter section header record
    RecLen=8
  }
  {#GENDOC:HIDE}
  TGIS_FileCADRG_ColorConverterHeaderRec = packed record
    ColorConverterOffsetTableOffset  : Cardinal ;
    ColorConverterOffsetRecordLength : Word     ;
    ColorConverterRecordLength       : Word     ;
    {$IFDEF OXYGENE}
      function Read ( _stream : TGIS_Stream ) : Integer ;
    {$ENDIF}
  end ;

  { Color table entry
    RecLen=4
  }
  {#GENDOC:HIDE}
  TGIS_FileCADRG_RGBMRec = {$IFDEF OXYGENE} public {$ENDIF} packed record
    Red        : Byte ;
    Green      : Byte ;
    Blue       : Byte ;
    Monochrome : Byte ;
    {$IFDEF OXYGENE}
      function Read ( _stream : TGIS_Stream ) : Integer ;
    {$ENDIF}
  end ;

  {$IFDEF OXYGENE}
    { Multidimensional rgbm bitmap type definition is a one-dimension array
      because of better efficiency
    }
    {#GENDOC:HIDE}
    TGIS_FileCADRG_RGBMBitmap = array [0..CADRG_RGBMBITMAP_SIZE-1] of TGIS_FileCADRG_RGBMRec;
  {$ELSE}
    { Multidimensional rgbm bitmap type definition
    }
    {#GENDOC:HIDE}
    TGIS_FileCADRG_RGBMBitmap = array [0..1535,0..1535] of TGIS_FileCADRG_RGBMRec;
  {$ENDIF}

  { Frame structure that contains all datastructures required to fully decode
    frame data to a bitmap
  }
  {#GENDOC:HIDE}
  TGIS_FileCADRG_Frame = record
    FrameFileIndexID    : Cardinal ; // This is an index into the FrameFileIndex
                                     // array used to map this frame to a member
                                     // of the FrameFileIndex array
    IsFrameValid        : Boolean ;
    Coverage            : TGIS_FileCADRG_CoverageRec ;
    CompressionTable  : array [0..65535] of Byte ;
    AllSubframesPresent : Boolean ;
    {$IFNDEF JAVA}
      SubFrameMasked      : array [0..5,0..5] of Boolean ;
    {$ELSE}
      SubFrameMasked      : array [0..5] of array [0..5] of Boolean ;
    {$ENDIF}
    SubFrameData      : array [0..221183] of Byte ;
    NumberOfColors      : Byte ;
    ColorTable          : array [0..CADRG_COLOR_TABLE_ENTRIES-1]
                          of TGIS_FileCADRG_RGBMRec;
  end ;

  {$IFNDEF OXYGENE}
  {$IFNDEF OXYGENE}
    { FrameEntry Record
      This record is a composite used to consolidate the various frame related
      structures and lookup tables to tie all frame related data into a single
      structure. This structure is used in public methods to retrieve and consolidated
      frame related information.
    }
    {#GENDOC:HIDE}
    TGIS_FileCADRG_FrameEntry = record
      // This is an index into the FrameFileIndex array
        FrameFileIndexID  : Cardinal ;
      // Main FrameFileIndex information
        FrameFileIndex    : TGIS_FileCADRG_FrameFileIndexRec;
      // This is the BoundaryRectangle information
        BoundaryRectangle : TGIS_FileCADRG_BoundaryRectangleRec;
      // This is the pathname information
        Pathname          : String;
    end ;
  {$ENDIF}
  {$ENDIF}

  /// <summary>
  ///   Encapsulation of CADRG-file low-level access.
  /// </summary>
  {#GENDOC:HIDE}
  TGIS_FileCADRGDecoder = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
    private
      FTransparentRedValue   : Byte ;
      FTransparentGreenValue : Byte ;
      FTransparentBlueValue  : Byte ;

      { Gets populated with the Decoded product data type constants. }
      FProductDataType       : Byte ;

      { Ivent reading support}
      FOnReadFrame           : TGIS_FileCADRG_ReadFrameEvent ;

    private
      isValid         : Boolean ;
      doByteSwapping  : Boolean ;
      rpfDirectory    : String  ;

      { Dynamic array used to hold Pathname records. }
      pathNameRecords : array of TGIS_FileCADRG_PathnameRec;

      tocHeader       : TGIS_FileCADRG_HeaderRec;

      { Contains date/time of last access for an entry in the frame cache. }
        frameCacheTimes : array [0..CADRG_FRAME_CACHE_SIZE] of TDateTime;
      { Stores frame data using a MRU technique. }
//        frameCache      : array [0..CADRG_FRAME_CACHE_SIZE] of
        frameCache      : array  of
                          TGIS_FileCADRG_Frame;

    private
      procedure loadToc           ;
      procedure loadDBToc         ;
      procedure loadFrame         ( _frameFileIndex : Cardinal
                                  ) ;
      procedure loadDBFrame       ( _frameFileIndex : Cardinal
                                  ) ;
      procedure loadFrameViaCache ( _frameFileIndex : Cardinal
                                  ) ;
      function  getFrameFilePath  ( _pathnameRecordOffset : Cardinal
                                  ) : String ;
      procedure checkSwap         ( _endian: Byte
                                  ) ;
      procedure doSwapW           ( var _word   : Word
                                  ) ; overload;
      procedure doSwapC           ( var _cardinal  : Cardinal
                                  ) ; overload;
      procedure doSwapD           ( var _double    : Double
                                  ) ; overload;
      procedure parseLocations  ( _fs            : TGIS_FileStream;
                                  var _locations : array of
                                                   TGIS_FileCADRG_ComponentLocationRec;
                                  _locationCount : Word
                                ) ;

  public

    Dct : TDictionary<Cardinal, Integer> ;
    { Current frame index from frame cache. }
      Frame : Integer ;
    { Dynamic array used to hold FrameFileIndex records. }
      FrameFileIndexRecords : array of TGIS_FileCADRG_FrameFileIndexRec ;
    { Dynamic array used to hold BoundaryRectangle records. }
      BoundaryRectangleRecords : array of TGIS_FileCADRG_BoundaryRectangleRec ;

    { Data Base sublayer list. }
      BDSubLayersList     : TGIS_ObjectList ;
    { Data Base single frames list. }
      BDSimpleFramesList  : TGIS_ObjectList ;
    { Mapseries list. }
      MapSeriesCadrg : TGIS_FileCADRG_MapSeries ;

    { Displaying layer. }
      CurrentLayer        : TGIS_FileCADRG_Sublayer ;
    protected
      procedure doDestroy       ; override;
    public
      constructor Create        ( const _path    : String         ) ; overload;
      constructor Create        ( const _ef : TGIS_FileCADRG_ReadFrameEvent ) ; overload;
      procedure   DoReload      ;
      function  GetFrameRowRGB( const _frmFileIndex : Cardinal ;
                                const _lineNumber   : Cardinal ;
                                const _start        : Integer  ;
                                const _count        : Integer  ;
                                const _buffer       : TBytes   ;
                                const _offset       : Integer
                              ) : Integer ;
      function  GetFrameRowARGB( const _frmFileIndex : Cardinal ;
                                 const _lineNumber   : Cardinal ;
                                 const _start        : Integer  ;
                                 const _count        : Integer  ;
                                 const _buffer       : TGIS_Pixels ;
                                 const _offset       : Integer
                               ) : Integer ;

      function  GetScaledFrameBlockARGB(
                                     const _frmIndex   : Integer ;
                                     const _buffer     : TGIS_Pixels  ;
                                     const _bwidth     : Integer ;
                                     const _bhidth     : Integer ;
                                     const _left       : Integer ;
                                     const _top        : Integer ;
                                     const _lineStart  : Integer ;
                                     const _lineCount  : Integer ;
                                     const _pixStart   : Integer ;
                                     const _pixCount   : Integer ;
                                     const _scale_y    : Single ;
                                     const _scale_x    : Single
                                   ) : TPoint;
      function  FillFrameBlockARGB(
                                     const _frmIndex   : Integer ;
                                     const _buffer     : TGIS_Pixels  ;
                                     const _bwidth     : Integer ;
                                     const _bhidth     : Integer ;
                                     const _left       : Integer ;
                                     const _top        : Integer ;
                                     const _lineStart  : Integer ;
                                     const _lineCount  : Integer ;
                                     const _pixStart   : Integer ;
                                     const _pixCount   : Integer ;
                                     const _scale_y    : Single ;
                                     const _scale_x    : Single
                                   ) : TPoint;


    public // properties
      property ProductType      : Byte read FProductDataType       ;
      property TransparentRed   : Byte read FTransparentRedValue   ;
      property TransparentBlue  : Byte read FTransparentBlueValue  ;
      property TransparentGreen : Byte read FTransparentGreenValue ;

   published //events
      /// <event/>
      /// <summary>
      ///   Use this event to read custom frame
      /// </summary>
      property ReadFrameEvent      : TGIS_FileCADRG_ReadFrameEvent
                                     read  FOnReadFrame
                                     write FOnReadFrame ;
  end ;

//##############################################################################
implementation
{ TGIS_FileCADRGDecoder }

{$IFDEF DCC}
  type
    TStringSearchOption = (
      soDown        = 0,
      soMatchCase   = 1,
      soWholeWord   = 2
    ) ;
    TStringSearchOptions = set of TStringSearchOption ;
{$ENDIF}

{$IFDEF OXYGENE}
  type
    TValueRelationship = -1..1;

    TStringSearchOptions = flags (
      soDown        = 0,
      soMatchCase   = 1,
      soWholeWord   = 2
    ) ;

  function TGIS_FileCADRG_BoundaryLocationRec.Read(
    _stream : TGIS_Stream
  ) : Integer ;
  begin
    Result := _stream.ReadCardinal( TableOffset,     4 ) +
              _stream.ReadWord( NumberOfRecords, 2 ) +
              _stream.ReadWord( RecordLength,    2 ) ;
  end ;

  function TGIS_FileCADRG_FrameLocationRec.Read (
    _stream : TGIS_Stream
  ) : Integer ;
  begin
    Result := _stream.ReadByte( HighestSecurityClassification, 1 ) +
              _stream.ReadCardinal( TableOffset,                   4 ) +
              _stream.ReadCardinal( NumberOfFrameFileRecords,      4 ) +
              _stream.ReadWord( NumberOfpathNameRecords,       2 ) +
              _stream.ReadWord( FrameFileRecordLength,         2 ) ;
  end ;

  function TGIS_FileCADRG_FrameFileIndexRec.Read(
    _stream : TGIS_Stream
  ) : Integer ;
  var
    frame_file_name : array[0..11] of Byte ;
    geographic_location : array[0..5] of Byte ;
    frame_file_security_country_code : array[0..1] of Byte ;
    frame_file_security_release_marking : array[0..1] of Byte ;
  begin
    Result := _stream.ReadWord( BoundaryRectangleNumber,            2 ) +
              _stream.ReadWord( FrameLocationRowNumber,              2 ) +
              _stream.ReadWord( FrameLocationColumnNumber,           2 ) +
              _stream.ReadCardinal( PathnameRecordOffset,                4 ) +
              _stream.Read( frame_file_name,                    12 ) +
              _stream.Read( geographic_location,                 6 ) +
              _stream.ReadByte( FrameFileSecurityClassification,     1 ) +
              _stream.Read( frame_file_security_country_code,    2 ) +
              _stream.Read( frame_file_security_release_marking, 2 ) ;
    FrameFileName := ConvertAnsiString( frame_file_name ) ;
    GeographicLocation := ConvertAnsiString( geographic_location ) ;
    FrameFileSecurityCountryCode := ConvertAnsiString( frame_file_security_country_code ) ;
    FrameFileSecurityReleaseMarking := ConvertAnsiString( frame_file_security_release_marking ) ;
  end ;

  function TGIS_FileCADRG_BoundaryRectangleRec.Read(
    _stream : TGIS_Stream
  ) : Integer ;
  begin
    SetLength( ProductDataType, 5 ) ;
    SetLength( CompressionRatio, 5 ) ;
    SetLength( ScaleOrResolution, 12 ) ;
    SetLength( Producer, 5 ) ;

Result := _stream.Read( ProductDataType,                       5 ) +
              _stream.Read( CompressionRatio,                  5 ) +
              _stream.Read( ScaleOrResolution,                12 ) +
              _stream.ReadByte( Zone,                          1 ) +
              _stream.Read( Producer,                          5 ) +
              _stream.ReadDouble( NWUpperLeftLatitude,         8 ) +
              _stream.ReadDouble( NWUpperLeftLongitude,        8 ) +
              _stream.ReadDouble( SWLowerLeftLatitude,         8 ) +
              _stream.ReadDouble( SWLowerLeftLongitude,        8 ) +
              _stream.ReadDouble( NEUpperRightLatitude,        8 ) +
              _stream.ReadDouble( NEUpperRightLongitude,       8 ) +
              _stream.ReadDouble( SELowerRightLatitude,        8 ) +
              _stream.ReadDouble( SELowerRightLongitude,       8 ) +
              _stream.ReadDouble( NSVerticalResolution,        8 ) +
              _stream.ReadDouble( EWHorizontalResolution,      8 ) +
              _stream.ReadDouble( LatitudeVerticalInterval,    8 ) +
              _stream.ReadDouble( LongitudeHorizontalInterval, 8 ) +
              _stream.ReadCardinal( NumberOfFramesNS,          4 ) +
              _stream.ReadCardinal( NumberOfFramesEW,          4 ) ;
  end ;

  function TGIS_FileCADRG_HeaderRec.Read(
    _stream : TGIS_Stream
  ) : Integer ;
  begin
    if not assigned( FileName ) then
      SetLength( FileName, 12 ) ;
    if not assigned( StandardNumber ) then
      SetLength( StandardNumber, 15 ) ;
    if not assigned( StandardDate ) then
      SetLength( StandardDate, 8 ) ;
    if not assigned( SecurityCountryCode ) then
      SetLength( SecurityCountryCode, 2 ) ;
    if not assigned( SecurityReleaseMarking ) then
      SetLength( SecurityReleaseMarking, 2 ) ;

    Result := _stream.ReadByte( Endian,                 1 ) +
              _stream.ReadWord( HeaderSectionLength,    2 ) +
              _stream.Read( FileName,              12 ) +
              _stream.ReadByte( NRU,                    1 ) +
              _stream.Read( StandardNumber,        15 ) +
              _stream.Read( StandardDate,           8 ) +
              _stream.ReadByte( SecurityClassification, 1 ) +
              _stream.Read( SecurityCountryCode,    2 ) +
              _stream.Read( SecurityReleaseMarking, 2 ) +
              _stream.ReadCardinal( LocationPhysical,       4 ) ;
  end ;

  function TGIS_FileCADRG_CoverageRec.Read(
    _stream : TGIS_Stream
  ) : Integer ;
  begin
    Result := _stream.ReadDouble( NWUpperLeftLatitude,         8 ) +
              _stream.ReadDouble( NWUpperLeftLongitude,        8 ) +
              _stream.ReadDouble( SWLowerLeftLatitude,         8 ) +
              _stream.ReadDouble( SWLowerLeftLongitude,        8 ) +
              _stream.ReadDouble( NEUpperRightLatitude,        8 ) +
              _stream.ReadDouble( NEUpperRightLongitude,       8 ) +
              _stream.ReadDouble( SELowerRightLatitude,        8 ) +
              _stream.ReadDouble( SELowerRightLongitude,       8 ) +
              _stream.ReadDouble( NSVerticalResolution,        8 ) +
              _stream.ReadDouble( EWHorizontalResolution,      8 ) +
              _stream.ReadDouble( LatitudeVerticalInterval,    8 ) +
              _stream.ReadDouble( LongitudeHorizontalInterval, 8 ) ;
  end ;

  function TGIS_FileCADRG_CompressionHeaderRec.Read(
    _stream : TGIS_Stream
  ) : Integer ;
  begin
    Result := _stream.ReadWord( CompressionAlgorithmId,                    2 ) +
              _stream.ReadWord( NumberOfCompressionLookupOffsetRecords,    2 ) +
              _stream.ReadWord( NumberOfCompressionParameterOffsetRecords, 2 ) ;
  end ;

  function TGIS_FileCADRG_ImageDescriptionHeaderRec.Read(
    _stream : TGIS_Stream
  ) : Integer ;
  begin
    Result := _stream.ReadWord( NumberOfSpectralGroups,               2 ) +
              _stream.ReadWord( NumberOfSubframeTables,               2 ) +
              _stream.ReadWord( NumberOfSpectralBandTables,           2 ) +
              _stream.ReadWord( NumberOfSpectralBandLinesPerImageRow, 2 ) +
              _stream.ReadWord( NumberOfSubframesEastWestDirection,   2 ) +
              _stream.ReadWord( NumberOfSubframesNorthSouthDirection, 2 ) +
              _stream.ReadCardinal( NumberOfOutputColumnsPerSubframe, 4 ) +
              _stream.ReadCardinal( NumberOfOutputRowsPerSubframe,    4 ) +
              _stream.ReadCardinal( SubframeMaskTableOffset,          4 ) +
              _stream.ReadCardinal( TransparencyMaskTableOffset,      4 ) ;
  end ;

  function TGIS_FileCADRG_ImageDisplayParametersHeaderRec.Read(
    _stream : TGIS_Stream
  ) : Integer ;
  begin
    Result := _stream.ReadCardinal( NumberOfImageRows,        4 ) +
              _stream.ReadCardinal( NumberOfImageCodesPerRow, 4 ) +
              _stream.ReadByte( ImageCodeBitLength,           1 ) ;
  end ;

  function TGIS_FileCADRG_CompressionLookupRecord.Read(
    _stream : TGIS_Stream
  ) : Integer ;
  begin
    Result := _stream.ReadWord( LookupTableId,         2 ) +
              _stream.ReadCardinal( NumberOfLookupRecords, 4 ) +
              _stream.ReadWord( NumberOfValues,        2 ) +
              _stream.ReadWord( LookupValueBitLength,  2 ) +
              _stream.ReadCardinal( LookupTableOffset,     4 ) ;
  end ;

  function TGIS_FileCADRG_ColorHeaderRec.Read(
    _stream : TGIS_Stream
  ) : Integer ;
  begin
    Result := _stream.ReadByte( NumberOfColorOffsetRecords,          1 ) +
              _stream.ReadByte( NumberOfColorConverterOffsetRecords, 1 ) +
              _stream.Read( ExternalColorFileName,              12 ) ;
  end ;

  function TGIS_FileCADRG_ColorMapHeaderRec.Read(
    _stream : TGIS_Stream
  ) : Integer ;
  begin
    Result := _stream.ReadCardinal( ColorMapOffsetTableOffset, 4 ) +
              _stream.ReadWord( ColorOffsetRecordLength,   2 ) ;
  end ;

  function TGIS_FileCADRG_ColorOffsetRec.Read(
    _stream : TGIS_Stream
  ) : Integer ;
  begin
    Result := _stream.ReadWord( ColorTableId,          2 ) +
              _stream.ReadCardinal( NumberOfColorRecords,  4 ) +
              _stream.ReadByte( ColorElementLength,    1 ) +
              _stream.ReadWord( HistogramRecordLength, 2 ) +
              _stream.ReadCardinal( ColorTableOffset,      4 ) +
              _stream.ReadCardinal( HistogramTableOffset,  4 ) ;
  end ;

  function TGIS_FileCADRG_ColorConverterHeaderRec.Read(
    _stream : TGIS_Stream
  ) : Integer ;
  begin
    Result := _stream.ReadCardinal( ColorConverterOffsetTableOffset,  4 ) +
              _stream.ReadWord( ColorConverterOffsetRecordLength, 2 ) +
              _stream.ReadWord( ColorConverterRecordLength,       2 ) ;
  end ;

  function TGIS_FileCADRG_RGBMRec.Read(
    _stream : TGIS_Stream
  ) : Integer ;
  begin
    Result := _stream.ReadByte( Red,        1 ) +
              _stream.ReadByte( Green,      1 ) +
              _stream.ReadByte( Blue,       1 ) +
              _stream.ReadByte( Monochrome, 1 ) ;
  end ;

  function CompareDateTime(const A, B: TDateTime): TValueRelationship;
  {$IFNDEF JAVA}
    var
      comp : Integer;
  {$ENDIF}
  begin
    {$IFDEF JAVA OR ISLAND}
      if A = B then
        Result := EqualsValue
      else if A < B then
        Result := LessThanValue
      else
        Result := GreaterThanValue;
    {$ELSE}
      comp := DateTime.Compare(A, B);
      if comp = 0 then
        Result := EqualsValue
      else if comp < 0 then
        Result := LessThanValue
      else
        Result := GreaterThanValue;
    {$ENDIF}
  end;
{$ENDIF}

  function SearchBuf(
    Buf : array of Byte;
    BufLen: Integer;
    SelStart, SelLength: Integer;
    SearchString: String;
    {$IFDEF OXYGENE}
    Options: TStringSearchOptions =  TStringSearchOptions.soDown
    {$ELSE}
    Options: TStringSearchOptions =  [TStringSearchOption.soDown]
    {$ENDIF}
  ) : Integer;

  const
      WordDelimiters : array [0..193] of Byte = (
        0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,
        58,59,60,61,62,63,64,91,92,93,94,95,96,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,
        147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,
        181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,
        215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,
        249,250,251,252,253,254,255
       ) ;
  var
    SearchCount, I: Integer;
    C: Byte;
    Direction: ShortInt;
    ShadowMap: array [0 .. 256] of Byte;
    search_string: TStringBuilder;


    function ByteInSet(
            _c   : Byte ;
      const _set : array of Byte
    ) : Boolean ;
    {$IFDEF DCC}
      var
        c : Byte ;
    {$ENDIF}
    begin
      Result := False ;
      for c in _set do
        if c = _c then begin
          Result := True ;
          break ;
        end;
    end ;

    function FindNextWordStart(var BufOff: Integer): Boolean;
    begin { (True XOR N) is equivalent to (not N) }
      { (False XOR N) is equivalent to (N) }
      { When Direction is forward (1), skip non delimiters, then skip delimiters. }
      { When Direction is backward (-1), skip delims, then skip non delims }
      while (SearchCount > 0) and
        ((Direction = 1) xor (ByteInSet(Buf[BufOff], WordDelimiters))) do
      begin
        inc(BufOff, Direction);
        dec(SearchCount);
      end;
      while (SearchCount > 0) and
        ((Direction = -1) xor (ByteInSet(Buf[BufOff], WordDelimiters))) do
      begin
        inc(BufOff, Direction);
        dec(SearchCount);
      end;
      Result := SearchCount > 0;
      if Direction = -1 then
      begin { back up one char, to leave ptr on first non delim }
        dec(BufOff, Direction);
        inc(SearchCount);
      end;
    end;

  begin
    Result := 0;
    if BufLen <= 0 then
      Exit;

    {$IFDEF DCC}
    if TStringSearchOption.soDown in Options then
    {$ELSE}
    if (Options and TStringSearchOptions.soDown) =  TStringSearchOptions.soDown then
    {$ENDIF}
    begin
      Direction := 1;
      inc(SelStart, SelLength); { start search past end of selection }
      SearchCount := BufLen - SelStart - length(SearchString) + 1;
      if SearchCount < 0 then
        Exit;
      if LongInt(SelStart) + SearchCount > BufLen then
        Exit;
    end
    else
    begin
      Direction := -1;
      dec(SelStart, length(SearchString));
      SearchCount := SelStart + 1;
    end;
    if (SelStart < 0) or (SelStart > BufLen) then
      Exit;
    Result := SelStart;

    { Using a Shadow map array is faster than calling AnsiUpper on every character }
    for C := low(ShadowMap) to high(ShadowMap) - 1 do
      ShadowMap[C] := C;
    ShadowMap[256] := 0;

    {$IFDEF DCC}
    if TStringSearchOption.soMatchCase in Options then
    {$ELSE}
    if ( TStringSearchOptions.soMatchCase and Options) <>  TStringSearchOptions.soMatchCase then
    {$ENDIF}
    begin
      for C := low(ShadowMap) to high(ShadowMap) - 1 do
        ShadowMap[C] := Byte( UpCase( Char( ShadowMap[C] ) ) ) ;
      SearchString := UpperCase( SearchString ) ;
    end;

    search_string := TStringBuilder.Create(SearchString);
    try
      while SearchCount > 0 do
      begin
        {$IFDEF DCC}
        if (TStringSearchOption.soWholeWord in Options) and
        {$ELSE}
        if (( TStringSearchOptions.soWholeWord and Options) =  TStringSearchOptions.soWholeWord) and
        {$ENDIF}
          (Result <> SelStart) then
          if not FindNextWordStart(Result) then
            Break;
        I := 0;
        while (ShadowMap[Buf[Result + I]] = Byte(search_string[I])) do
        begin
          inc(I);
          if I >= search_string.Length then
          begin
            {$IFDEF DCC}
            if (not (TStringSearchOption.soWholeWord in Options)) or (SearchCount = 0)
            {$ELSE}
            if (( TStringSearchOptions.soWholeWord and Options) <>  TStringSearchOptions.soWholeWord) or (SearchCount = 0)
            {$ENDIF}
              or (ByteInSet(Buf[Result + I], WordDelimiters)) then
              Exit;
            Break;
          end;
        end;
        inc(Result, Direction);
        dec(SearchCount);
      end;
    finally
      FreeObject(search_string);
    end;
    Result := -1;
  end;

constructor TGIS_FileCADRG_MapSeries.Create ;
begin
  inherited Create ;
  FItems := TGIS_ObjectList.Create ;
end ;

procedure TGIS_FileCADRG_MapSeries.doDestroy ;
begin
  FreeObject( FItems ) ;
  inherited ;
end ;

function  TGIS_FileCADRG_MapSeries.GetSeriesCodeId(const _s : String) : Integer ;
var
  s : String ;
begin
  s := Trim(_s) ;

  case length(s) of
    0 : Result := 0 ;
    1 : Result := 100*(1 +Integer('Z') -Integer(_s[StringFirst])) ;
  else
    Result := 100*(1 +Integer('Z') -Integer(_s[StringFirst]))
                   +Integer('Z') -Integer(_s[StringFirst+1]);
  end;

end ;

function TGIS_FileCADRG_MapSeries.AddUniqueMap(const _mc : TGIS_FileCADRG_Map
                                               ) : TGIS_FileCADRG_Map ;
var
  i : Integer ;
  function make_cpy(const _mcc : TGIS_FileCADRG_Map) : TGIS_FileCADRG_Map ;
  begin
    Result := TGIS_FileCADRG_Map.Create ;
    Result.Scale := _mcc.Scale ;
    Result.ProductNameId := _mcc.ProductNameId ;
    Result.ProductName := _mcc.ProductName ;
    Result.SeriesCode := _mcc.SeriesCode ;
    Result.SeriesCodeId := _mcc.SeriesCodeId ;
    Result.Enabled := _mcc.Enabled ;
  end ;
begin
  if FItems.Count = 0 then begin
    Result := make_cpy(_mc) ;
    FItems.Add(Result) ;
    exit ;
  end ;

  for i := 0 to FItems.Count - 1 do begin
    if TGIS_FileCADRG_Map(FItems[i]).Scale = _mc.Scale then begin
      if TGIS_FileCADRG_Map(FItems[i]).SeriesCodeId = _mc.SeriesCodeId then
      begin
        Result := TGIS_FileCADRG_Map(FItems[i]) ;
        Exit ;
      end ;
      if TGIS_FileCADRG_Map(FItems[i]).SeriesCodeId < _mc.SeriesCodeId then begin
        Result := make_cpy(_mc) ;
        FItems.Insert(i, Result) ;
        exit
      end
      else
        continue ;
    end ;
    if TGIS_FileCADRG_Map(FItems[i]).Scale < _mc.Scale then begin
      Result := make_cpy(_mc) ;
      FItems.Insert(i, Result) ;
      exit ;
    end ;
  end ;
  Result := make_cpy(_mc) ;
  FItems.Add(Result) ;
end ;

function  TGIS_FileCADRG_MapSeries.InSeries(const _mc : TGIS_FileCADRG_Map) : Boolean ;
var
  i : Integer ;
begin
  Result := False ;
  for i := 0 to FItems.Count - 1 do begin
    if TGIS_FileCADRG_Map(FItems[i]).Scale = _mc.Scale then begin
      if not IsStringEmpty( TGIS_FileCADRG_Map(FItems[i]).SeriesCode ) then begin
        if TGIS_FileCADRG_Map(FItems[i]).SeriesCode = _mc.SeriesCode then
        begin
          Result := True ;
          break ;
        end;
      end
      else begin
        Result := True ;
        break ;
      end;
    end;
  end ;
end ;

procedure TGIS_FileCADRG_MapSeries.AddUniqueMapSeries(const _mcs :TGIS_FileCADRG_MapSeries) ;
var
  i, k, starti : Integer ;
  mci : TGIS_FileCADRG_Map ;
  mcc : TGIS_FileCADRG_Map ;
  procedure make_cpy ;
  begin
    mcc := TGIS_FileCADRG_Map.Create ;
    mcc.Scale := mci.Scale ;
    mcc.ProductNameId := mci.ProductNameId ;
    mcc.ProductName := mci.ProductName ;
    mcc.SeriesCode := mci.SeriesCode ;
    mcc.SeriesCodeId := mci.SeriesCodeId ;
    mcc.Enabled := mci.Enabled ;
  end ;
begin
  if FItems.Count = 0 then begin
    for i := 0 to _mcs.FItems.Count - 1 do begin
      mci := TGIS_FileCADRG_Map(_mcs.FItems[i]) ;
      make_cpy ;
      FItems.Add(mcc) ;
    end ;
    exit ;
  end ;

  starti := 0 ;
  for k := 0 to _mcs.FItems.Count - 1 do begin

    mci :=  TGIS_FileCADRG_Map(_mcs.FItems[k]) ;
    for i := starti to FItems.Count - 1 do begin

      if TGIS_FileCADRG_Map(FItems[i]).Scale = mci.Scale then begin
        if TGIS_FileCADRG_Map(FItems[i]).SeriesCodeId = mci.SeriesCodeId then
        begin
          starti := i + 1 ;
          break ;
        end ;
        if  TGIS_FileCADRG_Map(FItems[i]).SeriesCodeId < mci.SeriesCodeId then begin
          make_cpy ;
          FItems.Insert(i, mcc) ;
          break ;
        end
        else
        if i = (FItems.Count - 1) then begin
          make_cpy ;
          FItems.Add(mcc) ;
          starti := i + 1 ;
          break ;
        end
        else
          continue ;
      end ;
      if TGIS_FileCADRG_Map(FItems[i]).Scale < mci.Scale then begin
        make_cpy ;
        FItems.Insert(i, mcc) ;
        starti := i +1;
        break ;
      end ;
    end ;
    i := FItems.Count - 1 ;
    if TGIS_FileCADRG_Map(FItems[i]).Scale > mci.Scale then begin
      make_cpy ;
      FItems.Add(mcc) ;
      starti := i +1 ;
    end ;
  end ;
end ;

procedure TGIS_FileCADRG_MapSeries.SynchronizeEnabled(const _mcs : TGIS_FileCADRG_MapSeries) ;
var
  i, k, starti : Integer ;
  mci : TGIS_FileCADRG_Map ;
begin
  if FItems.Count = 0 then begin
    exit ;
  end ;
  starti := 0 ;
  for k := 0 to _mcs.FItems.Count - 1 do begin

    mci :=  TGIS_FileCADRG_Map(_mcs.FItems[k]) ;
    for i := starti to FItems.Count - 1 do begin

      if TGIS_FileCADRG_Map(FItems[i]).Scale < mci.Scale then begin
        break ;
      end ;
      if TGIS_FileCADRG_Map(FItems[i]).Scale = mci.Scale then begin
        if TGIS_FileCADRG_Map(FItems[i]).SeriesCodeId = mci.SeriesCodeId then
        begin
          starti := i + 1 ;
          TGIS_FileCADRG_Map(FItems[i]).Enabled := mci.Enabled ;
          break ;
        end ;
      end ;
    end ;
  end ;
end ;

Constructor TGIS_FileCADRGDecoder.Create(const _path: String ) ;
var
  Counter: Integer;
  CurrentTime: TDateTime;
begin
  inherited Create ;

  Frame  := 0 ;
  isValid := False;  // Assume false until proper parsing occurrs
  doByteSwapping := False;  // Assume no "Endian" swapping required
  PathCADRG :=_path ;
  rpfDirectory := ExtractFilePath( _path );

  // Initialize a transparent value setting
  FTransparentRedValue   := Byte(Integer(CADRG_TRANSPARENT_COLOR) shr 16) ;
  FTransparentGreenValue := Byte((Integer(CADRG_TRANSPARENT_COLOR) and $0000FF00) shr 8) ;
  FTransparentBlueValue  := Byte(CADRG_TRANSPARENT_COLOR and $000000FF);
  // There seems to be a bug in tatug transparent support where black is not a valid transparent color
  // so never predefine the transparent color as all 0s for RGB values
  //TransparentBlueValue := 0;

  //Initialize the date times in the framecahe, and the cache entries
  {$IFDEF OXYGENE}
    CurrentTime := Time ;
  {$ELSE}
    CurrentTime := Time ;
  {$ENDIF}
  SetLength(frameCache, CADRG_FRAME_CACHE_SIZE +1) ;

  for Counter :=0 to CADRG_FRAME_CACHE_SIZE do begin
    frameCacheTimes[Counter] := CurrentTime;
    {$IFDEF OXYGENE}
      frameCache[Counter] := new TGIS_FileCADRG_Frame ;
    {$ENDIF}
    frameCache[Counter].FrameFileIndexID := CADRG_CACHE_ENTRY_AVAILABLE;
  end ;

  MapSeriesCadrg :=TGIS_FileCADRG_MapSeries.Create ;
  loadToc;
end ;

constructor TGIS_FileCADRGDecoder.Create( const _ef : TGIS_FileCADRG_ReadFrameEvent ) ;
var
  Counter: Integer;
  CurrentTime: TDateTime;
begin
  inherited Create ;

  Frame  := 0 ;
  isValid := False;  // Assume false until proper parsing occurrs
  doByteSwapping := False;  // Assume no "Endian" swapping required

  PathCADRG := '' ;
  FOnReadFrame := _ef ;

  // Initialize a transparent value setting
  FTransparentRedValue   := Byte(Integer(CADRG_TRANSPARENT_COLOR) shr 16) ;
  FTransparentGreenValue := Byte((Integer(CADRG_TRANSPARENT_COLOR) and $0000FF00) shr 8) ;
  FTransparentBlueValue  := Byte(CADRG_TRANSPARENT_COLOR and $000000FF);
  // There seems to be a bug in tatug transparent support where black is not a valid transparent color
  // so never predefine the transparent color as all 0s for RGB values
  //TransparentBlueValue := 0;

  //Initialize the date times in the framecahe, and the cache entries
  {$IFDEF OXYGENE}
    CurrentTime := Time ;
  {$ELSE}
    CurrentTime := Time ;
  {$ENDIF}
  for Counter :=0 to CADRG_FRAME_CACHE_SIZE do begin
    frameCacheTimes[Counter] := CurrentTime;
    {$IFDEF OXYGENE}
      frameCache[Counter] := new TGIS_FileCADRG_Frame ;
    {$ENDIF}
    frameCache[Counter].FrameFileIndexID := CADRG_CACHE_ENTRY_AVAILABLE;
  end ;

  MapSeriesCadrg := TGIS_FileCADRG_MapSeries.Create ;
  loadDBToc ;
end ;

procedure TGIS_FileCADRGDecoder.doDestroy ;
begin

 SetLength(frameCache, 0) ;
  if assigned(BDSimpleFramesList) then
    FreeObject( BDSimpleFramesList ) ;
  if assigned(BDSubLayersList) then
    FreeObject( BDSubLayersList ) ;

  FreeObject( MapSeriesCadrg ) ;
  FreeObject( Dct ) ;
  inherited ;
end ;

procedure TGIS_FileCADRGDecoder.DoReload ;
var
  Counter: Integer;
  CurrentTime: TDateTime;
begin
  if not assigned(FOnReadFrame) then
    exit ;

  Frame  := 0 ;
  isValid := False;  // Assume false until proper parsing occurrs
  doByteSwapping := False;  // Assume no "Endian" swapping required

  //Initialize the date times in the framecahe, and the cache entries
  CurrentTime := Time ;
  for Counter :=0 to CADRG_FRAME_CACHE_SIZE do begin
    frameCacheTimes[Counter] := CurrentTime;
    frameCache[Counter].FrameFileIndexID := CADRG_CACHE_ENTRY_AVAILABLE;
  end ;

  MapSeriesCadrg.Items.Clear ;
  loadDBToc ;
end;

// This procedure loads the requested Frame
procedure TGIS_FileCADRGDecoder.loadFrame( _frameFileIndex: Cardinal);
var
  FrameFilename : String ;
  FrameFilePath : String ;
  NITFTag : TBytes ;
  NITFBuffer : array [0..1023] of Byte ;
  RPFHeaderLoc : Integer ;
  Header       : TGIS_FileCADRG_HeaderRec ;
  HeaderOffset : Int64 ;
  {$IFDEF OXYGENE}
    Locations  : array of TGIS_FileCADRG_ComponentLocationRec ;
    SubFrameOffsets  : array of array of Cardinal ;
  {$ELSE}
    Locations  : array [0..9] of TGIS_FileCADRG_ComponentLocationRec ;
    SubFrameOffsets      : array [0..5] of array [0..5] of Cardinal ;
  {$ENDIF}
  CompressionHeader      : TGIS_FileCADRG_CompressionHeaderRec ;
  CTLookupOffset         : Cardinal ;
  CTLookupRecordLength   : Word ;
  CompressionLookupTable : array [0..3] of TGIS_FileCADRG_CompressionLookupRecord ;
  ImageDescriptionHeader : TGIS_FileCADRG_ImageDescriptionHeaderRec ;
  //BytesPerRow: Cardinal;
  //BytesPerSubFrame: Cardinal;
  IDPHeader              : TGIS_FileCADRG_ImageDisplayParametersHeaderRec ;
  ColorHeader            : TGIS_FileCADRG_ColorHeaderRec ;
  ColorMapHeader         : TGIS_FileCADRG_ColorMapHeaderRec ;
  ColorConverterHeader   : TGIS_FileCADRG_ColorConverterHeaderRec ;
  ColorOffsets           : array of TGIS_FileCADRG_ColorOffsetRec ;
  ReducedColorTable      : Boolean ;
  Counter  : Word   ;
  Counter2 : Word   ;
  frmname  : String ;
  fs : TGIS_FileStream ;
  offset : Integer ;
  {$IFDEF JAVA}
  i : Integer ;
  {$ENDIF}
begin
  {$IFDEF OXYGENE}
    SetLength( Locations, 10 ) ;
    SetLength( SubFrameOffsets, 6, 6 ) ;
    {$IFDEF JAVA}
      for i := 0 to 9 do
        Locations[i] := new TGIS_FileCADRG_ComponentLocationRec() ;
    {$ENDIF}
    ColorHeader := new TGIS_FileCADRG_ColorHeaderRec ;
  {$ENDIF}

  // Assume Frame contents invalid until fully parsed and loaded
  frameCache[Frame].IsFrameValid := False ;

  // Validate that the framefileindex is within the range
  //if FrameFileIndex > high(FrameFileIndexRecords) then
  //  end procedure;

  // Save the FrameEntryID
  frameCache[Frame].FrameFileIndexID := _frameFileIndex ;

  //**** This should be derived from Frame id reference but hard coded for now
  {$IFDEF OXYGENE}
    FrameFilename := FrameFileIndexRecords[_frameFileIndex].FrameFileName; // 'd:\rpf\cjga\cjgaz01\00057013.JA1';
    //FrameFilename := ConvertAnsiString( FrameFileIndexRecords[_frameFileIndex].FrameFileName ) ; // 'd:\rpf\cjga\cjgaz01\00057013.JA1';
    FrameFilePath := String( getFrameFilePath(FrameFileIndexRecords[_frameFileIndex].PathnameRecordOffset) );
  {$ELSE}
    FrameFilename := ConvertAnsiString( FrameFileIndexRecords[_frameFileIndex].FrameFileName); // 'd:\rpf\cjga\cjgaz01\00057013.JA1';
    FrameFilePath := getFrameFilePath(FrameFileIndexRecords[_frameFileIndex].PathnameRecordOffset) ;
  {$ENDIF}

  {$IFDEF GIS_MOBILE}
    frmname := System.IoUtils.TPath.Combine( rpfDirectory, FrameFilePath + FrameFilename ) ;
  {$ELSE}
    frmname := rpfDirectory + GisEnvironmentInfo.DirSep + FrameFilePath + FrameFilename ;
  {$ENDIF}
  if SafeFileExists(frmname) then
    fs := TGIS_FileStream.Create( frmname,
                                  fmOpenRead or
                                  fmShareDenyWrite
                                )
  else begin
    exit ;
  end ;

  try

    // Determine NITF version if any. Assume not NITF until proven wrong
    HeaderOffset := CADRG_FRAME_NOT_NTIF_OFFSET ;
    SetLength( NITFTag, 7 ) ;
    {$IFDEF OXYGENE}
      fs.ReadBuffer( NITFTag, 7 ) ;
    {$ELSE}
      fs.ReadBuffer( NITFTag[0], 7 ) ;
    {$ENDIF}
    if ( ConvertAnsiString( NITFTag ) = CADRG_NTIF2X_IDENTIFIER ) then
    begin
      fs.Seek( 360, soBeginning ) ; // the fixed portion of the NITF header ends here the rest is the
                                    // variable length portion. For simplicity of parsing read a 1K block
                                    // and find the RPFHDR TRE entry
      fs.ReadBuffer( NITFBuffer, Min( fs.Size-fs.Position, 1024 ) ) ;
      RPFHeaderLoc := SearchBuf( NITFBuffer, 1024, 0, 0, 'RPFHDR00048' ) ;
      if RPFHeaderLoc = -1 then begin
        // ***Todo raise exception RPFHeader TRE not found
      end ;
      HeaderOffset := 360 + RPFHeaderLoc + 11;
    end ;

    //seek to the Header
    fs.Seek( HeaderOffset, soBeginning ) ;
    {$IFDEF OXYGENE}
      Header.Read( fs ) ;
    {$ELSE}
      fs.ReadBuffer( Header, 48 ) ;
    {$ENDIF}

    // Examine the Endian component and setup what if any byte swapping is required
    checkSwap( Header.Endian ) ;

    doSwapW( Header.HeaderSectionLength ) ;
    doSwapC( Header.LocationPhysical ) ;

    // Seek to the location section
    fs.Seek( Header.LocationPhysical, soBeginning ) ;

    // Set up the array of sections/subsections IDs we care about
    Locations[0].ID := CADRG_LOC_COMPRESSION_SECTION;
    Locations[1].ID := CADRG_LOC_IMAGE_DESCR_SUBHEADER;
    Locations[2].ID := CADRG_LOC_COMPRESSION_LOOKUP_SUBSECTION;
    Locations[3].ID := CADRG_LOC_SPATIAL_DATA_SUBSECTION;
    Locations[4].ID := CADRG_LOC_IMAGE_DISPLAY_PARAM_SUBHEADER;
    Locations[5].ID := CADRG_LOC_MASK_SUBSECTION;
    Locations[6].ID := CADRG_LOC_COLORGRAY_SECTION_SUBHEADER;
    Locations[7].ID := CADRG_LOC_COLORMAP_SUBSECTION;
    Locations[8].ID := CADRG_LOC_COLOR_CONVERTER_SUBSECTION;
    Locations[9].ID := CADRG_LOC_COVERAGE_SECTION;

    parseLocations( fs, Locations, 10 ) ;

    /////////////////////////////////////
    // Parse the Coverage Section Info
    /////////////////////////////////////

    // Seek to the Product DataType
    fs.Seek( CADRG_LOC_PRODUCT_DATA_TYPE, soBeginning ) ;

    {$IFDEF OXYGENE}
      fs.Read( BoundaryRectangleRecords[0].ProductDataType, 5 ) ;
    {$ELSE}
      fs.Read( BoundaryRectangleRecords[0].ProductDataType, 5 ) ;
    {$ENDIF}

    //Determine which Product DataType we are dealing with
    // Currently supports CADRG and CIB
    if ConvertAnsiString( BoundaryRectangleRecords[0].ProductDataType ) = CADRG_CADRG_TAG then
      FProductDataType := CADRG_PRODUCT_DATA_TYPE_CADRG
    else
    if Trim( ConvertAnsiString( BoundaryRectangleRecords[0].ProductDataType )) = CADRG_CIB_TAG then
      FProductDataType := CADRG_PRODUCT_DATA_TYPE_CIB
    else begin
      //****TODO raise error condition unsupported product data type
    end ;

    fs.Seek( Locations[9].Location, soBeginning ) ;
    {$IFDEF OXYGENE}
      frameCache[Frame].Coverage.Read( fs ) ;
    {$ELSE}
      fs.ReadBuffer( frameCache[Frame].Coverage, 96 ) ;
    {$ENDIF}

    doSwapD( frameCache[Frame].Coverage.NWUpperLeftLatitude         ) ;
    doSwapD( frameCache[Frame].Coverage.NWUpperLeftLongitude        ) ;
    doSwapD( frameCache[Frame].Coverage.SWLowerLeftLatitude         ) ;
    doSwapD( frameCache[Frame].Coverage.SWLowerLeftLongitude        ) ;
    doSwapD( frameCache[Frame].Coverage.NEUpperRightLatitude        ) ;
    doSwapD( frameCache[Frame].Coverage.NEUpperRightLongitude       ) ;
    doSwapD( frameCache[Frame].Coverage.SELowerRightLatitude        ) ;
    doSwapD( frameCache[Frame].Coverage.SELowerRightLongitude       ) ;
    doSwapD( frameCache[Frame].Coverage.NSVerticalResolution        ) ;
    doSwapD( frameCache[Frame].Coverage.EWHorizontalResolution      ) ;
    doSwapD( frameCache[Frame].Coverage.LatitudeVerticalInterval    ) ;
    doSwapD( frameCache[Frame].Coverage.LongitudeHorizontalInterval ) ;

    /////////////////////////////////////
    // Parse the Compression Section Info
    /////////////////////////////////////
    fs.Seek( Locations[0].Location, soBeginning ) ;
    {$IFDEF OXYGENE}
      CompressionHeader.Read( fs ) ;
    {$ELSE}
      fs.ReadBuffer( CompressionHeader, 6 ) ;
    {$ENDIF}
    doSwapW( CompressionHeader.CompressionAlgorithmId ) ;
    doSwapW( CompressionHeader.NumberOfCompressionLookupOffsetRecords ) ;
    doSwapW( CompressionHeader.NumberOfCompressionParameterOffsetRecords ) ;

    // See if Compression Lookup subsection was found if not use alternate method
    if Locations[2].Location = 0 then
      // Go to compression section and skip over Compression subsection header
      // *** ToDo in sample parser from Mitre they skipped 10 bytes here. verify
      // that my calculation of 6 is correct
      fs.Seek( Locations[0].Location + 6, soBeginning )
    else
      fs.Seek( Locations[2].Location, soBeginning ) ;

    fs.ReadCardinal( CTLookupOffset, 4 ) ;
    doSwapC( CTLookupOffset ) ;
    fs.ReadWord( CTLookupRecordLength, 2 ) ;
    doSwapW( CTLookupRecordLength ) ;

    // Read the 4 lookup records
    for Counter := 0 to 3 do begin
      {$IFDEF OXYGENE}
        CompressionLookupTable[Counter].Read( fs ) ;
      {$ELSE}
        fs.ReadBuffer( CompressionLookupTable[Counter], 14 ) ;
      {$ENDIF}
      doSwapW( CompressionLookupTable[Counter].LookupTableId         ) ;
      doSwapC( CompressionLookupTable[Counter].NumberOfLookupRecords ) ;
      doSwapW( CompressionLookupTable[Counter].NumberOfValues        ) ;
      doSwapW( CompressionLookupTable[Counter].LookupValueBitLength  ) ;
      doSwapC( CompressionLookupTable[Counter].LookupTableOffset     ) ;
      // Do some validity checking on values
      if ( (CompressionLookupTable[Counter].NumberOfLookupRecords <> 4096) or
           (CompressionLookupTable[Counter].LookupValueBitLength <> 8) or
           (CompressionLookupTable[Counter].NumberOfValues <> 4) ) then begin
        // Invalid VQ info
        //*** Todo exit with invalid status for this Frame

      end ;

    end ;

    // Read the Compression lookup table
    offset := 0 ;
    for Counter := 0 to 3 do begin
      fs.Seek( Locations[2].Location + CompressionLookupTable[Counter].LookupTableOffset,
               soBeginning ) ;
      {$IFDEF OXYGENE}
        fs.Read( frameCache[Frame].CompressionTable, offset, 4096*4 ) ;
        offset := offset + 4096 * 4 ;
      {$ELSE}
        fs.ReadBuffer( frameCache[Frame].CompressionTable[offset], 4096*4 ) ;
        offset := offset + 4096 * 4 ;
      {$ENDIF}
    end ;

    /////////////////////////////////////
    // Parse the Image Section Info
    /////////////////////////////////////
    // Read the image description header record
    fs.Seek( Locations[1].Location, soBeginning ) ;
    {$IFDEF OXYGENE}
      ImageDescriptionHeader.Read( fs ) ;
    {$ELSE}
      fs.ReadBuffer( ImageDescriptionHeader, 28 ) ;
    {$ENDIF}
    doSwapW( ImageDescriptionHeader.NumberOfSpectralGroups               ) ;
    doSwapW( ImageDescriptionHeader.NumberOfSubframeTables               ) ;
    doSwapW( ImageDescriptionHeader.NumberOfSpectralBandTables           ) ;
    doSwapW( ImageDescriptionHeader.NumberOfSpectralBandLinesPerImageRow ) ;
    doSwapW( ImageDescriptionHeader.NumberOfSubframesEastWestDirection   ) ;
    doSwapW( ImageDescriptionHeader.NumberOfSubframesNorthSouthDirection ) ;
    doSwapC( ImageDescriptionHeader.NumberOfOutputColumnsPerSubframe     ) ;
    doSwapC( ImageDescriptionHeader.NumberOfOutputRowsPerSubframe        ) ;
    doSwapC( ImageDescriptionHeader.SubframeMaskTableOffset              ) ;
    doSwapC( ImageDescriptionHeader.TransparencyMaskTableOffset          ) ;

    // Do some sanity checking. Apparently there are old format files
    // where SubFrameMaskTableOffset was not present
    if ImageDescriptionHeader.SubframeMaskTableOffset = 0 then begin
      //*** Todo Invalid Frame file capture error message and mark as invalid

    end ;

    // Check subframeMaskTableOffset for indicator that all are present
    if ImageDescriptionHeader.SubframeMaskTableOffset = $ffffffff then
      frameCache[Frame].AllSubframesPresent := true
    else
      frameCache[Frame].AllSubframesPresent := false;

    // Initialize the subframe masked array to all false indicating all present
    for Counter := 0 to 5 do
      for Counter2 := 0 to 5 do
        {$IFDEF OXYGENE}
          frameCache[Frame].SubFrameMasked[Counter, Counter2] := false ;
        {$ELSE}
          frameCache[Frame].SubFrameMasked[Counter][Counter2] := false ;
        {$ENDIF}

    // If some subeframes are masked then see which ones
    if not frameCache[Frame].AllSubframesPresent then begin
      //Seek to the mask subsection. Perform some sanity check in case section wasn't
      //present in the locations section
      if Locations[5].Location = 0 then begin
        //*** Todo Invalid Frame file capture error message and mark as invalid

      end ;
      fs.Seek( Locations[5].Location, soBeginning ) ;
      // Skip to the offsets table
      fs.Seek( ImageDescriptionHeader.SubframeMaskTableOffset, soCurrent ) ;
      //Cycle trough the masks and adjust the subframemasked array booleans
      for Counter := 0 to 5 do begin
        for Counter2 := 0 to 5 do begin
          {$IFDEF OXYGENE}
            fs.ReadCardinal( SubFrameOffsets[Counter, Counter2], 4 ) ;
          {$ELSE}
            fs.ReadBuffer( SubFrameOffsets[Counter][Counter2], 4 ) ;
          {$ENDIF}
          {$IFDEF OXYGENE}
            doSwapC( SubFrameOffsets[Counter, Counter2] ) ;
          {$ELSE}
            doSwapC( SubFrameOffsets[Counter][Counter2] ) ;
          {$ENDIF}
          // If the offset is 0xffffffff then the subframe is masked
          {$IFDEF OXYGENE}
            if SubFrameOffsets[Counter, Counter2] = $ffffffff then
              frameCache[Frame].SubFrameMasked[Counter, Counter2] := true;
          {$ELSE}
            if SubFrameOffsets[Counter][Counter2] = $ffffffff then
              frameCache[Frame].SubFrameMasked[Counter][Counter2] := true;
          {$ENDIF}
        end ;
      end ;
    end ;

    //Seek to the Image subsection. Perform some sanity check in case section wasn't
    //present in the locations section
    if Locations[4].Location = 0 then begin
      //*** Todo Invalid Frame file capture error message and mark as invalid

    end ;

    // Go to the Image Display Parameters subsection
    // Not actually used but for completeness leave in for now. It also serves to position
    // to IMage spacial data location in case section wasn't in the locations section
    fs.Seek( Locations[4].Location, soBeginning ) ;
    {$IFDEF OXYGENE}
      IDPHeader.Read( fs ) ;
    {$ELSE}
      fs.ReadBuffer( IDPHeader, 9 ) ;
    {$ENDIF}
    doSwapC( IDPHeader.NumberOfImageRows ) ;
    doSwapC( IDPHeader.NumberOfImageCodesPerRow ) ;

    // Get the image spacial data. After doing sanity check
    if Locations[3].Location = 0 then
      // Should log a warning or something but because I have read the previous IDP subsection
      // I should be at the Image Spacial Data subsection.
      // ****Note the Mitre sample parser didn't read the IDP header info and sought forward 14
      // Not sure where this came from as the IDP subsection is only 9 bytes long?
    else
      fs.Seek( Locations[3].Location, soBeginning ) ;

    //************
    // The following commented section contains logic for calculating size requirements
    // but they are fixed for CADRG and CIB spatial image data
    // So Hard coded in Frame structure to be 6144 bytes
    //************
    // Calculate the space required per row
    //BytesPerRow := (IDPHeader.NumberOfImageCodesPerRow * IDPHeader.ImageCodeBitLength) div 8;
    // Calculate the space required per subframe
    //BytesPerSubFrame := IDPHeader.NumberOfImageRows * BytesPerRow;
    // *** Note the BytesPerSubframe could probably be hardcoded at 6144 bytes
    // as the image display parameters for cadrg are always
    // (64imagecodes * 12bits)/8 * 64 ImageRows = 6144
    // but for completeness I calculate them anyway.
    //SetLength(SubFrameData,BytesPerSubframe);

    // Read the subframe data in rowwise fashion
    for Counter := 0 to 5 do begin
        for Counter2 := 0 to 5 do begin
          // Read only frames that aren't masked
        if not frameCache[Frame].SubFrameMasked[Counter, Counter2] then begin
          if (fs.Size - fs.Position ) < 6144 then begin //
          // Destroy the stream object
            FreeObject( fs ) ;
            exit ;
          end;
          {$IFDEF OXYGENE}
            fs.Read( frameCache[Frame].SubFrameData, (Counter*6+Counter2)*6144, 6144 ) ;
          {$ELSE}
            fs.ReadBuffer(frameCache[Frame].SubFrameData[(Counter*6+Counter2)*6144],6144);
          {$ENDIF}
        end;
      end ;
    end ;

    /////////////////////////////////////
    // Parse the Color/Grayscale Section Info
    /////////////////////////////////////
    //Seek to the Color/Grayscale subsection. Perform some sanity check in case section wasn't
    //present in the locations section
    if Locations[6].Location = 0 then begin
      //*** Todo Invalid Frame file capture error message and mark as invalid

    end ;
    fs.Seek( Locations[6].Location, soBeginning ) ;
    {$IFDEF OXYGENE}
      ColorHeader.Read( fs ) ;
    {$ELSE}
      fs.ReadBuffer( ColorHeader, 14 ) ;
    {$ENDIF}

    // Resize the array that holds the coloroffset records
    SetLength( ColorOffsets, ColorHeader.NumberOfColorOffsetRecords ) ;
    {$IFDEF JAVA}
      for i := 0 to ColorHeader.NumberOfColorOffsetRecords-1 do
        ColorOffsets[i] := new TGIS_FileCADRG_ColorOffsetRec() ;
    {$ENDIF}


    //Seek to the ColorMap subsection. Perform some sanity check in case section wasn't
    //present in the locations section
    if Locations[7].Location = 0 then begin
      //*** Todo Invalid Frame file capture error message and mark as invalid

    end ;
    fs.Seek( Locations[7].Location, soBeginning ) ;
    {$IFDEF OXYGENE}
      ColorMapHeader.Read( fs ) ;
    {$ELSE}
      fs.ReadBuffer( ColorMapHeader, 6 ) ;
    {$ENDIF}
    doSwapC( ColorMapHeader.ColorMapOffsetTableOffset ) ;
    doSwapW( ColorMapHeader.ColorOffsetRecordLength   ) ;

    // ****Todo check where reduced colormap is set in mitre parser
    // If reduced is false then we don't need ColorConverter info
    ReducedColorTable := false ;
    if not ReducedColorTable then begin
      // Read in the coloroffset records
      for Counter := 0 to ColorHeader.NumberOfColorOffsetRecords - 1 do begin
        {$IFDEF OXYGENE}
          ColorOffsets[Counter].Read( fs ) ;
        {$ELSE}
          fs.ReadBuffer( ColorOffsets[Counter], 17 ) ;
        {$ENDIF}
        doSwapW( ColorOffsets[Counter].ColorTableId ) ;
        doSwapC( ColorOffsets[Counter].NumberOfColorRecords ) ;
        doSwapW( ColorOffsets[Counter].HistogramRecordLength ) ;
        doSwapC( ColorOffsets[Counter].ColorTableOffset ) ;
        doSwapC( ColorOffsets[Counter].HistogramTableOffset ) ;

        if ( (ColorOffsets[Counter].NumberOfColorRecords = 216) or
             (ColorOffsets[Counter].NumberOfColorRecords = 217) ) then begin

          // Save the number of colors in the Frame record
          frameCache[Frame].NumberOfColors := ColorOffsets[Counter].NumberOfColorRecords ;

          fs.Seek( Locations[7].Location + ColorOffsets[Counter].ColorTableOffset,
                   soBeginning
                 ) ;
          for Counter2 := 0 to ColorOffsets[Counter].NumberOfColorRecords-1 do begin
            // If we have a CIB file read only the single Monochrome value else we assume
            // we have CADRG 4 byte rgbm value
            if FProductDataType = CADRG_PRODUCT_DATA_TYPE_CIB then begin
              // Read the Monochrome byte and fill in the green and blue with the same
              fs.ReadByte( frameCache[Frame].ColorTable[Counter2].Monochrome, 1 ) ;
              // Calculate grayscale values for RGB components
              frameCache[Frame].ColorTable[Counter2].Red   := frameCache[Frame].ColorTable[Counter2].Monochrome;
              frameCache[Frame].ColorTable[Counter2].Green := frameCache[Frame].ColorTable[Counter2].Monochrome;
              frameCache[Frame].ColorTable[Counter2].Blue  := frameCache[Frame].ColorTable[Counter2].Monochrome;
            end
            else begin
              // Read the red green blue and Monochrome values
              {$IFDEF OXYGENE}
                frameCache[Frame].ColorTable[Counter2].Read( fs ) ;
              {$ELSE}
                fs.ReadBuffer( frameCache[Frame].ColorTable[Counter2], 4 ) ;
              {$ENDIF}
              // In order to make transparent color support work I need to slightly skew any color
              // that matches our predefined "Transparent" color. This was lesss coding than dynamically
              // adjusting the transparency table as new transparent values showed up in frames. Also
              // there seemed to be a bug where full black was an invalid tranparent value
              // therefore using a predefined value would address this problem.
              if ( ( frameCache[Frame].ColorTable[Counter2].Red = FTransparentRedValue ) and
                   ( frameCache[Frame].ColorTable[Counter2].Green = FTransparentGreenValue ) and
                   ( frameCache[Frame].ColorTable[Counter2].Blue = FTransparentBlueValue ) ) then begin
                 // Skew the blue value + or - 1 intensity to differentiate it from the transparent value
                if FTransparentBlueValue > 0 then
                  frameCache[Frame].ColorTable[Counter2].Blue := FTransparentBlueValue - 1
                else
                  frameCache[Frame].ColorTable[Counter2].Blue := FTransparentBlueValue + 1;
              end ;
            end ;
          end ;
          if ColorOffsets[Counter].NumberOfColorRecords = 217 then begin
            frameCache[Frame].ColorTable[216].Red := FTransparentRedValue ;
            frameCache[Frame].ColorTable[216].Green := FTransparentGreenValue ;
            frameCache[Frame].ColorTable[216].Blue := FTransparentBlueValue ;
          end ;
          // Break out of Counter loop as we found the colortable info
          break;
        end ;
      end ;
    end // if not ReducedColorTable
    else begin
      // Read the Color Conversion table to find out which color table we should read

      //Seek to the Color Conversion subsection. Perform some sanity check in case section wasn't
      //present in the locations section
      if Locations[8].Location = 0 then begin
        //*** Todo Invalid Frame file capture error message and mark as invalid

      end ;
      fs.Seek( Locations[8].Location, soBeginning ) ;
      {$IFDEF OXYGENE}
        ColorConverterHeader.Read( fs ) ;
      {$ELSE}
        fs.ReadBuffer( ColorConverterHeader, 8 ) ;
      {$ENDIF}
      doSwapC( ColorConverterHeader.ColorConverterOffsetTableOffset ) ;
      doSwapW( ColorConverterHeader.ColorConverterOffsetRecordLength ) ;
      doSwapW( ColorConverterHeader.ColorConverterRecordLength ) ;
    end ;

    // Destroy the stream object
    FreeObject( fs ) ;

    // Frame is valid
    frameCache[Frame].IsFrameValid := true ;
  except
    raise EGIS_Exception.Create( GIS_RS_ERR_FILEREAD, frmname, 0 ) ;
  end ;
end ;

// This procedure loads the requested Frame
procedure TGIS_FileCADRGDecoder.loadDBFrame( _frameFileIndex: Cardinal);
var
  NITFTag : TBytes ;
  NITFBuffer: array [0..1023] of Byte ;
  RPFHeaderLoc : Integer ;
  Header: TGIS_FileCADRG_HeaderRec;
  HeaderOffset: Int64;
  {$IFDEF OXYGENE}
    Locations: array of TGIS_FileCADRG_ComponentLocationRec;
  {$ELSE}
    Locations: array [0..9] of TGIS_FileCADRG_ComponentLocationRec;
  {$ENDIF}

  CompressionHeader: TGIS_FileCADRG_CompressionHeaderRec;
  CTLookupOffset: Cardinal;
  CTLookupRecordLength: Word;
  CompressionLookupTable: array [0..3] of TGIS_FileCADRG_CompressionLookupRecord;
  ImageDescriptionHeader: TGIS_FileCADRG_ImageDescriptionHeaderRec;
  SubFrameOffsets: array [0..5] of array [0..5] of Cardinal;
  //BytesPerRow: Cardinal;
  //BytesPerSubFrame: Cardinal;
  IDPHeader: TGIS_FileCADRG_ImageDisplayParametersHeaderRec;
  ColorHeader: TGIS_FileCADRG_ColorHeaderRec;
  ColorMapHeader: TGIS_FileCADRG_ColorMapHeaderRec;
  ColorConverterHeader: TGIS_FileCADRG_ColorConverterHeaderRec;
  ColorOffsets: array of TGIS_FileCADRG_ColorOffsetRec;
  ReducedColorTable: Boolean;
  Counter: Word;
  Counter2: Word;
  frmname : String ;
  fs : TGIS_FileStream ;

  offset : Integer ;
  productDataType           : array [0.. 5] of Byte ;
begin
  {$IFDEF OXYGENE}
  SetLength( Locations,10);
  ColorHeader := new TGIS_FileCADRG_ColorHeaderRec ;
  {$ENDIF}
  // Assume Frame contents invalid until fully parsed and loaded
  frameCache[Frame].IsFrameValid := False ;

  // Validate that the framefileindex is within the range
  //if FrameFileIndex > high(FrameFileIndexRecords) then
  //  end procedure;

  // Save the FrameEntryID
  frameCache[Frame].FrameFileIndexID := _frameFileIndex ;

  frmname := TGIS_FileCADRG_SimpleFrame
                    (BDSimpleFramesList[_frameFileIndex]).Path ;
  if SafeFileExists(frmname) then
    fs := TGIS_FileStream.Create( frmname,
                                  fmOpenRead or
                                  fmShareDenyWrite
                                )
  else begin
    exit ;
  end ;

  // Determine NITF version if any. Assume not NITF until proven wrong
  HeaderOffset := CADRG_FRAME_NOT_NTIF_OFFSET;
  SetLength(NITFTag,7);
  {$IFDEF OXYGENE}
    fs.ReadBuffer( NITFTag, 7 ) ;
  {$ELSE}
    fs.ReadBuffer( NITFTag[0], 7 ) ;
  {$ENDIF}
  if ( ConvertAnsiString( NITFTag ) = CADRG_NTIF2X_IDENTIFIER ) then
  begin
      fs.Seek( 360, soBeginning ) ; // the fixed portion of the NITF header ends here the rest is the
                                    // variable length portion. For simplicity of parsing read a 1K block
                                    // and find the RPFHDR TRE entry
      fs.ReadBuffer(NITFBuffer,Min( fs.Size-fs.Position, 1024 ));
      RPFHeaderLoc := SearchBuf(NITFBuffer,1024,0,0,'RPFHDR00048');
      if RPFHeaderLoc = -1 then begin
        // ***Todo raise exception RPFHeader TRE not found
      end ;
      HeaderOffset := 360 + RPFHeaderLoc + 11;
  end ;

  //seek to the Header
  fs.Seek( HeaderOffset, soBeginning ) ;
  {$IFDEF OXYGENE}
    Header.Read( fs ) ;
  {$ELSE}
    fs.ReadBuffer(Header,48);
  {$ENDIF}

  // Examine the Endian component and setup what if any byte swapping is required
  checkSwap(Header.Endian);

  doSwapW( Header.HeaderSectionLength ) ;
  doSwapC( Header.LocationPhysical ) ;

  // Seek to the location section
  fs.Seek( Header.LocationPhysical, soBeginning ) ;

  // Set up the array of sections/subsections IDs we care about
  Locations[0].ID := CADRG_LOC_COMPRESSION_SECTION;
  Locations[1].ID := CADRG_LOC_IMAGE_DESCR_SUBHEADER;
  Locations[2].ID := CADRG_LOC_COMPRESSION_LOOKUP_SUBSECTION;
  Locations[3].ID := CADRG_LOC_SPATIAL_DATA_SUBSECTION;
  Locations[4].ID := CADRG_LOC_IMAGE_DISPLAY_PARAM_SUBHEADER;
  Locations[5].ID := CADRG_LOC_MASK_SUBSECTION;
  Locations[6].ID := CADRG_LOC_COLORGRAY_SECTION_SUBHEADER;
  Locations[7].ID := CADRG_LOC_COLORMAP_SUBSECTION;
  Locations[8].ID := CADRG_LOC_COLOR_CONVERTER_SUBSECTION;
  Locations[9].ID := CADRG_LOC_COVERAGE_SECTION;

  parseLocations(fs, Locations, 10);

  /////////////////////////////////////
  // Parse the Coverage Section Info
  /////////////////////////////////////

  // Seek to the Product DataType
  fs.Seek( CADRG_LOC_PRODUCT_DATA_TYPE, soBeginning ) ;

  {$IFDEF OXYGENE}
  fs.Read( productDataType, 5 ) ;
  {$ELSE}
  fs.Read( productDataType[0], 5 ) ;
  {$ENDIF}

  //Determine which Product DataType we are dealing with
  // Currently supports CADRG and CIB
   if ConvertAnsiString( productDataType ) = CADRG_CADRG_TAG then
    FProductDataType := CADRG_PRODUCT_DATA_TYPE_CADRG
  else
   if Trim( ConvertAnsiString( productDataType ) ) = CADRG_CIB_TAG then
    FProductDataType := CADRG_PRODUCT_DATA_TYPE_CIB
  else begin
    //****TODO raise error condition unsupported product data type
    FProductDataType := CADRG_PRODUCT_DATA_TYPE_CIB
  end ;

  fs.Seek( Locations[9].Location, soBeginning ) ;
  {$IFDEF OXYGENE}
    frameCache[Frame].Coverage.Read( fs ) ;
  {$ELSE}
    fs.ReadBuffer(frameCache[Frame].Coverage,96);
  {$ENDIF}
  doSwapD( frameCache[Frame].Coverage.NWUpperLeftLatitude         ) ;
  doSwapD( frameCache[Frame].Coverage.NWUpperLeftLongitude        ) ;
  doSwapD( frameCache[Frame].Coverage.SWLowerLeftLatitude         ) ;
  doSwapD( frameCache[Frame].Coverage.SWLowerLeftLongitude        ) ;
  doSwapD( frameCache[Frame].Coverage.NEUpperRightLatitude        ) ;
  doSwapD( frameCache[Frame].Coverage.NEUpperRightLongitude       ) ;
  doSwapD( frameCache[Frame].Coverage.SELowerRightLatitude        ) ;
  doSwapD( frameCache[Frame].Coverage.SELowerRightLongitude       ) ;
  doSwapD( frameCache[Frame].Coverage.NSVerticalResolution        ) ;
  doSwapD( frameCache[Frame].Coverage.EWHorizontalResolution      ) ;
  doSwapD( frameCache[Frame].Coverage.LatitudeVerticalInterval    ) ;
  doSwapD( frameCache[Frame].Coverage.LongitudeHorizontalInterval ) ;

  /////////////////////////////////////
  // Parse the Compression Section Info
  /////////////////////////////////////
  fs.Seek( Locations[0].Location, soBeginning ) ;
  {$IFDEF OXYGENE}
    CompressionHeader.Read( fs ) ;
  {$ELSE}
    fs.ReadBuffer(CompressionHeader,6);
  {$ENDIF}
  doSwapW( CompressionHeader.CompressionAlgorithmId ) ;
  doSwapW( CompressionHeader.NumberOfCompressionLookupOffsetRecords ) ;
  doSwapW( CompressionHeader.NumberOfCompressionParameterOffsetRecords ) ;

  // See if Compression Lookup subsection was found if not use alternate method
  if Locations[2].Location = 0 then
    // Go to compression section and skip over Compression subsection header
    // *** ToDo in sample parser from Mitre they skipped 10 bytes here. verify
    // that my calculation of 6 is correct
    fs.Seek( Locations[0].Location + 6, soBeginning )
  else
    fs.Seek( Locations[2].Location, soBeginning ) ;

  fs.ReadCardinal(CTLookupOffset,4);
  doSwapC( CTLookupOffset ) ;
  fs.ReadWord(CTLookupRecordLength,2);
  doSwapW( CTLookupRecordLength ) ;

  // Read the 4 lookup records
  for Counter :=0 to 3 do begin
    {$IFDEF OXYGENE}
      CompressionLookupTable[Counter].Read( fs ) ;
    {$ELSE}
      fs.ReadBuffer(CompressionLookupTable[Counter],14);
    {$ENDIF}
    doSwapW( CompressionLookupTable[Counter].LookupTableId         ) ;
    doSwapC( CompressionLookupTable[Counter].NumberOfLookupRecords ) ;
    doSwapW( CompressionLookupTable[Counter].NumberOfValues        ) ;
    doSwapW( CompressionLookupTable[Counter].LookupValueBitLength  ) ;
    doSwapC( CompressionLookupTable[Counter].LookupTableOffset     ) ;
    // Do some validity checking on values
    if ( (CompressionLookupTable[Counter].NumberOfLookupRecords <> 4096) or
         (CompressionLookupTable[Counter].LookupValueBitLength <> 8) or
         (CompressionLookupTable[Counter].NumberOfValues <> 4) ) then begin
      // Invalid VQ info
      //*** Todo exit with invalid status for this Frame

    end ;

  end ;

  // Read the Compression lookup table
  offset := 0 ;
//  for Counter :=0 to 3 do begin
    fs.Seek( Locations[2].Location + CompressionLookupTable[Counter].LookupTableOffset,
             soBeginning ) ;
    {$IFDEF OXYGENE}
      fs.Read( frameCache[Frame].CompressionTable, offset, 4096*4*4 ) ;
    {$ELSE}
      fs.ReadBuffer(frameCache[Frame].CompressionTable[offset],4096*4*4);
    {$ENDIF}
//     offset := offset + 4096 * 4 ;
//  end ;

  /////////////////////////////////////
  // Parse the Image Section Info
  /////////////////////////////////////
  // Read the image description header record
  fs.Seek( Locations[1].Location, soBeginning ) ;
  {$IFDEF OXYGENE}
    ImageDescriptionHeader.Read( fs ) ;
  {$ELSE}
    fs.ReadBuffer(ImageDescriptionHeader,28);
  {$ENDIF}
  doSwapW( ImageDescriptionHeader.NumberOfSpectralGroups               ) ;
  doSwapW( ImageDescriptionHeader.NumberOfSubframeTables               ) ;
  doSwapW( ImageDescriptionHeader.NumberOfSpectralBandTables           ) ;
  doSwapW( ImageDescriptionHeader.NumberOfSpectralBandLinesPerImageRow ) ;
  doSwapW( ImageDescriptionHeader.NumberOfSubframesEastWestDirection   ) ;
  doSwapW( ImageDescriptionHeader.NumberOfSubframesNorthSouthDirection ) ;
  doSwapC( ImageDescriptionHeader.NumberOfOutputColumnsPerSubframe     ) ;
  doSwapC( ImageDescriptionHeader.NumberOfOutputRowsPerSubframe        ) ;
  doSwapC( ImageDescriptionHeader.SubframeMaskTableOffset              ) ;
  doSwapC( ImageDescriptionHeader.TransparencyMaskTableOffset          ) ;

  // Do some sanity checking. Apparently there are old format files
  // where SubFrameMaskTableOffset was not present
  if ImageDescriptionHeader.SubframeMaskTableOffset = 0 then begin
    //*** Todo Invalid Frame file capture error message and mark as invalid

  end ;

  // Check subframeMaskTableOffset for indicator that all are present
  if ImageDescriptionHeader.SubframeMaskTableOffset = $ffffffff then
    frameCache[Frame].AllSubframesPresent := true
  else
    frameCache[Frame].AllSubframesPresent := false;

  // Initialize the subframe masked array to all false indicating all present
  for Counter :=0 to 5 do
    for Counter2 :=0 to 5 do
      {$IFDEF OXYGENE}
        frameCache[Frame].SubFrameMasked[Counter,Counter2] := false;
      {$ELSE}
        frameCache[Frame].SubFrameMasked[Counter][Counter2] := false;
      {$ENDIF}

  // If some subeframes are masked then see which ones
  if not frameCache[Frame].AllSubframesPresent then begin
    //Seek to the mask subsection. Perform some sanity check in case section wasn't
    //present in the locations section
    if Locations[5].Location = 0 then begin
      //*** Todo Invalid Frame file capture error message and mark as invalid

    end ;
    fs.Seek( Locations[5].Location, soBeginning ) ;
    // Skip to the offsets table
    fs.Seek( ImageDescriptionHeader.SubframeMaskTableOffset, soCurrent ) ;
    //Cycle trough the masks and adjust the subframemasked array booleans
    for Counter :=0 to 5 do begin
      for Counter2 :=0 to 5 do begin
        {$IFDEF OXYGENE}
         fs.ReadCardinal(SubFrameOffsets[Counter,Counter2],4);
        {$ELSE}
         fs.ReadBuffer(SubFrameOffsets[Counter][Counter2],4);
        {$ENDIF}
        {$IFDEF OXYGENE}
         doSwapC( SubFrameOffsets[Counter,Counter2] ) ;
        {$ELSE}
         doSwapC( SubFrameOffsets[Counter][Counter2] ) ;
        {$ENDIF}
        // If the offset is 0xffffffff then the subframe is masked
        {$IFDEF OXYGENE}
        if SubFrameOffsets[Counter,Counter2] = $ffffffff then
          frameCache[Frame].SubFrameMasked[Counter,Counter2] := true;
        {$ELSE}
        if SubFrameOffsets[Counter][Counter2] = $ffffffff then
          frameCache[Frame].SubFrameMasked[Counter][Counter2] := true;
        {$ENDIF}
      end ;
    end ;
  end ;

  //Seek to the Image subsection. Perform some sanity check in case section wasn't
  //present in the locations section
  if Locations[4].Location = 0 then begin
    //*** Todo Invalid Frame file capture error message and mark as invalid

  end ;

  // Go to the Image Display Parameters subsection
  // Not actually used but for completeness leave in for now. It also serves to position
  // to IMage spacial data location in case section wasn't in the locations section
  fs.Seek( Locations[4].Location, soBeginning ) ;
  {$IFDEF OXYGENE}
    IDPHeader.Read( fs ) ;
  {$ELSE}
    fs.ReadBuffer(IDPHeader,9);
  {$ENDIF}
  doSwapC( IDPHeader.NumberOfImageRows ) ;
  doSwapC( IDPHeader.NumberOfImageCodesPerRow ) ;

  // Get the image spacial data. After doing sanity check
  if Locations[3].Location = 0 then
    // Should log a warning or something but because I have read the previous IDP subsection
    // I should be at the Image Spacial Data subsection.
    // ****Note the Mitre sample parser didn't read the IDP header info and sought forward 14
    // Not sure where this came from as the IDP subsection is only 9 bytes long?
  else
    fs.Seek( Locations[3].Location, soBeginning ) ;

  //************
  // The following commented section contains logic for calculating size requirements
  // but they are fixed for CADRG and CIB spatial image data
  // So Hard coded in Frame structure to be 6144 bytes
  //************
  // Calculate the space required per row
  //BytesPerRow := (IDPHeader.NumberOfImageCodesPerRow * IDPHeader.ImageCodeBitLength) div 8;
  // Calculate the space required per subframe
  //BytesPerSubFrame := IDPHeader.NumberOfImageRows * BytesPerRow;
  // *** Note the BytesPerSubframe could probably be hardcoded at 6144 bytes
  // as the image display parameters for cadrg are always
  // (64imagecodes * 12bits)/8 * 64 ImageRows = 6144
  // but for completeness I calculate them anyway.
  //SetLength(SubFrameData,BytesPerSubframe);

  // Read the subframe data in rowwise fashion
  for Counter :=0 to 5 do begin
      for Counter2 :=0 to 5 do begin
        // Read only frames that aren't masked
        {$IFDEF OXYGENE}
          if not frameCache[Frame].SubFrameMasked[Counter,Counter2] then
            fs.Read( frameCache[Frame].SubFrameData, (Counter*6+Counter2)*6144, 6144 ) ;
        {$ELSE}
          if not frameCache[Frame].SubFrameMasked[Counter][Counter2] then
            fs.ReadBuffer(frameCache[Frame].SubFrameData[(Counter*6+Counter2)*6144],6144);
        {$ENDIF}
      end ;
  end ;

  /////////////////////////////////////
  // Parse the Color/Grayscale Section Info
  /////////////////////////////////////
  //Seek to the Color/Grayscale subsection. Perform some sanity check in case section wasn't
  //present in the locations section
  if Locations[6].Location = 0 then begin
    //*** Todo Invalid Frame file capture error message and mark as invalid

  end ;
  fs.Seek( Locations[6].Location, soBeginning ) ;
  {$IFDEF OXYGENE}
    ColorHeader.Read( fs ) ;
  {$ELSE}
    fs.ReadBuffer(ColorHeader,14);
  {$ENDIF}

  // Resize the array that holds the coloroffset records
  SetLength(ColorOffsets,ColorHeader.NumberOfColorOffsetRecords);

  //Seek to the ColorMap subsection. Perform some sanity check in case section wasn't
  //present in the locations section
  if Locations[7].Location = 0 then begin
    //*** Todo Invalid Frame file capture error message and mark as invalid

  end ;
  fs.Seek( Locations[7].Location, soBeginning ) ;
  {$IFDEF OXYGENE}
    ColorMapHeader.Read( fs ) ;
  {$ELSE}
    fs.ReadBuffer(ColorMapHeader,6);
  {$ENDIF}
  doSwapC( ColorMapHeader.ColorMapOffsetTableOffset ) ;
  doSwapW( ColorMapHeader.ColorOffsetRecordLength   ) ;

  // ****Todo check where reduced colormap is set in mitre parser
  // If reduced is false then we don't need ColorConverter info
  ReducedColorTable := false;
  if not ReducedColorTable then begin
    // Read in the coloroffset records
    for Counter := 0 to ColorHeader.NumberOfColorOffsetRecords - 1 do begin
      {$IFDEF OXYGENE}
        ColorOffsets[Counter].Read( fs ) ;
      {$ELSE}
        fs.ReadBuffer(ColorOffsets[Counter],17);
      {$ENDIF}
      doSwapW( ColorOffsets[Counter].ColorTableId ) ;
      doSwapC( ColorOffsets[Counter].NumberOfColorRecords ) ;
      doSwapW( ColorOffsets[Counter].HistogramRecordLength ) ;
      doSwapC( ColorOffsets[Counter].ColorTableOffset ) ;
      doSwapC( ColorOffsets[Counter].HistogramTableOffset ) ;

      if ( (ColorOffsets[Counter].NumberOfColorRecords = 216) or
        (ColorOffsets[Counter].NumberOfColorRecords = 217 ) ) then begin

        // Save the number of colors in the Frame record
        frameCache[Frame].NumberOfColors := ColorOffsets[Counter].NumberOfColorRecords;

        fs.Seek( Locations[7].Location + ColorOffsets[Counter].ColorTableOffset,
                 soBeginning
               ) ;
        for Counter2 :=0 to ColorOffsets[Counter].NumberOfColorRecords-1 do begin
          // If we have a CIB file read only the single Monochrome value else we assume
          // we have CADRG 4 byte rgbm value
          if FProductDataType = CADRG_PRODUCT_DATA_TYPE_CIB then begin
            // Read the Monochrome byte and fill in the green and blue with the same
            {$IFDEF OXYGENE}
              fs.ReadByte( frameCache[Frame].ColorTable[Counter2].Monochrome, 1 ) ;
            {$ELSE}
              fs.ReadBuffer(frameCache[Frame].ColorTable[Counter2].Monochrome,1);
            {$ENDIF}
            // Calculate grayscale values for RGB components
            frameCache[Frame].ColorTable[Counter2].Red   := frameCache[Frame].ColorTable[Counter2].Monochrome;
            frameCache[Frame].ColorTable[Counter2].Green := frameCache[Frame].ColorTable[Counter2].Monochrome;
            frameCache[Frame].ColorTable[Counter2].Blue  := frameCache[Frame].ColorTable[Counter2].Monochrome;
          end
          else begin
            // Read the red green blue and Monochrome values
            {$IFDEF OXYGENE}
              frameCache[Frame].ColorTable[Counter2].Read( fs ) ;
            {$ELSE}
              fs.ReadBuffer(frameCache[Frame].ColorTable[Counter2],4);
            {$ENDIF}
            // In order to make transparent color support work I need to slightly skew any color
            // that matches our predefined "Transparent" color. This was lesss coding than dynamically
            // adjusting the transparency table as new transparent values showed up in frames. Also
            // there seemed to be a bug where full black was an invalid tranparent value
            // therefore using a predefined value would address this problem.
            if ( ( frameCache[Frame].ColorTable[Counter2].Red = FTransparentRedValue ) and
                 ( frameCache[Frame].ColorTable[Counter2].Green = FTransparentGreenValue ) and
                 ( frameCache[Frame].ColorTable[Counter2].Blue = FTransparentBlueValue ) ) then begin
               // Skew the blue value + or - 1 intensity to differentiate it from the transparent value
              if FTransparentBlueValue > 0 then
                frameCache[Frame].ColorTable[Counter2].Blue := FTransparentBlueValue - 1
              else
                frameCache[Frame].ColorTable[Counter2].Blue := FTransparentBlueValue + 1;
            end ;
          end ;
        end ;
        if ColorOffsets[Counter].NumberOfColorRecords = 217 then begin
          frameCache[Frame].ColorTable[216].Red := FTransparentRedValue ;
          frameCache[Frame].ColorTable[216].Green := FTransparentGreenValue ;
          frameCache[Frame].ColorTable[216].Blue := FTransparentBlueValue ;
        end ;
        // Break out of Counter loop as we found the colortable info
        break;
      end ;
    end ;
  end // if not ReducedColorTable
  else begin
    // Read the Color Conversion table to find out which color table we should read

    //Seek to the Color Conversion subsection. Perform some sanity check in case section wasn't
    //present in the locations section
    if Locations[8].Location = 0 then begin
      //*** Todo Invalid Frame file capture error message and mark as invalid

    end ;
    fs.Seek( Locations[8].Location, soBeginning ) ;
    {$IFDEF OXYGENE}
      ColorConverterHeader.Read( fs ) ;
    {$ELSE}
      fs.ReadBuffer(ColorConverterHeader,8);
    {$ENDIF}
    doSwapC( ColorConverterHeader.ColorConverterOffsetTableOffset ) ;
    doSwapW( ColorConverterHeader.ColorConverterOffsetRecordLength ) ;
    doSwapW( ColorConverterHeader.ColorConverterRecordLength ) ;
  end ;

  // Destroy the stream object
  FreeObject(fs);

  // Frame is valid
  frameCache[Frame].IsFrameValid := true;
end ;

// Load and populate class members with TOC information
procedure TGIS_FileCADRGDecoder.loadToc;
var
  NITFTag   : array of Byte ;
  sNITFTag  : String ;
  NITFBuffer : array [0..1023] of Byte ;
  RPFHeaderLoc : Integer ;
  {$IFDEF JAVA}
    i          : Integer ;
  {$ENDIF}
  HeaderOffset   : Int64 ;
  {$IFDEF OXYGENE}
    Locations    : array of TGIS_FileCADRG_ComponentLocationRec ;
  {$ELSE}
    Locations    : array [0..3] of TGIS_FileCADRG_ComponentLocationRec ;
  {$ENDIF}
  BoundaryHeader : TGIS_FileCADRG_BoundaryLocationRec ;
  FrameFileHeader: TGIS_FileCADRG_FrameLocationRec ;
  Counter        : Word ;
  LongCounter    : Cardinal ;
  fs : TGIS_FileStream ;
begin
  fs := TGIS_FileStream.Create( PathCADRG,
                                  fmOpenRead or
                                  fmShareDenyWrite
                                ) ;

  // Determine NITF version if any. Assume not until proven wrong
  HeaderOffset := CADRG_TOC_NOT_NTIF_OFFSET;
  SetLength(NITFTag,7);
  {$IFDEF OXYGENE}
    fs.ReadBuffer( NITFTag, 7 ) ;
  {$ELSE}
    fs.ReadBuffer( NITFTag[0], 7 ) ;
  {$ENDIF}
  sNITFTag := ConvertAnsiString( NITFTag ) ;
  if ( sNITFTag = CADRG_NTIF2X_IDENTIFIER ) then
  begin
      fs.Seek( 360, soBeginning ) ; // the fixed portion of the NITF header ends here the rest is the
                                    // variable length portion. For simplicity of parsing read a 1K block
                                    // and find the RPFHDR TRE entry
      fs.ReadBuffer(NITFBuffer,Min( fs.Size-fs.Position, 1024 ));
      RPFHeaderLoc := SearchBuf(NITFBuffer,1024,0,0,'RPFHDR00048');
      if RPFHeaderLoc = -1 then begin
        // ***Todo raise exception RPFHeader TRE not found
      end ;
      HeaderOffset := 360 + RPFHeaderLoc + 11 ;
  end ;

  //seek to the TOC Header
  fs.Seek( HeaderOffset, soBeginning ) ;
  {$IFDEF OXYGENE}
    tocHeader.Read( fs ) ;
    fs.Seek( 4, soCurrent ) ;
  {$ELSE}
    fs.ReadBuffer(tocHeader,52);
  {$ENDIF}

  // Examine the Endian component and setup what if any byte swapping is required
  checkSwap(tocHeader.Endian);

  // Perform swapping if required on all numeric data types
  doSwapW( tocHeader.HeaderSectionLength ) ;
  doSwapC( tocHeader.LocationPhysical ) ;

  // Seek to the location section
  fs.Seek( tocHeader.LocationPhysical, soBeginning ) ;

  // Set up the array of sections/subsections IDs we care about
  {$IFDEF OXYGENE}
    SetLength( Locations, 4 ) ;
    {$IFDEF JAVA}
      for i := 0 to 3 do
        Locations[i] := new TGIS_FileCADRG_ComponentLocationRec() ;
    {$ENDIF}
  {$ENDIF}
  Locations[0].ID := CADRG_LOC_BOUNDARY_SECTION_SUBHEADER ;
  Locations[1].ID := CADRG_LOC_BOUNDARY_RECTANGLE_TABLE ;
  Locations[2].ID := CADRG_LOC_FRAME_FILE_INDEX_SUBHEADER ;
  Locations[3].ID := CADRG_LOC_FRAME_FILE_INDEX_SUBSECTION ;

  parseLocations( fs, Locations, 4 ) ;

  // ***TODO Validate that all Locations found else IsValid = False and stop here

  /////////////////////////////////////
  // Parse the Boundary Rectangle Info
  /////////////////////////////////////
  fs.Seek( Locations[0].Location, soBeginning ) ;
  {$IFDEF OXYGENE}
    BoundaryHeader.Read( fs ) ;
  {$ELSE}
    fs.ReadBuffer( BoundaryHeader, 8 ) ;
  {$ENDIF}
  doSwapC( BoundaryHeader.TableOffset ) ;
  doSwapW( BoundaryHeader.NumberOfRecords ) ;
  doSwapW( BoundaryHeader.RecordLength ) ;

  // Seek to the boundary rectangles table
  fs.Seek( Locations[1].Location, soBeginning ) ;

  // Resize the array of BoundaryRectangleRecords to hold rectangle info
  SetLength( BoundaryRectangleRecords, BoundaryHeader.NumberOfRecords ) ;
  {$IFDEF JAVA}
    for i := 0 to BoundaryHeader.NumberOfRecords-1 do
      BoundaryRectangleRecords[i] := new TGIS_FileCADRG_BoundaryRectangleRec ;
  {$ENDIF}

  //Populate the BoundaryRectangles array. Resized array is 0..n-1
  for Counter := 0 to BoundaryHeader.NumberOfRecords-1 do begin
    // TODO Might want to check record length of our structure against length in BoundaryHeader for Sanity
    {$IFDEF OXYGENE}
      BoundaryRectangleRecords[Counter].Read( fs ) ;
    {$ELSE}
      fs.ReadBuffer( BoundaryRectangleRecords[Counter], 132 ) ;
    {$ENDIF}
    doSwapD( BoundaryRectangleRecords[Counter].NWUpperLeftLatitude ) ;
    doSwapD( BoundaryRectangleRecords[Counter].NWUpperLeftLongitude ) ;
    doSwapD( BoundaryRectangleRecords[Counter].SWLowerLeftLatitude ) ;
    doSwapD( BoundaryRectangleRecords[Counter].SWLowerLeftLongitude ) ;
    doSwapD( BoundaryRectangleRecords[Counter].NEUpperRightLatitude ) ;
    doSwapD( BoundaryRectangleRecords[Counter].NEUpperRightLongitude ) ;
    doSwapD( BoundaryRectangleRecords[Counter].SELowerRightLatitude ) ;
    doSwapD( BoundaryRectangleRecords[Counter].SELowerRightLongitude ) ;
    doSwapD( BoundaryRectangleRecords[Counter].NSVerticalResolution ) ;
    doSwapD( BoundaryRectangleRecords[Counter].EWHorizontalResolution ) ;
    doSwapD( BoundaryRectangleRecords[Counter].LatitudeVerticalInterval ) ;
    doSwapD( BoundaryRectangleRecords[Counter].LongitudeHorizontalInterval ) ;
    doSwapC( BoundaryRectangleRecords[Counter].NumberOfFramesNS ) ;
    doSwapC( BoundaryRectangleRecords[Counter].NumberOfFramesEW ) ;

  end ;

  //Determine which Product DataType we are dealing with
  // Currently supports CADRG and CIB
  if ConvertAnsiString(BoundaryRectangleRecords[0].ProductDataType) = CADRG_CADRG_TAG then
    FProductDataType := CADRG_PRODUCT_DATA_TYPE_CADRG
  else
    if Trim( ConvertAnsiString( BoundaryRectangleRecords[0].ProductDataType ) ) = CADRG_CIB_TAG then
      FProductDataType := CADRG_PRODUCT_DATA_TYPE_CIB
    else begin
      //****TODO raise error condition unsupported product data type
    end ;

  /////////////////////////////////////
  // Parse the FrameFileIndex Info
  /////////////////////////////////////

  // Seek to the Frame File Index Header
  fs.Seek( Locations[2].Location, soBeginning ) ;
  {$IFDEF OXYGENE}
    FrameFileHeader.Read( fs ) ;
  {$ELSE}
    fs.ReadBuffer( FrameFileHeader, 13 ) ;
  {$ENDIF}
  doSwapC( FrameFileHeader.TableOffset ) ;
  doSwapC( FrameFileHeader.NumberOfFrameFileRecords ) ;
  doSwapW( FrameFileHeader.NumberOfpathNameRecords ) ;
  doSwapW( FrameFileHeader.FrameFileRecordLength ) ;

  //Resize arrays to hold Pathname and FrameFile records
  SetLength( FrameFileIndexRecords, FrameFileHeader.NumberOfFrameFileRecords ) ;
  SetLength( pathNameRecords, FrameFileHeader.NumberOfpathNameRecords ) ;

  {$IFDEF JAVA}
    for i := 0 to FrameFileHeader.NumberOfFrameFileRecords-1 do
      FrameFileIndexRecords[i] := new TGIS_FileCADRG_FrameFileIndexRec ;
    for i := 0 to FrameFileHeader.NumberOfpathNameRecords-1 do
      pathNameRecords[i] := new TGIS_FileCADRG_PathnameRec ;
  {$ENDIF}

  // Seek to the FrameFileIndex subsection
  fs.Seek( Locations[3].Location, soBeginning ) ;

  // Populate FrameFileIndex records
  for LongCounter := 0 to FrameFileHeader.NumberOfFrameFileRecords-1 do begin
    // TODO Might want to check record length of our structure against length in FramFileHeader for Sanity
    {$IFDEF OXYGENE}
      FrameFileIndexRecords[LongCounter].Read( fs ) ;
    {$ELSE}
      fs.ReadBuffer(FrameFileIndexRecords[LongCounter],33);
    {$ENDIF}
    doSwapW( FrameFileIndexRecords[LongCounter].BoundaryRectangleNumber ) ;
    doSwapW( FrameFileIndexRecords[LongCounter].FrameLocationRowNumber ) ;
    doSwapW( FrameFileIndexRecords[LongCounter].FrameLocationColumnNumber ) ;
    doSwapC( FrameFileIndexRecords[LongCounter].PathnameRecordOffset ) ;
  end ;

  // Populate the Pathname Records
  for Counter := 0 to FrameFileHeader.NumberOfpathNameRecords-1 do begin
    // Values stored in FramefileIndexRecord for the PathnameRecordOffset are relative to beginning
    // of FrameFileIndexSubsection
    pathNameRecords[Counter].Offset := fs.Position - Locations[3].Location ;
    fs.ReadWord( pathNameRecords[Counter].Length, 2 ) ;
    doSwapW( pathNameRecords[Counter].Length ) ;
    SetLength( pathNameRecords[Counter].Path, pathNameRecords[Counter].Length ) ;
    {$IFDEF OXYGENE}
      fs.ReadBuffer( pathNameRecords[Counter].Path, pathNameRecords[Counter].Length ) ;
    {$ELSE}
      fs.ReadBuffer( pathNameRecords[Counter].Path[0], pathNameRecords[Counter].Length ) ;
    {$ENDIF}
  end ;

  // Close and free the filestream
  FreeObject( fs ) ;

  // If we parsed the TOC Successfully then set validity flag
  isValid := True;

end ;

// Load and populate class members with TOC information (from DB)
//SQL
//------------------------------
// SELECT
//   dbo.tblCoverageCadrg.DataSourceId,
//   dbo.tblDataSources.LocalFolderName,
//   dbo.tblCoverageCadrg.MapSeriesId,
//   dbo.tblCoverageCadrg.LocationSpec,
//   dbo.tblMapSeriesCadrg.ShortName,
//   dbo.tblMapSeriesCadrg.Scale,
//   dbo.tblMapSeriesCadrg.ProductName,
//   dbo.tblCoverageCadrg.llLat,
//   dbo.tblCoverageCadrg.llLon,
//   dbo.tblCoverageCadrg.urLat,
//   dbo.tblCoverageCadrg.urLon
// FROM
//   dbo.tblCoverageCadrg
// INNER JOIN
//   dbo.tblDataSources ON dbo.tblCoverageCadrg.DataSourceId = dbo.tblDataSources.[Identity] INNER JOIN
//   dbo.tblMapSeriesCadrg ON dbo.tblCoverageCadrg.MapSeriesId = dbo.tblMapSeriesCadrg.[Identity]
//------------------------------
procedure TGIS_FileCADRGDecoder.loadDBToc;
var
  exists         : Boolean ;
  k              : Integer ;
  in_code_id     : Integer ;
  w_code_id      : Integer ;
  in_scale       : Integer ;
  w_scale        : Integer ;
  file_path      : String  ;
  meo            : TGIS_FileCADRG_Map ;
  meu            : TGIS_FileCADRG_Map ;
  slayer         : TGIS_FileCADRG_Sublayer    ;
  flist          : TGIS_List   ;
  eof            : Boolean ;
  in_frame       : TGIS_FileCADRG_SimpleFrame ;
  w_frame        : TGIS_FileCADRG_SimpleFrame ;

  procedure set_slayer ;
  var
    fw, fh : Double ;
    clon, clat : Double ;
    i : Integer ;
    sframe          : TGIS_FileCADRG_SimpleFrame ;
    cframe         : TGIS_FileCADRG_SimpleFrame ;
  begin
    sframe := TGIS_FileCADRG_SimpleFrame( flist.Items[0] ) ;

    slayer.SWLowerLeftLatitude := sframe.SWLowerLeftLatitude ;
    slayer.SWLowerLeftLongitude := sframe.SWLowerLeftLongitude ;

    slayer.NEUpperRightLatitude := sframe.NEUpperRightLatitude ;
    clat := sframe.NEUpperRightLatitude ;
    slayer.NEUpperRightLongitude := sframe.NEUpperRightLongitude ;
    clon := sframe.NEUpperRightLongitude ;
    slayer.NumberOfFramesNS := 1 ;
    slayer.NumberOfFramesEW := 1 ;

    if flist.Count > 1 then begin
      for i := 1 to flist.Count -1 do begin
        cframe := TGIS_FileCADRG_SimpleFrame( flist.Items[i] ) ;

        if clat < cframe.NEUpperRightLatitude then begin
          if slayer.NEUpperRightLatitude < cframe.NEUpperRightLatitude then begin
            slayer.NEUpperRightLatitude := cframe.NEUpperRightLatitude ;
            inc(slayer.NumberOfFramesNS) ;
          end ;
          clon := sframe.NEUpperRightLongitude ;
          clat := cframe.NEUpperRightLatitude ;
        end
        else
        if clon < cframe.NEUpperRightLongitude then begin
          if slayer.NEUpperRightLongitude < cframe.NEUpperRightLongitude then begin
            slayer.NEUpperRightLongitude := cframe.NEUpperRightLongitude ;
            inc(slayer.NumberOfFramesEW) ;
          end ;
        end ;
      end ;
    end ;
    fw := sframe.NEUpperRightLongitude - sframe.SWLowerLeftLongitude ;
    fh := sframe.NEUpperRightLatitude - sframe.SWLowerLeftLatitude ;

    slayer.NSVerticalResolution        := fh/1536 ;
    slayer.EWHorizontalResolution      := fw/1536 ;
    slayer.LatitudeVerticalInterval    := slayer.NSVerticalResolution ;
    slayer.LongitudeHorizontalInterval := slayer.EWHorizontalResolution ;

    slayer.FrameStartIndex := BDSimpleFramesList.Count ;
    slayer.FrameStopIndex  := slayer.FrameStartIndex +flist.Count -1 ;
    slayer.MapCADRG := meu ; //TGIS_DBMapCadrg
    for i := 0 to flist.Count -1 do
      BDSimpleFramesList.Add(flist.Items[i]) ;
  end ;

begin

  if assigned(BDSubLayersList) then
    BDSubLayersList.Clear
  else
    BDSubLayersList    := TGIS_ObjectList.Create ;
  if assigned(BDSimpleFramesList) then
    BDSimpleFramesList.Clear
  else
    BDSimpleFramesList := TGIS_ObjectList.Create ;

  flist := TGIS_List.Create ;
  meo := TGIS_FileCADRG_Map.Create ;
  in_frame := TGIS_FileCADRG_SimpleFrame.Create ;
  try
    FOnReadFrame(self, in_frame, eof) ;
    if not eof then
    begin

      w_frame := in_frame ;

      while True do begin
        in_code_id   := MapSeriesCadrg.GetSeriesCodeId(in_frame.SeriesCode) ;
        w_code_id := in_code_id ;
        in_scale := in_frame.Scale ;
        w_scale := in_scale ;
        slayer := nil ;
        flist.Clear ;
        while (in_code_id = w_code_id) and (in_scale = w_scale) do begin
          file_path := in_frame.Path ;
          exists   := SafeFileExists( file_path ) ;
          if exists then begin
            if slayer = nil then begin
              slayer := TGIS_FileCADRG_Sublayer.Create ;
              BDSubLayersList.Add(slayer) ;
            end ;
            //add frame to sublayer
            meo.Scale       := in_frame.Scale ;
            meo.ProductName := in_frame.ProductName ;
            if meo.ProductName = 'CIB' then
              meo.ProductNameId := 1
            else
              meo.ProductNameId := 0 ;
            meo.SeriesCode   := in_frame.SeriesCode ;
            meo.SeriesCodeId := MapSeriesCadrg.GetSeriesCodeId(meo.SeriesCode) ;
            meo.Enabled     := True ;
            meu := MapSeriesCadrg.AddUniqueMap(meo) ;
          end ;

          if flist.Count > 0 then begin
            if ( w_frame.NEUpperRightLongitude <> in_frame.SWLowerLeftLongitude )
               or
               ( w_frame.SWLowerLeftLatitude   <> in_frame.SWLowerLeftLatitude )
            then
              break ;
          end ;

          w_frame := TGIS_FileCADRG_SimpleFrame.Create ;

          w_frame.Path                  := file_path ;
          w_frame.SWLowerLeftLatitude   := in_frame.SWLowerLeftLatitude   ;
          w_frame.SWLowerLeftLongitude  := in_frame.SWLowerLeftLongitude  ;
          w_frame.NEUpperRightLatitude  := in_frame.NEUpperRightLatitude  ;
          w_frame.NEUpperRightLongitude := in_frame.NEUpperRightLongitude ;
          w_frame.Available := exists ;

          flist.Add(w_frame) ;

          FOnReadFrame(self, in_frame, eof) ;
          if eof then
            break ;

          w_code_id := in_code_id ;
          in_code_id   := MapSeriesCadrg.GetSeriesCodeId(in_frame.SeriesCode) ;
          w_scale := in_scale ;
          in_scale := in_frame.Scale ;

        end ; //end of frames of sublayer

        if slayer <> nil then begin
          set_slayer ;
        end
        else begin//free frames
          for k := flist.Count -1 downto 0 do
            FreeObjectNotNil( TGIS_FileCADRG_SimpleFrame(flist.Items[k]) ) ;
        end ;
        if eof then
          break ;
      end ;
    end ;
  finally
    FreeObject( meo ) ;
    FreeObject( flist ) ;
  end;
end ;

// Procedure to determine if byte swapping is required for
// Endian and Intel(ian) combination for numerical datatypes
procedure TGIS_FileCADRGDecoder.checkSwap(_endian: Byte);
var
  {$IFNDEF OXYGENE}
    pTestVal: PByte;
  {$ENDIF}
  TestVal: Word;
begin
  TestVal := 1; // Assign a value to a variable to test if swapping is done on this platform
  {$IFNDEF OXYGENE}
    pTestVal := @TestVal;
  {$ENDIF}

  {$IFDEF OXYGENE}
    if ( (Byte(TestVal) = 0 ) and (_endian <> 0) ) then
  {$ELSE}
    if ( (pTestVal^ = 0 ) and (_endian <> 0) ) then
  {$ENDIF}
    doByteSwapping := True
  else begin
    {$IFDEF OXYGENE}
      if ( (Byte(TestVal) = 1) and (_endian = 0) ) then
    {$ELSE}
      if ( (pTestVal^ = 1) and (_endian = 0) ) then
    {$ENDIF}
      doByteSwapping := True
  end ;
end ;

{$IFDEF OXYGENE}

    // Procedure to perform byte swapping for numerical datatypes
    // Uses member variable DoByteSwapping as test if swapping is required
    procedure TGIS_FileCADRGDecoder.doSwapW( var _word   : Word ) ;
    begin
      // Do swapping based on the doSwapW boolean
      if ( doByteSwapping ) then begin
        _word := (_word shr 8) +
                 (Byte(_word) shl 8)
      end ;
    end ;

    procedure TGIS_FileCADRGDecoder.doSwapC( var _cardinal  : Cardinal ) ;
    var
      a : array [0..3] of Byte ;
      i : Integer ;
      b : Byte ;
    begin
      // Do swapping based on the doSwapW boolean
      if ( doByteSwapping ) then begin
        a := BitConverter.GetBytes( _cardinal ) ;
        for i := 0 to 1 do begin // swap bytes
          b      := a[i]   ;
          a[i]   := a[3-i] ;
          a[3-i] := b      ;
        end ;
        _cardinal := BitConverter.ToUInt32( a, 0 ) ;
      end ;
    end ;

    procedure TGIS_FileCADRGDecoder.doSwapD( var _double    : Double ) ;
    var
      a : array [0..7] of Byte ;
      i : Integer ;
      b : Byte ;
    begin
      // Do swapping based on the doSwapW boolean
      if ( doByteSwapping ) then begin
        a := BitConverter.GetBytes( _double ) ;
        for i := 0 to 3 do begin // swap bytes
          b      := a[i]   ;
          a[i]   := a[7-i] ;
          a[7-i] := b      ;
        end ;
        _double := BitConverter.ToDouble( a, 0 ) ;
      end ;
    end ;
{$ELSE}
  // Procedure to perform byte swapping for numerical datatypes
  // Uses member variable DoByteSwapping as test if swapping is required
  procedure TGIS_FileCADRGDecoder.doSwapW( var _word   : Word ) ;
  var
    temp : Word ;
  begin

    // Do swapping based on the doSwapW boolean
    if ( doByteSwapping ) then begin
      temp := ((_word and $00FF) shl 8) or (_word shr 8) ;
      _word := temp ;
    end ;

  end ;

  procedure TGIS_FileCADRGDecoder.doSwapC( var _cardinal  : Cardinal ) ;
  var
    bin, bout : PByte ;
    tempin, tempout : Cardinal ;
    i : Integer ;
  begin

    // Do swapping based on the doSwapW boolean
    if ( doByteSwapping ) then begin

      tempin := _cardinal ;

      bin  := PByte(@tempin)  ;
      bout := PByte(@tempout) ;

      for i := 0 to 3 do
        PByte(NativeInt(bout) + i)^ := PByte(NativeInt(bin) +3 -i)^ ;

      _cardinal := tempout ;
    end ;
  end ;

  {$IFDEF NEXTGEN}
  procedure TGIS_FileCADRGDecoder.doSwapD( var _double    : Double ) ;
  var
    a : array [0..7] of Byte ;
    i : Integer ;
    b : Byte ;
  begin
    // Do swapping based on the doSwapW boolean
    if ( doByteSwapping ) then begin
      Move( _double, a[0], 8 ) ;
      for i := 0 to 3 do begin // swap bytes
        b      := a[i]   ;
        a[i]   := a[7-i] ;
        a[7-i] := b      ;
      end ;
      Move( a[0], _double, 8 ) ;
    end ;
  end ;
  {$ELSE}
  procedure TGIS_FileCADRGDecoder.doSwapD( var _double    : Double ) ;
  var
    bin, bout : PByte ;
    tempin, tempout : Double ;
    i : Integer ;
  begin

    // Do swapping based on the doSwapW boolean
    if ( doByteSwapping ) then begin

      tempin := _double ;

      bin  := PByte(@tempin ) ;
      bout := PByte(@tempout) ;

      for i := 0 to 7 do
        PByte(NativeInt(bout) + i)^ := PByte(NativeInt(bin) +7 -i)^ ;

      _double := tempout ;
    end ;

  end ;
  {$ENDIF}
{$ENDIF}

// Procedure to parse the Locations for the offsets to sections/subsections we
// care about. Takes an array of TLocationRec which contain constants for the IDs
// we care about and fills in the offset values as it finds them
//
// Assumption: fs is currrently positioned to the Location Section

  procedure TGIS_FileCADRGDecoder.parseLocations(
        _fs            : TGIS_FileStream ;
    var _locations     : array of TGIS_FileCADRG_ComponentLocationRec ;
        _locationCount : Word
  ) ;
var
  Idx: Word;
  Counter: Word;
  SectionCounter: Word;

  LocationSectionLength: Word;
  LocationTableOffset: Cardinal;
  ComponentLocationRecords: Word;
  ComponentLocationRecordLength: Word;
  ComponentAggregateLength: Cardinal;

  //Section/Component variables
  SectionId: Word;
  SectionLength: Cardinal;
  SectionPhysicalLocation: Cardinal;

begin
  // Initialize the offset and length values to 0 so we can validate
  // found entries later
  Idx := low(_locations) ;
  Counter := _locationCount ;
  while ( (Counter > 0) and (Idx <= high(_locations)) ) do
  begin
    _locations[Idx].Location := 0 ;
    _locations[Idx].Length := 0 ;
    Idx := Idx + 1 ;
    Counter := Counter - 1 ;
  end ;

  // Read the Location Section values
  // **Note If we are not using or storing numerical values they are not swapped
  // this just helps readability of file structure instead of seeking over skipped
  // elements. A little bit extra overhead for reads but only done once for code readability sake
  _fs.ReadWord( LocationSectionLength, 2 ) ;
  _fs.ReadCardinal( LocationTableOffset, 4 ) ;
  _fs.ReadWord( ComponentLocationRecords, 2 ) ;
  doSwapW( ComponentLocationRecords ) ;
  _fs.ReadWord( ComponentLocationRecordLength, 2 ) ;
  _fs.ReadCardinal( ComponentAggregateLength, 4 ) ;

  // Now go through each of the section records looking for IDs of types
  // that were requested in the IDs of the Locations array elements
  SectionCounter := ComponentLocationRecords ;
  while ( SectionCounter > 0 ) do
  begin
    _fs.ReadWord( SectionId, 2 ) ;
    doSwapW( SectionId ) ;
    _fs.ReadCardinal( SectionLength, 4 ) ;
    doSwapC( SectionLength ) ;
    _fs.ReadCardinal( SectionPhysicalLocation, 4 ) ;
    doSwapC( SectionPhysicalLocation ) ;

    // Look through the source array for a matching ID if found fill in offset
    // and length values into the array
    Idx := low( _locations ) ;
    Counter := _locationCount ;
    while ( (Counter > 0) and (Idx <= high(_locations)) ) do
    begin
      if ( _locations[Idx].ID = SectionId ) then
      begin
        _locations[Idx].Location := SectionPhysicalLocation ;
        _locations[Idx].Length := SectionLength ;
        Counter := 1 ;  // Found entry so force a While exit condition
      end ;
      Idx := Idx + 1 ;
      Counter := Counter - 1 ;
    end ;
    SectionCounter := SectionCounter - 1 ;
  end ;

end ;

function TGIS_FileCADRGDecoder.getFrameFilePath(
  _pathnameRecordOffset: Cardinal
) : String ;
var
  Counter: Word;
begin
  Result := '';
  // Loop through the Pathname entries until we find the one we want
  for Counter := 0 to high(pathNameRecords) do begin
    if ( _pathnameRecordOffset = pathNameRecords[Counter].Offset ) then begin
      Result := ConvertAnsiString( pathNameRecords[Counter].Path ) ;
      break;
    end ;
  end ;
end ;


//
// This procedure is used to handle retrieving frames into the Frame member from
// Either the framecache or by loading it from disk and saving it to the cache
//
procedure TGIS_FileCADRGDecoder.loadFrameViaCache(
  _frameFileIndex: Cardinal);
var
  Counter: Integer;
  Oldest: TDateTime;
begin

  //First See if the Frame is already in the cache
  for Counter :=0 to CADRG_FRAME_CACHE_SIZE do begin
    if _frameFileIndex = frameCache[Counter].FrameFileIndexID then begin;
      // Just to optimize a little further if it the same as the Frame already in the
      //member variable don't copy
      if frameCache[Frame].FrameFileIndexID <> frameCache[Counter].FrameFileIndexID then
        Frame := Counter ;
      exit;
    end ;
  end ;

  //If not found then load it from disk into the next available or oldest entry in the cache
  Frame := -1 ;
  for Counter :=0 to CADRG_FRAME_CACHE_SIZE do begin
    If frameCache[Counter].FrameFileIndexID = CADRG_CACHE_ENTRY_AVAILABLE then begin
      Frame := Counter ;
      break;
    end ;
  end ;

  // If there were no entries free in the cache find oldest
  if Frame = -1 then begin
    Oldest := frameCacheTimes[0];
    Frame := 0 ;
    for Counter := 1 to CADRG_FRAME_CACHE_SIZE do Begin
      If CompareDateTime(frameCacheTimes[Counter],Oldest) = LessThanValue then begin
        Oldest := frameCacheTimes[Counter];
        Frame := Counter;
      end ;
    end ;
  end ;

  assert( Frame <> -1 ) ;

  //Now call the routin e that loads the Frame from disk
  if not assigned(BDSubLayersList) then
    loadFrame(_frameFileIndex)
  else
    loadDBFrame(_frameFileIndex);

  // Store it in the cache for future use
  {$IFDEF OXYGENE}
    frameCacheTimes[Frame] := Time ;
  {$ELSE}
    frameCacheTimes[Frame] := Time ;
  {$ENDIF}
end ;

// This procedure will retrieve an entire row or subset thereof from the supplied
// FrameFileIndex, LineNumber, Start Offset in row for Count of rgb bytes into the supplied buffer
//
// No bounds validation is done here on the target buffer. It's callers responsibility
// to make sure it's big enough.
//
// The function returns the number of bytes placed in the buffer. If there are not enough
// bytes of color data in the supplied Frame to satisfy the number of bytes requested the function
// returns how many it supplied. It is up to the caller to calculate byte counts accordingly
//
// ****Note this function name is misleading for now. It actually fills the buffer in BGR order
// which matches the order required by the NewReadline function for 24 bit pixel values.
// this may be because it is using tagRGBTRIPLE which is a packed record with the order of it's
// members being Blue Green Red. ??? Probably a lack of Delphi understanding on my part. name
// tagRGBTRIPLE seems misleading to me.
// Should probably change method name to make it GetFrameRowBGR in case others want it in RGB order
// leave for now until I verify.
//
function TGIS_FileCADRGDecoder.GetFrameRowRGB(
                                  const _frmFileIndex : Cardinal ;
                                  const _lineNumber   : Cardinal ;
                                  const _start        : Integer  ;
                                  const _count        : Integer  ;
                                  const _buffer       : TBytes   ;
                                  const _offset       : Integer
                                ) : Integer ;
var
  off : Integer ;
  dataOffset  : Integer ;
  pixelOffset : Integer ;
  CBOffset    : Integer ;
  comprOffset : Integer ;
  SubFrameRow: Integer;
  SubFrameColumn: Integer;
  CBTable: Integer; // Codebook table counter
  CBIndex: Integer; // Codebook Index (column within 4x4 row)
  CBByteOffset: Integer;  // Calculated offset into subframe data codebook values
  SBRow: Integer; // Row counter for 0..63 of decompressed 4X4 subblocks
  SBColumn: Integer; // Column counter for 0..63 of decompressed 4X4 subblocks
  CodebookValue: Word;
  ColorTableIndex: Byte;
  PixelX: Integer;
  Counter: Integer;
  start : Integer ;
begin
  //Initialize the return vlue for number of bytes filled to 0
  result := 0;
  off := _offset ;
  start := _start mod 256 ;

  // Do a bit of validation
  if ( _lineNumber > 1535 ) then exit;
  if ( ( _start      < 0 ) or ( _start      > 1535 ) ) then exit;
  if _count = 0 then exit;

  //Attempt to load the selected file
  loadFrameViaCache( _frmFileIndex ) ;

  // Calculate which subframe the desired linenumber is in
  SubFrameRow := _lineNumber div 256;

  // Within that Row determine which of the 64 subframe blocks it appears in
  SBRow := ( _lineNumber div 4 ) mod 64 ;

  // Determine which of the 4 codebook offsets corresponds to the desired rownumber
  CBTable := _lineNumber mod 4 ;
  CBOffset := CBTable * 4096 * 4 ;

  // Loop through the The Subframe Columns and Subframe Blocks
  // Determine which subframe Column the start offset falls in
  for SubFrameColumn := ( _start div 256) to 5 do begin
    // Check if the subframe is masked or not. If not fill it in, if so fill it
    //with transparent values
     dataOffset := ( SubFrameRow * 6 + SubFrameColumn ) * 6144 ;
     pixelOffset := SubFrameColumn * 256 ;
    {$IFDEF OXYGENE}
      if not frameCache[Frame].SubFrameMasked[SubFrameRow, SubFrameColumn] then begin
    {$ELSE}
      if not frameCache[Frame].SubFrameMasked[SubFrameRow][SubFrameColumn] then begin
    {$ENDIF}
      for SBColumn := 0 to 63 do begin

        // Each 12bit code is 1.5 bytes in the subframe. 96 bytes per row
        CBByteOffset := (SBRow * 96) + ((SBColumn * 3) div 2);

        // Get the 12 bit codebook value. If we are an even boundary then the 12 bits
        // is my byte value << 4 + high order 4 bits of next in array
        // otherwise is 4 lower Bits << 8 + next 8bits
        if ( (SBColumn mod 2) = 0) then
          {$IFDEF OXYGENE}
            CodebookValue := (frameCache[Frame].SubFrameData[dataOffset+CBByteOffset]*16) +
              (frameCache[Frame].SubFrameData[dataOffset+CBByteOffset+1] div 16)
          {$ELSE}
            CodebookValue := (frameCache[Frame].SubFrameData[dataOffset+CBByteOffset]*16) +
              (frameCache[Frame].SubFrameData[dataOffset+CBByteOffset+1] div 16)
          {$ENDIF}
        else
          {$IFDEF OXYGENE}
            CodebookValue := ((frameCache[Frame].SubFrameData[dataOffset+CBByteOffset] mod 16)* 256) +
              (frameCache[Frame].SubFrameData[dataOffset+CBByteOffset+1]);
          {$ELSE}
            CodebookValue := ((frameCache[Frame].SubFrameData[dataOffset+CBByteOffset] mod 16)* 256) +
              (frameCache[Frame].SubFrameData[dataOffset+CBByteOffset+1]);
          {$ENDIF}

          comprOffset := CBOffset + CodebookValue * 4 ;
          for CBIndex := 0 to 3 do begin
            {$IFDEF OXYGENE}
              ColorTableIndex := frameCache[Frame].CompressionTable[comprOffset];
              inc( comprOffset ) ;
            {$ELSE}
              ColorTableIndex := frameCache[Frame].CompressionTable[comprOffset];
            {$ENDIF}

            // Sanity check. ColorTableIndex should never be > 216 (217 colors in 0 based array)
            if ColorTableIndex >= CADRG_COLOR_TABLE_ENTRIES then begin
              // ****TODO Raise exception and or flag an error and fill with transparent color

            end
            else begin
              {$IFDEF OXYGENE}
                PixelX := pixelOffset + CBIndex ;
              {$ELSE}
              // Calculate the pixel row and column values based on our position within the
              // Decompression phase
              PixelX := (SubFrameColumn * 256) + (SBColumn*4)+ (CBIndex);
              // ***Todo remove this but use as a sanity check to verify
              // my calculations for LineNUmber should = Pixely
              // PixelY := (SubframeRow * 256) + (SBRow*4) + (CBTable);
              {$ENDIF}

              if PixelX >= _start then begin
                // ***** Curious seems like I need to return in B G R order for tatuk.?
                // ** Leave for now as is but fix as needed after. Change method name if required
                if (ColorTableIndex <> 216) and frameCache[Frame].IsFrameValid then begin
                  _buffer[off] := frameCache[Frame].ColorTable[ColorTableIndex].Blue;
                  inc( off ) ;
                  _buffer[off] := frameCache[Frame].ColorTable[ColorTableIndex].Green;
                  inc( off ) ;
                  _buffer[off] := frameCache[Frame].ColorTable[ColorTableIndex].Red;
                  inc( off ) ;
                end
                else begin
                  _buffer[off] := FTransparentBlueValue;
                  inc( off ) ;
                  _buffer[off] := FTransparentGreenValue;
                  inc( off ) ;
                  _buffer[off] := FTransparentRedValue;
                  inc( off ) ;
                end ;

                Result := Result + 3; // Added 3 bytes to supplied buffer so add 3 to return value
                If Result >= _count then Exit;
              end ;
            end ; // If ColorTableIndex
          end ; // for CBIndex
        {$IFDEF OXYGENE}
          inc( pixelOffset, 4 ) ;
        {$ENDIF}
      end ; // for SBColumn
    end
    else begin
      //****Frame buffer is initialized with transparent values before this routine is called
      // so just advance the buffer and counters

     for Counter := start +1 to 256 do begin
         //Buffer^ := TransparentBlueValue;
        inc( off ) ;
        //Buffer^ := TransparentGreenValue;
        inc( off ) ;
        //Buffer^ := TransparentRedValue;
        inc( off ) ;
        Result := Result + 3; // Added 3 bytes to supplied buffer so add 3 to return value
        If Result >= _count then exit;
      end ;
      start := 0 ;
    end ;
  end ;
end ;


function TGIS_FileCADRGDecoder.GetFrameRowARGB(
                                  const _frmFileIndex : Cardinal ;
                                  const _lineNumber   : Cardinal ;
                                  const _start        : Integer  ;
                                  const _count        : Integer  ;
                                  const _buffer       : TGIS_Pixels  ;
                                  const _offset       : Integer
                                ) : Integer ;
var
  off : Integer ;
  dataOffset  : Integer ;
  CBOffset    : Integer ;
  comprOffset : Integer ;
  SubFrameRow: Integer;
  SubFrameColumn: Integer;
  CBTable: Integer; // Codebook table counter
  CBIndex: Integer; // Codebook Index (column within 4x4 row)
  CBByteOffset: Integer;  // Calculated offset into subframe data codebook values
  SBRow: Integer; // Row counter for 0..63 of decompressed 4X4 subblocks
  SBColumn: Integer; // Column counter for 0..63 of decompressed 4X4 subblocks
  CodebookValue: Word;
  ColorTableIndex: Byte;
  PixelX: Integer;
  Counter: Integer;
  start : Integer ;
begin
  //Initialize the return vlue for number of bytes filled to 0
  result := 0;
  off := _offset ;
  start := _start mod 256 ;

  // Do a bit of validation
  if ( _lineNumber > 1535 ) then exit;
  if ( ( _start      < 0 ) or ( _start      > 1535 ) ) then exit;
  if _count = 0 then exit;

  //Attempt to load the selected file
  loadFrameViaCache( _frmFileIndex ) ;

  // Calculate which subframe the desired linenumber is in
  SubFrameRow := _lineNumber div 256;

  // Within that Row determine which of the 64 subframe blocks it appears in
  SBRow := ( _lineNumber div 4 ) mod 64 ;

  // Determine which of the 4 codebook offsets corresponds to the desired rownumber
  CBTable := _lineNumber mod 4 ;
  CBOffset := CBTable * 4096 * 4 ;

  // Loop through the The Subframe Columns and Subframe Blocks
  // Determine which subframe Column the start offset falls in
  for SubFrameColumn := ( _start div 256) to 5 do begin
    // Check if the subframe is masked or not. If not fill it in, if so fill it
    //with transparent values
    dataOffset := ( SubFrameRow * 6 + SubFrameColumn ) * 6144 ;
    if not frameCache[Frame].SubFrameMasked[SubFrameRow, SubFrameColumn] then begin
      for SBColumn := 0 to 63 do begin

        // Each 12bit code is 1.5 bytes in the subframe. 96 bytes per row
        CBByteOffset := (SBRow * 96) + ((SBColumn * 3) div 2);

        // Get the 12 bit codebook value. If we are an even boundary then the 12 bits
        // is my byte value << 4 + high order 4 bits of next in array
        // otherwise is 4 lower Bits << 8 + next 8bits
        if ( (SBColumn mod 2) = 0) then
          CodebookValue := (frameCache[Frame].SubFrameData[dataOffset+CBByteOffset]*16) +
            (frameCache[Frame].SubFrameData[dataOffset+CBByteOffset+1] div 16)
        else
          CodebookValue := ((frameCache[Frame].SubFrameData[dataOffset+CBByteOffset] mod 16)* 256) +
            (frameCache[Frame].SubFrameData[dataOffset+CBByteOffset+1]);
          comprOffset := CBOffset + CodebookValue * 4 ;
          for CBIndex := 0 to 3 do begin
            ColorTableIndex := frameCache[Frame].CompressionTable[comprOffset];
            inc( comprOffset ) ;

            // Sanity check. ColorTableIndex should never be > 216 (217 colors in 0 based array)
            if ColorTableIndex >= CADRG_COLOR_TABLE_ENTRIES then begin
              // ****TODO Raise exception and or flag an error and fill with transparent color

            end
            else begin
              // Calculate the pixel row and column values based on our position within the
              // Decompression phase
              PixelX := (SubFrameColumn * 256) + (SBColumn*4)+ (CBIndex);
              // ***Todo remove this but use as a sanity check to verify
              // my calculations for LineNUmber should = Pixely
              // PixelY := (SubframeRow * 256) + (SBRow*4) + (CBTable);

              if PixelX >= _start then begin
                // ***** Curious seems like I need to return in B G R order for tatuk.?
                // ** Leave for now as is but fix as needed after. Change method name if required
                if (ColorTableIndex <> 216) and frameCache[Frame].IsFrameValid then begin
                  _buffer[off] :=
                    (Integer(frameCache[Frame].ColorTable[ColorTableIndex].Blue) shl 00) or
                    (Integer(frameCache[Frame].ColorTable[ColorTableIndex].Green) shl 08) or
                    (Integer(frameCache[Frame].ColorTable[ColorTableIndex].Red) shl 016) or
                    (Integer($FF000000)) ;
                  inc( off ) ;
                end
                else begin
                  _buffer[off] := $FFFFFF ;
                  inc( off ) ;
                end ;

                inc(Result) ; // Added 1 pixel to supplied buffer so add 1 to return value
                If Result >= _count then Exit;
              end ;
            end ; // If ColorTableIndex
          end ; // for CBIndex
      end ; // for SBColumn
    end
    else begin
      //****Frame buffer is initialized with transparent values before this routine is called
      // so just advance the buffer and counters

     for Counter := start +1 to 256 do begin
        inc( off ) ;
        inc(Result) ; // Added 1 pixel to supplied buffer so add 1 to return value
        If Result >= _count then exit;
      end ;
      start := 0 ;
    end ;
  end ;
end ;


function TGIS_FileCADRGDecoder.GetScaledFrameBlockARGB(
                                     const _frmIndex   : Integer ;
                                     const _buffer     : TGIS_Pixels  ;
                                     const _bwidth     : Integer ;
                                     const _bhidth     : Integer ;
                                     const _left       : Integer ;
                                     const _top        : Integer ;
                                     const _lineStart  : Integer ;
                                     const _lineCount  : Integer ;
                                     const _pixStart   : Integer ;
                                     const _pixCount   : Integer ;
                                     const _scale_y    : Single ;
                                     const _scale_x    : Single
                                   ) : TPoint;
var
  out_off : Integer ;
  dataOffset  : Integer ;
  pixelOffset : Integer ;
  pixOff_MaxA : Integer ;
  pixOff_MaxB : Integer ;
  pixOff_MaxC : Integer ;
  CBOffset    : Integer ;
  comprOffset : Integer ;
  SubFrameRow: Integer;
  SubFrameColumn: Integer;
  CBTable: Integer; // Codebook table counter
  CBIndex: Integer; // Codebook Index (column within 4x4 row)
  CBByteOffset: Integer;  // Calculated offset into subframe data codebook values
  SBRow: Integer; // Row counter for 0..63 of decompressed 4X4 subblocks
  SBColumn: Integer; // Column counter for 0..63 of decompressed 4X4 subblocks
  CodebookValue: Word;
  ColorTableIndex: Byte;
  PixelX: Integer;
  start256 : Integer ;
  curr_in_offset : Integer ;
  d_curr_offset : Integer ;
  sdx, sdy : Single ;
  soffset  : Single ;
  extra_run : Boolean ;
  off_limit : Integer ;
  curr_line : Integer ;
  start_out_off : Integer ;
  fcurr_line : Single ;
  aframe : TGIS_FileCADRG_Frame ;
begin
  //Initialize the return vlue for number of bytes filled to 0
 {$IFDEF GIS_NORECORDS}
   Result := new TPoint(0, 0) ;
 {$ELSE}
   Result.X := 0;
   Result.Y := 0;
 {$ENDIF}
  off_limit := _left +_pixCount ;
  sdx := 1/_scale_x ;
  sdy := 1/_scale_y ;

  curr_line  := _lineStart ;
  fcurr_line := curr_line ;
  curr_in_offset := _pixStart ;


  // Do a bit of validation
  if ( ( curr_in_offset      < 0 ) or ( curr_in_offset      >= 1536 ) ) then exit;
  if _pixCount = 0 then exit;

  //Attempt to load the selected file
  loadFrameViaCache( _frmIndex ) ;
  aframe := frameCache[Frame] ;

//Lines loop begin
  while curr_line < 1536 do begin
    out_off := (_top +Result.Y) * _bwidth +_left;
    start_out_off := out_off ;

    start256 := _pixStart mod 256 ;
    soffset := _pixStart ;
    curr_in_offset := _pixStart ;

    off_limit := out_off +_bwidth -_left ;

    // Calculate which subframe the desired linenumber is in
    SubFrameRow := curr_line div 256;

    // Within that Row determine which of the 64 subframe blocks it appears in
    SBRow := ( curr_line div 4 ) mod 64 ;

    // Determine which of the 4 codebook offsets corresponds to the desired rownumber
    CBTable := curr_line mod 4 ;
    CBOffset := CBTable * 4096 * 4 ;

    // Loop through the The Subframe Columns and Subframe Blocks
    // Determine which subframe Column the start offset falls in
    pixOff_MaxA := _pixStart +_pixCount ;
    SubFrameColumn := curr_in_offset div 256 ;
    extra_run := False ;

    for SubFrameColumn := ( curr_in_offset div 256) to 5 do begin
      if (curr_in_offset  div 256) <> SubFrameColumn then begin
        if (curr_in_offset  div 256) > 5 then begin
          break ;
        end
        else begin

          extra_run := True ;
          if (curr_in_offset  div 256) > SubFrameColumn then
            continue ;

          while (curr_in_offset  div 256) < SubFrameColumn do begin
            soffset := soffset +sdx ;
            curr_in_offset := RoundS(soffset) ;
          end;

        end;
      end;
      // Check if the subframe is masked or not. If not fill it in, if so fill it
      //with transparent values
      dataOffset := ( SubFrameRow * 6 + SubFrameColumn ) * 6144 ;
      pixelOffset := SubFrameColumn * 256 ;
      if not aframe.SubFrameMasked[SubFrameRow, SubFrameColumn] then begin
        pixOff_MaxB := (SubFrameColumn +1)*256 ;
        if pixOff_MaxB > pixOff_MaxA then
          pixOff_MaxB :=  pixOff_MaxA ;

        SBColumn := 0 ;
        while (curr_in_offset < pixOff_MaxB) and (SBColumn <= 63) do begin
          pixOff_MaxB := (SubFrameColumn +1)*256 ;
          if pixOff_MaxB > pixOff_MaxA then
          pixOff_MaxB :=  pixOff_MaxA ;

           if (curr_in_offset  div 256) <> SubFrameColumn then
             break ;

          // Each 12bit code is 1.5 bytes in the subframe. 96 bytes per row
          CBByteOffset := (SBRow * 96) + ((SBColumn * 3) div 2);

          // Get the 12 bit codebook value. If we are an even boundary then the 12 bits
          // is my byte value << 4 + high order 4 bits of next in array
          // otherwise is 4 lower Bits << 8 + next 8bits
          if ( (SBColumn mod 2) = 0) then
          begin
            CodebookValue := (aframe.SubFrameData[dataOffset+CBByteOffset]*16) +
              (aframe.SubFrameData[dataOffset+CBByteOffset+1] div 16) ;
          end
          else
          begin
            CodebookValue := ((aframe.SubFrameData[dataOffset+CBByteOffset] mod 16)* 256) +
              (aframe.SubFrameData[dataOffset+CBByteOffset+1]);
          end ;
          comprOffset := CBOffset + CodebookValue * 4 ;
          CBIndex := 0 ;
          pixOff_MaxC := (SubFrameColumn * 256) + ((SBColumn +1)*4);
          while (curr_in_offset < pixOff_MaxC) and (CBIndex <= 3 ) do begin
            ColorTableIndex := aframe.CompressionTable[comprOffset];
            inc( comprOffset ) ;

              // Sanity check. ColorTableIndex should never be > 216 (217 colors in 0 based array)
            if ColorTableIndex >= CADRG_COLOR_TABLE_ENTRIES then begin
                // ****TODO Raise exception and or flag an error and fill with transparent color

            end
            else begin
              // Calculate the pixel row and column values based on our position within the
              // Decompression phase
              PixelX := (SubFrameColumn * 256) + (SBColumn*4)+ (CBIndex);
              if PixelX >= curr_in_offset then begin
                  // ***** Curious seems like I need to return in B G R order for tatuk.?
                  // ** Leave for now as is but fix as needed after. Change method name if required
                if (ColorTableIndex <> 216) and aframe.IsFrameValid then begin
                  if out_off >= off_limit then
                    break ;

                  if out_off >= length(_buffer) then
                    exit ;

                  _buffer[out_off] :=
                    (Integer(aframe.ColorTable[ColorTableIndex].Blue) shl 00) or
                    (Integer(aframe.ColorTable[ColorTableIndex].Green) shl 08) or
                    (Integer(aframe.ColorTable[ColorTableIndex].Red) shl 016) or
                    (Integer($FF000000)) ;
                  inc( out_off ) ;
                end
                else begin
                  inc( out_off ) ;
                end ;

                if out_off >= off_limit then
                  break ;

                soffset := soffset +sdx ;
                d_curr_offset := start_out_off + RoundS(soffset) -curr_in_offset;
                curr_in_offset := RoundS(soffset) ;
                If curr_in_offset >= 1536 then
                  break ;
              end ; // If ColorTableIndex
            end ; // for CBIndex
            inc(CBIndex) ;
          end ; // for SBColumn
          inc(SBColumn) ;
        end ;
      end
      else begin
        //****Frame buffer is initialized with transparent values before this routine is called
        // so just advance the buffer and counters
        pixOff_MaxB := (SubFrameColumn +1)*256 ;
        if pixOff_MaxB > pixOff_MaxA then
          pixOff_MaxB :=  pixOff_MaxA ;

        SBColumn := 0 ;
        while (curr_in_offset < pixOff_MaxB) and (SBColumn <= 63) do begin
          pixOff_MaxB := (SubFrameColumn +1)*256 ;
          if pixOff_MaxB > pixOff_MaxA then
          pixOff_MaxB :=  pixOff_MaxA ;

           if (curr_in_offset  div 256) <> SubFrameColumn then
             break ;

          CBIndex := 0 ;
          pixOff_MaxC := (SubFrameColumn * 256) + ((SBColumn +1)*4);
          while (curr_in_offset < pixOff_MaxC) and (CBIndex <= 3 ) do begin
            PixelX := (SubFrameColumn * 256) + (SBColumn*4)+ (CBIndex);

           // ***Todo remove this but use as a sanity check to verify
            // my calculations for LineNUmber should = Pixely
            // PixelY := (SubframeRow * 256) + (SBRow*4) + (CBTable);

            if PixelX >= curr_in_offset then begin
              inc( out_off ) ;

              if out_off >= off_limit then
                break ;

              soffset := soffset +sdx ;
              d_curr_offset := start_out_off + RoundS(soffset) -curr_in_offset;
              curr_in_offset := RoundS(soffset) ;
              If curr_in_offset >= 1536 then
                break ;
            end ; // If ColorTableIndex
            inc(CBIndex) ;
          end ; // for SBColumn
          inc(SBColumn) ;
        end ;
      end;
    end;
    fcurr_line := fcurr_line +sdy ;
    curr_line := TruncS(fcurr_line) ;
    Result.Y := Result.Y +1;
    if (out_off -start_out_off) > Result.X then
      Result.X := out_off -start_out_off ;
    if Result.Y >= _bhidth -_top then begin
      fcurr_line := 0 ;
      break ;

    end;

  end ; //Lines loop end

end ;

function TGIS_FileCADRGDecoder.FillFrameBlockARGB(
                                     const _frmIndex   : Integer ;
                                     const _buffer     : TGIS_Pixels  ;
                                     const _bwidth     : Integer ;
                                     const _bhidth     : Integer ;
                                     const _left       : Integer ;
                                     const _top        : Integer ;
                                     const _lineStart  : Integer ;
                                     const _lineCount  : Integer ;
                                     const _pixStart   : Integer ;
                                     const _pixCount   : Integer ;
                                     const _scale_y    : Single ;
                                     const _scale_x    : Single
                                   ) : TPoint;
var
  out_off : Integer ;
  dataOffset  : Integer ;
  pixelOffset : Integer ;
  pixOff_MaxA : Integer ;
  pixOff_MaxB : Integer ;
  pixOff_MaxC : Integer ;
  CBOffset    : Integer ;
  SubFrameRow: Integer;
  SubFrameColumn: Integer;
  CBTable: Integer; // Codebook table counter
  CBIndex: Integer; // Codebook Index (column within 4x4 row)
  SBRow: Integer; // Row counter for 0..63 of decompressed 4X4 subblocks
  SBColumn: Integer; // Column counter for 0..63 of decompressed 4X4 subblocks
  PixelX: Integer;
  start256 : Integer ;
  curr_in_offset : Integer ;
  d_curr_offset : Integer ;
  sdx, sdy : Single ;
  soffset  : Single ;
  extra_run : Boolean ;
  off_limit : Integer ;
  curr_line : Integer ;
  start_out_off : Integer ;
  fcurr_line : Single ;
begin
  //Initialize the return vlue for number of bytes filled to 0
 {$IFDEF GIS_NORECORDS}
   Result := new TPoint(0, 0) ;
 {$ELSE}
   Result.X := 0;
   Result.Y := 0;
 {$ENDIF}
//  off := _top * _bwidth +_left;
//  start_off := off ;
  off_limit := _left +_pixCount ;
  sdx := 1/_scale_x ;
  sdy := 1/_scale_y ;

  curr_line  := _lineStart ;
  fcurr_line := curr_line ;
  curr_in_offset := _pixStart ;


  // Do a bit of validation
  if ( ( curr_in_offset      < 0 ) or ( curr_in_offset      >= 1536 ) ) then exit;
  if _pixCount = 0 then exit;

  //Attempt to load the selected file

//Lines loop begin
  while curr_line < 1536 do begin
    out_off := (_top +Result.Y) * _bwidth +_left;
    start_out_off := out_off ;

    start256 := _pixStart mod 256 ;
    soffset := _pixStart ;
    curr_in_offset := _pixStart ;

    off_limit := out_off +_bwidth -_left ;

    // Calculate which subframe the desired linenumber is in
    SubFrameRow := curr_line div 256;

    // Within that Row determine which of the 64 subframe blocks it appears in
    SBRow := ( curr_line div 4 ) mod 64 ;

    // Determine which of the 4 codebook offsets corresponds to the desired rownumber
    CBTable := curr_line mod 4 ;
    CBOffset := CBTable * 4096 * 4 ;

    // Loop through the The Subframe Columns and Subframe Blocks
    // Determine which subframe Column the start offset falls in
    pixOff_MaxA := _pixStart +_pixCount ;
    SubFrameColumn := curr_in_offset div 256 ;
    extra_run := False ;

    for SubFrameColumn := ( curr_in_offset div 256) to 5 do begin
      if (curr_in_offset  div 256) <> SubFrameColumn then begin
        if (curr_in_offset  div 256) > 5 then begin
          break ;
        end
        else begin

          extra_run := True ;
          if (curr_in_offset  div 256) > SubFrameColumn then
            continue ;

          while (curr_in_offset  div 256) < SubFrameColumn do begin
            soffset := soffset +sdx ;
            curr_in_offset := RoundS(soffset) ;
          end;

        end;
      end;
      // Check if the subframe is masked or not. If not fill it in, if so fill it
      //with transparent values
      dataOffset := ( SubFrameRow * 6 + SubFrameColumn ) * 6144 ;
      pixelOffset := SubFrameColumn * 256 ;
      pixOff_MaxB := (SubFrameColumn +1)*256 ;
      if pixOff_MaxB > pixOff_MaxA then
        pixOff_MaxB :=  pixOff_MaxA ;

      SBColumn := 0 ;
      while (curr_in_offset < pixOff_MaxB) and (SBColumn <= 63) do begin
        pixOff_MaxB := (SubFrameColumn +1)*256 ;
        if pixOff_MaxB > pixOff_MaxA then
          pixOff_MaxB :=  pixOff_MaxA ;

         if (curr_in_offset  div 256) <> SubFrameColumn then
           break ;

        CBIndex := 0 ;
        pixOff_MaxC := (SubFrameColumn * 256) + ((SBColumn +1)*4);

        while (curr_in_offset < pixOff_MaxC) and (CBIndex <= 3 ) do begin
            // Sanity check. ColorTableIndex should never be > 216 (217 colors in 0 based array)
          PixelX := (SubFrameColumn * 256) + (SBColumn*4)+ (CBIndex);
          if PixelX >= curr_in_offset then begin
              // ***** Curious seems like I need to return in B G R order for tatuk.?
              // ** Leave for now as is but fix as needed after. Change method name if required
            if out_off >= off_limit then
              break ;

            if out_off >= length(_buffer) then
              exit ;

            _buffer[out_off] := Integer(FILL_COLOR) ;
            inc( out_off ) ;

            if out_off >= off_limit then
              break ;

            soffset := soffset +sdx ;
            d_curr_offset := start_out_off + RoundS(soffset) -curr_in_offset;
            curr_in_offset := RoundS(soffset) ;
            If curr_in_offset >= 1536 then
              break ;
          end ; // for CBIndex
          inc(CBIndex) ;
        end ; // for SBColumn
        inc(SBColumn) ;
      end ;
    end;
    fcurr_line := fcurr_line +sdy ;
    curr_line := TruncS(fcurr_line) ;
    Result.Y := Result.Y +1;
    if (out_off -start_out_off) > Result.X then
      Result.X := out_off -start_out_off ;
    if Result.Y >= _bhidth -_top then
      break ;
  end ; //Lines loop end

end;




{==================================== END =====================================}
end.
