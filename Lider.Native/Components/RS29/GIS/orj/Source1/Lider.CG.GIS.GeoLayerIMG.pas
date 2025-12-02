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
  Encapsulation of an  ERDAS IMAGINE image files Layer (.IMG extension).
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerIMG ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerIMG"'}
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
    System.Types,
    System.Classes,
    System.SysUtils,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoTypesUI,
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
  GisLayerIMG = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  {$IFNDEF GENDOC}
    //IMG data format.
    T_IMGPixelType {$IFDEF OXYGENE} nested in TGIS_LayerIMG {$ENDIF} =
    (
      U1,
      U2,
      U4,
      U8,
      S8,
      U16,
      S16,
      U32,
      S32,
      F32,
      F64,
      C64
    ) ;
  {$ENDIF}

  // TGIS_LayerIMG supporting class; Image File Header Tag
  {#GENDOC:HIDE}
  TGIS_FileIMG_Ehfa_HeaderTag = packed record
    Signature : array [0..15] of Byte ; //'EHFA_HEADER_TAG'
    HeaderPtr : Integer ;
    {$IFDEF OXYGENE}
      function Read( _stream : TGIS_BaseStream ) : Integer ;
    {$ENDIF}
  end ;

  // TGIS_LayerIMG supporting class; Image File
  {#GENDOC:HIDE}
  TGIS_FileIMG_Ehfa_File = packed record
    Version           : Cardinal ;
    FreeListPtr       : Integer  ;
    RootEntryPtr      : Integer  ;
    EntryHeaderLength : Word     ;
    DictionaryPtr     : Integer  ;
    {$IFDEF OXYGENE}
      function Read( _stream : TGIS_BaseStream ) : Integer ;
    {$ENDIF}
  end ;

  { TGIS_LayerIMG supporting class; Image File Entry
  }
  {#GENDOC:HIDE}
  TGIS_FileIMG_Ehfa_Entry = packed record
    Next      : Integer   ;
    Prev      : Integer   ;
    Parent    : Integer   ;
    Child     : Integer   ;
    Data      : Integer   ; // =0 if none
    DataSize  : Cardinal  ; // in bytes
    Name      : array [0..63] of Byte ;
    DataType  : array [0..31] of Byte ;
    ModTime   : Cardinal  ; // seconds since 00:00:00 1 JAN 1970
                            // (UNIX time keeping)
    {$IFDEF OXYGENE}
      function Read( _stream : TGIS_BaseStream ) : Integer ;
    {$ENDIF}
  end ;

  { TGIS_LayerIMG supporting class; Image File base node
  }
  {#GENDOC:HIDE}
  TGIS_FileIMG_Eimg_Layer = packed record
    Width       : Cardinal ;
    Height      : Cardinal ;
    LayerType   : Word     ; //Enum type (16b) : thematic=0, unthematic=1,
                            //                   fft of real-valued data=2
    PixelType   : Word     ; //Enum type (16b) : IMG_PixelType - ord(imgPixelTypeU1)=0
    BlockWidth  : Cardinal ;
    BlockHeight : Cardinal ;
    {$IFDEF OXYGENE}
      function Read( _stream : TGIS_BaseStream ) : Integer ;
    {$ENDIF}
  end ;
  {#GENDOC:HIDE}
  TGIS_FileIMG_Eimg_Layer_SubSample = TGIS_FileIMG_Eimg_Layer ;

  {#GENDOC:HIDE}
  TGIS_FileIMG_Edms_VirtualBlockInfo = packed record
    FileCode        : Word     ;
    offset          : Integer  ;
    Size            : Cardinal ;
    Logvalid        : Word     ; //false=0, true=1
    CompressionType : Word     ; //no compression=0,
                                 //ESRI GRID compression=1
    {$IFDEF OXYGENE}
      function Read( _stream : TGIS_BaseStream ) : Integer ;
    {$ENDIF}
  end ;

  {$IFDEF DCC}
    {#GENDOC:HIDE}
    PGIS_FileIMG_Edms_VirtualBlockInfo = ^TGIS_FileIMG_Edms_VirtualBlockInfo ;
  {$ENDIF}

  { TGIS_LayerIMG supporting class
  }
  {#GENDOC:HIDE}
  TGIS_FileIMG_Edms_State = packed record
    NumVirtualBlocs    : Cardinal ;
    NumObjectsPerBlock : Cardinal ;
    NextObjectNum      : Cardinal ;
    CompressionType    : Word ; //Enum : no compression=0, RLC compression=1
    BlockInfo          : Integer ;
    FreeList           : Integer ;
    ModTime            : Cardinal ;
    {$IFDEF OXYGENE}
      function Read( _stream : TGIS_BaseStream ) : Integer ;
    {$ENDIF}
  end ;

  { TGIS_LayerIMG supporting class
  }
  {#GENDOC:HIDE}
  TGIS_FileIMG_Edsc_Column = packed record
    NumRows       : Cardinal ;
    ColumnDataPtr : Integer ;
    DataType      : Word ;      //The data type of this column
                                //0="integer" (EMIF_T_LONG)
                                //1="real" (EMIF_T_DOUBLE)
                                //2="complex" (EMIF_T_DCOMPLEX)
                                //3="string" (EMIF_T_CHAR)

    MaxNumChars   : Cardinal ;  //The maximum string length (for string data
                                //only). It is 0 if the type is not a String.
    {$IFDEF OXYGENE}
      function Read( _stream : TGIS_BaseStream ) : Integer ;
    {$ENDIF}
  end ;

  { TGIS_LayerIMG supporting class
  }
  {#GENDOC:HIDE}
  TGIS_FileIMG_Eprj_MapInfo = packed record
    projName : array of Byte ;
    upperLeftCenter : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
    lowerRightCenter : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
    pixelSize : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF} ;
    units : array of Byte ;
  end ;

  { TGIS_LayerIMG supporting class
  }
  {#GENDOC:HIDE}
  TGIS_FileIMG_Eprj_Spheroid = packed record
    sphereName : array of Byte ; //The name of the spheroid/ellipsoid.
                                 //This name iscan be found in:
                                 //<IMAGINE_HOME>/etc/spheroid.tab.
    a           : Double ; //The semi-major axis of the elipsoid in meters.
    b           : Double ; //The semi-minor axis of the elipsoid in meters.
    eSquared    : Double ; //The eccentricity of the ellipsoid, squared.
    radius      : Double ; //The radius of the spheroid in meters.
  end;

  { TGIS_LayerIMG supporting class
  }
  {#GENDOC:HIDE}
  TGIS_FileIMG_Eprj_ProParameters = packed record
    proType : Word ;  // This defines whether the projection is internal or external.
                      // 0 = "EPRJ_INTERNAL"
                      // 1=" EPRJ_EXTERNAL"
    proNumber : DWORD ; // The projection number for internal projections.
                        // 0="Geographic(Latitude/Longitude)"
                        // 1="UTM"
                        // 2="State Plane"
                        // 3="Albers Conical Equal Area"
                        // 4="Lambert Conformal Conic"
                        // 5="Mercator"
                        // 6="Polar Stereographic"
                        // 7="Polyconic"
                        // 8="Equidistant Conic"
                        // 9="Transverse Mercator"
                        // 10="Stereographic"
                        // 11="Lambert Azimuthal Equal-area"
                        // 12="Polar Stereographic"
                        // 13="Gnomonic"
                        // 14="Orthographic"
                        // 15="General Vertical Near-Side Perspective"
                        // 16="Sinusoidal"
                        // 17="Equirectangular"
                        // 18="Miller Cylindrical"
                        // 19="Van der Grinten I"
                        // 20="Oblique Mercator (Hotine)"
                        // 21="Space Oblique Mercator"
                        // 22="Modified Transverse Mercator"

      proExeName  : array of Byte ; //The name of the executable to run for
                                    // an externalprojection.
      proName     : array of Byte ; //The name of the projection. This will be
                                    //one of the names given above in the
                                    //description of proNumber.
      proZone     : DWORD ; //The zone number for internal State Plane or
                            //UTM projections.
      proParams   : array of Double ;

      proSpheroid : TGIS_FileIMG_Eprj_Spheroid ;
  end ;

  { TGIS_LayerIMG supporting class
  }
  {#GENDOC:HIDE}
  TGIS_FileIMG_Esta_Statistics = packed record
    minimum : Double ;
    maximum : Double ;
    mean    : Double ;
    median  : Double ;
    mode    : Double ;
    stddev  : Double ;
    {$IFDEF OXYGENE}
      function Read( _stream : TGIS_BaseStream ) : Integer ;
    {$ENDIF}
  end ;

  { Local type
  }
  {#GENDOC:HIDE}
  TAASmallInt = Array of Array of SmallInt ;

  /// <summary>
  ///   Encapsulation of IMG (ERDAS IMAGINE) layer.
  /// </summary>
  TGIS_LayerIMG = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerPixel )

    private // property internal values

        /// <summary>
        ///   Block width
        /// </summary>
        blockWidth : Integer ;

        /// <summary>
        ///   Blocks Length
        /// </summary>
        blockLength : Integer ;

        /// <summary>
        ///   Block columns number
        /// </summary>
        blocksColumns : Integer ;

        /// <summary>
        ///   Blocks rows number
        /// </summary>
        blocksRows : Integer ;

        /// <summary>
        ///   Block info
        /// </summary>
        blockInfoArray : array of array of TGIS_FileIMG_Edms_VirtualBlockInfo ;

        /// <summary>
        ///   Statistics
        /// </summary>
        estaStatisticsArray : array of TGIS_FileIMG_Esta_Statistics ;
    private // various private values

        /// <summary>
        ///   Number of data layers.
        /// </summary>
        layersNo : Integer ;

        /// <summary>
        ///   Number of data layers.
        /// </summary>
        usedLayersNo : Integer ;

        /// <summary>
        ///   Bits count to internal use.
        /// </summary>
        bitsCountInfo : Integer ;

        /// <summary>
        ///   line buffer (1 or 3 bands).
        /// </summary>
        bandsBBuffer : array of array of Byte ;

        /// <summary>
        ///   One band line buffer of singles.
        /// </summary>
        bandsFBuffer : array of Single ;

        /// <summary>
        ///   One band line buffer of integers.
        /// </summary>
        bandsIBuffer : array of Integer ;

        /// <summary>
        ///   One band line buffer of words.
        /// </summary>
        bandsWBuffer : array of Word ;

        /// <summary>
        ///   One band line buffer of short integer.
        /// </summary>
        bandsSBuffer : array of SmallInt ;

        /// <summary>
        ///   Index of left data block in bands buffer.
        /// </summary>
        lBlockIdx : Integer ;

        /// <summary>
        ///   Index of right data block in bands buffer.
        /// </summary>
        rBlockIdx : Integer ;

        /// <summary>
        ///   Pixel type enumeration.
        /// </summary>
        pixelType : T_IMGPixelType ;

        /// <summary>
        ///   Projection params.
        /// </summary>
        dEprjMapinfo  : TGIS_FileIMG_Eprj_MapInfo ;

        /// <summary>
        ///   Work arrays for decompressed data.
        /// </summary>
        decompressedBlock : array of array of Byte ;

        /// <summary>
        ///   Work arrays for decompressed short integer data -band 0.
        /// </summary>
        decompressedSBlock0 : TAASmallInt ;

        /// <summary>
        ///   Work arrays for decompressed short integer data -band 1.
        /// </summary>
        decompressedSBlock1 : TAASmallInt ;

        /// <summary>
        ///   Work arrays for decompressed short integer data -band 2.
        /// </summary>
        decompressedSBlock2 : TAASmallInt ;

        /// <summary>
        ///   Actual decompressed column
        /// </summary>
        actualColumn : Integer ;

        /// <summary>
        ///   Work arrays for decompressed single data.
        /// </summary>
        decompressedFBlock : array of array of Single ;

        /// <summary>
        ///   Work array of decompressed blocs indexes.
        /// </summary>
        decompressedFBlockIdx : array of Integer ;

        /// <summary>
        ///   Bands validation.
        /// </summary>
        bandValidationFlag : array of Boolean ;

        /// <summary>
        ///   Bands blocks in buffer.
        /// </summary>
        bandsColumnsBlocks : array of array of Integer ;

        /// <summary>
        ///   Block in buffer
        /// </summary>
        blockInBuffer : Integer ;

        /// <summary>
        ///   Color mappings; 0=Blue, 1=Green, 2=Red
        /// </summary>
        bandsIdxMapArray : array [0..2] of Integer ;

        /// <summary>
        ///   External data file exist ('*.ige')
        /// </summary>
        externalData : Boolean ;

        /// <summary>
        ///   External data file name
        /// </summary>
        externalDataFileName : String ;

        /// <summary>
        ///   External block size
        /// </summary>
        extBlockSize : Int64 ;

        /// <summary>
        ///   Band size for external data
        /// </summary>
        extBandSize : Int64 ;

        /// <summary>
        ///   External file size
        /// </summary>
        extFileSize : Int64 ;

        /// <summary>
        ///   External data position
        /// </summary>
        extDataPos  : Int64 ;

        /// <summary>
        ///   No palette defined
        /// </summary>
        noPalette : Boolean ;

        /// <summary>
        ///   Not known FMinZ and FMaxZ for big IMG grid (Size &gt;= 2147483647)
        /// </summary>
        noBigImgMinMax : Boolean ;

    private // various private values

      /// <summary>
      ///   Reads header file.
      /// </summary>
      function  readFileInfo      : Boolean ;

      /// <summary>
      ///   Sets coordinate and resolution.
      /// </summary>
      function  setCoordinate     : Boolean ;

      /// <summary>
      ///   Data decompressing.
      /// </summary>
      procedure decompressBlock( const _bandidx : Integer = 0;
                                 const _bidx    : Integer = 0
                               ) ;

      /// <summary>
      ///   Data decompressing.
      /// </summary>
      procedure decompressSBlock( const _bandidx : Integer = 0 ;
                                  const _bidx    : Integer = 0 ;
                                  const _bufidx  : Integer = 0
                                ) ;

      /// <summary>
      ///   Data decompressing.
      /// </summary>
      procedure decompressFBlock( const _bandidx : Integer = 0;
                                  const _bidx    : Integer = 0
                                ) ;

      /// <summary>
      ///   Data bytes reversing.
      /// </summary>
      procedure reverseBytes    ( const _bufidx  : Integer = 0;
                                  const _sidx    : Integer = 0
                                ) ;

      /// <summary>
      ///   Data words to bytes converting.
      /// </summary>
      procedure convertWords2Bytes
                                ( const _bufidx  : Integer = 0;
                                  const _sidx    : Integer = 0
                                ) ;

      /// <summary>
      ///   Data bytes reversing.
      /// </summary>
      procedure reverseHBytes  ( const _bufidx  : Integer = 0;
                                 const _sidx    : Integer = 0
                               ) ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF} // various protected routines

      /// <inheritdoc/>
      procedure setUp          ; override;

      /// <inheritdoc/>
      procedure setupParams    ; override;

      /// <inheritdoc/>
      function  setFileScale      ( const _dwidth : Double ;
                                    const _swidth : Double
                                  ) : Double ; override;

      /// <inheritdoc/>
      function  getLine           ( const   _buffer : TBytes  ;
                                    const _offset : Integer ;
                                    const   _linenr : Integer ;
                                    const   _start  : Integer ;
                                    const   _bytes  : Integer
                                  ) : Integer; override;

      /// <inheritdoc/>
      function  getLineBits      ( const _buffer   : TBytes  ;
                                   const _offset   : Integer ;
                                   const _linenr   : Integer ;
                                   const _pixStart : Integer ;
                                   const _pixCount : Integer
                                 ) : Integer ; override;
      /// <inheritdoc/>
      function  convertBitsToPixels
                                   ( const _buffSrc  : TBytes  ;
                                     const _srcOffset : Integer ;
                                     const _buffDst   : TGIS_Pixels ;
                                     const _dstOffset : Integer ;
                                     const _pixStart   : Integer ;
                                     const _pixCount  : Integer
                                   ) : Integer ; override ;

      /// <inheritdoc/>
      function  getLinePixels     ( const _buffer   : TGIS_Pixels  ;
                                    const _offset   : Integer ;
                                    const _linenr   : Integer ;
                                    const _pixStart : Integer ;
                                    const _pixCount : Integer
                                  ) : Integer; override;




      /// <summary>
      ///   Internal use only. For reading an image line.
      /// </summary>
      /// <returns>
      ///   number of read pixels
      /// </returns>
      /// <param name="_buffer">
      ///   pixels array
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
      function  getLinePixelsExt8( const _buffer   : TGIS_Pixels  ;
                                    const _offset   : Integer ;
                                    const _linenr   : Integer ;
                                    const _pixStart : Integer ;
                                    const _pixCount : Integer
                                  ) : Integer;



      /// <summary>
      ///   Reading an image line segment from indicated band to proper buffer.
      /// </summary>
      /// <param name="_tlinenr">
      ///   line number in block
      /// </param>
      /// <param name="_bandidx">
      ///   band number
      /// </param>
      /// <param name="_bufidx">
      ///   index to imagBandsBuffer array
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FILEREAD
      /// </exception>
      procedure  getImgLineSegment( const _tlinenr : Integer ;
                                    const _bandidx : Integer = 0;
                                    const _bufidx  : Integer = 0
                                  ) ;

      /// <summary>
      ///   Reading an image line segment from indicated band to proper buffer.
      /// </summary>
      /// <param name="_tlinenr">
      ///   line number in block
      /// </param>
      /// <param name="_bandidx">
      ///   band number
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FILEREAD
      /// </exception>
      procedure  getImgGLineSegment
                               (
                                 const _tlinenr : Integer ;
                                 const _bandidx : Integer = 0
                               ) ;

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

    public // various public routines

      /// <inheritdoc/>
      constructor Create  ; override;

  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoStreams,
    Lider.CG.GIS.GeoParams,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoCsBase,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoCsFactory,
    Lider.CG.GIS.GeoCsProjections,
    Lider.CG.GIS.GeoRegistredLayers ;
{$ENDIF}

type
  T_DataInfo = packed record
    count  : DWORD ;
    offset : DWORD ;
    {$IFDEF OXYGENE}
      function Read( _stream : TGIS_BaseStream ) : Integer ;
    {$ENDIF}
  end ;

const

  // IMG dependent names
  EHFA_HEADER_TAG           = 'EHFA_HEADER_TAG' ;
  ERDAS_IMG_EXTERN          = 'ERDAS_IMG_EXTERNAL_RASTER' ;
  IMG_EXTERNAL_RASTER       = 'ImgExternalRaster' ;
  EIMG_LAYER                = 'Eimg_Layer' ;
  EIMG_LAYER_SUBSAMPLE      = 'Eimg_Layer_SubSample' ;
  EIMG_NONINITIALIZEDVALUE  = 'Eimg_NonInitializedValue' ;
  EDMS_VIRTUALBLOCKINFO     = 'Edms_VirtualBlockInfo' ;
  EDMS_STATE                = 'Edms_State' ;
  EDSC_TABLE                = 'Edsc_Table' ;
  EDSC_BINFUNCTION          = 'Edsc_BinFunction' ;
  EDSC_COLUMN               = 'Edsc_Column' ;
  DESCRIPTOR_TABLE          = 'Descriptor_Table' ;
  ESTA_STATISTICS           = 'Esta_Statistics' ;
  EIMG_STATISTICS           = 'Eimg_StatisticsParameters840' ;
  EPRJ_MAPINFO              = 'Eprj_MapInfo' ;

  EPRJ_PROPARAMETERS        = 'Eprj_ProParameters' ;
  EPRJ_DATUM                = 'Eprj_Datum' ;
  EPRJ_SPHEROID             = 'Eprj_Spheroid ' ;

  // Extension of world file (extension file for pixel layer).
  WORLD_FILE_EXT_IMG = '.igw' ;

  // Extension of world file (extension file for pixel layer).
  // Alternative naming.
  WORLD_FILE_EXT2_IMG = '.imgw' ;

  // Extension of external data file.
  EXTERNAL_DATA_FILE_EXT = '.ige' ;

{$IFDEF OXYGENE}
//==============================================================================
// T_DataInfo
//==============================================================================

  function T_DataInfo.Read(
    _stream : TGIS_BaseStream
  ) : Integer ;
  begin
    Result := _stream.ReadCardinal( count,  4 ) +
              _stream.ReadCardinal( offset, 4 ) ;
  end ;

//==============================================================================
// TGIS_FileIMG_Ehfa_HeaderTag
//==============================================================================

  function TGIS_FileIMG_Ehfa_HeaderTag.Read(
    _stream : TGIS_BaseStream
  ) : Integer ;
  begin
    Signature := new Byte[16] ;
    Result := _stream.Read( Signature, 16 ) +
              _stream.ReadInteger( HeaderPtr, 4  ) ;
  end ;

//==============================================================================
// TGIS_FileIMG_Ehfa_File
//==============================================================================

  function TGIS_FileIMG_Ehfa_File.Read(
    _stream : TGIS_BaseStream
  ) : Integer ;
  begin
    Result := _stream.ReadCardinal( Version,           4 ) +
              _stream.ReadInteger( FreeListPtr,       4 ) +
              _stream.ReadInteger( RootEntryPtr,      4 ) +
              _stream.ReadWord( EntryHeaderLength, 2 ) +
              _stream.ReadInteger( DictionaryPtr,     4 ) ;
  end ;

//==============================================================================
// TGIS_FileIMG_Ehfa_Entry
//==============================================================================

  function TGIS_FileIMG_Ehfa_Entry.Read(
    _stream : TGIS_BaseStream
  ) : Integer ;
  begin
    Name := new Byte[64] ;
    DataType := new Byte[32] ;
    Result := _stream.ReadInteger( Next,     4  ) +
              _stream.ReadInteger( Prev,     4  ) +
              _stream.ReadInteger( Parent,   4  ) +
              _stream.ReadInteger( Child,    4  ) +
              _stream.ReadInteger( Data,     4  ) +
              _stream.ReadCardinal( DataSize, 4  ) +
              _stream.Read( Name,     64 ) +
              _stream.Read( DataType, 32 ) +
              _stream.ReadCardinal( ModTime,  4  ) ;
  end ;

//==============================================================================
// TGIS_FileIMG_Eimg_Layer
//==============================================================================

  function TGIS_FileIMG_Eimg_Layer.Read(
    _stream : TGIS_BaseStream
  ) : Integer ;
  begin
    Result := _stream.ReadCardinal( Width,       4 ) +
              _stream.ReadCardinal( Height,      4 ) +
              _stream.ReadWord( LayerType,   2 ) +
              _stream.ReadWord( PixelType,   2 ) +
              _stream.ReadCardinal( BlockWidth,  4 ) +
              _stream.ReadCardinal( BlockHeight, 4 ) ;
  end ;

//==============================================================================
// TGIS_FileIMG_Edms_VirtualBlockInfo
//==============================================================================

  function TGIS_FileIMG_Edms_VirtualBlockInfo.Read(
    _stream : TGIS_BaseStream
  ) : Integer ;
  begin
    Result := _stream.ReadWord( FileCode,        2 ) +
              _stream.ReadInteger( offset,          4 ) +
              _stream.ReadCardinal( Size,            4 ) +
              _stream.ReadWord( Logvalid,        2 ) +
              _stream.ReadWord( CompressionType, 2 ) ;
  end ;

//==============================================================================
// TGIS_FileIMG_Edms_State
//==============================================================================

  function TGIS_FileIMG_Edms_State.Read(
    _stream : TGIS_BaseStream
  ) : Integer ;
  begin
    Result := _stream.ReadCardinal( NumVirtualBlocs,    4 ) +
              _stream.ReadCardinal( NumObjectsPerBlock, 4 ) +
              _stream.ReadCardinal( NextObjectNum,      4 ) +
              _stream.ReadWord( CompressionType,    2 ) +
              _stream.ReadInteger( BlockInfo,          4 ) +
              _stream.ReadInteger( FreeList,           4 ) +
              _stream.ReadCardinal( ModTime,            4 ) ;
  end ;

//==============================================================================
// TGIS_FileIMG_Edsc_Column
//==============================================================================

  function TGIS_FileIMG_Edsc_Column.Read(
    _stream : TGIS_BaseStream
  ) : Integer ;
  begin
    Result := _stream.ReadCardinal( NumRows,       4 ) +
              _stream.ReadInteger( ColumnDataPtr, 4 ) +
              _stream.ReadWord( DataType,      2 ) +
              _stream.ReadCardinal( MaxNumChars,   4 ) ;
  end ;

//==============================================================================
// TGIS_FileIMG_Esta_Statistics
//==============================================================================

  function TGIS_FileIMG_Esta_Statistics.Read(
    _stream : TGIS_BaseStream
  ) : Integer ;
  begin
    Result := _stream.ReadDouble( minimum, 8 ) +
              _stream.ReadDouble( maximum, 8 ) +
              _stream.ReadDouble( mean,    8 ) +
              _stream.ReadDouble( median,  8 ) +
              _stream.ReadDouble( mode,    8 ) +
              _stream.ReadDouble( stddev,  8 ) ;
  end ;

{$ENDIF}

//==============================================================================
// TGIS_LayerIMG
//==============================================================================

  constructor TGIS_LayerIMG.Create ;
  begin
    inherited ;
    FSubType := FSubType +  [TGIS_LayerSubType.Persistent] ;

    Params.Pixel.GridNoValue := GIS_GRID_NOVALUE ;
    FNoDataValue   := GIS_GRID_NOVALUE ;
    redTransp[0]   := BASE_TRANSPARENT_FLAG ;
    greenTransp[0] := BASE_TRANSPARENT_FLAG ;
    blueTransp[0]  := BASE_TRANSPARENT_FLAG ;
  end ;

  function TGIS_LayerIMG.readFileInfo : Boolean ;
  var
    hdr_tag        : TGIS_FileIMG_Ehfa_HeaderTag ;
    hdr            : TGIS_FileIMG_Ehfa_File ;
    entry          : TGIS_FileIMG_Ehfa_Entry ;
    child          : TGIS_FileIMG_Ehfa_Entry ;
    fchild         : TGIS_FileIMG_Ehfa_Entry ;
    lchild         : TGIS_FileIMG_Ehfa_Entry ;
    layers         : array of TGIS_FileIMG_Ehfa_Entry ;
    lastchild      : Integer ;
    data_name      : String ;
    units_name     : String ;
    ellipsoid_name : String ;
    datum_name     : String ;
    datum_name_arr : Array of Byte ;
    data_type      : String ;
    d_eimg_layer   : TGIS_FileIMG_Eimg_Layer ;
    d_edms_state   : TGIS_FileIMG_Edms_State ;
    layeridx       : Integer ;
    bufflength     :  Integer ;
    block_pixels   : Integer ;
    inforec        : T_DataInfo ;
    img_projparam  : TGIS_FileIMG_Eprj_ProParameters ;
    proj_param     : TGIS_CSProjParameters ;
    ellipsoid      : TGIS_CSAbstract ;
    datum          : TGIS_CSAbstract ;
    gcs            : TGIS_CSAbstract ;
    units          : TGIS_CSAbstract ;
    prj            : Integer         ;
    proj_unknown   : Boolean ;
    from_trim      : String ;
    pType : T_IMGPixelType ;
  const
    US_SURVEY_FEET = 'us_survey_feet' ;
    METERS = 'meters' ;

    procedure set_ext_data_pos ;
    var
      d_name : Array [0..63] of Byte ;
      n_len : Integer ;
    begin
      fileStream.Seek(child.Data +8, soBeginning) ;
      {$IFDEF OXYGENE}
        fileStream.Read( d_name, 64) ;
      {$ELSE}
        fileStream.Read( d_name[0], 64) ;
      {$ENDIF}
      n_len := 0 ;
      while d_name[n_len] <> 0 do
        inc(n_len) ;
      fileStream.Seek(child.Data +n_len +17, soBeginning) ;
      fileStream.ReadInt64( extDataPos, 8) ;
    end;
    procedure read_column ;
    var
      column   : TGIS_FileIMG_Edsc_Column ;
      n        : Integer ;
      ccolor   : Double ;
      cgreen : DWORD ;
      cblue  : DWORD ;
      cred   : DWORD ;
      procedure reset_palette ;
      var
        i : Integer ;
      begin
        for i := 0 to column.NumRows -1 do
          bitPalette[i] := TGIS_Color.Black ;
        noPalette := False ;
      end ;
    begin
      fileStream.Seek(lchild.Data, soBeginning) ;
      {$IFDEF OXYGENE}
        column.Read( fileStream ) ;
      {$ELSE}
        fileStream.Read(column, sizeOf(TGIS_FileIMG_Edsc_Column));
      {$ENDIF}
      fileStream.Seek(column.ColumnDataPtr, soBeginning) ;

      from_trim := Trim(ConvertAnsiString(lchild.Name)) ;
      if from_trim = 'Red' then
      begin
        if noPalette then
          reset_palette ;
        for n := 0 to column.NumRows -1 do begin
          fileStream.ReadDouble(ccolor, sizeOf(ccolor));
          cred   := (TruncS($FF*ccolor)) shl 16 ;
          bitPalette[n].ARGB :=  bitPalette[n].ARGB or cred ; ;
        end ;
      end
      else
      if from_trim = 'Green' then
      begin
        if noPalette then
          reset_palette ;
        for n := 0 to column.NumRows -1 do begin
          fileStream.ReadDouble(ccolor, sizeOf(ccolor));
          cgreen := (TruncS($FF*ccolor)) shl 8 ;
          bitPalette[n].ARGB := bitPalette[n].ARGB or cgreen ;
        end ;
      end
      else
      if from_trim = 'Blue' then
      begin
        if noPalette then
          reset_palette ;
        for n := 0 to column.NumRows -1 do begin
          fileStream.ReadDouble(ccolor, sizeOf(ccolor));
          cblue  := TruncS($FF*ccolor) ;
          bitPalette[n].ARGB := bitPalette[n].ARGB or cblue ;
        end ;
      end ;
    end ;
    procedure read_data ;
    {$IFDEF OXYGENE}
    var
      j : Integer ;
      i : Integer ;
    {$ENDIF}
    begin
      if data_type = EDMS_STATE then begin
        fileStream.Seek(child.Data, soBeginning) ;
        {$IFDEF OXYGENE}
          d_edms_state.Read( fileStream ) ;
        {$ELSE}
          fileStream.Read(d_edms_state, sizeOf(TGIS_FileIMG_Edms_State));
        {$ENDIF}
        fileStream.Seek(d_edms_state.FreeList, soBeginning) ;
        SetLength(blockInfoArray[layeridx], d_edms_state.NumVirtualBlocs ) ;
        {$IFDEF OXYGENE}
          for j := 0 to d_edms_state.NumVirtualBlocs-1 do begin
            {$IFDEF GIS_NORECORDS}
              blockInfoArray[layeridx][j] := new TGIS_FileIMG_Edms_VirtualBlockInfo ;
            {$ENDIF}
            blockInfoArray[layeridx][j].Read( fileStream ) ;
          end ;
        {$ELSE}
          fileStream.Read(
             blockInfoArray[layeridx][0],
             d_edms_state.NumVirtualBlocs
             * sizeOf( TGIS_FileIMG_Edms_VirtualBlockInfo )
          ) ;
        {$ENDIF}
      end
      else
      if data_type = EPRJ_DATUM then begin
        fileStream.Seek(child.Data, soBeginning) ;
        {$IFDEF OXYGENE}
          inforec.Read( fileStream ) ;
        {$ELSE}
          fileStream.Read(inforec, sizeOf(inforec));
        {$ENDIF}
        fileStream.Seek(inforec.offset, soBeginning) ;

        SetLength(datum_name_arr, inforec.count) ;
        {$IFDEF OXYGENE}
          fileStream.Read(datum_name_arr, inforec.count ) ;
        {$ELSE}
          fileStream.Read(datum_name_arr[0], inforec.count);
        {$ENDIF}
        datum_name := Trim(ConvertAnsiString(datum_name_arr)) ;
      end
      else
      if data_type = EPRJ_MAPINFO then begin

        fileStream.Seek(child.Data, soBeginning) ;
        {$IFDEF OXYGENE}
          inforec.Read( fileStream ) ;
        {$ELSE}
          fileStream.Read(inforec, sizeOf(inforec));
        {$ENDIF}
        fileStream.Seek(inforec.offset, soBeginning) ;

        SetLength(dEprjMapinfo.projName, inforec.count) ;
        {$IFDEF OXYGENE}
          fileStream.Read( dEprjMapinfo.projName, inforec.count ) ;
        {$ELSE}
          fileStream.Read(dEprjMapinfo.projName[0], inforec.count);
        {$ENDIF}

        {$IFDEF OXYGENE}
          inforec.Read( fileStream ) ;
        {$ELSE}
          fileStream.Read(inforec, sizeOf(inforec));
        {$ENDIF}
        fileStream.Seek(inforec.offset, soBeginning) ;
        {$IFDEF OXYGENE}
          dEprjMapinfo.upperLeftCenter := GisPoint(0, 0) ;
          fileStream.ReadDouble( dEprjMapinfo.upperLeftCenter.X ) ;
          fileStream.ReadDouble( dEprjMapinfo.upperLeftCenter.Y ) ;
        {$ELSE}
          fileStream.Read(dEprjMapinfo.upperLeftCenter,
                          sizeOf(dEprjMapinfo.upperLeftCenter));
        {$ENDIF}

        {$IFDEF OXYGENE}
          inforec.Read( fileStream ) ;
        {$ELSE}
          fileStream.Read(inforec, sizeOf(inforec));
        {$ENDIF}
        fileStream.Seek(inforec.offset, soBeginning) ;
        {$IFDEF OXYGENE}
          dEprjMapinfo.lowerRightCenter := GisPoint(0, 0) ;
          fileStream.ReadDouble( dEprjMapinfo.lowerRightCenter.X ) ;
          fileStream.ReadDouble( dEprjMapinfo.lowerRightCenter.Y ) ;
        {$ELSE}
          fileStream.Read(dEprjMapinfo.lowerRightCenter,
                          sizeOf(dEprjMapinfo.lowerRightCenter));
        {$ENDIF}

        {$IFDEF OXYGENE}
          inforec.Read( fileStream ) ;
        {$ELSE}
          fileStream.Read(inforec, sizeOf(inforec));
        {$ENDIF}
        fileStream.Seek(inforec.offset, soBeginning) ;
        {$IFDEF OXYGENE}
          dEprjMapinfo.pixelSize := GisPoint(0, 0) ;
          fileStream.ReadDouble( dEprjMapinfo.pixelSize.X ) ;
          fileStream.ReadDouble( dEprjMapinfo.pixelSize.Y ) ;
        {$ELSE}
          fileStream.Read(dEprjMapinfo.pixelSize,
                          sizeOf(dEprjMapinfo.pixelSize));
        {$ENDIF}

        {$IFDEF OXYGENE}
          inforec.Read( fileStream ) ;
        {$ELSE}
          fileStream.Read(inforec, sizeOf(inforec));
        {$ENDIF}
        fileStream.Seek(inforec.offset, soBeginning) ;

        SetLength(dEprjMapinfo.units, inforec.count ) ;
        {$IFDEF OXYGENE}
          fileStream.Read( dEprjMapinfo.units, inforec.count ) ;
        {$ELSE}
          fileStream.Read(dEprjMapinfo.units[0], inforec.count );
        {$ENDIF}
      end
      else
      if data_type = ESTA_STATISTICS then begin
        fileStream.Seek(child.Data, soBeginning) ;
        {$IFDEF OXYGENE}
          estaStatisticsArray[layeridx].Read( fileStream ) ;
        {$ELSE}
          fileStream.Read( estaStatisticsArray[layeridx],
                           sizeOf( TGIS_FileIMG_Esta_Statistics )
                         ) ;
        {$ENDIF}
      end
      else
      if data_type = EDSC_TABLE then begin
        if data_name = DESCRIPTOR_TABLE then begin
          fileStream.Seek(child.Child, soBeginning) ;
          {$IFDEF OXYGENE}
            lchild.Read( fileStream ) ;
          {$ELSE}
            fileStream.Read( lchild, sizeOf(lchild)) ;
          {$ENDIF}
          from_trim :=  Trim(ConvertAnsiString(lchild.DataType)) ;
          if from_trim = EDSC_COLUMN then
            read_column ;
          while lchild.Next <> 0 do begin
            lastchild := lchild.Child ;
            fileStream.Seek(lchild.Next, soBeginning) ;
            {$IFDEF OXYGENE}
              lchild.Read( fileStream ) ;
            {$ELSE}
              fileStream.Read( lchild, sizeOf(lchild)) ;
            {$ENDIF}

            from_trim :=  Trim(ConvertAnsiString(lchild.DataType)) ;
            if from_trim = EDSC_COLUMN then
              read_column ;

            { TODO -cReevaluate : some IMG has bad child ending }
            if lastchild = lchild.Child then
              break ;
          end ;
        end ;
      end
      else
      if data_type = IMG_EXTERNAL_RASTER then begin
         set_ext_data_pos ;
         externalData := True ;
      end
      else
      if data_type = EPRJ_PROPARAMETERS then begin
        proj_unknown := False ;

        fileStream.Seek(child.Data, soBeginning) ;

        fileStream.ReadWord( img_projparam.proType, sizeOf(Word)) ;
        fileStream.ReadCardinal( img_projparam.proNumber, sizeOf(DWORD)) ;

        {$IFDEF OXYGENE}
          inforec.Read( fileStream ) ;
        {$ELSE}
          fileStream.Read(inforec, sizeOf(inforec));
        {$ENDIF}

        if inforec.count > 0 then
        begin
          fileStream.Seek(inforec.offset, soBeginning) ;
          if inforec.count > 0 then begin
            SetLength(img_projparam.proExeName,inforec.count) ;
            {$IFDEF OXYGENE}
              fileStream.Read(img_projparam.proExeName,inforec.count) ;
            {$ELSE}
              fileStream.Read(img_projparam.proExeName[0],inforec.count) ;
            {$ENDIF}
          end ;
        end;

        {$IFDEF OXYGENE}
          inforec.Read( fileStream ) ;
        {$ELSE}
          fileStream.Read(inforec, sizeOf(inforec));
        {$ENDIF}

        if inforec.count > 0 then
        begin
          fileStream.Seek(inforec.offset, soBeginning) ;
          if inforec.count > 0 then begin
            SetLength(img_projparam.proName,inforec.count) ;
            {$IFDEF OXYGENE}
              fileStream.Read(img_projparam.proName,inforec.count) ;
            {$ELSE}
              fileStream.Read(img_projparam.proName[0],inforec.count) ;
            {$ENDIF}
          end ;
        end;

         fileStream.ReadCardinal( img_projparam.proZone, sizeOf(DWORD)) ;

        {$IFDEF OXYGENE}
          inforec.Read( fileStream ) ;
        {$ELSE}
          fileStream.Read(inforec, sizeOf(inforec));
        {$ENDIF}

        if inforec.count > 0 then
        begin
          fileStream.Seek(inforec.offset, soBeginning) ;
          if inforec.count > 0 then begin
            SetLength(img_projparam.proParams,inforec.count) ;
            {$IFDEF OXYGENE}
              for i := 0 to inforec.count - 1 do
                fileStream.ReadDouble(img_projparam.proParams[i]) ;
            {$ELSE}
              fileStream.Read(img_projparam.proParams[0],inforec.count*sizeOf(Double)) ;
            {$ENDIF}
          end ;
        end;

        {$IFDEF OXYGENE}
          inforec.Read( fileStream ) ;
        {$ELSE}
          fileStream.Read(inforec, sizeOf(inforec));
        {$ENDIF}

        fileStream.Seek(inforec.offset, soBeginning) ;

        {$IFDEF OXYGENE}
          inforec.Read( fileStream ) ;
        {$ELSE}
          fileStream.Read(inforec, sizeOf(inforec));
        {$ENDIF}

        if inforec.count > 0 then
        begin
          fileStream.Seek(inforec.offset, soBeginning) ;
          if inforec.count > 0 then begin
            SetLength(img_projparam.proSpheroid.sphereName,inforec.count) ;
            {$IFDEF OXYGENE}
              fileStream.Read(img_projparam.proSpheroid.sphereName,inforec.count) ;
            {$ELSE}
              fileStream.Read(img_projparam.proSpheroid.sphereName[0],inforec.count) ;
            {$ENDIF}
            ellipsoid_name := Trim(ConvertAnsiString(img_projparam.proSpheroid.sphereName)) ;
            {$IFDEF OXYGENE}
              ellipsoid_name := StringReplace( ellipsoid_name, ' ', '_', [TReplaceFlag.rfReplaceAll] ) ;
            {$ELSE}
              ellipsoid_name := StringReplace( ellipsoid_name, ' ', '_', [rfReplaceAll] ) ;
            {$ENDIF}
          end ;
        end;
        //The semi-major axis of the elipsoid in meters.
        fileStream.ReadDouble(img_projparam.proSpheroid.a , sizeOf(Double)) ;
        //The semi-minor axis of the elipsoid in meters.
        fileStream.ReadDouble(img_projparam.proSpheroid.b , sizeOf(Double)) ;
        //The eccentricity of the ellipsoid, squared.
        fileStream.ReadDouble(img_projparam.proSpheroid.eSquared , sizeOf(Double)) ;
        //The radius of the spheroid in meters.
        fileStream.ReadDouble(img_projparam.proSpheroid.radius , sizeOf(Double)) ;

        proj_param := CSProjectedCoordinateSystemList.EmptyParams ;

        case img_projparam.proNumber of
          0 : //Geographic(Latitude/Longitude)
            begin
              FCS := TGIS_CSFactory.ByEPSG( GIS_EPSG_WGS84 );
            end;
          1 : //UTM
            begin
              prj := CSPROJ_Transverse_Mercator ;
              proj_param := CSProjectedCoordinateSystemList.DefaultParamsForUTM(
                              TruncS(img_projparam.proZone*img_projparam.proParams[3])
                           ) ;
            end;
          2 : //State Plane
            begin
              if img_projparam.proParams[0] = 0 then
                FCS := TGIS_CSFactory.ByEPSG( 4267 ) // NAD27
              else
              if img_projparam.proParams[0] = 1 then
                FCS := TGIS_CSFactory.ByEPSG( 4269 ) // NAD83
              else
                proj_unknown := True ;
            end;
          3 : //Albers Conical Equal Area
            begin
              prj := CSPROJ_Albers ;
              proj_param := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;

              proj_param.StandardParallel_1 := img_projparam.proParams[2] ;
              proj_param.StandardParallel_2 := img_projparam.proParams[3] ;
              proj_param.CentralMeridian    := img_projparam.proParams[4] ;
              proj_param.LatitudeOfOrigin   := img_projparam.proParams[5] ;
              proj_param.FalseEasting       := img_projparam.proParams[6] ;
              proj_param.FalseNorthing      := img_projparam.proParams[7] ;
            end;
          4 : //Lambert Conformal Conic
            begin
              prj := CSPROJ_Lambert_Conformal_Conic ;
              proj_param := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;

              proj_param.StandardParallel_1 := img_projparam.proParams[2] ;
              proj_param.StandardParallel_2 := img_projparam.proParams[3] ;
              proj_param.CentralMeridian    := img_projparam.proParams[4] ;
              proj_param.LatitudeOfOrigin   := img_projparam.proParams[5] ;
              proj_param.FalseEasting       := img_projparam.proParams[6] ;
              proj_param.FalseNorthing      := img_projparam.proParams[7] ;
            end;
          5 : // Mercator
            begin
              prj := CSPROJ_Mercator ;
              proj_param := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;

              proj_param.CentralMeridian    := img_projparam.proParams[4] ;
              proj_param.LatitudeOfOrigin   := img_projparam.proParams[5] ;
              proj_param.FalseEasting       := img_projparam.proParams[6] ;
              proj_param.FalseNorthing      := img_projparam.proParams[7] ;
            end;
          6 : //Polar Stereographic  - unsupported
            begin
              prj := CSPROJ_Polar_Stereographic ;
              proj_param := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;

              proj_param.StandardParallel_1 := img_projparam.proParams[2] ;
              proj_param.CentralMeridian    := img_projparam.proParams[4] ;
              proj_param.LatitudeOfOrigin   := img_projparam.proParams[5] ;
              proj_param.FalseEasting       := img_projparam.proParams[6] ;
              proj_param.FalseNorthing      := img_projparam.proParams[7] ;
            end;
          7 : //Polyconic
            begin
              prj := CSPROJ_Polyconic ;
              proj_param := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;

              proj_param.CentralMeridian    := img_projparam.proParams[4] ;
              proj_param.LatitudeOfOrigin   := img_projparam.proParams[5] ;
              proj_param.FalseEasting       := img_projparam.proParams[6] ;
              proj_param.FalseNorthing      := img_projparam.proParams[7] ;
            end;
          8 : //Equidistant Conic    //SP1 SP2
            begin
              if img_projparam.proParams[8] = 1 then begin
                prj := CSPROJ_Equidistant_Conic ;
                proj_param := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;

                proj_param.StandardParallel_1 := img_projparam.proParams[2] ;
                proj_param.StandardParallel_2 := img_projparam.proParams[3] ;
                proj_param.CentralMeridian    := img_projparam.proParams[4] ;
                proj_param.LatitudeOfOrigin   := img_projparam.proParams[5] ;
                proj_param.FalseEasting       := img_projparam.proParams[6] ;
                proj_param.FalseNorthing      := img_projparam.proParams[7] ;
              end
              else if img_projparam.proParams[8] = 0 then begin
                prj := CSPROJ_Equidistant_Conic ;
                proj_param := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;

                proj_param.StandardParallel_1 := img_projparam.proParams[2] ;
                proj_param.StandardParallel_2 := img_projparam.proParams[2] ;
                proj_param.CentralMeridian    := img_projparam.proParams[4] ;
                proj_param.LatitudeOfOrigin   := img_projparam.proParams[5] ;
                proj_param.FalseEasting       := img_projparam.proParams[6] ;
                proj_param.FalseNorthing      := img_projparam.proParams[7] ;
              end
              else
                proj_unknown := True ;
            end;
          9  : //Transverse Mercator
            begin
              prj := CSPROJ_Transverse_Mercator ;
              proj_param := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;

              proj_param.ScaleFactor        := img_projparam.proParams[2] ;
              proj_param.CentralMeridian    := img_projparam.proParams[4] ;
              proj_param.LatitudeOfOrigin   := img_projparam.proParams[5] ;
              proj_param.FalseEasting       := img_projparam.proParams[6] ;
              proj_param.FalseNorthing      := img_projparam.proParams[7] ;
            end;
          10 : //Stereographic
            begin
              prj := CSPROJ_Stereographic ;
              proj_param := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;

              proj_param.LongitudeOfCenter  := img_projparam.proParams[4] ;
              proj_param.LatitudeOfCenter   := img_projparam.proParams[5] ;
              proj_param.FalseEasting       := img_projparam.proParams[6] ;
              proj_param.FalseNorthing      := img_projparam.proParams[7] ;
            end;
          11 : //Lambert Azimuthal Equal-area
            begin
              prj := CSPROJ_Lambert_Azimuthal_Equal_Area ;
              proj_param := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;

              proj_param.LongitudeOfCenter  := img_projparam.proParams[4] ;
              proj_param.LatitudeOfCenter   := img_projparam.proParams[5] ;
              proj_param.FalseEasting       := img_projparam.proParams[6] ;
              proj_param.FalseNorthing      := img_projparam.proParams[7] ;
            end;
          12 : //Azimuthal Equidistant
            begin
              prj := CSPROJ_Azimuthal_Equidistance ;
              proj_param := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;

              proj_param.LongitudeOfCenter  := img_projparam.proParams[4] ;
              proj_param.LatitudeOfCenter   := img_projparam.proParams[5] ;
              proj_param.FalseEasting       := img_projparam.proParams[6] ;
              proj_param.FalseNorthing      := img_projparam.proParams[7] ;
            end;
          13 : //Gnomonic
            begin
              prj := CSPROJ_Gnomic ;
              proj_param := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;

              proj_param.LongitudeOfCenter  := img_projparam.proParams[4] ;
              proj_param.LatitudeOfCenter   := img_projparam.proParams[5] ;
              proj_param.FalseEasting       := img_projparam.proParams[6] ;
              proj_param.FalseNorthing      := img_projparam.proParams[7] ;
            end;
          14 : //Orthographic
            begin
              prj := CSPROJ_Orthographic ;
              proj_param := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;

              proj_param.LongitudeOfCenter  := img_projparam.proParams[4] ;
              proj_param.LatitudeOfCenter   := img_projparam.proParams[5] ;
              proj_param.FalseEasting       := img_projparam.proParams[6] ;
              proj_param.FalseNorthing      := img_projparam.proParams[7] ;
            end;
          16 : //Sinusoidal
            begin
              prj := CSPROJ_Sinusoidal ;
              proj_param := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;

              proj_param.CentralMeridian    := img_projparam.proParams[4] ;
              proj_param.FalseEasting       := img_projparam.proParams[6] ;
              proj_param.FalseNorthing      := img_projparam.proParams[7] ;
            end;
          18 : //Miller Cylindrical
            begin
              prj := CSPROJ_Miller_Cylindrical ;
              proj_param := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;

              proj_param.CentralMeridian    := img_projparam.proParams[4] ;
              proj_param.FalseEasting       := img_projparam.proParams[6] ;
              proj_param.FalseNorthing      := img_projparam.proParams[7] ;
            end;
          19 : //Van der Grinten I
            begin
              prj := CSPROJ_van_der_Grinten_I ;
              proj_param := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;

              proj_param.CentralMeridian    := img_projparam.proParams[4] ;
              proj_param.FalseEasting       := img_projparam.proParams[6] ;
              proj_param.FalseNorthing      := img_projparam.proParams[7] ;
            end;
          20 : //Oblique Mercator (Hotine)
            begin
              if img_projparam.proParams[12] = 0 then begin
                prj := CSPROJ_Hotine_Oblique_Mercator_Two_Point ;
                proj_param := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;

                proj_param.ScaleFactor        := img_projparam.proParams[2] ;
                proj_param.FalseEasting       := img_projparam.proParams[6] ;
                proj_param.FalseNorthing      := img_projparam.proParams[7] ;
                proj_param.CentralMeridian    := img_projparam.proParams[4] ;
                proj_param.LatitudeOfOrigin   := img_projparam.proParams[5] ;
                proj_param.LongitudeOfPoint_1 := img_projparam.proParams[8] ;
                proj_param.LatitudeOfPoint_1  := img_projparam.proParams[9] ;
                proj_param.LongitudeOfPoint_2 := img_projparam.proParams[10] ;
                proj_param.LatitudeOfPoint_2  := img_projparam.proParams[11] ;
              end
              else if img_projparam.proParams[12] = 1 then begin
                prj := CSPROJ_Hotine_Oblique_Mercator_Azimuth_Natural_Origin ;
                proj_param := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;

                proj_param.ScaleFactor        := img_projparam.proParams[2] ;
                proj_param.FalseEasting       := img_projparam.proParams[6] ;
                proj_param.FalseNorthing      := img_projparam.proParams[7] ;
                proj_param.CentralMeridian    := img_projparam.proParams[4] ;
                proj_param.Azimuth := img_projparam.proParams[3] ;
                proj_param.LongitudeOfCenter := img_projparam.proParams[4] ;
                proj_param.LatitudeOfCenter  := img_projparam.proParams[5] ;
              end
              else
                proj_unknown := True ;
            end;
          else
            proj_unknown := True ;
        end;
      end;
    end ;

  begin
    Result      := False ;
    scaleX      := 0 ;
    scaleY      := 0 ;
    noPalette   := True ;
    proj_unknown := True ;

    if not SafeFileExists(Path) then exit ;

    try
      fileStream := openBufferedFileStream( Path ) ;

      // read a file header tag
        {$IFDEF OXYGENE}
          hdr_tag.Read( fileStream ) ;
        {$ELSE}
          fileStream.Read(hdr_tag, sizeOf(hdr_tag));
        {$ENDIF}
        from_trim := Trim( ConvertAnsiString(hdr_tag.Signature)) ;
        if from_trim <> EHFA_HEADER_TAG then
          exit ;

        // read a header file
        fileStream.Seek(hdr_tag.HeaderPtr, soBeginning) ;
        {$IFDEF OXYGENE}
          hdr.Read( fileStream ) ;
        {$ELSE}
          fileStream.Read(hdr, sizeOf(hdr));
        {$ENDIF}

        fileStream.Seek(hdr.RootEntryPtr, soBeginning) ;
        {$IFDEF OXYGENE}
          entry.Read( fileStream ) ;
        {$ELSE}
          fileStream.Read(entry, sizeOf(entry));
        {$ENDIF}

        fileStream.Seek(entry.Child, soBeginning) ;
        {$IFDEF OXYGENE}
          child.Read( fileStream ) ;
        {$ELSE}
          fileStream.Read(child, sizeOf(child));
        {$ENDIF}

        data_name := Trim(ConvertAnsiString(child.Name)) ;
        data_type := Trim(ConvertAnsiString(child.DataType)) ;
        fchild := child ;
        if data_type = EIMG_LAYER then begin
          layersNo := 1 ;
        end
        else
          layersNo := 0 ;

        while child.Next <> 0 do begin
          fileStream.Seek(child.Next, soBeginning) ;
          {$IFDEF OXYGENE}
            child.Read( fileStream ) ;
          {$ELSE}
            fileStream.Read(child, sizeOf(child));
          {$ENDIF}
          data_name := Trim(ConvertAnsiString(child.Name)) ;
          data_type := Trim(ConvertAnsiString(child.DataType)) ;
          if data_type = EIMG_LAYER then
            inc(layersNo) ;
        end ;

        if layersNo > 0 then begin
          SetLength( layers, layersNo ) ;
          child := fchild ;

          data_name := Trim(ConvertAnsiString(child.Name)) ;
          data_type := Trim(ConvertAnsiString(child.DataType)) ;
          layersNo := 0 ;
          if data_type = EIMG_LAYER then begin
            layers[layersNo] := child ;
            inc(layersNo) ;
          end ;
          while child.Next <> 0 do begin
            fileStream.Seek(child.Next, soBeginning) ;
            {$IFDEF OXYGENE}
              child.Read( fileStream ) ;
            {$ELSE}
              fileStream.Read(child, sizeOf(child));
            {$ENDIF}
            data_name := Trim(ConvertAnsiString(child.Name)) ;
            data_type := Trim(ConvertAnsiString(child.DataType)) ;
            if data_type = EIMG_LAYER then begin
              layers[layersNo] := child ;
              inc(layersNo) ;
            end ;
          end ;
        end ;

        SetLength(blockInfoArray, layersNo) ;
        SetLength(estaStatisticsArray, layersNo) ;
        {$IFDEF GIS_NORECORDS}
          for layeridx := 0 to layersNo - 1 do
            estaStatisticsArray[layeridx] := new TGIS_FileIMG_Esta_Statistics ;
        {$ENDIF}

        for layeridx := 0 to layersNo -1 do begin
          child := layers[layeridx] ;

          fileStream.Seek(child.Data, soBeginning) ;
          {$IFDEF OXYGENE}
            d_eimg_layer.Read( fileStream ) ;
          {$ELSE}
            fileStream.Read(d_eimg_layer, sizeOf(TGIS_FileIMG_Eimg_Layer));
          {$ENDIF}

          fileStream.Seek(child.Child, soBeginning) ;
          {$IFDEF OXYGENE}
            child.Read( fileStream ) ;
          {$ELSE}
            fileStream.Read(child, sizeOf(child));
          {$ENDIF}

          data_name := Trim(ConvertAnsiString(child.Name)) ;
          data_type := Trim(ConvertAnsiString(child.DataType)) ;
          if data_type = IMG_EXTERNAL_RASTER then begin
            set_ext_data_pos ;
            externalData := True ;
          end ;

          read_data ;
          while child.Next <> 0 do begin
            fileStream.Seek(child.Next, soBeginning) ;
            {$IFDEF OXYGENE}
              child.Read( fileStream ) ;
            {$ELSE}
              fileStream.Read(child, sizeOf(child));
            {$ENDIF}
            data_name := Trim(ConvertAnsiString(child.Name)) ;
            data_type := Trim(ConvertAnsiString(child.DataType)) ;
            read_data ;
          end ;
        end ;

        // Coordinate System
        if proj_unknown then begin
          FCS := CSUnknownCoordinateSystem ;
        end
        else begin
          if (img_projparam.proNumber <> 0) and // WGS84
             (img_projparam.proNumber <> 2)     // NAD27 or NAD83
          then begin
            gcs := nil ;
            if not IsStringEmpty( datum_name ) then
              gcs :=  CSGeographicCoordinateSystemList.ByWKT( datum_name )
            else
              if IsStringEmpty( ellipsoid_name ) then begin
                if img_projparam.proParams[0] = 0 then
                  gcs := CSGeographicCoordinateSystemList.ByEPSG( 4267 )  // NAD27
                else
                  gcs := CSGeographicCoordinateSystemList.ByEPSG( 4269 ) ;// NAD83
              end ;

            if not assigned( gcs ) then begin
              ellipsoid := CSEllipsoidList.ByWKT( ellipsoid_name ) ;

              if assigned( ellipsoid ) then begin
                datum := CSDatumList.Prepare(
                           -1,
                           ellipsoid.WKT,  // inherit name from ellipsoid
                           ellipsoid.EPSG,
                           0
                         ) ;
                gcs := CSGeographicCoordinateSystemList.Prepare(
                         -1,
                         datum.WKT,  // inherit name form datum
                         datum.EPSG,
                         8901,       // Greenwich
                         9122        // Degree
                       ) ;
              end ;
            end;

            if not assigned( gcs ) then begin
              // assign default WGS84
              gcs := CSGeographicCoordinateSystemList.ByEPSG(
                       GIS_EPSG_WGS84
                     ) ;
            end ;

            units_name := Trim(ConvertAnsiString(dEprjMapinfo.units)) ;

            units := CSUnitsList.ByWKT( units_name ) ;

            if not assigned( units ) then begin
              if units_name = 'us_survey_feet' then
                units := CSUnitsList.ByEPSG( 9003 )   // US_Survey_Foot
              else
                // assign default meter
                units := CSUnitsList.ByEPSG( 9001 ) ; // Meter
            end  ;

            FCS := CSProjectedCoordinateSystemList.Prepare(
                    -1,
                    '',
                    gcs.EPSG,
                    units.EPSG,
                    prj,
                    proj_param
                  ) ;
          end;
        end;

        realBitCount := 24 ;

        FBitWidth := d_eimg_layer.Width ;
        FBitHeight := d_eimg_layer.Height ;
        FBandsCount := layersNo ;
        if FBandsCount >= 3 then
          usedLayersNo := 3
        else
          usedLayersNo := 1 ;

        blockWidth := d_eimg_layer.BlockWidth ;
        blockLength := d_eimg_layer.BlockHeight ;
        block_pixels := blockWidth*blockLength ;

        intLineWidth := FBitWidth * 3 ;
        case d_eimg_layer.PixelType of
          00 : pixelType := T_IMGPixelType.U1   ;
          01 : pixelType := T_IMGPixelType.U2   ;
          02 : pixelType := T_IMGPixelType.U4   ;
          03 : pixelType := T_IMGPixelType.U8   ;
          04 : pixelType := T_IMGPixelType.S8   ;
          05 : pixelType := T_IMGPixelType.U16  ;
          06 : pixelType := T_IMGPixelType.S16  ;
          07 : pixelType := T_IMGPixelType.U32  ;
          08 : pixelType := T_IMGPixelType.S32  ;
          09 : pixelType := T_IMGPixelType.F32  ;
          10 : pixelType := T_IMGPixelType.F64  ;
          11 : pixelType := T_IMGPixelType.C64  ;
        end ;

        pType := pixelType ;

        if (pixelType = T_IMGPixelType.S16) and (estaStatisticsArray[0].minimum >= 0) then
          pixelType := T_IMGPixelType.U16 ;

        case pType of
          (T_IMGPixelType.U1)  :
            begin
              FIsGridImage := False ;
              FIsNativeGridImage := False ;
              bitsCountInfo := 1 ;
              realBitCount := 1 ;
              bytesPerPixel := 1 ;
            end ;
          (T_IMGPixelType.U4)  :
            begin
              FIsGridImage := False ;
              FIsNativeGridImage := False ;
              bitsCountInfo := 4 ;
              realBitCount := 4 ;
              bytesPerPixel := 1 ;
            end ;
          (T_IMGPixelType.U8)  :
            begin
              FIsGridImage := False ;
              FIsNativeGridImage := False ;
              bitsCountInfo := 8 ;
              bytesPerPixel := 1 ;
              realBitCount := 8 ;
              if FBandsCount >= 3 then
                realBitCount := 24 ;
              if FMinZ >= FMaxZ then begin
                FMinZ := estaStatisticsArray[0].minimum ;
                FMaxZ := estaStatisticsArray[0].maximum ;
              end ;
            end ;
          (T_IMGPixelType.S8)  :
            begin
              FIsGridImage := False ;
              realBitCount := 8 ;
              bitsCountInfo := 8 ;
              bytesPerPixel := 1 ;
            end ;
          (T_IMGPixelType.U16)  :
            begin
              bytesPerPixel := blockInfoArray[0][0].Size  div block_pixels ;
              if bytesPerPixel < 1 then
                bytesPerPixel := 2 ;

              if bytesPerPixel = 2 then begin
                if layersNo = 1 then begin
                  FIsGridImage := True ;
                  FAntialias := True ;
                  FIsNativeGridImage := True ;
                end
                else begin
                  FIsGridImage := False ;
                  FIsNativeGridImage := False ;
                  is48BitsPerPixel := True ;
                end ;
                if FMinZ >= FMaxZ then begin
                  FMinZ := estaStatisticsArray[0].minimum ;
                  FMaxZ := estaStatisticsArray[0].maximum ;
                end ;
                bitsCountInfo := 16 ;
              end
              else begin
                FIsGridImage := False ;
                FIsNativeGridImage := False ;
                bitsCountInfo := 8 ;
              end ;
            end ;
          (T_IMGPixelType.S16)  :
            begin
              Params.Pixel.GridNoValue := 32767 ;
              FNoDataValue := Params.Pixel.GridNoValue ;

              FIsGridImage := True ;
              FAntialias := True ;
              FIsNativeGridImage := True ;

              if FMinZ >= FMaxZ then begin
                FMinZ := estaStatisticsArray[0].minimum ;
                FMaxZ := estaStatisticsArray[0].maximum ;
              end ;

              bitsCountInfo := 16 ;
              bytesPerPixel := 2 ;
            end ;
          (T_IMGPixelType.U32)  :
            begin
              FIsGridImage := True ;
              FAntialias := True ;
              FIsNativeGridImage := True ;
              bitsCountInfo := 32 ;
              if FMinZ >= FMaxZ then begin
                FMinZ := estaStatisticsArray[0].minimum ;
                FMaxZ := estaStatisticsArray[0].maximum ;
              end ;
              bytesPerPixel := 4 ;
              Params.Pixel.GridNoValue := 0 ;
              FNoDataValue := Params.Pixel.GridNoValue ;
            end ;
          (T_IMGPixelType.S32)  :
            begin
              FIsGridImage := True ;
              FAntialias := True ;
              FIsNativeGridImage := True ;
              bitsCountInfo := 32 ;
              if FMinZ >= FMaxZ then begin
                FMinZ := estaStatisticsArray[0].minimum ;
                FMaxZ := estaStatisticsArray[0].maximum ;
              end ;
              bytesPerPixel := 4 ;
              Params.Pixel.GridNoValue := -2147483000 ;
              FNoDataValue := Params.Pixel.GridNoValue  ;
            end ;
          (T_IMGPixelType.F32) :
            begin
              FIsGridImage := True ;
              FAntialias := True ;
              FIsNativeGridImage := True ;
              if FMinZ >= FMaxZ then begin
                FMinZ := estaStatisticsArray[0].minimum ;
                FMaxZ := estaStatisticsArray[0].maximum ;
              end ;
              bitsCountInfo := 32 ;
              bytesPerPixel := 4 ;
              Params.Pixel.GridNoValue := -2147483000 ;
              FNoDataValue := Params.Pixel.GridNoValue  ;
            end ;
            else
              Abort ;
        end ;
        if FIsGridImage then
          usedLayersNo := 1 ;

        if externalData then begin

          externalDataFileName := GetPathNoExt(Path) +EXTERNAL_DATA_FILE_EXT ;
          if SafeFileExists( Path  ) then begin
            extBlockSize := (Int64(block_pixels)*bitsCountInfo +7) div 8 ;
            extBandSize := (Int64(FBitWidth * bitsCountInfo +7) div 8) *FBitHeight ;

            FreeObject(fileStream) ;
            Path := externalDataFileName ;
            {$IFDEF OXYGENE}
              externalDataFileName := '' ;
            {$ELSE}
              SetLength(externalDataFileName, 0) ;
            {$ENDIF}
            fileStream := TGIS_FileStream.Create( Path,
                                                  fmOpenRead or
                                                  fmShareDenyWrite
                                                );
            extFileSize := fileStream.Size ;
          end
          else
            raise EGIS_Exception.Create( 'External data file not exist ', Path, 0) ;
        end ;

        blocksColumns :=  (FBitWidth +blockWidth -1) div blockWidth ;
        blocksRows := Integer(d_edms_state.NumVirtualBlocs) div blocksColumns ;
        bufflength := blocksColumns*blockWidth ;

        if layersNo < 3 then begin

          if FIsGridImage then begin
            if bytesPerPixel = 4 then begin
              if pixelType = T_IMGPixelType.S32 then
                SetLength(bandsIBuffer, bufflength)
              else
                SetLength(bandsFBuffer, bufflength) ;
            end
            else
              if pixelType = T_IMGPixelType.S16 then
                SetLength(bandsSBuffer, bufflength)
              else
                SetLength(bandsWBuffer, bufflength) ;
          end
          else begin
            FBandsCount := usedLayersNo ;
            SetLength(bandsBBuffer, 1, bufflength) ;
          end ;

          bandsIdxMapArray[0] := 0 ;
          bandsIdxMapArray[1] := 0 ;
          bandsIdxMapArray[2] := 0 ;
        end
        else begin
          if bytesPerPixel = 2 then begin
            SetLength(bandsWBuffer, 2*bufflength) ;
            if layersNo > 1 then
              SetLength(bandsBBuffer, 3, bufflength) ;
          end
          else
            SetLength(bandsBBuffer, 3, bufflength) ;

          bandsIdxMapArray[0] := 0 ;
          bandsIdxMapArray[1] := 1 ;
          bandsIdxMapArray[2] := 2 ;
        end ;

        if not setCoordinate then begin
          Extent := GisExtent(0,0,0,0) ;
        end ;

    except
        raise EGIS_Exception.Create( '0997 Unsupported IMG format', Path, 0) ;
    end ;

    Result := True ;

  end ;

  function TGIS_LayerIMG.setCoordinate : Boolean ;
  begin
    if ( dEprjMapinfo.pixelSize.X = 0 ) or
       ( dEprjMapinfo.pixelSize.Y = 0 ) then begin
      Result := False ;
      exit ;
    end ;
    scaleX := dEprjMapinfo.pixelSize.X ;
    scaleY := -dEprjMapinfo.pixelSize.Y ;
    FExtent.XMin := dEprjMapinfo.upperLeftCenter.X ;
    FExtent.YMax := dEprjMapinfo.upperLeftCenter.Y ;

    FExtent.XMax := FExtent.XMin ;
    FExtent.YMin := FExtent.YMax ;

    Extent3D := GisExtent3D( Extent.XMin, Extent.YMin, FMinZ,
                             Extent.XMax, Extent.YMax, FMaxZ
                            ) ;
    Result := True ;

  end ;

  procedure TGIS_LayerIMG.decompressBlock( const _bandidx : Integer = 0 ;
                                           const _bidx    : Integer = 0
                                          ) ;
  var
    i, j, k : Integer ;
    blocksize : Integer ;
    mindata : Integer ;
    dataidx : Integer ;
    countsidx : Integer ;
    numruns : Integer ;
    offset  : Integer ;
    numbits : Integer ;
    bytecounter : Integer ;

    buffer    : Array of Byte ;
    size : Integer ;
    nrepeatcount : Integer ;
    ndatavalue : Integer ;
    maxidx : Integer ;
    nvaluebitoffset : Integer ;
    mask : Byte ;
    pixels : Integer ;
    pixpos : Integer ;
    bytepos : Integer ;
    repeatcount : Integer ;
    function bytes2int(const _idx : Integer) : Integer ;
    begin
      Result := buffer[_idx] +(buffer[_idx +1] shl 8) +
               (buffer[_idx +2] shl 16) + (buffer[_idx +3] shl 24) ;
    end ;
  begin
    blocksize := blockLength*blockWidth ;
    if not assigned(decompressedBlock) then begin
      SetLength(decompressedBlock, layersNo, blocksize) ;
      SetLength(bandValidationFlag, layersNo) ;
      blockInBuffer := _bidx ;
      for i := 0 to layersNo -1 do
        bandValidationFlag[i] := False ;
    end
    else begin
      if blockInBuffer <> _bidx then begin
        blockInBuffer := _bidx ;
        for i := 0 to layersNo -1 do
          bandValidationFlag[i] := False ;
      end
      else begin
        if bandValidationFlag[_bandidx] then
          exit ;
      end ;
    end ;
    offset := blockInfoArray[_bandidx][_bidx].offset ;
    fileStream.Seek(offset, soBeginning) ;

    size := blockInfoArray[_bandidx][_bidx].Size ;
    SetLength(buffer, size) ;
    {$IFDEF OXYGENE}
      fileStream.Read(buffer, size);
    {$ELSE}
      fileStream.Read(buffer[0], size);
    {$ENDIF}

    mindata := bytes2int(0) ;
    numruns := bytes2int(4) ;
    dataidx := bytes2int(8) ;
    numbits := buffer[12] ;

    if numruns = -1 then begin
      nvaluebitoffset := 0 ;
      dataidx := 13 ;
      for pixels := 0 to blocksize -1 do begin
        if numbits = 0 then
          ndatavalue := 0
        else
        if numbits = 1 then begin
          ndatavalue := (buffer[dataidx +(nvaluebitoffset shr 3)]
                         shr (nvaluebitoffset and $07)) and $01 ;
          inc(nvaluebitoffset) ;
        end
        else
        if numbits = 2 then
        begin
          ndatavalue := (buffer[dataidx +(nvaluebitoffset shr 3)]
                         shr (nvaluebitoffset and $07)) and $03 ;
          inc(nvaluebitoffset, 2) ;
        end
        else
        if numbits = 4 then begin
          ndatavalue := (buffer[dataidx +(nvaluebitoffset shr 3)]
                         shr (nvaluebitoffset and $07)) and $0F ;
          inc(nvaluebitoffset, 4) ;
        end
        else
        if numbits = 8 then begin
          ndatavalue := buffer[dataidx] ;
          inc(dataidx) ;
        end
        else
        if numbits = 16 then begin
          ndatavalue := (Integer(buffer[dataidx])) shl 8 ;
          inc(dataidx) ;
          ndatavalue := ndatavalue + buffer[dataidx] ;
          inc(dataidx) ;
        end
        else
        if numbits = 32 then begin
          ndatavalue := (Integer(buffer[dataidx])) shl 24 ;
          inc(dataidx) ;
          ndatavalue := ndatavalue + ((Integer(buffer[dataidx])) shl 16) ; ;
          inc(dataidx) ;
          ndatavalue := ndatavalue + ((Integer(buffer[dataidx])) shl 8) ; ;
          inc(dataidx) ;
          ndatavalue := ndatavalue + buffer[dataidx] ;
          inc(dataidx) ;
        end
        else //False
          ndatavalue := 0 ;

        ndatavalue := ndatavalue +mindata ;

        if (pixelType = T_IMGPixelType.U8) or (pixelType = T_IMGPixelType.U4) or
           (pixelType = T_IMGPixelType.U2) or (pixelType = T_IMGPixelType.U1) then
        begin
          decompressedBlock[_bandidx][pixels] := Byte(ndatavalue) ;
        end
        else
        if (pixelType = T_IMGPixelType.U16) then
        begin
          decompressedBlock[_bandidx][pixels] := Byte(ndatavalue) ;
        end
        else
        if (pixelType = T_IMGPixelType.S16) then
        begin
          decompressedBlock[_bandidx][pixels] := Byte(ndatavalue) ;
        end
        else
        if (pixelType = T_IMGPixelType.F32) then
        begin
          decompressedBlock[_bandidx][pixels] := Byte(ndatavalue) ;
        end ;
      end ;
      exit ;
    end ;

    countsidx := 13 ;

    if realBitCount = 1 then begin

      nvaluebitoffset := 0 ;
      pixels := 0 ;

      for i := 0 to numruns -1 do begin

        bytecounter := buffer[countsidx] and $C0 ;

        if bytecounter = 0   then begin
          nrepeatcount := buffer[countsidx] and $3F ;
          inc(countsidx) ;
        end
        else
        if bytecounter = $40 then begin
          nrepeatcount :=(Integer( buffer[countsidx] and $3F) shl 8)
                          + buffer[countsidx +1];
          inc(countsidx, 2) ;
        end
        else
          nrepeatcount := FBitWidth ;

        if numbits = 0 then
          ndatavalue := 0
        else
        if numbits = 1 then begin
          ndatavalue := (buffer[dataidx +(nvaluebitoffset shr 3)]
                         shr (nvaluebitoffset and $07)) and $01 ;
          inc(nvaluebitoffset) ;
        end
        else
        if numbits = 8 then begin
          ndatavalue := buffer[dataidx] ;
          inc(dataidx) ;
        end
        else
        begin
          ndatavalue := 0 ;
        end ;

        ndatavalue := ndatavalue + mindata ;
        pixpos := pixels and $07 ;
        bytepos := pixels shr 3 ;
        repeatcount := nrepeatcount ;

        maxidx := ((nrepeatcount +pixpos +7) div 8) - 1 ;

        for j := 0 to maxidx do begin
          if pixpos = 0 then
            decompressedBlock[_bandidx][bytepos +j] := 0 ;
          if ndatavalue <> 0 then
            mask := $80 shr pixpos
          else
            mask := 0 ;

          for k := 0 to 7 - pixpos do begin
            if mask <> 0 then begin

              decompressedBlock[_bandidx][bytepos +j] :=
                            decompressedBlock[_bandidx][bytepos +j]  or mask ;
              mask := mask shr 1 ;
            end ;
            dec(repeatcount) ;
            if repeatcount = 0 then
              break ;
          end ;
          pixpos := 0 ;
        end ;
        inc(pixels, nrepeatcount) ;
        if pixels >= FBitWidth then
          break ;
      end ;
    end
    else
    if realBitCount = 4 then begin

      nvaluebitoffset := 0 ;
      pixels := 0 ;

      for i := 0 to numruns -1 do begin

        bytecounter := buffer[countsidx] and $C0 ;

        if bytecounter = 0   then begin
          nrepeatcount := buffer[countsidx] and $3F ;
          inc(countsidx) ;
        end
        else
        if bytecounter = $40 then begin
          nrepeatcount :=(Integer( buffer[countsidx] and $3F) shl 8)
                          + buffer[countsidx +1];
          inc(countsidx, 2) ;
        end
        else
          nrepeatcount := FBitWidth ;

        if numbits = 0 then
          ndatavalue := 0
        else
        if numbits = 1 then begin
          ndatavalue := (buffer[dataidx +(nvaluebitoffset shr 3)]
                         shr (nvaluebitoffset and $07)) and $01 ;
          inc(nvaluebitoffset) ;
        end
        else
        if numbits = 2 then begin

          ndatavalue := (buffer[dataidx +(nvaluebitoffset shr 3)]
                         shr (nvaluebitoffset and $07)) and $03 ;
          inc(nvaluebitoffset, 2) ;
        end
        else
        if numbits = 4 then begin
          ndatavalue := (buffer[dataidx +(nvaluebitoffset shr 3)]
                         shr (nvaluebitoffset and $07)) and $0F ;
          inc(nvaluebitoffset, 4) ;
        end
        else
        begin
          ndatavalue := 0 ;
        end ;

        ndatavalue := ndatavalue + mindata ;
        pixpos := pixels and $01 ;
        bytepos := pixels shr 1 ;
        repeatcount := nrepeatcount ;

        maxidx := ((nrepeatcount +pixpos +1) div 2) - 1 ;

        for j := 0 to maxidx do begin
          if pixpos = 0 then
            decompressedBlock[_bandidx][bytepos +j] := 0 ;
          if ndatavalue <> 0 then begin
            if pixpos = 0 then
              mask := $F0 and (ndatavalue shl 4)
            else
              mask := $0F and ndatavalue ;
          end
          else
            mask := 0 ;

          for k := 0 to 1 - pixpos do begin
            if mask <> 0 then begin

              decompressedBlock[_bandidx][bytepos +j] :=
                            decompressedBlock[_bandidx][bytepos +j]  or mask ;
              mask := mask shr 4 ;
            end ;
            dec(repeatcount) ;
            if repeatcount = 0 then
              break ;
          end ;
          pixpos := 0 ;
        end ;
        inc(pixels, nrepeatcount) ;
        if pixels >= FBitWidth then begin
          break ;
        end;
      end ;
    end
    else begin
      j := 0 ;
      for i := 0 to numruns -1 do begin

        nrepeatcount := 0 ;
        bytecounter := buffer[countsidx] and $C0 ;
        if bytecounter = 0 then begin
          nrepeatcount := buffer[countsidx] and $3F ;
          inc(countsidx) ;
        end
        else
        if bytecounter = $40 then begin
          nrepeatcount := buffer[countsidx] and $3F ;
          inc(countsidx) ;
          nrepeatcount := 256*nrepeatcount + buffer[countsidx] ;
          inc(countsidx) ;
        end
        else
        if bytecounter = $80 then begin
          nrepeatcount := buffer[countsidx] and $3F ;
          inc(countsidx) ;
          nrepeatcount := 256*nrepeatcount + buffer[countsidx] ;
          inc(countsidx) ;
          nrepeatcount := 256*nrepeatcount + buffer[countsidx] ;
          inc(countsidx) ;
        end
        else
        if bytecounter = $c0 then begin
          nrepeatcount := buffer[countsidx] and $3F ;
          inc(countsidx) ;
          nrepeatcount := 256*nrepeatcount + buffer[countsidx] ;
          inc(countsidx) ;
          nrepeatcount := 256*nrepeatcount + buffer[countsidx] ;
          inc(countsidx) ;
          nrepeatcount := 256*nrepeatcount + buffer[countsidx] ;
          inc(countsidx) ;
        end ;


        if numbits = 16 then begin
          for k := 0 to nrepeatcount -1 do begin
            if dataidx >= size then begin
              break ;
            end ;
            if buffer[dataidx +1] <> 0 then begin
              decompressedBlock[_bandidx][j] := buffer[dataidx +1] ;
            end
            else
              decompressedBlock[_bandidx][j] := buffer[dataidx] +mindata;
            inc(j) ;
            if j >= blocksize  then begin
              break ;
            end ;
          end ;
          inc(dataidx, 2) ;
          if j >= blocksize then
            break ;
        end
        else begin
          for k := 0 to nrepeatcount -1 do begin
            if dataidx >= size then begin
              break ;
            end ;
            decompressedBlock[_bandidx][j] := buffer[dataidx] +mindata;
            inc(j) ;
            if j >= blocksize  then begin
              break ;
            end ;
          end ;
          inc(dataidx) ;
          if j >= blocksize then
            break ;
        end;
      end ;
    end ;
    bandValidationFlag[_bandidx] := True ;
  end ;

  procedure TGIS_LayerIMG.decompressSBlock( const _bandidx : Integer = 0 ;
                                            const _bidx    : Integer = 0 ;
                                            const _bufidx  : Integer = 0
                                           ) ;
  var
    i, j, k : Integer ;
    blocksize : Integer ;
    mindata : Integer ;
    dataidx : Integer ;
    countsidx : Integer ;
    numruns : Integer ;
    offset  : Integer ;
    numbits : Integer ;
    bytecounter : Integer ;

    buffer    : Array of Byte ;
    size : Integer ;
    nrepeatcount : Integer ;
    ndatavalue : Integer ;
    nvaluebitoffset : Integer ;
    pixels : Integer ;
    decompressedSBlock : TAASmallInt ;

    function bytes2int(const _idx : Integer) : Integer ;
    begin
      Result := Integer(buffer[_idx]) +(Integer(buffer[_idx +1]) shl 8) +
               (Integer(buffer[_idx +2]) shl 16) +
               (Integer(buffer[_idx +3]) shl 24) ;
    end ;

  begin
    blocksize := blockLength*blockWidth ;
    actualColumn := _bidx mod blocksColumns ;
    if not assigned(decompressedSBlock0) then begin

      SetLength(decompressedSBlock0, blocksColumns, blocksize) ;
      if usedLayersNo = 3 then begin
        SetLength(decompressedSBlock1, blocksColumns, blocksize) ;
        SetLength(decompressedSBlock2, blocksColumns, blocksize) ;
      end;
      SetLength(bandsColumnsBlocks, usedLayersNo, blocksColumns) ;
      SetLength(bandValidationFlag, layersNo) ;
      blockInBuffer := _bidx ;
      for k := 0 to usedLayersNo -1 do begin
        for i := 0 to blocksColumns -1 do
          bandsColumnsBlocks[k][i] := -1 ;
      end;

      for i := 0 to layersNo -1 do
        bandValidationFlag[i] := False ;
    end
    else begin
      if bandsColumnsBlocks[_bufidx][actualColumn] = _bidx  then
        exit ;

      bandsColumnsBlocks[_bufidx][actualColumn] := _bidx ;
    end ;
    if _bufidx = 0 then
      decompressedSBlock := decompressedSBlock0
    else
    if _bufidx = 1 then
      decompressedSBlock := decompressedSBlock1
    else
      decompressedSBlock := decompressedSBlock2 ;

    offset := blockInfoArray[_bandidx][_bidx].offset ;
    fileStream.Seek(offset, soBeginning) ;

    size := blockInfoArray[_bandidx][_bidx].Size ;
    size := ((size + 7) div 8) * 8 ;

    SetLength(buffer, 2*blocksize) ;

    {$IFDEF OXYGENE}
      fileStream.Read(buffer, size);
    {$ELSE}
      fileStream.Read(buffer[0], size);
    {$ENDIF}

    mindata := bytes2int(0) ;
    numruns := bytes2int(4) ;
    dataidx := bytes2int(8) ;
    numbits := buffer[12] ;

    if numruns = -1 then begin
      nvaluebitoffset := 0 ;
      dataidx := 13 ;
      for pixels := 0 to blocksize -1 do begin
        if numbits = 0 then
          ndatavalue := 0
        else
        if numbits = 1 then begin
          ndatavalue := (buffer[dataidx +(nvaluebitoffset shr 3)]
                         shr (nvaluebitoffset and $07)) and $01 ;
          inc(nvaluebitoffset) ;
        end
        else
        if numbits = 2 then
        begin
          ndatavalue := (buffer[dataidx +(nvaluebitoffset shr 3)]
                         shr (nvaluebitoffset and $07)) and $03 ;
          inc(nvaluebitoffset, 2) ;
        end
        else
        if numbits = 4 then begin
          ndatavalue := (buffer[dataidx +(nvaluebitoffset shr 3)]
                         shr (nvaluebitoffset and $07)) and $0F ;
          inc(nvaluebitoffset, 4) ;
        end
        else
        if numbits = 8 then begin
          ndatavalue := buffer[dataidx] ;
          inc(dataidx) ;
        end
        else
        if numbits = 16 then begin
          ndatavalue := (Integer(buffer[dataidx])) shl 8 ;
          inc(dataidx) ;
          ndatavalue := ndatavalue + buffer[dataidx] ;
          inc(dataidx) ;
        end
        else
        if numbits = 32 then begin
          ndatavalue := (Integer(buffer[dataidx])) shl 24 ;
          inc(dataidx) ;
          ndatavalue := ndatavalue + ((Integer(buffer[dataidx])) shl 16) ; ;
          inc(dataidx) ;
          ndatavalue := ndatavalue + ((Integer(buffer[dataidx])) shl 8) ; ;
          inc(dataidx) ;
          ndatavalue := ndatavalue + buffer[dataidx] ;
          inc(dataidx) ;
        end
        else //False
          ndatavalue := 0 ;

        ndatavalue := ndatavalue +mindata ;

        if (pixelType = T_IMGPixelType.U8) or (pixelType = T_IMGPixelType.U4) or
           (pixelType = T_IMGPixelType.U2) or (pixelType = T_IMGPixelType.U1) then
        begin
          decompressedSBlock[actualColumn][pixels] := SmallInt(Byte(ndatavalue)) ;
        end
        else
        if (pixelType = T_IMGPixelType.U16) then
        begin
          decompressedSBlock[actualColumn][pixels] := SmallInt(ndatavalue) ;
        end
        else
        if (pixelType = T_IMGPixelType.S16) then
        begin
          decompressedSBlock[actualColumn][pixels] := SmallInt(ndatavalue) ;
        end
        else
        if (pixelType = T_IMGPixelType.F32) then
        begin
          decompressedSBlock[actualColumn][pixels] := SmallInt(ndatavalue) ;
        end ;
      end ;
      exit ;
    end ;

    j := 0 ;
    countsidx := 13 ;
    nvaluebitoffset := 0 ;

    for i := 0 to numruns -1 do begin

      nrepeatcount := 0 ;
      bytecounter := buffer[countsidx] and $C0 ;

      if bytecounter = 0 then begin
        nrepeatcount := buffer[countsidx] and $3F ;
        inc(countsidx) ;
      end
      else
      if bytecounter = $40 then begin
        nrepeatcount := buffer[countsidx] and $3F ;
        inc(countsidx) ;
        nrepeatcount := 256*nrepeatcount + buffer[countsidx] ;
        inc(countsidx) ;
      end
      else
      if bytecounter = $80 then begin
        nrepeatcount := buffer[countsidx] and $3F ;
        inc(countsidx) ;
        nrepeatcount := 256*nrepeatcount + buffer[countsidx] ;
        inc(countsidx) ;
        nrepeatcount := 256*nrepeatcount + buffer[countsidx] ;
        inc(countsidx) ;
      end
      else
      if bytecounter = $c0 then begin
        nrepeatcount := buffer[countsidx] and $3F ;
        inc(countsidx) ;
        nrepeatcount := 256*nrepeatcount + buffer[countsidx] ;
        inc(countsidx) ;
        nrepeatcount := 256*nrepeatcount + buffer[countsidx] ;
        inc(countsidx) ;
        nrepeatcount := 256*nrepeatcount + buffer[countsidx] ;
        inc(countsidx) ;
      end ;

      if numbits = 32 then begin
        ndatavalue :=(Integer(buffer[dataidx])) shl 24;
        inc(dataidx) ;
        ndatavalue := ndatavalue + ((Integer(buffer[dataidx])) shl 16);
        inc(dataidx) ;
        ndatavalue := ndatavalue + ((Integer(buffer[dataidx])) shl 8) ;
        inc(dataidx) ;
        ndatavalue := ndatavalue + buffer[dataidx] ;
        inc(dataidx) ;
      end
      else
      if numbits = 16 then begin
        ndatavalue := 256*buffer[dataidx] ;
        inc(dataidx) ;
        ndatavalue := ndatavalue + buffer[dataidx] ;
        inc(dataidx) ;
      end
      else
      if numbits = 8 then begin
        ndatavalue := buffer[dataidx] ;
        inc(dataidx) ;
      end
      else
      if numbits = 4 then begin
        ndatavalue := (buffer[dataidx +(nvaluebitoffset shr 3)]
                       shr (nvaluebitoffset and $07)) and $0F ;
        inc(nvaluebitoffset, 4) ;
      end
      else
      if numbits = 2 then
      begin
        ndatavalue := (buffer[dataidx +(nvaluebitoffset shr 3)]
                       shr (nvaluebitoffset and $07)) and $03 ;
        inc(nvaluebitoffset, 2) ;
      end
      else
      if numbits = 1 then begin
        ndatavalue := (buffer[dataidx +(nvaluebitoffset shr 3)]
                       shr (nvaluebitoffset and $07)) and $01 ;
        inc(nvaluebitoffset) ;
      end
      else
      begin
        ndatavalue := 0 ;
      end ;

      ndatavalue := ndatavalue + mindata ;
      if nrepeatcount +j > blocksize then begin
        nrepeatcount := blocksize - j ;
        if nrepeatcount <= 0 then
          break ;
      end ;
      for k := 0 to nrepeatcount -1 do begin
        decompressedSBlock[actualColumn][j] := SmallInt(ndatavalue) ;
        inc(j) ;
      end ;
      if dataidx >= size then begin
        break ;
      end ;
      if j >= blocksize then begin
        break ;
      end ;
    end ;

    bandValidationFlag[_bandidx] := True ;
  end ;

  procedure TGIS_LayerIMG.decompressFBlock( const _bandidx : Integer = 0 ;
                                            const _bidx    : Integer = 0
                                           ) ;
  var
    i, j, k : Integer ;
    blocksize : Integer ;
    mindata : Integer ;
    dataidx : Integer ;
    countsidx : Integer ;
    numruns : Integer ;
    offset  : Integer ;
    numbits : Integer ;
    bytecounter : Integer ;

    buffer    : Array of Byte ;
    size : Integer ;
    nrepeatcount : Integer ;
    ndatavalue : Single ;
    pixels : Integer ;
    blocsno : Integer ;
    curridx : Integer ;

    bnval : Int64 ;
    b4b : Array [0..3] of Byte ;
    fndatavalue : Array [0..0] of Single ;

    function bytes2int(const _idx : Integer) : Integer ;
    begin
      Result := Integer(buffer[_idx]) +(Integer(buffer[_idx +1]) shl 8) +
               (Integer(buffer[_idx +2]) shl 16) +
               (Integer(buffer[_idx +3]) shl 24) ;
    end ;

    procedure take4(const _ival : Integer) ;
    begin
      b4b[0] := Byte((_ival       ) and $0FF) ;
      b4b[1] := Byte((_ival shr 08) and $0FF) ;
      b4b[2] := Byte((_ival shr 16) and $0FF) ;
      b4b[3] := Byte((_ival shr 24) and $0FF) ;
    end;

  begin
    blocksize := blockLength*blockWidth ;
    blocsno := (FBitWidth +blockWidth -1) div blockWidth ;
    curridx := _bidx mod blocsno ;

    if not assigned(decompressedFBlock) then begin
      SetLength(decompressedFBlock, blocsno, blocksize) ;
      SetLength(decompressedFBlockIdx, blocsno) ;
      for i := 0 to blocsno -1 do
        decompressedFBlockIdx[i] := -1 ;
      SetLength(bandValidationFlag, layersNo) ;
      blockInBuffer := _bidx ;
      for i := 0 to layersNo -1 do
        bandValidationFlag[i] := False ;
    end
    else begin
      if blockInBuffer <> _bidx then begin
        blockInBuffer := _bidx ;
        for i := 0 to layersNo -1 do
          bandValidationFlag[i] := False ;
      end
      else begin
        if bandValidationFlag[_bandidx] then
          exit ;
      end ;
    end ;

    decompressedFBlockIdx[curridx] := _bidx ;

    offset := blockInfoArray[_bandidx][_bidx].offset ;
    fileStream.Seek(offset, soBeginning) ;

    size := blockInfoArray[_bandidx][_bidx].Size ;

    SetLength(buffer, 4*blocksize) ;
    {$IFDEF OXYGENE}
      fileStream.Read(buffer, size);
    {$ELSE}
      fileStream.Read(buffer[0], size);
    {$ENDIF}

    mindata := bytes2int(0) ;
    numruns := bytes2int(4) ;
    dataidx := bytes2int(8) ;
    numbits := buffer[12] ;

    if numruns = -1 then begin
      dataidx := 13 ;
      for pixels := 0 to blocksize -1 do begin
        if numbits = 32 then begin
          bnval := (Integer(buffer[dataidx])) shl 24 ;
          inc(dataidx) ;
          bnval := bnval + ((Integer(buffer[dataidx])) shl 16) ;
          inc(dataidx) ;
          bnval := bnval + ((Integer(buffer[dataidx])) shl 8) ; ;
          inc(dataidx) ;
          bnval := bnval + buffer[dataidx] ;
          inc(dataidx) ;
        end
        else //False
          bnval := 0 ;

        bnval := bnval +mindata ;
        take4(Integer(bnval and $00000000FFFFFFFF)) ;
        {$IFDEF OXYGENE}
          {$IFDEF CLR}
            System.Buffer.BlockCopy(b4b, 0, fndatavalue, 0 ,4) ;
          {$ENDIF}
          {$IFDEF JAVA}
            fndatavalue[0] := BitConverter.ToSingle( b4b, 0 ) ;
          {$ENDIF}
        {$ELSE}
          System.Move(b4b, fndatavalue, 4) ;
        {$ENDIF}
        decompressedFBlock[curridx][pixels] := fndatavalue[0] ;
      end ;
      exit ;
    end ;

    j := 0 ;
    countsidx := 13 ;

    for i := 0 to numruns -1 do begin

      nrepeatcount := 0 ;
      bytecounter := buffer[countsidx] and $C0 ;

      if bytecounter = 0 then begin
        nrepeatcount := buffer[countsidx] and $3F ;
        inc(countsidx) ;
      end
      else
      if bytecounter = $40 then begin
        nrepeatcount := buffer[countsidx] and $3F ;
        inc(countsidx) ;
        nrepeatcount := 256*nrepeatcount + buffer[countsidx] ;
        inc(countsidx) ;
      end
      else
      if bytecounter = $80 then begin
        nrepeatcount := buffer[countsidx] and $3F ;
        inc(countsidx) ;
        nrepeatcount := 256*nrepeatcount + buffer[countsidx] ;
        inc(countsidx) ;
        nrepeatcount := 256*nrepeatcount + buffer[countsidx] ;
        inc(countsidx) ;
      end
      else
      if bytecounter = $c0 then begin
        nrepeatcount := buffer[countsidx] and $3F ;
        inc(countsidx) ;
        nrepeatcount := 256*nrepeatcount + buffer[countsidx] ;
        inc(countsidx) ;
        nrepeatcount := 256*nrepeatcount + buffer[countsidx] ;
        inc(countsidx) ;
        nrepeatcount := 256*nrepeatcount + buffer[countsidx] ;
        inc(countsidx) ;
      end ;



      if numbits = 16 then begin
        bnval :=(Integer(buffer[dataidx])) shl 8;
        inc(dataidx) ;
        bnval := bnval + (Integer(buffer[dataidx])) ;
        inc(dataidx) ;
      end
      else
      if numbits = 32 then begin
        bnval :=(Integer(buffer[dataidx])) shl 24;
        inc(dataidx) ;
        bnval := bnval + ((Integer(buffer[dataidx])) shl 16);
        inc(dataidx) ;
        bnval := bnval + ((Integer(buffer[dataidx])) shl 8) ;
        inc(dataidx) ;
        bnval := bnval + buffer[dataidx] ;
        inc(dataidx) ;
      end
      else
        bnval := 0 ;

      bnval := bnval +mindata;

      take4(Integer(bnval and $00000000FFFFFFFF)) ;
      {$IFDEF OXYGENE}
        {$IFDEF CLR}
          System.Buffer.BlockCopy(b4b, 0, fndatavalue, 0, 4) ;
        {$ENDIF}
        {$IFDEF JAVA}
          fndatavalue[0] := BitConverter.ToSingle( b4b, 0 ) ;
        {$ENDIF}
      {$ELSE}
        System.Move(b4b, fndatavalue, 4) ;
      {$ENDIF}
      ndatavalue := fndatavalue[0] ;

      if nrepeatcount +j > blocksize then begin
        nrepeatcount := blocksize - j ;
        if nrepeatcount <= 0 then
          break ;
      end ;
      for k := 0 to nrepeatcount -1 do begin
        decompressedFBlock[curridx][j] := ndatavalue ;
        inc(j) ;
      end ;
      if dataidx >= size then begin
        break ;
      end ;
      if j >= blocksize then begin
        break ;
      end ;
    end ;

    bandValidationFlag[_bandidx] := True ;
  end ;

  procedure TGIS_LayerIMG.reverseBytes( const _bufidx : Integer = 0 ;
                                        const _sidx   : Integer = 0
                                      ) ;
  var
    i : Integer ;
    tilewidth : Integer ;
  const
    {$IFDEF OXYGENE}
    T_IMGBitRevTable : array [0..255] of Byte = [
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
    {$ELSE}
    T_IMGBitRevTable : array [0..255] of Byte = (
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
    {$ENDIF}
  begin
    case realBitCount of
      24 :
        tilewidth := blockWidth ;
       4 :
        tilewidth := (blockWidth +1) div 2;
       1 :
        tilewidth := (blockWidth +7) div 8 ;
       else
        tilewidth := blockWidth ; //8 bits
    end ;

    for i := 0 to tilewidth -1 do
      bandsBBuffer[_bufidx][_sidx +i] :=
          T_IMGBitRevTable[ bandsBBuffer[_bufidx][_sidx +i] ] ;
  end ;

  procedure TGIS_LayerIMG.convertWords2Bytes( const _bufidx : Integer = 0 ;
                                              const _sidx   : Integer = 0
                                            ) ;
  var
    i : Integer ;
    divider : Double ;
    ival : Integer ;
  begin

    if FMaxZ > 0 then
      divider := 255 / FMaxZ
    else
      divider := 255 / 2000 ;
    for i := 0 to _sidx -1 do begin
      ival := RoundS(bandsWBuffer[ i]*divider) ;
      if ival > 255 then
        bandsBBuffer[_bufidx][i] :=  255
      else
        bandsBBuffer[_bufidx][i] :=  ival ;
    end;
  end ;

  procedure TGIS_LayerIMG.reverseHBytes( const _bufidx : Integer = 0 ;
                                         const _sidx   : Integer = 0
                                       ) ;
  var
    i : Integer ;
    tilewidth : Integer ;
  const
    {$IFDEF OXYGENE}
    T_IMGBitRevTable : array [0..255] of Byte = [
      $00, $10, $20, $30, $40, $50, $60, $70,
      $80, $90, $a0, $b0, $c0, $d0, $e0, $f0,
      $01, $11, $21, $31, $41, $51, $11, $71,
      $81, $91, $a1, $b1, $c1, $d1, $e1, $f1,
      $02, $12, $22, $32, $42, $52, $62, $72,
      $82, $92, $a2, $b2, $c2, $d2, $e2, $f2,
      $03, $13, $23, $33, $43, $53, $63, $73,
      $83, $93, $a3, $b3, $c3, $d3, $e3, $f3,
      $04, $14, $24, $34, $44, $54, $64, $74,
      $84, $94, $a4, $b4, $c4, $d4, $e4, $f4,
      $05, $15, $25, $35, $45, $55, $65, $75,
      $85, $95, $a5, $b5, $c5, $d5, $e5, $f5,
      $06, $16, $26, $36, $46, $56, $66, $76,
      $86, $96, $a6, $b6, $c6, $d6, $e6, $f6,
      $07, $17, $27, $37, $47, $57, $67, $77,
      $87, $97, $a7, $b7, $c7, $d7, $e7, $f7,
      $08, $18, $28, $38, $48, $58, $68, $78,
      $88, $98, $a8, $b8, $c8, $d8, $e8, $f8,
      $09, $19, $29, $39, $49, $59, $69, $79,
      $89, $99, $a9, $b9, $c9, $d9, $e9, $f9,
      $0a, $1a, $2a, $3a, $4a, $5a, $6a, $7a,
      $8a, $9a, $aa, $ba, $ca, $da, $ea, $fa,
      $0b, $1b, $2b, $3b, $4b, $5b, $6b, $7b,
      $8b, $9b, $ab, $bb, $cb, $db, $eb, $fb,
      $0c, $1c, $2c, $3c, $4c, $5c, $6c, $7c,
      $8c, $9c, $ac, $bc, $cc, $dc, $ec, $fc,
      $0d, $1d, $2d, $3d, $4d, $5d, $6d, $7d,
      $8d, $9d, $ad, $bd, $cd, $dd, $ed, $fd,
      $0e, $1e, $2e, $3e, $4e, $5e, $6e, $7e,
      $8e, $9e, $ae, $be, $ce, $de, $ee, $fe,
      $0f, $1f, $2f, $3f, $4f, $5f, $6f, $7f,
      $8f, $9f, $af, $bf, $cf, $df, $ef, $ff
    ] ;
    {$ELSE}
    T_IMGBitRevTable : array [0..255] of Byte = (
      $00, $10, $20, $30, $40, $50, $60, $70,
      $80, $90, $a0, $b0, $c0, $d0, $e0, $f0,
      $01, $11, $21, $31, $41, $51, $11, $71,
      $81, $91, $a1, $b1, $c1, $d1, $e1, $f1,
      $02, $12, $22, $32, $42, $52, $62, $72,
      $82, $92, $a2, $b2, $c2, $d2, $e2, $f2,
      $03, $13, $23, $33, $43, $53, $63, $73,
      $83, $93, $a3, $b3, $c3, $d3, $e3, $f3,
      $04, $14, $24, $34, $44, $54, $64, $74,
      $84, $94, $a4, $b4, $c4, $d4, $e4, $f4,
      $05, $15, $25, $35, $45, $55, $65, $75,
      $85, $95, $a5, $b5, $c5, $d5, $e5, $f5,
      $06, $16, $26, $36, $46, $56, $66, $76,
      $86, $96, $a6, $b6, $c6, $d6, $e6, $f6,
      $07, $17, $27, $37, $47, $57, $67, $77,
      $87, $97, $a7, $b7, $c7, $d7, $e7, $f7,
      $08, $18, $28, $38, $48, $58, $68, $78,
      $88, $98, $a8, $b8, $c8, $d8, $e8, $f8,
      $09, $19, $29, $39, $49, $59, $69, $79,
      $89, $99, $a9, $b9, $c9, $d9, $e9, $f9,
      $0a, $1a, $2a, $3a, $4a, $5a, $6a, $7a,
      $8a, $9a, $aa, $ba, $ca, $da, $ea, $fa,
      $0b, $1b, $2b, $3b, $4b, $5b, $6b, $7b,
      $8b, $9b, $ab, $bb, $cb, $db, $eb, $fb,
      $0c, $1c, $2c, $3c, $4c, $5c, $6c, $7c,
      $8c, $9c, $ac, $bc, $cc, $dc, $ec, $fc,
      $0d, $1d, $2d, $3d, $4d, $5d, $6d, $7d,
      $8d, $9d, $ad, $bd, $cd, $dd, $ed, $fd,
      $0e, $1e, $2e, $3e, $4e, $5e, $6e, $7e,
      $8e, $9e, $ae, $be, $ce, $de, $ee, $fe,
      $0f, $1f, $2f, $3f, $4f, $5f, $6f, $7f,
      $8f, $9f, $af, $bf, $cf, $df, $ef, $ff
    ) ;
    {$ENDIF}
  begin
    case realBitCount of
      24 :
        tilewidth := blockWidth ;
       4 :
        tilewidth := (blockWidth +1) div 2;
       1 :
        tilewidth := (blockWidth +7) div 8 ;
       else
        tilewidth := blockWidth ; //8 bits
    end ;

    for i := 0 to tilewidth -1 do
      bandsBBuffer[_bufidx][_sidx +i] :=
          T_IMGBitRevTable[ bandsBBuffer[_bufidx][_sidx +i] ] ;
  end ;


  procedure TGIS_LayerIMG.setUp ;
  var
    t       : Integer;
    m       : Byte ;
  begin
    externalData   := False ;
    noBigImgMinMax := False ;
    try
      if readFileInfo then begin
        // read bitmap parameters

         case realBitCount of
           1, 4, 8 : colorsNo := 1 shl realBitCount
         else
           colorsNo := 0
         end ;

        realLineWidth := ( FBitWidth*realBitCount +7 ) div 8 ;

        if (realBitCount = 1) OR (realBitCount = 4)
            OR (realBitCount = 8) then begin
          if noPalette then
            setBitmapPalette ;
          intLineWidth := FBitWidth * 3 ;
          colorsNo := 0 ;
        end
        else begin
          intLineWidth := realLineWidth ;
        end ;

      end ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;

    // read a word file
      if      SafeFileExists(GetPathNoExt( Path ) + WORLD_FILE_EXT_IMG) then
              setWorldFile( WORLD_FILE_EXT_IMG )
      else if SafeFileExists(GetPathNoExt( Path ) + WORLD_FILE_EXT2_IMG) then
              setWorldFile( WORLD_FILE_EXT2_IMG ) ;

    inherited ;

//    setLineFuncAddr(getLineFunc) ;

    if realBitCount  < 8 then begin
      if realBitCount = 1 then begin
        for t := 0 to 3 do begin
          m := testMask[t] ;
          testMask[t] := testMask[7 -t] ;
          testMask[7 -t] := m ;
        end ;
      end
      else begin
        m := testMask[0] ;
        testMask[0] := testMask[1] ;
        testMask[1] := m ;
      end ;

    end ;

    bitsPerPixel := FBandsCount*bitsCountInfo ;
    if SafeFileExists( Path ) then
      FAge := GisFileAge( Path ) ;

    FFileInfo := Format( 'IMG Erdas Layer'
                         +#13#10 +
                         '%d x %d',
                         [FBitWidth, FBitHeight]
                       ) ;

  end;

  procedure TGIS_LayerIMG.setupParams ;
  begin
//Bands managing

    if not FIsGridImage then begin
      if Params.Pixel.GridBand > 0 then
        FIsGridImage         := True ;
    end ;

    if not FIsGridImage and (layersNo >= 3) then begin
      if Params.Pixel.RedBand = 0 then begin
        if BandsCount > 3 then begin
          if (bitsCountInfo = 16) then
            Params.Pixel.RedBand := 1
          else
            Params.Pixel.RedBand := 4 ;
        end
        else
        if BandsCount = 3 then begin
          Params.Pixel.RedBand := 3 ;
        end
        else begin
          Params.Pixel.RedBand := 1 ;
        end ;
      end ;

      if Params.Pixel.GreenBand = 0 then begin

        if BandsCount > 3 then begin
          if (bitsCountInfo = 16) then
            Params.Pixel.GreenBand := 2
          else
            Params.Pixel.GreenBand := 3 ;
        end
        else
        if BandsCount = 3 then begin
          Params.Pixel.GreenBand := 2 ;
        end ;
      end ;

      if Params.Pixel.BlueBand = 0 then begin

        if BandsCount > 3 then begin
          if (bitsCountInfo = 16) then
            Params.Pixel.BlueBand := 3
          else
            Params.Pixel.BlueBand := 2 ;
        end
        else
        if BandsCount = 3 then begin
          Params.Pixel.BlueBand := 1 ;
        end ;
      end ;
    end ;

    inherited ;
    if FIsGridImage then begin
      if FMinZ >= FMaxZ then
        prepareMinMaxZ ;

      if FGridBand > 0 then begin
        bandsMap[0] := FGridBand -1 ;
        bandsMap[1] := bandsMap[0] ;
        bandsMap[2] := bandsMap[0] ;
      end;
    end ;

  end ;

  function  TGIS_LayerIMG.setFileScale(
    const _dwidth : Double ;
    const _swidth : Double
  ) : Double ;
  begin
    Result := 1.0 ;
    if externalData then begin
      if realBitCount = 8 then begin
        Result := _dwidth/_swidth ;
        if Result > 1.0 then
          Result := 1.0 ;
      end;
    end ;
  end;

  function TGIS_LayerIMG.getLine( const _buffer : TBytes  ;
                                  const _offset : Integer ;
                                  const _linenr : Integer ;
                                  const _start  : Integer ;
                                  const _bytes  : Integer
                                 ) : Integer ;
  var
    stidx : Integer ;
    lstart : Integer ;
    k, i, b   : Integer ;
    pixels : Integer ;
    lpix   : Integer ;
    tline  : Integer ;
    rcol   : Integer ;
    lcol   : Integer ;
    maxidx : Integer ;
    tilewidth3 : Integer ;
    bytes  : Integer ;
    maxb   : Integer ;
    nodata : Boolean ;
  begin
    try
      bytes := _bytes ;
      case realBitCount of
        24 :
          begin
            pixels := bytes div 3 ;
            lpix := _start div 3 ;
            rcol := ((lpix +pixels -1)  div blockWidth) ;
            lcol := ( lpix  div blockWidth) ;
            lstart := lpix mod blockWidth ;
            maxb := 2 ;
          end ;
         8 :
          begin
            if layersNo = 1 then begin
              pixels := bytes ;
              lpix := _start ;
              rcol := ((lpix +pixels -1)  div blockWidth) ;
              lcol := ( lpix  div blockWidth) ;
              lstart := lpix mod blockWidth ;
              maxb := 0 ;
            end
            else  begin
              pixels := bytes div 3 ;
              lpix := _start div 3 ;
              rcol := ((lpix +pixels -1)  div blockWidth) ;
              lcol := ( lpix  div blockWidth) ;
              lstart := lpix mod blockWidth ;
              maxb := 2 ;
            end ;
          end ;
         4 :
          begin
            tilewidth3 := (blockWidth +1) div 2;
            pixels := bytes ;
            lpix := _start ;
            rcol := ((_start +bytes -1)  div tilewidth3) ;
            lcol := ( _start  div tilewidth3) ;
            lstart := lpix mod tilewidth3 ;
            maxb := 0 ;
          end ;
         1 :
          begin
            if (bytes +_start) > realLineWidth then
              bytes := realLineWidth -_start ;
            tilewidth3 := (blockWidth +7) div 8 ;
            pixels := bytes ;
            lpix := _start ;
            rcol := ((_start +bytes -1)  div tilewidth3) ;
            lcol := ( _start  div tilewidth3) ;
            lstart := lpix mod tilewidth3 ;
            maxb := 0 ;
          end ;
        else begin
          Result := 0 ;
          exit ;
        end ;
      end ;

      Result := bytes ;
      tline   := _linenr mod blockLength ;

      stidx := blocksColumns * (_linenr div blockLength) ;
      lBlockIdx := lcol +stidx ;
      rBlockIdx := rcol +stidx ;

      if layersNo < 3 then
        maxidx := 0
      else
        maxidx := 2 ;

      nodata := True ;
      for b := 0 to maxidx do
       if bandsMap[b]>= 0 then begin
        getImgLineSegment(tline, bandsMap[b], b) ;
        nodata := False ;
       end;

      if nodata then begin
        Result := 0 ;
        exit ;
      end ;
      k := 0 ;
      if FBandsCount <=3 then begin
        for i := 0 to pixels -1 do begin
          for b := 0 to maxb do begin
            {$IFDEF OXYGENE}
              _buffer[_offset+k] :=
                bandsBBuffer[bandsIdxMapArray[b]][lstart +i] ;
            {$ELSE}
              PByte(NativeInt(_buffer) +k)^ :=
                bandsBBuffer[bandsIdxMapArray[b]][lstart +i] ;
            {$ENDIF}
            inc(k) ;
          end ;
        end ;
      end
      else begin
        for i := 0 to pixels -1 do begin
          for b := maxb downto 0 do begin
            {$IFDEF OXYGENE}
              _buffer[_offset+k] :=
                bandsBBuffer[bandsIdxMapArray[b]][lstart +i] ;
            {$ELSE}
              PByte(NativeInt(_buffer) +k)^ :=
                bandsBBuffer[bandsIdxMapArray[b]][lstart +i] ;
            {$ENDIF}
            inc(k) ;
          end ;
        end ;
      end ;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  function TGIS_LayerIMG.convertBitsToPixels
                                   ( const _buffSrc  : TBytes  ;
                                     const _srcOffset : Integer ;
                                     const _buffDst   : TGIS_Pixels ;
                                     const _dstOffset : Integer ;
                                     const _pixStart   : Integer ;
                                     const _pixCount  : Integer
                                   ) : Integer ;
  var
    i             : Integer ;
    sidx, didx    : Integer ;
    wpix          : Integer ;
    bval          : Byte ;
    pal_idx       : Integer ;
    idx8          : Integer ;
    val3, val2    : Cardinal ;
    val1          : Cardinal ;
    wbits         : Integer ;
    basemask      : Byte ;
    basesh        : Integer ;
    frmargin      : Integer ;
  const
    R_MASK4 = $0F ;
    R_MASK1 = $01 ;
  begin
    wpix  := 0 ;
    wbits := 0 ;
    idx8 := 0 ;
    basemask := 0 ;
    basesh  := 0 ;
    sidx := _srcOffset ;
    didx := _dstOffset ;
    if realBitCount < 8 then begin
      frmargin := ( _pixStart  * realBitCount ) mod 8 ;
      if realBitCount = 1 then begin
        basemask := R_MASK1 ;
        basesh  := 1 ;
      end
      else begin
        basemask := R_MASK4 ;
        basesh  := 4 ;
      end;

      wbits := _buffSrc[sidx] shr frmargin ;
      wpix := 8 -frmargin ;
    end
    else
      idx8 := _srcOffset ;

    for i := 0 to _pixCount -1 do begin
      if realBitCount < 8 then begin
        bval := wbits and basemask ;
        dec(wpix, basesh) ;
        if wpix = 0 then begin
          wpix := 8 ;
          inc(sidx) ;
          wbits := _buffSrc[sidx] ;
        end
        else
          wbits := wbits shr basesh ;
      end
      else begin
        bval := _buffSrc[idx8] ;
        inc(idx8) ;
      end;

      pal_idx := bval ;

      if unusedBlue then
        val3 := 0
      else
        val3 := Integer(bitPalette[pal_idx].B ) ;
      if unusedGreen then
        val2 := 0
      else
        val2 := Integer(bitPalette[pal_idx].G ) ;
      if unusedRed then
        val1 := 0
      else
        val1 := Integer(bitPalette[pal_idx].R ) ;

      _buffDst[didx] :=  Integer(val3 or ( val2 shl 8 ) or ( val1 shl 16)) or
                         Integer($FF000000) ;


      inc(didx) ;
    end;
    Result := _pixCount ;


  end ;

  function TGIS_LayerIMG.getLineBits ( const _buffer   : TBytes  ;
                                       const _offset   : Integer ;
                                       const _linenr   : Integer ;
                                       const _pixStart : Integer ;
                                       const _pixCount : Integer
                                     ) : Integer ;
  var
    stidx : Integer ;
    lstart : Integer ;
    k, i, b   : Integer ;
    pixels : Integer ;
    lpix   : Integer ;
    tline  : Integer ;
    rcol   : Integer ;
    lcol   : Integer ;
    maxidx : Integer ;
    tilewidth3 : Integer ;
    bytestart  : Integer ;
    bytestop  : Integer ;
    maxb   : Integer ;
    nodata : Boolean ;
  begin
    try
      pixels := _pixCount ;

      bytestart  := ( _pixStart * realBitCount) div 8 ;
      bytestop   := ((_pixStart + _pixCount -1) * realBitCount) div 8 ;
      Result :=  bytestop -bytestart +((realBitCount +7) div 8);

      case realBitCount of
        24 :
          begin
            maxb := 2 ;
            lpix := _pixStart ;
            rcol := ((lpix +pixels -1)  div blockWidth) ;
            lcol := ( lpix  div blockWidth) ;
            lstart := lpix mod blockWidth ;
          end;
         8 :
          begin
            pixels := Result ;
            lpix := bytestart ;
            rcol := ((lpix +pixels -1)  div blockWidth) ;
            lcol := ( lpix  div blockWidth) ;
            if layersNo = 1 then begin
              maxb := 0 ;
            end
            else  begin
              maxb := 2 ;
            end ;
            lstart := lpix mod blockWidth ;
          end ;
         4 :
          begin
            tilewidth3 := (blockWidth +1) div 2;
            pixels := Result ;
            lpix := bytestart ;
            rcol := ((bytestart +Result -1)  div tilewidth3) ;
            lcol := ( bytestart  div tilewidth3) ;
            lstart := lpix mod tilewidth3 ;
            maxb := 0 ;
          end;
         1 :
          begin
            tilewidth3 := (blockWidth +7) div 8 ;
            pixels := Result ;
            lpix := bytestart ;
            rcol := ((bytestart +Result -1)  div tilewidth3) ;
            lcol := ( bytestart  div tilewidth3) ;
            lstart := lpix mod tilewidth3 ;
            maxb := 0 ;
          end
        else begin
          Result := 0 ;
          exit ;
        end ;
      end ;

      tline   := _linenr mod blockLength ;

      stidx := blocksColumns * (_linenr div blockLength) ;
      lBlockIdx := lcol +stidx ;
      rBlockIdx := rcol +stidx ;

      if layersNo < 3 then
        maxidx := 0
      else
        maxidx := 2 ;

      nodata := True ;
      for b := 0 to maxidx do
       if bandsMap[b]>= 0 then begin
        getImgLineSegment(tline, bandsMap[b], b) ;
        nodata := False ;
       end;

      if nodata then begin
        Result := 0 ;
        exit ;
      end ;
      k := 0 ;
      if FBandsCount <=3 then begin
        for i := 0 to pixels -1 do begin
          for b := 0 to maxb do begin
            {$IFDEF OXYGENE}
              _buffer[_offset+k] :=
                bandsBBuffer[bandsIdxMapArray[b]][lstart +i] ;
            {$ELSE}
              PByte(NativeInt(_buffer) +k)^ :=
                bandsBBuffer[bandsIdxMapArray[b]][lstart +i] ;
            {$ENDIF}
            inc(k) ;
          end ;
        end ;
      end
      else begin
        for i := 0 to pixels -1 do begin
          for b := maxb downto 0 do begin
            {$IFDEF OXYGENE}
              _buffer[_offset+k] :=
                bandsBBuffer[bandsIdxMapArray[b]][lstart +i] ;
            {$ELSE}
              PByte(NativeInt(_buffer) +k)^ :=
                bandsBBuffer[bandsIdxMapArray[b]][lstart +i] ;
            {$ENDIF}
            inc(k) ;
          end ;
        end ;
      end ;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;


  function  TGIS_LayerIMG.getLinePixels ( const _buffer   : TGIS_Pixels  ;
                                          const _offset   : Integer ;
                                          const _linenr   : Integer ;
                                          const _pixStart : Integer ;
                                          const _pixCount : Integer
                                        ) : Integer;
  var
    i     : Integer ;
    buf   : TBytes ;
  begin
    Result := _pixCount ;

    if externalData then begin
      if realBitCount = 8 then begin
        getLinePixelsExt8 ( _buffer, _offset, _linenr, _pixStart, _pixCount) ;
        exit ;
      end;
    end;

    SetLength( buf, 3*_pixCount ) ;

    if getLineBits(buf, 0, _linenr, _pixStart, _pixCount) <= 0 then
      exit ;

    if realBitCount <= 8 then begin
      Result := convertBitsToPixels(buf, 0, _buffer, _offset,
                                    _pixStart, _pixCount) ;
    end
    else begin
      for i := _offset to _pixCount +_offset -1 do
        _buffer[i] := Integer($FF000000)   or
                     (buf[3*i +2] shl 16) or
                     (buf[3*i +1] shl 08) or
                      buf[3*i +0] ;
    end;
  end;

  function  TGIS_LayerIMG.getLinePixelsExt8 ( const _buffer   : TGIS_Pixels  ;
                                              const _offset   : Integer ;
                                              const _linenr   : Integer ;
                                              const _pixStart : Integer ;
                                              const _pixCount : Integer
                                            ) : Integer;
  var
    i, bidx     : Integer ;
    bytestart  : Integer ;
    bytestop  : Integer ;
    bytes  : Integer ;
    tline  : Integer ;

    linenr  : Integer ;
    r_idx, rcol   : Integer ;
    l_idx, lcol   : Integer ;
    step          : Integer ;
    stidx : Integer ;
    read : Integer ;
    offset64    : Int64 ;
    pal_idx : Integer ;
    oidx : Integer ;
    val3, val2    : Cardinal ;
    val1          : Cardinal ;
    sidx          : Integer ;
  begin
    Result := _pixCount ;

    if extZoom < 1 then begin
      step := TruncS(1/extZoom) ;
      linenr := RoundS(_linenr/extZoom) ;
      linenr := ((linenr +(step div 2)) div step) * step ;
      if linenr >= FBitHeight then
        linenr := FBitHeight -1 ;
      bytestart := TruncS(_pixStart/extZoom) ;
      bytestart := ((bytestart +(step div 2)) div step) * step ;
      bytes := RoundS(_pixCount/extZoom) ;
      if (bytestart +bytes) >= FBitWidth then begin
        if bytestart > 0 then begin
          bytestart := FBitWidth -bytes ;
          if bytestart < 0 then begin
            bytestart := 0 ;
            bytes := FBitWidth ;
          end
        end
        else begin
          bytestart := 0 ;
          bytes := FBitWidth ;
        end;
      end;
    end
    else begin
      linenr     := _linenr ;
      step := 1 ;
      bytes := _pixCount ;
      bytestart  := _pixStart ;
    end;

    bytestop   := bytestart +bytes-1 ;

    rcol := ((bytestart +bytes -1)  div blockWidth) ;
    lcol := ( bytestart  div blockWidth) ;
    bytestart := bytestart mod blockWidth ;

    tline   := linenr mod blockLength ;

    stidx := blocksColumns * (linenr div blockLength) ;
    l_idx := lcol +stidx ;
    r_idx := rcol +stidx ;

    read := 0 ;
    for bidx := l_idx to r_idx do
    begin
      offset64 := Int64(bidx)*extBlockSize +
                  tline*blockWidth+
                  extDataPos +bytestart;

      fileStream.Position :=  offset64 ;

      {$IFDEF OXYGENE}
        fileStream.Read( bandsBBuffer[0], read, blockWidth -bytestart) ;
      {$ELSE}
        fileStream.Read(bandsBBuffer[0][read], blockWidth -bytestart);
      {$ENDIF}
      read := read +blockWidth -bytestart;
      bytestart := 0 ;
    end;

    oidx := _offset ;
    for i := 0 to _pixCount -1 do begin
      sidx := RoundS(i/extZoom) ;
      pal_idx := bandsBBuffer[0][sidx] ;
      if unusedBlue then
        val3 := 0
      else
        val3 := Integer(bitPalette[pal_idx].B ) ;

      if unusedGreen then
        val2 := 0
      else
        val2 := Integer(bitPalette[pal_idx].G ) ;
      if unusedRed then
        val1 := 0
      else
        val1 := Integer(bitPalette[pal_idx].R ) ;

      _buffer[oidx +i] :=  Integer(val3 or ( val2 shl 8 ) or ( val1 shl 16)) or
                         Integer($FF000000) ;
    end;

  end;



  procedure  TGIS_LayerIMG.getImgLineSegment( const _tlinenr : Integer ;
                                              const _bandidx : Integer = 0;
                                              const _bufidx  : Integer = 0
                                             ) ;

  var
    bidx        : Integer ;
    read, readp : Integer ;
    offset64    : Int64 ;
    offset      : Integer ;
    i           : Integer ;
    wval        : Word    ;
    ival        : Integer ;
    maxidx      : Integer ;
    max, mmax   : Integer ;
    tilewidth   : Integer ;
    idxcurr     : Integer ;
    not_comressed : Boolean ;
  begin
    try

      case realBitCount of
        24 :
          tilewidth := blockWidth ;
         4 :
          tilewidth := (blockWidth +1) div 2;
         1 :
          tilewidth := (blockWidth +7) div 8 ;
         else
          tilewidth := blockWidth ; //8 bits
      end ;
      readp := tilewidth ;
      if bitsCountInfo = 16 then
        tilewidth := tilewidth*2 ;

      read := 0 ;
      idxcurr := 0 ;
      for bidx := lBlockIdx to rBlockIdx do
      begin
        if externalData then
          offset64 := Int64(_bandidx)* extBandSize +
                    Int64(bidx)*extBlockSize +
                    _tlinenr*tilewidth+
                    extDataPos
        else
          offset64 := blockInfoArray[_bandidx][bidx].offset +_tlinenr*tilewidth ;

        not_comressed := True ;
        if assigned(blockInfoArray[_bandidx]) then
          if blockInfoArray[_bandidx][bidx].CompressionType <> 0 then
            not_comressed := False ;

        if not_comressed then begin
          fileStream.Position :=  offset64 ;
          if bitsCountInfo = 16 then begin
            {$IFDEF OXYGENE}
              for i := 0 to (blockWidth*bytesPerPixel div 2)-1 do
                fileStream.ReadWord( bandsWBuffer[idxcurr +i], 2 ) ;
            {$ELSE}
              fileStream.Read(bandsWBuffer[idxcurr], blockWidth*bytesPerPixel) ;
            {$ENDIF}
            idxcurr := idxcurr +blockWidth ;
          end
          else begin
            {$IFDEF OXYGENE}
              fileStream.Read( bandsBBuffer[_bufidx], read, tilewidth ) ;
            {$ELSE}
              fileStream.Read(bandsBBuffer[_bufidx][read], tilewidth);
            {$ENDIF}
          end;
        end
        else begin
          if (bitsCountInfo = 16)  then begin
            decompressSBlock(_bandidx, bidx, _bufidx) ;
            offset := blockWidth*_tlinenr ;
            max := 0 ;

            if layersNo < 3 then
              maxidx := 0
            else
              maxidx := 2 ;

            for i := 0 to maxidx do begin
              mmax := RoundS(estaStatisticsArray[bandsMap[i]].maximum) ;
              if mmax > max then
                max := mmax ;
            end;
            if max = 0 then
              max := 255 ;

            if _bufidx = 0 then begin
              for i := 0 to blockWidth -1 do begin
                wval := decompressedSBlock0[actualColumn][offset +i] ;
                ival := (Integer(wval)*255) div max ;
                if ival < 255 then
                  bandsBBuffer[_bufidx][read +i] := Byte(ival)
                else
                  bandsBBuffer[_bufidx][read +i] := Byte(255) ;
              end ;
            end
            else
            if _bufidx = 1 then begin
              for i := 0 to blockWidth -1 do begin
                wval := decompressedSBlock1[actualColumn][offset +i] ;
                ival := (Integer(wval)*255) div max ;
                if ival < 255 then
                  bandsBBuffer[_bufidx][read +i] := Byte(ival)
                else
                  bandsBBuffer[_bufidx][read +i] := Byte(255) ;
              end ;
            end
            else
            begin
              for i := 0 to blockWidth -1 do begin
                wval := decompressedSBlock2[actualColumn][offset +i] ;
                ival := (Integer(wval)*255) div max ;
                if ival < 255 then
                  bandsBBuffer[_bufidx][read +i] := Byte(ival)
                else
                  bandsBBuffer[_bufidx][read +i] := Byte(255) ;
              end
            end;
          end
          else begin
            decompressBlock(_bandidx, bidx) ;
            offset := tilewidth*_tlinenr ;
            for i := 0 to tilewidth -1 do begin
              bandsBBuffer[_bufidx][read +i] :=
                  decompressedBlock[_bandidx][offset +i] ;
            end ;
          end ;

          if realBitCount = 1 then
            reverseBytes(_bufidx, read)
          else
          if realBitCount = 4 then // exchanges halves of bytes
            reverseHBytes(_bufidx, read)

        end ;
        read := read +readp ;
      end ;
      if (not FIsGridImage) and (bitsCountInfo = 16) then
        convertWords2Bytes(_bufidx, read) ;


      if FIsGridImage OR (bitsCountInfo = 16) then
        exit ;

      if (realBitCount >= 8) and (layersNo >= 3) then begin

        max := RoundS(estaStatisticsArray[_bandidx].maximum) ;
        if max = 0 then
          max := 255 ;

        for i := 0 to read -1 do begin
          ival := (Integer(bandsBBuffer[_bufidx][i])*255) div max ;
          if ival < 255 then
            bandsBBuffer[_bufidx][i] := Byte(ival)
          else
            bandsBBuffer[_bufidx][i] := Byte(255) ;
        end ;
      end ;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  procedure  TGIS_LayerIMG.getImgGLineSegment( const _tlinenr : Integer ;
                                               const _bandidx : Integer = 0
                                             ) ;

  var
    bidx      : Integer ;
    readp     : Integer ;
    offset64  : Int64   ;
    offset    : Integer ;
   {$IFDEF OXYGENE}
     j        : Integer ;
   {$ENDIF}
    tilewidth : Integer ;
    i         : Integer ;
    curridx   : Integer ;
    blocsno   : Integer ;
  begin
    try

      tilewidth := blockWidth ;
      readp := 0 ;
      blocsno := (FBitWidth +blockWidth -1) div blockWidth ;

      for bidx := lBlockIdx to rBlockIdx do
      begin
        curridx := bidx mod blocsno ;

        if externalData then
          offset64 := Int64(_bandidx)* extBandSize +
                    Int64(bidx)*extBlockSize +
                    _tlinenr*blockWidth*bytesPerPixel +
                    extDataPos
        else
          offset64 := blockInfoArray[_bandidx][bidx].offset +
                    TruncS(_tlinenr*blockWidth*bytesPerPixel) ;

        fileStream.Position :=  offset64 ;
        if bytesPerPixel = 4 then begin
          if externalData then begin
            if pixelType = T_IMGPixelType.F32 then begin
            {$IFDEF OXYGENE}
              for j := 0 to (TruncS(blockWidth*bytesPerPixel) div 4)-1 do
                fileStream.ReadSingle( bandsFBuffer[readp+j], 4 ) ;
            {$ELSE}
              fileStream.Read(bandsFBuffer[readp], TruncS(blockWidth*bytesPerPixel)) ;
            {$ENDIF}
            end;
          end
          else
          if pixelType = T_IMGPixelType.S32 then begin
          {$IFDEF OXYGENE}
            for j := 0 to (TruncS(blockWidth*bytesPerPixel) div 4)-1 do
              fileStream.ReadInteger( bandsIBuffer[readp+j], 4 ) ;
          {$ELSE}
            fileStream.Read(bandsIBuffer[readp], TruncS(blockWidth*bytesPerPixel)) ;
          {$ENDIF}
          end
          else begin
            if blockInfoArray[_bandidx][bidx].CompressionType <> 0 then begin
              if assigned(decompressedFBlockIdx) then begin
                if decompressedFBlockIdx[curridx] <> bidx  then
                  decompressFBlock(_bandidx, bidx) ;
              end else
                decompressFBlock(_bandidx, bidx) ;
              offset := tilewidth*_tlinenr ;
              for i := 0 to tilewidth -1 do
                bandsFBuffer[readp +i] := decompressedFBlock[curridx][offset +i] ;
            end
            else begin
            {$IFDEF OXYGENE}
              for j := 0 to (TruncS(blockWidth*bytesPerPixel) div 4)-1 do
                fileStream.ReadSingle( bandsFBuffer[readp+j], 4 ) ;
            {$ELSE}
              fileStream.Read(bandsFBuffer[readp], TruncS(blockWidth*bytesPerPixel)) ;
            {$ENDIF}
            end;
          end ;
        end
        else begin
          if blockInfoArray[_bandidx][bidx].CompressionType = 0 then begin
            if assigned(bandsSBuffer) then begin
              {$IFDEF OXYGENE}
                for j := 0 to blockWidth -1 do
                  fileStream.ReadSmallInt( bandsSBuffer[j], 2 ) ;
              {$ELSE}
                for i := 0 to blockWidth -1 do
                  fileStream.Read( bandsSBuffer[readp+i], 2 ) ;
              {$ENDIF}
            end
            else
            if assigned(bandsWBuffer) then begin
              {$IFDEF OXYGENE}
                for j := 0 to (TruncS(blockWidth*bytesPerPixel) div 2)-1 do
                  fileStream.ReadWord( bandsWBuffer[readp+j], 2 ) ;
              {$ELSE}
                fileStream.Read(bandsWBuffer[readp], TruncS(blockWidth*bytesPerPixel)) ;
              {$ENDIF}
            end;
          end
          else begin
            decompressSBlock(_bandidx, bidx, 0) ;
            offset := tilewidth*_tlinenr ;
            if assigned(bandsSBuffer) then begin
              for i := 0 to tilewidth -1 do
                bandsSBuffer[readp +i] := decompressedSBlock0[actualColumn][offset +i] ;
            end
            else begin
              for i := 0 to tilewidth -1 do begin
                bandsWBuffer[readp +i] := Word(decompressedSBlock0[actualColumn][offset +i]) ;
              end;
            end ;
          end ;
        end ;
        readp := readp +blockWidth ;
      end ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;

  end ;

  procedure TGIS_LayerIMG.prepareMinMaxZ( const _zoom : Double = -1 ) ;
  var
    i, k : Integer ;
    zoom : Double ;
    lines : Double ;
    fact : Double ;
    bands_buffer : TGIS_SingleArray ;
    test_val : Single ;
  const
    MAX_LINES = 900 ;
  begin
    if (FGridBand = 0) and rgbAsGrid then begin
      prepareMinMaxZGray ;
      exit ;
    end;

    Alive ;

    FMinZ :=  GIS_MAX_SINGLE ;
    FMaxZ := -GIS_MAX_SINGLE ;

    if (_zoom > 0) AND (_zoom <= 1) then begin
      lines := RoundS(FCellHeight * _zoom) ;
      if lines = 0 then
        lines := 1 ;
    end
    else
    begin
      if MAX_LINES > FCellHeight then
        lines := FCellHeight
      else
        lines := MAX_LINES ;
    end ;

    zoom := lines/FCellHeight ;
    fact := zoom/2 ;
    SetLength(bands_buffer, FCellWidth) ;
    if FNoDataValue > 0 then
      test_val := FNoDataValue
    else
      test_val := -FNoDataValue ;


    for i := 0 to FBitHeight -1 do begin
      fact := fact + zoom ;
      if fact >= 1 then begin
        fact := fact - 1 ;
        getNativeLine(bands_buffer, i, 0, FCellWidth) ;
        for k := 0 to FCellWidth -1 do begin
          if (bands_buffer[k] <  test_val) and
             (bands_buffer[k] > -test_val)then
          begin
            if bands_buffer[k] < FMinZ then
              FMinZ := bands_buffer[k] ;
            if bands_buffer[k] > FMaxZ then
              FMaxZ := bands_buffer[k] ;
          end ;
        end ;
      end ;
    end ;

    FExtent3D.ZMin := FMinZ ;
    FExtent3D.ZMax := FMaxZ ;

    bands_buffer := nil ;
  end ;


  function  TGIS_LayerIMG.getNativeValue( const _pt : TPoint  ;
                                          const _ar : TGIS_DoubleArray
                                         ) : Boolean ;

  var
    stidx  : Integer ;
    tline  : Integer ;
    rcol   : Integer ;
    lcol   : Integer ;
    lstart : Integer ;

  begin
    try
      Result := True ;

      tline   := _pt.Y mod blockLength ;
      rcol := _pt.X   div blockWidth ;
      lcol := rcol ;

      stidx := blocksColumns * (_pt.Y div blockLength) ;
      lBlockIdx := lcol +stidx ;
      rBlockIdx := rcol +stidx ;
      lstart := _pt.X mod blockWidth ;

      if not FIsNativeGridImage then begin
        if is48BitsPerPixel then begin
          getImgLineSegment(tline, bandsMap[0], 0) ;
          if blockInfoArray[bandsMap[0]][lBlockIdx].CompressionType = 0 then
            _ar[2] :=  bandsWBuffer[lstart]
          else
            _ar[2] :=  decompressedSBlock0[actualColumn][ blockWidth*tline +lstart] ;

          getImgLineSegment(tline, bandsMap[1], 1) ;
          if blockInfoArray[bandsMap[1]][lBlockIdx].CompressionType = 0 then
            _ar[1] :=  bandsWBuffer[lstart]
          else
            _ar[1] :=  decompressedSBlock1[actualColumn][blockWidth*tline +lstart] ;

          getImgLineSegment(tline, bandsMap[2], 2) ;
          if blockInfoArray[bandsMap[2]][lBlockIdx].CompressionType = 0 then
            _ar[0] :=  bandsWBuffer[lstart]
          else
            _ar[0] :=  decompressedSBlock2[actualColumn][blockWidth*tline +lstart] ;
        end
        else begin
          getImgLineSegment(tline, bandsMap[0], lBlockIdx) ;
          _ar[0] :=  bandsBBuffer[0][lstart] ;
        end;
      end
      else begin
        getImgGLineSegment(tline) ;
        if bitsCountInfo = 16 then begin
          if pixelType = T_IMGPixelType.S16 then
            _ar[0] :=  bandsSBuffer[lstart]
          else
            _ar[0] :=  bandsWBuffer[lstart] ;
        end
        else
          _ar[0] :=  bandsFBuffer[lstart] ;
      end ;

      if _ar[0] = FNoDataValue then
        Result := False ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  function TGIS_LayerIMG.getNativeLine( const _buffer   : TGIS_SingleArray ;
                                        const _linenr   : Integer          ;
                                        const _startIdx : Integer          ;
                                        const _count    : Integer
                                      ) : Integer ;
  var
    stidx  : Integer ;
    tline  : Integer ;
    rcol   : Integer ;
    lcol   : Integer ;
    i, k   : Integer ;
    bno    : Integer ;
    stno   : Integer ;
    lstidx  : Integer ;
  begin

    try
      Result := 0 ;
      if (_linenr < 0) or (_linenr > FBitHeight) then
        exit ;

      tline   := _linenr mod blockLength ;
      lstidx := _startIdx mod blockWidth ;

      rcol := (_startIdx +_count -1) div blockWidth ;
      lcol := _startIdx div blockWidth ;

      stidx := blocksColumns * (_linenr div blockLength) ;
      lBlockIdx := lcol +stidx ;
      rBlockIdx := rcol +stidx ;
      bno := length(_buffer) div _count ;
      if bno <> FBandsCount then
        bno := 1 ;

      if bno > 1 then
        stno := 0
      else
        stno :=  bandsMap[0] ;


      if not FIsNativeGridImage then begin
        for k :=0 to bno -1 do begin
          getImgLineSegment(tline, stno, 0) ;
          if bitsCountInfo = 16 then begin
            for i := 0 to _count -1 do
              _buffer[i*bno +k] := bandsWBuffer[i + lstidx] ;
          end
          else begin
            for i := 0 to _count -1 do
              _buffer[i*bno +k] := bandsBBuffer[0][i + lstidx] ;
          end;
          inc(stno) ;
        end;
      end
      else begin
        getImgGLineSegment(tline) ;
        if bitsCountInfo = 16 then begin
          for i := 0 to _count -1 do begin
            if pixelType = T_IMGPixelType.S16 then
              _buffer[i] := bandsSBuffer[i + lstidx]
            else
              _buffer[i] := bandsWBuffer[i + lstidx] ;
          end
        end
        else
        if bitsCountInfo = 32 then begin
          if pixelType = T_IMGPixelType.S32 then begin
            for i := 0 to _count -1 do
              _buffer[i] := bandsIBuffer[i + lstidx] ;
          end
          else begin
            for i := 0 to _count -1 do
              _buffer[i] := bandsFBuffer[i + lstidx] ;
          end ;
        end ;
      end ;
      Result := _count ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;


//==============================================================================
// Lider.CG.GIS.GeoLayerIMG
//==============================================================================

  class procedure GisLayerIMG.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-IMG', 'ERDAS IMAGINE Image File Format',
                   TGIS_LayerIMG, '.img',
                   TGIS_RegisteredLayerType.Pixel,
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                   True
                 ) ;
  end ;


//==============================================================================
// initialization/finalization
//==============================================================================

{$IFDEF DCC}
  initialization
    GisLayerIMG.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.

