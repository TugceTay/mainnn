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
  Procedures to read/write an image using GDAL.
}

{$IFDEF DCC}
  unit GisFileGDAL ;
  {$HPPEMIT '#pragma link "GisFileGDAL"'}
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
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    System.Classes,
    System.SysUtils,

    GisTypes,
    GisTypesUI,
    GisFilePixel,
    GisCsSystems ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl;
{$ENDIF}

type

  /// <summary>
  ///   The Class which encapsulates the reading/writing of image using GDAL.
  /// </summary>
  TGIS_FileGDAL = {$IFDEF OXYGENE} public {$ENDIF} class ( TGIS_FilePixel )
    private
      FLib        : TObject ;
      FDLLPath    : String ;
      FSubDataset : String ;
      FDataset    : TObject ;
      FInit       : Boolean ;
      FConvBmp    : TGIS_Bitmap ;
      {$IFDEF CLR}
      //FConvBmpEx  : TBitmapEx ;
      {$ENDIF}
      FIsPalette  : Boolean ;
      {$IFDEF CLR}
        paletteEntry : array of System.Drawing.Color ;
      {$ELSE}
        paletteEntry : array of TPaletteEntry ;
      {$ENDIF}
      FCreateOptions : TStrings ;
    private

      /// <summary>
      ///   Initialize library.
      /// </summary>
      function  initialize : Boolean ;

      /// <summary>
      ///   Find a driver that works with given extension.
      /// </summary>
      /// <param name="_ext">
      ///   extension name
      /// </param>
      function  findDriver( const _ext : String ) : String ;

      /// <summary>
      ///   Create a dataset.
      /// </summary>
      /// <param name="_driverName">
      ///   name of GDAL driver
      /// </param>
      function  createDataset( const _driverName   : String ;
                               const _numBands     : Integer ;
                               const _dataType     : Integer
                              ) : Boolean ;

      /// <summary>
      ///   Throw initialize error due to missing dll.
      /// </summary>
      procedure throwInitializeError ;
    protected // internal TGIS_FilePixel

      /// <inheritdoc/>

      procedure prepareCapabilities  ; override;
    protected

      /// <inheritdoc/>
      procedure doDestroy ; override;
    public // internal TGIS_FilePixel

      /// <inheritdoc/>
      constructor Create              ( const _path        : String      ;
                                        const _ext         : TGIS_Extent ;
                                        const _width       : Integer     ;
                                        const _height      : Integer     ;
                                        const _subformat   : TGIS_LayerPixelSubFormat     ;
                                        const _ppi         : Integer     ;
                                        const _cs          : TGIS_CSCoordinateSystem
                                      ) ; override;

      /// <summary>
      ///   Create and open a new pixel file.
      /// </summary>
      /// <param name="_path">
      ///   path to a file
      /// </param>
      constructor Create              ( const _path        : String
                                      ) ; {$IFNDEF OXYGENE}
                                           overload;
                                          {$ENDIF}

      /// <inheritdoc/>
      constructor Create               ; override;

      /// <inheritdoc/>
      procedure   Write              ( const _x       : Integer ;
                                       const _y       : Integer ;
                                       const _pixels  : TGIS_Pixels ;
                                       const _pformat : TGIS_PixelFormat ;
                                       const _width   : Integer ;
                                       const _height  : Integer
                                     ) ; override;

      /// <inheritdoc/>
      procedure   WriteGrid          ( const _x            : Integer ;
                                       const _y            : Integer ;
                                       const _grd          : TGIS_GridArray
                                     ) ; override;

      /// <summary>
      ///   Return a description.
      /// </summary>
      /// <returns>
      ///    file information
      /// </returns>
      function   GetInfo : String ;
    public

      /// <summary>
      ///   Open a dataset.
      /// </summary>
      /// <param name="_subdatasets">
      ///   if true, open subdatasets
      /// </param>
      procedure OpenDataset             ( const _subdatasets : Boolean
                                        ) ;

      /// <summary>
      ///   Build a new dataset.
      /// </summary>
      /// <param name="_isGrid">
      ///   is dataset a grid
      /// </param>
      /// <returns>
      ///    True if succeeded
      /// </returns>
      function  BuildDataset            ( const _isGrid       : Boolean
                                        ) : Boolean ;

      /// <summary>
      ///   Check if can create a dataset.
      /// </summary>
      /// <param name="_isGrid">
      ///   is dataset a grid
      /// </param>
      /// <returns>
      ///    True if succeeded
      /// </returns>
      function  CanCreateDataset        ( const _isGrid       : Boolean
                                        ) : Boolean ;

      /// <summary>
      ///   Read dataset basic properties.
      /// </summary>
      /// <param name="_xSize">
      ///   x size
      /// </param>
      /// <param name="_ySize">
      ///   y size
      /// </param>
      /// <param name="_csWkt">
      ///   projection wkt
      /// </param>
      /// <param name="_extent">
      ///   extent
      /// </param>
      /// <param name="_scaleX">
      ///   scale x
      /// </param>
      /// <param name="_scaleY">
      ///   scale y
      /// </param>
      /// <param name="_rotateX">
      ///   rotation x
      /// </param>
      /// <param name="_rotateY">
      ///   rotation y
      /// </param>
      /// <param name="_bands">
      ///   number of bands
      /// </param>
      /// <param name="_channels">
      ///   array of channels
      /// </param>
      procedure GetDatasetProperties    ( var _xSize          : Integer ;
                                          var _ySize          : Integer ;
                                          var _csWkt          : String  ;
                                          var _extent         : TGIS_Extent ;
                                          var _scaleX         : Double ;
                                          var _scaleY         : Double ;
                                          var _rotateX        : Double ;
                                          var _rotateY        : Double ;
                                          var _bands          : Integer ;
                                          var _channels       : TGIS_IntegerArray
                                         ) ;

      /// <summary>
      ///   Read raster band basic properties.
      /// </summary>
      /// <param name="_id">
      ///   band id
      /// </param>
      /// <param name="_xSize">
      ///   band x size
      /// </param>
      /// <param name="_dataType">
      ///   band data type
      /// </param>
      /// <param name="_dataSize">
      ///   band data size
      /// </param>
      /// <param name="_zMin">
      ///   band minimum value
      /// </param>
      /// <param name="_zMax">
      ///   band maximum value
      /// </param>
      /// <param name="_noData">
      ///   band nodata value
      /// </param>
      /// <param name="_colorInt">
      ///   color interpretation
      /// </param>
      /// <param name="_colors">
      ///   size of color table
      /// </param>
      procedure GetRasterBandProperties  ( const _id          : Integer ;
                                             var _xSize       : Integer ;
                                             var _dataType    : String ;
                                             var _dataSize    : Integer ;
                                             var _zMin        : Single  ;
                                             var _zMax        : Single ;
                                             var _noData      : Single ;
                                             var _colorInt    : String ;
                                             var _colors      : Integer
                                          ) ;

      /// <summary>
      ///   Get color entry from color table.
      /// </summary>
      /// <param name="_id">
      ///   band id
      /// </param>
      /// <param name="_tabIndex">
      ///   table index
      /// </param>
      /// <param name="_colorIndex">
      ///   color index
      /// </param>
      /// <returns>
      ///    value
      /// </returns>
      function  GetRasterBandGetColorEntry( const _id         : Integer ;
                                            const _tabIndex   : Integer ;
                                            const _colorIndex : Integer
                                           ) : Byte ;

      /// <summary>
      ///   Read raster band grid data.
      /// </summary>
      /// <param name="_id">
      ///   band id
      /// </param>
      /// <param name="_xOff">
      ///   the x coordinate of the top left corner of the rectangular region
      ///   of the image to be read/written.
      /// </param>
      /// <param name="_yOff">
      ///   the y coordinate of the top left corner of the rectangular region
      ///   of the image.
      /// </param>
      /// <param name="_xSize">
      ///   the width of the rectangular region of the image.
      /// </param>
      /// <param name="_ySize">
      ///   the height of the rectangular region of the image.
      /// </param>
      /// <param name="_buffer">
      ///   pointer to an in memory buffer.
      /// </param>
      /// <param name="_bufXSize">
      ///   the width of the buffer in pixels.
      /// </param>
      /// <param name="_bufYSize">
      ///   the height of the buffer in pixels.
      /// </param>
      procedure ReadRasterBandGrid        ( const _id         : Integer ;
                                            const _xOff       : Integer ;
                                            const _yOff       : Integer ;
                                            const _xSize      : Integer ;
                                            const _ySize      : Integer ;
                                            const _buffer     : TGIS_SingleArray ;
                                            const _bufXSize   : Integer ;
                                            const _bufYSize   : Integer
                                           ) ;

      /// <summary>
      ///   Read raster band rgb data.
      /// </summary>
      /// <param name="_id">
      ///   band id
      /// </param>
      /// <param name="_xOff">
      ///   the x coordinate of the top left corner of the rectangular region
      ///   of the image to be read/written.
      /// </param>
      /// <param name="_yOff">
      ///   the y coordinate of the top left corner of the rectangular region
      ///   of the image.
      /// </param>
      /// <param name="_xSize">
      ///   the width of the rectangular region of the image.
      /// </param>
      /// <param name="_ySize">
      ///   the height of the rectangular region of the image.
      /// </param>
      /// <param name="_buffer">
      ///   pointer to an in memory buffer.
      /// </param>
      /// <param name="_bufXSize">
      ///   the width of the buffer in pixels.
      /// </param>
      /// <param name="_bufYSize">
      ///   the height of the buffer in pixels.
      /// </param>
      procedure ReadRasterBandRGB         ( const _id         : Integer ;
                                            const _xOff       : Integer ;
                                            const _yOff       : Integer ;
                                            const _xSize      : Integer ;
                                            const _ySize      : Integer ;
                                            const _buffer     : TBytes ;
                                            const _bufXSize   : Integer ;
                                            const _bufYSize   : Integer
                                           ) ;

      /// <summary>
      ///   Write raster band rgb data.
      /// </summary>
      /// <param name="_id">
      ///   band id
      /// </param>
      /// <param name="_xOff">
      ///   the x coordinate of the top left corner of the rectangular region
      ///   of the image to be read/written.
      /// </param>
      /// <param name="_yOff">
      ///   the y coordinate of the top left corner of the rectangular region
      ///   of the image.
      /// </param>
      /// <param name="_xSize">
      ///   the width of the rectangular region of the image.
      /// </param>
      /// <param name="_ySize">
      ///   the height of the rectangular region of the image.
      /// </param>
      /// <param name="_buffer">
      ///   pointer to an in memory buffer.
      /// </param>
      /// <param name="_bufXSize">
      ///   the width of the buffer in pixels.
      /// </param>
      /// <param name="_bufYSize">
      ///   the height of the buffer in pixels.
      /// </param>
      procedure WriteRasterBandRGB        ( const _id         : Integer ;
                                            const _xOff       : Integer ;
                                            const _yOff       : Integer ;
                                            const _xSize      : Integer ;
                                            const _ySize      : Integer ;
                                            const _buffer     : TBytes ;
                                            const _bufXSize   : Integer ;
                                            const _bufYSize   : Integer
                                           ) ;

      /// <summary>
      ///   Read raster band grid data.
      /// </summary>
      /// <param name="_id">
      ///   band id
      /// </param>
      /// <param name="_xOff">
      ///   the x coordinate of the top left corner of the rectangular region
      ///   of the image to be read/written.
      /// </param>
      /// <param name="_yOff">
      ///   the y coordinate of the top left corner of the rectangular region
      ///   of the image.
      /// </param>
      /// <param name="_xSize">
      ///   the width of the rectangular region of the image.
      /// </param>
      /// <param name="_ySize">
      ///   the height of the rectangular region of the image.
      /// </param>
      /// <param name="_buffer">
      ///   pointer to an in memory buffer.
      /// </param>
      /// <param name="_bufXSize">
      ///   the width of the buffer in pixels.
      /// </param>
      /// <param name="_bufYSize">
      ///   the height of the buffer in pixels.
      /// </param>
      procedure WriteRasterBandGrid       ( const _id         : Integer ;
                                            const _xOff       : Integer ;
                                            const _yOff       : Integer ;
                                            const _xSize      : Integer ;
                                            const _ySize      : Integer ;
                                            const _buffer     : TGIS_SingleArray ;
                                            const _bufXSize   : Integer ;
                                            const _bufYSize   : Integer
                                           ) ;

      /// <summary>
      ///   Check if file can be opened.
      /// </summary>
      /// <returns>
      ///   True if can open a file
      /// </returns>
      function PreRecognize : Boolean ;

     {$IFDEF OXYGENE}
       /// <summary>
       ///   Get available formats with their capabilities.
       /// </summary>
       /// <remarks>
       ///    List Objects property keeps layer read only|write flag value 0|1
       /// </remarks>
       /// <returns>
       ///   list of formats
       /// </returns>
       function GetAvailableFormats : TGIS_Strings ;
     {$ELSE}
       /// <summary>
       ///   Get available formats with their capabilities.
       /// </summary>
       /// <remarks>
       ///    List Objects property keeps layer read only|write flag value 0|1
       /// </remarks>
       /// <returns>
       ///   list of formats
       /// </returns>
       function GetAvailableFormats : TStrings     ;
     {$ENDIF}
     {$IFDEF OXYGENE}
       /// <summary>
       ///   Get metadata list from domain.
       /// </summary>
       /// <param name="_domain">
       ///   domain name
       /// </param>
       /// <returns>
       ///   list of metadata
       /// </returns>
       function GetMetadata( const _domain : String ) : TGIS_Strings ;
     {$ELSE}
       /// <summary>
       ///   Get metadata list from domain.
       /// </summary>
       /// <param name="_domain">
       ///   domain name
       /// </param>
       /// <returns>
       ///   list of metadata
       /// </returns>
       function GetMetadata( const _domain : String ) : TStrings     ;
     {$ENDIF}

      /// <summary>
      ///   Set global config option.
      /// </summary>
      /// <param name="_key">
      ///   key name
      /// </param>
      /// <param name="_val">
      ///   value name
      /// </param>
      procedure SetConfigOption( const _key : String ;
                                 const _val : String
                                ) ;

      /// <summary>
      ///   Set dataset creation option.
      /// </summary>
      /// <param name="_key">
      ///   key name
      /// </param>
      /// <param name="_val">
      ///   value name
      /// </param>
      procedure SetCreateOption( const _key : String ;
                                 const _val : String
                                ) ;

      /// <summary>
      ///   Create a dataset copy.
      /// </summary>
      /// <param name="_destFileName">
      ///   destination file path
      /// </param>
      /// <param name="_destDriver">
      ///   destination driver
      /// </param>
      /// <param name="_strict">
      ///   strict flag
      /// </param>
      /// <param name="_options">
      ///   list of options
      /// </param>
      procedure CreateCopy( const _destFileName : String ;
                            const _destDriver   : String ;
                            const _strict       : Boolean ;
                            {$IFDEF OXYGENE}
                              const _options    : TStrings
                            {$ELSE}
                              const _options    : TStrings
                            {$ENDIF}
                           ) ;
     {$IFDEF OXYGENE}
       /// <summary>
       ///   Get creation options of a dataset.
       /// </summary>
       /// <returns>
       ///   list of options
       /// </returns>
       function GetCreateOptions : TGIS_Strings ;
     {$ELSE}
       /// <summary>
       ///   Get creation options of a dataset.
       /// </summary>
       /// <returns>
       ///   list of options
       /// </returns>
       function GetCreateOptions : TStrings     ;
     {$ENDIF}
    public
      /// <summary>
      ///   GDAL DLL path.
      /// </summary>
      property DLLPath    : String  read FDLLPath write FDLLPath ;
      /// <summary>
      ///   SubDataset name.
      /// </summary>
      property SubDataset : String  read FSubDataset write FSubDataset ;
  end ;

//##############################################################################
implementation

{$IFDEF CLR}
uses
  System.Security,
  System.IO,
  System.Runtime.InteropServices ;
{$ENDIF}
{$IFDEF DCC}
  uses
    GisRtl,
    GisFunctions,
    GisInternals,
    GisClasses,
    GisResource ;
{$ENDIF}

//==============================================================================
// GDAL defines
//==============================================================================

const
  GDAL_DEFAULT_DLL_NAME       = 'gdal300.dll' ;
  GDAL_DEFAULT_DLL_NAME_NOEXT = 'gdal300' ;

type
  { Define handle types related to various internal classes. }
    {$IFDEF JAVA}
    GDALMajorObjectH  = com.sun.jna.Pointer ;
    GDALDatasetH      = com.sun.jna.Pointer ;
    GDALRasterBandH   = com.sun.jna.Pointer ;
    GDALDriverH       = com.sun.jna.Pointer ;
    GDALColorTableH   = com.sun.jna.Pointer ;
    HandleRef         = com.sun.jna.Pointer ;
    {$ELSE}
    GDALMajorObjectH  = IntPtr ;
    GDALDatasetH      = IntPtr ;
    GDALRasterBandH   = IntPtr ;
    GDALDriverH       = IntPtr ;
    GDALColorTableH   = IntPtr ;
    {$ENDIF}

  { Pixel data types. }
    GDALDataType      = Integer ;
  { Flag indicating read/write, or read-only access to data. }
    GDALAccess        = Integer ;
  { Read/Write flag for RasterIO() method. }
    GDALRWFlag        = Integer ;
  { Types of color interpretation for raster bands. }
    GDALColorInterp   = Integer ;
  { Types of color interpretation for raster bands. }
    GDALPaletteInterp = Integer ;
  { Types of errors. }
    CPLErr            = Integer ;

  { Color tuple. }
  {$IFDEF CLR}
    [StructLayout(LayoutKind.Sequential)]
  {$ENDIF}
    GDALColorEntry = record
      { gray, red, cyan or hue }
      c1 : SmallInt ;
      { green, magenta, or lightness }
      c2 : SmallInt ;
      { blue, yellow, or saturation }
      c3 : SmallInt ;
      { alpha or blackband }
      c4 : SmallInt ;
    end ;
  {$IFDEF CLR}
    PGDALCOLORENTRY = IntPtr ;
  {$ENDIF}
  {$IFDEF JAVA}
    PGDALCOLORENTRY = com.sun.jna.Pointer ;
  {$ENDIF}
  {$IFDEF DCC}
    PGDALCOLORENTRY = ^GDALCOLORENTRY ;
  {$ENDIF}

type
  {$IFDEF OXYGENE}
    T_GDALProgressFunc = delegate ( dfComplete   : Double ;
                                   pszMessage   : String ;
                                   pProgressArg : IntPtr
                                  ) : Integer ;
  {$ELSE}
    T_GDALProgressFunc = function( dfComplete   : Double ;
                                   pszMessage   : PAnsiChar ;
                                   pProgressArg : IntPtr
                                  ) : Integer ; stdcall ;
  {$ENDIF}

const
    // data type
    GDT_Unknown   =  0 ;
    GDT_Byte      =  1 ;
    GDT_UInt16    =  2 ;
    GDT_Int16     =  3 ;
    GDT_UInt32    =  4 ;
    GDT_Int32     =  5 ;
    GDT_Float32   =  6 ;
    GDT_Float64   =  7 ;
    GDT_CInt16    =  8 ;
    GDT_CInt32    =  9 ;
    GDT_CFloat32  = 10 ;
    GDT_CFloat64  = 11 ;

    // access type
    GA_ReadOnly   =  0 ;
    GA_Update     =  1 ;

    // read write
    GF_Read       =  0 ;
    GF_Write      =  1 ;

    // error codes
    CE_None       =  0 ;
    CE_Debug      =  1 ;
    CE_Warning    =  2 ;
    CE_Failure    =  3 ;
    CE_Fatal      =  4 ;

    // color interp
    GCI_Undefined        =  0 ;
    GCI_GrayIndex        =  1 ;
    GCI_PaletteIndex     =  2 ;
    GCI_RedBand          =  3 ;
    GCI_GreenBand        =  4 ;
    GCI_BlueBand         =  5 ;
    GCI_AlphaBand        =  6 ;
    GCI_HueBand          =  7 ;
    GCI_SaturationBand   =  8 ;
    GCI_LightnessBand    =  9 ;
    GCI_CyanBand         = 10 ;
    GCI_MagentaBand      = 11 ;
    GCI_YellowBand       = 12 ;
    GCI_BlackBand        = 13 ;
    GCI_YCbCr_YBand      = 14 ;
    GCI_YCbCr_CbBand     = 15 ;
    GCI_YCbCr_CrBand     = 16 ;

    // palette interp
    GPI_Gray  = 0 ;
    GPI_RGB   = 1 ;
    GPI_CMYK  = 2 ;
    GPI_HLS   = 3 ;

type

//==============================================================================
// GDAL API wrappers
//==============================================================================

  {$IFDEF JAVA}
    IGDALLibrary = interface (com.sun.jna.Library)
       procedure GDALAllRegister ;
       function GDALOpen( const _pszFilename  : TBytes;
                          const _eAccess      : GDALAccess
                         ) : GDALDatasetH ;
       function GDALCreate( const _hDriver        : GDALDriverH;
                            const _pszFilename    : String;
                            const _nXSize         : Integer;
                            const _nYSize         : Integer;
                            const _nBands         : Integer;
                            const _eBandType      : GDALDataType;
                            const _papszParmList  : com.sun.jna.Pointer
                            ) : GDALDatasetH ;
      function GDALCreateCopy ( const _hDriver        : GDALDriverH;
                                 const _pszFilename    : String;
                                 const _hSrcDS         : GDALDatasetH;
                                 const _bStrict        : Integer;
                                 const _papszOptions   : com.sun.jna.Pointer ;
                                 const _pfnProgress    : T_GDALProgressFunc ;
                                 const _pProgressData  : IntPtr
                                ) : GDALDatasetH ;
       function GDALGetDataTypeSize( const _eDataType : GDALDataType
                                         ) : Integer ;
       function GDALGetMetadata( const _hDataset   : GDALMajorObjectH;
                                 const _pszDomain  : String
                                ) : IntPtr ;
       function GDALGetMetadataItem( const _hObject    : GDALMajorObjectH;
                                     const _pszName    : String ;
                                     const _pszDomain  : String
                                    ) : String ;
       function GDALGetDatasetDriver( const _hDataset  : GDALDatasetH
                                      ) : GDALDriverH ;
       function GDALGetDriverByName( const _name  : String
                                      ) : GDALDriverH ;
       function GDALGetDriver( const _index : Integer
                               ) : GDALDriverH ;
       function GDALGetDriverCount : Integer ;
       procedure GDALClose( const _hDataset  : GDALDatasetH
                                      ) ;
       function GDALGetRasterXSize( const _hDataset : GDALDatasetH
                                     ) : Integer ;
       function GDALGetRasterYSize( const _hDataset : GDALDatasetH
                                     ) : Integer ;
       function GDALGetRasterCount( const _hDataset : GDALDatasetH
                                     ) : Integer ;
       function GDALGetRasterBand( const _hDataset : GDALDatasetH;
                                      const _nBandId  : Integer
                                     ) : GDALRasterBandH ;
       function GDALGetProjectionRef( const _hDataset : GDALDatasetH
                                       ) : String ;
       function GDALSetProjection( const _hDataset      : GDALDatasetH ;
                                  const _pszProjection : String
                                 ) : CPLErr ;
       function GDALGetGeoTransform( const _hDataset       : GDALDatasetH;
                                    const _padfTransform  : com.sun.jna.Pointer
                                   ) : CPLErr ;
       function GDALSetGeoTransform( const _hDataset       : GDALDatasetH;
                                    const _padfTransform  : com.sun.jna.Pointer
                                   ) : CPLErr ;
       function GDALGetGCPProjection( const _hDataset : GDALDatasetH
                                      ) : String ;
       function GDALGetGCPCount( const _hDataset : GDALDatasetH
                                      ) : Integer ;
       function GDALGetGCPs( const _hDataset : GDALDatasetH
                                      ) : com.sun.jna.Pointer ;
       function GDALGCPsToGeoTransform( const _nGCPCount     : Integer ;
                                       const _asGCPs        : com.sun.jna.Pointer ;
                                       const _padfTransform : com.sun.jna.Pointer ;
                                       const _bApproxOK     : Integer
                                     ) : Integer ;
       function GDALGetDriverShortName( const _hDriver : GDALDriverH
                                       )  : String ;
       function GDALGetDriverLongName( const _hDriver : GDALDriverH
                                       )  : String ;
       function GDALGetRasterDataType( const _hBand : GDALRasterBandH
                                       ) : GDALDataType ;
       function GDALGetDataTypeName( const _eDataType : GDALDataType
                                    ) : String ;
       procedure GDALGetBlockSize( const _hBand      : GDALRasterBandH;
                                    const _pnXSize   : com.sun.jna.Pointer;
                                    const _pnYSize   : com.sun.jna.Pointer
                                   )  ;
       function GDALGetRasterBandXSize( const _hBand : GDALRasterBandH
                                        ) : Integer ;
       function GDALGetRasterBandYSize( const _hBand : GDALRasterBandH
                                        ) : Integer ;
       function GDALCreateColorTable( const _eColorInterp : GDALColorInterp
                                      ) : GDALColorTableH ;
       function GDALGetRasterColorInterpretation( const _hBand : GDALRasterBandH
                                                  ) : GDALColorInterp ;
       function GDALGetRasterColorTable( const _hBand : GDALRasterBandH
                                         ) : GDALColorTableH ;
       function GDALGetRasterMaximum( const _hBand     : GDALRasterBandH;
                                       const _pbSuccess : Integer
                                      ) : Double ;
       function GDALGetRasterMinimum( const _hBand     : GDALRasterBandH;
                                       const _pbSuccess : Integer
                                      ) : Double ;
       procedure GDALComputeRasterMinMax( const _hBand     : GDALRasterBandH;
                                           const _bApproxOK : Integer;
                                           const _adfMinMax : com.sun.jna.Pointer
                                          ) ;
       function GDALRasterIO( const _hRBand      : GDALRasterBandH;
                               const _eRWFlag     : GDALRWFlag;
                               const _nXOff       : Integer;
                               const _nYOff       : Integer;
                               const _nXSize      : Integer;
                               const _nYSize      : Integer;
                               const _pBuffer     : com.sun.jna.Pointer;
                               const _nBufXSize   : Integer;
                               const _nBufYSize   : Integer;
                               const _eBufType    : GDALDataType;
                               const _nPixelSpace : Integer;
                               const _nLineSpace  : Integer
                              ) : CPLErr ;
       function GDALGetRasterNoDataValue( const _hRBand    : GDALRasterBandH ;
                                         const _pbSuccess : Integer
                                        ) : Double ;
       function GDALSetRasterNoDataValue( const _hRBand    : GDALRasterBandH ;
                                         const _pvalue    : Double
                                        ) : CPLErr ;
       function GDALGetColorEntryCount( const _hColorTable : GDALColorTableH
                                        ) : Integer ;
       procedure GDALSetColorEntry(  const _hColorTable : GDALColorTableH;
                                    const _nTabIndex   : Integer ;
                                    const _entry       : PGDALCOLORENTRY
                                   ) ;
       function GDALGetColorEntry(  const _hColorTable : GDALColorTableH;
                                     const _nTabIndex   : Integer
                                   ) : PGDALCOLORENTRY ;
       function GDALGetColorInterpretationName ( const _eColorInterp : GDALColorInterp
                                      ) : String ;
       function CSLFetchBoolean( const _papszStrList  : IntPtr ;
                                  const _pszKey       : String ;
                                  const _bDefault     : Integer
                                ) : Integer ;
       function CSLCount( const _papszStrList : IntPtr
                            ) : Integer ;
       function CSLGetField( const _papszStrList : IntPtr ;
                             const _iField       : Integer
                            ) : String ;
       function CPLGetLastErrorMsg : String ;
       procedure CPLSetConfigOption( const _pszKey : String ;
                                        const _pszVal : String
                                       ) ;
       function CSLAddString ( _papszStrList : com.sun.jna.Pointer ;
                                       const _pszNewString : String
                                      ) : com.sun.jna.Pointer ;
       procedure CSLDestroy  ( _papszStrList : com.sun.jna.Pointer ) ;
    end;

    T_GDALMapper = class( com.sun.jna.win32.StdCallFunctionMapper )
      public
        method getFunctionName(&library: com.sun.jna.NativeLibrary; &method: &Method): String; override;
    end ;
  {$ENDIF}

  { Gdal library helper.
  }
  T_GDALLib = class
     {$IFDEF JAVA} public {$ELSE} private {$ENDIF}
      {$IFDEF JAVA}
      class var DLLHandle : IGDALLibrary ;
      {$ELSE}
      DLLHandle : THandle ;
      {$ENDIF}
      DLLLoaded : Boolean ;
    public
    {$IFDEF OXYGENE}
      GDALAllRegisterProc          : IntPtr ;
      GDALOpenProc                 : IntPtr ;
      GDALCloseProc                : IntPtr ;
      GDALCreateProc               : IntPtr ;
      GDALComputeRasterMinMaxProc  : IntPtr ;
      GDALGetDriverLongNameProc    : IntPtr ;
      GDALGetDriverShortNameProc   : IntPtr ;
      GDALGetBlockSizeProc         : IntPtr ;
      GDALGetColorEntryProc        : IntPtr ;
      GDALSetColorEntryProc        : IntPtr ;
      GDALGetColorEntryCountProc   : IntPtr ;
      GDALGetColorIntNameProc      : IntPtr ;
      GDALGetDataTypeSizeProc      : IntPtr ;
      GDALGetDatasetDriverProc     : IntPtr ;
      GDALGetDriverByNameProc      : IntPtr ;
      GDALGetDriverProc            : IntPtr ;
      GDALGetDriverCountProc       : IntPtr ;
      GDALGetGCPProjectionProc     : IntPtr ;
      GDALGetGeoTransformProc      : IntPtr ;
      GDALSetGeoTransformProc      : IntPtr ;
      GDALGetMetadataProc          : IntPtr ;
      GDALGetMetadataItemProc      : IntPtr ;
      GDALGetProjectionRefProc     : IntPtr ;
      GDALSetProjectionProc        : IntPtr ;
      GDALGetRasterBandProc        : IntPtr ;
      GDALGetRasterBandXSizeProc   : IntPtr ;
      GDALGetRasterBandYSizeProc   : IntPtr ;
      GDALGetRasterColorIntProc    : IntPtr ;
      GDALGetRasterColorTableProc  : IntPtr ;
      GDALCreateColorTableProc     : IntPtr ;
      GDALGetRasterCountProc       : IntPtr ;
      GDALGetRasterDataTypeProc    : IntPtr ;
      GDALGetDataTypeNameProc      : IntPtr ;
      GDALGetRasterMaximumProc     : IntPtr ;
      GDALGetRasterMinimumProc     : IntPtr ;
      GDALGetRasterNoDataValueProc : IntPtr ;
      GDALSetRasterNoDataValueProc : IntPtr ;
      GDALGetRasterXSizeProc       : IntPtr ;
      GDALGetRasterYSizeProc       : IntPtr ;
      GDALRasterIOProc             : IntPtr ;
      CSLFetchBooleanProc          : IntPtr ;
      CSLCountProc                 : IntPtr ;
      CSLGetFieldProc              : IntPtr ;
      CPLGetLastErrorMsgProc       : IntPtr ;

    {$ELSE}
      { Register all known configured GDAL drivers.
      }
      GDALAllRegister : procedure ; stdcall ;

      // Create a new dataset with this driver.
      // _hDriver       driver handle
      // _pszFilename   the name of the dataset to create
      // _nXSize        width of created raster in pixels
      // _nYSize        height of created raster in pixels
      // _nBands        number of bands
      // _eBandType     type of raster
      // _papszParmList list of driver specific control parameters
      // return               NULL on failure, or a new GDALDataset
      GDALCreate     : function( const _hDriver        : GDALDriverH;
                                 const _pszFilename    : PAnsiChar;
                                 const _nXSize         : Integer;
                                 const _nYSize         : Integer;
                                 const _nBands         : Integer;
                                 const _eBandType      : GDALDataType;
                                 const _papszParmList  : IntPtr
                               ) : GDALDatasetH ; stdcall ;

      // Create a copy of a dataset.
      // _hDriver        driver handle.
      // _pszFilename   the name for the new dataset. UTF-8 encoded.
      // _poSrcDS       the dataset being duplicated.
      // _bStrict       TRUE if the copy must be strictly equivalent, or more
      //                        normally FALSE indicating that the copy may adapt as
      //                        needed for the output format.
      // _papszOptions  additional format dependent options controlling
      //                        creation of the output file.
      // _pfnProgress   a function to be used to report progress of the copy.
      // _pProgressData application data passed into progress function.
      GDALCreateCopy : function( const _hDriver        : GDALDriverH;
                                 const _pszFilename    : PAnsiChar;
                                 const _hSrcDS         : GDALDatasetH;
                                 const _bStrict        : Integer;
                                 const _papszOptions   : IntPtr ;
                                 const _pfnProgress    : T_GDALProgressFunc ;
                                 const _pProgressData  : IntPtr
                                ) : GDALDatasetH ; stdcall ;

      // Open a raster file as a GDALDataset.
      // _pszFilename the name of the file to access
      // _eAccess     the desired access, either GA_Update or GA_ReadOnly.
      //                      Many drivers support only read only access.
      // return             A GDALDatasetH handle or NULL on failure
     GDALOpen        : function( const _pszFilename  : PAnsiChar;
                                 const _eAccess      : GDALAccess
                               ) : GDALDatasetH ; stdcall ;

      // Get data type size in bits.
      // _eDataType   data type, such as GDT_Byte
      // return             the size of a a GDT_* type in bits, not bytes!
      GDALGetDataTypeSize     : function( const _eDataType : GDALDataType
                                        ) : Integer ; stdcall ;

      // Get metadata.
      GDALGetMetadata : function( const _hDataset   : GDALMajorObjectH;
                                  const _pszDomain  : PAnsiChar
                                ) : PPAnsiChar ; stdcall ;

      // Get metadata item.
      GDALGetMetadataItem : function( const _hObject    : GDALMajorObjectH;
                                      const _pszName    : PAnsiChar ;
                                      const _pszDomain  : PAnsiChar
                                    ) : PAnsiChar ; stdcall ;

      // Get driver by name.
      // _name driver name
      // return       driver handle
      GDALGetDriverByName : function( const _name : PAnsiChar
                                    ) : GDALDriverH ; stdcall ;

      // Get dataset driver.
      // _hDataset dataset handle
      // return           driver handle
      GDALGetDatasetDriver : function( const _hDataset  : GDALDatasetH
                                      ) : GDALDriverH ; stdcall ;

      // Get driver count.
      // return driver count
      GDALGetDriverCount : function : Integer ; stdcall ;

      // Get driver.
      // _iDriver driver id
      // return          driver handle
      GDALGetDriver     : function( const _index : Integer
                                  ) : GDALDriverH ; stdcall ;

      // Close GDAL dataset.
      // _hDataset   The dataset to close
      GDALClose : procedure( const _hDataset : GDALDatasetH
                           ) ; stdcall ;

      // Get raster X size.
      // _hDataset   The dataset handle
      // return      X size
      GDALGetRasterXSize  : function( const _hDataset : GDALDatasetH
                                    ) : Integer ; stdcall ;

      // Get raster Y size.
      // _hDataset   The dataset handle
      // return       Y size
      GDALGetRasterYSize  : function( const _hDataset : GDALDatasetH
                                    ) : Integer ; stdcall ;

      // Get raster count.
      // _hDataset   The dataset handle
      // return      raster count
      GDALGetRasterCount  : function( const _hDataset : GDALDatasetH
                                    ) : Integer ; stdcall ;

      // Get raster band.
      // _hDataset   The dataset to close
      // _nBandId    band id
      // return      raster band handle
      GDALGetRasterBand   : function( const _hDataset : GDALDatasetH;
                                      const _nBandId  : Integer
                                    ) : GDALRasterBandH ; stdcall ;

      // Get projection.
      // _hDataset  dataset handle
      // return     projection string
      GDALGetProjectionRef  : function( const _hDataset : GDALDatasetH
                                      ) : PAnsiChar ; stdcall ;

      // Set projection.
      // _hDataset      dataset handle
      // _pszProjection projection string
      // return         error
      GDALSetProjection     : function( const _hDataset      : GDALDatasetH;
                                        const _pszProjection : PAnsiChar
                                      ) : CPLErr ; stdcall ;

      // Get geotransform.
      // _hDataset      dataset handle
      // _padfTransform projection array
      // return                error
      GDALGetGeoTransform : function( const _hDataset       : GDALDatasetH;
                                      const _padfTransform  : IntPtr
                                    ) : CPLErr ; stdcall ;

      // Set geotransform.
      // _hDataset      dataset handle
      // _padfTransform projection array
      // return                error
      GDALSetGeoTransform : function( const _hDataset       : GDALDatasetH;
                                      const _padfTransform  : IntPtr
                                    ) : CPLErr ; stdcall ;

      // Get GC Projection.
      // _hDataset      dataset handle
      // return         count
      GDALGetGCPProjection : function( const _hDataset : GDALDatasetH
                                     ) : PAnsiChar ; stdcall ;

      // Get number of GCPs.
      // _hDataset      dataset handle
      // return         count
      GDALGetGCPCount : function( const _hDataset : GDALDatasetH
                                     ) : Integer ; stdcall ;

      // Fetch GCPs.
      // _hDataset      dataset handle
      // return         count
      GDALGetGCPs : function( const _hDataset : GDALDatasetH
                              ) : IntPtr ; stdcall ;

      // Generate Geotransform from GCPs.
      GDALGCPsToGeoTransform : function( const _nGCPCount     : Integer ;
                                         const _asGCPs        : IntPtr ;
                                         const _padfTransform : IntPtr ;
                                         const _bApproxOK     : Integer
                                        ) : Integer ; stdcall ;

      // Get Driver Short Name.
      // _hDriver    driver handle
      // return      driver name
      GDALGetDriverShortName : function( const _hDriver : GDALDriverH
                                       ) : PAnsiChar ; stdcall ;

      // Get Driver long Name.
      // _hDriver    driver handle
      // return      driver name
      GDALGetDriverLongName : function( const _hDriver : GDALDriverH
                                      ) : PAnsiChar ; stdcall ;

      // Get raster data type.
      // _hBand raster band
      // return data type
      GDALGetRasterDataType : function( const _hBand : GDALRasterBandH
                                      ) : GDALDataType ; stdcall ;

      // Get name of data type.
      // _eDataType   data type, such as GDT_Byte
      // return       string corresponding to type
      GDALGetDataTypeName   : function( const _eDataType : GDALDATATYPE
                                      ) : PAnsiChar ; stdcall ;

      // Get block size.
      // _hBand    raster band
      // _pnXSize  X Size
      // _pnYSize  Y Size
      GDALGetBlockSize     : procedure( const _hBand      : GDALRasterBandH;
                                        const _pnXSize    : PInteger;
                                        const _pnYSize    : PInteger
                                      ) ; stdcall ;

      // Get raster band X size.
      // _hBand  raster band
      // return  X size
      GDALGetRasterBandXSize : function( const _hBand : GDALRasterBandH
                                       ) : Integer ; stdcall ;

      // Get raster band Y size.
      // _hBand raster band
      // return        Y size
      GDALGetRasterBandYSize : function( const _hBand : GDALRasterBandH
                                         ) : Integer ; stdcall ;

      // Get Raster Color Interpretation.
      // _hBand raster band
      // return        Color Interpretation
      GDALGetRasterColorInterpretation : function( const _hBand : GDALRasterBandH
                                                 ) : GDALColorInterp ; stdcall ;

      // Get Raster Color Table.
      // _hBand raster band
      // return        Color Table
      GDALGetRasterColorTable : function( const _hBand : GDALRasterBandH
                                        ) : GDALColorTableH ; stdcall ;

      // Get raster minimum.
      // _hBand        raster band
      // _pbSuccess    success
      // return        minimum
      GDALGetRasterMinimum : function( const _hBand     : GDALRasterBandH;
                                       const _pbSuccess : PInteger
                                     ) : Double ; stdcall ;

      // Get raster maximum.
      // _hBand        raster band
      // _pbSuccess    success
      // return        maximum
      GDALGetRasterMaximum : function( const _hBand     : GDALRasterBandH;
                                       const _pbSuccess : PInteger
                                     ) : Double ; stdcall ;

      // Compute the min/max values for a band.
      // _hBand       the band to copmute the range for
      // _bApproxOK   TRUE if an approximate (faster) answer is OK,
      //                      otherwise FALSE
      // _adfMinMax   the array in which the minimum (adfMinMax[0])
      //                      and the maximum (adfMinMax[1]) are returned.
      GDALComputeRasterMinMax : procedure( const _hBand     : GDALRasterBandH;
                                           const _bApproxOK : Integer;
                                           const _adfMinMax : IntPtr
                                         ) ; stdcall ;

      // Read/write a region of image data for this band.
      // _eRWFlag             Either GF_Read to read a region of data,
      //                      or GF_Write to write a region of data.
      // _nXOff               The pixel offset to the top left corner of the region
      //                      of the band to be accessed. This would be zero to
      //                      start from the left side.
      // _nYOff               The line offset to the top left corner of the region
      //                      of the band to be accessed. This would be zero to
      //                      start from the top.
      // _nXSize              The width of the region of the band to be accessed in pixels.
      // _nYSize              The height of the region of the band to be accessed in lines.
      // _pData               The buffer into which the data should be read, or from
      //                      which it should be written. This buffer must contain
      //                      at least nBufXSize * nBufYSize words of type eBufType.
      //                      It is organized in left to right, top to bottom pixel
      //                      order. Spacing is controlled by the nPixelSpace, and
      //                      nLineSpace parameters.
      // _nBufXSize           the width of the buffer image into which the desired
      //                      region is to be read, or from which it is to be written.
      // _nBufYSize           the height of the buffer image into which the desired
      //                      region is to be read, or from which it is to be written.
      // _eBufType            the type of the pixel values in the pData data buffer.
      //                      The pixel values will automatically be translated
      //                      to/from the GDALRasterBand data type as needed.
      // _nPixelSpace         The byte offset from the start of one pixel value in
      //                      pData to the start of the next pixel value within a
      //                      scanline. If defaulted (0) the size of the datatype
      //                      eBufType is used.
      // _nLineSpace          The byte offset from the start of one scanline in pData
      //                      to the start of the next. If defaulted the size of the
      //                      datatype eBufType * nBufXSize is used.
      // return               CE_Failure if the access fails, otherwise CE_None.
      GDALRasterIO : function( const _hRBand      : GDALRasterBandH;
                               const _eRWFlag     : GDALRWFLAG;
                               const _nXOff       : Integer;
                               const _nYOff       : Integer;
                               const _nXSize      : Integer;
                               const _nYSize      : Integer;
                               const _pBuffer     : IntPtr;
                               const _nBufXSize   : Integer;
                               const _nBufYSize   : Integer;
                               const _eBufType    : GDALDataType;
                               const _nPixelSpace : Integer;
                               const _nLineSpace  : Integer
                              ) : CPLErr ; stdcall ;

      { Get Raster NoData Value.
      }
      GDALGetRasterNoDataValue : function( const _hRBand    : GDALRasterBandH ;
                                           const _pbSuccess : PInteger
                                         ) : Double ; stdcall ;

      { Set Raster NoData Value.
      }
      GDALSetRasterNoDataValue : function( const _hRBand    : GDALRasterBandH ;
                                           const _value     : Double
                                         ) : CPLErr ; stdcall ;

      { Get Color Entry Count.
      }
      GDALGetColorEntryCount : function( const _hColorTable : GDALColorTableH
                                       ) : Integer ; stdcall ;

      { Get Color Entry.
      }
      GDALGetColorEntry : function( const _hColorTable : GDALColorTableH;
                                    const _nTabIndex   : Integer
                                  ) : PGDALCOLORENTRY ; stdcall ;

      { Set Color Entry.
      }
      GDALSetColorEntry : procedure( const _hColorTable : GDALColorTableH;
                                     const _nTabIndex   : Integer ;
                                     const _entry       : PGDALCOLORENTRY
                                    ) ; stdcall ;

      { Create color table.
      }
      GDALCreateColorTable : function( const _eColorInterp : GDALColorInterp
                                      ) : GDALColorTableH ; cdecl ;

      // Translate a GDALColorInterp into a user displayable string.
      // _eColorInterp Types of color interpretation for raster bands
      // return              displayable string of color interpretation
      GDALGetColorInterpretationName  : function( const _ePaletteInterp : GDALPaletteInterp
                                                ) : PAnsiChar ; cdecl ;

      { Fetch boolean value.
      }
      CSLFetchBoolean   : function( const _papszStrList : IntPtr ;
                                    const _pszKey       : PAnsiChar ;
                                    const _bDefault     : Integer
                                  ) : Integer ; cdecl ;
      { Count list elements.
      }
      CSLCount          : function( const _papszStrList : IntPtr
                                  ) : Integer ; cdecl ;

      { Get list element.
      }
      CSLGetField          : function( const _papszStrList : IntPtr ;
                                       const _iField       : Integer
                                     ) : PAnsiChar ; cdecl ;
      { Get last error message.
      }
      CPLGetLastErrorMsg   : function : PAnsiChar ; stdcall ;

      { Set Config option.
      }
      CPLSetConfigOption   : procedure( const _pszKey : PAnsiChar ;
                                        const _pszVal : PAnsiChar
                                       ) ; stdcall ;

      { Add string to string list.
      }
      CSLAddString         : function( _papszStrList : IntPtr ;
                                       const _pszNewString : PAnsiChar
                                      ) : IntPtr ; cdecl ;

      CSLDestroy           : procedure( _papszStrList : IntPtr ) ; stdcall ;
    {$ENDIF}

    {$IFDEF CLR}
      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class procedure GDALAllRegister ;  external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALOpen( const _pszFilename  : TBytes;
                          const _eAccess      : GDALAccess
                         ) : GDALDatasetH ;  external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALCreate( const _hDriver        : GDALDriverH;
                            const _pszFilename    : String;
                            const _nXSize         : Integer;
                            const _nYSize         : Integer;
                            const _nBands         : Integer;
                            const _eBandType      : GDALDataType;
                            const _papszParmList  : IntPtr
                            ) : GDALDatasetH ;  external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
     class function GDALCreateCopy ( const _hDriver        : GDALDriverH;
                                 const _pszFilename    : String;
                                 const _hSrcDS         : GDALDatasetH;
                                 const _bStrict        : Integer;
                                 const _papszOptions   : IntPtr ;
                                 const _pfnProgress    : T_GDALProgressFunc ;
                                 const _pProgressData  : IntPtr
                                ) : GDALDatasetH ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetDataTypeSize( const _eDataType : GDALDataType
                                         ) : Integer ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetMetadata( const _hDataset   : GDALMajorObjectH;
                                 const _pszDomain  : String
                                ) : IntPtr ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetMetadataItem( const _hObject    : GDALMajorObjectH;
                                     const _pszName    : String ;
                                     const _pszDomain  : String
                                    ) : IntPtr ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetDatasetDriver( const _hDataset  : GDALDatasetH
                                      ) : GDALDriverH ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetDriverByName( const _name  : String
                                      ) : GDALDriverH ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetDriver( const _index : Integer
                               ) : GDALDriverH ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetDriverCount : Integer ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class procedure GDALClose( const _hDataset  : GDALDatasetH
                                      ) ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetRasterXSize( const _hDataset : GDALDatasetH
                                     ) : Integer ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetRasterYSize( const _hDataset : GDALDatasetH
                                     ) : Integer ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetRasterCount( const _hDataset : GDALDatasetH
                                     ) : Integer ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetRasterBand( const _hDataset : GDALDatasetH;
                                      const _nBandId  : Integer
                                     ) : GDALRasterBandH ;  external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetProjectionRef( const _hDataset : GDALDatasetH
                                       ) : IntPtr ;  external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALSetProjection( const _hDataset      : GDALDatasetH ;
                                  const _pszProjection : String
                                 ) : CPLErr ;  external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetGeoTransform( const _hDataset       : GDALDatasetH;
                                    const _padfTransform  : IntPtr
                                   ) : CPLErr ;   external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALSetGeoTransform( const _hDataset       : GDALDatasetH;
                                    const _padfTransform  : IntPtr
                                   ) : CPLErr ;   external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetGCPProjection( const _hDataset : GDALDatasetH
                                      ) : IntPtr ;   external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetGCPCount( const _hDataset : GDALDatasetH
                                      ) : Integer ;   external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetGCPs( const _hDataset : GDALDatasetH
                                      ) : IntPtr ;   external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGCPsToGeoTransform( const _nGCPCount     : Integer ;
                                       const _asGCPs        : IntPtr ;
                                       const _padfTransform : IntPtr ;
                                       const _bApproxOK     : Integer
                                     ) : Integer ;   external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetDriverShortName( const _hDriver : GDALDriverH
                                       )  : IntPtr ;   external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetDriverLongName( const _hDriver : GDALDriverH
                                       )  : IntPtr ;   external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetRasterDataType( const _hBand : GDALRasterBandH
                                       ) : GDALDataType ;  external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetDataTypeName( const _eDataType : GDALDataType
                                    ) : IntPtr ;  external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class procedure GDALGetBlockSize( const _hBand      : GDALRasterBandH;
                                    const _pnXSize   : IntPtr;
                                    const _pnYSize   : IntPtr
                                   )  ;  external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetRasterBandXSize( const _hBand : GDALRasterBandH
                                        ) : Integer ;  external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetRasterBandYSize( const _hBand : GDALRasterBandH
                                        ) : Integer ;  external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALCreateColorTable( const _eColorInterp : GDALColorInterp
                                      ) : GDALColorTableH ;  external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetRasterColorInterpretation( const _hBand : GDALRasterBandH
                                                  ) : GDALColorInterp ;  external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetRasterColorTable( const _hBand : GDALRasterBandH
                                         ) : GDALColorTableH ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetRasterMaximum( const _hBand     : GDALRasterBandH;
                                       const _pbSuccess : Integer
                                      ) : Double ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetRasterMinimum( const _hBand     : GDALRasterBandH;
                                       const _pbSuccess : Integer
                                      ) : Double ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
     class  procedure GDALComputeRasterMinMax( const _hBand     : GDALRasterBandH;
                                           const _bApproxOK : Integer;
                                           const _adfMinMax : IntPtr
                                          ) ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALRasterIO( const _hRBand      : GDALRasterBandH;
                               const _eRWFlag     : GDALRWFlag;
                               const _nXOff       : Integer;
                               const _nYOff       : Integer;
                               const _nXSize      : Integer;
                               const _nYSize      : Integer;
                               const _pBuffer     : IntPtr;
                               const _nBufXSize   : Integer;
                               const _nBufYSize   : Integer;
                               const _eBufType    : GDALDataType;
                               const _nPixelSpace : Integer;
                               const _nLineSpace  : Integer
                              ) : CPLErr ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetRasterNoDataValue( const _hRBand    : GDALRasterBandH ;
                                         const _pbSuccess : Integer
                                        ) : Double ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALSetRasterNoDataValue( const _hRBand    : GDALRasterBandH ;
                                         const _pvalue    : Double
                                        ) : CPLErr ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetColorEntryCount( const _hColorTable : GDALColorTableH
                                        ) : Integer ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class procedure GDALSetColorEntry(  const _hColorTable : GDALColorTableH;
                                    const _nTabIndex   : Integer ;
                                    const _entry       : PGDALCOLORENTRY
                                   ) ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetColorEntry(  const _hColorTable : GDALColorTableH;
                                     const _nTabIndex   : Integer
                                   ) : PGDALCOLORENTRY ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function GDALGetColorInterpretationName ( const _eColorInterp : GDALColorInterp
                                      ) : IntPtr ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function CSLFetchBoolean( const _papszStrList  : IntPtr ;
                                  const _pszKey       : String ;
                                  const _bDefault     : Integer
                                ) : Integer ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function CSLCount( const _papszStrList : IntPtr
                            ) : Integer ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function CSLGetField( const _papszStrList : IntPtr ;
                             const _iField       : Integer
                            ) : IntPtr ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function CPLGetLastErrorMsg : IntPtr ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class procedure CPLSetConfigOption( const _pszKey : String ;
                                        const _pszVal : String
                                       ) ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                   CharSet = CharSet.Ansi
                  )
      ]
      class function CSLAddString ( _papszStrList : IntPtr ;
                                       const _pszNewString : String
                                      ) : IntPtr ; external ;

      [ SuppressUnmanagedCodeSecurity,
        DllImport( GDAL_DEFAULT_DLL_NAME, CallingConvention = CallingConvention.StdCall,
                   CharSet = CharSet.Ansi
                  )
      ]
      class procedure CSLDestroy  ( _papszStrList : IntPtr ) ; external ;

    {$ENDIF}

    {$IFDEF JAVA}
       class procedure GDALAllRegister ;
       class function GDALOpen( const _pszFilename  : TBytes;
                          const _eAccess      : GDALAccess
                         ) : GDALDatasetH ;
       class function GDALCreate( const _hDriver        : GDALDriverH;
                            const _pszFilename    : String;
                            const _nXSize         : Integer;
                            const _nYSize         : Integer;
                            const _nBands         : Integer;
                            const _eBandType      : GDALDataType;
                            const _papszParmList  : com.sun.jna.Pointer
                            ) : GDALDatasetH ;
      class function GDALCreateCopy ( const _hDriver        : GDALDriverH;
                                 const _pszFilename    : String;
                                 const _hSrcDS         : GDALDatasetH;
                                 const _bStrict        : Integer;
                                 const _papszOptions   : com.sun.jna.Pointer ;
                                 const _pfnProgress    : T_GDALProgressFunc ;
                                 const _pProgressData  : IntPtr
                                ) : GDALDatasetH ;
       class function GDALGetDataTypeSize( const _eDataType : GDALDataType
                                         ) : Integer ;
       class function GDALGetMetadata( const _hDataset   : GDALMajorObjectH;
                                 const _pszDomain  : String
                                ) : IntPtr ;
       class function GDALGetMetadataItem( const _hObject    : GDALMajorObjectH;
                                     const _pszName    : String ;
                                     const _pszDomain  : String
                                    ) : String ;
       class function GDALGetDatasetDriver( const _hDataset  : GDALDatasetH
                                      ) : GDALDriverH ;
       class function GDALGetDriverByName( const _name  : String
                                      ) : GDALDriverH ;
       class function GDALGetDriver( const _index : Integer
                               ) : GDALDriverH ;
       class function GDALGetDriverCount : Integer ;
       class procedure GDALClose( const _hDataset  : GDALDatasetH
                                      ) ;
       class function GDALGetRasterXSize( const _hDataset : GDALDatasetH
                                     ) : Integer ;
       class function GDALGetRasterYSize( const _hDataset : GDALDatasetH
                                     ) : Integer ;
       class function GDALGetRasterCount( const _hDataset : GDALDatasetH
                                     ) : Integer ;
       class function GDALGetRasterBand( const _hDataset : GDALDatasetH;
                                      const _nBandId  : Integer
                                     ) : GDALRasterBandH ;
       class function GDALGetProjectionRef( const _hDataset : GDALDatasetH
                                       ) : String ;
       class function GDALSetProjection( const _hDataset      : GDALDatasetH ;
                                  const _pszProjection : String
                                 ) : CPLErr ;
       class function GDALGetGeoTransform( const _hDataset       : GDALDatasetH;
                                    const _padfTransform  : com.sun.jna.Pointer
                                   ) : CPLErr ;
       class function GDALSetGeoTransform( const _hDataset       : GDALDatasetH;
                                    const _padfTransform  : com.sun.jna.Pointer
                                   ) : CPLErr ;
       class function GDALGetGCPProjection( const _hDataset : GDALDatasetH
                                      ) : String ;
       class function GDALGetGCPCount( const _hDataset : GDALDatasetH
                                      ) : Integer ;
       class function GDALGetGCPs( const _hDataset : GDALDatasetH
                                      ) : com.sun.jna.Pointer ;
       class function GDALGCPsToGeoTransform( const _nGCPCount     : Integer ;
                                       const _asGCPs        : com.sun.jna.Pointer ;
                                       const _padfTransform : com.sun.jna.Pointer ;
                                       const _bApproxOK     : Integer
                                     ) : Integer ;
       class function GDALGetDriverShortName( const _hDriver : GDALDriverH
                                       )  : String ;
       class function GDALGetDriverLongName( const _hDriver : GDALDriverH
                                       )  : String ;
       class function GDALGetRasterDataType( const _hBand : GDALRasterBandH
                                       ) : GDALDataType ;
       class function GDALGetDataTypeName( const _eDataType : GDALDataType
                                    ) : String ;
       class procedure GDALGetBlockSize( const _hBand      : GDALRasterBandH;
                                    const _pnXSize   : com.sun.jna.Pointer;
                                    const _pnYSize   : com.sun.jna.Pointer
                                   )  ;
       class function GDALGetRasterBandXSize( const _hBand : GDALRasterBandH
                                        ) : Integer ;
       class function GDALGetRasterBandYSize( const _hBand : GDALRasterBandH
                                        ) : Integer ;
       class function GDALCreateColorTable( const _eColorInterp : GDALColorInterp
                                      ) : GDALColorTableH ;
       class function GDALGetRasterColorInterpretation( const _hBand : GDALRasterBandH
                                                  ) : GDALColorInterp ;
       class function GDALGetRasterColorTable( const _hBand : GDALRasterBandH
                                         ) : GDALColorTableH ;
       class function GDALGetRasterMaximum( const _hBand     : GDALRasterBandH;
                                       const _pbSuccess : Integer
                                      ) : Double ;
       class function GDALGetRasterMinimum( const _hBand     : GDALRasterBandH;
                                       const _pbSuccess : Integer
                                      ) : Double ;
       class procedure GDALComputeRasterMinMax( const _hBand     : GDALRasterBandH;
                                           const _bApproxOK : Integer;
                                           const _adfMinMax : com.sun.jna.Pointer
                                          ) ;
       class function GDALRasterIO( const _hRBand      : GDALRasterBandH;
                               const _eRWFlag     : GDALRWFlag;
                               const _nXOff       : Integer;
                               const _nYOff       : Integer;
                               const _nXSize      : Integer;
                               const _nYSize      : Integer;
                               const _pBuffer     : com.sun.jna.Pointer;
                               const _nBufXSize   : Integer;
                               const _nBufYSize   : Integer;
                               const _eBufType    : GDALDataType;
                               const _nPixelSpace : Integer;
                               const _nLineSpace  : Integer
                              ) : CPLErr ;
       class function GDALGetRasterNoDataValue( const _hRBand    : GDALRasterBandH ;
                                         const _pbSuccess : Integer
                                        ) : Double ;
       class function GDALSetRasterNoDataValue( const _hRBand    : GDALRasterBandH ;
                                         const _pvalue    : Double
                                        ) : CPLErr ;
       class function GDALGetColorEntryCount( const _hColorTable : GDALColorTableH
                                        ) : Integer ;
       class procedure GDALSetColorEntry(  const _hColorTable : GDALColorTableH;
                                    const _nTabIndex   : Integer ;
                                    const _entry       : PGDALCOLORENTRY
                                   ) ;
       class function GDALGetColorEntry(  const _hColorTable : GDALColorTableH;
                                     const _nTabIndex   : Integer
                                   ) : PGDALCOLORENTRY ;
       class function GDALGetColorInterpretationName ( const _eColorInterp : GDALColorInterp
                                      ) : String ;
       class function CSLFetchBoolean( const _papszStrList  : IntPtr ;
                                  const _pszKey       : String ;
                                  const _bDefault     : Integer
                                ) : Integer ;
       class function CSLCount( const _papszStrList : IntPtr
                            ) : Integer ;
       class function CSLGetField( const _papszStrList : IntPtr ;
                             const _iField       : Integer
                            ) : String ;
       class function CPLGetLastErrorMsg : String ;
       class procedure CPLSetConfigOption( const _pszKey : String ;
                                        const _pszVal : String
                                       ) ;
       class function CSLAddString ( _papszStrList : com.sun.jna.Pointer ;
                                       const _pszNewString : String
                                      ) : com.sun.jna.Pointer ;
       class procedure CSLDestroy  ( _papszStrList : com.sun.jna.Pointer ) ;
    {$ENDIF}

    public
      // Constructor.
      constructor Create ;
      {$IFNDEF OXYGENE}
      // Destructor.
      destructor Destroy ; override;
      {$ENDIF}

      // Load a library.
      // _dllPath   path to dll file
      // return     True if library was loaded successfully
      function  LoadDLL     ( const _dllPath : String
                            ) : Boolean ;
  end ;

//==============================================================================
// GDAL types
//==============================================================================

  { GDAL object helper.
  }
  T_GDALObject = class( TGIS_ObjectDisposable )
    private
      {$IFDEF OXYGENE}
       FHandle   : HandleRef ;
      {$ELSE}
       FHandle   : IntPtr  ;
      {$ENDIF}
      FLib      : T_GDALLib ;
      FMetadata : TStringList ;
    protected

      // Read object properties.
      procedure readProperties ; virtual;

      function  fget_IsValid   : Boolean ;
      function  fget_IsLibrary : Boolean ;
      function  fget_Metadata  : String ;
      function  fget_Error     : String ;
      {$IFDEF JAVA}
      function  fget_Handle    : com.sun.jna.Pointer ;
      procedure fset_Handle( const _handle : com.sun.jna.Pointer ) ;
      {$ELSE}
      function  fget_Handle    : IntPtr ;
      procedure fset_Handle( const _handle : IntPtr ) ;
      {$ENDIF}
    protected
      // Destructor.
      procedure doDestroy ; override;
    public

      // Constructor.
      // _library   library handle
      constructor Create( const _library : T_GDALLib
                        ) ;

      // Get a description.
      // return   info text
      function GetInfo : String ; virtual;
    public
      {$IFDEF JAVA}
      property Handle : com.sun.jna.Pointer read  fget_Handle
                                            write fset_Handle ;
      {$ELSE}
      property Handle     : IntPtr       read  fget_Handle
                                         write fset_Handle ;
      {$ENDIF}
      property Lib        : T_GDALLib    read  FLib ;
      property IsValid    : Boolean      read  fget_IsValid ;
      property IsLibrary  : Boolean      read  fget_IsLibrary ;
      property Metadata   : String       read  fget_Metadata ;
      property LastError  : String       read  fget_Error ;
  end ;

  T_GDALDataset = class ;

  { GDAL driver helper.
  }
  T_GDALDriver = class( T_GDALObject )
    private
      FSName          : String ;
      FLName          : String ;
      FIsReadOnly     : Boolean ;
      FCanCreate      : Boolean ;
      FCanCreateCopy  : Boolean ;
      FExtension      : String ;
    protected

      // Read a driver properties.
      procedure readProperties ; override;
    public

      // Create a driver from a dataset.
      // _dataset   dataset handle
      // _library   library handle
      constructor Create( const _dataset : T_GDALDataset ;
                          const _library : T_GDALLib
                        ) ; overload;

      // Create a driver from a handle.
      // _driver    driver handle
      // _library   library handle
      constructor Create( const _driver  : GDALDriverH ;
                          const _library : T_GDALLib
                        ) ; overload;

      //  Create a driver by a name.
      //  _name      driver name
      // _library    library handle
      constructor Create( const _name    : String ;
                          const _library : T_GDALLib
                        ) ; overload;

      // Create a dataset in the current driver format.
      // _filename  filename to create
      // _width     the width of the image
      // _height    the height of the image
      // _bands     the number of bands in the image
      // _datatype  the data type of the image
      // _options   construction options.
      // return     new dataset or nil if failed
      function CreateDataset( const _filename : String ;
                              const _width    : Integer ;
                              const _height   : Integer ;
                              const _bands    : Integer ;
                              const _datatype : GDALDataType ;
                              const _options  : TStrings
                             ) : T_GDALDataset ;

      // Get a description.
      // return driver info
      function GetInfo : String ; override;

      // Get a driver.
      // _index   driver index
      // return   driver object
      function GetDriver( const _index : Integer ) : T_GDALDriver ;

      // Get driver count.
      // return number of registered drivers
      function GetDriverCount : Integer ;
    public
      property ShortName      : String   read FSName ;
      property LongName       : String   read FLName ;
      property IsReadOnly     : Boolean  read FIsReadOnly ;
      property CanCreate      : Boolean  read FCanCreate   ;
      property CanCreateCopy  : Boolean  read FCanCreateCopy ;
      property Extension      : String   read FExtension ;
    end ;

  { GDAL raster band helper.
  }
  T_GDALRasterBand = class( T_GDALObject )
     private
       FId               : Integer ;
       FXSize            : Integer ;
       FYSize            : Integer ;
       FBlockXSize       : Integer ;
       FBlockYSize       : Integer ;
       FDataType         : GDALDataType ;
       FDataTypeName     : String ;
       FDataTypeSize     : Integer ;
       FColorInt         : GDALColorInterp ;
       FColorIntName     : String ;
       FMin              : Double ;
       FMax              : Double ;
       FColorTable       : GDALColorTableH ;
       FColorTabEntryCnt : Integer ;
       FNoDataValue      : Single ;
    protected
      // Read band properties.
      procedure readProperties ; override;
     public

      // Constructor.
      // _dataset   dataset
      // _bandId    id of band
      constructor Create( const _dataset  : T_GDALDataset ;
                          const _bandId   : Integer
                         ) ;

      // Get color entry from color table.
      // _nTabIndex     index in table
      // _nColorIndex   index of color entry
      function GetColorEntry( const _tabIndex   : Integer ;
                              const _colorIndex : Integer
                             ) : Byte ;

      // Read or write raster band data.
      // _flag        set read or write
      // _xOff        the x coordinate of the top left corner of the rectangular
      //              region of the image to be read/written.
      // _yOff        the y coordinate of the top left corner of the rectangular
      //              region of the image.
      // _xSize       the width of the rectangular region of the image.
      // _ySize       the height of the rectangular region of the image.
      // _buffer      pointer to an in memory buffer.
      // _bufXSize    the width of the buffer in pixels.
      // _bufYSize    the height of the buffer in pixels.
      // _dataType    the type of the data within the buffer.
      procedure RasterIO    ( const _flag       : GDALRWFlag ;
                              const _xOff       : Integer ;
                              const _yOff       : Integer ;
                              const _xSize      : Integer ;
                              const _ySize      : Integer ;
                              {$IFDEF JAVA}
                              const _buffer     : com.sun.jna.Pointer ;
                              {$ELSE}
                              const _buffer     : IntPtr ;
                              {$ENDIF}
                              const _bufXSize   : Integer ;
                              const _bufYSize   : Integer ;
                              const _dataType   : GDALDataType
                             ) ;

      // Get a description.
      // return   info text
      function GetInfo : String ; override;

      // Set color table.
      // _paletteEntry   palette array
//      procedure SetColorTable( {$IFDEF CLR}
//                                const _paletteEntry : array of System.Drawing.Color
//                               {$ENDIF}
//                               {$IFDEF JAVA}
//                                const _paletteEntry : array of java.awt.Color
//                               {$ENDIF}
//                               {$IFDEF DCC}
//                                const _paletteEntry : array of TPaletteEntry
//                               {$ENDIF}
//                              ) ;
     public
      property Id                   : Integer           read FId ;
      property XSize                : Integer           read FXSize ;
      property YSize                : Integer           read FYSize ;
      property BlockXSize           : Integer           read FBlockXSize ;
      property BlockYSize           : Integer           read FBlockYSize ;
      property ColorInt             : GDALColorInterp   read FColorInt ;
      property ColorIntName         : String            read FColorIntName ;
      property DataType             : GDALDataType      read FDataType ;
      property DataTypeName         : String            read FDataTypeName ;
      property DataTypeSize         : Integer           read FDataTypeSize ;
      property MinValue             : Double            read FMin ;
      property MaxValue             : Double            read FMax ;
      property ColorTableCount      : Integer           read FColorTabEntryCnt ;
      property NoDataValue          : Single            read FNoDataValue ;
  end ;

  { GDAL dataset helper.
  }
  T_GDALDataset =  class( T_GDALObject )
    private
      FXSize            : Integer ;
      FYSize            : Integer ;
      FBands            : TGIS_ObjectList ;
      FBandCount        : Integer ;
      FProjectionRef    : String  ;
      FProjectionGCRef  : String  ;
      FOrigin           : TGIS_Point ;
      FPixelSize        : TGIS_Point ;
      FScaleX           : Double ;
      FScaleY           : Double ;
      FRotateX          : Double ;
      FRotateY          : Double ;
      FGeoTransform     : array[ 0..5 ] of Double;
      FGCPGeoTransform  : array[ 0..5 ] of Double;
      FHasGCP           : Boolean ;
      FAccessMode       : GDALAccess ;
      FFileName         : String ;
      FDriver           : T_GDALDriver ;
    protected

      //Read a dataset properties.
      procedure readProperties ; override;
    protected

      // Destructor.
      procedure doDestroy ; override;
    public

      // Constructor.
      // _filename    file name
      // _access      access type
      // _library     library handle
      constructor Create( const _filename  : String ;
                          const _access    : GDALAccess ;
                          const _library   : T_GDALLib
                        ) ; overload;

      // Constructor.
      // _filename  file name
      // _driver  file name
      // _library library handle
      constructor Create( const _filename : String ;
                          const _driver    : T_GDALDriver ;
                          const _library   : T_GDALLib
                        ) ; overload;

      // Constructor.
      // _handle  file name
      // _driver  file name
      // _library library handle
      constructor Create( {$IFDEF JAVA}
                          const _handle    :com.sun.jna.Pointer ;
                          {$ELSE}
                          const _handle    : IntPtr ;
                          {$ENDIF}
                          const _driver    : T_GDALDriver ;
                          const _library   : T_GDALLib
                        ) ; overload;

      // Get a raster band.
      // _bandId    id of band
      // return     band
      function  GetRasterBand( const _bandId : Integer
                             ) : T_GDALRasterBand ;

      // Open a dataset.
      // return  True if dataset was opened successfully
      function  Open : Boolean ;

      // Build a dataset.
      // return True if dataset was built successfully
      function  Build( const _width  : Integer ;
                       const _height : Integer ;
                       const _extent : TGIS_Extent ;
                       const _cs     : TGIS_CSCoordinateSystem
                      ) : Boolean ;

      // Get a description.
      // return info text
      function  GetInfo : String ; override;
    public
      property XSize        : Integer         read FXSize ;
      property YSize        : Integer         read FYSize ;
      property BandCount    : Integer         read FBandCount ;
      property Projection   : String          read FProjectionRef ;
      property ProjectionGC : String          read FProjectionGCRef ;
      property Driver       : T_GDALDriver read FDriver ;
      property Origin       : TGIS_Point      read FOrigin ;
      property PixelSize    : TGIS_Point      read FPixelSize ;
      property ScaleX       : Double          read FScaleX ;
      property ScaleY       : Double          read FScaleY ;
      property RotateX      : Double          read FRotateX ;
      property RotateY      : Double          read FRotateY ;
  end ;

//==============================================================================
// helper functions
//==============================================================================

  { Convert string buffer to string }
 {$IFDEF CLR}
  function asString( const _buf : IntPtr ) : String ;
  begin
    Result := Marshal.PtrToStringAnsi( _buf ) ;
  end ;
 {$ENDIF}
 {$IFDEF JAVA}
  function asString( const _buf : String ) : String ;
  begin
    Result := _buf ;
  end ;
 {$ENDIF}
 {$IFDEF DCC}
  function asString( const _buf : PAnsiChar ) : String ;
  begin
    Result := String( AnsiString( _buf ) ) ;
  end ;
 {$ENDIF}

 { Convert string to string buffer. }
 {$IFDEF OXYGENE}
  function toAPI( const _str : String ) : String ;
  begin
    Result := _str ;
  end ;
  function toAPIEx( const _str : String ) : TBytes ;
  begin
    Result := TEncoding.UTF8.GetBytes( _str ) ;
  end ;
 {$ELSE}
  function toAPI( const _str : String ) : PAnsiChar ;
  begin
    Result := MarshaledAString(UTF8Encode(_str) ) ;
  end ;
  function toAPIEx( const _str : String ) : PAnsiChar ;
  begin
    Result := toAPI( _str ) ;
  end ;
 {$ENDIF}

  {$IFDEF JAVA}
  method T_GDALMapper.getFunctionName(&library: com.sun.jna.NativeLibrary; &method: &Method): String;
  begin
    if &method.getName().equals('GDALAllRegister') then begin
      exit '_GDALAllRegister@0' ;
    end ;

    exit inherited getFunctionName( &library, &method) ;
  end ;
  {$ENDIF}

//==============================================================================
// T_GDALObject
//==============================================================================

  constructor T_GDALObject.Create(
    const _library : T_GDALLib
  ) ;
  begin
    inherited Create ;

    {$IFDEF LEVEL_XE2_RTL}
      Handle  := IntPtr(0) ;
    {$ELSE}
      Handle  := nil ;
    {$ENDIF}
    FLib      := _library ;
    FMetadata := TStringList.Create ;
  end ;

  procedure T_GDALObject.doDestroy ;
  begin
    FreeObject( FMetadata ) ;

    {$IFDEF LEVEL_XE2_RTL}
      Handle := IntPtr(0) ;
    {$ELSE}
      Handle := nil ;
    {$ENDIF}
    FLib     := nil ;

    inherited ;
  end ;

  procedure T_GDALObject.readProperties ;
  var
    buf : IntPtr ;
    cnt : Integer ;
    i   : Integer ;
  begin
    FMetadata.Clear ;

    if IsLibrary then begin
      {$IFDEF LEVEL_XE2_RTL}
        buf := IntPtr( Lib.GDALGetMetadata( Handle, nil ) ) ;
      {$ELSE}
        buf := Lib.GDALGetMetadata( Handle, nil ) ;
      {$ENDIF}
      {$IFDEF LEVEL_XE2_RTL}
        if buf <> IntPtr(0)   then begin
      {$ELSE}
        if AssignedPtr( buf ) then begin
      {$ENDIF}
        cnt := FLib.CSLCount( buf ) ;
        for i := 0 to cnt-1 do
          FMetadata.Add( '  ' + asString( FLib.CSLGetField( buf, i ) ) ) ;
      end ;

      {$IFDEF LEVEL_XE2_RTL}
        buf := IntPtr( Lib.GDALGetMetadata( Handle, toAPI( 'IMAGE_STRUCTURE' ) ) ) ;
      {$ELSE}
        buf := Lib.GDALGetMetadata( Handle, toAPI( 'IMAGE_STRUCTURE' ) ) ;
      {$ENDIF}
      {$IFDEF LEVEL_XE2_RTL}
        if buf <> IntPtr(0)   then begin
      {$ELSE}
        if AssignedPtr( buf ) then begin
      {$ENDIF}
        cnt := FLib.CSLCount( buf ) ;
        if cnt > 0 then
          FMetadata.Add( ' Image Structure Metadata:') ;
        for i := 0 to cnt-1 do
          FMetadata.Add( '  ' + asString( FLib.CSLGetField( buf, i ) ) ) ;
      end ;

      {$IFDEF LEVEL_XE2_RTL}
        buf := IntPtr( Lib.GDALGetMetadata( Handle, toAPI( 'SUBDATASETS' ) ) ) ;
      {$ELSE}
        buf := Lib.GDALGetMetadata( Handle, toAPI( 'SUBDATASETS' ) ) ;
      {$ENDIF}
      {$IFDEF LEVEL_XE2_RTL}
        if buf <> IntPtr(0)   then begin
      {$ELSE}
        if AssignedPtr( buf ) then begin
      {$ENDIF}
        cnt := FLib.CSLCount( buf ) ;
        if cnt > 0 then
          FMetadata.Add( ' Subdatasets:') ;
        for i := 0 to cnt-1 do
          FMetadata.Add( '  ' + asString( FLib.CSLGetField( buf, i ) ) ) ;
      end ;
    end ;
  end ;

  function T_GDALObject.fget_Error : String ;
  begin
    if IsLibrary then
      Result := asString( FLib.CPLGetLastErrorMsg )
    else
      Result := '' ;
  end ;

  function T_GDALObject.fget_IsValid : Boolean ;
  begin
    {$IFDEF CLR}
      Result := Handle <> IntPtr.Zero ;
    {$ENDIF}
    {$IFDEF JAVA}
      Result := Handle <> nil ;
    {$ENDIF}
    {$IFDEF DCC}
      {$IFDEF LEVEL_XE2_RTL}
        Result := FHandle <> IntPtr(0)   ;
      {$ELSE}
        Result := AssignedPtr( FHandle ) ;
      {$ENDIF}
    {$ENDIF}
  end ;

  function T_GDALObject.fget_IsLibrary : Boolean ;
  begin
    {$IFDEF OXYGENE}
      Result := True ;
    {$ELSE}
      Result := assigned( FLib ) ;
    {$ENDIF}
  end ;

  function T_GDALObject.fget_Metadata : String ;
  begin
    Result := FMetadata.Text ;
  end ;

  function T_GDALObject.GetInfo : String ;
  begin
    Result := '' ;
  end ;

  {$IFDEF JAVA}
    function  T_GDALObject.fget_Handle : com.sun.jna.Pointer ;
    begin
      Result := FHandle ;
    end ;

    procedure T_GDALObject.fset_Handle(
      const _handle : com.sun.jna.Pointer
    ) ;
    begin
      FHandle := _handle ;
    end ;
  {$ELSE}
    function  T_GDALObject.fget_Handle : IntPtr ;
    begin
      {$IFDEF OXYGENE}
        Result := FHandle.Handle ;
      {$ELSE}
        Result := FHandle ;
      {$ENDIF}
    end ;

    procedure T_GDALObject.fset_Handle(
      const _handle : IntPtr
    ) ;
    begin
      {$IFDEF OXYGENE}
        FHandle := HandleRef.Create( self, _handle ) ;
      {$ELSE}
        FHandle := _handle ;
      {$ENDIF}
    end ;
  {$ENDIF}

//==============================================================================
// T_GDALDataset
//==============================================================================

  constructor T_GDALDataset.Create(
    const _filename : String ;
    const _access   : GDALAccess ;
    const _library  : T_GDALLib
  ) ;
  begin
    inherited Create( _library ) ;

    FAccessMode := _access ;
    FFileName   := _filename ;
    FDriver     := nil ;
    FBands      := TGIS_ObjectList.Create( True ) ;
  end ;

  constructor T_GDALDataset.Create(
    const _filename : String ;
    const _driver   : T_GDALDriver ;
    const _library  : T_GDALLib
  ) ;
  begin
    inherited Create( _library ) ;

    FFileName := _filename ;
    FDriver   := _driver ;
    FBands    := TGIS_ObjectList.Create( True ) ;
  end ;

  constructor T_GDALDataset.Create(
    {$IFDEF JAVA}
    const _handle    :com.sun.jna.Pointer ;
    {$ELSE}
    const _handle    : IntPtr ;
    {$ENDIF}
    const _driver    : T_GDALDriver ;
    const _library   : T_GDALLib
  ) ;
  begin
    inherited Create( _library ) ;

    Handle  := _handle ;
    FDriver := _driver ;
    FBands  := TGIS_ObjectList.Create( True ) ;
  end ;

  procedure T_GDALDataset.doDestroy ;
  begin
    FreeObject( FBands ) ;

    if assigned( FDriver ) then
      FreeObject( FDriver ) ;

    if IsValid and IsLibrary then
      Lib.GDALClose( Handle ) ;

    inherited ;
  end ;

  procedure TGIS_FileGDAL.throwInitializeError ;
  begin
    {$IFDEF JAVA}
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEMAPPING ),
                                    FDLLPath, 0
                                )
    {$ELSE}
      try
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEMAPPING ),
                                       FDLLPath, 0
                                    )
      except
        on ex : Exception do begin
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERMISSED )
                                       + #13#10 + ex.Message,
                                       Path, 0
                                     ) ;
        end;
      end ;
    {$ENDIF}
  end ;

  procedure T_GDALDataset.readProperties ;
  var
  {$IFDEF CLR}
    buf : IntPtr ;
  {$ENDIF}
  {$IFDEF JAVA}
    buf : com.sun.jna.Pointer ;
  {$ENDIF}
    ngcp : Integer ;
    err  : CPLErr ;
  begin
    if IsValid and IsLibrary then begin
      FXSize           := Lib.GDALGetRasterXSize( Handle ) ;
      FYSize           := Lib.GDALGetRasterYSize( Handle ) ;
      FProjectionRef   := asString( Lib.GDALGetProjectionRef( Handle ) ) ;
      FProjectionGCRef := asString( Lib.GDALGetGCPProjection( Handle ) ) ;
      FBandCount       := Lib.GDALGetRasterCount( Handle ) ;

      if not assigned( FDriver ) then
        FDriver := T_GDALDriver.Create( self, Lib ) ;

      {$IFDEF OXYGENE}
        buf := Marshal.AllocHGlobal( length( FGeoTransform ) * sizeOf( Double ) ) ;
        Lib.GDALGetGeoTransform( Handle, buf ) ;
        {$IFDEF CLR}
        Marshal.Copy( buf, FGeoTransform, 0, length( FGeoTransform ) ) ;
        {$ELSE}
        FGeoTransform[0] := Marshal.ReadDouble( buf, 0  ) ;
        FGeoTransform[1] := Marshal.ReadDouble( buf, 8  ) ;
        FGeoTransform[2] := Marshal.ReadDouble( buf, 16 ) ;
        FGeoTransform[3] := Marshal.ReadDouble( buf, 24 ) ;
        FGeoTransform[4] := Marshal.ReadDouble( buf, 32 ) ;
        FGeoTransform[5] := Marshal.ReadDouble( buf, 40 ) ;
        {$ENDIF}
        Marshal.FreeHGlobal( buf ) ;
      {$ELSE}
        {$IFDEF LEVEL_XE2_RTL}
          err := Lib.GDALGetGeoTransform( Handle, IntPtr(@FGeoTransform[0]) ) ;
        {$ELSE}
          err := Lib.GDALGetGeoTransform( Handle, @FGeoTransform[0] ) ;
        {$ENDIF}
      {$ENDIF}
      {$IFDEF GIS_NORECORDS}
        FOrigin := new TGIS_Point ;
        FPixelSize := new TGIS_Point ;
      {$ENDIF}
      FOrigin.X    := FGeoTransform[ 0 ] ;
      FOrigin.Y    := FGeoTransform[ 3 ] ;
      FPixelSize.X := FGeoTransform[ 1 ] ;
      FPixelSize.Y := FGeoTransform[ 5 ] ;
      FScaleX      := FGeoTransform[ 1 ] ;
      FScaleY      := FGeoTransform[ 5 ] ;
      FRotateX     := FGeoTransform[ 2 ] ;
      FRotateY     := FGeoTransform[ 4 ] ;

      ngcp := Lib.GDALGetGCPCount( Handle ) ;
      FHasGCP := False ;
      if ngcp > 0 then
      {$IFDEF OXYGENE}
        begin
          buf := Marshal.AllocHGlobal( length( FGCPGeoTransform ) * sizeOf( Double ) ) ;
          FHasGCP := Lib.GDALGCPsToGeoTransform(
                           ngcp,
                           Lib.GDALGetGCPs( Handle ),
                           buf,
                           1
                         ) = 1 ;
          {$IFDEF CLR}
          Marshal.Copy( buf, FGCPGeoTransform, 0, length( FGCPGeoTransform ) ) ;
          {$ELSE}
          FGCPGeoTransform[0] := Marshal.ReadDouble( buf, 0  ) ;
          FGCPGeoTransform[1] := Marshal.ReadDouble( buf, 8  ) ;
          FGCPGeoTransform[2] := Marshal.ReadDouble( buf, 16 ) ;
          FGCPGeoTransform[3] := Marshal.ReadDouble( buf, 24 ) ;
          FGCPGeoTransform[4] := Marshal.ReadDouble( buf, 32 ) ;
          FGCPGeoTransform[5] := Marshal.ReadDouble( buf, 40 ) ;
          {$ENDIF}
          Marshal.FreeHGlobal( buf ) ;
        end ;
      {$ELSE}
        FHasGCP := Lib.GDALGCPsToGeoTransform(
                         ngcp,
                         Lib.GDALGetGCPs( Handle ),
                         {$IFDEF LEVEL_XE2_RTL}
                           IntPtr(@FGCPGeoTransform[0]),
                         {$ELSE}
                           @FGCPGeoTransform[0],
                         {$ENDIF}
                         1
                       ) = 1 ;
      {$ENDIF}

      if ( err <> CE_None ) and FHasGCP then begin
        FOrigin.X    := FGCPGeoTransform[ 0 ] ;
        FOrigin.Y    := FGCPGeoTransform[ 3 ] ;
        FPixelSize.X := FGCPGeoTransform[ 1 ] ;
        FPixelSize.Y := FGCPGeoTransform[ 5 ] ;
        FScaleX      := FGCPGeoTransform[ 1 ] ;
        FScaleY      := FGCPGeoTransform[ 5 ] ;
        FRotateX     := FGCPGeoTransform[ 2 ] ;
        FRotateY     := FGCPGeoTransform[ 4 ] ;
      end ;

      inherited ;
    end ;
  end ;

  function T_GDALDataset.GetInfo : String ;
  var
    i   : Integer ;
    sb  : TStringList ;
  begin
    sb := TStringList.Create ;
    try
      sb.Add( Format( 'Driver: %s', [ FDriver.GetInfo ] ) ) ;
      sb.Add( Format( 'Size: %dx%d', [ FXSize, FYSize ] ) ) ;
      sb.Add( Format( 'Bands: %d', [ FBandCount ] ) ) ;
      sb.Add( Format( 'Origin: %.8f, %.8f', [ FOrigin.X, FOrigin.Y ] ) ) ;
      sb.Add( Format( 'Pixel Size: %.8f %.8f', [ FPixelSize.X, FPixelSize.Y ] ) ) ;

      if not IsStringEmpty( FProjectionRef ) then begin
        sb.Add( 'Projection:' ) ;
        sb.Add( FProjectionRef ) ;
      end ;
      if not IsStringEmpty( FProjectionGCRef ) then begin
        sb.Add( 'Projection GCP:' ) ;
        sb.Add( FProjectionGCRef ) ;
      end ;

      if not IsStringEmpty( Metadata ) then begin
        sb.Add( 'Metadata:' ) ;
        sb.Add( Metadata ) ;
        sb.Add('') ;
      end ;

      for i := 0 to FBandCount-1 do begin
        sb.Add( T_GDALRasterBand( FBands[i] ).GetInfo ) ;
        sb.Add('') ;
      end ;

      Result := sb.Text ;
    finally
      FreeObject( sb ) ;
    end ;
  end ;

  function T_GDALDataset.GetRasterBand(
    const _bandId : Integer
  ) : T_GDALRasterBand ;
  var
    i : Integer ;
  begin
    Result := nil ;

    for i := 0 to FBands.Count-1 do begin
      if T_GDALRasterBand(FBands[i]).Id = _bandId then begin
        Result := T_GDALRasterBand( FBands[i] ) ;
        break ;
      end ;
    end;
  end ;

  function T_GDALDataset.Open : Boolean ;
  var
    i : Integer ;
  begin
    if IsLibrary then
      Handle := Lib.GDALOpen( toAPIEx( FFileName ), FAccessMode ) ;

    Result := IsValid ;
    if not Result then exit ;

    readProperties ;

    for i := 1 to FBandCount do
      FBands.Add( T_GDALRasterBand.Create( self, i ) ) ;
  end ;

  function T_GDALDataset.Build(
    const _width  : Integer ;
    const _height : Integer ;
    const _extent : TGIS_Extent ;
    const _cs     : TGIS_CSCoordinateSystem
   ) : Boolean ;
  var
    i   : Integer ;
    buf : array[0..5] of Double ;
    val : Double ;
    {$IFDEF CLR}
     pbuf : IntPtr ;
    {$ENDIF}
    {$IFDEF JAVA}
     pbuf : com.sun.jna.Pointer ;
    {$ENDIF}
  begin
    Result := IsValid ;
    if not Result then exit ;

    if assigned( _cs ) then
      Lib.GDALSetProjection( Handle, toAPI( _cs.WKT ) ) ;

    buf[0] := _extent.XMin ;
    buf[1] := ( _extent.XMax - _extent.XMin ) / _width ;
    buf[2] := 0.0 ;
    buf[3] := _extent.YMax ;
    buf[4] := 0.0 ;
    buf[5] := -( _extent.YMax - _extent.YMin ) / _height ;

    {$IFDEF OXYGENE}
      pbuf := Marshal.AllocHGlobal( length( buf ) * sizeOf( Double ) ) ;
      {$IFDEF CLR}
      Marshal.Copy( buf, 0, pbuf, length( buf ) ) ;
      {$ELSE}
      pbuf.write( 0, buf, 0, length( buf ) ) ;
      {$ENDIF}
      Lib.GDALSetGeoTransform( Handle, pbuf ) ;
      Marshal.FreeHGlobal( pbuf ) ;
    {$ELSE}
      {$IFDEF LEVEL_XE2_RTL}
        Lib.GDALSetGeoTransform( Handle, IntPtr(@buf[0]) ) ;
      {$ELSE}
        Lib.GDALSetGeoTransform( Handle, @buf[0] ) ;
      {$ENDIF}
    {$ENDIF}

    readProperties ;

    val := GIS_GRID_NOVALUE ;
    for i := 1 to FBandCount do begin
      FBands.Add( T_GDALRasterBand.Create( self, i ) ) ;
      Lib.GDALSetRasterNoDataValue( GetRasterBand(i).Handle, val  ) ;
    end ;
  end ;

//==============================================================================
// T_GDALRasterBand
//==============================================================================

  constructor T_GDALRasterBand.Create(
    const _dataset  : T_GDALDataset ;
    const _bandId   : Integer
   ) ;
  begin
    inherited Create( _dataset.Lib ) ;

    if IsLibrary then
      Handle := Lib.GDALGetRasterBand( _dataset.Handle, _bandId ) ;
    FId := _bandId ;

    readProperties ;
  end ;

  procedure T_GDALRasterBand.readProperties ;
  var
  {$IFDEF CLR}
    pbx, pby : IntPtr ;
    pminmax : IntPtr ;
  {$ENDIF}
  {$IFDEF JAVA}
    pbx, pby : com.sun.jna.Pointer ;
    pminmax : com.sun.jna.Pointer ;
  {$ENDIF}
   gotmin  : Integer ;
   gotmax  : Integer ;
  {$IFDEF DCC}
    minmax  : array[0..1] of Double ;
  {$ENDIF}
  begin
    if IsValid and IsLibrary then begin
      FDataType := Lib.GDALGetRasterDataType( Handle );
      FXSize    := Lib.GDALGetRasterBandXSize( Handle );
      FYSize    := Lib.GDALGetRasterBandYSize( Handle );

      FDataTypeName := asString( Lib.GDALGetDataTypeName( FDataType ) ) ;
      FDataTypeSize := Lib.GDALGetDataTypeSize( FDataType ) div 8 ;
      {$IFDEF OXYGENE}
        pbx := Marshal.AllocHGlobal( 4 ) ;
        pby := Marshal.AllocHGlobal( 4 ) ;
        Lib.GDALGetBlockSize( Handle, pbx, pby ) ;
        FBlockXSize := Marshal.ReadInt32( pbx,0 ) ;
        FBlockYSize := Marshal.ReadInt32( pby,0 ) ;
        Marshal.FreeHGlobal( pbx ) ;
        Marshal.FreeHGlobal( pby ) ;
      {$ELSE}
        Lib.GDALGetBlockSize( Handle, @FBlockXSize, @FBlockYSize );
      {$ENDIF}

      FColorTable := Lib.GDALGetRasterColorTable( Handle );
      {$IFDEF JAVA}
        if FColorTable <> nil   then
      {$ELSE}
        {$IFDEF LEVEL_XE2_RTL}
          if FColorTable <> IntPtr(0)   then
        {$ELSE}
          if AssignedPtr( FColorTable ) then
        {$ENDIF}
      {$ENDIF}
        FColorTabEntryCnt := Lib.GDALGetColorEntryCount( FColorTable )
      else
        FColorTabEntryCnt := 0 ;

      FColorInt     := Lib.GDALGetRasterColorInterpretation( Handle ) ;
      FColorIntName := asString( Lib.GDALGetColorInterpretationName ( FColorInt ) ) ;

      gotmin := 0 ;
      gotmax := 0 ;
      {$IFDEF OXYGENE}
        FMin := Lib.GDALGetRasterMinimum( Handle, gotmin );
        FMax := Lib.GDALGetRasterMaximum( Handle, gotmax );
      {$ELSE}
        FMin := Lib.GDALGetRasterMinimum( Handle, @gotmin );
        FMax := Lib.GDALGetRasterMaximum( Handle, @gotmax );
      {$ENDIF}
        if not ( ( gotmin = 1 ) and ( gotmax = 1 ) ) then begin
        {$IFDEF OXYGENE}
          pminmax := Marshal.AllocHGlobal( 16 ) ;
          Lib.GDALComputeRasterMinMax( Handle, 1, pminmax );
          {$IFDEF JAVA}
            FMin := Marshal.ReadDouble(pminmax,0) ;
            FMax := Marshal.ReadDouble(pminmax,8) ;
          {$ELSE}
            FMin := BitConverter.Int64BitsToDouble( Marshal.ReadInt64(pminmax,0) ) ;
            FMax := BitConverter.Int64BitsToDouble( Marshal.ReadInt64(pminmax,8) ) ;
          {$ENDIF}
          Marshal.FreeHGlobal( pminmax ) ;
        {$ELSE}
          minmax[ 0 ] := 0 ;
          minmax[ 1 ] := 0 ;
          {$IFDEF LEVEL_XE2_RTL}
            Lib.GDALComputeRasterMinMax( Handle, 1, IntPtr(@minmax[0]) );
          {$ELSE}
            Lib.GDALComputeRasterMinMax( Handle, 1, @minmax[0] );
          {$ENDIF}
          FMin := minmax[ 0 ] ;
          FMax := minmax[ 1 ] ;
        {$ENDIF}
        end ;
      {$IFDEF OXYGENE}
        FNoDataValue := Lib.GDALGetRasterNoDataValue( Handle, gotmin ) ;
      {$ELSE}
        FNoDataValue := Lib.GDALGetRasterNoDataValue( Handle, @gotmin ) ;
      {$ENDIF}

      inherited ;
    end ;
  end ;

  function T_GDALRasterBand.GetInfo : String ;
  var
    sb  : TStringList ;
  begin
    sb := TStringList.Create ;
    try
      sb.Add( Format( 'Bands Id: %d', [ FId ] ) ) ;
      sb.Add( Format( 'Data Type: %s', [ FDataTypeName ] ) ) ;
      sb.Add( Format( 'Size: %dx%d', [ FXSize, FYSize ] ) ) ;
      sb.Add( Format( 'Block Size: %dx%d', [ BlockXSize, BlockYSize ] ) ) ;
      sb.Add( Format( 'Color Table Size: %d', [ FColorTabEntryCnt ] ) ) ;
      sb.Add( Format( 'Color Interp: %s', [ FColorIntName ] ) ) ;
      sb.Add( Format( 'Min: %.8f', [ FMin ] ) ) ;
      sb.Add( Format( 'Max: %.8f', [ FMax ] ) ) ;
      sb.Add( Format( 'NoData: %.8f', [ FNoDataValue ] ) ) ;

      if not IsStringEmpty( Metadata ) then begin
        sb.Add( 'Metadata:' ) ;
        sb.Add( Metadata ) ;
      end ;

      Result := sb.Text ;
    finally
      FreeObject( sb ) ;
    end ;
  end ;

  procedure T_GDALRasterBand.RasterIO(
    const _flag       : GDALRWFlag ;
    const _xOff       : Integer ;
    const _yOff       : Integer ;
    const _xSize      : Integer ;
    const _ySize      : Integer ;
    {$IFDEF JAVA}
    const _buffer     : com.sun.jna.Pointer ;
    {$ELSE}
    const _buffer     : IntPtr ;
    {$ENDIF}
    const _bufXSize   : Integer ;
    const _bufYSize   : Integer ;
    const _dataType   : GDALDataType
   ) ;
  var
    err : CPLErr ;
  begin
    if IsValid and IsLibrary then begin
      err := Lib.GDALRasterIO(
                    Handle,
                    _flag,
                    _xOff,
                    _yOff,
                    _xSize,
                    _ySize,
                    _buffer,
                    _bufXSize,
                    _bufYSize,
                    _dataType,
                    0,
                    0
                  ) ;
      if err <> CE_None then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ) + #13#10 +
                                      Format( '%s', [fget_Error] ),
                                      '', 0
                                    ) ;
    end ;
  end ;

  function T_GDALRasterBand.GetColorEntry(
    const _tabIndex   : Integer ;
    const _colorIndex : Integer
   ) : Byte ;
  var
    colorEntry : PGDALCOLORENTRY ;
  begin
    if IsLibrary then begin
      colorEntry := Lib.GDALGetColorEntry( FColorTable, _tabIndex ) ;

      {$IFDEF OXYGENE}
        case _colorIndex of
          1  : Result := Byte( Marshal.ReadInt16( colorEntry, 0 ) ) ;
          2  : Result := Byte( Marshal.ReadInt16( colorEntry, 2 ) ) ;
          3  : Result := Byte( Marshal.ReadInt16( colorEntry, 4 ) ) ;
          4  : Result := Byte( Marshal.ReadInt16( colorEntry, 6 ) )
        else   Result := 0 ;
        end ;
      {$ELSE}
        case _colorIndex of
          1  : Result := Byte( colorEntry^.c1 ) ;
          2  : Result := Byte( colorEntry^.c2 ) ;
          3  : Result := Byte( colorEntry^.c3 ) ;
          4  : Result := Byte( colorEntry^.c4 )
        else   Result := 0 ;
        end ;
      {$ENDIF}
    end
    else
      Result := 0 ;
  end ;

//  procedure T_GDALRasterBand.SetColorTable(
//   {$IFDEF CLR}
//    const _paletteEntry : array of System.Drawing.Color
//   {$ENDIF}
//   {$IFDEF JAVA}
//    const _paletteEntry : array of java.awt.Color
//   {$ENDIF}
//   {$IFDEF DCC}
//    const _paletteEntry : array of TPaletteEntry
//   {$ENDIF}
//  ) ;
//  var
//    i,cnt  : Integer ;
//    centry : GDALColorEntry;
//    {$IFDEF OXYGENE}
//      ptr  : IntPtr ;
//    {$ENDIF}
//  begin
//    FColorTable := Lib.GDALGetRasterColorTable( Handle ) ;
//    {$IFDEF LEVEL_XE2_RTL}
//      if FColorTable = IntPtr(0)        then
//    {$ELSE}
//      if not AssignedPtr( FColorTable ) then
//    {$ENDIF}
//      FColorTable := Lib.GDALCreateColorTable( 0 ) ;
//
//    {$IFDEF LEVEL_XE2_RTL}
//      if FColorTable <> IntPtr(0)   then begin
//    {$ELSE}
//      if AssignedPtr( FColorTable ) then begin
//    {$ENDIF}
//      cnt := length( _paletteEntry )-1 ;
//      if cnt > 255 then
//        cnt := 255 ;
//      i := 0 ;
//      while i < cnt do begin
//        {$IFDEF OXYGENE}
//          centry.c1 := _paletteEntry[i].R ;
//          centry.c2 := _paletteEntry[i].G ;
//          centry.c3 := _paletteEntry[i].B ;
//          centry.c4 := _paletteEntry[i].A ;
//          ptr := Marshal.AllocHGlobal(Marshal.SizeOf(centry)) ;
//          Marshal.StructureToPtr(centry, ptr, False);
//          Lib.GDALSetColorEntry( FColorTable, i, ptr ) ;
//          Marshal.FreeHGlobal(ptr) ;
//        {$ELSE}
//          centry.c1 := _paletteEntry[i].peRed ;
//          centry.c2 := _paletteEntry[i].peGreen ;
//          centry.c3 := _paletteEntry[i].peBlue ;
//          centry.c4 := _paletteEntry[i].peFlags ;
//          Lib.GDALSetColorEntry( FColorTable, i, @centry.c1 ) ;
//        {$ENDIF}
//        inc( i ) ;
//      end ;
//    end ;
//  end ;

//==============================================================================
// T_GDALDriver
//==============================================================================

  constructor T_GDALDriver.Create(
    const _dataset : T_GDALDataset ;
    const _library : T_GDALLib
  ) ;
  begin
    inherited Create( _library ) ;

    if IsLibrary and assigned( _dataset ) then
      Handle := Lib.GDALGetDatasetDriver( _dataset.Handle )
    else
      {$IFDEF LEVEL_XE2_RTL}
        Handle := IntPtr(0) ;
      {$ELSE}
        Handle := nil ;
      {$ENDIF}

    readProperties ;
  end ;

  constructor T_GDALDriver.Create(
    const _driver  : GDALDriverH ;
    const _library : T_GDALLib
  ) ;
  begin
    inherited Create( _library ) ;

    Handle := _driver ;

    readProperties ;
  end ;

  constructor T_GDALDriver.Create(
    const _name    : String ;
    const _library : T_GDALLib
  ) ;
  begin
    inherited Create( _library ) ;

    if IsLibrary then
      Handle := Lib.GDALGetDriverByName( toAPI( _name ) )
    else
      {$IFDEF LEVEL_XE2_RTL}
        Handle := IntPtr(0) ;
      {$ELSE}
        Handle := nil ;
      {$ENDIF}

    readProperties ;
  end ;

  procedure T_GDALDriver.readProperties ;
  var
    val1 : String ;
    val2 : String ;
  begin
    if IsValid and IsLibrary then begin
      FSName := asString( Lib.GDALGetDriverShortName( Handle ) ) ;
      FLName := asString( Lib.GDALGetDriverLongName( Handle ) ) ;

      FExtension := asString( Lib.GDALGetMetadataItem( Handle, toAPI( 'DMD_EXTENSION' ), nil ) ) ;

      val1 := asString( Lib.GDALGetMetadataItem( Handle, toAPI( 'DCAP_CREATE' ), nil ) ) ;
      val2 := asString( Lib.GDALGetMetadataItem( Handle, toAPI( 'DCAP_CREATECOPY' ), nil ) ) ;
      FCanCreate     := not IsStringEmpty( val1 ) ;
      FCanCreateCopy := not IsStringEmpty( val2 ) ;
      if not FCanCreate and not FCanCreateCopy then
        FIsReadOnly := True
      else
        FIsReadOnly := False ;

      inherited ;
    end
    else begin
      FSName := '' ;
      FLName := '' ;
    end ;
  end ;

  function T_GDALDriver.GetInfo : String ;
  begin
    Result := Format( '%s (%s)', [ FSName, FLName ] ) ;
  end ;

  function T_GDALDriver.CreateDataset(
    const _filename : String ;
    const _width    : Integer ;
    const _height   : Integer ;
    const _bands    : Integer ;
    const _datatype : GDALDataType ;
    const _options  : TStrings
   ) : T_GDALDataset ;
  var
    newds   : GDALDatasetH ;
    {$IFDEF JAVA}
    options : com.sun.jna.Pointer ;
    {$ELSE}
    options : IntPtr ;
    {$ENDIF}
    i       : Integer ;
  begin
    Result := nil ;

    {$IFDEF LEVEL_XE2_RTL}
      options := IntPtr(0) ;
    {$ELSE}
      options := nil ;
    {$ENDIF}

    if _options.Count > 0 then begin
      for i := 0 to _options.Count-1 do
        options := T_GDALLib(FLib).CSLAddString( options, toAPI( _options[i] ) ) ;
    end ;

    if IsLibrary then
      newds := Lib.GDALCreate(
                      Handle,
                      toAPI( _filename ),
                      _width,
                      _height,
                      _bands,
                      _datatype,
                      options
                    )
    else
      {$IFDEF LEVEL_XE2_RTL}
        newds := IntPtr(0) ;
      {$ELSE}
        newds := nil ;
      {$ENDIF}

    {$IFDEF LEVEL_XE2_RTL}
      assert( newds <> IntPtr(0), fget_Error ) ;
    {$ELSE}
      assert( newds <> nil,       fget_Error ) ;
    {$ENDIF}

    {$IFDEF JAVA}
      if newds <> nil then
    {$ELSE}
      {$IFDEF LEVEL_XE2_RTL}
        if newds <> IntPtr(0)   then
      {$ELSE}
        if AssignedPtr( newds ) then
      {$ENDIF}
    {$ENDIF}
      Result := T_GDALDataset.Create( newds, Self, Lib ) ;
  end ;

  function T_GDALDriver.GetDriver(
    const _index : Integer
  ) : T_GDALDriver ;
  begin
    if IsLibrary then
      Result := T_GDALDriver.Create( Lib.GDALGetDriver( _index ), Lib )
    else
      Result := nil ;
  end ;

  function T_GDALDriver.GetDriverCount : Integer ;
  begin
    if IsLibrary then
      Result := Lib.GDALGetDriverCount
    else
      Result := 0 ;
  end ;

//==============================================================================
// T_GDALLib
//==============================================================================


  constructor T_GDALLib.Create ;
  begin
    {$IFNDEF OXYGENE}
    inherited Create;
    {$ENDIF}

    DLLLoaded := False ;
  end ;

  {$IFNDEF OXYGENE}

  destructor T_GDALLib.Destroy ;
  begin
    if DLLLoaded and ( DLLHandle <> 0 ) then
      FreeLibrary( DLLHandle ) ;

    DLLLoaded := False ;

    inherited ;
  end ;
  {$ENDIF}

  function T_GDALLib.LoadDLL(
    const _dllPath : String
  ) : Boolean ;

    {$IFNDEF JAVA}
      function stringLast(
        const _val : AnsiString
      ) : Integer ;
      begin
        {$IFDEF OXYGENE}
          if assigned( _val ) then
            Result := _val.Length - 1
          else
            Result := -1 ;
        {$ELSE}
          Result := Length( _val ) ;
        {$ENDIF}
      end ;

      {$IFDEF CLR}
        function translateName( const _name : String ) : String ;
      {$ELSE}
        function translateName( const _name : AnsiString ) : AnsiString ;
      {$ENDIF}
        var
          i : Integer ;
        begin
          if GisEnvironmentInfo.Is32 then
            Result := _name
          else begin
            Result := '' ;
            for i := StringFirst to stringLast( _name ) do begin
              if (i = StringFirst) and (_name[i] = '_' ) then continue ;

              if CharInSet( _name[i], ['a'..'z','A'..'Z', '_'] ) then
                Result := Result + _name[i] ;
            end ;
          end ;
        end ;

      procedure mapFnc( {$IFDEF LEVEL_XE2_RTL}
                          var _fnc : Pointer ;
                        {$ELSE}
                          var _fnc : IntPtr ;
                        {$ENDIF}
                        {$IFDEF CLR}
                          const _name : String
                        {$ELSE}
                          const _name : AnsiString
                        {$ENDIF}
                      ) ;
      {$IFNDEF CLR}
        var
          nn : AnsiString ;
      {$ENDIF}
      begin
        {$IFDEF CLR}
          _fnc := GetProcAddress( DLLHandle, translateName(_name) ) ;
        {$ELSE}
          nn := translateName(_name) ;
          _fnc := GetProcAddress( DLLHandle, PAnsiChar( nn ) ) ;
        {$ENDIF}
        assert( _fnc <> nil ) ;
        if _fnc = nil then
          Abort ;
      end;
      {$ENDIF}

  begin
    Result := DLLLoaded ;
    if DLLLoaded then exit;

    try
      {$IFDEF JAVA}
        var options: java.util.Map<String,com.sun.jna.FunctionMapper> := new java.util.HashMap<String,com.sun.jna.FunctionMapper>();
        options.put(com.sun.jna.&Library.OPTION_FUNCTION_MAPPER, new T_GDALMapper());

        DLLHandle := IGDALLibrary(com.sun.jna.Native.loadLibrary( _dllPath, typeOf(IGDALLibrary), options ) );
        if DLLHandle = nil then Abort ;
      {$ELSE}
        DLLHandle := LoadLibraryWithinHinstance( _dllPath );
        if DLLHandle = 0 then Abort ;
      {$ENDIF}

      {$IFDEF CLR}
        mapFnc( GDALAllRegisterProc,
                '_GDALAllRegister@0'
              ) ;
        mapFnc( GDALCloseProc,
                '_GDALClose@4'
              ) ;
        mapFnc( GDALCreateProc,
                '_GDALCreate@28'
              ) ;
        mapFnc( GDALComputeRasterMinMaxProc,
                '_GDALComputeRasterMinMax@12'
              ) ;
        mapFnc( GDALGetDriverLongNameProc,
                '_GDALGetDriverLongName@4'
              ) ;
        mapFnc( GDALGetDriverShortNameProc,
                '_GDALGetDriverShortName@4'
              ) ;
        mapFnc( GDALGetBlockSizeProc,
                '_GDALGetBlockSize@12'
              ) ;
        mapFnc( GDALGetColorEntryProc,
                '_GDALGetColorEntry@8'
              ) ;
        mapFnc( GDALSetColorEntryProc,
                '_GDALSetColorEntry@12'
              ) ;
        mapFnc( GDALCreateColorTableProc,
                '_GDALCreateColorTable@4'
              ) ;
        mapFnc( GDALGetColorEntryCountProc,
                '_GDALGetColorEntryCount@4'
              ) ;
        mapFnc( GDALGetColorIntNameProc,
                'GDALGetColorInterpretationName'
              ) ;
        mapFnc( GDALGetDataTypeSizeProc,
                '_GDALGetDataTypeSize@4'
              ) ;
        mapFnc( GDALGetDataTypeNameProc,
                '_GDALGetDataTypeName@4'
              ) ;
        mapFnc( GDALGetDatasetDriverProc,
                '_GDALGetDatasetDriver@4'
              ) ;
        mapFnc( GDALGetDriverByNameProc,
                '_GDALGetDriverByName@4'
              ) ;
        mapFnc( GDALGetDriverProc,
                '_GDALGetDriver@4'
              ) ;
        mapFnc( GDALGetDriverCountProc,
                '_GDALGetDriverCount@0'
              ) ;
        mapFnc( GDALGetGCPProjectionProc,
                '_GDALGetGCPProjection@4'
              ) ;
        mapFnc( GDALGetGeoTransformProc,
                '_GDALGetGeoTransform@8'
              ) ;
        mapFnc( GDALSetGeoTransformProc,
                '_GDALSetGeoTransform@8'
              ) ;
        mapFnc( GDALGetMetadataProc,
                '_GDALGetMetadata@8'
              ) ;
        mapFnc( GDALGetMetadataItemProc,
                '_GDALGetMetadataItem@12'
              ) ;
        mapFnc( GDALGetProjectionRefProc,
                '_GDALGetProjectionRef@4'
              ) ;
        mapFnc( GDALSetProjectionProc,
                '_GDALSetProjection@8'
              ) ;
        mapFnc( GDALGetRasterBandProc,
                '_GDALGetRasterBand@8'
              ) ;
        mapFnc( GDALGetRasterBandXSizeProc,
                '_GDALGetRasterBandXSize@4'
              ) ;
        mapFnc( GDALGetRasterBandYSizeProc,
                '_GDALGetRasterBandYSize@4'
              ) ;
        mapFnc( GDALGetRasterColorIntProc,
                '_GDALGetRasterColorInterpretation@4'
              ) ;
        mapFnc( GDALGetRasterColorTableProc,
                '_GDALGetRasterColorTable@4'
              ) ;
        mapFnc( GDALGetRasterCountProc,
                '_GDALGetRasterCount@4'
              ) ;
        mapFnc( GDALGetRasterDataTypeProc,
                '_GDALGetRasterDataType@4'
              ) ;
        mapFnc( GDALGetRasterMaximumProc,
                '_GDALGetRasterMaximum@8'
              ) ;
        mapFnc( GDALGetRasterMinimumProc,
                '_GDALGetRasterMinimum@8'
              ) ;
        mapFnc( GDALGetRasterNoDataValueProc,
                '_GDALGetRasterNoDataValue@8'
              ) ;
        mapFnc( GDALSetRasterNoDataValueProc,
                '_GDALSetRasterNoDataValue@12'
              ) ;
        mapFnc( GDALGetRasterXSizeProc,
                '_GDALGetRasterXSize@4'
              ) ;
        mapFnc( GDALGetRasterYSizeProc,
                '_GDALGetRasterYSize@4'
              ) ;
        mapFnc( GDALOpenProc,
                '_GDALOpen@8'
              ) ;
        mapFnc( GDALRasterIOProc,
                '_GDALRasterIO@48'
              ) ;
        mapFnc( CSLFetchBooleanProc,
                'CSLFetchBoolean'
              ) ;
        mapFnc( CSLCountProc,
                'CSLCount'
              ) ;
        mapFnc( CSLGetFieldProc,
                'CSLGetField'
              ) ;
        mapFnc( CPLGetLastErrorMsgProc,
                '_CPLGetLastErrorMsg@0'
              ) ;
      {$ENDIF}
      {$IFDEF JAVA}
        GDALAllRegisterProc          := 1 ;
        GDALOpenProc                 := 1 ;
        GDALCloseProc                := 1 ;
        GDALCreateProc               := 1 ;
        GDALComputeRasterMinMaxProc  := 1 ;
        GDALGetDriverLongNameProc    := 1 ;
        GDALGetDriverShortNameProc   := 1 ;
        GDALGetBlockSizeProc         := 1 ;
        GDALGetColorEntryProc        := 1 ;
        GDALSetColorEntryProc        := 1 ;
        GDALGetColorEntryCountProc   := 1 ;
        GDALGetColorIntNameProc      := 1 ;
        GDALGetDataTypeSizeProc      := 1 ;
        GDALGetDatasetDriverProc     := 1 ;
        GDALGetDriverByNameProc      := 1 ;
        GDALGetDriverProc            := 1 ;
        GDALGetDriverCountProc       := 1 ;
        GDALGetGCPProjectionProc     := 1 ;
        GDALGetGeoTransformProc      := 1 ;
        GDALSetGeoTransformProc      := 1 ;
        GDALGetMetadataProc          := 1 ;
        GDALGetMetadataItemProc      := 1 ;
        GDALGetProjectionRefProc     := 1 ;
        GDALSetProjectionProc        := 1 ;
        GDALGetRasterBandProc        := 1 ;
        GDALGetRasterBandXSizeProc   := 1 ;
        GDALGetRasterBandYSizeProc   := 1 ;
        GDALGetRasterColorIntProc    := 1 ;
        GDALGetRasterColorTableProc  := 1 ;
        GDALCreateColorTableProc     := 1 ;
        GDALGetRasterCountProc       := 1 ;
        GDALGetRasterDataTypeProc    := 1 ;
        GDALGetDataTypeNameProc      := 1 ;
        GDALGetRasterMaximumProc     := 1 ;
        GDALGetRasterMinimumProc     := 1 ;
        GDALGetRasterNoDataValueProc := 1 ;
        GDALSetRasterNoDataValueProc := 1 ;
        GDALGetRasterXSizeProc       := 1 ;
        GDALGetRasterYSizeProc       := 1 ;
        GDALRasterIOProc             := 1 ;
        CSLFetchBooleanProc          := 1 ;
        CSLCountProc                 := 1 ;
        CSLGetFieldProc              := 1 ;
        CPLGetLastErrorMsgProc       := 1 ;
      {$ENDIF}
      {$IFDEF DCC}
        mapFnc( @GDALAllRegister,
                '_GDALAllRegister@0'
              ) ;
        mapFnc( @GDALCreate,
                '_GDALCreate@28'
              ) ;
        mapFnc( @GDALCreateCopy,
                '_GDALCreateCopy@28'
              ) ;
        mapFnc( @GDALClose,
                '_GDALClose@4'
              ) ;
        mapFnc( @GDALComputeRasterMinMax,
                '_GDALComputeRasterMinMax@12'
              ) ;
        mapFnc( @GDALGetDriverLongName,
                '_GDALGetDriverLongName@4'
              ) ;
        mapFnc( @GDALGetDriverShortName,
                '_GDALGetDriverShortName@4'
              ) ;
        mapFnc( @GDALGetBlockSize,
                '_GDALGetBlockSize@12'
              ) ;
        mapFnc( @GDALGetColorEntry,
                '_GDALGetColorEntry@8'
              ) ;
        mapFnc( @GDALGetColorEntryCount,
                '_GDALGetColorEntryCount@4'
              ) ;
        mapFnc( @GDALSetColorEntry,
                '_GDALSetColorEntry@12'
              ) ;
        mapFnc( @GDALCreateColorTable,
                '_GDALCreateColorTable@4'
              ) ;
        mapFnc( @GDALGetColorInterpretationName,
                'GDALGetColorInterpretationName'
              ) ;
        mapFnc( @GDALGetDataTypeSize,
                '_GDALGetDataTypeSize@4'
              ) ;
        mapFnc( @GDALGetDataTypeName,
                '_GDALGetDataTypeName@4'
              ) ;
        mapFnc( @GDALGetDatasetDriver,
                '_GDALGetDatasetDriver@4'
              ) ;
        mapFnc( @GDALGetDriverByName,
                '_GDALGetDriverByName@4'
              ) ;
        mapFnc( @GDALGetDriver,
                '_GDALGetDriver@4'
              ) ;
        mapFnc( @GDALGetDriverCount,
                '_GDALGetDriverCount@0'
              ) ;
        mapFnc( @GDALGetGCPProjection,
                '_GDALGetGCPProjection@4'
              ) ;
        mapFnc( @GDALGetGCPCount,
                '_GDALGetGCPCount@4'
              ) ;
        mapFnc( @GDALGetGCPs,
                '_GDALGetGCPs@4'
              ) ;
        mapFnc( @GDALGCPsToGeoTransform,
                '_GDALGCPsToGeoTransform@16'
              ) ;
        mapFnc( @GDALGetGeoTransform,
                '_GDALGetGeoTransform@8'
              ) ;
        mapFnc( @GDALSetGeoTransform,
                '_GDALSetGeoTransform@8'
              ) ;
        mapFnc( @GDALGetMetadata,
                '_GDALGetMetadata@8'
              ) ;
        mapFnc( @GDALGetMetadataItem,
                '_GDALGetMetadataItem@12'
              ) ;
        mapFnc( @GDALGetProjectionRef,
                '_GDALGetProjectionRef@4'
              ) ;
        mapFnc( @GDALSetProjection,
                '_GDALSetProjection@8'
              ) ;
        mapFnc( @GDALGetRasterBand,
                '_GDALGetRasterBand@8'
              ) ;
        mapFnc( @GDALGetRasterBandXSize,
                '_GDALGetRasterBandXSize@4'
              ) ;
        mapFnc( @GDALGetRasterBandYSize,
                '_GDALGetRasterBandYSize@4'
              ) ;
        mapFnc( @GDALGetRasterColorInterpretation,
                '_GDALGetRasterColorInterpretation@4'
              ) ;
        mapFnc( @GDALGetRasterColorTable,
                '_GDALGetRasterColorTable@4'
              ) ;
        mapFnc( @GDALGetRasterCount,
                '_GDALGetRasterCount@4'
              ) ;
        mapFnc( @GDALGetRasterDataType,
                '_GDALGetRasterDataType@4'
              ) ;
        mapFnc( @GDALGetRasterMaximum,
                '_GDALGetRasterMaximum@8'
              ) ;
        mapFnc( @GDALGetRasterMinimum,
                '_GDALGetRasterMinimum@8'
              ) ;
        mapFnc( @GDALGetRasterNoDataValue,
                '_GDALGetRasterNoDataValue@8'
              ) ;
        mapFnc( @GDALSetRasterNoDataValue,
                '_GDALSetRasterNoDataValue@12'
              ) ;
        mapFnc( @GDALGetRasterXSize,
                '_GDALGetRasterXSize@4'
              ) ;
        mapFnc( @GDALGetRasterYSize,
                '_GDALGetRasterYSize@4'
              ) ;
        mapFnc( @GDALOpen,
                '_GDALOpen@8'
              ) ;
        mapFnc( @GDALRasterIO,
                '_GDALRasterIO@48'
              ) ;
        mapFnc( @CSLFetchBoolean,
                'CSLFetchBoolean'
              ) ;
        mapFnc( @CSLCount,
                'CSLCount'
              ) ;
        mapFnc( @CSLGetField,
                'CSLGetField'
              ) ;
        mapFnc( @CPLGetLastErrorMsg,
                '_CPLGetLastErrorMsg@0'
              ) ;
        mapFnc( @CPLSetConfigOption,
                '_CPLSetConfigOption@8'
              ) ;
        mapFnc( @CSLAddString,
                'CSLAddString'
              ) ;
        mapFnc( @CSLDestroy,
                '_CSLDestroy@4'
              ) ;

      {$ENDIF}

      DLLLoaded := True ;

    except
      DLLLoaded := False ;
    end ;

    Result := DLLLoaded ;
  end ;

  {$IFDEF JAVA}
    class procedure T_GDALLib.GDALAllRegister ;
    begin
      DLLHandle.GDALAllRegister ;
    end;

    class function T_GDALLib.GDALOpen( const _pszFilename  : TBytes;
                      const _eAccess      : GDALAccess
                      ) : GDALDatasetH ;
    begin
      Result := DLLHandle.GDALOpen( _pszFilename, _eAccess ) ;
    end;

    class function T_GDALLib.GDALCreate( const _hDriver        : GDALDriverH;
                        const _pszFilename    : String;
                        const _nXSize         : Integer;
                        const _nYSize         : Integer;
                        const _nBands         : Integer;
                        const _eBandType      : GDALDataType;
                        const _papszParmList  : com.sun.jna.Pointer
                        ) : GDALDatasetH ;
    begin
      Result := DLLHandle.GDALCreate(_hDriver,_pszFilename,_nXSize,_nYSize,_nBands,_eBandType,_papszParmList) ;
    end;

    class function T_GDALLib.GDALCreateCopy ( const _hDriver        : GDALDriverH;
                              const _pszFilename    : String;
                              const _hSrcDS         : GDALDatasetH;
                              const _bStrict        : Integer;
                              const _papszOptions   : com.sun.jna.Pointer ;
                              const _pfnProgress    : T_GDALProgressFunc ;
                              const _pProgressData  : IntPtr
                            ) : GDALDatasetH ;
    begin
      Result := DLLHandle.GDALCreateCopy( _hDriver, _pszFilename, _hSrcDS, _bStrict, _papszOptions, _pfnProgress, _pProgressData ) ;
    end;

    class function T_GDALLib.GDALGetDataTypeSize( const _eDataType : GDALDataType
                                      ) : Integer ;
    begin
      Result := DLLHandle.GDALGetDataTypeSize( _eDataType ) ;
    end;

    class function T_GDALLib.GDALGetMetadata( const _hDataset   : GDALMajorObjectH;
                              const _pszDomain  : String
                            ) : IntPtr ;
    begin
      Result := DLLHandle.GDALGetMetadata( _hDataset, _pszDomain ) ;
    end;

    class function T_GDALLib.GDALGetMetadataItem( const _hObject    : GDALMajorObjectH;
                                  const _pszName    : String ;
                                  const _pszDomain  : String
                                ) : String ;
    begin
      Result := DLLHandle.GDALGetMetadataItem( _hObject, _pszName, _pszDomain ) ;
    end;

    class function T_GDALLib.GDALGetDatasetDriver( const _hDataset  : GDALDatasetH
                                  ) : GDALDriverH ;
    begin
      Result := DLLHandle.GDALGetDatasetDriver( _hDataset ) ;
    end;

    class function T_GDALLib.GDALGetDriverByName( const _name  : String
                                  ) : GDALDriverH ;
    begin
      Result := DLLHandle.GDALGetDriverByName( _name ) ;
    end;

    class function T_GDALLib.GDALGetDriver( const _index : Integer
                            ) : GDALDriverH ;
    begin
      Result := DLLHandle.GDALGetDriver( _index ) ;
    end;

    class function T_GDALLib.GDALGetDriverCount : Integer ;
    begin
      Result := DLLHandle.GDALGetDriverCount ;
    end;

    class procedure T_GDALLib.GDALClose( const _hDataset  : GDALDatasetH
                                  ) ;
    begin
      DLLHandle.GDALClose( _hDataset ) ;
    end;

    class function T_GDALLib.GDALGetRasterXSize( const _hDataset : GDALDatasetH
                                  ) : Integer ;
    begin
      Result := DLLHandle.GDALGetRasterXSize( _hDataset ) ;
    end;

    class function T_GDALLib.GDALGetRasterYSize( const _hDataset : GDALDatasetH
                                  ) : Integer ;
    begin
      Result := DLLHandle.GDALGetRasterYSize( _hDataset ) ;
    end;

    class function T_GDALLib.GDALGetRasterCount( const _hDataset : GDALDatasetH
                                  ) : Integer ;
    begin
      Result := DLLHandle.GDALGetRasterCount( _hDataset ) ;
    end;

    class function T_GDALLib.GDALGetRasterBand( const _hDataset : GDALDatasetH;
                                  const _nBandId  : Integer
                                  ) : GDALRasterBandH ;
    begin
      Result := DLLHandle.GDALGetRasterBand( _hDataset, _nBandId ) ;
    end;

    class function T_GDALLib.GDALGetProjectionRef( const _hDataset : GDALDatasetH
                                    ) : String ;
    begin
      Result := DLLHandle.GDALGetProjectionRef( _hDataset ) ;
    end;

    class function T_GDALLib.GDALSetProjection( const _hDataset      : GDALDatasetH ;
                              const _pszProjection : String
                              ) : CPLErr ;
    begin
      Result := DLLHandle.GDALSetProjection( _hDataset, _pszProjection ) ;
    end;

    class function T_GDALLib.GDALGetGeoTransform( const _hDataset       : GDALDatasetH;
                                const _padfTransform  : com.sun.jna.Pointer
                                ) : CPLErr ;
    begin
      Result := DLLHandle.GDALGetGeoTransform( _hDataset, _padfTransform ) ;
    end;

    class function T_GDALLib.GDALSetGeoTransform( const _hDataset       : GDALDatasetH;
                                const _padfTransform  : com.sun.jna.Pointer
                                ) : CPLErr ;
    begin
      Result := DLLHandle.GDALSetGeoTransform( _hDataset, _padfTransform ) ;
    end;

    class function T_GDALLib.GDALGetGCPProjection( const _hDataset : GDALDatasetH
                                  ) : String ;
    begin
      Result := DLLHandle.GDALGetGCPProjection( _hDataset ) ;
    end;

    class function T_GDALLib.GDALGetGCPCount( const _hDataset : GDALDatasetH
                                  ) : Integer ;
    begin
      Result := DLLHandle.GDALGetGCPCount( _hDataset ) ;
    end;

    class function T_GDALLib.GDALGetGCPs( const _hDataset : GDALDatasetH
                                  ) : com.sun.jna.Pointer ;
    begin
      Result := DLLHandle.GDALGetGCPs( _hDataset ) ;
    end;

    class function T_GDALLib.GDALGCPsToGeoTransform( const _nGCPCount     : Integer ;
                                    const _asGCPs        : com.sun.jna.Pointer ;
                                    const _padfTransform : com.sun.jna.Pointer ;
                                    const _bApproxOK     : Integer
                                  ) : Integer ;
    begin
      Result := DLLHandle.GDALGCPsToGeoTransform(_nGCPCount,_asGCPs, _padfTransform, _bApproxOK ) ;
    end;

    class function T_GDALLib.GDALGetDriverShortName( const _hDriver : GDALDriverH
                                    )  : String ;
    begin
      Result := DLLHandle.GDALGetDriverShortName( _hDriver ) ;
    end;

    class function T_GDALLib.GDALGetDriverLongName( const _hDriver : GDALDriverH
                                    )  : String ;
    begin
      Result := DLLHandle.GDALGetDriverLongName( _hDriver ) ;
    end;

    class function T_GDALLib.GDALGetRasterDataType( const _hBand : GDALRasterBandH
                                    ) : GDALDataType ;
    begin
      Result := DLLHandle.GDALGetRasterDataType(_hBand) ;
    end;

    class function T_GDALLib.GDALGetDataTypeName( const _eDataType : GDALDataType
                                ) : String ;
    begin
      Result := DLLHandle.GDALGetDataTypeName(_eDataType) ;
    end;

    class procedure T_GDALLib.GDALGetBlockSize( const _hBand      : GDALRasterBandH;
                                const _pnXSize   : com.sun.jna.Pointer;
                                const _pnYSize   : com.sun.jna.Pointer
                                )  ;
    begin
      DLLHandle.GDALGetBlockSize(_hBand,_pnXSize,_pnYSize) ;
    end;

    class function T_GDALLib.GDALGetRasterBandXSize( const _hBand : GDALRasterBandH
                                    ) : Integer ;
    begin
      Result := DLLHandle.GDALGetRasterBandXSize(_hBand) ;
    end;

    class function T_GDALLib.GDALGetRasterBandYSize( const _hBand : GDALRasterBandH
                                    ) : Integer ;
    begin
      Result := DLLHandle.GDALGetRasterBandYSize(_hBand) ;
    end;

    class function T_GDALLib.GDALCreateColorTable( const _eColorInterp : GDALColorInterp
                                  ) : GDALColorTableH ;
    begin
      Result := DLLHandle.GDALCreateColorTable(_eColorInterp) ;
    end;

    class function T_GDALLib.GDALGetRasterColorInterpretation( const _hBand : GDALRasterBandH
                                              ) : GDALColorInterp ;
    begin
      Result := DLLHandle.GDALGetRasterColorInterpretation(_hBand) ;
    end;

    class function T_GDALLib.GDALGetRasterColorTable( const _hBand : GDALRasterBandH
                                      ) : GDALColorTableH ;
    begin
      Result := DLLHandle.GDALGetRasterColorTable(_hBand) ;
    end;

    class function T_GDALLib.GDALGetRasterMaximum( const _hBand     : GDALRasterBandH;
                                    const _pbSuccess : Integer
                                  ) : Double ;
    begin
      Result := DLLHandle.GDALGetRasterMaximum(_hBand,_pbSuccess) ;
    end;

    class function T_GDALLib.GDALGetRasterMinimum( const _hBand     : GDALRasterBandH;
                                    const _pbSuccess : Integer
                                  ) : Double ;
    begin
      Result := DLLHandle.GDALGetRasterMinimum(_hBand, _pbSuccess) ;
    end;

    class procedure T_GDALLib.GDALComputeRasterMinMax( const _hBand     : GDALRasterBandH;
                                        const _bApproxOK : Integer;
                                        const _adfMinMax : com.sun.jna.Pointer
                                      ) ;
    begin
      DLLHandle.GDALComputeRasterMinMax(_hBand, _bApproxOK, _adfMinMax) ;
    end;

    class function T_GDALLib.GDALRasterIO( const _hRBand      : GDALRasterBandH;
                            const _eRWFlag     : GDALRWFlag;
                            const _nXOff       : Integer;
                            const _nYOff       : Integer;
                            const _nXSize      : Integer;
                            const _nYSize      : Integer;
                            const _pBuffer     : com.sun.jna.Pointer;
                            const _nBufXSize   : Integer;
                            const _nBufYSize   : Integer;
                            const _eBufType    : GDALDataType;
                            const _nPixelSpace : Integer;
                            const _nLineSpace  : Integer
                          ) : CPLErr ;
    begin
      Result := DLLHandle.GDALRasterIO(_hRBand, _eRWFlag, _nXOff, _nYOff, _nXSize, _nYSize, _pBuffer, _nBufXSize, _nBufYSize, _eBufType, _nPixelSpace, _nLineSpace) ;
    end;

    class function T_GDALLib.GDALGetRasterNoDataValue( const _hRBand    : GDALRasterBandH ;
                                      const _pbSuccess : Integer
                                    ) : Double ;
    begin
      Result := DLLHandle.GDALGetRasterNoDataValue(_hRBand, _pbSuccess) ;
    end;

    class function T_GDALLib.GDALSetRasterNoDataValue( const _hRBand    : GDALRasterBandH ;
                                      const _pvalue    : Double
                                    ) : CPLErr ;
    begin
      Result := DLLHandle.GDALSetRasterNoDataValue(_hRBand, _pvalue) ;
    end;

    class function T_GDALLib.GDALGetColorEntryCount( const _hColorTable : GDALColorTableH
                                    ) : Integer ;
    begin
      Result := DLLHandle.GDALGetColorEntryCount(_hColorTable) ;
    end;

    class procedure T_GDALLib.GDALSetColorEntry(  const _hColorTable : GDALColorTableH;
                                const _nTabIndex   : Integer ;
                                const _entry       : PGDALCOLORENTRY
                                ) ;
    begin
      DLLHandle.GDALSetColorEntry(_hColorTable, _nTabIndex, _entry) ;
    end;

    class function T_GDALLib.GDALGetColorEntry(  const _hColorTable : GDALColorTableH;
                                  const _nTabIndex   : Integer
                                ) : PGDALCOLORENTRY ;
    begin
      Result := DLLHandle.GDALGetColorEntry(_hColorTable, _nTabIndex) ;
    end;

    class function T_GDALLib.GDALGetColorInterpretationName ( const _eColorInterp : GDALColorInterp
                                  ) : String ;
    begin
      Result := DLLHandle.GDALGetColorInterpretationName(_eColorInterp) ;
    end;

    class function T_GDALLib.CSLFetchBoolean( const _papszStrList  : IntPtr ;
                              const _pszKey       : String ;
                              const _bDefault     : Integer
                            ) : Integer ;
    begin
      Result := DLLHandle.CSLFetchBoolean( _papszStrList, _pszKey, _bDefault) ;
    end;

    class function T_GDALLib.CSLCount( const _papszStrList : IntPtr
                        ) : Integer ;
    begin
      Result := DLLHandle.CSLCount( _papszStrList) ;
    end;

    class function T_GDALLib.CSLGetField( const _papszStrList : IntPtr ;
                          const _iField       : Integer
                        ) : String ;
    begin
      Result := DLLHandle.CSLGetField( _papszStrList, _iField) ;
    end;

    class function T_GDALLib.CPLGetLastErrorMsg : String ;
    begin
      Result := DLLHandle.CPLGetLastErrorMsg ;
    end;

    class procedure T_GDALLib.CPLSetConfigOption( const _pszKey : String ;
                                    const _pszVal : String
                                    ) ;
    begin
      DLLHandle.CPLSetConfigOption( _pszKey, _pszVal) ;
    end;

    class function T_GDALLib.CSLAddString ( _papszStrList : com.sun.jna.Pointer ;
                                    const _pszNewString : String
                                  ) : com.sun.jna.Pointer ;
    begin
      Result := DLLHandle.CSLAddString( _papszStrList, _pszNewString) ;
    end;

    class procedure T_GDALLib.CSLDestroy  ( _papszStrList : com.sun.jna.Pointer ) ;
    begin
      DLLHandle.CSLDestroy( _papszStrList )  ;
    end;

  {$ENDIF}

//==============================================================================
// TGIS_FileGDAL
//==============================================================================

  constructor TGIS_FileGDAL.Create(
    const _path        : String      ;
    const _ext         : TGIS_Extent ;
    const _width       : Integer     ;
    const _height      : Integer     ;
    const _subformat   : TGIS_LayerPixelSubFormat     ;
    const _ppi         : Integer     ;
    const _cs          : TGIS_CSCoordinateSystem
  ) ;
  begin
    inherited Create( _path, _ext, _width, _height, _subformat, _ppi, _cs ) ;

    FInit := False ;
    FCreateOptions := TStringList.Create ;
  end ;

  constructor TGIS_FileGDAL.Create(
    const _path : String
  ) ;
  begin
    inherited Create ;

    FPath := _path  ;
    FSubDataset := '' ;
    FInit := False ;
    FCreateOptions := TStringList.Create ;
  end ;

  constructor TGIS_FileGDAL.Create;
  begin
    Create('') ;
  end ;

  procedure TGIS_FileGDAL.doDestroy ;
  begin
    FreeObject( FDataset ) ;
    FreeObject( FLib     ) ;
    FreeObject( FConvBmp ) ;
    FreeObject( FCreateOptions ) ;

    inherited ;
  end ;

  procedure TGIS_FileGDAL.prepareCapabilities ;
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
  end ;

  function TGIS_FileGDAL.initialize : Boolean ;
  begin
    Result := False ;

    if FInit then begin
      Result := True ;
      exit ;
    end ;

    if IsStringEmpty( FDLLPath ) then
      {$IFDEF JAVA}
      FDLLPath := GDAL_DEFAULT_DLL_NAME_NOEXT ;
      {$ELSE}
      FDLLPath := GDAL_DEFAULT_DLL_NAME ;
      {$ENDIF}

    FLib := T_GDALLib.Create ;

    if not T_GDALLib(FLib).LoadDLL( FDLLPath ) then begin
      exit ;
    end
    else begin
      Result := True ;
      FInit := True ;
      T_GDALLib(FLib).GDALAllRegister ;
    end ;
  end ;

  function TGIS_FileGDAL.findDriver(
    const _ext : String
  ) : String ;
  var
    i, cnt    : Integer ;
    drv, drvR : T_GDALDriver ;
    ext : String ;
  begin
    Result := '' ;
    if not initialize then exit ;

    drvR := T_GDALDriver.Create( '', T_GDALLib(FLib) ) ;
    try
      cnt := drvR.GetDriverCount ;
      for i := 0 to cnt-1 do begin
        drv := drvR.GetDriver(i) ;
        try
          if assigned( drv ) then begin
            ext := '.' + drv.Extension ;
            if CompareText( ext, _ext ) = 0 then begin
              Result := drv.ShortName ;
              exit ;
            end ;
          end;
        finally
          FreeObject( drv ) ;
        end ;
      end ;
    finally
      FreeObject( drvR ) ;
    end ;

    if IsStringEmpty( Result ) then begin
      if      CompareText( '.tiff', _ext ) = 0 then
        Result := 'GTiff'
      else if CompareText( '.ovr', _ext ) = 0 then
        Result := 'VRT'
      else if CompareText( '.jpeg', _ext ) = 0 then
        Result := 'JPEG'
      else if CompareText( '.j2k', _ext ) = 0 then
        Result := 'JPEG2000'
      else if CompareText( '.dem', _ext ) = 0 then
        Result := 'USGSDEM'
      else if CompareText( '.dt0', _ext ) = 0 then
        Result := 'DTED'
      else if CompareText( '.sid', _ext ) = 0 then
        Result := 'MrSID'
      else if CompareText( '.bil', _ext ) = 0 then
        Result := 'EHdr'
      else if CompareText( '.adf', _ext ) = 0 then
        Result := 'AIG'
      else if CompareText( '.hdf', _ext ) = 0 then
        Result := 'HDF4'
    end ;
  end ;

  function TGIS_FileGDAL.createDataset(
    const _driverName   : String ;
    const _numBands     : Integer ;
    const _dataType     : Integer
  ) : Boolean ;
  var
    drv : T_GDALDriver ;
  begin
    Result := False ;
    if not initialize then exit ;

    drv := T_GDALDriver.Create( _driverName, T_GDALLib(FLib) ) ;
    Result := assigned( drv ) and drv.IsValid ;
    assert( Result, 'Unknown driver' ) ;
    if not Result then exit ;

    FDataset := drv.CreateDataset( FPath, Width, Height, _numBands, _dataType, FCreateOptions ) ;
    Result := assigned( FDataset ) and T_GDALDataset(FDataset).IsValid ;
    if not Result then exit ;

    if not T_GDALDataset(FDataset).Build( Width, Height, FExtent, FCS ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ) + #13#10 +
        Format( '%s', [T_GDALDataset(FDataset).LastError] ),
        Path, 0
      ) ;
  end ;

  procedure TGIS_FileGDAL.Write(
    const _x       : Integer ;
    const _y       : Integer ;
    const _pixels  : TGIS_Pixels ;
    const _pformat : TGIS_PixelFormat ;
    const _width   : Integer ;
    const _height  : Integer
  ) ;
  var
    i, j  : Integer ;
    w, h  : Integer ;
    bufR  : TBytes ;
    bufG  : TBytes ;
    bufB  : TBytes ;
    bufA  : TBytes ;
    c     : TGIS_Color ;
  begin
    if (_x + _width) > Width then
      w := Width -_x
    else
      w := _width ;

    if (_y + _height) > Height then
      h := Height -_y
    else
      h := _height ;

    SetLength( bufR, w ) ;
    SetLength( bufG, w ) ;
    SetLength( bufB, w ) ;
    if _pformat = TGIS_PixelFormat.ARGB then
      SetLength( bufA, w ) ;

    for i := 0 to h-1 do begin
      for j := 0 to w-1 do begin
        c := TGIS_Color.FromARGB( Cardinal(_pixels[i*_width+j]) ) ;
        bufR[j] := c.R  ;
        bufG[j] := c.G  ;
        bufB[j] := c.B  ;
        if _pformat = TGIS_PixelFormat.ARGB then
          bufA[j] := c.A  ;
      end ;

      if _pformat = TGIS_PixelFormat.ARGB then
        WriteRasterBandRGB( 4, _x, _y+i, w, 1, bufA, w, 1 ) ;
      WriteRasterBandRGB( 3, _x, _y+i, w, 1, bufB, w, 1 ) ;
      WriteRasterBandRGB( 2, _x, _y+i, w, 1, bufG, w, 1 ) ;
      WriteRasterBandRGB( 1, _x, _y+i, w, 1, bufR, w, 1 ) ;
    end;
  end ;

  procedure TGIS_FileGDAL.WriteGrid(
    const _x   : Integer ;
    const _y   : Integer ;
    const _grd : TGIS_GridArray
  ) ;
  var
    i  : Integer ;
    w  : Integer ;
    h  : Integer ;
  begin
    h := length( _grd ) ;
    if h > 0 then
      w := length( _grd[0] )
    else
      w := 0 ;

    if (h + _y) > FHeight then
      h := FHeight - _y ;
    if (w + _x) > FWidth then
      w := FWidth - _x ;

    for i := 0 to h-1 do
      WriteRasterBandGrid( 1, _x, _y+i, w, 1, _grd[i], w, 1 ) ;
  end ;

  procedure TGIS_FileGDAL.OpenDataset(
    const _subdatasets : Boolean
  ) ;
  var
    lst : TStrings ;
    i   : Integer ;

    function get_name( const _sdataset : String ) : String ;
    var
      tkn : TGIS_Tokenizer ;
    begin
      Result := _sdataset ;

      tkn := TGIS_Tokenizer.Create ;
      try
        tkn.Execute( _sdataset, [':'], True ) ;
        if tkn.Result.Count = 3 then
          Result := tkn.Result[2] ;
      finally
        FreeObject( tkn ) ;
      end ;
    end ;

  begin
    if not initialize then
      throwInitializeError ;

    FDataset := T_GDALDataset.Create( Path, GA_ReadOnly, T_GDALLib(FLib) ) ;

    if not T_GDALDataset(FDataset).Open then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ) + #13#10 +
        Format( '%s', [T_GDALDataset(FDataset).LastError] ),
        Path, 0
      ) ;

    if not _subdatasets then exit ;
    
    lst := GetMetadata( 'SUBDATASETS' ) ;
    try
      if lst.Count > 0 then begin
        if IsStringEmpty( FSubDataset ) then begin
          FreeObject( FDataset ) ;

          FDataset := T_GDALDataset.Create( lst.ValueFromIndex[0], GA_ReadOnly, T_GDALLib(FLib) ) ;

          if not T_GDALDataset(FDataset).Open then
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ) + #13#10 +
              Format( '%s', [T_GDALDataset(FDataset).LastError] ),
              FSubDataset, 0
            ) ;
        end
        else begin
          for i := 0 to lst.Count-1 do begin
            if CompareText( get_name( lst.ValueFromIndex[i] ), FSubDataset ) = 0 then begin
              FreeObject( FDataset ) ;

              FDataset := T_GDALDataset.Create( lst.ValueFromIndex[i], GA_ReadOnly, T_GDALLib(FLib) ) ;

              if not T_GDALDataset(FDataset).Open then
                raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ) + #13#10 +
                  Format( '%s', [T_GDALDataset(FDataset).LastError] ),
                  FSubDataset, 0
                ) ;
              break ;
            end ;
          end ;
        end ;
      end ;
    finally
      FreeObject( lst ) ;
    end ;
  end ;

  function TGIS_FileGDAL.BuildDataset(
    const _isGrid : Boolean
  ) : Boolean ;
  var
    sdrv : String ;
    ext  : String ;
  begin
    ext  := GetFileExt( FPath ) ;
    sdrv := findDriver( ext ) ;

    if _isGrid then
      Result := createDataset( sdrv, 1, GDT_Float32 )
    else begin
      case SubFormat.PixelFormat of
        TGIS_PixelFormat.Bit1  : Result := createDataset( sdrv, 1, GDT_Byte ) ;
        TGIS_PixelFormat.Bit8  : Result := createDataset( sdrv, 1, GDT_Byte ) ;
        TGIS_PixelFormat.RGB   : Result := createDataset( sdrv, 3, GDT_Byte ) ;
        TGIS_PixelFormat.ARGB  : Result := createDataset( sdrv, 4, GDT_Byte )
      else                       Result := createDataset( sdrv, 0, GDT_Unknown ) ;
      end ;

    end ;
  end ;

  function TGIS_FileGDAL.CanCreateDataset(
    const _isGrid : Boolean
  ) : Boolean ;
  var
    ext  : String ;
    sdrv : String ;
    drv  : T_GDALDriver ;
  begin
    Result := False ;

    ext  := GetFileExt( FPath ) ;
    sdrv := findDriver( ext ) ;

    if not IsStringEmpty( sdrv ) then begin
      drv := T_GDALDriver.Create( sdrv, T_GDALLib(FLib) ) ;
      try
        Result := assigned( drv ) and drv.IsValid and drv.CanCreate ;
      finally
        FreeObject( drv ) ;
      end ;
    end ;
  end ;

  procedure TGIS_FileGDAL.GetDatasetProperties(
    var _xSize      : Integer ;
    var _ySize      : Integer ;
    var _csWkt      : String  ;
    var _extent     : TGIS_Extent ;
    var _scaleX     : Double ;
    var _scaleY     : Double ;
    var _rotateX    : Double ;
    var _rotateY    : Double ;
    var _bands      : Integer ;
    var _channels   : TGIS_IntegerArray
  ) ;
  var
    ds : T_GDALDataset ;
    i  : Integer ;
    c  : GDALColorInterp ;
  begin
    if not assigned( FDataset ) then exit ;

    ds := T_GDALDataset(FDataset) ;

    _xSize    := ds.XSize ;
    _ySize    := ds.YSize ;

    if not IsStringEmpty( ds.Projection ) then
      _csWkt := ds.Projection
    else if not IsStringEmpty( ds.ProjectionGC ) then
      _csWkt := ds.ProjectionGC
    else
      _csWkt := '' ;

    _scaleX := ds.ScaleX ;
    _scaleY := ds.ScaleY ;

    if _scaleX > 0 then begin
      _extent.XMin := ds.Origin.X ;
      _extent.XMax := _extent.XMin + _scaleX*_xSize ;
    end
    else begin
      _extent.XMin := ds.Origin.X ;
      _extent.XMax := _extent.XMin - _scaleX*_xSize ;
    end;

    if _scaleY > 0 then begin
      _extent.YMax := ds.Origin.Y ;
      _extent.YMin := _extent.YMax - _scaleY*_ySize ;
    end
    else begin
      _extent.YMax := ds.Origin.Y ;
      _extent.YMin := _extent.YMax + _scaleY*_ySize ;
    end ;

    _rotateX := ds.RotateX ;
    _rotateY := ds.RotateY ;

    _bands := ds.BandCount ;

    if (_bands=1) or (_bands=2) then begin
      // grayscale
      SetLength( _channels, 1 ) ;
      _channels[0] := 1 ;
    end
    else begin
      // RGB image
      SetLength( _channels, _bands ) ;
      if (_bands=3) then begin
        // RGB
        _channels[0] := 3 ;
        _channels[1] := 2 ;
        _channels[2] := 1 ;
      end
      else if (_bands=4) then begin
        // BGR...
        _channels[0] := 1 ;
        _channels[1] := 2 ;
        _channels[2] := 3 ;
        _channels[3] := -1 ;
      end
      else begin
        // Multiband
        for i := 0 to _bands-1 do
          _channels[i] := i+1 ;
      end ;

      // check to see if any bands have ColorInterp
      for i := 1 to _bands do begin
        c := ds.GetRasterBand(i).ColorInt ;
        if ( c = GCI_RedBand ) then
          _channels[2] := i
        else if ( c = GCI_GreenBand ) then
          _channels[1] := i
        else if ( c = GCI_BlueBand  ) then
          _channels[0] := i
        else if ( c = GCI_AlphaBand ) and ( _bands >=4 ) then
          _channels[3] := i ;
      end ;
    end ;
  end ;

  procedure TGIS_FileGDAL.GetRasterBandProperties(
    const _id         : Integer ;
      var _xSize      : Integer ;
      var _dataType   : String ;
      var _dataSize   : Integer ;
      var _zMin       : Single  ;
      var _zMax       : Single ;
      var _noData     : Single ;
      var _colorInt   : String ;
      var _colors     : Integer
  ) ;
  var
    band : T_GDALRasterBand ;
    ds   : T_GDALDataset ;
  begin
    if not assigned( FDataset ) then exit ;

    ds   := T_GDALDataset(FDataset) ;
    band := ds.GetRasterBand(_id) ;

    if assigned( band ) then begin
      _xSize    := band.XSize ;
      _dataType := band.DataTypeName ;
      _zMin     := band.MinValue ;
      _zMax     := band.MaxValue ;
      _noData   := band.NoDataValue ;
      _colorInt := band.ColorIntName ;
      _colors   := band.ColorTableCount ;
      _dataSize := band.DataTypeSize ;
    end ;
  end ;

  function TGIS_FileGDAL.GetRasterBandGetColorEntry(
    const _id         : Integer ;
    const _tabIndex   : Integer ;
    const _colorIndex : Integer
  ) : Byte ;
  var
    band : T_GDALRasterBand ;
    ds   : T_GDALDataset ;
  begin
    Result := 0 ;
    if not assigned( FDataset ) then exit ;

    ds   := T_GDALDataset(FDataset) ;
    band := ds.GetRasterBand(_id) ;

    if assigned( band ) then
      Result := band.GetColorEntry( _tabIndex, _colorIndex ) ;
  end ;

  procedure TGIS_FileGDAL.ReadRasterBandGrid(
    const _id         : Integer ;
    const _xOff       : Integer ;
    const _yOff       : Integer ;
    const _xSize      : Integer ;
    const _ySize      : Integer ;
    const _buffer     : TGIS_SingleArray ;
    const _bufXSize   : Integer ;
    const _bufYSize   : Integer
   ) ;
  var
    band : T_GDALRasterBand ;
    ds   : T_GDALDataset ;
    {$IFDEF CLR}
     gch : GCHandle ;
    {$ENDIF}
    {$IFDEF JAVA}
     gch : com.sun.jna.Pointer ;
    {$ENDIF}
  begin
    if not assigned( FDataset ) then exit ;

    ds   := T_GDALDataset(FDataset) ;
    band := ds.GetRasterBand(_id) ;

    {$IFDEF CLR}
     gch := GCHandle.Alloc(_buffer, GCHandleType.Pinned) ;
    {$ENDIF}
    {$IFDEF JAVA}
     gch := Marshal.AllocHGlobal( length(_buffer)*sizeOf(Single) ) ;
    {$ENDIF}

    if assigned( band ) then
      band.RasterIO( GF_Read, _xOff, _yOff, _xSize, _ySize,
                     {$IFDEF CLR}
                       gch.AddrOfPinnedObject(),
                     {$ENDIF}
                     {$IFDEF JAVA}
                       gch,
                     {$ENDIF}
                     {$IFDEF DCC}
                       {$IFDEF LEVEL_XE2_RTL}
                         IntPtr(@_buffer[0]),
                       {$ELSE}
                         @_buffer[0],
                       {$ENDIF}
                     {$ENDIF}
                     _bufXSize, _bufYSize, GDT_Float32
                    ) ;
    {$IFDEF JAVA}
      System.arraycopy( gch.getFloatArray( 0, length(_buffer)),  0, _buffer, 0, length(_buffer) ) ;
      Marshal.FreeHGlobal( gch ) ;
    {$ENDIF}
  end ;

  procedure TGIS_FileGDAL.ReadRasterBandRGB(
    const _id         : Integer ;
    const _xOff       : Integer ;
    const _yOff       : Integer ;
    const _xSize      : Integer ;
    const _ySize      : Integer ;
    const _buffer     : TBytes ;
    const _bufXSize   : Integer ;
    const _bufYSize   : Integer
   ) ;
  var
    band : T_GDALRasterBand ;
    ds   : T_GDALDataset ;
    {$IFDEF CLR}
     gch : GCHandle ;
    {$ENDIF}
    {$IFDEF JAVA}
     gch : com.sun.jna.Pointer ;
    {$ENDIF}
  begin
    if not assigned( FDataset ) then exit ;

    ds   := T_GDALDataset(FDataset) ;
    band := ds.GetRasterBand(_id) ;

    {$IFDEF CLR}
     gch := GCHandle.Alloc(_buffer, GCHandleType.Pinned) ;
    {$ENDIF}
    {$IFDEF JAVA}
     gch := Marshal.AllocHGlobal( length(_buffer) ) ;
    {$ENDIF}

    if assigned( band ) then begin
      band.RasterIO( GF_Read, _xOff, _yOff, _xSize, _ySize,
                    {$IFDEF CLR}
                       gch.AddrOfPinnedObject(),
                     {$ENDIF}
                     {$IFDEF JAVA}
                       gch,
                     {$ENDIF}
                     {$IFDEF DCC}
                       {$IFDEF LEVEL_XE2_RTL}
                         IntPtr(@_buffer[0]),
                       {$ELSE}
                         @_buffer[0],
                       {$ENDIF}
                     {$ENDIF}
                     _bufXSize,
                     _bufYSize,
                     band.DataType
                    ) ;
    end ;
    {$IFDEF JAVA}
      System.arraycopy( gch.getByteArray( 0, length(_buffer)),  0, _buffer, 0, length(_buffer) ) ;
      Marshal.FreeHGlobal( gch ) ;
    {$ENDIF}

  end ;

  procedure TGIS_FileGDAL.WriteRasterBandRGB(
    const _id         : Integer ;
    const _xOff       : Integer ;
    const _yOff       : Integer ;
    const _xSize      : Integer ;
    const _ySize      : Integer ;
    const _buffer     : TBytes ;
    const _bufXSize   : Integer ;
    const _bufYSize   : Integer
   ) ;
  var
    band : T_GDALRasterBand ;
    ds   : T_GDALDataset ;
    {$IFDEF CLR}
     gch : GCHandle ;
    {$ENDIF}
    {$IFDEF JAVA}
     gch : com.sun.jna.Pointer ;
    {$ENDIF}
  begin
    if not assigned( FDataset ) then exit ;

    ds   := T_GDALDataset(FDataset) ;
    band := ds.GetRasterBand(_id) ;

    {$IFDEF CLR}
     gch := GCHandle.Alloc(_buffer, GCHandleType.Pinned) ;
    {$ENDIF}
    {$IFDEF JAVA}
     gch := Marshal.AllocHGlobal( length(_buffer) ) ;
     gch.write( 0, _buffer, 0, length(_buffer) ) ;
    {$ENDIF}

    if assigned( band ) then
      band.RasterIO( GF_Write, _xOff, _yOff, _xSize, _ySize,
                    {$IFDEF CLR}
                       gch.AddrOfPinnedObject(),
                     {$ENDIF}
                     {$IFDEF JAVA}
                       gch,
                     {$ENDIF}
                     {$IFDEF DCC}
                       {$IFDEF LEVEL_XE2_RTL}
                         IntPtr(@_buffer[0]),
                       {$ELSE}
                         @_buffer[0],
                       {$ENDIF}
                     {$ENDIF}
                     _bufXSize, _bufYSize, GDT_Byte
                    ) ;
    {$IFDEF JAVA}
      Marshal.FreeHGlobal( gch ) ;
    {$ENDIF}

  end ;

  procedure TGIS_FileGDAL.WriteRasterBandGrid(
    const _id         : Integer ;
    const _xOff       : Integer ;
    const _yOff       : Integer ;
    const _xSize      : Integer ;
    const _ySize      : Integer ;
    const _buffer     : TGIS_SingleArray ;
    const _bufXSize   : Integer ;
    const _bufYSize   : Integer
   ) ;
  var
    band : T_GDALRasterBand ;
    ds   : T_GDALDataset ;
    {$IFDEF CLR}
     gch : GCHandle ;
    {$ENDIF}
    {$IFDEF JAVA}
     gch : com.sun.jna.Pointer ;
    {$ENDIF}
  begin
    if not assigned( FDataset ) then exit ;

    ds   := T_GDALDataset(FDataset) ;
    band := ds.GetRasterBand(_id) ;

    {$IFDEF CLR}
     gch := GCHandle.Alloc(_buffer, GCHandleType.Pinned) ;
    {$ENDIF}
    {$IFDEF JAVA}
     gch := Marshal.AllocHGlobal( length(_buffer)*sizeOf(Single) ) ;
     gch.write( 0, _buffer, 0, length(_buffer) ) ;
    {$ENDIF}

    if assigned( band ) then
      band.RasterIO( GF_Write, _xOff, _yOff, _xSize, _ySize,
                    {$IFDEF CLR}
                       gch.AddrOfPinnedObject(),
                     {$ENDIF}
                     {$IFDEF JAVA}
                       gch,
                     {$ENDIF}
                     {$IFDEF DCC}
                       {$IFDEF LEVEL_XE2_RTL}
                         IntPtr(@_buffer[0]),
                       {$ELSE}
                         @_buffer[0],
                       {$ENDIF}
                     {$ENDIF}
                     _bufXSize, _bufYSize, GDT_Float32
                    ) ;
    {$IFDEF JAVA}
      Marshal.FreeHGlobal( gch ) ;
    {$ENDIF}
  end ;

  function TGIS_FileGDAL.PreRecognize : Boolean ;
  var
    {$IFDEF JAVA}
    hdl : com.sun.jna.Pointer ;
    {$ELSE}
    hdl : IntPtr ;
    {$ENDIF}
  begin
    if IsStringEmpty( FDLLPath ) then
      {$IFDEF JAVA}
      FDLLPath := GDAL_DEFAULT_DLL_NAME_NOEXT ;
      {$ELSE}
      FDLLPath := GDAL_DEFAULT_DLL_NAME ;
      {$ENDIF}

    FLib := T_GDALLib.Create ;
    try
      if not T_GDALLib(FLib).LoadDLL( FDLLPath ) then begin
        Result := False ;
        exit ;
      end ;

      T_GDALLib(FLib).GDALAllRegister ;
      hdl := T_GDALLib(FLib).GDALOpen( toAPIEx( FPath ), GA_ReadOnly ) ;

      {$IFDEF JAVA}
        Result := hdl <> nil ;
      {$ELSE}
        {$IFDEF LEVEL_XE2_RTL}
          Result := hdl <> IntPtr(0)   ;
        {$ELSE}
          Result := AssignedPtr( hdl ) ;
        {$ENDIF}
      {$ENDIF}
      if Result then
        T_GDALLib(FLib).GDALClose( hdl ) ;
    finally
      FreeObject( FLib ) ;
    end ;
  end ;

  function TGIS_FileGDAL.GetInfo : String ;
  begin
    if assigned( FDataset ) then
      Result := T_GDALDataset(FDataset).GetInfo ;
  end ;

  {$IFDEF OXYGENE}
    function TGIS_FileGDAL.GetAvailableFormats : TGIS_Strings ;
  {$ELSE}
    function TGIS_FileGDAL.GetAvailableFormats : TStrings ;
  {$ENDIF}
  var
    i, cnt    : Integer ;
    drv, drvR : T_GDALDriver ;
    flag      : String ;
  begin
    Result := TStringList.Create ;

    if not initialize then exit ;

    drvR := T_GDALDriver.Create( '', T_GDALLib(FLib) ) ;
    try
      cnt := drvR.GetDriverCount ;

      for i := 0 to cnt-1 do begin
        drv := drvR.GetDriver(i) ;
        try
          if assigned( drv ) then begin
            if drv.IsReadOnly then
              flag := 'ro'
            else if drv.CanCreate then
              flag := 'rw+'
            else if drv.CanCreateCopy then
              flag := 'rw' ;

            Result.AddObject( Format( '[%s] %s (.%s | %s)',
                              [ drv.ShortName, drv.LongName, drv.Extension, flag ] ),
                              TObject(  drv.CanCreate )
                             ) ;
          end;
        finally
          FreeObject( drv ) ;
        end ;
      end ;
    finally
      FreeObject( drvR ) ;
    end;
  end ;

 {$IFDEF OXYGENE}
   function TGIS_FileGDAL.GetMetadata(
    const _domain : String
   ) : TGIS_Strings ;
 {$ELSE}
   function TGIS_FileGDAL.GetMetadata(
    const _domain : String
   ) : TStrings     ;
 {$ENDIF}
  var
    buf : IntPtr ;
    cnt : Integer ;
    i   : Integer ;
 begin
   Result := TStringList.Create ;

   if not initialize then exit ;

   if T_GDALDataset(FDataset).IsLibrary and T_GDALDataset(FDataset).IsValid then begin
    {$IFDEF LEVEL_XE2_RTL}
      buf := IntPtr( T_GDALLib(FLib).GDALGetMetadata(
                T_GDALDataset(FDataset).Handle,
                toAPI( _domain )
               )
              ) ;
    {$ELSE}
      buf := T_GDALLib(FLib).GDALGetMetadata(
                T_GDALDataset(FDataset).Handle,
                toAPI( _domain )
              ) ;
    {$ENDIF}
    {$IFDEF LEVEL_XE2_RTL}
      if buf <> IntPtr(0)   then begin
    {$ELSE}
      if AssignedPtr( buf ) then begin
    {$ENDIF}
      cnt := T_GDALLib(FLib).CSLCount( buf ) ;
      for i := 0 to cnt-1 do
        Result.Add( asString( T_GDALLib(FLib).CSLGetField( buf, i ) ) ) ;
    end ;
  end ;

  end ;

 {$IFDEF OXYGENE}
   function TGIS_FileGDAL.GetCreateOptions : TGIS_Strings ;
 {$ELSE}
   function TGIS_FileGDAL.GetCreateOptions : TStrings     ;
 {$ENDIF}
  begin
    Result := FCreateOptions ;
  end ;

  procedure TGIS_FileGDAL.SetConfigOption(
    const _key : String ;
    const _val : String
  ) ;
  begin
    if not initialize then exit ;

    T_GDALLib(FLib).CPLSetConfigOption( toAPI( _key ), toAPI( _val ) ) ;
  end ;

  procedure TGIS_FileGDAL.SetCreateOption(
    const _key : String ;
    const _val : String
  ) ;
  begin
    if not initialize then exit ;

    FCreateOptions.Values[ _key ] := _val ;
  end ;

  type
    T_SelfRef = record
      FOwner : TGIS_FileGDAL ;
    end ;

  {$IFDEF OXYGENE}
  function progressFunc(
    dfComplete    : Double ;
    pszMessage    : String ;
    pProgressArg  : IntPtr
   ) : Integer  ;
  {$ELSE}
  function progressFunc(
    dfComplete    : Double ;
    pszMessage    : PAnsiChar ;
    pProgressArg  : IntPtr
   ) : Integer  ; stdcall ;
  {$ENDIF}
  var
    abrt : Boolean ;
    {$IFNDEF OXYGENE}
      r  : T_SelfRef ;
    {$ENDIF}
  begin
    Result := 1 ;
    {$IFDEF LEVEL_XE2_RTL}
      if pProgressArg <> IntPtr(0)   then begin
    {$ELSE}
      if AssignedPtr( pProgressArg ) then begin
    {$ENDIF}
      {$IFDEF OXYGENE}
      // r := T_SelfRef(Marshal.PtrToStructure( pProgressArg, typeOf(T_SelfRef) ) ) ;
      {$ELSE}
      r := T_SelfRef(Pointer(pProgressArg)^) ;
      {$ENDIF}
      abrt := False ;
      {$IFDEF OXYGENE}
        { TODO -cEvaluate : Review }
        // if assigned( r.FOwner ) and assigned( r.FOwner.Busy ) and (dfComplete>0) then
        //   r.FOwner.Busy( nil, RoundS(dfComplete * 100), 100, abrt ) ;
      {$ELSE}
        if assigned( r.FOwner ) and assigned( r.FOwner.BusyEvent ) and (dfComplete>0) then
          r.FOwner.BusyEvent( nil, RoundS(dfComplete * 100), 100, abrt ) ;
      {$ENDIF}
      if abrt then
        Result := 0 ;
    end ;
  end;

  procedure TGIS_FileGDAL.CreateCopy(
    const _destFileName : String ;
    const _destDriver   : String ;
    const _strict       : Boolean ;
    {$IFDEF OXYGENE}
      const _options    : TStrings
    {$ELSE}
      const _options    : TStrings
    {$ENDIF}
   ) ;
  var
    drv     : T_GDALDriver ;
    fstrict : Integer ;
    {$IFDEF JAVA}
    newds   : com.sun.jna.Pointer  ;
    options : com.sun.jna.Pointer ;
    {$ELSE}
    newds   : IntPtr ;
    options : IntPtr ;
    {$ENDIF}
    i       : Integer ;
    rself   : T_SelfRef ;
    {$IFDEF OXYGENE}
      rptr  : IntPtr ;
    {$ELSE}
      fptr  : IntPtr ;
    {$ENDIF}
    {$IFNDEF OXYGENE}
      abrt  : Boolean ;
    {$ENDIF}
  begin
    if not initialize then exit ;

    {$IFDEF LEVEL_XE2_RTL}
      options := IntPtr(0) ;
    {$ELSE}
      options := nil ;
    {$ENDIF}

    drv := T_GDALDriver.Create( _destDriver, T_GDALLib(FLib) ) ;
    try
      if not drv.IsValid then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ) + #13#10 +
          'Invalid driver name',
          Path, 0
        ) ;

      if _strict then
        fstrict := 1
      else
        fstrict := 0 ;

      if assigned( _options ) then
        for i := 0 to _options.Count-1 do
          options := T_GDALLib(FLib).CSLAddString( options, toAPI( _options[i] ) ) ;

      rself.FOwner := Self ;
      {$IFDEF OXYGENE}
        { TODO -cEvaluate : Review }
      {$ELSE}
      abrt := False ;
      if assigned( BusyEvent ) then
        BusyEvent( nil, -1, 0, abrt ) ;
      {$ENDIF}

      {$IFDEF OXYGENE}
        //Marshal.StructureToPtr(rself, rptr, False ) ;
        //fptr := Marshal.GetFunctionPointerForDelegate( progressFunc ) ;

        newds := T_GDALLib(FLib).GDALCreateCopy(
                   drv.Handle,
                   toAPI( _destFileName ),
                   T_GDALDataset(FDataset).Handle,
                   fstrict,
                   options,
                   nil,
                   rptr
                 ) ;
      {$ELSE}
        {$IFDEF LEVEL_XE2_RTL}
          fptr  := NativeInt( @rself ) ;
        {$ELSE}
          fptr  := @rself ;
        {$ENDIF}
        newds := T_GDALLib(FLib).GDALCreateCopy(
                   drv.Handle,
                   toAPI( _destFileName ),
                   T_GDALDataset(FDataset).Handle,
                   fstrict,
                   options,
                   progressFunc,
                   fptr
                 ) ;
      {$ENDIF}

      {$IFDEF JAVA}
        if newds <> nil then
      {$ELSE}
        {$IFDEF LEVEL_XE2_RTL}
          if newds <> IntPtr(0) then
        {$ELSE}
          if AssignedPtr( newds ) then
        {$ENDIF}
      {$ENDIF}
          T_GDALLib(FLib).GDALClose( newds )
        else
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ) + #13#10 +
            Format( '%s', [T_GDALDataset(FDataset).LastError] ),
            Path, 0
          ) ;
    finally
      T_GDALLib(FLib).CSLDestroy( options ) ;
      FreeObject( drv ) ;
      {$IFDEF OXYGENE}
        { TODO -cEvaluate : Review }
      {$ELSE}
      if assigned( BusyEvent ) then
        BusyEvent( nil, -1, 0, abrt ) ;
      {$ENDIF}
    end ;
  end ;

//==================================== END =====================================
end.

