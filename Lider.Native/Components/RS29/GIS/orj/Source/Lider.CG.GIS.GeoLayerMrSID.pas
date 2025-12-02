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
  Encapsulation of a MrSID Layer (LizardTech's MrSID Technology).
}

{$IFDEF DCC}
  unit GisLayerMrSID ;
  {$HPPEMIT '#pragma link "GisLayerMrSID"'}
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
    System.Security,
    System.Runtime.InteropServices,
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}

    System.SysUtils,
    System.Classes,
    System.Math,

    GisTypes,
    GisTypesUI,
    GisFunctions,
    GisResource,
    GisLayerPixel ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerMrSID = class
    public
      // Perform initialization section.
      class procedure SelfRegisterLayer() ;
  end;

  /// <summary>
  ///   Encapsulation of MrSID layer.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     LIZARDTECH LICENSE.  The SOFTWARE may contain MrSID format decode
  ///     technology which is owned by Lizard Tech. Under terms of a LIZARDTECH
  ///     Decode SDK license agreement, LizardTech grants TatukGIS with a
  ///     non-exclusive, worldwide, non-transferable right to distribute in
  ///     object code format those portions of the Decode SDK necessary to
  ///     enable end-user viewing of *.sid-formatted files from within TatukGIS
  ///     products. In addition, the LIZARDTECH decode SDK license grants
  ///     TatukGIS with a non-exclusive, worldwide, non-transferable right to
  ///     distribute DSDK runtime .dll files, to enable licensed users of
  ///     TatukGIS DEVELOPMENT TOOL products to implement end-user viewing of
  ///     *.sid-formatted files in custom applications created from the
  ///     TatukGIS DEVELOPMENT TOOL products.
  ///   </para>
  ///   <para>
  ///     Password event will be fired upon connecting to layer to resolve
  ///     'password' key.
  ///   </para>
  /// </remarks>
  TGIS_LayerMrSID = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerPixel )

    private // various private values

        /// <summary>
        ///   Width of band in pixels.
        /// </summary>
        totalBitWidth  : Integer ;

        /// <summary>
        ///   Width of layer in bytes.
        /// </summary>
        totalLineWidth : Integer ;

        /// <summary>
        ///   Strip height - maximum number of decompressed lines
        /// </summary>
        stripHeight : Integer ;

        /// <summary>
        ///   First line in a strip
        /// </summary>
        stripStart : Integer ;

        /// <summary>
        ///   Actual number of decompressed lines
        /// </summary>
        stripLinesNo : Integer ;

        /// <summary>
        ///   Indicator of MrSID library loading.
        /// </summary>
        libLoaded : Boolean ;

        /// <summary>
        ///   Strip buffer
        /// </summary>
        {$IFDEF CLR}
        stripBuffer : IntPtr ;
        {$ENDIF}
        {$IFDEF JAVA}
        stripBuffer : com.sun.jna.Pointer ;
        {$ENDIF}
        {$IFDEF DCC}
        stripBuffer : TBytes ;
        {$ENDIF}

        /// <summary>
        ///   Strip buffer size
        /// </summary>
        stripBufferSize : Int64 ;

        /// <summary>
        ///   Line width to four bytes aligned
        /// </summary>
        lineBytesWidth : Integer ;

        /// <summary>
        ///   Zoom in lastreading
        /// </summary>
        lastZoom : Integer ;

        /// <summary>
        ///   Last used in setZoom function
        /// </summary>
        actualZoom : Integer ;

        /// <summary>
        ///   Last set view rectangle
        /// </summary>
        lastviewrect  : TRect    ;

        xLeft : Integer ;

        /// <summary>
        ///   Width of actual window - in pixels
        /// </summary>
        actualWidth : Integer ;

        /// <summary>
        ///   Maximal zoom level
        /// </summary>
        maxZoomLevel : Integer ;

        /// <summary>
        ///   Instance identifier from DLL
        /// </summary>
        instanceDLL : Integer ;

        /// <summary>
        ///   MrSID color space identifier (RGB, CMYK etc.)
        /// </summary>
        colorSpace : Integer ;

      /// <summary>
      ///   MrSID dat type identifier (number of bytes etc.).
      /// </summary>
      dataType : Integer ;
      FFileInfoEx : String ;
      stmBuf  : TBytes ;

    private // dll functions

    private // various private values

      /// <summary>
      ///   Load DLL and bind functions.
      /// </summary>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FILEREAD
      /// </exception>
      procedure bindLibrary   ;

      /// <summary>
      ///   Reinitialize the library.
      /// </summary>
      procedure reloadLibrary ;

      /// <summary>
      ///   Read geotiff info.
      /// </summary>
      procedure readGeoTiff ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF} // various protected routines

      /// <inheritdoc/>
      procedure setUp       ; override;

      /// <inheritdoc/>
      procedure setupParams ; override;

      /// <inheritdoc/>
      function  setFileScale( const _dwidth : Double ;
                              const _swidth : Double
                            ) : Double ; override;

      /// <inheritdoc/>
      procedure setFileView ( const _viewRect    : TRect) ; override;

      /// <inheritdoc/>
      function  getLine     ( const _buffer : TBytes  ;
                              const _offset : Integer ;
                              const _linenr : Integer ;
                              const _start  : Integer ;
                              const _bytes  : Integer
                            ) : Integer; override;

      /// <inheritdoc/>
      function  getLinePixels ( const _buffer   : TGIS_Pixels  ;
                                const _offset   : Integer ;
                                const _linenr   : Integer ;
                                const _pixStart : Integer ;
                                const _pixCount : Integer
                              ) : Integer; override;

      /// <inheritdoc/>
      function  getNativeLine ( const _buffer   : TGIS_SingleArray ;
                                const _linenr   : Integer          ;
                                const _startIdx : Integer          ;
                                const _count    : Integer
                              ) : Integer ; override;

    public // various public routines

      /// <inheritdoc/>
      procedure Alive             ; override;

      /// <inheritdoc/>
      function  DormantGain       : Integer ; override;

      /// <inheritdoc/>
      procedure Dormant           ; override;


      {#gendoc:hide:GENPDK} { TODO -cPDK : Verify }
      /// <summary>
      ///   Decode image to the buffer.
      /// </summary>
      /// <param name="_buffer">
      ///   allocated buffer that will be filled with decoded image
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Only for internal use of TatukGIS.
      ///    </note>
      /// </remarks>
      procedure DecodeImageInternal( const _buffer : TBytes
                                   ) ;
    protected

      /// <inheritdoc/>
      procedure doDestroy ; override;
    public // API
      // constructors

      /// <inheritdoc/>
      constructor Create  ; override;

  end ;

{$IFDEF OXYGENE}
  function  fnSetUp       ( _key          : String
                          ) : Integer ;
  function  fnInit        ( _sidFilePath  : String ;
                            _password     : String ;
                            _data         : TBytes = nil ;
                            _size         : Integer = 0
                          ) : Integer ;
  function  fnImageHeight ( _instance     : Integer
                          ) : Integer ;
  function  fnImageWidth  ( _instance     : Integer
                          ) : Integer ;
  function  fnNumBands    ( _instance     : Integer
                          ) : Integer ;
  function  fnDataType    ( _instance     : Integer
                          ) : Integer ;
  function  fnColorSpace  ( _instance     : Integer
                          ) : Integer ;
  function  fnImageResX   ( _instance     : Integer
                          ) : Double  ;
  function  fnImageResY   ( _instance     : Integer
                          ) : Double  ;
  function  fnImageOriginX( _instance     : Integer
                          ) : Double  ;
  function  fnImageOriginY( _instance     : Integer
                          ) : Double  ;
  function  fnSetZoom     ( _instance     : Integer ;
                            _zoomLevel    : Integer
                          ) : Integer ;
  function  fnMaxZoomLevel( _instance     : Integer
                          ) : Integer ;
  function  fnMaxValue    ( _instance     : Integer
                          ) : Single ;
  function  fnMinValue    ( _instance     : Integer
                          ) : Single ;
  function  fnNoDataValue ( _instance     : Integer
                          ) : Single ;
  function  fnSetGridBand ( _instance     : Integer ;
                            _gridBand     : SmallInt
                          ) : Integer ;
  procedure fnLoadStrip   ( _instance     : Integer ;
                            _lineStart    : Integer ;
                            _lines        : Integer ;
                            _xLeft        : Integer ;
                            _width        : Integer
                          ) ;
  procedure fnSetBuffer   ( _instance     : Integer ;
                            {$IFDEF CLR}
                            _imgBuffer    : IntPtr
                            {$ENDIF}
                            {$IFDEF JAVA}
                            _imgBuffer    : com.sun.jna.Pointer
                            {$ENDIF}
                          ) ;
  procedure fnSetBands    ( _instance     : Integer ;
                            _redBand      : Word    ;
                            _greenBand    : Word    ;
                            _blueBand     : Word
                          ) ;
  procedure fnFree        ( _instance     : Integer
                          ) ;
  {$IFDEF JAVA}
  function  fnGetMetadata ( _instance       : Integer ;
                            _key            : String ;
                            var _value      : TObject ;
                            _length         : Integer
                          ) : Integer ;
  {$ENDIF}
  {$IFDEF CLR}
  function  fnGetMetadata ( _instance       : Integer ;
                            _key            : String ;
                            var _value      : Integer ;
                            _length         : Integer
                          ) : Integer ; overload;
  function  fnGetMetadata ( _instance       : Integer ;
                            _key            : String ;
                            var _value      : Double ;
                            _length         : Integer
                          ) : Integer ; overload;
  function  fnGetMetadata ( _instance       : Integer ;
                            _key            : String ;
                            _value          : TBytes ;
                            _length         : Integer
                          ) : Integer ; overload;
  {$ENDIF}
  function  fnGetVersion  : Integer ;

{$ENDIF}


//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    GisRtl,
    GisClasses,
    GisInternals,
    GisRegistredLayers,
    GisCsBase,
    GisCsSystems,
    GisCsFactory,
    GisCsProjections ;
{$ENDIF}

  {$IFDEF DCC}
  type
    // MrSID Library interface functions types.
    T_SIDSetUp        = function ( _key            : PAnsiChar
                                 )  : Integer ; cdecl ;
    // MrSID Library interface functions types.
    T_SIDInit         = function ( _sidFilePath    : PAnsiChar   ;
                                   _password       : PAnsiChar   ;
                                   _data           : PByte = nil ;
                                   _size           : Integer = 0
                                 ) : Integer ; cdecl ;
    // MrSID Library interface functions types.
    T_SIDFree         = procedure( _instance       : Integer
                                 ) ; cdecl ;
    // MrSID Library interface functions types.
    T_SIDImageWidth   = function ( _instance       : Integer
                                 ) : Integer ; cdecl ;
    // MrSID Library interface functions types.
    T_SIDImageHeight  = function ( _instance       : Integer
                                 ) : Integer ; cdecl ;
    // MrSID Library interface functions types.
    T_SIDNumBands     = function ( _instance       : Integer
                                 ) : Integer ; cdecl ;
    // MrSID Library interface functions types.
    T_SIDDataType     = function ( _instance       : Integer
                                 ) : Integer ; cdecl ;
    // MrSID Library interface functions types.
    T_SIDColorSpace   = function ( _instance       : Integer
                                 ) : Integer ; cdecl ;
    // MrSID Library interface functions types.
    T_SIDImageResX    = function ( _instance       : Integer
                                 ) : Double  ; cdecl ;
    // MrSID Library interface functions types.
    T_SIDImageResY    = function ( _instance       : Integer
                                 ) : Double  ; cdecl ;
    // MrSID Library interface functions types.
    T_SIDImageOriginX = function ( _instance       : Integer
                                 ) : Double  ; cdecl ;
    // MrSID Library interface functions types.
    T_SIDImageOriginY = function ( _instance       : Integer
                                 ) : Double  ; cdecl ;
    // MrSID Library interface functions types.
    T_SIDSetBuffer    = procedure ( _instance      : Integer ;
                                    _imgBuffer     : PByte
                                  ) ; cdecl ;
    // MrSID Library interface functions types.
    T_SIDSetBands     = procedure( _instance       : Integer ;
                                   _redBand        : Word    ;
                                   _greenBand      : Word    ;
                                   _blueBand       : Word
                                 ) ; cdecl ;
    // MrSID Library interface functions types.
    T_SIDMaxZoomLevel = function ( _instance       : Integer
                                 ) : Integer  ; cdecl ;
    // MrSID Library interface functions types.
    T_SIDMaxValue     = function ( _instance       : Integer
                                 ) : Single ; cdecl ;
    // MrSID Library interface functions types.
    T_SIDMinValue     = function ( _instance       : Integer
                                 ) : Single ; cdecl ;
    // MrSID Library interface functions types.
    T_SIDNoDataValue  = function ( _instance       : Integer
                                 ) : Single ; cdecl ;
    // MrSID Library interface functions types.
    T_SIDSetGridBand  = function ( _instance       : Integer ;
                                   _gridBand       : SmallInt
                                 ) : Integer ; cdecl ;

    // MrSID Library interface functions types.
    T_SIDLoadStrip    = procedure( _instance       : Integer ;
                                   _lineStart      : Integer;
                                   _lines          : Integer;
                                   _xLeft          : Integer ;
                                   _width          : Integer
                                 ) ; cdecl ;
    // MrSID Library interface functions types.
    T_SIDSetZoom      = function ( _instance       : Integer ;
                                   _zoomLevel      : Integer
                                 ) : Integer ; cdecl ;
    // MrSID Library interface functions types.
    T_SIDGetMetadata  = function ( _instance       : Integer ;
                                   _key            : PAnsiChar ;
                                   _value          : Pointer ;
                                   _length         : Integer
                                 ) : Integer ; cdecl ;
    // MrSID Library interface functions types.
    T_SIDGetVersion   = function : Integer ; cdecl ;

{$ENDIF}

const
  // MrSID licens
  KEY_MRSID           = 'D1291359C66BA8423135218CA310C91E' ;
  MRSID_LIBVERSION    = 4045 ;

  // MrSID library nam
  SID_LIBNAME_NOEXT   = 'ttkMrSID95';
  SID_LIBNAME         = 'ttkMrSID95.dll';

  // DLL interface.
  SID_FUNC_SETUP      = 'ttkMrSID_SetUp'        ;
  SID_FUNC_INIT       = 'ttkMrSID_InitSID'      ;
  SID_PROC_FREE       = 'ttkMrSID_FreeSID'      ;
  SID_FUNC_IMGWIDTH   = 'ttkMrSID_ImageWidth'   ;
  SID_FUNC_IMGHEIGHT  = 'ttkMrSID_ImageHeight'  ;
  SID_FUNC_NUMBANDS   = 'ttkMrSID_NumBands'     ;
  SID_FUNC_DATATYPE   = 'ttkMrSID_DataType'     ;
  SID_FUNC_COLORSPACE = 'ttkMrSID_ColorSpace'   ;
  SID_FUNC_IMGRESX    = 'ttkMrSID_ImageResX'    ;
  SID_FUNC_IMGRESY    = 'ttkMrSID_ImageResY'    ;
  SID_FUNC_IMGORIGINX = 'ttkMrSID_ImageOriginX' ;
  SID_FUNC_IMGORIGINY = 'ttkMrSID_ImageOriginY' ;
  SID_PROC_SETBUFFER  = 'ttkMrSID_SetBuffer'    ;
  SID_PROC_SETBANDS   = 'ttkMrSID_SetBands'     ;

  SID_PROC_SETBGRIDBAND = 'ttkMrSID_SetGridBand'  ;
  SID_PROC_MAXZOOMLEVEL = 'ttkMrSID_MaxZoomLevel' ;
  SID_PROC_MAXVALUE     = 'ttkMrSID_MaxValue'     ;
  SID_PROC_MINVALUE     = 'ttkMrSID_MinValue'     ;
  SID_PROC_NODATAVALUE  = 'ttkMrSID_NoDataValue'  ;

  SID_PROC_LOADSTRIP  = 'ttkMrSID_LoadStrip'    ;
  SID_FUNC_SETZOOM    = 'ttkMrSID_SetZoom'      ;
  SID_FUNC_GETMETADATA  = 'ttkMrSID_GetMetadata'  ;
  SID_FUNC_GETVERSION   = 'ttkMrSID_Version' ;

  // Color space
  SID_COLORSPACE_RGB            = 1 ;
  SID_COLORSPACE_CMYK           = 3 ;
  SID_COLORSPACE_GRAYSCALE      = 4 ;
  SID_COLORSPACE_MULTISPECTRAL  = 8 ;

  // Color space

  SID_DATATYPE_INVALID  = 0 ;
  SID_DATATYPE_UINT8    = 1 ;
  SID_DATATYPE_SINT8    = 2 ;
  SID_DATATYPE_UINT16   = 3 ;
  SID_DATATYPE_SINT16   = 4 ;
  SID_DATATYPE_UINT32   = 5 ;
  SID_DATATYPE_SINT32   = 6 ;
  SID_DATATYPE_FLOAT32  = 7 ;
  SID_DATATYPE_FLOAT64  = 8 ;

  // Maximal buffer size for keeping decompressed line
  SID_MAX_STRIP_BUFFER_SIZE = 1024000 * 16 ;

  // extension of world file (extension file for pixel layer).
  WORLD_FILE_EXT_SID          = '.sid'  ;
  WORLD_FILE_EXT_SID_TFW      = '.sdw'  ;
  WORLD_FILE_EXT_SID_TFW2     = '.sidw' ;

  WORLD_FILE_EXT_SID_JPF      = '.jpf'  ;
  WORLD_FILE_EXT_SID_JPF_TFW  = '.jfw'  ;
  WORLD_FILE_EXT_SID_JPF_TFW2 = '.jpfw' ;

  WORLD_FILE_EXT_SID_JPX      = '.jpx'  ;
  WORLD_FILE_EXT_SID_JPX_TFW  = '.jxw'  ;
  WORLD_FILE_EXT_SID_JPX_TFW2 = '.jpxw' ;

  WORLD_FILE_EXT_SID_JPC      = '.jpc'  ;
  WORLD_FILE_EXT_SID_JPC_TFW  = '.jcw'  ;
  WORLD_FILE_EXT_SID_JPC_TFW2 = '.jpcw' ;

  WORLD_FILE_EXT_SID_J2C      = '.j2c'  ;
  WORLD_FILE_EXT_SID_J2C_TFW  = '.jcw'  ;
  WORLD_FILE_EXT_SID_J2C_TFW2 = '.j2cw' ;

  WORLD_FILE_EXT_SID_J2X      = '.j2x'  ;
  WORLD_FILE_EXT_SID_J2X_TFW  = '.jxw'  ;
  WORLD_FILE_EXT_SID_J2X_TFW2 = '.j2xw' ;

  WORLD_FILE_EXT_SID_J2K      = '.j2k'  ;
  WORLD_FILE_EXT_SID_J2K_TFW  = '.jkw'  ;
  WORLD_FILE_EXT_SID_J2K_TFW2 = '.j2kw' ;

  WORLD_FILE_EXT_SID_JP2      = '.jp2'  ;
  WORLD_FILE_EXT_SID_JP2_TFW  = '.j2w'  ;
  WORLD_FILE_EXT_SID_JP2_TFW2 = '.jp2w' ;

  //Coordinate Transformation Codes
  SID_CT_TRANSVERSEMERCATOR                   = 1 ;
  SID_CT_TRANSVERSEMERCATOR_MODIFIED_ALASKA   = 2 ;
  SID_CT_OBLIQUEMERCATOR                      = 3 ;
  SID_CT_OBLIQUEMERCATOR_LABORDE              = 4 ;
  SID_CT_OBLIQUEMERCATOR_ROSENMUND            = 5 ;
  SID_CT_OBLIQUEMERCATOR_SPHERICAL            = 6 ;
  SID_CT_MERCATOR                             = 7 ;
  SID_CT_LAMBERTCONFCONIC_2SP                 = 8 ;
  SID_CT_LAMBERTCONFCONIC_1SP                 = 9 ;
  SID_CT_LAMBERTAZIMEQUALAREA                 = 10 ;
  SID_CT_ALBERSEQUERAREA                      = 11 ;
  SID_CT_AZIMUTHALEQUIDISTANT                 = 12 ;
  SID_CT_EQUIDISTANTCONIC                     = 13 ;
  SID_CT_STEREOGRAPHIC                        = 14 ;
  SID_CT_POLARSTEREOGRAPHIC                   = 15 ;
  SID_CT_OBLIQUESTEREOGRAPHIC                 = 16 ;
  SID_CT_EQUIRECTANGULAR                      = 17 ;
  SID_CT_CASSINISOLDNER                       = 18 ;
  SID_CT_GNOMONIC                             = 19 ;
  SID_CT_MILLERCYLINDRICAL                    = 20 ;
  SID_CT_ORTHOGRAPHIC                         = 21 ;
  SID_CT_POLYCONIC                            = 22 ;
  SID_CT_ROBINSON                             = 23 ;
  SID_CT_SINUSOIDAL                           = 24 ;
  SID_CT_VANDERGRINTEN                        = 25 ;
  SID_CT_NEWZEALANDMAPGRID                    = 26 ;
  SID_CT_TRANSVMERCATOR_SOUTHORIENTED         = 27 ;

  {$IFDEF JAVA}
  type

    IMrSIDLibrary = interface (com.sun.jna.Library)
      function ttkMrSID_SetUp( _key : String
                              ) : Integer ;
      function ttkMrSID_InitSID( _sidFilePath : String  ;
                                 _password    : String  ;
                                 _data        : TBytes  ;
                                 _size        : Integer
                               ) : Integer ;
      function ttkMrSID_ImageHeight( _instance : Integer
                                    ) : Integer ;
      function ttkMrSID_ImageWidth( _instance : Integer
                                   ) : Integer ;
      function ttkMrSID_NumBands( _instance : Integer
                                 ) : Integer ;
      function ttkMrSID_DataType( _instance : Integer
                                 ) : Integer ;
      function ttkMrSID_ColorSpace( _instance : Integer
                                   ) : Integer ;
      function ttkMrSID_ImageResX( _instance : Integer
                                  ) : Double ;
      function ttkMrSID_ImageResY( _instance : Integer
                                  ) : Double ;
      function ttkMrSID_ImageOriginX( _instance : Integer
                                    ) : Double ;
      function ttkMrSID_ImageOriginY( _instance : Integer
                                    ) : Double ;
      function ttkMrSID_SetZoom( _instance  : Integer ;
                                 _zoomLevel : Integer
                               ) : Integer ;
      procedure ttkMrSID_LoadStrip( _instance  : Integer ;
                                    _lineStart : Integer ;
                                    _lines     : Integer ;
                                    _xLeft     : Integer ;
                                    _width     : Integer
                                   ) ;
      procedure ttkMrSID_SetBuffer( _instance  : Integer ;
                                    _imgBuffer : com.sun.jna.Pointer
                                  ) ;
      procedure ttkMrSID_SetBands( _instance  : Integer ;
                                   _redBand   : Word    ;
                                   _greenBand : Word    ;
                                   _blueBand  : Word
                                 ) ;
      function ttkMrSID_SetGridBand( _instance : Integer  ;
                                     _gridBand : SmallInt
                                   ) : Integer ;
      function ttkMrSID_MaxZoomLevel( _instance : Integer
                                     ) : Integer ;
      function ttkMrSID_MaxValue( _instance : Integer
                                ) : Single ;
      function ttkMrSID_MinValue( _instance : Integer
                                 ) : Single ;
      function ttkMrSID_NoDataValue( _instance : Integer
                                    ) : Single ;
      function ttkMrSID_GetMetadata(      _instance : Integer ;
                                          _key      : String ;
                                          _value    : com.sun.jna.Pointer ;
                                          _length   : Integer
                                    ) : Integer ;
      procedure ttkMrSID_FreeSID( _instance     : Integer
                                 ) ;
      function ttkMrSID_Version : Integer ;
    end;

  {$ENDIF}

var
  {$IFDEF OXYGENE}
    instanceCount_SID : Integer ;
  {$ELSE}
    instanceCount_SID : Integer = 0 ;
  {$ENDIF}
  {$IFDEF JAVA}
    libHandle_SID     : IMrSIDLibrary ;
  {$ELSE}
    libHandle_SID     : THandle ;
  {$ENDIF}

  {$IFDEF OXYGENE}
    fnSetUpProc        : IntPtr ;
    fnInitProc         : IntPtr ;
    fnImageHeightProc  : IntPtr ;
    fnImageWidthProc   : IntPtr ;
    fnNumBandsProc     : IntPtr ;
    fnDataTypeProc     : IntPtr ;
    fnColorSpaceProc   : IntPtr ;
    fnImageResXProc    : IntPtr ;
    fnImageResYProc    : IntPtr ;
    fnImageOriginXProc : IntPtr ;
    fnImageOriginYProc : IntPtr ;
    fnSetZoomProc      : IntPtr ;
    fnGetMetadataProc  : IntPtr ;
    fnLoadStripProc    : IntPtr ;
    fnSetBufferProc    : IntPtr ;
    fnSetBandsProc     : IntPtr ;
    fnMaxZoomLevelProc : IntPtr ;
    fnMaxValueProc     : IntPtr ;
    fnMinValueProc     : IntPtr ;
    fnNoDataValueProc  : IntPtr ;
    fnSetGridBandProc  : IntPtr ;
    fnFreeProc         : IntPtr ;
    fnGetVersionProc   : IntPtr ;
  {$ELSE}
    fnSetUp            : T_SIDSetUp        ;
    fnInit             : T_SIDInit         ;
    fnImageHeight      : T_SIDImageHeight  ;
    fnImageWidth       : T_SIDImageWidth   ;
    fnNumBands         : T_SIDNumBands     ;
    fnDataType         : T_SIDDataType     ;
    fnColorSpace       : T_SIDColorSpace   ;
    fnImageResX        : T_SIDImageResX    ;
    fnImageResY        : T_SIDImageResY    ;
    fnImageOriginX     : T_SIDImageOriginX ;
    fnImageOriginY     : T_SIDImageOriginY ;
    fnSetZoom          : T_SIDSetZoom      ;
    fnGetMetadata      : T_SIDGetMetadata  ;
    fnLoadStrip        : T_SIDLoadStrip    ;
    fnSetBuffer        : T_SIDSetBuffer    ;
    fnSetBands         : T_SIDSetBands     ;
    fnMaxZoomLevel     : T_SIDMaxZoomLevel ;
    fnMaxValue         : T_SIDMaxValue     ;
    fnMinValue         : T_SIDMinValue     ;
    fnNoDataValue      : T_SIDNoDataValue  ;
    fnSetGridBand      : T_SIDSetGridBand  ;
    fnFree             : T_SIDFree         ;
    fnGetVersion       : T_SIDGetVersion   ;
  {$ENDIF}

  {$IFDEF CLR}
    // externals from ttkMrSID.dll
    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_FUNC_SETUP
      )
    ]
    function fnSetUp( _key : String
                    ) : Integer ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_FUNC_INIT
      )
    ]
    function fnInit( _sidFilePath : String  ;
                     _password    : String  ;
                     _data        : TBytes  ;
                     _size        : Integer
                   ) : Integer ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_FUNC_IMGHEIGHT
      )
    ]
    function fnImageHeight( _instance : Integer
                          ) : Integer ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_FUNC_IMGWIDTH
      )
    ]
    function fnImageWidth( _instance : Integer
                         ) : Integer ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_FUNC_NUMBANDS
      )
    ]
    function fnNumBands( _instance : Integer
                       ) : Integer ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_FUNC_DATATYPE
      )
    ]
    function fnDataType( _instance : Integer
                       ) : Integer ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_FUNC_COLORSPACE
      )
    ]
    function fnColorSpace( _instance : Integer
                         ) : Integer ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_FUNC_IMGRESX
      )
    ]
    function fnImageResX( _instance : Integer
                        ) : Double ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_FUNC_IMGRESY
      )
    ]
    function fnImageResY( _instance : Integer
                        ) : Double ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_FUNC_IMGORIGINX
      )
    ]
    function fnImageOriginX( _instance : Integer
                           ) : Double ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_FUNC_IMGORIGINY
      )
    ]
    function fnImageOriginY( _instance : Integer
                           ) : Double ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_FUNC_SETZOOM
      )
    ]
    function fnSetZoom( _instance  : Integer ;
                        _zoomLevel : Integer
                      ) : Integer ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_PROC_LOADSTRIP
      )
    ]
    procedure fnLoadStrip( _instance  : Integer ;
                           _lineStart : Integer ;
                           _lines     : Integer ;
                           _xLeft     : Integer ;
                           _width     : Integer
                          ) ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_PROC_SETBUFFER
      )
    ]
    procedure fnSetBuffer( _instance  : Integer ;
                           _imgBuffer : IntPtr
                         ) ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_PROC_SETBANDS
      )
    ]
    procedure fnSetBands( _instance  : Integer ;
                          _redBand   : Word    ;
                          _greenBand : Word    ;
                          _blueBand  : Word
                        ) ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_PROC_SETBGRIDBAND
      )
    ]
    function fnSetGridBand( _instance : Integer  ;
                            _gridBand : SmallInt
                          ) : Integer ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_PROC_MAXZOOMLEVEL
      )
    ]
    function fnMaxZoomLevel( _instance : Integer
                           ) : Integer ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_PROC_MAXVALUE
      )
    ]
    function fnMaxValue( _instance : Integer
                       ) : Single ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_PROC_MINVALUE
      )
    ]
    function fnMinValue( _instance : Integer
                       ) : Single ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_PROC_NODATAVALUE
      )
    ]
    function fnNoDataValue( _instance : Integer
                          ) : Single ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_FUNC_GETMETADATA
      )
    ]
    function fnGetMetadata(     _instance : Integer ;
                                _key      : String ;
                            var _value    : Integer ;
                                _length   : Integer
                          ) : Integer ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_FUNC_GETMETADATA
      )
    ]
    function fnGetMetadata(     _instance : Integer ;
                                _key      : String ;
                            var _value    : Double ;
                                _length   : Integer
                          ) : Integer ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_FUNC_GETMETADATA
      )
    ]
    function fnGetMetadata( _instance : Integer ;
                            _key      : String ;
                            _value    : TBytes ;
                            _length   : Integer
                          ) : Integer ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_PROC_FREE
      )
    ]
    procedure fnFree( _instance     : Integer
                    ) ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport(SID_LIBNAME,
        CallingConvention = CallingConvention.Cdecl,
        CharSet = CharSet.Ansi,
        EntryPoint = SID_FUNC_GETVERSION
      )
    ]
    function fnGetVersion : Integer ; external ;
  {$ENDIF}

  {$IFDEF JAVA}
    function  fnSetUp       ( _key          : String
                            ) : Integer ;
    begin
      Result := libHandle_SID.ttkMrSID_SetUp( _key ) ;
    end ;

    function  fnInit        ( _sidFilePath  : String ;
                              _password     : String ;
                              _data         : TBytes = nil ;
                              _size         : Integer = 0
                            ) : Integer ;
    begin
      Result := libHandle_SID.ttkMrSID_InitSID( _sidFilePath, _password, _data, _size ) ;
    end ;

    function  fnImageHeight ( _instance     : Integer
                            ) : Integer ;
    begin
      Result := libHandle_SID.ttkMrSID_ImageHeight( _instance ) ;
    end ;

    function  fnImageWidth  ( _instance     : Integer
                            ) : Integer ;
    begin
      Result := libHandle_SID.ttkMrSID_ImageWidth( _instance ) ;
    end ;

    function  fnNumBands    ( _instance     : Integer
                            ) : Integer ;
    begin
      Result := libHandle_SID.ttkMrSID_NumBands( _instance ) ;
    end ;

    function  fnDataType    ( _instance     : Integer
                            ) : Integer ;
    begin
      Result := libHandle_SID.ttkMrSID_DataType( _instance ) ;
    end ;

    function  fnColorSpace  ( _instance     : Integer
                            ) : Integer ;
    begin
      Result := libHandle_SID.ttkMrSID_ColorSpace( _instance ) ;
    end ;

    function  fnImageResX   ( _instance     : Integer
                            ) : Double  ;
    begin
      Result := libHandle_SID.ttkMrSID_ImageResX( _instance ) ;
    end ;

    function  fnImageResY   ( _instance     : Integer
                            ) : Double  ;
    begin
      Result := libHandle_SID.ttkMrSID_ImageResY( _instance ) ;
    end ;

    function  fnImageOriginX( _instance     : Integer
                            ) : Double  ;
    begin
      Result := libHandle_SID.ttkMrSID_ImageOriginX( _instance ) ;
    end ;

    function  fnImageOriginY( _instance     : Integer
                            ) : Double  ;
    begin
      Result := libHandle_SID.ttkMrSID_ImageOriginY( _instance ) ;
    end ;

    function  fnSetZoom     ( _instance     : Integer ;
                              _zoomLevel    : Integer
                            ) : Integer ;
    begin
      Result := libHandle_SID.ttkMrSID_SetZoom( _instance, _zoomLevel  ) ;
    end ;

    function  fnMaxZoomLevel( _instance     : Integer
                            ) : Integer ;
    begin
      Result := libHandle_SID.ttkMrSID_MaxZoomLevel( _instance ) ;
    end ;

    function  fnMaxValue    ( _instance     : Integer
                            ) : Single ;
    begin
      Result := libHandle_SID.ttkMrSID_MaxValue( _instance ) ;
    end ;

    function  fnMinValue    ( _instance     : Integer
                            ) : Single ;
    begin
      Result := libHandle_SID.ttkMrSID_MinValue( _instance ) ;
    end ;

    function  fnNoDataValue ( _instance     : Integer
                            ) : Single ;
    begin
      Result := libHandle_SID.ttkMrSID_NoDataValue( _instance ) ;
    end ;

    function  fnSetGridBand ( _instance     : Integer ;
                              _gridBand     : SmallInt
                            ) : Integer ;
    begin
      Result := libHandle_SID.ttkMrSID_SetGridBand( _instance, _gridBand ) ;
    end ;

    procedure fnLoadStrip   ( _instance     : Integer ;
                              _lineStart    : Integer ;
                              _lines        : Integer ;
                              _xLeft        : Integer ;
                              _width        : Integer
                            ) ;
    begin
      libHandle_SID.ttkMrSID_LoadStrip( _instance, _lineStart, _lines, _xLeft, _width ) ;
    end ;

    procedure fnSetBuffer   ( _instance     : Integer ;
                              _imgBuffer    : com.sun.jna.Pointer
                            ) ;
    begin
      libHandle_SID.ttkMrSID_SetBuffer( _instance, _imgBuffer ) ;
    end ;

    procedure fnSetBands    ( _instance     : Integer ;
                              _redBand      : Word    ;
                              _greenBand    : Word    ;
                              _blueBand     : Word
                            ) ;
    begin
      libHandle_SID.ttkMrSID_SetBands( _instance, _redBand, _greenBand, _blueBand ) ;
    end ;

    procedure fnFree        ( _instance     : Integer
                            ) ;
    begin
      libHandle_SID.ttkMrSID_FreeSID( _instance ) ;
    end ;

    function  fnGetMetadata ( _instance       : Integer ;
                              _key            : String ;
                              var _value      : TObject ;
                              _length         : Integer
                            ) : Integer ;
    var
      vdbl : Double ;
      vint : Integer ;
      vbuf : TBytes ;
      pref : com.sun.jna.ptr.ByReference ;
      mem  : com.sun.jna.Memory ;
    begin
      case VarType( _value ) of
        varDouble  : begin
          pref := new com.sun.jna.ptr.DoubleByReference(vdbl) ;
          Result := libHandle_SID.ttkMrSID_GetMetadata( _instance, _key, pref.Pointer, _length ) ;
          _value := com.sun.jna.ptr.DoubleByReference(pref).Value ;
        end ;
        varInteger : begin
          pref := new com.sun.jna.ptr.IntByReference(vint) ;
          Result := libHandle_SID.ttkMrSID_GetMetadata( _instance, _key, pref.Pointer, _length ) ;
          _value := com.sun.jna.ptr.IntByReference(pref).Value ;
        end ;
        varArray   : begin
          vbuf := TBytes(_value) ;
          {$WARNING '### Verify JAVA code - invalid result'}
          mem := new com.sun.jna.Memory( length(vbuf) ) ;
          Result := libHandle_SID.ttkMrSID_GetMetadata( _instance, _key, mem, _length ) ;
          _value := mem.getByteArray( 0, mem.size )  ;
        end ;
      end;

    end ;

    function  fnGetVersion  : Integer ;
    begin
      Result := libHandle_SID.ttkMrSID_Version ;
    end ;

  {$ENDIF}

  {$IFDEF OXYGENE}
    function API( const _str : String ) : String ;
    begin
      Result := _str ;
    end;
  {$ELSE}
    function API( const _str : String ) : PAnsiChar ;
    var
      str : AnsiString ;
    begin
      str := AnsiString( _str ) ;
      Result := PAnsiChar( str ) ;
    end;
  {$ENDIF}

//==============================================================================
// TGIS_LayerMrSID
//==============================================================================

  constructor TGIS_LayerMrSID.Create ;
  begin
    inc( instanceCount_SID ) ;

    inherited ;

    Params.Pixel.GridNoValue := GIS_GRID_NOVALUE ;
    FNoDataValue := Params.Pixel.GridNoValue ;
    redTransp[0] := BASE_TRANSPARENT_FLAG ;
    greenTransp[0] := BASE_TRANSPARENT_FLAG ;
    blueTransp[0] := BASE_TRANSPARENT_FLAG ;
    FSubType := FSubType + [TGIS_LayerSubType.Persistent] ;
    {$IFDEF JAVA}
      libHandle_SID := nil ;
      instanceCount_SID := 0 ;
    {$ENDIF}
  end ;

  procedure TGIS_LayerMrSID.doDestroy ;
  begin
    Dormant ;

    dec( instanceCount_SID ) ;
    if instanceCount_SID <= 0 then begin
      {$IFNDEF JAVA}
        if libHandle_SID <> 0 then
            FreeLibrary(libHandle_SID) ;
        libHandle_SID := 0 ;
      {$ELSE}
        libHandle_SID := nil ;
      {$ENDIF}
      instanceCount_SID := 0 ;
    end ;

    inherited ;
  end ;

 {$IFNDEF OXYGENE}
   {$HINTS OFF} // we know that few variables were not used
 {$ENDIF}
 procedure TGIS_LayerMrSID.readGeoTiff ;
  var
    k    : Integer      ;
    lun  : TGIS_CSUnits ;
    aun  : TGIS_CSUnits ;
    flat : Double       ;
    elp  : TGIS_CSEllipsoid     ;
    pmd  : TGIS_CSPrimeMeridian ;
    dat  : TGIS_CSDatum ;
    gcs  : TGIS_CSGeographicCoordinateSystem ;
    pcs  : TGIS_CSProjectedCoordinateSystem  ;

    key                         : Integer ;

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
    keyGeogAzimuth              : Double  ;
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
    // keyGeogAzimuthUnits         : Short   ;
    keyVerticalCSType           : Short   ;
    keyVerticalCitation         : String  ;
    keyVerticalDatum            : Short   ;
    keyVerticalUnits            : Short   ;

    tabKeyStr : array of String  ;
    tabKeyInt : array of Integer ;
  const
    USER_DEFINED = 32767 ;

    procedure add_info_dbl( const _name : String; const _val : Double  ) ;
    begin
      FFileInfoEx := FFileInfoEx +#13#10 +
                     _name + ' = ' + DotFloatToStr( _val ) ;
    end;

    procedure add_info_int( const _name : String; const _val : Integer  ) ;
    begin
      FFileInfoEx := FFileInfoEx +#13#10 +
                     _name + ' = ' + IntToStr( _val ) ;
    end;

    procedure add_info_str( const _name : String; const _val : String  ) ;
    var
      t : String ;
    begin
      t := StringReplace( _val, #13, ' ', [{$IFDEF OXYGENE}TReplaceFlag.{$ENDIF}rfReplaceAll] ) ;
      t := StringReplace(    t, #10, '' , [{$IFDEF OXYGENE}TReplaceFlag.{$ENDIF}rfReplaceAll] ) ;

      FFileInfoEx := FFileInfoEx + #13#10 +
                     _name + ' = ' + t ;
    end;

    procedure add_info_adbl( const _name : String; const _ar : array of Double ) ;
    var
      i : Integer ;
    begin
      FFileInfoEx := FFileInfoEx +#13#10 +
                     _name + ' =' ;

      for i := low( _ar ) to high( _ar ) do begin
         FFileInfoEx := FFileInfoEx + ' ' + DotFloatToStr( _ar[i] ) ;
      end;
    end;

    function key_double : Double ;
    var
      data : Double ;
    begin
      data   := 0 ;
      Result := 0 ;

      {$IFDEF JAVA}
        var obj : TObject := data ;
        if fnGetMetadata( instanceDLL,
                          API( tabKeyStr[ k ] ),
                          obj,
                          1
                        ) = 1
        then
          Result := VarToDouble( obj ) ;
      {$ELSE}
        if fnGetMetadata( instanceDLL,
                          API( tabKeyStr[ k ] ),
                          {$IFNDEF OXYGENE}@{$ENDIF} data,
                          1
                        ) = 1
        then
          Result := data ;
      {$ENDIF}
    end;

    function key_int : Integer ;
    var
      data : Integer ;
    begin
      data   := 0 ;
      Result := 0 ;

      {$IFDEF JAVA}
        var obj : TObject := data ;
        if fnGetMetadata( instanceDLL,
                          API( tabKeyStr[ k ] ),
                          obj,
                          1
                        ) = 1
        then
          Result := VarToInt32( obj ) ;
      {$ELSE}
        if fnGetMetadata( instanceDLL,
                          API( tabKeyStr[ k ] ),
                          {$IFNDEF OXYGENE}@{$ENDIF} data,
                          1
                        ) = 1
        then
          Result := data ;
      {$ENDIF}
    end;

    function key_string : String ;
    var
      buf  : TBytes ;
    begin
      Result := '' ;

      SetLength( buf, 4096 ) ;
      {$IFDEF JAVA}
        var obj : TObject := buf ;
        if fnGetMetadata( instanceDLL,
                          API( tabKeyStr[ k ] ),
                          obj,
                          1
                        ) = 1
        then
          Result := ConvertAnsiString( TBytes(obj) ) ;
      {$ELSE}
        if fnGetMetadata( instanceDLL,
                          API( tabKeyStr[ k ] ),
                          buf, 4096
                        ) = 1
        then
          Result := ConvertAnsiString( buf ) ;
      {$ENDIF}
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
          18037 : cd := 22177 ;
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
             prm.CentralMeridian    := defdeg( 0, keyProjNatOriginLong    ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjNatOriginLat     ) ;
             prm.ScaleFactor        := defval( 0, keyProjScaleAtNatOrigin ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting     ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing    ) ;
           end;
         2 : // CT_TransvMercator_Modified_Alaska,
           begin
             prj := -1 ; // unknown projection, lack of documentation
             tx  := 'CT_TransvMercator_Modified_Alaska' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defval( 0, keyProjNatOriginLong    ) ;
             prm.LatitudeOfOrigin   := defval( 0, keyProjNatOriginLat     ) ;
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting     ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing    ) ;

             assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
           end;
         3 : // CT_ObliqueMercator,
           begin
             prj := -1 ; // unknown projection, lack of documentation 9815
             tx  := 'CT_ObliqueMercator' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjCenterLong       ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjCenterLat        ) ;
             prm.Azimuth            := defdeg( 0, keyProjAzimuthAngle     ) ;
             //RectifiedGridAngle recitified_grid_angle Angular
             prm.ScaleFactor        := defval( 1, keyProjScaleAtCenter    ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting     ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing    ) ;
           end;
         4 : // CT_ObliqueMercator_Laborde,
           begin
             prj := -1 ; // unknown projection, lack of documentation
             tx  := 'CT_ObliqueMercator_Laborde' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjCenterLong       ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjCenterLat        ) ;
             prm.Azimuth            := defdeg( 0, keyProjAzimuthAngle     ) ;
             //RectifiedGridAngle recitified_grid_angle Angular (90)
             prm.ScaleFactor        := defval( 1, keyProjScaleAtCenter    ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting     ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing    ) ;
           end;
         5 : // CT_ObliqueMercator_Rosenmund,
           begin
             prj := -1 ; // unknown projection, lack of documentation
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
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin ) ;
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
             tx  := 'CT_LambertConfConic_2P' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0,
                                               keyProjFalseOriginLong ,
                                               keyProjNatOriginLong
                                             ) ;
             prm.LatitudeOfOrigin   := defdeg( 0,
                                               keyProjFalseOriginLat  ,
                                               keyProjNatOriginLat
                                             ) ;
             prm.StandardParallel_1 := defdeg( 0, keyProjStdParallel1     ) ;
             prm.StandardParallel_2 := defdeg( 0, keyProjStdParallel2     ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting     ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing    ) ;
           end;
         9 : // CT_LambertConfConic_1SP
           begin
             prj := CSPROJ_Lambert_Conformal_Conic_1SP ;
             tx  := 'CT_LambertConfConic_1SP' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defval( 0, keyProjNatOriginLong    ) ;
             prm.LatitudeOfOrigin   := defval( 0, keyProjNatOriginLat     ) ;
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting     ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing    ) ;
           end;
         10 : // CT_LambertAzimEqualArea,
           begin
             prj := CSPROJ_Azimuthal_Equal_Area ;
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
             prm.FalseEasting       := defval( 0, keyProjFalseEasting     ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing    ) ;
           end;
         11 : // CT_AlbersEqualArea,
           begin
             prj := CSPROJ_Albers ;
             tx  := 'CT_AlbersEqualArea' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjNatOriginLong    ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjNatOriginLat     ) ;
             prm.StandardParallel_1 := defdeg( 29.5, keyProjStdParallel1  ) ;
             prm.StandardParallel_2 := defdeg( 45.4, keyProjStdParallel2  ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting     ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing    ) ;
           end;
         12 : // CT_AzimuthalEquidistant,
           begin
             prj := CSPROJ_Azimuthal_Equidistance ;
             tx  := 'CT_AzimuthalEquidistant' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.LongitudeOfCenter  := defdeg( 0, keyProjCenterLong       ) ;
             prm.LatitudeOfCenter   := defdeg( 0, keyProjCenterLat        ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting     ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing    ) ;
           end;
         13 : // CT_EquidistantConic,
           begin
             prj := CSPROJ_Equidistant_Conic ;
             tx  := 'CT_EquidistantConic' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.StandardParallel_1 := defdeg( 20.0, keyProjStdParallel1  ) ;
             prm.StandardParallel_2 := defdeg( 60.0, keyProjStdParallel2  ) ;
             prm.LongitudeOfCenter  := defdeg( 0, keyProjCenterLong       ) ;
             prm.LatitudeOfCenter   := defdeg( 0, keyProjCenterLat        ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting     ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing    ) ;
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
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting     ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing    ) ;
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
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjNatOriginLat     ) ;
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
             prm.CentralMeridian    := defdeg( 0, keyProjNatOriginLong    ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjNatOriginLat     ) ;
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting     ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing    ) ;
          end;
         17 : // CT_Equirectangular,
           begin
             prj := CSPROJ_Plate_Carree ;
             tx  := 'CT_Equirectangular' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjCenterLong       ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjCenterLat        ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting     ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing    ) ;
             prm.StandardParallel_1 := defdeg( 0, keyProjStdParallel1     ) ;
           end;
         18 : // CT_CassiniSoldner,
           begin
             prj := CSPROJ_Cassini_Soldner ;
             tx  := 'CT_CassiniSoldner' ;

             prm.CentralMeridian    := defdeg( 0, keyProjNatOriginLong    ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjNatOriginLat     ) ;
             prm.ScaleFactor        := defval( 0, keyProjScaleAtNatOrigin ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting     ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing    ) ;
           end;
         19 : // CT_Gnomonic,
           begin
             prj := CSPROJ_Gnomic ;
             tx  := 'CT_Gnomic' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjCenterLong       ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjCenterLat        ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting     ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing    ) ;
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
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting     ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing    ) ;
           end;
         21 : // CT_Orthographic,
           begin
             prj := CSPROJ_Orthographic ;
             tx  := 'CT_Orthographic' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjCenterLong       ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjCenterLat        ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting     ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing    ) ;
           end;
         22 : // CT_Polyconic,
           begin
             prj := CSPROJ_Polyconic ;
             tx  := 'CT_Polyconic' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjNatOriginLong    ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjNatOriginLat     ) ;
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting     ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing    ) ;
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
             prm.FalseEasting       := defval( 0, keyProjFalseEasting     ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing    ) ;
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
             prm.FalseEasting       := defval( 0, keyProjFalseEasting     ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing    ) ;
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
             prm.CentralMeridian    := defdeg( 0, prm.CentralMeridian     ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting     ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing    ) ;
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
             prm.CentralMeridian    := defval( 0, keyProjNatOriginLong    ) ;
             prm.LatitudeOfOrigin   := defval( 0, keyProjNatOriginLat     ) ;
             prm.LatitudeOfOrigin   := defval( 0, keyProjNatOriginLat     ) ;
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting     ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing    ) ;
           end;
         28 : // CT_CylindricalEqualArea,
           begin
             prj := CSPROJ_Cylindrical_Equal_Area ;
             tx  := 'CT_CylindricalEqualArea' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjNatOriginLong    ) ;
             prm.StandardParallel_1 := defdeg( 0, keyProjStdParallel1     ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting     ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing    ) ;
           end;
         -1  : // Undefined
           begin
             setup_projection_ex ;
           end ;
         32767 : // User defined
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

      if length( keyPCSCitation ) > 5 then begin
        // some significant citation
        tx := StringReplace( keyPCSCitation, #13, ' ', [{$IFDEF OXYGENE}TReplaceFlag.{$ENDIF}rfReplaceAll] ) ;
        tx := StringReplace( tx, #10, '', [{$IFDEF OXYGENE}TReplaceFlag.{$ENDIF}rfReplaceAll] ) ;
      end
      else
        tx := tx + '_' + _gcs.WKT ;

      Result := CSProjectedCoordinateSystemList.Prepare(
                  -1, tx, _gcs.EPSG, _unt.EPSG, prj, prm
                ) ;

    end;

   procedure addItem( const _key : String ; const _id : Integer ) ;
    var
      len   : Integer ;
      data  : TBytes  ;
    begin
      SetLength( data, 8 ) ;

      // test if item exists and then add it
      {$IFDEF JAVA}
        var obj : TObject := data ;
        if fnGetMetadata( instanceDLL,
                          API( _key ),
                          obj,
                          1
                        ) = 1
      then begin
        len := length( tabKeyStr ) ;
        SetLength( tabKeyStr, len + 1 );
        SetLength( tabKeyInt, len + 1 );

        tabKeyStr[ len ] := _key ;
        tabKeyInt[ len ] := _id  ;
      end;
      {$ELSE}
        if fnGetMetadata( instanceDLL,
                          API( _key ),
                          data,
                          1
                        ) = 1
        then begin
          len := length( tabKeyStr ) ;
          SetLength( tabKeyStr, len + 1 );
          SetLength( tabKeyInt, len + 1 );

          tabKeyStr[ len ] := _key ;
          tabKeyInt[ len ] := _id  ;
        end;
      {$ENDIF}
    end;

    procedure initProjectionKeys;
    begin
      addItem( 'GEOTIFF_NUM::1024::GTModelTypeGeoKey'             , 1024 ) ;
      addItem( 'GEOTIFF_NUM::1025::GTRasterTypeGeoKey'            , 1025 ) ;
      addItem( 'GEOTIFF_NUM::1026::GTCitationGeoKey'              , 1026 ) ;
      addItem( 'GEOTIFF_NUM::2048::GeographicTypeGeoKey'          , 2048 ) ;
      addItem( 'GEOTIFF_NUM::2049::GeogCitationGeoKey'            , 2049 ) ;
      addItem( 'GEOTIFF_NUM::2050::GeogGeodeticDatumGeoKey'       , 2050 ) ;
      addItem( 'GEOTIFF_NUM::2051::GeogPrimeMeridianGeoKey'       , 2051 ) ;
      addItem( 'GEOTIFF_NUM::2052::GeogLinearUnitsGeoKey'         , 2052 ) ;
      addItem( 'GEOTIFF_NUM::2054::GeogAngularUnitsGeoKey'        , 2054 ) ;
      addItem( 'GEOTIFF_NUM::2056::GeogEllipsoidGeoKey'           , 2056 ) ;
      addItem( 'GEOTIFF_NUM::2057::GeogSemiMajorAxisGeoKey'       , 2057 ) ;
      addItem( 'GEOTIFF_NUM::2058::GeogSemiMinorAxisGeoKey'       , 2058 ) ;
      addItem( 'GEOTIFF_NUM::2059::GeogInvFlatteningGeoKey'       , 2059 ) ;
      addItem( 'GEOTIFF_NUM::2061::GeogPrimeMeridianLongGeoKey'   , 2061 ) ;
      addItem( 'GEOTIFF_NUM::3072::ProjectedCSTypeGeoKey'         , 3072 ) ;
      addItem( 'GEOTIFF_NUM::3074::ProjectionGeoKey'              , 3074 ) ;
      addItem( 'GEOTIFF_NUM::3075::ProjCoordTransGeoKey'          , 3075 ) ;
      addItem( 'GEOTIFF_NUM::3076::ProjLinearUnitsGeoKey'         , 3076 ) ;
      addItem( 'GEOTIFF_NUM::3078::ProjStdParallel1GeoKey'        , 3078 ) ;
      addItem( 'GEOTIFF_NUM::3079::ProjStdParallel2GeoKey'        , 3079 ) ;
      addItem( 'GEOTIFF_NUM::3080::ProjNatOriginLongGeoKey'       , 3080 ) ;
      addItem( 'GEOTIFF_NUM::3081::ProjNatOriginLatGeoKey'        , 3081 ) ;
      addItem( 'GEOTIFF_NUM::3082::ProjFalseEastingGeoKey'        , 3082 ) ;
      addItem( 'GEOTIFF_NUM::3083::ProjFalseNorthingGeoKey'       , 3083 ) ;
      addItem( 'GEOTIFF_NUM::3084::ProjFalseOriginLongGeoKey'     , 3084 ) ;
      addItem( 'GEOTIFF_NUM::3085::ProjFalseOriginLatGeoKey'      , 3085 ) ;
      addItem( 'GEOTIFF_NUM::3088::ProjCenterLongGeoKey'          , 3088 ) ;
      addItem( 'GEOTIFF_NUM::3089::ProjCenterLatGeoKey'           , 3089 ) ;
      addItem( 'GEOTIFF_NUM::3090::ProjCenterEastingGeoKey'       , 3090 ) ;
      addItem( 'GEOTIFF_NUM::3091::ProjCenterNorthingGeoKey'      , 3091 ) ;
      addItem( 'GEOTIFF_NUM::3092::ProjScaleAtNatOriginGeoKey'    , 3092 ) ;
      addItem( 'GEOTIFF_NUM::3093::ProjScaleAtCenterGeoKey'       , 3093 ) ;
      addItem( 'GEOTIFF_NUM::3094::ProjAzimuthAngleGeoKey'        , 3094 ) ;
      addItem( 'GEOTIFF_NUM::3095::ProjStraightVertPoleLongGeoKey', 3095 ) ;
      addItem( 'GEOTIFF_NUM::3096::ProjRectifiedGridAngleGeoKey'  , 3096 ) ;
    end;

  begin
    initProjectionKeys ;

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
    keyGeogAzimuth                 := NaN  ;
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
    // keyGeogAzimuthUnits            := -1   ;
    keyVerticalCSType              := -1   ;
    keyVerticalCitation            := ''   ;
    keyVerticalDatum               := -1   ;
    keyVerticalUnits               := -1   ;

    // interpret table ;
    k := 0 ;

    add_info_int( 'Len', high( tabKeyInt ) ) ;

    while k <= high( tabKeyInt ) do begin
      key := tabKeyInt[ k ] ;
      case key of
        0    : // Empty key
          begin
          end ;
        1024 : // GTModelType
          begin
            keyGTModelType := key_int ;
            add_info_int( 'GTModelTypeGeoKey', keyGTModelType ) ;
          end ;
        1025 : // GTRasterType
          begin
            keyGTRasterType := key_int ;
            add_info_int( 'GTRasterTypeGeoKey', keyGTRasterType ) ;
          end ;
        1026 : // GTCitation
          begin
            keyGTCitation := key_string ;
            add_info_str( 'GTCitationGeoKey', keyGTCitation ) ;
          end ;
        2048 : // GeographicType
          begin
            keyGeographicType := key_int ;
            add_info_int( 'GeographicTypeGeoKey', keyGeographicType ) ;
          end;
        2049 : // GeogCitation
          begin
            keyGeogCitation := key_string ;
            add_info_str( 'GeogCitationGeoKey', keyGeogCitation ) ;
          end ;
        2050 : // GeogGeodeticDatum
          begin
            keyGeogGeodeticDatum := key_int ;
            add_info_int( 'GeogGeodeticDatumGeoKey', keyGeogGeodeticDatum ) ;
          end ;
        2051 : // GeogPrimeMeridian
          begin
            keyGeogPrimeMeridian := key_int ;
            add_info_int( 'GeogPrimeMeridianKey', keyGeogPrimeMeridian) ;
          end ;
        2061 : // GeogPrimeMeridianLong
          begin
            keyGeogPrimeMeridianLong := key_double ;
            add_info_dbl( 'GeogPrimeMeridianLongGeoKey', keyGeogPrimeMeridianLong ) ;
          end ;
        2052 : // GeogLinearUnits
          begin
            keyGeogLinearUnits := key_int ;
            add_info_int( 'GeogLinearUnitsGeoKey', keyGeogLinearUnits ) ;
          end ;
        2053 : // GeogLinearUnitSize
          begin
            keyGeogLinearUnitSize := key_double ;
            add_info_dbl( 'GeogLinearUnitSizeGeoKey', keyGeogLinearUnitSize ) ;
          end ;
        2054 : // GeogAngularUnits
          begin
            keyGeogAngularUnits := key_int ;
            add_info_int( 'GeogAngularUnitsGeoKey', keyGeogAngularUnits ) ;
          end ;
        2055 : // GeogAngularUnitSize
          begin
            keyGeogAngularUnitSize := key_double ;
            add_info_dbl( 'GeogAngularUnitSizeKey', keyGeogAngularUnitSize ) ;
          end ;
        2056 : // GeogEllipsoid
          begin
            keyGeogEllipsoid := key_int ;
            add_info_int( 'GeogEllipsoidGeoKey', keyGeogEllipsoid ) ;
          end ;
        2057 : // GeogSemiMajorAxis
          begin
            keyGeogSemiMajorAxis := key_double ;
            add_info_dbl( 'GeogSemiMajorAxisGeoKey', keyGeogSemiMajorAxis ) ;
          end ;
        2058 : // GeogSemiMinorAxis
          begin
            keyGeogSemiMinorAxis := key_double ;
            add_info_dbl( 'GeogSemiMinorAxisGeoKey', keyGeogSemiMinorAxis ) ;
          end ;
        2059 : // GeogInvFlattering
          begin
            keyGeogInvFlattering := key_double ;
            add_info_dbl( 'GeogInvFlatteringGeoKey', keyGeogInvFlattering ) ;
          end ;
        2060 : // GeogAzimuth
          begin
            add_info_dbl( 'GeogAzimuthGeoKey', keyGeogAzimuth ) ;
            assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
          end ;
        3072 : // ProjectedCSType
          begin
            keyProjectedCSType := key_int ;
            add_info_int( 'ProjectedCSTypeGeoKey', keyProjectedCSType ) ;
          end ;
        3073 : // PCSCitation
          begin
            keyPCSCitation := key_string ;
            add_info_str( 'PCSCitationGeoKey', keyPCSCitation ) ;
          end ;
        3074 : // Projected
          begin
            keyProjected := key_int ;
            add_info_int( 'ProjectedGeoKey', keyProjected ) ;
          end ;
        3075 : // ProjCoordTrans
          begin
            keyProjCoordTrans := key_int ;
            add_info_int( 'ProjCoordTransGeoKey', keyProjCoordTrans ) ;
            if keyProjCoordTrans = USER_DEFINED then
              exit ;
          end ;
        3076 : // ProjLinearUnits
          begin
            keyProjLinearUnits := key_int ;
            add_info_int( 'ProjLinearUnitsGeoKey', keyProjLinearUnits ) ;
          end ;
        3077 : // ProjLinearUnitSize
          begin
            keyProjLinearUnitSize := key_double ;
           add_info_dbl( 'ProjLinearUnitSizeGeoKey', keyProjLinearUnitSize ) ;
          end ;
        3078 : // ProjStdParallel1
          begin
            keyProjStdParallel1 := key_double ;
            add_info_dbl( 'ProjStdParallel1GeoKey', keyProjStdParallel1 ) ;
          end ;
        3079 : // ProjStdParallel2
          begin
            keyProjStdParallel2 := key_double ;
            add_info_dbl( 'ProjStdParallel2GeoKey', keyProjStdParallel2 ) ;
          end ;
        3080 : // ProjNatOriginLong
          begin
            keyProjNatOriginLong := key_double ;
            add_info_dbl( 'ProjNatOriginLongGeoKey', keyProjNatOriginLong ) ;
          end ;
        3081 : // ProjNatOriginLat
          begin
            keyProjNatOriginLat := key_double ;
            add_info_dbl( 'ProjNatOriginLatGeoKey', keyProjNatOriginLat ) ;
          end ;
        3082 : // ProjFalseEasing
          begin
            keyProjFalseEasting := key_double ;
            add_info_dbl( 'ProjFalseEastingGeoKey', keyProjFalseEasting ) ;
          end ;
        3083 : // ProjFalseNorthing
          begin
            keyProjFalseNorthing := key_double ;
            add_info_dbl( 'ProjFalseNorthingGeoKey', keyProjFalseNorthing ) ;
          end ;
        3084 : // ProjFalseOriginLong
          begin
            keyProjFalseOriginLong := key_double ;
            add_info_dbl( 'ProjFalseOriginLongGeoKey', keyProjFalseOriginLong ) ;
          end ;
        3085 : // ProjFalseOriginLat
          begin
            keyProjFalseOriginLat := key_double ;
            add_info_dbl( 'GeogProjFalseOriginLat', keyProjFalseOriginLat ) ;
          end ;
        3086 : // ProjFalseOriginEasting
          begin
            keyProjFalseOriginEasting := key_double ;
            add_info_dbl( 'ProjFalseOriginEastingGeoKey', keyProjFalseOriginEasting ) ;
          end ;
        3087 : // ProjFalseOriginNorthing
          begin
            keyProjFalseOriginNorthing := key_double ;
            add_info_dbl( 'ProjFalseOriginNorthingGeoKey', keyProjFalseOriginNorthing ) ;
          end ;
        3088 : // ProjCenterLong
          begin
            keyProjCenterLong := key_double ;
            add_info_dbl( 'ProjCenterLongGeoKey', keyProjCenterLong ) ;
          end ;
        3089 : // ProjCenterLat
          begin
            keyProjCenterLat := key_double ;
            add_info_dbl( 'ProjCenterLatGeoKey', keyProjCenterLat ) ;
          end ;
        3090 : // ProjCenterEasting
          begin
            keyProjCenterEasting := key_double ;
            add_info_dbl( 'ProjCenterEastingGeoKey', keyProjCenterEasting ) ;
          end ;
        3091 : // ProjCenterNorthing
          begin
            keyProjCenterNorthing := key_double ;
            add_info_dbl( 'ProjProjCenterNorthingGeoKey', keyProjCenterNorthing ) ;
          end ;
        3092 : // ProjScaleAtNatOrigin
          begin
            keyProjScaleAtNatOrigin := key_double ;
            add_info_dbl( 'ProjScaleAtNatOriginGeoKey', keyProjScaleAtNatOrigin ) ;
          end ;
        3093 : // ProjScaleAtCenter
          begin
            keyProjScaleAtCenter := key_double ;
            add_info_dbl( 'ProjScaleAtCenterGeoKey', keyProjScaleAtCenter ) ;
          end ;
        3094 : // ProjAzimuthAngle
          begin
            keyProjAzimuthAngle := key_double ;
            add_info_dbl( 'ProjAzimuthAngleGeoKey', keyProjAzimuthAngle ) ;
          end ;
        3095 : // ProjStraightVertPoleLong
          begin
            keyProjStraightVertPoleLong := key_double ;
            add_info_dbl( 'ProjStraightVertPoleLongGeoKey', keyProjStraightVertPoleLong ) ;
          end ;
        // documentation mismatch
        //  2060 : // GeogAzimuthUnits
        //  begin
        //    add_info( 'GeogAzimuthUnitsGeoKey', keyGeogAzimuthUnits ) ;
        //    assert( False, GIS_RS_ERR_UNTESTED ) ;
        //  end ;
        4096 : // VerticalCSType
          begin
            keyVerticalCSType := key_int ;
            add_info_dbl( 'VerticalCSTypeGeoKey', keyVerticalCSType ) ;
          end ;
        4097 : // VerticalCitation
          begin
            keyVerticalCitation := key_string ;
            add_info_str( 'VerticalCitationGeoKey', keyVerticalCitation ) ;
          end ;
        4098 : // VerticalDatum
          begin
            keyVerticalDatum := key_int ;
            add_info_dbl( 'VerticalDatumGeoKey', keyVerticalDatum ) ;
          end ;
        4099 : // VerticalUnits
          begin
            keyVerticalUnits := key_int ;
            add_info_dbl( 'GeogVerticalUnits', keyVerticalUnits ) ;
          end ;
        else
          begin
            assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
          end ;
      end ;
      inc( k ) ;
    end ;

    // construct angular units
       aun := nil ;
       if ( keyGeogAngularUnits > 0            ) and
          ( keyGeogAngularUnits < USER_DEFINED )
       then
         aun := CSUnitsList.ByEPSG( keyGeogAngularUnits )
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
       then
         lun := CSUnitsList.ByEPSG( keyProjLinearUnits )
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
      then
        elp := CSEllipsoidList.ByEPSG( keyGeogEllipsoid )
      else begin
        if not IsNan( keyGeogSemiMajorAxis ) then begin
          flat := 0 ;

          if not IsNan( keyGeogSemiMajorAxis ) and
             not IsNan( keyGeogSemiMinorAxis ) and
             not GisIsSameValue( keyGeogSemiMajorAxis, keyGeogSemiMinorAxis ) then
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
       then
         pmd := CSPrimeMeridianList.ByEPSG( keyGeogPrimeMeridian )
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
       then
         dat := CSDatumList.ByEPSG( keyGeogGeodeticDatum )
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
       then
         gcs := CSGeographicCoordinateSystemList.ByEPSG( keyGeographicType ) ;

       if assigned( dat ) and
          assigned( pmd ) and
          assigned( aun ) then
       begin
         if assigned( gcs ) and
            (
              ( gcs.Datum.EPSG         <> dat.EPSG ) or
              ( gcs.PrimeMeridian.EPSG <> pmd.EPSG ) or
              ( gcs.Units.EPSG         <> aun.EPSG )
            )
         then
           gcs := nil ;

         if not assigned( gcs ) then
           gcs := CSGeographicCoordinateSystemList.Prepare(
                    -1, 'Custom_GCS_' + dat.WKT , dat.EPSG, pmd.EPSG, aun.EPSG
                  )
       end ;

    // construct GCS
       gcs := nil ;
       if ( keyGeographicType > 0            ) and
          ( keyGeographicType < USER_DEFINED )
       then
         gcs := CSGeographicCoordinateSystemList.ByEPSG( keyGeographicType ) ;

       if assigned( dat ) and
          assigned( pmd ) and
          assigned( aun ) then
       begin
         if assigned( gcs ) and
            (
              ( gcs.Datum.EPSG         <> dat.EPSG ) or
              ( gcs.PrimeMeridian.EPSG <> pmd.EPSG ) or
              ( gcs.Units.EPSG         <> aun.EPSG )
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
         if pcs.Geocs.EPSG <> gcs.EPSG then begin
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
         if pcs.Units.EPSG <> lun.EPSG then begin
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
        1 :  begin
               if assigned( pcs ) then
                 self.CS := pcs
               else
                 self.CS := CSUnknownCoordinateSystem ;
             end ;
        2 :  begin
               if assigned( gcs ) then
                 self.CS := gcs
               else
                 self.CS := CSUnknownCoordinateSystem ;
             end ;
        else begin
               self.CS := CSUnknownCoordinateSystem
             end ;
      end;

  end;

  {$IFNDEF OXYGENE}
    {$HINTS ON}
  {$ENDIF}

  procedure TGIS_LayerMrSID.setUp ;
  var
    width               : Integer             ;
    line_width          : Integer             ;
    xres, yres          : Double              ;
    originx, originy    : Double              ;
    ext                 : TGIS_Extent         ;
    ex                  : String              ;
    typename            : String              ;
    copyright           : String              ;

    procedure use_world_file( const _ext1 : String; const _ext2 : String ) ;
    var
      fname1 : String ;
      fname2 : String ;
      ftab   : String ;
    begin
      fname1 := GetPathNoExt( Path ) + _ext1 ;
      fname2 := GetPathNoExt( Path ) + _ext2 ;

      if IsStringEmpty( PathTAB ) then
        ftab  := GetPathNoExt( Path ) + '.tab'
      else
        ftab  := PathTAB ;

      if SafeFileExists( fname1 ) or SafeFileExists( fname2 ) or  SafeFileExists( ftab ) then
      begin
        if      SafeFileExists( fname1 ) then
              setWorldFile( _ext1 )
        else if SafeFileExists( fname2 ) then
              setWorldFile( _ext2 )
        else if SafeFileExists( ftab   ) then
              setWorldFile( _ext1 ) ;
      end
      else begin
        xres    := fnImageResX   ( instanceDLL ) ;
        yres    := fnImageResY   ( instanceDLL ) ;
        originx := fnImageOriginX( instanceDLL ) ;
        originy := fnImageOriginY( instanceDLL ) ;

        {$IFDEF GIS_NORECORDS}
          ext := new TGIS_Extent ;
        {$ENDIF}

        ext.XMin := originx ;
        ext.XMax := originx ;

        ext.YMin := originy ;
        ext.YMax := originy ;
        Extent := ext ;
        scaleX := xres ;
        scaleY := yres ;
        if scaleY > 0 then
          scaleY := -scaleY ;
      end ;
    end ;

  begin

    reloadLibrary ;

    try
      if SafeFileExists( Path ) or assigned( Stream ) then begin
        ex := GetFileExt( LowerCase( Path ) ) ;

        // get bitmap size into easy to access properties
        FBitHeight := fnImageHeight( instanceDLL ) ;
        FBitWidth  := fnImageWidth ( instanceDLL ) ;

        if      ex = WORLD_FILE_EXT_SID then
                use_world_file( WORLD_FILE_EXT_SID_TFW, WORLD_FILE_EXT_SID_TFW2 )
        else if ex = WORLD_FILE_EXT_SID_JPF then
                use_world_file( WORLD_FILE_EXT_SID_JPF_TFW, WORLD_FILE_EXT_SID_JPF_TFW2 )
        else if ex = WORLD_FILE_EXT_SID_JPX then
                use_world_file( WORLD_FILE_EXT_SID_JPX_TFW, WORLD_FILE_EXT_SID_JPX_TFW2 )
        else if ex = WORLD_FILE_EXT_SID_JPC then
                use_world_file( WORLD_FILE_EXT_SID_JPC_TFW, WORLD_FILE_EXT_SID_JPC_TFW2 )
        else if ex = WORLD_FILE_EXT_SID_J2C then
                use_world_file( WORLD_FILE_EXT_SID_J2C_TFW, WORLD_FILE_EXT_SID_J2C_TFW2 )
        else if ex = WORLD_FILE_EXT_SID_J2X then
                use_world_file( WORLD_FILE_EXT_SID_J2X_TFW, WORLD_FILE_EXT_SID_J2X_TFW2 )
        else if ex = WORLD_FILE_EXT_SID_J2K then
                use_world_file( WORLD_FILE_EXT_SID_J2K_TFW, WORLD_FILE_EXT_SID_J2K_TFW2 )
        else if ex = WORLD_FILE_EXT_SID_JP2 then
                use_world_file( WORLD_FILE_EXT_SID_JP2_TFW, WORLD_FILE_EXT_SID_JP2_TFW2 ) ;

        if ex = WORLD_FILE_EXT_SID then begin
          typename  := 'MrSID Format (SID)' ;
          copyright := '';
        end
        else begin
          typename  := 'JPEG 2000 File Format (via MrSID Layer)' ;
          copyright := '' ;
        end ;

      end ;

      width  := FBitWidth  ;

      FBandsCount := fnNumBands( instanceDLL ) ;

      colorSpace := fnColorSpace( instanceDLL ) ;
      dataType := fnDataType( instanceDLL ) ;

      case dataType of
        SID_DATATYPE_UINT8,
        SID_DATATYPE_SINT8,
        SID_DATATYPE_UINT16,
        SID_DATATYPE_SINT16 : line_width := 3*width ;
        SID_DATATYPE_UINT32,
        SID_DATATYPE_SINT32,
        SID_DATATYPE_FLOAT32 : line_width := 4*width ;
        SID_DATATYPE_FLOAT64 : line_width := 8*width ;
      else
        exit ;
      end ;

      lineBytesWidth := line_width ;

      stripBufferSize := Int64(FBitWidth)*FBitHeight*3 ;
      if stripBufferSize > SID_MAX_STRIP_BUFFER_SIZE then
        stripBufferSize := SID_MAX_STRIP_BUFFER_SIZE ;

      lastviewrect := Rect(0, 0, 0, 0) ;    ;

      {$IFDEF CLR}
        if AssignedPtr( stripBuffer ) then
          Marshal.FreeHGlobal( stripBuffer ) ;
        stripBuffer := Marshal.AllocHGlobal( stripBufferSize ) ;
        fnSetBuffer( instanceDLL, stripBuffer ) ;
      {$ENDIF}
      {$IFDEF DCC}
        SetLength( stripBuffer, stripBufferSize ) ;
        fnSetBuffer( instanceDLL, @stripBuffer[0] ) ;
      {$ENDIF}
      {$IFDEF JAVA}
        stripBuffer := new com.sun.jna.Memory( stripBufferSize ) ;
        fnSetBuffer( instanceDLL, stripBuffer ) ;
      {$ENDIF}

      lastZoom := 1 ;
      actualZoom := 1 ;

      maxZoomLevel := fnMaxZoomLevel( instanceDLL) ;

      totalBitWidth  := line_width ;

      realLineWidth  := width ;
      totalLineWidth := line_width ;

      realBitCount := 24 ;
      intLineWidth := FBitWidth * 3 ;
      colorsNo     := 0 ;

      // decompressed lines number
      stripLinesNo := 0 ;

      inherited ;

      if SafeFileExists( Path ) then
        FAge := GisFileAge( Path ) ;

      readGeoTiff ;

      FFileInfo := Format( typename + #13#10 +
                           '%d x %d; %d bit'    + #13#10 +
                           copyright,
                           [ FBitWidth, FBitHeight, realBitCount]
                         ) ;
      FFileInfo := FFileInfo + FFileInfoEx ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0) ;
    end ;

  end;

  procedure TGIS_LayerMrSID.setupParams ;
  begin

    inherited ;

    if FIsNativeGridImage then begin
      if FGridBand <> Params.Pixel.GridBand then begin
        if ( Params.Pixel.GridBand <= FBandsCount) and
           ( Params.Pixel.GridBand >= 0 )
        then begin
          if Params.Pixel.GridBand > 0 then
            bandsMap[0] := Params.Pixel.GridBand -1
          else
            bandsMap[0] := 0 ;
        end
        else
          bandsMap[0] := 0 ;
        if bandsMap[0] >= 0 then begin
          FGridBand := bandsMap[0] +1 ;
          if not libLoaded then
            reloadLibrary ;
          fnSetGridBand( instanceDLL, SmallInt(bandsMap[0]) ) ;
          Params.Pixel.GridNoValue := fnNoDataValue( instanceDLL) ;
          FNoDataValue := Params.Pixel.GridNoValue ;
          if FMinZ >= FMaxZ then begin
            FMinZ := fnMinValue( instanceDLL) ;
            FMaxZ := fnMaxValue( instanceDLL) ;
          end;
        end;
      end ;
    end;

    if colorSpace = SID_COLORSPACE_MULTISPECTRAL then begin
      if not libLoaded then
        reloadLibrary ;
        fnSetBands( instanceDLL, Word(bandsMap[2]),
                  Word(bandsMap[1]), Word(bandsMap[0]) ) ;

    end ;

  end ;

  function  TGIS_LayerMrSID.setFileScale( const _dwidth : Double ;
                                          const _swidth : Double
                                        ) : Double ;
  var
    izoom         : Integer        ;
    zoomLevel     : Integer        ;
    idzoom        : Double         ;
  begin

    if _dwidth <> 0 then
      izoom := RoundS((_swidth)/_dwidth)
    else
      izoom := 0 ;

    if izoom <= 1 then
      zoomLevel := 0
    else begin
      zoomLevel := RoundS(Log2(izoom)) ;
    end;

    if zoomLevel > maxZoomLevel then
      zoomLevel := maxZoomLevel ;

    idzoom := Power(2, zoomLevel) ;
    izoom := Integer(TruncS(idzoom)) ;
    Result := 1/idzoom ;

    if izoom <> actualZoom then begin
      izoom := fnSetZoom(instanceDLL, zoomLevel) ;
      if izoom < zoomLevel then begin
        maxZoomLevel := izoom ;
      end ;
      actualZoom := RoundS(Power(2, izoom) );
      Result := 1.0/actualZoom ;
    end ;
  end ;

  procedure TGIS_LayerMrSID.setFileView( const _viewRect : TRect ) ;
  var
    t, b : Integer ;
    h, zh : Integer ;
  begin

    if (lastviewrect.Top    <= _viewRect.Top)    and
       (lastviewrect.Bottom >= _viewRect.Bottom) and
       (lastviewrect.Left   <= _viewRect.Left)   and
       (lastviewrect.Right  >= _viewRect.Right) then
      exit ;
    t := _viewRect.Top ;
    h := (_viewRect.Bottom -_viewRect.Top +2)* 1 ;
    zh := (FBitHeight + actualZoom -1) div actualZoom ;
    b := t +h ;
    if b >= zh then
      b := zh -1 ;
    lastviewrect := Rect(_viewRect.Left, t, _viewRect.Right, b) ;
    stripHeight := lastviewrect.Bottom - lastviewrect.Top +1 ;
  end;

  procedure TGIS_LayerMrSID.bindLibrary ;
  begin
    {$IFDEF JAVA}
      if libHandle_SID <> nil then exit ;

      assert( instanceCount_SID >= 0 ) ;
      libHandle_SID := IMrSIDLibrary(com.sun.jna.Native.loadLibrary( SID_LIBNAME_NOEXT, typeOf(IMrSIDLibrary) ) );
    {$ELSE}
      if libHandle_SID <> 0 then exit ;

      assert( instanceCount_SID >= 0 ) ;
      libHandle_SID := LoadLibraryWithinHinstance( SID_LIBNAME ) ;
    {$ENDIF}

    try
      {$IFDEF JAVA}
        if libHandle_SID = nil then begin
          instanceCount_SID := 0 ;
          Abort ;
        end ;
      {$ELSE}
        if libHandle_SID = 0 then begin
          instanceCount_SID := 0 ;
          Abort ;
        end ;
      {$ENDIF}

      {$IFDEF CLR}
        fnSetUpProc        := GetProcAddress(libHandle_SID, SID_FUNC_SETUP       ) ;
        fnInitProc         := GetProcAddress(libHandle_SID, SID_FUNC_INIT        ) ;
        fnImageResXProc    := GetProcAddress(libHandle_SID, SID_FUNC_IMGRESX     ) ;
        fnImageResYProc    := GetProcAddress(libHandle_SID, SID_FUNC_IMGRESY     ) ;
        fnImageOriginXProc := GetProcAddress(libHandle_SID, SID_FUNC_IMGORIGINX  ) ;
        fnImageOriginYProc := GetProcAddress(libHandle_SID, SID_FUNC_IMGORIGINY  ) ;
        fnImageHeightProc  := GetProcAddress(libHandle_SID, SID_FUNC_IMGHEIGHT   ) ;
        fnImageWidthProc   := GetProcAddress(libHandle_SID, SID_FUNC_IMGWIDTH    ) ;
        fnNumBandsProc     := GetProcAddress(libHandle_SID, SID_FUNC_NUMBANDS    ) ;
        fnDataTypeProc     := GetProcAddress(libHandle_SID, SID_FUNC_DATATYPE    ) ;
        fnColorSpaceProc   := GetProcAddress(libHandle_SID, SID_FUNC_COLORSPACE  ) ;
        fnLoadStripProc    := GetProcAddress(libHandle_SID, SID_PROC_LOADSTRIP   ) ;
        fnSetZoomProc      := GetProcAddress(libHandle_SID, SID_FUNC_SETZOOM     ) ;
        fnGetMetadataProc  := GetProcAddress(libHandle_SID, SID_FUNC_GETMETADATA ) ;
        fnSetBufferProc    := GetProcAddress(libHandle_SID, SID_PROC_SETBUFFER   ) ;
        fnSetBandsProc     := GetProcAddress(libHandle_SID, SID_PROC_SETBANDS   )  ;
        fnSetGridBandProc  := GetProcAddress(libHandle_SID, SID_PROC_SETBGRIDBAND) ;
        fnMaxZoomLevelProc := GetProcAddress(libHandle_SID, SID_PROC_MAXZOOMLEVEL) ;
        fnMaxValueProc     := GetProcAddress(libHandle_SID, SID_PROC_MAXVALUE    ) ;
        fnMinValueProc     := GetProcAddress(libHandle_SID, SID_PROC_MINVALUE    ) ;
        fnNoDataValueProc  := GetProcAddress(libHandle_SID, SID_PROC_NODATAVALUE ) ;
        fnFreeProc         := GetProcAddress(libHandle_SID, SID_PROC_FREE        ) ;
        fnGetVersionProc   := GetProcAddress(libHandle_SID, SID_FUNC_GETVERSION  ) ;
      {$ENDIF}
      {$IFDEF JAVA}
        fnSetUpProc        := 1 ;
        fnInitProc         := 1 ;
        fnImageResXProc    := 1 ;
        fnImageResYProc    := 1 ;
        fnImageOriginXProc := 1 ;
        fnImageOriginYProc := 1 ;
        fnImageHeightProc  := 1 ;
        fnImageWidthProc   := 1 ;
        fnNumBandsProc     := 1 ;
        fnDataTypeProc     := 1 ;
        fnColorSpaceProc   := 1 ;
        fnLoadStripProc    := 1 ;
        fnSetZoomProc      := 1 ;
        fnGetMetadataProc  := 1 ;
        fnSetBufferProc    := 1 ;
        fnSetBandsProc     := 1 ;
        fnSetGridBandProc  := 1 ;
        fnMaxZoomLevelProc := 1 ;
        fnMaxValueProc     := 1 ;
        fnMinValueProc     := 1 ;
        fnNoDataValueProc  := 1 ;
        fnFreeProc         := 1 ;
        fnGetVersionProc   := 1 ;
      {$ENDIF}
      {$IFDEF DCC}
        @fnSetUp        := GetProcAddress(libHandle_SID, SID_FUNC_SETUP       ) ;
        @fnInit         := GetProcAddress(libHandle_SID, SID_FUNC_INIT        ) ;
        @fnImageResX    := GetProcAddress(libHandle_SID, SID_FUNC_IMGRESX     ) ;
        @fnImageResY    := GetProcAddress(libHandle_SID, SID_FUNC_IMGRESY     ) ;
        @fnImageOriginX := GetProcAddress(libHandle_SID, SID_FUNC_IMGORIGINX  ) ;
        @fnImageOriginY := GetProcAddress(libHandle_SID, SID_FUNC_IMGORIGINY  ) ;
        @fnImageHeight  := GetProcAddress(libHandle_SID, SID_FUNC_IMGHEIGHT   ) ;
        @fnImageWidth   := GetProcAddress(libHandle_SID, SID_FUNC_IMGWIDTH    ) ;
        @fnNumBands     := GetProcAddress(libHandle_SID, SID_FUNC_NUMBANDS    ) ;
        @fnDataType     := GetProcAddress(libHandle_SID, SID_FUNC_DATATYPE    ) ;
        @fnColorSpace   := GetProcAddress(libHandle_SID, SID_FUNC_COLORSPACE  ) ;
        @fnLoadStrip    := GetProcAddress(libHandle_SID, SID_PROC_LOADSTRIP   ) ;
        @fnSetZoom      := GetProcAddress(libHandle_SID, SID_FUNC_SETZOOM     ) ;
        @fnGetMetadata  := GetProcAddress(libHandle_SID, SID_FUNC_GETMETADATA ) ;
        @fnSetBuffer    := GetProcAddress(libHandle_SID, SID_PROC_SETBUFFER   ) ;
        @fnSetBands     := GetProcAddress(libHandle_SID, SID_PROC_SETBANDS    ) ;
        @fnSetGridBand  := GetProcAddress(libHandle_SID, SID_PROC_SETBGRIDBAND) ;
        @fnMaxZoomLevel := GetProcAddress(libHandle_SID, SID_PROC_MAXZOOMLEVEL) ;
        @fnMaxValue     := GetProcAddress(libHandle_SID, SID_PROC_MAXVALUE    ) ;
        @fnMinValue     := GetProcAddress(libHandle_SID, SID_PROC_MINVALUE    ) ;
        @fnNoDataValue  := GetProcAddress(libHandle_SID, SID_PROC_NODATAVALUE ) ;
        @fnFree         := GetProcAddress(libHandle_SID, SID_PROC_FREE        ) ;
        @fnGetVersion   := GetProcAddress(libHandle_SID, SID_FUNC_GETVERSION  ) ;
      {$ENDIF}

      {$IFDEF OXYGENE}
        if not ( fnInitProc         <> 0 ) or
           not ( fnImageResXProc    <> 0 ) or
           not ( fnImageResYProc    <> 0 ) or
           not ( fnImageOriginXProc <> 0 ) or
           not ( fnImageOriginYProc <> 0 ) or
           not ( fnImageHeightProc  <> 0 ) or
           not ( fnImageWidthProc   <> 0 ) or
           not ( fnNumBandsProc     <> 0 ) or
           not ( fnDataTypeProc     <> 0 ) or
           not ( fnColorSpaceProc   <> 0 ) or
           not ( fnLoadStripProc    <> 0 ) or
           not ( fnSetZoomProc      <> 0 ) or
           not ( fnGetMetadataProc  <> 0 ) or
           not ( fnSetBufferProc    <> 0 ) or
           not ( fnSetBandsProc     <> 0 ) or
           not ( fnSetGridBandProc  <> 0 ) or
           not ( fnMaxZoomLevelProc <> 0 ) or
           not ( fnMaxValueProc     <> 0 ) or
           not ( fnMinValueProc     <> 0 ) or
           not ( fnNoDataValueProc  <> 0 ) or
           not ( fnFreeProc         <> 0 )
        then Abort ;
      {$ELSE}
        if not assigned( @fnInit         ) or
           not assigned( @fnImageResX    ) or
           not assigned( @fnImageResY    ) or
           not assigned( @fnImageOriginX ) or
           not assigned( @fnImageOriginY ) or
           not assigned( @fnImageHeight  ) or
           not assigned( @fnImageWidth   ) or
           not assigned( @fnNumBands     ) or
           not assigned( @fnDataType     ) or
           not assigned( @fnColorSpace   ) or
           not assigned( @fnLoadStrip    ) or
           not assigned( @fnSetZoom      ) or
           not assigned( @fnGetMetadata  ) or
           not assigned( @fnSetBuffer    ) or
           not assigned( @fnSetBands     ) or
           not assigned( @fnSetGridBand  ) or
           not assigned( @fnMaxZoomLevel ) or
           not assigned( @fnMaxValue     ) or
           not assigned( @fnMinValue     ) or
           not assigned( @fnNoDataValue  ) or
           not assigned( @fnFree         )
        then Abort ;
      {$ENDIF}

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), SID_LIBNAME, 0 ) ;
    end ;
  end ;

  procedure TGIS_LayerMrSID.reloadLibrary ;
  var
    width, height : Integer    ;
    line_width    : Integer    ;
    sbs           : Integer    ;
    spath         : String     ;
    mrsidpwd      : String     ;
    password      : String     ;
  begin
    bindLibrary ;

    mrsidpwd := GisKeyList.Get( GIS_KEY_MRSID, GIS_KEY_PASS ) ;

    {$IFDEF OXYGENE}
      if fnSetUpProc <> 0 then
    {$ELSE}
      if assigned( @fnSetUp ) then
    {$ENDIF}
    begin
      if fnSetUp( API( mrsidpwd ) ) = 0 then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_NOTLICENSED ), SID_LIBNAME, 0 ) ;
    end ;

    {$IFDEF OXYGENE}
      if fnGetVersionProc <> 0 then
    {$ELSE}
      if assigned( @fnGetVersion ) then
    {$ENDIF}
    begin
      if fnGetVersion <> MRSID_LIBVERSION then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEBADFORMAT ), SID_LIBNAME, MRSID_LIBVERSION ) ;
    end
    else
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEBADFORMAT ), SID_LIBNAME, MRSID_LIBVERSION ) ;

    try
      if not SafeFileExists( Path ) and not assigned( Stream ) then Abort ;

      // try to open w/o password first
      try
        spath := GetShortPath( Path ) ;
        if assigned( Stream ) then begin
          SetLength( stmBuf, Stream.Size ) ;
          Stream.Position := 0 ;
          {$IFDEF OXYGENE}
            Stream.Read( stmBuf, Stream.Size ) ;
            instanceDLL := fnInit( API( spath ), '', stmBuf, Stream.Size ) ;
          {$ELSE}
            Stream.Read( stmBuf[ 0 ], Stream.Size ) ;
            instanceDLL := fnInit( API( spath ), '', @stmBuf[0], Stream.Size ) ;
          {$ENDIF}
        end
        else
          instanceDLL := fnInit( API( spath ), '' ) ;
      except
        // do nothing
      end ;

      if instanceDLL < 0 then begin
        // and now try to open with password
        password := GisPasswordList.Get( Name, 'password' ) ;
        if ( IsStringEmpty( password ) ) and assigned( FOnPassword ) then
          {$IFDEF OXYGENE}
            password := FOnPassword(
                          Self,
                          TGIS_TemplateProducerEventArgs.Create('password')
                        ) ;
          {$ELSE}
            password := FOnPassword(
                          Self,
                          'password'
                        ) ;
          {$ENDIF}

        spath := GetShortPath( Path ) ;
        instanceDLL := fnInit( API( spath ), API( password ) ) ;

        if not IsStringEmpty( password ) then
          GisPasswordList.Add( Name, 'password', password ) ;
      end ;

    except
      if instanceDLL < 0 then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEPASSWORD ), Path, 0 )
      else
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;

    if instanceDLL < 0 then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;

    libLoaded := True ;

    if FIsNativeGridImage then begin
      fnSetGridBand( instanceDLL, SmallInt( Params.Pixel.GridBand -1) ) ;
      FAntialias := True ;
    end
    else begin
      if FIsGridImage then begin
        IsGridImage := True ;
      end ;
    end ;

    try

      if stripBufferSize > 0 then begin

        height := fnImageHeight( instanceDLL ) ;
        width  := fnImageWidth ( instanceDLL ) ;

        case dataType of
          SID_DATATYPE_UINT8,
          SID_DATATYPE_SINT8,
          SID_DATATYPE_UINT16,
          SID_DATATYPE_SINT16 : line_width := 3*width ;
          SID_DATATYPE_UINT32,
          SID_DATATYPE_SINT32,
          SID_DATATYPE_FLOAT32 : line_width := 4*width ;
          SID_DATATYPE_FLOAT64 : line_width := 8*width ;
        else
          line_width := 3*width ;
        end ;

        lineBytesWidth := line_width ;

        if assigned( Stream ) then
          stripHeight := height
        else
          stripHeight := SID_MAX_STRIP_BUFFER_SIZE div line_width ;

        if stripHeight > height then
          stripHeight := height ;

        sbs := stripHeight * line_width ;
        {$IFDEF CLR}
        if (not AssignedPtr(stripBuffer)) or (sbs > stripBufferSize) then begin
        {$ELSE}
        if (not assigned(stripBuffer)) or (sbs > stripBufferSize) then begin
        {$ENDIF}
          stripBufferSize := sbs ;

          {$IFDEF CLR}
            if AssignedPtr( stripBuffer ) then
              Marshal.FreeHGlobal( stripBuffer ) ;
            stripBuffer := Marshal.AllocHGlobal( stripBufferSize ) ;
            fnSetBuffer( instanceDLL, stripBuffer ) ;
          {$ENDIF}
          {$IFDEF DCC}
            SetLength( stripBuffer, stripBufferSize ) ;
            fnSetBuffer( instanceDLL, @stripBuffer[0] ) ;
          {$ENDIF}
          {$IFDEF JAVA}
            stripBuffer := new com.sun.jna.Memory( stripBufferSize ) ;
            fnSetBuffer( instanceDLL, stripBuffer ) ;
          {$ENDIF}

        end ;
      end ;
    except
      libLoaded := False ;
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), SID_LIBNAME, 0 ) ;
    end ;
  end ;

  function TGIS_LayerMrSID.getLine( const _buffer : TBytes  ;
                                    const _offset : Integer ;
                                    const _linenr : Integer ;
                                    const _start  : Integer ;
                                    const _bytes  : Integer
                                    ) : Integer ;
  var
    loc_start     : Integer ;
    {$IFDEF CLR}
      src_line    : IntPtr  ;
      src_linex   : Integer ;
    {$ENDIF}
    {$IFDEF JAVA}
      src_line    : com.sun.jna.Pointer  ;
      src_linex   : Integer ;
    {$ENDIF}
    {$IFDEF DCC}
      src_line    : PByte   ;
    {$ENDIF}
    xl, w         : Integer ;
    bytes4        : Integer ;
    lines_needed  : Integer ;
    zoomed_height : Integer ;
    file_strip_start : Integer ;
    linenr        : Integer ;

    procedure copy_bytes ;
    var
      i1 : Integer ;
      {$IFDEF OXYGENE}
      {$ELSE}
        src : PByte ;
      {$ENDIF}
    begin
      {$IFDEF OXYGENE}
      {$ELSE}
        src := PByte(NativeInt(src_line) );
      {$ENDIF}
      for i1 := 0 to _bytes -1 do
        {$IFDEF OXYGENE}
          _buffer[_offset+i1] := Marshal.ReadByte( src_line, src_linex+i1 ) ;
        {$ELSE}
          PByte(NativeInt(_buffer) +i1)^ := PByte(NativeInt(src) +i1)^;
        {$ENDIF}
    end ;
  begin

    try
      if not libLoaded then
        reloadLibrary ;

      xl := _start div 3 ;
      w  := _bytes div 3 ;

      linenr := _linenr ;
      if linenr = (stripStart +stripHeight) then
        dec(linenr) ;

      if (stripLinesNo > 0) and (actualZoom = lastZoom) then begin
        if (linenr >= stripStart) and (linenr < stripStart +stripLinesNo) then
        begin
          if (xl >= xLeft) and ((xl +w) <= (xLeft +actualWidth)) then begin
            loc_start := linenr -stripStart ;
            bytes4 := actualWidth * 3;
            {$IFDEF OXYGENE}
              src_line  := stripBuffer ;
              src_linex := loc_start*bytes4 + ( 3*xl -3*xLeft ) ;
            {$ELSE}
              src_line := PByte(NativeInt(stripBuffer) +loc_start*bytes4
                                +(3*xl -3*xLeft)) ;
            {$ENDIF}
            copy_bytes ;
            Result := _bytes ;
            exit ;
          end ;
        end ;
      end ;
      bytes4 := ((_bytes +3) div 4) * 4 ;

      zoomed_height := (FBitHeight + actualZoom -1) div actualZoom ;
      stripStart := linenr ;
      loc_start := 0 ;
      lines_needed := stripHeight ;
      stripLinesNo := stripHeight ;

      if Int64(lines_needed) * Int64(_bytes) > stripBufferSize then begin
        lines_needed := stripBufferSize div _bytes ;
        stripLinesNo := lines_needed ;
      end ;

      lastZoom := actualZoom ;
      xLeft := xl ;
      actualWidth := w ;

      file_strip_start := stripStart ;
      if file_strip_start + lines_needed >  zoomed_height then begin
        if actualZoom > 1 then
          if file_strip_start > 0 then dec(file_strip_start) ;
        lines_needed := zoomed_height -file_strip_start;
      end ;

      fnLoadStrip( instanceDLL, file_strip_start, lines_needed, xl, w ) ;

      {$IFDEF OXYGENE}
        src_line  := stripBuffer ;
        src_linex := loc_start * bytes4 ;
      {$ELSE}
        src_line := PByte( NativeInt(stripBuffer) + loc_start*bytes4 ) ;
      {$ENDIF}
      copy_bytes ;

      Result := _bytes ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end;

  function TGIS_LayerMrSID.getLinePixels( const _buffer   : TGIS_Pixels  ;
                                          const _offset   : Integer ;
                                          const _linenr   : Integer ;
                                          const _pixStart : Integer ;
                                          const _pixCount : Integer
                                         ) : Integer;
  var
    loc_start     : Integer ;
    {$IFDEF CLR}
      src_line    : IntPtr  ;
      src_linex   : Integer ;
    {$ENDIF}
    {$IFDEF JAVA}
      src_line    : com.sun.jna.Pointer  ;
      src_linex   : Integer ;
    {$ENDIF}
    {$IFDEF DCC}
      src_line    : PByte   ;
    {$ENDIF}
    xl, w         : Integer ;
    bytes4        : Integer ;
    bytes         : Integer ;
    lines_needed  : Integer ;
    zoomed_height : Integer ;
    file_strip_start : Integer ;
    linenr        : Integer ;

    procedure convert_bytes2pixels ;
    var
      i1 : Integer ;
      {$IFDEF OXYGENE}
      {$ELSE}
        src : PByte ;
      {$ENDIF}
    begin
      {$IFDEF OXYGENE}
      {$ELSE}
        src := PByte(NativeInt(src_line) );
      {$ENDIF}
      for i1 := 0 to _pixCount -1 do
        {$IFDEF OXYGENE}
          _buffer[_offset+i1] :=
            (Integer(Marshal.ReadByte( src_line, src_linex+i1*3  +0)) shl 00) or
            (Integer(Marshal.ReadByte( src_line, src_linex+i1*3  +1)) shl 08) or
            (Integer(Marshal.ReadByte( src_line, src_linex+i1*3  +2)) shl 16) or
            Integer($FF000000) ;
        {$ELSE}
          _buffer[_offset+i1] :=
            (Integer(PByte(NativeInt(src) +i1*3 +0)^) shl 00) or
            (Integer(PByte(NativeInt(src) +i1*3 +1)^) shl 08) or
            (Integer(PByte(NativeInt(src) +i1*3 +2)^) shl 16) or
            Integer($FF000000) ;
        {$ENDIF}
    end ;
  begin

    try
      if not libLoaded then
        reloadLibrary ;

      xl := _pixStart ;
      w  := _pixCount ;

      linenr := _linenr ;
      if linenr = (stripStart +stripHeight) then
        dec(linenr) ;

      if (stripLinesNo > 0) and (actualZoom = lastZoom) then begin
        if (linenr >= stripStart) and (linenr < stripStart +stripLinesNo) then
        begin
          if (xl >= xLeft) and ((xl +w) <= (xLeft +actualWidth)) then begin
            loc_start := linenr -stripStart ;
            bytes4 := actualWidth * 3;
            {$IFDEF OXYGENE}
              src_line  := stripBuffer ;
              src_linex := loc_start*bytes4 + ( 3*xl -3*xLeft ) ;
            {$ELSE}
              src_line := PByte(NativeInt(stripBuffer) +loc_start*bytes4
                                +(3*xl -3*xLeft)) ;
            {$ENDIF}
            convert_bytes2pixels ;
            Result := _pixCount ;
            exit ;
          end ;
        end ;
      end ;
      bytes := _pixCount*3 ;
      bytes4 := ((bytes +3) div 4) * 4 ;

      zoomed_height := (FBitHeight + actualZoom -1) div actualZoom ;
      stripStart := linenr ;
      loc_start := 0 ;
      lines_needed := stripHeight ;
      stripLinesNo := stripHeight ;

      if Int64(lines_needed) * Int64(bytes) > stripBufferSize then begin
        lines_needed := stripBufferSize div bytes ;
        stripLinesNo := lines_needed ;
      end ;

      lastZoom := actualZoom ;
      xLeft := xl ;
      actualWidth := w ;

      file_strip_start := stripStart ;
      if file_strip_start + lines_needed >  zoomed_height then begin
        if actualZoom > 1 then
          if file_strip_start > 0 then dec(file_strip_start) ;
        lines_needed := zoomed_height -file_strip_start;
      end ;

      fnLoadStrip( instanceDLL, file_strip_start, lines_needed, xl, w ) ;

      {$IFDEF OXYGENE}
        src_line  := stripBuffer ;
        src_linex := loc_start * bytes4 ;
      {$ELSE}
        src_line := PByte( NativeInt(stripBuffer) + loc_start*bytes4 ) ;
      {$ENDIF}
      convert_bytes2pixels ;

            Result := _pixCount ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end;



  function  TGIS_LayerMrSID.getNativeLine( const _buffer   : TGIS_SingleArray ;
                                           const _linenr   : Integer          ;
                                           const _startIdx : Integer          ;
                                           const _count    : Integer
                                         ) : Integer ;
  var
    loc_start     : Integer ;
    {$IFDEF CLR}
      src_line    : IntPtr  ;
      src_linex   : Integer ;
    {$ENDIF}
    {$IFDEF JAVA}
      src_line    : com.sun.jna.Pointer  ;
      src_linex   : Integer ;
    {$ENDIF}
    {$IFDEF DCC}
      src_line    : PByte   ;
    {$ENDIF}
    xl, w         : Integer ;
    bytes         : Integer ;
    lines_needed  : Integer ;
    zoomed_height : Integer ;
    file_strip_start : Integer ;
    bpp : Integer ;

    procedure copy_bytes_u8 ;
    var
      maxidx : Integer ;
      i : Integer ;
      {$IFDEF OXYGENE}
      {$ELSE}
        srcb : PByte ;
      {$ENDIF}
      offset : Integer ;
    begin
      {$IFDEF OXYGENE}
      {$ELSE}
        srcb := PByte(src_line) ;
      {$ENDIF}
      maxidx  := actualWidth -1 ;

      offset := 0 ;
      for i := 0 to maxidx do begin
        {$IFDEF OXYGENE}
          _buffer[i] := Marshal.ReadByte( src_line, src_linex+offset ) ;
        {$ELSE}
          PSingle(NativeInt(_buffer) +sizeOf(Single)*i)^ := PByte(NativeInt(srcb) +offset)^ ;
        {$ENDIF}
        offset := offset +bpp ;
      end ;

    end ;

    procedure copy_bytes_s8 ;
    var
      maxidx : Integer ;
      i : Integer ;
      {$IFDEF OXYGENE}
      {$ELSE}
        srcb : PShortint ;
      {$ENDIF}
      offset : Integer ;
    begin
      {$IFDEF OXYGENE}
      {$ELSE}
        srcb := PShortint(src_line) ;
      {$ENDIF}
      maxidx  := actualWidth -1 ;
      offset := 0 ;
      for i := 0 to maxidx do begin
        {$IFDEF OXYGENE}
          _buffer[i] := ShortInt( Marshal.ReadByte( src_line, src_linex+offset ) ) ;
        {$ELSE}
          PSingle(NativeInt(_buffer) +sizeOf(Single)*i)^ := PShortInt(NativeInt(srcb) +offset)^ ;
        {$ENDIF}
        offset := offset +bpp ;
      end ;
    end ;

    procedure copy_bytes_u16 ;
    var
      maxidx : Integer ;
      i : Integer ;
      {$IFDEF OXYGENE}
      {$ELSE}
        srcs : PWord ;
      {$ENDIF}
      offset : Integer ;
    begin
      {$IFDEF OXYGENE}
      {$ELSE}
        srcs := PWord(src_line) ;
      {$ENDIF}
      maxidx := actualWidth -1 ;
      offset := 0 ;
      for i := 0 to maxidx do begin
        {$IFDEF OXYGENE}
          _buffer[i] := Word( Marshal.ReadByte(src_line, src_linex+offset ) or
                              (Marshal.ReadByte( src_line, src_linex+offset+1 ) shl 8)
                            ) ;
        {$ELSE}
          PSingle(NativeInt(_buffer) +sizeOf(Single)*i)^ := PWord(NativeInt(srcs) +offset)^ ;
        {$ENDIF}
        offset := offset +bpp ;
      end ;

    end ;

    procedure copy_bytes_s16 ;
    var
      maxidx : Integer ;
      i : Integer ;
      {$IFDEF OXYGENE}
      {$ELSE}
        srcs : PSmallint ;
      {$ENDIF}
      offset : Integer ;
    begin
      {$IFDEF OXYGENE}
      {$ELSE}
        srcs := PSmallint(src_line) ;
      {$ENDIF}
      maxidx := actualWidth -1 ;
      offset := 0 ;
      for i := 0 to maxidx do begin
        {$IFDEF OXYGENE}
          _buffer[i] := SmallInt( Marshal.ReadByte(src_line, src_linex+offset ) or
                                  (Marshal.ReadByte(src_line, src_linex+offset+1 ) shl 8)
                                ) ;
        {$ELSE}
          PSingle(NativeInt(_buffer) +sizeOf(Single)*i)^ := PSmallInt(NativeInt(srcs) +offset)^ ;
        {$ENDIF}
        offset := offset +bpp ;
      end ;

    end ;

    procedure copy_bytes_u32 ;
    var
      maxidx : Integer ;
      i : Integer ;
      {$IFDEF OXYGENE}
      {$ELSE}
        srcs : PLongword  ;
      {$ENDIF}
      offset : Integer ;
    begin
      {$IFDEF OXYGENE}
      {$ELSE}
        srcs := PLongword(src_line) ;
      {$ENDIF}
      maxidx := actualWidth -1 ;
      offset := 0 ;
      for i := 0 to maxidx do begin
        {$IFDEF OXYGENE}
          _buffer[i] := LongWord( Marshal.ReadByte(src_line, src_linex+offset ) or
                                  (Marshal.ReadByte(src_line, src_linex+offset+1 ) shl 8)
                                ) ;
        {$ELSE}
          PSingle(NativeInt(_buffer) +sizeOf(Single)*i)^ := PLongWord(NativeInt(srcs) +offset)^ ;
        {$ENDIF}
        offset := offset +bpp ;
      end ;

    end ;

    procedure copy_bytes_s32 ;
    var
      maxidx : Integer ;
      i : Integer ;
      {$IFDEF OXYGENE}
      {$ELSE}
        srcs : PInteger ;
      {$ENDIF}
      offset : Integer ;
    begin
      {$IFDEF OXYGENE}
      {$ELSE}
        srcs := PInteger(src_line) ;
      {$ENDIF}
      maxidx := actualWidth -1 ;
      offset := 0 ;
      for i := 0 to maxidx do begin
        {$IFDEF OXYGENE}
          _buffer[i] := Integer( Marshal.ReadByte(src_line,src_linex+offset) or
                                 (Marshal.ReadByte(src_line,src_linex+offset+1) shl 8 ) or
                                 (Marshal.ReadByte(src_line,src_linex+offset+2) shl 16) or
                                 (Marshal.ReadByte(src_line,src_linex+offset+3) shl 24)
                               ) ;
        {$ELSE}
          PSingle(NativeInt(_buffer) +sizeOf(Single)*i)^ := PInteger(NativeInt(srcs) +offset)^ ;
        {$ENDIF}
        offset := offset +bpp ;
      end ;

    end ;

    procedure copy_bytes_f32 ;
    var
      maxidx : Integer ;
      i : Integer ;
      {$IFDEF OXYGENE}
        buf : TBytes ;
        j   : Integer ;
      {$ELSE}
        srcs : PSingle ;
      {$ENDIF}
      offset : Integer ;
    begin
      {$IFDEF OXYGENE}
        SetLength( buf, 4 ) ;
      {$ELSE}
        srcs := PSingle(src_line) ;
      {$ENDIF}
      maxidx := actualWidth -1 ;
      offset := 0 ;
      for i := 0 to maxidx do begin
        {$IFDEF OXYGENE}
          for j := 0 to 3 do
            buf[j] := Marshal.ReadByte( src_line, src_linex+offset+j ) ;

          _buffer[i] := BitConverter.ToSingle( buf, 0 ) ;
        {$ELSE}
          PSingle(NativeInt(_buffer) +sizeOf(Single)*i)^ := PSingle(NativeInt(srcs) +offset)^ ;
        {$ENDIF}
        offset := offset +bpp ;
      end ;

    end ;

    procedure copy_bytes_f64 ;
    var
      maxidx : Integer ;
      i : Integer ;
      {$IFDEF OXYGENE}
        buf : TBytes ;
        j   : Integer ;
      {$ELSE}
        srcs : PDouble ;
      {$ENDIF}
      offset : Integer ;
    begin
      {$IFDEF OXYGENE}
        SetLength( buf, 8 ) ;
      {$ELSE}
        srcs := PDouble(src_line) ;
      {$ENDIF}
      maxidx := actualWidth -1 ;
      offset := 0 ;
      for i := 0 to maxidx do begin
        {$IFDEF OXYGENE}
          for j := 0 to 7 do
            buf[j] := Marshal.ReadByte( src_line, src_linex+offset+j ) ;
          _buffer[i] := BitConverter.ToDouble( buf, 0 ) ;
        {$ELSE}
          PSingle(NativeInt(_buffer) +sizeOf(Single)*i)^ := PDouble(NativeInt(srcs) +offset)^ ;
        {$ENDIF}
        offset := offset +bpp ;
      end ;

    end ;

  begin

    if not FIsNativeGridImage then begin
      Result := inherited getNativeLine( _buffer, _linenr, _startIdx, _count  ) ;
      exit ;
    end ;

    try
      if not libLoaded then
        reloadLibrary ;

      xl := _startIdx ;
      w  := _count ;
      Result := w ;

      case dataType of
        SID_DATATYPE_UINT8,
        SID_DATATYPE_SINT8   : bpp := 1 ;
        SID_DATATYPE_UINT16,
        SID_DATATYPE_SINT16  : bpp := 2 ;
        SID_DATATYPE_UINT32,
        SID_DATATYPE_SINT32,
        SID_DATATYPE_FLOAT32 : bpp := 4 ;
        SID_DATATYPE_FLOAT64 : bpp := 8 ;
      else
        exit ;
      end ;

      if actualWidth = 0 then
        bytes := w*bpp
      else
        bytes := actualWidth * bpp;

      if (stripLinesNo > 0) and (actualZoom = lastZoom) then begin
        if (_linenr >= stripStart) and (_linenr < stripStart +stripLinesNo) then
        begin
          if (xl >= xLeft) and ((xl +w) <= (xLeft +actualWidth)) then begin
            loc_start := _linenr -stripStart ;
            {$IFDEF OXYGENE}
              src_line  := stripBuffer ;
              src_linex := loc_start*bytes + ( bpp*xl -bpp*xLeft ) ;
            {$ELSE}
              src_line := PByte(NativeInt(stripBuffer) +loc_start*bytes
                                +(bpp*xl -bpp*xLeft)) ;
            {$ENDIF}
            case dataType of
              SID_DATATYPE_UINT8   : copy_bytes_u8 ;
              SID_DATATYPE_SINT8   : copy_bytes_s8 ;
              SID_DATATYPE_UINT16  : copy_bytes_u16 ;
              SID_DATATYPE_SINT16  : copy_bytes_s16 ;
              SID_DATATYPE_UINT32  : copy_bytes_u32 ;
              SID_DATATYPE_SINT32  : copy_bytes_s32 ;
              SID_DATATYPE_FLOAT32 : copy_bytes_f32 ;
              SID_DATATYPE_FLOAT64 : copy_bytes_f64 ;
            end ;
//            copy_bytes ;
//            Result := _bytes ;
            exit ;
          end ;
        end ;
      end ;

      zoomed_height := TruncS( ( 1.0 * ( FBitHeight + actualZoom - 1 ) ) / actualZoom ) ;

      stripStart := _linenr ;
      loc_start := 0 ;

      lines_needed := stripHeight ;
      stripLinesNo := stripHeight ;

      if Int64(lines_needed) * Int64(bytes) > stripBufferSize then begin
        lines_needed := stripBufferSize div bytes ;
        stripLinesNo := lines_needed ;
      end ;

      lastZoom := actualZoom ;
      xLeft := xl ;
      actualWidth := w ;

      file_strip_start := stripStart ;
      if file_strip_start + lines_needed >  zoomed_height then begin
        if actualZoom > 1 then
          if file_strip_start > 0 then dec(file_strip_start) ;
        lines_needed := zoomed_height -file_strip_start;
      end ;

      fnLoadStrip( instanceDLL, file_strip_start, lines_needed, xl, w ) ;

      {$IFDEF OXYGENE}
        src_line  := stripBuffer ;
        src_linex := loc_start * bytes ;
      {$ELSE}
        src_line := PByte( NativeInt(stripBuffer) + loc_start*bytes ) ;
      {$ENDIF}
      case dataType of
        SID_DATATYPE_UINT8   : copy_bytes_u8 ;
        SID_DATATYPE_SINT8   : copy_bytes_s8 ;
        SID_DATATYPE_UINT16  : copy_bytes_u16 ;
        SID_DATATYPE_SINT16  : copy_bytes_s16 ;
        SID_DATATYPE_UINT32  : copy_bytes_u32 ;
        SID_DATATYPE_SINT32  : copy_bytes_s32 ;
        SID_DATATYPE_FLOAT32 : copy_bytes_f32 ;
        SID_DATATYPE_FLOAT64 : copy_bytes_f64 ;
      end ;
//      copy_bytes ;

//      Result := _bytes ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end;

  procedure TGIS_LayerMrSID.Alive ;
  begin
    if not libLoaded then
      reloadLibrary ;
  end ;

  function TGIS_LayerMrSID.DormantGain
    : Integer ;
  begin
    case DormantMode of
      TGIS_LayerDormantMode.Off :
        Result := 0;
      else
        Result := 5 ;
    end ;
  end ;

  procedure TGIS_LayerMrSID.Dormant ;
  begin
    if DormantMode = TGIS_LayerDormantMode.Off then
      exit ;

    {$IFDEF JAVA}
      if libHandle_SID <> nil then begin

        if instanceDLL <> 0 then
          fnFree( instanceDLL ) ;

        stripBuffer := nil ;
        instanceDLL := 0;
        libLoaded := false ;
        stripLinesNo := 0 ;
        actualZoom := 1 ;
      end ;
    {$ELSE}
      if libHandle_SID <> 0 then begin

        if instanceDLL <> 0 then
          fnFree( instanceDLL ) ;

        {$IFDEF CLR}
        if AssignedPtr( stripBuffer ) then
          Marshal.FreeHGlobal( stripBuffer ) ;
          stripBuffer := 0;
        {$ENDIF}
        {$IFDEF JAVA}
        stripBuffer := nil ;
        {$ENDIF}
        {$IFDEF DCC}
        stripBuffer := nil ;
        {$ENDIF}
        instanceDLL := 0;
        libLoaded := false ;
        stripLinesNo := 0 ;
        actualZoom := 1 ;
      end ;
    {$ENDIF}
  end ;

  procedure TGIS_LayerMrSID.DecodeImageInternal(
    const _buffer : TBytes
  ) ;
  var
    i, j : Integer ;
  begin
    setUp ;
    try
      fnLoadStrip( instanceDLL, 0, FBitHeight, 0, FBitWidth ) ;
      for i := 0 to FBitHeight - 1 do
        for j := 0 to FBitWidth-1 do
          {$IFDEF OXYGENE}
          _buffer[ i*FBitWidth + j ] := Marshal.ReadByte( stripBuffer, i*lineBytesWidth + j*3 ) ;
          {$ELSE}
          _buffer[ i*FBitWidth + j ] := stripBuffer[ i*lineBytesWidth + j*3 ] ;
          {$ENDIF}

    finally
      Dormant ;
    end;
  end;

  class procedure Unit_GisLayerMrSID.SelfRegisterLayer() ;
    begin
      GisKeyList.Add( GIS_KEY_MRSID, GIS_KEY_PASS, KEY_MRSID ) ;
      RegisterLayer( 'DK-MRSID', 'Lizardtech MrSID', TGIS_LayerMrSID,
                     '.sid'
                     ,TGIS_RegisteredLayerType.Pixel, TGIS_RegisteredFormatType.Local,
                     [ TGIS_RegisteredOperationType.Read ],
                     True
                   ) ;
      RegisterLayer( 'DK-J2K_MRSID', 'JPEG 2000 (via MRSID)', TGIS_LayerMrSID,
                     '.jp2'
                     ,TGIS_RegisteredLayerType.Pixel, TGIS_RegisteredFormatType.Local,
                     [ TGIS_RegisteredOperationType.Read ],
                     GIS_LOWER_LAYER_PRIORITY, True
                   ) ;
      RegisterLayer( 'DK-J2K_MRSID', 'JPEG 2000 (via MRSID)', TGIS_LayerMrSID,
                     '.j2k;.jpf;.jpx;.jpc;.j2c'
                     ,TGIS_RegisteredLayerType.Pixel, TGIS_RegisteredFormatType.Local,
                     [ TGIS_RegisteredOperationType.Read ],
                     False
                   ) ;
    end ;

{$IFNDEF OXYGENE}
  initialization
    Unit_GisLayerMrSID.SelfRegisterLayer() ;
{$ENDIF}

{==================================== END =====================================}
end.

