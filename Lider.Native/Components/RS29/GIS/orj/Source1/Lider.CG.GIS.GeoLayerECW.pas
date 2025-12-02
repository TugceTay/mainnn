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
  Encapsulation of a ECW Layer (Earth Resource Mapping Technology).
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerECW ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerECW"'}
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

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoFunctions,
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
  GisLayerECW = class
    public
      class procedure SelfRegisterLayer() ;
  end;

  { ECW Library interface structure types.
  }
  {#gendoc:hide}
  TGIS_LayerECW_CellSizeUnits = {$IFDEF OXYGENE} public {$ENDIF}
                                ( ECW_CELL_UNITS_INVALID,
                                  ECW_CELL_UNITS_METERS,
                                  ECW_CELL_UNITS_DEGREES,
                                  ECW_CELL_UNITS_FEET,
                                  ECW_CELL_UNITS_UNKNOWN
                                ) ;

  { Return status form reading routines. NCSECW_READ_CANCELLED used when
    view was canceled.
  }
  {#gendoc:hide}
  TGIS_LayerECW_NCSEcwReadStatus = {$IFDEF OXYGENE} public {$ENDIF}
                                   ( NCSECW_READ_OK,
                                     NCSECW_READ_FAILED,
                                     NCSECW_READ_CANCELLED
                                   ) ;

  {$IFDEF CLR}
    [StructLayout(LayoutKind.Sequential)]
  {$ENDIF}
  { ECW Library interface structure types.
  }
  {#gendoc:hide}
  TGIS_LayerECW_NCSViewFileInfo = record
    nSizeX            : LongInt       ;
    nSizeY            : LongInt       ;
    nBands            : Word          ;
    nCompressionRate  : Word          ;
    eCellSizeUnits    : Integer       ; //CellSizeUnits in C Library
    fCellIncrementX   : Double        ;
    fCellIncrementY   : Double        ;
    fOriginX          : Double        ;
    fOriginY          : Double        ;
    {$IFDEF OXYGENE}
      {$IFDEF CLR}
      [MarshalAs(UnmanagedType.LPStr)]
      {$ENDIF}
      szDatum         : String        ;
    {$ELSE}
      szDatum         : PAnsiChar     ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      {$IFDEF CLR}
      [MarshalAs(UnmanagedType.LPStr)]
      {$ENDIF}
      szProjection    : String        ;
    {$ELSE}
      szProjection    : PAnsiChar     ;
    {$ENDIF}
  end ;

  {#gendoc:hide}
  TGIS_LayerECW_NCSFileBandInfo = record
    nBits : Word ;
    bSigned : Boolean ;
    {$IFDEF OXYGENE}
      {$IFDEF CLR}
      [MarshalAs(UnmanagedType.LPStr)]
      {$ENDIF}
      szDesc    : String        ;
    {$ELSE}
      szDesc     : PAnsiChar     ;
    {$ENDIF}
  end ;
  {$IFDEF OXYGENE}
  {$ELSE}
    {#gendoc:hide}
    PGIS_LayerECW_NCSFileBandInfo     = ^TGIS_LayerECW_NCSFileBandInfo    ;
    {#gendoc:hide}
    PPGIS_LayerECW_NCSFileBandInfo    = ^PGIS_LayerECW_NCSFileBandInfo    ;
  {$ENDIF}

  {$IFDEF JAVA}
    ByReference nested in TGIS_LayerECW_NCSViewFileInfoEx
      = public class(
          TGIS_LayerECW_NCSViewFileInfoEx, com.sun.jna.Structure.ByReference
        )
    end;

    ByValue nested in TGIS_LayerECW_NCSViewFileInfoEx
      = public class(
          TGIS_LayerECW_NCSViewFileInfoEx, com.sun.jna.Structure.ByValue
        )
    end;

    {#gendoc:hide}
    TGIS_LayerECW_NCSViewFileInfoEx = public class( com.sun.jna.Structure )
      public
        var nSizeX              : Integer       ;
        var nSizeY              : Integer       ;
        var nBands              : Int16         ;
        var nCompressionRate    : Int16         ;
        var eCellSizeUnits      : Integer       ;
        var fCellIncrementX     : Double        ;
        var fCellIncrementY     : Double        ;
        var fOriginX            : Double        ;
        var fOriginY            : Double        ;
        var szDatum             : String        ;
        var szProjection        : String        ;
        var fCWRotationDegrees  : Double       ;
        var eColorSpace         : Integer      ;
        var eCellType           : Integer      ;
        var pBands              : com.sun.jna.Pointer ;
      public
        constructor Create ;
        constructor Create( _peer : com.sun.jna.Pointer ) ;
      protected
        method getFieldOrder: java.util.List<String>; override;
        property FieldOrder: java.util.List<String> read getFieldOrder ;
    end ;

    ByReference nested in TGIS_LayerECW_NCSFileViewSetInfo
      = public class(
          TGIS_LayerECW_NCSFileViewSetInfo, com.sun.jna.Structure.ByReference
        )
    end;

    ByValue nested in TGIS_LayerECW_NCSFileViewSetInfo
      = public class(
          TGIS_LayerECW_NCSFileViewSetInfo, com.sun.jna.Structure.ByValue
        )
    end;

    {#gendoc:hide}
    TGIS_LayerECW_NCSFileViewSetInfo = public class( com.sun.jna.Structure )
      public
        var pClientData       : com.sun.jna.Pointer ;
        var nBands            : Integer         ;
        var pBandList         : com.sun.jna.Pointer ;
        var nTopX             : Integer         ;
        var nLeftY            : Integer         ;
        var nBottomX          : Integer         ;
        var nRightY           : Integer         ;
        var nSizeX            : Integer         ;
        var nSizeY            : Integer         ;
        var nBlocksInView     : Integer         ;
        var nBlocksAvailable  : Integer         ;
        var nBlocksAvailableAtSetView : Integer         ;
        var nMissedBlockDuringRead  : Integer         ;
        var fTopX             : Double        ;
        var fTopY             : Double        ;
        var fBottomX          : Double        ;
        var fBottomY          : Double        ;
      public
        constructor Create ;
        constructor Create( _peer : com.sun.jna.Pointer ) ;
      protected
        method getFieldOrder: java.util.List<String>; override;
        property FieldOrder: java.util.List<String> read getFieldOrder ;
    end ;
  {$ELSE}
    {#gendoc:hide}
    TGIS_LayerECW_NCSViewFileInfoEx = record
      nSizeX            : LongInt       ;
      nSizeY            : LongInt       ;
      nBands            : Word          ;
      nCompressionRate  : Word          ;
      eCellSizeUnits    : Integer       ; //CellSizeUnits in C Library
      fCellIncrementX   : Double        ;
      fCellIncrementY   : Double        ;
      fOriginX          : Double        ;
      fOriginY          : Double        ;
      {$IFDEF OXYGENE}
        [MarshalAs(UnmanagedType.LPStr)]
        szDatum         : String        ;
      {$ELSE}
        szDatum         : PAnsiChar     ;
      {$ENDIF}
      {$IFDEF OXYGENE}
        [MarshalAs(UnmanagedType.LPStr)]
        szProjection    : String        ;
      {$ELSE}
        szProjection    : PAnsiChar     ;
      {$ENDIF}
      fCWRotationDegrees : Double       ;
      eColorSpace        : Integer      ;
      eCellType          : Integer      ;
      {$IFDEF CLR}
        pBands           : IntPtr       ;
      {$ENDIF}
      {$IFDEF DCC}
        pBands           : Pointer      ;
      {$ENDIF}
    end ;
    {$IFDEF OXYGENE}
    {$ELSE}
      {#gendoc:hide}
      PGIS_LayerECW_NCSViewFileInfoEx     = ^TGIS_LayerECW_NCSViewFileInfoEx    ;
      {#gendoc:hide}
      PPGIS_LayerECW_NCSViewFileInfoEx    = ^PGIS_LayerECW_NCSViewFileInfoEx    ;
    {$ENDIF}

    {$IFDEF CLR}
      [StructLayout(LayoutKind.Sequential)]
    {$ENDIF}
    { ECW Library interface structure types.
    }
    {#gendoc:hide}
    TGIS_LayerECW_NCSFileViewSetInfo = record
      {$IFDEF CLR}
        pClientData     : IntPtr        ;
      {$ENDIF}
      {$IFDEF JAVA}
        pClientData     : com.sun.jna.Pointer ;
      {$ENDIF}
      {$IFDEF DCC}
        pClientData     : Pointer       ;
      {$ENDIF}
      nBands            : DWORD         ;
      {$IFDEF CLR}
        pBandList       : IntPtr        ;
      {$ENDIF}
      {$IFDEF JAVA}
        pBandList       : com.sun.jna.Pointer ;
      {$ENDIF}
      {$IFDEF DCC}
        pBandList       : PDWORD        ;
      {$ENDIF}
      nTopX             : DWORD         ;
      nLeftY            : DWORD         ;
      nBottomX          : DWORD         ;
      nRightY           : DWORD         ;
      nSizeX            : DWORD         ;
      nSizeY            : DWORD         ;
      nBlocksInView     : DWORD         ;
      nBlocksAvailable  : DWORD         ;
      nBlocksAvailableAtSetView
                        : DWORD         ;
      nMissedBlockDuringRead
                        : DWORD         ;
      fTopX             : Double        ;
      fTopY             : Double        ;
      fBottomX          : Double        ;
      fBottomY          : Double        ;
    end ;

    {$IFDEF OXYGENE}
    {$ELSE}
      {#gendoc:hide}
      PGIS_LayerECW_NCSViewFileInfo     = ^TGIS_LayerECW_NCSViewFileInfo    ;
      {#gendoc:hide}
      PPGIS_LayerECW_NCSViewFileInfo    = ^PGIS_LayerECW_NCSViewFileInfo    ;
      {#gendoc:hide}
      PGIS_LayerECW_NCSFileViewSetInfo  = ^TGIS_LayerECW_NCSFileViewSetInfo ;
      {#gendoc:hide}
      PPGIS_LayerECW_NCSFileViewSetInfo = ^PGIS_LayerECW_NCSFileViewSetInfo ;
    {$ENDIF}
  {$ENDIF}

  {$IFDEF OXYGENE}
    T_viewState nested in TGIS_LayerECW = {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
      public
        firstCall     : Boolean  ;
        linesRead     : Integer  ;
        linesNeeded   : Integer  ;
        portion       : Integer  ;
        actualBlocks  : DWORD    ;
        {$IFDEF OXYGENE}
          buffer      : TBytes   ;
        {$ELSE}
          buffer      : Pointer  ;
        {$ENDIF}
        {$IFDEF OXYGENE}
          workBuffer  : TBytes   ;
          workBufferx : Integer  ;
        {$ELSE}
          workBuffer  : Pointer  ;
        {$ENDIF}
        nBands        : LongWord ;
        {$IFDEF OXYGENE}
          bandList    : array of LongWord ;
        {$ELSE}
          bandList    : Pointer  ;
        {$ENDIF}
        left          : LongWord ;
        top           : LongWord ;
        right         : LongWord ;
        bottom        : LongWord ;
        sizeX         : LongWord ;
        sizeY         : LongWord ;
        &empty        : Boolean  ;
        notComplette  : Boolean  ;
        cancelled     : Boolean  ;
        notRead       : Boolean  ;
        lastreadtime  : Cardinal ;
        viewsets      : Integer  ;
        lastviewrect  : TRect    ;
        inReadProcess : Boolean  ;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Encapsulation of ECW layer.
  /// </summary>
  TGIS_LayerECW = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerPixel )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF} // various private values

      {$IFDEF OXYGENE}
        viewState : T_viewState ;
      {$ELSE}

        /// <summary>
        ///   Actual view state.
        /// </summary>
        viewState : record
          firstCall     : Boolean  ;
          linesRead     : Integer  ;
          linesNeeded   : Integer  ;
          portion       : Integer  ;
          actualBlocks  : DWORD    ;
          {$IFDEF CLR}
            buffer      : TBytes   ;
          {$ELSE}
            buffer      : Pointer  ;
          {$ENDIF}
          {$IFDEF CLR}
            workBuffer  : TBytes   ;
            workBufferx : Integer  ;
          {$ELSE}
            workBuffer  : Pointer  ;
          {$ENDIF}
          nBands        : LongWord ;
          {$IFDEF CLR}
            bandList    : array of LongWord ;
          {$ELSE}
            bandList    : Pointer  ;
          {$ENDIF}
          left          : LongWord ;
          top           : LongWord ;
          right         : LongWord ;
          bottom        : LongWord ;
          sizeX         : LongWord ;
          sizeY         : LongWord ;
          empty         : Boolean  ;
          notComplette  : Boolean  ;
          cancelled     : Boolean  ;
          notRead       : Boolean  ;
          lastreadtime  : Cardinal ;
          viewsets      : Integer  ;
          lastviewrect  : TRect    ;
          inReadProcess : Boolean  ;
        end ;
      {$ENDIF}

        /// <summary>
        ///   Default callback mode for the library. True if callback is enabled.
        /// </summary>
        defaultCallBackMode : Boolean ;

        /// <summary>
        ///   Input file type.
        /// </summary>
        isJP2000 : Boolean ;

        /// <summary>
        ///   True if ECW layer not  used to JPEG2000 reading.
        /// </summary>
        useCallBack : Boolean ;

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
        ///   Strip buffer
        /// </summary>
        stripBuffer : TBytes ;

        /// <summary>
        ///   Strip buffer in band interleaved mode
        /// </summary>
        bilBuffer : TBytes ;

        /// <summary>
        ///   Strip buffer size
        /// </summary>
        stripBufferSize : Integer ;

        /// <summary>
        ///   Line width to four bytes aligned
        /// </summary>
        line4BytesWidth : Integer ;

        /// <summary>
        ///   Zoom at last read
        /// </summary>
        lastZoom : Double ;

        /// <summary>
        ///   Last used in setZoom function
        /// </summary>
        actualZoom : Double ;

        xLeft : Integer ;

        /// <summary>
        ///   Width of actual window - in pixels
        /// </summary>
        actualWidth : Integer ;
        /// <summary>
        ///   Pointer to file view structure
        /// </summary>
        pFileView :
          {$IFDEF CLR}
            IntPtr  ;
          {$ENDIF}
          {$IFDEF JAVA}
            com.sun.jna.Pointer ;
          {$ENDIF}
          {$IFDEF DCC}
            Pointer ;
          {$ENDIF}

        /// <summary>
        ///   Pointer to file view info structure
        /// </summary>
        pFileViewInfo :
          {$IFDEF OXYGENE}
            TGIS_LayerECW_NCSViewFileInfoEx ;
          {$ELSE}
            PGIS_LayerECW_NCSViewFileInfoEx ;
          {$ENDIF}

        /// <summary>
        ///   Units
        /// </summary>
        eCellSizeUnits : TGIS_LayerECW_CellSizeUnits ;

        /// <summary>
        ///   Number of bands
        /// </summary>
        nBands : Integer ;

        /// <summary>
        ///   Number of bytes per pixel
        /// </summary>
        pixelBytes : Integer ;

        /// <summary>
        ///   Number of bytes per pixel
        /// </summary>
        bandBytesDepth : Integer ;

        /// <summary>
        ///   Numbers of selected bands
        /// </summary>
        bandList :
          {$IFDEF OXYGENE}
            array of LongWord ;
          {$ELSE}
            array [0..3] of LongWord ;
          {$ENDIF}
        bandListOld :
          {$IFDEF OXYGENE}
            array of LongWord ;
          {$ELSE}
            array [0..3] of LongWord ;
          {$ENDIF}
    private // property access routines

    private // various private values

      /// <summary>
      ///   Load DLL and bind functions.
      /// </summary>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FILEREAD
      /// </exception>
      procedure bindLibrary ;

      /// <summary>
      ///   Reload DLL unloaded by Dormant procedure.
      /// </summary>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FILEREAD
      /// </exception>
      procedure reloadLibrary ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF} // various protected routines

      /// <inheritdoc/>
      procedure setUp       ; override;

      /// <inheritdoc/>
      function bandsMappingChanging  : Boolean  ; override;

      /// <inheritdoc/>
      procedure getAsyncState ( var _abort  : Boolean ;
                                var _ready  : Boolean ;
                                var _nodata : Boolean
                              ) ; override;

      /// <inheritdoc/>
      function  getLine     ( const _buffer : TBytes  ;
                              const _offset : Integer ;
                              const _linenr : Integer ;
                              const _start  : Integer ;
                              const _bytes  : Integer
                            ) : Integer; override;

      /// <inheritdoc/>
      function  getAlphaLine      (
                                    const _buffer : TBytes  ;
                                    const _offset : Integer ;
                                    const   _linenr : Integer ;
                                    const   _start  : Integer ;
                                    const   _bytes  : Integer
                                  ) : Integer; override;

      /// <inheritdoc/>
      function  getLinePixels     ( const _buffer   : TGIS_Pixels  ;
                                    const _offset   : Integer ;
                                    const _linenr   : Integer ;
                                    const _pixStart : Integer ;
                                    const _pixCount : Integer
                                  ) : Integer; override;

      /// <inheritdoc/>
      function  setFileScale( const _dwidth : Double ;
                              const _swidth : Double
                            ) : Double ; override;

      /// <inheritdoc/>
      procedure setFileView    ( const _viewRect    : TRect) ; override;

      /// <summary>
      ///   Set internal imagery view (now possible to ECW only).
      /// </summary>
      /// <param name="_viewRect">
      ///   define part of image which will be displaying
      /// </param>
      procedure setFileViewBIL ( const _viewRect    : TRect) ;

      /// <inheritdoc/>
      procedure setupParams ; override;

    public // various public routines

      /// <inheritdoc/>
      procedure Alive             ; override;

      /// <inheritdoc/>
      function  DormantGain       : Integer ; override;

      /// <inheritdoc/>
      procedure Dormant           ; override;
    protected

      /// <inheritdoc/>
      procedure doDestroy ; override;
    public // API
      // constructors

      /// <inheritdoc/>
      constructor Create  ; override;
  end ;



//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    System.SyncObjs,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoRegistredLayers,
    Lider.CG.GIS.GeoCsBase,
    Lider.CG.GIS.GeoCsFactory,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoCsProjections ;
{$ENDIF}

const
  // Extension of world file (extension file for pixel layer).
  FILE_EXT_ECW             = '.ecw'  ;

  WORLD_FILE_EXT_ECW_TFW   = '.eww'  ;
  WORLD_FILE_EXT_ECW_TFW2  = '.ecww' ;

  FILE_EXT_JPF             = '.jpf'  ;
  WORLD_FILE_EXT_JPF_TFW   = '.jfw'  ;
  WORLD_FILE_EXT_JPF_TFW2  = '.jpfw' ;

  FILE_EXT_JPX             = '.jpx'  ;
  WORLD_FILE_EXT_JPX_TFW   = '.jxw'  ;
  WORLD_FILE_EXT_JPX_TFW2  = '.jpxw' ;

  FILE_EXT_JPC             = '.jpc'  ;
  WORLD_FILE_EXT_JPC_TFW   = '.jcw'  ;
  WORLD_FILE_EXT_JPC_TFW2  = '.jpcw' ;

  FILE_EXT_J2C             = '.j2c'  ;
  WORLD_FILE_EXT_J2C_TFW   = '.jcw'  ;
  WORLD_FILE_EXT_J2C_TFW2  = '.j2cw' ;

  FILE_EXT_J2X             = '.j2x'  ;
  WORLD_FILE_EXT_J2X_TFW   = '.jxw'  ;
  WORLD_FILE_EXT_J2X_TFW2  = '.j2xw' ;

  FILE_EXT_J2K             = '.j2k'  ;
  WORLD_FILE_EXT_J2K_TFW   = '.jkw'  ;
  WORLD_FILE_EXT_J2K_TFW2  = '.j2kw' ;

  FILE_EXT_JP2             = '.jp2'  ;
  WORLD_FILE_EXT_JP2_TFW   = '.j2w'  ;
  WORLD_FILE_EXT_JP2_TFW2  = '.jp2w' ;

  // ECW library name
  NCS_ECW_LIBNAME           = 'NCSEcw.dll';
  NCS_ECW_LIBNAME_NOEXT     = 'NCSEcw';

  // DLL interface.
  ECW_FUNC_OPENFILEVIEW     = 'NCSOpenFileViewW'     ;
  ECW_PROC_GETVIEWFILEINFO  = 'NCSGetViewFileInfo'  ;
  ECW_PROC_GETVIEWINFO      = 'NCSGetViewInfo'      ;
  ECW_FUNC_SETFILEVIEW      = 'NCSSetFileView'      ;
  ECW_FUNC_READLINERGB      = 'NCSReadViewLineRGB'  ;
  ECW_FUNC_READLINEBGR      = 'NCSReadViewLineBGR'  ;
  ECW_FUNC_READLINEBGRA     = 'NCSReadViewLineBGRA' ;
  ECW_FUNC_READLINEBIL      = 'NCSReadViewLineBIL' ;
  ECW_FUNC_READLINEBILEX    = 'NCSReadViewLineBILEx' ;
  ECW_FUNC_CLOSEFILEVIEW    = 'NCSCloseFileViewEx'  ;
  ECW_FUNC_SETCONFIG        = 'NCSSetConfig'        ;

  ECW_MAX_VIEW_SETS         = 5         ;
  ECW_MAX_WAITING_TIME      = 20 * 1000 ;

  ECW_CACHE_MAXMEM          = 2 ;

  {$IFDEF JAVA}
  type
    T_CallBack = function( _fileView  : com.sun.jna.Pointer ) : TGIS_LayerECW_NCSEcwReadStatus ;

    IECWLibrary = interface (com.sun.jna.Library)

      function  NCSOpenFileViewW(   _ecwFilePath  : com.sun.jna.WString ;
                                   _fileView : com.sun.jna.ptr.PointerByReference ;
                                   _callBack     : T_CallBack
                                 ) : Integer ;

      procedure NCSGetViewFileInfo( _fileView         : com.sun.jna.Pointer  ;
                                    _fileViewInfo : com.sun.jna.ptr.PointerByReference
                                  ) ;

      procedure NCSGetViewInfo( _fileView         : com.sun.jna.Pointer ;
                                _fileViewInfo : com.sun.jna.ptr.PointerByReference
                              ) ;

      function  NCSSetFileView( _fileView     : com.sun.jna.Pointer   ;
                                _nBands       : Integer ;
                                _pBandList    : array of Cardinal ;
                                _nTopX        : Integer ;
                                _nLeftY       : Integer ;
                                _nBottomX     : Integer ;
                                _nRightY      : Integer ;
                                _nSizeX       : Integer ;
                                _nSizeY       : Integer
                              ) : Integer ;

      function  NCSReadViewLineBGR( _fileView     : com.sun.jna.Pointer ;
                                    _pRGBTriplets : com.sun.jna.Pointer
                                  ) : Integer ;

      function  NCSReadViewLineBGRA( _fileView      : com.sun.jna.Pointer ;
                                     _pRGBTripletsA : com.sun.jna.Pointer
                                  ) : Integer ;

      function  NCSReadViewLineBIL( _fileView      : com.sun.jna.Pointer ;
                                      _ppRGBTripletsA : com.sun.jna.Pointer
                                   ) : Integer ;

      function  NCSReadViewLineBILEx( _fileView      : com.sun.jna.Pointer ;
                                      _dataType      : Integer ;
                                      _ppRGBTripletsA : com.sun.jna.Pointer
                                   ) : Integer ;

      function  NCSCloseFileViewEx( _fileView     : com.sun.jna.Pointer ;
                                    _forceFree    : Boolean = False
                                  ) : Integer ;

      function  NCSSetConfig( _mode         : Integer ;
                              _inparam      : Integer
                            ) : Integer ;
    end;

  {$ENDIF}

  {$IFDEF CLR}
    type
      T_CallBack = function( _fileView  : IntPtr ) : TGIS_LayerECW_NCSEcwReadStatus ;

    // externals from ttkMrSID.dll
    [ SuppressUnmanagedCodeSecurity,
      DllImport( NCS_ECW_LIBNAME,
                 CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Unicode,
                 EntryPoint = ECW_FUNC_OPENFILEVIEW
      )
    ]
    function  ncsOpenFileView( _ecwFilePath  : String ;
                               out _fileView : IntPtr ;
                               _callBack     : T_CallBack
                             ) : Integer ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( NCS_ECW_LIBNAME,
                 CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi,
                 EntryPoint = ECW_PROC_GETVIEWFILEINFO
      )
    ]
    procedure ncsGetViewFileInfo( _fileView         : IntPtr ;
                                  out _fileViewInfo : IntPtr
                                ) ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( NCS_ECW_LIBNAME,
                 CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi,
                 EntryPoint = ECW_PROC_GETVIEWINFO
      )
    ]
    procedure ncsGetViewInfo( _fileView         : IntPtr ;
                              out _fileViewInfo : IntPtr
                            ) ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( NCS_ECW_LIBNAME,
                 CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi,
                 EntryPoint = ECW_FUNC_SETFILEVIEW
      )
    ]
    function  ncsSetFileView( _fileView     : IntPtr   ;
                              _nBands       : LongWord ;
                              _pBandList    : array of Cardinal ;
                              _nTopX        : LongWord ;
                              _nLeftY       : LongWord ;
                              _nBottomX     : LongWord ;
                              _nRightY      : LongWord ;
                              _nSizeX       : LongWord ;
                              _nSizeY       : LongWord
                            ) : Integer ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( NCS_ECW_LIBNAME,
                 CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi,
                 EntryPoint = ECW_FUNC_READLINEBGR
      )
    ]
    function  ncsReadViewLineBGR( _fileView     : IntPtr ;
                                  _pRGBTriplets : IntPtr
                                ) : Integer ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( NCS_ECW_LIBNAME,
                 CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi,
                 EntryPoint = ECW_FUNC_READLINEBGRA
      )
    ]
    function  ncsReadViewLineBGRA( _fileView      : IntPtr ;
                                   _pRGBTripletsA : IntPtr
                                ) : Integer ; external ;
    [ SuppressUnmanagedCodeSecurity,
      DllImport( NCS_ECW_LIBNAME,
                 CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi,
                 EntryPoint = ECW_FUNC_READLINEBIL
      )
    ]
    function  ncsReadViewLineBIL( _fileView      : IntPtr ;
                                   _ppRGBTripletsA : IntPtr
                                ) : Integer ; external ;
    [ SuppressUnmanagedCodeSecurity,
      DllImport( NCS_ECW_LIBNAME,
                 CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi,
                 EntryPoint = ECW_FUNC_READLINEBILEX
      )
    ]
    function  ncsReadViewLineBILEx( _fileView      : IntPtr ;
                                    _dataType      : Integer ;
                                   _ppRGBTripletsA : IntPtr
                                ) : Integer ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( NCS_ECW_LIBNAME,
                 CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi,
                 EntryPoint = ECW_FUNC_CLOSEFILEVIEW
      )
    ]
    function  ncsCloseFileView( _fileView     : IntPtr ;
                                _forceFree    : Boolean = False
                              ) : Integer ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( NCS_ECW_LIBNAME,
                 CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi,
                 EntryPoint = ECW_FUNC_SETCONFIG
      )
    ]
    function  ncsSetConfig( _mode         : Integer ;
                            _inparam      : DWORD
                          ) : Integer ; external ;
  {$ENDIF}
  {$IFDEF DCC}
    type
      T_NCScbmOpenFileView     = function ( _ecwFilePath  : PWideString ;
                                            _fileView     : Pointer     ;
                                            _callBack     : Pointer
                                         ) : Integer; cdecl ;
      T_NCScbmGetViewFileInfo  = procedure( _fileView     : Pointer ;
                                            _fileViewInfo : PPGIS_LayerECW_NCSViewFileInfo
                                          ) ; cdecl ;
      T_NCScbmGetViewInfo      = function( _fileView     : Pointer ;
                                            _fileViewInfo : PPGIS_LayerECW_NCSFileViewSetInfo
                                          ) : integer ; cdecl ;
      T_NCScbmSetFileView      = function ( _fileView     : Pointer  ;
                                            _nBands       : LongWord ;
                                            _pBandList    : Pointer  ;
                                            _nTopX        : LongWord ;
                                            _nLeftY       : LongWord ;
                                            _nBottomX     : LongWord ;
                                            _nRightY      : LongWord ;
                                            _nSizeX       : LongWord ;
                                            _nSizeY       : LongWord
                                          ) : Integer ; cdecl ;
      T_NCScbmReadViewLineRGB  = function ( _fileView      : Pointer  ;
                                            _pRGBTriplets  : PByte
                                          ) : Integer ; cdecl ;
      T_NCScbmReadViewLineBGR  = function ( _fileView      : Pointer  ;
                                            _pRGBTriplets  : PByte
                                          ) : Integer ; cdecl ;
      T_NCScbmReadViewLineBGRA = function ( _fileView      : Pointer  ;
                                            _pRGBATripletsA : PByte
                                          ) : Integer ; cdecl ;
      T_NCScbmReadViewLineBIL  = function ( _fileView      : Pointer  ;
                                            _pRGBATripletsA : PByte
                                          ) : Integer ; cdecl ;
      T_NCScbmReadViewLineBILEx = function ( _fileView      : Pointer  ;
                                             _dataType      : Integer  ;
                                            _pRGBATripletsA : PByte
                                          ) : Integer ; cdecl ;
      T_NCScbmCloseFileView    = function ( _fileView      : Pointer ;
                                            _forceFree     : Boolean = False
                                          ) : Integer ; cdecl ;
      T_NCSecwSetConfig        = function ( _mode          : Integer ;
                                            _inparam       : DWORD
                                          ) : Integer ; cdecl ;
  {$ENDIF}

var
  {$IFDEF OXYGENE}
    instanceCount : Integer := 0 ;
    {$IFDEF JAVA}
      libHandle     : IECWLibrary ;
    {$ELSE}
      libHandle     : THandle ;
    {$ENDIF}
  {$ELSE}
    instanceCount : Integer = 0 ;
    libHandle     : THandle ;
  {$ENDIF}


  {$IFDEF OXYGENE}
    Layer : TGIS_LayerECW ;
  {$ENDIF}

  {$IFDEF OXYGENE}
    { ECW dll function. }
      ncsOpenFileViewProc    : IntPtr ;
    { ECW dll function. }
      ncsGetViewFileInfoProc : IntPtr ;
    { ECW dll function. }
      ncsGetViewInfoProc     : IntPtr ;
    { ECW dll function. }
      ncsSetFileViewProc     : IntPtr ;
    { ECW dll function. }
      ncsReadViewLineBGRProc : IntPtr ;
    { ECW dll function. }
      ncsReadViewLineBGRAProc : IntPtr ;
    { ECW dll function. }
      ncsReadViewLineBILProc  : IntPtr ;
    { ECW dll function. }
      ncsReadViewLineBILExProc : IntPtr ;
    { ECW dll function. }
      ncsCloseFileViewProc   : IntPtr ;
    { ECW dll function. }
      ncsSetConfigProc       : IntPtr ;

  {$ELSE}
    { ECW dll function.ss }
      ncsOpenFileView     : T_NCScbmOpenFileView     ;
    { ECW dll function.sss }
      ncsGetViewFileInfo  : T_NCScbmGetViewFileInfo  ;
    { ECW dll function.ss }
      ncsGetViewInfo      : T_NCScbmGetViewInfo      ;
    { ECW dll function.ss }
      ncsSetFileView      : T_NCScbmSetFileView      ;
    { ECW dll function.ss }
      ncsReadViewLineBGR  : T_NCScbmReadViewLineBGR  ;
    { ECW dll function.ss }
      ncsReadViewLineBGRA : T_NCScbmReadViewLineBGRA ;
    { ECW dll function.ss }
      ncsReadViewLineBIL : T_NCScbmReadViewLineBIL ;
    { ECW dll function.ss }
      ncsReadViewLineBILEx : T_NCScbmReadViewLineBILEx ;
    { ECW dll function.ss }
      ncsCloseFileView    : T_NCScbmCloseFileView    ;
    { ECW dll function.sss }
      ncsSetConfig        : T_NCSecwSetConfig        ;
  {$ENDIF}

type

  { Item of the map of ECW datum names into CS objects.
  }
  T_gcsMapItem = class( TGIS_CSAbstract )
      FEllipsoidWkt      : String ;
      FSemiMajor         : Double ;
      FInverseFlattering : Double ;

      constructor Create( const _epsg               : Integer ;
                          const _wkt                : String  ;
                          const _ell_wkt            : String  ;
                          const _semi_major         : Double  ;
                          const _inverse_flattering : Double
                        ) ;
    public
      procedure   Assign( const _source             : TObject
                        ) ; override;
  end ;

  { Class to map ECW datum names into CS objects.
  }
  T_gcsMap = class( TGIS_CSAbstractList )
    public
      constructor Create  ;
      function    Add     ( const _epsg               : Integer ;
                            const _wkt                : String  ;
                            const _ell_wkt            : String  ;
                            const _semi_major         : Double  ;
                            const _inverse_flattering : Double
                          ) : T_gcsMapItem ; reintroduce ; virtual;
      procedure   Init    ; override;
      function    ByWKT   ( const _wkt                : String
                          ) : T_gcsMapItem ; reintroduce ; virtual;
  end;

  { Item of the map of ECW projection names into CS objects.
  }
  T_pcsMapItem = class( TGIS_CSAbstract )
      FProjEPSG : Integer ;
      FParams   : TGIS_CSProjParametersInternal ;
      constructor Create( const _wkt                 : String  ;
                          const _prj_epsg            : Integer ;
                          const _params              : TGIS_CSProjParameters
                        ) ;
    public
      procedure   Assign( const _source              : TObject
                        ) ; override;
  end;

  { Class to map ECW projection names into CS objects.
  }
  T_pcsMap = class( TGIS_CSAbstractList )
    public
      constructor Create ;
      function    Add    ( const _wkt                : String  ;
                           const _prj_epsg           : Integer ;
                           const _params             : TGIS_CSProjParameters
                         ) : T_pcsMapItem ; reintroduce ; virtual;
      procedure   Init   ; override;
      function    ByWKT  ( const _wkt               : String
                         ) : T_pcsMapItem ; reintroduce ; virtual;
  end;

var
  cs_GcsMap : T_gcsMap ;
  cs_PcsMap : T_pcsMap ;


  {$IFDEF JAVA}
    function  ncsOpenFileView(  _ecwFilePath  : String ;
                                  out _fileView : com.sun.jna.Pointer ;
                                  _callBack     : T_CallBack
                                ) : Integer ;
    var
      ptr : com.sun.jna.ptr.PointerByReference ;
      str : com.sun.jna.WString ;
    begin
      ptr := new com.sun.jna.ptr.PointerByReference() ;
      str := new com.sun.jna.WString(_ecwFilePath) ;
      Result := libHandle.NCSOpenFileViewW( str, ptr, _callBack ) ;
      _fileView := ptr.Value ;
    end;

    procedure ncsGetViewFileInfo( _fileView         : com.sun.jna.Pointer ;
                                  out _fileViewInfo : com.sun.jna.Pointer
                                ) ;
    var
      ptr : com.sun.jna.ptr.PointerByReference ;
    begin
      ptr := new com.sun.jna.ptr.PointerByReference() ;
      libHandle.NCSGetViewFileInfo( _fileView, ptr ) ;
      _fileViewInfo := ptr.Value ;
    end;

    procedure ncsGetViewInfo( _fileView         : com.sun.jna.Pointer ;
                              out _fileViewInfo : com.sun.jna.Pointer
                            ) ;
    var
      ptr : com.sun.jna.ptr.PointerByReference ;
    begin
      ptr := new com.sun.jna.ptr.PointerByReference() ;
      libHandle.NCSGetViewInfo( _fileView, ptr ) ;
      _fileViewInfo := ptr.Value ;
    end;

    function  ncsSetFileView( _fileView     : com.sun.jna.Pointer   ;
                              _nBands       : LongWord ;
                              _pBandList    : array of Cardinal ;
                              _nTopX        : LongWord ;
                              _nLeftY       : LongWord ;
                              _nBottomX     : LongWord ;
                              _nRightY      : LongWord ;
                              _nSizeX       : LongWord ;
                              _nSizeY       : LongWord
                            ) : Integer ;
    begin
      Result := libHandle.NCSSetFileView( _fileView, _nBands, _pBandList, _nTopX, _nLeftY, _nBottomX, _nRightY, _nSizeX, _nSizeY ) ;
    end;

    function  ncsReadViewLineBGR( _fileView     : com.sun.jna.Pointer ;
                                  _pRGBTriplets : com.sun.jna.Pointer
                                ) : Integer ;
    begin
      Result := libHandle.NCSReadViewLineBGR( _fileView, _pRGBTriplets ) ;
    end;

    function  ncsReadViewLineBGRA( _fileView      : com.sun.jna.Pointer ;
                                    _pRGBTripletsA : com.sun.jna.Pointer
                                ) : Integer ;
    begin
      Result := libHandle.NCSReadViewLineBGRA( _fileView, _pRGBTripletsA ) ;
    end;

    function  ncsReadViewLineBIL( _fileView      : com.sun.jna.Pointer ;
                                    _ppRGBTripletsA : com.sun.jna.Pointer
                                  ) : Integer ;
    begin
      Result := libHandle.NCSReadViewLineBIL( _fileView, _ppRGBTripletsA ) ;
    end;

    function  ncsReadViewLineBILEx( _fileView      : com.sun.jna.Pointer ;
                                    _dataType      : Integer ;
                                    _ppRGBTripletsA : com.sun.jna.Pointer
                                  ) : Integer ;
    begin
      Result := libHandle.ncsReadViewLineBILEx( _fileView, _dataType, _ppRGBTripletsA ) ;
    end;

    function  ncsCloseFileView( _fileView     : com.sun.jna.Pointer ;
                                  _forceFree    : Boolean = False
                                ) : Integer ;
    begin
      Result := libHandle.NCSCloseFileViewEx( _fileView, _forceFree ) ;
    end;

    function  ncsSetConfig( _mode         : Integer ;
                            _inparam      : DWORD
                          ) : Integer ;
    begin
      Result := libHandle.NCSSetConfig( _mode, _inparam ) ;
    end;

    constructor TGIS_LayerECW_NCSViewFileInfoEx.Create ;
    begin
      inherited ;
    end;

    constructor TGIS_LayerECW_NCSViewFileInfoEx.Create( _peer : com.sun.jna.Pointer ) ;
    begin
      inherited Create( _peer ) ;
      allocateMemory() ;
      &read() ;
    end;

    function TGIS_LayerECW_NCSViewFileInfoEx.getFieldOrder: java.util.List<String>;
    begin
      exit java.util.Arrays.asList(
            'nSizeX', 'nSizeY', 'nBands', 'nCompressionRate', 'eCellSizeUnits', 'fCellIncrementX',
            'fCellIncrementY', 'fOriginX', 'fOriginY', 'szDatum', 'szProjection', 'fCWRotationDegrees',
            'eColorSpace', 'eCellType', 'pBands'
           );
    end;

    constructor TGIS_LayerECW_NCSFileViewSetInfo.Create ;
    begin
      inherited ;
    end;

    constructor TGIS_LayerECW_NCSFileViewSetInfo.Create( _peer : com.sun.jna.Pointer ) ;
    begin
      inherited Create( _peer ) ;
      allocateMemory() ;
      &read() ;
    end;

    method TGIS_LayerECW_NCSFileViewSetInfo.getFieldOrder: java.util.List<String>;
    begin
      exit java.util.Arrays.asList(
              'pClientData','nBands','pBandList','nTopX','nLeftY','nBottomX','nRightY','nSizeX',
              'nSizeY','nBlocksInView','nBlocksAvailable','nBlocksAvailableAtSetView',
              'nMissedBlockDuringRead','fTopX','fTopY','fBottomX','fBottomY'
            );
    end;

  {$ENDIF}

  { Map between ECW datum names into CS objects.
  }
  function gcsMapList : T_gcsMap ;
  begin
    if not assigned( cs_GcsMap ) then
      cs_GcsMap := T_gcsMap.Create ;

    Result := cs_GcsMap ;
  end;

  { Map between ECW projection names into CS objects.
  }
  function pcsMapList : T_pcsMap ;
  begin
    if not assigned( cs_PcsMap ) then
      cs_PcsMap := T_pcsMap.Create ;

    Result := cs_PcsMap ;
  end;

//==============================================================================
// T_gcsMapItem
//==============================================================================

  constructor T_gcsMapItem.Create(
    const _epsg               : Integer ;
    const _wkt                : String  ;
    const _ell_wkt            : String  ;
    const _semi_major         : Double  ;
    const _inverse_flattering : Double
  ) ;
  begin
     inherited Create( _epsg, _wkt ) ;
     FEllipsoidWkt      := _ell_wkt ;
     FSemiMajor         := _semi_major ;
     FInverseFlattering := _inverse_flattering ;
  end;

  procedure T_gcsMapItem.Assign(
    const _source : TObject
  ) ;
  begin
    assert( _source is T_gcsMapItem ) ;

    inherited ;

    FEllipsoidWkt      := T_gcsMapItem( _source ).FEllipsoidWkt ;
    FSemiMajor         := T_gcsMapItem( _source ).FSemiMajor ;
    FInverseFlattering := T_gcsMapItem( _source ).FInverseFlattering ;
  end;

//==============================================================================
// T_gcsMap
//==============================================================================

  constructor T_gcsMap.Create ;
  begin
    inherited Create( nil, False, False ) ;
  end;

  function T_gcsMap.Add(
    const _epsg               : Integer ;
    const _wkt                : String  ;
    const _ell_wkt            : String  ;
    const _semi_major         : Double  ;
    const _inverse_flattering : Double
  ) : T_gcsMapItem ;
  begin
    LockThread ;
    try
      Result := T_gcsMapItem.Create(
                  _epsg,
                  uniqueWkt( _epsg,_wkt ),
                  _ell_wkt,
                  _semi_major,
                  _inverse_flattering
                ) ;
      inherited Add( Result ) ;
    finally
      UnlockThread ;
    end;
  end;

  function T_gcsMap.ByWKT(
    const _wkt    : String
  ) : T_gcsMapItem ;
  begin
    LockThread ;
    try
      Result := T_gcsMapItem( inherited ByWKT( _wkt ) ) ;
    finally
      UnlockThread ;
    end;
  end;

  procedure T_gcsMap.Init;
  {$INCLUDE CsData\GisEcwGeogcs.inc}
  begin
    LockThread ;
    try
      Clear ;

      Init_ECWGeographicCoordinateSystemList( self ) ;
    finally
      UnlockThread ;
    end;
  end;

//==============================================================================
// T_pcsMapItem
//==============================================================================

  constructor T_pcsMapItem.Create(
    const _wkt                : String  ;
    const _prj_epsg           : Integer ;
    const _params             : TGIS_CSProjParameters
  ) ;
  begin
    inherited Create( -1, _wkt ) ;

    {$IFDEF GIS_NORECORDS}
      FParams := new TGIS_CSProjParametersInternal() ;
    {$ENDIF}
    FParams.All := _params ;
    FProjEPSG := _prj_epsg ;
  end;

  procedure T_pcsMapItem.Assign(
    const _source : TObject
  ) ;
  begin
    assert( _source is T_pcsMapItem ) ;

    inherited ;

    FParams   := T_pcsMapItem( _source ).FParams ;
    FProjEPSG := T_pcsMapItem( _source ).FProjEPSG ;
  end;

//==============================================================================
// T_pcsMapItem
//==============================================================================

  constructor T_pcsMap.Create ;
  begin
    inherited Create( nil, False, False ) ;
  end;

  function T_pcsMap.Add(
    const _wkt                : String  ;
    const _prj_epsg           : Integer ;
    const _params             : TGIS_CSProjParameters
  ) : T_pcsMapItem ;
  begin
    LockThread ;
    try
      Result := T_pcsMapItem.Create(
                  _wkt,
                  _prj_epsg,
                  _params
                ) ;
      inherited Add( Result ) ;
    finally
      UnlockThread ;
    end;
  end;

  function T_pcsMap.ByWKT(
    const _wkt    : String
  ) : T_pcsMapItem ;
  begin
    LockThread ;
    try
      Result := T_pcsMapItem( inherited ByWKT( _wkt ) ) ;
    finally
      UnlockThread ;
    end;
  end;

  procedure T_pcsMap.Init;
  var
    pp : TGIS_CSProjParameters ;

  {$INCLUDE CsData\GisEcwProjcs.inc}
  begin
    LockThread ;
    try
      Clear ;
      {$IFDEF GIS_NORECORDS}
        pp := new TGIS_CSProjParameters ;
      {$ENDIF}
      Init_ECWProjectedCoordinateSystemList( self, pp ) ;
    finally
      UnlockThread ;
    end;
  end;

//==============================================================================
// TGIS_LayerECW
//==============================================================================

  {$IFDEF CLR}
    // Callback refresh function.
    // _fileView   pointer to file view structure
    // return      reading status
    function ViewCallback(
      _fileView : IntPtr
    ) : TGIS_LayerECW_NCSEcwReadStatus ;
  {$ENDIF}
  {$IFDEF JAVA}
    // Callback refresh function.
    // _fileView   pointer to file view structure
    // return      reading status
    function ViewCallback(
      _fileView : com.sun.jna.Pointer
    ) : TGIS_LayerECW_NCSEcwReadStatus ;
  {$ENDIF}
  {$IFDEF DCC}
    // Callback refresh function.
    // _fileView   pointer to file view structure
    // return      reading status
    function ViewCallback(
      _fileView : Pointer
    ) : TGIS_LayerECW_NCSEcwReadStatus ; cdecl ;
  {$ENDIF}
  var
    i               : Integer  ;
    ret             : TGIS_LayerECW_NCSEcwReadStatus ;
    iret            : Integer ;
    {$IFDEF OXYGENE}
      pFileViewInfo : TGIS_LayerECW_NCSFileViewSetInfo ;
    {$ELSE}
      pFileViewInfo : PGIS_LayerECW_NCSFileViewSetInfo ;
    {$ENDIF}
    layerECW        : TGIS_LayerECW ;

    {$IFDEF CLR}
      iptr          : IntPtr  ;
      iptr2         : IntPtr  ;
    {$ENDIF}
    {$IFDEF JAVA}
      iptr          : com.sun.jna.Pointer  ;
      iptr2         : com.sun.jna.Pointer  ;
    {$ENDIF}
  begin
    {$IFDEF OXYGENE}
      ncsGetViewInfo( _fileView, iptr2 ) ;
      {$IFDEF CLR}
      if iptr2 = 0 then Abort ;
      {$ENDIF}
      {$IFDEF JAVA}
      if iptr2 = nil then Abort ;
      {$ENDIF}
      pFileViewInfo := TGIS_LayerECW_NCSFileViewSetInfo(
        Marshal.PtrToStructure( iptr2, typeOf(TGIS_LayerECW_NCSFileViewSetInfo) )
      ) ;
    {$ELSE}
      ncsGetViewInfo( _fileView, @pFileViewInfo ) ;
    {$ENDIF}

    {$IFDEF OXYGENE}
      assert( assigned( Layer ) ) ;
      layerECW := Layer ;
    {$ELSE}
      layerECW := pFileViewInfo.pClientData ;
    {$ENDIF}

    // avoid reading previous View
    if ( layerECW.viewState.left   <> pFileViewInfo.nTopX    ) or
       ( layerECW.viewState.top    <> pFileViewInfo.nLeftY   ) or
       ( layerECW.viewState.right  <> pFileViewInfo.nBottomX ) or
       ( layerECW.viewState.bottom <> pFileViewInfo.nRightY  ) or
       ( layerECW.viewState.sizeX  <> pFileViewInfo.nSizeX   ) or
       ( layerECW.viewState.sizeY  <> pFileViewInfo.nSizeY   )
    then begin
      Result := TGIS_LayerECW_NCSEcwReadStatus.NCSECW_READ_CANCELLED ;
      exit ;
    end;

    if pFileViewInfo.nBlocksAvailable = 0 then begin
      Result := TGIS_LayerECW_NCSEcwReadStatus.NCSECW_READ_CANCELLED ;
      exit ;
    end;

    if not layerECW.useCallBack then
    begin
      repeat
        if GetTickCount - layerECW.viewState.lastreadtime > ECW_MAX_VIEW_SETS
        then // too much time
          break ;
        {$IFDEF OXYGENE}
           ncsGetViewInfo( _fileView, iptr2 ) ;
          {$IFDEF CLR}
          if iptr2 = 0 then Abort ;
          {$ENDIF}
          {$IFDEF JAVA}
          if iptr2 = nil then Abort ;
          {$ENDIF}
           pFileViewInfo := TGIS_LayerECW_NCSFileViewSetInfo(
             Marshal.PtrToStructure( iptr2, typeOf(TGIS_LayerECW_NCSFileViewSetInfo) )
           ) ;
        {$ELSE}
          ncsGetViewInfo( _fileView, @pFileViewInfo ) ;
        {$ENDIF}
      until pFileViewInfo.nBlocksAvailable = pFileViewInfo.nBlocksInView ;
    end ;

    try
      assert( assigned( layerECW ) ) ;

      if layerECW.viewState.linesRead = layerECW.viewState.linesNeeded then
      begin
        if not layerECW.viewState.notComplette then begin
          Result := TGIS_LayerECW_NCSEcwReadStatus.NCSECW_READ_OK ;
          exit ;
        end;
      end;

      layerECW.viewState.linesRead := 0 ;
      layerECW.viewState.notComplette := True ;
      layerECW.viewState.lastreadtime := GetTickCount ;

      {$IFDEF OXYGENE}
        iptr := Marshal.AllocHGlobal( layerECW.viewState.portion ) ;
      {$ENDIF}
      try

        layerECW.viewState.workBuffer := layerECW.viewState.buffer ;
        {$IFDEF OXYGENE}
          layerECW.viewState.workBufferx := 0 ;
        {$ENDIF}

        try
        assert( pFileViewInfo.nSizeY = Cardinal( layerECW.viewState.linesNeeded) ) ;
        except

        end;

        for i := layerECW.viewState.linesRead to
                 pFileViewInfo.nSizeY - 1 do
        begin

          while layerECW.viewState.inReadProcess do begin
            // wait for operations
          end ;

          layerECW.viewState.inReadProcess := True ;

          {$IFDEF OXYGENE}
            if pFileViewInfo.nBands = 4 then
              iret := ncsReadViewLineBGRA( _fileView, iptr )
            else
              iret := ncsReadViewLineBGR( _fileView, iptr ) ;
            {$IFDEF JAVA}
            case iret of
              0 : ret := TGIS_LayerECW_NCSEcwReadStatus.NCSECW_READ_OK ;
              1 : ret := TGIS_LayerECW_NCSEcwReadStatus.NCSECW_READ_FAILED ;
              2 : ret := TGIS_LayerECW_NCSEcwReadStatus.NCSECW_READ_CANCELLED ;
            end;
            {$ELSE}
            ret := TGIS_LayerECW_NCSEcwReadStatus(iret) ;
            {$ENDIF}
            if ret <> TGIS_LayerECW_NCSEcwReadStatus.NCSECW_READ_OK then
              break ;

            Marshal.Copy( iptr,
                          layerECW.viewState.workBuffer,
                          layerECW.viewState.workBufferx,
                          layerECW.viewState.portion
                        ) ;
            layerECW.viewState.workBufferx :=
              layerECW.viewState.workBufferx +
              layerECW.viewState.portion ;
          {$ELSE}
            if pFileViewInfo.nBands = 4 then
              ret := TGIS_LayerECW_NCSEcwReadStatus(
                       ncsReadViewLineBGRA( _fileView, layerECW.viewState.workBuffer )
                     )
            else
              ret := TGIS_LayerECW_NCSEcwReadStatus(
                       ncsReadViewLineBGR( _fileView, layerECW.viewState.workBuffer )
                     ) ;
            if ret <> TGIS_LayerECW_NCSEcwReadStatus.NCSECW_READ_OK then
              break ;

            layerECW.viewState.workBuffer :=
                      Pointer( NativeInt( layerECW.viewState.workBuffer ) +
                      layerECW.viewState.portion ) ;
          {$ENDIF}
          inc(layerECW.viewState.linesRead) ;
          layerECW.viewState.inReadProcess := False ;
        end ;

        if not layerECW.useCallBack then begin
          {$IFDEF OXYGENE}
            pFileViewInfo.nBlocksInView := 1 ;
            pFileViewInfo.nBlocksAvailable := pFileViewInfo.nBlocksInView ;
          {$ELSE}
            pFileViewInfo^.nBlocksInView := 1 ;
            pFileViewInfo^.nBlocksAvailable := pFileViewInfo^.nBlocksInView ;
          {$ENDIF}
        end ;

        {$IFDEF OXYGENE}
          if pFileViewInfo.nBlocksAvailable = 0 then
        {$ELSE}
          if pFileViewInfo^.nBlocksAvailable = 0 then
        {$ENDIF}
        begin
          layerECW.viewState.empty := True ;
          layerECW.viewState.linesRead := 0 ;
        end
        else begin
          layerECW.viewState.empty := False ;
        end ;

        {$IFDEF OXYGENE}
          layerECW.viewState.actualBlocks := pFileViewInfo.nBlocksAvailable ;
        {$ELSE}
          layerECW.viewState.actualBlocks := pFileViewInfo^.nBlocksAvailable ;
        {$ENDIF}

        {$IFDEF OXYGENE}
          layerECW.viewState.notComplette :=
            pFileViewInfo.nBlocksAvailable <> pFileViewInfo.nBlocksInView ;
        {$ELSE}
          layerECW.viewState.notComplette :=
            pFileViewInfo^.nBlocksAvailable <> pFileViewInfo^.nBlocksInView ;
        {$ENDIF}

        if not layerECW.viewState.notComplette then
          layerECW.viewState.notRead := False ;

      finally
        {$IFDEF OXYGENE}
          Marshal.FreeHGlobal( iptr ) ;
        {$ENDIF}
      end ;

      layerECW.viewState.workBuffer := layerECW.viewState.buffer ;

      if layerECW.viewState.cancelled then begin
        Result := TGIS_LayerECW_NCSEcwReadStatus.NCSECW_READ_CANCELLED ;
        layerECW.viewState.notRead := False ;
      end
      else
        Result := TGIS_LayerECW_NCSEcwReadStatus.NCSECW_READ_OK ;
    finally
      layerECW.viewState.inReadProcess := False ;
    end;
  end ;

  constructor TGIS_LayerECW.Create ;
  begin
    inc( instanceCount ) ;

    inherited ;

    FSubType := FSubType + [TGIS_LayerSubType.Persistent]  ;
    {$IFDEF JAVA}
    libHandle := nil  ;
    {$ELSE}
    libHandle := 0  ;
    {$ENDIF}
  end ;

  procedure TGIS_LayerECW.doDestroy ;
  begin
    Dormant ;

    dec( instanceCount ) ;

    inherited ;
  end ;

  procedure TGIS_LayerECW.bindLibrary ;
  begin
    try
      {$IFDEF JAVA}
        if libHandle <> nil then exit ;

        libHandle := IECWLibrary(com.sun.jna.Native.loadLibrary( NCS_ECW_LIBNAME_NOEXT, typeOf(IECWLibrary) ) );
        if libHandle = nil then Abort ;
      {$ELSE}
        if libHandle <> 0 then exit ;

        libHandle := LoadLibraryWithinHinstance( NCS_ECW_LIBNAME );
        if libHandle = 0 then Abort ;
      {$ENDIF}

      {$IFDEF CLR}
        ncsOpenFileViewProc      := GetProcAddress(libHandle, ECW_FUNC_OPENFILEVIEW   ) ;
        ncsCloseFileViewProc     := GetProcAddress(libHandle, ECW_FUNC_CLOSEFILEVIEW  ) ;
        ncsGetViewFileInfoProc   := GetProcAddress(libHandle, ECW_PROC_GETVIEWFILEINFO) ;
        ncsGetViewInfoProc       := GetProcAddress(libHandle, ECW_PROC_GETVIEWINFO    ) ;
        ncsSetFileViewProc       := GetProcAddress(libHandle, ECW_FUNC_SETFILEVIEW    ) ;
        ncsReadViewLineBGRProc   := GetProcAddress(libHandle, ECW_FUNC_READLINEBGR    ) ;
        ncsReadViewLineBGRAProc  := GetProcAddress(libHandle, ECW_FUNC_READLINEBGRA   ) ;
        ncsReadViewLineBILProc    := GetProcAddress(libHandle, ECW_FUNC_READLINEBIL ) ;
        ncsReadViewLineBILExProc := GetProcAddress(libHandle, ECW_FUNC_READLINEBILEX ) ;
        ncsSetConfigProc         := GetProcAddress(libHandle, ECW_FUNC_SETCONFIG      ) ;
      {$ENDIF}
      {$IFDEF JAVA}
        ncsOpenFileViewProc      := 1 ;
        ncsCloseFileViewProc     := 1 ;
        ncsGetViewFileInfoProc   := 1 ;
        ncsGetViewInfoProc       := 1 ;
        ncsSetFileViewProc       := 1 ;
        ncsReadViewLineBGRProc   := 1 ;
        ncsReadViewLineBGRAProc  := 1 ;
        ncsReadViewLineBILProc    := 1 ;
        ncsReadViewLineBILExProc := 1 ;
        ncsSetConfigProc         := 1 ;
      {$ENDIF}
      {$IFDEF DCC}
        @ncsOpenFileView        := GetProcAddress(libHandle, ECW_FUNC_OPENFILEVIEW   ) ;
        @ncsCloseFileView       := GetProcAddress(libHandle, ECW_FUNC_CLOSEFILEVIEW  ) ;
        @ncsGetViewFileInfo     := GetProcAddress(libHandle, ECW_PROC_GETVIEWFILEINFO) ;
        @ncsGetViewInfo         := GetProcAddress(libHandle, ECW_PROC_GETVIEWINFO    ) ;
        @ncsSetFileView         := GetProcAddress(libHandle, ECW_FUNC_SETFILEVIEW    ) ;
        @ncsReadViewLineBGR     := GetProcAddress(libHandle, ECW_FUNC_READLINEBGR    ) ;
        @ncsReadViewLineBGRA    := GetProcAddress(libHandle, ECW_FUNC_READLINEBGRA   ) ;
        @ncsReadViewLineBIL     := GetProcAddress(libHandle, ECW_FUNC_READLINEBIL   ) ;
        @ncsReadViewLineBILEx   := GetProcAddress(libHandle, ECW_FUNC_READLINEBILEX   ) ;
        @ncsSetConfig           := GetProcAddress(libHandle, ECW_FUNC_SETCONFIG      ) ;
      {$ENDIF}

      {$IFDEF OXYGENE}
        if ncsOpenFileViewProc     = nil then Abort ;
        if ncsCloseFileViewProc    = nil then Abort ;
        if ncsGetViewFileInfoProc  = nil then Abort ;
        if ncsGetViewInfoProc      = nil then Abort ;
        if ncsSetFileViewProc      = nil then Abort ;
        if ncsReadViewLineBGRProc  = nil then Abort ;
        if ncsReadViewLineBGRAProc = nil then Abort ;
        if ncsReadViewLineBILProc  = nil then Abort ;
        if ncsReadViewLineBILExProc = nil then Abort ;
        if ncsSetConfigProc        = nil then Abort ;
      {$ELSE}
        if @ncsOpenFileView        = nil then Abort ;
        if @ncsCloseFileView       = nil then Abort ;
        if @ncsGetViewFileInfo     = nil then Abort ;
        if @ncsGetViewInfo         = nil then Abort ;
        if @ncsSetFileView         = nil then Abort ;
        if @ncsReadViewLineBGR     = nil then Abort ;
        if @ncsReadViewLineBGRA    = nil then Abort ;
        if @ncsReadViewLineBIL     = nil then Abort ;
        if @ncsReadViewLineBILEx   = nil then Abort ;
        if @ncsSetConfig           = nil then Abort ;
      {$ENDIF}

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), NCS_ECW_LIBNAME, 0 ) ;
    end ;
  end ;

  function TGIS_LayerECW.bandsMappingChanging : Boolean ;
  begin
    Result := False ;
    if bandsMap[0] <> 0 then
      Result := True ;
    if bandsMap[1] <> 1 then
      Result := True ;
    if bandsMap[2] <> 2 then
      Result := True ;
    if (bandsMap[3] <> 3) and (bandsMap[3] <> -1) then
      Result := True ;
  end;


  procedure TGIS_LayerECW.reloadLibrary ;
  var
    res   : Integer ;
    spath : String  ;
    isize : Cardinal  ;
    bass  : Boolean ;
 begin
    bindLibrary ;
    try
      {$IFDEF JAVA}
        bass := assigned( pFileView ) ;
      {$ELSE}
        bass := AssignedPtr( pFileView ) ;
      {$ENDIF}
      if not bass then begin
        spath := GetShortPath( Path ) ;

        // force caching mode
        useCallBack := defaultCallBackMode ;

        if isFromNet then begin
          if      not assigned( Viewer )
          then
            useCallBack := False ;

    { TODO : Version with call back, defaultCallBackMode should be true  }
    (*
         else if ( Viewer.Ref.Device <> TGIS_Device.Window ) and
                  ( Viewer.Ref.Device <> TGIS_Device.Ghost  )
          then
                useCallBack := False ;
    *)
        end;

        {$IFDEF OXYGENE}
          res := ncsOpenFileView( spath, pFileView, nil ) ;
        {$ELSE}
          if useCallBack then
            res := ncsOpenFileView( PWideString( spath ),
                                    @pFileView,  @ViewCallback
                                  )
          else
            res := ncsOpenFileView( PWideString( spath ),
                                    @pFileView,  nil
                                  ) ;
        {$ENDIF}

        if res <> 0 then Abort ;

      end ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 1 ) ;
    end ;

    // metafile overwrite
    try
      isize := GisMetadataAsInteger( 'TGIS_LayerECW.CACHE_MAXMEM', 0 ) ;

      if isize > 0 then begin
        res := ncsSetConfig( ECW_CACHE_MAXMEM, isize ) ;

        if res <> 0 then Abort ;
      end;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 10 ) ;
    end;

  end ;

  procedure TGIS_LayerECW.setUp ;
  var
    width,
    height      : Integer     ;
    line_width  : Integer     ;
    xres,
    yres        : Double      ;
    originx,
    originy     : Double      ;
    ext         : TGIS_Extent ;
    ex          : String      ;
    typename    : String      ;
    copyright   : String      ;
    {$IFDEF CLR}
      iptr      : IntPtr      ;
    {$ENDIF}
    {$IFDEF JAVA}
      iptr      : com.sun.jna.Pointer ;
    {$ENDIF}

    gcs_itm     : T_gcsMapItem  ;
    pcs_itm     : T_pcsMapItem  ;
    cs_obj      : TGIS_CSCoordinateSystem           ;
    gcs_obj     : TGIS_CSGeographicCoordinateSystem ;
    unt_epsg    : Integer       ;
    sdatum      : String        ;
    sprojection : String        ;
    projparams  : TGIS_CSProjParameters ;

    procedure use_world_file(
      const _ext1 : String ;
      const _ext2 : String
    ) ;
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

     if      SafeFileExists( fname1 ) then
             setWorldFile( _ext1 )
     else if SafeFileExists( fname2 ) then
             setWorldFile( _ext2 )
     else if SafeFileExists( ftab   ) then
             setWorldFile( _ext1 )
    end ;

    function prepare_gcs(
      const _itm : T_gcsMapItem
    ) : TGIS_CSGeographicCoordinateSystem ;
    var
      oellps  : TGIS_CSEllipsoid ;
      odatum  : TGIS_CSDatum     ;
      ogcs    : TGIS_CSGeographicCoordinateSystem ;
    begin
      ogcs := CSGeographicCoordinateSystemList.ByEPSG( _itm.EPSG ) ;
      if not assigned( ogcs ) then begin
        // unknown GCS - create t

        oellps := CSEllipsoidList.Prepare(
                    -1, gcs_itm.FEllipsoidWkt,
                    gcs_itm.FSemiMajor, gcs_itm.FInverseFlattering
                  ) ;
        odatum := CSDatumList.Prepare(
                    -1, gcs_itm.WKT,
                    oellps.EPSG, 0
                  ) ;
        Result := CSGeographicCoordinateSystemList.Prepare(
                    -1, gcs_itm.WKT,
                    odatum.EPSG,
                    8901, // always Greenwich
                    9122  // always Degrees
                  ) ;
      end
      else begin
        Result := ogcs ;
      end;
    end;

  begin
    {$IFDEF OXYGENE}
      if ( TGIS_DeveloperKernelTypes.SERVER and
           GisDeveloperKernelType
         ) = TGIS_DeveloperKernelTypes.SERVER then begin
    {$ELSE}
      if TGIS_DeveloperKernelTypes.SERVER in GisDeveloperKernelType then begin
    {$ENDIF}
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 2) ;
    end ;

    // set world file if any
    ex := GetFileExt( LowerCase( Path ) ) ;

    if not assigned(Viewer) then begin
      bandsMap[0] := 0 ;
      bandsMap[1] := 1 ;
      bandsMap[2] := 2 ;
    end;
    if ex = FILE_EXT_ECW then begin
      {$IFDEF OXYGENE}
        defaultCallBackMode := False ;
      {$ELSE}
        defaultCallBackMode := True ;
      {$ENDIF}

      typename  := 'Enhanced Compression Wavelet Format (ECW)' ;
      copyright := '' ;
      isJP2000 := False ;
    end
    else begin
      defaultCallBackMode := False ;
      typename  := 'JPEG 2000 File Format (via ECW Layer)' ;
      copyright := '' ;
      isJP2000 := True ;
    end ;

    isFromNet := Pos( '://',  Path ) > 0 ;
    if useCallBack and (not isFromNet) then
      useCallBack := False ;

    reloadLibrary ;
    try
      {$IFDEF OXYGENE}
        SetLength( bandList, 4 ) ;
        SetLength( bandListOld, 4 ) ;
      {$ENDIF}
      bandList[0] := 0 ;
      bandList[1] := 1 ;
      bandList[2] := 2 ;
      bandList[3] := 3 ;
      if not UseConfig then begin
        bandsMap[0] := bandList[0] ;
        bandsMap[1] := bandList[1] ;
        bandsMap[2] := bandList[2] ;
      end;

      {$IFDEF CLR}
        if pFileView = 0 then Abort;
      {$ELSE}
        if not assigned( pFileView ) then Abort ;
      {$ENDIF}

      {$IFDEF OXYGENE}
        ncsGetViewFileInfo( pFileView, iptr ) ;
      {$ELSE}
        ncsGetViewFileInfo( pFileView, @pFileViewInfo ) ;
      {$ENDIF}

      {$IFDEF OXYGENE}
        {$IFDEF JAVA}
        if iptr = nil then Abort ;
        {$ELSE}
        if iptr = 0 then Abort ;
        {$ENDIF}
        pFileViewInfo := TGIS_LayerECW_NCSViewFileInfoEx(
           Marshal.PtrToStructure( iptr, typeOf(TGIS_LayerECW_NCSViewFileInfoEx) )
        ) ;
      {$ELSE}
        if not assigned( pFileViewInfo ) then Abort ;
      {$ENDIF}

      {$IFDEF OXYGENE}
        FBandsCount  := pFileViewInfo.nBands ;
      {$ELSE}
        FBandsCount  := pFileViewInfo^.nBands ;
      {$ENDIF}
      {$IFDEF OXYGENE}
        case pFileViewInfo.eCellType of
      {$ELSE}
        case TGIS_LayerECW_NCSViewFileInfoEx(pFileViewInfo^).eCellType of
      {$ENDIF}
        0 :
          bandBytesDepth := 1 ;
        1 :
          begin
            if FBandsCount = 3 then
              bandBytesDepth := 1
            else
              bandBytesDepth := 2 ;
          end
        else
          bandBytesDepth := 1 ;
      end;

      {$IFDEF OXYGENE}
        width  := pFileViewInfo.nSizeX ;
        height := pFileViewInfo.nSizeY ;
      {$ELSE}
        width  := pFileViewInfo^.nSizeX ;
        height := pFileViewInfo^.nSizeY ;
      {$ENDIF}

      {$IFDEF OXYGENE}
        {$IFDEF JAVA}
          case pFileViewInfo.eCellSizeUnits of
            0 : eCellSizeUnits := TGIS_LayerECW_CellSizeUnits.ECW_CELL_UNITS_INVALID ;
            1 : eCellSizeUnits := TGIS_LayerECW_CellSizeUnits.ECW_CELL_UNITS_METERS;
            2 : eCellSizeUnits := TGIS_LayerECW_CellSizeUnits.ECW_CELL_UNITS_DEGREES;
            3 : eCellSizeUnits := TGIS_LayerECW_CellSizeUnits.ECW_CELL_UNITS_FEET;
            4 : eCellSizeUnits := TGIS_LayerECW_CellSizeUnits.ECW_CELL_UNITS_UNKNOWN ;
          end ;
        {$ELSE}
        eCellSizeUnits := TGIS_LayerECW_CellSizeUnits(pFileViewInfo.eCellSizeUnits) ;
        {$ENDIF}
      {$ELSE}
        eCellSizeUnits := TGIS_LayerECW_CellSizeUnits(pFileViewInfo^.eCellSizeUnits) ;
      {$ENDIF}

      case eCellSizeUnits of
        TGIS_LayerECW_CellSizeUnits.ECW_CELL_UNITS_METERS  : unt_epsg := 9001 ;
        TGIS_LayerECW_CellSizeUnits.ECW_CELL_UNITS_DEGREES : unt_epsg := 9122 ;
        TGIS_LayerECW_CellSizeUnits.ECW_CELL_UNITS_FEET    : unt_epsg := 9002 ;
        else                                   unt_epsg := 0    ;
      end ;

      // setup projection info
      sdatum      := String( pFileViewInfo.szDatum      ) ;
      sprojection := String( pFileViewInfo.szProjection ) ;
      pFileViewInfo.szProjection := nil ;

      if unt_epsg <> 0 then begin

        if ( not IsStringEmpty( sprojection )       ) and
           ( UpperCase( sprojection ) <> 'GEODETIC' ) then
        begin
          gcs_itm := gcsMapList.ByWKT( sdatum      ) ;
          pcs_itm := pcsMapList.ByWKT( sprojection ) ;

          if assigned( gcs_itm ) then
            gcs_obj := prepare_gcs( gcs_itm )
          else
            gcs_obj := CSGeographicCoordinateSystemList.ByWKT( sdatum ) ;

          if assigned( pcs_itm ) and assigned( gcs_obj ) then begin

            projparams := pcs_itm.FParams.All ;

            // fix primem meridian
            if not IsNan( projparams.CentralMeridian    ) then
              projparams.CentralMeridian    :=
                projparams.CentralMeridian    - gcs_obj.PrimeMeridian.Longitude ;
            if not IsNan( projparams.LongitudeOfCenter  ) then
              projparams.LongitudeOfCenter  :=
                projparams.LongitudeOfCenter  - gcs_obj.PrimeMeridian.Longitude ;
            if not IsNan( projparams.LongitudeOfPoint_1 ) then
              projparams.LongitudeOfPoint_1 :=
                projparams.LongitudeOfCenter  - gcs_obj.PrimeMeridian.Longitude ;
            if not IsNan( projparams.LongitudeOfPoint_2 ) then
              projparams.LongitudeOfPoint_2 :=
                projparams.LongitudeOfPoint_2 - gcs_obj.PrimeMeridian.Longitude ;
            if not IsNan( projparams.LongitudeOfPoint_2 ) then
              projparams.LongitudeOfPoint_2 :=
                projparams.LongitudeOfPoint_2 - gcs_obj.PrimeMeridian.Longitude ;

            self.CS := CSProjectedCoordinateSystemList.Prepare(
                         0,
                         sprojection + '_' + sdatum,
                         gcs_obj.EPSG,
                         unt_epsg,
                         pcs_itm.FProjEPSG,
                         projparams
                       ) ;
          end;

          if self.CS is TGIS_CSUnknownCoordinateSystem then
            self.CS := TGIS_CSFactory.ByWKT( sprojection ) ;
        end
        else if not IsStringEmpty( sdatum ) then begin
          cs_obj := TGIS_CSFactory.ByWKT( sdatum ) ;
          if assigned( cs_obj ) and
             not ( cs_obj is TGIS_CSUnknownCoordinateSystem ) then begin
            self.CS := cs_obj
          end
          else begin
            gcs_itm := gcsMapList.ByWKT( sdatum ) ;
            if assigned( gcs_itm ) then begin
              self.CS := prepare_gcs( gcs_itm ) ;
            end ;
          end ;
          if self.CS is TGIS_CSUnknownCoordinateSystem then begin
            self.CS := TGIS_CSFactory.ByWKT( sdatum ) ;
          end;
        end ;

      end ;

      {$IFDEF OXYGENE}
        xres := pFileViewInfo.fCellIncrementX ;
        yres := pFileViewInfo.fCellIncrementY ;
      {$ELSE}
        xres := pFileViewInfo^.fCellIncrementX ;
        yres := pFileViewInfo^.fCellIncrementY ;
      {$ENDIF}

      scaleX := xres ;
      scaleY := yres ;

      if scaleX < 0 then scaleX := -scaleX ;
      if scaleY > 0 then scaleY := -scaleY ;

      {$IFDEF OXYGENE}
        originx := pFileViewInfo.fOriginX ;
        originy := pFileViewInfo.fOriginY ;
      {$ELSE}
        originx := pFileViewInfo^.fOriginX ;
        originy := pFileViewInfo^.fOriginY ;
      {$ENDIF}

      pixelBytes := 3 ;
      if FBandsCount <= 3 then begin
        nBands := FBandsCount ;
        FBandsCount := 3 ;
      end
      else begin
        nBands := 3 ;
        if FBandsCount = 4 then begin
          if not isJP2000 then begin
            isPartialTransparent := True ;
            defaultPartialTransparent := True ;
          end;
          forceCachedMode := True ;
          inc(pixelBytes) ;
          inc(nBands) ;
        end;
      end;

      {$IFDEF GIS_NORECORDS}
        ext := new TGIS_Extent ;
      {$ENDIF}

      ext.XMin := originx ;
      ext.YMax := originy ;

      ext.XMax := ext.XMin + width  * scaleX ;
      ext.YMin := ext.YMax + height * scaleY ;

      Extent := ext ;

      line_width := ((pixelBytes*width +3) div 4)*4 ;
      line4BytesWidth := line_width ;

      stripHeight := 0 ;
      stripBufferSize := 0 ;
//      bitsOffset := 0 ;
      lastZoom   := 1 ;
      actualZoom := 1 ;

      // get bitmap size into easy to access properties
      FBitWidth      := width ;
      totalBitWidth  := line_width ;
      FBitHeight     := height ;

      realLineWidth  := width ;
      totalLineWidth := line_width ;

      bilBuffer := nil ;


      realBitCount := 24 ;
//      intBitCount  := 24 ;
      intLineWidth := FBitWidth * 3 ;
      colorsNo     := 0 ;

      {$IFDEF GIS_NORECORDS}
        viewState := new T_viewState ;
      {$ENDIF}

      viewState.firstCall     := True  ;
      viewState.empty         := True  ;
      viewState.inReadProcess := False ;

      // decompressed lines number
      stripLinesNo := 0 ;

      if      ex = FILE_EXT_ECW then
              use_world_file( WORLD_FILE_EXT_ECW_TFW, WORLD_FILE_EXT_ECW_TFW2 )
      else if ex = FILE_EXT_JPF then
              use_world_file( WORLD_FILE_EXT_JPF_TFW, WORLD_FILE_EXT_JPF_TFW2 )
      else if ex = FILE_EXT_JPX then
              use_world_file( WORLD_FILE_EXT_JPX_TFW, WORLD_FILE_EXT_JPX_TFW2 )
      else if ex = FILE_EXT_JPC then
              use_world_file( WORLD_FILE_EXT_JPC_TFW, WORLD_FILE_EXT_JPC_TFW2 )
      else if ex = FILE_EXT_J2C then
              use_world_file( WORLD_FILE_EXT_J2C_TFW, WORLD_FILE_EXT_J2C_TFW2 )
      else if ex = FILE_EXT_J2X then
              use_world_file( WORLD_FILE_EXT_J2X_TFW, WORLD_FILE_EXT_J2X_TFW2 )
      else if ex = FILE_EXT_J2K then
              use_world_file( WORLD_FILE_EXT_J2K_TFW, WORLD_FILE_EXT_J2K_TFW2 )
      else if ex = FILE_EXT_JP2 then
              use_world_file( WORLD_FILE_EXT_JP2_TFW, WORLD_FILE_EXT_JP2_TFW2 ) ;

      inherited setUp ;

      if SafeFileExists( Path ) then
        FAge := GisFileAge( Path ) ;

      FFileInfo := Format( typename + #13#10 +
                           '%d x %d; %d bit; %d band(s)' + #13#10 +
                           copyright,
                           [ FBitWidth, FBitHeight, realBitCount, nBands ]
                         ) ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 3) ;
    end ;
  end ;

  procedure TGIS_LayerECW.getAsyncState( var _abort  : Boolean ;
                                         var _ready  : Boolean ;
                                         var _nodata : Boolean
                                       ) ;
  var
    dt : Cardinal ;
  begin
    // for safe inheritance only
    _ready  := not viewState.notComplette  ;
    _nodata := viewState.empty ;

    if useCallBack then begin
      dt := GetTickCount - viewState.lastreadtime ;
      if dt > ECW_MAX_WAITING_TIME then begin
        if viewState.viewsets < ECW_MAX_VIEW_SETS then begin
          setFileView(viewState.lastviewrect) ;
          viewState.lastreadtime := GetTickCount ;
          inc(viewState.viewsets) ;
        end
        else begin
          _ready  := True ;
          _abort  := True ;
          _nodata := False ;
        end ;
      end ;
    end ;
    viewState.cancelled := _abort ;
  end ;

  function TGIS_LayerECW.getLine( const _buffer : TBytes  ;
                                  const _offset : Integer ;
                                  const _linenr : Integer ;
                                  const _start  : Integer ;
                                  const _bytes  : Integer
                                ) : Integer ;
  var
    loc_start     : Integer ;
    dx            : Integer ;
    src_line      : TBytes  ;
    src_linex     : Integer ;
    xl            : Integer ;
    bytes4        : Integer ;
    line_nr       : Integer ;
    zoomed_height : Integer ;
    pixels        : Integer ;

    procedure copy_bytes ;
    var
      i   : Integer ;
    begin
      while viewState.inReadProcess do ; //waiting while data reading
      for i := 0 to _bytes -1 do
        _buffer[_offset+i] := src_line[src_linex+i];
    end ;

    procedure copy_bytes4 ;
    var
      i   : Integer ;
    begin
      while viewState.inReadProcess do ; //waiting while data reading
      for i := 0 to pixels -1 do begin
        _buffer[_offset+i*3 +0] := src_line[src_linex+i*4 +0];
        _buffer[_offset+i*3 +1] := src_line[src_linex+i*4 +1];
        _buffer[_offset+i*3 +2] := src_line[src_linex+i*4 +2];
      end;
    end ;

  begin
    xl := _start div 3 ;
    dx :=  xl - xLeft ;
    if dx < 0 then
      dx := 0 ;
    pixels := _bytes div 3 ;

    if actualWidth = 0 then bytes4 := ( ( pixels*pixelBytes +3) div 4 ) * 4
                       else bytes4 := ( (actualWidth*pixelBytes + 3 ) div 4 ) * 4 ;

    zoomed_height := RoundS(FBitHeight * actualZoom) ;
    if _linenr >= zoomed_height then
      line_nr := zoomed_height -1
    else
      line_nr := _linenr ;

    if line_nr < stripStart then
      line_nr := stripStart ;

    loc_start := line_nr -stripStart ;
    src_line  := stripBuffer ;
    src_linex := loc_start * bytes4 + pixelBytes * dx ;
    if pixelBytes = 3 then
      copy_bytes
    else
      copy_bytes4 ;

    Result := _bytes ;

  end;

  function  TGIS_LayerECW.getAlphaLine(
      const _buffer : TBytes  ;
      const _offset : Integer ;
      const _linenr : Integer ;
      const _start  : Integer ;
      const _bytes  : Integer
                                      ) : Integer;
  var
    loc_start     : Integer ;
    dx            : Integer ;
    src_line    : TBytes  ;
    src_linex   : Integer ;
    xl     : Integer ;
    bytes4        : Integer ;
    line_nr       : Integer ;
    zoomed_height : Integer ;
    pixels : Integer ;
    i : Integer ;
  begin
    if FBandsCount <= 3 then begin
      Result := inherited getAlphaLine(_buffer, _offset, _linenr, _start, _bytes) ;
      exit ;
    end;

    xl := _start ;
    dx :=  xl - xLeft ;
    if dx < 0 then
      dx := 0 ;
    pixels := _bytes ;

    if actualWidth = 0 then bytes4 := ( ( pixels*pixelBytes +3) div 4 ) * 4
                       else bytes4 := ( (actualWidth*pixelBytes + 3 ) div 4 ) * 4 ;

    zoomed_height := RoundS(FBitHeight * actualZoom) ;
    if _linenr >= zoomed_height then
      line_nr := zoomed_height -1
    else
      line_nr := _linenr ;

    if line_nr < stripStart then
      line_nr := stripStart ;

    loc_start := line_nr -stripStart ;
    src_line  := stripBuffer ;
    src_linex := loc_start * bytes4 + pixelBytes * dx ;

    if isJP2000 then begin
      for i := 0 to pixels -1 do begin
        if src_line[src_linex+i*4 +3] <> 0 then
          _buffer[_offset+i] := $FF
        else
          _buffer[_offset+i] := $00 ;
      end;
    end
    else begin
      for i := 0 to pixels -1 do begin
        _buffer[_offset+i] := src_line[src_linex+i*4 +3];
      end;
    end;
    Result := _bytes ;

  end;

  function TGIS_LayerECW.getLinePixels( const _buffer   : TGIS_Pixels  ;
                                        const _offset   : Integer ;
                                        const _linenr   : Integer ;
                                        const _pixStart : Integer ;
                                        const _pixCount : Integer
                                       ) : Integer;
  var
    loc_start     : Integer ;
    dx            : Integer ;
    src_line      : TBytes  ;
    src_linex     : Integer ;
    xl            : Integer ;
    bytes4        : Integer ;
    line_nr       : Integer ;
    zoomed_height : Integer ;
    pixels        : Integer ;

    procedure copy_bytes_set_alpha ;
    var
      i   : Integer ;
    begin
      while viewState.inReadProcess do ; //waiting while data reading
      for i := 0 to pixels -1 do begin
        _buffer[_offset +i] :=  Integer(src_line[src_linex +i*3 +0]) or
                               (Integer(src_line[src_linex +i*3 +1]) shl 08) or
                               (Integer(src_line[src_linex +i*3 +2]) shl 16) or
                                Integer($FF000000) ;
      end;
    end ;

    procedure copy_bytes4 ;
    var
      i   : Integer ;
    begin
      while viewState.inReadProcess do ; //waiting while data reading
      for i := 0 to pixels -1 do begin
        _buffer[_offset +i] :=  Integer(src_line[src_linex +i*4 +0]) or
                               (Integer(src_line[src_linex +i*4 +1]) shl 08) or
                               (Integer(src_line[src_linex +i*4 +2]) shl 16) or
                               (Integer(src_line[src_linex +i*4 +3]) shl 24) ;
      end;
    end ;

  begin
    xl := _pixStart ;
    dx :=  xl - xLeft ;
    if dx < 0 then
      dx := 0 ;
    pixels := _pixCount ;

    if actualWidth = 0 then bytes4 := ( ( pixels*pixelBytes +3) div 4 ) * 4
                       else bytes4 := ( (actualWidth*pixelBytes + 3 ) div 4 ) * 4 ;

    zoomed_height := RoundS(FBitHeight * actualZoom) ;
    if _linenr >= zoomed_height then
      line_nr := zoomed_height -1
    else
      line_nr := _linenr ;

    if line_nr < stripStart then
      line_nr := stripStart ;

    loc_start := line_nr -stripStart ;
    src_line  := stripBuffer ;
    src_linex := loc_start * bytes4 + pixelBytes * dx ;
    if pixelBytes = 3 then
      copy_bytes_set_alpha
    else
      copy_bytes4 ;

    Result := pixels ;

  end;

  function  TGIS_LayerECW.setFileScale( const _dwidth : Double ;
                                        const _swidth : Double
                                      ) : Double ;
  begin
    if _swidth <> 0 then begin
      Result := _dwidth / _swidth ;
    end
    else
      Result := 1 ;

    if Result > 1 then Result := 1 ;

    if Abs(Abs(actualZoom) -Abs(Result)) > 0.5/_dwidth then begin
//      viewState.empty := True ;
      actualZoom := Result ;
    end
    else
      Result := actualZoom ;
  end ;

  procedure TGIS_LayerECW.setupParams ;
  begin
    inherited ;

    if FGridBand > 0 then begin
      bandList[0] := FGridBand -1 ;
      bandList[1] := bandList[0] ;
      bandList[2] := bandList[0] ;
      FAntialias := True ;
    end
    else begin
      bandList[0] := bandsMap[0] ;
      bandList[1] := bandsMap[1] ;
      bandList[2] := bandsMap[2] ;
    end;
  end;

  procedure TGIS_LayerECW.setFileView( const _viewRect : TRect ) ;
  var
    zxl, zxr,
    zyt, zyb      : Integer  ;
    cor           : Integer  ;
    actual_size   : Integer  ;
    dt1           : Cardinal ;
    dt2           : Cardinal ;
    vb, vt, vl, vr : Integer ;
    k             : Integer ;
    {$IFNDEF OXYGENE}
      pFileViewInfo : PGIS_LayerECW_NCSFileViewSetInfo ;
      i: Integer;
    {$ENDIF}
  const
    MIN_LINE_NUMBER = 5 ;
    function theSameBands : Boolean ;
    var
      kk : Integer ;
    begin
      Result := True ;
      for kk := 0 to 3 do begin
        if bandList[kk] <> bandListOld[kk] then begin
          Result := False ;
          exit ;
        end;
      end;
    end ;
  begin

    if (bandBytesDepth > 1) and (nBands > 1) then begin
      if not assigned(bilBuffer) then begin
        SetLength( bilBuffer, (FBitWidth +2)*bandBytesDepth ) ;
      end;
      setFileViewBIL( _viewRect ) ;
      exit ;
    end;

    // gentle time between server calls
    if useCallBack and isFromNet then begin
      if not viewState.firstCall then begin
         dt1 := 4000 * GetRandom(100) div 100 + 1000 ;
         dt2 := GetTickCount - viewState.lastreadtime ;

        if dt1 > dt2 then
          Sleep( dt1 - dt2 ) ;
      end
      else
        viewState.firstCall := False ;
    end ;

    viewState.lastreadtime := GetTickCount ;
    vb := _viewRect.Bottom ;
    vr := _viewRect.Right ;
    vt := _viewRect.Top ;
    vl := _viewRect.Left ;

    if vb >= RoundS(FBitHeight * actualZoom) then
      vb := RoundS(FBitHeight * actualZoom) -1 ;

    if vr >= RoundS(FBitWidth * actualZoom) then
      vr := RoundS(FBitWidth * actualZoom) -1 ;

    if (viewState.lastviewrect.Left   <=  vl  ) and
       (viewState.lastviewrect.Top    <= vt   ) and
       (viewState.lastviewrect.Right  >=  vr ) and
       (viewState.lastviewrect.Bottom >= vb)
    then begin
      if not viewState.empty and(lastZoom = actualZoom) then
        if theSameBands then
          exit ;
    end ;
    if (vb - vt  +1) < MIN_LINE_NUMBER then begin
      vb := vt + MIN_LINE_NUMBER -1 ;
      if vb >= RoundS(FBitHeight * actualZoom) then
        vb := RoundS(FBitHeight * actualZoom) -1 ;
    end;

    viewState.viewsets := 0 ;
    viewState.lastviewrect := Rect(vl, vt, vr, vb) ;
    actual_size := (vb - vt +3)*(vr - vl +4)*pixelBytes ;

    if (actual_size > stripBufferSize) then begin
      stripBufferSize := actual_size ;

      { TODO : Verify code }
//      {$IFDEF OXYGENE}
        SetLength( stripBuffer, Integer(stripBufferSize) ) ;
//      {$ELSE}
//        if assigned(stripBuffer) then
//          GlobalFreePtr(stripBuffer) ;
//        stripBuffer := GlobalAllocPtr(GMEM_MOVEABLE or GMEM_SHARE, stripBufferSize ) ;
//      {$ENDIF}
    end ;

    stripStart := vt ;
    xLeft := vl ;
    if actualZoom >= 1 then begin
      if xLeft > 0 then
        dec(xLeft) ;
    end ;

    stripHeight := vb -vt +1 ;
    actualWidth := vr -xLeft +1 ;

    if actualZoom >= 1 then begin
      if xLeft +actualWidth > FBitWidth then begin
        if xLeft > 0 then
          dec(xLeft) ;
        actualWidth := FBitWidth -xLeft ;
      end ;
    end ;
    lastZoom := actualZoom ;

    zyt := TruncS(stripStart/actualZoom) ;
    zyb := TruncS(vb/actualZoom) ;

    if zyb >= FBitHeight then begin
      cor := zyb - FBitHeight +1 ;
      zyb := zyb -cor ;
      zyt := zyt -cor ;
      if zyt < 0 then
        zyt := 0 ;
    end ;
    zxl := TruncS(xLeft/actualZoom) ;

    zxr := TruncS(vr/actualZoom) ;

    if zxr >= FBitWidth then
      zxr := FBitWidth -1 ;

    if actualZoom >= 1 then begin
      if stripStart +stripHeight > FBitHeight then
        stripHeight := FBitHeight -stripStart ;
    end ;

    if actualWidth > (zxr -zxl + 1) then
      actualWidth := (zxr -zxl + 1) ;
    if stripHeight > (zyb -zyt + 1) then
      stripHeight := (zyb -zyt + 1) ;

    stripLinesNo := stripHeight ;

    viewState.linesNeeded := stripHeight ;
    viewState.linesRead := 0 ;

    viewState.portion := ((pixelBytes*actualWidth +3)div 4)*4 ;
    viewState.buffer := stripBuffer ;
    viewState.workBuffer := stripBuffer ;

    viewState.nBands       := nBands ;
    {$IFDEF OXYGENE}
      viewState.bandList   := bandList ;
    {$ELSE}
      viewState.bandList   := @(bandList[0]) ;
    {$ENDIF}
    viewState.left         := zxl ;
    viewState.top          := zyt ;
    viewState.right        := zxr ;
    viewState.bottom       := zyb ;
    viewState.sizeX        := actualWidth ;
    viewState.sizeY        := stripHeight ;

    viewState.empty        := True ;
    viewState.notComplette := True ;
    viewState.actualBlocks := 0    ;
    viewState.notRead      := True ;
    viewState.linesRead    := 0    ;

    {$IFDEF OXYGENE}
      Layer := Self ;
    {$ELSE}
      ncsGetViewInfo( pFileView, @pFileViewInfo ) ;
      pFileViewInfo^.pClientData := self ;
    {$ENDIF}

    {$IFDEF OXYGENE}
      ncsSetFileView( pFileView, nBands, bandList,
                      zxl, zyt, zxr, zyb, actualWidth, stripHeight
                    ) ;
    {$ELSE}
      ncsSetFileView( pFileView, nBands, @(bandList[0]),
                      zxl, zyt, zxr, zyb, actualWidth, stripHeight
                    ) ;
    {$ENDIF}
    for k := 0 to 3 do
      bandListOld[k] := bandList[k] ;

    viewState.empty        := True ;
    viewState.notComplette := True ;
    viewState.actualBlocks := 0    ;
    viewState.notRead      := True ;
    viewState.linesRead    := 0    ;

    if not useCallBack then
      ViewCallback( pFileView ) ;
  end ;

  procedure TGIS_LayerECW.setFileViewBIL( const _viewRect : TRect ) ;
  var
    zxl, zxr,
    zyt, zyb      : Integer  ;
    cor           : Integer  ;
    actual_size   : Integer  ;
    vb, vt, vl, vr : Integer ;
    first_band     : Cardinal ;
    ll, i : Integer ;
    ret             : TGIS_LayerECW_NCSEcwReadStatus ;
    iret : Integer ;
    {$IFDEF OXYGENE}
      pFileViewSetInfo : TGIS_LayerECW_NCSFileViewSetInfo ;
    {$ELSE}
      pFileViewSetInfo : PGIS_LayerECW_NCSFileViewSetInfo ;
    {$ENDIF}
    {$IFDEF CLR}
      iptr          : IntPtr  ;
      iptr2         : IntPtr  ;
      piptr         : IntPtr ;
      si            : Integer ;
    {$ENDIF}
    {$IFDEF JAVA}
      iptr          : com.sun.jna.Pointer  ;
      iptr2         : com.sun.jna.Pointer  ;
      piptr         : com.sun.jna.Pointer ;
      si            : Integer ;
    {$ENDIF}
      src_step, dst_step : Integer ; //for copy buffer function
      base_dst_off : Integer ;
  const
    MIN_LINE_NUMBER = 200 ;
    procedure copy_buffer ;
    var
      ii : Integer ;
      src_off : Integer ;
      dst_off : Integer ;
    begin
      src_off := 0 ;
      dst_off := base_dst_off ;
      {$IFDEF OXYGENE}
        for ii := 0 to actualWidth -1 do begin
          stripBuffer[dst_off] := bilBuffer[src_off] ;
          inc(dst_off, dst_step) ;
          inc(src_off, src_step) ;
        end;
      {$ELSE}
        for ii := 0 to actualWidth -1 do begin
          PByte(IntPtr(stripBuffer) +dst_off)^ := PByte(IntPtr(bilBuffer) +src_off)^ ;
          inc(dst_off, dst_step) ;
          inc(src_off, src_step) ;
        end;
      {$ENDIF}
    end;
  begin
    vb := _viewRect.Bottom ;
    vr := _viewRect.Right ;
    vt := _viewRect.Top ;
    vl := _viewRect.Left ;

    if vb >= RoundS(FBitHeight * actualZoom) then
      vb := RoundS(FBitHeight * actualZoom) -1 ;

    if vr >= RoundS(FBitWidth * actualZoom) then
      vr := RoundS(FBitWidth * actualZoom) -1 ;

    if (viewState.lastviewrect.Left   <=  vl  ) and
       (viewState.lastviewrect.Top    <= vt   ) and
       (viewState.lastviewrect.Right  >=  vr ) and
       (viewState.lastviewrect.Bottom >= vb)
    then begin
      if not viewState.empty and(lastZoom = actualZoom) then
        exit ;
    end ;
    if (vb - vt  +1) < MIN_LINE_NUMBER then begin
      vb := vt + MIN_LINE_NUMBER -1 ;
      if vb >= RoundS(FBitHeight * actualZoom) then
        vb := RoundS(FBitHeight * actualZoom) -1 ;
    end;

    viewState.viewsets := 0 ;
    viewState.lastviewrect := Rect(vl, vt, vr, vb) ;
    actual_size := (vb - vt +3)*(vr - vl +4)*pixelBytes ;

    if (actual_size > stripBufferSize) then begin
      stripBufferSize := actual_size ;

      SetLength( stripBuffer, Integer(stripBufferSize) ) ;
    end ;

    stripStart := vt ;
    xLeft := vl ;
    if actualZoom >= 1 then begin
      if xLeft > 0 then
        dec(xLeft) ;
    end ;

    stripHeight := vb -vt +1 ;
    actualWidth := vr -xLeft +1 ;

    if actualZoom >= 1 then begin
      if xLeft +actualWidth > FBitWidth then begin
        if xLeft > 0 then
          dec(xLeft) ;
        actualWidth := FBitWidth -xLeft ;
      end ;
    end ;
    lastZoom := actualZoom ;

    zyt := TruncS(stripStart/actualZoom) ;
    zyb := TruncS(vb/actualZoom) ;

    if zyb >= FBitHeight then begin
      cor := zyb - FBitHeight +1 ;
      zyb := zyb -cor ;
      zyt := zyt -cor ;
      if zyt < 0 then
        zyt := 0 ;
    end ;
    zxl := TruncS(xLeft/actualZoom) ;

    zxr := TruncS(vr/actualZoom) ;

    if zxr >= FBitWidth then
      zxr := FBitWidth -1 ;

    if actualZoom >= 1 then begin
      if stripStart +stripHeight > FBitHeight then
        stripHeight := FBitHeight -stripStart ;
    end ;

    if actualWidth > (zxr -zxl + 1) then
      actualWidth := (zxr -zxl + 1) ;
    if stripHeight > (zyb -zyt + 1) then
      stripHeight := (zyb -zyt + 1) ;

    stripLinesNo := stripHeight ;

    viewState.linesNeeded := stripHeight ;
    viewState.linesRead   := 0 ;

    viewState.portion := ((pixelBytes*actualWidth +3)div 4)*4 ;
    viewState.buffer := stripBuffer ;
    viewState.workBuffer := stripBuffer ;

    viewState.nBands       := nBands ;
    {$IFDEF OXYGENE}
      viewState.bandList   := bandList ;
    {$ELSE}
      viewState.bandList   := @(bandList[0]) ;
    {$ENDIF}

    dst_step := FBandsCount ;
    src_step := bandBytesDepth ;
    {$IFDEF OXYGENE}
      iptr  := Marshal.AllocHGlobal( length(bilBuffer) ) ;
      si := Marshal.SizeOf(iptr) ;
      piptr := Marshal.AllocHGlobal( si ) ;
      Marshal.StructureToPtr(iptr, piptr, False);
    {$ENDIF}

    first_band := bandList[0] ;
    //Loop bands reading
    for ll := 0 to FBandsCount -1  do begin

      bandList[0] := bandList[ll] ;
      base_dst_off := ll ;

      {$IFDEF OXYGENE}
        ncsSetFileView( pFileView, 1, bandList,
                        zxl, zyt, zxr, zyb, actualWidth, stripHeight
                      ) ;
      {$ELSE}
        ncsSetFileView( pFileView, 1, @(bandList[0]),
                        zxl, zyt, zxr, zyb, actualWidth, stripHeight
                      ) ;
      {$ENDIF}

      viewState.empty        := True ;
      viewState.notComplette := True ;
      viewState.actualBlocks := 0    ;
      viewState.notRead      := True ;
      viewState.linesRead    := 0    ;

      //Lines reading
      {$IFDEF OXYGENE}
        ncsGetViewInfo( pFileView, iptr2 ) ;
        {$IFDEF JAVA}
        if iptr2 = nil then Abort ;
        {$ELSE}
        if iptr2 = 0 then Abort ;
        {$ENDIF}
        pFileViewSetInfo := TGIS_LayerECW_NCSFileViewSetInfo(
          Marshal.PtrToStructure( iptr2, typeOf(TGIS_LayerECW_NCSFileViewSetInfo) )
        ) ;
      {$ELSE}
        ncsGetViewInfo( pFileView, @pFileViewSetInfo ) ;
      {$ENDIF}

        viewState.linesRead := 0 ;
        viewState.notComplette := False ;

        for i := viewState.linesRead to
                 pFileViewSetInfo.nSizeY - 1 do
        begin
          {$IFDEF OXYGENE}
            iret := ncsReadViewLineBILEx( pFileView, pFileViewInfo.eCellType, piptr ) ;
            {$IFDEF JAVA}
            case iret of
              0 : ret := TGIS_LayerECW_NCSEcwReadStatus.NCSECW_READ_OK ;
              1 : ret := TGIS_LayerECW_NCSEcwReadStatus.NCSECW_READ_FAILED ;
              2 : ret := TGIS_LayerECW_NCSEcwReadStatus.NCSECW_READ_CANCELLED ;
            end;
            {$ELSE}
            ret := TGIS_LayerECW_NCSEcwReadStatus( iret ) ;
            {$ENDIF}

            Marshal.Copy( iptr,
                          bilBuffer,
                          0,
                          length(bilBuffer)
                        ) ;
          {$ELSE}
              ret := TGIS_LayerECW_NCSEcwReadStatus(
                       ncsReadViewLineBILEx( pfileView, pFileViewInfo^.eCellType, @bilBuffer )
                     ) ;
          {$ENDIF}
          if ret <> TGIS_LayerECW_NCSEcwReadStatus.NCSECW_READ_OK then
            break ;
          copy_buffer ;
          inc(base_dst_off, actualWidth*FBandsCount) ;
          inc(viewState.linesRead) ;
        end ;
   end;
    //End of data reading
   {$IFDEF OXYGENE}
     Marshal.FreeHGlobal(piptr) ;
     Marshal.FreeHGlobal(iptr) ;
   {$ENDIF}
    viewState.empty := False ;
    viewState.inReadProcess := False ;
    bandList[0] := first_band ;
  end ;

  procedure TGIS_LayerECW.Alive ;
  begin
    reloadLibrary ;
  end ;

  function TGIS_LayerECW.DormantGain
    : Integer ;
  begin
    case DormantMode of
      TGIS_LayerDormantMode.Off :
        Result := 0;
      else
        Result := 5 ;
    end ;
  end ;

  procedure TGIS_LayerECW.Dormant ;
  var
    {$IFDEF CLR}
      iptr : IntPtr ;
    {$ENDIF}
    {$IFDEF JAVA}
      iptr : com.sun.jna.Pointer ;
    {$ENDIF}
    {$IFDEF DCC}
      pFileViewInfo : PGIS_LayerECW_NCSFileViewSetInfo ;
    {$ENDIF}
    bfwass : Boolean ;
    blibass : Boolean ;
  begin
    if DormantMode = TGIS_LayerDormantMode.Off then
      exit ;

    inherited ;

    {$IFDEF JAVA}
      blibass := libHandle <> nil ;
    {$ELSE}
      blibass := libHandle <> 0 ;
    {$ENDIF}

    if blibass then begin
      {$IFDEF JAVA}
        bfwass := assigned( pFileView ) ;
      {$ELSE}
        bfwass := AssignedPtr( pFileView ) ;
      {$ENDIF}
      if bfwass then begin
        if useCallBack then begin
          {$IFDEF OXYGENE}
            ncsGetViewInfo( pFileView, iptr ) ;
          {$ELSE}
            ncsGetViewInfo( pFileView, @pFileViewInfo ) ;
          {$ENDIF}
        end ;
        ncsCloseFileView( pFileView, True ) ;
      end ;

      viewState.cancelled := False ;

      pFileView := nil ;
      viewState.notRead := False ;

      if assigned(stripBuffer) then begin
        stripBuffer := nil ;
      end ;

      if assigned(bilBuffer) then begin
        bilBuffer := nil ;
      end ;

      stripBufferSize := 0 ;
      // decompressed lines number
      stripLinesNo := 0 ;
      viewState.linesRead := 0 ;
      viewState.empty := True ;
    end ;
  end ;

  { Perform initialization section.
  }
  class procedure GisLayerECW.SelfRegisterLayer() ;
  begin
    {$IFDEF OXYGENE}
      if not ( (TGIS_DeveloperKernelTypes.SERVER and GisDeveloperKernelType)
                = TGIS_DeveloperKernelTypes.SERVER
             )
      then
    {$ELSE}
      if not ( TGIS_DeveloperKernelTypes.SERVER in GisDeveloperKernelType ) then
    {$ENDIF}
    begin
      RegisterLayer( 'DK-ECW', 'Enhanced Compressed Wavelet', TGIS_LayerECW, '.ecw'
                     ,TGIS_RegisteredLayerType.Pixel, TGIS_RegisteredFormatType.Local,
                     [ TGIS_RegisteredOperationType.Read ],
                     True
                   ) ;
      RegisterLayer( 'DK-J2K_ECW', 'JPEG 2000 (via ECW)', TGIS_LayerECW,
                     '.jp2'
                     ,TGIS_RegisteredLayerType.Pixel, TGIS_RegisteredFormatType.Local,
                     [ TGIS_RegisteredOperationType.Read ],
                     True
                   ) ;
      RegisterLayer( 'DK-J2K_ECW', 'JPEG 2000 (via ECW)', TGIS_LayerECW,
                     '.j2k;.jpf;.jpx;.jpc;.j2c'
                     ,TGIS_RegisteredLayerType.Pixel, TGIS_RegisteredFormatType.Local,
                     [ TGIS_RegisteredOperationType.Read ],
                     GIS_LOWER_LAYER_PRIORITY, False
                   ) ;
    end ;
  end;

{$IFNDEF OXYGENE}
  initialization
    GisLayerECW.SelfRegisterLayer() ;

  finalization
      FreeObject( cs_gcsMap ) ;
      FreeObject( cs_pcsMap ) ;
{$ENDIF}

{==================================== END =====================================}
end.

