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
  Support for simply layer creation.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoRegistredLayers ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoRegistredLayers"'}
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
    System.Runtime.CompilerServices,
    System.Reflection,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    System.Generics.Collections,
    System.Generics.Defaults,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoLayer ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl,
    tatukgis.rtl ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    TatukGIS.RTL ;
{$ENDIF}

const

  /// <summary>
  ///   Driver priority - high
  /// </summary>
  GIS_HIGH_LAYER_PRIORITY    = 1   ;

  /// <summary>
  ///   Driver priority - default
  /// </summary>
  GIS_DEFAULT_LAYER_PRIORITY = 100 ;

  /// <summary>
  ///   Driver priority - low
  /// </summary>
  GIS_LOWER_LAYER_PRIORITY   = 110 ;

  /// <summary>
  ///   Driver priority - other
  /// </summary>
  GIS_OTHER_LAYER_PRIORITY   = 200 ;


type

  /// <summary>
  ///   Registered format type.
  /// </summary>
  TGIS_RegisteredFormatType = {$IFDEF OXYGENE} public {$ENDIF}
  (

    /// <summary>
    ///   Layer format type - local
    /// </summary>
    Local ,

    /// <summary>
    ///   Layer format type - server
    /// </summary>
    Server,

    /// <summary>
    ///   Layer format type - protocol
    /// </summary>
    Protocol
  ) {$IFDEF JAVA} of Integer {$ENDIF} ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
     gisRegisteredFormatTypeLocal    = TGIS_RegisteredFormatType.Local    ;
     gisRegisteredFormatTypeServer   = TGIS_RegisteredFormatType.Server   ;
     gisRegisteredFormatTypeProtocol = TGIS_RegisteredFormatType.Protocol ;
  {$ENDIF}

type

  /// <summary>
  ///   Registered operation type.
  /// </summary>
  TGIS_RegisteredOperationType = {$IFDEF OXYGENE} public {$ENDIF}
  (
    /// <summary>
    ///   Read operation.
    /// </summary>
    &Read   ,

    /// <summary>
    ///   Write operation.
    /// </summary>
    &Write  ,

    /// <summary>
    ///   Create operation.
    /// </summary>
    &Create ,

    /// <summary>
    ///   Merge operation.
    /// </summary>
    Merge
  ) {$IFDEF JAVA} of Integer {$ENDIF} ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisRegisteredOperationTypeRead   = TGIS_RegisteredOperationType.Read    ;
      gisRegisteredOperationTypeWrite  = TGIS_RegisteredOperationType.Write   ;
      gisRegisteredOperationTypeCreate = TGIS_RegisteredOperationType.&Create ;
      gisRegisteredOperationTypeMerge  = TGIS_RegisteredOperationType.Merge   ;
  {$ENDIF}

type

  /// <summary>
  ///   Registered operations set.
  /// </summary>
  TGIS_RegisteredOperations = {$IFDEF OXYGENE} public {$ENDIF} set of TGIS_RegisteredOperationType ;


  /// <summary>
  ///   Class that encapsulates the layer creation process.
  /// </summary>
  TGIS_RegistredLayers = {$IFDEF OXYGENE} public {$ENDIF} class
    public

    public

      /// <summary>
      ///   Create a layer given by a name.
      /// </summary>
      /// <param name="_name">
      ///   the name under which the layer will be recognized; if empty,
      ///   then the default name will be assigned (same as path)
      /// </param>
      /// <param name="_path">
      ///   path (with extension)
      /// </param>
      /// <returns>
      ///   created layer or nil
      /// </returns>
      class function GisCreateLayer
                            ( const _name : String ;
                              const _path : String
                            ) : TGIS_Layer ;
                            {$IFDEF GIS_STATIC} static ; {$ENDIF}

      /// <summary>
      ///   Prepare the list of supported files for use by open/save
      ///   dialog box.
      /// </summary>
      /// <param name="_mode">
      ///   mode of query (only vector files, all files etc.)
      /// </param>
      /// <param name="_save">
      ///   if true, the string for save dialog box context will be
      ///   prepared; if false then string for open dialog box context
      ///   will be prepared;
      /// </param>
      /// <returns>
      ///   string for dialog box filter in a format:
      ///     names|ext|name|ext|...
      ///   or list of registered names in a format
      ///     .EXT=format_nameCRLF
      /// </returns>
      class function GisSupportedFiles
                          ( const _mode        : TGIS_FileTypes ;
                            const _save        : Boolean
                          ) : String ; overload;

      /// <summary>
      ///   Prepare the list of supported files for use by open/save
      ///   dialog box.
      /// </summary>
      /// <param name="_mode">
      ///   mode of query (only vector files, all files etc.)
      /// </param>
      /// <param name="_save">
      ///   if true, the string for save dialog box context will be
      ///   prepared; if false then string for open dialog box context
      ///   will be prepared;
      /// </param>
      /// <param name="_fileformats">
      ///   if true then list of all supported file extensions and format
      ///   names will be returned instead; it can be used for providing
      ///   for example for file registration purposes
      /// </param>
      /// <returns>
      ///   string for dialog box filter in a format:
      ///     names|ext|name|ext|...
      ///   or list of registered names in a format
      ///     .EXT=format_nameCRLF
      /// </returns>
      /// <remarks>
      ///   By providing _fileformats = True function will return list
      ///   off all supported files extension and names of file format.
      /// </remarks>
      class function GisSupportedFiles
                            ( const _mode        : TGIS_FileTypes ;
                              const _save        : Boolean        ;
                              const _fileformats : Boolean
                            ) : String ; overload;

      /// <summary>
      ///   Register layer on the supported drivers' list.
      /// </summary>
      /// <param name="_driverName">
      ///   driver name
      /// </param>
      /// <param name="_driverInfo">
      ///   driver description
      /// </param>
      /// <param name="_class">
      ///   type of class used to process a layer
      /// </param>
      /// <param name="_extensions">
      ///   driver supported files extensions, separated with ";"
      /// </param>
      /// <param name="_layerType">
      ///   layer type
      /// </param>
      /// <param name="_formatType">
      ///   layer format type
      /// </param>
      /// <param name="_operations">
      ///   allowed operations
      /// </param>
      class procedure RegisterLayer
                         ( const _driverName  : String ;
                           const _driverInfo  : String ;
                           const _class       : TGIS_RegisteredLayerAbstract ;
                           const _extensions  : String ;
                           const _layerType   : TGIS_RegisteredLayerType  ;
                           const _formatType  : TGIS_RegisteredFormatType ;
                           const _operations  : TGIS_RegisteredOperations
                         ) ; overload;

      /// <summary>
      ///   Register layer on the supported drivers' list.
      /// </summary>
      /// <param name="_driverName">
      ///   driver name
      /// </param>
      /// <param name="_driverInfo">
      ///   driver description
      /// </param>
      /// <param name="_class">
      ///   type of class used to process a layer
      /// </param>
      /// <param name="_extensions">
      ///   driver supported files extensions, separated with ";"
      /// </param>
      /// <param name="_layerType">
      ///   layer type
      /// </param>
      /// <param name="_formatType">
      ///   layer format type
      /// </param>
      /// <param name="_operations">
      ///   allowed operations
      /// </param>
      /// <param name="_common">
      ///   if True, driver will be registered as commonly used
      /// </param>
      class procedure RegisterLayer
                         ( const _driverName  : String ;
                           const _driverInfo  : String ;
                           const _class       : TGIS_RegisteredLayerAbstract ;
                           const _extensions  : String ;
                           const _layerType   : TGIS_RegisteredLayerType  ;
                           const _formatType  : TGIS_RegisteredFormatType ;
                           const _operations  : TGIS_RegisteredOperations ;
                           const _common      : Boolean
                         ) ; overload;

      /// <summary>
      ///   Register layer on the supported drivers' list.
      /// </summary>
      /// <param name="_driverName">
      ///   driver name
      /// </param>
      /// <param name="_driverInfo">
      ///   driver description
      /// </param>
      /// <param name="_class">
      ///   type of class used to process a layer
      /// </param>
      /// <param name="_extensions">
      ///   driver supported files extensions, separated with ";"
      /// </param>
      /// <param name="_layerType">
      ///   layer type
      /// </param>
      /// <param name="_formatType">
      ///   layer format type
      /// </param>
      /// <param name="_operations">
      ///   allowed operations
      /// </param>
      /// <param name="_priority">
      ///   driver priority, lower value has higher priority on list
      /// </param>
      /// <param name="_common">
      ///   if True, driver will be registered as commonly used
      /// </param>
      class procedure RegisterLayer
                           ( const _driverName  : String ;
                             const _driverInfo  : String ;
                             const _class       : TGIS_RegisteredLayerAbstract ;
                             const _extensions  : String ;
                             const _layerType   : TGIS_RegisteredLayerType  ;
                             const _formatType  : TGIS_RegisteredFormatType ;
                             const _operations  : TGIS_RegisteredOperations ;
                             const _priority    : Integer ;
                             const _common      : Boolean
                           ) ; overload;


      /// <summary>
      ///   Set driver default flag on the supported drivers' list.
      /// </summary>
      /// <param name="_driverName">
      ///   driver name
      /// </param>
      /// <param name="_priority">
      ///   driver priority, lower value has higher priority on list
      /// </param>
      class procedure SetDriverPriority
                          ( const _driverName : String  ;
                            const _priority   : Integer
                          ) ;
  end ;

  /// <summary>
  ///   Create a layer given by a name.
  /// </summary>
  /// <param name="_name">
  ///   the name under which the layer will be recognized; if empty, then the
  ///   default name will be assigned (same as path)
  /// </param>
  /// <param name="_path">
  ///   path (with extension)
  /// </param>
  /// <returns>
  ///   created layer or nil
  /// </returns>
  function GisCreateLayer   ( const _name : String ;
                              const _path : String
                            ) : TGIS_Layer ;

  /// <summary>
  ///   Prepare the list of supported files for use by open/save dialog box.
  /// </summary>
  /// <param name="_mode">
  ///   mode of query (only vector files, all files etc.)
  /// </param>
  /// <param name="_save">
  ///   if true, the string for save dialog box context will be prepared; if
  ///   false then string for open dialog box context will be prepared;
  /// </param>
  /// <returns>
  ///   string for dialog box filter in a format:
  ///     names|ext|name|ext|...
  ///   or list of registered names in a format
  ///     .EXT=format_nameCRLF
  /// </returns>
  function GisSupportedFiles
                          ( const _mode        : TGIS_FileTypes ;
                            const _save        : Boolean
                          ) : String ; overload;

  /// <summary>
  ///   Prepare the list of supported files for use by open/save dialog box.
  /// </summary>
  /// <param name="_mode">
  ///   mode of query (only vector files, all files etc.)
  /// </param>
  /// <param name="_save">
  ///   if true, the string for save dialog box context will be prepared; if
  ///   false then string for open dialog box context will be prepared;
  /// </param>
  /// <param name="_fileformats">
  ///   if true then list of all supported file extensions and format names
  ///   will be returned instead; it can be used for example for file
  ///   registration purposes
  /// </param>
  /// <returns>
  ///   string for dialog box filter in a format:
  ///     names|ext|name|ext|...
  ///   or list of registered names in a format
  ///     .EXT=format_nameCRLF
  /// </returns>
  /// <remarks>
  ///   By providing _fileformats = True function will return list off all
  ///   supported files extension and names of file format.
  /// </remarks>
  function GisSupportedFiles( const _mode        : TGIS_FileTypes ;
                              const _save        : Boolean        ;
                              const _fileformats : Boolean
                            ) : String ; overload;

type

  /// <summary>
  ///   Registered layer class.
  /// </summary>
  TGIS_RegistredLayer = {$IFDEF OXYGENE} public {$ENDIF} class
    LayerClass     : TGIS_RegisteredLayerAbstract ;
    {$IFDEF OXYGENE}
      LayerClassEx : &Type ;
    {$ENDIF}
    Extensions     : String ;
    LayerType      : TGIS_RegisteredLayerType ;
    FormatType     : TGIS_RegisteredFormatType ;
    Operations     : TGIS_RegisteredOperations ;
    DriverName     : String ;
    DriverInfo     : String ;
    Priority       : Integer ;
    Common         : Boolean ;
  end ;

  /// <summary>
  ///   List of registered layers.
  /// </summary>
  TGIS_RegistredLayerList = {$IFDEF OXYGENE} public {$ENDIF}  TObjectList<TGIS_RegistredLayer> ;

  /// <summary>
  ///   <para>
  ///     Register layer on the supported drivers' list.
  ///   </para>
  ///   <para>
  ///     Register layer on the supported drivers' list.
  ///   </para>
  /// </summary>
  /// <param name="_driverName">
  ///   driver name
  /// </param>
  /// <param name="_driverInfo">
  ///   driver description
  /// </param>
  /// <param name="_class">
  ///   class used to process a layer
  /// </param>
  /// <param name="_extensions">
  ///   driver supported files extensions, separated with ";"
  /// </param>
  /// <param name="_layerType">
  ///   layer type
  /// </param>
  /// <param name="_formatType">
  ///   layer format type
  /// </param>
  /// <param name="_operations">
  ///   allowed operations
  /// </param>
  procedure RegisterLayer( const _driverName  : String ;
                           const _driverInfo  : String ;
                           const _class       : TGIS_RegisteredLayerAbstract ;
                           const _extensions  : String ;
                           const _layerType   : TGIS_RegisteredLayerType ;
                           const _formatType  : TGIS_RegisteredFormatType ;
                           const _operations  : TGIS_RegisteredOperations
                         ) ; overload;

  /// <summary>
  ///   Register layer on the supported drivers' list.
  /// </summary>
  /// <param name="_driverName">
  ///   driver name
  /// </param>
  /// <param name="_driverInfo">
  ///   driver description
  /// </param>
  /// <param name="_class">
  ///   class used to process a layer
  /// </param>
  /// <param name="_extensions">
  ///   driver supported files extensions, separated with ";"
  /// </param>
  /// <param name="_layerType">
  ///   layer type
  /// </param>
  /// <param name="_formatType">
  ///   layer format type
  /// </param>
  /// <param name="_operations">
  ///   allowed operations
  /// </param>
  /// <param name="_common">
  ///   if True, driver will be registered as commonly used
  /// </param>
  procedure RegisterLayer( const _driverName  : String ;
                           const _driverInfo  : String ;
                           const _class       : TGIS_RegisteredLayerAbstract ;
                           const _extensions  : String ;
                           const _layerType   : TGIS_RegisteredLayerType ;
                           const _formatType  : TGIS_RegisteredFormatType ;
                           const _operations  : TGIS_RegisteredOperations ;
                           const _common      : Boolean
                         ) ; overload;

  /// <summary>
  ///   Register layer on the supported drivers' list.
  /// </summary>
  /// <param name="_driverName">
  ///   driver name
  /// </param>
  /// <param name="_driverInfo">
  ///   driver description
  /// </param>
  /// <param name="_class">
  ///   class used to process a layer
  /// </param>
  /// <param name="_extensions">
  ///   driver supported files extensions, separated with ";"
  /// </param>
  /// <param name="_layerType">
  ///   layer type
  /// </param>
  /// <param name="_formatType">
  ///   layer format type
  /// </param>
  /// <param name="_operations">
  ///   allowed operations
  /// </param>
  /// <param name="_priority">
  ///   driver priority, lower value has higher priority on list
  /// </param>
  /// <param name="_common">
  ///   if True, driver will be registered as commonly used
  /// </param>
  procedure RegisterLayer  ( const _driverName  : String ;
                             const _driverInfo  : String ;
                             const _class       : TGIS_RegisteredLayerAbstract ;
                             const _extensions  : String ;
                             const _layerType   : TGIS_RegisteredLayerType ;
                             const _formatType  : TGIS_RegisteredFormatType ;
                             const _operations  : TGIS_RegisteredOperations ;
                             const _priority    : Integer ;
                             const _common      : Boolean
                           ) ; overload;


  {$IFDEF OXYGENE}

  /// <summary>
  ///   Call registration before first use of a registered layer (eg. inside GisCreateLayer).
  /// </summary>
  procedure SelfRegisterLayers ;
  {$ENDIF}

  /// <summary>
  ///   <para>
  ///     Set driver default flag on the supported drivers' list.
  ///   </para>
  ///   <para>
  ///     Set driver default flag on the supported drivers' list.
  ///   </para>
  /// </summary>
  /// <param name="_driverName">
  ///   driver name
  /// </param>
  /// <param name="_priority">
  ///   driver priority, lower value has higher priority on list
  /// </param>
  procedure SetDriverPriority( const _driverName  : String ;
                               const _priority    : Integer
                             ) ;

var

  /// <summary>
  ///   List of registered layers.
  /// </summary>
  lstRegisteredLayers : TGIS_RegistredLayerList ;

  /// <summary>
  ///   List of common layers.
  /// </summary>
  lstCommonLayers     : TDictionary<String, Integer> ;

  {$IFDEF JAVA}
    GisPackageCodePath : String ;
  {$ENDIF}

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoLogger,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoLayerPixel,
    Lider.CG.GIS.GeoLayerSHP,
    Lider.CG.GIS.GeoLayerGML,
    Lider.CG.GIS.GeoLayerJPG,
    Lider.CG.GIS.GeoLayerPNG,
    Lider.CG.GIS.GeoLayerWMS,
    Lider.CG.GIS.GeoLayerWFS,
    Lider.CG.GIS.GeoLayerWCS,
    Lider.CG.GIS.GeoLayerWMTS,
    Lider.CG.GIS.GeoLayerJSON,
    Lider.CG.GIS.GeoLayerTAB,
    Lider.CG.GIS.GeoLayerProject,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoConfig,
    Lider.CG.GIS.GeoFunctions ;
{$ENDIF}

{$IFDEF OXYGENE}
var
   wasSelfRegisterLayers : Boolean := False ;
   thcSelfRegisterLayers : TGIS_ThreadClass := TGIS_ThreadClass.Create ;
{$ENDIF}

type
  TGIS_RegisteredFormatTypes = set of TGIS_RegisteredFormatType ;

  class function TGIS_RegistredLayers.GisCreateLayer(
    const _name : String ;
    const _path : String
  ) : TGIS_Layer ;
  begin
    {$IFDEF OXYGENE}
      Result := __Global.GisCreateLayer( _name, _path ) ;
    {$ELSE}
      Result := GisCreateLayer( _name, _path ) ;
    {$ENDIF}
  end ;

  class function TGIS_RegistredLayers.GisSupportedFiles(
    const _mode        : TGIS_FileTypes ;
    const _save        : Boolean
  ) : String ;
  begin
    {$IFDEF OXYGENE}
      Result := __Global.GisSupportedFiles( _mode, _save ) ;
    {$ELSE}
      Result := GisSupportedFiles( _mode, _save ) ;
    {$ENDIF}
  end ;

  class function TGIS_RegistredLayers.GisSupportedFiles(
    const _mode        : TGIS_FileTypes ;
    const _save        : Boolean        ;
    const _fileformats : Boolean
  ) : String ;
  begin
    {$IFDEF OXYGENE}
      Result := __Global.GisSupportedFiles( _mode, _save, _fileformats ) ;
    {$ELSE}
      Result := GisSupportedFiles( _mode, _save, _fileformats ) ;
    {$ENDIF}
  end ;

  class procedure TGIS_RegistredLayers.RegisterLayer(
    const _driverName  : String ;
    const _driverInfo  : String ;
    const _class       : TGIS_RegisteredLayerAbstract ;
    const _extensions  : String ;
    const _layerType   : TGIS_RegisteredLayerType  ;
    const _formatType  : TGIS_RegisteredFormatType ;
    const _operations  : TGIS_RegisteredOperations
  ) ;
  begin
    {$IFDEF OXYGENE}
      __Global.RegisterLayer
                   ( _driverName,
                     _driverInfo,
                     _class,
                     _extensions,
                     _layerType,
                     _formatType,
                     _operations
                   ) ;
    {$ELSE}
      RegisterLayer( _driverName,
                     _driverInfo,
                     _class,
                     _extensions,
                     _layerType,
                     _formatType,
                     _operations
                   ) ;
    {$ENDIF}
  end ;

  class procedure TGIS_RegistredLayers.RegisterLayer(
    const _driverName  : String ;
    const _driverInfo  : String ;
    const _class       : TGIS_RegisteredLayerAbstract ;
    const _extensions  : String ;
    const _layerType   : TGIS_RegisteredLayerType  ;
    const _formatType  : TGIS_RegisteredFormatType ;
    const _operations  : TGIS_RegisteredOperations ;
    const _common      : Boolean
  ) ;
  begin
    {$IFDEF OXYGENE}
      __Global.RegisterLayer
                   ( _driverName,
                     _driverInfo,
                     _class,
                     _extensions,
                     _layerType,
                     _formatType,
                     _operations,
                     _common
                   ) ;
    {$ELSE}
      RegisterLayer( _driverName,
                     _driverInfo,
                     _class,
                     _extensions,
                     _layerType,
                     _formatType,
                     _operations,
                     _common
                   ) ;
    {$ENDIF}
  end ;

  class procedure TGIS_RegistredLayers.RegisterLayer(
    const _driverName  : String ;
    const _driverInfo  : String ;
    const _class       : TGIS_RegisteredLayerAbstract ;
    const _extensions  : String ;
    const _layerType   : TGIS_RegisteredLayerType  ;
    const _formatType  : TGIS_RegisteredFormatType ;
    const _operations  : TGIS_RegisteredOperations ;
    const _priority    : Integer ;
    const _common      : Boolean
  ) ;
  begin
    {$IFDEF OXYGENE}
      __Global.RegisterLayer
                   ( _driverName,
                     _driverInfo,
                     _class,
                     _extensions,
                     _layerType,
                     _formatType,
                     _operations,
                     _priority,
                     _common
                   ) ;
    {$ELSE}
      RegisterLayer( _driverName,
                     _driverInfo,
                     _class,
                     _extensions,
                     _layerType,
                     _formatType,
                     _operations,
                     _priority,
                     _common
                   ) ;
    {$ENDIF}
  end ;


  class procedure TGIS_RegistredLayers.SetDriverPriority(
    const _driverName : String  ;
    const _priority   : Integer
  ) ;
  begin
    {$IFDEF OXYGENE}
      __Global.SetDriverPriority( _driverName, _priority ) ;
    {$ELSE}
      SetDriverPriority( _driverName, _priority ) ;
    {$ENDIF}
  end ;

  function GisCreateLayer( const _name : String ;
                           const _path : String
                         ) : TGIS_Layer ;
  var
    ext     : String   ;
    fname   : String   ;
    path    : String   ;
    drv     : String   ;
    path2   : String   ;
    newPath : String   ;
    i, j    : Integer  ;
    layeri  : TGIS_RegistredLayer ;
    layerj  : TGIS_RegistredLayer ;
    idx     : {$IFDEF JAVA} nullable {$ENDIF} Integer ;
    {$IFDEF CLR}
      atyp  : array of System.Type   ;
      aobj  : array of System.Object ;
    {$ENDIF}

    function _t( const _ext : String ) : Boolean ;
    begin
      Result := CompareText( ext, _ext ) = 0 ;
    end ;

    function _u( const _path : String ) : Boolean ;
    begin
      Result := ( Pos( 'http://' , LowerCase( _path ) ) = StringFirst )
                or
                ( Pos( 'https://', LowerCase( _path ) ) = StringFirst ) ;
    end ;

    function _tkn( const _ext : String ) : Boolean ;
    var
      tkn : TGIS_Tokenizer ;
      t   : Integer ;
    begin
      Result := False;

      tkn := TGIS_Tokenizer.Create;
      try
        tkn.ExecuteEx( _ext, ';' );

        for t := 0 to tkn.Result.Count - 1 do begin
          Result := ( CompareText( ext,   tkn.Result[ t ] ) = 0 ) or
                    ( CompareText( fname, tkn.Result[ t ] ) = 0 ) or
                    ( CompareText( '.*',  tkn.Result[ t ] ) = 0 ) ;
          if Result then break ;
        end ;
      finally
        FreeObject( tkn ) ;
      end ;
    end ;

    {$IFNDEF MONO OR ISLAND}
    function openMapInfoSeamless(
      const _path : String
    )  : TGIS_Layer ;
    var
      ltab      : TGIS_LayerTAB ;
      lp        : TGIS_LayerProject ;
      la        : TGIS_Layer ;
      {$IFDEF DCC}
        shp     : TGIS_Shape ;
      {$ENDIF}
      allpaths  : TStringList ;
      fpath     : String ;
      i         : Integer ;
    begin
      lp := TGIS_LayerProject.Create ;
      lp.Path := _path ;
      Result := lp ;
      fpath := GetFilePath( _path ) ;

      ltab := TGIS_LayerTAB.Create ;
      try
        ltab.Path := _path ;
        ltab.Open ;
        if ltab.FindField( 'Table' ) = -1 then exit ;
        allpaths := TStringList.Create ;
        try
          for shp in ltab.Loop do
            allpaths.Add( fpath + Trim(VarToString(TGIS_Shape(shp).GetField('Table'))) ) ;

          for i := 0 to allpaths.Count-1 do begin
            fpath := allpaths[i] ;
            if SafeFileExists( fpath ) then begin
              la := GisCreateLayer( fpath, fpath ) ;
              if assigned( la ) then begin
                try
                  lp.Add( la ) ;
                  if i = 0 then
                    lp.CS := la.CS ;
                except
                  on e : Exception do begin
                    FreeObject( la ) ;
                    continue ;
                  end ;
                end ;
              end ;
            end ;
          end ;
          lp.RecalcProjectedExtent ;
        finally
          FreeObject( allpaths ) ;
        end ;
      finally
        FreeObject( ltab ) ;
      end ;
    end ;
    {$ENDIF}

    function parsePath( const _path : String ; var _drive : String ) : String ;
    var
      k : Integer ;
    begin
      k := Pos( '?', _path ) ;
      if k < StringFirst then begin
        Result := _path ;
        _drive := '' ;
      end
      else begin
        Result := Copy( _path, StringFirst, k - StringFirst ) ;
        _drive := Copy( _path, k+1, MaxInt ) ;
      end ;
    end ;

  begin
    {$IFDEF OXYGENE}
      // VCL do this upon initialization
      // OXYGENE must call directly
      SelfRegisterLayers ;
      {$IFDEF CLR}
      atyp := new System.Type[0] ;
      aobj := new System.Object[0] ;
      {$ENDIF}
    {$ENDIF}

    Result := nil ;

    if not IsServerPath( _path ) then begin
      path := parsePath( _path, drv ) ;
    end
    else if IsEmbeddedSQLPath( _path ) then
      path := parsePath( _path, drv )
    else
      path := _path ;

    ext := GetFileExt( path ) ;
    if IsStringEmpty( ext ) then
      fname := GetFileName( path )
    else
      fname := '' ;

    // forced driver
    if not IsStringEmpty( drv ) then begin
      for i := 0 to lstRegisteredLayers.Count - 1 do begin
        layeri := TGIS_RegistredLayer(lstRegisteredLayers[i]) ;
        if CompareText( layeri.DriverName, drv ) = 0 then begin
          {$IFDEF CLR}
            if assigned( layeri.LayerClass ) then begin
              {$IFDEF OXYGENE}
                Result := layeri.LayerClass.new ;
              {$ELSE}
                Result := layeri.LayerClass.Create ;
              {$ENDIF}
            end
            else begin
              Result := TGIS_Layer( layeri.LayerClassEx.GetConstructor( atyp ).Invoke( aobj ) ) ;
            end ;
          {$ELSE}
            Result := layeri.LayerClass.Create ;
          {$ENDIF}

          if not assigned( Result ) then continue ;

          if not Result.PreRecognize( path, newPath ) then
            FreeObject( Result )
          else
            break ;
        end ;
      end ;

      if assigned( Result ) then begin
        Result.Path := GetPathAbsolute( '', _path ) ;
        Result.Name := _name  ;
        exit ;
      end
      else // driver not found, use full path
        path := _path ;
    end ;

    if lstCommonLayers.TryGetValue( ext, idx ) then begin
      layeri := TGIS_RegistredLayer(lstRegisteredLayers[idx]) ;
      {$IFDEF CLR}
        if assigned( layeri.LayerClass ) then begin
          {$IFDEF OXYGENE}
            Result := layeri.LayerClass.new ;
          {$ELSE}
            Result := layeri.LayerClass.Create ;
          {$ENDIF}
        end
        else
          Result := TGIS_Layer( layeri.LayerClassEx.GetConstructor( atyp ).Invoke( aobj ) ) ;
      {$ELSE}
        Result := layeri.LayerClass.Create ;
      {$ENDIF}

      if assigned( Result ) then begin
        if not Result.PreRecognize( path, newPath ) then
          FreeObject( Result )
        else begin
          Result.Path := GetPathAbsolute( '', path ) ;
          Result.Name := _name  ;
          exit ;
        end ;
      end ;
    end ;

    for i := 0 to lstRegisteredLayers.Count - 1 do begin
      layeri := TGIS_RegistredLayer(lstRegisteredLayers[i]) ;
      if _tkn( layeri.Extensions ) then begin

        try
          {$IFDEF CLR}
             if assigned( layeri.LayerClass ) then
               Result := layeri.LayerClass.new
             else
               Result := TGIS_Layer( layeri.LayerClassEx.GetConstructor( atyp ).Invoke( aobj ) ) ;
          {$ELSE}
            Result := layeri.LayerClass.Create ;
          {$ENDIF}
        except
          on ex : Exception do begin
            TGIS_Logger.AsWarning( ex.Message, '' ) ;
            Result := nil ;
          end ;
        end ;

        if not assigned( Result ) then continue ;

        if not Result.PreRecognize( path, newPath ) then begin
          FreeObject( Result ) ;

          // because TAB can have embedded raster file
          if _t( '.tab' ) and ( not IsStringEmpty( newPath ) ) then begin
            if path <> newPath then begin
              // case for embedded raster file
              path2  := path ;
              path   := newPath ;
              ext    := GetFileExt( path ) ;

              for j := 0 to lstRegisteredLayers.Count - 1 do begin
                layerj := TGIS_RegistredLayer(lstRegisteredLayers[j]) ;
                if _tkn( layerj.Extensions ) then begin
                  {$IFDEF CLR}
                    if assigned( layerj.LayerClass ) then begin
                      {$IFDEF OXYGENE}
                        Result := layerj.LayerClass.new ;
                      {$ELSE}
                        Result := layerj.LayerClass.Create ;
                      {$ENDIF}
                    end
                    else begin
                      Result := TGIS_Layer( layerj.LayerClassEx.GetConstructor( atyp ).Invoke( aobj ) ) ;
                    end ;
                  {$ELSE}
                    Result := layerj.LayerClass.Create ;
                  {$ENDIF}

                  if not Result.PreRecognize( path, newPath ) then
                    FreeObject( Result )
                  else begin
                    TGIS_LayerPixel( Result ).PathTAB := path2 ;
                    break ;
                  end ;
                end ;
              end ;
              if assigned( Result ) then break ;
            end
            else begin
              // case for seamless tab
              {$IFNDEF MONO OR ISLAND}
              Result := openMapInfoSeamless( newPath ) ;
              if assigned( Result ) then break ;
              {$ENDIF}
            end;
          end ;
        end
        else begin
          if not lstCommonLayers.ContainsKey( ext ) then
            lstCommonLayers.AddOrSetValue( ext, i ) ;
          break ;
        end ;
      end
    end ;

    if not assigned( Result ) then begin
      if _u( _path  ) then begin
        {$IFDEF MONO OR ISLAND}
          {$WARNING '### Verify MONO code'}
          {$WARNING '### Verify ISLAND code'}
        {$ELSE}
          Result := TGIS_LayerWMS.Create ;
        {$ENDIF}
        if not Result.PreRecognize( _path, newPath ) then
          FreeObject( Result ) ;

        if not assigned( Result ) then begin
          {$IFDEF MONO OR ISLAND}
            {$WARNING '### Verify MONO code'}
            {$WARNING '### Verify ISLAND code'}
          {$ELSE}
            Result := TGIS_LayerWFS.Create ;
          {$ENDIF}
          if not Result.PreRecognize( _path, newPath ) then
            FreeObject( Result ) ;
        end ;

        if not assigned( Result ) then begin
          {$IFDEF MONO OR ISLAND}
            {$WARNING '### Verify MONO code'}
            {$WARNING '### Verify ISLAND code'}
          {$ELSE}
            Result := TGIS_LayerWCS.Create ;
          {$ENDIF}
          if not Result.PreRecognize( _path, newPath ) then
            FreeObject( Result ) ;
        end ;

        if not assigned( Result ) then begin
          {$IFDEF MONO OR ISLAND}
            {$WARNING '### Verify MONO code'}
            {$WARNING '### Verify ISLAND code'}
          {$ELSE}
            Result := TGIS_LayerWMTS.Create ;
          {$ENDIF}
          if not Result.PreRecognize( _path, newPath ) then
            FreeObject( Result ) ;
        end ;

        if not assigned( Result ) then begin
          {$IFDEF MONO OR ISLAND}
            {$WARNING '### Verify MONO code'}
            {$WARNING '### Verify ISLAND code'}
          {$ELSE}
            Result := TGIS_LayerJSON.Create ;
          {$ENDIF}
          if not Result.PreRecognize( _path, newPath ) then
            FreeObject( Result ) ;
        end ;

        // try default anyway
        if not assigned( Result ) then
        {$IFDEF MONO OR ISLAND}
          {$WARNING '### Verify MONO code'}
          {$WARNING '### Verify ISLAND code'}
        {$ELSE}
          Result := TGIS_LayerWMS.Create ;
        {$ENDIF}
      end
    end ;

    if not assigned( Result ) then exit ;

    Result.Path := GetPathAbsolute( '', path ) ;
    Result.Name := _name  ;
  end ;

  function GisSupportedFiles(
    const _mode : TGIS_FileTypes ;
    const _save : Boolean
  ) : String ;
  begin
    Result := GisSupportedFiles( _mode, _save, False ) ;
  end ;

  function GisSupportedFiles(
    const _mode        : TGIS_FileTypes ;
    const _save        : Boolean        ;
    const _fileformats : Boolean
  ) : String ;
  var
    lst_files   : TGIS_StringList ;
    lst_names   : TGIS_StringList ;
    lst_unique  : TGIS_StringList ;
    lst_all     : TGIS_StringList ;
    lst_common  : TGIS_StringList ;
    lst_project : TGIS_StringList ;
    lst_vector  : TGIS_StringList ;
    lst_vector3D: TGIS_StringList ;
    lst_pixel   : TGIS_StringList ;
    lst_grid    : TGIS_StringList ;
    tmp         : String      ;
    i           : Integer     ;
    rl          : TGIS_RegistredLayer ;
    stmplt_shrt : String      ;
    stmplt_lng  : String      ;
    alias       : String      ;

    procedure add_type( const _mode   : TGIS_FileType ;
                        const _name   : String        ;
                        const _ext    : String        ;
                        const _common : String
                      ) ;
    var
      i1  : Integer        ;
      tkn : TGIS_Tokenizer ;

      procedure concat_ext( const _dest : TGIS_StringList ;
                            const _ext  : String
                          ) ;
      var
        i2   : Integer ;
        ext2 : String ;
      begin
        ext2 := _ext ;
        if ext2 <> '*.*' then begin
          if not _dest.Find( ext2, i2 ) then
            _dest.Add( ext2 )
        end ;
      end ;

    begin
      lst_files.Add ( Format( stmplt_lng,
                              [ _name, _ext, _ext ]
                            )
                    ) ;
      tkn := TGIS_Tokenizer.Create ;
      try
        tkn.Execute( _ext, [';'] ) ;
        for i1:=0 to tkn.Result.Count -1 do begin
          lst_names.Add( Format( '%s=%s',
                                 [ Copy( tkn.Result[i1], StringFirst + 1, 255 ),
                                   _name ]
                               )
                       ) ;
          case _mode of
            TGIS_FileType.Project : concat_ext( lst_project, tkn.Result[i1] ) ;
            TGIS_FileType.Vector  : concat_ext( lst_vector , tkn.Result[i1] ) ;
            TGIS_FileType.Vector3D: concat_ext( lst_vector3D,tkn.Result[i1] ) ;
            TGIS_FileType.Pixel   : concat_ext( lst_pixel  , tkn.Result[i1] ) ;
            TGIS_FileType.Grid    : concat_ext( lst_grid   , tkn.Result[i1] ) ;
            else                    begin
                                      assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
                                    end ;
          end ;
        end ;
        concat_ext( lst_all, _ext ) ;
      finally
        FreeObject( tkn ) ;
      end ;

      tkn := TGIS_Tokenizer.Create ;
      try
        tkn.Execute( _common, [';'] ) ;
        for i1:=0 to tkn.Result.Count -1 do begin
          if Pos( '*.ttk', tkn.Result[i1] ) = StringFirst then
            concat_ext( lst_common , '*.ttk*' )
          else
            concat_ext( lst_common , tkn.Result[i1] ) ;
        end ;
      finally
        FreeObject( tkn ) ;
      end ;

    end ;

    function makeFilter( const _ext     : String ;
                         const _common  : Boolean
                       ) : String ;
    var
      tkn : TGIS_Tokenizer ;
      t   : Integer ;
    begin
      Result := '';

      if not _common then exit ;

      tkn := TGIS_Tokenizer.Create;
      try
        tkn.ExecuteEx( _ext, ';' );

        for t := 0 to tkn.Result.Count - 1 do begin
          if IsStringEmpty( tkn.Result[ t ] ) then continue ;

          if tkn.Result[ t ][StringFirst] = '.' then
            Result := Result + '*' + tkn.Result[ t ]
          else
            Result := Result + tkn.Result[ t ] ;

          if ( t < tkn.Result.Count - 1 ) then
            Result := Result + ';'
        end ;
      finally
        FreeObject( tkn ) ;
      end ;
    end ;

    procedure collectDrivers( const _layerType  : TGIS_RegisteredLayerType     ;
                              const _operation  : TGIS_RegisteredOperationType ;
                              const _mode       : TGIS_FileType
                            ) ;
    var
      i1 : Integer ;
      b1 : Boolean ;
      b2 : Boolean ;
    begin
      lst_unique.Clear;
      for i1 := 0 to lstRegisteredLayers.Count - 1 do begin
        rl := TGIS_RegistredLayer( lstRegisteredLayers[ i1 ] ) ;

        b1 := ( _layerType = rl.LayerType ) ;
        b2 := ( _operation in rl.Operations ) ;
        if b1 and b2 then
          if lst_unique.IndexOf( rl.DriverName ) < 0 then begin
            add_type( _mode, rl.DriverInfo,
                      makeFilter( rl.Extensions, True ),
                      makeFilter( rl.Extensions, rl.Common )
                    ) ;
            lst_unique.Add( rl.DriverName ) ;
          end ;
      end ;

    end ;

  begin
    {$IFDEF OXYGENE}
      SelfRegisterLayers ;
    {$ENDIF}

    {$IFDEF JAVA}
      stmplt_shrt := '%s-%s_' ;
      stmplt_lng  := '%s (%s)-%s_' ;
    {$ELSE}
      stmplt_shrt := '%s|%s|' ;
      stmplt_lng  := '%s (%s)|%s|' ;
    {$ENDIF}



    lst_project := TGIS_StringList.Create ;
    lst_project.Sorted := True ;
    lst_project.Delimiter := ';' ;
    lst_project.StrictDelimiter := True ;

    lst_vector  := TGIS_StringList.Create ;
    lst_vector.Sorted := True ;
    lst_vector.Delimiter := ';' ;
    lst_vector.StrictDelimiter := True ;

    lst_vector3D  := TGIS_StringList.Create ;
    lst_vector3D.Sorted := True ;
    lst_vector3D.Delimiter := ';' ;
    lst_vector3D.StrictDelimiter := True ;

    lst_pixel   := TGIS_StringList.Create ;
    lst_pixel.Sorted := True ;
    lst_pixel.Delimiter := ';' ;
    lst_pixel.StrictDelimiter := True ;

    lst_grid    := TGIS_StringList.Create ;
    lst_grid.Sorted := True ;
    lst_grid.Delimiter := ';' ;
    lst_grid.StrictDelimiter := True ;

    lst_common  := TGIS_StringList.Create ;
    lst_common.Sorted := True ;
    lst_common.Delimiter := ';' ;
    lst_common.StrictDelimiter := True ;

    lst_all     := TGIS_StringList.Create ;
    lst_all.Sorted := True ;
    lst_all.Delimiter := ';' ;
    lst_all.StrictDelimiter := True ;

    lst_files   := TGIS_StringList.Create ;
    lst_names   := TGIS_StringList.Create ;
    lst_unique  := TGIS_StringList.Create ;

    lst_files.Sorted := True ;
    try
      if not _save then begin

        if GisTestFileType( TGIS_FileType.All, _mode ) or
           GisTestFileType( TGIS_FileType.Project, _mode ) then
        begin
          add_type( TGIS_FileType.Project,
                    'MapInfo WorkFile',
                    '*.wor',
                    ''
                  ) ;
          add_type( TGIS_FileType.Project,
                    'ArcExplorer Project File',
                    '*.aep',
                    ''
                  ) ;
          add_type( TGIS_FileType.Project,
                    'ArcView 3.xx Project File',
                    '*.apr',
                    ''
                  ) ;
          add_type( TGIS_FileType.Project,
                    'MapInfoX Project File',
                    '*.gst',
                    ''
                  ) ;
          add_type( TGIS_FileType.Project,
                    'QGIS Project File',
                    '*.qgs;*.qgz',
                    '*.qgs;*.qgz'
                  ) ;
          add_type( TGIS_FileType.Project,
                    GIS_TTKGP_NAME,
                    '*'+GIS_TTKPROJECT_EXT + ';*'+GIS_TTKGP_EXT,
                    '*'+GIS_TTKPROJECT_EXT + ';*'+GIS_TTKGP_EXT
                  ) ;
          alias := GisMetadataAsString( METADATA_CONFIGFACTORY_ALIASES_TTKPROJECT, '' ) ;
          if not IsStringEmpty( alias ) then
            add_type( TGIS_FileType.Project,
                      GIS_TTKGP_NAME,
                      '*' + alias,
                      ''
                    ) ;
        end ;

        if GisTestFileType( TGIS_FileType.All, _mode ) or
           GisTestFileType( TGIS_FileType.Vector, _mode ) then
        begin
          // vector files
          collectDrivers( TGIS_RegisteredLayerType.Vector,
                          TGIS_RegisteredOperationType.Read,
                          TGIS_FileType.Vector
                        ) ;
        end ;

        if GisTestFileType( TGIS_FileType.All, _mode ) or
           GisTestFileType( TGIS_FileType.Vector3D, _mode ) then
        begin
          // vector files
          collectDrivers( TGIS_RegisteredLayerType.Vector3D,
                          TGIS_RegisteredOperationType.Read,
                          TGIS_FileType.Vector3D
                        ) ;
        end ;

        if GisTestFileType( TGIS_FileType.All, _mode ) or
           GisTestFileType( TGIS_FileType.Grid, _mode ) then
        begin
          // grid file
          collectDrivers( TGIS_RegisteredLayerType.Grid,
                          TGIS_RegisteredOperationType.Read,
                          TGIS_FileType.Grid
                        ) ;
        end ;

        if GisTestFileType( TGIS_FileType.All, _mode ) or
           GisTestFileType( TGIS_FileType.Pixel, _mode ) then
        begin
          // pixel file
          collectDrivers( TGIS_RegisteredLayerType.Pixel,
                          TGIS_RegisteredOperationType.Read,
                          TGIS_FileType.Pixel
                        ) ;
        end ;

        lst_files.Sorted := False ;

        // all files
        {$IFDEF JAVA}
          //Java has built in all files
        {$ELSE}
        lst_files.Insert( 0, Format( stmplt_lng,
                                     [ _rsrc( GIS_RS_GENERAL_ALLFILES ),
                                       '*.*',
                                       '*.*'
                                     ]
                                   )
                        ) ;
        {$ENDIF}

        if GisTestFileType( TGIS_FileType.All, _mode ) or
           GisTestFileType( TGIS_FileType.Pixel, _mode ) then
        begin
          // pixel file
          assert( length( lst_pixel.DelimitedText ) < 250 ) ;
          lst_files.Insert( 0, Format( stmplt_shrt,
                                       [ _rsrc( GIS_RS_GENERAL_ALLPIXEL ),
                                         lst_pixel.DelimitedText
                                       ]
                                     )
                          ) ;
        end ;

        if GisTestFileType( TGIS_FileType.All, _mode ) or
           GisTestFileType( TGIS_FileType.Grid, _mode ) then
        begin
          // grid file
          assert( length( lst_grid.DelimitedText ) < 250 ) ;
          lst_files.Insert( 0, Format( stmplt_shrt,
                                       [ _rsrc( GIS_RS_GENERAL_ALLGRID ),
                                         lst_grid.DelimitedText
                                       ]
                                     )
                          ) ;
        end ;

        if GisTestFileType( TGIS_FileType.All, _mode ) or
           GisTestFileType( TGIS_FileType.Vector, _mode ) then
        begin
          // vector file
          assert( length( lst_vector.DelimitedText ) < 250 ) ;
          lst_files.Insert( 0, Format( stmplt_shrt,
                                       [ _rsrc( GIS_RS_GENERAL_ALLVECTOR ),
                                         lst_vector.DelimitedText
                                       ]
                                     )
                          ) ;
        end ;

        if GisTestFileType( TGIS_FileType.All, _mode ) or
           GisTestFileType( TGIS_FileType.Vector3D, _mode ) then
        begin
          // vector file
          assert( length( lst_vector3D.DelimitedText ) < 250 ) ;
          lst_files.Insert( 0, Format( stmplt_shrt,
                                       [ _rsrc( GIS_RS_GENERAL_ALLVECTOR3D ),
                                         lst_vector3D.DelimitedText
                                       ]
                                     )
                          ) ;
        end ;

        if GisTestFileType( TGIS_FileType.All, _mode ) or
           GisTestFileType( TGIS_FileType.Project, _mode ) then
        begin
          // project files
          assert( length( lst_project.DelimitedText ) < 250 ) ;
          lst_files.Insert( 0, Format( stmplt_shrt,
                                       [ _rsrc( GIS_RS_GENERAL_ALLPROJECT ),
                                         lst_project.DelimitedText
                                       ]
                                     )
                          ) ;
        end ;

        // common files
        assert( length( lst_common.DelimitedText ) < 250 ) ;
        lst_files.Insert( 0, Format( stmplt_shrt,
                                    [ _rsrc( GIS_RS_GENERAL_COMMONFILES ),
                                      lst_common.DelimitedText
                                    ]
                                  )
                        ) ;

      end
      else begin

        if GisTestFileType( TGIS_FileType.All, _mode ) or
           GisTestFileType( TGIS_FileType.Project, _mode ) then
        begin
          if  GisTestFileType( TGIS_FileType.Project, _mode ) then
            lst_files.Sorted := False ; //force order!

          // project files
          add_type( TGIS_FileType.Project,
                    GIS_TTKGP_NAME,
                    '*'+GIS_TTKPROJECT_EXT,
                    ''
                  ) ;
          // project files
          add_type( TGIS_FileType.Project,
                    GIS_TTKGP_NAME,
                    '*'+GIS_TTKGP_EXT,
                    ''
                  ) ;
        end ;

        if GisTestFileType( TGIS_FileType.All, _mode ) or
           GisTestFileType( TGIS_FileType.Vector, _mode ) then
        begin
          // vector files
          collectDrivers( TGIS_RegisteredLayerType.Vector,
                          TGIS_RegisteredOperationType.Write,
                          TGIS_FileType.Vector
                        ) ;
        end ;

        if GisTestFileType( TGIS_FileType.All, _mode ) or
           GisTestFileType( TGIS_FileType.Grid, _mode ) then
        begin
          // grid file
          collectDrivers(TGIS_RegisteredLayerType.Grid,
                          TGIS_RegisteredOperationType.Write,
                          TGIS_FileType.Grid
                        ) ;
        end ;

        if GisTestFileType( TGIS_FileType.All, _mode ) or
           GisTestFileType( TGIS_FileType.Pixel, _mode ) then
        begin
          // pixel file
          collectDrivers( TGIS_RegisteredLayerType.Pixel,
                          TGIS_RegisteredOperationType.Write,
                          TGIS_FileType.Pixel
                        ) ;
        end ;

        lst_files.Sorted := False ;

      end ;
    finally
      if _fileformats then
        Result := lst_names.Text
      else begin
        tmp := lst_files.Text ;

        Result := '' ;
        for i:=StringFirst to length(tmp)-1 do begin
          case tmp[i] of
            #13 ,
            #10 : continue ;
            else Result := Result + tmp[i] ;
          end ;
        end ;
      end ;
      {$IFDEF OXYGENE}
        // delete the last vertical bar
        if not IsStringEmpty( Result ) then
        begin
          {$IFDEF JAVA}
            Result := Result.Substring( 0, Result.Length-1 ) ;
          {$ELSE}
            Result := Result.Remove( Result.Length-1, 1 ) ;
          {$ENDIF}
        end ;
      {$ENDIF}
      {$IFDEF GIS_XDK}
        Result := Result.Remove( Result.Length-1, 1 ) ;
      {$ENDIF}

      FreeObject( lst_files   ) ;
      FreeObject( lst_names   ) ;
      FreeObject( lst_unique  ) ;
      FreeObject( lst_all     ) ;
      FreeObject( lst_common  ) ;
      FreeObject( lst_project ) ;
      FreeObject( lst_vector  ) ;
      FreeObject( lst_vector3D) ;
      FreeObject( lst_pixel   ) ;
      FreeObject( lst_grid    ) ;
    end ;

  end ;

  {$IFDEF DCC}
    function SortList( const Item1, Item2 : TGIS_RegistredLayer ) : Integer ;
  {$ELSE}
    function SortList( const Item1, Item2 : TObject ) : Integer ;
  {$ENDIF}
  begin
    if TGIS_RegistredLayer( Item1 ).Priority <
       TGIS_RegistredLayer( Item2 ).Priority then
      Result := -1
    else if TGIS_RegistredLayer( Item1 ).Priority >
            TGIS_RegistredLayer( Item2 ).Priority then
      Result := 1
    else begin
      if TGIS_RegistredLayer( Item1 ).Common and not
         TGIS_RegistredLayer( Item2 ).Common then
        Result := -1
      else if not TGIS_RegistredLayer( Item1 ).Common and
        TGIS_RegistredLayer( Item2 ).Common then
        Result := 1
      else
        Result := CompareText( TGIS_RegistredLayer( Item1 ).Extensions,
                               TGIS_RegistredLayer( Item2 ).Extensions
                              ) ;
    end ;
  end ;

  procedure RegisterLayer( const _driverName  : String ;
                           const _driverInfo  : String ;
                           const _class       : TGIS_RegisteredLayerAbstract ;
                           const _extensions  : String ;
                           const _layerType   : TGIS_RegisteredLayerType ;
                           const _formatType  : TGIS_RegisteredFormatType ;
                           const _operations  : TGIS_RegisteredOperations
                          ) ;
  begin
    RegisterLayer( _driverName, _driverInfo, _class, _extensions,
                   _layerType, _formatType, _operations,
                   GIS_DEFAULT_LAYER_PRIORITY, False ) ;
  end ;

  procedure RegisterLayer( const _driverName  : String ;
                           const _driverInfo  : String ;
                           const _class       : TGIS_RegisteredLayerAbstract ;
                           const _extensions  : String ;
                           const _layerType   : TGIS_RegisteredLayerType ;
                           const _formatType  : TGIS_RegisteredFormatType ;
                           const _operations  : TGIS_RegisteredOperations ;
                           const _common      : Boolean
                         ) ;
  var
    priority : Integer ;
  begin
    if _common then
      priority := GIS_DEFAULT_LAYER_PRIORITY
    else
      priority := GIS_LOWER_LAYER_PRIORITY ;

    RegisterLayer( _driverName, _driverInfo, _class, _extensions,
                   _layerType, _formatType, _operations, priority, _common
                 ) ;
  end ;

  procedure RegisterLayer( const _driverName  : String ;
                           const _driverInfo  : String ;
                           const _class       : TGIS_RegisteredLayerAbstract ;
                           const _extensions  : String ;
                           const _layerType   : TGIS_RegisteredLayerType ;
                           const _formatType  : TGIS_RegisteredFormatType ;
                           const _operations  : TGIS_RegisteredOperations ;
                           const _priority    : Integer ;
                           const _common      : Boolean
                         ) ;
  var
    RegLayer : TGIS_RegistredLayer ;
  begin
    RegLayer := TGIS_RegistredLayer.Create ;

    RegLayer.DriverName     := _driverName  ;
    RegLayer.DriverInfo     := _driverInfo  ;
    RegLayer.LayerClass     := _class       ;
    {$IFDEF CLR}
      RegLayer.LayerClassEx := typeOf( _class ) ;
    {$ENDIF}
    RegLayer.Extensions     := _extensions  ;
    RegLayer.LayerType      := _layerType   ;
    RegLayer.FormatType     := _formatType  ;
    RegLayer.Operations     := _operations  ;
    RegLayer.Priority       := _priority    ;
    RegLayer.Common         := _common      ;

    if not assigned( lstRegisteredLayers ) then
      lstRegisteredLayers := TObjectList<TGIS_RegistredLayer>.Create( True ) ;

    lstRegisteredLayers.Add( RegLayer ) ;
    {$IFNDEF OXYGENE}
      lstRegisteredLayers.Sort( TComparer<TGIS_RegistredLayer>.Construct( SortList )  ) ;
    {$ELSE}
      lstRegisteredLayers.Sort( @SortList ) ;
    {$ENDIF}
    if not assigned( lstCommonLayers ) then
      lstCommonLayers := TDictionary<String, Integer>.Create(
                           {$IFDEF OXYGENE}
                             {$IFDEF JAVA}
                               java.lang.String.CASE_INSENSITIVE_ORDER
                             {$ENDIF}
                             {$IFDEF CLR}
                               StringComparer.OrdinalIgnoreCase
                             {$ENDIF}
                           {$ELSE}
                             TIStringComparer.Ordinal
                           {$ENDIF}
                         );
  end ;

  {$IFDEF CLR}
    procedure RegisterLayer( const _driverName  : String ;
                             const _driverInfo  : String ;
                             const _class       : &Type ;
                             const _extensions  : String ;
                             const _layerType   : TGIS_RegisteredLayerType ;
                             const _formatType  : TGIS_RegisteredFormatType ;
                             const _operations  : TGIS_RegisteredOperations
                           ) ;
    begin
      RegisterLayer( _driverName, _driverInfo, _class, _extensions,
                     _layerType, _formatType, _operations,
                     GIS_DEFAULT_LAYER_PRIORITY, False ) ;
    end ;

    procedure RegisterLayer( const _driverName  : String ;
                             const _driverInfo  : String ;
                             const _class       : &Type ;
                             const _extensions  : String ;
                             const _layerType   : TGIS_RegisteredLayerType ;
                             const _formatType  : TGIS_RegisteredFormatType ;
                             const _operations  : TGIS_RegisteredOperations ;
                             const _common      : Boolean
                           ) ;
    begin
      RegisterLayer( _driverName, _driverInfo, _class, _extensions,
                     _layerType, _formatType, _operations,
                     GIS_DEFAULT_LAYER_PRIORITY, _common ) ;
    end ;

    procedure RegisterLayer( const _driverName  : String ;
                             const _driverInfo  : String ;
                             const _class       : &Type ;
                             const _extensions  : String ;
                             const _layerType   : TGIS_RegisteredLayerType ;
                             const _formatType  : TGIS_RegisteredFormatType ;
                             const _operations  : TGIS_RegisteredOperations ;
                             const _priority    : Integer ;
                             const _common      : Boolean
                           ) ;
    var
      RegLayer : TGIS_RegistredLayer ;
    begin
      {$IFDEF OXYGENE}
        SelfRegisterLayers ;
      {$ENDIF}

      RegLayer := TGIS_RegistredLayer.Create ;

      RegLayer.DriverName     := _driverName   ;
      RegLayer.DriverInfo     := _driverInfo   ;
      RegLayer.LayerClass     := nil           ;
      RegLayer.LayerClassEx   := _class        ;
      RegLayer.Extensions     := _extensions   ;
      RegLayer.LayerType      := _layerType    ;
      RegLayer.FormatType     := _formatType   ;
      RegLayer.Operations     := _operations   ;
      RegLayer.Priority       := _priority     ;
      RegLayer.Common         := _common       ;

      if not assigned( lstRegisteredLayers ) then
        lstRegisteredLayers := TObjectList<TGIS_RegistredLayer>.Create( True ) ;

      lstRegisteredLayers.Add( RegLayer ) ;
      {$IFNDEF OXYGENE}
        lstRegisteredLayers.Sort(  SortList ) ;
      {$ELSE}
        lstRegisteredLayers.Sort( @SortList ) ;
      {$ENDIF}
      if not assigned( lstCommonLayers ) then
        lstCommonLayers := TDictionary<String, Integer>.Create(
                             {$IFDEF OXYGENE}
                               {$IFDEF JAVA}
                                 java.lang.String.CASE_INSENSITIVE_ORDER
                               {$ELSE}
                                 StringComparer.OrdinalIgnoreCase
                               {$ENDIF}
                             {$ELSE}
                               TIStringComparer.Ordinal
                             {$ENDIF}
                           );
    end ;

    procedure RegisterLayer( const _driverName  : String ;
                             const _driverInfo  : String ;
                             const _class       : &Type ;
                             const _extensions  : String ;
                             const _layerType   : TGIS_RegisteredLayerType ;
                             const _formatType  : TGIS_RegisteredFormatType ;
                             const _operations  : TGIS_RegisteredOperationType
                           ) ;
    begin
      RegisterLayer( _driverName, _driverInfo, _class, _extensions,
                     _layerType, _formatType, _operations,
                     GIS_DEFAULT_LAYER_PRIORITY, False ) ;
    end ;

    procedure RegisterLayer( const _driverName  : String ;
                             const _driverInfo  : String ;
                             const _class       : &Type ;
                             const _extensions  : String ;
                             const _layerType   : TGIS_RegisteredLayerType ;
                             const _formatType  : TGIS_RegisteredFormatType ;
                             const _operations  : TGIS_RegisteredOperationType ;
                             const _common      : Boolean
                           ) ;
    begin
      RegisterLayer( _driverName, _driverInfo, _class, _extensions,
                     _layerType, _formatType, _operations,
                     GIS_DEFAULT_LAYER_PRIORITY, _common ) ;
    end ;

    procedure RegisterLayer( const _driverName  : String ;
                             const _driverInfo  : String ;
                             const _class       : &Type ;
                             const _extensions  : String ;
                             const _layerType   : TGIS_RegisteredLayerType ;
                             const _formatType  : TGIS_RegisteredFormatType ;
                             const _operations  : TGIS_RegisteredOperationType ;
                             const _priority    : Integer ;
                             const _common      : Boolean
                           ) ;
    var
      RegLayer : TGIS_RegistredLayer ;

      function type_to_operations : TGIS_RegisteredOperations ;
      begin
        {$IFDEF JAVA}
          {$WARNING '### Verify JAVA code'}
        {$ELSE}
        case Integer( _operations ) of
          // one operation
          1  : Result := [ TGIS_RegisteredOperationType.Read    ] ;
          2  : Result := [ TGIS_RegisteredOperationType.Write   ] ;
          4  : Result := [ TGIS_RegisteredOperationType.&Create ] ;
          8  : Result := [ TGIS_RegisteredOperationType.Merge   ] ;
          // two operations
          3  : Result := [ TGIS_RegisteredOperationType.Read  ,
                           TGIS_RegisteredOperationType.Write   ] ;
          5  : Result := [ TGIS_RegisteredOperationType.Read  ,
                           TGIS_RegisteredOperationType.&Create ] ;
          6  : Result := [ TGIS_RegisteredOperationType.Write ,
                           TGIS_RegisteredOperationType.&Create ] ;
          9  : Result := [ TGIS_RegisteredOperationType.Read  ,
                           TGIS_RegisteredOperationType.Merge   ] ;
          10 : Result := [ TGIS_RegisteredOperationType.Write ,
                           TGIS_RegisteredOperationType.Merge   ] ;
          12 : Result := [ TGIS_RegisteredOperationType.&Create,
                           TGIS_RegisteredOperationType.Merge   ] ;
          // three operations
          7  : Result := [ TGIS_RegisteredOperationType.Read  ,
                           TGIS_RegisteredOperationType.Write ,
                           TGIS_RegisteredOperationType.&Create ] ;
          11 : Result := [ TGIS_RegisteredOperationType.Read  ,
                           TGIS_RegisteredOperationType.Write ,
                           TGIS_RegisteredOperationType.Merge   ] ;
          13 : Result := [ TGIS_RegisteredOperationType.Read  ,
                           TGIS_RegisteredOperationType.&Create,
                           TGIS_RegisteredOperationType.Merge   ] ;
          14 : Result := [ TGIS_RegisteredOperationType.Write ,
                           TGIS_RegisteredOperationType.&Create,
                           TGIS_RegisteredOperationType.Merge   ] ;
          // four operations
          15 : Result := [ TGIS_RegisteredOperationType.Read  ,
                           TGIS_RegisteredOperationType.Write ,
                           TGIS_RegisteredOperationType.&Create,
                           TGIS_RegisteredOperationType.Merge   ] ;
        end ;
        {$ENDIF}
      end ;

    begin
      {$IFDEF OXYGENE}
        SelfRegisterLayers ;
      {$ENDIF}

      RegLayer := TGIS_RegistredLayer.Create ;

      RegLayer.DriverName     := _driverName        ;
      RegLayer.DriverInfo     := _driverInfo        ;
      RegLayer.LayerClass     := nil                ;
      RegLayer.LayerClassEx   := _class             ;
      RegLayer.Extensions     := _extensions        ;
      RegLayer.LayerType      := _layerType         ;
      RegLayer.FormatType     := _formatType        ;
      RegLayer.Operations     := type_to_operations ;
      RegLayer.Priority       := _priority          ;
      RegLayer.Common         := _common            ;

      if not assigned( lstRegisteredLayers ) then
        lstRegisteredLayers := TObjectList<TGIS_RegistredLayer>.Create( True ) ;

      lstRegisteredLayers.Add( RegLayer ) ;
      {$IFNDEF OXYGENE}
        lstRegisteredLayers.Sort(  SortList ) ;
      {$ELSE}
        lstRegisteredLayers.Sort( @SortList ) ;
      {$ENDIF}
      if not assigned( lstCommonLayers ) then
        lstCommonLayers := TDictionary<String, Integer>.Create(
                             {$IFDEF OXYGENE}
                               {$IFDEF JAVA}
                                 java.lang.String.CASE_INSENSITIVE_ORDER
                               {$ELSE}
                                 StringComparer.OrdinalIgnoreCase
                               {$ENDIF}
                             {$ELSE}
                               TIStringComparer.Ordinal
                             {$ENDIF}
                           );
    end ;

  {$ENDIF}

  {$IFDEF OXYGENE}

    procedure SelfRegisterLayers ;
    var
      {$IFDEF CLR}
        asmb : System.Reflection.Assembly ;
        atyp : Array of System.Type ;
        aobj : Array of Object ;
        mi   : MethodInfo ;
      {$ENDIF}
      {$IFDEF JAVA}
        o : Object ;
      {$ENDIF}
      {$IFDEF ISLAND}
        o : Object ;
      {$ENDIF}
    begin
      if wasSelfRegisterLayers then exit ;
      try
        if assigned( thcSelfRegisterLayers ) then
          thcSelfRegisterLayers.LockThread ;
        try
          {$IFDEF CLR}
            {$IFDEF DOTNET_STANDARD}
              // crucial for .NET Core to provide proper layer encodings
              System.Text.Encoding.RegisterProvider(System.Text.CodePagesEncodingProvider.Instance);
              System.Data.Common.DbProviderFactories.RegisterFactory(
                "System.Data.SqlClient",
                "System.Data.SqlClient.SqlClientFactory, System.Data.SqlClient"
              );
            {$ENDIF}

            asmb := System.Reflection.Assembly.GetExecutingAssembly ;
            aobj := nil ;
            try
              atyp := asmb.GetTypes ;
            except
              on E : ReflectionTypeLoadException do
                atyp := E.Types ;
            end ;
            for t in atyp do begin
              if not assigned( t ) then
                continue ;
              try
                mi := t.GetMethod( "SelfRegisterLayer" ) ;
              except
                continue ;
              end ;
              if ( mi <> nil ) then
                mi.Invoke( nil, aobj ) ;
            end ;
          {$ENDIF}
          {$IFDEF JAVA}
            {$IFDEF ANDROID}
              var df : dalvik.system.DexFile := new dalvik.system.DexFile(GisPackageCodePath);
              var classFileNames := df.entries();
              while (classFileNames.hasMoreElements()) do begin
                var cname : String := classFileNames.nextElement();
                if cname.startsWith('tatukgis.jdk.Unit_') then begin
                  var c := java.lang.Thread.currentThread.ContextClassLoader.loadClass(cname) ;

                  for omth in c.DeclaredMethods do begin
                    if omth.Name = 'SelfRegisterLayer' then
                      omth.invoke( o, [] ) ;
                  end;
               end ;
              end ;
            {$ELSE}
              var cp : String := java.net.URLDecoder.decode(typeOf(TGIS_RegistredLayers).getProtectionDomain().getCodeSource().getLocation().getPath(), 'UTF-8');
              if assigned( cp ) then begin
                var jar: java.io.File := new java.io.File(cp);
                var &is: java.util.jar.JarInputStream := new java.util.jar.JarInputStream(new java.io.FileInputStream(jar));
                var entry: java.util.jar.JarEntry;
                entry := &is.NextJarEntry ;
                while (entry <> nil) do begin
                  if entry.Name.endsWith('.class') then begin
                    var cname := entry.getName().replaceAll('/','.').replace('.class','') ;
                    if cname.startsWith('tatukgis.jdk.Unit_') then begin
                      var c := java.lang.Thread.currentThread().ContextClassLoader.loadClass(cname) ;

                      for omth in c.DeclaredMethods do begin
                        if omth.Name = 'SelfRegisterLayer' then
                          omth.invoke( o, [] ) ;
                      end;
                    end ;
                  end;
                  entry := &is.getNextJarEntry() ;
                end;
              end ;
            {$ENDIF}
          {$ENDIF}
          {$IFDEF ISLAND}
            {$WARNING '### Verify ISLAND code'}
          {$ENDIF}
          wasSelfRegisterLayers := True ;
          _rsrc('') ;
        finally
          if assigned( thcSelfRegisterLayers ) then
            thcSelfRegisterLayers.UnlockThread ;
        end;
      finally
        FreeObject( thcSelfRegisterLayers );
      end;
    end ;

  {$ENDIF}

  // Free registered drivers.
  procedure FreeRegisteredLayers;
  begin
    if assigned( lstRegisteredLayers ) then begin
      lstRegisteredLayers.Clear ;

      FreeObject( lstRegisteredLayers ) ;
    end ;
    if assigned( lstCommonLayers ) then
      FreeObject( lstCommonLayers ) ;
  end ;

  procedure SetDriverPriority( const _driverName  : String ;
                               const _priority    : Integer
                             ) ;
  var
    i         : Integer ;
    RegLayer  : TGIS_RegistredLayer ;
  begin
    if assigned( lstRegisteredLayers ) then begin
      for i := 0 to lstRegisteredLayers.Count - 1 do begin
        RegLayer := TGIS_RegistredLayer( lstRegisteredLayers[ i ] ) ;
        if assigned( RegLayer ) then
          if RegLayer.DriverName = _driverName then
            RegLayer.Priority := _priority ;
      end ;
      {$IFNDEF OXYGENE}
        lstRegisteredLayers.Sort( TComparer<TGIS_RegistredLayer>.Construct( SortList ) ) ;
      {$ELSE}
        lstRegisteredLayers.Sort( @SortList ) ;
      {$ENDIF}
    end ;
  end ;

  // Remove driver from the supported drivers' list.
  // _driverName  driver name
  procedure RemoveDriver( const _driverName : String ) ;
  var
    i         : Integer ;
    RegLayer  : TGIS_RegistredLayer ;
  begin
    if assigned( lstRegisteredLayers ) then begin
      for i := 0 to lstRegisteredLayers.Count - 1 do begin
        RegLayer := TGIS_RegistredLayer( lstRegisteredLayers[ i ] ) ;
        if assigned( RegLayer ) then
          if RegLayer.DriverName = _driverName then
            lstRegisteredLayers.Delete( i ) ;
      end ;
    end ;
  end ;

{$IFDEF DCC}
  initialization

  finalization
    FreeRegisteredLayers ;
{$ENDIF}

//==================================== END =====================================
end.
