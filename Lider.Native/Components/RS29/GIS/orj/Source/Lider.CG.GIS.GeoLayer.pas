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

{$IFDEF DCC}
   unit GisLayer ;
   {$HPPEMIT '#pragma link "GisLayer"'}
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

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    System.Drawing,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.Generics.Collections,
    System.Math,
    System.SyncObjs,
    System.SysUtils,
    System.DateUtils,
    System.Types,
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}

    GisConfig,
    GisConfigXml,
    GisCsSystems,
    GisInterfaces,
    GisParams,
    GisRtl,
    GisStatistics,
    GisTransform,
    GisTypes;
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

type

  TGIS_Layer = {$IFDEF OXYGENE} public abstract {$ENDIF} class ;

  {$REGION 'TGIS_LayerEvent*'}
  //----------------------------------------------------------------------------

  {$IFDEF OXYGENE}
    /// <summary>
    ///   Provides data for the layer event.
    /// </summary>
    TGIS_LayerEventArgs = public class ( EventArgs )
      private
        FLayer : TGIS_Layer ;

      public
        /// <summary>
        /// Create an object
        /// </summary>
        /// <param name="_layer">
        ///   given layer
        /// </param>
        constructor Create  ( const _layer : TGIS_Layer
                            ) ;

      public
        /// <summary>
        ///   Layer for layer event.
        /// </summary>
        property Layer      : TGIS_Layer read FLayer ;
    end ;

    /// <summary>
    ///   Standard event for layer.
    /// </summary>
    /// <param name="_sender">
    ///   sender object
    /// </param>
    /// <param name="_e">
    ///   TGIS_LayerEvent arguments
    /// </param>
    TGIS_LayerEvent = public procedure(
      _sender  : Object         ;
      _e       : TGIS_LayerEventArgs
    ) of object ;
  {$ELSE}
    /// <summary>
    ///   Standard event for OnXXXX.
    /// </summary>
    /// <param name="_sender">
    ///   sender object
    /// </param>
    /// <param name="_layer">
    ///   layer to be passed
    /// </param>
    {$IFDEF GENXDK}
      TGIS_LayerEvent = procedure(
        var _translated : Boolean ;
            _sender     : TObject   ;
            _layer      : TGIS_Layer
      ) of object ;
    {$ELSE}
      TGIS_LayerEvent = procedure(
        _sender     : TObject   ;
        _layer      : TGIS_Layer
      ) of object ;
    {$ENDIF}
  {$ENDIF}

  {$IFDEF ActiveX}
    /// <summary>
    ///   Standard event for OnXXXX.
    /// </summary>
    TGIS_LayerEventEx = procedure(       _sender     : TObject        ;
                                   var   _translated : Boolean        ;
                                         _layer      : TGIS_Layer
                                 ) of object ;
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'TGIS_ForEachLayerEvent*'}
  //----------------------------------------------------------------------------

  /// <summary>
  ///   Callback event for ForEachSubLayer.
  /// </summary>
  /// <param name="_layer">
  ///   layer handle
  /// </param>
  /// <param name="_abort">
  ///   If True, traversing will be stopped
  /// </param>
  TGIS_ForEachLayerEvent = {$IFDEF OXYGENE} public {$ENDIF} procedure(
        _layer  : TGIS_Layer ;
    var _abort  : Boolean
  ) of object ;
  {$ENDREGION}

  {$REGION 'T_View3D'}
  //----------------------------------------------------------------------------

  {$IFDEF OXYGENE}
    T_View3D nested in TGIS_Layer = public record
      public
        /// <summary>
        ///   Mode of 3D operations
        /// </summary>
        /// <remarks>
        ///   Deprecated. Use Layer3D property instead.
        /// </remarks>
        Mode : TGIS_3DLayerType ;
      private
        /// <summary>
        ///   True if Mode was not set by config file.
        /// </summary>
        ModeDefault : Boolean ;
    end ;
  {$ENDIF}
  {$ENDREGION}

  /// <summary>
  ///  State of Tiled mode rendering
  /// </summary>
  TGIS_LayerTiledMode =
  (
    /// <summary>
    ///   No-tiled drawing
    /// </summary>
    None,

    /// <summary>
    ///   Drawing tiles connet.
    /// </summary>
    Tiles,

    /// <summary>
    ///   Drawing complete conent like labels, selections etc.
    /// </summary>
    Complete
  ) ;


  {$REGION 'TGIS_Layer*'}
  //============================================================================

  /// <summary>
  ///   General layer class. Must be derived to provide file access.
  /// </summary>
  TGIS_Layer = {$IFDEF OXYGENE} public abstract {$ENDIF}
                       class( TGIS_LayerAbstract )
    public
      /// <summary>
      ///   3D Representation properties.
      /// </summary>
      {$IFDEF OXYGENE}
        View3D : T_View3D ;
      {$ELSE}
        View3D : record
          public
            /// <summary>
            ///   Mode of 3D operations
            /// </summary>
            /// <remarks>
            ///   Deprecated. Use Layer3D property instead.
            /// </remarks>
            Mode : TGIS_3DLayerType ;
          private
            /// <summary>
            ///   True if Mode was not set by config file.
            /// </summary>
            ModeDefault : Boolean ;
        end ;
      {$ENDIF}

    // CS Context
    private
      csctxMustRepoject  : Integer ; // 0 - undefined; -1 no; 1 transform only, 2 - full

      {$IFDEF DCC}[unsafe]{$ENDIF}
      csctxViewer        : IGIS_Viewer ;
      csctxViewerCS      : TGIS_CSCoordinateSystem ;
      csctxRotationAngle : Double ;

    private
      lastHourglassState : Int64 ;
      criticalSection    : TCriticalSection ;
      modifiedCS         : Boolean ; // true if modified after opening
      modifiedCodePage   : Boolean ; // true if modified after opening
      lastHash           : Int64   ;

    // properties internal values
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <summary>
      ///   Indicates whether the layer supports auto styling.
      /// </summary>
      FSupportsAutoStyle : Boolean ;

      /// <summary>
      ///   Indicates whether the layer supports tiled paint.
      /// </summary>
      FSupportsTiledPaint : Boolean ;

      /// <summary>
      ///   Reference to viewer object.
      /// </summary>
      FViewer : TGIS_ViewerRef ;

      /// <summary>
      ///   Layer path.
      /// </summary>
      FPath : String ;

      /// <summary>
      ///   Layer driver.
      /// </summary>
      FDriver : String ;

      /// <summary>
      ///   Reference to a potential layer underlying stream.
      /// </summary>
      FStream : TStream ;

      /// <summary>
      ///   Layer caption.
      /// </summary>
      FCaption : String ;

      /// <summary>
      ///   Tag value.
      /// </summary>
      FTag : Integer ;

      /// <summary>
      ///   Pointer Tag value
      /// </summary>

        {$IFDEF MANAGED}
          FTagPointer : Object ;
        {$ELSE}
         FTagPointer : Pointer ;
        {$ENDIF}

      /// <summary>
      ///   Internal Tag value.
      /// </summary>
      FTagInternal : Integer ;

      /// <summary>
      ///   User object. Will be destroyed upon layer destroy.
      /// </summary>
      FUserObject : TObject ;

      /// <summary>
      ///   Layer name.
      /// </summary>
      FName : String ;

      /// <summary>
      ///   Additional comments.
      /// </summary>
      FComments : String ;

      /// <summary>
      ///   Precalculated projected extent.
      /// </summary>
      FProjectedExtent : TGIS_Extent ;

      /// <summary>
      ///   Precalculated projected extent - Viewer CS Context.
      /// </summary>
      FProjectedExtentViewerEPSG : Integer ;

      /// <summary>
      ///   Precalculated projected extent - Layer CS Context.
      /// </summary>
      FProjectedExtentEPSG : Integer ;

      /// <summary>
      ///   Precalculated projected extent - Base extent (unprojected).
      /// </summary>
      FProjectedExtentBase : TGIS_Extent ;

      /// <summary>
      ///   Precalculated projected extent - Rotation context.
      /// </summary>
      FProjectedExtentRotationAngle : Double ;

      /// <summary>
      ///   Precalculated projected extent - Rotation context.
      /// </summary>
      FProjectedExtentRotationPoint : TGIS_Point  ;

      /// <summary>
      ///   Is layer active?
      /// </summary>
      FActive : Boolean ;

      /// <summary>
      ///   True if layer should not be visible in a legend.
      /// </summary>
      FHideFromLegend : Boolean ;

      /// <summary>
      ///   Is layer collapsed?
      /// </summary>
      FCollapsed : Boolean ;

      /// <summary>
      ///   Is config file active?
      /// </summary>
      FUseConfig : Boolean ;

      /// <summary>
      ///   True if the layer upon paint process.
      /// </summary>
      FInPaint : Boolean ;

      /// <summary>
      ///   True if the layer is interpreted as basemap. Is
      ///   important only for bottommost layers.
      /// </summary>
      FBasemap : Boolean ;

      /// <summary>
      ///   False if layer must be painted directly (bypassing cache).
      ///   Important only for topmost layers.
      /// </summary>
      FCachedPaint : Boolean ;

      /// <summary>
      ///   Transparency value.
      /// </summary>
      FTransparency  : Integer ;

      /// <summary>
      ///   Addition value.
      /// </summary>
      FAddition : Integer ;

      /// <summary>
      ///   Dormant mode for the layer.
      /// </summary>
      FDormantMode : TGIS_LayerDormantMode ;

      /// <summary>
      ///   True if layer is in locked state Lock.
      /// </summary>
      FIsLocked : Boolean ;

      /// <summary>
      ///   True if layer was opened.
      /// </summary>
      FIsPrepared : Boolean ;

      /// <summary>
      ///   True if layer was opened.
      /// </summary>
      FIsOpened : Boolean ;

      /// <summary>
      ///   True if structure (field, geometry etc) was modified.
      /// </summary>
      FIsModified  : Boolean ;

      /// <summary>
      ///   Configuration file handle.
      /// </summary>
      FConfigFile : TGIS_Config ;
      /// <summary>
      ///   Configuration file handle.
      /// </summary>
      FConfigFile2 : TGIS_Config ;

      /// <summary>
      ///   Code Page in which text has been stored.
      /// </summary>
      FCodePage : Integer ;

      /// <summary>
      ///   Code Page forced by project file or property setting.
      /// </summary>
      FCodePageForced : Integer ;

      /// <summary>
      ///   Configuration file name. If not filed, directly then will be
      ///   filed with Path.
      /// </summary>
      FConfigName : String ;

      /// <summary>
      ///   Projection object.
      /// </summary>
      FCS : TGIS_CSCoordinateSystem ;

      /// <summary>
      ///   List of parameters set (sections).
      /// </summary>
      FParamsList : TGIS_ParamsList ;

      /// <summary>
      ///   List of custom data.
      /// </summary>
      FCustomData  : TGIS_StringList ;

      /// <summary>
      ///   Busy event. Will be fired regularly during long-drawn
      ///   operations.
      /// </summary>
      FOnBusy : TGIS_BusyEvent ;

      /// <summary>
      ///   Sub layers list.
      /// </summary>
      FSubLayers       : TGIS_LayerAbstractList ;

      /// <summary>
      ///   Parent layer.
      /// </summary>
      FParentLayer     : TGIS_Layer ;

      /// <summary>
      ///   Multiuser mode.
      /// </summary>
      FMultiUserMode   : TGIS_MultiUser ;

      /// <summary>
      ///   Read only flag.
      /// </summary>
      FIsReadOnly      : Boolean ;

      /// <summary>
      ///   Custom transformation.
      /// </summary>
      FTransform  : TGIS_Transform ;

      /// <summary>
      ///  Statistics engine.
      /// </summary>
      FStatistics : TGIS_StatisticsAbstract ;

      /// <summary>
      ///   Maximum allowed tile size for the layer. Default is no limit
      ///   and is represented by GIS_MAX_INTEGER value.
      /// </summary>
      FMaxTileSize     : TPoint ;

      /// <summary>
      ///   Handle to a renderer used for drawing shapes.
      /// </summary>
      {$IFDEF DCC}[unsafe]{$ENDIF}
      FRenderer  : TObject ;

      /// <summary>
      ///   Set of operations not supported by a layer.
      /// </summary>
      FUnSupportedOperations : TGIS_OperationTypes ;

      /// <summary>
      ///   If True, the layer at the moment is drawn inside a basemap thread.
      /// </summary>
      FBasemapDraw : Boolean ;

      /// <summary>
      ///   If not None then the layer at the moment is drawn inside a basemap thread.
      /// </summary>
      FTiledDrawMode : TGIS_LayerTiledMode ;

    // properties - events
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      /// <summary>
      ///   PaintLayer event. Will be fired on Paint etc.
      /// </summary>
      FOnPaintLayer : TGIS_LayerEvent ;

      /// <summary>
      ///   Will be fired upon opening layer to resolve any
      ///   username/password
      /// </summary>
      FOnPassword : TGIS_TemplateProducerEvent ;

      /// <summary>
      ///   Will be fired upon each file Read request.
      /// </summary>
      FOnRead  : TGIS_ReadWriteEvent ;

      /// <summary>
      ///   Will be fired upon each file Write request.
      /// </summary>
      FOnWrite : TGIS_ReadWriteEvent ;

    protected

      /// <summary>
      ///   Additional textual information about the layer such as
      ///   compression, number of pixels, copyright etc.
      /// </summary>
      FFileInfo : String ;

      /// <summary>
      ///   Additional textual information about layer copyright.
      /// </summary>
      FFileCopyright : String ;

    // property access routines
    protected

      function  fget_Path            : String ; virtual;
      procedure fset_Path            ( const _value : String
                                     ) ; virtual;
      function  fget_PathWithDriver  : String ; virtual;
      procedure fset_Statistics      ( const _value : TGIS_StatisticsAbstract
                                     ) ;
      function  fget_Stream          : TStream ; virtual;
      procedure fset_Stream          ( const _value : TStream
                                     ) ; virtual;
      function  fget_Caption         : String ; virtual;
      procedure fset_Caption         ( const _value : String
                                     ) ; virtual;
      function  fget_Tag             : Integer ; virtual;
      procedure fset_Tag             ( const _value : Integer
                                     ) ; virtual;
      {$IFDEF MANAGED}
        function  fget_TagPointer    : Object ; virtual;
        procedure fset_TagPointer    ( const _value : Object
                                     ) ; virtual;
      {$ELSE}
        function  fget_TagPointer    : Pointer ; virtual;
        procedure fset_TagPointer    ( const _value : Pointer
                                     ) ; virtual;
      {$ENDIF}

      function  fget_TagInternal     : Integer ; virtual;
      procedure fset_TagInternal     ( const _value : Integer
                                     ) ; virtual;
      function  fget_UserObject      : TObject ; virtual;
      procedure fset_UserObject      ( const _value : TObject
                                     ) ; virtual;
      function  fget_Name            : String ; virtual;
      procedure fset_Name            ( const _value : String
                                     ) ; virtual;
      function  fget_FileInfo        : String ; virtual;
      function  fget_FileCopyrights  : String ; virtual;
      function  fget_Comments        : String ; virtual;
      procedure fset_Comments        ( const _value : String
                                     ) ; virtual;
      function  fget_IsLocked        : Boolean ; virtual;
      function  fget_IsOpened        : Boolean ; virtual;
      function  fget_IsTopmost       : Boolean ; virtual;
      function  fget_DirectMode      : Boolean ; virtual;
      {$IFDEF CLR}
        function  fget_Age           : DateTime ; virtual;
      {$ELSE}
        function  fget_Age           : TDateTime ; virtual;
      {$ENDIF}
      function  fget_SubType         : TGIS_LayerSubTypeSet ; virtual;
      function  fget_IsExportable    : Boolean ;
      function  fget_IsPersistent    : Boolean ;
      function  fget_Viewer          : TGIS_ViewerRef ; virtual;
      procedure fset_Viewer          ( const _value : TGIS_ViewerRef
                                     ) ; virtual;
      function  fget_ConfigFile      : TGIS_ConfigAbstract ; virtual;
      function  fget_Extent          : TGIS_Extent ; virtual;
      procedure fset_Extent          ( const _value : TGIS_Extent
                                     ) ; virtual;
      function  fget_Extent3D        : TGIS_Extent3D ; virtual;
      procedure fset_Extent3D        ( const _value : TGIS_Extent3D
                                     ) ; virtual;
      function  fget_ProjectedExtent : TGIS_Extent ; virtual;
      procedure fset_ProjectedExtent ( const _value : TGIS_Extent
                                     ) ; virtual;
      function  fget_Active          : Boolean ; virtual;
      procedure fset_Active          ( const _value : Boolean
                                     ) ; virtual;
      function  fget_HideFromLegend  : Boolean ; virtual;
      procedure fset_HideFromLegend  ( const _value : Boolean
                                     ) ; virtual;
      function  fget_Collapsed       : Boolean ; virtual;
      procedure fset_Collapsed       ( const _value : Boolean
                                     ) ; virtual;
      function  fget_ZOrder          : Integer ; virtual;
      procedure fset_ZOrder          ( const _value : Integer
                                     ) ; virtual;
      function  fget_ZOrderEx        : Integer ; virtual;
      procedure fset_ZOrderEx        ( const _value : Integer
                                     ) ; virtual;
      function  fget_UseConfig       : Boolean ; virtual;
      procedure fset_UseConfig       ( const _value : Boolean
                                     ) ; virtual;
      function  fget_ConfigName      : String ; virtual;
      procedure fset_ConfigName      ( const _value : String
                                     ) ; virtual;
      function  fget_InPaint         : Boolean ; virtual;
      function  fget_Basemap         : Boolean ; virtual;
      procedure fset_Basemap         ( const _value : Boolean
                                     ) ; virtual;
      function  fget_CachedPaint     : Boolean ; virtual;
      procedure fset_CachedPaint     ( const _value : Boolean
                                     ) ; virtual;
      function  fget_Transparency    : Integer ; virtual;
      procedure fset_Transparency    ( const _value : Integer
                                     ) ; virtual;
      function  fget_Addition        : Integer ; virtual;
      procedure fset_Addition        ( const _value : Integer
                                     ) ; virtual;
      function  fget_DormantMode     : TGIS_LayerDormantMode ; virtual;
      procedure fset_DormantMode     ( const _value : TGIS_LayerDormantMode
                                     ) ; virtual;
      function  fget_CodePage        : Integer ; virtual;
      procedure fset_CodePage        ( const _value : Integer
                                     ) ; virtual;
      function  fget_SubLayers       : TGIS_LayerAbstractList ; virtual;
      procedure fset_SubLayers       ( const _value : TGIS_LayerAbstractList
                                     ) ; virtual;
      function  fget_ParentLayer     : TGIS_Layer ; virtual;
      procedure fset_ParentLayer     ( const _value : TGIS_Layer
                                     ) ; virtual;
      function  fget_CS              : TGIS_CSCoordinateSystem ; virtual;
      procedure fset_CS              ( const _value : TGIS_CSCoordinateSystem
                                     ) ; virtual;
      function  fget_MultiUserMode   : TGIS_MultiUser ; virtual;
      procedure fset_MultiUserMode   ( const _value : TGIS_MultiUser
                                     ) ; virtual;
      function  fget_IsReadOnly      : Boolean ; virtual;
      function  fget_Params          : TGIS_ParamsSection ; virtual;
      procedure fset_Params          ( const _value : TGIS_ParamsSection
                                     ) ; virtual;
      function  fget_Transform       : TGIS_Transform ; virtual;
      procedure fset_Transform       ( const _value : TGIS_Transform
                                     ) ; virtual;
      function  fget_ParamsList      : TGIS_ParamsList ; virtual;

      function  fget_Renderer        : TObject ; virtual;
      procedure fset_Renderer        ( const _value : TObject
                                     ) ; virtual;

      procedure fset_View3DMode      ( const _value : TGIS_3DLayerType
                                     ) ; virtual;

      function fget_SupportsTiledPaint
                                     : Boolean; virtual ;

    // protected properties internal values
    protected
      /// <summary>
      ///   Age of the layer.
      /// </summary>
      {$IFDEF CLR}
              FAge :  DateTime  ;
             {$ELSE}
             FAge :   TDateTime ;
             {$ENDIF}

      /// <summary>
      ///   Type of sublayer.
      /// </summary>
      FSubType : TGIS_LayerSubTypeSet ;

      /// <summary>
      ///   Layer extent.
      /// </summary>
      FExtent : TGIS_Extent ;

      /// <summary>
      ///   Layer extent 3D.
      /// </summary>
      FExtent3D : TGIS_Extent3D ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <summary>
      ///   Set tor True to force cached (bitmap) type of drawing.
      /// </summary>
      forceCachedMode : Boolean ;

      /// <summary>
      ///   True if layer paint procedure not yet complete. To avoid
      ///   circular reference.
      /// </summary>
      inDraw : Boolean ;

      /// <summary>
      ///   List of prepared parameters.
      /// </summary>
      paramsCache : TList< TGIS_ParamsSection >  ;

      /// <summary>
      ///   If True, parameters are ready to render
      /// </summary>
      paramsCacheUpdated : Boolean  ;

    // other functions
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <summary>
      ///   Checks if the layer is a sublayer.
      /// </summary>
      /// <returns>
      ///   True if the layer is a sublayer
      /// </returns>
      function  isSublayer           : Boolean ;

      /// <summary>
      ///   Reset MustReporoject state. Upon next MapReproject state will
      ///   be recalculated.
      /// </summary>
      /// <remarks>
      ///    To use upon subclassing of RecalcProjectedExtent if base version is
      ///    not going to be called (e.g. TGIS_LayerPixel).
      /// </remarks>
      procedure resetMustReproject ;

      /// <summary>
      ///   Calculates the drawing area /clipping area of device/.
      /// </summary>
      /// <returns>
      ///   calculated extent
      /// </returns>
      function  drawExtent     : TGIS_Extent ;

      /// <summary>
      ///   Calculates the drawing area /clipping area of device/.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    on printer and bigimage devices will provide bigger extent
      ///    to guarantee overlaps
      ///    </note>
      /// </remarks>
      /// <returns>
      ///   calculated extent
      /// </returns>
      function  drawExtentEx   : TGIS_Extent ; virtual ;

      // for internal use of TGIS_Viewer

      /// <summary>
      ///   Layer implementation of auto-styling.
      /// </summary>
      procedure doApplyAutoStyle ; virtual ;

      /// <summary>
      ///   Makes a general layer setup first step.
      ///   This is the place to add variable initialization, etc. after
      ///   setUp was called.
      /// </summary>
      procedure setUp          ; virtual;

      /// <summary>
      ///   Makes a general layer setup second step.
      ///   This is the place to additional initialization valid only
      ///   after normal setUp etc.
      /// </summary>
      procedure setUp2         ; virtual;

      /// <summary>
      ///   Makes a general layer setup third step.
      ///   This is the place to additional initialization valid only
      ///   after normal setUp etc.
      /// </summary>
      procedure setUp3         ; virtual;

      /// <summary>
      ///   Read config projection parameters.
      /// </summary>
      /// <param name="_cfg">
      ///   configuration file object
      /// </param>
      procedure applyConfigProjection
                                  ( const _cfg : TGIS_ConfigAbstract
                                  ) ; overload; virtual;

      /// <summary>
      ///   Read config projection parameters.
      /// </summary>
      /// <param name="_cfg">
      ///   configuration file object
      /// </param>
      /// <param name="_layer">
      ///   object will be set
      /// </param>
      procedure applyConfigProjection
                                  ( const _cfg : TGIS_ConfigAbstract ;
                                    const _layer : TGIS_Layer
                                  ) ; overload; virtual;

      /// <summary>
      ///   Read config option parameters.
      /// </summary>
      /// <param name="_cfg">
      ///   configuration file object
      /// </param>
      /// <remarks>
      ///   Options are a layer wide parameters like CodePage etc.
      /// </remarks>
      procedure applyConfigOptions( const _cfg : TGIS_ConfigAbstract
                                  ) ; virtual;

      /// <summary>
      ///   Store layer options in a configuration file.
      /// </summary>
      /// <param name="_cfg">
      ///   configuration file object
      /// </param>
      /// <remarks>
      ///   Options are a layer wide parameters like CodePage etc.
      /// </remarks>
      procedure storeConfigOptions( const _cfg : TGIS_ConfigAbstract
                                  ) ; virtual;

      /// <summary>
      ///   Checks whether the section is visible by testing layer properties:
      ///   Level with MinLevel/MaxLevel,
      ///   ScaleAsFloat with MinScale/MaxScale, and
      ///   Zoom with MinZoom/MaxZoom.
      /// </summary>
      /// <param name="_sec">
      ///   a params section to test for the visibility
      /// </param>
      /// <returns>
      ///   True if section is visible for current iewer Level / Scale / Zoom,
      ///   False oterwise
      /// </returns>
      function isSectionInVisibilityRange
                                  ( const _sec : TGIS_ParamsSection
                                  ) : Boolean ;
      /// <summary>
      ///   Prepare cached section list meeting the _style name.
      /// </summary>
      /// <remarks>
      ///   Cached list will be uses to optimized rendering. Cached list
      ///   will have only sections meeting MinZoom..MaxZoom
      ///   (MinScale..MaxScale, MinLevel..MaxLevel) criteria.
      /// </remarks>
      procedure prepareParamsCache; overload;

      /// <summary>
      ///   Prepare cached section list meeting the _style name.
      /// </summary>
      /// <param name="_style">
      ///   style name of sections to be used
      /// </param>
      /// <remarks>
      ///   Cached list will be uses to optimized rendering. Cached list
      ///   will have only sections meeting MinZoom..MaxZoom
      ///   (MinScale..MaxScale, MinLevel..MaxLevel) criteria.
      /// </remarks>
      procedure prepareParamsCache( const _style : String
                                  ) ; overload;

      /// <summary>
      ///   Move section specified by _idx to the top of the cached list.
      /// </summary>
      /// <param name="_idx">
      ///   index of section on ParamsList
      /// </param>
      /// <remarks>
      ///   Thanks this last used section will be traversed as a first
      ///   one giving speed gain.
      /// </remarks>
      procedure optimizeParamsCache
                                  ( const _idx : Integer
                                  ) ; overload;

      /// <summary>
      ///   Move section specified by _idx to the top of the cached list.
      /// </summary>
      /// <param name="_idx">
      ///   index of section on ParamsList
      /// </param>
      /// <param name="_lru">
      ///   if true then parameter should be reorganized in LRU manner
      /// </param>
      /// <remarks>
      ///   Thanks this last used section will be traversed as a first
      ///   one giving speed gain.
      /// </remarks>
      procedure optimizeParamsCache
                                  ( const _idx : Integer;
                                    const _lru : Boolean
                                  ) ; overload;

      /// <summary>
      ///   Custom transformation of the extent.
      /// </summary>
      /// <param name="_forward">
      ///   if true the transformation if forward; if else transformation
      ///   is reversed
      /// </param>
      /// <param name="_extent">
      ///   extent to be transformed;
      /// </param>
      /// <returns>
      ///   transformed extent
      /// </returns>
      function  transformExtent   ( const _forward : Boolean;
                                    const _extent  : TGIS_Extent
                                  ) : TGIS_Extent  ;

      /// <summary>
      ///   Locks thread.
      /// </summary>
      /// <remarks>
      ///   Lock list against other threads.
      /// </remarks>
      procedure lockThread   ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}

      /// <summary>
      ///   Unlocks thread.
      /// </summary>
      /// <remarks>
      ///   Release lock obtained by LockThread.
      /// </remarks>
      procedure unlockThread ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}

    // API level variables
    public

      /// <summary>
      ///   Path to file holding  the layer's data.
      /// </summary>
      property Path : String read fget_Path write fset_Path ;

      /// <summary>
      ///   The layer's driver name.
      /// </summary>
      property Driver : String read FDriver ;

      /// <summary>
      ///   Path to file with the layer's data extended with driver name.
      /// </summary>
      property PathWithDriver : String read fget_PathWithDriver ;

      /// <summary>
      ///   Layer statistics engine
      /// </summary>
      property Statistics : TGIS_StatisticsAbstract read FStatistics write fset_Statistics ;

      /// <summary>
      ///   Reference to a potential layer underlying the stream.
      ///   Some layers like (TGIS_LayerJPG) can read the data
      ///   from the stream instead of from the file provided by the Path.
      /// </summary>
      property Stream : TStream read fget_Stream write fset_Stream ;

      /// <summary>
      ///   Caption of layer; if not filed directly then will be filed
      ///   with Name when adding to the Viewer.
      /// </summary>
      property Caption : String read fget_Caption write fset_Caption ;

      /// <summary>
      ///   Tag has no predefined meaning. The Tag property is provided
      ///   for the convenience of storing additional integer value or
      ///   pointer information for special needs in an application.
      /// </summary>
      property Tag : Integer read fget_Tag write fset_Tag ;

      /// <summary>
      ///   TagPointer has no predefined meaning. The TagPointer
      ///   property is provided for the convenience of storing
      ///   additional pointer information for special needs in an
      ///   application.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Deprecated. Use UserObject instead.
      ///    </note>
      /// </remarks>
      property TagPointer :
        {$IFDEF MANAGED} Object {$ELSE} Pointer {$ENDIF}
          read  fget_TagPointer
          write fset_TagPointer ;

      /// <summary>
      ///   TagInternal has no predefined meaning. The TagInternal property
      ///   is for storing additional values or information for internal use by
      ///   TatukGIS. Please do not use this property on your own!
      /// </summary>
      property TagInternal : Integer read  fget_TagInternal
                                     write fset_TagInternal ;

      /// <summary>
      ///   UserObject can be used to associate with the layer a user-defined
      ///   object. Such object will be destroyed automatically
      ///   upon layer destroy. Attaching a new object to this property
      ///   will destroy existing  one.
      /// </summary>
      {#ownership:set:release}
      property UserObject : TObject read  fget_UserObject
                                    write fset_UserObject ;

      /// <summary>
      ///   Mode of 3D operations
      /// </summary>
      property Layer3D : TGIS_3DLayerType read  View3D.Mode
                                          write View3D.Mode ;

    protected

      /// <summary>
      ///   Destroy a layer and remove it from the Viewer.
      /// </summary>
      procedure doDestroy        ; override;

    // constructors & destructor
    public

      /// <summary>
      ///   Create a layer with default parameters.
      /// </summary>
      constructor Create         ; overload; virtual;
      {$IFDEF EXPERIMENTAL}
        /// <summary>
        ///   Create a layer and open specified file.
        /// </summary>
        /// <param name="_path">
        ///   path to the layer file
        /// </param>
        constructor Create         ( const _path     : String
                                   ) ; overload; virtual;
      {$ENDIF}

    // API
    public

      /// <summary>
      ///   Applies auto-styling to the layer.
      /// </summary>
      /// <returns>
      ///   True if the application was successful.
      /// </returns>
      /// <remarks>
      ///   The layer cannot have modified Params and must support auto-styling.
      /// </remarks>
      function ApplyAutoStyle : Boolean ; overload ; virtual ;

      /// <summary>
      ///   Applies auto-styling to the layer.
      /// </summary>
      /// <param name="_forceAutoStyle">
      ///   if True, forces auto-styling despite modified Params
      /// </param>
      /// <returns>
      ///   True if the application was successful.
      /// </returns>
      /// <remarks>
      ///   The layer must support auto-styling.
      /// </remarks>
      function ApplyAutoStyle( const _forceAutoStyle: Boolean ) : Boolean ; overload ; virtual ;

      /// <summary>
      ///   Changes the hash for the purpose of verifying layer's modifications.
      /// </summary>
      /// <returns>
      ///   True if item already exists on the list.
      /// </returns>
      /// <remarks>
      ///   Used to verify that the order of layers, params, etc.,
      ///   have been modified. Used mainly by TGIS_ControlLegend.
      /// </remarks>
      function ChangeHash        : Int64 ;

      /// <summary>
      ///   Opens the layer.
      /// </summary>
      /// <remarks>
      ///   This method opens the layer and reads all its data. After this, all
      ///   non-drawing operations on the layer will be available. But you can
      ///   add such layer later to the viewer, anyway.
      /// </remarks>
      procedure Prepare          ; virtual;

      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      /// <summary>
      ///   For internal use only.
      /// </summary>
      procedure PrepareParams    ;

      /// <summary>
      ///   Opens the layer.
      /// </summary>
      procedure Open             ; virtual;

      /// <summary>
      ///   Reopens the layer.
      /// </summary>
      /// <remarks>
      ///   Will reopen the layer and read all data. After this, all non drawing
      ///   operations on layer will be available. But you can add
      ///   such layer later to the viewer anyway.
      /// </remarks>
      procedure ReOpen           ; virtual;

      /// <summary>
      ///   Notifies of busy state using an hourglass.
      /// </summary>
      /// <remarks>
      ///   TGIS_Viewer.HourglassShake.
      /// </remarks>
      /// <returns>
      ///   True if current operation can be interrupted
      /// </returns>
      function  HourglassShake   : Boolean ; {$IFDEF DCC} inline ; {$ENDIF}

      /// <summary>
      ///   Moves a layer up or down in the viewer zorder list.
      /// </summary>
      /// <param name="_step">
      ///   how many steps and in which direction a layer must be moved;
      ///   negative values mean movement to the front.
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    To move only visible layers (HideFromLegend=False) use
      ///    TGIS_Layer.MoveEx instead.
      ///    </note>
      /// </remarks>
      procedure Move             ( const _step     : Integer
                                 ) ; virtual;

      /// <summary>
      ///   Moves a layer up or down in the viewer zorder list (only visible
      ///   layers). This procedure takes into account only visible layers;
      ///   invisible layers (HideFromLegend=True) located after current layer
      ///   will be moved together. If current layer is not visible, then the
      ///   nearest visible (but previous on the Items list) layer will be
      ///   used as an anchor.
      /// </summary>
      /// <param name="_step">
      ///   how many steps and in which direction a layer must be moved;
      ///   negative values mean movement to the front.
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    To move layer regardless of visibility (any HideFromLegend
      ///    value), use instead TGIS_Layer.Move instead.
      ///    </note>
      /// </remarks>
      procedure MoveEx           ( const _step     : Integer
                                 ) ; virtual;

      /// <summary>
      ///   Reads the layer configuration files.
      /// </summary>
      /// <remarks>
      ///   Screen will be invalidated. See TGIS_Viewer.ReadConfig for
      ///   example.
      /// </remarks>
      procedure ReadConfig       ; virtual;

      /// <summary>
      ///   Reads the layer configuration parameters.
      /// </summary>
      /// <param name="_name">
      ///   parameter name
      /// </param>
      /// <returns>
      ///   config name
      /// </returns>
      function  ReadConfigParam  ( const _name     : String
                                 ) : String ; virtual;

      /// <summary>
      ///   Rereads the layer configuration files. Screen will be invalidated.
      /// </summary>
      procedure RereadConfig     ; virtual;

      /// <summary>
      ///   Writes layer configuration files.
      /// </summary>
      /// <remarks>
      ///   <para>
      ///     See TGIS_Viewer.ReadConfig for example.
      ///   </para>
      ///   <para>
      ///      Params can be saved in project or configuration file. You can
      ///     select this using KeepParamsInProject.
      ///   </para>
      /// </remarks>
      procedure WriteConfig      ; virtual;

      /// <summary>
      ///   Draws the layer or fires PaintLayer event (if defined).
      /// </summary>
      /// <returns>
      ///   True if draw was performed
      /// </returns>
      function  Paint            : Boolean ;

      /// <summary>
      ///   Draws a layer.
      /// </summary>
      /// <returns>
      ///   True if draw was performed
      /// </returns>
      function  Draw             : Boolean ; virtual;

      /// <summary>
      ///   Draws only the selected item(s) on layer.
      /// </summary>
      /// <returns>
      ///   True if draw was performed
      /// </returns>
      function  DrawSelected     : Boolean ; virtual;

      /// <summary>
      ///   Draws a layer within a defined extent.
      /// </summary>
      /// <param name="_extent">
      ///   extent to be drawn
      /// </param>
      /// <returns>
      ///   True if draw was performed
      /// </returns>
      function  DrawEx           ( const _extent : TGIS_Extent
                                 ) : Boolean ; virtual;
      /// <summary>
      ///   Draws only the selected item(s) on layer that are within a defined extent.
      /// </summary>
      /// <param name="_extent">
      ///   extent to be drawn
      /// </param>
      /// <returns>
      ///   True if draw was performed
      /// </returns>
      function  DrawSelectedEx   ( const _extent : TGIS_Extent
                                 ) : Boolean ; virtual;
      /// <summary>
      ///   Draws a flash.
      /// </summary>
      procedure DrawFlash        ; virtual;

      /// <summary>
      ///   Reverts layer to its original content.
      /// </summary>
      /// <remarks>
      ///   All unsaved changes will be abandoned.
      /// </remarks>
      procedure RevertAll        ; virtual;

      /// <summary>
      ///   Saves layer and destroys mirrored items. See also
      ///   TGIS_Layer.SaveAll.
      /// </summary>
      procedure SaveData         ; virtual;

      /// <summary>
      ///   Saves layer and destroys mirrored items.
      /// </summary>
      /// <remarks>
      ///   <para>
      ///     Will save data and any exiting configuration file. Internally
      ///     will call TGIS_Layer.SaveData.
      ///   </para>
      ///   <para>
      ///      See TGIS_Viewer.MustSave for example.
      ///   </para>
      /// </remarks>
      procedure SaveAll          ; virtual;

      /// <summary>
      ///   Checks if the layer was modified by editing.
      /// </summary>
      /// <returns>
      ///   True if layer was modified.
      /// </returns>
      /// <remarks>
      ///   Use this method to check if the layer was edited any way. If Yes
      ///   you should save a project to keep changes.
      /// </remarks>
      function  MustSave         : Boolean ; virtual  ;

      //? summary is not true for LayerPixel
      /// <summary>
      ///   Recalculates extent based on real shapes.
      /// </summary>
      procedure RecalcExtent     ; virtual;

      /// <summary>
      ///   Recalculates a projected extent.
      /// </summary>
      /// <remarks>
      ///   Calculation is only an estimation based on a 72x36 grid. The grid
      ///   is necessary because any point from an extent can be moved the
      ///   after projection to, in fact, any new location. This grid size
      ///   is used because it will be no more than 5x5 degrees.
      /// </remarks>
      procedure RecalcProjectedExtent ; virtual;

      /// <summary>
      ///   Locks layer in burst-mode operation.
      /// </summary>
      /// <remarks>
      ///   <para>
      ///     Extent for viewer will not be refreshed (i.e., extent
      ///     will not be recalculated).
      ///   </para>
      ///   <para>
      ///     Calling Lock, disables any projection-level operations.
      ///     AddPoint/GetPoint will operate in the layer's native coordinates.
      ///   </para>
      ///   <para>
      ///      See TGIS_Shape.Lock for a similar example.
      ///   </para>
      /// </remarks>
      procedure Lock             ; virtual;

      /// <summary>
      ///   Unlocks layer from burst-mode operation.
      /// </summary>
      /// <remarks>
      ///   <para>
      ///     Do so after Lock. This method will modify layer and viewer
      ///     extent.
      ///   </para>
      ///   <para>
      ///      See TGIS_Shape.Lock for similar example.
      ///   </para>
      /// </remarks>
      procedure Unlock           ; virtual;

      /// <summary>
      ///   Applies a projection on the single point.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be projected
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    If Rotation has been applied then point will be rotated
      ///    accordingly.
      ///    </note>
      /// </remarks>
      procedure Project_Ref      ( {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point
                                 ) ; virtual;

      /// <summary>
      ///   Applies reverse projection (recover from projection) on the single
      ///   point.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be projected
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    If Rotation has been applied then point will be rotated
      ///    accordingly.
      ///    </note>
      /// </remarks>
      procedure Unproject_Ref    ( {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point
                                 ) ; virtual;

      /// <summary>
      ///   Applies a projection on the single point in 3D.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be unprojected
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    If Rotation has been applied then point will be rotated
      ///    accordingly.
      ///    </note>
      /// </remarks>
      procedure Project3D_Ref    ( {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
                                 ) ; virtual;

      /// <summary>
      ///   Applies reverse projection (recovers from the projection) on
      ///   the single point in 3D.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be unprojected
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    If Rotation has been applied then point will be rotated
      ///    accordingly.
      ///    </note>
      /// </remarks>
      procedure Unproject3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
                                 ) ; virtual;

      /// <summary>
      ///   Applies a projection on the single point.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be projected
      /// </param>
      /// <returns>
      ///   point with applied projection
      /// </returns>
      function  Project          ( const _ptg      : TGIS_Point
                                 ) : TGIS_Point ; virtual;

      /// <summary>
      ///   Applies a projection on the single point in 3D.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be projected
      /// </param>
      /// <returns>
      ///   3D point with applied projection
      /// </returns>
      function  Project3D        ( const _ptg      : TGIS_Point3D
                                 ) : TGIS_Point3D ; virtual;

      /// <summary>
      ///   Applies reverse projection (recovers from the projection) on
      ///   the single point.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be unprojected
      /// </param>
      /// <returns>
      ///   unprojected point
      /// </returns>
      function  Unproject        ( const _ptg      : TGIS_Point
                                 ) : TGIS_Point ; virtual;

      /// <summary>
      ///   Applies reverse projection (recovers from the projection) on
      ///   the singlepoint in 3D.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be unprojected
      /// </param>
      /// <returns>
      ///   unprojected 3D point
      /// </returns>
      function  Unproject3D      ( const _ptg      : TGIS_Point3D
                                 ) : TGIS_Point3D ; virtual;


      /// <summary>
      ///   Applies a projection on the extent.
      /// </summary>
      /// <param name="_extent">
      ///   extent to be projected
      /// </param>
      /// <returns>
      ///   projected extent
      /// </returns>
      function  ProjectExtent    ( const _extent  : TGIS_Extent
                                 ) : TGIS_Extent ; virtual;

      /// <summary>
      ///   Applies reverse projection (recovers from the projection) on
      ///   the extent.
      /// </summary>
      /// <param name="_extent">
      ///   extent to be unprojected
      /// </param>
      /// <returns>
      ///   unprojected extent
      /// </returns>
      function  UnprojectExtent  ( const _extent  : TGIS_Extent
                                 ) : TGIS_Extent ; virtual;

      /// <summary>
      ///   Makes a layer non-dormant. Reallocates files, memory, etc.
      /// </summary>
      procedure Alive            ; virtual;

      /// <summary>
      ///  Calculates the approximate gain (in Megabytes) which can be achieved
      ///  by releasing memory by calling Dormant method.
      /// </summary>
      /// <returns>
      ///   Value in Megabytes
      /// </returns>
      function DormantGain       : Integer; virtual;

      /// <summary>
      ///   Makes a layer dormant. Reduces memory consumption etc.
      /// </summary>
      procedure Dormant          ; virtual;

      /// <summary>
      ///   Checks if the layer can read the file.
      /// </summary>
      /// <param name="_path">
      ///   file path to open
      /// </param>
      /// <param name="_new_path">
      ///   new path extracted from the file to open
      /// </param>
      /// <returns>
      ///   True if the layer can read the file
      /// </returns>
      function  PreRecognize     ( const _path     : String ;
                                     var _new_path : String
                                 ) : Boolean ; virtual;

      /// <summary>
      ///   Sets up Coordinate System to a coordinate system provided by a WKT
      ///   string (GEOGCS or PROJCS). If provided WKT string is empty then
      ///   coordinate system will be turned off.
      /// </summary>
      /// <param name="_wkt">
      ///   WKT string
      /// </param>
      procedure SetCSByWKT       ( const _wkt      : String
                                 ) ; virtual;

      /// <summary>
      ///   Sets up Coordinate System to a coordinate system provided by a EPSG
      ///   code.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code
      /// </param>
      procedure SetCSByEPSG      ( const _epsg     : Integer
                                 ) ; virtual;

      /// <summary>
      ///   Sets up Coordinate System to a coordinate system provided by a file
      ///   which contains a WKT string (GEOGCS or PROJCS). If file does not
      ///   exist or provided WKT string is empty then coordinate system
      ///   will be turned off.
      /// </summary>
      /// <param name="_path">
      ///   path with WKT string (for example shapefile .PRJ)
      /// </param>
      procedure SetCSByWKTFile   ( const _path     : String
                                 ) ; virtual;

      /// <summary>
      ///   Returns the absolute parent layer.
      /// </summary>
      /// <returns>
      ///   parent layer
      /// </returns>
      function  RootLayer        : TGIS_Layer ;

      {#gendoc:hide:GENPDK} { TODO -cPDK : Verify }
      /// <summary>
      ///   Returns in callback event the layer and all sublayers belonging
      ///   to it.
      /// </summary>
      /// <param name="_callback">
      ///   event that will be called for each found layer
      /// </param>
      procedure ForEachSubLayer  ( const _callback : TGIS_ForEachLayerEvent
                                 ) ;

      /// <summary>
      ///   Fires BusyPrepare method of the assigned viewer.
      /// </summary>
      /// <param name="_sender">
      ///   component which is causing long-term operations
      /// </param>
      /// <param name="_text">
      ///   text to appear in a BusyText property
      /// </param>
      procedure RaiseBusyPrepare ( const _sender  : TObject ;
                                   const _text    : String
                                 ) ;

      /// <summary>
      ///   Fires BusyRelease method of the assigned viewer.
      /// </summary>
      /// <param name="_sender">
      ///   component which is causing long-term operations
      /// </param>
      procedure RaiseBusyRelease ( const _sender  : TObject
                                 ) ;

      /// <summary>
      ///   Fires BusyShake method of the assigned viewer.
      /// </summary>
      /// <param name="_sender">
      ///   component which is causing long-term operations
      /// </param>
      /// <param name="_pos">
      ///   current progress
      /// </param>
      /// <param name="_end">
      ///   maximal progress value
      /// </param>
      /// <returns>
      ///   True if aborted
      /// </returns>
      function  RaiseBusyShake   ( const _sender  : TObject ;
                                   const _pos     : Int64 ;
                                   const _end     : Int64
                                 ) : Boolean ;

      /// <summary>
      ///   Fires Busy event of the layer.
      /// </summary>
      /// <param name="_sender">
      ///   component which is causing long-term operations
      /// </param>
      /// <param name="_pos">
      ///   current progress
      /// </param>
      /// <param name="_end">
      ///   maximal progress value
      /// </param>
      /// <returns>
      ///   True if aborted
      /// </returns>
      function  RaiseBusyEvent   ( const _sender  : TObject ;
                                   const _pos     : Int64 ;
                                   const _end     : Int64
                                 ) : Boolean ;

      /// <summary>
      ///   Gets a list of layers available in storage.
      /// </summary>
      /// <returns>
      ///   list of layers available in storage
      /// </returns>
      function  GetAvailableLayers : TGIS_LayerInfoList  ; virtual;

      /// <summary>
      ///   Retrieve the layer identified by a name.
      ///   Sublayer or compund layers can be found by do notation like
      ///   'name.sublyer'
      /// </summary>
      /// <param name="_name">
      ///   name of layer to be found
      /// </param>
      /// <returns>
      ///   Found layer or nil if not found.
      /// </returns>
      function  GetSubLayer      ( const _name   : String
                                 ) : TGIS_LayerAbstract ;

      /// <summary>
      ///   Checks if the layer must be reprojected.
      /// </summary>
      /// <returns>
      ///   True if the layer has to be reprojected
      /// </returns>
      function MustReproject     : Integer ;
                                   {$IFDEF DCC}inline ;{$ENDIF} // OXYGENE ERROR
      { TODO : Verify if it's still needed }

      /// <summary>
      ///   Checks if the layer type is vector.
      /// </summary>
      /// <returns>
      ///   True if the layer type is vector
      /// </returns>
      function IsVector          : Boolean ; virtual;

      /// <summary>
      ///   Checks if the layer type is vector 3D.
      /// </summary>
      /// <returns>
      ///   True if the layer type is vector 3D
      /// </returns>
      function IsVector3D        : Boolean ; virtual;

      /// <summary>
      ///   Checks if the layer type is pixel.
      /// </summary>
      /// <returns>
      ///   True if the layer type is pixel
      /// </returns>
      function IsPixel           : Boolean ; virtual;

      /// <summary>
      ///   Checks if the layer type is grid.
      /// </summary>
      /// <returns>
      ///   True if the layer type is grid
      /// </returns>
      function IsGrid            : Boolean ; virtual;

      /// <summary>
      ///   Checks if the layer is visible in a defined extent and current params.
      /// </summary>
      /// <param name="_extent">
      ///   extent to be checked
      /// </param>
      /// <returns>
      ///   True if the layer is visible
      /// </returns>
      function  IsVisible        ( const _extent : TGIS_Extent
                                 ) : Boolean ;

      /// <summary>
      ///   Checks if the operation is supported by the layer.
      /// </summary>
      /// <param name="_operation">
      ///   operation
      /// </param>
      /// <returns>
      ///   True if the operation is supported by the layer
      /// </returns>
      function IsSupported       ( const _operation : TGIS_OperationType
                                 ) : Boolean ; virtual;

      /// <summary>
      ///   Sets a cutting polygon for a layer.
      /// </summary>
      /// <param name="_wkt">
      ///   WKT representation of a polygon
      /// </param>
      /// <remarks>
      ///   <note type="important">
      ///     Polygon must be in the same Coordinate System as the layer.
      ///   </note>
      /// </remarks>
      procedure ApplyCuttingPolygon
                                 (  const _wkt : String
                                 ) ; virtual;

      /// <summary>
      ///   Sets IsModified flag to false for a layer and its sublayers.
      /// </summary>
      procedure ClearModified    ; virtual;

      /// <summary>
      ///   Verifies if all statistics required to properly render the layer are
      ///   available.
      /// </summary>
      /// <returns>
      ///   True if some statistics must be computed.
      /// </returns>
      /// <remarks>
      ///   The function does not consider if the statistics are valid
      ///   (up-to-date). It verifies only that statistics exist.
      /// </remarks>
      function MustCalculateStatistics : Boolean ; virtual;

      /// <summary>
      ///   Sets viewer for the layer and its sublayers.
      /// </summary>
      /// <param name="_viewer">
      ///   viewer to set
      /// </param>
      /// <returns>
      ///   Previous viewer parent
      /// </returns>
      function ViewerReParent    ( const _viewer : TGIS_ViewerRef
                                 ) : TGIS_ViewerRef ;

    // properties for this class
    public

      /// <summary>
      ///   Indicates whether the layer supports auto styling.
      /// </summary>
      /// <remarks>
      ///   Default value of this property depends on the layer type.
      /// </remarks>
      property SupportsAutoStyle : Boolean read FSupportsAutoStyle
                                           write FSupportsAutoStyle ;

      /// <summary>
      ///   Indicates whether the layer supports tiled paint.
      /// </summary>
      property SupportsTiledPaint : Boolean read fget_SupportsTiledPaint ;

      /// <summary>
      ///   Name of layer; if not filled directly, then will be filled with Path
      ///   when added to the viewer. Name must be unique.
      /// </summary>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_LAYEREXIST
      /// </exception>
      property Name : String read fget_Name write fset_Name ;

      /// <summary>
      ///   Additional textual information about the layer such as
      ///   compression, number of pixels, copyright, etc.
      /// </summary>
      property FileInfo : String read fget_FileInfo ;

      /// <summary>
      ///   Copyright information about the layer.
      /// </summary>
      property FileCopyrights : String read fget_FileCopyrights ;

      /// <summary>
      ///   Additional user comments.
      /// </summary>
      property Comments : String read fget_Comments write fset_Comments ;

      /// <summary>
      ///   True if the layer is in locked state.
      /// </summary>
      property IsLocked : Boolean read fget_IsLocked ;

      /// <summary>
      ///   True if the layer is opened and is ready for operations.
      /// </summary>
      property IsOpened : Boolean read fget_IsOpened ;

      /// <summary>
      ///   True if the layer can be interpreted as the topmost (trackable) layer.
      /// </summary>
      property IsTopmost : Boolean read fget_IsTopmost ;

      /// <summary>
      ///   Age of the layer (since its creation).
      /// </summary>
      {$IFDEF CLR}
      property Age :   DateTime read fget_Age ;
      {$ELSE}
      property Age :   TDateTime read fget_Age ;
      {$ENDIF}

      /// <summary>
      ///   Type of sublayer.
      /// </summary>
      property SubType : TGIS_LayerSubTypeSet read fget_SubType ;

      /// <summary>
      ///   True if the layer is exportable. Layer is exportable if it can be
      ///   saved to a disk representation.
      /// </summary>
      property IsExportable : Boolean read fget_IsExportable ;

      /// <summary>
      ///   True if the layer is persistent. The layer is persistent if it can
      ///   be loaded automatically by a project file.
      /// </summary>
      property IsPersistent : Boolean read fget_IsPersistent ;

      /// <summary>
      ///   Reference to a viewer object.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    To properly attach layer to the viewer use TGIS_Viewer.Add.
      ///    </note>
      /// </remarks>
      property Viewer : TGIS_ViewerRef read fget_Viewer write fset_Viewer ;

      /// <summary>
      ///   Configuration file handle.
      /// </summary>
      property ConfigFile : TGIS_ConfigAbstract read fget_ConfigFile ;

      /// <summary>
      ///   Extent of the layer.
      /// </summary>
      property Extent : TGIS_Extent read fget_Extent write fset_Extent ;

      /// <summary>
      ///   Three-dimensional extent of the layer.
      /// </summary>
      property Extent3D : TGIS_Extent3D read fget_Extent3D write fset_Extent3D ;

      /// <summary>
      ///   Extent of the layer in projected units.
      /// </summary>
      property ProjectedExtent : TGIS_Extent read  fget_ProjectedExtent
                                             write fset_ProjectedExtent ;

      /// <summary>
      ///   Checks if the layer is active? Active means layer is visible and
      ///   an object can be localized on the layer.
      /// </summary>
      property Active : Boolean read fget_Active write fset_Active ;

      /// <summary>
      ///   False if the layer should not be visible in legend.
      /// </summary>
      property HideFromLegend : Boolean read  fget_HideFromLegend
                                        write fset_HideFromLegend ;

      /// <summary>
      ///   Checks if the layer is collapsed? Collapsed means only the layer's
      ///   title will be visible inside the legend.
      /// </summary>
      property Collapsed : Boolean read fget_Collapsed write fset_Collapsed ;

      /// <summary>
      ///   True if the layer is direct-mode (bypassing cache).
      /// </summary>
      property DirectMode : Boolean read fget_DirectMode ;

      /// <summary>
      ///   Layer's position relative to other layers.
      ///   For the topmost layers, ZOrder=0.
      ///   If the Layer is not attached to the viewer, then ZOrderEx=-1.
      ///   To set layer as bottommost layer - regardless of the number of
      ///   layers - assign any huge value, like 999999.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///     This property takes into account all layers. To get/set
      ///     order information only for visible (HideFormLegend=False) layers,
      ///     use ZOrderEx property instead.
      ///   </note>
      /// </remarks>
      property ZOrder : Integer read fget_ZOrder write fset_ZOrder ;

      /// <summary>
      ///   Layer position relative to other visible layers.
      ///   For topmost layers ZOrderEx=0.
      ///   If the Layer was not attached to the viewer then ZOrderEx=-1.
      ///   To set layer as bottommost layer - regardless of the number of
      ///   layers - assign any huge value, like 999999.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///     This property takes into account only visible layers
      ///     (HideFromLegend=False). To get/set order information for all
      ///     layers, use ZOrder property instead.
      ///   </note>
      /// </remarks>
      property ZOrderEx : Integer read fget_ZOrderEx write fset_ZOrderEx ;

      /// <summary>
      ///   True if config file is active.
      /// </summary>
      property UseConfig : Boolean read fget_UseConfig write fset_UseConfig ;

      /// <summary>
      ///   Configuration file name. If not filled directly, then will be
      ///   filled with the Path
      /// </summary>
      property ConfigName : String read fget_ConfigName write fset_ConfigName ;

    public

      /// <summary>
      ///   True if the layer upon paint process.
      /// </summary>
      property InPaint : Boolean read  fget_InPaint ;

      /// <summary>
      ///   True if the layer must be interpreted as basemap (for background
      ///   painting). Is important only for bottom most layers.
      /// </summary>
      property Basemap : Boolean read  fget_Basemap
                                 write fset_Basemap ;

      /// <summary>
      ///   False if layer must be painted directly (bypassing cache).
      ///   Important only for topmost layers.
      /// </summary>
      property CachedPaint : Boolean read  fget_CachedPaint
                                     write fset_CachedPaint ;

      /// <summary>
      ///   Transparency value for the layer (0..100).
      /// </summary>
      /// <remarks>
      ///   <list type="bullet">
      ///     <item>
      ///       If, 0 then transparent; if 100, then non transparent.
      ///     </item>
      ///     <item>
      ///       If Addition &lt;&gt; 0, then means: (output = output *
      ///       Transparency/100 + layer * Addition/100). <br />
      ///     </item>
      ///   </list>
      /// </remarks>
      property Transparency : Integer read fget_Transparency
                                      write fset_Transparency ;

      /// <summary>
      ///   Additional value.
      /// </summary>
      /// <remarks>
      ///   <list type="bullet">
      ///     <item>
      ///       If, 0 then Transparency will be used as transparent value
      ///     </item>
      ///     <item>
      ///       If &lt;&gt; 0, then means: (output = output *
      ///       Transparency/100 + layer * Addition/100).
      ///     </item>
      ///   </list>
      /// </remarks>
      property Addition : Integer read fget_Addition write fset_Addition ;

      /// <summary>
      ///   Dormant mode for the layer. Use: gisLayerDormantOff to turn
      ///   off to conserve memory, gisLayerDormantStandard to do standard
      ///   memory management, and gisLayerDormantAgressive to free as much
      ///   memory as possible (can slow down operations).
      /// </summary>
      property DormantMode : TGIS_LayerDormantMode read  fget_DormantMode
                                                   write fset_DormantMode ;

      /// <summary>
      ///   Code Page in which text has been stored. If 0 then viewer (if
      ///   attached) or system default code page will be used.
      /// </summary>
      property CodePage : Integer read fget_CodePage write fset_CodePage ;

      /// <summary>
      ///   List of sublayers belonging to the layer. Used for internal
      ///   presentation of fake layers in some formats on the legend.
      /// </summary>
      property SubLayers : TGIS_LayerAbstractList read  fget_SubLayers
                                                  write fset_SubLayers ;

      /// <summary>
      ///   Parent layer of a sub layer.
      /// </summary>
      property ParentLayer  : TGIS_Layer read  fget_ParentLayer
                                         write fset_ParentLayer ;

      /// <summary>
      ///   Coordinate System that is assigned to the layer. If set to nil, the
      ///   Coordinate System will be turned off.
      /// </summary>
      property CS : TGIS_CSCoordinateSystem read fget_CS write fset_CS ;

      /// <summary>
      ///   Type of multiuser mode.
      /// </summary>
      property MultiUserMode : TGIS_MultiUser read  fget_MultiUserMode
                                              write fset_MultiUserMode ;

      /// <summary>
      ///   True if the layer is read-only.
      /// </summary>
      property IsReadOnly : Boolean read  fget_IsReadOnly ;

      /// <summary>
      ///   Parameters (colors, fills, etc.) object. These parameters are
      ///   as selected in ParamsList property. Changing this parameter also
      ///   changes the selected parameter object in ParamsList.
      /// </summary>
      property Params : TGIS_ParamsSection read  fget_Params
                                           write fset_Params ;

      /// <summary>
      ///   List of all parameter sections attached to the current layer.
      ///   A section describes parameters and its zone of activity
      ///   (minimum zoom of visibility, etc.).
      /// </summary>
      property ParamsList : TGIS_ParamsList read fget_ParamsList ;

      /// <summary>
      ///   <para>
      ///     List of custom, user-defined data. List is saved/restored with
      ///     config/project files.
      ///   </para>
      ///   <para>
      ///     List is in a form of "name=value".
      ///   </para>
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///     <list type="bullet">
      ///       <item>
      ///         It is recommended to use product specific names like:
      ///         "MyProduct.MyValue=some data".
      ///       </item>
      ///       <item>
      ///         Using an ASCII file names without spaces or any
      ///         special characters is recommended.
      ///       </item>
      ///     </list>
      ///   </note>
      /// </remarks>
      property CustomData : TStringList read FCustomData  ;

      /// <summary>
      ///   Custom transformation.
      /// </summary>
      {#ownership:set:release}
      property Transform : TGIS_Transform
                           read  fget_Transform
                           write fset_Transform ;

      /// <summary>
      ///   Maximum allowed tile size for the layer. Default is no limit
      ///   and is represented by GIS_MAX_INTEGER value.
      /// </summary>
      property MaxTileSize    : {$IFDEF CLR}
                                  TPoint
                                {$ELSE}
                                  TPoint
                                {$ENDIF}
                                read FMaxTileSize ;

      /// <summary>
      ///   Handle to a renderer used for drawing shapes.
      /// </summary>
      property Renderer : TObject
                           read  fget_Renderer
                           write fset_Renderer ;

      /// <summary>
      ///   Set of operations not supported by the layer.
      /// </summary>
      property UnSupportedOperations : TGIS_OperationTypes
                                     read  FUnSupportedOperations
                                     write FUnSupportedOperations ;

      /// <summary>
      ///   If True, the layer at the moment is drawn inside a basemap thread.
      /// </summary>
      property BasemapDraw : Boolean read  FBasemapDraw
                                     write FBasemapDraw ;

      /// <summary>
      ///   If True, the layer at the moment is drawn by tiler.
      /// </summary>
      property TiledDrawMode    :   TGIS_LayerTiledMode
                                    read  FTiledDrawMode
                                    write FTiledDrawMode ;
    published //events

      /// <summary>
      ///   PaintLayer event. When attached, Draw method will not be
      ///   called internally. Use it to make custom actions when
      ///   drawing whole shapes.
      /// </summary>
      {$IFDEF CLR}
        event PaintLayerEvent    : TGIS_LayerEvent
                                   delegate FOnPaintLayer ;
      {$ELSE}
        /// <event/>
        property PaintLayerEvent : TGIS_LayerEvent
                                   read  FOnPaintLayer
                                   write FOnPaintLayer ;
      {$ENDIF}

      /// <summary>
      ///   Password event. Will be fired upon opening layer to resolve
      ///   any username/password as a key/value pair. Supported only on
      ///   selected layers.
      /// </summary>
      {$IFDEF CLR}
        event PasswordEvent      : TGIS_TemplateProducerEvent
                                   delegate FOnPassword ;
      {$ELSE}
        /// <event/>
        property PasswordEvent   : TGIS_TemplateProducerEvent
                                   read  FOnPassword
                                   write FOnPassword ;
      {$ENDIF}

      /// <summary>
      ///   Read event. Will be fired upon file Read request.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Should be set before layer will be opened; for example upon
      ///    TGIS_Viewer.LayerAddEvent
      ///    </note>
      ///   Currently supported only by TGIS_LayerSHP
      /// </remarks>
      {$IFDEF CLR}
        event ReadEvent          : TGIS_ReadWriteEvent
                                   delegate FOnRead ;
      {$ELSE}
        /// <event/>
        property ReadEvent       : TGIS_ReadWriteEvent
                                   read  FOnRead
                                   write FOnRead ;
      {$ENDIF}

      /// <summary>
      ///   Write event. Will be fired upon each file Write request.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Should be set before layer will be opened; for example upon
      ///    TGIS_Viewer.LayerAddEvent
      ///    </note>
      ///   Currently supported only by TGIS_LayerSHP
      /// </remarks>
      {$IFDEF CLR}
        event WriteEvent         : TGIS_ReadWriteEvent
                                   delegate FOnWrite ;
      {$ELSE}
        /// <event/>
        property WriteEvent      : TGIS_ReadWriteEvent
                                   read  FOnWrite
                                   write FOnWrite ;
      {$ENDIF}

      /// <summary>
      ///   Busy event. Will be fired regularly during long-drawn
      ///   operations. If end value will be zero, the meaning is:
      ///   long-drawn with unknown end time. Close long-drawn operation
      ///   by calling with parameters (-1,-1).
      /// </summary>
      {$IFDEF CLR}
        event BusyEvent    : TGIS_BusyEvent delegate FOnBusy ;
      {$ELSE}
        /// <event/>
        property BusyEvent : TGIS_BusyEvent read  FOnBusy
                                            write FOnBusy ;
      {$ENDIF}
  end ;
  {$ENDREGION}

  {$REGION 'TGIS_LayerGroup'}
  //============================================================================

  TGIS_RegisteredLayerAbstract = class of TGIS_Layer ;

  /// <summary>
  ///   General group layer class.
  /// </summary>
  TGIS_LayerGroup = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Layer )

    protected

      /// <summary>
      ///   Destroy a layer instance.
      /// </summary>
      procedure doDestroy  ; override;

    // constructor
    public

      /// <summary>
      ///   Create layer.
      /// </summary>
      constructor  Create  ; override;

    public
      /// <inheritdoc/>
      function  DrawEx            ( const _extent : TGIS_Extent
                                  ) : Boolean ; override;

      /// <inheritdoc/>
      procedure DrawFlash         ; override;

      /// <inheritdoc/>
      procedure RevertAll         ; override;

      /// <inheritdoc/>
      procedure ReadConfig        ; override;

      /// <inheritdoc/>
      procedure WriteConfig       ; override;
  end ;
  {$ENDREGION}

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisClasses,
    GisCsFactory,
    GisFunctions,
    GisInternals,
    GisLayerProject,
    GisRendererAbstract,
    GisResource,
    GisSqlQuery,
    GisViewer ;
{$ENDIF}


{$REGION 'TGIS_LayerEventArgs'}
//==============================================================================
{$IFDEF OXYGENE}

  constructor TGIS_LayerEventArgs.Create( const _layer : TGIS_Layer
                                        ) ;
  begin
    inherited Create ;
    FLayer := _layer ;
  end ;

{$ENDIF}
{$ENDREGION}

{$REGION 'TGIS_Layer'}
//==============================================================================

  constructor TGIS_Layer.Create ;
  begin
    inherited ;
    TagPointer := nil ;

    FIsLocked            := False ;
    FIsPrepared          := False ;
    FIsOpened            := False ;
    FHideFromLegend      := False ;

    FSubType             := []    ;

    FComments            := ''    ;
    FMultiUserMode       := TGIS_MultiUser.Default ;
    FActive              := True  ;
    FUseConfig           := True  ;
    FBasemap             := False ;
    FBasemapDraw         := False ;
    FTiledDrawMode       := TGIS_LayerTiledMode.None ;
    FCachedPaint         := True  ;
    FTransparency        := 100   ;
    FAddition            := 0     ;
    FDormantMode         := TGIS_LayerDormantMode.Standard ;
    FExtent              := GisNoWorld ;
    FExtent3D            := GisNoWorld3D ;
    FUserObject          := nil   ;

    CodePage             := 0 ;
    FCodePageForced      := 0 ;

    FCS                  := CSUnknownCoordinateSystem ;

    FAge                 := EncodeDateTime( 1899, 12, 30, 23, 59, 59, 99 ) ;

    FParamsList          := TGIS_ParamsList.Create ;

    FCustomData          := TGIS_StringList.Create ;

    FParamsList.Layer    := self ;


    paramsCache          := TList< TGIS_ParamsSection >.Create ;
    paramsCacheUpdated   := False ;

    forceCachedMode      := False ;
    inDraw               := False ;
    FIsModified          := False ;
    FIsReadOnly          := False ;

    FTransform           := nil ;

    FMaxTileSize         := Point( GIS_MAX_INTEGER, GIS_MAX_INTEGER );

    View3D.ModeDefault   := True ;
    View3D.Mode          := TGIS_3DLayerType.Off ;

    lastHourglassState   := GetTickCount ;

    {$IFDEF JAVA}
      FProjectedExtentRotationPoint := new TGIS_Point ;
      FProjectedExtentBase := new TGIS_Extent ;
      FProjectedExtent     := GisNoWorld ;
    {$ENDIF}

    FStatistics := TGIS_StatisticsFactory.CreateStatistics( Self ) ;

    criticalSection := TCriticalSection.Create ;

    modifiedCS         := False ;
    modifiedCodePage   := False ;

    FSupportsAutoStyle  := True ;
    FSupportsTiledPaint := True ;
  end ;

  {$IFDEF EXPERIMENTAL}
    constructor TGIS_Layer.Create(
      const _path : String
    ) ;
    begin
      Create ;
      Path := _path ;
      Open ;
    end;
  {$ENDIF}

  procedure TGIS_Layer.doDestroy ;
  var
    i  : Integer ;
    la : TGIS_Layer ;
  begin
    FreeObject( FConfigFile ) ;

    FreeObject( FParamsList ) ;

    FreeObject( FCustomData ) ;

    FreeObject( paramsCache ) ;

    if assigned( FSubLayers ) then
      for i := 0 to FSubLayers.Count-1 do begin
        la := TGIS_Layer( FSubLayers[i] ) ;
        FreeObject( la ) ;
      end;
    FreeObject( FSubLayers  ) ;

    FreeObject( FUserObject ) ;

    FreeObject( FTransform  ) ;

    FreeObject( FStatistics ) ;

    FreeObject( criticalSection ) ;

    inherited ;
  end ;

  function TGIS_Layer.MustReproject
    : Integer ;
  begin
    if csctxMustRepoject = 0 then begin
      if assigned( Viewer ) then begin
        csctxViewer := Viewer.Ref ;
        csctxViewerCS := csctxViewer.CS ;
        csctxRotationAngle := csctxViewer.RotationAngle ;
      end
      else begin
        csctxViewer := nil ;
        csctxViewerCS := nil ;
        csctxRotationAngle := 0 ;
      end;

      csctxMustRepoject := -1 ;

      if assigned( Transform ) and Transform.Active then
        csctxMustRepoject := 1 ;
      if csctxRotationAngle <> 0 then
        csctxMustRepoject := 1 ;

      if assigned( Viewer ) and
         ( csctxViewerCS.EPSG <> 0       ) and
         ( CS.EPSG            <> 0       ) and
         ( csctxViewerCS.EPSG <> CS.EPSG )
      then
        csctxMustRepoject := 2 ;
    end;

    Result :=  csctxMustRepoject ;
  end;

  function TGIS_Layer.IsVector : Boolean ;
  begin
    Result := False ;
  end ;

  function TGIS_Layer.IsVector3D : Boolean ;
  begin
    Result := False ;
  end ;

  function TGIS_Layer.IsPixel : Boolean ;
  begin
    Result := False ;
  end ;

  function TGIS_Layer.IsGrid : Boolean ;
  begin
    Result := False ;
  end ;

  function TGIS_Layer.IsVisible(
    const _extent : TGIS_Extent
  ) : Boolean ;
  var
    i   : Integer ;
    sec : TGIS_ParamsSection ;
  begin
    Result := Active and GisIsCommonExtent( ProjectedExtent, _extent ) ;

    if not Result then exit ;

    for i:=0 to ParamsList.Count - 1 do begin
      sec := ParamsList.Items[ i ] ;

      if not isSectionInVisibilityRange( sec ) then continue ;

      exit ; // Found!
    end;

    Result := False ;
  end ;

  function TGIS_Layer.IsSupported(
    const _operation : TGIS_OperationType
  ) : Boolean  ;
  begin
    if not (_operation in FUnSupportedOperations) then
      Result := True
    else
      Result := False ;
  end ;

  function TGIS_Layer.fget_Path
    : String ;
  begin
    Result := FPath ;
  end ;

  procedure TGIS_Layer.fset_Path(
    const _value : String
  ) ;
  var
    k : Integer ;
  begin
    if not IsServerPath( _value ) then begin
      k := Pos( '?', _value ) ;
      if k < StringFirst then
        FPath := _value
      else begin
        FPath   := Copy( _value, StringFirst, k - StringFirst ) ;
        FDriver := Copy( _value, k+1, MaxInt        ) ;
      end ;
    end
    else
      FPath := _value ;
  end ;

  function TGIS_Layer.fget_PathWithDriver
    : String ;
  begin
    if IsStringEmpty( FDriver ) then
      Result := FPath
    else
      Result := Format( '%s?%s', [FPath, FDriver] ) ;
  end ;

  procedure TGIS_Layer.fset_Statistics(
    const _value : TGIS_StatisticsAbstract
  ) ;
  begin
    if assigned( _value ) and ( TGIS_StatisticsLayer( _value ).Layer =  Self ) then begin
      FreeObject( FStatistics ) ;
      FStatistics := _value ;
    end ;
  end ;

  function TGIS_Layer.fget_Stream
    : TStream ;
  begin
    Result := FStream ;
  end ;

  procedure TGIS_Layer.fset_Stream(
    const _value : TStream
  ) ;
  begin
    FStream := _value ;
  end ;

  function TGIS_Layer.fget_Caption
    : String ;
  begin
    Result := FCaption ;
  end ;

  procedure TGIS_Layer.fset_Caption(
    const _value : String
  ) ;
  begin
    FCaption := _value ;
  end ;

  function TGIS_Layer.fget_Tag
    : Integer ;
  begin
    Result := FTag ;
  end ;

  procedure TGIS_Layer.fset_Tag(
    const _value : Integer
  ) ;
  begin
    FTag := _value ;
  end ;

  {$IFDEF MANAGED}
    function TGIS_Layer.fget_TagPointer
      : Object ;
    begin
      Result := FTagPointer ;
    end ;

    procedure TGIS_Layer.fset_TagPointer(
      const _value : Object
    ) ;
    begin
      FTagPointer := _value ;
    end ;
  {$ELSE}

    function TGIS_Layer.fget_TagPointer
      : Pointer ;
    begin
      Result := FTagPointer ;
    end ;

    procedure TGIS_Layer.fset_TagPointer(
      const _value : Pointer
    ) ;
    begin
      FTagPointer := _value ;
    end ;
  {$ENDIF}

  function TGIS_Layer.fget_TagInternal
    : Integer ;
  begin
    Result := FTagInternal ;
  end ;

  procedure TGIS_Layer.fset_TagInternal(
    const _value : Integer
  ) ;
  begin
    FTagInternal := _value ;
  end ;

  function TGIS_Layer.fget_UserObject
    : TObject ;
  begin
    Result := FUserObject ;
  end ;

  procedure TGIS_Layer.fset_UserObject(
    const _value : TObject
  ) ;
  begin
    FreeObject( FUserObject ) ;
    FUserObject := _value ;
  end ;

  function TGIS_Layer.fget_Name
    : String ;
  begin
    Result := FName ;
  end ;

  procedure TGIS_Layer.fset_Name(
    const _value : String
  ) ;
  begin
    {$IFDEF OXYGENE}
    if not assigned( _value ) then exit ;
    {$ENDIF}
    if CompareText( FName, _value ) = 0 then exit ;

    if assigned( Viewer ) and ( Viewer.Ref.Get( _value ) <> nil ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYEREXIST ), _value, 0 )
    else
      FName := _value ;
  end ;

  function TGIS_Layer.fget_FileInfo
    : String ;
  begin
    Result := FFileInfo ;
  end ;

  function TGIS_Layer.fget_FileCopyrights
    : String ;
  begin
    Result := FFileCopyright ;
  end ;

  function TGIS_Layer.fget_Comments
    : String ;
  begin
    Result := FComments ;
  end ;

  procedure TGIS_Layer.fset_Comments(
    const _value : String
  ) ;
  begin
    FComments := _value ;
  end ;

  function TGIS_Layer.fget_IsLocked
    : Boolean ;
  begin
    Result := FIsLocked ;
  end ;

  function TGIS_Layer.fget_IsOpened
    : Boolean ;
  begin
    Result := FIsOpened ;
  end ;

  function TGIS_Layer.fget_IsTopmost
    : Boolean ;
  var
    i  : Integer ;
    la : TGIS_Layer ;
  begin
    Result := False ;
    if CachedPaint then
      exit ;

    if assigned( Viewer ) then begin
      for i := Viewer.Ref.Items.Count -1 downto 0 do begin
        la := TGIS_Layer( Viewer.Ref.Items[ i ] ) ;

        if la.CachedPaint then
          break ;

        if la = self then begin
          Result := True ;
          break ;
        end;
      end;
    end ;
  end ;

  function TGIS_Layer.fget_DirectMode
    : Boolean ;
  var
    i       : Integer ;
    topmost : Boolean ;
    la      : TGIS_Layer ;
  begin

    topmost := False ;
    if assigned( Viewer ) then begin
      for i := Viewer.Ref.Items.Count -1 downto 0 do begin
        la := TGIS_Layer( Viewer.Ref.Items[i] ) ;

        if la.CachedPaint then
          break
        else
          topmost := True ;

        if la = self then
          break ;
      end;
    end;

    Result := Active                 and
              assigned( Viewer )     and
              (not CachedPaint)      and
              topmost ;
  end ;

  {$IFDEF CLR}
    function TGIS_Layer.fget_Age
      : DateTime ;
    begin
      Result := FAge ;
    end ;
  {$ELSE}

    function TGIS_Layer.fget_Age
      : TDateTime ;
    begin
      Result := FAge ;
    end ;
  {$ENDIF}

  function TGIS_Layer.fget_SubType
  : TGIS_LayerSubTypeSet ;
  begin
    Result := FSubType ;
  end ;

  function TGIS_Layer.fget_IsExportable
    : Boolean ;
  begin
    Result := TGIS_LayerSubType.Exportable in SubType ;
  end ;

  function TGIS_Layer.fget_IsPersistent
    : Boolean ;
  begin
    Result := TGIS_LayerSubType.Persistent in SubType ;
  end ;

  function TGIS_Layer.fget_Viewer
    : TGIS_ViewerRef ;
  begin
    Result := FViewer ;
  end ;

  procedure TGIS_Layer.fset_Viewer(
    const _value : TGIS_ViewerRef
  ) ;
  begin
    FViewer := _value ;
  end ;

  function TGIS_Layer.fget_ConfigFile
    : TGIS_ConfigAbstract ;
  begin
    Result := FConfigFile ;
  end ;

  function TGIS_Layer.fget_Extent
    : TGIS_Extent ;
  begin
    Result := FExtent ;
  end ;

  procedure TGIS_Layer.fset_Extent(
    const _value : TGIS_Extent
  ) ;
  begin
    if GisIsSameExtent( FExtent, _value ) then exit ;

    FExtent   := _value ;

    FExtent3D.XMin := _value.XMin ;
    FExtent3D.YMin := _value.YMin ;
    FExtent3D.XMax := _value.XMax ;
    FExtent3D.YMax := _value.YMax ;

    // no world
    if FExtent3D.ZMax < FExtent3D.ZMin then begin
      FExtent3D.ZMin := 0 ;
      FExtent3D.ZMax := 0 ;
    end ;

    FProjectedExtent := _value ;
    if assigned( Viewer ) and (not FIsLocked) then
      Viewer.Ref.RecalcExtent ;
  end ;

  function TGIS_Layer.fget_Extent3D
    : TGIS_Extent3D ;
  begin
    Result := FExtent3D ;
  end ;

  procedure TGIS_Layer.fset_Extent3D(
    const _value : TGIS_Extent3D
  ) ;
  begin
    FExtent3D := _value ;

    Extent := GisExtent2DFrom3D( _value ) ;
  end ;

  function TGIS_Layer.fget_ProjectedExtent
    : TGIS_Extent ;
  begin
    Result := FProjectedExtent ;
  end ;

  procedure TGIS_Layer.fset_ProjectedExtent(
    const _value : TGIS_Extent
  ) ;
  begin
    FProjectedExtent := _value ;
  end ;

  function TGIS_Layer.fget_Active
    : Boolean ;
  begin
    Result := FActive ;
  end ;

  procedure TGIS_Layer.fset_Active(
    const _value : Boolean
  ) ;
  begin
    FActive := _value ;
    if _value and FIsPrepared then
      Open ;
  end ;

  function TGIS_Layer.fget_HideFromLegend
    : Boolean ;
  begin
    Result := FHideFromLegend ;
  end ;

  procedure TGIS_Layer.fset_HideFromLegend(
    const _value : Boolean
  ) ;
  begin
    FHideFromLegend := _value ;
  end ;

  function TGIS_Layer.fget_Collapsed
    : Boolean ;
  begin
    Result := FCollapsed ;
  end ;

  procedure TGIS_Layer.fset_Collapsed(
    const _value : Boolean
  ) ;
  begin
    FCollapsed := _value ;
  end ;

  function TGIS_Layer.fget_ZOrder
    : Integer ;
  begin
    if assigned( Viewer ) then begin
      if isSublayer then
        Result := (ParentLayer.SubLayers.Count - 1 )- ParentLayer.SubLayers.IndexOf( Self )
      else
        Result := (Viewer.Ref.Items.Count - 1 )- Viewer.Ref.Items.IndexOf( Self )
    end
    else
      Result := -1 ;
  end ;

  procedure TGIS_Layer.fset_ZOrder(
    const _value : Integer
  ) ;
  var
    itms   : TGIS_LayerAbstractList ;
    org    : Integer     ;
    ipos   : Integer     ;
    cnt    : Integer     ;
  begin
    if not assigned( Viewer ) then exit ;

    if _value = ZOrder then exit ;

    if isSublayer then
      itms := ParentLayer.SubLayers
    else
      itms := Viewer.Ref.Items ;

    org := itms.IndexOf( Self ) ;
    cnt := itms.Count - 1 ;
    ipos := cnt - _value ;

    if ipos < 0 then ipos := 0 ;

    if ipos > cnt then ipos := cnt ;

    itms.Move( org, ipos ) ;

    if org <> ZOrder then
      Viewer.Ref.MarkModified;
  end ;

  function TGIS_Layer.fget_ZOrderEx
    : Integer ;
  var
    i     : Integer ;
    cnt   : Integer ;
    lpos  : Integer ;
    found : Boolean ;
    la    : TGIS_Layer ;
    itms  : TGIS_LayerAbstractList ;

  begin
    if assigned( Viewer ) then begin
      cnt   := 0     ;
      lpos  := -1    ;
      found := False ;

      if isSublayer then
        itms := ParentLayer.SubLayers
      else
        itms := Viewer.Ref.Items ;

      for i:=0 to (itms.Count - 1 ) do begin
        la := TGIS_Layer( itms[i] ) ;
        if la = Self then
          found := True ;
        if not ( la.HideFromLegend ) and ( la.TagInternal = 0 ) then begin
          inc( cnt ) ;

          if not found then begin
             // make anchor for the case when active layer is not visible
             // then previous visible layer will be used
            lpos := cnt ;
          end ;

          if la = Self then begin
            lpos  := cnt  ;
          end ;
        end ;
      end ;

      Result := cnt - lpos ;
    end
    else
      Result := -1 ;
  end ;

  procedure TGIS_Layer.fset_ZOrderEx(
    const _value : Integer
  ) ;
  var
    zo   : Integer ;
    zox  : Integer ;
    zmin : Integer ;
    zmax : Integer ;
    ar   : array of TGIS_Layer ;
    itms : TGIS_LayerAbstractList ;

    procedure prepare_layer ;
    var
      i1, j1, k1 : Integer ;
      la : TGIS_Layer ;

    begin
      j1 := -1 ;

      // find max ZOrder
      zmin := 0  ;
      zmax := -1 ;
      for i1 := itms.Count - 1 downto 0 do begin
        la := TGIS_Layer( itms[i1] ) ;
        if not ( la.HideFromLegend ) and ( la.TagInternal = 0 ) then begin
          inc( zmax ) ;
        end ;
      end ;

      // find anchor layer
      k1 := -1 ;
      for i1 := itms.Count - 1 downto 0 do begin
        la := TGIS_Layer( itms[i1] ) ;
        if not ( la.HideFromLegend ) and ( la.TagInternal = 0 ) then begin
          inc( j1 ) ;
        end ;

        if j1 = zox then begin
          SetLength( ar, length( ar ) + 1 ) ;
          ar[ length( ar ) -1 ] := la ;
          zo := la.ZOrder ;
          k1 := i1 ;
          break ;
        end
      end ;

      if k1 < 0 then exit ;
      if zo < 0 then exit ;

      for i1 := k1+1 to itms.Count - 1 do begin
        la := TGIS_Layer( itms[i1] ) ;
        if not ( la.HideFromLegend ) and ( la.TagInternal = 0 ) then
          break ;

        SetLength( ar, length( ar ) + 1 ) ;
        ar[ length( ar ) -1 ] := la ;
      end ;
    end ;

    function move_up : Boolean ;
    var
      i1, m1, d1 : Integer ;
      la : TGIS_Layer ;
    begin
      Result := False ;

      // find previous visible layer
      m1 := -1 ;
      for i1 := itms.Count - zo - 2 downto 0 do begin
        la := TGIS_Layer( itms[i1] ) ;
        if not ( la.HideFromLegend ) and ( la.TagInternal = 0 ) then begin
          m1 := la.ZOrder ;
          break ;
        end ;
      end ;

      if m1 < 0 then exit ;

      // move all layers individually
      d1 := m1 - zo ;
      for i1 := 0 to length( ar ) - 1 do begin
         ar[i1].Move( d1 ) ;
      end ;

      zo := ar[0].ZOrder ;

      Result := True ;
    end ;

    function move_down : Boolean ;
    var
      i1,k1, m1, d1 : Integer ;
      la : TGIS_Layer ;
    begin
      Result := False ;

      // find next visible layer
      k1 := -1 ;
      m1 := -1 ;
      for i1 := itms.Count - zo to itms.Count -1 do begin
        la := TGIS_Layer( itms[i1] ) ;
        if not ( la.HideFromLegend ) and ( la.TagInternal = 0 ) then begin
          m1 := la.ZOrder ;
          k1 := i1 ;
          break ;
        end ;
      end ;

      if m1 < 0 then exit ;

      // skip any sublayers
      for i1 := k1 +1 to itms.Count -1 do begin
        la := TGIS_Layer( itms[i1] ) ;
        if la.HideFromLegend or ( la.TagInternal <> 0 ) then
          m1 := la.ZOrder
        else
          break ;
      end ;

      if m1 < 0 then exit ;

      // move all layers individually
      d1 := m1 - zo;
      for i1 := 0 to length( ar ) - 1 do begin
         ar[i1].Move( d1 ) ;
      end ;

      zo := ar[0].ZOrder ;

      Result := True ;
    end ;

  begin
    if not assigned( Viewer ) then exit ;

    zox := ZOrderEx ;
    if _value = zox then exit ;

    if isSublayer then
      itms := ParentLayer.SubLayers
    else
      itms := Viewer.Ref.Items ;

    // prepare list of all layers to be moved
      prepare_layer ;
      if zo < 0 then exit ;

    // move layers
      if zox < _value then begin
       while true do begin
         if ZOrderEx >= Min( zmax, _value ) then
           break ;
         if not move_up then
           break ;
       end ;

      end
      else begin
       while true do begin
         if ZOrderEx <= Max( zmin, _value ) then
           break ;
         if not move_down then
           break ;
       end ;
      end ;
  end ;

  function TGIS_Layer.fget_UseConfig
    : Boolean ;
  begin
    Result := FUseConfig ;
  end ;

  procedure TGIS_Layer.fset_UseConfig(
    const _value : Boolean
  ) ;
  begin
    FUseConfig := _value ;
  end ;

  function TGIS_Layer.fget_ConfigName
    : String ;
  begin
    Result := FConfigName ;
  end ;

  procedure TGIS_Layer.fset_ConfigName(
    const _value : String
  ) ;
  var
    cname : String ;
  begin
    if assigned( FConfigFile ) and
       ( CompareText( Trim(FConfigName), Trim(_value) ) = 0 ) then exit ;

    FreeObject( FConfigFile ) ;
    FConfigName := Trim( _value ) ;

    if ( IsStringEmpty( ConfigName ) or IsEmbeddedSQLPath( ConfigName ) or
          ( Pos ( '://', ConfigName ) >= StringFirst ) or ( Pos ( '{', ConfigName ) >= StringFirst ) ) and
        (TGIS_LayerSubType.Persistent in FSubType)
    then
      FConfigName := GIS_CONFIG_DUMMY ;

    if not IsStringEmpty( FConfigName ) then begin
      if (CompareText( ExtractFileExt(ConfigName), GIS_INI_EXT      ) <> 0) and
         (CompareText( ExtractFileExt(ConfigName), GIS_TTKSTYLE_EXT ) <> 0) then
      begin
        cname := ConfigName + GIS_TTKSTYLE_EXT ;
        if not FileExists( cname ) then
          cname := ConfigName + GIS_INI_EXT ;

        FConfigName := cname ;
      end ;
    end ;

    if FileExists( ConfigName ) then
      FConfigFile := TGIS_ConfigFactory.CreateConfig( Self, ConfigName )

  end ;

  function TGIS_Layer.fget_InPaint
    : Boolean ;
  begin
    Result := FInPaint ;
  end ;

  function TGIS_Layer.fget_Basemap
    : Boolean ;
  begin
    Result := FBasemap ;
  end ;

  procedure TGIS_Layer.fset_Basemap(
    const _value : Boolean
  ) ;
  begin
    if _value = FBasemap then
      exit ;

    FBasemap := _value ;
    if assigned( Viewer ) and (not FIsLocked) then
      Viewer.Ref.RecalcExtent ;
  end ;

  function TGIS_Layer.fget_CachedPaint
    : Boolean ;
  begin
    Result := FCachedPaint ;
  end ;

  procedure TGIS_Layer.fset_CachedPaint(
    const _value : Boolean
  ) ;
  begin
    FCachedPaint := _value ;
  end ;

  function TGIS_Layer.fget_Transparency
    : Integer ;
  begin
    Result := FTransparency ;
  end ;

  procedure TGIS_Layer.fset_Transparency(
    const _value : Integer
  ) ;
  begin
    FTransparency := Min( Max( 0, _value ), 100 ) ;
  end ;

  function TGIS_Layer.fget_Addition
    : Integer ;
  begin
    Result := FAddition ;
  end ;

  procedure TGIS_Layer.fset_Addition(
    const _value : Integer
  ) ;
  begin
    FAddition := Min( Max( 0, _value ), 100 ) ;
  end ;

  function TGIS_Layer.fget_DormantMode
    : TGIS_LayerDormantMode ;
  begin
    Result := FDormantMode ;
  end ;

  procedure TGIS_Layer.fset_DormantMode(
    const _value : TGIS_LayerDormantMode
  ) ;
  begin
    FDormantMode := _value ;
  end ;

  function TGIS_Layer.fget_CodePage
    : Integer ;
  begin
    Result := FCodePage ;
    if Result > 0 then exit ;

    if Result = 0 then
      Result := GisSystemCodePage ;
  end ;

  procedure TGIS_Layer.fset_CodePage(
    const _value : Integer
  ) ;
  begin
    FCodePage         := _value ;
    FCodePageForced   := _value ;
    modifiedCodePage  := True   ;
  end ;

  function TGIS_Layer.fget_SubLayers
    : TGIS_LayerAbstractList ;
  begin
    Result := FSubLayers ;
  end ;

  procedure TGIS_Layer.fset_SubLayers(
    const _value : TGIS_LayerAbstractList
  ) ;
  begin
    FSubLayers := _value ;
  end ;

  function TGIS_Layer.fget_ParentLayer
    : TGIS_Layer ;
  begin
    Result := FParentLayer ;
  end ;

  procedure TGIS_Layer.fset_ParentLayer(
    const _value : TGIS_Layer
  ) ;
  begin
    FParentLayer := _value ;
  end ;

  function TGIS_Layer.fget_CS
    : TGIS_CSCoordinateSystem ;
  begin
    Result := FCS ;
  end ;

  procedure TGIS_Layer.fset_CS(
    const _value : TGIS_CSCoordinateSystem
  ) ;
  var
    tmp_cs : TGIS_CSCoordinateSystem ;
  begin
    if assigned( _value ) then
      tmp_cs := _value
    else
      tmp_cs := CSUnknownCoordinateSystem ;

    if FCS.EPSG = tmp_cs.EPSG then
      exit ;

    FCS := tmp_cs ;

    if assigned( Viewer )then
      Viewer.Ref.RecalcExtent ;

    modifiedCS := True ;
  end ;

  function TGIS_Layer.fget_MultiUserMode
    : TGIS_MultiUser ;
  begin
    Result := FMultiUserMode ;
  end ;

  procedure TGIS_Layer.fset_MultiUserMode(
    const _value : TGIS_MultiUser
  ) ;
  begin
    FMultiUserMode := _value ;
  end ;

  function TGIS_Layer.fget_IsReadOnly
    : Boolean ;
  begin
    Result := FIsReadOnly ;
  end ;

  function TGIS_Layer.fget_Params
    : TGIS_ParamsSection ;
  begin
    Result := ParamsList.SelectedObj ;
    Result.Shape := nil ;
  end ;

  procedure TGIS_Layer.fset_Params(
    const _value : TGIS_ParamsSection
  ) ;
  begin
    ParamsList.SelectedObj.Assign( _value ) ;
  end ;

  function TGIS_Layer.fget_Transform
    : TGIS_Transform ;
  begin
    Result := FTransform ;
  end ;

  procedure TGIS_Layer.fset_Transform(
    const _value : TGIS_Transform
  ) ;
  begin
    if _value = FTransform then exit ;

    FreeObject( FTransform ) ;
    FTransform := _value ;

    if assigned( FTransform ) then
      ApplyCuttingPolygon( FTransform.CuttingPolygon )
    else
      ApplyCuttingPolygon( '' ) ;

    if assigned( Viewer ) then
      Viewer.Ref.RecalcExtent ;
  end ;

  function TGIS_Layer.fget_ParamsList
    : TGIS_ParamsList ;
  begin
    Result := FParamsList ;
  end ;

  function TGIS_Layer.fget_Renderer
    : TObject ;
  begin
    Result := FRenderer ;
  end ;

  procedure TGIS_Layer.fset_Renderer(
    const _value : TObject
  ) ;
  var
    i : Integer ;
  begin
    if _value = FRenderer then exit ;

    FRenderer := _value ;

    if assigned( SubLayers ) then
      for i := 0 to SubLayers.Count - 1 do
        TGIS_Layer(SubLayers[i]).Renderer := FRenderer ;
  end ;

  procedure TGIS_Layer.fset_View3DMode(
    const _value : TGIS_3DLayerType
  ) ;
  begin
    if View3D.ModeDefault then begin
      View3D.Mode := _value ;
      View3D.ModeDefault := False ;
    end ;
  end;

  function TGIS_Layer.fget_SupportsTiledPaint
    : Boolean;
  begin
    Result := FSupportsTiledPaint;
  end;

  function TGIS_Layer.isSublayer : Boolean ;
  begin
    Result := assigned( ParentLayer ) and assigned( ParentLayer.SubLayers ) ;
  end ;

  procedure TGIS_Layer.resetMustReproject ;
  begin
    csctxMustRepoject := 0 ;
  end;

  function TGIS_Layer.drawExtent: TGIS_Extent ;
  var
    {$IFDEF CLR}
      rct : System.Drawing.Rectangle  ;
    {$ELSE}
      rct : TRect  ;
    {$ENDIF}
  begin
    if not assigned( Viewer ) then exit ;

    with Viewer do begin
      assert( Ref.Zoom > 0, 'Zoom must be > 0' ) ;

      rct := TGIS_RendererAbstract(
               Viewer.Ref.ViewerParent.ControlRenderer
             ).ViewRect ;
      {$IFDEF GIS_NORECORDS}
        Result := new TGIS_Extent ;
      {$ENDIF}
      Result.XMin := Extent.XMin + Ref.Viewport.X +
                     ( rct.Left   / Ref.Zoom ) ;
      Result.XMax := Extent.XMin + Ref.Viewport.X +
                     ( rct.Right  / Ref.Zoom ) ;
      Result.YMin := Extent.YMax - Ref.Viewport.Y -
                     ( rct.Bottom / Ref.Zoom ) ;
      Result.YMax := Extent.YMax - Ref.Viewport.Y -
                     ( rct.Top    / Ref.Zoom ) ;
    end ;
  end ;

  function TGIS_Layer.drawExtentEx: TGIS_Extent ;
  var
    {$IFDEF CLR}
      rct : TRect ;
    {$ELSE}
      rct : TRect ;
    {$ENDIF}
  begin
    with Viewer do begin
      rct := TGIS_RendererAbstract(
               Viewer.Ref.ViewerParent.ControlRenderer
             ).ViewRect ;
      {$IFDEF GIS_NORECORDS}
        Result := new TGIS_Extent ;
      {$ENDIF}
      Result.XMin := Ref.Extent.XMin + Ref.Viewport.X +
                     ( rct.Left   - Ref.OverlappedExtentMargin ) / Ref.Zoom ;
      Result.XMax := Ref.Extent.XMin + Ref.Viewport.X +
                     ( rct.Right  + Ref.OverlappedExtentMargin ) / Ref.Zoom ;
      Result.YMin := Ref.Extent.YMax - Ref.Viewport.Y -
                     ( rct.Bottom + Ref.OverlappedExtentMargin ) / Ref.Zoom ;
      Result.YMax := Ref.Extent.YMax - Ref.Viewport.Y -
                     ( rct.Top    - Ref.OverlappedExtentMargin ) / Ref.Zoom ;

      if GisIsCommonExtent( Result, Ref.Extent ) then
        Result := GisCommonExtent( Result, Ref.Extent )
      else
        Result := GisNoWorld ;

      if not GisIsNoWorld(  Ref.TemporaryVisibleExtent ) then
        Result := GisCommonExtent( Result, Ref.TemporaryVisibleExtent ) ;
    end ;
  end ;

  procedure TGIS_Layer.setUp ;
  begin
    FAge := Now ;

    if UseConfig then
      ConfigName := FConfigName ;
  end ;

  procedure TGIS_Layer.setUp2 ;
  begin
    if assigned( FStatistics ) then
      FStatistics.LoadFromFile ;
  end ;

  procedure TGIS_Layer.setUp3 ;
  var
    viewerAutoStyle : Boolean ;
  begin
    // apply AutoStyle when layer is open (ShapeType is known)
    viewerAutoStyle := False ;
    if assigned(Viewer) then
      viewerAutoStyle := Viewer.Ref.AutoStyle ;

    if viewerAutoStyle then
      ApplyAutoStyle ;

    PrepareParams;
  end ;

  procedure TGIS_Layer.doApplyAutoStyle;
  begin
    // implement in derived classes
  end ;

  function TGIS_Layer.ApplyAutoStyle : Boolean ;
  begin
    Result := ApplyAutoStyle( False ) ;
  end ;

  function TGIS_Layer.ApplyAutoStyle( const _forceAutoStyle: Boolean ) : Boolean ;
  begin
    if _forceAutoStyle then
      Result := SupportsAutoStyle
    else
      Result := SupportsAutoStyle and ( not ParamsList.IsModified ) ;

    if Result then
      doApplyAutoStyle ;
  end ;

  procedure TGIS_Layer.applyConfigProjection(
    const _cfg   : TGIS_ConfigAbstract
  ) ;
  begin
    applyConfigProjection( _cfg, self ) ;
  end ;

  procedure TGIS_Layer.applyConfigProjection(
    const _cfg   : TGIS_ConfigAbstract;
    const _layer : TGIS_Layer
  ) ;
  var
    epsg : Integer ;
    wkt  : String  ;
  begin
    with TGIS_Config( _cfg ) do begin
      epsg             := ReadInteger    ( GIS_INI_CS_EPSG,
                                           0
                                         ) ;
      wkt              := ReadString     ( GIS_INI_CS_WKT,
                                           ''
                                         ) ;
    end ;



    if ( epsg > 0 )  or ( not IsStringEmpty( wkt ) ) then begin
      if epsg <= 0 then
        _layer.FCS := CSUnknownCoordinateSystem
      else
        _layer.FCS := TGIS_CSFactory.ByEPSG( epsg ) ;

      if _layer.FCS.EPSG <= 0 then
        _layer.FCS := TGIS_CSFactory.ByWKT( wkt ) ;
    end ;
  end ;

  procedure TGIS_Layer.ApplyCuttingPolygon(
    const _wkt : String
  ) ;
  begin

  end ;

  procedure TGIS_Layer.applyConfigOptions(
    const _cfg : TGIS_ConfigAbstract
  ) ;
  var
    ext : TGIS_Extent3D ;
  begin
    with TGIS_Config( _cfg ) do begin
      CodePage         := ReadInteger    ( GIS_INI_CODEPAGE,
                                           CodePage
                                         ) ;
      FCodePageForced  := ReadInteger    ( GIS_INI_CODEPAGE,
                                           0
                                         ) ; // workaround for saving default CP
      Caption          := ReadString     ( GIS_INI_CAPTION,
                                           Caption
                                         ) ;
      Comments         := ReadString     ( GIS_INI_COMMENTS,
                                           Comments
                                         ) ;
      Active           := ReadBoolean    ( GIS_INI_ACTIVE,
                                           Active
                                         ) ;
      FIsReadOnly      := ReadBoolean    ( GIS_INI_READONLY,
                                           IsReadOnly
                                         ) ;
      Collapsed        := ReadBoolean    ( GIS_INI_COLLAPSED,
                                           Collapsed
                                         ) ;
      HideFromLegend   := ReadBoolean    ( GIS_INI_HIDEFROMLEGEND,
                                           HideFromLegend
                                         ) ;
      Basemap          := ReadBoolean    ( GIS_INI_BASEMAP,
                                           Basemap
                                         ) ;
      CachedPaint      := ReadBoolean    ( GIS_INI_CACHEDPAINT,
                                           CachedPaint
                                         ) ;
      Transparency     := ReadInteger    ( GIS_INI_TRANSPARENCY,
                                           Transparency
                                         ) ;
      Addition         := ReadInteger    ( GIS_INI_ADDITION,
                                           Addition
                                         ) ;
      DormantMode      := ReadDormant    ( GIS_INI_DORMANTMODE,
                                           DormantMode
                                         ) ;
      {$IFDEF GIS_3D}
        View3D.ModeDefault
                       := ReadString     ( GIS_INI_3DLAYERTYPE,
                                           ''
                                         ) = '' ;
        View3D.Mode    := Read3DLayerType( GIS_INI_3DLAYERTYPE,
                                           View3D.Mode
                                         ) ;
      {$ENDIF}
      {$IFDEF GIS_NORECORDS}
        ext := new TGIS_Extent3D ;
      {$ENDIF}
      ext.XMin := ReadFloat( GIS_INI_EXTENT_XMIN, GisNoWorld3D.XMin ) ;
      ext.XMax := ReadFloat( GIS_INI_EXTENT_XMAX, GisNoWorld3D.XMax ) ;
      ext.YMin := ReadFloat( GIS_INI_EXTENT_YMIN, GisNoWorld3D.YMin ) ;
      ext.YMax := ReadFloat( GIS_INI_EXTENT_YMAX, GisNoWorld3D.YMax ) ;
      ext.ZMin := ReadFloat( GIS_INI_EXTENT_ZMIN, GisNoWorld3D.ZMin ) ;
      ext.ZMax := ReadFloat( GIS_INI_EXTENT_ZMAX, GisNoWorld3D.ZMax ) ;
      if not GisIsNoWorld( GisExtent2DFrom3D( ext ) ) then
        Extent3D := ext ;

      ReadCustomData( GIS_INI_CUSTOM ,CustomData );
    end ;
  end ;

  procedure TGIS_Layer.storeConfigOptions(
    const _cfg : TGIS_ConfigAbstract
  ) ;

      function get_3DLayertype : TGIS_3DLayerType ;
      begin
        Result := TGIS_3DLayerType.Off ;
          if Self.IsPixel then begin
            if self.IsGrid then
              Result := TGIS_3DLayerType.Dem ;
          end
        else if Self.IsVector  then begin
          if Self.IsVector3D then
            Result := TGIS_3DLayerType.Shapes
        end ;
      end;

    begin
     with TGIS_Config( _cfg ) do begin
       // force writing of Active property
       if Active then
         WriteBoolean( GIS_INI_ACTIVE        , Active          , False )
       else
         WriteBoolean( GIS_INI_ACTIVE        , Active          , True  ) ;

       WriteString ( GIS_INI_CAPTION         , Caption         , ''    ) ;
       WriteString ( GIS_INI_COMMENTS        , Comments        , ''    ) ;
       WriteBoolean( GIS_INI_READONLY        , IsReadOnly      , False ) ;

       WriteBoolean( GIS_INI_COLLAPSED       , Collapsed       , False ) ;
       WriteBoolean( GIS_INI_HIDEFROMLEGEND  , HideFromLegend  , False ) ;
       WriteBoolean( GIS_INI_BASEMAP         , Basemap         , False ) ;
       WriteBoolean( GIS_INI_CACHEDPAINT     , CachedPaint     , True  ) ;
       WriteInteger( GIS_INI_TRANSPARENCY    , Transparency    , 100   ) ;
       WriteInteger( GIS_INI_ADDITION        , Addition        , 0     ) ;
       WriteDormant( GIS_INI_DORMANTMODE     , DormantMode     ,
                     TGIS_LayerDormantMode.Standard     ) ;
       Write3DLayerType(
                     GIS_INI_3DLAYERTYPE     , View3D.Mode     ,
                     get_3DLayertype                                   ) ;

       if modifiedCodePage then
         WriteInteger( GIS_INI_CODEPAGE      , CodePage        , 0     ) ;

       if modifiedCS then begin
         if CS.EPSG < GIS_EPSG_AUTO then begin
           WriteInteger( GIS_INI_CS_EPSG     , CS.EPSG         , 0     ) ;
           WriteString ( GIS_INI_CS_WKT      ,  ''             , ''    ) ;
         end
         else begin
           WriteInteger( GIS_INI_CS_EPSG     , 0               , 0     ) ;
           WriteString ( GIS_INI_CS_WKT      , CS.FullWKT      , ''    ) ;
         end;
       end ;

       WriteCustomData( GIS_INI_CUSTOM, CustomData );
    end ;
  end ;

  function TGIS_Layer.isSectionInVisibilityRange(
    const _sec : TGIS_ParamsSection
  ) : Boolean ;
  var
    ref         : Double ;
    minScale    : Double ;
    maxScale    : Double ;
    refInv      : Int64 ;
    minScaleInv : Integer ;
    maxScaleInv : Integer ;
  begin
    Result := False ;

    // check levels
    if ( _sec.MinLevel > -GIS_HALF_MAX_DOUBLE ) or ( _sec.MaxLevel < GIS_HALF_MAX_DOUBLE ) then begin
      // minimize Level-ScaleAsFloat-Level conversion error
      ref := RoundS( Viewer.Ref.Level / GIS_LEVEL_PRECISION ) * GIS_LEVEL_PRECISION ;

      if ref < _sec.MinLevel then Exit ;
      if ref > _sec.MaxLevel then Exit ;
      if GisIsSameValue( ref, _sec.MaxLevel ) then Exit ;
    end
    // check scale
    else if ( _sec.MinScale > 0 ) or ( _sec.MaxScale < GIS_HALF_MAX_DOUBLE ) then begin
      ref := Viewer.Ref.ScaleAsFloat ;

      minScale := _sec.MinScale ;
      maxScale := _sec.MaxScale ;

      if GisIsSameValue( minScale, 0 ) then
        minScaleInv := GIS_MAX_INTEGER
      else
        minScaleInv := RoundS( 1 / minScale ) ;

      if GisIsSameValue( maxScale, 0 ) then
        maxScaleInv := GIS_MAX_INTEGER
      else
        maxScaleInv := RoundS( 1 / maxScale ) ;

      refInv := RoundS( 1 / ref ) ;

      // operator reversed due to comparing inverted scale values
      if ( refInv > minScaleInv ) then Exit;
      if ( refInv <= maxScaleInv ) then Exit;
    end
    // check zoom
    else begin
      ref := _sec.MinZoom ;
      if ref < 0 then begin
        if Viewer.Ref.Zoom < Abs( ref ) then Exit ;
      end
      else begin
        if Viewer.Ref.ZoomEx < ref then Exit ;
      end ;

      ref := _sec.MaxZoom ;
      if ref < 0 then begin
        if Viewer.Ref.Zoom >= Abs( ref ) then Exit ;
      end
      else begin
        if Viewer.Ref.ZoomEx >= ref then Exit ;
      end ;
    end ;

    Result := True ;
  end;

  procedure TGIS_Layer.prepareParamsCache ;
  begin
    prepareParamsCache( '' ) ;
  end ;

  procedure TGIS_Layer.prepareParamsCache( const _style : String ) ;
  var
    i,j : Integer ;
    sec : TGIS_ParamsSection ;
  begin
    if not assigned( Viewer ) then exit ;

    paramsCache.Clear ;

    for i:=0 to ParamsList.Count - 1 do begin
      sec := ParamsList.Items[ i ] ;

      if CompareText( sec.Style, _style ) <> 0 then continue ;

      if not isSectionInVisibilityRange( sec ) then continue ;

      if sec is TGIS_ParamsSectionVector then begin
        with sec as TGIS_ParamsSectionVector do begin
          if assigned( QueryObj ) then
            QueryObj.RePrepare ;
          if assigned( Render.ExpressionObj ) then
            Render.ExpressionObj.RePrepare ;
            if assigned( Render.ChartObj ) then begin
            for j := 0 to Render.ChartObj.Count - 1  do begin
                if assigned( Render.ChartObj[ j ] ) then
                TGIS_SqlQuery( Render.ChartObj[ j ] ).RePrepare ;
            end ;
          end ;
        end ;
      end ;
      sec.InternalIndex := i ;
      paramsCache.Add( sec ) ;
    end ;
    paramsCacheUpdated := True ;

    ParamsList.Selected := 0 ;
  end ;

  procedure TGIS_Layer.optimizeParamsCache(
    const _idx : Integer
  ) ;
  begin
    optimizeParamsCache( _idx, False ) ;
  end ;

  procedure TGIS_Layer.optimizeParamsCache(
    const _idx : Integer ;
    const _lru : Boolean
  ) ;
  var
    obj : TGIS_ParamsSection ;
  begin
    if _idx < 0 then exit ;

    obj := paramsCache[ _idx ] ;
    if ParamsList.Items[ ParamsList.Selected ] = obj then exit ;

    if _lru then begin
      if _idx > 0 then begin
        {$IFDEF OXYGENE}
          paramsCache.RemoveAt( _idx ) ;
        {$ELSE}
          paramsCache.Delete( _idx ) ;
        {$ENDIF}
        paramsCache.Add( obj ) ;
      end ;
    end ;

    ParamsList.Selected := obj.InternalIndex ;
  end ;

  function TGIS_Layer.transformExtent(
    const _forward : Boolean   ;
    const _extent  : TGIS_Extent
  ) : TGIS_Extent  ;
  var
    ptg : TGIS_Point3D ;
  begin
    if assigned( Transform ) and Transform.Active then begin
      {$IFDEF GIS_NORECORDS}
        Result := new TGIS_Extent ;
      {$ENDIF}
      Result.XMin :=   GIS_MAX_DOUBLE ;
      Result.YMin :=   GIS_MAX_DOUBLE ;
      Result.XMax := - GIS_MAX_DOUBLE ;
      Result.YMax := - GIS_MAX_DOUBLE ;

      ptg := GisPoint3D( _extent.XMin, _extent.YMin, 0 ) ;
      if _forward then
         Transform.Transform3D_Ref( ptg )
      else
         Transform.Untransform3D_Ref( ptg ) ;

      Result.XMin := Min( Result.XMin, ptg.X ) ;
      Result.YMin := Min( Result.YMin, ptg.Y ) ;
      Result.XMax := Max( Result.XMax, ptg.X ) ;
      Result.YMax := Max( Result.YMax, ptg.Y ) ;

      ptg := GisPoint3D( _extent.XMin, _extent.YMax, 0 ) ;
      if _forward then
         Transform.Transform3D_Ref( ptg )
      else
         Transform.Untransform3D_Ref( ptg ) ;

      Result.XMin := Min( Result.XMin, ptg.X ) ;
      Result.YMin := Min( Result.YMin, ptg.Y ) ;
      Result.XMax := Max( Result.XMax, ptg.X ) ;
      Result.YMax := Max( Result.YMax, ptg.Y ) ;

      ptg := GisPoint3D( _extent.XMax, _extent.YMax, 0 ) ;
      if _forward then
        Transform.Transform3D_Ref( ptg )
      else
        Transform.Untransform3D_Ref( ptg ) ;

      Result.XMin := Min( Result.XMin, ptg.X ) ;
      Result.YMin := Min( Result.YMin, ptg.Y ) ;
      Result.XMax := Max( Result.XMax, ptg.X ) ;
      Result.YMax := Max( Result.YMax, ptg.Y ) ;

      ptg := GisPoint3D( _extent.XMax, _extent.YMin, 0 ) ;
      if _forward then
        FTransform.Transform3D_Ref( ptg )
      else
        FTransform.Untransform3D_Ref( ptg ) ;

      Result.XMin := Min( Result.XMin, ptg.X ) ;
      Result.YMin := Min( Result.YMin, ptg.Y ) ;
      Result.XMax := Max( Result.XMax, ptg.X ) ;
      Result.YMax := Max( Result.YMax, ptg.Y ) ;
    end
    else
      Result := _extent ;
  end;

  procedure TGIS_Layer.lockThread ;
  begin
    criticalSection.Enter ;
  end;

  procedure TGIS_Layer.unlockThread ;
  begin
    criticalSection.Leave ;
  end;

  function TGIS_Layer.ChangeHash
    : Int64 ;
  var
    str : String ;
    i   : Integer ;
  begin
    if FBasemapDraw then begin
      Result := lastHash ;
      exit;
    end;
    str := Name
           + ':' + Caption
           + ':' + IntToStr( ParamsList.Serial )
           + ':' + IntToStr( Integer( Active ) )
           + ':' + IntToStr( Integer( Collapsed ) ) ;

    if assigned( SubLayers ) then begin
      for i := 0 to SubLayers.Count - 1 do begin
         str := str +
                ':' + IntToStr( TGIS_Layer( SubLayers[ i ] ).ChangeHash ) ;
      end;
    end;
    Result := StringHash( str ) ;
    lastHash := Result ;
  end;

  procedure TGIS_Layer.Prepare ;
  begin
    if FIsPrepared then exit ;

    if assigned( Viewer ) then
      Viewer.Ref.Lock ;
    try
      Path := GetPathAbsolute( '', Path ) ;

      if IsStringEmpty( FConfigName ) then
        // setup ConfigName=Path if no other name exist
        FConfigName := Path ;

      if UseConfig then
        ConfigName := FConfigName ;

      ReadConfig ;

      if assigned( ConfigFile ) then begin
        TGIS_Config( ConfigFile ).SetLayer  ( self       ) ;
        TGIS_Config( ConfigFile ).SetSection( 0, False   ) ;
        applyConfigProjection( ConfigFile ) ;
      end ;

      if assigned( Viewer ) then begin
        if assigned( Viewer.Ref.ProjectFile ) and IsPersistent then begin
          TGIS_Config(Viewer.Ref.ProjectFile).SetLayer( self ) ;
          TGIS_Config(Viewer.Ref.ProjectFile).SetSection( 0, False ) ;
          applyConfigProjection( Viewer.Ref.ProjectFile ) ;
        end ;
      end;

      if not ( TGIS_LayerSubType.Exportable in FSubType ) then
        FIsReadOnly := True ;

      FIsModified := False ;
      FIsPrepared := True ;
    finally
      if assigned( Viewer ) then
        Viewer.Ref.Unlock ;
    end;
  end ;

  procedure TGIS_Layer.PrepareParams ;
  begin
    // used for preparing paramsCache when sections are inactive
    prepareParamsCache ;
  end ;

  procedure TGIS_Layer.Open ;
  var
    old_cs        : Integer ;
    old_cp        : Integer ;
  begin
    if assigned( Viewer ) then
      Viewer.Ref.Lock ;
    try
      if FIsOpened then exit ;

      if not FIsModified then
        Prepare ;

      FIsOpened := True ;

      old_cs := CS.EPSG ;
      old_cp := FCodePageForced ;

      setUp  ;          // and make internal initialization
      setUp2 ;          // and make internal initialization
      setUp3 ;          // and make internal initialization

      if ( old_cp <> 0 ) and ( CodePage <> old_cp ) then
        CodePage := old_cp
      else
        modifiedCodePage := False ;

      if ( old_cs <> 0 ) and ( CS.EPSG <> old_cs ) then
        SetCSByEPSG( old_cs )
      else
        modifiedCS := False ;

      if not GisIsNoWorld( Extent ) then begin
        with Extent do begin
          if ( Abs( XMin ) > 1e100 ) or
             ( Abs( XMax ) > 1e100 ) or
             ( Abs( YMin ) > 1e100 ) or
             ( Abs( YMax ) > 1e100 ) or
             ( XMin > XMax         ) or
             ( YMin > YMax         )
          then
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERBADEXTENT ), Name, 0 ) ;
        end ;
      end ;

      with Extent do
       if ( XMin > XMax ) or ( YMin > YMax ) then
          RecalcExtent ;

      if not IsEmbeddedSQLPath( Path ) and SafeFileExists( Path + GIS_TRN_EXT ) then
        self.Transform := GisTransformFactory( Path + GIS_TRN_EXT ) ;

      RecalcProjectedExtent ;

      if not IsEmbeddedSQLPath( Path ) and SafeFileExists( Path + GIS_PRJ_EXT ) then
        self.SetCSByWKTFile( Path + GIS_PRJ_EXT ) ;

      FIsModified := False ;
    finally
      if assigned( Viewer ) then
        Viewer.Ref.Unlock ;
    end;
  end ;

  procedure TGIS_Layer.ReOpen ;
  begin
    RevertAll ;
    ParamsList.ResetSerial ;
    FIsPrepared := False ;
    FIsOpened   := False ;
    Open ;
  end ;

  function TGIS_Layer.HourglassShake : Boolean ;
  var
    tm : Int64 ;
  begin
    tm := GetTickCount ;

    if assigned( Viewer ) and ( tm > lastHourglassState + 200 ) then begin
      Result := Viewer.Ref.HourglassShake ;

      lastHourglassState := tm ;
    end
    else
      Result := False ;
  end ;

  procedure TGIS_Layer.Move( const _step : Integer ) ;
  begin
    ZOrder := ZOrder + _step ;
  end ;

  procedure TGIS_Layer.MoveEx( const _step : Integer ) ;
  begin
    ZOrderEx := ZOrderEx + _step ;
  end ;

  procedure TGIS_Layer.ReadConfig ;
  var
    i   : Integer ;

    procedure read_sublayers_from_project( const _layer : TGIS_Layer ) ;
    var
      j  : Integer ;
      la : TGIS_Layer ;
    begin
      with TGIS_Config( Viewer.Ref.ProjectFile ) do begin
        if assigned( _layer.SubLayers ) then begin
          for j := 0 to _layer.SubLayers.Count - 1 do begin
            la := TGIS_Layer( _layer.SubLayers[ j ] ) ;
            SetSubLayer( la ) ;

            SetSubSection( 0, False ) ;
            la.applyConfigOptions( Viewer.Ref.ProjectFile ) ;
            la.ParamsList.LoadFromConfig( Viewer.Ref.ProjectFile, True ) ;

            read_sublayers_from_project( la ) ;
          end ;
        end ;
      end ;
    end ;

  begin
    // clear params
    ParamsList.Clear ;

    if assigned( ConfigFile ) then begin
      with TGIS_Config( ConfigFile ) do begin
        SetLayer( self ) ;

        SetSection( 0, False ) ;
        applyConfigOptions( ConfigFile ) ;
        ParamsList.LoadFromConfig( ConfigFile ) ;

        if assigned( SubLayers ) then begin
          for i := 0 to SubLayers.Count - 1 do begin
            SetSubLayer( SubLayers[ i ] ) ;

            SetSubSection( 0, False ) ;
            TGIS_Layer( SubLayers[ i ] ).applyConfigOptions( ConfigFile ) ;
            TGIS_Layer( SubLayers[ i ] ).ParamsList.LoadFromConfig( ConfigFile, True ) ;
          end ;
        end ;
      end ;
    end ;

    if not assigned( Viewer ) then exit ;
    if not assigned( Viewer.Ref.ProjectFile ) then exit ;

    with TGIS_Config( Viewer.Ref.ProjectFile ) do begin
      SetLayer( self ) ;

      if SetSection( 0, False ) then begin
        applyConfigOptions( Viewer.Ref.ProjectFile ) ;
        ParamsList.LoadFromConfig( Viewer.Ref.ProjectFile ) ;

        read_sublayers_from_project( Self ) ;
      end ;
    end ;

    Viewer.Ref.InvalidateWholeMap ;
  end ;

  function TGIS_Layer.ReadConfigParam(
    const _name : String
  ) : String ;
  begin
    Result := '' ;

    if assigned( ConfigFile ) then begin
      with TGIS_Config( ConfigFile ) do begin
        SetLayer( self ) ;
        SetSection( 0, False ) ;
        Result := ReadString( _name, '' ) ;
      end ;
    end
    else begin
      if not assigned( Viewer ) then exit ;
      if not assigned( Viewer.Ref.ProjectFile ) then exit ;

      with TGIS_Config( Viewer.Ref.ProjectFile ) do begin
        SetLayer( self ) ;
        SetSection( 0, False ) ;
        Result := ReadString( _name, '' ) ;
      end ;
    end ;
  end ;

  procedure TGIS_Layer.RereadConfig ;
  begin
    if not assigned( ConfigFile ) then exit ;

    TGIS_Config( ConfigFile ).Reread ;

    // clear params and apply default settings
    ParamsList.ClearAndSetDefaults ;
    ReadConfig ;
  end ;

  procedure TGIS_Layer.WriteConfig ;
  var
    bconfig  : Boolean     ;
    bupgrade : Boolean     ;
    cfg      : TGIS_Config ;
    cfg_tmp  : TGIS_ConfigAbstract ;

    procedure store_sublayers_to_project( const _cfg : TGIS_Config; const _layer : TGIS_Layer ) ;
    var
      i  : Integer    ;
      la : TGIS_Layer ;
    begin
      with _cfg do begin
        if assigned( _layer.SubLayers ) then begin
          for i := 0 to _layer.SubLayers.Count - 1 do begin
            SetSubLayer( _layer.SubLayers[ i ] ) ;
            ClearSubSections ;

            SetSubSection( 0, True ) ;
            la := TGIS_Layer( _layer.SubLayers[ i ] ) ;
            la.storeConfigOptions( Viewer.Ref.ProjectFile ) ;
            la.ParamsList.SaveToConfig( Viewer.Ref.ProjectFile, True ) ;

            store_sublayers_to_project( _cfg, la ) ;
          end ;
        end ;
      end ;
    end ;

    procedure store_to_project( const _cfg : TGIS_Config ) ;
    begin
      with _cfg do begin
        SetLayer( self ) ;
        ParamsList.SaveToConfig( Viewer.Ref.ProjectFile ) ;
        ClearSections ;

        SetSection( 0, True ) ;
        storeConfigOptions( Viewer.Ref.ProjectFile ) ;
        ParamsList.SaveToConfig( Viewer.Ref.ProjectFile ) ;

        store_sublayers_to_project( _cfg, Self ) ;
      end ;
    end ;

    procedure store_to_config( const _cfg : TGIS_Config ) ;
    var
      i       : Integer     ;
      la      : TGIS_Layer  ;
    begin
      with _cfg do begin
        Lock ;
        try
          SetLayer( self ) ;
          ClearSections ;

          SetSection( 0, True ) ;
          storeConfigOptions( _cfg ) ;
          ParamsList.SaveToConfig( _cfg ) ;

          if assigned( SubLayers ) then begin
            for i := 0 to SubLayers.Count - 1 do begin
              // create empty sublayer if we don't use a project file
              AddSubLayer( SubLayers[ i ] , 0, i ) ;
              SetSubLayer( SubLayers[ i ] ) ;
              ClearSubSections ;

              SetSubSection( 0, True ) ;
              la := TGIS_Layer( SubLayers[ i ] ) ;
              la.storeConfigOptions( _cfg ) ;
              la.ParamsList.SaveToConfig( _cfg, True ) ;
            end ;
          end ;
        finally
          Unlock ;
        end;
      end;
    end ;
  begin
    if assigned( ConfigFile ) then
      bconfig := SafeFileExists( TGIS_Config( ConfigFile ).FileName ) and UseConfig
    else
      bconfig := False ;

    if not bconfig then begin

      if not assigned( Viewer ) then exit ;
      if not assigned( Viewer.Ref.ProjectFile ) then exit ;

      store_to_project( TGIS_Config( Viewer.Ref.ProjectFile ) ) ;

    end
    else begin // save in layer configuration file

      if assigned( ConfigFile ) then begin
        bupgrade := GisMetadataAsBoolean( METADATA_UPGRADEPROJECT, False )
                    and
                    ( CompareText(
                        GetFileExt( TGIS_Config( ConfigFile ).FileName ),
                        GIS_INI_EXT
                      ) = 0
                    ) ;

        if assigned( Viewer ) and assigned( Viewer.Ref.ProjectFile ) then begin

          bupgrade := bupgrade
                      and
                      (
                        CompareText(
                          GetFileExt( TGIS_Config( Viewer.Ref.ProjectFile ).FileName ),
                          GIS_TTKPROJECT_EXT
                        ) = 0
                      ) ;
        end ;


        if bupgrade then begin
          // old file format - upgrade

          cfg_tmp := ConfigFile ;

          cfg := TGIS_ConfigFactory.CreateConfig(
                   nil,
                   GetPathNoExt( TGIS_Config( ConfigFile ).FileName ) +
                   GIS_TTKSTYLE_EXT
                 ) ;
          try
            store_to_config( cfg ) ;
            // save config to a file (required for xml) here, not in store_to_config
            // to not break read-only config caught error logic done inside
            // TGIS_Viewer.saveProjectFile
            cfg.Save ;
          finally
            try
              RenameFile( TGIS_Config( ConfigFile ).FileName, TGIS_Config( ConfigFile ).FileName + '.old' ) ;
            except
              // renaming problem can happen
            end ;

            FreeObject( FConfigFile )  ;
            FConfigFile := cfg ;
          end ;

        end
        else
          store_to_config( TGIS_Config( ConfigFile ) ) ;
      end;
    end ;
  end ;

  function TGIS_Layer.Paint : Boolean ;
  begin
    FInPaint := True ;
    try
      if assigned( FOnPaintLayer )
      then begin
        {$IFDEF OXYGENE}
          FOnPaintLayer( Viewer.Ref, new TGIS_LayerEventArgs( Self ) ) ;
        {$ELSE}
          FOnPaintLayer( TObject(Viewer.Ref), Self ) ;
        {$ENDIF}
        Result := True ;
      end
      else Result := Draw ;
    finally
      FInPaint := False ;
    end;
  end;

  function TGIS_Layer.Draw : Boolean ;
  begin
    lockThread ;
    try
      resetMustReproject ;

      Result := DrawEx( drawExtentEx ) ;
    finally
      unlockThread ;
    end;
  end ;

  function TGIS_Layer.DrawSelected : Boolean ;
  begin
    lockThread ;
    try
      resetMustReproject ;

      Result := DrawSelectedEx( drawExtentEx ) ;
    finally
      unlockThread ;
    end;
  end ;

  function TGIS_Layer.DrawEx(
    const _extent : TGIS_Extent
  ) : Boolean ;
  begin
    Result := IsVisible( _extent ) ;
  end ;

  function TGIS_Layer.DrawSelectedEx(
    const _extent : TGIS_Extent
  ) : Boolean ;
  begin
    Result := False ;
  end ;

  procedure TGIS_Layer.DrawFlash ;
  begin
    lockThread ;
    try
      resetMustReproject ; //?TK
    finally
      unlockThread ;
    end;
  end;

  procedure TGIS_Layer.SaveData ;
  begin
    // only for save inheritance
  end ;

  procedure TGIS_Layer.RevertAll ;
  begin
    // only for save inheritance
  end ;

  procedure TGIS_Layer.SaveAll ;
  begin
    if assigned( ConfigFile ) and ( not IsStringEmpty( ConfigName ) ) then
    begin
      if TGIS_Config( ConfigFile ).MustSave then
        try
          TGIS_Config( ConfigFile ).Save ;
        except
          // ignore INI save errors
          TGIS_Config( ConfigFile ).ClearSave ;
        end;
    end ;
    if assigned( Transform ) and Transform.Active then begin
      if Transform.MustSave then
        Transform.SaveToFile( Path + '.trn' );
    end ;
    if MustSave or ( not IsExportable ) then
      SaveData ;
  end ;

  function TGIS_Layer.MustSave : Boolean ;
  var
    i         : Integer ;
    lmodified : Boolean ;
  begin
    if not IsExportable then begin
      Result := False ;
      exit ;
    end;

    if IsReadOnly then begin
      Result := False ;
      exit ;
    end;

    lmodified := FIsModified ;

    if assigned( SubLayers ) then
      for i := 0 to SubLayers.Count-1 do begin
        lmodified := lmodified or TGIS_Layer( SubLayers[i] ).FIsModified ;
        if lmodified then break ;
      end ;

    Result := lmodified ;
    if assigned( ConfigFile ) and ( not IsStringEmpty( ConfigName ) ) then
      Result := lmodified or TGIS_Config( ConfigFile ).MustSave ;

    if assigned( Transform ) and Transform.Active then
      Result := lmodified or Transform.MustSave ;
  end ;

  procedure TGIS_Layer.RecalcExtent ;
  begin
    // just for safe inheritance
  end ;

  procedure TGIS_Layer.RecalcProjectedExtent ;
  var
    ext : TGIS_Extent ;
  begin
    if not ( assigned( Viewer ) ) or GisIsNoWorld( Extent ) then begin
      ProjectedExtent := Extent ;
      FProjectedExtentViewerEPSG    := -1 ;
      FProjectedExtentEPSG          := -1 ;

     resetMustReproject ;

     exit ;
    end ;

    resetMustReproject ;

    ext := transformExtent( True, Extent ) ;

    if assigned( Viewer )
       and
       (
         ( ( Viewer.Ref.CS.EPSG <> 0       ) and
           ( CS.EPSG            <> 0       ) and
           ( Viewer.Ref.CS.EPSG <> CS.EPSG )
         )
         or
         ( Viewer.Ref.RotationAngle <> 0 )
       )
    then begin
      if ( FProjectedExtentViewerEPSG    <> Viewer.Ref.CS.EPSG        ) or
         ( FProjectedExtentEPSG          <> CS.EPSG                   ) or
         ( FProjectedExtentRotationAngle <> Viewer.Ref.RotationAngle  ) or
         ( not GisIsSamePoint ( FProjectedExtentRotationPoint ,
                                Viewer.Ref.RotationPoint
                              )                                   ) or
         ( not GisIsSameExtent( FProjectedExtentBase, Extent )    )
      then begin
        // calculate only if required
        ProjectedExtent
          := Viewer.Ref.RotatedExtent( Viewer.Ref.CS.ExtentFromCS( CS, ext ) ) ;
      end
    end
    else begin
      ProjectedExtent := ext  ;
    end ;

    FProjectedExtentViewerEPSG    := Viewer.Ref.CS.EPSG       ;
    FProjectedExtentEPSG          := CS.EPSG              ;
    FProjectedExtentBase          := Extent               ;
    FProjectedExtentRotationAngle := Viewer.Ref.RotationAngle ;
    FProjectedExtentRotationPoint := Viewer.Ref.RotationPoint ;

  end ;

  procedure TGIS_Layer.Lock ;
  begin
    FIsLocked := True ;
  end ;

  procedure TGIS_Layer.Unlock ;
  begin
    if assigned( Viewer ) then Viewer.Ref.RecalcExtent ;
    FIsLocked  := False ;
  end ;

  procedure TGIS_Layer.Project_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point
  ) ;
  var
    ptg : TGIS_Point3D ;
  begin
    if MustReproject < 0 then exit ;

    {$IFDEF GIS_NORECORDS}
      ptg := new TGIS_Point3D ;
    {$ENDIF}
    ptg.X := _ptg.X ;
    ptg.Y := _ptg.Y ;
    ptg.Z := 0 ;
    ptg.M := 0 ;

    Project3D_Ref( ptg ) ;

    _ptg.X := ptg.X ;
    _ptg.Y := ptg.Y ;
  end ;

  procedure TGIS_Layer.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
  ) ;
  begin
    if assigned( Transform ) and Transform.Active then begin
      Transform.Transform3D_Ref( _ptg ) ;
    end;

    if not assigned( Viewer ) then
      exit ;

    if MustReproject = 2 then begin
      try
        CS.ToWGS3D_Ref( _ptg ) ;
        csctxViewerCS.FromWGS3D_Ref( _ptg ) ;
      except
        // do nothing
      end ;
    end ;

    if csctxRotationAngle <> 0 then begin
      csctxViewer.RotatedPoint3D_ref( _ptg ) ;
    end

  end ;

  procedure TGIS_Layer.Unproject_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point
  ) ;
  var
    ptg : TGIS_Point3D ;
  begin
    if MustReproject < 0 then exit ;

    {$IFDEF GIS_NORECORDS}
      ptg := new TGIS_Point3D ;
    {$ENDIF}
    ptg.X := _ptg.X ;
    ptg.Y := _ptg.Y ;
    ptg.Z := 0 ;
    ptg.M := 0 ;

    Unproject3D_Ref( ptg ) ;

    _ptg.X := ptg.X ;
    _ptg.Y := ptg.Y ;
  end ;

  procedure TGIS_Layer.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
  ) ;
  var
    ctx : Integer ;
  begin
    if not assigned( Viewer ) then
      exit ;

    ctx := MustReproject ;

    if csctxRotationAngle <> 0 then begin
      csctxViewer.UnrotatedPoint3D_ref( _ptg ) ;
    end ;

    if ctx = 2 then begin
      try
        csctxViewerCS.ToWGS3D_Ref( _ptg ) ;
        CS.FromWGS3D_Ref( _ptg ) ;
      except
        // do nothing
      end ;
    end ;

    if assigned( Transform ) and Transform.Active then begin
      Transform.Untransform3D_Ref( _ptg ) ;
    end;

  end ;

  function TGIS_Layer.Project(
    const _ptg : TGIS_Point
  ) : TGIS_Point ;
  var
    tmp : TGIS_Point3D ;
  begin
    if MustReproject < 0 then begin
      Result := _ptg ;
      exit ;
    end;

    {$IFDEF GIS_NORECORDS}
      tmp := new TGIS_Point3D ;
    {$ENDIF}
    tmp.X := _ptg.X ;
    tmp.Y := _ptg.Y ;
    tmp.Z := 0 ;
    tmp.M := 0 ;

    Project3D_Ref( tmp ) ;

    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point ;
    {$ENDIF}
    Result.X := tmp.X ;
    Result.Y := tmp.Y ;
  end ;

  function TGIS_Layer.Project3D(
    const _ptg : TGIS_Point3D
  ) : TGIS_Point3D ;
  var
    tmp : TGIS_Point3D ;
  begin
    if MustReproject < 0 then begin
      Result := _ptg ;
      exit ;
    end;

    tmp := _ptg ;

    Project3D_Ref( tmp ) ;

    Result := tmp ;
  end ;

  function TGIS_Layer.Unproject(
    const _ptg : TGIS_Point
  ) : TGIS_Point ;
  var
    tmp : TGIS_Point3D ;
  begin
    if MustReproject < 0 then begin
      Result := _ptg ;
      exit ;
    end;

    {$IFDEF GIS_NORECORDS}
      tmp := new TGIS_Point3D ;
    {$ENDIF}
    tmp.X := _ptg.X ;
    tmp.Y := _ptg.Y ;
    tmp.Z := 0 ;
    tmp.M := 0 ;

    Unproject3D_Ref( tmp ) ;

    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point ;
    {$ENDIF}
    Result.X := tmp.X ;
    Result.Y := tmp.Y ;
  end ;

  function TGIS_Layer.Unproject3D(
    const _ptg : TGIS_Point3D
  ) : TGIS_Point3D ;
  var
    tmp : TGIS_Point3D ;
  begin
    if MustReproject < 0 then begin
      Result := _ptg ;
      exit ;
    end;

    tmp := _ptg ;

    Unproject3D_Ref( tmp ) ;

    Result := tmp ;
  end ;

  function TGIS_Layer.ProjectExtent(
    const _extent : TGIS_Extent
  ) : TGIS_Extent ;
  var
    ext : TGIS_Extent ;
  begin
    ext := transformExtent( True, _extent ) ;

    if not assigned( Viewer ) then
      exit ;

    ext := self.CS.ExtentToCS( Viewer.Ref.CS, ext ) ;

    if csctxRotationAngle <> 0 then
      ext := Viewer.Ref.RotatedExtent( _extent ) ;

    Result := ext ;
  end ;

  function TGIS_Layer.UnprojectExtent(
    const _extent : TGIS_Extent
  ) : TGIS_Extent ;
  var
    ext : TGIS_Extent ;
  begin
    ext := _extent ;

    if assigned( Viewer ) then begin
      if csctxRotationAngle <> 0 then
        ext := Viewer.Ref.UnrotatedExtent( _extent ) ;

      if assigned( Viewer ) then
        ext := self.CS.ExtentFromCS( Viewer.Ref.CS, ext ) ;
    end ;

    ext := transformExtent( False, ext ) ;

    Result := ext ;
  end ;


  procedure TGIS_Layer.Alive ;
  begin
    // just for safe inheritance
  end ;

  function TGIS_Layer.DormantGain
   : Integer ;
  begin
    Result := 0 ;
  end ;

  procedure TGIS_Layer.Dormant ;
  begin
    // just for safe inheritance
  end ;

  function TGIS_Layer.PreRecognize(
    const _path     : String ;
    var   _new_path : String
  ) : Boolean ;
  begin
    Result    := True ;
    _new_path := '' ;
  end ;

  procedure TGIS_Layer.SetCSByWKT(
    const _wkt : String
  ) ;
  var
    wkt : String ;
  begin
    wkt := Trim( _wkt ) ;
    if IsStringEmpty( wkt ) then
      self.CS := nil
    else
      self.CS := TGIS_CSFactory.ByWKT( wkt ) ;
  end ;

  procedure TGIS_Layer.SetCSByEPSG(
    const _epsg : Integer
  ) ;
  begin
    if _epsg <= 0 then
      self.CS := nil
    else
      self.CS := TGIS_CSFactory.ByEPSG( _epsg ) ;
  end ;

  procedure TGIS_Layer.SetCSByWKTFile(
    const _path : String
  ) ;
  begin
    if SafeFileExists( _path ) then
      Self.CS := TGIS_CSFactory.ByWKTFile( _path ) ;
  end ;

  function TGIS_Layer.RootLayer
    : TGIS_Layer ;
  begin
    Result := Self ;
    while Result.ParentLayer <> nil do
      Result := Result.ParentLayer ;
  end ;

  procedure TGIS_Layer.ForEachSubLayer(
    const _callback : TGIS_ForEachLayerEvent
  ) ;
  var
    i     : Integer ;
    abrt  : Boolean ;
  begin
    if not assigned( _callback ) then exit ;

    abrt := False ;
    _callback( Self, abrt ) ;
    if abrt then exit ;

    if assigned( SubLayers ) then
      for i := 0 to SubLayers.Count - 1 do
        if SubLayers[i] is TGIS_Layer then
          TGIS_Layer(SubLayers[i]).ForEachSubLayer( _callback ) ;


      if Self is TGIS_LayerProject then begin
        for i := 0 to TGIS_LayerProject(Self).Items.Count - 1 do
          if TGIS_LayerProject(Self).Items[i] is TGIS_Layer then
            TGIS_Layer(TGIS_LayerProject(Self).Items[i]).ForEachSubLayer(
              _callback
            ) ;
      end ;

    end ;

  procedure TGIS_Layer.RaiseBusyPrepare(
    const _sender  : TObject ;
    const _text    : String
  ) ;
  var
    snd    : TGIS_Layer ;
    wasvwr : Boolean ;
  begin
    wasvwr := False ;

    if _sender is TGIS_Layer  then
      snd := TGIS_Layer( _sender )
    else
      snd := nil ;

    if assigned( snd ) and ( snd <> self ) then begin
      if assigned( snd.Viewer ) then begin
        snd.Viewer.Ref.BusyPrepare( self, _text ) ;
        wasvwr := true ;
      end
    end ;

    if assigned( Viewer ) and not wasvwr then begin
      Viewer.Ref.BusyPrepare( self, _text ) ;
    end ;

    if assigned( snd ) and ( snd <> self ) then begin
      snd.RaiseBusyEvent( _sender, 0, -1 ) ;
    end;
    RaiseBusyEvent( _sender, 0, -1 ) ;
  end ;

  procedure TGIS_Layer.RaiseBusyRelease(
    const _sender  : TObject
  ) ;
  var
    snd    : TGIS_Layer ;
    wasvwr : Boolean ;
  begin
    wasvwr := False ;

    if _sender is TGIS_Layer  then
      snd := TGIS_Layer( _sender )
    else
      snd := nil ;

    if assigned( snd ) and ( snd <> self ) then begin
      snd.RaiseBusyEvent( _sender, -1, -1 ) ;
    end;
    RaiseBusyEvent( _sender, -1, -1 ) ;

    if assigned( snd ) and ( snd <> self ) then begin
      if assigned( snd.Viewer ) then begin
        snd.Viewer.Ref.BusyRelease( self ) ;
        wasvwr := true ;
      end ;
    end;

    if assigned( Viewer ) and not wasvwr then begin
      Viewer.Ref.BusyRelease( self ) ;
    end ;
  end ;

  function TGIS_Layer.RaiseBusyShake(
    const _sender  : TObject ;
    const _pos     : Int64 ;
    const _end     : Int64
  ) : Boolean ;
  var
    abrt   : Boolean    ;
    snd    : TGIS_Layer ;
    wasvwr : Boolean    ;
  begin
    abrt := False ;
    wasvwr := False ;

    if _sender is TGIS_Layer  then
      snd := TGIS_Layer( _sender )
    else
      snd := nil ;

    if assigned( snd ) and ( snd <> self ) then begin
      if assigned( snd.Viewer ) then begin
        snd.Viewer.Ref.BusyShake( self, _pos, _end, abrt ) ;
        wasvwr := true ;
      end ;
    end ;

    if assigned( Viewer ) and not wasvwr then begin
      Viewer.Ref.BusyShake( self, _pos, _end, abrt ) ;
      wasvwr := true ;
    end ;

    if wasvwr then
      Result := abrt
    else
      Result := RaiseBusyEvent( _sender, _pos, _end ) ;
  end ;

  function TGIS_Layer.RaiseBusyEvent(
    const _sender  : TObject ;
    const _pos     : Int64 ;
    const _end     : Int64
  ) : Boolean ;
  var
    abrt   : Boolean    ;
    snd    : TGIS_Layer ;
    {$IFDEF OXYGENE}
      bargs : TGIS_BusyEventArgs ;
    {$ENDIF}
  begin
    Result := False ;

    if _sender is TGIS_Layer  then
      snd := TGIS_Layer( _sender )
    else
      snd := nil ;

    if assigned( snd ) and ( snd <> self ) then begin
      if assigned( snd.FOnBusy ) then begin
        abrt := False ;
        {$IFDEF OXYGENE}
          bargs := TGIS_BusyEventArgs.Create( _pos, _end, abrt ) ;
          snd.FOnBusy( _sender, bargs ) ;
          Result := bargs.Abort ;
        {$ELSE}
          snd.FOnBusy( _sender, _pos, _end, abrt ) ;
          Result := abrt ;
        {$ENDIF}
      end
    end;

    if assigned( FOnBusy ) then begin
      abrt := False ;
      {$IFDEF OXYGENE}
        bargs := TGIS_BusyEventArgs.Create( _pos, _end, abrt ) ;
        FOnBusy( _sender, bargs ) ;
        Result := bargs.Abort ;
      {$ELSE}
        FOnBusy( _sender, _pos, _end, abrt ) ;
        Result := abrt ;
      {$ENDIF}
    end ;
  end ;

  function TGIS_Layer.GetAvailableLayers : TGIS_LayerInfoList  ;
  begin
    Result := TGIS_LayerInfoList.Create ;
  end ;

  function TGIS_Layer.GetSublayer(
    const _name : String
  ) : TGIS_LayerAbstract;
  var
    k     : Integer ;
    lname : String ;
    sname : String ;
    sl    : TGIS_LayerAbstract ;
  begin
    Result := nil ; // not found
    if SubLayers.Count = 0 then exit ;

    // check for sublayers
    k := Pos( '.', _name ) ;
    if k >= StringFirst then begin
      lname := Copy( _name, StringFirst, k-1 ) ;
      sname := Copy( _name, k+1, MaxInt ) ;
    end
    else begin
      lname := _name ;
      sname := '' ;
    end ;

    for sl in SubLayers do begin
      if ( CompareText( TGIS_Layer( sl ).Name, lname ) = 0 ) then begin
        if IsStringEmpty( sname ) then
          Result := sl
        else
          Result :=  TGIS_Layer( sl ).GetSubLayer(sname) ;
       break ;
      end;
    end ;

  end;

  procedure TGIS_Layer.ClearModified ;
  var
    i : Integer ;
  begin
    FIsModified := False ;

    if assigned( SubLayers ) then
      for i := 0 to SubLayers.Count - 1 do
        TGIS_Layer( SubLayers[ i ] ).ClearModified ;
  end ;


  function TGIS_Layer.MustCalculateStatistics
    : Boolean ;
  begin
    if assigned( Statistics ) then
      Result := Statistics.Modified
    else
      Result := False ;
  end;


  function TGIS_Layer.ViewerReParent(
    const _viewer : TGIS_ViewerRef
  ) : TGIS_ViewerRef ;
  var
    i : Integer ;
  begin
    Result := Viewer ;
    Viewer := _viewer ;
    if assigned( SubLayers ) then
      for i := 0 to SubLayers.Count - 1 do
        if SubLayers[i] is TGIS_Layer then
          TGIS_Layer( SubLayers[i] ).Viewer := _viewer ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_LayerGroup'}
  //==============================================================================

  constructor TGIS_LayerGroup.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.InMemory ] ;

    ParamsList.SetUp( TGIS_ParamsSection.Create ) ;

    Path      := '' ;
    Name      := '' ;
    FFileInfo := 'Generic Group Layer' ;
    FIsModified := False ;
  end ;

  procedure TGIS_LayerGroup.doDestroy ;
  begin
    inherited ;
  end ;

  function TGIS_LayerGroup.DrawEx(
    const _extent : TGIS_Extent
  ) : Boolean ;
  begin
    Result := False ;
  end ;

  procedure TGIS_LayerGroup.DrawFlash ;
  begin
    inherited ;
  end ;

  procedure TGIS_LayerGroup.ReadConfig ;
  begin
    if not assigned( Viewer ) then exit ;
    if not assigned( Viewer.Ref.ProjectFile ) then exit ;

    ParamsList.Clear ;
    with TGIS_Config( Viewer.Ref.ProjectFile ) do begin
      SetGroup( self ) ;
      SetGroupSection( 0 ) ;
      applyConfigOptions( Viewer.Ref.ProjectFile ) ;
      ParamsList.LoadFromConfig( Viewer.Ref.ProjectFile ) ;
    end ;
  end ;

  procedure TGIS_LayerGroup.WriteConfig ;
  begin
    if not assigned( Viewer ) then exit ;
    if not assigned( Viewer.Ref.ProjectFile ) then exit ;

    with TGIS_Config( Viewer.Ref.ProjectFile ) do begin
      SetGroup( self ) ;
      ClearGroups ;

      SetGroupSection( 0 ) ;
      storeConfigOptions( Viewer.Ref.ProjectFile ) ;
      ParamsList.SaveToConfig( Viewer.Ref.ProjectFile, False, True ) ;
    end ;
  end ;

  procedure TGIS_LayerGroup.RevertAll ;
  begin
    inherited ;
  end ;

{$ENDREGION}

{==================================== END =====================================}
end.


