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
  All resources.
}

{$IFDEF DCC}
  unit GisResource ;
  {$HPPEMIT '#pragma link "GisResource"'}
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

{$IFDEF DCC}
  uses
    System.Classes,

    GisTypes ;
{$ENDIF}
{$IFDEF CLR}
  uses
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl;
{$ENDIF}
{$IFDEF COCOA}
  uses
    remobjects.elements.rtl.*,
    TatukGIS.RTL,
    Foundation ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    remobjects.elements.rtl.*,
    TatukGIS.RTL ;
{$ENDIF}

type

  /// <summary>
  ///   Prototype for call-back procedure for GisUpdateLocalizedStrings.
  /// </summary>
  /// <param name="_original">
  ///   text to be replaced
  /// </param>
  /// <param name="_key">
  ///   key name of the entry (like 'GIS_RS_BTN_OK')
  /// </param>
  TGIS_GetLocalizedString = {$IFDEF OXYGENE} public {$ENDIF}
                            procedure( var   _original : String;
                                       const _key      : String
                                     ) of object ;

  /// <summary>
  ///   Prototype for call-back procedure for notifying objects that language
  ///   was changed.
  /// </summary>
  TGIS_LocalizedNotificationProc = {$IFDEF OXYGENE} public {$ENDIF}
                                   procedure of object ;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Notification class. Will manage localization change notifications
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    Use this class always via LocalizedNotification global object.
  ///    </note>
  /// </remarks>
  TGIS_LocalizedNotification = {$IFDEF OXYGENE} public {$ENDIF} class

    private
      lstNotify : TGIS_ObjectList ;

    public
      {$IFNDEF MANAGED}

        /// <summary>
        ///   Destroy an object.
        /// </summary>
        /// <remarks>
        ///   <note type="note">
        ///    Use this class always via LocalizedNotification global object.
        ///    </note>
        /// </remarks>
        destructor Destroy ; override;
      {$ENDIF}

    public

      /// <summary>
      ///   Create an object.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Use this class always via LocalizedNotification global object.
      ///    </note>
      /// </remarks>
      constructor Create ;

      /// <summary>
      ///   Add a procedure to the notification list. Procedure should be
      ///   unsubscribed on Unsubscribe method.
      /// </summary>
      /// <param name="_prc">
      ///   procedure to be subscribed
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Use this class always via LocalizedNotification global object.
      ///    </note>
      /// </remarks>
      procedure  Subscribe  ( const _prc : TGIS_LocalizedNotificationProc ) ;

      /// <summary>
      ///   Remove a procedure form the notification list.
      /// </summary>
      /// <param name="_prc">
      ///   procedure to be unsubscribed
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Use this class always via LocalizedNotification global object.
      ///    </note>
      /// </remarks>
      procedure  UnSubscribe( const _prc : TGIS_LocalizedNotificationProc ) ;

      /// <summary>
      ///   Send a notification to all subscribers.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Use this class always via LocalizedNotification global object.
      ///    </note>
      /// </remarks>
      procedure  Update ;
  end ;

  /// <summary>
  ///   Updates resource string variables with the localized content.
  /// </summary>
  /// <param name="_fn">
  ///   call back function; if nil then default (English) language will be
  ///   setup; if assigned, then will be called for each string identifier like
  ///   _fn('GIS_RS_ERR_UNIMPLEMENTED')
  /// </param>
  procedure GisLocalizedStrings          ( const _fn : TGIS_GetLocalizedString
                                         ) ;

  {#gendoc:hide}
  function _rsrc                         ( const _r  : String
                                         ) : String ;

  {#gendoc:hide}
  function _rsrcna                       ( const _r  : String
                                         ) : String ;
  {#gendoc:hide}
  function _rsbidi                       : Boolean ;

{$IFDEF OXYGENE}
  procedure SelfLocalizeStrings          ;
{$ENDIF}

var

  /// <summary>
  ///   List of subscribers to be notified when localized information will be
  ///   updated.
  /// </summary>
  LocalizedNotification : TGIS_LocalizedNotification ;

const
{$IFNDEF GENDOC}
  {$DEFINE GIS_INCLUDE_CONST}
{$ENDIF}
{$IFDEF GIS_INCLUDE_CONST}
  // various constants
  GIS_PARAM_NIL         = 'nil' ;
  GIS_PARAM_SEPARATOR   = ',' ;
  GIS_RTREE_EXT         = '.rtree' ;
  GIS_TTKGP_EXT         = '.ttkgp' ;
  GIS_TTKGP_NAME        = 'TatukGIS Project File' ;
  GIS_TTKLS_EXT         = '.ttkls' ;
  GIS_TTKPS_EXT         = '.ttkps' ;
  GIS_TTKWV_EXT         = '.ttkwv' ;
  GIS_TTKWP_EXT         = '.ttkwp' ;
  GIS_INI_EXT           = '.ini' ;
  GIS_PRJ_EXT           = '.prj' ;
  GIS_FLD_EXT           = '.fld' ;
  GIS_FLDX_EXT          = '.fldx' ;
  GIS_TRN_EXT           = '.trn' ;
  GIS_SLD_EXT           = '.sld' ;
  GIS_AUX_XML_EXT       = '.aux.xml' ;

  GIS_TTKPROJECT_EXT    = '.ttkproject' ;
  GIS_TTKSTYLE_EXT      = '.ttkstyle' ;
  GIS_TTKLAYER_EXT      = '.ttklayer' ;
  GIS_SYMBOLLIBSVG_EXT  = '.ttklibsvg' ;
  GIS_TTKSTATS_EXT      = '.ttkstats' ;
  GIS_TTKTEMPLATE_EXT   = '.ttktemplate' ;

  GIS_TTKLAYER_VECTOR_FILTER = GIS_TTKLS_EXT       + ';' +
                               GIS_TTKLAYER_EXT ;
  GIS_TTKLAYER_PIXEL_FILTER  = GIS_TTKPS_EXT       + ';' +
                               GIS_TTKLAYER_EXT ;
  GIS_TTKLAYER_WEB_FILTER    = GIS_TTKWP_EXT       + ';' +
                               GIS_TTKLAYER_EXT ;
  GIS_TTKPROJECT_FILTER      = GIS_TTKGP_EXT       + ';' +
                               GIS_TTKPROJECT_EXT ;

  GIS_SQL_LAYER_CONNECTOR       = 'SQL Layer Connector' ;
  GIS_PROTOCOL_LAYER_CONNECTOR  = 'Protocol Layer Connector' ;
  GIS_AGENT_NAME                = 'TatukGIS Agent' ;

  // common files paths
  GIS_COMMONFILES_TATUKGIS   = 'TatukGIS'     ;
  GIS_COMMONFILES_GEOCODING  = 'Geocoding'    ;
  GIS_COMMONFILES_SQLDIALECT = 'SQLDialect'   ;

  // common pipeline params
  GIS_PIPELINE_PARAM_RESULT        = 'Result' ;
  GIS_PIPELINE_PARAM_LAYER         = 'Layer' ;
  GIS_PIPELINE_PARAM_NAME          = 'Name' ;
  GIS_PIPELINE_PARAM_PATH          = 'Path' ;
  GIS_PIPELINE_PARAM_CS            = 'CS' ;
  GIS_PIPELINE_PARAM_EXTENT        = 'Extent' ;
  GIS_PIPELINE_PARAM_SOURCE        = 'Source' ;
  GIS_PIPELINE_PARAM_DESTINATION   = 'Destination' ;
  GIS_PIPELINE_PARAM_FIELD         = 'Field' ;
  GIS_PIPELINE_PARAM_SAVE          = 'Save' ;

  GIS_PIPELINE_VAR_GIS             = '$GIS' ;

  GIS_PIPELINE_DEFAULT_CS          = '$cs1' ;
  GIS_PIPELINE_DEFAULT_EXTENT      = '$ext1' ;
  GIS_PIPELINE_DEFAULT_LAYER       = '$layer1' ;
  GIS_PIPELINE_DEFAULT_PATH        = 'c:\path\to\layer.extension' ;
  GIS_PIPELINE_DEFAULT_SOURCE      = '$src' ;
  GIS_PIPELINE_DEFAULT_DESTINATION = '$dst' ;
  GIS_PIPELINE_DEFAULT_SAVE        = True ;

  // common config file
  GIS_CONFIG_FILE           = 'TatukGIS.ini' ;
  GIS_CONFIG_FILE_SAMPLES   = 'Samples11'    ;
  GIS_CONFIG_FILE_DATADIR   = 'DataDir'      ;
  GIS_CONFIG_DUMMY          = '_DUMMY'       ;

  GIS_CONFIG_VER_DK10_MAJOR = 10 ;
  GIS_CONFIG_VER_DK10_MINOR = 0  ;
  GIS_CONFIG_VER_DK11_MAJOR = 11 ;
  GIS_CONFIG_VER_DK11_MINOR = 0  ;

  // common field descriptors
  GIS_FIELD_JOINPREFIX       = 'db.'        ;

  // default directories for DLLs
  GIS_DEFAULT_DLL_PATH_X86        = 'x86'        ;
  GIS_DEFAULT_DLL_PATH_X64        = 'x64'        ;
  GIS_DEFAULT_DLL_PATH_ARM32      = 'ARM'        ;
  GIS_DEFAULT_DLL_PATH_ARM64      = 'ARM64'      ;

  // names of ini parameters - don't change this
  GIS_INI_GENERAL_HEADER          = 'TatukGIS'           ;
  GIS_INI_LAYER_HEADER            = 'TatukGIS Layer'     ;
  GIS_INI_GROUP_HEADER            = 'TatukGIS Group'     ;
  GIS_INI_HIERARCHY_HEADER        = 'TatukGIS Hierarchy' ;
  GIS_INI_HIERARCHY_ALIAS_HEADER  = 'TatukGIS Hierarchy Alias' ;
  GIS_INI_FIELDS_HEADER           = 'TatukGIS Fields'    ;
  GIS_INI_HIERARCHY_INDEX         = 'Index' ;

  GIS_XML_GENERAL_TAG                     = 'TatukGIS'    ;
  GIS_XML_VIEWER_TAG                      = 'Viewer'      ;
  GIS_XML_LAYERS_TAG                      = 'Layers'      ;
  GIS_XML_LAYER_TAG                       = 'Layer'       ;
  GIS_XML_HIERARCHY_TAG                   = 'Hierarchy'   ;
  GIS_XML_SECTIONS_TAG                    = 'Sections'    ;
  GIS_XML_GROUPS_TAG                      = 'Groups'      ;
  GIS_XML_GROUP_TAG                       = 'Group'       ;
  GIS_XML_SECTION_TAG                     = 'Section'     ;
  GIS_XML_SUBLAYERS_TAG                   = 'Sublayers'   ;
  GIS_XML_SUBLAYER_TAG                    = 'Sublayer'    ;
  GIS_XML_ZONE_TAG                        = 'Zone'        ;
  GIS_XML_ID_TAG                          = 'id'          ;
  GIS_XML_VALUE_TAG                       = 'v'           ;
  GIS_XML_PIXEL_COLORRAMP                 = 'Pixel.ColorRamp';
  GIS_XML_RENDER_COLORRAMP                = 'Render.ColorRamp';
  GIS_XML_RENDER_INTERPOLATION_BASE       = 'Render.InterpolationBase';
  GIS_XML_RENDER_COLOR_INTERPOLATION_MODE = 'Render.ColorInterpolationMode';

  // common parameters
  GIS_INI_BACKGROUNDCOLOR            = 'BackgroundColor'       ;
  GIS_INI_TILEPICTURE                = 'TilePicture'           ;
  GIS_INI_RESTRICTED_XMIN            = 'RestrictedXMin'        ;
  GIS_INI_RESTRICTED_XMAX            = 'RestrictedXMax'        ;
  GIS_INI_RESTRICTED_YMIN            = 'RestrictedYMin'        ;
  GIS_INI_RESTRICTED_YMAX            = 'RestrictedYMax'        ;

  GIS_INI_CODEPAGE                   = 'CodePage'              ;
  GIS_INI_UNITS                      = 'Units'                 ;
  GIS_INI_CATEGORY                   = 'Category'              ;

  GIS_INI_CS_WKT                     = 'CS.WKT'                ;
  GIS_INI_CS_EPSG                    = 'CS.EPSG'               ;

  GIS_INI_CUSTOM                     = 'Custom'                ;

  // common layer parameters
  GIS_INI_PATH                       = 'Path'                  ;
  GIS_INI_NAME                       = 'Name'                  ;
  GIS_INI_CAPTION                    = 'Caption'               ;
  GIS_INI_COMMENTS                   = 'Comments'              ;
  GIS_INI_ACTIVE                     = 'Active'                ;
  GIS_INI_CONFIG                     = 'Config'                ;
  GIS_INI_DORMANTMODE                = 'DormantMode'           ;
  GIS_INI_HIDEFROMLEGEND             = 'HideFromLegend'        ;
  GIS_INI_STYLE                      = 'Style'                 ;
  GIS_INI_MINLEVEL                   = 'MinLevel'              ;
  GIS_INI_MAXLEVEL                   = 'MaxLevel'              ;
  GIS_INI_MINSCALE                   = 'MinScale'              ;
  GIS_INI_MAXSCALE                   = 'MaxScale'              ;
  GIS_INI_MINZOOM                    = 'MinZoom'               ;
  GIS_INI_MAXZOOM                    = 'MaxZoom'               ;
  GIS_INI_IGNORESHAPEPARAMS          = 'IgnoreShapeParams'     ;
  GIS_INI_MULTIPASSRENDERING         = 'MultipassRendering'    ;
  GIS_INI_TRANSPARENCY               = 'Transparency'          ;
  GIS_INI_ADDITION                   = 'Addition'              ;
  GIS_INI_INCREMENTALPAINT           = 'IncrementalPaint'      ;
  GIS_INI_BASEMAP                    = 'Basemap'               ;
  GIS_INI_CACHEDPAINT                = 'CachedPaint'           ;
  GIS_INI_VISIBLE                    = 'Visible'               ;
  GIS_INI_COLLAPSED                  = 'Collapsed'             ;
  GIS_INI_LEGEND                     = 'Legend'                ;
  GIS_INI_MULTIUSER_MODE             = 'MultiUserMode'         ;
  GIS_INI_MULTIUSER_MODE_SINGLE      = 'SingleUser'            ;
  GIS_INI_MULTIUSER_MODE_MULTI       = 'MultiUser'             ;
  GIS_INI_MULTIUSER_MODE_DEFAULT     = 'Default'               ;
  GIS_INI_PARAMSFIELD_FIELD          = '.Field'                ;
  GIS_INI_PARAMSFIELD_FACTOR         = '.Factor'               ;
  GIS_INI_PARAMSFIELD_SCALABLE       = '.Scalable'             ;
  GIS_INI_PARAMSFIELD_SUBTYPE        = '.SubType'              ; 
  GIS_INI_BBOX_IGNORED               = 'BBoxIgnored'           ;
  GIS_INI_EXTENT_PIXEL_ADJUSTMENT    = 'ExtentPixelAdjustment' ;
  GIS_INI_AXIS_ORDER                 = 'AxisOrder'             ;
  GIS_INI_AXIS_ORDER_IGNORED         = 'AxisOrderIgnored'      ;
  GIS_INI_AXIS_ORDER_REVERSED        = 'AxisOrderReversed'     ;
  GIS_INI_AXIS_ORDER_EN              = 'EN'                    ;
  GIS_INI_AXIS_ORDER_NE              = 'NE'                    ;
  GIS_INI_AXIS_EASTING               = 'E'                     ;
  GIS_INI_AXIS_NORTHING              = 'N'                     ;
  GIS_INI_AXIS_WESTING               = 'W'                     ;
  GIS_INI_AXIS_SOUTHING              = 'S'                     ;
  GIS_INI_FALSEZ                     = 'FalseZ'                ;
  GIS_INI_SCALEZ                     = 'ScaleZ'                ;
  GIS_INI_NORMALIZEDZ                = 'NormalizedZ'           ;
  GIS_INI_FALSEZEX                   = 'FalseZEx'              ;
  GIS_INI_FLATTEN_XML                = 'FlattenXML'            ;
  GIS_INI_TIMEOUT                    = 'TimeOut'               ;
  GIS_INI_READONLY                   = 'ReadOnly'              ;

  GIS_INI_MINZ                       = 'MinHeight'             ;
  GIS_INI_MAXZ                       = 'MaxHeight'             ;
  GIS_INI_ANTIALIAS                  = 'Antialias'             ;
  GIS_INI_INTERPRETATION             = 'Interpretation'        ;
  GIS_INI_INTERPRETATION_DEFAULT     = 'Default'               ;
  GIS_INI_INTERPRETATION_PIXEL       = 'Pixel'                 ;
  GIS_INI_INTERPRETATION_GRID        = 'Grid'                  ;

  GIS_INI_STEM_SMARTSIZE             = 'SmartSize'             ;
  GIS_INI_STEM_PATTERN               = 'Pattern'               ;
  GIS_INI_STEM_COLOR                 = 'Color'                 ;
  GIS_INI_STEM_OUTLINESTYLE          = 'OutlineStyle'          ;
  GIS_INI_STEM_OUTLINEPATTERN        = 'OutlinePattern'        ;
  GIS_INI_STEM_OUTLINECOLOR          = 'OutlineColor'          ;
  GIS_INI_STEM_OUTLINEWIDTH          = 'OutlineWidth'          ;
  GIS_INI_STEM_SHIELD                = 'Shield'                ;

  // vector layer parameters
  GIS_INI_SCOPE                      = 'Scope'                 ;
  GIS_INI_SCOPEEXTENT_XMIN           = 'ScopeExtent.XMin'      ;
  GIS_INI_SCOPEEXTENT_XMAX           = 'ScopeExtent.XMax'      ;
  GIS_INI_SCOPEEXTENT_YMIN           = 'ScopeExtent.YMin'      ;
  GIS_INI_SCOPEEXTENT_YMAX           = 'ScopeExtent.YMax'      ;
  GIS_INI_QUERY                      = 'Query'                 ;
  GIS_INI_JOIN_ADO                   = 'JoinADO'               ;
  GIS_INI_JOIN_PRIMARY               = 'JoinPrimary'           ;
  GIS_INI_JOIN_FOREIGN               = 'JoinForeign'           ;

  GIS_INI_RENDER_EXPRESSION          = 'Render.Expression'     ;
  GIS_INI_RENDER_CHART               = 'Render.Chart'          ;
  GIS_INI_RENDER_MINVAL              = 'Render.MinVal'         ;
  GIS_INI_RENDER_MAXVAL              = 'Render.MaxVal'         ;
  GIS_INI_RENDER_ZONES               = 'Render.Zones'          ;
  GIS_INI_RENDER_MINVAL_EX           = 'Render.MinValEx'       ;
  GIS_INI_RENDER_MAXVAL_EX           = 'Render.MaxValEx'       ;
  GIS_INI_RENDER_ZONES_EX            = 'Render.ZonesEx'        ;
  GIS_INI_RENDER_SIZE_DEF            = 'Render.SizeDefault'    ;
  GIS_INI_RENDER_COLOR_DEF           = 'Render.ColorDefault'   ;
  GIS_INI_RENDER_STARTSIZE           = 'Render.StartSize'      ;
  GIS_INI_RENDER_ENDSIZE             = 'Render.EndSize'        ;
  GIS_INI_RENDER_STARTCOLOR          = 'Render.StartColor'     ;
  GIS_INI_RENDER_ENDCOLOR            = 'Render.EndColor'       ;
  GIS_INI_RENDER_STARTSIZE_EX        = 'Render.StartSizeEx'    ;
  GIS_INI_RENDER_ENDSIZE_EX          = 'Render.EndSizeEx'      ;
  GIS_INI_RENDER_STARTCOLOR_EX       = 'Render.StartColorEx'   ;
  GIS_INI_RENDER_ENDCOLOR_EX         = 'Render.EndColorEx'     ;
  GIS_INI_RENDER_ROUND               = 'Render.Round'          ;

  GIS_INI_LINE_STYLE                 = 'Line.Style'            ;
  GIS_INI_LINE_STYLEEX               = 'Line.StyleEx'          ;
  GIS_INI_LINE_WIDTH                 = 'Line.Width'            ;
  GIS_INI_LINE_WIDTHEX               = 'Line.WidthEx'          ;
  GIS_INI_LINE_COLOR                 = 'Line.Color'            ;
  GIS_INI_LINE_COLOREX               = 'Line.ColorEx'          ;
  GIS_INI_LINE_BITMAP                = 'Line.Bitmap'           ;
  GIS_INI_LINE_PATTERN               = 'Line.Pattern'          ;
  GIS_INI_LINE_SYMBOL                = 'Line.Symbol'           ;
  GIS_INI_LINE_SYMBOLGAP             = 'Line.SymbolGap'        ;
  GIS_INI_LINE_SYMBOLROTATE          = 'Line.SymbolRotate'     ;
  GIS_INI_LINE_SYMBOLROTATEEX        = 'Line.SymbolRotateEx'   ;
  GIS_INI_LINE_OUTLINESTYLE          = 'Line.OutlineStyle'     ;
  GIS_INI_LINE_OUTLINESTYLEEX        = 'Line.OutlineStyleEx'   ;
  GIS_INI_LINE_OUTLINEWIDTH          = 'Line.OutlineWidth'     ;
  GIS_INI_LINE_OUTLINEWIDTHEX        = 'Line.OutlineWidthEx'   ;
  GIS_INI_LINE_OUTLINECOLOR          = 'Line.OutlineColor'     ;
  GIS_INI_LINE_OUTLINECOLOREX        = 'Line.OutlineColorEx'   ;
  GIS_INI_LINE_OUTLINEBITMAP         = 'Line.OutlineBitmap'    ;
  GIS_INI_LINE_OUTLINEPATTERN        = 'Line.OutlinePattern'   ;
  GIS_INI_LINE_SMARTSIZE             = 'Line.SmartSize'        ;
  GIS_INI_LINE_SMARTFIELD            = 'Line.SmartSizeEx.Field';
  GIS_INI_LINE_SHOWLEGEND            = 'Line.ShowLegend'       ;
  GIS_INI_LINE_OFFSETX               = 'Line.OffsetX'          ;
  GIS_INI_LINE_OFFSETXEX             = 'Line.OffsetXEx'        ;
  GIS_INI_LINE_OFFSETY               = 'Line.OffsetY'          ;
  GIS_INI_LINE_OFFSETYEX             = 'Line.OffsetYEx'        ;
  GIS_INI_LINE_OFFSETPOSITION        = 'Line.OffsetPosition'   ;

  GIS_INI_AREA_COLOR                 = 'Area.Color'            ;
  GIS_INI_AREA_COLOREX               = 'Area.ColorEx'          ;
  GIS_INI_AREA_BITMAP                = 'Area.Bitmap'           ;
  GIS_INI_AREA_PATTERN               = 'Area.Pattern'          ;
  GIS_INI_AREA_SYMBOL                = 'Area.Symbol'           ;
  GIS_INI_AREA_SYMBOLSIZE            = 'Area.SymbolSize'       ;
  GIS_INI_AREA_SYMBOLGAP             = 'Area.SymbolGap'        ;
  GIS_INI_AREA_SYMBOLROTATE          = 'Area.SymbolRotate'     ;
  GIS_INI_AREA_SYMBOLROTATEEX        = 'Area.SymbolRotateEx'   ;
  GIS_INI_AREA_OUTLINESTYLE          = 'Area.OutlineStyle'     ;
  GIS_INI_AREA_OUTLINESTYLEEX        = 'Area.OutlineStyleEx'   ;
  GIS_INI_AREA_OUTLINEWIDTH          = 'Area.OutlineWidth'     ;
  GIS_INI_AREA_OUTLINEWIDTHEX        = 'Area.OutlineWidthEx'   ;
  GIS_INI_AREA_OUTLINECOLOR          = 'Area.OutlineColor'     ;
  GIS_INI_AREA_OUTLINECOLOREX        = 'Area.OutlineColorEx'   ;
  GIS_INI_AREA_OUTLINEBITMAP         = 'Area.OutlineBitmap'    ;
  GIS_INI_AREA_OUTLINEPATTERN        = 'Area.OutlinePattern'   ;
  GIS_INI_AREA_OUTLINESYMBOL         = 'Area.OutlineSymbol'    ;
  GIS_INI_AREA_OUTLINESYMBOLGAP      = 'Area.OutlineGap'       ;
  GIS_INI_AREA_OUTLINESYMBOLROTATE   = 'Area.OutlineRotate'    ;
  GIS_INI_AREA_SMARTSIZE             = 'Area.SmartSize'        ;
  GIS_INI_AREA_SMARTFIELD            = 'Area.SmartSizeEx.Field';
  GIS_INI_AREA_SHOWLEGEND            = 'Area.ShowLegend'       ;
  GIS_INI_AREA_OFFSETX               = 'Area.OffsetX'          ;
  GIS_INI_AREA_OFFSETXEX             = 'Area.OffsetXEx'        ;
  GIS_INI_AREA_OFFSETY               = 'Area.OffsetY'          ;
  GIS_INI_AREA_OFFSETYEX             = 'Area.OffsetYEx'        ;
  GIS_INI_AREA_OFFSETPOSITION        = 'Area.OffsetPosition'   ;

  GIS_INI_MARKER_STYLE               = 'Marker.Style'          ;
  GIS_INI_MARKER_SIZE                = 'Marker.Size'           ;
  GIS_INI_MARKER_SIZEEX              = 'Marker.SizeEx'         ;
  GIS_INI_MARKER_COLOR               = 'Marker.Color'          ;
  GIS_INI_MARKER_COLOREX             = 'Marker.ColorEx'        ;
  GIS_INI_MARKER_BITMAP              = 'Marker.Bitmap'         ;
  GIS_INI_MARKER_PATTERN             = 'Marker.Pattern'        ;
  GIS_INI_MARKER_SYMBOL              = 'Marker.Symbol'         ;
  GIS_INI_MARKER_SYMBOLROTATE        = 'Marker.SymbolRotate'   ;
  GIS_INI_MARKER_SYMBOLROTATEEX      = 'Marker.SymbolRotateEx' ;
  GIS_INI_MARKER_OUTLINESTYLE        = 'Marker.OutlineStyle'   ;
  GIS_INI_MARKER_OUTLINEWIDTH        = 'Marker.OutlineWidth'   ;
  GIS_INI_MARKER_OUTLINEWIDTHEX      = 'Marker.OutlineWidthEx' ;
  GIS_INI_MARKER_OUTLINECOLOR        = 'Marker.OutlineColor'   ;
  GIS_INI_MARKER_OUTLINECOLOREX      = 'Marker.OutlineColorEx' ;
  GIS_INI_MARKER_OUTLINEBITMAP       = 'Marker.OutlineBitmap'  ;
  GIS_INI_MARKER_OUTLINEPATTERN      = 'Marker.OutlinePattern' ;
  GIS_INI_MARKER_SMARTSIZE           = 'Marker.SmartSize'      ;
  GIS_INI_MARKER_SMARTFIELD          = 'Marker.SmartSizeEx.Field';
  GIS_INI_MARKER_SHOWLEGEND          = 'Marker.ShowLegend'     ;
  GIS_INI_MARKER_OFFSETX             = 'Marker.OffsetX'        ;
  GIS_INI_MARKER_OFFSETXEX           = 'Marker.OffsetXEx'      ;
  GIS_INI_MARKER_OFFSETY             = 'Marker.OffsetY'        ;
  GIS_INI_MARKER_OFFSETYEX           = 'Marker.OffsetYEx'      ;
  GIS_INI_MARKER_OFFSETPOSITION      = 'Marker.OffsetPosition' ;

  GIS_INI_LABEL_VISIBLE              = 'Label.Visible'         ;
  GIS_INI_LABEL_ALLOCATOR            = 'Label.Allocator'       ;
  GIS_INI_LABEL_DUPLICATES           = 'Label.Duplicates'      ;
  GIS_INI_LABEL_FIELD                = 'Label.Field'           ;
  GIS_INI_LABEL_VALUE                = 'Label.Value'           ;
  GIS_INI_LABEL_FONTNAME             = 'Label.Font.Name'       ;
  GIS_INI_LABEL_FONTSIZEPT           = 'Label.Font.Size'       ;
  GIS_INI_LABEL_FONTSIZE             = 'Label.FontSize'        ;
  GIS_INI_LABEL_FONTSTYLE            = 'Label.Font.Style'      ;
  GIS_INI_LABEL_FONTCOLOR            = 'Label.Font.Color'      ;
  GIS_INI_LABEL_FONTCOLOREX          = 'Label.Font.ColorEx'    ;
  GIS_INI_LABEL_FONTSIZEEX           = 'Label.Font.SizeEx'     ;
  GIS_INI_LABEL_WIDTH                = 'Label.Width'           ;
  GIS_INI_LABEL_HEIGHT               = 'Label.Height'          ;
  GIS_INI_LABEL_POSITION             = 'Label.Position'        ;
  GIS_INI_LABEL_POSITIONEX           = 'Label.PositionEx'      ;
  GIS_INI_LABEL_ALIGNMENT            = 'Label.Alignment'       ;
  GIS_INI_LABEL_ROTATE               = 'Label.Rotate'          ;
  GIS_INI_LABEL_ROTATEEX             = 'Label.RotateEx'        ;
  GIS_INI_LABEL_COLOR                = 'Label.Color'           ;
  GIS_INI_LABEL_COLOREX              = 'Label.ColorEx'         ;
  GIS_INI_LABEL_BITMAP               = 'Label.Bitmap'          ;
  GIS_INI_LABEL_PATTERN              = 'Label.Pattern'         ;
  GIS_INI_LABEL_SHIELD               = 'Label.Shield'          ;
  GIS_INI_LABEL_OUTLINESTYLE         = 'Label.OutlineStyle'    ;
  GIS_INI_LABEL_OUTLINEWIDTH         = 'Label.OutlineWidth'    ;
  GIS_INI_LABEL_OUTLINEWIDTHEX       = 'Label.OutlineWidthEx'  ;
  GIS_INI_LABEL_OUTLINECOLOR         = 'Label.OutlineColor'    ;
  GIS_INI_LABEL_OUTLINECOLOREX       = 'Label.OutlineColorEx'  ;
  GIS_INI_LABEL_OUTLINEBITMAP        = 'Label.OutlineBitmap'   ;
  GIS_INI_LABEL_OUTLINEPATTERN       = 'Label.OutlinePattern'  ;
  GIS_INI_LABEL_SMARTSIZE            = 'Label.SmartSize'       ;
  GIS_INI_LABEL_SMARTFIELD           = 'Label.SmartSizeEx.Field';
  GIS_INI_LABEL_SHOWLEGEND           = 'Label.ShowLegend'      ;
  GIS_INI_LABEL_OFFSETX              = 'Label.OffsetX'         ;
  GIS_INI_LABEL_OFFSETXEX            = 'Label.OffsetXEx'       ;
  GIS_INI_LABEL_OFFSETY              = 'Label.OffsetY'         ;
  GIS_INI_LABEL_OFFSETYEX            = 'Label.OffsetYEx'       ;
  GIS_INI_LABEL_OFFSETPOSITION       = 'Label.OffsetPosition'  ;

  GIS_INI_CHART_STYLE                = 'Chart.Style'           ;
  GIS_INI_CHART_SIZE                 = 'Chart.Size'            ;
  GIS_INI_CHART_VALUES               = 'Chart.Values'          ;
  GIS_INI_CHART_COLORS               = 'Chart.Colors'          ;
  GIS_INI_CHART_LEGEND               = 'Chart.Legend'          ;
  GIS_INI_CHART_SHOWLEGEND           = 'Chart.ShowLegend'      ;

  GIS_INI_FALSEM                     = 'FalseM'                ;
  GIS_INI_FALSEMEX                   = 'FalseMEx'              ;
  GIS_INI_SCALEM                     = 'ScaleM'                ;
  GIS_INI_NORMALIZEDM                = 'NormalizedM'           ;
  GIS_INI_GROUND                     = 'Ground'                ;
  GIS_INI_BASEMENT                   = 'Basement'              ;
  GIS_INI_3DLAYERTYPE                = 'Layer3D'               ;

  // pixel layer parameters
  GIS_INI_CURRENTPAGE                = 'CurrentPage'           ;
  GIS_INI_PIXELSTORE                 = 'PixelStore'            ;
  GIS_INI_PIXEL_PAGE                 = 'Pixel.Page'            ;
  GIS_INI_PIXEL_REDBAND              = 'Pixel.RedBand'         ;
  GIS_INI_PIXEL_GREENBAND            = 'Pixel.GreenBand'       ;
  GIS_INI_PIXEL_BLUEBAND             = 'Pixel.BlueBand'        ;
  GIS_INI_PIXEL_ALPHABAND            = 'Pixel.AlphaBand'       ;
  GIS_INI_PIXEL_GRIDBAND             = 'Pixel.GridBand'        ;
  GIS_INI_PIXEL_GRIDNOVALUE          = 'Pixel.GridNoValue'     ;
  GIS_INI_PIXEL_GRIDSHADOW           = 'Pixel.GridShadow'      ;
  GIS_INI_PIXEL_GRIDSHADOWANGLE      = 'Pixel.GridShadowAngle' ;
  GIS_INI_PIXEL_GRIDSMOOTHCOLORS     = 'Pixel.GridSmoothColors';
  GIS_INI_PIXEL_RED                  = 'Pixel.Red'             ;
  GIS_INI_PIXEL_GREEN                = 'Pixel.Green'           ;
  GIS_INI_PIXEL_BLUE                 = 'Pixel.Blue'            ;
  GIS_INI_PIXEL_BRIGHTNESS           = 'Pixel.Brightness'      ;
  GIS_INI_PIXEL_CONTRAST             = 'Pixel.Contrast'        ;
  GIS_INI_PIXEL_CONTRASTENHANCED     = 'Pixel.ContrastEnhanced';
  GIS_INI_PIXEL_MINHEIGHTTHRESHOLD   = 'Pixel.MinHeightThreshold';
  GIS_INI_PIXEL_MAXHEIGHTTHRESHOLD   = 'Pixel.MaxHeightThreshold';
  GIS_INI_PIXEL_GRAYSCALE            = 'Pixel.GrayScale'       ;
  GIS_INI_PIXEL_INVERSION            = 'Pixel.Inversion'       ;
  GIS_INI_PIXEL_TRANSPARENTZONE      = 'Pixel.TransparentZone' ;
  GIS_INI_PIXEL_GRAYMAPZONE          = 'Pixel.GrayMapZone'     ;
  GIS_INI_PIXEL_REDMAPZONE           = 'Pixel.RedMapZone'      ;
  GIS_INI_PIXEL_GREENMAPZONE         = 'Pixel.GreenMapZone'    ;
  GIS_INI_PIXEL_BLUEMAPZONE          = 'Pixel.BlueMapZone'     ;
  GIS_INI_PIXEL_FULLRGBMAPZONE       = 'Pixel.FullRGBMapZone'  ;
  GIS_INI_PIXEL_ALTITUDEMAPZONE      = 'Pixel.AltitudeMapZone' ;
  GIS_INI_PIXEL_HISTOGRAM            = 'Pixel.Histogram'       ;
  GIS_INI_PIXEL_HISTOGRAMPATH        = 'Pixel.HistogramPath'   ;
  GIS_INI_PIXEL_SAVEHISTOGRAM        = 'Pixel.SaveHistogram'   ;
  GIS_INI_PIXEL_LOADHISTOGRAM        = 'Pixel.LoadHistogram'   ;
  GIS_INI_PIXEL_SHOWLEGEND           = 'Pixel.ShowLegend'      ;
  GIS_INI_PIXEL_LEGENDIMAGE          = 'Pixel.LegendImage'     ;
  GIS_INI_PIXEL_ANTIALIAS            = 'Pixel.Antialias'       ;

  // sql layer parameters
  GIS_INI_LAYERSQL                         = 'LayerSQL'              ;
  GIS_INI_LAYERSQL_DEFAULT_DATABASE        = 'Layers.mdb'            ;
  GIS_INI_LAYERSQL_DEFAULT_DATABASE_SQLITE = 'Layers.sqlite'         ;
  GIS_INI_LAYERSQL_MDB_DATABASE            = '%s.mdb'                ;
  GIS_INI_LAYERSQL_CONNECTOR               = 'Connector'             ;
  GIS_INI_LAYERSQL_CONNECTOR_ADO           = 'ADO'                   ;
  GIS_INI_LAYERSQL_CONNECTOR_ADO64         = 'ADO64'                 ;
  GIS_INI_LAYERSQL_CONNECTOR_ADONET        = 'ADONET'                ;
  GIS_INI_LAYERSQL_CONNECTOR_JDBC          = 'JDBC'                  ;
  GIS_INI_LAYERSQL_CONNECTOR_PROVIDER      = 'Provider'              ;
  GIS_INI_LAYERSQL_CONNECTOR_DRIVER        = 'Driver'                ;
  GIS_INI_LAYERSQL_CONNECTOR_DBX           = 'DriverName'            ;
  GIS_INI_LAYERSQL_CONNECTOR_FIREDAC       = 'DriverID'              ;
  GIS_INI_LAYERSQL_CONNECTOR_ZDBC          = 'Zdbc'                  ;
  GIS_INI_LAYERSQL_CONNECTOR_SQLITE        = 'Sqlite'                ;
  GIS_INI_LAYERSQL_CONNECTOR_OCI           = 'OCI'                   ;
  GIS_INI_LAYERSQL_CONNECTOR_LIBPQ         = 'LIBPQ'                 ;
  GIS_INI_LAYERSQL_ZDBC_PARAMETERS         = 'ZdbcParameters'        ;
  GIS_INI_LAYERSQL_CONNECTOR_NXDB          = 'NxDB'                  ;
  GIS_INI_LAYERSQL_DATASET                 = 'Dataset'               ;
  GIS_INI_LAYERSQL_LAYER                   = 'Layer'                 ;
  GIS_INI_LAYERSQL_FEATURES                = 'Features'              ;
  GIS_INI_LAYERSQL_READONLY                = 'ReadOnly'              ;
  GIS_INI_LAYERSQL_DIALECT                 = 'Dialect'               ;
  GIS_INI_LAYERSQL_OPENGIS                 = 'OpenGIS'               ;
  GIS_INI_LAYERSQL_STORAGE                 = 'Storage'               ;
  GIS_INI_LAYERSQL_NATIVE                  = 'Native'                ;
  GIS_INI_LAYERSQL_OPENGISBLOB             = 'OpenGisBlob'           ;
  GIS_INI_LAYERSQL_OPENGISWKT              = 'OpenGisWkt'            ;
  GIS_INI_LAYERSQL_OPENGISBLOB2            = 'OpenGisBlob2'          ;
  GIS_INI_LAYERSQL_OPENGISNORMALIZED       = 'OpenGisNormalized'     ;
  GIS_INI_LAYERSQL_OPENGISNORMALIZED2      = 'OpenGisNormalized2'    ;
  GIS_INI_LAYERSQL_POSTGIS                 = 'PostGis'               ;
  GIS_INI_LAYERSQL_KATMAI                  = 'Katmai'                ;
  GIS_INI_LAYERSQL_GEOMEDIA                = 'Geomedia'              ;
  GIS_INI_LAYERSQL_GEOMEDIAMSSPATIAL       = 'GeomediaMsSpatial'     ;
  GIS_INI_LAYERSQL_GEOMEDIAORACLESPATIAL   = 'GeomediaOracleSpatial' ;
  GIS_INI_LAYERSQL_SPATIALWARE             = 'SpatialWare'           ;
  GIS_INI_LAYERSQL_ORACLESPATIAL           = 'OracleSpatial'         ;
  GIS_INI_LAYERSQL_ORACLESPATIAL_PC        = 'OracleSpatialPc'       ;
  GIS_INI_LAYERSQL_ORACLESPATIAL_TIN       = 'OracleSpatialTin'      ;
  GIS_INI_LAYERSQL_ORACLEGEORASTER         = 'OracleGeoraster'       ;
  GIS_INI_LAYERSQL_DB2GSE                  = 'DB2SpatialExtender'    ;
  GIS_INI_LAYERSQL_IFXSDB                  = 'IfxSpatialDataBlade'   ;
  GIS_INI_LAYERSQL_ANYWHERESPATIAL         = 'AnywhereSpatial'       ;
  GIS_INI_LAYERSQL_PIXELSTORE2             = 'PixelStore2'           ;
  GIS_INI_LAYERSQL_PERSONALGDB             = 'PersonalGdb'           ;
  GIS_INI_LAYERSQL_FILEGDB                 = 'FileGDB'               ;
  GIS_INI_LAYERSQL_OGR                     = 'OGR'                   ;
  GIS_INI_LAYERSQL_GDAL                    = 'GDAL'                  ;
  GIS_INI_LAYERSQL_SDEBINARY               = 'SdeBinary'             ;
  GIS_INI_LAYERSQL_SDEOGCWKB               = 'SdeOgcWkb'             ;
  GIS_INI_LAYERSQL_SDELOB                  = 'SdeLob'                ;
  GIS_INI_LAYERSQL_SDEGEOMETRY             = 'SdeGeometry'           ;
  GIS_INI_LAYERSQL_SDEGEOGRAPHY            = 'SdeGeography'          ;
  GIS_INI_LAYERSQL_SDESTGEOMETRY           = 'SdeStGeometry'         ;
  GIS_INI_LAYERSQL_SDESDOGEOMETRY          = 'SdeSdoGeometry'        ;
  GIS_INI_LAYERSQL_SDERASTER               = 'SdeRaster'             ;
  GIS_INI_LAYERSQL_GPKG                    = 'Gpkg'                  ;
  GIS_INI_LAYERSQL_VERSION                 = 'Version'               ;
  GIS_INI_LAYERSQL_VERSION_OLD             = 'Old'                   ;
  GIS_INI_LAYERSQL_VERSION_NEW             = 'New'                   ;
  GIS_INI_LAYERSQL_GEOMETRY_FIELDCAST      = 'GeometryFieldCast'     ;
  GIS_INI_LAYERSQL_GEOMETRY_BINARY         = 'Binary'                ;
  GIS_INI_LAYERSQL_GEOMETRY_NATIVE         = 'Native'                ;
  GIS_INI_LAYERSQL_GEOMETRY_TEXT           = 'Text'                  ;
  GIS_INI_LAYERSQL_GEOMETRY_EWKB           = 'EWKB'                  ;
  GIS_INI_LAYERSQL_GEOMETRY_LONGRAW        = 'LongRaw'               ;
  GIS_INI_LAYERSQL_GEOMETRY_RELATIONSHIPS  = 'GeometryRelationships' ;
  GIS_INI_LAYERSQL_GEOMETRY_SERVER         = 'Server'                ;
  GIS_INI_LAYERSQL_GEOMETRY_CLIENT         = 'Client'                ;
  GIS_INI_LAYERSQL_FALSEX                  = 'XFalse'                ;
  GIS_INI_LAYERSQL_FALSEY                  = 'YFalse'                ;
  GIS_INI_LAYERSQL_FALSEZ                  = 'ZFalse'                ;
  GIS_INI_LAYERSQL_FALSEM                  = 'MFalse'                ;
  GIS_INI_LAYERSQL_SCALEXY                 = 'ScaleXY'               ;
  GIS_INI_LAYERSQL_SCALEZ                  = 'ScaleZ'                ;
  GIS_INI_LAYERSQL_SCALEM                  = 'ScaleM'                ;
  GIS_INI_LAYERSQL_GRIDSIZE1               = 'GridSize1'             ;
  GIS_INI_LAYERSQL_GRIDSIZE2               = 'GridSize2'             ;
  GIS_INI_LAYERSQL_GRIDSIZE3               = 'GridSize3'             ;
  GIS_INI_LAYERSQL_SYNONYMS                = 'Synonyms'              ;
  GIS_INI_LAYERSQL_USER                    = 'User_Name'             ;
  GIS_INI_LAYERSQL_PASSWORD                = 'Password'              ;
  GIS_INI_LAYERSQL_DATABASE                = 'Database'              ;
  GIS_INI_LAYERSQL_SCHEMA                  = 'Schema'                ;
  GIS_INI_LAYERSQL_GEOMETRY_COLUMN         = 'GeometryColumn'        ;
  GIS_INI_LAYERSQL_GEOMETRY_COLUMN_NATIVE  = 'GeometryColumnNative'  ;
  GIS_INI_LAYERSQL_INDEX_COLUMN            = 'IndexColumn'           ;
  GIS_INI_LAYERSQL_LAYER_LINK_COLUMN       = 'LayerLinkColumn'       ;
  GIS_INI_LAYERSQL_FEATURES_LINK_COLUMN    = 'FeaturesLinkColumn'    ;
  GIS_INI_LAYERSQL_VALIDATE_SETTINGS       = 'ValidateSettings'      ;
  GIS_INI_LAYERSQL_SRID                    = 'Srid'                  ;
  GIS_INI_LAYERSQL_GEOMETRY_TYPE           = 'GeometryType'          ;
  GIS_INI_LAYERSQL_FORCE_SPATIAL_INDEX     = 'ForceSpatialIndex'     ;
  GIS_INI_LAYERSQL_USE_IDENTITY            = 'UseIdentity'           ;
  GIS_INI_LAYERSQL_FIX_TOPOLOGY            = 'FixTopology'           ;
  GIS_INI_LAYERSQL_SIMPLIFY_GEOMETRY_COLL  = 'SimplifyGeometryCollection' ;
  GIS_INI_LAYERSQL_METADATA_TABLE          = 'MetadataTable'         ;
  GIS_INI_LAYERSQL_METADATA_TABLE_NATIVE   = 'Native'                ;
  GIS_INI_LAYERSQL_METADATA_TABLE_INTERNAL = 'Internal'              ;
  GIS_INI_LAYERSQL_ALTER_METADATA          = 'AlterMetadata'         ;
  GIS_INI_LAYERSQL_SEQUENCE                = 'Sequence'              ;
  GIS_INI_LAYERSQL_LOGGING                 = 'Logging'               ;
  GIS_INI_LAYERSQL_MOBILEVERSION           = 'MobileVersion'         ;
  GIS_INI_LAYERSQL_VACUUMANALYSE           = 'VacuumAnalyse'         ;
  GIS_INI_LAYERSQL_ROWSETSIZE              = 'RowsetSize'            ;
  GIS_INI_LAYERSQL_TOPO                    = 'Topo'                  ;
  GIS_INI_LAYERSQL_FORCE_NEW_CONNECTION    = 'ForceNewConnection'    ;
  GIS_INI_LAYERSQL_FORCE_COLUMNS_UPPERCASE = 'ForceColumnsUpperCase' ;
  GIS_INI_LAYERSQL_USE_STRICT_NAMES        = 'UseStrictNames'        ;
  GIS_INI_LAYERSQL_CONNECTION_POOL_ID      = 'ConnectionPoolId'      ;
  GIS_INI_LAYERSQL_VIRTUAL_FIELDS          = 'VirtualFields'         ;
  GIS_INI_LAYERSQL_VIRTUAL_FILTER          = 'VirtualFilter'         ;
  GIS_INI_LAYERSQL_VIRTUAL_ORDER           = 'VirtualOrder'          ;

  // rectify parameters
  GIS_INI_RECTIFY_HEADER                   = 'TatukGIS Rectify Layer' ;
  GIS_INI_RECTIFY_CONTROLFILE              = 'ControlFile'          ;
  GIS_INI_RECTIFY_ORDER                    = 'Order'                ;
  GIS_INI_RECTIFY_XMIN                     = 'XMin'                 ;
  GIS_INI_RECTIFY_XMAX                     = 'XMax'                 ;
  GIS_INI_RECTIFY_YMIN                     = 'YMin'                 ;
  GIS_INI_RECTIFY_YMAX                     = 'YMax'                 ;

  // ellipsoid parameters
  GIS_INI_ELLIPSOID_CODE                   = 'Ellipsoid.Code'        ;
  GIS_INI_ELLIPSOID_DESCRIPTION            = 'Ellipsoid.Description' ;
  GIS_INI_ELLIPSOID_SEMIMAJOR              = 'Ellipsoid.SemiMajor'   ;
  GIS_INI_ELLIPSOID_FLATTERING             = 'Ellipsoid.Flattering'  ;

  // datums parameters
  GIS_INI_DATUM_CODE                       = 'Datum.Code'            ;
  GIS_INI_DATUM_DESCRIPTION                = 'Datum.Description'     ;
  GIS_INI_DATUM_ELLIPSOID                  = 'Datum.Ellipsoid'       ;
  GIS_INI_DATUM_DELTA_X                    = 'Datum.Delta.X'         ;
  GIS_INI_DATUM_DELTA_Y                    = 'Datum.Delta.Y'         ;
  GIS_INI_DATUM_DELTA_Z                    = 'Datum.Delta.Z'         ;
  GIS_INI_DATUM_SIGMA_X                    = 'Datum.Sigma.X'         ;
  GIS_INI_DATUM_SIGMA_Y                    = 'Datum.Sigma.Y'         ;
  GIS_INI_DATUM_SIGMA_Z                    = 'Datum.Sigma.Z'         ;
  GIS_INI_DATUM_ROTATION_X                 = 'Datum.Rotation.X'      ;
  GIS_INI_DATUM_ROTATION_Y                 = 'Datum.Rotation.Y'      ;
  GIS_INI_DATUM_ROTATION_Z                 = 'Datum.Rotation.Z'      ;
  GIS_INI_DATUM_VALIDITY_YMin              = 'Datum.Validity.YMin'   ;
  GIS_INI_DATUM_VALIDITY_YMax              = 'Datum.Validity.YMax'   ;
  GIS_INI_DATUM_VALIDITY_XMin              = 'Datum.Validity.XMin'   ;
  GIS_INI_DATUM_VALIDITY_XMax              = 'Datum.Validity.XMax'   ;
  GIS_INI_DATUM_SCALE                      = 'Datum.Scale'           ;

  // parameter values
  GIS_INI_PARAM_RENDER                     = 'RENDER'               ;
  GIS_INI_PARAM_ALIGNMENT_SINGLE           = 'SINGLE'               ;
  GIS_INI_PARAM_ALIGNMENT_LEFTJUSTIFY      = 'LEFTJUSTIFY'          ;
  GIS_INI_PARAM_ALIGNMENT_CENTER           = 'CENTER'               ;
  GIS_INI_PARAM_ALIGNMENT_RIGHTJUSTIFY     = 'RIGHTJUSTIFY'         ;
  GIS_INI_PARAM_ALIGNMENT_FOLLOW           = 'FOLLOW'               ;

  GIS_INI_PARAM_BOOLEAN_YES                = 'YES'                  ;
  GIS_INI_PARAM_BOOLEAN_NO                 = 'NO'                   ;
  GIS_INI_PARAM_BOOLEAN_TRUE               = 'TRUE'                 ;
  GIS_INI_PARAM_BOOLEAN_FALSE              = 'FALSE'                ;
  GIS_INI_PARAM_BOOLEAN_1                  = '1'                    ;
  GIS_INI_PARAM_BOOLEAN_0                  = '0'                    ;

  GIS_INI_PARAM_COLOR_AQUA                 = 'AQUA'                 ;
  GIS_INI_PARAM_COLOR_GRAY                 = 'GRAY'                 ;
  GIS_INI_PARAM_COLOR_NAVY                 = 'NAVY'                 ;
  GIS_INI_PARAM_COLOR_SILVER               = 'SILVER'               ;
  GIS_INI_PARAM_COLOR_BLACK                = 'BLACK'                ;
  GIS_INI_PARAM_COLOR_GREEN                = 'GREEN'                ;
  GIS_INI_PARAM_COLOR_OLIVE                = 'OLIVE'                ;
  GIS_INI_PARAM_COLOR_TEAL                 = 'TEAL'                 ;
  GIS_INI_PARAM_COLOR_BLUE                 = 'BLUE'                 ;
  GIS_INI_PARAM_COLOR_LIME                 = 'LIME'                 ;
  GIS_INI_PARAM_COLOR_PURPLE               = 'PURPLE'               ;
  GIS_INI_PARAM_COLOR_WHITE                = 'WHITE'                ;
  GIS_INI_PARAM_COLOR_FUCHSIA              = 'FUCHSIA'              ;
  GIS_INI_PARAM_COLOR_MAROON               = 'MAROON'               ;
  GIS_INI_PARAM_COLOR_RED                  = 'RED'                  ;
  GIS_INI_PARAM_COLOR_YELLOW               = 'YELLOW'               ;

  GIS_INI_PARAM_CHART_PIE                  = 'PIE'                  ;
  GIS_INI_PARAM_CHART_BAR                  = 'BAR'                  ;

  GIS_INI_PARAM_DORMANT_OFF                = 'OFF'                  ;
  GIS_INI_PARAM_DORMANT_STANDARD           = 'STANDARD'             ;
  GIS_INI_PARAM_DORMANT_AGRESSIVE          = 'AGRESSIVE'            ;

  GIS_INI_PARAM_FONTSTYLE_NORMAL           = 'NORMAL'               ;
  GIS_INI_PARAM_FONTSTYLE_BOLD             = 'BOLD'                 ;
  GIS_INI_PARAM_FONTSTYLE_ITALIC           = 'ITALIC'               ;
  GIS_INI_PARAM_FONTSTYLE_UNDERLINE        = 'UNDERLINE'            ;
  GIS_INI_PARAM_FONTSTYLE_STRIKEOUT        = 'STRIKEOUT'            ;

  GIS_INI_PARAM_MARKER_BOX                 = 'BOX'                  ;
  GIS_INI_PARAM_MARKER_CIRCLE              = 'CIRCLE'               ;
  GIS_INI_PARAM_MARKER_CROSS               = 'CROSS'                ;
  GIS_INI_PARAM_MARKER_DIAGCROSS           = 'DIAGCROSS'            ;
  GIS_INI_PARAM_MARKER_TRIANGLEUP          = 'TRIANGLEUP'           ;
  GIS_INI_PARAM_MARKER_TRIANGLEDOWN        = 'TRIANGLEDOWN'         ;
  GIS_INI_PARAM_MARKER_TRIANGLELEFT        = 'TRIANGLELEFT'         ;
  GIS_INI_PARAM_MARKER_TRIANGLERIGHT       = 'TRIANGLERIGHT'        ;

  GIS_INI_PARAM_PATTERN_SOLID              = 'SOLID'                ;
  GIS_INI_PARAM_PATTERN_BDIAGONAL          = 'BDIAGONAL'            ;
  GIS_INI_PARAM_PATTERN_FDIAGONAL          = 'FDIAGONAL'            ;
  GIS_INI_PARAM_PATTERN_CROSS              = 'CROSS'                ;
  GIS_INI_PARAM_PATTERN_DIAGCROSS          = 'DIAGCROSS'            ;
  GIS_INI_PARAM_PATTERN_HORIZONTAL         = 'HORIZONTAL'           ;
  GIS_INI_PARAM_PATTERN_VERTICAL           = 'VERTICAL'             ;
  GIS_INI_PARAM_PATTERN_TRANSPARENT        = 'TRANSPARENT'          ;

  GIS_INI_PARAM_PEN_SOLID                  = 'SOLID'                ;
  GIS_INI_PARAM_PEN_DASH                   = 'DASH'                 ;
  GIS_INI_PARAM_PEN_DOT                    = 'DOT'                  ;
  GIS_INI_PARAM_PEN_DASHDOT                = 'DASHDOT'              ;
  GIS_INI_PARAM_PEN_DASHDOTDOT             = 'DASHDOTDOT'           ;
  GIS_INI_PARAM_PEN_CLEAR                  = 'CLEAR'                ;

  GIS_INI_PARAM_POSITION_UPLEFT            = 'UPLEFT'               ;
  GIS_INI_PARAM_POSITION_UPCENTER          = 'UPCENTER'             ;
  GIS_INI_PARAM_POSITION_UPRIGHT           = 'UPRIGHT'              ;
  GIS_INI_PARAM_POSITION_MIDDLELEFT        = 'MIDDLELEFT'           ;
  GIS_INI_PARAM_POSITION_MIDDLECENTER      = 'MIDDLECENTER'         ;
  GIS_INI_PARAM_POSITION_MIDDLERIGHT       = 'MIDDLERIGHT'          ;
  GIS_INI_PARAM_POSITION_DOWNLEFT          = 'DOWNLEFT'             ;
  GIS_INI_PARAM_POSITION_DOWNCENTER        = 'DOWNCENTER'           ;
  GIS_INI_PARAM_POSITION_DOWNRIGHT         = 'DOWNRIGHT'            ;
  GIS_INI_PARAM_POSITION_ANY               = 'ANY'                  ;
  GIS_INI_PARAM_POSITION_FLOW              = 'FLOW'                 ;

  // 3D
  GIS_INI_PARAM_GROUND_ONDEM               = 'OnDem'                ;
  GIS_INI_PARAM_GROUND_ABOVEDEM            = 'AboveDem'             ;
  GIS_INI_PARAM_GROUND_ABOVEZERO           = 'AboveZero'            ;

  GIS_INI_PARAM_NORMALIZED_OFF             = 'Off'                  ;
  GIS_INI_PARAM_NORMALIZED_MAX             = 'Max'                  ;
  GIS_INI_PARAM_NORMALIZED_RANGE           = 'Range'                ;

  GIS_INI_PARAM_BASEMENT_OFF               = 'Off'                  ;
  GIS_INI_PARAM_BASEMENT_LOWEST            = 'Lowest'               ;

  GIS_INI_PARAM_3DLAYER_OFF                = 'Off'                  ;
  GIS_INI_PARAM_3DLAYER_DEM                = 'Dem'                  ;
  GIS_INI_PARAM_3DLAYER_SHAPES             = 'Shapes'               ;

  // symbol
  GIS_INI_SYMBOL_HEADER                    = 'TatukGIS Symbol'      ;
  GIS_INI_SYMBOL_CENTERX                   = 'Center.X'             ;
  GIS_INI_SYMBOL_CENTERY                   = 'Center.Y'             ;

  // visible extent
  GIS_INI_VISIBLEEXTENT_XMIN               = 'VisibleExtent.XMin'   ;
  GIS_INI_VISIBLEEXTENT_XMAX               = 'VisibleExtent.XMax'   ;
  GIS_INI_VISIBLEEXTENT_YMIN               = 'VisibleExtent.YMin'   ;
  GIS_INI_VISIBLEEXTENT_YMAX               = 'VisibleExtent.YMax'   ;

  // default extent
  GIS_INI_EXTENT_XMIN                      = 'Extent.XMin'   ;
  GIS_INI_EXTENT_XMAX                      = 'Extent.XMax'   ;
  GIS_INI_EXTENT_YMIN                      = 'Extent.YMin'   ;
  GIS_INI_EXTENT_YMAX                      = 'Extent.YMax'   ;
  GIS_INI_EXTENT_ZMIN                      = 'Extent.ZMin'   ;
  GIS_INI_EXTENT_ZMAX                      = 'Extent.ZMax'   ;

  // aggregators
  GIS_INI_AGGREGATOR_NAME                  = 'Aggregator.Name'     ;
  GIS_INI_AGGREGATOR_RADIUS                = 'Aggregator.Radius'     ;
  GIS_INI_AGGREGATOR_THRESHOLD             = 'Aggregator.Threshold' ;

  // name in a template file
  GIS_TPL_GENERAL                          = 'PrintTemplate' ;
  GIS_TPL_GENERAL_HEADER                   = GIS_COMMONFILES_TATUKGIS + ' ' +
                                             GIS_TPL_GENERAL ;
  GIS_TPL_PAGESIZE                         = 'PAGESIZE'        ;
  GIS_TPL_GRAPHIC                          = 'GRAPHIC'         ;
  GIS_TPL_TEXT                             = 'TEXT'            ;
  GIS_TPL_BOX                              = 'BOX'             ;
  GIS_TPL_FRAME                            = 'FRAME'           ;
  GIS_TPL_MAP                              = 'MAP'             ;
  GIS_TPL_LEGEND                           = 'LEGEND'          ;
  GIS_TPL_SCALE                            = 'SCALE'           ;
  GIS_TPL_NORTHARROW                       = 'NORTHARROW'      ;

  GIS_TPL_GRAPHIC_D                        = GIS_TPL_GRAPHIC      + '%d' ;
  GIS_TPL_TEXT_D                           = GIS_TPL_TEXT         + '%d' ;
  GIS_TPL_BOX_D                            = GIS_TPL_BOX          + '%d' ;
  GIS_TPL_FRAME_D                          = GIS_TPL_FRAME        + '%d' ;
  GIS_TPL_MAP_D                            = GIS_TPL_MAP          + '%d' ;
  GIS_TPL_LEGEND_D                         = GIS_TPL_LEGEND       + '%d' ;
  GIS_TPL_SCALE_D                          = GIS_TPL_SCALE        + '%d' ;
  GIS_TPL_NORTHARROW_D                     = GIS_TPL_NORTHARROW   + '%d' ;

  // name in the template designer
  GIS_TPL_DESIGNER_FLD_NAME                = 'Name' ;
  GIS_TPL_DESIGNER_FLD_INDEX               = 'Index' ;
  GIS_TPL_DESIGNER_FLD_INDEX_MAP           = 'IndexMap' ;
  GIS_TPL_DESIGNER_FLD_INDEX_LEGEND        = 'IndexLegend' ;
  GIS_TPL_DESIGNER_FLD_INDEX_SCALE         = 'IndexScale' ;
  GIS_TPL_DESIGNER_FLD_INDEX_NORTHARROW    = 'IndexNorthArrow' ;
  GIS_TPL_DESIGNER_FLD_INDEX_TEXT          = 'IndexText' ;
  GIS_TPL_DESIGNER_FLD_INDEX_GRAPHICS      = 'IndexGraphics' ;
  GIS_TPL_DESIGNER_FLD_LEFT                = 'Left' ;
  GIS_TPL_DESIGNER_FLD_TOP                 = 'Top' ;
  GIS_TPL_DESIGNER_FLD_RIGHT               = 'Right' ;
  GIS_TPL_DESIGNER_FLD_BOTTOM              = 'Bottom' ;
  GIS_TPL_DESIGNER_FLD_BOXCOLOR            = 'BoxColor' ;
  GIS_TPL_DESIGNER_FLD_BOXFRAME            = 'BoxFrame' ;
  GIS_TPL_DESIGNER_FLD_BOXWIDTH            = 'BoxWidth' ;
  GIS_TPL_DESIGNER_FLD_FRAMECOLOR          = 'FrameColor' ;
  GIS_TPL_DESIGNER_FLD_FRAMEWIDTH          = 'FrameWidth' ;
  GIS_TPL_DESIGNER_FLD_GRAPHICS            = 'Graphics' ;
  GIS_TPL_DESIGNER_FLD_GRAPHICSPATH        = 'GraphicsPath' ;
  GIS_TPL_DESIGNER_FLD_GRAPHICSPATH_BTN    = 'GraphicsPathBtn' ;
  GIS_TPL_DESIGNER_FLD_TEXT                = 'Text' ;
  GIS_TPL_DESIGNER_FLD_FONTNAME            = 'FontName' ;
  GIS_TPL_DESIGNER_FLD_FONTSIZE            = 'FontSize' ;
  GIS_TPL_DESIGNER_FLD_FONTSTYLE           = 'FontStyle' ;
  GIS_TPL_DESIGNER_FLD_TEXTALIGN           = 'TextAlign' ;
  GIS_TPL_DESIGNER_FLD_TEXTCOLOR           = 'TextColor' ;
  GIS_TPL_DESIGNER_FLD_TEXTBGCOLOR         = 'TextBgColor' ;
  GIS_TPL_DESIGNER_FLD_TEXTBGWIDTH         = 'TextBgWidth' ;
  GIS_TPL_DESIGNER_FLD_EXTENT              = 'Extent' ;
  GIS_TPL_DESIGNER_FLD_XMIN                = 'XMin' ;
  GIS_TPL_DESIGNER_FLD_YMIN                = 'YMin' ;
  GIS_TPL_DESIGNER_FLD_XMAX                = 'XMax' ;
  GIS_TPL_DESIGNER_FLD_YMAX                = 'YMax' ;
  GIS_TPL_DESIGNER_FLD_COMPACTVIEW         = 'CompactView' ;
  GIS_TPL_DESIGNER_FLD_DRAWICONSTYLE       = 'DrawIconStyle' ;
  GIS_TPL_DESIGNER_FLD_REVERSEORDER        = 'ReverseOrder' ;
  GIS_TPL_DESIGNER_FLD_DIVIDERS            = 'Dividers' ;
  GIS_TPL_DESIGNER_FLD_DIVIDERCOLOR1       = 'DividerColor1' ;
  GIS_TPL_DESIGNER_FLD_DIVIDERCOLOR2       = 'DividerColor2' ;
  GIS_TPL_DESIGNER_FLD_COLOR1              = 'Color1' ;
  GIS_TPL_DESIGNER_FLD_COLOR2              = 'Color2' ;
  GIS_TPL_DESIGNER_FLD_NARROWSTYLE         = 'NarrowStyle' ;
  GIS_TPL_DESIGNER_FLD_NARROWPATH          = 'NarrowPath' ;
  GIS_TPL_DESIGNER_FLD_NARROWPATH_BTN      = 'NarrowPathBtn' ;

  // name for geocoding
  GIS_GEO_LAYERNAME                        = 'TGIS_Geocoding'    ;
  GIS_GEO_SPLITMASTER                      = 'GEOCODESPLITMASTER';
  GIS_GEO_SPLITTYPE                        = 'GEOCODESPLITTYPE'  ;
  GIS_GEO_SPLITVALUE                       = 'GEOCODESPLITMVALUE';

  // names for OSM geocoding
  GIS_GEO_OSM_FIELD_NAME                   = 'name' ;
  GIS_GEO_OSM_FIELD_STREET                 = 'street' ;
  GIS_GEO_OSM_FIELD_HOUSENUMBER            = 'housenumber' ;
  GIS_GEO_OSM_FIELD_CITY                   = 'city' ;
  GIS_GEO_OSM_FIELD_COUNTRY                = 'country' ;

  // names for OSM routing
  GIS_RTR_OSM_FIELD_NAME                   = 'name' ;
  GIS_RTR_OSM_FIELD_TIME                   = 'time' ;
  GIS_RTR_OSM_FIELD_DISTANCE               = 'distance' ;
  GIS_RTR_OSM_ROUTE_QUERY                  = '( type = ''route'' )' ;

  // name for geocoding
  GIS_TRN_HEADER                           = 'TatukGIS Rectify'  ;
  GIS_TRN_METHOD                           = 'Method'            ;
  GIS_TRN_POLYNOMIAL                       = 'Polynomial'        ;
  GIS_TRN_ORDER                            = 'Order'             ;
  GIS_TRN_ACTIVE                           = 'Active'            ;
  GIS_TRN_INACTIVE                         = 'Inactive'          ;
  GIS_TRN_POINT                            = 'Point%d'           ;
  GIS_TRN_CUTTINGPOLYGON                   = 'CuttingPolygon'    ;
  GIS_TRN_ACTIVE_OLD                       = 'Active'            ;

  // SQL parsers
  GIS_SQL_DIALECT_NAME_MSJET               = 'MSJET'           ;
  GIS_SQL_DIALECT_NAME_MSSQL               = 'MSSQL'           ;
  GIS_SQL_DIALECT_NAME_MSSQLCE             = 'MSSQLCE'         ;
  GIS_SQL_DIALECT_NAME_INTERBASE           = 'INTERBASE'       ;
  GIS_SQL_DIALECT_NAME_MYSQL               = 'MYSQL'           ;
  GIS_SQL_DIALECT_NAME_DB2                 = 'DB2'             ;
  GIS_SQL_DIALECT_NAME_SYBASE              = 'SYBASE'          ;
  GIS_SQL_DIALECT_NAME_ORACLE              = 'ORACLE'          ;
  GIS_SQL_DIALECT_NAME_PROGRESS            = 'PROGRESS'        ;
  GIS_SQL_DIALECT_NAME_INFORMIX            = 'INFORMIX'        ;
  GIS_SQL_DIALECT_NAME_ADVANTAGE           = 'ADVANTAGE'       ;
  GIS_SQL_DIALECT_NAME_SAPDB               = 'SAPDB'           ;
  GIS_SQL_DIALECT_NAME_POSTGRESQL          = 'POSTGRESQL'      ;
  GIS_SQL_DIALECT_NAME_FLASHFILER          = 'FLASHFILER'      ;
  GIS_SQL_DIALECT_NAME_NEXUSDB             = 'NEXUSDB'         ;
  GIS_SQL_DIALECT_NAME_BLACKFISHSQL        = 'BLACKFISHSQL'    ;
  GIS_SQL_DIALECT_NAME_SQLITE              = 'SQLITE'          ;
  GIS_SQL_DIALECT_NAME_FGDB                = 'FILEGDB'         ;
  GIS_SQL_DIALECT_NAME_INTERSYSTEMS        = 'INTERSYSTEMS'    ;

  GIS_SQL_PARAMS_ENGINE                    = 'ENGINE'          ;
  GIS_SQL_PARAMS_ENGINEOPTIONS             = 'ENGINEOPTIONS'   ;
  GIS_SQL_PARAMS_REUSE_QUERY               = 'REUSEQUERY'      ;
  GIS_SQL_ORIGINAL_UID_NAME                = 'ORIG_ID'         ;

  GIS_SQL_DIALECT_MSJET        = 'ENGINE=MSJET'                   + #13#10 +
                                 'ALTER=ALTER COLUMN'             + #13#10 +
                                 'VARCHAR_LEN='                   + #13#10 +
                                 'VARCHAR_TYPE='                  + #13#10 +
                                 'VARCHAR(2048)=MEMO'             + #13#10 +
                                 'BIGINT=DOUBLE'                  + #13#10 +
                                 'LOGICAL=CHAR(1)'                + #13#10 +
                                 'BLOB=LONGBINARY'                + #13#10 +
                                 'IF_EXISTS='                     + #13#10 +
                                 'CLOB=TEXT'                      + #13#10 +
                                 'AS=AS'                          + #13#10 +
                                 'DESC_IB='                       + #13#10 +
                                 'MAX_NAMELENGTH=64'              + #13#10 +
                                 'MAX_TEXTLENGTH=255'             + #13#10 +
                                 'QUOTE LEFT=['                   + #13#10 +
                                 'QUOTE RIGHT=]'                  + #13#10 +
                                 'ENGINEOPTIONS=0'                + #13#10 +
                                 '$date=#MM/dd/yyyy#'             + #13#10 +
                                 '$time=#HH:mm:ss.fff#'           + #13#10 +
                                 '$datetime=#MM/dd/yyyy HH:mm:ss.fff#'     ;

  GIS_SQL_DIALECT_MSSQL        = 'ENGINE=MSSQL'                   + #13#10 +
                                 'ALTER=ALTER COLUMN'             + #13#10 +
                                 'VARCHAR_LEN='                   + #13#10 +
                                 'VARCHAR_TYPE='                  + #13#10 +
                                 'VARCHAR=NVARCHAR'               + #13#10 +
                                 'VARCHAR(2048)=NVARCHAR(2048)'   + #13#10 +
                                 'CLOB=NVARCHAR(MAX)'             + #13#10 +
                                 'BLOB=IMAGE'                     + #13#10 +
                                 'BYTE=SMALLINT'                  + #13#10 +
                                 'DOUBLE=DOUBLE PRECISION'        + #13#10 +
                                 'DATE=DATETIME'                  + #13#10 +
                                 'LOGICAL=CHAR(1)'                + #13#10 +
                                 'IF_EXISTS='                     + #13#10 +
                                 'AS=AS'                          + #13#10 +
                                 'COUNTER=INTEGER IDENTITY(1,1)'  + #13#10 +
                                 'DESC_IB='                       + #13#10 +
                                 'MAX_NAMELENGTH=64'              + #13#10 +
                                 'MAX_TEXTLENGTH='                + #13#10 +
                                 'QUOTE LEFT=['                   + #13#10 +
                                 'QUOTE RIGHT=]'                  + #13#10 +
                                 'ENGINEOPTIONS=0'                + #13#10 +
                                 '$date=''yyyy-MM-dd'''           + #13#10 +
                                 '$time=''HH:mm:ss.fff'''         + #13#10 +
                                 '$datetime=''yyyy-MM-dd HH:mm:ss.fff'''   ;

  GIS_SQL_DIALECT_MSSQLCE      = 'ENGINE=MSSQLCE'                 + #13#10 +
                                 'ALTER=ALTER COLUMN'             + #13#10 +
                                 'VARCHAR_LEN='                   + #13#10 +
                                 'VARCHAR_TYPE='                  + #13#10 +
                                 'VARCHAR=NVARCHAR'               + #13#10 +
                                 'VARCHAR(2048)=NVARCHAR(2048)'   + #13#10 +
                                 'CLOB=NVARCHAR(MAX)'             + #13#10 +
                                 'BLOB=IMAGE'                     + #13#10 +
                                 'BYTE=SMALLINT'                  + #13#10 +
                                 'DOUBLE=FLOAT'                   + #13#10 +
                                 'DATE=DATETIME'                  + #13#10 +
                                 'LOGICAL=CHAR(1)'                + #13#10 +
                                 'IF_EXISTS='                     + #13#10 +
                                 'AS=AS'                          + #13#10 +
                                 'COUNTER=INTEGER IDENTITY(1,1)'  + #13#10 +
                                 'DESC_IB='                       + #13#10 +
                                 'MAX_NAMELENGTH=64'              + #13#10 +
                                 'MAX_TEXTLENGTH='                + #13#10 +
                                 'QUOTE LEFT=['                   + #13#10 +
                                 'QUOTE RIGHT=]'                  + #13#10 +
                                 'ENGINEOPTIONS=0'                + #13#10 +
                                 '$date=''yyyy-MM-dd'''           + #13#10 +
                                 '$time=''HH:mm:ss.fff'''         + #13#10 +
                                 '$datetime=''yyyy-MM-dd HH:mm:ss.fff'''   ;

  GIS_SQL_DIALECT_INTERBASE    = 'ENGINE=INTERBASE'               + #13#10 +
                                 'VARCHAR_LEN='                   + #13#10 +
                                 'VARCHAR_TYPE='                  + #13#10 +
                                 'BIGINT=DOUBLE PRECISION'        + #13#10 +
                                 'DOUBLE=DOUBLE PRECISION'        + #13#10 +
                                 'CLOB=BLOB SUB_TYPE TEXT'        + #13#10 +
                                 'LOGICAL=CHAR(1)'                + #13#10 +
                                 'IF_EXISTS='                     + #13#10 +
                                 'AS= '                           + #13#10 +
                                 'DESC_IB=DESC'                   + #13#10 +
                                 'DESC='                          + #13#10 +
                                 'MAX_NAMELENGTH=32'              + #13#10 +
                                 'MAX_TEXTLENGTH='                + #13#10 +
                                 'DROP COLUMN=DROP'               + #13#10 +
                                 'QUOTE LEFT="'                   + #13#10 +
                                 'QUOTE RIGHT="'                  + #13#10 +
                                 'ENGINEOPTIONS=4'                + #13#10 +
                                 '$date=''yyyy-MM-dd'''           + #13#10 +
                                 '$time=''HH:mm:ss.fff'''         + #13#10 +
                                 '$datetime=''yyyy-MM-dd HH:mm:ss.fff'''   ;

  GIS_SQL_DIALECT_MYSQL        = 'ENGINE=MYSQL'                   + #13#10 +
                                 'VARCHAR_LEN='                   + #13#10 +
                                 'VARCHAR_TYPE='                  + #13#10 +
                                 'DOUBLE=DOUBLE PRECISION'        + #13#10 +
                                 'BLOB=MEDIUMBLOB'                + #13#10 +
                                 'LOGICAL=CHAR(1)'                + #13#10 +
                                 'IF_EXISTS=IF EXISTS'            + #13#10 +
                                 'AS= '                           + #13#10 +
                                 'DESC_IB='                       + #13#10 +
                                 'MAX_NAMELENGTH=64'              + #13#10 +
                                 'MAX_TEXTLENGTH='                + #13#10 +
                                 'QUOTE LEFT=`'                   + #13#10 +
                                 'QUOTE RIGHT=`'                  + #13#10 +
                                 'ENGINEOPTIONS=0'                + #13#10 +
                                 '$date=''yyyy-MM-dd'''           + #13#10 +
                                 '$time=''HH:mm:ss.fff'''         + #13#10 +
                                 '$datetime=''yyyy-MM-dd HH:mm:ss.fff'''   ;

  GIS_SQL_DIALECT_DB2          = 'ENGINE=DB2'                     + #13#10 +
                                 '_IDX_=_'                        + #13#10 +
                                 'VARCHAR_LEN=SET DATA TYPE'      + #13#10 +
                                 'VARCHAR_TYPE='                  + #13#10 +
                                 'LOGICAL=CHAR(1)'                + #13#10 +
                                 'BLOB=BLOB(50 K)'                + #13#10 +
                                 'AS= '                           + #13#10 +
                                 'IF_EXISTS='                     + #13#10 +
                                 'DESC_IB='                       + #13#10 +
                                 'QUOTE LEFT="'                   + #13#10 +
                                 'QUOTE RIGHT="'                  + #13#10 +
                                 'MAX_NAMELENGTH=18'              + #13#10 +
                                 'MAX_TEXTLENGTH='                + #13#10 +
                                 'ENGINEOPTIONS=0'                + #13#10 +
                                 '$date=''yyyy-MM-dd'''           + #13#10 +
                                 '$time=''HH:mm:ss.fff'''         + #13#10 +
                                 '$datetime=''yyyy-MM-dd HH:mm:ss.fff'''   ;

  GIS_SQL_DIALECT_INFORMIX     = 'ENGINE=INFORMIX'                + #13#10 +
                                 'ALTER=MODIFY'                   + #13#10 +
                                 'VARCHAR_LEN= '                  + #13#10 +
                                 'VARCHAR_TYPE='                  + #13#10 +
                                 'VARCHAR(2048)=CHAR(2048)'       + #13#10 +
                                 'BIGINT=INTEGER'                 + #13#10 +
                                 'DOUBLE=DOUBLE PRECISION'        + #13#10 +
                                 'DATE=DATETIME'                  + #13#10 +
                                 'LOGICAL=CHAR(1)'                + #13#10 +
                                 'BLOB=BYTE'                      + #13#10 +
                                 'CLOB=TEXT'                      + #13#10 +
                                 'IF_EXISTS='                     + #13#10 +
                                 'AS=AS'                          + #13#10 +
                                 'DESC_IB='                       + #13#10 +
                                 'QUOTE LEFT="'                   + #13#10 +
                                 'QUOTE RIGHT="'                  + #13#10 +
                                 'MAX_NAMELENGTH=18'              + #13#10 +
                                 'MAX_TEXTLENGTH='                + #13#10 +
                                 'ENGINEOPTIONS=0'                + #13#10 +
                                 '$date=''yyyy-MM-dd'''           + #13#10 +
                                 '$time=''HH:mm:ss.fff'''         + #13#10 +
                                 '$datetime=''yyyy-MM-dd HH:mm:ss.fff'''   ;

  GIS_SQL_DIALECT_ORACLE       = 'ENGINE=ORACLE'                  + #13#10 +
                                 'ALTER=MODIFY'                   + #13#10 +
                                 'VARCHAR_LEN='                   + #13#10 +
                                 'VARCHAR_TYPE= CHAR'             + #13#10 +
                                 'INTEGER=NUMERIC(9,0)'           + #13#10 +
                                 'BIGINT=NUMERIC(18,0)'           + #13#10 +
                                 'DOUBLE=DOUBLE PRECISION'        + #13#10 +
                                 'LOGICAL=CHAR(1)'                + #13#10 +
                                 'UID=FUID'                       + #13#10 +
                                 'IF_EXISTS='                     + #13#10 +
                                 ':WKB_GEOMETRY=empty_blob()'     + #13#10 +
                                 ':GEOMETRY=empty_blob()'         + #13#10 +
                                 ':CELL=empty_blob()'             + #13#10 +
                                 'AS= '                           + #13#10 +
                                 'DESC_IB='                       + #13#10 +
                                 'DESC='                          + #13#10 +
                                 'MAX_NAMELENGTH=30'              + #13#10 +
                                 'MAX_TEXTLENGTH='                + #13#10 +
                                 'QUOTE LEFT="'                   + #13#10 +
                                 'QUOTE RIGHT="'                  + #13#10 +
                                 'ENGINEOPTIONS=1'                + #13#10 +
                                 '$date=''yyyy-MM-dd'''           + #13#10 +
                                 '$time=''HH:mm:ss.fff'''         + #13#10 +
                                 '$datetime=''yyyy-MM-dd HH:mm:ss.fff'''   ;

  GIS_SQL_DIALECT_ADVANTAGE    = 'ENGINE=ADVANTAGE'               + #13#10 +
                                 'VARCHAR_LEN='                   + #13#10 +
                                 'VARCHAR_TYPE='                  + #13#10 +
                                 'SMALLINT=INTEGER'               + #13#10 +
                                 'LOGICAL=CHAR(1)'                + #13#10 +
                                 'UID=FUID'                       + #13#10 +
                                 'AS= '                           + #13#10 +
                                 'DESC_IB='                       + #13#10 +
                                 'IF_EXISTS='                     + #13#10 +
                                 'MAX_NAMELENGTH=32'              + #13#10 +
                                 'MAX_TEXTLENGTH='                + #13#10 +
                                 'QUOTE LEFT="'                   + #13#10 +
                                 'QUOTE RIGHT="'                  + #13#10 +
                                 'ENGINEOPTIONS=0'                + #13#10 +
                                 '$date=''yyyy-MM-dd'''           + #13#10 +
                                 '$time=''HH:mm:ss.fff'''         + #13#10 +
                                 '$datetime=''yyyy-MM-dd HH:mm:ss.fff'''   ;

  GIS_SQL_DIALECT_SAPDB        = 'ENGINE=SAPDB'                   + #13#10 +
                                 'BLOB=LONG BYTE'                 + #13#10 +
                                 'VARCHAR_LEN='                   + #13#10 +
                                 'VARCHAR_TYPE='                  + #13#10 +
                                 'DOUBLE=FLOAT(38)'               + #13#10 +
                                 'LOGICAL=CHAR(1)'                + #13#10 +
                                 'UID=FUID'                       + #13#10 +
                                 'AS= '                           + #13#10 +
                                 'DESC_IB='                       + #13#10 +
                                 'IF_EXISTS='                     + #13#10 +
                                 'MAX_NAMELENGTH=32'              + #13#10 +
                                 'MAX_TEXTLENGTH='                + #13#10 +
                                 'QUOTE LEFT="'                   + #13#10 +
                                 'QUOTE RIGHT="'                  + #13#10 +
                                 'ENGINEOPTIONS=0'                + #13#10 +
                                 '$date=''yyyy-MM-dd'''           + #13#10 +
                                 '$time=''HH:mm:ss.fff'''         + #13#10 +
                                 '$datetime=''yyyy-MM-dd HH:mm:ss.fff'''   ;

  GIS_SQL_DIALECT_POSTGRESQL   = 'ENGINE=POSTGRESQL'              + #13#10 +
                                 'VARCHAR_LEN='                   + #13#10 +
                                 'VARCHAR_TYPE='                  + #13#10 +
                                 'BLOB=BYTEA'                     + #13#10 +
                                 'CLOB=TEXT'                      + #13#10 +
                                 'DOUBLE=DOUBLE PRECISION'        + #13#10 +
                                 'LOGICAL=CHAR(1)'                + #13#10 +
                                 'IF_EXISTS=IF EXISTS'            + #13#10 +
                                 'AS=AS'                          + #13#10 +
                                 'DESC_IB='                       + #13#10 +
                                 'DESC='                          + #13#10 +
                                 'XMIN=_xmin'                     + #13#10 +
                                 'XMAX=_xmax'                     + #13#10 +
                                 'YMIN=_ymin'                     + #13#10 +
                                 'YMAX=_ymax'                     + #13#10 +
                                 'QUOTE LEFT="'                   + #13#10 +
                                 'QUOTE RIGHT="'                  + #13#10 +
                                 'MAX_NAMELENGTH=63'              + #13#10 +
                                 'MAX_TEXTLENGTH='                + #13#10 +
                                 'ENGINEOPTIONS=0'                + #13#10 +
                                 '$date=''yyyy-MM-dd'''           + #13#10 +
                                 '$time=''HH:mm:ss.fff'''         + #13#10 +
                                 '$datetime=''yyyy-MM-dd HH:mm:ss.fff'''   ;

  GIS_SQL_DIALECT_BLACKFISHSQL = 'ENGINE=BLACKFISHSQL'            + #13#10 +
                                 'VARCHAR_TYPE='                  + #13#10 +
                                 'BLOB=VARBINARY'                 + #13#10 +
                                 'VARCHAR_LEN='                   + #13#10 +
                                 'BIGINT=DOUBLE PRECISION'        + #13#10 +
                                 'DOUBLE=DOUBLE PRECISION'        + #13#10 +
                                 'CLOB=VARCHAR'                   + #13#10 +
                                 'LOGICAL=CHAR(1)'                + #13#10 +
                                 'IF_EXISTS='                     + #13#10 +
                                 'AS= '                           + #13#10 +
                                 'DESC_IB='                       + #13#10 +
                                 'DESC='                          + #13#10 +
                                 'MAX_NAMELENGTH=32'              + #13#10 +
                                 'MAX_TEXTLENGTH='                + #13#10 +
                                 'QUOTE LEFT="'                   + #13#10 +
                                 'QUOTE RIGHT="'                  + #13#10 +
                                 'ENGINEOPTIONS=4'                + #13#10 +
                                 '$date=''yyyy-MM-dd'''           + #13#10 +
                                 '$time=''HH:mm:ss.fff'''         + #13#10 +
                                 '$datetime=''yyyy-MM-dd HH:mm:ss.fff'''   ;

  GIS_SQL_DIALECT_SQLITE       = 'ENGINE=SQLITE'                  + #13#10 +
                                 'BIGINT=INTEGER'                 + #13#10 +
                                 'CLOB=TEXT'                      + #13#10 +
                                 'IF_EXISTS=IF EXISTS'            + #13#10 +
                                 'VARCHAR_TYPE='                  + #13#10 +
                                 'LOGICAL=BOOL'                   + #13#10 +
                                 'AS= '                           + #13#10 +
                                 'DESC_IB='                       + #13#10 +
                                 'DESC='                          + #13#10 +
                                 'MAX_NAMELENGTH=32'              + #13#10 +
                                 'MAX_TEXTLENGTH='                + #13#10 +
                                 'QUOTE LEFT="'                   + #13#10 +
                                 'QUOTE RIGHT="'                  + #13#10 +
                                 'ENGINEOPTIONS=0'                + #13#10 +
                                 '$date=''yyyy-MM-dd'''           + #13#10 +
                                 '$time=''HH:mm:ss.fff'''         + #13#10 +
                                 '$datetime=''yyyy-MM-dd HH:mm:ss.fff'''   ;

  GIS_SQL_DIALECT_SYBASE       = 'ENGINE=SYBASE'                  + #13#10 +
                                 'ALTER=ALTER COLUMN'             + #13#10 +
                                 'VARCHAR_LEN='                   + #13#10 +
                                 'VARCHAR_TYPE='                  + #13#10 +
                                 'VARCHAR(2048)=LONG VARCHAR'     + #13#10 +
                                 'BIGINT=BIGINT'                  + #13#10 +
                                 'LOGICAL=CHAR(1)'                + #13#10 +
                                 'BLOB=LONG BINARY'               + #13#10 +
                                 'CLOB=LONG VARCHAR'              + #13#10 +
                                 'IF_EXISTS='                     + #13#10 +
                                 'AS=AS'                          + #13#10 +
                                 'DESC_IB='                       + #13#10 +
                                 'MAX_NAMELENGTH=29'              + #13#10 +
                                 'MAX_TEXTLENGTH='                + #13#10 +
                                 'QUOTE LEFT="'                   + #13#10 +
                                 'QUOTE RIGHT="'                  + #13#10 +
                                 'ENGINEOPTIONS=0'                + #13#10 +
                                 '$date=''yyyy/MM/dd'''           + #13#10 +
                                 '$time=''HH:mm:ss.fff'''         + #13#10 +
                                 '$datetime=''yyyy/MM/dd HH:mm:ss.fff'''   ;

  GIS_SQL_DIALECT_INTERSYSTEMS = 'ENGINE=INTERSYSTEMS'            + #13#10 +
                                 'ALTER=ALTER COLUMN'             + #13#10 +
                                 'VARCHAR_LEN='                   + #13#10 +
                                 'VARCHAR_TYPE='                  + #13#10 +
                                 'VARCHAR(2048)=TEXT'             + #13#10 +
                                 'BIGINT=DOUBLE'                  + #13#10 +
                                 'LOGICAL=CHAR(1)'                + #13#10 +
                                 'IF_EXISTS='                     + #13#10 +
                                 'BLOB=IMAGE'                     + #13#10 +
                                 'CLOB=TEXT'                      + #13#10 +
                                 'AS=AS'                          + #13#10 +
                                 'DESC_IB='                       + #13#10 +
                                 'MAX_NAMELENGTH=64'              + #13#10 +
                                 'MAX_TEXTLENGTH=255'             + #13#10 +
                                 'QUOTE LEFT="'                   + #13#10 +
                                 'QUOTE RIGHT="'                  + #13#10 +
                                 'ENGINEOPTIONS=0'                + #13#10 +
                                 '$date=#MM/dd/yyyy#'             + #13#10 +
                                 '$time=#HH:mm:ss.fff#'           + #13#10 +
                                 '$datetime=#MM/dd/yyyy HH:mm:ss.fff#'     ;

  // predefined keys
  GIS_KEY_WPPDF = 'WPPDF' ;
  GIS_KEY_MRSID = 'MRSID' ;
  GIS_KEY_SDO   = 'SDO'   ;
  GIS_KEY_USER  = 'USER'  ;
  GIS_KEY_PASS  = 'PASS'  ;

  // Busy special keys
  GIS_BUSY_NOEVENTS    = '$NOEVENTS'    ; // Busy notification - no events; only hourglass
  GIS_BUSY_NOHOURGLASS = '$NOHOURGLASS' ; // Busy notification - no events; no hourglass.

  // Classification
  GIS_CLASSIFY_KMEANS_ID = 'KMEANS_ID' ;

  // Shields
  GIS_SHIELD_CATEGORY                   = 'Shields' ;

  // Color space
  GIS_COLORSPACE_RGB    = 'RGB' ;
  GIS_COLORSPACE_HSL    = 'HSL' ;
  GIS_COLORSPACE_HSL360 = 'HSL360' ;

var
  {$DEFINE GIS_RESOURCE_VAR}
    {$INCLUDE GisResource.inc}
  {$UNDEF GIS_RESOURCE_VAR}
{$ENDIF}

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SysUtils,
    GisRtl ;
{$ENDIF}

{$IFDEF OXYGENE}
  var
    wasSelfLocalizeStrings : Boolean := False ;
{$ENDIF}

type
  { TGIS_LocalizedNotification helper object.
  }
  T_LocalizedNotificationObj = class
    public
      Prc : TGIS_LocalizedNotificationProc ;
  end ;

  { Ensures that the resource strings are initialized and returns appropriate string.
  }
  function _rsrc(
    const _r : String
  ) : String ;
  begin
    if not assigned( LocalizedNotification ) then begin
      LocalizedNotification := TGIS_LocalizedNotification.Create ;
      GisLocalizedStrings( nil ) ;
    end ;

    Result := _r ;
  end ;

  { Provide resource string with accelearator key filered out.
  }
  function _rsrcna(
    const _r : String
  ) : String ;
  begin
    {$IFDEF OXYGENE}if assigned( _rsrc( _r ) ) then {$ENDIF}
      Result := _rsrc( _r ).Replace( '&', '' ) ;
  end ;

  { True if bidi mode is enabled.
  }
  function _rsbidi : Boolean ;
  begin
    Result := _rsrc( GIS_RS_BIDIRECTIONAL ) = '1' ;
  end;



{$IFDEF OXYGENE}
  { Call registration before first use of registered layer (eg iniside CreateLayer)
  }
  procedure SelfLocalizeStrings ;
  begin
    if wasSelfLocalizeStrings then exit ;

    LocalizedNotification := TGIS_LocalizedNotification.Create ;
    GisLocalizedStrings( nil ) ;

    wasSelfLocalizeStrings := True ;
  end ;
{$ENDIF}

  constructor TGIS_LocalizedNotification.Create ;
  begin
    inherited ;
    lstNotify := TGIS_ObjectList.Create ;
  end ;

  {$IFNDEF MANAGED}

    destructor TGIS_LocalizedNotification.Destroy ;
    begin
      FreeObject( lstNotify ) ;
      inherited ;
    end ;
  {$ENDIF}

  procedure TGIS_LocalizedNotification.Subscribe(
              const _prc : TGIS_LocalizedNotificationProc
            ) ;
  var
    obj : T_LocalizedNotificationObj ;
  begin
    obj := T_LocalizedNotificationObj.Create ;
    obj.Prc := _prc ;

    lstNotify.Add( obj ) ;
  end ;

  procedure TGIS_LocalizedNotification.UnSubscribe(
              const _prc : TGIS_LocalizedNotificationProc
            ) ;
  var
    i : Integer ;
  begin
    for i := lstNotify.Count - 1 downto 0 do begin
      {$IFDEF OXYGENE}
        if T_LocalizedNotificationObj( lstNotify[i] ).Prc = _prc then
      {$ELSE}
        if @T_LocalizedNotificationObj( lstNotify[i] ).Prc = @_prc then
      {$ENDIF}
      begin
        lstNotify.Delete( i ) ;
      end ;
    end ;
  end ;

  procedure  TGIS_LocalizedNotification.Update ;
  var
    i : Integer ;
  begin
    for i:= 0 to lstNotify.Count - 1 do begin
      T_LocalizedNotificationObj( lstNotify[i] ).Prc() ;
    end ;
  end ;

//==============================================================================
// Public methods
//==============================================================================

  procedure GisLocalizedStrings( const _fn : TGIS_GetLocalizedString ) ;
    procedure fn1 ;
    begin
      {$DEFINE GIS_RESOURCE_DEFAULT}
        {$INCLUDE GisResource.Inc}
      {$UNDEF GIS_RESOURCE_DEFAULT}
    end;

    procedure fn2 ;
    begin
      {$DEFINE GIS_RESOURCE_FN}
        {$INCLUDE GisResource.Inc}
      {$UNDEF GIS_RESOURCE_FN}
    end;
  begin
    if not assigned( _fn ) then begin
      fn1 ;
    end
    else begin
      fn2 ;
    end ;

    {$IFNDEF OXYGENE}
      LocalizedNotification.Update ;
    {$ENDIF}
  end ;

//==============================================================================
// Initialization / Finalization
//==============================================================================
{$IFDEF DCC}
  initialization
    LocalizedNotification := TGIS_LocalizedNotification.Create ;
    GisLocalizedStrings( nil ) ;

  finalization
    LocalizedNotification.Free ;
{$ENDIF}

//==================================== END =====================================
end.



