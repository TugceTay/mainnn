//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DKv100.1.37476fv
// (c)2000-2025 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// ILKER#LIDERYAZILIM.COM-481078-KSVX7UYN-1D12B8B5
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================
{
  Utilities for constructing legend.
}

{$IFDEF DCC}
  unit GisLegendUtils ;
  {$HPPEMIT '#pragma link "GisLegendUtils"'}
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
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,
    System.UITypes,
    System.Classes,
    System.Generics.Collections,

    GisInterfaces,
    GisTypes,
    GisTypesUI,
    GisRtl,
    GisRendererAbstract,
    GisLayer,
    GisHierarchy,
    GisParams,
    GisLayerVector ;
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

  /// <summary>
  ///   FOR INTERNAL USE ONLY. Platform independent TGIS_ViewerBmp
  ///   encapsulation.
  /// </summary>
  TGIS_LegendViewer = {$IFDEF OXYGENE} public abstract {$ENDIF} class
    protected
      function  fget_CustomPPI     : Integer ; virtual; abstract;
      procedure fset_CustomPPI     ( const _ppi    : Integer
                                   ) ; virtual; abstract;
      function  fget_Color         : TGIS_Color ; virtual; abstract;
      procedure fset_Color         ( const _color  : TGIS_Color
                                   ) ; virtual; abstract;
      function  fget_VisibleExtent : TGIS_Extent ; virtual; abstract;
      procedure fset_VisibleExtent ( const _extent : TGIS_Extent
                                   ) ; virtual; abstract;
      function  fget_NativeBitmap  : TObject ; virtual; abstract;
      function  fget_Bitmap        : TGIS_Bitmap ; virtual; abstract;
      function  fget_Renderer      : TGIS_RendererAbstract ;
                                   virtual; abstract;
      procedure fset_Renderer      ( const _rndr   : TGIS_RendererAbstract
                                   ) ; virtual; abstract;
    public
      /// <summary>
      ///   Adds a layer to the underlying TGIS_ViewerBmp.
      /// </summary>
      /// <param name="_layer">
      ///   the layer to be added
      /// </param>
      procedure Add           ( const _layer : TGIS_Layer
                              ) ; virtual; abstract;
      /// <summary>
      ///   Converts twips to pixels.
      /// </summary>
      /// <param name="_size">
      ///   the value to be converted
      /// </param>
      /// <returns>
      ///   size in pixels
      /// </returns>
      function  TwipsToPixels ( const _size  : Integer
                              ) : Integer ; virtual; abstract;
      /// <summary>
      ///   Converts pixels to twips.
      /// </summary>
      /// <param name="_size">
      ///   the value to be converted
      /// </param>
      /// <returns>
      ///   size in twips
      /// </returns>
      function  PixelsToTwips ( const _size  : Integer
                              ) : Integer ; virtual; abstract;
      /// <summary>
      ///   Forces redraw on the underlying TGIS_ViewerBmp.
      /// </summary>
      procedure Draw          ; virtual; abstract;
    public
      /// <summary>
      ///   The PPI (pixels-per-inch) of the underlying TGIS_ViewerBmp.
      /// </summary>
      property CustomPPI : Integer
                           read  fget_CustomPPI
                           write fset_CustomPPI ;
      /// <summary>
      ///   The color of the underlying TGIS_ViewerBmp.
      /// </summary>
      property Color         : TGIS_Color
                               read  fget_Color
                               write fset_Color ;
      /// <summary>
      ///   The visible extent of the underlying TGIS_ViewerBmp.
      /// </summary>
      property VisibleExtent : TGIS_Extent
                               read  fget_VisibleExtent
                               write fset_VisibleExtent ;
      /// <summary>
      ///   The platform native bitmap of the underlying TGIS_ViewerBmp.
      /// </summary>
      property NativeBitmap  : TObject
                               read  fget_NativeBitmap ;
      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      /// <summary>
      ///   The bitmap of the underlying TGIS_ViewerBmp.
      /// </summary>
      property Bitmap        : TGIS_Bitmap
                               read  fget_Bitmap ;
      /// <summary>
      ///   The renderer of the underlying TGIS_ViewerBmp.
      /// </summary>
      property Renderer      : TGIS_RendererAbstract
                               read  fget_Renderer
                               write fset_Renderer ;
  end ;


  /// <summary>
  ///   FOR INTERNAL USE ONLY. Platform independent TGIS_LegendViewer
  ///   creation utility.
  /// </summary>
  TGIS_LegendViewerFactory = {$IFDEF OXYGENE} public abstract {$ENDIF} class
    public
      /// <summary>
      ///   Creates an instance of TGIS_LegendViewer.
      /// </summary>
      /// <param name="_width">
      ///   width of the viewer
      /// </param>
      /// <param name="_height">
      ///   height of the viewer
      /// </param>
      /// <returns>
      ///   handle to a newly created viewer
      /// </returns>
      function CreateLegendViewer ( const _width  : Integer ;
                                    const _height : Integer
                                  ) : TGIS_LegendViewer ; virtual; abstract;
  end ;
{$IFNDEF JAVA}
  var
    /// <summary>
    ///   FOR INTERNAL USE ONLY. Handle to Platform independent TGIS_LegendViewer
    ///   creation utility.
    /// </summary>
    LegendViewerFactory : TGIS_LegendViewerFactory ;
{$ELSE}
{$IFNDEF ANDROID}
  function LegendViewerFactory : TGIS_LegendViewerFactory ;
{$ELSE}
  var
    /// <summary>
    ///   FOR INTERNAL USE ONLY. Handle to Platform independent TGIS_LegendViewer
    ///   creation utility.
    /// </summary>
    LegendViewerFactory : TGIS_LegendViewerFactory ;
{$ENDIF}
{$ENDIF}

type

  /// <summary>
  ///   FOR INTERNAL USE ONLY. Actual type of the legend item represented by
  ///   TGIS_LegendItemData.
  /// </summary>
  TGIS_LegendItemType = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Empty legend item (placeholder).
    /// </summary>
    Empty,
    /// <summary>
    ///   Legend item which represents a layer.
    /// </summary>
    Layer,
    /// <summary>
    ///   Legend item which represents a group.
    /// </summary>
    Group,
    /// <summary>
    ///   legend item which represents layer parameters.
    /// </summary>
    Params
  ) ;


  /// <summary>
  ///   FOR INTERNAL USE ONLY. Container for all the information necessary to
  ///   construct a legend item.
  /// </summary>
  TGIS_LegendItemData = {$IFDEF OXYGENE} public {$ENDIF} class
    private
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      oViewer    : IGIS_Viewer ;
      sRootLayer : String  ;
    private
      FChecked   : Boolean ;
      FDataType  : TGIS_LegendItemType ;
      FData      : TObject ;
      FName      : String  ;
      FCaption   : String  ;
      FRows      : Integer ;
      FFeatures  : Integer ;
      FRdrMarker : Boolean ;
      FRdrLine   : Boolean ;
      FRdrArea   : Boolean ;
      FRdrLabel  : Boolean ;
      FChartMap  : String ;
      FChartLeg  : TGIS_ListOfStrings ;
    private
      function  fget_Checked        : Boolean ;
      procedure fset_Checked        ( const _check : Boolean
                                    ) ;
      function  fget_DataType       : TGIS_LegendItemType ;
      function  fget_Layer          : TGIS_Layer ;
      function  fget_Group          : TGIS_HierarchyGroup ;
      function  fget_Params         : TGIS_ParamsSection ;
      function  fget_IsVector       : Boolean ;
      function  fget_IsSubLayer     : Boolean ;
      function  fget_RowCount       : Integer ;
      function  fget_FeaCount       : Integer ;
      function  fget_Render         : Boolean ;
      function  fget_RenderMarker   : Boolean ;
      function  fget_RenderLine     : Boolean ;
      function  fget_RenderArea     : Boolean ;
      function  fget_RenderLabel    : Boolean ;
      function  fget_ChartMap       : String  ;
      function  fget_Name           : String  ;
      function  fget_Caption        : String  ;
    private
      function  getSubLayer         ( const _root : TGIS_Layer
                                    ) : TGIS_Layer ;
    private
      /// <summary>
      ///   Sets the default values.
      /// </summary>
      procedure setDefaultVaues ;
      /// <summary>
      ///   Determines the information necessary to construct the legend item
      ///   that represents the associated parameter section.
      /// </summary>
      procedure processSection ;
    public
      /// <summary>
      ///   Creates an instance that is to be associated with an empty
      ///   (placeholder) legend item.
      /// </summary>
      constructor Create ; overload;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Creates an instance that is to be associated with a legend item
      ///   which represents a layer.
      /// </summary>
      /// <param name="_layer">
      ///   the layer
      /// </param>
      /// <param name="_viewer">
      ///   the viewer which owns the layer
      /// </param>
      constructor Create ( const _layer  : TGIS_Layer ;
                           const _viewer : IGIS_Viewer
                         ) ; overload;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Creates an instance that is to be associated with a legend item
      ///   which represents a sublayer.
      /// </summary>
      /// <param name="_layer">
      ///   the parent layer
      /// </param>
      /// <param name="_rootLayer">
      ///   root layer name
      /// </param>
      /// <param name="_viewer">
      ///   the viewer which owns the layer
      /// </param>
      constructor Create ( const _layer     : TGIS_Layer ;
                           const _rootLayer : String     ;
                           const _viewer    : IGIS_Viewer
                         ) ; overload;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Creates an instance that is to be associated with a legend item
      ///   which represents a hierarchy group.
      /// </summary>
      /// <param name="_group">
      ///   the hierarchy group
      /// </param>
      /// <param name="_viewer">
      ///   the viewer which owns the hierarchy
      /// </param>
      constructor Create ( const _group  : IGIS_HierarchyGroup ;
                           const _viewer : IGIS_Viewer
                         ) ; overload;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Creates an instance that is to be associated with a legend item
      ///   which represents a parameter section.
      /// </summary>
      /// <param name="_param">
      ///   the parameter section
      /// </param>
      /// <param name="_viewer">
      ///   the viewer which owns the associated layer
      /// </param>
      constructor Create ( const _param  : TGIS_ParamsSection ;
                           const _viewer : IGIS_Viewer
                         ) ; overload;
    {$IFNDEF OXYGENE}
      public
        /// <summary>
        ///   Standard destructor.
        /// </summary>
        destructor Destroy ; override;
    {$ENDIF}
    public
      /// <summary>
      ///   True if the associated legend item is checked/active.
      /// </summary>
      property Checked        : Boolean
                                read  fget_Checked
                                write fset_Checked ;
      /// <summary>
      ///   Type of the associated object.
      /// </summary>
      property DataType       : TGIS_LegendItemType
                                read  fget_DataType ;
      /// <summary>
      ///   Handle to the layer object, valid if DataType = Layer.
      /// </summary>
      property Layer          : TGIS_Layer
                                read  fget_Layer ;
      /// <summary>
      ///   Handle to the group object, valid if DataType = Group.
      /// </summary>
      property Group          : TGIS_HierarchyGroup
                                read  fget_Group ;
      /// <summary>
      ///   Handle to the parameter section object, valid if DataType = Params.
      /// </summary>
      property Params         : TGIS_ParamsSection
                                read  fget_Params ;
      /// <summary>
      ///   True if the layer/parameter section is of vector type.
      /// </summary>
      property IsVector       : Boolean
                                read  fget_IsVector ;
      /// <summary>
      ///   True if the layer is a sublayer of another layer.
      /// </summary>
      property IsSubLayer     : Boolean
                                read  fget_IsSubLayer ;
      /// <summary>
      ///   Total number of rows of standard height necessary to construct the
      ///   legend item.
      /// </summary>
      property RowCount       : Integer
                                read  fget_RowCount ;
      /// <summary>
      ///   Total number of different features contained within the legend item.
      /// </summary>
      property FeatureCount   : Integer
                                read  fget_FeaCount ;
      /// <summary>
      ///   True if at least one feature type is managed via the section
      ///   renderer.
      /// </summary>
      property Render         : Boolean
                                read  fget_Render ;
      /// <summary>
      ///   True if the marker feature type is managed via the section
      ///   renderer.
      /// </summary>
      property RenderMarker   : Boolean
                                read  fget_RenderMarker ;
      /// <summary>
      ///   True if the line feature type is managed via the section
      ///   renderer.
      /// </summary>
      property RenderLine     : Boolean
                                read  fget_RenderLine ;
      /// <summary>
      ///   True if the area feature type is managed via the section
      ///   renderer.
      /// </summary>
      property RenderArea     : Boolean
                                read  fget_RenderArea ;
      /// <summary>
      ///   True if the label feature type is managed via the section
      ///   renderer.
      /// </summary>
      property RenderLabel    : Boolean
                                read  fget_RenderLabel ;
      /// <summary>
      ///   Simple map that indicates which chart colors are used.
      /// </summary>
      property ChartMap       : String
                                read  fget_ChartMap ;
      /// <summary>
      ///   Name of data object.
      /// </summary>
      property Name           : String
                                read  fget_Name ;

      /// <summary>
      ///   Caption of data object.
      /// </summary>
      property Caption        : String
                                read  fget_Caption ;

      /// <summary>
      ///   List of chart legend labels which matches the ChartMap.
      /// </summary>
      property ChartLegend    : TGIS_ListOfStrings
                                read  FChartLeg ;
    public
      /// <summary>
      ///   Flag used to indicate that the legend item is being dragged.
      /// </summary>
      IsMoving   : Boolean ;
      /// <summary>
      ///   Used to store the left coordinate of the rectangle of the associated
      ///   legend item (including subitems).
      /// </summary>
      NodeLeft   : Integer ;
      /// <summary>
      ///   Used to store the top coordinate of the rectangle of the associated
      ///   legend item (including subitems).
      /// </summary>
      NodeTop    : Integer ;
      /// <summary>
      ///   Used to store the width of the rectangle of the associated
      ///   legend item (including subitems).
      /// </summary>
      NodeWidth  : Integer ;
      /// <summary>
      ///   Used to store the total height of the associated legend item
      ///   (including subitems).
      /// </summary>
      NodeHeight : Integer ;
  end ;


  /// <summary>
  ///   The type of feature for which TGIS_LegendIconFactory prepares the icon.
  /// </summary>
  // Content of the T_bmpFactory bitmap result.
  TGIS_LegendIconType = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Icon which represents a point (marker).
    /// </summary>
    Marker,
    /// <summary>
    ///   Icon which represents a line (arc).
    /// </summary>
    Line,
    /// <summary>
    ///   Icon which represents a polygon (area).
    /// </summary>
    Area,
    /// <summary>
    ///   Icon which represents a label.
    /// </summary>
    &Label,
    /// <summary>
    ///   Icon which represents a chart.
    /// </summary>
    Chart
  ) ;

  /// <summary>
  ///   A utility class which prepares the feature icons that appear in the
  ///   legend.
  /// </summary>
  TGIS_LegendIconFactory = {$IFDEF OXYGENE} public {$ENDIF} class {$IFDEF CLR} ( IDisposable ) {$ENDIF}
    private
      FIconType : TGIS_LegendIconType ;
    private
      oViewer   : TGIS_LegendViewer ;
      oMarker   : TGIS_LayerVector ;
      oLine     : TGIS_LayerVector ;
      oArea     : TGIS_LayerVector ;
      oLabel    : TGIS_LayerVector ;
      oChart    : TGIS_LayerVector ;
      bCompact  : Boolean ;
      bRectArea : Boolean ;
      iWidth    : Integer ;
      style     : TGIS_LegendIconStyle ;
    private
      function  fget_CustomPPI     : Integer ;
      procedure fset_CustomPPI     ( const _ppi    : Integer
                                   ) ;
      function  fget_Layer         : TGIS_LayerVector ;
      function  fget_Renderer      : TGIS_RendererAbstract ;
      procedure fset_Renderer      ( const _rndr   : TGIS_RendererAbstract
                                   ) ;
    private
      /// <summary>
      ///   Prepares the template layers for all feature types.
      /// </summary>
      procedure prepareLayers ;
    public
      /// <summary>
      ///   Constructor, creates an instance.
      /// </summary>
      /// <param name="_width">
      ///   width of the output icon
      /// </param>
      /// <param name="_compact">
      ///   if True, height=width/2, otherwise height=width
      /// </param>
      constructor Create ( const _width   : Integer ;
                           const _compact : Boolean
                         ) ; overload ;
      /// <summary>
      ///   Constructor, creates an instance.
      /// </summary>
      /// <param name="_width">
      ///   width of the output icon
      /// </param>
      /// <param name="_compact">
      ///   if True, height=width/2, otherwise height=width
      /// </param>
      /// <param name="_style">
      ///   style of legend icons
      /// </param>
      constructor Create ( const _width   : Integer ;
                           const _compact : Boolean ;
                           const _style   : TGIS_LegendIconStyle
                         ) ; overload ;
    {$IFNDEF OXYGENE}
      public
        /// <summary>
        ///   Destructor, destroys the instance.
        /// </summary>
        destructor Destroy ; override;
    {$ENDIF}
    {$IFDEF OXYGENE}
      protected
        /// <summary>
        ///   Destructor, destroys the instance.
        /// </summary>
        procedure Dispose ;
    {$ENDIF}
    public
      /// <summary>
      ///   Sets the background color of the icon.
      /// </summary>
      /// <param name="_color">
      ///   background color
      /// </param>
      procedure SetColor  ( const _color : TGIS_Color
                          ) ;
      /// <summary>
      ///   Sets the visual parameters using the provided parameter section.
      /// </summary>
      /// <param name="_param">
      ///   the parameter section
      /// </param>
      procedure SetParams ( const _param : TGIS_ParamsSectionVector
                          ) ;
      /// <summary>
      ///   Sets the visual parameters using the specified section renderer
      ///   index.
      /// </summary>
      /// <param name="_layer">
      ///   the layer which contains the section renderer
      /// </param>
      /// <param name="_index">
      ///   the index of the section renderer
      /// </param>
      procedure SetParamsRenderer
                          ( const _layer : TGIS_LayerVector ;
                            const _index : Integer
                          ) ; overload ;
      /// <summary>
      ///   Sets the visual parameters using the specified section renderer
      ///   index.
      /// </summary>
      /// <param name="_layer">
      ///   the layer which contains the section renderer
      /// </param>
      /// <param name="_section">
      ///   the index of the parameters section
      /// </param>
      /// <param name="_index">
      ///   the index of the section renderer
      /// </param>
      /// <returns>
      ///   If False, it is not possible to get caurrent data
      /// </returns>
      function  SetParamsRenderer
                          ( const _layer   : TGIS_LayerVector ;
                            const _section : Integer ;
                            const _index   : Integer
                          ) : Boolean ; overload ;
      /// <summary>
      ///   Prepares and returns a native (platform specific) bitmap
      ///   which represents the current visual parameters and icon type.
      /// </summary>
      /// <returns>
      ///   handle to a bitmap
      /// </returns>
      function  GetBitmap : TGIS_Bitmap ;
    public
      /// <summary>
      ///   The PPI (pixels-per-inch) of the icon.
      /// </summary>
      property CustomPPI : Integer
                           read  fget_CustomPPI
                           write fset_CustomPPI ;
      /// <summary>
      ///   A template layer which is used to produce an icon of the current
      ///   icon type.
      /// </summary>
      property Layer     : TGIS_LayerVector
                           read  fget_Layer ;
      /// <summary>
      ///   The type of feature that is to be represented by the icon.
      /// </summary>
      property IconType  : TGIS_LegendIconType
                           read  FIconType
                           write FIconType ;
      /// <summary>
      ///   The renderer of the underlying TGIS_ViewerBmp.
      /// </summary>
      property Renderer      : TGIS_RendererAbstract
                               read  fget_Renderer
                               write fset_Renderer ;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Math,
    System.SysUtils,

    GisFunctions,
    GisClasses ;
{$ENDIF}

{$IFDEF JAVA}
  {$IFNDEF ANDROID}
    var
      /// <summary>
      ///   FOR INTERNAL USE ONLY. Handle to Platform independent TGIS_LegendViewer
      ///   creation utility.
      /// </summary>
      oLegendViewerFactory : TGIS_LegendViewerFactory ;
  {$ENDIF}
{$ENDIF}

//==============================================================================
// TGIS_LegendItemData
//==============================================================================

  constructor TGIS_LegendItemData.Create ;
  begin
    inherited ;

    setDefaultVaues ;

    FDataType := TGIS_LegendItemType.Empty ;
  end ;


  constructor TGIS_LegendItemData.Create(
    const _layer  : TGIS_Layer ;
    const _viewer : IGIS_Viewer
  ) ;
  begin
    inherited Create ;

    setDefaultVaues ;

    FDataType := TGIS_LegendItemType.Layer ;
    oViewer   := _viewer ;
    FName     := _layer.Name ;
    FCaption  := _layer.Caption ;
  end ;


  constructor TGIS_LegendItemData.Create(
    const _layer      : TGIS_Layer ;
    const _rootLayer  : String    ;
    const _viewer     : IGIS_Viewer
  ) ;
  begin
    inherited Create ;

    setDefaultVaues ;

    FDataType   := TGIS_LegendItemType.Layer ;
    oViewer     := _viewer ;
    sRootLayer  := _rootLayer ;
    FName       := _layer.Name ;
    FCaption    := _layer.Caption ;
  end ;


  constructor TGIS_LegendItemData.Create(
    const _group  : IGIS_HierarchyGroup ;
    const _viewer : IGIS_Viewer
  ) ;
  begin
    inherited Create ;

    setDefaultVaues ;

    FDataType := TGIS_LegendItemType.Group ;
    FName     := _group.Name ;
    FCaption  := _group.Caption ;
    oViewer   := _viewer ;
  end ;


  constructor TGIS_LegendItemData.Create(
    const _param : TGIS_ParamsSection ;
    const _viewer : IGIS_Viewer
  ) ;
  begin
    inherited Create ;

    setDefaultVaues ;

    FDataType := TGIS_LegendItemType.Params ;
    FData := _param ;
    oViewer := _viewer ;

    processSection ;
  end ;


  {$IFNDEF OXYGENE}
    destructor TGIS_LegendItemData.Destroy ;
    begin
      FreeObject( FChartLeg ) ;

      inherited ;
    end ;
  {$ENDIF}


  function TGIS_LegendItemData.fget_Checked : Boolean ;
  var
    la : TGIS_Layer ;
  begin
    Result := False ;

    if DataType = TGIS_LegendItemType.Layer then begin
      la := Layer ;
      if assigned( la ) then
        Result := la.Active ;
    end
    else
    if DataType = TGIS_LegendItemType.Params then
      Result := TGIS_ParamsSection( FData ).Visible
    else
      Result := FChecked ;
  end ;


  procedure TGIS_LegendItemData.fset_Checked(
    const _check : Boolean
  ) ;
  var
    la : TGIS_Layer ;
  begin
    if DataType = TGIS_LegendItemType.Layer then begin
      la := Layer ;
      if assigned( la ) then begin
        // inactive layer can raise an error upon Open
        try
          la.Active := _check ;
        except
          la.Active := False
        end;
      end ;
    end
    else
    if DataType = TGIS_LegendItemType.Params then
      TGIS_ParamsSection( FData ).Visible := _check
    else
      FChecked := _check ;
  end ;


  function TGIS_LegendItemData.fget_DataType : TGIS_LegendItemType ;
  begin
    Result := FDataType ;
  end ;

  function TGIS_LegendItemData.getSubLayer(
    const _root : TGIS_Layer
  ) : TGIS_Layer ;
  var
    la : TGIS_Layer ;
    i  : Integer ;
  begin
    Result := nil ;
    if assigned( _root ) then begin
      if assigned( _root.SubLayers ) then
        for i := 0 to _root.SubLayers.Count-1 do begin
          la := TGIS_Layer( _root.SubLayers[i] ) ;
          if la.Name = FName then begin
            Result := la ;
            break ;
          end;
          Result := getSubLayer( la ) ;
          if assigned( Result ) then break ;
        end
    end
  end ;

  function TGIS_LegendItemData.fget_Layer : TGIS_Layer ;
  var
    la : TGIS_Layer ;
  begin
    if DataType = TGIS_LegendItemType.Layer then begin
      la := TGIS_Layer( oViewer.Get( FName ) ) ;
      if assigned( la ) then begin
        if IsSubLayer then begin
          Result := getSubLayer( TGIS_Layer( oViewer.Get( sRootLayer ) ) ) ;
          if not assigned( Result ) then
            Result := la ;
        end
        else
          Result := la ;
      end
      else
        Result := getSubLayer( TGIS_Layer( oViewer.Get( sRootLayer ) ) )
    end
    else
      Result := nil ;
  end ;


  function TGIS_LegendItemData.fget_Group : TGIS_HierarchyGroup ;
  begin
    if DataType = TGIS_LegendItemType.Group then
      Result := TGIS_HierarchyGroup( oViewer.Hierarchy.Groups[FName] )
    else
      Result := nil ;
  end ;


  function TGIS_LegendItemData.fget_Params : TGIS_ParamsSection ;
  begin
    if DataType = TGIS_LegendItemType.Params then
      Result := TGIS_ParamsSection( FData )
    else
      Result := nil ;
  end ;


  function TGIS_LegendItemData.fget_IsVector : Boolean ;
  var
    la : TGIS_Layer ;
  begin
    Result := False ;

    if DataType = TGIS_LegendItemType.Layer then begin
      la := Layer ;
      if assigned( la ) then begin
        if la is TGIS_LayerVector then
          Result := True ;
      end ;
    end
    else
    if DataType = TGIS_LegendItemType.Params then begin
      if assigned( FData ) then begin
        if FData is TGIS_ParamsSectionVector then
          Result := True ;
      end;
    end ;
  end ;


  function TGIS_LegendItemData.fget_IsSubLayer : Boolean ;
  begin
    Result := not IsStringEmpty( sRootLayer ) ;
  end ;


  function TGIS_LegendItemData.fget_RowCount : Integer ;
  begin
    Result := FRows ;
  end ;


  function TGIS_LegendItemData.fget_FeaCount : Integer ;
  begin
    Result := FFeatures ;
  end ;


  function TGIS_LegendItemData.fget_Render : Boolean ;
  begin
    Result := FRdrMarker or FRdrLine or FRdrArea or FRdrLabel ;
  end ;


  function TGIS_LegendItemData.fget_RenderMarker : Boolean ;
  begin
    Result := FRdrMarker ;
  end ;


  function TGIS_LegendItemData.fget_RenderLine   : Boolean ;
  begin
    Result := FRdrLine ;
  end ;


  function TGIS_LegendItemData.fget_RenderArea   : Boolean ;
  begin
    Result := FRdrArea ;
  end ;


  function TGIS_LegendItemData.fget_RenderLabel  : Boolean ;
  begin
    Result := FRdrLabel ;
  end ;


  function TGIS_LegendItemData.fget_ChartMap : String ;
  begin
    Result := FChartMap ;
  end ;

  function TGIS_LegendItemData.fget_Name : String ;
  begin
    Result := FName ;
  end;

  function TGIS_LegendItemData.fget_Caption : String ;
  begin
    Result := FCaption ;
  end;

  procedure TGIS_LegendItemData.setDefaultVaues ;
  begin
    oViewer := nil ;
    sRootLayer := '' ;

    FChecked  := True ;
    FDataType := TGIS_LegendItemType.Empty ;
    FData     := nil ;
    FName     := '' ;
    FRows     := 0 ;
    FFeatures := 0 ;

    IsMoving   := False ;
    NodeHeight := 0 ;
  end ;


  procedure TGIS_LegendItemData.processSection ;
  var
    secv : TGIS_ParamsSectionVector ;
    cnt  : Integer ;
    fea  : Integer ;
    rdr  : Integer ;

    procedure proc_marker ;
    var
      rr : Boolean ;
    begin
      if not secv.Marker.ShowLegend then
        exit ;

      with secv.Marker do begin
        rr := False ;
        rr := rr or ( Size         = GIS_RENDER_SIZE  ) ;
        rr := rr or ( Color        = TGIS_Color.RenderColor ) ;
        rr := rr or ( OutlineWidth = GIS_RENDER_SIZE  ) ;
        rr := rr or ( OutlineColor = TGIS_Color.RenderColor ) ;
      end ;
      with secv do begin
        rr := rr and ( Render.Expression <> '' ) ;
        rr := rr and ( ( Render.Zones <> 0 ) or ( Render.ZonesEx <> 0 ) ) ;
      end ;

      if rr then begin
        inc( rdr, Abs( secv.Render.ZonesEx ) ) ;
        inc( rdr, Abs( secv.Render.Zones ) ) ;
        inc( cnt, 2*Abs( secv.Render.ZonesEx ) ) ;
        inc( cnt, 2*Abs( secv.Render.Zones ) ) ;
      end
      else
        inc( fea ) ;

      FRdrMarker := rr ;
    end ;

    procedure proc_line ;
    var
      rr : Boolean ;
    begin
      if not secv.Line.ShowLegend then
        exit ;

      with secv.Line do begin
        rr := False ;
        rr := rr or ( Width        = GIS_RENDER_SIZE  ) ;
        rr := rr or ( Color        = TGIS_Color.RenderColor ) ;
        rr := rr or ( OutlineWidth = GIS_RENDER_SIZE  ) ;
        rr := rr or ( OutlineColor = TGIS_Color.RenderColor ) ;
      end ;
      with secv do begin
        rr := rr and ( Render.Expression <> '' ) ;
        rr := rr and ( ( Render.Zones <> 0 ) or ( Render.ZonesEx <> 0 ) ) ;
      end ;

      if rr then begin
        inc( rdr, Abs( secv.Render.ZonesEx ) ) ;
        inc( rdr, Abs( secv.Render.Zones ) ) ;
        inc( cnt, 2*Abs( secv.Render.ZonesEx ) ) ;
        inc( cnt, 2*Abs( secv.Render.Zones ) ) ;
      end
      else
        inc( fea ) ;

      FRdrLine := rr ;
    end ;

    procedure proc_area ;
    var
      rr : Boolean ;
    begin
      if not secv.Area.ShowLegend then
        exit ;

      with secv.Area do begin
        rr := False ;
        rr := rr or ( Color        = TGIS_Color.RenderColor ) ;
        rr := rr or ( OutlineWidth = GIS_RENDER_SIZE  ) ;
        rr := rr or ( OutlineColor = TGIS_Color.RenderColor ) ;
      end ;
      with secv do begin
        rr := rr and ( Render.Expression <> '' ) ;
        rr := rr and ( ( Render.Zones <> 0 ) or ( Render.ZonesEx <> 0 ) ) ;
      end ;

      if rr then begin
        inc( rdr, Abs( secv.Render.ZonesEx ) ) ;
        inc( rdr, Abs( secv.Render.Zones ) ) ;
        inc( cnt, 2*Abs( secv.Render.ZonesEx ) ) ;
        inc( cnt, 2*Abs( secv.Render.Zones ) ) ;
      end
      else
        inc( fea ) ;

      FRdrArea := rr ;
    end ;

    procedure proc_label ;
    var
      rr : Boolean ;
    begin
      if not secv.Labels.ShowLegend then
        exit ;

      with secv.Labels do begin
        rr := False ;
        rr := rr or ( Color        = TGIS_Color.RenderColor ) ;
        rr := rr or ( FontColor    = TGIS_Color.RenderColor ) ;
        rr := rr or ( OutlineWidth = GIS_RENDER_SIZE  ) ;
        rr := rr or ( OutlineColor = TGIS_Color.RenderColor ) ;
      end ;
      with secv do begin
        rr := rr and ( Render.Expression <> '' ) ;
        rr := rr and ( ( Render.Zones <> 0 ) or ( Render.ZonesEx <> 0 ) ) ;
      end ;

      if rr then begin
        inc( rdr, Abs( secv.Render.ZonesEx ) ) ;
        inc( rdr, Abs( secv.Render.Zones ) ) ;
        inc( cnt, 2*Abs( secv.Render.ZonesEx ) ) ;
        inc( cnt, 2*Abs( secv.Render.Zones ) ) ;
      end
      else
        inc( fea ) ;

      FRdrLabel := rr ;
    end ;

    procedure proc_chart ;
    var
      sb  : TStringBuilder ;
      ii  : Integer ;
      bb  : Boolean ;
    begin
      if not secv.Chart.ShowLegend then
        exit ;

      if IsStringEmpty(secv.Render.Chart) then
        exit ;

      FChartLeg := TGIS_ListOfStrings.Create ;

      bb := True ;
      FChartMap := '' ;
      for ii := StringFirst to StringLast( secv.Render.Chart ) do begin
        case secv.Render.Chart[ii] of
          ':' :
            begin
              if bb then
                FChartMap := FChartMap + '0' ;
              bb := True ;
              continue ;
            end ;
          else
            begin
              if bb then begin
                FChartMap := FChartMap + '1' ;
                inc( fea, 2 ) ;
                inc( cnt, 2 ) ;
                bb := False ;
              end ;
            end ;
        end ;
      end ;

      sb := TStringBuilder.Create ;
      try
        bb := True ;
        for ii := StringFirst to StringLast( secv.Chart.Legend ) do begin
          case secv.Chart.Legend[ii] of
            ':' :
              begin
                if bb then
                  FChartLeg.Add( '' )
                else
                  FChartLeg.Add( sb.ToString ) ;
                bb := True ;
                continue ;
              end ;
            else
              begin
                if bb then begin
                  bb := False ;
                  {$IFDEF CLR}
                    sb.Length := 0 ;
                  {$ELSE}
                    sb.Clear ;
                  {$ENDIF}
                end ;
                sb.Append( secv.Chart.Legend[ii] ) ;
                if ii = StringLast( secv.Chart.Legend ) then
                  FChartLeg.Add( sb.ToString ) ;
              end ;
          end ;
        end ;
      finally
        FreeObject( sb ) ;
      end ;
    end ;

    procedure proc_vector ;
    begin
      secv := TGIS_ParamsSectionVector( FData ) ;

      rdr := 0 ;
      proc_marker ;
      proc_line ;
      proc_area ;
      proc_label ;
      if ( rdr = 0 ) and ( fea > 0 ) then
        inc( cnt, 2 )
      else
        inc( cnt, 2*fea ) ;
      inc( fea, rdr ) ;
      proc_chart ;

      if ( length( secv.Legend ) <> 0 ) and ( ( fea > 1 ) or Render ) then
        inc( cnt ) ;
    end ;

    procedure proc_pixel ;
    var
      tkn  : TGIS_Tokenizer ;
      secp : TGIS_ParamsSectionPixel ;
      ppix : TGIS_ParamsPixel ;
      ii   : Integer ;
    begin
      secp := TGIS_ParamsSectionPixel( FData ) ;
      ppix := TGIS_ParamsSectionPixel( FData ).Pixel ;

      if not ppix.ShowLegend then exit ;

      if assigned( ppix.LegendImage ) then begin
        cnt := ppix.LegendImage.Height div 16 ;
      end
      else begin
        tkn := TGIS_Tokenizer.Create ;
        try
          for ii := 0 to ppix.AltitudeMapZones.Count - 1 do begin
            tkn.ExecuteEx( ppix.AltitudeMapZones[ii] ) ;
            if tkn.Result.Count >= 4 then
              inc( cnt ) ;
          end ;
        finally
          FreeObject( tkn ) ;
        end ;
      end ;

      fea := 1 ;
    end ;

    function is_visible : Boolean ;
    var
      sec         : TGIS_ParamsSection ;
      ref         : Double ;
      minScale    : Double ;
      maxScale    : Double ;
      refInv      : Integer ;
      minScaleInv : Integer ;
      maxScaleInv : Integer ;
    begin
      Result := False ;

      sec := TGIS_ParamsSection( FData ) ;
      // check levels
      if ( sec.MinLevel > -GIS_HALF_MAX_DOUBLE ) or ( sec.MaxLevel < GIS_HALF_MAX_DOUBLE ) then begin
        // minimize Level-ScaleAsFloat-Level conversion error
        ref := RoundS( oViewer.Level / GIS_LEVEL_PRECISION ) * GIS_LEVEL_PRECISION ;

        if ref < sec.MinLevel then Exit ;
        if ref > sec.MaxLevel then Exit ;
        if GisIsSameValue( ref, sec.MaxLevel ) then Exit ;
      end
      // check scale
      else if ( sec.MinScale > 0 ) or ( sec.MaxScale < GIS_HALF_MAX_DOUBLE ) then begin
        ref := oViewer.ScaleAsFloat ;

        minScale := sec.MinScale ;
        maxScale := sec.MaxScale ;

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
        ref := sec.MinZoom ;
        if ref < 0 then begin
          if oViewer.Zoom < Abs( ref ) then Exit ;
        end
        else begin
          if oViewer.ZoomEx < ref then Exit ;
        end ;

        ref := sec.MaxZoom ;
        if ref < 0 then begin
          if oViewer.Zoom >= Abs( ref ) then Exit ;
        end
        else begin
          if oViewer.ZoomEx >= ref then Exit ;
        end ;
      end ;

      Result := True ;
    end;

  begin
    cnt := -1 ;
    fea := 0 ;

    if is_visible then begin
      if IsVector then
        proc_vector
      else
        proc_pixel ;
    end ;

    FRows := cnt ;
    FFeatures := fea ;
  end ;


//==============================================================================
// TGIS_LegendIconFactory
//==============================================================================

  constructor TGIS_LegendIconFactory.Create(
    const _width   : Integer ;
    const _compact : Boolean
  ) ;
  begin
    Create( _width, _compact, TGIS_LegendIconStyle.Default ) ;
  end ;

  constructor TGIS_LegendIconFactory.Create(
    const _width   : Integer ;
    const _compact : Boolean ;
    const _style   : TGIS_LegendIconStyle
  ) ;
  begin
    inherited Create ;

    bCompact := _compact ;
    style    := _style ;

    if _compact then
      oViewer := LegendViewerFactory.CreateLegendViewer( _width div 2, _width div 2 )
    else
      oViewer := LegendViewerFactory.CreateLegendViewer( _width, _width ) ;

    if _compact then
      iWidth := _width div 2
    else
      iWidth := _width ;

    oViewer.Color := TGIS_Color.FromARGB( 0, 0, 0, 0 ) ;

    bRectArea := style = TGIS_LegendIconStyle.Rectangular ;

    prepareLayers ;

    FIconType := TGIS_LegendIconType.Marker ;
  end ;

  {$IFDEF JAVA}
    {$IFNDEF ANDROID}
      function LegendViewerFactory: TGIS_LegendViewerFactory;
      begin
        if not assigned( oLegendViewerFactory ) then
          oLegendViewerFactory := TGIS_LegendViewerFactorySwing.Create ;
        Result := oLegendViewerFactory ;
      end;
    {$ENDIF}
  {$ENDIF}
  {$IFNDEF OXYGENE}
    destructor TGIS_LegendIconFactory.Destroy ;
    begin
      FreeObject( oViewer ) ;

      inherited ;
    end ;
  {$ENDIF}

  {$IFDEF OXYGENE}
    procedure TGIS_LegendIconFactory.Dispose ;
    begin
      FreeObject( oViewer ) ;
    end ;
  {$ENDIF}

  function TGIS_LegendIconFactory.fget_CustomPPI : Integer ;
  begin
    Result := oViewer.CustomPPI ;
  end ;


  procedure TGIS_LegendIconFactory.fset_CustomPPI(
    const _ppi : Integer
  ) ;
  begin
    oViewer.CustomPPI := _ppi ;
  end ;


  function TGIS_LegendIconFactory.fget_Layer : TGIS_LayerVector ;
  begin
    Result := nil ;
    case FIconType of
      TGIS_LegendIconType.Marker : Result := oMarker ;
      TGIS_LegendIconType.Line   : Result := oLine   ;
      TGIS_LegendIconType.Area   : Result := oArea   ;
      TGIS_LegendIconType.&Label : Result := oLabel  ;
      TGIS_LegendIconType.Chart  : Result := oChart  ;
    end ;
  end ;


  function TGIS_LegendIconFactory.fget_Renderer : TGIS_RendererAbstract ;
  begin
    Result := oViewer.Renderer ;
  end ;


  procedure TGIS_LegendIconFactory.fset_Renderer(
    const _rndr : TGIS_RendererAbstract
  ) ;
  begin
    oViewer.Renderer := _rndr ;
  end ;


  procedure TGIS_LegendIconFactory.prepareLayers ;
  var
    shp   : TGIS_Shape ;
    h     : Double ;
  begin
    h := 10.0 ;

    // MARKER
    oMarker := TGIS_LayerVector.Create ;
    oMarker.Name := 'marker' ;
    oMarker.Open ;
    oMarker.Extent := GisExtent( -10.0, -1.0, 10.0, 1.0 ) ;
    shp := oMarker.CreateShape( TGIS_ShapeType.Point, TGIS_DimensionType.XY ) ;
    shp.AddPart ;
    shp.AddPoint( GisPoint( 0.0, 0.0 ) ) ;
    oViewer.Add( oMarker ) ;

    // LINE
    oLine := TGIS_LayerVector.Create ;
    oLine.Name := 'line' ;
    oLine.Open ;
    oLine.Extent := GisExtent( -10.0, -1.0, 10.0, 1.0 ) ;
    shp := oLine.CreateShape( TGIS_ShapeType.Arc, TGIS_DimensionType.XY ) ;
    shp.AddPart ;
    shp.AddPoint( GisPoint( -10.0, -h ) ) ;
    shp.AddPoint( GisPoint(  10.0,  h ) ) ;
    oViewer.Add( oLine ) ;

    // AREA
    oArea := TGIS_LayerVector.Create ;
    oArea.Name := 'area' ;
    oArea.Open ;
    oArea.Extent := GisExtent( -10.0, -1.0, 10.0, 1.0 ) ;
    shp := oArea.CreateShape( TGIS_ShapeType.Polygon, TGIS_DimensionType.XY ) ;
    shp.AddPart ;
    if bRectArea then begin
      shp.AddPoint( GisPoint( -7.0, -h/2 ) ) ;
      shp.AddPoint( GisPoint( -7.0,  h/2 ) ) ;
      shp.AddPoint( GisPoint(  7.0,  h/2 ) ) ;
      shp.AddPoint( GisPoint(  7.0, -h/2 ) ) ;
    end
    else begin
      shp.AddPoint( GisPoint( -10.0, -h ) ) ;
      shp.AddPoint( GisPoint(  10.0,  h ) ) ;
      shp.AddPoint( GisPoint(  10.0, -h ) ) ;
    end;
    oViewer.Add( oArea ) ;

    // LABEL
    oLabel := TGIS_LayerVector.Create ;
    oLabel.Name := 'label' ;
    oLabel.Open ;
    oLabel.Extent := GisExtent( -10.0, -1.0, 10.0, 1.0 ) ;
    shp := oLabel.CreateShape( TGIS_ShapeType.Point, TGIS_DimensionType.XY ) ;
    shp.AddPart ;
    shp.AddPoint( GisPoint( 0.0, 0.0 ) ) ;
    oLabel.Params.Labels.Position := [TGIS_LabelPosition.MiddleCenter] ;
    oViewer.Add( oLabel ) ;

    // CHART
    oChart := TGIS_LayerVector.Create ;
    oChart.Name := 'chart' ;
    oChart.Open ;
    oChart.Extent := GisExtent( -10.0, -1.0, 10.0, 1.0 ) ;
    shp := oChart.CreateShape( TGIS_ShapeType.Point, TGIS_DimensionType.XY ) ;
    shp.AddPart ;
    if bRectArea then
      shp.AddPoint( GisPoint( 0.0, h/2 ) )
    else
      shp.AddPoint( GisPoint( 0.0, 0.0 ) ) ;
    oViewer.Add( oChart ) ;

    if bRectArea then
      oViewer.VisibleExtent := GisExtent( -8.0, -h/2, 8.0, h/2 )
    else
      oViewer.VisibleExtent := GisExtent( -5.0, -h/2, 5.0, h/2 ) ;
  end ;


  procedure TGIS_LegendIconFactory.SetColor(
    const _color : TGIS_Color
  ) ;
  begin
    oViewer.Color := _color ;
  end ;


  procedure TGIS_LegendIconFactory.SetParams(
    const _param : TGIS_ParamsSectionVector
  ) ;
  var
    siz  : Integer ;
    imax : Integer ;
  begin
    Layer.Params.Assign( _param ) ;
    Layer.Params.Visible := True ;
    Layer.Params.Query := '' ;
    Layer.Params.MinLevel := -GIS_MAX_DOUBLE ;
    Layer.Params.MinScale := 0 ;
    Layer.Params.MinZoom := 0 ;
    Layer.Params.MaxLevel := GIS_MAX_DOUBLE ;
    Layer.Params.MaxScale := GIS_MAX_DOUBLE ;
    Layer.Params.MaxZoom := GIS_MAX_DOUBLE ;
    Layer.Params.Labels.Field := '' ;
    Layer.Params.Labels.Value := '' ;
    Layer.Params.Labels.Allocator := False ;
    if not bCompact and not bRectArea and not assigned(Layer.Params.Labels.Shield) then
      Layer.Params.Labels.Rotate := -Pi/4 ;

    if bCompact then
      imax := oViewer.PixelsToTwips( iWidth-1 ) div 2
    else
      imax := 12 * 15 ;

    if Layer.Params.Marker.Size <> GIS_RENDER_SIZE then begin
      if Abs( Layer.Params.Marker.Size ) > GIS_AUTOSIZE_SIZE then
        Layer.Params.Marker.Size := 2*imax
      else begin
        siz := oViewer.PixelsToTwips(
          oViewer.TwipsToPixels( Layer.Params.Marker.Size ) ) ;
        if siz > 2*imax then
          Layer.Params.Marker.Size := 2*imax ;
      end
    end
    else
      Layer.Params.Marker.Size := imax ;

    if Layer.Params.Line.Width <> GIS_RENDER_SIZE then begin
      if Abs( Layer.Params.Line.Width ) > GIS_AUTOSIZE_SIZE then
        Layer.Params.Line.Width := imax
      else begin
        siz := oViewer.PixelsToTwips(
          oViewer.TwipsToPixels( Layer.Params.Line.Width ) ) ;
        if siz > imax then
          Layer.Params.Line.Width := imax ;
      end ;
    end
    else
      Layer.Params.Line.Width := imax ;

    if Layer.Params.Line.OutlineWidth <> GIS_RENDER_SIZE then begin
      if Abs( Layer.Params.Line.OutlineWidth ) > GIS_AUTOSIZE_SIZE then
        Layer.Params.Line.OutlineWidth := imax
      else begin
        siz := oViewer.PixelsToTwips(
          oViewer.TwipsToPixels( Layer.Params.Line.OutlineWidth ) ) ;
        if siz > imax then
          Layer.Params.Line.OutlineWidth := imax ;
      end ;
    end
    else
      Layer.Params.Line.OutlineWidth := imax ;

    if Layer.Params.Area.OutlineWidth <> GIS_RENDER_SIZE then begin
      if Abs( Layer.Params.Area.OutlineWidth ) > GIS_AUTOSIZE_SIZE then
        Layer.Params.Area.OutlineWidth := imax
      else begin
        siz := oViewer.PixelsToTwips(
          oViewer.TwipsToPixels( Layer.Params.Area.OutlineWidth ) ) ;
        if siz > imax then
          Layer.Params.Area.OutlineWidth := imax ;
      end ;
    end
    else
      Layer.Params.Area.OutlineWidth := imax ;

    if Layer.Params.Labels.FontSize <> GIS_RENDER_SIZE then begin
      if Abs( Layer.Params.Labels.FontSize ) > GIS_AUTOSIZE_SIZE then
        Layer.Params.Labels.FontSize := imax
      else begin
        siz := oViewer.PixelsToTwips(
          oViewer.TwipsToPixels( Layer.Params.Labels.FontSize ) ) ;
        if siz > imax then
          Layer.Params.Labels.FontSize := imax ;
      end ;
    end
    else
      Layer.Params.Labels.FontSize := imax ;
  end ;


  procedure TGIS_LegendIconFactory.SetParamsRenderer(
    const _layer : TGIS_LayerVector ;
    const _index : Integer
  ) ;
  var
    tmp : TGIS_ParamsSectionVector ;
  begin
    tmp := TGIS_ParamsSectionVector.Create ;
    try
      _layer.LegendInfo( _index, tmp ) ;
      SetParams( tmp ) ;
    finally
      FreeObject( tmp ) ;
    end ;
  end ;

  function TGIS_LegendIconFactory.SetParamsRenderer(
    const _layer   : TGIS_LayerVector ;
    const _section : Integer ;
    const _index   : Integer
  ) : Boolean ;
  var
    tmp : TGIS_ParamsSectionVector ;
  begin
    tmp := TGIS_ParamsSectionVector.Create ;
    try
      Result := _layer.LegendInfo( _section, _index, tmp ) ;
      SetParams( tmp ) ;
    finally
      FreeObject( tmp ) ;
    end ;
  end ;


  function TGIS_LegendIconFactory.GetBitmap : TGIS_Bitmap ;
  var
    old_center : Boolean ;
  begin
    oMarker.Active := False ;
    oLine.Active   := False ;
    oArea.Active   := False ;
    oLabel.Active  := False ;
    oChart.Active  := False ;

    case FIconType of
      TGIS_LegendIconType.Marker :
        begin
          oMarker.Active := True ;
          oMarker.Params.Labels.Visible := False ;
          oMarker.Params.Render.Chart := '' ;
          oMarker.Params.Chart.Values := '' ;

          if assigned( oMarker.Params.Marker ) then begin
            if assigned( oMarker.Params.Marker.Symbol ) then begin
              old_center := oMarker.Params.Marker.Symbol.AutoCenter ;
              oMarker.Params.Marker.Symbol.AutoCenter := True ;
            end ;
          end;
        end ;
      TGIS_LegendIconType.Line   :
        begin
          oLine.Active := True ;
          oLine.Params.Labels.Visible := False ;
          oLine.Params.Render.Chart := '' ;
          oLine.Params.Chart.Values := '' ;
        end ;
      TGIS_LegendIconType.Area   :
        begin
          oArea.Active := True ;
          oArea.Params.Labels.Visible := False ;
          oArea.Params.Render.Chart := '' ;
          oArea.Params.Chart.Values := '' ;
        end ;
      TGIS_LegendIconType.&Label :
        begin
          oLabel.Active := True ;
          if assigned(oLabel.Params.Labels.Shield) then
            oLabel.Params.Labels.Value := '66'
          else
            oLabel.Params.Labels.Value := 'Label' ;
          oLabel.Params.Labels.Position := [TGIS_LabelPosition.MiddleCenter] ;
          oLabel.Params.Marker.Size := 0 ;
          oLabel.Params.Render.Chart := '' ;
          oLabel.Params.Chart.Values := '' ;
        end ;
      TGIS_LegendIconType.Chart  :
        begin
          oChart.Active := True ;
          oChart.Params.Labels.Visible := False ;
          oChart.Params.Marker.Size := 0 ;
          oChart.Params.Chart.Size := -( 3 * iWidth ) div 4 ;
        end ;
    end ;

    oViewer.Draw ;

    case FIconType of
      TGIS_LegendIconType.Marker :
        begin
          if assigned( oMarker.Params.Marker ) then begin
            if assigned( oMarker.Params.Marker.Symbol ) then begin
              oMarker.Params.Marker.Symbol.AutoCenter := old_center ;
            end ;
         end;
        end;

    end;

    result := oViewer.Bitmap ;
  end ;


//==================================== END =====================================
end.
