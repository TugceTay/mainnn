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
  MVC for layer properties legend form control.
}
{$IFDEF DCC}
  unit GisControlLegendFormControler ;
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}
interface

{$INCLUDE GisInclude.inc}

{$IFDEF DCC}
  uses
    System.Generics.Collections,
    GisTypes,
    GisInterfaces,
    GisLayer,
    GisParams,
    GisCsSystems ;
{$ENDIF}
{$IFDEF CLR}
  uses
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type
  TGIS_ControlLegendFormMVC = {$IFDEF OXYGENE} public {$ENDIF} class ;
  {$REGION 'TGIS_ControlLegendCallbackEvent'}
  {$IFDEF OXYGENE}
    /// <summary>
    ///   Event arguments of the callback event which informs the visual
    ///   control that it needs to be updated.
    /// </summary>
    TGIS_ControlLegendCallbackEventArgs = public class ( EventArgs )
      {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
        FCode : Integer ;
      public
        /// <summary>
        ///   Creates an instance.
        /// </summary>
        /// <param name="_code">
        ///   code of the update event
        /// </param>
        constructor Create ( const _code : Integer
                           ) ;
      public
        /// <summary>
        ///   Code of the update event.
        /// </summary>
        property Code : Integer read FCode ;
    end ;
    /// <summary>
    ///   Callback event which informs the visual control that it needs
    ///   to be updated.
    /// </summary>
    /// <param name="_sender">
    ///   sender
    /// </param>
    /// <param name="_e">
    ///   event arguments
    /// </param>
    TGIS_ControlLegendCallbackEvent = public procedure (
      _sender : Object ;
      _e      : TGIS_ControlLegendCallbackEventArgs
    ) of Object ;

  {$ELSE}
    /// <summary>
    ///   Callback event which informs the visual control that it needs
    ///   to be updated.
    /// </summary>
    /// <param name="_sender">
    ///   sender
    /// </param>
    /// <param name="_code">
    ///   code of the update event
    /// </param>
    TGIS_ControlLegendCallbackEvent = procedure (
      _sender : TObject ;
      _code   : Integer
    ) of Object ;
  {$ENDIF}
  {$ENDREGION}
  {$REGION 'TGIS_ControlLegendFormMVC_Panel'}
  /// <summary>
  ///   Base class for all MVCs which refer to a specific panel in the
  ///   layer properties legend form control.
  /// </summary>
  TGIS_ControlLegendFormMVC_Panel = {$IFDEF OXYGENE} public {$ENDIF} class
    protected
      /// <summary>
      ///   The MVC object to which this panel MVC belongs.
      /// </summary>
      oParent : TGIS_ControlLegendFormMVC ;
    protected
      /// <summary>
      ///   Indicates whether the corresponding panel has a preview window.
      /// </summary>
      FHasPreview : Boolean ;
    protected
      /// <summary>
      ///   Attached callback event.
      /// </summary>
      FCallback : TGIS_ControlLegendCallbackEvent ;
    protected
      /// <summary>
      ///   Causes update on the corresponding panel.
      /// </summary>
      /// <param name="_code">
      ///   code of the update event
      /// </param>
      procedure raiseCallback ( const _code : Integer
                              ) ;
    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      /// <param name="_parent">
      ///   MVC object to which this panel MVC belongs
      /// </param>
      constructor Create ( const _parent : TGIS_ControlLegendFormMVC
                         ) ;
    {$IFDEF DCC}
      public
        /// <inheritdoc/>
        destructor Destroy ; override;
    {$ENDIF}
    public
      /// <summary>
      ///   Reads the parameter section from the layer.
      /// </summary>
      /// <param name="_blank">
      ///   if true, then the section will be initialized as blank
      /// </param>
      procedure SectionRead ( const _blank : Boolean
                            ) ; virtual;
      /// <summary>
      ///   Deletes the parameter section from MVC;
      ///   changes are not applied on the layer until confirmed
      ///   in the layer properties legend form.
      /// </summary>
      procedure SectionDelete ; virtual;
      /// <summary>
      ///   Moves the parameter section in the MVC section order;
      ///   changes are not applied on the layer until confirmed
      ///   in the layer properties legend form.
      /// </summary>
      /// <param name="_up">
      ///   True to move toward beginning; False toward end
      /// </param>
      procedure SectionMove   ( const _up : Boolean
                              ) ; virtual;
      /// <summary>
      ///   Writes the changes to the parameter section.
      /// </summary>
      /// <param name="_section">
      ///   destination section
      /// </param>
      procedure SectionWrite  ( const _section : TGIS_ParamsSection
                              )  ; virtual;
      /// <summary>
      ///   Writes all the changes to the layer.
      /// </summary>
      procedure Write         ; virtual;
      /// <summary>
      ///   Prepares preview of the parameters from the panel.
      /// </summary>
      /// <param name="_viewer">
      ///   viewer which displays the preview
      /// </param>
      procedure PreparePreview( const _viewer : IGIS_Viewer
                              ) ; virtual;
    public
      /// <summary>
      ///   Indicates whether the corresponding panel has a preview window.
      /// </summary>
      property HasPreview : Boolean
                            read  FHasPreview ;
    public
      /// <summary>
      ///   Attached callback event.
      /// </summary>
      property Callback   : TGIS_ControlLegendCallbackEvent
                            read  FCallback
                            write FCallback ;
  end ;
  {$ENDREGION}
  {$REGION 'TGIS_ControlLegendFormMVC_General'}
  /// <summary>
  ///   MVC which refers to the General panel in the
  ///   layer properties legend form control.
  /// </summary>
  TGIS_ControlLegendFormMVC_General = {$IFDEF OXYGENE} public {$ENDIF}
                                      class( TGIS_ControlLegendFormMVC_Panel )
    private
      vCaption           : String ;
      vCS                : TGIS_CSCoordinateSystem ;
      vBasemap           : Boolean ;
      vCachedPaint       : Boolean ;
      vIgnoreShapeParams : Boolean ;
      vMultipassRendering: Boolean ;
      vTransparency      : Integer ;
      vAddition          : Integer ;
      vComments          : String ;
      vScope             : String ;
      vCodePage          : Integer ;
      vHeightMin         : Single ;
      vHeightMax         : Single ;
      vInterpretation    : Integer ;
      vUseConfig         : Boolean ;
      vAggMethod         : String ;
      vAggRadius         : String ;
      vAggThreshold      : Integer ;
    private
      function  fget_Path              : String ;
      function  fget_Name              : String ;
      function  fget_Caption           : String ;
      procedure fset_Caption           ( const _str : String
                                       ) ;
      function  fget_CS                : TGIS_CSCoordinateSystem ;
      procedure fset_CS                ( const _cs : TGIS_CSCoordinateSystem
                                       ) ;
      function  fget_Basemap           : Boolean ;
      procedure fset_Basemap           ( const _bool : Boolean
                                       ) ;
      function  fget_CachedPaint       : Boolean ;
      procedure fset_CachedPaint       ( const _bool : Boolean
                                       ) ;
      function  fget_IgnoreShapeParams : Boolean ;
      procedure fset_IgnoreShapeParams ( const _bool : Boolean
                                       ) ;
      function  fget_MultipassRendering: Boolean ;
      procedure fset_MultipassRendering( const _bool : Boolean
                                       ) ;
      function  fget_Transparency      : Integer ;
      procedure fset_Transparency      ( const _val : Integer
                                       ) ;
      function  fget_Addition          : Integer ;
      procedure fset_Addition          ( const _val : Integer
                                       ) ;
      function  fget_FileInformation   : String ;
      function  fget_Comments          : String ;
      procedure fset_Comments          ( const _str : String
                                       ) ;
      function  fget_Scope             : String ;
      procedure fset_Scope             ( const _str : String
                                       ) ;
      function  fget_CodePage          : Integer ;
      procedure fset_CodePage          ( const _val : Integer
                                       ) ;
      function  fget_HeightMin         : Single ;
      procedure fset_HeightMin         ( const _val : Single
                                       ) ;
      function  fget_HeightMax         : Single ;
      procedure fset_HeightMax         ( const _val : Single
                                       ) ;
      function  fget_Interpretation    : Integer ;
      procedure fset_Interpretation    ( const _val : Integer
                                       ) ;
      function  fget_UseConfig         : Boolean ;
      procedure fset_UseConfig         ( const _val : Boolean
                                       ) ;
      function  getLayerInfo           : String ;

      function  fget_AggregationMethod : String ;
      procedure fset_AggregationMethod ( const _val : String
                                       ) ;
      function  fget_AggregationRadius : String ;
      procedure fset_AggregationRadius ( const _val : String
                                       ) ;

      function  fget_AggregationThreshold: Integer ;
      procedure fset_AggregationThreshold( const _val : Integer
                                         ) ;

    public
      /// <inheritdoc/>
      constructor Create ( const _parent : TGIS_ControlLegendFormMVC
                         ) ;
    {$IFDEF DCC}
      public
        /// <inheritdoc/>
        destructor Destroy ; override;
    {$ENDIF}
    protected
      /// <summary>
      ///   Reads all the parameters from the corresponding layer.
      /// </summary>
      procedure readAll ;
    public
      /// <inheritdoc/>
      procedure SectionRead   ( const _blank : Boolean
                              ) ; override ;
      /// <inheritdoc/>
      procedure SectionDelete ; override ;
      /// <inheritdoc/>
      procedure SectionMove   ( const _up : Boolean
                              ) ; override ;
      /// <inheritdoc/>
      procedure Write         ; override ;

      /// <summary>
      ///   Reads all the parameters from the corresponding layer.
      /// </summary>
      procedure Read          ;

    public
      /// <summary>
      ///   See TGIS_Layer.Path for details.
      /// </summary>
      property  Path             : String
                                   read  fget_Path ;
      /// <summary>
      ///   See TGIS_Layer.Name for details.
      /// </summary>
      property  Name             : String
                                   read  fget_Name ;
      /// <summary>
      ///   See TGIS_Layer.Caption for details.
      /// </summary>
      property  Caption          : String
                                   read  fget_Caption
                                   write fset_Caption ;
      /// <summary>
      ///   See TGIS_Layer.CS for details.
      /// </summary>
      property  CS               : TGIS_CSCoordinateSystem
                                   read  fget_CS
                                   write fset_CS ;
      /// <summary>
      ///   See TGIS_Layer.Basemap for details.
      /// </summary>
      property  Basemap          : Boolean
                                   read  fget_Basemap
                                   write fset_Basemap ;
      /// <summary>
      ///   See TGIS_Layer.CachedPaint for details.
      /// </summary>
      property  CachedPaint      : Boolean
                                   read  fget_CachedPaint
                                   write fset_CachedPaint ;
      /// <summary>
      ///   See TGIS_LayerVector.IgnoreShapeParams for details.
      /// </summary>
      property  IgnoreShapeParams : Boolean
                                   read  fget_IgnoreShapeParams
                                   write fset_IgnoreShapeParams ;
      /// <summary>
      ///   See TGIS_LayerVector.MultipassRendering for details.
      /// </summary>
      property  MultipassRendering : Boolean
                                   read  fget_MultipassRendering
                                   write fset_MultipassRendering ;
      /// <summary>
      ///   See TGIS_Layer.Transparency for details.
      /// </summary>
      property  Transparency     : Integer
                                   read  fget_Transparency
                                   write fset_Transparency ;
      /// <summary>
      ///   See TGIS_Layer.Addition for details.
      /// </summary>
      property  Addition         : Integer
                                   read  fget_Addition
                                   write fset_Addition ;
      /// <summary>
      ///   See TGIS_Layer.FileInfo for details.
      /// </summary>
      property  FileInformation  : String
                                   read  fget_FileInformation ;
      /// <summary>
      ///   See TGIS_Layer.Comments for details.
      /// </summary>
      property  Comments         : String
                                   read  fget_Comments
                                   write fset_Comments ;
      /// <summary>
      ///   See TGIS_LayerVector.Scope for details.
      /// </summary>
      property  Scope            : String
                                   read  fget_Scope
                                   write fset_Scope ;
      /// <summary>
      ///   See TGIS_Layer.CodePage for details.
      /// </summary>
      property  CodePage         : Integer
                                   read  fget_CodePage
                                   write fset_CodePage ;

      /// <summary>
      ///   See TGIS_LayerPixel.MinHeight for details.
      /// </summary>
      property  HeightMin         : Single
                                   read  fget_HeightMin
                                   write fset_HeightMin ;

      /// <summary>
      ///   See TGIS_LayerPixel.MaxHeight for details.
      /// </summary>
      property  HeightMax         : Single
                                   read  fget_HeightMax
                                   write fset_HeightMax ;

      /// <summary>
      ///   See TGIS_LayerPixel.Interpretation for details.
      /// </summary>
      property  Interpretation    : Integer
                                   read  fget_Interpretation
                                   write fset_Interpretation ;

      /// <summary>
      ///   See TGIS_Layer.UseConfig for details.
      /// </summary>
      property  UseConfig         : Boolean
                                   read  fget_UseConfig
                                   write fset_UseConfig ;

      /// <summary>
      ///   See TGIS_LayerVector.DynamicAggregator for details.
      /// </summary>
      property  AggregationMethod    : String
                                   read  fget_AggregationMethod
                                   write fset_AggregationMethod ;
      /// <summary>
      ///   See TGIS_LayerVector.DynamicAggregator for details.
      /// </summary>
      property  AggregationRadius      : String
                                   read  fget_AggregationRadius
                                   write fset_AggregationRadius ;
      /// <summary>
      ///   See TGIS_LayerVector.DynamicAggregator for details.
      /// </summary>
      property  AggregationThreshold : Integer
                                   read  fget_AggregationThreshold
                                   write fset_AggregationThreshold ;
    public
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoCSClick ;

      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoControlChange ;

      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoAggregateChange ;

  end ;
  {$ENDREGION}
  {$REGION 'TGIS_ControlLegendFormMVC_3D'}
  /// <summary>
  ///   MVC which refers to the 3D panel in the
  ///   layer properties legend form control.
  /// </summary>
  TGIS_ControlLegendFormMVC_3D = {$IFDEF OXYGENE} public {$ENDIF}
                                 class( TGIS_ControlLegendFormMVC_Panel )
    private
      vMode        : TGIS_3DLayerType ;
      vNormalizedZ : TList<TGIS_3DNormalizationType> ;
      vScaleZ      : TList<Double> ;
      vNormalizedM : TList<TGIS_3DNormalizationType> ;
      vScaleM      : TList<Double> ;
      vFalseZ      : TList<String> ;
      vFalseM      : TList<String> ;
      vGround      : TList<TGIS_3DGroundType> ;
      vBasement    : TList<TGIS_3DBasementType> ;
    private
      function  fget_TreatLayerAs   : TGIS_3DLayerType ;
      procedure fset_TreatLayerAs   ( const _val : TGIS_3DLayerType
                                    ) ;
      function  fget_NormalizedZ    : TGIS_3DNormalizationType ;
      procedure fset_NormalizedZ    ( const _val : TGIS_3DNormalizationType
                                    ) ;
      function  fget_ScaleZ         : Double ;
      procedure fset_ScaleZ         ( const _val : Double
                                    ) ;
      function  fget_NormalizedM    : TGIS_3DNormalizationType ;
      procedure fset_NormalizedM    ( const _val : TGIS_3DNormalizationType
                                    ) ;
      function  fget_ScaleM         : Double ;
      procedure fset_ScaleM         ( const _val : Double
                                    ) ;
      function  fget_FalseZ         : String ;
      procedure fset_FalseZ         ( const _val : String
                                    ) ;
      function  fget_FalseM         : String ;
      procedure fset_FalseM         ( const _val : String
                                    ) ;
      function  fget_AdjustZ        : TGIS_3DGroundType ;
      procedure fset_AdjustZ        ( const _val : TGIS_3DGroundType
                                    ) ;
      function  fget_AdjustBasement : TGIS_3DBasementType ;
      procedure fset_AdjustBasement ( const _val : TGIS_3DBasementType
                                    ) ;
    private
      function  fget_Params         : TGIS_ParamsSection ;
    public
      /// <inheritdoc/>
      constructor Create ( const _parent : TGIS_ControlLegendFormMVC
                         ) ;
    {$IFDEF DCC}
      public
        /// <inheritdoc/>
        destructor Destroy ; override;
    {$ENDIF}
    protected
      /// <inheritdoc/>
      procedure readAll ;
    public
      /// <inheritdoc/>
      procedure SectionRead   ( const _blank : Boolean
                              ) ; override ;
      /// <inheritdoc/>
      procedure SectionDelete ; override ;
      /// <inheritdoc/>
      procedure SectionMove   ( const _up : Boolean
                              ) ; override ;
      /// <inheritdoc/>
      procedure Write         ; override ;
    public
      /// <summary>
      ///   Corresponding parameter section.
      /// </summary>
      property  Params         : TGIS_ParamsSection
                                 read  fget_Params ;
    public
      /// <summary>
      ///   See TGIS_3DLayerType for details.
      /// </summary>
      property  TreatLayerAs   : TGIS_3DLayerType
                                 read  fget_TreatLayerAs
                                 write fset_TreatLayerAs ;
      /// <summary>
      ///   See TGIS_ParamsSection.NormalizedZ for details.
      /// </summary>
      property  NormalizedZ    : TGIS_3DNormalizationType
                                 read  fget_NormalizedZ
                                 write fset_NormalizedZ ;
      /// <summary>
      ///   See TGIS_ParamsSection.ScaleZ for details.
      /// </summary>
      property  ScaleZ         : Double
                                 read  fget_ScaleZ
                                 write fset_ScaleZ ;
      /// <summary>
      ///   See TGIS_ParamsSectionVector.NormalizedM for details.
      /// </summary>
      property  NormalizedM    : TGIS_3DNormalizationType
                                 read  fget_NormalizedM
                                 write fset_NormalizedM ;
      /// <summary>
      ///   See TGIS_ParamsSectionVector.ScaleM for details.
      /// </summary>
      property  ScaleM         : Double
                                 read  fget_ScaleM
                                 write fset_ScaleM ;
      /// <summary>
      ///   See TGIS_ParamsSection.FalseZ for details.
      /// </summary>
      property  FalseZ         : String
                                 read  fget_FalseZ
                                 write fset_FalseZ ;
      /// <summary>
      ///   See TGIS_ParamsSectionVector.FalseM for details.
      /// </summary>
      property  FalseM         : String
                                 read  fget_FalseM
                                 write fset_FalseM ;
      /// <summary>
      ///   See TGIS_ParamsSectionVector.Ground for details.
      /// </summary>
      property  AdjustZ        : TGIS_3DGroundType
                                 read  fget_AdjustZ
                                 write fset_AdjustZ ;
      /// <summary>
      ///   See TGIS_ParamsSectionVector.Basement for details.
      /// </summary>
      property  AdjustBasement : TGIS_3DBasementType
                                 read  fget_AdjustBasement
                                 write fset_AdjustBasement ;
    public
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoAs2DClick ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoAsDEMClick ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoAs3DClick ;
  end ;
  {$ENDREGION}
  {$REGION 'TGIS_ControlLegendFormMVC_Section'}
  /// <summary>
  ///   MVC which refers to the Section panel in the
  ///   layer properties legend form control.
  /// </summary>
  TGIS_ControlLegendFormMVC_Section = {$IFDEF OXYGENE} public {$ENDIF}
                                      class( TGIS_ControlLegendFormMVC_Panel )
    private
      vVisible  : TList<Boolean> ;
      vMinScale : TList<Double> ;
      vMaxScale : TList<Double> ;
      vQuery    : TList<String> ;
      vLegend   : TList<String> ;
    private
      function  fget_Visible  : Boolean ;
      procedure fset_Visible  ( const _bool : Boolean
                              ) ;
      function  fget_MinScale : Double ;
      procedure fset_MinScale ( const _val : Double
                              ) ;
      function  fget_MaxScale : Double ;
      procedure fset_MaxScale ( const _val : Double
                              ) ;
      function  fget_Query    : String ;
      procedure fset_Query    ( const _str : String
                              ) ;
      function  fget_Legend   : String ;
      procedure fset_Legend   ( const _str : String
                              ) ;
    public
      /// <inheritdoc/>
      constructor Create ( const _parent : TGIS_ControlLegendFormMVC
                         ) ;
    {$IFDEF DCC}
      public
        /// <inheritdoc/>
        destructor Destroy ; override;
    {$ENDIF}
    protected
      /// <inheritdoc/>
      procedure readAll ;
    public
      /// <inheritdoc/>
      procedure SectionRead   ( const _blank : Boolean
                              ) ; override ;
      /// <inheritdoc/>
      procedure SectionDelete ; override ;
      /// <inheritdoc/>
      procedure SectionMove   ( const _up : Boolean
                              ) ; override ;
      /// <inheritdoc/>
      procedure Write         ; override ;
    public
      /// <summary>
      ///   See TGIS_ParamsSection.Visible for details.
      /// </summary>
      property  Visible  : Boolean
                           read  fget_Visible
                           write fset_Visible ;
      /// <summary>
      ///   See TGIS_ParamsSection.MinScale for details.
      /// </summary>
      property  MinScale : Double
                           read  fget_MinScale
                           write fset_MinScale ;
      /// <summary>
      ///   See TGIS_ParamsSection.MaxScale for details.
      /// </summary>
      property  MaxScale : Double
                           read  fget_MaxScale
                           write fset_MaxScale ;
      /// <summary>
      ///   See TGIS_ParamsSectionVector.Query for details.
      /// </summary>
      property  Query    : String
                           read  fget_Query
                           write fset_Query ;
      /// <summary>
      ///   See TGIS_ParamsSection.Legend for details.
      /// </summary>
      property  Legend   : String
                           read  fget_Legend
                           write fset_Legend ;
    public
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoVisibleClick ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoMinScaleClick ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoMinScaleClear ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoMinScaleChange ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoMaxScaleClick ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoMaxScaleClear ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoMaxScaleChange ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoQueryChange ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoLegendChange ;
  end ;
  {$ENDREGION}
  {$REGION 'TGIS_ControlLegendFormMVC_Renderer'}
  /// <summary>
  ///   MVC which refers to the Renderer panel in the
  ///   layer properties legend form control.
  /// </summary>
  TGIS_ControlLegendFormMVC_Renderer = {$IFDEF OXYGENE} public {$ENDIF}
                                       class( TGIS_ControlLegendFormMVC_Panel )
    private
      lstParamsRender : TObjectList<TGIS_ParamsRender> ;
    private
      function fget_ParamsRender : TGIS_ParamsRender ;
    public
      /// <inheritdoc/>
      constructor Create ( const _parent : TGIS_ControlLegendFormMVC
                         ) ;
    {$IFDEF DCC}
      public
        /// <inheritdoc/>
        destructor Destroy ; override;
    {$ENDIF}
    protected
      /// <inheritdoc/>
      procedure readAll ;
    public
      /// <inheritdoc/>
      procedure SectionRead   ( const _blank : Boolean
                              ) ; override ;
      /// <inheritdoc/>
      procedure SectionDelete ; override ;
      /// <inheritdoc/>
      procedure SectionMove   ( const _up : Boolean
                              ) ; override ;
      /// <inheritdoc/>
      procedure SectionWrite  ( const _section : TGIS_ParamsSection
                              ) ; override ;
      /// <inheritdoc/>
      procedure Write         ; override ;
    public
      /// <summary>
      ///   Corresponding parameter section.
      /// </summary>
      property ParamsRender : TGIS_ParamsRender
                              read  fget_ParamsRender ;
    public
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoExpressionChange ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoZonesChange ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoZonesExChange ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoControlChange ;
  end ;
  {$ENDREGION}
  {$REGION 'TGIS_ControlLegendFormMVC_Marker'}
  /// <summary>
  ///   MVC which refers to the Marker panel in the
  ///   layer properties legend form control.
  /// </summary>
  TGIS_ControlLegendFormMVC_Marker = {$IFDEF OXYGENE} public {$ENDIF}
                                     class( TGIS_ControlLegendFormMVC_Panel )
    private
      lstParamsMarker : TObjectList<TGIS_ParamsMarker> ;
    private
      function fget_ParamsMarker : TGIS_ParamsMarker ;
    public
      /// <inheritdoc/>
      constructor Create ( const _parent : TGIS_ControlLegendFormMVC
                         ) ;
    {$IFDEF DCC}
      public
        /// <inheritdoc/>
        destructor Destroy ; override;
    {$ENDIF}
    protected
      /// <inheritdoc/>
      procedure readAll ;
    public
      /// <inheritdoc/>
      procedure SectionRead   ( const _blank : Boolean
                              ) ; override ;
      /// <inheritdoc/>
      procedure SectionDelete ; override ;
      /// <inheritdoc/>
      procedure SectionMove   ( const _up : Boolean
                              ) ; override ;
      /// <inheritdoc/>
      procedure SectionWrite  ( const _section : TGIS_ParamsSection
                              ) ; override ;
      /// <inheritdoc/>
      procedure Write         ; override ;
      /// <inheritdoc/>
      procedure PreparePreview( const _viewer : IGIS_Viewer
                              ) ; override;
    public
      /// <summary>
      ///   Corresponding parameter section.
      /// </summary>
      property ParamsMarker : TGIS_ParamsMarker
                              read  fget_ParamsMarker ;
    public
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoSmartSizeFieldChange ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoPatternChange ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoOPatternChange ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoControlChange ;
  end ;
  {$ENDREGION}
  {$REGION 'TGIS_ControlLegendFormMVC_Line'}
  /// <summary>
  ///   MVC which refers to the Line panel in the
  ///   layer properties legend form control.
  /// </summary>
  TGIS_ControlLegendFormMVC_Line = {$IFDEF OXYGENE} public {$ENDIF}
                                   class( TGIS_ControlLegendFormMVC_Panel )
    private
      lstParamsLine : TObjectList<TGIS_ParamsLine> ;
    private
      function fget_ParamsLine : TGIS_ParamsLine ;
    public
      /// <inheritdoc/>
      constructor Create ( const _parent : TGIS_ControlLegendFormMVC
                         ) ;
    {$IFDEF DCC}
      public
        /// <inheritdoc/>
        destructor Destroy ; override;
    {$ENDIF}
    protected
      /// <inheritdoc/>
      procedure readAll ;
    public
      /// <inheritdoc/>
      procedure SectionRead   ( const _blank : Boolean
                              ) ; override ;
      /// <inheritdoc/>
      procedure SectionDelete ; override ;
      /// <inheritdoc/>
      procedure SectionMove   ( const _up : Boolean
                              ) ; override ;
      /// <inheritdoc/>
      procedure SectionWrite  ( const _section : TGIS_ParamsSection
                              ) ; override ;
      /// <inheritdoc/>
      procedure Write         ; override ;
      /// <inheritdoc/>
      procedure PreparePreview( const _viewer : IGIS_Viewer
                              ) ; override;
    public
      /// <summary>
      ///   Corresponding parameter section.
      /// </summary>
      property ParamsLine : TGIS_ParamsLine
                            read  fget_ParamsLine ;
    public
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoSmartSizeFieldChange ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoPatternChange ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoOPatternChange ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoControlChange ;
  end ;
  {$ENDREGION}
  {$REGION 'TGIS_ControlLegendFormMVC_Area'}
  /// <summary>
  ///   MVC which refers to the Area panel in the
  ///   layer properties legend form control.
  /// </summary>
  TGIS_ControlLegendFormMVC_Area = {$IFDEF OXYGENE} public {$ENDIF}
                                   class( TGIS_ControlLegendFormMVC_Panel )
    private
      lstParamsArea : TObjectList<TGIS_ParamsArea> ;
    private
      function fget_ParamsArea : TGIS_ParamsArea ;
    public
      /// <inheritdoc/>
      constructor Create ( const _parent : TGIS_ControlLegendFormMVC
                         ) ;
    {$IFDEF DCC}
      public
        /// <inheritdoc/>
        destructor Destroy ; override;
    {$ENDIF}
    protected
      /// <inheritdoc/>
      procedure readAll ;
    public
      /// <inheritdoc/>
      procedure SectionRead   ( const _blank : Boolean
                              ) ; override ;
      /// <inheritdoc/>
      procedure SectionDelete ; override ;
      /// <inheritdoc/>
      procedure SectionMove   ( const _up : Boolean
                              ) ; override ;
      /// <inheritdoc/>
      procedure SectionWrite  ( const _section : TGIS_ParamsSection
                              ) ; override ;
      /// <inheritdoc/>
      procedure Write         ; override ;
      /// <inheritdoc/>
      procedure PreparePreview( const _viewer : IGIS_Viewer
                              ) ; override;
    public
      /// <summary>
      ///   Corresponding parameter section.
      /// </summary>
      property ParamsArea : TGIS_ParamsArea
                            read  fget_ParamsArea ;
    public
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoSmartSizeFieldChange ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoPatternChange ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoOPatternChange ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoControlChange ;
  end ;
  {$ENDREGION}
  {$REGION 'TGIS_ControlLegendFormMVC_Label'}
  /// <summary>
  ///   MVC which refers to the Label panel in the
  ///   layer properties legend form control.
  /// </summary>
  TGIS_ControlLegendFormMVC_Label = {$IFDEF OXYGENE} public {$ENDIF}
                                    class( TGIS_ControlLegendFormMVC_Panel )
    private
      lstParamsLabel : TObjectList<TGIS_ParamsLabel> ;
    private
      function fget_ParamsLabel : TGIS_ParamsLabel ;
    public
      /// <inheritdoc/>
      constructor Create ( const _parent : TGIS_ControlLegendFormMVC
                         ) ;
    {$IFDEF DCC}
      public
        /// <inheritdoc/>
        destructor Destroy ; override;
    {$ENDIF}
    protected
      /// <inheritdoc/>
      procedure readAll ;
    public
      /// <inheritdoc/>
      procedure SectionRead   ( const _blank : Boolean
                              ) ; override ;
      /// <inheritdoc/>
      procedure SectionDelete ; override ;
      /// <inheritdoc/>
      procedure SectionMove   ( const _up : Boolean
                              ) ; override ;
      /// <inheritdoc/>
      procedure SectionWrite  ( const _section : TGIS_ParamsSection
                              ) ; override ;
      /// <inheritdoc/>
      procedure Write         ; override ;
      /// <inheritdoc/>
      procedure PreparePreview( const _viewer : IGIS_Viewer
                              ) ; override;
    public
      /// <summary>
      ///   Corresponding parameter section.
      /// </summary>
      property ParamsLabel : TGIS_ParamsLabel
                             read  fget_ParamsLabel ;
    public
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoFieldChange ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoSmartSizeFieldChange ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoPositionExNotify ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoAlignmentChange ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoUpdateBitmap ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoUpdateOBitmap ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoControlChange ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoOPatternChange ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoPatternChange ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoShadowChange ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoShieldChange ;
    end ;
  {$ENDREGION}
  {$REGION 'TGIS_ControlLegendFormMVC_Chart'}
  /// <summary>
  ///   MVC which refers to the Chart panel in the
  ///   layer properties legend form control.
  /// </summary>
  TGIS_ControlLegendFormMVC_Chart = {$IFDEF OXYGENE} public {$ENDIF}
                                    class( TGIS_ControlLegendFormMVC_Panel )
    private
      lstParamsChart : TObjectList<TGIS_ParamsChart> ;
    private
      function  fget_ParamsChart : TGIS_ParamsChart ;
      function  fget_RenderChart : String ;
      procedure fset_RenderChart( const _value : String ) ;
    public
      /// <inheritdoc/>
      constructor Create ( const _parent : TGIS_ControlLegendFormMVC
                         ) ;
    {$IFDEF DCC}
      public
        /// <inheritdoc/>
        destructor Destroy ; override;
    {$ENDIF}
    protected
      /// <inheritdoc/>
      procedure readAll ;
    public
      /// <inheritdoc/>
      procedure SectionRead   ( const _blank : Boolean
                              ) ; override ;
      /// <inheritdoc/>
      procedure SectionDelete ; override ;
      /// <inheritdoc/>
      procedure SectionMove   ( const _up : Boolean
                              ) ; override ;
      /// <inheritdoc/>
      procedure SectionWrite  ( const _section : TGIS_ParamsSection
                              ) ; override ;
      /// <inheritdoc/>
      procedure Write         ; override ;
      /// <inheritdoc/>
      procedure PreparePreview( const _viewer : IGIS_Viewer
                              ) ; override;
    public
      /// <summary>
      ///   Corresponding parameter section.
      /// </summary>
      property ParamsChart : TGIS_ParamsChart
                             read  fget_ParamsChart ;
      /// <summary>
      ///   See TGIS_ParamsRender.Chart for details.
      /// </summary>
      property RenderChart : String
                             read  fget_RenderChart
                             write fset_RenderChart ;
    public
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoStyleChange ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoSizeUseRenderer ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoValue1Change ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoLegend1Change ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoValue2Change ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoLegend2Change ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoValue3Change ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoLegend3Change ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoValue4Change ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoLegend4Change ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoValue5Change ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoLegend5Change ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoValue6Change ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoLegend6Change ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoValue7Change ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoLegend7Change ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoValue8Change ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoLegend8Change ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoControlChange ;
  end ;
  {$ENDREGION}
  {$REGION 'TGIS_ControlLegendFormMVC_Pixel'}
  /// <summary>
  ///   MVC which refers to the Pixel panel in the
  ///   layer properties legend form control.
  /// </summary>
  TGIS_ControlLegendFormMVC_Pixel = {$IFDEF OXYGENE} public {$ENDIF}
                                    class( TGIS_ControlLegendFormMVC_Panel )
    private
      lstParamsPixel : TObjectList<TGIS_ParamsPixel> ;
    private
      function  fget_ParamsPixel : TGIS_ParamsPixel ;
      function  fget_MinZ        : Single ;
      function  fget_MaxZ        : Single ;
    public
      /// <inheritdoc/>
      constructor Create ( const _parent : TGIS_ControlLegendFormMVC
                         ) ;
    {$IFDEF DCC}
      public
        /// <inheritdoc/>
        destructor Destroy ; override;
    {$ENDIF}
    protected
      /// <inheritdoc/>
      procedure readAll ;
    public
      /// <inheritdoc/>
      procedure SectionRead   ( const _blank : Boolean
                              ) ; override ;
      /// <inheritdoc/>
      procedure SectionDelete ; override ;
      /// <inheritdoc/>
      procedure SectionMove   ( const _up : Boolean
                              ) ; override ;
      /// <inheritdoc/>
      procedure SectionWrite  ( const _section : TGIS_ParamsSection
                              ) ; override ;
      /// <inheritdoc/>
      procedure Write         ; override ;
      /// <inheritdoc/>
      procedure PreparePreview( const _viewer : IGIS_Viewer
                              ) ; override;
    public
      /// <summary>
      ///   Corresponding parameter section.
      /// </summary>
      property ParamsPixel : TGIS_ParamsPixel
                             read  fget_ParamsPixel ;
     /// <summary>
     ///   Minimum elevation value of the grid.
     /// </summary>
     property MinHeight : Single read fget_MinZ ;
     /// <summary>
     ///   Maximum elevation value of the grid.
     /// </summary>
     property MaxHeight : Single read fget_MaxZ ;
    public
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoReset ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoControlChange ;
      /// <summary>
      ///   Raises the callback event for a specific
      ///   layer properties legend form event.
      /// </summary>
      procedure DoTransparentZonesChange ;
  end ;
  {$ENDREGION}
  {$REGION 'TGIS_ControlLegendFormMVC'}
  /// <summary>
  ///   MVC for the layer properties legend form control.
  /// </summary>
  TGIS_ControlLegendFormMVC = {$IFDEF OXYGENE} public {$ENDIF} class
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      oLayer        : TGIS_Layer ;
      oBlank        : TGIS_Layer ;
      oLayerEx      : TGIS_Layer ;
      oLayerTemp    : TGIS_Layer ;
      oSection      : TGIS_ParamsSection ;
      oFieldNames   : TGIS_StringList ;
      oFieldNamesEx : TGIS_StringList ;
      iSectionCount : Integer ;
      bForceClean   : Boolean ;
    private
      FIsVector  : Boolean ;
      FIsPixel   : Boolean ;
      FIsGrid    : Boolean ;
      FHasMarker : Boolean ;
      FHasLine   : Boolean ;
      FHasArea   : Boolean ;
      FIndex     : Integer ;
    private
      FGeneral  : TGIS_ControlLegendFormMVC_General ;
      F3D       : TGIS_ControlLegendFormMVC_3D ;
      FSection  : TGIS_ControlLegendFormMVC_Section ;
      FRenderer : TGIS_ControlLegendFormMVC_Renderer ;
      FMarker   : TGIS_ControlLegendFormMVC_Marker ;
      FLine     : TGIS_ControlLegendFormMVC_Line ;
      FArea     : TGIS_ControlLegendFormMVC_Area ;
      FLabel    : TGIS_ControlLegendFormMVC_Label ;
      FChart    : TGIS_ControlLegendFormMVC_Chart ;
      FPixel    : TGIS_ControlLegendFormMVC_Pixel ;
    private
      function  fget_SectionCount : Integer ;
      function  fget_Index        : Integer ;
      procedure fset_Index        ( const _i : Integer
                                  ) ;
      function  fget_FieldNames   : TGIS_StringList ;
      function  fget_FieldNamesEx : TGIS_StringList ;
    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      /// <param name="_layer">
      ///   layer for which the MVC is to be created
      /// </param>
      constructor Create ( const _layer : TGIS_Layer
                         ) ;
    {$IFDEF DCC}
      public
        /// <inheritdoc/>
        destructor Destroy ; override;
    {$ENDIF}
    private
      procedure prepareFieldNames ;
    public
      /// <summary>
      ///   Adds a new parameter section to the MVC;
      ///   changes are not applied on the layer until confirmed
      ///   in the layer properties legend form.
      /// </summary>
      /// <param name="_blank">
      ///   if True then a blank section will be added
      /// </param>
      procedure SectionAdd ( const _blank : Boolean
                           ) ;
      /// <summary>
      ///   Deletes the current parameter section from the MVC;
      ///   changes are not applied on the layer until confirmed
      ///   in the layer properties legend form.
      /// </summary>
      procedure SectionDelete ;
      /// <summary>
      ///   Clears the list of parameter sections of the MVC;
      ///   changes are not applied on the layer until confirmed
      ///   in the layer properties legend form.
      /// </summary>
      procedure SectionClear  ;
      /// <summary>
      ///   Moves the parameter section in the MVC section order;
      ///   changes are not applied on the layer until confirmed
      ///   in the layer properties legend form.
      /// </summary>
      /// <param name="_up">
      ///   True to move toward beginning; False toward end
      /// </param>
      procedure SectionMove   ( const _up : Boolean
                              ) ;
      /// <summary>
      ///   Writes all the changes to the layer.
      /// </summary>
      procedure Write         ;
      /// <summary>
      ///   Prepares a blank layer for internal use.
      /// </summary>
      procedure PrepareLayerEx ;
      /// <summary>
      ///   Destroys the blank layer used internally.
      /// </summary>
      procedure UnPrepareLayerEx ;
      /// <summary>
      ///   Sets the blank layer as the source layer.
      /// </summary>
      procedure SwitchToLayerEx ;
      /// <summary>
      ///   Sets the actual layer as the source layer.
      /// </summary>
      procedure SwitchToLayer ;
      /// <summary>
      ///   Load and apply a config file to current layer.
      /// </summary>
      /// <param name="_path">
      ///   path to a file
      /// </param>
      procedure LoadConfig   ( const _path : String
                             ) ;
      /// <summary>
      ///   Save a config file of current layer.
      /// </summary>
      /// <param name="_path">
      ///   path to a file
      /// </param>
      procedure SaveConfig   ( const _path : String
                             ) ;
    public
      /// <summary>
      ///   The source layer.
      /// </summary>
      property  Layer        : TGIS_Layer
                               read  oLayer ;
      /// <summary>
      ///   The blank layer for internal use.
      /// </summary>
      property  LayerEx      : TGIS_Layer
                               read  oLayerEx ;
      /// <summary>
      ///   True if the source layer is of vector type.
      /// </summary>
      property  IsVector     : Boolean
                               read  FIsVector ;
      /// <summary>
      ///   True if the source layer is of raster type.
      /// </summary>
      property  IsPixel      : Boolean
                               read  FIsPixel ;
      /// <summary>
      ///   True if the source layer is of grid type.
      /// </summary>
      property  IsGrid       : Boolean
                               read  FIsGrid ;
      /// <summary>
      ///   True if the source vector layer supports
      ///   point type geometry.
      /// </summary>
      property  HasMarker    : Boolean
                               read  FHasMarker ;
      /// <summary>
      ///   True if the source vector layer supports
      ///   line/arc type geometry.
      /// </summary>
      property  HasLine      : Boolean
                               read  FHasLine ;
      /// <summary>
      ///   True if the source vector layer supports
      ///   polygon type geometry.
      /// </summary>
      property  HasArea      : Boolean
                               read  FHasArea ;
      /// <summary>
      ///   The total number of sections.
      /// </summary>
      property  SectionCount : Integer
                               read  fget_SectionCount ;
      /// <summary>
      ///   The index of the active section.
      /// </summary>
      property  SectionIndex : Integer
                               read  fget_Index
                               write fset_Index ;
      /// <summary>
      ///   The names of the attribute fields of
      ///   the source vector layer.
      /// </summary>
      property  FieldNames   : TGIS_StringList
                               read  fget_FieldNames ;
      /// <summary>
      ///   The names of the attribute fields of
      ///   the source vector layer.
      /// </summary>
      property  FieldNamesEx : TGIS_StringList
                               read  fget_FieldNamesEx ;
      /// <summary>
      ///   The MVC of the General panel.
      /// </summary>
      property  General      : TGIS_ControlLegendFormMVC_General
                               read  FGeneral ;
      /// <summary>
      ///   The MVC of the 3D panel.
      /// </summary>
      property  View3D       : TGIS_ControlLegendFormMVC_3D
                               read  F3D ;
      /// <summary>
      ///   The MVC of the Section panel.
      /// </summary>
      property  Section      : TGIS_ControlLegendFormMVC_Section
                               read  FSection ;
      /// <summary>
      ///   The MVC of the Renderer panel.
      /// </summary>
      property  Renderer     : TGIS_ControlLegendFormMVC_Renderer
                               read  FRenderer ;
      /// <summary>
      ///   The MVC of the Marker panel.
      /// </summary>
      property  Marker       : TGIS_ControlLegendFormMVC_Marker
                               read  FMarker ;
      /// <summary>
      ///   The MVC of the Line panel.
      /// </summary>
      property  Line         : TGIS_ControlLegendFormMVC_Line
                               read  FLine ;
      /// <summary>
      ///   The MVC of the Area panel.
      /// </summary>
      property  Area         : TGIS_ControlLegendFormMVC_Area
                               read  FArea ;
      /// <summary>
      ///   The MVC of the Label panel.
      /// </summary>
      property  &Label       : TGIS_ControlLegendFormMVC_Label
                               read  FLabel ;
      /// <summary>
      ///   The MVC of the Chart panel.
      /// </summary>
      property  Chart        : TGIS_ControlLegendFormMVC_Chart
                               read  FChart ;
      /// <summary>
      ///   The MVC of the Pixel panel.
      /// </summary>
      property  Pixel        : TGIS_ControlLegendFormMVC_Pixel
                               read  FPixel ;
  end ;
  {$ENDREGION}

//##############################################################################
implementation
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    GisRtl,
    GisConfig,
    GisLayerVector,
    GisLayerPixel,
    GisInternals,
    GisSldFiles,
    GisResource,
    GisUtils,
    GisTypesUI,
    GisFunctions ;
{$ENDIF}

//==============================================================================
// TGIS_ControlLegendCallbackEventArgs
//==============================================================================
  {$IFDEF OXYGENE}
    constructor TGIS_ControlLegendCallbackEventArgs.Create(
      const _code : Integer
    ) ;
    begin
      inherited Create ;
      FCode := _code ;
    end ;
  {$ENDIF}

  {$REGION 'TGIS_ControlLegendFormMVC_Panel'}
//==============================================================================
// TGIS_ControlLegendFormMVC_Panel
//==============================================================================

  constructor TGIS_ControlLegendFormMVC_Panel.Create(
    const _parent : TGIS_ControlLegendFormMVC
  ) ;
  begin
    inherited Create ;
    oParent := _parent ;
  end ;

  {$IFDEF DCC}
    destructor TGIS_ControlLegendFormMVC_Panel.Destroy ;
    begin
      inherited ;
    end ;
  {$ENDIF}

  procedure TGIS_ControlLegendFormMVC_Panel.SectionRead(
    const _blank : Boolean
  ) ;
  begin
    // to be implemented in descendant classes
  end ;

  procedure TGIS_ControlLegendFormMVC_Panel.SectionDelete ;
  begin
    // to be implemented in descendant classes
  end ;

  procedure TGIS_ControlLegendFormMVC_Panel.SectionMove(
    const _up : Boolean
  ) ;
  begin
    // to be implemented in descendant classes
  end ;
  procedure TGIS_ControlLegendFormMVC_Panel.SectionWrite(
    const _section : TGIS_ParamsSection
  )  ;
  begin
    // to be implemented in descendant classes
  end ;

  procedure TGIS_ControlLegendFormMVC_Panel.Write ;
  begin
    // to be implemented in descendant classes
  end ;

  procedure TGIS_ControlLegendFormMVC_Panel.raiseCallback(
    const _code : Integer
  ) ;
  begin
    if assigned( FCallback ) then
      {$IFDEF OXYGENE}
        FCallback( Self, new TGIS_ControlLegendCallbackEventArgs( _code ) ) ;
      {$ELSE}
        FCallback( Self, _code ) ;
      {$ENDIF}
  end ;
  procedure TGIS_ControlLegendFormMVC_Panel.PreparePreview(
    const _viewer : IGIS_Viewer
  ) ;
  begin
  end ;
  {$ENDREGION}

  {$REGION 'TGIS_ControlLegendFormMVC_General'}
//==============================================================================
// TGIS_ControlLegendFormMVC_General
//==============================================================================

  constructor TGIS_ControlLegendFormMVC_General.Create(
    const _parent : TGIS_ControlLegendFormMVC
  ) ;
  begin
    inherited ;
    FHasPreview := False ;
    readAll ;
  end ;

  {$IFDEF DCC}
    destructor TGIS_ControlLegendFormMVC_General.Destroy ;
    begin
      inherited ;
    end ;
  {$ENDIF}

  procedure TGIS_ControlLegendFormMVC_General.readAll ;
  var
    lv  : TGIS_LayerVector ;
    agg : TGIS_DynamicAggregatorAbstract ;
  begin
    vCaption := oParent.oLayer.Caption ;
    vCS := oParent.oLayer.CS ;
    vBasemap := oParent.oLayer.Basemap ;
    vCachedPaint := oParent.oLayer.CachedPaint ;
    if oParent.oLayer is TGIS_LayerVector then
      vIgnoreShapeParams := TGIS_LayerVector(oParent.oLayer).IgnoreShapeParams ;
    if oParent.oLayer is TGIS_LayerVector then
      vMultipassRendering := TGIS_LayerVector(oParent.oLayer).MultipassRendering ;
    vTransparency := oParent.oLayer.Transparency ;
    vAddition := oParent.oLayer.Addition ;
    vComments := oParent.oLayer.Comments ;
    vCodePage := oParent.oLayer.CodePage ;

    if oParent.oLayer is TGIS_LayerVector then begin
      lv := TGIS_LayerVector(oParent.oLayer) ;
      vScope := lv.Scope ;

      agg := lv.DynamicAggregator ;
      if assigned( agg ) then begin
        vAggMethod    := agg.Name ;
        vAggRadius    := agg.RadiusAsText ;
        vAggThreshold := agg.Threshold ;
      end
      else begin
        vAggMethod    := _rsrc( GIS_RS_LEGEND_PRM_OFF ) ;
        vAggRadius    := 'SIZE:0m' ;
        vAggThreshold := 0 ;
      end ;
    end
    else if oParent.oLayer is TGIS_LayerPixel then begin
      vHeightMin := TGIS_LayerPixel(oParent.oLayer).MinHeight ;
      vHeightMax := TGIS_LayerPixel(oParent.oLayer).MaxHeight ;
      case TGIS_LayerPixel(oParent.oLayer).Interpretation of
         TGIS_LayerPixelInterpretation.Default : vInterpretation := 0 ;
         TGIS_LayerPixelInterpretation.Pixel   : vInterpretation := 1 ;
         TGIS_LayerPixelInterpretation.Grid    : vInterpretation := 2 ;
      end ;
    end ;
    vUseConfig := oParent.oLayer.UseConfig ;
  end ;

  procedure TGIS_ControlLegendFormMVC_General.SectionRead(
    const _blank : Boolean
  ) ;
  begin
    inherited ;
    // do nothing
  end ;

  procedure TGIS_ControlLegendFormMVC_General.SectionDelete ;
  begin
    inherited ;
    // do nothing
  end ;

  procedure TGIS_ControlLegendFormMVC_General.SectionMove(
    const _up : Boolean
  ) ;
  begin
    inherited ;
    // do nothing
  end ;

  procedure TGIS_ControlLegendFormMVC_General.Read ;
  begin
    readAll ;
  end ;

  procedure TGIS_ControlLegendFormMVC_General.Write ;
  var
    lv   : TGIS_LayerVector ;
    nagg : Boolean ;
  begin
    inherited ;
    oParent.oLayer.Caption          := vCaption ;
    oParent.oLayer.CS               := vCS ;
    oParent.oLayer.Basemap          := vBasemap ;
    oParent.oLayer.CachedPaint      := vCachedPaint ;
    if oParent.oLayer is TGIS_LayerVector then
      TGIS_LayerVector(oParent.oLayer).IgnoreShapeParams := vIgnoreShapeParams ;
    if oParent.oLayer is TGIS_LayerVector then
      TGIS_LayerVector(oParent.oLayer).MultipassRendering := vMultipassRendering ;
    oParent.oLayer.Transparency     := vTransparency ;
    oParent.oLayer.Addition         := vAddition ;
    oParent.oLayer.Comments         := vComments ;
    oParent.oLayer.CodePage         := vCodePage ;

    if oParent.oLayer is TGIS_LayerVector then begin
      lv := TGIS_LayerVector(oParent.oLayer) ;
      lv.Scope := vScope ;

      nagg := True ;
      if assigned( lv.DynamicAggregator ) and
        ( lv.DynamicAggregator.Name = vAggMethod ) then
        nagg := False ;

      if nagg then
        lv.DynamicAggregator := TGIS_DynamicAggregatorFactory.CreateInstance( vAggMethod, lv ) ;

      if assigned( lv.DynamicAggregator ) then begin
        lv.DynamicAggregator.RadiusAsText := vAggRadius ;
        lv.DynamicAggregator.Threshold := vAggThreshold ;
      end ;
    end
    else if oParent.oLayer is TGIS_LayerPixel then begin
      TGIS_LayerPixel(oParent.oLayer).MinHeight := vHeightMin ;
      TGIS_LayerPixel(oParent.oLayer).MaxHeight := vHeightMax ;
      case vInterpretation of
        0 : TGIS_LayerPixel(oParent.oLayer).Interpretation := TGIS_LayerPixelInterpretation.Default ;
        1 : TGIS_LayerPixel(oParent.oLayer).Interpretation := TGIS_LayerPixelInterpretation.Pixel ;
        2 : TGIS_LayerPixel(oParent.oLayer).Interpretation := TGIS_LayerPixelInterpretation.Grid ;
      end;
    end ;
    oParent.oLayer.UseConfig := vUseConfig ;
  end ;

  function TGIS_ControlLegendFormMVC_General.fget_Path : String ;
  begin
    Result := oParent.oLayer.Path ;
  end ;

  function TGIS_ControlLegendFormMVC_General.fget_Name : String ;
  begin
    Result := oParent.oLayer.Name ;
  end ;

  function TGIS_ControlLegendFormMVC_General.fget_Caption : String ;
  begin
    Result := vCaption ;
  end ;

  procedure TGIS_ControlLegendFormMVC_General.fset_Caption(
    const _str : String
  ) ;
  begin
    vCaption := _str ;
  end ;

  function TGIS_ControlLegendFormMVC_General.fget_CS : TGIS_CSCoordinateSystem ;
  begin
    Result := vCS ;
  end ;

  procedure TGIS_ControlLegendFormMVC_General.fset_CS(
    const _cs : TGIS_CSCoordinateSystem
  ) ;
  begin
    vCS := _cs ;
  end ;

  function TGIS_ControlLegendFormMVC_General.fget_Basemap : Boolean ;
  begin
    Result := vBasemap ;
  end ;

  procedure TGIS_ControlLegendFormMVC_General.fset_Basemap(
    const _bool : Boolean
  ) ;
  begin
    vBasemap := _bool ;
  end ;

  function TGIS_ControlLegendFormMVC_General.fget_CachedPaint : Boolean ;
  begin
    Result := vCachedPaint ;
  end ;

  procedure TGIS_ControlLegendFormMVC_General.fset_CachedPaint(
    const _bool : Boolean
  ) ;
  begin
    vCachedPaint := _bool ;
  end ;

  function TGIS_ControlLegendFormMVC_General.fget_IgnoreShapeParams : Boolean ;
  begin
    Result := vIgnoreShapeParams ;
  end ;

  procedure TGIS_ControlLegendFormMVC_General.fset_IgnoreShapeParams(
    const _bool : Boolean
  ) ;
  begin
    vIgnoreShapeParams := _bool ;
  end ;

  function TGIS_ControlLegendFormMVC_General.fget_MultipassRendering : Boolean ;
  begin
    Result := vMultipassRendering ;
  end ;

  procedure TGIS_ControlLegendFormMVC_General.fset_MultipassRendering(
    const _bool : Boolean
  ) ;
  begin
    vMultipassRendering := _bool ;
  end ;

  function TGIS_ControlLegendFormMVC_General.fget_Transparency : Integer ;
  begin
    Result := vTransparency ;
  end ;

  procedure TGIS_ControlLegendFormMVC_General.fset_Transparency(
    const _val : Integer
  ) ;
  begin
    vTransparency := _val ;
  end ;

  function TGIS_ControlLegendFormMVC_General.fget_Addition : Integer ;
  begin
    Result := vAddition ;
  end ;

  procedure TGIS_ControlLegendFormMVC_General.fset_Addition(
    const _val : Integer
  ) ;
  begin
    vAddition := _val ;
  end ;

  function TGIS_ControlLegendFormMVC_General.getLayerInfo : String ;
  var
    ext   : TGIS_Extent3D ;
    is3D  : Boolean ;
    layer : TGIS_Layer ;
    sb    : TStringBuilder ;
    lv    : TGIS_LayerVector ;
    lp    : TGIS_LayerPixel ;
    px    : Double ;
    py    : Double ;
  begin
    layer := oParent.Layer ;

    Result := layer.FileInfo ;

    if layer is TGIS_LayerVector then
      ext   := layer.Extent3D
    else
      ext   := GisExtent3DFrom2D( layer.Extent ) ;

    is3D := not((ext.ZMin = 0) and (ext.ZMax = 0)) and
            not((ext.ZMin = 1) and (ext.ZMax = -1));

    sb := TStringBuilder.Create ;
    try
      sb.AppendLine ;
      sb.AppendLine ;
      sb.Append( 'Extent : ' ) ;
      sb.AppendLine ;
      sb.Append( '  XMin : ' + DotFloatToStr( ext.XMin ) ) ;
      sb.AppendLine ;
      sb.Append( '  YMin : ' + DotFloatToStr( ext.YMin ) ) ;
      sb.AppendLine ;
      if is3D then begin
      sb.Append( '  ZMin : ' + DotFloatToStr( ext.ZMin ) ) ;
      sb.AppendLine ;
      end ;
      sb.Append( '  XMax : ' + DotFloatToStr( ext.XMax ) ) ;
      sb.AppendLine ;
      sb.Append( '  YMax : ' + DotFloatToStr( ext.YMax ) ) ;
      sb.AppendLine ;
      if is3D then begin
      sb.Append( '  ZMax : ' + DotFloatToStr( ext.ZMax ) ) ;
      sb.AppendLine ;
      end;

      if layer is TGIS_LayerVector then begin
        lv := TGIS_LayerVector(layer) ;

        sb.AppendLine ;
        sb.Append( 'Default shape type : ' ) ;
        case lv.DefaultShapeType of
          TGIS_ShapeType.Point :       sb.Append( 'Point'      ) ;
          TGIS_ShapeType.MultiPoint :  sb.Append( 'MultiPoint' ) ;
          TGIS_ShapeType.Arc :         sb.Append( 'Line'       ) ;
          TGIS_ShapeType.Polygon :     sb.Append( 'Polygon'    ) ;
          TGIS_ShapeType.MultiPatch :  sb.Append( 'MultiPatch' ) ;
          TGIS_ShapeType.Complex :     sb.Append( 'Complex'    )
        else                           sb.Append( 'Unknown'    )
        end ;

        sb.AppendLine ;
        sb.Append( 'Supported shapes : ' ) ;
        if TGIS_ShapeType.Point in lv.SupportedShapesAll then
          sb.Append( 'Point ' ) ;
        if TGIS_ShapeType.MultiPoint in lv.SupportedShapesAll then
          sb.Append( 'MultiPoint ' ) ;
        if TGIS_ShapeType.Arc in lv.SupportedShapesAll then
          sb.Append( 'Line ' ) ;
        if TGIS_ShapeType.Polygon in lv.SupportedShapesAll then
          sb.Append( 'Polygon ' ) ;
        if TGIS_ShapeType.MultiPatch in lv.SupportedShapesAll then
          sb.Append( 'MultiPatch ' ) ;
        if TGIS_ShapeType.Complex in lv.SupportedShapesAll then
          sb.Append( 'Complex' ) ;

        sb.AppendLine ;
        sb.Append( 'Default dimension : ' ) ;
        case lv.DefaultDimension of
          TGIS_DimensionType.XY :      sb.Append( 'XY'      ) ;
          TGIS_DimensionType.XYZ :     sb.Append( 'XYZ'     ) ;
          TGIS_DimensionType.XYZM :    sb.Append( 'XYZM'    ) ;
          TGIS_DimensionType.XYM :     sb.Append( 'XYM'     ) ;
          TGIS_DimensionType.Unknown : sb.Append( 'Unknown' )
        end ;

        sb.AppendLine ;
        sb.Append( 'Supported dimension : ' ) ;
        if TGIS_DimensionType.XY in lv.SupportedDimensions then
          sb.Append( 'XY ' ) ;
        if TGIS_DimensionType.XYZ in lv.SupportedDimensions then
          sb.Append( 'XYZ ' ) ;
        if TGIS_DimensionType.XYM in lv.SupportedDimensions then
          sb.Append( 'XYM ' ) ;
        if TGIS_DimensionType.XYZM in lv.SupportedDimensions then
          sb.Append( 'XYZM' ) ;

      end ;

      if layer is TGIS_LayerPixel then begin
        lp := TGIS_LayerPixel(layer) ;

        sb.AppendLine ;
        sb.Append( 'Width : ' ) ;
        sb.Append( IntToStr( lp.BitWidth ) ) ;
        sb.AppendLine ;
        sb.Append( 'Height : ' ) ;
        sb.Append( IntToStr( lp.BitHeight ) ) ;
        sb.AppendLine ;
        sb.Append( 'Bands Count : ' ) ;
        sb.Append( IntToStr( lp.BandsCount ) ) ;

        if ( lp.BitWidth  <> 0 ) and ( lp.BitHeight <> 0 ) then begin
          px := ( lp.Extent.XMax - lp.Extent.XMin ) / lp.BitWidth  ;
          py := ( lp.Extent.YMax - lp.Extent.YMin ) / lp.BitHeight ;
          sb.AppendLine ;
          sb.Append( Format( 'Pixel Size : %s x %s',
                             [ DotFloatToStrPrec( px, 3 ), DotFloatToStrPrec( py, 3 ) ] )
                    ) ;
        end ;

        if lp.IsGrid then begin
          sb.AppendLine ;
          sb.Append( 'NoData value : ' ) ;
          sb.Append( DotFloatToStr(lp.Params.Pixel.GridNoValue) ) ;
          sb.AppendLine ;
          sb.Append( 'Min height : ' ) ;
          sb.Append( DotFloatToStr(lp.MinHeight) ) ;
          sb.AppendLine ;
          sb.Append( 'Max height : ' ) ;
          sb.Append( DotFloatToStr(lp.MaxHeight) ) ;
        end ;
      end ;

      Result := Result + sb.ToString ;
    finally
      FreeObject( sb ) ;
    end ;
  end ;

  function TGIS_ControlLegendFormMVC_General.fget_FileInformation : String ;
  begin
    Result := getLayerInfo ;
  end ;
  function TGIS_ControlLegendFormMVC_General.fget_Comments : String ;
  begin
    Result := vComments ;
  end ;
  procedure TGIS_ControlLegendFormMVC_General.fset_Comments(
    const _str : String
  ) ;
  begin
    vComments := _str ;
  end ;

  function TGIS_ControlLegendFormMVC_General.fget_Scope : String ;
  begin
    Result := vScope ;
  end ;

  procedure TGIS_ControlLegendFormMVC_General.fset_Scope(
    const _str : String
  ) ;
  begin
    vScope := _str ;
  end ;

  function TGIS_ControlLegendFormMVC_General.fget_CodePage : Integer ;
  begin
    Result := vCodePage ;
  end ;

  procedure TGIS_ControlLegendFormMVC_General.fset_CodePage(
    const _val : Integer
  ) ;
  begin
    vCodePage := _val ;
  end ;

  function TGIS_ControlLegendFormMVC_General.fget_HeightMin : Single ;
  begin
    Result := vHeightMin ;
  end ;

  procedure TGIS_ControlLegendFormMVC_General.fset_HeightMin(
    const _val : Single
  ) ;
  begin
    vHeightMin := _val ;
  end ;

  function TGIS_ControlLegendFormMVC_General.fget_HeightMax : Single ;
  begin
    Result := vHeightMax ;
  end ;

  procedure TGIS_ControlLegendFormMVC_General.fset_HeightMax(
    const _val : Single
  ) ;
  begin
    vHeightMax := _val ;
  end ;

  function TGIS_ControlLegendFormMVC_General.fget_Interpretation : Integer ;
  begin
    Result := vInterpretation ;
  end ;

  procedure TGIS_ControlLegendFormMVC_General.fset_Interpretation(
    const _val : Integer
  ) ;
  begin
    vInterpretation := _val ;
  end ;

  function  TGIS_ControlLegendFormMVC_General.fget_UseConfig : Boolean ;
  begin
    Result := vUseConfig ;
  end ;

  procedure TGIS_ControlLegendFormMVC_General.fset_UseConfig(
    const _val : Boolean
  ) ;
  begin
    vUseConfig := _val ;
  end ;

  function  TGIS_ControlLegendFormMVC_General.fget_AggregationMethod : String ;
  begin
    Result := vAggMethod ;
  end ;

  procedure TGIS_ControlLegendFormMVC_General.fset_AggregationMethod(
    const _val : String
  ) ;
  begin
    vAggMethod := _val ;
  end ;

  function  TGIS_ControlLegendFormMVC_General.fget_AggregationRadius : String ;
  begin
    Result := vAggRadius ;
  end ;

  procedure TGIS_ControlLegendFormMVC_General.fset_AggregationRadius(
    const _val : String
  ) ;
  begin
    vAggRadius := _val ;
  end ;

  function  TGIS_ControlLegendFormMVC_General.fget_AggregationThreshold : Integer ;
  begin
    Result := vAggThreshold ;
  end ;

  procedure TGIS_ControlLegendFormMVC_General.fset_AggregationThreshold(
    const _val : Integer
  ) ;
  begin
    vAggThreshold := _val ;
  end ;

  procedure TGIS_ControlLegendFormMVC_General.DoCSClick ;
  begin
    raiseCallback( 1 ) ; // open CS selection dialog box
  end ;

  procedure TGIS_ControlLegendFormMVC_General.DoControlChange ;
  begin
    raiseCallback( 2 ) ; // change control
  end ;

  procedure TGIS_ControlLegendFormMVC_General.DoAggregateChange ;
  begin
    raiseCallback( 3 ) ; // change control
  end ;
  {$ENDREGION}

  {$REGION 'TGIS_ControlLegendFormMVC_3D'}
//==============================================================================
// TGIS_ControlLegendFormMVC_3D
//==============================================================================

  constructor TGIS_ControlLegendFormMVC_3D.Create(
    const _parent : TGIS_ControlLegendFormMVC
  ) ;
  begin
    inherited ;
    FHasPreview := False ;
    vNormalizedZ := TList<TGIS_3DNormalizationType>.Create ;
    vScaleZ      := TList<Double>.Create ;
    vFalseZ      := TList<String>.Create ;
    if oParent.IsVector then begin
      vNormalizedM := TList<TGIS_3DNormalizationType>.Create ;
      vScaleM      := TList<Double>.Create ;
      vFalseM      := TList<String>.Create ;
      vGround      := TList<TGIS_3DGroundType>.Create ;
      vBasement    := TList<TGIS_3DBasementType>.Create ;
    end ;
    readAll ;
  end ;

  {$IFDEF DCC}
    destructor TGIS_ControlLegendFormMVC_3D.Destroy ;
    begin
      FreeObject( vNormalizedZ ) ;
      FreeObject( vScaleZ ) ;
      FreeObject( vNormalizedM ) ;
      FreeObject( vScaleM ) ;
      FreeObject( vFalseZ ) ;
      FreeObject( vFalseM ) ;
      FreeObject( vGround ) ;
      FreeObject( vBasement ) ;
      inherited ;
    end ;
  {$ENDIF}

  procedure TGIS_ControlLegendFormMVC_3D.readAll ;
  var
    i : Integer ;
  begin
    vMode := oParent.oLayer.View3D.Mode ;
    for i := 0 to oParent.SectionCount - 1 do begin
      oParent.SectionIndex := i ;
      SectionRead( False ) ;
    end ;
  end ;

  procedure TGIS_ControlLegendFormMVC_3D.SectionRead(
    const _blank : Boolean
  ) ;
  var
    sec : TGIS_ParamsSection ;
  begin
    inherited ;

    if _blank then
      sec := oParent.oBlank.Params
    else
    if assigned( oParent.oSection ) and not oParent.bForceClean then
      sec := oParent.oSection
    else
      sec := oParent.oBlank.Params ;
    vNormalizedZ.Add( sec.NormalizedZ ) ;
    vScaleZ.Add( sec.ScaleZ ) ;
    vFalseZ.Add( sec.FalseZAsText ) ;
    if oParent.IsVector then begin
      vNormalizedM.Add( TGIS_ParamsSectionVector( sec ).NormalizedM ) ;
      vScaleM.Add( TGIS_ParamsSectionVector( sec ).ScaleM ) ;
      vFalseM.Add( TGIS_ParamsSectionVector( sec ).FalseMAsText ) ;
      vGround.Add( TGIS_ParamsSectionVector( sec ).Ground ) ;
      vBasement.Add( TGIS_ParamsSectionVector( sec ).Basement ) ;
    end ;
  end ;

  procedure TGIS_ControlLegendFormMVC_3D.SectionDelete ;
  begin
    inherited ;
    {$IFDEF OXYGENE}
      vNormalizedZ.RemoveAt( oParent.SectionIndex ) ;
      vScaleZ.RemoveAt( oParent.SectionIndex ) ;
      vFalseZ.RemoveAt( oParent.SectionIndex ) ;
    {$ELSE}
      vNormalizedZ.Delete( oParent.SectionIndex ) ;
      vScaleZ.Delete( oParent.SectionIndex ) ;
      vFalseZ.Delete( oParent.SectionIndex ) ;
    {$ENDIF}
    if oParent.IsVector then begin
      {$IFDEF OXYGENE}
        vNormalizedM.RemoveAt( oParent.SectionIndex ) ;
        vScaleM.RemoveAt( oParent.SectionIndex ) ;
        vFalseM.RemoveAt( oParent.SectionIndex ) ;
        vGround.RemoveAt( oParent.SectionIndex ) ;
        vBasement.RemoveAt( oParent.SectionIndex ) ;
      {$ELSE}
        vNormalizedM.Delete( oParent.SectionIndex ) ;
        vScaleM.Delete( oParent.SectionIndex ) ;
        vFalseM.Delete( oParent.SectionIndex ) ;
        vGround.Delete( oParent.SectionIndex ) ;
        vBasement.Delete( oParent.SectionIndex ) ;
      {$ENDIF}
    end ;
  end ;

  procedure TGIS_ControlLegendFormMVC_3D.SectionMove(
    const _up : Boolean
  ) ;
  var
    idx : Integer ;
    {$IFDEF OXYGENE}
      idx1 : Integer ;
      idx2 : Integer ;
      vb   : TGIS_3DNormalizationType ;
      vd   : Double ;
      vs   : String ;
      vt1  : TGIS_3DGroundType ;
      vt2  : TGIS_3DBasementType ;
    {$ENDIF}
  begin
    inherited ;
    if _up then
      idx := oParent.SectionIndex - 1
    else
      idx := oParent.SectionIndex + 1 ;
    {$IFDEF OXYGENE}
      if _up then begin
        idx1 := idx ;
        idx2 := oParent.SectionIndex ;
      end else begin
        idx1 := oParent.SectionIndex ;
        idx2 := idx ;
      end ;
      vb := vNormalizedZ[idx1] ;
      vNormalizedZ.RemoveAt( idx1 ) ;
      vNormalizedZ.Insert( idx2, vb ) ;
      vd := vScaleZ[idx1] ;
      vScaleZ.RemoveAt( idx1 ) ;
      vScaleZ.Insert( idx2, vd ) ;
      vs := vFalseZ[idx1] ;
      vFalseZ.RemoveAt( idx1 ) ;
      vFalseZ.Insert( idx2, vs ) ;
      if oParent.IsVector then begin
        vb := vNormalizedM[idx1] ;
        vNormalizedM.RemoveAt( idx1 ) ;
        vNormalizedM.Insert( idx2, vb ) ;
        vd := vScaleM[idx1] ;
        vScaleM.RemoveAt( idx1 ) ;
        vScaleM.Insert( idx2, vd ) ;
        vs := vFalseM[idx1] ;
        vFalseM.RemoveAt( idx1 ) ;
        vFalseM.Insert( idx2, vs ) ;
        vt1 := vGround[idx1] ;
        vGround.RemoveAt( idx1 ) ;
        vGround.Insert( idx2, vt1 ) ;
        vt2 := vBasement[idx1] ;
        vBasement.RemoveAt( idx1 ) ;
        vBasement.Insert( idx2, vt2 ) ;
      end ;
    {$ELSE}
      vNormalizedZ.Exchange( oParent.SectionIndex, idx ) ;
      vScaleZ.Exchange( oParent.SectionIndex, idx ) ;
      vFalseZ.Exchange( oParent.SectionIndex, idx ) ;
      if oParent.IsVector then begin
        vNormalizedM.Exchange( oParent.SectionIndex, idx ) ;
        vScaleM.Exchange( oParent.SectionIndex, idx ) ;
        vFalseM.Exchange( oParent.SectionIndex, idx ) ;
        vGround.Exchange( oParent.SectionIndex, idx ) ;
        vBasement.Exchange( oParent.SectionIndex, idx ) ;
      end ;
    {$ENDIF}
  end ;

  procedure TGIS_ControlLegendFormMVC_3D.Write ;
  var
    i : Integer ;
  begin
    inherited ;
    oParent.oLayer.View3D.Mode := vMode ;
    for i := 0 to oParent.SectionCount - 1 do begin
      oParent.SectionIndex := i ;
      oParent.oSection.NormalizedZ  := vNormalizedZ[i] ;
      oParent.oSection.ScaleZ       := vScaleZ[i] ;
      oParent.oSection.FalseZAsText := vFalseZ[i] ;
      if oParent.IsVector then begin
        TGIS_ParamsSectionVector( oParent.oSection ).NormalizedM := vNormalizedM[i] ;
        TGIS_ParamsSectionVector( oParent.oSection ).ScaleM      := vScaleM[i] ;
        TGIS_ParamsSectionVector( oParent.oSection ).FalseMAsText:= vFalseM[i] ;
        TGIS_ParamsSectionVector( oParent.oSection ).Ground      := vGround[i] ;
        TGIS_ParamsSectionVector( oParent.oSection ).Basement    := vBasement[i] ;
      end ;
    end ;
  end ;

  function TGIS_ControlLegendFormMVC_3D.fget_TreatLayerAs : TGIS_3DLayerType ;
  begin
    Result := vMode ;
  end ;

  procedure TGIS_ControlLegendFormMVC_3D.fset_TreatLayerAs(
    const _val : TGIS_3DLayerType
  ) ;
  begin
    vMode := _val ;
  end ;

  function TGIS_ControlLegendFormMVC_3D.fget_NormalizedZ : TGIS_3DNormalizationType ;
  begin
    Result := vNormalizedZ[oParent.SectionIndex] ;
  end ;

  procedure TGIS_ControlLegendFormMVC_3D.fset_NormalizedZ(
    const _val : TGIS_3DNormalizationType
  ) ;
  begin
    vNormalizedZ[oParent.SectionIndex] := _val ;
  end ;

  function TGIS_ControlLegendFormMVC_3D.fget_ScaleZ : Double ;
  begin
    Result := vScaleZ[oParent.SectionIndex] ;
  end ;

  procedure TGIS_ControlLegendFormMVC_3D.fset_ScaleZ(
    const _val : Double
  ) ;
  begin
    vScaleZ[oParent.SectionIndex] := _val ;
  end ;

  function TGIS_ControlLegendFormMVC_3D.fget_NormalizedM : TGIS_3DNormalizationType ;
  begin
    Result := vNormalizedM[oParent.SectionIndex] ;
  end ;

  procedure TGIS_ControlLegendFormMVC_3D.fset_NormalizedM(
    const _val : TGIS_3DNormalizationType
  ) ;
  begin
    vNormalizedM[oParent.SectionIndex] := _val ;
  end ;

  function TGIS_ControlLegendFormMVC_3D.fget_ScaleM : Double ;
  begin
    Result := vScaleM[oParent.SectionIndex] ;
  end ;

  procedure TGIS_ControlLegendFormMVC_3D.fset_ScaleM(
    const _val : Double
  ) ;
  begin
    vScaleM[oParent.SectionIndex] := _val ;
  end ;

  function TGIS_ControlLegendFormMVC_3D.fget_FalseZ : String ;
  begin
    Result := vFalseZ[oParent.SectionIndex] ;
  end ;

  procedure TGIS_ControlLegendFormMVC_3D.fset_FalseZ(
    const _val : String
  ) ;
  begin
    vFalseZ[oParent.SectionIndex] := _val ;
  end ;

  function TGIS_ControlLegendFormMVC_3D.fget_FalseM : String ;
  begin
    Result := vFalseM[oParent.SectionIndex] ;
  end ;

  procedure TGIS_ControlLegendFormMVC_3D.fset_FalseM(
    const _val : String
  ) ;
  begin
    vFalseM[oParent.SectionIndex] := _val ;
  end ;

  function TGIS_ControlLegendFormMVC_3D.fget_AdjustZ : TGIS_3DGroundType ;
  begin
    Result := vGround[oParent.SectionIndex] ;
  end ;

  procedure TGIS_ControlLegendFormMVC_3D.fset_AdjustZ(
    const _val : TGIS_3DGroundType
  ) ;
  begin
    vGround[oParent.SectionIndex] := _val ;
  end ;

  function TGIS_ControlLegendFormMVC_3D.fget_AdjustBasement
    : TGIS_3DBasementType ;
  begin
    Result := vBasement[oParent.SectionIndex] ;
  end ;

  procedure TGIS_ControlLegendFormMVC_3D.fset_AdjustBasement(
    const _val : TGIS_3DBasementType
  ) ;
  begin
    vBasement[oParent.SectionIndex] := _val ;
  end ;

  function TGIS_ControlLegendFormMVC_3D.fget_Params : TGIS_ParamsSection ;
  begin
    Result := oParent.oSection ;
  end ;

  procedure TGIS_ControlLegendFormMVC_3D.DoAs2DClick ;
  begin
    raiseCallback( 1 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_3D.DoAsDEMClick ;
  begin
    raiseCallback( 2 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_3D.DoAs3DClick ;
  begin
    raiseCallback( 3 ) ;
  end ;
  {$ENDREGION}

  {$REGION 'TGIS_ControlLegendFormMVC_Section'}
//==============================================================================
// TGIS_ControlLegendFormMVC_Section
//==============================================================================

  constructor TGIS_ControlLegendFormMVC_Section.Create(
    const _parent : TGIS_ControlLegendFormMVC
  ) ;
  begin
    inherited ;
    FHasPreview := False ;
    vVisible  := TList<Boolean>.Create ;
    vMinScale := TList<Double>.Create ;
    vMaxScale := TList<Double>.Create ;
    vQuery    := TList<String>.Create ;
    vLegend   := TList<String>.Create ;
    readAll ;
  end ;

  {$IFDEF DCC}
    destructor TGIS_ControlLegendFormMVC_Section.Destroy ;
    begin
      FreeObject( vVisible ) ;
      FreeObject( vMinScale ) ;
      FreeObject( vMaxScale ) ;
      FreeObject( vQuery ) ;
      FreeObject( vLegend ) ;
      inherited ;
    end ;
  {$ENDIF}

  procedure TGIS_ControlLegendFormMVC_Section.readAll ;
  var
    i : Integer ;
  begin
    for i := 0 to oParent.SectionCount - 1 do begin
      oParent.SectionIndex := i ;
      SectionRead( False ) ;
    end ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Section.SectionRead(
    const _blank : Boolean
  ) ;
  var
    sec : TGIS_ParamsSection ;
  begin
    inherited ;

    if _blank then
      sec := oParent.oBlank.Params
    else
    if assigned( oParent.oSection ) and not oParent.bForceClean then
      sec := oParent.oSection
    else
      sec := oParent.oBlank.Params ;
    vVisible.Add( sec.Visible ) ;
    vMinScale.Add( sec.MinScale ) ;
    vMaxScale.Add( sec.MaxScale ) ;
    if oParent.IsVector then begin
      vQuery.Add( TGIS_ParamsSectionVector( sec ).Query ) ;
    end ;
    vLegend.Add( sec.Legend ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Section.SectionDelete ;
  begin
    inherited ;
    {$IFDEF OXYGENE}
      vVisible.RemoveAt( oParent.SectionIndex ) ;
      vMinScale.RemoveAt( oParent.SectionIndex ) ;
      vMaxScale.RemoveAt( oParent.SectionIndex ) ;
      if oParent.IsVector then
        vQuery.RemoveAt( oParent.SectionIndex ) ;
      vLegend.RemoveAt( oParent.SectionIndex ) ;
    {$ELSE}
      vVisible.Delete( oParent.SectionIndex ) ;
      vMinScale.Delete( oParent.SectionIndex ) ;
      vMaxScale.Delete( oParent.SectionIndex ) ;
      if oParent.IsVector then
        vQuery.Delete( oParent.SectionIndex ) ;
      vLegend.Delete( oParent.SectionIndex ) ;
    {$ENDIF}
  end ;

  procedure TGIS_ControlLegendFormMVC_Section.SectionMove(
    const _up : Boolean
  ) ;
  var
    idx : Integer ;
    {$IFDEF OXYGENE}
      idx1 : Integer ;
      idx2 : Integer ;
      vb : Boolean ;
      vd : Double ;
      vs : String ;
    {$ENDIF}
  begin
    inherited ;
    if _up then
      idx := oParent.SectionIndex - 1
    else
      idx := oParent.SectionIndex + 1 ;
    {$IFDEF OXYGENE}
      if _up then begin
        idx1 := idx ;
        idx2 := oParent.SectionIndex ;
      end else begin
        idx1 := oParent.SectionIndex ;
        idx2 := idx ;
      end ;
      vb := vVisible[idx1] ;
      vVisible.RemoveAt( idx1 ) ;
      vVisible.Insert( idx2, vb ) ;
      vd := vMinScale[idx1] ;
      vMinScale.RemoveAt( idx1 ) ;
      vMinScale.Insert( idx2, vd ) ;
      vd := vMaxScale[idx1] ;
      vMaxScale.RemoveAt( idx1 ) ;
      vMaxScale.Insert( idx2, vd ) ;
      if oParent.IsVector then begin
        vs := vQuery[idx1] ;
        vQuery.RemoveAt( idx1 ) ;
        vQuery.Insert( idx2, vs ) ;
      end ;
      vs := vLegend[idx1] ;
      vLegend.RemoveAt( idx1 ) ;
      vLegend.Insert( idx2, vs ) ;
    {$ELSE}
      vVisible.Exchange( oParent.SectionIndex, idx ) ;
      vMinScale.Exchange( oParent.SectionIndex, idx ) ;
      vMaxScale.Exchange( oParent.SectionIndex, idx ) ;
      if oParent.IsVector then
        vQuery.Exchange( oParent.SectionIndex, idx ) ;
      vLegend.Exchange( oParent.SectionIndex, idx ) ;
    {$ENDIF}
  end ;

  procedure TGIS_ControlLegendFormMVC_Section.Write ;
  var
    i : Integer ;
  begin
    inherited ;
    for i := 0 to oParent.SectionCount - 1 do begin
      oParent.SectionIndex := i ;
      oParent.oSection.Visible := vVisible[i] ;
      oParent.oSection.MinScale := vMinScale[i] ;
      oParent.oSection.MaxScale := vMaxScale[i] ;
      if oParent.IsVector then begin
        TGIS_ParamsSectionVector( oParent.oSection ).Query := vQuery[i] ;
      end ;
      oParent.oSection.Legend := vLegend[i] ;
    end ;
  end ;

  function TGIS_ControlLegendFormMVC_Section.fget_Visible : Boolean ;
  begin
    Result := vVisible[oParent.SectionIndex] ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Section.fset_Visible(
    const _bool : Boolean
  ) ;
  begin
    vVisible[oParent.SectionIndex] := _bool ;
  end ;

  function TGIS_ControlLegendFormMVC_Section.fget_MinScale : Double ;
  begin
    Result := vMinScale[oParent.SectionIndex] ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Section.fset_MinScale(
    const _val : Double
  ) ;
  begin
    vMinScale[oParent.SectionIndex] := _val ;
  end ;

  function TGIS_ControlLegendFormMVC_Section.fget_MaxScale : Double ;
  begin
    Result := vMaxScale[oParent.SectionIndex] ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Section.fset_MaxScale(
    const _val : Double
  ) ;
  begin
    vMaxScale[oParent.SectionIndex] := _val ;
  end ;

  function TGIS_ControlLegendFormMVC_Section.fget_Query : String ;
  begin
    Result := vQuery[oParent.SectionIndex] ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Section.fset_Query(
    const _str : String
  ) ;
  begin
    vQuery[oParent.SectionIndex] := _str ;
  end ;

  function TGIS_ControlLegendFormMVC_Section.fget_Legend : String ;
  begin
    Result := vLegend[oParent.SectionIndex] ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Section.fset_Legend(
    const _str : String
  ) ;
  begin
    vLegend[oParent.SectionIndex] := _str ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Section.DoVisibleClick ;
  begin
    raiseCallback( 0 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Section.DoMinScaleClick ;
  begin
    raiseCallback( 1 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Section.DoMinScaleClear ;
  begin
    raiseCallback( 2 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Section.DoMinScaleChange ;
  begin
    raiseCallback( 3 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Section.DoMaxScaleClick ;
  begin
    raiseCallback( 4 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Section.DoMaxScaleClear ;
  begin
    raiseCallback( 5 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Section.DoMaxScaleChange ;
  begin
    raiseCallback( 6 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Section.DoQueryChange ;
  begin
    raiseCallback( 7 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Section.DoLegendChange ;
  begin
    raiseCallback( 8 ) ;
  end ;
  {$ENDREGION}

  {$REGION 'TGIS_ControlLegendFormMVC_Renderer'}
//==============================================================================
// TGIS_ControlLegendFormMVC_Renderer
//==============================================================================
  constructor TGIS_ControlLegendFormMVC_Renderer.Create(
    const _parent : TGIS_ControlLegendFormMVC
  ) ;
  begin
    inherited ;
    FHasPreview := False ;
    lstParamsRender := TObjectList<TGIS_ParamsRender>.Create ;
    readAll ;
  end ;

  {$IFDEF DCC}
    destructor TGIS_ControlLegendFormMVC_Renderer.Destroy ;
    begin
      FreeObject( lstParamsRender ) ;
      inherited ;
    end ;
  {$ENDIF}

  procedure TGIS_ControlLegendFormMVC_Renderer.readAll ;
  var
    i : Integer ;
  begin
    for i := 0 to oParent.SectionCount - 1 do begin
      oParent.SectionIndex := i ;
      SectionRead( False ) ;
    end ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Renderer.SectionRead(
    const _blank : Boolean
  ) ;
  var
    sec : TGIS_ParamsSectionVector ;
  begin
    inherited ;

    if _blank then
      sec := TGIS_ParamsSectionVector( oParent.oBlank.Params )
    else
    if assigned( oParent.oSection ) and not oParent.bForceClean then
      sec := TGIS_ParamsSectionVector( oParent.oSection )
    else
      sec := TGIS_ParamsSectionVector( oParent.oBlank.Params ) ;
    lstParamsRender.Add( TGIS_ParamsRender.Create ) ;
    lstParamsRender[oParent.SectionIndex].Assign( sec.Render ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Renderer.SectionDelete ;
  begin
    inherited ;
    lstParamsRender.Delete( oParent.SectionIndex ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Renderer.SectionMove(
    const _up : Boolean
  ) ;
  var
    idx : Integer ;
  begin
    inherited ;
    if _up then
      idx := oParent.SectionIndex - 1
    else
      idx := oParent.SectionIndex + 1 ;
    lstParamsRender.Exchange( oParent.SectionIndex, idx ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Renderer.SectionWrite(
    const _section : TGIS_ParamsSection
  )  ;
  var
    sec : TGIS_ParamsSectionVector ;
  begin
    sec := TGIS_ParamsSectionVector( _section ) ;
    sec.Render.Assign( lstParamsRender[oParent.SectionIndex] ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Renderer.Write ;
  var
    i   : Integer ;
  begin
    inherited ;
    for i := 0 to oParent.SectionCount - 1 do begin
      oParent.SectionIndex := i ;
      SectionWrite( oParent.oSection ) ;
    end ;
  end ;

  function TGIS_ControlLegendFormMVC_Renderer.fget_ParamsRender : TGIS_ParamsRender ;
  begin
    Result := lstParamsRender[oParent.SectionIndex] ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Renderer.DoExpressionChange ;
  begin
    raiseCallback( 1 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Renderer.DoZonesChange ;
  begin
    raiseCallback( 2 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Renderer.DoZonesExChange ;
  begin
    raiseCallback( 3 ) ;
  end ;
  procedure TGIS_ControlLegendFormMVC_Renderer.DoControlChange ;
  begin
    raiseCallback( 0 ) ;
  end ;
  {$ENDREGION}

  {$REGION 'TGIS_ControlLegendFormMVC_Marker'}
//==============================================================================
// TGIS_ControlLegendFormMVC_Marker
//==============================================================================

  constructor TGIS_ControlLegendFormMVC_Marker.Create(
    const _parent : TGIS_ControlLegendFormMVC
  ) ;
  begin
    inherited ;
    FHasPreview := True ;
    lstParamsMarker := TObjectList<TGIS_ParamsMarker>.Create ;
    readAll ;
  end ;

  {$IFDEF DCC}
    destructor TGIS_ControlLegendFormMVC_Marker.Destroy ;
    begin
      FreeObject( lstParamsMarker ) ;
      inherited ;
    end ;
  {$ENDIF}

  procedure TGIS_ControlLegendFormMVC_Marker.readAll ;
  var
    i : Integer ;
  begin
    for i := 0 to oParent.SectionCount - 1 do begin
      oParent.SectionIndex := i ;
      SectionRead( False ) ;
    end ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Marker.SectionRead(
    const _blank : Boolean
  ) ;
  var
    sec : TGIS_ParamsSectionVector ;
  begin
    inherited ;

    if _blank then
      sec := TGIS_ParamsSectionVector( oParent.oBlank.Params )
    else
    if assigned( oParent.oSection ) and not oParent.bForceClean then
      sec := TGIS_ParamsSectionVector( oParent.oSection )
    else
      sec := TGIS_ParamsSectionVector( oParent.oBlank.Params ) ;
    lstParamsMarker.Add( TGIS_ParamsMarker.Create ) ;
    lstParamsMarker[oParent.SectionIndex].Assign( sec.Marker ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Marker.SectionDelete ;
  begin
    inherited ;
    lstParamsMarker.Delete( oParent.SectionIndex ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Marker.SectionMove(
    const _up : Boolean
  ) ;
  var
    idx : Integer ;
  begin
    inherited ;
    if _up then
      idx := oParent.SectionIndex - 1
    else
      idx := oParent.SectionIndex + 1 ;
    lstParamsMarker.Exchange( oParent.SectionIndex, idx ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Marker.SectionWrite(
    const _section : TGIS_ParamsSection
  )  ;
  var
    sec : TGIS_ParamsSectionVector ;
  begin
    sec := TGIS_ParamsSectionVector( _section ) ;
    sec.Marker.Assign( lstParamsMarker[oParent.SectionIndex] ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Marker.Write ;
  var
    i   : Integer ;
  begin
    inherited ;
    for i := 0 to oParent.SectionCount - 1 do begin
      oParent.SectionIndex := i ;
      SectionWrite( oParent.oSection ) ;
    end ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Marker.PreparePreview(
    const _viewer : IGIS_Viewer
  ) ;
  var
    ll  : TGIS_LayerVector ;
    shp : TGIS_Shape       ;
  begin
    _viewer.Close ;
    ll := TGIS_LayerVector.Create ;
    _viewer.Add( ll ) ;
    shp := ll.CreateShape( TGIS_ShapeType.Point ) ;
    shp.Lock( TGIS_Lock.Extent ) ;
    shp.AddPart ;
    shp.AddPoint( GisPoint( 0, 0 ) ) ;
    shp.Unlock ;
    shp := ll.CreateShape( TGIS_ShapeType.Polygon ) ;
    shp.Lock( TGIS_Lock.Extent ) ;
    shp.AddPart ;
    shp.AddPoint( GisPoint( -3, -3 ) ) ;
    shp.AddPoint( GisPoint( -3,  3 ) ) ;
    shp.AddPoint( GisPoint(  3,  3 ) ) ;
    shp.AddPoint( GisPoint(  3, -3 ) ) ;
    shp.AddPoint( GisPoint( -3, -3 ) ) ;
    shp.Unlock ;
    shp.IsHidden := True ;
    _viewer.FullExtent ;
  end ;

  function TGIS_ControlLegendFormMVC_Marker.fget_ParamsMarker : TGIS_ParamsMarker ;
  begin
    Result := lstParamsMarker[oParent.SectionIndex] ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Marker.DoSmartSizeFieldChange ;
  begin
    raiseCallback( 60 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Marker.DoPatternChange ;
  begin
    raiseCallback( 100 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Marker.DoOPatternChange ;
  begin
    raiseCallback( 130 ) ;
  end ;
  procedure TGIS_ControlLegendFormMVC_Marker.DoControlChange ;
  begin
    raiseCallback( 0 ) ;
  end ;
  {$ENDREGION}

  {$REGION 'TGIS_ControlLegendFormMVC_Line'}
//==============================================================================
// TGIS_ControlLegendFormMVC_Line
//==============================================================================

  constructor TGIS_ControlLegendFormMVC_Line.Create(
    const _parent : TGIS_ControlLegendFormMVC
  ) ;
  begin
    inherited ;
    FHasPreview := True ;
    lstParamsLine := TObjectList<TGIS_ParamsLine>.Create ;
    readAll ;
  end ;

  {$IFDEF DCC}
    destructor TGIS_ControlLegendFormMVC_Line.Destroy ;
    begin
      FreeObject( lstParamsLine ) ;
      inherited ;
    end ;
  {$ENDIF}

  procedure TGIS_ControlLegendFormMVC_Line.readAll ;
  var
    i : Integer ;
  begin
    for i := 0 to oParent.SectionCount - 1 do begin
      oParent.SectionIndex := i ;
      SectionRead( False ) ;
    end ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Line.SectionRead(
    const _blank : Boolean
  ) ;
  var
    sec : TGIS_ParamsSectionVector ;
  begin
    inherited ;

    if _blank then
      sec := TGIS_ParamsSectionVector( oParent.oBlank.Params )
    else
    if assigned( oParent.oSection ) and not oParent.bForceClean then
      sec := TGIS_ParamsSectionVector( oParent.oSection )
    else
      sec := TGIS_ParamsSectionVector( oParent.oBlank.Params ) ;
    lstParamsLine.Add( TGIS_ParamsLine.Create ) ;
    lstParamsLine[oParent.SectionIndex].Assign( sec.Line ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Line.SectionDelete ;
  begin
    inherited ;
    lstParamsLine.Delete( oParent.SectionIndex ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Line.SectionMove(
    const _up : Boolean
  ) ;
  var
    idx : Integer ;
  begin
    inherited ;
    if _up then
      idx := oParent.SectionIndex - 1
    else
      idx := oParent.SectionIndex + 1 ;
    lstParamsLine.Exchange( oParent.SectionIndex, idx ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Line.SectionWrite(
    const _section : TGIS_ParamsSection
  )  ;
  var
    sec : TGIS_ParamsSectionVector ;
  begin
    sec := TGIS_ParamsSectionVector( _section ) ;
    sec.Line.Assign( lstParamsLine[oParent.SectionIndex] ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Line.Write ;
  var
    i   : Integer ;
  begin
    inherited ;
    for i := 0 to oParent.SectionCount - 1 do begin
      oParent.SectionIndex := i ;
      SectionWrite( oParent.oSection ) ;
    end ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Line.PreparePreview(
    const _viewer : IGIS_Viewer
  ) ;
  var
    ll  : TGIS_LayerVector ;
    shp : TGIS_Shape       ;
  begin
    _viewer.Close ;
    ll := TGIS_LayerVector.Create ;
    _viewer.Add( ll ) ;
    shp := ll.CreateShape( TGIS_ShapeType.Arc ) ;
    shp.Lock( TGIS_Lock.Extent ) ;
    shp.AddPart ;
    shp.AddPoint( GisPoint( -2, -2 ) ) ;
    shp.AddPoint( GisPoint(  2,  2 ) ) ;
    shp.Unlock ;
    shp := ll.CreateShape( TGIS_ShapeType.Polygon ) ;
    shp.Lock( TGIS_Lock.Extent ) ;
    shp.AddPart ;
    shp.AddPoint( GisPoint( -3, -3 ) ) ;
    shp.AddPoint( GisPoint( -3,  3 ) ) ;
    shp.AddPoint( GisPoint(  3,  3 ) ) ;
    shp.AddPoint( GisPoint(  3, -3 ) ) ;
    shp.AddPoint( GisPoint( -3, -3 ) ) ;
    shp.Unlock ;
    shp.IsHidden := True ;
    _viewer.FullExtent ;
  end ;

  function TGIS_ControlLegendFormMVC_Line.fget_ParamsLine : TGIS_ParamsLine ;
  begin
    Result := lstParamsLine[oParent.SectionIndex] ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Line.DoSmartSizeFieldChange ;
  begin
    raiseCallback( 60 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Line.DoPatternChange ;
  begin
    raiseCallback( 100 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Line.DoOPatternChange ;
  begin
    raiseCallback( 130 ) ;
  end ;
  procedure TGIS_ControlLegendFormMVC_Line.DoControlChange ;
  begin
    raiseCallback( 0 ) ;
  end ;
  {$ENDREGION}

  {$REGION 'TGIS_ControlLegendFormMVC_Area'}
//==============================================================================
// TGIS_ControlLegendFormMVC_Area
//==============================================================================

  constructor TGIS_ControlLegendFormMVC_Area.Create(
    const _parent : TGIS_ControlLegendFormMVC
  ) ;
  begin
    inherited ;
    FHasPreview := True ;
    lstParamsArea := TObjectList<TGIS_ParamsArea>.Create ;
    readAll ;
  end ;

  {$IFDEF DCC}
    destructor TGIS_ControlLegendFormMVC_Area.Destroy ;
    begin
      FreeObject( lstParamsArea ) ;
      inherited ;
    end ;
  {$ENDIF}

  procedure TGIS_ControlLegendFormMVC_Area.readAll ;
  var
    i : Integer ;
  begin
    for i := 0 to oParent.SectionCount - 1 do begin
      oParent.SectionIndex := i ;
      SectionRead( False ) ;
    end ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Area.SectionRead(
    const _blank : Boolean
  ) ;
  var
    sec : TGIS_ParamsSectionVector ;
  begin
    inherited ;

    if _blank then
      sec := TGIS_ParamsSectionVector( oParent.oBlank.Params )
    else
    if assigned( oParent.oSection ) and not oParent.bForceClean then
      sec := TGIS_ParamsSectionVector( oParent.oSection )
    else
      sec := TGIS_ParamsSectionVector( oParent.oBlank.Params ) ;
    lstParamsArea.Add( TGIS_ParamsArea.Create ) ;
    lstParamsArea[oParent.SectionIndex].Assign( sec.Area ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Area.SectionDelete ;
  begin
    inherited ;
    lstParamsArea.Delete( oParent.SectionIndex ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Area.SectionMove(
    const _up : Boolean
  ) ;
  var
    idx : Integer ;
  begin
    inherited ;
    if _up then
      idx := oParent.SectionIndex - 1
    else
      idx := oParent.SectionIndex + 1 ;
    lstParamsArea.Exchange( oParent.SectionIndex, idx ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Area.SectionWrite(
    const _section : TGIS_ParamsSection
  )  ;
  var
    sec : TGIS_ParamsSectionVector ;
  begin
    sec := TGIS_ParamsSectionVector( _section ) ;
    sec.Area.Assign( lstParamsArea[oParent.SectionIndex] ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Area.PreparePreview(
    const _viewer : IGIS_Viewer
  ) ;
  var
    ll  : TGIS_LayerVector ;
    shp : TGIS_Shape       ;
  begin
    _viewer.Close ;
    ll := TGIS_LayerVector.Create ;
    _viewer.Add( ll ) ;
    shp := ll.CreateShape( TGIS_ShapeType.Polygon ) ;
    shp.Lock( TGIS_Lock.Extent ) ;
    shp.AddPart ;
    shp.AddPoint( GisPoint( -2,  0 ) ) ;
    shp.AddPoint( GisPoint(  0,  2 ) ) ;
    shp.AddPoint( GisPoint(  2,  0 ) ) ;
    shp.AddPoint( GisPoint(  0, -2 ) ) ;
    shp.AddPoint( GisPoint( -2, -0 ) ) ;
    shp.Unlock ;
    shp := ll.CreateShape( TGIS_ShapeType.Polygon ) ;
    shp.Lock( TGIS_Lock.Extent ) ;
    shp.AddPart ;
    shp.AddPoint( GisPoint( -3, -3 ) ) ;
    shp.AddPoint( GisPoint( -3,  3 ) ) ;
    shp.AddPoint( GisPoint(  3,  3 ) ) ;
    shp.AddPoint( GisPoint(  3, -3 ) ) ;
    shp.AddPoint( GisPoint( -3, -3 ) ) ;
    shp.Unlock ;
    shp.IsHidden := True ;
    _viewer.FullExtent ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Area.Write ;
  var
    i   : Integer ;
  begin
    inherited ;
    for i := 0 to oParent.SectionCount - 1 do begin
      oParent.SectionIndex := i ;
      SectionWrite( oParent.oSection ) ;
    end ;
  end ;

  function TGIS_ControlLegendFormMVC_Area.fget_ParamsArea : TGIS_ParamsArea ;
  begin
    Result := lstParamsArea[oParent.SectionIndex] ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Area.DoSmartSizeFieldChange ;
  begin
    raiseCallback( 60 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Area.DoPatternChange ;
  begin
    raiseCallback( 100 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Area.DoOPatternChange ;
  begin
    raiseCallback( 130 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Area.DoControlChange ;
  begin
    raiseCallback( 0 ) ;
  end ;
  {$ENDREGION}

  {$REGION 'TGIS_ControlLegendFormMVC_Label'}
//==============================================================================
// TGIS_ControlLegendFormMVC_Label
//==============================================================================

  constructor TGIS_ControlLegendFormMVC_Label.Create(
    const _parent : TGIS_ControlLegendFormMVC
  ) ;
  begin
    inherited ;
    FHasPreview := True ;
    lstParamsLabel := TObjectList<TGIS_ParamsLabel>.Create ;
    readAll ;
  end ;

  {$IFDEF DCC}
    destructor TGIS_ControlLegendFormMVC_Label.Destroy ;
    begin
      FreeObject( lstParamsLabel ) ;
      inherited ;
    end ;
  {$ENDIF}

  procedure TGIS_ControlLegendFormMVC_Label.readAll ;
  var
    i : Integer ;
  begin
    for i := 0 to oParent.SectionCount - 1 do begin
      oParent.SectionIndex := i ;
      SectionRead( False ) ;
    end ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Label.SectionRead(
    const _blank : Boolean
  ) ;
  var
    sec : TGIS_ParamsSectionVector ;
  begin
    inherited ;

    if _blank then
      sec := TGIS_ParamsSectionVector( oParent.oBlank.Params )
    else
    if assigned( oParent.oSection ) and not oParent.bForceClean then
      sec := TGIS_ParamsSectionVector( oParent.oSection )
    else
      sec := TGIS_ParamsSectionVector( oParent.oBlank.Params ) ;
    lstParamsLabel.Add( TGIS_ParamsLabel.Create ) ;
    lstParamsLabel[oParent.SectionIndex].Assign( sec.Labels ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Label.SectionDelete ;
  begin
    inherited ;
    lstParamsLabel.Delete( oParent.SectionIndex ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Label.SectionMove(
    const _up : Boolean
  ) ;
  var
    idx : Integer ;
  begin
    inherited ;
    if _up then
      idx := oParent.SectionIndex - 1
    else
      idx := oParent.SectionIndex + 1 ;
    lstParamsLabel.Exchange( oParent.SectionIndex, idx ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Label.SectionWrite(
    const _section : TGIS_ParamsSection
  )  ;
  var
    sec : TGIS_ParamsSectionVector ;
  begin
    sec := TGIS_ParamsSectionVector( _section ) ;
    sec.Labels.Assign( lstParamsLabel[oParent.SectionIndex] ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Label.Write ;
  var
    i   : Integer ;
  begin
    inherited ;
    for i := 0 to oParent.SectionCount - 1 do begin
      oParent.SectionIndex := i ;
      SectionWrite( oParent.oSection ) ;
    end ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Label.PreparePreview(
    const _viewer : IGIS_Viewer
  ) ;
  var
    ll  : TGIS_LayerVector ;
    shp : TGIS_Shape       ;
  begin
    _viewer.Close ;
    ll := TGIS_LayerVector.Create ;
    _viewer.Add( ll ) ;
    shp := ll.CreateShape( TGIS_ShapeType.Point ) ;
    shp.Lock( TGIS_Lock.Extent ) ;
    shp.AddPart ;
    shp.AddPoint( GisPoint( 0, 0 ) ) ;
    shp.Unlock ;
    shp := ll.CreateShape( TGIS_ShapeType.Polygon ) ;
    shp.Lock( TGIS_Lock.Extent ) ;
    shp.AddPart ;
    shp.AddPoint( GisPoint( -3, -3 ) ) ;
    shp.AddPoint( GisPoint( -3,  3 ) ) ;
    shp.AddPoint( GisPoint(  3,  3 ) ) ;
    shp.AddPoint( GisPoint(  3, -3 ) ) ;
    shp.AddPoint( GisPoint( -3, -3 ) ) ;
    shp.Unlock ;
    shp.IsHidden := True ;
    _viewer.FullExtent ;
  end ;

  function TGIS_ControlLegendFormMVC_Label.fget_ParamsLabel : TGIS_ParamsLabel ;
  begin
    Result := lstParamsLabel[oParent.SectionIndex] ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Label.DoFieldChange ;
  begin
    raiseCallback( 10 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Label.DoSmartSizeFieldChange ;
  begin
    raiseCallback( 70 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Label.DoPositionExNotify ;
  begin
    raiseCallback( 80 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Label.DoAlignmentChange ;
  begin
    raiseCallback( 82 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Label.DoUpdateBitmap ;
  begin
    raiseCallback( 100 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Label.DoUpdateOBitmap ;
  begin
    raiseCallback( 120 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Label.DoControlChange ;
  begin
    raiseCallback( 0 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Label.DoOPatternChange ;
  begin
    raiseCallback( 150 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Label.DoPatternChange ;
  begin
    raiseCallback( 160 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Label.DoShadowChange ;
  begin
    raiseCallback( 170 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Label.DoShieldChange ;
  begin
    raiseCallback( 180 ) ;
  end ;

  {$ENDREGION}

  {$REGION 'TGIS_ControlLegendFormMVC_Chart'}
//==============================================================================
// TGIS_ControlLegendFormMVC_Chart
//==============================================================================

  constructor TGIS_ControlLegendFormMVC_Chart.Create(
    const _parent : TGIS_ControlLegendFormMVC
  ) ;
  begin
    inherited ;
    FHasPreview := True ;
    lstParamsChart := TObjectList<TGIS_ParamsChart>.Create ;
    readAll ;
  end ;

  {$IFDEF DCC}
    destructor TGIS_ControlLegendFormMVC_Chart.Destroy ;
    begin
      FreeObject( lstParamsChart ) ;
      inherited ;
    end ;
  {$ENDIF}

  procedure TGIS_ControlLegendFormMVC_Chart.readAll ;
  var
    i : Integer ;
  begin
    for i := 0 to oParent.SectionCount - 1 do begin
      oParent.SectionIndex := i ;
      SectionRead( False ) ;
    end ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.SectionRead(
    const _blank : Boolean
  ) ;
  var
    sec : TGIS_ParamsSectionVector ;
  begin
    inherited ;

    if _blank then
      sec := TGIS_ParamsSectionVector( oParent.oBlank.Params )
    else
    if assigned( oParent.oSection ) and not oParent.bForceClean then
      sec := TGIS_ParamsSectionVector( oParent.oSection )
    else
      sec := TGIS_ParamsSectionVector( oParent.oBlank.Params ) ;
    lstParamsChart.Add( TGIS_ParamsChart.Create ) ;
    lstParamsChart[oParent.SectionIndex].Assign( sec.Chart ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.SectionDelete ;
  begin
    inherited ;
    lstParamsChart.Delete( oParent.SectionIndex ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.SectionMove(
    const _up : Boolean
  ) ;
  var
    idx : Integer ;
  begin
    inherited ;
    if _up then
      idx := oParent.SectionIndex - 1
    else
      idx := oParent.SectionIndex + 1 ;
    lstParamsChart.Exchange( oParent.SectionIndex, idx ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.SectionWrite(
    const _section : TGIS_ParamsSection
  )  ;
  var
    sec : TGIS_ParamsSectionVector ;
  begin
    sec := TGIS_ParamsSectionVector( _section ) ;
    sec.Chart.Assign( lstParamsChart[oParent.SectionIndex] ) ;
    oParent.Renderer.SectionWrite( _section );
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.Write ;
  var
    i   : Integer ;
  begin
    inherited ;
    for i := 0 to oParent.SectionCount - 1 do begin
      oParent.SectionIndex := i ;
      SectionWrite( oParent.oSection ) ;
    end ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.PreparePreview(
    const _viewer : IGIS_Viewer
  ) ;
  var
    ll  : TGIS_LayerVector ;
    shp : TGIS_Shape       ;
  begin
    _viewer.Close ;
    ll := TGIS_LayerVector.Create ;
    _viewer.Add( ll ) ;
    shp := ll.CreateShape( TGIS_ShapeType.Point ) ;
    shp.Lock( TGIS_Lock.Extent ) ;
    shp.AddPart ;
    shp.AddPoint( GisPoint( 0, 0 ) ) ;
    shp.Unlock ;
    shp := ll.CreateShape( TGIS_ShapeType.Polygon ) ;
    shp.Lock( TGIS_Lock.Extent ) ;
    shp.AddPart ;
    shp.AddPoint( GisPoint( -3, -3 ) ) ;
    shp.AddPoint( GisPoint( -3,  3 ) ) ;
    shp.AddPoint( GisPoint(  3,  3 ) ) ;
    shp.AddPoint( GisPoint(  3, -3 ) ) ;
    shp.AddPoint( GisPoint( -3, -3 ) ) ;
    shp.Unlock ;
    shp.IsHidden := True ;
    _viewer.FullExtent ;
  end ;

  function TGIS_ControlLegendFormMVC_Chart.fget_ParamsChart : TGIS_ParamsChart ;
  begin
    Result := lstParamsChart[oParent.SectionIndex] ;
  end ;

  function TGIS_ControlLegendFormMVC_Chart.fget_RenderChart : String ;
  begin
    Result := oParent.Renderer.ParamsRender.Chart ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.fset_RenderChart(
    const _value: String);
  begin
    oParent.Renderer.ParamsRender.Chart := _value ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.DoStyleChange ;
  begin
    raiseCallback( 1 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.DoSizeUseRenderer ;
  begin
    raiseCallback( 2 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.DoValue1Change ;
  begin
    raiseCallback( 10 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.DoLegend1Change ;
  begin
    raiseCallback( 11 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.DoValue2Change ;
  begin
    raiseCallback( 20 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.DoLegend2Change ;
  begin
    raiseCallback( 21 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.DoValue3Change ;
  begin
    raiseCallback( 30 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.DoLegend3Change ;
  begin
    raiseCallback( 31 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.DoValue4Change ;
  begin
    raiseCallback( 40 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.DoLegend4Change ;
  begin
    raiseCallback( 41 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.DoValue5Change ;
  begin
    raiseCallback( 50 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.DoLegend5Change ;
  begin
    raiseCallback( 51 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.DoValue6Change ;
  begin
    raiseCallback( 60 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.DoLegend6Change ;
  begin
    raiseCallback( 61 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.DoValue7Change ;
  begin
    raiseCallback( 70 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.DoLegend7Change ;
  begin
    raiseCallback( 71 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.DoValue8Change ;
  begin
    raiseCallback( 80 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.DoLegend8Change ;
  begin
    raiseCallback( 81 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Chart.DoControlChange ;
  begin
    raiseCallback( 0 ) ;
  end ;
  {$ENDREGION}

  {$REGION 'TGIS_ControlLegendFormMVC_Pixel'}
//==============================================================================
// TGIS_ControlLegendFormMVC_Pixel
//==============================================================================

  constructor TGIS_ControlLegendFormMVC_Pixel.Create(
    const _parent : TGIS_ControlLegendFormMVC
  ) ;
  begin
    inherited ;
    FHasPreview := True ;
    lstParamsPixel := TObjectList<TGIS_ParamsPixel>.Create ;
    readAll ;
  end ;

  {$IFDEF DCC}
  destructor TGIS_ControlLegendFormMVC_Pixel.Destroy ;
  begin
    FreeObject( lstParamsPixel ) ;
      inherited ;
    end ;
  {$ENDIF}

  procedure TGIS_ControlLegendFormMVC_Pixel.readAll ;
  var
    i : Integer ;
  begin
    for i := 0 to oParent.SectionCount - 1 do begin
      oParent.SectionIndex := i ;
      SectionRead( False ) ;
    end ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Pixel.SectionRead(
    const _blank : Boolean
  ) ;
  var
    sec : TGIS_ParamsSectionPixel ;
  begin
    inherited ;

    if _blank then
      sec := TGIS_ParamsSectionPixel( oParent.oBlank.Params )
    else
    if assigned( oParent.oSection ) and not oParent.bForceClean then
      sec := TGIS_ParamsSectionPixel( oParent.oSection )
    else
      sec := TGIS_ParamsSectionPixel( oParent.oBlank.Params ) ;
    lstParamsPixel.Add( TGIS_ParamsPixel.Create ) ;
    lstParamsPixel[oParent.SectionIndex].Assign( sec.Pixel ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Pixel.SectionDelete ;
  begin
    inherited ;
    lstParamsPixel.Delete( oParent.SectionIndex ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Pixel.SectionMove(
    const _up : Boolean
  ) ;
  var
    idx : Integer ;
  begin
    inherited ;
    if _up then
      idx := oParent.SectionIndex - 1
    else
      idx := oParent.SectionIndex + 1 ;
    lstParamsPixel.Exchange( oParent.SectionIndex, idx ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Pixel.SectionWrite(
    const _section : TGIS_ParamsSection
  )  ;
  var
    sec : TGIS_ParamsSectionPixel ;
  begin
    sec := TGIS_ParamsSectionPixel( _section ) ;
    sec.Pixel.Assign( lstParamsPixel[oParent.SectionIndex] ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Pixel.Write ;
  var
    i   : Integer ;
  begin
    inherited ;
    for i := 0 to oParent.SectionCount - 1 do begin
      oParent.SectionIndex := i ;
      SectionWrite( oParent.oSection ) ;
    end ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Pixel.PreparePreview(
    const _viewer : IGIS_Viewer
  ) ;
  var
    lp     : TGIS_LayerPixel  ;
    ls     : TGIS_LayerPixel  ;
    ipage  : Integer ;  // pages
    bantl  : Boolean ;  // antialiasing
    a, r, g,
    b, gr  : Integer ;
    sf     : TGIS_LayerPixelSubFormat ;
  begin
    _viewer.Close ;
    ls := TGIS_LayerPixel(oParent.oLayer) ;
    lp := TGIS_LayerPixel.Create ;

    // force specific page
    ipage := ls.Params.Pixel.Page ;
    ls.Params.Pixel.Page := ParamsPixel.Page ;
    a  := ls.Params.Pixel.AlphaBand ;
    r  := ls.Params.Pixel.RedBand ;
    g  := ls.Params.Pixel.GreenBand ;
    b  := ls.Params.Pixel.BlueBand ;
    gr := ls.Params.Pixel.GridBand ;

    ls.Params.Pixel.AlphaBand := ParamsPixel.AlphaBand ;
    ls.Params.Pixel.RedBand   := ParamsPixel.RedBand ;
    ls.Params.Pixel.GreenBand := ParamsPixel.GreenBand ;
    ls.Params.Pixel.BlueBand  := ParamsPixel.BlueBand ;
    ls.Params.Pixel.GridBand  := ParamsPixel.GridBand ;

    bantl := ls.Antialias ;
    if ls.IsGridImage or ls.IsTiled then
      ls.ApplyAntialiasSettings( ParamsPixel.Antialias ) ;

    if ls.IsGridImage then
      sf := TGIS_LayerPixelSubFormat.Create(
                      TGIS_PixelFormat.Custom,
                      False,
                      TGIS_PixelSubFormat.GRID,
                      TGIS_CompressionType.None,
                      0
                    )
    else
      sf := _TGIS_LayerPixelSubFormat(ls.DefaultSubFormat) ;

    lp.ImportLayer( ls,
                    GisCommonExtent( ls.Viewer.Ref.VisibleExtent,
                                     ls.ProjectedExtent
                                   ),
                    ls.CS,
                    _viewer.ViewerParent.ControlCanvasWidth, 0,
                    sf
                  ) ;
    ls.Params.Pixel.AlphaBand := a ;
    ls.Params.Pixel.RedBand   := r ;
    ls.Params.Pixel.GreenBand := g ;
    ls.Params.Pixel.BlueBand  := b ;
    ls.Params.Pixel.GridBand  := gr ;

    _viewer.Lock ;
    try
      _viewer.Add( lp ) ;
      _viewer.FullExtent ;

      // restore parameters
      ls.Params.Pixel.Page := ipage ;
      ls.ApplyAntialiasSettings( bantl ) ;

      _viewer.VisibleExtent := GisCommonExtent(
                                 lp.Viewer.Ref.VisibleExtent,
                                 lp.ProjectedExtent
                               ) ;
    finally
      _viewer.Unlock ;
    end;
  end ;

  function TGIS_ControlLegendFormMVC_Pixel.fget_MaxZ: Single ;
  begin
    if oParent.oLayer is TGIS_LayerPixel then
      Result := TGIS_LayerPixel(oParent.oLayer).MaxHeight
    else
      Result := GIS_MAX_SINGLE ;
  end ;

  function TGIS_ControlLegendFormMVC_Pixel.fget_MinZ: Single ;
  begin
    if oParent.oLayer is TGIS_LayerPixel then
      Result := TGIS_LayerPixel(oParent.oLayer).MinHeight
    else
      Result := -GIS_MAX_SINGLE ;
  end ;

  function TGIS_ControlLegendFormMVC_Pixel.fget_ParamsPixel : TGIS_ParamsPixel ;
  begin
    Result := lstParamsPixel[oParent.SectionIndex] ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Pixel.DoReset ;
  begin
    raiseCallback( 1 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Pixel.DoControlChange ;
  begin
    raiseCallback( 0 ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC_Pixel.DoTransparentZonesChange ;
  begin
    raiseCallback( 2 ) ;
  end ;
  {$ENDREGION}

  {$REGION 'TGIS_ControlLegendFormMVC'}
//==============================================================================
// TGIS_ControlLegendFormMVC
//==============================================================================

  constructor TGIS_ControlLegendFormMVC.Create(
    const _layer : TGIS_Layer
  ) ;
  var
    ss : TGIS_ShapeTypes ;
  begin
    inherited Create ;
    oLayer := _layer ;
    iSectionCount := oLayer.ParamsList.Count ;
    SectionIndex := 0 ;
    bForceClean := False ;
    FIsVector := oLayer is TGIS_LayerVector ;
    FIsPixel  := oLayer is TGIS_LayerPixel ;
    if FIsPixel then
      FIsGrid := TGIS_LayerPixel( oLayer ).IsGridImage
    else
      FIsGrid := False ;
    if IsVector then
      oBlank := TGIS_LayerVector.Create
    else if FIsPixel then
      oBlank := TGIS_LayerPixel.Create
    else
      oBlank := TGIS_LayerVector.Create ;
    FHasMarker := False ;
    FHasLine   := False ;
    FHasArea   := False ;
    if IsVector then begin
      ss := TGIS_LayerVector( oLayer ).SupportedShapesAll ;
      if ( TGIS_ShapeType.Point      in ss ) or
         ( TGIS_ShapeType.MultiPoint in ss ) then
        FHasMarker := True ;
      if ( TGIS_ShapeType.Arc        in ss ) then
        FHasLine   := True ;
      if ( TGIS_ShapeType.Polygon    in ss ) then
        FHasArea   := True ;
      if ( TGIS_ShapeType.MultiPatch in ss ) then
        FHasArea   := True ;
      if ( TGIS_ShapeType.Complex in ss ) then begin
        FHasMarker := True ;
        FHasLine   := True ;
        FHasArea   := True ;
      end ;
    end ;
    FGeneral  := TGIS_ControlLegendFormMVC_General.Create( Self ) ;
    F3D       := TGIS_ControlLegendFormMVC_3D.Create( Self ) ;
    FSection  := TGIS_ControlLegendFormMVC_Section.Create( Self ) ;
    if IsVector then
      FRenderer := TGIS_ControlLegendFormMVC_Renderer.Create( Self ) ;
    if HasMarker then
      FMarker   := TGIS_ControlLegendFormMVC_Marker.Create( Self ) ;
    if HasLine then
      FLine     := TGIS_ControlLegendFormMVC_Line.Create( Self ) ;
    if HasArea then
      FArea     := TGIS_ControlLegendFormMVC_Area.Create( Self ) ;
    if IsVector then
      FLabel    := TGIS_ControlLegendFormMVC_Label.Create( Self ) ;
    if IsVector then
      FChart    := TGIS_ControlLegendFormMVC_Chart.Create( Self ) ;
    if IsGrid or FIsPixel then
      FPixel := TGIS_ControlLegendFormMVC_Pixel.Create( Self ) ;
    if IsVector then
      prepareFieldNames ;
  end ;

  {$IFDEF DCC}
    destructor TGIS_ControlLegendFormMVC.Destroy ;
    begin
      FreeObject( FGeneral ) ;
      FreeObject( F3D ) ;
      FreeObject( FSection ) ;
      FreeObject( FRenderer ) ;
      FreeObject( FMarker ) ;
      FreeObject( FLine ) ;
      FreeObject( FArea ) ;
      FreeObject( FLabel ) ;
      FreeObject( FChart ) ;
      FreeObject( FPixel ) ;
      FreeObject( oFieldNames ) ;
      FreeObject( oFieldNamesEx ) ;
      FreeObject( oBlank ) ;
      inherited ;
    end ;
  {$ENDIF}

  procedure TGIS_ControlLegendFormMVC.prepareFieldNames ;
  var
    lv      : TGIS_LayerVector ;
    i       : Integer ;
    wasjoin : Boolean     ;
    {$IFDEF CLR}
      dt    : System.Data.DataTable    ;
    {$ENDIF}
  begin
    lv := TGIS_LayerVector( oLayer ) ;
    oFieldNames := TGIS_StringList.Create ;
    oFieldNamesEx := TGIS_StringList.Create ;
    oFieldNames.Text := GIS_FIELDS_PREDEFINED  ;
    for i := 0 to lv.Fields.Count - 1 do begin
      if lv.FieldInfo( i ).Hidden or lv.FieldInfo( i ).Deleted then
        continue ;
      oFieldNames.Add( TGIS_Utils.GisNormalizedSQLName( lv.FieldInfo( i ).NewName ) ) ;
      oFieldNamesEx.Add( TGIS_Utils.GisNormalizedSQLName( lv.FieldInfo( i ).NewName ) ) ;
    end ;

    wasjoin := False ;

    {$IFNDEF GIS_NOADO_JOIN}
     if ( not wasjoin ) and Assigned( lv.JoinADO ) then begin
       wasjoin := True ;
       if Assigned( lv.JoinAdo ) then begin
         for i := 0 to lv.JoinADO.Fields.Count-1 do
           if JoinFieldInfo( lv.JoinADO.Fields[i] ) then begin
             oFieldNames.Add( ToJoinFieldName( lv.JoinADO.Fields.Item[i].Name ) ) ;
             oFieldNamesEx.Add( ToJoinFieldName( lv.JoinADO.Fields.Item[i].Name ) ) ;
           end;
       end ;
     end ;
    {$ENDIF}

    {$IFDEF CLR}
      dt := nil ; // to avoid hints
      {$IFNDEF GIS_NOADO}
        if ( not wasjoin ) and assigned( lv.JoinNET ) then begin
          wasjoin := True ;
          dt := lv.JoinNET as System.Data.DataTable ;
          for i := 0 to dt.Columns.Count -1 do
            if JoinFieldInfo( dt.Columns[i].DataType ) then begin
              oFieldNames.Add( ToJoinFieldName( dt.Columns[ i ].ColumnName ) ) ;
              oFieldNamesEx.Add( ToJoinFieldName( dt.Columns[ i ].ColumnName ) ) ;
            end;
        end ;
      {$ENDIF}
    {$ENDIF}

    {$IFDEF JAVA}
      {$IFNDEF GIS_NOJDBC}
      if ( not wasjoin ) and assigned( lv.JoinJDBC ) then begin
        wasjoin := True ;
        for i := 1 to java.sql.ResultSet(lv.JoinJDBC).getMetaData.ColumnCount do begin
          if JoinFieldInfo( java.sql.ResultSet(lv.JoinJDBC).getMetaData.ColumnType[i] ) then begin
            oFieldNames.Add( ToJoinFieldName( java.sql.ResultSet(lv.JoinJDBC).getMetaData.ColumnName[i] ) ) ;
            oFieldNamesEx.Add( ToJoinFieldName( java.sql.ResultSet(lv.JoinJDBC).getMetaData.ColumnName[i] ) ) ;
          end ;
        end ;
      end ;
      {$ENDIF}
    {$ENDIF}

    {$IFNDEF GIS_NODB}
     if ( not wasjoin ) and Assigned( lv.JoinDB ) then begin
       //wasjoin := True ;
       for i := 0 to lv.JoinDB.FieldCount-1 do
         if JoinFieldInfo( lv.JoinDB.Fields[i] ) then begin
           oFieldNames.Add( ToJoinFieldName( lv.JoinDB.Fields[i].DisplayName ) ) ;
           oFieldNamesEx.Add( ToJoinFieldName( lv.JoinDB.Fields[i].DisplayName ) ) ;
         end;
     end ;
    {$ENDIF}
  end ;

  function TGIS_ControlLegendFormMVC.fget_SectionCount : Integer ;
  begin
    Result := iSectionCount ;
  end ;

  function TGIS_ControlLegendFormMVC.fget_Index : Integer ;
  begin
    Result := FIndex ;
  end ;

  procedure TGIS_ControlLegendFormMVC.fset_Index(
    const _i : Integer
  ) ;
  begin
    if ( _i < 0 ) or ( _i >= SectionCount ) then
      FIndex := 0
    else
      FIndex := _i ;
    if _i > oLayer.ParamsList.Count - 1 then
      oSection := nil
    else
      oSection := oLayer.ParamsList.Items[SectionIndex] ;
  end ;

  function TGIS_ControlLegendFormMVC.fget_FieldNames : TGIS_StringList ;
  begin
    if IsVector then
      Result := oFieldNames
    else
      Result := nil ;
  end ;

  function TGIS_ControlLegendFormMVC.fget_FieldNamesEx : TGIS_StringList ;
  begin
    if IsVector then
      Result := oFieldNamesEx
    else
      Result := nil ;
  end ;

  procedure TGIS_ControlLegendFormMVC.SectionAdd(
    const _blank : Boolean
  ) ;
  var
    idx : Integer ;
  begin
    inc( iSectionCount ) ;
    idx := SectionIndex ;
    SectionIndex := SectionCount - 1 ;
    FSection.SectionRead( _blank ) ;
    F3D.SectionRead( _blank )  ;
    if assigned( FRenderer ) then
      FRenderer.SectionRead( _blank )  ;
    if assigned( FMarker ) then
      FMarker.SectionRead( _blank )  ;
    if assigned( FLine ) then
      FLine.SectionRead( _blank )  ;
    if assigned( FArea ) then
      FArea.SectionRead( _blank )  ;
    if assigned( FLabel ) then
      FLabel.SectionRead( _blank )  ;
    if assigned( FChart ) then
      FChart.SectionRead( _blank )  ;
    if assigned( FPixel ) then
      FPixel.SectionRead( _blank )  ;
    SectionIndex := idx ;
  end ;

  procedure TGIS_ControlLegendFormMVC.SectionDelete ;
  begin
    FSection.SectionDelete ;
    F3D.SectionDelete ;
    if assigned( FRenderer ) then
      FRenderer.SectionDelete ;
    if assigned( FMarker ) then
      FMarker.SectionDelete ;
    if assigned( FLine ) then
      FLine.SectionDelete ;
    if assigned( FArea ) then
      FArea.SectionDelete ;
    if assigned( FLabel ) then
      FLabel.SectionDelete ;
    if assigned( FChart ) then
      FChart.SectionDelete ;
    if assigned( FPixel ) then
      FPixel.SectionDelete ;
    dec( iSectionCount ) ;
    if SectionIndex = SectionCount then
      SectionIndex := SectionCount - 1 ;
  end ;

  procedure TGIS_ControlLegendFormMVC.SectionClear ;
  var
    i   : Integer ;
    sec : TGIS_ParamsSection ;
  begin
    SectionIndex := iSectionCount - 1 ;
    for i := iSectionCount - 1 downto 0 do
      SectionDelete ;
    try
      if SectionCount = 0 then begin
        sec := oSection ;
        oSection := nil ;
        bForceClean := True ;
        SectionAdd( True ) ;
        oSection := sec ;
        SectionIndex := 0 ;
      end ;
    finally
      bForceClean := False ;
    end ;
  end ;

  procedure TGIS_ControlLegendFormMVC.SectionMove(
    const _up : Boolean
  ) ;
  begin
    if _up and ( SectionIndex = 0 ) then
      exit ;
    if ( not _up ) and ( SectionIndex = SectionCount - 1 ) then
      exit ;
    FSection.SectionMove( _up ) ;
    F3D.SectionMove( _up ) ;
    if assigned( FRenderer ) then
      FRenderer.SectionMove( _up ) ;
    if assigned( FMarker ) then
      FMarker.SectionMove( _up ) ;
    if assigned( FLine ) then
      FLine.SectionMove( _up ) ;
    if assigned( FArea ) then
      FArea.SectionMove( _up ) ;
    if assigned( FLabel ) then
      FLabel.SectionMove( _up ) ;
    if assigned( FChart ) then
      FChart.SectionMove( _up ) ;
    if assigned( FPixel ) then
      FPixel.SectionMove( _up ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC.Write ;
  var
    cnt : Integer ;
    i   : Integer ;
  begin
    cnt := SectionCount - oLayer.ParamsList.Count ;
    if cnt > 0 then begin
      for i := 1 to cnt do
        oLayer.ParamsList.Add ;
    end
    else
    if cnt < 0 then begin
      cnt := -cnt ;
      for i := 1 to cnt do begin
        oLayer.ParamsList.Selected :=
          oLayer.ParamsList.Count - 1 ;
        oLayer.ParamsList.Delete ;
      end ;
    end ;
    FSection.Write ;
    F3D.Write ;
    if assigned( FRenderer ) then
      FRenderer.Write ;
    if assigned( FMarker ) then
      FMarker.Write ;
    if assigned( FLine ) then
      FLine.Write ;
    if assigned( FArea ) then
      FArea.Write ;
    if assigned( FLabel ) then
      FLabel.Write ;
    if assigned( FChart ) then
      FChart.Write ;
    if assigned( FPixel ) then
      FPixel.Write ;

    FGeneral.Write ;
  end ;

  procedure TGIS_ControlLegendFormMVC.PrepareLayerEx ;
  var
    layerEx_vec : TGIS_LayerVector ;
    layer_vec   : TGIS_LayerVector ;

  begin
    if FIsVector then
      oLayerEx := TGIS_LayerVector.Create
    else
      oLayerEx := TGIS_LayerPixel.Create ;

    oLayerEx.ParamsList.Assign( oLayer.ParamsList ) ;

    oLayerEx.View3D       := oLayer.View3D ;
    oLayerEx.Active       := oLayer.Active ;
    oLayerEx.Caption      := oLayer.Caption ;
    oLayerEx.Comments     := oLayer.Comments ;
    oLayerEx.Basemap      := oLayer.Basemap ;
    oLayerEx.CachedPaint  := oLayer.CachedPaint ;
    oLayerEx.Transparency := oLayer.Transparency ;
    oLayerEx.CodePage     := oLayer.CodePage ;
    oLayerEx.Addition     := oLayer.Addition ;
    oLayerEx.CS           := oLayer.CS ;
    oLayerEx.Name         := oLayer.Name ;
    oLayerEx.Path         := oLayer.Path ;
    oLayerEx.UseConfig    := oLayer.UseConfig ;

    if FIsVector then begin
      layerEx_vec := TGIS_LayerVector( oLayerEx ) ;
      layer_vec := TGIS_LayerVector( oLayer ) ;

      layerEx_vec.MultipassRendering := layer_vec.MultipassRendering ;
      layerEx_vec.IgnoreShapeParams  := layer_vec.IgnoreShapeParams ;
      layerEx_vec.Scope              := layer_vec.Scope ;
      layerEx_vec.ImportStructure( layer_vec ) ;
      if assigned( layer_vec.DynamicAggregator ) then
        layerEx_vec.DynamicAggregator  := layer_vec.DynamicAggregator.Copy(layerEx_vec) ;
    end
    else if FIsPixel or FIsGrid then begin
      TGIS_LayerPixel(oLayerEx).MinHeight := TGIS_LayerPixel(oLayer).MinHeight ;
      TGIS_LayerPixel(oLayerEx).MaxHeight := TGIS_LayerPixel(oLayer).MaxHeight ;
      TGIS_LayerPixel(oLayerEx).Interpretation := TGIS_LayerPixel(oLayer).Interpretation ;
    end;
    oLayerEx.Open ;
  end ;

  procedure TGIS_ControlLegendFormMVC.UnPrepareLayerEx ;
  begin
    FreeObject( oLayerEx ) ;
  end ;

  procedure TGIS_ControlLegendFormMVC.SwitchToLayerEx ;
  begin
    if assigned( oLayerEx ) then begin
      oLayerTemp := oLayer ;
      oLayer := oLayerEx ;
    end ;
  end ;

  procedure TGIS_ControlLegendFormMVC.SwitchToLayer ;
  begin
    if assigned( oLayerTemp ) then begin
      oLayer := oLayerTemp ;
    end ;
    oLayerTemp := nil ;
  end ;

  procedure TGIS_ControlLegendFormMVC.LoadConfig(
    const _path : String
  ) ;
  var
    sld    : TGIS_SldFile ;
    ext    : String ;
  begin
    ext := GetFileExt( _path ) ;

    if ext = GIS_SLD_EXT then begin
      sld := TGIS_SldFile.Create( _path ) ;
      try
        sld.ReadConfig( oLayer ) ;
      finally
        FreeObject( sld ) ;
      end ;
    end
    else begin
      oLayer.ConfigName := _path ;
      oLayer.ParamsList.ClearAndSetDefaults ;
      oLayer.ReadConfig ;
      oLayer.ParamsList.LoadFromFile( _path ) ;
    end ;

  end ;

  procedure TGIS_ControlLegendFormMVC.SaveConfig(
    const _path : String
  ) ;
  begin
    oLayer.ParamsList.SaveToFile( _path ) ;
    oLayer.ConfigName := _path ;
    oLayer.WriteConfig ;
    if assigned( oLayer.ConfigFile ) then
      TGIS_Config(oLayer.ConfigFile).Save ;
  end ;

  {$ENDREGION}
//==================================== END =====================================
end.
