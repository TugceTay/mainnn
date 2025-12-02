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
  Legend control.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLegend ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLegend"'}
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

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    System.Collections.Generic,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
  System.Types,
  System.Classes,
  System.Generics.Collections,

  Lider.CG.GIS.GeoInterfaces,
  Lider.CG.GIS.GeoLayer,
  Lider.CG.GIS.GeoHierarchy,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoLegendUtils,
  Lider.CG.GIS.GeoRendererAbstract ;
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

const
  /// <summary>
  ///   Maximum number of unique values.
  /// </summary>
  DIALOG_OPT_VW_UNIQUE_LIMIT = 256 ;

  /// <summary>
  ///   Maximum number of records searched for unique values.
  /// </summary>
  DIALOG_OPT_VW_UNIQUE_SEARCH_LIMIT = 16384 ;

type

  /// <summary>
  ///   Options for TGIS_Legend.
  /// </summary>
  TGIS_ControlLegendOption = {$IFDEF OXYGENE} public {$IFNDEF JAVA} flags {$ENDIF} {$ENDIF} (

    /// <summary>
    ///   Allows reordering of layers/groups .
    /// </summary>
    AllowMove,

    /// <summary>
    ///   Allows changing of layer/group Active property (with check box).
    /// </summary>
    AllowActive,

    /// <summary>
    ///   Allows expanding/collapsing of layers.
    /// </summary>
    AllowExpand,

    /// <summary>
    ///   Allows changing of layer parameters.
    /// </summary>
    AllowParams,

    /// <summary>
    ///   Allows selection of layers/groups.
    /// </summary>
    AllowSelect,

    /// <summary>
    ///   Makes the legend display hidden layers.
    /// </summary>
    ShowHiddenLayers,

    /// <summary>
    ///   Makes the legend display layer path instead of caption/name.
    /// </summary>
    ShowLayerFullPath,

    /// <summary>
    ///   Makes the legend display sublayers.
    /// </summary>
    ShowSubLayers,

    /// <summary>
    ///   Allows changing of parameter sections Visible property (with check box).
    /// </summary>
    AllowParamsVisible
  ) ;

  /// <summary>
  ///   Set of TGIS_LegendOption.
  /// </summary>
  {$IFDEF OXYGENE}
    {$IFDEF JAVA}
      TGIS_ControlLegendOptions = public set of TGIS_ControlLegendOption ;
    {$ELSE}
      TGIS_ControlLegendOptions = TGIS_ControlLegendOption ;
    {$ENDIF}
  {$ELSE}
    TGIS_ControlLegendOptions = set of TGIS_ControlLegendOption ;
  {$ENDIF}

  /// <summary>
  ///   Modes for TGIS_Legend.
  /// </summary>
  TGIS_ControlLegendMode = {$IFDEF OXYGENE} public {$ENDIF} (

    /// <summary>
    ///   Mode in which the legend displays layers in the drawing order.
    /// </summary>
    Layers,

    /// <summary>
    ///   Mode in which the legend displays grouped layers as a tree view.
    /// </summary>
    Groups
  ) ;

  /// <summary>
  ///   Dialog options.
  /// </summary>
  TGIS_ControlLegendDialogOptions = class( TPersistent )
  private
    FVectorWizardUniqueLimit       : Integer ;
    FVectorWizardUniqueSearchLimit : Integer ;
  public
    /// <summary>
    ///   Constructor.
    /// </summary>
    constructor Create ; overload;

    /// <summary>
    ///   Constructor.
    /// </summary>
    /// <param name="_uniqueLimit">
    ///   maximum number of unique values.
    /// </param>
    /// <param name="_uniqueSearchLimit">
    ///   maximum number of records searched for unique values.
    /// </param>
    constructor Create ( const _uniqueLimit        : Integer ;
                         const _uniqueSearchLimit  : Integer
                       ) ; overload;

  published

    /// <summary>
    ///   Vector Wizard maximum number of unique values.
    /// </summary>
    property VectorWizardUniqueLimit       : Integer read  FVectorWizardUniqueLimit
                                                     write FVectorWizardUniqueLimit
                                             {$IFDEF OXYGENE}
                                               ;
                                             {$ELSE}
                                               default DIALOG_OPT_VW_UNIQUE_LIMIT ;
                                             {$ENDIF}

    /// <summary>
    ///   Vector Wizard maximum number of records searched for unique values.
    /// </summary>
    property VectorWizardUniqueSearchLimit : Integer read  FVectorWizardUniqueSearchLimit
                                                     write FVectorWizardUniqueSearchLimit
                                             {$IFDEF OXYGENE}
                                               ;
                                             {$ELSE}
                                               default DIALOG_OPT_VW_UNIQUE_SEARCH_LIMIT ;
                                             {$ENDIF}
  end ;

  {#gendoc:hide}
  TGIS_TreeNode = class ;

  {#gendoc:hide}
  TGIS_TreeList = class( TList<TGIS_TreeNode> )
    private
      lSelected : TList<TGIS_TreeNode> ;
    private
      function  fget_Selected : TGIS_TreeNode ;
      procedure fset_Selected ( const _val : TGIS_TreeNode
                              ) ;
    private
      procedure deselectAll ;
    public
      constructor Create ( const _slctd : TList<TGIS_TreeNode>
                         ) ;
    public
      destructor  Destroy ; override ;
    public
      function  AddChild    ( const _node : TGIS_TreeNode ;
                              const _name : String
                            ) : TGIS_TreeNode ;
      procedure InsertChild ( const _idx  : Integer ;
                              const _node : TGIS_TreeNode
                            ) ;
      procedure AppendChild ( const _node : TGIS_TreeNode
                            ) ;
      procedure BeginUpdate ;
      procedure EndUpdate   ;
    public
      property Selected : TGIS_TreeNode
                          read  fget_Selected
                          write fset_Selected ;
  end ;

  {#gendoc:hide}
  TGIS_TreeNode = class
    private
      lSiblings : TGIS_TreeList ;
      lSelected : TList<TGIS_TreeNode> ;
    private
      FParent   : TGIS_TreeNode ;
      FItems    : TGIS_TreeList ;
      FSelected : Boolean ;
      FExpanded : Boolean ;
      FText     : String ;
      FData     : TGIS_LegendItemData ;
    private
      function  fget_Items    ( const _idx : Integer
                              ) : TGIS_TreeNode ;
      procedure fset_Items    ( const _idx : Integer ;
                                const _val : TGIS_TreeNode
                              ) ;
      function  fget_Count    : Integer ;
      function  fget_Index    : Integer ;
      function  fget_Level    : Integer ;
      function  fget_Selected : Boolean ;
      procedure fset_Selected ( const _val : Boolean
                              ) ;
    public
      constructor Create ;
    public
      destructor  Destroy ; override ;
    public
      function  AddNode        : TGIS_TreeNode ;
      procedure AppendNode     ( const _node : TGIS_TreeNode ;
                                 const _pos  : Integer
                               ) ;
      procedure Remove         ( const _node : TGIS_TreeNode
                               ) ;
      function  GetPrevSibling : TGIS_TreeNode ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
      function  GetNextSibling : TGIS_TreeNode ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
      procedure Expand         ( const _deep : Boolean
                               ) ;
      function  FindSelected   : TGIS_TreeNode ;
    public
      property Parent   : TGIS_TreeNode
                          read  FParent ;
      property Items    [const _idx: Integer]
                        : TGIS_TreeNode
                          read  fget_Items
                          write fset_Items ;
      property Count    : Integer
                          read  fget_Count ;
      property Index    : Integer
                          read  fget_Index ;
      property Level    : Integer
                          read  fget_Level ;
      property Selected : Boolean
                          read  fget_Selected
                          write fset_Selected ;
      property Expanded : Boolean
                          read  FExpanded
                          write FExpanded ;
      property Text     : String
                          read  FText
                          write FText ;
      property Data     : TGIS_LegendItemData
                          read  FData
                          write FData ;
  end ;

  {$IFDEF OXYGENE}
    /// <summary>
    ///   TGIS_ControlLegendOptions type helper.
    /// </summary>
    TGIS_ControlLegendOptionsHelper = {$IFDEF OXYGENE} public {$ENDIF} class
      public
        /// <summary>
        ///   Make a useable set from enums so it can be pass to properties properly.
        /// </summary>
        /// <param name="_options">
        ///   Array of enums
        /// </param>
        /// <returns>
        ///   Return sum of enums integer values
        /// </returns>
        {$IFDEF CLR OR ISLAND}
          class function  SetLegendOptions  ( const _options : List<Integer>
                                            ) : TGIS_ControlLegendOption ;
        {$ENDIF}
        {$IFDEF JAVA}
          class function  SetLegendOptions  ( const _options : java.util.List<Integer>
                                            ) : Integer ;
        {$ENDIF}

        /// <summary>
        ///   Get an array of integer values of TGIS_LegendOption enum
        /// </summary>
        /// <param name="_options">
        ///   Options from TGIS_ControlLegend.Options
        /// </param>
        /// <returns>
        ///   List of TGIS_ControlLegendOption integer values
        /// </returns>
        {$IFDEF CLR OR ISLAND}
          class function  GetLegendOptions  ( const _options: Integer
                                            ) : List<Integer>;
        {$ENDIF}
        {$IFDEF JAVA}
          class function  GetLegendOptions  ( const _options: Integer
                                            ) : java.util.List<TGIS_ControlLegendOption>;
        {$ENDIF}
    end;
  {$ENDIF}

  {#gendoc:hide:GENSCR}
  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  /// <summary>
  ///   Legend class.
  /// </summary>
  TGIS_Legend = class ( TGIS_UncountedInterfacedObject,
                        IGIS_Subscribe )
    private
      FViewer           : IGIS_Viewer ;
      FItems            : TGIS_TreeList ;
      FMode             : TGIS_ControlLegendMode ;
      FOptions          : TGIS_ControlLegendOptions ;
      FDialogOptions    : TGIS_ControlLegendDialogOptions ;
      FReverseOrder     : Boolean ;
      FCompactView      : Boolean ;
      FDrawIconStyle    : TGIS_LegendIconStyle ;
      FInternalName     : String  ;
      FOldStyleDrag     : Boolean ;
      FDirectTouchLongTap
                        : Boolean ;

    private
      FDefHeight        : Integer ;
      FDefHeight16      : Integer ;
      FDefBmpSize       : Integer ;
      FFontSize         : Single  ;
      FTopOffset        : Integer ;
      FLength           : Integer ;
      FScaleFactor      : Double  ;
      FLastNode         : TGIS_TreeNode ;
      FPPI              : Integer ;
      FExpandRectSize   : Integer ;
      FCheckBoxRectSize : Integer ;
      FLevelIndent      : Integer ;
      FValHScroll       : Integer ;
      FDifHScroll       : Integer ;
      FMaxHScroll       : Integer ;

      FNoDataInfo       : Boolean ;
      FNoParams         : Boolean ;
      FBasemap          : Boolean ;

    private
      FLayerParamsChangeEvent : TGIS_LayerEvent ;
      FLayerActiveChangeEvent : TGIS_LayerEvent ;
      FLayerSelectEvent       : TGIS_LayerEvent ;
      FGroupActiveChangeEvent : TGIS_HierarchyGroupEvent ;
      FGroupSelectEvent       : TGIS_HierarchyGroupEvent ;
      FOrderChangeEvent       : {$IFDEF CLR}
                                  EventHandler ;
                                {$ELSE}
                                  TNotifyEvent ;
                                {$ENDIF}

    private
      oParent          : IGIS_LegendParent ;
      oSelected        : TList<TGIS_TreeNode> ;
      sNewSelected     : String ;
      oBMPFactory      : TGIS_LegendIconFactory ;
      lstOldLayerNodes : TDictionary< String, TGIS_TreeNode >;
      lstOldGroupNodes : TDictionary< String, TGIS_TreeNode >;
      moveLevel        : Integer ;
      moveTop          : Integer ;
      fOldZoom         : Double  ;
      ptDrag           : TPoint  ;
      ptTouch          : TPoint  ;
      isMouseDn        : Boolean ;
      isTouch          : Boolean ;
      canDrag          : Boolean ;
      isLongTap        : Boolean ;
      bTapLeft         : Boolean ;
      dragNode         : TGIS_TreeNode ;
      bDragBar         : Boolean ;
      bGroupAboveGroup : Boolean ;
      skipSelect       : Boolean ;
      skipUpdate       : Boolean ;
      selectedFlag     : Boolean ;
      canExpand        : Boolean ;
      lastHash         : Int64   ;
      isDrag           : Boolean ;
      isMove           : Boolean ;
      iLocked          : Integer ;

    private
      opAllowMove          : Boolean ;
      opAllowActive        : Boolean ;
      opAllowExpand        : Boolean ;
      opAllowParams        : Boolean ;
      opAllowSelect        : Boolean ;
      opShowHiddenLayers   : Boolean ;
      opShowLayerFullPath  : Boolean ;
      opShowSubLayers      : Boolean ;
      opAllowParamsVisible : Boolean ;

    private
      procedure fset_Viewer        ( const _value : IGIS_Viewer
                                   ) ;
      function  fget_Layer         : TGIS_Layer ;
      procedure fset_Layer         ( const _value : TGIS_Layer
                                   ) ;
      function  fget_Group         : IGIS_HierarchyGroup ;
      procedure fset_Group         ( const _value : IGIS_HierarchyGroup
                                   ) ;
      procedure fset_Mode          ( const _value : TGIS_ControlLegendMode
                                   ) ;
      procedure fset_Options       ( const _value : TGIS_ControlLegendOptions
                                   ) ;
      procedure fset_ReverseOrder  ( const _value : Boolean
                                   ) ;
      procedure fset_DialogOptions ( const _value : TGIS_ControlLegendDialogOptions
                                   ) ;
      function  fget_Renderer      : TGIS_RendererAbstract ;
      procedure fset_Renderer      ( const _value : TGIS_RendererAbstract
                                   ) ;
      function  fget_SelectedNode  : TGIS_TreeNode ;
      procedure fset_SelectedNode  ( const _value : TGIS_TreeNode
                                   ) ;
      procedure fset_CompactView   ( const _value : Boolean
                                   ) ;
      procedure fset_DrawIconStyle ( const _value : TGIS_LegendIconStyle
                                   ) ;
      procedure fset_PPI           ( const _value : Integer
                                   ) ;
      procedure fset_TopOffset     ( const _value : Integer
                                   ) ;

    private

      /// <summary>
      ///   Scales a size in pixels according to current PPI.
      /// </summary>
      /// <param name="_size">
      ///   size in pixels
      /// </param>
      /// <returns>
      ///   scaled size
      /// </returns>
      function  apScale         ( const _size : Integer
                                ) : Integer ;

      procedure msgScrollUpdate ;

      /// <summary>
      ///   Creates and adds a node related to a layer.
      /// </summary>
      /// <param name="_layer">
      ///   the layer for which the node is to be created
      /// </param>
      /// <param name="_index">
      ///   -1 or index of sublayer
      /// </param>
      /// <param name="_node">
      ///   parent node or nil if none
      /// </param>
      function  addLayerNode    ( const _layer   : TGIS_Layer ;
                                  const _index   : Integer    ;
                                  const _node    : TGIS_TreeNode
                                ) : TGIS_TreeNode ;

      /// <summary>
      ///   Retrieves a node related to the specified layer.
      /// </summary>
      /// <param name="_layer">
      ///   the layer to be searched for
      /// </param>
      function  findLayerNode   ( const _layer   : TGIS_Layer
                                ) : TGIS_TreeNode ;

      /// <summary>
      ///   Retrieves a node related to the specified group.
      /// </summary>
      /// <param name="_group">
      ///   the group to be searched for
      /// </param>
      function  findGroupNode   ( const _group   : IGIS_HierarchyGroup
                                ) : TGIS_TreeNode ;

      /// <summary>
      ///   Retrieves a node by a node text.
      /// </summary>
      /// <param name="_name">
      ///   node text
      /// </param>
      function  findNodeByName  ( const _name    : String
                                ) : TGIS_TreeNode ;

      /// <summary>
      ///   Deletes a node and all its subnodes (recursively).
      /// </summary>
      /// <param name="_node">
      ///   the node to be deleted
      /// </param>
      procedure deleteNode      ( const _node    : TGIS_TreeNode
                                ) ;

      /// <summary>
      ///   Creates and adds parameter subnodes of a node related to a layer.
      /// </summary>
      /// <param name="_node">
      ///   the layer node to be filled
      /// </param>
      procedure addParams       ( const _node    : TGIS_TreeNode
                                ) ;

      /// <summary>
      ///   Fills the legend with the list of layers.
      /// </summary>
      procedure readLayers      ;

      /// <summary>
      ///   Fills the legend with the hierarchy tree.
      /// </summary>
      procedure readGroups      ;

      function  getFirstLayerInGroup
                                ( const _group : TGIS_TreeNode
                                ) : TGIS_TreeNode ;
      function  getParamsTextY  ( const _offset  : Integer ;
                                    var _h       : Integer
                                ) : Integer ;

      function  getHeightStd    : Integer ;

      function  getHeight16     ( const _text   : Boolean ;
                                  const _offset : Integer
                                ) : Integer ;

      function  getHeightParams ( const _text   : Boolean
                                ) : Integer ;

      function  getHeightParamsHeader
                                : Integer ;

      function  getHeightParamsText
                                : Integer ;

      function  getHeightParamsRender
                                : Integer ;

      function  getHeightParamsChart
                                : Integer ;

      function  getHeightPixel  : Integer ;

      /// <summary>
      ///   Calculates the current node height (with all its subnodes)
      ///   in expanded state and stores the value in the node data.
      /// </summary>
      /// <param name="_node">
      ///   the node
      /// </param>
      procedure calcNodeHeight  ( const _node    : TGIS_TreeNode
                                ) ;

      /// <summary>
      ///   Retrieves the drawing rectangle for the text of a node.
      /// </summary>
      /// <param name="_node">
      ///   the node
      /// </param>
      /// <param name="_size">
      ///   True for coordinates relative to the node rectangle,
      ///   False for coordinates relative to the control
      /// </param>
      /// <param name="_selected">
      ///   if True, text to be selected
      /// </param>
      /// <returns>
      ///   rectangle
      /// </returns>
      function  getTextRect     ( const _node    : TGIS_TreeNode ;
                                  const _size    : Boolean ;
                                  const _selected: Boolean
                                ) : TRect ; overload ;

      /// <summary>
      ///   Retrieves the drawing rectangle for the text of a node.
      /// </summary>
      /// <param name="_node">
      ///   the node
      /// </param>
      /// <param name="_size">
      ///   True for coordinates relative to the node rectangle,
      ///   False for coordinates relative to the control
      /// </param>
      /// <param name="_selected">
      ///   if True, text to be selected
      /// </param>
      /// <param name="_text">
      ///   text to draw
      /// </param>
      /// <returns>
      ///   rectangle
      /// </returns>
      function  getTextRect     ( const _node    : TGIS_TreeNode ;
                                  const _size    : Boolean ;
                                  const _selected: Boolean ;
                                  const _text    : String
                                ) : TRect ; overload ;

      /// <summary>
      ///   Retrieves the drawing rectangle for the parameters of a layer node.
      /// </summary>
      /// <param name="_node">
      ///   the node
      /// </param>
      /// <param name="_size">
      ///   True for coordinates relative to the node rectangle,
      ///   False for coordinates relative to the control
      /// </param>
      /// <returns>
      ///   rectangle
      /// </returns>
      function  getParamsRect   ( const _node    : TGIS_TreeNode ;
                                  const _size    : Boolean
                                ) : TRect ;

      /// <summary>
      ///   Checks whether the supplied node is visible on screen.
      /// </summary>
      /// <param name="_node">
      ///   the node to be checked
      /// </param>
      /// <param name="_size">
      ///   True for size only (Left=0, Top=0),
      ///   False for coordinates relative to the control
      /// </param>
      /// <returns>
      ///   true or false
      /// </returns>
      function  isNodeOnScreen    ( const _node    : TGIS_TreeNode ;
                                    const _size    : Boolean
                                  ) : Boolean ;

      procedure makeNodeVisible   ( const _node    : TGIS_TreeNode ;
                                    const _delta   : Integer ;
                                    const _move    : Boolean
                                  ) ; overload ;
      procedure makeNodeVisible   ( const _node    : TGIS_TreeNode
                                  ) ; overload ;

      procedure enableDrag        ;
      procedure disableDrag       ;
      function  dragEnabled       : Boolean ; inline ;
      function  initDrag          ( const _top     : Integer
                                  ) : Boolean ;

    protected

      /// <summary>
      ///   Clear selection.
      /// </summary>
      procedure ClearSelection ;

    private

      /// <summary>
      ///   Overwrites the default node expansion control.
      /// </summary>
      /// <param name="_sender">
      ///   the control which owns the node
      /// </param>
      /// <param name="_node">
      ///   the node to be expanded
      /// </param>
      /// <param name="_allow">
      ///   if True then the node can be expanded
      /// </param>
      procedure doExpanding     (       _sender  : TObject ;
                                        _node    : TGIS_TreeNode ;
                                  var   _allow   : Boolean
                                ) ;

      /// <summary>
      ///   Overwrites the default node selection model.
      /// </summary>
      /// <param name="_sender">
      ///   the control which owns the node
      /// </param>
      /// <param name="_node">
      ///   the node to be selected
      /// </param>
      /// <param name="_allow">
      ///   True to allow change of selected node, False to block
      /// </param>
      procedure doChanging      (       _sender  : TObject ;
                                        _node    : TGIS_TreeNode ;
                                  var   _allow   : Boolean
                                ) ;

      /// <summary>
      ///   Reloads the tree.
      /// </summary>
      /// <param name="_disableEvents">
      ///   If True, select events will be disabled during the operation
      /// </param>
      procedure recreateTree    ( const _disableEvents : Boolean
                                ) ;

      /// <summary>
      ///   Locks the control for any visual updates.
      /// </summary>
      procedure lock            ;

      /// <summary>
      ///   Unlocks the control for any visual updates.
      /// </summary>
      procedure unlock          ;

      /// <summary>
      ///   Checks if the legend is locked.
      /// </summary>
      function  isLocked       : Boolean ;

      /// <summary>
      ///   Overrides the hierarchy of attached TGIS_ViewerWnd with the
      ///   current content of the legend.
      /// </summary>
      procedure buildHierarchy ;

      /// <summary>
      ///   Retrieves the drawing rectangle for the expand/collapse glyph
      ///   of a node.
      /// </summary>
      /// <param name="_node">
      ///   the node
      /// </param>
      /// <param name="_size">
      ///   True for coordinates relative to the node rectangle,
      ///   False for coordinates relative to the control
      /// </param>
      /// <param name="_isTouch">
      ///   if True, the returned rectangle will be larger;
      ///   especially for touches
      /// </param>
      /// <returns>
      ///   rectangle
      /// </returns>
      function  getExpandRect   ( const _node    : TGIS_TreeNode ;
                                  const _size    : Boolean  ;
                                  const _isTouch : Boolean
                                ) : TRect ;

      function  getCheckBoxRectTop
                                ( const _nodeType : TGIS_LegendItemType
                                ) : Integer ;

      /// <summary>
      ///   Retrieves the drawing rectangle for the checkbox of a node.
      /// </summary>
      /// <param name="_node">
      ///   the node
      /// </param>
      /// <param name="_size">
      ///   True for coordinates relative to the node rectangle,
      ///   False for coordinates relative to the control
      /// </param>
      /// <returns>
      ///   rectangle
      /// </returns>
      function  getCheckBoxRect ( const _node    : TGIS_TreeNode ;
                                  const _size    : Boolean
                                ) : TRect ;

      /// <param name="_node">
      ///   the node to be expanded/collapsed
      /// </param>
      /// <param name="_expand">
      ///   True to expand, False to collapse
      /// </param>
      /// <returns>
      ///   If True, the legend has to be repainted
      /// </returns>
      function  forceExpanded   ( const _node    : TGIS_TreeNode ;
                                  const _expand  : Boolean
                                ) : Boolean ;

      /// <summary>
      ///   Moves a node.
      /// </summary>
      /// <param name="_src">
      ///   start index
      /// </param>
      /// <param name="_dst">
      ///   destination index
      /// </param>
      procedure moveNode        ( const _src     : Integer ;
                                  const _dst     : Integer
                                ) ;

      /// <summary>
      ///   Checks whether the oParent.ControlUpdate can be performed and if yes
      ///   then executes it.
      /// </summary>
      procedure safeUpdate      ;

      /// <summary>
      ///   Checks whether the oParent.ControlRepaint can be performed and if yes
      ///   then executes it.
      /// </summary>
      procedure safeRepaint     ;

    protected
      /// <summary>
      ///   Perform component cleanup
      /// </summary>
      {$IFNDEF MANAGED}
        procedure doDestroy      ; override ;
      {$ELSE}
        procedure doDestroy      ; virtual ;
      {$ENDIF}

    public

      /// <summary>
      ///   Constructor, creates an instance.
      /// </summary>
      /// <param name="_owner">
      ///   the control which owns the legend
      /// </param>
      {#ownership:_owner:ownif_empty}
      constructor Create        ( const _owner   : IGIS_LegendParent
                                ) ;

    public

      {$IFNDEF OXYGENE}
        /// <summary>
        ///   Destroy an instance.
        /// </summary>
        destructor  Destroy     ; override;
      {$ENDIF}

    public

      /// <inheritdoc from="IGIS_Subscribe"/>
      procedure SubscribedEvent (       _sender  : TObject ;
                                        _event   : Integer ;
                                        _context : TObject
                                ) ;

      /// <summary>
      ///   Checks if the legend item associated with the specific layer
      ///   is expanded.
      /// </summary>
      /// <param name="_layer">
      ///   the layer to be checked
      /// </param>
      /// <returns>
      ///   True if the item is expanded, False otherwise
      /// </returns>
      function  IsExpanded      ( const _layer   : TGIS_Layer
                                ) : Boolean ; overload ;

      /// <summary>
      ///   Checks if the legend item associated with the specific hierarchy
      ///   group is expanded.
      /// </summary>
      /// <param name="_group">
      ///   the group to be checked
      /// </param>
      /// <returns>
      ///   True if the item is expanded, False otherwise
      /// </returns>
      function  IsExpanded      ( const _group   : IGIS_HierarchyGroup
                                ) : Boolean ; overload ;

      /// <summary>
      ///   Expands a legend item associated with the specific layer.
      /// </summary>
      /// <param name="_layer">
      ///   the layer to be expanded
      /// </param>
      procedure Expand          ( const _layer   : TGIS_Layer
                                ) ; overload ;

      /// <summary>
      ///   Expands of a legend item associated with the specific hierarchy
      ///   group.
      /// </summary>
      /// <param name="_group">
      ///   the group to be expanded
      /// </param>
      procedure Expand          ( const _group   : IGIS_HierarchyGroup
                                ) ; overload ;

      /// <summary>
      ///   Expands a legend item corresponding to a hierarchy group.
      /// </summary>
      /// <param name="_group">
      ///   the group to be expanded
      /// </param>
      /// <param name="_deep">
      ///   if True the subitems will be expanded as well
      /// </param>
      /// <param name="_layers">
      ///   if True and _deep is True the layer items will be expanded as well
      /// </param>
      procedure Expand          ( const _group   : IGIS_HierarchyGroup ;
                                  const _deep    : Boolean ;
                                  const _layers  : Boolean
                                ) ; overload ;

      /// <summary>
      ///   Collapses a legend item corresponding to a layer.
      /// </summary>
      /// <param name="_layer">
      ///   the layer to be collapsed
      /// </param>
      procedure Collapse        ( const _layer   : TGIS_Layer
                                ) ; overload ;

      /// <summary>
      ///   Collapses a legend item corresponding to a hierarchy group.
      /// </summary>
      /// <param name="_group">
      ///   the group to be collapsed
      /// </param>
      procedure Collapse        ( const _group   : IGIS_HierarchyGroup
                                ) ; overload ;

      /// <summary>
      ///   Collapses a legend item corresponding to a hierarchy
      ///   group.
      /// </summary>
      /// <param name="_group">
      ///   the group to be collapsed
      /// </param>
      /// <param name="_deep">
      ///   if True the subitems will be collapsed as well
      /// </param>
      /// <param name="_layers">
      ///   if True and _deep is True the layer items will be collapsed as well
      /// </param>
      procedure Collapse        ( const _group   : IGIS_HierarchyGroup ;
                                  const _deep    : Boolean ;
                                  const _layers  : Boolean
                                ) ; overload ;

    public

      /// <summary>
      ///   Forces resize of the legend.
      /// </summary>
      procedure Resize          ;

      /// <summary>
      ///   Update PPI.
      /// </summary>
      /// <param name="_fontSize">
      ///   current font size
      /// </param>
      procedure UpdatePPI       ( const _fontSize : Single
                                ) ;

      /// <summary>
      ///   Retrieves the drawing rectangle for a node in its current state.
      /// </summary>
      /// <param name="_node">
      ///   the node
      /// </param>
      /// <param name="_size">
      ///   True for size only (Left=0, Top=0),
      ///   False for coordinates relative to the control
      /// </param>
      /// <returns>
      ///   node rectangle
      /// </returns>
      function  GetNodeRect     ( const _node    : TGIS_TreeNode ;
                                  const _size    : Boolean
                                ) : TRect ;

      /// <summary>
      ///   Retrieves the drawing rectangle for the drag bar.
      /// </summary>
      /// <param name="_node">
      ///   the node
      /// </param>
      /// <param name="_top">
      ///   top of the moved node
      /// </param>
      /// <returns>
      ///   drag bar rectangle
      /// </returns>
      function  GetDragBarRect  ( const _node    : TGIS_TreeNode ;
                                  const _top     : Integer
                                ) : TRect ;

      /// <summary>
      ///   Retrieves the node at a given position.
      /// </summary>
      /// <param name="_x">
      ///   X coordinate
      /// </param>
      /// <param name="_y">
      ///   Y coordinate
      /// </param>
      /// <returns>
      ///   rectangle
      /// </returns>
      function  GetNodeAtAlt    ( const _x       : Integer ;
                                  const _y       : Integer
                                ) : TGIS_TreeNode ;

      /// <summary>
      ///   Retrieves a layer or a group node to be actually moved.
      /// </summary>
      /// <param name="_node">
      ///   the node for which the dragging was initialized
      /// </param>
      /// <returns>
      ///   retrieved node
      /// </returns>
      function  GetMovableNode    ( const _node    : TGIS_TreeNode
                                  ) : TGIS_TreeNode ;

      /// <summary>
      ///   Draws the node.
      /// </summary>
      /// <param name="_node">
      ///   the node to be rendered
      /// </param>
      /// <param name="_always">
      ///   draw even the node is invisible on screen
      /// </param>
      procedure DrawNode          ( const _node    : TGIS_TreeNode ;
                                    const _always  : Boolean
                                  ) ;

      /// <summary>
      ///   Draws all nodes.
      /// </summary>
      /// <returns>
      ///   True, if there is something to draw
      /// </returns>
      function  DrawNodes         : Boolean ;

      /// <summary>
      ///   Checks if update is locked.
      /// </summary>
      /// <returns>
      ///   True, if update is locked
      /// </returns>
      function  IsUpdateLocked    : Boolean ;

      /// <summary>
      ///   update.
      /// </summary>
      /// <param name="_redraw">
      ///   if true, the legend will be always redrawn
      ///   if false, the legend will be redrawn conditionally
      /// </param>
      procedure DoUpdate          ( const _redraw  : Boolean
                                  ) ;

      /// <summary>
      ///   Forces invalidation of all the legend items.
      /// </summary>
      procedure InvalidateItems ;

      /// <summary>
      ///   Checks consistency if the internal data.
      ///   Recreates the tree if it is needed.
      ///   Recommended before paint operations.
      /// </summary>
      /// <returns>
      ///   True, if data has been recreated
      /// </returns>
      function FixDataConsistency : Boolean ;

      /// <summary>
      ///   Do mouse down.
      /// </summary>
      /// <param name="_x">
      ///   x-coordinate of the mouse
      /// </param>
      /// <param name="_y">
      ///   y-coordinate of the mouse
      /// </param>
      /// <param name="_isTouch">
      ///   True, if it is touch action
      /// </param>
      /// <param name="_action">
      ///   True, if the action is done
      /// </param>
      procedure MouseDown         ( const _x       : Double  ;
                                    const _y       : Double  ;
                                    const _isTouch : Boolean ;
                                    var   _action  : Boolean
                                  ) ;

      /// <summary>
      ///   Do mouse move.
      /// </summary>
      /// <param name="_x">
      ///   x-coordinate of the mouse
      /// </param>
      /// <param name="_y">
      ///   y-coordinate of the mouse
      /// </param>
      /// <param name="_leftBtn">
      ///   True, if left mouse button is in action
      /// </param>
      procedure MouseMove         ( const _x       : Double  ;
                                    const _y       : Double  ;
                                    const _leftBtn : Boolean
                                  ) ;

      /// <summary>
      ///   Do mouse up.
      /// </summary>
      /// <param name="_x">
      ///   x-coordinate of the mouse
      /// </param>
      /// <param name="_y">
      ///   y-coordinate of the mouse
      /// </param>
      procedure MouseUp           ( const _x       : Double  ;
                                    const _y       : Double
                                  ) ;

      /// <summary>
      ///   Do mouse wheel.
      /// </summary>
      /// <param name="_delta">
      ///   amount the mouse wheel has been moved
      /// </param>
      procedure MouseWheel        ( const _delta   : Integer
                                  ) ;

      /// <summary>
      ///   Do mouse leave.
      /// </summary>
      procedure MouseLeave        ;

      /// <summary>
      ///   Do tap simple.
      /// </summary>
      /// <param name="_leftBtn">
      ///   True, if left mouse button is in action
      /// </param>
      procedure TapSimple         ( const _leftBtn : Boolean
                                  ) ;

      /// <summary>
      ///   Do tap double.
      /// </summary>
      /// <param name="_leftBtn">
      ///   True, if left mouse button is in action
      /// </param>
      /// <param name="_action">
      ///   True, if the action is done
      /// </param>
      procedure TapDouble         ( const _leftBtn : Boolean ;
                                    var   _action  : Boolean
                                  ) ;

      /// <summary>
      ///   Do tap long.
      /// </summary>
      /// <param name="_leftBtn">
      ///   True, if left mouse button is in action
      /// </param>
      procedure TapLong           ( const _leftBtn : Boolean
                                  ) ;

      /// <summary>
      ///   Starts a touch.
      /// </summary>
      /// <param name="_x">
      ///   x-coordinate of the pointer
      /// </param>
      /// <param name="_y">
      ///   y-coordinate of the pointer
      /// </param>
      /// <param name="_handled">
      ///   True, if the touch is already handled
      /// </param>
      procedure TouchDown         ( const _x       : Double  ;
                                    const _y       : Double  ;
                                    var   _handled : Boolean
                                  ) ;

      /// <summary>
      ///   Continues the touch.
      /// </summary>
      /// <param name="_x">
      ///   x-coordinate of the pointer
      /// </param>
      /// <param name="_y">
      ///   y-coordinate of the pointer
      /// </param>
      /// <param name="_movement">
      ///   Yes, if there is a movement
      /// </param>
      procedure TouchMove         ( const _x       : Double  ;
                                    const _y       : Double  ;
                                    const _movement: Boolean
                                  ) ;

      /// <summary>
      ///   Completes the touch
      /// </summary>
      /// <param name="_x">
      ///   x-coordinate of the pointer
      /// </param>
      /// <param name="_y">
      ///   y-coordinate of the pointer
      /// </param>
      procedure TouchUp           ( const _x       : Double  ;
                                    const _y       : Double
                                  ) ;

      /// <summary>
      ///   Scrolls the content of the legend
      /// </summary>
      /// <param name="_x">
      ///   x-coordinate of the pointer
      /// </param>
      /// <param name="_y">
      ///   y-coordinate of the pointer
      /// </param>
      procedure ScrollWindow      ( const _x       : Double ;
                                    const _y       : Double
                                  ) ;

      /// <summary>
      ///   Do mouse move.
      /// </summary>
      /// <param name="_step">
      ///   how much to move
      /// </param>
      procedure DoMove            ( const _step    : Integer
                                  ) ;

      /// <summary>
      ///   Do mouse leave.
      /// </summary>
      /// <param name="_step">
      ///   how much to move
      /// </param>
      procedure DoSelect          ( const _step    : Integer
                                  ) ;
    public

      /// <summary>
      ///   Renderer.
      /// </summary>
      property Renderer   : TGIS_RendererAbstract
                            read  fget_Renderer
                            write fset_Renderer ;

      /// <summary>
      ///   Selected node.
      /// </summary>
      property SelectedNode : TGIS_TreeNode
                            read  fget_SelectedNode
                            write fset_SelectedNode ;

    public

      ///   <summary>
      ///   Selected layer.
      /// </summary>
      property GIS_Layer  : TGIS_Layer
                            read  fget_Layer
                            write fset_Layer ;

      /// <summary>
      ///   Selected group.
      /// </summary>
      property GIS_Group  : IGIS_HierarchyGroup
                            read  fget_Group
                            write fset_Group ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_PrintableControl"/>
      property InternalName : String
                            read  FInternalName
                            write FInternalName ;

    public

      /// <summary>
      ///   DefHeight.
      /// </summary>
      property PPI          : Integer
                              read  FPPI
                              write fset_PPI ;

      /// <summary>
      ///   DefHeight16.
      /// </summary>
      property DefHeight16 : Integer
                             read  FDefHeight16
                             write FDefHeight16 ;

      /// <summary>
      ///   Top offset.
      /// </summary>
      property TopOffset   : Integer
                             read  FTopOffset
                             write fset_TopOffset ;

      /// <summary>
      ///   Length.
      /// </summary>
      property LegendLength : Integer
                             read  FLength
                             write FLength ;

      /// <summary>
      ///   Last node.
      /// </summary>
      property LastNode     : TGIS_TreeNode
                              read  FLastNode
                              write FLastNode ;

      /// <summary>
      ///   Scale factor.
      /// </summary>
      property ScaleFactor  : Double
                              read  FScaleFactor ;

      /// <summary>
      ///   Level indent.
      /// </summary>
      property ExpandRectSize : Integer
                              read  FExpandRectSize
                              write FExpandRectSize ;

      /// <summary>
      ///   Level indent.
      /// </summary>
      property CheckBoxRectSize : Integer
                              read  FCheckBoxRectSize
                              write FCheckBoxRectSize ;

      /// <summary>
      ///   Level indent.
      /// </summary>
      property LevelIndent  : Integer
                              read  FLevelIndent
                              write FLevelIndent ;

      /// <summary>
      ///   DifHScroll.
      /// </summary>
      property DifHScroll   : Integer
                              read  FDifHScroll
                              write FDifHScroll ;

      /// <summary>
      ///   ValHScroll.
      /// </summary>
      property ValHScroll   : Integer
                              read  FValHScroll
                              write FValHScroll ;

      /// <summary>
      ///   MaxHScroll.
      /// </summary>
      property MaxHScroll   : Integer
                              read  FMaxHScroll
                              write FMaxHScroll ;

  public

      /// <summary>
      ///   Attached TGIS_ViewerWnd control.
      /// </summary>
      property GIS_Viewer : IGIS_Viewer
                            read  FViewer
                            write fset_Viewer ;

      /// <summary>
      ///   Items.
      /// </summary>
      property Items      : TGIS_TreeList
                            read  FItems ;

      /// <summary>
      ///   If True then the legend view is compacted - icons are smaller.
      /// </summary>
      property CompactView : Boolean
                             read  FCompactView
                             write fset_CompactView ;
      /// <summary>
      ///   Draw style of legend icons.
      /// </summary>
      property DrawIconStyle : TGIS_LegendIconStyle
                               read  FDrawIconStyle
                               write fset_DrawIconStyle ;
      /// <summary>
      ///   Mode of the legend - list of layers or grouped tree view.
      /// </summary>
      property Mode       : TGIS_ControlLegendMode
                            read  FMode
                            write fset_Mode ;

      /// <summary>
      ///   Options of the legend.
      /// </summary>
      property Options    : TGIS_ControlLegendOptions
                            read  FOptions
                            write fset_Options ;

      /// <summary>
      ///   True if the order of legend entries in the Layer mode should be
      ///   reverse, i.e. the topmost layer in the attached TGIS_ViewerWnd
      ///   is bottommost in the legend.
      /// </summary>
      property ReverseOrder : Boolean
                              read  FReverseOrder
                              write fset_ReverseOrder ;

      /// <summary>
      ///   Options defining dialogs behavior.
      /// </summary>
      property DialogOptions : TGIS_ControlLegendDialogOptions
                               read  FDialogOptions
                               write fset_DialogOptions ;

      /// <summary>
      ///   True if long tap timer for touches is involved directly by pressing the pointer.
      ///   False if the event is involved rather after releasing the pointer.
      /// </summary>
      property DirectTouchLongTap : Boolean
                               read  FDirectTouchLongTap
                               write FDirectTouchLongTap ;

    public //events

      /// <event/>
      /// <summary>
      ///   Event fired when a layer parameters get changed.
      /// </summary>
      {$IFDEF CLR}
        event    LayerParamsChangeEvent : TGIS_LayerEvent
                                          delegate FLayerParamsChangeEvent ;
      {$ELSE}
        property LayerParamsChangeEvent : TGIS_LayerEvent
                                          read  FLayerParamsChangeEvent
                                          write FLayerParamsChangeEvent ;
      {$ENDIF}

      /// <event/>
      /// <summary>
      ///   Event fired when a layer is activated/deactivated.
      /// </summary>
      {$IFDEF CLR}
        event    LayerActiveChangeEvent : TGIS_LayerEvent
                                          delegate FLayerActiveChangeEvent ;
      {$ELSE}
        property LayerActiveChangeEvent : TGIS_LayerEvent
                                          read  FLayerActiveChangeEvent
                                          write FLayerActiveChangeEvent ;
      {$ENDIF}

      /// <event/>
      /// <summary>
      ///   Event fired upon layer selection.
      /// </summary>
      {$IFDEF CLR}
        event    LayerSelectEvent       : TGIS_LayerEvent
                                          delegate FLayerSelectEvent ;
      {$ELSE}
        property LayerSelectEvent       : TGIS_LayerEvent
                                          read  FLayerSelectEvent
                                          write FLayerSelectEvent ;
      {$ENDIF}

      /// <event/>
      /// <summary>
      ///   Event fired when a group is activated/deactivated.
      /// </summary>
      {$IFDEF CLR}
        event    GroupActiveChangeEvent : TGIS_HierarchyGroupEvent
                                          delegate FGroupActiveChangeEvent ;
      {$ELSE}
        property GroupActiveChangeEvent : TGIS_HierarchyGroupEvent
                                          read  FGroupActiveChangeEvent
                                          write FGroupActiveChangeEvent ;
      {$ENDIF}

      /// <event/>
      /// <summary>
      ///   Event fired upon group selection.
      /// </summary>
      {$IFDEF CLR}
        event    GroupSelectEvent       : TGIS_HierarchyGroupEvent
                                          delegate FGroupSelectEvent ;
      {$ELSE}
        property GroupSelectEvent       : TGIS_HierarchyGroupEvent
                                          read  FGroupSelectEvent
                                          write FGroupSelectEvent ;
      {$ENDIF}

      /// <event/>
      /// <summary>
      ///   Event fired upon a change of the order of layers.
      /// </summary>
      {$IFDEF CLR}
        event    OrderChangeEvent       : EventHandler
                                          delegate FOrderChangeEvent ;
      {$ELSE}
        property OrderChangeEvent       : TNotifyEvent
                                          read  FOrderChangeEvent
                                          write FOrderChangeEvent ;
      {$ENDIF}

  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Math,
    System.Generics.Defaults,

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoParams,
    Lider.CG.GIS.GeoLayerVector ;
{$ENDIF}

//==============================================================================
// TGIS_TreeNode
//==============================================================================

  constructor TGIS_TreeNode.Create ;
  begin
    inherited ;

    lSiblings := nil ;

    FParent   := nil ;
    FItems    := nil ;
    FSelected := False ;
    FExpanded := False ;
    FText     := '' ;
    FData     := nil ;
  end ;

  destructor TGIS_TreeNode.Destroy ;
  begin
    FreeObject( FItems ) ;
    FreeObject( FData ) ;

    inherited ;
  end ;

  function TGIS_TreeNode.fget_Items(
    const _idx : Integer
  ) : TGIS_TreeNode ;
  begin
    if assigned( FItems ) then
      Result := FItems[_idx]
    else
      Result := nil ;
  end ;

  procedure TGIS_TreeNode.fset_Items(
    const _idx : Integer ;
    const _val : TGIS_TreeNode
  ) ;
  begin
    if not assigned( FItems ) then
      exit ;

    {$IFDEF OXYGENE}
      FItems.RemoveAt( _idx ) ;
    {$ELSE}
      FItems.Delete( _idx ) ;
    {$ENDIF}
    FItems.Insert( _idx, _val ) ;
  end ;

  function TGIS_TreeNode.fget_Count : Integer ;
  begin
    if assigned( FItems ) then
      Result := FItems.Count
    else
      Result := 0 ;
  end ;

  function TGIS_TreeNode.fget_Index : Integer ;
  begin
    Result := lSiblings.IndexOf( Self ) ;
  end ;

  function TGIS_TreeNode.fget_Level : Integer ;
  var
    n : TGIS_TreeNode ;
    i : Integer ;
  begin
    i := 0 ;
    n := Self.Parent ;
    while assigned( n ) do begin
      inc( i ) ;
      n := n.Parent ;
    end ;

    Result := i ;
  end ;

  function TGIS_TreeNode.fget_Selected : Boolean ;
  begin
    Result := FSelected ;
  end ;

  procedure TGIS_TreeNode.fset_Selected(
    const _val : Boolean
  ) ;
  begin
    FSelected := _val ;
    if assigned( lSelected ) then begin
      if _val then
        lSelected.Add( Self )
      else
        lSelected.Remove( Self ) ;
    end ;
  end ;

  function TGIS_TreeNode.AddNode : TGIS_TreeNode ;
  var
    n : TGIS_TreeNode ;
  begin
    if not assigned( FItems ) then
      FItems := TGIS_TreeList.Create( nil ) ;

    n := TGIS_TreeNode.Create ;
    n.FParent := Self ;
    n.lSiblings := FItems ;
    n.lSelected := lSelected ;
    FItems.Add( n ) ;

    Result := n ;
  end ;

  procedure TGIS_TreeNode.AppendNode(
    const _node : TGIS_TreeNode ;
    const _pos  : Integer
  ) ;
  begin
    if not assigned( FItems ) then
      FItems := TGIS_TreeList.Create( nil ) ;

    _node.FParent   := Self ;
    _node.lSiblings := FItems ;

    if ( _pos <> -1 ) and ( _pos < Count )
    then
      FItems.Insert( _pos, _node )
    else
      FItems.Add( _node ) ;
  end ;

  procedure TGIS_TreeNode.Remove(
    const _node : TGIS_TreeNode
  ) ;
  begin
    FItems.Remove( _node ) ;
  end ;

  function TGIS_TreeNode.GetPrevSibling : TGIS_TreeNode ;
  var
    n : TGIS_TreeNode ;
  begin
    if Index <= 0 then
      n := nil
    else
      {$IFDEF OXYGENE}
        n := lSiblings.Item[Index-1] ;
      {$ELSE}
        n := lSiblings.Items[Index-1] ;
      {$ENDIF}

    Result := n ;
  end ;

  function TGIS_TreeNode.GetNextSibling : TGIS_TreeNode ;
  var
    n : TGIS_TreeNode ;
  begin
    if Index = lSiblings.Count - 1 then
      n := nil
    else
      {$IFDEF OXYGENE}
        n := lSiblings.Item[Index+1] ;
      {$ELSE}
        n := lSiblings.Items[Index+1] ;
      {$ENDIF}

    Result := n ;
  end ;

  function TGIS_TreeNode.FindSelected : TGIS_TreeNode ;
  begin
    if not assigned( FItems ) then
      Result := nil
    else
      Result := FItems.Selected ;
  end ;

  procedure TGIS_TreeNode.Expand(
    const _deep : Boolean
  ) ;
  var
    i : Integer ;
  begin
    FExpanded := True ;
    if _deep and assigned( FItems ) then begin
      for i := 0 to FItems.Count - 1 do
        FItems[i].Expand( _deep ) ;
    end ;
  end ;

//==============================================================================
// TGIS_TreeList
//==============================================================================

  constructor TGIS_TreeList.Create(
    const _slctd : TList<TGIS_TreeNode>
  ) ;
  begin
    inherited Create ;
    lSelected := _slctd ;
  end ;

  destructor TGIS_TreeList.Destroy ;
  var
    i  : Integer ;
    nd : TGIS_TreeNode ;
  begin
    for i := 0 to Count - 1 do begin
      {$IFDEF OXYGENE}
        nd := Item[i] ;
      {$ELSE}
        nd := Items[i] ;
      {$ENDIF}
      FreeObject( nd ) ;
    end ;
    inherited ;
  end ;

  function TGIS_TreeList.fget_Selected : TGIS_TreeNode ;
  begin
    if lSelected.Count = 0 then
      Result := nil
    else
      Result := lSelected[0] ;
  end ;

  procedure TGIS_TreeList.fset_Selected(
    const _val : TGIS_TreeNode
  ) ;
  begin
    deselectAll ;

    if assigned( _val ) then
      _val.Selected := True ;
  end ;

  procedure TGIS_TreeList.deselectAll ;
  var
    i : Integer ;
  begin
    for i := lSelected.Count - 1 downto 0 do
      lSelected[i].Selected := False ;
  end ;

  function TGIS_TreeList.AddChild(
    const _node : TGIS_TreeNode ;
    const _name : String
  ) : TGIS_TreeNode ;
  var
    n : TGIS_TreeNode ;
  begin
    if assigned( _node ) then
      n := _node.AddNode
    else begin
      n := TGIS_TreeNode.Create ;
      n.FParent := nil ;
      n.lSiblings := Self ;
      n.lSelected := lSelected ;
      inherited Add( n ) ;
    end ;
    n.Text := _name ;

    Result := n ;
  end ;

  procedure TGIS_TreeList.InsertChild(
    const _idx  : Integer ;
    const _node : TGIS_TreeNode
  ) ;
  begin
    Insert( _idx, _node ) ;

    _node.lSiblings := Self ;
    _node.FParent := nil ;
  end ;

  procedure TGIS_TreeList.AppendChild(
    const _node : TGIS_TreeNode
  ) ;
  begin
    Add( _node ) ;

    _node.lSiblings := Self ;
    _node.FParent := nil ;
  end ;

  procedure TGIS_TreeList.BeginUpdate ;
  begin
    // do nothing
  end ;

  procedure TGIS_TreeList.EndUpdate ;
  begin
    // do nothing
  end ;

//==============================================================================
// TGIS_LegendDialogOptions
//==============================================================================

  constructor TGIS_ControlLegendDialogOptions.Create ;
  begin
    inherited Create ;

    FVectorWizardUniqueLimit       := DIALOG_OPT_VW_UNIQUE_LIMIT ;
    FVectorWizardUniqueSearchLimit := DIALOG_OPT_VW_UNIQUE_SEARCH_LIMIT ;
  end ;

  constructor TGIS_ControlLegendDialogOptions.Create(
    const _uniqueLimit        : Integer ;
    const _uniqueSearchLimit  : Integer
   ) ;
  begin
    inherited Create ;

    FVectorWizardUniqueLimit       := _uniqueLimit ;
    FVectorWizardUniqueSearchLimit := _uniqueSearchLimit ;
  end ;

//==============================================================================
// TGIS_ControlLegendOptionsHelper
//==============================================================================

  {$IFDEF OXYGENE}
    {$IFDEF CLR OR ISLAND}
      class function TGIS_ControlLegendOptionsHelper.SetLegendOptions(
        const _options : List<Integer>
      ) : TGIS_ControlLegendOption ;
    {$ENDIF}
    {$IFDEF JAVA}
      class function TGIS_ControlLegendOptionsHelper.SetLegendOptions(
        const _options: java.util.List<Integer>
      ) : Integer;
    {$ENDIF}
    begin
      Result := 0;
      for elem in _options do begin
        {$IFDEF CLR}
          Result := Result or elem;
        {$ENDIF}
        {$IFDEF JAVA}
          Result := Result + Integer( Math.Pow( 2, elem as Integer ) ) ;
        {$ENDIF}
      end;
    end;

    {$IFDEF CLR OR ISLAND}
      class function TGIS_ControlLegendOptionsHelper.GetLegendOptions(
        const _options: integer
      ) : List<Integer>;
    {$ENDIF}
    {$IFDEF JAVA}
      class function TGIS_ControlLegendOptionsHelper.GetLegendOptions(
        const _options: integer
      ) : java.util.List<TGIS_ControlLegendOption>;
    {$ENDIF}
    var
      {$IFDEF CLR OR ISLAND}
        res : List<Integer>;
      {$ENDIF}
      {$IFDEF JAVA}
        res : java.util.List<TGIS_ControlLegendOption>;
      {$ENDIF}
      opt : Integer ;
      opAllowMove : Integer ;
      opAllowActive : Integer ;
      opAllowExpand : Integer ;
      opAllowParams : Integer ;
      opAllowSelect : Integer ;
      opShowHiddenLayers : Integer ;
      opShowLayerFullPath : Integer ;
      opShowSubLayers : Integer ;
      opAllowParamsVisible : Integer ;

      {$IFDEF CLR OR ISLAND}
        method initialize ;
        begin
          opAllowMove           := Integer( TGIS_ControlLegendOption.AllowMove )           ;
          opAllowActive         := Integer( TGIS_ControlLegendOption.AllowActive )         ;
          opAllowExpand         := Integer( TGIS_ControlLegendOption.AllowExpand )         ;
          opAllowParams         := Integer( TGIS_ControlLegendOption.AllowParams )         ;
          opAllowSelect         := Integer( TGIS_ControlLegendOption.AllowSelect )         ;
          opShowHiddenLayers    := Integer( TGIS_ControlLegendOption.ShowHiddenLayers )    ;
          opShowLayerFullPath   := Integer( TGIS_ControlLegendOption.ShowLayerFullPath )   ;
          opShowSubLayers       := Integer( TGIS_ControlLegendOption.ShowSubLayers )       ;
          opAllowParamsVisible  := Integer( TGIS_ControlLegendOption.AllowParamsVisible )  ;
        end;
      {$ENDIF}
      {$IFDEF JAVA}
        function convert_legend_option_to_integer( const _val : TGIS_ControlLegendOption) : Integer;
        begin
          Result := Integer( Math.Pow( 2, _val as Integer ) )
        end;

        method initialize ;
        begin
          opAllowMove           := convert_legend_option_to_integer( TGIS_ControlLegendOption.AllowMove )           ;
          opAllowActive         := convert_legend_option_to_integer( TGIS_ControlLegendOption.AllowActive )         ;
          opAllowExpand         := convert_legend_option_to_integer( TGIS_ControlLegendOption.Allowexpand )         ;
          opAllowParams         := convert_legend_option_to_integer( TGIS_ControlLegendOption.AllowParams )         ;
          opAllowSelect         := convert_legend_option_to_integer( TGIS_ControlLegendOption.AllowSelect )         ;
          opShowHiddenLayers    := convert_legend_option_to_integer( TGIS_ControlLegendOption.ShowHiddenLayers )    ;
          opShowLayerFullPath   := convert_legend_option_to_integer( TGIS_ControlLegendOption.ShowLayerFullPath )   ;
          opShowSubLayers       := convert_legend_option_to_integer( TGIS_ControlLegendOption.ShowSubLayers )       ;
          opAllowParamsVisible  := convert_legend_option_to_integer( TGIS_ControlLegendOption.AllowParamsVisible )  ;
        end;
      {$ENDIF}

    begin
      initialize;

      {$IFDEF CLR OR ISLAND}
        res := List<Integer>.create;
      {$ENDIF}
      {$IFDEF JAVA}
        res := java.util.ArrayList<TGIS_ControlLegendOption>.create;
      {$ENDIF}
      opt := _options;

      if opt >= opAllowParamsVisible then begin
        {$IFDEF CLR}
          res.Add( TGIS_ControlLegendOption.AllowParamsVisible as Integer ) ;
        {$ENDIF}
        {$IFDEF JAVA}
          res.add( TGIS_ControlLegendOption.AllowParamsVisible ) ;
        {$ENDIF}
        opt := opt -opAllowParamsVisible ;
      end;

      if opt >= opShowSubLayers then begin
        {$IFDEF CLR}
          res.Add( TGIS_ControlLegendOption.ShowSubLayers as Integer ) ;
        {$ENDIF}
        {$IFDEF JAVA}
          res.add( TGIS_ControlLegendOption.ShowSubLayers ) ;
        {$ENDIF}
        opt := opt -opShowSubLayers ;
      end;

      if opt >= opShowLayerFullPath then begin
        {$IFDEF CLR}
          res.Add( TGIS_ControlLegendOption.ShowLayerFullPath as Integer ) ;
        {$ENDIF}
        {$IFDEF JAVA}
          res.add( TGIS_ControlLegendOption.ShowLayerFullPath ) ;
        {$ENDIF}
        opt := opt -opShowLayerFullPath ;
      end;

      if opt >= opShowHiddenLayers then begin
        {$IFDEF CLR}
          res.Add( TGIS_ControlLegendOption.ShowHiddenLayers as Integer ) ;
        {$ENDIF}
        {$IFDEF JAVA}
          res.add( TGIS_ControlLegendOption.ShowHiddenLayers ) ;
        {$ENDIF}
        opt := opt -opShowHiddenLayers ;
      end;

      if opt >= opAllowSelect then begin
        {$IFDEF CLR}
          res.Add( TGIS_ControlLegendOption.AllowSelect as Integer ) ;
        {$ENDIF}
        {$IFDEF JAVA}
          res.add( TGIS_ControlLegendOption.AllowSelect ) ;
        {$ENDIF}
        opt := opt -opAllowSelect ;
      end;

      if opt >= opAllowParams then begin
        {$IFDEF CLR}
          res.Add( TGIS_ControlLegendOption.AllowParams as Integer ) ;
        {$ENDIF}
        {$IFDEF JAVA}
          res.add( TGIS_ControlLegendOption.AllowParams ) ;
        {$ENDIF}
        opt := opt -opAllowParams ;
      end;

      if opt >= opAllowExpand then begin
        {$IFDEF CLR}
          res.Add( TGIS_ControlLegendOption.AllowExpand as Integer ) ;
        {$ENDIF}
        {$IFDEF JAVA}
          res.add( TGIS_ControlLegendOption.AllowExpand ) ;
        {$ENDIF}
        opt := opt -opAllowExpand ;
      end;

      if opt >= opAllowActive then begin
        {$IFDEF CLR}
          res.Add( TGIS_ControlLegendOption.AllowActive as Integer ) ;
        {$ENDIF}
        {$IFDEF JAVA}
          res.add( TGIS_ControlLegendOption.AllowActive ) ;
        {$ENDIF}
        opt := opt -opAllowActive ;
      end;

      if opt >= opAllowMove then begin
        {$IFDEF CLR}
          res.Add( TGIS_ControlLegendOption.AllowMove as Integer ) ;
        {$ENDIF}
        {$IFDEF JAVA}
          res.add( TGIS_ControlLegendOption.AllowMove ) ;
        {$ENDIF}
        opt := opt -opAllowMove ;
      end;

      Result := res ;
    end ;
  {$ENDIF}

//==============================================================================
// TGIS_Legend
//==============================================================================

  constructor TGIS_Legend.Create(
    const _owner : IGIS_LegendParent
  ) ;
  begin
    inherited Create ;

    FLength     := 0 ;
    FTopOffset  := 0 ;

    iLocked      := 0 ;
    canExpand    := False ;
    selectedFlag := False ;
    skipUpdate   := False ;
    skipSelect   := False ;
    lastHash     := 0 ;

    FExpandRectSize   := 9  ;
    FCheckBoxRectSize := 14 ;
    FLevelIndent      := 12 ;

    FValHScroll  := 0 ;
    FDifHScroll  := 0 ;
    FMaxHScroll  := 0 ;

    oParent := _owner ;
    FViewer := nil ;

    FMode := TGIS_ControlLegendMode.Layers ;
    Options := [
      TGIS_ControlLegendOption.AllowMove,
      TGIS_ControlLegendOption.AllowActive,
      TGIS_ControlLegendOption.AllowExpand,
      TGIS_ControlLegendOption.AllowParams,
      TGIS_ControlLegendOption.AllowSelect,
      TGIS_ControlLegendOption.ShowSubLayers,
      TGIS_ControlLegendOption.AllowParamsVisible
    ] ;

    FReverseOrder := False ;

    FPPI            := 96 ;
    FScaleFactor    := 1.0 ;
    FFontSize       := 8 ;
    FDefHeight      := apScale( 16 ) ;
    FDefHeight16    := apScale( 16 ) ;
    FDefBmpSize     := apScale( 28 ) ;
    FCompactView    := False ;
    FDrawIconStyle  := TGIS_LegendIconStyle.Default ;

    oBMPFactory := TGIS_LegendIconFactory.Create( FDefBmpSize, FCompactView, FDrawIconStyle ) ;
    oBMPFactory.CustomPPI := PPI ;

    FDialogOptions := TGIS_ControlLegendDialogOptions.Create(
      DIALOG_OPT_VW_UNIQUE_LIMIT,
      DIALOG_OPT_VW_UNIQUE_SEARCH_LIMIT
    ) ;

    lstOldLayerNodes := TDictionary<String, TGIS_TreeNode>.Create(
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
                        ) ;
    lstOldGroupNodes := TDictionary<String, TGIS_TreeNode>.Create(
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
                        ) ;

    oSelected := TList<TGIS_TreeNode>.Create ;
    FItems := TGIS_TreeList.Create( oSelected ) ;
    sNewSelected := '' ;

    moveTop     := 0 ;
    moveLevel   := 0 ;
    fOldZoom    := 0 ;
    isMouseDn   := False ;

    FDirectTouchLongTap := False ;
    FOldStyleDrag := False ;
    disableDrag ;
    {$IFDEF JAVA}
      ptDrag       := new TPoint( 0, 0 ) ;
    {$ENDIF}

    FLastNode := nil ;

    FLayerParamsChangeEvent := nil ;
    FLayerActiveChangeEvent := nil ;
    FLayerSelectEvent := nil ;
    FGroupActiveChangeEvent := nil ;
    FGroupSelectEvent := nil ;
    FOrderChangeEvent := nil ;

    FNoDataInfo := False ;
    FNoParams   := False ;
    FBasemap    := False ;
  end ;

  procedure TGIS_Legend.doDestroy ;
  begin
    FreeObject( FDialogOptions   ) ;
    FreeObject( oBMPFactory      ) ;
    FreeObject( lstOldLayerNodes ) ;
    FreeObject( lstOldGroupNodes ) ;
    FreeObject( oSelected        ) ;
    FreeObject( FItems           ) ;

    {$IFNDEF MANAGED}
      inherited ;
    {$ENDIF}
  end ;

  {$IFNDEF OXYGENE}
    destructor TGIS_Legend.Destroy ;
    begin
      doDestroy ;
      inherited ;
    end ;
  {$ENDIF}

  procedure TGIS_Legend.fset_CompactView(
    const _value : Boolean
  ) ;
  begin
    if FCompactView = _value then exit ;

    FreeObject( oBMPFactory ) ;
    oBMPFactory := TGIS_LegendIconFactory.Create( FDefBmpSize, _value, FDrawIconStyle ) ;
    oBMPFactory.CustomPPI := PPI ;
    FCompactView := _value ;
    safeRepaint ;
  end ;

  procedure TGIS_Legend.fset_DrawIconStyle(
    const _value : TGIS_LegendIconStyle
  ) ;
  begin
    if FDrawIconStyle = _value then exit ;

    FreeObject( oBMPFactory ) ;
    oBMPFactory := TGIS_LegendIconFactory.Create( FDefBmpSize, FCompactView, _value ) ;
    oBMPFactory.CustomPPI := PPI ;
    FDrawIconStyle := _value ;
    safeRepaint ;
  end ;

  procedure TGIS_Legend.fset_PPI(
    const _value : Integer
  ) ;
  begin
    if FPPI = _value then exit ;

    FPPI := _value ;
    FScaleFactor := FPPI / 96.0 ;
    FDefHeight   := apScale( RoundS( 2 * FFontSize ) ) ;
    FDefHeight16 := apScale( 16 ) ;
    FDefBmpSize  := apScale( 28 ) ;
    FreeObject( oBMPFactory ) ;
    oBMPFactory := TGIS_LegendIconFactory.Create( FDefBmpSize, FCompactView, FDrawIconStyle ) ;
    oBMPFactory.CustomPPI := PPI ;
  end ;

  procedure TGIS_Legend.fset_TopOffset(
    const _value : Integer
  ) ;
  begin
    if FTopOffset = _value then exit ;

    FTopOffset := _value ;
    safeRepaint ;
  end ;

  procedure TGIS_Legend.fset_Viewer(
    const _value : IGIS_Viewer
  ) ;
  begin
    if assigned( FViewer ) then
      FViewer.UnSubscribe( Self ) ;

    FViewer := _value ;

    if not assigned( FViewer ) then
      exit ;

    recreateTree( False ) ;

    FViewer.Subscribe( Self ) ;

    safeRepaint ;
  end ;

  function TGIS_Legend.fget_Layer : TGIS_Layer ;
  var
    dt : TGIS_LegendItemData ;
    nd : TGIS_TreeNode ;
  begin
    Result := nil ;

    if isLocked then exit ;

    nd := SelectedNode ;
    if not assigned( nd ) or not selectedFlag then
      exit ;

    dt := TGIS_LegendItemData( nd.Data ) ;
    if not assigned( dt ) then exit ;

    if dt.DataType = TGIS_LegendItemType.Layer then
      Result := dt.Layer ;
  end ;

  procedure TGIS_Legend.fset_Layer(
    const _value : TGIS_Layer
  ) ;
  var
    node : TGIS_TreeNode ;
  begin
    node := findLayerNode( _value ) ;
    if assigned( node ) then begin
      SelectedNode  := node ;
      selectedFlag  := True ;
      makeNodeVisible( node ) ;
    end
    else begin
      ClearSelection ;
      selectedFlag := False ;
    end ;
    safeRepaint ;
  end ;

  function TGIS_Legend.fget_Group : IGIS_HierarchyGroup ;
  var
    dt   : TGIS_LegendItemData ;
    node : TGIS_TreeNode ;
  begin
    Result := nil ;

    if isLocked then exit ;

    node := SelectedNode ;
    if not assigned( node ) or not selectedFlag then
      exit ;

    dt := TGIS_LegendItemData( node.Data ) ;
    if not assigned( dt ) then exit ;

    if dt.DataType = TGIS_LegendItemType.Group then
      Result := dt.Group ;
  end ;

  procedure TGIS_Legend.fset_Group(
    const _value : IGIS_HierarchyGroup
  ) ;
  var
    node : TGIS_TreeNode ;
  begin
    node := findGroupNode( _value ) ;
    if assigned( node ) then begin
      SelectedNode  := node ;
      selectedFlag := True ;
      makeNodeVisible( node ) ;
    end
    else begin
      ClearSelection ;
      selectedFlag := False ;
    end ;
    safeRepaint ;
  end ;

  procedure TGIS_Legend.fset_Mode(
    const _value : TGIS_ControlLegendMode
  ) ;
  begin
    if FMode = _value then exit ;

    FMode := _value ;

    if not assigned( FViewer ) then
      exit ;

    recreateTree( False ) ;

    safeRepaint ;
  end ;

  procedure TGIS_Legend.fset_Options(
    const _value : TGIS_ControlLegendOptions
  ) ;
  begin
    if FOptions = _value then  exit ;

    FOptions := _value ;

    if TGIS_ControlLegendOption.AllowMove in FOptions then
      opAllowMove := True
    else
      opAllowMove := False ;

    if TGIS_ControlLegendOption.AllowActive in FOptions then
      opAllowActive := True
    else
      opAllowActive := False ;

    if TGIS_ControlLegendOption.AllowExpand in FOptions then
      opAllowExpand := True
    else
      opAllowExpand := False ;

    if TGIS_ControlLegendOption.AllowParams in FOptions then
      opAllowParams := True
    else
      opAllowParams := False ;

    if TGIS_ControlLegendOption.AllowSelect in FOptions then
      opAllowSelect := True
    else
      opAllowSelect := False ;

    if TGIS_ControlLegendOption.ShowHiddenLayers in FOptions then
      opShowHiddenLayers := True
    else
      opShowHiddenLayers := False ;

    if TGIS_ControlLegendOption.ShowLayerFullPath in FOptions then
      opShowLayerFullPath := True
    else
      opShowLayerFullPath := False ;

    if TGIS_ControlLegendOption.ShowSubLayers in FOptions then
      opShowSubLayers := True
    else
      opShowSubLayers := False ;

    if TGIS_ControlLegendOption.AllowParamsVisible in FOptions then
      opAllowParamsVisible := True
    else
      opAllowParamsVisible := False ;

    // force recreating
    InvalidateItems ;
  end ;

  procedure TGIS_Legend.fset_ReverseOrder(
    const _value : Boolean
  ) ;
  begin
    if FReverseOrder = _value then exit ;

    FReverseOrder := _value ;

    if not assigned( GIS_Viewer ) then
      exit ;

    if FMode = TGIS_ControlLegendMode.Groups then
      exit ;

    recreateTree( False ) ;

    safeRepaint ;
  end ;

  procedure TGIS_Legend.fset_DialogOptions(
    const _value : TGIS_ControlLegendDialogOptions
  );
  begin
    FDialogOptions.Assign( _value ) ;
  end ;

  function TGIS_Legend.fget_Renderer
    : TGIS_RendererAbstract ;
  begin
    Result := oBMPFactory.Renderer ;
  end ;

  procedure TGIS_Legend.fset_Renderer(
    const _value : TGIS_RendererAbstract
  ) ;
  begin
    oBMPFactory.Renderer := _value ;
  end ;

  function TGIS_Legend.fget_SelectedNode : TGIS_TreeNode ;
  begin
    Result := FItems.Selected ;
  end ;

  procedure TGIS_Legend.fset_SelectedNode(
    const _value : TGIS_TreeNode
  ) ;
  var
    allow : Boolean ;
  begin
    doChanging( self, _value, allow ) ;
    if allow then
      FItems.Selected := _value ;
  end ;

  procedure TGIS_Legend.lock ;
  begin
    iLocked := iLocked + 1 ;
  end ;

  procedure TGIS_Legend.unlock ;
  begin
    iLocked := iLocked - 1 ;
  end ;

  function TGIS_Legend.isLocked
    : Boolean ;
  begin
    Result := ( iLocked > 0 ) ;
  end ;

  function TGIS_Legend.apScale(
    const _size : Integer
  ) : Integer ;
  begin
    {$IFDEF OXYGENE}
      Result := Convert.ToInt32( Math.Floor( _size * FScaleFactor ) ) ;
    {$ELSE}
      {$IFDEF LEVEL_RX10_VCL}
        Result := Floor( _size * FScaleFactor ) ;
      {$ELSE}
        Result := _size ;
      {$ENDIF}
    {$ENDIF}
  end ;

  function TGIS_Legend.addLayerNode(
    const _layer   : TGIS_Layer ;
    const _index   : Integer ;
    const _node    : TGIS_TreeNode
  ) : TGIS_TreeNode ;
  var
    la   : TGIS_Layer ;
    prev : TGIS_TreeNode ;
    slcd : Boolean ;
    expd : Boolean ;
    nfea : Integer ;
    node : TGIS_TreeNode ;
    str  : String ;
    sb   : TStringBuilder ;
    i    : Integer ;
  begin
    Result := nil ;

    if _index = -1 then
      la := _layer
    else
      la := TGIS_Layer( _layer.SubLayers[_index] ) ;

    if la.HideFromLegend then begin
      if not opShowHiddenLayers then
        exit ;
    end ;

    lstOldLayerNodes.TryGetValue( la.Name, prev ) ;
    if assigned( prev ) then begin
      slcd := prev.Selected ;
      expd := prev.Expanded ;
      nfea := prev.Count ;
    end
    else begin
      slcd := False ;
      expd := not la.Collapsed ;
      nfea := -1 ;
    end ;

    if opShowLayerFullPath and ( _index = -1 ) then
      str := la.Path
    else
      str := la.Caption ;

    sb := TStringBuilder.Create ;
    try
      sb.Append( str ) ;
      sb.Append( '    ' ) ;
      str := sb.ToString ;
    finally
      FreeObject( sb ) ;
    end ;

    node := FItems.AddChild( _node, str ) ;
    node.Data := TGIS_LegendItemData.Create( _layer, _index, FViewer ) ;

    if opShowSubLayers and assigned( la.SubLayers ) then begin
      for i := 0 to la.SubLayers.Count - 1 do
        addLayerNode( la, i, node ) ;
    end ;

    if opAllowExpand then
      addParams( node ) ;

    calcNodeHeight( node ) ;

    if not opAllowExpand then
      expd := False
    else
    if ( nfea = 0 ) and ( node.Count > 0 ) then
      expd := True ;

    forceExpanded( node, expd ) ;
    node.Selected := slcd ;
    if slcd then
      SelectedNode := node ;

    Result := node ;
  end ;

  function TGIS_Legend.findLayerNode(
    const _layer : TGIS_Layer
  ) : TGIS_TreeNode ;
  var
    node : TGIS_TreeNode ;
    //?dt   : TGIS_LegendItemData ;
    i    : Integer ;

    function find_layer_node( const _n : TGIS_TreeNode ) : TGIS_TreeNode ;
    var
      nn : TGIS_TreeNode ;
      dd : TGIS_LegendItemData ;
      ii : Integer ;
    begin
      Result := nil ;

      dd := TGIS_LegendItemData( _n.Data ) ;
      if dd.DataType = TGIS_LegendItemType.Group then begin

        if not assigned( dd.Group ) then
          exit ;

        for ii := 0 to _n.Count - 1 do begin
          nn := find_layer_node( _n.Items[ii] ) ;
          if assigned( nn ) then begin
            Result := nn ;
            exit ;
          end ;
        end ;

      end
      else
      if dd.DataType = TGIS_LegendItemType.Layer then begin

        if not assigned( dd.Layer ) then
          exit ;

        if assigned( dd.Layer.SubLayers ) then begin
          if dd.Layer.SubLayers.Count > 0 then begin
            for ii := 0 to _n.Count - 1 do begin
              nn := find_layer_node( _n.Items[ii] ) ;
              if assigned( nn ) then begin
                Result := nn ;
                exit ;
              end ;
            end ;
          end ;
        end ;

        if dd.Layer.Name <> _layer.Name then
          exit ;

        Result := _n ;
      end ;
    end ;

  begin
    Result := nil ;
    node   := nil ;
    if not assigned( _layer ) then
      exit ;

    if FItems.Count = 0 then
      exit ;

    for i := 0 to FItems.Count - 1 do begin
      node := FItems[i] ;
      //?dt := TGIS_LegendItemData( node.Data ) ;

      node := find_layer_node( node ) ;
      if assigned( node ) then
        break ;
    end ;

    Result := node ;
  end ;

  function TGIS_Legend.findGroupNode(
    const _group   : IGIS_HierarchyGroup
  ) : TGIS_TreeNode ;
  var
    node : TGIS_TreeNode ;
    dt   : TGIS_LegendItemData ;
    i    : Integer ;

    function find_group_node( const _n : TGIS_TreeNode ) : TGIS_TreeNode ;
    var
      nn : TGIS_TreeNode ;
      dd : TGIS_LegendItemData ;
      ii : Integer ;
    begin
      Result := nil ;

      dd := TGIS_LegendItemData( _n.Data ) ;
      if dd.DataType <> TGIS_LegendItemType.Group then
        exit ;

      if not assigned( dd.Group ) then
        exit ;

      if dd.Group.GroupsCount > 0 then begin
        for ii := 0 to _n.Count - 1 do begin
          nn := find_group_node( _n.Items[ii] ) ;
          if assigned( nn ) then begin
            Result := nn ;
            exit ;
          end ;
        end ;
      end ;

      if dd.Group.Name <> _group.Name then
        exit ;

      Result := _n ;
    end ;

  begin
    Result := nil ;
    node   := nil ;
    if not assigned( _group ) then
      exit ;

    if FItems.Count = 0 then
      exit ;

    for i := 0 to FItems.Count - 1 do begin
      node := FItems[i] ;
      dt := TGIS_LegendItemData( node.Data ) ;

      if dt.DataType <> TGIS_LegendItemType.Group then
        continue ;

      node := find_group_node( node ) ;
      if assigned( node ) then
        break ;
    end ;

    Result := node ;
  end ;

  function TGIS_Legend.findNodeByName(
    const _name : String
  ) : TGIS_TreeNode ;
  var
    i    : Integer ;
    node : TGIS_TreeNode ;
  begin
    Result := nil ;
    for i := 0 to FItems.Count - 1 do begin
      node := FItems[i] ;
      if node.Text = _name then begin
        Result := node ;
        break ;
      end;
    end ;
  end ;

  procedure TGIS_Legend.deleteNode(
    const _node : TGIS_TreeNode
  ) ;
  begin
    FItems.Remove( _node ) ;
    oSelected.Remove( _node ) ;
    FreeObjectNotNil( _node ) ;
  end ;

  procedure TGIS_Legend.addParams(
    const _node : TGIS_TreeNode
  ) ;
  var
    la   : TGIS_Layer ;
    nsec : TGIS_TreeNode ;
    nnil : TGIS_TreeNode ;
    sec  : TGIS_ParamsSection ;
    dt   : TGIS_LegendItemData ;
    i    : Integer ;
    j    : Integer ;
  begin
    dt := TGIS_LegendItemData( _node.Data ) ;
    if dt.DataType <> TGIS_LegendItemType.Layer then
      exit ;

    la := dt.Layer ;

    if la.BasemapDraw then begin
      // params data cannot be read
      FNoDataInfo := True ;
      exit ;
    end ;

    FItems.BeginUpdate ;

    for i := 0 to la.ParamsList.Count - 1 do begin
      sec := la.ParamsList.Items[i] ;

      dt := TGIS_LegendItemData.Create( sec, FViewer ) ;
      if dt.RowCount < 0 then begin
        FreeObject( dt ) ;
        continue ;
      end ;

      nsec := FItems.AddChild( _node, sec.Legend ) ;
      nsec.Data := dt ;

      for j := 1 to dt.RowCount do begin
        nnil := FItems.AddChild( nsec, '' ) ;
        nnil.Data := TGIS_LegendItemData.Create ;
        nnil.Text := 'Wy' ;
      end ;

      calcNodeHeight( nsec ) ;

      forceExpanded( nsec, True ) ;
    end ;

    FItems.EndUpdate ;
  end ;

  procedure TGIS_Legend.msgScrollUpdate ;
  var
    tmp : Integer ;
  begin
    tmp := FLength - oParent.ControlClientHeight ;

    if tmp < 0 then
      tmp := 0 ;
    if ( tmp = 0 ) and ( FTopOffset < 0 ) then
      //tmp := 2 ;
      FTopOffset := 0 ;

    oParent.ControlSetVScroll( 0, tmp, Abs( FTopOffset ) ) ;
    oParent.ControlSetHScroll( 0, 0, 0 ) ;
    (*if oParent.ControlRightToLeft then begin
      FMaxHScroll := oParent.ControlWidth ;
      oParent.ControlSetHScrollPosition( FMaxHScroll ) ;
      FValHScroll := oParent.ControlGetHScrollPosition ;
      FMaxHScroll := oParent.ControlWidth + FValHScroll ;
      FDifHScroll := FMaxHScroll - FValHScroll ;
    end ;*)
  end ;

  procedure TGIS_Legend.Resize ;
  begin
    msgScrollUpdate ;
  end ;

  procedure TGIS_Legend.UpdatePPI(
    const _fontSize : Single
  ) ;
  begin
    if FFontSize <> _fontSize then begin
      FFontSize  := _fontSize ;
      FDefHeight := apScale( RoundS( 2 * FFontSize ) ) ;
    end ;
  end ;

  procedure TGIS_Legend.readLayers ;
  var
    lst  : TList<TGIS_TreeNode> ;
    la   : TGIS_Layer ;
    node : TGIS_TreeNode ;
    tn   : TGIS_TreeNode ;
    i    : Integer ;

    procedure add_layer_node( const _idx : Integer ) ;
    begin
      la := TGIS_Layer( FViewer.Items.Items[_idx] ) ;
      node := addLayerNode( la, -1, nil ) ;
      if assigned( node ) then
        FLastNode := node ;
    end ;

    procedure addOldLayerNodes( const _node : TGIS_TreeNode ) ;
    var
      j   : Integer ;
      dt  : TGIS_LegendItemData ;
    begin
      dt := TGIS_LegendItemData( _node.Data ) ;
      if dt.DataType = TGIS_LegendItemType.Layer then begin
        la := dt.Layer ;
        if assigned( la ) and
           ( not lstOldLayerNodes.TryGetValue( la.Name, tn ) ) then
          lstOldLayerNodes.Add( la.Name, _node ) ;
      end ;

      if _node.Count = 0 then
        exit ;

      for j := 0 to _node.FItems.Count-1 do
        addOldLayerNodes( _node.FItems[j] ) ;
    end;

  begin
    lst := TList<TGIS_TreeNode>.Create ;
    try
      for i := 0 to FItems.Count - 1 do begin

        node := FItems[i] ;

        if node.Level = 0 then
          lst.Add( node ) ;

        addOldLayerNodes( node ) ;
      end ;

      node := nil ;

      FItems.BeginUpdate ;
      oSelected.Clear ;

      if FReverseOrder then begin
        for i := 0 to FViewer.Items.Count - 1 do
          add_layer_node( i ) ;
      end
      else begin
        for i := FViewer.Items.Count - 1 downto 0 do
          add_layer_node( i ) ;
      end ;

      lstOldLayerNodes.Clear ;
      for i := lst.Count - 1 downto 0 do
        {$IFDEF OXYGENE}
          deleteNode( lst.Item[i] ) ;
        {$ELSE}
          deleteNode( lst.Items[i] ) ;
        {$ENDIF}

      if ( not opAllowSelect ) or ( FItems.Count = 0 ) then begin
        SelectedNode := nil ;
        oSelected.Clear ;
      end ;

      FItems.EndUpdate ;
    finally
      FreeObject( lst ) ;
    end ;
  end ;

  function TGIS_Legend.GetNodeRect(
    const _node : TGIS_TreeNode ;
    const _size : Boolean
  ) : TRect ;
  var
    rct_left   : Integer ;
    rct_top    : Integer ;
    rct_right  : Integer ;
    rct_width  : Integer ;
    rct_height : Integer ;

    dt  : TGIS_LegendItemData ;

    par : TGIS_TreeNode ;
    prv : TGIS_TreeNode ;
    pdt : TGIS_LegendItemData ;

  begin
    if not assigned( _node ) then begin
      Result := Rect( 0, 0, oParent.ControlClientWidth, getHeightStd ) ;
      exit ;
    end ;

    dt := TGIS_LegendItemData( _node.Data ) ;

    prv := _node.GetPrevSibling ;
    par := _node.Parent ;

    if assigned( prv ) then begin
      pdt := TGIS_LegendItemData( prv.Data ) ;
      rct_left    := pdt.NodeLeft ;
      if prv.Expanded then
        rct_top   := pdt.NodeTop + pdt.NodeHeight
      else begin
        if ( pdt.DataType = TGIS_LegendItemType.Layer ) or
           ( pdt.DataType = TGIS_LegendItemType.Group ) then
          rct_top := pdt.NodeTop + getHeightStd
        else if pdt.IsVector then
          rct_top := pdt.NodeTop + getHeightParamsHeader
        else
          rct_top := pdt.NodeTop + getHeightPixel ;
      end ;
      rct_right   := pdt.NodeLeft + pdt.NodeWidth ;
    end
    else
    if assigned( par ) then begin
      pdt := TGIS_LegendItemData( par.Data ) ;
      rct_left   := pdt.NodeLeft ;
      if ( pdt.DataType = TGIS_LegendItemType.Layer ) or
         ( pdt.DataType = TGIS_LegendItemType.Group ) then
        rct_top  := pdt.NodeTop + getHeightStd
      else if pdt.IsVector then
        rct_top  := pdt.NodeTop + getHeightParamsHeader
      else
        rct_top  := pdt.NodeTop + getHeightPixel ;
      rct_right  := pdt.NodeLeft + pdt.NodeWidth ;
    end
    else
    begin
      (*if updVScroll then    //Winforms only?????
        rct := Nodes[0].Bounds
      else
        rct := prevBounds ;
      rct.Width := Width - 5 ;*)
      rct_left   := 0 ;
      rct_top    := FTopOffset ;
      rct_right  := oParent.ControlClientWidth ;
      (*if not oStyleLayer.isStyleEnabled then begin
        if rct.Top < -1e6 then
          rct.Top := GetScrollPos( Handle, SB_VERT ) ;
      end ;*)
    end ;

    if _node.Expanded then
      rct_height := dt.NodeHeight + 1
    else
      rct_height := getHeightStd + 1 ;

    if assigned( dt ) then begin
      dt.NodeTop   := rct_top ;
      dt.NodeLeft  := rct_left ;
      dt.NodeWidth := rct_right - rct_left ;
    end ;

    if _size then begin
      rct_width := ( rct_right - rct_left ) - _node.Level * apScale( FLevelIndent ) ;
      if ( _node.Level = 0 ) or
         TGIS_LegendItemData( _node.Data ).IsMoving then begin
        rct_top  := 0 ;
        rct_left := FValHScroll ;
        rct_right := rct_width ;
      end
      else begin
        rct_top  := rct_top - moveTop ;
        rct_left := - FValHScroll + ( _node.Level - moveLevel ) * apScale( FLevelIndent ) ;
        rct_right := rct_width ;
      end ;
    end
    else begin
      if oParent.ControlRightToLeft then begin
        rct_left  := (*FValHScroll*) 0 ;
        //rct_left  := oParent.ControlWidth - FDifHScroll ;
        rct_right := rct_right - _node.Level * apScale( FLevelIndent ) (*- rct_left*) ;
      end
      else begin
        rct_left  := (*- FValHScroll + *)_node.Level * apScale( FLevelIndent ) ;
      end ;
    end ;

    Result := Rect( rct_left, rct_top, rct_right, rct_top + rct_height ) ;
  end ;

  function TGIS_Legend.GetDragBarRect(
    const _node : TGIS_TreeNode ;
    const _top  : Integer
  ) : TRect ;
  var
    node : TGIS_TreeNode ;
    rct  : TRect ;
    mov  : TGIS_LegendItemData ;
    dd   : TGIS_LegendItemData ;
    mrg  : Integer ;
    gup  : Boolean ;

    function get_first_layernode_rect : TRect ;
    var
      n : TGIS_TreeNode ;
    begin
      Result := rct ;
      if not assigned( node.Parent ) then exit ;

      n := getFirstLayerInGroup( node.Parent ) ;
      if assigned( n ) then
        Result := GetNodeRect( n, False ) ;
    end ;

  begin
    gup := False ;
    if Mode = TGIS_ControlLegendMode.Layers then begin

      if assigned( _node ) then begin
        node := GetMovableNode( _node ) ;
        rct := GetNodeRect( node, False ) ;
        Result := Rect( rct.Left, rct.Top, rct.Right, rct.Top ) ;
      end
      else begin
        rct := GetNodeRect( LastNode, False ) ;
        Result := Rect( rct.Left, rct.Bottom, rct.Right, rct.Bottom ) ;
      end ;

    end
    else begin

      mov := TGIS_LegendItemData( SelectedNode.Data ) ;
      if assigned( _node ) then begin
        node := GetMovableNode( _node ) ;
        dd := TGIS_LegendItemData( node.Data ) ;
        rct := GetNodeRect( node, False ) ;

        if dd.DataType = TGIS_LegendItemType.Group then begin
          mrg := FDefHeight div 3 ;
          if ( mov.DataType = TGIS_LegendItemType.Group ) and
             ( _top < rct.Top + mrg ) then begin
            Result := Rect( rct.Left,  rct.Top,
                            rct.Right, rct.Top ) ;
            gup := True ;
          end
          else
            Result := Rect( rct.Left,  rct.Top + FDefHeight div 2,
                            rct.Right, rct.Top + FDefHeight div 2 )
        end
        else begin
          if mov.DataType = TGIS_LegendItemType.Group then
            rct := get_first_layernode_rect ;
          Result := Rect( rct.Left, rct.Top, rct.Right, rct.Top ) ;
        end ;
      end
      else begin
        if mov.DataType = TGIS_LegendItemType.Layer then begin
          rct := GetNodeRect( SelectedNode, False )
        end
        else
          rct := GetNodeRect( LastNode, False ) ;
        Result := Rect( rct.Left, rct.Bottom, rct.Right, rct.Bottom ) ;
      end ;

    end ;
    bGroupAboveGroup := gup ;
  end ;

  function TGIS_Legend.GetNodeAtAlt(
    const _x : Integer ;
    const _y : Integer
  ) : TGIS_TreeNode ;
  var
    node : TGIS_TreeNode ;
    rct  : TRect ;
    i    : Integer ;

    function deep_at( const _n : TGIS_TreeNode ) : Boolean ;
    var
      ii : Integer ;
      nd : TGIS_TreeNode ;
      dt : TGIS_LegendItemData ;
    begin
      Result := False ;

      rct := GetNodeRect( _n, False ) ;
      if ( _y < rct.Top ) or ( _y > rct.Bottom ) then
        exit ;

      node := _n ;
      Result := True ;

      if not _n.Expanded then
        exit ;

      for ii := 0 to _n.Count - 1 do begin
        nd := _n.Items[ii] ;
        dt := TGIS_LegendItemData( nd.Data ) ;
        if dt.DataType = TGIS_LegendItemType.Empty then
          continue ;
        if deep_at( nd ) then
          break ;
      end ;
    end ;

  begin
    node := nil ;

    if _y < 0 then begin
      Result := FItems[0] ;
      exit ;
    end ;

    for i := 0 to FItems.Count - 1 do begin
      if FItems[i].Level <> 0 then
        continue ;
      if deep_at( FItems[i] ) then
        break ;
    end ;

    Result := node ;
  end ;

  function TGIS_Legend.getExpandRect(
    const _node    : TGIS_TreeNode ;
    const _size    : Boolean  ;
    const _isTouch : Boolean
  ) : TRect ;
  var
    rct : TRect ;
    mrg : Integer ;

    rct_left   : Integer ;
    rct_top    : Integer ;
    rct_width  : Integer ;
    rct_height : Integer ;
  begin
    rct := GetNodeRect( _node, _size ) ;

    if _isTouch then begin
      mrg := Max( 0, apScale( 3 ) +
                     apScale( FExpandRectSize   div 2 ) -
                     apScale( FCheckBoxRectSize div 2 ) ) ;
      (*if  ( oParent.ControlRightToLeft and ( bCustomDraw or _size ) ) then   ///WinForms???*)
      if  oParent.ControlRightToLeft  then
        rct_left := rct.Right - mrg - apScale( FCheckBoxRectSize )
      else
        rct_left := rct.Left + mrg ;
      rct_top    := rct.Top + ( FDefHeight div 2 ) -
                    apScale( FCheckBoxRectSize div 2 ) + apScale( 1 ) ;
      rct_width  := apScale( FCheckBoxRectSize ) ;
      rct_height := apScale( FCheckBoxRectSize ) ;
    end else begin
      (*if  ( ( RightToLeft = RightToLeft.Yes ) and ( bCustomDraw or _size ) ) then  ///WinForms??*)
      if  oParent.ControlRightToLeft  then
        rct_left := rct.Right - apScale( FExpandRectSize + 3 )
      else
        rct_left := rct.Left + apScale( 3 ) ;
      rct_top    := rct.Top + ( FDefHeight div 2 ) -
                    apScale( FExpandRectSize div 2 ) ;
      rct_width  := apScale( FExpandRectSize ) ;
      rct_height := apScale( FExpandRectSize ) ;
    end ;

    Result := Rect( rct_left,  rct_top,
                    rct_left + rct_width,
                    rct_top  + rct_height ) ;
  end ;

  function TGIS_Legend.getCheckBoxRectTop(
    const _nodeType : TGIS_LegendItemType
  ) : Integer ;
  begin
    if _nodeType = TGIS_LegendItemType.Params then
      Result := ( FDefHeight16 div 2 ) - apScale( FCheckBoxRectSize div 2 )
    else
      Result := ( FDefHeight div 2 )   - apScale( FCheckBoxRectSize div 2 ) ;
  end ;

  function TGIS_Legend.getCheckBoxRect(
    const _node : TGIS_TreeNode ;
    const _size : Boolean
  ) : TRect ;
  var
    rct : TRect ;

    rct_left   : Integer ;
    rct_top    : Integer ;
    rct_width  : Integer ;
    rct_height : Integer ;
  begin
    rct := GetNodeRect( _node, _size ) ;

    (*if ( ( RightToLeft = RightToLeft.Yes ) and ( bCustomDraw or _size ) ) then  ///WinForms???*)
    if oParent.ControlRightToLeft then
      rct_left := rct.Right - apScale( FCheckBoxRectSize + 16 )
    else
      rct_left := rct.Left + apScale( 16 ) ;

    rct_top := rct.Top + getCheckBoxRectTop( _node.Data.DataType ) + apScale( 1 ) ;

    rct_width  := apScale( FCheckBoxRectSize ) ;
    rct_height := apScale( FCheckBoxRectSize ) ;

    Result := Rect( rct_left,  rct_top,
                    rct_left + rct_width,
                    rct_top  + rct_height ) ;
  end ;

  function TGIS_Legend.GetMovableNode(
    const _node : TGIS_TreeNode
  ) : TGIS_TreeNode ;
  var
    node : TGIS_TreeNode ;
    dt   : TGIS_LegendItemData ;
  begin
    node := _node ;

    dt := TGIS_LegendItemData( node.Data ) ;
    if dt.DataType = TGIS_LegendItemType.Params then
      node := node.Parent
    else
    if dt.DataType = TGIS_LegendItemType.Empty then
      node := node.Parent.Parent ;

    dt := TGIS_LegendItemData( node.Data ) ;
    if dt.DataType = TGIS_LegendItemType.Layer then begin
      if dt.IsSubLayer then
        node := node.Parent ;
    end ;

    Result := node ;
  end ;

  function TGIS_Legend.IsNodeOnScreen(
    const _node : TGIS_TreeNode ;
    const _size : Boolean
  ) : Boolean ;
  var
    next : TGIS_TreeNode ;
    rct  : TRect   ;
    tmp  : TRect   ;
    h    : Integer ;
  begin
    Result := False ;

    rct := GetNodeRect( _node, _size ) ;

    h := oParent.ControlHeight ;

    if _size then begin
      if rct.Top < h then
        Result := True ;
      exit ;
    end ;

    if rct.Top > h then
      exit ;

    next := _node.GetNextSibling ;
    if assigned( next ) then begin
      tmp := GetNodeRect( next, False ) ;
      if ( rct.Top < h ) and
         ( tmp.Top > 0 ) then
        Result := True ;
    end
    else begin
      if rct.Top < h then
        Result := True ;
    end ;
  end ;

  procedure TGIS_Legend.makeNodeVisible(
    const _node  : TGIS_TreeNode ;
    const _delta : Integer ;
    const _move  : Boolean
  ) ;
  var
    rct : TRect ;
    h   : Integer ;
  begin
    rct := GetNodeRect( _node, false ) ;
    if _delta < 0 then begin
      if rct.Top < 0 then
        FTopOffset := FTopOffset - rct.Top ;
    end ;
    if _delta > 0 then begin
      h := oParent.ControlClientHeight ;
      if rct.Bottom > h + 1 then begin
        if rct.Height >= h then
          FTopOffset := FTopOffset - rct.Top
        else
        if _move then
          FTopOffset := FTopOffset - ( rct.Bottom - h ) + rct.Height
        else
          FTopOffset := FTopOffset - ( rct.Bottom - h )
      end ;
    end ;
  end ;

  procedure TGIS_Legend.makeNodeVisible(
    const _node : TGIS_TreeNode
  ) ;
  var
    dt  : TGIS_LegendItemData ;
    val : Integer ;
  begin
    dt := TGIS_LegendItemData( _node.Data ) ;
    val := dt.NodeTop - FTopOffset - (oParent.ControlClientHeight div 2) ;
    if val > (FLength - oParent.ControlClientHeight) then
      val := FLength - oParent.ControlClientHeight ;
    if val < 0 then
      val := 0 ;
    if not isLocked then
      oParent.ControlSetVScrollPosition( RoundS(val) ) ;
    FTopOffset := - RoundS(val) ;
  end ;

  procedure TGIS_Legend.enableDrag ;
  begin
    canDrag := True ;
    isLongTap := False ;
    isDrag := False ;
    bDragBar := False ;
    bGroupAboveGroup := False ;
  end ;

  procedure TGIS_Legend.disableDrag ;
  begin
    canDrag := False ;
    isLongTap := False ;
    isDrag := False ;
    bDragBar := False ;
    bGroupAboveGroup := False ;
  end ;

  function TGIS_Legend.dragEnabled
    : Boolean ;
  begin
    Result := canDrag ;
  end ;

  function TGIS_Legend.initDrag(
    const _top : Integer
  ) : Boolean ;
  var
    node : TGIS_TreeNode ;
  begin
    Result := False ;

    if not ( opAllowMove and opAllowSelect ) then
      exit ;

    if not dragEnabled then
      exit ;

    node := GetNodeAtAlt( ptDrag.X, ptDrag.Y ) ;

    if not assigned( node ) then
      exit ;

    // be sure that the bar will be drawn on screen
    oParent.ControlDragFreeNode ;

    oParent.ControlDragDrawBar( node, _top ) ;
    bDragBar := true ;
    Result := True ;
  end ;

  procedure TGIS_Legend.DrawNode(
    const _node   : TGIS_TreeNode ;
    const _always : Boolean
  ) ;
  const
    LOCAL_ARR_CHART : array[0..9] of String = (
      '',
      '',
      '::5::::::::',
      ':::5:::::::',
      '::::5::::::',
      ':::::5:::::',
      '::::::5::::',
      ':::::::5:::',
      '::::::::5::',
      ':::::::::5:'
    ) ;
  var
    dt  : TGIS_LegendItemData ;
    rct : TRect   ;
    i   : Integer ;
    lv  : TGIS_LayerVector ;
    asSelected : Boolean ;

    function deep_selected( const _n : TGIS_TreeNode ) : Boolean ;
    var
      bb : Boolean ;
    begin
      bb := _n.Selected ;
      if assigned( _n.Parent ) then
        bb := bb or deep_selected( _n.Parent ) ;
      Result := bb ;
    end ;

    function set_dy( var _h : Integer ) : Integer ;
    var
      h  : Integer ;
      ss : TPoint  ;
    begin
      Result := getParamsTextY( 0, _h ) ;
      if dt.IsVector then
        h := getHeightParams( False )
      else
        h := getHeight16( False, 0 ) ;
      if Result + _h < h then begin
        ss := oParent.ControlStyleGetTextExtent( _node.Selected, 'Wy' ) ;
        _h := ss.Y ;
        if dt.IsVector then
          h := getHeightParams( True )
        else
          h := getHeightPixel ;
        Result := - ( ss.Y - h ) div 2 - apScale( 2 ) ;
      end ;
    end ;

    procedure draw_renderer_zones(
      const _section   : TGIS_ParamsSectionVector ;
      const _l         : Integer ;
      const _lcw       : Integer ;
      var   _t         : Integer ;
      const _dy        : Integer ;
      const _hh        : Integer ;
      const _useZoneEx : Boolean
    ) ;
    var
      legend_txt         : String  ;
      legend_txt_caption : TPoint  ;
      legend_txt_rect    : TRect   ;
      i_zone             : Integer ;
      zones_count        : Integer ;
      zone_index_sign    : Integer ;
    begin
      if _useZoneEx then begin
        zones_count :=  _section.Render.ZonesEx ;
        zone_index_sign := -1 ;
      end
      else begin
        zones_count :=  _section.Render.Zones ;
        zone_index_sign := 1 ;
      end;

      for i_zone := 1 to Abs( zones_count ) do begin
        if oBMPFactory.SetParamsRenderer( lv, _section.InternalIndex,
                                          zone_index_sign * i_zone ) then
        begin
          oParent.ControlStyleDrawImage( oBMPFactory.GetBitmap, _l, _t, false ) ;
          legend_txt := oBMPFactory.Layer.Params.Legend ;
        end
        else begin
          // data cannot be retrived; probably shouldn't happen
          FNoParams := True ;
          legend_txt := '[ no data ]' ;
        end ;
        if oParent.ControlRightToLeft then begin
          legend_txt_caption := oParent.ControlStyleGetTextExtent( _node.Selected, legend_txt ) ;
          if FCompactView then
            legend_txt_rect := Rect( _l - _lcw + FDefBmpSize div 2 - legend_txt_caption.X, _t + _dy,
                                     _l - _lcw + FDefBmpSize div 2, _t + _dy + _hh )
          else
            legend_txt_rect := Rect( _l - _lcw + FDefBmpSize - legend_txt_caption.X, _t + _dy,
                                     _l - _lcw + FDefBmpSize, _t + _dy + _hh ) ;
        end
        else
          legend_txt_rect := Rect( _lcw, _t + _dy, rct.Width, _t + _dy + _hh ) ;

        oParent.ControlStyleDrawText( asSelected, legend_txt, legend_txt_rect ) ;

        inc( _t, getHeightParamsRender ) ;
      end ;
    end ;

    procedure draw_params_vector_renderer(
      const _section : TGIS_ParamsSectionVector ;
      const _l       : Integer ;
      const _lcw     : Integer ;
        var _t       : Integer
    ) ;
    var
      dy : Integer ;
      hh : Integer ;
    begin
      dy := set_dy( hh ) ;

      if (_section.Render.MinVal < _section.Render.MinValEx) and
         (_section.Render.MaxVal < _section.Render.MaxValEx) then
      begin
        // first draw Zones, then ZonesEx
        draw_renderer_zones( _section, _l, _lcw, _t, dy, hh, False ) ;
        draw_renderer_zones( _section, _l, _lcw, _t, dy, hh, True ) ;
      end
      else begin
        // first draw ZonesEx, then Zones (historiacal, deprecated)
        draw_renderer_zones( _section, _l, _lcw, _t, dy, hh, True ) ;
        draw_renderer_zones( _section, _l, _lcw, _t, dy, hh, False ) ;
      end ;
    end ;

    procedure draw_params_vector_charts(
      const _dd      : TGIS_LegendItemData ;
      const _section : TGIS_ParamsSectionVector ;
      const _l       : Integer ;
      const _lcw     : Integer ;
        var _t       : Integer
    ) ;
    var
      dy  : Integer ;
      hh  : Integer ;
      ii  : Integer ;
      txt : String  ;
      ss  : TPoint  ;
      rr  : TRect   ;
    begin
      dy := set_dy( hh ) ;

      for ii := StringFirst to StringLast( _dd.ChartMap ) do begin
        oBMPFactory.Layer.Params.Chart.Style := _section.Chart.Style ;
        if _dd.ChartMap[ii] = '1' then begin
          if _section.Chart.Style = TGIS_ChartStyle.Pie then
            oBMPFactory.Layer.Params.Render.Chart := LOCAL_ARR_CHART[ii-StringFirst]+'3'
          else
            oBMPFactory.Layer.Params.Render.Chart := LOCAL_ARR_CHART[ii-StringFirst] ;
          oParent.ControlStyleDrawImage( oBMPFactory.GetBitmap, _l, _t, false ) ;
          txt := _dd.ChartLegend[ii-StringFirst] ;
          if oParent.ControlRightToLeft then begin
            ss := oParent.ControlStyleGetTextExtent( _node.Selected, txt ) ;
            if FCompactView then
              rr := Rect( _l - _lcw + FDefBmpSize div 2 - ss.X, _t + dy,
                          _l - _lcw + FDefBmpSize div 2, _t + dy + hh )
            else
              rr := Rect( _l - _lcw + FDefBmpSize - ss.X, _t + dy,
                          _l - _lcw + FDefBmpSize, _t + dy + hh ) ;
          end
          else
            rr := Rect( _lcw, _t + dy, rct.Right, _t + dy + hh ) ;
          oParent.ControlStyleDrawText( asSelected, txt, rr ) ;

          inc( _t, getHeightParamsChart ) ;
        end ;
      end ;
    end ;

    procedure draw_params_vector( const _n : TGIS_TreeNode ) ;
    var
      dd   : TGIS_LegendItemData ;
      l0   : Integer ;
      l    : Integer ;
      lcw  : Integer ;
      src  : TGIS_ParamsSectionVector ;
      htxt : Boolean ;
      t    : Integer ;
      t0   : Integer ;
      bb   : Boolean ;
      h    : Integer ;
    begin
      if not isNodeOnScreen( _n, _always ) then
        exit ;

      dd := TGIS_LegendItemData( _n.Data ) ;

      if dd.DataType <> TGIS_LegendItemType.Params then
        exit ;

      if opAllowParamsVisible then begin
        rct := getCheckBoxRect( _n, _always ) ;
        oParent.ControlStyleDrawCheckBox( dd.Checked, rct ) ;
      end ;

      if FCompactView then begin
        l0  := apScale( 4 ) ;
        lcw := FDefBmpSize div 2 + apScale( 9 ) ;
      end
      else begin
        l0  := 0   ;
        lcw := FDefBmpSize + apScale( 5 ) ;
      end ;
      htxt := ( dd.FeatureCount > 1 ) or dd.Render ;

      t := 0 ;
      if dd.RowCount >= 0 then begin

        rct := getParamsRect( _n, _always ) ;
        if ( rct.Width > 0 ) and ( rct.Height > 0 ) then begin

          oParent.ControlStyleCreateTemporaryContext( rct.Width, rct.Height ) ;
          try

            if oParent.ControlRightToLeft then
              l0 := rct.Width - lcw ;

            src := TGIS_ParamsSectionVector( dd.Params ) ;

            oBMPFactory.SetColor( oParent.ControlStyleGetColor ) ;
            if ( length( src.Legend ) <> 0 ) and htxt then
              t0 := getHeightParamsText
            else
              t0 := 0 ;

            t  := t0 ;
            l  := l0 ;
            bb := False ;
            if src.Marker.ShowLegend then begin
              oBMPFactory.IconType := TGIS_LegendIconType.Marker ;
              if dd.RenderMarker then
                draw_params_vector_renderer( src, l, lcw, t )
              else begin
                oBMPFactory.SetParams( src ) ;
                oParent.ControlStyleDrawImage( oBMPFactory.GetBitmap, l, t, false ) ;
                if dd.Render then begin
                  if t0 = 0 then
                    inc( t, getHeightParams( not IsStringEmpty( _n.Text ) ) )
                  else
                    inc( t, getHeightParams( False ) ) ;
                end else begin
                  if oParent.ControlRightToLeft then
                    dec( l, lcw )
                  else
                    inc( l, lcw ) ;
                end ;
                bb := True ;
              end ;
            end ;

            if src.Line.ShowLegend then begin
              oBMPFactory.IconType := TGIS_LegendIconType.Line ;
              if dd.RenderLine then
                draw_params_vector_renderer( src, l, lcw, t )
              else begin
                oBMPFactory.SetParams( src ) ;
                oParent.ControlStyleDrawImage( oBMPFactory.GetBitmap, l, t, false ) ;
                if dd.Render then begin
                  if t0 = 0 then
                    inc( t, getHeightParams( not IsStringEmpty( _n.Text ) ) )
                  else
                    inc( t, getHeightParams( False ) ) ;
                end else begin
                  if oParent.ControlRightToLeft then
                    dec( l, lcw )
                  else
                    inc( l, lcw ) ;
                end ;
                bb := True ;
              end ;
            end ;

            if src.Area.ShowLegend then begin
              oBMPFactory.IconType := TGIS_LegendIconType.Area ;
              if dd.RenderArea then
                draw_params_vector_renderer( src, l, lcw, t )
              else begin
                oBMPFactory.SetParams( src ) ;
                oParent.ControlStyleDrawImage( oBMPFactory.GetBitmap, l, t, false ) ;
                if dd.Render then begin
                  if t0 = 0 then
                    inc( t, getHeightParams( not IsStringEmpty( _n.Text ) ) )
                  else
                    inc( t, getHeightParams( False ) ) ;
                end else begin
                  if oParent.ControlRightToLeft  then
                    dec( l, lcw )
                  else
                    inc( l, lcw ) ;
                end ;
                bb := True ;
              end ;
            end ;

            if src.Labels.ShowLegend then begin
              oBMPFactory.IconType := TGIS_LegendIconType.Label ;
              if dd.RenderLabel then
                draw_params_vector_renderer( src, l, lcw, t )
              else begin
                oBMPFactory.SetParams( src ) ;
                oParent.ControlStyleDrawImage( oBMPFactory.GetBitmap, l, t, false ) ;
                bb := True ;
              end ;
            end ;

            if bb then begin
              if t > 0 then
                inc( t, getHeightParams( False ) )
              else
                inc( t, getHeightParams( not IsStringEmpty( _n.Text ) ) ) ;
            end ;

            if src.Chart.ShowLegend and
               ( not IsStringEmpty( dd.ChartMap ) ) then begin
              oBMPFactory.IconType := TGIS_LegendIconType.Chart ;
              oBMPFactory.Layer.Params.Chart.ColorsInternal := src.Chart.ColorsInternal ;
              draw_params_vector_charts( dd, src, l0, lcw, t ) ;
            end ;

          finally
            oParent.ControlStyleRenderTemporaryContext( rct.Left, rct.Top ) ;
          end ;
        end ;
      end ;

      if not IsStringEmpty( _n.Text ) then begin

        rct := getTextRect( _n, _always, _node.Selected ) ;
        if not htxt then
          t := set_dy( h )
        else
          t := getParamsTextY( getCheckBoxRectTop( TGIS_LegendItemType.Params ), h ) ;
        rct.Height := t + h ;

        if oParent.ControlRightToLeft then
          rct := Rect( rct.Left - lcw, rct.Top + t,
                       rct.Left - lcw + rct.Width,
                       rct.Top  + rct.Height )
        else
          rct := Rect( rct.Left + lcw, rct.Top + t,
                       rct.Left + lcw + rct.Width,
                       rct.Top  + rct.Height ) ;
        oParent.ControlStyleDrawText( asSelected, _n.Text, rct ) ;
      end ;
    end ;

    procedure draw_params_pixel( const _n : TGIS_TreeNode ) ;
    var
      ppix  : TGIS_ParamsPixel ;
      tkn   : TGIS_Tokenizer ;
      gcl   : TGIS_Color ;
      dd    : TGIS_LegendItemData ;
      rr    : TRect   ;
      ii    : Integer ;
      jj    : Integer ;
      ss    : TPoint  ;
      dy    : Integer ;
      ident : Integer ;
      hh    : Integer ;
    begin
      if not isNodeOnScreen( _n, _always ) then
        exit ;

      dd := TGIS_LegendItemData( _n.Data ) ;

      if dd.DataType <> TGIS_LegendItemType.Params then
        exit ;

      if ( dd.RowCount = 0 ) and ( dd.FeatureCount = 0 ) then
        exit ;

      rct := getParamsRect( _n, _always ) ;

      if ( rct.Width < 0 ) or ( rct.Height < 0 ) then
        exit ;

      ident := apScale( 32 ) ;
      dy    := set_dy( hh ) ;

      ppix := TGIS_ParamsSectionPixel( dd.Params ).Pixel ;

      if ppix.ShowLegend then begin
        if assigned( ppix.LegendImage ) then begin
          if assigned( ppix.LegendImage.NativeBitmap ) then begin
            if oParent.ControlRightToLeft then
              oParent.ControlStyleDrawImage( ppix.LegendImage.NativeBitmap,
                                             rct.Right - ppix.LegendImage.Width,
                                             rct.Top,
                                             False )
            else
              oParent.ControlStyleDrawImage( ppix.LegendImage.NativeBitmap,
                                             rct.Left,
                                             rct.Top,
                                             False ) ;
          end ;
        end
        else begin

          jj := 0 ;
          tkn := TGIS_Tokenizer.Create ;
          try
            for ii := 0 to ppix.AltitudeMapZones.Count - 1 do begin
              tkn.ExecuteEx( ppix.AltitudeMapZones[ii] ) ;

              if tkn.Result.Count < 4 then
                continue ;

              gcl := ParamColor( tkn.Result.Strings[2], TGIS_Color.None ) ;

              ss := oParent.ControlStyleGetTextExtent(
                      _node.Selected, tkn.Result.Strings[3]
                    ) ;

              if jj = 0 then
                rr := getTextRect( _n, _always,
                                   _node.Selected, tkn.Result.Strings[3] )
              else
                rr := getTextRect( _n.Items[jj-1], _always,
                                   _node.Selected, tkn.Result.Strings[3] ) ;

              if oParent.ControlRightToLeft then
                oParent.ControlStyleDrawRectangle( rct.Right-FDefBmpSize, rr.Top,
                                                   FDefBmpSize, FDefBmpSize div 2,
                                                   gcl, TGIS_Color.Black )
              else
                oParent.ControlStyleDrawRectangle( rct.Left, rr.Top,
                                                   FDefBmpSize, FDefBmpSize div 2,
                                                   gcl, TGIS_Color.Black ) ;

              if oParent.ControlRightToLeft then
                rr := Rect( rct.Right - ident - 1 - ss.X,
                            rr.Top + dy,
                            rct.Right - ident - 1,
                            rr.Top + dy + ss.Y )
              else
                rr := Rect( rct.Left + ident,
                            rr.Top + dy,
                            rr.Right,
                            rr.Top + dy + ss.Y ) ;

              oParent.ControlStyleDrawText( asSelected,
                                            tkn.Result.Strings[3], rr ) ;

              inc( jj ) ;
            end ;
          finally
            FreeObject( tkn ) ;
          end ;
        end ;
      end ;
    end ;

  begin
    dt := TGIS_LegendItemData( _node.Data ) ;

    if ( dt.DataType = TGIS_LegendItemType.Params ) or
       ( dt.DataType = TGIS_LegendItemType.Empty  ) then
      exit ;

    calcNodeHeight( _node ) ;
    if _node.Data.NodeHeight = 0 then
      exit ;

    if ( not _always ) and ( not isNodeOnScreen( _node, False ) ) then
      exit ;

    if ( dt.DataType = TGIS_LegendItemType.Layer ) then begin
      if dt.IsSubLayer and ( not _node.Parent.Expanded ) then
        exit ;
    end ;

    // check if the node will be drawn as selected
    asSelected := deep_selected( _node ) ;

    // set colors for selected or not selected
    oParent.ControlStyleSetColor( asSelected ) ;

    // if selected prepare and draw layer node's frame
    if _node.Selected then begin
      rct := GetNodeRect( _node, _always ) ;
      rct := Rect( 0, rct.Top, rct.Right, rct.Bottom ) ;

      (*if b then begin         ///VCL ???
        _bmp.PixelFormat := TPixelFormat.pf24bit ;
        if rct.Height > Height then
          rct.Height := Height ;
        _bmp.SetSize( rct.Width, rct.Height ) ;
      end ;*)
      (*  rct.Height := rct.Height - 1 ;    ///WinForms ???

        if ( RightToLeft = RightToLeft.Yes ) and bCustomDraw then
          rct.X := rct.Left - oLegend.ValHScroll ;*)
      oParent.ControlStyleDrawRectangle( rct ) ;
    end ;

    // draw checkbox
    if opAllowActive then begin
      rct := getCheckBoxRect( _node, _always ) ;
      oParent.ControlStyleDrawCheckBox( dt.Checked, rct ) ;
    end ;

    // draw expand/collapse marker
    if opAllowExpand then begin
      rct := getExpandRect( _node, _always, false ) ;
      if _node.Count > 0 then
        oParent.ControlStyleDrawExpandCollapseMarker( _node.Expanded, rct ) ;
    end ;

    // draw caption
    rct := getTextRect( _node, _always, _node.Selected ) ;
    oParent.ControlStyleDrawText( asSelected, _node.Text, rct ) ;

    // draw sections' nodes
    if dt.DataType = TGIS_LegendItemType.Layer then begin
      if _node.Expanded then begin
        if dt.IsVector then begin
          lv := TGIS_LayerVector( dt.Layer ) ;
          for i := 0 to _node.Count - 1 do
            draw_params_vector( _node.Items[i] ) ;
        end
        else begin
          for i := 0 to _node.Count - 1 do
            draw_params_pixel( _node.Items[i] ) ;
        end ;
      end ;
    end ;

    // draw groups' elements
    if {b  and ??} ( dt.DataType = TGIS_LegendItemType.Group ) then begin
      if _node.Expanded then begin
        for i := 0 to _node.Count - 1 do
          DrawNode( _node.Items[i], _always ) ;
      end ;
    end ;

    // draw sublayers' elements
    // ??
    if {b and ??} ( dt.DataType = TGIS_LegendItemType.Layer ) then begin
      if _node.Expanded then begin
        for i := 0 to _node.Count - 1 do
          DrawNode( _node.Items[i], _always ) ;
      end ;
    end ;

  end ;

  function TGIS_Legend.DrawNodes
    : Boolean ;
  var
    ytmp : Integer ;
    i : Integer ;
    nd   : TGIS_TreeNode ;
    dt   : TGIS_LegendItemData ;

    function deep_expanded( const _n : TGIS_TreeNode ) : Boolean ;
    var
      bb : Boolean ;
    begin
      Result := True ;
      if not assigned( _n ) then
        exit ;

      bb := _n.Expanded ;
      if assigned( _n.Parent ) then
        bb := bb and deep_expanded( _n.Parent ) ;
      Result := bb ;
    end ;

  begin
    result := false ;

    if oParent.ControlIsDesignMode then
      exit ;

    if isLocked then
      exit ;

    // data not up to date
    if FNoDataInfo then
      exit ;

    result := true ;

    if not assigned( FViewer ) then
      exit ;

    oBMPFactory.CustomPPI := FViewer.PPI ;

    ytmp := 0 ;
    if FMode = TGIS_ControlLegendMode.Layers then begin
      if assigned( FItems ) then begin
        for i := 0 to FItems.Count - 1 do begin
          nd := FItems[i] ;
          dt := TGIS_LegendItemData( nd.Data ) ;
          if not assigned( dt ) then
            continue ;
          if dt.DataType <> TGIS_LegendItemType.Layer then
            continue ;

          FNoParams := False ;
          oParent.ControlDrawNode( nd ) ;
          if FNoParams then
            // drawing failed
            Result := False ;

          if nd.Expanded then
            ytmp := dt.NodeTop + dt.NodeHeight
          else
            ytmp := dt.NodeTop + getHeightStd ;
        end ;
      end ;
    end
    else begin
      if assigned( FItems ) then begin
        for i := 0 to FItems.Count - 1 do begin
          nd := FItems[i] ;
          dt := TGIS_LegendItemData( nd.Data ) ;
          if not assigned( dt ) then
            continue ;
          if ( dt.DataType <> TGIS_LegendItemType.Params ) and
             ( dt.DataType <> TGIS_LegendItemType.Empty ) and
             ( not deep_expanded( nd.Parent ) ) then
            continue ;

          FNoParams := False ;
          oParent.ControlDrawNode( nd ) ;
          if FNoParams then
            // drawing failed
            Result := False ;

          if nd.Expanded then
            ytmp := dt.NodeTop + dt.NodeHeight
          else
            ytmp := dt.NodeTop + getHeightStd ;
        end ;
      end ;
    end ;
    FLength := ytmp - FTopOffset ;
    msgScrollUpdate ;
  end ;

  function TGIS_Legend.forceExpanded(
    const _node    : TGIS_TreeNode ;
    const _expand  : Boolean
  ) : Boolean ;
  var
    allow : Boolean ;
    exp   : Boolean ;
  begin
    Result := False ;
    canExpand := True ;
    doExpanding( Self, _node, allow ) ;
    if allow then begin
      exp := _node.Expanded ;
      if _node.Data.DataType = TGIS_LegendItemType.Layer then begin
        if assigned( _node.Data.Layer ) then
          _node.Data.Layer.Collapsed := not _expand
      end
      else if _node.Data.DataType = TGIS_LegendItemType.Group then begin
        if assigned( _node.Data.Group ) then
          _node.Data.Group.Collapsed := not _expand ;
      end;
      _node.Expanded := _expand ;
      if exp <> _node.Expanded then
        Result := True ;
    end ;
    canExpand := False ;
  end ;

  procedure TGIS_Legend.moveNode(
    const _src : Integer ;
    const _dst : Integer
  ) ;
  {$IFDEF OXYGENE}
    var
      n : TGIS_TreeNode ;
  {$ENDIF}
  begin
    {$IFDEF OXYGENE}
      n := FItems[ _src ] ;
      FItems.RemoveAt( _src ) ;
      FItems.Insert( _dst, n ) ;
      SelectedNode := FItems[ _dst ] ;
    {$ELSE}
      FItems.Move( _src, _dst ) ;
    {$ENDIF}
  end ;

  procedure TGIS_Legend.safeUpdate ;
  begin
    if oParent.ControlIsDesignMode then
      exit ;

    if not assigned( FViewer ) then
      exit ;

    if GIS_Viewer.IsEmpty then
      exit ;

    if isLocked then
      exit ;

    oParent.ControlUpdate ;
  end ;

  procedure TGIS_Legend.safeRepaint ;
  begin
    if isLocked then
      exit ;

    oParent.ControlRepaint ;
  end ;

  function  TGIS_Legend.IsUpdateLocked
    : Boolean ;
  begin
    result := skipUpdate ;
  end ;

  procedure TGIS_Legend.DoUpdate(
    const _redraw : Boolean
  ) ;
  begin
    if FixDataConsistency or _redraw or FNoParams or FBasemap then
      safeRepaint ;
  end ;

  procedure TGIS_Legend.InvalidateItems ;
  begin
    lastHash := 0 ;
    safeUpdate ;
  end ;

  function TGIS_Legend.FixDataConsistency
    : Boolean ;
  var
    hashChange : Boolean ;
  begin
    Result := False ;
    if not assigned( GIS_Viewer ) then exit ;

    hashChange := ( lastHash <> GIS_Viewer.ChangeHash ) or
                  ( Abs( GIS_Viewer.Zoom - fOldZoom ) > GIS_Viewer.Zoom*1e-5 ) ;
    lastHash   := GIS_Viewer.ChangeHash ;
    fOldZoom   := GIS_Viewer.Zoom ;

    if lastHash = 0 then
      FTopOffset := 0 ;

    if hashChange then begin
      FNoDataInfo := False ;
      recreateTree( True ) ;
      //if the recreating is upon mouse operation then disable dragging
      if isMouseDn then
        disableDrag ;
      Result := True ;
    end
    else if FBasemap and FNoDataInfo then begin
      //subscribe from the basemap thread and data is not up to date
      FNoDataInfo := False ;
      recreateTree( True ) ;
      if isMouseDn then
        disableDrag ;
      Result := True ;
    end ;
  end ;

  procedure TGIS_Legend.MouseDown(
    const _x       : Double  ;
    const _y       : Double  ;
    const _isTouch : Boolean ;
    var   _action  : Boolean
  ) ;
  var
    update : Boolean ;
    x, y   : Integer ;
    node   : TGIS_TreeNode ;
    dt     : TGIS_LegendItemData ;
    dtt    : TGIS_LegendItemData ;

    function isMouseInRect(
      _rct : TRect
    ) : Boolean ;
    begin
      if ( x >= _rct.Left ) and ( x < _rct.Right  ) and
         ( y >= _rct.Top  ) and ( y < _rct.Bottom ) then
        Result := True
      else
        Result := False ;
    end ;

    procedure deep_checked( const _n : TGIS_TreeNode ; const _b : Boolean ) ;
    var
      dd : TGIS_LegendItemData ;
      ii : Integer ;
      change : Boolean ;
    begin
      dd := TGIS_LegendItemData( _n.Data ) ;
      change := ( dd.Checked <> _b ) ;
      dd.Checked := _b ;

      if dd.DataType = TGIS_LegendItemType.Layer then begin
        if change and assigned( FLayerActiveChangeEvent ) then
          {$IFDEF OXYGENE}
            FLayerActiveChangeEvent( Self, new TGIS_LayerEventArgs( dd.Layer ) ) ;
          {$ELSE}
            FLayerActiveChangeEvent( Self, dd.Layer ) ;
          {$ENDIF}
      end ;

      if dd.DataType = TGIS_LegendItemType.Group then begin
        if assigned( dt.Group ) then
          dt.Group.Active := dd.Checked ;
        if change and assigned( FGroupActiveChangeEvent ) then
          {$IFDEF OXYGENE}
            FGroupActiveChangeEvent( Self, new TGIS_HierarchyGroupEventArgs( dd.Group ) ) ;
          {$ELSE}
            FGroupActiveChangeEvent( Self, dd.Group ) ;
          {$ENDIF}
      end ;

      for ii := 0 to _n.Count - 1 do begin
        dd := TGIS_LegendItemData( _n.Items[ii].Data ) ;
        if ( dd.DataType = TGIS_LegendItemType.Layer ) or
           ( dd.DataType = TGIS_LegendItemType.Group ) then
          deep_checked( _n.Items[ii], _b ) ;
      end ;
    end ;

    procedure up_checked( const _n : TGIS_TreeNode ) ;
    var
      dd : TGIS_LegendItemData ;
      change : Boolean ;
    begin
      if not assigned( _n.Parent ) then
        exit ;

      dd := TGIS_LegendItemData( _n.Parent.Data ) ;
      change := not dd.Checked ;
      dd.Checked := True ;

      if dd.DataType = TGIS_LegendItemType.Group then begin
        if assigned( dd.Group ) then
          dd.Group.Active := dd.Checked ;
        if change and assigned( FGroupActiveChangeEvent ) then
          {$IFDEF OXYGENE}
            FGroupActiveChangeEvent( Self, new TGIS_HierarchyGroupEventArgs( dd.Group ) ) ;
          {$ELSE}
            FGroupActiveChangeEvent( Self, dd.Group ) ;
          {$ENDIF}
      end ;

      up_checked( _n.Parent ) ;
    end ;

    procedure activate_desactivate_node ;
    var
      rct : TRect ;
    begin
      if dt.DataType = TGIS_LegendItemType.Empty then
        exit ;

      rct := getCheckBoxRect( node, False ) ;
      if dt.DataType = TGIS_LegendItemType.Params then begin
        if opAllowParamsVisible and
           isMouseInRect( rct ) then begin
          _action := True ;
          skipUpdate := True ;
          try
            dt.Checked := not dt.Checked ;
          finally
            skipUpdate := False ;
          end;
          update := True ;
          if assigned( FLayerParamsChangeEvent ) then begin
            dtt := TGIS_LegendItemData( node.Parent.Data ) ;
            if assigned( dtt ) then
              {$IFDEF OXYGENE}
                FLayerParamsChangeEvent( Self, new TGIS_LayerEventArgs( dtt.Layer ) ) ;
              {$ELSE}
                FLayerParamsChangeEvent( Self, dtt.Layer ) ;
              {$ENDIF}
          end ;
        end ;
      end
      else begin
        if opAllowActive and
           isMouseInRect( rct ) then begin
          _action := True ;
          skipUpdate := True ;
          try
            deep_checked( node, not dt.Checked ) ;
            if dt.Checked then
              up_checked( node ) ;
          finally
            skipUpdate := False ;
          end;
          update := True ;
        end ;
      end ;
    end ;

    procedure expand_collapse_node ;
    var
      rct : TRect ;
    begin
      if ( dt.DataType = TGIS_LegendItemType.Empty ) or
         ( dt.DataType = TGIS_LegendItemType.Params ) then
        exit ;

      rct := getExpandRect( node, False, _isTouch ) ;
      if isMouseInRect( rct ) then begin
        _action := True ;

        forceExpanded( node, not node.Expanded ) ;

        if oParent.ControlRightToLeft then begin
          FMaxHScroll := oParent.ControlWidth ;
          oParent.ControlSetHScrollPosition( FMaxHScroll ) ;
          FValHScroll := oParent.ControlGetHScrollPosition ;
          FMaxHScroll := oParent.ControlWidth + FValHScroll ;
          FDifHScroll := FMaxHScroll - FValHScroll ;
        end ;

        if dt.DataType = TGIS_LegendItemType.Group then
          if assigned( dt.Group ) then
            dt.Group.Collapsed := not node.Expanded ;
      end ;
    end ;

    procedure set_new_selected( _exists : Boolean ) ;
    begin
      if not _exists then begin
        sNewSelected := '' ;
        exit ;
      end;

      if dt.DataType = TGIS_LegendItemType.Params then
        sNewSelected := node.Parent.Text
      else
      if dt.DataType = TGIS_LegendItemType.Empty then
        sNewSelected := node.Parent.Parent.Text
      else
        sNewSelected := node.Text ;
    end ;

  begin
    update  := false ;
    _action := false ;

    x := RoundS( _x ) ;
    y := RoundS( _y ) ;

    isMouseDn := True ;
    isTouch   := _isTouch ;
    enableDrag ;

    try
      if isLocked then exit ;
      // the line commented; created problems for touches
      //if assigned( GIS_Viewer ) and GIS_Viewer.IsBusy then exit ;

      ptDrag.X := x ;
      ptDrag.Y := y ;

      node := GetNodeAtAlt( x, y ) ;

      if not assigned( node ) then
        exit ;

      dt := TGIS_LegendItemData( node.Data ) ;

      if skipSelect then
        skipSelect := False ;

      activate_desactivate_node ;
      expand_collapse_node ;

      if _action or not isTouch then begin
        set_new_selected ( False ) ;
        SelectedNode := node ;
        if assigned( SelectedNode ) then
          // selectedFlag will be True if opAllowSelect = True
          selectedFlag := SelectedNode.Selected
        else
          selectedFlag := False ;
      end
      else
        // touches: touch down does not select the node
        // this is to be considered later
        set_new_selected( True ) ;

    finally
      if _action then
        disableDrag ;
      oParent.ControlFullUpdate( update ) ;
    end ;
  end ;

  procedure TGIS_Legend.MouseMove(
    const _x       : Double ;
    const _y       : Double ;
    const _leftBtn : Boolean
  ) ;
  var
    x, y : Integer ;

    procedure scroll_window ;
    begin
    end ;

    procedure drag_node ;
    var
      node : TGIS_TreeNode ;
      rct  : TRect ;
      mrg  : Integer ;
    begin
      if not ( opAllowMove and opAllowSelect ) then
        exit ;

      if not _leftBtn then begin
        if isDrag then begin
          isDrag := False ;
          oParent.ControlDragFreeNode ;
        end ;
        exit ;
      end ;

      if not IsStringEmpty( sNewSelected ) then begin
        SelectedNode := findNodeByName( sNewSelected ) ;
        if assigned( SelectedNode ) then
          // selectedFlag will be True if opAllowSelect = True
          selectedFlag := SelectedNode.Selected
        else
          selectedFlag := False ;
        sNewSelected := '' ;
        oParent.ControlRepaint ;
      end ;

      if not isDrag then begin
        // start dragging

        if not assigned( SelectedNode ) then
          exit ;
        if TGIS_LegendItemData( SelectedNode.Data ).IsSubLayer then
          exit ;
        if not dragEnabled then
          exit ;
        if ( not isTouch and FOldStyleDrag and not isLongTap ) then
          // old style requires long tap to process draging
          exit ;

        node := GetNodeAtAlt( ptDrag.X, ptDrag.Y ) ;

        mrg := RoundS( 4 * (PPI / 96 ) ) ;
        if assigned( node ) and
           ( ( Abs( ptDrag.X - x ) > mrg ) or
             ( Abs( ptDrag.Y - y ) > mrg )
           ) then begin

          if not initDrag( y ) then
            exit ;

          dragNode := node ;

          TGIS_LegendItemData( SelectedNode.Data ).IsMoving := True ;
          moveLevel := SelectedNode.Level ;
          moveTop := GetNodeRect( SelectedNode, false ).Top ;
          oParent.ControlDragPrepareNode( SelectedNode ) ;
          TGIS_LegendItemData( SelectedNode.Data ).IsMoving := False ;

          isDrag := True ;
        end ;

      end
      else begin

        oParent.ControlDragCreateTemporaryContext ;
        try

          oParent.ControlDrawFromCache ;

          node := GetNodeAtAlt( x, y ) ;
          if not assigned( node ) then begin
            if y > 0 then
              node := nil
            else
              node := dragNode ;
          end
          else
            dragNode := node ;

          oParent.ControlDragDrawBar( node, y ) ;
          bDragBar := False ;

          rct := GetNodeRect( SelectedNode, False ) ;
          oParent.ControlDragDrawNode( rct.Left, y, 192 / 255 ) ;

        finally
          oParent.ControlDragRenderTemporaryContext ;
        end ;

      end ;
    end ;

  begin
    x := RoundS( _x ) ;
    y := RoundS( _y ) ;

    if isTouch and ( not isLongTap ) then
      scroll_window
    else
      drag_node ;
  end ;

  procedure TGIS_Legend.MouseUp(
    const _x : Double ;
    const _y : Double
  ) ;
  var
    update : Boolean ;
    x, y   : Integer ;
    node   : TGIS_TreeNode ;
    snode  : TGIS_TreeNode ;
    tnode  : TGIS_TreeNode ;
    dt     : TGIS_LegendItemData ;
    b      : Boolean ;

    procedure drag_layers ;
    var
      ds : TGIS_LegendItemData ;
      dd : TGIS_LegendItemData ;
      su : Integer ;
      sd : Integer ;
      id : Integer ;
      sn : TGIS_TreeNode ;
    begin
      su := 0 ;
      sd := 0 ;
      if FReverseOrder then
        su := 1
      else
        sd := 1 ;

      sn := SelectedNode ;
      if assigned( node ) then begin
        node := GetMovableNode( node ) ;

        if ( sn = FLastNode ) and ( node <> FLastNode ) then
          FLastNode := sn.GetPrevSibling ;

        if node <> sn then begin
          ds := TGIS_LegendItemData( sn.Data ) ;
          dd := TGIS_LegendItemData( node.Data ) ;
          if ds.Layer.ZOrder < dd.Layer.ZOrder then begin
            ds.Layer.ZOrder := dd.Layer.ZOrder - sd ;
            id := FItems.IndexOf( node ) - sd ;
          end
          else begin
            ds.Layer.ZOrder := dd.Layer.ZOrder + su ;
            id := FItems.IndexOf( node ) - su ;
          end ;

          if id < 0 then
            id := 0
          else
          if id >= FItems.Count then
            id := FItems.Count - 1 ;

          moveNode( FItems.IndexOf( sn ), id ) ;

          update := True ;
          if assigned( FOrderChangeEvent ) then
            {$IFDEF CLR}
              FOrderChangeEvent( Self, EventArgs.create ) ;
            {$ELSE}
              FOrderChangeEvent( Self ) ;
            {$ENDIF}
        end ;
      end
      else begin
        if sn <> FLastNode then begin
          if assigned( FLastNode ) then
            TGIS_LegendItemData( sn.Data ).Layer.ZOrder :=
              TGIS_LegendItemData( FLastNode.Data ).Layer.ZOrder
          else
            TGIS_LegendItemData( sn.Data ).Layer.ZOrder := GIS_MAX_INTEGER ;

          moveNode( FItems.IndexOf( sn ), FLastNode.Index ) ;
          FLastNode := sn ;

          update := True ;
          if assigned( FOrderChangeEvent ) then
            {$IFDEF CLR}
              FOrderChangeEvent( Self, EventArgs.create ) ;
            {$ELSE}
              FOrderChangeEvent( Self ) ;
            {$ENDIF}
        end ;
      end ;
    end ;

    procedure drag_groups ;
    var
      dd : TGIS_LegendItemData ;
      ds : TGIS_LegendItemData ;
      sn : TGIS_TreeNode ;
      n  : TGIS_TreeNode ;
    begin
      sn := SelectedNode ;
      if assigned( node ) then begin
        node := GetMovableNode( node ) ;

        if ( sn = FLastNode ) and ( node <> FLastNode ) then
          FLastNode := sn.GetPrevSibling ;

        if node <> sn then begin
          if assigned( sn.Parent ) then begin
            sn.Parent.Remove( sn ) ;
          end
          else begin
            FItems.Remove( sn ) ;
          end ;

          dd := TGIS_LegendItemData( node.Data ) ;
          ds := TGIS_LegendItemData( sn.Data ) ;
          if dd.DataType = TGIS_LegendItemType.Group then begin
            if ds.DataType = TGIS_LegendItemType.Group then begin
              if bGroupAboveGroup then begin
                // insert group above group
                if assigned( node.Parent ) then
                  node.Parent.AppendNode( sn, node.Index )
                else
                  FItems.InsertChild( node.Index, sn ) ;
              end
              else begin
                n := getFirstLayerInGroup( node ) ;
                if assigned( n ) then
                  node.AppendNode( sn, n.Index )
                else
                  node.AppendNode( sn, -1 ) ;
              end ;
            end
            else
              node.AppendNode( sn, -1 )
          end
          else if assigned( node.Parent ) then begin
            if ds.DataType = TGIS_LegendItemType.Group then begin
              n := getFirstLayerInGroup( node.Parent ) ;
              // there must be a group
              node.Parent.AppendNode( sn, n.Index )
            end
            else
              node.Parent.AppendNode( sn, node.Index ) ;
          end;

          update := True ;
        end ;
      end
      else begin
        if ( dt.DataType = TGIS_LegendItemType.Group ) and
           ( sn <> FLastNode ) then begin
          if assigned( sn.Parent ) then
            sn.Parent.Remove( sn )
          else
            FItems.Remove( sn ) ;

          update := True ;
          FItems.AppendChild( sn ) ;
          FLastNode := sn ;
        end ;
      end ;
    end ;

  begin
    update := false ;

    x := RoundS( _x ) ;
    y := RoundS( _y ) ;


    if not isDrag then begin
      if not IsStringEmpty( sNewSelected ) and ( not isMove ) then begin
        SelectedNode := findNodeByName( sNewSelected ) ;
        if assigned( SelectedNode ) then
          // selectedFlag will be True if opAllowSelect = True
          selectedFlag := SelectedNode.Selected
        else
          selectedFlag := False ;
        oParent.ControlRepaint ;
      end ;
      sNewSelected := '' ;
      if bDragBar then
        oParent.ControlDrawFromCache ;
      disableDrag ;
      isMouseDn := False ;
      isTouch := false ;
      exit ;
    end ;

    lock ;
    try
      node := GetNodeAtAlt( x, y ) ;

      snode := SelectedNode ;
      dt := TGIS_LegendItemData( snode.Data ) ;
      b := snode.Expanded ;

      if dt.DataType = TGIS_LegendItemType.Group then begin
        tnode := node ;
        if assigned( tnode ) then begin
          while True do begin
            if tnode.Parent = snode then
              exit ;
            if tnode.Level = 0 then
              break ;
            tnode := tnode.Parent ;
          end ;
        end ;
      end ;

      FItems.BeginUpdate ;
      try
        if FMode = TGIS_ControlLegendMode.Layers then
          drag_layers
        else
          drag_groups ;
      finally
        FItems.EndUpdate ;
      end ;

      if FMode = TGIS_ControlLegendMode.Groups then
        buildHierarchy ;

      canExpand := True ;
      snode.Expanded := b ;
      canExpand := False ;
    finally
      unlock ;
      oParent.ControlDragFreeNode ;
      disableDrag ;
      isMouseDn := False ;
      isTouch := false ;
      oParent.ControlFullUpdate( update ) ;
    end ;
  end ;

  procedure TGIS_Legend.MouseWheel(
    const _delta : Integer
  ) ;
  var
    new_val : Integer ;
    old_val : Integer ;
  begin
    old_val := oParent.ControlGetVScrollPosition ;
    new_val := old_val - _delta ;

    // scroll window
    if new_val > (FLength - oParent.ControlClientHeight) then
    new_val := FLength - oParent.ControlClientHeight ;
    if new_val < 0 then
    new_val := 0 ;

    TopOffset := - new_val ;
  end ;

  procedure TGIS_Legend.MouseLeave ;
  begin
    if not isDrag then
      exit ;
    oParent.ControlDrawFromCache ;
  end ;

  procedure TGIS_Legend.TouchDown(
    const _x       : Double ;
    const _y       : Double ;
    var   _handled : Boolean
  ) ;
  begin
    isMove := False ;
    ptTouch.X := RoundS( _x ) ;
    ptTouch.Y := RoundS( _y ) ;
    MouseDown( _x, _y, True, _handled ) ;
  end ;

  procedure TGIS_Legend.TouchMove(
    const _x        : Double ;
    const _y        : Double ;
    const _movement : Boolean
  ) ;
  begin
    if (     FDirectTouchLongTap and not _movement ) or
       ( not FDirectTouchLongTap and not _movement and not isLongTap ) then
      // no action
      exit ;
    if isMove or ( ( not isDrag ) and ( not isLongTap ) ) then
      ScrollWindow( _x, _y ) ;
    if not isMove then begin
      MouseMove( _x, _y, True ) ;
      ptTouch.X := RoundS( _x ) ;
      ptTouch.Y := RoundS( _y ) ;
    end ;
  end ;

  procedure TGIS_Legend.TouchUp(
    const _x : Double ;
    const _y : Double
  ) ;
  begin
    MouseUp( _x, _y ) ;
    isMove := False ;
  end ;

  procedure TGIS_Legend.ScrollWindow(
    const _x : Double ;
    const _y : Double
  ) ;
  var
    x, y : Integer ;
    h    : Integer ;
    h1   : Integer ;

    function test_scroll_down( _old, _new, _h : Integer ) : Integer ;
    begin
      Result := ptTouch.Y - y ;
      if _h + Result > (LegendLength - oParent.ControlClientHeight) then
        Result := LegendLength - oParent.ControlClientHeight - _h ;
    end ;

    function test_scroll_up( _old, _new, _h : Integer ) : Integer ;
    begin
      Result := _new - _old ;
      if _h - result < 0 then
        Result := _h ;
    end ;

  begin
    h := oParent.ControlGetVScrollPosition ;

    x := RoundS( _x ) ;
    y := RoundS( _y ) ;

    if ( y < 0 ) then y := 0 ;
    if ( y > oParent.ControlHeight-1 ) then y := oParent.ControlHeight-1 ;

    if ptTouch.Y - y > 2 then begin
      // scroll down
      isMove := True ;
      h1 := test_scroll_down( ptTouch.Y, y, h ) ;
      if h1 > 0 then begin
        ptTouch.X := x ;
        ptTouch.Y := y ;
        oParent.ControlSetVScrollPosition( h + h1 ) ;
        TopOffset := - ( h + h1 ) ;
      end ;
    end else if y - ptTouch.Y > 2 then begin
      // scroll up
      isMove := True ;
      h1 := test_scroll_up( ptTouch.Y, y, h ) ;
      if h1 > 0 then begin
        ptTouch.X := x ;
        ptTouch.Y := y ;
        oParent.ControlSetVScrollPosition( h - h1 ) ;
        TopOffset := - ( h - h1 ) ;
      end ;
    end ;
  end;

  procedure TGIS_Legend.TapSimple(
    const _leftBtn : Boolean
  ) ;
  begin
    bTapLeft := _leftBtn ;
  end ;

  procedure TGIS_Legend.TapDouble(
    const _leftBtn : Boolean ;
    var   _action  : Boolean
  ) ;
  begin
    _action := false ;

    isMouseDn := False ;

    if not ( bTapLeft and _leftBtn ) then
      exit ;
    if not _leftBtn then
      exit ;

    _action := true ;
  end ;

  procedure TGIS_Legend.TapLong(
    const _leftBtn : Boolean
  ) ;
  begin
    if not ( _leftBtn ) then
      exit ;
    isLongTap := True ;
    if isTouch and FDirectTouchLongTap then
      // when long tap is involved directly then select the node
      MouseMove( ptTouch.X, ptTouch.Y, True ) ;
  end ;

  procedure TGIS_Legend.DoMove(
    const _step : Integer
  ) ;
  var
    dt : TGIS_LegendItemData ;
    ii : Integer ;

    function calculate_step : Integer ;
    var
      start_idx : Integer ;
      im        : Integer ;
      ll        : TGIS_Layer ;
      ii        : Integer ;
    begin
      if FReverseOrder then
        im := -1
      else
        im := 1 ;
      Result := im * _step ;

      if opShowHiddenLayers then exit ;

      if _step > 0 then
        start_idx := GIS_Viewer.Items.IndexOf( dt.Layer ) - im
      else
        start_idx := GIS_Viewer.Items.IndexOf( dt.Layer ) + im ;

      if ( ( _step > 0 ) and ( not FReverseOrder ) ) or
         ( ( _step < 0 ) and (     FReverseOrder ) ) then begin
        for ii := start_idx downto 0 do begin
          ll := TGIS_Layer(GIS_Viewer.Items[ii]) ;
          if not ll.HideFromLegend then
            break ;
          inc( Result ) ;
        end ;
      end
      else begin
        for ii := start_idx to GIS_Viewer.Items.Count - 1 do begin
          ll := TGIS_Layer(GIS_Viewer.Items[ii]) ;
          if not ll.HideFromLegend then
            break ;
          dec( Result ) ;
        end ;
      end ;
    end ;

  begin
    if ( not assigned( GIS_Viewer ) ) or ( GIS_Viewer.Items.Count <= 0 ) then
      exit ;

    if not assigned( SelectedNode ) then
      exit ;

    if Mode <> TGIS_ControlLegendMode.Layers then
      exit ;

    if SelectedNode.Level <> 0 then
      exit ;

    dt := TGIS_LegendItemData( SelectedNode.Data ) ;

    FItems.BeginUpdate ;
    if _step > 0 then begin
      if SelectedNode.Index + _step > FItems.Count - 1 then begin
        moveNode( SelectedNode.Index, FItems.Count - 1 ) ;
        if FReverseOrder then
          dt.Layer.ZOrder := 0
        else
          dt.Layer.ZOrder := GIS_MAX_INTEGER ;
      end
      else begin
        moveNode( SelectedNode.Index, SelectedNode.Index + _step ) ;
        dt.Layer.ZOrder := dt.Layer.ZOrder + calculate_step ;
      end ;
    end
    else begin
      if SelectedNode.Index + _step < 0 then begin
        moveNode( SelectedNode.Index, 0 ) ;
        if FReverseOrder then
          dt.Layer.ZOrder := GIS_MAX_INTEGER
        else
          dt.Layer.ZOrder := 0 ;
      end
      else begin
        moveNode( SelectedNode.Index, SelectedNode.Index + _step ) ;
        dt.Layer.ZOrder := dt.Layer.ZOrder + calculate_step ;
      end ;
    end ;
    FItems.EndUpdate ;

    for ii := FItems.Count - 1 downto 0 do begin
      FLastNode := FItems[ii] ;
      if FLastNode.Level = 0 then
        break ;
    end ;

    //makeNodeVisible( SelectedNode, _step, True ) ;
    oParent.ControlFullUpdate( True ) ;
  end ;

  procedure TGIS_Legend.DoSelect(
    const _step : Integer
  ) ;
  begin
    if ( not assigned( GIS_Viewer ) ) or ( GIS_Viewer.Items.Count <= 0 ) then
      exit ;

    if not assigned( SelectedNode ) then
      exit ;

    if Mode <> TGIS_ControlLegendMode.Layers then
      exit ;

    if SelectedNode.Level = 0 then begin
      if _step > 0 then begin
        if SelectedNode.Index + _step > FItems.Count - 1 then
          SelectedNode := FItems[FItems.Count - 1]
        else
          SelectedNode := FItems[SelectedNode.Index + _step] ;
      end
      else begin
        if SelectedNode.Index + _step < 0 then
          SelectedNode := FItems[0]
        else
          SelectedNode := FItems[SelectedNode.Index + _step] ;
      end ;
    end
    else begin
      if _step > 0 then begin
        if SelectedNode.Index + _step > SelectedNode.Parent.Count - 1 then
          SelectedNode := SelectedNode.Parent.Items[SelectedNode.Parent.Count - 1]
        else
          SelectedNode := SelectedNode.Parent.Items[SelectedNode.Index + _step] ;
      end
      else begin
        if SelectedNode.Index + _step < 0 then
          SelectedNode := SelectedNode.Parent.Items[0]
        else
          SelectedNode := SelectedNode.Parent.Items[SelectedNode.Index + _step] ;
      end ;
    end ;

    makeNodeVisible( SelectedNode, _step, False ) ;
    oParent.ControlRepaint ;
  end ;

  procedure TGIS_Legend.readGroups ;
  var
    lst  : TList<TGIS_TreeNode> ;
    node : TGIS_TreeNode ;
    tn   : TGIS_TreeNode ;
    i    : Integer ;

    function add_group( const _g : IGIS_HierarchyGroup ;
                        const _n : TGIS_TreeNode
                      ) : TGIS_TreeNode ;
    var
      prev : TGIS_TreeNode ;
      slcd : Boolean ;
      expd : Boolean ;
      chkd : Boolean ;
      ii   : Integer ;
      nn   : TGIS_TreeNode ;
      dt   : TGIS_LegendItemData ;
      sb   : TStringBuilder ;
      str  : String ;
    begin
      lstOldGroupNodes.TryGetValue( _g.Name, prev ) ;
      if assigned( prev ) then begin
        slcd := prev.Selected ;
        expd := prev.Expanded ;
        chkd := TGIS_LegendItemData( prev.Data ).Checked ;
      end
      else begin
        slcd := False ;
        expd := not _g.Collapsed ;
        chkd := _g.Active ;
      end ;

      sb := TStringBuilder.Create ;
      try
        sb.Append( _g.Caption ) ;
        sb.Append( '   ' ) ;
        str := sb.ToString ;
      finally
        FreeObject( sb ) ;
      end ;

      nn := FItems.AddChild( _n, str ) ;
      dt := TGIS_LegendItemData.Create( _g, FViewer ) ;
      dt.Checked := chkd ;
      nn.Data := dt ;

      for ii := 0 to _g.GroupsCount - 1 do
        add_group( _g.Groups[ii], nn ) ;

      for ii := 0 to _g.LayersCount - 1 do
        addLayerNode( TGIS_Layer( _g.Layers[ii] ), -1, nn ) ;

      calcNodeHeight( nn ) ;
      forceExpanded( nn, expd ) ;
      nn.Selected := slcd ;

      Result := nn ;
    end ;

    procedure addOldGroupNodes( const _node : TGIS_TreeNode ) ;
    var
      j   : Integer ;
      nd  : TGIS_TreeNode ;
      dt  : TGIS_LegendItemData ;
      gp  : IGIS_HierarchyGroup ;
      la  : TGIS_Layer ;
    begin
      dt := TGIS_LegendItemData( _node.Data ) ;
      if dt.DataType = TGIS_LegendItemType.Layer then begin
        la := dt.Layer ;
        if assigned( la ) and
           ( not lstOldLayerNodes.TryGetValue( la.Name, tn ) ) then
          lstOldLayerNodes.Add( la.Name, _node ) ;
      end
      else
      if dt.DataType = TGIS_LegendItemType.Group then begin
        gp := dt.Group ;
        if assigned( gp ) and
           ( not lstOldGroupNodes.TryGetValue( gp.Name, tn ) ) then
          lstOldGroupNodes.Add( gp.Name, _node ) ;
      end ;

      if _node.Count = 0 then
        exit ;

      for j := 0 to _node.FItems.Count - 1 do begin
         nd := _node.FItems[j] ;
         addOldGroupNodes( nd ) ;
      end;
    end;

  begin
    lst := TList<TGIS_TreeNode>.Create ;
    try
      for i := 0 to FItems.Count - 1 do begin

        node := FItems[i] ;

        if node.Level = 0 then
          lst.Add( node ) ;

        addOldGroupNodes( node ) ;
      end ;

      node := nil ;

      FItems.BeginUpdate ;
      oSelected.Clear ;

      for i := 0 to FViewer.Hierarchy.GroupsCount - 1 do
        node := add_group( FViewer.Hierarchy.Groups[i], nil ) ;
      FLastNode := node ;

      lstOldLayerNodes.Clear ;
      lstOldGroupNodes.Clear ;
      for i := lst.Count - 1 downto 0 do
        {$IFDEF OXYGENE}
          deleteNode( lst.Item[i] ) ;
        {$ELSE}
          deleteNode( lst.Items[i] ) ;
        {$ENDIF}

      if ( not opAllowSelect ) or ( FItems.Count = 0 ) then begin
        SelectedNode := nil ;
        oSelected.Clear ;
      end ;

      FItems.EndUpdate ;
    finally
      FreeObject( lst ) ;
    end ;
  end ;

  function TGIS_Legend.getFirstLayerInGroup(
    const _group : TGIS_TreeNode
  ) : TGIS_TreeNode ;
  var
    i : Integer ;
    n : TGIS_TreeNode ;
  begin
    Result := nil ;
    if not assigned( _group ) then exit ;
    for i := 0 to _group.Count - 1 do begin
      n := _group.Items[i] ;
      if TGIS_LegendItemData( n.Data ).DataType = TGIS_LegendItemType.Layer then
      begin
        Result := n ;
        break ;
      end ;
    end ;
  end ;

  function TGIS_Legend.getParamsTextY(
    const _offset : Integer ;
      var _h      : Integer
  ) : Integer ;
  var
    ss : TPoint ;
  begin
    // text rectangle will be moved up a little
    ss := oParent.ControlStyleGetTextExtent( False, 'Wy' ) ;
    _h := ss.Y ;
    Result := ( ss.Y - apScale( RoundS(FFontSize) ) ) div 2 ;
    Result := _offset - Result * 16 div 25 ;
    Result := Result * apScale( 100 ) div 100 - apScale( 1 ) ;
  end ;

  function TGIS_Legend.getHeightStd
    : Integer ;
  begin
    Result := FDefHeight ;
  end ;

  function TGIS_Legend.getHeight16(
    const _text   : Boolean ;
    const _offset : Integer
  ) : Integer ;
  var
    dy : Integer ;
    h  : Integer ;
  begin
    Result := FDefHeight16 ;
    if _text then begin
      dy := getParamsTextY( _offset, h ) ;
      if dy + h > Result then
        Result := dy + h ;
    end ;
  end ;

  function TGIS_Legend.getHeightParams(
    const _text : Boolean
  ) : Integer ;
  var
    dy : Integer ;
    h  : Integer ;
  begin
    if FCompactView then
      Result := FDefHeight16
    else
      Result := 2 * FDefHeight16 ;
    if _text then begin
      dy := getParamsTextY( 0, h ) ;
      if dy + h > Result then
        Result := dy + h ;
    end ;
  end ;

  function TGIS_Legend.getHeightParamsHeader
    : Integer ;
  begin
    Result := getHeight16( False, 0 ) ;
  end ;

  function TGIS_Legend.getHeightParamsText
    : Integer ;
  begin
    Result := getHeight16( True, getCheckBoxRectTop( TGIS_LegendItemType.Params ) ) ;
  end ;

  function TGIS_Legend.getHeightParamsRender
    : Integer ;
  begin
    Result := getHeightParams( True ) ;
  end ;

  function TGIS_Legend.getHeightParamsChart
    : Integer ;
  begin
    Result := getHeightParams( True ) ;
  end ;

  function TGIS_Legend.getHeightPixel
    : Integer ;
  begin
    Result := getHeight16( True, 0 ) ;
  end ;

  procedure TGIS_Legend.calcNodeHeight(
    const _node : TGIS_TreeNode
  ) ;
  var
    dt : TGIS_LegendItemData ;

    function calc_params_height( const _n : TGIS_TreeNode ) : Integer ;
    var
      hh  : Integer ;
      h0  : Integer ;
      dd  : TGIS_LegendItemData ;
      src : TGIS_ParamsSectionVector ;
      ii  : Integer ;
      ll  : Boolean ;
    begin
      dd := TGIS_LegendItemData( _n.Data ) ;
      if not dd.IsVector then
        hh := ( dd.RowCount + 1 ) * getHeightPixel
      else begin
        if ( not IsStringEmpty( _n.Text ) ) and
           ( ( dd.FeatureCount > 1 ) or dd.Render ) then
          h0 := getHeightParamsText
        else
          h0 := 0 ;
        hh := h0 ;

        src := TGIS_ParamsSectionVector( dd.Params ) ;

        ll := False ;
        if src.Marker.ShowLegend then begin
          if dd.RenderMarker then begin
            for ii := 1 to Abs( src.Render.ZonesEx ) do
              inc( hh, getHeightParamsRender ) ;
            for ii := 1 to Abs( src.Render.Zones ) do
              inc( hh, getHeightParamsRender ) ;
          end else begin
            if h0 = 0 then
              inc( hh, getHeightParams( not IsStringEmpty( _n.Text ) ) )
            else
              inc( hh, getHeightParams( False ) ) ;
            ll := True ;
          end ;
        end ;

        if src.Line.ShowLegend then begin
          if dd.RenderLine then begin
            for ii := 1 to Abs( src.Render.ZonesEx ) do
              inc( hh, getHeightParamsRender ) ;
            for ii := 1 to Abs( src.Render.Zones ) do
              inc( hh, getHeightParamsRender ) ;
          end else begin
            if ( not ll ) or dd.Render then begin
              if h0 = 0 then
                inc( hh, getHeightParams( not IsStringEmpty( _n.Text ) ) )
              else
                inc( hh, getHeightParams( False ) ) ;
              ll := True ;
            end ;
          end ;
        end ;

        if src.Area.ShowLegend then begin
          if dd.RenderArea then begin
            for ii := 1 to Abs( src.Render.ZonesEx ) do
              inc( hh, getHeightParamsRender ) ;
            for ii := 1 to Abs( src.Render.Zones ) do
              inc( hh, getHeightParamsRender ) ;
          end else begin
            if ( not ll ) or dd.Render then begin
              if h0 = 0 then
                inc( hh, getHeightParams( not IsStringEmpty( _n.Text ) ) )
              else
                inc( hh, getHeightParams( False ) ) ;
              ll := True ;
            end ;
          end ;
        end ;

        if src.Labels.ShowLegend then begin
          if dd.RenderLabel then begin
            for ii := 1 to Abs( src.Render.ZonesEx ) do
              inc( hh, getHeightParamsRender ) ;
            for ii := 1 to Abs( src.Render.Zones ) do
              inc( hh, getHeightParamsRender ) ;
          end else begin
            if ( not ll ) or ( h0 = 0 ) then
              inc( hh, getHeightParams( not IsStringEmpty( _n.Text ) ) ) ;
          end ;
        end ;

        if src.Chart.ShowLegend and
           ( not IsStringEmpty( dd.ChartMap ) ) then begin
          for ii := StringFirst to StringLast( dd.ChartMap ) do begin
            if dd.ChartMap[ii] = '1' then
              inc( hh, getHeightParamsChart ) ;
          end ;
        end ;
      end ;
      Result := hh ;
    end ;

    function calc_layer_height( const _n : TGIS_TreeNode ) : Integer ;
    var
      hh : Integer ;
      ii : Integer ;
      ds : TGIS_LegendItemData ;
    begin
      hh := getHeightStd ;
      for ii := 0 to _n.Count - 1 do begin
        ds := TGIS_LegendItemData( _n.Items[ii].Data ) ;
        case ds.DataType of
          TGIS_LegendItemType.Params :
            ds.NodeHeight := calc_params_height( _n.Items[ii] ) ;
          else
        end;
        inc( hh, ds.NodeHeight ) ;
      end;
      Result := hh ;
    end ;

    function calc_curr_height( const _n : TGIS_TreeNode ) : Integer ;
    var
      hh : Integer ;
      ii : Integer ;
      ds : TGIS_LegendItemData ;
    begin
      hh := getHeightStd ;
      for ii := 0 to _n.Count - 1 do begin
        ds := TGIS_LegendItemData( _n.Items[ii].Data ) ;
        case ds.DataType of
          TGIS_LegendItemType.Layer :
            begin
              if assigned( ds.Layer ) then begin
                if _n.Items[ii].Expanded then
                  inc( hh, calc_layer_height( _n.Items[ii] ) )
                else
                  inc( hh, getHeightStd ) ;
              end;
            end ;
          TGIS_LegendItemType.Group :
            begin
              if _n.Items[ii].Expanded then
                inc( hh, calc_curr_height( _n.Items[ii] ) )
              else
                inc( hh, getHeightStd ) ;
            end ;
        end ;
      end ;
      Result := hh ;
    end ;

  begin
    dt := TGIS_LegendItemData( _node.Data ) ;
    dt.NodeHeight := 0 ;
    case dt.DataType of
      TGIS_LegendItemType.Empty  :
        dt.NodeHeight := getHeightStd ;
      TGIS_LegendItemType.Layer  :
        begin
          if assigned( dt.Layer ) then begin
            if not assigned( dt.Layer.SubLayers ) then
              dt.NodeHeight := calc_layer_height( _node )
            else
              dt.NodeHeight := calc_curr_height( _node ) ;
          end ;
        end ;
      TGIS_LegendItemType.Group  :
        begin
          dt.NodeHeight   := calc_curr_height( _node ) ;
        end ;
      TGIS_LegendItemType.Params :
        begin
          dt.NodeHeight   := calc_params_height( _node ) ;
        end ;
    end ;
  end ;

  function TGIS_Legend.getTextRect(
    const _node     : TGIS_TreeNode ;
    const _size     : Boolean ;
    const _selected : Boolean
  ) : TRect ;
  var
    dt  : TGIS_LegendItemData ;
  begin
    Result := getTextRect( _node, _size, _selected, _node.Text ) ;
    dt := TGIS_LegendItemData( _node.Data ) ;
    if ( dt.DataType = TGIS_LegendItemType.Layer ) or
       ( dt.DataType = TGIS_LegendItemType.Group ) then
      Result.Height := getHeightStd ;
  end ;

  function TGIS_Legend.getTextRect(
    const _node     : TGIS_TreeNode ;
    const _size     : Boolean ;
    const _selected : Boolean ;
    const _text     : String
  ) : TRect ;
  var
    dt  : TGIS_LegendItemData ;
    rct : TRect ;
    ss  : TPoint ;
    rct_left   : Integer ;
    rct_top    : Integer ;
    rct_right  : Integer ;
    rct_bottom : Integer ;
  begin
    rct := GetNodeRect( _node, _size ) ;

    rct_top    := rct.Top + apScale( 1 ) ;
    rct_bottom := rct.Bottom ;

    if oParent.ControlRightToLeft then begin
      if not IsStringEmpty( _text ) then
        ss := oParent.ControlStyleGetTextExtent( _selected, _text )
      else
        ss := TPoint.Create( 0, 0 ) ;
      rct_left  := rct.Right - apScale( 16 ) - ss.X ;
      rct_right := rct_left + ss.X ;
    end
    else begin
      rct_left := rct.Left + apScale( 16 ) ;
      rct_right  := rct.Right ;
    end ;

    dt := TGIS_LegendItemData( _node.Data ) ;
    if ( dt.DataType = TGIS_LegendItemType.Layer ) or
       ( dt.DataType = TGIS_LegendItemType.Group ) then begin
        if opAllowActive then begin
          if oParent.ControlRightToLeft then begin
            rct_left  := rct_left  - apScale( FCheckBoxRectSize + 4 ) ;
            rct_right := rct_right - apScale( FCheckBoxRectSize + 4 ) ;
          end
          else
            rct_left := rct_left + apScale( FCheckBoxRectSize + 4 ) ;
        end ;
    end
    else
    if dt.DataType = TGIS_LegendItemType.Params then
    begin
      if opAllowParamsVisible then begin
        if oParent.ControlRightToLeft then begin
          rct_left  := rct_left  - apScale( FCheckBoxRectSize + 4 ) ;
          rct_right := rct_right - apScale( FCheckBoxRectSize + 4 ) ;
        end else
          rct_left := rct_left + apScale( FCheckBoxRectSize + 4 ) ;
      end ;
    end ;

    Result := Rect( rct_left, rct_top, rct_right, rct_bottom ) ;
  end ;

  function TGIS_Legend.getParamsRect(
    const _node : TGIS_TreeNode ;
    const _size : Boolean
  ) : TRect ;
  var
    rct : TRect ;
    l   : Integer ;

    rct_left   : Integer ;
    rct_top    : Integer ;
    rct_right  : Integer ;
    rct_bottom : Integer ;
  begin
    rct := GetNodeRect( _node, _size ) ;

    rct_top := rct.Top + apScale( 1 ) ;
    rct_bottom := rct.Bottom - apScale( 1 ) ;

    if TGIS_LegendItemData( _node.Data ).IsVector then begin
      if opAllowParamsVisible then
        l := apScale( 16 )
      else
        l := 0 ;
      if oParent.ControlRightToLeft then begin
        rct_left  := 0 ;
        rct_right := rct.Right - l - apScale( 14 ) ;
      end
      else begin
        rct_left  := l + rct.Left + apScale( 14 ) ;
        rct_right := rct.Right ;
      end ;
    end
    else begin
      if oParent.ControlRightToLeft then begin
        rct_left  := 0 ;
        rct_right := rct.Right - apScale( 16 ) ;
      end
      else begin
        rct_left  := rct.Left + apScale( 16 ) ;
        rct_right := rct.Right ;
      end ;
    end ;

    Result := Rect( rct_left, rct_top, rct_right, rct_bottom ) ;
  end ;

  procedure TGIS_Legend.buildHierarchy ;
  var
    hrh : IGIS_HierarchyManager ;
    i   : Integer ;

    procedure build_group( const _g : IGIS_HierarchyGroup ;
                           const _n : TGIS_TreeNode ) ;
    var
      gg : IGIS_HierarchyGroup ;
      dd : TGIS_LegendItemData ;
      ii : Integer ;
    begin
      dd := TGIS_LegendItemData( _n.Data ) ;

      if dd.DataType <> TGIS_LegendItemType.Group then
        exit ;

      if not assigned( _g ) then
        gg := hrh.CreateGroup( dd.Name )
      else
        gg := _g.CreateGroup( dd.Name ) ;

      gg.Active    := dd.Checked ;
      gg.Collapsed := not _n.Expanded ;
      gg.Caption   := dd.Caption ;

      for ii := 0 to _n.Count - 1 do begin

        dd := TGIS_LegendItemData( _n.Items[ii].Data ) ;
        if dd.DataType = TGIS_LegendItemType.Group then
          build_group( gg, _n.Items[ii] )
        else
        if dd.DataType = TGIS_LegendItemType.Layer then
          gg.AddLayer( dd.Layer )
        else
          exit ;
      end ;
    end ;

  begin
    hrh := FViewer.Hierarchy ;

    hrh.ClearGroups ;

    for i := 0 to FItems.Count - 1 do begin
      if assigned( FItems[i].Parent ) then
        continue ;
      build_group( nil, FItems[i] ) ;
    end ;
  end ;

  procedure TGIS_Legend.doExpanding(
        _sender : TObject ;
        _node   : TGIS_TreeNode ;
    var _allow  : Boolean
  ) ;
  begin
    if opAllowExpand then
      _allow := canExpand
    else
      _allow := False ;
  end ;

  procedure TGIS_Legend.doChanging(
          _sender  : TObject ;
          _node    : TGIS_TreeNode ;
    var   _allow   : Boolean
  ) ;
  var
    dt : TGIS_LegendItemData ;
  begin
    _allow := False ;

    if ( not opAllowSelect ) and assigned( _node ) then begin
      SelectedNode := nil ;
      exit ;
    end ;

    if not assigned( _node ) then begin
      if assigned( SelectedNode ) then begin
        dt := TGIS_LegendItemData( SelectedNode.Data ) ;
        if ( dt.DataType = TGIS_LegendItemType.Layer ) and
           assigned( FLayerSelectEvent ) then begin
          skipSelect := True ;
          skipUpdate := True ;
          try
            {$IFDEF OXYGENE}
              FLayerSelectEvent( Self, new TGIS_LayerEventArgs( nil ) ) ;
            {$ELSE}
              FLayerSelectEvent( Self, nil ) ;
            {$ENDIF}
          finally
            skipUpdate := False ;
          end;
        end ;
        if ( dt.DataType = TGIS_LegendItemType.Group ) and
           assigned( FGroupSelectEvent ) then begin
          skipSelect := True ;
          skipUpdate := True ;
          try
            {$IFDEF OXYGENE}
              FGroupSelectEvent( Self, new TGIS_HierarchyGroupEventArgs( nil ) ) ;
            {$ELSE}
              FGroupSelectEvent( Self, nil ) ;
            {$ENDIF}
          finally
            skipUpdate := False ;
          end;
        end ;
      end ;
      _allow := True ;
      exit ;
    end ;

    dt := TGIS_LegendItemData( _node.Data ) ;

    if dt.DataType = TGIS_LegendItemType.Params then
      SelectedNode := _node.Parent
    else
    if dt.DataType = TGIS_LegendItemType.Empty then
      SelectedNode := _node.Parent.Parent
    else begin
      if ( dt.DataType = TGIS_LegendItemType.Layer ) and
         assigned( FLayerSelectEvent ) then begin
        skipSelect := True ;
        skipUpdate := True ;
        try
          {$IFDEF OXYGENE}
            FLayerSelectEvent( Self, TGIS_LayerEventArgs.Create( dt.Layer ) ) ;
          {$ELSE}
            FLayerSelectEvent( Self, dt.Layer ) ;
          {$ENDIF}
        finally
          skipUpdate := False ;
        end;
      end ;
      if ( dt.DataType = TGIS_LegendItemType.Group ) and
         assigned( FGroupSelectEvent ) then begin
        skipSelect := True ;
        skipUpdate := True ;
        try
          {$IFDEF OXYGENE}
            FGroupSelectEvent( Self, TGIS_HierarchyGroupEventArgs.Create( dt.Group ) ) ;
          {$ELSE}
            FGroupSelectEvent( Self, dt.Group ) ;
          {$ENDIF}
        finally
          skipUpdate := False ;
        end ;
      end ;
      _allow := True ;
    end ;
  end ;

  procedure TGIS_Legend.SubscribedEvent(
    _sender  : TObject ;
    _event   : Integer ;
    _context : TObject
  ) ;
  begin
    if isMouseDn and ( _event <> GIS_SUBSCRIBED_TOUCH ) then
      exit ;

    case _event of
      GIS_SUBSCRIBED_DESTROY :
        begin
          FViewer := nil ;
        end ;
      GIS_SUBSCRIBED_AFTERPAINT :
        begin
          FBasemap := not assigned( _context ) ;
          try
            oParent.ControlSubscribedUpdate ;
          finally
            FBasemap := False ;
          end ;
        end ;
      GIS_SUBSCRIBED_PROJECT_CLOSE :
        begin
          FTopOffset := 0 ;
          lastHash   := 0 ;
        end ;
      {$IFDEF DCC}
        // FMX
        GIS_SUBSCRIBED_TOUCH :
          begin
            oParent.ControlTouch( _context ) ;
          end ;
      {$ENDIF}
    end ;
  end ;

  function TGIS_Legend.IsExpanded(
    const _layer : TGIS_Layer
  ) : Boolean ;
  var
    node : TGIS_TreeNode ;
  begin
    Result := False ;

    node := findLayerNode( _layer ) ;

    if not assigned( node ) then
      exit ;

    Result := node.Expanded ;
  end ;

  function TGIS_Legend.IsExpanded(
    const _group : IGIS_HierarchyGroup
  ) : Boolean ;
  var
    node : TGIS_TreeNode ;
  begin
    Result := False ;

    node := findGroupNode( _group ) ;

    if not assigned( node ) then
      exit ;

    Result := node.Expanded ;
  end ;

  procedure TGIS_Legend.Expand(
    const _layer : TGIS_Layer
  ) ;
  var
    node : TGIS_TreeNode ;
  begin
    node := findLayerNode( _layer ) ;
    if forceExpanded( node, True ) then
      safeRepaint ;
  end ;

  procedure TGIS_Legend.Expand(
    const _group : IGIS_HierarchyGroup
  ) ;
  begin
    Expand( _group, False, False ) ;
  end ;

  procedure TGIS_Legend.Expand(
    const _group   : IGIS_HierarchyGroup ;
    const _deep    : Boolean ;
    const _layers  : Boolean
  ) ;

    function _expand(
      const _group  : IGIS_HierarchyGroup ;
      const _deep   : Boolean ;
      const _layers : Boolean
    ) : Boolean ;
    var
      node : TGIS_TreeNode ;
      i    : Integer ;
    begin
      node := findGroupNode( _group ) ;
      Result := forceExpanded( node, True ) ;
      _group.Collapsed := False ;

      if not _deep then
        exit ;

      for i := 0 to _group.GroupsCount - 1 do
        Result := Result or _expand( _group.Groups[i], _deep, _layers ) ;

      if not _layers then
        exit ;

      for i := 0 to _group.LayersCount - 1 do begin
        node := findLayerNode( TGIS_Layer( _group.Layers[i] ) ) ;
        Result := Result or forceExpanded( node, True ) ;
      end ;
    end ;
  begin
    if _expand( _group, _deep, _layers ) then
      safeRepaint ;
  end ;

  procedure TGIS_Legend.ClearSelection;
  begin
    SelectedNode := nil ;
  end ;

  procedure TGIS_Legend.Collapse(
    const _layer : TGIS_Layer
  ) ;
  var
    node : TGIS_TreeNode ;
  begin
    node := findLayerNode( _layer ) ;
    if forceExpanded( node, False ) then
      safeRepaint ;
  end ;

  procedure TGIS_Legend.Collapse(
    const _group : IGIS_HierarchyGroup
  ) ;
  begin
    Collapse( _group, False, False ) ;
  end ;

  procedure TGIS_Legend.Collapse(
    const _group   : IGIS_HierarchyGroup ;
    const _deep    : Boolean ;
    const _layers  : Boolean
  ) ;

    function _collapse(
      const _group  : IGIS_HierarchyGroup ;
      const _deep   : Boolean ;
      const _layers : Boolean
    ) : Boolean ;
    var
      node : TGIS_TreeNode ;
      i    : Integer ;
    begin
      node := findGroupNode( _group ) ;
      Result := forceExpanded( node, False ) ;
      _group.Collapsed := True ;

      if not _deep then
        exit ;

      for i := 0 to _group.GroupsCount - 1 do
        Result := Result or _collapse( _group.Groups[i], _deep, _layers ) ;

      if not _layers then
        exit ;

      for i := 0 to _group.LayersCount - 1 do begin
        node := findLayerNode( TGIS_Layer( _group.Layers[i] ) ) ;
        Result := Result or forceExpanded( node, False ) ;
      end ;
    end ;

  begin
    if _collapse( _group, _deep, _layers ) then
      safeRepaint ;
  end ;

  procedure TGIS_Legend.recreateTree(
    const _disableEvents : Boolean
  ) ;
  var
    layer_evt : TGIS_LayerEvent ;
    group_evt : TGIS_HierarchyGroupEvent ;
  begin
    layer_evt := nil ;
    group_evt := nil ;
    if _disableEvents then begin
      layer_evt := FLayerSelectEvent ;
      FLayerSelectEvent := nil ;
      group_evt := FGroupSelectEvent ;
      FGroupSelectEvent := nil ;
    end ;
    lock ;
    try
      case FMode of
        TGIS_ControlLegendMode.Layers : readLayers ;
        TGIS_ControlLegendMode.Groups : readGroups ;
      end ;
    finally
      unlock ;
      if _disableEvents then begin
        FLayerSelectEvent := layer_evt ;
        FGroupSelectEvent := group_evt ;
      end ;
    end ;
  end ;

//==================================== END =====================================
end.
