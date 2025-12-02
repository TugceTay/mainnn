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
  VCL legend control.
}

unit VCL.GisControlLegend ;
{$HPPEMIT '#pragma link "VCL.GisControlLegend"'}

{$INCLUDE GisInclude.inc}

interface

uses
  System.Types,
  System.Classes,
  Winapi.Windows,
  Winapi.Messages,

  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.ExtCtrls,

  {$IFDEF GIS_XDK}
    XDK.Core,
  {$ENDIF}

  GisRtl,
  GisTypesUI,
  GisInterfaces,
  GisLayer,
  GisHierarchy,
  GisLegend,
  GisLegendUtils,
  GisGestureHelper,
  VCL.GisLegendUtilsFactory,
  VCL.GisStyleRenderer,
  VCL.GisViewerWnd ;

type

  /// <summary>
  ///   Visual Legend component.
  /// </summary>
  /// <remarks>
  ///   Place this component on a form and connect GIS_Viewer
  ///   to IGIS_Viewer object. Selected layer is available via GIS_Layer property.
  /// </remarks>
  [ComponentPlatformsAttribute( pfidWindows )]
  TGIS_ControlLegend = class ( TCustomControl,
                               IGIS_LegendParent,
                               IGIS_PrintableControl )
    private
      FBiDiModeFromTranslation : Boolean ;
      FLock                    : Boolean ;
      FInPaint                 : Boolean ;

    private
      FSelectionColor      : TColor ;
      FSelectionFontColor  : TColor ;

    private
      oLegend              : TGIS_Legend ;
      oRenderer            : TGIS_StyleRendererVCL ;
      oGestHlpr            : TGIS_GestureHelper ;
      oTimer               : TTimer  ;
      bTouchesRegistered   : Boolean ;
      bmpCache             : TBitmap ;
      bmpCacheOld          : TBitmap ;
      bmpDrag              : TBitmap ;
      bFormOpened          : Boolean ;
      oCanvasBitmap        : TBitmap ;

    private
      st_CompactView  : Boolean ;
      st_ReverseOrder : Boolean ;
      st_IconStyle    : TGIS_LegendIconStyle ;
      st_FontName     : String  ;
      st_FontSize     : Integer ;
      st_FontStyle    : TFontStyles ;
      st_FontColor    : TColor  ;

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
      modeBiDi             : TBiDiMode   ;
      prvHScroll           : Integer     ;

    private

      /// <summary>
      ///   Private font.
      /// </summary>
      FFont                : TFont   ;

      /// <summary>
      ///   LayerParamsChangeEvent. Will be fired upon layer's parameters are activated/desactivated.
      /// </summary>
      FOnLayerParamsChange : TGIS_LayerEvent ;

      /// <summary>
      ///   LayerActiveChangeEvent. Will be fired upon a layer is activated/desactivated.
      /// </summary>
      FOnLayerActiveChange : TGIS_LayerEvent ;

      /// <summary>
      ///   LayerSelectEvent. Will be fired upon a layer is selected.
      /// </summary>
      FOnLayerSelect : TGIS_LayerEvent ;

      /// <summary>
      ///   GroupActiveChangeEvent. Will be fired upon a group is activated/desactivated.
      /// </summary>
      FOnGroupActiveChange : TGIS_HierarchyGroupEvent ;

      /// <summary>
      ///   GroupSelectEvent. Will be fired upon a layer is selected.
      /// </summary>
      FOnGroupSelect : TGIS_HierarchyGroupEvent ;

      /// <summary>
      ///   OrderChangeEvent. Will be fired upon a change of the order of layers.
      /// </summary>
      FOnOrderChange : TNotifyEvent ;

      /// <summary>
      ///   OpenDialogEvent. Will be fired upon double press down/up.
      /// </summary>
      FOnOpenDialog  : TGIS_LayerEvent ;

      /// <summary>
      ///   TapSimpleEvent. Will be fired upon press down/up.
      /// </summary>
      FOnTapSimple   : TMouseEvent ;

      /// <summary>
      ///   TapDoubleEvent. Will be fired upon double press down/up.
      /// </summary>
      FOnTapDouble   : TMouseEvent ;

      /// <summary>
      ///   TapLongEvent. Will be fired upon longer press down.
      /// </summary>
      FOnTapLong     : TMouseEvent ;

    private // properties 'inherited' from TGIS_Legend
      function  fget_Viewer         : IGIS_Viewer ;
      procedure fset_Viewer         ( const _value : IGIS_Viewer
                                    ) ;
      function  fget_Layer          : TGIS_Layer ;
      procedure fset_Layer          ( const _value : TGIS_Layer
                                    ) ;
      function  fget_Layers         : TGIS_LayerAbstractList ;
      procedure fset_Layers         ( const _value : TGIS_LayerAbstractList
                                    ) ;
      function  fget_Group          : IGIS_HierarchyGroup ;
      procedure fset_Group          ( const _value : IGIS_HierarchyGroup
                                    ) ;
      function  fget_Mode           : TGIS_ControlLegendMode ;
      procedure fset_Mode           ( const _value : TGIS_ControlLegendMode
                                    ) ;
      function  fget_Options        : TGIS_ControlLegendOptions ;
      procedure fset_Options        ( const _value : TGIS_ControlLegendOptions
                                    ) ;
      function  fget_ReverseOrder   : Boolean ;
      procedure fset_ReverseOrder   ( const _value : Boolean
                                    ) ;
      function  fget_CompactView    : Boolean ;
      procedure fset_CompactView    ( const _value : Boolean
                                    ) ;
      function  fget_DrawIconStyle  : TGIS_LegendIconStyle ;
      procedure fset_DrawIconStyle  ( const _value : TGIS_LegendIconStyle
                                    ) ;
      function  fget_DialogOptions  : TGIS_ControlLegendDialogOptions ;
      procedure fset_DialogOptions  ( const _value : TGIS_ControlLegendDialogOptions
                                    ) ;

    private // IGIS_PrintableControl property access routines
      function  fget_InternalName   : String ;
      procedure fset_InternalName   ( const _value : String
                                    ) ;

    private // new public properties
      procedure fset_BiDiModeFromTranslation
                                    ( const _value : Boolean
                                    ) ;
      function  fget_BorderStyle    : TBorderStyle ;
      procedure fset_BorderStyle    ( const _value : TBorderStyle
                                    ) ;
      procedure fset_SelectionColor ( const _value : TColor
                                    ) ;
      procedure fset_SelectionFontColor
                                    ( const _value : TColor
                                    ) ;

    private // protected property
      function  fget_SelectedNode   : TGIS_TreeNode ;

    private // reintroduced properties
      procedure fset_ParentBiDiMode ( const _value : Boolean
                                    ) ;
      function  fget_Font           : TFont ;
      procedure fset_Font           ( const _value : TFont
                                    ) ;

    private
      procedure doFontChanged       (       _sender : TObject
                                    ) ;
      procedure doLayerParamsChange (       _sender : TObject ;
                                            _layer  : TGIS_Layer
                                    ) ;
      procedure doLayerActiveChange (       _sender : TObject ;
                                            _layer  : TGIS_Layer
                                    ) ;
      procedure doLayerSelect       (       _sender : TObject ;
                                            _layer  : TGIS_Layer
                                    ) ;
      procedure doGroupActiveChange (       _sender : TObject ;
                                            _group  : IGIS_HierarchyGroup
                                    ) ;
      procedure doGroupSelect       (       _sender : TObject ;
                                            _group  : IGIS_HierarchyGroup
                                    ) ;
      procedure doOrderChange       (       _sender : TObject
                                    ) ;
      procedure doOpenDialog        (       _sender : TObject ;
                                            _layer  : TGIS_Layer
                                    ) ;

    private
      procedure msgEraseBkGnd       ( var    _msg       : TWMPaint
                                    ); message WM_ERASEBKGND;
      procedure msgPaint            ( var    _msg       : TWMPaint
                                    ); message WM_PAINT;
      procedure msgGetDlgCode       ( var   _msg        : TMessage
                                    ) ; message WM_GETDLGCODE ;

      procedure msgVScroll          ( var   _msg        : TWMVScroll
                                    ) ; message WM_VSCROLL ;

      procedure msgHScroll          ( var   _msg        : TWMHScroll
                                    ) ; message WM_HSCROLL ;

      procedure msgTouch            ( var   _msg        : TMessage
                                    ) ; message WM_TOUCH ;

      procedure msgDestroy          ( var   _msg        : TMessage
                                    ) ; message WM_DESTROY ;
      procedure msgCtl3DChanged     ( var    _msg       : TMessage
                                    ); message CM_CTL3DCHANGED;

      procedure doMouseWheelProc    (       _sender     : TObject ;
                                            _shift      : TShiftState ;
                                            _wheelDelta : Integer ;
                                            _mousePos   : TPoint ;
                                      var   _handled    : Boolean
                                    ) ;

      /// <summary>
      ///   Overwrites the default behavior when the user moves the mouse
      ///   outside of the control.
      /// </summary>
      /// <param name="_sender">
      ///   the control which fired the event
      /// </param>
      procedure doMouseLeave        (       _sender : TObject
                                    ) ;

      /// <summary>
      ///   Callback function of the TGIS_GestureHelper object.
      /// </summary>
      /// <param name="_sender">
      ///   the control which calls the update
      /// </param>
      procedure updateTap           (       _sender : TObject
                                    ) ;

      /// <summary>
      ///   The event which updates the attached IGIS_Viewer, delayed.
      /// </summary>
      /// <param name="_sender">
      ///   the control which calls the update
      /// </param>
      procedure doTimer             (       _sender : TObject
                                    ) ;

    private

      /// <summary>
      ///   Calculates the scaling factor according to current PPI.
      /// </summary>
      procedure updatePPI           ;

      /// <summary>
      ///   Draws the node.
      /// </summary>
      /// <param name="_node">
      ///   the node to be rendered
      /// </param>
      /// <param name="_bmp">
      ///   if not nil then the node is rendered on the bitmap instead of
      ///   the legend
      /// </param>
      procedure drawNode            ( const _node    : TGIS_TreeNode ;
                                      const _bmp     : TBitmap
                                    ) ;

      /// <summary>
      ///   Updates the legend when a full repaint is requested by
      ///   the attached IGIS_Viewer.
      /// </summary>
      procedure doSubscribedUpdate  ;

      function  setRenderer         : Boolean ;

      procedure openLegendForm      ;

    protected // overridden protected methods

      /// <summary>
      ///   Event handler of the single tap action.
      /// </summary>
      /// <param name="_button">
      ///   mouse button
      /// </param>
      /// <param name="_shift">
      ///   shift state
      /// </param>
      /// <param name="_x">
      ///    X screen coordinate
      /// </param>
      /// <param name="_y">
      ///    Y screen coordinate
      /// </param>
      procedure TapSingle           (       _button  : TMouseButton ;
                                            _shift   : TShiftState  ;
                                            _x       : Single       ;
                                            _y       : Single
                                    ) ; virtual ;

      /// <summary>
      ///   Event handler of the double tap action.
      /// </summary>
      /// <param name="_button">
      ///   mouse button
      /// </param>
      /// <param name="_shift">
      ///   shift state
      /// </param>
      /// <param name="_x">
      ///    X screen coordinate
      /// </param>
      /// <param name="_y">
      ///    Y screen coordinate
      /// </param>
      procedure TapDouble           (       _button  : TMouseButton ;
                                            _shift   : TShiftState  ;
                                            _x       : Single       ;
                                            _y       : Single
                                    ) ; virtual ;

      /// <summary>
      ///   Event handler of the long tap action.
      /// </summary>
      /// <param name="_button">
      ///   mouse button
      /// </param>
      /// <param name="_shift">
      ///   shift state
      /// </param>
      /// <param name="_x">
      ///    X screen coordinate
      /// </param>
      /// <param name="_y">
      ///    Y screen coordinate
      /// </param>
      procedure TapLong             (       _button  : TMouseButton ;
                                            _shift   : TShiftState  ;
                                            _x       : Single       ;
                                            _y       : Single
                                    ) ; virtual ;

      /// <summary>
      ///   Overrides the default KeyDown event behavior.
      /// </summary>
      /// <param name="_key">
      ///   the pressed key
      /// </param>
      /// <param name="_shift">
      ///   the pressed shift keys
      /// </param>
      procedure KeyDown             ( var   _key     : Word ;
                                            _shift   : TShiftState
                                    ) ; override ;

      /// <summary>
      ///   Overrides the default MouseDown event behavior.
      /// </summary>
      /// <param name="_button">
      ///   the pressed mouse button
      /// </param>
      /// <param name="_shift">
      ///   the pressed shift keys
      /// </param>
      /// <param name="_x">
      ///   the X coordinate
      /// </param>
      /// <param name="_y">
      ///   the Y coordinate
      /// </param>
      procedure MouseDown           (       _button  : TMouseButton ;
                                            _shift   : TShiftState ;
                                            _x       : Integer ;
                                            _y       : Integer
                                    ) ; override ;

      /// <summary>
      ///   Overrides the default MouseMove event behaviour.
      /// </summary>
      /// <param name="_shift">
      ///   the pressed shift keys
      /// </param>
      /// <param name="_x">
      ///   the X coordinate
      /// </param>
      /// <param name="_y">
      ///   the Y coordinate
      /// </param>
      procedure MouseMove           (       _shift   : TShiftState ;
                                            _x       : Integer ;
                                            _y       : Integer
                                    ) ; override ;

      /// <summary>
      ///   Overrides the default MouseUp event behaviour.
      /// </summary>
      /// <param name="_button">
      ///   the pressed mouse button
      /// </param>
      /// <param name="_shift">
      ///   the pressed shift keys
      /// </param>
      /// <param name="_x">
      ///   the X coordinate
      /// </param>
      /// <param name="_y">
      ///   the Y coordinate
      /// </param>
      procedure MouseUp             (       _button  : TMouseButton ;
                                            _shift   : TShiftState ;
                                            _x       : Integer ;
                                            _y       : Integer
                                    ) ; override ;

      /// <inheritdoc/>
      procedure Paint               ; override ;

      /// <inheritdoc/>
      procedure Resize              ; override ;

      /// <inheritdoc/>
      procedure SetBiDiMode         (       _value   : TBiDiMode
                                    ) ; override ;

      /// <summary>
      ///   Startup parameter overriding.
      /// </summary>
      /// <param name="_params">
      ///   control parameters
      /// </param>
      procedure CreateParams        ( var   _params  : TCreateParams
                                    ) ; override ;

    protected

      /// <summary>
      ///   Overrides the default Delete method.
      /// </summary>
      /// <param name="_node">
      ///   the node to be deleted
      /// </param>
      procedure Delete              (       _node    : TGIS_TreeNode
                                    ) ;

    public // overridden public methods

      /// <inheritdoc/>
      procedure Repaint             ; override ;

    public

      /// <summary>
      ///   Constructor, creates an instance.
      /// </summary>
      /// <param name="_owner">
      ///   the component which will own the instance
      /// </param>
      constructor Create            (       _owner   : TComponent
                                    ) ; override ;

      /// <summary>
      ///   Destructor, destroys the instance.
      /// </summary>
      destructor Destroy            ; override ;

    public

      /// <summary>
      ///   Checks if the legend item is associated with the specific layer
      ///   is expanded.
      /// </summary>
      /// <param name="_layer">
      ///   the layer to be checked
      /// </param>
      /// <returns>
      ///   True if the item is expanded, False otherwise
      /// </returns>
      function  IsExpanded          ( const _layer   : TGIS_Layer
                                    ) : Boolean ; overload ;

      /// <summary>
      ///   Checks if the legend item is associated with the specific hierarchy
      ///   group is expanded.
      /// </summary>
      /// <param name="_group">
      ///   the group to be checked
      /// </param>
      /// <returns>
      ///   True if the item is expanded, False otherwise
      /// </returns>
      function  IsExpanded          ( const _group   : IGIS_HierarchyGroup
                                    ) : Boolean ; overload ;

      /// <summary>
      ///   Expands a legend item associated with the specific layer.
      /// </summary>
      /// <param name="_layer">
      ///   the layer to be expanded
      /// </param>
      procedure Expand              ( const _layer   : TGIS_Layer
                                    ) ; overload ;

      /// <summary>
      ///   Expands of a legend item associated with the specific hierarchy
      ///   group.
      /// </summary>
      /// <param name="_group">
      ///   the group to be expanded
      /// </param>
      procedure Expand              ( const _group   : IGIS_HierarchyGroup
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
      procedure Expand              ( const _group   : IGIS_HierarchyGroup ;
                                      const _deep    : Boolean ;
                                      const _layers  : Boolean
                                    ) ; overload ;

      /// <summary>
      ///   Collapses a legend item corresponding to a layer.
      /// </summary>
      /// <param name="_layer">
      ///   the layer to be collapsed
      /// </param>
      procedure Collapse            ( const _layer   : TGIS_Layer
                                    ) ; overload ;

      /// <summary>
      ///   Collapses a legend item corresponding to a hierarchy group.
      /// </summary>
      /// <param name="_group">
      ///   the group to be collapsed
      /// </param>
      procedure Collapse            ( const _group   : IGIS_HierarchyGroup
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
      procedure Collapse            ( const _group   : IGIS_HierarchyGroup ;
                                      const _deep    : Boolean ;
                                      const _layers  : Boolean
                                    ) ; overload ;

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
      ///   tree node
      /// </returns>
      function  GetNodeAt           ( const _x       : Integer ;
                                      const _y       : Integer
                                    ) : TGIS_TreeNode ;

      /// <summary>
      ///   Forces invalidation of all the legend items.
      /// </summary>
      procedure InvalidateItems ;

      /// <summary>
      ///   Draw control on a provided bitmap.
      /// </summary>
      /// <param name="_bitmap">
      ///   bitmap on which the drawing will be performed; if null then bitmap
      ///   will be created based on control size and returned by function
      /// </param>
      /// <param name="_scale">
      ///   scale of the map; if 0 then map scale will be used
      /// </param>
      /// <param name="_ppi">
      ///   force PPI resolution; if 0 then set by corresponding GIS_Viewer
      ///   object
      /// </param>
      /// <returns>
      ///   Bitmap (newly create bitmap if _bmp is nil)
      /// </returns>
      function  DrawBmp             ( const _bitmap  : TBitmap ;
                                      const _scale   : Double  ;
                                      const _ppi     : Integer
                                    ) : TBitmap ;

      /// <inheritdoc from="IGIS_PrintableControl"/>
      procedure PrintBmp            ( const _bitmap  : TGIS_Bitmap
                                    ) ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_PrintableControl"/>
      function  CreateCopy          : IGIS_PrintableControl ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_PrintableControl"/>
      procedure FreeCopy            ( const _control : IGIS_PrintableControl
                                    ) ;

      /// <summary>
      ///   Prints the current state of the legend to the clipboard as a bitmap.
      /// </summary>
      procedure PrintClipboard      ;

    protected // IGIS_LegendParent

      /// <inheritdoc from="IGIS_LegendParent"/>
      function  ControlWidth               : Integer ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      function  ControlHeight              : Integer ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      function  ControlClientWidth         : Integer ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      function  ControlClientHeight        : Integer ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlTouch               ( const _context : TObject
                                           ) ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      function  ControlGetVScrollPosition  : Integer ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlSetVScrollPosition  ( const _position : Integer
                                           ) ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      function  ControlGetHScrollPosition  : Integer ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlSetHScrollPosition  ( const _position : Integer
                                           ) ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlSetVScroll          ( const _minPosition : Integer ;
                                             const _maxPosition : Integer ;
                                             const _position    : Integer
                                           ) ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlSetHScroll          ( const _minPosition : Integer ;
                                             const _maxPosition : Integer ;
                                             const _position    : Integer
                                           ) ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlRepaint             ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      function  ControlIsDesignMode        : Boolean ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      function  ControlRightToLeft         : Boolean ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlSubscribedUpdate    ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlUpdate              ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlFullUpdate          ( const _updvwr   : Boolean
                                           ) ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlDrawNode            ( const _node     : TObject
                                           ) ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlDragDrawBar         ( const _node     : TObject ;
                                             const _top      : Integer
                                           ) ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlDragPrepareNode     ( const _node     : TObject
                                           ) ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlDragDrawNode        ( const _left     : Integer ;
                                             const _top      : Integer ;
                                             const _transparency
                                                             : Single
                                           ) ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlDragFreeNode        ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlDragCreateTemporaryContext ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlDragRenderTemporaryContext ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlDrawFromCache       ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      function  ControlRenderer            : TObject ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      function  ControlStyleGetColor       : TGIS_Color ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlStyleSetColor       ( const _selected : Boolean
                                           ) ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlStyleDrawRectangle  ( const _rect     : TRect
                                           ) ; overload ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlStyleDrawRectangle  ( const _left     : Integer ;
                                             const _top      : Integer ;
                                             const _width    : Integer ;
                                             const _height   : Integer ;
                                             const _brush    : TGIS_Color ;
                                             const _pen      : TGIS_Color
                                           ) ; overload ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlStyleDrawCheckBox   ( const _checked  : Boolean ;
                                             const _rect     : TRect
                                           ) ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlStyleDrawExpandCollapseMarker
                                           ( const _expanded : Boolean ;
                                             const _rect     : TRect
                                           ) ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      function  ControlStyleGetTextExtent  ( const _selected : Boolean ;
                                             const _text     : String
                                           ) : TPoint ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlStyleDrawText       ( const _selected : Boolean ;
                                             const _text     : String  ;
                                                   _rect     : TRect
                                           ) ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlStyleDrawImage      ( const _bitmap   : TGIS_Bitmap ;
                                             const _left     : Integer ;
                                             const _top      : Integer ;
                                             const _transparent
                                                             : Boolean
                                           ) ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlStyleCreateTemporaryContext
                                           ( const _width    : Integer ;
                                             const _height   : Integer
                                           ) ;

      /// <inheritdoc from="IGIS_LegendParent"/>
      procedure ControlStyleRenderTemporaryContext
                                           ( const _left     : Integer ;
                                             const _top      : Integer
                                           ) ;

    public

      /// <summary>
      ///   Selected node.
      /// </summary>
      property SelectedNode : TGIS_TreeNode
                              read  fget_SelectedNode ;

    public

      /// <summary>
      ///   Selected layer.
      /// </summary>
      property GIS_Layer    : TGIS_Layer
                              read  fget_Layer
                              write fset_Layer ;

      /// <summary>
      ///   Selected layers.
      /// </summary>
      property GIS_Layers    : TGIS_LayerAbstractList
                              read  fget_Layers
                              write fset_Layers ;

      /// <summary>
      ///   Selected group.
      /// </summary>
      property GIS_Group    : IGIS_HierarchyGroup
                              read  fget_Group
                              write fset_Group ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_PrintableControl"/>
      property InternalName : String
                              read  fget_InternalName
                              write fset_InternalName ;

    published

      /// <summary>
      ///   Attached IGIS_Viewer object.
      /// </summary>
      property GIS_Viewer   : IGIS_Viewer
                              read  fget_Viewer
                              write fset_Viewer ;

      /// <summary>
      ///   Mode of the legend - list of layers or grouped tree view.
      /// </summary>
      property Mode         : TGIS_ControlLegendMode
                              read  fget_Mode
                              write fset_Mode
                              default TGIS_ControlLegendMode.Layers ;

      /// <summary>
      ///   Options of the legend.
      /// </summary>
      property Options      : TGIS_ControlLegendOptions
                              read  fget_Options
                              write fset_Options ;

      /// <summary>
      ///   True if the order of legend entries in the Layer mode should be
      ///   reverse, i.e. the topmost layer in the attached IGIS_Viewer
      ///   is bottommost in the legend.
      /// </summary>
      property ReverseOrder : Boolean
                              read  fget_ReverseOrder
                              write fset_ReverseOrder
                              default False ;

      /// <summary>
      ///   If True then the legend view is compacted - icons are smaller.
      /// </summary>
      property CompactView  : Boolean
                              read  fget_CompactView
                              write fset_CompactView
                              default False ;

      /// <summary>
      ///   Draw style of legend icons.
      /// </summary>
      property DrawIconStyle  : TGIS_LegendIconStyle
                                read  fget_DrawIconStyle
                                write fset_DrawIconStyle
                                default TGIS_LegendIconStyle.Default ;

      /// <summary>
      ///   Options defining dialogs behavior.
      /// </summary>
      property DialogOptions : TGIS_ControlLegendDialogOptions
                               read  fget_DialogOptions
                               write fset_DialogOptions ;

      /// <summary>
      ///   Selected item color backround. Used if StyleElements does not
      ///   include seFont.
      /// </summary>
      property SelectionColor     : TColor
                                    read  FSelectionColor
                                    write fset_SelectionColor;
      /// <summary>
      ///   Selected item font color. Used if StyleElements does not
      ///   include seFont.
      /// </summary>
      property SelectionFontColor : TColor
                                    read FSelectionFontColor
                                    write fset_SelectionFontColor;

    published

      /// <summary>
      ///   Defines which BiDi we are using: the one from property or the one from
      ///   translation
      /// </summary>
      property BiDiModeFromTranslation
                            : Boolean
                              read  FBiDiModeFromTranslation
                              write fset_BiDiModeFromTranslation
                              default True ;

      /// <summary>
      ///   Border style for component windows. Can be: bsSingle or bsNone.
      /// </summary>
      property BorderStyle  : TBorderStyle
                              read  fget_BorderStyle
                              write fset_BorderStyle
                              default bsSingle ;

    published //events

      /// <event/>
      /// <summary>
      ///   Event fired when a layer parameters get changed.
      /// </summary>
      property LayerParamsChangeEvent : TGIS_LayerEvent
                                        read  FOnLayerParamsChange
                                        write FOnLayerParamsChange ;

      /// <event/>
      /// <summary>
      ///   Event fired when a layer is activated/deactivated.
      /// </summary>
      property LayerActiveChangeEvent : TGIS_LayerEvent
                                        read  FOnLayerActiveChange
                                        write FOnLayerActiveChange ;

      /// <event/>
      /// <summary>
      ///   Event fired upon layer selection.
      /// </summary>
      property LayerSelectEvent       : TGIS_LayerEvent
                                        read  FOnLayerSelect
                                        write FOnLayerSelect ;

      /// <event/>
      /// <summary>
      ///   Event fired when a group is activated/deactivated.
      /// </summary>
      property GroupActiveChangeEvent : TGIS_HierarchyGroupEvent
                                        read  FOnGroupActiveChange
                                        write FOnGroupActiveChange ;

      /// <event/>
      /// <summary>
      ///   Event fired upon group selection.
      /// </summary>
      property GroupSelectEvent       : TGIS_HierarchyGroupEvent
                                        read  FOnGroupSelect
                                        write FOnGroupSelect ;

      /// <event/>
      /// <summary>
      ///   Event fired upon a change of the order of layers.
      /// </summary>
      property OrderChangeEvent       : TNotifyEvent
                                        read  FOnOrderChange
                                        write FOnOrderChange ;

      /// <event/>
      /// <summary>
      ///   Event fired upon opening of the layer properties dialog box.
      /// </summary>
      property OpenDialogEvent        : TGIS_LayerEvent
                                        read  FOnOpenDialog
                                        write FOnOpenDialog ;

      /// <event/>
      /// <summary>
      ///   TapSimple event. Will be fired upon press down/up.
      /// </summary>
      property TapSimpleEvent         : TMouseEvent
                                        read  FOnTapSimple
                                        write FOnTapSimple ;

      /// <event/>
      /// <summary>
      ///   TapDouble event. Will be fired upon double press down/up.
      /// </summary>
      property TapDoubleEvent         : TMouseEvent
                                        read  FOnTapDouble
                                        write FOnTapDouble ;

      /// <event/>
      /// <summary>
      ///   TapLong event. Will be fired upon longer press down.
      /// </summary>
      property TapLongEvent           : TMouseEvent
                                        read  FOnTapLong
                                        write FOnTapLong ;

    published // properties derived from base class

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      ///
      property ParentBiDiMode write fset_ParentBiDiMode ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property BiDiMode ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property Align ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property Anchors ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property Ctl3D ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property Color default clWindow ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property Enabled ;

      {#gendoc:hide:GENPDK}
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property Font : TFont read fget_Font write fset_Font ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property HelpContext ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property Hint ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property ParentColor default False ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property ParentCtl3D ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property ParentFont default False ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property ParentShowHint ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property PopupMenu ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property TabStop  ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property TabOrder ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property Visible ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property Touch ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property StyleElements;

      {$IFDEF LEVEL_RX11_VCL}
        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property StyleName;
      {$ENDIF}

    published
      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnClick
               {$IFDEF GENDOC}
                 : TNotifyEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnDblClick
               {$IFDEF GENDOC}
                 : TNotifyEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnEnter ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnExit ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnKeyDown
               {$IFDEF GENDOC}
                 : TKeyEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnKeyPress
               {$IFDEF GENDOC}
                 : TKeyPressEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnKeyUp
               {$IFDEF GENDOC}
                 : TKeyEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnMouseDown
               {$IFDEF GENDOC}
                 : TMouseEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnMouseMove
               {$IFDEF GENDOC}
                 : TMouseMoveEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnMouseUp
               {$IFDEF GENDOC}
                 : TMouseEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnMouseWheel
               {$IFDEF GENDOC}
                 : TMouseWheelEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnMouseWheelUp
               {$IFDEF GENDOC}
                 : TMouseWheelUpDownEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnMouseWheelDown
               {$IFDEF GENDOC}
                 : TMouseWheelUpDownEvent read dummy write dummy
               {$ENDIF} ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnDragDrop ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnDragOver ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnEndDrag ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnStartDrag ;

      /// <event/>
      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnGesture ;

      /// <event/>
      /// <summary>
      ///   Standard VCL event. See platform documentation.
      /// </summary>
      property OnResize ;

    {$IFDEF GIS_XDK}
      public
        {#gendoc:hide:GENXDK}
        XDK : TGIS_ControlXDK ;
    {$ENDIF}
  end ;

 {#gendoc:hide}
 procedure Register ;

//##############################################################################
implementation

{$R GisControlLegend_16x16.RES}

uses
  System.UITypes,
  System.SysUtils,
  VCL.Themes,
  VCL.Clipbrd,

  GisTypes,
  GisInternals,
  GisResource,
  GisRendererAbstract,
  VCL.GisFramework,
  VCL.GisControlLegendForm,
  VCL.GisPrintLegend ;

const
  LEGEND_SELECTION_COLOR     = $FFE8CC ;
  LEGEND_SELECTION_FONTCOLOR = clBlack ;

//==============================================================================
// properties access functions
//==============================================================================

  function TGIS_ControlLegend.fget_Viewer
    : IGIS_Viewer ;
  begin
    Result := oLegend.GIS_Viewer ;
  end ;

  procedure TGIS_ControlLegend.fset_Viewer(
    const _value : IGIS_Viewer
  ) ;
  begin
    oLegend.GIS_Viewer := _value ;
    FreeObject( bmpCache ) ;
    FreeObject( bmpCacheOld ) ;
  end ;

  function TGIS_ControlLegend.fget_Layer
    : TGIS_Layer ;
  begin
    Result := oLegend.GIS_Layer ;
  end ;

  procedure TGIS_ControlLegend.fset_Layer(
    const _value : TGIS_Layer
  ) ;
  begin
    oLegend.GIS_Layer := _value ;
  end ;

  function TGIS_ControlLegend.fget_Layers : TGIS_LayerAbstractList ;
  begin
    Result := oLegend.GIS_Layers ;
  end ;

  procedure TGIS_ControlLegend.fset_Layers(
    const _value : TGIS_LayerAbstractList
  ) ;
  begin
    oLegend.GIS_Layers := _value ;
  end ;

  function TGIS_ControlLegend.fget_Group
    : IGIS_HierarchyGroup ;
  begin
    Result := oLegend.GIS_Group ;
  end ;

  procedure TGIS_ControlLegend.fset_Group(
    const _value : IGIS_HierarchyGroup
  ) ;
  begin
    oLegend.GIS_Group := _value ;
  end ;

  function TGIS_ControlLegend.fget_Mode
    : TGIS_ControlLegendMode ;
  begin
    Result := oLegend.Mode ;
  end ;

  procedure TGIS_ControlLegend.fset_Mode(
    const _value : TGIS_ControlLegendMode
  ) ;
  begin
    oLegend.Mode := _value ;
  end ;

  function TGIS_ControlLegend.fget_Options
    : TGIS_ControlLegendOptions ;
  begin
    Result := oLegend.Options ;
  end ;

  procedure TGIS_ControlLegend.fset_Options(
    const _value : TGIS_ControlLegendOptions
  ) ;
  begin
    oLegend.Options := _value ;

    if TGIS_ControlLegendOption.AllowMove in oLegend.Options then
      opAllowMove := True
    else
      opAllowMove := False ;

    if TGIS_ControlLegendOption.AllowActive in oLegend.Options then
      opAllowActive := True
    else
      opAllowActive := False ;

    if TGIS_ControlLegendOption.AllowExpand in oLegend.Options then
      opAllowExpand := True
    else
      opAllowExpand := False ;

    if TGIS_ControlLegendOption.AllowParams in oLegend.Options then
      opAllowParams := True
    else
      opAllowParams := False ;

    if TGIS_ControlLegendOption.AllowSelect in oLegend.Options then
      opAllowSelect := True
    else
      opAllowSelect := False ;

    if TGIS_ControlLegendOption.ShowHiddenLayers in oLegend.Options then
      opShowHiddenLayers := True
    else
      opShowHiddenLayers := False ;

    if TGIS_ControlLegendOption.ShowLayerFullPath in oLegend.Options then
      opShowLayerFullPath := True
    else
      opShowLayerFullPath := False ;

    if TGIS_ControlLegendOption.ShowSubLayers in oLegend.Options then
      opShowSubLayers := True
    else
      opShowSubLayers := False ;

    if TGIS_ControlLegendOption.AllowParamsVisible in oLegend.Options then
      opAllowParamsVisible := True
    else
      opAllowParamsVisible := False ;
  end ;

  function TGIS_ControlLegend.fget_ReverseOrder
    : Boolean ;
  begin
    Result := oLegend.ReverseOrder ;
  end ;

  procedure TGIS_ControlLegend.fset_ReverseOrder(
    const _value : Boolean
  ) ;
  begin
    oLegend.ReverseOrder := _value ;
  end ;

  function TGIS_ControlLegend.fget_CompactView
    : Boolean ;
  begin
    Result := oLegend.CompactView ;
  end ;

  procedure TGIS_ControlLegend.fset_CompactView(
    const _value : Boolean
  ) ;
  begin
    oLegend.CompactView := _value ;
  end ;

  function TGIS_ControlLegend.fget_DrawIconStyle : TGIS_LegendIconStyle ;
  begin
    Result := oLegend.DrawIconStyle ;
  end ;

  procedure TGIS_ControlLegend.fset_DrawIconStyle(
    const _value : TGIS_LegendIconStyle
  ) ;
  begin
    oLegend.DrawIconStyle := _value ;
  end ;

  function TGIS_ControlLegend.fget_DialogOptions
    : TGIS_ControlLegendDialogOptions ;
  begin
    Result := oLegend.DialogOptions ;
  end ;

  procedure TGIS_ControlLegend.fset_DialogOptions(
    const _value : TGIS_ControlLegendDialogOptions
  );
  begin
    oLegend.DialogOptions := _value ;
  end ;

  function TGIS_ControlLegend.fget_InternalName
    : String ;
  begin
    Result := oLegend.InternalName ;
  end ;

  procedure TGIS_ControlLegend.fset_InternalName(
    const _value : String
  ) ;
  begin
    oLegend.InternalName := _value ;
  end ;

  procedure TGIS_ControlLegend.fset_BiDiModeFromTranslation(
    const _value : Boolean
  ) ;
  begin
    if _value <> FBiDiModeFromTranslation then begin
      FBiDiModeFromTranslation := _value ;
      ParentBiDiMode := not FBiDiModeFromTranslation ;

      if FBiDiModeFromTranslation then begin
        ParentBiDiMode := False ;
        if _rsbidi then begin
          if BiDiMode <> TBiDiMode.bdRightToLeft then
            BiDiMode := TBiDiMode.bdRightToLeft
        end
        else begin
          if BiDiMode <> TBiDiMode.bdLeftToRight then
            BiDiMode := TBiDiMode.bdLeftToRight
        end ;
      end ;
    end ;
  end ;

  function TGIS_ControlLegend.fget_BorderStyle
    : TBorderStyle ;
  begin
    Result := oRenderer.GetBorderStyle ;
  end ;

  procedure TGIS_ControlLegend.fset_BorderStyle(
    const _value : TBorderStyle
  ) ;
  begin
    if _value <> oRenderer.GetBorderStyle then begin
      oRenderer.SetBorderStyle( _value ) ;
      RecreateWnd ;
    end ;
  end ;

  procedure TGIS_ControlLegend.fset_SelectionColor(
    const _value : TColor
  ) ;
  begin
    FSelectionColor := _value ;

  end;

  procedure TGIS_ControlLegend.fset_SelectionFontColor(
    const _value : TColor
  ) ;
  begin
    FSelectionFontColor := _value ;
  end;

  function TGIS_ControlLegend.fget_SelectedNode
    : TGIS_TreeNode ;
  begin
    Result := oLegend.SelectedNode ;
  end ;

  procedure TGIS_ControlLegend.fset_ParentBiDiMode(
    const _value : Boolean
  ) ;
  begin
    if _value <> ParentBiDiMode then begin
      inherited ParentBiDiMode := _value ;
      if ParentBiDiMode = true then
        FBiDiModeFromTranslation := not ParentBiDiMode ;
    end ;
  end ;

  function TGIS_ControlLegend.fget_Font
    : TFont ;
  begin
    Result := FFont ;
  end ;

  procedure TGIS_ControlLegend.fset_Font(
    const _value : TFont
  ) ;
  begin
    FFont.Assign( _value ) ;
  end ;

//==============================================================================
// events handling
//==============================================================================

  procedure TGIS_ControlLegend.doFontChanged(
    _sender : TObject
  ) ;
  begin
    if IsStringEmpty( InternalName ) then
      Repaint ;
  end ;

  procedure TGIS_ControlLegend.doLayerParamsChange(
    _sender : TObject ;
    _layer  : TGIS_Layer
  ) ;
  begin
    if assigned( FOnLayerParamsChange ) then
      FOnLayerParamsChange( Self, _layer ) ;
  end ;

  procedure TGIS_ControlLegend.doLayerActiveChange(
    _sender : TObject ;
    _layer  : TGIS_Layer
  ) ;
  begin
    if assigned( FOnLayerActiveChange ) then
      FOnLayerActiveChange( Self, _layer ) ;
  end ;

  procedure TGIS_ControlLegend.doLayerSelect(
    _sender : TObject ;
    _layer  : TGIS_Layer
  ) ;
  begin
    if assigned( FOnLayerSelect ) then
      FOnLayerSelect( Self, _layer ) ;
  end ;

  procedure TGIS_ControlLegend.doGroupActiveChange(
    _sender : TObject ;
    _group  : IGIS_HierarchyGroup
  ) ;
  begin
    if assigned( FOnGroupActiveChange ) then
      FOnGroupActiveChange( Self, _group ) ;
  end ;

  procedure TGIS_ControlLegend.doGroupSelect(
    _sender : TObject ;
    _group  : IGIS_HierarchyGroup
  ) ;
  begin
    if assigned( FOnGroupSelect ) then
      FOnGroupSelect( Self, _group ) ;
  end ;

  procedure TGIS_ControlLegend.doOrderChange(
    _sender : TObject
  ) ;
  begin
    if assigned( FOnOrderChange ) then
      FOnOrderChange( Self ) ;
  end ;

  procedure TGIS_ControlLegend.doOpenDialog(
    _sender : TObject ;
    _layer  : TGIS_Layer
  ) ;
  begin
    if assigned( FOnOpenDialog ) then
      FOnOpenDialog( Self, _layer ) ;
  end ;

  procedure TGIS_ControlLegend.msgEraseBkGnd(
    var _msg: TWMPaint
  );
  begin
    if csDesigning in ComponentState then begin
      inherited
    end
    else begin
      // do nothing
    end ;

    _msg.Result := 1 ;
  end;

  procedure TGIS_ControlLegend.msgPaint(
    var _msg : TWMPaint
  ) ;
  begin
    if not IsStringEmpty( InternalName ) then exit ;

    oLegend.FixDataConsistency ;
    inherited ;
  end ;

  procedure TGIS_ControlLegend.msgGetDlgCode(
    var _msg : TMessage
  ) ;
  begin
    _msg.Result := $FFFF ;
  end ;

  procedure TGIS_ControlLegend.msgVScroll(
    var _msg : TWMVScroll
  ) ;
  var
    new_val : Integer  ;
    old_val : Integer  ;
    size    : Integer  ;
    min_pos : Integer  ;
    max_pos : Integer  ;
  begin
    // store current position
    old_val := GetScrollPos( Handle, SB_VERT ) ;
    GetScrollRange( Handle, SB_VERT, min_pos, max_pos ) ;

    // calculate size
    size := ClientHeight ;

    // respond to the scroll bar events
    case _msg.ScrollCode of
      SB_LINEUP:         // by 1/10 of window
         new_val := old_val - ( size div 10 + 1 ) ;
      SB_LINEDOWN:       // by 1/10 of window
         new_val := old_val + ( size div 10 + 1 ) ;
      SB_PAGEUP:         // by whole window (with thin overlapping border)
         new_val := old_val - size ;
      SB_PAGEDOWN:       // by whole window (with thin overlapping border)
         new_val := old_val + size ;
      SB_THUMBPOSITION:  // to the thumb position
         new_val := _msg.Pos ;
      SB_THUMBTRACK:     // to the thumb position
         new_val := _msg.Pos ;
      SB_BOTTOM:         // to the beginning
         new_val := 99999    ;
      SB_TOP:            // to the end
         new_val := 0;
    else
        exit ;
    end ;

    // scroll window
    if new_val > (oLegend.LegendLength - ClientHeight) then
      new_val := oLegend.LegendLength - ClientHeight ;
    if new_val < 0 then
      new_val := 0 ;

    oLegend.TopOffset := - RoundS( new_val ) ;
  end ;

  procedure TGIS_ControlLegend.msgHScroll(
    var _msg : TWMHScroll
  ) ;
  begin
    inherited ;
    oLegend.ValHScroll := GetScrollPos( Handle, _msg.ScrollBar ) ;
    oLegend.DifHScroll := oLegend.MaxHScroll - oLegend.ValHScroll ;
    Repaint ;
  end ;

  procedure TGIS_ControlLegend.msgTouch(
    var _msg : TMessage
  ) ;
  var
    cInputs : Cardinal ;
    pInputs : TTouchInput ;
    handled : Boolean ;
    pt      : TPoint  ;
  begin
    cInputs := LOWORD( _msg.WParam ) ;

    handled := False ;

    if cInputs > 0 then begin
      if GetTouchInputInfo( _msg.LParam, 1, @pInputs,
                            SizeOf( TTouchInput ) ) then begin
        pt := ScreenToClient( Point( RoundS(pInputs.x / 100), RoundS(pInputs.y / 100) ) ) ;
        if ( pInputs.dwFlags and TOUCHEVENTF_DOWN ) = TOUCHEVENTF_DOWN then begin
          SetFocus ;
          oLegend.TouchDown( pt.X, pt.Y, handled ) ;
          if not handled then
            oGestHlpr.GestureMouseDown(
              ( GetKeyState( VK_SHIFT ) = 1 ),
              ( GetKeyState( VK_MENU ) = 1 ),
              ( GetKeyState( VK_CONTROL ) = 1 ),
              True,
              False,
              False,
              pt.X,
              pt.Y
            ) ;
          handled := True ;
        end
        else
        if ( pInputs.dwFlags and TOUCHEVENTF_UP ) = TOUCHEVENTF_UP then begin
          oGestHlpr.GestureMouseUp(
            ( GetKeyState( VK_SHIFT ) = 1 ),
            ( GetKeyState( VK_MENU ) = 1 ),
            ( GetKeyState( VK_CONTROL ) = 1 ),
            True,
            False,
            False,
            pt.X,
            pt.Y
          ) ;
          oLegend.TouchUp( pt.X, pt.Y ) ;
          handled := True ;
        end
        else
        if ( pInputs.dwFlags and TOUCHEVENTF_MOVE ) = TOUCHEVENTF_MOVE then begin
          oGestHlpr.GestureMouseMoveEx(
            ( GetKeyState( VK_SHIFT ) = 1 ),
            ( GetKeyState( VK_MENU ) = 1 ),
            ( GetKeyState( VK_CONTROL ) = 1 ),
            True,
            False,
            False,
            pt.X,
            pt.Y
          ) ;
          oLegend.TouchMove( pt.X, pt.Y, not oGestHlpr.GestureNoMovement ) ;
          handled := True ;
        end ;
      end
      else begin
        // GetLastError() and error handling
      end ;
    end
    else begin
      // error handling, presumably out of memory
    end ;
    if handled then begin
      CloseTouchInputHandle( _msg.LParam ) ;
      _msg.Result := 0 ;
    end
    else
      inherited ;
  end ;

  procedure TGIS_ControlLegend.msgDestroy(
    var _msg : TMessage
  ) ;
  begin
    if bTouchesRegistered then
      UnregisterTouchWindow( Handle ) ;
    inherited ;
  end ;

  procedure TGIS_ControlLegend.msgCtl3DChanged(
    var _msg : TMessage
  );
  begin
    RecreateWnd;
  end ;

  procedure TGIS_ControlLegend.doMouseWheelProc(
    _sender      : TObject ;
    _shift       : TShiftState ;
    _wheelDelta  : Integer ;
    _mousePos    : TPoint ;
    var _handled : Boolean
  ) ;
  begin
    _handled := false;
    if PtInRect( ClientRect, ScreenToClient( _mousePos ) ) then begin
      _handled := True;
      oLegend.MouseWheel( _wheelDelta ) ;
    end ;
  end ;

  procedure TGIS_ControlLegend.doMouseLeave(
    _sender  : TObject
  ) ;
  begin
    oLegend.MouseLeave ;
  end ;

  procedure TGIS_ControlLegend.updateTap(
    _sender : TObject
  ) ;
  var
    btn_state   : TMouseButton ;
    shift_state : TShiftState  ;
  begin
    btn_state   := TMouseButton.mbLeft ;
    shift_state := []  ;

    if oGestHlpr.GestureState.Left then begin
      btn_state   := TMouseButton.mbLeft ;
      shift_state := shift_state + [ssLeft] ;
    end ;
    if oGestHlpr.GestureState.Right then begin
      btn_state   := TMouseButton.mbRight ;
      shift_state := shift_state + [ssRight] ;
    end ;
    if oGestHlpr.GestureState.Middle then begin
      btn_state   := TMouseButton.mbMiddle ;
      shift_state := shift_state + [ssMiddle] ;
    end ;
    if oGestHlpr.GestureState.Shift then begin
      shift_state := shift_state + [ssShift] ;
    end ;
    if oGestHlpr.GestureState.Alt then begin
      shift_state := shift_state + [ssAlt] ;
    end ;
    if oGestHlpr.GestureState.Ctrl then begin
      shift_state := shift_state + [ssCtrl] ;
    end ;

    case oGestHlpr.GestureState.DownCount of
      1 : TapSingle( btn_state,
                     shift_state,
                     oGestHlpr.GestureState.DownX,
                     oGestHlpr.GestureState.DownY
                   ) ;
      2 : TapDouble( btn_state,
                     shift_state,
                     oGestHlpr.GestureState.DownX,
                     oGestHlpr.GestureState.DownY
                   ) ;
      3 : TapLong  ( btn_state,
                     shift_state,
                     oGestHlpr.GestureState.DownX,
                     oGestHlpr.GestureState.DownY
                   ) ;
    end ;
  end ;

  procedure TGIS_ControlLegend.doTimer(
    _sender : TObject
  ) ;
  begin
    if GetKeyState( VK_CONTROL ) < 0 then
      exit ;

    if assigned( GIS_Viewer ) then
      GIS_Viewer.InvalidateWholeMap ;

    oTimer.Enabled := False ;
  end ;

//==============================================================================
// private methods
//==============================================================================

  procedure TGIS_ControlLegend.updatePPI ;
  {$IFDEF LEVEL_RX10_VCL}
    var
      frm : TCustomForm ;
      ppi : Integer ;
  {$ENDIF}
  begin
    if not assigned( oRenderer ) then exit ;

    {$IFDEF LEVEL_RX10_VCL}
      ppi := Screen.PixelsPerInch ;
      frm := GetParentForm( self, True ) ;
      if Assigned( frm ) then
        ppi := frm.Monitor.PixelsPerInch ;

      ppi := RoundS( ppi * GUIScale ) ;

      if ppi <> oLegend.PPI then begin
        oLegend.PPI := ppi ;
        oRenderer.SetPPI( oLegend.PPI ) ;
      end ;
    {$ENDIF}
    if ParentFont then
      oRenderer.SetFont( inherited Font )
    else
      oRenderer.SetFont( Font ) ;
    oLegend.UpdatePPI( oRenderer.GetFont.Size ) ;
  end ;

  procedure TGIS_ControlLegend.drawNode(
    const _node : TGIS_TreeNode ;
    const _bmp  : TBitmap
  ) ;
  var
    cnv : TCanvas ;
  begin
    if assigned( _bmp ) then
      cnv := _bmp.Canvas
    else
      cnv := bmpCache.Canvas ;
    try
      oRenderer.CreateContext( cnv, ControlRightToLeft ) ;
      oLegend.DrawNode( _node, assigned( _bmp ) ) ;
    finally
      oRenderer.FreeContext ;
    end ;
  end ;

  procedure TGIS_ControlLegend.doSubscribedUpdate ;
  var
    hmin   : Integer ;
    hmax   : Integer ;
    redraw : Boolean ;
  begin
    if oLegend.IsUpdateLocked then exit ;

    GetScrollRange( Handle, SB_HORZ, hmin, hmax ) ;
    if ( BiDiMode <> modeBiDi ) or ( hmax <> prvHScroll ) then begin
      if ( ControlRightToLeft ) then begin
        oLegend.MaxHScroll := Width ;
        SetScrollPos( Handle, SB_HORZ, oLegend.MaxHScroll, True ) ;
        oLegend.ValHScroll := GetScrollPos( Handle, SB_HORZ ) ;
        oLegend.MaxHScroll := Width + oLegend.ValHScroll ;
        oLegend.DifHScroll := oLegend.MaxHScroll - oLegend.ValHScroll ;
      end
      else begin
        SetScrollPos( Handle, SB_HORZ, 0, True ) ;
        oLegend.MaxHScroll := hmax ;
        oLegend.ValHScroll := 0 ;
        oLegend.DifHScroll := hmax ;
      end ;
    end ;
    prvHScroll := hmax ;

    updatePPI ;

    redraw := setRenderer ;

    oLegend.DoUpdate( redraw ) ;
  end ;

  function TGIS_ControlLegend.setRenderer
    : Boolean ;
  var
    renderer : TGIS_RendererAbstract ;
  begin
    Result := False ;
    renderer := nil ;
    if not assigned( GIS_Viewer ) then exit ;
    if GIS_Viewer is TGIS_ViewerWnd then
      renderer := TGIS_ViewerWnd( GIS_Viewer ).Renderer ;
    if not assigned( renderer ) then exit ;
    if not IsSameType( oLegend.Renderer, renderer ) then begin
      oLegend.Renderer := renderer.CreateInstance ;
      Result := True ;
    end ;
  end ;

  procedure TGIS_ControlLegend.openLegendForm ;
  var
    dt  : TGIS_LegendItemData ;
    dlg : TGIS_ControlLegendForm ;
    hlp : TGIS_HelpEvent ;
    la  : TGIS_Layer ;
  begin
    if not assigned( SelectedNode ) then
      exit ;

    dt := TGIS_LegendItemData( SelectedNode.Data ) ;

    if dt.DataType <> TGIS_LegendItemType.Layer then
      exit ;

    if not opAllowParams then
      exit ;

    la := dt.Layer ;

    if bFormOpened then exit ;

    dlg := TGIS_ControlLegendForm.Create( Self ) ;
    try
      bFormOpened := True ;
      doOpenDialog( Self, la ) ;

      if assigned( GIS_Viewer ) then
        hlp := GIS_Viewer.AssignedHelpEvent
      else
        hlp := nil ;

      if dlg.Execute( la, Self, hlp ) = mrOK then begin
        doLayerParamsChange( Self, la ) ;
        if assigned( GIS_Viewer ) then
          GIS_Viewer.InvalidateWholeMap ;
      end ;
    finally
      bFormOpened := False ;
      FreeObject( dlg ) ;
    end ;
  end ;

//==============================================================================
// overridden protected methods
//==============================================================================

  procedure TGIS_ControlLegend.TapSingle(
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single       ;
    _y      : Single
  ) ;
  begin
    if assigned( FOnTapSimple ) then
      FOnTapSimple( Self, _button, _shift, RoundS( _x ), RoundS( _y ) ) ;
    oLegend.TapSimple( _button = TMouseButton.mbLeft ) ;
  end ;

  procedure TGIS_ControlLegend.TapDouble(
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single       ;
    _y      : Single
  ) ;
  var
    action : Boolean ;
  begin
    if assigned( FOnTapDouble ) then begin
      FOnTapDouble( Self, _button, _shift, RoundS( _x ), RoundS( _y ) ) ;
      {$IFNDEF GIS_XDK}
        exit ;
      {$ELSE}
        if XDK.MouseTranslated then
          exit ;
      {$ENDIF}
    end ;
    oLegend.TapDouble( _button = TMouseButton.mbLeft, action ) ;
    if action then
      openLegendForm ;
  end ;

  procedure TGIS_ControlLegend.TapLong(
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single       ;
    _y      : Single
  ) ;
  begin
    if assigned( FOnTapLong ) then begin
      FOnTapLong( Self, _button, _shift, RoundS( _x ), RoundS( _y ) ) ;
      {$IFNDEF GIS_XDK}
        exit ;
      {$ELSE}
        if XDK.MouseTranslated then
          exit ;
      {$ENDIF}
    end ;
    oLegend.TapLong( ssLeft in _shift ) ;
  end ;

  procedure TGIS_ControlLegend.KeyDown(
    var _key   : Word ;
        _shift : TShiftState
  ) ;
  begin
    inherited ;
    case _key of
      vkUp       :
        begin
          if ssCtrl in _shift then
            oLegend.DoMove( -1 )
          else
            oLegend.DoSelect( -1 ) ;
        end ;
      vkDown     :
        begin
          if ssCtrl in _shift then
            oLegend.DoMove( 1 )
          else
            oLegend.DoSelect( 1 ) ;
        end ;
      vkLeft     :
        begin
          if ( not assigned( GIS_Viewer ) ) or ( GIS_Viewer.Items.Count <= 0 ) then
            exit ;
          if not assigned( SelectedNode ) then
            exit ;
          if Mode <> TGIS_ControlLegendMode.Layers then
            exit ;
          if SelectedNode.Level = 0 then
            exit ;
          oLegend.SelectedNode := SelectedNode.Parent ;
          Repaint ;
        end ;
      vkRight    :
        begin
          if ( not assigned( GIS_Viewer ) ) or ( GIS_Viewer.Items.Count <= 0 ) then
            exit ;
          if not assigned( SelectedNode ) then
            exit ;
          if Mode <> TGIS_ControlLegendMode.Layers then
            exit ;
          if ( not SelectedNode.Expanded ) or ( SelectedNode.Count = 0 ) then
            exit ;
          oLegend.SelectedNode := SelectedNode.Items[0] ;
          Repaint ;
        end ;
      vkPrior    :
        begin
          if ssCtrl in _shift then
            oLegend.DoMove( -5 )
          else
            oLegend.DoSelect( -5 ) ;
        end ;
      vkNext     :
        begin
          if ssCtrl in _shift then
            oLegend.DoMove( 5 )
          else
            oLegend.DoSelect( 5 ) ;
        end ;
      vkHome     :
        begin
          if ssCtrl in _shift then
            oLegend.DoMove( -99999 )
          else
            oLegend.DoSelect( -99999 ) ;
        end ;
      vkEnd      :
        begin
          if ssCtrl in _shift then
            oLegend.DoMove( 99999 )
          else
            oLegend.DoSelect( 99999 ) ;
        end ;
      vkSpace    :
        begin
          if ( not assigned( GIS_Viewer ) ) or ( GIS_Viewer.Items.Count <= 0 ) then
            exit ;
          if not assigned( SelectedNode ) then
            exit ;
          if Mode <> TGIS_ControlLegendMode.Layers then
            exit ;
          GIS_Layer.Active := not GIS_Layer.Active ;
          ControlFullUpdate( True ) ;
        end ;
        vkAdd      :
        begin
          if ( not assigned( GIS_Viewer ) ) or ( GIS_Viewer.Items.Count <= 0 ) then
            exit ;
          if not assigned( SelectedNode ) then
            exit ;
          if Mode <> TGIS_ControlLegendMode.Layers then
            exit ;
          oLegend.Expand( GIS_Layer ) ;
        end ;
      vkSubtract :
        begin
          if ( not assigned( GIS_Viewer ) ) or ( GIS_Viewer.Items.Count <= 0 ) then
            exit ;
          if not assigned( SelectedNode ) then
            exit ;
          if Mode <> TGIS_ControlLegendMode.Layers then
            exit ;
          oLegend.Collapse( GIS_Layer ) ;
        end ;
      vkReturn   :
        begin
          openLegendForm ;
        end ;
    end ;
  end ;

  procedure TGIS_ControlLegend.MouseDown(
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x      : Integer ;
    _y      : Integer
  ) ;
  var
    handled : Boolean ;
  begin
    inherited ;

    if ssTouch in _shift then
      // handled in msgTouch()
      exit ;

    SetFocus ;

    try
      oLegend.MouseDown( _x, _y, False, ssCtrl in _shift, ssShift in _shift, handled ) ;
    finally
      if not handled then
        oGestHlpr.GestureMouseDown(
          ssShift  in _shift,
          ssAlt    in _shift,
          ssCtrl   in _shift,
          ssLeft   in _shift,
          ssRight  in _shift,
          ssMiddle in _shift,
          _x,
          _y
        ) ;
    end ;
  end ;

  procedure TGIS_ControlLegend.MouseMove(
    _shift : TShiftState ;
    _x     : Integer ;
    _y     : Integer
  ) ;
  begin
    inherited ;

    if ssTouch in _shift then
      // handled in msgTouch()
      exit ;
    if _shift = [] then
      exit ;

    oGestHlpr.GestureMouseMove(
      ssShift  in _shift,
      ssAlt    in _shift,
      ssCtrl   in _shift,
      ssLeft   in _shift,
      ssRight  in _shift,
      ssMiddle in _shift,
      _x,
      _y
    ) ;
    oLegend.MouseMove( _x, _y, ssLeft in _shift ) ;
  end ;

  procedure TGIS_ControlLegend.MouseUp(
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x      : Integer ;
    _y      : Integer
  ) ;
  begin
    inherited ;

    if ssTouch in _shift then
      // handled in msgTouch()
      exit ;

    oGestHlpr.GestureMouseUp(
      ssShift  in _shift,
      ssAlt    in _shift,
      ssCtrl   in _shift,
      ssLeft   in _shift,
      ssRight  in _shift,
      ssMiddle in _shift,
      _x,
      _y
    ) ;
    oLegend.MouseUp( _x, _y ) ;
  end ;

  procedure TGIS_ControlLegend.Paint ;
  var
    w, h : Integer ;
    cl   : TColor ;
    draw_result : Boolean ;
  begin
    if FLock then exit ;

    inherited ;

    w := ControlClientWidth ;
    h := ControlClientHeight ;

    if ( w = 0 ) or ( h = 0 ) then
      exit ;

    if csDesigning in ComponentState then begin
      Canvas.Brush.Color := oRenderer.GetActualColor( Color, False );
      Canvas.FillRect( ClientRect );
      exit ;
    end ;

    if not assigned( oLegend ) then
      exit ;

    if assigned( bmpCache ) then begin
      FreeObject( bmpCacheOld ) ;
      bmpCacheOld := bmpCache ;
    end ;

    bmpCache := TBitmap.Create ;
    bmpCache.Width  := w ;
    bmpCache.Height := h ;
    bmpCache.PixelFormat := pf24bit ;

    // important to pass current Color to oRenderer
    cl := oRenderer.GetActualColor( Color, False ) ;

    bmpCache.Canvas.Pen.Color   := cl ;
    bmpCache.Canvas.Brush.Color := cl ;
    bmpCache.Canvas.Rectangle( 0, 0, w, h ) ;

    FInPaint := True ;
    try
      draw_result := oLegend.DrawNodes ;
    finally
      FInPaint := False ;
    end ;

    if draw_result then begin
      Canvas.Draw( 0, 0, bmpCache ) ;
    end
    else begin
      bmpCache.Canvas.Pen.Color   := cl ;
      bmpCache.Canvas.Brush.Color := cl ;
      bmpCache.Canvas.Rectangle( 0, 0, w, h ) ;
      if assigned( bmpCacheOld ) then begin
        if ControlRightToLeft then
          bmpCache.Canvas.Draw( w - bmpCacheOld.Width, 0, bmpCacheOld )
        else
          bmpCache.Canvas.Draw( 0, 0, bmpCacheOld ) ;
      end ;
      Canvas.Draw( 0, 0, bmpCache ) ;
      if assigned( bmpCacheOld ) then begin
        // bmpCacheOld will be the cache in future
        FreeObject( bmpCache ) ;
      end ;
    end ;
  end ;

  procedure TGIS_ControlLegend.Resize ;
  begin
    if ( not ControlIsDesignMode ) and ( not bTouchesRegistered ) then begin
      RegisterTouchWindow( Handle, 0 ) ;
      bTouchesRegistered := True ;
    end ;
    updatePPI ;
    oLegend.Resize ;
    inherited ;
  end ;

  procedure TGIS_ControlLegend.SetBiDiMode(
    _value : TBiDiMode
  ) ;
  begin
    inherited ;

    ControlRepaint ;

    modeBiDi := BiDiMode ;
  end ;

  procedure TGIS_ControlLegend.CreateParams(
    var _params: TCreateParams
  ) ;
  begin
    inherited createParams( _params ) ;

    with _params do begin
      Style := Style or WS_VSCROLL or WS_HSCROLL;
      if oRenderer.GetBorderStyle = bsSingle then
        if NewStyleControls and Ctl3D then begin
          Style := Style and not WS_BORDER;
          ExStyle := ExStyle or WS_EX_CLIENTEDGE;
        end
        else
          Style := Style or WS_BORDER;
    end ;

    if {$IFDEF LEVEL_RX11_VCL}
         IsCustomStyleActive
       {$ELSE}
         TStyleManager.IsCustomStyleActive
       {$ENDIF}
       and
       ( BorderStyle = bsSingle )
       and
       ( seBorder in StyleElements )
    then
      ControlStyle := ControlStyle + [csNeedsBorderPaint]
    else
      ControlStyle := ControlStyle - [csNeedsBorderPaint] ;
  end ;

//==============================================================================
// new protected methods
//==============================================================================

  procedure TGIS_ControlLegend.Delete(
    _node : TGIS_TreeNode
  ) ;
  var
    dt : TGIS_LegendItemData ;
  begin
    dt := _node.Data as TGIS_LegendItemData ;
    FreeObject( dt ) ;

    inherited ;
  end ;

//==============================================================================
// overridden public methods
//==============================================================================

  procedure TGIS_ControlLegend.Repaint ;
  begin
    updatePPI ;
    setRenderer ;
    inherited ;
  end ;

//==============================================================================
// constructors/destructors
//==============================================================================

  constructor TGIS_ControlLegend.Create(
    _owner : TComponent
  ) ;
  var
    fnt : TFont ;
  begin
    inherited ;

    // set default values
    DoubleBuffered := True ;
    DragMode := TDragMode.dmManual ;
    ParentColor := False ;
    ParentFont  := False ;


    FLock := False ;
    FFont := TFont.Create ;
    FFont.PixelsPerInch := 96 ;
    FFont.Assign( inherited Font ) ;

    // underlying legend abstract
    oLegend := TGIS_Legend.Create( Self ) ;
    oLegend.ExpandRectSize   := GIS_LEGEND_EXPAND_RECT_SIZE ;
    oLegend.CheckBoxRectSize := GIS_LEGEND_CHECKBOX_RECT_SIZE ;
    oLegend.LevelIndent      := GIS_LEGEND_LEVEL_INDENT ;
    oLegend.LayerParamsChangeEvent := doLayerParamsChange ;
    oLegend.LayerActiveChangeEvent := doLayerActiveChange ;
    oLegend.LayerSelectEvent := doLayerSelect ;
    oLegend.GroupActiveChangeEvent := doGroupActiveChange ;
    oLegend.GroupSelectEvent := doGroupSelect ;
    oLegend.OrderChangeEvent := doOrderChange ;
    // for proper options' initialization
    Options := oLegend.Options ;

    oRenderer := TGIS_StyleRendererVCL.Create( Self ) ;
    fnt := TFont.Create ;
    try
      fnt.PixelsPerInch := 96 ;
      fnt.Assign( Font ) ;
      oRenderer.SetFont( fnt ) ;
    finally
      FreeObject( fnt ) ;
    end;
    Font.OnChange := doFontChanged;
    BiDiModeFromTranslation := True ;

    modeBiDi    := BiDiMode ;
    prvHScroll  := 0 ;

    bFormOpened := False ;

    // after oLegend has been created
    Width  := 120 ;
    Height := 100 ;

    // after oRenderer has been created
    updatePPI ;


    FSelectionColor     := LEGEND_SELECTION_COLOR ;
    FSelectionFontColor := LEGEND_SELECTION_FONTCOLOR ;

    FOnLayerParamsChange := nil ;
    FOnLayerActiveChange := nil ;
    FOnLayerSelect := nil ;
    FOnGroupActiveChange := nil ;
    FOnGroupSelect := nil ;
    FOnOrderChange := nil ;
    FOnOpenDialog := nil ;
    FOnTapSimple  := nil ;
    FOnTapDouble  := nil ;
    FOnTapLong    := nil ;

    OnMouseLeave  := doMouseLeave ;
    OnMouseWheel  := doMouseWheelProc ;

    bmpCache      := nil ;
    bmpCacheOld   := nil ;

    oGestHlpr := TGIS_GestureHelper.Create( Self, updateTap ) ;

    oTimer := TTimer.Create( Self ) ;
    oTimer.Interval := GIS_LEGEND_UPDATE_DELAY ;
    oTimer.OnTimer  := doTimer ;
    oTimer.Enabled  := False ;

    bTouchesRegistered := False ;
    Touch.InteractiveGestures := [ ] ;
    Touch.InteractiveGestureOptions := [ ] ;
  end ;

  destructor TGIS_ControlLegend.Destroy ;
  begin
    FreeObject( bmpCache    ) ;
    FreeObject( bmpCacheOld ) ;
    FreeObject( oGestHlpr   ) ;

    // when the license is not valid,
    // the object is destroyed before oLegend is created
    if assigned( oLegend ) then
      GIS_Viewer := nil ;

    FreeObject( oRenderer ) ;
    FreeObject( oLegend ) ;
    FreeObject( FFont ) ;

    inherited ;
  end ;

//==============================================================================
// public methods
//==============================================================================

  function TGIS_ControlLegend.IsExpanded(
    const _layer : TGIS_Layer
  ) : Boolean ;
  begin
    Result := oLegend.IsExpanded( _layer ) ;
  end ;

  function TGIS_ControlLegend.IsExpanded(
    const _group : IGIS_HierarchyGroup
  ) : Boolean ;
  begin
    Result := oLegend.IsExpanded( _group ) ;
  end ;

  procedure TGIS_ControlLegend.Expand(
    const _layer : TGIS_Layer
  ) ;
  begin
    oLegend.Expand( _layer ) ;
  end ;

  procedure TGIS_ControlLegend.Expand(
    const _group : IGIS_HierarchyGroup
  ) ;
  begin
    oLegend.Expand( _group ) ;
  end ;

  procedure TGIS_ControlLegend.Expand(
    const _group  : IGIS_HierarchyGroup ;
    const _deep   : Boolean ;
    const _layers : Boolean
  ) ;
  begin
    oLegend.Expand( _group, _deep, _layers ) ;
  end ;

  procedure TGIS_ControlLegend.Collapse(
    const _layer : TGIS_Layer
  ) ;
  begin
    oLegend.Collapse( _layer ) ;
  end ;

  procedure TGIS_ControlLegend.Collapse(
    const _group : IGIS_HierarchyGroup
  ) ;
  begin
    oLegend.Collapse( _group ) ;
  end ;

  procedure TGIS_ControlLegend.Collapse(
    const _group  : IGIS_HierarchyGroup ;
    const _deep   : Boolean ;
    const _layers : Boolean
  ) ;
  begin
    oLegend.Collapse( _group, _deep, _layers ) ;
  end ;

  function TGIS_ControlLegend.GetNodeAt(
    const _x : Integer ;
    const _y : Integer
  ) : TGIS_TreeNode ;
  begin
    Result := oLegend.GetNodeAt( _x, _y ) ;
  end ;

  procedure TGIS_ControlLegend.InvalidateItems ;
  begin
    oLegend.InvalidateItems ;
  end ;

  function TGIS_ControlLegend.DrawBmp(
    const _bitmap : TBitmap ;
    const _scale  : Double  ;
    const _ppi    : Integer
  ) : TBitmap ;
  var
    r   : TRect ;
    bmp : TGIS_Bitmap ;
    pl  : TGIS_PrintLegend ;
  begin
    Result := _bitmap ;

    if assigned( Result ) then
      r := Rect( 0, 0, Result.Width, Result.Height )
    else
      r := Rect( 0, 0, ControlClientWidth, ControlClientHeight ) ;
    bmp := TGIS_Bitmap.Create( r.Width, r.Height, True );
    try
      pl := TGIS_PrintLegend.Create( Self, bmp ) ;
      try
        pl.Draw( _scale, _ppi ) ;
      finally
        FreeObject( pl ) ;
      end ;
      if not assigned( Result ) then
        Result := TBitmap.Create ;
      Result.Assign( TBitmap( bmp.NativeBitmap ) ) ;
    finally
      FreeObject( bmp ) ;
    end ;
  end ;

  procedure TGIS_ControlLegend.PrintBmp(
    const _bitmap : TGIS_Bitmap
  ) ;
  var
    pl : TGIS_PrintLegend ;
  begin
    pl := TGIS_PrintLegend.Create( Self, _bitmap ) ;
    try
      pl.Draw ;
    finally
      FreeObject( pl ) ;
    end ;
  end ;

  function TGIS_ControlLegend.CreateCopy
    : IGIS_PrintableControl ;
  begin
    FLock := True ;
    st_CompactView := CompactView ;
    st_ReverseOrder := ReverseOrder ;
    st_IconStyle := DrawIconStyle ;
    st_FontName := Font.Name ;
    st_FontSize := Font.Size ;
    st_FontStyle := Font.Style ;
    st_FontColor := Font.Color ;
    InternalName := Name ;
    Result := Self ;
  end ;

  procedure TGIS_ControlLegend.FreeCopy(
    const _control : IGIS_PrintableControl
  ) ;
  var
    fnt : TFont ;
  begin
    CompactView := st_CompactView ;
    ReverseOrder := st_ReverseOrder ;
    DrawIconStyle := st_IconStyle ;
    fnt := TFont.Create ;
    try
      fnt.Name  := st_FontName ;
      fnt.Size  := st_FontSize ;
      fnt.Style := st_FontStyle ;
      fnt.Color := st_FontColor ;
      Font.Assign( fnt ) ;
    finally
      FreeObject( fnt ) ;
    end;
    InternalName := '' ;
    FLock := False ;
    Repaint ;
  end ;

  procedure TGIS_ControlLegend.PrintClipboard ;
  var
    bmp : TGIS_Bitmap ;
    pl  : TGIS_PrintLegend ;
  begin
    bmp := TGIS_Bitmap.Create( Width, Height ) ;
    pl := TGIS_PrintLegend.Create( Self, bmp ) ;
    try
      pl.Draw ;
      Clipboard.Assign( TBitmap(bmp.NativeBitmap) ) ;
    finally
      FreeObject( pl ) ;
      FreeObject( bmp ) ;
    end ;
  end ;

//==============================================================================
// IGIS_LegendParent methods
//==============================================================================

  function TGIS_ControlLegend.ControlWidth
    : Integer ;
  begin
    Result := Width ;
  end ;

  function TGIS_ControlLegend.ControlHeight
    : Integer ;
  begin
    Result := Height ;
  end ;

  function TGIS_ControlLegend.ControlClientWidth
    : Integer ;
  begin
    Result := ClientWidth ;
  end ;

  function TGIS_ControlLegend.ControlClientHeight
    : Integer ;
  begin
    Result := ClientHeight ;
  end ;

  procedure TGIS_ControlLegend.ControlTouch(
    const _context : TObject
  ) ;
  begin

  end ;

  function TGIS_ControlLegend.ControlGetVScrollPosition
    : Integer ;
  begin
    Result := GetScrollPos( Handle, SB_VERT ) ;
  end ;

  procedure TGIS_ControlLegend.ControlSetVScrollPosition(
    const _position : Integer
  ) ;
  begin
    SetScrollPos( Handle, SB_VERT, _position, True ) ;
  end ;

  function TGIS_ControlLegend.ControlGetHScrollPosition
    : Integer ;
  begin
    Result := GetScrollPos( Handle, SB_HORZ ) ;
  end ;

  procedure TGIS_ControlLegend.ControlSetHScrollPosition(
    const _position : Integer
  ) ;
  begin
    SetScrollPos( Handle, SB_HORZ, _position, True ) ;
  end ;

  procedure TGIS_ControlLegend.ControlSetVScroll(
    const _minPosition : Integer ;
    const _maxPosition : Integer ;
    const _position    : Integer
  ) ;
  begin
    if _maxPosition > 0 then begin
      SetScrollRange( Handle, SB_VERT, _minPosition, _maxPosition, False ) ;
      SetScrollPos( Handle, SB_VERT, _position, True ) ;
    end
    else
      SetScrollRange( Handle, SB_VERT, _minPosition, _maxPosition, True ) ;
  end ;

  procedure TGIS_ControlLegend.ControlSetHScroll(
    const _minPosition : Integer ;
    const _maxPosition : Integer ;
    const _position    : Integer
  ) ;
  begin
    if _maxPosition > 0 then begin
      SetScrollRange( Handle, SB_HORZ, _minPosition, _maxPosition, False ) ;
      SetScrollPos( Handle, SB_HORZ, _position, True ) ;
    end
    else
      SetScrollRange( Handle, SB_HORZ, _minPosition, _maxPosition, True ) ;
  end ;

  procedure TGIS_ControlLegend.ControlRepaint ;
  begin
    if FLock then exit ;
    if FInPaint then
      Invalidate
    else
      Repaint ;
  end ;

  function TGIS_ControlLegend.ControlIsDesignMode
    : Boolean ;
  begin
    Result := csDesigning in ComponentState ;
  end ;

  function TGIS_ControlLegend.ControlRightToLeft
    : Boolean ;
  begin
    Result := not ( BiDiMode = TBiDiMode.bdLeftToRight ) ;
  end ;

  procedure TGIS_ControlLegend.ControlSubscribedUpdate ;
  begin
    doSubscribedUpdate ;
  end ;

  procedure TGIS_ControlLegend.ControlUpdate ;
  begin
    doSubscribedUpdate ;
  end ;

  procedure TGIS_ControlLegend.ControlFullUpdate(
    const _updvwr : Boolean
  ) ;
  begin
    Repaint ;

    if not _updvwr then
      exit ;

    oTimer.Enabled := False ;
    oTimer.Enabled := True ;
  end ;

  procedure TGIS_ControlLegend.ControlDrawNode(
    const _node : TObject
  ) ;
  begin
    drawNode( TGIS_TreeNode( _node ), nil ) ;
  end ;

  procedure TGIS_ControlLegend.ControlDragDrawBar(
    const _node : TObject ;
    const _top  : Integer
  ) ;
  var
    cnv : TCanvas ;
    rct : TRect ;
  begin
    if assigned( oCanvasBitmap ) then
      cnv := oCanvasBitmap.Canvas
    else
      cnv := Canvas ;

    rct := oLegend.GetDragBarRect( TGIS_TreeNode( _node ), _top ) ;
    cnv.Pen.Color := $0000FF ;
    cnv.MoveTo( rct.Left,  rct.Top - 1 ) ;
    cnv.LineTo( rct.Right, rct.Top - 1 ) ;
    cnv.MoveTo( rct.Left,  rct.Top ) ;
    cnv.LineTo( rct.Right, rct.Top ) ;
  end ;

  procedure TGIS_ControlLegend.ControlDragPrepareNode(
    const _node : TObject
  ) ;
  var
    node : TGIS_TreeNode ;
    rct  : TRect ;
  begin
    node := TGIS_TreeNode( _node ) ;
    rct := oLegend.GetNodeRect( node, True ) ;
    if rct.Height > Height then
      rct.Height := Height ;
    bmpDrag := Vcl.Graphics.TBitmap.Create ;
    bmpDrag.Width  := rct.Width ;
    bmpDrag.Height := rct.Height ;
    bmpDrag.PixelFormat := pf24bit ;
    drawNode( node, bmpDrag ) ;
  end ;

  procedure TGIS_ControlLegend.ControlDragDrawNode(
    const _left         : Integer ;
    const _top          : Integer ;
    const _transparency : Single
  ) ;
  begin
    if assigned( oCanvasBitmap ) then
      oCanvasBitmap.Canvas.Draw( _left, _top, bmpDrag, RoundS( _transparency * 255 ) )
    else
      Canvas.Draw( _left, _top, bmpDrag, RoundS( _transparency * 255 ) ) ;
  end ;

  procedure TGIS_ControlLegend.ControlDragFreeNode ;
  begin
    FreeObject( bmpDrag ) ;
  end ;

  procedure TGIS_ControlLegend.ControlDrawFromCache ;
  begin
    if assigned( oCanvasBitmap ) then
      oCanvasBitmap.Canvas.Draw( 0, 0, bmpCache )
    else
      Canvas.Draw( 0, 0, bmpCache ) ;
  end ;

  procedure TGIS_ControlLegend.ControlDragCreateTemporaryContext ;
  begin
    oCanvasBitmap := TBitmap.Create ;
    oCanvasBitmap.Width := Width ;
    oCanvasBitmap.Height := Height ;
    oCanvasBitmap.PixelFormat := pf24bit ;
  end ;

  procedure TGIS_ControlLegend.ControlDragRenderTemporaryContext ;
  begin
    Canvas.Draw( 0, 0, oCanvasBitmap ) ;
    FreeObject( oCanvasBitmap ) ;
  end ;

  function TGIS_ControlLegend.ControlRenderer
    : TObject ;
  begin
    Result := oRenderer ;
  end ;

  function TGIS_ControlLegend.ControlStyleGetColor
    : TGIS_Color ;
  begin
    Result := oRenderer.GetBrushColor ;
  end ;

  procedure TGIS_ControlLegend.ControlStyleSetColor(
    const _selected : Boolean
  ) ;
  begin
    oRenderer.SetSelectionColors(
      SelectionColor,
      SelectionFontColor
    );
    oRenderer.SetBrushColor( _selected ) ;
  end ;

  procedure TGIS_ControlLegend.ControlStyleDrawRectangle(
    const _rect : TRect
  ) ;
  begin
    oRenderer.DrawRectangle( _rect ) ;
  end ;

  procedure TGIS_ControlLegend.ControlStyleDrawRectangle(
    const _left   : Integer ;
    const _top    : Integer ;
    const _width  : Integer ;
    const _height : Integer ;
    const _brush  : TGIS_Color ;
    const _pen    : TGIS_Color
  ) ;
  begin
    oRenderer.DrawRectangle( _left, _top, _width, _height, _brush, _pen ) ;
  end ;

  procedure TGIS_ControlLegend.ControlStyleDrawCheckBox(
    const _checked : Boolean ;
    const _rect    : TRect
  ) ;
  begin
    oRenderer.DrawCheckBox( _checked, _rect ) ;
  end ;

  procedure TGIS_ControlLegend.ControlStyleDrawExpandCollapseMarker(
    const _expanded : Boolean ;
    const _rect     : TRect
  ) ;
  begin
    oRenderer.DrawExpandCollapseMarker(  _expanded, _rect ) ;
  end ;

  function TGIS_ControlLegend.ControlStyleGetTextExtent(
    const _selected : Boolean ;
    const _text     : String
  ) : TPoint ;
  begin
    Result := oRenderer.GetTextExtent( _selected, _text ) ;
  end ;

  procedure TGIS_ControlLegend.ControlStyleDrawText(
    const _selected : Boolean ;
    const _text     : String  ;
          _rect     : TRect
  ) ;
  begin
    oRenderer.DrawText( _selected, _text, _rect ) ;
  end ;

  procedure TGIS_ControlLegend.ControlStyleDrawImage(
    const _bitmap : TGIS_Bitmap ;
    const _left   : Integer ;
    const _top    : Integer ;
    const _transparent
                  : Boolean
  ) ;
  begin
    oRenderer.DrawImage( _bitmap, _left, _top, _transparent ) ;
  end ;

  procedure TGIS_ControlLegend.ControlStyleCreateTemporaryContext(
    const _width  : Integer ;
    const _height : Integer
  ) ;
  begin
    oRenderer.CreateTemporaryContext( _width, _height, oLegend.PPI ) ;
  end ;

  procedure TGIS_ControlLegend.ControlStyleRenderTemporaryContext(
    const _left : Integer ;
    const _top  : Integer
  ) ;
  begin
    oRenderer.RenderTemporaryContext( _left, _top ) ;
  end ;

  procedure Register ;
  begin
    RegisterComponents( 'TatukGIS', [ TGIS_ControlLegend ] ) ;
  end ;

initialization
  TCustomStyleEngine.RegisterStyleHook(TGIS_ControlLegend, TScrollingStyleHook);

finalization
  TCustomStyleEngine.UnRegisterStyleHook(TGIS_ControlLegend, TScrollingStyleHook);

//==================================== END =====================================
end.

