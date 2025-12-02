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
  FMX legend control.
}

unit FMX.GisControlLegend ;
{$HPPEMIT '#pragma link "FMX.GisControlLegend"'}

{$INCLUDE GisInclude.inc}

interface

uses
  System.Types,
  System.UITypes,
  System.Classes,

  FMX.Types,
  FMX.StdCtrls,
  FMX.Graphics,
  FMX.Controls,
  FMX.Layouts,
  FMX.Objects,

  GisRtl,
  GisTypesUI,
  GisInterfaces,
  GisLayer,
  GisHierarchy,
  GisLegend,
  GisGestureHelper,
  FMX.GisFramework,
  FMX.GisLegendUtilsFactory,
  FMX.GisStyleRenderer,
  FMX.GisViewerWnd ;

type
  /// <summary>
  ///   Visual Legend component.
  /// </summary>
  /// <remarks>
  ///   Place this component on a form and connect GIS_Viewer
  ///   to IGIS_Viewer object. Selected layer is available via GIS_Layer property.
  /// </remarks>
  [ComponentPlatformsAttribute( pidAllPlatforms )]
  TGIS_ControlLegend = class( TStyledControl,
                              IGIS_LegendParent,
                              IGIS_PrintableControl )
    private
      FBiDiMode                : TBiDiMode ;
      FBiDiModeFromTranslation : Boolean   ;
      FInPaint                 : Boolean   ;

    private
      oLegend              : TGIS_Legend ;
      oRenderer            : TGIS_StyleRendererFMX ;
      oGestHlpr            : TGIS_GestureHelper ;
      oTimer               : TTimer   ;
      bmpCache             : FMX.Graphics.TBitmap ;
      bmpCacheOld          : FMX.Graphics.TBitmap ;
      oDragNode            : TGIS_TreeNode ;
      iDragTop             : Integer  ;
      bDragBar             : Boolean  ;
      bDragBitmapTmp       : Boolean  ;
      bDragFromCache       : Boolean  ;
      bmpDrag              : FMX.Graphics.TBitmap ;
      bFormOpened          : Boolean  ;
      bForceRepaint        : Boolean  ;
      oCanvasBitmap        : FMX.Graphics.TBitmap ;

      oVScroll             : TScrollBar ;
      oHScroll             : TScrollBar ;

    private
      modeBidi             : TBiDiMode   ;
      prvHScroll           : Integer     ;

    private

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
      FOnTapSimple  : TMouseEvent ;

      /// <summary>
      ///   TapDoubleEvent. Will be fired upon double press down/up.
      /// </summary>
      FOnTapDouble  : TMouseEvent ;

      /// <summary>
      ///   TapLongEvent. Will be fired upon longer press down.
      /// </summary>
      FOnTapLong    : TMouseEvent ;

      {$IFDEF LEVEL_RX10_FMX}
        /// <summary>
        ///   Styled settings.
        /// </summary>
        FStyledSettings   : TGIS_ControlLegendStyledSettings ;
      {$ENDIF}
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
      procedure fset_BiDiMode       ( const _value : TBiDiMode
                                    ) ;
      procedure fset_BiDiModeFromTranslation
                                    ( const _value : Boolean
                                    ) ;
      function  fget_BackgroundColor: TAlphaColor ;
      procedure fset_BackgroundColor( const _value : TAlphaColor
                                    ) ;
      function  fget_Font           : TFont ;
      function  fget_FontColor      : TAlphaColor ;
      procedure fset_FontColor      ( const _value : TAlphaColor
                                    ) ;

    private // protected property
      function  fget_SelectedNode   : TGIS_TreeNode ;

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

      /// <summary>
      ///   Callback function of the TGIS_GestureHelper object.
      /// </summary>
      /// <param name="_sender">
      ///   the control which calls the update
      /// </param>
      procedure updateTap           (       _sender     : TObject
                                    ) ;

      /// <summary>
      ///   The event which updates the attached IGIS_Viewer, delayed.
      /// </summary>
      /// <param name="_sender">
      ///   the control which calls the update
      /// </param>
      procedure doTimer             (       _sender     : TObject
                                    ) ;

      procedure doVScrollChange     (       _sender     : TObject
                                    ) ;

      procedure doHScrollChange     (       _sender     : TObject
                                    ) ;

    private

      function  canvasScale         : Single ;

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
                                      const _bmp     : FMX.Graphics.TBitmap
                                    ) ;

      /// <summary>
      ///   Updates the legend when a full repaint is requested by
      ///   the attached IGIS_Viewer.
      /// </summary>
      procedure doSubscribedUpdate  ;

      function  setRenderer         : Boolean ;

      procedure openLegendForm      ;

      procedure disableDragging     ;

      function  drawDragging        : Boolean ;

      function  drawDragBar         (       _canvas  : TCanvas ;
                                            _scaled  : Boolean
                                    ) : Boolean ;

      function  drawDragBitmapTmp   (       _canvas  : TCanvas
                                    ) : Boolean ;

    protected // new event methods

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

    protected // overridden methods

      /// <summary>
      ///   Overrides the default KeyDown event behavior.
      /// </summary>
      /// <param name="_key">
      ///   the pressed key
      /// </param>
      /// <param name="_char">
      ///   the character represented by the pressed key
      /// </param>
      /// <param name="_shift">
      ///   the pressed shift keys
      /// </param>
      procedure KeyDown             ( var   _key     : Word ;
                                      var   _char    : WideChar ;
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
                                            _x       : Single ;
                                            _y       : Single
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
                                            _x       : Single ;
                                            _y       : Single
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
                                            _x       : Single ;
                                            _y       : Single
                                    ) ; override ;

      /// <summary>
      ///   Overrides the default MouseWheel event behaviour.
      /// </summary>
      /// <param name="_shift">
      ///   the pressed shift keys
      /// </param>
      /// <param name="_delta">
      ///   wheel delta: is positive if the mouse was rotated upward
      ///                is negative if the mouse was rotated downward
      /// </param>
      /// <param name="_handled">
      ///   indicates whether the scroll bar was already moved
      /// </param>
      procedure MouseWheel          (       _shift   : TShiftState ;
                                            _delta   : Integer ;
                                      var   _handled : Boolean
                                    ) ; override ;

      /// <inheritdoc/>
      procedure DoMouseLeave        ; override ;

      /// <inheritdoc/>
      procedure Painting            ; override ;

      /// <inheritdoc/>
      procedure Paint               ; override ;

      /// <inheritdoc/>
      procedure Resize              ; override ;

      {$IFDEF LEVEL_RX10_FMX}
        {#gendoc:hide}
        procedure DoStyleChanged ; override ;
      {$ENDIF}

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
      ///   rectangle
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
      function  DrawBmp             ( const _bitmap  : FMX.Graphics.TBitmap ;
                                      const _scale   : Double  ;
                                      const _ppi     : Integer
                                    ) : FMX.Graphics.TBitmap ;

      /// <inheritdoc from="IGIS_PrintableControl"/>
      procedure PrintBmp            ( const _bitmap  : TGIS_Bitmap
                                    ) ;

      /// <inheritdoc from="IGIS_PrintableControl"/>
      function  CreateCopy          : IGIS_PrintableControl ;

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

    published

      /// <summary>
      ///   BiDi value.
      /// </summary>
      property BiDiMode     : TBiDiMode
                              read  FBiDiMode
                              write fset_BiDiMode
                              default bdLeftToRight ;

      /// <summary>
      ///   Defines which BiDi will be used: the one from property or the one from
      ///   translation
      /// </summary>
      property BiDiModeFromTranslation
                            : Boolean
                              read  FBiDiModeFromTranslation
                              write fset_BiDiModeFromTranslation
                              default True ;

      /// <summary>
      ///   Background color. Default TAlphaColorRec.White
      /// </summary>
      property BackgroundColor : TAlphaColor
                              read  fget_BackgroundColor
                              write fset_BackgroundColor
                              default TAlphaColorRec.White ;

      /// <summary>
      ///   Display font.
      /// </summary>
      property Font         : TFont
                              read  fget_Font ;

      /// <summary>
      ///   Display font.
      /// </summary>
      property FontColor    : TAlphaColor
                              read  fget_FontColor
                              write fset_FontColor
                              default TAlphaColorRec.Black ;

    published // events

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

      {$IFDEF LEVEL_RX10_FMX}
        /// <summary>
        ///   Styled settings.
        /// </summary>
        property StyledSettings        : TGIS_ControlLegendStyledSettings
                                         read  FStyledSettings
                                         write FStyledSettings ;
      {$ENDIF}

    published // properties derived from base class

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Align;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Anchors;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property ClipChildren default False;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property ClipParent default False;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Cursor default crDefault;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property DragMode default TDragMode.dmManual;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property EnableDragHighlight default True;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Enabled default True;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Locked default False;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Height;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property HitTest ;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Padding;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Opacity;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Margins;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property PopupMenu;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Position;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property RotationAngle;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property RotationCenter;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Scale;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Size;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Visible;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property TabOrder;

      /// <summary>
      ///   Standard FMX property. See platform documentation.
      /// </summary>
      property Width;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnDragEnter;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnDragLeave;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnDragOver;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnDragDrop;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnDragEnd;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnClick;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnDblClick;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnMouseDown;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnMouseMove;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnMouseUp;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnMouseWheel;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnMouseEnter;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnMouseLeave;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnPainting;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnPaint;

      /// <event/>
      /// <summary>
      ///   Standard FMX event. See platform documentation.
      /// </summary>
      property OnResize;
  end ;

  {#gendoc:hide}
  procedure Register ;

//##############################################################################
implementation

{$IFDEF MSWINDOWS}
  {$R GisControlLegend_16x16.RES}
{$ENDIF}

uses
  {$IFDEF MSWINDOWS}
    Winapi.Windows,
  {$ENDIF}
  System.Math,

  FMX.Surfaces,
  FMX.Platform,
  FMX.TreeView,
  FMX.Forms,
  {$IFDEF LEVEL_RX101_FMX}
    FMX.Clipboard,
  {$ENDIF}

  GisTypes,
  GisInternals,
  GisClasses,
  GisResource,
  GisRendererAbstract,
  GisLegendUtils,
  FMX.GisControlLegendForm,
  FMX.GisPrintLegend;

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

  procedure TGIS_ControlLegend.fset_BiDiMode(
    const _value : TBiDiMode
  ) ;
  begin
    if _value <> FBiDiMode then begin
      if _value = TBiDiMode.bdLeftToRight then begin
        FBiDiMode := _value ;
        oVScroll.Align := TAlignLayout.Right ;
      end else begin
        FBiDiMode := TBiDiMode.bdRightToLeft ;
        oVScroll.Align := TAlignLayout.Left ;
      end ;

      Repaint ;

      modeBiDi := FBiDiMode ;
    end ;
  end ;

  procedure TGIS_ControlLegend.fset_BiDiModeFromTranslation(
    const _value : Boolean
  ) ;
  begin
    if _value <> FBiDiModeFromTranslation then begin
      FBiDiModeFromTranslation := _value ;

      if FBiDiModeFromTranslation then begin
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

  function TGIS_ControlLegend.fget_BackgroundColor
    : TAlphaColor ;
  begin
    Result := oRenderer.GetBackgroundColor ;
  end ;

  procedure TGIS_ControlLegend.fset_BackgroundColor(
    const _value : TAlphaColor
  ) ;
  begin
    if _value <> oRenderer.GetBackgroundColor then begin
      oRenderer.SetBackgroundColor( _value ) ;
      Repaint ;
    end ;
  end ;

  function TGIS_ControlLegend.fget_Font
    : TFont ;
  begin
    Result := oRenderer.GetFont ;
  end ;

  function TGIS_ControlLegend.fget_FontColor
    : TAlphaColor ;
  begin
    Result := oRenderer.GetFontColor ;
  end ;

  procedure TGIS_ControlLegend.fset_FontColor(
    const _value : TAlphaColor
  ) ;
  begin
    if _value <> oRenderer.GetFontColor then begin
      oRenderer.SetFontColor( _value ) ;
      Repaint ;
    end;
  end ;

  function TGIS_ControlLegend.fget_SelectedNode
    : TGIS_TreeNode ;
  begin
    Result := oLegend.SelectedNode ;
  end ;

//==============================================================================
// events handling
//==============================================================================

  procedure TGIS_ControlLegend.doFontChanged(
    _sender : TObject
  ) ;
  begin
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
    {$IFDEF MSWINDOWS}
      if GetKeyState( VK_CONTROL ) < 0 then
        exit ;
    {$ENDIF}

    if assigned( GIS_Viewer ) then
      GIS_Viewer.InvalidateWholeMap ;

    oTimer.Enabled := False ;
  end ;

  procedure TGIS_ControlLegend.doVScrollChange(
    _sender : TObject
  ) ;
  begin
    oLegend.TopOffset := - RoundS( oVScroll.Value ) ;
  end ;

  procedure TGIS_ControlLegend.doHScrollChange(
    _sender : TObject
  ) ;
  begin

  end ;

//==============================================================================
// private methods
//==============================================================================

  function TGIS_ControlLegend.canvasScale
    : Single ;
  begin
    if ControlIsDesignMode or
       not assigned( GIS_Viewer ) or
       not ( GIS_Viewer is TGIS_ViewerWnd ) then
      Result := 1
    else
      Result := TGIS_ViewerWnd( GIS_Viewer ).ControlCanvasScale ;
  end ;

  procedure TGIS_ControlLegend.updatePPI ;
  begin
    if not assigned( oRenderer ) then exit ;

    if assigned( oLegend.GIS_Viewer ) then begin
      oLegend.PPI := oLegend.GIS_Viewer.PPI ;
      oRenderer.SetPPI( oLegend.PPI ) ;
      oRenderer.SetCanvasScale( canvasScale ) ;
    end ;

    oLegend.UpdatePPI( oRenderer.GetActualFontSize ) ;
  end ;

  procedure TGIS_ControlLegend.drawNode(
    const _node : TGIS_TreeNode ;
    const _bmp  : FMX.Graphics.TBitmap
  ) ;
  var
    b   : Boolean ;
    cnv : TCanvas ;
  begin
    if assigned( _bmp ) then begin
      cnv := _bmp.Canvas ;
      b := True ;
    end
    else begin
      cnv := bmpCache.Canvas ;
      b := False ;
    end ;
    try
      oRenderer.CreateContext( cnv, ControlRightToLeft ) ;
      oLegend.DrawNode( _node, b ) ;
    finally
      oRenderer.FreeContext ;
    end ;
  end ;

  procedure TGIS_ControlLegend.doSubscribedUpdate ;
  var
    hmax   : Integer ;
    redraw : Boolean ;
  begin
    if oLegend.IsUpdateLocked then exit ;

    //hmax := RoundS( oHScroll.Max ) ;
    hmax := 0 ;
    if ( BiDiMode <> modeBiDi ) or ( hmax <> prvHScroll ) then begin
      if ( ControlRightToLeft ) then begin
        oLegend.MaxHScroll := RoundS( Width ) ;
        oHScroll.Value := oLegend.MaxHScroll ;
        oLegend.ValHScroll := RoundS( oHScroll.Value ) ;
        oLegend.MaxHScroll := RoundS( Width ) + oLegend.ValHScroll ;
        oLegend.DifHScroll := oLegend.MaxHScroll - oLegend.ValHScroll ;
      end
      else begin
        oHScroll.Value := 0 ;
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

    if not ( TGIS_ControlLegendOption.AllowParams in oLegend.Options ) then
      exit ;

    la := dt.Layer ;

    if bFormOpened then exit ;

    dlg := TGIS_ControlLegendForm.Create( Self ) ;
    bFormOpened := True ;
    doOpenDialog( Self, la ) ;

    if assigned( GIS_Viewer ) then
      hlp := GIS_Viewer.AssignedHelpEvent
    else
      hlp := nil ;

    dlg.Execute( la, Self, procedure( _modal_result : TModalResult )
                           begin
                             bFormOpened := False ;
                             if _modal_result = mrOK then begin
                               doLayerParamsChange( Self, la ) ;
                               if assigned( GIS_Viewer ) then
                                 GIS_Viewer.InvalidateWholeMap ;
                             end ;
                           end
               ) ;
  end ;

  procedure TGIS_ControlLegend.disableDragging ;
  begin
    oDragNode := nil ;
    iDragTop  := -1 ;
    bDragBar  := False ;
    bDragBitmapTmp := False ;
    bDragFromCache := False ;
  end ;

  function TGIS_ControlLegend.drawDragging
    : Boolean ;
  begin
    Result := bDragBar or bDragBitmapTmp or bDragFromCache ;
  end ;

  function TGIS_ControlLegend.drawDragBar(
    _canvas : TCanvas ;
    _scaled : Boolean
  ) : Boolean ;
  var
    l, t : Single ;
    b    : Single ;
    rct  : TRect  ;
    node : TGIS_TreeNode ;
  begin
    Result := False ;
    if not bDragBar then exit ;

    Result := True ;

    l := 0 ;
    t := 0 ;
    if not assigned( oCanvasBitmap ) then
      if ControlRightToLeft and oVScroll.Visible then
        l := l + oVScroll.Width + 1 ;

    rct := oLegend.GetDragBarRect( oDragNode, iDragTop ) ;
    if _scaled then
      rct := Rect( RoundS( rct.Left / canvasScale ),
                   RoundS( rct.Top / canvasScale ),
                   RoundS( rct.Right / canvasScale ),
                   RoundS( rct.Bottom / canvasScale ) ) ;
    l := l + rct.Left ;
    t := t + rct.Top ;
    if _scaled then
      b := t
    else
      b := t + canvasScale - 1 ;
    _canvas.Stroke.Kind := TBrushKind.Solid ;
    _canvas.Stroke.Thickness := 1 ;
    _canvas.Stroke.Color := TAlphaColorRec.Red ;
    _canvas.DrawRect( RectF( l, t, l+rct.Width-1, b ), 0, 0,
                      [ TCorner.TopLeft, TCorner.TopRight,
                        TCorner.BottomLeft, TCorner.BottomRight
                      ]
                      , 1
                    ) ;

    bDragBar := False ;
  end ;

  function TGIS_ControlLegend.drawDragBitmapTmp(
    _canvas : TCanvas
  ) : Boolean ;
  begin
    Result := False ;
    try
    if not bDragBitmapTmp then exit ;
    if not assigned( oCanvasBitmap ) then exit ;

    Result := True ;
    if ControlRightToLeft and oVScroll.Visible then
      _canvas.DrawBitmap(
        oCanvasBitmap,
        RectF( 0, 0, oCanvasBitmap.Width, oCanvasBitmap.Height ),
        RectF( oVScroll.Width, 0,
               oVScroll.Width + ( oCanvasBitmap.Width / canvasScale ),
               oCanvasBitmap.Height / canvasScale ),
        1
      )
    else
      _canvas.DrawBitmap(
        oCanvasBitmap,
        RectF( 0, 0, oCanvasBitmap.Width, oCanvasBitmap.Height ),
        RectF( 0, 0,
               oCanvasBitmap.Width / canvasScale,
               oCanvasBitmap.Height / canvasScale  ),
        1
      ) ;
    finally
      FreeObject( oCanvasBitmap ) ;
      bDragBitmapTmp := False ;
    end;
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
      FOnTapSimple( Self, _button, _shift, _x, _y ) ;
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
      FOnTapDouble( Self, _button, _shift, _x, _y ) ;
      exit ;
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
      FOnTapLong( Self, _button, _shift, _x, _y ) ;
      exit ;
    end ;
    oLegend.TapLong( ssLeft in _shift ) ;
  end ;

  procedure TGIS_ControlLegend.KeyDown(
    var _key   : Word ;
    var _char  : WideChar ;
        _shift : TShiftState
  ) ;
  begin
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
      vkReturn   :
        begin
          openLegendForm ;
        end ;
    end ;

    case _char of
      ' ' :
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
      '+' :
        begin
          if ( not assigned( GIS_Viewer ) ) or ( GIS_Viewer.Items.Count <= 0 ) then
            exit ;
          if not assigned( SelectedNode ) then
            exit ;
          if Mode <> TGIS_ControlLegendMode.Layers then
            exit ;
          oLegend.Expand( GIS_Layer ) ;
        end ;
      '-' :
        begin
          if ( not assigned( GIS_Viewer ) ) or ( GIS_Viewer.Items.Count <= 0 ) then
            exit ;
          if not assigned( SelectedNode ) then
            exit ;
          if Mode <> TGIS_ControlLegendMode.Layers then
            exit ;
          oLegend.Collapse( GIS_Layer ) ;
        end ;
    end ;
  end ;

  procedure TGIS_ControlLegend.MouseDown(
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x      : Single ;
    _y      : Single
  ) ;
  var
    x, y    : Double  ;
    handled : Boolean ;
  begin
    if ssTouch in _shift then
      exit ;

    inherited ;

    try
      if ControlRightToLeft and oVScroll.Visible then
        x := ( _x - oVScroll.Width ) * canvasScale
      else
        x := _x * canvasScale ;
      y := _y * canvasScale ;

      oLegend.MouseDown( x, y, ssTouch in _shift, ssCtrl in _shift, ssShift in _shift, handled ) ;
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
    _x     : Single ;
    _y     : Single
  ) ;
  var
    x, y : Double ;
  begin
    if _shift = [] then
      exit ;

    if ssTouch in _shift then
      exit ;

    inherited ;

    if ControlRightToLeft and oVScroll.Visible then
      x := ( _x - oVScroll.Width ) * canvasScale
    else
      x := _x * canvasScale ;
    y := _y * canvasScale ;

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
    oLegend.MouseMove( x, y, ssLeft in _shift ) ;
  end ;

  procedure TGIS_ControlLegend.MouseUp(
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x      : Single ;
    _y      : Single
  ) ;
  var
    x, y : Double ;
  begin
    if ssTouch in _shift then
      exit ;

    inherited ;

    disableDragging ;

    if ControlRightToLeft and oVScroll.Visible then
      x := ( _x - oVScroll.Width ) * canvasScale
    else
      x := _x * canvasScale ;
    y := _y * canvasScale ;

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
    oLegend.MouseUp( x, y ) ;
  end ;

  procedure TGIS_ControlLegend.MouseWheel(
        _shift   : TShiftState ;
        _delta   : Integer ;
    var _handled : Boolean
  ) ;
  begin
    oLegend.MouseWheel( RoundS( _delta * canvasScale ) ) ;
    _handled := True ;
  end ;

  procedure TGIS_ControlLegend.DoMouseLeave ;
  begin
    inherited ;
    oLegend.MouseLeave ;
  end ;

  procedure TGIS_ControlLegend.Painting ;
  begin
    oLegend.FixDataConsistency ;
    inherited ;
  end ;

  procedure TGIS_ControlLegend.Paint ;
  var
    w, h : Integer ;
    draw : Boolean ;

    function draw_on_bitmap : Boolean ;
    begin
      if assigned( bmpCache ) then
        FreeObject( bmpCache ) ;

      bmpCache := FMX.Graphics.TBitmap.Create( w, h ) ;
      bmpCache.Canvas.BeginScene ;
      try
        oRenderer.DrawBackground( bmpCache.Canvas, w, h ) ;
      finally
        bmpCache.Canvas.EndScene ;
      end ;

      oLegend.CheckBoxRectSize := oRenderer.GetActualCheckBoxSize ;

      bmpCache.Canvas.BeginScene ;
      try
        Result := oLegend.DrawNodes ;
      finally
        bmpCache.Canvas.EndScene ;
      end;
    end ;

  begin
    inherited ;

    if not assigned( oLegend ) then
      exit ;

    w := RoundS( ControlClientWidth  ) ;
    h := RoundS( ControlClientHeight ) ;

    {$IFDEF LEVEL_RX10_FMX}
      oRenderer.ApplyStyledSettings( StyledSettings ) ;
    {$ENDIF}

    if drawDragging then begin
      bDragFromCache := False ;
      draw := True ;
    end
    else begin

      if assigned( bmpCache ) then begin
        FreeObject( bmpCacheOld ) ;
        bmpCacheOld := bmpCache ;
        bmpCache := nil ;
      end ;

      updatePPI ;
      FInPaint := True ;
      try
        bForceRepaint := false ;
        draw := draw_on_bitmap ;
        if bForceRepaint then
          // once again
          draw := draw_on_bitmap ;
      finally
        FInPaint := False ;
      end ;
    end ;

    if draw then begin
      if oVScroll.Visible then begin
        if ControlRightToLeft then
          Canvas.DrawBitmap( bmpCache,
                             RectF( 0, 0, w, h ),
                             RectF( oVScroll.Width, 0, Width, Height ),
                             AbsoluteOpacity
                           )
        else
          Canvas.DrawBitmap( bmpCache,
                             RectF( 0, 0, w, h ),
                             RectF( 0, 0, Width - oVScroll.Width, Height ),
                             AbsoluteOpacity
                           ) ;
      end
      else
        Canvas.DrawBitmap( bmpCache,
                           RectF( 0, 0, w, h ),
                           RectF( 0, 0, Width, Height ),
                           AbsoluteOpacity
                         ) ;

      drawDragBar( Canvas, True ) ;
      drawDragBitmapTmp( Canvas ) ;
    end
    else begin
      bmpCache.Canvas.BeginScene ;
      try
        oRenderer.DrawBackground( bmpCache.Canvas, w, h ) ;
        if assigned( bmpCacheOld ) then begin
          if ControlRightToLeft then
            bmpCache.Canvas.DrawBitmap( bmpCacheOld,
                                        RectF( 0, 0,
                                               bmpCacheOld.Width, bmpCacheOld.Height ),
                                        RectF( w - bmpCacheOld.Width, 0,
                                               w, bmpCacheOld.Height ),
                                        1
                                      )
          else
            bmpCache.Canvas.DrawBitmap( bmpCacheOld,
                                        RectF( 0, 0,
                                               bmpCacheOld.Width, bmpCacheOld.Height ),
                                        RectF( 0, 0,
                                               bmpCacheOld.Width, bmpCacheOld.Height ),
                                        1
                                      ) ;
        end ;
      finally
        bmpCache.Canvas.EndScene ;
      end ;
      if oVScroll.Visible then begin
        if ControlRightToLeft then
          Canvas.DrawBitmap( bmpCache,
                             RectF( 0, 0, w, h ),
                             RectF( oVScroll.Width, 0, Width, Height ),
                             AbsoluteOpacity
                           )
        else
          Canvas.DrawBitmap( bmpCache,
                             RectF( 0, 0, w, h ),
                             RectF( 0, 0, Width - oVScroll.Width, Height ),
                             AbsoluteOpacity
                           ) ;
      end
      else
        Canvas.DrawBitmap( bmpCache,
                           RectF( 0, 0, w, h ),
                           RectF( 0, 0, Width, Height ),
                           AbsoluteOpacity
                         ) ;
      if assigned( bmpCacheOld ) then
        // bmpCacheOld will be the cache in future
        FreeObject( bmpCache ) ;
    end ;
  end ;

  procedure TGIS_ControlLegend.Resize ;
  begin
    updatePPI ;
    oLegend.Resize ;
    inherited ;
  end ;

{$IFDEF LEVEL_RX10_FMX}
  procedure TGIS_ControlLegend.DoStyleChanged;
  begin
    inherited ;
    oRenderer.ChangeStyle ;
  end ;
{$ENDIF LEVEL_RX10_FMX}

//==============================================================================
// constructors/destructors
//==============================================================================

  constructor TGIS_ControlLegend.Create(
    _owner : TComponent
  ) ;
  var
    itm : TTreeViewItem ;
  begin
    inherited ;

    if csDesigning in ComponentState then
      EnsureFramework ;

    // set default values
    CanFocus := True ;

    oLegend := TGIS_Legend.Create( Self ) ;
    oLegend.ExpandRectSize   := GIS_LEGEND_EXPAND_RECT_SIZE ;
    oLegend.CheckBoxRectSize := GIS_LEGEND_CHECKBOX_RECT_SIZE ;
    oLegend.LevelIndent      := GIS_LEGEND_LEVEL_INDENT ;
    {$IFDEF ANDROID}
    oLegend.DirectTouchLongTap := True ;
    {$ENDIF}
    {$IFDEF MACOS}
    oLegend.DirectTouchLongTap := True ;
    {$ENDIF}
    oLegend.LayerParamsChangeEvent := doLayerParamsChange ;
    oLegend.LayerActiveChangeEvent := doLayerActiveChange ;
    oLegend.LayerSelectEvent := doLayerSelect ;
    oLegend.GroupActiveChangeEvent := doGroupActiveChange ;
    oLegend.GroupSelectEvent := doGroupSelect ;
    oLegend.OrderChangeEvent := doOrderChange ;
    // for proper options' initialization
    Options := oLegend.Options ;

    oRenderer := TGIS_StyleRendererFMX.Create( Self ) ;
    oRenderer.GetFont.OnChanged := doFontChanged ;
    itm := TTreeViewItem.Create( nil ) ;
    try
      oRenderer.SetFont( itm.Font ) ;
    finally
      FreeObject( itm ) ;
    end ;
    {$IFDEF LEVEL_RX10_FMX}
      oRenderer.ChangeStyle ;
    {$ENDIF LEVEL_RX10_FMX}

    FBiDiMode := TBiDiMode.bdLeftToRight ;
    BiDiModeFromTranslation := True ;
    modeBiDi    := BiDiMode ;
    prvHScroll  := 0 ;

    bFormOpened := False ;

    // after oLegend has been created
    Width  := 120 ;
    Height := 100 ;

    // after oRenderer has been created
    updatePPI ;

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

    bmpCache      := nil ;
    bmpCacheOld   := nil ;
    oCanvasBitmap := nil ;

    oGestHlpr := TGIS_GestureHelper.Create( Self, updateTap ) ;

    disableDragging ;

    oTimer := TTimer.Create( Self ) ;
    oTimer.Stored   := False ;
    oTimer.Interval := GIS_LEGEND_UPDATE_DELAY ;
    oTimer.OnTimer  := doTimer ;
    oTimer.Enabled  := False ;

    oVScroll := TScrollBar.Create( Self ) ;
    oVScroll.Orientation := TOrientation.Vertical ;
    oVScroll.Align := TAlignLayout.Right ;
    oVScroll.Visible := False ;
    oVScroll.OnChange := doVScrollChange ;

    oHScroll := TScrollBar.Create( Self ) ;
    oHScroll.Orientation := TOrientation.Horizontal ;
    oHScroll.Align := TAlignLayout.Bottom ;
    oHScroll.Visible := False ;
    oHScroll.OnChange := doHScrollChange ;
  end ;

  destructor TGIS_ControlLegend.Destroy ;
  begin
    oVScroll.Parent := nil ;
    FreeObject( oVScroll ) ;
    oHScroll.Parent := nil ;
    FreeObject( oHScroll ) ;
    FreeObject( bmpCache ) ;
    FreeObject( bmpCacheOld ) ;
    FreeObject( oGestHlpr ) ;

    // when the license is not valid,
    // the object is destroyed before oLegend is created
    if assigned( oLegend ) then
      GIS_Viewer := nil ;

    FreeObject( oRenderer ) ;
    FreeObject( oLegend ) ;

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
    const _bitmap : FMX.Graphics.TBitmap ;
    const _scale  : Double  ;
    const _ppi    : Integer
  ) : FMX.Graphics.TBitmap ;
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
        Result := FMX.Graphics.TBitmap.Create ;
      Result.Assign( FMX.Graphics.TBitmap( bmp.NativeBitmap ) ) ;
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
    Result := TGIS_ControlLegend.Create( Parent ) ;
    TGIS_ControlLegend(Result).Parent := Parent ;
    TGIS_ControlLegend(Result).InternalName := Name ;
    TGIS_ControlLegend(Result).Visible := False ;
    TGIS_ControlLegend(Result).GIS_Viewer := GIS_Viewer ;
    TGIS_ControlLegend(Result).CompactView := CompactView ;
    TGIS_ControlLegend(Result).ReverseOrder := ReverseOrder ;
    TGIS_ControlLegend(Result).Font.Family := oRenderer.GetFont.Family ;
    TGIS_ControlLegend(Result).Font.Size := oRenderer.GetFont.Size ;
    TGIS_ControlLegend(Result).Font.Style := oRenderer.GetFont.Style ;
    TGIS_ControlLegend(Result).FontColor := oRenderer.GetFontColor ;
  end ;

  procedure TGIS_ControlLegend.FreeCopy(
    const _control : IGIS_PrintableControl
  ) ;
  var
    legend : TGIS_ControlLegend ;
  begin
    legend := TGIS_ControlLegend( _control ) ;
    FreeObject( legend ) ;
  end ;

  procedure TGIS_ControlLegend.PrintClipboard ;
  {$IFNDEF LEVEL_RX101_FMX}
    begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNIMPLEMENTED ) ) ;
    end ;
  {$ELSE}
    var
      svc : IFMXExtendedClipboardService ;
      srf : TBitmapSurface ;
      bmp : TGIS_Bitmap ;
      pl  : TGIS_PrintLegend ;
    begin
      svc := IFMXExtendedClipboardService(
               TPlatformServices.Current.GetPlatformService(
                 IFMXExtendedClipboardService
               )
             ) ;

      if not Assigned( svc ) then begin
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNIMPLEMENTED ) );
        exit ;
      end ;

      bmp := TGIS_Bitmap.Create( RoundS( Width ), RoundS( Height ) ) ;
      pl := TGIS_PrintLegend.Create( Self, bmp ) ;
      srf := TBitmapSurface.Create ;
      try
        pl.Draw ;
        srf.Assign( FMX.Graphics.TBitmap( bmp.NativeBitmap ) ) ;
        svc.SetImage( srf ) ;
      finally
        FreeObject( srf ) ;
        FreeObject( pl ) ;
        FreeObject( bmp ) ;
      end ;
    end ;
  {$ENDIF}

//==============================================================================
// IGIS_LegendParent methods
//==============================================================================

  function TGIS_ControlLegend.ControlWidth
    : Integer ;
  begin
    Result := RoundS( Width * canvasScale ) ;
  end ;

  function TGIS_ControlLegend.ControlHeight
    : Integer ;
  begin
    Result := RoundS( Height * canvasScale ) ;
  end ;

  function TGIS_ControlLegend.ControlClientWidth
    : Integer ;
  begin
    if assigned( oVScroll ) and oVScroll.Visible then
      Result := RoundS( ( Width - oVScroll.Width ) * canvasScale  )
    else
      Result := RoundS( Width * canvasScale ) ;
  end ;

  function TGIS_ControlLegend.ControlClientHeight
    : Integer ;
  begin
    if assigned( oHScroll ) and oHScroll.Visible then
      Result := RoundS( ( Height - oHScroll.Height ) * canvasScale )
    else
      Result := RoundS( Height * canvasScale ) ;
  end ;

  procedure TGIS_ControlLegend.ControlTouch(
    const _context : TObject
  ) ;
  var
    context : TGIS_TouchContext ;
    loc     : TPointF ;
    x, y    : Double ;
    handled : Boolean ;
  begin
    // ignore when the control is not accessible
    if not Visible then exit ;
    if not Enabled then exit ;

    if not assigned( _context ) or not ( _context is TGIS_TouchContext ) then
      exit ;
    context := TGIS_TouchContext( _context ) ;
    if not assigned( context.Touches ) or ( Length( context.Touches ) < 1 ) then
      exit ;
    loc := TForm( self.Root ).ClientToScreen( context.Touches[0].Location ) ;
    loc := ScreenToLocal( loc ) ;

    handled := False ;

    if ControlRightToLeft and oVScroll.Visible then
      x := ( loc.X - oVScroll.Width ) * canvasScale
    else
      x := loc.X * canvasScale ;
    y := loc.Y * canvasScale ;

    // ignore when the touch is beyond the legend
    if ( x < 0 ) or ( x > ControlClientWidth ) or
       ( y < 0 ) or ( y > ControlClientHeight ) then
      exit ;

    if context.Action = TTouchAction.Down then begin
      SetFocus ;
      oLegend.TouchDown( x, y, handled ) ;
      if not handled then
        oGestHlpr.GestureMouseDown(
          False, False, False,
          True,
          False,
          False,
          RoundS( x ),
          RoundS( y )
        ) ;
    end
    else
    if context.Action = TTouchAction.Up then begin
      disableDragging ;
      oGestHlpr.GestureMouseUp(
        False, False, False,
        True,
        False,
        False,
        RoundS( x ),
        RoundS( y )
      ) ;
      oLegend.TouchUp( x, y ) ;
    end
    else
    if context.Action = TTouchAction.Move then begin
      if oLegend.DirectTouchLongTap then
        oGestHlpr.GestureMouseMove(
          False, False, False,
          True,
          False,
          False,
          RoundS( x ),
          RoundS( y )
        )
      else
        oGestHlpr.GestureMouseMoveEx(
          False, False, False,
          True,
          False,
          False,
          RoundS( x ),
          RoundS( y )
        ) ;
      oLegend.TouchMove( x, y, not oGestHlpr.GestureNoMovement ) ;
    end ;
  end ;

  function TGIS_ControlLegend.ControlGetVScrollPosition
    : Integer ;
  begin
    if assigned( oVScroll ) and oVScroll.Visible then
      Result := RoundS( oVScroll.Value )
    else
      Result := 0 ;
  end ;

  procedure TGIS_ControlLegend.ControlSetVScrollPosition(
    const _position : Integer
  ) ;
  begin
    if assigned( oVScroll ) then
      oVScroll.Value := _position ;
  end ;

  function TGIS_ControlLegend.ControlGetHScrollPosition
    : Integer ;
  begin
    if assigned( oHScroll ) and oHScroll.Visible then
      Result := RoundS( oHScroll.Value )
    else
      Result := 0 ;
  end ;

  procedure TGIS_ControlLegend.ControlSetHScrollPosition(
    const _position : Integer
  ) ;
  begin
    if assigned( oHScroll ) then
      oHScroll.Value := _position ;
  end ;

  procedure TGIS_ControlLegend.ControlSetVScroll(
    const _minPosition : Integer ;
    const _maxPosition : Integer ;
    const _position    : Integer
  ) ;
  var
    visible : Boolean ;
  begin
    if not assigned( oVScroll ) then exit ;
    visible := oVScroll.Visible ;
    if ( _minPosition = 0 ) and ( _maxPosition = 0 )  then begin
      oVScroll.Parent  := nil ;
      oVScroll.Visible := False ;
    end
    else begin
      oVScroll.Parent  := Self ;
      oVScroll.Visible := True ;
      oVScroll.Min := _minPosition ;
      oVScroll.Max := _maxPosition ;
      oVScroll.Value := _position ;
    end ;
    if visible <> oVScroll.Visible then
      ControlRepaint ;
  end ;

  procedure TGIS_ControlLegend.ControlSetHScroll(
    const _minPosition : Integer ;
    const _maxPosition : Integer ;
    const _position    : Integer
  ) ;
  var
    visible : Boolean ;
  begin
    if not assigned( oHScroll ) then exit ;
    visible := assigned( oHScroll.Parent ) and oHScroll.Visible ;
    if ( _minPosition = 0 ) and ( _maxPosition = 0 )  then begin
      oHScroll.Parent  := nil ;
      oHScroll.Visible := False ;
    end
    else begin
      oHScroll.Parent  := Self ;
      oHScroll.Visible := True ;
      oHScroll.Min := _minPosition ;
      oHScroll.Max := _maxPosition ;
      oHScroll.Value := _position ;
    end ;
    if visible <> oHScroll.Visible then
      ControlRepaint ;
  end ;

  procedure TGIS_ControlLegend.ControlRepaint ;
  begin
    if FInPaint then
      bForceRepaint := true
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
  begin
    bDragBar  := True ;
    oDragNode := TGIS_TreeNode( _node ) ;
    iDragTop  := _top ;
    if assigned( oCanvasBitmap ) then begin
      oCanvasBitmap.Canvas.BeginScene() ;
      try
        drawDragBar( oCanvasBitmap.Canvas, False ) ;
      finally
        oCanvasBitmap.Canvas.EndScene ;
      end ;
    end
    else
      Repaint ;
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
    if rct.Height > Height * canvasScale then
      rct.Height := RoundS( Height * canvasScale ) ;
    bmpDrag := FMX.Graphics.TBitmap.Create ;
    bmpDrag.Width  := rct.Width ;
    bmpDrag.Height := rct.Height ;
    bmpDrag.Canvas.BeginScene ;
    try
      drawNode( node, bmpDrag ) ;
    finally
      bmpDrag.Canvas.EndScene ;
    end;
  end ;

  procedure TGIS_ControlLegend.ControlDragDrawNode(
    const _left         : Integer ;
    const _top          : Integer ;
    const _transparency : Single
  ) ;
  begin
    if not assigned( oCanvasBitmap ) then exit ;
    oCanvasBitmap.Canvas.BeginScene ;
    try
      oCanvasBitmap.Canvas.DrawBitmap( bmpDrag,
                                       RectF( 0, 0,
                                              bmpDrag.Width,
                                              bmpDrag.Height ),
                                       RectF( _left, _top,
                                              _left + bmpDrag.Width,
                                              _top + bmpDrag.Height ),
                                       _transparency
                                     ) ;
    finally
      oCanvasBitmap.Canvas.EndScene ;
    end;
  end ;

  procedure TGIS_ControlLegend.ControlDragFreeNode ;
  begin
    FreeObject( bmpDrag ) ;
    FreeObject( oCanvasBitmap ) ;
  end ;

  procedure TGIS_ControlLegend.ControlDrawFromCache ;
  begin
    if assigned( oCanvasBitmap ) then begin
      oCanvasBitmap.Canvas.BeginScene ;
      try
        oCanvasBitmap.Canvas.DrawBitmap(
          bmpCache,
          RectF( 0, 0, bmpCache.Width, bmpCache.Height ),
          RectF( 0, 0, bmpCache.Width, bmpCache.Height ),
          1
        ) ;
      finally
        oCanvasBitmap.Canvas.EndScene ;
      end;
    end
    else begin
      bDragFromCache := True ;
      Repaint ;
    end ;
  end ;

  procedure TGIS_ControlLegend.ControlDragCreateTemporaryContext ;
  begin
    if not assigned( oCanvasBitmap ) then begin
      oCanvasBitmap := FMX.Graphics.TBitmap.Create ;
      if oVScroll.Visible then
        oCanvasBitmap.Width := RoundS( ( Width - oVScroll.Width ) * canvasScale )
      else
        oCanvasBitmap.Width := RoundS( Width * canvasScale ) ;
      oCanvasBitmap.Height := RoundS( Height * canvasScale ) ;
      oCanvasBitmap.Canvas.BeginScene();
      oCanvasBitmap.Canvas.Clear( TAlphaColorRec.Yellow);
      oCanvasBitmap.Canvas.EndScene;
    end ;
  end ;

  procedure TGIS_ControlLegend.ControlDragRenderTemporaryContext ;
  begin
    bDragBitmapTmp := True ;
    Repaint ;
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

//==================================== END =====================================
end.

