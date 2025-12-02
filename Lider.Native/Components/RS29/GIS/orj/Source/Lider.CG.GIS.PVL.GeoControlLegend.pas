{$IFDEF DCC}
  unit PVL.GisControlLegend;
  {$HPPEMIT '#pragma link "PVL.GisControlLegend"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK.PVL ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk.pvl ;
{$ENDIF}
interface

uses
  {$IFDEF CLR}
    TatukGIS.NDK,
    TatukGIS.NDK.WinForms,
    TatukGIS.RTL ;
  {$ENDIF}

  {$IFDEF DCC}
    {$M+}
    System.Types,
    System.Classes,
    System.SysUtils,

    PVL.GisPvl,

    GisInterfaces,
    GisTypes,
    GisTypesUI,
    GisHierarchy,
    GisLayer,
    GisLegend;

  {$ENDIF}

  {$IFDEF JAVA}
    java.util,
    tatukgis.jdk.*,
    java.awt.*,
    javax.swing.*,
    java.beans.*,
    tatukgis.rtl ;
  {$ENDIF}

type

  /// <summary>
  ///   PVL control legend intraface
  /// </summary>
  IGIS_PvlControlLegend = interface
    {$IFDEF DCC}
      ['{F1FCC6D9-BB6B-4F44-99B4-660AAD11BE29}']
    {$ENDIF}

    // property access routines
    function  fget_GIS_Viewer     : IGIS_Viewer ;
    procedure fset_GIS_Viewer     ( const _value : IGIS_Viewer
                                  ) ;
    function  fget_GIS_Layer      : TGIS_Layer ;
    procedure fset_GIS_Layer      ( const _value : TGIS_Layer
                                  ) ;
    function  fget_GIS_Layers     : TGIS_LayerAbstractList ;
    procedure fset_GIS_Layers     ( const _value : TGIS_LayerAbstractList
                                  ) ;
    function  fget_GIS_Group      : IGIS_HierarchyGroup ;
    procedure fset_GIS_Group      ( const _value : IGIS_HierarchyGroup
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
    function  fget_RightToLeft    : Boolean ;
    procedure fset_RightToLeft    ( const _value : Boolean
                                  ) ;
    function  fget_RightToLeftFromTranslation
                                  : Boolean ;
    procedure fset_RightToLeftFromTranslation
                                  ( const _value : Boolean
                                  ) ;
    function  fget_BackgroundColor: TGIS_Color ;
    procedure fset_BackgroundColor( const _value : TGIS_Color
                                  ) ;
    function  fget_Font           : TGIS_Font ;
    function  fget_FontColor      : TGIS_Color ;
    procedure fset_FontColor      ( const _value : TGIS_Color
                                  ) ;

    function  fget_SelectedNode   : TGIS_TreeNode ;

    // property access routines
    function  fget_LayerParamsChangeEvent : TGIS_LayerEvent;
    procedure fset_LayerParamsChangeEvent ( const _value : TGIS_LayerEvent
                                          );
    function  fget_LayerActiveChangeEvent : TGIS_LayerEvent;
    procedure fset_LayerActiveChangeEvent ( const _value : TGIS_LayerEvent
                                          );
    function  fget_LayerSelectEvent       : TGIS_LayerEvent;
    procedure fset_LayerSelectEvent       ( const _value : TGIS_LayerEvent
                                          );
    function  fget_GroupActiveChangeEvent : TGIS_HierarchyGroupEvent;
    procedure fset_GroupActiveChangeEvent ( const _value : TGIS_HierarchyGroupEvent
                                          );
    function  fget_GroupSelectEvent       : TGIS_HierarchyGroupEvent;
    procedure fset_GroupSelectEvent       ( const _value : TGIS_HierarchyGroupEvent
                                          );
    function  fget_OrderChangeEvent       : TGIS_PvlEvent;
    procedure fset_OrderChangeEvent       ( const _value : TGIS_PvlEvent
                                          );
    function  fget_OpenDialogEvent        : TGIS_LayerEvent ;
    procedure fset_OpenDialogEvent        ( const _value : TGIS_LayerEvent
                                          );
    function  fget_TapSimpleEvent         :  TGIS_PvlMouseEvent;
    procedure fset_TapSimpleEvent         ( const _value : TGIS_PvlMouseEvent
                                          );
    function  fget_TapLongEvent           : TGIS_PvlMouseEvent;
    procedure fset_TapLongEvent           ( const _value : TGIS_PvlMouseEvent
                                          );
    function  fget_TapDoubleEvent         :  TGIS_PvlMouseEvent;
    procedure fset_TapDoubleEvent         ( const _value : TGIS_PvlMouseEvent
                                          );

    // public methods

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
    function  DrawBmp             ( const _bitmap  : TGIS_Bitmap ;
                                    const _scale   : Double  ;
                                    const _ppi     : Integer
                                  ) : TGIS_Bitmap ;

    /// <summary>
    ///   Prints the current state of the legend to the clipboard as a bitmap.
    /// </summary>
    procedure PrintClipboard      ;
  end;

  /// <summary>
  ///   PVL GIS Legend control.
  /// </summary>
  TGIS_PvlControlLegend = class( TGIS_PvlControl, IGIS_PvlControlLegend, IGIS_PrintableControl )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
    protected
      /// <inheritdoc/>
      procedure initControl       ;                              override;

    private
      function fget_PlatformControl
                                  : IGIS_PvlControlLegend ;      reintroduce;
      property PlatformControl    : IGIS_PvlControlLegend
                                    read  fget_PlatformControl;  {$IFDEF OXYGENE}reintroduce;{$ENDIF}

    public
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_PrintableControl"/>
      function CreateCopy         : IGIS_PrintableControl ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_PrintableControl"/>
      procedure FreeCopy          ( const _control : IGIS_PrintableControl
                                  ) ;

      /// <inheritdoc from="IGIS_PrintableControl"/>
      procedure PrintBmp          ( const _bitmap  : TGIS_Bitmap
                                  ) ;


    private // IGIS_PrintableControl property access routines
      function  fget_InternalName : String ;
      procedure fset_InternalName ( const _value : String
                                  ) ;
    published
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_PrintableControl"/>
      property InternalName       : String
                                          read  fget_InternalName
                                          write fset_InternalName ;

    protected // IGIS_PvlControlLegend property access routines

      function  fget_GIS_Viewer     : IGIS_Viewer ;
      procedure fset_GIS_Viewer     ( const _value : IGIS_Viewer
                                    ) ;
      function  fget_GIS_Layer      : TGIS_Layer ;
      procedure fset_GIS_Layer      ( const _value : TGIS_Layer
                                    ) ;
      function  fget_GIS_Layers     : TGIS_LayerAbstractList ;
      procedure fset_GIS_Layers     ( const _value : TGIS_LayerAbstractList
                                    ) ;
      function  fget_GIS_Group      : IGIS_HierarchyGroup ;
      procedure fset_GIS_Group      ( const _value : IGIS_HierarchyGroup
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
      function  fget_RightToLeft    : Boolean ;
      procedure fset_RightToLeft    ( const _value : Boolean
                                    ) ;
      function  fget_RightToLeftFromTranslation
                                    : Boolean ;
      procedure fset_RightToLeftFromTranslation
                                    ( const _value : Boolean
                                    ) ;
      function  fget_BackgroundColor: TGIS_Color ;
      procedure fset_BackgroundColor( const _value : TGIS_Color
                                    ) ;
      function  fget_Font           : TGIS_Font ;
      function  fget_FontColor      : TGIS_Color ;
      procedure fset_FontColor      ( const _value : TGIS_Color
                                    ) ;

      function  fget_SelectedNode   : TGIS_TreeNode ;

    private // IGIS_PvlControlLegend events access routines
      function  fget_LayerParamsChangeEvent : TGIS_LayerEvent;
      procedure fset_LayerParamsChangeEvent ( const _value : TGIS_LayerEvent
                                            );
      function  fget_LayerActiveChangeEvent : TGIS_LayerEvent;
      procedure fset_LayerActiveChangeEvent ( const _value : TGIS_LayerEvent
                                            );
      function  fget_LayerSelectEvent       : TGIS_LayerEvent;
      procedure fset_LayerSelectEvent       ( const _value : TGIS_LayerEvent
                                            );
      function  fget_GroupActiveChangeEvent : TGIS_HierarchyGroupEvent;
      procedure fset_GroupActiveChangeEvent ( const _value : TGIS_HierarchyGroupEvent
                                            );
      function  fget_GroupSelectEvent       : TGIS_HierarchyGroupEvent;
      procedure fset_GroupSelectEvent       ( const _value : TGIS_HierarchyGroupEvent
                                            );
      function  fget_OrderChangeEvent       : TGIS_PvlEvent;
      procedure fset_OrderChangeEvent       ( const _value : TGIS_PvlEvent
                                            );
      function  fget_OpenDialogEvent        : TGIS_LayerEvent ;
      procedure fset_OpenDialogEvent        ( const _value : TGIS_LayerEvent
                                            );
      function  fget_TapSimpleEvent         :  TGIS_PvlMouseEvent;
      procedure fset_TapSimpleEvent         ( const _value : TGIS_PvlMouseEvent
                                            );
      function  fget_TapLongEvent           : TGIS_PvlMouseEvent;
      procedure fset_TapLongEvent           ( const _value : TGIS_PvlMouseEvent
                                            );
      function  fget_TapDoubleEvent         :  TGIS_PvlMouseEvent;
      procedure fset_TapDoubleEvent         ( const _value : TGIS_PvlMouseEvent
                                            );
    public // public methods
      /// <inheritdoc from="IGIS_PvlControlLegend"/>
      function  IsExpanded          ( const _layer   : TGIS_Layer
                                    ) : Boolean ; overload ;

      /// <inheritdoc from="IGIS_PvlControlLegend"/>
      function  IsExpanded          ( const _group   : IGIS_HierarchyGroup
                                    ) : Boolean ; overload ;

      /// <inheritdoc from="IGIS_PvlControlLegend"/>
      procedure Expand              ( const _layer   : TGIS_Layer
                                    ) ; overload ;

      /// <inheritdoc from="IGIS_PvlControlLegend"/>
      procedure Expand              ( const _group   : IGIS_HierarchyGroup
                                    ) ; overload ;

      /// <inheritdoc from="IGIS_PvlControlLegend"/>
      procedure Expand              ( const _group   : IGIS_HierarchyGroup ;
                                      const _deep    : Boolean ;
                                      const _layers  : Boolean
                                    ) ; overload ;

      /// <inheritdoc from="IGIS_PvlControlLegend"/>
      procedure Collapse            ( const _layer   : TGIS_Layer
                                    ) ; overload ;

      /// <inheritdoc from="IGIS_PvlControlLegend"/>
      procedure Collapse            ( const _group   : IGIS_HierarchyGroup
                                    ) ; overload ;

      /// <inheritdoc from="IGIS_PvlControlLegend"/>
      procedure Collapse            ( const _group   : IGIS_HierarchyGroup ;
                                      const _deep    : Boolean ;
                                      const _layers  : Boolean
                                    ) ; overload ;

      /// <inheritdoc from="IGIS_PvlControlLegend"/>
      function  GetNodeAt           ( const _x       : Integer ;
                                      const _y       : Integer
                                    ) : TGIS_TreeNode ;

      /// <inheritdoc from="IGIS_PvlControlLegend"/>
      procedure InvalidateItems ;

      /// <inheritdoc from="IGIS_PvlControlLegend"/>
      function  DrawBmp             ( const _bitmap  : TGIS_Bitmap ;
                                      const _scale   : Double  ;
                                      const _ppi     : Integer
                                    ) : TGIS_Bitmap ;

      /// <inheritdoc from="IGIS_PvlControlLegend"/>
      procedure PrintClipboard      ;



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
                              read  fget_GIS_Layer
                              write fset_GIS_Layer ;

      /// <summary>
      ///   Selected layers.
      /// </summary>
      property GIS_Layers    : TGIS_LayerAbstractList
                              read  fget_GIS_Layers
                              write fset_GIS_Layers ;

      /// <summary>
      ///   Selected group.
      /// </summary>
      property GIS_Group    : IGIS_HierarchyGroup
                              read  fget_GIS_Group
                              write fset_GIS_Group ;

    public // properties

      /// <summary>
      ///   Attached IGIS_Viewer object.
      /// </summary>
      property GIS_Viewer   : IGIS_Viewer
                              read  fget_GIS_Viewer
                              write fset_GIS_Viewer ;

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

      //?/ <summary>
      //?/   Styled settings.
      //?/ </summary>
//?      property StyledSettings         : TGIS_ControlLegendStyledSettings
//?                                        read  FStyledSettings
//?                                        write FStyledSettings ;


    public

      /// <summary>
      ///   BiDi value.
      /// </summary>
      property RightToLeft     : Boolean
                              read  fget_RightToLeft
                              write fset_RightToLeft
                              default false ;

      /// <summary>
      ///   Defines which BiDi will be used: the one from property or the one from
      ///   translation
      /// </summary>
      property RightToLeftFromTranslation
                            : Boolean
                              read  fget_RightToLeftFromTranslation
                              write fset_RightToLeftFromTranslation
                              default false ;

      /// <summary>
      ///   Background color.
      /// </summary>
      property BackgroundColor : TGIS_Color
                              read  fget_BackgroundColor
                              write fset_BackgroundColor;

      /// <summary>
      ///   Display font.
      /// </summary>
      property Font         : TGIS_Font
                              read  fget_Font ;

      /// <summary>
      ///   Display font.
      /// </summary>
      property FontColor    : TGIS_Color
                              read  fget_FontColor
                              write fset_FontColor;



    published // IGIS_PvlControlLegend events

      /// <event/>
      /// <summary>
      ///   Event fired when a layer parameters get changed.
      /// </summary>
      property LayerParamsChangeEvent : TGIS_LayerEvent
                                        read  fget_LayerParamsChangeEvent
                                        write fset_LayerParamsChangeEvent ;

      /// <event/>
      /// <summary>
      ///   Event fired when a layer is activated/deactivated.
      /// </summary>
      property LayerActiveChangeEvent : TGIS_LayerEvent
                                        read  fget_LayerActiveChangeEvent
                                        write fset_LayerActiveChangeEvent ;

      /// <event/>
      /// <summary>
      ///   Event fired upon layer selection.
      /// </summary>
      property LayerSelectEvent       : TGIS_LayerEvent
                                        read  fget_LayerSelectEvent
                                        write fset_LayerSelectEvent ;

      /// <event/>
      /// <summary>
      ///   Event fired when a group is activated/deactivated.
      /// </summary>
      property GroupActiveChangeEvent : TGIS_HierarchyGroupEvent
                                        read  fget_GroupActiveChangeEvent
                                        write fset_GroupActiveChangeEvent;

      /// <event/>
      /// <summary>
      ///   Event fired upon group selection.
      /// </summary>
      property GroupSelectEvent       : TGIS_HierarchyGroupEvent
                                        read  fget_GroupSelectEvent
                                        write fset_GroupSelectEvent ;

      /// <event/>
      /// <summary>
      ///   Event fired upon a change of the order of layers.
      /// </summary>
      property OrderChangeEvent       : TGIS_PvlEvent
                                        read  fget_OrderChangeEvent
                                        write fset_OrderChangeEvent ;

      /// <event/>
      /// <summary>
      ///   Event fired upon opening of the layer properties dialog box.
      /// </summary>
      property OpenDialogEvent        : TGIS_LayerEvent
                                        read  fget_OpenDialogEvent
                                        write fset_OpenDialogEvent ;

      /// <event/>
      /// <summary>
      ///   TapSimple event. Will be fired upon press down/up.
      /// </summary>
      property TapSimpleEvent          : TGIS_PvlMouseEvent
                                         read  fget_TapSimpleEvent
                                         write fset_TapSimpleEvent ;

      /// <event/>
      /// <summary>
      ///   TapLong event. Will be fired upon longer press down.
      /// </summary>
      property TapLongEvent            : TGIS_PvlMouseEvent
                                         read  fget_TapLongEvent
                                         write fset_TapLongEvent ;

      /// <event/>
      /// <summary>
      ///   TapDouble event. Will be fired upon double press down/up.
      /// </summary>
      property TapDoubleEvent           : TGIS_PvlMouseEvent
                                          read  fget_TapDoubleEvent
                                          write fset_TapDoubleEvent ;
  end;



implementation

{$IFDEF DCC}
  uses
    GisRtl
    // ensure that proper implementation files are referenced in Delphi
    {$IFDEF USE_FMX}
      ,FMX.GisPvlControlLegend
    {$ENDIF}
    {$IFDEF USE_VCL}
      ,VCL.GisPvlControlLegend
    {$ENDIF}
    ;
{$ENDIF}

{$REGION 'IGIS_ControlLegend property access routines'}

function TGIS_PvlControlLegend.fget_GIS_Viewer
  : IGIS_Viewer;
begin
  Result := PlatformControl.fget_GIS_Viewer;
end;

procedure TGIS_PvlControlLegend.fset_GIS_Viewer(
  const _value : IGIS_Viewer
);
begin
  PlatformControl.fset_GIS_Viewer( _value ) ;
end;

function TGIS_PvlControlLegend.fget_GIS_Layer
  : TGIS_Layer ;
begin
  Result := PlatformControl.fget_GIS_Layer;
end;

procedure TGIS_PvlControlLegend.fset_GIS_Layer(
  const _value : TGIS_Layer
);
begin
  PlatformControl.fset_GIS_Layer( _value ) ;
end;

function TGIS_PvlControlLegend.fget_GIS_Layers
  : TGIS_LayerAbstractList ;
begin
  Result := PlatformControl.fget_GIS_Layers;
end;

procedure TGIS_PvlControlLegend.fset_GIS_Layers(
  const _value : TGIS_LayerAbstractList
);
begin
  PlatformControl.fset_GIS_Layers( _value ) ;
end;

function TGIS_PvlControlLegend.fget_GIS_Group
  : IGIS_HierarchyGroup ;
begin
  Result := PlatformControl.fget_GIS_Group;
end;

procedure TGIS_PvlControlLegend.fset_GIS_Group(
  const _value : IGIS_HierarchyGroup
);
begin
  PlatformControl.fset_GIS_Group( _value ) ;
end;

function TGIS_PvlControlLegend.fget_Mode
  : TGIS_ControlLegendMode ;
begin
  Result := PlatformControl.fget_Mode;
end;

procedure TGIS_PvlControlLegend.fset_Mode(
  const _value : TGIS_ControlLegendMode
);
begin
  PlatformControl.fset_Mode( _value ) ;
end;

function TGIS_PvlControlLegend.fget_Options
  : TGIS_ControlLegendOptions ;
begin
  Result := PlatformControl.fget_Options;
end;

procedure TGIS_PvlControlLegend.fset_Options(
  const _value : TGIS_ControlLegendOptions
);
begin
  PlatformControl.fset_Options( _value ) ;
end;

function TGIS_PvlControlLegend.fget_ReverseOrder
  : Boolean ;
begin
  Result := PlatformControl.fget_ReverseOrder;
end;

procedure TGIS_PvlControlLegend.fset_ReverseOrder(
  const _value : Boolean
);
begin
  PlatformControl.fset_ReverseOrder( _value ) ;
end;

function TGIS_PvlControlLegend.fget_CompactView
  : Boolean ;
begin
  Result := PlatformControl.fget_CompactView;
end;

procedure TGIS_PvlControlLegend.fset_CompactView(
  const _value : Boolean
);
begin
  PlatformControl.fset_CompactView( _value ) ;
end;

function TGIS_PvlControlLegend.fget_DrawIconStyle
  : TGIS_LegendIconStyle ;
begin
  Result := PlatformControl.fget_DrawIconStyle;
end;

procedure TGIS_PvlControlLegend.fset_DrawIconStyle(
  const _value : TGIS_LegendIconStyle
);
begin
  PlatformControl.fset_DrawIconStyle( _value ) ;
end;

function TGIS_PvlControlLegend.fget_DialogOptions
  : TGIS_ControlLegendDialogOptions ;
begin
  Result := PlatformControl.fget_DialogOptions;
end;

procedure TGIS_PvlControlLegend.fset_DialogOptions(
  const _value : TGIS_ControlLegendDialogOptions
);
begin
  PlatformControl.fset_DialogOptions( _value ) ;
end;

function TGIS_PvlControlLegend.fget_RightToLeft
  : Boolean;
begin
  Result := PlatformControl.fget_RightToLeft;
end;

procedure TGIS_PvlControlLegend.fset_RightToLeft(
  const _value : Boolean
) ;
begin
  PlatformControl.fset_RightToLeft( _value ) ;
end;

function TGIS_PvlControlLegend.fget_RightToLeftFromTranslation
  : Boolean;
begin
  Result := PlatformControl.fget_RightToLeftFromTranslation;
end;

procedure TGIS_PvlControlLegend.fset_RightToLeftFromTranslation(
  const _value : Boolean
) ;
begin
  PlatformControl.fset_RightToLeftFromTranslation( _value ) ;
end;

function TGIS_PvlControlLegend.fget_BackgroundColor
  : TGIS_Color ;
begin
  Result := PlatformControl.fget_BackgroundColor;
end;

procedure TGIS_PvlControlLegend.fset_BackgroundColor(
  const _value : TGIS_Color
) ;
begin
  PlatformControl.fset_BackgroundColor( _value ) ;
end;

function  TGIS_PvlControlLegend.fget_Font
  : TGIS_Font ;
begin
  Result := PlatformControl.fget_Font;
end;

function TGIS_PvlControlLegend.fget_FontColor
  : TGIS_Color ;
begin
  Result := PlatformControl.fget_FontColor;
end;

procedure TGIS_PvlControlLegend.fset_FontColor(
  const _value : TGIS_Color
);
begin
  PlatformControl.fset_FontColor( _value ) ;
end;

function TGIS_PvlControlLegend.fget_SelectedNode
  : TGIS_TreeNode ;
begin
  Result := PlatformControl.fget_SelectedNode;
end;

{$ENDREGION 'IGIS_ControlLegend property access routines'}

{$REGION 'IGIS_ControlLegend events access routines'}

function TGIS_PvlControlLegend.fget_LayerParamsChangeEvent
  : TGIS_LayerEvent;
begin
  Result := PlatformControl.fget_LayerParamsChangeEvent;
end;

procedure TGIS_PvlControlLegend.fset_LayerParamsChangeEvent(
  const _value : TGIS_LayerEvent
);
begin
  PlatformControl.fset_LayerParamsChangeEvent( _value ) ;
end;

function TGIS_PvlControlLegend.fget_LayerActiveChangeEvent
  : TGIS_LayerEvent;
begin
  Result := PlatformControl.fget_LayerActiveChangeEvent;
end;

procedure TGIS_PvlControlLegend.fset_LayerActiveChangeEvent(
  const _value : TGIS_LayerEvent
);
begin
  PlatformControl.fset_LayerActiveChangeEvent( _value ) ;
end;

function TGIS_PvlControlLegend.fget_LayerSelectEvent
  : TGIS_LayerEvent;
begin
  Result := PlatformControl.fget_LayerSelectEvent;
end;

procedure TGIS_PvlControlLegend.fset_LayerSelectEvent(
  const _value : TGIS_LayerEvent
);
begin
  PlatformControl.fset_LayerSelectEvent( _value ) ;
end;

function TGIS_PvlControlLegend.fget_GroupActiveChangeEvent
  : TGIS_HierarchyGroupEvent;
begin
  Result := PlatformControl.fget_GroupActiveChangeEvent;
end;

procedure TGIS_PvlControlLegend.fset_GroupActiveChangeEvent(
  const _value : TGIS_HierarchyGroupEvent
);
begin
  PlatformControl.fset_GroupActiveChangeEvent( _value ) ;
end;

function TGIS_PvlControlLegend.fget_GroupSelectEvent
  : TGIS_HierarchyGroupEvent;
begin
  Result := PlatformControl.fget_GroupSelectEvent;
end;

procedure TGIS_PvlControlLegend.fset_GroupSelectEvent(
  const _value : TGIS_HierarchyGroupEvent
);
begin
  PlatformControl.fset_GroupSelectEvent( _value ) ;
end;

function TGIS_PvlControlLegend.fget_OrderChangeEvent
  : TGIS_PvlEvent;
begin
  Result := PlatformControl.fget_OrderChangeEvent;
end;

procedure TGIS_PvlControlLegend.fset_OrderChangeEvent(
  const _value : TGIS_PvlEvent
);
begin
  PlatformControl.fset_OrderChangeEvent( _value ) ;
end;

function TGIS_PvlControlLegend.fget_OpenDialogEvent
  : TGIS_LayerEvent ;
begin
  Result := PlatformControl.fget_OpenDialogEvent;
end;

procedure TGIS_PvlControlLegend.fset_OpenDialogEvent(
  const _value : TGIS_LayerEvent
) ;
begin
  PlatformControl.fset_OpenDialogEvent( _value ) ;
end;

function TGIS_PvlControlLegend.fget_TapSimpleEvent
  : TGIS_PvlMouseEvent;
begin
  Result := PlatformControl.fget_TapSimpleEvent;
end;

procedure TGIS_PvlControlLegend.fset_TapSimpleEvent(
  const _value : TGIS_PvlMouseEvent
);
begin
  PlatformControl.fset_TapSimpleEvent( _value ) ;
end;

function TGIS_PvlControlLegend.fget_TapLongEvent :
  TGIS_PvlMouseEvent;
begin
  Result := PlatformControl.fget_TapLongEvent;
end;

procedure TGIS_PvlControlLegend.fset_TapLongEvent(
  const _value : TGIS_PvlMouseEvent
);
begin
  PlatformControl.fset_TapLongEvent( _value ) ;
end;

function TGIS_PvlControlLegend.fget_TapDoubleEvent
  : TGIS_PvlMouseEvent;
begin
  Result := PlatformControl.fget_TapDoubleEvent;
end;

procedure TGIS_PvlControlLegend.fset_TapDoubleEvent(
  const _value : TGIS_PvlMouseEvent
);
begin
  PlatformControl.fset_TapDoubleEvent( _value ) ;
end;


{$ENDREGION 'IGIS_ControlLegend events access routines'}

{$REGION 'IGIS_PvlControlLegend public methods'}

function TGIS_PvlControlLegend.IsExpanded(
  const _layer   : TGIS_Layer
) : Boolean ;
begin
  Result := PlatformControl.IsExpanded( _layer );
end;

function TGIS_PvlControlLegend.IsExpanded(
  const _group   : IGIS_HierarchyGroup
) : Boolean ;
begin
  Result := PlatformControl.IsExpanded( _group );
end;

procedure TGIS_PvlControlLegend.Expand(
  const _layer   : TGIS_Layer
) ;
begin
  PlatformControl.Expand( _layer );
end;

procedure TGIS_PvlControlLegend.Expand(
  const _group   : IGIS_HierarchyGroup
);
begin
  PlatformControl.Expand( _group );
end;

procedure TGIS_PvlControlLegend.Expand(
  const _group   : IGIS_HierarchyGroup ;
  const _deep    : Boolean ;
  const _layers  : Boolean
);
begin
  PlatformControl.Expand( _group, _deep, _layers );
end;

procedure TGIS_PvlControlLegend.Collapse(
  const _layer   : TGIS_Layer
);
begin
  PlatformControl.Collapse( _layer );
end;

procedure TGIS_PvlControlLegend.Collapse(
  const _group   : IGIS_HierarchyGroup
);
begin
  PlatformControl.Collapse( _group );
end;

procedure TGIS_PvlControlLegend.Collapse(
  const _group   : IGIS_HierarchyGroup ;
  const _deep    : Boolean ;
  const _layers  : Boolean
);
begin
  PlatformControl.Collapse( _group, _deep, _layers );
end;

function TGIS_PvlControlLegend.GetNodeAt(
  const _x       : Integer ;
  const _y       : Integer
) : TGIS_TreeNode ;
begin
  Result := PlatformControl.GetNodeAt( _x, _y  );
end;

procedure TGIS_PvlControlLegend.InvalidateItems ;
begin
  PlatformControl.InvalidateItems;
end;

function TGIS_PvlControlLegend.DrawBmp(
  const _bitmap  : TGIS_Bitmap ;
  const _scale   : Double  ;
  const _ppi     : Integer
) : TGIS_Bitmap ;
begin
  Result := PlatformControl.DrawBmp( _bitmap, _scale, _ppi );
end;

function TGIS_PvlControlLegend.fget_InternalName
  : string ;
begin
  Result := ( PlatformControl as IGIS_PrintableControl ).InternalName ;
end;

procedure TGIS_PvlControlLegend.fset_InternalName(
  const _value: string
) ;
begin
  ( PlatformControl as IGIS_PrintableControl ).InternalName := _value ;
end;

function TGIS_PvlControlLegend.CreateCopy
  : IGIS_PrintableControl ;
begin
  Result := ( PlatformControl as IGIS_PrintableControl ).CreateCopy ;
end;

procedure TGIS_PvlControlLegend.FreeCopy(
  const _control: IGIS_PrintableControl
) ;
begin
  ( PlatformControl as IGIS_PrintableControl ).FreeCopy( _control ) ;
end;

procedure TGIS_PvlControlLegend.PrintBmp(
  const _bitmap  : TGIS_Bitmap
) ;
begin
  ( PlatformControl as IGIS_PrintableControl ).PrintBmp( _bitmap );
end;

procedure TGIS_PvlControlLegend.PrintClipboard ;
begin
  PlatformControl.PrintClipboard ;
end;

{$ENDREGION 'IGIS_PvlControlLegend'}

{$REGION 'TGIS_PvlControlLegend specific'}

procedure TGIS_PvlControlLegend.initControl;
begin
  inherited;
end;


procedure TGIS_PvlControlLegend.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.Platform.CreateObject(self, 'Legend');
end;

function TGIS_PvlControlLegend.fget_PlatformControl
  : IGIS_PvlControlLegend;
begin
  Result := oPlatform as IGIS_PvlControlLegend;
end;

{$ENDREGION 'TGIS_PvlControlLegend specific'}

end.

