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
  Editing support.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoEditor ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoEditor"'}
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
    System.ComponentModel,
    TatukGIS.RTL,
    TatukGIS.NDK;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.Types,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoClasses ;
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
  ///   For internal use of TGIS_Editor. Basic mode editing operations.
  /// </summary>
  TGIS_EditorAction = {$IFDEF OXYGENE} public {$ENDIF} (

    /// <summary>
    ///   Point will be added.
    /// </summary>
    Add,

    /// <summary>
    ///   Point will be moved.
    /// </summary>
    Move,

    /// <summary>
    ///   Point will be deleted.
    /// </summary>
    Delete,

    /// <summary>
    ///   Point will be located as current.
    /// </summary>
    Locate,

    /// <summary>
    ///   custom edit modes will be added.
    /// </summary>
    Group
  ) ;

  /// <summary>
  ///   For internal use of TGIS_Editor. Status of part editing.
  /// </summary>
  TGIS_EditorPartState = record

      /// <summary>
      ///   True if new part must be added.
      /// </summary>
      MustCreate : Boolean ;

      /// <summary>
      ///   True if part must be deleted.
      /// </summary>
      MustDelete : Boolean ;

      /// <summary>
      ///   Point added to a newly created part.
      /// </summary>
      PointToAdd : TGIS_Point3D ;
  end ;

  /// <summary>
  ///   For internal use of TGIS_Editor. Status of point editing.
  /// </summary>
  TGIS_EditorPointState = record

    /// <summary>
    ///   Buffer for editing - all points in part.
    /// </summary>
    Buf :  TGIS_Point3DArray ;

    /// <summary>
    ///   Number of points in buffer.
    /// </summary>
    Count : Integer ;

    /// <summary>
    ///   Position of points in buffer.
    /// </summary>
    Pos : Integer ;

    /// <summary>
    ///   Action to be performed.
    /// </summary>
    Action : TGIS_EditorAction ;

    /// <summary>
    ///   If True point must be deleted.
    /// </summary>
    MustDeleteOrMove : Boolean ;

    /// <summary>
    ///   New position of point.
    /// </summary>
    Point : TGIS_Point3D ;

    /// <summary>
    ///   Start mouse position - to track minimal movements.
    /// </summary>
    MouseStart : TPoint ;

    /// <summary>
    ///   Switched on MouseBegin/MouseEnd.
    /// </summary>
    WasMouseBegin : Boolean ;
  end ;

type

  /// <summary>
  ///   Editor encapsulation.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     This class is closely connected to TGIS_Viewer, so never construct it
  ///     independently.
  ///   </para>
  ///   <para>
  ///      The basic editing functionality includes the moving of points,
  ///     moving vector vertices, creating new vector points, line, and
  ///     polygons, deleting vector points and vertices, snapping, with mouse
  ///     clicks.
  ///   </para>
  ///   <para>
  ///      Various objects were declared in Pointer manner to avoid circular
  ///     references.
  ///   </para>
  /// </remarks>
  {$IFDEF CLR}
    [TypeConverter(typeOf(ExpandableObjectConverter))]
  {$ENDIF}
  TGIS_Editor = {$IFDEF OXYGENE} public {$ENDIF}
                class ( TGIS_UncountedInterfacedObject, IGIS_Editor )

    private // property internal values

      /// <summary>
      ///   Currently edited shape.
      /// </summary>
      FCurrentShape : TGIS_ShapeAbstract ;

      /// <summary>
      ///   True if editor should be redrawn.
      /// </summary>
      FMustRedraw : Boolean ;

      /// <summary>
      ///   Viewer on which editor has been created.
      /// </summary>
      FViewer : TObject ;

      /// <summary>
      ///   Part number of currently edited shape.
      /// </summary>
      FPart : Integer ;

      /// <summary>
      ///   Source of selection.
      /// </summary>
      FPointerMode : TGIS_PointerMode ;

      /// <summary>
      ///   Tolerance of selection.
      /// </summary>
      FSelectTolerance : Integer ;

      /// <summary>
      ///   Tolerance of selection upon touch gesture.
      /// </summary>
      FSelectToleranceTouch : Integer ;

      /// <summary>
      ///   Tolerance of selection upon pen gesture.
      /// </summary>
      FSelectTolerancePen : Integer ;

      /// <summary>
      ///   Margin of snapping during editing.
      /// </summary>
      FSnapMargin : Integer ;

      /// <summary>
      ///   Layer to which edited point will be
      ///   snapped.
      /// </summary>
      FSnapLayers : TGIS_LayerAbstractList ;

      /// <summary>
      ///   Use snap layers or no snapping.
      /// </summary>
      FUseSnapLayers : Boolean ;

      /// <summary>
      ///   Minimal mouse movement to perform a change.
      /// </summary>
      FMinMove : Integer ;

      /// <summary>
      ///   Show tracking point on snap layer.
      /// </summary>
      FShowTracking : Boolean ;

      /// <summary>
      ///   Show editing shape points numbers
      /// </summary>
      FShowPointsNumbers : Boolean ;

      /// <summary>
      ///   If True, show edges lengths from vertex to vertex
      /// </summary>
      FShowEdgesLengths : Boolean ;

      /// <summary>
      ///   Show editing shape points 3D values
      /// </summary>
      FShowPoints3D      : Boolean ;

      /// <summary>
      ///   Snap type - snap to point or to line
      /// </summary>
      FSnapType : TGIS_EditorSnapType ;

      /// <summary>
      ///   Snap to intersection of shapes
      /// </summary>
      FSnapToIntersection : Boolean ;

      /// <summary>
      ///   Spacing of grid to which points will be snapped.
      /// </summary>
      FSnapGridSpacing : Double ;

      /// <summary>
      ///   Mode of edition.
      /// </summary>
      FMode : TGIS_EditorMode ;

      /// <summary>
      ///   Show tracking line upon point move.
      /// </summary>
      FShowDraggingTrack : Boolean ;

      /// <summary>
      ///   Editing lines style.
      /// </summary>
      FEditingLinesStyle  : TGIS_EditorStyle ;

      /// <summary>
      ///   Editing points style.
      /// </summary>
      FEditingPointsStyle : TGIS_EditorEditingPointsStyle ;

      /// <summary>
      ///   Edge lengths style.
      /// </summary>
      FEditingEdgeLengthsStyle : TGIS_EditorEdgeLengthsStyle ;

      /// <summary>
      ///   If true then Editor is enabled in a ViewerWnd.
      /// </summary>
      FViewerEnabled : Boolean ;

      /// <summary>
      ///   Custom snap event.
      /// </summary>
      FOnSnapPoint : TGIS_EditorSnapPointEvent ;

      /// <summary>
      ///   Point change event.
      /// </summary>
      FOnPointChange : TGIS_EditorPointChangeEvent ;

      /// <summary>
      ///   Point editing event.
      /// </summary>
      FOnPointEditing : TGIS_EditorPointMoveEvent ;

    private // other variables

      /// <summary>
      ///   Status of part editing.
      /// </summary>
      partState : TGIS_EditorPartState ;

      /// <summary>
      ///   Status of point editing.
      /// </summary>
      pointState : TGIS_EditorPointState ;

      /// <summary>
      ///   List of undo items.
      /// </summary>
      undoList : TGIS_ObjectList ;

      /// <summary>
      ///   List of redo items.
      /// </summary>
      redoList : TGIS_ObjectList ;

      /// <summary>
      ///   Old mouse position upon editing.
      /// </summary>
      oldEditPoint : TGIS_Point3D ;

      /// <summary>
      ///   Current editor mode.
      /// </summary>
      FEditorMode : TGIS_EditorModeEx ;

      /// <summary>
      ///   Is point dragging mode active?
      /// </summary>
      pointDrag : Boolean ;

      /// <summary>
      ///   Snapping layer added dynamically upon EditShape and to be removed on EndEdit.
      /// </summary>
      oDynamicSnapLayer : TGIS_LayerAbstract ;
      /// <summary>
      ///   True if edited shape is topmost.
      /// </summary>
      bTopmostShape : Boolean ;

    private // other private methods

      /// <summary>
      ///   Initialization of internal structures
      /// </summary>
      procedure initialState        ;

      /// <summary>
      ///   Clears the list of undo able actions.
      /// </summary>
      procedure clearUndo           ;

      /// <summary>
      ///   Clears the list of redo able actions.
      /// </summary>
      procedure clearRedo           ;

      /// <summary>
      ///   Perform single editing action. Action parameters are in pointState.
      /// </summary>
      procedure doAction            ; overload;

      /// <summary>
      ///   Perform single editing action. Action parameters are in pointState.
      /// </summary>
      procedure doAction            ( const _recGroup   : Boolean
                                    ) ; overload;

      /// <summary>
      ///   Undo/Redo a given action.
      /// </summary>
      /// <param name="_pitem">
      ///   pointer to undo/redo action
      /// </param>
      procedure undoAction        ( const _pitem      : TObject
                                  ) ;

      /// <summary>
      ///   Record an action on the undo list.
      /// </summary>
      /// <param name="_action">
      ///   performed action
      /// </param>
      /// <param name="_point_pos">
      ///   point position in editing buffer
      /// </param>
      procedure recordAction        ( const _action     : TGIS_EditorAction ;
                                      const _point_pos  : Integer ;
                                      const _delta      : TGIS_Point3D
                                    ) ;

      procedure recordGroup         ;

      /// <summary>
      ///   Load shape from the shape geometry into an editing buffer.
      /// </summary>
      procedure loadShape           ;

      /// <summary>
      ///   Find a correct snap point.
      /// </summary>
      /// <param name="_ptg">
      ///   point in which snap must be found
      /// </param>
      function  doSnapPoint         ( const _ptg        : TGIS_Point3D
                                    ) : TGIS_Point3D ; overload ;

      function  doSnapPoint         ( const _ptg        : TGIS_Point3D ;
                                        var _found      : Boolean ;
                                        var _snapType   : TGIS_EditorSnapResultType
                                    ) : TGIS_Point3D ; overload ;

      function  doPointChange       ( const _ptg        : TGIS_Point3D ;
                                      const _operation  : TGIS_EditorOperation
                                    ) : TGIS_Point3D ;
      procedure doPointEditing      ( const _ptg        : TGIS_Point3D ;
                                      const _pt         : TPoint
                                    )  ;
    private

      /// <summary>
      ///   call InvalidateEditor() with proper mode.
      /// </summary>
      procedure invalidateInternal  ;

      /// <summary>
      ///   See EditShape()
      /// </summary>
      procedure editShapeInternal   ( const _shp        : TObject ;
                                      const _part       : Integer
                                    ) ;

      /// <summary>
      ///   See CreateShape()
      /// </summary>
      procedure createShapeInternal ( const _layer      : TGIS_LayerAbstract ;
                                      const _ptg        : TGIS_Point3D ;
                                      const _type       : TGIS_ShapeType ;
                                      const _dim        : TGIS_DimensionType
                                    ) ;


      /// <summary>
      ///   See EndEdit()
      /// </summary>
      procedure endEditInternal     ;

      /// <summary>
      ///   See EditShape()
      /// </summary>
      procedure deleteShapeInternal ;

      /// <summary>
      ///   See RevertShape()
      /// </summary>
      procedure revertShapeInternal ;

      /// <summary>
      ///   See Undo()
      /// </summary>
      procedure undoInternal        ;

      /// <summary>
      ///   See Redo()
      /// </summary>
      procedure redoInternal        ;

      /// <summary>
      ///   See ChangeWinding()
      /// </summary>
      procedure changeWindingInternal;

      /// <summary>
      ///   See CreatePart()
      /// </summary>
      procedure createPartInternal  ( const _ptg        : TGIS_Point3D
                                    ) ;

      /// <summary>
      ///   See DeletePart()
      /// </summary>
      procedure deletePartInternal  ;

      /// <summary>
      ///   See MouseBegin()
      /// </summary>
      procedure mouseBeginInternal  ( const _pt         : TPoint ;
                                      const _nearest    : Boolean
                                    ) ;

      /// <summary>
      ///   See MouseBegin()
      /// </summary>
      procedure mouseMoveInternal   ( const _pt         : TPoint
                                    ) ;

      /// <summary>
      ///   See MouseEnd()
      /// </summary>
      procedure mouseEndInternal    ( const _pt         : TPoint
                                    ) ;

      /// <summary>
      ///   See AddPoint()
      /// </summary>
      procedure addPointInternal    ( const _ptg        : TGIS_Point3D ;
                                      const _group      : Boolean
                                    ) ;

      /// <summary>
      ///   See InsertPoint()
      /// </summary>
      procedure insertPointInternal ( const _pos        : Integer ;
                                      const _ptg        : TGIS_Point3D
                                    ) ;

      /// <summary>
      ///   See MovePoint()
      /// </summary>
      procedure movePointInternal   ( const _pos        : Integer ;
                                      const _ptg        : TGIS_Point3D
                                    ) ;

      /// <summary>
      ///   See DeletePoint()
      /// </summary>
      procedure deletePointInternal ( const _pos        : Integer
                                    ) ;

    protected // properties from IGIS_Editor
      function  fget_CurrentShape       : TGIS_ShapeAbstract ;
      function  fget_EditingLinesStyle  : TGIS_EditorStyle ;
      procedure fset_EditingLinesStyle  ( const _value : TGIS_EditorStyle
                                        ) ;
      function  fget_EditingPointsStyle : TGIS_EditorEditingPointsStyle ;
      procedure fset_EditingPointsStyle ( const _value : TGIS_EditorEditingPointsStyle
                                        ) ;
      function  fget_EdgeLengthsStyle   : TGIS_EditorEdgeLengthsStyle ;
      procedure fset_EdgeLengthsStyle   ( const _value : TGIS_EditorEdgeLengthsStyle
                                        ) ;
      function  fget_EditorMode         : TGIS_EditorModeEx ;
      procedure fset_EditorMode         ( const _value : TGIS_EditorModeEx
                                        ) ;
      function  fget_MustRedraw         : Boolean ;
      procedure fset_MustRedraw         ( const _value : Boolean
                                        ) ;
      function  fget_InEdit             : Boolean ;
      function  fget_Layer              : TGIS_LayerAbstract ;
      function  fget_MinMove            : Integer ;
      procedure fset_MinMove            ( const _value : Integer
                                        ) ;
      function  fget_Mode               : TGIS_EditorMode ;
      procedure fset_Mode               ( const _value : TGIS_EditorMode
                                        ) ;
      function  fget_Part               : Integer ;
      function  fget_Point              ( const _pos   : Integer
                                        ) : TGIS_Point3D ;
      procedure fset_Point              ( const _pos   : Integer ;
                                          const _value : TGIS_Point3D
                                        ) ;
      function  fget_PointCount         : Integer ;
      function  fget_PointPos           : Integer ;
      procedure fset_PointPos           ( const _value : Integer
                                        ) ;
      function  fget_PointerMode        : TGIS_PointerMode ;
      procedure fset_PointerMode        ( const _value : TGIS_PointerMode
                                        ) ; 
      function  fget_RedoState          : Boolean ;
      function  fget_SelectTolerance    : Integer ;
      procedure fset_SelectTolerance    ( const _value : Integer
                                        ) ;
      function  fget_SelectTolerancePen : Integer ;
      procedure fset_SelectTolerancePen ( const _value : Integer
                                        ) ;
      function  fget_SelectToleranceTouch : Integer ;
      procedure fset_SelectToleranceTouch ( const _value : Integer
                                        ) ;
      function  fget_ShowDraggingTrack  : Boolean ;
      procedure fset_ShowDraggingTrack  ( const _value : Boolean
                                        ) ;
      function  fget_ShowPoints3D       : Boolean ;
      procedure fset_ShowPoints3D       ( const _value : Boolean
                                        ) ;
      function  fget_ShowPointsNumbers  : Boolean ;
      procedure fset_ShowPointsNumbers  ( const _value : Boolean
                                        ) ;
      function  fget_ShowEdgesLengths   : Boolean ;
      procedure fset_ShowEdgesLengths   ( const _value : Boolean
                                        ) ;
      function  fget_ShowTracking       : Boolean ;
      procedure fset_ShowTracking       ( const _value : Boolean
                                        ) ;
      function  fget_SnapLayer          : TGIS_LayerAbstract ;
      procedure fset_SnapLayer          ( const _value : TGIS_LayerAbstract
                                        ) ;
      function  fget_SnapMargin         : Integer ;
      procedure fset_SnapMargin         ( const _value : Integer
                                        ) ;
      function  fget_SnapType           : TGIS_EditorSnapType ;
      procedure fset_SnapType           ( const _value : TGIS_EditorSnapType
                                        ) ;
      function  fget_SnapToIntersection : Boolean ;
      procedure fset_SnapToIntersection ( const _value : Boolean
                                        ) ;
      function  fget_SnapGridSpacing    : Double ;
      procedure fset_SnapGridSpacing    ( const _value : Double
                                        ) ;
      function  fget_Uid                : TGIS_Uid ;
      function  fget_UndoState          : Boolean ;
      function  fget_Viewer             : TObject ;
      function  fget_ViewerEnabled      : Boolean ;
      procedure fset_ViewerEnabled      ( const _value : Boolean
                                        ) ;

    public // constructors

      /// <summary>
      ///   Create an instance of editor inside TGIS_Viewer.
      /// </summary>
      /// <param name="_viewer">
      ///   TGIS_Viewer in which editor must be created
      /// </param>
      constructor  Create           ( const _viewer     : TObject
                                    ) ;
      {$IFNDEF OXYGENE}

        /// <summary>
        ///   Destroy instance.
        /// </summary>
        destructor Destroy          ; override;
      {$ENDIF}

    public // API from IGIS_Editor

      /// <inheritdoc from="IGIS_Editor"/>
      procedure AddPoint            ( const _ptg        : TGIS_Point3D
                                    ) ;

      /// <inheritdoc from="IGIS_Editor"/>
      procedure AddPointEx          ( const _ptg        : TGIS_Point3D
                                    ) ;

      /// <inheritdoc from="IGIS_Editor"/>
      procedure ChangeWinding       ;

      /// <inheritdoc from="IGIS_Editor"/>
      procedure CreatePart          ( const _ptg        : TGIS_Point3D
                                    ) ;

      /// <inheritdoc from="IGIS_Editor"/>
      procedure CreateShape         ( const _layer      : TGIS_LayerAbstract ;
                                      const _ptg        : TGIS_Point ;
                                      const _type       : TGIS_ShapeType
                                    ) ; overload;

      /// <inheritdoc from="IGIS_Editor"/>
      procedure CreateShape         ( const _layer      : TGIS_LayerAbstract ;
                                      const _ptg        : TGIS_Point3D ;
                                      const _type       : TGIS_ShapeType
                                    ) ; overload;

      /// <inheritdoc from="IGIS_Editor"/>
      procedure CreateShape         ( const _layer      : TGIS_LayerAbstract ;
                                      const _ptg        : TGIS_Point3D ;
                                      const _type       : TGIS_ShapeType ;
                                      const _dim        : TGIS_DimensionType
                                    ) ; overload;

      /// <inheritdoc from="IGIS_Editor"/>
      procedure DeletePart          ;

      /// <inheritdoc from="IGIS_Editor"/>
      procedure DeletePoint         ( const _pos        : Integer
                                    ) ;

      /// <inheritdoc from="IGIS_Editor"/>
      procedure DeleteShape         ;

      /// <inheritdoc from="IGIS_Editor"/>
      procedure EditShape           ( const _shp        : TGIS_ShapeAbstract ;
                                      const _part       : Integer
                                    ) ;

      /// <inheritdoc from="IGIS_Editor"/>
      procedure EndEdit             ;

      /// <inheritdoc from="IGIS_Editor"/>
      procedure InsertPoint         ( const _pos        : Integer ;
                                      const _ptg        : TGIS_Point3D
                                    ) ;

      /// <inheritdoc from="IGIS_Editor"/>
      procedure MouseBegin          ( const _pt         : TPoint ;
                                      const _nearest    : Boolean
                                    ) ;

      /// <inheritdoc from="IGIS_Editor"/>
      procedure MouseMove           ( const _pt         : TPoint
                                    ) ;

      /// <inheritdoc from="IGIS_Editor"/>
      procedure MouseEnd            ( const _pt         : TPoint
                                    ) ;

      /// <inheritdoc from="IGIS_Editor"/>
      procedure MovePoint           ( const _pos        : Integer ;
                                      const _ptg        : TGIS_Point3D
                                    ) ;

      /// <inheritdoc from="IGIS_Editor"/>
      procedure Redo                ;

      /// <inheritdoc from="IGIS_Editor"/>
      procedure RefreshShape        ;

      /// <inheritdoc from="IGIS_Editor"/>
      procedure RevertShape         ;

      /// <inheritdoc from="IGIS_Editor"/>
      procedure Undo                ;

      /// <inheritdoc from="IGIS_Editor"/>
      procedure ClearSnapLayers ;

      /// <inheritdoc from="IGIS_Editor"/>
      function  AddSnapLayer        ( const _layer       : TGIS_LayerAbstract
                                    ) : Integer ;

      /// <inheritdoc from="IGIS_Editor"/>
      procedure RemoveSnapLayer     ( const _layer       : TGIS_LayerAbstract
                                    ) ;

      /// <inheritdoc from="IGIS_Editor"/>
      function FindSnapLayer        ( const _layer       : TGIS_LayerAbstract
                                    ) : Boolean ;

      /// <inheritdoc from="IGIS_Editor"/>
      function FindSnapPoint        ( const _pt          : TPoint ;
                                        var _ptg         : TGIS_Point ;
                                        var _snapType    : TGIS_EditorSnapResultType
                                    ) : Boolean ;
    public // properties from IGIS_Editor

      /// <inheritdoc from="IGIS_Editor"/>
      {$IFDEF CLR}
        [Browsable(False)]
      {$ENDIF}
      property CanRedo            : Boolean
                                    read fget_RedoState ;

      /// <inheritdoc from="IGIS_Editor"/>
      {$IFDEF CLR}
        [Browsable(False)]
      {$ENDIF}
      property CanUndo            : Boolean
                                    read fget_UndoState ;

      /// <inheritdoc from="IGIS_Editor"/>
      {$IFDEF CLR}
        [Browsable(False)]
      {$ENDIF}
      property CurrentShape       : TGIS_ShapeAbstract
                                    read fget_CurrentShape ;

      /// <inheritdoc from="IGIS_Editor"/>
      {$IFDEF CLR}
        [Browsable(False)]
      {$ENDIF}
      property MustRedraw         : Boolean
                                    read  fget_MustRedraw
                                    write fset_MustRedraw ;


      /// <inheritdoc from="IGIS_Editor"/>
      {$IFDEF CLR}
        [Browsable(False)]
      {$ENDIF}
      property InEdit             : Boolean
                                    read fget_InEdit ;

      /// <inheritdoc from="IGIS_Editor"/>
      {$IFDEF CLR}
        [Browsable(False)]
      {$ENDIF}
      property Layer              : TGIS_LayerAbstract
                                    read fget_Layer  ;

      /// <inheritdoc from="IGIS_Editor"/>
      {$IFDEF CLR}
        [Browsable(False)]
      {$ENDIF}
      property Mode               : TGIS_EditorMode
                                    read  fget_Mode
                                    write fset_Mode
                                    {$IFDEF OXYGENE}
                                      ;
                                    {$ELSE}
                                      default TGIS_EditorMode.Default ;
                                    {$ENDIF}

      /// <inheritdoc from="IGIS_Editor"/>
      {$IFDEF CLR}
        [Browsable(False)]
      {$ENDIF}
      property Part               : Integer
                                    read fget_Part ;

      /// <summary>
      ///   Access to individual points in a part. Points are counted from 0
      ///   to PointCount.
      /// </summary>
      /// <param name="_pos">
      ///   index of a point
      /// </param>
      {$IFDEF CLR}
        [Browsable(False)]
      {$ENDIF}
      property Point[ const _pos : Integer ]
                                  : TGIS_Point3D
                                    read  fget_Point
                                    write fset_Point ;

      /// <inheritdoc from="IGIS_Editor"/>
      {$IFDEF CLR}
        [Browsable(False)]
      {$ENDIF}
      property PointCount         : Integer
                                    read fget_PointCount ;

      /// <inheritdoc from="IGIS_Editor"/>
      {$IFDEF CLR}
        [Browsable(False)]
      {$ENDIF}
      property PointPos           : Integer
                                    read  fget_PointPos
                                    write fset_PointPos ;

      /// <inheritdoc from="IGIS_Editor"/>
      {$IFDEF CLR}
        [Browsable(False)]
      {$ENDIF}
      property PointerMode        : TGIS_PointerMode
                                    read  fget_PointerMode
                                    write fset_PointerMode ;

      /// <inheritdoc from="IGIS_Editor"/>
      {$IFDEF DCC} [weak] {$ENDIF}
      {$IFDEF CLR}
        [Browsable(False)]
      {$ENDIF}
      property SnapLayer          : TGIS_LayerAbstract
                                    read  fget_SnapLayer
                                    write fset_SnapLayer ;

      /// <inheritdoc from="IGIS_Editor"/>
      {$IFDEF CLR}
        [Browsable(False)]
      {$ENDIF}
      property Uid                : TGIS_Uid
                                    read fget_Uid ;

      /// <inheritdoc from="IGIS_Editor"/>
      {$IFDEF CLR}
        [Browsable(False)]
      {$ENDIF}
      property Viewer             : TObject
                                    read fget_Viewer ;

    {$IFDEF OXYGENE} public {$ELSE} published {$ENDIF} // from IGIS_Editor

      /// <inheritdoc from="IGIS_Editor"/>
      property EditingLinesStyle  : TGIS_EditorStyle
                                    read  fget_EditingLinesStyle
                                    write fset_EditingLinesStyle ;

      /// <inheritdoc from="IGIS_Editor"/>
      property EditingPointsStyle : TGIS_EditorEditingPointsStyle
                                    read  fget_EditingPointsStyle
                                    write fset_EditingPointsStyle ;

      /// <inheritdoc from="IGIS_Editor"/>
      property EditingEdgeLengthsStyle : TGIS_EditorEdgeLengthsStyle
                                    read  fget_EdgeLengthsStyle
                                    write fset_EdgeLengthsStyle ;

      /// <inheritdoc from="IGIS_Editor"/>
      {$IFDEF CLR}
        [DefaultValue(0)]
      {$ENDIF}
      property EditorMode         : TGIS_EditorModeEx
                                    read  fget_EditorMode
                                    write fset_EditorMode
                                    {$IFDEF OXYGENE}
                                      ;
                                    {$ELSE}
                                      default TGIS_EditorModeEx.Normal ;
                                    {$ENDIF}

      /// <inheritdoc from="IGIS_Editor"/>
      {$IFDEF CLR}
        [DefaultValue(GIS_MIN_EDIT_MOVE)]
      {$ENDIF}
      property MinMove            : Integer
                                    read  fget_MinMove
                                    write fset_MinMove
                                    {$IFDEF OXYGENE}
                                      ;
                                    {$ELSE}
                                      default GIS_MIN_EDIT_MOVE ;
                                    {$ENDIF}

      /// <inheritdoc from="IGIS_Editor"/>
      {$IFDEF CLR}
        [DefaultValue(GIS_SELECT_TOLERANCE)]
      {$ENDIF}
      property SelectTolerance    : Integer
                                    read  fget_SelectTolerance
                                    write fset_SelectTolerance
                                    {$IFDEF OXYGENE}
                                      ;
                                    {$ELSE}
                                      default GIS_SELECT_TOLERANCE ;
                                    {$ENDIF}

      /// <inheritdoc from="IGIS_Editor"/>
      {$IFDEF CLR}
        [DefaultValue(GIS_SELECT_TOLERANCE_PEN)]
      {$ENDIF}
      property SelectTolerancePen : Integer
                                    read  fget_SelectTolerancePen
                                    write fset_SelectTolerancePen
                                    {$IFDEF OXYGENE}
                                      ;
                                    {$ELSE}
                                      default GIS_SELECT_TOLERANCE_PEN ;
                                    {$ENDIF}

      /// <inheritdoc from="IGIS_Editor"/>
      {$IFDEF CLR}
        [DefaultValue(GIS_SELECT_TOLERANCE_TOUCH)]
      {$ENDIF}
      property SelectToleranceTouch : Integer
                                      read  fget_SelectToleranceTouch
                                      write fset_SelectToleranceTouch
                                    {$IFDEF OXYGENE}
                                      ;
                                    {$ELSE}
                                      default GIS_SELECT_TOLERANCE_TOUCH ;
                                    {$ENDIF}

      /// <summary>
      ///   Show dragging track upon point move.
      /// </summary>
      {$IFDEF CLR}
        [DefaultValue(True)]
      {$ENDIF}
      property ShowDraggingTrack  : Boolean
                                    read  fget_ShowDraggingTrack
                                    write fset_ShowDraggingTrack
                                    {$IFDEF OXYGENE}
                                      ;
                                    {$ELSE}
                                      default True ;
                                    {$ENDIF}

      /// <inheritdoc from="IGIS_Exitor"/>
      {$IFDEF CLR}
        [DefaultValue(True)]
      {$ENDIF}
      property ShowPoints3D       : Boolean
                                    read  fget_ShowPoints3D
                                    write fset_ShowPoints3D
                                    {$IFDEF OXYGENE}
                                      ;
                                    {$ELSE}
                                      default True ;
                                    {$ENDIF}

      /// <inheritdoc from="IGIS_Exitor"/>
      {$IFDEF CLR}
        [DefaultValue(True)]
      {$ENDIF}
      property ShowPointsNumbers  : Boolean
                                    read  fget_ShowPointsNumbers
                                    write fset_ShowPointsNumbers
                                    {$IFDEF OXYGENE}
                                      ;
                                    {$ELSE}
                                      default True ;
                                    {$ENDIF}

      /// <inheritdoc from="IGIS_Exitor"/>
      {$IFDEF CLR}
        [DefaultValue(True)]
      {$ENDIF}
      property ShowEdgesLengths   : Boolean
                                    read  fget_ShowEdgesLengths
                                    write fset_ShowEdgesLengths
                                    {$IFDEF OXYGENE}
                                      ;
                                    {$ELSE}
                                      default False ;
                                    {$ENDIF}


      /// <inheritdoc from="IGIS_Exitor"/>
      {$IFDEF CLR}
        [DefaultValue(True)]
      {$ENDIF}
      property ShowTracking       : Boolean
                                    read  fget_ShowTracking
                                    write fset_ShowTracking
                                    {$IFDEF OXYGENE}
                                      ;
                                    {$ELSE}
                                      default True ;
                                    {$ENDIF}

      /// <inheritdoc from="IGIS_Exitor"/>
      {$IFDEF CLR}
        [DefaultValue(GIS_SNAP_MARGIN)]
      {$ENDIF}
      property SnapMargin         : Integer
                                    read  fget_SnapMargin
                                    write fset_SnapMargin
                                    {$IFDEF OXYGENE}
                                      ;
                                    {$ELSE}
                                      default GIS_SNAP_MARGIN ;
                                    {$ENDIF}

      /// <inheritdoc from="IGIS_Exitor"/>
      {$IFDEF CLR}
        [DefaultValue(0)]
      {$ENDIF}
      property SnapType           : TGIS_EditorSnapType
                                    read  fget_SnapType
                                    write fset_SnapType
                                    {$IFDEF OXYGENE}
                                      ;
                                    {$ELSE}
                                      default TGIS_EditorSnapType.Point ;
                                    {$ENDIF}

      /// <inheritdoc from="IGIS_Exitor"/>
      {$IFDEF CLR}
        [DefaultValue(False)]
      {$ENDIF}
      property SnapToIntersection : Boolean
                                    read  fget_SnapToIntersection
                                    write fset_SnapToIntersection
                                    {$IFDEF OXYGENE}
                                      ;
                                    {$ELSE}
                                      default False ;
                                    {$ENDIF}

      /// <inheritdoc from="IGIS_Exitor"/>
      property SnapGridSpacing : Double
                                    read  fget_SnapGridSpacing
                                    write fset_SnapGridSpacing ;
      /// <inheritdoc from="IGIS_Exitor"/>
      property ViewerEnabled : Boolean
                                    read  fget_ViewerEnabled
                                    write fset_ViewerEnabled ;
    published //events
      /// <event/>
      /// <summary>
      ///   Event to call a custom snap routine.
      /// </summary>
      {$IFDEF CLR}
        event    SnapPointEvent   : TGIS_EditorSnapPointEvent
                                    delegate FOnSnapPoint ;
      {$ELSE}
        property SnapPointEvent   : TGIS_EditorSnapPointEvent
                                    read  FOnSnapPoint
                                    write FOnSnapPoint ;
      {$ENDIF}

      /// <event/>
      /// <summary>
      ///   Event to notify a point change.
      /// </summary>
      {$IFDEF CLR}
        event    PointChangeEvent   : TGIS_EditorPointChangeEvent
                                      delegate FOnPointChange ;
      {$ELSE}
        property PointChangeEvent   : TGIS_EditorPointChangeEvent
                                      read  FOnPointChange
                                      write FOnPointChange ;
      {$ENDIF}

      /// <event/>
      /// <summary>
      ///   Event to notify editing a point.
      /// </summary>
      {$IFDEF CLR}
        event    PointEditingEvent   : TGIS_EditorPointMoveEvent
                                       delegate FOnPointEditing ;
      {$ELSE}
        property PointEditingEvent   : TGIS_EditorPointMoveEvent
                                      read  FOnPointEditing
                                      write FOnPointEditing ;
      {$ENDIF}

  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Math,

    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoLayer,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoTopology,
    Lider.CG.GIS.GeoViewer;
{$ENDIF}

type
  { Encapsulation of single undo/redo action.
  }
  T_undoItem = class
    Undo        : Boolean           ; // True - undo; False - redo
    Action      : TGIS_EditorAction ; // Recorded action
    PointPos    : Integer           ; // Point position in editing buffer
    Delta       : TGIS_Point3D      ; // Delta value
  end ;


//============================================================================
// TGIS_Editor
//============================================================================

  constructor TGIS_Editor.Create( const _viewer : TObject ) ;
  begin
    inherited Create ;

    FViewer  := _viewer ;

    undoList := TGIS_ObjectList.Create( False ) ;
    redoList := TGIS_ObjectList.Create( False ) ;

    FPointerMode          := TGIS_PointerMode.Mouse ;
    FSelectTolerance      := GIS_SELECT_TOLERANCE ;
    FSelectTolerancePen   := GIS_SELECT_TOLERANCE_PEN ;
    FSelectToleranceTouch := GIS_SELECT_TOLERANCE_TOUCH ;
    FSnapMargin           := GIS_SNAP_MARGIN   ;
    FMinMove              := GIS_MIN_EDIT_MOVE ;
    FShowTracking         := True ;
    FShowDraggingTrack    := True ;
    FShowPointsNumbers    := True ;
    FShowEdgesLengths     := False ;
    FShowPoints3D         := False ;
    FSnapType             := TGIS_EditorSnapType.Point ;
    FMode                 := TGIS_EditorMode.Default   ;
    {$IFDEF GIS_NORECORDS}
      oldEditPoint     := new TGIS_Point3D ;
    {$ENDIF}
    FSnapToIntersection := False ;
    FSnapGridSpacing    := 1 ;

    FEditingLinesStyle := TGIS_EditorStyle.Create ;
    FEditingLinesStyle.BrushStyle := TGIS_BrushStyle.Solid ;
    FEditingLinesStyle.BrushColor := TGIS_Color.Yellow ;
    FEditingLinesStyle.PenStyle   := TGIS_PenStyle.Dot ;
    FEditingLinesStyle.PenColor   := TGIS_Color.Black ;
    FEditingLinesStyle.PenWidth   := 1 ;

    FEditingPointsStyle := TGIS_EditorEditingPointsStyle.Create ;
    FEditingPointsStyle.PointsFont.Name           := 'Arial' ;
    FEditingPointsStyle.PointsFont.Size           := 7 ;
    FEditingPointsStyle.PointsFont.Color          := TGIS_Color.Green ;
    FEditingPointsStyle.PointsFont.Style          := GisGetEmptyFontStyle ;

    FEditingPointsStyle.PointsBackground          := TGIS_Color.Green ;

    FEditingPointsStyle.ActivePoints.BrushStyle   := TGIS_BrushStyle.Solid ;
    FEditingPointsStyle.ActivePoints.BrushColor   := TGIS_Color.Green ;
    FEditingPointsStyle.ActivePoints.PenStyle     := TGIS_PenStyle.Solid ;
    FEditingPointsStyle.ActivePoints.PenColor     := TGIS_Color.Black ;
    FEditingPointsStyle.ActivePoints.PenWidth     := 0 ;

    FEditingPointsStyle.InactivePoints.BrushStyle := TGIS_BrushStyle.Solid ;
    FEditingPointsStyle.InactivePoints.BrushColor := TGIS_Color.Blue ;
    FEditingPointsStyle.InactivePoints.PenStyle   := TGIS_PenStyle.Solid ;
    FEditingPointsStyle.InactivePoints.PenColor   := TGIS_Color.Black ;
    FEditingPointsStyle.InactivePoints.PenWidth   := 0 ;

    FEditingPointsStyle.SelectedPoints.BrushStyle := TGIS_BrushStyle.Solid ;
    FEditingPointsStyle.SelectedPoints.BrushColor := TGIS_Color.Red ;
    FEditingPointsStyle.SelectedPoints.PenStyle   := TGIS_PenStyle.Solid ;
    FEditingPointsStyle.SelectedPoints.PenColor   := TGIS_Color.Black ;
    FEditingPointsStyle.SelectedPoints.PenWidth   := 0 ;

    FEditingPointsStyle.Points3D.BrushStyle       := TGIS_BrushStyle.Solid ;
    FEditingPointsStyle.Points3D.BrushColor       := TGIS_Color.Red ;
    FEditingPointsStyle.Points3D.PenStyle         := TGIS_PenStyle.Solid ;
    FEditingPointsStyle.Points3D.PenColor         := TGIS_Color.Olive ;
    FEditingPointsStyle.Points3D.PenWidth         := 0 ;

    FEditingPointsStyle.SnappingPoints.BrushStyle := TGIS_BrushStyle.Solid ;
    FEditingPointsStyle.SnappingPoints.BrushColor := TGIS_Color.Yellow ;
    FEditingPointsStyle.SnappingPoints.PenStyle   := TGIS_PenStyle.Solid ;
    FEditingPointsStyle.SnappingPoints.PenColor   := TGIS_Color.Black ;
    FEditingPointsStyle.SnappingPoints.PenWidth   := 0 ;

    FEditingEdgeLengthsStyle := TGIS_EditorEdgeLengthsStyle.Create ;
    FEditingEdgeLengthsStyle.Font.Name           := 'Arial' ;
    FEditingEdgeLengthsStyle.Font.Size           := 6 ;
    FEditingEdgeLengthsStyle.Font.Color          := TGIS_Color.Black ;
    FEditingEdgeLengthsStyle.Font.Style          := GisGetEmptyFontStyle ;

    FEditingEdgeLengthsStyle.Background.BrushStyle   := TGIS_BrushStyle.Clear ;
    FEditingEdgeLengthsStyle.Background.BrushColor   := TGIS_Color.Black ;
    FEditingEdgeLengthsStyle.Background.PenStyle     := TGIS_PenStyle.Solid ;
    FEditingEdgeLengthsStyle.Background.PenColor     := TGIS_Color.Black ;
    FEditingEdgeLengthsStyle.Background.PenWidth     := 0 ;

    FSnapLayers := TGIS_LayerAbstractList.Create( False ) ;
    FUseSnapLayers := False ;

    oDynamicSnapLayer := nil ;

    FViewerEnabled := False ;

    initialState ;
  end ;

  {$IFNDEF OXYGENE}

    destructor TGIS_Editor.Destroy ;
    begin
      clearUndo ;
      clearRedo ;
      undoList.Free ;
      redoList.Free ;
      FEditingLinesStyle.Free ;
      FEditingPointsStyle.Free ;
      FEditingEdgeLengthsStyle.Free ;
      FSnapLayers.Free ;

      inherited ;
    end ;
  {$ENDIF}

  procedure TGIS_Editor.initialState ;
  begin
    pointState.Count            := 0 ;
    pointState.Pos              := 0 ;
    pointState.MustDeleteOrMove := False ;
    pointState.WasMouseBegin    := False ;

    FEditorMode := TGIS_EditorModeEx.Normal ;

    clearUndo ;
    clearRedo ;
  end ;

  procedure TGIS_Editor.clearUndo ;
  var
    i : Integer ;
  begin
    {$IFNDEF NEXTGEN}
      for i := undoList.Count -1 downto 0 do
        FreeObjectNotNil( undoList[ i ] ) ;
    {$ENDIF}

    undoList.Clear ;
  end ;

  procedure TGIS_Editor.clearRedo ;
  var
    i : Integer ;
  begin
    {$IFNDEF NEXTGEN}
      for i := redoList.Count -1 downto 0 do
        FreeObjectNotNil( redoList[ i ] ) ;
    {$ENDIF}

    redoList.Clear ;
  end ;

  procedure TGIS_Editor.doAction ;
  begin
    doAction( True ) ;
  end;

  procedure TGIS_Editor.doAction( const _recGroup : Boolean ) ;
  var
    i      : Integer ;
  begin
    case pointState.Action of
      TGIS_EditorAction.Delete :
        begin
          if pointState.Count > 1 then
          begin
            recordAction( pointState.Action, pointState.Pos,
                          pointState.Buf[pointState.Pos] ) ;

            if _recGroup then
              recordGroup ;

            doPointChange( pointState.Point, TGIS_EditorOperation.Delete ) ;

            dec( pointState.Count ) ;
            for i := pointState.Pos to pointState.Count - 1 do
              pointState.Buf[ i ] := pointState.Buf[ i+1 ] ;

            dec( pointState.Pos ) ;
            if ( pointState.Pos   < 0 ) and ( pointState.Count > 0 ) then
            begin
              // first point deleted, make the next one active
              pointState.Pos := 0  ;
            end;

            if pointState.Pos > pointState.Count-1 then
            begin
              // trying to set active after the last -
              // make the last active
              pointState.Pos := pointState.Count-1 ;
            end;

          end ;
        end ;
      TGIS_EditorAction.Move   :
        begin
          if pointState.Pos >= 0 then
          begin
            if _recGroup then
              recordGroup ;

            recordAction( pointState.Action, pointState.Pos,
               GisPointsDelta3D( pointState.Buf[pointState.Pos],
                               pointState.Point )
            ) ;
            pointState.Buf[ pointState.Pos ] := doPointChange( pointState.Point,
                                                               TGIS_EditorOperation.Move
                                                             ) ;
          end ;
        end ;
      TGIS_EditorAction.Add    :
        begin
          inc( pointState.Count ) ;
          SetLength( pointState.Buf, pointState.Count ) ;

          if pointState.Count <= 2 then
          begin
            for i := pointState.Count-1 downto 1 do
              pointState.Buf[ i ] := pointState.Buf[ i-1 ] ;
            pointState.Buf[ 1 ] := doPointChange( pointState.Point,
                                                  TGIS_EditorOperation.Add
                                                ) ;
            pointState.Pos := 1 ;
          end else
          begin
            if pointState.Pos = -1 then
            begin
              for i := pointState.Count-1 downto 1 do
                pointState.Buf[ i ] := pointState.Buf[ i-1 ] ;
              pointState.Buf[ 0 ] := doPointChange( pointState.Point,
                                                    TGIS_EditorOperation.Add
                                                  ) ;
              pointState.Pos := 0 ;
            end else
            begin
              for i := pointState.Count-1 downto pointState.Pos + 1 do
                pointState.Buf[ i ] := pointState.Buf[ i-1 ] ;
              pointState.Buf[ pointState.Pos + 1 ] := doPointChange( pointState.Point,
                                                                     TGIS_EditorOperation.Add
                                                                   ) ;
              inc( pointState.Pos ) ;
            end ;
          end ;

         if _recGroup then
          recordGroup ;

          recordAction( pointState.Action, pointState.Pos,
                        pointState.Point ) ;

        end ;
        TGIS_EditorAction.Locate :
          doPointChange( pointState.Point, TGIS_EditorOperation.Locate ) ;

      else
        //begin
        //  assert( False, GIS_RS_ERR_UNTESTED ) ;
        //end;
    end ;

    // do final touch
       RefreshShape ;

  end ;

  procedure TGIS_Editor.undoAction( const _pitem : TObject ) ;
  var
    i      : Integer ;
    action : TGIS_EditorAction ;
    ptno   : Integer ;
    delta  : TGIS_Point3D ;
  begin

    // set action regarding undo/redo marker
      action := T_undoItem(_pitem).Action ;
      ptno  :=  T_undoItem(_pitem).PointPos ;
      delta :=  T_undoItem(_pitem).Delta ;

      if  T_undoItem(_pitem).Undo then
      case action of
        TGIS_EditorAction.Delete : action := TGIS_EditorAction.Add    ;
        TGIS_EditorAction.Move   : action := TGIS_EditorAction.Move   ;
        TGIS_EditorAction.Add    : action := TGIS_EditorAction.Delete ;
      end
    else
      case action of
        TGIS_EditorAction.Delete : action := TGIS_EditorAction.Delete ;
        TGIS_EditorAction.Move   : begin
                                     action := TGIS_EditorAction.Move   ;
                                     delta.X := -delta.X ;
                                     delta.Y := -delta.Y ;
                                   end ;
        TGIS_EditorAction.Add    : action := TGIS_EditorAction.Add    ;
      end ;

    case action of
       TGIS_EditorAction.Delete :
                         begin
                           dec( pointState.Count ) ;
                           for i := ptno to pointState.Count-1 do
                             pointState.Buf[ i ] := pointState.Buf[ i+1 ] ;

                         end ;
       TGIS_EditorAction.Move   :
                         begin
                           if ptno >= 0 then begin
                             pointState.Buf[ ptno ] :=
                               GisMovePoint3D( pointState.Buf[ ptno ], delta ) ;
                           end ;
                         end ;
       TGIS_EditorAction.Add    :
                         begin
                           inc( pointState.Count ) ;
                           SetLength( pointState.Buf, pointState.Count ) ;

                           for i := pointState.Count-1 downto ptno + 1 do
                             pointState.Buf[ i ] := pointState.Buf[ i-1 ] ;
                           pointState.Buf[ ptno ] := delta ;
                         end ;
    end ;
    pointState.Pos := ptno ;

    // be sure that we are in bounds
    if pointState.Pos > pointState.Count-1 then
      pointState.Pos := pointState.Count-1 ;
    if pointState.Pos < 0 then
      pointState.Pos := 0 ;

  end ;

  procedure TGIS_Editor.recordAction( const _action       : TGIS_EditorAction ;
                                      const _point_pos    : Integer           ;
                                      const _delta        : TGIS_Point3D
                                    ) ;
  var
    pitem : T_undoItem ;
  begin
    clearRedo ;

    pitem := T_undoItem.Create ;
    pitem.Undo       := True          ;
    pitem.Action     := _action       ;
    pitem.PointPos   := _point_pos    ;
    pitem.Delta      := _delta        ;

    undoList.Add( pitem ) ;
  end ;

  procedure TGIS_Editor.loadShape  ;
  var
    i      : Integer ;
    newptg : TGIS_Point3D ;
    oldptg : TGIS_Point3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      oldptg := new TGIS_Point3D ;
    {$ENDIF}
    oldptg.X := GIS_MAX_DOUBLE ;
    oldptg.Y := GIS_MAX_DOUBLE ;
    pointState.Count := 0 ;

    TGIS_Shape(FCurrentShape).Lock( TGIS_Lock.Projection );
    try
      SetLength( pointState.Buf, TGIS_Shape(FCurrentShape).GetPartSize(FPart) ) ;

      for i := 0 to TGIS_Shape(FCurrentShape).GetPartSize(FPart) - 1 do begin
        newptg := TGIS_Shape(FCurrentShape).GetPoint3D( FPart, i ) ;
        if GisIsSamePoint3D( oldptg, newptg ) then continue ;
        pointState.Buf[ pointState.Count ] := newptg ;
        oldptg := _TGIS_Point3D( newptg ) ;
        inc( pointState.Count ) ;
      end ;
      if TGIS_Shape(FCurrentShape) is TGIS_ShapePolygon then
        if pointState.Count > 2 then
          dec( pointState.Count ) ;

       if pointState.Pos > pointState.Count - 1 then
         pointState.Pos := pointState.Count - 1 ;

    finally
      TGIS_Shape(FCurrentShape).Unlock ;
    end ;
  end ;

  function TGIS_Editor.doSnapPoint(
    const _ptg      : TGIS_Point3D
  ) : TGIS_Point3D ;
  var
    found     : Boolean ;
    snap_type : TGIS_EditorSnapResultType ;
  begin
    Result := doSnapPoint( _ptg, found, snap_type ) ;
  end ;

  function TGIS_Editor.doSnapPoint(
    const _ptg      : TGIS_Point3D ;
      var _found    : Boolean ;
      var _snapType : TGIS_EditorSnapResultType
  ) : TGIS_Point3D ;
  var
    dst       : Double     ;
    dst_min   : Double     ;
    proj      : TGIS_Point ;
    mrgn      : Double     ;
    ipart     : Integer    ;
    shp       : TGIS_Shape ;
    vwr       : TGIS_Viewer      ;
    ll        : TGIS_LayerVector ;
    ls        : TGIS_LayerVector ;
    {$IFDEF OXYGENE}
      args    : TGIS_EditorSnapPointEventArgs ;
    {$ENDIF}
    si        : Integer ;
    res       : TGIS_Point3D ;
    found     : Boolean ;

    function get_snap_point_intersect( const _ptg   : TGIS_Point3D ;
                                       const _prec  : Double ;
                                         var _dist  : Double ;
                                         var _found : Boolean
                                     ) : TGIS_Point3D ;
    var
      {$IFDEF DCC}
      tmp   : TGIS_Shape ;
      {$ENDIF}
      plist : TGIS_PointList ;
      p     : Integer ;
      min   : Double ;
      dtmp  : Double ;
      lss   : TGIS_LayerVector ;
      sis   : Integer ;
    begin
      Result := _ptg ;
      min    := Abs(_prec);
      _dist  := GIS_MAX_DOUBLE ;
      _found := False ;

      for sis := 0 to FSnapLayers.Count-1 do begin
        lss := TGIS_LayerVector( FSnapLayers[sis] ) ;
        if lss = nil then  continue ;
        // TODO cut extent to visible
        for tmp in lss.Loop( shp.Extent, '', shp, RELATE_INTERSECT, True ) do begin
          if ( lss = Layer ) and ( TGIS_Shape(tmp).Uid = shp.Uid ) then continue ;

          plist := shp.GetCrossings( TGIS_Shape(tmp) ) ;
          try
            if assigned( plist ) then begin
              for p := 0 to plist.Count-1 do begin
                dtmp := GisPoint2Point( GisPoint2DFrom3D(_ptg), plist[p] ) ;
                if dtmp < min then begin
                  min := dtmp ;
                  Result := GisPoint3DFrom2D( plist[p] ) ;
                  _found := True ;
                  _snapType := TGIS_EditorSnapResultType.Intersection ;
                end ;
              end ;
            end ;
          finally
            FreeObject( plist ) ;
          end ;
        end ;
        _dist := min ;
      end ;
    end ;

    function get_snap_point( const _ptg   : TGIS_Point3D ;
                             const _prec  : Double ;
                               var _dist  : Double ;
                               var _found : Boolean
                           ) : TGIS_Point3D ;
    var
      i,j : Integer ;
      tmp : Double ;
      min : Double ;
    begin
      Result := _ptg ;
      min    := Abs(_prec);
      _dist  := GIS_MAX_DOUBLE ;
      _found := False ;

      for i := 0 to shp.GetNumParts-1 do begin
        for j := 0 to shp.GetPartSize(i)-1 do begin
          tmp := GisPoint2Point( GisPoint2DFrom3D(_ptg), shp.GetPoint(i, j)) ;
          if tmp < min then begin
            min := tmp ;
            Result := shp.GetPoint3D( i, j ) ;
            _found := True ;
            _snapType := TGIS_EditorSnapResultType.Vertex ;
          end ;
        end ;
      end ;
      _dist := min ;
    end ;

    function get_snap_point_grid ( const _ptg   : TGIS_Point3D ;
                                   const _prec  : Double ;
                                     var _dist  : Double ;
                                     var _found : Boolean
                                 ) : TGIS_Point3D ;
    begin
      Result := _ptg ;
      _found := True ;
      _snapType := TGIS_EditorSnapResultType.Vertex ;

      // Floor(f / s + 0.5 ) * s
      Result.X := FloorS(_ptg.X / FSnapGridSpacing + 0.5 ) * FSnapGridSpacing ;
      Result.Y := FloorS(_ptg.Y / FSnapGridSpacing + 0.5 ) * FSnapGridSpacing ;
      Result.Z := _ptg.Z ;
      Result.M := _ptg.M ;

      _dist := GisPoint2Point( GisPoint2DFrom3D(_ptg), GisPoint2DFrom3D(Result) ) ;
    end ;

    function get_snap_end_point( const _ptg   : TGIS_Point3D ;
                                 const _prec  : Double ;
                                   var _dist  : Double ;
                                   var _found : Boolean
                               ) : TGIS_Point3D ;
    var
      i,j : Integer ;
      tmp : Double ;
      min : Double ;
      np  : Integer ;
    begin
      Result := _ptg ;
      min    := Abs(_prec);
      _dist  := GIS_MAX_DOUBLE ;
      _found := False ;

      for i := 0 to shp.GetNumParts-1 do begin
        np := shp.GetPartSize(i)-1 ;
        for j := 0 to np do begin
          if ( j <> 0 ) and ( j <> np ) then continue ;

          tmp := GisPoint2Point( GisPoint2DFrom3D(_ptg), shp.GetPoint(i, j)) ;
          if tmp < min then begin
            min := tmp ;
            Result := shp.GetPoint3D( i, j ) ;
            _found := True ;
            _snapType := TGIS_EditorSnapResultType.Vertex ;
          end ;
        end ;
      end ;
      _dist := min ;
    end ;

    function get_snap_point_line( const _ptg    : TGIS_Point3D ;
                                  const _prec   : Double ;
                                    var _dist   : Double ;
                                    var _found  : Boolean
                                ) : TGIS_Point3D ;
    var
      i,j    : Integer    ;
      tmp    : Double     ;
      dmin   : Double     ;
      pt     : TGIS_Point3D ;
    begin
      Result := _ptg ;

      _dist  := GIS_MAX_DOUBLE ;
      _found := False ;

      if      shp is TGIS_ShapePoint then
              Result := get_snap_point( _ptg, _prec, _dist, _found )
      else if shp is TGIS_ShapeMultiPoint then
              Result := get_snap_point( _ptg, _prec, _dist, _found )
      else begin
        dmin := GIS_MAX_DOUBLE ;
        for i := 0 to shp.GetNumParts - 1 do begin
          for j := 0 to shp.GetPartSize( i ) - 2 do begin
            pt  := GisPointOnLine3D( shp.GetPoint3D( i, j   ),
                                     shp.GetPoint3D( i, j+1 ),
                                     _ptg
                                    ) ;
            tmp := GisPoint2Point( GisPoint2DFrom3D( _ptg ),
                                   GisPoint2DFrom3D( pt ) )  ;
            if tmp < dmin then begin
              dmin   := tmp ;
              _dist  := tmp ;
              if tmp < _prec then begin
                Result := pt  ;
                _found := True ;
                _snapType := TGIS_EditorSnapResultType.Edge ;
              end ;
            end ;
          end ;
        end ;
      end ;
    end ;

    function get_snap_point_line_midpoint ( const _ptg    : TGIS_Point3D ;
                                            const _prec   : Double ;
                                              var _dist   : Double ;
                                              var _found  : Boolean
                                          ) : TGIS_Point3D ;
    var
      i,j    : Integer    ;
      tmp    : Double     ;
      dmin   : Double     ;
      pt     : TGIS_Point3D ;
      p1     : TGIS_Point3D ;
      p2     : TGIS_Point3D ;
    begin
      Result := _ptg ;

      _dist  := GIS_MAX_DOUBLE ;
      _found := False ;

      if      shp is TGIS_ShapePoint then
              Result := get_snap_point( _ptg, _prec, _dist, _found )
      else if shp is TGIS_ShapeMultiPoint then
              Result := get_snap_point( _ptg, _prec, _dist, _found )
      else begin
        dmin := GIS_MAX_DOUBLE ;
        for i := 0 to shp.GetNumParts - 1 do begin
          for j := 0 to shp.GetPartSize( i ) - 2 do begin
            p1 := shp.GetPoint3D( i, j ) ;
            p2 := shp.GetPoint3D( i, j+1 ) ;
            pt := GisPoint3D( (p1.X + p2.X)/2, (p1.Y + p2.Y)/2, p1.Z, p1.M ) ;

            tmp := GisPoint2Point( GisPoint2DFrom3D( _ptg ),
                                   GisPoint2DFrom3D( pt ) )  ;
            if tmp < dmin then begin
              dmin   := tmp ;
              _dist  := tmp ;
              if tmp < _prec then begin
                Result := pt  ;
                _found := True ;
                _snapType := TGIS_EditorSnapResultType.Midpoint ;
              end ;
            end ;
          end ;
        end ;
      end ;
    end ;

    function find_point_perpendicular(
      const _a, _b, _c : TGIS_Point3D
    ) : TGIS_Point3D ;
    var
      x1, y1, x2, y2, x3, y3 : Double ;
      px, py, u, d12 : Double ;
    begin
      x1 := _a.X ;
      y1 := _a.Y ;
      x2 := _b.X ;
      y2 := _b.Y ;
      x3 := _c.X ;
      y3 := _c.Y ;
      px := x2 - x1 ;
      py := y2 - y1 ;
      d12 := px*px + py*py ;
      if d12 <> 0 then
        u := ((x3 - x1) * px + (y3 - y1) * py) / d12
      else
        u := 0;
      Result := GisPoint3D( x1 + u * px, y1 + u * py, _c.Z, _c.M ) ;
    end ;

    function get_snap_point_line_perpendicular(
      const _ptg    : TGIS_Point3D ;
      const _ref    : TGIS_Point3D ;
      const _prec   : Double ;
        var _dist   : Double ;
        var _found  : Boolean
    ) : TGIS_Point3D ;
    var
      i,j    : Integer    ;
      tmp    : Double     ;
      dmin   : Double     ;
      pt     : TGIS_Point3D ;
      p1     : TGIS_Point3D ;
      p2     : TGIS_Point3D ;
    begin
      Result := _ptg ;

      _dist  := GIS_MAX_DOUBLE ;
      _found := False ;

      if      shp is TGIS_ShapePoint then
              Result := get_snap_point( _ptg, _prec, _dist, _found )
      else if shp is TGIS_ShapeMultiPoint then
              Result := get_snap_point( _ptg, _prec, _dist, _found )
      else begin
        dmin := GIS_MAX_DOUBLE ;
        for i := 0 to shp.GetNumParts - 1 do begin
          for j := 0 to shp.GetPartSize( i ) - 2 do begin
            p1 := shp.GetPoint3D( i, j ) ;
            p2 := shp.GetPoint3D( i, j+1 ) ;
            pt := find_point_perpendicular( p1, p2, _ref ) ;

            if (pt.X < Min(p1.X, p2.X)) or (pt.X > Max(p1.X, p2.X)) then continue ;
            if (pt.Y < Min(p1.Y, p2.Y)) or (pt.Y > Max(p1.Y, p2.Y)) then continue ;

            tmp := GisPoint2Point( GisPoint2DFrom3D( _ptg ),
                                   GisPoint2DFrom3D( pt ) )  ;
            if tmp < dmin then begin
              dmin   := tmp ;
              _dist  := tmp ;
           //   if tmp < _prec then begin
                Result := pt  ;
                _found := True ;
                _snapType := TGIS_EditorSnapResultType.Intersection ;
           //   end ;
            end ;
          end ;
        end ;
      end ;
    end ;

  begin
    vwr := TGIS_Viewer( FViewer ) ;
    ll  := TGIS_LayerVector( Layer ) ;

    mrgn    := vwr.TwipsToPixels( FSnapMargin ) / vwr.Zoom ;
    Result  := _ptg ;
    shp     := nil ;
    dst_min := GIS_MAX_DOUBLE ;
    dst     := GIS_MAX_DOUBLE ;
    found   := False ;
    _found  := False ;

    if ( FSnapType = TGIS_EditorSnapType.GridPoint ) then begin
      if FSnapGridSpacing > 0 then
        Result := get_snap_point_grid( _ptg, mrgn, dst, found )
      else
        Result := _ptg ;

      _found := found ;
      exit ;
    end ;

    if ( not FUseSnapLayers ) then exit ;

    for si := 0 to FSnapLayers.Count-1 do begin
      ls  := TGIS_LayerVector( FSnapLayers[si] ) ;

      if ls <> nil then begin
        // SnapLayer can be the same layer which own editShape
        if ls = Layer then
          shp := ll.LocateEx(
                   GisPoint2DFrom3D( _ptg ), mrgn, -1,
                   dst, ipart, proj
                 )
        else
          shp := ls.LocateEx(
                   GisPoint2DFrom3D( _ptg ), mrgn, -1,
                   dst, ipart, proj
                 ) ;
        if shp <> nil then begin
          case FSnapType of
            TGIS_EditorSnapType.Point  :
              begin
                res := get_snap_point( _ptg, mrgn, dst, found ) ;
                if FSnapToIntersection and not found then
                  res := get_snap_point_intersect( _ptg, mrgn, dst, found ) ;
              end ;
            TGIS_EditorSnapType.Line   :
              res := get_snap_point_line( _ptg, mrgn, dst, found ) ;
            TGIS_EditorSnapType.PointOverLine :
              begin
                res := get_snap_point( _ptg, mrgn, dst, found ) ;
                if FSnapToIntersection and not found then
                  res := get_snap_point_intersect( _ptg, mrgn, dst, found ) ;
                if not found then
                  res := get_snap_point_line( _ptg, mrgn, dst, found ) ;
              end ;
            TGIS_EditorSnapType.Custom :
              begin
                if assigned( FOnSnapPoint ) then
                  {$IFDEF OXYGENE}
                    begin
                      args := TGIS_EditorSnapPointEventArgs.Create(
                                _ptg, mrgn, Result, dst
                              ) ;
                      FOnSnapPoint( Self, args ) ;
                      Result := args.Proj ;
                    end
                  {$ELSE}
                    FOnSnapPoint( Self, _ptg, mrgn, Result, dst )
                  {$ENDIF}
                else
                  Result := _ptg ;
            end ;
            TGIS_EditorSnapType.EndPoint  :
                res := get_snap_end_point( _ptg, mrgn, dst, found ) ;
            TGIS_EditorSnapType.Midpoint   :
              res := get_snap_point_line_midpoint( _ptg, mrgn, dst, found ) ;
            TGIS_EditorSnapType.Perpendicular   :
              if ( pointState.Count > 0 ) then begin
                if pointDrag then // move
                  res := get_snap_point_line_perpendicular(
                            _ptg, pointState.Buf[Max(0, pointState.Pos-1)], mrgn, dst, found
                         )
                else
                  res := get_snap_point_line_perpendicular(
                            _ptg, pointState.Buf[pointState.Count-1], mrgn, dst, found
                         )
              end
              else begin
                res := get_snap_point( _ptg, mrgn, dst, found ) ;
                if FSnapToIntersection and not found then
                  res := get_snap_point_intersect( _ptg, mrgn, dst, found ) ;
                if not found then
                  res := get_snap_point_line( _ptg, mrgn, dst, found ) ;
              end ;
          end ;

          if FSnapType <> TGIS_EditorSnapType.Custom then begin
            if dst < dst_min then begin
              dst_min := dst ;
              Result  := res ;
              _found := found ;
            end ;
          end ;
        end ;
      end ;
    end ;
  end ;

  function TGIS_Editor.doPointChange(
    const _ptg        : TGIS_Point3D ;
    const _operation  : TGIS_EditorOperation
  ) : TGIS_Point3D ;
  {$IFDEF OXYGENE}
  var
    args    : TGIS_EditorPointChangeEventArgs ;
  {$ENDIF}
  begin
    Result := _ptg ;

    if assigned( FOnPointChange ) then
      {$IFDEF OXYGENE}
        begin
          args := TGIS_EditorPointChangeEventArgs.Create(
                    _ptg, _operation, Result
                  ) ;
          FOnPointChange( Self, args ) ;
          Result := args.Proj ;
        end
      {$ELSE}
        FOnPointChange( Self, _ptg, _operation, Result ) ;
      {$ENDIF}
  end ;

  procedure TGIS_Editor.doPointEditing(
    const _ptg        : TGIS_Point3D ;
    const _pt         : TPoint
  ) ;
  {$IFDEF OXYGENE}
  var
    args    : TGIS_EditorPointMoveEventArgs ;
  {$ENDIF}
  begin
    if assigned( FOnPointEditing ) then
      {$IFDEF OXYGENE}
        begin
          args := TGIS_EditorPointMoveEventArgs.Create(
                    _ptg, _pt
                  ) ;
          FOnPointEditing( Self, args ) ;
        end
      {$ELSE}
        FOnPointEditing( Self, _ptg, _pt ) ;
      {$ENDIF}
  end ;

  procedure TGIS_Editor.invalidateInternal ;
  var
    bfull : Boolean ;
    vwr   : TGIS_Viewer ;
    {$IFDEF DCC}
      la  : TGIS_LayerAbstract ;
    {$ENDIF}
  begin
    vwr := TGIS_Viewer(FViewer) ;

    bfull := True ;

    if bTopmostShape then begin
      bfull := False ;

      if ShowTracking then begin
        for la in  FSnapLayers do begin
          if not TGIS_Layer( la ).IsTopmost then begin
            bfull := True  ;
          end;
          break ;
        end;
      end;
    end;

    vwr.InvalidateEditor( bfull ) ;
  end;

  procedure TGIS_Editor.editShapeInternal(
    const _shp  : TObject ;
    const _part : Integer
  ) ;
  var
    vwr : TGIS_Viewer ;
  begin
    vwr := TGIS_Viewer(FViewer) ;
    if _shp = nil then exit ;

    if assigned( TGIS_Shape(_shp).Layer ) then
      if not TGIS_Shape(_shp).Layer.IsSupported( TGIS_OperationType.Edit ) then
        exit ;

    clearUndo ;
    clearRedo ;

    EndEdit ;

    // default snap layer is the layer which hold the shape
    // can be changed later
    if FSnapLayers.IndexOf( TGIS_Shape(_shp).Layer ) = -1 then begin
      oDynamicSnapLayer := TGIS_Shape(_shp).Layer ;
      FSnapLayers.Insert( 0, TGIS_Shape(_shp).Layer ) ;
      FUseSnapLayers := True ;
    end
    else
      oDynamicSnapLayer := nil ;

    bTopmostShape :=  TGIS_Shape( _shp ).Layer.IsTopmost ;

    FPart  := _part ;

    FCurrentShape := TGIS_Shape(_shp).MakeEditable ;

    loadShape ;

    MustRedraw := True ;
    vwr.InvalidateEditor( False ) ;

    oldEditPoint.X := NaN ;

    vwr.Parent.ControlRaiseEditorChangeEvent( Self ) ;

    // set position to the last point
    if FMode = TGIS_EditorMode.AfterActivePoint then
      pointState.Pos := pointState.Count - 1 ;
  end ;

  procedure TGIS_Editor.createShapeInternal(
    const _layer : TGIS_LayerAbstract ;
    const _ptg   : TGIS_Point3D ;
    const _type  : TGIS_ShapeType ;
    const _dim   : TGIS_DimensionType
  ) ;
  var
    shp : TGIS_Shape ;
    ptg : TGIS_Point3D ;
  begin
    if not assigned( _layer ) then exit ;
    if not ( TGIS_Layer(_layer) is TGIS_LayerVector ) then exit ;

    shp := TGIS_LayerVector( _layer ).CreateShape( _type, _dim ) ;

    if not assigned( shp ) then exit ;

    ptg := doSnapPoint( _ptg ) ;

    // basic creation
    shp.Lock( TGIS_Lock.Extent ) ;
      shp.AddPart ;
      shp.AddPoint3D( ptg )  ;
    shp.Unlock ;
    EditShape( shp, 0 ) ;

    // and now snap first point
    pointState.Buf[ pointState.Pos ] :=
      TGIS_Shape(FCurrentShape).Layer.Unproject3D( doPointChange( ptg, TGIS_EditorOperation.Add ) ) ;

    RefreshShape ;
  end ;

  procedure TGIS_Editor.endEditInternal ;
  var
    tpl  : TGIS_Topology ;
    vwr  : TGIS_Viewer   ;
    res  : Boolean       ;
  begin
    RemoveSnapLayer( oDynamicSnapLayer ) ;

    if assigned( FCurrentShape ) and
      ( TGIS_Shape( FCurrentShape ).ShapeType = TGIS_ShapeType.Polygon ) and
      ( TGIS_Shape( FCurrentShape ).Dimension = TGIS_DimensionType.XY ) then
    begin
      tpl := TGIS_Topology.Create ;
      try
        tpl.FixShapeEx( TGIS_Shape( FCurrentShape ), False, True, res ) ;
      finally
        FreeObject( tpl ) ;
      end ;
    end ;

    vwr := TGIS_Viewer(FViewer) ;

    FCurrentShape := nil ;
    initialState ;

    MustRedraw := False ;

    invalidateInternal ;

    vwr.Parent.ControlRaiseEditorChangeEvent( Self ) ;
  end ;

  procedure TGIS_Editor.deleteShapeInternal ;
  begin
    if not InEdit then exit ;

    TGIS_Shape(FCurrentShape).Delete ;
    EndEdit ;
  end ;

  procedure TGIS_Editor.revertShapeInternal ;
  var
    tmp_uid   : TGIS_Uid    ;
    tmp_part  : Integer    ;
    tmp_layer : TGIS_LayerVector ;
    shp       : TGIS_Shape ;
  begin
    if not InEdit then exit ;

    tmp_uid   := Uid ;
    tmp_part  := Part ;
    tmp_layer := TGIS_LayerVector( Layer ) ;
    EndEdit ;

    TGIS_LayerVector(tmp_layer).Revert( tmp_uid ) ;

    shp := TGIS_LayerVector(tmp_layer).GetShape( tmp_uid ) ;
    if not assigned( shp ) then exit ;
    tmp_part := Min( tmp_part, shp.GetNumParts - 1 ) ;
    EditShape( shp, tmp_part ) ;
  end ;

  procedure TGIS_Editor.undoInternal ;
  var
    pitem : T_undoItem ;
    act   : TGIS_EditorAction ;
  begin
    if not InEdit then exit ;
    if not CanUndo then exit ;

    act := TGIS_EditorAction.Add ;
    repeat
      if undoList.Count = 0 then break ;

      pitem := T_undoItem(undoList.Last) ;
      undoList.Delete( undoList.Count -1 ) ;

      act := pitem.Action ;

      if act <> TGIS_EditorAction.Group then
         undoAction( pitem ) ;

      pitem.Undo  := False ;
      redoList.Add( pitem ) ;
    until act = TGIS_EditorAction.Group ;

    RefreshShape ;
  end ;

  procedure TGIS_Editor.redoInternal ;
  var
    pitem : T_undoItem ;
    act   : TGIS_EditorAction ;
    count : Integer ;
  begin
    if not InEdit then exit ;
    if not CanRedo then exit ;

    count := 0;
    repeat
      if redoList.Count = 0 then break ;

      pitem := T_undoItem(redoList.Last) ;

      if ( pitem.Action = TGIS_EditorAction.Group ) and ( count > 0 ) then break ;

      redoList.Delete( redoList.Count - 1 ) ;

      act := pitem.Action ;

      if act <> TGIS_EditorAction.Group then
         undoAction( pitem ) ;

      pitem.Undo  := True ;

      undoList.Add( pitem ) ;

      inc( count ) ;
    until False ;

    RefreshShape ;
  end ;

  procedure TGIS_Editor.changeWindingInternal ;
  var
    i   : Integer ;
    x   : Integer ;
    y   : Integer ;
    ptg : TGIS_Point3D ;
  begin
    if not InEdit then exit ;

    RefreshShape ;
    x := 0 ;
    y := pointState.Count-1 ;
    for i := 0 to (pointState.Count-1) div 2 do
    begin
      ptg := pointState.Buf[ x ] ;
      pointState.Buf[ x ] := pointState.Buf[ y ] ;
      pointState.Buf[ y ] := ptg ;
      inc( x ) ;
      dec( y ) ;
    end ;
    RefreshShape ;
  end ;

  procedure TGIS_Editor.createPartInternal(
    const _ptg : TGIS_Point3D
  ) ;
  begin
    if not InEdit then exit ;

    RefreshShape ;
    partState.MustCreate := True ;
    partState.PointToAdd := TGIS_Shape(FCurrentShape).Layer.Unproject3D(
                              doSnapPoint( _ptg )
                            ) ;
    RefreshShape ;
    pointState.Pos := 0 ;
    clearUndo ;
    clearRedo ;
  end ;

  procedure TGIS_Editor.deletePartInternal ;
  begin
    if not InEdit then exit ;

    partState.MustDelete := True ;
    RefreshShape ;
    clearUndo ;
    clearRedo ;
  end ;

  procedure TGIS_Editor.mouseBeginInternal(
    const _pt      : TPoint  ;
    const _nearest : Boolean
  ) ;
  var
    i       : Integer     ;
    ptg     : TGIS_Point3D ;
    tlrnc   : Integer     ;
    min     : Double      ;
    min2    : Double      ;
    tmp     : Double      ;
    ptpos   : Integer     ;
    ptpos2  : Integer     ;
    nearest : Boolean     ;
    vwr     : TGIS_Viewer ;
    ll      : TGIS_Layer  ;
    shp     : TGIS_Shape  ;
  begin
    if not InEdit then exit ;


    vwr := TGIS_Viewer( FViewer ) ;
    shp := TGIS_Shape(FCurrentShape) ;
    ll  := shp.Layer ;

    if shp is TGIS_ShapeComplex then exit ;

    case FMode of
      TGIS_EditorMode.Default          : nearest := _nearest     ;
      TGIS_EditorMode.Reversed         : nearest := not _nearest ;
      TGIS_EditorMode.NearestPoint     : nearest := True         ;
      TGIS_EditorMode.AfterActivePoint : nearest := False        ;
      else                               begin
                                           nearest := _nearest   ;
                                           assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
                                         end ;
    end;

    pointState.MouseStart := ConvertPoint( _pt ) ;
    pointState.WasMouseBegin := True ;
    // prepare temp data
      ptg := vwr.ScreenToMap3D( _pt ) ;
      if assigned( ll ) then
        ptg := ll.Unproject3D( ptg ) ;

      case PointerMode of
        TGIS_PointerMode.Pen   : tlrnc := SelectTolerancePen ;
        TGIS_PointerMode.Touch : tlrnc := SelectToleranceTouch ;
        else                     tlrnc := SelectTolerance ;
      end ;

      min := Max( 5, vwr.TwipsToPixels(tlrnc) ) / vwr.Zoom ;

      if assigned( SnapLayer ) and ( SnapLayer = ll )
      then
        min2 := Max( min,
                     vwr.TwipsToPixels(FSnapMargin) / vwr.Zoom
                   )
      else
        min2 := 0 ;

    // is it an existing point /or close enough/?
      ptpos  := -1 ;
      ptpos2 := -1 ;
      for i := 0 to pointState.Count-1 do
      begin
        if not assigned( ll ) then
          tmp := GisPoint2Point( GisPoint2DFrom3D( ptg ),
                                 GisPoint2DFrom3D( pointState.Buf[ i ] )
                                )
        else
          tmp := GisPoint2Point(
                   vwr.CS.FromCS( ll.CS,
                                  GisPoint2DFrom3D( ptg )
                                ),
                   vwr.CS.FromCS( ll.CS,
                                  GisPoint2DFrom3D( pointState.Buf[ i ] )
                                )
                 ) ;

        if tmp < min then
        begin
          min   := tmp ;
          ptpos := i   ;
        end ;
        if tmp < min2 then
        begin
          min2   := tmp ;
          ptpos2 := i   ;
        end ;
      end ;

      pointDrag := False ;

    // if yes, then if it can be moved or deleted
      if ptpos >= 0 then
      begin
        pointDrag := True ;
        pointState.MustDeleteOrMove := True ;
        if pointState.Pos = ptpos then
          // point already selected? - delete it
          pointState.Action := TGIS_EditorAction.Delete
        else
        begin
          // only do selection
          pointState.Action := TGIS_EditorAction.Locate ; // we are only locating
          pointState.Pos := ptpos ;
        end ;
        exit ;
      end
      else if ptpos2 >= 0 then begin
        // do nothing - snapping area
        pointState.Action := TGIS_EditorAction.Locate
      end
      else
      begin
        pointState.MustDeleteOrMove := False  ;
        pointState.Action := TGIS_EditorAction.Add ;

        if ( not nearest ) then
        begin
          if ( pointState.Pos = 0 ) and ( not ( shp is TGIS_ShapePolygon ) )
          then
            pointState.Pos := -1 ;
          exit ;
        end ;

        // but we mut know which position - so that we will find
        // the closest one
          min := 1e305 ;
          pointState.Pos := 0 ;
          for i := 0 to pointState.Count-2 do
          begin
            tmp := GisLine2PointFuzzy(
                     GisPoint2DFrom3D(pointState.Buf[i]),
                     GisPoint2DFrom3D(pointState.Buf[i+1]),
                     GisPoint2DFrom3D(ptg)
                   ) ;
            if tmp < min then
            begin
              min := tmp ;
              pointState.Pos := i ;
            end ;
          end ;
          if shp is TGIS_ShapePolygon then
          begin
            tmp := GisLine2PointFuzzy(
                     GisPoint2DFrom3D(pointState.Buf[pointState.Count-1]),
                     GisPoint2DFrom3D(pointState.Buf[0]),
                     GisPoint2DFrom3D(ptg)
                   ) ;
            if tmp < min then
            begin
              pointState.Pos := pointState.Count-1 ;
            end ;
          end
          else
          begin
            tmp := GisPoint2Point(
                     GisPoint2DFrom3D( pointState.Buf[pointState.Count-1] ),
                     GisPoint2DFrom3D( ptg )
                   ) ;

            if tmp <= min then
            begin
              min := tmp ;
              pointState.Pos := pointState.Count-1 ;
            end ;

            tmp := GisPoint2Point(
                     GisPoint2DFrom3D( pointState.Buf[0] ),
                     GisPoint2DFrom3D( ptg )
                   ) ;

            if tmp <= min then
            begin
              pointState.Pos := -1 ;
            end ;
          end ;
      end ;
  end ;

  procedure TGIS_Editor.mouseMoveInternal(
     const _pt : TPoint
  ) ;

  begin
    if not InEdit then exit ;

    if not pointState.WasMouseBegin then exit ;

    // ignore mouse move event w/o real mouse move actions
    if ( pointState.MouseStart.X = _pt.X ) and
       ( pointState.MouseStart.Y = _pt.Y ) then
       exit ;

    doPointEditing( pointState.Point, _pt ) ;

    // point exists, so we will move it
      if pointState.MustDeleteOrMove then
        pointState.Action := TGIS_EditorAction.Move ;
  end ;

  procedure TGIS_Editor.mouseEndInternal(
    const _pt : TPoint
  ) ;
  var
    ptg       : TGIS_Point3D ;
    ptg2      : TGIS_Point3D ;
    tz, tm    : Double ;
    new_point : TGIS_Point3D ;
    dx, dy    : Double     ;
    vwr       : TGIS_Viewer  ;
  begin
    if not InEdit then exit ;

    if not pointState.WasMouseBegin then exit ;
    pointState.WasMouseBegin := False ;

    vwr := TGIS_Viewer( FViewer ) ;

    // ignore small movements
    if pointState.Action = TGIS_EditorAction.Move then begin
      if Sqrt( Sqr( pointState.MouseStart.X - _pt.X  ) +
               Sqr( pointState.MouseStart.Y - _pt.Y  )
             ) < vwr.TwipsToPixels( FMinMove )
       then exit ;
    end ;

    // ignore the addition of a point to a TGIS_ShapeType.Point shape
    if ( TGIS_Shape(CurrentShape).ShapeType = TGIS_ShapeType.Point ) and
       ( pointState.Action = TGIS_EditorAction.Add )
    then exit ;

    // do shape editing
    ptg := doSnapPoint( vwr.ScreenToMap3D(_pt) ) ;
    pointDrag := False ;
    if ( pointState.Pos >= 0 ) and ( pointState.Pos < pointState.Count ) then
      ptg2 := pointState.Buf[ pointState.Pos ]
    else
      ptg2 := ptg ;

    tz := ptg2.Z ;
    tm := ptg2.M ;
    if assigned( TGIS_Shape(FCurrentShape).Layer ) then
      pointState.Point := TGIS_Shape(FCurrentShape).Layer.Unproject3D( ptg )
    else
      pointState.Point := ptg ;

    pointState.Point.Z := tz ;
    pointState.Point.M := tm ;

    // do not add a new point
    if EditorMode <> TGIS_EditorModeEx.Normal then exit ;

    doAction ;

    // Autocenter code
    new_point := vwr.ScreenToMap3D( _pt ) ;

    if not IsNan( oldEditPoint.X ) then begin
      dx := - ( oldEditPoint.X - new_point.X ) ;
      dy :=   ( oldEditPoint.Y - new_point.Y ) ;
      vwr.Parent.ControlAutoCenterViewport( dx,dy  ) ;
    end ;
    oldEditPoint := new_point ;

  end ;

  procedure TGIS_Editor.addPointInternal(
    const _ptg     : TGIS_Point3D ;
    const _group   : Boolean
  ) ;
  begin
    if not InEdit then exit ;

    pointState.Action := TGIS_EditorAction.Add ;
    pointState.Pos    := pointState.Count - 1    ;
    pointState.Point  := TGIS_Shape(FCurrentShape).Layer.Unproject3D( _ptg ) ;
    doAction( _group ) ;
  end ;

  procedure TGIS_Editor.insertPointInternal(
    const _pos : Integer;
    const _ptg : TGIS_Point3D
  ) ;
  begin
    if not InEdit then exit ;

    pointState.Action := TGIS_EditorAction.Add ;
    pointState.Pos    := _pos - 1     ;
    pointState.Point  := TGIS_Shape(FCurrentShape).Layer.Unproject3D( _ptg ) ;
    doAction ;
  end ;

  procedure TGIS_Editor.movePointInternal(
    const _pos : Integer      ;
    const _ptg : TGIS_Point3D
  );
  begin
    if not InEdit then exit ;

    pointState.Action := TGIS_EditorAction.Move ;
    pointState.Pos    := _pos - 1      ;
    pointState.Point  := TGIS_Shape(FCurrentShape).Layer.Unproject3D( _ptg ) ;
    doAction ;
  end ;

  procedure TGIS_Editor.deletePointInternal(
    const _pos : Integer
  ) ;
  begin
    if not InEdit then exit ;

    pointState.Action := TGIS_EditorAction.Delete ;
    pointState.Pos     := _pos - 1        ;
    doAction ;
  end ;

  procedure TGIS_Editor.recordGroup ;
  begin
    recordAction( TGIS_EditorAction.Group, 0, GisPoint3D( 0, 0, 0 ) ) ;
  end ;

//----------------------------------------------------------------------------
// property access routines from IGIS_Editor
//----------------------------------------------------------------------------

  function TGIS_Editor.fget_CurrentShape
    : TGIS_ShapeAbstract ;
  begin
    Result := FCurrentShape ;
  end ;

  function TGIS_Editor.fget_EditingLinesStyle
    : TGIS_EditorStyle ;
  begin
    Result := FEditingLinesStyle ;
  end ;

  procedure TGIS_Editor.fset_EditingLinesStyle(
    const _value : TGIS_EditorStyle
  ) ;
  begin
    FEditingLinesStyle := _value ;
  end ;

  function TGIS_Editor.fget_EditingPointsStyle
    : TGIS_EditorEditingPointsStyle ;
  begin
    Result := FEditingPointsStyle ;
  end ;

  procedure TGIS_Editor.fset_EditingPointsStyle(
    const _value : TGIS_EditorEditingPointsStyle
  ) ;
  begin
    FEditingPointsStyle := _value ;
  end ;

  function TGIS_Editor.fget_EdgeLengthsStyle
    : TGIS_EditorEdgeLengthsStyle ;
  begin
    Result := FEditingEdgeLengthsStyle ;
  end ;

  procedure TGIS_Editor.fset_EdgeLengthsStyle(
    const _value : TGIS_EditorEdgeLengthsStyle
  ) ;
  begin
    FEditingEdgeLengthsStyle := _value ;
  end ;

  function TGIS_Editor.fget_EditorMode
    : TGIS_EditorModeEx ;
  begin
    Result := FEditorMode ;
  end ;

  procedure TGIS_Editor.fset_EditorMode(
    const _value : TGIS_EditorModeEx
  ) ;
  begin
    FEditorMode := _value ;
  end ;

  function TGIS_Editor.fget_InEdit
    : Boolean ;
  begin
    Result := assigned( FCurrentShape ) ;
  end ;

  procedure TGIS_Editor.fset_MustRedraw(
    const _value : Boolean
  ) ;
  begin
    FMustRedraw := _value ;
  end ;

  function TGIS_Editor.fget_MustRedraw
    : Boolean ;
  begin
    Result := FMustRedraw ;
  end ;

  function TGIS_Editor.fget_Layer
    : TGIS_LayerAbstract ;
  begin
    if InEdit then
      Result := TGIS_Shape(FCurrentShape).Layer
    else
      Result := nil ;
  end ;

  function TGIS_Editor.fget_MinMove
    : Integer ;
  begin
    Result := FMinMove ;
  end ;

  procedure TGIS_Editor.fset_MinMove(
    const _value : Integer
  ) ;
  begin
    FMinMove := _value ;
  end ;

  function TGIS_Editor.fget_Mode
    : TGIS_EditorMode ;
  begin
    Result := FMode ;
  end ;

  procedure TGIS_Editor.fset_Mode(
    const _value : TGIS_EditorMode
  ) ;
  begin
    FMode := _value ;
  end ;

  function TGIS_Editor.fget_Part
    : Integer ;
  begin
    Result := FPart ;
  end ;

  function TGIS_Editor.fget_Point(
    const _pos : Integer
  ) : TGIS_Point3D ;
  begin
    if _pos < 0 then Result := pointState.Buf[ 0 ]
                else Result := pointState.Buf[ _pos ] ;
  end ;

  procedure TGIS_Editor.fset_Point(
    const _pos   : Integer ;
    const _value : TGIS_Point3D
  ) ;
  begin
    pointState.Buf[ _pos ] := _value ;
  end ;

  function TGIS_Editor.fget_PointCount
    : Integer ;
  begin
    Result := pointState.Count ;
  end ;

  function TGIS_Editor.fget_PointPos
    : Integer ;
  begin
    Result := pointState.Pos ;
  end ;

  procedure TGIS_Editor.fset_PointPos(
    const _value : Integer
  ) ;
  begin
    pointState.Pos := _value ;
  end ;

  function TGIS_Editor.fget_PointerMode
    : TGIS_PointerMode ;
  begin
    result := FPointerMode ;
  end ;

  procedure TGIS_Editor.fset_PointerMode(
    const _value : TGIS_PointerMode
  ) ;
  begin
    FPointerMode := _value ;
  end ;

  function TGIS_Editor.fget_RedoState
    : Boolean ;
  begin
    Result := redoList.Count > 0 ;
  end ;

  function TGIS_Editor.fget_SelectTolerance
    : Integer ;
  begin
    Result := FSelectTolerance ;
  end ;

  procedure TGIS_Editor.fset_SelectTolerance(
    const _value : Integer
  ) ;
  begin
    FSelectTolerance := _value ;
  end ;

  function TGIS_Editor.fget_SelectTolerancePen
    : Integer ;
  begin
    result := FSelectTolerancePen ;
  end ;

  procedure TGIS_Editor.fset_SelectTolerancePen(
    const _value : Integer
  ) ;
  begin
    FSelectTolerancePen := _value ;
  end ;

  function TGIS_Editor.fget_SelectToleranceTouch
    : Integer ;
  begin
    result := FSelectToleranceTouch ;
  end ;

  procedure TGIS_Editor.fset_SelectToleranceTouch(
    const _value : Integer
  ) ;
  begin
    FSelectToleranceTouch := _value ;
  end ;

  function TGIS_Editor.fget_ShowDraggingTrack
    : Boolean ;
  begin
    Result := FShowDraggingTrack ;
  end ;

  procedure TGIS_Editor.fset_ShowDraggingTrack(
    const _value : Boolean
  ) ;
  begin
    FShowDraggingTrack := _value ;
  end ;

  function TGIS_Editor.fget_ShowPoints3D
    : Boolean ;
  begin
    Result := FShowPoints3D ;
  end ;

  procedure TGIS_Editor.fset_ShowPoints3D(
    const _value : Boolean
  ) ;
  begin
    FShowPoints3D := _value ;
  end ;

  function TGIS_Editor.fget_ShowPointsNumbers
    : Boolean ;
  begin
    Result := FShowPointsNumbers ;
  end ;

  procedure TGIS_Editor.fset_ShowPointsNumbers(
    const _value : Boolean
  ) ;
  begin
    FShowPointsNumbers := _value ;
  end ;

  function TGIS_Editor.fget_ShowEdgesLengths
    : Boolean ;
  begin
    Result := FShowEdgesLengths ;
  end ;

  procedure TGIS_Editor.fset_ShowEdgesLengths(
    const _value : Boolean
  ) ;
  begin
    FShowEdgesLengths := _value ;
  end ;

  function TGIS_Editor.fget_ShowTracking
    : Boolean ;
  begin
    Result := FShowTracking ;
  end ;

  procedure TGIS_Editor.fset_ShowTracking(
    const _value : Boolean
  ) ;
  begin
    FShowTracking := _value ;
  end ;

  function TGIS_Editor.fget_SnapLayer
    : TGIS_LayerAbstract ;
  begin
    if FUseSnapLayers and ( FSnapLayers.Count > 0 ) then
      Result := FSnapLayers[0]
    else
      Result := nil ;
  end ;

  procedure TGIS_Editor.fset_SnapLayer(
    const _value : TGIS_LayerAbstract
  ) ;
  begin
    FSnapLayers.Clear ;
    if assigned( _value ) then begin
      FUseSnapLayers := True ;
      FSnapLayers.Add( _value ) ;
    end
    else
      FUseSnapLayers := False ; // emit old behavior
  end ;

  function TGIS_Editor.fget_SnapMargin
    : Integer ;
  begin
    Result := FSnapMargin ;
  end ;

  procedure TGIS_Editor.fset_SnapMargin(
    const _value : Integer
  ) ;
  begin
    FSnapMargin := _value ;
  end ;

  function TGIS_Editor.fget_SnapType
    : TGIS_EditorSnapType ;
  begin
    Result := FSnapType ;
  end ;

  procedure TGIS_Editor.fset_SnapType(
    const _value : TGIS_EditorSnapType
  ) ;
  begin
    FSnapType := _value ;
  end ;

  function TGIS_Editor.fget_SnapToIntersection
    : Boolean ;
  begin
    Result := FSnapToIntersection ;
  end ;

  procedure TGIS_Editor.fset_SnapToIntersection(
    const _value : Boolean
  ) ;
  begin
    FSnapToIntersection := _value ;
  end ;

  function TGIS_Editor.fget_SnapGridSpacing
    : Double ;
  begin
    Result := FSnapGridSpacing ;
  end ;

  procedure TGIS_Editor.fset_SnapGridSpacing(
    const _value : Double
  ) ;
  begin
    FSnapGridSpacing := _value ;
  end ;

  function TGIS_Editor.fget_Uid  : TGIS_Uid ;
  begin
    if InEdit then
      Result := TGIS_Shape(FCurrentShape).Uid
    else
      Result := -1 ;
  end ;

  function TGIS_Editor.fget_UndoState
    : Boolean ;
  begin
    Result := undoList.Count > 0 ;
  end ;

  function TGIS_Editor.fget_Viewer
    : TObject ;
  begin
    Result := FViewer ;
  end ;

  function  TGIS_Editor.fget_ViewerEnabled
    : Boolean ;
  begin
    Result := FViewerEnabled ;
  end;

  procedure TGIS_Editor.fset_ViewerEnabled(
    const _value : Boolean
  ) ;
  begin
    FViewerEnabled := _value ;
  end;

//----------------------------------------------------------------------------
// public methods from IGIS_Editor
//----------------------------------------------------------------------------

  procedure TGIS_Editor.AddPoint( const _ptg : TGIS_Point3D );
  var
    vwr : TGIS_Viewer ;
  begin
    vwr := TGIS_Viewer(FViewer) ;
    vwr.BusyPrepare( Self, GIS_BUSY_NOHOURGLASS ) ;
    try
      addPointInternal( _ptg, False ) ;
    finally
      vwr.BusyRelease( Self ) ;
    end ;
  end ;

  procedure TGIS_Editor.AddPointEx( const _ptg : TGIS_Point3D );
  var
    vwr : TGIS_Viewer ;
  begin
    vwr := TGIS_Viewer(FViewer) ;
    vwr.BusyPrepare( Self, GIS_BUSY_NOHOURGLASS ) ;
    try
      addPointInternal( _ptg, True ) ;
    finally
      vwr.BusyRelease( Self ) ;
    end ;
  end ;

  procedure TGIS_Editor.ChangeWinding ;
  var
    vwr : TGIS_Viewer ;
  begin
    vwr := TGIS_Viewer(FViewer) ;
    vwr.BusyPrepare( Self, GIS_BUSY_NOHOURGLASS ) ;
    try
      changeWindingInternal ;
    finally
      vwr.BusyRelease( Self ) ;
    end ;
  end ;

  procedure TGIS_Editor.CreatePart( const _ptg : TGIS_Point3D ) ;
  var
    vwr : TGIS_Viewer ;
  begin
    vwr := TGIS_Viewer(FViewer) ;
    vwr.BusyPrepare( Self, GIS_BUSY_NOHOURGLASS ) ;
    try
      createPartInternal( _ptg ) ;
    finally
      vwr.BusyRelease( Self ) ;
    end ;
  end ;

  procedure TGIS_Editor.CreateShape( const _layer : TGIS_LayerAbstract ;
                                     const _ptg   : TGIS_Point ;
                                     const _type  : TGIS_ShapeType ) ;
  var
    vwr : TGIS_Viewer ;
  begin
    vwr := TGIS_Viewer(FViewer) ;
    vwr.BusyPrepare( Self, GIS_BUSY_NOHOURGLASS ) ;
    try
      createShapeInternal( _layer, GisPoint3DFrom2D( _ptg ), _type,
                           TGIS_DimensionType.Unknown
                         ) ;
    finally
      vwr.BusyRelease( Self ) ;
    end ;
  end ;

  procedure TGIS_Editor.CreateShape( const _layer : TGIS_LayerAbstract ;
                                     const _ptg   : TGIS_Point3D ;
                                     const _type  : TGIS_ShapeType ) ;
  var
    vwr : TGIS_Viewer ;
  begin
    vwr := TGIS_Viewer(FViewer) ;
    vwr.BusyPrepare( Self, GIS_BUSY_NOHOURGLASS ) ;
    try
      createShapeInternal( _layer, _ptg, _type, TGIS_DimensionType.Unknown ) ;
    finally
      vwr.BusyRelease( Self ) ;
    end ;
  end ;

  procedure TGIS_Editor.CreateShape( const _layer : TGIS_LayerAbstract ;
                                     const _ptg   : TGIS_Point3D ;
                                     const _type  : TGIS_ShapeType ;
                                     const _dim   : TGIS_DimensionType
                                    ) ;
  var
    vwr : TGIS_Viewer ;
  begin
    vwr := TGIS_Viewer(FViewer) ;
    vwr.BusyPrepare( Self, GIS_BUSY_NOHOURGLASS ) ;
    try
      createShapeInternal( _layer, _ptg, _type, _dim ) ;
    finally
      vwr.BusyRelease( Self ) ;
    end ;
  end ;

  procedure TGIS_Editor.DeletePart ;
  var
    vwr : TGIS_Viewer ;
  begin
    vwr := TGIS_Viewer(FViewer) ;
    vwr.BusyPrepare( Self, GIS_BUSY_NOHOURGLASS ) ;
    try
      deletePartInternal ;
    finally
      vwr.BusyRelease( Self ) ;
    end ;
  end ;

  procedure TGIS_Editor.DeletePoint( const _pos : Integer ) ;
  var
    vwr : TGIS_Viewer ;
  begin
    vwr := TGIS_Viewer(FViewer) ;
    vwr.BusyPrepare( Self, GIS_BUSY_NOHOURGLASS ) ;
    try
      deletePointInternal( _pos ) ;
    finally
      vwr.BusyRelease( Self ) ;
    end ;
  end ;

  procedure TGIS_Editor.DeleteShape ;
  var
    vwr : TGIS_Viewer ;
  begin
    vwr := TGIS_Viewer(FViewer) ;
    vwr.BusyPrepare( Self, GIS_BUSY_NOHOURGLASS ) ;
    try
      deleteShapeInternal ;
    finally
      vwr.BusyRelease( Self ) ;
    end ;
  end ;

  procedure TGIS_Editor.EditShape(
    const _shp  : TGIS_ShapeAbstract ;
    const _part : Integer
  ) ;
  var
    vwr : TGIS_Viewer ;
  begin
    vwr := TGIS_Viewer(FViewer) ;
    vwr.BusyPrepare( Self, GIS_BUSY_NOHOURGLASS ) ;
    try
      editShapeInternal( _shp, _part ) ;
    finally
      vwr.BusyRelease( Self ) ;
    end ;
  end ;

  procedure TGIS_Editor.EndEdit ;
  var
    vwr : TGIS_Viewer ;
  begin
    if not InEdit then exit ;

    vwr := TGIS_Viewer(FViewer) ;
    vwr.BusyPrepare( Self, GIS_BUSY_NOHOURGLASS ) ;
    try
      endEditInternal ;
    finally
      vwr.BusyRelease( Self ) ;
    end ;
  end ;

  procedure TGIS_Editor.InsertPoint(
    const _pos : Integer      ;
    const _ptg : TGIS_Point3D
  );
  var
    vwr : TGIS_Viewer ;
  begin
    vwr := TGIS_Viewer(FViewer) ;
    vwr.BusyPrepare( Self, GIS_BUSY_NOHOURGLASS ) ;
    try
      insertPointInternal( _pos, _ptg ) ;
    finally
      vwr.BusyRelease( Self ) ;
    end ;
  end ;

  {$IFDEF CLR}
    procedure TGIS_Editor.MouseBegin(
      const _pt      : TPoint  ;
      const _nearest : Boolean
    ) ;
  {$ELSE}

    procedure TGIS_Editor.MouseBegin( const _pt : TPoint ;
                                      const _nearest : Boolean
                                    ) ;
  {$ENDIF}
  var
    vwr : TGIS_Viewer ;
  begin
    vwr := TGIS_Viewer(FViewer) ;
    vwr.BusyPrepare( Self, GIS_BUSY_NOHOURGLASS ) ;
    try
      mouseBeginInternal( _pt, _nearest ) ;
    finally
      vwr.BusyRelease( Self ) ;
    end ;
  end ;

  procedure TGIS_Editor.MouseMove ( const _pt : TPoint ) ;

  begin
    mouseMoveInternal( _pt ) ;
  end ;

  procedure TGIS_Editor.MouseEnd( const _pt : TPoint ) ;

  var
    vwr : TGIS_Viewer ;
  begin
    vwr := TGIS_Viewer(FViewer) ;
    vwr.BusyPrepare( Self, GIS_BUSY_NOHOURGLASS ) ;
    try
      mouseEndInternal( _pt ) ;
    finally
      vwr.BusyRelease( Self ) ;
    end ;
  end ;

  procedure TGIS_Editor.MovePoint(
    const _pos : Integer      ;
    const _ptg : TGIS_Point3D
  ) ;
  var
    vwr : TGIS_Viewer ;
  begin
    vwr := TGIS_Viewer(FViewer) ;
    vwr.BusyPrepare( Self, GIS_BUSY_NOHOURGLASS ) ;
    try
      movePointInternal( _pos, _ptg ) ;
    finally
      vwr.BusyRelease( Self ) ;
    end ;
  end ;

  procedure TGIS_Editor.Redo ;
  var
    vwr : TGIS_Viewer ;
  begin
    vwr := TGIS_Viewer(FViewer) ;
    vwr.BusyPrepare( Self, GIS_BUSY_NOHOURGLASS ) ;
    try
      redoInternal ;
    finally
      vwr.BusyRelease( Self ) ;
    end ;
  end ;

  procedure TGIS_Editor.RefreshShape ;
  var
    part_no      : Integer     ;
    point_no     : Integer     ;
    points_count : Integer     ;
    shp_src      : TGIS_Shape  ;
    shp_dst      : TGIS_Shape  ;
    part_cnt     : Integer     ;
    vwr          : TGIS_Viewer ;
  begin
    if not assigned( FCurrentShape ) then exit ;

    vwr := TGIS_Viewer(FViewer) ;
    // move everything from temporary shape to a real shape.
    shp_dst := TGIS_Shape( FCurrentShape ) ;
    shp_src := shp_dst.CreateCopy ;

    shp_src.Lock( TGIS_Lock.Projection ) ;
    shp_dst.Lock( TGIS_Lock.Projection ) ;
    shp_dst.Reset ; // make it empty and later re-add points

    part_cnt := 0 ;

    for part_no:= 0 to shp_src.GetNumParts - 1 do
    begin
      if part_no = Part then
      begin
        if partState.MustDelete then
        begin
          partState.MustDelete := False ;
          FPart := 0 ;
        end
        else
        begin
          shp_dst.AddPart ;
          inc(part_cnt) ;
          for point_no := 0 to pointState.Count-1 do
          begin
            shp_dst.AddPoint3D( pointState.Buf[ point_no ] ) ;
          end ;
        end
      end
      else
      begin
        shp_dst.AddPart ;
        inc(part_cnt) ;
        points_count := shp_src.GetPartSize( part_no );
        for point_no := 0 to points_count -1 do
        begin
          shp_dst.AddPoint( shp_src.GetPoint( part_no, point_no ) ) ;
        end ;
      end ;
    end ;
    shp_src.Unlock ;
    FreeObject( shp_src ) ;
    if partState.MustCreate then
    begin
      shp_dst.AddPart ;
      shp_dst.AddPoint3D ( partState.PointToAdd ) ;
      FPart := part_cnt ;
      partState.MustCreate := False ;
    end ;
    shp_dst.Unlock ;

    loadShape ;

    MustRedraw := True ;
    vwr.InvalidateEditor( False ) ;

    vwr.Parent.ControlRaiseEditorChangeEvent( Self ) ;
  end ;

  procedure TGIS_Editor.RevertShape ;
  var
    vwr : TGIS_Viewer ;
  begin
    vwr := TGIS_Viewer(FViewer) ;
    vwr.BusyPrepare( Self, GIS_BUSY_NOHOURGLASS ) ;
    try
      revertShapeInternal ;
    finally
      vwr.BusyRelease( Self ) ;
    end ;
  end ;

  procedure TGIS_Editor.Undo ;
  var
    vwr : TGIS_Viewer ;
  begin
    vwr := TGIS_Viewer(FViewer) ;
    vwr.BusyPrepare( Self, GIS_BUSY_NOHOURGLASS ) ;
    try
      undoInternal ;
    finally
      vwr.BusyRelease( Self ) ;
    end ;
  end ;

  procedure TGIS_Editor.ClearSnapLayers ;
  begin
    FSnapLayers.Clear ;
    FUseSnapLayers := False ;
  end ;

  function  TGIS_Editor.AddSnapLayer(
    const _layer : TGIS_LayerAbstract
  ) : Integer ;
  begin
    FSnapLayers.Add( _layer ) ;
    FUseSnapLayers := True ;
    Result := FSnapLayers.IndexOf( _layer ) ;
  end ;

  procedure TGIS_Editor.RemoveSnapLayer(
    const _layer : TGIS_LayerAbstract
  ) ;
  begin
    if not FindSnapLayer( oDynamicSnapLayer ) then
      exit ;

    FSnapLayers.Remove( _layer ) ;
    if FSnapLayers.Count = 0 then
      FUseSnapLayers := False ;
  end ;

  function TGIS_Editor.FindSnapLayer(
    const _layer : TGIS_LayerAbstract
  ) : Boolean ;
  begin
    Result := FSnapLayers.IndexOf( _layer ) >= 0 ;
  end ;

  function TGIS_Editor.FindSnapPoint(
    const _pt       : TPoint ;
      var _ptg      : TGIS_Point ;
      var _snapType : TGIS_EditorSnapResultType
  ) : Boolean ;
  var
    ptg1 : TGIS_Point3D ;
  begin
    ptg1 := TGIS_Viewer(FViewer).ScreenToMap3D( _pt ) ;
    _ptg := GisPoint2DFrom3D( doSnapPoint( ptg1, Result, _snapType ) ) ;
  end ;

//==================================== END =====================================
end.
