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
  Common interfaces
}

{$IFDEF DCC}
  unit GisInterfaces ;
  {$HPPEMIT '#pragma link "GisInterfaces"'}
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
    System.ComponentModel,
    System.Collections.Generic,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.SysUtils,
    System.Types,
    System.Generics.Defaults,
    System.Generics.Collections,

    {$IFDEF GIS_XDK}
      VCL.Graphics,
    {$ENDIF}

    GisRtl,
    GisTypes,
    GisTypesUI,
    GisTypes3D,
    GisClasses,
    GisCsSystems ;
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

//----------------------------------------------------------------------------
// general GIS types and declarations
//----------------------------------------------------------------------------
type
  {$IFDEF GIS_3D}
    IGIS_Viewer3D = {$IFDEF OXYGENE} public partial {$ENDIF}  interface ;
  {$ELSEIF JAVA}
    // just to emulate proper functioning in PVL.

    /// <summary>
    ///   Common API for 3D viewing operations.
    /// </summary>
    IGIS_Viewer3D = public partial interface
      /// <summary>
      ///   Dummy funciton.
      /// </summary>
      procedure dummyfunction ;
    end;
  {$ENDIF}


  /// <summary>
  ///   Callback for TGIS_Viewer.WaitForNotBusy
  /// </summary>
  /// <param name="_sender">
  ///   sender object
  /// </param>
  TGIS_WaitForNotBusyProc = procedure( _sender : TObject
                                     ) of object ;

  /// <summary>
  ///   Stub parent for TGIS_Config.
  /// </summary>
  TGIS_ConfigAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF}
                        class( TGIS_BaseObjectDisposable )
  end;

  /// <summary>
  ///   Stub parent for TGIS_Layer.
  /// </summary>
  TGIS_LayerAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF}
                       class( TGIS_BaseObjectDisposable )
  end;

  /// <summary>
  ///   Stub parent for TGIS_Shape.
  /// </summary>
  TGIS_ShapeAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF}
                       class( TGIS_Object )
  end;

  /// <summary>
  ///   Stub parent for TGIS_LabelsArea.
  /// </summary>
  TGIS_LabelsAreaAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF}
                            class( TGIS_Object )
    public
      /// <summary>
      ///   Free all reserved areas.
      /// </summary>
      procedure  Reset ;        virtual; abstract;

      /// <summary>
      ///   Test if a given label was already allocated.
      /// </summary>
      /// <param name="_label">
      ///   label to be tested
      /// </param>
      /// <returns>
      ///   True if item already exist on the list.
      /// </returns>
      function   IsDuplicated   ( const _label     : String
                                ) : Boolean ;
                                virtual; abstract;

      /// <summary>
      ///   Add a given label to a duplicated list.
      /// </summary>
      /// <param name="_label">
      ///   label text to added
      /// </param>
      procedure  AddDuplicated  ( const _label     : String
                                ) ;
                                virtual; abstract;

      /// <summary>
      ///   Reserve extent for label in Label Area Allocator. This operation
      ///   avoids overlapping.
      /// </summary>
      /// <param name="_rect">
      ///   extent to be reserved;
      /// </param>
      /// <param name="_layer">
      ///   layer to which label belongs
      /// </param>
      /// <param name="_uid">
      ///   Uid of shape related with label
      /// </param>
      /// <param name="_tolerance">
      ///   tolerance of label placement in device pixels; generally should be
      ///   expressed like 1.5/GIS.Zoom
      /// </param>
      /// <returns>
      ///   True if place for label was found and has been allocated.
      /// </returns>
      function   Allocate       ( const _rect      : TGIS_Extent ;
                                  const _layer     : Integer     ;
                                  const _uid       : TGIS_Uid    ;
                                  const _tolerance : Double
                                ) : Boolean ;
                                virtual; abstract;

      /// <summary>
      ///   Find a shape uid related with label close to a point.
      /// </summary>
      /// <param name="_ptg">
      ///   point
      /// </param>
      /// <param name="_layer">
      ///   layer to which label belongs
      /// </param>
      /// <param name="_uid">
      ///   Uid of shape related with label
      /// </param>
      /// <returns>
      ///   Found shape or nil.
      /// </returns>
      function   FindShape      ( const _ptg       : TGIS_Point ;
                                  const _layer     : Integer ;
                                    var _uid       : TGIS_Uid
                                ) : Boolean ;
                                virtual; abstract;

      /// <summary>
      ///   Reserve extent for a 4 node polygon in the Label Area Allocator.
      ///   This operation avoids overlapping.
      /// </summary>
      /// <param name="_ptgA">
      ///   point of polygon
      /// </param>
      /// <param name="_ptgB">
      ///   point of polygon
      /// </param>
      /// <param name="_ptgC">
      ///   point of polygon
      /// </param>
      /// <param name="_ptgD">
      ///   point of polygon
      /// </param>
      /// <param name="_layer">
      ///   layer to which label belongs
      /// </param>
      /// <param name="_uid">
      ///   Uid of shape related with label
      /// </param>
      /// <param name="_tolerance">
      ///   tolerance of label placement in device pixels; generally should be
      ///   expressed like 1.5/GIS.Zoom
      /// </param>
      /// <returns>
      ///   True if place for label was found and has been allocated.
      /// </returns>
      function   AllocateEx     ( const _ptgA      : TGIS_Point ;
                                  const _ptgB      : TGIS_Point ;
                                  const _ptgC      : TGIS_Point ;
                                  const _ptgD      : TGIS_Point ;
                                  const _layer     : Integer    ;
                                  const _uid       : TGIS_Uid   ;
                                  const _tolerance : Double
                                ) : Boolean ;
                                virtual; abstract;

      /// <summary>
      ///   Check if there is any label for particular shape already allocated.
      /// </summary>
      /// <param name="_layer">
      ///   layer to which label belongs
      /// </param>
      /// <param name="_uid">
      ///   Uid of shape related with label
      /// </param>
      /// <returns>
      ///   True if label is allocated.
      /// </returns>
      function   IsAny          ( const _layer     : Integer    ;
                                  const _uid       : TGIS_Uid
                                ) : Boolean ;
                                virtual; abstract;
  end;

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  /// <summary>
  ///   Layers list for Items.
  /// </summary>
  {$IFNDEF GIS_NOGENERICS}
    TGIS_LayerAbstractList  = {$IFDEF OXYGENE} public {$ENDIF} TObjectList<TGIS_LayerAbstract> ;
  {$ELSE}
    TGIS_LayerAbstractList  = class ( TObjectList<TGIS_LayerAbstract> ) ;
  {$ENDIF}

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  /// <summary>
  ///   Abstract shapes list.
  /// </summary>
  {$IFNDEF GIS_NOGENERICS}
    TGIS_ShapeAbstractList  = {$IFDEF OXYGENE} public {$ENDIF} TObjectList<TGIS_ShapeAbstract> ;
  {$ELSE}
    TGIS_ShapeAbstractList  = class( TObjectList<TGIS_ShapeAbstract> ) ;
  {$ENDIF}

  /// <summary>
  ///   Subscriber mechanism interface.
  /// </summary>
  /// <remarks>
  ///   TGIS_ControlLegend and TGIS_ControlScale are capable to subscribe
  ///   TGIS_Viewer update event. See also TGIS_Viewer.Subscribe and
  ///   TGIS_Viewer.UnSubscribe
  /// </remarks>
  IGIS_Subscribe = {$IFDEF OXYGENE} public {$ENDIF} interface
    {$IFDEF DCC}
      ['{9F757476-8DCD-4B4E-80BE-A1CCAF3FE3AD}']
    {$ENDIF}
    {$REGION IGIS_Subscribe public methods}

      /// <summary>
      ///   Subscriber notification event.
      /// </summary>
      /// <param name="_sender">
      ///   sender object
      /// </param>
      /// <param name="_event">
      ///   event identifier
      /// </param>
      /// <param name="_context">
      ///   context object
      /// </param>
      procedure SubscribedEvent(       _sender  : TObject ;
                                       _event   : Integer ;
                                       _context : TObject
                               ) ;
    {$ENDREGION}
  end ;


  /// <summary>
  ///    Common API for shape visual editor.
  /// </summary>
  IGIS_Editor = {$IFDEF OXYGENE} public {$ENDIF} interface
    {$IFDEF DCC}
      ['{83F0DC57-BB7B-4E6F-96AD-1A9B1872996A}']
    {$ENDIF}

    {$REGION IGIS_Editor property access routines}
      function  fget_BlockSnapping      : Boolean ;
      procedure fset_BlockSnapping      ( const _value : Boolean
                                        ) ;
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
      function  fget_ShowTracking       : Boolean ;
      procedure fset_ShowTracking       ( const _value : Boolean
                                        ) ;
      function  fget_ShowEdgesLengths   : Boolean ;
      procedure fset_ShowEdgesLengths   ( const _value : Boolean
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
    {$ENDREGION}

    {$REGION IGIS_Editor public methods}

      /// <summary>
      ///   Add a new point.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be added; expected Viewer coordinate system
      /// </param>
      /// <remarks>
      ///   Use this method to the tail of the current part of the currently
      ///   edited shape. See TGIS_Shape.Reverse for example.
      /// </remarks>
      procedure AddPoint            ( const _ptg        : TGIS_Point3D
                                    ) ;

      /// <summary>
      ///   Add a new point for a group.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be added; expected Viewer coordinate system
      /// </param>
      /// <remarks>
      ///   Use this method to the tail of the current part of the currently
      ///   edited shape within a group. See TGIS_Shape.Reverse for example.
      /// </remarks>
      procedure AddPointEx          ( const _ptg        : TGIS_Point3D
                                    ) ;

      /// <summary>
      ///   Change winding of the current part. Valid and important only for
      ///   polygons. Thanks to this, the whole inside polygon can be
      ///   transparent.
      /// </summary>
      procedure ChangeWinding       ;

      /// <summary>
      ///   Create and activate the editing procedure for a new part for the
      ///   current shape.
      /// </summary>
      /// <param name="_ptg">
      ///   "starting" point of a new part to be created; expected Viewer
      ///   coordinate system
      /// </param>
      procedure CreatePart          ( const _ptg        : TGIS_Point3D
                                    ) ;

      /// <summary>
      ///   Create and activate editing of a new shape.
      /// </summary>
      /// <param name="_layer">
      ///   TGIS_Layer which will own new shape
      /// </param>
      /// <param name="_ptg">
      ///   "starting" of a new shape to be created; expected Viewer
      ///   coordinate system
      /// </param>
      /// <param name="_type">
      ///   type of shape to be created; it must be type supported by layer;
      ///   if TGIS_ShapeType.Unknown then shape of default type will be
      ///   created
      /// </param>
      procedure CreateShape         ( const _layer      : TGIS_LayerAbstract ;
                                      const _ptg        : TGIS_Point ;
                                      const _type       : TGIS_ShapeType
                                    ) ; overload;

      /// <summary>
      ///   Create and activate editing of a new shape.
      /// </summary>
      /// <param name="_layer">
      ///   TGIS_Layer which will own new shape
      /// </param>
      /// <param name="_ptg">
      ///   "starting" 3D of a new shape to be created; expected Viewer
      ///   coordinate system
      /// </param>
      /// <param name="_type">
      ///   type of shape to be created; it must be type supported by layer;
      ///   if TGIS_DimensionType.Unknown then shape of default type will be
      ///   created
      /// </param>
      procedure CreateShape         ( const _layer      : TGIS_LayerAbstract ;
                                      const _ptg        : TGIS_Point3D ;
                                      const _type       : TGIS_ShapeType
                                    ) ; overload;

      /// <summary>
      ///   Create and activate editing of a new shape.
      /// </summary>
      /// <param name="_layer">
      ///   TGIS_Layer which will own new shape
      /// </param>
      /// <param name="_ptg">
      ///   "starting" 3D of a new shape to be created; expected Viewer
      ///   coordinate system
      /// </param>
      /// <param name="_type">
      ///   type of shape to be created; it must be type supported by layer; if
      ///   TGIS_ShapeType.Unknown then shape of default type will be created
      /// </param>
      /// <param name="_dim">
      ///   dimension of shape to be created; it must be type supported by
      ///   layer; if TGIS_ShapeType.Unknown then shape of default dimension
      ///   will be created
      /// </param>
      procedure CreateShape         ( const _layer      : TGIS_LayerAbstract ;
                                      const _ptg        : TGIS_Point3D ;
                                      const _type       : TGIS_ShapeType ;
                                      const _dim        : TGIS_DimensionType
                                    ) ; overload;

      /// <summary>
      ///   Delete current part from shape.
      /// </summary>
      /// <remarks>
      ///   <para>
      ///     Editing will activate part number 0. If current shape has only
      ///     one part, then whole shape will be deleted and edition will be
      ///     stopped.
      ///   </para>
      ///   <para>
      ///      See CreatePart for example.
      ///   </para>
      /// </remarks>
      procedure DeletePart          ;

      /// <summary>
      ///   Delete a point from the current part of the currently edited shape.
      /// </summary>
      /// <param name="_pos">
      ///   the point which will be deleted; points are counted from 1;
      /// </param>
      /// <remarks>
      ///   <note type="important">
      ///    The method is deprecated. Use rather DeletePointEx().
      ///   </note>
      ///   <para>
      ///     The numbering of points differs by 1
      ///     from the numbering of points displayed on the screen.
      ///   </para>
      /// </remarks>
      procedure DeletePoint         ( const _pos        : Integer
                                    ) ; {$IFNDEF GENDOC} deprecated ; {$ENDIF}

      /// <summary>
      ///   Delete a point from the current part of the currently edited shape.
      /// </summary>
      /// <param name="_pos">
      ///   the point which will be deleted; points are counted from 0;
      /// </param>
      /// <remarks>
      ///   <para>
      ///     The numbering of points is consistent
      ///     with the numbering of points displayed on the screen.
      ///   </para>
      /// </remarks>
      procedure DeletePointEx       ( const _pos        : Integer
                                    ) ;

      /// <summary>
      ///   Delete currently edited shape and end editing.
      /// </summary>
      procedure DeleteShape         ;

      /// <summary>
      ///   Starts editing of a shape given by _shp. Only the part given by
      ///   _part will be in edit mode.
      /// </summary>
      /// <param name="_shp">
      ///   shape to be edited
      /// </param>
      /// <param name="_part">
      ///   part in _shape to be edited
      /// </param>
      procedure EditShape           ( const _shp        : TGIS_ShapeAbstract ;
                                      const _part       : Integer
                                    ) ;
      /// <summary>
      ///   End editing of the shape given by EditShape.
      /// </summary>
      procedure EndEdit             ;

      /// <summary>
      ///   Insert a point to the current part of the currently edited shape.
      /// </summary>
      /// <param name="_pos">
      ///   where point must be inserted; points are counted from 0
      /// </param>
      /// <param name="_ptg">
      ///   point to be inserted; expected Viewer coordinate system
      /// </param>
      /// <remarks>
      ///   <para>
      ///     This is similar to AddPoint except inserting position. Using _pos
      ///     param we can choose a destination part to which we insert a new
      ///     point.
      ///   </para>
      ///   <para>
      ///     The numbering of points is consistent
      ///     with the numbering of points displayed on the screen.
      ///   </para>
      ///   <para>
      ///      See TGIS_Shape.Reverse for similar example.
      ///   </para>
      /// </remarks>
      procedure InsertPoint         ( const _pos        : Integer ;
                                      const _ptg        : TGIS_Point3D
                                    ) ;

      /// <summary>
      ///   Insert a point to the current part of the currently edited shape for a group.
      /// </summary>
      /// <param name="_pos">
      ///   where point must be inserted; points are counted from 0
      /// </param>
      /// <param name="_ptg">
      ///   point to be inserted; expected Viewer coordinate system
      /// </param>
      /// <remarks>
      ///   <para>
      ///     This is similar to AddPoint except inserting position. Using _pos
      ///     param we can choose a destination part to which we insert a new
      ///     point.
      ///   </para>
      ///   <para>
      ///     The numbering of points is consistent
      ///     with the numbering of points displayed on the screen.
      ///   </para>
      ///   <para>
      ///      See TGIS_Shape.Reverse for similar example.
      ///   </para>
      /// </remarks>
      procedure InsertPointEx       ( const _pos        : Integer ;
                                      const _ptg        : TGIS_Point3D
                                    ) ;

      /// <summary>
      ///   Start the editing of a point at the location given by _pt.
      /// </summary>
      /// <param name="_pt">
      ///   start point defined in screen coordinates
      /// </param>
      /// <param name="_nearest">
      ///   if true, then nearest insertion point will be chosen; could be
      ///   overwritten by Mode property.
      /// </param>
      /// <remarks>
      ///   <para>
      ///     If a point exists near _pt, then it will be moved /if movement
      ///     occurs/, or otherwise deleted.
      ///   </para>
      ///   <para>
      ///      Editing must be started by EditShape.
      ///   </para>
      /// </remarks>
      procedure MouseBegin          ( const _pt         : TPoint ;
                                      const _nearest    : Boolean
                                    ) ;

      /// <summary>
      ///   Continuing of point movement, deletion, or addition.
      /// </summary>
      /// <param name="_pt">
      ///   new location of point /in screen coordinates/
      /// </param>
      /// <remarks>
      ///   Caution! See notes in MouseBegin.
      /// </remarks>
      procedure MouseMove           ( const _pt         : TPoint
                                    ) ;

      /// <summary>
      ///   End of point movement, deletion, or addition.
      /// </summary>
      /// <param name="_pt">
      ///   new location of point /in screen coordinates/
      /// </param>
      /// <remarks>
      ///   Caution! See notes in MouseBegin.
      /// </remarks>
      procedure MouseEnd            ( const _pt         : TPoint
                                    ) ;

      /// <summary>
      ///   Move a point of the current part of the currently edited shape to a
      ///   new location.
      /// </summary>
      /// <param name="_pos">
      ///   which point will be moved; points are counted from 1
      /// </param>
      /// <param name="_ptg">
      ///   new location; expected Viewer coordinate system
      /// </param>
      /// <remarks>
      ///   <note type="important">
      ///    The method is deprecated. Use rather MovePointEx().
      ///   </note>
      ///   <para>
      ///     The numbering of points differs by 1
      ///     from the numbering of points displayed on the screen.
      ///   </para>
      /// </remarks>
      procedure MovePoint           ( const _pos        : Integer ;
                                      const _ptg        : TGIS_Point3D
                                    ) ; {$IFNDEF GENDOC} deprecated ; {$ENDIF}

      /// <summary>
      ///   Move a point of the current part of the currently edited shape to a
      ///   new location.
      /// </summary>
      /// <param name="_pos">
      ///   which point will be moved; points are counted from 0
      /// </param>
      /// <param name="_ptg">
      ///   new location; expected Viewer coordinate system
      /// </param>
      /// <remarks>
      ///   <para>
      ///     The numbering of points is consistent
      ///     with the numbering of points displayed on the screen.
      ///   </para>
      /// </remarks>
      procedure MovePointEx         ( const _pos        : Integer ;
                                      const _ptg        : TGIS_Point3D
                                    ) ;

      /// <summary>
      ///   Redo the previous action. This applies only to changes performed on a
      ///   single part.
      /// </summary>
      procedure Redo                ;

      /// <summary>
      ///   Store the editing buffer back into the shape. Perform screen redraw.
      /// </summary>
      procedure RefreshShape        ;

      /// <summary>
      ///   Revert currently edited shape to a saved version.
      /// </summary>
      /// <remarks>
      ///   For newly created shapes, do nothing. See DeleteShape for example.
      /// </remarks>
      procedure RevertShape         ;

      /// <summary>
      ///   Undo the last action. This applies only to changes performed on a
      ///   single part.
      /// </summary>
      procedure Undo                ;

      /// <summary>
      ///   Clear the list of snap-to layers.
      /// </summary>
      procedure ClearSnapLayers ;

      /// <summary>
      ///   Add a snap-to layer to the list.
      /// </summary>
      /// <param name="_layer">
      ///   layer to snap
      /// </param>
      /// <returns>
      ///   index of added layer on the list
      /// </returns>
      function  AddSnapLayer        ( const _layer       : TGIS_LayerAbstract
                                    ) : Integer ;

      /// <summary>
      ///   Remove a snap layer from the list.
      /// </summary>
      /// <param name="_layer">
      ///   layer to snap
      /// </param>
      procedure RemoveSnapLayer     ( const _layer       : TGIS_LayerAbstract
                                    ) ;

      /// <summary>
      ///   Find a snap layer on the list.
      /// </summary>
      /// <param name="_layer">
      ///   layer to snap
      /// </param>
      /// <returns>
      ///   True if layer is on the list
      /// </returns>
      function FindSnapLayer        ( const _layer       : TGIS_LayerAbstract
                                    ) : Boolean ;

      /// <summary>
      ///   Find a snap point based on a snap type from snap-to layers.
      /// </summary>
      /// <param name="_pt">
      ///   Mouse position
      /// </param>
      /// <param name="_ptg">
      ///   found snap point coordinates
      /// </param>
      /// <param name="_snapType">
      ///   found type of snap point (vertex, edge, intersection, midpoint )
      /// </param>
      /// <returns>
      ///   True if any snap point was found
      /// </returns>
      function FindSnapPoint        ( const _pt          : TPoint ;
                                        var _ptg         : TGIS_Point ;
                                        var _snapType    : TGIS_EditorSnapResultType
                                    ) : Boolean ;
    {$ENDREGION}

    {$REGION IGIS_Editor public properties}

      /// <summary>
      ///   If True, redo buffer is not empty.
      /// </summary>
      property CanRedo : Boolean
                read {$IFNDEF OXYGENE} fget_RedoState {$ENDIF} ;

      /// <summary>
      ///   If True, undo buffer is not empty.
      /// </summary>
      property CanUndo : Boolean
                read {$IFNDEF OXYGENE} fget_UndoState {$ENDIF} ;

      /// <summary>
      ///   Currently edited shape.
      /// </summary>
      property CurrentShape : TGIS_ShapeAbstract
                read {$IFNDEF OXYGENE} fget_CurrentShape {$ENDIF} ;

      /// <summary>
      ///   True if editor should be redraw. State should be canceled
      ///   after successful rendering.
      /// </summary>
      property MustRedraw : Boolean
                read  {$IFNDEF OXYGENE} fget_MustRedraw {$ENDIF}
                write {$IFNDEF OXYGENE} fset_MustRedraw {$ENDIF} ;

      /// <summary>
      ///   If True, editing is active.
      /// </summary>
      property InEdit : Boolean
                read {$IFNDEF OXYGENE} fget_InEdit {$ENDIF} ;

      /// <summary>
      ///   Layer that holds the shape being currently edited.
      /// </summary>
      property Layer : TGIS_LayerAbstract
                read {$IFNDEF OXYGENE} fget_Layer {$ENDIF} ;

      /// <summary>
      ///   Mode of edition. Will control how _nearest parameter of the
      ///   MouseBegin() behaves and therefore will control editing behavior
      ///   in TGIS_ViewerWnd.
      /// </summary>
      property Mode : TGIS_EditorMode
                read  {$IFNDEF OXYGENE} fget_Mode {$ENDIF}
                write {$IFNDEF OXYGENE} fset_Mode {$ENDIF} ;

      /// <summary>
      ///   Part number of currently edited shape.
      /// </summary>
      property Part : Integer
                read {$IFNDEF OXYGENE} fget_Part {$ENDIF} ;

      /// <summary>
      ///   Access to individual points in a part.
      /// </summary>
      /// <param name="_pos">
      ///   index of individual point on a shape; points are counted from 0 to
      ///   PointCount.
      /// </param>
      property Point[ const _pos : Integer ] : TGIS_Point3D
                read  {$IFNDEF OXYGENE} fget_Point {$ENDIF}
                write {$IFNDEF OXYGENE} fset_Point {$ENDIF} ;

      /// <summary>
      ///   Number of points in the current part of the currently edited shape.
      /// </summary>
      property PointCount : Integer
                read {$IFNDEF OXYGENE} fget_PointCount {$ENDIF} ;

      /// <summary>
      ///   Position of the currently edited point.
      /// </summary>
      property PointPos : Integer
                read  {$IFNDEF OXYGENE} fget_PointPos {$ENDIF}
                write {$IFNDEF OXYGENE} fset_PointPos {$ENDIF} ;

      /// <summary>
      ///   Pointer mode ( mouse, touch or pen ).
      /// </summary>
      property PointerMode : TGIS_PointerMode
                read  {$IFNDEF OXYGENE} fget_PointerMode {$ENDIF}
                write {$IFNDEF OXYGENE} fset_PointerMode {$ENDIF} ;

      /// <summary>
      ///   Layer containing geometry to which edited points will be snapped.
      ///   After editing start /see: EditShape/
      ///   will always be the same as the edited shape layer.
      /// </summary>
      property SnapLayer : TGIS_LayerAbstract
                read  {$IFNDEF OXYGENE} fget_SnapLayer {$ENDIF}
                write {$IFNDEF OXYGENE} fset_SnapLayer {$ENDIF} ;

      /// <summary>
      ///   Uid of currently edited shape or -1.
      /// </summary>
      property Uid : TGIS_Uid
                read {$IFNDEF OXYGENE} fget_Uid {$ENDIF} ;

      /// <summary>
      ///   Viewer on which editor has been created.
      /// </summary>
      property Viewer : TObject
                read {$IFNDEF OXYGENE} fget_Viewer {$ENDIF} ;
    {$ENDREGION}

    {$REGION IGIS_Editor published properties}
      {#gendoc:published:GENXDK}

      /// <summary>
      ///   If True, block snapping operations during adding points.
      /// </summary>
      property BlockSnapping : Boolean
                read  {$IFNDEF OXYGENE} fget_BlockSnapping {$ENDIF}
                write {$IFNDEF OXYGENE} fset_BlockSnapping {$ENDIF} ;

      /// <summary>
      ///   Editing lines style.
      /// </summary>
      property EditingLinesStyle  : TGIS_EditorStyle
                read  {$IFNDEF OXYGENE} fget_EditingLinesStyle {$ENDIF}
                write {$IFNDEF OXYGENE} fset_EditingLinesStyle {$ENDIF};

      /// <summary>
      ///   Editing points style.
      /// </summary>
      property EditingPointsStyle : TGIS_EditorEditingPointsStyle
                read  {$IFNDEF OXYGENE} fget_EditingPointsStyle {$ENDIF}
                write {$IFNDEF OXYGENE} fset_EditingPointsStyle {$ENDIF} ;

      /// <summary>
      ///   Edges lengths style.
      /// </summary>
      property EditingEdgeLengthsStyle : TGIS_EditorEdgeLengthsStyle
                read  {$IFNDEF OXYGENE} fget_EdgeLengthsStyle {$ENDIF}
                write {$IFNDEF OXYGENE} fset_EdgeLengthsStyle {$ENDIF} ;

      /// <summary>
      ///    Determines how new vertices are created relative to existing vertices.
      /// </summary>
      property EditorMode : TGIS_EditorModeEx
                read  {$IFNDEF OXYGENE} fget_EditorMode {$ENDIF}
                write {$IFNDEF OXYGENE} fset_EditorMode {$ENDIF} ;

      /// <summary>
      ///   Minimal mouse movement to perform a change.
      /// </summary>
      property MinMove : Integer
                read  {$IFNDEF OXYGENE} fget_MinMove {$ENDIF}
                write {$IFNDEF OXYGENE} fset_MinMove {$ENDIF} ;

      /// <summary>
      ///   Distance tolerance of vertices selection.
      /// </summary>
      property SelectTolerance : Integer
                read  {$IFNDEF OXYGENE} fget_SelectTolerance {$ENDIF}
                write {$IFNDEF OXYGENE} fset_SelectTolerance {$ENDIF} ;

      /// <summary>
      ///   Distance tolerance of vertices selection upon pen gesture.
      /// </summary>
      property SelectTolerancePen : Integer
                read  {$IFNDEF OXYGENE} fget_SelectTolerancePen {$ENDIF}
                write {$IFNDEF OXYGENE} fset_SelectTolerancePen {$ENDIF} ;

      /// <summary>
      ///   Distance tolerance of vertices selection upon touch gesture.
      /// </summary>
      property SelectToleranceTouch : Integer
                read  {$IFNDEF OXYGENE} fget_SelectToleranceTouch {$ENDIF}
                write {$IFNDEF OXYGENE} fset_SelectToleranceTouch {$ENDIF} ;

      /// <summary>
      ///   If True, show dragging track when moving a vertex to a new position.
      /// </summary>
      property ShowDraggingTrack  : Boolean
                read  {$IFNDEF OXYGENE} fget_ShowDraggingTrack {$ENDIF}
                write {$IFNDEF OXYGENE} fset_ShowDraggingTrack {$ENDIF} ;

      /// <summary>
      ///   If True, show the Z coordinates for the vertices.
      /// </summary>
      property ShowPoints3D : Boolean
                read  {$IFNDEF OXYGENE} fget_ShowPoints3D {$ENDIF}
                write {$IFNDEF OXYGENE} fset_ShowPoints3D {$ENDIF} ;

      /// <summary>
      ///   If True, show the vertex numbers.
      /// </summary>
      property ShowPointsNumbers  : Boolean
                read  {$IFNDEF OXYGENE} fget_ShowPointsNumbers {$ENDIF}
                write {$IFNDEF OXYGENE} fset_ShowPointsNumbers {$ENDIF} ;

      /// <summary>
      ///   If True, show vertices to trace from a shape in the snap-to layer.
      /// </summary>
      property ShowTracking : Boolean
                read  {$IFNDEF OXYGENE} fget_ShowTracking {$ENDIF}
                write {$IFNDEF OXYGENE} fset_ShowTracking {$ENDIF} ;

      /// <summary>
      ///   If True, show edges lengths from vertex to vertex.
      /// </summary>
      property ShowEdgesLengths : Boolean
                read  {$IFNDEF OXYGENE} fget_ShowEdgesLengths {$ENDIF}
                write {$IFNDEF OXYGENE} fset_ShowEdgesLengths {$ENDIF} ;

      /// <summary>
      ///   The distance tolerance from the snap-to feature for a snap to occur.
      /// </summary>
      property SnapMargin : Integer
                read  {$IFNDEF OXYGENE} fget_SnapMargin {$ENDIF}
                write {$IFNDEF OXYGENE} fset_SnapMargin {$ENDIF} ;

      /// <summary>
      ///   Snap type - how to snap to a vertex or segment.
      /// </summary>
      property SnapType : TGIS_EditorSnapType
                read  {$IFNDEF OXYGENE} fget_SnapType {$ENDIF}
                write {$IFNDEF OXYGENE} fset_SnapType {$ENDIF} ;

      /// <summary>
      ///   If True, enable snap to the point of intersections between shapes.
      /// </summary>
      property SnapToIntersection : Boolean
                read  {$IFNDEF OXYGENE} fget_SnapToIntersection {$ENDIF}
                write {$IFNDEF OXYGENE} fset_SnapToIntersection {$ENDIF} ;

      /// <summary>
      ///   The spacing of the grid to which points will be snapped.
      /// </summary>
      property SnapGridSpacing : Double
                read  {$IFNDEF OXYGENE} fget_SnapGridSpacing {$ENDIF}
                write {$IFNDEF OXYGENE} fset_SnapGridSpacing {$ENDIF} ;

      /// <summary>
      ///   Set to true if Editor Mode is enabled in a TGIS_ViewerWnd.
      /// </summary>
      /// <remarks>
      ///   For internal purposes mainly.
      /// </remarks>
      property ViewerEnabled : Boolean
                read  {$IFNDEF OXYGENE} fget_ViewerEnabled {$ENDIF}
                write {$IFNDEF OXYGENE} fset_ViewerEnabled {$ENDIF} ;
    {$ENDREGION}

    {$REGION IGIS_Editor recommended events}
    (*
        property SnapPointEvent   : TGIS_EditorSnapPointEvent
                                    read  FOnSnapPoint
                                    write FOnSnapPoint ;
    *)
    {$ENDREGION}
  end ;

  /// <summary>
  ///   Types of config format.
  /// </summary>
  TGIS_ConfigFormat = {$IFDEF OXYGENE} public {$ENDIF}
  (
      /// <summary>
      ///   Unknown format.
      /// </summary>
      Unknown     ,

      /// <summary>
      ///   Configuration save as Ini file.
      /// </summary>
      Ini     ,

      /// <summary>
      ///   Configuration save as Xml file.
      /// </summary>
      Xml
  ) ;


  IGIS_HierarchyManager = {$IFDEF OXYGENE} public partial {$ENDIF} interface ;

  /// <summary>
  ///   Hierarchy group class.
  /// </summary>
  IGIS_HierarchyGroup = {$IFDEF OXYGENE} public {$ENDIF} interface( IInterface )
    {$IFDEF DCC}
      ['{5470EA92-9F4A-4F77-8239-7797A84AD7C0}']
    {$ENDIF}

    {$REGION IGIS_HierarchyGroup property access routines}
      function  fget_GroupsCount : Integer ;
      function  fget_LayersCount : Integer ;
      function  fget_Group       ( const _indexOrName : OleVariant
                                 ) : IGIS_HierarchyGroup ;
      function  fget_Layer       ( const _index : Integer
                                 ) : TGIS_LayerAbstract ;
      function  fget_Collapsed   : Boolean ;
      procedure fset_Collapsed   ( const _value : Boolean
                                 ) ;
      function  fget_Active      : Boolean ;
      procedure fset_Active      ( const _value : Boolean
                                 ) ;
      function  fget_Caption     : String ;
      procedure fset_Caption     ( const _value : String
                                 ) ;
      function  fget_Name        : String ;
      function  fget_IsModified  : Boolean ;
      function  fget_ParentGroup : IGIS_HierarchyGroup ;
      procedure fset_ParentGroup ( const _group : IGIS_HierarchyGroup
                                 ) ;
      function  fget_Manager     : IGIS_HierarchyManager ;
      procedure fset_Manager     ( const _manager : IGIS_HierarchyManager
                                 ) ;
    {$ENDREGION}

    {$REGION IGIS_HierarchyGroup public methods}
      /// <summary>
      ///   Create a new sub group for this group .
      /// </summary>
      /// <param name="_name">
      ///   group name (must be unique)
      /// </param>
      /// <returns>
      ///   group
      /// </returns>
      function  CreateGroup    ( const _name : String
                               ) : IGIS_HierarchyGroup ;
      /// <summary>
      ///   Delete all sub groups.
      /// </summary>
      procedure ClearGroups ;

      /// <summary>
      ///   Delete a group from the group list.
      /// </summary>
      /// <param name="_name">
      ///   group name
      /// </param>
      procedure DeleteGroup    ( const _name : String
                               ) ;
      /// <summary>
      ///   Remove a group from the group list.
      /// </summary>
      /// <param name="_group">
      ///   existing group handle
      /// </param>
      procedure RemoveGroup    ( const _group : IGIS_HierarchyGroup
                               ) ;
      /// <summary>
      ///   Add a sub group to the group list.
      /// </summary>
      /// <param name="_group">
      ///   existing group handle
      /// </param>
      {#ownership:_group:release}
      procedure AddGroup       ( const _group : IGIS_HierarchyGroup
                               ) ;
      /// <summary>
      ///   Insert a sub group to the group list at insert index.
      /// </summary>
      /// <param name="_group">
      ///   existing group handle
      /// </param>
      /// <param name="_index">
      ///   insert index
      /// </param>
      {#ownership:group:release}
      procedure InsertGroup    ( const _group : IGIS_HierarchyGroup ;
                                 const _index : Integer
                               ) ;
      /// <summary>
      ///   Add a layer to the group.
      /// </summary>
      /// <param name="_layer">
      ///   layer handle
      /// </param>
      procedure AddLayer       ( const _layer : TGIS_LayerAbstract
                               ) ;
      /// <summary>
      ///   Insert a layer to the group as index position.
      /// </summary>
      /// <param name="_layer">
      ///   layer handle
      /// </param>
      /// <param name="_index">
      ///   insert index
      /// </param>
      procedure InsertLayer    ( const _layer : TGIS_LayerAbstract ;
                                 const _index : Integer
                               ) ;
      /// <summary>
      ///   Delete a layer from a group.
      /// </summary>
      /// <param name="_layer">
      ///   layer handle
      /// </param>
      procedure DeleteLayer    ( const _layer : TGIS_LayerAbstract
                               ) ;
    {$ENDREGION}

    {$REGION IGIS_HierarchyGroup properties}
      /// <summary>
      ///   Get group by name.
      /// </summary>
      /// <param name="_indexOrName">
      ///   group index or name
      /// </param>
      property Groups[ const _indexOrName : OleVariant ]   : IGIS_HierarchyGroup
                                                  read {$IFNDEF OXYGENE} fget_Group {$ENDIF} ;
      /// <summary>
      ///   Get layer by index.
      /// </summary>
      /// <param name="_index">
      ///   group index
      /// </param>
      property Layers[ const _index : Integer ] : TGIS_LayerAbstract
                                                  read {$IFNDEF OXYGENE} fget_Layer {$ENDIF} ;
      /// <summary>
      ///   Group name.
      /// </summary>
      property Name                             : String  read  {$IFNDEF OXYGENE} fget_Name {$ENDIF} ;
      /// <summary>
      ///   Group caption.
      /// </summary>
      property Caption                          : String  read  {$IFNDEF OXYGENE} fget_Caption {$ENDIF}
                                                  write {$IFNDEF OXYGENE} fset_Caption {$ENDIF} ;
      /// <summary>
      ///   Is group collapsed.
      /// </summary>
      property Collapsed                        : Boolean  read  {$IFNDEF OXYGENE} fget_Collapsed {$ENDIF}
                                                  write {$IFNDEF OXYGENE} fset_Collapsed {$ENDIF} ;
      /// <summary>
      ///   Is group active.
      /// </summary>
      property Active                           : Boolean  read  {$IFNDEF OXYGENE} fget_Active {$ENDIF}
                                                  write {$IFNDEF OXYGENE} fset_Active {$ENDIF} ;
      /// <summary>
      ///   Groups count.
      /// </summary>
      property GroupsCount                      : Integer read  {$IFNDEF OXYGENE} fget_GroupsCount {$ENDIF} ;

      /// <summary>
      ///   Layers count.
      /// </summary>
      property LayersCount                      : Integer read  {$IFNDEF OXYGENE} fget_LayersCount {$ENDIF} ;

      /// <summary>
      ///   Is group modified.
      /// </summary>
      property IsModified                       : Boolean read  {$IFNDEF OXYGENE} fget_IsModified {$ENDIF} ;

      /// <summary>
      ///   Parent group.
      /// </summary>
      property ParentGroup                      : IGIS_HierarchyGroup
                                                  read  {$IFNDEF OXYGENE} fget_ParentGroup {$ENDIF}
                                                  write {$IFNDEF OXYGENE} fset_ParentGroup {$ENDIF} ;
      /// <summary>
      ///   Handle to hierarchy manager.
      /// </summary>
      property Manager                          : IGIS_HierarchyManager
                                                  read  {$IFNDEF OXYGENE} fget_Manager {$ENDIF}
                                                  write {$IFNDEF OXYGENE} fset_Manager {$ENDIF} ;
    {$ENDREGION}
  end;

  /// <summary>
  ///   <para>
  ///     Hierarchy manager class.
  ///   </para>
  ///   <para>
  ///     The hierarchy data contain a hierarchy list and additional hierarchy
  ///     groups that are saved in a project file under [TatukGIS Hierarchy]
  ///     and [TatukGIS GroupN] sections.
  ///   </para>
  ///   <para>
  ///     The hierarchy list section has a format like:
  ///   </para>
  /// </summary>
  IGIS_HierarchyManager = {$IFDEF OXYGENE} public partial {$ENDIF} interface( IInterface )
    {$IFDEF DCC}
      ['{6D977B07-CF7D-46DB-A2F3-6AFA8E90EC2B}']
    {$ENDIF}

    {$REGION IGIS_HierarchyManager property access routines}
      function  fget_GroupsCount: Integer ;
      function  fget_Group      ( const _indexOrName : OleVariant
                                ) : IGIS_HierarchyGroup ;
      procedure fset_IsModified ( const _val     : Boolean
                                ) ;
      function  fget_IsModified : Boolean ;
    {$ENDREGION}

    {$REGION IGIS_HierarchyManager public methods}

      /// <summary>
      ///   Create a group.
      /// </summary>
      /// <param name="_name">
      ///   group name
      /// </param>
      /// <returns>
      ///   group
      /// </returns>
      function  CreateGroup     ( const _name    : String
                                ) : IGIS_HierarchyGroup ;

      /// <summary>
      ///   Delete all groups.
      /// </summary>
      procedure ClearGroups     ;

      /// <summary>
      ///   Delete group with sub groups.
      /// </summary>
      /// <param name="_name">
      ///   group name
      /// </param>
      procedure DeleteGroup     ( const _name    : String
                                ) ;

      /// <summary>
      ///   Move group to another group.
      /// </summary>
      /// <param name="_source">
      ///   source group name
      /// </param>
      /// <param name="_target">
      ///   target group name
      /// </param>
      /// <remarks>
      ///   A group will be removed from _source parent and moved to _target
      ///   group.
      /// </remarks>
      procedure MoveGroup       ( const _source  : String ;
                                  const _target  : String
                                ) ;

      /// <summary>
      ///   Move group to another group.
      /// </summary>
      /// <param name="_source">
      ///   source group name
      /// </param>
      /// <param name="_target">
      ///   target group name
      /// </param>
      /// <param name="_index">
      ///   insert position; if -1 then will be added as last
      /// </param>
      /// <remarks>
      ///   A group will be removed from _source parent and moved to _target
      ///   group.
      /// </remarks>
      procedure MoveGroupEx     ( const _source  : String ;
                                  const _target  : String ;
                                  const _index   : Integer
                                ) ;
      /// <summary>
      ///   Add existing group to another group.
      /// </summary>
      /// <param name="_group">
      ///   group handle
      /// </param>
      procedure AddGroup        ( const _group   : IGIS_HierarchyGroup
                                ) ;

      /// <summary>
      ///   Add existing group to another group at indexed position.
      /// </summary>
      /// <param name="_group">
      ///   group handle
      /// </param>
      /// <param name="_index">
      ///   insert position
      /// </param>
      /// <remarks>
      ///   This method doesn't move a group and should be used to insert new
      ///   groups.
      /// </remarks>
      procedure InsertGroup     ( const _group   : IGIS_HierarchyGroup ;
                                  const _index   : Integer
                                 ) ;
      /// <summary>
      ///   Add a layer to groups.
      /// </summary>
      /// <param name="_layer">
      ///   layer handle
      /// </param>
      procedure AddLayer        ( const _layer   : TGIS_LayerAbstract
                                ) ;

      /// <summary>
      ///   Add a layer to groups.
      /// </summary>
      /// <param name="_layer">
      ///   layer handle
      /// </param>
      /// <param name="_index">
      ///   insert index
      /// </param>
      procedure InsertLayer     ( const _layer   : TGIS_LayerAbstract ;
                                  const _index   : Integer
                                 ) ;
      /// <summary>
      ///   Delete layer from groups.
      /// </summary>
      /// <param name="_layer">
      ///   layer handle
      /// </param>
      procedure DeleteLayer     ( const _layer   : TGIS_LayerAbstract
                                ) ;

      /// <summary>
      ///   Move a layer from a group to another.
      /// </summary>
      /// <param name="_from">
      ///   source group name
      /// </param>
      /// <param name="_to">
      ///   destination group name
      /// </param>
      /// <param name="_layer">
      ///   layer handle
      /// </param>
      procedure MoveLayer       ( const _from    : String ;
                                  const _to      : String ;
                                  const _layer   : TGIS_LayerAbstract
                                ) ;

      /// <summary>
      ///   Add layers from the viewer (not used in the hierarchy list) to the
      ///   hierarchy.
      /// </summary>
      /// <remarks>
      ///   A new group called Other will be created and filled with these
      ///   layers.
      /// </remarks>
      procedure AddOtherLayers  ;

      /// <summary>
      ///   Parse hierarchy list and build groups with layers.
      /// </summary>
      /// <param name="_hierarchy">
      ///   string
      /// </param>
      procedure ParseHierarchy   ( const _hierarchy : String
                                 ) ; overload;

      /// <summary>
      ///   Parse hierarchy list and build groups with layers.
      /// </summary>
      /// <param name="_hierarchy">
      ///   string list
      /// </param>
      /// <param name="_format">
      ///   hierarchy format
      /// </param>
      procedure ParseHierarchy( const _hierarchy : {$IFDEF OXYGENE}
                                                      TGIS_Strings
                                                    {$ELSE}
                                                      TStrings
                                                    {$ENDIF} ;
                                 const _format : TGIS_ConfigFormat
                               ) ; overload;

      /// <summary>
      ///   Load hierarchy from config.
      /// </summary>
      /// <param name="_config">
      ///   config handle
      /// </param>
      procedure LoadHierarchy   ( const _config : TGIS_ConfigAbstract
                                ) ;
      /// <summary>
      ///   Get hierarchy list.
      /// </summary>
      /// <param name="_format">
      ///   config format
      /// </param>
      /// <returns>
      ///   hierarchy text
      /// </returns>
      function  GetHierarchy    ( const _format : TGIS_ConfigFormat
                                ) : String ;
      /// <summary>
      ///   Get aliases list.
      /// </summary>
      /// <returns>
      ///   groups text
      /// </returns>
      function  GetGroups       : String ;
    {$ENDREGION}

    {$REGION IGIS_HierarchyManager properties}

      /// <summary>
      ///   Get group by name.
      /// </summary>
      /// <param name="_indexOrName">
      ///   group index or name
      /// </param>
      property Groups[ const _indexOrName : OleVariant ] : IGIS_HierarchyGroup
                                                read {$IFNDEF OXYGENE} fget_Group {$ENDIF} ;

      /// <summary>
      ///   Groups count.
      /// </summary>
      property GroupsCount      : Integer        read {$IFNDEF OXYGENE} fget_GroupsCount {$ENDIF} ;

      /// <summary>
      ///   Is hierarchy modified.
      /// </summary>
      property IsModified       : Boolean        read  {$IFNDEF OXYGENE} fget_IsModified {$ENDIF}
                                                 write {$IFNDEF OXYGENE} fset_IsModified {$ENDIF} ;
    {$ENDREGION}
  end ;


  IGIS_ViewerParent = {$IFDEF OXYGENE} public partial {$ENDIF} interface ;

  /// <summary>
  ///   Common API for viewer operations. To be shared across platforms.
  /// </summary>
  IGIS_Viewer = {$IFDEF OXYGENE} public {$ENDIF} interface( IInterface )
    {$IFDEF DCC}
      ['{1EE91EC2-77AA-4131-8B76-8EE254CC74D7}']
    {$ENDIF}

    {$REGION IGIS_Viewer property access routines}
      function  fget_AutoStyle          : Boolean;
      procedure fset_AutoStyle          ( const _value : Boolean ) ;
      function  fget_BigExtent          : TGIS_Extent ;
      function  fget_BigExtentMargin    : Integer     ;
      procedure fset_BigExtentMargin    ( const _value : Integer     ) ;
      function  fget_KeepScale          : Boolean ;
      procedure fset_KeepScale          ( const _value : Boolean ) ;
      function  fget_BusyLevel          : Integer     ;
      function  fget_BusyText           : String      ;
      function  fget_Center             : TGIS_Point  ;
      procedure fset_Center             ( const _value : TGIS_Point  ) ;
      function  fget_CenterPtg          : TGIS_Point  ;
      procedure fset_CenterPtg          ( const _value : TGIS_Point  ) ;
      function  fget_Color              : TGIS_Color  ;
      procedure fset_Color              ( const _value : TGIS_Color  ) ;
      function  fget_Copyright          : String      ;
      function  fget_CS                 : TGIS_CSCoordinateSystem ;
      procedure fset_CS                 ( const _value : TGIS_CSCoordinateSystem ) ;
      function  fget_CustomData         : TGIS_StringList ;
      function  fget_CustomPPI          : Integer ;
      procedure fset_CustomPPI          ( const _value : Integer     ) ;
      function  fget_Editor             : IGIS_Editor ;
      procedure fset_Editor             ( const _value : IGIS_Editor ) ;
      function  fget_Extent             : TGIS_Extent ;
      function  fget_FileCopyrights     : String      ;
      function  fget_FontScale          : Integer ;
      procedure fset_FontScale          ( const _value : Integer     ) ;
      function  fget_FullDrawExtent     : TGIS_Extent ;
      function  fget_Hierarchy          : IGIS_HierarchyManager ;
      function  fget_IncrementalPaint   : Boolean     ;
      procedure fset_IncrementalPaint   ( const _value : Boolean     ) ;
      function  fget_TiledPaint         : Boolean     ;
      procedure fset_TiledPaint         ( const _value : Boolean     ) ;
      function  fget_InPaint            : Boolean     ;
      function  fget_IsBusy             : Boolean     ;
      function  fget_IsEmpty            : Boolean     ;
      function  fget_IsLocked           : Boolean     ;
      function  fget_IsTopmost          : Boolean     ;
      function  fget_Items              : TGIS_LayerAbstractList ;
      function  fget_LabelsReg          : TGIS_LabelsAreaAbstract ;
      function  fget_Level              : Double      ;
      procedure fset_Level              ( const _value : Double      ) ;
      function  fget_MultiUserMode      : TGIS_MultiUser ;
      procedure fset_MultiUserMode      ( const _value : TGIS_MultiUser ) ;
      function  fget_OverlappedExtentMargin
                                        : Integer     ;
      procedure fset_OverlappedExtentMargin
                                        ( const _value : Integer     ) ;
      function  fget_PPI                : Integer ;
      function  fget_ProjectFile        : TGIS_ConfigAbstract ;
      procedure fset_ProjectFile        ( const _value : TGIS_ConfigAbstract ) ;
      function  fget_ProjectName        : String      ;
      function  fget_DelayedUpdate      : Integer     ;
      procedure fset_DelayedUpdate      ( const _value : Integer     ) ;
      function  fget_ProgressiveUpdate  : Integer     ;
      procedure fset_ProgressiveUpdate  ( const _value : Integer     ) ;
      function  fget_RestrictedDrag     : Boolean     ;
      procedure fset_RestrictedDrag     ( const _value : Boolean     ) ;
      function  fget_RestrictedExtent   : TGIS_Extent ;
      procedure fset_RestrictedExtent   ( const _value : TGIS_Extent ) ;
      function  fget_RotationAngle      : Double      ;
      procedure fset_RotationAngle      ( const _value : Double      ) ;
      function  fget_RotationPoint      : TGIS_Point  ;
      procedure fset_RotationPoint      ( const _value : TGIS_Point  ) ;
      function  fget_ScaleAsFloat       : Double      ;
      procedure fset_ScaleAsFloat       ( const _value : Double      ) ;
      function  fget_ScaleAsText        : String      ;
      procedure fset_ScaleAsText        ( const _value : String      ) ;
      function  fget_SelectionGisColor  : TGIS_Color  ;
      procedure fset_SelectionGisColor  ( const _value : TGIS_Color  ) ;
      function  fget_SelectionOutlineOnly: Boolean    ;
      procedure fset_SelectionOutlineOnly
                                        ( const _value : Boolean     ) ;
      function  fget_SelectionTransparency
                                        : Integer  ;
      procedure fset_SelectionTransparency
                                        ( const _value : Integer     ) ;
      function  fget_SelectionWidth     : Integer     ;
      procedure fset_SelectionWidth     ( const _value : Integer     ) ;
      function  fget_SystemPPI          : Integer ;
      function  fget_TemporaryScaleInternal
                                        : Double      ;
      procedure fset_TemporaryScaleInternal
                                        ( const _value : Double      ) ;
      function  fget_TemporaryVisibleExtent
                                        : TGIS_Extent ;
      procedure fset_TemporaryVisibleExtent
                                        ( const _value : TGIS_Extent ) ;
      function  fget_ViewerParent       : IGIS_ViewerParent ;
      function  fget_ViewerParentRoot   : IGIS_ViewerParent ;
      function  fget_Viewport           : TGIS_Point  ;
      procedure fset_Viewport           ( const _value : TGIS_Point  ) ;
      function  fget_VisibleExtent      : TGIS_Extent ;
      procedure fset_VisibleExtent      ( const _value : TGIS_Extent ) ;
      function  fget_UponDestroy        : Boolean     ;
      function  fget_UseRTree           : Boolean     ;
      procedure fset_UseRTree           ( const _value : Boolean     ) ;
      function  fget_UseAnimations      : Boolean     ;
      procedure fset_UseAnimations      ( const _value : Boolean     ) ;
      function  fget_Zoom               : Double      ;
      procedure fset_Zoom               ( const _value : Double      ) ;
      function  fget_ZoomEx             : Double      ;
      procedure fset_ZoomEx             ( const _value : Double      ) ;

      function  fget_MasterViewer       : IGIS_Viewer ;
      procedure fset_MasterViewer       ( const _value : IGIS_Viewer ) ;

    {$ENDREGION}

    {$REGION IGIS_Viewer public methods}
      /// <summary>
      ///   Change hash. Uses to verify if order of layers, params, etc.
      ///   has been modified. Used mainly by TGIS_ControlLegend.
      /// </summary>
      /// <returns>
      ///   Calculated hash value.
      /// </returns>
      function  ChangeHash          : Int64 ;


      /// <summary>
      ///   Add provided control to the notification subscription list.
      /// </summary>
      /// <param name="_control">
      ///   - control which should become a subscriber
      /// </param>
      procedure Subscribe           ( const _control : IGIS_Subscribe
                                    ) ;

      /// <summary>
      ///   Remove provided control from the notification subscription list.
      /// </summary>
      /// <param name="_control">
      ///   control which subscription should be canceled
      /// </param>
      procedure UnSubscribe         ( const _control : IGIS_Subscribe
                                    ) ;

      /// <summary>
      ///   Send notification to subscribers.
      /// </summary>
      /// <param name="_event">
      ///   event identifying number
      /// </param>
      /// <param name="_context">
      ///   control context; what is expected depends on _event;
      ///   it can be canvas, control itself.
      /// </param>
      procedure NotifySubscribers   ( const _event   : Integer ;
                                      const _context : TObject
                                    ) ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK} { TODO -cPDK : Verify }
      /// <summary>
      ///   Transform underlying exception into TGIS_Viewer.OnPaintException
      /// </summary>
      /// <param name="_message">
      ///   exception message, usually layer on which exception occurred
      /// </param>
      /// <param name="_exception">
      ///   original exception object
      /// </param>
      /// <returns>
      ///    True if event attached; false if not
      /// </returns>
      function NotifyPaintException ( const _message    : String ;
                                      const _exception  : Exception
                                    ) : Boolean ;
      /// <summary>
      ///   Lock viewer.
      /// </summary>
      /// <remarks>
      ///   Prevent any screen updates. Use this method like for shapes. For
      ///   some layers it can be time-consuming operation when unlocking
      ///   viewer.
      /// </remarks>
      procedure Lock                ;

      /// <summary>
      ///   Unlock viewer.
      /// </summary>
      /// <remarks>
      ///   Do screen refresh via Reposition. Use this method like for
      ///   shapes. See TGIS_Shape.Lock for similar example.
      /// </remarks>
      procedure Unlock              ; overload;


      /// <summary>
      ///   Unlock the viewer but do not redraw
      /// </summary>
      /// <param name="_redraw">
      ///   If true then map should be redraw upon unlock; if false then redraw
      ///   should be issued.
      /// </param>
      procedure Unlock              ( const _redraw  : Boolean
                                    ) ; overload;

      /// <summary>
      ///   Will terminate timely operation as soon as possible.
      /// </summary>
      /// <remarks>
      ///   Application can check for abort state by querying HourglassShake.
      /// </remarks>
      procedure Interrupt           ;

      /// <summary>
      ///   Test if any pending operation must be aborted.
      /// </summary>
      /// <returns>
      ///   True is if any lengy;lu operation must be aborted.
      /// </returns>
      function  Interrupted        : Boolean ;

      /// <summary>
      ///   Test is hourglass is active.
      /// </summary>
      /// <returns>
      ///   True is Hourglass is visible and Interrupt can be called.
      /// </returns>
      function  HourglassActive     : Boolean ;

      /// <summary>
      ///   Prepare Hourglass cursor for timely operation.
      /// </summary>
      /// <remarks>
      ///   Hourglass will be turn on after a while (500 ms). Must be paired
      ///   with HourglassRelease. Hourglass must be shake regularly (for
      ///   example in any loop) to visible) - use HourglassShake to do this.
      /// </remarks>
      procedure HourglassPrepare    ;

      /// <summary>
      ///   Release Hourglass.
      /// </summary>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BAD_CALL
      /// </exception>
      /// <remarks>
      ///   see HourglassPrepare.
      /// </remarks>
      procedure HourglassRelease    ;

      /// <summary>
      ///   Shake Hourglass.
      /// </summary>
      /// <remarks>
      ///   Shaking hourglass means waking it up. See HourglassPrepare.
      /// </remarks>
      /// <returns>
      ///   If True then current operation should be interrupted.
      /// </returns>
      function  HourglassShake      : Boolean ;

      /// <summary>
      ///   Restart hourglass timing used for progressive updates.
      /// </summary>
      procedure HourglassRestart ;

      /// <summary>
      ///   Prepare BusyEvent for long-term operation.
      /// </summary>
      /// <param name="_sender">
      ///   component which is causing long-term operations
      /// </param>
      /// <param name="_text">
      ///   text to appear in a BusyText property; predefined
      ///   GIS_BUSY_NOEVENTS and GIS_BUSY_NOHOURGLASS could be used to
      ///   define "silent" busy state; in such state BusyEvent will not
      ///   be called and, in a case of GIS_BUSY_NOHOURGLASS, hourglass will
      ///   not be displayed and check for interrupt as well.
      /// </param>
      /// <remarks>
      ///   Do not use during OnPaint event directly. Must be paired with
      ///   BusyRelease. The actual state can be obtained from IsBusy
      ///   property.
      /// </remarks>
      procedure BusyPrepare         (       _sender  : TObject ;
                                            _text    : String
                                    ) ;

      /// <summary>
      ///   Release Busy state.
      /// </summary>
      /// <param name="_sender">
      ///   component which is causing long-term operations
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BAD_CALL
      /// </exception>
      /// <remarks>
      ///   see BusyPrepare.
      /// </remarks>
      procedure BusyRelease         (       _sender  : TObject
                                    ) ;

      /// <summary>
      ///   Shake Busy state.
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
      /// <param name="_abort">
      ///   if set to True inside message handler then an abort request.
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BAD_CALL
      /// </exception>
      /// <remarks>
      ///   <para>
      ///     For percent of completion calculation you should provide
      ///     proper _pos and _end values. If you can not calculate these
      ///     values - please provide 0.
      ///   </para>
      ///   <para>
      ///      See BusyPrepare.
      ///   </para>
      /// </remarks>
      procedure BusyShake           (       _sender  : TObject ;
                                            _pos     : Int64 ;
                                            _end     : Int64 ;
                                      var   _abort   : Boolean
                                    ) ;
      {$IFDEF OXYGENE}
        /// <summary>
        ///   Fire Busy event of a viewer.
        /// </summary>
        /// <param name="_sender">
        ///   component which is causing long-term operations
        /// </param>
        /// <param name="_e">
        ///   current progress, maximal progress value, an abort request.
        /// </param>
        procedure RaiseBusyEvent    (       _sender  : TObject ;
                                            _e       : TGIS_BusyEventArgs
                                    ) ;
      {$ELSE}
        /// <summary>
        ///   Fire Busy event of a viewer.
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
        /// <param name="_abort">
        ///   if set to True inside message handler then an abort request.
        /// </param>
        procedure RaiseBusyEvent    (       _sender  : TObject ;
                                            _pos     : Int64 ;
                                            _end     : Int64 ;
                                      var   _abort   : Boolean
                                    ) ;
      {$ENDIF}


      {$IFDEF OXYGENE}
        /// <summary>
        ///   Fire Busy event of a viewer.
        /// </summary>
        /// <param name="_sender">
        ///   component asking for help
        /// </param>
        /// <param name="_e">
        ///   help context
        /// </param>
        procedure RaiseHelpEvent    (       _sender  : TObject ;
                                            _e       : TGIS_HelpEventArgs
                                    ) ;
      {$ELSE}
        /// <summary>
        ///   Fire Busy event of a viewer.
        /// </summary>
        /// <param name="_sender">
        ///   component asking for help
        /// </param>
        /// <param name="_name">
        ///   help context
        /// </param>
        procedure RaiseHelpEvent    (       _sender  : TObject ;
                                            _name    : String
                                    ) ;
      {$ENDIF}

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Reference to a help event handler assigned to the Viewer.
      /// </summary>
      /// <returns>
      ///   Event handler.
      /// </returns>
      function AssignedBusyEvent  : TGIS_BusyEvent ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Reference to a busy event handler assigned to the Viewer.
      /// </summary>
      /// <returns>
      ///   Event handler.
      /// </returns>
      function AssignedHelpEvent : TGIS_HelpEvent ;

      /// <summary>
      ///   Store the current paint state.
      /// </summary>
      /// <remarks>
      ///   Utility function for printing methods.
      ///   See also RestorePaintState.
      /// </remarks>
      /// <returns>
      ///   Stored paint state.
      /// </returns>
      function  StorePaintState     : TObject ;

      /// <summary>
      ///   Restore and free the current paint state.
      /// </summary>
      /// <param name="_state">
      ///   paint state to be restored
      /// </param>
      /// <remarks>
      ///   Utility function for printing methods.
      ///   See storePaintState.
      /// </remarks>
      procedure RestorePaintState   ( var _state    : TObject
                                    ) ;

      /// <summary>
      ///   Notify viewer about entering of a paint mode.
      /// </summary>
      procedure BeginPaintInternal  ;

      /// <summary>
      ///   Notify viewer about leaving of a paint mode.
      /// </summary>
      procedure EndPaintInternal    ;


      /// <summary>
      ///   Wait for pending paint operation to be finalized.
      /// </summary>
      /// <param name="_interrupt">
      ///   interrupts any pending operation
      /// </param>
      /// <returns>
      ///   True if operation were synchronized; False if synchronization
      ///   is not possible and current operation should not continue.
      /// </returns>
      function SynchronizePaint     ( const _interrupt : Boolean
                                    ) : Boolean ;

      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      /// <summary>
      ///   Block reparenting.
      /// </summary>
      /// <remarks>
      ///   Only for internal use of TatukGIS.
      /// </remarks>
      procedure ReParentLock;

      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      /// <summary>
      ///   Unblock reparenting.
      /// </summary>
      /// <remarks>
      ///   Only for internal use of TatukGIS.
      /// </remarks>
      procedure ReParentUnlock;

      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      /// <summary>
      ///   Set a new parent for the viewer.
      /// </summary>
      /// <param name="_parent">
      ///   new parent object
      /// </param>
      /// <returns>
      ///   Previous parent.
      /// </returns>
      /// <remarks>
      ///   Only for internal use of TatukGIS.
      /// </remarks>
      function  ReParent            ( const _parent  : IGIS_ViewerParent
                                    ) : IGIS_ViewerParent ;

      /// <summary>
      ///   Attach layer to the viewer by re-parenting.
      /// </summary>
      /// <param name="_layer">
      ///   layer to be attached
      /// </param>
      /// <returns>
      ///   Previous parent.
      /// </returns>
      /// <remarks>
      ///   Only for internal use of TatukGIS.
      /// </remarks>
      function  AttachLayer         ( const _layer : TGIS_LayerAbstract
                                    ) : IGIS_Viewer ;


      /// <summary>
      ///   Open project.
      /// </summary>
      /// <param name="_path">
      ///   project path (or layer)
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_LAYERMISSED if not all layers can be opened.
      /// </exception>
      /// <remarks>
      ///   If any project exists, then it will be closed and any stored
      ///   passwords will be cleared. If _path content is a path to any
      ///   registered layer type, then layer will be opened.
      /// </remarks>
      procedure Open                ( const _path    : String
                                    ) ; overload;

      /// <summary>
      ///   Open project.
      /// </summary>
      /// <param name="_path">
      ///   project path (or layer)
      /// </param>
      /// <param name="_strict">
      ///   if False then non existing layers will be ignored (only while
      ///   opening project files); default is True ;
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_LAYERMISSED if not all layers can be opened.
      /// </exception>
      /// <remarks>
      ///   If any project exists, then it will be closed and any stored
      ///   passwords will be cleared. If _path content is a path to any
      ///   registered layer type, then layer will be opened.
      /// </remarks>
      procedure Open                ( const _path    : String ;
                                      const _strict  : Boolean
                                    ) ; overload;

      /// <summary>
      ///   Open project file from the memory.
      /// </summary>
      /// <param name="_configFile">
      ///   project file
      /// </param>
      /// <param name="_path">
      ///   project path (or layer)
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_LAYERMISSED if not all layers can be opened.
      /// </exception>
      /// <remarks>
      ///   If any project exists, then it will be closed and any stored
      ///   passwords will be cleared. If _path content is a path to any
      ///   registered layer type, then layer will be opened.
      /// </remarks>
      procedure OpenEx              ( const _configFile : TGIS_ConfigAbstract ;
                                      const _path    : String
                                    ) ; overload;

      /// <summary>
      ///   Open project file from the memory.
      /// </summary>
      /// <param name="_configFile">
      ///   project file
      /// </param>
      /// <param name="_path">
      ///   project path (or layer)
      /// </param>
      /// <param name="_strict">
      ///   if False then non existing layers will be ignored (only while
      ///   opening project files); default is True ;
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_LAYERMISSED if not all layers can be opened.
      /// </exception>
      /// <remarks>
      ///   If any project exists, then it will be closed and any stored
      ///   passwords will be cleared. If _path content is a path to any
      ///   registered layer type, then layer will be opened.
      /// </remarks>
      procedure OpenEx              ( const _configFile : TGIS_ConfigAbstract ;
                                      const _path    : String ;
                                      const _strict  : Boolean
                                    ) ; overload;
      /// <summary>
      ///   Close a project.
      /// </summary>
      /// <remarks>
      ///   See Open for example.
      /// </remarks>
      procedure Close               ;

      /// <summary>
      ///   Read all configuration data from project.
      /// </summary>
      /// <remarks>
      ///   This method is called internally in Open method while working
      ///   with projects. Primary parameters will be read. Screen will be
      ///   invalidated.
      /// </remarks>
      procedure ReadConfig          ;

      /// <summary>
      ///   Reread all configuration data from project and layer
      ///   configuration files.
      /// </summary>
      /// <remarks>
      ///   Screen will be invalidated ;
      /// </remarks>
      procedure RereadConfig        ;

      /// <summary>
      ///   Write all configuration data to the project or layer ini file.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Layer configuration will be saved to project or configuration
      ///    files if such configuration file exits.
      ///    </note>
      ///   This method is called internally in Save or SaveAll method while
      ///   working with projects. See ReadConfig for example.
      /// </remarks>
      procedure WriteConfig         ;

      /// <summary>
      ///   Add a layer to the Viewer.
      /// </summary>
      /// <param name="_layer">
      ///   layer to be added
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_LAYEREXIST
      /// </exception>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_LAYERBADEXTENT
      /// </exception>
      /// <remarks>
      ///   Use this method to add a new custom layer to the viewer. For
      ///   some layers (on-disk) it's recommended to assign a layer extent
      ///   earlier to avoid its recalculation.
      /// </remarks>
      {#ownership:_layer:release}
      procedure Add                 ( const _layer   : TGIS_LayerAbstract
                                    ) ;

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
      function  Get                 ( const _name    : String
                                    ) : TGIS_LayerAbstract ;

      /// <summary>
      ///   Delete the layer identified by a name.
      /// </summary>
      /// <param name="_name">
      ///   name of layer to be deleted
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_LAYERNOEXIST
      /// </exception>
      /// <remarks>
      ///   See Get method for example.
      /// </remarks>
      procedure Delete              ( const _name    : String
                                    ) ;

      /// <summary>
      ///   Read hierarchy and build groups with layers.
      /// </summary>
      procedure AddHierarchy        ;

      /// <summary>
      ///   Draw  all layers on a current renderer.
      /// </summary>
      /// <param name="_renderer">
      ///   renderer context
      /// </param>
      /// <param name="_mode">
      ///   drawing mode: which set of layers should be rendered
      /// </param>
      procedure Draw                ( const _renderer : TObject ;
                                      const _mode     : TGIS_DrawMode
                                    ) ;

      /// <summary>
      ///   Fills provided _grid array with values defined by _extent. If
      ///   extent is not fully covered by the gird layers then values
      ///   outside the layers scope will be left untouched.
      /// </summary>
      /// <param name="_extent">
      ///   extent of the _grid
      /// </param>
      /// <param name="_grid">
      ///   allocated array (with/height ratio of the _grid should be the
      ///   same as width/height ration of the _extent)
      /// </param>
      /// <returns>
      ///   True if success.
      /// </returns>
      function  GetGrid             ( const _extent  : TGIS_Extent  ;
                                      const _grid    : TGIS_GridArray
                                    ) : Boolean ;


      /// <summary>
      ///   Revert all layers to a file-based original.
      /// </summary>
      /// <remarks>
      ///   All unsaved shapes will be deleted. See TGIS_Shape.MakeEditable.
      /// </remarks>
      procedure RevertAll           ;

      /// <summary>
      ///   Save current project.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Layer configuration will be saved to project or configuration
      ///    files if such configuration file exits.
      ///    </note>
      ///   Save only project file. Data will not be saved.
      /// </remarks>
      procedure SaveProject         ; overload;

      /// <summary>
      ///   Save current project.
      /// </summary>
      /// <param name="_relativepath">
      ///   if True (default) then project will be saved using paths
      ///   relative to project file location
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Layer configuration will be saved to project or configuration
      ///    files if such configuration file exits.
      ///    </note>
      ///   Save only project file. Data will not be saved.
      /// </remarks>
      procedure SaveProject         ( const _relativepath : Boolean
                                    ) ; overload;

      /// <summary>
      ///   Save current project under new name.
      /// </summary>
      /// <param name="_path">
      ///   path to the new project file; should have '.ttkproject'
      ///   (or obsolete '.ttkgp') extension
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Layer configuration will be saved to project or configuration
      ///    files if such configuration file exits.
      ///    </note>
      ///   Save only project file. Data will not be saved.
      /// </remarks>
      procedure SaveProjectAs       ( const _path         : String
                                    ) ; overload;

      /// <summary>
      ///   Save current project under new name.
      /// </summary>
      /// <param name="_path">
      ///   path to the new project file; should have '.ttkproject'
      ///   (or obsolete '.ttkgp') extension
      /// </param>
      /// <param name="_relativepath">
      ///   if True (default) then project will be saved using paths
      ///   relative to project file location
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Layer configuration will be saved to project or configuration
      ///    files if such configuration file exits.
      ///    </note>
      ///   Save only project file. Data will not be saved.
      /// </remarks>
      procedure SaveProjectAs       ( const _path         : String          ;
                                      const _relativepath : Boolean
                                    ) ; overload;

      /// <summary>
      ///   Save current project under new name.
      /// </summary>
      /// <param name="_configFile">
      ///   existing project file
      /// </param>
      /// <param name="_path">
      ///   path to the new project file; should have '.ttkproject'
      ///   (or obsolete '.ttkgp') extension
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Layer configuration will be saved to project or configuration
      ///    files if such configuration file exits.
      ///    </note>
      ///   Save only project file. Data will not be saved.
      /// </remarks>
      procedure SaveProjectAsEx     ( const _configFile   : TGIS_ConfigAbstract ;
                                      const _path         : String
                                    ) ; overload;

      /// <summary>
      ///   Save current project under new name.
      /// </summary>
      /// <param name="_configFile">
      ///   existing project file
      /// </param>
      /// <param name="_path">
      ///   path to the new project file; should have '.ttkproject'
      ///   (or obsolete '.ttkgp') extension
      /// </param>
      /// <param name="_relativepath">
      ///   if True (default) then project will be saved using paths
      ///   relative to project file location
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Layer configuration will be saved to project or configuration
      ///    files if such configuration file exits.
      ///    </note>
      ///   Save only project file. Data will not be saved.
      /// </remarks>
      procedure SaveProjectAsEx     ( const _configFile   : TGIS_ConfigAbstract ;
                                      const _path         : String              ;
                                      const _relativepath : Boolean
                                    ) ; overload;

      /// <summary>
      ///   Save all changes to the data.
      /// </summary>
      procedure SaveData            ;

      /// <summary>
      ///   Save all layers (parameters and data)
      /// </summary>
      procedure SaveAll             ;

      /// <summary>
      ///   Check if any layer or the project file was modified by editing.
      /// </summary>
      /// <returns>
      ///   True if project should be saved.
      /// </returns>
      function  MustSave            : Boolean ;

      /// <summary>
      ///   Mark viewer upon some modifications.
      /// </summary>
      procedure MarkModified        ;

      /// <summary>
      ///   Calculates a common extent.
      /// </summary>
      /// <remarks>
      ///   Such extent covers the extents of all layers.
      /// </remarks>
      procedure RecalcExtent        ;

      /// <summary>
      ///   Reposition the map within the component window.
      /// </summary>
      /// <remarks>
      ///   Recalculate viewport, etc to match actual extent,
      ///   center etc.
      /// </remarks>
      procedure Reposition          ;

      /// <summary>
      ///   Invalidate an extent for full map redraw
      /// </summary>
      /// <param name="_extent">
      ///   extent to redraw
      /// </param>
      procedure InvalidateExtent    ( const _extent : TGIS_Extent
                                    ) ; overload;

      /// <summary>
      ///   Invalidate an extent.
      /// </summary>
      /// <param name="_extent">
      ///   extent to redraw
      /// </param>
      /// <param name="_deep">
      ///   True if whole map must marked for update; if False then
      ///   only shallow repaint will be issued (only topmost layers)
      /// </param>
      procedure InvalidateExtent    ( const _extent : TGIS_Extent ;
                                      const _deep   : Boolean
                                    ) ; overload;

      /// <summary>
      ///   Invalidate whole map. All layers must be updated.
      /// </summary>
      procedure InvalidateWholeMap  ;

      /// <summary>
      ///   Invalidate only topmost layers.
      /// </summary>
      procedure InvalidateTopmost   ;

      /// <summary>
      ///   Invalidate only basemap layers.
      /// </summary>
      procedure InvalidateBasemap   ;

      /// <summary>
      ///   Invalidate selection. To mark viewer after selection change to
      ///    redraw state.
      /// </summary>
      procedure InvalidateSelection ;

      /// <summary>
      ///   Invalidate editor. To mark viewer after editor change to
      ///    redraw state.
      /// </summary>
      /// <param name="_final">
      ///   True if map must updated at the end of editing process;
      ///   otherwise a normal "shallow" screen refresh to be performed
      /// </param>
      procedure InvalidateEditor    ( const _final   : Boolean
                                    ) ;

      /// <summary>
      ///   Calculates a zoom which places the whole map inside the window
      /// </summary>
      /// <returns>
      ///   Zoom value.
      /// </returns>
      function  FullExtentZoom      : Double ;

      /// <summary>
      ///   Calculate the screen origin, zoom, and viewport to place the
      ///   whole map in the window.
      /// </summary>
      procedure FullExtent          ;

      /// <summary>
      ///   Locate a shape that is near _ptg, but is closer than _prec
      ///   distance.
      /// </summary>
      /// <param name="_ptg">
      ///   reference point /searching point/
      /// </param>
      /// <param name="_prec">
      ///   precision /not a longer distance than/; for points inside the
      ///   polygon (if _prec &gt;= 0) the distance will be multiply by
      ///   0.95 (to prefer points inside the polygon) but will not be
      ///   bigger then _prec; for point distance will be multiplied by
      ///   0.9 to prefer points over lines and polygons; precision is
      ///   express in map units
      /// </param>
      /// <remarks>
      ///   To improve performance sections visibility can be used to
      ///   choose layer to call its Locate method.
      /// </remarks>
      /// <returns>
      ///   Located shape or nil
      /// </returns>
      function  Locate              ( const _ptg     : TGIS_Point ;
                                      const _prec    : Double
                                    ) : TGIS_ShapeAbstract ; overload;

      /// <summary>
      ///   Locate a shape that is near _ptg, but is closer than _prec
      ///   distance.
      /// </summary>
      /// <param name="_ptg">
      ///   reference point /searching point/
      /// </param>
      /// <param name="_prec">
      ///   precision /not a longer distance than/; for points inside the
      ///   polygon (if _prec &gt;= 0) the distance will be multiply by 0.95
      ///   (to prefer points inside the polygon) but will not be bigger
      ///   then _prec; for point distance will be multiplied by 0.9 to
      ///   prefer points over lines and polygons; precision is express in
      ///   map units
      /// </param>
      /// <param name="_visible">
      ///   if true the only visible shapes will be evaluated; shapes turned
      ///   of by query of hidden will be ignored
      /// </param>
      /// <remarks>
      ///   To improve performance sections visibility can be used to choose
      ///   layer to call its Locate method. Only visible shapes will be
      ///   evaluated.
      /// </remarks>
      /// <returns>
      ///   Located shape or nil
      /// </returns>
      function  Locate              ( const _ptg     : TGIS_Point ;
                                      const _prec    : Double     ;
                                      const _visible : Boolean
                                    ) : TGIS_ShapeAbstract ; overload;

      /// <summary>
      ///   Locate a shape that is near _pt, but is closer than _prec
      ///   distance.
      /// </summary>
      /// <param name="_pt">
      ///   reference point /searching point/ in pixels
      /// </param>
      /// <param name="_prec">
      ///   precision /not a longer distance than/; for points inside the
      ///   polygon (if _prec &gt;= 0) the distance will be multiply by 0.95
      ///   (to prefer points inside the polygon) but will not be bigger
      ///   then _prec; for point distance will be multiplied by 0.9 to
      ///   prefer points over lines and polygons; precision is express in
      ///   pixels
      /// </param>
      /// <returns>
      ///   Located shape or nil
      /// </returns>
      function  Locate              ( const _pt      : TPoint     ;
                                      const _prec    : Integer
                                    ) : TGIS_ShapeAbstract ; overload;

      /// <summary>
      ///   Locate shapes on layers that are near _ptg, but closer than _prec distance.
      ///   Still only the best one candidate is chosen from each layer.
      /// </summary>
      /// <param name="_ptg">
      ///   reference point /searching point/
      /// </param>
      /// <param name="_prec">
      ///   precision /not a longer distance than/; for points inside the
      ///   polygon (if _prec &gt;= 0) the distance will be multiply by 0.95
      ///   (to prefer points inside the polygon) but will not be bigger
      ///   then _prec; for point distance will be multiplied by 0.9 to
      ///   prefer points over lines and polygons; precision is express in
      ///   map units
      /// </param>
      /// <param name="_visible">
      ///   if true the only visible shapes will be evaluated; shapes turned
      ///   of by query of hidden will be ignored
      /// </param>
      /// <remarks>
      ///   To improve performance sections visibility can be used to choose
      ///   layer to call its Locate method. Only visible shapes will be
      ///   evaluated.
      /// </remarks>
      /// <returns>
      ///   Allocated list of shapes from all viewer layers
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    returned list should be freed by a calling application.
      ///    </note>
      /// </remarks>
      function  LocateEx            ( const _ptg     : TGIS_Point ;
                                      const _prec    : Double    ;
                                      const _visible : Boolean
                                    ) : TGIS_ShapeAbstractList ;

      /// <summary>
      ///   Converts point coordinates from map related to screen related.
      /// </summary>
      /// <param name="_ptg">
      ///   coordinate in map units
      /// </param>
      /// <remarks>
      ///   See TGIS_LayerVector.DrawLabel for example.
      /// </remarks>
      /// <returns>
      ///   Map coordinates converted to screen coordinates.
      /// </returns>
      function  MapToScreen         ( const _ptg     : TGIS_Point
                                    ) : TPoint ;

      /// <summary>
      ///   Converts 3D point coordinates from map related to screen
      ///   related.
      /// </summary>
      /// <param name="_ptg">
      ///   coordinate in map units
      /// </param>
      /// <remarks>
      ///   See TGIS_LayerVector.DrawLabel for example.
      /// </remarks>
      /// <returns>
      ///   Map coordinates converted to screen coordinates.
      /// </returns>
      function  MapToScreen3D       ( const _ptg     : TGIS_Point3D
                                    ) : TPoint ;

      /// <summary>
      ///   Converts point coordinates from screen related to map related.
      /// </summary>
      /// <param name="_pt">
      ///   coordinate in screen units
      /// </param>
      /// <remarks>
      ///   See TGIS_LayerVector.Locate for example.
      /// </remarks>
      /// <returns>
      ///   Screen coordinates converted to map coordinates.
      /// </returns>
      function  ScreenToMap         ( const _pt      : TPoint
                                    ) : TGIS_Point ;

      /// <summary>
      ///   Converts point coordinates from screen related to map related.
      /// </summary>
      /// <param name="_pt">
      ///   coordinate in screen units
      /// </param>
      /// <remarks>
      ///   See TGIS_LayerVector.Locate for example.
      /// </remarks>
      /// <returns>
      ///   Screen coordinates converted to map coordinates.
      /// </returns>
      function  ScreenToMap3D       ( const _pt      : TPoint
                                    ) : TGIS_Point3D ;

      /// <summary>
      ///   Converts point coordinates from map related to screen related,
      ///   but result will be in TGIS_Point.
      /// </summary>
      /// <param name="_pt">
      ///   coordinate in map units
      /// </param>
      /// <remarks>
      ///   See TGIS_LayerVector.Locate for similar example.
      /// </remarks>
      /// <returns>
      ///   map coordinates converted to screen coordinates
      /// </returns>
      function  MapToScreenEx       ( const _pt      : TGIS_Point
                                    ) : TGIS_Point ;

      /// <summary>
      ///   Converts point coordinates from screen related to map related, but
      ///   source will be in TGIS_Point.
      /// </summary>
      /// <param name="_pt">
      ///   coordinate in screen units
      /// </param>
      /// <returns>
      ///   screen coordinates converted to map coordinates
      /// </returns>
      /// <remarks>
      ///   See TGIS_LayerVector.Locate for example.
      /// </remarks>
      function  ScreenToMapEx       ( const _pt      : TGIS_Point
                                    ) : TGIS_Point ;

      /// <summary>
      ///   Converts rectangle coordinates from map related to screen
      ///   related.
      /// </summary>
      /// <param name="_rct">
      ///   coordinate in map units
      /// </param>
      /// <returns>
      ///   Calculated screen rectangle.
      /// </returns>
      function  MapToScreenRect     ( const _rct     : TGIS_Extent
                                    ) : TRect ;

      /// <summary>
      ///   Converts rectangle coordinates from screen related to map
      ///   related.
      /// </summary>
      /// <param name="_rct">
      ///   coordinate in screen units
      /// </param>
      /// <remarks>
      ///   This is a reverse process of MapToScreenRect method. See
      ///   MapToScreenRect for similar example.
      /// </remarks>
      /// <returns>
      ///   Calculated extent.
      /// </returns>
      function  ScreenToMapRect     ( const _rct     : TRect
                                    ) : TGIS_Extent ;

      /// <summary>
      ///   Convert size from device dependent pixels to device independent
      ///   Twips (1/1440 inch). Used to make map device independent.
      /// </summary>
      /// <param name="_size">
      ///   size in pixels; absolute value will be taken
      /// </param>
      /// <returns>
      ///   Size in twips.
      /// </returns>
      function  PixelsToTwips       ( const _size    : Integer
                                    ) : Integer ;

      /// <summary>
      ///   Convert the size from to device independent Twips (1/1440 inch)
      ///   to device dependent pixels. Used to make map device independent.
      /// </summary>
      /// <param name="_size">
      ///   size in twips; if negative then treated as pixels
      /// </param>
      /// <returns>
      ///   Size in twips.
      /// </returns>
      function  TwipsToPixels       ( const _size    : Integer
                                    ) : Integer ;

      /// <summary>
      ///   Convert the size from to device independent Twips (1/1440 inch)
      ///   to device points. Used for advanced font sizing.
      /// </summary>
      /// <param name="_size">
      ///   size in twips; if negative then treated as pixels
      /// </param>
      /// <returns>
      ///   Size in points.
      /// </returns>
      function  TwipsToPoints       ( const _size : Integer
                                    ) : Integer ;

      /// <summary>
      ///   Move the screen origin of the map by delta values.
      /// </summary>
      /// <param name="_dx">
      ///   x delta in pixels
      /// </param>
      /// <param name="_dy">
      ///   y delta in pixels
      /// </param>
      procedure MoveViewport        ( var   _dx, _dy : Integer
                                    ) ;

      /// <summary>
      ///   Move the screen origin of the map by delta values.
      /// </summary>
      /// <param name="_dx">
      ///   x delta in map units
      /// </param>
      /// <param name="_dy">
      ///   y delta in map units
      /// </param>
      {$IFDEF JAVA}
      procedure MoveViewportEx      ( var   _dx, _dy : java.lang.Double     ) ;
      {$else}
      procedure MoveViewportEx      ( var   _dx, _dy : Double     ) ;
      {$ENDIF}
      /// <summary>
      ///   Set viewport to a given position.
      /// </summary>
      /// <param name="_x">
      ///   x coordinate
      /// </param>
      /// <param name="_y">
      ///   y coordinate
      /// </param>

      {$IFDEF JAVA}
      procedure SetViewport         ( var   _x ,  _y : java.lang.Double     ) ;
      {$else}
      procedure SetViewport         ( var   _x ,  _y : Double     ) ;
      {$ENDIF}
      /// <summary>
      ///   Set viewport to the vale when _ptg will be visible on the center
      ///   of the screen. Same as property Center
      /// </summary>
      /// <param name="_ptg">
      ///   coordinates in map units
      /// </param>
      procedure CenterViewport      ( const _ptg     : TGIS_Point ) ;

      /// <summary>
      ///   Setup Coordinate System to a coordinate system provided by WKT
      ///   string (GEOGCS or PROJCS). If provided WKT string is empty then
      ///   coordinate system will be turn off.
      /// </summary>
      /// <param name="_wkt">
      ///   WKT string
      /// </param>
      /// <remarks>
      ///   <note type="important">
      ///     All layers must have proper coordinate system. Otherwise
      ///     coordinate system will be turned-off.
      ///   </note>
      /// </remarks>
      procedure SetCSByWKT          ( const  _wkt    : String     ) ;

      /// <summary>
      ///   Setup Coordinate System to a coordinate system provided by EPSG
      ///   code.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code
      /// </param>
      /// <remarks>
      ///   <note type="important">
      ///     All layers must have proper coordinate system. Otherwise
      ///     coordinate system will be turned-off.
      ///   </note>
      /// </remarks>
      procedure SetCSByEPSG         ( const  _epsg   : Integer    ) ;

      /// <summary>
      ///   Setup Coordinate System to a coordinate system provided by file
      ///   which contains WKT string (GEOGCS or PROJCS). If file does not
      ///   exist or provided WKT string is empty then coordinate system
      ///   will be turned off.
      /// </summary>
      /// <param name="_path">
      ///   path with WKT string (for example shapefile .PRJ)
      /// </param>
      /// <remarks>
      ///   <note type="important">
      ///     All layers must have proper coordinate system. Otherwise
      ///     coordinate system will be turned-off.
      ///   </note>
      /// </remarks>
      procedure SetCSByWKTFile      ( const  _path   : String     ) ;

      /// <summary>
      ///   Compute position of the point after the viewer rotation.
      /// </summary>
      /// <param name="_ptg">
      ///   point in to be rotated
      /// </param>
      /// <returns>
      ///   Rotated map coordinates.
      /// </returns>
      function  RotatedPoint        ( const _ptg     : TGIS_Point
                                    ) : TGIS_Point;

      /// <summary>
      ///   Compute original position of the rotated point.
      /// </summary>
      /// <param name="_ptg">
      ///   rotated point
      /// </param>
      /// <returns>
      ///   Original un-rotated map coordinates.
      /// </returns>
      function  UnrotatedPoint      ( const _ptg     : TGIS_Point
                                    ) : TGIS_Point ;

      /// <summary>
      ///   Compute position of the 3D point after the viewer rotation.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be rotated
      /// </param>
      /// <returns>
      ///   Rotated map coordinates.
      /// </returns>
      function  RotatedPoint3D      ( const _ptg     : TGIS_Point3D
                                    ) : TGIS_Point3D ;

      /// <summary>
      ///   Compute position of the 3D point after the viewer rotation.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be rotated
      /// </param>
      procedure RotatedPoint3D_ref  ( {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
                                    ) ;
      /// <summary>
      ///   Compute original position of the rotated 3D point.
      /// </summary>
      /// <param name="_ptg">
      ///   rotated point
      /// </param>
      /// <returns>
      ///   Original un-rotated map coordinates.
      /// </returns>
      function  UnrotatedPoint3D    ( const _ptg     : TGIS_Point3D
                                    ) : TGIS_Point3D ;

      /// <summary>
      ///   Compute original position of the rotated 3D point.
      /// </summary>
      /// <param name="_ptg">
      ///   rotated point
      /// </param>
      procedure UnrotatedPoint3D_ref( {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
                                    ) ;

      /// <summary>
      ///   Compute extent (encompassing area of the extent) after the
      ///   viewer rotation.
      /// </summary>
      /// <param name="_extent">
      ///   extent to be rotated
      /// </param>
      /// <returns>
      ///   Rotated extent.
      /// </returns>
      function  RotatedExtent       ( const _extent  : TGIS_Extent
                                    ) : TGIS_Extent ;

      /// <summary>
      ///   Compute original extent of the rotated extent.
      /// </summary>
      /// <param name="_extent">
      ///   rotated extent
      /// </param>
      /// <returns>
      ///   Original un-rotated extent.
      /// </returns>
      function  UnrotatedExtent     ( const _extent  : TGIS_Extent
                                    ) : TGIS_Extent ;
      /// <summary>
      ///   Return current context object.
      /// </summary>
      /// <returns>
      ///   Current renderer context.
      /// </returns>
      function  GetRenderContext    : TObject ;

      /// <summary>
      ///   Wait for any pending background processes that must be finalized
      ///   before app can continue.
      /// </summary>
      /// <remarks>
      ///   For example TGIS_ViewerWnd will wait here for BaseMap to by finished.
      ///   Use this internally for example before showing PrintPreview.
      /// </remarks>
      procedure WaitForBackgroundProcesses ;

      /// <summary>
      ///   Call _proc when viewer will fully end painting procees.
      /// </summary>
      /// <param name="_sender">
      ///   sender object
      /// </param>
      /// <param name="_proc">
      ///   procedure to be executed
      /// </param>
      procedure WaitForNotBusy      (       _sender  : TObject ;
                                      const _proc    : TGIS_WaitForNotBusyProc
                                    ) ;
    {$ENDREGION}

    {$REGION IGIS_Viewer public properties}
      /// <summary>
      ///   Map extent. Encompassing the collective extents of all layers and
      ///   a border defined by BigExtentMargin. Is depending on current scale.
      /// </summary>
      property BigExtent : TGIS_Extent
                read  {$IFNDEF OXYGENE} fget_BigExtent {$ENDIF} ;

      /// <summary>
      ///   Level of current busy state. If 1 then most significant busy is
      ///   active; if 2 then subsequent etc.
      /// </summary>
      property BusyLevel : Integer
                read  {$IFNDEF OXYGENE} fget_BusyLevel {$ENDIF} ;

      /// <summary>
      ///   Text related to current busy state. Text could be set upon
      ///   BusyPrepare() call. If IsBusy is not true then text will be an
      ///   empty string.
      /// </summary>
      property BusyText : String
                read  {$IFNDEF OXYGENE} fget_BusyText {$ENDIF} ;

      /// <summary>
      ///   Center the screen based on given point. Useful for "Center on
      ///   Click". See property CenterPtg.
      /// </summary>
      property Center : TGIS_Point
                read  {$IFNDEF OXYGENE} fget_Center {$ENDIF}
                write {$IFNDEF OXYGENE} fset_Center {$ENDIF} ;

      /// <summary>
      ///   Point on which Viewer will be centered during Zoom. Will not
      ///   modify the Viewport. Will set up the point without re centering
      ///   the screen. See property Center.
      /// </summary>
      property CenterPtg : TGIS_Point
                read  {$IFNDEF OXYGENE} fget_CenterPtg {$ENDIF}
                write {$IFNDEF OXYGENE} fset_CenterPtg {$ENDIF} ;

      /// <summary>
      ///   Legal notice and product version.
      /// </summary>
      property Copyright : String
                read  {$IFNDEF OXYGENE} fget_Copyright {$ENDIF};

      /// <summary>
      ///   Coordinate System assigned to the viewer. If set to nil the
      ///   Coordinate System will be turned off. Default is turned off.
      /// </summary>
      /// <remarks>
      ///   <note type="important">
      ///     All layers must have proper coordinate system. Otherwise
      ///     coordinate system will be turned-off.
      ///   </note>
      /// </remarks>
      property CS : TGIS_CSCoordinateSystem
                read  {$IFNDEF OXYGENE} fget_CS {$ENDIF}
                write {$IFNDEF OXYGENE} fset_CS {$ENDIF} ;

      /// <summary>
      ///   Editor context.
      /// </summary>
      property Editor : IGIS_Editor
                read  {$IFNDEF OXYGENE} fget_Editor {$ENDIF}
                write {$IFNDEF OXYGENE} fset_Editor {$ENDIF} ;

      /// <summary>
      ///   Map extent. Encompassing the collective extents of all layers.
      /// </summary>
      property Extent : TGIS_Extent
                read  {$IFNDEF OXYGENE} fget_Extent {$ENDIF} ;

      /// <summary>
      ///   List Copyrights
      /// </summary>
      property FileCopyrights : String
                read  {$IFNDEF OXYGENE} fget_FileCopyrights {$ENDIF} ;

      /// <summary>
      ///   Extent of full current/last drawing area including margin and w/o tiling.
      /// </summary>
      property FullDrawExtent : TGIS_Extent
                read  {$IFNDEF OXYGENE} fget_FullDrawExtent {$ENDIF} ;

      /// <summary>
      ///   Hierarchy list.
      /// </summary>
      property Hierarchy : IGIS_HierarchyManager
                read  {$IFNDEF OXYGENE} fget_Hierarchy {$ENDIF} ;

      /// <summary>
      ///   True if Viewer is busy in Paint procedure.
      /// </summary>
      property InPaint : Boolean
                read  {$IFNDEF OXYGENE} fget_InPaint {$ENDIF} ;

      /// <summary>
      ///   True if Viewer is busy.
      /// </summary>
      property IsBusy : Boolean
                read  {$IFNDEF OXYGENE} fget_IsBusy {$ENDIF} ;

      /// <summary>
      ///   True if Viewer is empty (no layers).
      /// </summary>
      property IsEmpty : Boolean
                read {$IFNDEF OXYGENE} fget_IsEmpty {$ENDIF} ;

      /// <summary>
      ///   True if Viewer is in locked state Lock.
      /// </summary>
      property IsLocked : Boolean
                read  {$IFNDEF OXYGENE} fget_IsLocked {$ENDIF} ;

      /// <summary>
      ///   True if Viewer has a topmost layer.
      /// </summary>
      property IsTopmost : Boolean
                read  {$IFNDEF OXYGENE} fget_IsTopmost {$ENDIF} ;

      /// <summary>
      ///   All layers.
      /// </summary>
      property Items : TGIS_LayerAbstractList
                read  {$IFNDEF OXYGENE} fget_Items {$ENDIF} ;

      /// <summary>
      ///   Labels position allocator object.
      /// </summary>
      property LabelsReg : TGIS_LabelsAreaAbstract
                read  {$IFNDEF OXYGENE} fget_LabelsReg {$ENDIF} ;

      /// <summary>
      ///   Overlapped extent margin (in pixels).
      /// </summary>
      /// <remarks>
      ///   Used to help render shapes placed at tiles' boundaries where
      ///   rendered shape's elements must be placed also at neighboring tiles.
      ///   Default value is 0.
      /// </remarks>
      property OverlappedExtentMargin : Integer
                read  {$IFNDEF OXYGENE} fget_OverlappedExtentMargin {$ENDIF}
                write {$IFNDEF OXYGENE} fset_OverlappedExtentMargin {$ENDIF} ;

      /// <summary>
      ///   Rendering resolution in pixels-per-inch. Returns
      ///   SystemPPI value or CustomPPI (if &lt;&gt; 0)
      ///   multiplied by ControlCanvasScale.
      /// </summary>
      property PPI : Integer
                read  {$IFNDEF OXYGENE} fget_PPI {$ENDIF} ;

      /// <summary>
      ///   Project file itself as opened in Open procedure.
      /// </summary>
      property ProjectFile : TGIS_ConfigAbstract
                read  {$IFNDEF OXYGENE} fget_ProjectFile {$ENDIF}
                write {$IFNDEF OXYGENE} fset_ProjectFile {$ENDIF} ;

      /// <summary>
      ///   Project file name as opened in Open procedure.
      /// </summary>
      property ProjectName : String
                read  {$IFNDEF OXYGENE} fget_ProjectName {$ENDIF} ;

      /// <summary>
      ///  Delayed update threshold in milliseconds. If &gt;0 then
      ///  map will be updated after a while allowing for example subsequent
      ///  draw operation before full map redraw.
      ///  Default value is 700.
      /// </summary>
      property DelayedUpdate : Integer
                read  {$IFNDEF OXYGENE} fget_DelayedUpdate {$ENDIF}
                write {$IFNDEF OXYGENE} fset_DelayedUpdate {$ENDIF} ;

      /// <summary>
      ///  Progressive update threshold in milliseconds. If &gt;0 then
      ///  map will be updated on a time consuming updates every provided period.
      ///  Default value is 2500.
      /// </summary>
      property ProgressiveUpdate : Integer
                read  {$IFNDEF OXYGENE} fget_ProgressiveUpdate {$ENDIF}
                write {$IFNDEF OXYGENE} fset_ProgressiveUpdate {$ENDIF} ;

      /// <summary>
      ///   Restricted Map extent.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Default value is GisWholeWorld.
      ///    </note>
      ///   Extent of the map will never be larger then RestrictedExtent.
      /// </remarks>
      property RestrictedExtent : TGIS_Extent
                read  {$IFNDEF OXYGENE} fget_RestrictedExtent {$ENDIF}
                write {$IFNDEF OXYGENE} fset_RestrictedExtent {$ENDIF} ;

      /// <summary>
      ///   Angle of viewer rotation in radians.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    If a map is rotated, remember that rotation is treated as
      ///    projection, so coordinates are transformable. If map will be
      ///    rotated, the using RestrictedDrag is not recommended.
      ///    </note>
      ///   See also RotationPoint and TGIS_Layer.Unproject.
      /// </remarks>
      property RotationAngle : Double
                read  {$IFNDEF OXYGENE} fget_RotationAngle {$ENDIF}
                write {$IFNDEF OXYGENE} fset_RotationAngle {$ENDIF} ;

      /// <summary>
      ///   Point of viewer rotation in map units.
      /// </summary>
      /// <remarks>
      ///   See also RotationAngle.
      /// </remarks>
      property RotationPoint : TGIS_Point
                read  {$IFNDEF OXYGENE} fget_RotationPoint {$ENDIF}
                write {$IFNDEF OXYGENE} fset_RotationPoint {$ENDIF} ;

      {$IFNDEF OXYGENE}

        /// <summary>
        ///   Setting up and reading the scale factor.
        /// </summary>
        /// <remarks>
        ///   <note type="note">
        ///    same as ScaleAsFloat
        ///    </note>
        ///   Be sure to set up properly coordinate system.
        /// </remarks>
        property Scale : Double
                read  fget_ScaleAsFloat
                write fset_ScaleAsFloat ;
      {$ENDIF}

      /// <summary>
      ///   Setting up and reading the scale factor.
      /// </summary>
      /// <remarks>
      ///   Be sure to set up properly coordinate system.
      /// </remarks>
      property ScaleAsFloat : Double
                read  {$IFNDEF OXYGENE} fget_ScaleAsFloat {$ENDIF}
                write {$IFNDEF OXYGENE} fset_ScaleAsFloat {$ENDIF} ;

      /// <summary>
      ///   Setting up and reading the scale factor as text in format
      ///   '1:10000'.
      /// </summary>
      /// <remarks>
      ///   Be sure to set up properly coordinate system.
      /// </remarks>
      property ScaleAsText : String
                read  {$IFNDEF OXYGENE} fget_ScaleAsText {$ENDIF}
                write {$IFNDEF OXYGENE} fset_ScaleAsText {$ENDIF} ;

      /// <summary>
      ///   Sets and gets zoom level for a viewer.
      /// </summary>
      /// <remarks>
      ///   Zoom level is an alternative for a Scale property.
      ///   It is commonly used in web maps in Web Mercator projection
      ///   (EPSG: 3857), but can be used with any coordinate system.
      /// </remarks>
      property Level : Double
                read  {$IFNDEF OXYGENE} fget_Level {$ENDIF}
                write {$IFNDEF OXYGENE} fset_Level {$ENDIF} ;

      /// <summary>
      ///   Color used for selecting object. See also SelectionTransparency.
      /// </summary>
      property SelectionGisColor : TGIS_Color
                read  {$IFNDEF OXYGENE} fget_SelectionGisColor {$ENDIF}
                write {$IFNDEF OXYGENE} fset_SelectionGisColor {$ENDIF} ;

      /// <summary>
      ///   System resolution in pixels-per-inch.
      /// </summary>
      property SystemPPI : Integer
                read  {$IFNDEF OXYGENE} fget_SystemPPI {$ENDIF} ;

      /// <summary>
      ///   Used to force rendering in a different scale then actual.
      /// </summary>
      /// <remarks>
      ///   Only for internal use of TatukGIS
      /// </remarks>
      property TemporaryScaleInternal : Double
                read  {$IFNDEF OXYGENE} fget_TemporaryScaleInternal {$ENDIF}
                write {$IFNDEF OXYGENE} fset_TemporaryScaleInternal {$ENDIF} ;

      /// <summary>
      ///   Used to force rendering labels while drawing with tiles.
      /// </summary>
      /// <remarks>
      ///   Only for internal use of TatukGIS
      /// </remarks>
      property TemporaryVisibleExtent : TGIS_Extent
                read  {$IFNDEF OXYGENE} fget_TemporaryVisibleExtent {$ENDIF}
                write {$IFNDEF OXYGENE} fset_TemporaryVisibleExtent {$ENDIF} ;

      /// <summary>
      ///   Parent for the current IGIS_Viewer object.
      /// </summary>
      property ViewerParent : IGIS_ViewerParent
                read  {$IFNDEF OXYGENE} fget_ViewerParent {$ENDIF} ;

      /// <summary>
      ///   Parent for the current IGIS_Viewer object. If object is reparented then root parent 
      ///   object will be returned.
      /// </summary>
      property ViewerParentRoot : IGIS_ViewerParent
                read  {$IFNDEF OXYGENE} fget_ViewerParentRoot {$ENDIF} ;

      /// <summary>
      ///   Setting an upper left corner position of the components window
      ///   within the map.
      /// </summary>
      property Viewport : TGIS_Point
                read  {$IFNDEF OXYGENE} fget_Viewport {$ENDIF}
                write {$IFNDEF OXYGENE} fset_Viewport {$ENDIF} ;

      /// <summary>
      ///   Extent of the map that is visible in the window. On setting an
      ///   optimal zoom will be calculated.
      /// </summary>
      property VisibleExtent : TGIS_Extent
                read  {$IFNDEF OXYGENE} fget_VisibleExtent {$ENDIF}
                write {$IFNDEF OXYGENE} fset_VisibleExtent {$ENDIF} ;

      /// <summary>
      ///   True if viewer is upon destruction so some operations (like
      ///   screen updates) should not be performed
      /// </summary>
      property UponDestroy : Boolean
                read  {$IFNDEF OXYGENE} fget_UponDestroy {$ENDIF} ;

      /// <summary>
      ///   Setting up and reading the related zoom factor. If one map unit =
      ///   one pixel, then Zoom = 1.
      /// </summary>
      property Zoom : Double
                read  {$IFNDEF OXYGENE} fget_Zoom {$ENDIF}
                write {$IFNDEF OXYGENE} fset_Zoom {$ENDIF} ;

      /// <summary>
      ///   Setting up and reading the absolute zoom factor. If one map unit
      ///   = one twips, then Zoom = 1.
      /// </summary>
      property ZoomEx : Double
                read  {$IFNDEF OXYGENE} fget_ZoomEx {$ENDIF}
                write {$IFNDEF OXYGENE} fset_ZoomEx {$ENDIF} ;

      /// <summary>
      ///   Master Viewer reference. To be used only by TiledPaint.
      /// </summary>
      property MasterViewer : IGIS_Viewer
                read  {$IFNDEF OXYGENE} fget_MasterViewer {$ENDIF}
                write {$IFNDEF OXYGENE} fset_MasterViewer {$ENDIF} ;
    {$ENDREGION}

    {$REGION IGIS_Viewer published properties}
      {#gendoc:published:GENXDK}
      /// <summary>
      ///   Map extent margin.
      /// </summary>
      /// <remarks>
      ///   Used to help place a map inside a window with a small margin.
      ///   Define in percent of window size (when negative) or map extent
      ///   (when positive). Possible values are -50..50 where: 0 means no
      ///   margin; -50 means that up 50% of a the window size will be
      ///   occupied by margin; 50 means that then margin will be equal to
      ///   half of extent. Default value is -10. See also BigExtent.
      /// </remarks>
      property BigExtentMargin : Integer
                read  {$IFNDEF OXYGENE} fget_BigExtentMargin {$ENDIF}
                write {$IFNDEF OXYGENE} fset_BigExtentMargin {$ENDIF} ;

      /// <summary>
      ///   If True, layers added to the viewer will have a unique style applied;
      ///   Default is False.
      /// </summary>
      /// <remarks>
      ///   Only works with layers that support auto-styling.
      /// </remarks>
      property AutoStyle : Boolean
                read  {$IFNDEF OXYGENE} fget_AutoStyle {$ENDIF}
                write {$IFNDEF OXYGENE} fset_AutoStyle {$ENDIF} ;

      /// <summary>
      ///   True if a viewer should maintain scale upon resize.
      /// </summary>
      property KeepScale  : Boolean
                read  {$IFNDEF OXYGENE} fget_KeepScale {$ENDIF}
                write {$IFNDEF OXYGENE} fset_KeepScale {$ENDIF} ;

      /// <summary>
      ///   Background color.
      /// </summary>
      property Color : TGIS_Color
                read  {$IFNDEF OXYGENE} fget_Color {$ENDIF}
                write {$IFNDEF OXYGENE} fset_Color {$ENDIF} ;

      /// <summary>
      ///   Custom resolution in pixels-per-inch. If &lt;&gt; 0 then
      ///   used instead of SystemPPI
      /// </summary>
      property CustomPPI : Integer
                read  {$IFNDEF OXYGENE} fget_CustomPPI {$ENDIF}
                write {$IFNDEF OXYGENE} fset_CustomPPI {$ENDIF} ;

      /// <summary>
      ///   Font magnifying factor in percents. To make fonts more readable
      ///   without changing PPI.
      /// </summary>
      property FontScale : Integer
                read  {$IFNDEF OXYGENE} fget_FontScale {$ENDIF}
                write {$IFNDEF OXYGENE} fset_FontScale {$ENDIF} ;

      /// <summary>
      ///   Is partial drawing to screen done in incremental mode?
      /// </summary>
      property IncrementalPaint : Boolean
                read  {$IFNDEF OXYGENE} fget_IncrementalPaint {$ENDIF}
                write {$IFNDEF OXYGENE} fset_IncrementalPaint {$ENDIF} ;

      /// <summary>
      ///   Is partial drawing to screen done in tiled  mode?
      /// </summary>
      property TiledPaint : Boolean
                read  {$IFNDEF OXYGENE} fget_TiledPaint {$ENDIF}
                write {$IFNDEF OXYGENE} fset_TiledPaint {$ENDIF} ;

      /// <summary>
      ///   Multiuser mode.
      /// </summary>
      property MultiUserMode : TGIS_MultiUser
                read  {$IFNDEF OXYGENE} fget_MultiUserMode {$ENDIF}
                write {$IFNDEF OXYGENE} fset_MultiUserMode {$ENDIF} ;

      /// <summary>
      ///   <para>
      ///     List of custom, user-defined data. List is save/restored
      ///     witch config/project files.
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
      ///         Using an ASCII filed names without spaces or any
      ///         special characters is recommended.
      ///       </item>
      ///     </list>
      ///   </note>
      /// </remarks>
      property CustomData : TGIS_StringList
                read  {$IFNDEF OXYGENE} fget_CustomData {$ENDIF} ;

      /// <summary>
      ///   If true, then you can not drag outside the map Extent.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    <para>
      ///      If map will be rotated by RotationAngle then using
      ///      RestrictedDrag is not recommended.
      ///    </para>
      ///    </note>
      /// </remarks>
      property RestrictedDrag : Boolean
                read  {$IFNDEF OXYGENE} fget_RestrictedDrag {$ENDIF}
                write {$IFNDEF OXYGENE} fset_RestrictedDrag {$ENDIF} ;

      /// <summary>
      ///   If true then polygons will be marked only with outline not a full fill.
      /// </summary>
      property SelectionOutlineOnly : Boolean
                read  {$IFNDEF OXYGENE} fget_SelectionOutlineOnly {$ENDIF}
                write {$IFNDEF OXYGENE} fset_SelectionOutlineOnly {$ENDIF} ;

      /// <summary>
      ///   Transparency value for selection; if 100 SelectionGisColor will not
      ///   be transparent.
      /// </summary>
      property SelectionTransparency : Integer
                read  {$IFNDEF OXYGENE} fget_SelectionTransparency {$ENDIF}
                write {$IFNDEF OXYGENE} fset_SelectionTransparency {$ENDIF} ;

      /// <summary>
      ///   Outline width of selection area (&gt;0 in twips, &lt;0 in pixels).
      /// </summary>
      property SelectionWidth : Integer
                read  {$IFNDEF OXYGENE} fget_SelectionWidth {$ENDIF}
                write {$IFNDEF OXYGENE} fset_SelectionWidth {$ENDIF} ;

      /// <summary>
      ///  True if animation upon zooming are allowed.
      /// </summary>
      /// <remarks>
      ///  If system discovers that animation are to slow then it will
      ///  disable it internally anyway.
      /// </remarks>
      property UseAnimations : Boolean
              read  {$IFNDEF OXYGENE} fget_UseAnimations {$ENDIF}
              write {$IFNDEF OXYGENE} fset_UseAnimations {$ENDIF} ;

      /// <summary>
      ///   True if RTree will be used by default upon creation of any layer.
      /// </summary>
      property UseRTree : Boolean
                read  {$IFNDEF OXYGENE} fget_UseRTree {$ENDIF}
                write {$IFNDEF OXYGENE} fset_UseRTree {$ENDIF} ;
    {$ENDREGION}

    {$REGION IGIS_Viewer recommended events}
      (*
        The following events are commented out intentionally.
        These events should be implemented in a descendant
        class for full operability but are not necessary.

        /// <summary>
        ///   Busy event. Will be fired regularly during long-drawn operations.
        ///   If end value will be zero, the meaning is: long-drawn with unknown
        ///   end time.
        ///   Close long-drawn operation by calling with parameters (-1,-1).
        /// </summary>
        property BusyEvent : TGIS_BusyEvent ;

        /// <summary>
        ///   Help event. If attached, all dialog boxes will have a help button,
        ///   and clicking on this button will raise an event.
        /// </summary>
        property HelpEvent : TGIS_HelpEvent ;

        /// <summary>
        ///   EditorSnapPoint event. Will be fired upon snapping a point in
        ///   the editor in custom snapping mode.
        /// </summary>
        property EditorSnapPointEvent : TGIS_EditorSnapPointEvent ;

        /// <summary>
        ///   EditorPointChange event. Will be fired upon changing a point in the
        ///   editor.
        /// </summary>
        property EditorPointChangeEvent : TGIS_EditorPointChangeEvent ;

        /// <summary>
        ///   EditorPointMove event. Will be fired upon mouse moving of edited
        ///   point in the editor.
        /// </summary>
        property EditorPointMoveEvent : TGIS_EditorPointMoveEvent ;

        /// <summary>
        ///   ExtentChange event. Will be fired on TGIS_Viewer.Extent change.
        ///   the editor in custom snapping mode.
        /// </summary>
        property ExtentChangeEvent : TNotifyEvent ;

        /// <summary>
        ///   LayerAdd event. Will be fired upon adding layer to the viewer:
        ///   after creation but before opening.
        /// </summary>
        property LayerAddEvent : TGIS_LayerEvent ;

        /// <summary>
        ///   LayerDelete event. Will be fired upon deleting layer to the
        ///   viewer: just before destructing the layer.
        /// </summary>
        property LayerDeleteEvent : TGIS_LayerEvent ;

        /// <summary>
        ///   PaintException event. Will be fired when Paint rises
        ///   an exception.
        /// </summary>
        property PaintExceptionEvent : TGIS_PaintExceptionEvent ;

        /// <summary>
        ///   Password event. Will be fired upon opening layer to resolve any username/password
        ///   as a key/value pair. Supported only on selected layers.
        /// </summary>
        property PasswordEvent : TGIS_TemplateProducerEvent ;

        /// <summary>
        ///   Project close event. Will be fired on TGIS_Viewer.Close.
        /// </summary>
        property ProjectCloseEvent : TNotifyEvent ;

        /// <summary>
        ///   Project open event. Will be fired on TGIS_Viewer.Open.
        ///   an exception.
        /// </summary>
        property ProjectOpenEvent : TNotifyEvent ;

        /// <summary>
        ///   VisibleExtentChange event.  Will be fired before OnBeforePaint if
        ///   TGIS_Viewer.VisibleExtent was changed. Will not be fired if
        ///   TGIS_Viewer.VisibleExtent was changed based on changed
        ///   Zoom - in such situation only OnZoomChange will be fired.
        /// </summary>
        property VisibleExtentChangeEvent : TNotifyEvent ;

        /// <summary>
        ///   ZoomChange event. Will be fired before OnBeforePaint if
        ///   Viewer.Zoom was changed.
        /// </summary>
        property ZoomChangeEvent : TNotifyEvent ;
      *)
    {$ENDREGION}
  end;

  /// <summary>
  ///   Common API for viewer parent operations. To be shared across platforms.
  /// </summary>
  /// <remarks>
  ///   Do not call this methods and properties of this interface directly.
  ///   This interface has been design for internal Viewer/Parent
  ///   communication only.
  /// </remarks>
  IGIS_ViewerParent = {$IFDEF OXYGENE} public partial {$ENDIF} interface
    {$IFDEF DCC}
      ['{FDFE1EE0-37C7-42DF-BAE1-5A0DA6A4D15E}']
    {$ENDIF}

    {$REGION IGIS_ViewerParent public methods}
      /// <summary>
      ///   Notify control that is map is going to be closed.
      /// </summary>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      procedure ControlClose           ;

      /// <summary>
      ///   Draw texture.
      /// </summary>
      /// <param name="_bmp">
      ///   bitmap
      /// </param>
      /// <param name="_extent">
      ///   extent
      /// </param>
      /// <param name="_ppi">
      ///   PPI value
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      procedure ControlDrawTexture     (        _bmp     : TObject     ;
                                          const _extent  : TGIS_Extent ;
                                          const _ppi     : Integer
                                       ) ; overload ;

      /// <summary>
      ///   Draw texture.
      /// </summary>
      /// <param name="_bmp">
      ///   bitmap
      /// </param>
      /// <param name="_layer">
      ///   layer to be drawn; if nil then all non 3d layers will be rendered
      /// </param>
      /// <param name="_extent">
      ///   extent
      /// </param>
      /// <param name="_ppi">
      ///   PPI value
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      procedure ControlDrawTexture     (        _bmp     : TObject     ;
                                          const _layer   : TGIS_LayerAbstract ;
                                          const _extent  : TGIS_Extent ;
                                          const _ppi     : Integer
                                       ) ; overload ;

      /// <summary>
      ///   Get renderer instance.
      /// </summary>
      /// <returns>
      ///   renderer.
      /// </returns>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      function  ControlRenderer        : TObject ;

      /// <summary>
      ///   Do flash.
      /// </summary>
      /// <param name="_times">
      ///   how many times
      /// </param>
      /// <param name="_delay">
      ///   delay between each flash
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      procedure ControlFlash           ( const _times    : Integer ;
                                         const _delay    : Integer
                                       ) ;
      /// <summary>
      ///   Get system PPI.
      /// </summary>
      /// <returns>
      ///   PPI value.
      /// </returns>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      function  ControlSystemPPI       : Integer ;

      /// <summary>
      ///   Get current PPI.
      /// </summary>
      /// <returns>
      ///   PPI value.
      /// </returns>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      function  ControlPPI             : Integer ;

      /// <summary>
      ///   Get canvas scale.
      /// </summary>
      /// <returns>
      ///   scale.
      /// </returns>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      function  ControlCanvasScale     : Single ;

      /// <summary>
      ///   Get canvas width.
      /// </summary>
      /// <returns>
      ///   width.
      /// </returns>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      function  ControlCanvasWidth     : Integer ;

      /// <summary>
      ///   Get canvas height.
      /// </summary>
      /// <returns>
      ///   height.
      /// </returns>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      function  ControlCanvasHeight    : Integer ;

      /// <summary>
      ///   Repaint control.
      /// </summary>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      procedure ControlRepaint         ;

      /// <summary>
      ///   Do process messages.
      /// </summary>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      procedure ControlProcessMessages ;

      /// <summary>
      ///   Wait for pending paint operation to be finalized.
      /// </summary>
      /// <param name="_interrupt">
      ///   interrupt any pending operation
      /// </param>
      /// <returns>
      ///   True if operation were synchronized; False if synchronization
      ///   is not possible and current operation should not continue.
      /// </returns>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      function ControlUpdateSynchronize( const _interrupt : Boolean
                                       ) : Boolean ;

      /// <summary>
      ///   Update whole map.
      /// </summary>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      procedure ControlUpdateWholeMap  ;

      /// <summary>
      ///   Update basemap.
      /// </summary>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      procedure ControlUpdateBasemap  ;

      /// <summary>
      ///   Progressive update.
      /// </summary>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      procedure ControlUpdateProgressive ;

      /// <summary>
      ///   Update Topmost layer.
      /// </summary>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      procedure ControlUpdateTopmost   ;

      /// <summary>
      ///   Update Selection layer.
      /// </summary>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      procedure ControlUpdateSelection ;

      /// <summary>
      ///   Update the editor.
      /// </summary>
      /// <param name="_final">
      ///   Is this final update
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      procedure ControlUpdateEditor    ( const _final    : Boolean
                                       ) ;

      /// <summary>
      ///   Do Hourglass show.
      /// </summary>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      procedure ControlHourglassShow   ;

      /// <summary>
      ///   Do Hourglass hide.
      /// </summary>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      procedure ControlHourglassHide   ;

      /// <summary>
      ///   Do Hourglass shake.
      /// </summary>
      /// <returns>
      ///   True if shake was made.
      /// </returns>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      function  ControlHourglassShake  : Boolean ;

      /// <summary>
      ///   Set current 3D mode.
      /// </summary>
      /// <param name="_mode">
      ///   3D mode
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      procedure ControlSet3DMode       ( const _mode     : TGIS_Viewer3DMode
                                       ) ;

      /// <summary>
      ///   Raise event on editor change.
      /// </summary>
      /// <param name="_sender">
      ///   sender object
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      procedure ControlRaiseEditorChangeEvent(
                                               _sender   : TObject
                                       ) ;

      /// <summary>
      ///   Move the screen origin of the map by delta values.
      /// </summary>
      /// <param name="_dx">
      ///   x delta
      /// </param>
      /// <param name="_dy">
      ///   y delta
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      procedure ControlAutoCenterViewport   ( const _dx, _dy : Double ) ;

      /// <summary>
      ///   Notify control that is map extent was changed
      /// </summary>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      procedure ControlExtentChanged ;


      /// <summary>
      ///   Set internal viewer handle.
      /// </summary>
      /// <param name="_viewer">
      ///   new viewer object
      /// </param>
      /// <returns>
      ///   old viewer object.
      /// </returns>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      function  SetViewer ( const _viewer : TObject ) : TObject ;

      /// <summary>
      ///   Get internal viewer handle.
      /// </summary>
      /// <returns>
      ///   current viewer object.
      /// </returns>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      function  GetViewer : TObject ;
    {$ENDREGION}

    {$REGION IGIS_ViewerParent events}
      (*
        The following events are commented out intentionally.
        These events should be implemented in a descendant
        class for full operability but are not necessary.

        /// <summary>
        ///   BeforePaint event. Will be fired before any Paint
        ///   operation.
        /// </summary>
        property BeforePaintEvent : TGIS_PaintEvent ;

        /// <summary>
        ///   AfterPaint event. Will be fired after any Paint
        ///   operation.
        /// </summary>
        property AfterPaintEvent  : TGIS_PaintEvent ;

        /// <summary>
        ///   BeforePaintRenderer event. Will be fired before any Paint operation.
        ///   Uses renderer object.
        /// </summary>
        property BeforePaintRendererEvent : TGIS_RendererEvent ;

        /// <summary>
        ///   AfterPaintRenderer event. Will be fired after any Paint operation.
        ///   Uses renderer object.
        /// </summary>
        property AfterPaintRendererEvent : TGIS_RendererEvent ;

        /// <summary>
        ///   PaintExtra event. Called after renderer.PaintExtra()
        ///   method.
        /// </summary>
        property PaintExtraEvent  : TGIS_RendererEvent ;

        /// <summary>
        ///   BeforeUpdate event. Will be fired before Update
        ///   operation.
        /// </summary>
        property BeforeUpdateEvent : TGIS_RendererEvent ;

        /// <summary>
        ///   AfterUpdate event. Will be fired after Update
        ///   operation.
        /// </summary>
        property AfterUpdateEvent : TGIS_RendererEvent ;
      *)
    {$ENDREGION}
  end;

  IGIS_Printer = {$IFDEF OXYGENE} public partial {$ENDIF} interface ;

  /// <summary>
  ///   Common API for windows viewers.
  /// </summary>
  IGIS_ViewerWnd = {$IFDEF OXYGENE} public {$ENDIF} interface
    {$IFDEF DCC}
      ['{BD1662AB-A792-4809-8FF4-11E09E4005D5}']
    {$ENDIF}

    {$REGION IGIS_ViewerWnd property access routines}
      {$IFDEF GIS_3D}
        function  fget_Viewer3D : IGIS_Viewer3D ;
        procedure fset_Viewer3D ( const _viewer : IGIS_Viewer3D ) ;
        function  fget_View3D   : Boolean ;
        procedure fset_View3D   ( const _value : Boolean ) ;
      {$ENDIF}
    {$ENDREGION}

    {$REGION IGIS_ViewerWnd public methods}
      /// <summary>
      ///   Print the current content on a bitmap.
      ///   Print area will match the current VisibleExtent of the control.
      /// </summary>
      /// <param name="_bmp">
      ///   bitmap on which the drawing will be performed
      /// </param>
      procedure PrintBmp    ( var   _bmp   : TGIS_Bitmap
                            ) ; overload ;

      /// <summary>
      ///   Print the current content on a bitmap.
      ///   Print area will match the current VisibleExtent of the control.
      /// </summary>
      /// <param name="_bmp">
      ///   bitmap on which the drawing will be performed
      /// </param>
      /// <param name="_full">
      ///   if yes, all paint events are triggered
      /// </param>
      procedure PrintBmp    ( var   _bmp   : TGIS_Bitmap ;
                              const _full  : Boolean
                            ) ; overload ;

      /// <summary>
      ///   Print the current viewer content using the default printer.
      /// </summary>
      /// <remarks>
      ///   Print area will match the current VisibleExtent of the control.
      /// </remarks>
      procedure Print       ; overload ;

      /// <summary>
      ///   Print the current viewer content using the given printer.
      /// </summary>
      /// <desc>
      ///   Print area will match the current VisibleExtent of the control.
      /// </desc>
      /// <param name="_printer">
      ///   printer object
      /// </param>
      procedure Print       ( _printer     : IGIS_Printer
                            ) ; overload ;

      /// <summary>
      ///   Zoom the viewer.
      /// </summary>
      /// <param name="_zm">
      ///   zoom value
      /// </param>
      /// <param name="_x">
      ///   center point x coordinate
      /// </param>
      /// <param name="_y">
      ///   center point y coordinate
      /// </param>
      /// <remarks>
      ///   This function will always perform regardless of UseAnimations
      ///   property value.
      /// </remarks>
      procedure ZoomBy      ( const _zm    : Double  ;
                              const _x     : Integer ;
                              const _y     : Integer
                            ) ;
    {$ENDREGION}

    {$REGION IGIS_ViewerWnd property access routines}
      {$IFDEF GIS_3D}
        /// <summary>
        ///   Viewer 3D object. Nil if 3D mode is off.
        /// </summary>
        property Viewer3D : IGIS_Viewer3D read  {$IFNDEF OXYGENE} fget_Viewer3D {$ENDIF}
                                          write {$IFNDEF OXYGENE} fset_Viewer3D {$ENDIF} ;

        /// <summary>
        ///   True if viewer is in 3D mode.
        /// </summary>
        property View3D   : Boolean       read  {$IFNDEF OXYGENE} fget_View3D {$ENDIF}
                                          write {$IFNDEF OXYGENE} fset_View3D {$ENDIF} ;
      {$ENDIF}
    {$ENDREGION}
  end ;

  /// <summary>
  ///   Common API for bitmap viewers.
  /// </summary>
  IGIS_ViewerBmp = {$IFDEF OXYGENE} public {$ENDIF} interface
    {$IFDEF DCC}
      ['{79A40B8C-081B-4F02-AFD4-107965540D16}']
    {$ENDIF}

      function  fget_Bitmap        : TObject ;
      function  fget_GIS_Bitmap    : TGIS_Bitmap ;
      function  fget_TileRect      : TRect ;
      procedure fset_TileRect      ( const _val : TRect ) ;
      function  fget_Width         : Integer ;
      function  fget_Height        : Integer ;
      function  fget_KeepContextInternal
                                    : Boolean ;
      procedure fset_KeepContextInternal
                                   ( const _val : Boolean ) ;
      function  fget_ContextInternal: TObject ;

    {$REGION IGIS_ViewerBmp public methods}
      /// <summary>
      ///   Zoom in slightly.
      /// </summary>
      procedure ZoomIn        ;

      /// <summary>
      ///   Zoom out slightly.
      /// </summary>
      procedure ZoomOut       ;

      /// <summary>
      ///   Clear a bitmap and prepare background color and picture.
      /// </summary>
      procedure Clear         ;

      /// <summary>
      ///   Draw a current extent on a bitmap.
      /// </summary>
      /// <remarks>
      ///   Bitmap is normally updated by FullExtent, VisibleChange, Zoom, Scroll* and
      ///   InvalidateWholeMap operations. Those operations can be grouped with Lock..Unlock.
      ///   Use Draw method will draw map out-of-the-sequence and should be used only
      ///   for very specific needs.
      /// </remarks>
      procedure Draw          ;

      /// <summary>
      ///   Do scrolling page up.
      /// </summary>
      procedure ScrollPgUp    ;

      /// <summary>
      ///   Do scrolling page down.
      /// </summary>
      procedure ScrollPgDn    ;

      /// <summary>
      ///   Do scrolling page right.
      /// </summary>
      procedure ScrollPgRight ;

      /// <summary>
      ///   Do scrolling page left.
      /// </summary>
      procedure ScrollPgLeft  ;

      /// <summary>
      ///   Do scrolling up.
      /// </summary>
      procedure ScrollUp      ;

      /// <summary>
      ///   Do scrolling down.
      /// </summary>
      procedure ScrollDn      ;

      /// <summary>
      ///   Do scrolling right.
      /// </summary>
      procedure ScrollRight   ;

      /// <summary>
      ///   Do scrolling left.
      /// </summary>
      procedure ScrollLeft    ;

      /// <summary>
      ///   Set bitmap new size.
      /// </summary>
      /// <param name="_width">
      ///   bitmap width
      /// </param>
      /// <param name="_height">
      ///   bitmap height
      /// </param>
      procedure SetSize( const _width  : Integer ;
                         const _height : Integer
                        ) ;

      /// <summary>
      ///   Underlying bitmap as a native, platform dependent object.
      /// </summary>
      property Bitmap              : TObject
                                     read  {$IFNDEF OXYGENE} fget_Bitmap {$ENDIF};

      /// <summary>
      ///   Underlying bitmap
      /// </summary>
      property GIS_Bitmap          : TGIS_Bitmap
                                     read  {$IFNDEF OXYGENE} fget_GIS_Bitmap {$ENDIF};

      /// <summary>
      ///   Location of the bitmap over the entire rendered area (in pixels).
      /// </summary>
      /// <remarks>
      ///   For tiled drawing it is a location of the bitmap on the final output.
      ///   For non tiled drawing it is (0, 0, Width, Height).
      /// </remarks>
      property TileRect            : TRect
                                     read  {$IFNDEF OXYGENE} fget_TileRect {$ENDIF}
                                     write {$IFNDEF OXYGENE} fset_TileRect {$ENDIF};

      /// <summary>
      ///   Underlying bitmap width.
      /// </summary>
      property Width               : Integer
                                     read {$IFNDEF OXYGENE} fget_Width {$ENDIF};

      /// <summary>
      ///   Underlying bitmap height.
      /// </summary>
      property Height              : Integer
                                     read {$IFNDEF OXYGENE} fget_Height {$ENDIF};
    {$ENDREGION}
  end ;

  /// <summary>
  ///   IGIS_Viewer reference, used to avoid calling on _AddRef, _Release upon
  ///   every
  /// </summary>
  TGIS_ViewerRef = {$IFDEF OXYGENE} public {$ENDIF} class
    public
      /// <summary>
      ///   Referenced viewer
      /// </summary>
      Ref : IGIS_Viewer ;
  end ;

  /// <summary>
  ///   Common API for printing control.
  /// </summary>
  IGIS_PrintableControl = {$IFDEF OXYGENE} public {$ENDIF} interface
    {$IFDEF DCC}
      ['{F10457CF-E51C-4EB2-8EC9-1F9390C53D33}']
    {$ENDIF}

    {$REGION IGIS_PrintableControl property access routines}
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      function  fget_InternalName : String ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      procedure fset_InternalName ( const _value : String ) ;
    {$ENDREGION}

    {$REGION IGIS_PrintableControl public methods}
      /// <summary>
      ///   Print the control on the given bitmap.
      /// </summary>
      /// <param name="_bitmap">
      ///   bitmap to print on
      /// </param>
      procedure PrintBmp     ( const _bitmap : TGIS_Bitmap
                             ) ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Prepare a copy of the control for printing.
      ///   It prevents for making changes to the original control
      ///   during printing.
      /// </summary>
      /// <returns>
      ///   created copy object.
      /// </returns>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal communication.
      /// </remarks>
      function CreateCopy    : IGIS_PrintableControl ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Release the copy of the control.
      /// </summary>
      /// <param name="_control">
      ///   copy to free
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal communication.
      /// </remarks>
      procedure FreeCopy  ( const _control : IGIS_PrintableControl
                             ) ;
    {$ENDREGION}

    {$REGION IGIS_PrintableControl properties}
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Used for internal use of TatukGIS.
      /// </summary>
      property InternalName : String
        read  {$IFNDEF OXYGENE} fget_InternalName {$ENDIF}
        write {$IFNDEF OXYGENE} fset_InternalName {$ENDIF} ;
    {$ENDREGION}
  end ;

  /// <summary>
  ///   Common API for printer control.
  /// </summary>
  IGIS_Printer = {$IFDEF OXYGENE} public partial {$ENDIF} interface
    {$IFDEF DCC}
      ['{A35B9FC0-6C09-4739-8FD3-7702BAA90EFB}']
    {$ENDIF}
  end ;

{$IFDEF GIS_3D}
  /// <summary>
  ///   Common API for 3D viewing operations.
  /// </summary>
  IGIS_Viewer3D = {$IFDEF OXYGENE} public partial {$ENDIF} interface
    {$IFDEF DCC}
      ['{1C2B8DFA-E6DD-4839-A394-98DD77191510}']
    {$ENDIF}
      function  fget_Mode             : TGIS_Viewer3DMode ;
      procedure fset_Mode             ( const _value :
                                          TGIS_Viewer3DMode          ) ;
      function  fget_ScaleZ           : Double ;
      procedure fset_ScaleZ           ( const _value : Double        ) ;
      function  fget_ScaleM           : Double ;
      procedure fset_ScaleM           ( const _value : Double        ) ;
      function  fget_Scale            : Double ;
      procedure fset_Scale            ( const _value : Double        ) ;
      function  fget_ScaleAsText      : String ;
      procedure fset_ScaleAsText      ( const _value : String        ) ;
      function  fget_Wireframe        : Boolean ;
      procedure fset_Wireframe        ( const _value : Boolean       ) ;
      function  fget_Lights           : Boolean ;
      procedure fset_Lights           ( const _value : Boolean       ) ;
      function  fget_Labels           : Boolean ;
      procedure fset_Labels           ( const _value : Boolean       ) ;
      function  fget_HideLabels       : Boolean ;
      procedure fset_HideLabels       ( const _value : Boolean       ) ;
      function  fget_VectorEdges      : Boolean ;
      procedure fset_VectorEdges      ( const _value : Boolean       ) ;
      function  fget_EdgesColor       : TGIS_Color ;
      procedure fset_EdgesColor       ( const _value : TGIS_Color    ) ;
      function  fget_ReferencePointer : Boolean ;
      procedure fset_ReferencePointer ( const _value : Boolean       ) ;
      function  fget_ReferencePoint   : TGIS_Point3D ;
      procedure fset_ReferencePoint   ( const _value : TGIS_Point3D
                                      ) ;
      function  fget_ReferencePointMode
                                      : TGIS_Viewer3DReferenceMode ;
      procedure fset_ReferencePointMode
                                      ( const _value :
                                          TGIS_Viewer3DReferenceMode ) ;
      function  fget_ReferencePointOffset
                                      : Double ;
      procedure fset_ReferencePointOffset
                                      ( const _value : Double ) ;
      function  fget_CameraPosition   : TGIS_Point3D ;
      procedure fset_CameraPosition   ( const _value : TGIS_Point3D  ) ;
      function  fget_CameraPositionEx : TGIS_Point3D ;
      procedure fset_CameraPositionEx ( const _value : TGIS_Point3D  ) ;
      function  fget_CameraRotation   : TGIS_Point3D ;
      procedure fset_CameraRotation   ( const _value : TGIS_Point3D  ) ;
      function  fget_SunPosition      : TGIS_Point ;
      procedure fset_SunPosition      ( const _value : TGIS_Point    ) ;
      function  fget_ShadowsLevel     : Integer ;
      procedure fset_ShadowsLevel     ( const _value : Integer       ) ;
      function  fget_FastMode         : Boolean ;
      procedure fset_FastMode         ( const _value : Boolean       ) ;
      function  fget_LightVector      : Boolean ;
      procedure fset_LightVector      ( const _value : Boolean       ) ;
      function  fget_DemWalls         : TGIS_Viewer3DDemWall           ;
      procedure fset_DemWalls         ( const _value : TGIS_Viewer3DDemWall ) ;
      function  fget_UniverseColor    : TGIS_Color ;
      procedure fset_UniverseColor    ( const _value : TGIS_Color   ) ;
      function  fget_TextureMode      : Boolean ;
      procedure fset_TextureMode      ( const _value : Boolean       ) ;
      function  fget_IsolineGap       : Double ;
      procedure fset_IsolineGap       ( const _value : Double) ;
      function  fget_IsolineColor     : TGIS_Color ;
      procedure fset_IsolineColor     ( const _value : TGIS_Color) ;
      procedure fset_SolidWallColor   ( const _value : TGIS_Color        ) ;
      function  fget_Flood            : TGIS_Viewer3DFlood ;
      procedure fset_Flood            ( const _value : TGIS_Viewer3DFlood ) ;
      function  fget_BasePlane        : TGIS_Viewer3DBasePlane ;
      procedure fset_BasePlane        ( const _value : TGIS_Viewer3DBasePlane) ;
      function  fget_VisibleExtent3D  : TGIS_Extent3D ;
      function  fget_VisibleExtent    : TGIS_Extent ;
      procedure fset_VisibleExtent    ( const _value : TGIS_Extent ) ;
      function  fget_ZoomFactor       : Double ;
      function  fget_Restriction      : TGIS_Viewer3DViewRestriction ;
      procedure fset_Restriction      ( const _value :
                                        TGIS_Viewer3DViewRestriction ) ;
      function  fget_DemTransparency  : Boolean ;
      procedure fset_DemTransparency  ( const _value : Boolean ) ;
      function  fget_TransparencyPriority : TGIS_Viewer3DTransparencyPriority ;
      procedure fset_TransparencyPriority ( const _value :
                                          TGIS_Viewer3DTransparencyPriority ) ;
      function  fget_ErrorMsg         : String ;
      function  fget_PixelSize        : TGIS_Point ;
      function  fget_DemDetailExtentFactor : Double ;
      procedure fset_DemDetailExtentFactor ( const _value : Double ) ;
      function  fget_DemDraftExtentFactor  : Double ;
      procedure fset_DemDraftExtentFactor  ( const _value : Double ) ;
      function  fget_VectorExtentFactor    : Double ;
      procedure fset_VectorExtentFactor    ( const _value : Double ) ;
      function  fget_VectorSimplification  : Boolean ;
      procedure fset_VectorSimplification  ( const _value : Boolean ) ;
      function  fget_VectorSmartSize       : Integer ;
      procedure fset_VectorSmartSize       ( const _value : Integer ) ;
      function  fget_DemGridSize           : Integer ;
      function  fget_DemCachedSize         : TGIS_Viewer3DDemCacheSize ;
      procedure fset_DemCachedSize         ( const _value :
                                             TGIS_Viewer3DDemCacheSize
                                           ) ;
      function  fget_IgnoreAbove           : Double  ;
      procedure fset_IgnoreAbove           ( const _value : Double ) ;
      function  fget_IgnoreBelow           : Double  ;
      procedure fset_IgnoreBelow           ( const _value : Double ) ;
      function  fget_CutAbove              : Double  ;
      procedure fset_CutAbove              ( const _value : Double ) ;
      function  fget_CutBelow              : Double  ;
      procedure fset_CutBelow              ( const _value : Double ) ;
      function  fget_IgnoreEllipsoidHeight : Boolean  ;
      procedure fset_IgnoreEllipsoidHeight ( const _value : Boolean ) ;
      function  fget_AdvNavigation         : Boolean ;
      procedure fset_AdvNavigation         ( const _value : Boolean ) ;
      function  fget_OrthoView             : Boolean ;
      procedure fset_OrthoView             ( const _value : Boolean ) ;
      function  fget_IsBusy                : Boolean ;

      function  fget_KeepSunCamera         : Boolean ;
      procedure fset_KeepSunCamera         ( const _value : Boolean ) ;

      /// <summary>
      ///   Viewer3D mode working mode (camera mode, sun, zoom, select ...).
      /// </summary>
      property Mode                        : TGIS_Viewer3DMode
                                              read  {$IFNDEF OXYGENE} fget_Mode {$ENDIF}
                                              write {$IFNDEF OXYGENE} fset_Mode  {$ENDIF};

      /// <summary>
      ///   Z scaling; default is 1.
      /// </summary>
      property ScaleZ         : Double          read  {$IFNDEF OXYGENE} fget_ScaleZ {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_ScaleZ  {$ENDIF};

      /// <summary>
      ///   M scaling; default is 1.
      /// </summary>
      property ScaleM         : Double          read  {$IFNDEF OXYGENE} fget_ScaleM {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_ScaleM  {$ENDIF};

      /// <summary>
      ///   Setting up and reading the 3D scale factor.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    same as TGIS_Viewer.Scale
      ///    </note>
      /// </remarks>
      property Scale           : Double         read  {$IFNDEF OXYGENE} fget_Scale {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_Scale  {$ENDIF};

      /// <summary>
      ///   Setting up and reading the 3D scale factor as text in format
      ///   '1:10000'.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    same as TGIS_Viewer.ScaleAsText.
      ///    </note>
      /// </remarks>
      property ScaleAsText     : String         read  {$IFNDEF OXYGENE} fget_ScaleAsText {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_ScaleAsText {$ENDIF} ;

      /// <summary>
      ///   Wire frame mode on/off. Default is off what means solid models.
      /// </summary>
      property ShowWireframe  : Boolean         read  {$IFNDEF OXYGENE} fget_Wireframe {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_Wireframe {$ENDIF} ;

      /// <summary>
      ///   Turn Lights on/off. Default is off (what means perfectly diffused
      ///   light). On means that scene lights like sun etc are turned on.
      /// </summary>
      property ShowLights     : Boolean         read  {$IFNDEF OXYGENE} fget_Lights {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_Lights {$ENDIF} ;

      /// <summary>
      ///   Labels mode on/off. Default is on what means display labels.
      /// </summary>
      property ShowLabels     : Boolean         read  {$IFNDEF OXYGENE} fget_Labels {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_Labels {$ENDIF} ;

      /// <summary>
      ///   Labels not drawn during scene navigation etc. Default is off what means display labels.
      /// </summary>
      property HideLabelsUponNavigation   : Boolean
                                                read  {$IFNDEF OXYGENE} fget_HideLabels {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_HideLabels {$ENDIF} ;

      /// <summary>
      ///   Turn VectorEdges drawing on/off. Default is on when Lights are
      ///   off (what means that VectorEdges are drawn). When Lights are on
      ///   default setting for VectorEdges is off.
      /// </summary>
      property ShowVectorEdges: Boolean         read  {$IFNDEF OXYGENE} fget_VectorEdges {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_VectorEdges {$ENDIF} ;

      /// <summary>
      ///   Vector edges color
      /// </summary>
      property EdgesColor     : TGIS_Color      read  {$IFNDEF OXYGENE} fget_EdgesColor {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_EdgesColor {$ENDIF} ;

      /// <summary>
      ///   Turn ReferencePoint drawing on/off. Default is on (what means
      ///   that ReferencePoint is drawn). ReferencePoint is an arrow
      ///   pointing from ReferencePoint to a "natural" north.
      /// </summary>
      property ShowReferencePoint : Boolean     read  {$IFNDEF OXYGENE} fget_ReferencePointer {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_ReferencePointer {$ENDIF} ;

      /// <summary>
      ///   ReferencePoint in map units, usually screen center. Use
      ///   ReferencePointMode/ReferencePointOffsetZ properties to set.
      /// </summary>
      property ReferencePoint : TGIS_Point3D    read  {$IFNDEF OXYGENE} fget_ReferencePoint {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_ReferencePoint {$ENDIF} ;

      /// <summary>
      ///   Set reference (rotation) point to one of GIS_3DReferencePoint
      ///   type: Base, Zero, OnDem, Lowest, Highest, FlyOnDem
      /// </summary>
      property ReferencePointMode : TGIS_Viewer3DReferenceMode
                                    read  {$IFNDEF OXYGENE} fget_ReferencePointMode {$ENDIF}
                                    write {$IFNDEF OXYGENE} fset_ReferencePointMode {$ENDIF} ;

      /// <summary>
      ///   Set reference (rotation) point Z value offset.
      /// </summary>
      property ReferencePointOffsetZ : Double    read  {$IFNDEF OXYGENE} fget_ReferencePointOffset {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_ReferencePointOffset {$ENDIF} ;

      /// <summary>
      ///   Camera position in radians; Changes of CameraPosition settings
      ///   change CameraRotation settings; Camera always points to the
      ///   central point of the visible area; * X height of the camera above
      ///   horizon, range 0 - Pi/2 rad; * Y azimuth to the camera from
      ///   natural north, range 0 - 2Pi rad; * Z distance to the camera in
      ///   map units; * M optional parameter, for future use.
      /// </summary>
      property CameraPosition  : TGIS_Point3D   read  {$IFNDEF OXYGENE} fget_CameraPosition {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_CameraPosition {$ENDIF} ;

      /// <summary>
      ///   Camera position in XYZ in map units Changes of CameraPositionEx
      ///   settings do not change CameraRotation settings; * X x coordinate;
      ///   * Y y coordinate; * Z z coordinate; * M optional parameter, for
      ///   future use.
      /// </summary>
      property CameraPositionEx : TGIS_Point3D  read  {$IFNDEF OXYGENE} fget_CameraPositionEx {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_CameraPositionEx {$ENDIF} ;

      /// <summary>
      ///   Camera rotation in radians; * X x rotation, vertical , range +-
      ///   Pi/2 rad; * Y y rotation, inclination, range +- Pi/2 rad; * Z z
      ///   rotation, horizontal , range 0 - 2Pi rad; * M camera focal length
      ///   in millimeters, range 20 - 500 mm (default 50).
      /// </summary>
      property CameraRotation  : TGIS_Point3D   read  {$IFNDEF OXYGENE} fget_CameraRotation {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_CameraRotation {$ENDIF} ;

      /// <summary>
      ///   Sun position in radian; natural north * X height of the sun above
      ///   horizon, range +- Pi/2 rad; initial Pi/4 (45 deg) * Y azimuth to
      ///   the sun from natural north, range 0 - 2Pi rad; initial 3Pi/4 (225
      ///   deg)
      /// </summary>
      property SunPosition     : TGIS_Point     read  {$IFNDEF OXYGENE} fget_SunPosition {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_SunPosition {$ENDIF} ;

      /// <summary>
      ///   Check for keeping sun and camera positions relative.
      /// </summary>
      property KeepSunCamera   : Boolean        read  {$IFNDEF OXYGENE} fget_KeepSunCamera {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_KeepSunCamera {$ENDIF} ;

      /// <summary>
      ///   Shadows level; 0..100; 0-deep shadows, 100-light shadows
      /// </summary>
      property ShadowsLevel    : Integer        read  {$IFNDEF OXYGENE} fget_ShadowsLevel {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_ShadowsLevel {$ENDIF} ;
      /// <summary>
      ///   DEM wall types
      /// </summary>
      property DemWalls        : TGIS_Viewer3DDemWall
                                                read  {$IFNDEF OXYGENE} fget_DemWalls {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_DemWalls {$ENDIF} ;

      /// <summary>
      ///   3D viewer color of area which exceeds map extent
      /// </summary>
      property UniverseColor   : TGIS_Color   read  {$IFNDEF OXYGENE} fget_UniverseColor {$ENDIF}
                                              write {$IFNDEF OXYGENE} fset_UniverseColor {$ENDIF} ;

      /// <summary>
      ///   Turn Texture on/off. Default is on
      /// </summary>
      property ShowDemTexture  : Boolean        read  {$IFNDEF OXYGENE} fget_TextureMode {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_TextureMode {$ENDIF} ;

      /// <summary>
      ///   Isoline gap
      /// </summary>
      property DemIsolineGap   : Double         read  {$IFNDEF OXYGENE} fget_IsolineGap {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_IsolineGap {$ENDIF} ;

      /// <summary>
      ///   Isoline color
      /// </summary>
      property DemIsolineColor : TGIS_Color     read  {$IFNDEF OXYGENE} fget_IsolineColor {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_IsolineColor {$ENDIF} ;

      /// <summary>
      ///   DEM wall color
      /// </summary>
      property WallsColor      : TGIS_Color     write {$IFNDEF OXYGENE} fset_SolidWallColor {$ENDIF} ;

      /// <summary>
      ///   Flood parameters, active, level &amp; color, transparency
      /// </summary>
      property Flood           : TGIS_Viewer3DFlood
                                                read  {$IFNDEF OXYGENE} fget_Flood {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_Flood {$ENDIF} ;

      /// <summary>
      ///   BasePlane parameters, active, level &amp; colors
      /// </summary>
      property BasePlane       : TGIS_Viewer3DBasePlane
                                                read  {$IFNDEF OXYGENE} fget_BasePlane {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_BasePlane {$ENDIF} ;

      /// <summary>
      ///   Get 3D extent visible in 3D window.
      /// </summary>
      property VisibleExtent3D : TGIS_Extent3D  read  {$IFNDEF OXYGENE} fget_VisibleExtent3D {$ENDIF} ;

      /// <summary>
      ///   Get/set 2D extent visible in 3D window.
      /// </summary>
      property VisibleExtent   : TGIS_Extent    read  {$IFNDEF OXYGENE} fget_VisibleExtent {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_VisibleExtent {$ENDIF} ;


      /// <summary>
      ///   Get 3D zoom, same as TGIS_Viewer.Zoom
      /// </summary>
      property Zoom            : Double         read  {$IFNDEF OXYGENE} fget_ZoomFactor {$ENDIF} ;

      /// <summary>
      ///   Get AllowDemTransparency status. True allows DEM to be
      ///   transparent, False will follow DEM layer property settings
      /// </summary>
      property AllowDemTransparency : Boolean   read  {$IFNDEF OXYGENE} fget_DemTransparency {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_DemTransparency {$ENDIF} ;

      /// <summary>
      ///   Transparency priority. Order of DEM/vector rendering. Types of
      ///   transparency priority in case that both DEM &amp; vector are
      ///   transparent (default is Auto)
      /// </summary>
      property TransparencyPriority : TGIS_Viewer3DTransparencyPriority
                                                read  {$IFNDEF OXYGENE} fget_TransparencyPriority {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_TransparencyPriority {$ENDIF};

      /// <summary>
      ///   Current 3D view restrictions.
      /// </summary>
      property ViewRestriction      : TGIS_Viewer3DViewRestriction
                                                read  {$IFNDEF OXYGENE} fget_Restriction {$ENDIF}
                                                write {$IFNDEF OXYGENE} fset_Restriction {$ENDIF} ;

      /// <summary>
      ///   Get error message String.
      /// </summary>
      property ErrorMessage         : String       read  {$IFNDEF OXYGENE} fget_ErrorMsg {$ENDIF} ;

      /// <summary>
      ///   Get the pixel size (xsize &amp; ysize) at reference point in map
      ///   units
      /// </summary>
      property PixelSize            : TGIS_Point   read  {$IFNDEF OXYGENE} fget_PixelSize {$ENDIF} ;

      /// <summary>
      ///   Extent in which DEM is displayed in max LevelOfDetail (must be
      ///   &gt;= 0.5, 1 means screen width).
      /// </summary>
      property DemDetailExtentFactor   : Double
                                         read  {$IFNDEF OXYGENE} fget_DemDetailExtentFactor {$ENDIF}
                                         write {$IFNDEF OXYGENE} fset_DemDetailExtentFactor {$ENDIF} ;

      /// <summary>
      ///   Extent in which DEM is displayed in lower LevelOfDetail (must be
      ///   &gt;= 1.0, 4 means 4 times screen width).
      /// </summary>
      property DemDraftExtentFactor    : Double
                                         read  {$IFNDEF OXYGENE} fget_DemDraftExtentFactor {$ENDIF}
                                         write {$IFNDEF OXYGENE} fset_DemDraftExtentFactor {$ENDIF}  ;

      /// <summary>
      ///   Extent from which vector is displayed (must be &gt;= 1.0, 2 means
      ///   2 times screen width).
      /// </summary>
      property VectorExtentFactor      : Double
                                         read  {$IFNDEF OXYGENE} fget_VectorExtentFactor {$ENDIF}
                                         write {$IFNDEF OXYGENE} fset_VectorExtentFactor {$ENDIF}    ;

      /// <summary>
      ///   Enable/disable vector simplification process.
      /// </summary>
      property VectorSimplification    : Boolean
                                         read  {$IFNDEF OXYGENE} fget_VectorSimplification {$ENDIF}
                                         write {$IFNDEF OXYGENE} fset_VectorSimplification {$ENDIF}  ;

      /// <summary>
      ///   Shape size in pixels, less or equal will not be displayed ( must
      ///   be &gt;= 0 )
      /// </summary>
      property VectorSmartSize         : Integer
                                         read  {$IFNDEF OXYGENE} fget_VectorSmartSize {$ENDIF}
                                         write {$IFNDEF OXYGENE} fset_VectorSmartSize  {$ENDIF}      ;

      /// <summary>
      ///   Current grid size setting.
      /// </summary>
      property DemGridSize             : Integer
                                         read  {$IFNDEF OXYGENE} fget_DemGridSize  {$ENDIF}          ;

      /// <summary>
      ///   Grid size used to display regular DEMs &amp; IMAGEs
      /// </summary>
      property DemCachedSize           : TGIS_Viewer3DDemCacheSize
                                         read  {$IFNDEF OXYGENE} fget_DemCachedSize {$ENDIF}
                                         write {$IFNDEF OXYGENE} fset_DemCachedSize  {$ENDIF}        ;

      /// <summary>
      ///   Set noDataValue for DEM cells with z &gt; IgnoreAbove value
      /// </summary>
      property IgnoreAbove             : Double
                                         read  {$IFNDEF OXYGENE} fget_IgnoreAbove {$ENDIF}
                                         write {$IFNDEF OXYGENE} fset_IgnoreAbove  {$ENDIF}          ;

      /// <summary>
      ///   Set noDataValue for DEM cells with z &lt; IgnoreBelow value
      /// </summary>
      property IgnoreBelow             : Double
                                         read  {$IFNDEF OXYGENE} fget_IgnoreBelow {$ENDIF}
                                         write {$IFNDEF OXYGENE} fset_IgnoreBelow  {$ENDIF}          ;

      /// <summary>
      ///   Set CutAbove value for DEM cells with z &gt; CutAbove
      /// </summary>
      property CutAbove                : Double
                                         read  {$IFNDEF OXYGENE} fget_CutAbove {$ENDIF}
                                         write {$IFNDEF OXYGENE} fset_CutAbove  {$ENDIF}             ;

      /// <summary>
      ///   Set CutBelow value for DEM cells with z &lt; CutBelow
      /// </summary>
      property CutBelow                : Double
                                         read  {$IFNDEF OXYGENE} fget_CutBelow {$ENDIF}
                                         write {$IFNDEF OXYGENE} fset_CutBelow  {$ENDIF}             ;

      /// <summary>
      ///   Block scene redrawing when set to True, immediately redraws scene
      ///   when set to False
      /// </summary>
      property FastMode                : Boolean
                                         read  {$IFNDEF OXYGENE} fget_FastMode {$ENDIF}
                                         write {$IFNDEF OXYGENE} fset_FastMode {$ENDIF}              ;

      /// <summary>
      ///   Revers Light normal Vector
      /// </summary>
      property LightVector             : Boolean
                                         read  {$IFNDEF OXYGENE} fget_LightVector {$ENDIF}
                                         write {$IFNDEF OXYGENE} fset_LightVector {$ENDIF}           ;

      /// <summary>
      ///   If set to True, differences in Z value caused by different
      ///   ellipsoids will be ignored
      /// </summary>
      property IgnoreEllipsoidHeight   : Boolean
                                         read  {$IFNDEF OXYGENE} fget_IgnoreEllipsoidHeight {$ENDIF}
                                         write {$IFNDEF OXYGENE} fset_IgnoreEllipsoidHeight {$ENDIF} ;

      /// <summary>
      ///   Standard / AdvanceNavigation switch if True mouse location become
      ///   a rotation point if False screen center is a rotation point
      /// </summary>
      property AdvNavigation           : Boolean
                                         read  {$IFNDEF OXYGENE} fget_AdvNavigation {$ENDIF}
                                         write {$IFNDEF OXYGENE} fset_AdvNavigation {$ENDIF} ;

      /// <summary>
      ///   Orthogonal View switch, if, True camera looks orthogonally,
      ///   camera can be moved only in XY plane, zoom is allowed
      /// </summary>
      property OrthoView               : Boolean
                                         read  {$IFNDEF OXYGENE} fget_OrthoView {$ENDIF}
                                         write {$IFNDEF OXYGENE} fset_OrthoView {$ENDIF} ;

      /// <summary>
      ///   Flag. If True, Viewer is busy in painting.
      /// </summary>
      property IsBusy               : Boolean
                                         read  {$IFNDEF OXYGENE} fget_IsBusy {$ENDIF} ;

      /// <summary>
      ///   Draw scene
      /// </summary>
      procedure Draw                  ;

      /// <summary>
      ///   Prepare navigate operation - for example set draft mode. Must be
      ///   paired with UnlockUpdates.
      /// </summary>
      procedure LockUpdates           ;

      /// <summary>
      ///   Finalize navigate operation. Must be paired with LockUpdates.
      /// </summary>
      procedure UnlockUpdates         ;

      /// <summary>
      ///   Stop rendering and wait for GeneralUnlock Must be paired with Unlock.
      /// </summary>
      procedure Lock                  ;

      /// <summary>
      ///   Cancels GeneralLock, continue rendering Must be paired with Lock.
      /// </summary>
      procedure Unlock                ;

      /// <summary>
      ///   Get the mouse position on ReferenceLevel in map units
      /// </summary>
      /// <param name="_pt">
      ///   screen pixel position
      /// </param>
      /// <returns>
      ///   screen coordinates converted to map coordinates
      /// </returns>
      function  ScreenToMap           (  const _pt      : TPoint
                                      ) : TGIS_Point ;

      /// <summary>
      ///   Get map coordinates of screen pixel
      /// </summary>
      /// <param name="_pt">
      ///   screen pixel position
      /// </param>
      /// <returns>
      ///   screen coordinates converted to map coordinates
      /// </returns>
      function  ScreenToMap3D         (  const _pt      : TPoint
                                      ) : TGIS_Point3D ;

      /// <summary>
      ///   Get intersection point between a ray and a DEM
      /// </summary>
      /// <param name="_orig">
      ///   ray origin coordinates in map units
      /// </param>
      /// <param name="_dir">
      ///   ray direction
      /// </param>
      /// <param name="_ptg">
      ///   intersection point between a ray and a DEM if Result = True
      /// </param>
      /// <returns>
      ///   True if ray intersects DEM, False if not
      /// </returns>
      /// <remarks>
      ///   ScaleZ property must be set to 1.0
      ///   AdvNavigation must be set to False
      /// </remarks>
      function  RayIntersectDem       (  const _orig    : TGIS_Point3D ;
                                         const _dir     : TGIS_Point3D ;
                                         out   _ptg     : TGIS_Point3D
                                      ) : Boolean ;

      /// <summary>
      ///   Dump required screen rectangle to a bitmap
      /// </summary>
      /// <param name="_rect">
      ///   required screen rectangle to be returned as TGIS_Bitmap
      /// </param>
      /// <param name="_bmp">
      ///   32 bits TBitmap returned as a result
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    if aspect ratios of _rect and _bmp are different procedure will
      ///    preserve _rect ratio (empty margins will occur on _bmp)
      ///    </note>
      /// </remarks>
      procedure PrintBmp              ( const _rect     : TRect ;
                                        const _bmp      : TGIS_Bitmap
                                      ) ;

      /// <summary>
      ///   Get size of tile used while tiling.
      /// </summary>
      /// <param name="_width">
      ///   output bitmap width
      /// </param>
      /// <param name="_height">
      ///   output bitmap height
      /// </param>
      /// <returns>
      ///   Tile size to be used as a param in PrintTile method
      /// </returns>
      function  PrintBegin             ( const _width  : Integer ;
                                         const _height : Integer
                                       ) : TPoint ;

      /// <summary>
      ///   Dump visible screen rectangle to a bitmap
      /// </summary>
      /// <param name="_bmp">
      ///   32 bits TBitmap filled with tile data
      /// </param>
      /// <param name="_offsetx">
      ///   offset in x direction (in pixels)
      /// </param>
      /// <param name="_offsety">
      ///   offset in x direction (in pixels)
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    if aspect ratios of _rect and _bmp are different procedure will
      ///    preserve _rect ratio (empty margins will occur on _bmp)
      ///    </note>
      /// </remarks>
      procedure PrintTile             ( const _bmp      : TGIS_Bitmap ;
                                        const _offsetx  : Integer ;
                                        const _offsety  : Integer
                                      ) ;

      /// <summary>
      ///   Close printing using tiles.
      /// </summary>
      procedure PrintEnd              ;

      /// <summary>
      ///   Rotate view by delta values expressed in radians
      /// </summary>
      /// <param name="_delta">
      ///   delta values
      /// </param>
      procedure Rotate                ( const _delta    : TGIS_Point3D
                                      ) ;

      /// <summary>
      ///   Move scene along XY axis by delta values expressed in map units
      /// </summary>
      /// <param name="_delta">
      ///   delta values
      /// </param>
      procedure Move                  ( const _delta    : TGIS_Point3D
                                      ) ;

      /// <summary>
      ///   Drag scene left, right, forward or backward by delta in map units
      /// </summary>
      /// <param name="_delta">
      ///   delta values
      /// </param>
      procedure Drag                  ( const _delta    : TGIS_Point3D
                                      ) ;

      /// <summary>
      ///   Drag scene left, right, forward+up or backward+down by delta in map
      ///   units
      /// </summary>
      /// <param name="_delta">
      ///   delta values
      /// </param>
      procedure DragEx                ( const _delta    : TGIS_Point3D
                                      ) ;

      /// <summary>
      ///   Zoom view by factor (screen center if AdvNavigation = False,
      ///   mouse position if AdvNavigation = True.
      /// </summary>
      /// <param name="_value">
      ///   zoom multiplier to multiply current zoom value
      /// </param>
      /// <param name="_x">
      ///   center point x coordinate
      /// </param>
      /// <param name="_y">
      ///   center point y coordinate
      /// </param>
      procedure ZoomBy                ( const _value    : Double ;
                                        const _x        : Integer ;
                                        const _y        : Integer
                                      ) ;

      /// <summary>
      ///   Perform interactive action on mouse message defined by current Mode.
      /// </summary>
      /// <param name="_xpos">
      ///   x mouse position
      /// </param>
      /// <param name="_ypos">
      ///   y mouse position
      /// </param>
      procedure DoMouseDown           ( const _xpos      : SmallInt ;
                                        const _ypos      : SmallInt
                                      ) ;

      /// <summary>
      ///   Perform interactive action on mouse message defined by current Mode.
      /// </summary>
      procedure DoMouseUp             ;

      /// <summary>
      ///   Perform interactive action on mouse message defined by current Mode.
      /// </summary>
      /// <param name="_xpos">
      ///   x mouse position
      /// </param>
      /// <param name="_ypos">
      ///   y mouse position
      /// </param>
      procedure DoMouseMove           ( const _xpos      : SmallInt ;
                                        const _ypos      : SmallInt
                                      ) ;

      /// <summary>
      ///   Start gesture.
      /// </summary>
      procedure GestureBegin          ;

      /// <summary>
      ///   Do gesture.
      /// </summary>
      /// <param name="_dragdx">
      ///   change of x coordinate for dragging
      /// </param>
      /// <param name="_dragdy">
      ///   change of y coordinate for dragging
      /// </param>
      /// <param name="_zoomxpos">
      ///   x coordinate of zoom center position
      /// </param>
      /// <param name="_zoomypos">
      ///   y coordinate of zoom center position
      /// </param>
      /// <param name="_zoomdistance">
      ///   zoom distance; if set to 1 then nothing is done
      /// </param>
      /// <param name="_camerapositiondx">
      ///   change of x coordinate for camera position mode
      /// </param>
      /// <param name="_camerapositiondy">
      ///   change of y coordinate for camera position mode
      /// </param>
      /// <param name="_paramex">
      ///   not used
      /// </param>
      procedure DoGesture             ( const _dragdx       : SmallInt ;
                                        const _dragdy       : SmallInt ;
                                        const _zoomxpos     : SmallInt ;
                                        const _zoomypos     : SmallInt ;
                                        const _zoomdistance : Double   ;
                                        const _camerapositiondx
                                                            : SmallInt ;
                                        const _camerapositiondy
                                                            : SmallInt ;
                                        const _paramex      : Double
                                      ) ;

      /// <summary>
      ///   Finish gesture.
      /// </summary>
      procedure GestureEnd ;

      /// <summary>
      ///   Set view to initial state (like FullExtent in 2D).
      /// </summary>
      procedure ResetView             ;

      /// <summary>
      ///   Set view to initial state.
      /// </summary>
      procedure FullExtent            ;

      /// <summary>
      ///   Get height where reference pointer crosses DEM
      /// </summary>
      /// <returns>
      ///   Height in vertical units.
      /// </returns>
      function  GetDemLevelAtReferencePointer
                                      : Double  ;

      /// <summary>
      ///   Get rendering speed in frames-per-second
      /// </summary>
      /// <returns>
      ///   Frame per seconds.
      /// </returns>
      function  GetFps                : Integer ;


      /// <summary>
      ///   Mouse position (necessary in ZoomMode only)
      /// </summary>
      /// <param name="_value">
      ///   last mouseDown position
      /// </param>
      procedure StoreMousePos         ( const _value    : TPoint
                                      ) ;

      /// <summary>
      ///   Select object in 3D (as in 2D mode)
      /// </summary>
      /// <param name="_pt">
      ///   screen coordinate
      /// </param>
      /// <param name="_prec">
      ///   precision in pixels (must be &gt;= 0)
      /// </param>
      /// <returns>
      ///   Shape itself or nil,
      /// </returns>
      function  Locate                ( const _pt       : TPoint             ;
                                        const _prec     : Integer
                                      ) : TGIS_ShapeAbstract ;

      /// <summary>
      ///   Select object in 3D
      /// </summary>
      /// <param name="_pt">
      ///   screen coordinate
      /// </param>
      /// <param name="_prec">
      ///   precision in pixels (must be &gt;= 0)
      /// </param>
      /// <param name="_layer">
      ///   input : layer to be searched, if nil all layers will be searched
      ///   output : layer contained located object, if nil no object located
      /// </param>
      /// <param name="_ptg">
      ///   hitting point
      /// </param>
      /// <param name="_shp">
      ///   located shape or nil
      /// </param>
      /// <param name="_part">
      ///   located shape part number
      /// </param>
      /// <returns>
      ///   True if shape was found
      /// </returns>
      function  Locate3D               ( const _pt       : TPoint             ;
                                         const _prec     : Integer            ;
                                         var   _layer    : TGIS_LayerAbstract ;
                                         var   _ptg      : TGIS_Point3D       ;
                                         var   _shp      : TGIS_ShapeAbstract ;
                                         var   _part     : Integer
                                       ) : Boolean ;

      /// <summary>
      ///   Mark shape with color
      /// </summary>
      /// <param name="_layer">
      ///   layer contained shape
      /// </param>
      /// <param name="_shpid">
      ///   shape to be marked
      /// </param>
      /// <param name="_part">
      ///   shape part to be marked, -1 if entire shape
      /// </param>
      /// <param name="_color">
      ///   color used to mark shape
      /// </param>
      /// <param name="_update">
      ///   if True redraw scene after mark, do not redraw if False
      /// </param>
      procedure MarkShape             ( const _layer     : TGIS_LayerAbstract ;
                                        const _shpid     : Integer            ;
                                        const _part      : Integer            ;
                                        const _color     : TGIS_Color         ;
                                        const _update    : Boolean
                                      ) ;

      /// <summary>
      ///   Cancel previously marked shape with color
      /// </summary>
      /// <param name="_layer">
      ///   layer contained shape
      /// </param>
      /// <param name="_shpid">
      ///   shape to be unmarked
      /// </param>
      /// <param name="_update">
      ///   if True redraw scene after mark, do not redraw if False
      /// </param>
      procedure UnMarkShape           ( const _layer     : TGIS_LayerAbstract ;
                                        const _shpid     : Integer            ;
                                        const _update    : Boolean
                                      ) ;

      /// <summary>
      ///   Redraw all selected objects.
      /// </summary>
      procedure UpdateAllSelectedObjects ;

      /// <summary>
      ///   Construct an instance of 3D Viewer
      /// </summary>
      /// <param name="_extent">
      ///   extent restriction (all objects outside will be cut off)
      /// </param>
      /// <param name="_initdraw">
      ///   draw initial view if True, do not draw if False
      /// </param>
      /// <returns>
      ///   True if success
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     To support automatic mouse handling object should be created on a
      ///   </note>
      /// </remarks>
      function InitialRedraw          ( const _extent    : TGIS_Extent        ;
                                        const _initdraw  : Boolean
                                      ) : Boolean ;

      /// <summary>
      ///   Issue control update action (deep refresh).
      /// </summary>
      procedure UpdateWholeMap ;

      /// <summary>
      ///   Issue control update action (topmost refresh).
      /// </summary>
      procedure UpdateTopmost ;

      /// <summary>
      ///   Issue control repainting action (shallow refresh).
      /// </summary>
      procedure ControlRepaint ;

  end ;
{$ENDIF}

{$IFDEF OXYGENE}
  /// <summary>
  ///   Uncounted interface object used mainly to reference IGIS_Viewer object
  /// </summary>
  TGIS_UncountedInterfacedObject = public interface
  end ;
{$ELSE}
  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  /// <summary>
  ///   Uncounted interface object used mainly to reference IGIS_Viewer object
  /// </summary>
  TGIS_UncountedInterfacedObject = class(TGIS_Object, IInterface)
    function QueryInterface ( const _iid : TGUID;
                              out   _obj
                            ) : HResult; stdcall;
    function _AddRef        : Integer; stdcall;
    function _Release       : Integer; stdcall;
  end ;
{$ENDIF}

  {#gendoc:hide:GENSCR}
  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  /// <summary>
  ///   Common API for legend parent operations. To be shared across platforms.
  /// </summary>
  /// <remarks>
  ///   Do not call this methods and properties of this interface directly.
  ///   This interface has been design for internal Legend/Parent
  ///   communication only.
  /// </remarks>
  IGIS_LegendParent = {$IFDEF OXYGENE} public partial {$ENDIF} interface
    {$IFDEF DCC}
      ['{D00E1CDA-A728-46BD-BB31-E006DB882510}']
    {$ENDIF}

    {$REGION IGIS_LegendParent public methods}
      /// <summary>
      ///   Gets control width.
      /// </summary>
      /// <returns>
      ///   width.
      /// </returns>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      function  ControlWidth           : Integer ;

      /// <summary>
      ///   Gets control height.
      /// </summary>
      /// <returns>
      ///   height.
      /// </returns>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      function  ControlHeight          : Integer ;

      /// <summary>
      ///   Gets client width.
      /// </summary>
      /// <returns>
      ///   width.
      /// </returns>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      function  ControlClientWidth     : Integer ;

      /// <summary>
      ///   Gets client height.
      /// </summary>
      /// <returns>
      ///   height.
      /// </returns>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      function  ControlClientHeight    : Integer ;

      {$IFDEF DCC}
        /// <summary>
        ///   Passes the touch event context.
        /// </summary>
        /// <param name="_context">
        ///   context of the touch event
        /// </param>
        /// <remarks>
        ///   Do not call this method directly.
        ///   Method is only for internal Legend/Parent communication.
        /// </remarks>
        procedure ControlTouch         ( const _context : TObject
                                       ) ;
      {$ENDIF}

      /// <summary>
      ///   Gets vertical scrollbar position.
      /// </summary>
      /// <returns>
      ///   vertical scroll position
      /// </returns>
      function  ControlGetVScrollPosition : Integer ;

      /// <summary>
      ///   Sets vertical scrollbar position.
      /// </summary>
      /// <param name="_position">
      ///   position to set
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlSetVScrollPosition
                                       ( const _position : Integer
                                       ) ;

      /// <summary>
      ///   Gets horizontal scrollbar position.
      /// </summary>
      /// <returns>
      ///   horizontal scroll position
      /// </returns>
      function  ControlGetHScrollPosition : Integer ;

      /// <summary>
      ///   Sets horizontal scrollbar position.
      /// </summary>
      /// <param name="_position">
      ///   position to set
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlSetHScrollPosition
                                       ( const _position : Integer
                                       ) ;

      /// <summary>
      ///   Sets vertical scrollbar range & position.
      /// </summary>
      /// <param name="_minPosition">
      ///   minimal position
      /// </param>
      /// <param name="_maxPosition">
      ///   maximal position
      /// </param>
      /// <param name="_position">
      ///   position to set
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlSetVScroll      ( const _minPosition : Integer ;
                                         const _maxPosition : Integer ;
                                         const _position    : Integer
                                       ) ;

      /// <summary>
      ///   Sets horizontal scrollbar range & position.
      /// </summary>
      /// <param name="_minPosition">
      ///   minimal position
      /// </param>
      /// <param name="_maxPosition">
      ///   maximal position
      /// </param>
      /// <param name="_position">
      ///   position to set
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlSetHScroll      ( const _minPosition : Integer ;
                                         const _maxPosition : Integer ;
                                         const _position    : Integer
                                       ) ;

      /// <summary>
      ///   Repaints the control.
      /// </summary>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlRepaint         ;

      /// <summary>
      ///   Checks if the control is in design mode.
      /// </summary>
      /// <returns>
      ///   If true, the control is in design mode.
      /// </returns>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      function  ControlIsDesignMode    : Boolean ;

      /// <summary>
      ///   Checks if the control is in 'right to left' mode.
      /// </summary>
      /// <returns>
      ///   If true, the control is in 'right to left' mode.
      /// </returns>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      function  ControlRightToLeft     : Boolean ;

      /// <summary>
      ///   Does subscribed update.
      /// </summary>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlSubscribedUpdate ;

      /// <summary>
      ///   Does update.
      /// </summary>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlUpdate          ;

      /// <summary>
      ///   Causes repaint of the entire control.
      /// </summary>
      /// <param name="_updvwr">
      ///   True if the attached IGIS_Viewer should be updated too.
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlFullUpdate      ( const _updvwr  : Boolean
                                       ) ;

      /// <summary>
      ///   Draws a node.
      /// </summary>
      /// <param name="_node">
      ///   node to draw
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlDrawNode        ( const _node     : TObject
                                       ) ;

      /// <summary>
      ///   Draws drag bar.
      /// </summary>
      /// <param name="_node">
      ///   node to drag
      /// </param>
      /// <param name="_top">
      ///   top of the moved node
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlDragDrawBar     ( const _node     : TObject ;
                                         const _top      : Integer
                                       ) ;

      /// <summary>
      ///   Draws a node for dragging.
      /// </summary>
      /// <param name="_node">
      ///   node to draw
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlDragPrepareNode ( const _node     : TObject
                                       ) ;

      /// <summary>
      ///   Draws drag bitmap.
      /// </summary>
      /// <param name="_left">
      ///   x-coordinate of the upper-left corner
      /// </param>
      /// <param name="_top">
      ///   y-coordinate of the upper-left corner
      /// </param>
      /// <param name="_transparency">
      ///   transparency (value between 0 and 1)
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlDragDrawNode    ( const _left     : Integer ;
                                         const _top      : Integer ;
                                         const _transparency
                                                         : Single
                                       ) ;

      /// <summary>
      ///   Frees a node for dragging.
      /// </summary>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlDragFreeNode    ;

      /// <summary>
      ///   Creates temporary context to avoid flickering.
      /// </summary>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlDragCreateTemporaryContext ;

      /// <summary>
      ///   Renders temporary context and destroy it.
      /// </summary>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlDragRenderTemporaryContext ;

      /// <summary>
      ///   Draws cache.
      /// </summary>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlDrawFromCache   ;

      /// <summary>
      ///   Gets renderer instance.
      /// </summary>
      /// <returns>
      ///   renderer.
      /// </returns>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Viewer/Parent communication.
      /// </remarks>
      function  ControlRenderer        : TObject ;

      /// <summary>
      ///   Gets background color.
      /// </summary>
      /// <returns>
      ///   current set background color
      /// </returns>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      function  ControlStyleGetColor   : TGIS_Color ;

      /// <summary>
      ///   Sets background color.
      /// </summary>
      /// <param name="_selected">
      ///   if True, it sets a brush color for selected nodes
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlStyleSetColor   ( const _selected : Boolean
                                       ) ;

      /// <summary>
      ///   Draws a rectangle.
      /// </summary>
      /// <param name="_rect">
      ///   rectangle to draw
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlStyleDrawRectangle
                                       ( const _rect     : TRect
                                       ) ; overload ;

      /// <summary>
      ///   Draws a rectangle.
      /// </summary>
      /// <param name="_left">
      ///   x-coordinate of the upper-left corner
      /// </param>
      /// <param name="_top">
      ///   y-coordinate of the upper-left corner
      /// </param>
      /// <param name="_width">
      ///   width
      /// </param>
      /// <param name="_height">
      ///   height
      /// </param>
      /// <param name="_brush">
      ///   brush color
      /// </param>
      /// <param name="_pen">
      ///   pen color
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlStyleDrawRectangle
                                       ( const _left     : Integer ;
                                         const _top      : Integer ;
                                         const _width    : Integer ;
                                         const _height   : Integer ;
                                         const _brush    : TGIS_Color ;
                                         const _pen      : TGIS_Color
                                       ) ; overload ;

      /// <summary>
      ///   Draws a check box.
      /// </summary>
      /// <param name="_checked">
      ///   if checked
      /// </param>
      /// <param name="_rect">
      ///   rectangle
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlStyleDrawCheckBox
                                       ( const _checked  : Boolean ;
                                         const _rect     : TRect
                                       ) ;

      /// <summary>
      ///   Draws an expand/collapse marker.
      /// </summary>
      /// <param name="_expanded">
      ///   if expanded
      /// </param>
      /// <param name="_rect">
      ///   rectangle
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlStyleDrawExpandCollapseMarker
                                       ( const _expanded : Boolean ;
                                         const _rect     : TRect
                                       ) ;

      /// <summary>
      ///   Gets text extent.
      /// </summary>
      /// <param name="_selected">
      ///   if node is selected
      /// </param>
      /// <param name="_text">
      ///   text to measure
      /// </param>
      /// <returns>
      ///   extent
      /// </returns>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      function  ControlStyleGetTextExtent
                                       ( const _selected : Boolean ;
                                         const _text     : String
                                       ) : TPoint ;

      /// <summary>
      ///   Draws text.
      /// </summary>
      /// <param name="_selected">
      ///   if node is selected
      /// </param>
      /// <param name="_text">
      ///   text to draw
      /// </param>
      /// <param name="_rect">
      ///   rectangle
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlStyleDrawText   ( const _selected : Boolean ;
                                         const _text     : String  ;
                                               _rect     : TRect
                                       ) ;

      /// <summary>
      ///   Creates temporary context.
      /// </summary>
      /// <param name="_width">
      ///   width
      /// </param>
      /// <param name="_height">
      ///   height
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlStyleCreateTemporaryContext
                                       ( const _width    : Integer ;
                                         const _height   : Integer
                                       ) ;

      /// <summary>
      ///   Renders temporary context and destroy it.
      /// </summary>
      /// <param name="_left">
      ///   x-coordinate of the upper-left corner
      /// </param>
      /// <param name="_top">
      ///   y-coordinate of the upper-left corner
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlStyleRenderTemporaryContext
                                       ( const _left     : Integer ;
                                         const _top      : Integer
                                       ) ;

      /// <summary>
      ///   Draws image on temporary context.
      /// </summary>
      /// <param name="_bitmap">
      ///   image
      /// </param>
      /// <param name="_left">
      ///   x-coordinate of the upper-left corner
      /// </param>
      /// <param name="_top">
      ///   y-coordinate of the upper-left corner
      /// </param>
      /// <param name="_transparent">
      ///   transparent
      /// </param>
      /// <remarks>
      ///   Do not call this method directly.
      ///   Method is only for internal Legend/Parent communication.
      /// </remarks>
      procedure ControlStyleDrawImage  ( const _bitmap   : TGIS_Bitmap ;
                                         const _left     : Integer ;
                                         const _top      : Integer ;
                                         const _transparent
                                                         : Boolean
                                       ) ;

    {$ENDREGION}

    {$REGION IGIS_LegendParent events}
      (*
        The following events are commented out intentionally.
        These events should be implemented in a descendant
        class for full operability but are not necessary.

        /// <summary>
        ///   BeforePaint event. Will be fired before any Paint
        ///   operation.
        /// </summary>
        property BeforePaintEvent : TGIS_PaintEvent ;

        /// <summary>
        ///   AfterPaint event. Will be fired after any Paint
        ///   operation.
        /// </summary>
        property AfterPaintEvent  : TGIS_PaintEvent ;

        /// <summary>
        ///   PaintExtra event. Called after renderer.PaintExtra()
        ///   method.
        /// </summary>
        property PaintExtraEvent  : TGIS_RendererEvent ;

        /// <summary>
        ///   BeforeUpdate event. Will be fired before Update
        ///   operation.
        /// </summary>
        property BeforeUpdateEvent : TGIS_RendererEvent ;

        /// <summary>
        ///   Update event. Will be fired for Update operation.
        ///   If attached, then Draw(renderer) method will not be called
        ///   internally.
        /// </summary>
        property UpdateEvent      : TGIS_RendererEvent ;

        /// <summary>
        ///   AfterUpdate event. Will be fired after Update
        ///   operation.
        /// </summary>
        property AfterUpdateEvent : TGIS_RendererEvent ;
      *)
    {$ENDREGION}
  end;

//##############################################################################
implementation

{$IFNDEF OXYGENE}
//==============================================================================
// TGIS_UncountedInterfacedObject
//==============================================================================
  function TGIS_UncountedInterfacedObject.QueryInterface(
    const _iid : TGUID ;
    out   _obj
  ): HResult;
  begin
    if GetInterface( _iid, _obj ) then
      Result := S_OK
    else
      Result := E_NOINTERFACE;
  end;

  function TGIS_UncountedInterfacedObject._AddRef
    : Integer ;
  begin
    Result := -1;
  end;

  function TGIS_UncountedInterfacedObject._Release
    : Integer ;
  begin
    Result := -1;
  end;
{$ENDIF}

//==================================== END =====================================
end.


