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
  Main unit of TatukGIS.

  In this unit all basic objects were implemented including layers and basic
  viewer's behavior.

  Internal representation of a shape closely mimics SHP-file. Specially, the
  representation of parts, points, point count, and part count remain identical
  in the both cases.

  Shape naming convention is delivered from the SHP-File specification
}

{$IFDEF DCC}
  unit GisViewer ;
  {$HPPEMIT '#pragma link "GisViewer"'}
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

{$IFNDEF OXYGENE}
  {$M+}
{$ENDIF}

interface

{$IFDEF CLR}
  uses
    System.ComponentModel,
    System.Drawing,
    System.Collections.Generic,
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    System.Math,
    System.Types,

    System.Generics.Defaults,
    System.Generics.Collections,

    GisRtl,
    GisLogger,
    GisInterfaces,
    GisTypes,
    GisTypesUI,
    GisClasses,
    GisConfig,
    GisCsSystems,
    GisEditor,
    GisHierarchy,
    GisLabelsArea,
    GisLayer;
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
  {$IFDEF OXYGENE}
    T_Locked nested in TGIS_Viewer = record
      public
        /// <summary>
        ///   If > 0, then Viewer is in locked state Lock.
        /// </summary>
        Level    : Integer     ;

        /// <summary>
        ///   If > 0, then Viewer is in repaqrent locked state Lock.
        /// </summary>
        Reparent : Integer     ;

        /// <summary>
        ///   True if whole maps should be refreshed.
        /// </summary>
        WholeMap : Boolean      ;

        /// <summary>
        ///   True if only topmost layers should be refreshed.
        /// </summary>
        Topmost : Boolean       ;

        /// <summary>
        ///   True if selection were modified.
        /// </summary>
        Selection : Boolean     ;

        /// <summary>
        ///   True if Editor was modified.
        /// </summary>
        Editor : Boolean     ;

        /// <summary>
        ///   Extent to be refreshed on unlock.
        /// </summary>
        Extent   : TGIS_Extent ;
    end ;

    T_lastPaint nested in TGIS_Viewer = public record
      public

      /// <summary>
      ///   Zoom value after last draw or 0.
      /// </summary>
      Zoom : Double ;

      /// <summary>
      ///   VisibleExtent value after last draw or GisNoWorld. }
      /// </summary>
      VisibleExtent : TGIS_Extent ;
    end ;

    T_oldProperties nested in TGIS_Viewer = public record
      public

      /// <summary>
      ///   True if properties was altered.
      /// </summary>
      Modified : Boolean ;

      /// <summary>
      ///   See property Color.
      /// </summary>
      Color    : TGIS_Color ;

      /// <summary>
      ///   See property IncrementalPaint.
      /// </summary>
      IncrementalPaint : Boolean ;

    end ;
  {$ENDIF}

  {$IFDEF CLR}
    [ToolboxItem(False)]
  {$ENDIF}

  /// <summary>
  ///   Encapsulation of the generic viewer. This viewer will not be
  ///   accessed directly. Instead you can use TGIS_ViewerWnd,
  ///   TGIS_ViewerBmp, or any future successors.
  /// </summary>
  TGIS_Viewer = {$IFDEF OXYGENE} public {$ENDIF} class(
    TGIS_UncountedInterfacedObject,
    IGIS_Viewer
    {$IFDEF CLR}
      ,IDisposable
    {$ENDIF}
  )
    private
    {$IFDEF OXYGENE} unit {$ENDIF}
      /// <summary>
      ///  Fast (non count referenced) reference to a parent viewer object.
      /// </summary>
      FSelfRef : TGIS_ViewerRef ;

      /// <summary>
      ///  Interface reference to parent object.
      /// </summary>
      {$IFDEF DCC} [weak] {$ENDIF}
      oParent : IGIS_ViewerParent ;

      /// <summary>
      ///   Legal notice and product version.
      /// </summary>
      FCopyright : String ;

      /// <summary>
      ///   Greater then 0 if Viewer is busy in Paint procedure.
      /// </summary>
      FInPaint : Integer ;

      /// <summary>
      ///   Greater then 0 if Viewer is busy during long-time operations.
      /// </summary>
      FBusyText : TGIS_KeyValueList<String,Integer> ;

      /// <summary>
      ///   Used to force rendering in a different scale then actual.
      /// </summary>
      FTemporaryScaleInternal : Double ;

      /// <summary>
      ///   Used to force rendering labels while drawing with tiles.
      /// </summary>
      FTemporaryVisibleExtent : TGIS_Extent ;

      /// <summary>
      ///   If true, then you can not drag outside the map Extent.
      /// </summary>
      FRestrictedDrag : Boolean ;

      /// <summary>
      ///   Delayed update threshold in milliseconds; if 0 then disable
      /// </summary>
      FDelayedUpdate : Integer ;

      /// <summary>
      ///   Progressive update threshold in milliseconds; if 0 then disable
      /// </summary>
      FProgressiveUpdate : Integer ;

      /// <summary>
      ///   True if partial drawing to screen is done in incremental mode.
      /// </summary>
      FIncrementalPaint : Boolean ;

      /// <summary>
      ///   True if partial drawing to screen is done in tiled mode.
      /// </summary>
      FTiledPaint : Boolean ;

      {$IFDEF OXYGENE}
        /// <summary>
        ///   Lock mode information.
        /// </summary>
        FLocked : T_Locked ;
      {$ELSE}
        /// <summary>
        ///   Lock mode information.
        /// </summary>
        FLocked : record
          /// <summary>
          ///   If > 0, then Viewer is in locked state Lock.
          /// <summary>
          Level    : Integer     ;

          /// <summary>
          ///   If > 0, then Viewer is in repaqrent locked state Lock.
          /// </summary>
          Reparent : Integer     ;

          /// <summary>
          ///   True if whole maps should be refreshed.
          /// <summary>
          WholeMap : Boolean      ;

          /// <summary>
          ///   True if only topmost layers should be refreshed.
          /// <summary>
          Topmost : Boolean       ;

          /// <summary>
          ///   True if selection were modified.
          /// <summary>
          Selection : Boolean     ;

          /// <summary>
          ///   True if Editor was modified.
          /// <summary>
          Editor : Boolean     ;

          /// <summary>
          ///   Extent to be refreshed on unlock.
          /// <summary>
          Extent   : TGIS_Extent ;
        end ;
      {$ENDIF}

      /// <summary>
      ///   Editor context.
      /// </summary>
      FEditor : TGIS_Editor ;

      /// <summary>
      ///   Labels collision detector class.
      /// </summary>
      FLabelsReg : TGIS_LabelsAreaAbstract ;

      /// <summary>
      ///   Project file name as opened in Open procedure.
      /// </summary>
      FProjectName : String ;

      /// <summary>
      ///   Project file itself as opened in Open procedure.
      /// </summary>
      FProjectFile : TGIS_Config ;

      /// <summary>
      ///   List of attached layers.
      /// </summary>
      FItems : TGIS_LayerAbstractList ;

      /// <summary>
      ///   Map position inside window.
      /// </summary>
      FViewport : TGIS_Point ;

      /// <summary>
      ///   If True, layers added to the viewer will have a random (unique) style applied.
      /// </summary>
      FAutoStyle : Boolean;

      /// <summary>
      ///   Point on which the Viewer will be centered during Zoom. Useful
      ///   for "Center on Click".
      /// </summary>
      FCenter : TGIS_Point ;

      /// <summary>
      ///   Map extent. Encompassing the collective extent.
      /// </summary>
      FExtent : TGIS_Extent ;

      /// <summary>
      ///   Scale for actual extent.
      /// </summary>
      FScale : Double ;

      /// <summary>
      ///   Extent of full drawing area (w/o tiling).
      /// </summary>
      FFullDrawExtent : TGIS_Extent ;

      /// <summary>
      ///   BigExtent margin.
      /// </summary>
      FBigExtentMargin : Integer ;

      /// <summary>
      ///   True if a viewer should maintain scale upon resize.
      /// </summary>
      FKeepScale : Boolean ;

      /// <summary>
      ///   Overlapped extent margin.
      /// </summary>
      FOverlappedExtentMargin : Integer ;

      /// <summary>
      ///   Restricted Map extent. Extent of the map will never be larger
      ///   then RestrictedExtent.
      /// </summary>
      FRestrictedExtent : TGIS_Extent ;

      /// <summary>
      ///   Angle of viewer rotation in radians.
      /// </summary>
      FRotationAngle : Double ;

      /// <summary>
      ///   Point of viewer rotation in map units.
      /// </summary>
      FRotationPoint : TGIS_Point ;

      /// <summary>
      ///   Related zoom factor. Zoom = 1 if one pixel = one map unit.
      /// </summary>
      FZoom : Double ;

      /// <summary>
      ///   Master Viewer reference. To be used only by TiledPaint.
      /// </summary>
      FMasterViewer : IGIS_Viewer ;

      /// <summary>
      ///   Background color.
      /// </summary>
      FColor : TGIS_Color ;

      /// <summary>
      ///   Rendering resolution in pixels-per-inch.
      /// </summary>
      FCustomPPI : Integer ;

      /// <summary>
      ///   Rendering resolution in pixels-per-inch.
      /// </summary>
      FCurrentPPI : Integer ;

      /// <summary>
      ///   Font magnifying factor in percents.
      /// </summary>
      FFontScale : Integer ;

      /// <summary>
      ///   Outline width of selection area.
      /// </summary>
      FSelectionWidth : Integer ;

      /// <summary>
      ///   True if only polygon outline will be drawn; False, if only
      ///   whole polygon will be filled.
      /// </summary>
      FSelectionOutlineOnly : Boolean ;

      /// <summary>
      ///   Color used for selection. Active if not white
      /// </summary>
      FSelectionGisColor : TGIS_Color ;

      /// <summary>
      ///   Selection transparency factor.
      /// </summary>
      FSelectionTransparency : Integer ;

      /// <summary>
      ///   Projection object.
      /// </summary>
      FCS : TGIS_CSCoordinateSystem ;

      /// <summary>
      ///   True if animations are allowed.
      /// </summary>
      FUseAnimations : Boolean ;

      /// <summary>
      ///   True if Rtree will be used be default.
      /// </summary>
      FUseRTree : Boolean ;

      /// <summary>
      ///   Hierarchy list.
      /// </summary>
      FHierarchy : IGIS_HierarchyManager ;

      /// <summary>
      ///   MultiUser mode.
      /// </summary>
      FMultiUserMode : TGIS_MultiUser ;

      /// <summary>
      ///   List of custom data.
      /// </summary>
      FCustomData  : TGIS_StringList ;

      /// <summary>
      ///   True if layer order was modified.
      /// </summary>
      FIsModified : Boolean ;

      /// <summary>
      ///   True if object is upon destroy.
      /// </summary>
      FUponDestroy : Boolean ;

      /// <summary>
      ///   True if event must be blocked (upon set of ScaleAsdFloat)
      /// </summary>
      bBlockRepositionEvents : Boolean ;


      /// <summary>
      ///   Kid of semaphor to keep WaitForNotBusy singleton
      /// </summary>
      iWaitForNotBusy : Integer ;

      /// <summary>
      ///   Path of temporary dir to delete
      /// </summary>
      FTemporaryDirToDelete : String ;

      FTiledDraw : Boolean ;


    protected // properties - events
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <summary>
      ///   PaintException event. Will be fired on exception at painting.
      /// </summary>
      FOnPaintException : TGIS_PaintExceptionEvent ;

      /// <summary>
      ///   ExtentChange event. Will be fired on TGIS_Viewer.Extent
      ///   change.
      /// </summary>
      FOnExtentChange :
         {$IFDEF CLR}
           EventHandler ;
         {$ELSE}
           TNotifyEvent ;
         {$ENDIF}

      /// <summary>
      ///   VisibleExtentChange event. Will be fired on
      ///   TGIS_Viewer.VisibleExtent change.
      /// </summary>
      FOnVisibleExtentChange :
         {$IFDEF CLR}
           EventHandler ;
         {$ELSE}
           TNotifyEvent ;
         {$ENDIF}

      /// <summary>
      ///   ZoomChange event. Will be fired on TGIS_Viewer.Zoom change.
      /// </summary>
      FOnZoomChange :
         {$IFDEF CLR}
           EventHandler ;
         {$ELSE}
           TNotifyEvent ;
         {$ENDIF}

      /// <summary>
      ///   Busy event. Will be fired regularly during long-drawn
      ///   operations.
      /// </summary>
      FOnBusy : TGIS_BusyEvent ;

      /// <summary>
      ///   Handler for Help event.
      /// </summary>
      FOnHelp : TGIS_HelpEvent ;

      {$IFDEF GIS_XDK}
        FUseOnHelp : Boolean ;
      {$ENDIF}

      /// <summary>
      ///   LayerAdd event. Will be fired upon adding layer to the viewer:
      ///   after creation but before opening.
      /// </summary>
      FOnLayerAdd : TGIS_LayerEvent ;

      /// <summary>
      ///   LayerDelete event. Will be fired upon deleting layer to the
      ///   viewer: just before destructing the layer.
      /// </summary>
      FOnLayerDelete : TGIS_LayerEvent ;

      /// <summary>
      ///   Will be fired upon opening layer to resolve any
      ///   username/password
      /// </summary>
      FOnPassword : TGIS_TemplateProducerEvent ;

      /// <summary>
      ///   On project open event. Will be fired on TGIS_Viewer.Open.
      /// </summary>
      FOnProjectOpen :
         {$IFDEF CLR}
           EventHandler ;
         {$ELSE}
           TNotifyEvent ;
         {$ENDIF}

      /// <summary>
      ///   On project close event. Will be fired on TGIS_Viewer.Close.
      /// </summary>
      FOnProjectClose :
       {$IFDEF CLR}
         EventHandler ;
       {$ELSE}
         TNotifyEvent ;
       {$ENDIF}

    private
    {$IFDEF OXYGENE} assembly or protected {$ENDIF}
       // IGIS_Viewer properties access functions
      function  fget_AutoStyle          : Boolean ;
      procedure fset_AutoStyle          ( const _value : Boolean ) ;
      function  fget_BigExtent          : TGIS_Extent ;
      function  fget_BigExtentMargin    : Integer ;
      procedure fset_BigExtentMargin    ( const _value : Integer     ) ;
      function  fget_KeepScale          : Boolean ;
      procedure fset_KeepScale          ( const _value  : Boolean     ) ;
      function  fget_BusyLevel          : Integer     ;
      function  fget_BusyText           : String      ;
      function  fget_Center             : TGIS_Point  ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_Center             ( const _value : TGIS_Point  ) ;
      function  fget_CenterPtg          : TGIS_Point  ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_CenterPtg          ( const _value : TGIS_Point  ) ;
      function  fget_Color              : TGIS_Color ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_Color              ( const _value : TGIS_Color  ) ;
      function  fget_Copyright          : String ;
      function  fget_CS                 : TGIS_CSCoordinateSystem ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_CS                 ( const _value : TGIS_CSCoordinateSystem ) ;
      function  fget_CustomPPI          : Integer ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_CustomPPI          ( const _value : Integer     ) ;
      function  fget_Editor             : IGIS_Editor ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_Editor             ( const _value : IGIS_Editor ) ;
      function  fget_Extent             : TGIS_Extent ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      function  fget_FileCopyrights     : String      ;
      function  fget_FontScale          : Integer ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_FontScale          ( const _value : Integer     ) ;
      function  fget_FullDrawExtent     : TGIS_Extent ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      function  fget_IncrementalPaint   : Boolean     ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_IncrementalPaint   ( const _value : Boolean     ) ;
      function  fget_TiledPaint         : Boolean     ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_TiledPaint         ( const _value : Boolean     ) ;
      function  fget_InPaint            : Boolean     ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      function  fget_IsBusy             : Boolean     ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      function  fget_IsEmpty            : Boolean     ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      function  fget_IsLocked           : Boolean     ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      function  fget_IsTopmost          : Boolean     ;
      function  fget_Items              : TGIS_LayerAbstractList ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      function  fget_LabelsReg          : TGIS_LabelsAreaAbstract ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      function  fget_Level              : Double ;
      procedure fset_Level              ( const _value : Double      ) ;
      function  fget_MultiUserMode      : TGIS_MultiUser ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_MultiUserMode      ( const _value : TGIS_MultiUser ) ;
      function  fget_CustomData         : TGIS_StringList ;
      function  fget_OverlappedExtentMargin
                                        : Integer     ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_OverlappedExtentMargin
                                        ( const _value : Integer     ) ;
      function  fget_PPI                : Integer ;
//                                        {$IFNDEF GIS_NOINLINE1} inline; {$ENDIF}
      function  fget_ProjectFile        : TGIS_ConfigAbstract ;
      procedure fset_ProjectFile        ( const _value : TGIS_ConfigAbstract
                                        ) ;
      function  fget_ProjectName        : String      ;
      function  fget_DelayedUpdate      : Integer     ;
      procedure fset_DelayedUpdate      ( const _value : Integer     ) ;
      function  fget_ProgressiveUpdate  : Integer     ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_ProgressiveUpdate  ( const _value : Integer     ) ;
      function  fget_RestrictedDrag     : Boolean     ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_RestrictedDrag     ( const _value : Boolean     ) ;
      function  fget_RestrictedExtent   : TGIS_Extent ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_RestrictedExtent   ( const _value : TGIS_Extent ) ;
      function  fget_RotationAngle      : Double ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_RotationAngle      ( const _value : Double      ) ;
      function  fget_RotationPoint      : TGIS_Point  ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_RotationPoint      ( const _value : TGIS_Point  ) ;
      function  fget_ScaleAsFloat       : Double ;
      procedure fset_ScaleAsFloat       ( const _value : Double      ) ;
      function  fget_ScaleAsText        : String ;
      procedure fset_ScaleAsText        ( const _value : String      ) ;
      function  fget_SelectionGisColor  : TGIS_Color ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_SelectionGisColor  ( const _value : TGIS_Color  ) ;
      function  fget_SelectionOutlineOnly : Boolean ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_SelectionOutlineOnly(
                                          const _value : Boolean     ) ;
      function  fget_SelectionTransparency : Integer ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_SelectionTransparency(
                                          const _value : Integer     ) ;
      function  fget_SelectionWidth     : Integer ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_SelectionWidth     ( const _value : Integer     ) ;
      function  fget_SystemPPI          : Integer ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      function  fget_ViewerParent       : IGIS_ViewerParent ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      function  fget_ViewerParentRoot   : IGIS_ViewerParent ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      function  fget_Viewport           : TGIS_Point ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_Viewport           ( const _value : TGIS_Point  ) ;
      function  fget_VisibleExtent      : TGIS_Extent ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_VisibleExtent      ( const _value : TGIS_Extent ) ;
      function  fget_UseAnimations      : Boolean ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_UseAnimations      ( const _value : Boolean     ) ;
      function  fget_UseRTree           : Boolean ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_UseRTree           ( const _value : Boolean     ) ;
      function  fget_Zoom               : Double ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_Zoom               ( const _value : Double      ) ;
      function  fget_ZoomEx             : Double ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      function  fget_MasterViewer       : IGIS_Viewer ;
      procedure fset_MasterViewer       ( const _value : IGIS_Viewer ) ;

      procedure fset_ZoomEx             ( const _value : Double      ) ;
      function  fget_UponDestroy        : Boolean     ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      function  fget_TemporaryScaleInternal: Double      ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_TemporaryScaleInternal(
                                          const _value : Double      ) ;
      function  fget_TemporaryVisibleExtent : TGIS_Extent ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_TemporaryVisibleExtent(
                                          const _value : TGIS_Extent  ) ;

    private // not in IGIS_Viewer due to class references

      function  fget_Hierarchy          : IGIS_HierarchyManager ;
                                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

    private
      // TGIS_Viewer access functions
      function  fget_VisibleExtentEx    : TGIS_Extent ; virtual;

    private
      {$IFDEF CLR}
        procedure fadd_EditorSnapPointEvent(
                                   const _value : TGIS_EditorSnapPointEvent
                                 ) ; virtual;
        procedure fremove_EditorSnapPointEvent(
                                   const _value : TGIS_EditorSnapPointEvent
                                 ) ; virtual;
        procedure fadd_EditorPointChangeEvent(
                                   const _value : TGIS_EditorPointChangeEvent
                                 ) ; virtual;
        procedure fremove_EditorPointChangeEvent(
                                   const _value : TGIS_EditorPointChangeEvent
                                 ) ; virtual;
        procedure fadd_EditorPointMoveEvent(
                                   const _value : TGIS_EditorPointMoveEvent
                                 ) ; virtual;
        procedure fremove_EditorPointMoveEvent(
                                   const _value : TGIS_EditorPointMoveEvent
                                 ) ; virtual;
      {$ELSE}
        function  fget_EditorSnapPointEvent : TGIS_EditorSnapPointEvent ;
        procedure fset_EditorSnapPointEvent(
                                   const _value : TGIS_EditorSnapPointEvent
                                 ) ; virtual;
        function  fget_EditorPointChangeEvent : TGIS_EditorPointChangeEvent ;
        procedure fset_EditorPointChangeEvent(
                                   const _value : TGIS_EditorPointChangeEvent
                                 ) ; virtual;
        function  fget_EditorPointMoveEvent : TGIS_EditorPointMoveEvent ;
        procedure fset_EditorPointMoveEvent(
                                   const _value : TGIS_EditorPointMoveEvent
                                 ) ; virtual;
      {$ENDIF}

    protected // other internal variables
    {$IFDEF OXYGENE} assembly or protected {$ENDIF}

      /// <summary>
      ///   notification subscribers list.
      /// </summary>
      lstSubscribers : TInterfaceList ;

      {$IFDEF OXYGENE}
        /// <summary>
        ///   State (zoom, visible extent etc) after last paint.
        /// </summary>
        lastPaint : T_lastPaint ;
      {$ELSE}
        /// <summary>
        ///   State (zoom, visible extent etc) after last paint.
        /// </summary>
        lastPaint : record

          /// <summary>
          ///    Zoom value after last draw or 0.
          /// </summary>
          Zoom : Double ;

          /// <summary>
          ///   VisibleExtent value after last draw or GisNoWorld.
          /// </summary>
          VisibleExtent : TGIS_Extent ;
        end ;
      {$ENDIF}

      /// <summary>
      ///   Color of screen to be restored after project changes.
      /// </summary>
      oldColor : TGIS_Color ;

      /// <summary>
      ///   Saved originalViewport value.
      /// </summary>
      originalViewport : TGIS_Point ;

      /// <summary>
      ///   Helper for rotations.
      /// </summary>
      rotateSin : Double ;

      /// <summary>
      ///   Helper for rotations.
      /// </summary>
      rotateCos : Double ;

      /// <summary>
      ///   Helper for rotations.
      /// </summary>
      unrotateSin : Double ;

      /// <summary>
      ///   Helper for rotations.
      /// </summary>
      unrotateCos : Double ;

      /// <summary>
      ///   Saved level called - to avoid save-upon-save case.
      /// </summary>
      iUponSave : Integer ;

      /// <summary>
      ///   Open function call. To avoid blocking layers adding due
      ///   to busy state.
      /// </summary>
      bUponOpen : Boolean ;

      /// <summary>
      ///   Project can be upgraded to a new version.
      ///   Set to false upon SaveProjectAs.
      /// </summary>
      bUpgradable : Boolean ;

      /// <summary>
      ///   Hourglass state variable.
      /// </summary>
      hourglassState       : Integer ;

      /// <summary>
      ///   Hourglass state variable.
      /// </summary>
      hourglassVisible     : Boolean ;

      /// <summary>
      ///   Hourglass state variable.
      /// </summary>
      hourglassInterval    : Int64 ;

      /// <summary>
      ///   Hourglass state variable.
      /// </summary>
      hourglassProgressive : Int64 ;

      /// <summary>
      ///   Hourglass state variable.
      /// </summary>
      hourglassInterrupted : Boolean ;

      /// <summary>
      ///   Interrupte ASAP
      /// </summary>
      asapInterrupted : Boolean ;

      {$IFDEF OXYGENE}
        /// <summary>
        ///   Old viewer properties state (to restore upon Close)
        /// </summary>
        oldProperties : T_oldProperties ;
      {$ELSE}
        /// <summary>
        ///   Old viewer properties state (to restore upon Close)
        /// </summary>
        oldProperties : record

          /// <summary>
          ///   True if properties was altered.
          /// </summary>
          Modified : Boolean ;

          /// <summary>
          ///   See property Color.
          /// </summary>
          Color  : TGIS_Color ;

          /// <summary>
          ///   See property IncrementalPaint.
          /// </summary>
          IncrementalPaint : Boolean ;
        end ;
      {$ENDIF}

      /// <summary>
      ///   True current paint mode is done for preparing 3D texture.
      /// </summary>
      bPaintTexture : Boolean ;

      /// <summary>
      ///   Hash of last opened project.
      /// </summary>
      iLastHash : Int64 ;

    private  // other internal methods
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Save project into file. TO be used by SaveProject* methods.
      /// </summary>
      /// <param name="_path">
      ///    Path to the project
      /// </param>
      /// <param name="_relativepath">
      ///   if True (default) then project will be saved using paths
      ///   relative to project file location
      /// </param>
      /// <param name="_clearstate">
      ///   If False, then all "mustsave" will remain intact.
      /// </param>
      /// <param name="_substitute">
      ///   If True, then replace a current ProjectFile with a newly saved
      ///   file.
      /// </param>
      procedure saveProjectFile  ( const _path         : String          ;
                                   const _relativepath : Boolean         ;
                                   const _clearstate   : Boolean         ;
                                   const _substitute   : Boolean
                                 ) ; overload;

      /// <summary>
      ///   Notify viewer, in a locked stated, about necessary updates upon
      ///   Unlock.
      /// </summary>
      /// <param name="_extent">
      ///    extent which should be updated
      /// </param>
      /// <param name="_wholemap">
      ///   True, is whole map should be updated; if False then only
      ///   topmost layer
      /// </param>
      procedure notifyLocked       ( const _extent   : TGIS_Extent ;
                                     const _wholemap : Boolean
                                   ) ;
                                   {$IFNDEF GENDOC} deprecated ; {$ENDIF}

      /// <summary>
      ///   Provide unique layer name based on provided name.
      /// </summary>
      /// <param name="_name">
      ///   template name
      /// </param>
      /// <returns>
      ///   layer name
      /// </returns>
      /// <remarks>
      ///   If the name exist, then number will be attached and the tail to
      ///   guarantee uniqueness
      /// </remarks>
      function  getUniqueLayerName ( const _name   : String
                                   ) : String ;

      /// <summary>
      ///   Sets a direct painting mode. It means that the next paint cannot
      ///   be a simple restoration from cache.
      /// </summary>
      procedure setDirectPaint     ;

      /// <summary>
      ///   Rotate 3D point relative to RotationPoint.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be rotated
      /// </param>
      procedure rotatePtg3D_ref    ( {$IFNDEF JAVA} var {$ENDIF}   _ptg    : TGIS_Point3D
                                   ) ;

      /// <summary>
      ///   Unrotate 3D point relative to RotationPoint.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be unrotated
      /// </param>
      procedure unrotatePtg3D_ref  ( {$IFNDEF JAVA} var {$ENDIF}   _ptg    : TGIS_Point3D
                                   ) ;

      /// <summary>
      ///   Rotate point relative to RotationPoint.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be rotated
      /// </param>
      function  rotatePtg          ( const _ptg    : TGIS_Point
                                   ) : TGIS_Point ;

      /// <summary>
      ///   Rotate 3D point relative to RotationPoint.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be rotated
      /// </param>
      function  rotatePtg3D        ( const _ptg    : TGIS_Point3D
                                   ) : TGIS_Point3D ;

      /// <summary>
      ///   Unrotate point relative to RotationPoint.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be unrotated
      /// </param>
      function  unrotatePtg        ( const _ptg    : TGIS_Point
                                   ) : TGIS_Point ;

      /// <summary>
      ///   Unrotate 3D point relative to RotationPoint.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be Unrotate
      /// </param>
      function  unrotatePtg3D      ( const _ptg    : TGIS_Point3D
                                   ) : TGIS_Point3D ;

      /// <summary>
      ///   Get big extent for a given zoom.
      /// </summary>
      /// <param name="_zoom">
      ///   zoom value for which the big extent will be calculated
      /// </param>
      function  getBigExtentEx     ( const _zoom   : Double
                                   ) : TGIS_Extent ;

      /// <summary>
      ///   Returns current busy state.
      /// </summary>
      /// <returns>
      ///   Greater then 0 if busy is pending.
      /// </returns>
      /// <remarks>
      ///   See BusyPrepare, BusyRelease.
      /// </remarks>
      function  getBusyState       : Integer ; virtual;

      /// <summary>
      ///   Calculate hash. Uses to verify if order of layers, params, etc.
      ///   have been modified.
      /// </summary>
      /// <param name="_persistent">
      ///   if True, only persistent layers will be included in calculations
      /// </param>
      /// <returns>
      ///   Hash value.
      /// </returns>
      function doGetHash         ( const _persistent : Boolean
                                 ) : Int64 ;
    protected // constructors

      /// <summary>
      ///   Called by Create method.
      /// </summary>
      /// <param name="_owner">
      ///   object owner
      /// </param>
      procedure   doCreate       ( const _owner : IGIS_ViewerParent ) ;

      {$IFNDEF MANAGED}
        procedure doDestroy      ; override ;
      {$ELSE}
        procedure doDestroy      ; virtual ;
      {$ENDIF}

    public // constructors
      /// <summary>
      ///   Standard constructor.
      /// </summary>
      /// <param name="_owner">
      ///   object owner
      /// </param>
      /// <remarks>
      ///   <para>
      ///     Internally uses doCreate method.
      ///   </para>
      /// </remarks>
      {#ownership:_owner:ownif_empty}
      constructor Create         (       _owner    : IGIS_ViewerParent
                                 ) ; overload;

    {$IFDEF CLR}
    protected
      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure Dispose          ;
    {$ENDIF}

    public // Non interface function

      /// <summary>
      ///   Allow PPI recalculation.
      /// </summary>
      procedure ResetPPI ;

    public // API functions

      /// <inheritdoc from="IGIS_Viewer"/>
      function ChangeHash : Int64 ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Subscribe        ( const _control : IGIS_Subscribe
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure UnSubscribe      ( const _control : IGIS_Subscribe
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure NotifySubscribers( const _event   : Integer ;
                                   const _context : TObject
                                 ) ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      /// <inheritdoc from="IGIS_Viewer"/>
      function NotifyPaintException(
                                   const _message   : String   ;
                                   const _exception : Exception
                                 ) : Boolean ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Lock             ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Unlock           ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Unlock           ( const _redraw : Boolean
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Interrupt        ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  Interrupted     : Boolean ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function HourglassActive : Boolean ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure HourglassPrepare ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure HourglassRelease ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  HourglassShake   : Boolean ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure HourglassRestart ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure BusyPrepare      (       _sender  : TObject ;
                                         _text    : String
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure BusyRelease      (       _sender  : TObject
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure BusyShake        (       _sender  : TObject ;
                                         _pos     : Int64 ;
                                         _end     : Int64 ;
                                   var   _abort   : Boolean
                                 ) ;

      {$IFDEF OXYGENE}
        /// <inheritdoc from="IGIS_Viewer"/>
        procedure RaiseBusyEvent     (       _sender  : TObject ;
                                             _e       : TGIS_BusyEventArgs
                                     ) ;
      {$ELSE}
        /// <inheritdoc from="IGIS_Viewer"/>
        procedure RaiseBusyEvent     (       _sender  : TObject ;
                                             _pos     : Int64 ;
                                             _end     : Int64 ;
                                       var   _abort   : Boolean
                                     )  ;
      {$ENDIF}

      {$IFDEF OXYGENE}
        /// <inheritdoc from="IGIS_Viewer"/>
        procedure RaiseHelpEvent     (       _sender  : TObject ;
                                             _e       : TGIS_HelpEventArgs
                                     ) ;
      {$ELSE}
        /// <inheritdoc from="IGIS_Viewer"/>
        procedure RaiseHelpEvent     (       _sender  : TObject ;
                                             _name    : String
                                     )  ;
      {$ENDIF}

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_Viewer"/>
      function AssignedBusyEvent : TGIS_BusyEvent ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_Viewer"/>
      function  AssignedHelpEvent    : TGIS_HelpEvent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  StorePaintState      : TObject ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure RestorePaintState    ( var _state    : TObject
                                     ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure BeginPaintInternal   ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure EndPaintInternal ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  SynchronizePaint     ( const _interrupt : Boolean
                                     ) : Boolean ;

      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ReParentLock         ;

      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ReParentUnlock       ;


      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      /// <inheritdoc from="IGIS_Viewer"/>
      function ReParent              ( const _parent : IGIS_ViewerParent
                                     ) : IGIS_ViewerParent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function AttachLayer           ( const _layer : TGIS_LayerAbstract
                                     ) : IGIS_Viewer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Open                 ( const _path    : String
                                     ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Open             ( const _path    : String ;
                                   const _strict  : Boolean
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure OpenEx           ( const _configFile : TGIS_ConfigAbstract ;
                                   const _path       : String
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure OpenEx           ( const _configFile : TGIS_ConfigAbstract ;
                                   const _path       : String              ;
                                   const _strict     : Boolean
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Close            ; virtual;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ReadConfig       ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure RereadConfig     ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure WriteConfig      ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Add              ( const _layer   : TGIS_LayerAbstract
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure AddHierarchy ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  Get              ( const _name    : String
                                 ) : TGIS_LayerAbstract ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Delete           ( const _name    : String
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Draw             ( const _renderer : TObject ;
                                   const _mode     : TGIS_DrawMode
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  GetGrid          ( const _extent  : TGIS_Extent  ;
                                   const _grid    : TGIS_GridArray
                                 ) : Boolean ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure RevertAll ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SaveProject      ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SaveProject      ( const _relativepath : Boolean
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SaveProjectAs    ( const _path         : String
                                 ) ; overload;


      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SaveProjectAs    ( const _path         : String          ;
                                   const _relativepath : Boolean
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SaveProjectAsEx  ( const _configFile   : TGIS_ConfigAbstract ;
                                   const _path         : String
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SaveProjectAsEx  ( const _configFile   : TGIS_ConfigAbstract ;
                                   const _path         : String              ;
                                   const _relativepath : Boolean
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SaveData         ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SaveAll          ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  MustSave         : Boolean ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure MarkModified     ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure RecalcExtent     ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Reposition       ; virtual;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateExtent ( const _extent : TGIS_Extent
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateExtent ( const _extent : TGIS_Extent ;
                                   const _deep   : Boolean
                                 ) ; overload;
                                 {$IFNDEF GENDOC} deprecated ; {$ENDIF}

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateWholeMap ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateTopmost  ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateBasemap  ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateSelection ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateEditor    ( const _final : Boolean
                                    ) ;
      /// <inheritdoc from="IGIS_Viewer"/>
      function  FullExtentZoom   : Double ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure FullExtent       ; virtual;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  Locate           ( const _ptg     : TGIS_Point ;
                                   const _prec    : Double
                                 ) : TGIS_ShapeAbstract ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  Locate           ( const _ptg     : TGIS_Point ;
                                   const _prec    : Double     ;
                                   const _visible : Boolean
                                 ) : TGIS_ShapeAbstract ; overload;


      /// <inheritdoc from="IGIS_Viewer"/>
      function  Locate           ( const _pt      : TPoint     ;
                                   const _prec    : Integer
                                 ) : TGIS_ShapeAbstract ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  LocateEx            ( const _ptg     : TGIS_Point ;
                                      const _prec    : Double    ;
                                      const _visible : Boolean
                                    ) : TGIS_ShapeAbstractList ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  MapToScreen      ( const _ptg : TGIS_Point
                                 ) : TPoint ;

      {$IFDEF CLR}
        {$IFNDEF MONO}
          {#gendoc:hide}
          { TODO -cReview : move to section of new introduced methods }
          /// <summary>
          ///   Converts point coordinates from map related to screen related.
          /// </summary>
          /// <param name="_ptg">
          ///   coordinate in map units
          /// </param>
          /// <returns>
          ///   map coordinates converted to screen coordinates
          /// </returns>
          function MapToScreenF( const _ptg : TGIS_Point
                               ) : TPointF ;
        {$ENDIF}
      {$ENDIF}

      /// <inheritdoc from="IGIS_Viewer"/>
      function  MapToScreen3D  ( const _ptg : TGIS_Point3D
                               ) : TPoint ;
      {$IFDEF CLR}
        {$IFNDEF MONO}
          {#gendoc:hide}
          { TODO -cReview : move to section of new introduced methods }
          /// <summary>
          ///   Converts 3D point coordinates from map related to screen
          ///   related.
          /// </summary>
          /// <param name="_ptg">
          ///   coordinate in map units
          /// </param>
          /// <returns>
          ///   map coordinates converted to screen coordinates
          /// </returns>
          function  MapToScreenF3D
                               ( const _ptg : TGIS_Point3D
                               ) : TPointF ;
        {$ENDIF}
      {$ENDIF}

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ScreenToMap    ( const _pt  : TPoint
                               ) : TGIS_Point ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ScreenToMap3D  ( const _pt  : TPoint
                               ) : TGIS_Point3D ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  MapToScreenEx    ( const _pt  : TGIS_Point
                                 ) : TGIS_Point ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ScreenToMapEx    ( const _pt  : TGIS_Point
                                 ) : TGIS_Point ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  MapToScreenRect( const _rct : TGIS_Extent
                               ) : TRect ;

      {$IFDEF CLR}
        {$IFNDEF MONO}
          {#gendoc:hide}
          { TODO -cReview : move to section of new introduced methods }
          /// <summary>
          ///   Converts rectangle coordinates from map related to screen
          ///   related.
          /// </summary>
          /// <param name="_rct">
          ///   coordinate in map units
          /// </param>
          /// <returns>
          ///   Map coordinates converted to screen coordinates.
          /// </returns>
          function  MapToScreenRectF
                               ( const _rct : TGIS_Extent
                               ) : TRectF ;
        {$ENDIF}
      {$ENDIF}

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ScreenToMapRect( const _rct : TRect
                               ) : TGIS_Extent ; overload;

      /// <summary>
      ///   Converts point coordinates from screen related to map related.
      /// </summary>
      /// <param name="_left">
      ///   rectangle coordinate in screen units
      /// </param>
      /// <param name="_top">
      ///   rectangle coordinate in screen units
      /// </param>
      /// <param name="_right">
      ///   rectangle coordinate in screen units
      /// </param>
      /// <param name="_bottom">
      ///   rectangle coordinate in screen units
      /// </param>
      /// <returns>
      ///   Screen coordinates converted to map coordinates.
      /// </returns>
      function  ScreenToMapRect( const _left, _top, _right, _bottom : Integer
                               ) : TGIS_Extent ; overload;


      /// <inheritdoc from="IGIS_Viewer"/>
      function  PixelsToTwips    ( const _size : Integer
                                 ) : Integer ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <inheritdoc from="IGIS_Viewer"/>
      function  TwipsToPixels    ( const _size : Integer
                                 ) : Integer ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <inheritdoc from="IGIS_Viewer"/>
      function  TwipsToPoints    ( const _size : Integer
                                 ) : Integer ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure MoveViewport     ( var   _dx, _dy : Integer
                                 ) ; virtual;
      {$IFDEF JAVA}
        /// <inheritdoc from="IGIS_Viewer"/>
        procedure MoveViewportEx   ( var   _dx, _dy : java.lang.Double     ) ;

        /// <inheritdoc from="IGIS_Viewer"/>
        procedure SetViewport      ( var   _x ,  _y : java.lang.Double     ) ;
      {$ELSE}
        /// <inheritdoc from="IGIS_Viewer"/>
        procedure MoveViewportEx   ( var   _dx, _dy : Double     ) ;

        /// <inheritdoc from="IGIS_Viewer"/>
      procedure SetViewport      ( var   _x ,  _y : Double     ) ;
      {$ENDIF}

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure CenterViewport   ( const _ptg     : TGIS_Point ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SetCSByWKT       ( const  _wkt    : String     ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SetCSByEPSG      ( const  _epsg   : Integer    ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SetCSByWKTFile   ( const  _path   : String     ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  RotatedPoint     ( const _ptg     : TGIS_Point
                                 ) : TGIS_Point;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  UnrotatedPoint   ( const _ptg     : TGIS_Point
                                 ) : TGIS_Point ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  RotatedPoint3D   ( const _ptg     : TGIS_Point3D
                                 ) : TGIS_Point3D ;
      /// <inheritdoc from="IGIS_Viewer"/>
      procedure RotatedPoint3D_ref(
                                   {$IFNDEF JAVA} var {$ENDIF}
                                         _ptg     : TGIS_Point3D
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  UnrotatedPoint3D ( const _ptg     : TGIS_Point3D
                                 ) : TGIS_Point3D ;
      /// <inheritdoc from="IGIS_Viewer"/>
      procedure UnrotatedPoint3D_ref(
                                    {$IFNDEF JAVA} var {$ENDIF}
                                         _ptg     : TGIS_Point3D
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  RotatedExtent    ( const _extent  : TGIS_Extent
                                 ) : TGIS_Extent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  UnrotatedExtent  ( const _extent  : TGIS_Extent
                                 ) : TGIS_Extent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function GetRenderContext  : TObject ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure WaitForBackgroundProcesses ;

      {#gendoc:hide:GENPDK} { TODO -cPDK : Verify }
      /// <inheritdoc from="IGIS_Viewer"/>
      procedure WaitForNotBusy   (        _sender  : TObject ;
                                    const _proc    : TGIS_WaitForNotBusyProc
                                 ) ;    public

      /// <summary>
      ///   Parent viewer.
      /// </summary>
      property Parent : IGIS_ViewerParent
                        read  oParent ;

    public // IGIS_Viewer public properties

      /// <inheritdoc from="IGIS_Viewer"/>
      property Copyright : String        read  fget_Copyright ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property BigExtent : TGIS_Extent read fget_BigExtent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property BusyLevel : Integer read fget_BusyLevel ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property BusyText : String read fget_BusyText ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Center : TGIS_Point       read  fget_Center
                                         write fset_Center ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property CenterPtg : TGIS_Point    read  fget_CenterPtg
                                         write fset_CenterPtg ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property CS : TGIS_CSCoordinateSystem read fget_CS write fset_CS ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Editor : IGIS_Editor read  fget_Editor
                                    write fset_Editor ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Extent : TGIS_Extent read fget_Extent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property FileCopyrights : String read fget_FileCopyrights ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property FullDrawExtent : TGIS_Extent read fget_FullDrawExtent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property InPaint : Boolean read fget_InPaint ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property IsBusy : Boolean read fget_IsBusy ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property IsEmpty : Boolean read fget_IsEmpty ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property IsLocked : Boolean read fget_IsLocked ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property IsTopmost : Boolean read fget_IsTopmost ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Items : TGIS_LayerAbstractList read fget_Items ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property LabelsReg : TGIS_LabelsAreaAbstract read fget_LabelsReg ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Level : Double read  fget_Level
                              write fset_Level ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property OverlappedExtentMargin : Integer
                                        read  fget_OverlappedExtentMargin
                                        write fset_OverlappedExtentMargin ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property PPI : Integer read fget_PPI ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ProjectFile : TGIS_ConfigAbstract read  fget_ProjectFile
                                                 write fset_ProjectFile ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ProjectName : String read fget_ProjectName ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property DelayedUpdate : Integer read  fget_DelayedUpdate
                                       write fset_DelayedUpdate ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ProgressiveUpdate : Integer read  fget_ProgressiveUpdate
                                           write fset_ProgressiveUpdate ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property RestrictedExtent : TGIS_Extent read  fget_RestrictedExtent
                                              write fset_RestrictedExtent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property RotationAngle : Double read  fget_RotationAngle
                                      write fset_RotationAngle ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property RotationPoint : TGIS_Point read  fget_RotationPoint
                                          write fset_RotationPoint ;

      {$IFNDEF CLR}
         /// <inheritdoc from="IGIS_Viewer"/>
         property Scale : Double        read  fget_ScaleAsFloat
                                        write fset_ScaleAsFloat ;
      {$ENDIF}

      /// <inheritdoc from="IGIS_Viewer"/>
      property ScaleAsFloat : Double   read  fget_ScaleAsFloat
                                       write fset_ScaleAsFloat ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ScaleAsText : String      read  fget_ScaleAsText
                                         write fset_ScaleAsText ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property SystemPPI : Integer read fget_SystemPPI ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ViewerParent : IGIS_ViewerParent
                                        read fget_ViewerParent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ViewerParentRoot : IGIS_ViewerParent
                                        read fget_ViewerParentRoot ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Viewport : TGIS_Point read  fget_Viewport
                                     write fset_Viewport ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property VisibleExtent : TGIS_Extent read  fget_VisibleExtent
                                           write fset_VisibleExtent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Zoom : Double             read  fget_Zoom
                                         write fset_Zoom ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ZoomEx : Double           read  fget_ZoomEx
                                         write fset_ZoomEx ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property MasterViewer : IGIS_Viewer
                                        read  fget_MasterViewer
                                        write fset_MasterViewer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property UponDestroy : Boolean    read  fget_UponDestroy ;

    published // IGIS_Viewer published properties
    {$IFDEF OXYGENE} public {$ENDIF}

      /// <inheritdoc from="IGIS_Viewer"/>
      property AutoStyle : Boolean read  fget_AutoStyle
                                   write fset_AutoStyle ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property BigExtentMargin : Integer read  fget_BigExtentMargin
                                         write fset_BigExtentMargin ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property KeepScale  : Boolean     read  fget_KeepScale
                                        write fset_KeepScale ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Color : TGIS_Color        read  fget_Color
                                         write fset_Color ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property CustomPPI : Integer read fget_CustomPPI write fset_CustomPPI ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property FontScale : Integer read fget_FontScale write fset_FontScale ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property IncrementalPaint : Boolean read  fget_IncrementalPaint
                                          write fset_IncrementalPaint ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property TiledPaint : Boolean       read  fget_TiledPaint
                                          write fset_TiledPaint ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property MultiUserMode : TGIS_MultiUser read  fget_MultiUserMode
                                              write fset_MultiUserMode
                                              {$IFDEF OXYGENE}
                                                ;
                                              {$ELSE}
                                                default TGIS_MultiUser.Default ;
                                              {$ENDIF}

      /// <inheritdoc from="IGIS_Viewer"/>
      property CustomData : TGIS_StringList   read  fget_CustomData ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property RestrictedDrag : Boolean  read  fget_RestrictedDrag
                                         write fset_RestrictedDrag ;


      /// <inheritdoc from="IGIS_Viewer"/>
      property SelectionGisColor :
         TGIS_Color read  fget_SelectionGisColor
                    write fset_SelectionGisColor ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property SelectionOutlineOnly : Boolean read  fget_SelectionOutlineOnly
                                              write fset_SelectionOutlineOnly ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property SelectionTransparency : Integer
                                          read  fget_SelectionTransparency
                                          write fset_SelectionTransparency
                                         {$IFNDEF OXYGENE} default 60 {$ENDIF} ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property SelectionWidth : Integer read  fget_SelectionWidth
                                        write fset_SelectionWidth
                                        {$IFNDEF OXYGENE} default 150 {$ENDIF} ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property UseAnimations : Boolean read fget_UseAnimations
                                       write fset_UseAnimations
                                       {$IFNDEF OXYGENE} default True {$ENDIF} ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property UseRTree : Boolean read fget_UseRTree
                                  write fset_UseRTree
                                  {$IFNDEF OXYGENE} default True {$ENDIF} ;

    public

      /// <inheritdoc from="IGIS_Viewer"/>
      property Hierarchy : IGIS_HierarchyManager read fget_Hierarchy ;

    public
      /// <inheritdoc from="IGIS_Viewer"/>
      property TemporaryScaleInternal : Double
                                        read  fget_TemporaryScaleInternal
                                        write fset_TemporaryScaleInternal ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property TemporaryVisibleExtent : TGIS_Extent
                                        read  fget_TemporaryVisibleExtent
                                        write fset_TemporaryVisibleExtent ;

    published //event

      /// <event/>
      /// <summary>
      ///   ExtentChanged event. Will be fired on TGIS_Viewer.Extent change.
      /// </summary>
      {$IFDEF CLR}
        event    ExtentChangeEvent : EventHandler
                                     delegate FOnExtentChange ;
      {$ELSE}
        property ExtentChangeEvent : TNotifyEvent
                                     read  FOnExtentChange
                                     write FOnExtentChange ;
      {$ENDIF}

      /// <event/>
      /// <summary>
      ///   ExtentChanged event. Will be fired on TGIS_Viewer.Extent change. }
      /// </summary>
      {$IFDEF CLR}
        event    VisibleExtentChangeEvent : EventHandler
                                            delegate FOnVisibleExtentChange ;
      {$ELSE}
        property VisibleExtentChangeEvent : TNotifyEvent
                                            read  FOnVisibleExtentChange
                                            write FOnVisibleExtentChange ;
      {$ENDIF}

      /// <event/>
      /// <summary>
      ///   ZoomChange event. Will be fired before OnBeforePaint if
      ///   Viewer.Zoom was changed.
      /// </summary>
      {$IFDEF CLR}
        event    ZoomChangeEvent : EventHandler
                                   delegate FOnZoomChange ;
      {$ELSE}
        property ZoomChangeEvent : TNotifyEvent
                                   read  FOnZoomChange
                                   write FOnZoomChange ;
      {$ENDIF}

      /// <event/>
      /// <summary>
      ///   PaintException event. Will be fired when Paint rises an
      ///   exception. If not assigned a regular exception will be raised.
      /// </summary>
      {$IFDEF CLR}
        event    PaintExceptionEvent : TGIS_PaintExceptionEvent
                                       delegate FOnPaintException ;
      {$ELSE}
        property PaintExceptionEvent : TGIS_PaintExceptionEvent
                                       read  FOnPaintException
                                       write FOnPaintException ;
      {$ENDIF}

      /// <event/>
      /// <summary>
      ///   Busy event handler. Will be fired regularly during
      ///   long-drawn operations. If end value will be zero, the
      ///   meaning is: long-drawn with unknown end time. Close
      ///   long-drawn operation by calling with parameters (-1,-1).
      /// </summary>
      {$IFDEF CLR}
        event    BusyEvent : TGIS_BusyEvent
                             delegate FOnBusy ;
      {$ELSE}
        property BusyEvent : TGIS_BusyEvent
                             read  FOnBusy
                             write FOnBusy ;
      {$ENDIF}

      /// <event/>
      /// <summary>
      ///   Help event. If attached, all dialog boxes will have a help button,
      ///   and clicking on this button will raise an event.
      /// </summary>
      {$IFDEF CLR}
        event    HelpEvent : TGIS_HelpEvent
                             delegate FOnHelp ;
      {$ELSE}
        property HelpEvent : TGIS_HelpEvent
                             read  FOnHelp
                             write FOnHelp ;
      {$ENDIF}

      {$IFDEF GIS_XDK}
        /// <summary>
        ///   True if Help Event should be used. There is no other way in ActiveX
        ///   to really know if viewer is attached.
        /// </summary>
        property UseHelp      : Boolean
                                read  FUseOnHelp
                                write FUseOnHelp ;
      {$ENDIF}

      /// <event/>
      /// <summary>
      ///   LayerAdd event. Will be fired upon adding layer to the
      ///   viewer: after creation but before opening.
      /// </summary>
      {$IFDEF CLR}
        event    LayerAddEvent : TGIS_LayerEvent
                                 delegate FOnLayerAdd ;
      {$ELSE}
        property LayerAddEvent : TGIS_LayerEvent
                                 read  FOnLayerAdd
                                 write FOnLayerAdd ;
      {$ENDIF}

      /// <event/>
      /// <summary>
      ///   LayerDelete event. Will be fired upon deleting layer to the
      ///   viewer: just before destructing the layer.
      /// </summary>
      {$IFDEF CLR}
        event    LayerDeleteEvent : TGIS_LayerEvent
                                    delegate FOnLayerDelete ;
      {$ELSE}
        property LayerDeleteEvent : TGIS_LayerEvent
                                    read  FOnLayerDelete
                                    write FOnLayerDelete ;
      {$ENDIF}

      /// <event/>
      /// <summary>
      ///   EditorSnapPoint event. Will be fired upon snapping a point
      ///   in the editor in custom snapping mode.
      /// </summary>
      {$IFDEF CLR}
        event    EditorSnapPointEvent : TGIS_EditorSnapPointEvent
                                        add    fadd_EditorSnapPointEvent
                                        remove fremove_EditorSnapPointEvent ;
      {$ELSE}
        property   EditorSnapPointEvent : TGIS_EditorSnapPointEvent
                                        read  fget_EditorSnapPointEvent
                                        write fset_EditorSnapPointEvent ;
      {$ENDIF}

      /// <event/>
      /// <summary>
      ///   EditorPointChangeEvent event. Will be fired upon changing a point
      ///   in the editor.
      /// </summary>
      {$IFDEF CLR}
        event    EditorPointChangeEvent : TGIS_EditorPointChangeEvent
                                        add    fadd_EditorPointChangeEvent
                                        remove fremove_EditorPointChangeEvent ;
      {$ELSE}
        property   EditorPointChangeEvent : TGIS_EditorPointChangeEvent
                                        read  fget_EditorPointChangeEvent
                                        write fset_EditorPointChangeEvent ;
      {$ENDIF}

      /// <event/>
      /// <summary>
      ///   EditorPointMoveEvent event. Will be fired upon editing a point
      ///   in the editor.
      /// </summary>
      {$IFDEF CLR}
        event    EditorPointMoveEvent : TGIS_EditorPointMoveEvent
                                        add    fadd_EditorPointMoveEvent
                                        remove fremove_EditorPointMoveEvent ;
      {$ELSE}
        property   EditorPointMoveEvent : TGIS_EditorPointMoveEvent
                                        read  fget_EditorPointMoveEvent
                                        write fset_EditorPointMoveEvent ;
      {$ENDIF}

      /// <event/>
      /// <summary>
      ///   Password event. Will be fired upon opening layer to resolve
      ///   any username/password as a key/value pair. Supported only on
      ///   selected layers.
      /// </summary>
      {$IFDEF CLR}
        event    PasswordEvent : TGIS_TemplateProducerEvent
                                 delegate FOnPassword ;
      {$ELSE}
        property PasswordEvent : TGIS_TemplateProducerEvent
                                 read  FOnPassword
                                 write FOnPassword ;
      {$ENDIF}

      /// <event/>
      /// <summary>
      ///   ProjectOpen event. Will be fired on TGIS_Viewer.Open.
      /// </summary>
      {$IFDEF CLR}
        event    ProjectOpenEvent : EventHandler
                                    delegate FOnProjectOpen ;
      {$ELSE}
        property ProjectOpenEvent : TNotifyEvent
                                    read  FOnProjectOpen
                                    write FOnProjectOpen ;
      {$ENDIF}

      /// <event/>
      /// <summary>
      ///   ProjectClose event. Will be fired on TGIS_Viewer.Close.
      /// </summary>
      {$IFDEF CLR}
        event    ProjectCloseEvent : EventHandler
                                     delegate FOnProjectClose ;
      {$ELSE}
        property ProjectCloseEvent : TNotifyEvent
                                     read  FOnProjectClose
                                     write FOnProjectClose ;
      {$ENDIF}
  end ;

  /// <summary>
  ///   Encapsulation of the viewer w/o any kind of visual implementation.
  ///   This viewer can be used to manipulate project files, etc.
  /// </summary>
  TGIS_ViewerNonVisual = {$IFDEF OXYGENE} public {$ENDIF} class(
                           {$IFDEF DCC}
                             TComponent,
                           {$ELSE}
                             TGIS_BaseObjectDisposable,
                           {$ENDIF}
                           IGIS_Viewer,
                           IGIS_ViewerParent
                         )
    private
      oVwr : TGIS_Viewer;

    protected // IGIS_Viewer property access routines
      function  fget_AutoStyle          : Boolean;
      procedure fset_AutoStyle          ( const _value : Boolean ) ;
      function  fget_BigExtent          : TGIS_Extent ;
      function  fget_BigExtentMargin    : Integer    ;
      procedure fset_BigExtentMargin    ( const _value : Integer     ) ;
      function  fget_KeepScale          : Boolean ;
      procedure fset_KeepScale          ( const _value  : Boolean     ) ;
      function  fget_BusyLevel          : Integer    ;
      function  fget_BusyText           : String     ;
      function  fget_Center             : TGIS_Point ;
      procedure fset_Center             ( const _value : TGIS_Point  ) ;
      function  fget_CenterPtg          : TGIS_Point ;
      procedure fset_CenterPtg          ( const _value : TGIS_Point  ) ;
      function  fget_Color              : TGIS_Color ;
      procedure fset_Color              ( const _value : TGIS_Color  ) ;
      function  fget_Copyright          : String     ;
      function  fget_CS                 : TGIS_CSCoordinateSystem ;
      procedure fset_CS                 ( const _value : TGIS_CSCoordinateSystem ) ;
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
      function  fget_CustomData         : TGIS_StringList ;
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
      procedure fset_SelectionOutlineOnly( const _value : Boolean    ) ;
      function  fget_SelectionTransparency : Integer  ;
      procedure fset_SelectionTransparency( const _value : Integer   ) ;
      function  fget_SelectionWidth     : Integer     ;
      procedure fset_SelectionWidth     ( const _value : Integer     ) ;
      function  fget_SystemPPI          : Integer ;
      function  fget_ViewerParent       : IGIS_ViewerParent ;
      function  fget_ViewerParentRoot   : IGIS_ViewerParent ;
      function  fget_Viewport           : TGIS_Point  ;
      procedure fset_Viewport           ( const _value : TGIS_Point  ) ;
      function  fget_VisibleExtent      : TGIS_Extent ;
      procedure fset_VisibleExtent      ( const _value : TGIS_Extent ) ;
      function  fget_UseAnimations      : Boolean     ;
      procedure fset_UseAnimations      ( const _value : Boolean     ) ;
      function  fget_UseRTree           : Boolean     ;
      procedure fset_UseRTree           ( const _value : Boolean     ) ;
      function  fget_Zoom               : Double      ;
      procedure fset_Zoom               ( const _value : Double      ) ;
      function  fget_ZoomEx             : Double      ;
      procedure fset_ZoomEx             ( const _value : Double      ) ;
      function  fget_MasterViewer       : IGIS_Viewer ;
      procedure fset_MasterViewer       ( const _value : IGIS_Viewer ) ;
      function  fget_UponDestroy        : Boolean     ;
      function  fget_TemporaryScaleInternal
                                        : Double      ;
      procedure fset_TemporaryScaleInternal(
                                          const _value : Double      ) ;
      function  fget_TemporaryVisibleExtent
                                         : TGIS_Extent ;
      procedure fset_TemporaryVisibleExtent(
                                          const _value : TGIS_Extent ) ;

    private // IGIS_Viewer events' access routines
      function  fget_BusyEvent          : TGIS_BusyEvent ;
      procedure fset_BusyEvent          ( const _value : TGIS_BusyEvent ) ;
      {$IFDEF CLR}
        function  fget_ExtentChangeEvent: EventHandler ;
        procedure fset_ExtentChangeEvent( const _value : EventHandler   ) ;
      {$ELSE}
        function  fget_ExtentChangeEvent: TNotifyEvent ;
        procedure fset_ExtentChangeEvent( const _value : TNotifyEvent   ) ;
      {$ENDIF}
      {$IFDEF CLR}
        function  fget_VisibleExtentChangeEvent
                                        : EventHandler ;
        procedure fset_VisibleExtentChangeEvent
                                        ( const _value : EventHandler   ) ;
      {$ELSE}
        function  fget_VisibleExtentChangeEvent
                                        : TNotifyEvent ;
        procedure fset_VisibleExtentChangeEvent
                                        ( const _value : TNotifyEvent   ) ;
      {$ENDIF}
      {$IFDEF CLR}
        function  fget_ZoomChangeEvent  : EventHandler ;
        procedure fset_ZoomChangeEvent  ( const _value : EventHandler   ) ;
      {$ELSE}
        function  fget_ZoomChangeEvent  : TNotifyEvent ;
        procedure fset_ZoomChangeEvent  ( const _value : TNotifyEvent   ) ;
      {$ENDIF}

    public
      /// <summary>
      ///   Standard constructor. Creates bitmap 320x240.
      /// </summary>
      constructor Create         ; {$IFDEF DCC} reintroduce ; overload ; {$ENDIF}

      {$IFDEF DCC}
        /// <inheritdoc/>
        destructor Destroy       ; override ;
      {$ENDIF}
    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure    doDestroy     ; {$IFNDEF DCC} override; {$ENDIF}

    public // IGIS_Viewer public methods

      /// <inheritdoc from="IGIS_Viewer"/>
      function ChangeHash        : Int64 ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Subscribe        ( const _control : IGIS_Subscribe
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure UnSubscribe      ( const _control : IGIS_Subscribe
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure NotifySubscribers( const _event   : Integer ;
                                   const _context : TObject
                                 ) ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK} { TODO -cPDK : Verify }
      /// <inheritdoc from="IGIS_Viewer"/>
      function NotifyPaintException(
                                   const _message   : String   ;
                                   const _exception : Exception
                                 ) : Boolean ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Lock             ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Unlock           ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Unlock           ( const _redraw  : Boolean
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Interrupt        ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  Interrupted      : Boolean ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  HourglassActive  : Boolean ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure HourglassPrepare ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure HourglassRelease ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  HourglassShake   : Boolean ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure HourglassRestart ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure BusyPrepare      (       _sender  : TObject ;
                                         _text    : String
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure BusyRelease      (       _sender  : TObject
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure BusyShake        (       _sender  : TObject ;
                                         _pos     : Int64 ;
                                         _end     : Int64 ;
                                   var   _abort   : Boolean
                                 ) ;

      {$IFDEF OXYGENE}
        /// <inheritdoc from="IGIS_Viewer"/>
        procedure RaiseBusyEvent     (       _sender  : TObject ;
                                             _e       : TGIS_BusyEventArgs
                                     ) ;
      {$ELSE}
        /// <inheritdoc from="IGIS_Viewer"/>
        procedure RaiseBusyEvent     (       _sender  : TObject ;
                                             _pos     : Int64 ;
                                             _end     : Int64 ;
                                       var   _abort   : Boolean
                                     )  ;
      {$ENDIF}

      {$IFDEF OXYGENE}
        /// <inheritdoc from="IGIS_Viewer"/>
        procedure RaiseHelpEvent     (       _sender  : TObject ;
                                             _e       : TGIS_HelpEventArgs
                                     ) ;
      {$ELSE}
        /// <inheritdoc from="IGIS_Viewer"/>
        procedure RaiseHelpEvent     (       _sender  : TObject ;
                                             _name    : String
                                     )  ;
      {$ENDIF}

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_Viewer"/>
      function  AssignedBusyEvent: TGIS_BusyEvent;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_Viewer"/>
      function  AssignedHelpEvent: TGIS_HelpEvent;

      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_Viewer"/>
      function  StorePaintState  : TObject ;

      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_Viewer"/>
      procedure RestorePaintState( var _state     : TObject
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      procedure BeginPaintInternal ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      procedure EndPaintInternal ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      function  SynchronizePaint ( const _interrupt : Boolean
                                 ) : Boolean ;

      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ReParentLock         ;

      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ReParentUnlock       ;

      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      /// <inheritdoc from="IGIS_Viewer"/>
      function ReParent          ( const _parent     : IGIS_ViewerParent
                                 ) : IGIS_ViewerParent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function AttachLayer       ( const _layer : TGIS_LayerAbstract
                                 ) : IGIS_Viewer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Open             ( const _path       : String
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Open             ( const _path       : String ;
                                   const _strict     : Boolean
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure OpenEx           ( const _configFile : TGIS_ConfigAbstract ;
                                   const _path       : String
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure OpenEx           ( const _configFile : TGIS_ConfigAbstract ;
                                   const _path       : String              ;
                                   const _strict     : Boolean
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Close            ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ReadConfig       ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure RereadConfig     ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure WriteConfig      ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Add              ( const _layer      : TGIS_LayerAbstract
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  Get              ( const _name       : String
                                 ) : TGIS_LayerAbstract ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Delete           ( const _name       : String
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure AddHierarchy     ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      procedure Draw             ( const _renderer   : TObject     ;
                                   const _mode       : TGIS_DrawMode
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  GetGrid          ( const _extent     : TGIS_Extent  ;
                                   const _grid       : TGIS_GridArray
                                 ) : Boolean ;


      /// <inheritdoc from="IGIS_Viewer"/>
      procedure RevertAll        ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SaveProject      ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SaveProject      ( const _relativepath : Boolean
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SaveProjectAs    ( const _path         : String
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SaveProjectAs    ( const _path         : String              ;
                                   const _relativepath : Boolean
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SaveProjectAsEx  ( const _configFile   : TGIS_ConfigAbstract ;
                                   const _path         : String
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SaveProjectAsEx  ( const _configFile   : TGIS_ConfigAbstract ;
                                   const _path         : String              ;
                                   const _relativepath : Boolean
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SaveData         ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SaveAll          ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  MustSave         : Boolean ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure MarkModified     ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      procedure RecalcExtent     ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      procedure Reposition       ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      procedure InvalidateExtent ( const _extent  : TGIS_Extent
                                 ) ; overload;
                                 {$IFNDEF GENDOC} deprecated ; {$ENDIF}

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      procedure InvalidateExtent ( const _extent : TGIS_Extent ;
                                   const _deep   : Boolean
                                 ) ; overload;
                                 {$IFNDEF GENDOC} deprecated ; {$ENDIF}


      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      procedure InvalidateWholeMap ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      procedure InvalidateTopmost ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      procedure InvalidateBasemap ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateSelection ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      procedure InvalidateEditor ( const _final   : Boolean
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      function  FullExtentZoom   : Double ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      procedure FullExtent       ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      function  Locate           ( const _ptg     : TGIS_Point ;
                                   const _prec    : Double
                                 ) : TGIS_ShapeAbstract ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  Locate           ( const _ptg     : TGIS_Point ;
                                   const _prec    : Double     ;
                                   const _visible : Boolean
                                 ) : TGIS_ShapeAbstract ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  Locate           ( const _pt      : TPoint     ;
                                   const _prec    : Integer
                                 ) : TGIS_ShapeAbstract ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  LocateEx            ( const _ptg     : TGIS_Point ;
                                      const _prec    : Double    ;
                                      const _visible : Boolean
                                    ) : TGIS_ShapeAbstractList ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      function  MapToScreen      ( const _ptg     : TGIS_Point
                                 ) : TPoint ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      function  MapToScreen3D    ( const _ptg     : TGIS_Point3D
                                 ) : TPoint ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      function  ScreenToMap      ( const _pt      : TPoint
                                 ) : TGIS_Point ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      function  ScreenToMap3D    ( const _pt      : TPoint
                                 ) : TGIS_Point3D ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      function  MapToScreenEx    ( const _pt      : TGIS_Point
                                 ) : TGIS_Point ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      function  ScreenToMapEx    ( const _pt      : TGIS_Point
                                 ) : TGIS_Point ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      function  MapToScreenRect  ( const _rct     : TGIS_Extent
                                 ) : TRect ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      function  ScreenToMapRect  ( const _rct     : TRect
                                 ) : TGIS_Extent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      function  PixelsToTwips    ( const _size    : Integer
                                 ) : Integer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      function  TwipsToPixels    ( const _size    : Integer
                                 ) : Integer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      function  TwipsToPoints    ( const _size : Integer
                                 ) : Integer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      procedure MoveViewport     ( var   _dx, _dy : Integer
                                 ) ;

      {$IFDEF JAVA}
        /// <inheritdoc from="IGIS_Viewer"/>
        procedure MoveViewportEx   ( var   _dx, _dy : java.lang.Double     ) ;

        /// <inheritdoc from="IGIS_Viewer"/>
        procedure SetViewport      ( var   _x ,  _y : java.lang.Double     ) ;
      {$ELSE}
        /// <inheritdoc from="IGIS_Viewer"/>
        procedure MoveViewportEx   ( var   _dx, _dy : Double     ) ;

        /// <inheritdoc from="IGIS_Viewer"/>
        procedure SetViewport      ( var   _x ,  _y : Double     ) ;
      {$ENDIF}

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      procedure CenterViewport   ( const _ptg     : TGIS_Point
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SetCSByWKT       ( const _wkt     : String
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SetCSByEPSG      ( const _epsg    : Integer
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SetCSByWKTFile   ( const _path    : String
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      function  RotatedPoint     ( const _ptg     : TGIS_Point
                                 ) : TGIS_Point;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      function  UnrotatedPoint   ( const _ptg     : TGIS_Point
                                 ) : TGIS_Point ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      function  RotatedPoint3D   ( const _ptg     : TGIS_Point3D
                                 ) : TGIS_Point3D ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      procedure RotatedPoint3D_ref(
                                   {$IFNDEF JAVA} var {$ENDIF}   _ptg     : TGIS_Point3D
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      function  UnrotatedPoint3D ( const _ptg     : TGIS_Point3D
                                 ) : TGIS_Point3D ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      procedure UnrotatedPoint3D_ref(
                                    {$IFNDEF JAVA} var {$ENDIF}  _ptg     : TGIS_Point3D
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      function  RotatedExtent    ( const _extent  : TGIS_Extent
                                 ) : TGIS_Extent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      function  UnrotatedExtent  ( const _extent  : TGIS_Extent
                                 ) : TGIS_Extent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      function  GetRenderContext : TObject ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure WaitForBackgroundProcesses ;

      {#gendoc:hide:GENPDK} { TODO -cPDK : Verify }
      /// <inheritdoc from="IGIS_Viewer"/>
      procedure WaitForNotBusy         (       _sender  : TObject ;
                                         const _proc    : TGIS_WaitForNotBusyProc
                                       ) ;

    public // IGIS_ViewerParent public methods

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ControlClose           ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ControlDrawTexture     (       _bmp     : TObject     ;
                                         const _extent  : TGIS_Extent ;
                                         const _ppi     : Integer
                                       ) ; overload ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ControlDrawTexture     (       _bmp     : TObject     ;
                                         const _layer   : TGIS_LayerAbstract ;
                                         const _extent  : TGIS_Extent ;
                                         const _ppi     : Integer
                                       ) ; overload ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ControlRenderer        : TObject ;


      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ControlFlash           ( const _times   : Integer ;
                                         const _delay   : Integer
                                       ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ControlSystemPPI       : Integer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ControlPPI             : Integer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ControlCanvasScale     : Single ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ControlCanvasHeight    : Integer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ControlCanvasWidth     : Integer ;


      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ControlRepaint         ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ControlProcessMessages ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function ControlUpdateSynchronize( const _interrupt : Boolean
                                       ) : Boolean ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ControlUpdateWholeMap  ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlUpdateProgressive  ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ControlUpdateTopmost   ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ControlUpdateBasemap   ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ControlUpdateSelection ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ControlUpdateEditor    ( const _final : Boolean
                                       ) ;


      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ControlHourglassShow   ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ControlHourglassHide   ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ControlHourglassShake  : Boolean ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ControlSet3DMode       ( const _mode    : TGIS_Viewer3DMode
                                       ) ;


      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ControlRaiseEditorChangeEvent(
                                               _sender  : TObject
                                       ) ;
      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlAutoCenterViewport      ( const _dx, _dy : Double ) ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlExtentChanged   ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  SetViewer ( const _viewer : TObject ) : TObject ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  GetViewer : TObject ;

    private // IGIS_Viewer public properties forced to be hidden

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property KeepScale  : Boolean        read  fget_KeepScale
                                           write fset_KeepScale ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   Always 0 on TGIS_ViewerBmp.
      /// </remarks>
      property DelayedUpdate : Integer     read  fget_DelayedUpdate
                                           write fset_DelayedUpdate ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   Always 0 on TGIS_ViewerBmp.
      /// </remarks>
      property ProgressiveUpdate : Integer read  fget_ProgressiveUpdate
                                           write fset_ProgressiveUpdate ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   Always False on TGIS_ViewerBmp.
      /// </remarks>
      property UseAnimations : Boolean     read  fget_UseAnimations
                                           write fset_UseAnimations ;

    public // IGIS_Viewer public properties

      /// <inheritdoc from="IGIS_Viewer"/>
      property Copyright    : String       read  fget_Copyright ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property CustomData : TGIS_StringList read fget_CustomData ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property AutoStyle : Boolean         read  fget_AutoStyle
                                           write fset_AutoStyle ;

      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property BigExtent : TGIS_Extent     read  fget_BigExtent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property BigExtentMargin : Integer   read  fget_BigExtentMargin
                                           write fset_BigExtentMargin ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property BusyLevel : Integer         read  fget_BusyLevel ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property BusyText : String           read  fget_BusyText ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property Center : TGIS_Point         read  fget_Center
                                           write fset_Center ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property CenterPtg : TGIS_Point      read  fget_CenterPtg
                                           write fset_CenterPtg ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Color : TGIS_Color          read  fget_Color
                                           write fset_Color ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property CS : TGIS_CSCoordinateSystem
                                           read  fget_CS
                                           write fset_CS ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property CustomPPI : Integer         read  fget_CustomPPI
                                           write fset_CustomPPI ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Editor : IGIS_Editor        read  fget_Editor
                                           write fset_Editor ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Extent : TGIS_Extent        read  fget_Extent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property FileCopyrights : String     read  fget_FileCopyrights ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property FontScale : Integer         read  fget_FontScale
                                           write fset_FontScale ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property FullDrawExtent : TGIS_Extent
                                           read  fget_FullDrawExtent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Hierarchy : IGIS_HierarchyManager
                                               read  fget_Hierarchy      ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property IncrementalPaint : Boolean  read  fget_IncrementalPaint
                                           write fset_IncrementalPaint ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property TiledPaint : Boolean        read  fget_TiledPaint
                                           write fset_TiledPaint ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property InPaint : Boolean           read  fget_InPaint ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property IsBusy : Boolean            read  fget_IsBusy ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property IsEmpty : Boolean           read  fget_IsEmpty ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property IsLocked : Boolean          read  fget_IsLocked ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property IsTopmost : Boolean         read  fget_IsTopmost ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Items : TGIS_LayerAbstractList
                                           read  fget_Items ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property LabelsReg : TGIS_LabelsAreaAbstract
                                           read fget_LabelsReg ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property Level : Double              read  fget_Level
                                           write fset_Level ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property MultiUserMode : TGIS_MultiUser
                                           read  fget_MultiUserMode
                                           write fset_MultiUserMode ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property OverlappedExtentMargin : Integer
                                       read  fget_OverlappedExtentMargin
                                       write fset_OverlappedExtentMargin ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property PPI : Integer               read  fget_PPI ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ProjectFile : TGIS_ConfigAbstract
                                           read  fget_ProjectFile
                                           {$IFDEF OXYGENE}
                                             write fset_ProjectFile
                                           {$ENDIF}
                                           ;


      /// <inheritdoc from="IGIS_Viewer"/>
      property ProjectName : String        read  fget_ProjectName ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property RestrictedDrag : Boolean    read  fget_RestrictedDrag
                                           write fset_RestrictedDrag ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property RestrictedExtent : TGIS_Extent
                                           read  fget_RestrictedExtent
                                           write fset_RestrictedExtent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property RotationAngle : Double      read  fget_RotationAngle
                                           write fset_RotationAngle ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property RotationPoint : TGIS_Point  read  fget_RotationPoint
                                           write fset_RotationPoint ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property Scale : Double              read  fget_ScaleAsFloat
                                           write fset_ScaleAsFloat ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property ScaleAsFloat : Double       read  fget_ScaleAsFloat
                                           write fset_ScaleAsFloat ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property ScaleAsText : String        read  fget_ScaleAsText
                                           write fset_ScaleAsText ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property SelectionGisColor : TGIS_Color
                                           read  fget_SelectionGisColor
                                           write fset_SelectionGisColor ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property SelectionOutlineOnly : Boolean
                                           read  fget_SelectionOutlineOnly
                                           write fset_SelectionOutlineOnly ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property SelectionTransparency : Integer
                                           read  fget_SelectionTransparency
                                           write fset_SelectionTransparency ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property SelectionWidth : Integer    read  fget_SelectionWidth
                                           write fset_SelectionWidth ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property SystemPPI : Integer         read  fget_SystemPPI ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ViewerParent : IGIS_ViewerParent
                                           read  fget_ViewerParent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ViewerParentRoot : IGIS_ViewerParent
                                           read  fget_ViewerParentRoot ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property Viewport : TGIS_Point       read  fget_Viewport
                                           write fset_Viewport ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property VisibleExtent : TGIS_Extent read  fget_VisibleExtent
                                           write fset_VisibleExtent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property UseRTree : Boolean          read  fget_UseRTree
                                           write fset_UseRTree ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property Zoom : Double               read  fget_Zoom
                                           write fset_Zoom ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property ZoomEx : Double             read  fget_ZoomEx
                                           write fset_ZoomEx ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property MasterViewer : IGIS_Viewer
                                           read  fget_MasterViewer
                                           write fset_MasterViewer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property UponDestroy  : Boolean      read  fget_UponDestroy ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property TemporaryScaleInternal : Double read  fget_TemporaryScaleInternal
                                               write fset_TemporaryScaleInternal ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      property TemporaryVisibleExtent : TGIS_Extent
                                           read  fget_TemporaryVisibleExtent
                                           write fset_TemporaryVisibleExtent ;
    published // IGIS_Viewer events

      /// <event/>
      /// <summary>
      ///   Busy event. Will be fired regularly during long-drawn operations.
      ///   If end value will be zero, the meaning is: long-drawn with
      ///   unknown end time. Close long-drawn operation by calling with
      ///   parameters (-1,-1).
      /// </summary>
      property BusyEvent : TGIS_BusyEvent      read  fget_BusyEvent
                                               write fset_BusyEvent ;

      /// <event/>
      /// <summary>
      ///   ExtentChange event. Will be fired on TGIS_Viewer.Extent change.
      /// </summary>
      {$IFDEF CLR}
        property ExtentChangeEvent : EventHandler
                                               read  fget_ExtentChangeEvent
                                               write fset_ExtentChangeEvent ;
      {$ELSE}
        property ExtentChangeEvent : TNotifyEvent
                                               read  fget_ExtentChangeEvent
                                               write fset_ExtentChangeEvent ;
      {$ENDIF}

      /// <event/>
      /// <summary>
      ///   VisibleExtentChange event. Will be fired before BeforePaint if
      ///   TGIS_Viewer.VisibleExtent was changed. Will not be fired if
      ///   TGIS_Viewer.VisibleExtent was changed based on changed Zoom - in
      ///   such situation only ZoomChange will be fired.
      /// </summary>
      {$IFDEF CLR}
        property VisibleExtentChangeEvent : EventHandler
                                         read  fget_VisibleExtentChangeEvent
                                         write fset_VisibleExtentChangeEvent ;
      {$ELSE}
        property VisibleExtentChangeEvent : TNotifyEvent
                                         read  fget_VisibleExtentChangeEvent
                                         write fset_VisibleExtentChangeEvent ;
      {$ENDIF}

      /// <event/>
      /// <summary>
      ///   ZoomChange event. Will be fired before OnBeforePaint if
      ///   Viewer.Zoom was changed.
      /// </summary>
      {$IFDEF CLR}
        property ZoomChangeEvent : EventHandler  read  fget_ZoomChangeEvent
                                                 write fset_ZoomChangeEvent ;
      {$ELSE}
        property ZoomChangeEvent : TNotifyEvent  read  fget_ZoomChangeEvent
                                                 write fset_ZoomChangeEvent ;
      {$ENDIF}

  end ;


const
  {#gendoc:hide}
  METADATA_PAINTNAVIGATEFEEDBACKFULLCACHE
    = 'TGIS_ViewerWnd.Paint.NavigateFeedback.FullCache';
  {#gendoc:hide}
  METADATA_PAINTPROGRESSIVETRANSPARENCY
    = 'TGIS_ViewerWnd.Paint.Progressive.Transparency';
  {#gendoc:hide}
  METADATA_PAINTPROGRESSIVEFULLCACHE
    = 'TGIS_ViewerWnd.Paint.Progressive.FullCache';
  {#gendoc:hide}
  METADATA_PAINTLABELSONTOP
    = 'TGIS_ViewerWnd.Paint.LabelsOnTop';
  {#gendoc:hide}
  METADATA_PAINTTOPMOSTLABELSONTOP
    = 'TGIS_ViewerWnd.Paint.Topmost.LabelsOnTop';
  {#gendoc:hide}
  METADATA_MODEDRAGALLGESTURES
    = 'TGIS_ViewerWnd.Mode.Drag.AllGestures';
  {#gendoc:hide}
  METADATA_MODEZOOMALLGESTURES
    = 'TGIS_ViewerWnd.Mode.Zoom.AllGestures';
  {#gendoc:hide}
  METADATA_MODESELECTALLGESTURES
    = 'TGIS_ViewerWnd.Mode.Select.AllGestures';
  {#gendoc:hide}
  METADATA_MODEEDITALLGESTURES
    = 'TGIS_ViewerWnd.Mode.Edit.AllGestures';
  {#gendoc:hide}
  METADATA_VIEWER3DMODECAMERAPOSITIONALLGESTURES
    = 'TGIS_ViewerWnd.Viewer3D.Mode.CameraPosition.AllGestures';
  {#gendoc:hide}
  METADATA_VIEWER3DMODECAMERAXYZALLGESTURES
    = 'TGIS_ViewerWnd.Viewer3D.Mode.CameraXYZ.AllGestures';
  {#gendoc:hide}
  METADATA_VIEWER3DMODECAMERAXYALLGESTURES
    = 'TGIS_ViewerWnd.Viewer3D.Mode.CameraXY.AllGestures';
  {#gendoc:hide}
  METADATA_VIEWER3DMODECAMERAROTATIONALLGESTURES
    = 'TGIS_ViewerWnd.Viewer3D.Mode.CameraRotation.AllGestures';
  {#gendoc:hide}
  METADATA_VIEWER3DMODESUNPOSITIONALLGESTURES
    = 'TGIS_ViewerWnd.Viewer3D.Mode.SunPosition.AllGestures';
  {#gendoc:hide}
  METADATA_VIEWER3DMODEZOOMALLGESTURES
    = 'TGIS_ViewerWnd.Viewer3D.Mode.Zoom.AllGestures';
  {#gendoc:hide}
  METADATA_VIEWER3DMODESELECTALLGESTURES
    = 'TGIS_ViewerWnd.Viewer3D.Mode.Select.AllGestures';

  {#gendoc:hide}
  METADATA_PIXELEXPORTLABELSETTINGS
    = 'TGIS_PixelExportManager.Export.LabelSettings';
  {#gendoc:hide}
  METADATA_PIXELEXPORTDRAWLABELSETTINGS
    = 'TGIS_PixelExportManager.Export.Draw.LabelSettings';

  {#gendoc:hide}
  METADATA_UPGRADEPROJECT
    = 'TGIS_Viewer.UpgradeProject' ;

{$INCLUDE GisVersion.inc}


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisResource,
    GisFunctions,
    GisInternals,
    GisCsBase,
    GisCsFactory,
    GisRendererAbstract,
    GisLayerVector,
    GisLayerPixel,
    GisPrintManagerAbstract,
    GisRegistredLayers,
    GisImportProject,
    GisLayerProject,
    GisLayerCompound ;
{$ENDIF}

var
  { Thread synchronization for draw management. }
    othreadDraw : TGIS_ThreadClass ;

  function threadDrawViewer : TGIS_ThreadClass ;
  begin
    if not assigned( othreadDraw ) then
      othreadDraw := TGIS_ThreadClass.Create ;
    Result := othreadDraw ;
  end ;

const
  {$IFDEF CLR}
    ResourceBaseName = 'GisViewer';
  {$ENDIF}
  { Memory allocation unit for shape data (FGeometry pointer).
    Any reallocation can be only a multiplication of DATA_CHUNK_SIZE. }
    DATA_CHUNK_SIZE = 128 ;
  { Size of caching device for PDF. }
    PDF_TILE_SIZE = 12 * 300 ;
  { Minimum tile size. }
    MIN_TILE_SIZE = 1 * 300 ;

  { Hourglass wake up time (ms). }
    HOURGLASS_WAKEUP = 500 ;

  { Busy notification - full notification (events & hourglass). }
    BUSY_NORMAL      = 0 ;
  { Busy notification - no events; only hourglass. }
    BUSY_NOEVENTS    = 1 ;
  { Busy notification - no events; no hourglass. }
    BUSY_NOHOURGLASS = 2 ;

  { If set to 0 then layer will not be opened is it is not active }
    METADATA_OPENNONACTIVE = 'TGIS_Viewer.OpenNonActive' ;

  { constants for zoom level calculations }
    EQUATOR_LENGTH = 40075.0167 ;
    TILE_SIZE = 512 ;
    ONE_PX_IN_M =  EQUATOR_LENGTH * 1000 / TILE_SIZE ;
    INCH_TO_M = 0.0254 ;

  type
  { Internal representation for storePaintState and restorePaintState.
  }
  T_paintState = class
    public
      Center    : TGIS_Point   ;
      Extent    : TGIS_Extent  ;
      PPI       : Integer      ;
  end ;


{$REGION 'T_waitForThread'}
{$IFNDEF ISLAND}
type
  T_waitForThread = Class( TThread )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      oProc   : TGIS_WaitForNotBusyProc ;
      oVwr    : TGIS_Viewer ;
      oSender : TObject ;
    protected
      procedure Execute ; override ;
  end ;

  procedure T_waitForThread.Execute ;
  begin
    if oVwr.iWaitForNotBusy > 0 then
      exit ;
    inc( oVwr.iWaitForNotBusy ) ;
    try
      while oVwr.InPaint or oVwr.IsBusy do
        sleep( 10 ) ;

      oProc(oSender) ;
    finally
      dec( oVwr.iWaitForNotBusy ) ;
    end;
  end;
{$ENDIF}
{$ENDREGION 'T_waitForThread'}

{$REGION 'TGIS_Viewer'}
procedure TGIS_Viewer.doCreate( const _owner : IGIS_ViewerParent ) ;
begin
  oParent := _owner ;

  FCopyright := 'TatukGIS Developer Kernel. ' +
                GIS_VERSION + ' ' +
                GIS_COPYRIGHT ;

  {$IFNDEF OXYGENE}
    inherited ;
  {$ENDIF}

  lstSubscribers      := TInterfaceList.Create ;

  FInPaint            := 0     ;
  FBusyText           := TGIS_KeyValueList<String,Integer>.Create ;
  FHierarchy          := TGIS_HierarchyManager.Create( Self ) ;
  FMultiUserMode      := TGIS_MultiUser.Default ;
  FCustomData         := TGIS_StringList.Create ;
  FIncrementalPaint   := False ;
  FTiledPaint         := False ;
  FDelayedUpdate      := 700   ;
  FProgressiveUpdate  := 2500  ;
  FRestrictedDrag     := True  ;

  FLocked.Level       := 0     ;
  FLocked.Reparent    := 0     ;
  FLocked.Extent      := GisNoWorld ;
  FLocked.WholeMap    := False ;
  FLocked.Topmost     := False ;
  FLocked.Selection   := False ;
  FLocked.Editor      := False ;

  FItems              := TGIS_LayerAbstractList.Create( True ) ; // create layers list
  FLabelsReg          := TGIS_LabelsArea.Create( self ) ;

  FEditor             := TGIS_Editor.Create( self ) ;
  FExtent             := GisNoWorld ;
  FRestrictedExtent   := GisWholeWorld ;
  {$IFDEF GIS_NORECORDS}
    FViewport          := new TGIS_Point ;
    FCenter            := new TGIS_Point ;
    FRotationPoint     := new TGIS_Point ;
  {$ENDIF}

  FCS                 := CSUnknownCoordinateSystem ;
  FCustomPPI          := 0 ;
  FCurrentPPI         := 0 ;
  FFontScale          := 100 ;
  FZoom               := GIS_INITIAL_ZOOM ;
  FUseAnimations      := True ;
  FUseRTree           := True ;

  FAutoStyle          := False ;
  FColor              := TGIS_Color.White ;
  oldColor            := TGIS_Color.None ;

  RotationAngle       := 0 ;

  FBigExtentMargin    := -10 ;
  FOverlappedExtentMargin := 0 ;
  FTemporaryVisibleExtent := GisNoWorld ;

  FKeepScale          := False ;

  iUponSave           := 0     ;
  bUponOpen           := False ;
  FIsModified         := False ;
  FUponDestroy        := False ;
  iLastHash           := 0 ;

  bUpgradable         := True ;

  hourglassState        := 0     ;
  hourglassVisible      := False ;
  hourglassInterval     := 0     ;
  hourglassProgressive  := 0     ;
  hourglassInterrupted  := False ;

  asapInterrupted        := False ;

  FSelectionWidth          := 100         ;
  FSelectionOutlineOnly    := False       ;
  FSelectionTransparency   := 60          ;
  FSelectionGisColor       := TGIS_Color.Red ;

  lastPaint.Zoom          := 0          ;
  lastPaint.VisibleExtent := GisNoWorld ;

  bBlockRepositionEvents  := False ;

  iWaitForNotBusy := 0 ;
  FTemporaryDirToDelete := '' ;
end ;

constructor TGIS_Viewer.Create( _owner : IGIS_ViewerParent ) ;
begin
  inherited Create ;

  FSelfRef := TGIS_ViewerRef.Create ;
  FSelfRef.Ref := self ;

  doCreate( _owner ) ;
end ;

{$IFDEF CLR}
  procedure TGIS_Viewer.Dispose ;
  begin
    doDestroy ;
  end ;
{$ENDIF}

procedure TGIS_Viewer.doDestroy ;
begin
  if not FUponDestroy then begin
    FUponDestroy := True ;

    if assigned( lstSubscribers ) then begin
      NotifySubscribers( GIS_SUBSCRIBED_DESTROY, nil ) ;
      FreeObject( lstSubscribers ) ;
    end ;

    FOnExtentChange        := nil ;
    FOnVisibleExtentChange := nil ;
    FOnZoomChange          := nil ;
    FOnVisibleExtentChange := nil ;
    FOnPaintException      := nil ;
    FOnBusy                := nil ;
    FOnLayerAdd            := nil ;
    FOnLayerDelete         := nil ;
    FOnPassword            := nil ;
    FOnProjectOpen         := nil ;
    FOnProjectClose        := nil ;

    Close ;

    FreeObject( FBusyText ) ;
    FHierarchy := nil ;

    FreeObject( FCustomData  ) ;
    FreeObject( FEditor      ) ;
    FreeObject( FItems       ) ;
    FreeObject( FLabelsReg   ) ;
    FreeObject( FProjectFile ) ;
    FreeObject( FSelfRef     ) ;
  end;
  {$IFNDEF MANAGED}
    inherited ;
  {$ENDIF}
end ;

function TGIS_Viewer.fget_AutoStyle
  : Boolean ;
begin
  Result := FAutoStyle ;
end ;

procedure TGIS_Viewer.fset_AutoStyle(
  const _value : Boolean
) ;
begin
  FAutoStyle := _value;
end ;

function TGIS_Viewer.fget_BigExtent
  : TGIS_Extent ;
begin
  Result := getBigExtentEx( FZoom ) ;
end ;

function TGIS_Viewer.fget_BigExtentMargin
  : Integer ;
begin
  Result := FBigExtentMargin ;
end ;

procedure TGIS_Viewer.fset_BigExtentMargin(
  const _value : Integer
) ;
begin
  if      _value < -50 then FBigExtentMargin := -50
  else if _value >  50 then FBigExtentMargin :=  50
  else                      FBigExtentMargin := _value ;
end ;

function TGIS_Viewer.fget_KeepScale
  : Boolean ;
begin
  Result := FKeepScale ;
end ;

procedure TGIS_Viewer.fset_KeepScale(
  const _value : Boolean
) ;
begin
  FKeepScale := _value ;
end ;

function TGIS_Viewer.fget_BusyLevel
  : Integer ;
begin
  Result := FBusyText.Count  ;
end ;

function TGIS_Viewer.fget_BusyText
  : String ;
begin
  if FBusyText.Count > 0 then begin
    Result := FBusyText[ FBusyText.Count - 1 ].Key ;
  end
  else
    Result := '' ;
end ;

function TGIS_Viewer.fget_Center
  : TGIS_Point ;
begin
  Result := FCenter ;
end ;

procedure TGIS_Viewer.fset_Center(
  const _value : TGIS_Point
) ;
begin
  FCenter := _TGIS_Point( _value ) ;
  Reposition ;
end ;

function TGIS_Viewer.fget_CenterPtg
  : TGIS_Point ;
begin
  Result := FCenter ;
end ;

procedure TGIS_Viewer.fset_CenterPtg(
  const _value : TGIS_Point
) ;
begin
  FCenter := _TGIS_Point( _value ) ;
end ;

function TGIS_Viewer.fget_Color
  : TGIS_Color ;
begin
  Result := FColor ;
end ;

procedure TGIS_Viewer.fset_Color(
  const _value : TGIS_Color
) ;
begin
  FColor := _value ;
end  ;

function TGIS_Viewer.fget_Copyright
  : String ;
begin
  Result := FCopyright ;
end ;

function TGIS_Viewer.fget_CS
  : TGIS_CSCoordinateSystem ;
begin
  Result := FCS ;
end ;

procedure TGIS_Viewer.fset_CS(
  const _value : TGIS_CSCoordinateSystem
) ;
var
  ext   : TGIS_Extent ;
  ext2  : TGIS_Extent ;
  oldcs : TGIS_CSCoordinateSystem ;
begin

  if not assigned( _value ) and ( CS is TGIS_CSUnknownCoordinateSystem ) then
    exit ;
  if assigned( _value ) and ( CS.EPSG = _value.EPSG ) then
    exit ;

  oldcs := CS ;

  ext := _TGIS_Extent(VisibleExtent) ;

  if assigned( _value ) then
    FCS := _value
  else
    FCS := CSUnknownCoordinateSystem ;

  RecalcExtent ;

  if ( ( oldcs.EPSG > 0 ) and ( CS.EPSG > 0 ) )
     and
     ( not GisIsSameExtent( ext, GisExtent( 0, 0, 0, 0 ) ) )
  then begin
    ext2 := CS.ExtentFromCS( oldcs, ext ) ;
    if not GisIsValidExtent( ext2 ) then begin
      FullExtent ;
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_PRJ_INCOMPATIBLE_CS ), '', 0 ) ;
    end ;
    VisibleExtent := ext2 ;
  end
  else
    FullExtent ;

  if oldcs.EPSG <> CS.EPSG then
    FIsModified := True ;
end ;

function TGIS_Viewer.fget_CustomPPI
  : Integer ;
begin
  Result := FCustomPPI ;
end ;

procedure TGIS_Viewer.fset_CustomPPI(
  const _value : Integer
) ;
begin
  FCustomPPI := Max( 0, _value ) ;
  FCurrentPPI := 0 ;
end ;

function TGIS_Viewer.fget_Editor
  : IGIS_Editor ;
begin
  Result := FEditor ;
end ;

procedure TGIS_Viewer.fset_Editor(
  const _value : IGIS_Editor
) ;
begin
  if _value is TGIS_Editor then
    FEditor := TGIS_Editor( _value ) ;
end ;

function TGIS_Viewer.fget_Extent
  : TGIS_Extent ;
begin
  Result := _TGIS_Extent(FExtent) ;
end ;

function TGIS_Viewer.fget_FileCopyrights
  : String ;
var
  i  : Integer ;
  ll : TGIS_Layer ;
begin
  Result := '' ;

  for i := 0 to Items.Count -1 do begin
    ll := TGIS_Layer( Items[i] ) ;

    if not ll.Active then
      continue ;

    if IsStringEmpty( ll.FileCopyrights ) then
      continue ;

    if not IsStringEmpty( Result ) then
      Result := Result + #13#10 ;

    Result := Result + ll.Caption + ': ' + ll.FileCopyrights ;
  end ;
end ;

function TGIS_Viewer.fget_FontScale
  : Integer ;
begin
  Result := FFontScale ;
end ;

procedure TGIS_Viewer.fset_FontScale(
  const _value : Integer
) ;
begin
  FFontScale := Max( 1, _value ) ;
end ;

function TGIS_Viewer.fget_FullDrawExtent
  : TGIS_Extent ;
begin
  Result := _TGIS_Extent(FFullDrawExtent) ;
end ;

function TGIS_Viewer.fget_IncrementalPaint
  : Boolean ;
begin
  Result := FIncrementalPaint ;
end ;

procedure TGIS_Viewer.fset_IncrementalPaint(
  const _value : Boolean
) ;
begin
  if _value then
    FTiledPaint := False ;
  FIncrementalPaint := _value
end ;

function TGIS_Viewer.fget_TiledPaint
  : Boolean ;
begin
  Result := FTiledPaint ;
end ;

procedure TGIS_Viewer.fset_TiledPaint(
  const _value : Boolean
) ;
begin
  if _value then
    FIncrementalPaint := False ;
  FTiledPaint := _value
end ;

function TGIS_Viewer.fget_InPaint
  : Boolean ;
begin
  Result := FInPaint > 0 ;
end ;

function TGIS_Viewer.fget_IsBusy
  : Boolean ;
begin
  Result := ( FBusyText.Count > 0 ) or ( HourglassActive );
end ;

function TGIS_Viewer.fget_IsEmpty
  : Boolean ;
begin
  Result := ( Items.Count <= 0 ) or GisIsNoWorld( Extent ) ;
end ;

function TGIS_Viewer.fget_IsLocked
  : Boolean ;
begin
  Result := FLocked.Level > 0  ;
end ;

function TGIS_Viewer.fget_IsTopmost
  : Boolean ;
begin
  if ( Items.Count <= 0 ) or
     ( TGIS_Layer( Items[Items.Count-1] ).CachedPaint ) then
    Result := False
  else
    Result := True ;
end ;

function TGIS_Viewer.fget_Items
  : TGIS_LayerAbstractList ;
begin
  Result := FItems ;
end ;

function TGIS_Viewer.fget_LabelsReg
  : TGIS_LabelsAreaAbstract ;
begin
  Result := FLabelsReg ;
end ;

function TGIS_Viewer.fget_MultiUserMode
  : TGIS_MultiUser ;
begin
  Result := FMultiUserMode ;
end ;

procedure TGIS_Viewer.fset_MultiUserMode(
  const _value : TGIS_MultiUser
) ;
begin
  FMultiUserMode := _value ;
end ;

function TGIS_Viewer.fget_CustomData
  : TGIS_StringList ;
begin
  Result := FCustomData ;
end ;


function TGIS_Viewer.fget_OverlappedExtentMargin
  : Integer ;
begin
  Result := FOverlappedExtentMargin ;
end ;

procedure TGIS_Viewer.fset_OverlappedExtentMargin(
  const _value : Integer
) ;
begin
  if _value < 0
  then FOverlappedExtentMargin := 0
  else FOverlappedExtentMargin := _value ;
end ;

function TGIS_Viewer.fget_PPI
  : Integer ;
begin
  if FCurrentPPI = 0 then begin
    if FCustomPPI = 0 then
      FCurrentPPI := SystemPPI
    else
      FCurrentPPI := FCustomPPI ;
  end ;
  Result := RoundS( FCurrentPPI * Parent.ControlCanvasScale ) ;
end ;

function TGIS_Viewer.fget_ProjectFile
  : TGIS_ConfigAbstract ;
begin
  Result := FProjectFile ;
end ;

procedure TGIS_Viewer.fset_ProjectFile(
  const _value : TGIS_ConfigAbstract
) ;
begin
  FProjectFile := TGIS_Config( _value );
end ;

function TGIS_Viewer.fget_ProjectName
  : String ;
begin
  Result := FProjectName ;
end ;

function TGIS_Viewer.fget_DelayedUpdate
  : Integer ;
begin
  Result := FDelayedUpdate  ;
end ;

procedure TGIS_Viewer.fset_DelayedUpdate(
  const _value : Integer
) ;
begin
  if _value > 0 then
    FDelayedUpdate := _value
  else
    FDelayedUpdate := 0 ;
end ;

function TGIS_Viewer.fget_ProgressiveUpdate
  : Integer ;
begin
  Result := FProgressiveUpdate  ;
end ;

procedure TGIS_Viewer.fset_ProgressiveUpdate(
  const _value : Integer
) ;
begin
  if _value > 0 then
    FProgressiveUpdate := _value
  else
    FProgressiveUpdate := 0 ;
end ;

function TGIS_Viewer.fget_RestrictedDrag
  : Boolean ;
begin
  Result := FRestrictedDrag  ;
end ;

procedure TGIS_Viewer.fset_RestrictedDrag(
  const _value : Boolean
) ;
begin
  FRestrictedDrag := _value ;
end ;

function TGIS_Viewer.fget_RestrictedExtent
  : TGIS_Extent ;
begin
  Result := FRestrictedExtent ;
end ;

procedure TGIS_Viewer.fset_RestrictedExtent(
  const _value : TGIS_Extent
) ;
begin
  {$IFDEF GIS_NORECORDS}
    if not assigned( FRestrictedExtent ) then
      FRestrictedExtent := TGIS_Extent.create ;
  {$ENDIF}
  FRestrictedExtent.XMin := _value.XMin - GIS_GAP_SIZE / Zoom ;
  FRestrictedExtent.XMax := _value.XMax + GIS_GAP_SIZE / Zoom ;
  FRestrictedExtent.YMin := _value.YMin - GIS_GAP_SIZE / Zoom ;
  FRestrictedExtent.YMax := _value.YMax + GIS_GAP_SIZE / Zoom ;

  RecalcExtent ;
end ;

function TGIS_Viewer.fget_RotationAngle
  : Double ;
begin
  Result := FRotationAngle ;
end ;

procedure TGIS_Viewer.fset_RotationAngle(
  const _value : Double
) ;
begin
  FRotationAngle := _value ;

  SinCos( -FRotationAngle, rotateSin, rotateCos     ) ;
  SinCos(  FRotationAngle, unrotateSin, unrotateCos ) ;

  RecalcExtent ;
end ;

function TGIS_Viewer.fget_RotationPoint
  : TGIS_Point ;
begin
  Result := FRotationPoint ;
end ;

procedure TGIS_Viewer.fset_RotationPoint(
  const _value : TGIS_Point
) ;
var
  ptg  : TGIS_Point ;
  ptg1 : TGIS_Point ;
  ptg2 : TGIS_Point ;
  x,y  : Double     ;
begin

  ptg :=  unrotatePtg( _TGIS_Point( _value ) ) ;
  ptg1 := rotatePtg  ( CenterPtg ) ;

  FRotationPoint := _TGIS_Point(ptg) ;

  ptg2 := rotatePtg( CenterPtg ) ;
  x := ptg2.X - ptg1.X + Viewport.X ;
  y := ptg1.Y - ptg2.Y + Viewport.Y ;
  {$IFDEF JAVA}
    SetViewport( java.lang.Double(x), java.lang.Double(y) );
  {$ELSE}
    SetViewport( x, y );
  {$ENDIF}

  RotationAngle := RotationAngle ;
end ;

function TGIS_Viewer.fget_Level : Double ;
var
  lat : Double ;
begin
  lat := CS.ToWGS( Center ).Y ;
  Result := Log2( ONE_PX_IN_M / INCH_TO_M * PPI * ScaleAsFloat * Abs( Cos( lat ) ) ) ;
end ;

procedure TGIS_Viewer.fset_Level(
  const _value : Double
) ;
var
  res : Double ;
  lat : Double ;
  res_lat_correction : Double ;
begin
  res := ONE_PX_IN_M / Power( 2, _value ) ;
  lat := CS.ToWGS( Center ).Y ;
  res_lat_correction := res * Cos( lat ) ;
  ScaleAsFloat := INCH_TO_M / ( PPI * res_lat_correction ) ;
end ;

function TGIS_Viewer.fget_ScaleAsFloat
  : Double ;
var
  pt1    : TGIS_Point ;
  pt2    : TGIS_Point ;
  dist   : Double     ;
  elp    : TGIS_CSEllipsoid ;
  factor : Double    ;

  function prepare_point( const _delta : Integer ) : Boolean ;
  var
    pt : TGIS_Point ;
  begin
    pt := ScreenToMapEx( GisPoint( 1.0 * oParent.ControlCanvasWidth  / 2 - _delta,
                                   1.0 * oParent.ControlCanvasHeight / 2
                                 )
                       ) ;
    pt := UnrotatedPoint( pt ) ;
    pt1 := _TGIS_Point(pt) ;

    pt := ScreenToMapEx( GisPoint( 1.0 * oParent.ControlCanvasWidth  / 2 + _delta,
                                   1.0 * oParent.ControlCanvasHeight / 2
                                 )
                       ) ;
    pt := UnrotatedPoint( pt ) ;
    pt2 := _TGIS_Point(pt) ;

    factor := ( 1.0 * PixelsToTwips( 2 * _delta ) / 1440 * 2.54 / 100 ) ;

    // protect against potential overflow
    if ( pt1.X > 1e30 ) or
       ( pt1.Y > 1e30 ) or
       ( pt2.X > 1e30 ) or
       ( pt2.Y > 1e30 )
    then
      Result := False
    else
      Result := True ;
  end ;

  function calculate_distance : Double ;
  var
    ptwgs1 : TGIS_Point ;
    ptwgs2 : TGIS_Point ;
  begin
    Result := 0 ;

    ptwgs1 := CS.ToWGS( pt1 ) ;
    if not GisIsValidPtg( ptwgs1 ) then exit ;

    ptwgs2 := CS.ToWGS( pt2 ) ;
    if not GisIsValidPtg( ptwgs2 ) then exit ;

    Result := elp.Distance( ptwgs1, ptwgs2 ) ;
  end ;

begin
  if TemporaryScaleInternal <> 0 then begin
    Result := TemporaryScaleInternal ;
    exit ;
  end ;

  Result := 1 ;

  if IsEmpty then exit ;

  try
    if not prepare_point( 5 ) then exit ;

    elp := nil ;
    if      CS is TGIS_CSProjectedCoordinateSystem  then begin
            elp := ( CS as TGIS_CSProjectedCoordinateSystem
                   ).Geocs.Datum.Ellipsoid ;
    end
    else if CS is TGIS_CSGeographicCoordinateSystem then
            elp := ( CS as TGIS_CSGeographicCoordinateSystem
                   ).Datum.Ellipsoid ;

    if assigned( elp ) then begin
      dist := calculate_distance ;
      if dist = 0 then exit ;
      if ( dist < 10 ) then begin
        // increase precision
        if not prepare_point( 500 ) then exit ;
        dist := calculate_distance ;
        if dist = 0 then exit ;
      end ;
      Result := factor / dist ;
    end
    else begin
      dist := Sqrt( Sqr( pt1.X - pt2.X ) + Sqr( pt1.Y - pt2.Y ) ) ;
      if dist = 0 then exit ;
      Result := factor / dist ;
    end ;
  except
    // any potential float overflow
  end ;
end ;

procedure TGIS_Viewer.fset_ScaleAsFloat(
  const _value : Double
) ;
var
  i   : Integer ;
  eps : Double  ;
begin
  Lock ;
  bBlockRepositionEvents := True ;
  try
    eps := _value * GIS_DOUBLE_RESOLUTION ;

    if Abs( FScale - _value ) < eps then
      exit ;

    i := 0 ;
    while ( i < 10 ) and
          ( Abs( FScale - _value ) > eps )
    do begin
      Zoom := Zoom * _value / ScaleAsFloat ;
      inc( i ) ;
    end ;
  finally
    bBlockRepositionEvents := False ;
    Zoom := FZoom ;
    Unlock ;
  end ;
end ;

function TGIS_Viewer.fget_ScaleAsText
  : String ;
var
  dval  : Double ;
  dval2 : Double ;
begin
  dval  := ScaleAsFloat ;

  if      dval =  0     then Result := ''
  else if dval >  1e30  then Result := ''
  else if dval <  1     then begin
                               dval2 := 1/dval ;
                               if      dval2 > 1000000 then
                                       dval2 := RoundTo( dval2, 4)
                               else if dval2 >  100000 then
                                       dval2 := RoundTo( dval2, 3)
                               else if dval2 >   10000 then
                                       dval2 := RoundTo( dval2, 2)
                               else if dval2 >    1000 then
                                       dval2 := RoundTo( dval2, 1)
                               else if dval2 >     100 then
                                       dval2 := RoundTo( dval2, 1)
                               else if dval2 >      10 then
                                       dval2 := RoundTo( dval2, 0) ;
                               Result := Format( '1:%.0f'  , [ dval2 ] ) ;
                             end
  else if dval >= 1     then begin
                               dval2 := dval ;
                               if      dval2 > 1000000 then
                                       dval2 := RoundTo( dval2, 4)
                               else if dval2 >  100000 then
                                       dval2 := RoundTo( dval2, 3)
                               else if dval2 >   10000 then
                                       dval2 := RoundTo( dval2, 2)
                               else if dval2 >    1000 then
                                       dval2 := RoundTo( dval2, 1)
                               else if dval2 >     100 then
                                       dval2 := RoundTo( dval2, 1)
                               else if dval2 >      10 then
                                       dval2 := RoundTo( dval2, 0) ;
                               Result := Format( '%.0f:1'  , [ dval2 ] ) ;
                             end
end ;

procedure TGIS_Viewer.fset_ScaleAsText(
  const _value : String
) ;
var
  i    : Integer ;
  a, b : Double  ;
begin
  i := Pos( String(':'), _value ) ;
  if i = StringFirst - 1 then  i := Pos( String('/'), _value ) ;

  if i > StringFirst - 1 then begin
    a := DotStrToFloat( Copy( _value, StringFirst    , i - StringFirst ) ) ;
    b := DotStrToFloat( Copy( _value, i + 1, 1024  ) ) ;

    ScaleAsFloat := a/b ;
  end
  else
    ScaleAsFloat := DotStrToFloat( _value ) ;
end ;

function TGIS_Viewer.fget_SelectionGisColor
  : TGIS_Color ;
begin
  Result := FSelectionGisColor ;
end ;

procedure TGIS_Viewer.fset_SelectionGisColor(
  const _value : TGIS_Color
) ;
begin
  FSelectionGisColor := _value ;
end ;

function TGIS_Viewer.fget_SelectionOutlineOnly
  : Boolean ;
begin
  Result := FSelectionOutlineOnly ;
end ;

procedure TGIS_Viewer.fset_SelectionOutlineOnly(
  const _value : Boolean
) ;
begin
  FSelectionOutlineOnly := _value ;
end ;

function TGIS_Viewer.fget_SelectionTransparency
  : Integer ;
begin
  Result := FSelectionTransparency ;
end ;

procedure TGIS_Viewer.fset_SelectionTransparency(
  const _value : Integer
) ;
begin
  FSelectionTransparency := Min( Max( 0, _value ), 100 ) ;
end ;

function TGIS_Viewer.fget_SelectionWidth
  : Integer ;
begin
  Result := FSelectionWidth ;
end ;

procedure TGIS_Viewer.fset_SelectionWidth(
  const _value : Integer
) ;
begin
  FSelectionWidth := _value ;
end ;

function TGIS_Viewer.fget_SystemPPI
  : Integer ;
begin
  Result := Parent.ControlSystemPPI ;
end ;

function TGIS_Viewer.fget_ViewerParent
  : IGIS_ViewerParent ;
begin
  Result := oParent ;
end ;

function TGIS_Viewer.fget_ViewerParentRoot
  : IGIS_ViewerParent ;
begin
  if assigned( MasterViewer ) then
    Result := MasterViewer.ViewerParent
  else
    Result := oParent ;
end ;

function TGIS_Viewer.fget_Viewport
  : TGIS_Point ;
begin
  Result := _TGIS_Point(FViewport) ;
end ;

procedure TGIS_Viewer.fset_Viewport(
  const _value : TGIS_Point
) ;
begin
  FViewport := _TGIS_Point(_value) ;
end ;

function TGIS_Viewer.fget_VisibleExtent
  : TGIS_Extent ;
begin
  if InPaint then
    Result := _TGIS_Extent(FFullDrawExtent)
  else
    Result := fget_VisibleExtentEx ;
end ;

procedure TGIS_Viewer.fset_VisibleExtent(
  const _value : TGIS_Extent
) ;
var
  zm       : Double      ;
  zm1, zm2 : Double      ;
  rct      : TGIS_Extent ;
  w,h      : Double      ;
begin
  if IsEmpty then exit ;
  if RestrictedDrag then
    if not GisIsCommonExtent( _value, Extent ) then exit ;
  if GisIsEmptyExtent(_value) then exit ;

  rct := _TGIS_Extent(_value) ;

  CenterPtg := GisCenterPoint( rct ) ;

  w := Abs( rct.XMax - rct.XMin) ;
  h := Abs( rct.YMax - rct.YMin) ;

  zm1 := 0 ;
  zm2 := 0 ;
  if ( w <> 0 ) then zm1 := oParent.ControlCanvasWidth  / w ;
  if ( h <> 0 ) then zm2 := oParent.ControlCanvasHeight / h ;

  if      h = 0 then begin
                  zm := zm1 ;
                end
  else if w = 0 then begin
                  zm := zm2 ;
                end
  else          begin
                  if zm1 < zm2 then // fit by width or height ?
                    zm := zm1
                  else
                    zm := zm2 ;
                  end ;

  if zm > 0 then begin
    FZoom := zm ;
  end ;

  Reposition ;
end ;

function TGIS_Viewer.fget_UseAnimations
  : Boolean ;
begin
  Result := FUseAnimations ;
end ;

procedure TGIS_Viewer.fset_UseAnimations(
  const _value : Boolean
) ;
begin
  FUseAnimations := _value ;
end ;

function TGIS_Viewer.fget_UseRTree
  : Boolean ;
begin
  Result := FUseRTree ;
end ;

procedure TGIS_Viewer.fset_UseRTree(
  const _value : Boolean
) ;
begin
  FUseRTree := _value ;
end ;

function TGIS_Viewer.fget_Zoom
  : Double ;
begin
  Result := FZoom ;
end ;

procedure TGIS_Viewer.fset_Zoom(
  const _value : Double
) ;
var
  value : Double ;
begin
  if IsEmpty then
    exit ;

  if FLocked.Level = 0  then
    if not SynchronizePaint( True) then
      exit ;

  if _value > 1e10 then value := 1
                   else value := _value ;

  if FItems.Count <= 0 then begin
    // to avoid zooming on empty viewer
    FZoom := value ;
    exit ;
  end ;

  if RestrictedDrag then
    if ( Extent.XMin <> Extent.XMax ) or
       ( Extent.YMin <> Extent.YMax ) then
      if value * 1.01 < FullExtentZoom then value := FullExtentZoom ;

  FViewport.X := 0 ;
  FViewport.Y := 0 ;

  FZoom := value ;

  Reposition ;
end ;

function TGIS_Viewer.fget_ZoomEx
  : Double ;
begin
  Result := Zoom/ PPI * 1440
end ;

procedure TGIS_Viewer.fset_ZoomEx(
  const _value : Double
) ;
begin
  Zoom := PPI /1440 * _value ;
end ;

function TGIS_Viewer.fget_MasterViewer
  : IGIS_Viewer ;
begin
  Result := FMasterViewer ;
end;

procedure TGIS_Viewer.fset_MasterViewer(
  const _value : IGIS_Viewer
) ;
begin
  FMasterViewer := _value ;
end;

function TGIS_Viewer.fget_UponDestroy
  : Boolean ;
begin
  Result := FUponDestroy ;
end ;

function  TGIS_Viewer.fget_TemporaryScaleInternal : Double      ;
begin
  Result := FTemporaryScaleInternal ;
end ;

procedure TGIS_Viewer.fset_TemporaryScaleInternal(
  const _value : Double
) ;
begin
  FTemporaryScaleInternal := _value ;
end;

function  TGIS_Viewer.fget_TemporaryVisibleExtent : TGIS_Extent ;
begin
  Result := FTemporaryVisibleExtent ;
end ;

procedure TGIS_Viewer.fset_TemporaryVisibleExtent(
  const _value : TGIS_Extent
) ;
begin
  FTemporaryVisibleExtent := _TGIS_Extent( _value ) ;
end;

function TGIS_Viewer.fget_Hierarchy
  : IGIS_HierarchyManager ;
begin
  Result := FHierarchy ;
end ;

function TGIS_Viewer.fget_VisibleExtentEx : TGIS_Extent ;
begin
  if GisIsNoWorld( Extent ) then begin
    Result := GisExtent( 0,0,0,0 ) ;
  end
  else begin
   Result := ScreenToMapRect(
               0,
               0,
               oParent.ControlCanvasWidth,
               oParent.ControlCanvasHeight
             ) ;
  end ;
end ;

{$IFDEF CLR}
  procedure TGIS_Viewer.fadd_EditorSnapPointEvent(
    const _value : TGIS_EditorSnapPointEvent
  ) ;
  begin
    if assigned( FEditor ) then
      FEditor.SnapPointEvent += _value ;
  end ;

  procedure TGIS_Viewer.fremove_EditorSnapPointEvent(
    const _value : TGIS_EditorSnapPointEvent
  ) ;
  begin
    if assigned( FEditor ) then
      FEditor.SnapPointEvent -= _value ;
  end ;

  procedure TGIS_Viewer.fadd_EditorPointChangeEvent(
    const _value : TGIS_EditorPointChangeEvent
  ) ;
  begin
    if assigned( FEditor ) then
      FEditor.PointChangeEvent += _value ;
  end ;

  procedure TGIS_Viewer.fremove_EditorPointChangeEvent(
    const _value : TGIS_EditorPointChangeEvent
  ) ;
  begin
    if assigned( FEditor ) then
      FEditor.PointChangeEvent -= _value ;
  end ;

  procedure TGIS_Viewer.fadd_EditorPointMoveEvent(
    const _value : TGIS_EditorPointMoveEvent
  ) ;
  begin
    if assigned( FEditor ) then
      FEditor.PointEditingEvent += _value ;
  end ;

  procedure TGIS_Viewer.fremove_EditorPointMoveEvent(
    const _value : TGIS_EditorPointMoveEvent
  ) ;
  begin
    if assigned( FEditor ) then
      FEditor.PointEditingEvent -= _value ;
  end ;

{$ELSE}

  function TGIS_Viewer.fget_EditorSnapPointEvent
    : TGIS_EditorSnapPointEvent ;
  begin
    if assigned( FEditor ) then
      Result := FEditor.SnapPointEvent
    else
      Result := nil ;
  end ;

  procedure TGIS_Viewer.fset_EditorSnapPointEvent(
    const _value : TGIS_EditorSnapPointEvent
  ) ;
  begin
    if assigned( FEditor ) then
      FEditor.SnapPointEvent := _value ;
  end ;

  function TGIS_Viewer.fget_EditorPointChangeEvent
    : TGIS_EditorPointChangeEvent ;
  begin
    if assigned( FEditor ) then
      Result := FEditor.PointChangeEvent
    else
      Result := nil ;
  end ;

  procedure TGIS_Viewer.fset_EditorPointChangeEvent(
    const _value : TGIS_EditorPointChangeEvent
  ) ;
  begin
    if assigned( FEditor ) then
      FEditor.PointChangeEvent := _value ;
  end ;

  function TGIS_Viewer.fget_EditorPointMoveEvent
    : TGIS_EditorPointMoveEvent ;
  begin
    if assigned( FEditor ) then
      Result := FEditor.PointEditingEvent
    else
      Result := nil ;
  end ;

  procedure TGIS_Viewer.fset_EditorPointMoveEvent(
    const _value : TGIS_EditorPointMoveEvent
  ) ;
  begin
    if assigned( FEditor ) then
      FEditor.PointEditingEvent := _value ;
  end ;
{$ENDIF}

procedure TGIS_Viewer.saveProjectFile(
  const _path         : String  ;
  const _relativepath : Boolean ;
  const _clearstate   : Boolean ;
  const _substitute   : Boolean
) ;
var
  i       : Integer         ;
  la      : TGIS_Layer      ;
  prj     : TGIS_Config     ;
  prj_tmp : TGIS_Config     ;
  lst     : TGIS_StringList ;
  done    : Boolean         ;
begin
  inc( iUponSave ) ;
  try
    done := False ;

    try
      prj := TGIS_ConfigFactory.CreateConfig( nil, _path );
      try
        lst := TGIS_StringList.Create ;
        try
          if assigned( ProjectFile ) then
            FProjectFile.GetStrings( lst );
          prj.SetStrings( lst )  ;
        finally
          FreeObject( lst ) ;
        end ;

        prj_tmp := FProjectFile ;
        FProjectFile := prj ;
        try
          FProjectFile.UseRelativePath := _relativepath ;
          FProjectFile.BuildProject( self ) ;

          WriteConfig ;

          for i:=0 to Items.Count -1 do begin
            la := TGIS_Layer( Items[i] ) ;
            la.WriteConfig ;
            if assigned( la.ConfigFile )                and
               TGIS_Config( la.ConfigFile ).MustSave    and
               la.UseConfig
            then
              try
                // save config here to catch read-only error
                // and save changes to project file
                TGIS_Config( la.ConfigFile ).Save ;
                if _clearstate then
                  TGIS_Config( la.ConfigFile ).ClearSave ;
              except
                // ignore INI save errors and save to ProjectFile
                la.UseConfig := False ;
                la.WriteConfig ;
                la.UseConfig := True ;
                if _clearstate then
                  TGIS_Config( la.ConfigFile ).ClearSave ;
              end;
          end;

          FProjectFile.WriteHierarchyGroups( self ) ;

          FProjectFile.Save ;

          if _substitute then begin
            FProjectName := _path ;
            FreeObject( prj_tmp ) ;
            done := True ;
          end
          else
            FProjectFile := prj_tmp ;
        except
          FProjectFile := prj_tmp ;
        end ;
      finally
        if not done then
          FreeObject( prj ) ;
      end ;
    except
    end ;

    if _clearstate then begin
      FIsModified := False ;
      Hierarchy.IsModified := False ;
    end;
  finally
    dec( iUponSave ) ;
  end;
end;

procedure TGIS_Viewer.notifyLocked(
  const _extent   : TGIS_Extent ;
  const _wholemap : Boolean
) ;
begin
  if not IsLocked then exit ;

  FLocked.WholeMap := FLocked.WholeMap or _wholemap ;
  FLocked.Topmost  := FLocked.Topmost  or ( not _wholemap ) ;

  FLocked.Extent := GisMaxExtent( FLocked.Extent, _extent ) ;
end ;

function TGIS_Viewer.getUniqueLayerName( const _name : String ) : String ;
var
  i   : Integer ;
  tmp : String  ;
begin
  i := 0 ;
  Result := _name ;
  while Get( Result ) <> nil do begin
    if i > 0 then
      tmp := Copy( Result, StringFirst, length( Result ) -
                   length( Format( ' [%d]', [i] ) )
                 )
    else
      tmp := Result ;

    Result := Format( '%s [%d]', [tmp, i+1 ] ) ;

    inc( i ) ;
  end ;
end ;

procedure TGIS_Viewer.setDirectPaint ;
begin
  notifyLocked( GisNoWorld, True ) ;
end ;

procedure TGIS_Viewer.rotatePtg3D_ref(
  {$IFNDEF JAVA} var {$ENDIF}  _ptg : TGIS_Point3D
) ;
var
  tmp : TGIS_Point3D ;
begin
  if RotationAngle = 0 then
    exit ;

  tmp := _TGIS_Point3D(_ptg);

  _ptg.X := ( ( tmp.X - FRotationPoint.X ) * rotateCos  -
              ( tmp.Y - FRotationPoint.Y ) * rotateSin
            ) +
            FRotationPoint.X ;
  _ptg.Y := ( ( tmp.X - FRotationPoint.X ) * rotateSin  +
              ( tmp.Y - FRotationPoint.Y ) * rotateCos
            ) +
            FRotationPoint.Y ;

end ;

procedure TGIS_Viewer.unrotatePtg3D_ref(
  {$IFNDEF JAVA} var {$ENDIF}  _ptg : TGIS_Point3D
) ;
var
  tmp : TGIS_Point3D ;
begin
  if RotationAngle = 0 then
    exit ;

  tmp := _TGIS_Point3D(_ptg) ;


  _ptg.X := ( ( tmp.X - FRotationPoint.X ) * unrotateCos  -
              ( tmp.Y - FRotationPoint.Y ) * unrotateSin
            ) +
            FRotationPoint.X ;
  _ptg.Y := ( ( tmp.X - FRotationPoint.X ) * unrotateSin  +
              ( tmp.Y - FRotationPoint.Y ) * unrotateCos
            ) +
            FRotationPoint.Y ;
end ;

function TGIS_Viewer.rotatePtg( const _ptg : TGIS_Point ) : TGIS_Point ;
var
  tmp : TGIS_Point3D ;
begin
  {$IFDEF GIS_NORECORDS}
    tmp := new TGIS_Point3D ;
  {$ENDIF}
  tmp.X := _ptg.X ;
  tmp.Y := _ptg.Y ;
  tmp.Z := 0 ;
  tmp.M := 0 ;

  rotatePtg3D_ref( tmp ) ;

  {$IFDEF GIS_NORECORDS}
  Result := new TGIS_Point ;
  {$ENDIF}
  Result.X := tmp.X ;
  Result.Y := tmp.Y ;
end ;

function TGIS_Viewer.rotatePtg3D( const _ptg : TGIS_Point3D ) : TGIS_Point3D ;
var
  tmp : TGIS_Point3D ;
begin
  tmp := _TGIS_Point3D(_ptg) ;

  rotatePtg3D_ref( tmp ) ;

  Result := tmp ;
end ;

function TGIS_Viewer.unrotatePtg( const _ptg : TGIS_Point ) : TGIS_Point ;
var
  tmp : TGIS_Point3D ;
begin
  {$IFDEF GIS_NORECORDS}
    tmp := new TGIS_Point3D ;
  {$ENDIF}
  tmp.X := _ptg.X ;
  tmp.Y := _ptg.Y ;
  tmp.Z := 0 ;
  tmp.M := 0 ;

  unrotatePtg3D_ref( tmp ) ;

  {$IFDEF GIS_NORECORDS}
  Result := new TGIS_Point ;
  {$ENDIF}
  Result.X := tmp.X ;
  Result.Y := tmp.Y ;
end ;

function TGIS_Viewer.unrotatePtg3D(
  const _ptg : TGIS_Point3D
) : TGIS_Point3D ;
var
  tmp : TGIS_Point3D ;
begin
  tmp := _TGIS_Point3D(_ptg) ;

  unrotatePtg3D_ref( tmp ) ;

  Result := tmp ;
end ;

function TGIS_Viewer.getBigExtentEx( const _zoom : Double ) : TGIS_Extent ;
var
  factor : Double ;
begin
  Result := _TGIS_Extent( Extent ) ;
  if BigExtentMargin < 0 then begin
    factor := 1.0 * Abs( BigExtentMargin ) / 100 / 2 / _zoom ;
    Result.XMin := Result.XMin - oParent.ControlCanvasWidth  * factor ;
    Result.XMax := Result.XMax + oParent.ControlCanvasWidth  * factor ;
    Result.YMin := Result.YMin - oParent.ControlCanvasHeight * factor ;
    Result.YMax := Result.YMax + oParent.ControlCanvasHeight * factor ;
  end
  else if BigExtentMargin > 0 then begin
    factor := 1.0 * Abs( BigExtentMargin ) / 100 / 2 ;
    Result.XMin := Result.XMin - ( Result.XMax - Result.XMin ) * factor ;
    Result.XMax := Result.XMax + ( Result.XMax - Result.XMin ) * factor ;
    Result.YMin := Result.YMin - ( Result.YMax - Result.YMin ) * factor ;
    Result.YMax := Result.YMax + ( Result.YMax - Result.YMin ) * factor ;
  end
end ;

function TGIS_Viewer.getBusyState : Integer ;
begin
  if FBusyText.Count > 0 then begin
    Result := FBusyText[ FBusyText.Count - 1 ].Value ;
  end
  else
    Result := 0 ;
end ;

procedure TGIS_Viewer.ResetPPI ;
begin
  FCurrentPPI := 0 ;
end;

function TGIS_Viewer.doGetHash(
  const _persistent : Boolean
) : Int64 ;
var
  i   : Integer ;
  tmp : TStringBuilder ;
  la  : TGIS_Layer ;

  procedure hashGroup( const _g : IGIS_HierarchyGroup )  ;
  var
    jj  : Integer ;
  begin
    tmp.Append( ':' ) ;
    tmp.Append( _g.Name ) ;
    tmp.Append( ':' ) ;
    tmp.Append( _g.Caption ) ;
    tmp.Append( ':' ) ;
    tmp.Append( IntToStr( Integer( _g.Active ) ) ) ;
    tmp.Append( ':' ) ;
    tmp.Append( IntToStr( Integer( _g.Collapsed ) ) ) ;

    for jj := 0 to _g.LayersCount -1 do begin
      tmp.Append( ':' )  ;
      tmp.Append( TGIS_Layer( _g.Layers[jj] ).Name ) ;
    end;

    for jj := 0 to _g.GroupsCount-1 do
      hashGroup( _g.Groups[jj] ) ;
  end;

begin
  tmp := TStringBuilder.Create ;
  try
    for i := 0 to Items.Count - 1 do begin
      la := TGIS_Layer( Items[i] ) ;
      if _persistent and not la.IsPersistent then continue ;

      tmp.Append(':' ) ;
      tmp.Append( IntToStr( la.ChangeHash ) ) ;
    end;

    for i := 0 to Hierarchy.GroupsCount - 1 do
      hashGroup( Hierarchy.Groups[i] )  ;

    Result := StringHash( tmp.ToString ) ;
  finally
    FreeObject( tmp ) ;
  end ;
end;

function TGIS_Viewer.ChangeHash : Int64 ;
begin
  Result := doGetHash( False ) ;
end;

procedure TGIS_Viewer.Subscribe( const _control : IGIS_Subscribe ) ;
begin
  if assigned( _control ) then
    if lstSubscribers.IndexOf( _control ) < 0 then
      lstSubscribers.Add( _control ) ;
end ;

procedure TGIS_Viewer.UnSubscribe( const _control : IGIS_Subscribe ) ;
begin
  lstSubscribers.Remove( _control ) ;
end ;

procedure TGIS_Viewer.NotifySubscribers(
  const _event   : Integer ;
  const _context : TObject
) ;
var
  i : Integer ;
begin
  if oParent is TGIS_PrintManagerAbstract then exit ;

  if not assigned( lstSubscribers ) then exit ;
  for i:=0 to lstSubscribers.Count -1 do
    IGIS_Subscribe( lstSubscribers[i] ).SubscribedEvent(
                                          self, _event, _context
                                        ) ;
end;

function TGIS_Viewer.NotifyPaintException(
  const _message    : String   ;
  const _exception  : Exception
) : Boolean ;
var
  slog : String ;
  {$IFNDEF OXYGENE}
    exception : EGIS_PaintException ;
  {$ENDIF}
begin
  Result := False ;

  slog := _exception.Message ;

  {$IFDEF OXYGENE}
    if assigned( FOnPaintException ) then begin
      Result := True ;
      if _exception is EGIS_PaintException then
        FOnPaintException( Self,
                           TGIS_PaintExceptionEventArgs.Create(
                             EGIS_PaintException( _exception )
                           )
                         )
      else
        FOnPaintException( Self,
                           TGIS_PaintExceptionEventArgs.Create(
                             EGIS_PaintException.Create(
                              _message, _exception
                             )
                           )
                         ) ;
    end ;

    {$IFDEF CLR}
      using st := new System.Diagnostics.StackTrace do begin
        slog := slog + ' ' + st.ToString;
      end;

      Parent.ControlProcessMessages ;
    {$ENDIF}
    {$IFDEF JAVA}
      var sw: java.io.StringWriter := new java.io.StringWriter();
      var pw: java.io.PrintWriter := new java.io.PrintWriter(sw);
      _exception.printStackTrace(pw);
      slog := slog + ' ' + sw.ToString;

      java.lang.Thread.yield ;
    {$ENDIF}
    {$IFDEF ISLAND}
    {$ENDIF}
  {$ELSE}
    if assigned( PaintExceptionEvent ) then begin
      Result := True ;
      if _exception is EGIS_PaintException then
        PaintExceptionEvent( Self, EGIS_PaintException( _exception ) )
      else begin
        exception := EGIS_PaintException.Create( _message, _exception ) ;
        try
          PaintExceptionEvent( Self, exception ) ;
        finally
          FreeObject( exception ) ;
        end;
      end;
    end ;

  {$ENDIF}

  TGIS_Logger.AsError( _message, slog ) ;
end;



procedure TGIS_Viewer.Lock ;
begin
  inc( FLocked.Level ) ;
end ;

procedure TGIS_Viewer.Unlock ;
begin
  Unlock( True ) ;
end ;

procedure TGIS_Viewer.Unlock(
  const _redraw : Boolean
) ;
begin
  FLocked.Level   := Max( FLocked.Level   -1, 0 ) ;
  if FLocked.Level = 0 then begin
    if _redraw then begin
      if FLocked.WholeMap and _redraw then
        Parent.ControlUpdateWholeMap
      else if FLocked.Selection then
        Parent.ControlUpdateSelection
      else if FLocked.Topmost then
        Parent.ControlUpdateTopmost
      else if FLocked.Editor then
        Parent.ControlUpdateEditor( _redraw ) ;
    end;

    FLocked.WholeMap  := False ;
    FLocked.Topmost   := False ;
    FLocked.Selection := False ;
    FLocked.Editor    := False ;
    FLocked.Extent    := GisNoWorld ;
  end ;
end ;

procedure TGIS_Viewer.Interrupt ;
begin
  asapInterrupted := True ;
  if hourglassState > 0 then
    hourglassInterrupted := True ;
end ;

function TGIS_Viewer.Interrupted
  : Boolean ;
begin
  Result := asapInterrupted ;
end ;


function TGIS_Viewer.HourglassActive
  : Boolean ;
begin
  Result := hourglassState > 0 ;
end ;

procedure TGIS_Viewer.HourglassPrepare ;
begin
  inc( hourglassState ) ;
  if hourglassState = 1 then begin
    hourglassInterrupted := False ;
    hourglassInterval    := GetTickCount ;
    hourglassProgressive := hourglassInterval ;
  end ;
end ;

procedure TGIS_Viewer.HourglassRelease ;
begin
  if UponDestroy then
    exit ;

  if hourglassState = 0 then begin
    assert( False ) ;
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BAD_CALL ), 'HourglassRelease', 0 ) ;
  end ;

  dec( hourglassState ) ;

  if hourglassState = 0 then begin
    if hourglassVisible then
      Parent.ControlHourglassHide ;
    hourglassVisible := False ;
    hourglassInterrupted := False ;
  end ;
end ;

function TGIS_Viewer.HourglassShake : Boolean ;
var
  itick : Int64 ;

begin
  if UponDestroy then begin
    Result := True ;
    exit ;
  end;

  itick := GetTickCount ;
  if ( hourglassState > 0 ) and
     ( ( itick - hourglassInterval ) > HOURGLASS_WAKEUP ) then
  begin
    if not hourglassVisible then begin
      oParent.ControlHourglassShow ;
      hourglassVisible := True ;
    end;

    if FProgressiveUpdate > 0 then begin
      if ( itick - hourglassProgressive ) > FProgressiveUpdate then begin
        oParent.ControlUpdateProgressive ;
        hourglassProgressive := GetTickCount ;
      end;
    end ;

    Result := hourglassInterrupted ;
    Result := Result or oParent.ControlHourglassShake ;
    hourglassInterval := GetTickCount ;
  end
  else
    Result := False ;
end ;

procedure TGIS_Viewer.HourglassRestart ;
begin
  hourglassInterval := GetTickCount ;
  hourglassProgressive := hourglassInterval ;
end;

procedure TGIS_Viewer.BusyPrepare( _sender : TObject ;
                                   _text   : String
                                 ) ;
var
 bs : Integer ;
 {$IFNDEF OXYGENE}
   abort : Boolean ;
 {$ENDIF}
begin
  if IsStringEmpty( _text ) then
    FBusyText.Add( _rsrc( GIS_RS_BUSY_DEFAULT ), BUSY_NORMAL )
  else if _text = GIS_BUSY_NOEVENTS then
    FBusyText.Add( '', BUSY_NOEVENTS )
  else if _text = GIS_BUSY_NOHOURGLASS then
    FBusyText.Add( '', BUSY_NOHOURGLASS )
  else
    FBusyText.Add( _text, BUSY_NORMAL ) ;

  bs := getBusyState ;

  if bs < BUSY_NOHOURGLASS then
   HourglassPrepare ;

  if ( bs < BUSY_NOEVENTS ) and assigned( FOnBusy ) then begin
    // abort will be ignored
    {$IFDEF OXYGENE}
       FOnBusy( _sender, TGIS_BusyEventArgs.Create( 0, -1 ) ) ;
    {$ELSE}
      abort := False ;
      FOnBusy( _sender, 0, -1, abort ) ;
    {$ENDIF}
  end ;
end ;

procedure TGIS_Viewer.BusyRelease(
  _sender : TObject
) ;
var
  bs : Integer ;
  {$IFNDEF OXYGENE}
    abort : Boolean ;
  {$ENDIF}
begin
  if UponDestroy then
    exit ;

  if FBusyText.Count = 0 then begin
    assert( False ) ;
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BAD_CALL ), 'BusyRelease', 0 ) ;
  end ;

  bs := getBusyState ;

  if bs < BUSY_NOHOURGLASS then
    HourglassRelease ;

  if ( bs < BUSY_NOEVENTS ) and assigned( FOnBusy ) then begin
    // abort will be ignored
    {$IFDEF OXYGENE}
      FOnBusy( self, TGIS_BusyEventArgs.Create( -1, -1 ) ) ;
    {$ELSE}
      abort := False ;
      FOnBusy( self, -1, -1, abort ) ;
    {$ENDIF}
  end ;

  FBusyText.Delete( FBusyText.Count-1 ) ;

end ;

procedure TGIS_Viewer.BusyShake(
      _sender    : TObject ;
      _pos, _end : Int64 ;
  var _abort     : Boolean
) ;
var
  bs      : Integer ;
  isabort : Boolean ;
  {$IFDEF OXYGENE}
    bargs : TGIS_BusyEventArgs ;
  {$ENDIF}
begin
  if FBusyText.Count <= 0 then begin
    assert( False ) ;
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BAD_CALL ), 'BusyShake', 0 ) ;
  end ;

  bs := getBusyState ;

  if bs < BUSY_NOHOURGLASS then
    isabort := HourglassShake
  else
    isabort := False ;

  if ( bs < BUSY_NOEVENTS ) and assigned( FOnBusy ) then begin
    {$IFDEF OXYGENE}
      bargs := TGIS_BusyEventArgs.Create( _pos, _end, _abort ) ;
      try
        FOnBusy( _sender, bargs ) ;
      finally
        _abort := bargs.Abort ;
        FreeObject( bargs ) ;
      end ;
    {$ELSE}
      FOnBusy( _sender, _pos, _end, _abort ) ;
    {$ENDIF}
  end
  else
    _abort := isabort ;
end ;

{$IFDEF OXYGENE}
  procedure TGIS_Viewer.RaiseBusyEvent(
    _sender  : TObject ;
    _e       : TGIS_BusyEventArgs
  ) ;
{$ELSE}
  procedure TGIS_Viewer.RaiseBusyEvent(
        _sender  : TObject ;
        _pos     : Int64 ;
        _end     : Int64 ;
    var _abort   : Boolean
  ) ;
{$ENDIF}
begin
  if assigned( FOnBusy ) then begin
    {$IFDEF OXYGENE}
      FOnBusy( _sender, _e ) ;
    {$ELSE}
      FOnBusy( _sender, _pos, _end, _abort ) ;
    {$ENDIF}
  end
end ;

{$IFDEF OXYGENE}
  procedure TGIS_Viewer.RaiseHelpEvent(
    _sender  : TObject ;
    _e       : TGIS_HelpEventArgs
  ) ;
{$ELSE}
  procedure TGIS_Viewer.RaiseHelpEvent(
        _sender  : TObject ;
        _name    : String
  ) ;
{$ENDIF}
begin
  if assigned( FOnHelp ) then begin
    {$IFDEF OXYGENE}
      FOnHelp( _sender, _e ) ;
    {$ELSE}
      FOnHelp( _sender, _name ) ;
    {$ENDIF}
  end
end ;

function TGIS_Viewer.AssignedBusyEvent
  : TGIS_BusyEvent ;
begin
  Result := FOnBusy ;
end;

function TGIS_Viewer.AssignedHelpEvent
  : TGIS_HelpEvent ;
begin
  Result := FOnHelp ;
end;

function TGIS_Viewer.StorePaintState : TObject ;
var
  obj : T_paintState ;
begin
  obj := T_paintState.Create ;
  obj.Extent   := _TGIS_Extent(VisibleExtent)  ;
  obj.PPI      := PPI            ;
  obj.Center   := _TGIS_Point(CenterPtg)      ;

  Result := obj ;
end ;

procedure TGIS_Viewer.RestorePaintState( var _state : TObject ) ;
var
  obj : T_paintState ;
begin
  Lock ;
  try
    if assigned( _state ) then begin
      assert( _state is T_paintState ) ;

      obj := T_paintState( _state ) ;
      VisibleExtent   := _TGIS_Extent(obj.Extent) ;
      CenterPtg       := _TGIS_Point(obj.Center) ;
      FLabelsReg.Reset ;

      FreeObject( _state ) ;
    end ;
  finally
    Unlock( False ) ;
  end ;
end ;

procedure TGIS_Viewer.BeginPaintInternal ;
begin
  inc( FInPaint ) ;
  FFullDrawExtent := fget_VisibleExtentEx ;
end;

procedure TGIS_Viewer.EndPaintInternal ;
begin
  dec( FInPaint ) ;
end;

function TGIS_Viewer.SynchronizePaint(
  const _interrupt : Boolean
) : Boolean ;
begin
  if bUponOpen then begin
    Result := True ;
    exit ;
  end;

  if UponDestroy then begin
    Result := True ;
    exit ;
  end;

  Result := Parent.ControlUpdateSynchronize( _interrupt ) ;
end;

procedure TGIS_Viewer.ReParentLock ;
begin
  assert( FLocked.Reparent = 0 ) ;
  inc( FLocked.Reparent ) ;
end ;

procedure TGIS_Viewer.ReParentUnlock ;
begin
  dec( FLocked.Reparent ) ;
end ;

function TGIS_Viewer.ReParent(
  const _parent : IGIS_ViewerParent
) : IGIS_ViewerParent ;
begin
  Result := oParent ;
  oParent := _parent ;
  FCurrentPPI := 0 ;
end ;

function TGIS_Viewer.AttachLayer(
  const _layer : TGIS_LayerAbstract
) : IGIS_Viewer ;
begin
  if not assigned( _layer ) then begin
    Result := nil ;
    exit ;
  end;

  if assigned( TGIS_Layer( _layer ).Viewer ) then
    Result := TGIS_Layer( _layer ).Viewer.Ref
  else
    Result := nil ;

  TGIS_Layer( _layer ).ViewerReParent( FSelfRef ) ;
end;

procedure TGIS_Viewer.Open(
  const _path : String
) ;
begin
  Open( _path, True ) ;
end ;

procedure TGIS_Viewer.Open( const _path   : String ;
                            const _strict : Boolean
                          ) ;
var
  obj  : TGIS_ImportProject ;
  configObj : TGIS_Config  ;
  sPath    : String ;
begin
  if not SynchronizePaint( True) then
    exit ;

  try
    if IsStringEmpty( _path ) then exit ;

    if not IsServerPath( _path ) then
      sPath := ExpandFileNameEx( _path )
    else
      sPath := _path ;

    if assigned( FOnProjectOpen ) then
      {$IFDEF CLR}
        FOnProjectOpen( self, EventArgs.Create ) ;
      {$ELSE}
        FOnProjectOpen( self ) ;
      {$ENDIF}

    obj := TGIS_ImportProject.Create( self ) ;
    try
      if obj.IsSupported( sPath ) then begin
        OpenEx( nil, '', _strict ) ;
        obj.Import( sPath ) ;
        FTemporaryDirToDelete := obj.TemporaryDir ;
        exit ;
      end ;
    finally
      FreeObject( obj ) ;
    end ;

    if TGIS_ConfigFactory.IsProject( sPath ) then
      configObj := TGIS_ConfigFactory.CreateConfig( nil, _path )
    else
      configObj := nil ;

    OpenEx( configObj, sPath, _strict );
  finally
    FIsModified := False ;
    bUpgradable := True ;
  end ;
end ;

procedure TGIS_Viewer.OpenEx(
  const _configFile : TGIS_ConfigAbstract ;
  const _path       : String
) ;
begin
  OpenEx( _configFile, _path, True ) ;
end ;

procedure TGIS_Viewer.OpenEx(
  const _configFile : TGIS_ConfigAbstract ;
  const _path       : String              ;
  const _strict     : Boolean
) ;
var
  err      : String      ;
  abort    : Boolean     ;
  usebusy  : Boolean     ;
  dir      : String      ;
  sconfig  : String      ;
  ex       : TGIS_Extent ;
  i        : Integer     ;
  cnt      : Integer     ;
  ll       : TGIS_Layer  ;
begin
  if not SynchronizePaint( True) then
    exit ;

  usebusy := False ;
  abort   := False ;
  err     := '' ;

  {$IFDEF DCC}
    // This line is dedicated to the VCL framework.
    // When the OpenEx function is called inside the FormCreate handler,
    // the line below forces the window to be physically created
    // before opening the project.
    oParent.ControlCanvasWidth ;
  {$ENDIF}

  bUponOpen := True ;
  try
    Close ;

    oldColor := FColor ;

    if IsStringEmpty( _path ) then exit ;

    if assigned( FOnProjectOpen ) then
      {$IFDEF CLR}
        FOnProjectOpen( self, EventArgs.Create ) ;
      {$ELSE}
        FOnProjectOpen( self ) ;
      {$ENDIF}

    FIsModified := False ;

    dir := GetFileDir( _path ) ;

    if assigned( _configFile ) and ( TGIS_Config( _configFile ).IsProject ) then
    begin
      FProjectFile := TGIS_Config( _configFile ) ;

      BusyPrepare( Self,
                   Format( _rsrc( GIS_RS_BUSY_OPEN ), [ GetFileNameNoExt(_path) ] )
                 ) ;
      usebusy := True ;
      BusyShake( Self, 1, 100, abort ) ;

      ReadConfig ;

      BusyShake( Self, 5, 100, abort ) ;

      FProjectName := _path ;

      try
        Lock ;
        cnt := FProjectFile.PrjLayersCount ;
        for i := 1 to cnt do begin
          BusyShake( Self,
                     RoundS(5 + i*95.0/cnt),
                     100,
                     abort
                   ) ;

          ll := GisCreateLayer( FProjectFile.PrjLayerName[i],
                                GetPathAbsolute( dir,
                                                 FProjectFile.PrjLayerPath[i]
                                                )
                               ) ;

          if assigned( ll ) then begin
            if _strict or IsFileOrServerPath( GetPathAbsolute( dir, ll.Path ) )
            then begin
              try
                Add( ll ) ;
              except
                on e : Exception do begin
                  if Items.Remove( ll ) < 0 then
                    FreeObject( ll ) ;
                  ll := nil ;
                  if _strict then
                    err := err + #13#10 + e.Message ;
                  continue ;
                end ;
              end ;
              sconfig := ll.ConfigName ;
              ll.ConfigName := GetPathAbsolute( dir,
                                                FProjectFile.PrjLayerConfig[i]
                                              ) ;
              if ll.ConfigName <> sconfig then
                ll.ReadConfig ;
            end
            else
              FreeObject( ll );
          end
          else begin
            if _strict then begin
              try
                raise EGIS_Exception.Create(
                        _rsrc( GIS_RS_ERR_LAYERUNKNOWN ),
                        FProjectFile.PrjLayerPath[i],
                        0
                      ) ;

              except
                on e : Exception do begin
                  err := err + #13#10 + e.Message ;
                end;
              end;
            end;

            FreeObject( ll );
          end;

          if abort then break ;
        end ;

        AddHierarchy;

        FullExtent ;

        with FProjectFile do begin
          SetLayer( nil ) ;
          {$IFDEF GIS_NORECORDS}
            ex := new TGIS_Extent ;
          {$ENDIF}
          ex.XMin := ReadFloat( GIS_INI_VISIBLEEXTENT_XMIN, VisibleExtent.XMin ) ;
          ex.XMax := ReadFloat( GIS_INI_VISIBLEEXTENT_XMAX, VisibleExtent.XMax ) ;
          ex.YMin := ReadFloat( GIS_INI_VISIBLEEXTENT_YMIN, VisibleExtent.YMin ) ;
          ex.YMax := ReadFloat( GIS_INI_VISIBLEEXTENT_YMAX, VisibleExtent.YMax ) ;
        end ;
        VisibleExtent := _TGIS_Extent(ex) ;

      finally
        Unlock ;
        FIsModified := False ;
      end ;
    end
    else begin
      ll := GisCreateLayer( '', _path ) ;
      if assigned( ll ) then begin
        try
          Add( ll ) ;
          AddHierarchy;
          FullExtent ;
        except
          if Items.Remove( ll ) < 0 then
            FreeObject( ll ) ;
          ll := nil ;
          raise ;
        end ;
      end ;
    end ;

  finally
    for i:= 0 to Items.Count -1 do begin
      ll := TGIS_Layer( Self.Items[i] ) ;
      if ll.IsOpened and (ll.CS is TGIS_CSUnknownCoordinateSystem) then begin
        Self.CS := nil ;
        break ;
      end ;
    end ;

    if usebusy then
      BusyRelease( self ) ;

    FIsModified := False ;
    bUponOpen := False ;
    iLastHash := ChangeHash ;

    if not IsStringEmpty( err ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERMISSED ), err, 0 ) ;
  end ;
end ;

procedure TGIS_Viewer.Close ;
var
  i  : Integer ;
  la : TGIS_Layer ;
begin
  if not SynchronizePaint( True) then
    exit ;

  if Items.Count > 0 then
    NotifySubscribers( GIS_SUBSCRIBED_PROJECT_CLOSE, self ) ;

  Parent.ControlClose ;

  if assigned( FOnProjectClose ) then
    {$IFDEF CLR}
      FOnProjectClose( self, EventArgs.Create ) ;
    {$ELSE}
      FOnProjectClose( self ) ;
    {$ENDIF}

//  if assigned( FEditor ) then
//    FEditor.EndEdit ;

  // restore color but only if it was something stored in oldColor
  if oldColor.ARGB <> TGIS_Color.None.ARGB then FColor := oldColor ;

  if Items.Count > 0 then
    GisPasswordList.Clear ;

  if assigned( FHierarchy ) and not FUponDestroy then
    FHierarchy.ClearGroups ;

  for i := ( Items.Count-1 ) downto 0 do begin// all layers
    la := TGIS_Layer( Items[i] ) ;

    while Items.OwnsObjects and (la.Viewer <> FSelfRef) do begin
      la.Viewer := la.Viewer ;
      //wait for Basemap object for properly released layer
      Sleep(100) ;
    end;

    if la.Viewer <> FSelfRef then
      continue ;

    if assigned( FOnLayerDelete ) then begin
      {$IFDEF OXYGENE}
        FOnLayerDelete( self, TGIS_LayerEventArgs.Create( la ) ) ;
      {$ELSE}
        FOnLayerDelete( self, la ) ;
      {$ENDIF}
    end ;

    if assigned( Editor ) then
      FEditor.RemoveSnapLayer( la ) ;
  end ;
  Items.Clear ;
  FExtent := GisNoWorld ;

  FProjectName := '' ;
  FreeObject( FProjectFile ) ;

  if not IsStringEmpty( FTemporaryDirToDelete ) then begin
    DeleteDirectory( FTemporaryDirToDelete ) ;
    FTemporaryDirToDelete := '' ;
  end ;

  FZoom := GIS_INITIAL_ZOOM ;

  // restore Viewer setup
  if oldProperties.Modified then begin
    Color            := oldProperties.Color            ;
    IncrementalPaint := oldProperties.IncrementalPaint ;

    oldProperties.Modified := False ;
  end ;

  if not FUponDestroy then begin
    oParent.ControlUpdateWholeMap ;
    CS := nil ;
  end ;

  FIsModified := False ;
end ;

procedure TGIS_Viewer.ReadConfig ;
var
  epsg : Integer ;
  wkt  : String  ;
begin
  if not assigned( ProjectFile ) then exit ;

  with FProjectFile do begin
    SetLayer( nil ) ;

    // store current properties
    oldProperties.Modified         := True             ;
    oldProperties.Color            := Color            ;
    oldProperties.IncrementalPaint := IncrementalPaint ;

    Color            := ReadColor      ( GIS_INI_BACKGROUNDCOLOR,
                                         Color
                                       ) ;

    IncrementalPaint := ReadBoolean    ( GIS_INI_INCREMENTALPAINT,
                                         IncrementalPaint
                                       ) ;

    epsg             := ReadInteger    ( GIS_INI_CS_EPSG,
                                         0
                                       ) ;
    wkt              := ReadString     ( GIS_INI_CS_WKT,
                                         ''
                                       ) ;

    if ( epsg > 0 )  or ( not IsStringEmpty( wkt ) ) then begin
      if epsg <= 0 then
        self.FCS := CSUnknownCoordinateSystem
      else
        self.FCS := TGIS_CSFactory.ByEPSG( epsg ) ;

      if self.FCS.EPSG <= 0 then
        self.FCS := TGIS_CSFactory.ByWKT( wkt ) ;
    end ;

    ReadCustomData( GIS_INI_CUSTOM, CustomData );
  end ;
end ;

procedure TGIS_Viewer.WriteConfig ;
begin
  if not assigned( ProjectFile ) then exit ;

  with FProjectFile do begin

    SetLayer( nil ) ;

    WriteBoolean    ( GIS_INI_INCREMENTALPAINT, IncrementalPaint, True  ) ;

    WriteFloat( GIS_INI_VISIBLEEXTENT_XMIN, VisibleExtent.XMin, 0 ) ;
    WriteFloat( GIS_INI_VISIBLEEXTENT_XMAX, VisibleExtent.XMax, 0 ) ;
    WriteFloat( GIS_INI_VISIBLEEXTENT_YMIN, VisibleExtent.YMin, 0 ) ;
    WriteFloat( GIS_INI_VISIBLEEXTENT_YMAX, VisibleExtent.YMax, 0 ) ;

    if CS.EPSG < GIS_EPSG_AUTO then begin
      WriteInteger( GIS_INI_CS_EPSG, CS.EPSG    , 0  ) ;
      WriteString ( GIS_INI_CS_WKT,  ''         , '' ) ;
    end
    else begin
      WriteInteger( GIS_INI_CS_EPSG, 0          , 0  ) ;
      WriteString ( GIS_INI_CS_WKT,  CS.FullWKT , '' ) ;
    end;

    WriteCustomData( GIS_INI_CUSTOM, CustomData );
  end ;
end ;

procedure TGIS_Viewer.RereadConfig ;
var
  i  : Integer ;
begin
  if assigned( ProjectFile ) then
    ReadConfig ;

  for i:=(Items.Count-1) downto 0 do begin // all layers'
    TGIS_Layer( Items[i] ).RereadConfig ;
  end ;

end ;

procedure TGIS_Viewer.Add(
  const _layer : TGIS_LayerAbstract
) ;
var
  lst : TGIS_StringList ;
  tmp : String ;
  ll  : TGIS_Layer ;
begin
  if not SynchronizePaint( True) then
    exit ;
  if not assigned( _layer ) then exit ;

  ll := TGIS_Layer( _layer ) ;
  ll.ViewerReParent( FSelfRef ) ;

  if IsStringEmpty( ll.Name ) then begin
     if not IsServerPath( ll.Path ) and SafeFileExists( ll.Path ) then begin
       // for file based layers setup Name=Path if no other name exist
       tmp := GetFileNameNoExt( ll.Path ) ;
     end
     else begin
       // for SQL Layers use table names
       lst := TGIS_StringList.Create ;
       try
         ReadSQLParamsFromPath( ll.Path, lst ) ;
         tmp := lst.Values[ GIS_INI_LAYERSQL_LAYER ] ;
       finally
         FreeObject( lst ) ;
       end ;

       if IsStringEmpty( tmp ) then begin
         lst := TGIS_StringList.Create ;
         try
           ReadSQLParamsFromPath( ll.Path, lst ) ;
           tmp := lst.Values[ 'NAME' ] ;
         finally
           FreeObject( lst ) ;
         end ;
       end ;

       // for protocol based layers like ECWP:// - use file name
       if IsStringEmpty( tmp ) then begin
         tmp := GetServerName( ll.Path ) ;
         if tmp = ll.Path then begin
           // not this case - it is not protocol based format
           tmp := ''
         end ;
       end ;

       // All other cases - use generic name
       if IsStringEmpty( tmp ) then
         tmp := _rsrc( GIS_RS_GENERAL_DEFAULT_LAYERNAME ) ;

     end ;
     ll.Name := getUniqueLayerName( tmp ) ;
  end ;

  if assigned( FOnLayerAdd ) then begin
    {$IFDEF OXYGENE}
      FOnLayerAdd( self, TGIS_LayerEventArgs.Create( ll ) ) ;
    {$ELSE}
      FOnLayerAdd( self, ll ) ;
    {$ENDIF}
  end ;

  if assigned( FOnPassword ) then
    ll.PasswordEvent := FOnPassword ;

  if not ll.IsOpened then
    ll.Prepare
  else if AutoStyle then
    ll.ApplyAutoStyle ;

  if IsStringEmpty( ll.Caption ) then
    // setup ConfigName=Path if no other name exist
    ll.Caption := ll.Name ;

  if Get( ll.Name ) <> nil then
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYEREXIST ), ll.Name, 0 ) ;


  if GisMetadataAsBoolean( METADATA_OPENNONACTIVE, True ) or ll.Active
  then
    ll.Open ;

  Items.Add( ll ) ;           // add to list

  if Self.FCS = CSUnknownCoordinateSystem then begin
    if Items.Count = 1 then begin
      if Self.FCS.EPSG <> ll.CS.EPSG then begin
        Self.FCS := ll.CS ;
        Parent.ControlExtentChanged ;
      end;
    end;
  end ;

  RecalcExtent ;

  if _layer is TGIS_LayerVector then begin
    TGIS_LayerVector( _layer ).UseRTree := TGIS_LayerVector( _layer ).UseRTree ;
  end ;

  if ll.IsPersistent then
    FIsModified := True ;
end ;

procedure TGIS_Viewer.AddHierarchy ;
begin
  FHierarchy.ClearGroups ;

  FHierarchy.LoadHierarchy( ProjectFile ) ;
  FHierarchy.AddOtherLayers ;
  FHierarchy.IsModified := False ;
end ;

function TGIS_Viewer.Get(
  const _name : String
) : TGIS_LayerAbstract ;
var
  i, k  : Integer ;
  lname : String ;
  sname : String ;
begin
  Result := nil ; // not found
  if Items.Count = 0 then exit ;

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

  // all layers
  for i := 0 to (Items.Count-1) do begin
    if CompareText( TGIS_Layer( Items[i] ).Name, _name ) = 0 then begin
      // found!
      Result := TGIS_Layer( Items[i] ) ;
      exit ;
    end ;
  end ;

  // all compound layers
  for i:=0 to (Items.Count-1) do begin
    if ( CompareText( TGIS_Layer( Items[i] ).Name, lname ) = 0 ) then
    begin
      // found any!
      Result := TGIS_Layer( Items[i] ) ;
      if assigned( Result ) and IsStringEmpty( sname ) then exit ;

      if assigned( Result ) and (Result is TGIS_LayerCompoundAbstract) then begin
        // real compound layer
        if not IsStringEmpty(sname) then begin
          Result := TGIS_LayerCompoundAbstract(Result).Get( sname ) ;
          if assigned( Result ) then
            exit;
        end ;
      end
      else
      if assigned( Result ) and assigned( TGIS_Layer(Result).SubLayers ) then begin
        Result := TGIS_Layer(Result).GetSubLayer( sname ) ;
      end
      else begin
        // fake compound layer
        Result := nil ;
      end ;
    end ;
  end ;
end ;

procedure TGIS_Viewer.Delete( const _name : String );
var
  i  : Integer ;
  la : TGIS_Layer ;
begin
  if not SynchronizePaint( True) then
    exit ;

  for i := Items.Count-1 downto 0 do begin // all layers
    if CompareText( TGIS_Layer( Items[i] ).Name, _name ) = 0 then
    begin
      // found! delete it!
      la := TGIS_Layer( Items[i] ) ;

      while Items.OwnsObjects and (la.Viewer <> FSelfRef) do begin
        la.Viewer := la.Viewer ;
        //wait for Basemap object for properly released layer
        Sleep(100) ;
      end;

      if assigned( FOnLayerDelete ) then begin
        {$IFDEF OXYGENE}
          FOnLayerDelete( self, TGIS_LayerEventArgs.Create( la ) ) ;
        {$ELSE}
          FOnLayerDelete( self, la ) ;
        {$ENDIF}
      end ;
      if assigned( FEditor ) then
        FEditor.RemoveSnapLayer( la ) ;

      FHierarchy.DeleteLayer( la ) ;

      if la.IsPersistent then
        FIsModified := True ;

      {$IFNDEF DCC}
        FreeObject( la ) ;
      {$ENDIF}
      Items.Delete(i);

      if assigned( FProjectFile ) then
        FProjectFile.BuildProject( self ); //remove old references

      exit ;
    end ;
  end ;
  raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERNOEXIST ), _name, 0 ) ;
end ;

procedure TGIS_Viewer.Draw(
  const _renderer : TObject ;
  const _mode     : TGIS_DrawMode
) ;
const
  DORMANT_TRESHOLD = 250 ;
var
  i           : Integer ;
  la          : TGIS_Layer ;

  idormant    : Integer ;
  bdormant    : Boolean ;
  bbase       : Boolean ;
  ilastcached : Integer ;

  visext      : TGIS_Extent ;


  // if too many pixels layers are opened in the same extent then
  // free already rendered ones to conserve memory
  procedure dodormant( const _layer : TGIS_Layer ) ;
  var
    ii  : Integer ;
  begin
    if GisIsCommonExtent( _layer.ProjectedExtent, VisibleExtent ) then begin
      idormant := idormant + _layer.DormantGain ;
    end;

    if idormant > DORMANT_TRESHOLD then begin
      if bdormant then begin
        _layer.Dormant ;
      end
      else begin
        for ii := 0 to Items.Count -1  do begin
          if TObject( la.Viewer.Ref  ) = self then begin // not on reparented!
            TGIS_Layer( Items[ii] ).Dormant  ;
          end;
        end;
        bdormant := True ;
      end;

    end;
  end;

  function layervisible( const _layer : TGIS_Layer ) : Boolean ;
  begin
    if _layer.IsPixel then
      Result := _layer.Active
                 and
                 GisIsCommonExtent( _layer.ProjectedExtent, VisibleExtent )
    else
      Result := _layer.Active
                 and
                 GisIsCommonExtent( _layer.ProjectedExtent, visext ) ;
    try
      if not Result then
        // specially for legend
        _layer.PrepareParams ;
    except
    end ;
  end ;

begin
  if not ( _renderer is TGIS_RendererAbstract ) then exit ;

  asapInterrupted := False ;

  idormant := 0 ;
  bdormant := False ;

  // release memory for all non visible layers
  // and find last cached
  ilastcached := -1 ;
  for i := 0 to Items.Count -1  do begin
    if Interrupted then
      break;

    la := TGIS_Layer( Items[i] ) ;
    if not GisIsCommonExtent( la.ProjectedExtent, VisibleExtent ) then begin
      if not assigned( la.Viewer.Ref.MasterViewer ) then begin // not on reparented!
        la.Dormant ;
      end ;
    end ;
    if la.CachedPaint then
      ilastcached := i ;
  end;

  bbase := IncrementalPaint and ( not TiledPaint );

  if GisIsNoWorld( TemporaryVisibleExtent ) then
    visext := VisibleExtent
  else
    visext := _TGIS_Extent( TemporaryVisibleExtent ) ;

  case _mode of
    TGIS_DrawMode.All :
      begin
        for i := 0 to Items.Count -1  do begin
          la := TGIS_Layer( Items[i] ) ;

          if layervisible( la ) then begin

            if bbase and la.Basemap then
              continue
            else
              bbase := False ;

            TGIS_RendererAbstract(_renderer).LockTransparent( la.Transparency ) ;
            try
              try
                la.Renderer := _renderer ;
                la.Paint ;
              except
                on e : Exception do begin
                  if not NotifyPaintException( la.Name, e ) then
                    raise ;
                end ;
              end ;
            finally
              TGIS_RendererAbstract(_renderer).UnlockTransparent ;
            end ;
          end ;

          dodormant( la ) ;

          if HourglassShake then
            break ;
        end ;
      end;
    TGIS_DrawMode.AllExceptTop :
      begin
        // find
        for i := 0 to ilastcached do begin
          la := TGIS_Layer( Items[i] ) ;

          if bbase and la.Basemap then
            continue
          else
            bbase := False ;

          if layervisible( la ) then begin

            TGIS_RendererAbstract(_renderer).LockTransparent( la.Transparency ) ;
            try
              try
                la.Renderer := _renderer ;
                la.Paint ;
              except
                on e : Exception do begin
                  if not NotifyPaintException( la.Name, e ) then
                    raise ;
                end ;
              end ;
            finally
              TGIS_RendererAbstract(_renderer).UnlockTransparent ;
            end ;

          end ;

          dodormant( la ) ;

          if HourglassShake then
            break ;
        end ;
      end;
    TGIS_DrawMode.Top :
      begin
        for i := ilastcached + 1 to Items.Count -1  do begin
          la := TGIS_Layer( Items[i] ) ;

          if layervisible( la ) then begin

            if bbase and la.Basemap then
              continue
            else
              bbase := False ;

            TGIS_RendererAbstract(_renderer).LockTransparent( la.Transparency ) ;
            try
              try
                la.Renderer := _renderer ;
                la.Paint ;
              except
                on e : Exception do begin
                  if not NotifyPaintException( la.Name, e ) then
                    raise ;
                end ;
              end ;
            finally
              TGIS_RendererAbstract(_renderer).UnlockTransparent ;
            end ;

          end ;

          dodormant( la ) ;

          if HourglassShake then
            break ;
        end ;
      end;
    TGIS_DrawMode.OnlySelectedAll :
      begin
        for i := 0 to Items.Count -1  do begin
          la := TGIS_Layer( Items[i] ) ;

          if layervisible( la ) then begin

            if bbase and la.Basemap then
              continue
            else
              bbase := False ;

            try
              la.Renderer := _renderer ;
              la.DrawSelected ;
            except
              on e : Exception do begin
                if not NotifyPaintException( la.Name, e ) then
                  raise ;
              end ;
            end ;

          end ;

          dodormant( la ) ;

          if HourglassShake then
            break ;
         end ;
      end;
    TGIS_DrawMode.OnlySelectedAllExceptTop :
      begin
        for i := 0 to ilastcached do begin
          la := TGIS_Layer( Items[i] ) ;

          if la.InPaint then
            continue ;

          if layervisible( la ) then begin

            if bbase and la.Basemap then
              continue
            else
              bbase := False ;

            try
              la.Renderer := _renderer ;
              la.DrawSelected ;
            except
              on e : Exception do begin
                if not NotifyPaintException( la.Name, e ) then
                  raise ;
              end ;
            end ;

          end ;

          dodormant( la ) ;

          if HourglassShake then
            break ;
        end ;
      end;
    TGIS_DrawMode.OnlySelectedTop :
      begin
        for i := ilastcached + 1 to Items.Count -1  do begin
          la := TGIS_Layer( Items[i] ) ;

          if layervisible( la ) then begin

            if bbase and la.Basemap then
              continue
            else
              bbase := False ;

            try
              la.Renderer := _renderer ;
              la.DrawSelected ;
            except
              on e : Exception do begin
                if not NotifyPaintException( la.Name, e ) then
                  raise;
              end ;
            end ;

          end ;

          dodormant( la ) ;

          if HourglassShake then
            break ;
        end ;
      end;
    TGIS_DrawMode.Flash :
      begin
        for i := 0 to Items.Count -1  do begin
          la := TGIS_Layer( Items[i] ) ;

          if layervisible( la ) then begin

            if bbase and la.Basemap then
              continue
            else
              bbase := False ;

            try
              la.Renderer := _renderer ;
              la.DrawFlash ;
            except
              on e : Exception do begin
                if not NotifyPaintException( la.Name, e ) then
                  raise;
              end ;
            end ;

          end ;

          dodormant( la ) ;

          if HourglassShake then
            break ;
        end ;
      end ;
    TGIS_DrawMode.AllExcept3D :
      begin
        for i := 0 to Items.Count -1  do begin
          la := TGIS_Layer( Items[i] ) ;

          if layervisible( la ) then begin

            if bbase and la.Basemap then
              continue
            else
              bbase := False ;

            if la.View3D.Mode = TGIS_3DLayerType.Shapes
            then
              continue ;

            TGIS_RendererAbstract(_renderer).LockTransparent( la.Transparency ) ;
            try
              try
                la.Renderer := _renderer ;
                la.Paint ;
              except
                on e : Exception do begin
                  if not NotifyPaintException( la.Name, e ) then
                    raise;
                end ;
              end ;
            finally
              TGIS_RendererAbstract(_renderer).UnlockTransparent ;
            end ;

          end ;

          dodormant( la ) ;

          if HourglassShake then
            break ;
        end ;
      end ;
    else
      begin
        assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
      end;
  end;
end ;

function TGIS_Viewer.GetGrid( const _extent  : TGIS_Extent  ;
                              const _grid    : TGIS_GridArray
                            ) : Boolean ;
var
  i   : Integer ;
  r   : Boolean ;
  rc  : Integer ;
begin
  rc := 0 ;
  for i:=0 to Items.Count -1 do begin
    if Items[i] is TGIS_LayerPixel then begin
      r := TGIS_LayerPixel( Items[i] ).GetGrid( _extent, _grid ) ;
      if r then inc( rc ) ;
    end ;
  end ;
  Result := rc > 0 ;
end ;

procedure TGIS_Viewer.RevertAll ;
var
  i : Integer ;
begin
  for i:= 0 to Items.Count -1 do begin // all layers
    TGIS_Layer( Items[i] ).RevertAll ;
  end ;
end ;

procedure TGIS_Viewer.SaveProject ;
begin
  SaveProject( True ) ;
end ;

procedure TGIS_Viewer.SaveProject(
  const _relativepath : Boolean
) ;
var
  bupgrade : Boolean ;
  old_name : String  ;
begin
  if assigned( FProjectFile ) and ( not IsStringEmpty( ProjectName ) ) then
  begin
    inc( iUponSave ) ;
    try
      bupgrade := GisMetadataAsBoolean( METADATA_UPGRADEPROJECT, True )
                  and
                  ( CompareText( GetFileExt( FProjectName ), GIS_TTKGP_EXT ) = 0 )
                  and
                  ( bUpgradable ) ;

      if bupgrade then begin
        // old file format - save also a new format

        old_name := FProjectName ;
        saveProjectFile(
          GetPathNoExt( FProjectName ) + GIS_TTKPROJECT_EXT,
          _relativepath,
          True,
          True
        );

        RenameFile( old_name, old_name + '.old' ) ;
      end
      else
        saveProjectFile(
          FProjectName,
          _relativepath,
          True,
          True
        ) ;
    finally
      dec( iUponSave ) ;
    end;
    iLastHash := doGetHash( True ) ;
  end ;
end ;

procedure TGIS_Viewer.SaveProjectAs(
  const _path : String
) ;
begin
  SaveProjectAs( _path, True ) ;
end ;


procedure TGIS_Viewer.SaveProjectAs(
  const _path         : String ;
  const _relativepath : Boolean
) ;
begin
  inc( iUponSave ) ;
  try
    bUpgradable := False ;
    saveProjectFile(
      _path,
      _relativepath,
      False,
      True
    ) ;
  finally
    dec( iUponSave ) ;
  end;
end ;

procedure TGIS_Viewer.SaveProjectAsEx(
  const _configFile : TGIS_ConfigAbstract ;
  const _path       : String
) ;
begin
  SaveProjectAsEx( _configFile, _path, True ) ;
end ;

procedure TGIS_Viewer.SaveProjectAsEx(
  const _configFile    : TGIS_ConfigAbstract ;
  const _path          : String  ;
  const _relativepath  : Boolean
) ;
var
  i       : Integer         ;
  la      : TGIS_Layer      ;
  prj     : TGIS_Config     ;
  prj_tmp : TGIS_Config     ;
  lst     : TGIS_StringList ;
begin
  inc( iUponSave ) ;
  try
    prj := TGIS_Config( _configFile );
    lst := TGIS_StringList.Create ;
    try
      if assigned( FProjectFile ) then
        FProjectFile.GetStrings( lst );
      prj.SetStrings( lst )  ;
    finally
      FreeObject( lst ) ;
    end ;

    prj_tmp := FProjectFile ;
    FProjectFile := prj ;
    try
      FProjectFile.UseRelativePath := _relativepath ;
      FProjectFile.BuildProject( self ) ;

      WriteConfig ;

      for i:=0 to Items.Count -1 do begin
        la := TGIS_Layer( Items[i] ) ;
        la.WriteConfig ;
        if assigned( la.ConfigFile )                and
           TGIS_Config( la.ConfigFile ).MustSave    and
           la.UseConfig
        then
          try
            TGIS_Config( la.ConfigFile ).Save ;
          except
            // ignore INI save errors and save to ProjectFile
            la.UseConfig := False ;
            la.WriteConfig ;
            la.UseConfig := True ;
            TGIS_Config( la.ConfigFile ).ClearSave ;
          end;
      end;

      FProjectFile.WriteHierarchyGroups( self ) ;
    finally
      FProjectFile := prj_tmp ;
    end ;
  finally
    dec( iUponSave ) ;
  end;
end ;

procedure TGIS_Viewer.SaveData ;
var
  i : Integer ;
begin
  inc( iUponSave ) ;
  try
    if assigned( FEditor ) then
      FEditor.EndEdit ;

    for i:=0 to (Items.Count-1) do // all layers
      TGIS_Layer( Items[i] ).SaveData ;

    FIsModified := False ;
  finally
    dec( iUponSave ) ;
  end;
end ;

procedure TGIS_Viewer.SaveAll ;
var
  i : Integer ;
begin
  inc( iUponSave ) ;
  try
    if assigned( FEditor ) then
      FEditor.EndEdit ;

    SaveProject ;

    for i:=0 to (Items.Count-1) do // all layers
      TGIS_Layer( Items[i] ).SaveAll ;

    FIsModified := False ;
  finally
    dec( iUponSave ) ;
  end;
end ;

function TGIS_Viewer.MustSave : Boolean ;
var
  i  : Integer ;
  la : TGIS_Layer ;
begin
  if iUponSave > 0 then begin
    Result := False ;
    exit ;
  end;

  Result := FIsModified ;

  if assigned( FProjectFile ) and ( not IsStringEmpty( FProjectName ) ) then
    Result := Result or FProjectFile.MustSave or (iLastHash <> doGetHash( True )) ;

  for i:=0 to (Items.Count-1) do begin // all layers
    la := TGIS_Layer( Items[i] ) ;
    if la.IsPersistent then begin
      if TGIS_Layer( Items[i] ).MustSave then begin
        Result := True ;
        break ;
      end ;
    end ;
  end ;
end ;

procedure TGIS_Viewer.MarkModified ;
begin
  FIsModified := True ;
end ;

procedure TGIS_Viewer.RecalcExtent ;
var
  i     : Integer     ;
  la    : TGIS_Layer  ;
  oldex : TGIS_Extent ;
  cnt1  : TGIS_Point  ;
  cnt2  : TGIS_Point  ;
begin
  if GisIsNoWorld( Extent ) then
    cnt1 := GisPoint( 0, 0 )
  else
    cnt1 := ScreenToMap( Point( 0, 0 ) ) ;

  oldex := _TGIS_Extent(FExtent) ;
  FExtent := GisNoWorld ;

  for i := 0 to Items.Count - 1  do begin
    la := TGIS_Layer( Items[i] ) ;

    la.RecalcProjectedExtent ;

    if GisIsNoWorld( la.ProjectedExtent ) then
      continue ;

    FExtent := GisMaxExtent( Extent, la.ProjectedExtent ) ;
  end ;

  if GisIsNoWorld( FExtent ) then begin
    // repeat but this time with baselayer

      for i := 0 to Items.Count - 1  do begin
        la := TGIS_Layer( Items[i] ) ;

      if GisIsNoWorld( la.ProjectedExtent ) then
        continue ;

      FExtent := GisMaxExtent( Extent, la.ProjectedExtent ) ;
    end;
  end;

  FExtent := GisCommonExtent( FRestrictedExtent, Extent ) ;

  if ( FExtent.XMin < -1e30 ) or
     ( FExtent.YMin < -1e30 ) or
     ( FExtent.XMin >  1e30 ) or
     ( FExtent.YMax >  1e30 )
  then
    FExtent := GisNoWorld ;

  if not GisIsSameExtent( FExtent, oldex ) then begin
    if not GisIsNoWorld( oldex ) then begin
      FViewport.X := Viewport.X + ( Extent.XMin - oldex.XMin  ) ;
      FViewport.Y := Viewport.Y + ( oldex.YMax  - Extent.YMax ) ;
    end
    else begin
      FViewport.X :=  Extent.XMin ;
      FViewport.Y := -Extent.YMax ;
    end ;
    Parent.ControlExtentChanged;

    if assigned( FOnExtentChange ) then begin
      {$IFDEF CLR}
        FOnExtentChange( Self, EventArgs.Create ) ;
      {$ELSE}
        FOnExtentChange( Self ) ;
      {$ENDIF}
    end ;
  end ;

  if not GisIsNoWorld( Extent ) then begin
    cnt2 := ScreenToMap( Point( 0, 0 ) ) ;
    FViewport.X := cnt1.X - cnt2.X + Viewport.X ;
    FViewport.Y := cnt2.Y - cnt1.Y + Viewport.Y ;
  end ;
end ;

procedure TGIS_Viewer.Reposition ;
var
  oldscale : Double ;
  ptcnt  : TGIS_Point ;
  deltas : TGIS_Point ;
begin
  if Zoom <= 0 then exit ;
  oldscale := FScale ;

  // is anything valid?
  if GisIsNoWorld( Extent ) then exit ;

  // calculate new center of screen
  ptcnt := ScreenToMapEx( GisPoint( 1.0 * oParent.ControlCanvasWidth  / 2,
                                    1.0 * oParent.ControlCanvasHeight / 2
                                  )
                        ) ;

  // use deltas to center the screen

  {$IFDEF GIS_NORECORDS}
    deltas := new TGIS_Point ;
  {$ENDIF}
  deltas.X :=  ( CenterPtg.X - ptcnt.X ) ;
  deltas.Y := -( CenterPtg.Y - ptcnt.Y ) ;

  if (deltas.X<>0) or (deltas.Y<>0) then begin
    //ptcnt := Viewport ;

    {$IFDEF JAVA}
      var dx :  java.lang.Double := deltas.X;
      var dy :  java.lang.Double := deltas.Y;
      MoveViewportEx( dx, dy ) ;
      deltas.X  := dx ;
      deltas.Y  := dy ;
    {$ELSE}
      MoveViewportEx( deltas.X , deltas.Y  ) ;
    {$ENDIF}
    FScale := ScaleAsFloat ;
    if ( deltas.X <> 0 ) or ( deltas.Y <> 0 ) then
      if FLocked.Level = 0 then
        oParent.ControlUpdateWholeMap ;

    if GisIsNoWorld( FTemporaryVisibleExtent ) then
      FLabelsReg.Reset ;

    if not bBlockRepositionEvents then begin
      if assigned( FOnZoomChange ) and ( Abs( lastPaint.Zoom/Zoom-1 ) > 1E-5 ) then
      begin
        {$IFDEF CLR}
          FOnZoomChange( Self, EventArgs.Create ) ;
        {$ELSE}
          FOnZoomChange( self ) ;
        {$ENDIF}
      end ;

      if assigned( FOnVisibleExtentChange ) and
         ( not GisIsSameExtent( lastPaint.VisibleExtent, VisibleExtent ) ) then
      begin
        {$IFDEF CLR}
          FOnVisibleExtentChange( Self, EventArgs.Create ) ;
        {$ELSE}
          FOnVisibleExtentChange( Self ) ;
        {$ENDIF}
      end ;

      lastPaint.Zoom          := Zoom ;
      lastPaint.VisibleExtent := _TGIS_Extent(VisibleExtent)  ;
    end;
  end
  else begin
    FScale := ScaleAsFloat ;
    if FScale <> oldscale then
      setDirectPaint ;
  end;
end ;

procedure TGIS_Viewer.InvalidateExtent(
  const _extent  : TGIS_Extent
) ;
begin
  InvalidateExtent( _extent, True ) ;
end ;

procedure TGIS_Viewer.InvalidateExtent(
  const _extent  : TGIS_Extent ;
  const _deep    : Boolean
) ;
begin
  if UponDestroy then
    exit ;

  if IsLocked then
    notifyLocked( Extent, True )
  else if _deep then
    Parent.ControlUpdateWholeMap
  else
    Parent.ControlRepaint ;
end ;

procedure TGIS_Viewer.InvalidateWholeMap ;
begin
  if UponDestroy then
    exit ;

  if FLocked.Level > 0 then
    FLocked.WholeMap := True
  else
    Parent.ControlUpdateWholeMap ;
end;

procedure TGIS_Viewer.InvalidateTopmost ;
begin
  if UponDestroy then
    exit ;

  if FLocked.Level > 0 then
    FLocked.Topmost := True
  else
    Parent.ControlUpdateTopmost ;
end;

procedure TGIS_Viewer.InvalidateBasemap ;
begin
  if UponDestroy then
    exit ;

  Parent.ControlUpdateBasemap ;
end;

procedure TGIS_Viewer.InvalidateSelection ;
begin
  if UponDestroy then
    exit ;

  if FLocked.Level > 0 then
    FLocked.Selection := True
  else
    Parent.ControlUpdateSelection ;
end;

procedure TGIS_Viewer.InvalidateEditor(
  const _final : Boolean
) ;
begin
  if UponDestroy then
    exit ;

  if FLocked.Level > 0 then
    FLocked.Editor := True
  else
    Parent.ControlUpdateEditor( _final ) ;
end;

function TGIS_Viewer.FullExtentZoom : Double ;
const
  eps = 1e-15 ;
var
  zm1, zm2 : Double  ;
  w,h      : Double  ;
  cw,ch    : Integer ;
begin
  if GisIsNoWorld( Extent ) then begin
    Result := 1 ;
    exit ;
  end ;

  w := Max( Abs( Extent.XMax - Extent.XMin ), eps ) ;
  h := Max( Abs( Extent.YMax - Extent.YMin ), eps ) ;

  cw := oParent.ControlCanvasWidth  ;
  ch := oParent.ControlCanvasHeight ;

  if BigExtentMargin > 0 then begin
    w := w + Abs( BigExtentMargin ) / 100 * w ;
    h := h + Abs( BigExtentMargin ) / 100 * h ;
  end
  else if BigExtentMargin < 0 then begin
    cw := RoundS( cw - 1.0 * Abs( BigExtentMargin ) / 100 * cw ) ;
    ch := RoundS( ch - 1.0 * Abs( BigExtentMargin ) / 100 * ch ) ;
  end ;

  if ( Items.Count > 0 ) and
     ( cw          > 0 ) and
     ( ch          > 0 ) and
     ( w           > 0 ) and
     ( h           > 0 ) then
  begin
    zm1 := cw / w ;
    zm2 := ch / h ;
    if zm1 < zm2 then // fit by width or height ?
      Result := zm1
    else
      Result := zm2 ;
  end
  else
    Result := 1 ;
end ;

procedure TGIS_Viewer.FullExtent ;
begin
  if not SynchronizePaint( True) then
    exit ;

  if Items.Count = 0 then exit ;

  FViewport.X := 0 ;
  FViewport.Y := 0 ;

  FCenter := GisCenterPoint( Extent ) ;
  Zoom := FullExtentZoom ;
end ;

function TGIS_Viewer.Locate( const _ptg     : TGIS_Point ;
                             const _prec    : Double
                           ) : TGIS_ShapeAbstract ;
begin
  Result := Locate( _ptg, _prec, True ) ;
end ;

function TGIS_Viewer.Locate( const _ptg     : TGIS_Point ;
                             const _prec    : Double     ;
                             const _visible : Boolean
                           ) : TGIS_ShapeAbstract ;
var
  i         : Integer    ;
  tmp_dist  : Double     ;          // temporary values
  tmp_distp : Double     ;          // temporary values
  tmp_part  : Integer    ;          // temporary values
  tmp_shape : TGIS_Shape ;
  shape     : TGIS_Shape ;          // found values
  dist      : Double     ;
  proj      : TGIS_Point ;
  ll        : TGIS_LayerVector  ;
  lp        : TGIS_LayerProject ;
  lc        : TGIS_LayerCompoundVector ;
begin
  shape := nil ;            // assume we find nothing
  dist  := GIS_MAX_DOUBLE ; // assume really huge starting distance
  tmp_shape := nil ;
  Result := nil ;

  if InPaint then
    exit ;

  for i:= Items.Count -1 downto 0 do begin // all layers
    if TGIS_Layer( Items[i] ).Active then begin // but only visible
      try
        if ( Items[i] is TGIS_LayerVector ) then begin

          ll := Items[i] as TGIS_LayerVector ;

          try
            tmp_shape := ll.LocateEx( _ptg, _prec, -1,
                                      tmp_dist, tmp_part, proj, tmp_distp,
                                      _visible
                                    ) ;
          except
            tmp_dist := GIS_MAX_DOUBLE ;
            // do nothing
          end ;
        end
        else if ( Items[i] is TGIS_LayerProject ) then begin
          lp := Items[i] as TGIS_LayerProject ;
          tmp_shape := lp.LocateEx( _ptg, _prec, -1,
                                    tmp_dist, tmp_part, proj, tmp_distp,
                                    _visible
                                  ) ;
        end
        else if ( Items[i] is TGIS_LayerCompoundVector ) then begin
          lc := Items[i] as TGIS_LayerCompoundVector ;
          tmp_shape := lc.LocateEx( _ptg, _prec, -1,
                                    tmp_dist, tmp_part, proj, tmp_distp,
                                    _visible
                                  ) ;
        end
        else
          tmp_shape := nil ;

        if tmp_shape <> nil then begin
          if tmp_distp < dist then begin
            // is closer then previous (or lay out on top)
            shape := tmp_shape ;
            dist := tmp_distp ;
          end ;
        end ;
      except
      end ;
    end ;
  end ;
  Result := shape;
end ;

function TGIS_Viewer.Locate(
  const _pt   : TPoint     ;
  const _prec : Integer
) : TGIS_ShapeAbstract ;
begin
  Result := Locate( ScreenToMap( _pt ), _prec / Zoom ) ;
end;

function TGIS_Viewer.LocateEx(
  const _ptg     : TGIS_Point ;
  const _prec    : Double     ;
  const _visible : Boolean
) : TGIS_ShapeAbstractList ;
var
  i         : Integer    ;
  tmp_dist  : Double     ;          // temporary values
  tmp_part  : Integer    ;          // temporary values
  tmp_shape : TGIS_Shape ;
  shape     : TGIS_Shape ;          // found values
  dist      : Double     ;
  proj      : TGIS_Point ;
  ll        : TGIS_LayerVector  ;
  lp        : TGIS_LayerProject ;
  lc        : TGIS_LayerCompoundVector ;
begin
  Result := TGIS_ShapeAbstractList.Create( False ) ;

  shape := nil ;            // assume we find nothing
  dist  := GIS_MAX_DOUBLE ; // assume really huge starting distance
  tmp_shape := nil ;

  if InPaint then
    exit ;

  for i:= Items.Count -1 downto 0 do begin // all layers
    if TGIS_Layer( Items[i] ).Active then begin // but only visible
      try
        if ( Items[i] is TGIS_LayerVector ) then begin

          ll := Items[i] as TGIS_LayerVector ;

          try
            tmp_shape := ll.LocateEx( _ptg, _prec, -1,
                                      tmp_dist, tmp_part, proj,
                                      _visible
                                    ) ;
          except
            tmp_dist := GIS_MAX_DOUBLE ;
            // do nothing
          end ;
        end
        else if ( Items[i] is TGIS_LayerProject ) then begin
          lp := Items[i] as TGIS_LayerProject ;
          tmp_shape := lp.LocateEx( _ptg, _prec, -1,
                                    tmp_dist, tmp_part, proj,
                                    _visible
                                  ) ;
        end
        else if ( Items[i] is TGIS_LayerCompoundVector ) then begin
          lc := Items[i] as TGIS_LayerCompoundVector ;
          tmp_shape := lc.LocateEx( _ptg, _prec, -1,
                                    tmp_dist, tmp_part, proj,
                                    _visible
                                  ) ;
        end
        else
          tmp_shape := nil ;

        if tmp_shape <> nil then begin
          shape := tmp_shape ;
          Result.Add( shape ) ;
          dist := tmp_dist ;
        end ;
      except
      end ;
    end ;
  end ;
end ;

function TGIS_Viewer.MapToScreen(
  const _ptg : TGIS_Point
) : TPoint ;
begin
  if IsEmpty or ( FZoom <= 0 ) then begin
    Result := Point( 0, 0 ) ;
    exit ;
  end ;

  assert( not GisIsNoWorld( Extent ) ) ;
  {$IFDEF GIS_NORECORDS}
    Result := new TPoint(0,0) ;
  {$ENDIF}
  Result.X := TruncS( ( _ptg.X - FExtent.XMin - FViewport.X ) * FZoom +0.5) ;
  Result.Y := TruncS( ( FExtent.YMax - _ptg.Y - FViewport.Y ) * FZoom +0.5) ;
end ;

{$IFDEF CLR}
  {$IFNDEF MONO}
    function TGIS_Viewer.MapToScreenF(
      const _ptg : TGIS_Point
      ) : TPointF ;
    begin
      if IsEmpty or ( FZoom <= 0 ) then begin
        Result := new Point( 0, 0 ) ;
        exit ;
      end ;

      assert( not GisIsNoWorld( Extent ) ) ;

      Result := PointF(
                  ( _ptg.X - FExtent.XMin - FViewport.X ) * FZoom,
                  ( FExtent.YMax - _ptg.Y - FViewport.Y ) * FZoom
                ) ;
    end ;
  {$ENDIF}
{$ENDIF}

{$IFDEF CLR}
  function TGIS_Viewer.MapToScreen3D(
    const _ptg : TGIS_Point3D
  ) : TPoint ;
{$ELSE}

  function TGIS_Viewer.MapToScreen3D( const _ptg : TGIS_Point3D ) : TPoint ;
{$ENDIF}
begin
  Result := MapToScreen( GisPoint2DFrom3D( _ptg ) ) ;
end ;

{$IFDEF CLR}
  {$IFNDEF MONO}
    function TGIS_Viewer.MapToScreenF3D(
      const _ptg : TGIS_Point3D
    ) : TPointF ;
    begin
      Result := MapToScreenF( GisPoint2DFrom3D( _ptg ) ) ;
    end ;
  {$ENDIF}
{$ENDIF}

function TGIS_Viewer.ScreenToMap( const _pt : TPoint ) : TGIS_Point ;
begin
  if IsEmpty or ( FZoom <= 0 ) then begin
    Result := GisPoint(0, 0 ) ;
    exit ;
  end ;

  assert( not GisIsNoWorld( Extent ) ) ;

  {$IFDEF GIS_NORECORDS}
    Result := new TGIS_Point ;
  {$ENDIF}
  Result.X := FExtent.XMin + ( _pt.X / FZoom + FViewport.X ) ;
  Result.Y := FExtent.YMax - ( _pt.Y / FZoom + FViewport.Y ) ;
end ;

function TGIS_Viewer.ScreenToMap3D(
  const _pt : TPoint
) : TGIS_Point3D ;
begin
  if IsEmpty or ( FZoom <= 0 ) then begin
    Result := GisPoint3D(0, 0, 0, 0 ) ;
    exit ;
  end ;

  assert( not GisIsNoWorld( Extent ) ) ;
  {$IFDEF GIS_NORECORDS}
    Result := new TGIS_Point3D ;
  {$ENDIF}
  Result.X := FExtent.XMin + ( _pt.X / FZoom + FViewport.X ) ;
  Result.Y := FExtent.YMax - ( _pt.Y / FZoom + FViewport.Y ) ;
  Result.Z := 0 ;
  Result.M := 0 ;
end ;

function TGIS_Viewer.MapToScreenEx( const _pt : TGIS_Point ) : TGIS_Point ;
begin
  if IsEmpty or ( FZoom <= 0 ) then begin
    Result := GisPoint(0, 0 ) ;
    exit ;
  end ;

  assert( not GisIsNoWorld( Extent ) ) ;

  {$IFDEF GIS_NORECORDS}
    Result := new TGIS_Point ;
  {$ENDIF}
  Result.X := ( _pt.X - FExtent.XMin - FViewport.X ) * FZoom ;
  Result.Y := ( FExtent.YMax - _pt.Y - FViewport.Y ) * FZoom ;
end ;

function TGIS_Viewer.ScreenToMapEx( const _pt : TGIS_Point ) : TGIS_Point ;
begin
  if IsEmpty or ( FZoom <= 0 ) then begin
    Result := GisPoint(0, 0 ) ;
    exit ;
  end ;

  assert( not GisIsNoWorld( Extent ) ) ;

  {$IFDEF GIS_NORECORDS}
    Result := new TGIS_Point ;
  {$ENDIF}
  Result.X := FExtent.XMin + ( _pt.X / FZoom + FViewport.X ) ;
  Result.Y := FExtent.YMax - ( _pt.Y / FZoom + FViewport.Y ) ;
end ;

function TGIS_Viewer.MapToScreenRect ( const _rct : TGIS_Extent ) : TRect ;
begin
  if IsEmpty or ( FZoom <= 0 ) then begin
    Result := Rect( 0, 0, 0, 0 ) ;
    exit ;
  end ;

  assert( not GisIsNoWorld( Extent ), 'Empty viewer' ) ;

  Result := Rect(
              RoundS( ( _rct.XMin - FExtent.XMin - FViewport.X ) * FZoom ),
              RoundS( ( FExtent.YMax - _rct.YMax - FViewport.Y ) * FZoom ),
              RoundS( ( _rct.XMax - FExtent.XMin - FViewport.X ) * FZoom ),
              RoundS( ( FExtent.YMax - _rct.YMin - FViewport.Y ) * FZoom )
            ) ;
end ;

{$IFDEF CLR}
  {$IFNDEF MONO}
    function TGIS_Viewer.MapToScreenRectF( const _rct : TGIS_Extent ) : TRectF  ;
    begin
      if IsEmpty or ( FZoom <= 0 ) then begin
        Result := RectF( 0, 0, 0, 0 ) ;
        exit ;
      end ;

      assert( not GisIsNoWorld( Extent ), 'Empty viewer' ) ;

      Result := RectF(
                  ( _rct.XMin - FExtent.XMin - FViewport.X ) * FZoom,
                  ( FExtent.YMax - _rct.YMax - FViewport.Y ) * FZoom,
                  ( _rct.XMax - _rct.XMin ) * FZoom,
                  ( _rct.YMax - _rct.YMin ) * FZoom
                ) ;
    end ;
  {$ENDIF}
{$ENDIF}

function TGIS_Viewer.ScreenToMapRect( const _rct : TRect ) : TGIS_Extent ;
begin
  if IsEmpty or ( FZoom <= 0 ) then begin
    Result := GisExtent(0, 0, 0, 0 ) ;
    exit ;
  end ;

  assert( not GisIsNoWorld( Extent ), 'Empty viewer' ) ;

  {$IFDEF GIS_NORECORDS}
    Result := new TGIS_Extent ;
  {$ENDIF}
  Result.XMin := FExtent.XMin + ( _rct.Left   / FZoom + FViewport.X ) ;
  Result.XMax := FExtent.XMin + ( _rct.Right  / FZoom + FViewport.X ) ;
  Result.YMin := FExtent.YMax - ( _rct.Bottom / FZoom + FViewport.Y ) ;
  Result.YMax := FExtent.YMax - ( _rct.Top    / FZoom + FViewport.Y ) ;
end ;

function TGIS_Viewer.ScreenToMapRect(
  const _left, _top, _right, _bottom  : Integer
) : TGIS_Extent ;
begin
  if IsEmpty or ( FZoom <= 0 ) then begin
    Result := GisExtent(0, 0, 0, 0 ) ;
    exit ;
  end ;

  assert( not GisIsNoWorld( Extent ), 'Empty viewer' ) ;

  {$IFDEF GIS_NORECORDS}
    Result := new TGIS_Extent ;
  {$ENDIF}
  Result.XMin := FExtent.XMin + ( _left   / FZoom + FViewport.X ) ;
  Result.XMax := FExtent.XMin + ( _right  / FZoom + FViewport.X ) ;
  Result.YMin := FExtent.YMax - ( _bottom / FZoom + FViewport.Y ) ;
  Result.YMax := FExtent.YMax - ( _top    / FZoom + FViewport.Y ) ;
end ;

function TGIS_Viewer.PixelsToTwips( const _size : Integer ) : Integer ;
begin
  Result := RoundS( 1.0 * Abs( _size ) * 1440 / PPI ) ;
end ;

function TGIS_Viewer.TwipsToPixels( const _size : Integer ) : Integer ;
var
  tmp   : Double  ;
begin
  if      _size < 0 then begin
                           if _size <= -GIS_AUTOSIZE_SIZE_MU then begin
                              // special case for value expressed in map units
                              tmp := 1.0
                                     * ( -_size mod GIS_AUTOSIZE_SIZE_MU )
                                     * FZoom ;
                              Result := RoundS( tmp / 100 ) ;
                           end
                           else
                           if _size <= -GIS_AUTOSIZE_SIZE then begin
                              tmp := 1.0
                                     * ( -_size mod GIS_AUTOSIZE_SIZE )
                                     * FScale * PPI ;
                              Result := RoundS( tmp /1440 * 56.692913386 ) ;
                           end
                           else
                             Result := -_size ; // minus so real pixels
                           if Result > 4096 then
                             Result := 4098 ;
                         end
  else if _size > 0 then begin
                           if _size >= GIS_AUTOSIZE_SIZE_MU then begin
                              // undefined - treat same as GIS_AUTOSIZE_SIZE
                              tmp := 1.0
                                     * ( _size mod GIS_AUTOSIZE_SIZE_MU )
                                     * FScale * PPI ;
                              Result := RoundS( tmp /1440 ) ;
                           end
                           else
                           if _size >= GIS_AUTOSIZE_SIZE then begin
                              tmp := 1.0 * ( _size mod GIS_AUTOSIZE_SIZE ) *
                                     FScale * PPI ;
                              Result := RoundS( tmp /1440 ) ;
                           end
                           else begin
                              tmp := 1.0 * _size * PPI ;
                              if tmp > 1440 then Result := RoundS( tmp /1440 )
                                            else Result := 1 ;
                           end;
                           if Result > 4096 then
                             Result := 4096 ;
                         end
  else                   Result := 0 ;
end ;

function TGIS_Viewer.TwipsToPoints( const _size : Integer ) : Integer ;
var
  tmp   : Double  ;
begin
  if       _size = GIS_RENDER_SIZE then
                           Result := GIS_RENDER_SIZE
  else if _size < 0 then begin
                           if _size <= -GIS_AUTOSIZE_SIZE_MU then begin
                              // special case for value expressed in map units
                              tmp := 1.0
                                     * ( -_size mod GIS_AUTOSIZE_SIZE_MU )
                                     * FZoom ;
                              Result := RoundS( tmp / 100 ) ;
                           end
                           else
                           if _size <= -GIS_AUTOSIZE_SIZE then begin
                              tmp := 1.0
                                     * ( -_size mod GIS_AUTOSIZE_SIZE )
                                     * FScale ;
                              Result := RoundS( tmp * 2.834645669291 ) ;
                           end
                           else
                             Result := RoundS( -_size * ( 1440 / PPI ) / 20
                                             ) ; // minus so real pixels
                           if Result > 2048 then
                             Result := 2048 ;
                         end
  else if _size > 0 then begin
                           if _size >= GIS_AUTOSIZE_SIZE_MU then begin
                              // undefined - treat same as GIS_AUTOSIZE_MARKER
                              tmp := 1.0
                                     * ( _size mod GIS_AUTOSIZE_SIZE_MU )
                                     * FScale ;
                              Result := RoundS( tmp / 20 ) ;
                           end
                           else
                           if _size >= GIS_AUTOSIZE_SIZE then begin
                              tmp := 1.0
                                     * ( _size mod GIS_AUTOSIZE_SIZE )
                                     * FScale ;
                              Result := RoundS( tmp / 20 ) ;
                           end
                           else begin
                              tmp := 1.0 * _size ;
                              Result := RoundS( tmp / 20 ) ;
                           end;
                           if Result > 2048 then
                             Result := 2048 ;
                         end
  else                   Result := 0 ;
end ;

procedure TGIS_Viewer.MoveViewport( var _dx, _dy : Integer ) ;
var
  exw , exh   : Double  ;
  bexw, bexh  : Double  ;
  dcw , dch   : Double  ;
  minviewport : TGIS_Point ;
  maxviewport : TGIS_Point ;
  bext        : TGIS_Extent ;
begin
  assert( Zoom > 0, 'Zoom must be > 0' ) ;

  dcw  := oParent.ControlCanvasWidth  * FZoom ;
  dch  := oParent.ControlCanvasHeight * FZoom ;

  exw := Abs( Extent.XMax - Extent.XMin) ;
  exh := Abs( Extent.YMax - Extent.YMin) ;

  bext := BigExtent ;
  bexw := Abs( bext.XMax - bext.XMin) ;
  bexh := Abs( bext.YMax - bext.YMin) ;

  // normalize movement to avoid moving outside window
  if RestrictedDrag then begin
    {$IFDEF GIS_NORECORDS}
      minviewport := new TGIS_Point ;
      maxviewport := new TGIS_Point ;
    {$ENDIF}

    // drag if not fully visible
    if bexw > dcw then begin
      minviewport.X := ( bext.XMin - Extent.XMin ) ;
      maxviewport.X := ( bext.XMax - Extent.XMin ) - dcw ;

      if      ( FViewport.X + _dx/Zoom ) < minviewport.X then
              _dx := RoundS( ( minviewport.X - FViewport.X ) * Zoom )
      else if ( FViewport.X + _dx/Zoom ) > maxviewport.X then
              _dx := RoundS( ( maxviewport.X - FViewport.X ) * Zoom ) ;
    end
    else begin
      minviewport.X := - ( dcw - exw ) / 2 ;
      _dx := RoundS( ( minviewport.X - FViewport.X ) * Zoom ) ;
    end ;

    // drag if not fully visible
    if bexh > dch then begin
      minviewport.Y := ( bext.YMin - Extent.YMin ) ; ;
      maxviewport.Y := ( bext.YMax - Extent.YMin ) - dch ;
      if      ( FViewport.Y + _dy/Zoom ) < minviewport.Y
              then _dy := RoundS( ( minviewport.Y - FViewport.Y ) * Zoom )
      else if ( FViewport.Y + _dy/Zoom ) > maxviewport.Y
              then _dy := RoundS( ( maxviewport.Y - FViewport.Y ) * Zoom ) ;
    end
    else begin
      minviewport.Y := - ( dch - exh ) / 2 ;
      _dy := RoundS( ( minviewport.Y - FViewport.Y ) * Zoom ) ;
    end ;
  end ;

  // move by normalized factors
  FViewport.X := FViewport.X + _dx / Zoom ;
  FViewport.Y := FViewport.Y + _dy / Zoom ;

  setDirectPaint ;

end ;

{$IFDEF JAVA}
  procedure TGIS_Viewer.MoveViewportEx( var _dx, _dy : java.lang.Double ) ;
  var
    exw , exh   : java.lang.Double  ;
    bexw, bexh  : java.lang.Double  ;
    dcw , dch   : java.lang.Double  ;
    minviewport : TGIS_Point        ;
    maxviewport : TGIS_Point        ;
    bext        : TGIS_Extent       ;
{$ELSE}
  procedure TGIS_Viewer.MoveViewportEx( var _dx, _dy : Double ) ;
  var
    exw , exh   : Double            ;
    bexw, bexh  : Double            ;
    dcw , dch   : Double            ;
    minviewport : TGIS_Point        ;
    maxviewport : TGIS_Point        ;
    bext        : TGIS_Extent       ;
{$ENDIF}
begin
  assert( Zoom > 0, 'Zoom must be > 0' ) ;

  dcw  := oParent.ControlCanvasWidth  / FZoom ;
  dch  := oParent.ControlCanvasHeight / FZoom ;

  exw := Abs( Extent.XMax - Extent.XMin) ;
  exh := Abs( Extent.YMax - Extent.YMin) ;

  bext := BigExtent ;
  bexw := Abs( bext.XMax - bext.XMin) ;
  bexh := Abs( bext.YMax - bext.YMin) ;

  {$IFDEF GIS_NORECORDS}
    minviewport := new TGIS_Point ;
    maxviewport := new TGIS_Point ;
  {$ENDIF}

  // normalize movement to avoid moving outside window
  if RestrictedDrag then begin

    // drag if not fully visible
    if bexw > dcw then begin
      minviewport.X := ( bext.XMin - Extent.XMin ) ;
      maxviewport.X := ( bext.XMax - Extent.XMin ) - dcw ;

      if      ( FViewport.X + _dx ) < minviewport.X then
              _dx := minviewport.X - FViewport.X
      else if ( FViewport.X + _dx ) > maxviewport.X then
              _dx := maxviewport.X - FViewport.X ;
    end
    else begin
      minviewport.X := - ( dcw - exw ) / 2 ;
      _dx := minviewport.X - FViewport.X;
    end ;

    // drag if not fully visible
    if bexh > dch then begin
      minviewport.Y := ( bext.YMin - Extent.YMin ) ; ;
      maxviewport.Y := ( bext.YMax - Extent.YMin ) - dch ;
      if      ( FViewport.Y + _dy ) < minviewport.Y
              then _dy := minviewport.Y - FViewport.Y
      else if ( FViewport.Y + _dy ) > maxviewport.Y
              then _dy := maxviewport.Y - FViewport.Y ;
    end
    else begin
      minviewport.Y := - ( dch - exh ) / 2 ;
      _dy := minviewport.Y - FViewport.Y ;
    end ;
  end ;

  // move by normalized factors
  FViewport.X := FViewport.X + _dx ;
  FViewport.Y := FViewport.Y + _dy ;

  if ( _dx <> 0 ) or ( _dy <> 0 ) then
    setDirectPaint ;

end ;

{$IFDEF JAVA}
  procedure TGIS_Viewer.SetViewport(
    var _x, _y : java.lang.Double
  ) ;
{$ELSE}
  procedure TGIS_Viewer.SetViewport(
    var _x, _y : Double
  ) ;
{$ENDIF}
begin
  if ( FViewport.X <> _x ) or ( FViewport.Y <> _y ) then begin
    FViewport.X := _x ;
    FViewport.Y := _y ;

    setDirectPaint ;
  end ;
end ;

procedure TGIS_Viewer.CenterViewport( const _ptg : TGIS_Point ) ;
begin
  CenterPtg := _TGIS_Point(_ptg) ;
  Reposition ;
end ;

procedure TGIS_Viewer.SetCSByWKT(
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

procedure TGIS_Viewer.SetCSByEPSG(
  const _epsg : Integer
) ;
begin
  if _epsg <= 0 then
    self.CS := nil
  else
    self.CS := TGIS_CSFactory.ByEPSG( _epsg ) ;
end ;

procedure TGIS_Viewer.SetCSByWKTFile(
  const _path : String
) ;
var
  lst : TGIS_StringList ;
begin
  lst := TGIS_StringList.Create ;
  try
    if not SafeFileExists( _path ) then
      self.CS := nil
    else begin
      lst.LoadFromFile( _path ) ;
      SetCSByWKT( lst.Text ) ;
    end ;
  finally
    FreeObject( lst ) ;
  end ;
end ;

function TGIS_Viewer.RotatedPoint(
  const _ptg : TGIS_Point
) : TGIS_Point  ;
begin
  Result := rotatePtg( _ptg ) ;
end;

function TGIS_Viewer.UnrotatedPoint(
  const _ptg : TGIS_Point
) : TGIS_Point  ;
begin
  Result := unrotatePtg( _ptg ) ;
end;

function TGIS_Viewer.RotatedPoint3D(
  const _ptg : TGIS_Point3D
) : TGIS_Point3D  ;
begin
  Result := rotatePtg3D( _ptg ) ;
end;

procedure TGIS_Viewer.RotatedPoint3D_ref(
  {$IFNDEF JAVA} var {$ENDIF}  _ptg : TGIS_Point3D
) ;
begin
  rotatePtg3D_ref( _ptg ) ;
end;

function TGIS_Viewer.UnrotatedPoint3D(
  const _ptg : TGIS_Point3D
) : TGIS_Point3D ;
begin
  Result := unrotatePtg3D( _ptg ) ;
end;

procedure TGIS_Viewer.UnrotatedPoint3D_ref(
  {$IFNDEF JAVA} var {$ENDIF}  _ptg : TGIS_Point3D
) ;
begin
  unrotatePtg3D_ref( _ptg ) ;
end;

function TGIS_Viewer.RotatedExtent(
  const _extent : TGIS_Extent
) : TGIS_Extent ;
var
  ptg1, ptg2, ptg3, ptg4 : TGIS_Point ;
begin
  ptg1 := rotatePtg( GisPoint( _extent.XMin, _extent.YMin ) ) ;
  ptg2 := rotatePtg( GisPoint( _extent.XMin, _extent.YMax ) ) ;
  ptg3 := rotatePtg( GisPoint( _extent.XMax, _extent.YMax ) ) ;
  ptg4 := rotatePtg( GisPoint( _extent.XMax, _extent.YMin ) ) ;

  {$IFDEF GIS_NORECORDS}
    Result := new TGIS_Extent ;
  {$ENDIF}
  Result.XMin := Min( Min( Min( ptg1.X, ptg2.X ), ptg3.X ), ptg4.X ) ;
  Result.YMin := Min( Min( Min( ptg1.Y, ptg2.Y ), ptg3.Y ), ptg4.Y ) ;
  Result.XMax := Max( Max( Max( ptg1.X, ptg2.X ), ptg3.X ), ptg4.X ) ;
  Result.YMax := Max( Max( Max( ptg1.Y, ptg2.Y ), ptg3.Y ), ptg4.Y ) ;
end;

function TGIS_Viewer.UnrotatedExtent(
  const _extent : TGIS_Extent
) : TGIS_Extent ;
var
  ptg1, ptg2, ptg3, ptg4 : TGIS_Point ;
begin
  ptg1 := unrotatePtg( GisPoint( _extent.XMin, _extent.YMin ) ) ;
  ptg2 := unrotatePtg( GisPoint( _extent.XMin, _extent.YMax ) ) ;
  ptg3 := unrotatePtg( GisPoint( _extent.XMax, _extent.YMax ) ) ;
  ptg4 := unrotatePtg( GisPoint( _extent.XMax, _extent.YMin ) ) ;

  {$IFDEF GIS_NORECORDS}
    Result := new TGIS_Extent ;
  {$ENDIF}
  Result.XMin := Min( Min( Min( ptg1.X, ptg2.X ), ptg3.X ), ptg4.X ) ;
  Result.YMin := Min( Min( Min( ptg1.Y, ptg2.Y ), ptg3.Y ), ptg4.Y ) ;
  Result.XMax := Max( Max( Max( ptg1.X, ptg2.X ), ptg3.X ), ptg4.X ) ;
  Result.YMax := Max( Max( Max( ptg1.Y, ptg2.Y ), ptg3.Y ), ptg4.Y ) ;
end;


function TGIS_Viewer.GetRenderContext
  : TObject ;
begin
  Result := Parent.ControlRenderer ;
end ;

procedure TGIS_Viewer.WaitForBackgroundProcesses ;
begin
  // do nothning
end;

procedure TGIS_Viewer.WaitForNotBusy(
        _sender : TObject ;
  const _proc   : TGIS_WaitForNotBusyProc
) ;
{$IFNDEF ISLAND}
var
  th : T_waitForThread ;
{$ENDIF}
begin
  if not assigned( Parent ) then exit ;
  if not assigned( Parent.GetViewer ) then exit ;
  if not ( Parent.GetViewer is TGIS_Viewer ) then exit ;
  TGIS_Viewer(Parent.GetViewer).WaitForBackgroundProcesses ;
{$IFNDEF ISLAND}
  th := T_waitForThread.Create( True ) ;
  th.FreeOnTerminate := True ;
  th.oProc := _proc ;
  th.oVwr  := Self ;
  th.oSender := _sender ;
  th.start ;
{$ENDIF}
end;
{$ENDREGION 'TGIS_Viewer'}

{$REGION 'TGIS_ViewerNonVisual'}
function TGIS_ViewerNonVisual.fget_AutoStyle
  : Boolean ;
begin
  Result := oVwr.AutoStyle ;
end ;

procedure TGIS_ViewerNonVisual.fset_AutoStyle(
  const _value : Boolean
) ;
begin
  oVwr.AutoStyle := _value;
end ;

function TGIS_ViewerNonVisual.fget_BigExtent
  : TGIS_Extent ;
begin
  Result := GisNoWorld ;
end ;

function TGIS_ViewerNonVisual.fget_BigExtentMargin
  : Integer ;
begin
  Result := 0 ;
end ;

procedure TGIS_ViewerNonVisual.fset_BigExtentMargin(
  const _value : Integer
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.fget_KeepScale
  : Boolean ;
begin
  Result := False ;
end ;

procedure TGIS_ViewerNonVisual.fset_KeepScale(
  const _value : Boolean
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.fget_BusyLevel
  : Integer ;
begin
  Result := oVwr.BusyLevel ;
end ;

function TGIS_ViewerNonVisual.fget_BusyText
  : String ;
begin
  Result := oVwr.BusyText ;
end ;

function TGIS_ViewerNonVisual.fget_Center
  : TGIS_Point  ;
begin
  Result := GisPoint( NaN, NaN ) ;
end ;

procedure TGIS_ViewerNonVisual.fset_Center(
  const _value : TGIS_Point
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.fget_CenterPtg
  : TGIS_Point  ;
begin
  Result := GisPoint( NaN, NaN ) ;
end ;

procedure TGIS_ViewerNonVisual.fset_CenterPtg(
  const _value : TGIS_Point
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.fget_Color
  : TGIS_Color ;
begin
  Result := TGIS_Color.None
end ;

procedure TGIS_ViewerNonVisual.fset_Color(
  const _value : TGIS_Color
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.fget_Copyright
  : String ;
begin
  Result := oVwr.Copyright ;
end ;

function TGIS_ViewerNonVisual.fget_CS
  : TGIS_CSCoordinateSystem ;
begin
  Result := oVwr.CS ;
end ;

procedure TGIS_ViewerNonVisual.fset_CS(
  const _value : TGIS_CSCoordinateSystem
) ;
begin
  oVwr.CS := _value ;
end ;

function TGIS_ViewerNonVisual.fget_CustomPPI
  : Integer ;
begin
  Result := oVwr.CustomPPI ;
end ;

procedure TGIS_ViewerNonVisual.fset_CustomPPI(
  const _value : Integer
) ;
begin
  // do nothing ;
end ;

function TGIS_ViewerNonVisual.fget_Editor
  : IGIS_Editor ;
begin
  Result := oVwr.Editor ;
end ;

procedure TGIS_ViewerNonVisual.fset_Editor(
  const _value : IGIS_Editor
) ;
begin
  if _value is TGIS_Editor then
    oVwr.Editor := TGIS_Editor( _value ) ;
end ;

function TGIS_ViewerNonVisual.fget_Extent
  : TGIS_Extent ;
begin
  Result := oVwr.Extent ;
end ;

function TGIS_ViewerNonVisual.fget_FileCopyrights
  : String      ;
begin
  Result := oVwr.FileCopyrights ;
end ;

function TGIS_ViewerNonVisual.fget_FontScale
  : Integer ;
begin
  Result := oVwr.FontScale ;
end ;

procedure TGIS_ViewerNonVisual.fset_FontScale(
  const _value : Integer
) ;
begin
  // do nothing ;
end ;

function TGIS_ViewerNonVisual.fget_FullDrawExtent
  : TGIS_Extent ;
begin
 Result := oVwr.Extent ;
end ;

function TGIS_ViewerNonVisual.fget_IncrementalPaint
  : Boolean ;
begin
  Result := False
end ;

procedure TGIS_ViewerNonVisual.fset_IncrementalPaint(
  const _value : Boolean
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.fget_TiledPaint
  : Boolean ;
begin
  Result := False
end ;

procedure TGIS_ViewerNonVisual.fset_TiledPaint(
  const _value : Boolean
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.fget_InPaint
  : Boolean ;
begin
  Result := False ;
end ;

function TGIS_ViewerNonVisual.fget_IsBusy
  : Boolean ;
begin
  Result := oVwr.IsBusy ;
end ;

function TGIS_ViewerNonVisual.fget_IsEmpty
  : Boolean ;
begin
  Result := oVwr.IsEmpty ;
end ;

function TGIS_ViewerNonVisual.fget_IsLocked
 : Boolean ;
begin
  Result := oVwr.IsLocked ;
end ;

function TGIS_ViewerNonVisual.fget_IsTopmost
 : Boolean ;
begin
  Result := oVwr.IsTopmost ;
end ;

function TGIS_ViewerNonVisual.fget_Items
  : TGIS_LayerAbstractList ;
begin
  Result := oVwr.Items ;
end ;

function TGIS_ViewerNonVisual.fget_LabelsReg
  : TGIS_LabelsAreaAbstract ;
begin
  Result := nil ;
end ;

function TGIS_ViewerNonVisual.fget_MultiUserMode
  : TGIS_MultiUser ;
begin
  Result := oVwr.MultiUserMode ;
end ;

procedure TGIS_ViewerNonVisual.fset_MultiUserMode(
  const _value : TGIS_MultiUser
) ;
begin
  oVwr.MultiUserMode := _value ;
end ;

function TGIS_ViewerNonVisual.fget_CustomData
  : TGIS_StringList ;
begin
  Result := oVwr.CustomData ;
end ;

function TGIS_ViewerNonVisual.fget_OverlappedExtentMargin
  : Integer ;
begin
  Result := 0 ;
end ;

procedure TGIS_ViewerNonVisual.fset_OverlappedExtentMargin(
  const _value : Integer
) ;
begin
  // do nothing ;
end ;

function TGIS_ViewerNonVisual.fget_PPI
  : Integer ;
begin
  Result := oVwr.PPI ;
end ;

function TGIS_ViewerNonVisual.fget_ProjectFile
  : TGIS_ConfigAbstract ;
begin
  Result := oVwr.ProjectFile
end ;

procedure TGIS_ViewerNonVisual.fset_ProjectFile(
  const _value : TGIS_ConfigAbstract
) ;
begin
  oVwr.ProjectFile := _value ;
end ;

function TGIS_ViewerNonVisual.fget_ProjectName
  : String ;
begin
  Result := oVwr.ProjectName
end ;

function TGIS_ViewerNonVisual.fget_DelayedUpdate
  : Integer ;
begin
  Result := oVwr.DelayedUpdate ;
end;

procedure TGIS_ViewerNonVisual.fset_DelayedUpdate(
  const _value : Integer
) ;
begin
  oVwr.DelayedUpdate := 0 ;
end;

function TGIS_ViewerNonVisual.fget_ProgressiveUpdate
  : Integer ;
begin
  Result := oVwr.ProgressiveUpdate ;
end;

procedure TGIS_ViewerNonVisual.fset_ProgressiveUpdate(
  const _value : Integer
) ;
begin
  oVwr.ProgressiveUpdate := 0 ;
end;

function TGIS_ViewerNonVisual.fget_RestrictedDrag
  : Boolean ;
begin
  Result := oVwr.RestrictedDrag ;
end ;

procedure TGIS_ViewerNonVisual.fset_RestrictedDrag(
  const _value : Boolean
) ;
begin
  oVwr.RestrictedDrag := False ;
end ;

function TGIS_ViewerNonVisual.fget_RestrictedExtent
  : TGIS_Extent ;
begin
  Result := GisNoWorld ;
end ;

procedure TGIS_ViewerNonVisual.fset_RestrictedExtent(
  const _value : TGIS_Extent
) ;
begin
  // do nothing ;
end ;

function TGIS_ViewerNonVisual.fget_RotationAngle
  : Double ;
begin
  Result := 0 ;
end ;

procedure TGIS_ViewerNonVisual.fset_RotationAngle(
  const _value : Double
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.fget_RotationPoint
  : TGIS_Point ;
begin
  Result := GisPoint( NaN, NaN ) ;
end ;

procedure TGIS_ViewerNonVisual.fset_RotationPoint(
  const _value : TGIS_Point
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.fget_ScaleAsFloat
  : Double ;
begin
  Result := 1 ;
end ;

procedure TGIS_ViewerNonVisual.fset_ScaleAsFloat(
  const _value : Double
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.fget_ScaleAsText
  : String ;
begin
  Result := 'N/A' ;
end ;

procedure TGIS_ViewerNonVisual.fset_ScaleAsText(
  const _value : String
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.fget_Level
  : Double ;
begin
  Result := 0 ;
end ;

procedure TGIS_ViewerNonVisual.fset_Level(
  const _value : Double
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.fget_SelectionGisColor
  : TGIS_Color ;
begin
  Result := TGIS_Color.None ;
end ;

procedure TGIS_ViewerNonVisual.fset_SelectionGisColor(
  const _value : TGIS_Color
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.fget_SelectionOutlineOnly
  : Boolean ;
begin
  Result := False ;
end ;

procedure TGIS_ViewerNonVisual.fset_SelectionOutlineOnly(
  const _value : Boolean
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.fget_SelectionTransparency
  : Integer ;
begin
  Result := 0 ;
end ;

procedure TGIS_ViewerNonVisual.fset_SelectionTransparency(
  const _value : Integer
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.fget_SelectionWidth
  : Integer ;
begin
  Result := 0 ;
end ;

procedure TGIS_ViewerNonVisual.fset_SelectionWidth(
  const _value : Integer
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.fget_SystemPPI
  : Integer ;
begin
  Result := oVwr.SystemPPI ;
end ;

function TGIS_ViewerNonVisual.fget_ViewerParent
  : IGIS_ViewerParent ;
begin
  Result := Self ;
end ;

function TGIS_ViewerNonVisual.fget_ViewerParentRoot
  : IGIS_ViewerParent ;
begin
  Result := oVwr.ViewerParentRoot ;
end ;

function TGIS_ViewerNonVisual.fget_Viewport
  : TGIS_Point ;
begin
  Result := GisPoint( NaN, NaN ) ;
end ;

procedure TGIS_ViewerNonVisual.fset_Viewport(
  const _value : TGIS_Point
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.fget_VisibleExtent
  : TGIS_Extent ;
begin
  Result := oVwr.Extent ;
end ;

procedure TGIS_ViewerNonVisual.fset_VisibleExtent(
  const _value : TGIS_Extent
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.fget_UseAnimations
  : Boolean ;
begin
  Result := oVwr.UseAnimations ;
end ;

procedure TGIS_ViewerNonVisual.fset_UseAnimations(
  const _value : Boolean
) ;
begin
  oVwr.UseAnimations := False ;
end ;

function TGIS_ViewerNonVisual.fget_UseRTree
  : Boolean ;
begin
  Result := oVwr.UseRTree ;
end ;

procedure TGIS_ViewerNonVisual.fset_UseRTree(
  const _value : Boolean
) ;
begin
  oVwr.UseRTree := _value ;
end ;

function TGIS_ViewerNonVisual.fget_Zoom
  : Double ;
begin
  Result := 1 ;
end ;

procedure TGIS_ViewerNonVisual.fset_Zoom(
  const _value : Double
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.fget_ZoomEx
  : Double ;
begin
  Result := 1 ;
end ;

procedure TGIS_ViewerNonVisual.fset_ZoomEx(
  const _value : Double
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.fget_MasterViewer
  : IGIS_Viewer ;
begin
  Result := oVwr.MasterViewer ;
end;

procedure TGIS_ViewerNonVisual.fset_MasterViewer(
  const _value : IGIS_Viewer
) ;
begin
  oVwr.MasterViewer := _value ;
end;

function TGIS_ViewerNonVisual.fget_UponDestroy
  : Boolean ;
begin
  Result := oVwr.UponDestroy ;
end ;

function  TGIS_ViewerNonVisual.fget_TemporaryScaleInternal : Double      ;
begin
  Result := 1 ;
end ;

procedure TGIS_ViewerNonVisual.fset_TemporaryScaleInternal(
  const _value : Double
) ;
begin
  // do nothing
end ;

function  TGIS_ViewerNonVisual.fget_TemporaryVisibleExtent : TGIS_Extent ;
begin
  Result := oVwr.Extent ;
end ;

procedure TGIS_ViewerNonVisual.fset_TemporaryVisibleExtent(
  const _value : TGIS_Extent
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.fget_Hierarchy
  : IGIS_HierarchyManager ;
begin
  Result := oVwr.Hierarchy ;
end ;

function TGIS_ViewerNonVisual.fget_BusyEvent
  : TGIS_BusyEvent ;
begin
  Result := oVwr.BusyEvent ;
end ;

procedure TGIS_ViewerNonVisual.fset_BusyEvent(
  const _value : TGIS_BusyEvent
) ;
begin
  oVwr.BusyEvent := _value ;
end ;

{$IFDEF CLR}
  function TGIS_ViewerNonVisual.fget_ExtentChangeEvent
    : EventHandler ;
{$ELSE}
  function TGIS_ViewerNonVisual.fget_ExtentChangeEvent
    : TNotifyEvent ;
{$ENDIF}
begin
  Result := oVwr.ExtentChangeEvent ;
end ;

{$IFDEF CLR}
  procedure TGIS_ViewerNonVisual.fset_ExtentChangeEvent(
    const _value : EventHandler
  ) ;
{$ELSE}
  procedure TGIS_ViewerNonVisual.fset_ExtentChangeEvent(
    const _value : TNotifyEvent
  ) ;
{$ENDIF}
begin
  oVwr.ExtentChangeEvent := _value ;
end ;

{$IFDEF CLR}
  function TGIS_ViewerNonVisual.fget_VisibleExtentChangeEvent
    : EventHandler ;
{$ELSE}
  function TGIS_ViewerNonVisual.fget_VisibleExtentChangeEvent
    : TNotifyEvent ;
{$ENDIF}
begin
  Result := oVwr.VisibleExtentChangeEvent ;
end ;

{$IFDEF CLR}
  procedure TGIS_ViewerNonVisual.fset_VisibleExtentChangeEvent(
    const _value : EventHandler
  ) ;
{$ELSE}
  procedure TGIS_ViewerNonVisual.fset_VisibleExtentChangeEvent(
    const _value : TNotifyEvent
  ) ;
{$ENDIF}
begin
  oVwr.VisibleExtentChangeEvent := _value ;
end ;

{$IFDEF CLR}
  function TGIS_ViewerNonVisual.fget_ZoomChangeEvent
    : EventHandler ;
{$ELSE}
  function TGIS_ViewerNonVisual.fget_ZoomChangeEvent
    : TNotifyEvent ;
{$ENDIF}
begin
  Result := oVwr.ZoomChangeEvent ;
end ;

{$IFDEF CLR}
  procedure TGIS_ViewerNonVisual.fset_ZoomChangeEvent(
    const _value : EventHandler
  ) ;
{$ELSE}
  procedure TGIS_ViewerNonVisual.fset_ZoomChangeEvent(
    const _value : TNotifyEvent
  ) ;
{$ENDIF}
begin
  oVwr.ZoomChangeEvent := _value ;
end ;

function  TGIS_ViewerNonVisual.ChangeHash
  : Int64 ;
begin
  Result := oVwr.ChangeHash ;
end ;

procedure TGIS_ViewerNonVisual.Subscribe(
  const _control : IGIS_Subscribe
) ;
begin
  oVwr.Subscribe( _control ) ;
end ;

procedure TGIS_ViewerNonVisual.UnSubscribe(
  const _control : IGIS_Subscribe
) ;
begin
  oVwr.UnSubscribe( _control ) ;
end ;

procedure TGIS_ViewerNonVisual.NotifySubscribers(
  const _event   : Integer ;
  const _context : TObject
) ;
begin
  oVwr.NotifySubscribers( _event, _context ) ;
end ;

function TGIS_ViewerNonVisual.NotifyPaintException(
  const _message   : String ;
  const _exception : Exception
) : Boolean ;
begin
  Result := oVwr.NotifyPaintException( _message, _exception ) ;
end ;

procedure TGIS_ViewerNonVisual.Lock ;
begin
  oVwr.Lock ;
end ;

procedure TGIS_ViewerNonVisual.Unlock ;
begin
  oVwr.Unlock ;
end ;

procedure TGIS_ViewerNonVisual.Unlock(
  const _redraw : Boolean
) ;
begin
  oVwr.Unlock( _redraw ) ;
end ;

procedure TGIS_ViewerNonVisual.Interrupt ;
begin
  oVwr.Interrupt ;
end ;

function TGIS_ViewerNonVisual.Interrupted
  : Boolean ;
begin
  Result := oVwr.Interrupted ;
end ;

function  TGIS_ViewerNonVisual.HourglassActive
  : Boolean ;
begin
  Result := oVwr.HourglassActive ;
end ;

procedure TGIS_ViewerNonVisual.HourglassPrepare ;
begin
  oVwr.HourglassPrepare ;
end ;

procedure TGIS_ViewerNonVisual.HourglassRelease ;
begin
  oVwr.HourglassRelease ;
end ;

function  TGIS_ViewerNonVisual.HourglassShake
  : Boolean ;
begin
  Result := oVwr.HourglassShake ;
end ;

procedure TGIS_ViewerNonVisual.HourglassRestart ;
begin
  oVwr.HourglassRestart ;
end;

procedure TGIS_ViewerNonVisual.BusyPrepare(
  _sender : TObject ;
  _text   : String
) ;
begin
  oVwr.BusyPrepare( _sender, _text ) ;
end ;

procedure TGIS_ViewerNonVisual.BusyRelease(
  _sender : TObject
) ;
begin
  oVwr.BusyRelease( _sender ) ;
end ;

procedure TGIS_ViewerNonVisual.BusyShake(
      _sender : TObject ;
      _pos    : Int64 ;
      _end    : Int64 ;
  var _abort  : Boolean
) ;
begin
  oVwr.BusyShake( _sender, _pos, _end, _abort ) ;
end ;

{$IFDEF OXYGENE}
  procedure TGIS_ViewerNonVisual.RaiseBusyEvent(
    _sender  : TObject ;
    _e       : TGIS_BusyEventArgs
  ) ;
  begin
    oVwr.RaiseBusyEvent( _sender, _e ) ;
  end;
{$ELSE}
  procedure TGIS_ViewerNonVisual.RaiseBusyEvent(
        _sender  : TObject ;
        _pos     : Int64 ;
        _end     : Int64;
    var _abort   : Boolean
  ) ;
  begin
    oVwr.RaiseBusyEvent( _sender, _pos, _end, _abort ) ;
  end ;
{$ENDIF}

{$IFDEF OXYGENE}
  procedure TGIS_ViewerNonVisual.RaiseHelpEvent(
    _sender  : TObject ;
    _e       : TGIS_HelpEventArgs
  ) ;
  begin
    oVwr.RaiseHelpEvent( _sender, _e ) ;
  end ;
{$ELSE}
  procedure TGIS_ViewerNonVisual.RaiseHelpEvent(
        _sender  : TObject ;
        _name    : String
  ) ;
  begin
    oVwr.RaiseHelpEvent( _sender, _name ) ;
  end ;
{$ENDIF}

function TGIS_ViewerNonVisual.AssignedBusyEvent
  : TGIS_BusyEvent ;
begin
  Result := oVwr.AssignedBusyEvent() ;
end ;

function TGIS_ViewerNonVisual.AssignedHelpEvent
  : TGIS_HelpEvent ;
begin
  Result := oVwr.AssignedHelpEvent() ;
end ;

function TGIS_ViewerNonVisual.StorePaintState
  : TObject ;
begin
  Result := oVwr.StorePaintState ;
end ;

procedure TGIS_ViewerNonVisual.RestorePaintState(
  var _state : TObject
) ;
begin
  oVwr.RestorePaintState( _state ) ;
end ;

procedure TGIS_ViewerNonVisual.BeginPaintInternal ;
begin
  oVwr.BeginPaintInternal ;
end ;

procedure TGIS_ViewerNonVisual.EndPaintInternal ;
begin
  oVwr.EndPaintInternal ;
end ;

function TGIS_ViewerNonVisual.SynchronizePaint(
  const _interrupt : Boolean
) : Boolean ;
begin
  Result := oVwr.SynchronizePaint( _interrupt ) ;
end;

procedure TGIS_ViewerNonVisual.ReParentLock ;
begin
  oVwr.ReParentLock ;
end ;

procedure TGIS_ViewerNonVisual.ReParentUnlock ;
begin
  oVwr.ReParentUnlock ;
end ;

function TGIS_ViewerNonVisual.ReParent(
  const _parent : IGIS_ViewerParent
) : IGIS_ViewerParent ;
begin
  Result := oVwr.ReParent( _parent ) ;
end ;

function TGIS_ViewerNonVisual.AttachLayer(
  const _layer : TGIS_LayerAbstract
) : IGIS_Viewer ;
begin
  Result := oVwr.AttachLayer( _layer ) ;
end ;

procedure TGIS_ViewerNonVisual.Open(
  const _path : String
) ;
begin
  oVwr.Open( _path ) ;
end ;

procedure TGIS_ViewerNonVisual.Open(
  const _path   : String ;
  const _strict : Boolean
) ;
begin
  oVwr.Open( _path, _strict  ) ;
end ;

procedure TGIS_ViewerNonVisual.OpenEx(
  const _configFile : TGIS_ConfigAbstract ;
  const _path       : String
) ;
begin
  oVwr.OpenEx( _configFile, _path ) ;
end ;

procedure TGIS_ViewerNonVisual.OpenEx(
  const _configFile : TGIS_ConfigAbstract ;
  const _path       : String              ;
  const _strict     : Boolean
) ;
begin
  oVwr.OpenEx( _configFile, _path, _strict ) ;
end ;

procedure TGIS_ViewerNonVisual.Close ;
begin
  oVwr.Close ;
end ;

procedure TGIS_ViewerNonVisual.ReadConfig ;
begin
  oVwr.ReadConfig ;
end ;

procedure TGIS_ViewerNonVisual.RereadConfig ;
begin
  oVwr.RereadConfig ;
end ;

procedure TGIS_ViewerNonVisual.WriteConfig ;
begin
  oVwr.WriteConfig ;
end ;

procedure TGIS_ViewerNonVisual.Add(
  const _layer : TGIS_LayerAbstract
) ;
begin
  oVwr.Add( _layer ) ;
end ;

function TGIS_ViewerNonVisual.Get(
  const _name    : String
) : TGIS_LayerAbstract ;
begin
  Result := oVwr.Get( _name ) ;
end ;

procedure TGIS_ViewerNonVisual.Delete(
  const _name : String
) ;
begin
  oVwr.Delete( _name ) ;
end ;

procedure TGIS_ViewerNonVisual.AddHierarchy ;
begin
  oVwr.AddHierarchy ;
end ;

procedure TGIS_ViewerNonVisual.Draw(
  const _renderer   : TObject     ;
  const _mode       : TGIS_DrawMode
) ;
begin
  // do nothing
end;

function TGIS_ViewerNonVisual.GetGrid(
  const _extent : TGIS_Extent ;
  const _grid   : TGIS_GridArray
) : Boolean ;
begin
  Result := oVwr.GetGrid( _extent, _grid ) ;
end ;

procedure TGIS_ViewerNonVisual.RevertAll ;
begin
  oVwr.RevertAll ;
end ;

procedure TGIS_ViewerNonVisual.SaveProject ;
begin
  oVwr.SaveProject ;
end ;

procedure TGIS_ViewerNonVisual.SaveProject(
  const _relativepath : Boolean
) ;
begin
  oVwr.SaveProject( _relativepath ) ;
end ;

procedure TGIS_ViewerNonVisual.SaveProjectAs(
  const _path : String
) ;
begin
  oVwr.SaveProjectAs( _path ) ;
end ;

procedure TGIS_ViewerNonVisual.SaveProjectAs(
  const _path         : String ;
  const _relativepath : Boolean
) ;
begin
  oVwr.SaveProjectAs( _path, _relativepath ) ;
end ;

procedure TGIS_ViewerNonVisual.SaveProjectAsEx(
  const _configFile : TGIS_ConfigAbstract ;
  const _path       : String
) ;
begin
  oVwr.SaveProjectAsEx( _configFile, _path ) ;
end ;

procedure TGIS_ViewerNonVisual.SaveProjectAsEx(
  const _configFile   : TGIS_ConfigAbstract ;
  const _path         : String              ;
  const _relativepath : Boolean
) ;
begin
  oVwr.SaveProjectAsEx( _configFile, _path, _relativepath ) ;
end ;

procedure TGIS_ViewerNonVisual.SaveData ;
begin
  oVwr.SaveData ;
end ;

procedure TGIS_ViewerNonVisual.SaveAll ;
begin
  oVwr.SaveAll ;
end ;

function  TGIS_ViewerNonVisual.MustSave
  : Boolean ;
begin
  Result := oVwr.MustSave ;
end ;

procedure TGIS_ViewerNonVisual.MarkModified ;
begin
  oVwr.MarkModified ;
end ;

procedure TGIS_ViewerNonVisual.RecalcExtent ;
begin
  oVwr.RecalcExtent ;
end ;

procedure TGIS_ViewerNonVisual.Reposition ;
begin
  oVwr.Reposition ;
end ;

procedure TGIS_ViewerNonVisual.InvalidateExtent(
  const _extent : TGIS_Extent
) ;
begin
  // do nothing
end ;

procedure TGIS_ViewerNonVisual.InvalidateExtent(
  const _extent : TGIS_Extent ;
  const _deep   : Boolean
) ;
begin
  // do nothing
end ;

procedure TGIS_ViewerNonVisual.InvalidateWholeMap ;
begin
  // do nothing
end ;

procedure TGIS_ViewerNonVisual.InvalidateTopmost ;
begin
  // do nothing
end ;

procedure TGIS_ViewerNonVisual.InvalidateBasemap ;
begin
  // do nothing
end ;

procedure TGIS_ViewerNonVisual.InvalidateSelection ;
begin
  // do nothing
end ;

procedure TGIS_ViewerNonVisual.InvalidateEditor(
  const _final : Boolean
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.FullExtentZoom
  : Double ;
begin
  Result := 1;
end ;

procedure TGIS_ViewerNonVisual.FullExtent ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.Locate(
  const _ptg  : TGIS_Point ;
  const _prec : Double
) : TGIS_ShapeAbstract ;
begin
  Result := oVwr.Locate( _ptg, _prec ) ;
end ;

function TGIS_ViewerNonVisual.Locate(
  const _ptg     : TGIS_Point ;
  const _prec    : Double     ;
  const _visible : Boolean
) : TGIS_ShapeAbstract ;
begin
  Result := oVwr.Locate( _ptg, _prec, _visible ) ;
end ;

function TGIS_ViewerNonVisual.Locate(
  const _pt      : TPoint     ;
  const _prec    : Integer
) : TGIS_ShapeAbstract ;
begin
  Result := nil  ;
end ;

function TGIS_ViewerNonVisual.LocateEx(
  const _ptg     : TGIS_Point ;
  const _prec    : Double    ;
  const _visible : Boolean
) : TGIS_ShapeAbstractList ;
begin
  Result := oVwr.LocateEx( _ptg, _prec, _visible ) ;
end ;

function TGIS_ViewerNonVisual.MapToScreen(
  const _ptg : TGIS_Point
) : TPoint ;
begin
  Result := Point(0, 0) ;
end ;

function TGIS_ViewerNonVisual.MapToScreen3D(
  const _ptg : TGIS_Point3D
) : TPoint ;
begin
  Result := Point(0, 0) ;
end ;

function TGIS_ViewerNonVisual.ScreenToMap(
  const _pt : TPoint
) : TGIS_Point ;
begin
  Result := GisPoint( NaN, NaN) ;
end ;

function TGIS_ViewerNonVisual.ScreenToMap3D(
  const _pt : TPoint
) : TGIS_Point3D ;
begin
  Result := GisPoint3D( NaN, NaN, NaN, NaN ) ;
end ;

function TGIS_ViewerNonVisual.MapToScreenEx(
  const _pt : TGIS_Point
) : TGIS_Point ;
begin
  Result := GisPoint( NaN, NaN) ;
end ;

function TGIS_ViewerNonVisual.ScreenToMapEx(
  const _pt : TGIS_Point
) : TGIS_Point ;
begin
  Result := GisPoint( NaN, NaN) ;
end ;

function TGIS_ViewerNonVisual.MapToScreenRect(
  const _rct : TGIS_Extent
) : TRect ;
begin
  Result := Rect( 0, 0, 0, 0 ) ;
end ;

function TGIS_ViewerNonVisual.ScreenToMapRect(
  const _rct : TRect
) : TGIS_Extent ;
begin
  Result := GisExtent( NaN, NaN, NaN, NaN ) ;
end ;

function TGIS_ViewerNonVisual.PixelsToTwips(
  const _size : Integer
) : Integer ;
begin
  Result := 1 ;
end ;

function TGIS_ViewerNonVisual.TwipsToPixels(
  const _size : Integer
) : Integer ;
begin
  Result := 1 ;
end ;

function TGIS_ViewerNonVisual.TwipsToPoints(
  const _size : Integer
) : Integer ;
begin
  Result := 1 ;
end ;

procedure TGIS_ViewerNonVisual.MoveViewport(
  var _dx : Integer ;
  var _dy : Integer
) ;
begin
  // do nothing
end ;

{$IFDEF JAVA}
  procedure TGIS_ViewerNonVisual.MoveViewportEx(
    var _dx : java.lang.Double ;
    var _dy : java.lang.Double
  ) ;
  begin
    // do nothing
  end ;

  procedure TGIS_ViewerNonVisual.SetViewport(
    var _x : java.lang.Double ;
    var _y : java.lang.Double
  ) ;
  begin
    // do nothing
  end ;
{$ELSE}
  procedure TGIS_ViewerNonVisual.MoveViewportEx(
    var _dx : Double ;
    var _dy : Double
  ) ;
  begin
    // do nothing
  end ;

  procedure TGIS_ViewerNonVisual.SetViewport(
    var _x : Double ;
    var _y : Double
  ) ;
  begin
    // do nothing
  end ;
{$ENDIF}

procedure TGIS_ViewerNonVisual.CenterViewport(
  const _ptg : TGIS_Point
) ;
begin
  // do nothing
end ;

procedure TGIS_ViewerNonVisual.SetCSByWKT(
  const _wkt : String
) ;
begin
  oVwr.SetCSByWKT( _wkt ) ;
end ;

procedure TGIS_ViewerNonVisual.SetCSByEPSG(
  const _epsg : Integer
) ;
begin
  oVwr.SetCSByEPSG( _epsg ) ;
end ;

procedure TGIS_ViewerNonVisual.SetCSByWKTFile(
  const _path : String
) ;
begin
  oVwr.SetCSByWKTFile( _path ) ;
end ;

function TGIS_ViewerNonVisual.RotatedPoint(
  const _ptg : TGIS_Point
) : TGIS_Point ;
begin
  Result := oVwr.RotatedPoint( _ptg ) ;
end ;

function TGIS_ViewerNonVisual.UnrotatedPoint(
  const _ptg : TGIS_Point
) : TGIS_Point ;
begin
  Result := GisPoint( NaN, NaN );
end ;

function TGIS_ViewerNonVisual.RotatedPoint3D(
  const _ptg : TGIS_Point3D
) : TGIS_Point3D ;
begin
  Result := GisPoint3D( NaN, NaN, NaN, NaN );
end ;

procedure TGIS_ViewerNonVisual.RotatedPoint3D_ref(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.UnrotatedPoint3D(
  const _ptg : TGIS_Point3D
) : TGIS_Point3D ;
begin
  Result := _ptg ;
end ;

procedure TGIS_ViewerNonVisual.UnrotatedPoint3D_ref(
  {$IFNDEF JAVA} var {$ENDIF}  _ptg : TGIS_Point3D
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.RotatedExtent(
  const _extent : TGIS_Extent
) : TGIS_Extent ;
begin
  Result := _extent ;
end ;

function TGIS_ViewerNonVisual.UnrotatedExtent(
  const _extent : TGIS_Extent
) : TGIS_Extent ;
begin
  Result := _extent ;
end ;

function TGIS_ViewerNonVisual.GetRenderContext
  : TObject ;
begin
  Result := nil ;
end ;

procedure TGIS_ViewerNonVisual.WaitForBackgroundProcesses ;
begin
  oVwr.WaitForBackgroundProcesses ;
end;

procedure TGIS_ViewerNonVisual.WaitForNotBusy(
        _sender  : TObject ;
  const _proc : TGIS_WaitForNotBusyProc
) ;
begin
  oVwr.WaitForNotBusy( _sender, _proc ) ;
end;

procedure TGIS_ViewerNonVisual.ControlClose ;
begin
  // do nothing
end ;

procedure TGIS_ViewerNonVisual.ControlDrawTexture(
        _bmp : TObject     ;
  const _extent : TGIS_Extent ;
  const _ppi    : Integer
) ;
begin
  // do nothing
end ;

procedure TGIS_ViewerNonVisual.ControlDrawTexture(
        _bmp : TObject     ;
  const _layer  : TGIS_LayerAbstract ;
  const _extent : TGIS_Extent ;
  const _ppi    : Integer
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.ControlRenderer
  : TObject ;
begin
  Result := nil ;
end ;

procedure TGIS_ViewerNonVisual.ControlFlash(
  const _times : Integer ;
  const _delay : Integer
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.ControlSystemPPI
  : Integer ;
begin
  Result := 96 ;
end ;

function TGIS_ViewerNonVisual.ControlPPI
  : Integer ;
begin
  Result := 96 ;
end ;

procedure TGIS_ViewerNonVisual.ControlRepaint ;
begin
  // do nothing
end ;

procedure TGIS_ViewerNonVisual.ControlProcessMessages ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.ControlUpdateSynchronize(
  const _interrupt : Boolean
) : Boolean ;
begin
  Result := True ; // always synchronize
end;

procedure TGIS_ViewerNonVisual.ControlUpdateWholeMap ;
begin
  // do nothing
end ;

procedure TGIS_ViewerNonVisual.ControlUpdateProgressive ;
begin
  // do nothing
end;

procedure TGIS_ViewerNonVisual.ControlUpdateTopmost ;
begin
  // do nothing
end ;

procedure TGIS_ViewerNonVisual.ControlUpdateBasemap ;
begin
  // do nothing
end ;

procedure TGIS_ViewerNonVisual.ControlUpdateSelection;
begin
  // do nothing
end ;

procedure TGIS_ViewerNonVisual.ControlUpdateEditor(
  const _final : Boolean
) ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.ControlCanvasScale
  : Single ;
begin
  Result := 1 ;
end ;

procedure TGIS_ViewerNonVisual.ControlAutoCenterViewport(
  const _dx, _dy: Double
);
begin
  // do nothing
end ;

procedure TGIS_ViewerNonVisual.ControlExtentChanged ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.ControlCanvasHeight
  : Integer ;
begin
  Result := 0 ;
end ;

function TGIS_ViewerNonVisual.ControlCanvasWidth
  : Integer ;
begin
  Result := 0 ;
end ;

procedure TGIS_ViewerNonVisual.ControlHourglassShow ;
begin
  // do nothing
end ;

procedure TGIS_ViewerNonVisual.ControlHourglassHide ;
begin
  // do nothing
end ;

function TGIS_ViewerNonVisual.ControlHourglassShake
  : Boolean ;
begin
  Result := False ;
end ;

procedure TGIS_ViewerNonVisual.ControlSet3DMode(
  const _mode : TGIS_Viewer3DMode
) ;
begin
  // do nothing
end ;

procedure TGIS_ViewerNonVisual.ControlRaiseEditorChangeEvent(
  _sender : TObject
) ;
begin
  // do nothing
end ;

constructor TGIS_ViewerNonVisual.Create ;
begin
  {$IFDEF OXYGENE}
    inherited Create ;
  {$ELSE}
    inherited Create( nil ) ;
  {$ENDIF}

  oVwr := TGIS_Viewer.Create( Self ) ;

  DelayedUpdate := 0;
  ProgressiveUpdate := 0;
  UseAnimations := False ;
end ;

{$IFDEF DCC}
  destructor TGIS_ViewerNonVisual.Destroy ;
  begin
    doDestroy ;
    inherited ;
  end;
{$ENDIF}

procedure TGIS_ViewerNonVisual.doDestroy ;
begin
  FreeObject( oVwr ) ;

  inherited ;
end ;

function TGIS_ViewerNonVisual.SetViewer(const _viewer: TObject): TObject;
begin
  Result := oVwr ;
  oVwr := _viewer as TGIS_Viewer;
  oVwr.ReParent( self ) ;
end ;

function TGIS_ViewerNonVisual.GetViewer: TObject;
begin
  Result := oVwr ;
end ;

{$ENDREGION 'TGIS_ViewerNonVisual'}

{==================================== END =====================================}
end.
