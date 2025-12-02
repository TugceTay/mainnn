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
  VCL implementation of TGIS_PvlViewerWnd
}

unit VCL.GisPvlViewerWnd;

interface

uses
  System.Types, System.Classes,
  System.SysUtils,
  System.UiTypes,
  VCL.Controls,


  VCL.GisFramework,
  VCL.GisPvl,
  VCL.GisViewerWnd,
  PVL.GisPvl,
  PVL.GisViewerWnd,

  GisRendererAbstract,
  GisTypes,
  GisTypesUI,
  GisInterfaces,
  GisCsSystems,
  GisGraticuleHelper;

implementation

type
  T_PvlViewerWnd = class( TGIS_PvlControlVcl, IGIS_PvlViewerWnd, IGIS_Viewer, IGIS_ViewerWnd )
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;

    private
      procedure fset_3DMode           ( const _value  : TGIS_Viewer3DMode ) ;
      function  fget_View3D           : Boolean ;
      procedure fset_View3D           ( const _value  : Boolean     ) ;
      function  fget_Viewer3D         : IGIS_Viewer3D ;
      procedure fset_Viewer3D         ( const _viewer : IGIS_Viewer3D ) ;
      function  fget_Graticule        : TGIS_Graticule ;

    private // IGIS_Viewer event helpers
      FBusyEvent                    : TGIS_BusyEvent;
      FExtentChangeEvent            : TGIS_PvlEvent;
      FVisibleExtentChangeEvent     : TGIS_PvlEvent;
      FZoomChangeEvent              : TGIS_PvlEvent;

      procedure doBusyEvent         (       _sender     : TObject ;
                                            _pos        : Integer ;
                                            _end        : Integer ;
                                      var   _abort      : Boolean
                                    ) ;
      procedure doExtentChangedEvent(       _sender     : TObject
                                    ) ;
      procedure doVisibleExtentChangeEvent(
                                            _sender     : TObject
                                    ) ;
      procedure doZoomChangeEvent   (       _sender     : TObject
                                    ) ;

    private // IGIS_ViewerWnd event helpers
      FEditorChangeEvent            : TGIS_PvlEvent;
      FModeChangeEvent              : TGIS_PvlEvent;
      FTapSimpleEvent               : TGIS_PvlMouseEvent;
      FTapDoubleEvent               : TGIS_PvlMouseEvent;
      FTapLongEvent                 : TGIS_PvlMouseEvent;

      procedure doEditorChangeEvent (       _sender     : TObject
                                    ) ;
      procedure doModeChangeEvent   (       _sender     : TObject
                                    ) ;
      procedure doTapDoubleEvent    (       _sender     : TObject       ;
                                            _button     : TMouseButton  ;
                                            _shift      : TShiftState   ;
                                            _x          : Integer       ;
                                            _y          : Integer
                                    ) ;
      procedure doTapLongEvent      (       _sender     : TObject       ;
                                            _button     : TMouseButton  ;
                                            _shift      : TShiftState   ;
                                            _x          : Integer       ;
                                            _y          : Integer
                                    ) ;
      procedure doTapSimpleEvent    (       _sender     : TObject       ;
                                            _button     : TMouseButton  ;
                                            _shift      : TShiftState   ;
                                            _x          : Integer       ;
                                            _y          : Integer
                                    ) ;

    private // IGIS_ViewerParent eventrs

      FBeforePaintEvent             : TGIS_PaintEvent ;
      FAfterPaintEvent              : TGIS_PaintEvent ;
      FBeforePaintRendererEvent     : TGIS_RendererEvent ;
      FAfterPaintRendererEvent      : TGIS_RendererEvent ;
      FPaintExtraEvent              : TGIS_RendererEvent ;
      FBeforeUpdateEvent            : TGIS_RendererEvent ;
      FAfterUpdateEvent             : TGIS_RendererEvent ;

      procedure doBeforePaintEvent  (       _sender     : TObject       ;
                                            _canvas     : TObject
                                    ) ;
      procedure doAfterPaintEvent   (       _sender     : TObject       ;
                                            _canvas     : TObject
                                    ) ;
      procedure doBeforePaintRendererEvent(
                                           _sender     : TObject        ;
                                           _renderer   : TGIS_RendererAbstract ;
                                           _mode       : TGIS_DrawMode
                                    ) ;
      procedure doAfterPaintRendererEvent(
                                           _sender     : TObject        ;
                                           _renderer   : TGIS_RendererAbstract ;
                                           _mode       : TGIS_DrawMode
                                    ) ;
      procedure doPaintExtraEvent   (      _sender     : TObject        ;
                                           _renderer   : TGIS_RendererAbstract ;
                                           _mode       : TGIS_DrawMode
                                    ) ;
      procedure doBeforeUpdateEvent (      _sender     : TObject        ;
                                           _renderer   : TGIS_RendererAbstract ;
                                           _mode       : TGIS_DrawMode
                                    ) ;
      procedure doAfterUpdateEvent  (      _sender     : TObject        ;
                                           _renderer   : TGIS_RendererAbstract ;
                                           _mode       : TGIS_DrawMode
                                    ) ;

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
      function  fget_DelayedUpdate      : Integer     ;
      procedure fset_DelayedUpdate      ( const _value : Integer     ) ;
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
      function  fget_TiledPaint         : Boolean     ;   //?TILER added
      procedure fset_TiledPaint         ( const _value  : Boolean    ) ; //?TILER added
      function  fget_InPaint            : Boolean     ;
      function  fget_IsBusy             : Boolean     ;
      function  fget_IsEmpty            : Boolean     ;
      function  fget_IsLocked           : Boolean     ;
      function  fget_IsTopmost          : Boolean     ;
      function  fget_Items              : TGIS_LayerAbstractList ;
      function  fget_LabelsReg          : TGIS_LabelsAreaAbstract ;
      function  fget_Level              : Double      ;
      procedure fset_Level              ( const _value : Double      ) ;
      function  fget_Mode               : TGIS_ViewerMode ;
      procedure fset_Mode               ( const _value : TGIS_ViewerMode ) ;
      function  fget_MultiUserMode      : TGIS_MultiUser ;
      procedure fset_MultiUserMode      ( const _value : TGIS_MultiUser ) ;
      function  fget_CustomData         : TGIS_StringList ;
      function  fget_OverlappedExtentMargin
                                        : Integer     ;
      procedure fset_OverlappedExtentMargin
                                        ( const _value : Integer     ) ;
      function  fget_PPI                : Integer ;
      function  fget_ProgressiveUpdate  : Integer     ;
      procedure fset_ProgressiveUpdate  ( const _value : Integer     ) ;
      function  fget_ProjectFile        : TGIS_ConfigAbstract ;
      procedure fset_ProjectFile        ( const _value : TGIS_ConfigAbstract ) ;
      function  fget_ProjectName        : String      ;
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
      function  fget_SelectionOutlineOnly
                                        : Boolean     ;
      procedure fset_SelectionOutlineOnly(
                                          const _value : Boolean     ) ;
      function  fget_SelectionTransparency
                                        : Integer     ;
      procedure fset_SelectionTransparency(
                                          const _value : Integer     ) ;
      function  fget_SelectionWidth     : Integer     ;
      procedure fset_SelectionWidth     ( const _value : Integer     ) ;
      function  fget_SystemPPI          : Integer ;
      function  fget_UponDestroy        : Boolean     ;
      function  fget_UseAnimations      : Boolean     ;
      procedure fset_UseAnimations      ( const _value : Boolean     ) ;
      function  fget_UseRTree           : Boolean     ;
      procedure fset_UseRTree           ( const _value : Boolean     ) ;
      function  fget_ViewerParent       : IGIS_ViewerParent ;
      function  fget_ViewerParentRoot   : IGIS_ViewerParent ;
      function  fget_Viewport           : TGIS_Point  ;
      procedure fset_Viewport           ( const _value : TGIS_Point  ) ;
      function  fget_VisibleExtent      : TGIS_Extent ;
      procedure fset_VisibleExtent      ( const _value : TGIS_Extent ) ;
      function  fget_Zoom               : Double      ;
      procedure fset_Zoom               ( const _value : Double      ) ;
      function  fget_ZoomEx             : Double      ;
      procedure fset_ZoomEx             ( const _value : Double      ) ;
      function  fget_MasterViewer       : IGIS_Viewer ;
      procedure fset_MasterViewer       ( const _value : IGIS_Viewer ) ;
      function  fget_TemporaryScaleInternal
                                        : Double      ;
      procedure fset_TemporaryScaleInternal(
                                          const _value : Double      ) ;
      function  fget_TemporaryVisibleExtent
                                        : TGIS_Extent ;
      procedure fset_TemporaryVisibleExtent(
                                          const _value : TGIS_Extent ) ;

    private // IGIS_Viewer events access routines
      function  fget_BusyEvent          : TGIS_BusyEvent ;
      procedure fset_BusyEvent          ( const _value : TGIS_BusyEvent
                                        ) ;
      function  fget_ExtentChangeEvent  : TGIS_PvlEvent ;
      procedure fset_ExtentChangeEvent  ( const _value : TGIS_PvlEvent
                                        ) ;
      function  fget_VisibleExtentChangeEvent
                                        : TGIS_PvlEvent ;
      procedure fset_VisibleExtentChangeEvent
                                        ( const _value : TGIS_PvlEvent
                                        ) ;
      function  fget_ZoomChangeEvent    : TGIS_PvlEvent ;
      procedure fset_ZoomChangeEvent    ( const _value : TGIS_PvlEvent
                                        );

    private  // IGIS_ViewerParent events
      function  fget_BeforePaintEvent   : TGIS_PaintEvent ;
      procedure fset_BeforePaintEvent   ( const _value      : TGIS_PaintEvent
                                        );
      function  fget_AfterPaintEvent    : TGIS_PaintEvent ;
      procedure fset_AfterPaintEvent    ( const _value      : TGIS_PaintEvent
                                        );
      function  fget_BeforePaintRendererEvent
                                        : TGIS_RendererEvent ;
      procedure fset_BeforePaintRendererEvent(
                                          const _value      : TGIS_RendererEvent
                                        );
      function  fget_AfterPaintRendererEvent
                                        : TGIS_RendererEvent ;
      procedure fset_AfterPaintRendererEvent(
                                          const _value      : TGIS_RendererEvent
                                        );
      function  fget_PaintExtraEvent    : TGIS_RendererEvent ;
      procedure fset_PaintExtraEvent    ( const _value      : TGIS_RendererEvent
                                        );
      function  fget_BeforeUpdateEvent  : TGIS_RendererEvent ;
      procedure fset_BeforeUpdateEvent  ( const _value      : TGIS_RendererEvent
                                        );
      function  fget_AfterUpdateEvent   : TGIS_RendererEvent ;
      procedure fset_AfterUpdateEvent   ( const _value      : TGIS_RendererEvent
                                        );

    private // IGIS_ViewerWnd events access routines
      function  fget_ModeChangeEvent    : TGIS_PvlEvent;
      procedure fset_ModeChangeEvent    ( const _value      : TGIS_PvlEvent
                                        );
      function  fget_EditorChangeEvent  : TGIS_PvlEvent;
      procedure fset_EditorChangeEvent  ( const _value      : TGIS_PvlEvent
                                        );
      function  fget_TapSimpleEvent     : TGIS_PvlMouseEvent;
      procedure fset_TapSimpleEvent     ( const _value      : TGIS_PvlMouseEvent
                                        );
      function  fget_TapLongEvent       : TGIS_PvlMouseEvent;
      procedure fset_TapLongEvent       ( const _value      : TGIS_PvlMouseEvent
                                        );
      function  fget_TapDoubleEvent     : TGIS_PvlMouseEvent;
      procedure fset_TapDoubleEvent     ( const _value      : TGIS_PvlMouseEvent
                                        );

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
      function  Interrupted      : Boolean ; //?TILER ADDED

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
      procedure ReParentLock     ;

      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ReParentUnlock   ;

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

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      procedure MoveViewportEx   ( var   _dx, _dy : Double
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This method has no meaning on TGIS_ViewerNonVisual.
      /// </remarks>
      procedure SetViewport      ( var   _x , _y  : Double
                                 ) ;

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

    private

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

    public

      /// <summary>
      ///   3D Viewer object. Nil if 3D mode is off.
      /// </summary>
      property Viewer3D : IGIS_Viewer3D    read  fget_Viewer3D
                                           write fset_Viewer3D ;

      /// <summary>
      ///   3D state; If set to true them Viewer is in a 3D mode.
      /// </summary>
      property View3D   : Boolean          read  fget_View3D
                                           write fset_View3D  ;

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
      property TiledPaint : Boolean        read  fget_TiledPaint   //?TILLER addded
                                           write fset_TiledPaint
                                           default False           ;

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
      property MasterViewer : IGIS_Viewer  read  fget_MasterViewer
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

    public // IGIS_ViewerWnd public methods

      /// <inheritdoc from="IGIS_ViewerWnd"/>
      procedure PrintBmp       ( var   _bmp   : TGIS_Bitmap
                               ) ; overload ;

      /// <inheritdoc from="IGIS_ViewerWnd"/>
      procedure PrintBmp       ( var   _bmp   : TGIS_Bitmap ;
                                 const _full  : Boolean
                               ) ; overload ;

      /// <inheritdoc from="IGIS_ViewerWnd"/>
      procedure Print          ; overload ;

      /// <inheritdoc from="IGIS_ViewerWnd"/>
      procedure Print          ( _printer     : IGIS_Printer
                               ) ; overload ;

      /// <inheritdoc from="IGIS_ViewerWnd"/>
      procedure ZoomBy         ( const _zm    : Double  ;
                                 const _x     : Integer ;
                                 const _y     : Integer
                               ) ;

  end;

{$REGION 'Utility functions'}

function cMouseButton(
  const _value : TMouseButton
) : TGIS_PvlMouseButton ;
begin
  case( _value ) of
    TMouseButton.mbLeft   : Result := TGIS_PvlMouseButton.Left ;
    TMouseButton.mbRight  : Result := TGIS_PvlMouseButton.Right ;
    TMouseButton.mbMiddle : Result := TGIS_PvlMouseButton.Middle ;
  end;
end;

function cShiftState(
  const _value : TShiftState
) : TGIS_PvlShiftState ;
begin
  Result := [] ;

  if ssShift in _value then
    Result := Result + [TGIS_PvlShiftStateItem.Shift] ;
  if ssCtrl in _value then
    Result := Result + [TGIS_PvlShiftStateItem.Ctrl] ;
  if ssAlt in _value then
    Result := Result + [TGIS_PvlShiftStateItem.Alt] ;
  if ssLeft in _value then
    Result := Result + [TGIS_PvlShiftStateItem.Left] ;
  if ssRight in _value then
    Result := Result + [TGIS_PvlShiftStateItem.Right] ;
  if ssMiddle in _value then
    Result := Result + [TGIS_PvlShiftStateItem.Middle] ;
  if ssDouble in _value then
    Result := Result + [TGIS_PvlShiftStateItem.Double] ;
  if ssTouch in _value then
    Result := Result + [TGIS_PvlShiftStateItem.Touch] ;
  if ssPen in _value then
    Result := Result + [TGIS_PvlShiftStateItem.Pen] ;
  if ssCommand in _value then
    Result := Result + [TGIS_PvlShiftStateItem.Command] ;
  if ssHorizontal in _value then
    Result := Result + [TGIS_PvlShiftStateItem.Horizontal] ;
end;

{$ENDREGION 'Utility functions'}

{$REGION 'IGIS_PvlViewerWnd properties accessor'}

function T_PvlViewerWnd.fget_View3D
  : Boolean ;
begin
  Result := TGIS_ViewerWnd( oControl ).View3D ;
end;

procedure T_PvlViewerWnd.fset_View3D(
  const _value: Boolean
) ;
begin
  TGIS_ViewerWnd( oControl ).View3D := _value ;
end;

function T_PvlViewerWnd.fget_Viewer3D
  : IGIS_Viewer3D ;
begin
  Result := TGIS_ViewerWnd( oControl ).Viewer3D ;
end;

procedure T_PvlViewerWnd.fset_Viewer3D(
  const _viewer: IGIS_Viewer3D
) ;
begin
  TGIS_ViewerWnd( oControl ).Viewer3D := _viewer ;
end;

procedure T_PvlViewerWnd.fset_3DMode(
  const _value: TGIS_Viewer3DMode
) ;
begin
  TGIS_ViewerWnd( oControl ).ControlSet3DMode( _value ) ;
end;

function T_PvlViewerWnd.fget_Graticule
  : TGIS_Graticule ;
begin
  Result := TGIS_ViewerWnd( oControl ).Graticule ;
end;

{$ENDREGION 'IGIS_ViewerWnd property access routines'}

{$REGION 'IGIS_PvlViewer event helpers'}

procedure T_PvlViewerWnd.doBusyEvent(
        _sender     : TObject       ;
        _pos        : Integer ;
        _end        : Integer ;
  var   _abort      : Boolean
);
begin
  if Assigned( FBusyEvent ) then
    FBusyEvent( _sender, _pos, _end, _abort );
end;

procedure T_PvlViewerWnd.doExtentChangedEvent(
        _sender     : TObject
);
begin
  if Assigned( FExtentChangeEvent ) then
    FExtentChangeEvent( _sender ) ;
end;

procedure T_PvlViewerWnd.doVisibleExtentChangeEvent(
        _sender     : TObject
);
begin
  if Assigned( FVisibleExtentChangeEvent ) then
    FVisibleExtentChangeEvent( _sender ) ;
end;

procedure T_PvlViewerWnd.doZoomChangeEvent(
        _sender     : TObject
);
begin
  if Assigned( FZoomChangeEvent ) then
    FZoomChangeEvent( _sender ) ;
end;

{$ENDREGION 'IGIS_Viewer event helpers'}

{$REGION 'IGIS_PvlViewerWnd event helpers'}

procedure T_PvlViewerWnd.doBeforePaintEvent(
       _sender     : TObject       ;
       _canvas     : TObject
) ;
begin
  if Assigned( FBeforePaintEvent ) then
    FBeforePaintEvent( _sender, _canvas ) ;
end;

procedure T_PvlViewerWnd.doAfterPaintEvent(
       _sender     : TObject       ;
       _canvas     : TObject
) ;
begin
  if Assigned( FAfterPaintEvent ) then
    FAfterPaintEvent( _sender, _canvas ) ;
end;

procedure T_PvlViewerWnd.doBeforePaintRendererEvent(
        _sender     : TObject        ;
        _renderer   : TGIS_RendererAbstract ;
        _mode       : TGIS_DrawMode
) ;
begin
  if Assigned( FBeforePaintRendererEvent ) then
    FBeforePaintRendererEvent( _sender, _renderer, _mode ) ;
end;

procedure T_PvlViewerWnd.doAfterPaintRendererEvent(
        _sender     : TObject        ;
        _renderer   : TGIS_RendererAbstract ;
        _mode       : TGIS_DrawMode
) ;
begin
  if Assigned( FAfterPaintRendererEvent ) then
    FAfterPaintRendererEvent( _sender, _renderer, _mode ) ;
end;

procedure T_PvlViewerWnd.doPaintExtraEvent(
        _sender     : TObject        ;
        _renderer   : TGIS_RendererAbstract ;
        _mode       : TGIS_DrawMode
) ;
begin
  if Assigned( FPaintExtraEvent ) then
    FPaintExtraEvent( _sender, _renderer, _mode ) ;
end;

procedure T_PvlViewerWnd.doBeforeUpdateEvent(
        _sender     : TObject        ;
        _renderer   : TGIS_RendererAbstract ;
        _mode       : TGIS_DrawMode
) ;
begin
  if Assigned( FBeforeUpdateEvent ) then
    FBeforeUpdateEvent( _sender, _renderer, _mode ) ;
end;

procedure T_PvlViewerWnd.doAfterUpdateEvent(
        _sender     : TObject        ;
        _renderer   : TGIS_RendererAbstract ;
        _mode       : TGIS_DrawMode
) ;
begin
  if Assigned( FAfterUpdateEvent ) then
    FAfterUpdateEvent( _sender, _renderer, _mode ) ;
end;

{$ENDREGION 'IGIS_PvlViewerWnd event helpers'}

{$REGION 'IGIS_ViewerWnd event helpers'}

procedure T_PvlViewerWnd.doEditorChangeEvent(
        _sender     : TObject
);
begin
  if Assigned( FEditorChangeEvent ) then
    FEditorChangeEvent( _sender ) ;
end;

procedure T_PvlViewerWnd.doModeChangeEvent(
        _sender     : TObject
);
begin
  if Assigned( FModeChangeEvent ) then
    FModeChangeEvent( _sender ) ;
end;

procedure T_PvlViewerWnd.doTapDoubleEvent(
        _sender     : TObject       ;
        _button     : TMouseButton  ;
        _shift      : TShiftState   ;
        _x          : Integer       ;
        _y          : Integer
) ;
begin
  if Assigned( FTapDoubleEvent ) then
    FTapDoubleEvent( _sender, cMouseButton(_button), cShiftState(_shift), _x, _y ) ;
end;

procedure T_PvlViewerWnd.doTapLongEvent(
        _sender     : TObject       ;
        _button     : TMouseButton  ;
        _shift      : TShiftState   ;
        _x          : Integer       ;
        _y          : Integer
) ;
begin
  if Assigned( FTapLongEvent ) then
    FTapLongEvent( _sender, cMouseButton(_button), cShiftState(_shift), _x, _y ) ;
end;

procedure T_PvlViewerWnd.doTapSimpleEvent(
        _sender     : TObject       ;
        _button     : TMouseButton  ;
        _shift      : TShiftState   ;
        _x          : Integer       ;
        _y          : Integer
) ;
begin
  if Assigned( FTapSimpleEvent ) then
    FTapSimpleEvent( _sender, cMouseButton(_button), cShiftState(_shift), _x, _y ) ;
end;

{$ENDREGION 'IGIS_ViewerWnd event helpers'}

{$REGION 'IGIS_Viewer property access routines'}

function T_PvlViewerWnd.fget_AutoStyle
  : Boolean;
begin
  Result := TGIS_ViewerWnd( oControl ).AutoStyle;
end;

procedure T_PvlViewerWnd.fset_AutoStyle(
  const _value : Boolean
);
begin
  TGIS_ViewerWnd( oControl ).AutoStyle := _value;
end;

function T_PvlViewerWnd.fget_BigExtent
  : TGIS_Extent;
begin
  Result := TGIS_ViewerWnd( oControl ).BigExtent;
end;

function T_PvlViewerWnd.fget_BigExtentMargin
  : Integer;
begin
  Result := TGIS_ViewerWnd( oControl ).BigExtentMargin;
end;

procedure T_PvlViewerWnd.fset_BigExtentMargin(
  const _value : Integer
);
begin
  TGIS_ViewerWnd( oControl ).BigExtentMargin := _value;
end;

function T_PvlViewerWnd.fget_KeepScale
  : Boolean;
begin
  Result := TGIS_ViewerWnd( oControl ).KeepScale;
end;

procedure T_PvlViewerWnd.fset_KeepScale(
  const _value : Boolean
);
begin
  TGIS_ViewerWnd( oControl ).KeepScale := _value;
end;

function T_PvlViewerWnd.fget_BusyLevel
  : Integer;
begin
  Result := TGIS_ViewerWnd( oControl ).BusyLevel;
end;

function T_PvlViewerWnd.fget_BusyText
  : String;
begin
  Result := TGIS_ViewerWnd( oControl ).BusyText;
end;

function T_PvlViewerWnd.fget_Center
  : TGIS_Point;
begin
  Result := TGIS_ViewerWnd( oControl ).Center;
end;

procedure T_PvlViewerWnd.fset_Center(
  const _value : TGIS_Point
);
begin
  TGIS_ViewerWnd( oControl ).Center := _value;
end;

function T_PvlViewerWnd.fget_CenterPtg
  : TGIS_Point;
begin
  Result := TGIS_ViewerWnd( oControl ).CenterPtg;
end;

procedure T_PvlViewerWnd.fset_CenterPtg(
  const _value : TGIS_Point
);
begin
  TGIS_ViewerWnd( oControl ).CenterPtg := _value;
end;

function T_PvlViewerWnd.fget_Color
  : TGIS_Color;
begin
  Result := GISColor( TGIS_ViewerWnd( oControl ).Color );
end;

procedure T_PvlViewerWnd.fset_Color(
  const _value : TGIS_Color
);
begin
  TGIS_ViewerWnd( oControl ).Color := VCLColor( _value );
end;

function T_PvlViewerWnd.fget_Copyright
  : String;
begin
  Result := TGIS_ViewerWnd( oControl ).Copyright;
end;

function T_PvlViewerWnd.fget_CS
  : TGIS_CSCoordinateSystem;
begin
  Result := TGIS_ViewerWnd( oControl ).CS;
end;

procedure T_PvlViewerWnd.fset_CS(
  const _value : TGIS_CSCoordinateSystem
);
begin
  TGIS_ViewerWnd( oControl ).CS := _value;
end;

function T_PvlViewerWnd.fget_CustomPPI
  : Integer;
begin
  Result := TGIS_ViewerWnd( oControl ).CustomPPI;
end;

procedure T_PvlViewerWnd.fset_CustomPPI(
  const _value : Integer
);
begin
  TGIS_ViewerWnd( oControl ).CustomPPI := _value;
end;

function T_PvlViewerWnd.fget_DelayedUpdate
  : Integer;
begin
  Result := TGIS_ViewerWnd( oControl ).DelayedUpdate;
end;

procedure T_PvlViewerWnd.fset_DelayedUpdate(
  const _value: Integer
);
begin
  TGIS_ViewerWnd( oControl ).DelayedUpdate := _value;
end;

function T_PvlViewerWnd.fget_Editor
  : IGIS_Editor;
begin
  Result := TGIS_ViewerWnd( oControl ).Editor;
end;

procedure T_PvlViewerWnd.fset_Editor(
  const _value : IGIS_Editor
);
begin
  TGIS_ViewerWnd( oControl ).Editor := _value;
end;

function T_PvlViewerWnd.fget_Extent
  : TGIS_Extent;
begin
  Result := TGIS_ViewerWnd( oControl ).Extent;
end;

function T_PvlViewerWnd.fget_FileCopyrights: String;
begin
  Result := TGIS_ViewerWnd( oControl ).FileCopyrights;
end;

function T_PvlViewerWnd.fget_FontScale
  : Integer;
begin
  Result := TGIS_ViewerWnd( oControl ).FontScale;
end;

procedure T_PvlViewerWnd.fset_FontScale(
  const _value : Integer
);
begin
  TGIS_ViewerWnd( oControl ).FontScale := _value;
end;

function T_PvlViewerWnd.fget_FullDrawExtent
  : TGIS_Extent;
begin
  Result := TGIS_ViewerWnd( oControl ).FullDrawExtent;
end;

function T_PvlViewerWnd.fget_Hierarchy
  : IGIS_HierarchyManager;
begin
  Result := TGIS_ViewerWnd( oControl ).Hierarchy;
end;

function T_PvlViewerWnd.fget_IncrementalPaint
  : Boolean;
begin
  Result := TGIS_ViewerWnd( oControl ).IncrementalPaint;
end;

procedure T_PvlViewerWnd.fset_IncrementalPaint(
  const _value : Boolean
);
begin
  TGIS_ViewerWnd( oControl ).IncrementalPaint := _value;
end;

function T_PvlViewerWnd.fget_TiledPaint
  : Boolean;
begin
  Result := TGIS_ViewerWnd( oControl ).TiledPaint;
end;

procedure T_PvlViewerWnd.fset_TiledPaint(
  const _value : Boolean
);
begin
  TGIS_ViewerWnd( oControl ).TiledPaint := _value;
end;

function T_PvlViewerWnd.fget_InPaint
  : Boolean;
begin
  Result := TGIS_ViewerWnd( oControl ).InPaint;
end;

function T_PvlViewerWnd.fget_IsBusy
  : Boolean;
begin
  Result := TGIS_ViewerWnd( oControl ).IsBusy;
end;

function T_PvlViewerWnd.fget_IsEmpty
  : Boolean;
begin
  Result := TGIS_ViewerWnd( oControl ).IsEmpty;
end;

function T_PvlViewerWnd.fget_IsLocked
  : Boolean;
begin
  Result := TGIS_ViewerWnd( oControl ).IsLocked;
end;

function T_PvlViewerWnd.fget_IsTopmost
  : Boolean;
begin
  Result := TGIS_ViewerWnd( oControl ).IsTopmost;
end;

function T_PvlViewerWnd.fget_Items
  : TGIS_LayerAbstractList;
begin
  Result := TGIS_ViewerWnd( oControl ).Items;
end;

function T_PvlViewerWnd.fget_LabelsReg
  : TGIS_LabelsAreaAbstract;
begin
  Result := TGIS_ViewerWnd( oControl ).LabelsReg;
end;

function T_PvlViewerWnd.fget_Level
  : Double;
begin
  Result := TGIS_ViewerWnd( oControl ).Level;
end;

procedure T_PvlViewerWnd.fset_Level(
  const _value : Double
);
begin
  TGIS_ViewerWnd( oControl ).Level := _value;
end;

function T_PvlViewerWnd.fget_Mode
  : TGIS_ViewerMode;
begin
  Result := TGIS_ViewerWnd( oControl ).Mode;
end;

procedure T_PvlViewerWnd.fset_Mode(
  const _value : TGIS_ViewerMode
);
begin
  TGIS_ViewerWnd( oControl ).Mode := _value;
end;

function T_PvlViewerWnd.fget_MultiUserMode
  : TGIS_MultiUser;
begin
  Result := TGIS_ViewerWnd( oControl ).MultiUserMode;
end;

procedure T_PvlViewerWnd.fset_MultiUserMode(
  const _value : TGIS_MultiUser
);
begin
  TGIS_ViewerWnd( oControl ).MultiUserMode := _value;
end;

function T_PvlViewerWnd.fget_CustomData
  : TGIS_StringList;
begin
  Result := TGIS_ViewerWnd( oControl ).CustomData;
end;

function T_PvlViewerWnd.fget_OverlappedExtentMargin
  : Integer;
begin
  Result := TGIS_ViewerWnd( oControl ).OverlappedExtentMargin;
end;

procedure T_PvlViewerWnd.fset_OverlappedExtentMargin(
  const _value : Integer
);
begin
  TGIS_ViewerWnd( oControl ).OverlappedExtentMargin := _value;
end;

function T_PvlViewerWnd.fget_PPI
  : Integer;
begin
  Result := TGIS_ViewerWnd( oControl ).PPI;
end;

function T_PvlViewerWnd.fget_ProgressiveUpdate
  : Integer;
begin
  Result := TGIS_ViewerWnd( oControl ).ProgressiveUpdate;
end;

procedure T_PvlViewerWnd.fset_ProgressiveUpdate(
  const _value : Integer
);
begin
  TGIS_ViewerWnd( oControl ).ProgressiveUpdate := _value;
end;

function T_PvlViewerWnd.fget_ProjectFile
  : TGIS_ConfigAbstract;
begin
  Result := TGIS_ViewerWnd( oControl ).ProjectFile;
end;

procedure T_PvlViewerWnd.fset_ProjectFile(
  const _value : TGIS_ConfigAbstract
);
begin
//?  TGIS_ViewerWnd( oControl ).ProjectFile := _value;
end;

function T_PvlViewerWnd.fget_ProjectName
  : String;
begin
  Result := TGIS_ViewerWnd( oControl ).ProjectName;
end;

function T_PvlViewerWnd.fget_RestrictedDrag
  : Boolean;
begin
  Result := TGIS_ViewerWnd( oControl ).RestrictedDrag;
end;

procedure T_PvlViewerWnd.fset_RestrictedDrag(
  const _value : Boolean
);
begin
  TGIS_ViewerWnd( oControl ).RestrictedDrag := _value;
end;

function T_PvlViewerWnd.fget_RestrictedExtent
  : TGIS_Extent;
begin
  Result := TGIS_ViewerWnd( oControl ).RestrictedExtent;
end;

procedure T_PvlViewerWnd.fset_RestrictedExtent(
  const _value : TGIS_Extent
);
begin
  TGIS_ViewerWnd( oControl ).RestrictedExtent := _value;
end;

function T_PvlViewerWnd.fget_RotationAngle
  : Double;
begin
  Result := TGIS_ViewerWnd( oControl ).RotationAngle;
end;

procedure T_PvlViewerWnd.fset_RotationAngle(
  const _value : Double
);
begin
  TGIS_ViewerWnd( oControl ).RotationAngle := _value;
end;

function T_PvlViewerWnd.fget_RotationPoint
  : TGIS_Point;
begin
  Result := TGIS_ViewerWnd( oControl ).RotationPoint;
end;

procedure T_PvlViewerWnd.fset_RotationPoint(
  const _value : TGIS_Point
);
begin
  TGIS_ViewerWnd( oControl ).RotationPoint := _value;
end;

function T_PvlViewerWnd.fget_ScaleAsFloat
  : Double;
begin
  Result := TGIS_ViewerWnd( oControl ).ScaleAsFloat;
end;

procedure T_PvlViewerWnd.fset_ScaleAsFloat(
  const _value : Double
);
begin
  TGIS_ViewerWnd( oControl ).ScaleAsFloat := _value;
end;

function T_PvlViewerWnd.fget_ScaleAsText
  : String;
begin
  Result := TGIS_ViewerWnd( oControl ).ScaleAsText;
end;

procedure T_PvlViewerWnd.fset_ScaleAsText(
  const _value : String
);
begin
  TGIS_ViewerWnd( oControl ).ScaleAsText := _value;
end;

function T_PvlViewerWnd.fget_SelectionGisColor
  : TGIS_Color;
begin
  Result := TGIS_ViewerWnd( oControl ).SelectionGisColor;
end;

procedure T_PvlViewerWnd.fset_SelectionGisColor(
  const _value : TGIS_Color
);
begin
  TGIS_ViewerWnd( oControl ).SelectionGisColor := _value;
end;

function T_PvlViewerWnd.fget_SelectionOutlineOnly
  : Boolean;
begin
  Result := TGIS_ViewerWnd( oControl ).SelectionOutlineOnly;
end;

procedure T_PvlViewerWnd.fset_SelectionOutlineOnly(
  const _value : Boolean
);
begin
  TGIS_ViewerWnd( oControl ).SelectionOutlineOnly := _value;
end;

function T_PvlViewerWnd.fget_SelectionTransparency
  : Integer;
begin
  Result := TGIS_ViewerWnd( oControl ).SelectionTransparency;
end;

procedure T_PvlViewerWnd.fset_SelectionTransparency(
  const _value : Integer
);
begin
  TGIS_ViewerWnd( oControl ).SelectionTransparency := _value;
end;

function T_PvlViewerWnd.fget_SelectionWidth
  : Integer;
begin
  Result := TGIS_ViewerWnd( oControl ).SelectionWidth;
end;

procedure T_PvlViewerWnd.fset_SelectionWidth(
  const _value : Integer
);
begin
  TGIS_ViewerWnd( oControl ).SelectionWidth := _value;
end;

function T_PvlViewerWnd.fget_SystemPPI
  : Integer;
begin
  Result := TGIS_ViewerWnd( oControl ).SystemPPI;
end;

function T_PvlViewerWnd.fget_UponDestroy
  : Boolean;
begin
  Result := TGIS_ViewerWnd( oControl ).UponDestroy;
end;

function T_PvlViewerWnd.fget_UseAnimations
  : Boolean;
begin
  Result := TGIS_ViewerWnd( oControl ).UseAnimations
end;

procedure T_PvlViewerWnd.fset_UseAnimations(
  const _value : Boolean
);
begin
  TGIS_ViewerWnd( oControl ).UseAnimations := _value;
end;

function T_PvlViewerWnd.fget_UseRTree
  : Boolean;
begin
  Result := TGIS_ViewerWnd( oControl ).UseRTree;
end;

procedure T_PvlViewerWnd.fset_UseRTree(
  const _value : Boolean
);
begin
  TGIS_ViewerWnd( oControl ).UseRTree := _value;
end;

function T_PvlViewerWnd.fget_ViewerParent
  : IGIS_ViewerParent;
begin
  Result := TGIS_ViewerWnd( oControl ).ViewerParent;
end;

function T_PvlViewerWnd.fget_ViewerParentRoot
  : IGIS_ViewerParent;
begin
  Result := TGIS_ViewerWnd( oControl ).ViewerParentRoot;
end;

function T_PvlViewerWnd.fget_Viewport
  : TGIS_Point;
begin
  Result := TGIS_ViewerWnd( oControl ).Viewport;
end;

procedure T_PvlViewerWnd.fset_Viewport(
  const _value : TGIS_Point
);
begin
  TGIS_ViewerWnd( oControl ).Viewport := _value;
end;

function T_PvlViewerWnd.fget_VisibleExtent
  : TGIS_Extent;
begin
  Result := TGIS_ViewerWnd( oControl ).VisibleExtent;
end;

procedure T_PvlViewerWnd.fset_VisibleExtent(
  const _value : TGIS_Extent
);
begin
  TGIS_ViewerWnd( oControl ).VisibleExtent := _value;
end;

function T_PvlViewerWnd.fget_Zoom
  : Double;
begin
  Result := TGIS_ViewerWnd( oControl ).Zoom;
end;

procedure T_PvlViewerWnd.fset_Zoom(
  const _value : Double
);
begin
  TGIS_ViewerWnd( oControl ).Zoom := _value;
end;

function T_PvlViewerWnd.fget_ZoomEx
  : Double;
begin
  Result := TGIS_ViewerWnd( oControl ).ZoomEx;
end;

procedure T_PvlViewerWnd.fset_ZoomEx(
  const _value : Double
);
begin
  TGIS_ViewerWnd( oControl ).ZoomEx := _value;
end;

function T_PvlViewerWnd.fget_MasterViewer
  : IGIS_Viewer ;
begin
  Result := TGIS_ViewerWnd( oControl ).MasterViewer ;
end;

procedure T_PvlViewerWnd.fset_MasterViewer(
  const _value : IGIS_Viewer
) ;
begin
  TGIS_ViewerWnd( oControl ).MasterViewer := _value ;
end;

function T_PvlViewerWnd.fget_TemporaryScaleInternal
  : Double;
begin
  Result := TGIS_ViewerWnd( oControl ).TemporaryScaleInternal;
end;

procedure T_PvlViewerWnd.fset_TemporaryScaleInternal(
  const _value : Double
);
begin
  TGIS_ViewerWnd( oControl ).TemporaryScaleInternal := _value;
end;

function T_PvlViewerWnd.fget_TemporaryVisibleExtent
  : TGIS_Extent;
begin
  Result := TGIS_ViewerWnd( oControl ).TemporaryVisibleExtent;
end;

procedure T_PvlViewerWnd.fset_TemporaryVisibleExtent(
  const _value : TGIS_Extent
);
begin
  TGIS_ViewerWnd( oControl ).TemporaryVisibleExtent := _value;
end;

{$ENDREGION 'IGIS_Viewer property access routines'}

{$REGION 'IGIS_Viewer events access routines'}

function T_PvlViewerWnd.fget_BusyEvent
  : TGIS_BusyEvent;
begin
  Result := FBusyEvent;
end;

procedure T_PvlViewerWnd.fset_BusyEvent(
  const _value : TGIS_BusyEvent
);
begin
  FBusyEvent := _value ;
  if Assigned( _value ) then
    TGIS_ViewerWnd( oControl ).BusyEvent := doBusyEvent
  else
    TGIS_ViewerWnd( oControl ).BusyEvent := nil ;
end;

function T_PvlViewerWnd.fget_ExtentChangeEvent
  : TGIS_PvlEvent;
begin
  Result := FExtentChangeEvent;
end;

procedure T_PvlViewerWnd.fset_ExtentChangeEvent(
  const _value : TGIS_PvlEvent
);
begin
  FExtentChangeEvent := _value ;
  if Assigned( _value ) then
    TGIS_ViewerWnd( oControl ).ExtentChangeEvent := doExtentChangedEvent
  else
    TGIS_ViewerWnd( oControl ).ExtentChangeEvent := nil ;
end;

function T_PvlViewerWnd.fget_VisibleExtentChangeEvent
  : TGIS_PvlEvent;
begin
  Result := FVisibleExtentChangeEvent;
end;

procedure T_PvlViewerWnd.fset_VisibleExtentChangeEvent(
  const _value : TGIS_PvlEvent
);
begin
  FVisibleExtentChangeEvent := _value ;
  if Assigned( _value ) then
    TGIS_ViewerWnd( oControl ).VisibleExtentChangeEvent := doVisibleExtentChangeEvent
  else
    TGIS_ViewerWnd( oControl ).VisibleExtentChangeEvent := nil ;
end;

function T_PvlViewerWnd.fget_ZoomChangeEvent
  : TGIS_PvlEvent;
begin
  Result := FZoomChangeEvent;
end;

procedure T_PvlViewerWnd.fset_ZoomChangeEvent(
  const _value : TGIS_PvlEvent
);
begin
  FZoomChangeEvent := _value ;
  if Assigned( _value ) then
    TGIS_ViewerWnd( oControl ).ZoomChangeEvent := doZoomChangeEvent
  else
    TGIS_ViewerWnd( oControl ).ZoomChangeEvent := nil ;
end;


{$ENDREGION 'IGIS_Viewer events access routines'}

{$REGION 'IGIS_ViewerParent events access routines'}

function T_PvlViewerWnd.fget_BeforePaintEvent
  : TGIS_PaintEvent ;
begin
  Result := FBeforePaintEvent;
end;

procedure T_PvlViewerWnd.fset_BeforePaintEvent(
  const _value : TGIS_PaintEvent
);
begin
  FBeforePaintEvent := _value ;
  if Assigned( _value ) then
    TGIS_ViewerWnd( oControl ).BeforePaintEvent := doBeforePaintEvent
  else
    TGIS_ViewerWnd( oControl ).BeforePaintEvent := nil ;
end;

function T_PvlViewerWnd.fget_AfterPaintEvent
  : TGIS_PaintEvent ;
begin
  Result := FAfterPaintEvent;
end;

procedure T_PvlViewerWnd.fset_AfterPaintEvent(
  const _value : TGIS_PaintEvent
);
begin
  FAfterPaintEvent := _value ;
  if Assigned( _value ) then
    TGIS_ViewerWnd( oControl ).AfterPaintEvent := doAfterPaintEvent
  else
    TGIS_ViewerWnd( oControl ).AfterPaintEvent := nil ;
end;

function T_PvlViewerWnd.fget_BeforePaintRendererEvent
  : TGIS_RendererEvent ;
begin
  Result := FBeforePaintRendererEvent;
end;

procedure T_PvlViewerWnd.fset_BeforePaintRendererEvent(
  const _value : TGIS_RendererEvent
);
begin
  FBeforePaintRendererEvent := _value ;
  if Assigned( _value ) then
    TGIS_ViewerWnd( oControl ).BeforePaintRendererEvent := doBeforePaintRendererEvent
  else
    TGIS_ViewerWnd( oControl ).BeforePaintRendererEvent := nil ;
end;

function T_PvlViewerWnd.fget_AfterPaintRendererEvent
  : TGIS_RendererEvent ;
begin
  Result := FAfterPaintRendererEvent;
end;

procedure T_PvlViewerWnd.fset_AfterPaintRendererEvent(
  const _value : TGIS_RendererEvent
);
begin
  FAfterPaintRendererEvent := _value ;
  if Assigned( _value ) then
    TGIS_ViewerWnd( oControl ).AfterPaintRendererEvent := doAfterPaintRendererEvent
  else
    TGIS_ViewerWnd( oControl ).AfterPaintRendererEvent := nil ;
end;

function T_PvlViewerWnd.fget_PaintExtraEvent
  : TGIS_RendererEvent ;
begin
  Result := FPaintExtraEvent;
end;

procedure T_PvlViewerWnd.fset_PaintExtraEvent(
  const _value : TGIS_RendererEvent
);
begin
  FPaintExtraEvent := _value ;
  if Assigned( _value ) then
    TGIS_ViewerWnd( oControl ).PaintExtraEvent := doPaintExtraEvent
  else
    TGIS_ViewerWnd( oControl ).PaintExtraEvent := nil ;
end;

function T_PvlViewerWnd.fget_BeforeUpdateEvent
  : TGIS_RendererEvent ;
begin
  Result := FBeforeUpdateEvent;
end;

procedure T_PvlViewerWnd.fset_BeforeUpdateEvent(
  const _value : TGIS_RendererEvent
);
begin
  FBeforeUpdateEvent := _value ;
  if Assigned( _value ) then
    TGIS_ViewerWnd( oControl ).BeforeUpdateEvent := doBeforeUpdateEvent
  else
    TGIS_ViewerWnd( oControl ).BeforeUpdateEvent := nil ;
end;

function T_PvlViewerWnd.fget_AfterUpdateEvent
  : TGIS_RendererEvent ;
begin
  Result := FAfterUpdateEvent;
end;

procedure T_PvlViewerWnd.fset_AfterUpdateEvent(
  const _value : TGIS_RendererEvent
);
begin
  FAfterUpdateEvent := _value ;
  if Assigned( _value ) then
    TGIS_ViewerWnd( oControl ).AfterUpdateEvent := doAfterUpdateEvent
  else
    TGIS_ViewerWnd( oControl ).AfterUpdateEvent := nil ;
end;

{$ENDREGION 'IGIS_ViewerWnd events access routines'}

{$REGION 'IGIS_ViewerWnd events access routines'}

function T_PvlViewerWnd.fget_ModeChangeEvent
  : TGIS_PvlEvent;
begin
  Result := FModeChangeEvent;
end;

procedure T_PvlViewerWnd.fset_ModeChangeEvent(
  const _value : TGIS_PvlEvent
);
begin
  FModeChangeEvent := _value ;
  if Assigned( _value ) then
    TGIS_ViewerWnd( oControl ).ModeChangeEvent := doModeChangeEvent
  else
    TGIS_ViewerWnd( oControl ).ModeChangeEvent := nil ;
end;

function T_PvlViewerWnd.fget_EditorChangeEvent
  : TGIS_PvlEvent;
begin
  Result := FEditorChangeEvent;
end;

procedure T_PvlViewerWnd.fset_EditorChangeEvent(
  const _value : TGIS_PvlEvent
);
begin
  FEditorChangeEvent := _value ;
  if Assigned( _value ) then
    TGIS_ViewerWnd( oControl ).EditorChangeEvent := doEditorChangeEvent
  else
    TGIS_ViewerWnd( oControl ).EditorChangeEvent := nil ;
end;

function T_PvlViewerWnd.fget_TapSimpleEvent
  : TGIS_PvlMouseEvent;
begin
  Result := FTapSimpleEvent;
end;

procedure T_PvlViewerWnd.fset_TapSimpleEvent(
  const _value : TGIS_PvlMouseEvent
);
begin
  FTapSimpleEvent := _value ;
  if Assigned( _value ) then
    TGIS_ViewerWnd( oControl ).TapSimpleEvent := doTapSimpleEvent
  else
    TGIS_ViewerWnd( oControl ).TapSimpleEvent := nil ;
end;

function T_PvlViewerWnd.fget_TapLongEvent
  : TGIS_PvlMouseEvent;
begin
  Result := FTapLongEvent;
end;

procedure T_PvlViewerWnd.fset_TapLongEvent(
  const _value : TGIS_PvlMouseEvent
);
begin
  FTapLongEvent := _value ;
  if Assigned( _value ) then
    TGIS_ViewerWnd( oControl ).TapLongEvent := doTapLongEvent
  else
    TGIS_ViewerWnd( oControl ).TapLongEvent := nil ;
end;

function T_PvlViewerWnd.fget_TapDoubleEvent
  : TGIS_PvlMouseEvent;
begin
  Result := FTapDoubleEvent;
end;

procedure T_PvlViewerWnd.fset_TapDoubleEvent(
  const _value : TGIS_PvlMouseEvent
);
begin
  FTapDoubleEvent := _value ;
  if Assigned( _value ) then
    TGIS_ViewerWnd( oControl ).TapDoubleEvent := doTapDoubleEvent
  else
    TGIS_ViewerWnd( oControl ).TapDoubleEvent := nil ;
end;

{$ENDREGION 'IGIS_ViewerWnd events access routines'}

{$REGION 'IGIS_Viewer public methods'}

function T_PvlViewerWnd.ChangeHash
  : Int64;
begin
  TGIS_ViewerWnd( oControl ).ChangeHash;
end;

procedure T_PvlViewerWnd.Subscribe(
  const _control : IGIS_Subscribe
);
begin
  TGIS_ViewerWnd( oControl ).Subscribe( _control );
end;

procedure T_PvlViewerWnd.UnSubscribe(
  const _control : IGIS_Subscribe
);
begin
  TGIS_ViewerWnd( oControl ).UnSubscribe( _control );
end;

procedure T_PvlViewerWnd.NotifySubscribers(
  const _event   : Integer;
  const _context : TObject
);
begin
  TGIS_ViewerWnd( oControl ).NotifySubscribers( _event, _context );
end;

function T_PvlViewerWnd.NotifyPaintException(
  const _message   : String;
  const _exception : Exception
) : Boolean;
begin
  Result := TGIS_ViewerWnd( oControl ).NotifyPaintException( _message, _exception );
end;

procedure T_PvlViewerWnd.Lock;
begin
  TGIS_ViewerWnd( oControl ).Lock;
end;

procedure T_PvlViewerWnd.Unlock;
begin
  TGIS_ViewerWnd( oControl ).Unlock;
end;

procedure T_PvlViewerWnd.Unlock(
  const _redraw : Boolean
);
begin
  TGIS_ViewerWnd( oControl ).Unlock( _redraw );
end;

procedure T_PvlViewerWnd.Interrupt;
begin
  TGIS_ViewerWnd( oControl ).Interrupt;
end;

function T_PvlViewerWnd.Interrupted
  : Boolean ;
begin
  Result := TGIS_ViewerWnd( oControl ).Interrupted;
end;

function T_PvlViewerWnd.HourglassActive
  : Boolean;
begin
  Result := TGIS_ViewerWnd( oControl ).HourglassActive;
end;

procedure T_PvlViewerWnd.HourglassPrepare;
begin
  TGIS_ViewerWnd( oControl ).HourglassPrepare;
end;

procedure T_PvlViewerWnd.HourglassRelease;
begin
  TGIS_ViewerWnd( oControl ).HourglassRelease;
end;

function T_PvlViewerWnd.HourglassShake
  : Boolean;
begin
  Result := TGIS_ViewerWnd( oControl ).HourglassShake;
end;

procedure T_PvlViewerWnd.HourglassRestart;
begin
  TGIS_ViewerWnd( oControl ).HourglassRestart;
end;

procedure T_PvlViewerWnd.BusyPrepare(
  _sender : TObject;
  _text   : String
);
begin
  TGIS_ViewerWnd( oControl ).BusyPrepare( _sender, _text );
end;


procedure T_PvlViewerWnd.BusyRelease(
  _sender: TObject
);
begin
  TGIS_ViewerWnd( oControl ).BusyRelease( _sender );
end;

procedure T_PvlViewerWnd.BusyShake(
      _sender : TObject;
      _pos,
      _end    : Int64;
  var _abort : Boolean
);
begin
  TGIS_ViewerWnd( oControl ).BusyShake( _sender, _pos, _end, _abort );
end;

procedure T_PvlViewerWnd.RaiseBusyEvent(
      _sender : TObject;
      _pos,
      _end    : Int64;
  var _abort  : Boolean);
begin
  TGIS_ViewerWnd( oControl ).RaiseBusyEvent( _sender, _pos, _end, _abort );
end;

procedure T_PvlViewerWnd.RaiseHelpEvent(
  _sender : TObject;
  _name   : String
);
begin
  TGIS_ViewerWnd( oControl ).RaiseHelpEvent( _sender, _name );
end;

function T_PvlViewerWnd.AssignedBusyEvent
  : TGIS_BusyEvent;
begin
//?
end;

function T_PvlViewerWnd.AssignedHelpEvent
  : TGIS_HelpEvent;
begin
//?
end;

function T_PvlViewerWnd.StorePaintState
  : TObject;
begin
  Result := TGIS_ViewerWnd( oControl ).StorePaintState;
end;

procedure T_PvlViewerWnd.RestorePaintState(
  var _state: TObject
);
begin
  TGIS_ViewerWnd( oControl ).RestorePaintState( _state );
end;

procedure T_PvlViewerWnd.BeginPaintInternal;
begin
  TGIS_ViewerWnd( oControl ).BeginPaintInternal;
end;

procedure T_PvlViewerWnd.EndPaintInternal;
begin
  TGIS_ViewerWnd( oControl ).EndPaintInternal;
end;

function T_PvlViewerWnd.SynchronizePaint(
  const _interrupt : Boolean
) : Boolean;
begin
  Result := TGIS_ViewerWnd( oControl ).SynchronizePaint( _interrupt );
end;

procedure T_PvlViewerWnd.ReParentLock;
begin
  TGIS_ViewerWnd( oControl ).ReParentLock;
end;

procedure T_PvlViewerWnd.ReParentUnlock;
begin
  TGIS_ViewerWnd( oControl ).ReParentUnlock;
end;

function T_PvlViewerWnd.ReParent(
  const _parent : IGIS_ViewerParent
) : IGIS_ViewerParent;
begin
  Result := TGIS_ViewerWnd( oControl ).ReParent( _parent );
end;

function T_PvlViewerWnd.AttachLayer(
  const _layer : TGIS_LayerAbstract
) : IGIS_Viewer;
begin
  TGIS_ViewerWnd( oControl ).AttachLayer( _layer );
end;

procedure T_PvlViewerWnd.Open(
  const _path : String
);
begin
  TGIS_ViewerWnd( oControl ).Open( _path );
end;

procedure T_PvlViewerWnd.Open(
  const _path   : String;
  const _strict : Boolean
);
begin
  TGIS_ViewerWnd( oControl ).Open( _path, _strict );
end;

procedure T_PvlViewerWnd.OpenEx(
  const _configFile : TGIS_ConfigAbstract;
  const _path       : String
);
begin
  TGIS_ViewerWnd( oControl ).OpenEx( _configFile, _path );
end;

procedure T_PvlViewerWnd.OpenEx(
  const _configFile : TGIS_ConfigAbstract;
  const _path       : String;
  const _strict     : Boolean
);
begin
  TGIS_ViewerWnd( oControl ).OpenEx( _configFile, _path, _strict );
end;

procedure T_PvlViewerWnd.Close;
begin
  TGIS_ViewerWnd( oControl ).Close;
end;

procedure T_PvlViewerWnd.ReadConfig;
begin
  TGIS_ViewerWnd( oControl ).ReadConfig;
end;

procedure T_PvlViewerWnd.RereadConfig;
begin
  TGIS_ViewerWnd( oControl ).RereadConfig;
end;

procedure T_PvlViewerWnd.WriteConfig;
begin
  TGIS_ViewerWnd( oControl ).WriteConfig;
end;

procedure T_PvlViewerWnd.Add(
  const _layer : TGIS_LayerAbstract
);
begin
  TGIS_ViewerWnd( oControl ).Add( _layer );
end;

function T_PvlViewerWnd.Get(
  const _name : String
) : TGIS_LayerAbstract;
begin
  Result := TGIS_ViewerWnd( oControl ).Get( _name );
end;

procedure T_PvlViewerWnd.Delete(
  const _name: String
);
begin
  TGIS_ViewerWnd( oControl ).Delete( _name );
end;

procedure T_PvlViewerWnd.AddHierarchy;
begin
  TGIS_ViewerWnd( oControl ).AddHierarchy;
end;

procedure T_PvlViewerWnd.Draw(
  const _renderer : TObject;
  const _mode     : TGIS_DrawMode
);
begin
  TGIS_ViewerWnd( oControl ).Draw( _renderer, _mode );
end;

function T_PvlViewerWnd.GetGrid(
  const _extent : TGIS_Extent;
  const _grid   : TGIS_GridArray
) : Boolean;
begin
  Result := TGIS_ViewerWnd( oControl ).GetGrid( _extent, _grid );
end;

procedure T_PvlViewerWnd.RevertAll;
begin
  TGIS_ViewerWnd( oControl ).RevertAll;
end;


procedure T_PvlViewerWnd.SaveProject;
begin
  TGIS_ViewerWnd( oControl ).SaveProject;
end;

procedure T_PvlViewerWnd.SaveProject(
  const _relativepath : Boolean
);
begin
  TGIS_ViewerWnd( oControl ).SaveProject( _relativepath );
end;

procedure T_PvlViewerWnd.SaveProjectAs(
  const _path : String
);
begin
  TGIS_ViewerWnd( oControl ).SaveProjectAs( _path );
end;

procedure T_PvlViewerWnd.SaveProjectAs(
  const _path         : String;
  const _relativepath : Boolean
);
begin
  TGIS_ViewerWnd( oControl ).SaveProjectAs( _path, _relativepath );
end;

procedure T_PvlViewerWnd.SaveProjectAsEx(
  const _configFile : TGIS_ConfigAbstract;
  const _path       : String
);
begin
  TGIS_ViewerWnd( oControl ).SaveProjectAsEx( _configFile, _path );
end;

procedure T_PvlViewerWnd.SaveProjectAsEx(
  const _configFile   : TGIS_ConfigAbstract;
  const _path         : String;
  const _relativepath : Boolean
);
begin
  TGIS_ViewerWnd( oControl ).SaveProjectAsEx( _configFile, _path, _relativepath );
end;

procedure T_PvlViewerWnd.SaveData;
begin
  TGIS_ViewerWnd( oControl ).SaveData;
end;

procedure T_PvlViewerWnd.SaveAll;
begin
  TGIS_ViewerWnd( oControl ).SaveAll;
end;

function T_PvlViewerWnd.MustSave
  : Boolean;
begin
  Result := TGIS_ViewerWnd( oControl ).MustSave;
end;

procedure T_PvlViewerWnd.MarkModified;
begin
  TGIS_ViewerWnd( oControl ).MarkModified;
end;

procedure T_PvlViewerWnd.RecalcExtent;
begin
  TGIS_ViewerWnd( oControl ).RecalcExtent;
end;

procedure T_PvlViewerWnd.Reposition;
begin
  TGIS_ViewerWnd( oControl ).Reposition;
end;

procedure T_PvlViewerWnd.InvalidateExtent(
  const _extent : TGIS_Extent
);
begin
  TGIS_ViewerWnd( oControl ).InvalidateExtent( _extent );
end;

procedure T_PvlViewerWnd.InvalidateExtent(
  const _extent : TGIS_Extent;
  const _deep   : Boolean
);
begin
  TGIS_ViewerWnd( oControl ).InvalidateExtent( _extent, _deep );
end;

procedure T_PvlViewerWnd.InvalidateWholeMap;
begin
  TGIS_ViewerWnd( oControl ).InvalidateWholeMap;
end;

procedure T_PvlViewerWnd.InvalidateTopmost;
begin
  TGIS_ViewerWnd( oControl ).InvalidateTopmost;
end;

procedure T_PvlViewerWnd.InvalidateBasemap;
begin
  TGIS_ViewerWnd( oControl ).InvalidateBasemap;
end;

procedure T_PvlViewerWnd.InvalidateSelection;
begin
  TGIS_ViewerWnd( oControl ).InvalidateSelection;
end;

procedure T_PvlViewerWnd.InvalidateEditor(
  const _final : Boolean
);
begin
  TGIS_ViewerWnd( oControl ).InvalidateEditor( _final );
end;

function T_PvlViewerWnd.FullExtentZoom
  : Double;
begin
  TGIS_ViewerWnd( oControl ).FullExtentZoom
end;

procedure T_PvlViewerWnd.FullExtent;
begin
  TGIS_ViewerWnd( oControl ).FullExtent;
end;

function T_PvlViewerWnd.Locate(
  const _ptg  : TGIS_Point;
  const _prec : Double
) : TGIS_ShapeAbstract;
begin
  Result := TGIS_ViewerWnd( oControl ).Locate( _ptg, _prec );
end;

function T_PvlViewerWnd.Locate(
  const _ptg     : TGIS_Point;
  const _prec    : Double;
  const _visible : Boolean
) : TGIS_ShapeAbstract;
begin
  Result := TGIS_ViewerWnd( oControl ).Locate( _ptg, _prec, _visible);
end;

function T_PvlViewerWnd.Locate(
  const _pt   : TPoint;
  const _prec : Integer
) : TGIS_ShapeAbstract;
begin
  Result := TGIS_ViewerWnd( oControl ).Locate( _pt, _prec );
end;

function T_PvlViewerWnd.LocateEx(
  const _ptg     : TGIS_Point;
  const _prec    : Double;
  const _visible : Boolean
) : TGIS_ShapeAbstractList;
begin
  Result := TGIS_ViewerWnd( oControl ).LocateEx( _ptg, _prec, _visible );
end;

function T_PvlViewerWnd.MapToScreen(
  const _ptg : TGIS_Point
) : TPoint;
begin
  Result := TGIS_ViewerWnd( oControl ).MapToScreen( _ptg );
end;

function T_PvlViewerWnd.MapToScreen3D(
  const _ptg : TGIS_Point3D
) : TPoint;
begin
  Result := TGIS_ViewerWnd( oControl ).MapToScreen3D( _ptg );
end;

function T_PvlViewerWnd.ScreenToMap(
  const _pt : TPoint
) : TGIS_Point;
begin
  Result := TGIS_ViewerWnd( oControl ).ScreenToMap( _pt );
end;

function T_PvlViewerWnd.ScreenToMap3D(
  const _pt : TPoint
) : TGIS_Point3D;
begin
  Result := TGIS_ViewerWnd( oControl ).ScreenToMap3D( _pt );
end;

function T_PvlViewerWnd.MapToScreenEx(
  const _pt : TGIS_Point
) : TGIS_Point;
begin
  Result := TGIS_ViewerWnd( oControl ).MapToScreenEx( _pt );
end;

function T_PvlViewerWnd.ScreenToMapEx(
  const _pt : TGIS_Point
) : TGIS_Point;
begin
  Result := TGIS_ViewerWnd( oControl ).ScreenToMapEx( _pt );
end;

function T_PvlViewerWnd.MapToScreenRect(
  const _rct : TGIS_Extent
) : TRect;
begin
  Result := TGIS_ViewerWnd( oControl ).MapToScreenRect( _rct );
end;

function T_PvlViewerWnd.ScreenToMapRect(
  const _rct : TRect
) : TGIS_Extent;
begin
  Result := TGIS_ViewerWnd( oControl ).ScreenToMapRect( _rct );
end;

function T_PvlViewerWnd.PixelsToTwips(
  const _size : Integer
) : Integer;
begin
  Result := TGIS_ViewerWnd( oControl ).PixelsToTwips( _size );
end;

function T_PvlViewerWnd.TwipsToPixels(
  const _size : Integer
) : Integer;
begin
  Result := TGIS_ViewerWnd( oControl ).TwipsToPixels( _size );
end;

function T_PvlViewerWnd.TwipsToPoints(
  const _size : Integer
): Integer;
begin
  Result := TGIS_ViewerWnd( oControl ).TwipsToPoints( _size );
end;

procedure T_PvlViewerWnd.MoveViewport(
  var _dx, _dy: Integer
);
begin
  TGIS_ViewerWnd( oControl ).MoveViewport( _dx, _dy );
end;

procedure T_PvlViewerWnd.MoveViewportEx(
  var _dx, _dy: Double
);
begin
  TGIS_ViewerWnd( oControl ).MoveViewportEx( _dx, _dy );
end;

procedure T_PvlViewerWnd.SetViewport(
  var _x, _y: Double
);
begin
  TGIS_ViewerWnd( oControl ).SetViewport( _x, _y );
end;

procedure T_PvlViewerWnd.CenterViewport(
  const _ptg : TGIS_Point
);
begin
  TGIS_ViewerWnd( oControl ).CenterViewport( _ptg );
end;

procedure T_PvlViewerWnd.SetCSByWKT(
  const _wkt : String
);
begin
  TGIS_ViewerWnd( oControl ).SetCSByWKT( _wkt );
end;

procedure T_PvlViewerWnd.SetCSByEPSG(
  const _epsg : Integer
);
begin
  TGIS_ViewerWnd( oControl ).SetCSByEPSG( _epsg );
end;

procedure T_PvlViewerWnd.SetCSByWKTFile(
  const _path : String
);
begin
  TGIS_ViewerWnd( oControl ).SetCSByWKTFile( _path );
end;

function T_PvlViewerWnd.RotatedPoint(
  const _ptg : TGIS_Point
) : TGIS_Point;
begin
  Result := TGIS_ViewerWnd( oControl ).RotatedPoint( _ptg );
end;

function T_PvlViewerWnd.UnrotatedPoint(
  const _ptg : TGIS_Point
) : TGIS_Point;
begin
  Result := TGIS_ViewerWnd( oControl ).UnrotatedPoint( _ptg );
end;

function T_PvlViewerWnd.RotatedPoint3D(
  const _ptg : TGIS_Point3D
) : TGIS_Point3D;
begin
  Result := TGIS_ViewerWnd( oControl ).RotatedPoint3D( _ptg );
end;

procedure T_PvlViewerWnd.RotatedPoint3D_ref(
  var _ptg : TGIS_Point3D
);
begin
  TGIS_ViewerWnd( oControl ).RotatedPoint3D_ref( _ptg );
end;

function T_PvlViewerWnd.UnrotatedPoint3D(
  const _ptg : TGIS_Point3D
) : TGIS_Point3D;
begin
  Result := TGIS_ViewerWnd( oControl ).UnrotatedPoint3D( _ptg );
end;

procedure T_PvlViewerWnd.UnrotatedPoint3D_ref(
  var _ptg : TGIS_Point3D
);
begin
  TGIS_ViewerWnd( oControl ).UnrotatedPoint3D_ref( _ptg );
end;

function T_PvlViewerWnd.RotatedExtent(
  const _extent : TGIS_Extent
) : TGIS_Extent;
begin
  Result := TGIS_ViewerWnd( oControl ).RotatedExtent( _extent );
end;

function T_PvlViewerWnd.UnrotatedExtent(
  const _extent : TGIS_Extent
) : TGIS_Extent;
begin
  Result := TGIS_ViewerWnd( oControl ).UnrotatedExtent( _extent );
end;

function T_PvlViewerWnd.GetRenderContext
  : TObject;
begin
  Result := TGIS_ViewerWnd( oControl ).GetRenderContext;
end;

procedure T_PvlViewerWnd.WaitForBackgroundProcesses;
begin
  TGIS_ViewerWnd( oControl ).WaitForBackgroundProcesses;
end;

procedure T_PvlViewerWnd.WaitForNotBusy(
        _sender : TObject;
  const _proc   : TGIS_WaitForNotBusyProc
);
begin
  TGIS_ViewerWnd( oControl ).WaitForNotBusy( _sender, _proc );
end;

{$ENDREGION 'IGIS_Viewer public methods'}

{$REGION 'T_PvlViewerWnd specific'}

procedure T_PvlViewerWnd.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oControl := TGIS_ViewerWnd.Create( TWinControl( _context.NativeParent ) );
  TControl( oControl ).Parent := TWinControl( _context.NativeParent );
end;

{$ENDREGION 'T_PvlViewerWnd specific'}

{$REGION 'IGIS_ViewerWnd public methods'}

procedure T_PvlViewerWnd.PrintBmp(
  var _bmp  : TGIS_Bitmap
) ;
begin
  TGIS_ViewerWnd( oControl ).PrintBmp(_bmp) ;
end;

procedure T_PvlViewerWnd.PrintBmp(
  var _bmp  : TGIS_Bitmap ;
  const _full  : Boolean
) ;
begin
  TGIS_ViewerWnd( oControl ).PrintBmp(_bmp, _full) ;
end;

procedure T_PvlViewerWnd.Print  ;
begin
  TGIS_ViewerWnd( oControl ).Print() ;
end;

procedure T_PvlViewerWnd.Print(
  _printer     : IGIS_Printer
) ;
begin
  TGIS_ViewerWnd( oControl ).Print(_printer) ;
end;

procedure T_PvlViewerWnd.ZoomBy(
  const _zm : Double  ;
  const _x  : Integer ;
  const _y  : Integer
) ;
begin
  TGIS_ViewerWnd( oControl ).ZoomBy( _zm, _x, _y );
end;

{$ENDREGION 'IGIS_ViewerWnd public methods'}

initialization
  RegisterPVLPlatformControl( 'ViewerWnd', T_PvlViewerWnd );

end.

