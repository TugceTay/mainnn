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
  PVL Viewer Window.
}

{$IFDEF DCC}
  unit PVL.GisViewerWnd;
  {$HPPEMIT '#pragma link "PVL.GisViewerWnd"'}
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
    GisTypes,
    GisRendererAbstract,
    GisTypesUI,
    GisInterfaces,
    GisCsSystems,
    GisGraticuleHelper;
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
  ///   PVL viewer intraface
  /// </summary>
  IGIS_PvlViewerWnd = interface( IGIS_PvlControl )
    {$IFDEF DCC}
      ['{DAB18D1A-A82F-4C2C-A0D5-03BEB6FCF00F}']
    {$ENDIF}

    procedure fset_3DMode           ( const _value  : TGIS_Viewer3DMode ) ;
    function  fget_View3D           : Boolean ;
    procedure fset_View3D           ( const _value  : Boolean     ) ;
    function  fget_Viewer3D         : IGIS_Viewer3D ;
    procedure fset_Viewer3D         ( const _viewer : IGIS_Viewer3D ) ;

    function  fget_Graticule        : TGIS_Graticule ;

    // IGIS_ViewerParent events

    function  fget_BeforePaintEvent : TGIS_PaintEvent ;
    procedure fset_BeforePaintEvent ( const _value      : TGIS_PaintEvent
                                    );
    function  fget_AfterPaintEvent  : TGIS_PaintEvent ;
    procedure fset_AfterPaintEvent  ( const _value      : TGIS_PaintEvent
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
    function  fget_PaintExtraEvent  : TGIS_RendererEvent ;
    procedure fset_PaintExtraEvent  ( const _value      : TGIS_RendererEvent
                                    );
    function  fget_BeforeUpdateEvent: TGIS_RendererEvent ;
    procedure fset_BeforeUpdateEvent( const _value      : TGIS_RendererEvent
                                    );
    function  fget_AfterUpdateEvent : TGIS_RendererEvent ;
    procedure fset_AfterUpdateEvent ( const _value      : TGIS_RendererEvent
                                    );

    // IGIS_ViewerWnd events

    function  fget_BusyEvent         : TGIS_BusyEvent ;
    procedure fset_BusyEvent         ( const _value : TGIS_BusyEvent ) ;

    function  fget_ModeChangeEvent   : TGIS_PvlEvent;
    procedure fset_ModeChangeEvent   ( const _value      : TGIS_PvlEvent
                                     );

    function  fget_EditorChangeEvent : TGIS_PvlEvent;
    procedure fset_EditorChangeEvent ( const _value      : TGIS_PvlEvent
                                     );

    function  fget_Mode              : TGIS_ViewerMode ;
    procedure fset_Mode              ( const _value    : TGIS_ViewerMode
                                     ) ;

    function  fget_ExtentChangeEvent : TGIS_PvlEvent ;
    procedure fset_ExtentChangeEvent ( const _value     : TGIS_PvlEvent
                                     ) ;
    function  fget_VisibleExtentChangeEvent
                                     : TGIS_PvlEvent ;
    procedure fset_VisibleExtentChangeEvent
                                     ( const _value     : TGIS_PvlEvent
                                     ) ;
    function  fget_ZoomChangeEvent   : TGIS_PvlEvent ;
    procedure fset_ZoomChangeEvent   ( const _value     : TGIS_PvlEvent

                                     ) ;

    function  fget_TapSimpleEvent    : TGIS_PvlMouseEvent;
    procedure fset_TapSimpleEvent    ( const _value      : TGIS_PvlMouseEvent
                                     );

    function  fget_TapLongEvent      : TGIS_PvlMouseEvent;
    procedure fset_TapLongEvent      ( const _value      : TGIS_PvlMouseEvent
                                     );

    function  fget_TapDoubleEvent    : TGIS_PvlMouseEvent;
    procedure fset_TapDoubleEvent    ( const _value      : TGIS_PvlMouseEvent
                                     );
  end;

  /// <summary>
  ///   PVL GIS Viewer control.
  /// </summary>
  TGIS_PvlViewerWnd = class( TGIS_PvlControl, IGIS_PvlViewerWnd, IGIS_Viewer, IGIS_ViewerWnd )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
    protected
      /// <inheritdoc/>
      procedure initControl       ;                              override;

    private
      function fget_PlatformControl
                                  : IGIS_PvlViewerWnd ;          reintroduce;
      property PlatformControl    : IGIS_PvlViewerWnd
                                    read  fget_PlatformControl;  {$IFDEF OXYGENE}reintroduce;{$ENDIF}

      function fget_PlatformControlWnd
                                  : IGIS_ViewerWnd ;          reintroduce;
      property PlatformControlWnd : IGIS_ViewerWnd
                                    read  fget_PlatformControlWnd;  {$IFDEF OXYGENE}reintroduce;{$ENDIF}

      function fget_PlatformControlVwr
                                  : IGIS_Viewer ;          reintroduce;
      property PlatformControlVwr : IGIS_Viewer
                                    read  fget_PlatformControlVwr;  {$IFDEF OXYGENE}reintroduce;{$ENDIF}

    private
      procedure fset_3DMode             ( const _value  : TGIS_Viewer3DMode ) ;
      function  fget_View3D             : Boolean ;
      procedure fset_View3D             ( const _value  : Boolean     ) ;
      function  fget_Viewer3D           : IGIS_Viewer3D ;
      procedure fset_Viewer3D           ( const _viewer : IGIS_Viewer3D ) ;
      function  fget_Graticule          : TGIS_Graticule;

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
      function  fget_TiledPaint         : Boolean     ;
      procedure fset_TiledPaint         ( const _value  : Boolean    ) ;
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
                                        : Integer  ;
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
      function  fget_PaintExtraEvent      : TGIS_RendererEvent ;
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
      procedure BeginPaintInternal ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure EndPaintInternal ;

      /// <inheritdoc from="IGIS_Viewer"/>
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
      procedure RecalcExtent     ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Reposition       ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateExtent ( const _extent  : TGIS_Extent
                                 ) ; overload;
                                 {$IFNDEF GENDOC} deprecated ; {$ENDIF}

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateExtent ( const _extent : TGIS_Extent ;
                                   const _deep   : Boolean
                                 ) ; overload;
                                 {$IFNDEF GENDOC} deprecated ; {$ENDIF}


      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateWholeMap ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateTopmost ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateBasemap ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateSelection ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateEditor ( const _final   : Boolean
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  FullExtentZoom   : Double ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure FullExtent       ;

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
      function  MapToScreen      ( const _ptg     : TGIS_Point
                                 ) : TPoint ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  MapToScreen3D    ( const _ptg     : TGIS_Point3D
                                 ) : TPoint ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ScreenToMap      ( const _pt      : TPoint
                                 ) : TGIS_Point ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ScreenToMap3D    ( const _pt      : TPoint
                                 ) : TGIS_Point3D ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  MapToScreenEx    ( const _pt      : TGIS_Point
                                 ) : TGIS_Point ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ScreenToMapEx    ( const _pt      : TGIS_Point
                                 ) : TGIS_Point ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  MapToScreenRect  ( const _rct     : TGIS_Extent
                                 ) : TRect ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ScreenToMapRect  ( const _rct     : TRect
                                 ) : TGIS_Extent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  PixelsToTwips    ( const _size    : Integer
                                 ) : Integer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  TwipsToPixels    ( const _size    : Integer
                                 ) : Integer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  TwipsToPoints    ( const _size : Integer
                                 ) : Integer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure MoveViewport     ( var   _dx, _dy : Integer
                                 ) ;

      {$IFDEF JAVA}
      /// <inheritdoc from="IGIS_Viewer"/>
      procedure MoveViewportEx   ( var   _dx, _dy : java.lang.Double
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SetViewport      ( var   _x , _y  : java.lang.Double
                                 ) ;
      {$ELSE}
    
      /// <inheritdoc from="IGIS_Viewer"/>
      procedure MoveViewportEx   ( var   _dx, _dy : Double
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SetViewport      ( var   _x , _y  : Double
                                 ) ;
      {$ENDIF}
      /// <inheritdoc from="IGIS_Viewer"/>
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
                                   {$IFNDEF JAVA} var {$ENDIF}   _ptg     : TGIS_Point3D
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  UnrotatedPoint3D ( const _ptg     : TGIS_Point3D
                                 ) : TGIS_Point3D ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure UnrotatedPoint3D_ref(
                                    {$IFNDEF JAVA} var {$ENDIF}  _ptg     : TGIS_Point3D
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  RotatedExtent    ( const _extent  : TGIS_Extent
                                 ) : TGIS_Extent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  UnrotatedExtent  ( const _extent  : TGIS_Extent
                                 ) : TGIS_Extent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  GetRenderContext : TObject ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure WaitForBackgroundProcesses ;

      {#gendoc:hide:GENPDK} { TODO -cPDK : Verify }
      /// <inheritdoc from="IGIS_Viewer"/>
      procedure WaitForNotBusy   (       _sender  : TObject ;
                                   const _proc    : TGIS_WaitForNotBusyProc
                                 ) ;

      /// <inheritdoc from="IGIS_PvlViewerWnd"/>
      procedure ControlSet3DMode ( const _mode     : TGIS_Viewer3DMode
                                 ) ;

    public // IGIS_ViewerWnd public methods

      /// <inheritdoc from="IGIS_ViewerWnd"/>
      procedure PrintBmp         ( var   _bmp   : TGIS_Bitmap
                                 ) ; overload ;

      /// <inheritdoc from="IGIS_ViewerWnd"/>
      procedure PrintBmp         ( var   _bmp   : TGIS_Bitmap ;
                                   const _full  : Boolean
                                 ) ; overload ;

      /// <inheritdoc from="IGIS_ViewerWnd"/>
      procedure Print            ; overload ;

      /// <inheritdoc from="IGIS_ViewerWnd"/>
      procedure Print            ( _printer     : IGIS_Printer
                                 ) ; overload ;

      /// <inheritdoc from="IGIS_ViewerWnd"/>
      procedure ZoomBy           ( const _zm    : Double  ;
                                   const _x     : Integer ;
                                   const _y     : Integer
                                 ) ;

    private // IGIS_Viewer public properties forced to be hidden

      /// <inheritdoc from="IGIS_Viewer"/>
      property KeepScale  : Boolean        read  fget_KeepScale
                                           write fset_KeepScale ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property DelayedUpdate : Integer     read  fget_DelayedUpdate
                                           write fset_DelayedUpdate ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ProgressiveUpdate : Integer read  fget_ProgressiveUpdate
                                           write fset_ProgressiveUpdate ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property UseAnimations : Boolean     read  fget_UseAnimations
                                           write fset_UseAnimations ;

    public

      /// <summary>
      ///   3D Viewer object. Nil if 3D mode is off.
      /// </summary>
      property Viewer3D : IGIS_Viewer3D        read  fget_Viewer3D
                                               write fset_Viewer3D ;

      /// <summary>
      ///   3D state; If set to true them Viewer is in a 3D mode.
      /// </summary>
      property View3D   : Boolean              read  fget_View3D
                                               write fset_View3D  ;

      /// <summary>
      ///   Graticule object. Set Graticule.Enabled to turn on/off.
      /// </summary>
      property Graticule : TGIS_Graticule
                                               read  fget_Graticule ;


    public // IGIS_Viewer public properties

      /// <inheritdoc from="IGIS_Viewer"/>
      property Copyright    : String       read  fget_Copyright ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property CustomData : TGIS_StringList read fget_CustomData ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property AutoStyle : Boolean         read  fget_AutoStyle
                                           write fset_AutoStyle ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property BigExtent : TGIS_Extent     read  fget_BigExtent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property BigExtentMargin : Integer   read  fget_BigExtentMargin
                                           write fset_BigExtentMargin ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property BusyLevel : Integer         read  fget_BusyLevel ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property BusyText : String           read  fget_BusyText ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Center : TGIS_Point         read  fget_Center
                                           write fset_Center ;

      /// <inheritdoc from="IGIS_Viewer"/>
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
      property FontScale : Integer         read  fget_FontScale
                                           write fset_FontScale ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property FullDrawExtent : TGIS_Extent
                                           read  fget_FullDrawExtent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Hierarchy : IGIS_HierarchyManager
                                           read  fget_Hierarchy      ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property IncrementalPaint : Boolean  read  fget_IncrementalPaint
                                           write fset_IncrementalPaint ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property TiledPaint : Boolean        read  fget_TiledPaint
                                           write fset_TiledPaint
                                           default False           ;

      /// <inheritdoc from="IGIS_Viewer"/>
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
      property LabelsReg : TGIS_LabelsAreaAbstract
                                           read fget_LabelsReg ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Level : Double              read  fget_Level
                                           write fset_Level ;

      /// <inheritdoc from="IGIS_PvlViewerWnd"/>
      property Mode : TGIS_ViewerMode
                                           read  fget_Mode
                                           write fset_Mode ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property MultiUserMode : TGIS_MultiUser
                                           read  fget_MultiUserMode
                                           write fset_MultiUserMode ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property OverlappedExtentMargin : Integer
                                           read  fget_OverlappedExtentMargin
                                           write fset_OverlappedExtentMargin ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property PPI : Integer               read  fget_PPI ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ProjectFile : TGIS_ConfigAbstract
                                           read  fget_ProjectFile
                                           write fset_ProjectFile ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ProjectName : String        read  fget_ProjectName ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property RestrictedDrag : Boolean    read  fget_RestrictedDrag
                                           write fset_RestrictedDrag ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property RestrictedExtent : TGIS_Extent
                                           read  fget_RestrictedExtent
                                           write fset_RestrictedExtent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property RotationAngle : Double      read  fget_RotationAngle
                                           write fset_RotationAngle ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property RotationPoint : TGIS_Point  read  fget_RotationPoint
                                           write fset_RotationPoint ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Scale : Double              read  fget_ScaleAsFloat
                                           write fset_ScaleAsFloat ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ScaleAsFloat : Double       read  fget_ScaleAsFloat
                                           write fset_ScaleAsFloat ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ScaleAsText : String        read  fget_ScaleAsText
                                           write fset_ScaleAsText ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property SelectionGisColor : TGIS_Color
                                           read  fget_SelectionGisColor
                                           write fset_SelectionGisColor ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property SelectionOutlineOnly : Boolean
                                           read  fget_SelectionOutlineOnly
                                           write fset_SelectionOutlineOnly ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property SelectionTransparency : Integer
                                           read  fget_SelectionTransparency
                                           write fset_SelectionTransparency ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property SelectionWidth : Integer    read  fget_SelectionWidth
                                           write fset_SelectionWidth ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property SystemPPI : Integer         read  fget_SystemPPI ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ViewerParent : IGIS_ViewerParent
                                           read  fget_ViewerParent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ViewerParentRoot : IGIS_ViewerParent
                                           read  fget_ViewerParentRoot ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Viewport : TGIS_Point       read  fget_Viewport
                                           write fset_Viewport ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property VisibleExtent : TGIS_Extent read  fget_VisibleExtent
                                           write fset_VisibleExtent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property UseRTree : Boolean          read  fget_UseRTree
                                           write fset_UseRTree ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Zoom : Double               read  fget_Zoom
                                           write fset_Zoom ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ZoomEx : Double             read  fget_ZoomEx
                                           write fset_ZoomEx ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property MasterViewer : IGIS_Viewer  read  fget_MasterViewer
                                           write fset_MasterViewer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property UponDestroy  : Boolean      read  fget_UponDestroy ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property TemporaryScaleInternal : Double
                                           read  fget_TemporaryScaleInternal
                                           write fset_TemporaryScaleInternal ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property TemporaryVisibleExtent : TGIS_Extent
                                           read  fget_TemporaryVisibleExtent
                                           write fset_TemporaryVisibleExtent ;


    published // IGIS_Viewer
      /// <event/>
      /// <inheritdoc from="IGIS_Viewer"/>
      property BusyEvent  : TGIS_BusyEvent
                                           read  fget_BusyEvent
                                           write fset_BusyEvent ;

      /// <event/>
      /// <inheritdoc from="IGIS_Viewer"/>
      property ExtentChangeEvent : TGIS_PvlEvent
                                           read  fget_ExtentChangeEvent
                                           write fset_ExtentChangeEvent ;

      /// <event/>
      /// <inheritdoc from="IGIS_Viewer"/>
      property VisibleExtentChangeEvent : TGIS_PvlEvent
                                           read  fget_VisibleExtentChangeEvent
                                           write fset_VisibleExtentChangeEvent ;

      /// <event/>
      /// <inheritdoc from="IGIS_Viewer"/>
      property ZoomChangeEvent : TGIS_PvlEvent
                                           read  fget_ZoomChangeEvent
                                           write fset_ZoomChangeEvent ;

    published // IGIS_ViewerParent events

      /// <event/>
      /// <summary>
      ///   BeforePaint event. Will be fired before any Paint
      ///   operation.
      /// </summary>
      property BeforePaintEvent : TGIS_PaintEvent
                                           read  fget_BeforePaintEvent
                                           write fset_BeforePaintEvent ;

      /// <event/>
      /// <summary>
      ///   AfterPaint event. Will be fired after any Paint
      ///   operation.
      /// </summary>
      property AfterPaintEvent : TGIS_PaintEvent
                                           read  fget_AfterPaintEvent
                                           write fset_AfterPaintEvent ;

      /// <event/>
      /// <summary>
      ///   BeforePaintRenderer event. Will be fired before any Paint operation.
      ///   Uses renderer object.
      /// </summary>
      property BeforePaintRendererEvent : TGIS_RendererEvent
                                           read  fget_BeforePaintRendererEvent
                                           write fset_BeforePaintRendererEvent ;

      /// <event/>
      /// <summary>
      ///   AfterPaintRenderer event. Will be fired after any Paint operation.
      ///   Uses renderer object.
      /// </summary>
      property AfterPaintRendererEvent : TGIS_RendererEvent
                                           read  fget_AfterPaintRendererEvent
                                           write fset_AfterPaintRendererEvent ;

      /// <event/>
      /// <summary>
      ///   PaintExtra event. Called after renderer.PaintExtra()
      ///   method.
      /// </summary>
      property PaintExtraEvent : TGIS_RendererEvent
                                           read  fget_PaintExtraEvent
                                           write fset_PaintExtraEvent ;

      /// <event/>
      /// <summary>
      ///   BeforeUpdate event. Will be fired before Update
      ///   operation.
      /// </summary>
      property BeforeUpdateEvent : TGIS_RendererEvent
                                           read  fget_BeforeUpdateEvent
                                           write fset_BeforeUpdateEvent ;

      /// <event/>
      /// <summary>
      ///   AfterUpdate event. Will be fired after Update
      ///   operation.
      /// </summary>
      property AfterUpdateEvent : TGIS_RendererEvent
                                           read  fget_AfterUpdateEvent
                                           write fset_AfterUpdateEvent ;


    published // events new for this class

      /// <event/>
      /// <summary>
      ///   Will be fired on viewer mode change.
      /// </summary>
      property ModeChangeEvent   : TGIS_PvlEvent read  fget_ModeChangeEvent
                                                 write fset_ModeChangeEvent ;

      /// <event/>
      /// <summary>
      ///   Will be fired after any change made by editor.
      /// </summary>
      property EditorChangeEvent : TGIS_PvlEvent read  fget_EditorChangeEvent
                                                 write fset_EditorChangeEvent ;


      /// <event/>
      /// <summary>
      ///   TapSimple event. Will be fired upon press down/up.
      /// </summary>
      property TapSimpleEvent    : TGIS_PvlMouseEvent
                                                 read  fget_TapSimpleEvent
                                                 write fset_TapSimpleEvent ;

      /// <event/>
      /// <summary>
      ///   TapLong event. Will be fired upon longer press down.
      /// </summary>
      property TapLongEvent      : TGIS_PvlMouseEvent
                                                 read  fget_TapLongEvent
                                                 write fset_TapLongEvent ;

      /// <event/>
      /// <summary>
      ///   TapDouble event. Will be fired upon double press down/up.
      /// </summary>
      property TapDoubleEvent    : TGIS_PvlMouseEvent
                                                 read  fget_TapDoubleEvent
                                                 write fset_TapDoubleEvent ;
  end;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisRtl
    // ensure that proper implementation files are referenced in Delphi
    {$IFDEF USE_FMX}
      ,FMX.GisPvlViewerWnd
    {$ENDIF}
    {$IFDEF USE_VCL}
      ,VCL.GisPvlViewerWnd
    {$ENDIF}
    ;
{$ENDIF}

{$REGION 'IGIS_ViewerWnd property access routines'}

function TGIS_PvlViewerWnd.fget_View3D
  : Boolean ;
begin
  Result := PlatformControl.fget_View3D ;
end;

procedure TGIS_PvlViewerWnd.fset_View3D(
  const _value: Boolean
) ;
begin
  PlatformControl.fset_View3D( _value ) ;
end;

function TGIS_PvlViewerWnd.fget_Viewer3D
  : IGIS_Viewer3D ;
begin
  Result := PlatformControl.fget_Viewer3D ;
end;

procedure TGIS_PvlViewerWnd.fset_Viewer3D(
  const _viewer: IGIS_Viewer3D
) ;
begin
  PlatformControl.fset_Viewer3D( _viewer ) ;
end;

procedure TGIS_PvlViewerWnd.fset_3DMode(
  const _value: TGIS_Viewer3DMode
) ;
begin
  PlatformControl.fset_3DMode( _value ) ;
end;

{$ENDREGION 'IGIS_ViewerWnd property access routines'}

{$REGION 'IGIS_Viewer property access routines'}

function TGIS_PvlViewerWnd.fget_AutoStyle
  : Boolean;
begin
  Result := PlatformControlVwr.fget_AutoStyle;
end;

procedure TGIS_PvlViewerWnd.fset_AutoStyle(
  const _value : Boolean
);
begin
  PlatformControlVwr.fset_AutoStyle( _value );
end;

function TGIS_PvlViewerWnd.fget_BigExtent
  : TGIS_Extent;
begin
  Result := PlatformControlVwr.fget_BigExtent;
end;

function TGIS_PvlViewerWnd.fget_BigExtentMargin
  : Integer;
begin
  Result := PlatformControlVwr.fget_BigExtentMargin;
end;

procedure TGIS_PvlViewerWnd.fset_BigExtentMargin(
  const _value : Integer
);
begin
  PlatformControlVwr.fset_BigExtentMargin( _value );
end;

function TGIS_PvlViewerWnd.fget_KeepScale
  : Boolean;
begin
  Result := PlatformControlVwr.fget_KeepScale;
end;

procedure TGIS_PvlViewerWnd.fset_KeepScale(
  const _value : Boolean
);
begin
  PlatformControlVwr.fset_KeepScale( _value );
end;

function TGIS_PvlViewerWnd.fget_BusyLevel
  : Integer;
begin
  Result := PlatformControlVwr.fget_BusyLevel;
end;

function TGIS_PvlViewerWnd.fget_BusyText
  : String;
begin
  Result := PlatformControlVwr.fget_BusyText;
end;

function TGIS_PvlViewerWnd.fget_Center
  : TGIS_Point;
begin
  Result := PlatformControlVwr.fget_Center;
end;

procedure TGIS_PvlViewerWnd.fset_Center(
  const _value : TGIS_Point
);
begin
  PlatformControlVwr.fset_Center( _value );
end;

function TGIS_PvlViewerWnd.fget_CenterPtg
  : TGIS_Point;
begin
  Result := PlatformControlVwr.fget_CenterPtg;
end;

procedure TGIS_PvlViewerWnd.fset_CenterPtg(
  const _value : TGIS_Point
);
begin
  PlatformControlVwr.fset_CenterPtg( _value );
end;

function TGIS_PvlViewerWnd.fget_Color
  : TGIS_Color;
begin
  Result := PlatformControlVwr.fget_Color;
end;

procedure TGIS_PvlViewerWnd.fset_Color(
  const _value : TGIS_Color
);
begin
  PlatformControlVwr.fset_Color( _value );
end;

function TGIS_PvlViewerWnd.fget_Copyright
  : String;
begin
  Result := PlatformControlVwr.fget_Copyright;
end;

function TGIS_PvlViewerWnd.fget_CS
  : TGIS_CSCoordinateSystem;
begin
  Result := PlatformControlVwr.fget_CS;
end;

procedure TGIS_PvlViewerWnd.fset_CS(
  const _value : TGIS_CSCoordinateSystem
);
begin
  PlatformControlVwr.fset_CS( _value );
end;

function TGIS_PvlViewerWnd.fget_CustomPPI
  : Integer;
begin
  Result := PlatformControlVwr.fget_CustomPPI;
end;

procedure TGIS_PvlViewerWnd.fset_CustomPPI(
  const _value : Integer
);
begin
  PlatformControlVwr.fset_CustomPPI( _value );
end;

function TGIS_PvlViewerWnd.fget_DelayedUpdate
  : Integer;
begin
  Result := PlatformControlVwr.fget_DelayedUpdate;
end;

procedure TGIS_PvlViewerWnd.fset_DelayedUpdate(
  const _value: Integer
);
begin
  PlatformControlVwr.fset_DelayedUpdate( _value );
end;

function TGIS_PvlViewerWnd.fget_Editor
  : IGIS_Editor;
begin
  Result := PlatformControlVwr.fget_Editor;
end;

procedure TGIS_PvlViewerWnd.fset_Editor(
  const _value : IGIS_Editor
);
begin
  PlatformControlVwr.fset_Editor( _value );
end;

function TGIS_PvlViewerWnd.fget_Extent
  : TGIS_Extent;
begin
  Result := PlatformControlVwr.fget_Extent;
end;

function TGIS_PvlViewerWnd.fget_FileCopyrights: String;
begin
  Result := PlatformControlVwr.fget_FileCopyrights;
end;

function TGIS_PvlViewerWnd.fget_FontScale
  : Integer;
begin
  Result := PlatformControlVwr.fget_FontScale;
end;

procedure TGIS_PvlViewerWnd.fset_FontScale(
  const _value : Integer
);
begin
  PlatformControlVwr.fset_FontScale( _value );
end;

function TGIS_PvlViewerWnd.fget_FullDrawExtent
  : TGIS_Extent;
begin
  Result := PlatformControlVwr.fget_FullDrawExtent;
end;

function TGIS_PvlViewerWnd.fget_Hierarchy
  : IGIS_HierarchyManager;
begin
  Result := PlatformControlVwr.fget_Hierarchy;
end;

function TGIS_PvlViewerWnd.fget_IncrementalPaint
  : Boolean;
begin
  Result := PlatformControlVwr.fget_IncrementalPaint;
end;

function TGIS_PvlViewerWnd.fget_TiledPaint
  : Boolean ;
begin
  Result := PlatformControlVwr.fget_TiledPaint ;
end ;

procedure TGIS_PvlViewerWnd.fset_TiledPaint(
  const _value : Boolean
) ;
begin
  PlatformControlVwr.fset_TiledPaint( _value );
end ;

procedure TGIS_PvlViewerWnd.fset_IncrementalPaint(
  const _value : Boolean
);
begin
  PlatformControlVwr.fset_IncrementalPaint( _value );
end;

function TGIS_PvlViewerWnd.fget_InPaint
  : Boolean;
begin
  Result := PlatformControlVwr.fget_InPaint;
end;

function TGIS_PvlViewerWnd.fget_IsBusy
  : Boolean;
begin
  Result := PlatformControlVwr.fget_IsBusy;
end;

function TGIS_PvlViewerWnd.fget_IsEmpty
  : Boolean;
begin
  Result := PlatformControlVwr.fget_IsEmpty;
end;

function TGIS_PvlViewerWnd.fget_IsLocked
  : Boolean;
begin
  Result := PlatformControlVwr.fget_IsLocked;
end;

function TGIS_PvlViewerWnd.fget_IsTopmost
  : Boolean;
begin
  Result := PlatformControlVwr.fget_IsTopmost;
end;

function TGIS_PvlViewerWnd.fget_Items
  : TGIS_LayerAbstractList;
begin
  Result := PlatformControlVwr.fget_Items;
end;

function TGIS_PvlViewerWnd.fget_LabelsReg
  : TGIS_LabelsAreaAbstract;
begin
  Result := PlatformControlVwr.fget_LabelsReg;
end;

function TGIS_PvlViewerWnd.fget_Level
  : Double;
begin
  Result := PlatformControlVwr.fget_Level;
end;

procedure TGIS_PvlViewerWnd.fset_Level(
  const _value : Double
);
begin
  PlatformControlVwr.fset_Level( _value );
end;

function TGIS_PvlViewerWnd.fget_Mode
  : TGIS_ViewerMode;
begin
  Result := PlatformControl.fget_Mode;
end;

procedure TGIS_PvlViewerWnd.fset_Mode(
  const _value : TGIS_ViewerMode
);
begin
  PlatformControl.fset_Mode( _value );
end;

function TGIS_PvlViewerWnd.fget_MultiUserMode
  : TGIS_MultiUser;
begin
  Result := PlatformControlVwr.fget_MultiUserMode;
end;

procedure TGIS_PvlViewerWnd.fset_MultiUserMode(
  const _value : TGIS_MultiUser
);
begin
  PlatformControlVwr.fset_MultiUserMode( _value );
end;

function TGIS_PvlViewerWnd.fget_CustomData
  : TGIS_StringList;
begin
  Result := PlatformControlVwr.fget_CustomData;
end;

function TGIS_PvlViewerWnd.fget_OverlappedExtentMargin
  : Integer;
begin
  Result := PlatformControlVwr.fget_OverlappedExtentMargin;
end;

procedure TGIS_PvlViewerWnd.fset_OverlappedExtentMargin(
  const _value : Integer
);
begin
  PlatformControlVwr.fset_OverlappedExtentMargin( _value );
end;

function TGIS_PvlViewerWnd.fget_PPI
  : Integer;
begin
  Result := PlatformControlVwr.fget_PPI;
end;

function TGIS_PvlViewerWnd.fget_ProgressiveUpdate
  : Integer;
begin
  Result := PlatformControlVwr.fget_ProgressiveUpdate;
end;

procedure TGIS_PvlViewerWnd.fset_ProgressiveUpdate(
  const _value : Integer
);
begin
  PlatformControlVwr.fset_ProgressiveUpdate( _value );
end;

function TGIS_PvlViewerWnd.fget_ProjectFile
  : TGIS_ConfigAbstract;
begin
  Result := PlatformControlVwr.fget_ProjectFile;
end;

procedure TGIS_PvlViewerWnd.fset_ProjectFile(
  const _value : TGIS_ConfigAbstract
);
begin
  PlatformControlVwr.fset_ProjectFile( _value );
end;

function TGIS_PvlViewerWnd.fget_ProjectName
  : String;
begin
  Result := PlatformControlVwr.fget_ProjectName;
end;

function TGIS_PvlViewerWnd.fget_RestrictedDrag
  : Boolean;
begin
  Result := PlatformControlVwr.fget_RestrictedDrag;
end;

procedure TGIS_PvlViewerWnd.fset_RestrictedDrag(
  const _value : Boolean
);
begin
  PlatformControlVwr.fset_RestrictedDrag( _value );
end;

function TGIS_PvlViewerWnd.fget_RestrictedExtent
  : TGIS_Extent;
begin
  Result := PlatformControlVwr.fget_RestrictedExtent;
end;

procedure TGIS_PvlViewerWnd.fset_RestrictedExtent(
  const _value : TGIS_Extent
);
begin
  PlatformControlVwr.fset_RestrictedExtent( _value );
end;

function TGIS_PvlViewerWnd.fget_RotationAngle
  : Double;
begin
  Result := PlatformControlVwr.fget_RotationAngle;
end;

procedure TGIS_PvlViewerWnd.fset_RotationAngle(
  const _value : Double
);
begin
  PlatformControlVwr.fset_RotationAngle( _value );
end;

function TGIS_PvlViewerWnd.fget_RotationPoint
  : TGIS_Point;
begin
  Result := PlatformControlVwr.fget_RotationPoint;
end;

procedure TGIS_PvlViewerWnd.fset_RotationPoint(
  const _value : TGIS_Point
);
begin
  PlatformControlVwr.fset_RotationPoint( _value );
end;

function TGIS_PvlViewerWnd.fget_ScaleAsFloat
  : Double;
begin
  Result := PlatformControlVwr.fget_ScaleAsFloat;
end;

procedure TGIS_PvlViewerWnd.fset_ScaleAsFloat(
  const _value : Double
);
begin
  PlatformControlVwr.fset_ScaleAsFloat( _value );
end;

function TGIS_PvlViewerWnd.fget_ScaleAsText
  : String;
begin
  Result := PlatformControlVwr.fget_ScaleAsText;
end;

procedure TGIS_PvlViewerWnd.fset_ScaleAsText(
  const _value : String
);
begin
  PlatformControlVwr.fset_ScaleAsText( _value );
end;

function TGIS_PvlViewerWnd.fget_SelectionGisColor
  : TGIS_Color;
begin
  Result := PlatformControlVwr.fget_SelectionGisColor;
end;

procedure TGIS_PvlViewerWnd.fset_SelectionGisColor(
  const _value : TGIS_Color
);
begin
  PlatformControlVwr.fset_SelectionGisColor( _value );
end;

function TGIS_PvlViewerWnd.fget_SelectionOutlineOnly
  : Boolean;
begin
  Result := PlatformControlVwr.fget_SelectionOutlineOnly;
end;

procedure TGIS_PvlViewerWnd.fset_SelectionOutlineOnly(
  const _value : Boolean
);
begin
  PlatformControlVwr.fset_SelectionOutlineOnly( _value );
end;

function TGIS_PvlViewerWnd.fget_SelectionTransparency
  : Integer;
begin
  Result := PlatformControlVwr.fget_SelectionTransparency;
end;

procedure TGIS_PvlViewerWnd.fset_SelectionTransparency(
  const _value : Integer
);
begin
  PlatformControlVwr.fset_SelectionTransparency( _value );
end;

function TGIS_PvlViewerWnd.fget_SelectionWidth
  : Integer;
begin
  Result := PlatformControlVwr.fget_SelectionWidth;
end;

procedure TGIS_PvlViewerWnd.fset_SelectionWidth(
  const _value : Integer
);
begin
  PlatformControlVwr.fset_SelectionWidth( _value );
end;

function TGIS_PvlViewerWnd.fget_SystemPPI
  : Integer;
begin
  Result := PlatformControlVwr.fget_SystemPPI;
end;

function TGIS_PvlViewerWnd.fget_UponDestroy
  : Boolean;
begin
  Result := PlatformControlVwr.fget_UponDestroy;
end;

function TGIS_PvlViewerWnd.fget_UseAnimations
  : Boolean;
begin
  Result := PlatformControlVwr.fget_UseAnimations
end;

procedure TGIS_PvlViewerWnd.fset_UseAnimations(
  const _value : Boolean
);
begin
  PlatformControlVwr.fset_UseAnimations( _value );
end;

function TGIS_PvlViewerWnd.fget_UseRTree
  : Boolean;
begin
  Result := PlatformControlVwr.fget_UseRTree;
end;

procedure TGIS_PvlViewerWnd.fset_UseRTree(
  const _value : Boolean
);
begin
  PlatformControlVwr.fset_UseRTree( _value );
end;

function TGIS_PvlViewerWnd.fget_ViewerParent
  : IGIS_ViewerParent;
begin
  Result := PlatformControlVwr.fget_ViewerParent;
end;

function TGIS_PvlViewerWnd.fget_ViewerParentRoot
  : IGIS_ViewerParent;
begin
  Result := PlatformControlVwr.fget_ViewerParentRoot;
end;

function TGIS_PvlViewerWnd.fget_Viewport
  : TGIS_Point;
begin
  Result := PlatformControlVwr.fget_Viewport;
end;

procedure TGIS_PvlViewerWnd.fset_Viewport(
  const _value : TGIS_Point
);
begin
  PlatformControlVwr.fset_Viewport( _value );
end;

function TGIS_PvlViewerWnd.fget_VisibleExtent
  : TGIS_Extent;
begin
  Result := PlatformControlVwr.fget_VisibleExtent;
end;

procedure TGIS_PvlViewerWnd.fset_VisibleExtent(
  const _value : TGIS_Extent
);
begin
  PlatformControlVwr.fset_VisibleExtent( _value );
end;

function TGIS_PvlViewerWnd.fget_Zoom
  : Double;
begin
  Result := PlatformControlVwr.fget_Zoom;
end;

procedure TGIS_PvlViewerWnd.fset_Zoom(
  const _value : Double
);
begin
  PlatformControlVwr.fset_Zoom( _value );
end;

function TGIS_PvlViewerWnd.fget_ZoomEx
  : Double;
begin
  Result := PlatformControlVwr.fget_ZoomEx;
end;

procedure TGIS_PvlViewerWnd.fset_ZoomEx(
  const _value : Double
);
begin
  PlatformControlVwr.fset_ZoomEx( _value );
end;

function TGIS_PvlViewerWnd.fget_MasterViewer
  : IGIS_Viewer ;
begin
  Result := PlatformControlVwr.fget_MasterViewer ;
end;

procedure TGIS_PvlViewerWnd.fset_MasterViewer(
  const _value : IGIS_Viewer
) ;
begin
  PlatformControlVwr.fset_MasterViewer( _value ) ;
end;

function TGIS_PvlViewerWnd.fget_TemporaryScaleInternal
  : Double;
begin
  Result := PlatformControlVwr.fget_TemporaryScaleInternal;
end;

procedure TGIS_PvlViewerWnd.fset_TemporaryScaleInternal(
  const _value : Double
);
begin
  PlatformControlVwr.fset_TemporaryScaleInternal( _value );
end;

function TGIS_PvlViewerWnd.fget_TemporaryVisibleExtent
  : TGIS_Extent;
begin
  Result := PlatformControlVwr.fget_TemporaryVisibleExtent;
end;

function TGIS_PvlViewerWnd.fget_Graticule
  : TGIS_Graticule;
begin
  Result := PlatformControl.fget_Graticule;
end;

procedure TGIS_PvlViewerWnd.fset_TemporaryVisibleExtent(
  const _value : TGIS_Extent
);
begin
  PlatformControlVwr.fset_TemporaryVisibleExtent( _value );
end;

{$ENDREGION 'IGIS_Viewer property access routines'}

{$REGION 'IGIS_Viewer events access routines'}
function TGIS_PvlViewerWnd.fget_BusyEvent
  : TGIS_BusyEvent;
begin
  Result := PlatformControl.fget_BusyEvent;
end;

procedure TGIS_PvlViewerWnd.fset_BusyEvent(
  const _value : TGIS_BusyEvent
);
begin
  PlatformControl.fset_BusyEvent( _value );
end;

function TGIS_PvlViewerWnd.fget_ExtentChangeEvent
  : TGIS_PvlEvent;
begin
  Result := PlatformControl.fget_ExtentChangeEvent;
end;

procedure TGIS_PvlViewerWnd.fset_ExtentChangeEvent(
  const _value : TGIS_PvlEvent
);
begin
  PlatformControl.fset_ExtentChangeEvent( _value );
end;

function TGIS_PvlViewerWnd.fget_VisibleExtentChangeEvent
  : TGIS_PvlEvent;
begin
  Result := PlatformControl.fget_VisibleExtentChangeEvent;
end;

procedure TGIS_PvlViewerWnd.fset_VisibleExtentChangeEvent(
  const _value : TGIS_PvlEvent
);
begin
 PlatformControl.fset_VisibleExtentChangeEvent( _value );
end;

function TGIS_PvlViewerWnd.fget_ZoomChangeEvent
  : TGIS_PvlEvent;
begin
  Result := PlatformControl.fget_ZoomChangeEvent;
end;

procedure TGIS_PvlViewerWnd.fset_ZoomChangeEvent(
  const _value : TGIS_PvlEvent
);
begin
  PlatformControl.fset_ZoomChangeEvent( _value );
end;

{$ENDREGION 'IGIS_Viewer events access routines'}

{$REGION 'IGIS_ViewerParent events access routines'}

function TGIS_PvlViewerWnd.fget_BeforePaintEvent
  : TGIS_PaintEvent ;
begin
  Result := PlatformControl.fget_BeforePaintEvent ;
end;

procedure TGIS_PvlViewerWnd.fset_BeforePaintEvent(
  const _value : TGIS_PaintEvent
);
begin
  PlatformControl.fset_BeforePaintEvent( _value ) ;
end;

function TGIS_PvlViewerWnd.fget_AfterPaintEvent
  : TGIS_PaintEvent ;
begin
  Result := PlatformControl.fget_AfterPaintEvent ;
end;

procedure TGIS_PvlViewerWnd.fset_AfterPaintEvent(
  const _value : TGIS_PaintEvent
);
begin
  PlatformControl.fset_AfterPaintEvent( _value ) ;
end;

function TGIS_PvlViewerWnd.fget_BeforePaintRendererEvent
  : TGIS_RendererEvent ;
begin
  Result := PlatformControl.fget_BeforePaintRendererEvent ;
end;

procedure TGIS_PvlViewerWnd.fset_BeforePaintRendererEvent(
  const _value : TGIS_RendererEvent
);
begin
  PlatformControl.fset_BeforePaintRendererEvent( _value ) ;
end;

function TGIS_PvlViewerWnd.fget_AfterPaintRendererEvent
  : TGIS_RendererEvent ;
begin
  Result := PlatformControl.fget_AfterPaintRendererEvent ;
end;

procedure TGIS_PvlViewerWnd.fset_AfterPaintRendererEvent(
  const _value : TGIS_RendererEvent
);
begin
  PlatformControl.fset_AfterPaintRendererEvent( _value ) ;
end;

function TGIS_PvlViewerWnd.fget_PaintExtraEvent
  : TGIS_RendererEvent ;
begin
  Result := PlatformControl.fget_PaintExtraEvent ;
end;

procedure TGIS_PvlViewerWnd.fset_PaintExtraEvent(
  const _value : TGIS_RendererEvent
);
begin
  PlatformControl.fset_PaintExtraEvent( _value ) ;
end;

function TGIS_PvlViewerWnd.fget_BeforeUpdateEvent
  : TGIS_RendererEvent ;
begin
  Result := PlatformControl.fget_BeforeUpdateEvent ;
end;

procedure TGIS_PvlViewerWnd.fset_BeforeUpdateEvent(
  const _value : TGIS_RendererEvent
);
begin
  PlatformControl.fset_BeforeUpdateEvent( _value ) ;
end;

function TGIS_PvlViewerWnd.fget_AfterUpdateEvent
  : TGIS_RendererEvent ;
begin
  Result := PlatformControl.fget_AfterUpdateEvent ;
end;

procedure TGIS_PvlViewerWnd.fset_AfterUpdateEvent(
  const _value : TGIS_RendererEvent
);
begin
  PlatformControl.fset_AfterUpdateEvent( _value ) ;
end;

{$ENDREGION 'IGIS_ViewerWnd events access routines'}

{$REGION 'IGIS_ViewerWnd events access routines'}

function TGIS_PvlViewerWnd.fget_ModeChangeEvent
  : TGIS_PvlEvent;
begin
  Result := PlatformControl.fget_ModeChangeEvent ;
end;

procedure TGIS_PvlViewerWnd.fset_ModeChangeEvent(
  const _value : TGIS_PvlEvent
);
begin
  PlatformControl.fset_ModeChangeEvent( _value ) ;
end;

function TGIS_PvlViewerWnd.fget_EditorChangeEvent
  : TGIS_PvlEvent;
begin
  Result := PlatformControl.fget_EditorChangeEvent ;
end;

procedure TGIS_PvlViewerWnd.fset_EditorChangeEvent(
  const _value : TGIS_PvlEvent
);
begin
  PlatformControl.fset_EditorChangeEvent( _value ) ;
end;

function TGIS_PvlViewerWnd.fget_TapSimpleEvent
  : TGIS_PvlMouseEvent;
begin
  Result := PlatformControl.fget_TapSimpleEvent ;
end;

procedure TGIS_PvlViewerWnd.fset_TapSimpleEvent(
  const _value : TGIS_PvlMouseEvent
);
begin
  PlatformControl.fset_TapSimpleEvent( _value ) ;
end;

function TGIS_PvlViewerWnd.fget_TapLongEvent
  : TGIS_PvlMouseEvent;
begin
  Result := PlatformControl.fget_TapLongEvent ;
end;

procedure TGIS_PvlViewerWnd.fset_TapLongEvent(
  const _value : TGIS_PvlMouseEvent
);
begin
  PlatformControl.fset_TapDoubleEvent( _value ) ;
end;

function TGIS_PvlViewerWnd.fget_TapDoubleEvent
  : TGIS_PvlMouseEvent;
begin
  Result := PlatformControl.fget_TapDoubleEvent ;
end;

procedure TGIS_PvlViewerWnd.fset_TapDoubleEvent(
  const _value : TGIS_PvlMouseEvent
);
begin
  PlatformControl.fset_TapDoubleEvent( _value ) ;
end;

{$ENDREGION 'IGIS_ViewerWnd events access routines'}

{$REGION 'IGIS_Viewer public methods'}

function TGIS_PvlViewerWnd.ChangeHash
  : Int64;
begin
  Result := PlatformControlVwr.ChangeHash;
end;

procedure TGIS_PvlViewerWnd.Subscribe(
  const _control : IGIS_Subscribe
);
begin
  PlatformControlVwr.Subscribe( _control );
end;

procedure TGIS_PvlViewerWnd.UnSubscribe(
  const _control : IGIS_Subscribe
);
begin
  PlatformControlVwr.UnSubscribe( _control );
end;

procedure TGIS_PvlViewerWnd.NotifySubscribers(
  const _event   : Integer;
  const _context : TObject
);
begin
  PlatformControlVwr.NotifySubscribers( _event, _context );
end;

function TGIS_PvlViewerWnd.NotifyPaintException(
  const _message   : String;
  const _exception : Exception
) : Boolean;
begin
  Result := PlatformControlVwr.NotifyPaintException( _message, _exception );
end;

procedure TGIS_PvlViewerWnd.Lock;
begin
  PlatformControlVwr.Lock;
end;

procedure TGIS_PvlViewerWnd.Unlock;
begin
  PlatformControlVwr.Unlock;
end;

procedure TGIS_PvlViewerWnd.Unlock(
  const _redraw : Boolean
);
begin
  PlatformControlVwr.Unlock( _redraw );
end;

procedure TGIS_PvlViewerWnd.Interrupt;
begin
  PlatformControlVwr.Interrupt;
end;

function TGIS_PvlViewerWnd.Interrupted
  : Boolean ;
begin
  Result := PlatformControlVwr.Interrupted;
end;

function TGIS_PvlViewerWnd.HourglassActive
  : Boolean;
begin
  Result := PlatformControlVwr.HourglassActive;
end;

procedure TGIS_PvlViewerWnd.HourglassPrepare;
begin
  PlatformControlVwr.HourglassPrepare;
end;

procedure TGIS_PvlViewerWnd.HourglassRelease;
begin
  PlatformControlVwr.HourglassRelease;
end;

function TGIS_PvlViewerWnd.HourglassShake
  : Boolean;
begin
  Result := PlatformControlVwr.HourglassShake;
end;

procedure TGIS_PvlViewerWnd.HourglassRestart;
begin
  PlatformControlVwr.HourglassRestart;
end;

procedure TGIS_PvlViewerWnd.BusyPrepare(
  _sender : TObject;
  _text   : String
);
begin
  PlatformControlVwr.BusyPrepare( _sender, _text );
end;


procedure TGIS_PvlViewerWnd.BusyRelease(
  _sender: TObject
);
begin
  PlatformControlVwr.BusyRelease( _sender );
end;

procedure TGIS_PvlViewerWnd.BusyShake(
      _sender : TObject;
      _pos,
      _end    : Int64;
  var _abort : Boolean
);
begin
  PlatformControlVwr.BusyShake( _sender, _pos, _end, _abort );
end;

{$IFDEF OXYGENE}
  procedure TGIS_PvlViewerWnd.RaiseBusyEvent(
        _sender : TObject;
        _e      : TGIS_BusyEventArgs
  );
  begin
    PlatformControlVwr.RaiseBusyEvent( _sender, _e );
  end;

  procedure TGIS_PvlViewerWnd.RaiseHelpEvent(
    _sender : TObject;
    _e      : TGIS_HelpEventArgs
  );
  begin
    PlatformControlVwr.RaiseHelpEvent( _sender, _e );
  end;
{$ELSE}
  procedure TGIS_PvlViewerWnd.RaiseBusyEvent(
        _sender : TObject;
        _pos,
        _end    : Int64;
    var _abort  : Boolean);
  begin
    PlatformControlVwr.RaiseBusyEvent( _sender, _pos, _end, _abort );
  end;

  procedure TGIS_PvlViewerWnd.RaiseHelpEvent(
    _sender : TObject;
    _name   : String
  );
  begin
    PlatformControlVwr.RaiseHelpEvent( _sender, _name );
  end;
{$ENDIF}

function TGIS_PvlViewerWnd.AssignedBusyEvent
  : TGIS_BusyEvent;
begin
//?
end;

function TGIS_PvlViewerWnd.AssignedHelpEvent
  : TGIS_HelpEvent;
begin
//?
end;

function TGIS_PvlViewerWnd.StorePaintState
  : TObject;
begin
  Result := PlatformControlVwr.StorePaintState;
end;

procedure TGIS_PvlViewerWnd.RestorePaintState(
  var _state: TObject
);
begin
  PlatformControlVwr.RestorePaintState( _state );
end;

procedure TGIS_PvlViewerWnd.BeginPaintInternal;
begin
  PlatformControlVwr.BeginPaintInternal;
end;

procedure TGIS_PvlViewerWnd.EndPaintInternal;
begin
  PlatformControlVwr.EndPaintInternal;
end;

function TGIS_PvlViewerWnd.SynchronizePaint(
  const _interrupt : Boolean
) : Boolean;
begin
  Result := PlatformControlVwr.SynchronizePaint( _interrupt );
end;

procedure TGIS_PvlViewerWnd.ReParentLock;
begin
  PlatformControlVwr.ReParentLock;
end;

procedure TGIS_PvlViewerWnd.ReParentUnlock;
begin
  PlatformControlVwr.ReParentUnlock;
end;

function TGIS_PvlViewerWnd.ReParent(
  const _parent : IGIS_ViewerParent
) : IGIS_ViewerParent;
begin
  Result := PlatformControlVwr.ReParent( _parent );
end;

function TGIS_PvlViewerWnd.AttachLayer(
  const _layer : TGIS_LayerAbstract
) : IGIS_Viewer;
begin
  PlatformControlVwr.AttachLayer( _layer );
end;

procedure TGIS_PvlViewerWnd.Open(
  const _path : String
);
begin
  PlatformControlVwr.Open( _path );
end;

procedure TGIS_PvlViewerWnd.Open(
  const _path   : String;
  const _strict : Boolean
);
begin
  PlatformControlVwr.Open( _path, _strict );
end;

procedure TGIS_PvlViewerWnd.OpenEx(
  const _configFile : TGIS_ConfigAbstract;
  const _path       : String
);
begin
  PlatformControlVwr.OpenEx( _configFile, _path );
end;

procedure TGIS_PvlViewerWnd.OpenEx(
  const _configFile : TGIS_ConfigAbstract;
  const _path       : String;
  const _strict     : Boolean
);
begin
  PlatformControlVwr.OpenEx( _configFile, _path, _strict );
end;

procedure TGIS_PvlViewerWnd.Close;
begin
  PlatformControlVwr.Close;
end;

procedure TGIS_PvlViewerWnd.ReadConfig;
begin
  PlatformControlVwr.ReadConfig;
end;

procedure TGIS_PvlViewerWnd.RereadConfig;
begin
  PlatformControlVwr.RereadConfig;
end;

procedure TGIS_PvlViewerWnd.WriteConfig;
begin
  PlatformControlVwr.WriteConfig;
end;

procedure TGIS_PvlViewerWnd.Add(
  const _layer : TGIS_LayerAbstract
);
begin
  PlatformControlVwr.Add( _layer );
end;

function TGIS_PvlViewerWnd.Get(
  const _name : String
) : TGIS_LayerAbstract;
begin
  Result := PlatformControlVwr.Get( _name );
end;

procedure TGIS_PvlViewerWnd.Delete(
  const _name: String
);
begin
  PlatformControlVwr.Delete( _name );
end;

procedure TGIS_PvlViewerWnd.AddHierarchy;
begin
  PlatformControlVwr.AddHierarchy;
end;

procedure TGIS_PvlViewerWnd.Draw(
  const _renderer : TObject;
  const _mode     : TGIS_DrawMode
);
begin
  PlatformControlVwr.Draw( _renderer, _mode );
end;

function TGIS_PvlViewerWnd.GetGrid(
  const _extent : TGIS_Extent;
  const _grid   : TGIS_GridArray
) : Boolean;
begin
  Result := PlatformControlVwr.GetGrid( _extent, _grid );
end;

procedure TGIS_PvlViewerWnd.RevertAll;
begin
  PlatformControlVwr.RevertAll;
end;


procedure TGIS_PvlViewerWnd.SaveProject;
begin
  PlatformControlVwr.SaveProject;
end;

procedure TGIS_PvlViewerWnd.SaveProject(
  const _relativepath : Boolean
);
begin
  PlatformControlVwr.SaveProject( _relativepath );
end;

procedure TGIS_PvlViewerWnd.SaveProjectAs(
  const _path : String
);
begin
  PlatformControlVwr.SaveProjectAs( _path );
end;

procedure TGIS_PvlViewerWnd.SaveProjectAs(
  const _path         : String;
  const _relativepath : Boolean
);
begin
  PlatformControlVwr.SaveProjectAs( _path, _relativepath );
end;

procedure TGIS_PvlViewerWnd.SaveProjectAsEx(
  const _configFile : TGIS_ConfigAbstract;
  const _path       : String
);
begin
  PlatformControlVwr.SaveProjectAsEx( _configFile, _path );
end;

procedure TGIS_PvlViewerWnd.SaveProjectAsEx(
  const _configFile   : TGIS_ConfigAbstract;
  const _path         : String;
  const _relativepath : Boolean
);
begin
  PlatformControlVwr.SaveProjectAsEx( _configFile, _path, _relativepath );
end;

procedure TGIS_PvlViewerWnd.SaveData;
begin
  PlatformControlVwr.SaveData;
end;

procedure TGIS_PvlViewerWnd.SaveAll;
begin
  PlatformControlVwr.SaveAll;
end;

function TGIS_PvlViewerWnd.MustSave
  : Boolean;
begin
  Result := PlatformControlVwr.MustSave;
end;

procedure TGIS_PvlViewerWnd.MarkModified;
begin
  PlatformControlVwr.MarkModified;
end;

procedure TGIS_PvlViewerWnd.RecalcExtent;
begin
  PlatformControlVwr.RecalcExtent;
end;

procedure TGIS_PvlViewerWnd.Reposition;
begin
  PlatformControlVwr.Reposition;
end;

procedure TGIS_PvlViewerWnd.InvalidateExtent(
  const _extent : TGIS_Extent
);
begin
  PlatformControlVwr.InvalidateExtent( _extent );
end;

procedure TGIS_PvlViewerWnd.InvalidateExtent(
  const _extent : TGIS_Extent;
  const _deep   : Boolean
);
begin
  PlatformControlVwr.InvalidateExtent( _extent, _deep );
end;

procedure TGIS_PvlViewerWnd.InvalidateWholeMap;
begin
  PlatformControlVwr.InvalidateWholeMap;
end;

procedure TGIS_PvlViewerWnd.InvalidateTopmost;
begin
  PlatformControlVwr.InvalidateTopmost;
end;

procedure TGIS_PvlViewerWnd.InvalidateBasemap;
begin
  PlatformControlVwr.InvalidateBasemap;
end;

procedure TGIS_PvlViewerWnd.InvalidateSelection;
begin
  PlatformControlVwr.InvalidateSelection;
end;

procedure TGIS_PvlViewerWnd.InvalidateEditor(
  const _final : Boolean
);
begin
  PlatformControlVwr.InvalidateEditor( _final );
end;

function TGIS_PvlViewerWnd.FullExtentZoom
  : Double;
begin
  Result := PlatformControlVwr.FullExtentZoom
end;

procedure TGIS_PvlViewerWnd.FullExtent;
begin
  PlatformControlVwr.FullExtent;
end;

function TGIS_PvlViewerWnd.Locate(
  const _ptg  : TGIS_Point;
  const _prec : Double
) : TGIS_ShapeAbstract;
begin
  Result := PlatformControlVwr.Locate( _ptg, _prec );
end;

function TGIS_PvlViewerWnd.Locate(
  const _ptg     : TGIS_Point;
  const _prec    : Double;
  const _visible : Boolean
) : TGIS_ShapeAbstract;
begin
  Result := PlatformControlVwr.Locate( _ptg, _prec, _visible);
end;

function TGIS_PvlViewerWnd.Locate(
  const _pt   : TPoint;
  const _prec : Integer
) : TGIS_ShapeAbstract;
begin
  Result := PlatformControlVwr.Locate( _pt, _prec );
end;

function TGIS_PvlViewerWnd.LocateEx(
  const _ptg     : TGIS_Point;
  const _prec    : Double;
  const _visible : Boolean
) : TGIS_ShapeAbstractList;
begin
  Result := PlatformControlVwr.LocateEx( _ptg, _prec, _visible );
end;

function TGIS_PvlViewerWnd.MapToScreen(
  const _ptg : TGIS_Point
) : TPoint;
begin
  Result := PlatformControlVwr.MapToScreen( _ptg );
end;

function TGIS_PvlViewerWnd.MapToScreen3D(
  const _ptg : TGIS_Point3D
) : TPoint;
begin
  Result := PlatformControlVwr.MapToScreen3D( _ptg );
end;

function TGIS_PvlViewerWnd.ScreenToMap(
  const _pt : TPoint
) : TGIS_Point;
begin
  Result := PlatformControlVwr.ScreenToMap( _pt );
end;

function TGIS_PvlViewerWnd.ScreenToMap3D(
  const _pt : TPoint
) : TGIS_Point3D;
begin
  Result := PlatformControlVwr.ScreenToMap3D( _pt );
end;

function TGIS_PvlViewerWnd.MapToScreenEx(
  const _pt : TGIS_Point
) : TGIS_Point;
begin
  Result := PlatformControlVwr.MapToScreenEx( _pt );
end;

function TGIS_PvlViewerWnd.ScreenToMapEx(
  const _pt : TGIS_Point
) : TGIS_Point;
begin
  Result := PlatformControlVwr.ScreenToMapEx( _pt );
end;

function TGIS_PvlViewerWnd.MapToScreenRect(
  const _rct : TGIS_Extent
) : TRect;
begin
  Result := PlatformControlVwr.MapToScreenRect( _rct );
end;

function TGIS_PvlViewerWnd.ScreenToMapRect(
  const _rct : TRect
) : TGIS_Extent;
begin
  Result := PlatformControlVwr.ScreenToMapRect( _rct );
end;

function TGIS_PvlViewerWnd.PixelsToTwips(
  const _size : Integer
) : Integer;
begin
  Result := PlatformControlVwr.PixelsToTwips( _size );
end;

function TGIS_PvlViewerWnd.TwipsToPixels(
  const _size : Integer
) : Integer;
begin
  Result := PlatformControlVwr.TwipsToPixels( _size );
end;

function TGIS_PvlViewerWnd.TwipsToPoints(
  const _size : Integer
): Integer;
begin
  Result := PlatformControlVwr.TwipsToPoints( _size );
end;

procedure TGIS_PvlViewerWnd.MoveViewport(
  var _dx, _dy: Integer
);
begin
  PlatformControlVwr.MoveViewport( _dx, _dy );
end;
{$IFDEF JAVA}
procedure TGIS_PvlViewerWnd.MoveViewportEx(
  var _dx, _dy: java.lang.Double
);
begin
  PlatformControlVwr.MoveViewportEx( _dx, _dy );
end;

procedure TGIS_PvlViewerWnd.SetViewport(
  var _x, _y: java.lang.Double
);
begin
  PlatformControlVwr.SetViewport( _x, _y );
end;
{$ELSE}
procedure TGIS_PvlViewerWnd.MoveViewportEx(
  var _dx, _dy: Double
);
begin
  PlatformControlVwr.MoveViewportEx( _dx, _dy );
end;

procedure TGIS_PvlViewerWnd.SetViewport(
  var _x, _y: Double
);
begin
  PlatformControlVwr.SetViewport( _x, _y );
end;
{$ENDIF}
procedure TGIS_PvlViewerWnd.CenterViewport(
  const _ptg : TGIS_Point
);
begin
  PlatformControlVwr.CenterViewport( _ptg );
end;

procedure TGIS_PvlViewerWnd.SetCSByWKT(
  const _wkt : String
);
begin
  PlatformControlVwr.SetCSByWKT( _wkt );
end;

procedure TGIS_PvlViewerWnd.SetCSByEPSG(
  const _epsg : Integer
);
begin
  PlatformControlVwr.SetCSByEPSG( _epsg );
end;

procedure TGIS_PvlViewerWnd.SetCSByWKTFile(
  const _path : String
);
begin
  PlatformControlVwr.SetCSByWKTFile( _path );
end;

function TGIS_PvlViewerWnd.RotatedPoint(
  const _ptg : TGIS_Point
) : TGIS_Point;
begin
  Result := PlatformControlVwr.RotatedPoint( _ptg );
end;

function TGIS_PvlViewerWnd.UnrotatedPoint(
  const _ptg : TGIS_Point
) : TGIS_Point;
begin
  Result := PlatformControlVwr.UnrotatedPoint( _ptg );
end;

function TGIS_PvlViewerWnd.RotatedPoint3D(
  const _ptg : TGIS_Point3D
) : TGIS_Point3D;
begin
  Result := PlatformControlVwr.RotatedPoint3D( _ptg );
end;

procedure TGIS_PvlViewerWnd.RotatedPoint3D_ref(
  var _ptg : TGIS_Point3D
);
begin
  PlatformControlVwr.RotatedPoint3D_ref( _ptg );
end;

function TGIS_PvlViewerWnd.UnrotatedPoint3D(
  const _ptg : TGIS_Point3D
) : TGIS_Point3D;
begin
  Result := PlatformControlVwr.UnrotatedPoint3D( _ptg );
end;

procedure TGIS_PvlViewerWnd.UnrotatedPoint3D_ref(
  var _ptg : TGIS_Point3D
);
begin
  PlatformControlVwr.UnrotatedPoint3D_ref( _ptg );
end;

function TGIS_PvlViewerWnd.RotatedExtent(
  const _extent : TGIS_Extent
) : TGIS_Extent;
begin
  Result := PlatformControlVwr.RotatedExtent( _extent );
end;

function TGIS_PvlViewerWnd.UnrotatedExtent(
  const _extent : TGIS_Extent
) : TGIS_Extent;
begin
  Result := PlatformControlVwr.UnrotatedExtent( _extent );
end;

function TGIS_PvlViewerWnd.GetRenderContext
  : TObject;
begin
  Result := PlatformControlVwr.GetRenderContext;
end;

procedure TGIS_PvlViewerWnd.WaitForBackgroundProcesses;
begin
  PlatformControlVwr.WaitForBackgroundProcesses; //?
end;

procedure TGIS_PvlViewerWnd.WaitForNotBusy(
        _sender : TObject;
  const _proc   : TGIS_WaitForNotBusyProc
);
begin
  PlatformControlVwr.WaitForNotBusy( _sender, _proc ); //?
end;

procedure TGIS_PvlViewerWnd.ControlSet3DMode(
  const _mode: TGIS_Viewer3DMode
) ;
begin
  fset_3DMode( _mode ) ;
end;

{$ENDREGION 'IGIS_Viewer public methods'}

{$REGION 'TGIS_PvlViewerWnd specific'}

procedure TGIS_PvlViewerWnd.initControl;
begin
  inherited;
end;


procedure TGIS_PvlViewerWnd.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.Platform.CreateObject(self, 'ViewerWnd');
end;

function TGIS_PvlViewerWnd.fget_PlatformControl
  : IGIS_PvlViewerWnd;
begin
  Result := oPlatform as IGIS_PvlViewerWnd;
end;

function TGIS_PvlViewerWnd.fget_PlatformControlVwr
  : IGIS_Viewer;
begin
  Result := oPlatform as IGIS_Viewer;
end;

function TGIS_PvlViewerWnd.fget_PlatformControlWnd
  : IGIS_ViewerWnd;
begin
  Result := oPlatform as IGIS_ViewerWnd;
end;

{$ENDREGION 'TGIS_PvlViewerWnd specific'}

{$REGION 'IGIS_ViewerWnd specific'}

procedure TGIS_PvlViewerWnd.PrintBmp(
  var _bmp  : TGIS_Bitmap
) ;
begin
  PlatformControlWnd.PrintBmp(_bmp) ;
end;

procedure TGIS_PvlViewerWnd.PrintBmp(
  var _bmp  : TGIS_Bitmap ;
  const _full  : Boolean
) ;
begin
  PlatformControlWnd.PrintBmp(_bmp, _full) ;
end;

procedure TGIS_PvlViewerWnd.Print ;
begin
  PlatformControlWnd.Print() ;
end;

procedure TGIS_PvlViewerWnd.Print(
  _printer     : IGIS_Printer
) ;
begin
  PlatformControlWnd.Print(_printer) ;
end;

procedure TGIS_PvlViewerWnd.ZoomBy(
  const _zm : Double  ;
  const _x  : Integer ;
  const _y  : Integer
) ;
begin
  PlatformControlWnd.ZoomBy(_zm, _x, _y) ;
end;

{$ENDREGION 'IGIS_ViewerWnd specific'}

//==================================== END =====================================
end.
