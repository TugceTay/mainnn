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
  VCL implementation of TGIS_PvlViewerBmp
}

unit VCL.GisPvlViewerBmp;

interface

uses
  System.Types, System.Classes,
  System.SysUtils,
  System.UiTypes,
  VCL.Graphics,


  VCL.GisFramework,
  VCL.GisPvl,
  VCL.GisViewerBmp,
  PVL.GisPvl,
  PVL.GisViewerBmp,

  GisRendererAbstract,
  GisTypes,
  GisTypesUI,
  GisInterfaces,
  GisCsSystems;

//##############################################################################
implementation

type
  T_PvlViewerBmp = class( TGIS_PvlObjectVcl, IGIS_PvlViewerBmp, IGIS_Viewer, IGIS_ViewerBmp )
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;

    private
      /// <summary>
      ///   BeforePaint event. Will be fired before any Draw operation.
      /// </summary>
      FOnBeforePaint  : TGIS_PaintEvent ;

      /// <summary>
      ///   BeforePaintRenderer event. Will be fired before any Draw operation.
      /// </summary>
      FOnBeforePaintRenderer : TGIS_RendererEvent ;

      /// <summary>
      ///   AfterPaint event. Will be fired after any Draw operation.
      /// </summary>
      FOnAfterPaint   : TGIS_PaintEvent ;

      /// <summary>
      ///   AfterPaintRenderer event. Will be fired after any Draw operation.
      /// </summary>
      FOnAfterPaintRenderer : TGIS_RendererEvent ;

      /// <summary>
      ///   PaintExtra event. Will be fired after real draw.
      /// </summary>
      FOnPaintExtra    : TGIS_RendererEvent ;

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
      function  fget_Level              : Double ;
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

//    protected
//      function  fget_SelectionColor     : TAlphaColor ;
//      procedure fset_SelectionColor     ( const _value : TAlphaColor ) ;

    protected // IGIS_ViewerBmp property access routines
      function  fget_Bitmap             : TObject ;
      function  fget_GIS_Bitmap         : TGIS_Bitmap ;
      function  fget_TileRect           : TRect ;
      procedure fset_TileRect           ( const _val : TRect ) ;
      function  fget_Width              : Integer ;
      function  fget_Height             : Integer ;

      function  fget_KeepContextInternal: Boolean ;
      procedure fset_KeepContextInternal( const _val   : Boolean        ) ;
      function  fget_ContextInternal    : TObject ;


    private // TGIS_ViewerBmp property access routines
      procedure fset_Renderer           ( const _value :
                                              TGIS_RendererAbstract ) ;
      function  fget_Renderer           : TGIS_RendererAbstract ;
      procedure fset_DrawBasemapOnly    ( const _value :
                                                Boolean ) ;
      function  fget_DrawBasemapOnly    : Boolean ;
      procedure fset_TilePicture        ( const _value :
                                                TGIS_Bitmap ) ;
      function  fget_TilePicture        : TGIS_Bitmap ;
      function  fget_BusyEvent          : TGIS_BusyEvent ;
      procedure fset_BusyEvent          ( const _value : TGIS_BusyEvent ) ;
      function  fget_ExtentChangeEvent  : TGIS_PvlEvent ;
      procedure fset_ExtentChangeEvent  ( const _value : TGIS_PvlEvent ) ;
      function  fget_VisibleExtentChangeEvent
                                        : TGIS_PvlEvent ;
      procedure fset_VisibleExtentChangeEvent
                                        ( const _value : TGIS_PvlEvent ) ;
      function  fget_ZoomChangeEvent    : TGIS_PvlEvent ;
      procedure fset_ZoomChangeEvent    ( const _value : TGIS_PvlEvent ) ;

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

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure RaiseBusyEvent   (       _sender  : TObject ;
                                         _pos     : Int64 ;
                                         _end     : Int64 ;
                                   var   _abort   : Boolean
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure RaiseHelpEvent   (       _sender  : TObject ;
                                         _name    : String
                                 ) ;

      {#gendoc:hide:GENPDK}
      /// <inheritdoc from="IGIS_Viewer"/>
      function  AssignedBusyEvent: TGIS_BusyEvent;

      {#gendoc:hide:GENPDK}
      /// <inheritdoc from="IGIS_Viewer"/>
      function  AssignedHelpEvent: TGIS_HelpEvent;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  StorePaintState  : TObject ;

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

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure MoveViewportEx   ( var   _dx, _dy : Double
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SetViewport      ( var   _x , _y  : Double
                                 ) ;

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
                                   var   _ptg     : TGIS_Point3D
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  UnrotatedPoint3D ( const _ptg     : TGIS_Point3D
                                 ) : TGIS_Point3D ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure UnrotatedPoint3D_ref(
                                   var  _ptg      : TGIS_Point3D
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

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure WaitForNotBusy   (       _sender   : TObject ;
                                   const _proc     : TGIS_WaitForNotBusyProc
                                 ) ;

    public // public methods of IGIS_ViewerBmp

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ZoomIn        ;


      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ZoomOut       ;


      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Clear         ;


      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Draw             ; overload;


      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ScrollPgUp    ;


      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ScrollPgDn    ;


      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ScrollPgRight ;


      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ScrollPgLeft  ;


      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ScrollUp      ;


      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ScrollDn      ;


      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ScrollRight   ;


      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ScrollLeft    ;

      /// <inheritdoc from="IGIS_ViewerBmp"/>
      procedure SetSize( const _width  : Integer ;
                         const _height : Integer
                      ) ;
    public
      /// <summary>
      ///   Print the current content on a bitmap.
      ///   Print area will match the current VisibleExtent of the control.
      /// </summary>
      /// <param name="_bmp">
      ///   bitmap on which the drawing will be performed
      /// </param>
      procedure PrintBmp         ( var   _bmp   : TGIS_Bitmap
                                 ) ; overload ; virtual;

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
      procedure PrintBmp         ( var   _bmp   : TGIS_Bitmap ;
                                   const _full    : Boolean
                                 ) ; overload ; virtual;

    private // IGIS_Viewer public properties forced to be hidden

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   Always False on TGIS_ViewerBmp.
      /// </remarks>
      property KeepScale    : Boolean      read  fget_KeepScale
                                           write fset_KeepScale  ;

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
      property Copyright    : String           read  fget_Copyright      ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property CustomData : TGIS_StringList    read  fget_CustomData     ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property AutoStyle : Boolean             read  fget_AutoStyle
                                               write fset_AutoStyle      ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property BigExtent    : TGIS_Extent      read  fget_BigExtent      ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property BigExtentMargin  : Integer      read  fget_BigExtentMargin
                                               write fset_BigExtentMargin
                                               default -10               ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property BusyLevel    : Integer          read  fget_BusyLevel      ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property BusyText     : String           read  fget_BusyText       ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Center       : TGIS_Point       read  fget_Center
                                               write fset_Center         ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property CenterPtg    : TGIS_Point       read  fget_CenterPtg
                                               write fset_CenterPtg      ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Color            : TGIS_Color   read  fget_Color
                                               write fset_Color         ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property CS                : TGIS_CSCoordinateSystem
                                               read  fget_CS
                                               write fset_CS             ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property CustomPPI        : Integer      read  fget_CustomPPI
                                               write fset_CustomPPI
                                               default 0                 ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Editor : IGIS_Editor            read  fget_Editor
                                               write fset_Editor ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Extent       : TGIS_Extent      read  fget_Extent         ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property FileCopyrights : String         read  fget_FileCopyrights ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property FontScale        : Integer      read  fget_FontScale
                                               write fset_FontScale
                                               default 100               ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property FullDrawExtent : TGIS_Extent    read  fget_FullDrawExtent ;


      /// <inheritdoc from="IGIS_Viewer"/>
      property Hierarchy : IGIS_HierarchyManager
                                               read  fget_Hierarchy      ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property IncrementalPaint : Boolean      read  fget_IncrementalPaint
                                               write fset_IncrementalPaint
                                               default True             ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   This property has no meaning on TGIS_ViewerBmp.
      /// </remarks>
      property TiledPaint   : Boolean          read  fget_TiledPaint
                                               write fset_TiledPaint     ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property InPaint      : Boolean          read  fget_InPaint        ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property IsBusy       : Boolean          read  fget_IsBusy         ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property IsEmpty      : Boolean          read  fget_IsEmpty        ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property IsLocked     : Boolean          read  fget_IsLocked       ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property IsTopmost    : Boolean          read  fget_IsTopmost      ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Items        : TGIS_LayerAbstractList
                                               read  fget_Items          ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property LabelsReg    : TGIS_LabelsAreaAbstract
                                               read  fget_LabelsReg       ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Level : Double                  read  fget_Level
                                               write fset_Level ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property MultiUserMode : TGIS_MultiUser  read  fget_MultiUserMode
                                               write fset_MultiUserMode
                                               default TGIS_MultiUser.Default;

      /// <inheritdoc from="IGIS_Viewer"/>
      property OverlappedExtentMargin : Integer
                                       read  fget_OverlappedExtentMargin
                                       write fset_OverlappedExtentMargin ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property PPI          : Integer          read  fget_PPI            ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ProjectFile  : TGIS_ConfigAbstract
                                               read  fget_ProjectFile    ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ProjectName  : String           read  fget_ProjectName    ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property RestrictedDrag   : Boolean      read  fget_RestrictedDrag
                                               write fset_RestrictedDrag
                                               default True             ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property RestrictedExtent : TGIS_Extent  read  fget_RestrictedExtent
                                               write fset_RestrictedExtent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property RotationAngle : Double          read  fget_RotationAngle
                                               write fset_RotationAngle  ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property RotationPoint : TGIS_Point      read  fget_RotationPoint
                                               write fset_RotationPoint  ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Scale        : Double           read  fget_ScaleAsFloat
                                               write fset_ScaleAsFloat   ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ScaleAsFloat : Double           read  fget_ScaleAsFloat
                                               write fset_ScaleAsFloat   ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ScaleAsText  : String           read  fget_ScaleAsText
                                               write fset_ScaleAsText    ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property SelectionGisColor : TGIS_Color  read  fget_SelectionGisColor
                                               write fset_SelectionGisColor ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property SelectionOutlineOnly : Boolean  read  fget_SelectionOutlineOnly
                                               write fset_SelectionOutlineOnly
                                               default False            ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property SelectionTransparency : Integer read  fget_SelectionTransparency
                                               write fset_SelectionTransparency
                                               default 60               ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property SelectionWidth   : Integer      read  fget_SelectionWidth
                                               write fset_SelectionWidth
                                               default 100              ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property SystemPPI    : Integer          read fget_SystemPPI ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property TemporaryVisibleExtent : TGIS_Extent
                                               read  fget_TemporaryVisibleExtent
                                               write fset_TemporaryVisibleExtent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ViewerParent : IGIS_ViewerParent
                                               read  fget_ViewerParent     ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ViewerParentRoot : IGIS_ViewerParent
                                               read  fget_ViewerParentRoot ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Viewport     : TGIS_Point       read  fget_Viewport
                                               write fset_Viewport       ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property VisibleExtent : TGIS_Extent     read  fget_VisibleExtent
                                               write fset_VisibleExtent  ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property UseRTree         : Boolean      read  fget_UseRTree
                                               write fset_UseRTree
                                               default True              ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property Zoom         : Double           read  fget_Zoom
                                               write fset_Zoom           ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property ZoomEx       : Double           read  fget_ZoomEx
                                               write fset_ZoomEx         ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property MasterViewer : IGIS_Viewer      read  fget_MasterViewer
                                               write fset_MasterViewer   ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property UponDestroy  : Boolean          read  fget_UponDestroy    ;

    public // IGIS_ViewerBmp public properties

      /// <inheritdoc from="IGIS_ViewerBmp"/>
      property Bitmap : TObject            read fget_Bitmap ;

      /// <inheritdoc from="IGIS_ViewerBmp"/>
      property GIS_Bitmap : TGIS_Bitmap    read fget_GIS_Bitmap ;

      /// <inheritdoc from="IGIS_ViewerBmp"/>
      property TileRect : TRect            read  fget_TileRect
                                           write fset_TileRect ;

      /// <inheritdoc from="IGIS_ViewerBmp"/>
      property Width : Integer             read fget_Width ;

      /// <inheritdoc from="IGIS_ViewerBmp"/>
      property Height : Integer            read fget_Height ;

    public

      // public properties new for TGIS_ViewerBmp

      /// <summary>
      ///   Picture to be used as a background.
      /// </summary>
      property TilePicture : TGIS_Bitmap   read  fget_TilePicture
                                           write fset_TilePicture ;

      /// <summary>
      ///   Currently operating renderer. By assigning a new a different
      ///   rendering engine a rendering will be switched to a new engine .
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///     Do not free renderer directly - control maintains a renderer
      ///     life time.
      ///   </note>
      /// </remarks>
      {#ownership:set:release}
      property Renderer : TGIS_RendererAbstract
                                           read  fget_Renderer
                                           write fset_Renderer ;

//      /// <summary>
//      ///   Color used for selected objects.
//      /// </summary>
//      property SelectionColor : TAlphaColor read  fget_SelectionColor
//                                            write fset_SelectionColor ;

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
      property ExtentChangeEvent : TGIS_PvlEvent read  fget_ExtentChangeEvent
                                                 write fset_ExtentChangeEvent ;

      /// <event/>
      /// <summary>
      ///   VisibleExtentChange event. Will be fired before BeforePaint if
      ///   TGIS_Viewer.VisibleExtent was changed. Will not be fired if
      ///   TGIS_Viewer.VisibleExtent was changed based on changed Zoom - in
      ///   such situation only ZoomChange will be fired.
      /// </summary>
      property VisibleExtentChangeEvent : TGIS_PvlEvent
                                          read  fget_VisibleExtentChangeEvent
                                          write fset_VisibleExtentChangeEvent ;

      /// <event/>
      /// <summary>
      ///   ZoomChange event. Will be fired before OnBeforePaint if
      ///   Viewer.Zoom was changed.
      /// </summary>
      property ZoomChangeEvent : TGIS_PvlEvent  read  fget_ZoomChangeEvent
                                                write fset_ZoomChangeEvent ;

    published // new events for this class

      /// <event/>
      /// <summary>
      ///   BeforePaint event. Will be fired before any Draw operation.
      ///   Uses painting context object.
      /// </summary>
      property BeforePaintEvent  : TGIS_PaintEvent read  FOnBeforePaint
                                                   write FOnBeforePaint ;

      /// <event/>
      /// <summary>
      ///   BeforePaintRenderer event. Will be fired before any Draw operation.
      ///   Uses renderer object.
      /// </summary>
      property BeforePaintRendererEvent : TGIS_RendererEvent
                                               read  FOnBeforePaintRenderer
                                               write FOnBeforePaintRenderer ;

      /// <event/>
      /// <summary>
      ///   AfterPaint event. Will be fired after any Draw operation.
      ///   Uses painting context object.
      /// </summary>
      property AfterPaintEvent   : TGIS_PaintEvent read  FOnAfterPaint
                                                   write FOnAfterPaint ;

      /// <event/>
      /// <summary>
      ///   AfterPaintRenderer event. Will be fired after any Draw operation.
      ///   Uses renderer object.
      /// </summary>
      property AfterPaintRendererEvent : TGIS_RendererEvent
                                               read  FOnAfterPaintRenderer
                                               write FOnAfterPaintRenderer ;

      /// <event/>
      /// <summary>
      ///   PaintExtra event. Called after renderer.PaintExtra() method.
      /// </summary>
      property PaintExtraEvent   : TGIS_RendererEvent
                                               read  FOnPaintExtra
                                               write FOnPaintExtra ;

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

{$REGION 'properties access functions of IGIS_Viewer'}

  function T_PvlViewerBmp.fget_AutoStyle
    : Boolean ;
  begin
    Result := TGIS_ViewerBmp( oControl ).AutoStyle ;
  end ;

  procedure T_PvlViewerBmp.fset_AutoStyle(
    const _value : Boolean
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).AutoStyle := _value ;
  end ;

  function T_PvlViewerBmp.fget_BigExtent
    : TGIS_Extent ;
  begin
    Result := TGIS_ViewerBmp( oControl ).BigExtent ;
  end ;

  function T_PvlViewerBmp.fget_BigExtentMargin
    : Integer ;
  begin
    Result := TGIS_ViewerBmp( oControl ).BigExtentMargin ;
  end ;

  function T_PvlViewerBmp.fget_Bitmap
    : TObject ;
  begin
    Result := TGIS_ViewerBmp( oControl ).Bitmap
  end ;

  function T_PvlViewerBmp.fget_GIS_Bitmap
    : TGIS_Bitmap ;
  begin
    Result := TGIS_ViewerBmp( oControl ).GIS_Bitmap ;
  end ;

  function  T_PvlViewerBmp.fget_TileRect
    : TRect ;
  begin
    Result := TGIS_ViewerBmp( oControl ).TileRect ;
  end ;

  procedure T_PvlViewerBmp.fset_TileRect(
    const _val : TRect
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).TileRect := _val ;
  end ;

  function T_PvlViewerBmp.fget_Width
    : Integer ;
  begin
    Result := TGIS_ViewerBmp( oControl ).Width ;
  end;

  function T_PvlViewerBmp.fget_Height
    : Integer ;
  begin
    Result := TGIS_ViewerBmp( oControl ).Height ;
  end;

  function  T_PvlViewerBmp.fget_KeepContextInternal
    : Boolean ;
  begin
    Result := TGIS_ViewerBmp( oControl ).KeepContextInternal ;
  end;

  procedure T_PvlViewerBmp.fset_KeepContextInternal(
    const _val   : Boolean
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).KeepContextInternal := _val ;
  end;

  function T_PvlViewerBmp.fget_ContextInternal
    : TObject ;
  begin
    Result := TGIS_ViewerBmp( oControl ).ContextInternal ;
  end;

  procedure T_PvlViewerBmp.fset_BigExtentMargin(
    const _value : Integer
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).BigExtentMargin := _value ;
  end ;

  function T_PvlViewerBmp.fget_BusyLevel
    : Integer ;
  begin
    Result := TGIS_ViewerBmp( oControl ).BusyLevel ;
  end ;

  function T_PvlViewerBmp.fget_BusyText
    : String ;
  begin
    Result := TGIS_ViewerBmp( oControl ).BusyText ;
  end ;

  function T_PvlViewerBmp.fget_Center
    : TGIS_Point  ;
  begin
    Result := TGIS_ViewerBmp( oControl ).Center ;
  end ;

  procedure T_PvlViewerBmp.fset_Center(
    const _value : TGIS_Point
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).Center := _value ;
  end ;

  function T_PvlViewerBmp.fget_CenterPtg
    : TGIS_Point  ;
  begin
    Result := TGIS_ViewerBmp( oControl ).CenterPtg ;
  end ;

  procedure T_PvlViewerBmp.fset_CenterPtg(
    const _value : TGIS_Point
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).CenterPtg := _value ;
  end ;

  function T_PvlViewerBmp.fget_Color
    : TGIS_Color ;
  begin
    Result := TGIS_ViewerBmp( oControl ).Color ;
  end ;

  procedure T_PvlViewerBmp.fset_Color(
    const _value : TGIS_Color
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).Color := _value ;
  end ;

  function T_PvlViewerBmp.fget_Copyright
    : String ;
  begin
    Result := TGIS_ViewerBmp( oControl ).Copyright ;
  end ;

  function T_PvlViewerBmp.fget_CS
    : TGIS_CSCoordinateSystem ;
  begin
    Result := TGIS_ViewerBmp( oControl ).CS ;
  end ;

  procedure T_PvlViewerBmp.fset_CS(
    const _value : TGIS_CSCoordinateSystem
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).CS := _value ;
  end ;

  function T_PvlViewerBmp.fget_CustomPPI
    : Integer ;
  begin
    Result := TGIS_ViewerBmp( oControl ).CustomPPI ;
  end ;

  procedure T_PvlViewerBmp.fset_CustomPPI(
    const _value : Integer
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).CustomPPI := _value ;
  end ;

  function T_PvlViewerBmp.fget_Editor
    : IGIS_Editor ;
  begin
    Result := TGIS_ViewerBmp( oControl ).Editor ;
  end ;

  procedure T_PvlViewerBmp.fset_Editor(
    const _value : IGIS_Editor
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).Editor := _value ;
  end ;

  function T_PvlViewerBmp.fget_Extent
    : TGIS_Extent ;
  begin
    Result := TGIS_ViewerBmp( oControl ).Extent ;
  end ;

  function T_PvlViewerBmp.fget_FileCopyrights
    : String      ;
  begin
    Result := TGIS_ViewerBmp( oControl ).FileCopyrights ;
  end ;

  function T_PvlViewerBmp.fget_FontScale
    : Integer ;
  begin
    Result := TGIS_ViewerBmp( oControl ).FontScale ;
  end ;

  procedure T_PvlViewerBmp.fset_FontScale(
    const _value : Integer
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).FontScale := _value ;
  end ;

  function T_PvlViewerBmp.fget_FullDrawExtent
    : TGIS_Extent ;
  begin
    Result := TGIS_ViewerBmp( oControl ).FullDrawExtent ;
  end ;

  function T_PvlViewerBmp.fget_Hierarchy: IGIS_HierarchyManager;
  begin
    Result := TGIS_ViewerBmp( oControl ).Hierarchy
  end ;

  function T_PvlViewerBmp.fget_IncrementalPaint
    : Boolean ;
  begin
    Result := TGIS_ViewerBmp( oControl ).IncrementalPaint ;
  end ;

  procedure T_PvlViewerBmp.fset_IncrementalPaint(
    const _value : Boolean
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).IncrementalPaint := _value ;
  end ;

  //TILER
  function T_PvlViewerBmp.fget_TiledPaint
    : Boolean ;
  begin
    Result := TGIS_ViewerBmp( oControl ).TiledPaint ;
  end ;

  //TILER
  procedure T_PvlViewerBmp.fset_TiledPaint(
    const _value : Boolean
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).TiledPaint := _value ;
  end ;

  function T_PvlViewerBmp.fget_InPaint
    : Boolean ;
  begin
    Result := TGIS_ViewerBmp( oControl ).InPaint ;
  end ;

  function T_PvlViewerBmp.fget_IsBusy
    : Boolean ;
  begin
    Result := TGIS_ViewerBmp( oControl ).IsBusy ;
  end ;

  function T_PvlViewerBmp.fget_IsEmpty
    : Boolean ;
  begin
    Result := TGIS_ViewerBmp( oControl ).IsEmpty ;
  end ;

  function T_PvlViewerBmp.fget_IsLocked
   : Boolean ;
  begin
    Result := TGIS_ViewerBmp( oControl ).IsLocked ;
  end ;

  function T_PvlViewerBmp.fget_IsTopmost
   : Boolean ;
  begin
    Result := TGIS_ViewerBmp( oControl ).IsTopmost ;
  end ;

  function T_PvlViewerBmp.fget_Items
    : TGIS_LayerAbstractList ;
  begin
    Result := TGIS_ViewerBmp( oControl ).Items ;
  end ;

  function T_PvlViewerBmp.fget_KeepScale
    : Boolean ;
  begin
//    Result := TGIS_ViewerBmp( oControl ).Keep ;
  end ;

  function T_PvlViewerBmp.fget_LabelsReg
    : TGIS_LabelsAreaAbstract ;
  begin
    Result := TGIS_ViewerBmp( oControl ).LabelsReg ;
  end ;

  function T_PvlViewerBmp.fget_MultiUserMode
    : TGIS_MultiUser ;
  begin
    Result := TGIS_ViewerBmp( oControl ).MultiUserMode ;
  end ;

  procedure T_PvlViewerBmp.fset_MultiUserMode(
    const _value : TGIS_MultiUser
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).MultiUserMode := _value ;
  end ;

  function T_PvlViewerBmp.fget_CustomData
    : TGIS_StringList ;
  begin
    Result := TGIS_ViewerBmp( oControl ).CustomData ;
  end ;

  function T_PvlViewerBmp.fget_OverlappedExtentMargin
    : Integer ;
  begin
    Result := TGIS_ViewerBmp( oControl ).OverlappedExtentMargin ;
  end ;

  procedure T_PvlViewerBmp.fset_OverlappedExtentMargin(
    const _value : Integer
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).OverlappedExtentMargin := _value ;
  end ;

  function T_PvlViewerBmp.fget_PPI
    : Integer ;
  begin
    Result := TGIS_ViewerBmp( oControl ).PPI ;
  end ;

  function T_PvlViewerBmp.fget_ProjectFile
    : TGIS_ConfigAbstract ;
  begin
    Result := TGIS_ViewerBmp( oControl ).ProjectFile
  end ;

  procedure T_PvlViewerBmp.fset_ProjectFile(
    const _value : TGIS_ConfigAbstract
  ) ;
  begin
    //?TGIS_ViewerBmp( oControl ).ProjectFile := _value ;
  end ;

  function T_PvlViewerBmp.fget_ProjectName
    : String ;
  begin
    Result := TGIS_ViewerBmp( oControl ).ProjectName
  end ;

  function T_PvlViewerBmp.fget_RestrictedDrag
    : Boolean ;
  begin
    Result := TGIS_ViewerBmp( oControl ).RestrictedDrag ;
  end ;

  function T_PvlViewerBmp.fget_DelayedUpdate
    : Integer ;
  begin
//    Result := TGIS_ViewerBmp( oControl ).Del ;
  end;

  procedure T_PvlViewerBmp.fset_DelayedUpdate(
    const _value : Integer
  ) ;
  begin
//    TGIS_ViewerBmp( oControl ).fset_DelayedUpdate( _value ) ;
  end;

  procedure T_PvlViewerBmp.fset_KeepScale(
    const _value : Boolean
  ) ;
  begin
//    TGIS_ViewerBmp( oControl ).fset_KeepScale( _value ) ;
  end ;

  function T_PvlViewerBmp.fget_ProgressiveUpdate
    : Integer ;
  begin
//    Result := TGIS_ViewerBmp( oControl ).fget_ProgressiveUpdate ;
  end;

  procedure T_PvlViewerBmp.fset_ProgressiveUpdate(
    const _value : Integer
  ) ;
  begin
//    TGIS_ViewerBmp( oControl ).fset_ProgressiveUpdate( _value ) ;
  end;

  procedure T_PvlViewerBmp.fset_RestrictedDrag(
    const _value : Boolean
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).RestrictedDrag := _value ;
  end ;

  function T_PvlViewerBmp.fget_RestrictedExtent
    : TGIS_Extent ;
  begin
    Result := TGIS_ViewerBmp( oControl ).RestrictedExtent ;
  end ;

  procedure T_PvlViewerBmp.fset_RestrictedExtent(
    const _value : TGIS_Extent
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).RestrictedExtent := _value ;
  end ;

  function T_PvlViewerBmp.fget_RotationAngle
    : Double ;
  begin
    Result := TGIS_ViewerBmp( oControl ).RotationAngle ;
  end ;

  procedure T_PvlViewerBmp.fset_RotationAngle(
    const _value : Double
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).RotationAngle := _value ;
  end ;

  function T_PvlViewerBmp.fget_RotationPoint
    : TGIS_Point ;
  begin
    Result := TGIS_ViewerBmp( oControl ).RotationPoint ;
  end ;

  procedure T_PvlViewerBmp.fset_RotationPoint(
    const _value : TGIS_Point
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).RotationPoint := _value ;
  end ;

  function T_PvlViewerBmp.fget_ScaleAsFloat
    : Double ;
  begin
    Result := TGIS_ViewerBmp( oControl ).ScaleAsFloat ;
  end ;

  procedure T_PvlViewerBmp.fset_ScaleAsFloat(
    const _value : Double
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).ScaleAsFloat := _value ;
  end ;

  function T_PvlViewerBmp.fget_ScaleAsText
    : String ;
  begin
    Result := TGIS_ViewerBmp( oControl ).ScaleAsText ;
  end ;

  procedure T_PvlViewerBmp.fset_ScaleAsText(
    const _value : String
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).ScaleAsText := _value ;
  end ;

  function T_PvlViewerBmp.fget_SelectionGisColor
    : TGIS_Color ;
  begin
    Result := TGIS_ViewerBmp( oControl ).SelectionGisColor ;
  end ;

  procedure T_PvlViewerBmp.fset_Level(
    const _value : Double
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).Level := _value ;
  end ;

  function T_PvlViewerBmp.fget_Level
    : Double ;
  begin
    Result := TGIS_ViewerBmp( oControl ).Level ;
  end ;

  procedure T_PvlViewerBmp.fset_SelectionGisColor(
    const _value : TGIS_Color
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).SelectionGisColor := _value ;
  end ;

  function T_PvlViewerBmp.fget_SelectionOutlineOnly
    : Boolean ;
  begin
    Result := TGIS_ViewerBmp( oControl ).SelectionOutlineOnly ;
  end ;

  procedure T_PvlViewerBmp.fset_SelectionOutlineOnly(
    const _value : Boolean
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).SelectionOutlineOnly := _value ;
  end ;

  function T_PvlViewerBmp.fget_SelectionTransparency
    : Integer ;
  begin
    Result := TGIS_ViewerBmp( oControl ).SelectionTransparency ;
  end ;

  procedure T_PvlViewerBmp.fset_SelectionTransparency(
    const _value : Integer
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).SelectionTransparency := _value ;
  end ;

  function T_PvlViewerBmp.fget_SelectionWidth
    : Integer ;
  begin
    Result := TGIS_ViewerBmp( oControl ).SelectionWidth ;
  end ;

  procedure T_PvlViewerBmp.fset_SelectionWidth(
    const _value : Integer
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).SelectionWidth := _value ;
  end ;

  function T_PvlViewerBmp.fget_SystemPPI
    : Integer ;
  begin
    Result := TGIS_ViewerBmp( oControl ).SystemPPI ;
  end ;

  function T_PvlViewerBmp.fget_ViewerParent
    : IGIS_ViewerParent ;
  begin
    Result := TGIS_ViewerBmp( oControl ).ViewerParent ;
  end ;

  function T_PvlViewerBmp.fget_ViewerParentRoot
    : IGIS_ViewerParent ;
  begin
    Result := TGIS_ViewerBmp( oControl ).ViewerParentRoot ;
  end ;

  function T_PvlViewerBmp.fget_Viewport
    : TGIS_Point ;
  begin
    Result := TGIS_ViewerBmp( oControl ).Viewport ;
  end ;

  procedure T_PvlViewerBmp.fset_Viewport(
    const _value : TGIS_Point
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).Viewport := _value ;
  end ;

  function T_PvlViewerBmp.fget_VisibleExtent
    : TGIS_Extent ;
  begin
    Result := TGIS_ViewerBmp( oControl ).VisibleExtent ;
  end ;

  procedure T_PvlViewerBmp.fset_VisibleExtent(
    const _value : TGIS_Extent
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).VisibleExtent := _value ;
  end ;

  function T_PvlViewerBmp.fget_UseAnimations
    : Boolean ;
  begin
//    Result := TGIS_ViewerBmp( oControl ).UseAni ;
  end ;

  procedure T_PvlViewerBmp.fset_UseAnimations(
    const _value : Boolean
  ) ;
  begin
//    TGIS_ViewerBmp( oControl ).fset_UseAnimations( _value ) ;
  end ;

  function T_PvlViewerBmp.fget_UseRTree
    : Boolean ;
  begin
    Result := TGIS_ViewerBmp( oControl ).UseRTree ;
  end ;

  procedure T_PvlViewerBmp.fset_UseRTree(
    const _value : Boolean
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).UseRTree := _value ;
  end ;

  function T_PvlViewerBmp.fget_Zoom
    : Double ;
  begin
    Result := TGIS_ViewerBmp( oControl ).Zoom ;
  end ;

  procedure T_PvlViewerBmp.fset_Zoom(
    const _value : Double
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).Zoom := _value ;
  end ;

  function T_PvlViewerBmp.fget_ZoomEx
    : Double ;
  begin
    Result := TGIS_ViewerBmp( oControl ).ZoomEx ;
  end ;

  procedure T_PvlViewerBmp.fset_ZoomEx(
    const _value : Double
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).ZoomEx := _value ;
  end ;

  function T_PvlViewerBmp.fget_MasterViewer
    : IGIS_Viewer ;
  begin
    Result := TGIS_ViewerBmp( oControl ).MasterViewer  ;
  end;

  procedure T_PvlViewerBmp.fset_MasterViewer(
    const _value : IGIS_Viewer
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).MasterViewer := _value ;
  end;

  function T_PvlViewerBmp.fget_UponDestroy
    : Boolean ;
  begin
    Result := TGIS_ViewerBmp( oControl ).UponDestroy ;
  end ;

  function  T_PvlViewerBmp.fget_TemporaryScaleInternal : Double ;
  begin
//    Result := TGIS_ViewerBmp( oControl ).fget_TemporaryScaleInternal ;
  end ;

  procedure T_PvlViewerBmp.fset_TemporaryScaleInternal(
    const _value : Double
  ) ;
  begin
//    TGIS_ViewerBmp( oControl ).fset_TemporaryScaleInternal( _value ) ;
  end ;

  function  T_PvlViewerBmp.fget_TemporaryVisibleExtent : TGIS_Extent ;
  begin
    Result := TGIS_ViewerBmp( oControl ).TemporaryVisibleExtent ;
  end ;

  procedure T_PvlViewerBmp.fset_TemporaryVisibleExtent(
    const _value : TGIS_Extent
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).TemporaryVisibleExtent := _value ;
  end ;

{$ENDREGION 'properties access functions of IGIS_Viewer'}

{$REGION 'new properties access routines of T_PvlViewerBmp'}

  procedure T_PvlViewerBmp.fset_Renderer(
    const _value : TGIS_RendererAbstract
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).Renderer := _value ;
  end ;

  function T_PvlViewerBmp.fget_Renderer
    : TGIS_RendererAbstract ;
  begin
    Result := TGIS_ViewerBmp( oControl ).Renderer ;
  end;

  procedure T_PvlViewerBmp.fset_DrawBasemapOnly(
    const _value: Boolean
  ) ;
  begin
//    TGIS_ViewerBmp( oControl ).DrawBase( _value ) ;
  end;

  function T_PvlViewerBmp.fget_DrawBasemapOnly
    : Boolean;
  begin
//    Result := TGIS_ViewerBmp( oControl ).fget_DrawBasemapOnly ;
  end;

  procedure T_PvlViewerBmp.fset_TilePicture(
    const _value: TGIS_Bitmap
  );
  begin
    TGIS_ViewerBmp( oControl ).TilePicture := TBitmap( _value.NativeBitmap ) ;
  end;

  function T_PvlViewerBmp.fget_TilePicture
    : TGIS_Bitmap ;
  begin
    Result := TGIS_Bitmap.Create ;
    Result.NativeBitmap := TGIS_ViewerBmp( oControl ).TilePicture ;
  end;

{$ENDREGION 'new properties access routines of T_PvlViewerBmp'}

{$REGION 'property access routines of TGIS_Viewer's event handlers'}

  function T_PvlViewerBmp.fget_BusyEvent
    : TGIS_BusyEvent ;
  begin
    Result := TGIS_ViewerBmp( oControl ).BusyEvent ;
  end ;

  procedure T_PvlViewerBmp.fset_BusyEvent(
    const _value : TGIS_BusyEvent
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).BusyEvent := _value ;
  end ;

  function T_PvlViewerBmp.fget_ExtentChangeEvent
    : TGIS_PvlEvent ;
  begin
    Result := TGIS_ViewerBmp( oControl ).ExtentChangeEvent ;
  end ;

  procedure T_PvlViewerBmp.fset_ExtentChangeEvent(
    const _value : TGIS_PvlEvent
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).ExtentChangeEvent := _value ;
  end ;

  function T_PvlViewerBmp.fget_VisibleExtentChangeEvent
    : TGIS_PvlEvent ;
  begin
    Result := TGIS_ViewerBmp( oControl ).VisibleExtentChangeEvent ;
  end ;

  procedure T_PvlViewerBmp.fset_VisibleExtentChangeEvent(
    const _value : TGIS_PvlEvent
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).VisibleExtentChangeEvent := _value ;
  end ;

  function T_PvlViewerBmp.fget_ZoomChangeEvent
    : TGIS_PvlEvent ;
  begin
    Result := TGIS_ViewerBmp( oControl ).ZoomChangeEvent ;
  end ;

  procedure T_PvlViewerBmp.fset_ZoomChangeEvent(
    const _value : TGIS_PvlEvent
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).ZoomChangeEvent := _value ;
  end ;

{$ENDREGION 'property access routines of TGIS_Viewer's event handlers'}

{$REGION 'IGIS_ViewerParent events access routines'}

function T_PvlViewerBmp.fget_BeforePaintEvent
  : TGIS_PaintEvent ;
begin
  Result := TGIS_ViewerBmp( oControl ).BeforePaintEvent ;
end;

procedure T_PvlViewerBmp.fset_BeforePaintEvent(
  const _value : TGIS_PaintEvent
);
begin
  TGIS_ViewerBmp( oControl ).BeforePaintEvent := _value ;
end;

function T_PvlViewerBmp.fget_AfterPaintEvent
  : TGIS_PaintEvent ;
begin
  Result := TGIS_ViewerBmp( oControl ).AfterPaintEvent ;
end;

procedure T_PvlViewerBmp.fset_AfterPaintEvent(
  const _value : TGIS_PaintEvent
);
begin
  TGIS_ViewerBmp( oControl ).AfterPaintEvent := _value ;
end;

function T_PvlViewerBmp.fget_BeforePaintRendererEvent
  : TGIS_RendererEvent ;
begin
  Result := TGIS_ViewerBmp( oControl ).BeforePaintRendererEvent ;
end;

procedure T_PvlViewerBmp.fset_BeforePaintRendererEvent(
  const _value : TGIS_RendererEvent
);
begin
  TGIS_ViewerBmp( oControl ).BeforePaintRendererEvent := _value ;
end;

function T_PvlViewerBmp.fget_AfterPaintRendererEvent
  : TGIS_RendererEvent ;
begin
  Result := TGIS_ViewerBmp( oControl ).AfterPaintRendererEvent ;
end;

procedure T_PvlViewerBmp.fset_AfterPaintRendererEvent(
  const _value : TGIS_RendererEvent
);
begin
  TGIS_ViewerBmp( oControl ).AfterPaintRendererEvent := _value ;
end;

function T_PvlViewerBmp.fget_PaintExtraEvent
  : TGIS_RendererEvent ;
begin
  Result := TGIS_ViewerBmp( oControl ).PaintExtraEvent ;
end;

procedure T_PvlViewerBmp.fset_PaintExtraEvent(
  const _value : TGIS_RendererEvent
);
begin
  TGIS_ViewerBmp( oControl ).PaintExtraEvent := _value ;
end;

function T_PvlViewerBmp.fget_BeforeUpdateEvent
  : TGIS_RendererEvent ;
begin
//  Result := TGIS_ViewerBmp( oControl ).Before ;
end;

procedure T_PvlViewerBmp.fset_BeforeUpdateEvent(
  const _value : TGIS_RendererEvent
);
begin
//  TGIS_ViewerBmp( oControl ).fset_BeforeUpdateEvent( _value ) ;
end;

function T_PvlViewerBmp.fget_AfterUpdateEvent
  : TGIS_RendererEvent ;
begin
//  Result := TGIS_ViewerBmp( oControl ).fget_AfterUpdateEvent ;
end;

procedure T_PvlViewerBmp.fset_AfterUpdateEvent(
  const _value : TGIS_RendererEvent
);
begin
//  TGIS_ViewerBmp( oControl ).fset_AfterUpdateEvent( _value ) ;
end;

{$ENDREGION 'IGIS_ViewerWnd events access routines'}

{$REGION 'public methods of IGIS_Viewer'}

  function T_PvlViewerBmp.ChangeHash
    : Int64 ;
  begin
    Result := TGIS_ViewerBmp( oControl ).ChangeHash ;
  end ;

  procedure T_PvlViewerBmp.Subscribe(
    const _control : IGIS_Subscribe
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).Subscribe( _control ) ;
  end ;

  procedure T_PvlViewerBmp.UnSubscribe(
    const _control : IGIS_Subscribe
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).Unsubscribe( _control ) ;
  end ;

  procedure T_PvlViewerBmp.NotifySubscribers(
    const _event   : Integer ;
    const _context : TObject
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).NotifySubscribers( _event, _context ) ;
  end ;

  function T_PvlViewerBmp.NotifyPaintException(
    const _message    : String   ;
    const _exception  : Exception
  ) : Boolean ;
  begin
    Result := TGIS_ViewerBmp( oControl ).NotifyPaintException( _message, _exception ) ;
  end ;

  procedure T_PvlViewerBmp.Lock ;
  begin
    TGIS_ViewerBmp( oControl ).Lock ;
  end ;

  procedure T_PvlViewerBmp.Unlock ;
  begin
    TGIS_ViewerBmp( oControl ).Unlock ;
  end ;

  procedure T_PvlViewerBmp.Unlock(
    const _redraw : Boolean
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).Unlock( _redraw ) ;
  end ;

  procedure T_PvlViewerBmp.Interrupt ;
  begin
    TGIS_ViewerBmp( oControl ).Interrupt ;
  end ;

  //TILER
  function T_PvlViewerBmp.Interrupted
    : Boolean ;
  begin
    Result := TGIS_ViewerBmp( oControl ).Interrupted ;
  end ;

  function  T_PvlViewerBmp.HourglassActive
    : Boolean ;
  begin
    Result := TGIS_ViewerBmp( oControl ).HourglassActive ;
  end ;

  procedure T_PvlViewerBmp.HourglassPrepare ;
  begin
    TGIS_ViewerBmp( oControl ).HourglassPrepare ;
  end ;

  procedure T_PvlViewerBmp.HourglassRelease ;
  begin
    TGIS_ViewerBmp( oControl ).HourglassRelease ;
  end ;

  function  T_PvlViewerBmp.HourglassShake
    : Boolean ;
  begin
    Result := TGIS_ViewerBmp( oControl ).HourglassShake ;
  end ;

  procedure T_PvlViewerBmp.HourglassRestart ;
  begin
    TGIS_ViewerBmp( oControl ).HourglassRestart ;
  end ;

  procedure T_PvlViewerBmp.BusyPrepare(
    _sender : TObject ;
    _text   : String
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).BusyPrepare( _sender, _text ) ;
  end ;

  procedure T_PvlViewerBmp.BusyRelease(
    _sender : TObject
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).BusyRelease( _sender ) ;
  end ;

  procedure T_PvlViewerBmp.BusyShake(
         _sender : TObject ;
         _pos    : Int64 ;
         _end    : Int64 ;
    var _abort  : Boolean
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).BusyShake( _sender, _pos, _end, _abort ) ;
  end ;

  procedure T_PvlViewerBmp.RaiseBusyEvent(
        _sender  : TObject ;
        _pos     : Int64 ;
        _end     : Int64;
    var _abort   : Boolean
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).RaiseBusyEvent( _sender, _pos, _end, _abort ) ;
  end ;

  procedure T_PvlViewerBmp.RaiseHelpEvent(
        _sender  : TObject ;
        _name    : String
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).RaiseHelpEvent( _sender, _name ) ;
  end ;

  function T_PvlViewerBmp.AssignedBusyEvent
    : TGIS_BusyEvent ;
  begin
    Result := TGIS_ViewerBmp( oControl ).AssignedBusyEvent() ;
  end ;

  function T_PvlViewerBmp.AssignedHelpEvent
    : TGIS_HelpEvent ;
  begin
    Result := TGIS_ViewerBmp( oControl ).AssignedHelpEvent() ;
  end ;

  function T_PvlViewerBmp.StorePaintState
    : TObject ;
  begin
    Result := TGIS_ViewerBmp( oControl ).StorePaintState ;
  end ;

  procedure T_PvlViewerBmp.RestorePaintState(
    var _state : TObject
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).RestorePaintState( _state ) ;
  end ;

  procedure T_PvlViewerBmp.BeginPaintInternal ;
  begin
    TGIS_ViewerBmp( oControl ).BeginPaintInternal ;
  end ;

  procedure T_PvlViewerBmp.EndPaintInternal ;
  begin
    TGIS_ViewerBmp( oControl ).EndPaintInternal ;
  end ;

  function T_PvlViewerBmp.SynchronizePaint(
    const _interrupt : Boolean
  ) : Boolean ;
  begin
    Result := TGIS_ViewerBmp( oControl ).SynchronizePaint( _interrupt ) ;
  end;

  procedure T_PvlViewerBmp.ReParentLock ;
  begin
    TGIS_ViewerBmp( oControl ).ReParentLock ;
  end ;

  procedure T_PvlViewerBmp.ReParentUnlock ;
  begin
    TGIS_ViewerBmp( oControl ).ReParentUnlock ;
  end ;

  function T_PvlViewerBmp.ReParent(
    const _parent : IGIS_ViewerParent
  ) : IGIS_ViewerParent ;
  begin
    Result := TGIS_ViewerBmp( oControl ).ReParent( _parent ) ;
  end ;

  function T_PvlViewerBmp.AttachLayer(
    const _layer : TGIS_LayerAbstract
  ) : IGIS_Viewer ;
  begin
    Result := TGIS_ViewerBmp( oControl ).AttachLayer( _layer ) ;
  end ;

  procedure T_PvlViewerBmp.Open(
    const _path    : String
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).Open( _path ) ;
  end ;

  procedure T_PvlViewerBmp.Open(
    const _path    : String ;
    const _strict  : Boolean
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).Open( _path, _strict  ) ;
  end ;

  procedure T_PvlViewerBmp.OpenEx(
    const _configFile : TGIS_ConfigAbstract ;
    const _path       : String
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).OpenEx( _configFile, _path ) ;
  end ;

  procedure T_PvlViewerBmp.OpenEx(
    const _configFile : TGIS_ConfigAbstract ;
    const _path       : String              ;
    const _strict     : Boolean
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).OpenEx( _configFile, _path, _strict ) ;
  end ;

  procedure T_PvlViewerBmp.Close ;
  begin
    TGIS_ViewerBmp( oControl ).Close ;
  end ;

  procedure T_PvlViewerBmp.ReadConfig ;
  begin
    TGIS_ViewerBmp( oControl ).ReadConfig ;
  end ;

  procedure T_PvlViewerBmp.RereadConfig ;
  begin
    TGIS_ViewerBmp( oControl ).RereadConfig ;
  end ;

  procedure T_PvlViewerBmp.WriteConfig ;
  begin
    TGIS_ViewerBmp( oControl ).WriteConfig ;
  end ;

  procedure T_PvlViewerBmp.Add(
    const _layer : TGIS_LayerAbstract
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).Add( _layer ) ;
  end ;

  function T_PvlViewerBmp.Get(
    const _name    : String
  ) : TGIS_LayerAbstract ;
  begin
    Result := TGIS_ViewerBmp( oControl ).Get( _name ) ;
  end ;

  procedure T_PvlViewerBmp.Delete(
    const _name : String
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).Delete( _name ) ;
  end ;

  procedure T_PvlViewerBmp.AddHierarchy ;
  begin
    TGIS_ViewerBmp( oControl ).AddHierarchy ;
  end ;

  procedure T_PvlViewerBmp.Draw(
    const _renderer : TObject     ;
    const _mode     : TGIS_DrawMode
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).Draw( _renderer, _mode ) ;
  end ;

  function T_PvlViewerBmp.GetGrid(
    const _extent : TGIS_Extent ;
    const _grid   : TGIS_GridArray
  ) : Boolean ;
  begin
    Result := TGIS_ViewerBmp( oControl ).GetGrid( _extent, _grid ) ;
  end ;

  procedure T_PvlViewerBmp.RevertAll ;
  begin
    TGIS_ViewerBmp( oControl ).RevertAll ;
  end ;

  procedure T_PvlViewerBmp.SaveProject ;
  begin
    TGIS_ViewerBmp( oControl ).SaveProject ;
  end ;

  procedure T_PvlViewerBmp.SaveProject(
    const _relativepath : Boolean
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).SaveProject( _relativepath ) ;
  end ;

  procedure T_PvlViewerBmp.SaveProjectAs(
    const _path         : String
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).SaveProjectAs( _path ) ;
  end ;

  procedure T_PvlViewerBmp.SaveProjectAs(
    const _path         : String ;
    const _relativepath : Boolean
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).SaveProjectAs( _path, _relativepath ) ;
  end ;

  procedure T_PvlViewerBmp.SaveProjectAsEx(
    const _configFile   : TGIS_ConfigAbstract ;
    const _path         : String
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).SaveProjectAsEx( _configFile, _path ) ;
  end ;

  procedure T_PvlViewerBmp.SaveProjectAsEx(
    const _configFile   : TGIS_ConfigAbstract ;
    const _path         : String              ;
    const _relativepath : Boolean
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).SaveProjectAsEx( _configFile, _path, _relativepath ) ;
  end ;

  procedure T_PvlViewerBmp.SaveData ;
  begin
    TGIS_ViewerBmp( oControl ).SaveData ;
  end ;

  procedure T_PvlViewerBmp.SaveAll ;
  begin
    TGIS_ViewerBmp( oControl ).SaveAll ;
  end ;

  function  T_PvlViewerBmp.MustSave
    : Boolean ;
  begin
    Result := TGIS_ViewerBmp( oControl ).MustSave ;
  end ;

  procedure T_PvlViewerBmp.MarkModified ;
  begin
    TGIS_ViewerBmp( oControl ).MarkModified ;
  end ;

  procedure T_PvlViewerBmp.RecalcExtent ;
  begin
    TGIS_ViewerBmp( oControl ).RecalcExtent ;
  end ;

  procedure T_PvlViewerBmp.Reposition ;
  begin
    TGIS_ViewerBmp( oControl ).Reposition ;
  end ;

  procedure T_PvlViewerBmp.InvalidateExtent(
    const _extent  : TGIS_Extent
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).InvalidateExtent( _extent ) ;
  end ;

  procedure T_PvlViewerBmp.InvalidateExtent(
    const _extent : TGIS_Extent ;
    const _deep   : Boolean
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).InvalidateExtent( _extent, _deep ) ;
  end ;

  procedure T_PvlViewerBmp.InvalidateWholeMap ;
  begin
    TGIS_ViewerBmp( oControl ).InvalidateWholeMap ;
  end ;

  procedure T_PvlViewerBmp.InvalidateTopmost ;
  begin
    TGIS_ViewerBmp( oControl ).InvalidateTopmost ;
  end ;

  procedure T_PvlViewerBmp.InvalidateBasemap ;
  begin
    TGIS_ViewerBmp( oControl ).InvalidateBasemap ;
  end ;

  procedure T_PvlViewerBmp.InvalidateSelection ;
  begin
    TGIS_ViewerBmp( oControl ).InvalidateSelection ;
  end ;

  procedure T_PvlViewerBmp.InvalidateEditor(
    const _final : Boolean
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).InvalidateEditor( _final ) ;
  end ;

  function T_PvlViewerBmp.FullExtentZoom
    : Double ;
  begin
    Result := TGIS_ViewerBmp( oControl ).FullExtentZoom ;
  end ;

  procedure T_PvlViewerBmp.FullExtent ;
  begin
    TGIS_ViewerBmp( oControl ).FullExtent ;
  end ;

  function T_PvlViewerBmp.Locate(
    const _ptg     : TGIS_Point ;
    const _prec    : Double
  ) : TGIS_ShapeAbstract ;
  begin
    Result := TGIS_ViewerBmp( oControl ).Locate( _ptg, _prec ) ;
  end ;

  function T_PvlViewerBmp.Locate(
    const _ptg     : TGIS_Point ;
    const _prec    : Double     ;
    const _visible : Boolean
  ) : TGIS_ShapeAbstract ;
  begin
    Result := TGIS_ViewerBmp( oControl ).Locate( _ptg, _prec, _visible ) ;
  end ;

  function T_PvlViewerBmp.Locate(
    const _pt      : TPoint     ;
    const _prec    : Integer
  ) : TGIS_ShapeAbstract ;
  begin
    Result := TGIS_ViewerBmp( oControl ).Locate( _pt, _prec ) ;
  end ;

  function T_PvlViewerBmp.LocateEx(
    const _ptg     : TGIS_Point ;
    const _prec    : Double    ;
    const _visible : Boolean
  ) : TGIS_ShapeAbstractList ;
  begin
    Result := TGIS_ViewerBmp( oControl ).LocateEx( _ptg, _prec, _visible ) ;
  end ;

  function T_PvlViewerBmp.MapToScreen(
    const _ptg : TGIS_Point
  ) : TPoint ;
  begin
    Result := TGIS_ViewerBmp( oControl ).MapToScreen( _ptg ) ;
  end ;

  function T_PvlViewerBmp.MapToScreen3D(
    const _ptg : TGIS_Point3D
  ) : TPoint ;
  begin
    Result := TGIS_ViewerBmp( oControl ).MapToScreen3D( _ptg ) ;
  end ;

  function T_PvlViewerBmp.ScreenToMap(
    const _pt : TPoint
  ) : TGIS_Point ;
  begin
    Result := TGIS_ViewerBmp( oControl ).ScreenToMap( _pt ) ;
  end ;

  function T_PvlViewerBmp.ScreenToMap3D(
    const _pt : TPoint
  ) : TGIS_Point3D ;
  begin
    Result := TGIS_ViewerBmp( oControl ).ScreenToMap3D( _pt ) ;
  end ;

  function T_PvlViewerBmp.MapToScreenEx(
    const _pt : TGIS_Point
  ) : TGIS_Point ;
  begin
    Result := TGIS_ViewerBmp( oControl ).MapToScreenEx( _pt ) ;
  end ;

  function T_PvlViewerBmp.ScreenToMapEx(
    const _pt : TGIS_Point
  ) : TGIS_Point ;
  begin
    Result := TGIS_ViewerBmp( oControl ).ScreenToMapEx( _pt ) ;
  end ;

  function T_PvlViewerBmp.MapToScreenRect(
    const _rct : TGIS_Extent
  ) : TRect ;
  begin
    Result := TGIS_ViewerBmp( oControl ).MapToScreenRect( _rct ) ;
  end ;

  function T_PvlViewerBmp.ScreenToMapRect(
    const _rct : TRect
  ) : TGIS_Extent ;
  begin
    Result := TGIS_ViewerBmp( oControl ).ScreenToMapRect( _rct ) ;
  end ;

  function T_PvlViewerBmp.PixelsToTwips(
    const _size : Integer
  ) : Integer ;
  begin
    Result := TGIS_ViewerBmp( oControl ).PixelsToTwips( _size ) ;
  end ;

  function T_PvlViewerBmp.TwipsToPixels(
    const _size : Integer
  ) : Integer ;
  begin
    Result := TGIS_ViewerBmp( oControl ).TwipsToPixels( _size ) ;
  end ;

  function T_PvlViewerBmp.TwipsToPoints(
    const _size : Integer
  ) : Integer ;
  begin
    Result := TGIS_ViewerBmp( oControl ).TwipsToPoints( _size ) ;
  end ;

  procedure T_PvlViewerBmp.MoveViewport(
    var _dx : Integer ;
    var _dy : Integer
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).MoveViewport( _dx, _dy ) ;
  end ;

  procedure T_PvlViewerBmp.MoveViewportEx(
    var _dx : Double ;
    var _dy : Double
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).MoveViewportEx( _dx, _dy ) ;
  end ;

  procedure T_PvlViewerBmp.SetViewport(
    var _x : Double ;
    var _y : Double
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).SetViewport( _x, _y ) ;
  end ;

  procedure T_PvlViewerBmp.CenterViewport(
    const _ptg : TGIS_Point
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).CenterViewport( _ptg ) ;
  end ;

  procedure T_PvlViewerBmp.SetCSByWKT(
    const _wkt : String
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).SetCSByWKT( _wkt ) ;
  end ;

  procedure T_PvlViewerBmp.SetCSByEPSG(
    const _epsg : Integer
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).SetCSByEPSG( _epsg ) ;
  end ;

  procedure T_PvlViewerBmp.SetCSByWKTFile(
    const _path : String
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).SetCSByWKTFile( _path ) ;
  end ;

  function T_PvlViewerBmp.RotatedPoint(
    const _ptg : TGIS_Point
  ) : TGIS_Point ;
  begin
    Result := TGIS_ViewerBmp( oControl ).RotatedPoint( _ptg ) ;
  end ;

  function T_PvlViewerBmp.UnrotatedPoint(
    const _ptg : TGIS_Point
  ) : TGIS_Point ;
  begin
    Result := TGIS_ViewerBmp( oControl ).UnrotatedPoint( _ptg ) ;
  end ;

  function T_PvlViewerBmp.RotatedPoint3D(
    const _ptg : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    Result := TGIS_ViewerBmp( oControl ).RotatedPoint3D( _ptg ) ;
  end ;

  procedure T_PvlViewerBmp.RotatedPoint3D_ref(
    var _ptg : TGIS_Point3D
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).RotatedPoint3D_ref( _ptg ) ;
  end ;

  function T_PvlViewerBmp.UnrotatedPoint3D(
    const _ptg : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    Result := TGIS_ViewerBmp( oControl ).UnrotatedPoint3D( _ptg ) ;
  end ;

  procedure T_PvlViewerBmp.UnrotatedPoint3D_ref(
    var _ptg : TGIS_Point3D
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).UnrotatedPoint3D_ref( _ptg ) ;
  end ;

  function T_PvlViewerBmp.RotatedExtent(
    const _extent : TGIS_Extent
  ) : TGIS_Extent ;
  begin
    Result := TGIS_ViewerBmp( oControl ).RotatedExtent( _extent ) ;
  end ;

  function T_PvlViewerBmp.UnrotatedExtent(
    const _extent : TGIS_Extent
  ) : TGIS_Extent ;
  begin
    Result := TGIS_ViewerBmp( oControl ).UnrotatedExtent( _extent ) ;
  end ;

  function T_PvlViewerBmp.GetRenderContext
    : TObject ;
  begin
    Result := TGIS_ViewerBmp( oControl ).GetRenderContext ;
  end ;

  procedure T_PvlViewerBmp.WaitForBackgroundProcesses ;
  begin
    TGIS_ViewerBmp( oControl ).WaitForBackgroundProcesses ;
  end;

  procedure T_PvlViewerBmp.WaitForNotBusy(
          _sender : TObject ;
    const _proc   : TGIS_WaitForNotBusyProc
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).WaitForNotBusy( _sender, _proc ) ;
  end;

{$ENDREGION 'public methods of IGIS_Viewer'}

{$REGION 'public methods of IGIS_ViewerBmp'}

  procedure T_PvlViewerBmp.ZoomIn ;
  begin
    TGIS_ViewerBmp( oControl ).ZoomIn ;
  end ;

  procedure T_PvlViewerBmp.ZoomOut ;
  begin
    TGIS_ViewerBmp( oControl ).ZoomOut ;
  end ;

  procedure T_PvlViewerBmp.Clear;
  begin
    TGIS_ViewerBmp( oControl ).Clear ;
  end ;

  procedure T_PvlViewerBmp.Draw ;
  begin
    TGIS_ViewerBmp( oControl ).Draw ;
  end ;

  procedure T_PvlViewerBmp.ScrollPgUp ;
  begin
    TGIS_ViewerBmp( oControl ).ScrollPgUp ;
  end ;

  procedure T_PvlViewerBmp.ScrollPgDn ;
  begin
    TGIS_ViewerBmp( oControl ).ScrollPgDn ;
  end ;

  procedure T_PvlViewerBmp.ScrollPgRight ;
  begin
    TGIS_ViewerBmp( oControl ).ScrollPgRight ;
  end ;

  procedure T_PvlViewerBmp.ScrollPgLeft ;
  begin
    TGIS_ViewerBmp( oControl ).ScrollPgLeft ;
  end ;

  procedure T_PvlViewerBmp.ScrollUp ;
  begin
    TGIS_ViewerBmp( oControl ).ScrollUp ;
  end ;

  procedure T_PvlViewerBmp.ScrollDn ;
  begin
    TGIS_ViewerBmp( oControl ).ScrollDn ;
  end ;

  procedure T_PvlViewerBmp.ScrollRight ;
  begin
    TGIS_ViewerBmp( oControl ).ScrollRight ;
  end ;

  procedure T_PvlViewerBmp.ScrollLeft ;
  begin
    TGIS_ViewerBmp( oControl ).ScrollLeft ;
  end ;

  procedure T_PvlViewerBmp.SetSize(
    const _width  : Integer ;
    const _height : Integer
  ) ;
  begin
    TGIS_ViewerBmp( oControl ).SetSize( _width, _height ) ;
  end ;


{$ENDREGION 'public methods of IGIS_ViewerBmp'}

{$REGION 'Other public methods'}

  procedure T_PvlViewerBmp.PrintBmp(
    var   _bmp    : TGIS_Bitmap
  ) ;
  begin
    PrintBmp( _bmp, False ) ;
  end ;

  procedure T_PvlViewerBmp.PrintBmp(
    var   _bmp    : TGIS_Bitmap ;
    const _full   : Boolean
  ) ;
  var
    bmp : TBitmap ;
  begin
    bmp := TBitmap( _bmp.NativeBitmap ) ;
    try
      TGIS_ViewerBmp( oControl ).PrintBmp( bmp, _full ) ;
    finally
      bmp.Free ;
    end;
  end ;

{$ENDREGION 'Other public methods'}

{$REGION 'T_PvlViewerBmp specific'}

procedure T_PvlViewerBmp.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oControl := TGIS_ViewerBmp.Create ;
end;

{$ENDREGION 'T_PvlViewerBmp specific'}

initialization
  RegisterPVLPlatformControl( 'ViewerBmp', T_PvlViewerBmp );

//==================================== END =====================================
end.

