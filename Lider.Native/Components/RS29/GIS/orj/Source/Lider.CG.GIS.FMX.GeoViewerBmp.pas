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
  FMX Viewer control.
}

unit FMX.GisViewerBmp;
{$HPPEMIT '#pragma link "FMX.GisViewerBmp"'}

interface

{$INCLUDE GisInclude.inc}

uses
  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.Types,
  System.Generics.Defaults,
  System.Generics.Collections,

  FMX.Types,
  FMX.Forms,
  {$IFDEF LEVEL_XE5_FMX}
    FMX.Graphics,
  {$ENDIF}

  GisInterfaces,
  GisTypes,
  GisTypesUI,
  GisViewer,
  GisCsSystems,
  GisRendererAbstract ;

type
  {#gendoc:hide}
  T_FMXBitmap = {$IFDEF LEVEL_XE5_FMX}
                  FMX.Graphics.TBitmap ;
                {$ELSE}
                  FMX.Types.TBitmap ;
                {$ENDIF}

type

  /// <summary>
  ///   Class responsible for map presentation on bitmap.
  /// </summary>
  /// <remarks>
  ///   For window output see TGIS_ViewerWnd class.
  /// </remarks>
  TGIS_ViewerBmp = class( TComponent, IGIS_Viewer, IGIS_ViewerParent,
                          IGIS_ViewerBmp )
    private
      oVwr           : TGIS_Viewer;
      oRenderer      : TGIS_RendererAbstract ;
      oBitmap        : T_FMXBitmap ;
      oGIS_Bitmap    : TGIS_Bitmap ;
      oContext       : TGIS_RendererContext;
      oTilePicture   : T_FMXBitmap ;
      FTileRect      : TRect ;
      bKeepContext   : Boolean ; //use only for Tiled draw

    private
      localBitmap  : Boolean ;

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

      /// <summary>
      ///   Mark center of opartion as center of current view.
      /// </summary>
      procedure markCenter      ;

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

    protected
      function  fget_SelectionColor     : TAlphaColor ;
      procedure fset_SelectionColor     ( const _value : TAlphaColor ) ;

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

      function  fget_BusyEvent          : TGIS_BusyEvent ;
      procedure fset_BusyEvent          ( const _value : TGIS_BusyEvent ) ;
      function  fget_ExtentChangeEvent  : TNotifyEvent ;
      procedure fset_ExtentChangeEvent  ( const _value : TNotifyEvent ) ;
      function  fget_VisibleExtentChangeEvent
                                        : TNotifyEvent ;
      procedure fset_VisibleExtentChangeEvent
                                        ( const _value : TNotifyEvent ) ;
      function  fget_ZoomChangeEvent    : TNotifyEvent ;
      procedure fset_ZoomChangeEvent    ( const _value : TNotifyEvent ) ;

    public
      /// <summary>
      ///   Standard constructor. Creates bitmap 320x240.
      /// </summary>
      constructor Create         ; overload;

      /// <summary>
      ///   Create an instance based on provided dimensions.
      /// </summary>
      /// <param name="_width">
      ///   bitmap width in pixels
      /// </param>
      /// <param name="_height">
      ///   bitmap height in pixels
      /// </param>
      constructor Create         ( _width, _height : Integer
                                 ) ; overload;

      /// <summary>
      ///   Create an instance to be set on provided existing bitmap.
      /// </summary>
      /// <param name="_bitmap">
      ///   bitmap to be used
      /// </param>
      constructor Create         ( const _bitmap  : TBitmap
                                 ) ; overload;

      /// <summary>
      ///   Standard destructor.
      /// </summary>
      destructor Destroy         ; override;

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

    public // IGIS_ViewerParent public methods

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ControlClose           ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ControlDrawTexture     (       _bmp     : TObject     ;
                                          const _extent : TGIS_Extent ;
                                         const _ppi     : Integer
                                       ) ; overload ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure ControlDrawTexture     (       _bmp    : TObject     ;
                                         const _layer  : TGIS_LayerAbstract ;
                                         const _extent : TGIS_Extent ;
                                         const _ppi    : Integer
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
      procedure ControlExtentChanged ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure SetSize( const _width  : Integer ;
                         const _height : Integer
                        ) ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  SetViewer ( const _viewer : TObject ) : TObject ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  GetViewer : TObject ;

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

    public
      /// <summary>
      ///   Print the current content on a bitmap.
      ///   Print area will match the current VisibleExtent of the control.
      /// </summary>
      /// <param name="_bmp">
      ///   bitmap on which the drawing will be performed
      /// </param>
      procedure PrintBmp         ( var   _bmp   : {$IFDEF LEVEL_XE5_FMX}
                                                    FMX.Graphics.TBitmap
                                                  {$ELSE}
                                                    FMX.Types.TBitmap
                                                  {$ENDIF}
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
      procedure PrintBmp         ( var   _bmp   : {$IFDEF LEVEL_XE5_FMX}
                                                    FMX.Graphics.TBitmap ;
                                                  {$ELSE}
                                                    FMX.Types.TBitmap ;
                                                  {$ENDIF}
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

      /// <summary>
      ///   Context management used only internally.
      /// </summary>
      property KeepContextInternal : Boolean
                                           read  fget_KeepContextInternal
                                           write fset_KeepContextInternal;

      /// <summary>
      ///   Context management used only internally.
      /// </summary>
      property ContextInternal : TObject   read fget_ContextInternal ;


    public

      // public properties new for TGIS_ViewerBmp

      /// <summary>
      ///   Picture to be used as a background.
      /// </summary>
      property TilePicture : T_FMXBitmap   read  oTilePicture
                                           write oTilePicture ;

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
                                           read  oRenderer
                                           write fset_Renderer ;

      /// <summary>
      ///   Color used for selected objects.
      /// </summary>
      property SelectionColor : TAlphaColor read  fget_SelectionColor
                                            write fset_SelectionColor ;

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
      property ExtentChangeEvent : TNotifyEvent read  fget_ExtentChangeEvent
                                                write fset_ExtentChangeEvent ;

      /// <event/>
      /// <summary>
      ///   VisibleExtentChange event. Will be fired before BeforePaint if
      ///   TGIS_Viewer.VisibleExtent was changed. Will not be fired if
      ///   TGIS_Viewer.VisibleExtent was changed based on changed Zoom - in
      ///   such situation only ZoomChange will be fired.
      /// </summary>
      property VisibleExtentChangeEvent : TNotifyEvent
                                          read  fget_VisibleExtentChangeEvent
                                          write fset_VisibleExtentChangeEvent ;

      /// <event/>
      /// <summary>
      ///   ZoomChange event. Will be fired before OnBeforePaint if
      ///   Viewer.Zoom was changed.
      /// </summary>
      property ZoomChangeEvent : TNotifyEvent  read  fget_ZoomChangeEvent
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

  end ;

//##############################################################################
implementation

uses
  GisFunctions,
  GisRtl,
  GisEditor,
  GisLayer,
  FMX.GisFramework,
  FMX.GisRenderer ;

const
  LINE_SCROLL = 20 ;
  PAGE_SCROLL = 90 ;
  ZOOM_FACTOR = 1.8 ;

//==============================================================================
// private methods
//==============================================================================

  procedure TGIS_ViewerBmp.markCenter ;
  var
    pt_x : Integer ;
    pt_y : Integer ;
  begin
    if oVwr.IsEmpty then exit ;

    // prepare center of screen
      pt_x := oBitmap.Width  div 2 ;
      pt_y := oBitmap.Height div 2 ;
    // set the new center of window
      oVwr.CenterPtg := ScreenToMap( TPoint.Create( pt_x, pt_y ) ) ;
  end ;

//==============================================================================
// properties access functions of IGIS_Viewer
//==============================================================================

  function TGIS_ViewerBmp.fget_AutoStyle
    : Boolean ;
  begin
    Result := oVwr.AutoStyle ;
  end ;

  procedure TGIS_ViewerBmp.fset_AutoStyle(
    const _value : Boolean
  ) ;
  begin
    oVwr.AutoStyle := _value;
  end ;

  function TGIS_ViewerBmp.fget_BigExtent
    : TGIS_Extent ;
  begin
    Result := oVwr.BigExtent ;
  end ;

  function TGIS_ViewerBmp.fget_BigExtentMargin
    : Integer ;
  begin
    Result := oVwr.BigExtentMargin ;
  end ;

  function TGIS_ViewerBmp.fget_KeepScale
    : Boolean ;
  begin
    Result := oVwr.KeepScale ;
  end ;

  procedure TGIS_ViewerBmp.fset_KeepScale(
    const _value : Boolean
  ) ;
  begin
    oVwr.KeepScale := False ;
  end ;

  function TGIS_ViewerBmp.fget_Bitmap: TObject;
  begin
    Result := oBitmap ;
  end ;

  function TGIS_ViewerBmp.fget_GIS_Bitmap
    : TGIS_Bitmap ;
  begin
    FreeObject( oGIS_Bitmap ) ;
    oGIS_Bitmap := TGIS_Bitmap.Create(1,1);
    oGIS_Bitmap.LoadFromBitmap( oBitmap, '' ) ;
    Result := oGIS_Bitmap ;
  end ;

  function  TGIS_ViewerBmp.fget_TileRect
    : TRect ;
  begin
    Result := FTileRect ;
  end ;

  procedure TGIS_ViewerBmp.fset_TileRect(
    const _val : TRect
  ) ;
  begin
    FTileRect := _val ;
  end ;

  function TGIS_ViewerBmp.fget_Width
    : Integer ;
    begin
    Result := oBitmap.Width ;
  end;

  function TGIS_ViewerBmp.fget_Height
    : Integer ;
  begin
    Result := oBitmap.Height ;
  end;

  function  TGIS_ViewerBmp.fget_KeepContextInternal
    : Boolean ;
  begin
    Result := bKeepContext ;
  end;

  procedure TGIS_ViewerBmp.fset_KeepContextInternal(
    const _val   : Boolean
  ) ;
  begin
    bKeepContext := _val ;
  end;

  function  TGIS_ViewerBmp.fget_ContextInternal
    : TObject ;
  begin
    Result := oContext;
  end;

  procedure TGIS_ViewerBmp.fset_BigExtentMargin(
    const _value : Integer
  ) ;
  begin
    oVwr.BigExtentMargin := _value ;
  end ;

  function TGIS_ViewerBmp.fget_BusyLevel
    : Integer ;
  begin
    Result := oVwr.BusyLevel ;
  end ;

  function TGIS_ViewerBmp.fget_BusyText
    : String ;
  begin
    Result := oVwr.BusyText ;
  end ;

  function TGIS_ViewerBmp.fget_Center
    : TGIS_Point  ;
  begin
    Result := oVwr.Center ;
  end ;

  procedure TGIS_ViewerBmp.fset_Center(
    const _value : TGIS_Point
  ) ;
  begin
    oVwr.Center := _value ;
  end ;

  function TGIS_ViewerBmp.fget_CenterPtg
    : TGIS_Point  ;
  begin
    Result := oVwr.CenterPtg ;
  end ;

  procedure TGIS_ViewerBmp.fset_CenterPtg(
    const _value : TGIS_Point
  ) ;
  begin
    oVwr.CenterPtg := _value ;
  end ;

  function TGIS_ViewerBmp.fget_Color
    : TGIS_Color ;
  begin
    Result := oVwr.Color ;
  end ;

  procedure TGIS_ViewerBmp.fset_Color(
    const _value : TGIS_Color
  ) ;
  begin
    oVwr.Color := _value ;
  end ;

  function TGIS_ViewerBmp.fget_Copyright
    : String ;
  begin
    Result := oVwr.Copyright ;
  end ;

  function TGIS_ViewerBmp.fget_CS
    : TGIS_CSCoordinateSystem ;
  begin
    Result := oVwr.CS ;
  end ;

  procedure TGIS_ViewerBmp.fset_CS(
    const _value : TGIS_CSCoordinateSystem
  ) ;
  begin
    oVwr.CS := _value ;
  end ;

  function TGIS_ViewerBmp.fget_CustomPPI
    : Integer ;
  begin
    Result := oVwr.CustomPPI ;
  end ;

  procedure TGIS_ViewerBmp.fset_CustomPPI(
    const _value : Integer
  ) ;
  begin
    oVwr.CustomPPI := _value ;
  end ;

  function TGIS_ViewerBmp.fget_Editor
    : IGIS_Editor ;
  begin
    Result := oVwr.Editor ;
  end ;

  procedure TGIS_ViewerBmp.fset_Editor(
    const _value : IGIS_Editor
  ) ;
  begin
    if _value is TGIS_Editor then
      oVwr.Editor := TGIS_Editor( _value ) ;
  end ;

  function TGIS_ViewerBmp.fget_Extent
    : TGIS_Extent ;
  begin
    Result := oVwr.Extent ;
  end ;

  function TGIS_ViewerBmp.fget_FileCopyrights
    : String      ;
  begin
    Result := oVwr.FileCopyrights ;
  end ;

  function TGIS_ViewerBmp.fget_FontScale
    : Integer ;
  begin
    Result := oVwr.FontScale ;
  end ;

  procedure TGIS_ViewerBmp.fset_FontScale(
    const _value : Integer
  ) ;
  begin
    oVwr.FontScale := _value ;
  end ;

  function TGIS_ViewerBmp.fget_FullDrawExtent
    : TGIS_Extent ;
  begin
    Result := oVwr.FullDrawExtent ;
  end ;

  function TGIS_ViewerBmp.fget_IncrementalPaint
    : Boolean ;
  begin
    Result := oVwr.IncrementalPaint ;
  end ;

  procedure TGIS_ViewerBmp.fset_IncrementalPaint(
    const _value : Boolean
  ) ;
  begin
    oVwr.IncrementalPaint := False ;
  end ;

  function TGIS_ViewerBmp.fget_TiledPaint
    : Boolean ;
  begin
    Result := oVwr.TiledPaint ;
  end ;

  procedure TGIS_ViewerBmp.fset_TiledPaint(
    const _value : Boolean
  ) ;
  begin
    oVwr.TiledPaint := False ;
  end ;

  function TGIS_ViewerBmp.fget_InPaint
    : Boolean ;
  begin
    Result := oVwr.InPaint ;
  end ;

  function TGIS_ViewerBmp.fget_IsBusy
    : Boolean ;
  begin
    Result := oVwr.IsBusy ;
  end ;

  function TGIS_ViewerBmp.fget_IsEmpty
    : Boolean ;
  begin
    Result := oVwr.IsEmpty ;
  end ;

  function TGIS_ViewerBmp.fget_IsLocked
   : Boolean ;
  begin
    Result := oVwr.IsLocked ;
  end ;

  function TGIS_ViewerBmp.fget_IsTopmost
   : Boolean ;
  begin
    Result := oVwr.IsTopmost ;
  end ;

  function TGIS_ViewerBmp.fget_Items
    : TGIS_LayerAbstractList ;
  begin
    Result := oVwr.Items ;
  end ;

  function TGIS_ViewerBmp.fget_LabelsReg
    : TGIS_LabelsAreaAbstract ;
  begin
    Result := oVwr.LabelsReg ;
  end ;

  procedure TGIS_ViewerBmp.fset_Level(
    const _value : Double
  ) ;
  begin
    oVwr.Level := _value ;
  end ;

  function TGIS_ViewerBmp.fget_Level
    : Double ;
  begin
    Result := oVwr.Level ;
  end ;

  function TGIS_ViewerBmp.fget_MultiUserMode
    : TGIS_MultiUser ;
  begin
    Result := oVwr.MultiUserMode ;
  end ;

  procedure TGIS_ViewerBmp.fset_MultiUserMode(
    const _value : TGIS_MultiUser
  ) ;
  begin
    oVwr.MultiUserMode := _value ;
  end ;

  function TGIS_ViewerBmp.fget_CustomData
    : TGIS_StringList ;
  begin
    Result := oVwr.CustomData ;
  end ;

  function TGIS_ViewerBmp.fget_OverlappedExtentMargin
    : Integer ;
  begin
    Result := oVwr.OverlappedExtentMargin ;
  end ;

  procedure TGIS_ViewerBmp.fset_OverlappedExtentMargin(
    const _value : Integer
  ) ;
  begin
    oVwr.OverlappedExtentMargin := _value ;
  end ;

  function TGIS_ViewerBmp.fget_PPI
    : Integer ;
  begin
    Result := oVwr.PPI ;
  end ;

  function TGIS_ViewerBmp.fget_ProjectFile
    : TGIS_ConfigAbstract ;
  begin
    Result := oVwr.ProjectFile
  end ;

  procedure TGIS_ViewerBmp.fset_ProjectFile(
    const _value : TGIS_ConfigAbstract
  ) ;
  begin
    oVwr.ProjectFile := _value ;
  end ;

  function TGIS_ViewerBmp.fget_ProjectName
    : String ;
  begin
    Result := oVwr.ProjectName
  end ;

  function TGIS_ViewerBmp.fget_DelayedUpdate
    : Integer ;
  begin
    Result := oVwr.DelayedUpdate ;
  end;

  procedure TGIS_ViewerBmp.fset_DelayedUpdate(
    const _value : Integer
  ) ;
  begin
    oVwr.DelayedUpdate := 0 ;
  end;

  function TGIS_ViewerBmp.fget_ProgressiveUpdate
    : Integer ;
  begin
    Result := oVwr.ProgressiveUpdate ;
  end;

  procedure TGIS_ViewerBmp.fset_ProgressiveUpdate(
    const _value : Integer
  ) ;
  begin
    oVwr.ProgressiveUpdate := 0 ;
  end;

  function TGIS_ViewerBmp.fget_RestrictedDrag
    : Boolean ;
  begin
    Result := oVwr.RestrictedDrag ;
  end ;

  procedure TGIS_ViewerBmp.fset_RestrictedDrag(
    const _value : Boolean
  ) ;
  begin
    oVwr.RestrictedDrag := _value ;
  end ;

  function TGIS_ViewerBmp.fget_RestrictedExtent
    : TGIS_Extent ;
  begin
    Result := oVwr.RestrictedExtent ;
  end ;

  procedure TGIS_ViewerBmp.fset_RestrictedExtent(
    const _value : TGIS_Extent
  ) ;
  begin
    oVwr.RestrictedExtent := _value ;
  end ;

  function TGIS_ViewerBmp.fget_RotationAngle
    : Double ;
  begin
    Result := oVwr.RotationAngle ;
  end ;

  procedure TGIS_ViewerBmp.fset_RotationAngle(
    const _value : Double
  ) ;
  begin
    oVwr.RotationAngle := _value ;
  end ;

  function TGIS_ViewerBmp.fget_RotationPoint
    : TGIS_Point ;
  begin
    Result := oVwr.RotationPoint ;
  end ;

  procedure TGIS_ViewerBmp.fset_RotationPoint(
    const _value : TGIS_Point
  ) ;
  begin
    oVwr.RotationPoint := _value ;
  end ;

  function TGIS_ViewerBmp.fget_ScaleAsFloat
    : Double ;
  begin
    Result := oVwr.ScaleAsFloat ;
  end ;

  procedure TGIS_ViewerBmp.fset_ScaleAsFloat(
    const _value : Double
  ) ;
  begin
    oVwr.ScaleAsFloat := _value ;
  end ;

  function TGIS_ViewerBmp.fget_ScaleAsText
    : String ;
  begin
    Result := oVwr.ScaleAsText ;
  end ;

  procedure TGIS_ViewerBmp.fset_ScaleAsText(
    const _value : String
  ) ;
  begin
    oVwr.ScaleAsText := _value ;
  end ;

  function TGIS_ViewerBmp.fget_SelectionGisColor
    : TGIS_Color ;
  begin
    Result := oVwr.SelectionGisColor ;
  end ;

  procedure TGIS_ViewerBmp.fset_SelectionGisColor(
    const _value : TGIS_Color
  ) ;
  begin
    oVwr.SelectionGisColor := _value ;
  end ;

  function TGIS_ViewerBmp.fget_SelectionOutlineOnly
    : Boolean ;
  begin
    Result := oVwr.SelectionOutlineOnly ;
  end ;

  procedure TGIS_ViewerBmp.fset_SelectionOutlineOnly(
    const _value : Boolean
  ) ;
  begin
    oVwr.SelectionOutlineOnly := _value ;
  end ;

  function TGIS_ViewerBmp.fget_SelectionTransparency
    : Integer ;
  begin
    Result := oVwr.SelectionTransparency ;
  end ;

  procedure TGIS_ViewerBmp.fset_SelectionTransparency(
    const _value : Integer
  ) ;
  begin
    oVwr.SelectionTransparency := _value ;
  end ;

  function TGIS_ViewerBmp.fget_SelectionWidth
    : Integer ;
  begin
    Result := oVwr.SelectionWidth ;
  end ;

  procedure TGIS_ViewerBmp.fset_SelectionWidth(
    const _value : Integer
  ) ;
  begin
    oVwr.SelectionWidth := _value ;
  end ;

  function TGIS_ViewerBmp.fget_SystemPPI
    : Integer ;
  begin
    Result := oVwr.SystemPPI ;
  end ;

  function TGIS_ViewerBmp.fget_ViewerParent
    : IGIS_ViewerParent ;
  begin
    Result := Self ;
  end ;

  function TGIS_ViewerBmp.fget_ViewerParentRoot
    : IGIS_ViewerParent ;
  begin
    Result := oVwr.ViewerParentRoot ;
  end ;

  function TGIS_ViewerBmp.fget_Viewport
    : TGIS_Point ;
  begin
    Result := oVwr.Viewport ;
  end ;

  procedure TGIS_ViewerBmp.fset_Viewport(
    const _value : TGIS_Point
  ) ;
  begin
    oVwr.Viewport := _value ;
  end ;

  function TGIS_ViewerBmp.fget_VisibleExtent
    : TGIS_Extent ;
  begin
    Result := oVwr.VisibleExtent ;
  end ;

  procedure TGIS_ViewerBmp.fset_VisibleExtent(
    const _value : TGIS_Extent
  ) ;
  begin
    oVwr.VisibleExtent := _value ;
  end ;

  function TGIS_ViewerBmp.fget_UseAnimations
    : Boolean ;
  begin
    Result := oVwr.UseAnimations ;
  end ;

  procedure TGIS_ViewerBmp.fset_UseAnimations(
    const _value : Boolean
  ) ;
  begin
    oVwr.UseAnimations := _value ;
  end ;

  function TGIS_ViewerBmp.fget_UseRTree
    : Boolean ;
  begin
    Result := oVwr.UseRTree ;
  end ;

  procedure TGIS_ViewerBmp.fset_UseRTree(
    const _value : Boolean
  ) ;
  begin
    oVwr.UseRTree := _value ;
  end ;

  function TGIS_ViewerBmp.fget_Zoom
    : Double ;
  begin
    Result := oVwr.Zoom ;
  end ;

  procedure TGIS_ViewerBmp.fset_Zoom(
    const _value : Double
  ) ;
  begin
    oVwr.Zoom := _value ;
  end ;

  function TGIS_ViewerBmp.fget_ZoomEx
    : Double ;
  begin
    Result := oVwr.ZoomEx ;
  end ;

  procedure TGIS_ViewerBmp.fset_ZoomEx(
    const _value : Double
  ) ;
  begin
    oVwr.ZoomEx := _value ;
  end ;

  function TGIS_ViewerBmp.fget_MasterViewer
    : IGIS_Viewer ;
  begin
    Result := oVwr.MasterViewer ;
  end;

  procedure TGIS_ViewerBmp.fset_MasterViewer(
    const _value : IGIS_Viewer
  ) ;
  begin
    oVwr.MasterViewer := _value ;
  end;

  function TGIS_ViewerBmp.fget_UponDestroy
    : Boolean ;
  begin
    Result := oVwr.UponDestroy ;
  end ;

  function  TGIS_ViewerBmp.fget_TemporaryScaleInternal : Double      ;
  begin
    Result := oVwr.TemporaryScaleInternal ;
  end ;

  procedure TGIS_ViewerBmp.fset_TemporaryScaleInternal(
    const _value : Double
  ) ;
  begin
    oVwr.TemporaryScaleInternal := _value ;
  end ;

  function  TGIS_ViewerBmp.fget_TemporaryVisibleExtent : TGIS_Extent ;
  begin
    Result := oVwr.TemporaryVisibleExtent ;
  end ;

  procedure TGIS_ViewerBmp.fset_TemporaryVisibleExtent(
    const _value : TGIS_Extent
  ) ;
  begin
    oVwr.TemporaryVisibleExtent := _value ;
  end ;

  function TGIS_ViewerBmp.fget_Hierarchy
    : IGIS_HierarchyManager ;
  begin
    Result := oVwr.Hierarchy ;
  end ;

//==============================================================================
// reintroduced properties
//==============================================================================

  function TGIS_ViewerBmp.fget_SelectionColor
    : TAlphaColor ;
  begin
    Result := oVwr.SelectionGisColor.ARGB ;
  end ;

  procedure TGIS_ViewerBmp.fset_SelectionColor(
    const _value : TAlphaColor
  ) ;
  begin
    oVwr.SelectionGisColor := TGIS_Color.FromARGB( _value ) ;
  end ;

//==============================================================================
// new properties access routines of TGIS_ViewerBmp
//==============================================================================

  procedure TGIS_ViewerBmp.fset_Renderer(
    const _value : TGIS_RendererAbstract
  ) ;
  begin
    assert( assigned( _value ) ) ;
    FreeObject( oRenderer ) ;
    oRenderer := _value ;
  end ;

//==============================================================================
// property access routines of TGIS_Viewer's event handlers
//==============================================================================

  function TGIS_ViewerBmp.fget_BusyEvent
    : TGIS_BusyEvent ;
  begin
    Result := oVwr.BusyEvent ;
  end ;

  procedure TGIS_ViewerBmp.fset_BusyEvent(
    const _value : TGIS_BusyEvent
  ) ;
  begin
    oVwr.BusyEvent := _value ;
  end ;

  function TGIS_ViewerBmp.fget_ExtentChangeEvent
    : TNotifyEvent ;
  begin
    Result := oVwr.ExtentChangeEvent ;
  end ;

  procedure TGIS_ViewerBmp.fset_ExtentChangeEvent(
    const _value : TNotifyEvent
  ) ;
  begin
    oVwr.ExtentChangeEvent := _value ;
  end ;

  function TGIS_ViewerBmp.fget_VisibleExtentChangeEvent
    : TNotifyEvent ;
  begin
    Result := oVwr.VisibleExtentChangeEvent ;
  end ;

  procedure TGIS_ViewerBmp.fset_VisibleExtentChangeEvent(
    const _value : TNotifyEvent
  ) ;
  begin
    oVwr.VisibleExtentChangeEvent := _value ;
  end ;

  function TGIS_ViewerBmp.fget_ZoomChangeEvent
    : TNotifyEvent ;
  begin
    Result := oVwr.ZoomChangeEvent ;
  end ;

  procedure TGIS_ViewerBmp.fset_ZoomChangeEvent(
    const _value : TNotifyEvent
  ) ;
  begin
    oVwr.ZoomChangeEvent := _value ;
  end ;

//==============================================================================
// public methods of IGIS_Viewer
//==============================================================================

  function  TGIS_ViewerBmp.ChangeHash
    : Int64 ;
  begin
    Result := oVwr.ChangeHash ;
  end ;

  procedure TGIS_ViewerBmp.Subscribe(
    const _control : IGIS_Subscribe
  ) ;
  begin
    oVwr.Subscribe( _control ) ;
  end ;

  procedure TGIS_ViewerBmp.UnSubscribe(
    const _control : IGIS_Subscribe
  ) ;
  begin
    oVwr.Unsubscribe( _control ) ;
  end ;

  procedure TGIS_ViewerBmp.NotifySubscribers(
    const _event   : Integer ;
    const _context : TObject
  ) ;
  begin
    oVwr.NotifySubscribers( _event, _context ) ;
  end ;

  function TGIS_ViewerBmp.NotifyPaintException(
    const _message   : String ;
    const _exception : Exception
  ) : Boolean ;
  begin
    Result := oVwr.NotifyPaintException( _message, _exception ) ;
  end ;

  procedure TGIS_ViewerBmp.Lock ;
  begin
    oVwr.Lock ;
  end ;

  procedure TGIS_ViewerBmp.Unlock ;
  begin
    oVwr.Unlock ;
  end ;

  procedure TGIS_ViewerBmp.Unlock(
    const _redraw : Boolean
  ) ;
  begin
    oVwr.Unlock( _redraw ) ;
  end ;

  procedure TGIS_ViewerBmp.Interrupt ;
  begin
    oVwr.Interrupt ;
  end ;

  function TGIS_ViewerBmp.Interrupted
    : Boolean ;
  begin
    Result := oVwr.Interrupted ;
  end ;

  function  TGIS_ViewerBmp.HourglassActive
    : Boolean ;
  begin
    Result := oVwr.HourglassActive ;
  end ;

  procedure TGIS_ViewerBmp.HourglassPrepare ;
  begin
    oVwr.HourglassPrepare ;
  end ;

  procedure TGIS_ViewerBmp.HourglassRelease ;
  begin
    oVwr.HourglassRelease ;
  end ;

  function  TGIS_ViewerBmp.HourglassShake
    : Boolean ;
  begin
    Result := oVwr.HourglassShake ;
  end ;

  procedure TGIS_ViewerBmp.HourglassRestart ;
  begin
    oVwr.HourglassRestart ;
  end;

  procedure TGIS_ViewerBmp.BusyPrepare(
    _sender : TObject ;
    _text   : String
  ) ;
  begin
    oVwr.BusyPrepare( _sender, _text ) ;
  end ;

  procedure TGIS_ViewerBmp.BusyRelease(
    _sender : TObject
  ) ;
  begin
    oVwr.BusyRelease( _sender ) ;
  end ;

  procedure TGIS_ViewerBmp.BusyShake(
        _sender : TObject ;
        _pos    : Int64 ;
        _end    : Int64 ;
    var _abort  : Boolean
  ) ;
  begin
    oVwr.BusyShake( _sender, _pos, _end, _abort ) ;
  end ;

  procedure TGIS_ViewerBmp.RaiseBusyEvent(
        _sender  : TObject ;
        _pos     : Int64 ;
        _end     : Int64;
    var _abort   : Boolean
  ) ;
  begin
    oVwr.RaiseBusyEvent( _sender, _pos, _end, _abort ) ;
  end ;

  procedure TGIS_ViewerBmp.RaiseHelpEvent(
        _sender  : TObject ;
        _name    : String
  ) ;
  begin
    oVwr.RaiseHelpEvent( _sender, _name ) ;
  end ;

  function TGIS_ViewerBmp.AssignedBusyEvent
    : TGIS_BusyEvent ;
  begin
    Result := oVwr.AssignedBusyEvent() ;
  end ;

  function TGIS_ViewerBmp.AssignedHelpEvent
    : TGIS_HelpEvent ;
  begin
    Result := oVwr.AssignedHelpEvent() ;
  end ;

  function TGIS_ViewerBmp.StorePaintState
    : TObject ;
  begin
    Result := oVwr.StorePaintState ;
  end ;

  procedure TGIS_ViewerBmp.RestorePaintState(
    var _state : TObject
  ) ;
  begin
    oVwr.RestorePaintState( _state ) ;
  end ;

  procedure TGIS_ViewerBmp.BeginPaintInternal ;
  begin
    oVwr.BeginPaintInternal ;
  end ;

  procedure TGIS_ViewerBmp.EndPaintInternal ;
  begin
    oVwr.EndPaintInternal ;
  end ;

  function TGIS_ViewerBmp.SynchronizePaint(
    const _interrupt : Boolean
  ) : Boolean ;
  begin
    Result := oVwr.SynchronizePaint( _interrupt ) ;
  end;

  procedure TGIS_ViewerBmp.ReParentLock ;
  begin
    oVwr.ReParentLock ;
  end ;

  procedure TGIS_ViewerBmp.ReParentUnlock ;
  begin
    oVwr.ReParentUnlock ;
  end ;

  function TGIS_ViewerBmp.ReParent(
    const _parent : IGIS_ViewerParent
  ) : IGIS_ViewerParent ;
  begin
    Result := oVwr.ReParent( _parent ) ;
  end ;

  function TGIS_ViewerBmp.AttachLayer(
    const _layer : TGIS_LayerAbstract
  ) : IGIS_Viewer ;
  begin
    Result := oVwr.AttachLayer( _layer ) ;
  end ;

  procedure TGIS_ViewerBmp.Open(
    const _path : String
  ) ;
  begin
    oVwr.Open( _path ) ;
  end ;

  procedure TGIS_ViewerBmp.Open(
    const _path   : String ;
    const _strict : Boolean
  ) ;
  begin
    oVwr.Open( _path, _strict  ) ;
  end ;

  procedure TGIS_ViewerBmp.OpenEx(
    const _configFile : TGIS_ConfigAbstract ;
    const _path       : String
  ) ;
  begin
    oVwr.OpenEx( _configFile, _path ) ;
  end ;

  procedure TGIS_ViewerBmp.OpenEx(
    const _configFile : TGIS_ConfigAbstract ;
    const _path       : String              ;
    const _strict     : Boolean
  ) ;
  begin
    oVwr.OpenEx( _configFile, _path, _strict ) ;
  end ;

  procedure TGIS_ViewerBmp.Close ;
  begin
    oVwr.Close ;
  end ;

  procedure TGIS_ViewerBmp.ReadConfig ;
  begin
    oVwr.ReadConfig ;
  end ;

  procedure TGIS_ViewerBmp.RereadConfig ;
  begin
    oVwr.RereadConfig ;
  end ;

  procedure TGIS_ViewerBmp.WriteConfig ;
  begin
    oVwr.WriteConfig ;
  end ;

  procedure TGIS_ViewerBmp.Add(
    const _layer : TGIS_LayerAbstract
  ) ;
  begin
    oVwr.Add( _layer ) ;
  end ;

  function TGIS_ViewerBmp.Get(
    const _name    : String
  ) : TGIS_LayerAbstract ;
  begin
    Result := oVwr.Get( _name ) ;
  end ;

  procedure TGIS_ViewerBmp.Delete(
    const _name : String
  ) ;
  begin
    oVwr.Delete( _name ) ;
  end ;

  procedure TGIS_ViewerBmp.AddHierarchy ;
  begin
    oVwr.AddHierarchy ;
  end ;

  procedure TGIS_ViewerBmp.Draw(
    const _renderer : TObject     ;
    const _mode     : TGIS_DrawMode
  ) ;
  begin
    oVwr.Draw( _renderer, _mode ) ;
  end ;

  function TGIS_ViewerBmp.GetGrid(
    const _extent : TGIS_Extent ;
    const _grid   : TGIS_GridArray
  ) : Boolean ;
  begin
    Result := oVwr.GetGrid( _extent, _grid ) ;
  end ;

  procedure TGIS_ViewerBmp.RevertAll ;
  begin
    oVwr.RevertAll ;
  end ;

  procedure TGIS_ViewerBmp.SaveProject ;
  begin
    oVwr.SaveProject ;
  end ;

  procedure TGIS_ViewerBmp.SaveProject(
    const _relativepath : Boolean
  ) ;
  begin
    oVwr.SaveProject( _relativepath ) ;
  end ;

  procedure TGIS_ViewerBmp.SaveProjectAs(
    const _path : String
  ) ;
  begin
    oVwr.SaveProjectAs( _path ) ;
  end ;

  procedure TGIS_ViewerBmp.SaveProjectAs(
    const _path         : String ;
    const _relativepath : Boolean
  ) ;
  begin
    oVwr.SaveProjectAs( _path, _relativepath ) ;
  end ;

  procedure TGIS_ViewerBmp.SaveProjectAsEx(
    const _configFile : TGIS_ConfigAbstract ;
    const _path       : String
  ) ;
  begin
    oVwr.SaveProjectAsEx( _configFile, _path ) ;
  end ;

  procedure TGIS_ViewerBmp.SaveProjectAsEx(
    const _configFile   : TGIS_ConfigAbstract ;
    const _path         : String              ;
    const _relativepath : Boolean
  ) ;
  begin
    oVwr.SaveProjectAsEx( _configFile, _path, _relativepath ) ;
  end ;

  procedure TGIS_ViewerBmp.SaveData ;
  begin
    oVwr.SaveData ;
  end ;

  procedure TGIS_ViewerBmp.SaveAll ;
  begin
    oVwr.SaveAll ;
  end ;

  function  TGIS_ViewerBmp.MustSave
    : Boolean ;
  begin
    Result := oVwr.MustSave ;
  end ;

  procedure TGIS_ViewerBmp.MarkModified ;
  begin
    oVwr.MarkModified ;
  end ;

  procedure TGIS_ViewerBmp.RecalcExtent ;
  begin
    oVwr.RecalcExtent ;
  end ;

  procedure TGIS_ViewerBmp.Reposition ;
  begin
    oVwr.Reposition ;
  end ;

  procedure TGIS_ViewerBmp.InvalidateExtent(
    const _extent : TGIS_Extent
  ) ;
  begin
    oVwr.InvalidateExtent( _extent );
  end ;

  procedure TGIS_ViewerBmp.InvalidateExtent(
    const _extent : TGIS_Extent ;
    const _deep   : Boolean
  ) ;
  begin
    oVwr.InvalidateExtent( _extent, _deep ) ;
  end ;

  procedure TGIS_ViewerBmp.InvalidateWholeMap ;
  begin
    oVwr.InvalidateWholeMap ;
  end ;

  procedure TGIS_ViewerBmp.InvalidateTopmost ;
  begin
    oVwr.InvalidateTopmost ;
  end ;

  procedure TGIS_ViewerBmp.InvalidateBasemap ;
  begin
    oVwr.InvalidateBasemap ;
  end ;

  procedure TGIS_ViewerBmp.InvalidateSelection ;
  begin
    oVwr.InvalidateSelection ;
  end ;

  procedure TGIS_ViewerBmp.InvalidateEditor(
    const _final : Boolean
  ) ;
  begin
    oVwr.InvalidateEditor( _final ) ;
  end ;

  function TGIS_ViewerBmp.FullExtentZoom
    : Double ;
  begin
    Result := oVwr.FullExtentZoom ;
  end ;

  procedure TGIS_ViewerBmp.FullExtent ;
  begin
    oVwr.FullExtent ;
  end ;

  function TGIS_ViewerBmp.Locate(
    const _ptg  : TGIS_Point ;
    const _prec : Double
  ) : TGIS_ShapeAbstract ;
  begin
    Result := oVwr.Locate( _ptg, _prec ) ;
  end ;

  function TGIS_ViewerBmp.Locate(
    const _ptg     : TGIS_Point ;
    const _prec    : Double     ;
    const _visible : Boolean
  ) : TGIS_ShapeAbstract ;
  begin
    Result := oVwr.Locate( _ptg, _prec, _visible ) ;
  end ;

  function TGIS_ViewerBmp.Locate(
    const _pt      : TPoint     ;
    const _prec    : Integer
  ) : TGIS_ShapeAbstract ;
  begin
    Result := oVwr.Locate( _pt, _prec ) ;
  end ;

  function TGIS_ViewerBmp.LocateEx(
    const _ptg     : TGIS_Point ;
    const _prec    : Double    ;
    const _visible : Boolean
  ) : TGIS_ShapeAbstractList ;
  begin
    Result := oVwr.LocateEx( _ptg, _prec, _visible ) ;
  end ;

  function TGIS_ViewerBmp.MapToScreen(
    const _ptg : TGIS_Point
  ) : TPoint ;
  begin
    Result := oVwr.MapToScreen( _ptg ) ;
  end ;

  function TGIS_ViewerBmp.MapToScreen3D(
    const _ptg : TGIS_Point3D
  ) : TPoint ;
  begin
    Result := oVwr.MapToScreen3D( _ptg ) ;
  end ;

  function TGIS_ViewerBmp.ScreenToMap(
    const _pt : TPoint
  ) : TGIS_Point ;
  begin
    Result := oVwr.ScreenToMap( _pt ) ;
  end ;

  function TGIS_ViewerBmp.ScreenToMap3D(
    const _pt : TPoint
  ) : TGIS_Point3D ;
  begin
    Result := oVwr.ScreenToMap3D( _pt ) ;
  end ;

  function TGIS_ViewerBmp.MapToScreenEx(
    const _pt : TGIS_Point
  ) : TGIS_Point ;
  begin
    Result := oVwr.MapToScreenEx( _pt ) ;
  end ;

  function TGIS_ViewerBmp.ScreenToMapEx(
    const _pt : TGIS_Point
  ) : TGIS_Point ;
  begin
    Result := oVwr.ScreenToMapEx( _pt ) ;
  end ;

  function TGIS_ViewerBmp.MapToScreenRect(
    const _rct : TGIS_Extent
  ) : TRect ;
  begin
    Result := oVwr.MapToScreenRect( _rct ) ;
  end ;

  function TGIS_ViewerBmp.ScreenToMapRect(
    const _rct : TRect
  ) : TGIS_Extent ;
  begin
    Result := oVwr.ScreenToMapRect( _rct ) ;
  end ;

  function TGIS_ViewerBmp.PixelsToTwips(
    const _size : Integer
  ) : Integer ;
  begin
    Result := oVwr.PixelsToTwips( _size ) ;
  end ;

  function TGIS_ViewerBmp.TwipsToPixels(
    const _size : Integer
  ) : Integer ;
  begin
    Result := oVwr.TwipsToPixels( _size ) ;
  end ;

  function TGIS_ViewerBmp.TwipsToPoints(
    const _size : Integer
  ) : Integer ;
  begin
    Result := oVwr.TwipsToPoints( _size ) ;
  end ;

  procedure TGIS_ViewerBmp.MoveViewport(
    var _dx : Integer ;
    var _dy : Integer
  ) ;
  begin
    oVwr.MoveViewport( _dx, _dy ) ;
  end ;

  procedure TGIS_ViewerBmp.MoveViewportEx(
    var _dx : Double ;
    var _dy : Double
  ) ;
  begin
    oVwr.MoveViewportEx( _dx, _dy ) ;
  end ;

  procedure TGIS_ViewerBmp.SetViewport(
    var _x : Double ;
    var _y : Double
  ) ;
  begin
    oVwr.SetViewport( _x, _y ) ;
  end ;

  procedure TGIS_ViewerBmp.CenterViewport(
    const _ptg : TGIS_Point
  ) ;
  begin
    oVwr.CenterViewport( _ptg ) ;
  end ;

  procedure TGIS_ViewerBmp.SetCSByWKT(
    const _wkt : String
  ) ;
  begin
    oVwr.SetCSByWKT( _wkt ) ;
  end ;

  procedure TGIS_ViewerBmp.SetCSByEPSG(
    const _epsg : Integer
  ) ;
  begin
    oVwr.SetCSByEPSG( _epsg ) ;
  end ;

  procedure TGIS_ViewerBmp.SetCSByWKTFile(
    const _path : String
  ) ;
  begin
    oVwr.SetCSByWKTFile( _path ) ;
  end ;

  function TGIS_ViewerBmp.RotatedPoint(
    const _ptg : TGIS_Point
  ) : TGIS_Point ;
  begin
    Result := oVwr.RotatedPoint( _ptg ) ;
  end ;

  function TGIS_ViewerBmp.UnrotatedPoint(
    const _ptg : TGIS_Point
  ) : TGIS_Point ;
  begin
    Result := oVwr.UnrotatedPoint( _ptg ) ;
  end ;

  function TGIS_ViewerBmp.RotatedPoint3D(
    const _ptg : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    Result := oVwr.RotatedPoint3D( _ptg ) ;
  end ;

  procedure TGIS_ViewerBmp.RotatedPoint3D_ref(
    var _ptg : TGIS_Point3D
  ) ;
  begin
    oVwr.RotatedPoint3D_ref( _ptg ) ;
  end ;

  function TGIS_ViewerBmp.UnrotatedPoint3D(
    const _ptg : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    Result := oVwr.UnrotatedPoint3D( _ptg ) ;
  end ;

  procedure TGIS_ViewerBmp.UnrotatedPoint3D_ref(
    var _ptg : TGIS_Point3D
  ) ;
  begin
    oVwr.UnrotatedPoint3D_ref( _ptg ) ;
  end ;

  function TGIS_ViewerBmp.RotatedExtent(
    const _extent : TGIS_Extent
  ) : TGIS_Extent ;
  begin
    Result := oVwr.RotatedExtent( _extent ) ;
  end ;

  function TGIS_ViewerBmp.UnrotatedExtent(
    const _extent : TGIS_Extent
  ) : TGIS_Extent ;
  begin
    Result := oVwr.UnrotatedExtent( _extent ) ;
  end ;

  function TGIS_ViewerBmp.GetRenderContext
    : TObject ;
  begin
    Result := oVwr.GetRenderContext ;
  end ;

  procedure TGIS_ViewerBmp.WaitForBackgroundProcesses ;
  begin
    oVwr.WaitForBackgroundProcesses ;
  end;

  procedure TGIS_ViewerBMp.WaitForNotBusy(
          _sender : TObject ;
    const _proc   : TGIS_WaitForNotBusyProc
  ) ;
  begin
    oVwr.WaitForNotBusy( _sender, _proc ) ;
  end;

//==============================================================================
// public methods of IGIS_ViewerParent
//==============================================================================

  procedure TGIS_ViewerBmp.ControlClose ;
  begin
    // do nothing
  end ;

  procedure TGIS_ViewerBmp.ControlDrawTexture(
          _bmp : TObject     ;
    const _extent : TGIS_Extent ;
    const _ppi : Integer
  ) ;
  begin
    // do nothing
  end ;

  procedure TGIS_ViewerBmp.ControlDrawTexture(
          _bmp   : TObject     ;
    const _layer : TGIS_LayerAbstract ;
    const _extent: TGIS_Extent ;
    const _ppi   : Integer
  ) ;
  begin
    // do nothing
  end ;

  function TGIS_ViewerBmp.ControlRenderer ;
  begin
    Result := oRenderer ;
  end ;

  procedure TGIS_ViewerBmp.ControlFlash(
    const _times : Integer ;
    const _delay : Integer
  ) ;
  begin
    // do nothing
  end ;

  function TGIS_ViewerBmp.ControlSystemPPI
    : Integer ;
  begin
    Result := 96 ;
  end ;

  function TGIS_ViewerBmp.ControlPPI
    : Integer ;
  begin
    Result := PPI ;
  end ;

  procedure TGIS_ViewerBmp.ControlRepaint ;
  begin
    // do nothing
  end ;

  procedure TGIS_ViewerBmp.ControlProcessMessages ;
  begin
    // do nothing
  end ;

  function TGIS_ViewerBmp.ControlUpdateSynchronize(
    const _interrupt : Boolean
  ) : Boolean ;
  begin
    Result := True ; // always synchronic
  end;

  procedure TGIS_ViewerBmp.ControlUpdateWholeMap ;
  begin
    Draw ;
  end ;

  procedure TGIS_ViewerBmp.ControlUpdateProgressive ;
  begin
    // do nothing
  end;

  procedure TGIS_ViewerBmp.ControlUpdateTopmost ;
  begin
    ControlUpdateWholeMap ;
  end ;

  procedure TGIS_ViewerBmp.ControlUpdateBasemap ;
  begin
    ControlUpdateWholeMap ;
  end ;

  procedure TGIS_ViewerBmp.ControlUpdateSelection;
  begin
    ControlUpdateWholeMap ;
  end ;

  procedure TGIS_ViewerBmp.ControlUpdateEditor(
    const _final : Boolean
  ) ;
  begin
    ControlUpdateWholeMap
  end ;

  function TGIS_ViewerBmp.ControlCanvasScale
    : Single ;
  begin
    Result := 1 ;
  end ;

  procedure TGIS_ViewerBmp.ControlAutoCenterViewport(
    const _dx, _dy: Double
  );
  begin
    // do nothing
  end ;

  procedure TGIS_ViewerBmp.ControlExtentChanged ;
  begin
    // do nothing
  end ;

  function TGIS_ViewerBmp.ControlCanvasHeight
    : Integer ;
  begin
    Result := oBitmap.Height ;
  end ;

  function TGIS_ViewerBmp.ControlCanvasWidth
    : Integer ;
  begin
    Result := oBitmap.Width ;
  end ;

  procedure TGIS_ViewerBmp.ControlHourglassShow ;
  begin
    // do nothing
  end ;

  procedure TGIS_ViewerBmp.ControlHourglassHide ;
  begin
    // do nothing
  end ;

  function TGIS_ViewerBmp.ControlHourglassShake
    : Boolean ;
  begin
    Result := False ;
  end ;

  procedure TGIS_ViewerBmp.ControlSet3DMode(
    const _mode : TGIS_Viewer3DMode
  ) ;
  begin
    // do nothing
  end ;

  procedure TGIS_ViewerBmp.ControlRaiseEditorChangeEvent(
    _sender : TObject
  ) ;
  begin
    // do nothing
  end ;

//==============================================================================
// public methods of IGIS_ViewerBmp
//==============================================================================

  procedure TGIS_ViewerBmp.ZoomIn ;
  begin
    oVwr.Zoom := oVwr.Zoom * ZOOM_FACTOR ;
  end ;

  procedure TGIS_ViewerBmp.ZoomOut ;
  begin
    oVwr.Zoom := oVwr.Zoom / ZOOM_FACTOR ;
  end ;

  procedure TGIS_ViewerBmp.Clear;
  begin
    if oVwr.Color.ARGB = TGIS_Color.None.ARGB then begin
      T_FMXBitmap(oBitmap).Clear( TAlphaColorRec.Null ) ;
    end
    else begin
      T_FMXBitmap(oBitmap).Clear( FMXColor( oVwr.Color ) ) ;
    end ;
  end ;

  procedure TGIS_ViewerBmp.Draw ;
  var
    metPixelExportDrawLabelSettings : Boolean ;
    metPaintTopmostLabelsOnTop      : Boolean ;
    metPaintLabelsOnTop             : Boolean ;

    procedure draw_with_topmost ;
    var
      ctx       : TGIS_RendererContext ;
      ctxLabels : T_FMXBitmap ;
    begin
      ctx := TGIS_RendererContext.Create ;
      try
        ctx.AssignBaseMap   ( oBitmap, False ) ;
        ctx.AssignSelection ( nil,     True  ) ;
        ctx.AssignCharts    ( nil,     True  ) ;
        ctx.AssignLabels    ( nil,     True  ) ;

        oRenderer.ReleaseContext ;

        oBitmap.Canvas.BeginScene ;
          if assigned( FOnBeforePaintRenderer ) then
            oRenderer.PaintExtra( Self, oBitmap.Canvas, FOnBeforePaintRenderer ) ;
          if assigned( FOnBeforePaint ) then
            FOnBeforePaint( Self, oBitmap.Canvas ) ;
        oBitmap.Canvas.EndScene ;

        oRenderer.CreateContext( self, self.oVwr, ctx, Point( 0, 0 ),
                                 oBitmap.Width, oBitmap.Height,
                                 PPI, FontScale, FTileRect
                               ) ;
        try

          if GisIsNoWorld( oVwr.TemporaryVisibleExtent ) then
            oVwr.LabelsReg.Reset ;
          oRenderer.PrepareDraw ;
          oVwr.BeginPaintInternal ;
          Draw( oRenderer, TGIS_DrawMode.AllExceptTop ) ;
        finally
          oVwr.EndPaintInternal ;
          oRenderer.AfterDraw ;

          oBitmap.Canvas.BeginScene ;
          try
            if not bKeepContext then begin
              if Assigned( ctx.Selection )then
                oBitmap.Canvas.DrawBitmap(
                  T_FMXBitmap(ctx.Selection),
                  RectF( 0, 0, T_FMXBitmap(ctx.Selection).Width,
                               T_FMXBitmap(ctx.Selection).Height),
                  RectF( 0, 0, oBitmap.Width,
                               oBitmap.Height
                       ),
                  oVwr.SelectionTransparency / 100, True
                ) ;
              if Assigned( ctx.Charts ) then
                oBitmap.Canvas.DrawBitmap(
                  T_FMXBitmap(ctx.Charts),
                  RectF( 0, 0, T_FMXBitmap(ctx.Charts).Width,
                               T_FMXBitmap(ctx.Charts).Height),
                  RectF( 0, 0, oBitmap.Width,
                               oBitmap.Height
                       ),
                  1, False
                ) ;
              if Assigned( ctx.Labels ) then begin
                ctxLabels := nil ;
                if not metPaintLabelsOnTop then
                  oBitmap.Canvas.DrawBitmap(
                    T_FMXBitmap(ctx.Labels),
                    RectF( 0, 0, T_FMXBitmap(ctx.Labels).Width,
                                 T_FMXBitmap(ctx.Labels).Height),
                    RectF( 0, 0, oBitmap.Width,
                                 oBitmap.Height
                         ),
                    1, False
                  )
                else begin
                  ctxLabels := T_FMXBitmap.Create( oBitmap.Width, oBitmap.Height ) ;
                  ctxLabels.Assign( T_FMXBitmap(ctx.Labels) ) ;
                end ;
              end ;
            end;
        finally
          oBitmap.Canvas.EndScene ;
        end ;

        oRenderer.ReleaseContext ;

        // topmost
        ctx.AssignSelection( nil, False ) ;
        ctx.AssignCharts   ( nil, False ) ;
        if metPaintTopmostLabelsOnTop then
          ctx.AssignLabels( nil, True )
        else
          ctx.AssignLabels( nil, False ) ;
        oRenderer.CreateContext( self, self.oVwr, ctx, Point( 0, 0 ),
                               RoundS( oBitmap.Width), RoundS( oBitmap.Height ),
                               PPI, FontScale, FTileRect
                             ) ;
        try
          oRenderer.PrepareDraw ;
          oVwr.BeginPaintInternal ;

          oVwr.Draw( oRenderer, TGIS_DrawMode.Top ) ;

        finally
          oVwr.EndPaintInternal ;
          oRenderer.AfterDraw ;
        end;

        if not bKeepContext then begin
          if Assigned( ctx.Labels ) then
          begin
            if not metPaintLabelsOnTop then begin
              if oBitmap.Canvas.BeginScene then begin
                try
                  oBitmap.Canvas.DrawBitmap(
                    T_FMXBitmap(ctx.Labels),
                    RectF( 0, 0, T_FMXBitmap(ctx.Labels).Width,
                                 T_FMXBitmap(ctx.Labels).Height),
                    RectF( 0, 0, oBitmap.Width,
                                 oBitmap.Height
                         ),
                    1, False
                  ) ;
                finally
                  oBitmap.Canvas.EndScene ;
                end;
              end;
            end;
          end;
        end;

        oBitmap.Canvas.BeginScene ;
        oRenderer.RenderEditor( oBitmap.Canvas ) ;
        oRenderer.PaintExtra( Self, oBitmap.Canvas, FOnPaintExtra ) ;
        oBitmap.Canvas.EndScene ;

        if ( not bKeepContext ) and metPaintLabelsOnTop then
        begin
          if oBitmap.Canvas.BeginScene then
            try
              if assigned( ctxLabels ) then
                oBitmap.Canvas.DrawBitmap(
                  ctxLabels,
                  RectF( 0, 0, ctxLabels.Width,
                               ctxLabels.Height ),
                  RectF( 0, 0, oBitmap.Width,
                               oBitmap.Height
                       ),
                  1, False
                ) ;
              if assigned( ctx.Labels ) then
                oBitmap.Canvas.DrawBitmap(
                  T_FMXBitmap(ctx.Labels),
                  RectF( 0, 0, T_FMXBitmap(ctx.Labels).Width,
                               T_FMXBitmap(ctx.Labels).Height ),
                  RectF( 0, 0, oBitmap.Width,
                               oBitmap.Height
                       ),
                  1, False
                ) ;
            finally
              oBitmap.Canvas.EndScene ;
            end;
        end ;

        oRenderer.ReleaseContext ;

        oBitmap.Canvas.BeginScene ;
          if assigned( FOnAfterPaint ) then
            FOnAfterPaint( Self, oBitmap.Canvas ) ;
          if assigned( FOnAfterPaintRenderer ) then
            oRenderer.PaintExtra( Self, oBitmap.Canvas, FOnAfterPaintRenderer ) ;
        oBitmap.Canvas.EndScene ;
      end ;
      finally
        FreeObject( ctxLabels ) ;
        if not bKeepContext then
          FreeObject( ctx ) ;
        oContext := ctx ;
      end ;
    end ;

    procedure draw_without_topmost ;
    var
      ctx : TGIS_RendererContext ;
    begin
      ctx := TGIS_RendererContext.Create ;
      try
        ctx.AssignBaseMap   ( oBitmap, False ) ;
        ctx.AssignSelection ( nil,     True  ) ;
        ctx.AssignCharts    ( nil,     True  ) ;
        ctx.AssignLabels    ( nil,     True  ) ;

        oRenderer.ReleaseContext ;

        oBitmap.Canvas.BeginScene ;
        if assigned( FOnBeforePaintRenderer ) then
          oRenderer.PaintExtra( Self, oBitmap.Canvas, FOnBeforePaintRenderer ) ;
        if assigned( FOnBeforePaint ) then
          FOnBeforePaint( Self, oBitmap.Canvas ) ;
        oBitmap.Canvas.EndScene ;

        oRenderer.CreateContext( self, self.oVwr, ctx, Point( 0, 0 ),
                                 oBitmap.Width, oBitmap.Height,
                                 PPI, FontScale, FTileRect
                               ) ;
        try
          if GisIsNoWorld( oVwr.TemporaryVisibleExtent ) then
            oVwr.LabelsReg.Reset ;
          oRenderer.PrepareDraw ;
          oVwr.BeginPaintInternal ;
          Draw( oRenderer, TGIS_DrawMode.All ) ;
        finally
          oVwr.EndPaintInternal ;
          oRenderer.AfterDraw ;

          if (not bKeepContext) then begin
            oBitmap.Canvas.BeginScene ;
            try
              if Assigned( ctx.Selection )then
                oBitmap.Canvas.DrawBitmap(
                  T_FMXBitmap(ctx.Selection),
                  RectF( 0, 0, T_FMXBitmap(ctx.Selection).Width,
                               T_FMXBitmap(ctx.Selection).Height),
                  RectF( 0, 0, oBitmap.Width,
                               oBitmap.Height
                       ),
                  oVwr.SelectionTransparency / 100, True
                ) ;
              if Assigned( ctx.Charts ) then
                oBitmap.Canvas.DrawBitmap(
                  T_FMXBitmap(ctx.Charts),
                  RectF( 0, 0, T_FMXBitmap(ctx.Charts).Width,
                               T_FMXBitmap(ctx.Charts).Height),
                  RectF( 0, 0, oBitmap.Width,
                               oBitmap.Height
                       ),
                  1, False
                ) ;
              if Assigned( ctx.Labels )then
                oBitmap.Canvas.DrawBitmap(
                  T_FMXBitmap(ctx.Labels),
                  RectF( 0, 0, T_FMXBitmap(ctx.Labels).Width,
                               T_FMXBitmap(ctx.Labels).Height),
                  RectF( 0, 0, oBitmap.Width,
                               oBitmap.Height
                       ),
                  1, False
                ) ;
            finally
              oBitmap.Canvas.EndScene ;
            end ;
          end;

          oBitmap.Canvas.BeginScene ;
          oRenderer.RenderEditor( oBitmap.Canvas ) ;
          oRenderer.PaintExtra( Self, oBitmap.Canvas, FOnPaintExtra ) ;
          oBitmap.Canvas.EndScene ;
          oRenderer.ReleaseContext ;

          oBitmap.Canvas.BeginScene ;
            if assigned( FOnAfterPaint ) then
              FOnAfterPaint( Self, oBitmap.Canvas ) ;
            if assigned( FOnAfterPaintRenderer ) then
              oRenderer.PaintExtra( Self, oBitmap.Canvas, FOnAfterPaintRenderer ) ;
          oBitmap.Canvas.EndScene ;
        end ;
      finally
        if not bKeepContext then
          FreeObject( ctx ) ;
        oContext := ctx ;
      end ;
    end ;

  begin

    Clear ;
    if oVwr.IsEmpty then
      exit ;

    metPixelExportDrawLabelSettings := GisMetadataAsBoolean(
                                         METADATA_PIXELEXPORTDRAWLABELSETTINGS,
                                         False
                                       ) ;
    if metPixelExportDrawLabelSettings then begin
      metPaintLabelsOnTop := GisMetadataAsBoolean(
         METADATA_PAINTLABELSONTOP,
         False
      ) ;
      metPaintTopmostLabelsOnTop := GisMetadataAsBoolean(
         METADATA_PAINTTOPMOSTLABELSONTOP,
         False
      ) ;
      if ( oVwr.IsTopmost ) then
        draw_with_topmost
      else
        draw_without_topmost ;
    end
    else begin
      metPaintTopmostLabelsOnTop := False ;
      metPaintLabelsOnTop := False ;
      draw_without_topmost ;
    end ;

  end ;

  procedure TGIS_ViewerBmp.ScrollPgUp ;
  var
    x, y : Integer ;
  begin
    if oVwr.IsEmpty then exit ;

    x := 0 ;
    y := - oBitmap.Height * PAGE_SCROLL div 100 ;
    MoveViewport( x, y ) ;
    markCenter ;
    InvalidateWholeMap ;
  end ;

  procedure TGIS_ViewerBmp.ScrollPgDn ;
  var
    x, y : Integer ;
  begin
    if oVwr.IsEmpty then exit ;

    x := 0 ;
    y := oBitmap.Height * PAGE_SCROLL div 100 ;
    MoveViewport( x, y ) ;
    markCenter ;
    InvalidateWholeMap ;
  end ;

  procedure TGIS_ViewerBmp.ScrollPgRight ;
  var
    x, y : Integer ;
  begin
    if oVwr.IsEmpty then exit ;

    x := oBitmap.Height * PAGE_SCROLL div 100 ;
    y := 0 ;
    MoveViewport( x, y ) ;
    markCenter ;
    InvalidateWholeMap ;
  end ;

  procedure TGIS_ViewerBmp.ScrollPgLeft ;
  var
    x, y : Integer ;
  begin
    if oVwr.IsEmpty then exit ;

    x := - oBitmap.Height * PAGE_SCROLL div 100 ;
    y := 0 ;
    MoveViewport( x, y ) ;
    markCenter ;
    InvalidateWholeMap ;
  end ;

  procedure TGIS_ViewerBmp.ScrollUp ;
  var
    x, y : Integer ;
  begin
    if oVwr.IsEmpty then exit ;

    x := 0 ;
    y := - oBitmap.Height * LINE_SCROLL div 100 ;
    MoveViewport( x, y ) ;
    markCenter ;
    InvalidateWholeMap ;
  end ;

  procedure TGIS_ViewerBmp.ScrollDn ;
  var
    x, y : Integer ;
  begin
    if oVwr.IsEmpty then exit ;

    x := 0 ;
    y := oBitmap.Height * LINE_SCROLL div 100 ;
    MoveViewport( x, y ) ;
    markCenter ;
    InvalidateWholeMap ;
  end ;

  procedure TGIS_ViewerBmp.ScrollRight ;
  var
    x, y : Integer ;
  begin
    if oVwr.IsEmpty then exit ;

    x := oBitmap.Width * LINE_SCROLL div 100 ;
    y := 0 ;
    MoveViewport( x, y ) ;
    markCenter ;
    InvalidateWholeMap ;
  end ;

  procedure TGIS_ViewerBmp.ScrollLeft ;
  var
    x, y : Integer ;
  begin
    if oVwr.IsEmpty then exit ;

    x := - oBitmap.Width * LINE_SCROLL div 100 ;
    y := 0 ;
    MoveViewport( x, y ) ;
    markCenter ;
    InvalidateWholeMap ;
  end ;

//==============================================================================
// Constructors & destructors
//==============================================================================

  constructor TGIS_ViewerBmp.Create ;
  begin
    Create( 320, 240 ) ;
  end ;

  constructor TGIS_ViewerBmp.Create(
    _width, _height : Integer
  ) ;
  begin
    inherited Create( nil ) ;

    oVwr := TGIS_Viewer.Create( Self ) ;
    oVwr.Color := TGIS_Color.White ;
    oVwr.AutoStyle := False ;

    // default renderer
    oRenderer := TGIS_RendererFmx.Create ;

    oBitmap := T_FMXBitmap.Create( _width, _height ) ;
    { TODO -cReview : no TPixelFormat }
//    oBitmap.PixelFormat := TPixelFormat.RGBA32F ;
    oBitmap.Clear( TColorRec.Null );
    localBitmap := True ;

    FTileRect      := Rect ( 0, 0, oBitmap.Width, oBitmap.Height ) ;

    FOnBeforePaint := nil ;
    FOnAfterPaint  := nil ;
    FOnBeforePaintRenderer := nil ;
    FOnAfterPaintRenderer  := nil ;
    FOnPaintExtra  := nil ;
    DelayedUpdate  := 0;
    ProgressiveUpdate := 0;
    UseAnimations := False ;

    oGIS_Bitmap := nil ;
  end ;

  constructor TGIS_ViewerBmp.Create(
    const _bitmap : T_FMXBitmap
  ) ;
  begin
    inherited Create( nil ) ;

    oVwr := TGIS_Viewer.Create( self ) ;
    oVwr.Color := TGIS_Color.White ;

    oRenderer := TGIS_RendererFmx.Create ;
    oBitmap := _bitmap ;

    localBitmap := False ;
    FOnPaintExtra   := nil ;
  end ;

  procedure TGIS_ViewerBmp.SetSize(const _width, _height: Integer);
  begin
    FreeObject( oBitmap ) ;
    oBitmap := T_FMXBitmap.Create( _width, _height ) ;
    oBitmap.Clear( TColorRec.Null );
    localBitmap := True ;
  end ;

  function TGIS_ViewerBmp.SetViewer(const _viewer: TObject): TObject;
  begin
    Result := oVwr ;
    oVwr := _viewer as TGIS_Viewer;
    oVwr.ReParent( self ) ;
  end ;

  function TGIS_ViewerBmp.GetViewer: TObject;
  begin
    Result := oVwr ;
  end ;

  destructor TGIS_ViewerBmp.Destroy ;
  begin
    {$IFDEF GIS_PDK}
      RemoveFreeNotifications ;
    {$ENDIF}

    if localBitmap then
      FreeObject( oBitmap ) ;
    FreeObject( oGIS_Bitmap ) ;
    FreeObject( oRenderer ) ;
    FreeObject( oVwr      ) ;

    inherited ;
  end ;

  procedure TGIS_ViewerBmp.PrintBmp(
    var   _bmp   : {$IFDEF LEVEL_XE5_FMX}
                     FMX.Graphics.TBitmap
                   {$ELSE}
                     FMX.Types.TBitmap
                   {$ENDIF}
  ) ;
  begin
    PrintBmp( _bmp, False ) ;
  end ;

  procedure TGIS_ViewerBmp.PrintBmp(
    var   _bmp   : {$IFDEF LEVEL_XE5_FMX}
                     FMX.Graphics.TBitmap ;
                   {$ELSE}
                     FMX.Types.TBitmap ;
                   {$ENDIF}
    const _full    : Boolean
  ) ;
  var
    bmp     : T_FMXBitmap ;
    w, h    : Integer ;
    wextent : TGIS_Extent ;
    scl     : Double ;

    procedure print_dc(
      _ext : TGIS_Extent ;
      _scl : Double
    ) ;
    var
      ctx  : TGIS_RendererContext ;
      bincrmntl : Boolean ;
      basemap_store : array of Boolean ;

      procedure store_basemap ;
      var
        i : Integer ;
      begin
        SetLength( basemap_store, oVwr.Items.Count ) ;
        for i := 0 to oVwr.Items.Count - 1 do begin
          basemap_store[i] := TGIS_Layer(oVwr.Items[i]).Basemap ;
          if basemap_store[i] then
            TGIS_Layer(oVwr.Items[i]).Basemap := False ;
        end;
      end;

      procedure restore_basemap ;
      var
        i : Integer ;
      begin
        if not assigned( basemap_store ) then exit ;
        for i := 0 to oVwr.Items.Count - 1 do begin
          if basemap_store[i] then
            TGIS_Layer(oVwr.Items[i]).Basemap := basemap_store[i] ;
        end;
      end ;

    begin

      if oVwr.IsEmpty then exit ;

      WaitForBackgroundProcesses ;

      bincrmntl := oVwr.IncrementalPaint ;
      oVwr.IncrementalPaint := False ;

      ctx := TGIS_RendererContext.Create ;
      try
        // Map
        ctx.AssignBaseMap  ( bmp, False ) ;

        // Selection
        ctx.AssignSelection( nil, True  ) ;

        // Charts
        ctx.AssignCharts   ( nil, True  ) ;

        // Labels
        ctx.AssignLabels   ( nil, True  ) ;

        store_basemap ;

        // paint layers
        oRenderer.ReleaseContext ;

        if _full then begin
          bmp.Canvas.BeginScene;
            if assigned( FOnBeforePaintRenderer ) then
              oRenderer.PaintExtra( Self, bmp.Canvas, FOnBeforePaintRenderer ) ;

            if assigned( FOnBeforePaint ) then
              FOnBeforePaint( Self, bmp.Canvas ) ;
          bmp.Canvas.EndScene ;
        end ;

        oRenderer.CreateContext( self, self.oVwr, ctx, Point( 0, 0 ),
                                 w, h, PPI, FontScale
                               ) ;

        oVwr.LabelsReg.Reset ;
        oRenderer.PrepareDraw ;
        oVwr.BeginPaintInternal ;

        try
          Draw( oRenderer, TGIS_DrawMode.All ) ;

        finally
          oVwr.EndPaintInternal ;
          oRenderer.AfterDraw ;

          T_FMXBitmap(ctx.BaseMap).Canvas.BeginScene ;
          try
            if Assigned( ctx.Selection ) then
              T_FMXBitmap(ctx.BaseMap).Canvas.DrawBitmap(
                T_FMXBitmap(ctx.Selection),
                RectF( 0, 0, T_FMXBitmap(ctx.Selection).Width,
                             T_FMXBitmap(ctx.Selection).Height),
                RectF( 0, 0, T_FMXBitmap(ctx.BaseMap).Width,
                             T_FMXBitmap(ctx.Selection).Height
                             / T_FMXBitmap(ctx.Selection).Width
                             * T_FMXBitmap(ctx.BaseMap).Width),
                oVwr.SelectionTransparency / 100, True
              ) ;
            if Assigned( ctx.Charts ) then
              T_FMXBitmap(ctx.BaseMap).Canvas.DrawBitmap(
                T_FMXBitmap(ctx.Charts),
                RectF( 0, 0, T_FMXBitmap(ctx.Charts).Width,
                             T_FMXBitmap(ctx.Charts).Height),
                RectF( 0, 0, T_FMXBitmap(ctx.BaseMap).Width,
                             T_FMXBitmap(ctx.Charts).Height
                             / T_FMXBitmap(ctx.Charts).Width
                             * T_FMXBitmap(ctx.BaseMap).Width),
                       1, False
              ) ;
            if Assigned( ctx.Labels ) then
              T_FMXBitmap(ctx.BaseMap).Canvas.DrawBitmap(
                T_FMXBitmap(ctx.Labels),
                RectF( 0, 0, T_FMXBitmap(ctx.Labels).Width,
                             T_FMXBitmap(ctx.Labels).Height),
                RectF( 0, 0, T_FMXBitmap(ctx.BaseMap).Width,
                             T_FMXBitmap(ctx.Labels).Height
                             / T_FMXBitmap(ctx.Labels).Width
                             * T_FMXBitmap(ctx.BaseMap).Width),
                       1, False
              ) ;
          finally
            T_FMXBitmap(ctx.BaseMap).Canvas.EndScene ;
          end ;
          oRenderer.ReleaseContext ;
        end ;

        if _full then begin
          bmp.Canvas.BeginScene;
            oRenderer.PaintExtra( Self, bmp.Canvas, FOnPaintExtra ) ;

            if assigned( FOnAfterPaint ) then
              FOnAfterPaint( Self, bmp.Canvas ) ;

            if assigned( FOnAfterPaintRenderer ) then
              oRenderer.PaintExtra( Self, bmp.Canvas, FOnAfterPaintRenderer ) ;
          bmp.Canvas.EndScene ;
        end;

      finally
        FreeObject( ctx ) ;
        restore_basemap ;
        oVwr.IncrementalPaint := bincrmntl ;
      end ;

    end ;

  begin
    try
      if assigned( _bmp ) then
        bmp := _bmp
      else
        bmp := T_FMXBitmap.Create( ControlCanvasWidth, ControlCanvasHeight ) ;

      w := bmp.Width ;
      h := bmp.Height ;

      // paint background
      T_FMXBitmap(bmp).Clear( FMXColor(oVwr.Color) ) ;

      wextent := VisibleExtent ;
      try
        scl := 0 ;
        print_dc( VisibleExtent, scl ) ;
      finally
        VisibleExtent := wextent ;
      end ;

      if not assigned( _bmp ) then
        _bmp := bmp ;
    finally
    end;
  end ;

//==================================== END =====================================
end.

