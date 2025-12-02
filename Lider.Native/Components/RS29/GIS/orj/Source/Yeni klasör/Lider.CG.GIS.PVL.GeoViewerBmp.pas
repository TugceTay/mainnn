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
  PVL Viewer Bitmap.
}

{$IFDEF DCC}
  unit PVL.GisViewerBmp;
  {$HPPEMIT '#pragma link "PVL.GisViewerBmp"'}
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
    GisCsSystems;
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
  ///   PVL BMP Viewer intraface
  /// </summary>
  IGIS_PvlViewerBmp = interface( IGIS_PvlObject )
    {$IFDEF DCC}
      ['{FCBC9BCE-9699-4A48-AAD8-B5F0F9BAC582}']
    {$ENDIF}

    procedure fset_Renderer         ( const _value :
                                        TGIS_RendererAbstract ) ;
    function  fget_Renderer         : TGIS_RendererAbstract ;
    procedure fset_DrawBasemapOnly    ( const _value :
                                              Boolean ) ;
    function  fget_DrawBasemapOnly  : Boolean ;
    procedure fset_TilePicture      ( const _value :
                                            TGIS_Bitmap ) ;
    function  fget_TilePicture      : TGIS_Bitmap ;
    function  fget_TileRect         : TRect ;
    procedure fset_TileRect         ( const _val : TRect ) ;
    function  fget_KeepContextInternal
                                    : Boolean ;
    procedure fset_KeepContextInternal
                                    ( const _val : Boolean ) ;
    function  fget_ContextInternal  : TObject ;

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
    function  fget_BusyEvent         : TGIS_BusyEvent ;
    procedure fset_BusyEvent         ( const _value : TGIS_BusyEvent ) ;

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
  end;

  /// <summary>
  ///   PVL GIS BMP Viewer control.
  /// </summary>
  TGIS_PvlViewerBmp = class( TGIS_PvlObject, IGIS_PvlViewerBmp, IGIS_Viewer, IGIS_ViewerBmp  )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
    private
      function fget_PlatformControl
                                  : IGIS_PvlViewerBmp ;          reintroduce;
      property PlatformControl    : IGIS_PvlViewerBmp
                                    read  fget_PlatformControl;  {$IFDEF OXYGENE}reintroduce;{$ENDIF}
      function fget_PlatformControlVwr
                                  : IGIS_Viewer ;
      property PlatformControlVwr : IGIS_Viewer
                                    read  fget_PlatformControlVwr;
      function fget_PlatformControlBmp
                                  : IGIS_ViewerBmp ;
      property PlatformControlBmp : IGIS_ViewerBmp
                                    read  fget_PlatformControlBmp;

    private // properties access functions of IGIS_Viewer
      function  fget_AutoStyle          : Boolean;
      procedure fset_AutoStyle          ( const _value : Boolean ) ;
      function  fget_BigExtent          : TGIS_Extent ;
      function  fget_BigExtentMargin    : Integer ;
      procedure fset_BigExtentMargin    ( const _value : Integer     ) ;
      function  fget_KeepScale          : Boolean ;
      procedure fset_KeepScale          ( const _value  : Boolean     ) ;
      function  fget_BusyLevel          : Integer     ;
      function  fget_BusyText           : String      ;
      function  fget_Center             : TGIS_Point  ;
      procedure fset_Center             ( const _value : TGIS_Point  ) ;
      function  fget_CenterPtg          : TGIS_Point  ;
      procedure fset_CenterPtg          ( const _value : TGIS_Point  ) ;
      function  fget_Color              : TGIS_Color ;
      procedure fset_Color              ( const _value : TGIS_Color  ) ;
      function  fget_Copyright          : String ;
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
      function  fget_TiledPaint         : Boolean     ;                   //TILER
      procedure fset_TiledPaint         ( const _value : Boolean     ) ;  //TILER
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
      function  fget_SelectionGisColor  : TGIS_Color ;
      procedure fset_SelectionGisColor  ( const _value : TGIS_Color  ) ;
      function  fget_SelectionOutlineOnly
                                        : Boolean     ;
      procedure fset_SelectionOutlineOnly(
                                          const _value : Boolean     ) ;
      function  fget_SelectionTransparency
                                        : Integer     ;
      procedure fset_SelectionTransparency(
                                          const _value : Integer     ) ;
      function  fget_SelectionWidth     : Integer ;
      procedure fset_SelectionWidth     ( const _value : Integer     ) ;
      function  fget_SystemPPI          : Integer ;
      function  fget_ViewerParent       : IGIS_ViewerParent ;
      function  fget_ViewerParentRoot   : IGIS_ViewerParent ;
      function  fget_Viewport           : TGIS_Point ;
      procedure fset_Viewport           ( const _value : TGIS_Point  ) ;
      function  fget_VisibleExtent      : TGIS_Extent ;
      procedure fset_VisibleExtent      ( const _value : TGIS_Extent ) ;
      function  fget_UseAnimations      : Boolean ;
      procedure fset_UseAnimations      ( const _value : Boolean     ) ;
      function  fget_UseRTree           : Boolean ;
      procedure fset_UseRTree           ( const _value : Boolean     ) ;
      function  fget_Zoom               : Double ;
      procedure fset_Zoom               ( const _value : Double      ) ;
      function  fget_ZoomEx             : Double ;
      procedure fset_ZoomEx             ( const _value : Double      ) ;
      function  fget_MasterViewer       : IGIS_Viewer ;
      procedure fset_MasterViewer       ( const _value : IGIS_Viewer ) ;
      function  fget_UponDestroy        : Boolean ;
      function  fget_TemporaryScaleInternal
                                        : Double      ;
      procedure fset_TemporaryScaleInternal(
                                          const _value : Double      ) ;
      function  fget_TemporaryVisibleExtent
                                        : TGIS_Extent ;
      procedure fset_TemporaryVisibleExtent(
                                          const _value : TGIS_Extent ) ;

    private // properties access functions of IGIS_ViewerBmp
      function  fget_Width              : Integer ;
      function  fget_Height             : Integer ;

      function  fget_Bitmap             : TObject ;
      function  fget_GIS_Bitmap         : TGIS_Bitmap ;
      function  fget_TileRect           : TRect ;
      procedure fset_TileRect           ( const _val : TRect ) ;

      function  fget_KeepContextInternal: Boolean ;
      procedure fset_KeepContextInternal( const _val   : Boolean        ) ;
      function  fget_ContextInternal    : TObject ;


    private // TGIS_PvlViewerBmp property access routines
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
      procedure fset_ExtentChangeEvent  ( const _value : TGIS_PvlEvent   ) ;
      function  fget_VisibleExtentChangeEvent
                                        : TGIS_PvlEvent ;
      procedure fset_VisibleExtentChangeEvent
                                        ( const _value : TGIS_PvlEvent   ) ;
      function  fget_ZoomChangeEvent    : TGIS_PvlEvent ;
      procedure fset_ZoomChangeEvent    ( const _value : TGIS_PvlEvent   ) ;

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
    public
      /// <summary>
      ///   Standard constructor. Creates bitmap 320x240.
      /// </summary>
      constructor Create         ; reintroduce ; overload;

      /// <summary>
      ///   Create an instance based on provided dimensions.
      /// </summary>
      /// <param name="_width">
      ///   bitmap width in pixels
      /// </param>
      /// <param name="_height">
      ///   bitmap height in pixels
      /// </param>
      constructor Create         ( const _width      : Integer ;
                                   const _height     : Integer
                                 ) ; reintroduce ; overload;
//
//      /// <summary>
//      ///   Create an instance based on provided dimensions.
//      /// </summary>
//      /// <param name="_width">
//      ///   bitmap width in pixels
//      /// </param>
//      /// <param name="_height">
//      ///   bitmap height in pixels
//      /// </param>
//      /// <param name="_renderer">
//      ///   renderer to be used
//      /// </param>
//      constructor Create         ( const _width      : Integer ;
//                                   const _height     : Integer ;
//                                   const _renderer   : TGIS_RendererAbstract
//                                 ) ; reintroduce ; overload;
//
//      /// <summary>
//      ///   Create an instance to be set on provided existing bitmap.
//      /// </summary>
//      /// <param name="_bitmap">
//      ///   bitmap to be used
//      /// </param>
//      constructor Create         ( const _bitmap     : TBitmap
//                                 ) ; reintroduce ; overload;
//
//      /// <summary>
//      ///   Create an instance to be set on provided existing bitmap.
//      /// </summary>
//      /// <param name="_bitmap">
//      ///   bitmap to be used
//      /// </param>
//      constructor Create         ( const _bitmap     : TGIS_Bitmap
//                                 ) ; reintroduce ; overload;
//
//      /// <summary>
//      ///   Create an instance to be set on provided existing bitmap.
//      /// </summary>
//      /// <param name="_bitmap">
//      ///   bitmap to be used
//      /// </param>
//      /// <param name="_renderer">
//      ///   renderer to be used
//      /// </param>
//      constructor Create         ( const _bitmap     : TBitmap ;
//                                   const _renderer   : TGIS_RendererAbstract
//                                 ) ; reintroduce ; overload;
//      /// <summary>
//      ///   Create an instance to be set on provided existing bitmap.
//      /// </summary>
//      /// <param name="_bitmap">
//      ///   bitmap to be used
//      /// </param>
//      /// <param name="_renderer">
//      ///   renderer to be used
//      /// </param>
//      constructor Create         ( const _bitmap     : TGIS_Bitmap ;
//                                   const _renderer   : TGIS_RendererAbstract
//                                 ) ; reintroduce ; overload;
//
//      /// <summary>
//      ///   Standard destructor.
//      /// </summary>
//      destructor Destroy         ; override;

    public // public methods of IGIS_Viewer

      /// <inheritdoc from="IGIS_Viewer"/>
      function ChangeHash        : Int64 ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Subscribe        ( const _control    : IGIS_Subscribe
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure UnSubscribe      ( const _control    : IGIS_Subscribe
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure NotifySubscribers( const _event      : Integer ;
                                   const _context    : TObject
                                 ) ;

      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENXDK}
      /// <inheritdoc from="IGIS_Viewer"/>
      function NotifyPaintException(
                                   const _message    : String ;
                                   const _exception  : Exception
                                 ) : Boolean ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Lock             ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Unlock           ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Unlock           ( const _redraw     : Boolean
                                 ) ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure Interrupt        ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  Interrupted      : Boolean ;  //TILER

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
      procedure BusyPrepare      (       _sender     : TObject ;
                                         _text       : String
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure BusyRelease      (       _sender     : TObject
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure BusyShake        (       _sender     : TObject ;
                                         _pos        : Int64 ;
                                         _end        : Int64 ;
                                   var   _abort      : Boolean
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

      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      /// <inheritdoc from="IGIS_Viewer"/>
      function  AssignedBusyEvent: TGIS_BusyEvent;

      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      /// <inheritdoc from="IGIS_Viewer"/>
      function  AssignedHelpEvent: TGIS_HelpEvent;

      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_Viewer"/>
      function  StorePaintState  : TObject ;

      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_Viewer"/>
      procedure RestorePaintState( var _state        : TObject
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
      procedure InvalidateExtent ( const _extent     : TGIS_Extent
                                 ) ; overload;
                                 {$IFNDEF GENDOC} deprecated ; {$ENDIF}

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateExtent ( const _extent     : TGIS_Extent ;
                                   const _deep       : Boolean
                                 ) ; overload;
                                 {$IFNDEF GENDOC} deprecated ; {$ENDIF}

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateWholeMap  ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateTopmost   ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateBasemap   ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateSelection ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure InvalidateEditor ( const _final      : Boolean
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  FullExtentZoom   : Double ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure FullExtent       ;

      /// <inheritdoc from="IGIS_Viewer"/>
     function  Locate            ( const _ptg        : TGIS_Point ;
                                   const _prec       : Double
                                 ) : TGIS_ShapeAbstract ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  Locate           ( const _ptg        : TGIS_Point ;
                                   const _prec       : Double     ;
                                   const _visible    : Boolean
                                 ) : TGIS_ShapeAbstract ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  Locate           ( const _pt         : TPoint     ;
                                   const _prec       : Integer
                                 ) : TGIS_ShapeAbstract ; overload;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  LocateEx            ( const _ptg     : TGIS_Point ;
                                      const _prec    : Double    ;
                                      const _visible : Boolean
                                    ) : TGIS_ShapeAbstractList ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  MapToScreen      ( const _ptg        : TGIS_Point
                                 ) : TPoint ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  MapToScreen3D    ( const _ptg        : TGIS_Point3D
                                 ) : TPoint ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ScreenToMap      ( const _pt         : TPoint
                                 ) : TGIS_Point ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ScreenToMap3D    ( const _pt         : TPoint
                                 ) : TGIS_Point3D ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  MapToScreenEx    ( const _pt         : TGIS_Point
                                 ) : TGIS_Point ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ScreenToMapEx    ( const _pt         : TGIS_Point
                                 ) : TGIS_Point ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  MapToScreenRect  ( const _rct : TGIS_Extent
                                 ) : TRect ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  ScreenToMapRect  ( const _rct       : TRect
                                 ) : TGIS_Extent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  PixelsToTwips    ( const _size      : Integer
                                 ) : Integer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  TwipsToPixels    ( const _size      : Integer
                                 ) : Integer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  TwipsToPoints    ( const _size : Integer
                                 ) : Integer ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure MoveViewport     ( var   _dx, _dy  : Integer
                                 ) ;
{$IFDEF JAVA}
      /// <inheritdoc from="IGIS_Viewer"/>
      procedure MoveViewportEx   ( var   _dx, _dy  : java.lang.Double
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SetViewport      ( var   _x ,  _y  : java.lang.Double
                                 ) ;
{$ELSE}
      /// <inheritdoc from="IGIS_Viewer"/>
      procedure MoveViewportEx   ( var   _dx, _dy  : Double
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SetViewport      ( var   _x ,  _y  : Double
                                 ) ;
{$ENDIF}
      /// <inheritdoc from="IGIS_Viewer"/>
      procedure CenterViewport   ( const _ptg      : TGIS_Point
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SetCSByWKT       ( const  _wkt     : String
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SetCSByEPSG      ( const  _epsg    : Integer
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure SetCSByWKTFile   ( const  _path    : String
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  RotatedPoint     ( const _ptg      : TGIS_Point
                                 ) : TGIS_Point;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  UnrotatedPoint   ( const _ptg      : TGIS_Point
                                 ) : TGIS_Point ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  RotatedPoint3D   ( const _ptg      : TGIS_Point3D
                                 ) : TGIS_Point3D ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure RotatedPoint3D_ref(
                                   {$IFNDEF JAVA}var{$ENDIF}   _ptg      : TGIS_Point3D
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  UnrotatedPoint3D ( const _ptg      : TGIS_Point3D
                                 ) : TGIS_Point3D ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure UnrotatedPoint3D_ref(
                                   {$IFNDEF JAVA}var{$ENDIF}  _ptg       : TGIS_Point3D
                                 ) ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  RotatedExtent    ( const _extent   : TGIS_Extent
                                 ) : TGIS_Extent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  UnrotatedExtent  ( const _extent   : TGIS_Extent
                                 ) : TGIS_Extent ;

      /// <inheritdoc from="IGIS_Viewer"/>
      function  GetRenderContext : TObject ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure WaitForBackgroundProcesses ;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure WaitForNotBusy   (        _sender  : TObject ;
                                    const _proc    : TGIS_WaitForNotBusyProc
                                 ) ;

    public // public methods of IGIS_ViewerBmp

      /// <inheritdoc from="IGIS_ViewerBmp"/>
      procedure ZoomIn        ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ZoomOut       ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure Clear         ;

      /// <inheritdoc from="IGIS_PvlViewerBmp"/>
      procedure Draw          ; overload;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ScrollPgUp    ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ScrollPgDn    ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ScrollPgRight ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ScrollPgLeft  ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ScrollUp      ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ScrollDn      ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ScrollRight   ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ScrollLeft    ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure SetSize( const _width  : Integer ;
                         const _height : Integer
                        ) ;

    public
      /// <inheritdoc from="IGIS_PvlViewerBmp"/>
      procedure PrintBmp         ( var   _bmp    : TGIS_Bitmap
                                 ) ; overload ; virtual;

      /// <inheritdoc from="IGIS_PvlViewerBmp"/>
      procedure PrintBmp         ( var   _bmp    : TGIS_Bitmap ;
                                   const _full   : Boolean
                                 ) ; overload ; virtual;

    private // properties of IGIS_Viewer forced to be hidden

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   Always False on TGIS_PvlViewerBmp.
      /// </remarks>
      property KeepScale    : Boolean      read  fget_KeepScale
                                           write fset_KeepScale ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   Always 0 on TGIS_PvlViewerBmp.
      /// </remarks>
      property DelayedUpdate : Integer     read  fget_DelayedUpdate
                                           write fset_DelayedUpdate ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   Always 0 on TGIS_PvlViewerBmp.
      /// </remarks>
      property ProgressiveUpdate : Integer read  fget_ProgressiveUpdate
                                           write fset_ProgressiveUpdate ;

      /// <inheritdoc from="IGIS_Viewer"/>
      /// <remarks>
      ///   Always False on TGIS_PvlViewerBmp.
      /// </remarks>
      property UseAnimations : Boolean     read  fget_UseAnimations
                                           write fset_UseAnimations ;

    public // properties of IGIS_Viewer

      /// <inheritdoc from="IGIS_Viewer"/>
      property Copyright : String          read  fget_Copyright ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property CustomData : TGIS_StringList
                                           read  fget_CustomData ;

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
      /// <remarks>
      ///   This property has no meaning on TGIS_PvlViewerBmp.
      /// </remarks>
      property TiledPaint : Boolean        read  fget_TiledPaint    //TILER
                                           write fset_TiledPaint ;

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
      property TemporaryScaleInternal : Double read  fget_TemporaryScaleInternal
                                               write fset_TemporaryScaleInternal ;

      /// <inheritdoc from="IGIS_Viewer"/>
      property TemporaryVisibleExtent : TGIS_Extent
                                           read  fget_TemporaryVisibleExtent
                                           write fset_TemporaryVisibleExtent ;

    public // properties of IGIS_ViewerBmp
    
      /// <inheritdoc from="IGIS_ViewerBmp"/>
      property Height : Integer            read fget_Height ;

      /// <inheritdoc from="IGIS_ViewerBmp"/>
      property Width  : Integer            read fget_Width ;

      /// <inheritdoc from="IGIS_ViewerBmp"/>
      property Bitmap : TObject            read fget_Bitmap ;

      /// <inheritdoc from="IGIS_ViewerBmp"/>
      property GIS_Bitmap : TGIS_Bitmap    read fget_GIS_Bitmap ;

      /// <inheritdoc from="IGIS_ViewerBmp"/>
      property TileRect : TRect            read  fget_TileRect
                                           write fset_TileRect ;


    public // public properties new for TGIS_PvlViewerBmp

      /// <summary>
      ///   If yes only base map will be drawn (without selection, labels and charts).
      /// </summary>
      property DrawBasemapOnly : Boolean   read  fget_DrawBasemapOnly
                                           write fset_DrawBasemapOnly ;

      /// <inheritdoc from="IGIS_Viewer"/>
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
      property Renderer : TGIS_RendererAbstract
                                           read  fget_Renderer
                                           write fset_Renderer ;
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
      property ExtentChangeEvent : TGIS_PvlEvent read fget_ExtentChangeEvent
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
  end;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisRtl
    // ensure that proper implementation files are referenced in Delphi
    {$IFDEF USE_FMX}
      ,FMX.GisPvlViewerBmp
    {$ENDIF}
    {$IFDEF USE_VCL}
      ,VCL.GisPvlViewerBmp
    {$ENDIF}
    ;
{$ENDIF}

{$REGION 'properties access functions of IGIS_Viewer'}

  function TGIS_PvlViewerBmp.fget_AutoStyle
    : Boolean ;
  begin
    Result := PlatformControlVwr.fget_AutoStyle
  end ;

  procedure TGIS_PvlViewerBmp.fset_AutoStyle(
    const _value : Boolean
  ) ;
  begin
    PlatformControlVwr.fset_AutoStyle( _value );
  end ;

  function TGIS_PvlViewerBmp.fget_BigExtent
    : TGIS_Extent ;
  begin
    Result := PlatformControlVwr.fget_BigExtent ;
  end ;

  function TGIS_PvlViewerBmp.fget_BigExtentMargin
    : Integer ;
  begin
    Result := PlatformControlVwr.fget_BigExtentMargin ;
  end ;

  function TGIS_PvlViewerBmp.fget_Width
    : Integer ;
  begin
    Result := PlatformControlBmp.fget_Width ;
  end;

  function TGIS_PvlViewerBmp.fget_Height
    : Integer ;
  begin
    Result := PlatformControlBmp.fget_Height ;
  end;

  function TGIS_PvlViewerBmp.fget_Bitmap
    : TObject ;
  begin
    Result := PlatformControlBmp.fget_Bitmap
  end ;

  function TGIS_PvlViewerBmp.fget_GIS_Bitmap
    : TGIS_Bitmap ;
  begin
    Result := PlatformControlBmp.fget_GIS_Bitmap ;
  end ;

  function  TGIS_PvlViewerBmp.fget_TileRect
    : TRect ;
  begin
    Result := PlatformControlBmp.fget_TileRect ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_TileRect(
    const _val : TRect
  ) ;
  begin
    PlatformControlBmp.fset_TileRect( _val )
  end ;

  //TILER
  function  TGIS_PvlViewerBmp.fget_KeepContextInternal
    : Boolean ;
  begin
    Result := PlatformControlBmp.fget_KeepContextInternal ;
  end;

  //TILER
  procedure TGIS_PvlViewerBmp.fset_KeepContextInternal(
    const _val   : Boolean
  ) ;
  begin
    PlatformControlBmp.fset_KeepContextInternal( _val ) ;
  end;

  //TILER
  function TGIS_PvlViewerBmp.fget_ContextInternal
    : TObject ;
  begin
    Result := PlatformControlBmp.fget_ContextInternal ;
  end;

  procedure TGIS_PvlViewerBmp.fset_BigExtentMargin(
    const _value : Integer
  ) ;
  begin
    PlatformControlVwr.fset_BigExtentMargin( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_BusyLevel
    : Integer ;
  begin
    Result := PlatformControlVwr.fget_BusyLevel ;
  end ;

  function TGIS_PvlViewerBmp.fget_BusyText
    : String ;
  begin
    Result := PlatformControlVwr.fget_BusyText ;
  end ;

  function TGIS_PvlViewerBmp.fget_Center
    : TGIS_Point  ;
  begin
    Result := PlatformControlVwr.fget_Center ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_Center(
    const _value : TGIS_Point
  ) ;
  begin
    PlatformControlVwr.fset_Center( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_CenterPtg
    : TGIS_Point  ;
  begin
    Result := PlatformControlVwr.fget_CenterPtg ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_CenterPtg(
    const _value : TGIS_Point
  ) ;
  begin
    PlatformControlVwr.fset_CenterPtg( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_Color
    : TGIS_Color ;
  begin
    Result := PlatformControlVwr.fget_Color ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_Color(
    const _value : TGIS_Color
  ) ;
  begin
    PlatformControlVwr.fset_Color( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_Copyright
    : String ;
  begin
    Result := PlatformControlVwr.fget_Copyright ;
  end ;

  function TGIS_PvlViewerBmp.fget_CS
    : TGIS_CSCoordinateSystem ;
  begin
    Result := PlatformControlVwr.fget_CS ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_CS(
    const _value : TGIS_CSCoordinateSystem
  ) ;
  begin
    PlatformControlVwr.fset_CS( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_CustomPPI
    : Integer ;
  begin
    Result := PlatformControlVwr.fget_CustomPPI ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_CustomPPI(
    const _value : Integer
  ) ;
  begin
    PlatformControlVwr.fset_CustomPPI( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_Editor
    : IGIS_Editor ;
  begin
    Result := PlatformControlVwr.fget_Editor ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_Editor(
    const _value : IGIS_Editor
  ) ;
  begin
    PlatformControlVwr.fset_Editor( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_Extent
    : TGIS_Extent ;
  begin
    Result := PlatformControlVwr.fget_Extent ;
  end ;

  function TGIS_PvlViewerBmp.fget_FileCopyrights
    : String      ;
  begin
    Result := PlatformControlVwr.fget_FileCopyrights ;
  end ;

  function TGIS_PvlViewerBmp.fget_FontScale
    : Integer ;
  begin
    Result := PlatformControlVwr.fget_FontScale ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_FontScale(
    const _value : Integer
  ) ;
  begin
    PlatformControlVwr.fset_FontScale( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_FullDrawExtent
    : TGIS_Extent ;
  begin
    Result := PlatformControlVwr.fget_FullDrawExtent ;
  end ;

  function TGIS_PvlViewerBmp.fget_Hierarchy: IGIS_HierarchyManager;
  begin
    Result := PlatformControlVwr.fget_Hierarchy
  end ;

  function TGIS_PvlViewerBmp.fget_IncrementalPaint
    : Boolean ;
  begin
    Result := PlatformControlVwr.fget_IncrementalPaint ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_IncrementalPaint(
    const _value : Boolean
  ) ;
  begin
    PlatformControlVwr.fset_IncrementalPaint( _value ) ;
  end ;

  //TILER
  function TGIS_PvlViewerBmp.fget_TiledPaint
    : Boolean ;
  begin
    Result := PlatformControlVwr.fget_TiledPaint ;
  end ;

  //TILER
  procedure TGIS_PvlViewerBmp.fset_TiledPaint(
    const _value : Boolean
  ) ;
  begin
    PlatformControlVwr.fset_TiledPaint( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_InPaint
    : Boolean ;
  begin
    Result := PlatformControlVwr.fget_InPaint ;
  end ;

  function TGIS_PvlViewerBmp.fget_IsBusy
    : Boolean ;
  begin
    Result := PlatformControlVwr.fget_IsBusy ;
  end ;

  function TGIS_PvlViewerBmp.fget_IsEmpty
    : Boolean ;
  begin
    Result := PlatformControlVwr.fget_IsEmpty ;
  end ;

  function TGIS_PvlViewerBmp.fget_IsLocked
   : Boolean ;
  begin
    Result := PlatformControlVwr.fget_IsLocked ;
  end ;

  function TGIS_PvlViewerBmp.fget_IsTopmost
   : Boolean ;
  begin
    Result := PlatformControlVwr.fget_IsTopmost ;
  end ;

  function TGIS_PvlViewerBmp.fget_Items
    : TGIS_LayerAbstractList ;
  begin
    Result := PlatformControlVwr.fget_Items ;
  end ;

  function TGIS_PvlViewerBmp.fget_KeepScale
    : Boolean ;
  begin
    Result := PlatformControlVwr.fget_KeepScale ;
  end ;

  function TGIS_PvlViewerBmp.fget_LabelsReg
    : TGIS_LabelsAreaAbstract ;
  begin
    Result := PlatformControlVwr.fget_LabelsReg ;
  end ;

  function TGIS_PvlViewerBmp.fget_MultiUserMode
    : TGIS_MultiUser ;
  begin
    Result := PlatformControlVwr.fget_MultiUserMode ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_MultiUserMode(
    const _value : TGIS_MultiUser
  ) ;
  begin
    PlatformControlVwr.fset_MultiUserMode( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_CustomData
    : TGIS_StringList ;
  begin
    Result := PlatformControlVwr.fget_CustomData ;
  end ;

  function TGIS_PvlViewerBmp.fget_OverlappedExtentMargin
    : Integer ;
  begin
    Result := PlatformControlVwr.fget_OverlappedExtentMargin ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_OverlappedExtentMargin(
    const _value : Integer
  ) ;
  begin
    PlatformControlVwr.fset_OverlappedExtentMargin( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_PPI
    : Integer ;
  begin
    Result := PlatformControlVwr.fget_PPI ;
  end ;

  function TGIS_PvlViewerBmp.fget_ProjectFile
    : TGIS_ConfigAbstract ;
  begin
    Result := PlatformControlVwr.fget_ProjectFile
  end ;

  procedure TGIS_PvlViewerBmp.fset_ProjectFile(
    const _value : TGIS_ConfigAbstract
  ) ;
  begin
    PlatformControlVwr.fset_ProjectFile( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_ProjectName
    : String ;
  begin
    Result := PlatformControlVwr.fget_ProjectName
  end ;

  function TGIS_PvlViewerBmp.fget_RestrictedDrag
    : Boolean ;
  begin
    Result := PlatformControlVwr.fget_RestrictedDrag ;
  end ;

  function TGIS_PvlViewerBmp.fget_DelayedUpdate
    : Integer ;
  begin
    Result := PlatformControlVwr.fget_DelayedUpdate ;
  end;

  procedure TGIS_PvlViewerBmp.fset_DelayedUpdate(
    const _value : Integer
  ) ;
  begin
    PlatformControlVwr.fset_DelayedUpdate( _value ) ;
  end;

  procedure TGIS_PvlViewerBmp.fset_KeepScale(
    const _value : Boolean
  ) ;
  begin
    PlatformControlVwr.fset_KeepScale( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_ProgressiveUpdate
    : Integer ;
  begin
    Result := PlatformControlVwr.fget_ProgressiveUpdate ;
  end;

  procedure TGIS_PvlViewerBmp.fset_ProgressiveUpdate(
    const _value : Integer
  ) ;
  begin
    PlatformControlVwr.fset_ProgressiveUpdate( _value ) ;
  end;

  procedure TGIS_PvlViewerBmp.fset_RestrictedDrag(
    const _value : Boolean
  ) ;
  begin
    PlatformControlVwr.fset_RestrictedDrag( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_RestrictedExtent
    : TGIS_Extent ;
  begin
    Result := PlatformControlVwr.fget_RestrictedExtent ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_RestrictedExtent(
    const _value : TGIS_Extent
  ) ;
  begin
    PlatformControlVwr.fset_RestrictedExtent( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_RotationAngle
    : Double ;
  begin
    Result := PlatformControlVwr.fget_RotationAngle ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_RotationAngle(
    const _value : Double
  ) ;
  begin
    PlatformControlVwr.fset_RotationAngle( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_RotationPoint
    : TGIS_Point ;
  begin
    Result := PlatformControlVwr.fget_RotationPoint ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_RotationPoint(
    const _value : TGIS_Point
  ) ;
  begin
    PlatformControlVwr.fset_RotationPoint( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_ScaleAsFloat
    : Double ;
  begin
    Result := PlatformControlVwr.fget_ScaleAsFloat ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_ScaleAsFloat(
    const _value : Double
  ) ;
  begin
    PlatformControlVwr.fset_ScaleAsFloat( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_ScaleAsText
    : String ;
  begin
    Result := PlatformControlVwr.fget_ScaleAsText ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_ScaleAsText(
    const _value : String
  ) ;
  begin
    PlatformControlVwr.fset_ScaleAsText( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_SelectionGisColor
    : TGIS_Color ;
  begin
    Result := PlatformControlVwr.fget_SelectionGisColor ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_Level(
    const _value : Double
  ) ;
  begin
    PlatformControlVwr.fset_Level( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_Level
    : Double ;
  begin
    Result := PlatformControlVwr.fget_Level ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_SelectionGisColor(
    const _value : TGIS_Color
  ) ;
  begin
    PlatformControlVwr.fset_SelectionGisColor( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_SelectionOutlineOnly
    : Boolean ;
  begin
    Result := PlatformControlVwr.fget_SelectionOutlineOnly ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_SelectionOutlineOnly(
    const _value : Boolean
  ) ;
  begin
    PlatformControlVwr.fset_SelectionOutlineOnly( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_SelectionTransparency
    : Integer ;
  begin
    Result := PlatformControlVwr.fget_SelectionTransparency ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_SelectionTransparency(
    const _value : Integer
  ) ;
  begin
    PlatformControlVwr.fset_SelectionTransparency( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_SelectionWidth
    : Integer ;
  begin
    Result := PlatformControlVwr.fget_SelectionWidth ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_SelectionWidth(
    const _value : Integer
  ) ;
  begin
    PlatformControlVwr.fset_SelectionWidth( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_SystemPPI
    : Integer ;
  begin
    Result := PlatformControlVwr.fget_SystemPPI ;
  end ;

  function TGIS_PvlViewerBmp.fget_ViewerParent
    : IGIS_ViewerParent ;
  begin
    Result := PlatformControlVwr.fget_ViewerParent ;
  end ;

  function TGIS_PvlViewerBmp.fget_ViewerParentRoot
    : IGIS_ViewerParent ;
  begin
    Result := PlatformControlVwr.fget_ViewerParentRoot ;
  end ;

  function TGIS_PvlViewerBmp.fget_Viewport
    : TGIS_Point ;
  begin
    Result := PlatformControlVwr.fget_Viewport ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_Viewport(
    const _value : TGIS_Point
  ) ;
  begin
    PlatformControlVwr.fset_Viewport( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_VisibleExtent
    : TGIS_Extent ;
  begin
    Result := PlatformControlVwr.fget_VisibleExtent ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_VisibleExtent(
    const _value : TGIS_Extent
  ) ;
  begin
    PlatformControlVwr.fset_VisibleExtent( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_UseAnimations
    : Boolean ;
  begin
    Result := PlatformControlVwr.fget_UseAnimations ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_UseAnimations(
    const _value : Boolean
  ) ;
  begin
    PlatformControlVwr.fset_UseAnimations( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_UseRTree
    : Boolean ;
  begin
    Result := PlatformControlVwr.fget_UseRTree ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_UseRTree(
    const _value : Boolean
  ) ;
  begin
    PlatformControlVwr.fset_UseRTree( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_Zoom
    : Double ;
  begin
    Result := PlatformControlVwr.fget_Zoom ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_Zoom(
    const _value : Double
  ) ;
  begin
    PlatformControlVwr.fset_Zoom( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_ZoomEx
    : Double ;
  begin
    Result := PlatformControlVwr.fget_ZoomEx ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_ZoomEx(
    const _value : Double
  ) ;
  begin
    PlatformControlVwr.fset_ZoomEx( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_MasterViewer
    : IGIS_Viewer ;
  begin
    Result := PlatformControlVwr.fget_MasterViewer ;
  end;

  procedure TGIS_PvlViewerBmp.fset_MasterViewer(
    const _value : IGIS_Viewer
  ) ;
  begin
    PlatformControlVwr.fset_MasterViewer( _value ) ;
  end;

  function TGIS_PvlViewerBmp.fget_UponDestroy
    : Boolean ;
  begin
    Result := PlatformControlVwr.fget_UponDestroy ;
  end ;

  function  TGIS_PvlViewerBmp.fget_TemporaryScaleInternal : Double ;
  begin
    Result := PlatformControlVwr.fget_TemporaryScaleInternal ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_TemporaryScaleInternal(
    const _value : Double
  ) ;
  begin
    PlatformControlVwr.fset_TemporaryScaleInternal( _value ) ;
  end ;

  function  TGIS_PvlViewerBmp.fget_TemporaryVisibleExtent : TGIS_Extent ;
  begin
    Result := PlatformControlVwr.fget_TemporaryVisibleExtent ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_TemporaryVisibleExtent(
    const _value : TGIS_Extent
  ) ;
  begin
    PlatformControlVwr.fset_TemporaryVisibleExtent( _value ) ;
  end ;

{$ENDREGION 'properties access functions of IGIS_Viewer'}

{$REGION 'new properties access routines of TGIS_PvlViewerBmp'}

  procedure TGIS_PvlViewerBmp.fset_Renderer(
    const _value : TGIS_RendererAbstract
  ) ;
  begin
    PlatformControl.fset_Renderer( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_Renderer
    : TGIS_RendererAbstract ;
  begin
    Result := PlatformControl.fget_Renderer ;
  end;

  procedure TGIS_PvlViewerBmp.fset_DrawBasemapOnly(
    const _value: Boolean
  ) ;
  begin
    PlatformControl.fset_DrawBasemapOnly( _value ) ;
  end;

  function TGIS_PvlViewerBmp.fget_DrawBasemapOnly
    : Boolean;
  begin
    Result := PlatformControl.fget_DrawBasemapOnly ;
  end;

  procedure TGIS_PvlViewerBmp.fset_TilePicture(
    const _value: TGIS_Bitmap
  );
  begin
    PlatformControl.fset_TilePicture( _value ) ;
  end;

  function TGIS_PvlViewerBmp.fget_TilePicture
    : TGIS_Bitmap ;
  begin
    Result := PlatformControl.fget_TilePicture ;
  end;

{$ENDREGION 'new properties access routines of TGIS_PvlViewerBmp'}

{$REGION 'property access routines of TGIS_Viewer's event handlers'}

  function TGIS_PvlViewerBmp.fget_BusyEvent
    : TGIS_BusyEvent ;
  begin
    Result := PlatformControl.fget_BusyEvent ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_BusyEvent(
    const _value : TGIS_BusyEvent
  ) ;
  begin
    PlatformControl.fset_BusyEvent( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_ExtentChangeEvent
    : TGIS_PvlEvent ;
  begin
    Result := PlatformControl.fget_ExtentChangeEvent ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_ExtentChangeEvent(
    const _value : TGIS_PvlEvent
  ) ;
  begin
    PlatformControl.fset_ExtentChangeEvent( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_VisibleExtentChangeEvent
    : TGIS_PvlEvent ;
  begin
    Result := PlatformControl.fget_VisibleExtentChangeEvent ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_VisibleExtentChangeEvent(
    const _value : TGIS_PvlEvent
  ) ;
  begin
    PlatformControl.fset_VisibleExtentChangeEvent( _value ) ;
  end ;

  function TGIS_PvlViewerBmp.fget_ZoomChangeEvent
    : TGIS_PvlEvent ;
  begin
    Result := PlatformControl.fget_ZoomChangeEvent ;
  end ;

  procedure TGIS_PvlViewerBmp.fset_ZoomChangeEvent(
    const _value : TGIS_PvlEvent
  ) ;
  begin
    PlatformControl.fset_ZoomChangeEvent( _value ) ;
  end ;

{$ENDREGION 'property access routines of TGIS_Viewer's event handlers'}

{$REGION 'IGIS_ViewerParent events access routines'}

function TGIS_PvlViewerBmp.fget_BeforePaintEvent
  : TGIS_PaintEvent ;
begin
  Result := PlatformControl.fget_BeforePaintEvent ;
end;

procedure TGIS_PvlViewerBmp.fset_BeforePaintEvent(
  const _value : TGIS_PaintEvent
);
begin
  PlatformControl.fset_BeforePaintEvent( _value ) ;
end;

function TGIS_PvlViewerBmp.fget_AfterPaintEvent
  : TGIS_PaintEvent ;
begin
  Result := PlatformControl.fget_AfterPaintEvent ;
end;

procedure TGIS_PvlViewerBmp.fset_AfterPaintEvent(
  const _value : TGIS_PaintEvent
);
begin
  PlatformControl.fset_AfterPaintEvent( _value ) ;
end;

function TGIS_PvlViewerBmp.fget_BeforePaintRendererEvent
  : TGIS_RendererEvent ;
begin
  Result := PlatformControl.fget_BeforePaintRendererEvent ;
end;

procedure TGIS_PvlViewerBmp.fset_BeforePaintRendererEvent(
  const _value : TGIS_RendererEvent
);
begin
  PlatformControl.fset_BeforePaintRendererEvent( _value ) ;
end;

function TGIS_PvlViewerBmp.fget_AfterPaintRendererEvent
  : TGIS_RendererEvent ;
begin
  Result := PlatformControl.fget_AfterPaintRendererEvent ;
end;

procedure TGIS_PvlViewerBmp.fset_AfterPaintRendererEvent(
  const _value : TGIS_RendererEvent
);
begin
  PlatformControl.fset_AfterPaintRendererEvent( _value ) ;
end;

function TGIS_PvlViewerBmp.fget_PaintExtraEvent
  : TGIS_RendererEvent ;
begin
  Result := PlatformControl.fget_PaintExtraEvent ;
end;

procedure TGIS_PvlViewerBmp.fset_PaintExtraEvent(
  const _value : TGIS_RendererEvent
);
begin
  PlatformControl.fset_PaintExtraEvent( _value ) ;
end;

function TGIS_PvlViewerBmp.fget_BeforeUpdateEvent
  : TGIS_RendererEvent ;
begin
  Result := PlatformControl.fget_BeforeUpdateEvent ;
end;

procedure TGIS_PvlViewerBmp.fset_BeforeUpdateEvent(
  const _value : TGIS_RendererEvent
);
begin
  PlatformControl.fset_BeforeUpdateEvent( _value ) ;
end;

function TGIS_PvlViewerBmp.fget_AfterUpdateEvent
  : TGIS_RendererEvent ;
begin
  Result := PlatformControl.fget_AfterUpdateEvent ;
end;

procedure TGIS_PvlViewerBmp.fset_AfterUpdateEvent(
  const _value : TGIS_RendererEvent
);
begin
  PlatformControl.fset_AfterUpdateEvent( _value ) ;
end;

{$ENDREGION 'IGIS_ViewerWnd events access routines'}

{$REGION 'constructors'}
  constructor TGIS_PvlViewerBmp.Create ;
  begin
    inherited create( TGIS_PvlContext.create( nil ) ) ;
  end;

  constructor TGIS_PvlViewerBmp.Create(
    const _width      : Integer ;
    const _height     : Integer
  ) ;
  begin
      inherited Create( TGIS_PvlContext.Create(nil) ) ;
      SetSize( _width, _height );
  end;


{$ENDREGION 'constructors'}

{$REGION 'public methods of IGIS_Viewer'}

  function TGIS_PvlViewerBmp.ChangeHash
    : Int64 ;
  begin
    Result := PlatformControlVwr.ChangeHash ;
  end ;

  procedure TGIS_PvlViewerBmp.Subscribe(
    const _control : IGIS_Subscribe
  ) ;
  begin
    PlatformControlVwr.Subscribe( _control ) ;
  end ;

  procedure TGIS_PvlViewerBmp.UnSubscribe(
    const _control : IGIS_Subscribe
  ) ;
  begin
    PlatformControlVwr.UnSubscribe( _control ) ;
  end ;

  procedure TGIS_PvlViewerBmp.NotifySubscribers(
    const _event   : Integer ;
    const _context : TObject
  ) ;
  begin
    PlatformControlVwr.NotifySubscribers( _event, _context ) ;
  end ;

  function TGIS_PvlViewerBmp.NotifyPaintException(
    const _message    : String   ;
    const _exception  : Exception
  ) : Boolean ;
  begin
    Result := PlatformControlVwr.NotifyPaintException( _message, _exception ) ;
  end ;

  procedure TGIS_PvlViewerBmp.Lock ;
  begin
    PlatformControlVwr.Lock ;
  end ;

  procedure TGIS_PvlViewerBmp.Unlock ;
  begin
    PlatformControlVwr.Unlock ;
  end ;

  procedure TGIS_PvlViewerBmp.Unlock(
    const _redraw : Boolean
  ) ;
  begin
    PlatformControlVwr.Unlock( _redraw ) ;
  end ;

  procedure TGIS_PvlViewerBmp.Interrupt ;
  begin
    PlatformControlVwr.Interrupt ;
  end ;

  //TILER
  function TGIS_PvlViewerBmp.Interrupted
    : Boolean ;
  begin
    Result := PlatformControlVwr.Interrupted ;
  end ;

  function  TGIS_PvlViewerBmp.HourglassActive
    : Boolean ;
  begin
    Result := PlatformControlVwr.HourglassActive ;
  end ;

  procedure TGIS_PvlViewerBmp.HourglassPrepare ;
  begin
    PlatformControlVwr.HourglassPrepare ;
  end ;

  procedure TGIS_PvlViewerBmp.HourglassRelease ;
  begin
    PlatformControlVwr.HourglassRelease ;
  end ;

  function  TGIS_PvlViewerBmp.HourglassShake
    : Boolean ;
  begin
    Result := PlatformControlVwr.HourglassShake ;
  end ;

  procedure TGIS_PvlViewerBmp.HourglassRestart ;
  begin
    PlatformControlVwr.HourglassRestart ;
  end ;

  procedure TGIS_PvlViewerBmp.BusyPrepare(
    _sender : TObject ;
    _text   : String
  ) ;
  begin
    PlatformControlVwr.BusyPrepare( _sender, _text ) ;
  end ;

  procedure TGIS_PvlViewerBmp.BusyRelease(
    _sender : TObject
  ) ;
  begin
    PlatformControlVwr.BusyRelease( _sender ) ;
  end ;

  procedure TGIS_PvlViewerBmp.BusyShake(
         _sender : TObject ;
         _pos    : Int64 ;
         _end    : Int64 ;
    var _abort  : Boolean
  ) ;
  begin
    PlatformControlVwr.BusyShake( _sender, _pos, _end, _abort ) ;
  end ;

{$IFDEF OXYGENE}
  procedure TGIS_PvlViewerBmp.RaiseBusyEvent(
        _sender : TObject;
        _e      : TGIS_BusyEventArgs
  );
  begin
    PlatformControlVwr.RaiseBusyEvent( _sender, _e );
  end;

  procedure TGIS_PvlViewerBmp.RaiseHelpEvent(
    _sender : TObject;
    _e      : TGIS_HelpEventArgs
  );
  begin
    PlatformControlVwr.RaiseHelpEvent( _sender, _e );
  end;
{$ELSE}
  procedure TGIS_PvlViewerBmp.RaiseBusyEvent(
        _sender : TObject;
        _pos,
        _end    : Int64;
    var _abort  : Boolean);
  begin
    PlatformControlVwr.RaiseBusyEvent( _sender, _pos, _end, _abort );
  end;

  procedure TGIS_PvlViewerBmp.RaiseHelpEvent(
    _sender : TObject;
    _name   : String
  );
  begin
    PlatformControlVwr.RaiseHelpEvent( _sender, _name );
  end;
{$ENDIF}


  function TGIS_PvlViewerBmp.AssignedBusyEvent
    : TGIS_BusyEvent ;
  begin
    Result := PlatformControlVwr.AssignedBusyEvent() ;
  end ;

  function TGIS_PvlViewerBmp.AssignedHelpEvent
    : TGIS_HelpEvent ;
  begin
    Result := PlatformControlVwr.AssignedHelpEvent() ;
  end ;

  function TGIS_PvlViewerBmp.StorePaintState
    : TObject ;
  begin
    Result := PlatformControlVwr.StorePaintState ;
  end ;

  procedure TGIS_PvlViewerBmp.RestorePaintState(
    var _state : TObject
  ) ;
  begin
    PlatformControlVwr.RestorePaintState( _state ) ;
  end ;

  procedure TGIS_PvlViewerBmp.BeginPaintInternal ;
  begin
    PlatformControlVwr.BeginPaintInternal ;
  end ;

  procedure TGIS_PvlViewerBmp.EndPaintInternal ;
  begin
    PlatformControlVwr.EndPaintInternal ;
  end ;

  function TGIS_PvlViewerBmp.SynchronizePaint(
    const _interrupt : Boolean
  ) : Boolean ;
  begin
    Result := PlatformControlVwr.SynchronizePaint( _interrupt ) ;
  end;

  procedure TGIS_PvlViewerBmp.ReParentLock ;
  begin
    PlatformControlVwr.ReParentLock ;
  end ;

  procedure TGIS_PvlViewerBmp.ReParentUnlock ;
  begin
    PlatformControlVwr.ReParentUnlock ;
  end ;

  function TGIS_PvlViewerBmp.ReParent(
    const _parent : IGIS_ViewerParent
  ) : IGIS_ViewerParent ;
  begin
    Result := PlatformControlVwr.ReParent( _parent ) ;
  end ;

  function TGIS_PvlViewerBmp.AttachLayer(
    const _layer : TGIS_LayerAbstract
  ) : IGIS_Viewer ;
  begin
    Result := PlatformControlVwr.AttachLayer( _layer ) ;
  end ;

  procedure TGIS_PvlViewerBmp.SetSize(
    const _width  : Integer ;
    const _height : Integer
  );
  begin
    PlatformControlBmp.SetSize( _width, _height ) ;
  end ;

  procedure TGIS_PvlViewerBmp.Open(
    const _path    : String
  ) ;
  begin
    PlatformControlVwr.Open( _path ) ;
  end ;

  procedure TGIS_PvlViewerBmp.Open(
    const _path    : String ;
    const _strict  : Boolean
  ) ;
  begin
    PlatformControlVwr.Open( _path, _strict  ) ;
  end ;

  procedure TGIS_PvlViewerBmp.OpenEx(
    const _configFile : TGIS_ConfigAbstract ;
    const _path       : String
  ) ;
  begin
    PlatformControlVwr.OpenEx( _configFile, _path ) ;
  end ;

  procedure TGIS_PvlViewerBmp.OpenEx(
    const _configFile : TGIS_ConfigAbstract ;
    const _path       : String              ;
    const _strict     : Boolean
  ) ;
  begin
    PlatformControlVwr.OpenEx( _configFile, _path, _strict ) ;
  end ;

  procedure TGIS_PvlViewerBmp.Close ;
  begin
    PlatformControlVwr.Close ;
  end ;

  procedure TGIS_PvlViewerBmp.ReadConfig ;
  begin
    PlatformControlVwr.ReadConfig ;
  end ;

  procedure TGIS_PvlViewerBmp.RereadConfig ;
  begin
    PlatformControlVwr.RereadConfig ;
  end ;

  procedure TGIS_PvlViewerBmp.WriteConfig ;
  begin
    PlatformControlVwr.WriteConfig ;
  end ;

  procedure TGIS_PvlViewerBmp.Add(
    const _layer : TGIS_LayerAbstract
  ) ;
  begin
    PlatformControlVwr.Add( _layer ) ;
  end ;

  function TGIS_PvlViewerBmp.Get(
    const _name    : String
  ) : TGIS_LayerAbstract ;
  begin
    Result := PlatformControlVwr.Get( _name ) ;
  end ;

  procedure TGIS_PvlViewerBmp.Delete(
    const _name : String
  ) ;
  begin
    PlatformControlVwr.Delete( _name ) ;
  end ;

  procedure TGIS_PvlViewerBmp.AddHierarchy ;
  begin
    PlatformControlVwr.AddHierarchy ;
  end ;

  procedure TGIS_PvlViewerBmp.Draw(
    const _renderer : TObject     ;
    const _mode     : TGIS_DrawMode
  ) ;
  begin
    PlatformControlVwr.Draw( _renderer, _mode ) ;
  end ;

  function TGIS_PvlViewerBmp.GetGrid(
    const _extent : TGIS_Extent ;
    const _grid   : TGIS_GridArray
  ) : Boolean ;
  begin
    Result := PlatformControlVwr.GetGrid( _extent, _grid ) ;
  end ;

  procedure TGIS_PvlViewerBmp.RevertAll ;
  begin
    PlatformControlVwr.RevertAll ;
  end ;

  procedure TGIS_PvlViewerBmp.SaveProject ;
  begin
    PlatformControlVwr.SaveProject ;
  end ;

  procedure TGIS_PvlViewerBmp.SaveProject(
    const _relativepath : Boolean
  ) ;
  begin
    PlatformControlVwr.SaveProject( _relativepath ) ;
  end ;

  procedure TGIS_PvlViewerBmp.SaveProjectAs(
    const _path         : String
  ) ;
  begin
    PlatformControlVwr.SaveProjectAs( _path ) ;
  end ;

  procedure TGIS_PvlViewerBmp.SaveProjectAs(
    const _path         : String ;
    const _relativepath : Boolean
  ) ;
  begin
    PlatformControlVwr.SaveProjectAs( _path, _relativepath ) ;
  end ;

  procedure TGIS_PvlViewerBmp.SaveProjectAsEx(
    const _configFile   : TGIS_ConfigAbstract ;
    const _path         : String
  ) ;
  begin
    PlatformControlVwr.SaveProjectAsEx( _configFile, _path ) ;
  end ;

  procedure TGIS_PvlViewerBmp.SaveProjectAsEx(
    const _configFile   : TGIS_ConfigAbstract ;
    const _path         : String              ;
    const _relativepath : Boolean
  ) ;
  begin
    PlatformControlVwr.SaveProjectAsEx( _configFile, _path, _relativepath ) ;
  end ;

  procedure TGIS_PvlViewerBmp.SaveData ;
  begin
    PlatformControlVwr.SaveData ;
  end ;

  procedure TGIS_PvlViewerBmp.SaveAll ;
  begin
    PlatformControlVwr.SaveAll ;
  end ;

  function  TGIS_PvlViewerBmp.MustSave
    : Boolean ;
  begin
    Result := PlatformControlVwr.MustSave ;
  end ;

  procedure TGIS_PvlViewerBmp.MarkModified ;
  begin
    PlatformControlVwr.MarkModified ;
  end ;

  procedure TGIS_PvlViewerBmp.RecalcExtent ;
  begin
    PlatformControlVwr.RecalcExtent ;
  end ;

  procedure TGIS_PvlViewerBmp.Reposition ;
  begin
    PlatformControlVwr.Reposition ;
  end ;

  procedure TGIS_PvlViewerBmp.InvalidateExtent(
    const _extent  : TGIS_Extent
  ) ;
  begin
    PlatformControlVwr.InvalidateExtent( _extent ) ;
  end ;

  procedure TGIS_PvlViewerBmp.InvalidateExtent(
    const _extent : TGIS_Extent ;
    const _deep   : Boolean
  ) ;
  begin
    PlatformControlVwr.InvalidateExtent( _extent, _deep ) ;
  end ;

  procedure TGIS_PvlViewerBmp.InvalidateWholeMap ;
  begin
    PlatformControlVwr.InvalidateWholeMap ;
  end ;

  procedure TGIS_PvlViewerBmp.InvalidateTopmost ;
  begin
    PlatformControlVwr.InvalidateTopmost ;
  end ;

  procedure TGIS_PvlViewerBmp.InvalidateBasemap ;
  begin
    PlatformControlVwr.InvalidateBasemap ;
  end ;

  procedure TGIS_PvlViewerBmp.InvalidateSelection ;
  begin
    PlatformControlVwr.InvalidateSelection ;
  end ;

  procedure TGIS_PvlViewerBmp.InvalidateEditor(
    const _final : Boolean
  ) ;
  begin
    PlatformControlVwr.InvalidateEditor( _final ) ;
  end ;

  function TGIS_PvlViewerBmp.FullExtentZoom
    : Double ;
  begin
    Result := PlatformControlVwr.FullExtentZoom ;
  end ;

  procedure TGIS_PvlViewerBmp.FullExtent ;
  begin
    PlatformControlVwr.FullExtent ;
  end ;

  function TGIS_PvlViewerBmp.Locate(
    const _ptg     : TGIS_Point ;
    const _prec    : Double
  ) : TGIS_ShapeAbstract ;
  begin
    Result := PlatformControlVwr.Locate( _ptg, _prec ) ;
  end ;

  function TGIS_PvlViewerBmp.Locate(
    const _ptg     : TGIS_Point ;
    const _prec    : Double     ;
    const _visible : Boolean
  ) : TGIS_ShapeAbstract ;
  begin
    Result := PlatformControlVwr.Locate( _ptg, _prec, _visible ) ;
  end ;

  function TGIS_PvlViewerBmp.Locate(
    const _pt      : TPoint     ;
    const _prec    : Integer
  ) : TGIS_ShapeAbstract ;
  begin
    Result := PlatformControlVwr.Locate( _pt, _prec ) ;
  end ;

  function TGIS_PvlViewerBmp.LocateEx(
    const _ptg     : TGIS_Point ;
    const _prec    : Double    ;
    const _visible : Boolean
  ) : TGIS_ShapeAbstractList ;
  begin
    Result := PlatformControlVwr.LocateEx( _ptg, _prec, _visible ) ;
  end ;

  function TGIS_PvlViewerBmp.MapToScreen(
    const _ptg : TGIS_Point
  ) : TPoint ;
  begin
    Result := PlatformControlVwr.MapToScreen( _ptg ) ;
  end ;

  function TGIS_PvlViewerBmp.MapToScreen3D(
    const _ptg : TGIS_Point3D
  ) : TPoint ;
  begin
    Result := PlatformControlVwr.MapToScreen3D( _ptg ) ;
  end ;

  function TGIS_PvlViewerBmp.ScreenToMap(
    const _pt : TPoint
  ) : TGIS_Point ;
  begin
    Result := PlatformControlVwr.ScreenToMap( _pt ) ;
  end ;

  function TGIS_PvlViewerBmp.ScreenToMap3D(
    const _pt : TPoint
  ) : TGIS_Point3D ;
  begin
    Result := PlatformControlVwr.ScreenToMap3D( _pt ) ;
  end ;

  function TGIS_PvlViewerBmp.MapToScreenEx(
    const _pt : TGIS_Point
  ) : TGIS_Point ;
  begin
    Result := PlatformControlVwr.MapToScreenEx( _pt ) ;
  end ;

  function TGIS_PvlViewerBmp.ScreenToMapEx(
    const _pt : TGIS_Point
  ) : TGIS_Point ;
  begin
    Result := PlatformControlVwr.ScreenToMapEx( _pt ) ;
  end ;

  function TGIS_PvlViewerBmp.MapToScreenRect(
    const _rct : TGIS_Extent
  ) : TRect ;
  begin
    Result := PlatformControlVwr.MapToScreenRect( _rct ) ;
  end ;

  function TGIS_PvlViewerBmp.ScreenToMapRect(
    const _rct : TRect
  ) : TGIS_Extent ;
  begin
    Result := PlatformControlVwr.ScreenToMapRect( _rct ) ;
  end ;

  function TGIS_PvlViewerBmp.PixelsToTwips(
    const _size : Integer
  ) : Integer ;
  begin
    Result := PlatformControlVwr.PixelsToTwips( _size ) ;
  end ;

  function TGIS_PvlViewerBmp.TwipsToPixels(
    const _size : Integer
  ) : Integer ;
  begin
    Result := PlatformControlVwr.TwipsToPixels( _size ) ;
  end ;

  function TGIS_PvlViewerBmp.TwipsToPoints(
    const _size : Integer
  ) : Integer ;
  begin
    Result := PlatformControlVwr.TwipsToPoints( _size ) ;
  end ;

  procedure TGIS_PvlViewerBmp.MoveViewport(
    var _dx : Integer ;
    var _dy : Integer
  ) ;
  begin
    PlatformControlVwr.MoveViewport( _dx, _dy ) ;
  end ;
{$IFDEF JAVA}
  procedure TGIS_PvlViewerBmp.MoveViewportEx(
    var _dx : java.lang.Double ;
    var _dy : java.lang.Double
  ) ;
  begin
    PlatformControlVwr.MoveViewportEx( _dx, _dy ) //;  Java problems
  end ;

  procedure TGIS_PvlViewerBmp.SetViewport(
    var _x : java.lang.Double ;
    var _y : java.lang.Double
  ) ;
  begin
    PlatformControlVwr.SetViewport( _x, _y ) //;  Java problems
  end ;
{$ELSE}
  procedure TGIS_PvlViewerBmp.MoveViewportEx(
    var _dx : Double ;
    var _dy : Double
  ) ;
  begin
    //?PlatformControlVwr.MoveViewportEx( _dx, _dy ) ;  Java problems
  end ;

  procedure TGIS_PvlViewerBmp.SetViewport(
    var _x : Double ;
    var _y : Double
  ) ;
  begin
    //?PlatformControlVwr.SetViewport( _x, _y ) ;  Java problems
  end ;
{$ENDIF}
  procedure TGIS_PvlViewerBmp.CenterViewport(
    const _ptg : TGIS_Point
  ) ;
  begin
    PlatformControlVwr.CenterViewport( _ptg ) ;
  end ;

  procedure TGIS_PvlViewerBmp.SetCSByWKT(
    const _wkt : String
  ) ;
  begin
    PlatformControlVwr.SetCSByWKT( _wkt ) ;
  end ;

  procedure TGIS_PvlViewerBmp.SetCSByEPSG(
    const _epsg : Integer
  ) ;
  begin
    PlatformControlVwr.SetCSByEPSG( _epsg ) ;
  end ;

  procedure TGIS_PvlViewerBmp.SetCSByWKTFile(
    const _path : String
  ) ;
  begin
    PlatformControlVwr.SetCSByWKTFile( _path ) ;
  end ;

  function TGIS_PvlViewerBmp.RotatedPoint(
    const _ptg : TGIS_Point
  ) : TGIS_Point ;
  begin
    Result := PlatformControlVwr.RotatedPoint( _ptg ) ;
  end ;

  function TGIS_PvlViewerBmp.UnrotatedPoint(
    const _ptg : TGIS_Point
  ) : TGIS_Point ;
  begin
    Result := PlatformControlVwr.UnrotatedPoint( _ptg ) ;
  end ;

  function TGIS_PvlViewerBmp.RotatedPoint3D(
    const _ptg : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    Result := PlatformControlVwr.RotatedPoint3D( _ptg ) ;
  end ;

  procedure TGIS_PvlViewerBmp.RotatedPoint3D_ref(
    var _ptg : TGIS_Point3D
  ) ;
  begin
    PlatformControlVwr.RotatedPoint3D_ref( _ptg ) ;
  end ;

  function TGIS_PvlViewerBmp.UnrotatedPoint3D(
    const _ptg : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    Result := PlatformControlVwr.UnrotatedPoint3D( _ptg ) ;
  end ;

  procedure TGIS_PvlViewerBmp.UnrotatedPoint3D_ref(
    var _ptg : TGIS_Point3D
  ) ;
  begin
    PlatformControlVwr.UnrotatedPoint3D_ref( _ptg ) ;
  end ;

  function TGIS_PvlViewerBmp.RotatedExtent(
    const _extent : TGIS_Extent
  ) : TGIS_Extent ;
  begin
    Result := PlatformControlVwr.RotatedExtent( _extent ) ;
  end ;

  function TGIS_PvlViewerBmp.UnrotatedExtent(
    const _extent : TGIS_Extent
  ) : TGIS_Extent ;
  begin
    Result := PlatformControlVwr.UnrotatedExtent( _extent ) ;
  end ;

  function TGIS_PvlViewerBmp.GetRenderContext
    : TObject ;
  begin
    Result := PlatformControlVwr.GetRenderContext ;
  end ;

  procedure TGIS_PvlViewerBmp.WaitForBackgroundProcesses ;
  begin
    PlatformControlVwr.WaitForBackgroundProcesses ;
  end;

  procedure TGIS_PvlViewerBmp.WaitForNotBusy(
          _sender : TObject ;
    const _proc   : TGIS_WaitForNotBusyProc
  ) ;
  begin
    PlatformControlVwr.WaitForNotBusy( _sender, _proc ) ;
  end;

{$ENDREGION 'public methods of IGIS_Viewer'}

{$REGION 'public methods of IGIS_ViewerBmp'}

  procedure TGIS_PvlViewerBmp.ZoomIn ;
  begin
    PlatformControlBmp.ZoomIn ;
  end ;

  procedure TGIS_PvlViewerBmp.ZoomOut ;
  begin
    PlatformControlBmp.ZoomOut ;
  end ;

  procedure TGIS_PvlViewerBmp.Clear;
  begin
    PlatformControlBmp.Clear ;
  end ;

  procedure TGIS_PvlViewerBmp.Draw ;
  begin
    PlatformControlBmp.Draw ;
  end ;

  procedure TGIS_PvlViewerBmp.ScrollPgUp ;
  begin
    PlatformControlBmp.ScrollPgUp ;
  end ;

  procedure TGIS_PvlViewerBmp.ScrollPgDn ;
  begin
    PlatformControlBmp.ScrollPgDn ;
  end ;

  procedure TGIS_PvlViewerBmp.ScrollPgRight ;
  begin
    PlatformControlBmp.ScrollPgRight ;
  end ;

  procedure TGIS_PvlViewerBmp.ScrollPgLeft ;
  begin
    PlatformControlBmp.ScrollPgLeft ;
  end ;

  procedure TGIS_PvlViewerBmp.ScrollUp ;
  begin
    PlatformControlBmp.ScrollUp ;
  end ;

  procedure TGIS_PvlViewerBmp.ScrollDn ;
  begin
    PlatformControlBmp.ScrollDn ;
  end ;

  procedure TGIS_PvlViewerBmp.ScrollRight ;
  begin
    PlatformControlBmp.ScrollRight ;
  end ;

  procedure TGIS_PvlViewerBmp.ScrollLeft ;
  begin
    PlatformControlBmp.ScrollLeft ;
  end ;

{$ENDREGION 'public methods of IGIS_ViewerBmp'}

{$REGION 'Other public methods'}

  procedure TGIS_PvlViewerBmp.PrintBmp(
    var   _bmp    : TGIS_Bitmap
  ) ;
  begin
    PrintBmp( _bmp, False ) ;
  end ;

  procedure TGIS_PvlViewerBmp.PrintBmp(
    var   _bmp    : TGIS_Bitmap ;
    const _full   : Boolean
  ) ;
  begin
//?    PlatformControlVwr.PrintBmp( _bmp, _full ) ;
  end ;

{$ENDREGION 'Other public methods'}

{$REGION 'TGIS_PvlViewerBmp specific'}

procedure TGIS_PvlViewerBmp.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.Platform.CreateObject(self, 'ViewerBmp');
end;

function TGIS_PvlViewerBmp.fget_PlatformControl
  : IGIS_PvlViewerBmp;
begin
  Result := oPlatform as IGIS_PvlViewerBmp;
end;

function TGIS_PvlViewerBmp.fget_PlatformControlVwr
  : IGIS_Viewer;
begin
  Result := oPlatform as IGIS_Viewer;
end;

function TGIS_PvlViewerBmp.fget_PlatformControlBmp
  : IGIS_ViewerBmp;
begin
  Result := oPlatform as IGIS_ViewerBmp;
end;

{$ENDREGION 'TGIS_PvlViewerBmp specific'}

//==================================== END =====================================
end.
