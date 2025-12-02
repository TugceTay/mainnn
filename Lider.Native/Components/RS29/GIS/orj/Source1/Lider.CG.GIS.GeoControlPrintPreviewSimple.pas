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
  Print Preview Simple component.
}

unit Lider.CG.GIS.GeoControlPrintPreviewSimple ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.GeoControlPrintPreviewSimple"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.UITypes,

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoInterfaces,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoPrintManagerAbstract,
  Lider.CG.GIS.GeoPrintPreviewHelperAbstract ;

type

  /// <summary>
  ///   Control for managing print preview. Simplest version (using predefined form).
  /// </summary>
  [ComponentPlatformsAttribute( pfidAllDesktops )]
  TGIS_ControlPrintPreviewSimple = class( TComponent, IGIS_Subscribe )

    private
      [weak]
      FGIS_Viewer   : IGIS_Viewer  ;
      FCaption      : String       ;
      FWindowState  : TWindowState ;
      FWindowLeft   : Integer      ;
      FWindowTop    : Integer      ;
      FWindowWidth  : Integer      ;
      FWindowHeight : Integer      ;

      FOnHelp       : TGIS_HelpEvent ;

    private // property access routines

      procedure fset_GIS_Viewer( const _value : IGIS_Viewer ) ;
      function  fget_IsPrinter : Boolean ;

    public // API

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_owner">
      ///   owner of the component
      /// </param>
      {#ownership:_owner:ownif_empty}
      constructor Create       ( _owner : TComponent
                               ) ; override;

      /// <summary>
      ///   Destroy dataset.
      /// </summary>
      destructor  Destroy               ; override;

     /// <inheritdoc form="IGIS_Subscribe"/>
      procedure SubscribedEvent(       _sender  : TObject ;
                                       _event   : Integer ;
                                       _context : TObject
                               ) ;

    public

      /// <summary>
      ///   Execute print preview with default layout.
      /// </summary>
      /// <remarks>
      ///   <para>
      ///     Printing fails when GIS_Viewer.InPaint is set to True.
      ///   </para>
      /// </remarks>
      procedure Preview        ; overload;

      /// <summary>
      ///   Execute print preview with layout defined by print manager object.
      /// </summary>
      /// <param name="_printManager">
      ///   print manager object
      /// </param>
      /// <remarks>
      ///   <para>
      ///     Printing fails when GIS_Viewer.InPaint is set to True.
      ///   </para>
      /// </remarks>
      procedure Preview        ( const _printManager : TGIS_PrintManagerAbstract
                               ) ; overload;

      /// <summary>
      ///   Execute print preview with layout defined by print manager object.
      /// </summary>
      /// <param name="_printManager">
      ///   print manager object
      /// </param>
      /// <param name="_scale">
      ///    scale factor used during print (for printing scale etc);
      ///    if scale=0 then scale will be calculated automatically to fit
      ///    the _extent
      /// </param>
      /// <remarks>
      ///   <para>
      ///     Printing fails when GIS_Viewer.InPaint is set to True.
      ///   </para>
      /// </remarks>
      procedure Preview        ( const _printManager : TGIS_PrintManagerAbstract ;
                                 var   _scale        : Double
                               ) ; overload;

      /// <summary>
      ///   Execute print preview with layout defined by print manager object.
      /// </summary>
      /// <param name="_printManager">
      ///   print manager object
      /// </param>
      /// <param name="_customPage">
      ///   custom page format;
      ///   see TGIS_ControlPrintPreview.Preview for more information
      /// </param>
      /// <param name="_scale">
      ///    scale factor used during print (for printing scale etc);
      ///    if scale=0 then scale will be calculated automatically to fit
      ///    the _extent
      /// </param>
      /// <remarks>
      ///   <para>
      ///     Printing fails when GIS_Viewer.InPaint is set to True.
      ///   </para>
      /// </remarks>
      procedure Preview        ( const _printManager : TGIS_PrintManagerAbstract ;
                                 const _customPage   : String ;
                                 var   _scale        : Double
                               ) ; overload;

    public
      /// <summary>
      ///   Check if any printer exists.
      /// </summary>
      /// <returns>
      ///   True, if any printer exists.
      /// </returns>
      property IsPrinter : Boolean         read  fget_IsPrinter ;

    published
      /// <summary>
      ///   Caption of Print Preview form.
      /// </summary>
      property Caption : String            read  FCaption
                                           write FCaption ;

      /// <summary>
      ///   Window state (maximized, minimized etc.).
      /// </summary>
      property WindowState  : TWindowState read  FWindowState
                                           write FWindowState
                                           default TWindowState.wsMaximized ;

      /// <summary>
      ///   Form size.
      /// </summary>
      property WindowLeft : Integer        read  FWindowLeft
                                           write FWindowLeft
                                           default 0 ;

      /// <summary>
      ///   Form size.
      /// </summary>
      property WindowTop : Integer         read  FWindowTop
                                           write FWindowTop
                                           default 0 ;

      /// <summary>
      ///   Form size.
      /// </summary>
      property WindowWidth : Integer       read  FWindowWidth
                                           write FWindowWidth
                                           default 640 ;

      /// <summary>
      ///   Form size.
      /// </summary>
      property WindowHeight : Integer      read  FWindowHeight
                                           write FWindowHeight
                                           default 480 ;

      /// <summary>
      ///   Viewer to which Print Preview will be attached.
      /// </summary>
      property GIS_Viewer : IGIS_Viewer    read  FGIS_Viewer
                                           write fset_GIS_Viewer ;


      /// <event/>
      property HelpEvent : TGIS_HelpEvent  read  FOnHelp
                                           write FOnHelp ;
  end ;

  {#gendoc:hide}
  procedure Register;

//##############################################################################
implementation

{$R Lider.CG.GIS.GeoControlPrintPreviewSimple_16x16.RES}

//==============================================================================
// TGIS_ControlPrintPreviewSimple
//==============================================================================

  constructor TGIS_ControlPrintPreviewSimple.Create(
    _owner : TComponent
  ) ;
  begin
    inherited Create( _owner ) ;

    FGIS_Viewer   := nil ;
    FCaption      := _rsrc( GIS_RS_PREVIEW_DLG ) ;
    FWindowState  := TWindowState.wsMaximized ;
    FWindowLeft   := 0   ;
    FWindowTop    := 0   ;
    FWindowWidth  := 640 ;
    FWindowHeight := 480 ;
    FOnHelp       := nil ;
  end;

  destructor TGIS_ControlPrintPreviewSimple.Destroy;
  begin
    if assigned( FGIS_Viewer ) then
      FGIS_Viewer.UnSubscribe( Self ) ;

    inherited;
  end ;

  procedure TGIS_ControlPrintPreviewSimple.fset_GIS_Viewer(
    const _value : IGIS_Viewer
  ) ;
  begin
    FGIS_Viewer := _value ;

    if assigned( FGIS_Viewer ) then
      FGIS_Viewer.Subscribe( Self ) ;
  end ;

  function TGIS_ControlPrintPreviewSimple.fget_IsPrinter
    : Boolean ;
  begin
    try
      Result := PrintPreviewHelper.IsPrinter ;
    except
      Result := False ;
    end ;
  end ;

  procedure TGIS_ControlPrintPreviewSimple.Preview ;
  var
    scale : Double ;
  begin
    scale := 0 ;
    PrintPreviewHelper.Preview( FGIS_Viewer,
                                nil,
                                '',
                                scale,
                                FCaption,
                                FWindowLeft,
                                FWindowTop,
                                FWindowWidth,
                                FWindowHeight,
                                FWindowState,
                                FOnHelp
                              ) ;
  end ;

  procedure TGIS_ControlPrintPreviewSimple.Preview(
    const _printManager : TGIS_PrintManagerAbstract
  ) ;
  var
    scale : Double ;
  begin
    scale := 0 ;
    PrintPreviewHelper.Preview( FGIS_Viewer,
                                _printManager,
                                '',
                                scale,
                                FCaption,
                                FWindowLeft,
                                FWindowTop,
                                FWindowWidth,
                                FWindowHeight,
                                FWindowState,
                                FOnHelp
                              ) ;
  end ;

  procedure TGIS_ControlPrintPreviewSimple.Preview(
    const _printManager : TGIS_PrintManagerAbstract ;
    var   _scale        : Double
  ) ;
  begin
    PrintPreviewHelper.Preview( FGIS_Viewer,
                                _printManager,
                                '',
                                _scale,
                                FCaption,
                                FWindowLeft,
                                FWindowTop,
                                FWindowWidth,
                                FWindowHeight,
                                FWindowState,
                                FOnHelp
                              ) ;
  end ;

  procedure TGIS_ControlPrintPreviewSimple.Preview(
    const _printManager : TGIS_PrintManagerAbstract ;
    const _customPage   : String ;
    var   _scale        : Double
  ) ;
  begin
    PrintPreviewHelper.Preview( FGIS_Viewer,
                                _printManager,
                                _customPage,
                                _scale,
                                FCaption,
                                FWindowLeft,
                                FWindowTop,
                                FWindowWidth,
                                FWindowHeight,
                                FWindowState,
                                FOnHelp
                              ) ;
  end ;

  procedure TGIS_ControlPrintPreviewSimple.SubscribedEvent(
    _sender  : TObject ;
    _event   : Integer ;
    _context : TObject
  ) ;
  begin
    case _event of
      GIS_SUBSCRIBED_DESTROY :
        begin
          FGIS_Viewer := nil ;
        end ;
    end ;
  end ;

  procedure Register ;
  begin
    RegisterComponents( 'TatukGIS', [ TGIS_ControlPrintPreviewSimple ] ) ;
  end ;

{==================================== END =====================================}
end.


