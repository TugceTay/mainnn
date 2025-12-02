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
  Visual control for displaying print preview.
}

{$IFDEF DCC}
  unit PVL.GisControlPrintPreview;
  {$HPPEMIT '#pragma link "PVL.GisControlAttributes"'}
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
    GisCsBase,
    GisTypes,
    GisTypesUI,
    GisPrintManagerAbstract,
    GisInterfaces,
    GisLayerVector,

    PVL.GisPvl,
    PVL.GisViewerWnd;
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
  ///   PVL control attributes intraface
  /// </summary>
  IGIS_PvlControlPrintPreview = interface( IGIS_Subscribe )
    {$IFDEF DCC}
      ['{60907F06-C1B8-441E-A243-6F8B596D7F69}']
    {$ENDIF}

      /// <summary>
      ///   Preview selected page.
      /// </summary>
      /// <param name="_pageNumber">
      ///   page number to be previewed (1 means the first page)
      /// </param>
      /// <param name="_printer">
      ///   printer object
      /// </param>
      /// <param name="_printManager">
      ///   print manager object or nil
      /// </param>
      /// <returns>
      ///   title string
      /// </returns>
      /// <remarks>
      ///   <para>
      ///     Printing fails when GIS_Viewer.InPaint is set to True.
      ///   </para>
      /// </remarks>
      function  Preview   ( const _pageNumber   : Integer ;
                            const _printer      : TObject ;
                            const _printManager : TGIS_PrintManagerAbstract
                          ) : String ; overload ;

      /// <summary>
      ///   Preview selected page.
      /// </summary>
      /// <param name="_pageNumber">
      ///   page number to be previewed (1 means the first page)
      /// </param>
      /// <param name="_printer">
      ///   printer object
      /// </param>
      /// <param name="_printManager">
      ///   print manager object or nil
      /// </param>
      /// <param name="_scale">
      ///    scale factor used during print (for printing scale etc);
      ///    if scale=0 then scale will be calculated automatically to fit
      ///    the _extent
      /// </param>
      /// <returns>
      ///   title string
      /// </returns>
      /// <remarks>
      ///   <para>
      ///     Printing fails when GIS_Viewer.InPaint is set to True.
      ///   </para>
      /// </remarks>
      function  Preview   ( const _pageNumber   : Integer ;
                            const _printer      : TObject ;
                            const _printManager : TGIS_PrintManagerAbstract ;
                            var   _scale        : Double
                          ) : String ; overload ;

      /// <summary>
      ///   Preview selected page.
      /// </summary>
      /// <param name="_pageNumber">
      ///   page number to be previewed (1 means the first page)
      /// </param>
      /// <param name="_printer">
      ///   printer object
      /// </param>
      /// <param name="_printManager">
      ///   print manager object or nil
      /// </param>
      /// <param name="_customPage">
      ///   default page orientation: "Landscape" or "Portrait"
      /// </param>
      /// <param name="_scale">
      ///    scale factor used during print (for printing scale etc);
      ///    if scale=0 then scale will be calculated automatically to fit
      ///    the _extent
      /// </param>
      /// <returns>
      ///   title string
      /// </returns>
      /// <remarks>
      ///   <para>
      ///     Printing fails when GIS_Viewer.InPaint is set to True.
      ///   </para>
      /// </remarks>
      function  Preview   ( const _pageNumber   : Integer ;
                            const _printer      : TObject ;
                            const _printManager : TGIS_PrintManagerAbstract ;
                            const _customPage   : String ;
                            var   _scale        : Double
                          ) : String ; overload ;

      /// <summary>
      ///   Print document.
      /// </summary>
      /// <param name="_printer">
      ///   printer object
      /// </param>
      /// <remarks>
      ///   <para>
      ///     Printing fails when GIS_Viewer.InPaint is set to True.
      ///   </para>
      /// </remarks>
      procedure   Print   ( const _printer    : TObject
                          ) ; overload;

      /// <summary>
      ///   Print document.
      /// </summary>
      /// <remarks>
      ///   <para>
      ///     Printing fails when GIS_Viewer.InPaint is set to True.
      ///   </para>
      /// </remarks>
      procedure   Print   ; overload;

      /// <summary>
      ///   Getter for IsPrinter property.
      /// </summary>
      /// <returns>
      ///   True, if any printer exists.
      /// </returns>
      function    fget_IsPrinter  : Boolean ;

      /// <summary>
      ///   Getter for IsLast property.
      /// </summary>
      /// <returns>
      ///   True, if last page in preview was reached.
      /// </returns>
      function    fget_IsLast  : Boolean ;

      /// <summary>
      ///   Setter for GIS_Viewer property.
      /// </summary>
      /// <param name="_viewer">
      ///   New GIS_Viewer value.
      /// </param>
      procedure   fset_GIS_Viewer( const _viewer : TGIS_PvlViewerWnd
                                 ) ;

      /// <summary>
      ///   Getter for GIS_Viewer property.
      /// </summary>
      /// <returns>
      ///   Current GIS_Viewer value.
      /// </returns>
      function    fget_GIS_Viewer : TGIS_PvlViewerWnd ;

      /// <inheritdoc form="IGIS_Subscribe"/>
      procedure SubscribedEvent(       _sender  : TObject ;
                                       _event   : Integer ;
                                       _context : TObject
                               ) ;
  end;

  /// <summary>
  ///   PVL attributes control.
  /// </summary>
  TGIS_PvlControlPrintPreview = class( TGIS_PvlControl, IGIS_PvlControlPrintPreview )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
    protected
      /// <inheritdoc/>
      procedure initControl       ;                              override;

    private
      function fget_PlatformControl
                                  : IGIS_PvlControlPrintPreview ;          reintroduce;
      property PlatformControl    : IGIS_PvlControlPrintPreview
                                    read  fget_PlatformControl;  {$IFDEF OXYGENE}reintroduce;{$ENDIF}

    protected

      /// <inheritdoc/>
      function    fget_IsPrinter  : Boolean ;

      /// <inheritdoc/>
      function    fget_IsLast  : Boolean ;

      /// <inheritdoc/>
      procedure   fset_GIS_Viewer( const _viewer : TGIS_PvlViewerWnd
                                 ) ;

      /// <inheritdoc/>
      function    fget_GIS_Viewer : TGIS_PvlViewerWnd ;

    public

      /// <inheritdoc/>
      function  Preview   ( const _pageNumber   : Integer ;
                            const _printer      : TObject ;
                            const _printManager : TGIS_PrintManagerAbstract
                          ) : String ; overload ;

      /// <inheritdoc/>
      function  Preview   ( const _pageNumber   : Integer ;
                            const _printer      : TObject ;
                            const _printManager : TGIS_PrintManagerAbstract ;
                            var   _scale        : Double
                          ) : String ; overload ;

      /// <inheritdoc/>
      function  Preview   ( const _pageNumber   : Integer ;
                            const _printer      : TObject ;
                            const _printManager : TGIS_PrintManagerAbstract ;
                            const _customPage   : String ;
                            var   _scale        : Double
                          ) : String ; overload ;

      /// <inheritdoc/>
      procedure   Print   ( const _printer    : TObject
                          ) ; overload;

      /// <inheritdoc/>
      procedure   Print   ; overload;

    protected // IGIS_Viewer property access routines

      /// <inheritdoc form="IGIS_Subscribe"/>
      procedure SubscribedEvent(       _sender  : TObject ;
                                       _event   : Integer ;
                                       _context : TObject
                               ) ;

    published

      /// <summary>
      ///   Viewer to which Print Preview will be attached.
      /// </summary>
      property GIS_Viewer : TGIS_PvlViewerWnd read  fget_GIS_Viewer
                                              write fset_GIS_Viewer;

      /// <summary>
      ///   True, if last page in preview was reached.
      /// </summary>
      property IsLast     : Boolean           read  fget_IsLast ;

      /// <summary>
      ///   True, if any printer exists.
      /// </summary>
      property IsPrinter  : Boolean           read  fget_IsPrinter ;
//
//      {$IFDEF LEVEL_RX10_FMX}
//        /// <summary>
//        ///   Styled settings.
//        /// </summary>
//        property StyledSettings      : TGIS_ControlScaleStyledSettings
//                                       read  FStyledSettings
//                                       write FStyledSettings ;
//      {$ENDIF}
  end;



implementation

{$IFDEF DCC}
  uses
    GisRtl
    // ensure that proper implementation files are referenced in Delphi
    {$IFDEF USE_FMX}
      ,FMX.GisPvlControlPrintPreview
    {$ENDIF}
    {$IFDEF USE_VCL}
      ,VCL.GisPvlControlPrintPreview
    {$ENDIF}
    ;
{$ENDIF}

procedure TGIS_PvlControlPrintPreview.Print ;
begin
  PlatformControl.Print ;
end;

procedure TGIS_PvlControlPrintPreview.Print(
  const _printer: TObject
) ;
begin
  PlatformControl.Print( _printer ) ;
end;

function TGIS_PvlControlPrintPreview.Preview(
  const _pageNumber   : Integer;
  const _printer      : TObject;
  const _printManager : TGIS_PrintManagerAbstract
): string ;
begin
  Result := PlatformControl.Preview( _pageNumber, _printer, _printManager ) ;
end;

function TGIS_PvlControlPrintPreview.Preview(
  const _pageNumber   : Integer;
  const _printer      : TObject;
  const _printManager : TGIS_PrintManagerAbstract;
  var   _scale        : Double
): string ;
begin
  Result := PlatformControl.Preview( _pageNumber, _printer, _printManager, _scale ) ;
end;

function TGIS_PvlControlPrintPreview.Preview(
  const _pageNumber   : Integer;
  const _printer      : TObject;
  const _printManager : TGIS_PrintManagerAbstract;
  const _customPage   : string;
  var   _scale        : Double
): string ;
begin
  Result := PlatformControl.Preview( _pageNumber, _printer, _printManager, _customPage, _scale ) ;
end;

function TGIS_PvlControlPrintPreview.fget_IsPrinter
  : Boolean ;
begin
  Result := PlatformControl.fget_IsPrinter ;
end;

function TGIS_PvlControlPrintPreview.fget_IsLast
  : Boolean ;
begin
  Result := PlatformControl.fget_IsLast ;
end;

procedure TGIS_PvlControlPrintPreview.fset_GIS_Viewer(
  const _viewer: TGIS_PvlViewerWnd
) ;
begin
  PlatformControl.fset_GIS_Viewer( _viewer ) ;
end;

function TGIS_PvlControlPrintPreview.fget_GIS_Viewer
  : TGIS_PvlViewerWnd ;
begin
  Result := PlatformControl.fget_GIS_Viewer ;
end;

procedure TGIS_PvlControlPrintPreview.SubscribedEvent(
  _sender  : TObject ;
  _event   : Integer ;
  _context : TObject
) ;
begin
  PlatformControl.SubscribedEvent( _sender, _event, _context ) ;
end ;

{$REGION 'TGIS_PvlControlPrintPreview specific'}

procedure TGIS_PvlControlPrintPreview.initControl;
begin
  inherited;
end;


procedure TGIS_PvlControlPrintPreview.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.Platform.CreateObject(self, 'PrintPreview');
end;

function TGIS_PvlControlPrintPreview.fget_PlatformControl
  : IGIS_PvlControlPrintPreview;
begin
  Result := oPlatform as IGIS_PvlControlPrintPreview;
end;

{$ENDREGION 'TGIS_PvlControlPrintPreview specific'}

end.
