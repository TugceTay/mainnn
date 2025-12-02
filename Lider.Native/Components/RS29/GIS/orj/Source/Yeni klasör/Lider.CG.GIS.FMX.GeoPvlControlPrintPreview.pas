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
  FMX implementation of TGIS_PvlControlPrintPreview
}


unit FMX.GisPvlControlPrintPreview ;
{$HPPEMIT '#pragma link "FMX.GisPvlControlPrintPreview"'}

{$INCLUDE GisInclude.inc}

interface

uses
  FMX.Controls,
  FMX.Types,

  GisPrintManagerAbstract,

  FMX.GisControlPrintPreview,
  FMX.GisViewerWnd,
  FMX.GisPrintManager,
  FMX.GisPvl,

  PVL.GisPvl,
  PVL.GisViewerWnd,
  PVL.GisControlPrintPreview;

implementation
type

  T_PvlControlPrintPreview = class ( TGIS_PvlControlFmx, IGIS_PvlControlPrintPreview )
    private
      oGIS                        : TGIS_PvlViewerWnd ;
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );override;

      procedure doDestroy         ;override;
    {$IFNDEF OXYGENE} private {$ELSE} assembly {$ENDIF}

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
      /// <param name="_value">
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
    public

    protected // IGIS_Viewer property access routines

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

      /// <inheritdoc form="IGIS_Subscribe"/>
      procedure SubscribedEvent(       _sender  : TObject ;
                                       _event   : Integer ;
                                       _context : TObject
                               ) ;
  end;

//==================================== END =====================================

procedure T_PvlControlPrintPreview.fset_GIS_Viewer(
  const _viewer: TGIS_PvlViewerWnd
) ;
begin
  TGIS_ControlPrintPreview( oControl ).GIS_Viewer := _viewer.NativeControl as TGIS_ViewerWnd ;
  oGIS := _viewer ;
end;

function T_PvlControlPrintPreview.fget_GIS_Viewer
  : TGIS_PvlViewerWnd ;
begin
  Result := oGIS ;
end;

function T_PvlControlPrintPreview.fget_IsPrinter
  : Boolean ;
begin
  Result := TGIS_ControlPrintPreview( oControl ).IsPrinter ;
end;

function T_PvlControlPrintPreview.fget_IsLast
  : Boolean ;
begin
  Result := TGIS_ControlPrintPreview( oControl ).IsLastPage ;
end;

procedure T_PvlControlPrintPreview.Print ;
begin
  TGIS_ControlPrintPreview( oControl ).Print ;
end;

procedure T_PvlControlPrintPreview.Print(
  const _printer: TObject
) ;
begin
    assert( _printer is TGIS_PrintManager ) ;
    TGIS_ControlPrintPreview( oControl ).Print ;
end;

function T_PvlControlPrintPreview.Preview(
  const _pageNumber: Integer;
  const _printer: TObject;
  const _printManager: TGIS_PrintManagerAbstract
): string ;
begin
  Result := TGIS_ControlPrintPreview( oControl ).Preview( _pageNumber, _printManager ) ;
end;

function T_PvlControlPrintPreview.Preview(
  const _pageNumber: Integer;
  const _printer: TObject;
  const _printManager: TGIS_PrintManagerAbstract;
  var   _scale: Double
): string ;
begin
  Result := TGIS_ControlPrintPreview( oControl ).Preview( _pageNumber, _printManager, _scale ) ;
end;

function T_PvlControlPrintPreview.Preview(
  const _pageNumber: Integer;
  const _printer: TObject;
  const _printManager: TGIS_PrintManagerAbstract;
  const _customPage: string;
  var   _scale: Double
): string ;
begin
  Result := TGIS_ControlPrintPreview( oControl ).Preview( _pageNumber, _printManager, _customPage, _scale ) ;
end;

procedure T_PvlControlPrintPreview.SubscribedEvent(
  _sender  : TObject ;
  _event   : Integer ;
  _context : TObject
) ;
begin
  TGIS_ControlPrintPreview( oControl ).SubscribedEvent( _sender, _event, _context ) ;
end ;

{$REGION 'T_PvlControlPrintPreview specific'}

procedure T_PvlControlPrintPreview.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oControl := TGIS_ControlPrintPreview.Create( TFmxObject( _context.NativeParent ) );
  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );
end;

procedure T_PvlControlPrintPreview.doDestroy;
begin
//  FreeObject( oGIS ) ;

  inherited ;
end;

{$ENDREGION 'T_PvlControlAttributes specific'}

initialization
  RegisterPVLPlatformControl( 'PrintPreview', T_PvlControlPrintPreview );
end.

