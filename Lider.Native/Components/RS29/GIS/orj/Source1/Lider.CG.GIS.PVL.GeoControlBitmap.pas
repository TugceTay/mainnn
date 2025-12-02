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
  Legend component. Vector dialog - bitmap selector.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.PVL.GeoControlBitmap ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.PVL.GeoControlBitmap"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK.PVL ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk.pvl ;
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  {$IFDEF CLR}
    TatukGIS.ndk,
    TatukGIS.RTL ;
  {$ENDIF}

  {$IFDEF DCC}
    System.Classes,

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoParams,
    Lider.CG.GIS.PVL.GeoPvl,
    Lider.CG.GIS.PVL.GeoPvlForms;
  {$ENDIF}

  {$IFDEF JAVA}
    java.util,
    java.awt.*,
    javax.swing.*,
    java.beans.*,
    tatukgis.jdk.*,
    tatukgis.rtl ;
  {$ENDIF}


type

  /// <summary>
  ///   Visual form for managing bitmaps.
  /// </summary>
  /// <remarks>
  ///   To be use only from: TGIS_ControlLegend.
  /// </remarks>
  TGIS_ControlBitmap = class( TGIS_PvlModalForm )
  private
    btnSelectFile   : TGIS_PvlButton        ;
    dlgOpen         : TGIS_PvlOpenDialog    ;
    pnlMain         : TGIS_PvlPreviewPanel  ;

  private
    procedure actSelectFileExecute(Sender: TObject);

  private // properties internal value
    FBitmap : TGIS_Bitmap ;

  public
    /// <inheritdoc/>
    procedure DoInitForm         ; override;

    /// <inheritdoc/>
    procedure DoInitControls     ; override;

  protected // properties access routine
    procedure fset_Bitmap( const _value : TGIS_Bitmap ) ;

  public

    /// <summary>
    ///   Execute dialog on a bitmap given by path.
    /// </summary>
    /// <param name="_path">
    ///   path to the bitmap.
    /// </param>
    /// <returns>
    ///   Modal result.
    /// </returns>
    function Execute   ( const _path   : String
                       ) : TGIS_PvlModalResult ; overload;

    /// <summary>
    ///   Execute dialog on a bitmap given by path.
    /// </summary>
    /// <param name="_path">
    ///   path to the bitmap.
    /// </param>
    /// <param name="_proc">
    ///   Action to be performed after closing modal form.
    /// </param>
    /// <returns>
    ///   Modal result.
    /// </returns>
    function  Execute  ( const _path   : String ;
                         const _proc   : TGIS_Proc
                       ) : TGIS_PvlModalResult ; overload;

    /// <summary>
    ///   Execute dialog on a bitmap given by path.
    /// </summary>
    /// <param name="_path">
    ///   path to the bitmap
    /// </param>
    /// <param name="_onhelp">
    ///   help notification function; if assigned the help button will be
    ///   visible and help support will be enabled
    /// </param>
    /// <param name="_proc">
    ///   Action to be performed after closing modal form.
    /// </param>
    /// <returns>
    ///   Modal result.
    /// </returns>
    function  Execute  ( const _path   : String        ;
                         const _onhelp : TGIS_HelpEvent;
                         const _proc   : TGIS_Proc
                       ) : TGIS_PvlModalResult ; overload;

  public
    /// <summary>
    ///   Used bitmap.
    /// </summary>
    property Bitmap : TGIS_Bitmap read FBitmap write fset_Bitmap ;

  protected
    /// <inheritdoc/>
    procedure doDestroy ; override ;
  end;

//##############################################################################
implementation
{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoResource;
{$ENDIF}

//==============================================================================
// Private events & methods
//==============================================================================

  procedure TGIS_ControlBitmap.fset_Bitmap( const _value : TGIS_Bitmap ) ;
  begin
    if not assigned( _value ) then begin
      FreeObject( FBitmap ) ;
      pnlMain.Bitmap := nil ;
      pnlMain.Caption := GIS_RS_LEGEND_NONE ;
      exit ;
    end ;

    if not assigned( FBitmap ) then
      FBitmap := TGIS_Bitmap.Create ;

    FBitmap.Assign( _value ) ;
    FBitmap.Transparent := False ;
    pnlMain.Bitmap := FBitmap ;
    pnlMain.Caption := '' ;
    pnlMain.Invalidate ;
  end ;

//==============================================================================
// Control events & methods
//==============================================================================

  procedure TGIS_ControlBitmap.actSelectFileExecute(Sender: TObject);
  var
    bmp : TGIS_Bitmap ;
  begin
    if not ( dlgOpen.Execute = TGIS_PvlModalResult.OK ) then exit ;

    bmp := TGIS_Bitmap.Create ;
    try
      try
        bmp.Transparent := False ;
        bmp.LoadFromFile( dlgOpen.FileName );
        fset_Bitmap( bmp ) ;
      except
        fset_Bitmap( nil ) ;
      end ;
    finally
      FreeObject( bmp ) ;
    end ;
  end;

//==============================================================================
// Public events & methods
//==============================================================================

  procedure TGIS_ControlBitmap.DoInitForm ;
  begin
    Self.Caption := _rsrc( GIS_RS_LEGEND_DLGBITMAP ) ;
    Self.ClientHeight := 212 ;
    Self.ClientWidth := 260 ;
    Self.Name := 'TGIS_ControlBitmap' ;
  end ;

  procedure TGIS_ControlBitmap.DoInitControls ;
  var
    ytop : Integer ;
  begin
    ytop := Context.VMargin ;

    btnSelectFile := TGIS_PvlButton.Create( Context ) ;
    btnSelectFile.Place( 75, 25, nil, Context.HMargin, nil, ytop ) ;
    btnSelectFile.Caption := _rsrc( GIS_RS_LEGEND_DLGSYMBOL_FILE ) ;
    {$IFDEF DCC}
      btnSelectFile.OnClick := actSelectFileExecute ;
    {$ELSE}
      btnSelectFile.OnClick := @actSelectFileExecute ;
    {$ENDIF}
    btnSelectFile.TabOrder := 0 ;

    ytop := Context.VMargin ;

    pnlMain := TGIS_PvlPreviewPanel.Create( Context ) ;
    pnlMain.Place( 163, 153, nil, btnSelectFile.Left + btnSelectFile.Width + Context.HMargin, nil, ytop ) ;
    pnlMain.Border := True ;
    pnlMain.Caption := _rsrc( GIS_RS_LEGEND_NONE ) ;
    pnlMain.TabOrder := 1 ;

    BtnHelp.Visible := assigned( OnHelpEvent ) ;
    BtnHelp.TabOrder := 2 ;
    BtnCancel.TabOrder := 4 ;
    BtnOK.TabOrder := 3 ;

    dlgOpen := TGIS_PvlOpenDialog.Create( Context ) ;
    dlgOpen.Filter := _rsrc( GIS_RS_LEGEND_DLGBITMAP_FILTER ) ;
  end ;

  function TGIS_ControlBitmap.Execute(
    const _path : String
  ) : TGIS_PvlModalResult;
  begin
    Result := Execute( _path, nil ) ;
  end ;

  function TGIS_ControlBitmap.Execute(
    const _path   : String ;
    const _proc   : TGIS_Proc
  ) : TGIS_PvlModalResult ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := Execute( _path, hlp, _proc ) ;
  end;

  function TGIS_ControlBitmap.Execute(
    const _path   : String         ;
    const _onhelp : TGIS_HelpEvent ;
    const _proc   : TGIS_Proc
  ) : TGIS_PvlModalResult ;
  var
    bmp : TGIS_Bitmap ;
  begin
    OnHelpEvent := _onhelp ;
    BtnHelp.Visible := assigned( OnHelpEvent ) ;

    dlgOpen.FileName := _path ;

    bmp := TGIS_Bitmap.Create ;
    bmp.Transparent := False ;
    try
      try
        if not IsStringEmpty( _path ) then begin
          bmp.LoadFromFile( _path ) ;
          fset_Bitmap( bmp ) ;
        end
        else
          fset_Bitmap( nil ) ;
      except
        fset_Bitmap( nil ) ;
      end ;
    finally
     FreeObject( bmp ) ;
    end ;

    Result := ShowModal( _proc, assigned( _proc ) ) ;
  end ;

  procedure TGIS_ControlBitmap.doDestroy;
  begin
    FreeObject( FBitmap ) ;
    inherited ;
  end;

//==================================== END =====================================
end.
