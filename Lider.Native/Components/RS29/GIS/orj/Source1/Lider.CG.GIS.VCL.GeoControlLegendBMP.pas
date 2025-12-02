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

unit Lider.CG.GIS.VCL.GeoControlLegendBMP ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.VCL.GeoControlLegendBMP"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  Winapi.Windows,
  VCL.Forms,
  VCL.Dialogs,
  VCL.ExtDlgs,
  VCL.Graphics,
  VCL.Controls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  System.Classes,

  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.VCL.GeoModalForm ;

type

  /// <summary>
  ///   Visual form for managing bitmaps.
  /// </summary>
  /// <remarks>
  ///   To be use only from: TGIS_ControlLegend.
  /// </remarks>
  TGIS_ControlLegendBMP = class( TGIS_ModalForm )
    btnSelectFile: TButton;
    dlgOpen: TOpenPictureDialog;
    pnlMain: TPanel;
    imgBitmap: TImage;
    procedure btnSelectFileClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private // properties internal value
    FBitmap : TGIS_Bitmap ;

  protected

    /// <inheritdoc/>
    procedure initForm         ; override;

    /// <inheritdoc/>
    procedure initControls     ; override;

  protected // properties access routine
    procedure fset_Bitmap( const _value : TGIS_Bitmap ) ;

  public

    /// <summary>
    ///   Execute dialog on a bitmap given by path.
    /// </summary>
    /// <param name="_path">
    ///   path to the bitmap.
    /// </param>
    /// <param name="_onhelp">
    ///   help notification function; if assigned the help button will be
    ///   visible and help support will be enabled;
    /// </param>
    /// <returns>
    ///   Modal result.
    /// </returns>
    function Execute  ( const _path   : String        ;
                        const _onhelp : TGIS_HelpEvent
                      ) : Integer ; overload;

    /// <summary>
    ///   Execute dialog on a bitmap given by path.
    /// </summary>
    /// <param name="_path">
    ///   path to the bitmap.
    /// </param>
    /// <returns>
    ///   Modal result.
    /// </returns>
    function Execute  ( const _path   : String
                      ) : Integer ; overload;

  public

      /// <summary>
      ///   Used bitmap.
      /// </summary>
      property Bitmap : TGIS_Bitmap read FBitmap write fset_Bitmap ;
  end;

//##############################################################################
implementation

uses
  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.VCL.GeoControlHelper ;

//==============================================================================
// Private events & methods
//==============================================================================

  procedure TGIS_ControlLegendBMP.fset_Bitmap( const _value : TGIS_Bitmap ) ;
  begin
    if not Assigned( _value ) then begin
      FreeObject( FBitmap ) ;
      imgBitmap.Picture.Assign( nil ) ;
      pnlMain.Caption := GIS_RS_LEGEND_NONE ;
      exit ;
    end ;

    if not Assigned( FBitmap ) then
      FBitmap := TGIS_Bitmap.Create ;

    FBitmap.Assign( _value ) ;
    FBitmap.Transparent := False ;
    imgBitmap.Picture.Assign( TPersistent(_value.NativeBitmap) ) ;
    imgBitmap.Update ;
    pnlMain.Caption := '' ;
  end ;

//==============================================================================
// Form events & methods
//==============================================================================

  procedure TGIS_ControlLegendBMP.FormDestroy(Sender: TObject);
  begin
    FreeObject( FBitmap ) ;
  end;

//==============================================================================
// Control events & methods
//==============================================================================

  procedure TGIS_ControlLegendBMP.btnSelectFileClick(Sender: TObject);
  var
    bmp : TGIS_Bitmap ;
  begin
    if not dlgOpen.Execute then exit ;

    try
      bmp := TGIS_Bitmap.Create ;
      bmp.Transparent := False ;
      try
        bmp.LoadFromFile( dlgOpen.FileName );
        fset_Bitmap( bmp ) ;
      finally
        bmp.Free ;
      end ;
    except
      fset_Bitmap( nil ) ;
    end ;
  end;

//==============================================================================
// Public events & methods
//==============================================================================

  procedure TGIS_ControlLegendBMP.initForm ;
  begin
    Self.Caption := _rsrc( GIS_RS_LEGEND_DLGBITMAP ) ;
    Self.ClientHeight := 212 ;
    Self.ClientWidth := 260 ;
    Self.Name := 'TGIS_ControlLegendBMP' ;
    Self.OnDestroy := FormDestroy ;
  end ;

  procedure TGIS_ControlLegendBMP.initControls ;
  var
    anchors : TAnchors ;
  begin
    if BiDiMode = bdRightToLeft then
      anchors := [akRight, akTop]
    else
      anchors := [akLeft, akTop] ;

    btnSelectFile := TButton.Create( Self ) ;
    btnSelectFile.Parent := Self ;
    btnSelectFile.Anchors := anchors ;
    btnSelectFile.Top := 8 ;
    btnSelectFile.Height := 25 ;
    PlaceControl( BiDiMode, nil, btnSelectFile, 8, 75 ) ;
    btnSelectFile.Caption := _rsrc( GIS_RS_LEGEND_DLGBITMAP_FILE ) ;
    btnSelectFile.TabOrder := 0 ;
    btnSelectFile.OnClick := btnSelectFileClick ;

    pnlMain := TPanel.Create( Self ) ;
    pnlMain.Parent := Self ;
    pnlMain.Anchors := anchors ;
    pnlMain.Top := 8 ;
    pnlMain.Height := 153 ;
    PlaceControl( BiDiMode, nil, pnlMain, 90, 162 ) ;
    pnlMain.BevelInner := bvRaised ;
    pnlMain.BevelOuter := bvLowered ;
    pnlMain.Caption := _rsrc( GIS_RS_LEGEND_NONE ) ;
    pnlMain.TabOrder := 1 ;

    imgBitmap := TImage.Create( pnlMain ) ;
    imgBitmap.Parent := pnlMain ;
    imgBitmap.Top := 2 ;
    imgBitmap.Height := 133 ;
    PlaceControl( BiDiMode, nil, imgBitmap, 2, -1 ) ;
    imgBitmap.Align := alClient ;
    imgBitmap.Proportional := True ;
    imgBitmap.Center := True ;

    btnHelp.Visible := assigned( pOnHelp ) ;
    btnHelp.TabOrder := 2 ;
    btnCancel.TabOrder := 4 ;
    btnOK.TabOrder := 3 ;

    dlgOpen := TOpenPictureDialog.Create( Self ) ;
    dlgOpen.Filter := GIS_RS_LEGEND_DLGBITMAP_FILTER ;
    dlgOpen.Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing] ;

  end ;

  function TGIS_ControlLegendBMP.Execute(
    const _path   : String         ;
    const _onhelp : TGIS_HelpEvent
  ) : Integer ;
  var
    bmp : TGIS_Bitmap ;
  begin
    pOnHelp := _onhelp ;
    btnHelp.Visible := Assigned( pOnHelp ) ;

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
      bmp.Free ;
    end ;

    Result := ShowModal ;
  end ;

  function TGIS_ControlLegendBMP.Execute(
    const _path : String
  ) : Integer ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := Execute( _path, hlp ) ;
  end;

//==================================== END =====================================
end.

