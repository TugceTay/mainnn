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

unit Lider.CG.GIS.FMX.GeoControlLegendBMP ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.FMX.GeoControlLegendBMP"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Graphics,
  FMX.Objects,
  FMX.StdCtrls,
  FMX.Types,
  FMX.Controls,

  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.FMX.GeoModalForm, FMX.Controls.Presentation ;

type

  /// <summary>
  ///   Visual form for managing bitmaps.
  /// </summary>
  /// <remarks>
  ///   To be use only from: TGIS_ControlLegend.
  /// </remarks>
  TGIS_ControlLegendBMP = class( TGIS_ModalForm )
    btnSelectFile: TButton;
    dlgOpen: TOpenDialog;
    pnlMain: TRectangle;
    lblMain: TLabel;
    imgBitmap: TImage;
    procedure btnSelectFileClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject) ; override;
    procedure FormDestroy(Sender: TObject);
  private // properties internal value
    FBitmap : TGIS_Bitmap ;

  private
    const frmClientHeight = 225 ;
    const frmClientWidth  = 260 ;

  protected

    /// <inheritdoc/>
    procedure initForm     ; override;

    /// <inheritdoc/>
    procedure initControls ; override;

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
    /// <param name="_proc">
    ///   to be executed after the form was closed
    /// </param>
    procedure Execute      ( const _path   : String         ;
                             const _onhelp : TGIS_HelpEvent ;
                             const _proc   : TProc<TModalResult>
                           ) ; overload;

    /// <summary>
    ///   Execute dialog on a bitmap given by path.
    /// </summary>
    /// <param name="_path">
    ///   path to the bitmap.
    /// </param>
    /// <param name="_proc">
    ///   to be executed after the form was closed
    /// </param>
    procedure Execute      ( const _path   : String         ;
                             const _proc   : TProc<TModalResult>
                           ) ; overload;
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
  Lider.CG.GIS.FMX.GeoControlHelper ;

//==============================================================================
// Private events & methods
//==============================================================================

  procedure TGIS_ControlLegendBMP.fset_Bitmap( const _value : TGIS_Bitmap ) ;
  begin
    if not Assigned( _value ) then begin
      FreeObject( FBitmap ) ;
      imgBitmap.MultiResBitmap.Assign( nil ) ;
      lblMain.Text := _rsrcna( GIS_RS_LEGEND_NONE ) ;
      btnOK.Enabled := False ;
      exit ;
    end ;

    if not Assigned( FBitmap ) then
      FBitmap := TGIS_Bitmap.Create ;

    FBitmap.Assign( _value ) ;
    FBitmap.Transparent := False ;
    imgBitmap.Bitmap.Assign( TPersistent(_value.NativeBitmap) ) ;
    imgBitmap.Repaint ;
    lblMain.Text := '' ;
    btnOK.Enabled := True ;
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

  procedure TGIS_ControlLegendBMP.btnOKClick(Sender: TObject) ;
  begin
    if Assigned( Bitmap ) then
      ModalResult := mrOK ;
  end;

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
    Self.ClientHeight := frmClientHeight ;
    Self.ClientWidth := frmClientWidth ;
    Self.Name := 'TGIS_ControlLegendBMP' ;
    OnDestroy := FormDestroy ;
  end ;

  procedure TGIS_ControlLegendBMP.initControls ;
  begin
    btnSelectFile := TButton.Create( oMainForm ) ;
    btnSelectFile.Parent := oMainForm ;
    btnSelectFile.Size.PlatformDefault := False ;
    btnSelectFile.TabOrder := 0 ;
    btnSelectFile.Text := _rsrcna( GIS_RS_LEGEND_DLGBITMAP_FILE ) ;
    btnSelectFile.OnClick := btnSelectFileClick ;

    pnlMain := TRectangle.Create( oMainForm ) ;
    pnlMain.Parent := oMainForm ;
    pnlMain.Fill.Kind := TBrushKind.None ;
    pnlMain.Stroke.Color := TAlphaColorRec.Darkgrey ;
    pnlMain.TabOrder := 1 ;

    lblMain := TLabel.Create( pnlMain ) ;
    lblMain.Parent := pnlMain ;
    lblMain.Size.PlatformDefault := False ;
    lblMain.Text := _rsrcna( GIS_RS_LEGEND_NONE ) ;
    lblMain.Align := TAlignLayout.Client ;
    lblMain.TextAlign := TTextAlign.Center ;
    lblMain.VertTextAlign := TTextAlign.Center ;
    lblMain.FixSize ;

    imgBitmap := TImage.Create( pnlMain ) ;
    imgBitmap.Parent := pnlMain ;
    imgBitmap.Align := TAlignLayout.Client ;
    imgBitmap.Size.PlatformDefault := False ;
    imgBitmap.Margins := TBounds.Create( TRectF.Create(1,1,1,1)) ;

    btnSelectFile.Position.Y := 8 ;
    btnSelectFile.Size.Height := 24 ;
    PlaceControl( BiDiMode, nil, btnSelectFile, 8, 75 ) ;

    pnlMain.Position.Y := 8 ;
    pnlMain.Size.Height := 153 ;
    PlaceControl( BiDiMode, nil, pnlMain, -8, 153 ) ;

    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Position.Y := ClientHeight - btnHelp.Height - 8 ;
      PlaceControl( BiDiMode, nil, btnHelp, 8, 75 ) ;

      btnCancel.Position.Y := ClientHeight - btnCancel.Height - 8 ;
      PlaceControl( BiDiMode, nil, btnCancel, -8, 75 ) ;

      btnOK.Position.Y := ClientHeight - btnOK.Height - 8 ;
      PlaceControl( BiDiMode, btnCancel, btnOK, -8, 75 ) ;
    {$ENDIF}
    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Visible := assigned( pOnHelp ) ;
      btnHelp.TabOrder := 2 ;
      btnCancel.TabOrder := 4 ;
      btnOK.TabOrder := 3 ;
    {$ENDIF}

    dlgOpen := TOpenDialog.Create( oMainForm ) ;
    dlgOpen.Parent := oMainForm ;
    dlgOpen.Filter := _rsrcna( GIS_RS_LEGEND_DLGBITMAP_FILTER ) ;
    dlgOpen.Options := [TOpenOption.ofHideReadOnly,
                        TOpenOption.ofFileMustExist,
                        TOpenOption.ofEnableSizing] ;
  end ;

  procedure TGIS_ControlLegendBMP.Execute(
    const _path   : String         ;
    const _onhelp : TGIS_HelpEvent ;
    const _proc : TProc<TModalResult>
  ) ;
  var
    bmp : TGIS_Bitmap ;
  begin
    pOnHelp := _onhelp ;
    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Visible := Assigned( pOnHelp ) ;
    {$ENDIF}

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

    ShowModaleX( _proc ) ;
  end ;

  procedure TGIS_ControlLegendBMP.Execute(
    const _path : String ;
    const _proc : TProc<TModalResult>
  ) ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Execute( _path, hlp, _proc ) ;
  end;

//==================================== END =====================================
end.

