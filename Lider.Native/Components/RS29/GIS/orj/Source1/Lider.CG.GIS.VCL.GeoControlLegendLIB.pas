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
  Legend component. Vector dialog - library symbol selector.
}

unit Lider.CG.GIS.VCL.GeoControlLegendLIB ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.VCL.GeoControlLegendLIB"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  Winapi.Windows,
  VCL.Forms,
  VCL.Dialogs,
  VCL.ExtDlgs,
  VCL.Controls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.ComCtrls,
  VCL.Graphics,
  System.Classes,

  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoSymbol,
  Lider.CG.GIS.GeoLibrarySVG,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.VCL.GeoModalForm;

type

  /// <summary>
  ///   Visual form for managing bitmaps.
  /// </summary>
  /// <remarks>
  ///   To be use only from: TGIS_ControlLegend.
  /// </remarks>
  TGIS_ControlLegendLIB = class( TGIS_ModalForm )
    pnlMain: TPanel;
    lstItems: TListView;
    cmbFilter: TComboBox;
    imgItems: TImageList;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmbFilterChange(Sender: TObject);
    procedure lstItemsClick(Sender: TObject);

  private // properties internal value
    FSymbol : TGIS_SymbolAbstract ;

  private
    lstNames: TStringList;
    oLib : TGIS_SymbolLibrarySVG ;

  protected

    /// <inheritdoc/>
    procedure initForm        ; override;

    /// <inheritdoc/>
    procedure initControls    ; override;

    /// <inheritdoc/>
    procedure showForm        ; override;

    /// <inheritdoc/>
    procedure afterPPIChanged ; override;

    /// <inheritdoc/>
    procedure btnCancelClick  ( _sender : TObject
                              ) ; override;

  public

    /// <summary>
    ///   Execute dialog.
    /// </summary>
    /// <returns>
    ///   Show modal results: mrCancel or mrOK.
    /// </returns>
    function Execute  : Integer ;

  public

    /// <summary>
    ///   Selected Symbol or nil.
    /// </summary>
    property Symbol : TGIS_SymbolAbstract read FSymbol ;
  end;

//##############################################################################
implementation

uses
  System.StrUtils,
  System.SysUtils,
  Lider.CG.GIS.VCL.GeoControlHelper,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoRtl;

//==============================================================================
// Form events & methods
//==============================================================================

  procedure TGIS_ControlLegendLIB.FormCreate(Sender: TObject);
  begin
     lstNames := TStringList.Create ;

     oLib := TGIS_SymbolLibrarySVG.Handle ;

     oLib.OpenForBrowsing ;

     cmbFilter.Items.Assign( oLib.Categories );
     cmbFilter.ItemIndex := 1 ;
  end;

  procedure TGIS_ControlLegendLIB.FormDestroy(Sender: TObject);
  begin
    FreeObject( lstNames ) ;
  end;

  procedure TGIS_ControlLegendLIB.lstItemsClick(Sender: TObject);
  begin
    if not Assigned( lstItems.Selected ) then exit ;
    if lstItems.Selected.Index < 0 then exit ;

    FSymbol := SymbolList.Prepare( lstNames[ lstItems.Selected.Index ] ) ;
  end;

//==============================================================================
// Control events & methods
//==============================================================================

  procedure TGIS_ControlLegendLIB.cmbFilterChange(Sender: TObject);
  var
    idx         : Integer ;
    bmp         : TBitmap ;
    itm         : TListItem ;
    symbol_name : String ;
    caption     : String ;
    filter      : String ;
    loop_filter : String ;
    search_str  : Boolean ;
  begin
    lstItems.Clear ;
    lstNames.Clear ;
    imgItems.Clear ;
    idx := 0 ;

    if Length( cmbFilter.Text ) = 0 then
      cmbFilter.ItemIndex := 1 ;

    filter := cmbFilter.Text ;
    if Length( filter ) < 2 then
      exit ;

    if oLib.Categories.IndexOf( filter ) > -1 then begin
      loop_filter := filter ; // filter is category name
      search_str := False ;
    end
    else begin
      loop_filter := '*' ;   // filter is search string
      search_str := True ;
    end ;

    lstItems.Items.BeginUpdate ;
    try
      for symbol_name in oLib.Loop( loop_filter ) do begin
        caption := oLib.GetCaption( symbol_name ) ;

        if search_str and ( not ContainsText( caption, filter ) ) then
          continue ;

        bmp := TBitmap.Create ;
        try
          bmp.Assign( TBitmap( oLib.GetPreview(symbol_name).NativeBitmap ) );
          bmp.PixelFormat := pf24bit ;
          imgItems.Add( bmp, nil ) ;
        finally
          bmp.Free ;
        end;
        lstNames.Add( symbol_name ) ;

        itm := lstItems.Items.Add ;
        itm.Caption := caption ;
        itm.ImageIndex := idx ;

        inc( idx ) ;
      end ;
    finally
      lstItems.Items.EndUpdate ;
    end ;
  end;

//==============================================================================
// Public events & methods
//==============================================================================

  procedure TGIS_ControlLegendLIB.initForm ;
  begin
    Self.Caption := _rsrc( GIS_RS_LEGEND_DLGLIB ) ;
    Self.ClientHeight := 289 ;
    Self.ClientWidth := 372 ;
    Self.OnCreate := FormCreate ;
    Self.OnDestroy := FormDestroy ;
  end ;

  procedure TGIS_ControlLegendLIB.initControls ;
  begin
    imgItems := TImageList.Create( Self ) ;
    imgItems.Height := 64 ;
    imgItems.Width := 64 ;

    pnlMain := TPanel.Create( Self ) ;
    pnlMain.Parent := Self ;
    pnlMain.Anchors := [akLeft, akTop, akRight] ;
    pnlMain.Top := 8 ;
    pnlMain.Height := 241 ;
    PlaceControl( BiDiMode, nil, pnlMain, 8, -1 ) ;
    pnlMain.Caption := _rsrc( GIS_RS_LEGEND_NONE ) ;
    pnlMain.BevelInner := bvRaised ;
    pnlMain.BevelOuter := bvLowered ;
    pnlMain.TabOrder := 1 ;

    lstItems := TListView.Create( pnlMain ) ;
    lstItems.Parent := pnlMain ;
    lstItems.Left := 0 ;
    lstItems.Top := 0 ;
    lstItems.Width := pnlMain.Width ;
    lstItems.Height := pnlMain.Height ;
    lstItems.Align := alClient ;
    lstItems.BorderStyle := bsNone ;
    lstItems.LargeImages := imgItems ;
    lstItems.TabOrder := 1 ;
    lstItems.ViewStyle := vsIcon ;
    lstItems.OnClick := lstItemsClick ;

    btnCancel.Visible := False ;
    btnHelp.Visible := False ;
    btnOK.TabOrder := 2 ;

    cmbFilter := TComboBox.Create( Self ) ;
    cmbFilter.Parent := Self ;
    cmbFilter.Anchors := [akLeft, akRight, akBottom] ;
    cmbFilter.Style := csDropDown ;
    cmbFilter.TabOrder := 0 ;
    cmbFilter.OnChange := cmbFilterChange ;
  end ;

  procedure TGIS_ControlLegendLIB.showForm ;
  begin
    afterPPIChanged ;
  end ;

  procedure TGIS_ControlLegendLIB.afterPPIChanged ;
  begin
    cmbFilterChange( Self ) ;
    btnOK.Height := cmbFilter.Height + 2 ;
    btnOK.Top := Self.ClientHeight - btnOK.Height - 8 ;
    PlaceControl( BiDiMode, nil, btnOK, -8, 92 ) ;
    cmbFilter.Top := btnOK.Top + 1 ;
    PlaceControl( BiDiMode, nil, cmbFilter, 8, ClientWidth - btnOK.Width - 3 * 8 ) ;
  end;

  procedure TGIS_ControlLegendLIB.btnCancelClick ;
  begin
    ModalResult := mrCancel ;
  end;

  function TGIS_ControlLegendLIB.Execute : Integer ;
  begin
    Result := ShowModal ;
  end ;

//==================================== END =====================================
end.

