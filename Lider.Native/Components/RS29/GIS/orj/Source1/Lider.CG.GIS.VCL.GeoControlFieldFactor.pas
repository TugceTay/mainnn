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
  Visual control for custom values.
}

unit Lider.CG.GIS.VCL.GeoControlFieldFactor ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.VCL.GeoControlFieldFactor"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.StdCtrls,

  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoClasses,
  Lider.CG.GIS.GeoCsBase,
  Lider.CG.GIS.VCL.GeoModalForm;

type

  /// <summary>
  ///   Mode indicating what kind of units to service
  /// </summary>
  TGIS_FieldFactorUnitsType = (

    /// <summary>
    ///   Angular units.
    /// </summary>
    Angular,

    /// <summary>
    ///   Size units.
    /// </summary>
    Size,

    /// <summary>
    ///   Measure units.
    /// </summary>
    Measure,

    /// <summary>
    ///   No units.
    /// </summary>
    NoScale
  ) ;

  /// <summary>
  ///   Form used for defining visual parameters according to field values.
  /// </summary>
  TGIS_ControlFieldFactor = class( TGIS_ModalForm )
    cmbFields: TComboBox;
    lblFields: TLabel;
    lblFactor: TLabel;
    lblUnits: TLabel;
    spnFactor: TEdit;
    cmbUnits: TComboBox;

  protected

    /// <inheritdoc/>
    procedure initForm         ; override;

    /// <inheritdoc/>
    procedure initControls     ; override;

    /// <inheritdoc/>
    procedure showForm         ; override;

    /// <inheritdoc/>
    procedure afterPPIChanged  ; override;

  public

    /// <summary>
    ///   Fill field list with values.
    /// </summary>
    /// <param name="_list">
    ///   list with field names
    /// </param>
    procedure   FillFields( const _list : TStrings
                          ) ;

    /// <summary>
    ///   Fill units list with predefined values.
    /// </summary>
    /// <param name="_unitsType">
    ///   type of units
    /// </param>
    procedure   FillUnits ( _unitsType  : TGIS_FieldFactorUnitsType
                          ) ;

    /// <summary>
    ///   Execute dialog.
    /// </summary>
    /// <param name="_onhelp">
    ///   help notification function; if assigned the help button will be
    ///   visible and help support will be enabled;
    /// </param>
    /// <returns>
    ///   Modal result.
    /// </returns>
    function Execute      ( const _onhelp : TGIS_HelpEvent
                          ) : Integer ; overload;

    /// <summary>
    ///   Execute dialog.
    /// </summary>
    /// <returns>
    ///   Modal result.
    /// </returns>
    function Execute      : Integer ; overload;
  end;

//##############################################################################
implementation

uses
  System.Math,

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoParams,
  Lider.CG.GIS.VCL.GeoControlHelper ;

//==============================================================================
// Form events
//==============================================================================

  procedure TGIS_ControlFieldFactor.initForm ;
  begin
    Self.Caption := _rsrc( GIS_RS_FIELDFACTOR ) ;
    Self.ClientHeight := 150 ;
    Self.ClientWidth := 280 ;
    Self.Name := 'TGIS_ControlFieldFactor' ;
  end ;

  procedure TGIS_ControlFieldFactor.initControls ;
  var
    anchors   : TAnchors ;
    anchorsLR : TAnchors ;
  begin
    if BiDiMode = bdRightToLeft then
      anchors := [akRight, akTop]
    else
      anchors := [akLeft, akTop] ;
    anchorsLR := [akLeft, akTop, akRight] ;

    lblFields := TLabel.Create( Self ) ;
    lblFields.Parent := Self ;
    lblFields.Anchors := anchorsLR ;
    lblFields.Top := 8 ;
    lblFields.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblFields, 8, -1 ) ;
    lblFields.AutoSize := False ;
    lblFields.Caption := _rsrc( GIS_RS_FIELDFACTOR_FIELD ) ;
    lblFields.FocusControl := cmbFields ;

    cmbFields := TComboBox.Create( Self ) ;
    cmbFields.Parent := Self ;
    cmbFields.Anchors := anchorsLR ;
    cmbFields.Top := 23 ;
    cmbFields.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbFields, 8, -1 ) ;
    cmbFields.Style := csDropDownList ;
    cmbFields.Sorted := True ;
    cmbFields.TabOrder := 0 ;

    lblFactor := TLabel.Create( Self ) ;
    lblFactor.Parent := Self ;
    lblFactor.Anchors := anchors ;
    lblFactor.Top := 54 ;
    lblFactor.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblFactor, 8, 115 ) ;
    lblFactor.AutoSize := False ;
    lblFactor.Caption := _rsrc( GIS_RS_FIELDFACTOR_FACTOR ) ;
    lblFactor.FocusControl := spnFactor ;

    spnFactor := TEdit.Create( Self ) ;
    spnFactor.Parent := Self ;
    spnFactor.Anchors := anchors ;
    spnFactor.Top := 69 ;
    spnFactor.Height := 21 ;
    PlaceControl( BiDiMode, nil, spnFactor, 8, 115 ) ;
    spnFactor.Font.Charset := DEFAULT_CHARSET ;
    spnFactor.Font.Color := clWindowText ;
    spnFactor.Font.Height := -11 ;
    spnFactor.Font.Name := 'Tahoma' ;
    spnFactor.Font.Style := [] ;
    spnFactor.ParentFont := False ;
    spnFactor.TabOrder := 1 ;
    spnFactor.Text := '1' ;

    lblUnits := TLabel.Create( Self ) ;
    lblUnits.Parent := Self ;
    lblUnits.Anchors := anchors ;
    lblUnits.Top := 54 ;
    lblUnits.Height := 13 ;
    PlaceControl( BiDiMode, lblFactor, lblUnits, 6, 144 ) ;
    lblUnits.AutoSize := False ;
    lblUnits.Caption := _rsrc( GIS_RS_FIELDFACTOR_UNITS ) ;
    lblUnits.FocusControl := cmbUnits ;

    cmbUnits := TComboBox.Create( Self ) ;
    cmbUnits.Parent := Self ;
    cmbUnits.Anchors := anchors ;
    cmbUnits.Top := 69 ;
    cmbUnits.Height := 21 ;
    PlaceControl( BiDiMode, spnFactor, cmbUnits, 6, 144 ) ;
    cmbUnits.Style := csDropDownList ;
    cmbUnits.ParentShowHint := False ;
    cmbUnits.ShowHint := True ;
    cmbUnits.TabOrder := 2 ;

    btnHelp.Visible := assigned( pOnHelp ) ;
    btnHelp.TabOrder := 3 ;
    btnCancel.TabOrder := 5 ;
    btnOK.TabOrder := 4 ;
  end ;

  procedure TGIS_ControlFieldFactor.showForm ;
  begin
    if lblFactor.Visible then
      Self.ClientHeight := spnFactor.Top +
                           spnFactor.Height + ppiFix(60)
    else
      Self.ClientHeight := cmbFields.Top +
                           cmbFields.Height + ppiFix(60) ;
  end ;

  procedure TGIS_ControlFieldFactor.afterPPIChanged ;
  begin

  end ;

  procedure TGIS_ControlFieldFactor.FillFields(
    const _list : TStrings
  ) ;
  var
    i : Integer ;
  begin
    cmbFields.Items.BeginUpdate ;
    try
      cmbFields.Clear ;
      for i := 0 to _list.Count - 1 do
        cmbFields.Items.Add( _list[ i ] ) ;
    finally
      cmbFields.Items.EndUpdate ;
    end;
    if cmbFields.Items.Count > 0 then
      cmbFields.ItemIndex := 0 ;
  end;

  procedure TGIS_ControlFieldFactor.FillUnits(
    _unitsType : TGIS_FieldFactorUnitsType
  ) ;
  var
    i   : Integer ;
    unt : TGIS_CSUnits ;
  begin
    cmbUnits.Items.BeginUpdate ;
    try
      cmbUnits.Items.Clear ;
      case _unitsType of
        TGIS_FieldFactorUnitsType.Size :
          begin
            cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_PX    ) ;
            cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_DIP   ) ;
            cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_TWIPS ) ;
            cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_PT    ) ;
            cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_MM    ) ;
            cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_CM    ) ;
            cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_M     ) ;
            cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_KM    ) ;
            cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_IN    ) ;
            cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_FT    ) ;
            cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_YD    ) ;
            cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_MI    ) ;
            cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_NM    ) ;
            assert( cmbUnits.Items.Count = GIS_PARAMTXT_SIZE_CNT ) ;
          end ;
        TGIS_FieldFactorUnitsType.Measure :
          begin
            cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_IN ) ;
            cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_FT ) ;
            cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_YD ) ;
            cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_MI ) ;
            cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_MM ) ;
            cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_CM ) ;
            cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_M  ) ;
            cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_KM ) ;
            cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_NM ) ;
          end ;
        TGIS_FieldFactorUnitsType.Angular :
          begin
            cmbUnits.Items.Add( GIS_PARAMTXT_ROTATION_DEG ) ;
            cmbUnits.Items.Add( GIS_PARAMTXT_ROTATION_RAD ) ;
            assert( cmbUnits.Items.Count = GIS_PARAMTXT_ANGLE_CNT ) ;
          end ;
      end;
    finally
      cmbUnits.Items.EndUpdate ;
    end;
    if _unitsType = TGIS_FieldFactorUnitsType.NoScale then begin
      lblFactor.Visible := False ;
      spnFactor.Visible := False ;
      lblUnits.Visible  := False ;
      cmbUnits.Visible  := False ;
    end else begin
      lblFactor.Visible := True ;
      spnFactor.Visible := True ;
      lblUnits.Visible  := True ;
      cmbUnits.Visible  := True ;
      if cmbUnits.Items.Count > 0 then
        cmbUnits.ItemIndex := 0 ;
    end;
  end;

  function TGIS_ControlFieldFactor.Execute(
    const _onhelp : TGIS_HelpEvent
  ) : Integer ;
  begin
    pOnHelp := _onhelp ;
    btnHelp.Visible := Assigned( pOnHelp ) ;

    Result := ShowModal ;
  end ;

  function TGIS_ControlFieldFactor.Execute
    : Integer ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := Execute( hlp ) ;
  end;

//==================================== END =====================================
end.

