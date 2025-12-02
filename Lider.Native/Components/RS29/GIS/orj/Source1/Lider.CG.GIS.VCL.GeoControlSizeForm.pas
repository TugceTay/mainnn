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
  Visual control for selecting fields.
}

unit Lider.CG.GIS.VCL.GeoControlSizeForm ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.VCL.GeoControlSizeForm"'}

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

  Lider.CG.GIS.GeoClasses,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoCsBase,
  Lider.CG.GIS.VCL.GeoValueValidatorHelper,
  Lider.CG.GIS.VCL.GeoModalForm;

type
  /// <summary>
  ///   Visual form for managing custom size values.
  /// </summary>
  TGIS_ControlSizeForm = class( TGIS_ModalForm )
    lblValue: TLabel;
    lblUnits: TLabel;
    spnFactor: TEdit;
    cmbUnits: TComboBox;
    vvedtValue : IGIS_ValueValidator ;

  private
    { Private declarations }
    isRotation : Boolean ;

  private
    procedure fset_Precision( _value : Integer ) ;

  protected

    /// <inheritdoc/>
    procedure initForm     ; override;

    /// <inheritdoc/>
    procedure initControls ; override;

  public

    /// <summary>
    ///   Fill out list of possible units.
    /// </summary>
    /// <param name="_rotation">
    ///   if it is a list of rotation units
    /// </param>
    procedure FillUnits( const _rotation : Boolean ) ;

    /// <summary>
    ///   Fill out list of possible measure units.
    /// </summary>
    procedure FillRealWorldUnits ;

    /// <summary>
    ///   Fill out list of possible print width units.
    /// </summary>
    procedure FillPrintWidthUnits ;

    /// <summary>
    ///   Fill out list of possible print font size units.
    /// </summary>
    procedure FillPrintFontSizeUnits ;

    /// <summary>
    ///   Fill out list of possible snap size units.
    /// </summary>
    procedure FillSnapUnits ;

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
                          ) : String ; overload;

    /// <summary>
    ///   Execute dialog.
    /// </summary>
    /// <returns>
    ///   Modal result.
    /// </returns>
    function Execute      : String ; overload;

  public
    /// <summary>
    ///   Setting value creates a value validator
    ///   for Value edit box with given precision.
    /// </summary>
    property Precision : Integer write fset_Precision ;
  end;

//##############################################################################
implementation

uses
  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoParams,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.VCL.GeoControlHelper ;

//==============================================================================
// Form events
//==============================================================================

  procedure TGIS_ControlSizeForm.initForm ;
  begin
    Self.Caption := _rsrc( GIS_RS_CUSTOMSIZE ) ;
    Self.ClientHeight := 104 ;
    Self.ClientWidth := 275 ;
    Self.Name := 'TGIS_ControlSizeForm' ;
  end ;

  procedure TGIS_ControlSizeForm.initControls ;
  var
    anchors   : TAnchors ;
    anchorsLR : TAnchors ;
  begin
    if BiDiMode = bdRightToLeft then
      anchors := [akRight, akTop]
    else
      anchors := [akLeft, akTop] ;
    anchorsLR := [akLeft, akTop, akRight] ;

    lblValue := TLabel.Create( Self ) ;
    lblValue.Parent := Self ;
    lblValue.Anchors := anchors ;
    lblValue.Top := 8 ;
    lblValue.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblValue, 8, 115 ) ;
    lblValue.AutoSize := False ;
    lblValue.Caption := _rsrc( GIS_RS_CUSTOMSIZE_VALUE ) ;
    lblValue.FocusControl := spnFactor ;

    spnFactor := TEdit.Create( Self ) ;
    spnFactor.Parent := Self ;
    spnFactor.Anchors := anchors ;
    spnFactor.Top := 23 ;
    spnFactor.Height := 21 ;
    PlaceControl( BiDiMode, nil, spnFactor, 8, 115 ) ;
    spnFactor.Font.Charset := DEFAULT_CHARSET ;
    spnFactor.Font.Color := clWindowText ;
    spnFactor.Font.Height := -11 ;
    spnFactor.Font.Name := 'Tahoma' ;
    spnFactor.Font.Style := [] ;
    spnFactor.ParentFont := False ;
    spnFactor.TabOrder := 0 ;
    spnFactor.Text := '1' ;

    vvedtValue := nil ;

    lblUnits := TLabel.Create( Self ) ;
    lblUnits.Parent := Self ;
    lblUnits.Anchors := anchors ;
    lblUnits.Top := 8 ;
    lblUnits.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblUnits, 129, 115 ) ;
    lblUnits.AutoSize := False ;
    lblUnits.Caption := _rsrc( GIS_RS_CUSTOMSIZE_UNITS ) ;
    lblUnits.FocusControl := cmbUnits ;

    cmbUnits := TComboBox.Create( Self ) ;
    cmbUnits.Parent := Self ;
    cmbUnits.Anchors := anchorsLR ;
    cmbUnits.Top := 23 ;
    cmbUnits.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbUnits, 129, Self.ClientWidth - 129 - 8 ) ;
    cmbUnits.Style := csDropDownList ;
    cmbUnits.ParentShowHint := False ;
    cmbUnits.ShowHint := True ;
    cmbUnits.TabOrder := 1 ;

    btnHelp.Visible := assigned( pOnHelp ) ;
    btnHelp.TabOrder := 2 ;
    btnCancel.TabOrder := 4 ;
    btnOK.TabOrder := 3 ;
  end ;

  procedure TGIS_ControlSizeForm.fset_Precision(
    _value : Integer
  ) ;
  begin
    if _value = 0 then begin
      FreeObject( vvedtValue ) ;
      vvedtValue := TGIS_ValueValidatorEditHelper.Create( spnFactor ) ;
      vvedtValue.Precision := 0 ;
      vvedtValue.MinVal := 1 ;
      vvedtValue.MaxVal := 65535 ;
    end ;
  end ;

  procedure TGIS_ControlSizeForm.FillUnits(
    const _rotation : Boolean
  ) ;
  begin
    isRotation := _rotation ;

    cmbUnits.Items.BeginUpdate ;
    try
      cmbUnits.Items.Clear ;
      if _rotation then begin
        cmbUnits.Items.Add( GIS_PARAMTXT_ROTATION_DEG ) ;
        cmbUnits.Items.Add( GIS_PARAMTXT_ROTATION_RAD ) ;
        assert( cmbUnits.Items.Count = GIS_PARAMTXT_ANGLE_CNT ) ;
      end
      else begin
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
      end;
      cmbUnits.ItemIndex := 0 ;
    finally
      cmbUnits.Items.EndUpdate ;
    end ;
  end ;

  procedure TGIS_ControlSizeForm.FillRealWorldUnits ;
  begin
    isRotation := False ;

    cmbUnits.Items.BeginUpdate ;
    try
      cmbUnits.Items.Clear ;
      cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_IN ) ;
      cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_FT ) ;
      cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_YD ) ;
      cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_MI ) ;
      cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_MM ) ;
      cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_CM ) ;
      cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_M  ) ;
      cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_KM ) ;
      cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_NM ) ;
      cmbUnits.ItemIndex := 0 ;
    finally
      cmbUnits.Items.EndUpdate ;
    end ;
  end ;

  procedure TGIS_ControlSizeForm.FillPrintWidthUnits ;
  begin
    isRotation := False ;

    cmbUnits.Items.BeginUpdate ;
    try
      cmbUnits.Items.Clear ;
      cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_PX    ) ;
      cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_TWIPS ) ;
      cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_PT    ) ;
      cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_MM    ) ;
      cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_CM    ) ;
      cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_IN    ) ;
      cmbUnits.ItemIndex := 0 ;
    finally
      cmbUnits.Items.EndUpdate ;
    end ;
  end ;

  procedure TGIS_ControlSizeForm.FillSnapUnits ;
  begin
    isRotation := False ;

    cmbUnits.Items.BeginUpdate ;
    try
      cmbUnits.Items.Clear ;
      cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_CM    ) ;
      cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_MM    ) ;
      cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_PT    ) ;
      cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_IN    ) ;
      cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_PX    ) ;
      cmbUnits.ItemIndex := 0 ;
    finally
      cmbUnits.Items.EndUpdate ;
    end ;
  end ;

  procedure TGIS_ControlSizeForm.FillPrintFontSizeUnits ;
  begin
    isRotation := False ;

    cmbUnits.Items.BeginUpdate ;
    try
      cmbUnits.Items.Clear ;
      cmbUnits.Items.Add( GIS_PARAMTXT_SIZE_PT ) ;
      cmbUnits.ItemIndex := 0 ;
    finally
      cmbUnits.Items.EndUpdate ;
    end ;
  end;

  function TGIS_ControlSizeForm.Execute(
    const _onhelp : TGIS_HelpEvent
  ) : String ;
  var
    res : Integer ;
  begin
    pOnHelp := _onhelp ;
    btnHelp.Visible := Assigned( pOnHelp ) ;

    res := ShowModal ;
    if res = mrOk then begin
      if isRotation then
        Result := GIS_PARAMTXT_TYPE_ANGLE + ':' +
                  spnFactor.Text + ' ' + cmbUnits.Text
      else
        Result := GIS_PARAMTXT_TYPE_SIZE + ':' +
                  spnFactor.Text + ' ' + cmbUnits.Text
    end
    else
      Result := '' ;
  end ;

  function TGIS_ControlSizeForm.Execute
    : String ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := Execute( hlp ) ;
  end;

//==================================== END =====================================
end.

